
#include "config.h"

#include <stdio.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#include <stdlib.h>
#include <unistd.h>

#include "mp_msg.h"
#include "help_mp.h"

#include "linux/timer.h"
#include "linux/shmem.h"

extern int verbose; // defined in mplayer.c

#include "stream.h"
#include "demuxer.h"
#include "parse_es.h"

#include "codec-cfg.h"

#include "libvo/video_out.h"

#include "stheader.h"
#include "vd.h"
#include "vf.h"

#include "dec_video.h"

// ===================================================================

extern double video_time_usage;
extern double vout_time_usage;
extern vo_vaa_t vo_vaa;

#include "postproc/postprocess.h"

#include "cpudetect.h"

int divx_quality=0;

vd_functions_t* mpvdec=NULL;

int get_video_quality_max(sh_video_t *sh_video){
  vf_instance_t* vf=sh_video->vfilter;
  if(vf){
    int ret=vf->control(vf,VFCTRL_QUERY_MAX_PP_LEVEL,NULL);
    if(ret>0){
      mp_msg(MSGT_DECVIDEO,MSGL_INFO,"[PP] Using external postprocessing filter, max q = %d\n",ret);
      return ret;
    }
  }
  if(mpvdec){
    int ret=mpvdec->control(sh_video,VDCTRL_QUERY_MAX_PP_LEVEL,NULL);
    if(ret>0){
      mp_msg(MSGT_DECVIDEO,MSGL_INFO,"[PP] Using codec's postprocessing, max q = %d\n",ret);
      return ret;
    }
  }
  mp_msg(MSGT_DECVIDEO,MSGL_INFO,"[PP] Sorry, postprocessing is not available\n");
  return 0;
}

void set_video_quality(sh_video_t *sh_video,int quality){
  vf_instance_t* vf=sh_video->vfilter;
  if(vf){
    int ret=vf->control(vf,VFCTRL_SET_PP_LEVEL, (void*)(&quality));
    if(ret==CONTROL_TRUE) return; // success
  }
  if(mpvdec)
    mpvdec->control(sh_video,VDCTRL_SET_PP_LEVEL, (void*)(&quality));
}

int set_video_colors(sh_video_t *sh_video,char *item,int value)
{
    if(vo_vaa.get_video_eq)
    {
	vidix_video_eq_t veq;
	if(vo_vaa.get_video_eq(&veq) == 0)
	{
	    int v_hw_equ_cap = veq.cap;
	    if(v_hw_equ_cap != 0)
	    {
		if(vo_vaa.set_video_eq)
		{
		    vidix_video_eq_t veq;
		    veq.flags = VEQ_FLG_ITU_R_BT_601; /* Fixme please !!! */
		    if(strcmp(item,"Brightness") == 0)
		    {
			if(!(v_hw_equ_cap & VEQ_CAP_BRIGHTNESS)) goto try_sw_control;
			veq.brightness = value*10;
			veq.cap = VEQ_CAP_BRIGHTNESS;
		    }
		    else
		    if(strcmp(item,"Contrast") == 0)
		    {
			if(!(v_hw_equ_cap & VEQ_CAP_CONTRAST)) goto try_sw_control;
			veq.contrast = value*10;
			veq.cap = VEQ_CAP_CONTRAST;
		    }
		    else
		    if(strcmp(item,"Saturation") == 0)
		    {
			if(!(v_hw_equ_cap & VEQ_CAP_SATURATION)) goto try_sw_control;
			veq.saturation = value*10;
			veq.cap = VEQ_CAP_SATURATION;
		    }
		    else
		    if(strcmp(item,"Hue") == 0)
		    {
			if(!(v_hw_equ_cap & VEQ_CAP_HUE)) goto try_sw_control;
			veq.hue = value*10;
			veq.cap = VEQ_CAP_HUE;
		    }
		    else goto try_sw_control;;
		    vo_vaa.set_video_eq(&veq);
		}
		return 1;
	    }
	}
    }
    try_sw_control:
    if(mpvdec) return mpvdec->control(sh_video,VDCTRL_SET_EQUALIZER,item,(int)value);
    return 0;
}

void uninit_video(sh_video_t *sh_video){
    if(!sh_video->inited) return;
    mp_msg(MSGT_DECVIDEO,MSGL_V,"uninit video: %d  \n",sh_video->codec->driver);
    mpvdec->uninit(sh_video);
    sh_video->inited=0;
}

int init_video(sh_video_t *sh_video,char* codecname,int vfm,int status){
    sh_video->codec=NULL;
    while((sh_video->codec=find_codec(sh_video->format,
      sh_video->bih?((unsigned int*) &sh_video->bih->biCompression):NULL,
      sh_video->codec,0) )){
	// ok we found one codec
	int i;
	if(sh_video->codec->flags&CODECS_FLAG_SELECTED) continue; // already tried & failed
	if(codecname && strcmp(sh_video->codec->name,codecname)) continue; // -vc
	if(vfm>=0 && sh_video->codec->driver!=vfm) continue; // vfm doesn't match
	if(sh_video->codec->status<status) continue; // too unstable
	sh_video->codec->flags|=CODECS_FLAG_SELECTED; // tagging it
	// ok, it matches all rules, let's find the driver!
	for (i=0; mpcodecs_vd_drivers[i] != NULL; i++)
	    if(mpcodecs_vd_drivers[i]->info->id==sh_video->codec->driver) break;
	mpvdec=mpcodecs_vd_drivers[i];
	if(!mpvdec){ // driver not available (==compiled in)
	    mp_msg(MSGT_DECVIDEO,MSGL_ERR,"Requested video codec family [%s] (vfm=%d) not available (enable it at compile time!)\n",
		sh_video->codec->name, sh_video->codec->driver);
	    continue;
	}
	// it's available, let's try to init!
	printf("Opening Video Decoder: [%s] %s\n",mpvdec->info->short_name,mpvdec->info->name);
	if(!mpvdec->init(sh_video)){
	    printf("VDecoder init failed :(\n");
	    continue; // try next...
	}
	// Yeah! We got it!
	sh_video->inited=1;
	return 1;
    }
    return 0;
}

extern int vo_directrendering;

int decode_video(sh_video_t *sh_video,unsigned char *start,int in_size,int drop_frame){
vf_instance_t* vf=sh_video->vfilter;
mp_image_t *mpi=NULL;
int blit_frame=0;
unsigned int t=GetTimer();
unsigned int t2;
double tt;

//if(!(sh_video->ds->flags&1) || sh_video->ds->pack_no<5)
mpi=mpvdec->decode(sh_video, start, in_size, drop_frame);

//------------------------ frame decoded. --------------------

#ifdef ARCH_X86
	// some codecs is broken, and doesn't restore MMX state :(
	// it happens usually with broken/damaged files.
if(gCpuCaps.has3DNow){
	__asm __volatile ("femms\n\t":::"memory");
}
else if(gCpuCaps.hasMMX){
	__asm __volatile ("emms\n\t":::"memory");
}
#endif

if(!mpi) return 0; // error / skipped frame

t2=GetTimer();t=t2-t;
tt = t*0.000001f;
video_time_usage+=tt;

if(drop_frame) return 0;

//vo_draw_image(video_out,mpi);
vf->put_image(vf,mpi);

    t2=GetTimer()-t2;
    tt=t2*0.000001f;
    vout_time_usage+=tt;
    blit_frame=1;

  return blit_frame;
}


#include "config.h"

#include <stdio.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#include <stdlib.h>
#include <unistd.h>

#include "mp_msg.h"
#include "help_mp.h"

#include "osdep/timer.h"
#include "osdep/shmem.h"

#include "stream.h"
#include "demuxer.h"
#include "parse_es.h"

#include "codec-cfg.h"

#include "libvo/video_out.h"

#include "stheader.h"
#include "vd.h"
#include "vf.h"

#include "dec_video.h"

#ifdef DYNAMIC_PLUGINS
#include <dlfcn.h>
#endif

// ===================================================================

extern double video_time_usage;
extern double vout_time_usage;

#include "cpudetect.h"

int divx_quality=0;

vd_functions_t* mpvdec=NULL;

int get_video_quality_max(sh_video_t *sh_video){
  vf_instance_t* vf=sh_video->vfilter;
  if(vf){
    int ret=vf->control(vf,VFCTRL_QUERY_MAX_PP_LEVEL,NULL);
    if(ret>0){
      mp_msg(MSGT_DECVIDEO,MSGL_INFO,MSGTR_UsingExternalPP,ret);
      return ret;
    }
  }
  if(mpvdec){
    int ret=mpvdec->control(sh_video,VDCTRL_QUERY_MAX_PP_LEVEL,NULL);
    if(ret>0){
      mp_msg(MSGT_DECVIDEO,MSGL_INFO,MSGTR_UsingCodecPP,ret);
      return ret;
    }
  }
//  mp_msg(MSGT_DECVIDEO,MSGL_INFO,"[PP] Sorry, postprocessing is not available\n");
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
    vf_instance_t* vf=sh_video->vfilter;
    vf_equalizer_t data;

    data.item = item;
    data.value = value;

    mp_dbg(MSGT_DECVIDEO,MSGL_V,"set video colors %s=%d \n", item, value);
    if (vf)
    {
	int ret = vf->control(vf, VFCTRL_SET_EQUALIZER, &data);
	if (ret == CONTROL_TRUE)
	    return(1);
    }
    /* try software control */
    if(mpvdec)
	if( mpvdec->control(sh_video,VDCTRL_SET_EQUALIZER, item, (int *)value)
	    == CONTROL_OK) return 1;
    mp_msg(MSGT_DECVIDEO,MSGL_INFO,MSGTR_VideoAttributeNotSupportedByVO_VD,item);
    return 0;
}

int get_video_colors(sh_video_t *sh_video,char *item,int *value)
{
    vf_instance_t* vf=sh_video->vfilter;
    vf_equalizer_t data;

    data.item = item;

    mp_dbg(MSGT_DECVIDEO,MSGL_V,"get video colors %s \n", item);
    if (vf)
    {
        int ret = vf->control(vf, VFCTRL_GET_EQUALIZER, &data);
	if (ret == CONTROL_TRUE){
	    *value = data.value;
	    return(1);
	}
    }
    /* try software control */
    if(mpvdec) return mpvdec->control(sh_video,VDCTRL_GET_EQUALIZER, item, value);
    return 0;
}

int set_rectangle(sh_video_t *sh_video,int param,int value)
{
    vf_instance_t* vf=sh_video->vfilter;
    int data[] = {param, value};

    mp_dbg(MSGT_DECVIDEO,MSGL_V,"set rectangle \n");
    if (vf)
    {
        int ret = vf->control(vf, VFCTRL_CHANGE_RECTANGLE, data);
	if (ret)
	    return(1);
    }
    return 0;
}

void resync_video_stream(sh_video_t *sh_video)
{
    if(mpvdec) mpvdec->control(sh_video, VDCTRL_RESYNC_STREAM, NULL);
}

void uninit_video(sh_video_t *sh_video){
    if(!sh_video->inited) return;
    mp_msg(MSGT_DECVIDEO,MSGL_V,MSGTR_UninitVideoStr,sh_video->codec->drv);
    mpvdec->uninit(sh_video);
#ifdef DYNAMIC_PLUGINS
    if (sh_video->dec_handle)
	dlclose(sh_video->dec_handle);
#endif
    vf_uninit_filter_chain(sh_video->vfilter);
    sh_video->inited=0;
}

void vfm_help(){
    int i;
    mp_msg(MSGT_DECVIDEO,MSGL_INFO,MSGTR_AvailableVideoFm);
    if (identify)
      mp_msg(MSGT_GLOBAL, MSGL_INFO, "ID_VIDEO_DRIVERS\n");
    mp_msg(MSGT_DECVIDEO,MSGL_INFO,"   vfm:    info:  (comment)\n");
    for (i=0; mpcodecs_vd_drivers[i] != NULL; i++)
	mp_msg(MSGT_DECVIDEO,MSGL_INFO,"%8s  %s (%s)\n",
	    mpcodecs_vd_drivers[i]->info->short_name,
	    mpcodecs_vd_drivers[i]->info->name,
	    mpcodecs_vd_drivers[i]->info->comment);
}

int init_video(sh_video_t *sh_video,char* codecname,char* vfm,int status){
    unsigned int orig_fourcc=sh_video->bih?sh_video->bih->biCompression:0;
    sh_video->codec=NULL;
    sh_video->vf_inited=0;

    while(1){
	int i;
	// restore original fourcc:
	if(sh_video->bih) sh_video->bih->biCompression=orig_fourcc;
	if(!(sh_video->codec=find_codec(sh_video->format,
          sh_video->bih?((unsigned int*) &sh_video->bih->biCompression):NULL,
          sh_video->codec,0) )) break;
	// ok we found one codec
	if(sh_video->codec->flags&CODECS_FLAG_SELECTED) continue; // already tried & failed
	if(codecname && strcmp(sh_video->codec->name,codecname)) continue; // -vc
	if(vfm && strcmp(sh_video->codec->drv,vfm)) continue; // vfm doesn't match
	if(sh_video->codec->status<status) continue; // too unstable
	sh_video->codec->flags|=CODECS_FLAG_SELECTED; // tagging it
	// ok, it matches all rules, let's find the driver!
	for (i=0; mpcodecs_vd_drivers[i] != NULL; i++)
//	    if(mpcodecs_vd_drivers[i]->info->id==sh_video->codec->driver) break;
	    if(!strcmp(mpcodecs_vd_drivers[i]->info->short_name,sh_video->codec->drv)) break;
	mpvdec=mpcodecs_vd_drivers[i];
#ifdef DYNAMIC_PLUGINS
	if (!mpvdec)
	{
	    /* try to open shared decoder plugin */
	    int buf_len;
	    char *buf;
	    vd_functions_t *funcs_sym;
	    vd_info_t *info_sym;

	    buf_len = strlen(MPLAYER_LIBDIR)+strlen(sh_video->codec->drv)+16;
	    buf = malloc(buf_len);
	    if (!buf)
		break;
	    snprintf(buf, buf_len, "%s/mplayer/vd_%s.so", MPLAYER_LIBDIR, sh_video->codec->drv);
	    mp_msg(MSGT_DECVIDEO, MSGL_DBG2, "Trying to open external plugin: %s\n", buf);
	    sh_video->dec_handle = dlopen(buf, RTLD_LAZY);
	    if (!sh_video->dec_handle)
		break;
	    snprintf(buf, buf_len, "mpcodecs_vd_%s", sh_video->codec->drv);
	    funcs_sym = dlsym(sh_video->dec_handle, buf);
	    if (!funcs_sym || !funcs_sym->info || !funcs_sym->init ||
		!funcs_sym->uninit || !funcs_sym->control || !funcs_sym->decode)
		break;
	    info_sym = funcs_sym->info;
	    if (strcmp(info_sym->short_name, sh_video->codec->drv))
		break;
	    free(buf);
	    mpvdec = funcs_sym;
	    mp_msg(MSGT_DECVIDEO, MSGL_V, "Using external decoder plugin (%s/mplayer/vd_%s.so)!\n",
		MPLAYER_LIBDIR, sh_video->codec->drv);
	}
#endif
	if(!mpvdec){ // driver not available (==compiled in)
	    mp_msg(MSGT_DECVIDEO,MSGL_WARN,MSGTR_VideoCodecFamilyNotAvailableStr,
		sh_video->codec->name, sh_video->codec->drv);
	    continue;
	}
	// it's available, let's try to init!
	if(sh_video->codec->flags & CODECS_FLAG_ALIGN16){
	    // align width/height to n*16
	    // FIXME: save orig w/h, and restore if codec init failed!
	    if(sh_video->bih){
		sh_video->disp_w=sh_video->bih->biWidth=(sh_video->bih->biWidth+15)&(~15);
		sh_video->disp_h=sh_video->bih->biHeight=(sh_video->bih->biHeight+15)&(~15);
	    } else {
		sh_video->disp_w=(sh_video->disp_w+15)&(~15);
		sh_video->disp_h=(sh_video->disp_h+15)&(~15);
	    }
	}
	// init()
	mp_msg(MSGT_DECVIDEO,MSGL_INFO,MSGTR_OpeningVideoDecoder,mpvdec->info->short_name,mpvdec->info->name);
	if(!mpvdec->init(sh_video)){
	    mp_msg(MSGT_DECVIDEO,MSGL_INFO,MSGTR_VDecoderInitFailed);
	    continue; // try next...
	}
	// Yeah! We got it!
	sh_video->inited=1;
	return 1;
    }
    return 0;
}

extern char *get_path(char *filename);

int init_best_video_codec(sh_video_t *sh_video,char** video_codec_list,char** video_fm_list){
char* vc_l_default[2]={"",(char*)NULL};
// hack:
if(!video_codec_list) video_codec_list=vc_l_default;
// Go through the codec.conf and find the best codec...
sh_video->inited=0;
codecs_reset_selection(0);
while(!sh_video->inited && *video_codec_list){
  char* video_codec=*(video_codec_list++);
  if(video_codec[0]){
    if(video_codec[0]=='-'){
      // disable this codec:
      select_codec(video_codec+1,0);
    } else {
      // forced codec by name:
      mp_msg(MSGT_DECVIDEO,MSGL_INFO,MSGTR_ForcedVideoCodec,video_codec);
      init_video(sh_video,video_codec,NULL,-1);
    }
  } else {
    int status;
    // try in stability order: UNTESTED, WORKING, BUGGY. never try CRASHING.
    if(video_fm_list){
      char** fmlist=video_fm_list;
      // try first the preferred codec families:
      while(!sh_video->inited && *fmlist){
        char* video_fm=*(fmlist++);
	mp_msg(MSGT_DECVIDEO,MSGL_INFO,MSGTR_TryForceVideoFmtStr,video_fm);
	for(status=CODECS_STATUS__MAX;status>=CODECS_STATUS__MIN;--status)
	    if(init_video(sh_video,NULL,video_fm,status)) break;
      }
    }
    if(!sh_video->inited)
	for(status=CODECS_STATUS__MAX;status>=CODECS_STATUS__MIN;--status)
	    if(init_video(sh_video,NULL,NULL,status)) break;
  }
}

if(!sh_video->inited){
    mp_msg(MSGT_DECVIDEO,MSGL_ERR,MSGTR_CantFindVideoCodec,sh_video->format);
    mp_msg(MSGT_DECAUDIO,MSGL_HINT, MSGTR_RTFMCodecs);
    return 0; // failed
}

mp_msg(MSGT_DECVIDEO,MSGL_INFO,"Selected video codec: [%s] vfm:%s (%s)\n",
    sh_video->codec->name,sh_video->codec->drv,sh_video->codec->info);
return 1; // success
}

extern int vo_directrendering;

int decode_video(sh_video_t *sh_video,unsigned char *start,int in_size,int drop_frame){
vf_instance_t* vf;
mp_image_t *mpi=NULL;
unsigned int t=GetTimer();
unsigned int t2;
double tt;
int ret;

//if(!(sh_video->ds->flags&1) || sh_video->ds->pack_no<5)
mpi=mpvdec->decode(sh_video, start, in_size, drop_frame);

//------------------------ frame decoded. --------------------

#ifdef ARCH_X86
	// some codecs are broken, and doesn't restore MMX state :(
	// it happens usually with broken/damaged files.
if(gCpuCaps.has3DNow){
	__asm __volatile ("femms\n\t":::"memory");
}
else if(gCpuCaps.hasMMX){
	__asm __volatile ("emms\n\t":::"memory");
}
#endif

t2=GetTimer();t=t2-t;
tt = t*0.000001f;
video_time_usage+=tt;

if(!mpi || drop_frame) return 0; // error / skipped frame

//vo_draw_image(video_out,mpi);
vf=sh_video->vfilter;
ret = vf->put_image(vf,mpi); // apply video filters and call the leaf vo/ve
if(ret>0) vf->control(vf,VFCTRL_DRAW_OSD,NULL);

    t2=GetTimer()-t2;
    tt=t2*0.000001f;
    vout_time_usage+=tt;

return ret;
}

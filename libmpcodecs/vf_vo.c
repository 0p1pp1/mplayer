#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../config.h"
#include "../mp_msg.h"

#include "mp_image.h"
#include "vf.h"

#include "../libvo/video_out.h"

//===========================================================================//

#define video_out ((vo_functions_t*)(vf->priv))

static int query_format(struct vf_instance_s* vf, unsigned int fmt); /* forward declaration */

static int config(struct vf_instance_s* vf,
        int width, int height, int d_width, int d_height,
	unsigned int flags, unsigned int outfmt){

    if ((width <= 0) || (height <= 0) || (d_width <= 0) || (d_height <= 0))
    {
	mp_msg(MSGT_CPLAYER, MSGL_ERR, "VO: invalid dimensions!\n");
	return 0;
    }

  if(video_out->info)
  { const vo_info_t *info = video_out->info;
    mp_msg(MSGT_CPLAYER,MSGL_INFO,"VO: [%s] %dx%d => %dx%d %s %s%s%s%s\n",info->short_name,
         width, height,
         d_width, d_height,
	 vo_format_name(outfmt),
         (flags&1)?" [fs]":"",
         (flags&2)?" [vm]":"",
         (flags&4)?" [zoom]":"",
         (flags&8)?" [flip]":"");
    mp_msg(MSGT_CPLAYER,MSGL_V,"VO: Description: %s\n",info->name);
    mp_msg(MSGT_CPLAYER,MSGL_V,"VO: Author: %s\n", info->author);
    if(info->comment && strlen(info->comment) > 0)
        mp_msg(MSGT_CPLAYER,MSGL_V,"VO: Comment: %s\n", info->comment);
  }

    // save vo's stride capability for the wanted colorspace:
    vf->default_caps=query_format(vf,outfmt) & VFCAP_ACCEPT_STRIDE;

    if(video_out->config(width,height,d_width,d_height,flags,"MPlayer",outfmt))
	return 0;

    ++vo_config_count;
    return 1;
}

static int control(struct vf_instance_s* vf, int request, void* data)
{
    switch(request){
#ifdef USE_OSD
    case VFCTRL_DRAW_OSD:
	if(!vo_config_count) return CONTROL_FALSE; // vo not configured?
	video_out->draw_osd();
	return CONTROL_TRUE;
#endif
    case VFCTRL_SET_EQUALIZER:
    {
	vf_equalizer_t *eq=data;
	if(!vo_config_count) return CONTROL_FALSE; // vo not configured?
	return((video_out->control(VOCTRL_SET_EQUALIZER, eq->item, eq->value) == VO_TRUE) ? CONTROL_TRUE : CONTROL_FALSE);
    }
    case VFCTRL_GET_EQUALIZER:
    {
	vf_equalizer_t *eq=data;
	if(!vo_config_count) return CONTROL_FALSE; // vo not configured?
	return((video_out->control(VOCTRL_GET_EQUALIZER, eq->item, &eq->value) == VO_TRUE) ? CONTROL_TRUE : CONTROL_FALSE);
    }
    }
    // return video_out->control(request,data);
    return CONTROL_UNKNOWN;
}

static int query_format(struct vf_instance_s* vf, unsigned int fmt){
    int flags=video_out->control(VOCTRL_QUERY_FORMAT,&fmt);
    // draw_slice() accepts stride, draw_frame() doesn't:
    if(flags)
	if(fmt==IMGFMT_YV12 || fmt==IMGFMT_I420 || fmt==IMGFMT_IYUV)
	    flags|=VFCAP_ACCEPT_STRIDE;
    return flags;
}

static void get_image(struct vf_instance_s* vf,
        mp_image_t *mpi){
    if(vo_directrendering && vo_config_count)
	video_out->control(VOCTRL_GET_IMAGE,mpi);
}

static int put_image(struct vf_instance_s* vf,
        mp_image_t *mpi){
  if(!vo_config_count) return 0; // vo not configured?
  // first check, maybe the vo/vf plugin implements draw_image using mpi:
  if(video_out->control(VOCTRL_DRAW_IMAGE,mpi)==VO_TRUE) return 1; // done.
  // nope, fallback to old draw_frame/draw_slice:
  if(!(mpi->flags&(MP_IMGFLAG_DIRECT|MP_IMGFLAG_DRAW_CALLBACK))){
    // blit frame:
//    if(mpi->flags&MP_IMGFLAG_PLANAR)
    if(vf->default_caps&VFCAP_ACCEPT_STRIDE)
        video_out->draw_slice(mpi->planes,mpi->stride,mpi->w,mpi->h,mpi->x,mpi->y);
    else
        video_out->draw_frame(mpi->planes);
  }
  return 1;
}

static void draw_slice(struct vf_instance_s* vf,
        unsigned char** src, int* stride, int w,int h, int x, int y){
    if(!vo_config_count) return; // vo not configured?
    video_out->draw_slice(src,stride,w,h,x,y);
}

//===========================================================================//

static int open(vf_instance_t *vf, char* args){
    vf->config=config;
    vf->control=control;
    vf->query_format=query_format;
    vf->get_image=get_image;
    vf->put_image=put_image;
    vf->draw_slice=draw_slice;
    vf->priv=(void*)args; // video_out
    if(!video_out) return 0; // no vo ?
//    if(video_out->preinit(args)) return 0; // preinit failed
    return 1;
}

vf_info_t vf_info_vo = {
    "libvo wrapper",
    "vo",
    "A'rpi",
    "for internal use",
    open
};

//===========================================================================//

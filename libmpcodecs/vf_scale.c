#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

#include "../config.h"
#include "../mp_msg.h"

#include "img_format.h"
#include "mp_image.h"
#include "vf.h"

#include "../libvo/fastmemcpy.h"
#include "../postproc/swscale.h"

struct vf_priv_s {
    int w,h;
    unsigned int fmt;
    SwsContext *ctx;
};

extern int opt_screen_size_x;
extern int opt_screen_size_y;

//===========================================================================//

static unsigned int outfmt_list[]={
    IMGFMT_BGR32,
    IMGFMT_BGR24,
    IMGFMT_BGR16,
    IMGFMT_BGR15,
    IMGFMT_YV12,
    IMGFMT_I420,
    IMGFMT_IYUV,
    IMGFMT_Y800,
    IMGFMT_Y8,
    IMGFMT_YVU9,
    0
};

static unsigned int find_best_out(vf_instance_t *vf){
    unsigned int best=0;
    unsigned int* p=outfmt_list;
    // find the best outfmt:
    while(*p){
	int ret=vf_next_query_format(vf,*p);
	mp_msg(MSGT_VFILTER,MSGL_V,"scale: query(%s) -> %d\n",vo_format_name(*p),ret&3);
	if(ret&2){ best=*p; break;} // no conversion -> bingo!
	if(ret&1 && !best) best=*p; // best with conversion
	++p;
    }
    return best;
}

static int config(struct vf_instance_s* vf,
        int width, int height, int d_width, int d_height,
	unsigned int flags, unsigned int outfmt){
    unsigned int best=find_best_out(vf);
    int vo_flags;
    
    if(!best){
	mp_msg(MSGT_VFILTER,MSGL_WARN,"SwScale: no supported outfmt found :(\n");
	return 0;
    }
    
    vo_flags=vf->next->query_format(vf->next,best);
    
    // scaling to dwidth*d_height, if all these TRUE:
    // - option -zoom
    // - no other sw/hw up/down scaling avail.
    // - we're after postproc
    // - user didn't set w:h
    if(!(vo_flags&VFCAP_POSTPROC) && (flags&4) && 
	    vf->priv->w<0 && vf->priv->h<0){	// -zoom
	int x=(vo_flags&VFCAP_SWSCALE) ? 0 : 1;
	if(d_width<width || d_height<height){
	    // downscale!
	    if(vo_flags&VFCAP_HWSCALE_DOWN) x=0;
	} else {
	    // upscale:
	    if(vo_flags&VFCAP_HWSCALE_UP) x=0;
	}
	if(x){
	    // user wants sw scaling! (-zoom)
	    vf->priv->w=d_width;
	    vf->priv->h=d_height;
	}
    }

    // calculate the missing parameters:
    if(vf->priv->w==-3) vf->priv->w=vf->priv->h*width/height; else
    if(vf->priv->w==-2) vf->priv->w=vf->priv->h*d_width/d_height;
    if(vf->priv->w<0) vf->priv->w=width; else
    if(vf->priv->w==0) vf->priv->w=d_width;
    
    if(vf->priv->h==-3) vf->priv->h=vf->priv->w*height/width; else
    if(vf->priv->h==-2) vf->priv->h=vf->priv->w*d_height/d_width;
    if(vf->priv->h<0) vf->priv->h=height; else
    if(vf->priv->h==0) vf->priv->h=d_height;
    
    mp_msg(MSGT_VFILTER,MSGL_DBG2,"SwScale: scaling %dx%d %s to %dx%d %s  \n",
	width,height,vo_format_name(outfmt),
	vf->priv->w,vf->priv->h,vo_format_name(best));

    // free old ctx:
    if(vf->priv->ctx) freeSwsContext(vf->priv->ctx);
    
    // new swscaler:
    vf->priv->ctx=getSwsContextFromCmdLine(width,height,
	    (outfmt==IMGFMT_I420 || outfmt==IMGFMT_IYUV)?IMGFMT_YV12:outfmt,
		  vf->priv->w,vf->priv->h,
	    (best==IMGFMT_I420 || best==IMGFMT_IYUV)?IMGFMT_YV12:best);
    if(!vf->priv->ctx){
	// error...
	mp_msg(MSGT_VFILTER,MSGL_WARN,"Couldn't init SwScaler for this setup\n");
	return 0;
    }
    vf->priv->fmt=best;

    if(!opt_screen_size_x && !opt_screen_size_y){
	d_width=d_width*vf->priv->w/width;
	d_height=d_height*vf->priv->h/height;
    }
    return vf_next_config(vf,vf->priv->w,vf->priv->h,d_width,d_height,flags,best);
}

static void put_image(struct vf_instance_s* vf, mp_image_t *mpi){
    mp_image_t *dmpi;

    // hope we'll get DR buffer:
    dmpi=vf_get_image(vf->next,vf->priv->fmt,
	MP_IMGTYPE_TEMP, MP_IMGFLAG_ACCEPT_STRIDE,
	vf->priv->w, vf->priv->h);
    vf->priv->ctx->swScale(vf->priv->ctx,mpi->planes,mpi->stride,0,mpi->h,dmpi->planes,dmpi->stride);
    
    if(vf->priv->w==mpi->w && vf->priv->h==mpi->h){
	// just conversion, no scaling -> keep postprocessing data
	// this way we can apply pp filter to non-yv12 source using scaler
	dmpi->qscale=mpi->qscale;
	dmpi->qstride=mpi->qstride;
    }
    
    vf_next_put_image(vf,dmpi);
}

//===========================================================================//

//  supported Input formats: YV12, I420, IYUV, YUY2, BGR32, BGR24, BGR16, BGR15, RGB32, RGB24, Y8, Y800

static int query_format(struct vf_instance_s* vf, unsigned int fmt){
    switch(fmt){
    case IMGFMT_YV12:
    case IMGFMT_I420:
    case IMGFMT_IYUV:
    case IMGFMT_YUY2:
    case IMGFMT_BGR32:
    case IMGFMT_BGR24:
    case IMGFMT_BGR16:
    case IMGFMT_BGR15:
    case IMGFMT_RGB32:
    case IMGFMT_RGB24:
    case IMGFMT_Y800: 
    case IMGFMT_Y8: 
    case IMGFMT_YVU9: 
    {
	unsigned int best=find_best_out(vf);
	int flags;
	if(!best) return 0;	 // no matching out-fmt
	flags=vf_next_query_format(vf,best);
	if(!(flags&3)) return 0; // huh?
	if(fmt!=best) flags&=~VFCAP_CSP_SUPPORTED_BY_HW;
	// do not allow scaling, if we are before the PP fliter!
	if(!(flags&VFCAP_POSTPROC)) flags|=VFCAP_SWSCALE;
	return flags;
      }
    }
    return 0;	// nomatching in-fmt
}

static int open(vf_instance_t *vf, char* args){
    vf->config=config;
    vf->put_image=put_image;
    vf->query_format=query_format;
    vf->priv=malloc(sizeof(struct vf_priv_s));
    // TODO: parse args ->
    vf->priv->ctx=NULL;
    vf->priv->w=
    vf->priv->h=-1;
    if(args) sscanf(args, "%d:%d",
    &vf->priv->w,
    &vf->priv->h);
    mp_msg(MSGT_VFILTER,MSGL_V,"SwScale params: %d x %d (-1=no scaling)\n",
    vf->priv->w,
    vf->priv->h);
    return 1;
}

vf_info_t vf_info_scale = {
    "software scaling",
    "scale",
    "A'rpi",
    "",
    open
};

//===========================================================================//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <errno.h>

#include "../config.h"
#include "../mp_msg.h"
#include "../cpudetect.h"

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include "img_format.h"
#include "mp_image.h"
#include "vf.h"

#ifdef USE_LIBAVCODEC

#define EMU_OLD

#include "../libavcodec/libpostproc/postprocess.h"

#ifdef EMU_OLD
#include "../libavcodec/libpostproc/postprocess_internal.h"
#endif

struct vf_priv_s {
    int pp;
    pp_mode_t *ppMode[PP_QUALITY_MAX+1];
    void *context;
    mp_image_t *dmpi;
    unsigned int outfmt;
};

//===========================================================================//

static int config(struct vf_instance_s* vf,
        int width, int height, int d_width, int d_height,
	unsigned int voflags, unsigned int outfmt){
    int flags=
          (gCpuCaps.hasMMX   ? PP_CPU_CAPS_MMX   : 0)
	| (gCpuCaps.hasMMX2  ? PP_CPU_CAPS_MMX2  : 0)
	| (gCpuCaps.has3DNow ? PP_CPU_CAPS_3DNOW : 0);

    switch(outfmt){
    case IMGFMT_444P: flags|= PP_FORMAT_444; break;
    case IMGFMT_422P: flags|= PP_FORMAT_422; break;
    case IMGFMT_411P: flags|= PP_FORMAT_411; break;
    default:          flags|= PP_FORMAT_420; break;
    }
        
    if(vf->priv->context) pp_free_context(vf->priv->context);
    vf->priv->context= pp_get_context(width, height, flags);

    return vf_next_config(vf,width,height,d_width,d_height,voflags,outfmt);
}

static void uninit(struct vf_instance_s* vf){
    int i;
    for(i=0; i<=PP_QUALITY_MAX; i++){
        if(vf->priv->ppMode[i])
	    pp_free_mode(vf->priv->ppMode[i]);
    }
    if(vf->priv->context) pp_free_context(vf->priv->context);
}

static int query_format(struct vf_instance_s* vf, unsigned int fmt){
    switch(fmt){
    case IMGFMT_YV12:
    case IMGFMT_I420:
    case IMGFMT_IYUV:
    case IMGFMT_444P:
    case IMGFMT_422P:
    case IMGFMT_411P:
	return vf_next_query_format(vf,fmt);
    }
    return 0;
}

static int control(struct vf_instance_s* vf, int request, void* data){
    switch(request){
    case VFCTRL_QUERY_MAX_PP_LEVEL:
	return PP_QUALITY_MAX;
    case VFCTRL_SET_PP_LEVEL:
	vf->priv->pp= *((unsigned int*)data);
	return CONTROL_TRUE;
    }
    return vf_next_control(vf,request,data);
}

static void get_image(struct vf_instance_s* vf, mp_image_t *mpi){
    if(vf->priv->pp&0xFFFF) return; // non-local filters enabled
    if((mpi->type==MP_IMGTYPE_IPB || vf->priv->pp) && 
	mpi->flags&MP_IMGFLAG_PRESERVE) return; // don't change
    if(!(mpi->flags&MP_IMGFLAG_ACCEPT_STRIDE) && mpi->imgfmt!=vf->priv->outfmt)
	return; // colorspace differ
    // ok, we can do pp in-place (or pp disabled):
    vf->priv->dmpi=vf_get_image(vf->next,mpi->imgfmt,
        mpi->type, mpi->flags, mpi->w, mpi->h);
    mpi->planes[0]=vf->priv->dmpi->planes[0];
    mpi->stride[0]=vf->priv->dmpi->stride[0];
    mpi->width=vf->priv->dmpi->width;
    if(mpi->flags&MP_IMGFLAG_PLANAR){
        mpi->planes[1]=vf->priv->dmpi->planes[1];
        mpi->planes[2]=vf->priv->dmpi->planes[2];
	mpi->stride[1]=vf->priv->dmpi->stride[1];
	mpi->stride[2]=vf->priv->dmpi->stride[2];
    }
    mpi->flags|=MP_IMGFLAG_DIRECT;
}

static int put_image(struct vf_instance_s* vf, mp_image_t *mpi){
    if(!(mpi->flags&MP_IMGFLAG_DIRECT)){
	// no DR, so get a new image! hope we'll get DR buffer:
	vf->priv->dmpi=vf_get_image(vf->next,mpi->imgfmt,
	    MP_IMGTYPE_TEMP, MP_IMGFLAG_ACCEPT_STRIDE|MP_IMGFLAG_PREFER_ALIGNED_STRIDE,
//	    MP_IMGTYPE_TEMP, MP_IMGFLAG_ACCEPT_STRIDE,
//	    mpi->w,mpi->h);
	    (mpi->w+7)&(~7),(mpi->h+7)&(~7));
	vf->priv->dmpi->w=mpi->w; vf->priv->dmpi->h=mpi->h; // display w;h
    }
    
    if(vf->priv->pp || !(mpi->flags&MP_IMGFLAG_DIRECT)){
	// do the postprocessing! (or copy if no DR)
	pp_postprocess(mpi->planes           ,mpi->stride,
		    vf->priv->dmpi->planes,vf->priv->dmpi->stride,
		    (mpi->w+7)&(~7),mpi->h,
		    mpi->qscale, mpi->qstride,
		    vf->priv->ppMode[ vf->priv->pp ], vf->priv->context,
		    mpi->pict_type);
    }
    return vf_next_put_image(vf,vf->priv->dmpi);
}

//===========================================================================//

extern int divx_quality;

static unsigned int fmt_list[]={
    IMGFMT_YV12,
    IMGFMT_I420,
    IMGFMT_IYUV,
    IMGFMT_444P,
    IMGFMT_422P,
    IMGFMT_411P,
    0
};

static int open(vf_instance_t *vf, char* args){
    char *endptr, *name;
    int i;
    int hex_mode=0;
    
    vf->query_format=query_format;
    vf->control=control;
    vf->config=config;
    vf->get_image=get_image;
    vf->put_image=put_image;
    vf->uninit=uninit;
    vf->default_caps=VFCAP_ACCEPT_STRIDE|VFCAP_POSTPROC;
    vf->priv=malloc(sizeof(struct vf_priv_s));
    vf->priv->context=NULL;

    // check csp:
    vf->priv->outfmt=vf_match_csp(&vf->next,fmt_list,IMGFMT_YV12);
    if(!vf->priv->outfmt) return 0; // no csp match :(
    
    if(args){
	hex_mode= strtol(args, &endptr, 0);
	if(*endptr){
            name= args;
	}else
            name= NULL;
    }else{
        name="de";
    }

#ifdef EMU_OLD
    if(name){
#endif
	for(i=0; i<=PP_QUALITY_MAX; i++){
            vf->priv->ppMode[i]= pp_get_mode_by_name_and_quality(name, i);
            if(vf->priv->ppMode[i]==NULL) return -1;
        }
#ifdef EMU_OLD
    }else{
        /* hex mode for compatibility */
        for(i=0; i<=PP_QUALITY_MAX; i++){
	    PPMode *ppMode;
	    
	    ppMode= (PPMode*)memalign(8, sizeof(PPMode));
	    
	    ppMode->lumMode= hex_mode;
	    ppMode->chromMode= ((hex_mode&0xFF)>>4) | (hex_mode&0xFFFFFF00);
	    ppMode->maxTmpNoise[0]= 700;
	    ppMode->maxTmpNoise[1]= 1500;
	    ppMode->maxTmpNoise[2]= 3000;
	    ppMode->maxAllowedY= 234;
	    ppMode->minAllowedY= 16;
	    ppMode->baseDcDiff= 256/4;
	    ppMode->flatnessThreshold=40;
    
            vf->priv->ppMode[i]= ppMode;
        }
    }
#endif
    
    vf->priv->pp=PP_QUALITY_MAX; //divx_quality;
    return 1;
}

int readPPOpt(void *conf, char *arg)
{
  int val;

  if(arg == NULL)
    return -2; // ERR_MISSING_PARAM
  errno = 0;
  val = (int)strtol(arg,NULL,0);
  if(errno != 0)
    return -4;  // What about include cfgparser.h and use ERR_* defines */
  if(val < 0)
    return -3; // ERR_OUT_OF_RANGE

  divx_quality = val;

  return 1;
}
  
void revertPPOpt(void *conf, char* opt) 
{
  divx_quality=0;
}

vf_info_t vf_info_pp = {
    "postprocessing",
    "pp",
    "A'rpi",
    "",
    open,
    NULL
};

//===========================================================================//

#endif // USE_LIBAVCODEC

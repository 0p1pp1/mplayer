#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

#include "config.h"
#include "mp_msg.h"
#include "help_mp.h"

#include "img_format.h"
#include "mp_image.h"
#include "vf.h"

//#include "libvo/fastmemcpy.h"

#ifdef USE_LIBAVCODEC_SO
#include <ffmpeg/avcodec.h>
#else
#include "libavcodec/avcodec.h"
#endif

#if LIBAVCODEC_BUILD < 4641
#error we do not support libavcodec prior to build 4641, get the latest libavcodec CVS
#endif

#if LIBAVCODEC_BUILD < 4645
#warning your version of libavcodec is old, u might want to get a newer one
#endif

#if LIBAVCODEC_BUILD < 4645
#define AVFrame AVVideoFrame
#define coded_frame coded_picture
#endif

#if LIBAVCODEC_BUILD < 4684
#define FF_QP2LAMBDA 1
#endif

extern int avcodec_inited;

struct vf_priv_s {
    unsigned char* outbuf;
    int outbuf_size;
    AVCodecContext* context;
    AVFrame* pic;
    AVCodec* codec;
    vo_mpegpes_t pes;
};

#define lavc_venc_context (*vf->priv->context)

//===========================================================================//

static int config(struct vf_instance_s* vf,
        int width, int height, int d_width, int d_height,
	unsigned int flags, unsigned int outfmt){
    if(vf_next_query_format(vf,IMGFMT_MPEGPES)<=0) return 0;

    lavc_venc_context.width = width;
    lavc_venc_context.height = height;
    
#if LIBAVCODEC_BUILD >= 4754
    if(!lavc_venc_context.time_base.num || !lavc_venc_context.time_base.den){
#else
    if(!lavc_venc_context.frame_rate){
#endif
	// guess FPS:
	switch(height){
	case 240:
	case 480:
#if LIBAVCODEC_BUILD >= 4754
	    lavc_venc_context.time_base= (AVRational){1001,30000};
#else
#if LIBAVCODEC_BUILD >= 4662
	    lavc_venc_context.frame_rate     = 30000;
	    lavc_venc_context.frame_rate_base= 1001;
#else
	    lavc_venc_context.frame_rate=29.97*FRAME_RATE_BASE; // NTSC
#endif
#endif
	    break;
	case 576:
	case 288:
	default:
#if LIBAVCODEC_BUILD >= 4754
	    lavc_venc_context.time_base= (AVRational){1,25};
#else
#if LIBAVCODEC_BUILD >= 4662
	    lavc_venc_context.frame_rate     = 25;
	    lavc_venc_context.frame_rate_base= 1;
#else
	    lavc_venc_context.frame_rate=25*FRAME_RATE_BASE; // PAL
#endif
#endif
	    break;
//	    lavc_venc_context.frame_rate=vo_fps*FRAME_RATE_BASE; // same as src
	}
    }

    if(vf->priv->outbuf) free(vf->priv->outbuf);

    vf->priv->outbuf_size=10000+width*height;  // must be enough!
    vf->priv->outbuf = malloc(vf->priv->outbuf_size);

    if (avcodec_open(&lavc_venc_context, vf->priv->codec) != 0) {
	mp_msg(MSGT_MENCODER,MSGL_ERR,MSGTR_CantOpenCodec);
	return 0;
    }

    if (lavc_venc_context.codec->encode == NULL) {
	mp_msg(MSGT_MENCODER,MSGL_ERR,"avcodec init failed (ctx->codec->encode == NULL)!\n");
	return 0;
    }

    return vf_next_config(vf,width,height,d_width,d_height,flags,IMGFMT_MPEGPES);
}

static int put_image(struct vf_instance_s* vf, mp_image_t *mpi){
    mp_image_t* dmpi;
    int out_size;
    AVFrame *pic= vf->priv->pic;

    pic->data[0]=mpi->planes[0];
    pic->data[1]=mpi->planes[1];
    pic->data[2]=mpi->planes[2];
    pic->linesize[0]=mpi->stride[0];
    pic->linesize[1]=mpi->stride[1];
    pic->linesize[2]=mpi->stride[2];

    out_size = avcodec_encode_video(&lavc_venc_context, 
	vf->priv->outbuf, vf->priv->outbuf_size, pic);

    if(out_size<=0) return 1;

    dmpi=vf_get_image(vf->next,IMGFMT_MPEGPES,
	MP_IMGTYPE_EXPORT, 0,
	mpi->w, mpi->h);
    
    vf->priv->pes.data=vf->priv->outbuf;
    vf->priv->pes.size=out_size;
    vf->priv->pes.id=0x1E0;
    vf->priv->pes.timestamp=-1; // dunno
    
    dmpi->planes[0]=(unsigned char*)&vf->priv->pes;
    
    return vf_next_put_image(vf,dmpi);
}

//===========================================================================//

static int query_format(struct vf_instance_s* vf, unsigned int fmt){
    switch(fmt){
    case IMGFMT_YV12:
    case IMGFMT_I420:
    case IMGFMT_IYUV:
	return (vf_next_query_format(vf,IMGFMT_MPEGPES) & (~(VFCAP_CSP_SUPPORTED_BY_HW|VFCAP_ACCEPT_STRIDE)));
    }
    return 0;
}

static int open(vf_instance_t *vf, char* args){
    int p_quality=0;
    float p_fps=0;
    
    vf->config=config;
    vf->put_image=put_image;
    vf->query_format=query_format;
    vf->priv=malloc(sizeof(struct vf_priv_s));
    memset(vf->priv,0,sizeof(struct vf_priv_s));

    if (!avcodec_inited){
	avcodec_init();
	avcodec_register_all();
	avcodec_inited=1;
    }

    vf->priv->codec = (AVCodec *)avcodec_find_encoder_by_name("mpeg1video");
    if (!vf->priv->codec) {
	mp_msg(MSGT_MENCODER,MSGL_ERR,MSGTR_MissingLAVCcodec, "mpeg1video");
	return 0;
    }
    
    vf->priv->context=avcodec_alloc_context();
#if LIBAVCODEC_BUILD >= 4645
    vf->priv->pic = avcodec_alloc_frame();
#else
    vf->priv->pic = avcodec_alloc_picture();
#endif

    // TODO: parse args ->
    if(args) sscanf(args, "%d:%f", &p_quality, &p_fps);

    if(p_quality<32){
	// fixed qscale
	lavc_venc_context.flags = CODEC_FLAG_QSCALE;
#if LIBAVCODEC_BUILD >= 4668
	lavc_venc_context.global_quality =
#endif
	vf->priv->pic->quality = (int)(FF_QP2LAMBDA * ((p_quality<1) ? 1 : p_quality) + 0.5);
    } else {
	// fixed bitrate (in kbits)
	lavc_venc_context.bit_rate = 1000*p_quality;
    }
#if LIBAVCODEC_BUILD >= 4754
    lavc_venc_context.time_base.num = 1000*1001;
    lavc_venc_context.time_base.den = (p_fps<1.0) ? 0 : (p_fps * lavc_venc_context.time_base.num);
#else
#if LIBAVCODEC_BUILD >= 4662
    lavc_venc_context.frame_rate_base = 1000*1001;
    lavc_venc_context.frame_rate      = (p_fps<1.0) ? 0 : (p_fps * lavc_venc_context.frame_rate_base);
#else
    lavc_venc_context.frame_rate      = (p_fps<1.0) ? 0 : (p_fps * FRAME_RATE_BASE);
#endif
#endif
    lavc_venc_context.gop_size = 0; // I-only
    lavc_venc_context.pix_fmt= PIX_FMT_YUV420P;

    return 1;
}

vf_info_t vf_info_lavc = {
    "realtime mpeg1 encoding with libavcodec",
    "lavc",
    "A'rpi",
    "",
    open,
    NULL
};

//===========================================================================//

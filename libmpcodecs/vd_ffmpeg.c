#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <time.h>

#include "config.h"
#include "mp_msg.h"
#include "help_mp.h"

#ifdef USE_LIBAVCODEC

#include "bswap.h"

#include "vd_internal.h"

static vd_info_t info = {
	"FFmpeg's libavcodec codec family",
	"ffmpeg",
	"A'rpi",
	"http://ffmpeg.sf.net",
	"native codecs"
};

LIBVD_EXTERN(ffmpeg)

#include "../postproc/rgb2rgb.h"


#ifdef USE_LIBAVCODEC_SO
#include <ffmpeg/avcodec.h>
#else
#include "libavcodec/avcodec.h"
#endif

int avcodec_inited=0;

#ifdef FF_POSTPROCESS
int quant_store[MBR+1][MBC+1];
#endif

typedef struct {
    AVCodecContext *avctx;
    int last_aspect;
    int do_slices;
    int do_dr1;
    int vo_inited;
    int convert;
    int best_csp;
} vd_ffmpeg_ctx;

//#ifdef FF_POSTPROCESS
//unsigned int lavc_pp=0;
//#endif

#include "cfgparser.h"

static void get_buffer(struct AVCodecContext *avctx, int width, int height, int pict_type);

static int lavc_param_workaround_bugs=0;
static int lavc_param_error_resilience=-1;
static int lavc_param_gray=0;
static int lavc_param_vstats=0;

struct config lavc_decode_opts_conf[]={
#if LIBAVCODEC_BUILD >= 4611
	{"bug", &lavc_param_workaround_bugs, CONF_TYPE_INT, CONF_RANGE, 0, 99, NULL},
	{"ver", &lavc_param_error_resilience, CONF_TYPE_INT, CONF_RANGE, -1, 99, NULL},
#endif
#if LIBAVCODEC_BUILD >= 4614
	{"gray", &lavc_param_gray, CONF_TYPE_FLAG, 0, 0, CODEC_FLAG_PART, NULL},
#endif
	{"vstats", &lavc_param_vstats, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{NULL, NULL, 0, 0, 0, 0, NULL}
};

// to set/get/query special features/parameters
static int control(sh_video_t *sh,int cmd,void* arg,...){
    vd_ffmpeg_ctx *ctx = sh->context;
    AVCodecContext *avctx = ctx->avctx;
    switch(cmd){
    case VDCTRL_QUERY_FORMAT:
	if( (*((int*)arg)) == ctx->best_csp ) return CONTROL_TRUE;//supported
	// possible conversions:
	switch( (*((int*)arg)) ){
        case IMGFMT_YV12:
        case IMGFMT_IYUV:
        case IMGFMT_I420:
	    // "converted" using pointer/stride modification
	    if(avctx->pix_fmt==PIX_FMT_YUV420P) return CONTROL_TRUE;// u/v swap
	    if(avctx->pix_fmt==PIX_FMT_YUV422P) return CONTROL_TRUE;// half stride
	    break;
#if 1
        case IMGFMT_YUY2:
	    // converted using yuv422ptoyuy2()
	    if(avctx->pix_fmt==PIX_FMT_YUV422P) return CONTROL_TRUE;
	    break;
	}
#endif
        return CONTROL_FALSE;
    }
    return CONTROL_UNKNOWN;
}

// init driver
static int init(sh_video_t *sh){
    AVCodecContext *avctx;
    vd_ffmpeg_ctx *ctx;
    AVCodec *lavc_codec;

    if(!avcodec_inited){
      avcodec_init();
      avcodec_register_all();
      avcodec_inited=1;
    }

    ctx = sh->context = malloc(sizeof(vd_ffmpeg_ctx));
    if (!ctx)
	return(0);
    memset(ctx, 0, sizeof(vd_ffmpeg_ctx));
    
    lavc_codec = (AVCodec *)avcodec_find_decoder_by_name(sh->codec->dll);
    if(!lavc_codec){
	mp_msg(MSGT_DECVIDEO,MSGL_ERR,MSGTR_MissingLAVCcodec,sh->codec->dll);
	return 0;
    }

    if(vd_use_slices && lavc_codec->capabilities&CODEC_CAP_DRAW_HORIZ_BAND)
	ctx->do_slices=1;
 
#if LIBAVCODEC_BUILD > 4615
    if(lavc_codec->capabilities&CODEC_CAP_DR1)
	ctx->do_dr1=1;
#endif
            
    ctx->avctx = malloc(sizeof(AVCodecContext));
    memset(ctx->avctx, 0, sizeof(AVCodecContext));
    avctx = ctx->avctx;

#if LIBAVCODEC_BUILD > 4615
    if(ctx->do_dr1){
        avctx->flags|= CODEC_FLAG_EMU_EDGE | CODEC_FLAG_DR1; 
        avctx->get_buffer_callback= get_buffer;
    }
#endif
    
    avctx->width = sh->disp_w;
    avctx->height= sh->disp_h;
#if LIBAVCODEC_BUILD >= 4611
    avctx->workaround_bugs= lavc_param_workaround_bugs;
    avctx->error_resilience= lavc_param_error_resilience;
#endif
#if LIBAVCODEC_BUILD >= 4614
    if(lavc_param_gray) avctx->flags|= CODEC_FLAG_GRAY;
#endif
    
    mp_dbg(MSGT_DECVIDEO,MSGL_DBG2,"libavcodec.size: %d x %d\n",avctx->width,avctx->height);
    if (sh->format == mmioFOURCC('R', 'V', '1', '3'))
	avctx->sub_id = 3;
#if LIBAVCODEC_BUILD >= 4605
    /* AVRn stores huffman table in AVI header */
    /* Pegasus MJPEG stores it also in AVI header, but it uses the common
       MJPG fourcc :( */
    if (sh->bih && (sh->bih->biSize != sizeof(BITMAPINFOHEADER)) &&
	(sh->format == mmioFOURCC('A','V','R','n') ||
	sh->format == mmioFOURCC('M','J','P','G')))
    {
	avctx->flags |= CODEC_FLAG_EXTERN_HUFF;
	avctx->extradata_size = sh->bih->biSize-sizeof(BITMAPINFOHEADER);
	avctx->extradata = malloc(avctx->extradata_size);
	memcpy(avctx->extradata, sh->bih+sizeof(BITMAPINFOHEADER),
	    avctx->extradata_size);

#if 0
	{
	    int x;
	    uint8_t *p = avctx->extradata;
	    
	    for (x=0; x<avctx->extradata_size; x++)
		printf("[%x] ", p[x]);
	    printf("\n");
	}
#endif
    }
#endif
    if(   sh->format == mmioFOURCC('R', 'V', '1', '0')
       || sh->format == mmioFOURCC('R', 'V', '1', '3')){
        unsigned int* extrahdr=(unsigned int*)(sh->bih+1);
        avctx->extradata_size= 8;
	avctx->extradata = malloc(avctx->extradata_size);
        ((uint32_t*)avctx->extradata)[0] = extrahdr[0];
        ((uint32_t*)avctx->extradata)[1] = extrahdr[1];
//        printf("%X %X %d %d\n", extrahdr[0], extrahdr[1]);
    }

    /* open it */
    if (avcodec_open(avctx, lavc_codec) < 0) {
        mp_msg(MSGT_DECVIDEO,MSGL_ERR, MSGTR_CantOpenCodec);
        return 0;
    }
    mp_msg(MSGT_DECVIDEO,MSGL_V,"INFO: libavcodec init OK!\n");
    ctx->last_aspect=-3;
    return 1; //mpcodecs_config_vo(sh,sh->disp_w,sh->disp_h,IMGFMT_YV12);
}

// uninit driver
static void uninit(sh_video_t *sh){
    vd_ffmpeg_ctx *ctx = sh->context;
    AVCodecContext *avctx = ctx->avctx;

    if (avcodec_close(avctx) < 0)
    	    mp_msg(MSGT_DECVIDEO,MSGL_ERR, MSGTR_CantCloseCodec);

#if LIBAVCODEC_BUILD >= 4605
    if (avctx->extradata_size)
	free(avctx->extradata);
#endif

    if (avctx)
	free(avctx);
    if (ctx)
	free(ctx);
}

static void draw_slice(struct AVCodecContext *s,
                	UINT8 **src, int linesize,
                	int y, int width, int height){
    sh_video_t * sh = s->opaque;
    int stride[3];
    int start=0, i;
    int skip_stride= (s->width+15)>>4;
#if LIBAVCODEC_BUILD > 4615
    UINT8 *skip= &s->mbskip_table[(y>>4)*skip_stride];
    int threshold= s->pict_type==B_TYPE ? -99 : s->dr_ip_buffer_count;
#endif

    stride[0]=linesize;
#if LIBAVCODEC_BUILD > 4615
    if(s->dr_uvstride)
        stride[1]=stride[2]= s->dr_uvstride;
    else
#endif
        stride[1]=stride[2]=stride[0]/2;
#if 0
    if(s->pict_type!=B_TYPE){
        for(i=0; i*16<width+16; i++){ 
            if(i*16>=width || skip[i]>=threshold){
                if(start==i) start++;
                else{
                    UINT8 *src2[3]= {src[0] + start*16, 
                                     src[1] + start*8, 
                                     src[2] + start*8};
//printf("%2d-%2d x %d\n", start, i, y);
                    mpcodecs_draw_slice (sh,src2, stride, (i-start)*16, height, start*16, y);
                    start= i+1;
                }
            }       
        }
    }else
#endif
        mpcodecs_draw_slice (sh,src, stride, width, height, 0, y);
}

static int init_vo(sh_video_t *sh){
    vd_ffmpeg_ctx *ctx = sh->context;
    AVCodecContext *avctx = ctx->avctx;

    if (avctx->aspect_ratio_info != ctx->last_aspect ||
	avctx->width != sh->disp_w ||
	avctx->height != sh->disp_h ||
	!ctx->vo_inited)
    {
	ctx->last_aspect = avctx->aspect_ratio_info;
	switch(avctx->aspect_ratio_info)
	{
	    case FF_ASPECT_4_3_625:
	    case FF_ASPECT_4_3_525:
		sh->aspect = 4.0/3.0;
		break;
	    case FF_ASPECT_16_9_625:
	    case FF_ASPECT_16_9_525:
		sh->aspect = 16.0/9.0;
		break;
	    case FF_ASPECT_SQUARE:
		sh->aspect = 0.0;
		break;
	}
	sh->disp_w = avctx->width;
	sh->disp_h = avctx->height;
	ctx->vo_inited=1;
	switch(avctx->pix_fmt){
#if LIBAVCODEC_BUILD >= 4615
	case PIX_FMT_YUV410P: ctx->best_csp=IMGFMT_YVU9;break; //svq1
#endif
	case PIX_FMT_YUV420P: ctx->best_csp=IMGFMT_YV12;break; //mpegs
	case PIX_FMT_YUV422P: ctx->best_csp=IMGFMT_422P;break; //mjpeg
	case PIX_FMT_YUV444P: ctx->best_csp=IMGFMT_444P;break; //???
	case PIX_FMT_YUV422:  ctx->best_csp=IMGFMT_YUY2;break; //???
	default:
	    ctx->best_csp=0;
	}
    	if (!mpcodecs_config_vo(sh,sh->disp_w,sh->disp_h, ctx->best_csp))
    		return -1;
	ctx->convert=(sh->codec->outfmt[sh->outfmtidx]==IMGFMT_YUY2
	    && ctx->best_csp!=IMGFMT_YUY2); // yuv422p->yuy2 conversion
    }
    return 0;
}

#if LIBAVCODEC_BUILD > 4615
static void get_buffer(struct AVCodecContext *avctx, int width, int height, int pict_type){
    sh_video_t * sh = avctx->opaque;
    vd_ffmpeg_ctx *ctx = sh->context;
    mp_image_t* mpi=NULL;
    int flags= MP_IMGFLAG_ACCEPT_STRIDE | MP_IMGFLAG_PREFER_ALIGNED_STRIDE;
    int type= MP_IMGTYPE_IPB;
    int align=15;

    if(avctx->pix_fmt == PIX_FMT_YUV410P)
        align=63; //yes seriously, its really needed (16x16 chroma blocks in SVQ1 -> 64x64)

    if(init_vo(sh)<0){
        printf("init_vo failed\n");
        return;
    }

    if(pict_type==B_TYPE)
        flags|=(!avctx->hurry_up && ctx->do_slices) ?
                MP_IMGFLAG_DRAW_CALLBACK:0;
    else
        flags|= MP_IMGFLAG_PRESERVE|MP_IMGFLAG_READABLE
                | (ctx->do_slices ? MP_IMGFLAG_DRAW_CALLBACK : 0);

#if LIBAVCODEC_BUILD > 4616
    if(avctx->has_b_frames){
        type= MP_IMGTYPE_IPB;
    }else{
        type= MP_IMGTYPE_IP;
    }
#endif
    mp_msg(MSGT_DECVIDEO,MSGL_DBG2, type== MP_IMGTYPE_IPB ? "using IPB\n" : "using IP\n");

    mpi= mpcodecs_get_image(sh,type, flags,
			(width+align)&(~align), (height+align)&(~align));

    // ok, lets see what did we get:
    if(  mpi->flags&MP_IMGFLAG_DRAW_CALLBACK &&
       !(mpi->flags&MP_IMGFLAG_DIRECT)){
	// nice, filter/vo likes draw_callback :)
	avctx->draw_horiz_band= draw_slice;
    } else
	avctx->draw_horiz_band= NULL;
    avctx->dr_buffer[0]= mpi->planes[0];
    avctx->dr_buffer[1]= mpi->planes[1];
    avctx->dr_buffer[2]= mpi->planes[2];
    
    if(avctx->dr_stride && avctx->dr_stride !=mpi->stride[0]){
        mp_msg(MSGT_DECVIDEO,MSGL_ERR, "Error: stride changed\n");
    }
    
    if(avctx->dr_uvstride && avctx->dr_uvstride !=mpi->stride[1]){
        mp_msg(MSGT_DECVIDEO,MSGL_ERR, "Error: uvstride changed\n");
    }
    
    assert(mpi->width  >= ((width +align)&(~align)));   
    assert(mpi->height >= ((height+align)&(~align)));
    assert(mpi->stride[0] >= mpi->width);
    if(mpi->imgfmt==IMGFMT_I420 || mpi->imgfmt==IMGFMT_YV12 || mpi->imgfmt==IMGFMT_IYUV){
        const int y_size= mpi->stride[0] * mpi->height;
        const int c_size= mpi->stride[1] * mpi->chroma_height;
        
        assert(mpi->planes[0] > mpi->planes[1] || mpi->planes[0] + y_size <= mpi->planes[1]);
        assert(mpi->planes[0] > mpi->planes[2] || mpi->planes[0] + y_size <= mpi->planes[2]);
        assert(mpi->planes[1] > mpi->planes[0] || mpi->planes[1] + c_size <= mpi->planes[0]);
        assert(mpi->planes[1] > mpi->planes[2] || mpi->planes[1] + c_size <= mpi->planes[2]);
        assert(mpi->planes[2] > mpi->planes[0] || mpi->planes[2] + c_size <= mpi->planes[0]);
        assert(mpi->planes[2] > mpi->planes[1] || mpi->planes[2] + c_size <= mpi->planes[1]);
    }

    avctx->dr_stride   = mpi->stride[0];
    avctx->dr_uvstride = mpi->stride[1];

    avctx->dr_opaque_frame = mpi;
    avctx->dr_ip_buffer_count=2; //FIXME
//printf("%X\n", (int)mpi->planes[0]);
#if 0
if(mpi->flags&MP_IMGFLAG_DIRECT)
    printf("D");
else if(mpi->flags&MP_IMGFLAG_DRAW_CALLBACK)
    printf("S");
else
    printf(".");
#endif
}
#endif

// decode a frame
static mp_image_t* decode(sh_video_t *sh,void* data,int len,int flags){
    int got_picture=0;
    int ret;
    AVPicture lavc_picture;
    vd_ffmpeg_ctx *ctx = sh->context;
    AVCodecContext *avctx = ctx->avctx;
    mp_image_t* mpi=NULL;
    int dr1= ctx->do_dr1;

    if(len<=0) return NULL; // skipped frame

    avctx->draw_horiz_band=NULL;
    avctx->opaque=sh;
    if(ctx->vo_inited && !ctx->convert && !(flags&3) && !dr1){
	mpi=mpcodecs_get_image(sh, MP_IMGTYPE_EXPORT, MP_IMGFLAG_PRESERVE |
	    (ctx->do_slices?MP_IMGFLAG_DRAW_CALLBACK:0),
	    sh->disp_w, sh->disp_h);
	if(mpi && mpi->flags&MP_IMGFLAG_DRAW_CALLBACK){
	    // vd core likes slices!
	    avctx->draw_horiz_band=draw_slice;
	}
    }

#if LIBAVCODEC_BUILD > 4603
    avctx->hurry_up=(flags&3)?((flags&2)?2:1):0;
#endif

    ret = avcodec_decode_video(avctx, &lavc_picture,
	     &got_picture, data, len);
    if(ret<0) mp_msg(MSGT_DECVIDEO,MSGL_WARN, "Error while decoding frame!\n");

//-- vstats generation
    while(lavc_param_vstats){ // always one time loop
        static FILE *fvstats=NULL;
        char filename[20];
        static long long int all_len=0;
        static int frame_number=0;
        static double all_frametime=0.0;

        if(!fvstats) {
            time_t today2;
            struct tm *today;
            today2 = time(NULL);
            today = localtime(&today2);
            sprintf(filename, "vstats_%02d%02d%02d.log", today->tm_hour,
                today->tm_min, today->tm_sec);
            fvstats = fopen(filename,"w");
            if(!fvstats) {
                perror("fopen");
                lavc_param_vstats=0; // disable block
                break;
                /*exit(1);*/
            }
        }

        all_len+=len;
        all_frametime+=sh->frametime;
        fprintf(fvstats, "frame= %5d q= %2d f_size= %6d s_size= %8.0fkB ",
            ++frame_number, avctx->quality, len, (double)all_len/1024);
        fprintf(fvstats, "time= %0.3f br= %7.1fkbits/s avg_br= %7.1fkbits/s ",
           all_frametime, (double)(len*8)/sh->frametime/1000.0,
           (double)(all_len*8)/all_frametime/1000.0);
        fprintf(fvstats, "type= %c\n", sh->ds->flags&1 ? 'I' : 'P');
        break;
    }
//--

    if(!got_picture) return NULL;	// skipped image

    if(init_vo(sh)<0) return NULL;

#if LIBAVCODEC_BUILD > 4615
    if(dr1 && avctx->dr_opaque_frame){
        mpi= (mp_image_t*)avctx->dr_opaque_frame;
    }
#endif
        
    if(!mpi && ctx->convert){
	// do yuv422p -> yuy2 conversion:
        mpi=mpcodecs_get_image(sh, MP_IMGTYPE_TEMP, MP_IMGFLAG_ACCEPT_STRIDE,
	    avctx->width, avctx->height);
	if(!mpi) return NULL;
	yuv422ptoyuy2(lavc_picture.data[0],lavc_picture.data[1],lavc_picture.data[2],
	    mpi->planes[0],avctx->width,avctx->height,
	    lavc_picture.linesize[0],lavc_picture.linesize[1],mpi->stride[0]);
	return mpi;
    }
    
    if(!mpi)
    mpi=mpcodecs_get_image(sh, MP_IMGTYPE_EXPORT, MP_IMGFLAG_PRESERVE,
	avctx->width, avctx->height);
    if(!mpi){	// temporary!
	printf("couldn't allocate image for codec\n");
	return NULL;
    }
    
    if(!dr1){
        mpi->planes[0]=lavc_picture.data[0];
        mpi->planes[1]=lavc_picture.data[1];
        mpi->planes[2]=lavc_picture.data[2];
        mpi->stride[0]=lavc_picture.linesize[0];
        mpi->stride[1]=lavc_picture.linesize[1];
        mpi->stride[2]=lavc_picture.linesize[2];
    }

    if(avctx->pix_fmt==PIX_FMT_YUV422P && mpi->chroma_y_shift==1){
	// we have 422p but user wants 420p
	mpi->stride[1]*=2;
	mpi->stride[2]*=2;
    }
    
/* to comfirm with newer lavc style */
#if !defined(FF_POSTPROCESS) && (LIBAVCODEC_BUILD > 4612)
    mpi->qscale=avctx->quant_store;
#if LIBAVCODEC_BUILD > 4613
    mpi->qstride=avctx->qstride;
#else
    mpi->qstride=MBC+1;
#endif
#elif defined(FF_POSTPROCESS)
    mpi->qscale=&quant_store[0][0];
    mpi->qstride=MBC+1;
#endif
    
    return mpi;
}

#endif


#include <stdio.h>
#include <stdlib.h>

#include "config.h"
#include "mp_msg.h"
#include "help_mp.h"

#ifdef USE_WIN32DLL

#include "loader.h"
//#include "wine/mmreg.h"
#include "wine/vfw.h"
#include "wine/avifmt.h"

#include "vd_internal.h"

static vd_info_t info = {
	"Win32/VfWex video codecs",
	"vfwex",
	"A'rpi",
	"A'rpi & Alex",
	"win32 codecs"
};

LIBVD_EXTERN(vfwex)

typedef struct {
    BITMAPINFOHEADER *o_bih;
    HIC handle;
    unsigned char *palette;
} vd_vfw_ctx;

extern int divx_quality;

static int vfw_set_postproc(sh_video_t* sh, int quality)
{
    vd_vfw_ctx *priv = sh->context;
    // Works only with opendivx/divx4 based DLL
    return ICSendMessage(priv->handle, ICM_USER+80, (long)(&quality) ,NULL);
}

// to set/get/query special features/parameters
static int control(sh_video_t *sh,int cmd,void* arg,...){
    vd_vfw_ctx *priv = sh->context;
    switch(cmd){
    case VDCTRL_QUERY_MAX_PP_LEVEL:
	return 9;
    case VDCTRL_SET_PP_LEVEL:
	vfw_set_postproc(sh,10*(*((int*)arg)));
	return CONTROL_OK;
    case VDCTRL_QUERY_FORMAT:
      {
	int outfmt = *((int*)arg);
	int yuv = 0;
	HRESULT ret;

	switch (outfmt)
	{
	/* planar format */
	case IMGFMT_YV12:
	case IMGFMT_I420:
	case IMGFMT_IYUV:
	    priv->o_bih->biBitCount=12;
	    yuv=1;
	    break;
	case IMGFMT_YVU9:
	case IMGFMT_IF09:
	    priv->o_bih->biBitCount=9;
	    yuv=1;
	    break;
	/* packed format */
	case IMGFMT_YUY2:
        case IMGFMT_UYVY:
        case IMGFMT_YVYU:
    	    priv->o_bih->biBitCount=16;
	    yuv=1;
	    break;
	/* rgb/bgr format */
	case IMGFMT_RGB8:
	case IMGFMT_BGR8:
	    priv->o_bih->biBitCount=8;
	    break;
	case IMGFMT_RGB15:
	case IMGFMT_RGB16:
	case IMGFMT_BGR15:
	case IMGFMT_BGR16:
	    priv->o_bih->biBitCount=16;
	    break;
	case IMGFMT_RGB24:
	case IMGFMT_BGR24:
	    priv->o_bih->biBitCount=24;
	    break;
	case IMGFMT_RGB32:
	case IMGFMT_BGR32:
	    priv->o_bih->biBitCount=32;
	    break;
	default:
	    mp_msg(MSGT_WIN32,MSGL_ERR,"Unsupported image format: %s\n", vo_format_name(outfmt));
	    return 0;
	}

	priv->o_bih->biSizeImage = abs(priv->o_bih->biWidth * priv->o_bih->biHeight * (priv->o_bih->biBitCount/8));

	if (yuv && !(sh->codec->outflags[sh->outfmtidx] & CODECS_FLAG_YUVHACK))
	    priv->o_bih->biCompression = outfmt;
	else
	     priv->o_bih->biCompression = 0;

	ret = ICDecompressQueryEx(priv->handle, sh->bih, priv->o_bih);
	if (ret)
	{
	    mp_msg(MSGT_WIN32, MSGL_DBG2, "ICDecompressQuery failed:: Error %d\n", (int)ret);
	    return CONTROL_FALSE;
	}
	else
	    return CONTROL_TRUE;
      }
    }
    return CONTROL_UNKNOWN;
}

// init driver
static int init(sh_video_t *sh){
    HRESULT ret;
    int yuv=0;
    unsigned int outfmt=sh->codec->outfmt[sh->outfmtidx];
    int i, o_bih_len;
    vd_vfw_ctx *priv;
  
    priv = malloc(sizeof(vd_vfw_ctx));
    if (!priv)
	return 0;
    memset(priv, 0, sizeof(vd_vfw_ctx));
    sh->context = priv;

    mp_msg(MSGT_WIN32,MSGL_V,"======= Win32 (VFW) VIDEO Codec init =======\n");

    win32_codec_name = sh->codec->dll;
//    sh->hic = ICOpen( 0x63646976, sh->bih->biCompression, ICMODE_FASTDECOMPRESS);
    priv->handle = ICOpen( 0x63646976, sh->bih->biCompression, ICMODE_DECOMPRESS);
    if(!priv->handle){
	mp_msg(MSGT_WIN32,MSGL_ERR,"ICOpen failed! unknown codec / wrong parameters?\n");
	return 0;
    }

//    sh->bih->biBitCount=32;

    o_bih_len = ICDecompressGetFormatSize(priv->handle, sh->bih);
  
    priv->o_bih = malloc(o_bih_len);
    memset(priv->o_bih, 0, o_bih_len);

    printf("ICDecompressGetFormatSize ret: %d\n", o_bih_len);

    ret = ICDecompressGetFormat(priv->handle, sh->bih, priv->o_bih);
    if(ret < 0){
	mp_msg(MSGT_WIN32,MSGL_ERR,"ICDecompressGetFormat failed: Error %d\n", (int)ret);
	for (i=0; i < o_bih_len; i++) mp_msg(MSGT_WIN32, MSGL_DBG2, "%02x ", priv->o_bih[i]);
	return 0;
    }
    mp_msg(MSGT_WIN32,MSGL_V,"ICDecompressGetFormat OK\n");

    ret = ICDecompressGetPalette(priv->handle, sh->bih, priv->o_bih);
    if (!ret)
    {
	priv->palette = ((unsigned char*)priv->o_bih) + sh->bih->biSize;
	mp_msg(MSGT_WIN32,MSGL_V,"ICDecompressGetPalette OK\n");
    }

#if 0
    // workaround for pegasus MJPEG:
    if(!sh_video->o_bih.biWidth) sh_video->o_bih.biWidth=sh_video->bih->biWidth;
    if(!sh_video->o_bih.biHeight) sh_video->o_bih.biHeight=sh_video->bih->biHeight;
    if(!sh_video->o_bih.biPlanes) sh_video->o_bih.biPlanes=sh_video->bih->biPlanes;
#endif

    switch (outfmt)
    {
    /* planar format */
    case IMGFMT_YV12:
    case IMGFMT_I420:
    case IMGFMT_IYUV:
	priv->o_bih->biBitCount=12;
	yuv=1;
	break;
    case IMGFMT_YVU9:
    case IMGFMT_IF09:
	priv->o_bih->biBitCount=9;
	yuv=1;
	break;
    /* packed format */
    case IMGFMT_YUY2:
    case IMGFMT_UYVY:
    case IMGFMT_YVYU:
	priv->o_bih->biBitCount=16;
	yuv=1;
	break;
    /* rgb/bgr format */
    case IMGFMT_RGB8:
    case IMGFMT_BGR8:
	priv->o_bih->biBitCount=8;
	break;
    case IMGFMT_RGB15:
    case IMGFMT_RGB16:
    case IMGFMT_BGR15:
    case IMGFMT_BGR16:
	priv->o_bih->biBitCount=16;
	break;
    case IMGFMT_RGB24:
    case IMGFMT_BGR24:
	priv->o_bih->biBitCount=24;
	break;
    case IMGFMT_RGB32:
    case IMGFMT_BGR32:
	priv->o_bih->biBitCount=32;
	break;
    default:
	mp_msg(MSGT_WIN32,MSGL_ERR,"Unsupported image format: %s\n", vo_format_name(outfmt));
	return 0;
    }

    priv->o_bih->biSizeImage = priv->o_bih->biWidth * priv->o_bih->biHeight * (priv->o_bih->biBitCount/8);
  
    if (!(sh->codec->outflags[sh->outfmtidx]&CODECS_FLAG_FLIP)) {
	priv->o_bih->biHeight=-sh->bih->biHeight; // flip image!
    }

    if (yuv && !(sh->codec->outflags[sh->outfmtidx] & CODECS_FLAG_YUVHACK))
	priv->o_bih->biCompression = outfmt;
    else
	 priv->o_bih->biCompression = 0;

#if 0
    ret = ICDecompressQueryEx(priv->handle, sh->bih, priv->o_bih);
    if (ret)
    {
	mp_msg(MSGT_WIN32,MSGL_ERR,"ICDecompressQuery failed: Error %d\n", (int)ret);
//	return 0;
    } else
	mp_msg(MSGT_WIN32,MSGL_V,"ICDecompressQuery OK\n");
#endif

    if(!mpcodecs_config_vo(sh,sh->disp_w,sh->disp_h,IMGFMT_YUY2)) return 0;

    ret = ICDecompressBeginEx(priv->handle, sh->bih, priv->o_bih);
    if (ret)
    {
	mp_msg(MSGT_WIN32,MSGL_ERR,"ICDecompressBegin failed: Error %d\n", (int)ret);
//	return 0;
    }

    if (yuv && sh->codec->outflags[sh->outfmtidx] & CODECS_FLAG_YUVHACK)
	priv->o_bih->biCompression = outfmt;

    mp_msg(MSGT_WIN32, MSGL_V, "Input format:\n");
    mp_msg(MSGT_WIN32, MSGL_V, "  biSize %ld\n", sh->bih->biSize);
    mp_msg(MSGT_WIN32, MSGL_V, "  biWidth %ld\n", sh->bih->biWidth);
    mp_msg(MSGT_WIN32, MSGL_V, "  biHeight %ld\n", sh->bih->biHeight);
    mp_msg(MSGT_WIN32, MSGL_V, "  biPlanes %d\n", sh->bih->biPlanes);
    mp_msg(MSGT_WIN32, MSGL_V, "  biBitCount %d\n", sh->bih->biBitCount);
    mp_msg(MSGT_WIN32, MSGL_V, "  biCompression 0x%lx ('%.4s')\n", sh->bih->biCompression, (char *)&sh->bih->biCompression);
    mp_msg(MSGT_WIN32, MSGL_V, "  biSizeImage %ld\n", sh->bih->biSizeImage);
    mp_msg(MSGT_WIN32, MSGL_V, "  biXPelsPerMeter %ld\n", sh->bih->biXPelsPerMeter);
    mp_msg(MSGT_WIN32, MSGL_V, "  biYPelsPerMeter %ld\n", sh->bih->biYPelsPerMeter);
    mp_msg(MSGT_WIN32, MSGL_V, "  biClrUsed %ld\n", sh->bih->biClrUsed);
    mp_msg(MSGT_WIN32, MSGL_V, "  biClrImportant %ld\n", sh->bih->biClrImportant);
    mp_msg(MSGT_WIN32, MSGL_V, "Output format:\n");
    mp_msg(MSGT_WIN32, MSGL_V, "  biSize %ld\n", priv->o_bih->biSize);
    mp_msg(MSGT_WIN32, MSGL_V, "  biWidth %ld\n", priv->o_bih->biWidth);
    mp_msg(MSGT_WIN32, MSGL_V, "  biHeight %ld\n", priv->o_bih->biHeight);
    mp_msg(MSGT_WIN32, MSGL_V, "  biPlanes %d\n", priv->o_bih->biPlanes);
    mp_msg(MSGT_WIN32, MSGL_V, "  biBitCount %d\n", priv->o_bih->biBitCount);
    mp_msg(MSGT_WIN32, MSGL_V, "  biCompression 0x%lx ('%.4s')\n", priv->o_bih->biCompression, (char *)&priv->o_bih->biCompression);
    mp_msg(MSGT_WIN32, MSGL_V, "  biSizeImage %ld\n", priv->o_bih->biSizeImage);
    mp_msg(MSGT_WIN32, MSGL_V, "  biXPelsPerMeter %ld\n", priv->o_bih->biXPelsPerMeter);
    mp_msg(MSGT_WIN32, MSGL_V, "  biYPelsPerMeter %ld\n", priv->o_bih->biYPelsPerMeter);
    mp_msg(MSGT_WIN32, MSGL_V, "  biClrUsed %ld\n", priv->o_bih->biClrUsed);
    mp_msg(MSGT_WIN32, MSGL_V, "  biClrImportant %ld\n", priv->o_bih->biClrImportant);

    ICSendMessage(priv->handle, ICM_USER+80, (long)(&divx_quality) ,NULL);

    mp_msg(MSGT_DECVIDEO,MSGL_V,"INFO: Win32 video codec init OK!\n");
    return 1;
}

// uninit driver
static void uninit(sh_video_t *sh){
    HRESULT ret;
    vd_vfw_ctx *priv = sh->context;
    
    ret = ICDecompressEnd(priv->handle);
    if (ret)
    {
	mp_msg(MSGT_WIN32, MSGL_WARN, "ICDecompressEnd failed: %d\n", ret);
	return;
    }

    ret = ICClose(priv->handle);
    if (ret)
    {
	mp_msg(MSGT_WIN32, MSGL_WARN, "ICClose failed: %d\n", ret);
	return;
    }
    
    free(priv->o_bih);
    free(priv);
}

// decode a frame
static mp_image_t* decode(sh_video_t *sh,void* data,int len,int flags){
    vd_vfw_ctx *priv = sh->context;
    mp_image_t* mpi;
    HRESULT ret;

    if(len<=0) return NULL; // skipped frame

    mpi=mpcodecs_get_image(sh, 
	(sh->codec->outflags[sh->outfmtidx] & CODECS_FLAG_STATIC) ?
	MP_IMGTYPE_STATIC : MP_IMGTYPE_TEMP, MP_IMGFLAG_ACCEPT_WIDTH, 
	sh->disp_w, sh->disp_h);
    if(!mpi){	// temporary!
	printf("couldn't allocate image for cinepak codec\n");
	return NULL;
    }

    // set stride:  (trick discovered by Andreas Ackermann - thanx!)
    sh->bih->biWidth=mpi->width; //mpi->stride[0]/(mpi->bpp/8);
    priv->o_bih->biWidth=mpi->width; //mpi->stride[0]/(mpi->bpp/8);

    sh->bih->biSizeImage = len;
    
    ret = ICDecompressEx(priv->handle, 
	  ( (sh->ds->flags&1) ? 0 : ICDECOMPRESS_NOTKEYFRAME ) |
	  ( ((flags&3)==2 && !(sh->ds->flags&1))?(ICDECOMPRESS_HURRYUP|ICDECOMPRESS_PREROL):0 ),
	   sh->bih, data, priv->o_bih, (flags&3) ? 0 : mpi->planes[0]);

    if ((int)ret){
      mp_msg(MSGT_DECVIDEO,MSGL_WARN,"Error decompressing frame, err=%d\n",ret);
      return NULL;
    }
    
    // export palette:
    if(mpi->imgfmt==IMGFMT_RGB8 || mpi->imgfmt==IMGFMT_BGR8){
	if (priv->palette)
	{
	    mpi->planes[1] = priv->palette;
	    mpi->flags |= MP_IMGFLAG_RGB_PALETTE;
	    mp_dbg(MSGT_DECVIDEO, MSGL_DBG2, "Found and copied palette\n");
	}
	else
	    mpi->planes[1]=NULL;
    }
    
    return mpi;
}
#endif

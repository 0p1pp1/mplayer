#include <stdio.h>
#include <stdlib.h>

#include "config.h"
#include "mp_msg.h"

#include "vd_internal.h"

#ifdef USE_LZO
#include <lzo1x.h>
#else
#include "native/minilzo.h"
#define lzo_malloc malloc
#define lzo_free free
#endif

#define MOD_NAME "DecLZO"

static vd_info_t info = {
	"LZO compressed Video",
	"lzo",
	"Tilmann Bitterberg",
	"Transcode development team <http://www.theorie.physik.uni-goettingen.de/~ostreich/transcode/>",
	"based on liblzo: http://www.oberhumer.com/opensource/lzo/"
};

LIBVD_EXTERN(lzo)


static lzo_byte *wrkmem=NULL;
static int codec = -1;

// to set/get/query special features/parameters
static int control (sh_video_t *sh, int cmd, void* arg, ...)
{

    //printf("[%s] Query!! (%s)\n", MOD_NAME, (codec==IMGFMT_BGR24)?"BGR":"none");
    //printf("[%s] Query!! (%s)\n", MOD_NAME, (codec==IMGFMT_YV12)?"YV12":"none");
    switch(cmd){
    case VDCTRL_QUERY_FORMAT:
	if( (*((int*)arg)) == IMGFMT_BGR24 && codec == IMGFMT_BGR24) return CONTROL_TRUE;
	if( (*((int*)arg)) == IMGFMT_YV12 && codec == IMGFMT_YV12) return CONTROL_TRUE;
	return CONTROL_FALSE;
    }
    return CONTROL_UNKNOWN;
}


// init driver
static int init(sh_video_t *sh)
{

    if (lzo_init() != LZO_E_OK) {
	mp_msg (MSGT_DECVIDEO, MSGL_WARN, "[%s] lzo_init() failed\n", MOD_NAME);
	return 0; 
    }

    if (!wrkmem) wrkmem = (lzo_bytep) lzo_malloc(LZO1X_1_MEM_COMPRESS);

    if (wrkmem == NULL) {
	mp_msg (MSGT_DECVIDEO, MSGL_ERR, "[%s] Cannot alloc work memory\n", MOD_NAME);
	return 0;
    }

    return 1;
}

// uninit driver
static void uninit(sh_video_t *sh)
{
    if (wrkmem) { lzo_free(wrkmem); wrkmem = NULL;}
}

//mp_image_t* mpcodecs_get_image(sh_video_t *sh, int mp_imgtype, int mp_imgflag, int w, int h);

// decode a frame
static mp_image_t* decode(sh_video_t *sh,void* data,int len,int flags)
{
    static int init_done = 0;
    int r;
    int cb = 1;
    int cr = 2;
    mp_image_t* mpi;
    int w, h;

    if (len <= 0) {
	    return NULL; // skipped frame
    }
    

    if (!init_done) {
	lzo_byte *tmp=NULL;
	
	// decompress one frame to see if its
	// either YV12 or RGB24
	if (!tmp) tmp = lzo_malloc(sh->bih->biSizeImage);

	mp_msg (MSGT_DECVIDEO, MSGL_V, "[%s] 2 depth %d, format %d data %p len (%d) (%d)\n",
	    MOD_NAME, sh->bih->biBitCount, sh->format, data, len, sh->bih->biSizeImage
	    );

	/* decompress the frame */
	r = lzo1x_decompress (data, len, tmp, &w, wrkmem);

	if (r != LZO_E_OK) {
	    /* this should NEVER happen */
	    mp_msg (MSGT_DECVIDEO, MSGL_ERR, 
		    "[%s] internal error - decompression failed: %d\n", MOD_NAME, r);
	    return NULL;
	}

	if        (w == (sh->bih->biSizeImage))   {
	    codec = IMGFMT_BGR24;
	    mp_msg (MSGT_DECVIDEO, MSGL_V, "[%s] codec choosen is BGR24\n", MOD_NAME);
	} else if (w == (sh->bih->biSizeImage)/2) {
	    codec = IMGFMT_YV12;
	    mp_msg (MSGT_DECVIDEO, MSGL_V, "[%s] codec choosen is YV12\n", MOD_NAME);
	} else {
	    codec = -1;
	    mp_msg(MSGT_DECVIDEO,MSGL_ERR,"[%s] Unsupported out_fmt\n", MOD_NAME);
	    return NULL;
	}

	mpcodecs_config_vo(sh,sh->disp_w,sh->disp_h,codec);
	init_done++;
	free(tmp);
    }

    mpi = mpcodecs_get_image(sh, MP_IMGTYPE_TEMP, 0,
	sh->disp_w, sh->disp_h);


    if (!mpi) {
	    mp_msg (MSGT_DECVIDEO, MSGL_ERR, "[%s] mpcodecs_get_image failed\n", MOD_NAME);
	    return NULL;
    }

    r = lzo1x_decompress (data, len, mpi->planes[0], &w, wrkmem);
    if (r != LZO_E_OK) {
	/* this should NEVER happen */
	mp_msg (MSGT_DECVIDEO, MSGL_ERR, 
		"[%s] internal error - decompression failed: %d\n", MOD_NAME, r);
      return NULL;
    }

    mp_msg (MSGT_DECVIDEO, MSGL_DBG2, 
		"[%s] decompressed %lu bytes into %lu bytes\n", MOD_NAME,
		(long) len, (long)w);

    return mpi;
}

/* vim: sw=4
   */

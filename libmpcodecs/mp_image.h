#ifndef __MP_IMAGE_H
#define __MP_IMAGE_H 1

// set if buffer content shouldn't be modified:
#define MP_IMGFLAG_PRESERVE 0x01
// set if buffer content will be READED for next frame's MC: (I/P mpeg frames)
#define MP_IMGFLAG_READABLE 0x02
// set if buffer is allocated (used in destination images):
#define MP_IMGFLAG_ALLOCATED 0x04
// set if it's in video buffer/memory:
#define MP_IMGFLAG_DIRECT 0x08
// codec accept any stride (>=width):
#define MP_IMGFLAG_ACCEPT_STRIDE 0x10
// codec accept any width (width*bpp=stride) (>=width):
#define MP_IMGFLAG_ACCEPT_WIDTH 0x20
// stride should be aligned to 16-byte (MB) boundary:
#define MP_IMGFLAG_ALIGNED_STRIDE 0x40
// codec uses drawing/rendering callbacks (draw_slice()-like thing, DR method 2)
#define MP_IMGFLAG_DRAW_CALLBACK 0x80

// set if number of planes > 1
#define MP_IMGFLAG_PLANAR 0x100
// set if it's YUV colorspace
#define MP_IMGFLAG_YUV 0x200
// set if it's swapped plane/byteorder
#define MP_IMGFLAG_SWAPPED 0x400
// type displayed (do not set this flag - it's for internal use!)
#define MP_IMGFLAG_TYPE_DISPLAYED 0x800

// codec doesn't support any form of direct rendering - it has own buffer
// allocation. so we just export its buffer pointers:
#define MP_IMGTYPE_EXPORT 0
// codec requires a static WO buffer, but it does only partial updates later:
#define MP_IMGTYPE_STATIC 1
// codec just needs some WO memory, where it writes/copies the whole frame to:
#define MP_IMGTYPE_TEMP 2
// I+P type, requires 2+ independent static R/W buffers
#define MP_IMGTYPE_IP 3
// I+P+B type, requires 2+ independent static R/W and 1+ temp WO buffers
#define MP_IMGTYPE_IPB 4

typedef struct mp_image_s {
    unsigned short flags;
    unsigned char type;
    unsigned char bpp;  // bits/pixel. NOT depth! for RGB it will be n*8
    unsigned int imgfmt;
    int width,height;  // stored dimensions
    int x,y,w,h;  // visible dimensions
    unsigned char* planes[3];
    unsigned int stride[3];
    int* qscale;
    int qstride;
} mp_image_t;

#ifdef IMGFMT_YUY2
static inline void mp_image_setfmt(mp_image_t* mpi,unsigned int out_fmt){
    mpi->flags&=~(MP_IMGFLAG_PLANAR|MP_IMGFLAG_YUV|MP_IMGFLAG_SWAPPED);
    mpi->imgfmt=out_fmt;
    if( (out_fmt&IMGFMT_RGB_MASK) == IMGFMT_RGB ){
	mpi->bpp=((out_fmt&255)+7)&(~7);
	return;
    }
    if( (out_fmt&IMGFMT_BGR_MASK) == IMGFMT_BGR ){
	mpi->bpp=((out_fmt&255)+7)&(~7);
	mpi->flags|=MP_IMGFLAG_SWAPPED;
	return;
    }
    mpi->flags|=MP_IMGFLAG_YUV;
    switch(out_fmt){
    case IMGFMT_I420:
    case IMGFMT_IYUV:
	mpi->flags|=MP_IMGFLAG_SWAPPED;
    case IMGFMT_YV12:
	mpi->flags|=MP_IMGFLAG_PLANAR;
	mpi->bpp=12;
	return;
    case IMGFMT_UYVY:
	mpi->flags|=MP_IMGFLAG_SWAPPED;
    case IMGFMT_YUY2:
	mpi->bpp=16;
	return;
    case IMGFMT_MPEGPES:
	mpi->bpp=0;
	return;
    }
    printf("mp_image: Unknown out_fmt: 0x%X\n",out_fmt);
    mpi->bpp=0;
}
#endif

static inline mp_image_t* new_mp_image(int w,int h){
    mp_image_t* mpi=malloc(sizeof(mp_image_t));
    if(!mpi) return NULL; // error!
    memset(mpi,0,sizeof(mp_image_t));
    mpi->width=mpi->w=w;
    mpi->height=mpi->h=h;
    return mpi;
}

static inline void free_mp_image(mp_image_t* mpi){
    if(!mpi) return;
    if(mpi->flags&MP_IMGFLAG_ALLOCATED){
	if(mpi->planes[0]) free(mpi->planes[0]);
    }
    free(mpi);
}

#endif

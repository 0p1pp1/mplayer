/*
    Copyright (C) 2002 Michael Niedermayer <michaelni@gmx.at>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <math.h>

#include "../config.h"
#include "../mp_msg.h"
#include "../cpudetect.h"

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include "img_format.h"
#include "mp_image.h"
#include "vf.h"
#include "../libvo/fastmemcpy.h"

#define MAX_NOISE 4096
#define MAX_SHIFT 1024
#define MAX_RES (MAX_NOISE-MAX_SHIFT)

//===========================================================================//

static inline void lineNoise_C(uint8_t *dst, uint8_t *src, int8_t *noise, int len, int shift);

static void (*lineNoise)(uint8_t *dst, uint8_t *src, int8_t *noise, int len, int shift)= lineNoise_C;

typedef struct FilterParam{
	int strength;
	int uniform;
	int temporal;
	int8_t *noise;
}FilterParam;

struct vf_priv_s {
	FilterParam lumaParam;
	FilterParam chromaParam;
	mp_image_t *dmpi;
	unsigned int outfmt;
};

static int nonTempRandShift[MAX_RES]= {-1};

static int8_t *initNoise(FilterParam *fp){
	int strength= fp->strength;
	int uniform= fp->uniform;
	int8_t *noise= memalign(16, MAX_NOISE*sizeof(int8_t));
	int i;

	srand(123457);

	for(i=0; i<MAX_NOISE; i++)
	{
		if(uniform)
			noise[i]= ((rand()/11)%strength) - strength/2;
		else
		{
			double x1, x2, w, y1;
			do {
				x1 = 2.0 * rand()/(float)RAND_MAX - 1.0;
				x2 = 2.0 * rand()/(float)RAND_MAX - 1.0;
				w = x1 * x1 + x2 * x2;
			} while ( w >= 1.0 );

			w = sqrt( (-2.0 * log( w ) ) / w );
			y1= x1 * w;

			y1*= strength / sqrt(3.0); 
			if     (y1<-128) y1=-128;
			else if(y1> 127) y1= 127;
			noise[i]= (int)y1;
		}
	}

	if(nonTempRandShift[0]==-1){
		for(i=0; i<MAX_RES; i++){
			nonTempRandShift[i]= rand()&(MAX_SHIFT-1);
		}
	}

	fp->noise= noise;
	return noise;
}

#ifdef HAVE_MMX
static inline void lineNoise_MMX(uint8_t *dst, uint8_t *src, int8_t *noise, int len, int shift){
	int mmx_len= len&(~7);
	shift&= ~7;
	noise+=shift;

	asm volatile(
		"movl %3, %%eax			\n\t"
		"pcmpeqb %%mm7, %%mm7		\n\t"
		"psllw $15, %%mm7		\n\t"
		"packsswb %%mm7, %%mm7		\n\t"
		".balign 16			\n\t"
		"1:				\n\t"
		"movq (%0, %%eax), %%mm0	\n\t"
		"movq (%1, %%eax), %%mm1	\n\t"
		"pxor %%mm7, %%mm0		\n\t"
		"paddsb %%mm1, %%mm0		\n\t"
		"pxor %%mm7, %%mm0		\n\t"
		"movq %%mm0, (%2, %%eax)	\n\t"
		"addl $8, %%eax			\n\t"
		" js 1b				\n\t"
		:: "r" (src+mmx_len), "r" (noise+mmx_len), "r" (dst+mmx_len), "g" (-mmx_len)
		: "%eax"
	);
	if(mmx_len!=len)
		lineNoise_C(dst+mmx_len, src+mmx_len, noise+mmx_len, len-mmx_len, 0);
}
#endif

//duplicate of previous except movntq
#ifdef HAVE_MMX2
static inline void lineNoise_MMX2(uint8_t *dst, uint8_t *src, int8_t *noise, int len, int shift){
	int mmx_len= len&(~7);
	shift&= ~7;
	noise+=shift;

	asm volatile(
		"movl %3, %%eax			\n\t"
		"pcmpeqb %%mm7, %%mm7		\n\t"
		"psllw $15, %%mm7		\n\t"
		"packsswb %%mm7, %%mm7		\n\t"
		".balign 16			\n\t"
		"1:				\n\t"
		"movq (%0, %%eax), %%mm0	\n\t"
		"movq (%1, %%eax), %%mm1	\n\t"
		"pxor %%mm7, %%mm0		\n\t"
		"paddsb %%mm1, %%mm0		\n\t"
		"pxor %%mm7, %%mm0		\n\t"
		"movntq %%mm0, (%2, %%eax)	\n\t"
		"addl $8, %%eax			\n\t"
		" js 1b				\n\t"
		:: "r" (src+mmx_len), "r" (noise+mmx_len), "r" (dst+mmx_len), "g" (-mmx_len)
		: "%eax"
	);
	if(mmx_len!=len)
		lineNoise_C(dst+mmx_len, src+mmx_len, noise+mmx_len, len-mmx_len, 0);
}
#endif

static inline void lineNoise_C(uint8_t *dst, uint8_t *src, int8_t *noise, int len, int shift){
	int i;
	noise+= shift;
	for(i=0; i<len; i++)
	{
		int v= src[i]+ noise[i];
		if(v>255) 	dst[i]=255; //FIXME optimize
		else if(v<0) 	dst[i]=0;
		else		dst[i]=v;
	}
}

static void noise(uint8_t *dst, uint8_t *src, int dstStride, int srcStride, int width, int height, FilterParam *fp){
	int8_t *noise= fp->noise;
	int y;
	int shift=0;

	if(!noise)
	{
		if(src==dst) return;

		if(dstStride==srcStride) memcpy(dst, src, srcStride*height);
		else
		{
			for(y=0; y<height; y++)
			{
				memcpy(dst, src, width);
				dst+= dstStride;
				src+= srcStride;	
			}
		}
		return;
	}

	for(y=0; y<height; y++)
	{
		if(fp->temporal)	shift=  rand()&(MAX_SHIFT  -1);
		else			shift= nonTempRandShift[y];

		lineNoise(dst, src, noise, width, shift);
		dst+= dstStride;
		src+= srcStride;
	}
}

static int config(struct vf_instance_s* vf,
        int width, int height, int d_width, int d_height,
	unsigned int flags, unsigned int outfmt){
    
	return vf_next_config(vf,width,height,d_width,d_height,flags,outfmt);
}

static void get_image(struct vf_instance_s* vf, mp_image_t *mpi){
    if(mpi->flags&MP_IMGFLAG_PRESERVE) return; // don't change
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

static void put_image(struct vf_instance_s* vf, mp_image_t *mpi){
	mp_image_t *dmpi;

	if(!(mpi->flags&MP_IMGFLAG_DIRECT)){
		// no DR, so get a new image! hope we'll get DR buffer:
		vf->priv->dmpi=vf_get_image(vf->next,vf->priv->outfmt,
		MP_IMGTYPE_TEMP, MP_IMGFLAG_ACCEPT_STRIDE,
		mpi->w,mpi->h);
//printf("nodr\n");
	}
//else printf("dr\n");
	dmpi= vf->priv->dmpi;

	noise(dmpi->planes[0], mpi->planes[0], dmpi->stride[0], mpi->stride[0], mpi->w, mpi->h, &vf->priv->lumaParam);
	noise(dmpi->planes[1], mpi->planes[1], dmpi->stride[1], mpi->stride[1], mpi->w/2, mpi->h/2, &vf->priv->chromaParam);
	noise(dmpi->planes[2], mpi->planes[2], dmpi->stride[2], mpi->stride[2], mpi->w/2, mpi->h/2, &vf->priv->chromaParam);

	dmpi->qscale=mpi->qscale;
	dmpi->qstride=mpi->qstride;

#ifdef HAVE_MMX
	if(gCpuCaps.hasMMX) asm volatile ("emms\n\t");
#endif
#ifdef HAVE_MMX2
	if(gCpuCaps.hasMMX2) asm volatile ("sfence\n\t");
#endif

	vf_next_put_image(vf,dmpi);
}

static void uninit(struct vf_instance_s* vf){
	if(!vf->priv) return;

	if(vf->priv->chromaParam.noise) free(vf->priv->chromaParam.noise);
	vf->priv->chromaParam.noise= NULL;

	if(vf->priv->lumaParam.noise) free(vf->priv->lumaParam.noise);
	vf->priv->lumaParam.noise= NULL;
	
	free(vf->priv);
	vf->priv=NULL;
}

//===========================================================================//

static int query_format(struct vf_instance_s* vf, unsigned int fmt){
	switch(fmt)
	{
	case IMGFMT_YV12:
	case IMGFMT_I420:
	case IMGFMT_IYUV:
		return vf_next_query_format(vf,vf->priv->outfmt);
	}
	return 0;
}

static void parse(FilterParam *fp, char* args){
	char *pos;
	char *max= strchr(args, ':');

	if(!max) max= args + strlen(args);

	fp->strength= atoi(args);
	pos= strchr(args, 'u');
	if(pos && pos<max) fp->uniform=1;
	pos= strchr(args, 't');
	if(pos && pos<max) fp->temporal=1;

	if(fp->strength) initNoise(fp);
}

static unsigned int fmt_list[]={
    IMGFMT_YV12,
    IMGFMT_I420,
    IMGFMT_IYUV,
    0
};

static int open(vf_instance_t *vf, char* args){
    vf->config=config;
    vf->put_image=put_image;
    vf->get_image=get_image;
    vf->query_format=query_format;
    vf->uninit=uninit;
    vf->priv=malloc(sizeof(struct vf_priv_s));
    memset(vf->priv, 0, sizeof(struct vf_priv_s));
    if(args)
    {
	char *arg2= strchr(args,':');
	if(arg2) parse(&vf->priv->chromaParam, arg2+1);
	parse(&vf->priv->lumaParam, args);
    }

    // check csp:
    vf->priv->outfmt=vf_match_csp(&vf->next,fmt_list,IMGFMT_YV12);
    if(!vf->priv->outfmt)
    {
	uninit(vf);
        return 0; // no csp match :(
    }

 
#ifdef HAVE_MMX
    if(gCpuCaps.hasMMX) lineNoise= lineNoise_MMX;
#endif
#ifdef HAVE_MMX2
    if(gCpuCaps.hasMMX2) lineNoise= lineNoise_MMX2;
#endif
    
    return 1;
}

vf_info_t vf_info_noise = {
    "noise genenerator",
    "noise",
    "Michael Niedermayer",
    "",
    open
};

//===========================================================================//

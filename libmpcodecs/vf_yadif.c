/*
    Copyright (C) 2006 Michael Niedermayer <michaelni@gmx.at>

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
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <math.h>

#include "config.h"

#include "mp_msg.h"

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include "img_format.h"
#include "mp_image.h"
#include "vf.h"
#include "libvo/fastmemcpy.h"

#define MIN(a,b) ((a) > (b) ? (b) : (a))
#define MAX(a,b) ((a) < (b) ? (b) : (a))
#define ABS(a) ((a) > 0 ? (a) : (-(a)))

#define MIN3(a,b,c) MIN(MIN(a,b),c)
#define MAX3(a,b,c) MAX(MAX(a,b),c)

//===========================================================================//

struct vf_priv_s {
    int mode;
    int parity;
    int buffered_i;
    int buffered_tff;
    double buffered_pts;
    mp_image_t *buffered_mpi;
    int stride[3];
    uint8_t *ref[4][3];
};

static void store_ref(struct vf_priv_s *p, uint8_t *src[3], int src_stride[3], int width, int height){
    int i;

    memcpy (p->ref[3], p->ref[0], sizeof(uint8_t *)*3);
    memmove(p->ref[0], p->ref[1], sizeof(uint8_t *)*3*3);

    for(i=0; i<3; i++){
        int is_chroma= !!i;

        memcpy_pic(p->ref[2][i], src[i], width>>is_chroma, height>>is_chroma, p->stride[i], src_stride[i]);
    }
}

static void filter_line(struct vf_priv_s *p, uint8_t *dst, uint8_t *prev, uint8_t *cur, uint8_t *next, int w, int refs, int parity){
    int x;
    uint8_t *prev2= parity ? prev : cur ;
    uint8_t *next2= parity ? cur  : next;
    for(x=0; x<w; x++){
        int c= cur[-refs];
        int d= (prev2[0] + next2[0])>>1;
        int e= cur[+refs];
        int temporal_diff0= ABS(prev2[0] - next2[0]);
        int temporal_diff1=( ABS(prev[-refs] - c) + ABS(prev[+refs] - e) )>>1;
        int temporal_diff2=( ABS(next[-refs] - c) + ABS(next[+refs] - e) )>>1;
        int diff= MAX3(temporal_diff0>>1, temporal_diff1, temporal_diff2);
        int spatial_pred= (c+e)>>1;
        int spatial_score= ABS(cur[-refs-1] - cur[+refs-1]) + ABS(c-e)
                         + ABS(cur[-refs+1] - cur[+refs+1]) - 1;

#define CHECK(j)\
    {   int score= ABS(cur[-refs-1+j] - cur[+refs-1-j])\
                 + ABS(cur[-refs  +j] - cur[+refs  -j])\
                 + ABS(cur[-refs+1+j] - cur[+refs+1-j]);\
        if(score < spatial_score){\
            spatial_score= score;\
            spatial_pred= (cur[-refs  +j] + cur[+refs  -j])>>1;\

        CHECK(-1) CHECK(-2) }} }}
        CHECK( 1) CHECK( 2) }} }}

        if(p->mode<2){
            int b= (prev2[-2*refs] + next2[-2*refs])>>1;
            int f= (prev2[+2*refs] + next2[+2*refs])>>1;
#if 0
            int a= cur[-3*refs];
            int g= cur[+3*refs];
            int max= MAX3(d-e, d-c, MIN3(MAX(b-c,f-e),MAX(b-c,b-a),MAX(f-g,f-e)) );
            int min= MIN3(d-e, d-c, MAX3(MIN(b-c,f-e),MIN(b-c,b-a),MIN(f-g,f-e)) );
#else
            int max= MAX3(d-e, d-c, MIN(b-c, f-e));
            int min= MIN3(d-e, d-c, MAX(b-c, f-e));
#endif

            diff= MAX3(diff, min, -max);
        }

        if(spatial_pred > d + diff)
           spatial_pred = d + diff;
        else if(spatial_pred < d - diff)
           spatial_pred = d - diff;

        dst[0] = spatial_pred;

        dst++;
        cur++;
        prev++;
        next++;
        prev2++;
        next2++;
    }
}

static void filter(struct vf_priv_s *p, uint8_t *dst[3], int dst_stride[3], int width, int height, int parity, int tff){
    int x, y, i;

    for(i=0; i<3; i++){
        int is_chroma= !!i;
        int w= width >>is_chroma;
        int h= height>>is_chroma;
        int refs= p->stride[i];

        for(y=0; y<h; y++){
            if((y ^ parity) & 1){
                uint8_t *prev= &p->ref[0][i][y*refs];
                uint8_t *cur = &p->ref[1][i][y*refs];
                uint8_t *next= &p->ref[2][i][y*refs];
                uint8_t *dst2= &dst[i][y*dst_stride[i]];
                filter_line(p, dst2, prev, cur, next, w, refs, parity ^ tff);
            }else{
                memcpy(&dst[i][y*dst_stride[i]], &p->ref[1][i][y*refs], w);
            }
        }
    }
}

static int config(struct vf_instance_s* vf,
        int width, int height, int d_width, int d_height,
	unsigned int flags, unsigned int outfmt){
        int i, j;

        for(i=0; i<3; i++){
            int is_chroma= !!i;
            int w= ((width   + 31) & (~31))>>is_chroma;
            int h= ((height+6+ 31) & (~31))>>is_chroma;

            vf->priv->stride[i]= w;
            for(j=0; j<3; j++)
                vf->priv->ref[j][i]= malloc(w*h*sizeof(uint8_t))+3*w;
        }

	return vf_next_config(vf,width,height,d_width,d_height,flags,outfmt);
}

static int continue_buffered_image(struct vf_instance_s *vf);
extern int correct_pts;

static int put_image(struct vf_instance_s* vf, mp_image_t *mpi, double pts){
    int tff;

    if(vf->priv->parity < 0) {
        if (mpi->fields & MP_IMGFIELD_ORDERED)
            tff = !!(mpi->fields & MP_IMGFIELD_TOP_FIRST);
        else
            tff = 1;
    }
    else tff = (vf->priv->parity&1)^1;

    store_ref(vf->priv, mpi->planes, mpi->stride, mpi->w, mpi->h);

    vf->priv->buffered_mpi = mpi;
    vf->priv->buffered_tff = tff;
    vf->priv->buffered_i = 0;
    vf->priv->buffered_pts = pts;

    return continue_buffered_image(vf);
}

static int continue_buffered_image(struct vf_instance_s *vf)
{
    mp_image_t *mpi = vf->priv->buffered_mpi;
    int tff = vf->priv->buffered_tff;
    double pts = vf->priv->buffered_pts;
    int i;
    int ret=0;
    mp_image_t *dmpi;

    pts += vf->priv->buffered_i * .02; // XXX not right

    for(i = vf->priv->buffered_i; i<=(vf->priv->mode&1); i++){
        dmpi=vf_get_image(vf->next,mpi->imgfmt,
            MP_IMGTYPE_TEMP,
            MP_IMGFLAG_ACCEPT_STRIDE|MP_IMGFLAG_PREFER_ALIGNED_STRIDE,
            mpi->width,mpi->height);
        vf_clone_mpi_attributes(dmpi, mpi);
        filter(vf->priv, dmpi->planes, dmpi->stride, mpi->w, mpi->h, i ^ tff ^ 1, tff);
        if (correct_pts && i < (vf->priv->mode & 1))
            vf_queue_frame(vf, continue_buffered_image);
        ret |= vf_next_put_image(vf, dmpi, pts /*FIXME*/);
        if (correct_pts)
            break;
        if(i<(vf->priv->mode&1))
            vf_next_control(vf, VFCTRL_FLIP_PAGE, NULL);
    }
    vf->priv->buffered_i = 1;
    return ret;
}

static void uninit(struct vf_instance_s* vf){
    int i;
    if(!vf->priv) return;

    for(i=0; i<3*3; i++){
        uint8_t **p= &vf->priv->ref[i%3][i/3];
        if(*p) free(*p - 3*vf->priv->stride[i/3]);
        *p= NULL;
    }
    free(vf->priv);
    vf->priv=NULL;
}

//===========================================================================//
static int query_format(struct vf_instance_s* vf, unsigned int fmt){
    switch(fmt){
	case IMGFMT_YV12:
	case IMGFMT_I420:
	case IMGFMT_IYUV:
	case IMGFMT_Y800:
	case IMGFMT_Y8:
	    return vf_next_query_format(vf,fmt);
    }
    return 0;
}

static int open(vf_instance_t *vf, char* args){

    vf->config=config;
    vf->put_image=put_image;
    vf->query_format=query_format;
    vf->uninit=uninit;
    vf->priv=malloc(sizeof(struct vf_priv_s));
    memset(vf->priv, 0, sizeof(struct vf_priv_s));

    vf->priv->mode=0;
    vf->priv->parity= -1;

    if (args) sscanf(args, "%d:%d", &vf->priv->mode, &vf->priv->parity);

    return 1;
}

vf_info_t vf_info_yadif = {
    "Yet Another DeInterlacing Filter",
    "yadif",
    "Michael Niedermayer",
    "",
    open,
    NULL
};

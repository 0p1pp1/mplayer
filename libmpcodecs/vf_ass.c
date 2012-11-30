/*
 * Copyright (C) 2006 Evgeniy Stepanov <eugeni.stepanov@gmail.com>
 * Copyright (C) 2012 Xidorn Quan <quanxunzhen@gmail.com>
 *
 * This file is part of MPlayer.
 *
 * MPlayer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * MPlayer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with MPlayer; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <assert.h>

#include "config.h"
#include "mp_msg.h"
#include "help_mp.h"
#include "mpcommon.h"
#include "img_format.h"
#include "mp_image.h"
#include "vd.h"
#include "vf.h"

#include "libvo/fastmemcpy.h"
#include "libavutil/intreadwrite.h"
#include "sub/sub.h"
#include "m_option.h"
#include "m_struct.h"

#include "sub/ass_mp.h"
#include "sub/eosd.h"

#define _r(c)  ((c)>>24)
#define _g(c)  (((c)>>16)&0xFF)
#define _b(c)  (((c)>>8)&0xFF)
#define _a(c)  ((c)&0xFF)
#define rgba2y(c)  ( (( 263*_r(c) + 516*_g(c) + 100*_b(c)) >> 10) + 16  )
#define rgba2u(c)  ( ((-152*_r(c) - 298*_g(c) + 450*_b(c)) >> 10) + 128 )
#define rgba2v(c)  ( (( 450*_r(c) - 376*_g(c) -  73*_b(c)) >> 10) + 128 )

/* map 0 - 0xFF -> 0 - 0x101 */
#define MAP_16BIT(v) RSHIFT(0x102 * (v), 8)
/* map 0 - 0xFF -> 0 - 0x10101 */
#define MAP_24BIT(v) RSHIFT(0x10203 * (v), 8)

static const struct vf_priv_s {
    int outh, outw;

    int is_planar;
    unsigned int outfmt;

    // 1 = auto-added filter: insert only if chain does not support EOSD already
    // 0 = insert always
    int auto_insert;

    // planar data to be directly rendered on frames
    uint8_t *planes[MP_MAX_PLANES];
    // alpha here is actually transparency, not opacity
    uint8_t *alphas[MP_MAX_PLANES];
    struct dirty_rows_extent {
        int xmin, xmax;
    } *dirty_rows;

    // called for every eosd image when subtitle is changed
    void (*draw_image)(vf_instance_t *, struct mp_eosd_image *);
    // called for every time subtitle is changed
    void (*prepare_buffer)(vf_instance_t *);
    // called for every frame
    void (*render_frame)(vf_instance_t *);
} vf_priv_dflt;

static void draw_image_yuv(vf_instance_t *vf, struct mp_eosd_image *img)
{
    uint32_t color = img->color;
    uint32_t opacity = 0xFF - _a(color);
    uint8_t y = rgba2y(color),
            u = rgba2u(color),
            v = rgba2v(color);
    int outw = vf->priv->outw;
    uint8_t *alpha = vf->priv->alphas[0],
            *dst_y = vf->priv->planes[0],
            *dst_u = vf->priv->planes[1],
            *dst_v = vf->priv->planes[2];
    struct dirty_rows_extent *dirty_rows = vf->priv->dirty_rows;
    int src_x = img->dst_x, src_w = img->w,
        src_y = img->dst_y, src_h = img->h,
        stride = img->stride;
    uint8_t *src = img->bitmap;
    int i, j;

    opacity = MAP_24BIT(opacity);
    for (i = 0; i < src_h; i++) {
        struct dirty_rows_extent *dirty_row = &dirty_rows[src_y + i];
        dirty_row->xmin = FFMIN(dirty_row->xmin, src_x);
        dirty_row->xmax = FFMAX(dirty_row->xmax, src_x + src_w);

        for (j = 0; j < src_w; j++) {
            uint32_t k = src[i * stride + j];
            if (k) {
                size_t p = (src_y + i) * outw + src_x + j;
                k *= opacity;
                alpha[p] = RSHIFT((0xFFFFFF - k) * alpha[p], 24);
                dst_y[p] = RSHIFT((0xFFFFFF - k) * dst_y[p] + k * y, 24);
                dst_u[p] = RSHIFT((0xFFFFFF - k) * dst_u[p] + k * u, 24);
                dst_v[p] = RSHIFT((0xFFFFFF - k) * dst_v[p] + k * v, 24);
            }
        }
    }
}

static void prepare_buffer_422(vf_instance_t *vf)
{
    uint8_t *dst_u = vf->priv->planes[1],
            *dst_v = vf->priv->planes[2];
    int outw = vf->priv->outw,
        outh = vf->priv->outh;
    struct dirty_rows_extent *dirty_rows = vf->priv->dirty_rows;
    int i, j;

    for (i = 0; i < outh; i++) {
        int xmin = dirty_rows[i].xmin & ~1,
            xmax = dirty_rows[i].xmax;
        for (j = xmin; j < xmax; j += 2) {
            size_t p = i * outw + j;
            dst_u[p] = (dst_u[p] + dst_u[p + 1]) / 2;
            dst_v[p] = (dst_v[p] + dst_v[p + 1]) / 2;
        }
    }
}

static void render_frame_yuv422(vf_instance_t *vf)
{
    uint8_t *alpha = vf->priv->alphas[0];
    uint8_t *src_y = vf->priv->planes[0],
            *src_u = vf->priv->planes[1],
            *src_v = vf->priv->planes[2];
    int outw = vf->priv->outw,
        outh = vf->priv->outh;
    struct dirty_rows_extent *dirty_rows = vf->priv->dirty_rows;
    uint8_t *dest = vf->dmpi->planes[0];
    int stride = vf->dmpi->stride[0];
    int is_uyvy = vf->priv->outfmt == IMGFMT_UYVY;
    int i, j;

    for (i = 0; i < outh; i++) {
        int xmin = dirty_rows[i].xmin & ~1,
            xmax = dirty_rows[i].xmax;
        for (j = xmin; j < xmax; j += 2) {
            size_t src = i * outw + j,
                   dst = i * stride + j * 2;
            uint_fast16_t a0 = alpha[src],
                          a1 = alpha[src + 1];
            uint8_t y0, y1, u, v;

            if (a0 == 0xFF && a1 == 0xFF)
                continue;

            y0 = dest[dst + is_uyvy + 0];
            y1 = dest[dst + is_uyvy + 2];
            u  = dest[dst - is_uyvy + 1];
            v  = dest[dst - is_uyvy + 3];

            a0 = MAP_16BIT(a0);
            a1 = MAP_16BIT(a1);
            y0 = ((a0 * y0) >> 8) + src_y[src];
            y1 = ((a1 * y1) >> 8) + src_y[src + 1];

            a0 = (a0 + a1) / 2;
            u = ((a0 * u) >> 8) + src_u[src];
            v = ((a0 * v) >> 8) + src_v[src];

            dest[dst + is_uyvy + 0] = y0;
            dest[dst + is_uyvy + 2] = y1;
            dest[dst - is_uyvy + 1] = u;
            dest[dst - is_uyvy + 3] = v;
        }
    }
}

static void prepare_buffer_420p(vf_instance_t *vf)
{
    int outw = vf->priv->outw,
        outh = vf->priv->outh;
    uint8_t *dst_u = vf->priv->planes[1],
            *dst_v = vf->priv->planes[2];
    uint8_t *src_a = vf->priv->alphas[0],
            *dst_a = vf->priv->alphas[1];
    struct dirty_rows_extent *dirty_rows = vf->priv->dirty_rows;
    int i, j;

    for (i = 0; i < outh; i += 2) {
        int xmin = FFMIN(dirty_rows[i].xmin, dirty_rows[i + 1].xmin) & ~1,
            xmax = FFMAX(dirty_rows[i].xmax, dirty_rows[i + 1].xmax);
        for (j = xmin; j < xmax; j += 2) {
            size_t p = i * outw / 4 + j / 2,
                   q1 = i * outw + j,
                   q2 = q1 + outw;
            dst_a[p] = (src_a[q1] + src_a[q1 + 1] +
                        src_a[q2] + src_a[q2 + 1] + 2) / 4;
            dst_u[p] = (dst_u[q1] + dst_u[q1 + 1] +
                        dst_u[q2] + dst_u[q2 + 1] + 2) / 4;
            dst_v[p] = (dst_v[q1] + dst_v[q1 + 1] +
                        dst_v[q2] + dst_v[q2 + 1] + 2) / 4;
        }
    }
}

static void render_frame_yuv420p(vf_instance_t *vf)
{
    uint8_t **planes = vf->priv->planes;
    uint8_t **dest = vf->dmpi->planes;
    struct dirty_rows_extent *dirty_rows = vf->priv->dirty_rows;
    uint8_t *alpha;
    uint8_t *src_y = planes[0],
            *src_u = planes[1],
            *src_v = planes[2];
    uint8_t *dst_y = dest[0],
            *dst_u = dest[1],
            *dst_v = dest[2];
    int stride;
    int outw = vf->priv->outw,
        outh = vf->priv->outh;
    int i, j;

    // y
    alpha  = vf->priv->alphas[0];
    stride = vf->dmpi->stride[0];
    for (i = 0; i < outh; i++) {
        int xmin = dirty_rows[i].xmin,
            xmax = dirty_rows[i].xmax;
        for (j = xmin; j < xmax; j++) {
            size_t s = i * outw + j,
                   d = i * stride + j;
            if (alpha[s] != 0xFF)
                dst_y[d] = ((MAP_16BIT(alpha[s]) * dst_y[d]) >> 8) + src_y[s];
        }
    }

    // u & v
    alpha  = vf->priv->alphas[1];
    stride = vf->dmpi->stride[1];
    for (i = 0; i < outh / 2; i++) {
        int xmin = FFMIN(dirty_rows[i * 2].xmin, dirty_rows[i * 2 + 1].xmin),
            xmax = FFMAX(dirty_rows[i * 2].xmax, dirty_rows[i * 2 + 1].xmax);
        for (j = xmin / 2; j < (xmax + 1) / 2; j++) {
            size_t s = i * outw / 2 + j,
                   d = i * stride + j;
            if (alpha[s] != 0xFF) {
                uint_fast16_t a = MAP_16BIT(alpha[s]);
                dst_u[d] = ((a * dst_u[d]) >> 8) + src_u[s];
                dst_v[d] = ((a * dst_v[d]) >> 8) + src_v[s];
            }
        }
    }
}

static void clean_buffer(vf_instance_t *vf)
{
    int outw = vf->priv->outw,
        outh = vf->priv->outh;
    struct dirty_rows_extent *dirty_rows = vf->priv->dirty_rows;
    uint8_t **planes = vf->priv->planes;
    uint8_t *alpha = vf->priv->alphas[0];
    int i, j;

    for (i = 0; i < MP_MAX_PLANES; i++) {
        uint8_t *plane = planes[i];
        if (!plane)
            break;
        for (j = 0; j < outh; j++) {
            int xmin = dirty_rows[j].xmin;
            int width = dirty_rows[j].xmax - xmin;
            if (width > 0)
                memset(plane + j * outw + xmin, 0, width);
        }
    }
    for (i = 0; i < outh; i++) {
        int xmin = dirty_rows[i].xmin;
        int width = dirty_rows[i].xmax - xmin;
        if (width > 0)
            memset(alpha + i * outw + xmin, -1, width);
    }
    for (i = 0; i < outh; i++) {
        dirty_rows[i].xmin = outw;
        dirty_rows[i].xmax = 0;
    }
}

static int config(struct vf_instance *vf,
                  int width, int height, int d_width, int d_height,
                  unsigned int flags, unsigned int outfmt)
{
    struct mp_eosd_settings res = {0};
    struct dirty_rows_extent *dirty_rows;
    int outw, outh;
    int planes, alphas;
    int i;

    switch (outfmt) {
    case IMGFMT_YV12:
    case IMGFMT_I420:
    case IMGFMT_IYUV:
        vf->priv->is_planar = 1;
        planes = 3;
        alphas = 2;
        vf->priv->draw_image = draw_image_yuv;
        vf->priv->render_frame = render_frame_yuv420p;
        vf->priv->prepare_buffer = prepare_buffer_420p;
        break;
    case IMGFMT_UYVY:
    case IMGFMT_YUY2:
        vf->priv->is_planar = 0;
        planes = 3;
        alphas = 1;
        vf->priv->draw_image = draw_image_yuv;
        vf->priv->render_frame = render_frame_yuv422;
        vf->priv->prepare_buffer = prepare_buffer_422;
        break;
    default:
        return 0;
    }

    vf->priv->outfmt = outfmt;
    vf->priv->outh = outh = height + ass_top_margin + ass_bottom_margin;
    vf->priv->outw = outw = width;

    if (!opt_screen_size_x && !opt_screen_size_y) {
        d_width  = d_width  * vf->priv->outw / width;
        d_height = d_height * vf->priv->outh / height;
    }

    for (i = 0; i < planes; i++)
        vf->priv->planes[i] = av_malloc(outw * outh);
    for (i = 0; i < alphas; i++)
        vf->priv->alphas[i] = av_malloc(outw * outh);
    dirty_rows = av_malloc(outh * sizeof(*dirty_rows));
    // mark all rows dirty here
    // so that they can be properly cleaned in clear_buffer()
    for (i = 0; i < outh; i++) {
        dirty_rows[i].xmin = 0;
        dirty_rows[i].xmax = outw;
    }
    vf->priv->dirty_rows = dirty_rows;
    clean_buffer(vf);

    res.w    = vf->priv->outw;
    res.h    = vf->priv->outh;
    res.srcw = width;
    res.srch = height;
    res.mt   = ass_top_margin;
    res.mb   = ass_bottom_margin;
    eosd_configure(&res);

    return vf_next_config(vf, vf->priv->outw, vf->priv->outh, d_width,
                          d_height, flags, outfmt);
}

static void get_image(struct vf_instance *vf, mp_image_t *mpi)
{
    if (mpi->type == MP_IMGTYPE_IPB)
        return;
    if (mpi->flags & MP_IMGFLAG_PRESERVE)
        return;
    if (mpi->imgfmt != vf->priv->outfmt)
        return;                 // colorspace differ

    // width never changes, always try full DR
    mpi->priv = vf->dmpi = vf_get_image(vf->next, mpi->imgfmt, mpi->type,
                                        mpi->flags | MP_IMGFLAG_READABLE,
                                        FFMAX(mpi->width,  vf->priv->outw),
                                        FFMAX(mpi->height, vf->priv->outh));

    if ( (vf->dmpi->flags & MP_IMGFLAG_DRAW_CALLBACK) &&
        !(vf->dmpi->flags & MP_IMGFLAG_DIRECT)) {
        mp_msg(MSGT_ASS, MSGL_INFO, MSGTR_MPCODECS_FullDRNotPossible);
        return;
    }
    // set up mpi as a cropped-down image of dmpi:
    if (mpi->flags & MP_IMGFLAG_PLANAR) {
        mpi->planes[0] = vf->dmpi->planes[0] +  ass_top_margin                         * vf->dmpi->stride[0];
        mpi->planes[1] = vf->dmpi->planes[1] + (ass_top_margin >> mpi->chroma_y_shift) * vf->dmpi->stride[1];
        mpi->planes[2] = vf->dmpi->planes[2] + (ass_top_margin >> mpi->chroma_y_shift) * vf->dmpi->stride[2];
        mpi->stride[1] = vf->dmpi->stride[1];
        mpi->stride[2] = vf->dmpi->stride[2];
    } else {
        mpi->planes[0] = vf->dmpi->planes[0] +  ass_top_margin                         * vf->dmpi->stride[0];
    }
    mpi->stride[0] = vf->dmpi->stride[0];
    mpi->width     = vf->dmpi->width;
    mpi->flags    |=  MP_IMGFLAG_DIRECT;
    mpi->flags    &= ~MP_IMGFLAG_DRAW_CALLBACK;
//      vf->dmpi->flags &= ~MP_IMGFLAG_DRAW_CALLBACK;
}

static void blank(mp_image_t *mpi, int y1, int y2)
{
    int color[3] = { 16, 128, 128 };    // black (YUV)
    int y;
    unsigned char *dst;
    int chroma_rows = (y2 - y1) >> mpi->chroma_y_shift;

    if (mpi->flags & MP_IMGFLAG_PLANAR) {
        dst = mpi->planes[0] + y1 * mpi->stride[0];
        for (y = 0; y < y2 - y1; ++y) {
            memset(dst, color[0], mpi->w);
            dst += mpi->stride[0];
        }
        dst = mpi->planes[1] + (y1 >> mpi->chroma_y_shift) * mpi->stride[1];
        for (y = 0; y < chroma_rows; ++y) {
            memset(dst, color[1], mpi->chroma_width);
            dst += mpi->stride[1];
        }
        dst = mpi->planes[2] + (y1 >> mpi->chroma_y_shift) * mpi->stride[2];
        for (y = 0; y < chroma_rows; ++y) {
            memset(dst, color[2], mpi->chroma_width);
            dst += mpi->stride[2];
        }
    } else {
        unsigned char packed_color[4];
        int x;
        if (mpi->imgfmt == IMGFMT_UYVY) {
            packed_color[0] = color[1];
            packed_color[1] = color[0];
            packed_color[2] = color[2];
            packed_color[3] = color[0];
        } else {
            packed_color[0] = color[0];
            packed_color[1] = color[1];
            packed_color[2] = color[0];
            packed_color[3] = color[2];
        }
        dst = mpi->planes[0] + y1 * mpi->stride[0];
        for (y = y1; y < y2; ++y) {
            for (x = 0; x < mpi->w / 2; ++x)
                AV_COPY32(dst + 4 * x, packed_color);
            dst += mpi->stride[0];
        }
    }
}

static int prepare_image(struct vf_instance *vf, mp_image_t *mpi)
{
    if (mpi->flags & MP_IMGFLAG_DIRECT ||
	mpi->flags & MP_IMGFLAG_DRAW_CALLBACK) {
        vf->dmpi = mpi->priv;
        if (!vf->dmpi) {
            mp_msg(MSGT_ASS, MSGL_WARN, MSGTR_MPCODECS_FunWhydowegetNULL);
            return 0;
        }
        mpi->priv = NULL;
        // we've used DR, so we're ready...
        if (ass_top_margin)
            blank(vf->dmpi, 0, ass_top_margin);
        if (ass_bottom_margin)
            blank(vf->dmpi, vf->priv->outh - ass_bottom_margin, vf->priv->outh);
        if (!(mpi->flags & MP_IMGFLAG_PLANAR))
            vf->dmpi->planes[1] = mpi->planes[1];       // passthrough rgb8 palette
        return 0;
    }
    // hope we'll get DR buffer:
    vf->dmpi = vf_get_image(vf->next, vf->priv->outfmt, MP_IMGTYPE_TEMP,
                            MP_IMGFLAG_ACCEPT_STRIDE | MP_IMGFLAG_READABLE,
                            vf->priv->outw, vf->priv->outh);

    // copy mpi->dmpi...
    if (mpi->flags & MP_IMGFLAG_PLANAR) {
        memcpy_pic(vf->dmpi->planes[0] +  ass_top_margin * vf->dmpi->stride[0],
                   mpi->planes[0],
                   mpi->w,
                   mpi->h,
                   vf->dmpi->stride[0],
                   mpi->stride[0]);
        memcpy_pic(vf->dmpi->planes[1] + (ass_top_margin >> mpi->chroma_y_shift) * vf->dmpi->stride[1],
                   mpi->planes[1],
                   mpi->w >> mpi->chroma_x_shift,
                   mpi->h >> mpi->chroma_y_shift,
                   vf->dmpi->stride[1],
                   mpi->stride[1]);
        memcpy_pic(vf->dmpi->planes[2] + (ass_top_margin >> mpi->chroma_y_shift) * vf->dmpi->stride[2],
                   mpi->planes[2],
                   mpi->w >> mpi->chroma_x_shift,
                   mpi->h >> mpi->chroma_y_shift,
                   vf->dmpi->stride[2],
                   mpi->stride[2]);
    } else {
        memcpy_pic(vf->dmpi->planes[0] + ass_top_margin * vf->dmpi->stride[0],
                   mpi->planes[0],
                   mpi->w * (vf->dmpi->bpp / 8),
                   mpi->h,
                   vf->dmpi->stride[0],
                   mpi->stride[0]);
        vf->dmpi->planes[1] = mpi->planes[1];   // passthrough rgb8 palette
    }
    if (ass_top_margin)
        blank(vf->dmpi, 0, ass_top_margin);
    if (ass_bottom_margin)
        blank(vf->dmpi, vf->priv->outh - ass_bottom_margin, vf->priv->outh);
    return 0;
}

static void prepare_eosd(vf_instance_t *vf, struct mp_eosd_image_list *imgs)
{
    struct mp_eosd_image *img = eosd_image_first(imgs);
    void (*draw_image)(vf_instance_t *, struct mp_eosd_image *);

    clean_buffer(vf);
    draw_image = vf->priv->draw_image;
    for (; img; img = eosd_image_next(imgs))
        draw_image(vf, img);
    vf->priv->prepare_buffer(vf);
}

static int put_image(struct vf_instance *vf, mp_image_t *mpi, double pts)
{
    struct mp_eosd_image_list images;
    eosd_render_frame(pts, &images);
    prepare_image(vf, mpi);
    if (images.changed)
        prepare_eosd(vf, &images);
    vf->priv->render_frame(vf);
    return vf_next_put_image(vf, vf->dmpi, pts);
}

static int query_format(struct vf_instance *vf, unsigned int fmt)
{
    switch (fmt) {
    case IMGFMT_YV12:
    case IMGFMT_I420:
    case IMGFMT_IYUV:
    case IMGFMT_UYVY:
    case IMGFMT_YUY2:
        return vf_next_query_format(vf, fmt) | VFCAP_EOSD;
    }
    return 0;
}

static int control(vf_instance_t *vf, int request, void *data)
{
    switch (request) {
    case VFCTRL_INIT_EOSD:
        return CONTROL_TRUE;
    case VFCTRL_DRAW_EOSD:
        return CONTROL_TRUE;
    }
    return vf_next_control(vf, request, data);
}

static void uninit(struct vf_instance *vf)
{
    int i;
    for (i = 0; i < MP_MAX_PLANES; i++)
        av_free(vf->priv->planes[i]);
    for (i = 0; i < MP_MAX_PLANES; i++)
        av_free(vf->priv->alphas[i]);
    av_free(vf->priv->dirty_rows);
}

static const unsigned int fmt_list[] = {
    IMGFMT_YV12,
    IMGFMT_I420,
    IMGFMT_IYUV,
    IMGFMT_UYVY,
    IMGFMT_YUY2,
    0
};

static int vf_open(vf_instance_t *vf, char *args)
{
    int flags;
    unsigned outfmt = vf_match_csp(&vf->next, fmt_list, IMGFMT_YV12);
    if (outfmt)
        flags = vf_next_query_format(vf, outfmt);
    if (!outfmt || (vf->priv->auto_insert && flags & VFCAP_EOSD)) {
        uninit(vf);
        return 0;
    }

    if (vf->priv->auto_insert)
        mp_msg(MSGT_ASS, MSGL_INFO, "[ass] auto-open\n");

    vf->config       = config;
    vf->query_format = query_format;
    vf->uninit       = uninit;
    vf->control      = control;
    vf->get_image    = get_image;
    vf->put_image    = put_image;
    vf->default_caps = VFCAP_EOSD;
    return 1;
}

#define ST_OFF(f) M_ST_OFF(struct vf_priv_s,f)
static const m_option_t vf_opts_fields[] = {
    {"auto", ST_OFF(auto_insert), CONF_TYPE_FLAG, 0, 0, 1, NULL},
    {NULL, NULL, 0, 0, 0, 0, NULL}
};

static const m_struct_t vf_opts = {
    "ass",
    sizeof(struct vf_priv_s),
    &vf_priv_dflt,
    vf_opts_fields
};

const vf_info_t vf_info_ass = {
    "Render ASS/SSA subtitles",
    "ass",
    "Evgeniy Stepanov, Xidorn Quan",
    "",
    vf_open,
    &vf_opts
};

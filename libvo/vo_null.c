/* 
 *  video_out_null.c
 *
 *	Copyright (C) Aaron Holtzman - June 2000
 *
 *  This file is part of mpeg2dec, a free MPEG-2 video stream decoder.
 *	
 *  mpeg2dec is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *   
 *  mpeg2dec is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *   
 *  You should have received a copy of the GNU General Public License
 *  along with GNU Make; see the file COPYING.  If not, write to
 *  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. 
 *
 */

#include "config.h"
#include "video_out.h"
#include "video_out_internal.h"

LIBVO_EXTERN(null)


static vo_info_t vo_info = 
{
	"Null video output",
	"null",
	"Aaron Holtzman <aholtzma@ess.engr.uvic.ca>",
	""
};

static uint32_t image_width, image_height;

//static uint32_t
static uint32_t draw_slice(uint8_t *image[], int stride[], int w,int h,int x,int y)
//draw_slice(uint8_t *src[], uint32_t slice_num)
{
	return 0;
}

static void
flip_page(void)
{
}

static uint32_t
draw_frame(uint8_t *src[])
{
	return 0;
}

static uint32_t
query_format(uint32_t format)
{
    return 1;
}

static uint32_t
init(uint32_t width, uint32_t height, uint32_t d_width, uint32_t d_height, uint32_t fullscreen, char *title, uint32_t format)
{
	image_width = width;
	image_height = height;
	return 0;
}

static const vo_info_t*
get_info(void)
{
	return &vo_info;
}

static void
uninit(void)
{
}


static void check_events(void)
{
}





//#define memcpy(a,b,c)

/* 
 *    video_out_mga.c
 *
 *	Copyright (C) Aaron Holtzman - Aug 1999
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"
#include "video_out.h"
#include "video_out_internal.h"

LIBVO_EXTERN(mga)

#include <sys/ioctl.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>

#include "drivers/mga_vid.h"

static vo_info_t vo_info = 
{
	"Matrox G200/G400 overlay (/dev/mga_vid)",
	"mga",
	"Aaron Holtzman <aholtzma@ess.engr.uvic.ca>",
	""
};

static mga_vid_config_t mga_vid_config;
static uint8_t *vid_data, *frames[4];
static int f;

#include "mga_common.c"

static uint32_t
init(uint32_t width, uint32_t height, uint32_t d_width, uint32_t d_height, uint32_t fullscreen, char *title, uint32_t format)
{
	char *frame_mem;
	uint32_t frame_size;

	f = open("/dev/mga_vid",O_RDWR);

	if(f == -1)
	{
		fprintf(stderr,"Couldn't open /dev/mga_vid\n"); 
		return(-1);
	}

        switch(format){
        case IMGFMT_YV12:
            mga_vid_config.format=MGA_VID_FORMAT_YV12; break;
        case IMGFMT_YUY2:
            mga_vid_config.format=MGA_VID_FORMAT_YUY2; break;
        default: 
            fprintf(stderr,"mga: invalid output format %0X\n",format);
            return (-1);
        }

	mga_vid_config.src_width = width;
	mga_vid_config.src_height= height;
//	mga_vid_config.dest_width = width;
//	mga_vid_config.dest_height= height;
	mga_vid_config.dest_width = d_width;
//        (width<704)?704:width;   // HACK
	mga_vid_config.dest_height= d_height;
//        (height<576)?576:height; // HACK
	mga_vid_config.x_org= 0; // (720-mga_vid_config.dest_width)/2;
	mga_vid_config.y_org= 0; // (576-mga_vid_config.dest_height)/2;

	if (ioctl(f,MGA_VID_CONFIG,&mga_vid_config))
	{
		perror("Error in mga_vid_config ioctl");
	}
	ioctl(f,MGA_VID_ON,0);

	frame_size = ((width + 31) & ~31) * height + (((width + 31) & ~31) * height) / 2;
	frame_mem = (char*)mmap(0,frame_size*4,PROT_WRITE,MAP_SHARED,f,0);
	frames[0] = frame_mem;
	frames[1] = frame_mem + 1*frame_size;
	frames[2] = frame_mem + 2*frame_size;
	frames[3] = frame_mem + 3*frame_size;
	mga_next_frame = 0;
	vid_data = frames[mga_next_frame];

	//clear the buffer
	memset(frame_mem,0x80,frame_size*4);

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
 ioctl( f,MGA_VID_OFF,0 );
printf("vo: uninit!\n");
}


static void flip_page(void)
{
    vo_mga_flip_page();
}


static void check_events(void)
{
}




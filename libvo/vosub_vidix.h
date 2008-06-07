/*
 *  vosub_vidix.h
 *
 *	Copyright (C) Nick Kurshev <nickols_k@mail.ru> - 2002
 *
 *  You can redistribute this file under terms and conditions
 *  of GNU General Public licence v2 or later.
 *
 * This file contains vosub_vidix interface to any mplayer's VO driver
 */

#ifndef MPLAYER_VOSUB_VIDIX_H
#define MPLAYER_VOSUB_VIDIX_H

#include <stdint.h>
#include "video_out.h"

		    /* drvname can be NULL */
int	 vidix_preinit(const char *drvname,vo_functions_t *server);
int      vidix_init(unsigned src_width,unsigned src_height,
		    unsigned dest_x,unsigned dest_y,unsigned dst_width,
		    unsigned dst_height,unsigned format,unsigned dest_bpp,
		    unsigned vid_w,unsigned vid_h);
int	 vidix_start(void);
int	 vidix_stop(void);
void     vidix_term( void );
uint32_t vidix_control(uint32_t request, void *data, ...);
uint32_t vidix_query_fourcc(uint32_t fourcc);

#include "vidix/vidix.h"
/* graphic keys */
int vidix_grkey_support(void);
int vidix_grkey_get(vidix_grkey_t *gr_key);
int vidix_grkey_set(const vidix_grkey_t *gr_key);

#endif /* MPLAYER_VOSUB_VIDIX_H */

/*
 * bitstream.c
 * Copyright (C) 2000-2001 Michel Lespinasse <walken@zoy.org>
 * Copyright (C) 1999-2000 Aaron Holtzman <aholtzma@ess.engr.uvic.ca>
 *
 * This file is part of a52dec, a free ATSC A-52 stream decoder.
 * See http://liba52.sourceforge.net/ for updates.
 *
 * Modified for use with MPlayer, changes contained in liba52_changes.diff.
 * detailed CVS changelog at http://www.mplayerhq.hu/cgi-bin/cvsweb.cgi/main/
 * $Id$
 *
 * a52dec is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * a52dec is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "config.h"

#include <inttypes.h>

#include "a52.h"
#include "a52_internal.h"
#include "bitstream.h"

#define BUFFER_SIZE 4096

#ifdef ALT_BITSTREAM_READER
int indx=0;
uint32_t * buffer_start;
#else
static uint32_t * buffer_start;
#endif

uint32_t bits_left;
uint32_t current_word;

void bitstream_set_ptr (uint8_t * buf)
{
    int align;

    align = (int)buf & 3;
    buffer_start = (uint32_t *) (buf - align);
    bits_left = 0;
#ifdef ALT_BITSTREAM_READER
    indx=0;
#endif
    bitstream_get (align * 8);
}

static inline void
bitstream_fill_current()
{
    uint32_t tmp;

    tmp = *(buffer_start++);
    current_word = swab32 (tmp);
}

/*
 * The fast paths for _get is in the
 * bitstream.h header file so it can be inlined.
 *
 * The "bottom half" of this routine is suffixed _bh
 *
 * -ah
 */

uint32_t
bitstream_get_bh(uint32_t num_bits)
{
    uint32_t result;

    num_bits -= bits_left;
    result = (current_word << (32 - bits_left)) >> (32 - bits_left);

    bitstream_fill_current();

    if(num_bits != 0)
	result = (result << num_bits) | (current_word >> (32 - num_bits));
	
    bits_left = 32 - num_bits;

    return result;
}

int32_t
bitstream_get_bh_2(uint32_t num_bits)
{
    int32_t result;

    num_bits -= bits_left;
    result = (((int32_t)current_word) << (32 - bits_left)) >> (32 - bits_left);

    bitstream_fill_current();

    if(num_bits != 0)
	result = (result << num_bits) | (current_word >> (32 - num_bits));
	
    bits_left = 32 - num_bits;

    return result;
}

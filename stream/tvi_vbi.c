/*
 * Teletext support
 * Copyright (C) 2007 Vladimir Voroshilov <voroshil@gmail.com>
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
 * You should have received a copy of the GNU General Public License
 * along with MPlayer; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *
 *
 * Based on Attila Otvos' teletext patch, Michael Niedermayer's
 * proof-of-concept teletext capture utility and some parts
 * (decode_raw_line_runin,pll_add,pll_reset) of MythTV project.
 * Code for calculating [soc:eoc] is based on aletv of Edgar Toernig.
 *
 *
 * Some implementation details:
 * How to port teletext to another tvi_* driver (see tvi_v4l2.c for example):
 *
 * 1. Implement TVI_CONTROL_VBI_INIT (initialize driver-related vbi subsystem,
 *    start grabbing thread)
 *    input data: vbi device name.
 *    (driver should also call TV_VBI_CONTROL_START for common vbi subsystem initialization
 *    with pointer to initialized tt_stream_properties structure.
 *    After ioctl call variable will contain pointer to initialized priv_vbi_t structure.
 *
 * 2. After receiving next chunk of raw vbi data call TV_VBI_CONTROL_DECODE_PAGE
 *    ioctl with pointer to data buffer
 * 3. pass all other VBI related ioctl cmds to teletext_control routine
 *
 * Page displaying process consist of following stages:
 *
 * ---grabbing stage---
 * 0. stream/tvi_*.c: vbi_grabber(...)
 *      getting vbi data from video device
 * ---decoding stage---
 * 1. stream/tvi_vbi.c: decode_raw_line_runin(...) or decode_raw_line_sine(...)
 *      decode raw vbi data into sliced 45(?) bytes long packets
 * 2. stream/tvi_vbi.c: decode_pkt0(...), decode_pkt_page(...)
 *      packets processing (header analyzing, storing complete page in cache,
 *      only raw member of tt_char is filled at this stage)
 * 3. stream/tvi_vbi.c: decode_page(...)
 *      page decoding. filling unicode,gfx,ctl,etc members of tt_char structure
 *      with appropriate values according to teletext control chars, converting
 *      text to utf8.
 * ---rendering stage---
 * 4. stream/tvi_vbi.c: prepare_visible_page(...)
 *      processing page. adding number of just received by background process
 *      teletext page, adding current time,etc.
 * 5. libvo/sub.c: vo_update_text_teletext(...)
 *      rendering displayable osd with text and graphics
 *
 * TODO:
 *  v4lv1,bktr support
 *  spu rendering
 *  is better quality on poor signal possible ?
 *  link support
 *  font autoscale
 *  greyscale osd
 *  slave command for dumping pages
 *  fix bcd<->dec as suggested my Michael
 *
 *  BUGS:
 *  wrong colors in debug dump
 *  blinking when visible page was just updated
 */

#include "config.h"

#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>

#include <pthread.h>

#include "tv.h"
#include "mp_msg.h"
#include "help_mp.h"
#include "libmpcodecs/img_format.h"
#include "libavutil/common.h"
#include "input/input.h"

//#define DEBUG_DUMP 1

/// page magazine entry structure
typedef struct mag_s{
    tt_page* pt;
    int      order;
    int      lang;
} mag_t;

typedef struct {
    int             on;            ///< teletext on/off
    int             pagenum;       ///< seek page number
    int             subpagenum;    ///< seek subpage
    int             curr_pagenum;  ///< current page number
    int             pagenumdec;    ///< set page num with dec

    teletext_format tformat;       ///< see teletext_format enum
    teletext_zoom   zoom;          ///< see teletext_zoom enum
    mag_t*          mag;           ///< pages magazine (has 8 entities)
    /// Currently displayed page (with additional info, e.g current time)
    tt_char         display_page[VBI_ROWS*VBI_COLUMNS];
    /// number of raw bytes between two subsequent encoded bits
    int bpb;
    /// clock run-in sequence will be searched in buffer in [soc:eoc] bytes range
    int soc;
    int eoc;
    /// minimum number of raw vbi bytes wich can be decoded into 8 data bits
    int bp8bl;
    /// maximum number of raw vbi bytes wich can be decoded into 8 data bits
    int bp8bh;

    int pll_adj;
    int pll_dir;
    int pll_cnt;
    int pll_err;
    int pll_lerr;
    int pll_fixed;
    /// vbi stream properties (buffer size,bytes per line, etc)
    tt_stream_props* ptsp;
    pthread_mutex_t buffer_mutex;

    tt_page** ptt_cache;
    unsigned char* ptt_cache_first_subpage;
} priv_vbi_t;

static unsigned char fixParity[256];

static tt_char tt_space={0x20,7,0,0,0,0,0x20};
static tt_char tt_error={'?',1,0,0,0,0,'?'}; // Red '?' on black background
static double si[12];
static double co[12];

#define VBI_FORMAT(priv) (*(priv->ptsp))

#define FIXP_SH 16
#define ONE_FIXP (1<<FIXP_SH)
#define FIXP2INT(a) ((a)>>FIXP_SH)
#define ANY2FIXP(a) ((int)((a)*ONE_FIXP))

static const unsigned char corrHamm48[256]={
  0x01, 0xff, 0x01, 0x01, 0xff, 0x00, 0x01, 0xff,
  0xff, 0x02, 0x01, 0xff, 0x0a, 0xff, 0xff, 0x07,
  0xff, 0x00, 0x01, 0xff, 0x00, 0x00, 0xff, 0x00,
  0x06, 0xff, 0xff, 0x0b, 0xff, 0x00, 0x03, 0xff,
  0xff, 0x0c, 0x01, 0xff, 0x04, 0xff, 0xff, 0x07,
  0x06, 0xff, 0xff, 0x07, 0xff, 0x07, 0x07, 0x07,
  0x06, 0xff, 0xff, 0x05, 0xff, 0x00, 0x0d, 0xff,
  0x06, 0x06, 0x06, 0xff, 0x06, 0xff, 0xff, 0x07,
  0xff, 0x02, 0x01, 0xff, 0x04, 0xff, 0xff, 0x09,
  0x02, 0x02, 0xff, 0x02, 0xff, 0x02, 0x03, 0xff,
  0x08, 0xff, 0xff, 0x05, 0xff, 0x00, 0x03, 0xff,
  0xff, 0x02, 0x03, 0xff, 0x03, 0xff, 0x03, 0x03,
  0x04, 0xff, 0xff, 0x05, 0x04, 0x04, 0x04, 0xff,
  0xff, 0x02, 0x0f, 0xff, 0x04, 0xff, 0xff, 0x07,
  0xff, 0x05, 0x05, 0x05, 0x04, 0xff, 0xff, 0x05,
  0x06, 0xff, 0xff, 0x05, 0xff, 0x0e, 0x03, 0xff,
  0xff, 0x0c, 0x01, 0xff, 0x0a, 0xff, 0xff, 0x09,
  0x0a, 0xff, 0xff, 0x0b, 0x0a, 0x0a, 0x0a, 0xff,
  0x08, 0xff, 0xff, 0x0b, 0xff, 0x00, 0x0d, 0xff,
  0xff, 0x0b, 0x0b, 0x0b, 0x0a, 0xff, 0xff, 0x0b,
  0x0c, 0x0c, 0xff, 0x0c, 0xff, 0x0c, 0x0d, 0xff,
  0xff, 0x0c, 0x0f, 0xff, 0x0a, 0xff, 0xff, 0x07,
  0xff, 0x0c, 0x0d, 0xff, 0x0d, 0xff, 0x0d, 0x0d,
  0x06, 0xff, 0xff, 0x0b, 0xff, 0x0e, 0x0d, 0xff,
  0x08, 0xff, 0xff, 0x09, 0xff, 0x09, 0x09, 0x09,
  0xff, 0x02, 0x0f, 0xff, 0x0a, 0xff, 0xff, 0x09,
  0x08, 0x08, 0x08, 0xff, 0x08, 0xff, 0xff, 0x09,
  0x08, 0xff, 0xff, 0x0b, 0xff, 0x0e, 0x03, 0xff,
  0xff, 0x0c, 0x0f, 0xff, 0x04, 0xff, 0xff, 0x09,
  0x0f, 0xff, 0x0f, 0x0f, 0xff, 0x0e, 0x0f, 0xff,
  0x08, 0xff, 0xff, 0x05, 0xff, 0x0e, 0x0d, 0xff,
  0xff, 0x0e, 0x0f, 0xff, 0x0e, 0x0e, 0xff, 0x0e };


enum {
  LAT_UNI=0,
  RUS_UNI,
  LANGS
};

// conversion table for chars 0x20-0x7F (UTF8)
// TODO: add another languages
static unsigned int lang_chars[LANGS][0x60]={
 {
  //Latin
  0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,
  0x28,0x29,0x2a,0x2b,0x2c,0x2d,0x2e,0x2f,
  0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,
  0x38,0x39,0x3a,0x3b,0x3c,0x3d,0x3e,0x3f,
  0x40,0x41,0x42,0x43,0x44,0x45,0x46,0x47,
  0x48,0x49,0x4a,0x4b,0x4c,0x4d,0x4e,0x4f,
  0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,
  0x58,0x59,0x5a,0x5b,0x5c,0x5d,0x5e,0x5f,
  0x60,0x61,0x62,0x63,0x64,0x65,0x66,0x67,
  0x68,0x69,0x6a,0x6b,0x6c,0x6d,0x6e,0x6f,
  0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,
  0x78,0x79,0x7a,0x7b,0x7c,0x7d,0x7e,0x7f
 },
 {
  //Russian
  0x20,0x21,0x22,0x23,0x24,0x25,0x044b,0x27,
  0x28,0x29,0x2a,0x2b,0x2c,0x2d,0x2e,0x2f,
  0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,
  0x38,0x39,0x3a,0x3b,0x3c,0x3d,0x3e,0x3f,
  0x042e,0x0410,0x0411,0x0426,0x0414,0x0415,0x0424,0x0413,
  0x0425,0x0418,0x0419,0x041a,0x041b,0x041c,0x041d,0x041e,
  0x041f,0x042f,0x0420,0x0421,0x0422,0x0423,0x0416,0x0412,
  0x042c,0x042a,0x0417,0x0428,0x042d,0x0429,0x0427,0x042b,
  0x044e,0x0430,0x0431,0x0446,0x0434,0x0435,0x0444,0x0433,
  0x0445,0x0438,0x0439,0x043a,0x043b,0x043c,0x043d,0x043e,
  0x043f,0x044f,0x0440,0x0441,0x0442,0x0443,0x0436,0x0432,
  0x044c,0x044a,0x0437,0x0448,0x044d,0x0449,0x0447,0x044b
 }
};

/**
 * Latin National Option Sub-Sets
 * see Table 36 of ETS specification for details.
 *
 * 00:  £ $ @ « ½ » ¬ # ­ ¼ ¦ ¾ ÷  English
 * 01:  é ï à ë ê ù î # è â ô û ç  French
 * 02:  # ¤ É Ä Ö Å Ü _ é ä ö å ü  Swedish/Finnish/Hungarian
 * 03:  # u c t z ý í r é á e ú s  Czech/Slovak
 * 04:  # $ § Ä Ö Ü ^ _ ° ä ö ü ß  German
 * 05:  ç $ ¡ á é í ó ú ¿ ü ñ è à  Portuguese/Spanish
 * 06:  £ $ é ° ç » ¬ # ù à ò è ì  Italian
 *
 */
static unsigned int latin_subchars[8][13]={
  // English
  {0xa3,0x24,0x40,0xab,0xbd,0xbb,0xac,0x23,0xad,0xbc,0xa6,0xbe,0xf7},
  // French
  {0xe9,0xef,0xe0,0xeb,0xea,0xf9,0xee,0x23,0xe8,0xe2,0xf4,0xfb,0xe7},
  // Swedish/Finnish/Hungarian
  {0x23,0xa4,0xc9,0xc4,0xd6,0xc5,0xdc,0x5f,0xe9,0xe4,0xf6,0xe5,0xfc},
  // Czech/Slovak
  {0x23,0x75,0x63,0x74,0x7a,0xfd,0xed,0x72,0xe9,0xe1,0x65,0xfa,0x73},
  // German
  {0x23,0x24,0xa7,0xc4,0xd6,0xdc,0x5e,0x5f,0xb0,0xe4,0xf6,0xfc,0xdf},
  // Portuguese/Spanish
  {0xe7,0x24,0xa1,0xe1,0xe9,0xed,0xf3,0xfa,0xbf,0xfc,0xf1,0xe8,0xe0},
  // Italian
  {0xa3,0x24,0xe9,0xb0,0xe7,0xbb,0xac,0x23,0xf9,0xe0,0xf2,0xe8,0xec},
  // Reserved
  {0x23,0x24,0x40,0x5b,0x5c,0x5d,0x5e,0x5f,0x60,0x7b,0x7c,0x7d,0x7e}
};

static int lang2id (int lang){
  switch(lang){
  case 0:
      return LAT_UNI;
  case 3: /* Stub. My teletext provider (1TV, Russia) sets this this language 
             code for russian teletext pages
             TODO: make this configurable
           */
      return RUS_UNI;
  default:
      return LAT_UNI;
  }
}

/**
 * \brief convert chars from curent teletext codepage into MPlayer charset
 * \param p raw teletext char to decode
 * \param lang teletext internal language code (see lang2id)
 * \return UTF8 char
 *
 * \remarks
 * routine will analyze raw member of given tt_char structure and
 * fill unicode member of the same struct with appropriate utf8 code.
 */
static unsigned int conv2uni(unsigned int p,int lang)
{
    int charset=lang2id(lang);
    if(p<0x80 && p>=0x20){
        if(charset==LAT_UNI){
            if (p>=0x23 && p<=0x24){
                return latin_subchars[lang][p-0x23];
            }else if (p==0x40){
                return latin_subchars[lang][2];
            }else if (p>=0x5b && p<=0x60){
                return latin_subchars[lang][p-0x5c+3];
            }else if (p>=0x7b && p<=0x7e){
                return latin_subchars[lang][p-0x7b+9];
	    }
	}
        return lang_chars[charset][p-0x20];
    }else
        return 0x20;
}

static void init_vbi_consts(priv_vbi_t* priv){
    int i,j;
    double ang;
    for(i=0; i<256; i++){
        j=i&0x7F;
        j^= j+j;
        j^= j<<2;
        j^= j<<4;
        fixParity[i]= i ^ (j&0x80) ^ 0x80;
    }

    for(i=0,ang=0; i<12; i++,ang+=M_PI/priv->bpb){
        si[i]= sin(ang);
        co[i]= cos(ang);
    }

    priv->bpb=(priv->ptsp->sampling_rate/6937500.0)*ONE_FIXP+0.5;
    priv->soc=FFMAX(9.2e-6*priv->ptsp->sampling_rate-priv->ptsp->offset, 0);
    priv->eoc=FFMIN(12.9e-6*priv->ptsp->sampling_rate-priv->ptsp->offset,
                    priv->ptsp->samples_per_line-43*8*priv->bpb/ONE_FIXP);
    if (priv->eoc - priv->soc<16*priv->bpb/ONE_FIXP){ // invalid [soc:eoc]
        priv->soc=0;
        priv->eoc=92;
    };
    priv->bp8bl=0.97*8*priv->bpb/ONE_FIXP; // -3% tolerance
    priv->bp8bh=1.03*8*priv->bpb/ONE_FIXP; // +3% tolerance
}
/**
 * \brief calculate increased/decreased by given value page number
 * \param curr  current page number in hexadecimal for
 * \param direction decimal value (can be negative) to add to value
 *        of curr parameter
 * \return new page number in hexadecimal form
 *
 * VBI page numbers are represented in special hexadecimal form, e.g.
 * page with number 123 (as seen by user) internally has number 0x123.
 * and equation 0x123+8 should be equal to 0x131 instead of regular 0x12b.
 *
 *
 * Page numbers 0xYYY (where Y is not belongs to (0..9).
 * Page number belongs to [0x000,0x799] or [0x100:0x899] (first 0 can be 
 * treated as '8')
 */
static int steppage(int p, int direction, int skip_hidden)
{
    if(skip_hidden)
        p=(p&15)+((p>>4)&15)*10+(p>>8)*100;
    p+=direction;
    if(skip_hidden){
        p=(p+800)%800;
        p=(p%10)+((p/10)%10)*16+(p/100)*256;
    }

    return p&0x7ff;
}

/*
------------------------------------------------------------------
   Cache stuff
------------------------------------------------------------------
*/

/**
 * \brief add/update entry in cache
 * \param priv private data structure
 * \param pg page to store in cache
 * \param line line to update (value below 0 means update entire page)
 */
static void put_to_cache(priv_vbi_t* priv,tt_page* pg,int line){
    tt_page* pgc; //page in cache
    int i,count;

    if(line<0){
        i=0;
	count=VBI_ROWS*VBI_COLUMNS;
    }else if(line<VBI_ROWS){
        i=line*VBI_COLUMNS;
        count=(line+1)*VBI_COLUMNS;
    }else
        return;

    pthread_mutex_lock(&(priv->buffer_mutex));

    if(!priv->ptt_cache[pg->pagenum]){
        priv->ptt_cache[pg->pagenum]=calloc(1,sizeof(tt_page));
        pgc=priv->ptt_cache[pg->pagenum];
    }else{
        pgc=priv->ptt_cache[pg->pagenum];
        while(pgc->next_subpage && pgc->subpagenum!=pg->subpagenum)
            pgc=pgc->next_subpage;

        if(pgc->subpagenum!=pg->subpagenum){
            pgc->next_subpage=calloc(1,sizeof(tt_page));
            pgc=pgc->next_subpage;
        }
    }
    pgc->pagenum=pg->pagenum;
    pgc->subpagenum=pg->subpagenum;
    pgc->lang=pg->lang;
    pgc->flags=pg->flags;
    //instead of copying entire page into cache, copy only undamaged
    //symbols into cache
    for(;i<count;i++){
        if(!(pg->raw[i]&0x80))
            pgc->raw[i]=pg->raw[i];
        else
            mp_msg(MSGT_TV,MSGL_DBG3,"char error. pg:%x, c[%d]=0x%x\n",
                pg->pagenum,i,pg->raw[i]);
    }
    pgc->active=1;
    pthread_mutex_unlock(&(priv->buffer_mutex));
}

/**
 * \brief get any subpage number of given page
 * \param priv private data structure
 * \param pagenum page number to search subpages in
 *
 * \return subpage number of first found subpage which belongs to
 * given page number
 *
 * \note page itself is subpage too (and usually has subpage number 0)
 */
static inline int get_subpagenum_from_cache(priv_vbi_t* priv, int pagenum){
    if (!priv->ptt_cache[pagenum])
        return -1;
    else
        return priv->ptt_cache[pagenum]->subpagenum;
}

/**
 * \brief get page from cache by it page and subpage number
 * \param priv private data structure
 * \param pagenum page number
 * \param subpagenum subpage number
 *
 * \return pointer to tt_page structure if requested page is found
 * and NULL otherwise
 */
static inline tt_page* get_from_cache(priv_vbi_t* priv, int pagenum,int subpagenum){
    tt_page* tp=priv->ptt_cache[pagenum];

    while(tp && tp->subpagenum!=subpagenum)
        tp=tp->next_subpage;
    return tp;
}

/**
 * \brief clears cache
 * \param priv private data structure
 *
 * Deletes all tt_page structures from cache and frees allocated memory.
 * Only zero-filled array of pointers remains in memory
 */
static void clear_cache(priv_vbi_t* priv){
    int i;
    tt_page* tp;
    for(i=0;i<VBI_MAX_PAGES;i++){
        while(priv->ptt_cache[i]){
            tp=priv->ptt_cache[i];
            priv->ptt_cache[i]=tp->next_subpage;
            free(tp);
        }
    }
}

/**
 * \brief cache initialization
 * \param priv private data structure
 *
 * \note Has to be called before any cache operations!
 */
static void init_cache(priv_vbi_t* priv){
    priv->ptt_cache=calloc(VBI_MAX_PAGES,sizeof(tt_page*));
}

/**
 * \brief destroys cache
 * \param priv private data structure
 *
 * Frees all memory allocated for cache (including array of pointers).
 * It is safe to call this routine multiple times
 */
static void destroy_cache(priv_vbi_t* priv){
    if(priv->ptt_cache){
        clear_cache(priv);
        free(priv->ptt_cache);
        priv->ptt_cache=NULL;
    }
}

/*
------------------------------------------------------------------
   Decoder stuff
------------------------------------------------------------------
*/
/**
 * \brief converts raw teletext page into useful format (1st rendering stage)
 * \param pg page to decode
 *
 * Routine fills tt_char structure of each teletext_page character with proper
 * info about foreground and background colors, character
 * type (graphics/control/text).
 */
static void decode_page(tt_char* p,int lang,unsigned char* raw)
{
    int c,gfx=0,lat=0;
    int i=0;
    int fg_color=0;
    int bg_color=0;
    int row,col;
    int separated=0;

    for(row=0;row<VBI_ROWS;row++)   {
        lat=(lang==0);
        gfx=0;
        fg_color=7;
        bg_color=0;
        separated=0;
        for(col=0;col<VBI_COLUMNS;col++){
            i=row*VBI_COLUMNS+col;
            c=raw[i];
            p[i].raw=c;
            if(c&0x80){ //damaged char
                p[i]=tt_error;
                continue;
            }
            p[i].gfx=gfx?(separated?2:1):0;
            p[i].lng=lat?0:lang;
            p[i].ctl=(c&0x60)==0?1:0;
            p[i].fg=fg_color;
            p[i].bg=bg_color;

            if ((c&0x60)==0){ //control chars
                if(c>=0x08 && c<=0x0f){
                }else if (c<=0x17){ //colors
                    fg_color=c&0x0f;
                    gfx=c>>4;
                }else if (c<=0x18){
                }else if (c<=0x1a){ //Contiguous/Separated gfx
                    separated=!(c&1);
                }else if (c<=0x1b){
                    lat=!lat;
                }else if (c<=0x1d){
                    bg_color=(c&1)?fg_color:0;
                    p[i].bg=bg_color;
                }
                p[i].ctl=1;
                p[i].unicode=p[i].gfx?0:' ';
                continue;
            }

            if(gfx){
                p[i].unicode=c-0x20;
                if (p[i].unicode>0x3f) p[i].unicode-=0x20;
            }else
                p[i].unicode=conv2uni(c,p[i].lng);

            p[i].fg=fg_color;
            p[i].bg=bg_color;
        }
    }
}

/**
 * \brief prepares current page for displaying
 * \param priv_vbi private data structure
 *
 * Routine adds some useful info (time and page number of page, grabbed by
 * background thread to top line of current page). Displays "No teletext"
 * string if no vbi data available.
 */
#define PRINT_HEX(dp,i,h) dp[i].unicode=((h)&0xf)>9?'A'+((h)&0xf)-10:'0'+((h)&0xf)
static void prepare_visible_page(priv_vbi_t* priv){
    tt_page *pg,*curr_pg;
    unsigned char *p;
    int i;

    pthread_mutex_lock(&(priv->buffer_mutex));
    mp_msg(MSGT_TV,MSGL_DBG3,"tvi_vbi: prepare_visible_page pg:0x%x, sub:0x%x\n",
        priv->pagenum,priv->subpagenum);
    if(priv->subpagenum==-1) //no page yet
        priv->subpagenum=get_subpagenum_from_cache(priv,priv->pagenum);

    pg=get_from_cache(priv,priv->pagenum,priv->subpagenum);
    mp_dbg(MSGT_TV,MSGL_DBG3,"tvi_vbi: prepare_vibible_page2 pg:0x%x, sub:0x%x\n",
        priv->pagenum,priv->subpagenum);

    curr_pg=get_from_cache(priv,priv->curr_pagenum,
        get_subpagenum_from_cache(priv,priv->curr_pagenum));
    if (!pg && !curr_pg){
        p=MSGTR_TV_NoTeletext;
        for(i=0;i<VBI_COLUMNS && *p;i++){
            GET_UTF8(priv->display_page[i].unicode,*p++,break;);
        }
        for(;i<VBI_ROWS*VBI_COLUMNS;i++)
            priv->display_page[i]=tt_space;
        pthread_mutex_unlock(&(priv->buffer_mutex));
        return;
    }

    if (!pg || !pg->active){
        for(i=0;i<VBI_ROWS*VBI_COLUMNS;i++){
            priv->display_page[i]=tt_space;
        }
    }else{
        decode_page(priv->display_page,pg->lang,pg->raw);
        mp_msg(MSGT_TV,MSGL_DBG3,"page #%x was decoded!\n",pg->pagenum);
    }

    PRINT_HEX(priv->display_page,0,(priv->curr_pagenum&0x700)?priv->curr_pagenum>>8:8);
    PRINT_HEX(priv->display_page,1,priv->curr_pagenum>>4);
    PRINT_HEX(priv->display_page,2,priv->curr_pagenum);
    priv->display_page[3].unicode=' ';
    priv->display_page[4].unicode=' ';
    switch(priv->pagenumdec>>12){
    case 1:
        priv->display_page[5].unicode='_';
        priv->display_page[6].unicode='_';
        PRINT_HEX(priv->display_page,7,priv->pagenumdec);
        break;
    case 2:
        priv->display_page[5].unicode='_';
        PRINT_HEX(priv->display_page,6,priv->pagenumdec>>4);
        PRINT_HEX(priv->display_page,7,priv->pagenumdec);
    break;
    default:
        PRINT_HEX(priv->display_page,5,(priv->pagenum&0x700)?priv->pagenum>>8:8);
        PRINT_HEX(priv->display_page,6,priv->pagenum>>4);
        PRINT_HEX(priv->display_page,7,priv->pagenum);
    }
    if(priv->subpagenum!=-1){
        priv->display_page[8].unicode='.';
        PRINT_HEX(priv->display_page,9,priv->subpagenum>>4);
        PRINT_HEX(priv->display_page,10,priv->subpagenum);
    }else{
        priv->display_page[8].unicode=' ';
        priv->display_page[9].unicode=' ';
        priv->display_page[10].unicode=' ';
    }
    priv->display_page[11].unicode=' ';
    for(i=VBI_TIME_LINEPOS;i<VBI_COLUMNS;i++){
        priv->display_page[i].unicode=curr_pg->raw[i];
    }
    pthread_mutex_unlock(&(priv->buffer_mutex));
}
/*
------------------------------------------------------------------
   Renderer stuff
------------------------------------------------------------------
*/
#ifdef DEBUG_DUMP
/**
 * \brief renders teletext page into given file
 * \param pt page to render
 * \param f opened file descriptor
 * \param pagenum which page to render
 * \param colored use colors not implementede yet)
 *
 * Text will be UTF8 encoded
 */
static void render2text(tt_page* pt,FILE* f,int colored){
    int i,j;
    unsigned int u;
    unsigned char buf[8];
    unsigned char tmp;
    int pos;
    tt_char dp[VBI_ROWS*VBI_COLUMNS];
    int color=0;
    int bkg=0;
    int c1,b1;
    if(!pt)
        return;
    fprintf(f,"+========================================+\n");
    fprintf(f,"| lang:%d pagenum:0x%x subpagenum:%d flags:0x%x|\n",
    pt->lang,
    pt->pagenum,
    pt->subpagenum,
    0);
    fprintf(f,"+----------------------------------------+\n");

    decode_page(dp,pt->lang,pt->raw);
    for(i=0;i<VBI_ROWS;i++){
        fprintf(f,"|");
        if(colored) fprintf(f,"\033[40m");
        for(j=0;j<VBI_COLUMNS;j++)
        {
             u=dp[i*VBI_COLUMNS+j].unicode;
              if(dp[i*VBI_COLUMNS+j].fg <= 7)
                c1=30+dp[i*VBI_COLUMNS+j].fg;
              else
                c1=38;
              if(dp[i*VBI_COLUMNS+j].bg <= 7)
                  b1=40+dp[i*VBI_COLUMNS+j].bg;
              else
                b1=40;
            if (b1!=bkg  && colored){
                fprintf(f,"\033[%dm",b1);
                bkg=b1;
            }
            if(c1!=color && colored){
                fprintf(f,"\033[%dm",c1);
                color=c1;
            }
            if(dp[i*VBI_COLUMNS+j].gfx){
                fprintf(f,"*");
            }else{
                pos=0;
                PUT_UTF8(u,tmp,if(pos<7) buf[pos++]=tmp;);
                buf[pos]='\0';
                fprintf(f,"%s",buf);
            }
        }

        if (colored) fprintf(f,"\033[0m");
        color=-1;bkg=-1;
        fprintf(f,"|\n");
    }
#if 1
    //for debug
    fprintf(f,"+====================raw=================+\n");
    for(i=0;i<VBI_ROWS;i++){
        for(j=0;j<VBI_COLUMNS;j++)
            fprintf(f,"%02x ",dp[i*VBI_COLUMNS+j].raw);
        fprintf(f,"\n");
    }
    fprintf(f,"+====================lng=================+\n");
    for(i=0;i<VBI_ROWS;i++){
        for(j=0;j<VBI_COLUMNS;j++)
            fprintf(f,"%02x ",dp[i*VBI_COLUMNS+j].lng);
        fprintf(f,"\n");
    }
#endif
    fprintf(f,"+========================================+\n");
}

/**
 * \brief dump page into pgXXX.txt file in vurrent directory
 * \param pt page to dump
 *
 * \note XXX in filename is page number
 * \note use only for debug purposes
 */
static void dump_page(tt_page* pt)
{
    FILE*f;
    char name[100];
    snprintf(name,99,"pg%x.txt",pt->pagenum);
    f=fopen(name,"wb");
    render2text(pt,f,1);
    fclose(f);
}
#endif //DEBUG_DUMP


/**
 * \brief checks whether page is ready and copies it into cache array if so
 * \param priv private data structure
 * \param magAddr page's magazine address (0-7)
 *
 * Routine also calls decode_page to perform 1st stage of rendering
 */
static void store_in_cache(priv_vbi_t* priv, int magAddr, int line){
    mp_msg(MSGT_TV,MSGL_DBG2,"store_in_cache(%d): pagenum:%x\n",
        priv->mag[magAddr].order,
        priv->mag[magAddr].pt->pagenum);

    put_to_cache(priv,priv->mag[magAddr].pt,line);
    priv->curr_pagenum=priv->mag[magAddr].pt->pagenum;

#ifdef DEBUG_DUMP
    dump_page(get_from_cache(priv,
        priv->mag[magAddr].pt->pagenum,
        priv->mag[magAddr].pt->subpagenum));
#endif
}


/*
------------------------------------------------------------------
  Grabber stuff
------------------------------------------------------------------
*/
#define PLL_SAMPLES 4
#define PLL_ERROR   4
#define PLL_ADJUST  4

/**
 * \brief adjust current phase for better signal decoding
 * \param n count of bytes processed (?)
 * \param err count of error bytes (?)
 *
 * \remarks code was got from MythTV project
 */
static void pll_add(priv_vbi_t* priv,int n,int err){
    if(priv->pll_fixed)
        return;
    if(err>PLL_ERROR*2/3)
        err=PLL_ERROR*2/3;
    priv->pll_err+=err;
    priv->pll_cnt+=n;
    if(priv->pll_cnt<PLL_SAMPLES)
        return;
    if(priv->pll_err>PLL_ERROR)
    {
        if(priv->pll_err>priv->pll_lerr)
            priv->pll_dir= -priv->pll_dir;
        priv->pll_lerr=priv->pll_err;
        priv->pll_adj+=priv->pll_dir;
        if (priv->pll_adj<-PLL_ADJUST || priv->pll_adj>PLL_ADJUST)
        {
            priv->pll_adj=0;
            priv->pll_dir=-1;
            priv->pll_lerr=0;
        }
        mp_msg(MSGT_TV,MSGL_DBG3,"vbi: pll_adj=%2d\n",priv->pll_adj);
    }
    priv->pll_cnt=0;
    priv->pll_err=0;
}

/**
 * \brief reset error correction
 * \param priv private data structure
 * \param fine_tune shift value for adjusting
 *
 * \remarks code was got from MythTV project
 */
static void pll_reset(priv_vbi_t* priv,int fine_tune){
    priv->pll_fixed=fine_tune >= -PLL_ADJUST && fine_tune <= PLL_ADJUST;

    priv->pll_err=0;
    priv->pll_lerr=0;
    priv->pll_cnt=0;
    priv->pll_dir=-1;
    priv->pll_adj=0;
    if(priv->pll_fixed)
        priv->pll_adj=fine_tune;
    if(priv->pll_fixed)
        mp_msg(MSGT_TV,MSGL_DBG3,"pll_reset (fixed@%2d)\n",priv->pll_adj);
    else
        mp_msg(MSGT_TV,MSGL_DBG3,"pll_reset (auto)\n");

}
/**
 * \brief decode packet 0 (teletext page header)
 * \param priv private data structure
 * \param data raw teletext data (with not applied hamm correction yet)
 * \param magAddr teletext page's magazine address
 *
 * \remarks
 * data buffer was shifted by 6 and now contains:
 *  0..1 page number
 *  2..5 sub-code
 *  6..7 control codes
 *  8..39 display data
 *
 *  only first 8 bytes protected by Hamm 8/4 code
 */
static int decode_pkt0(priv_vbi_t* priv,unsigned char* data,int magAddr)
{
    int d[8];
    int i,err;

    if (magAddr<0 || magAddr>7)
        return 0;
    for(i=0;i<8;i++){
        d[i]= corrHamm48[ data[i] ];
        if(d[i]&0x80){
            pll_add(priv,2,4);

            if(priv->mag[magAddr].pt)
                  free(priv->mag[magAddr].pt);
            priv->mag[magAddr].pt=NULL;
            priv->mag[magAddr].order=0;
            return 0;
        }
    }
    if (!priv->mag[magAddr].pt)
        priv->mag[magAddr].pt= malloc(sizeof(tt_page));

    priv->mag[magAddr].lang=(d[7]>>1)&0x7;
    priv->mag[magAddr].pt->lang=priv->mag[magAddr].lang;
    priv->mag[magAddr].pt->subpagenum=(d[2]|(d[3]<<4)|(d[4]<<8)|(d[5]<<12))&0x3f7f;
    priv->mag[magAddr].pt->pagenum=(magAddr<<8) | d[0] | (d[1]<<4);
    priv->mag[magAddr].pt->flags=( d[6] | (d[7]<<4));

    memset(priv->mag[magAddr].pt->raw, 0x00, VBI_COLUMNS*VBI_ROWS);
    priv->mag[magAddr].order=0;

    for(i=0;i<8;i++){
        priv->mag[magAddr].pt->raw[i]=0x20;
    }
    err=0;
    for(i=8; i<VBI_COLUMNS; i++){
        data[i]= fixParity[data[i]];
        priv->mag[magAddr].pt->raw[i]=data[i];
        if(data[i]&0x80) //Error
            err++;
        pll_add(priv,1,err);
    }

    store_in_cache(priv,magAddr,0);

    return 1;
}

/**
 * \brief decode packets 1..24 (teletext page header)
 * \param priv private data structure
 * \param data raw teletext data
 * \param magAddr teletext page's magazine address
 * \param rowAddr teletext page's row number
 *
 * \remarks
 * data buffer was shifted by 6 and now contains 40 bytes of display data:
 * this type of packet is not proptected by Hamm 8/4 code
 */
static void decode_pkt_page(priv_vbi_t* priv,unsigned char*data,int magAddr,int rowAddr){
    int i,err;
    if (!priv->mag[magAddr].pt)
        return;

    priv->mag[magAddr].order=rowAddr;

    err=0;
    for(i=0; i<VBI_COLUMNS; i++){
        data[i]= fixParity[ data[i] ];
        priv->mag[magAddr].pt->raw[i+rowAddr*VBI_COLUMNS]=data[i];
        if( data[i]&0x80) //HammError
            err++;
    }
    pll_add(priv,1,err);

    store_in_cache(priv,magAddr,rowAddr);
}

/**
 * \brief decodes raw vbi data (signal amplitudes) into sequence of bytes
 * \param priv private data structure
 * \param buf raw vbi data (one line of frame)
 * \param data output buffer for decoded bytes (at least 45 bytes long)
 *
 * Used XawTV's algorithm. Signal phase is calculated with help of starting clock
 * run-in sequence (min/max values and bit distance values are calculated)
 */
static int decode_raw_line_runin(priv_vbi_t* priv,unsigned char* buf,unsigned char* data){
    const int magic= 0x27; // reversed 1110010
    int dt[256],hi[6],lo[6];
    int i,x,r;
    int decoded;
    int sync;
    unsigned char min,max;
    int thr=0; //threshold

    //stubs
    int soc=priv->soc;
    int eoc=priv->eoc;

    for(i=soc;i<eoc;i++)
        dt[i]=buf[i+priv->bpb/ONE_FIXP]-buf[i];    // amplifies the edges best.
    /* set barrier */
    for (i=eoc; i<eoc+16; i+=2)
        dt[i]=100, dt[i+1]=-100;

    /* find 6 rising and falling edges */
    for (i=soc, x=0; x<6; ++x)
    {
        while (dt[i]<32)
            i++;
        hi[x]=i;
        while (dt[i]>-32)
            i++;
        lo[x]=i;
    }
    if (i>=eoc)
    {
        return 0;      // not enough periods found
    }
    i=hi[5]-hi[1]; // length of 4 periods (8 bits)
    if (i<priv->bp8bl || i>priv->bp8bh)
    {
        mp_msg(MSGT_TV,MSGL_DBG3,"vbi: wrong freq %d (%d,%d)\n",
            i,priv->bp8bl,priv->bp8bh);
        return 0;      // bad frequency
    }
    /* AGC and sync-reference */
    min=255, max=0, sync=0;
    for (i=hi[4]; i<hi[5]; ++i)
        if (buf[i]>max)
            max=buf[i], sync=i;
    for (i=lo[4]; i<lo[5]; ++i)
        if (buf[i]<min)
            min=buf[i];
    thr=(min+max)/2;

    buf+=sync;
    // searching for '11'
    for(i=priv->pll_adj*priv->bpb/10;i<16*priv->bpb;i+=priv->bpb)
        if(buf[FIXP2INT(i)]>thr && buf[FIXP2INT(i+priv->bpb)]>thr)
            break;
    r=0;
    for(decoded=1; decoded<= (VBI_COLUMNS+3)<<3;decoded++){
        r>>=1;
        if(buf[FIXP2INT(i)]>thr) r|=0x80;
        if(!(decoded & 0x07)){
            data[(decoded>>3) - 1]=r;
            r=0;
        }
        i+=priv->bpb;
    }
    if(data[0]!=magic)
        return 0; //magic not found

    //stub
    for(i=0;i<43;i++){
        data[i]=data[i+1];
    }
    mp_msg(MSGT_TV,MSGL_DBG3,"thr:%d sync:%d ",thr,sync);

    return 1;
}

/**
 * \brief decodes raw vbi data (signal amplitudes) into sequence of bytes
 * \param priv private data structure
 * \param buf raw vbi data (one line of frame)
 * \param data output buffer for decoded bytes (at least 45 bytes long)
 *
 * Used Michael Niedermayer's algorithm.
 * Signal phase is calculated using correlation between given samples data and
 * pure sine
 */
static int decode_raw_line_sine(priv_vbi_t* priv,unsigned char* buf,unsigned char* data){
    int i,x,r,amp,xFixp;
    int avg=0;
    double sin_sum=0, cos_sum=0;

    for(x=0; x< FIXP2INT(10*priv->bpb); x++)
      avg+=buf[x];

    avg/=FIXP2INT(10*priv->bpb);

    for(x=0; x<12; x++){
      amp= buf[x<<1];
      sin_sum+= si[x]*(amp-avg);
      cos_sum+= co[x]*(amp-avg);
    }
    //this is always zero. Why ?
    xFixp= atan(sin_sum/cos_sum)*priv->bpb/M_PI;

    //Without this line the result is full of errors
    //and routine is unable to find magic sequence
    buf+=FIXP2INT(10*priv->bpb);

    r=0;
    for(x=FIXP2INT(xFixp);x<70;x=FIXP2INT(xFixp)){
      r=(r<<1) & 0xFFFF;
      if(buf[x]>avg) r|=1;
      xFixp+=priv->bpb;
      if(r==0xAAE4) break;
    }

    //this is not teletext
    if (r!=0xaae4) return 0;

    //Decode remaining 45-2(clock run-in)-1(framing code)=42 bytes
    for(i=1; i<=(42<<3); i++){
      r>>=1;
      x=FIXP2INT(xFixp);
      if(buf[x]> avg)
          r|=0x80;

      if(!(i & 0x07)){
          data[(i>>3)-1]=r;
          r=0;
      }
      xFixp+=priv->bpb;
    }

    return 1;
}

/**
 * \brief decodes all vbi lines from one video frame
 * \param priv private data structure
 * \param buf buffer with raw vbi data in it
 *
 * \note buffer size have to be at least priv->ptsp->bufsize bytes
 */
static void vbi_decode(priv_vbi_t* priv,unsigned char*buf){
    int magAddr;
    int pkt;
    unsigned char data[64];
    unsigned char* linep;
    int d0,d1;
    int i=0;
    mp_msg(MSGT_TV,MSGL_DBG3,"vbi: vbi_decode\n");
    for(linep=buf; linep<buf+priv->ptsp->bufsize; linep+=priv->ptsp->samples_per_line,i++){
#if 0
        /*
          This routine is alternative implementation of raw VBI data decoding.
	  Unfortunately, it detects only about 20% of incoming data,
          but Michael says that this algorithm is better, and he wants to fix it.
        */
        if(decode_raw_line_sine(priv,linep,data)<=0){
#endif
        if(decode_raw_line_runin(priv,linep,data)<=0){
             continue; //this is not valid teletext line
        }
        d0= corrHamm48[ data[0] ];
        d1= corrHamm48[ data[1] ];

        if(d0&0x80 || d1&0x80){
           pll_add(priv,2,4);
           mp_msg(MSGT_TV,MSGL_V,"vbi_decode(%d):HammErr after decode_raw_line\n",i);

           continue; //hamError
        }
        magAddr=d0 & 0x7;
        pkt=(d0>>3)|(d1<<1);
        mp_msg(MSGT_TV,MSGL_DBG3,"vbi_decode(%d):%x %x (mag:%x, pkt:%d)\n",
            i,d0,d1,magAddr,pkt);
        if(!pkt){
            decode_pkt0(priv,data+2,magAddr); //skip MRGA
        }else if(pkt>0 && pkt<VBI_ROWS){
            if(!priv->mag[magAddr].pt) continue;
            decode_pkt_page(priv,data+2,magAddr,pkt);//skip MRGA
        } else {
            mp_msg(MSGT_TV,MSGL_DBG3,"unsupported packet:%d\n",pkt);
        }
    }

}

/*
---------------------------------------------------------------------------------
    Public routines
---------------------------------------------------------------------------------
*/

/**
 * \brief toggles teletext page displaying format
 * \param priv_vbi private data structure
 * \param flag new format
 * \return
 *   TVI_CONTROL_TRUE is success,
 *   TVI_CONTROL_FALSE otherwise
 *
 * flag:
 * 0 - opaque
 * 1 - transparent
 * 2 - opaque  with black foreground color (only in bw mode)
 * 3 - transparent  with black foreground color (only in bw mode)
 */
static int teletext_set_format(priv_vbi_t * priv, teletext_format flag)
{
    flag&=3;

    mp_msg(MSGT_TV,MSGL_DBG3,"teletext_set_format_is called. mode:%d\n",flag);
    pthread_mutex_lock(&(priv->buffer_mutex));

    priv->tformat=flag;

    priv->pagenumdec=0;

    pthread_mutex_unlock(&(priv->buffer_mutex));
    return TVI_CONTROL_TRUE;
}

/**
 * \brief append just entered digit to editing page number
 * \param priv_vbi private data structure
 * \param dec decimal digit to append
 *
 *  dec:
 *   '0'..'9' append digit
 *    '-' remove last digit (backspace emulation)
 *
 * This routine allows user to jump to arbitrary page.
 * It implements simple page number editing algorithm.
 *
 * Subsystem can be on one of two modes: normal and page number edit mode.
 * Zero value of priv->pagenumdec means normal mode
 * Non-zero value means page number edit mode and equals to packed
 * decimal number of already entered part of page number.
 *
 * How this works.
 * Let's assume that current mode is normal (pagenumdec is zero), teletext page
 * 100 are displayed as usual. topmost left corner of page contains page number.
 * Then vbi_add_dec is sequentially called (through slave
 * command of course) with 1,4,-,2,3 * values of dec parameter.
 *
 * +-----+------------+------------------+
 * | dec | pagenumdec | displayed number |
 * +-----+------------+------------------+
 * |     | 0x000      | 100              |
 * +-----+------------+------------------+
 * | 1   | 0x001      | __1              |
 * +-----+------------+------------------+
 * | 4   | 0x014      | _14              |
 * +-----+------------+------------------+
 * | -   | 0x001      | __1              |
 * +-----+------------+------------------+
 * | 2   | 0x012      | _12              |
 * +-----+------------+------------------+
 * | 3   | 0x123      | 123              |
 * +-----+------------+------------------+
 * |     | 0x000      | 123              |
 * +-----+------------+------------------+
 *
 * pagenumdec will automatically receive zero value after third digit of page
 * number is entered and current page will be switched to another one with
 * entered page number.
 */
static void vbi_add_dec(priv_vbi_t * priv, char *dec)
{
    int count, shift;
    if (!dec)
        return;
    if (!priv->on)
        return;
    if ((*dec<'0' || *dec>'9') && *dec!='-')
        return;
    if (!priv->pagenumdec) //first digit cannot be '0','9' or '-'
        if(*dec=='-' || *dec=='0' || *dec=='9')
            return;
    pthread_mutex_lock(&(priv->buffer_mutex));
    count=(priv->pagenumdec>>12)&0xf;
    if (*dec=='-') {
        count--;
        if (count)
            priv->pagenumdec=((priv->pagenumdec>>4)&0xfff)|(count<<12);
        else
            priv->pagenumdec=0;
    } else {
        shift = count * 4;
        count++;
        priv->pagenumdec=
            (((priv->pagenumdec)<<4|(*dec-'0'))&0xfff)|(count<<12);
        if (count==3) {
            priv->pagenum=priv->pagenumdec&0x7ff;
            priv->subpagenum=get_subpagenum_from_cache(priv,priv->pagenum);
            priv->pagenumdec=0;
        }
    }
    pthread_mutex_unlock(&(priv->buffer_mutex));
}


/**
 * \brief Teletext control routine
 * \param priv_vbi private data structure
 * \param cmd command
 * \param arg command parameter (has to be not null)
 */
int teletext_control(void* p, int cmd, void *arg)
{
    int fine_tune=99;
    priv_vbi_t* priv=(priv_vbi_t*)p;

    if (!priv && cmd!=TV_VBI_CONTROL_START)
        return TVI_CONTROL_FALSE;
    if (!arg && cmd!=TV_VBI_CONTROL_STOP)
        return TVI_CONTROL_FALSE;

    switch (cmd) {
    case TV_VBI_CONTROL_RESET:
    {
        tv_param_t* tv_param=arg;
        pthread_mutex_lock(&(priv->buffer_mutex));
        priv->pagenumdec=0;
        clear_cache(priv);
        priv->pagenum=steppage(0,tv_param->tpage&0x7ff,1);
        priv->tformat=tv_param->tformat;
        priv->subpagenum=0;
        pll_reset(priv,fine_tune);
        pthread_mutex_unlock(&(priv->buffer_mutex));
        return TVI_CONTROL_TRUE;
    }
    case TV_VBI_CONTROL_START:
    {
        int i;
        tt_stream_props* ptsp=*(tt_stream_props**)arg;

        if(!ptsp)
            return TVI_CONTROL_FALSE;

        priv=calloc(1,sizeof(priv_vbi_t));

        priv->ptsp=malloc(sizeof(tt_stream_props));
        memcpy(priv->ptsp,ptsp,sizeof(tt_stream_props));
        *(priv_vbi_t**)arg=priv;

        priv->subpagenum=0;
        pthread_mutex_init(&priv->buffer_mutex, NULL);
        priv->pagenumdec=0;
        for(i=0;i<VBI_ROWS*VBI_COLUMNS;i++)
            priv->display_page[i]=tt_space;

        priv->mag=calloc(8,sizeof(mag_t));
        init_cache(priv);
        init_vbi_consts(priv);
        pll_reset(priv,fine_tune);
        return TVI_CONTROL_TRUE;
    }
    case TV_VBI_CONTROL_STOP:
    {
        if(priv->mag)
            free(priv->mag);
        if(priv->ptsp)
            free(priv->ptsp);
        destroy_cache(priv);
        free(priv);
        return TVI_CONTROL_TRUE;
    }
    case TV_VBI_CONTROL_SET_MODE:
        priv->on=(*(int*)arg%2);
        return TVI_CONTROL_TRUE;
    case TV_VBI_CONTROL_GET_MODE:
        *(int*)arg=priv->on;
        return TVI_CONTROL_TRUE;
    case TV_VBI_CONTROL_SET_FORMAT:
        return teletext_set_format(priv, *(int *) arg);
    case TV_VBI_CONTROL_GET_FORMAT:
        pthread_mutex_lock(&(priv->buffer_mutex));
        *(int*)arg=priv->tformat;
        pthread_mutex_unlock(&(priv->buffer_mutex));
        return TVI_CONTROL_TRUE;
    case TV_VBI_CONTROL_GET_HALF_PAGE:
        if(!priv->on)
            return TVI_CONTROL_FALSE;
        *(int *)arg=priv->zoom;
        return TVI_CONTROL_TRUE;
    case TV_VBI_CONTROL_SET_HALF_PAGE:
    {
        int val=*(int*)arg;
        val%=3;
        if(val<0)
            val+=3;
        pthread_mutex_lock(&(priv->buffer_mutex));
        priv->zoom=val;
        pthread_mutex_unlock(&(priv->buffer_mutex));
        return TVI_CONTROL_TRUE;
    }
    case TV_VBI_CONTROL_SET_PAGE:
    {
        int val=*(int *) arg;
        if(val<100 || val>0x899)
            return TVI_CONTROL_FALSE;
        pthread_mutex_lock(&(priv->buffer_mutex));
        priv->pagenum=val&0x7ff;
        priv->subpagenum=get_subpagenum_from_cache(priv,priv->pagenum);
        priv->pagenumdec=0;
        pthread_mutex_unlock(&(priv->buffer_mutex));
        return TVI_CONTROL_TRUE;
    }
    case TV_VBI_CONTROL_STEP_PAGE:
    {
        int direction=*(int *) arg;
        pthread_mutex_lock(&(priv->buffer_mutex));
        priv->pagenum=steppage(priv->pagenum, direction,1);
        priv->subpagenum=get_subpagenum_from_cache(priv,priv->pagenum);
        priv->pagenumdec=0;
        pthread_mutex_unlock(&(priv->buffer_mutex));
        return TVI_CONTROL_TRUE;
    }
    case TV_VBI_CONTROL_GET_PAGE:
        *(int*)arg=((priv->pagenum+0x700)&0x7ff)+0x100;
        return TVI_CONTROL_TRUE;
    case TV_VBI_CONTROL_SET_SUBPAGE:
        pthread_mutex_lock(&(priv->buffer_mutex));
        priv->pagenumdec=0;
        priv->subpagenum=*(int*)arg;
        if(priv->subpagenum<0)
            priv->subpagenum=0;
        if(priv->subpagenum>=VBI_MAX_SUBPAGES)
            priv->subpagenum=VBI_MAX_SUBPAGES-1;
        pthread_mutex_unlock(&(priv->buffer_mutex));
        return TVI_CONTROL_TRUE;
    case TV_VBI_CONTROL_GET_SUBPAGE:
        *(int*)arg=priv->subpagenum;
        return TVI_CONTROL_TRUE;
    case TV_VBI_CONTROL_ADD_DEC:
        vbi_add_dec(priv, *(char **) arg);
        return TVI_CONTROL_TRUE;
    case TV_VBI_CONTROL_DECODE_PAGE:
        vbi_decode(priv,*(unsigned char**)arg);
        return TVI_CONTROL_TRUE;
    case TV_VBI_CONTROL_GET_VBIPAGE:
        if(!priv->on)
            return TVI_CONTROL_FALSE;
        prepare_visible_page(priv);
        *(void **)arg=priv->display_page;
        return TVI_CONTROL_TRUE;
    }
    return TVI_CONTROL_UNKNOWN;
}

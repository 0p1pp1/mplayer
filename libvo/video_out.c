
/* this file contains libvo's common functions, variables used by 
   many/all drivers. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <unistd.h>
//#include <sys/mman.h>

#include "config.h"
#include "video_out.h"

#include "mp_msg.h"
#include "help_mp.h"

#include "../osdep/shmem.h"

//int vo_flags=0;

// currect resolution/bpp on screen:  (should be autodetected by vo_init())
int vo_depthonscreen=0;
int vo_screenwidth=0;
int vo_screenheight=0;

int vo_config_count=0;

// requested resolution/bpp:  (-x -y -bpp options)
int vo_dx=0;
int vo_dy=0;
int vo_dwidth=0;
int vo_dheight=0;
int vo_dbpp=0;

int vo_nomouse_input = 0;
int vo_grabpointer = 1;
int vo_doublebuffering = 0;
int vo_vsync = 0;
int vo_fs = 0;
int vo_fsmode = 0;
float vo_panscan = 0.0f;
int vo_ontop = 0;
int vo_adapter_num=0;
int vo_refresh_rate=0;
int vo_keepaspect=1;
int vo_rootwin=0;

int vo_pts=0; // for hw decoding
float vo_fps=0; // for mp1e rte

char *vo_subdevice = NULL;
int vo_directrendering=0;

int vo_colorkey = 0x0000ff00; // default colorkey is green
			      // (0xff000000 means that colorkey has been disabled)

//
// Externally visible list of all vo drivers
//
extern vo_functions_t video_out_mga;
extern vo_functions_t video_out_xmga;
extern vo_functions_t video_out_x11;
extern vo_functions_t video_out_xover;
extern vo_functions_t video_out_xvmc;
extern vo_functions_t video_out_xv;
extern vo_functions_t video_out_gl;
extern vo_functions_t video_out_gl2;
extern vo_functions_t video_out_dga;
extern vo_functions_t video_out_fsdga;
extern vo_functions_t video_out_sdl;
extern vo_functions_t video_out_3dfx;
extern vo_functions_t video_out_tdfxfb;
extern vo_functions_t video_out_null;
//extern vo_functions_t video_out_odivx;
extern vo_functions_t video_out_zr;
extern vo_functions_t video_out_zr2;
extern vo_functions_t video_out_bl;
extern vo_functions_t video_out_syncfb;
extern vo_functions_t video_out_fbdev;
extern vo_functions_t video_out_fbdev2;
extern vo_functions_t video_out_svga;
extern vo_functions_t video_out_png;
extern vo_functions_t video_out_ggi;
extern vo_functions_t video_out_aa;
extern vo_functions_t video_out_caca;
extern vo_functions_t video_out_mpegpes;
extern vo_functions_t video_out_yuv4mpeg;
#ifdef HAVE_DIRECTX
extern vo_functions_t video_out_directx;
#endif
#ifdef HAVE_DXR2
extern vo_functions_t video_out_dxr2;
#endif
extern vo_functions_t video_out_dxr3;
#ifdef HAVE_JPEG
extern vo_functions_t video_out_jpeg;
#endif
#ifdef HAVE_GIF
extern vo_functions_t video_out_gif89a;
#endif
#ifdef HAVE_VESA
extern vo_functions_t video_out_vesa;
#endif
#ifdef HAVE_DIRECTFB
extern vo_functions_t video_out_directfb;
#if DIRECTFBVERSION >= 915
extern vo_functions_t video_out_dfbmga;
#endif
#endif
#ifdef CONFIG_VIDIX
extern vo_functions_t video_out_xvidix;
extern vo_functions_t video_out_winvidix;
extern vo_functions_t video_out_cvidix;
#endif
#ifdef HAVE_TDFX_VID
extern vo_functions_t video_out_tdfx_vid;
#endif
#ifdef HAVE_TGA
extern vo_functions_t video_out_tga;
#endif
#ifdef MACOSX
extern vo_functions_t video_out_quartz;
#endif
extern vo_functions_t video_out_pnm;
extern vo_functions_t video_out_md5sum;

vo_functions_t* video_out_drivers[] =
{
#ifdef HAVE_TDFX_VID
        &video_out_tdfx_vid,
#endif
#ifdef HAVE_DIRECTX
        &video_out_directx,
#endif
#ifdef MACOSX
	&video_out_quartz,
#endif
#ifdef HAVE_XMGA
        &video_out_xmga,
#endif
#ifdef HAVE_MGA
        &video_out_mga,
#endif
#ifdef HAVE_SYNCFB
        &video_out_syncfb,
#endif
#ifdef HAVE_3DFX
        &video_out_3dfx,
#endif
#ifdef HAVE_TDFXFB
        &video_out_tdfxfb,
#endif
#ifdef HAVE_XVMC
        &video_out_xvmc,
#endif
#ifdef HAVE_XV
        &video_out_xv,
#endif
#ifdef HAVE_X11
        &video_out_x11,
        &video_out_xover,
#endif
#ifdef HAVE_GL
	#ifndef GL_WIN32
	        &video_out_gl,
	#endif
        &video_out_gl2,
#endif
#ifdef HAVE_DGA
        &video_out_dga,
//        &video_out_fsdga,
#endif
#ifdef HAVE_SDL
        &video_out_sdl,
#endif
#ifdef HAVE_GGI
	&video_out_ggi,
#endif
#ifdef HAVE_FBDEV
	&video_out_fbdev,
	&video_out_fbdev2,
#endif
#ifdef HAVE_SVGALIB
	&video_out_svga,
#endif
#ifdef HAVE_AA
	&video_out_aa,
#endif
#ifdef HAVE_CACA
	&video_out_caca,
#endif
#ifdef HAVE_DXR2
	&video_out_dxr2,
#endif
#ifdef HAVE_DXR3
	&video_out_dxr3,
#endif
#ifdef HAVE_ZR
	&video_out_zr,
	&video_out_zr2,
#endif
#ifdef HAVE_BL
	&video_out_bl,
#endif

#ifdef HAVE_PNG
	&video_out_png,
#endif	
#ifdef HAVE_JPEG
	&video_out_jpeg,
#endif
#ifdef HAVE_GIF
	&video_out_gif89a,
#endif
        &video_out_null,
//        &video_out_odivx,
	&video_out_mpegpes,
	&video_out_yuv4mpeg,
#ifdef HAVE_VESA
	&video_out_vesa,
#endif
#ifdef HAVE_DIRECTFB
	&video_out_directfb,
#if DIRECTFBVERSION >= 915
        &video_out_dfbmga,
#endif
#endif
#ifdef CONFIG_VIDIX
#ifdef HAVE_X11
	&video_out_xvidix,
#endif
#ifdef WIN32
    &video_out_winvidix,
#endif
    &video_out_cvidix,
#endif
#ifdef HAVE_TGA
        &video_out_tga,
#endif
    &video_out_pnm,
    &video_out_md5sum,
        NULL
};

void list_video_out(){
      int i=0;
      mp_msg(MSGT_CPLAYER, MSGL_INFO, MSGTR_AvailableVideoOutputDrivers);
      while (video_out_drivers[i]) {
        const vo_info_t *info = video_out_drivers[i++]->info;
      	printf("\t%s\t%s\n", info->short_name, info->name);
      }
      printf("\n");
}

vo_functions_t* init_best_video_out(char** vo_list){
    int i;
    // first try the preferred drivers, with their optional subdevice param:
    if(vo_list && vo_list[0])
      while(vo_list[0][0]){
        char* vo=strdup(vo_list[0]);
	vo_subdevice=strchr(vo,':');
	if (!strcmp(vo, "pgm"))
	    mp_msg(MSGT_CPLAYER, MSGL_ERR, MSGTR_VO_PGM_HasBeenReplaced);
	if (!strcmp(vo, "md5"))
	    mp_msg(MSGT_CPLAYER, MSGL_ERR, MSGTR_VO_MD5_HasBeenReplaced);
	if(vo_subdevice){
	    vo_subdevice[0]=0;
	    ++vo_subdevice;
	}
	for(i=0;video_out_drivers[i];i++){
	    vo_functions_t* video_driver=video_out_drivers[i];
	    const vo_info_t *info = video_driver->info;
	    if(!strcmp(info->short_name,vo)){
		// name matches, try it
		if(!video_driver->preinit(vo_subdevice))
		{
		    free(vo);
		    return video_driver; // success!
		}
	    }
	}
        // continue...
	free(vo);
	++vo_list;
	if(!(vo_list[0])) return NULL; // do NOT fallback to others
      }
    // now try the rest...
    vo_subdevice=NULL;
    for(i=0;video_out_drivers[i];i++){
	vo_functions_t* video_driver=video_out_drivers[i];
	if(!video_driver->preinit(vo_subdevice))
	    return video_driver; // success!
    }
    return NULL;
}


#if defined(HAVE_FBDEV)||defined(HAVE_VESA)  
/* Borrowed from vo_fbdev.c 
Monitor ranges related functions*/

char *monitor_hfreq_str = NULL;
char *monitor_vfreq_str = NULL;
char *monitor_dotclock_str = NULL;

float range_max(range_t *r)
{
float max = 0;

	for (/* NOTHING */; (r->min != -1 && r->max != -1); r++)
		if (max < r->max) max = r->max;
	return max;
}


int in_range(range_t *r, float f)
{
	for (/* NOTHING */; (r->min != -1 && r->max != -1); r++)
		if (f >= r->min && f <= r->max)
			return 1;
	return 0;
}

range_t *str2range(char *s)
{
	float tmp_min, tmp_max;
	char *endptr = s;	// to start the loop
	range_t *r = NULL;
	int i;

	if (!s)
		return NULL;
	for (i = 0; *endptr; i++) {
		if (*s == ',')
			goto out_err;
		if (!(r = (range_t *) realloc(r, sizeof(*r) * (i + 2)))) {
			printf("can't realloc 'r'\n");
			return NULL;
		}
		tmp_min = strtod(s, &endptr);
		if (*endptr == 'k' || *endptr == 'K') {
			tmp_min *= 1000.0;
			endptr++;
		} else if (*endptr == 'm' || *endptr == 'M') {
			tmp_min *= 1000000.0;
			endptr++;
		}
		if (*endptr == '-') {
			tmp_max = strtod(endptr + 1, &endptr);
			if (*endptr == 'k' || *endptr == 'K') {
				tmp_max *= 1000.0;
				endptr++;
			} else if (*endptr == 'm' || *endptr == 'M') {
				tmp_max *= 1000000.0;
				endptr++;
			}
			if (*endptr != ',' && *endptr)
				goto out_err;
		} else if (*endptr == ',' || !*endptr) {
			tmp_max = tmp_min;
		} else
			goto out_err;
		r[i].min = tmp_min;
		r[i].max = tmp_max;
		if (r[i].min < 0 || r[i].max < 0)
			goto out_err;
		s = endptr + 1;
	}
	r[i].min = r[i].max = -1;
	return r;
out_err:
	if (r)
		free(r);
	return NULL;
}

/* Borrowed from vo_fbdev.c END */
#endif


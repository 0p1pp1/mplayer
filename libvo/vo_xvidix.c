/*
    VIDIX accelerated overlay in a X window
    
    (C) Alex Beregszaszi & Zoltan Ponekker & Nick Kurshev
    
    WS window manager by Pontscho/Fresh!

    Based on vo_gl.c and vo_vesa.c and vo_xmga.c (.so mastah! ;))
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <errno.h>

#include "config.h"
#include "video_out.h"
#include "video_out_internal.h"

#include <X11/Xlib.h>
#include <X11/Xutil.h>
//#include <X11/keysym.h>

#ifdef HAVE_XINERAMA
#include <X11/extensions/Xinerama.h>
#endif

#include "x11_common.h"
#include "aspect.h"
#include "mp_msg.h"

#include "vosub_vidix.h"
#include "../vidix/vidixlib.h"

LIBVO_EXTERN(xvidix)

static vo_info_t vo_info = 
{
    "X11 (VIDIX)",
    "xvidix",
    "Alex Beregszaszi",
    ""
};

#define UNUSED(x) ((void)(x)) /* Removes warning about unused arguments */

/* X11 related variables */
static int X_already_started = 0;

/* Colorkey handling */
static XGCValues mGCV;
static uint32_t	fgColor;
static uint32_t bgColor;
static vidix_grkey_t gr_key;

/* VIDIX related */
static char *vidix_name;
static vo_tune_info_t vtune;

/* Image parameters */
static uint32_t image_width;
static uint32_t image_height;
static uint32_t image_format;
static uint32_t image_depth;

/* Window parameters */
static uint32_t window_x, window_y;
static uint32_t window_width, window_height;

/* used by XGetGeometry & XTranslateCoordinates for moving/resizing window */
static Window mRoot;
static uint32_t drwX, drwY, drwWidth, drwHeight, drwBorderWidth,
    drwDepth, drwcX, drwcY, dwidth, dheight;

static void set_window(int force_update,const vo_tune_info_t *info)
{
    XGetGeometry(mDisplay, vo_window, &mRoot, &drwX, &drwY, &drwWidth,
	&drwHeight, &drwBorderWidth, &drwDepth);
    drwX = drwY = 0;
    XTranslateCoordinates(mDisplay, vo_window, mRoot, 0, 0,
	&drwcX, &drwcY, &mRoot);

    aspect(&dwidth,&dheight,A_NOZOOM);
    if (!vo_fs)
	mp_msg(MSGT_VO, MSGL_V, "[xvidix] dcx: %d dcy: %d dx: %d dy: %d dw: %d dh: %d\n",
	    drwcX, drwcY, drwX, drwY, drwWidth, drwHeight);

    /* following stuff copied from vo_xmga.c */
#if X11_FULLSCREEN
    if (vo_fs)
    {
        aspect(&dwidth,&dheight,A_ZOOM);
	drwX = (vo_screenwidth - (dwidth > vo_screenwidth ? vo_screenwidth : dwidth)) / 2;
	drwcX += drwX;
	drwY = (vo_screenheight - (dheight > vo_screenheight ? vo_screenheight : dheight)) / 2;
	drwcY += drwY;
	drwWidth = (dwidth > vo_screenwidth ? vo_screenwidth : dwidth);
	drwHeight = (dheight > vo_screenheight ? vo_screenheight : dheight);
	mp_msg(MSGT_VO, MSGL_V, "[xvidix-fs] dcx: %d dcy: %d dx: %d dy: %d dw: %d dh: %d\n",
	    drwcX, drwcY, drwX, drwY, drwWidth, drwHeight);
    }
#endif

#ifdef HAVE_XINERAMA
    if (XineramaIsActive(mDisplay))
    {
	XineramaScreenInfo *screens;
	int num_screens;
	int i = 0;
	
	screens = XineramaQueryScreens(mDisplay, &num_screens);
	
	/* find the screen we are on */
	while ((screens[i].x_org <= drwcX) || (screens[i].y_org <= drwcY) ||
		(screens[i].x_org + screens[i].width >= drwcX) ||
		(screens[i].y_org + screens[i].height >= drwcY))
	    i++;

	/* set drwcX and drwcY to the right values */
	drwcX = drwcX - screens[i].x_org;
	drwcY = drwcY - screens[i].y_org;
	XFree(screens);
    }
#endif

    /* set new values in VIDIX */
    if (force_update || (window_x != drwcX) || (window_y != drwcY) ||
	(window_width != drwWidth) || (window_height != drwHeight))
    {
	window_x = drwcX;
	window_y = drwcY;
	window_width = drwWidth;
	window_height = drwHeight;

	/* FIXME: implement runtime resize/move if possible, this way is very ugly! */
	vidix_stop();
	if (vidix_init(image_width, image_height, window_x, window_y,
	    window_width, window_height, image_format, vo_depthonscreen,
	    vo_screenwidth, vo_screenheight,info) != 0)
        {
	    mp_msg(MSGT_VO, MSGL_FATAL, "Can't initialize VIDIX driver: %s: %s\n",
		vidix_name, strerror(errno));
	    vidix_term();
	    uninit();
    	    exit(1); /* !!! */
	}
	vidix_start();
    }
    
    mp_msg(MSGT_VO, MSGL_INFO, "[xvidix] window properties: pos: %dx%d, size: %dx%d\n",
	window_x, window_y, window_width, window_height);

    /* mDrawColorKey: */

    /* fill drawable with specified color */
    XSetBackground( mDisplay,vo_gc,bgColor );
    XClearWindow( mDisplay,vo_window );
    XSetForeground(mDisplay, vo_gc, fgColor);
    XFillRectangle(mDisplay, vo_window, vo_gc, drwX, drwY, drwWidth,
	(vo_fs ? drwHeight - 1 : drwHeight));
    /* flush, update drawable */
    XFlush(mDisplay);

    return;
}

/* connect to server, create and map window,
 * allocate colors and (shared) memory
 */
static uint32_t config(uint32_t width, uint32_t height, uint32_t d_width,
    uint32_t d_height, uint32_t flags, char *title, uint32_t format,const vo_tune_info_t *info)
{
    XVisualInfo vinfo;
    XSizeHints hint;
    XSetWindowAttributes xswa;
    unsigned long xswamask;
    XWindowAttributes attribs;
    int window_depth;

//    if (title)
//	free(title);
    title = "MPlayer VIDIX X11 Overlay";

    image_height = height;
    image_width = width;
    image_format = format;

    if (IMGFMT_IS_RGB(format))
    {
	image_depth = IMGFMT_RGB_DEPTH(format);
    }
    else
    if (IMGFMT_IS_BGR(format))
    {
	image_depth = IMGFMT_BGR_DEPTH(format);
    }
    else
    switch(format)
    {
	case IMGFMT_IYUV:
	case IMGFMT_I420:
	case IMGFMT_YV12:
	    image_depth = 12;
	    break;
	case IMGFMT_UYVY:
	case IMGFMT_YUY2:
	    image_depth = 16;
	    break;
	default:
	    image_depth = 16;
	    mp_msg(MSGT_VO, MSGL_FATAL, "Unknown image format: %s\n",
		vo_format_name(format));
	    break;
    }

    if (X_already_started)
        return(-1);
    if (!vo_init())
        return(-1);

    aspect_save_orig(width, height);
    aspect_save_prescale(d_width, d_height);
    aspect_save_screenres(vo_screenwidth, vo_screenheight);

    window_x = 0;
    window_y = 0;
    window_width = d_width;
    window_height = d_height;

    vo_fs = flags&0x01;
    if (vo_fs)
     { vo_old_width=d_width; vo_old_height=d_height; }

    X_already_started++;
    
    /* from xmga.c */
    bgColor = 0x0L;
    switch(vo_depthonscreen)
    {
	case 32:
	case 24:
	    fgColor = 0x00ff00ffL;
	    break;
	case 16:
	    fgColor = 0xf81fL;
	    break;
	case 15:
	    fgColor = 0x7c1fL;
	    break;
	default:
	    mp_msg(MSGT_VO, MSGL_ERR, "Sorry, this (%d) color depth is not supported\n",
		vo_depthonscreen);
    }

    aspect(&d_width, &d_height, A_NOZOOM);

#ifdef HAVE_NEW_GUI
if (vo_window == None)
{
#endif

#ifdef X11_FULLSCREEN
    if (vo_fs) /* fullscreen */
    {
        if (flags & 0x04)
        {
    	    aspect(&d_width, &d_height, A_ZOOM);
        }
    	else
    	{
	    d_width = vo_screenwidth;
	    d_height = vo_screenheight;
    	}
	window_width = vo_screenwidth;
	window_height = vo_screenheight;
    }
#endif
    dwidth = d_width;
    dheight = d_height;
    /* Make the window */
    XGetWindowAttributes(mDisplay, DefaultRootWindow(mDisplay), &attribs);

    /* from vo_x11 */
    window_depth = attribs.depth;
    if ((window_depth != 15) && (window_depth != 16) && (window_depth != 24)
	&& (window_depth != 32))
        window_depth = 24;
    XMatchVisualInfo(mDisplay, mScreen, window_depth, TrueColor, &vinfo);

    xswa.background_pixel = vo_fs ? BlackPixel(mDisplay, mScreen) : fgColor;
    xswa.border_pixel     = 0;
    xswa.colormap         = XCreateColormap(mDisplay, RootWindow(mDisplay, mScreen),
					    vinfo.visual, AllocNone);
    xswa.event_mask = StructureNotifyMask | ExposureMask | KeyPressMask | 
       ((WinID==0)?0:(ButtonPressMask | ButtonReleaseMask | PointerMotionMask));
    xswamask = CWBackPixel | CWBorderPixel | CWColormap | CWEventMask;

    if (WinID >= 0)
    {
	vo_window = WinID ? ((Window)WinID) : RootWindow(mDisplay, mScreen);
	XUnmapWindow(mDisplay, vo_window);
	XChangeWindowAttributes(mDisplay, vo_window, xswamask, &xswa);
    }
    else
	vo_window = XCreateWindow(mDisplay, RootWindow(mDisplay, mScreen),
	    window_x, window_y, window_width, window_height, xswa.border_pixel,
	    vinfo.depth, InputOutput, vinfo.visual, xswamask, &xswa);

    vo_x11_classhint(mDisplay, vo_window, "xvidix");
    vo_hidecursor(mDisplay, vo_window);

#ifdef X11_FULLSCREEN
    if (vo_fs) /* fullscreen */
	vo_x11_decoration(mDisplay, vo_window, 0);
#endif

    XGetNormalHints(mDisplay, vo_window, &hint);
    hint.x = window_x;
    hint.y = window_y;
    hint.base_width = hint.width = window_width;
    hint.base_height = hint.height = window_height;
    hint.flags = USPosition | USSize;
    XSetNormalHints(mDisplay, vo_window, &hint);

    XStoreName(mDisplay, vo_window, title);
    /* Map window. */

    XMapWindow(mDisplay, vo_window);
#ifdef HAVE_XINERAMA
    vo_x11_xinerama_move(mDisplay, vo_window);
#endif

    vo_gc = XCreateGC(mDisplay, vo_window, GCForeground, &mGCV);

    XSelectInput( mDisplay,vo_window,StructureNotifyMask | KeyPressMask | ButtonPressMask | ButtonReleaseMask );
#ifdef HAVE_NEW_GUI
}
#endif

    mp_msg(MSGT_VO, MSGL_INFO, "[xvidix] image properties: %dx%d depth: %d\n",
	image_width, image_height, image_depth);

    if (vidix_grkey_support())
    {
	vidix_grkey_get(&gr_key);
	gr_key.key_op = KEYS_PUT;
	gr_key.ckey.op = CKEY_TRUE;
	gr_key.ckey.red = 255;
	gr_key.ckey.green = 0;
	gr_key.ckey.blue = 255;
	vidix_grkey_set(&gr_key);
    }

    set_window(1,info);
    if(info) memcpy(&vtune,info,sizeof(vo_tune_info_t));
    else     memset(&vtune,0,sizeof(vo_tune_info_t));
    XFlush(mDisplay);
    XSync(mDisplay, False);

    saver_off(mDisplay); /* turning off screen saver */

    return(0);
}

static const vo_info_t *get_info(void)
{
    return(&vo_info);
}

static void check_events(void)
{
    const int event = vo_x11_check_events(mDisplay);

    if ((event & VO_EVENT_RESIZE) || (event & VO_EVENT_EXPOSE))
	set_window(0,&vtune);

    return;
}

/* draw_osd, flip_page, draw_slice, draw_frame should be
   overwritten with vidix functions (vosub_vidix.c) */
static void draw_osd(void)
{
    mp_msg(MSGT_VO, MSGL_FATAL, "[xvidix] error: didn't used vidix draw_osd!\n");
    return;
}

static void flip_page(void)
{
    mp_msg(MSGT_VO, MSGL_FATAL, "[xvidix] error: didn't used vidix flip_page!\n");
    return;
}

static uint32_t draw_slice(uint8_t *src[], int stride[],
    int w, int h, int x, int y)
{
    UNUSED(src);
    UNUSED(stride);
    UNUSED(w);
    UNUSED(h);
    UNUSED(x);
    UNUSED(y);
    mp_msg(MSGT_VO, MSGL_FATAL, "[xvidix] error: didn't used vidix draw_slice!\n");
    return(0);
}

static uint32_t draw_frame(uint8_t *src[])
{
    UNUSED(src);
    mp_msg(MSGT_VO, MSGL_FATAL, "[xvidix] error: didn't used vidix draw_frame!\n");
    return(0);
}

static uint32_t query_format(uint32_t format)
{
  return(vidix_query_fourcc(format));
}

static void uninit(void)
{
    vidix_term();

    saver_on(mDisplay); /* screen saver back on */
    vo_x11_uninit(mDisplay, vo_window);
}

static uint32_t preinit(const char *arg)
{
    if (arg)
        vidix_name = strdup(arg);
    else
    {
	mp_msg(MSGT_VO, MSGL_INFO, "No vidix driver name provided, probing available ones!\n");
	vidix_name = NULL;
    }

    if (vidix_preinit(vidix_name, &video_out_xvidix) != 0)
	return(1);

    return(0);
}

static uint32_t control(uint32_t request, void *data, ...)
{
  switch (request) {
  case VOCTRL_QUERY_FORMAT:
    return query_format(*((uint32_t*)data));
  case VOCTRL_GUISUPPORT:
    return VO_TRUE;
  case VOCTRL_FULLSCREEN:
    vo_x11_fullscreen();
    return VO_TRUE;
  }
  return VO_NOTIMPL;
}


//#define SHOW_TIME

/*
 *    video_out_xmga.c
 *
 *      Copyright (C) Zoltan Ponekker - Jan 2001
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
#include "mp_msg.h"

#include "video_out.h"
#include "video_out_internal.h"

LIBVO_EXTERN( xmga )

#include <sys/ioctl.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>

#include "drivers/mga_vid.h"

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <errno.h>

#ifdef HAVE_XINERAMA
#include <X11/extensions/Xinerama.h>
#endif

#include "x11_common.h"
#include "sub.h"
#include "aspect.h"

#ifdef SHOW_TIME
#include "../linux/timer.h"
static unsigned int timer=0;
static unsigned int timerd=0;
#endif

#ifdef HAVE_NEW_GUI
#include "../Gui/interface.h"
#endif

static vo_info_t vo_info =
{
 "X11 (Matrox G200/G4x0/G550 overlay in window using /dev/mga_vid)",
 "xmga",
 "Zoltan Ponekker <pontscho@makacs.poliod.hu>",
 ""
};

static XGCValues              wGCV;

static uint32_t               mDepth;
static XWindowAttributes      attribs;
static uint32_t               fgColor;

static uint32_t               mvHeight;
static uint32_t               mvWidth;

static Window                 mRoot;
static uint32_t               drwX,drwY,drwWidth,drwHeight,drwBorderWidth,drwDepth;
static uint32_t               drwcX,drwcY,dwidth,dheight;

static XSetWindowAttributes   xWAttribs;

static int inited=0;

#define VO_XMGA
#include "mga_common.c"
#undef  VO_XMGA

static void mDrawColorKey( void )
{
 XSetBackground( mDisplay,vo_gc,0 );
 XClearWindow( mDisplay,vo_window );
 XSetForeground( mDisplay,vo_gc,fgColor );
 XFillRectangle( mDisplay,vo_window,vo_gc,drwX,drwY,drwWidth,(vo_fs?drwHeight - 1:drwHeight) );
 XFlush( mDisplay );
}

static void set_window( void ){

	 if ( WinID )
	  {
           XGetGeometry( mDisplay,vo_window,&mRoot,&drwX,&drwY,&drwWidth,&drwHeight,&drwBorderWidth,&drwDepth );
           mp_msg(MSGT_VO,MSGL_V,"[xmga] x: %d y: %d w: %d h: %d\n",drwX,drwY,drwWidth,drwHeight );
           drwX=0; drwY=0;
           XTranslateCoordinates( mDisplay,vo_window,mRoot,0,0,&drwcX,&drwcY,&mRoot );
           mp_msg(MSGT_VO,MSGL_V,"[xmga] dcx: %d dcy: %d dx: %d dy: %d dw: %d dh: %d\n",drwcX,drwcY,drwX,drwY,drwWidth,drwHeight );
	  }
	  else { drwX=drwcX=vo_dx; drwY=drwcY=vo_dy; drwWidth=vo_dwidth; drwHeight=vo_dheight; }

         aspect(&dwidth,&dheight,A_NOZOOM);
         if ( vo_fs )
          {
           aspect(&dwidth,&dheight,A_ZOOM);
           drwX=( vo_screenwidth - (dwidth > vo_screenwidth?vo_screenwidth:dwidth) ) / 2;
           drwcX+=drwX;
           drwY=( vo_screenheight - (dheight > vo_screenheight?vo_screenheight:dheight) ) / 2;
           drwcY+=drwY;
           drwWidth=(dwidth > vo_screenwidth?vo_screenwidth:dwidth);
           drwHeight=(dheight > vo_screenheight?vo_screenheight:dheight);
           mp_msg(MSGT_VO,MSGL_V,"[xmga-fs] dcx: %d dcy: %d dx: %d dy: %d dw: %d dh: %d\n",drwcX,drwcY,drwX,drwY,drwWidth,drwHeight );
          }
	 vo_dwidth=drwWidth; vo_dheight=drwHeight;

#ifdef HAVE_XINERAMA
		 if(XineramaIsActive(mDisplay))
		 {
		 	XineramaScreenInfo *screens;
		 	int num_screens;
		 	int i;

		 	screens = XineramaQueryScreens(mDisplay,&num_screens);

		 	/* find the screen we are on */
		 	i = 0;
		 	while(!(screens[i].x_org <= drwcX && screens[i].y_org <= drwcY &&
		 	       screens[i].x_org + screens[i].width >= drwcX &&
		 	       screens[i].y_org + screens[i].height >= drwcY ))
		 	{
		 		i++;
		 	}

		 	/* set drwcX and drwcY to the right values */
		 	drwcX = drwcX - screens[i].x_org;
		 	drwcY = drwcY - screens[i].y_org;
		 	XFree(screens);
		 }

#endif

         mDrawColorKey();

         mga_vid_config.x_org=drwcX;
         mga_vid_config.y_org=drwcY;
         mga_vid_config.dest_width=drwWidth;
         mga_vid_config.dest_height=drwHeight;
	 if ( vo_panscan > 0.0f && vo_fs )
	  {
	   drwX-=vo_panscan_x>>1;
	   drwY-=vo_panscan_y>>1;
	   drwWidth+=vo_panscan_x;
	   drwHeight+=vo_panscan_y;
  
	   mga_vid_config.x_org-=vo_panscan_x>>1;
	   mga_vid_config.y_org-=vo_panscan_y>>1;
           mga_vid_config.dest_width=drwWidth;
           mga_vid_config.dest_height=drwHeight;
	   mDrawColorKey();
	   if ( ioctl( f,MGA_VID_CONFIG,&mga_vid_config ) ) mp_msg(MSGT_VO,MSGL_WARN,"Error in mga_vid_config ioctl (wrong mga_vid.o version?)" );
	  }
}

static void check_events(void)
{
 int e=vo_x11_check_events(mDisplay);
 if ( !(e&VO_EVENT_RESIZE) && !(e&VO_EVENT_EXPOSE) ) return;
 set_window();
 mDrawColorKey();
 if ( ioctl( f,MGA_VID_CONFIG,&mga_vid_config ) ) mp_msg(MSGT_VO,MSGL_WARN,"Error in mga_vid_config ioctl (wrong mga_vid.o version?)" );
}

static void draw_osd(void)
{ vo_draw_text(mga_vid_config.src_width,mga_vid_config.src_height,draw_alpha);}

static void flip_page(void){
#ifdef SHOW_TIME
    unsigned int t;
    t=GetTimer();
    mp_msg(MSGT_VO,MSGL_STATUS,"  [timer: %08X  diff: %6d  dd: %6d ]  \n",t,t-timer,(t-timer)-timerd);
    timerd=t-timer;
    timer=t;
#endif

   vo_mga_flip_page();
}

static uint32_t config( uint32_t width, uint32_t height, uint32_t d_width, uint32_t d_height, uint32_t flags, char *title, uint32_t format)
{
 char                 * mTitle=(title == NULL) ? "XMGA render" : title;
 XVisualInfo            vinfo;

 unsigned long          xswamask;

 if (!vo_init()) return -1;

 width+=width&1;

 switch(format)
  {
   case IMGFMT_YV12:
	height+=height&1;
        mga_vid_config.format=MGA_VID_FORMAT_IYUV;
        mga_vid_config.frame_size=( ( width + 31 ) & ~31 ) * height + ( ( ( width + 31 ) & ~31 ) * height ) / 2;
        break;
   case IMGFMT_I420:
   case IMGFMT_IYUV:
	height+=height&1;
        mga_vid_config.format=MGA_VID_FORMAT_YV12;
        mga_vid_config.frame_size=( ( width + 31 ) & ~31 ) * height + ( ( ( width + 31 ) & ~31 ) * height ) / 2;
        break;
   case IMGFMT_YUY2:
        mga_vid_config.format=MGA_VID_FORMAT_YUY2;
        mga_vid_config.frame_size=( ( width + 31 ) & ~31 ) * height * 2;
        break;
   case IMGFMT_UYVY:
        mga_vid_config.format=MGA_VID_FORMAT_UYVY;
        mga_vid_config.frame_size=( ( width + 31 ) & ~31 ) * height * 2;
        break;
   default:
       mp_msg(MSGT_VO,MSGL_ERR,"mga: invalid output format %0X\n",format);
       return -1;
  }

 aspect_save_orig(width,height);
 aspect_save_prescale(d_width,d_height);
 aspect_save_screenres(vo_screenwidth,vo_screenheight);

 mvWidth=width; mvHeight=height;

 vo_panscan_x=vo_panscan_y=vo_panscan_amount=0;

 vo_dx=( vo_screenwidth - d_width ) / 2; vo_dy=( vo_screenheight - d_height ) / 2;
 vo_dwidth=d_width; vo_dheight=d_height;
 vo_mouse_autohide=1;

 switch ( vo_depthonscreen )
  {
   case 32:
   case 24: fgColor=0x00ff00ffL; break;
   case 16: fgColor=0xf81fL; break;
   case 15: fgColor=0x7c1fL; break;
   default: mp_msg(MSGT_VO,MSGL_ERR,"Sorry, this (%d) color depth not supported.\n",vo_depthonscreen ); return -1;
  }

  inited=1;

  aspect(&vo_dwidth,&vo_dheight,A_NOZOOM);

#ifdef HAVE_NEW_GUI
  if(use_gui)
    guiGetEvent( guiSetShVideo,0 ); // the GUI will set up / resize the window
  else
#endif
  {
#ifdef X11_FULLSCREEN
   if ( flags&1 ) aspect(&dwidth,&dheight,A_ZOOM);
#endif

   XGetWindowAttributes( mDisplay,mRootWin,&attribs );
   mDepth=attribs.depth;
   if ( mDepth != 15 && mDepth != 16 && mDepth != 24 && mDepth != 32 ) mDepth=24;
   XMatchVisualInfo( mDisplay,mScreen,mDepth,TrueColor,&vinfo );
   xWAttribs.colormap=XCreateColormap( mDisplay,mRootWin,vinfo.visual,AllocNone );
   xWAttribs.background_pixel=0;
   xWAttribs.border_pixel=0;
   xWAttribs.event_mask=StructureNotifyMask | ExposureMask | KeyPressMask |
       ((WinID==0)?0:(ButtonPressMask | ButtonReleaseMask | PointerMotionMask | PropertyChangeMask));
   xswamask=CWBackPixel | CWBorderPixel | CWColormap | CWEventMask;

    if ( WinID>=0 ){
    
      vo_window = WinID ? ((Window)WinID) : mRootWin;
      if ( WinID )
       {
        XUnmapWindow( mDisplay,vo_window );
        XChangeWindowAttributes( mDisplay,vo_window,xswamask,&xWAttribs);
        vo_x11_selectinput_witherr( mDisplay,vo_window,StructureNotifyMask | KeyPressMask | PropertyChangeMask | PointerMotionMask | ButtonPressMask | ButtonReleaseMask | ExposureMask );
       } else XSelectInput( mDisplay,vo_window,ExposureMask );
       
    } else 
     {
      vo_window=XCreateWindow( mDisplay,mRootWin,
         vo_dx,vo_dy,
         vo_dwidth,vo_dheight,
         xWAttribs.border_pixel,
         mDepth,
         InputOutput,
         vinfo.visual,xswamask,&xWAttribs );
     
      vo_x11_classhint( mDisplay,vo_window,"xmga" );
      vo_hidecursor(mDisplay,vo_window);
      vo_x11_sizehint( vo_dx,vo_dy,vo_dwidth,vo_dheight,0 );

      XStoreName( mDisplay,vo_window,mTitle );
      XMapWindow( mDisplay,vo_window );
 
      if ( flags&1 ) vo_x11_fullscreen();

#ifdef HAVE_XINERAMA
      vo_x11_xinerama_move(mDisplay,vo_window);
#endif
     }
    vo_gc=XCreateGC( mDisplay,vo_window,GCForeground,&wGCV );
  }

 if ( ( flags&1 )&&( !WinID ) ) { vo_dx=0; vo_dy=0; vo_dwidth=vo_screenwidth; vo_dheight=vo_screenheight; vo_fs=1; }

 panscan_calc();
 
 set_window();

 saver_off(mDisplay);

 XFlush( mDisplay );
 XSync( mDisplay,False );

 mga_vid_config.src_width=width;
 mga_vid_config.src_height=height;

 mga_vid_config.colkey_on=1;
 mga_vid_config.colkey_red=255;
 mga_vid_config.colkey_green=0;
 mga_vid_config.colkey_blue=255;
 
 mga_vid_config.version=MGA_VID_VERSION;

 return mga_init();
}

static const vo_info_t* get_info( void )
{ return &vo_info; }


static void
uninit(void)
{
 if(!inited) return;
 inited=0;
 mga_uninit();
 saver_on(mDisplay);
 vo_x11_uninit();
 mp_msg(MSGT_VO,MSGL_V,"vo: uninit!\n");
}


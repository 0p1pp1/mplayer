/* vo_xv.c, X11 Xv interface */

// Number of buffers _FOR_DOUBLEBUFFERING_MODE_
// Use option -double to enable double buffering! (default: single buffer)
#define NUM_BUFFERS 3

/*
Buffer allocation:

-nodr:
  1: TEMP
  2: 2*TEMP

-dr:
  1: TEMP
  3: 2*STATIC+TEMP
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"
#include "mp_msg.h"
#include "video_out.h"
#include "video_out_internal.h"


#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <errno.h>

#include "x11_common.h"

#include "fastmemcpy.h"
#include "sub.h"
#include "aspect.h"

#ifdef HAVE_NEW_GUI
#include "../Gui/interface.h"
#endif

static vo_info_t info =
{
        "X11/Xv",
        "xv",
        "Gerd Knorr <kraxel@goldbach.in-berlin.de> and others",
        ""
};

LIBVO_EXTERN(xv)

#include <X11/extensions/Xv.h>
#include <X11/extensions/Xvlib.h>
// FIXME: dynamically allocate this stuff
static void allocate_xvimage(int);
static unsigned int ver,rel,req,ev,err;
static unsigned int formats, adaptors,i,xv_port,xv_format;
static XvAdaptorInfo        *ai = NULL;
static XvImageFormatValues  *fo;

static int current_buf=0;
static int current_ip_buf=0;
static int num_buffers=1; // default
static XvImage* xvimage[NUM_BUFFERS];

#ifdef HAVE_SHM
#include <sys/ipc.h>
#include <sys/shm.h>
#include <X11/extensions/XShm.h>

/* since it doesn't seem to be defined on some platforms */
int XShmGetEventBase(Display*);

static XShmSegmentInfo Shminfo[NUM_BUFFERS];
static int Shmem_Flag;
#endif

static uint32_t image_width;
static uint32_t image_height;
static uint32_t image_format;
static int flip_flag;

static Window                 mRoot;
static uint32_t               drwX,drwY,drwBorderWidth,drwDepth;
static uint32_t               dwidth,dheight;

static void (*draw_alpha_fnc)(int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride);

static void draw_alpha_yv12(int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride){
   x0+=image_width*(vo_panscan_x>>1)/(vo_dwidth+vo_panscan_x);
   vo_draw_alpha_yv12(w,h,src,srca,stride,
       xvimage[current_buf]->data+xvimage[current_buf]->offsets[0]+
       xvimage[current_buf]->pitches[0]*y0+x0,xvimage[current_buf]->pitches[0]);
}

static void draw_alpha_yuy2(int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride){
   x0+=image_width*(vo_panscan_x>>1)/(vo_dwidth+vo_panscan_x);
   vo_draw_alpha_yuy2(w,h,src,srca,stride,
       xvimage[current_buf]->data+xvimage[current_buf]->offsets[0]+
       xvimage[current_buf]->pitches[0]*y0+2*x0,xvimage[current_buf]->pitches[0]);
}

static void draw_alpha_uyvy(int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride){
   x0+=image_width*(vo_panscan_x>>1)/(vo_dwidth+vo_panscan_x);
   vo_draw_alpha_yuy2(w,h,src,srca,stride,
       xvimage[current_buf]->data+xvimage[current_buf]->offsets[0]+
       xvimage[current_buf]->pitches[0]*y0+2*x0+1,xvimage[current_buf]->pitches[0]);
}

static void draw_alpha_null(int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride){
}


static void deallocate_xvimage(int foo);

/*
 * connect to server, create and map window,
 * allocate colors and (shared) memory
 */
static uint32_t config(uint32_t width, uint32_t height, uint32_t d_width, uint32_t d_height, uint32_t flags, char *title, uint32_t format)
{
// int screen;
 char *hello = (title == NULL) ? "Xv render" : title;
// char *name = ":0.0";
 XSizeHints hint;
 XVisualInfo vinfo;
 XGCValues xgcv;
 XSetWindowAttributes xswa;
 XWindowAttributes attribs;
 unsigned long xswamask;
 int depth;
#ifdef HAVE_XF86VM
 int vm=0;
 unsigned int modeline_width, modeline_height;
 static uint32_t vm_width;
 static uint32_t vm_height;
#endif

 panscan_init();

 aspect_save_orig(width,height);
 aspect_save_prescale(d_width,d_height);

 image_height = height;
 image_width = width;
 image_format=format;
 
 vo_mouse_autohide=1;

 vo_dx=( vo_screenwidth - d_width ) / 2; vo_dy=( vo_screenheight - d_height ) / 2;
 geometry(&vo_dx, &vo_dy, &d_width, &d_height, vo_screenwidth, vo_screenheight);
 vo_dwidth=d_width; vo_dheight=d_height;
     
#ifdef HAVE_XF86VM
 if( flags&0x02 ) vm = 1;
#endif
 flip_flag=flags&8;
 num_buffers=vo_doublebuffering?(vo_directrendering?NUM_BUFFERS:2):1;

   /* check image formats */
     xv_format=0;
     for(i = 0; i < formats; i++){
       mp_msg(MSGT_VO,MSGL_V,"Xvideo image format: 0x%x (%4.4s) %s\n", fo[i].id,(char*)&fo[i].id, (fo[i].format == XvPacked) ? "packed" : "planar");
       if (fo[i].id == format) xv_format = fo[i].id;
     }
     if (!xv_format) return -1;
 
 aspect_save_screenres(vo_screenwidth,vo_screenheight);

#ifdef HAVE_NEW_GUI
  if(use_gui)
    guiGetEvent( guiSetShVideo,0 ); // let the GUI to setup/resize our window
  else
#endif
  {
   hint.x = vo_dx;
   hint.y = vo_dy;
   aspect(&d_width,&d_height,A_NOZOOM);
   hint.width = d_width;
   hint.height = d_height;
#ifdef HAVE_XF86VM
    if ( vm )
      {
	if ((d_width==0) && (d_height==0))
	  { vm_width=image_width; vm_height=image_height; }
	else
	  { vm_width=d_width; vm_height=d_height; }
	vo_vm_switch(vm_width, vm_height,&modeline_width, &modeline_height);
	hint.x=(vo_screenwidth-modeline_width)/2;
	hint.y=(vo_screenheight-modeline_height)/2;
	hint.width=modeline_width;
	hint.height=modeline_height;
	aspect_save_screenres(modeline_width,modeline_height);
      }
    else
#endif
   if ( vo_fs )
    {
#ifdef X11_FULLSCREEN
     /* this code replaces X11_FULLSCREEN hack in mplayer.c
      * aspect() is available through aspect.h for all vos.
      * besides zooming should only be done with -zoom,
      * but I leave the old -fs behaviour so users don't get
      * irritated for now (and send lots o' mails ;) ::atmos
      */

     aspect(&d_width,&d_height,A_ZOOM);
#endif

    }
//   dwidth=d_width; dheight=d_height; //XXX: what are the copy vars used for?
   vo_dwidth=d_width; vo_dheight=d_height;
   hint.flags = PPosition | PSize /* | PBaseSize */;
   hint.base_width = hint.width; hint.base_height = hint.height;
   XGetWindowAttributes(mDisplay, DefaultRootWindow(mDisplay), &attribs);
   depth=attribs.depth;
   if (depth != 15 && depth != 16 && depth != 24 && depth != 32) depth = 24;
   XMatchVisualInfo(mDisplay, mScreen, depth, TrueColor, &vinfo);

   xswa.background_pixel = 0;
   xswa.border_pixel     = 0;
   xswamask = CWBackPixel | CWBorderPixel;

    if ( WinID>=0 ){
      vo_window = WinID ? ((Window)WinID) : mRootWin;
      if ( WinID ) 
       {
        XUnmapWindow( mDisplay,vo_window );
        XChangeWindowAttributes( mDisplay,vo_window,xswamask,&xswa );
	vo_x11_selectinput_witherr( mDisplay,vo_window,StructureNotifyMask | KeyPressMask | PropertyChangeMask | PointerMotionMask | ButtonPressMask | ButtonReleaseMask | ExposureMask );
        XMapWindow( mDisplay,vo_window );
       } else { drwX=vo_dx; drwY=vo_dy; }
    } else 
    if ( vo_window == None ){
        vo_window = XCreateWindow(mDisplay, mRootWin,
          hint.x, hint.y, hint.width, hint.height,
          0, depth,CopyFromParent,vinfo.visual,xswamask,&xswa);

        vo_x11_classhint( mDisplay,vo_window,"xv" );
        vo_hidecursor(mDisplay,vo_window);

        vo_x11_selectinput_witherr(mDisplay, vo_window, StructureNotifyMask | KeyPressMask | PropertyChangeMask |
	((WinID==0) ? 0 : (PointerMotionMask
		| ButtonPressMask | ButtonReleaseMask
	  )));
        XSetStandardProperties(mDisplay, vo_window, hello, hello, None, NULL, 0, &hint);
        XSetWMNormalHints( mDisplay,vo_window,&hint );
	XMapWindow(mDisplay, vo_window);
	if ( flags&1 ) vo_x11_fullscreen();
	else {
#ifdef HAVE_XINERAMA
	    vo_x11_xinerama_move(mDisplay,vo_window);
#endif
		vo_x11_sizehint( hint.x, hint.y, hint.width, hint.height,0 );
	}
    } else  {
	// vo_fs set means we were already at fullscreen
	vo_x11_sizehint( hint.x, hint.y, hint.width, hint.height,0 );
	if ( !vo_fs ) XMoveResizeWindow( mDisplay,vo_window,hint.x,hint.y,hint.width,hint.height );
	if ( flags&1 && !vo_fs ) vo_x11_fullscreen(); // handle -fs on non-first file
    }
    
//    vo_x11_sizehint( hint.x, hint.y, hint.width, hint.height,0 );   
    
    if ( vo_gc != None ) XFreeGC( mDisplay,vo_gc );
    vo_gc = XCreateGC(mDisplay, vo_window, 0L, &xgcv);
    XFlush(mDisplay);
    XSync(mDisplay, False);
#ifdef HAVE_XF86VM
    if ( vm )
     {
      /* Grab the mouse pointer in our window */
      if(vo_grabpointer)
      XGrabPointer(mDisplay, vo_window, True, 0,
                   GrabModeAsync, GrabModeAsync,
                   vo_window, None, CurrentTime);
      XSetInputFocus(mDisplay, vo_window, RevertToNone, CurrentTime);
     }
#endif
  }

     mp_msg(MSGT_VO,MSGL_V, "using Xvideo port %d for hw scaling\n",xv_port );
       
       switch (xv_format){
	case IMGFMT_YV12:  
	case IMGFMT_I420:
        case IMGFMT_IYUV: draw_alpha_fnc=draw_alpha_yv12; break;
	case IMGFMT_YUY2:
	case IMGFMT_YVYU: draw_alpha_fnc=draw_alpha_yuy2; break;	
	case IMGFMT_UYVY: draw_alpha_fnc=draw_alpha_uyvy; break;
	default:   	  draw_alpha_fnc=draw_alpha_null;
       }

      if ( vo_config_count )
        for(current_buf=0;current_buf<num_buffers;++current_buf)
         deallocate_xvimage(current_buf);

      for(current_buf=0;current_buf<num_buffers;++current_buf)
       allocate_xvimage(current_buf);

     current_buf=0;
     current_ip_buf=0;

#if 0
     set_gamma_correction();
#endif

     aspect(&vo_dwidth,&vo_dheight,A_NOZOOM);
     if ( (( flags&1 )&&( WinID <= 0 )) || vo_fs )
      {
       aspect(&vo_dwidth,&vo_dheight,A_ZOOM);
       drwX=( vo_screenwidth - (vo_dwidth > vo_screenwidth?vo_screenwidth:vo_dwidth) ) / 2;
       drwY=( vo_screenheight - (vo_dheight > vo_screenheight?vo_screenheight:vo_dheight) ) / 2;
       vo_dwidth=(vo_dwidth > vo_screenwidth?vo_screenwidth:vo_dwidth);
       vo_dheight=(vo_dheight > vo_screenheight?vo_screenheight:vo_dheight);
       mp_msg(MSGT_VO,MSGL_V, "[xv-fs] dx: %d dy: %d dw: %d dh: %d\n",drwX,drwY,vo_dwidth,vo_dheight );
      }

     panscan_calc();

     XClearWindow(mDisplay, vo_window);
#if 0
#ifdef HAVE_SHM
     if ( Shmem_Flag )
     {
	 XvShmPutImage(mDisplay, xv_port, vo_window, vo_gc, xvimage[current_buf], 0, 0,  image_width, image_height, drwX, drwY, 1, 1, False);
	 XvShmPutImage(mDisplay, xv_port, vo_window, vo_gc, xvimage[current_buf], 0, 0,  image_width, image_height, drwX,drwY,vo_dwidth,(vo_fs?vo_dheight - 1:vo_dheight), False);
     }
     else
#endif
     {
	 XvPutImage(mDisplay, xv_port, vo_window, vo_gc, xvimage[current_buf], 0, 0,  image_width, image_height, drwX, drwY, 1, 1);
	 XvPutImage(mDisplay, xv_port, vo_window, vo_gc, xvimage[current_buf], 0, 0,  image_width, image_height, drwX,drwY,vo_dwidth,(vo_fs?vo_dheight - 1:vo_dheight));
     }
#endif
     
     mp_msg(MSGT_VO,MSGL_V, "[xv] dx: %d dy: %d dw: %d dh: %d\n",drwX,drwY,vo_dwidth,vo_dheight );

     saver_off(mDisplay);  // turning off screen saver
     return 0;
}

static void allocate_xvimage(int foo)
{
 /*
  * allocate XvImages.  FIXME: no error checking, without
  * mit-shm this will bomb... trzing to fix ::atmos
  */
#ifdef HAVE_SHM
 if ( mLocalDisplay && XShmQueryExtension( mDisplay ) ) Shmem_Flag = 1;
 else
  {
   Shmem_Flag = 0;
   mp_msg(MSGT_VO,MSGL_INFO, "Shared memory not supported\nReverting to normal Xv\n" );
  }
 if ( Shmem_Flag ) 
  {
   xvimage[foo] = (XvImage *) XvShmCreateImage(mDisplay, xv_port, xv_format, NULL, image_width, image_height, &Shminfo[foo]);

   Shminfo[foo].shmid    = shmget(IPC_PRIVATE, xvimage[foo]->data_size, IPC_CREAT | 0777);
   Shminfo[foo].shmaddr  = (char *) shmat(Shminfo[foo].shmid, 0, 0);
   Shminfo[foo].readOnly = False;

   xvimage[foo]->data = Shminfo[foo].shmaddr;
   XShmAttach(mDisplay, &Shminfo[foo]);
   XSync(mDisplay, False);
   shmctl(Shminfo[foo].shmid, IPC_RMID, 0);
  }
 else
#endif
  {
    xvimage[foo] = (XvImage *) XvCreateImage(mDisplay, xv_port, xv_format, NULL, image_width, image_height);
    xvimage[foo]->data = malloc(xvimage[foo]->data_size);
    XSync(mDisplay,False);
  }
 memset(xvimage[foo]->data,128,xvimage[foo]->data_size);
 return;
}

static void deallocate_xvimage(int foo)
{
#ifdef HAVE_SHM
 if ( Shmem_Flag )
  {
   XShmDetach( mDisplay,&Shminfo[foo] );
   shmdt( Shminfo[foo].shmaddr );
  }
 else
#endif
  {
   free(xvimage[foo]->data);
  }
 XFree(xvimage[foo]);
 
 XFlush( mDisplay );
 XSync(mDisplay, False);
 return;
}

static void check_events(void)
{
 int e=vo_x11_check_events(mDisplay);
 if(e&VO_EVENT_RESIZE)
  {
      if (vo_fs) {
        e |= VO_EVENT_EXPOSE;
        XClearWindow(mDisplay, vo_window);
        XFlush(mDisplay);
      }
      
   XGetGeometry( mDisplay,vo_window,&mRoot,&drwX,&drwY,&vo_dwidth,&vo_dheight,&drwBorderWidth,&drwDepth );
   drwX = drwY = 0;
   mp_msg(MSGT_VO,MSGL_V, "[xv] dx: %d dy: %d dw: %d dh: %d\n",drwX,drwY,vo_dwidth,vo_dheight );

   aspect(&dwidth,&dheight,A_NOZOOM);
   if ( vo_fs )
    {
     aspect(&dwidth,&dheight,A_ZOOM);
     drwX=( vo_screenwidth - (dwidth > vo_screenwidth?vo_screenwidth:dwidth) ) / 2;
     drwY=( vo_screenheight - (dheight > vo_screenheight?vo_screenheight:dheight) ) / 2;
     vo_dwidth=(dwidth > vo_screenwidth?vo_screenwidth:dwidth);
     vo_dheight=(dheight > vo_screenheight?vo_screenheight:dheight);
     mp_msg(MSGT_VO,MSGL_V, "[xv-fs] dx: %d dy: %d dw: %d dh: %d\n",drwX,drwY,vo_dwidth,vo_dheight );
    }
  }
 if ( e & VO_EVENT_EXPOSE )
  {
#ifdef HAVE_SHM
   if ( Shmem_Flag )
    {
     XvShmPutImage(mDisplay, xv_port, vo_window, vo_gc, xvimage[current_buf], 0, 0,  image_width, image_height, drwX, drwY, 1, 1, False);
     XvShmPutImage(mDisplay, xv_port, vo_window, vo_gc, xvimage[current_buf], 0, 0,  image_width, image_height, drwX,drwY,vo_dwidth,(vo_fs?vo_dheight - 1:vo_dheight), False);
    }
   else
#endif
    {
     XvPutImage(mDisplay, xv_port, vo_window, vo_gc, xvimage[current_buf], 0, 0,  image_width, image_height, drwX, drwY, 1, 1);
     XvPutImage(mDisplay, xv_port, vo_window, vo_gc, xvimage[current_buf], 0, 0,  image_width, image_height, drwX,drwY,vo_dwidth,(vo_fs?vo_dheight - 1:vo_dheight));
    }
  }
}

static void draw_osd(void)
{ vo_draw_text(image_width-image_width*vo_panscan_x/(vo_dwidth+vo_panscan_x),image_height,draw_alpha_fnc);}

static void flip_page(void)
{

#ifdef HAVE_SHM
 if ( Shmem_Flag )
  {
   XvShmPutImage(mDisplay, xv_port, vo_window, vo_gc, xvimage[current_buf],
         0, 0,  image_width, image_height,
         drwX-(vo_panscan_x>>1),drwY-(vo_panscan_y>>1),vo_dwidth+vo_panscan_x,(vo_fs?vo_dheight - 1:vo_dheight)+vo_panscan_y,
         False);
  }
 else
#endif
  {
   XvPutImage(mDisplay, xv_port, vo_window, vo_gc, xvimage[current_buf],
         0, 0,  image_width, image_height,
         drwX-(vo_panscan_x>>1),drwY-(vo_panscan_y>>1),vo_dwidth+vo_panscan_x,(vo_fs?vo_dheight - 1:vo_dheight)+vo_panscan_y);
  }
 if (num_buffers>1){
    current_buf=vo_directrendering?0:((current_buf+1)%num_buffers);
    XFlush(mDisplay);
 } else
    XSync(mDisplay, False);   
 return;
}

static uint32_t draw_slice(uint8_t *image[], int stride[], int w,int h,int x,int y)
{
 uint8_t *dst;

 dst = xvimage[current_buf]->data + xvimage[current_buf]->offsets[0] + 
       xvimage[current_buf]->pitches[0]*y + x;
 memcpy_pic(dst, image[0], w, h, xvimage[current_buf]->pitches[0], stride[0]);

 x/=2;y/=2;w/=2;h/=2;

 dst = xvimage[current_buf]->data + xvimage[current_buf]->offsets[1] + 
       xvimage[current_buf]->pitches[1]*y + x;
 if(image_format!=IMGFMT_YV12)
   memcpy_pic(dst, image[1], w, h, xvimage[current_buf]->pitches[1], stride[1]);
 else
   memcpy_pic(dst, image[2], w, h, xvimage[current_buf]->pitches[1], stride[2]);

 dst = xvimage[current_buf]->data + xvimage[current_buf]->offsets[2] + 
       xvimage[current_buf]->pitches[2]*y + x;
 if(image_format==IMGFMT_YV12)
   memcpy_pic(dst, image[1], w, h, xvimage[current_buf]->pitches[1], stride[1]);
 else
   memcpy_pic(dst, image[2], w, h, xvimage[current_buf]->pitches[1], stride[2]);

 return 0;
}

static uint32_t draw_frame(uint8_t *src[]){
    printf("draw_frame() called!!!!!!");
    return -1;
}

static uint32_t draw_image(mp_image_t *mpi){
    if(mpi->flags&MP_IMGFLAG_DIRECT){
	// direct rendering:
	current_buf=(int)(mpi->priv);	// hack!
	return VO_TRUE;
    }
    if(mpi->flags&MP_IMGFLAG_DRAW_CALLBACK) return VO_TRUE; // done
    if(mpi->flags&MP_IMGFLAG_PLANAR){
	draw_slice(mpi->planes,mpi->stride,mpi->w,mpi->h,0,0);
	return VO_TRUE;
    }
    if(mpi->flags&MP_IMGFLAG_YUV){
	// packed YUV:
	memcpy_pic(xvimage[current_buf]->data+xvimage[current_buf]->offsets[0],
	    mpi->planes[0],mpi->w*(mpi->bpp/8),mpi->h,
	    xvimage[current_buf]->pitches[0], mpi->stride[0]);
	return VO_TRUE;
    }
    return VO_FALSE; // not (yet) supported
}

static uint32_t get_image(mp_image_t *mpi){
    int buf=current_buf; // we shouldn't change current_buf unless we do DR!
    if(mpi->type==MP_IMGTYPE_STATIC && num_buffers>1) return VO_FALSE; // it is not static
    if(mpi->imgfmt!=image_format) return VO_FALSE; // needs conversion :(
//    if(mpi->flags&MP_IMGFLAG_READABLE) return VO_FALSE; // slow video ram
    if(mpi->flags&MP_IMGFLAG_READABLE &&
	(mpi->type==MP_IMGTYPE_IPB || mpi->type==MP_IMGTYPE_IP)){
	// reference (I/P) frame of IP or IPB:
	if(num_buffers<2) return VO_FALSE; // not enough
	current_ip_buf^=1;
	// for IPB with 2 buffers we can DR only one of the 2 P frames:
	if(mpi->type==MP_IMGTYPE_IPB && num_buffers<3 && current_ip_buf) return VO_FALSE;
	buf=current_ip_buf;
	if(mpi->type==MP_IMGTYPE_IPB) ++buf; // preserve space for B
    }
    if(mpi->height > xvimage[buf]->height) return VO_FALSE; //buffer to small
    if(mpi->width*(mpi->bpp/8) > xvimage[buf]->pitches[0]) return VO_FALSE; //buffer to small
    if( (mpi->flags&(MP_IMGFLAG_ACCEPT_STRIDE|MP_IMGFLAG_ACCEPT_WIDTH)) ||
	(mpi->width*(mpi->bpp/8)==xvimage[buf]->pitches[0]) ){
	current_buf=buf;
	mpi->planes[0]=xvimage[current_buf]->data+xvimage[current_buf]->offsets[0];
	mpi->stride[0]=xvimage[current_buf]->pitches[0];
	mpi->width=mpi->stride[0]/(mpi->bpp/8);
	if(mpi->flags&MP_IMGFLAG_PLANAR){
	    if(mpi->flags&MP_IMGFLAG_SWAPPED){
		// I420
		mpi->planes[1]=xvimage[current_buf]->data+xvimage[current_buf]->offsets[1];
		mpi->planes[2]=xvimage[current_buf]->data+xvimage[current_buf]->offsets[2];
		mpi->stride[1]=xvimage[current_buf]->pitches[1];
		mpi->stride[2]=xvimage[current_buf]->pitches[2];
	    } else {
	       // YV12
		mpi->planes[1]=xvimage[current_buf]->data+xvimage[current_buf]->offsets[2];
		mpi->planes[2]=xvimage[current_buf]->data+xvimage[current_buf]->offsets[1];
		mpi->stride[1]=xvimage[current_buf]->pitches[2];
		mpi->stride[2]=xvimage[current_buf]->pitches[1];
	    }
	}
       mpi->flags|=MP_IMGFLAG_DIRECT;
       mpi->priv=(void*)current_buf;
//	printf("mga: get_image() SUCCESS -> Direct Rendering ENABLED\n");
       return VO_TRUE;
    }
    return VO_FALSE;
}

static uint32_t query_format(uint32_t format)
{
    int flag=VFCAP_CSP_SUPPORTED|VFCAP_CSP_SUPPORTED_BY_HW|
	    VFCAP_HWSCALE_UP|VFCAP_HWSCALE_DOWN|VFCAP_OSD|VFCAP_ACCEPT_STRIDE; // FIXME! check for DOWN
   /* check image formats */
     for(i = 0; i < formats; i++){
       if (fo[i].id == format) return flag; //xv_format = fo[i].id;
     }
     return 0;
}

static void uninit(void) 
{
 int i;
 if ( !vo_config_count ) return;
 XvFreeAdaptorInfo(ai);
 ai = NULL;
 saver_on(mDisplay); // screen saver back on
 for( i=0;i<num_buffers;i++ ) deallocate_xvimage( i );
#ifdef HAVE_XF86VM
 vo_vm_close(mDisplay);
#endif
 vo_x11_uninit();
}

static uint32_t preinit(const char *arg)
{
    XvPortID xv_p;
    int busy_ports=0;
    
    if(arg) 
    {
	mp_msg(MSGT_VO,MSGL_ERR,"vo_xv: Unknown subdevice: %s\n",arg);
	return ENOSYS;
    }
    if (!vo_init()) return -1;

    xv_port = 0;
   /* check for Xvideo extension */
    if (Success != XvQueryExtension(mDisplay,&ver,&rel,&req,&ev,&err)){
	mp_msg(MSGT_VO,MSGL_ERR,"Sorry, Xv not supported by this X11 version/driver\n");
	mp_msg(MSGT_VO,MSGL_ERR,"******** Try with  -vo x11  or  -vo sdl  *********\n");
	return -1;
    }
    
   /* check for Xvideo support */
    if (Success != XvQueryAdaptors(mDisplay,DefaultRootWindow(mDisplay), &adaptors,&ai)){
	mp_msg(MSGT_VO,MSGL_ERR,"Xv: XvQueryAdaptors failed");
	return -1;
    }

   /* check adaptors */
    for (i = 0; i < adaptors && xv_port == 0; i++){
      if ((ai[i].type & XvInputMask) && (ai[i].type & XvImageMask)) {
	 for (xv_p = ai[i].base_id; xv_p < ai[i].base_id+ai[i].num_ports; ++xv_p)
	     if (!XvGrabPort(mDisplay, xv_p, CurrentTime)) {
		 xv_port = xv_p;
		 break;
	     } else {
		 mp_msg(MSGT_VO,MSGL_WARN,"Xv: could not grab port %i\n", (int)xv_p);
		 ++busy_ports;
	     }
      }
    }
    if(!xv_port){
      if(busy_ports)
	mp_msg(MSGT_VO,MSGL_ERR,"Could not find free Xvideo port - maybe another process is already using it.\n"
	    "Close all video applications, and try again. If that does not help,\n"
	    "see 'mplayer -vo help' for other (non-xv) video out drivers.\n");
      else
	mp_msg(MSGT_VO,MSGL_ERR,"It seems there is no Xvideo support for your video card available.\n"
	    "Run 'xvinfo' to verify its Xv support and read DOCS/en/video.html#xv!\n"
	    "See 'mplayer -vo help' for other (non-xv) video out drivers. Try -vo x11\n");
	return -1;
    }

    {
      int howmany, i;
      const XvAttribute * const attributes = XvQueryPortAttributes(mDisplay, xv_port, &howmany);
      
      for (i = 0; i < howmany && attributes; i++)
	if (!strcmp(attributes[i].name, "XV_AUTOPAINT_COLORKEY"))
	  {
	    const Atom autopaint = XInternAtom(mDisplay, "XV_AUTOPAINT_COLORKEY", False);
	    XvSetPortAttribute(mDisplay, xv_port, autopaint, 1);
	    break;
	  }
    }

    fo = XvListImageFormats(mDisplay, xv_port, (int*)&formats);

    return 0;
}

static uint32_t control(uint32_t request, void *data, ...)
{
  switch (request) {
  case VOCTRL_QUERY_FORMAT:
    return query_format(*((uint32_t*)data));
  case VOCTRL_GET_IMAGE:
    return get_image(data);
  case VOCTRL_DRAW_IMAGE:
    return draw_image(data);
  case VOCTRL_GUISUPPORT:
    return VO_TRUE;
  case VOCTRL_GET_PANSCAN:
    if ( !vo_config_count || !vo_fs ) return VO_FALSE;
    return VO_TRUE;
  case VOCTRL_FULLSCREEN:
    vo_x11_fullscreen();
  /* indended, fallthrough to update panscan on fullscreen/windowed switch */
  case VOCTRL_SET_PANSCAN:
   if ( ( vo_fs && ( vo_panscan != vo_panscan_amount ) ) || ( !vo_fs && vo_panscan_amount ) )
     {
      int old_y = vo_panscan_y;
      panscan_calc();
      
      if(old_y != vo_panscan_y)
       {
        XClearWindow(mDisplay, vo_window);
        XFlush(mDisplay);
       }
     }
    return VO_TRUE;
  case VOCTRL_SET_EQUALIZER:
  {
    va_list ap;
    int value;
    
    va_start(ap, data);
    value = va_arg(ap, int);
    va_end(ap);
    
    return(vo_xv_set_eq(xv_port, data, value));
  }
  case VOCTRL_GET_EQUALIZER:
  {
    va_list ap;
    int *value;
    
    va_start(ap, data);
    value = va_arg(ap, int*);
    va_end(ap);
    
    return(vo_xv_get_eq(xv_port, data, value));
  }
  }
  return VO_NOTIMPL;
}

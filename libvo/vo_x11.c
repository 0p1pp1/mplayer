
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

#include "config.h"
#include "video_out.h"
#include "video_out_internal.h"


#include <X11/Xlib.h>
#include <X11/Xutil.h>

#ifdef HAVE_XF86VM
#include <X11/extensions/xf86vmode.h>
#endif
#include <errno.h>

#include "x11_common.h"

#include "fastmemcpy.h"
#include "sub.h"

#include "../postproc/swscale.h"
#include "../postproc/swscale_internal.h" //FIXME
#include "../postproc/rgb2rgb.h"

#include "../mp_msg.h"

#ifdef HAVE_NEW_GUI
#include "../Gui/interface.h"
#include "../mplayer.h"
#endif

static vo_info_t info =
{
        "X11 ( XImage/Shm )",
        "x11",
        "Aaron Holtzman <aholtzma@ess.engr.uvic.ca>",
        ""
};

LIBVO_EXTERN( x11 )

/* private prototypes */
static void Display_Image ( XImage * myximage,unsigned char *ImageData );
static void (*draw_alpha_fnc)(int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride);

/* local data */
static unsigned char *ImageData;

/* X11 related variables */
static XImage *myximage = NULL;
static int depth,bpp,mode;
static XWindowAttributes attribs;

static int Flip_Flag;
static int zoomFlag;

#ifdef HAVE_SHM
#include <sys/ipc.h>
#include <sys/shm.h>
#include <X11/extensions/XShm.h>

static int Shmem_Flag;
//static int Quiet_Flag;  Here also what is this for. It's used but isn't inited ?
static XShmSegmentInfo Shminfo[1];
static int gXErrorFlag;
static int CompletionType=-1;

/* since it doesn't seem to be defined on some platforms */
int XShmGetEventBase( Display* );
#endif

static uint32_t image_width;
static uint32_t image_height;
static uint32_t in_format;
static uint32_t out_format=0;
static int srcW=-1;
static int srcH=-1;
static int aspect; // 1<<16 based fixed point aspect, so that the aspect stays correct during resizing

static int old_vo_dwidth=-1;
static int old_vo_dheight=-1;

static void check_events(){
  int ret = vo_x11_check_events(mDisplay);
  
   /* clear the old window */
  if ( (ret & VO_EVENT_RESIZE)||(ret & VO_EVENT_EXPOSE) )
  {
    XSetBackground(mDisplay, vo_gc, 0);
    XClearWindow(mDisplay, vo_window);
  }
}

static void draw_alpha_32(int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride){
   vo_draw_alpha_rgb32(w,h,src,srca,stride,ImageData+4*(y0*image_width+x0),4*image_width);
}

static void draw_alpha_24(int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride){
   vo_draw_alpha_rgb24(w,h,src,srca,stride,ImageData+3*(y0*image_width+x0),3*image_width);
}

static void draw_alpha_16(int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride){
   vo_draw_alpha_rgb16(w,h,src,srca,stride,ImageData+2*(y0*image_width+x0),2*image_width);
}

static void draw_alpha_15(int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride){
   vo_draw_alpha_rgb15(w,h,src,srca,stride,ImageData+2*(y0*image_width+x0),2*image_width);
}

static void draw_alpha_null(int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride){
}

static SwsContext *swsContext=NULL;
extern int sws_flags;

static XVisualInfo vinfo;

static void getMyXImage()
{
#ifdef HAVE_SHM
 if ( mLocalDisplay && XShmQueryExtension( mDisplay ) ) Shmem_Flag=1;
  else
   {
    Shmem_Flag=0;
    mp_msg(MSGT_VO,MSGL_WARN, "Shared memory not supported\nReverting to normal Xlib\n" );
   }
 if ( Shmem_Flag ) CompletionType=XShmGetEventBase( mDisplay ) + ShmCompletion;

 if ( Shmem_Flag )
  {
   myximage=XShmCreateImage( mDisplay,vinfo.visual,depth,ZPixmap,NULL,&Shminfo[0],image_width,image_height );
   if ( myximage == NULL )
    {
     if ( myximage != NULL ) XDestroyImage( myximage );
     mp_msg(MSGT_VO,MSGL_WARN,"Shared memory error,disabling ( Ximage error )\n" );
     goto shmemerror;
    }
   Shminfo[0].shmid=shmget( IPC_PRIVATE,
   myximage->bytes_per_line * myximage->height ,
   IPC_CREAT | 0777 );
   if ( Shminfo[0].shmid < 0 )
   {
    XDestroyImage( myximage );
    mp_msg(MSGT_VO,MSGL_V, "%s\n",strerror( errno ) );
    //perror( strerror( errno ) );
    mp_msg(MSGT_VO,MSGL_WARN,"Shared memory error,disabling ( seg id error )\n" );
    goto shmemerror;
   }
   Shminfo[0].shmaddr=( char * ) shmat( Shminfo[0].shmid,0,0 );

   if ( Shminfo[0].shmaddr == ( ( char * ) -1 ) )
   {
    XDestroyImage( myximage );
    if ( Shminfo[0].shmaddr != ( ( char * ) -1 ) ) shmdt( Shminfo[0].shmaddr );
    mp_msg(MSGT_VO,MSGL_WARN, "Shared memory error,disabling ( address error )\n" );
    goto shmemerror;
   }
   myximage->data=Shminfo[0].shmaddr;
   ImageData=( unsigned char * ) myximage->data;
   Shminfo[0].readOnly=False;
   XShmAttach( mDisplay,&Shminfo[0] );

   XSync( mDisplay,False );

   if ( gXErrorFlag )
   {
    XDestroyImage( myximage );
    shmdt( Shminfo[0].shmaddr );
    mp_msg(MSGT_VO,MSGL_WARN, "Shared memory error,disabling.\n" );
    gXErrorFlag=0;
    goto shmemerror;
   }
   else
    shmctl( Shminfo[0].shmid,IPC_RMID,0 );

   {
     static int firstTime=1;
     if ( firstTime){
       mp_msg(MSGT_VO,MSGL_V, "Sharing memory.\n" );
       firstTime=0;
     } 
   }
 }
 else
  {
   shmemerror:
   Shmem_Flag=0;
#endif
   myximage=XGetImage( mDisplay,vo_window,0,0,
   image_width,image_height,AllPlanes,ZPixmap );
   ImageData=myximage->data;
#ifdef HAVE_SHM
  }
#endif
}

static void freeMyXImage()
{
#ifdef HAVE_SHM
 if ( Shmem_Flag )
  {
   XShmDetach( mDisplay,&Shminfo[0] );
   XDestroyImage( myximage );
   shmdt( Shminfo[0].shmaddr );
  }
  else
#endif
  {
   XDestroyImage( myximage );
  }
  myximage=NULL;
}

static uint32_t config( uint32_t width,uint32_t height,uint32_t d_width,uint32_t d_height,uint32_t flags,char *title,uint32_t format)
{
// int screen;
 int fullscreen=0;
 int vm=0;
// int interval, prefer_blank, allow_exp, nothing;
 unsigned int fg,bg;
 XEvent xev;
 XGCValues xgcv;
 Colormap theCmap;
 XSetWindowAttributes xswa;
 unsigned long xswamask;
#ifdef HAVE_XF86VM
 unsigned int modeline_width, modeline_height;
 static uint32_t vm_width;
 static uint32_t vm_height;
#endif

 vo_mouse_autohide=1;
 old_vo_dwidth=-1;
 old_vo_dheight=-1;

 if (!title)
    title = strdup("MPlayer X11 (XImage/Shm) render");

 in_format=format;
 srcW= width;
 srcH= height;
 vo_dx=( vo_screenwidth - d_width ) / 2; vo_dy=( vo_screenheight - d_height ) / 2;
 vo_dwidth=d_width; vo_dheight=d_height;
 
 if( flags&0x03 ) fullscreen = 1;
 if( flags&0x02 ) vm = 1;
 if( flags&0x08 ) Flip_Flag = 1;
 zoomFlag = flags&0x04;
// if(!fullscreen) zoomFlag=1; //it makes no sense to avoid zooming on windowd mode
 
//printf( "w: %d h: %d\n\n",vo_dwidth,vo_dheight );

 XGetWindowAttributes( mDisplay,mRootWin,&attribs );
 depth=attribs.depth;

 if ( depth != 15 && depth != 16 && depth != 24 && depth != 32 ) {
   Visual *visual;
   depth = vo_find_depth_from_visuals(mDisplay, mScreen, &visual);
 }
 if ( !XMatchVisualInfo( mDisplay,mScreen,depth,DirectColor,&vinfo ) ||
      WinID > 0 && vinfo.visualid != XVisualIDFromVisual(attribs.visual))
   XMatchVisualInfo( mDisplay,mScreen,depth,TrueColor,&vinfo );

 /* set image size (which is indeed neither the input nor output size), 
    if zoom is on it will be changed during draw_slice anyway so we dont dupplicate the aspect code here 
 */
 image_width=(width + 7) & (~7);
 image_height=height;

 aspect= ((1<<16)*d_width + d_height/2)/d_height;

#ifdef HAVE_NEW_GUI
 if(use_gui) guiGetEvent( guiSetShVideo,0 ); // the GUI will set up / resize the window
  else
#endif   
   {
#ifdef HAVE_XF86VM
    if ( vm )
   {
	if ((d_width==0) && (d_height==0))
	  { vm_width=image_width; vm_height=image_height; }
	else
	  { vm_width=d_width; vm_height=d_height; }
	vo_vm_switch(vm_width, vm_height,&modeline_width, &modeline_height);
	vo_dx=(vo_screenwidth-modeline_width)/2;
	vo_dy=(vo_screenheight-modeline_height)/2;
	vo_dwidth=modeline_width;
	vo_dheight=modeline_height;
   }
#endif
    bg=WhitePixel( mDisplay,mScreen );
    fg=BlackPixel( mDisplay,mScreen );

    theCmap=vo_x11_create_colormap(&vinfo);

    xswa.background_pixel=0;
    xswa.border_pixel=0;
    xswa.colormap=theCmap;
    xswamask=CWBackPixel | CWBorderPixel | CWColormap;

#ifdef HAVE_XF86VM
    if ( vm )
     {
      xswa.override_redirect=True;
      xswamask|=CWOverrideRedirect;
     }
#endif
 
    if ( WinID>=0 ){
      vo_window = WinID ? ((Window)WinID) : mRootWin;
      if ( WinID )
       {
        XUnmapWindow( mDisplay,vo_window );
        XChangeWindowAttributes( mDisplay,vo_window,xswamask,&xswa );
	vo_x11_selectinput_witherr( mDisplay,vo_window,StructureNotifyMask | KeyPressMask | PropertyChangeMask | PointerMotionMask | ButtonPressMask | ButtonReleaseMask | ExposureMask );
	XMapWindow( mDisplay,vo_window );
       } else XSelectInput( mDisplay,vo_window,ExposureMask );
    }
    else
     {
      if ( vo_window == None )
       {
        vo_window=XCreateWindow( mDisplay,mRootWin,
    			 vo_dx,vo_dy,
			 vo_dwidth,vo_dheight,
                         xswa.border_pixel,depth,CopyFromParent,vinfo.visual,xswamask,&xswa );

        vo_x11_classhint( mDisplay,vo_window,"x11" );
        vo_hidecursor(mDisplay,vo_window);
        vo_x11_sizehint( vo_dx,vo_dy,vo_dwidth,vo_dheight,0 );
        XSelectInput( mDisplay,vo_window,StructureNotifyMask );
        XStoreName( mDisplay,vo_window,title );
        XMapWindow( mDisplay,vo_window );
//      if(WinID!=0)
           do { XNextEvent( mDisplay,&xev ); } while ( xev.type != MapNotify || xev.xmap.event != vo_window );
 
        if ( fullscreen ) vo_x11_fullscreen();
#ifdef HAVE_XINERAMA
        vo_x11_xinerama_move(mDisplay,vo_window);
#endif
      } else if ( !fullscreen ) XMoveResizeWindow( mDisplay,vo_window,vo_dx,vo_dy,vo_dwidth,vo_dheight );
     }

    XFlush( mDisplay );
    XSync( mDisplay,False );

    // we cannot grab mouse events on root window :(
    vo_x11_selectinput_witherr( mDisplay,vo_window,StructureNotifyMask | KeyPressMask | PropertyChangeMask | ExposureMask |
	((WinID==0)?0:(ButtonPressMask | ButtonReleaseMask | PointerMotionMask)) );

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

  if ( vo_gc != None ) XFreeGC( mDisplay,vo_gc );
  vo_gc=XCreateGC( mDisplay,vo_window,0L,&xgcv );
  
  if ( myximage )
   {
    freeMyXImage();
    sws_freeContext(swsContext);
   }
  getMyXImage();
  
  if ( !WinID )
   { vo_dwidth=vo_screenwidth; vo_dheight=vo_screenheight; }

  switch ((bpp=myximage->bits_per_pixel)){
         case 24: draw_alpha_fnc=draw_alpha_24; 
	 	  out_format= IMGFMT_BGR24; break;
         case 32: draw_alpha_fnc=draw_alpha_32;
	 	  out_format= IMGFMT_BGR32; break;
         case 15:
         case 16: if (depth==15){
	 	     draw_alpha_fnc=draw_alpha_15;
	 	     out_format= IMGFMT_BGR15;
       		  }else{
	 	     draw_alpha_fnc=draw_alpha_16;
	 	     out_format= IMGFMT_BGR16;
          	  }break;
	case  8: draw_alpha_fnc=draw_alpha_null;
		 out_format= IMGFMT_BGR8; break;
   	default:  draw_alpha_fnc=draw_alpha_null;
  }

  /* always allocate swsContext as size could change between frames */
  swsContext= sws_getContextFromCmdLine(width, height, in_format, width, height, out_format );

  //printf( "X11 bpp: %d  color mask:  R:%lX  G:%lX  B:%lX\n",bpp,myximage->red_mask,myximage->green_mask,myximage->blue_mask );

  // If we have blue in the lowest bit then obviously RGB
  mode=( ( myximage->blue_mask & 0x01 ) != 0 ) ? MODE_RGB : MODE_BGR;
#ifdef WORDS_BIGENDIAN
  if ( myximage->byte_order != MSBFirst )
#else
  if ( myximage->byte_order != LSBFirst )
#endif
  {
    mode=( ( myximage->blue_mask & 0x01 ) != 0 ) ? MODE_BGR : MODE_RGB;
//   printf( "No support for non-native XImage byte order!\n" );
//   return -1;
  }

#ifdef WORDS_BIGENDIAN
  if(mode==MODE_BGR && bpp!=32){
    mp_msg(MSGT_VO,MSGL_ERR,"BGR%d not supported, please contact the developers\n", bpp);
    return -1;
  }
#else
  if(mode==MODE_BGR){
    mp_msg(MSGT_VO,MSGL_ERR,"BGR not supported, please contact the developers\n");
    return -1;
  }
#endif  

 saver_off(mDisplay);
 return 0;
}

static void Display_Image( XImage *myximage,uint8_t *ImageData )
{
#ifdef HAVE_SHM
 if ( Shmem_Flag )
  {
   XShmPutImage( mDisplay,vo_window,vo_gc,myximage,
                 0,0,
                 ( vo_dwidth - swsContext->dstW ) / 2,( vo_dheight - myximage->height ) / 2,
                 swsContext->dstW,myximage->height,True );
  }
  else
#endif
   {
    XPutImage( mDisplay,vo_window,vo_gc,myximage,
               0,0,
               ( vo_dwidth - swsContext->dstW ) / 2,( vo_dheight - myximage->height ) / 2,
               swsContext->dstW,myximage->height);
  }
}

static void draw_osd(void)
{ vo_draw_text(image_width,image_height,draw_alpha_fnc); }

static void flip_page( void ){
    Display_Image( myximage,ImageData );
    XSync(mDisplay, False);
}

static uint32_t draw_slice( uint8_t *src[],int stride[],int w,int h,int x,int y )
{
  uint8_t *dst[3];
  int dstStride[3];

  if((old_vo_dwidth != vo_dwidth || old_vo_dheight != vo_dheight) /*&& y==0*/ && zoomFlag)
  {
    int newW= vo_dwidth;
    int newH= vo_dheight;
    int newAspect= (newW*(1<<16) + (newH>>1))/newH;
    SwsContext *oldContext= swsContext;
    
    if(newAspect>aspect) newW= (newH*aspect + (1<<15))>>16;
    else                 newH= ((newW<<16) + (aspect>>1)) /aspect;

    old_vo_dwidth=  vo_dwidth;
    old_vo_dheight= vo_dheight;

    if(sws_flags==0) newW&= (~31); // not needed but, if the user wants the FAST_BILINEAR SCALER, then its needed

    swsContext= sws_getContextFromCmdLine(srcW, srcH, in_format, 
    					 newW, newH, out_format);
    if(swsContext)
    {
	image_width= (newW+7)&(~7);
	image_height= newH;

	freeMyXImage();
	getMyXImage();
	sws_freeContext(oldContext);
    }    
    else
    {
	swsContext= oldContext;
    }
  }
  dstStride[1]=
  dstStride[2]=0;
  dst[1]=
  dst[2]=NULL;

  if(Flip_Flag)
  {
	dstStride[0]= -image_width*((bpp+7)/8);
	dst[0]=ImageData - dstStride[0]*(image_height-1);
	sws_scale_ordered(swsContext,src,stride,y,h,dst, dstStride);
  }
  else
  {
	dstStride[0]=image_width*((bpp+7)/8);
	dst[0]=ImageData;
	sws_scale_ordered(swsContext,src,stride,y,h,dst, dstStride);
  }
  return 0;
}

static uint32_t draw_frame( uint8_t *src[] ){
#if 0
      int stride[3]= {0,0,0};
      
      if     (in_format==IMGFMT_YUY2)  stride[0]=srcW*2;
      else if(in_format==IMGFMT_BGR8)  stride[0]=srcW;
      else if(in_format==IMGFMT_BGR15) stride[0]=srcW*2;
      else if(in_format==IMGFMT_BGR16) stride[0]=srcW*2;
      else if(in_format==IMGFMT_BGR24) stride[0]=srcW*3;
      else if(in_format==IMGFMT_BGR32) stride[0]=srcW*4;
      
      return draw_slice(src, stride, srcW, srcH, 0, 0);
#else
    printf("draw_frame() called!!!!!!\n");
    return -1;
#endif
}

static uint32_t get_image(mp_image_t *mpi)
{
    if (zoomFlag ||
	!IMGFMT_IS_BGR(mpi->imgfmt) ||
	(IMGFMT_BGR_DEPTH(mpi->imgfmt) != vo_depthonscreen) ||
	((mpi->type != MP_IMGTYPE_STATIC) && (mpi->type != MP_IMGTYPE_TEMP)) ||
	(mpi->flags & MP_IMGFLAG_PLANAR) ||
	(mpi->flags & MP_IMGFLAG_YUV) ||
	(mpi->width != image_width) ||
	(mpi->height != image_height)
    )
	return(VO_FALSE);

    if (Flip_Flag)
    {
	mpi->stride[0] = -image_width*((bpp+7)/8);
	mpi->planes[0] = ImageData - mpi->stride[0]*(image_height-1);
    }
    else
    {
	mpi->stride[0] = image_width*((bpp+7)/8);
	mpi->planes[0] = ImageData;
    }
    mpi->flags |= MP_IMGFLAG_DIRECT;
    
    return(VO_TRUE);
}

static uint32_t query_format( uint32_t format )
{
    mp_msg(MSGT_VO,MSGL_DBG2,"vo_x11: query_format was called: %x (%s)\n",format,vo_format_name(format));
    if (IMGFMT_IS_BGR(format))
    {
	if (IMGFMT_BGR_DEPTH(format) == 8)
	    return 0; // TODO 8bpp not yet fully implemented
	if (IMGFMT_BGR_DEPTH(format) == vo_depthonscreen)
	    return 3|VFCAP_OSD|VFCAP_SWSCALE|VFCAP_FLIP|VFCAP_ACCEPT_STRIDE;
	else
	    return 1|VFCAP_OSD|VFCAP_SWSCALE|VFCAP_FLIP|VFCAP_ACCEPT_STRIDE;
    }

 switch( format )
  {
//   case IMGFMT_BGR8:  
//   case IMGFMT_BGR15:
//   case IMGFMT_BGR16:
//   case IMGFMT_BGR24:
//   case IMGFMT_BGR32:
//    return 0x2;
//   case IMGFMT_YUY2: 
   case IMGFMT_I420:
   case IMGFMT_IYUV:
   case IMGFMT_YV12:
    return 1|VFCAP_OSD|VFCAP_SWSCALE|VFCAP_ACCEPT_STRIDE;
  }
 return 0;
}


static void uninit(void)
{
 if(!myximage) return;
 
 freeMyXImage();
 saver_on(mDisplay); // screen saver back on

#ifdef HAVE_XF86VM
 vo_vm_close(mDisplay);
#endif

 zoomFlag=0;
 vo_x11_uninit();

 sws_freeContext(swsContext);
}

static uint32_t preinit(const char *arg)
{
    if(arg) 
    {
	mp_msg(MSGT_VO,MSGL_ERR,"vo_x11: Unknown subdevice: %s\n",arg);
	return ENOSYS;
    }

    if( !vo_init() ) return -1; // Can't open X11
    return 0;
}

static uint32_t control(uint32_t request, void *data, ...)
{
  switch (request) {
  case VOCTRL_QUERY_FORMAT:
    return query_format(*((uint32_t*)data));
  case VOCTRL_GUISUPPORT:
    return VO_TRUE;
  case VOCTRL_GET_IMAGE:
    return get_image(data);
  case VOCTRL_SET_EQUALIZER:
    {
      va_list ap;
      int value;
      va_start(ap, data);
      value = va_arg(ap, int);
      va_end(ap);
      return vo_x11_set_equalizer(data, value);
	}
  case VOCTRL_GET_EQUALIZER:
    {
      va_list ap;
      int *value;
      va_start(ap, data);
      value = va_arg(ap, int *);
      va_end(ap);
      return vo_x11_get_equalizer(data, value);
    }
  case VOCTRL_FULLSCREEN:
    vo_x11_fullscreen();
    return VO_TRUE;
  }
  return VO_NOTIMPL;
}


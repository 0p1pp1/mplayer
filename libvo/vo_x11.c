#define DISP

/*
 * video_out_x11.c,X11 interface
 *
 *
 * Copyright ( C ) 1996,MPEG Software Simulation Group. All Rights Reserved.
 *
 * Hacked into mpeg2dec by
 *
 * Aaron Holtzman <aholtzma@ess.engr.uvic.ca>
 *
 * 15 & 16 bpp support added by Franck Sicard <Franck.Sicard@solsoft.fr>
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

#include "config.h"
#include "video_out.h"
#include "video_out_internal.h"

LIBVO_EXTERN( x11 )

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/extensions/XShm.h>
#ifdef HAVE_XF86VM
#include <X11/extensions/xf86vmode.h>
#endif
#include <errno.h>
#include "yuv2rgb.h"

#include "x11_common.h"

#include "fastmemcpy.h"
#include "sub.h"

#include "../postproc/swscale.h"

static vo_info_t vo_info =
{
        "X11 ( XImage/Shm )",
        "x11",
        "Aaron Holtzman <aholtzma@ess.engr.uvic.ca>",
        ""
};

/* private prototypes */
static void Display_Image ( XImage * myximage,unsigned char *ImageData );
static void (*draw_alpha_fnc)(int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride);

/* since it doesn't seem to be defined on some platforms */
int XShmGetEventBase( Display* );

/* local data */
static unsigned char *ImageData;

#ifdef HAVE_XF86VM
XF86VidModeModeInfo **vidmodes=NULL;
#endif

/* X11 related variables */
//static Display *mDisplay;
static Window mywindow;
static GC mygc;
static XImage *myximage;
static int depth,bpp,mode;
static XWindowAttributes attribs;
static int X_already_started=0;

//static int vo_dwidth,vo_dheight;

#define SH_MEM

#ifdef SH_MEM

#include <sys/ipc.h>
#include <sys/shm.h>
#include <X11/extensions/XShm.h>

//static int HandleXError _ANSI_ARGS_( ( Display * dpy,XErrorEvent * event ) );
static void InstallXErrorHandler ( void );
static void DeInstallXErrorHandler ( void );

static int Shmem_Flag;
static int Quiet_Flag;
static XShmSegmentInfo Shminfo[1];
static int gXErrorFlag;
static int CompletionType=-1;

static int Flip_Flag;

static void InstallXErrorHandler()
{
        //XSetErrorHandler( HandleXError );
        XFlush( mDisplay );
}

static void DeInstallXErrorHandler()
{
        XSetErrorHandler( NULL );
        XFlush( mDisplay );
}

#endif

static uint32_t image_width;
static uint32_t image_height;
static uint32_t image_format;

static void check_events(){
  vo_x11_check_events(mDisplay);
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

static unsigned int scale_xinc=0;
static unsigned int scale_yinc=0;

static uint32_t init( uint32_t width,uint32_t height,uint32_t d_width,uint32_t d_height,uint32_t flags,char *title,uint32_t format )
{
// int screen;
 int fullscreen=0;
 int vm=0;
// int interval, prefer_blank, allow_exp, nothing;
 unsigned int fg,bg;
 char *hello=( title == NULL ) ? "X11 render" : title;
// char *name=":0.0";
 XSizeHints hint;
 XVisualInfo vinfo;
 XEvent xev;
 XGCValues xgcv;
 Colormap theCmap;
 XSetWindowAttributes xswa;
 unsigned long xswamask;
 unsigned int modeline_width, modeline_height;

 image_height=height;
 image_width=width;
 image_format=format;

 if( flags&0x03 ) fullscreen = 1;
 if( flags&0x02 ) vm = 1;
 if( flags&0x08 ) Flip_Flag = 1;
 
//printf( "w: %d h: %d\n\n",vo_dwidth,vo_dheight );

 XGetWindowAttributes( mDisplay,DefaultRootWindow( mDisplay ),&attribs );
 depth=attribs.depth;

 if ( depth != 15 && depth != 16 && depth != 24 && depth != 32 ) depth=24;
 XMatchVisualInfo( mDisplay,mScreen,depth,TrueColor,&vinfo );

 if( flags&0x04 && depth>=24 && format==IMGFMT_YV12 ) {
     // software scale
     scale_xinc=(width << 8) / d_width - 1;  // -1 needed for proper rounding
     scale_yinc=(height << 16) / d_height;
     image_width=d_width;
     image_height=d_height;
     SwScale_Init();
 }

#ifdef HAVE_NEW_GUI
 if ( vo_window != None ) { mywindow=vo_window; mygc=vo_gc; }
  else
#endif   
   {
    if ( X_already_started ) return -1;
    if( !vo_init() ) return 0; // Can't open X11

    hint.x=0;
    hint.y=0;
    hint.width=image_width;
    hint.height=image_height;
 

#ifdef HAVE_XF86VM
    if (vm) {
        unsigned int vm_event, vm_error;
	unsigned int vm_ver, vm_rev;
        int i,j,have_vm=0,X,Y;

        int modecount;

        if (XF86VidModeQueryExtension(mDisplay, &vm_event, &vm_error)) {
            XF86VidModeQueryVersion(mDisplay, &vm_ver, &vm_rev);
            printf("XF86VidMode Extension v%i.%i\n", vm_ver, vm_rev);
            have_vm=1;
        } else
            printf("XF86VidMode Extenstion not available.\n");

    if (have_vm) {
      if (vidmodes==NULL)
        XF86VidModeGetAllModeLines(mDisplay,mScreen,&modecount,&vidmodes);
      j=0;
      modeline_width=vidmodes[0]->hdisplay;
      modeline_height=vidmodes[0]->vdisplay;
      if ((d_width==0) && (d_height==0))
        { X=image_width; Y=image_height; }
      else
        { X=d_width; Y=d_height; }

      for (i=1; i<modecount; i++)
        if ((vidmodes[i]->hdisplay >= X) && (vidmodes[i]->vdisplay >= Y))
          if ( (vidmodes[i]->hdisplay < modeline_width ) && (vidmodes[i]->vdisplay < modeline_height) )
          {
             modeline_width=vidmodes[i]->hdisplay;
             modeline_height=vidmodes[i]->vdisplay;
             j=i;
          }

      printf("XF86VM: Selected video mode %dx%d for image size %dx%d.\n",modeline_width, modeline_height, image_width, image_height);
      XF86VidModeLockModeSwitch(mDisplay,mScreen,0);
      XF86VidModeSwitchToMode(mDisplay,mScreen,vidmodes[j]);
      XF86VidModeSwitchToMode(mDisplay,mScreen,vidmodes[j]);
      X=(vo_screenwidth-modeline_width)/2;
      Y=(vo_screenheight-modeline_height)/2;
      XF86VidModeSetViewPort(mDisplay,mScreen,X,Y);
    }
  }
#endif

#ifdef HAVE_XF86VM
    if ( vm )
     {
      hint.x=(vo_screenwidth-modeline_width)/2;
      hint.y=(vo_screenheight-modeline_height)/2;
      hint.width=modeline_width;
      hint.height=modeline_height;
     }
    else
#endif
    if ( fullscreen )
     {
      hint.width=vo_screenwidth;
      hint.height=vo_screenheight;
     }
    hint.flags=PPosition | PSize;

    bg=WhitePixel( mDisplay,mScreen );
    fg=BlackPixel( mDisplay,mScreen );
    vo_dwidth=hint.width;
    vo_dheight=hint.height;

    theCmap  =XCreateColormap( mDisplay,RootWindow( mDisplay,mScreen ),
    vinfo.visual,AllocNone );

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
 
    mywindow=XCreateWindow( mDisplay,RootWindow( mDisplay,mScreen ),
                         hint.x,hint.y,
                         hint.width,hint.height,
                         xswa.border_pixel,depth,CopyFromParent,vinfo.visual,xswamask,&xswa );

    vo_x11_classhint( mDisplay,mywindow,"x11" );
    vo_hidecursor(mDisplay,mywindow);
    if ( fullscreen ) vo_x11_decoration( mDisplay,mywindow,0 );
    XSelectInput( mDisplay,mywindow,StructureNotifyMask );
    XSetStandardProperties( mDisplay,mywindow,hello,hello,None,NULL,0,&hint );
    XMapWindow( mDisplay,mywindow );
    do { XNextEvent( mDisplay,&xev ); } while ( xev.type != MapNotify || xev.xmap.event != mywindow );
    XSelectInput( mDisplay,mywindow,NoEventMask );

    XFlush( mDisplay );
    XSync( mDisplay,False );
    mygc=XCreateGC( mDisplay,mywindow,0L,&xgcv );

#ifdef HAVE_XF86VM
    if ( vm )
     {
      /* Grab the mouse pointer in our window */
      XGrabPointer(mDisplay, mywindow, True, 0,
                   GrabModeAsync, GrabModeAsync,
                   mywindow, None, CurrentTime);
      XSetInputFocus(mDisplay, mywindow, RevertToNone, CurrentTime);
     }
#endif
   }

#ifdef SH_MEM
 if ( mLocalDisplay && XShmQueryExtension( mDisplay ) ) Shmem_Flag=1;
  else
   {
    Shmem_Flag=0;
    if ( !Quiet_Flag ) printf( "Shared memory not supported\nReverting to normal Xlib\n" );
   }
 if ( Shmem_Flag ) CompletionType=XShmGetEventBase( mDisplay ) + ShmCompletion;

 InstallXErrorHandler();

 if ( Shmem_Flag )
  {
   myximage=XShmCreateImage( mDisplay,vinfo.visual,depth,ZPixmap,NULL,&Shminfo[0],image_width,image_height );
   if ( myximage == NULL )
    {
     if ( myximage != NULL ) XDestroyImage( myximage );
     if ( !Quiet_Flag ) printf( "Shared memory error,disabling ( Ximage error )\n" );
     goto shmemerror;
    }
   Shminfo[0].shmid=shmget( IPC_PRIVATE,
   myximage->bytes_per_line * myximage->height ,
   IPC_CREAT | 0777 );
   if ( Shminfo[0].shmid < 0 )
   {
    XDestroyImage( myximage );
    if ( !Quiet_Flag )
     {
      printf( "%s\n",strerror( errno ) );
      perror( strerror( errno ) );
      printf( "Shared memory error,disabling ( seg id error )\n" );
     }
    goto shmemerror;
   }
   Shminfo[0].shmaddr=( char * ) shmat( Shminfo[0].shmid,0,0 );

   if ( Shminfo[0].shmaddr == ( ( char * ) -1 ) )
   {
    XDestroyImage( myximage );
    if ( Shminfo[0].shmaddr != ( ( char * ) -1 ) ) shmdt( Shminfo[0].shmaddr );
    if ( !Quiet_Flag ) printf( "Shared memory error,disabling ( address error )\n" );
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
    if ( !Quiet_Flag ) printf( "Shared memory error,disabling.\n" );
    gXErrorFlag=0;
    goto shmemerror;
   }
   else
    shmctl( Shminfo[0].shmid,IPC_RMID,0 );

   if ( !Quiet_Flag ) printf( "Sharing memory.\n" );
 }
 else
  {
   shmemerror:
   Shmem_Flag=0;
#endif
   myximage=XGetImage( mDisplay,mywindow,0,0,
   image_width,image_height,AllPlanes,ZPixmap );
   ImageData=myximage->data;
#ifdef SH_MEM
  }

  DeInstallXErrorHandler();
#endif

  switch ((bpp=myximage->bits_per_pixel)){
         case 24: draw_alpha_fnc=draw_alpha_24; break;
         case 32: draw_alpha_fnc=draw_alpha_32; break;
         case 15:
         case 16: if (depth==15)
	 	     draw_alpha_fnc=draw_alpha_15;
       		  else
	 	     draw_alpha_fnc=draw_alpha_16;
          	  break;
   	default:  draw_alpha_fnc=draw_alpha_null;
 }

//  printf( "X11 color mask:  R:%lX  G:%lX  B:%lX\n",myximage->red_mask,myximage->green_mask,myximage->blue_mask );

  // If we have blue in the lowest bit then obviously RGB
  mode=( ( myximage->blue_mask & 0x01 ) != 0 ) ? MODE_RGB : MODE_BGR;
#ifdef WORDS_BIGENDIAN
  if ( myximage->byte_order != MSBFirst )
#else
  if ( myximage->byte_order != LSBFirst )
#endif
  {
    mode=( ( myximage->blue_mask & 0x01 ) != 0 ) ? MODE_BGR : MODE_RGB;
//   printf( "No support fon non-native XImage byte order!\n" );
//   return -1;
  }

 if( format==IMGFMT_YV12 ) yuv2rgb_init( ( depth == 24 ) ? bpp : depth,mode );
  
#ifdef HAVE_NEW_GUI  
 if ( vo_window == None ) 
#endif 
  {
   XSelectInput( mDisplay,mywindow,StructureNotifyMask | KeyPressMask );
   saver_off(mDisplay);
  }
 X_already_started++;
 return 0;
}

static const vo_info_t* get_info( void )
{ return &vo_info; }

static void Terminate_Display_Process( void )
{
 getchar();      /* wait for enter to remove window */
#ifdef SH_MEM
 if ( Shmem_Flag )
  {
   XShmDetach( mDisplay,&Shminfo[0] );
   XDestroyImage( myximage );
   shmdt( Shminfo[0].shmaddr );
  }
#endif
 XDestroyWindow( mDisplay,mywindow );
 XCloseDisplay( mDisplay );
 X_already_started=0;
}

static void Display_Image( XImage *myximage,uint8_t *ImageData )
{
#ifdef DISP
#ifdef SH_MEM
 if ( Shmem_Flag )
  {
   XShmPutImage( mDisplay,mywindow,mygc,myximage,
                 0,0,
                 ( vo_dwidth - myximage->width ) / 2,( vo_dheight - myximage->height ) / 2,
                 myximage->width,myximage->height,True );
  }
  else
#endif
   {
    XPutImage( mDisplay,mywindow,mygc,myximage,
               0,0,
               ( vo_dwidth - myximage->width ) / 2,( vo_dheight - myximage->height ) / 2,
               myximage->width,myximage->height );
  }
#endif
}

static void draw_osd(void)
{ vo_draw_text(image_width,image_height,draw_alpha_fnc); }

static void flip_page( void ){
    Display_Image( myximage,ImageData );
    XSync(mDisplay, False);
}

static uint32_t draw_slice( uint8_t *src[],int stride[],int w,int h,int x,int y )
{

if(scale_xinc){
 SwScale_YV12slice_brg24(src,stride,y,h,
                         ImageData, image_width*((bpp+7)/8), image_width, ((bpp+7)/8),
			 scale_xinc, scale_yinc);
} else {
 uint8_t *dst=ImageData + ( image_width * y + x ) * ( bpp/8 );
 yuv2rgb( dst,src[0],src[1],src[2],w,h,image_width*( bpp/8 ),stride[0],stride[1] );
}
 return 0;
}

void rgb15to16_mmx( char* s0,char* d0,int count );

static uint32_t draw_frame( uint8_t *src[] ){
    int sbpp=( ( image_format&0xFF )+7 )/8;
    int dbpp=( bpp+7 )/8;
    char *d=ImageData;
    char *s=src[0];
    //printf( "sbpp=%d  dbpp=%d  depth=%d  bpp=%d\n",sbpp,dbpp,depth,bpp );

  if( Flip_Flag ){
    // flipped BGR
    int i;
    //printf( "Rendering flipped BGR frame  bpp=%d  src=%d  dst=%d\n",bpp,sbpp,dbpp );
    s+=sbpp*image_width*image_height;
    for( i=0;i<image_height;i++ ) {
      s-=sbpp*image_width;
      if( sbpp==dbpp ) {
        if( depth==16 && image_format==( IMGFMT_BGR|15 ) ){

       // do 15bpp->16bpp
#ifdef HAVE_MMX
       rgb15to16_mmx( s,d,2*image_width );
#else
       unsigned short *s1=( unsigned short * )s;
       unsigned short *d1=( unsigned short * )d;
       unsigned short *e=s1+image_width;
       while( s1<e ){
         register int x=*( s1++ );
         // rrrrrggggggbbbbb
         // 0rrrrrgggggbbbbb
         // 0111 1111 1110 0000=0x7FE0
         // 00000000000001 1111=0x001F
         *( d1++ )=( x&0x001F )|( ( x&0x7FE0 )<<1 );
       }
#endif

        } else
          memcpy( d,s,sbpp*image_width );
      } else {
         // sbpp!=dbpp
         char *s2=s;
         char *d2=d;
         char *e=s2+sbpp*image_width;
         while( s2<e ) {
           d2[0]=s2[0];
           d2[1]=s2[1];
           d2[2]=s2[2];
           s2+=sbpp;d2+=dbpp;
         }
      }
      d+=dbpp*image_width;
    }
  } else {
    if( sbpp==dbpp ) {
      if( depth==16 && image_format==( IMGFMT_BGR|15 ) ){
       // do 15bpp->16bpp
#ifdef HAVE_MMX
       rgb15to16_mmx( s,d,2*image_width*image_height );
#else
       unsigned short *s1=( unsigned short * )s;
       unsigned short *d1=( unsigned short * )d;
       unsigned short *e=s1+image_width*image_height;
       while( s1<e ){
         register int x=*( s1++ );
         // rrrrrggggggbbbbb
         // 0rrrrrgggggbbbbb
         // 0111 1111 1110 0000=0x7FE0
         // 00000000000001 1111=0x001F
         *( d1++ )=( x&0x001F )|( ( x&0x7FE0 )<<1 );
       }
#endif
      } else
        memcpy( d,s,sbpp*image_width*image_height );
    } else {
        // sbpp!=dbpp
      char *e=s+sbpp*image_width*image_height;
      //printf( "libvo: using C 24->32bpp conversion\n" );
      while( s<e ){
        d[0]=s[0];
        d[1]=s[1];
        d[2]=s[2];
        s+=sbpp;d+=dbpp;
      }
    }
  }
  return 0;
}

static uint32_t query_format( uint32_t format )
{
 if( !vo_init() ) return 0; // Can't open X11

 if( ( format&IMGFMT_BGR_MASK )==IMGFMT_BGR ){
   int bpp=format&0xFF;
   if( bpp==vo_depthonscreen ) return 1;
   if( bpp==15 && vo_depthonscreen==16) return 1; // built-in conversion
   if( bpp==24 && vo_depthonscreen==32) return 1; // built-in conversion
 }

 switch( format )
  {
   case IMGFMT_YV12: return 1;
  }
 return 0;
}


static void
uninit(void)
{
#ifdef HAVE_NEW_GUI
 if ( vo_window == None )
#endif
 {
  saver_on(mDisplay); // screen saver back on
  XDestroyWindow( mDisplay,mywindow );
 } 
#ifdef HAVE_XF86VM
 #ifdef HAVE_NEW_GUI
        if ((vidmodes!=NULL)&&( vo_window == None ) )
 #else
        if (vidmodes!=NULL)
 #endif
        {
          int screen; screen=DefaultScreen( mDisplay );
          XF86VidModeSwitchToMode(mDisplay,screen,vidmodes[0]);
          XF86VidModeSwitchToMode(mDisplay,screen,vidmodes[0]);
          free(vidmodes);
        }
#endif
printf("vo: uninit!\n");
}




#define DISP

/*
 * video_out_dga.c, X11 interface
 *
 *
 * Copyright ( C ) 2001, Andreas Ackermann. All Rights Reserved.
 *
 * <acki@acki-netz.de>
 *
 * note well: 
 *   
 * o this is alpha
 * o covers only common video card formats
 * o works only on intel architectures
 *
 */



#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "linux/keycodes.h"
#include "config.h"
#include "video_out.h"
#include "video_out_internal.h"
#include "yuv2rgb.h"

LIBVO_EXTERN( dga )

#include <X11/Xlib.h>
#include <X11/extensions/xf86dga.h>

#include "x11_common.h"

static vo_info_t vo_info =
{
        "DGA ( Direct Graphic Access )",
        "dga",
        "Andreas Ackermann <acki@acki-netz.de>",
        ""
};

static int       vo_dga_width;           // bytes per line in framebuffer
static int       vo_dga_vp_width;        // visible pixels per line in framebuffer
static int       vo_dga_vp_height;       // visible lines in framebuffer
static int       vo_dga_is_running = 0; 
static int       vo_dga_src_width;       // width of video in pixels
static int       vo_dga_src_height;      // height of video in pixels
static int       vo_dga_bpp;             // bytes per pixel in framebuffer
static int       vo_dga_src_offset=0;    // offset in src
static int       vo_dga_vp_offset=0;   // offset in dest
static int       vo_dga_bytes_per_line;  // longwords per line to copy
static int       vo_dga_src_skip;        // bytes to skip after copying one line 
                                  // (not supported yet) in src
static int       vo_dga_vp_skip;       // dto. for dest 
static int       vo_dga_lines;         // num of lines to copy
static int       vo_dga_src_format;                                 

static unsigned char     *vo_dga_base;
static Display  *vo_dga_dpy;


#include "mmx.h"

#if defined (HAVE_SSE) || defined (HAVE_3DNOW)
#define movntq "movntq" // use this for processors that have SSE or 3Dnow
#else
#define movntq "movq" // for MMX-only processors
#endif


#define rep_movsl(dest, src, numwords, d_add, count) \
__asm__ __volatile__( \
" \
xfer:                     \n\t\
                  movl %%edx, %%ecx \n\t \
                  cld\n\t \
                  rep\n\t \
                  movsl \n\t\
                  add %%eax, %%edi \n\t\
                  dec %%ebx \n\t\
                  jnz xfer \n\t\
" \
                  : \
                  : "a" (d_add), "b" (count), "S" (src), "D" (dest), "d" (numwords) \
                  : "memory" )

#if 0
                  : "S" (src), "D" (dest), "c" (numwords) \
	  movq (%%eax), %%mm0      \n\t \
          add $64, %%edx            \n\t \
	  movq 8(%%eax), %%mm1     \n\t \
          add $64, %%eax            \n\t \
	  movq -48(%%eax), %%mm2   \n\t \
          movq %%mm0, -64(%%edx)   \n\t \
	  movq -40(%%eax), %%mm3   \n\t \
          movq %%mm1, -56(%%edx)   \n\t \
	  movq -32(%%eax), %%mm4   \n\t \
          movq %%mm2, -48(%%edx)   \n\t \
	  movq -24(%%eax), %%mm5   \n\t \
          movq %%mm3, -40(%%edx)   \n\t \
	  movq -16(%%eax), %%mm6   \n\t \
          movq %%mm4, -32(%%edx)   \n\t \
	  movq -8(%%eax), %%mm7    \n\t \
          movq %%mm5, -24(%%edx)   \n\t \
          movq %%mm6, -16(%%edx)   \n\t \
          dec %%ecx                \n\t \
          movq %%mm7, -8(%%edx)    \n\t \
          jnz xfer                  \n\t \

#endif

#define mmx_movsl(dest, src, numwords) \
__asm__ __volatile__(  \
" \
                                  \n\t \
xfer:                              \n\t \
	  movq (%%eax), %%mm0      \n\t \
          add $64, %%edx            \n\t \
	  movq 8(%%eax), %%mm1     \n\t \
          add $64, %%eax            \n\t \
	  movq -48(%%eax), %%mm2   \n\t \
          movq %%mm0, -64(%%edx)   \n\t \
	  movq -40(%%eax), %%mm3   \n\t \
          movq %%mm1, -56(%%edx)   \n\t \
	  movq -32(%%eax), %%mm4   \n\t \
          movq %%mm2, -48(%%edx)   \n\t \
	  movq -24(%%eax), %%mm5   \n\t \
          movq %%mm3, -40(%%edx)   \n\t \
	  movq -16(%%eax), %%mm6   \n\t \
          movq %%mm4, -32(%%edx)   \n\t \
	  movq -8(%%eax), %%mm7    \n\t \
          movq %%mm5, -24(%%edx)   \n\t \
          movq %%mm6, -16(%%edx)   \n\t \
          dec %%ecx                \n\t \
          movq %%mm7, -8(%%edx)    \n\t \
          jnz xfer                  \n\t \
             \
" \
      : \
      : "a" (src), "d" (dest), "c" (numwords) \
      :  "memory" )

     // src <= eax
     // dst <= edx
     // num <= ecx  
 
static uint32_t draw_frame( uint8_t *src[] ){

  int vp_skip = vo_dga_vp_skip;
  int lpl = vo_dga_bytes_per_line >> 2; 
  int numlines = vo_dga_lines;     

  char *s, *d;

  if( vo_dga_src_format==IMGFMT_YV12 ){
    // We'll never reach this point, because YV12 codecs always calls draw_slice
    printf("vo_dga: draw_frame() doesn't support IMGFMT_YV12 (yet?)\n");
  }else{
    s = *src;
    d = (&((char *)vo_dga_base)[vo_dga_vp_offset]);
    rep_movsl(d, s, lpl, vo_dga_vp_skip, numlines );
  }

  return 0;
}

static void check_events(void)
{
    int e=vo_x11_check_events(vo_dga_dpy);
}

static void flip_page( void ){
    check_events(); 
  //  printf("vo_dga: In flippage\n");

}

static uint32_t draw_slice( uint8_t *src[],int stride[],
                            int w,int h,int x,int y )
{
  //  printf("vo_dga: draw_slice() not implemented (yet?)\n");

  yuv2rgb( vo_dga_base + vo_dga_vp_offset + 
          (vo_dga_width * y +x) * vo_dga_bpp,
           src[0], src[1], src[2],
           w,h, vo_dga_width * vo_dga_bpp,
           stride[0],stride[1] );
 return 0;


  return 0;
};

static void Terminate_Display_Process( void ){

  printf("vo_dga: Terminating display process\n");
}

static const vo_info_t* get_info( void )
{ return &vo_info; }

static uint32_t query_format( uint32_t format )
{
 printf("vo_dga: query_format\n");

 if( !vo_init() ) return 0; // Can't open X11
 printf("Format: %lx\n", format);

 if( format==IMGFMT_YV12 ) return 1;
 if( ( format&IMGFMT_BGR_MASK )==IMGFMT_BGR && 
     ( format&0xFF )==vo_depthonscreen ) return 1;
 return 0;
}


static void
uninit(void)
{

  vo_dga_is_running = 0;
  printf("vo_dga: in uninit\n");
  XUngrabPointer (vo_dga_dpy, CurrentTime);
  XUngrabKeyboard (vo_dga_dpy, CurrentTime);
  XF86DGADirectVideo (vo_dga_dpy, XDefaultScreen(vo_dga_dpy), 0);
  XCloseDisplay(vo_dga_dpy);
}




static uint32_t init( uint32_t width,  uint32_t height,
                      uint32_t d_width,uint32_t d_height,
                      uint32_t fullscreen,char *title,uint32_t format )
{

  int bank, ram;
  int x_off, y_off;

#ifdef HAVE_DGA2
// needed to change DGA video mode
  int modecount,mX, mY, i,j;
  int X,Y;
  XDGAMode *modelines=NULL;
  XDGADevice *dgadevice;
#endif

  if( vo_dga_is_running )return -1;

  if( !vo_init() ){
    printf("vo_dga: vo_init() failed!\n");
    return 0; 
  }

  if((vo_dga_dpy = XOpenDisplay(0))==NULL)
  {
    printf ("vo_dga: Can't open display\n");
    return 1;
  } 

#ifdef HAVE_DGA2
// Code to change the video mode added by Michael Graffam
// mgraffam@idsi.net
  if (modelines==NULL)
    modelines=XDGAQueryModes(vo_dga_dpy, XDefaultScreen(vo_dga_dpy),&modecount);
  
  mX=modelines[0].imageWidth;
  mY=modelines[0].imageHeight;
  X=d_width; Y=d_height; 
  
  j=0; 
  for (i=1; i<=modecount; i++)
  {
    if ( (modelines[i].bitsPerPixel == vo_depthonscreen) && 
         (modelines[i].maxViewportX) && 
         (modelines[i].viewportWidth >= X) && 
         (modelines[i].viewportHeight >= Y) && 
         (modelines[i].viewportWidth < mX) &&
         (modelines[i].viewportHeight < mY) ) 
        {
           mX=modelines[i].viewportWidth;
           mY=modelines[i].viewportHeight;
           j=i;
        }
   }
  X=(modelines[j].imageWidth-mX)/2;
  Y=(modelines[j].imageHeight-mY)/2;
  printf("vo_dga: Using DGA 2.0 mode changing support\n");
  printf("vo_dga: Selected video mode %dx%d for image size %dx%d.\n", mX, mY,width, height);  

  XF86DGASetViewPort (vo_dga_dpy, XDefaultScreen(vo_dga_dpy), X,Y);
  dgadevice=XDGASetMode(vo_dga_dpy, XDefaultScreen(vo_dga_dpy), modelines[j].num);
  XDGASync(vo_dga_dpy, XDefaultScreen(vo_dga_dpy));

  XFree(modelines);
  XFree(dgadevice);
  // end mode change code
#else
printf("vo_dga: DGA 1.0 compatibility code\n");
#endif

XF86DGAGetViewPortSize(vo_dga_dpy,XDefaultScreen(vo_dga_dpy),
			&vo_dga_vp_width,
			&vo_dga_vp_height); 

XF86DGAGetVideo (vo_dga_dpy, XDefaultScreen(vo_dga_dpy), 
                (char **)&vo_dga_base, &vo_dga_width, &bank, &ram);

#ifndef HAVE_DGA2
XF86DGASetViewPort (vo_dga_dpy, XDefaultScreen(vo_dga_dpy), 0, 0);
#endif

  // do some more checkings here ...
  if( format==IMGFMT_YV12 ) 
    yuv2rgb_init( vo_depthonscreen, MODE_RGB );

  vo_dga_src_format = format;
  vo_dga_src_width = width;
  vo_dga_src_height = height;
  vo_dga_bpp = (vo_depthonscreen+7) >> 3;

  printf("vo_dga: bytes/line: %d, screen res: %dx%d, depth: %d, base: %08x, bpp: %d\n", 
          vo_dga_width, vo_dga_vp_width, 
          vo_dga_vp_height, vo_depthonscreen, vo_dga_base,
          vo_dga_bpp);
  printf("vo_dga: video res: %dx%d\n", vo_dga_src_width, vo_dga_src_height);

  if(vo_dga_src_width > vo_dga_vp_width ||
     vo_dga_src_height > vo_dga_vp_height){
    printf("vo_dga: Sorry, video larger than viewport is not yet supported!\n");
    // ugly, do something nicer in the future ...
    return 1;
  }

  x_off = (vo_dga_vp_width - vo_dga_src_width)>>1; 
  y_off = (vo_dga_vp_height - vo_dga_src_height)>>1;

  vo_dga_bytes_per_line = vo_dga_src_width * vo_dga_bpp; // todo
  vo_dga_lines = vo_dga_src_height;                      // todo


  vo_dga_src_offset = 0;
  vo_dga_vp_offset = (y_off * vo_dga_width + x_off ) * vo_dga_bpp;

  vo_dga_vp_skip = (vo_dga_width - vo_dga_src_width) * vo_dga_bpp;  // todo
    
  printf("vo_dga: vp_off=%d, vp_skip=%d, bpl=%d\n", 
         vo_dga_vp_offset, vo_dga_vp_skip, vo_dga_bytes_per_line);


  XF86DGADirectVideo (vo_dga_dpy, XDefaultScreen(vo_dga_dpy), 
                      XF86DGADirectGraphics | XF86DGADirectMouse | 
                      XF86DGADirectKeyb);
  
  XGrabKeyboard (vo_dga_dpy, DefaultRootWindow(vo_dga_dpy), True, 
                 GrabModeAsync,GrabModeAsync, CurrentTime);
  XGrabPointer (vo_dga_dpy, DefaultRootWindow(vo_dga_dpy), True, 
                ButtonPressMask,GrabModeAsync, GrabModeAsync, 
                None, None, CurrentTime);
   
  // now clear screen

  memset(vo_dga_base, 0, vo_dga_width * vo_dga_vp_height * vo_dga_bpp);  

  vo_dga_is_running = 1;
  return 0;
}

#if 0
int vo_dga_query_event(void){

  XEvent  myevent;
  char    text[10];
  KeySym  mykey;
  int     retval = 0;
  int     i;

  if( vo_dga_is_running ){  
     if(XPending(vo_dga_dpy)>0)
      {
	XNextEvent(vo_dga_dpy, &myevent);
	switch (myevent.type)
	  {
	  case ButtonPress:
	    /* Reaktion auf Knopfdruck ---> Textausgabe an der 
	       Mauscursorposition */ 
	   
	    retval = 'q';
            break;
	  case KeyPress:
	    /* Reaktion auf Tastendruck --> Testen ob Taste == "q",
	       falls ja: Programmende */
	    i=XLookupString(&myevent, text, 10, &mykey, 0);

            if (mykey&0xff00 != 0) mykey=mykey&0x00ff + 256;
	
            switch ( mykey )
            {
	    case wsLeft:      retval=KEY_LEFT; break;
	    case wsRight:     retval=KEY_RIGHT; break;
	    case wsUp:        retval=KEY_UP; break;
	    case wsDown:      retval=KEY_DOWN; break;
	    case wsSpace:     retval=' '; break;
	    case wsEscape:    retval=KEY_ESC; break;
	    case wsEnter:     retval=KEY_ENTER; break;
	    case wsq:
	    case wsQ:         retval='q'; break;
	    case wsp:
	    case wsP:         retval='p'; break;
	    case wsMinus:
	    case wsGrayMinus: retval='-'; break;
	    case wsPlus:
	    case wsGrayPlus:  retval='+'; break;
	    }
	    break;
	  }
      }
  }
  return retval;
}
#endif







#define DISP

// this can be 3 or 4  (regarding 24bpp and 32bpp)
#define BYTES_PP 3

#define TEXTUREFORMAT_32BPP

/* 
 * video_out_gl.c, X11/OpenGL interface
 * based on video_out_x11 by Aaron Holtzman,
 * and WS opengl window manager by Pontscho/Fresh!
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "config.h"
#include "video_out.h"
#include "video_out_internal.h"


LIBVO_EXTERN(gl)

#include <X11/Xlib.h>
#include <X11/Xutil.h>
//#include <X11/keysym.h>
#include <GL/glx.h>
#include <errno.h>
#include "yuv2rgb.h"

#include <GL/gl.h>

#include "x11_common.h"

static vo_info_t vo_info = 
{
	"X11 (OpenGL)",
	"gl",
	"Arpad Gereoffy <arpi@esp-team.scene.hu>",
	""
};

/* private prototypes */
// static void Display_Image (unsigned char *ImageData);

/* local data */
static unsigned char *ImageData=NULL;

/* X11 related variables */
static Display *mydisplay;
static Window mywindow;
//static GC mygc;
//static XImage *myximage;
//static int depth,mode;
static XWindowAttributes attribs;
static int X_already_started = 0;

//static int texture_id=1;

static GLXContext wsGLXContext;
//XVisualInfo        * wsVisualInfo;
int                  wsGLXAttrib[] = { GLX_RGBA,
                                       GLX_RED_SIZE,1,
                                       GLX_GREEN_SIZE,1,
                                       GLX_BLUE_SIZE,1,
//                                       GLX_DEPTH_SIZE,16,
                                       GLX_DOUBLEBUFFER,
                                       None };


static uint32_t image_width;
static uint32_t image_height;
static uint32_t image_format;
static uint32_t image_bpp;
static uint32_t image_bytes;

static uint32_t texture_width;
static uint32_t texture_height;

static void resize(int x,int y){
  printf("[gl] Resize: %dx%d\n",x,y);
  glViewport( 0, 0, x, y );

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(0, image_width, image_height, 0, -1,1);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
}

/* connect to server, create and map window,
 * allocate colors and (shared) memory
 */
static uint32_t 
init(uint32_t width, uint32_t height, uint32_t d_width, uint32_t d_height, uint32_t fullscreen, char *title, uint32_t format)
{
	int screen;
        int dwidth,dheight;
	unsigned int fg, bg;
	char *hello = (title == NULL) ? "OpenGL rulez" : title;
	char *name = ":0.0";
	XSizeHints hint;
	XVisualInfo *vinfo;
	XEvent xev;

//	XGCValues xgcv;
	XSetWindowAttributes xswa;
	unsigned long xswamask;

	image_height = height;
	image_width = width;
	image_format = format;
  
	if (X_already_started) return -1;

	if(getenv("DISPLAY"))	name = getenv("DISPLAY");

	mydisplay = XOpenDisplay(name);

	if (mydisplay == NULL)
	{
		printf("[gl] Can not open display\n");
		return -1;
	}

	screen = DefaultScreen(mydisplay);
        vo_screenwidth = DisplayWidth(mydisplay, myscreen);
	vo_screenheight = DisplayHeight(mydisplay, myscreen);

        dwidth=d_width; dheight=d_height;
#ifdef X11_FULLSCREEN
        if(fullscreen){ // handle flags correct
          d_height=(int)((float)vo_screenwidth/(float)dwidth*(float)dheight);
          d_height+=d_height%2; // round
          d_width=vo_screenwidth;
          if(dheight>vo_screenheight){
            d_width=(int)((float)vo_screenheight/(float)dheight*(float)dwidth);
            d_width+=d_width%2; // round
            d_height=vo_screenheight;
          }
          dwidth=d_width; dheight=d_height;
        }
#endif
	hint.x = 0;
	hint.y = 0;
	hint.width = d_width;
	hint.height = d_height;
	hint.flags = PPosition | PSize;

	/* Get some colors */

	bg = WhitePixel(mydisplay, screen);
	fg = BlackPixel(mydisplay, screen);

	/* Make the window */

	XGetWindowAttributes(mydisplay, DefaultRootWindow(mydisplay), &attribs);

#if 0
	/*
	 *
	 * depth in X11 terminology land is the number of bits used to
	 * actually represent the colour.
   	 *
	 * bpp in X11 land means how many bits in the frame buffer per
	 * pixel. 
	 *
	 * ex. 15 bit color is 15 bit depth and 16 bpp. Also 24 bit
	 *     color is 24 bit depth, but can be 24 bpp or 32 bpp.
	 */

	depth = attribs.depth;

	if (depth != 15 && depth != 16 && depth != 24 && depth != 32) 
	{
		/* The root window may be 8bit but there might still be
		* visuals with other bit depths. For example this is the 
		* case on Sun/Solaris machines.
		*/
		depth = 24;
	}
	//BEGIN HACK
	//mywindow = XCreateSimpleWindow(mydisplay, DefaultRootWindow(mydisplay),
	//hint.x, hint.y, hint.width, hint.height, 4, fg, bg);
	//
#endif

//	XMatchVisualInfo(mydisplay, screen, depth, TrueColor, &vinfo);
  vinfo=glXChooseVisual( mydisplay,screen,wsGLXAttrib );
  if (vinfo == NULL)
  {
    printf("[gl] no GLX support present\n");
    return -1;
  }

	xswa.background_pixel = 0;
	xswa.border_pixel     = 1;
//	xswa.colormap         = XCreateColormap(mydisplay, RootWindow(mydisplay,screen), vinfo.visual, AllocNone);
	xswa.colormap         = XCreateColormap(mydisplay, RootWindow(mydisplay,screen), vinfo->visual, AllocNone);
	xswamask = CWBackPixel | CWBorderPixel | CWColormap;
//  xswamask = CWBackPixel | CWBorderPixel | CWColormap | CWEventMask | CWCursor | CWOverrideRedirect | CWSaveUnder | CWX | CWY | CWWidth | CWHeight;

  mywindow = XCreateWindow(mydisplay, RootWindow(mydisplay,screen),
    hint.x, hint.y, hint.width, hint.height, 4, vinfo->depth,CopyFromParent,vinfo->visual,xswamask,&xswa);

  vo_x11_classhint( mydisplay,mywindow,"gl" );
  vo_hidecursor(mydisplay,mywindow);

  wsGLXContext=glXCreateContext( mydisplay,vinfo,NULL,True );
//  XStoreName( wsDisplay,wsMyWin,wsSysName );

//  printf("GLXcontext ok\n");

  if ( fullscreen ) vo_x11_decoration( mydisplay,mywindow,0 );

	XSelectInput(mydisplay, mywindow, StructureNotifyMask);

	/* Tell other applications about this window */

	XSetStandardProperties(mydisplay, mywindow, hello, hello, None, NULL, 0, &hint);

	/* Map window. */

	XMapWindow(mydisplay, mywindow);

	/* Wait for map. */
	do 
	{
		XNextEvent(mydisplay, &xev);
	}
	while (xev.type != MapNotify || xev.xmap.event != mywindow);

	XSelectInput(mydisplay, mywindow, NoEventMask);

  glXMakeCurrent( mydisplay,mywindow,wsGLXContext );

	XFlush(mydisplay);
	XSync(mydisplay, False);

//	mygc = XCreateGC(mydisplay, mywindow, 0L, &xgcv);

//		myximage = XGetImage(mydisplay, mywindow, 0, 0,
//		width, image_height, AllPlanes, ZPixmap);
//		ImageData = myximage->data;
//	bpp = myximage->bits_per_pixel;

	//XSelectInput(mydisplay, mywindow, StructureNotifyMask); // !!!!
        XSelectInput(mydisplay, mywindow, StructureNotifyMask | KeyPressMask );

//  printf("Window setup ok\n");

#if 0
	// If we have blue in the lowest bit then obviously RGB 
	mode = ((myximage->blue_mask & 0x01) != 0) ? MODE_RGB : MODE_BGR;
#ifdef WORDS_BIGENDIAN 
	if (myximage->byte_order != MSBFirst)
#else
	if (myximage->byte_order != LSBFirst) 
#endif
	{
		printf("[gl] no support for non-native XImage byte order!\n");
		return -1;
	}

  printf("DEPTH=%d  BPP=%d\n",depth,bpp);
#endif

	/* 
	 * If depth is 24 then it may either be a 3 or 4 byte per pixel
	 * format. We can't use bpp because then we would lose the 
	 * distinction between 15/16bit depth (2 byte formate assumed).
	 *
	 * FIXME - change yuv2rgb_init to take both depth and bpp
	 * parameters
	 */

  texture_width=32;
  while(texture_width<image_width) texture_width*=2;
  while(texture_width<image_height) texture_width*=2;
  texture_height=texture_width;

  if(format==IMGFMT_YV12){
    yuv2rgb_init(8*BYTES_PP, MODE_BGR);
    printf("[gl] YUV init OK!\n");
    image_bpp=8*BYTES_PP;
    image_bytes=BYTES_PP;
  } else {
    image_bpp=format&0xFF;
    image_bytes=(image_bpp+7)/8;
  }

  ImageData=malloc(texture_width*texture_height*image_bytes);
  memset(ImageData,128,texture_width*texture_height*image_bytes);

  glDisable(GL_BLEND); 
  glDisable(GL_DEPTH_TEST);
  glDepthMask(GL_FALSE);
  glDisable(GL_CULL_FACE);

  glEnable(GL_TEXTURE_2D);

  printf("[gl] Creating %dx%d texture...\n",texture_width,texture_height);

#if 1
//  glBindTexture(GL_TEXTURE_2D, texture_id);
  glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER, GL_LINEAR);
#ifdef TEXTUREFORMAT_32BPP
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB8, texture_width, texture_height, 0,
#else
  glTexImage2D(GL_TEXTURE_2D, 0, BYTES_PP, texture_width, texture_height, 0,
#endif
       (image_bytes==4)?GL_RGBA:GL_BGR, GL_UNSIGNED_BYTE, ImageData);
#endif

  resize(d_width,d_height);

  glClearColor( 1.0f,0.0f,1.0f,0.0f );
  glClear( GL_COLOR_BUFFER_BIT );

//  printf("OpenGL setup OK!\n");

	X_already_started++;
	return 0;
}

static const vo_info_t*
get_info(void)
{
	return &vo_info;
}

static void 
Terminate_Display_Process(void) 
{
	getchar();	/* wait for enter to remove window */
	XDestroyWindow(mydisplay, mywindow);
	XCloseDisplay(mydisplay);
	X_already_started = 0;
}


static void check_events(void)
{
    int e=vo_x11_check_events(mydisplay);
    if(e&VO_EVENT_RESIZE) resize(vo_dwidth,vo_dheight);
}

static void draw_osd(void)
{
}

static void
flip_page(void)
{

//  glEnable(GL_TEXTURE_2D);
//  glBindTexture(GL_TEXTURE_2D, texture_id);

  glColor3f(1,1,1);
  glBegin(GL_QUADS);
    glTexCoord2f(0,0);glVertex2i(0,0);
    glTexCoord2f(0,1);glVertex2i(0,texture_height);
    glTexCoord2f(1,1);glVertex2i(texture_width,texture_height);
    glTexCoord2f(1,0);glVertex2i(texture_width,0);
  glEnd();

//  glFlush();
  glFinish();
  glXSwapBuffers( mydisplay,mywindow );
  
}

//static inline uint32_t draw_slice_x11(uint8_t *src[], uint32_t slice_num)
static uint32_t draw_slice(uint8_t *src[], int stride[], int w,int h,int x,int y)
{
    int i;
    int dstride=w*BYTES_PP;
    
//    dstride=(dstride+15)&(~15);

	yuv2rgb(ImageData, src[0], src[1], src[2], 
			w,h, dstride, stride[0],stride[1]);

//	emms ();

    for(i=0;i<h;i++){
      glTexSubImage2D( GL_TEXTURE_2D,  // target
		       0,              // level
		       x,              // x offset
		       y+i,            // y offset
		       w,              // width
		       1,              // height
		       (BYTES_PP==4)?GL_RGBA:GL_RGB,        // format
		       GL_UNSIGNED_BYTE, // type
		       ImageData+i*dstride );        // *pixels
    }

	return 0;
}

static inline uint32_t 
draw_frame_x11_yv12(uint8_t *src[])
{
int i;
//  printf("Converting YUV->RGB...\n");
	yuv2rgb(ImageData, src[0], src[1], src[2],
		image_width, image_height, 
		image_width*BYTES_PP, image_width, image_width/2 );
//  printf("Ready!\n");

//		emms ();

    for(i=0;i<image_height;i++){
      glTexSubImage2D( GL_TEXTURE_2D,  // target
		       0,              // level
		       0,              // x offset
		       i,              // y offset
		       image_width,    // width
		       1,              // height
		       (BYTES_PP==4)?GL_RGBA:GL_RGB,        // format
		       GL_UNSIGNED_BYTE, // type
		       ImageData+i*BYTES_PP*image_width );        // *pixels
    }

//	Display_Image(ImageData);
	return 0; 
}


static inline uint32_t 
draw_frame_x11_bgr(uint8_t *src[])
{
int i;
uint8_t *s=src[0];
uint8_t *de=&ImageData[3*image_width];

    for(i=0;i<image_height;i++){
      uint8_t *d=ImageData;
      while(d<de){
        d[0]=s[2];
        d[1]=s[1];
        d[2]=s[0];
        s+=3;d+=3;
      }
      glTexSubImage2D( GL_TEXTURE_2D,  // target
		       0,              // level
		       0,              // x offset
//		       image_height-1-i,  // y offset
		       i,  // y offset
		       image_width,    // width
		       1,              // height
		       (image_bytes==4)?GL_RGBA:GL_RGB,        // format
		       GL_UNSIGNED_BYTE, // type
		       ImageData);        // *pixels
    }

//	Display_Image(ImageData);
	return 0; 
}

static inline uint32_t 
draw_frame_x11_rgb(uint8_t *src[])
{
int i;
uint8_t *ImageData=src[0];

    for(i=0;i<image_height;i++){
      glTexSubImage2D( GL_TEXTURE_2D,  // target
		       0,              // level
		       0,              // x offset
//		       image_height-1-i,  // y offset
		       i,  // y offset
		       image_width,    // width
		       1,              // height
		       (image_bytes==4)?GL_RGBA:GL_RGB,        // format
		       GL_UNSIGNED_BYTE, // type
		       ImageData+i*image_bytes*image_width );        // *pixels
    }

//	Display_Image(ImageData);
	return 0; 
}


static uint32_t
draw_frame(uint8_t *src[])
{
    if(image_format==IMGFMT_YV12)
	return draw_frame_x11_yv12(src);
    else 
    if((image_format&IMGFMT_RGB_MASK)==IMGFMT_RGB)
	return draw_frame_x11_rgb(src);
    else
	return draw_frame_x11_bgr(src);
}

static uint32_t
query_format(uint32_t format)
{
    switch(format){
    case IMGFMT_YV12:
    case IMGFMT_RGB|24:
    case IMGFMT_BGR|24:
        return 1;
    }
    return 0;
}


static void
uninit(void)
{
}

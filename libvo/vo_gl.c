#define TEXTUREFORMAT_ALWAYS GL_RGB8

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <errno.h>

#include "mp_msg.h"
#include "config.h"
#include "video_out.h"
#include "video_out_internal.h"
#include "font_load.h"
#include "sub.h"

#include <X11/Xlib.h>
#include <X11/Xutil.h>
//#include <X11/keysym.h>
#include <GL/glx.h>
#include <errno.h>

#include <GL/gl.h>

#include "x11_common.h"
#include "aspect.h"

static vo_info_t info = 
{
	"X11 (OpenGL)",
	"gl",
	"Arpad Gereoffy <arpi@esp-team.scene.hu>",
	""
};

LIBVO_EXTERN(gl)

static GLXContext wsGLXContext;
static int                  wsGLXAttrib[] = { GLX_RGBA,
                                       GLX_RED_SIZE,1,
                                       GLX_GREEN_SIZE,1,
                                       GLX_BLUE_SIZE,1,
                                       GLX_DOUBLEBUFFER,
                                       None };

int use_osd;
#define MAX_OSD_PARTS 20
GLuint osdtex[MAX_OSD_PARTS];
#ifndef FAST_OSD
GLuint osdatex[MAX_OSD_PARTS];
#endif
GLuint osdDispList[MAX_OSD_PARTS];
int osdtexCnt = 0;

static uint32_t image_width;
static uint32_t image_height;
static uint32_t image_bytes;
static int many_fmts;
static GLenum gl_texfmt;
static GLenum gl_format;
static GLenum gl_type;

static int int_pause;

static uint32_t texture_width;
static uint32_t texture_height;

static unsigned int slice_height = 1;

static void resize(int x,int y){
  mp_msg(MSGT_VO, MSGL_V, "[gl] Resize: %dx%d\n",x,y);
  glViewport( 0, 0, x, y );

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(0, image_width, image_height, 0, -1,1);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

#ifdef HAVE_FREETYPE
  // adjust font size to display size
  force_load_font = 1;
#endif
  vo_osd_changed(1);
}

static int find_gl_format (uint32_t format)
{
  image_bytes = (IMGFMT_RGB_DEPTH(format)+7)/8;
  gl_texfmt = 3;
  switch (format) {
    case IMGFMT_RGB24:
      gl_format = GL_RGB;
      gl_type = GL_UNSIGNED_BYTE;
      break;
    case IMGFMT_RGBA:
      gl_texfmt = 4;
      gl_format = GL_RGBA;
      gl_type = GL_UNSIGNED_BYTE;
      break;
    case IMGFMT_Y800:
    case IMGFMT_Y8:
      gl_texfmt = 1;
      image_bytes = 1;
      gl_format = GL_LUMINANCE;
      gl_type = GL_UNSIGNED_BYTE;
      break;
#ifdef GL_VERSION_1_2
    case IMGFMT_RGB8:
      gl_format = GL_RGB;
      gl_type = GL_UNSIGNED_BYTE_2_3_3_REV;
      break;
    case IMGFMT_RGB15:
      gl_format = GL_RGBA;
      gl_type = GL_UNSIGNED_SHORT_1_5_5_5_REV;
      break;
    case IMGFMT_RGB16:
      gl_format = GL_RGB;
      gl_type = GL_UNSIGNED_SHORT_5_6_5_REV;
      break;
    case IMGFMT_BGR8:
      // special case as red and blue have a differen number of bits.
      // GL_BGR and GL_UNSIGNED_BYTE_3_3_2 isn't supported at least
      // by nVidia drivers, and in addition would give more bits to
      // blue than to red, which isn't wanted
      gl_format = GL_RGB;
      gl_type = GL_UNSIGNED_BYTE_3_3_2;
      break;
    case IMGFMT_BGR15:
      gl_format = GL_BGRA;
      gl_type = GL_UNSIGNED_SHORT_1_5_5_5_REV;
      break;
    case IMGFMT_BGR16:
      gl_format = GL_RGB;
      gl_type = GL_UNSIGNED_SHORT_5_6_5;
      break;
    case IMGFMT_BGR24:
      gl_format = GL_BGR;
      gl_type = GL_UNSIGNED_BYTE;
      break;
    case IMGFMT_BGRA:
      gl_texfmt = 4;
      gl_format = GL_BGRA;
      gl_type = GL_UNSIGNED_BYTE;
      break;
#endif
    default:
      gl_texfmt = 4;
      gl_format = GL_RGBA;
      gl_type = GL_UNSIGNED_BYTE;
      return 0;
  }
#ifdef TEXTUREFORMAT_ALWAYS
  gl_texfmt = TEXTUREFORMAT_ALWAYS;
#endif
  return 1;
}

/* connect to server, create and map window,
 * allocate colors and (shared) memory
 */
static uint32_t 
config(uint32_t width, uint32_t height, uint32_t d_width, uint32_t d_height, uint32_t flags, char *title, uint32_t format)
{
	unsigned char *ImageData=NULL;
//	int screen;
	unsigned int fg, bg;
	XSizeHints hint;
	XVisualInfo *vinfo;
	XEvent xev;
	GLint gl_alignment;

//	XGCValues xgcv;

	image_height = height;
	image_width = width;
	find_gl_format (format);
    vo_dwidth = d_width;
    vo_dheight = d_height;

  sub_bg_alpha = 255; // We need alpha = 255 for invisible part of the OSD
	int_pause = 0;

	aspect_save_orig(width,height);
	aspect_save_prescale(d_width,d_height);
	aspect_save_screenres(vo_screenwidth,vo_screenheight);

	aspect(&d_width,&d_height,A_NOZOOM);
#ifdef X11_FULLSCREEN
//        if( flags&0x01 ){ // (-fs)
//          aspect(&d_width,&d_height,A_ZOOM);
//        }
#endif
	hint.x = 0;
	hint.y = 0;
	hint.width = d_width;
	hint.height = d_height;
	hint.flags = PPosition | PSize;

	/* Get some colors */

	bg = WhitePixel(mDisplay, mScreen);
	fg = BlackPixel(mDisplay, mScreen);

	/* Make the window */

  vinfo=glXChooseVisual( mDisplay,mScreen,wsGLXAttrib );
  if (vinfo == NULL)
  {
    mp_msg(MSGT_VO, MSGL_ERR, "[gl] no GLX support present\n");
    return -1;
  }



	if ( vo_window == None )
	 {
          vo_window = vo_x11_create_smooth_window(mDisplay, mRootWin, vinfo->visual, hint.x, hint.y, hint.width, hint.height,
			                          vinfo->depth, XCreateColormap(mDisplay, mRootWin, vinfo->visual, AllocNone));

      vo_x11_classhint( mDisplay,vo_window,"gl" );
      vo_hidecursor(mDisplay,vo_window);

//      if ( flags&0x01 ) vo_x11_decoration( mDisplay,vo_window,0 );
	  XSelectInput(mDisplay, vo_window, StructureNotifyMask);
	  /* Tell other applications about this window */
	  XSetStandardProperties(mDisplay, vo_window, title, title, None, NULL, 0, &hint);
	  /* Map window. */
	  XMapWindow(mDisplay, vo_window);
	  if ( flags&1 ) vo_x11_fullscreen();
#ifdef HAVE_XINERAMA
	  vo_x11_xinerama_move(mDisplay,vo_window);
#endif

	  /* Wait for map. */
	  do 
	  {
		XNextEvent(mDisplay, &xev);
	  }
	  while (xev.type != MapNotify || xev.xmap.event != vo_window);

	  XSelectInput(mDisplay, vo_window, NoEventMask);
	 }

	if ( vo_config_count ) glXDestroyContext( mDisplay,wsGLXContext );
    wsGLXContext=glXCreateContext( mDisplay,vinfo,NULL,True );
    glXMakeCurrent( mDisplay,vo_window,wsGLXContext );

	XSync(mDisplay, False);

	vo_x11_selectinput_witherr(mDisplay, vo_window, StructureNotifyMask | KeyPressMask | PointerMotionMask
		     | ButtonPressMask | ButtonReleaseMask | ExposureMask
        );

  texture_width=32;
  while(texture_width<image_width || texture_width<image_height) texture_width*=2;
  texture_height=texture_width;

  ImageData=malloc(texture_width*texture_height*image_bytes);
  memset(ImageData,128,texture_width*texture_height*image_bytes);

  glDisable(GL_BLEND); 
  glDisable(GL_DEPTH_TEST);
  glDepthMask(GL_FALSE);
  glDisable(GL_CULL_FACE);

  glEnable(GL_TEXTURE_2D);

  // set alignment as default is 4 which will break some files
  if ((image_width * image_bytes) % 8 == 0)
    gl_alignment=8;
  else if ((image_width * image_bytes) % 4 == 0)
    gl_alignment=4;
  else if ((image_width * image_bytes) % 2 == 0)
    gl_alignment=2;
  else
    gl_alignment=1;
  glPixelStorei (GL_UNPACK_ALIGNMENT, gl_alignment);

  mp_msg(MSGT_VO, MSGL_V, "[gl] Creating %dx%d texture...\n",texture_width,texture_height);

#if 1
//  glBindTexture(GL_TEXTURE_2D, texture_id);
  glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexImage2D(GL_TEXTURE_2D, 0, gl_texfmt, texture_width, texture_height, 0,
       gl_format, gl_type, ImageData);
#endif

  free (ImageData);

  resize(d_width,d_height);

  glClearColor( 1.0f,0.0f,1.0f,0.0f );
  glClear( GL_COLOR_BUFFER_BIT );

//  printf("OpenGL setup OK!\n");

      if (vo_ontop) vo_x11_setlayer(mDisplay, vo_window, vo_ontop);

	return 0;
}

static void check_events(void)
{
    int e=vo_x11_check_events(mDisplay);
    if(e&VO_EVENT_RESIZE) resize(vo_dwidth,vo_dheight);
    if(e&VO_EVENT_EXPOSE && int_pause) flip_page();
}

/**
 * Creates the textures and the display list needed for displaying
 * an OSD part.
 * Callback function for vo_draw_text().
 */
static void create_osd_texture(int x0, int y0, int w, int h,
                                 unsigned char *src, unsigned char *srca,
                                 int stride)
{
  int sx = 1, sy = 1;
  GLfloat xcov, ycov;
  char *clearTexture;
  while (sx < w) sx *= 2;
  while (sy < h) sy *= 2;
  xcov = (GLfloat) w / (GLfloat) sx;
  ycov = (GLfloat) h / (GLfloat) sy;

  if (osdtexCnt >= MAX_OSD_PARTS) {
    mp_msg(MSGT_VO, MSGL_ERR, "Too many OSD parts, contact the developers!\n");
    return;
  }
  clearTexture = malloc(sx * sy);
  memset(clearTexture, 0, sx * sy);

  // create Textures for OSD part
  glGenTextures(1, &osdtex[osdtexCnt]);
  glBindTexture(GL_TEXTURE_2D, osdtex[osdtexCnt]);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_LUMINANCE, sx, sy, 0,
                 GL_LUMINANCE, GL_UNSIGNED_BYTE, clearTexture);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glPixelStorei(GL_UNPACK_ROW_LENGTH, stride);
  glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, w, h, GL_LUMINANCE,
                    GL_UNSIGNED_BYTE, src);
  glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);

#ifndef FAST_OSD
  glGenTextures(1, &osdatex[osdtexCnt]);
  glBindTexture(GL_TEXTURE_2D, osdatex[osdtexCnt]);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_ALPHA, sx, sy, 0,
                 GL_LUMINANCE, GL_UNSIGNED_BYTE, clearTexture);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glPixelStorei(GL_UNPACK_ROW_LENGTH, stride);
  glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, w, h, GL_ALPHA,
                    GL_UNSIGNED_BYTE, srca);
  glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
#endif

  glBindTexture(GL_TEXTURE_2D, 0);
  free(clearTexture);

  // Create a list for rendering this OSD part
  osdDispList[osdtexCnt] = glGenLists(1);
  glNewList(osdDispList[osdtexCnt], GL_COMPILE);
#ifndef FAST_OSD
  // render alpha
  glBlendFunc(GL_ZERO, GL_SRC_ALPHA);
  glBindTexture(GL_TEXTURE_2D, osdatex[osdtexCnt]);
  glBegin(GL_QUADS);
  glTexCoord2f (0, 0);
  glVertex2f (x0, y0);
  glTexCoord2f (0, ycov);
  glVertex2f (x0, y0 + h);
  glTexCoord2f (xcov, ycov);
  glVertex2f (x0 + w, y0 + h);
  glTexCoord2f (xcov, 0);
  glVertex2f (x0 + w, y0);
  glEnd();
#endif
  // render OSD
  glBlendFunc (GL_ONE, GL_ONE);
  glBindTexture(GL_TEXTURE_2D, osdtex[osdtexCnt]);
  glBegin(GL_QUADS);
  glTexCoord2f (0, 0);
  glVertex2f (x0, y0);
  glTexCoord2f (0, ycov);
  glVertex2f (x0, y0 + h);
  glTexCoord2f (xcov, ycov);
  glVertex2f (x0 + w, y0 + h);
  glTexCoord2f (xcov, 0);
  glVertex2f (x0 + w, y0);
  glEnd();
  glEndList();

  osdtexCnt++;
}

static void draw_osd(void)
{
  int i;
  if (!use_osd) return;
  if (vo_osd_changed(0)) {
    for (i = 0; i < osdtexCnt; i++) {
      glDeleteTextures(1, &osdtex[i]);
#ifndef FAST_OSD
      glDeleteTextures(1, &osdatex[i]);
#endif
      glDeleteLists(osdDispList[i], 1);
    }
    osdtexCnt = 0;
    // draw OSD with full resolution
    vo_draw_text(vo_dwidth, vo_dheight, create_osd_texture);
  }
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

  if (osdtexCnt > 0) {
    // set special rendering parameters
    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    glLoadIdentity();
    glOrtho(0, vo_dwidth, vo_dheight, 0, -1, 1);
    glEnable(GL_BLEND);
    // draw OSD
    glCallLists(osdtexCnt, GL_UNSIGNED_INT, osdDispList);
    // set rendering parameters back to defaults
    glDisable (GL_BLEND);
    glPopMatrix();
    glBindTexture(GL_TEXTURE_2D, 0);
  }

//  glFlush();
  glFinish();
  glXSwapBuffers( mDisplay,vo_window );
 
}

//static inline uint32_t draw_slice_x11(uint8_t *src[], uint32_t slice_num)
static uint32_t draw_slice(uint8_t *src[], int stride[], int w,int h,int x,int y)
{
	return 0;
}


static uint32_t
draw_frame(uint8_t *src[])
{
unsigned int i;
uint8_t *ImageData=src[0];

  if (slice_height == 0)
    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, image_width, image_height,
		    gl_format, gl_type, ImageData);
  else
    for(i=0;i<image_height;i+=slice_height){
      glTexSubImage2D( GL_TEXTURE_2D,  // target
		       0,              // level
		       0,              // x offset
//		       image_height-1-i,  // y offset
		       i,  // y offset
		       image_width,    // width
		       (i+slice_height<=image_height)?slice_height:image_height-i,              // height
		       gl_format,
		       gl_type,
		       ImageData+i*image_bytes*image_width );        // *pixels
    }

	return 0; 
}

static uint32_t
query_format(uint32_t format)
{
    int caps = VFCAP_CSP_SUPPORTED | VFCAP_CSP_SUPPORTED_BY_HW;
    if (use_osd)
      caps |= VFCAP_OSD;
    if ((format == IMGFMT_RGB24) || (format == IMGFMT_RGBA))
        return caps;
    if (many_fmts && find_gl_format(format))
        return caps;
    return 0;
}


static void
uninit(void)
{
  if ( !vo_config_count ) return;
  vo_x11_uninit();
}

static uint32_t preinit(const char *arg)
{
    int parse_err = 0;
    unsigned int parse_pos = 0;
    many_fmts = 0;
    use_osd = 1;
    slice_height = 4;
    if(arg) 
    {
        while (arg[parse_pos] && !parse_err) {
            if (strncmp (&arg[parse_pos], "manyfmts", 8) == 0) {
                parse_pos += 8;
                many_fmts = 1;
            } else if (strncmp (&arg[parse_pos], "nomanyfmts", 10) == 0) {
                parse_pos += 10;
                many_fmts = 0;
            } else if (strncmp (&arg[parse_pos], "osd", 3) == 0) {
                parse_pos += 3;
                use_osd = 1;
            } else if (strncmp (&arg[parse_pos], "noosd", 5) == 0) {
                parse_pos += 5;
                use_osd = 0;
            } else if (strncmp (&arg[parse_pos], "slice-height=", 13) == 0) {
                int val;
                char *end;
                parse_pos += 13;
                val = strtol(&arg[parse_pos], &end, 0);
                if (val < 0) parse_err = 1;
                else {
                  slice_height = val;
                  parse_pos = end - arg;
                }
            }
            if (arg[parse_pos] == ':') parse_pos++;
            else if (arg[parse_pos]) parse_err = 1;
        }
    }
    if (parse_err) {
      unsigned int i;
      mp_msg(MSGT_VO, MSGL_FATAL, "Could not parse arguments:\n%s\n", arg);
      for (i = 0; i < parse_pos; i++)
        mp_msg(MSGT_VO, MSGL_FATAL, " ");
      mp_msg(MSGT_VO, MSGL_FATAL, "^\n");
      mp_msg(MSGT_VO, MSGL_FATAL,
              "\n-vo gl command line help:\n"
              "Example: mplayer -vo gl:slice-height=4\n"
              "\nOptions:\n"
              "  manyfmts\n"
              "    Enable extended color formats for OpenGL 1.2 and later\n"
              "  slice-height=<0-...>\n"
              "    Slice size for texture transfer, 0 for whole image\n"
              "  noosd\n"
              "    Do not use OpenGL OSD code\n"
              "\n" );
      return -1;
    }
    if (many_fmts)
      mp_msg (MSGT_VO, MSGL_WARN, "[gl] using extended formats.\n"
               "Make sure you have OpenGL >= 1.2 and used corresponding "
               "headers for compiling!\n");
    mp_msg (MSGT_VO, MSGL_INFO, "[gl] Using %d as slice height "
             "(0 means image height).\n", slice_height);
    if( !vo_init() ) return -1; // Can't open X11

    return 0;
}

static uint32_t control(uint32_t request, void *data, ...)
{
  switch (request) {
  case VOCTRL_PAUSE: return (int_pause=1);
  case VOCTRL_RESUME: return (int_pause=0);
  case VOCTRL_QUERY_FORMAT:
    return query_format(*((uint32_t*)data));
  case VOCTRL_ONTOP:
    vo_x11_ontop();
    return VO_TRUE;
  case VOCTRL_FULLSCREEN:
    vo_x11_fullscreen();
    return VO_TRUE;
  }
  return VO_NOTIMPL;
}

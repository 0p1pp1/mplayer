#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <errno.h>

#include "mp_msg.h"
#include "config.h"
#include "subopt-helper.h"
#include "video_out.h"
#include "video_out_internal.h"
#include "font_load.h"
#include "sub.h"

#include <errno.h>

#include "gl_common.h"
#include "aspect.h"
#ifdef HAVE_NEW_GUI
#include "Gui/interface.h"
#endif

static vo_info_t info = 
{
	"X11 (OpenGL)",
	"gl",
	"Arpad Gereoffy <arpi@esp-team.scene.hu>",
	""
};

LIBVO_EXTERN(gl)

#ifdef GL_WIN32
static int gl_vinfo = 0;
static HGLRC gl_context = 0;
#else
static XVisualInfo *gl_vinfo = NULL;
static GLXContext gl_context = 0;
static int                  wsGLXAttrib[] = { GLX_RGBA,
                                       GLX_RED_SIZE,1,
                                       GLX_GREEN_SIZE,1,
                                       GLX_BLUE_SIZE,1,
                                       GLX_DOUBLEBUFFER,
                                       None };
#endif

static int use_osd;
static int scaled_osd;
//! How many parts the OSD may consist of at most
#define MAX_OSD_PARTS 20
//! Textures for OSD
static GLuint osdtex[MAX_OSD_PARTS];
#ifndef FAST_OSD
//! Alpha textures for OSD
static GLuint osdatex[MAX_OSD_PARTS];
#endif
//! Display lists that draw the OSD parts
static GLuint osdDispList[MAX_OSD_PARTS];
//! How many parts the OSD currently consists of
static int osdtexCnt = 0;

static int use_aspect;
static int use_rectangle;
static int err_shown;
static uint32_t image_width;
static uint32_t image_height;
static uint32_t image_bytes;
static int many_fmts;
static GLenum gl_target;
static GLenum gl_texfmt;
static GLenum gl_format;
static GLenum gl_type;
static int gl_buffersize;

static int int_pause;

static uint32_t texture_width;
static uint32_t texture_height;

static unsigned int slice_height = 1;

static void resize(int x,int y){
  mp_msg(MSGT_VO, MSGL_V, "[gl] Resize: %dx%d\n",x,y);
#ifndef GL_WIN32
  if (WinID >= 0) {
    int top = 0, left = 0, w = x, h = y;
    geometry(&top, &left, &w, &h, vo_screenwidth, vo_screenheight);
    glViewport(top, left, w, h);
  } else
#endif
  glViewport( 0, 0, x, y );

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  if (vo_fs && use_aspect) {
    int new_w, new_h;
    GLdouble scale_x, scale_y;
    aspect(&new_w, &new_h, A_ZOOM);
    panscan_calc();
    new_w += vo_panscan_x;
    new_h += vo_panscan_y;
    scale_x = (GLdouble) new_w / (GLdouble) x;
    scale_y = (GLdouble) new_h / (GLdouble) y;
    glScaled(scale_x, scale_y, 1);
  }
  glOrtho(0, image_width, image_height, 0, -1,1);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  if (!scaled_osd) {
#ifdef HAVE_FREETYPE
  // adjust font size to display size
  force_load_font = 1;
#endif
  vo_osd_changed(OSDTYPE_OSD);
  }
}

static void texSize(int w, int h, int *texw, int *texh) {
  if (use_rectangle) {
    *texw = w; *texh = h;
  } else {
    *texw = 32;
    while (*texw < w)
      *texw *= 2;
    *texh = 32;
    while (*texh < h)
      *texh *= 2;
  }
}

/**
 * \brief Initialize a (new or reused) OpenGL context.
 */
static int initGl(uint32_t d_width, uint32_t d_height) {
  unsigned char *ImageData = NULL;
  texSize(image_width, image_height, &texture_width, &texture_height);

  glDisable(GL_BLEND); 
  glDisable(GL_DEPTH_TEST);
  glDepthMask(GL_FALSE);
  glDisable(GL_CULL_FACE);
  glEnable(gl_target);

  mp_msg(MSGT_VO, MSGL_V, "[gl] Creating %dx%d texture...\n",
          texture_width, texture_height);

  glTexParameterf(gl_target, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameterf(gl_target, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  glAdjustAlignment(texture_width * image_bytes);
  ImageData = malloc(texture_width * texture_height * image_bytes);
  memset(ImageData, 0, texture_width * texture_height * image_bytes);
  glTexImage2D(gl_target, 0, gl_texfmt, texture_width, texture_height, 0,
       gl_format, gl_type, ImageData);
  free (ImageData);

  // set alignment as default is 4 which will break some files
  glAdjustAlignment(image_width * image_bytes);

  resize(d_width, d_height);

  glClearColor( 0.0f,0.0f,0.0f,0.0f );
  glClear( GL_COLOR_BUFFER_BIT );
  gl_buffersize = 0;
  err_shown = 0;
  return 1;
}

/* connect to server, create and map window,
 * allocate colors and (shared) memory
 */
static uint32_t 
config(uint32_t width, uint32_t height, uint32_t d_width, uint32_t d_height, uint32_t flags, char *title, uint32_t format)
{
	image_height = height;
	image_width = width;
	glFindFormat(format, &image_bytes, &gl_texfmt, &gl_format, &gl_type);
	image_bytes = (image_bytes + 7) / 8;

	int_pause = 0;

	panscan_init();
	aspect_save_orig(width,height);
	aspect_save_prescale(d_width,d_height);
	aspect_save_screenres(vo_screenwidth,vo_screenheight);

	aspect(&d_width,&d_height,A_NOZOOM);
	vo_dx = (vo_screenwidth - d_width) / 2;
	vo_dy = (vo_screenheight - d_height) / 2;
	geometry(&vo_dx, &vo_dy, &d_width, &d_height,
	          vo_screenwidth, vo_screenheight);
#ifdef X11_FULLSCREEN
//        if( flags&VOFLAG_FULLSCREEN ){ // (-fs)
//          aspect(&d_width,&d_height,A_ZOOM);
//        }
#endif
#ifdef HAVE_NEW_GUI
  if (use_gui) {
    // GUI creates and manages window for us
    vo_dwidth = d_width;
    vo_dheight= d_height;
    guiGetEvent(guiSetShVideo, 0);
    goto glconfig;
  }
#endif
#ifdef GL_WIN32
  o_dwidth = d_width;
  o_dheight = d_height;
  vo_fs = flags & VOFLAG_FULLSCREEN;
  vo_vm = flags & VOFLAG_MODESWITCHING;
  vo_dwidth = d_width;
  vo_dheight = d_height;
  if (!createRenderingContext())
    return -1;
#else
  if (WinID >= 0) {
    Window win_tmp;
    int int_tmp;
    vo_window = WinID ? (Window)WinID : mRootWin;
    goto glconfig;
  }
  if ( vo_window == None ) {
	unsigned int fg, bg;
	XSizeHints hint;
	XVisualInfo *vinfo;
	XEvent xev;

	vo_fs = VO_FALSE;

	hint.x = vo_dx;
	hint.y = vo_dy;
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



          vo_window = vo_x11_create_smooth_window(mDisplay, mRootWin, vinfo->visual, hint.x, hint.y, hint.width, hint.height,
			                          vinfo->depth, XCreateColormap(mDisplay, mRootWin, vinfo->visual, AllocNone));

      vo_x11_classhint( mDisplay,vo_window,"gl" );
      vo_hidecursor(mDisplay,vo_window);

//      if ( flags&VOFLAG_FULLSCREEN ) vo_x11_decoration( mDisplay,vo_window,0 );
	  XSelectInput(mDisplay, vo_window, StructureNotifyMask);
	  /* Tell other applications about this window */
	  XSetStandardProperties(mDisplay, vo_window, title, title, None, NULL, 0, &hint);
	  /* Map window. */
	  XMapWindow(mDisplay, vo_window);
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

	XSync(mDisplay, False);

	vo_x11_selectinput_witherr(mDisplay, vo_window, StructureNotifyMask | KeyPressMask | PointerMotionMask
		     | ButtonPressMask | ButtonReleaseMask | ExposureMask
        );
  }
      if (vo_ontop) vo_x11_setlayer(mDisplay, vo_window, vo_ontop);

  vo_x11_nofs_sizepos(vo_dx, vo_dy, d_width, d_height);
  if (vo_fs ^ (flags & VOFLAG_FULLSCREEN))
    vo_x11_fullscreen();
#endif

glconfig:
  setGlWindow(&gl_vinfo, &gl_context, vo_window);
  initGl(vo_dwidth, vo_dheight);

	return 0;
}

static void check_events(void)
{
#ifdef GL_WIN32
    int e=vo_w32_check_events();
#else
    int e=vo_x11_check_events(mDisplay);
#endif
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
  int i;
  // initialize to 8 to avoid special-casing on alignment
  int sx = 8, sy = 8;
  GLfloat xcov, ycov;
  GLint scale_type = (scaled_osd) ? GL_LINEAR : GL_NEAREST;
  char *clearTexture;
  texSize(w, h, &sx, &sy);
  xcov = (GLfloat) w / (GLfloat) sx;
  ycov = (GLfloat) h / (GLfloat) sy;
  if (use_rectangle == 1) {
    xcov = w;
    ycov = h;
  }

  if (osdtexCnt >= MAX_OSD_PARTS) {
    mp_msg(MSGT_VO, MSGL_ERR, "Too many OSD parts, contact the developers!\n");
    return;
  }
  clearTexture = malloc(sx * sy);
  memset(clearTexture, 0, sx * sy);

  // create Textures for OSD part
  glAdjustAlignment(stride);
  glPixelStorei(GL_UNPACK_ROW_LENGTH, stride);
  glGenTextures(1, &osdtex[osdtexCnt]);
  BindTexture(gl_target, osdtex[osdtexCnt]);
  glTexImage2D(gl_target, 0, GL_LUMINANCE, sx, sy, 0,
                 GL_LUMINANCE, GL_UNSIGNED_BYTE, clearTexture);
  glTexParameteri(gl_target, GL_TEXTURE_MIN_FILTER, scale_type);
  glTexParameteri(gl_target, GL_TEXTURE_MAG_FILTER, scale_type);
  glTexSubImage2D(gl_target, 0, 0, 0, w, h, GL_LUMINANCE,
                    GL_UNSIGNED_BYTE, src);

#ifndef FAST_OSD
  glGenTextures(1, &osdatex[osdtexCnt]);
  BindTexture(gl_target, osdatex[osdtexCnt]);
  glTexImage2D(gl_target, 0, GL_ALPHA, sx, sy, 0,
                 GL_LUMINANCE, GL_UNSIGNED_BYTE, clearTexture);
  glTexParameteri(gl_target, GL_TEXTURE_MIN_FILTER, scale_type);
  glTexParameteri(gl_target, GL_TEXTURE_MAG_FILTER, scale_type);
  for (i = 0; i < h * stride; i++)
    clearTexture[i] = ~(-srca[i]);
  glTexSubImage2D(gl_target, 0, 0, 0, w, h, GL_ALPHA,
                    GL_UNSIGNED_BYTE, clearTexture);
#endif
  glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
  glAdjustAlignment(image_width * image_bytes);

  BindTexture(gl_target, 0);
  free(clearTexture);

  // Create a list for rendering this OSD part
  osdDispList[osdtexCnt] = glGenLists(1);
  glNewList(osdDispList[osdtexCnt], GL_COMPILE);
#ifndef FAST_OSD
  // render alpha
  glBlendFunc(GL_ZERO, GL_SRC_ALPHA);
  BindTexture(gl_target, osdatex[osdtexCnt]);
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
  BindTexture(gl_target, osdtex[osdtexCnt]);
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
  int osd_h, osd_w;
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

    osd_w = (scaled_osd) ? image_width : vo_dwidth;
    osd_h = (scaled_osd) ? image_height : vo_dheight;
    vo_draw_text(osd_w, osd_h, create_osd_texture);
  }
}

static void
flip_page(void)
{
  int tc_x = 1, tc_y = 1;
  if (use_rectangle == 1) {
    tc_x = texture_width;
    tc_y = texture_height;
  }

//  glEnable(GL_TEXTURE_2D);
//  glBindTexture(GL_TEXTURE_2D, texture_id);

  glColor3f(1,1,1);
  glBegin(GL_QUADS);
    glTexCoord2f(0,0);glVertex2i(0,0);
    glTexCoord2f(0,tc_y);glVertex2i(0,texture_height);
    glTexCoord2f(tc_x,tc_y);glVertex2i(texture_width,texture_height);
    glTexCoord2f(tc_x,0);glVertex2i(texture_width,0);
  glEnd();

  if (osdtexCnt > 0) {
    // set special rendering parameters
    if (!scaled_osd) {
    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    glLoadIdentity();
    glOrtho(0, vo_dwidth, vo_dheight, 0, -1, 1);
    }
    glEnable(GL_BLEND);
    // draw OSD
    glCallLists(osdtexCnt, GL_UNSIGNED_INT, osdDispList);
    // set rendering parameters back to defaults
    glDisable (GL_BLEND);
    if (!scaled_osd)
    glPopMatrix();
    BindTexture(gl_target, 0);
  }

//  glFlush();
  glFinish();
#ifdef GL_WIN32
  SwapBuffers(vo_hdc);
#else
  glXSwapBuffers( mDisplay,vo_window );
#endif
 
  if (vo_fs && use_aspect)
    glClear(GL_COLOR_BUFFER_BIT);
}

//static inline uint32_t draw_slice_x11(uint8_t *src[], uint32_t slice_num)
static uint32_t draw_slice(uint8_t *src[], int stride[], int w,int h,int x,int y)
{
	return 0;
}

static uint32_t get_image(mp_image_t *mpi) {
  if (!BindBuffer || !BufferData || !MapBuffer) {
    if (!err_shown)
      mp_msg(MSGT_VO, MSGL_ERR, "[gl] extensions missing for dr\n"
                                "Expect a _major_ speed penalty\n");
    err_shown = 1;
    return VO_FALSE;
  }
  if (mpi->flags & MP_IMGFLAG_READABLE) return VO_FALSE;
  if (mpi->type == MP_IMGTYPE_STATIC) return VO_FALSE;
  BindBuffer(GL_PIXEL_UNPACK_BUFFER, 1);
  mpi->stride[0] = mpi->width * mpi->bpp / 8;
  if (mpi->stride[0] * mpi->h > gl_buffersize) {
    BufferData(GL_PIXEL_UNPACK_BUFFER, mpi->stride[0] * mpi->h,
               NULL, GL_STREAM_DRAW);
    gl_buffersize = mpi->stride[0] * mpi->h;
  }
  mpi->planes[0] = MapBuffer(GL_PIXEL_UNPACK_BUFFER, GL_WRITE_ONLY);
  BindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
  if (mpi->planes[0] == NULL) {
    if (!err_shown)
      mp_msg(MSGT_VO, MSGL_ERR, "[gl] could not aquire buffer for dr\n"
                                "Expect a _major_ speed penalty\n");
    err_shown = 1;
    return VO_FALSE;
  }
  mpi->flags |= MP_IMGFLAG_DIRECT;
  return VO_TRUE;
}

static uint32_t draw_image(mp_image_t *mpi) {
  char *data = mpi->planes[0];
  int x = mpi->x;
  int y = mpi->y;
  int y_max = mpi->y + mpi->h;
  int h = slice_height ? slice_height : mpi->h;
  if (mpi->flags & MP_IMGFLAG_DRAW_CALLBACK)
    return VO_TRUE;
  if (mpi->flags & MP_IMGFLAG_DIRECT) {
    data = NULL;
    BindBuffer(GL_PIXEL_UNPACK_BUFFER, 1);
    UnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
    h = mpi->h; // always "upload" full texture
  }
  // this is not always correct, but should work for MPlayer
  glAdjustAlignment(mpi->stride[0]);
  glPixelStorei(GL_UNPACK_ROW_LENGTH, mpi->stride[0] / (mpi->bpp / 8));
  for (y = mpi->y; y + h <= y_max; y += h) {
    glTexSubImage2D(gl_target, 0, x, y,
                    mpi->w, h,
                    gl_format, gl_type,
                    data);
    data += mpi->stride[0] * h;
  }
  if (y < y_max)
    glTexSubImage2D(gl_target, 0, x, y, mpi->w, y_max - y,
                    gl_format, gl_type, data);
  if (mpi->flags & MP_IMGFLAG_DIRECT)
    BindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
  return VO_TRUE;
}

static uint32_t
draw_frame(uint8_t *src[])
{
  return VO_ERROR; 
}

static uint32_t
query_format(uint32_t format)
{
    int caps = VFCAP_CSP_SUPPORTED | VFCAP_CSP_SUPPORTED_BY_HW |
               VFCAP_HWSCALE_UP | VFCAP_HWSCALE_DOWN | VFCAP_ACCEPT_STRIDE;
    if (use_osd)
      caps |= VFCAP_OSD;
    if ((format == IMGFMT_RGB24) || (format == IMGFMT_RGBA))
        return caps;
    if (many_fmts &&
         glFindFormat(format, NULL, NULL, NULL, NULL))
        return caps;
    return 0;
}


static void
uninit(void)
{
  if ( !vo_config_count ) return;
  releaseGlContext(&gl_vinfo, &gl_context);
#ifdef GL_WIN32
  vo_w32_uninit();
#else
  vo_x11_uninit();
#endif
}

static opt_t subopts[] = {
  {"manyfmts",     OPT_ARG_BOOL, &many_fmts,    NULL},
  {"osd",          OPT_ARG_BOOL, &use_osd,      NULL},
  {"scaled-osd",   OPT_ARG_BOOL, &scaled_osd,   NULL},
  {"aspect",       OPT_ARG_BOOL, &use_aspect,   NULL},
  {"slice-height", OPT_ARG_INT,  &slice_height, (opt_test_f)int_non_neg},
  {"rectangle",    OPT_ARG_INT,  &use_rectangle,(opt_test_f)int_non_neg},
  {NULL}
};

static uint32_t preinit(const char *arg)
{
    // set defaults
    many_fmts = 1;
    use_osd = 1;
    scaled_osd = 0;
    use_aspect = 1;
    use_rectangle = 0;
    slice_height = 4;
    if (subopt_parse(arg, subopts) != 0) {
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
              "  noaspect\n"
              "    Do not do aspect scaling\n"
              "  rectangle=<0,1,2>\n"
              "    0: use power-of-two textures\n"
              "    1: use texture_rectangle\n"
              "    2: use texture_non_power_of_two\n"
              "\n" );
      return -1;
    }
    if (use_rectangle == 1)
      gl_target = GL_TEXTURE_RECTANGLE;
    else
      gl_target = GL_TEXTURE_2D;
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
  case VOCTRL_GET_IMAGE:
    return get_image(data);
  case VOCTRL_DRAW_IMAGE:
    return draw_image(data);
  case VOCTRL_GUISUPPORT:
    return VO_TRUE;
  case VOCTRL_ONTOP:
#ifdef GL_WIN32
    vo_w32_ontop();
#else
    vo_x11_ontop();
#endif
    return VO_TRUE;
  case VOCTRL_FULLSCREEN:
#ifdef GL_WIN32
    vo_w32_fullscreen();
    resize(vo_dwidth, vo_dheight);
#else
    vo_x11_fullscreen();
#endif
    return VO_TRUE;
  case VOCTRL_GET_PANSCAN:
    if (!use_aspect) return VO_NOTIMPL;
    return VO_TRUE;
  case VOCTRL_SET_PANSCAN:
    if (!use_aspect) return VO_NOTIMPL;
    resize (vo_dwidth, vo_dheight);
    return VO_TRUE;
  }
  return VO_NOTIMPL;
}

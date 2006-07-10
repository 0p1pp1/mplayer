#include "config.h"

#include <assert.h>
#include <math.h>
#include <stdint.h>
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_STROKER_H
#include FT_GLYPH_H
#include FT_SYNTHESIS_H

#include "mp_msg.h"

#include "ass.h"
#include "ass_cache.h"
#include "ass_utils.h"
#include "ass_fontconfig.h"

#include "libvo/sub.h" // for utf8_get_char

#define MAX_GLYPHS 1000
#define MAX_LINES 100

char *get_path(char *);

extern char *font_name;
#ifdef HAVE_FONTCONFIG
extern int font_fontconfig;
#else
static int font_fontconfig = 0;
#endif

struct ass_instance_s {
	FT_Library library;
	fc_instance_t* fontconfig_priv;
	ass_settings_t settings;

	ass_image_t* images_root; // rendering result is stored here
	int n_images;
	int max_images;
};

int no_more_font_messages = 0;  // don't print font warnings

typedef enum {EF_NONE = 0, EF_KARAOKE, EF_KARAOKE_KF, EF_KARAOKE_KO} effect_t;

// describes a glyph
// glyph_info_t and text_info_t are used for text centering and word-wrapping operations
typedef struct glyph_info_s {
	unsigned symbol;
	FT_Glyph glyph;
	FT_Glyph outline_glyph;
	FT_BBox bbox;
	FT_Vector pos;
	char linebreak; // the first (leading) glyph of some line ?
	uint32_t c1, c2, c3, c4; // colors
	char bitmap; // bool
	FT_Vector advance; // 26.6
	effect_t effect_type;
	int effect_timing;
	int asc, desc; // font max ascender and descender
//	int height;
	
	glyph_hash_key_t hash_key;
} glyph_info_t;

typedef struct line_info_s {
	int asc, desc;
} line_info_t;

typedef struct text_info_s {
	glyph_info_t* glyphs;
	int length;
	line_info_t lines[MAX_LINES];
	int n_lines;
	int height;
} text_info_t;


// Renderer state.
// Values like current font face, color, screen position, clipping and so on are stored here.
typedef struct render_context_s {
	ass_event_t* event;
	ass_style_t* style;
	
	FT_Face face;
	char* font_path;
	int font_size;
	
	FT_Stroker stroker;
	int alignment; // alignment overrides go here; if zero, style value will be used
	double rotation;
	enum {	EVENT_NORMAL, // "normal" top-, sub- or mid- title
		EVENT_POSITIONED // happens after pos(,), margins are ignored
		} evt_type;
	int pos_x, pos_y; // position
	int org_x, org_y; // origin
	double scale_x, scale_y;
	int hspacing; // distance between letters, in pixels
	double border; // outline width
	uint32_t c1, c2, c3, c4; // colors(Primary, Secondary, so on) in RGBA
	int clip_x0, clip_y0, clip_x1, clip_y1;
	char detect_collisions;

	effect_t effect_type;
	int effect_timing;

	// face properties
	char* family;
	unsigned bold;
	unsigned italic;
	
} render_context_t;

// frame-global data
typedef struct frame_context_s {
	ass_instance_t* ass_priv;
	int width, height; // screen dimensions
	int orig_height; // frame height ( = screen height - margins )
	ass_track_t* track;
	int add_bottom_margin; // additional margin, used to shift subtitles in case of collision
	int add_top_margin;
	long long time; // frame's timestamp, ms
	double font_scale_x; // x scale applied to all glyphs to preserve text aspect ratio
} frame_context_t;

static ass_instance_t* ass_instance;
static ass_settings_t* global_settings;
static text_info_t text_info;
static render_context_t render_context;
static frame_context_t frame_context;

static void ass_lazy_track_init(void)
{
	ass_track_t* track = frame_context.track;
	if (track->PlayResX && track->PlayResY)
		return;
	if (!track->PlayResX && !track->PlayResY) {
		mp_msg(MSGT_GLOBAL, MSGL_WARN, "Neither PlayResX nor PlayResY defined. Assuming 384x288.  \n");
		track->PlayResX = 384;
		track->PlayResY = 288;
	} else {
		double orig_aspect = (global_settings->aspect * frame_context.height) / frame_context.orig_height;
		if (!track->PlayResY) {
			track->PlayResY = track->PlayResX / orig_aspect + .5;
			mp_msg(MSGT_GLOBAL, MSGL_WARN, "PlayResY undefined, setting %d  \n", track->PlayResY);
		} else if (!track->PlayResX) {
			track->PlayResX = track->PlayResY * orig_aspect + .5;
			mp_msg(MSGT_GLOBAL, MSGL_WARN, "PlayResX undefined, setting %d  \n", track->PlayResX);
		}
	}
}

ass_instance_t* ass_init(void)
{
	char* family = 0;
	char* path = 0;
	char* fonts_path = 0;
	int error;
	fc_instance_t* fc_priv;
	FT_Library ft;
	ass_instance_t* priv;
	
	if (font_fontconfig && font_name)
		family = strdup(font_name);
	
	if (!font_fontconfig && font_name)
		path = strdup(font_name);
	else
		path = get_path("subfont.ttf");

	fonts_path = get_path("fonts");
	
	fc_priv = fontconfig_init(fonts_path, family, path);

	free(fonts_path);
	if (path) free(path);
	if (family) free(family);

	if (!fc_priv)
		return 0;
	
	error = FT_Init_FreeType( &ft );
	if ( error ) { 
		mp_msg(MSGT_GLOBAL, MSGL_FATAL, "FT_Init_FreeType failed\n");
		fontconfig_done(fc_priv);
		return 0;
	}

	priv = calloc(1, sizeof(ass_instance_t));
	if (!priv) {
		FT_Done_FreeType(ft);
		fontconfig_done(fc_priv);
		return 0;
	}
	priv->library = ft;
	priv->fontconfig_priv = fc_priv;
	// images_root and related stuff is zero-filled in calloc
	
	ass_face_cache_init();
	ass_glyph_cache_init();

	text_info.glyphs = calloc(MAX_GLYPHS, sizeof(glyph_info_t));
	
	return priv;
}

void ass_done(ass_instance_t* priv)
{
	ass_face_cache_done();
	ass_glyph_cache_done();
	if (priv && priv->library) FT_Done_FreeType(priv->library);
	if (priv && priv->fontconfig_priv) fontconfig_done(priv->fontconfig_priv);
	if (priv) free(priv);
	if (text_info.glyphs) free(text_info.glyphs);
}

/**
 * \brief Create a new ass_image_t and append it to images_root
 * Parameters are the same as ass_image_t fields.
 */
static void my_draw_bitmap(unsigned char* bitmap, int bitmap_w, int bitmap_h, int stride, int dst_x, int dst_y, uint32_t color)
{
	ass_instance_t* priv = ass_instance;
	ass_image_t* img;
	
	assert(priv->n_images <= priv->max_images);
	if (priv->n_images == priv->max_images) {
		if (!priv->max_images) priv->max_images = 100;
		else priv->max_images *= 2;
		priv->images_root = (ass_image_t*)realloc(priv->images_root, priv->max_images * sizeof(ass_image_t));
	}
	assert(priv->images_root);
	img = priv->images_root + priv->n_images;

	img->w = bitmap_w;
	img->h = bitmap_h;
	img->stride = stride;
	img->bitmap = bitmap;
	img->color = color;
	img->dst_x = dst_x;
	img->dst_y = dst_y;
	
	priv->n_images++;
}

/**
 * \brief convert bitmap glyph into ass_image_t struct(s)
 * \param bit freetype bitmap glyph, FT_PIXEL_MODE_GRAY
 * \param dst_x bitmap x coordinate in video frame
 * \param dst_y bitmap y coordinate in video frame
 * \param color first color, RGBA
 * \param color2 second color, RGBA
 * \param brk x coordinate relative to glyph origin, color is used to the left of brk, color2 - to the right
 * Performs clipping. Uses my_draw_bitmap for actual bitmap convertion.
 */
static int render_glyph(FT_BitmapGlyph bit, int dst_x, int dst_y, uint32_t color, uint32_t color2, int brk)
{
	// brk is relative to dst_x
	// color = color left of brk
	// color2 = color right of brk
	int b_x0, b_y0, b_x1, b_y1; // visible part of the bitmap
	int clip_x0, clip_y0, clip_x1, clip_y1;
	int tmp;
	FT_Bitmap* bitmap;

	bitmap = &(bit->bitmap);
	dst_x += bit->left;
	dst_y -= bit->top;
	brk -= bit->left;
	
	if (bitmap->pixel_mode != FT_PIXEL_MODE_GRAY) {
		mp_msg(MSGT_GLOBAL, MSGL_WARN, "Unsupported pixel mode: %d\n", (int)(bitmap->pixel_mode));
		return 1;
	}

	// clipping
	clip_x0 = render_context.clip_x0;
	clip_y0 = render_context.clip_y0;
	clip_x1 = render_context.clip_x1;
	clip_y1 = render_context.clip_y1;
	b_x0 = 0;
	b_y0 = 0;
	b_x1 = bitmap->width;
	b_y1 = bitmap->rows;
	
	tmp = dst_x - clip_x0;
	if (tmp < 0) {
		mp_msg(MSGT_GLOBAL, MSGL_DBG2, "clip left\n");
		b_x0 = - tmp;
	}
	tmp = dst_y - clip_y0;
	if (tmp < 0) {
		mp_msg(MSGT_GLOBAL, MSGL_DBG2, "clip top\n");
		b_y0 = - tmp;
	}
	tmp = clip_x1 - dst_x - bitmap->width;
	if (tmp < 0) {
		mp_msg(MSGT_GLOBAL, MSGL_DBG2, "clip right\n");
		b_x1 = bitmap->width + tmp;
	}
	tmp = clip_y1 - dst_y - bitmap->rows;
	if (tmp < 0) {
		mp_msg(MSGT_GLOBAL, MSGL_DBG2, "clip bottom\n");
		b_y1 = bitmap->rows + tmp;
	}
	
	if ((b_y0 >= b_y1) || (b_x0 >= b_x1))
		return 0;

	if (brk > b_x0) { // draw left part
		if (brk > b_x1) brk = b_x1;
		my_draw_bitmap(bitmap->buffer + bitmap->pitch * b_y0 + b_x0, 
			brk - b_x0, b_y1 - b_y0, bitmap->pitch,
			dst_x + b_x0, dst_y + b_y0, color);
	}
	if (brk < b_x1) { // draw right part
		if (brk < b_x0) brk = b_x0;
		my_draw_bitmap(bitmap->buffer + bitmap->pitch * b_y0 + brk, 
			b_x1 - brk, b_y1 - b_y0, bitmap->pitch,
			dst_x + brk, dst_y + b_y0, color2);
		
	}
	return 0;
}

/**
 * \brief Render text_info_t struct into ass_images_t list
 * Rasterize glyphs and put them in glyph cache.
 */
static int render_text(text_info_t* text_info, int dst_x, int dst_y)
{
	int pen_x, pen_y;
	int error, error2;
	int i;
	FT_Glyph image;
	FT_BitmapGlyph bit;
	glyph_hash_val_t hash_val;

	for (i = 0; i < text_info->length; ++i) {
		if (text_info->glyphs[i].bitmap != 1) {
			if ((text_info->glyphs[i].symbol == '\n') || (text_info->glyphs[i].symbol == 0))
				continue;
			error = FT_Glyph_To_Bitmap( &(text_info->glyphs[i].outline_glyph), FT_RENDER_MODE_NORMAL, 0, 1);
			error2 = FT_Glyph_To_Bitmap( &(text_info->glyphs[i].glyph), FT_RENDER_MODE_NORMAL, 0, 1);

			if (error || error2) {
				FT_Done_Glyph(text_info->glyphs[i].outline_glyph);
				FT_Done_Glyph(text_info->glyphs[i].glyph);
				mp_msg(MSGT_GLOBAL, MSGL_WARN, "FT_Glyph_To_Bitmap error %d %d, symbol %d, index %d\n", 
						error, error2, text_info->glyphs[i].symbol, text_info->glyphs[i].hash_key.index);
				text_info->glyphs[i].symbol = 0; // do not render
				continue;
			}
			// cache
			text_info->glyphs[i].hash_key.bitmap = 1; // other hash_key fields were set in get_glyph()
			hash_val.bbox_scaled = text_info->glyphs[i].bbox;
			hash_val.outline_glyph = text_info->glyphs[i].outline_glyph;
			hash_val.glyph = text_info->glyphs[i].glyph;
			hash_val.advance.x = text_info->glyphs[i].advance.x;
			hash_val.advance.y = text_info->glyphs[i].advance.y;
			cache_add_glyph(&(text_info->glyphs[i].hash_key), &hash_val);
		}
	}

	for (i = 0; i < text_info->length; ++i) {
		glyph_info_t* info = text_info->glyphs + i;
		if ((info->symbol == 0) || (info->symbol == '\n'))
			continue;

		pen_x = dst_x + info->pos.x;
		pen_y = dst_y + info->pos.y;
		image = info->outline_glyph;
		bit = (FT_BitmapGlyph)image;
		
		if ((info->effect_type == EF_KARAOKE_KO) && (info->effect_timing <= info->bbox.xMax)) {
			// do nothing
		} else
			render_glyph(bit, pen_x, pen_y, info->c3, 0, 1000000);
	}
	for (i = 0; i < text_info->length; ++i) {
		glyph_info_t* info = text_info->glyphs + i;
		if ((info->symbol == 0) || (info->symbol == '\n'))
			continue;

		pen_x = dst_x + info->pos.x;
		pen_y = dst_y + info->pos.y;
		image = info->glyph;
		bit = (FT_BitmapGlyph)image;

		if ((info->effect_type == EF_KARAOKE) || (info->effect_type == EF_KARAOKE_KO)) {
			if (info->effect_timing > info->bbox.xMax)
				render_glyph(bit, pen_x, pen_y, info->c1, 0, 1000000);
			else
				render_glyph(bit, pen_x, pen_y, info->c2, 0, 1000000);
		} else if (info->effect_type == EF_KARAOKE_KF) {
			render_glyph(bit, pen_x, pen_y, info->c1, info->c2, info->effect_timing);
		} else
			render_glyph(bit, pen_x, pen_y, info->c1, 0, 1000000);
	}

	return 0;
}

/**
 * \brief Mapping between script and screen coordinates
 */
static int x2scr(int x) {
	return x*frame_context.width / frame_context.track->PlayResX;
}
/**
 * \brief Mapping between script and screen coordinates
 */
static int y2scr(int y) {
	return y * frame_context.orig_height / frame_context.track->PlayResY + global_settings->top_margin;
}
// the same for toptitles
static int y2scr_top(int y) {
	return y * frame_context.orig_height / frame_context.track->PlayResY;
}
// the same for subtitles
static int y2scr_sub(int y) {
	return y * frame_context.orig_height / frame_context.track->PlayResY + global_settings->top_margin + global_settings->bottom_margin;
}

static void vmirror_bbox(FT_BBox* orig, FT_BBox* pbbox) {
	pbbox->xMin = orig->xMin;
	pbbox->xMax = orig->xMax;
	pbbox->yMin = - orig->yMax;
	pbbox->yMax = - orig->yMin;
}

static void compute_string_bbox( text_info_t* info, FT_BBox *abbox ) {
	FT_BBox bbox;
	int n;
	
	/* initialize string bbox to "empty" values */
	bbox.xMin = bbox.yMin = 32000;
	bbox.xMax = bbox.yMax = -32000;
	
	/* for each glyph image, compute its bounding box, */
	/* translate it, and grow the string bbox */
	for ( n = 0; n < info->length; n++ ) {
		FT_BBox glyph_bbox;
		vmirror_bbox( &(info->glyphs[n].bbox), &glyph_bbox );
		glyph_bbox.xMin += info->glyphs[n].pos.x;
		glyph_bbox.xMax += info->glyphs[n].pos.x;
		glyph_bbox.yMin += info->glyphs[n].pos.y;
		glyph_bbox.yMax += info->glyphs[n].pos.y;
		if ( glyph_bbox.xMin < bbox.xMin ) bbox.xMin = glyph_bbox.xMin;
		if ( glyph_bbox.yMin < bbox.yMin ) bbox.yMin = glyph_bbox.yMin;
		if ( glyph_bbox.xMax > bbox.xMax ) bbox.xMax = glyph_bbox.xMax;
		if ( glyph_bbox.yMax > bbox.yMax ) bbox.yMax = glyph_bbox.yMax;
	}
	
	/* check that we really grew the string bbox */
	if ( bbox.xMin > bbox.xMax ) {
		bbox.xMin = 0;
		bbox.yMin = 0;
		bbox.xMax = 0;
		bbox.yMax = 0;
	}

	/* return string bbox */
	*abbox = bbox;
}


/**
 * \brief Check if starting part of (*p) matches sample. If true, shift p to the first symbol after the matching part.
 */
static inline int mystrcmp(char** p, char* sample) {
	int len = strlen(sample);
	if (strncmp(*p, sample, len) == 0) {
		(*p) += len;
		return 1;
	} else
		return 0;
}

static void change_font_size(int sz)
{
	double size = (double)sz * global_settings->font_size_coeff * 0.8;
	size *= frame_context.height;
	size /= frame_context.track->PlayResY;

	if (size < 1)
		size = 1;
	else if (size > frame_context.height * 2)
		size = frame_context.height * 2;
	
	FT_Set_Pixel_Sizes(render_context.face, 0, size);

	render_context.font_size = sz;
}

/**
 * \brief Change current font, using setting from render_context.
 */
static void update_font(void)
{
	int error;
	unsigned val;
	ass_instance_t* priv = frame_context.ass_priv;
	face_desc_t desc;
	desc.family = strdup(render_context.family);

	val = render_context.bold;
	// 0 = normal, 1 = bold, >1 = exact weight
	if (val == 0) val = 80; // normal
	else if (val == 1) val = 200; // bold
	desc.bold = val;

	val = render_context.italic;
	if (val == 0) val = 0; // normal
	else if (val == 1) val = 110; //italic
	desc.italic = val;

	error = ass_new_face(priv->library, priv->fontconfig_priv, &desc, &(render_context.face));
	if (error) {
		render_context.face = 0;
	}
	
	if (render_context.face)
	{
		change_font_size(render_context.font_size);
	}
}

/**
 * \brief Change border width
 */
static void change_border(double border)
{
	if (!render_context.stroker) {
		if (!no_more_font_messages)
			mp_msg(MSGT_GLOBAL, MSGL_WARN, "No stroker!\n");
	} else {
		render_context.border = border;
		FT_Stroker_Set( render_context.stroker,
				(int)(64 * border),
				FT_STROKER_LINECAP_ROUND,
				FT_STROKER_LINEJOIN_ROUND,
				0 );
	}
}

#define _r(c)  ((c)>>24)
#define _g(c)  (((c)>>16)&0xFF)
#define _b(c)  (((c)>>8)&0xFF)
#define _a(c)  ((c)&0xFF)

/**
 * \brief Calculate a weighted average of two colors
 * calculates c1*(1-a) + c2*a, but separately for each component except alpha
 */
static void change_color(uint32_t* var, uint32_t new, double pwr)
{
	(*var)= ((uint32_t)(_r(*var) * (1 - pwr) + _r(new) * pwr) << 24) +
		((uint32_t)(_g(*var) * (1 - pwr) + _g(new) * pwr) << 16) +
		((uint32_t)(_b(*var) * (1 - pwr) + _b(new) * pwr) << 8) +
		_a(*var);
}

// like change_color, but for alpha component only
static void change_alpha(uint32_t* var, uint32_t new, double pwr)
{
	*var = (_r(*var) << 24) + (_g(*var) << 16) + (_b(*var) << 8) + (_a(*var) * (1 - pwr) + _a(new) * pwr);
}


/**
 * \brief Calculate alpha value by piecewise linear function
 * Used for \fad, \fade implementation.
 */
static void interpolate_alpha(long long now, 
		long long t1, long long t2, long long t3, long long t4,
		unsigned a1, unsigned a2, unsigned a3)
{
	unsigned a;
	double cf;
	if (now <= t1) {
		a = a1;
	} else if (now >= t4) {
		a = a3;
	} else if (now < t2) { // and > t1
		cf = ((double)(now - t1)) / (t2 - t1);
		a = a1 * (1 - cf) + a2 * cf;
	} else if (now > t3) {
		cf = ((double)(now - t3)) / (t4 - t3);
		a = a2 * (1 - cf) + a3 * cf;
	} else { // t2 <= now <= t3
		a = a2;
	}


	change_alpha(&render_context.c1, a, 1.);
	change_alpha(&render_context.c2, a, 1.);
	change_alpha(&render_context.c3, a, 1.);
	change_alpha(&render_context.c4, a, 1.);
}

/**
 * \brief Parse style override tag.
 * \param p string to parse
 * \param pwr multiplier for some tag effects (comes from \t tags)
 */
static char* parse_tag(char* p, double pwr) {
#define skip_all(x) if (*p == (x)) ++p; else { \
	while ((*p != (x)) && (*p != '}') && (*p != 0)) {++p;} }
#define skip(x) if (*p == (x)) ++p; else { return p; }
	
	skip_all('\\');
	if ((*p == '}') || (*p == 0))
		return p;

	if (mystrcmp(&p, "fsc")) {
		char tp = *p++;
		double val;
		if (tp == 'x') {
			if (mystrtod(&p, &val)) {
				val /= 100;
				render_context.scale_x = (val - 1.) * pwr + 1.;
			} else
				render_context.scale_x = render_context.style->ScaleX;
		} else if (tp == 'y') {
			if (mystrtod(&p, &val)) {
				val /= 100;
				render_context.scale_y = (val - 1.) * pwr + 1.;
			} else
				render_context.scale_y = render_context.style->ScaleY;
		}
	} else if (mystrcmp(&p, "fsp")) {
		int val;
		if (mystrtoi(&p, 10, &val))
			render_context.hspacing = val * pwr;
		else
			render_context.hspacing = 0;
	} else if (mystrcmp(&p, "fs")) {
		int val;
		if (mystrtoi(&p, 10, &val))
			val = render_context.font_size * ( 1 - pwr ) + val * pwr;
		else
			val = render_context.style->FontSize;
		if (render_context.face)
			change_font_size(val);
	} else if (mystrcmp(&p, "bord")) {
		double val;
		if (mystrtod(&p, &val))
			val = render_context.border * ( 1 - pwr ) + val * pwr;
		else
			val = (render_context.style->BorderStyle == 1) ? render_context.style->Outline : 1.;
		change_border(val);
	} else if (mystrcmp(&p, "move")) {
		int x1, x2, y1, y2;
		long long t1, delta_t, t;
		int x, y;
		double k;
		skip('(');
		x1 = strtol(p, &p, 10);
		skip(',');
		y1 = strtol(p, &p, 10);
		skip(',');
		x2 = strtol(p, &p, 10);
		skip(',');
		y2 = strtol(p, &p, 10);
		skip(')'); // FIXME: 2 more optional args
		mp_msg(MSGT_GLOBAL, MSGL_DBG2, "movement: (%d, %d) -> (%d, %d)\n", x1, y1, x2, y2);
		t1 = render_context.event->Start;
		delta_t = render_context.event->Duration;
		t = frame_context.time; // FIXME: move to render_context
		k = ((double)(t - t1)) / delta_t;
		x = k * (x2 - x1) + x1;
		y = k * (y2 - y1) + y1;
		render_context.pos_x = x;
		render_context.pos_y = y;
		render_context.detect_collisions = 0;
		render_context.evt_type = EVENT_POSITIONED;
	} else if (mystrcmp(&p, "frx") || mystrcmp(&p, "fry")) {
		int val;
		mystrtoi(&p, 10, &val);
		mp_msg(MSGT_GLOBAL, MSGL_V, "frx/fry unimplemented \n");
	} else if (mystrcmp(&p, "frz") || mystrcmp(&p, "fr")) {
		double angle;
		int val;
		val = strtol(p, &p, 10);
		mp_msg(MSGT_GLOBAL, MSGL_DBG2, "setting rotation to %.2f\n", val * pwr);
		angle = M_PI * val / 180;
		render_context.rotation = angle * pwr;
	} else if (mystrcmp(&p, "fn")) {
		char* start = p;
		char* family;
		skip_all('\\');
		family = malloc(p - start + 1);
		strncpy(family, start, p - start);
		family[p - start] = '\0';
		if (render_context.family)
			free(render_context.family);
		render_context.family = family;
		update_font();
	} else if (mystrcmp(&p, "alpha")) {
		uint32_t val;
		if (strtocolor(&p, &val)) {
			unsigned char a = val >> 24;
			change_alpha(&render_context.c1, a, pwr);
			change_alpha(&render_context.c2, a, pwr);
			change_alpha(&render_context.c3, a, pwr);
			change_alpha(&render_context.c4, a, pwr);
		} else {
			change_alpha(&render_context.c1, render_context.style->PrimaryColour, pwr);
			change_alpha(&render_context.c2, render_context.style->SecondaryColour, pwr);
			change_alpha(&render_context.c3, render_context.style->OutlineColour, pwr);
			change_alpha(&render_context.c4, render_context.style->BackColour, pwr);
		}
		// FIXME: simplify
	} else if (mystrcmp(&p, "an")) {
		int val = strtol(p, &p, 10);
		int v = (val - 1) / 3; // 0, 1 or 2 for vertical alignment
		mp_msg(MSGT_GLOBAL, MSGL_DBG2, "an %d\n", val);
		if (v != 0) v = 3 - v;
		val = ((val - 1) % 3) + 1; // horizontal alignment
		val += v*4;
		mp_msg(MSGT_GLOBAL, MSGL_DBG2, "align %d\n", val);
		render_context.alignment = val;
	} else if (mystrcmp(&p, "a")) {
		int val = strtol(p, &p, 10);
		render_context.alignment = val;
	} else if (mystrcmp(&p, "pos")) {
		int v1, v2;
		skip('(');
		v1 = strtol(p, &p, 10);
		skip(',');
		v2 = strtol(p, &p, 10);
		skip(')');
		mp_msg(MSGT_GLOBAL, MSGL_DBG2, "pos(%d, %d)\n", v1, v2);
		render_context.evt_type = EVENT_POSITIONED;
		render_context.detect_collisions = 0;
		render_context.pos_x = v1;
		render_context.pos_y = v2;
	} else if (mystrcmp(&p, "fade")) {
		int a1, a2, a3, v1, v2, v3, v4;
		skip('(');
		a1 = strtol(p, &p, 10);
		skip(',');
		a2 = strtol(p, &p, 10);
		skip(',');
		a3 = strtol(p, &p, 10);
		skip(',');
		v1 = strtol(p, &p, 10);
		skip(',');
		v2 = strtol(p, &p, 10);
		skip(',');
		v3 = strtol(p, &p, 10);
		skip(',');
		v4 = strtol(p, &p, 10);
		skip(')');
		interpolate_alpha(frame_context.time - render_context.event->Start, v1, v2, v3, v4, a1, a2, a3);
	} else if (mystrcmp(&p, "fad")) {
		int v1, v2;
		long long now, t1, t2, t3, t4;
		skip('(');
		v1 = strtol(p, &p, 10);
		skip(',');
		v2 = strtol(p, &p, 10);
		skip(')');
		now = frame_context.time;
		t1 = render_context.event->Start;
		t2 = t1 + v1;
		t4 = render_context.event->Start + render_context.event->Duration;
		t3 = t4 - v2;
		interpolate_alpha(now, t1, t2, t3, t4, 0xFF, 0, 0xFF);
	} else if (mystrcmp(&p, "org")) {
		int v1, v2;
		skip('(');
		v1 = strtol(p, &p, 10);
		skip(',');
		v2 = strtol(p, &p, 10);
		skip(')');
		mp_msg(MSGT_GLOBAL, MSGL_DBG2, "org(%d, %d)\n", v1, v2);
		//				render_context.evt_type = EVENT_POSITIONED;
		render_context.org_x = v1;
		render_context.org_y = v2;
	} else if (mystrcmp(&p, "t")) {
		double v[3];
		int v1, v2;
		double v3;
		int cnt;
		long long t1, t2, t, delta_t;
		double k;
		skip('(');
		for (cnt = 0; cnt < 3; ++cnt) {
			if (*p == '\\')
				break;
			v[cnt] = strtod(p, &p);
			skip(',');
		}
		if (cnt == 3) {
			v1 = v[0]; v2 = v[1]; v3 = v[2];
		} else if (cnt == 2) {
			v1 = v[0]; v2 = v[1]; v3 = 1.;
		} else if (cnt == 1) {
			v1 = 0; v2 = render_context.event->Duration; v3 = v[0];
		} else { // cnt == 0
			v1 = 0; v2 = render_context.event->Duration; v3 = 1.;
		}
		render_context.detect_collisions = 0;
		t1 = v1;
		t2 = v2;
		delta_t = v2 - v1;
		t = frame_context.time - render_context.event->Start; // FIXME: move to render_context
		if (t < t1)
			k = 0.;
		else if (t > t2)
			k = 1.;
		else k = ((double)(t - t1)) / delta_t;
		while (*p == '\\')
			p = parse_tag(p, k); // maybe k*pwr ? no, specs forbid nested \t's 
		skip_all(')'); // FIXME: better skip(')'), but much more tags support required
	} else if (mystrcmp(&p, "clip")) {
		int x0, y0, x1, y1;
		int res = 1;
		skip('(');
		res &= mystrtoi(&p, 10, &x0);
		skip(',');
		res &= mystrtoi(&p, 10, &y0);
		skip(',');
		res &= mystrtoi(&p, 10, &x1);
		skip(',');
		res &= mystrtoi(&p, 10, &y1);
		skip(')');
		if (res) {
			render_context.clip_x0 = render_context.clip_x0 * (1-pwr) + x0 * pwr;
			render_context.clip_x1 = render_context.clip_x1 * (1-pwr) + x1 * pwr;
			render_context.clip_y0 = render_context.clip_y0 * (1-pwr) + y0 * pwr;
			render_context.clip_y1 = render_context.clip_y1 * (1-pwr) + y1 * pwr;
		} else {
			render_context.clip_x0 = 0;
			render_context.clip_y0 = 0;
			render_context.clip_x1 = frame_context.track->PlayResX;
			render_context.clip_y1 = frame_context.track->PlayResY;
		}
	} else if (mystrcmp(&p, "c")) {
		uint32_t val;
		if (!strtocolor(&p, &val))
			val = render_context.style->PrimaryColour;
		mp_msg(MSGT_GLOBAL, MSGL_DBG2, "color: %X\n", val);
		change_color(&render_context.c1, val, pwr);
	} else if ((*p >= '1') && (*p <= '4') && (++p) && (mystrcmp(&p, "c") || mystrcmp(&p, "a"))) {
		char n = *(p-2);
		char cmd = *(p-1);
		uint32_t* pcolor;
		uint32_t val;
		assert((n >= '1') && (n <= '4'));
		if (!strtocolor(&p, &val))
			switch(n) {
				case '1': val = render_context.style->PrimaryColour; break;
				case '2': val = render_context.style->SecondaryColour; break;
				case '3': val = render_context.style->OutlineColour; break;
				case '4': val = render_context.style->BackColour; break;
				default : val = 0; break; // impossible due to assert; avoid compilation warning
			}
		switch (n) {
			case '1': pcolor = &render_context.c1; break;
			case '2': pcolor = &render_context.c2; break;
			case '3': pcolor = &render_context.c3; break;
			case '4': pcolor = &render_context.c4; break;
			default : pcolor = 0; break;
		}
		switch (cmd) {
			case 'c': change_color(pcolor, val, pwr); break;
			case 'a': change_alpha(pcolor, val >> 24, pwr); break;
			default: mp_msg(MSGT_GLOBAL, MSGL_WARN, "Bad command: %c%c\n", n, cmd); break;
		}
		mp_msg(MSGT_GLOBAL, MSGL_DBG2, "single c/a at %f: %c%c = %X   \n", pwr, n, cmd, *pcolor);
	} else if (mystrcmp(&p, "r")) {
		render_context.c1 = render_context.style->PrimaryColour;
		render_context.c2 = render_context.style->SecondaryColour;
		render_context.c3 = render_context.style->OutlineColour;
		render_context.c4 = render_context.style->BackColour;
		render_context.font_size = render_context.style->FontSize;

		if (render_context.family)
			free(render_context.family);
		render_context.family = strdup(render_context.style->FontName);
		render_context.bold = - render_context.style->Bold;
		render_context.italic = - render_context.style->Italic;
		update_font();

		if (render_context.stroker) {
			double border = (render_context.style->BorderStyle == 1) ? render_context.style->Outline : 1.;
			change_border(border);
		}
		render_context.scale_x = render_context.style->ScaleX;
		render_context.scale_y = render_context.style->ScaleY;
		render_context.hspacing = 0; // FIXME
		
		// FIXME: does not reset unsupported attributes.
	} else if (mystrcmp(&p, "be")) {
		int val;
		mystrtoi(&p, 10, &val);
		mp_msg(MSGT_GLOBAL, MSGL_V, "be unimplemented \n");
	} else if (mystrcmp(&p, "b")) {
		int b;
		if (mystrtoi(&p, 10, &b))
			render_context.bold = b;
		else
			render_context.bold = - render_context.style->Bold;
		update_font();
	} else if (mystrcmp(&p, "i")) {
		int i;
		if (mystrtoi(&p, 10, &i))
			render_context.italic = i;
		else
			render_context.italic = - render_context.style->Italic;
		update_font();
	} else if (mystrcmp(&p, "kf") || mystrcmp(&p, "K")) {
		int val = strtol(p, &p, 10);
		render_context.effect_type = EF_KARAOKE_KF;
		render_context.effect_timing = val * 10;
	} else if (mystrcmp(&p, "ko")) {
		int val = strtol(p, &p, 10);
		render_context.effect_type = EF_KARAOKE_KO;
		render_context.effect_timing = val * 10;
	} else if (mystrcmp(&p, "k")) {
		int val = strtol(p, &p, 10);
		render_context.effect_type = EF_KARAOKE;
		render_context.effect_timing = val * 10;
	}

	return p;

#undef skip
#undef skip_all
}

/**
 * \brief Get next ucs4 char from string, parsing and executing style overrides
 * \param str string pointer
 * \return ucs4 code of the next char
 * On return str points to the unparsed part of the string
 */
static unsigned get_next_char(char** str)
{
	char* p = *str;
	unsigned chr;
	if (*p == '{') { // '\0' goes here
		p++;
		while (1) {
			p = parse_tag(p, 1.);
			if (*p == '}') { // end of tag
				p++;
				if (*p == '{') {
					p++;
					continue;
				} else
					break;
			} else if (*p != '\\')
				mp_msg(MSGT_GLOBAL, MSGL_V, "Unable to parse: \"%s\" \n", p);
			if (*p == 0)
				break;
		}
	}
	if (*p == '\t') {
		++p;
		*str = p;
		return ' ';
	}
	if (*p == '\\') {
		if ((*(p+1) == 'N') || ((*(p+1) == 'n') && (frame_context.track->WrapStyle == 2))) {
			p += 2;
			*str = p;
			return '\n';
		} else if (*(p+1) == 'n') {
			p += 2;
			*str = p;
			return ' ';
		}
	}
	chr = utf8_get_char(&p);
	*str = p;
	return chr;
}

/**
 * \brief Start new event. Reset render_context.
 */
static int init_render_context(ass_event_t* event)
{
	int error;

// init render_context
	render_context.event = event;
	render_context.style = frame_context.track->styles + event->Style;
	
	render_context.font_size = render_context.style->FontSize;
	render_context.evt_type = EVENT_NORMAL;
	render_context.alignment = 0;
	render_context.rotation = 0.;
	render_context.pos_x = 0;
	render_context.pos_y = 0;
	render_context.org_x = 0;
	render_context.org_y = 0;
	render_context.scale_x = render_context.style->ScaleX;
	render_context.scale_y = render_context.style->ScaleY;
	render_context.hspacing = 0;
	render_context.c1 = render_context.style->PrimaryColour;
	render_context.c2 = render_context.style->SecondaryColour;
	render_context.c3 = render_context.style->OutlineColour;
	render_context.c4 = render_context.style->BackColour;
	render_context.clip_x0 = 0;
	render_context.clip_y0 = 0;
	render_context.clip_x1 = frame_context.track->PlayResX;
	render_context.clip_y1 = frame_context.track->PlayResY;
	render_context.detect_collisions = 1;
	
	if (render_context.family)
		free(render_context.family);
	render_context.family = strdup(render_context.style->FontName);
	render_context.bold = - render_context.style->Bold; // style value for bold text is -1
	render_context.italic = - render_context.style->Italic;
	
	update_font();

	if (render_context.face) {
#if (FREETYPE_MAJOR > 2) || ((FREETYPE_MAJOR == 2) && (FREETYPE_MINOR > 1))
		error = FT_Stroker_New( ass_instance->library, &render_context.stroker );
#else // < 2.2
		error = FT_Stroker_New( render_context.face->memory, &render_context.stroker );
#endif
		if ( error ) {
			mp_msg(MSGT_GLOBAL, MSGL_V, "failed to get stroker\n");
			render_context.stroker = 0;
		} else {
			// FIXME: probably wrong when render_context.Border == 3
			double border = (render_context.style->BorderStyle == 1) ? render_context.style->Outline : 1.;
			change_border(border);
		}
	}
	
	return 0;
}

static int free_render_context(void)
{
	if (render_context.stroker != 0) {
		FT_Stroker_Done(render_context.stroker);
		render_context.stroker = 0;
	}
	return 0;
}

/**
 * \brief Get normal and outline glyphs from cache (if possible) or font face
 * \param index face glyph index
 * \param symbol ucs4 char
 * \param info out: struct filled with extracted data
 * \param advance advance vector of the extracted glyph
 * \return 0 on success
 */
static int get_glyph(int index, int symbol, glyph_info_t* info, FT_Vector* advance)
{
	int error;
	glyph_hash_val_t* val;
	glyph_hash_key_t* key = &(info->hash_key);
	
	key->face = render_context.face;
	key->size = render_context.font_size;
	key->index = index;
	key->outline = (render_context.border * 0xFFFF); // convert to 16.16
	key->scale_x = (render_context.scale_x * 0xFFFF);
	key->scale_y = (render_context.scale_y * 0xFFFF);
	key->angle = (render_context.rotation * 0xFFFF);
	key->advance = *advance;
	key->bold = render_context.bold;
	key->italic = render_context.italic;

	key->bitmap = 1; // looking for bitmap glyph

	
	val = cache_find_glyph(key);
//	val = 0;
	
	if (val) {
		// bitmap glyph found, no need for FT_Glyph_Copy
		info->glyph = val->glyph;
		info->outline_glyph = val->outline_glyph;
		info->bbox = val->bbox_scaled;
		info->advance.x = val->advance.x;
		info->advance.y = val->advance.y;
		info->bitmap = 1; // bitmap glyph

		return 0;
	}

	// not found, get a new outline glyph from face
//	mp_msg(MSGT_GLOBAL, MSGL_INFO, "miss, index = %d, symbol = %c, adv = (%d, %d)\n", index, symbol, advance->x, advance->y);
	
	error = FT_Load_Glyph(render_context.face, index, FT_LOAD_NO_BITMAP );
	if (error) {
		mp_msg(MSGT_GLOBAL, MSGL_WARN, "Error loading glyph\n");
		return error;
	}
	
#if (FREETYPE_MAJOR > 2) || \
    ((FREETYPE_MAJOR == 2) && (FREETYPE_MINOR >= 2)) || \
    ((FREETYPE_MAJOR == 2) && (FREETYPE_MINOR == 1) && (FREETYPE_PATCH >= 10))
// FreeType >= 2.1.10 required
	if (!(render_context.face->style_flags & FT_STYLE_FLAG_ITALIC) && 
			((render_context.italic == 1) || (render_context.italic > 55))) {
		FT_GlyphSlot_Oblique(render_context.face->glyph);
	}
#endif
	error = FT_Get_Glyph(render_context.face->glyph, &(info->glyph));
	if (error) {
		mp_msg(MSGT_GLOBAL, MSGL_WARN, "Error getting glyph\n");
		return error;
	}

	info->advance.x = info->glyph->advance.x >> 10;
	info->advance.y = info->glyph->advance.y >> 10;

	info->outline_glyph = info->glyph;
	error = FT_Glyph_Stroke( &(info->outline_glyph), render_context.stroker, 0 ); // don't destroy original
	if (error) {
		mp_msg(MSGT_GLOBAL, MSGL_WARN, "FT_Glyph_Stroke error %d \n", error);
		FT_Glyph_Copy(info->glyph, &info->outline_glyph);
	}

	info->bitmap = 0; // outline glyph

	return 0;
}

/**
 * \brief rearrange text between lines
 * \param max_text_width maximal text line width in pixels
 * The algo is similar to the one in libvo/sub.c:
 * 1. Place text, wrapping it when current line is full
 * 2. Try moving words from the end of a line to the beginning of the next one while it reduces
 * the difference in lengths between this two lines.
 * The result may not be optimal, but usually is good enough.
 */
static void wrap_lines_smart(int max_text_width)
{
	int i, j;
	glyph_info_t *cur, *s1, *e1, *s2, *s3, *w;
	int last_space;
	int break_type;
	int exit;
	int pen_shift_x;
	int pen_shift_y;
	int max_asc, max_desc;
	int cur_line;

	last_space = -1;
	text_info.n_lines = 1;
	break_type = 0;
	s1 = text_info.glyphs; // current line start
	for (i = 0; i < text_info.length; ++i) {
		cur = text_info.glyphs + i;
		int break_at = -1;
		int s_offset = s1->bbox.xMin + s1->pos.x;
		int len = (cur->bbox.xMax + cur->pos.x) - s_offset;

		if (cur->symbol == '\n') {
			break_type = 2;
			break_at = i;
			mp_msg(MSGT_GLOBAL, MSGL_DBG2, "forced line break at %d\n", break_at);
		}
		
		if (len >= max_text_width) {
			break_type = 1;
			break_at = last_space;
			if (break_at == -1)
				break_at = i - 1;
			if (break_at == -1)
				break_at = 0;
			mp_msg(MSGT_GLOBAL, MSGL_DBG2, "overfill at %d\n", i);
			mp_msg(MSGT_GLOBAL, MSGL_DBG2, "line break at %d\n", break_at);
		}

		if (break_at != -1) {
			// need to use one more line
			// marking break_at+1 as start of a new line
			int lead = break_at + 1; // the first symbol of the new line
			if (text_info.n_lines >= MAX_LINES) {
				// to many lines ! 
				// no more linebreaks
				for (j = lead; j < text_info.length; ++j)
					text_info.glyphs[j].linebreak = 0;
				break;
			}
			if (lead < text_info.length)
				text_info.glyphs[lead].linebreak = break_type;
			last_space = -1;
			s1 = text_info.glyphs + lead;
			s_offset = s1->bbox.xMin + s1->pos.x;
			text_info.n_lines ++;
		}
		
		if (cur->symbol == ' ')
			last_space = i;
	}
#define DIFF(x,y) (((x) < (y)) ? (y - x) : (x - y))
	exit = 0;
	while (!exit) {
		exit = 1;
		w = s3 = text_info.glyphs;
		s1 = s2 = 0;
		for (i = 0; i <= text_info.length; ++i) {
			cur = text_info.glyphs + i;
			if ((i == text_info.length) || cur->linebreak) {
				s1 = s2;
				s2 = s3;
				s3 = cur;
//				printf("line: %d, %d, %d  \n", s1 - text_info.glyphs, s2 - text_info.glyphs, s3 - text_info.glyphs);
				if (s1 && (s2->linebreak == 1)) { // have at least 2 lines, and linebreak is 'soft'
					int l1, l2, l1_new, l2_new;

					w = s2;
					do { --w; } while ((w > s1) && (w->symbol == ' '));
					do { --w; } while ((w > s1) && (w->symbol != ' '));
					e1 = w;
					do { --e1; } while ((e1 > s1) && (e1->symbol == ' '));
					if (w->symbol == ' ') ++w;

//					printf("check: %d, %d, %d, w: %d, e: %d  \n", s1 - text_info.glyphs, s2 - text_info.glyphs, s3 - text_info.glyphs,
//							w - text_info.glyphs, e1 - text_info.glyphs);

					l1 = ((s2-1)->bbox.xMax + (s2-1)->pos.x) - (s1->bbox.xMin + s1->pos.x);
					l2 = ((s3-1)->bbox.xMax + (s3-1)->pos.x) - (s2->bbox.xMin + s2->pos.x);
					l1_new = (e1->bbox.xMax + e1->pos.x) - (s1->bbox.xMin + s1->pos.x);
					l2_new = ((s3-1)->bbox.xMax + (s3-1)->pos.x) - (w->bbox.xMin + w->pos.x);

					if (DIFF(l1_new, l2_new) < DIFF(l1, l2)) {
						w->linebreak = 1;
						s2->linebreak = 0;
						exit = 0;
					}
				}
			}
			if (i == text_info.length)
				break;
		}
		
	}
	assert(text_info.n_lines >= 1);
#undef DIFF
	
	text_info.height = 0;
	max_asc = max_desc = 0;
	cur_line = 0;
	for (i = 0; i < text_info.length + 1; ++i) {
		if ((i == text_info.length) || text_info.glyphs[i].linebreak) {
			text_info.lines[cur_line].asc = max_asc;
			text_info.lines[cur_line].desc = max_desc;
			text_info.height += max_asc + max_desc;
			cur_line ++;
			max_asc = max_desc = 0;
		}
		if (i < text_info.length) {
			cur = text_info.glyphs + i;
			if (cur->asc > max_asc)
				max_asc = cur->asc * render_context.scale_y;
			if (cur->desc > max_desc)
				max_desc = cur->desc * render_context.scale_y;
		}
	}
	
	pen_shift_x = 0;
	pen_shift_y = 0;
	cur_line = 1;
	for (i = 0; i < text_info.length; ++i) {
		cur = text_info.glyphs + i;
		if (cur->linebreak) {
			int height = text_info.lines[cur_line - 1].desc + text_info.lines[cur_line].asc;
			cur_line ++;
			pen_shift_x = - cur->pos.x;
			pen_shift_y += (height >> 6) + global_settings->line_spacing;
			mp_msg(MSGT_GLOBAL, MSGL_DBG2, "shifting from %d to %d by (%d, %d)\n", i, text_info.length - 1, pen_shift_x, pen_shift_y);
		}
		cur->pos.x += pen_shift_x;
		cur->pos.y += pen_shift_y;
	}
//	printf("=============  \n");
}

/**
 * \brief determine karaoke effects
 * Karaoke effects cannot be calculated during parse stage (get_next_char()),
 * so they are done in a separate step.
 * Parse stage: when karaoke style override is found, its parameters are stored in the next glyph's 
 * (the first glyph of the karaoke word)'s effect_type and effect_timing.
 * This function:
 * 1. sets effect_type for all glyphs in the word (_karaoke_ word)
 * 2. sets effect_timing for all glyphs to x coordinate of the border line between the left and right karaoke parts
 * (left part is filled with PrimaryColour, right one - with SecondaryColour).
 */
static void process_karaoke_effects(void)
{
	glyph_info_t *cur, *cur2;
	glyph_info_t *s1, *e1; // start and end of the current word
	glyph_info_t *s2; // start of the next word
	int i;
	int timing; // current timing
	int tm_start, tm_end; // timings at start and end of the current word
	int tm_current;
	double dt;
	int x;
	int x_start, x_end;

	tm_current = frame_context.time - render_context.event->Start;
	timing = 0;
	s1 = s2 = 0;
	for (i = 0; i <= text_info.length; ++i) {
		cur = text_info.glyphs + i;
		if ((i == text_info.length) || (cur->effect_type != EF_NONE)) {
			s1 = s2;
			s2 = cur;
			if (s1) {
				e1 = s2 - 1;
				tm_start = timing;
				tm_end = timing + s1->effect_timing;
				timing = tm_end;
				x_start = s1->bbox.xMin + s1->pos.x;
				x_end = e1->bbox.xMax + e1->pos.x;

				dt = (tm_current - tm_start);
				if ((s1->effect_type == EF_KARAOKE) || (s1->effect_type == EF_KARAOKE_KO)) {
					if (dt > 0)
						x = x_end + 1;
					else
						x = x_start;
				} else if (s1->effect_type == EF_KARAOKE_KF) {
					dt /= (tm_end - tm_start);
					x = x_start + (x_end - x_start) * dt;
				} else {
					mp_msg(MSGT_GLOBAL, MSGL_ERR, "Unknown effect type (internal error)  \n");
					continue;
				}

				for (cur2 = s1; cur2 <= e1; ++cur2) {
					cur2->effect_type = s1->effect_type;
					cur2->effect_timing = x - cur2->pos.x;
				}
			}
		}
	}
}

int get_face_ascender(FT_Face face)
{
	int v = face->size->metrics.ascender;
	if (!v)
		v = FT_MulFix(face->bbox.yMax, face->size->metrics.y_scale);
	return v;
}

int get_face_descender(FT_Face face)
{
	int v = face->size->metrics.descender;
	if (!v)
		v = FT_MulFix(face->bbox.yMin, face->size->metrics.y_scale);
	return -v;
}

/**
 * \brief Main ass rendering function, glues everything together
 * \param event event to render
 * Process event, appending resulting ass_image_t's to images_root.
 */
int ass_render_event(ass_event_t* event)
{
	char* p;
	FT_UInt glyph_index; 
	FT_Bool use_kerning; 
	FT_UInt previous; 
	FT_UInt num_glyphs;
	FT_Vector pen;
	int error;
	unsigned code;
	FT_BBox bbox;
	int i, j;
	FT_Vector shift;
	int MarginL, MarginR, MarginV;
	int max_text_width;
	ass_style_t* style = frame_context.track->styles + event->Style;
	int last_break;
	int alignment, halign, valign;
	int device_x, device_y;

	init_render_context(event);

	text_info.length = 0;
	pen.x = 0;
	pen.y = 0;
	previous = 0;
	num_glyphs = 0;

	
	p = event->Text;
	if (!p) {
		mp_msg(MSGT_GLOBAL, MSGL_WARN, "Empty event!\n");
		return 0;
	}

	// Event parsing.
	while (1) {
		render_context.effect_type = EF_NONE;
	
		// get next char, executing style override
		// this affects render_context
		code = get_next_char(&p);
		
		// face could have been changed in get_next_char
		if (!render_context.face) {
			free_render_context();
			return 0;
		}

		if (code == 0)
			break;

		use_kerning = FT_HAS_KERNING(render_context.face);

		if (text_info.length >= MAX_GLYPHS) {
			mp_msg(MSGT_GLOBAL, MSGL_WARN, "\nMAX_GLYPHS reached: event %d, start = %llu, duration = %llu\n Text = %s\n", 
					(int)(event - frame_context.track->events), event->Start, event->Duration, event->Text);
			break;
		}

		glyph_index = FT_Get_Char_Index( render_context.face, code);

		if ( use_kerning && previous && glyph_index ) {
			FT_Vector delta;
			FT_Get_Kerning( render_context.face, previous, glyph_index, FT_KERNING_DEFAULT, &delta );
			pen.x += delta.x;
			pen.y += delta.y;
		}

		shift.x = pen.x & 63;
		shift.y = pen.y & 63;

		if ((render_context.scale_x != 1.) || (render_context.scale_y != 1.) ||
				(frame_context.font_scale_x != 1.)) {
			FT_Matrix matrix;
			matrix.xx = (FT_Fixed)( render_context.scale_x * frame_context.font_scale_x * 0x10000L );
			matrix.xy = (FT_Fixed)( 0 * 0x10000L );
			matrix.yx = (FT_Fixed)( 0 * 0x10000L );
			matrix.yy = (FT_Fixed)( render_context.scale_y * 0x10000L );

			FT_Set_Transform( render_context.face, &matrix, &shift );
		} else {
			FT_Set_Transform(render_context.face, 0, &shift);
		}
		
		error = get_glyph(glyph_index, code, text_info.glyphs + text_info.length, &shift);

		if (error) {
			continue;
		}
		
		text_info.glyphs[text_info.length].pos.x = pen.x >> 6;
		text_info.glyphs[text_info.length].pos.y = pen.y >> 6;
		
		pen.x += text_info.glyphs[text_info.length].advance.x;
		pen.x += render_context.hspacing;
		pen.y += text_info.glyphs[text_info.length].advance.y;
		
		// if it's an outline glyph, we still need to fill the bbox
		if (text_info.glyphs[text_info.length].bitmap != 1) {
			FT_Glyph_Get_CBox( text_info.glyphs[text_info.length].glyph, FT_GLYPH_BBOX_PIXELS, &(text_info.glyphs[text_info.length].bbox) );
		}

		
		previous = glyph_index;
		
		text_info.glyphs[text_info.length].symbol = code;
		text_info.glyphs[text_info.length].linebreak = 0;
		text_info.glyphs[text_info.length].c1 = render_context.c1;
		text_info.glyphs[text_info.length].c2 = render_context.c2;
		text_info.glyphs[text_info.length].c3 = render_context.c3;
		text_info.glyphs[text_info.length].c4 = render_context.c4;
		text_info.glyphs[text_info.length].effect_type = render_context.effect_type;
		text_info.glyphs[text_info.length].effect_timing = render_context.effect_timing;
		text_info.glyphs[text_info.length].asc = get_face_ascender(render_context.face);
		text_info.glyphs[text_info.length].desc = get_face_descender(render_context.face);

		text_info.length++;
	}
	
	if (text_info.length == 0) {
		// no valid symbols in the event; this can be smth like {comment}
		free_render_context();
		return 0;
	}
	
	// depends on glyph x coordinates being monotonous, so it should be done before line wrap
	process_karaoke_effects();
	
	// calculate max length of a line
	MarginL = (event->MarginL) ? event->MarginL : style->MarginL; 
	MarginR = (event->MarginR) ? event->MarginR : style->MarginR; 
	MarginV = (event->MarginV) ? event->MarginV : style->MarginV;

	max_text_width = x2scr(frame_context.track->PlayResX - MarginR) - x2scr(MarginL);
	mp_msg(MSGT_GLOBAL, MSGL_DBG2, "normal text width: %d\n", max_text_width);

	// rearrange text in several lines
	wrap_lines_smart(max_text_width);
	
	// alignments
	alignment = render_context.alignment;
	if (!alignment)
		alignment = render_context.style->Alignment;
	halign = alignment & 3;
	valign = alignment & 12;

	// align text
	last_break = -1;
	for (i = 1; i < text_info.length + 1; ++i) { // (text_info.length + 1) is the end of the last line
		if ((i == text_info.length) || text_info.glyphs[i].linebreak) {

			glyph_info_t* first_glyph = text_info.glyphs + last_break + 1;
			glyph_info_t* last_glyph = text_info.glyphs + i - 1;

			while ((last_glyph >= first_glyph) && ((last_glyph->symbol == '\n') || (last_glyph->symbol == 0)))
				last_glyph --;

			int width = last_glyph->pos.x + last_glyph->bbox.xMax - first_glyph->pos.x - first_glyph->bbox.xMin;
			int shift = - first_glyph->bbox.xMin; // now text line starts exactly at 0 (left margin)
			if (halign == HALIGN_LEFT) { // left aligned, no action
			} else if (halign == HALIGN_RIGHT) { // right aligned
				shift = max_text_width - width;
			} else if (halign == HALIGN_CENTER) { // centered
				shift = (max_text_width - width) / 2;
			}
			for (j = last_break + 1; j < i; ++j) {
				text_info.glyphs[j].pos.x += shift;
			}
			last_break = i - 1;
		}
	}
	
	// determing text bounding box
	compute_string_bbox(&text_info, &bbox);
	
	// determine device coordinates for text

	// FIXME: using current font descender, ascender and height here is wrong.
	// correct way is using max(descender) over all the fonts used in a line
	// the same for height and ascender
	if (render_context.evt_type == EVENT_NORMAL) {
		device_x = x2scr(MarginL);
		if (valign == VALIGN_TOP) { // toptitle
			device_y = y2scr_top(MarginV) + (text_info.lines[0].asc >> 6);
			if (render_context.detect_collisions) {
				device_y += frame_context.add_top_margin;
				frame_context.add_top_margin += (text_info.height >> 6);
			}
		} else if (valign == VALIGN_CENTER) { // midtitle
			int scr_y = y2scr(frame_context.track->PlayResY / 2);
			device_y = scr_y - (bbox.yMax - bbox.yMin) / 2;
		} else { // subtitle
			int scr_y;
			if (valign != VALIGN_SUB)
				mp_msg(MSGT_GLOBAL, MSGL_V, "Invalid valign, supposing 0 (subtitle)\n");
			scr_y = y2scr_sub(frame_context.track->PlayResY - MarginV);
			device_y = scr_y;
			device_y -= (text_info.height >> 6);
			device_y += (text_info.lines[0].asc >> 6);
			if (render_context.detect_collisions) {
				device_y -= frame_context.add_bottom_margin;
				frame_context.add_bottom_margin += (text_info.height >> 6);
			}
		}
	} else if (render_context.evt_type == EVENT_POSITIONED) {
		int align_shift_x = 0;
		int align_shift_y = 0;
		mp_msg(MSGT_GLOBAL, MSGL_DBG2, "positioned event at %d, %d\n", render_context.pos_x, render_context.pos_y);
		switch(halign) {
			case HALIGN_LEFT:
				align_shift_x = - bbox.xMin;
				break;
			case HALIGN_CENTER:
				align_shift_x = - (bbox.xMax + bbox.xMin) /2;
				break;
			case HALIGN_RIGHT:
				align_shift_x = - bbox.xMax;
				break;
		}
		switch(valign) {
			case VALIGN_TOP:
				align_shift_y = - bbox.yMin;
				break;
			case VALIGN_CENTER:
				align_shift_y = - (bbox.yMax + bbox.yMin) /2;
				break;
			case VALIGN_SUB:
				align_shift_y = - bbox.yMax;
				break;
		}
		device_x = x2scr(render_context.pos_x) + align_shift_x;
		device_y = y2scr(render_context.pos_y) + align_shift_y;
	} else {
		mp_msg(MSGT_GLOBAL, MSGL_V, "unknown evt_type\n");
		device_x = 10;
		device_y = 10;
	}
	
	// fix clip coordinates (they depend on alignment)
	render_context.clip_x0 = x2scr(render_context.clip_x0);
	render_context.clip_x1 = x2scr(render_context.clip_x1);
	if (render_context.evt_type == EVENT_NORMAL) {
		if (valign == VALIGN_TOP) {
			render_context.clip_y0 = y2scr_top(render_context.clip_y0);
			render_context.clip_y1 = y2scr_top(render_context.clip_y1);
		} else if (valign == VALIGN_CENTER) {
			render_context.clip_y0 = y2scr(render_context.clip_y0);
			render_context.clip_y1 = y2scr(render_context.clip_y1);
		} else if (valign == VALIGN_SUB) {
			render_context.clip_y0 = y2scr_sub(render_context.clip_y0);
			render_context.clip_y1 = y2scr_sub(render_context.clip_y1);
		}
	} else if (render_context.evt_type == EVENT_POSITIONED) {
		render_context.clip_y0 = y2scr(render_context.clip_y0);
		render_context.clip_y1 = y2scr(render_context.clip_y1);
	}

	// rotate glyphs if needed
	if (render_context.rotation != 0.) {
		double angle = render_context.rotation;
		FT_Vector center;
		FT_Matrix matrix_rotate;
		
		matrix_rotate.xx = (FT_Fixed)( cos( angle ) * 0x10000L );
		matrix_rotate.xy = (FT_Fixed)( -sin( angle ) * 0x10000L );
		matrix_rotate.yx = (FT_Fixed)( sin( angle ) * 0x10000L );
		matrix_rotate.yy = (FT_Fixed)( cos( angle ) * 0x10000L );
		
		if (((render_context.org_x != 0) || (render_context.org_y != 0)) && (render_context.evt_type == EVENT_POSITIONED)) {
			center.x = render_context.org_x;
			center.y = render_context.org_y;
		} else {
			FT_BBox str_bbox;

			center.x = text_info.glyphs[0].pos.x + device_x;
			center.y = text_info.glyphs[0].pos.y + device_y;

			compute_string_bbox(&text_info, &str_bbox);
			center.x += (str_bbox.xMax - str_bbox.xMin) / 2;
			center.y += (str_bbox.yMax - str_bbox.yMin) / 2;
		}
//		mp_msg(MSGT_GLOBAL, MSGL_DBG2, "\ncenter: %d, %d\n", center.x, center.y);

		for (i = 0; i < text_info.length; ++i) {
			glyph_info_t* info = text_info.glyphs + i;

			// calculating shift vector
			// shift = (position - center)*M - (position - center)
			FT_Vector start;
			FT_Vector start_old;
//			mp_msg(MSGT_GLOBAL, MSGL_INFO, "start: (%d, %d) + (%d, %d) - (%d, %d) = (%d, %d)\n", info->pos.x, info->pos.y, device_x, device_y, center.x, center.y,
//					info->pos.x + device_x - center.x, info->pos.y + device_y - center.y);
			start.x = (info->pos.x + device_x - center.x) << 6;
			start.y = - (info->pos.y + device_y - center.y) << 6;
			start_old.x = start.x;
			start_old.y = start.y;
//			mp_msg(MSGT_GLOBAL, MSGL_INFO, "start: %d, %d\n", start.x / 64, start.y / 64);

			FT_Vector_Transform(&start, &matrix_rotate);
			
			start.x -= start_old.x;
			start.y -= start_old.y;

			info->pos.x += start.x >> 6;
			info->pos.y -= start.y >> 6;

//			mp_msg(MSGT_GLOBAL, MSGL_DBG2, "shift: %d, %d\n", start.x / 64, start.y / 64);
			if (info->bitmap != 1) {
				FT_Glyph_Transform( info->glyph, &matrix_rotate, 0 );
				FT_Glyph_Transform( info->outline_glyph, &matrix_rotate, 0 );
			}
		}
	}

	// render
	render_text(&text_info, device_x, device_y);

	free_render_context();
	
	return 0;
}

void ass_configure(ass_instance_t* priv, const ass_settings_t* config)
{
	memcpy(&priv->settings, config, sizeof(ass_settings_t));
}

/**
 * \brief Start a new frame
 */
int ass_start_frame(ass_instance_t *priv, ass_track_t* track, long long now)
{
	ass_instance = priv;
	global_settings = &priv->settings;

	if (!priv->settings.frame_width && !priv->settings.frame_height)
		return 1; // library not initialized
	
	frame_context.ass_priv = priv;
	frame_context.width = global_settings->frame_width;
	frame_context.height = global_settings->frame_height;
	frame_context.orig_height = global_settings->frame_height - global_settings->top_margin - global_settings->bottom_margin;
	frame_context.track = track;
	frame_context.add_bottom_margin = 0;
	frame_context.add_top_margin = 0;
	frame_context.time = now;

	ass_lazy_track_init();
	
	if (frame_context.width * track->PlayResY == frame_context.height * track->PlayResX)
		frame_context.font_scale_x = 1.;
	else
		frame_context.font_scale_x = ((double)(frame_context.width * track->PlayResY)) / (frame_context.height * track->PlayResX);

	priv->n_images = 0;

	return 0;
}

/**
 * \brief End a frame, give out rendering results
 * \return list of ass_image_t
 */
ass_image_t* ass_end_frame(void)
{
	ass_instance_t* priv = ass_instance;
	if (priv->n_images) {
		int i;
		for (i = 0; i < priv->n_images - 1; ++i)
			priv->images_root[i].next = priv->images_root + i + 1;
		priv->images_root[priv->n_images - 1].next = 0;
		return priv->images_root;
	} else {
		return 0;
	}
}
/**
 * \brief render a frame
 * \param priv library handle
 * \param track track
 * \param now current video timestamp (ms)
 */
ass_image_t* ass_render_frame(ass_instance_t *priv, ass_track_t* track, long long now)
{
	int i, rc;
	
	rc = ass_start_frame(priv, track, now);
	if (rc != 0) // some error
		return 0;
	for (i = 0; i < track->n_events; ++i) {
		ass_event_t* event = track->events + i;
		if ( (event->Start <= now) && (now < (event->Start + event->Duration)) ) {
			ass_render_event(event);
		}
	}
	return ass_end_frame();
}


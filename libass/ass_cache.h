#ifndef __ASS_CACHE_H__
#define __ASS_CACHE_H__

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_STROKER_H
#include FT_GLYPH_H

// font cache
typedef struct face_desc_s {
	char* family;
	unsigned bold;
	unsigned italic;
} face_desc_t;

void ass_face_cache_init(void);
int ass_new_face(FT_Library library, void* fontconfig_priv, face_desc_t* desc, /*out*/ FT_Face* face);
void ass_face_cache_done(void);


// describes a glyph; glyphs with equivalents structs are considered identical
typedef struct glyph_hash_key_s {
	char bitmap; // bool : true = bitmap, false = outline
	FT_Face face;
	int size; // font size
	int index; // glyph index in the face
	unsigned outline; // border width, 16.16 fixed point value
	int bold, italic;

	// the following affects bitmap glyphs only
	unsigned scale_x, scale_y; // 16.16
	int angle; // signed 16.16
	
	FT_Vector advance; // subpixel shift vector
} glyph_hash_key_t;

typedef struct glyph_hash_val_s {
	FT_Glyph glyph; // the actual glyphs
	FT_Glyph outline_glyph;
	FT_BBox bbox_scaled; // bbox after scaling, but before rotation
	FT_Vector advance; // 26.6, advance distance to the next glyph in line
} glyph_hash_val_t;

void ass_glyph_cache_init(void);
void cache_add_glyph(glyph_hash_key_t* key, glyph_hash_val_t* val);
glyph_hash_val_t* cache_find_glyph(glyph_hash_key_t* key);
void ass_glyph_cache_done(void);

#endif


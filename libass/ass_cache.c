// -*- c-basic-offset: 8; indent-tabs-mode: t -*-
// vim:ts=8:sw=8:noet:ai:
/*
  Copyright (C) 2006 Evgeniy Stepanov <eugeni.stepanov@gmail.com>

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
*/

#include "config.h"

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

#include <assert.h>

#include "mputils.h"
#include "ass_fontconfig.h"
#include "ass_bitmap.h"
#include "ass_cache.h"
#include "ass_font.h"

#define MAX_FONT_CACHE_SIZE 100

static ass_font_t* font_cache;
static int font_cache_size;

extern int no_more_font_messages;

static int font_compare(ass_font_desc_t* a, ass_font_desc_t* b) {
	if (strcmp(a->family, b->family) != 0)
		return 0;
	if (a->bold != b->bold)
		return 0;
	if (a->italic != b->italic)
		return 0;
	return 1;
}

/**
 * \brief Get a face object, either from cache or created through FreeType+FontConfig.
 * \param library FreeType library object
 * \param fontconfig_priv fontconfig private data
 * \param desc required face description
 * \return new font struct
*/ 
ass_font_t* ass_new_font(FT_Library library, void* fontconfig_priv, ass_font_desc_t* desc)
{
	int i;
	ass_font_t* item;
	int error;
	
	for (i=0; i<font_cache_size; ++i)
		if (font_compare(desc, &(font_cache[i].desc)))
			return font_cache + i;

	if (font_cache_size == MAX_FONT_CACHE_SIZE) {
		mp_msg(MSGT_ASS, MSGL_FATAL, MSGTR_LIBASS_TooManyFonts);
		return 0;
	}

	item = font_cache + font_cache_size;
	error = ass_font_init(library, fontconfig_priv, item, desc);
	if (error) // FIXME: mp_msg
		return 0;
	font_cache_size++;

	return item;
}

void ass_font_cache_init(void)
{
	font_cache = calloc(MAX_FONT_CACHE_SIZE, sizeof(ass_font_t));
	font_cache_size = 0;
}

void ass_font_cache_done(void)
{
	int i;
	for (i = 0; i < font_cache_size; ++i) {
		ass_font_t* item = font_cache + i;
		ass_font_free(item);
	}
	free(font_cache);
	font_cache_size = 0;
}

//---------------------------------
// glyph cache

#define GLYPH_HASH_SIZE (0xFFFF + 13)

typedef struct glyph_hash_item_s {
	glyph_hash_key_t key;
	glyph_hash_val_t val;
	struct glyph_hash_item_s* next;
} glyph_hash_item_t;

typedef glyph_hash_item_t* glyph_hash_item_p;

static glyph_hash_item_p* glyph_hash_root;
static int glyph_hash_size;

static int glyph_compare(glyph_hash_key_t* a, glyph_hash_key_t* b) {
	if (memcmp(a, b, sizeof(glyph_hash_key_t)) == 0)
		return 1;
	else
		return 0;
}

static unsigned glyph_hash(glyph_hash_key_t* key) {
	unsigned val = 0;
	unsigned i;
	for (i = 0; i < sizeof(key->face); ++i)
		val += *(unsigned char *)(&(key->face) + i);
	val <<= 21;
	
	if (key->bitmap)   val &= 0x80000000;
	if (key->be) val &= 0x40000000;
	val += key->index;
	val += key->size << 8;
	val += key->outline << 3;
	val += key->advance.x << 10;
	val += key->advance.y << 16;
	val += key->bold << 1;
	val += key->italic << 20;
	return val;
}

/**
 * \brief Add a glyph to glyph cache.
 * \param key hash key
 * \param val hash val: 2 bitmap glyphs + some additional info
*/ 
void cache_add_glyph(glyph_hash_key_t* key, glyph_hash_val_t* val)
{
	unsigned hash = glyph_hash(key);
	glyph_hash_item_t** next = glyph_hash_root + (hash % GLYPH_HASH_SIZE);
	while (*next) {
		if (glyph_compare(key, &((*next)->key)))
			return;
		next = &((*next)->next);
		assert(next);
	}
	(*next) = malloc(sizeof(glyph_hash_item_t));
//	(*next)->desc = glyph_key_copy(key, &((*next)->key));
	memcpy(&((*next)->key), key, sizeof(glyph_hash_key_t));
	memcpy(&((*next)->val), val, sizeof(glyph_hash_val_t));
	(*next)->next = 0;

	glyph_hash_size ++;
/*	if (glyph_hash_size  && (glyph_hash_size % 25 == 0)) {
		printf("\nGlyph cache: %d entries, %d bytes\n", glyph_hash_size, glyph_hash_size * sizeof(glyph_hash_item_t));
	} */
}

/**
 * \brief Get a glyph from glyph cache.
 * \param key hash key
 * \return requested hash val or 0 if not found
*/ 
glyph_hash_val_t* cache_find_glyph(glyph_hash_key_t* key)
{
	unsigned hash = glyph_hash(key);
	glyph_hash_item_t* item = glyph_hash_root[hash % GLYPH_HASH_SIZE];
	while (item) {
		if (glyph_compare(key, &(item->key))) {
			return &(item->val);
		}
		item = item->next;
	}
	return 0;
}

void ass_glyph_cache_init(void)
{
	glyph_hash_root = calloc(GLYPH_HASH_SIZE, sizeof(glyph_hash_item_p));
	glyph_hash_size = 0;
}

void ass_glyph_cache_done(void)
{
	int i;
	for (i = 0; i < GLYPH_HASH_SIZE; ++i) {
		glyph_hash_item_t* item = glyph_hash_root[i];
		while (item) {
			glyph_hash_item_t* next = item->next;
			if (item->val.bm) ass_free_bitmap(item->val.bm);
			if (item->val.bm_o) ass_free_bitmap(item->val.bm_o);
			if (item->val.bm_s) ass_free_bitmap(item->val.bm_s);
			free(item);
			item = next;
		}
	}
	free(glyph_hash_root);
	glyph_hash_size = 0;
}

void ass_glyph_cache_reset(void)
{
	ass_glyph_cache_done();
	ass_glyph_cache_init();
}


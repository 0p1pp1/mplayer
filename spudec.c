/* SPUdec.c
   Skeleton of function spudec_process_controll() is from xine sources.
   Further works:
   LGB,... (yeah, try to improve it and insert your name here! ;-)

   Kim Minh Kaplan
   implement fragments reassembly, RLE decoding.
   read brightness from the IFO.

   For information on SPU format see <URL:http://sam.zoy.org/doc/dvd/subtitles/>

 */

#ifdef USE_DVDREAD

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "spudec.h"

typedef struct {
  dvd_priv_t *dvd_info;		/* Info from libmpdemux */
  unsigned char* packet;
  size_t packet_reserve;	/* size of the memory pointed to by packet */
  int packet_offset;		/* end of the currently assembled fragment */
  int packet_size;		/* size of the packet once all fragments are assembled */
  int control_start;		/* index of start of control data */
  int palette[4];
  int alpha[4];
  int now_pts;
  int start_pts, end_pts;
  int start_col, end_col;
  int start_row, end_row;
  int width, height;
  int current_nibble[2];	/* next data nibble (4 bits) to be
                                   processed (for RLE decoding) for
                                   even and odd lines */
  int deinterlace_oddness;	/* 0 or 1, index into current_nibble */
  size_t image_size;		/* Size of the image buffer */
  unsigned char *image;		/* Grayscale value */
  unsigned char *aimage;	/* Alpha value */
} spudec_handle_t;

static inline unsigned int get_be16(const unsigned char *p)
{
  return (p[0] << 8) + p[1];
}

static inline unsigned int get_be24(const unsigned char *p)
{
  return (get_be16(p) << 8) + p[2];
}

static void next_line(spudec_handle_t *this)
{
  if (this->current_nibble[this->deinterlace_oddness] % 2)
    this->current_nibble[this->deinterlace_oddness]++;
  this->deinterlace_oddness = (this->deinterlace_oddness + 1) % 2;
}

static inline unsigned char get_nibble(spudec_handle_t *this)
{
  unsigned char nib;
  int *nibblep = this->current_nibble + this->deinterlace_oddness;
  if (*nibblep / 2 >= this->control_start) {
    fprintf(stderr, "ERROR: get_nibble past end of packet\n");
    return 0;
  }
  nib = this->packet[*nibblep / 2];
  if (*nibblep % 2)
    nib &= 0xf;
  else
    nib >>= 4;
  ++*nibblep;
  return nib;
}

static inline int mkalpha(int i)
{
  /* In mplayer's alpha planes, 0 is transparent, then 1 is nearly
     opaque upto 255 which is transparent */
  switch (i) {
  case 0xf:
    return 1;
  case 0:
    return 0;
  default:
    return (0xf - i) << 4;
  }
}

static void spudec_process_data(spudec_handle_t *this)
{
  int cmap[4], alpha[4];
  int i;
  int y = 0, x = 0;

  for (i = 0; i < 4; ++i) {
    alpha[i] = mkalpha(this->alpha[i]);
    if (alpha[i] == 0)
      cmap[i] = 0;
    else {
      cmap[i] = ((this->dvd_info->vts_file->vts_pgcit->pgci_srp[0].pgc->palette[this->palette[i]] >> 16) & 0xff) - alpha[i];
      if (cmap[i] < 0)
	cmap[i] = 0;
    }
  }

  if (this->image_size < this->width * this->height) {
    if (this->image != NULL)
      free(this->image);
    this->image = malloc(2 * this->width * this->height);
    if (this->image) {
      this->image_size = this->width * this->height;
      this->aimage = this->image + this->image_size;
    }
  }
  if (this->image == NULL)
    return;
  i = this->current_nibble[1];
  while (this->current_nibble[0] < i
	 && this->current_nibble[1] / 2 < this->control_start
	 && y < this->height) {
    int len, color;
    unsigned int rle = 0;
    rle = get_nibble(this);
    if (rle < 0x04) {
      rle = (rle << 4) | get_nibble(this);
      if (rle < 0x10) {
	rle = (rle << 4) | get_nibble(this);
	if (rle < 0x040) {
	  rle = (rle << 4) | get_nibble(this);
	  if (rle < 0x0004)
	    rle |= ((this->width - x) << 2);
	}
      }
    }
    color = 3 - (rle & 0x3);
    len = rle >> 2;
    if (len > this->width - x)
      len = this->width - x;
    /* FIXME have to use palette and alpha map*/
    memset(this->image + y * this->width + x, cmap[color], len);
    memset(this->aimage + y * this->width + x, alpha[color], len);
    x += len;
    if (x >= this->width) {
      next_line(this);
      x = 0;
      ++y;
    }
  }
}

static void spudec_process_control(spudec_handle_t *this)
{
  int a,b; /* Temporary vars */
  int date, type;
  int off;
  int start_off = 0;
  int next_off;

  this->control_start = get_be16(this->packet + 2);
  next_off = this->control_start;
  while (start_off != next_off) {
    start_off = next_off;
    date = get_be16(this->packet + start_off);
    next_off = get_be16(this->packet + start_off + 2);
    printf("date=%d\n", date);
    off = start_off + 4;
    for (type = this->packet[off++]; type != 0xff; type = this->packet[off++]) {
      printf("cmd=%d  ",type);
      switch(type) {
      case 0x00:
	/* Menu ID, 1 byte */
	printf("Menu ID\n");
	break;
      case 0x01:
	/* Start display */
	printf("Start display!\n");
	this->start_pts = this->now_pts + date;
	break;
      case 0x02:
	/* Stop display */
	printf("Stop display!\n");
	this->end_pts = this->now_pts + date;
	break;
      case 0x03:
	/* Palette */
	this->palette[0] = this->packet[off] >> 4;
	this->palette[1] = this->packet[off] & 0xf;
	this->palette[2] = this->packet[off + 1] >> 4;
	this->palette[3] = this->packet[off + 1] & 0xf;
	printf("Palette %d, %d, %d, %d\n",
	       this->palette[0], this->palette[1], this->palette[2], this->palette[3]);
	off+=2;
	break;
      case 0x04:
	/* Alpha */
	this->alpha[0] = this->packet[off] >> 4;
	this->alpha[1] = this->packet[off] & 0xf;
	this->alpha[2] = this->packet[off + 1] >> 4;
	this->alpha[3] = this->packet[off + 1] & 0xf;
	printf("Alpha %d, %d, %d, %d\n",
	       this->alpha[0], this->alpha[1], this->alpha[2], this->alpha[3]);
	off+=2;
	break;
      case 0x05:
	/* Co-ords */
	a = get_be24(this->packet + off);
	b = get_be24(this->packet + off + 3);
	this->start_col = a >> 12;
	this->end_col = a & 0xfff;
	this->width = this->end_col - this->start_col + 1;
	this->start_row = b >> 12;
	this->end_row = b & 0xfff;
	this->height = this->end_row - this->start_row /* + 1 */;
	printf("Coords  col: %d - %d  row: %d - %d  (%dx%d)\n",
	       this->start_col, this->end_col, this->start_row, this->end_row,
	       this->width, this->height);
	off+=6;
	break;
      case 0x06:
	/* Graphic lines */
	this->current_nibble[0] = 2 * get_be16(this->packet + off);
	this->current_nibble[1] = 2 * get_be16(this->packet + off + 2);
	printf("Graphic offset 1: %d  offset 2: %d\n",
	       this->current_nibble[0] / 2, this->current_nibble[1] / 2);
	off+=4;
	break;
      case 0xff:
	/* All done, bye-bye */
	printf("Done!\n");
	return;
	break;
      default:
	printf("spudec: Error determining control type 0x%02x.\n",type);
	return;
	break;
      }

      /* printf("spudec: Processsed control type 0x%02x.\n",type); */
    }
  }
}

static void spudec_decode(spudec_handle_t *this)
{
  spudec_process_control(this);
  spudec_process_data(this);
}


void spudec_assemble(void *this, unsigned char *packet, int len, int pts100)
{
  spudec_handle_t *spu = (spudec_handle_t*)this;
  spudec_heartbeat(this, pts100);
  if (spu->packet_offset == 0) {
    unsigned int len2 = get_be16(packet);
    // Start new fragment
    if (spu->packet_reserve < len2) {
      if (spu->packet != NULL)
	free(spu->packet);
      spu->packet = malloc(len2);
      spu->packet_reserve = spu->packet != NULL ? len2 : 0;
    }
    if (spu->packet != NULL) {
      spu->deinterlace_oddness = 0;
      spu->packet_size = len2;
      memcpy(spu->packet, packet, len);
      spu->packet_offset = len;
    }
  } else {
    // Continue current fragment
    if (spu->packet_size < spu->packet_offset + len)
      fprintf(stderr,"invalid fragment\n");
    else {
      memcpy(spu->packet + spu->packet_offset, packet, len);
      spu->packet_offset += len;
    }
  }
  if (spu->packet_offset == spu->packet_size) {
    spudec_decode(spu);
    spu->packet_offset = 0;
  }
}

void spudec_heartbeat(void *this, int pts100)
{
  ((spudec_handle_t *)this)->now_pts = pts100;
}

void spudec_draw(void *this, void (*draw_alpha)(int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride))
{
    spudec_handle_t *spu = (spudec_handle_t *)this;
    if (spu->start_pts <= spu->now_pts && spu->now_pts < spu->end_pts && spu->image)
	draw_alpha(spu->start_col, spu->start_row, spu->width, spu->height,
		   spu->image, spu->aimage, spu->width);
}

void *spudec_new(dvd_priv_t *dvd_info)
{
  spudec_handle_t *this = calloc(1, sizeof(spudec_handle_t));
  if (this) {
    this->dvd_info = dvd_info;
  }
  else
    perror("FATAL: spudec_init: calloc");
  return this;
}

void spudec_free(void *this)
{
  spudec_handle_t *spu = (spudec_handle_t*)this;
  if (spu) {
    if (spu->packet)
      free(spu->packet);
    if (spu->image)
      free(spu->image);
    free(spu);
  }
}

#endif /* USE_DVDREAD */


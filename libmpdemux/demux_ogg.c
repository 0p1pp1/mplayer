
#include "config.h"

#ifdef HAVE_OGGVORBIS

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <math.h>

#include "../mp_msg.h"
#include "../help_mp.h"
#include "stream.h"
#include "demuxer.h"
#include "stheader.h"

#ifdef TREMOR
#include <tremor/ogg.h>
#include <tremor/ivorbiscodec.h>
#else
#include <ogg/ogg.h>
#include <vorbis/codec.h>
#endif

#ifdef HAVE_OGGTHEORA
#include <theora/theora.h>
#endif

#define BLOCK_SIZE 4096

/// Vorbis decoder context : we need the vorbis_info for vorbis timestamping
/// Shall we put this struct def in a common header ?
typedef struct ov_struct_st {
  vorbis_info      vi; /* struct that stores all the static vorbis bitstream
			  settings */
  vorbis_comment   vc; /* struct that stores all the bitstream user comments */
  vorbis_dsp_state vd; /* central working state for the packet->PCM decoder */
  vorbis_block     vb; /* local working space for packet->PCM decode */
  float            rg_scale; /* replaygain scale */
#ifdef TREMOR
  int              rg_scale_int;
#endif
} ov_struct_t;

/* Theora decoder context : we won't be able to interpret granule positions
 * without using theora_granule_time with the theora_state of the stream.
 * This is duplicated in `vd_theora.c'; put this in a common header?
 */
#ifdef HAVE_OGGTHEORA
typedef struct theora_struct_st {
    theora_state st;
    theora_comment cc;
    theora_info inf;
} theora_struct_t;
#endif

//// OggDS headers
// Header for the new header format
typedef struct stream_header_video
{
	ogg_int32_t	width;
	ogg_int32_t	height;
} stream_header_video;
	
typedef struct stream_header_audio
{
	ogg_int16_t	channels;
	ogg_int16_t	blockalign;
	ogg_int32_t	avgbytespersec;
} stream_header_audio;

typedef struct stream_header
{
	char	streamtype[8];
	char	subtype[4];

	ogg_int32_t	size;				// size of the structure

	ogg_int64_t	time_unit;			// in reference time
	ogg_int64_t	samples_per_unit;
	ogg_int32_t default_len;		// in media time

	ogg_int32_t buffersize;
	ogg_int16_t	bits_per_sample;

	union
	{
		// Video specific
		stream_header_video	video;
		// Audio specific
		stream_header_audio	audio;
	} sh;
} stream_header;

/// Our private datas

typedef struct ogg_syncpoint {
  int64_t granulepos;
  off_t page_pos;
} ogg_syncpoint_t;

/// A logical stream
typedef struct ogg_stream {
  /// Timestamping stuff
  float samplerate; /// granulpos 2 time
  int64_t lastpos;
  int32_t lastsize;

  // Logical stream state
  ogg_stream_state stream;
  int hdr_packets;
  int vorbis;
  int theora;
  int flac;
  int text;
} ogg_stream_t;

typedef struct ogg_demuxer {
  /// Physical stream state
  ogg_sync_state sync;
  /// Current page
  ogg_page page;
  /// Logical streams
  ogg_stream_t *subs;
  int num_sub;
  ogg_syncpoint_t* syncpoints;
  int num_syncpoint;
  off_t pos, last_size;
  int64_t final_granulepos;

  /* Used for subtitle switching. */
  int n_text;
  int *text_ids;
} ogg_demuxer_t;

#define NUM_VORBIS_HDR_PACKETS 3

/// Some defines from OggDS
#define PACKET_TYPE_HEADER   0x01
#define PACKET_TYPE_BITS         0x07
#define PACKET_LEN_BITS01       0xc0
#define PACKET_LEN_BITS2         0x02
#define PACKET_IS_SYNCPOINT  0x08

extern int index_mode;

extern char *dvdsub_lang, *audio_lang;
extern int dvdsub_id;

//-------- subtitle support - should be moved to decoder layer, and queue
//                          - subtitles up in demuxer buffer...

#include "../subreader.h"
#include "../libvo/sub.h"
#define OGG_SUB_MAX_LINE 128

static subtitle ogg_sub;
extern subtitle* vo_sub;
static float clear_sub;
//FILE* subout;

static
uint16_t get_uint16 (const void *buf)
{
  uint16_t      ret;
  unsigned char *tmp;

  tmp = (unsigned char *) buf;

  ret = tmp[1] & 0xff;
  ret = (ret << 8) + (tmp[0] & 0xff);

  return (ret);
}

static
uint32_t get_uint32 (const void *buf)
{
  uint32_t      ret;
  unsigned char *tmp;

  tmp = (unsigned char *) buf;

  ret = tmp[3] & 0xff;
  ret = (ret << 8) + (tmp[2] & 0xff);
  ret = (ret << 8) + (tmp[1] & 0xff);
  ret = (ret << 8) + (tmp[0] & 0xff);

  return (ret);
}

static
uint64_t get_uint64 (const void *buf)
{
  uint64_t      ret;
  unsigned char *tmp;

  tmp = (unsigned char *) buf;

  ret = tmp[7] & 0xff;
  ret = (ret << 8) + (tmp[6] & 0xff);
  ret = (ret << 8) + (tmp[5] & 0xff);
  ret = (ret << 8) + (tmp[4] & 0xff);
  ret = (ret << 8) + (tmp[3] & 0xff);
  ret = (ret << 8) + (tmp[2] & 0xff);
  ret = (ret << 8) + (tmp[1] & 0xff);
  ret = (ret << 8) + (tmp[0] & 0xff);

  return (ret);
}

void demux_ogg_init_sub () {
  int lcv;
  if(!ogg_sub.text[0]) // not yet allocated
  for (lcv = 0; lcv < SUB_MAX_TEXT; lcv++) {
    ogg_sub.text[lcv] = (char*)malloc(OGG_SUB_MAX_LINE);
  }
}

void demux_ogg_add_sub (ogg_stream_t* os,ogg_packet* pack) {
  int lcv;
  int line_pos = 0;
  int ignoring = 0;
  char *packet = pack->packet;

  mp_msg(MSGT_DEMUX,MSGL_DBG2,"\ndemux_ogg_add_sub %02X %02X %02X '%s'\n",
      (unsigned char)packet[0],
      (unsigned char)packet[1],
      (unsigned char)packet[2],
      &packet[3]);

  ogg_sub.lines = 0;
  if (((unsigned char)packet[0]) == 0x88) { // some subtitle text
    // Find data start
    int32_t duration = 0;
    int16_t hdrlen = (*packet & PACKET_LEN_BITS01)>>6, i;
    hdrlen |= (*packet & PACKET_LEN_BITS2) <<1;
    lcv = 1 + hdrlen;
    for (i = hdrlen; i > 0; i--) {
      duration <<= 8;
      duration |= (unsigned char)packet[i];
    }
    if ((hdrlen > 0) && (duration > 0)) {
      float pts;
      if(pack->granulepos == -1)
        pack->granulepos = os->lastpos + os->lastsize;
      pts = (float)pack->granulepos/(float)os->samplerate;
      clear_sub = 1.0 + pts + (float)duration/1000.0;
    }
    while (1) {
      int c = packet[lcv++];
      if(c=='\n' || c==0 || line_pos >= OGG_SUB_MAX_LINE-1){
	  ogg_sub.text[ogg_sub.lines][line_pos] = 0; // close sub
          if(line_pos) ogg_sub.lines++;
	  if(!c || ogg_sub.lines>=SUB_MAX_TEXT) break; // EOL or TooMany
          line_pos = 0;
      }
      switch (c) {
        case '\r':
        case '\n': // just ignore linefeeds for now
                   // their placement seems rather haphazard
          break;
        case '<': // some html markup, ignore for now
          ignoring = 1;
          break;
        case '>':
          ignoring = 0;
          break;
        default:
          if(!ignoring) 
	  ogg_sub.text[ogg_sub.lines][line_pos++] = c;
          break;
      }
    }
  }

  mp_msg(MSGT_DEMUX,MSGL_DBG2,"Ogg sub lines: %d  first: '%s'\n",
      ogg_sub.lines, ogg_sub.text[0]);
#ifdef USE_ICONV
  subcp_recode1(&ogg_sub);
#endif
  vo_sub = &ogg_sub;
  vo_osd_changed(OSDTYPE_SUBTITLE);
}


// get the logical stream of the current page
// fill os if non NULL and return the stream id
static  int demux_ogg_get_page_stream(ogg_demuxer_t* ogg_d,ogg_stream_state** os) {
  int id,s_no;
  ogg_page* page = &ogg_d->page;

  s_no = ogg_page_serialno(page);

  for(id= 0; id < ogg_d->num_sub ; id++) {
    if(s_no == ogg_d->subs[id].stream.serialno)
      break;
  }

  if(id == ogg_d->num_sub) {
    // If we have only one vorbis stream allow the stream id to change
    // it's normal on radio stream (each song have an different id).
    // But we (or the codec?) should check that the samplerate, etc
    // doesn't change (for radio stream it's ok)
    if(ogg_d->num_sub == 1 && ogg_d->subs[0].vorbis) {
      ogg_stream_reset(&ogg_d->subs[0].stream);
      ogg_stream_init(&ogg_d->subs[0].stream,s_no);
      id = 0;
    } else
      return -1;
  }
  
  if(os)
    *os = &ogg_d->subs[id].stream;

  return id;

}

static unsigned char* demux_ogg_read_packet(ogg_stream_t* os,ogg_packet* pack,void *context,float* pts,int* flags, int samplesize) {
  unsigned char* data;

  *pts = 0;
  *flags = 0;

  if(os->vorbis) {
    data = pack->packet;
    if(*pack->packet & PACKET_TYPE_HEADER)
      os->hdr_packets++;
    else if (context )
    {
       vorbis_info *vi = &((ov_struct_t*)context)->vi;

       // When we dump the audio, there is no vi, but we don't care of timestamp in this case
       int32_t blocksize = vorbis_packet_blocksize(vi,pack) / samplesize;
       // Calculate the timestamp if the packet don't have any
       if(pack->granulepos == -1) {
	  pack->granulepos = os->lastpos;
	  if(os->lastsize > 0)
	     pack->granulepos += os->lastsize;
       }
       *pts = pack->granulepos / (float)vi->rate;
       os->lastsize = blocksize;
       os->lastpos = pack->granulepos;
    }
# ifdef HAVE_OGGTHEORA
  } else if (os->theora) {
     /* we pass complete packets to theora, mustn't strip the header! */
     data = pack->packet;
     os->lastsize = 1;
     
     /* header packets beginn on 1-bit: thus check (*data&0x80).  We don't
	have theora_state st, until all header packets were passed to the
	decoder. */
     if (context != NULL && !(*data&0x80))
     {
        theora_info *thi = ((theora_struct_t*)context)->st.i;
        int keyframe_granule_shift=_ilog(thi->keyframe_frequency_force-1);
        int64_t iframemask = (1 << keyframe_granule_shift) - 1;

	if (pack->granulepos >= 0)
	{
	   os->lastpos = pack->granulepos >> keyframe_granule_shift;
	   os->lastpos += pack->granulepos & iframemask;
	   *flags = ((pack->granulepos & iframemask) == 0);
	}
	else
	{
	   os->lastpos++;
	}
	pack->granulepos = os->lastpos;
	*pts = (double)os->lastpos / (double)os->samplerate;
     }
#endif /* HAVE_OGGTHEORA */
# ifdef HAVE_FLAC
  } else if (os->flac) {
     /* we pass complete packets to flac, mustn't strip the header! */
     data = pack->packet;
#endif /* HAVE_FLAC */
  } else {
    if(*pack->packet & PACKET_TYPE_HEADER)
      os->hdr_packets++;
    else {
    // Find data start
    int16_t hdrlen = (*pack->packet & PACKET_LEN_BITS01)>>6;
    hdrlen |= (*pack->packet & PACKET_LEN_BITS2) <<1;
    data = pack->packet + 1 + hdrlen;
    // Calculate the timestamp
    if(pack->granulepos == -1)
      pack->granulepos = os->lastpos + (os->lastsize ? os->lastsize : 1);
    // If we alredy have a timestamp it can be a syncpoint
    if(*pack->packet & PACKET_IS_SYNCPOINT)
      *flags = 1;
    *pts =  pack->granulepos/os->samplerate;
    // Save the packet length and timestamp
    os->lastsize = 0;
    while(hdrlen) {
      os->lastsize <<= 8;
      os->lastsize |= pack->packet[hdrlen];
      hdrlen--;
    }
    os->lastpos = pack->granulepos;
  }
  }
  return data;
}

// check if clang has substring from comma separated langlist
static int demux_ogg_check_lang(char *clang, char *langlist)
{
  char *c;

  if (!langlist || !*langlist)
    return 0;
  while ((c = strchr(langlist, ',')))
  {
    if (!strncasecmp(clang, langlist, c - langlist))
      return 1;
    langlist = &c[1];
  }
  if (!strncasecmp(clang, langlist, strlen(langlist)))
    return 1;
  return 0;
}

/// Try to print out comments and also check for LANGUAGE= tag
static void demux_ogg_check_comments(demuxer_t *d, ogg_stream_t *os, int id, vorbis_comment *vc)
{
  char *hdr, *val;
  char **cmt = vc->user_comments;

  while(*cmt)
  {
    hdr = NULL;
    if (!strncasecmp(*cmt, "ENCODED_USING=", 14))
    {
      hdr = "Software";
      val = *cmt + 14;
    }
    else if (!strncasecmp(*cmt, "LANGUAGE=", 9))
    {
      val = *cmt + 9;
      // check for -slang if subs are uninitialized yet
      if (os->text && d->sub->id == -1 && demux_ogg_check_lang(val, dvdsub_lang))
      {
	d->sub->id = id;
	dvdsub_id = index;
        mp_msg(MSGT_DEMUX, MSGL_V, "Ogg demuxer: Displaying subtitle stream id %d which matched -slang %s\n", id, val);
      }
      else
	hdr = "Language";
    }
    else if (!strncasecmp(*cmt, "ENCODER_URL=", 12))
    {
      hdr = "Encoder URL";
      val = *cmt + 12;
    }
    else if (!strncasecmp(*cmt, "TITLE=", 6))
    {
      hdr = "Name";
      val = *cmt + 6;
    }
    if (hdr)
      mp_msg(MSGT_DEMUX, MSGL_V, " %s: %s\n", hdr, val);
    cmt++;
  }
}

/// Calculate the timestamp and add the packet to the demux stream
// return 1 if the packet was added, 0 otherwise
static int demux_ogg_add_packet(demux_stream_t* ds,ogg_stream_t* os,int id,ogg_packet* pack) {
  demuxer_t* d = ds->demuxer;
  demux_packet_t* dp;
  unsigned char* data;
  float pts = 0;
  int flags = 0;
  void *context = NULL;
  int samplesize = 1;

  // If packet is an comment header then we try to get comments at first
  if (pack->bytes >= 7 && !memcmp(pack->packet, "\003vorbis", 7))
  {
    vorbis_info vi;
    vorbis_comment vc;

    vorbis_info_init(&vi);
    vorbis_comment_init(&vc);
    vi.rate = 1L; // it's checked by vorbis_synthesis_headerin()
    if(vorbis_synthesis_headerin(&vi, &vc, pack) == 0) // if no errors
      demux_ogg_check_comments(d, os, id, &vc);
    vorbis_comment_clear(&vc);
    vorbis_info_clear(&vi);
  }
  if (os->text) {
    if (id == d->sub->id) // don't want to add subtitles to the demuxer for now
      demux_ogg_add_sub(os,pack);
    return 0;
  }
  // If packet is an header we jump it except for vorbis and theora
  // (PACKET_TYPE_HEADER bit doesn't even exist for theora ?!)
  // We jump nothing for FLAC. Ain't this great? Packet contents have to be
  // handled differently for each and every stream type. The joy! The joy!
  if(!os->flac && ((*pack->packet & PACKET_TYPE_HEADER) && 
     (ds != d->audio || ( ((sh_audio_t*)ds->sh)->format != 0xFFFE || os->hdr_packets >= NUM_VORBIS_HDR_PACKETS ) ) &&
     (ds != d->video || (((sh_video_t*)ds->sh)->format != 0xFFFC))))
    return 0;

  // For vorbis packet the packet is the data, for other codec we must jump
  // the header
  if(ds == d->audio && ((sh_audio_t*)ds->sh)->format == 0xFFFE) {
     context = ((sh_audio_t *)ds->sh)->context;
     samplesize = ((sh_audio_t *)ds->sh)->samplesize;
  }
  if (ds == d->video && ((sh_audio_t*)ds->sh)->format == 0xFFFC)
     context = ((sh_video_t *)ds->sh)->context;
  data = demux_ogg_read_packet(os,pack,context,&pts,&flags,samplesize);
  if(d->video->id < 0)
      ((sh_audio_t*)ds->sh)->delay = pts;

  /// Clear subtitles if necessary (for broken files)
  if ((clear_sub > 0) && (pts >= clear_sub)) {
    ogg_sub.lines = 0;
    vo_sub = &ogg_sub;
    vo_osd_changed(OSDTYPE_SUBTITLE);
    clear_sub = -1;
  }
  /// Send the packet
  dp = new_demux_packet(pack->bytes-(data-pack->packet));
  memcpy(dp->buffer,data,pack->bytes-(data-pack->packet));
  dp->pts = pts;
  dp->flags = flags;
  ds_add_packet(ds,dp);
  mp_msg(MSGT_DEMUX,MSGL_DBG2,"New dp: %p  ds=%p  pts=%5.3f  len=%d  flag=%d  \n",
      dp, ds, pts, dp->len, flags);
  return 1;
}

/// if -forceidx build a table of all syncpoints to make seeking easier
/// otherwise try to get at least the final_granulepos
void demux_ogg_scan_stream(demuxer_t* demuxer) {
  ogg_demuxer_t* ogg_d = demuxer->priv;
  stream_t *s = demuxer->stream;
  ogg_sync_state* sync = &ogg_d->sync;
  ogg_page* page= &ogg_d->page;
  ogg_stream_state* oss;
  ogg_stream_t* os;
  ogg_packet op;
  int np,sid,p,samplesize=1;
  void *context = NULL;
  off_t pos, last_pos;
  pos = last_pos = demuxer->movi_start;

  // Reset the stream
  if(index_mode == 2) {
  stream_seek(s,demuxer->movi_start);
  } else {
    //the 270000 are just a wild guess
    stream_seek(s,max(ogg_d->pos,demuxer->movi_end-270000));
  }
  ogg_sync_reset(sync);

  // Get the serial number of the stream we use
  if(demuxer->video->id >= 0) {
    sid = demuxer->video->id;
    /* demux_ogg_read_packet needs decoder context for Theora streams */
    if (((sh_video_t*)demuxer->video->sh)->format == 0xFFFC)
      context = ((sh_video_t*)demuxer->video->sh)->context;
  }
  else {
    sid = demuxer->audio->id;
    /* demux_ogg_read_packet needs decoder context for Vorbis streams */
    if(((sh_audio_t*)demuxer->audio->sh)->format == 0xFFFE) {
      context = ((sh_audio_t*)demuxer->audio->sh)->context;
      samplesize = ((sh_audio_t*)demuxer->audio->sh)->samplesize;
    }
  }
  os = &ogg_d->subs[sid];
  oss = &os->stream;

  while(1) {
    np = ogg_sync_pageseek(sync,page);
    if(np < 0) { // We had to skip some bytes
      if(index_mode == 2) mp_msg(MSGT_DEMUX,MSGL_ERR,"Bad page sync while building syncpoints table (%d)\n",-np);
      pos += -np;
      continue;
    }
    if(np <= 0) { // We need more data
      char* buf = ogg_sync_buffer(sync,BLOCK_SIZE);
      int len = stream_read(s,buf,BLOCK_SIZE);
      if(len == 0 && s->eof)
	break;
      ogg_sync_wrote(sync,len);
      continue;
    }
    // The page is ready
    //ogg_sync_pageout(sync,page);
    if(ogg_page_serialno(page) != os->stream.serialno) { // It isn't a page from the stream we want
      pos += np;
      continue;
    }
    if(ogg_stream_pagein(oss,page) != 0) {
      mp_msg(MSGT_DEMUX,MSGL_ERR,"Pagein error ????\n");
      pos += np;
      continue;
    }
    p = 0;
    while(ogg_stream_packetout(oss,&op) == 1) {
      float pts;
      int flags;
      demux_ogg_read_packet(os,&op,context,&pts,&flags,samplesize);
      if(op.granulepos >= 0) ogg_d->final_granulepos = op.granulepos;
      if(index_mode == 2 && (flags || (os->vorbis && op.granulepos >= 0))) {
	ogg_d->syncpoints = (ogg_syncpoint_t*)realloc(ogg_d->syncpoints,(ogg_d->num_syncpoint+1)*sizeof(ogg_syncpoint_t));
	ogg_d->syncpoints[ogg_d->num_syncpoint].granulepos = op.granulepos;
	ogg_d->syncpoints[ogg_d->num_syncpoint].page_pos = (ogg_page_continued(page) && p == 0) ? last_pos : pos;
	ogg_d->num_syncpoint++;
      }
      p++;
    }
    if(p > 1 || (p == 1 && ! ogg_page_continued(page)))
      last_pos = pos;
    pos += np;
    if(index_mode == 2) mp_msg(MSGT_DEMUX,MSGL_INFO,"Building syncpoint table %d%%\r",(int)(pos*100/s->end_pos));
  }
  if(index_mode == 2) mp_msg(MSGT_DEMUX,MSGL_INFO,"\n");

  if(index_mode == 2) mp_msg(MSGT_DEMUX,MSGL_V,"Ogg syncpoints table builed: %d syncpoints\n",ogg_d->num_syncpoint);
  mp_msg(MSGT_DEMUX,MSGL_V,"Ogg stream length (granulepos): %lld\n",ogg_d->final_granulepos);

  stream_reset(s);
  stream_seek(s,demuxer->movi_start);
  ogg_sync_reset(sync);
  for(np = 0 ; np < ogg_d->num_sub ; np++) {
    ogg_stream_reset(&ogg_d->subs[np].stream);
    ogg_d->subs[np].lastpos = ogg_d->subs[np].lastsize = ogg_d->subs[np].hdr_packets = 0;
  }

  
  // Get the first page
  while(1) {
    np = ogg_sync_pageout(sync,page);
    if(np <= 0) { // We need more data
      char* buf = ogg_sync_buffer(sync,BLOCK_SIZE);
      int len = stream_read(s,buf,BLOCK_SIZE);
      if(len == 0 && s->eof) {
	mp_msg(MSGT_DEMUX,MSGL_ERR,"EOF while trying to get the first page !!!!\n");
	break;
      }
      
      ogg_sync_wrote(sync,len);
      continue;
    }
    demux_ogg_get_page_stream(ogg_d,&oss);
    ogg_stream_pagein(oss,page);
    break;
  }
  
}

extern void print_wave_header(WAVEFORMATEX *h);
extern void print_video_header(BITMAPINFOHEADER *h);

/** \brief Return the number of subtitle tracks in the file.

  \param demuxer The demuxer for which the number of subtitle tracks
  should be returned.
*/
int demux_ogg_num_subs(demuxer_t *demuxer) {
  ogg_demuxer_t *ogg_d = (ogg_demuxer_t *)demuxer->priv;
  return ogg_d->n_text;
}

/** \brief Change the current subtitle stream and return its ID.

  \param demuxer The demuxer whose subtitle stream will be changed.
  \param new_num The number of the new subtitle track. The number must be
  between 0 and ogg_d->n_text - 1.

  \returns The Ogg stream number ( = page serial number) of the newly selected
  track.
*/
int demux_ogg_sub_id(demuxer_t *demuxer, int index) {
  ogg_demuxer_t *ogg_d = (ogg_demuxer_t *)demuxer->priv;
  return (index < 0) ? index : (index >= ogg_d->n_text) ? -1 : ogg_d->text_ids[index];
}

/// Open an ogg physical stream
int demux_ogg_open(demuxer_t* demuxer) {
  ogg_demuxer_t* ogg_d;
  stream_t *s;
  char* buf;
  int np,s_no, n_audio = 0, n_video = 0;
  int audio_id = -1, video_id = -1, text_id = -1;
  ogg_sync_state* sync;
  ogg_page* page;
  ogg_packet pack;
  sh_audio_t* sh_a;
  sh_video_t* sh_v;

#ifdef USE_ICONV
  subcp_open(NULL);
#endif

  clear_sub = -1;
  s = demuxer->stream;

  ogg_d = (ogg_demuxer_t*)calloc(1,sizeof(ogg_demuxer_t));
  sync = &ogg_d->sync;
  page = &ogg_d->page;

  ogg_sync_init(sync);

  while(1) {
    /// Try to get a page
    ogg_d->pos += ogg_d->last_size;
    np = ogg_sync_pageseek(sync,page);
    /// Error
    if(np < 0) {
      mp_msg(MSGT_DEMUX,MSGL_DBG2,"Ogg demuxer : Bad page sync\n");
      free(ogg_d);
      return 0;
    }
    /// Need some more data
    if(np == 0) {
      int len;
      buf = ogg_sync_buffer(sync,BLOCK_SIZE);
      len = stream_read(s,buf,BLOCK_SIZE);      
      if(len == 0 && s->eof) {
	free(ogg_d);
	return 0;
      }
      ogg_sync_wrote(sync,len);
      continue;
    }
    ogg_d->last_size = np;
    // We got one page now

    if( ! ogg_page_bos(page) ) { // It's not a begining page
      // Header parsing end here, we need to get the page otherwise it will be lost
      int id = demux_ogg_get_page_stream(ogg_d,NULL);
      if(id >= 0)
	ogg_stream_pagein(&ogg_d->subs[id].stream,page);
      else
	mp_msg(MSGT_DEMUX,MSGL_ERR,"Ogg : Warning found none bos page from unknown stream %d\n",ogg_page_serialno(page));
      break;
    }

    /// Init  the data structure needed for a logical stream
    ogg_d->subs = (ogg_stream_t*)realloc(ogg_d->subs,(ogg_d->num_sub+1)*sizeof(ogg_stream_t));
    memset(&ogg_d->subs[ogg_d->num_sub],0,sizeof(ogg_stream_t));
    /// Get the stream serial number
    s_no = ogg_page_serialno(page);
    ogg_stream_init(&ogg_d->subs[ogg_d->num_sub].stream,s_no);
    mp_msg(MSGT_DEMUX,MSGL_DBG2,"Ogg : Found a stream with serial=%d\n",s_no);
    // Take the first page
    ogg_stream_pagein(&ogg_d->subs[ogg_d->num_sub].stream,page);
    // Get first packet of the page
    ogg_stream_packetout(&ogg_d->subs[ogg_d->num_sub].stream,&pack);

    // Reset our vars
    sh_a = NULL;
    sh_v = NULL;

    // Check for Vorbis
    if(pack.bytes >= 7 && ! strncmp(&pack.packet[1],"vorbis", 6) ) {
      sh_a = new_sh_audio(demuxer,ogg_d->num_sub);
      sh_a->format = 0xFFFE;
      ogg_d->subs[ogg_d->num_sub].vorbis = 1;
      n_audio++;
      mp_msg(MSGT_DEMUX,MSGL_V,"Ogg : stream %d is vorbis\n",ogg_d->num_sub);

      // check for Theora
#   ifdef HAVE_OGGTHEORA
    } else if (pack.bytes >= 7 && !strncmp (&pack.packet[1], "theora", 6)) {
	int errorCode = 0;
	theora_info inf;
	theora_comment cc;
	
	theora_info_init (&inf);
	theora_comment_init (&cc);
	
	errorCode = theora_decode_header (&inf, &cc, &pack);
	if (errorCode)
	    mp_msg(MSGT_DEMUX,MSGL_ERR,"Theora header parsing failed: %i \n",
		   errorCode);
	else
	{
	    sh_v = new_sh_video(demuxer,ogg_d->num_sub);

	    sh_v->context = NULL;
	    sh_v->bih = (BITMAPINFOHEADER*)calloc(1,sizeof(BITMAPINFOHEADER));
	    sh_v->bih->biSize=sizeof(BITMAPINFOHEADER);
	    sh_v->bih->biCompression= sh_v->format = 0xFFFC;
	    sh_v->fps = ((double)inf.fps_numerator)/
		(double)inf.fps_denominator;
	    sh_v->frametime = ((double)inf.fps_denominator)/
		(double)inf.fps_numerator;
	    sh_v->disp_w = sh_v->bih->biWidth = inf.width;
	    sh_v->disp_h = sh_v->bih->biHeight = inf.height;
	    sh_v->bih->biBitCount = 24;
	    sh_v->bih->biPlanes = 3;
	    sh_v->bih->biSizeImage = ((sh_v->bih->biBitCount/8) * 
				      sh_v->bih->biWidth*sh_v->bih->biHeight);
	    ogg_d->subs[ogg_d->num_sub].samplerate = sh_v->fps;
	    ogg_d->subs[ogg_d->num_sub].theora = 1;
	    n_video++;
	    mp_msg(MSGT_DEMUX,MSGL_V,
		   "Ogg : stream %d is theora v%i.%i.%i %i:%i, %.3f FPS,"
		   " aspect %i:%i\n", ogg_d->num_sub,
		   (int)inf.version_major, 
		   (int)inf.version_minor, 
		   (int)inf.version_subminor, 
		   inf.width, inf.height,
		   sh_v->fps,
		   inf.aspect_numerator, inf.aspect_denominator);
	    if(verbose>0) print_video_header(sh_v->bih);
	}
#   endif /* HAVE_OGGTHEORA */
#   ifdef HAVE_FLAC
    } else if (pack.bytes >= 4 && !strncmp (&pack.packet[0], "fLaC", 4)) {
	sh_a = new_sh_audio(demuxer,ogg_d->num_sub);
	sh_a->format =  mmioFOURCC('f', 'L', 'a', 'C');
	n_audio++;
	ogg_d->subs[ogg_d->num_sub].flac = 1;
	sh_a->wf = NULL;
	mp_msg(MSGT_DEMUX,MSGL_V,"Ogg : stream %d is FLAC\n",ogg_d->num_sub);
#   endif /* HAVE_FLAC */

      /// Check for old header
    } else if(pack.bytes >= 142 && ! strncmp(&pack.packet[1],"Direct Show Samples embedded in Ogg",35) ) {

       // Old video header
      if(get_uint32 (pack.packet+96) == 0x05589f80 && pack.bytes >= 184) {
	sh_v = new_sh_video(demuxer,ogg_d->num_sub);
	sh_v->bih = (BITMAPINFOHEADER*)calloc(1,sizeof(BITMAPINFOHEADER));
	sh_v->bih->biSize=sizeof(BITMAPINFOHEADER);
	sh_v->bih->biCompression=
	sh_v->format = mmioFOURCC(pack.packet[68],pack.packet[69],
				pack.packet[70],pack.packet[71]);
	sh_v->frametime = get_uint64(pack.packet+164)*0.0000001;
	sh_v->fps = 1/sh_v->frametime;
	sh_v->disp_w = sh_v->bih->biWidth = get_uint32(pack.packet+176);
	sh_v->disp_h = sh_v->bih->biHeight = get_uint32(pack.packet+180);
	sh_v->bih->biBitCount = get_uint16(pack.packet+182);
	if(!sh_v->bih->biBitCount) sh_v->bih->biBitCount=24; // hack, FIXME
	sh_v->bih->biPlanes=1;
	sh_v->bih->biSizeImage=(sh_v->bih->biBitCount>>3)*sh_v->bih->biWidth*sh_v->bih->biHeight;

	ogg_d->subs[ogg_d->num_sub].samplerate = sh_v->fps;
	n_video++;
	mp_msg(MSGT_DEMUX,MSGL_V,"Ogg stream %d is video (old hdr)\n",ogg_d->num_sub);
	if(verbose>0) print_video_header(sh_v->bih);
	// Old audio header
      } else if(get_uint32(pack.packet+96) == 0x05589F81) {
	unsigned int extra_size;
	sh_a = new_sh_audio(demuxer,ogg_d->num_sub);
	extra_size = get_uint16(pack.packet+140);
	sh_a->wf = (WAVEFORMATEX*)calloc(1,sizeof(WAVEFORMATEX)+extra_size);
	sh_a->format = sh_a->wf->wFormatTag = get_uint16(pack.packet+124);
	sh_a->channels = sh_a->wf->nChannels = get_uint16(pack.packet+126);
	sh_a->samplerate = sh_a->wf->nSamplesPerSec = get_uint32(pack.packet+128);
	sh_a->wf->nAvgBytesPerSec = get_uint32(pack.packet+132);
	sh_a->wf->nBlockAlign = get_uint16(pack.packet+136);
	sh_a->wf->wBitsPerSample = get_uint16(pack.packet+138);
	sh_a->samplesize = (sh_a->wf->wBitsPerSample+7)/8;
	sh_a->wf->cbSize = extra_size;
	if(extra_size > 0)
	  memcpy(sh_a->wf+sizeof(WAVEFORMATEX),pack.packet+142,extra_size);

	ogg_d->subs[ogg_d->num_sub].samplerate = sh_a->samplerate; // * sh_a->channels;
	n_audio++;
	mp_msg(MSGT_DEMUX,MSGL_V,"Ogg stream %d is audio (old hdr)\n",ogg_d->num_sub);
	if(verbose>0) print_wave_header(sh_a->wf);
      } else
	mp_msg(MSGT_DEMUX,MSGL_WARN,"Ogg stream %d contains an old header but the header type is unknown\n",ogg_d->num_sub);

        // Check new header
    } else if ( (*pack.packet & PACKET_TYPE_BITS ) == PACKET_TYPE_HEADER && 
	      pack.bytes >= (int)sizeof(stream_header)+1) {
      stream_header *st = (stream_header*)(pack.packet+1);
      /// New video header
      if(strncmp(st->streamtype,"video",5) == 0) {
	sh_v = new_sh_video(demuxer,ogg_d->num_sub);
	sh_v->bih = (BITMAPINFOHEADER*)calloc(1,sizeof(BITMAPINFOHEADER));
	sh_v->bih->biSize=sizeof(BITMAPINFOHEADER);
	sh_v->bih->biCompression=
	sh_v->format = mmioFOURCC(st->subtype[0],st->subtype[1],
				  st->subtype[2],st->subtype[3]);
	sh_v->frametime = get_uint64(&st->time_unit)*0.0000001;
	sh_v->fps = 1.0/sh_v->frametime;
	sh_v->bih->biBitCount = get_uint16(&st->bits_per_sample);
	sh_v->disp_w = sh_v->bih->biWidth = get_uint32(&st->sh.video.width);
	sh_v->disp_h = sh_v->bih->biHeight = get_uint32(&st->sh.video.height);
	if(!sh_v->bih->biBitCount) sh_v->bih->biBitCount=24; // hack, FIXME
	sh_v->bih->biPlanes=1;
	sh_v->bih->biSizeImage=(sh_v->bih->biBitCount>>3)*sh_v->bih->biWidth*sh_v->bih->biHeight;

	ogg_d->subs[ogg_d->num_sub].samplerate= sh_v->fps;
	n_video++;
	mp_msg(MSGT_DEMUX,MSGL_V,"Ogg stream %d is video (new hdr)\n",ogg_d->num_sub);
	if(verbose>0) print_video_header(sh_v->bih);
	/// New audio header
      } else if(strncmp(st->streamtype,"audio",5) == 0) {
	char buffer[5];
	unsigned int extra_size = get_uint32 (&st->size) - sizeof(stream_header);
	memcpy(buffer,st->subtype,4);
	buffer[4] = '\0';
	sh_a = new_sh_audio(demuxer,ogg_d->num_sub);
	sh_a->wf = (WAVEFORMATEX*)calloc(1,sizeof(WAVEFORMATEX)+extra_size);
	sh_a->format =  sh_a->wf->wFormatTag = strtol(buffer, NULL, 16);
	sh_a->channels = sh_a->wf->nChannels = get_uint16(&st->sh.audio.channels);
	sh_a->samplerate = sh_a->wf->nSamplesPerSec = get_uint64(&st->samples_per_unit);
	sh_a->wf->nAvgBytesPerSec = get_uint32(&st->sh.audio.avgbytespersec);
	sh_a->wf->nBlockAlign = get_uint16(&st->sh.audio.blockalign);
	sh_a->wf->wBitsPerSample = get_uint16(&st->bits_per_sample);
	sh_a->samplesize = (sh_a->wf->wBitsPerSample+7)/8;
	sh_a->wf->cbSize = extra_size;
	if(extra_size)
	  memcpy(sh_a->wf+sizeof(WAVEFORMATEX),st+1,extra_size);

	ogg_d->subs[ogg_d->num_sub].samplerate = sh_a->samplerate; // * sh_a->channels;
	n_audio++;
	mp_msg(MSGT_DEMUX,MSGL_V,"Ogg stream %d is audio (new hdr)\n",ogg_d->num_sub);
	if(verbose>0) print_wave_header(sh_a->wf);

	/// Check for text (subtitles) header
      } else if (strncmp(st->streamtype, "text", 4) == 0) {
          mp_msg(MSGT_DEMUX, MSGL_V, "Ogg stream %d is text\n", ogg_d->num_sub);
	  ogg_d->subs[ogg_d->num_sub].samplerate= get_uint64(&st->time_unit)/10;
	  ogg_d->subs[ogg_d->num_sub].text = 1;
          if (demuxer->sub->id == ogg_d->n_text)
            text_id = ogg_d->num_sub;
          ogg_d->n_text++;
          ogg_d->text_ids = (int *)realloc(ogg_d->text_ids, sizeof(int) * ogg_d->n_text);
          ogg_d->text_ids[ogg_d->n_text - 1] = ogg_d->num_sub;
          demux_ogg_init_sub();
	//// Unknown header type
      } else
	mp_msg(MSGT_DEMUX,MSGL_ERR,"Ogg stream %d has a header marker but is of an unknown type\n",ogg_d->num_sub);
      /// Unknown (invalid ?) header
    } else
      mp_msg(MSGT_DEMUX,MSGL_ERR,"Ogg stream %d is of an unknown type\n",ogg_d->num_sub);

    if(sh_a || sh_v) {
      demux_stream_t* ds = NULL;
      if(sh_a) {
	// If the audio stream is not defined we took the first one
	if(demuxer->audio->id == -1) {
	  demuxer->audio->id = n_audio - 1;
//	  if(sh_a->wf) print_wave_header(sh_a->wf);
	}
	/// Is it the stream we want
	if(demuxer->audio->id == (n_audio - 1)) {
	  demuxer->audio->sh = sh_a;
	  sh_a->ds = demuxer->audio;
	  ds = demuxer->audio;
          audio_id = ogg_d->num_sub;
	}
      }
      if(sh_v) {
	/// Also for video
	if(demuxer->video->id == -1) {
	  demuxer->video->id = n_video - 1;
//	  if(sh_v->bih) print_video_header(sh_v->bih);
	}
	if(demuxer->video->id == (n_video - 1)) {
	  demuxer->video->sh = sh_v;
	  sh_v->ds = demuxer->video;
	  ds = demuxer->video;
          video_id = ogg_d->num_sub;
	}
      }
      /// Add the header packets if the stream isn't seekable
      if(ds && !s->end_pos) {
	/// Finish the page, otherwise packets will be lost
	do {
	  demux_ogg_add_packet(ds,&ogg_d->subs[ogg_d->num_sub],ogg_d->num_sub,&pack);
	} while(ogg_stream_packetout(&ogg_d->subs[ogg_d->num_sub].stream,&pack) == 1);
      }
    }
    ogg_d->num_sub++;      
  }

  if(!n_video && !n_audio) {
    free(ogg_d);
    return 0;
  }

  /// Finish to setup the demuxer
  demuxer->priv = ogg_d;

  if(!n_video || (video_id < 0))
    demuxer->video->id = -2;
  else
    demuxer->video->id = video_id;
  if(!n_audio || (audio_id < 0))
    demuxer->audio->id = -2;
  else
    demuxer->audio->id = audio_id;
  /* Disable the subs only if there are no text streams at all.
     Otherwise the stream to display might be chosen later when the comment
     packet is encountered and the user used -slang instead of -sid. */
  if(!ogg_d->n_text)
    demuxer->sub->id = -2;
  else if (text_id >= 0) {
    demuxer->sub->id = text_id;
    mp_msg(MSGT_DEMUX, MSGL_V, "Ogg demuxer: Displaying subtitle stream id %d\n", text_id);
  }

  ogg_d->final_granulepos=0;
  if(!s->end_pos)
    demuxer->seekable = 0;
  else {
    demuxer->movi_start = s->start_pos; // Needed for XCD (Ogg written in MODE2)
    demuxer->movi_end = s->end_pos;
    demuxer->seekable = 1;
    demux_ogg_scan_stream(demuxer);
  }

  mp_msg(MSGT_DEMUX,MSGL_V,"Ogg demuxer : found %d audio stream%s, %d video stream%s and %d text stream%s\n",n_audio,n_audio>1?"s":"",n_video,n_video>1?"s":"",ogg_d->n_text,ogg_d->n_text>1?"s":"");
 
  return 1;
}


int demux_ogg_fill_buffer(demuxer_t *d) {
  ogg_demuxer_t* ogg_d;
  stream_t *s;
  demux_stream_t *ds;
  ogg_sync_state* sync;
  ogg_stream_state* os;
  ogg_page* page;
  ogg_packet pack;
  int np = 0, id=0;

  s = d->stream;
  ogg_d = d->priv;
  sync = &ogg_d->sync;
  page = &ogg_d->page;

  /// Find the stream we are working on
  if ( (id = demux_ogg_get_page_stream(ogg_d,&os)) < 0) {
      mp_msg(MSGT_DEMUX,MSGL_ERR,"Ogg demuxer : can't get current stream\n");
      return 0;
  }

  while(1) {
    np = 0;
    ds = NULL;
    /// Try to get some packet from the current page
    while( (np = ogg_stream_packetout(os,&pack)) != 1) {
      /// No packet we go the next page
      if(np == 0) {
	while(1) {
	  int pa,len;
	  char *buf;
	  ogg_d->pos += ogg_d->last_size;
	  /// Get the next page from the physical stream
	  while( (pa = ogg_sync_pageseek(sync,page)) <= 0) {
	    /// Error : we skip some bytes
	    if(pa < 0) {
	      mp_msg(MSGT_DEMUX,MSGL_WARN,"Ogg : Page out not synced, we skip some bytes\n");
	      ogg_d->pos -= pa;
	      continue;
	    }
	    /// We need more data
	    buf = ogg_sync_buffer(sync,BLOCK_SIZE);	    
	    len = stream_read(s,buf,BLOCK_SIZE);
	    if(len == 0 && s->eof) {
	      mp_msg(MSGT_DEMUX,MSGL_DBG2,"Ogg : Stream EOF !!!!\n");
	      return 0;
	    }
	    ogg_sync_wrote(sync,len);
	  } /// Page loop
	  ogg_d->last_size = pa;
	  /// Find the page's logical stream
	  if( (id = demux_ogg_get_page_stream(ogg_d,&os)) < 0) {
	    mp_msg(MSGT_DEMUX,MSGL_ERR,"Ogg demuxer error : we met an unknown stream\n");
	    return 0;
	  }
	  /// Take the page
	  if(ogg_stream_pagein(os,page) == 0)
	    break;
	  /// Page was invalid => retry
	  mp_msg(MSGT_DEMUX,MSGL_WARN,"Ogg demuxer : got invalid page !!!!!\n");
	  ogg_d->pos += ogg_d->last_size;
	}
      } else /// Packet was corrupted
	mp_msg(MSGT_DEMUX,MSGL_WARN,"Ogg : bad packet in stream %d\n",id);
    } /// Packet loop
    
    /// Is the actual logical stream in use ?
    if(id == d->audio->id)
      ds = d->audio;
    else if(id == d->video->id)
      ds = d->video;
    else if (ogg_d->subs[id].text)
      ds = d->sub;

    if(ds) {
      if(!demux_ogg_add_packet(ds,&ogg_d->subs[id],id,&pack))
	continue; /// Unuseful packet, get another
      d->filepos = ogg_d->pos;
      return 1;
    }

  } /// while(1)

}

/// For avi with Ogg audio stream we have to create an ogg demuxer for this
// stream, then we join the avi and ogg demuxer with a demuxers demuxer
demuxer_t* init_avi_with_ogg(demuxer_t* demuxer) {
  demuxer_t  *od;
  ogg_demuxer_t *ogg_d;
  stream_t* s;
  uint32_t hdrsizes[3];
  demux_packet_t *dp;
  sh_audio_t *sh_audio = demuxer->audio->sh;
  int np;
  unsigned char *p = NULL,*buf;
  int plen;

  /// Check that the cbSize is enouth big for the following reads
  if(sh_audio->wf->cbSize < 22+3*sizeof(uint32_t)) {
    mp_msg(MSGT_DEMUX,MSGL_ERR,"AVI Ogg : Initial audio header is too small !!!!!\n");
    goto fallback;
  }
  /// Get the size of the 3 header packet
  memcpy(hdrsizes, ((unsigned char*)sh_audio->wf)+22+sizeof(WAVEFORMATEX), 3*sizeof(uint32_t));
//  printf("\n!!!!!! hdr sizes: %d %d %d   \n",hdrsizes[0],hdrsizes[1],hdrsizes[2]);

  /// Check the size
  if(sh_audio->wf->cbSize < 22+3*sizeof(uint32_t)+hdrsizes[0]+hdrsizes[1] + hdrsizes[2]) {
    mp_msg(MSGT_DEMUX,MSGL_ERR,"AVI Ogg : Audio header is too small !!!!!\n");
    goto fallback;
  }

  // Build the ogg demuxer private datas
  ogg_d = (ogg_demuxer_t*)calloc(1,sizeof(ogg_demuxer_t));
  ogg_d->num_sub = 1;
  ogg_d->subs = (ogg_stream_t*)malloc(sizeof(ogg_stream_t));
  ogg_d->subs[0].vorbis = 1;

   // Init the ogg physical stream
  ogg_sync_init(&ogg_d->sync);

  // Get the first page of the stream : we assume there only 1 logical stream
  while((np = ogg_sync_pageout(&ogg_d->sync,&ogg_d->page)) <= 0 ) {
    if(np < 0) {
      mp_msg(MSGT_DEMUX,MSGL_ERR,"AVI Ogg error : Can't init using first stream packets\n");
      free(ogg_d);
      goto fallback;
    }
    // Add some data
    plen = ds_get_packet(demuxer->audio,&p);    
    buf = ogg_sync_buffer(&ogg_d->sync,plen);
    memcpy(buf,p,plen);
    ogg_sync_wrote(&ogg_d->sync,plen);
  }
  // Init the logical stream
  mp_msg(MSGT_DEMUX,MSGL_DBG2,"AVI Ogg found page with serial %d\n",ogg_page_serialno(&ogg_d->page));
  ogg_stream_init(&ogg_d->subs[0].stream,ogg_page_serialno(&ogg_d->page));
  // Write the page
  ogg_stream_pagein(&ogg_d->subs[0].stream,&ogg_d->page);

  // Create the ds_stream and the ogg demuxer
  s = new_ds_stream(demuxer->audio);
  od = new_demuxer(s,DEMUXER_TYPE_OGG,0,-2,-2);

  /// Add the header packets in the ogg demuxer audio stream
  // Initial header
  dp = new_demux_packet(hdrsizes[0]);
  memcpy(dp->buffer,((unsigned char*)sh_audio->wf)+22+sizeof(WAVEFORMATEX)+3*sizeof(uint32_t),hdrsizes[0]);
  ds_add_packet(od->audio,dp);
  /// Comments
  dp = new_demux_packet(hdrsizes[1]);
  memcpy(dp->buffer,((unsigned char*)sh_audio->wf)+22+sizeof(WAVEFORMATEX)+3*sizeof(uint32_t)+hdrsizes[0],hdrsizes[1]);
  ds_add_packet(od->audio,dp);
  /// Code book
  dp = new_demux_packet(hdrsizes[2]);
  memcpy(dp->buffer,((unsigned char*)sh_audio->wf)+22+sizeof(WAVEFORMATEX)+3*sizeof(uint32_t)+hdrsizes[0]+hdrsizes[1],hdrsizes[2]);
  ds_add_packet(od->audio,dp);

  // Finish setting up the ogg demuxer
  od->priv = ogg_d;
  sh_audio = new_sh_audio(od,0);
  od->audio->id = 0;
  od->video->id = -2;
  od->audio->sh = sh_audio;
  sh_audio->ds = od->audio;
  sh_audio->format = 0xFFFE;

  /// Return the joined demuxers
  return new_demuxers_demuxer(demuxer,od,demuxer);

 fallback:
  demuxer->audio->id = -2;
  return demuxer;

}

extern void resync_audio_stream(sh_audio_t *sh_audio);

void demux_ogg_seek(demuxer_t *demuxer,float rel_seek_secs,int flags) {
  ogg_demuxer_t* ogg_d = demuxer->priv;
  ogg_sync_state* sync = &ogg_d->sync;
  ogg_page* page= &ogg_d->page;
  ogg_stream_state* oss;
  ogg_stream_t* os;
  demux_stream_t* ds;
  sh_audio_t* sh_audio = demuxer->audio->sh;
  ogg_packet op;
  float rate;
  int i,sp,first,precision=1,do_seek=1;
  vorbis_info* vi = NULL;
  int64_t gp = 0, old_gp;
  void *context = NULL;
  off_t pos, old_pos;
  int np;
  int is_gp_valid;
  float pts;
  int is_keyframe;
  int samplesize=1;

  if(demuxer->video->id >= 0) {
    ds = demuxer->video;
    /* demux_ogg_read_packet needs decoder context for Theora streams */
    if (((sh_video_t*)demuxer->video->sh)->format == 0xFFFC)
      context = ((sh_video_t*)demuxer->video->sh)->context;
    rate = ogg_d->subs[ds->id].samplerate;
  } else {
    ds = demuxer->audio;
    /* demux_ogg_read_packet needs decoder context for Vorbis streams */
    if(((sh_audio_t*)demuxer->audio->sh)->format == 0xFFFE)
      context = ((sh_audio_t*)demuxer->audio->sh)->context;
    vi = &((ov_struct_t*)((sh_audio_t*)ds->sh)->context)->vi;
    rate = (float)vi->rate;
    samplesize = ((sh_audio_t*)ds->sh)->samplesize;
  }

  os = &ogg_d->subs[ds->id];
  oss = &os->stream;

  old_gp = os->lastpos;
  old_pos = ogg_d->pos;

  //calculate the granulepos to seek to
    gp = flags & 1 ? 0 : os->lastpos;
  if(flags & 2) {
    if (ogg_d->final_granulepos > 0)
      gp += ogg_d->final_granulepos * rel_seek_secs;
      else
      gp += rel_seek_secs * (demuxer->movi_end - demuxer->movi_start) * os->lastpos / ogg_d->pos;
  } else
      gp += rel_seek_secs * rate;
  if (gp < 0) gp = 0;

  //calculate the filepos to seek to
  if(ogg_d->syncpoints) {
    for(sp = 0; sp < ogg_d->num_syncpoint ; sp++) {
      if(ogg_d->syncpoints[sp].granulepos >= gp) break;
    }

    if(sp >= ogg_d->num_syncpoint) return;
    if (sp > 0 && ogg_d->syncpoints[sp].granulepos - gp > gp - ogg_d->syncpoints[sp-1].granulepos)
      sp--;
    if (ogg_d->syncpoints[sp].granulepos == os->lastpos) {
      if (sp > 0 && gp < os->lastpos) sp--;
      if (sp < ogg_d->num_syncpoint-1 && gp > os->lastpos) sp++;
    }
    pos = ogg_d->syncpoints[sp].page_pos;
    precision = 0;
  } else {
    pos = flags & 1 ? 0 : ogg_d->pos;
    if(flags & 2)
      pos += (demuxer->movi_end - demuxer->movi_start) * rel_seek_secs;
    else {
      if (ogg_d->final_granulepos > 0) {
        pos += rel_seek_secs * (demuxer->movi_end - demuxer->movi_start) / (ogg_d->final_granulepos / rate);
      } else if (os->lastpos > 0) {
      pos += rel_seek_secs * ogg_d->pos / (os->lastpos / rate);
    }
  }
    if (pos < 0) pos = 0;
    if (pos > (demuxer->movi_end - demuxer->movi_start)) return;
  } // if(ogg_d->syncpoints)

  while(1) {
    if (do_seek) {
  stream_seek(demuxer->stream,pos+demuxer->movi_start);
  ogg_sync_reset(sync);
  for(i = 0 ; i < ogg_d->num_sub ; i++) {
    ogg_stream_reset(&ogg_d->subs[i].stream);
    ogg_d->subs[i].lastpos = ogg_d->subs[i].lastsize = 0;
  }
  ogg_d->pos = pos;
  ogg_d->last_size = 0;
      /* we just guess that we reached correct granulepos, in case a
         subsequent search occurs before we read a valid granulepos */
      os->lastpos = gp;
      first = !(ogg_d->syncpoints);
      do_seek=0;
    }
    ogg_d->pos += ogg_d->last_size;
    ogg_d->last_size = 0;
    np = ogg_sync_pageseek(sync,page);

    if(np < 0)
      ogg_d->pos -= np;
    if(np <= 0) { // We need more data
      char* buf = ogg_sync_buffer(sync,BLOCK_SIZE);
      int len = stream_read(demuxer->stream,buf,BLOCK_SIZE);
       if(len == 0 && demuxer->stream->eof) {
	mp_msg(MSGT_DEMUX,MSGL_ERR,"EOF while trying to seek !!!!\n");
	break;
      }
      ogg_sync_wrote(sync,len);
      continue;
    }
    ogg_d->last_size = np;
    if(ogg_page_serialno(page) != oss->serialno)
      continue;

    if(ogg_stream_pagein(oss,page) != 0)
      continue;

     while(1) {
      np = ogg_stream_packetout(oss,&op);
      if(np < 0)
	continue;
      else if(np == 0)
	break;
      if (first) { /* Discard the first packet as it's probably broken,
           and we don't have any other means to decide whether it is
           complete or not. */
        first = 0;
        break;
      }
      is_gp_valid = (op.granulepos >= 0);
      demux_ogg_read_packet(os,&op,context,&pts,&is_keyframe,samplesize);
      if (precision && is_gp_valid) {
        precision--;
        if (abs(gp - op.granulepos) > rate && (op.granulepos != old_gp)) {
          //prepare another seek because we are off by more than 1s
          pos += (gp - op.granulepos) * (pos - old_pos) / (op.granulepos - old_gp);
          if (pos < 0) pos = 0;
          if (pos < (demuxer->movi_end - demuxer->movi_start)) {
            do_seek=1;
            break;
          }
        }
      }
      if (is_gp_valid && (pos > 0) && (old_gp > gp)
          && (2 * (old_gp - op.granulepos) < old_gp - gp)) {
        /* prepare another seek because looking for a syncpoint
           destroyed the backward search */
        pos = old_pos - 1.5 * (old_pos - pos);
        if (pos < 0) pos = 0;
        if (pos < (demuxer->movi_end - demuxer->movi_start)) {
          do_seek=1;
          break;
        }
      }
      if(!precision && (is_keyframe || os->vorbis) ) {
        ogg_sub.lines = 0;
        vo_sub = &ogg_sub;
        vo_osd_changed(OSDTYPE_SUBTITLE);
        clear_sub = -1;
	demux_ogg_add_packet(ds,os,ds->id,&op);
	if(sh_audio)
	  resync_audio_stream(sh_audio); 
	return;
      }
     }
  }

  mp_msg(MSGT_DEMUX,MSGL_ERR,"Can't find the good packet :(\n");  

}

void demux_close_ogg(demuxer_t* demuxer) {
  ogg_demuxer_t* ogg_d = demuxer->priv;

  if(!ogg_d)
    return;

#ifdef USE_ICONV
  subcp_close();
#endif

  if(ogg_d->subs)
    free(ogg_d->subs);
  if(ogg_d->syncpoints)
    free(ogg_d->syncpoints);
  if (ogg_d->text_ids)
    free(ogg_d->text_ids);
  free(ogg_d);
}

int demux_ogg_control(demuxer_t *demuxer,int cmd, void *arg){
  ogg_demuxer_t* ogg_d = demuxer->priv;
  ogg_stream_t* os;
  float rate;

  if(demuxer->video->id >= 0) {
    os = &ogg_d->subs[demuxer->video->id];
    rate = os->samplerate;
  } else {
    os = &ogg_d->subs[demuxer->audio->id];
    rate = (float)((ov_struct_t*)((sh_audio_t*)demuxer->audio->sh)->context)->vi.rate;
  }


    switch(cmd) {
	case DEMUXER_CTRL_GET_TIME_LENGTH:
	    if (ogg_d->final_granulepos<=0) return DEMUXER_CTRL_DONTKNOW;
	    *((unsigned long *)arg)=ogg_d->final_granulepos / rate;
	    return DEMUXER_CTRL_GUESS;

	case DEMUXER_CTRL_GET_PERCENT_POS:
	    if (ogg_d->final_granulepos<=0) return DEMUXER_CTRL_DONTKNOW;
	    *((int *)arg)=(int)( (os->lastpos*100) / ogg_d->final_granulepos);
	    return DEMUXER_CTRL_OK;

	default:
	    return DEMUXER_CTRL_NOTIMPL;
    }
}

#endif

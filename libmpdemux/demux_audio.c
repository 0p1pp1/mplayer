
#include "config.h"
#include "../mp_msg.h"

#include <stdlib.h>
#include <stdio.h>
#include "stream.h"
#include "demuxer.h"
#include "stheader.h"
#include "genres.h"
#include "mp3_hdr.h"

#include <string.h>
#ifdef MP_DEBUG
#include <assert.h>
#endif

#define MP3 1
#define WAV 2
#define fLaC 3


#define HDR_SIZE 4

typedef struct da_priv {
  int frmt;
  float last_pts;
} da_priv_t;

// how many valid frames in a row we need before accepting as valid MP3
#define MIN_MP3_HDRS 5

//! Used to describe a potential (chain of) MP3 headers we found
typedef struct mp3_hdr {
  off_t frame_pos; // start of first frame in this "chain" of headers
  off_t next_frame_pos; // here we expect the next header with same parameters
  int mp3_chans;
  int mp3_freq;
  int cons_hdrs; // if this reaches MIN_MP3_HDRS we accept as MP3 file
  struct mp3_hdr *next;
} mp3_hdr_t;

extern void free_sh_audio(sh_audio_t* sh);
extern void resync_audio_stream(sh_audio_t *sh_audio);
extern void print_wave_header(WAVEFORMATEX *h);

int hr_mp3_seek = 0;

/**
 * \brief free a list of MP3 header descriptions
 * \param list pointer to the head-of-list pointer
 */
static void free_mp3_hdrs(mp3_hdr_t **list) {
  mp3_hdr_t *tmp;
  while (*list) {
    tmp = (*list)->next;
    free(*list);
    *list = tmp;
  }
}

/**
 * \brief add another potential MP3 header to our list
 * If it fits into an existing chain this one is expanded otherwise
 * a new one is created.
 * All entries that expected a MP3 header before the current position
 * are discarded.
 * The list is expected to be and will be kept sorted by next_frame_pos
 * and when those are equal by frame_pos.
 * \param list pointer to the head-of-list pointer
 * \param st_pos stream position where the described header starts
 * \param mp3_chans number of channels as specified by the header
 * \param mp3_freq sampling frequency as specified by the header
 * \param mp3_flen length of the frame as specified by the header
 * \return If non-null the current file is accepted as MP3 and the
 * mp3_hdr struct describing the valid chain is returned. Must be
 * freed independent of the list.
 */
static mp3_hdr_t *add_mp3_hdr(mp3_hdr_t **list, off_t st_pos,
                               int mp3_chans, int mp3_freq, int mp3_flen) {
  mp3_hdr_t *tmp;
  int in_list = 0;
  while (*list && (*list)->next_frame_pos <= st_pos) {
    if (((*list)->next_frame_pos < st_pos) || ((*list)->mp3_chans != mp3_chans)
         || ((*list)->mp3_freq != mp3_freq)) { // wasn't valid!
      tmp = (*list)->next;
      free(*list);
      *list = tmp;
    } else {
      (*list)->cons_hdrs++;
      (*list)->next_frame_pos = st_pos + mp3_flen;
      if ((*list)->cons_hdrs >= MIN_MP3_HDRS) {
        // copy the valid entry, so that the list can be easily freed
        tmp = malloc(sizeof(mp3_hdr_t));
        memcpy(tmp, *list, sizeof(mp3_hdr_t));
        tmp->next = NULL;
        return tmp;
      }
      in_list = 1;
      list = &((*list)->next);
    }
  }
  if (!in_list) { // does not belong into an existing chain, insert
    tmp = malloc(sizeof(mp3_hdr_t));
    tmp->frame_pos = st_pos;
    tmp->next_frame_pos = st_pos + mp3_flen;
    tmp->mp3_chans = mp3_chans;
    tmp->mp3_freq = mp3_freq;
    tmp->cons_hdrs = 1;
    tmp->next = *list;
    *list = tmp;
  }
  return NULL;
}

int demux_audio_open(demuxer_t* demuxer) {
  stream_t *s;
  sh_audio_t* sh_audio;
  uint8_t hdr[HDR_SIZE];
  int frmt = 0, n = 0, step, mp3_freq, mp3_chans, mp3_flen, mpa_layer = 3, mpa_spf = 1152;
  off_t st_pos = 0, next_frame_pos = 0;
  // mp3_hdrs list is sorted first by next_frame_pos and then by frame_pos
  mp3_hdr_t *mp3_hdrs = NULL, *mp3_found = NULL;
  da_priv_t* priv;
#ifdef MP_DEBUG
  assert(demuxer != NULL);
  assert(demuxer->stream != NULL);
#endif
  
  s = demuxer->stream;

  stream_read(s, hdr, HDR_SIZE);
  while(n < 30000 && !s->eof) {
    st_pos = stream_tell(s) - HDR_SIZE;
    step = 1;

    if( hdr[0] == 'R' && hdr[1] == 'I' && hdr[2] == 'F' && hdr[3] == 'F' ) {
      stream_skip(s,4);
      if(s->eof)
	break;
      stream_read(s,hdr,4);
      if(s->eof)
	break;
      if(hdr[0] != 'W' || hdr[1] != 'A' || hdr[2] != 'V'  || hdr[3] != 'E' )
	stream_skip(s,-8);
      else
      // We found wav header. Now we can have 'fmt ' or a mp3 header
      // empty the buffer
	step = 4;
    } else if( hdr[0] == 'I' && hdr[1] == 'D' && hdr[2] == '3' && (hdr[3] >= 2)) {
      int len;
      stream_skip(s,2);
      stream_read(s,hdr,4);
      len = (hdr[0]<<21) | (hdr[1]<<14) | (hdr[2]<<7) | hdr[3];
      stream_skip(s,len);
      step = 4;
    } else if( hdr[0] == 'f' && hdr[1] == 'm' && hdr[2] == 't' && hdr[3] == ' ' ) {
      frmt = WAV;
      break;      
    } else if((mp3_flen = mp_get_mp3_header(hdr,&mp3_chans,&mp3_freq,&mpa_spf,&mpa_layer)) > 0) {
      stream_skip(s, mp3_flen - HDR_SIZE);
      mp3_found = add_mp3_hdr(&mp3_hdrs, st_pos, mp3_chans, mp3_freq, mp3_flen);
      if (mp3_found) {
        frmt = MP3;
        break;
      }
    } else if( hdr[0] == 'f' && hdr[1] == 'L' && hdr[2] == 'a' && hdr[3] == 'C' ) {
      frmt = fLaC;
      stream_skip(s,-4);
      break;
    }
    // Add here some other audio format detection
    if(step < HDR_SIZE)
      memmove(hdr,&hdr[step],HDR_SIZE-step);
    stream_read(s, &hdr[HDR_SIZE - step], step);
    n++;
  }

  free_mp3_hdrs(&mp3_hdrs);

  if(!frmt)
    return 0;

  sh_audio = new_sh_audio(demuxer,0);

  switch(frmt) {
  case MP3:
    sh_audio->format = (mpa_layer < 3 ? 0x50 : 0x55);
    demuxer->movi_start = mp3_found->frame_pos;
    next_frame_pos = mp3_found->next_frame_pos;
    sh_audio->audio.dwSampleSize= 0;
    sh_audio->audio.dwScale = mpa_spf;
    sh_audio->audio.dwRate = mp3_found->mp3_freq;
    sh_audio->wf = malloc(sizeof(WAVEFORMATEX));
    sh_audio->wf->wFormatTag = sh_audio->format;
    sh_audio->wf->nChannels = mp3_found->mp3_chans;
    sh_audio->wf->nSamplesPerSec = mp3_found->mp3_freq;
    sh_audio->wf->nBlockAlign = mpa_spf;
    sh_audio->wf->wBitsPerSample = 16;
    sh_audio->wf->cbSize = 0;    
    free(mp3_found);
    mp3_found = NULL;
    if(s->end_pos) {
      char tag[4];
      stream_seek(s,s->end_pos-128);
      stream_read(s,tag,3);
      tag[3] = '\0';
      if(strcmp(tag,"TAG"))
	demuxer->movi_end = s->end_pos;
      else {
	char buf[31];
	uint8_t g;
	demuxer->movi_end = stream_tell(s)-3;
	stream_read(s,buf,30);
	buf[30] = '\0';
	demux_info_add(demuxer,"Title",buf);
	stream_read(s,buf,30);
	buf[30] = '\0';
	demux_info_add(demuxer,"Artist",buf);
	stream_read(s,buf,30);
	buf[30] = '\0';
	demux_info_add(demuxer,"Album",buf);
	stream_read(s,buf,4);
	buf[4] = '\0';
	demux_info_add(demuxer,"Year",buf);
	stream_read(s,buf,30);
	buf[30] = '\0';
	demux_info_add(demuxer,"Comment",buf);
	if(buf[28] == 0 && buf[29] != 0) {
	  uint8_t trk = (uint8_t)buf[29];
	  sprintf(buf,"%d",trk);
	  demux_info_add(demuxer,"Track",buf);
	}
	g = stream_read_char(s);
	demux_info_add(demuxer,"Genre",genres[g]);
      }
    }
    break;
  case WAV: {
    unsigned int chunk_type;
    unsigned int chunk_size;
    WAVEFORMATEX* w;
    int l;
    l = stream_read_dword_le(s);
    if(l < 16) {
      mp_msg(MSGT_DEMUX,MSGL_ERR,"[demux_audio] Bad wav header length: too short (%d)!!!\n",l);
      free_sh_audio(sh_audio);
      return 0;
    }
    sh_audio->wf = w = (WAVEFORMATEX*)malloc(l > sizeof(WAVEFORMATEX) ? l : sizeof(WAVEFORMATEX));
    w->wFormatTag = sh_audio->format = stream_read_word_le(s);
    w->nChannels = sh_audio->channels = stream_read_word_le(s);
    w->nSamplesPerSec = sh_audio->samplerate = stream_read_dword_le(s);
    w->nAvgBytesPerSec = stream_read_dword_le(s);
    w->nBlockAlign = stream_read_word_le(s);
    w->wBitsPerSample = sh_audio->samplesize = stream_read_word_le(s);
    w->cbSize = 0;
    l -= 16;
    if (l > 0) {
    w->cbSize = stream_read_word_le(s);
    l -= 2;
     if (w->cbSize > 0) {
      if (l < w->cbSize) {
        mp_msg(MSGT_DEMUX,MSGL_ERR,"[demux_audio] truncated extradata (%d < %d)\n",
	l,w->cbSize);
        stream_read(s,(char*)((char*)(w)+sizeof(WAVEFORMATEX)),l);
        l = 0;
      } else {
        stream_read(s,(char*)((char*)(w)+sizeof(WAVEFORMATEX)),w->cbSize);
        l -= w->cbSize;
      }
     }
    }

    if(verbose>0) print_wave_header(w);
    if(l)
      stream_skip(s,l);
    do
    {
      chunk_type = stream_read_fourcc(demuxer->stream);
      chunk_size = stream_read_dword_le(demuxer->stream);
      if (chunk_type != mmioFOURCC('d', 'a', 't', 'a'))
        stream_skip(demuxer->stream, chunk_size);
    } while (chunk_type != mmioFOURCC('d', 'a', 't', 'a'));
    demuxer->movi_start = stream_tell(s);
    demuxer->movi_end = s->end_pos;
//    printf("wav: %X .. %X\n",(int)demuxer->movi_start,(int)demuxer->movi_end);
    // Check if it contains dts audio
    if((w->wFormatTag == 0x01) && (w->nChannels == 2) && (w->nSamplesPerSec == 44100)) {
	unsigned char buf[16384]; // vlc uses 16384*4 (4 dts frames)
	unsigned int i;
	stream_read(s, buf, sizeof(buf));
	for (i = 0; i < sizeof(buf); i += 2) {
	    // DTS, 14 bit, LE
	    if((buf[i] == 0xff) && (buf[i+1] == 0x1f) && (buf[i+2] == 0x00) &&
	       (buf[i+3] == 0xe8) && ((buf[i+4] & 0xfe) == 0xf0) && (buf[i+5] == 0x07)) {
		sh_audio->format = 0x2001;
		mp_msg(MSGT_DEMUX,MSGL_V,"[demux_audio] DTS audio in wav, 14 bit, LE\n");
		break;
	    }
	    // DTS, 14 bit, BE
	    if((buf[i] == 0x1f) && (buf[i+1] == 0xff) && (buf[i+2] == 0xe8) &&
	       (buf[i+3] == 0x00) && (buf[i+4] == 0x07) && ((buf[i+5] & 0xfe) == 0xf0)) {
		sh_audio->format = 0x2001;
		mp_msg(MSGT_DEMUX,MSGL_V,"[demux_audio] DTS audio in wav, 14 bit, BE\n");
		break;
	    }
	    // DTS, 16 bit, BE
	    if((buf[i] == 0x7f) && (buf[i+1] == 0xfe) && (buf[i+2] == 0x80) &&
	       (buf[i+3] == 0x01)) {
		sh_audio->format = 0x2001;
		mp_msg(MSGT_DEMUX,MSGL_V,"[demux_audio] DTS audio in wav, 16 bit, BE\n");
		break;
	    }
	    // DTS, 16 bit, LE
	    if((buf[i] == 0xfe) && (buf[i+1] == 0x7f) && (buf[i+2] == 0x01) &&
	       (buf[i+3] == 0x80)) {
		sh_audio->format = 0x2001;
		mp_msg(MSGT_DEMUX,MSGL_V,"[demux_audio] DTS audio in wav, 16 bit, LE\n");
		break;
	    }
	}
	if (sh_audio->format == 0x2001)
	    mp_msg(MSGT_DEMUX,MSGL_DBG2,"[demux_audio] DTS sync offset = %u\n", i);

    }
    stream_seek(s,demuxer->movi_start);
  } break;
  case fLaC:
	    sh_audio->format = mmioFOURCC('f', 'L', 'a', 'C');
	    demuxer->movi_start = stream_tell(s);
	    demuxer->movi_end = s->end_pos;
	    break;
  }

  priv = (da_priv_t*)malloc(sizeof(da_priv_t));
  priv->frmt = frmt;
  priv->last_pts = -1;
  demuxer->priv = priv;
  demuxer->audio->id = 0;
  demuxer->audio->sh = sh_audio;
  sh_audio->ds = demuxer->audio;
  sh_audio->samplerate = sh_audio->audio.dwRate;

  if(stream_tell(s) != demuxer->movi_start)
  {
    mp_msg(MSGT_DEMUX, MSGL_V, "demux_audio: seeking from 0x%X to start pos 0x%X\n",
            (int)stream_tell(s), (int)demuxer->movi_start);
    stream_seek(s,demuxer->movi_start);
    if (stream_tell(s) != demuxer->movi_start) {
      mp_msg(MSGT_DEMUX, MSGL_V, "demux_audio: seeking failed, now at 0x%X!\n",
              (int)stream_tell(s));
      if (next_frame_pos) {
        mp_msg(MSGT_DEMUX, MSGL_V, "demux_audio: seeking to 0x%X instead\n",
                (int)next_frame_pos);
        stream_seek(s, next_frame_pos);
      }
    }
  }

  mp_msg(MSGT_DEMUX,MSGL_V,"demux_audio: audio data 0x%X - 0x%X  \n",(int)demuxer->movi_start,(int)demuxer->movi_end);

  return 1;
}


int demux_audio_fill_buffer(demux_stream_t *ds) {
  int l;
  demux_packet_t* dp;
  sh_audio_t* sh_audio;
  demuxer_t* demux;
  da_priv_t* priv;
  stream_t* s;
#ifdef MP_DEBUG
  assert(ds != NULL);
  assert(ds->sh != NULL);
  assert(ds->demuxer != NULL);
#endif
  sh_audio = ds->sh;
  demux = ds->demuxer;
  priv = demux->priv;
  s = demux->stream;

  if(s->eof || (demux->movi_end && stream_tell(s) >= demux->movi_end) )
    return 0;

  switch(priv->frmt) {
  case MP3 :
    while(1) {
      uint8_t hdr[4];
      stream_read(s,hdr,4);
      if (s->eof || (demux->movi_end && stream_tell(s) >= demux->movi_end))
        return 0;
      l = mp_decode_mp3_header(hdr);
      if(l < 0) {
	stream_skip(s,-3);
      } else {
	dp = new_demux_packet(l);
	memcpy(dp->buffer,hdr,4);
	stream_read(s,dp->buffer + 4,l-4);
	priv->last_pts = priv->last_pts < 0 ? 0 : priv->last_pts + sh_audio->audio.dwScale/(float)sh_audio->samplerate;
	break;
      }
    } break;
  case WAV : {
    l = sh_audio->wf->nAvgBytesPerSec;
    dp = new_demux_packet(l);
    l = stream_read(s,dp->buffer,l);
    priv->last_pts = priv->last_pts < 0 ? 0 : priv->last_pts + l/(float)sh_audio->i_bps;
    break;
  }
  case fLaC: {
    l = 65535;
    dp = new_demux_packet(l);
    l = stream_read(s,dp->buffer,l);
    priv->last_pts = priv->last_pts < 0 ? 0 : priv->last_pts + l/(float)sh_audio->i_bps;
    break;
  }
  default:
    printf("Audio demuxer : unknown format %d\n",priv->frmt);
    return 0;
  }

  resize_demux_packet(dp, l);
  ds->pts = priv->last_pts - (ds_tell_pts(demux->audio) -
              sh_audio->a_in_buffer_len)/(float)sh_audio->i_bps;
  ds_add_packet(ds, dp);
  return 1;
}

static void high_res_mp3_seek(demuxer_t *demuxer,float time) {
  uint8_t hdr[4];
  int len,nf;
  da_priv_t* priv = demuxer->priv;
  sh_audio_t* sh = (sh_audio_t*)demuxer->audio->sh;

  nf = time*sh->samplerate/sh->audio.dwScale;
  while(nf > 0) {
    stream_read(demuxer->stream,hdr,4);
    len = mp_decode_mp3_header(hdr);
    if(len < 0) {
      stream_skip(demuxer->stream,-3);
      continue;
    }
    stream_skip(demuxer->stream,len-4);
    priv->last_pts += sh->audio.dwScale/(float)sh->samplerate;
    nf--;
  }
}

void demux_audio_seek(demuxer_t *demuxer,float rel_seek_secs,int flags){
  sh_audio_t* sh_audio;
  stream_t* s;
  int base,pos;
  float len;
  da_priv_t* priv;

  if(!(sh_audio = demuxer->audio->sh))
    return;
  s = demuxer->stream;
  priv = demuxer->priv;

  if(priv->frmt == MP3 && hr_mp3_seek && !(flags & 2)) {
    len = (flags & 1) ? rel_seek_secs - priv->last_pts : rel_seek_secs;
    if(len < 0) {
      stream_seek(s,demuxer->movi_start);
      len = priv->last_pts + len;
      priv->last_pts = 0;
    }
    if(len > 0)
      high_res_mp3_seek(demuxer,len);
    sh_audio->delay = priv->last_pts -  (ds_tell_pts(demuxer->audio)-sh_audio->a_in_buffer_len)/(float)sh_audio->i_bps;
    resync_audio_stream(sh_audio);
    return;
  }

  base = flags&1 ? demuxer->movi_start : stream_tell(s);
  if(flags&2)
    pos = base + ((demuxer->movi_end - demuxer->movi_start)*rel_seek_secs);
  else
    pos = base + (rel_seek_secs*sh_audio->i_bps);

  if(demuxer->movi_end && pos >= demuxer->movi_end) {
     pos = demuxer->movi_end;
    //sh_audio->delay = (stream_tell(s) - demuxer->movi_start)/(float)sh_audio->i_bps;
    //return;
  } else if(pos < demuxer->movi_start)
    pos = demuxer->movi_start;

  priv->last_pts = (pos-demuxer->movi_start)/(float)sh_audio->i_bps;
  sh_audio->delay = priv->last_pts - (ds_tell_pts(demuxer->audio)-sh_audio->a_in_buffer_len)/(float)sh_audio->i_bps;
  
  switch(priv->frmt) {
  case WAV:
    pos -= (pos % (sh_audio->channels * sh_audio->samplesize) );
    // We need to decrease the pts by one step to make it the "last one"
    priv->last_pts -= sh_audio->wf->nAvgBytesPerSec/(float)sh_audio->i_bps;
    break;
  }

  stream_seek(s,pos);

  resync_audio_stream(sh_audio);

}

void demux_close_audio(demuxer_t* demuxer) {
  da_priv_t* priv = demuxer->priv;

  if(!priv)
    return;
  free(priv);
}

int demux_audio_control(demuxer_t *demuxer,int cmd, void *arg){
    sh_audio_t *sh_audio=demuxer->audio->sh;
    int audio_length = demuxer->movi_end / sh_audio->i_bps;
    da_priv_t* priv = demuxer->priv;
	    
    switch(cmd) {
	case DEMUXER_CTRL_GET_TIME_LENGTH:
	    if (audio_length<=0) return DEMUXER_CTRL_DONTKNOW;
	    *((unsigned long *)arg)=(unsigned long)audio_length;
	    return DEMUXER_CTRL_GUESS;

	case DEMUXER_CTRL_GET_PERCENT_POS:
	    if (audio_length<=0) 
    		return DEMUXER_CTRL_DONTKNOW;
    	    *((int *)arg)=(int)( (priv->last_pts*100)  / audio_length);
	    return DEMUXER_CTRL_OK;

	default:
	    return DEMUXER_CTRL_NOTIMPL;
    }
}

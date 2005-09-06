/**
 * Demuxer for Musepack v7 bitstream
 * by Reimar D�ffinger <Reimar.Doeffinger@stud.uni-karlsruhe.de>
 * This code may be be relicensed under the terms of the GNU LGPL when it
 * becomes part of the FFmpeg project (ffmpeg.org)
 */

#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "mp_msg.h"
#include "bswap.h"
#include "stream.h"
#include "demuxer.h"
#include "stheader.h"


#define HDR_SIZE (6 * 4)

typedef struct da_priv {
  float last_pts;
  uint32_t dword;
  int pos;
} da_priv_t;

extern void free_sh_audio(sh_audio_t* sh);

static uint32_t get_bits(da_priv_t* priv, stream_t* s, int bits) {
  uint32_t out = priv->dword;
  uint32_t mask = (1 << bits) - 1;
  priv->pos += bits;
  if (priv->pos < 32) {
    out >>= (32 - priv->pos);
  }
  else {
    stream_read(s, (void *)&priv->dword, 4);
    priv->dword = le2me_32(priv->dword);
    priv->pos -= 32;
    if (priv->pos) {
      out <<= priv->pos;
      out |= priv->dword >> (32 - priv->pos);
    }
  }
  return out & mask;
}

static int demux_mpc_check(demuxer_t* demuxer) {
  stream_t *s = demuxer->stream;
  uint8_t hdr[HDR_SIZE];
  int i;

  if (stream_read(s, hdr, HDR_SIZE) != HDR_SIZE)
    return 0;
  for (i = 0; i < 30000 && !s->eof; i++) {
    if (hdr[0] == 'M' && hdr[1] == 'P' && hdr[2] == '+')
      break;
    memmove(hdr, &hdr[1], HDR_SIZE - 1);
    stream_read(s, &hdr[HDR_SIZE - 1], 1);
  }

  if (hdr[0] != 'M' || hdr[1] != 'P' || hdr[2] != '+')
    return 0;
  demuxer->movi_start = stream_tell(s) - HDR_SIZE;
  demuxer->movi_end = s->end_pos;
  demuxer->priv = malloc(HDR_SIZE);
  memcpy(demuxer->priv, hdr, HDR_SIZE);
  return DEMUXER_TYPE_MPC;
}

static demuxer_t *demux_mpc_open(demuxer_t* demuxer) {
  stream_t *s = demuxer->stream;
  sh_audio_t* sh_audio;
  da_priv_t* priv = demuxer->priv;

  sh_audio = new_sh_audio(demuxer,0);

  {
    char *wf = (char *)calloc(1, sizeof(WAVEFORMATEX) + HDR_SIZE);
    char *header = &wf[sizeof(WAVEFORMATEX)];
    const int freqs[4] = {44100, 48000, 37800, 32000};
    int frames;
    int seconds;
    sh_audio->format = mmioFOURCC('M', 'P', 'C', ' ');
    memcpy(header, priv, HDR_SIZE);
    free(priv);
    frames = header[4] | header[5] << 8 | header[6] << 16 | header[7] << 24;
    sh_audio->wf = (WAVEFORMATEX *)wf;
    sh_audio->wf->wFormatTag = sh_audio->format;
    sh_audio->wf->nChannels = 2;
    sh_audio->wf->nSamplesPerSec = freqs[header[10] & 3];
    sh_audio->wf->nBlockAlign = 32 * 36;
    sh_audio->wf->wBitsPerSample = 16;
    seconds = 1152 * frames / sh_audio->wf->nSamplesPerSec;
    if (demuxer->movi_end > demuxer->movi_start && seconds > 0)
      sh_audio->wf->nAvgBytesPerSec = (demuxer->movi_end - demuxer->movi_start) / seconds;
    else
      sh_audio->wf->nAvgBytesPerSec = 32 * 1024; // dummy to make mencoder not hang
    sh_audio->wf->cbSize = HDR_SIZE;
    demuxer->movi_start = stream_tell(s);
    demuxer->movi_end = s->end_pos;
  }

  priv = (da_priv_t *)malloc(sizeof(da_priv_t));
  priv->last_pts = -1;
  priv->dword = 0;
  priv->pos = 0;
  stream_read(s, (void *)&priv->dword, 4);
  priv->pos = 8;
  demuxer->priv = priv;
  demuxer->audio->id = 0;
  demuxer->audio->sh = sh_audio;
  sh_audio->ds = demuxer->audio;
  sh_audio->samplerate = sh_audio->wf->nSamplesPerSec;
  sh_audio->i_bps = sh_audio->wf->nAvgBytesPerSec;
  sh_audio->audio.dwSampleSize = 0;
  sh_audio->audio.dwScale = 32 * 36;
  sh_audio->audio.dwRate = sh_audio->samplerate;

  return demuxer;
}

static int demux_mpc_fill_buffer(demuxer_t *demux, demux_stream_t *ds) {
  int l;
  int bit_len;
  demux_packet_t* dp;
  sh_audio_t* sh_audio = ds->sh;
  da_priv_t* priv = demux->priv;
  stream_t* s = demux->stream;
  sh_audio = ds->sh;

  if (s->eof)
    return 0;

  bit_len = get_bits(priv, s, 20);
  dp = new_demux_packet((bit_len + 7) / 8);
  for (l = 0; l < (bit_len / 8); l++)
    dp->buffer[l] = get_bits(priv, s, 8);
  bit_len %= 8;
  if (bit_len)
    dp->buffer[l] = get_bits(priv, s, bit_len) << (8 - bit_len);
  if (priv->last_pts < 0)
    priv->last_pts = 0;
  else
    priv->last_pts += (36 * 32) / (float)sh_audio->samplerate;
  ds->pts = priv->last_pts - (ds_tell_pts(demux->audio) -
              sh_audio->a_in_buffer_len)/(float)sh_audio->i_bps;
  ds_add_packet(ds, dp);
  return 1;
}

static void demux_mpc_seek(demuxer_t *demuxer,float rel_seek_secs,int flags){
// TODO
}

static void demux_close_mpc(demuxer_t* demuxer) {
  da_priv_t* priv = demuxer->priv;

  if(!priv)
    return;
  free(priv);
}

static int demux_mpc_control(demuxer_t *demuxer,int cmd, void *arg){
  return DEMUXER_CTRL_NOTIMPL;
}


demuxer_desc_t demuxer_desc_mpc = {
  "Musepack demuxer",
  "mpc",
  "MPC",
  "Reza Jelveh, Reimar Doeffinger",
  "supports v7 bitstream only",
  DEMUXER_TYPE_MPC,
  0, // unsafe autodetect
  demux_mpc_check,
  demux_mpc_fill_buffer,
  demux_mpc_open,
  demux_close_mpc,
  demux_mpc_seek,
  demux_mpc_control
};

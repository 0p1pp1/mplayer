/*
	FILM file parser for the MPlayer program
	by Mike Melanson

        Details of the FILM file format can be found at:
          http://www.pcisys.net/~melanson/codecs/
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "config.h"
#include "mp_msg.h"
#include "help_mp.h"

#include "stream.h"
#include "demuxer.h"
#include "stheader.h"

// chunk types found in a FILM file
#define CHUNK_FILM mmioFOURCC('F', 'I', 'L', 'M')
#define CHUNK_FDSC mmioFOURCC('F', 'D', 'S', 'C')
#define CHUNK_STAB mmioFOURCC('S', 'T', 'A', 'B')

typedef struct _film_chunk_t
{
  off_t chunk_offset;
  int chunk_size;
  unsigned int syncinfo1;
  unsigned int syncinfo2;
} film_chunk_t;

typedef struct _film_data_t
{
  unsigned int total_chunks;
  unsigned int current_chunk;
  film_chunk_t *chunks;
  unsigned int chunks_per_second;
} film_data_t;

void demux_seek_film(demuxer_t *demuxer, float rel_seek_secs, int flags)
{
  film_data_t *film_data = (film_data_t *)demuxer->priv;
  int new_current_chunk;

  // bit 2 of the flags apparently means that the seek is relative to
  // the beginning of the file
  if (flags & 1)
    new_current_chunk =
      rel_seek_secs * film_data->chunks_per_second;
  else
    new_current_chunk = film_data->current_chunk +
      rel_seek_secs * film_data->chunks_per_second;

printf ("current, total chunks = %d, %d; seek %5.3f sec, new chunk guess = %d\n",
  film_data->current_chunk, film_data->total_chunks,
  rel_seek_secs, new_current_chunk);

  // check if the new chunk number is valid
  if (new_current_chunk < 0)
    new_current_chunk = 0;
  if ((unsigned int)new_current_chunk > film_data->total_chunks)
    new_current_chunk = film_data->total_chunks;

  while (((film_data->chunks[new_current_chunk].syncinfo1 == 0xFFFFFFFF) ||
    (film_data->chunks[new_current_chunk].syncinfo1 & 0x80000000)) &&
    (new_current_chunk > 0))
    new_current_chunk--;

printf ("  actual new chunk = %d (syncinfo1 = %08X)\n",
  new_current_chunk, film_data->chunks[new_current_chunk].syncinfo1);
  film_data->current_chunk = new_current_chunk;
}

// return value:
//     0 = EOF or no stream found
//     1 = successfully read a packet
int demux_film_fill_buffer(demuxer_t *demuxer)
{
  int i;
  unsigned char byte_swap;
  int cvid_size;
  sh_video_t *sh_video = demuxer->video->sh;
  sh_audio_t *sh_audio = demuxer->audio->sh;
  film_data_t *film_data = (film_data_t *)demuxer->priv;
  film_chunk_t film_chunk;

  // see if the end has been reached
  if (film_data->current_chunk >= film_data->total_chunks)
    return 0;

  film_chunk = film_data->chunks[film_data->current_chunk];

  // position stream and fetch chunk
  stream_seek(demuxer->stream, film_chunk.chunk_offset);

  // load the chunks manually (instead of using ds_read_packet()), since
  // they require some adjustment
  // (all ones in syncinfo1 indicates an audio chunk)
  if (film_chunk.syncinfo1 == 0xFFFFFFFF)
  {
    demux_packet_t* dp=new_demux_packet(film_chunk.chunk_size);
    if (stream_read(demuxer->stream, dp->buffer, film_chunk.chunk_size) !=
      film_chunk.chunk_size)
      return 0;
    dp->pts = 0;
    dp->pos = film_chunk.chunk_offset;
    dp->flags = 0;

    // adjust the data before queuing it:
    //   8-bit: signed -> unsigned
    //  16-bit: big-endian -> little-endian
    if (sh_audio->wf->wBitsPerSample == 8)
      for (i = 0; i < film_chunk.chunk_size; i++)
        dp->buffer[i] += 128;
    else
      for (i = 0; i < film_chunk.chunk_size; i += 2)
      {
        byte_swap = dp->buffer[i];
        dp->buffer[i] = dp->buffer[i + 1];
        dp->buffer[i + 1] = byte_swap;
      }

    // append packet to DS stream
    ds_add_packet(demuxer->audio, dp);
    film_data->current_chunk++;
  }
  else
  {
    // if the demuxer is dealing with CVID data, deal with it a special way
    if (sh_video->format == mmioFOURCC('c', 'v', 'i', 'd'))
    {
      // account for 2 extra bytes
      demux_packet_t* dp=new_demux_packet(film_chunk.chunk_size - 2);

      // these CVID data chunks appear to have 2 extra bytes; skip them
      if (stream_read(demuxer->stream, dp->buffer, 10) != 10)
        return 0;
      stream_skip(demuxer->stream, 2);
      if (stream_read(demuxer->stream, dp->buffer + 10, 
        film_chunk.chunk_size - 12) != (film_chunk.chunk_size - 12))
        return 0;
      dp->pts = (film_chunk.syncinfo1 & 0x7FFFFFFF) / sh_video->fps;
      dp->pos = film_chunk.chunk_offset;
      dp->flags = (film_chunk.syncinfo1 & 0x80000000) ? 1 : 0;

      // fix the CVID chunk size by adding 6
      cvid_size = (dp->buffer[1] << 16) | (dp->buffer[2] << 8) | dp->buffer[3];
      cvid_size += 6;
      dp->buffer[1] = (cvid_size >> 16) & 0xFF;
      dp->buffer[2] = (cvid_size >>  8) & 0xFF;
      dp->buffer[3] = (cvid_size >>  0) & 0xFF;

      // append packet to DS stream
      ds_add_packet(demuxer->video, dp);
      film_data->current_chunk++;
    }
    else
    {
      ds_read_packet(demuxer->video, demuxer->stream, film_chunk.chunk_size,
        (film_chunk.syncinfo1 & 0x7FFFFFFF) / sh_video->fps,
        film_chunk.chunk_offset, (film_chunk.syncinfo1 & 0x80000000) ? 1 : 0);
      film_data->current_chunk++;
    }
  }

  return 1;
}

demuxer_t* demux_open_film(demuxer_t* demuxer)
{
  sh_video_t *sh_video = NULL;
  sh_audio_t *sh_audio = NULL;
  film_data_t *film_data;
  film_chunk_t film_chunk;
  int header_size;
  unsigned int chunk_type;
  unsigned int chunk_size;
  unsigned int i;
  unsigned int video_format;
  int audio_channels;
  unsigned int film_version;
  int counting_chunks;

  film_data = (film_data_t *)malloc(sizeof(film_data_t));
  film_data->total_chunks = 0;
  film_data->current_chunk = 0;
  film_data->chunks = NULL;
  film_data->chunks_per_second = 0;

  // go back to the beginning
  stream_reset(demuxer->stream);
  stream_seek(demuxer->stream, 0);

  // read the master chunk type
  chunk_type = stream_read_fourcc(demuxer->stream);
  // validate the chunk type
  if (chunk_type != CHUNK_FILM)
  {
    mp_msg(MSGT_DEMUX, MSGL_ERR, "Not a FILM file\n");
    return(NULL);    
  }

  // get the header size, which implicitly points past the header and
  // to the start of the data
  header_size = stream_read_dword(demuxer->stream);
  film_version = stream_read_fourcc(demuxer->stream);
  demuxer->movi_start = header_size;
  demuxer->movi_end = demuxer->stream->end_pos;
  header_size -= 16;

  mp_msg(MSGT_DEMUX, MSGL_HINT, "FILM version %.4s\n", &film_version);

  // skip to where the next chunk should be
  stream_skip(demuxer->stream, 4);

  // traverse through the header
  while (header_size > 0)
  {
    // fetch the chunk type and size
    chunk_type = stream_read_fourcc(demuxer->stream);
    chunk_size = stream_read_dword(demuxer->stream);
    header_size -= chunk_size;

    switch (chunk_type)
    {
    case CHUNK_FDSC:
      mp_msg(MSGT_DECVIDEO, MSGL_V, "parsing FDSC chunk\n");

      // fetch the video codec fourcc to see if there's any video
      video_format = stream_read_fourcc(demuxer->stream);
      if (video_format)
      {
        // create and initialize the video stream header
        sh_video = new_sh_video(demuxer, 0);
        demuxer->video->sh = sh_video;
        sh_video->ds = demuxer->video;

        sh_video->format = video_format;
        sh_video->disp_h = stream_read_dword(demuxer->stream);
        sh_video->disp_w = stream_read_dword(demuxer->stream);
        stream_skip(demuxer->stream, 1);  // unknown byte
        mp_msg(MSGT_DECVIDEO, MSGL_V,
          "  FILM video: %d x %d\n", sh_video->disp_w,
          sh_video->disp_h);
      }
      else
        stream_skip(demuxer->stream, 9);

      // fetch the audio channels to see if there's any audio
      audio_channels = stream_read_char(demuxer->stream);
      if (audio_channels > 0)
      {
        // create and initialize the audio stream header
        sh_audio = new_sh_audio(demuxer, 0);
        demuxer->audio->sh = sh_audio;
        sh_audio->ds = demuxer->audio;

        sh_audio->wf = (WAVEFORMATEX *)malloc(sizeof(WAVEFORMATEX));

        // uncompressed PCM format
        sh_audio->wf->wFormatTag = 1;
        sh_audio->format = 1;
        sh_audio->wf->nChannels = audio_channels;
        sh_audio->wf->wBitsPerSample = stream_read_char(demuxer->stream);
        stream_skip(demuxer->stream, 1);  // skip unknown byte
        sh_audio->wf->nSamplesPerSec = stream_read_word(demuxer->stream);
        sh_audio->wf->nAvgBytesPerSec = 
          sh_audio->wf->nSamplesPerSec * sh_audio->wf->wBitsPerSample 
          * sh_audio->wf->nChannels / 8;
        stream_skip(demuxer->stream, 6);  // skip the rest of the unknown

        mp_msg(MSGT_DECVIDEO, MSGL_V,
          "  FILM audio: %d channels, %d bits, %d Hz\n",
          sh_audio->wf->nChannels, 8 * sh_audio->wf->wBitsPerSample, 
          sh_audio->wf->nSamplesPerSec);
      }
      else
        stream_skip(demuxer->stream, 10);
      break;

    case CHUNK_STAB:
      mp_msg(MSGT_DECVIDEO, MSGL_V, "parsing STAB chunk\n");

      if (sh_video)
      {
        sh_video->fps = stream_read_dword(demuxer->stream);
        sh_video->frametime = 1.0 / sh_video->fps;
      }

      // fetch the number of chunks
      film_data->total_chunks = stream_read_dword(demuxer->stream);
      film_data->current_chunk = 0;
      mp_msg(MSGT_DECVIDEO, MSGL_V,
        "  STAB chunk contains %d chunks\n", film_data->total_chunks);

      // allocate enough entries for the chunk
      film_data->chunks = 
        (film_chunk_t *)malloc(film_data->total_chunks * sizeof(film_chunk_t));

      // build the chunk index
      counting_chunks = 1;
      for (i = 0; i < film_data->total_chunks; i++)
      {
        film_chunk = film_data->chunks[i];
        film_chunk.chunk_offset = 
          demuxer->movi_start + stream_read_dword(demuxer->stream);
        film_chunk.chunk_size = stream_read_dword(demuxer->stream);
        film_chunk.syncinfo1 = stream_read_dword(demuxer->stream);
        film_chunk.syncinfo2 = stream_read_dword(demuxer->stream);
        film_data->chunks[i] = film_chunk;

        // count chunks for the purposes of seeking
        if (counting_chunks)
        {
          // if we're counting chunks, always count an audio chunk
          if (film_chunk.syncinfo1 == 0xFFFFFFFF)
            film_data->chunks_per_second++;
          // if it's a video chunk, check if it's time to stop counting
          else if ((film_chunk.syncinfo1 & 0x7FFFFFFF) >= sh_video->fps)
            counting_chunks = 0;
          else
            film_data->chunks_per_second++;
        }
      }

      // in some FILM files (notable '1.09'), the length of the FDSC chunk
      // follows different rules
      if (chunk_size == (film_data->total_chunks * 16))
        header_size -= 16;
      break;

    default:
      mp_msg(MSGT_DEMUX, MSGL_ERR, "Unrecognized FILM header chunk: %08X\n",
        chunk_type);
      return(NULL);    
      break;
    }
  }

  demuxer->priv = film_data;

  return demuxer;
}

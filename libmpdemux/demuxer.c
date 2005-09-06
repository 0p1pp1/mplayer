//=================== DEMUXER v2.5 =========================

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>

#include "config.h"
#include "mp_msg.h"
#include "help_mp.h"
#include "../m_config.h"

#include "stream.h"
#include "demuxer.h"
#include "stheader.h"
#include "mf.h"

#include "../libaf/af_format.h"
#include "../libvo/fastmemcpy.h"


// Demuxer list
extern demuxer_desc_t demuxer_desc_rawaudio;
extern demuxer_desc_t demuxer_desc_rawvideo;
extern demuxer_desc_t demuxer_desc_tv;
extern demuxer_desc_t demuxer_desc_mf;
extern demuxer_desc_t demuxer_desc_avi;
extern demuxer_desc_t demuxer_desc_y4m;
extern demuxer_desc_t demuxer_desc_asf;
extern demuxer_desc_t demuxer_desc_nsv;
extern demuxer_desc_t demuxer_desc_nuv;
extern demuxer_desc_t demuxer_desc_real;
extern demuxer_desc_t demuxer_desc_smjpeg;
extern demuxer_desc_t demuxer_desc_matroska;
extern demuxer_desc_t demuxer_desc_realaudio;
extern demuxer_desc_t demuxer_desc_vqf;
extern demuxer_desc_t demuxer_desc_mov;
extern demuxer_desc_t demuxer_desc_vivo;
extern demuxer_desc_t demuxer_desc_fli;
extern demuxer_desc_t demuxer_desc_film;
extern demuxer_desc_t demuxer_desc_roq;
extern demuxer_desc_t demuxer_desc_gif;
extern demuxer_desc_t demuxer_desc_ogg;
extern demuxer_desc_t demuxer_desc_avs;
extern demuxer_desc_t demuxer_desc_pva;
extern demuxer_desc_t demuxer_desc_mpeg_ts;
extern demuxer_desc_t demuxer_desc_lmlm4;
extern demuxer_desc_t demuxer_desc_mpeg_ps;
extern demuxer_desc_t demuxer_desc_mpeg_pes;
extern demuxer_desc_t demuxer_desc_mpeg_es;
extern demuxer_desc_t demuxer_desc_mpeg_gxf;
extern demuxer_desc_t demuxer_desc_mpeg4_es;
extern demuxer_desc_t demuxer_desc_h264_es;
extern demuxer_desc_t demuxer_desc_rawdv;
extern demuxer_desc_t demuxer_desc_mpc;
extern demuxer_desc_t demuxer_desc_audio;
extern demuxer_desc_t demuxer_desc_xmms;
extern demuxer_desc_t demuxer_desc_mpeg_ty;
extern demuxer_desc_t demuxer_desc_rtp;
extern demuxer_desc_t demuxer_desc_lavf;
extern demuxer_desc_t demuxer_desc_aac;

demuxer_desc_t* demuxer_list[] = {
  &demuxer_desc_rawaudio,
  &demuxer_desc_rawvideo,
#ifdef USE_TV
  &demuxer_desc_tv,
#endif
  &demuxer_desc_mf,
  &demuxer_desc_avi,
  &demuxer_desc_y4m,
  &demuxer_desc_asf,
  &demuxer_desc_nsv,
  &demuxer_desc_nuv,
  &demuxer_desc_real,
  &demuxer_desc_smjpeg,
#ifdef HAVE_MATROSKA
  &demuxer_desc_matroska,
#endif
  &demuxer_desc_realaudio,
  &demuxer_desc_vqf,
  &demuxer_desc_mov,
  &demuxer_desc_vivo,
  &demuxer_desc_fli,
  &demuxer_desc_film,
  &demuxer_desc_roq,
#ifdef HAVE_GIF
  &demuxer_desc_gif,
#endif
#ifdef HAVE_OGGVORBIS
  &demuxer_desc_ogg,
#endif
#ifdef USE_WIN32DLL
  &demuxer_desc_avs,
#endif
  &demuxer_desc_pva,
  &demuxer_desc_mpeg_ts,
  &demuxer_desc_lmlm4,
  &demuxer_desc_mpeg_ps,
  &demuxer_desc_mpeg_pes,
  &demuxer_desc_mpeg_es,
  &demuxer_desc_mpeg_gxf,
  &demuxer_desc_mpeg4_es,
  &demuxer_desc_h264_es,
#ifdef HAVE_LIBDV095
  &demuxer_desc_rawdv,
#endif
  &demuxer_desc_mpc,
  &demuxer_desc_audio,
#ifdef HAVE_XMMS
  &demuxer_desc_xmms,
#endif
  &demuxer_desc_mpeg_ty,
#ifdef STREAMING_LIVE_DOT_COM
  &demuxer_desc_rtp,
#endif
#ifdef USE_LIBAVFORMAT
  &demuxer_desc_lavf,
#endif
  &demuxer_desc_aac,
  NULL
};

// Should be set to 1 by demux module if ids it passes to new_sh_audio and
// new_sh_video don't match aids and vids it accepts from the command line
int demux_aid_vid_mismatch = 0;

void free_demuxer_stream(demux_stream_t *ds){
    ds_free_packs(ds);
    free(ds);
}

demux_stream_t* new_demuxer_stream(struct demuxer_st *demuxer,int id){
  demux_stream_t* ds=malloc(sizeof(demux_stream_t));
  ds->buffer_pos=ds->buffer_size=0;
  ds->buffer=NULL;
  ds->pts=0;
  ds->pts_bytes=0;
  ds->eof=0;
  ds->pos=0;
  ds->dpos=0;
  ds->pack_no=0;
//---------------
  ds->packs=0;
  ds->bytes=0;
  ds->first=ds->last=ds->current=NULL;
  ds->id=id;
  ds->demuxer=demuxer;
//----------------
  ds->asf_seq=-1;
  ds->asf_packet=NULL;
//----------------
  ds->ss_mul=ds->ss_div=0;
//----------------
  ds->sh=NULL;
  return ds;
}


/**
 * Get demuxer description structure for a given demuxer type
 *
 * @param file_format    type of the demuxer
 * @return               structure for the demuxer, NULL if not found
 */
static demuxer_desc_t* get_demuxer_desc_from_type(int file_format)
{
  int i;

  for (i = 0; demuxer_list[i]; i++)
    if (file_format == demuxer_list[i]->type)
      return demuxer_list[i];

  return NULL;
}


demuxer_t* new_demuxer(stream_t *stream,int type,int a_id,int v_id,int s_id,char *filename){
  demuxer_t *d=malloc(sizeof(demuxer_t));
  memset(d,0,sizeof(demuxer_t));
  d->stream=stream;
  d->movi_start=stream->start_pos;
  d->movi_end=stream->end_pos;
  d->seekable=1;
  d->synced=0;
  d->filepos=0;
  d->audio=new_demuxer_stream(d,a_id);
  d->video=new_demuxer_stream(d,v_id);
  d->sub=new_demuxer_stream(d,s_id);
  d->type=type;
  if(type)
    if (!(d->desc = get_demuxer_desc_from_type(type)))
      mp_msg(MSGT_DEMUXER,MSGL_ERR,"BUG! Invalid demuxer type in new_demuxer(), big troubles ahead.");
  if(filename) // Filename hack for avs_check_file
    d->filename=strdup(filename);
  stream_reset(stream);
  stream_seek(stream,stream->start_pos);
  return d;
}

sh_audio_t* new_sh_audio(demuxer_t *demuxer,int id){
    if(id > MAX_A_STREAMS-1 || id < 0)
    {
	mp_msg(MSGT_DEMUXER,MSGL_WARN,"Requested audio stream id overflow (%d > %d)\n",
	    id, MAX_A_STREAMS);
	return NULL;
    }
    if(demuxer->a_streams[id]){
        mp_msg(MSGT_DEMUXER,MSGL_WARN,MSGTR_AudioStreamRedefined,id);
    } else {
        sh_audio_t *sh;
        mp_msg(MSGT_DEMUXER,MSGL_V,MSGTR_FoundAudioStream,id);
        demuxer->a_streams[id]=malloc(sizeof(sh_audio_t));
        memset(demuxer->a_streams[id],0,sizeof(sh_audio_t));
        sh = demuxer->a_streams[id];
        // set some defaults
        sh->samplesize=2;
        sh->sample_format=AF_FORMAT_S16_NE;
        sh->audio_out_minsize=8192;/* default size, maybe not enough for Win32/ACM*/
        if (identify && !demux_aid_vid_mismatch)
          mp_msg(MSGT_GLOBAL, MSGL_INFO, "ID_AUDIO_ID=%d\n", id);
    }
    return demuxer->a_streams[id];
}

void free_sh_audio(sh_audio_t* sh){
    mp_msg(MSGT_DEMUXER,MSGL_DBG2,"DEMUXER: freeing sh_audio at %p\n",sh);
    if(sh->wf) free(sh->wf);
    free(sh);
}

sh_video_t* new_sh_video(demuxer_t *demuxer,int id){
    if(id > MAX_V_STREAMS-1 || id < 0)
    {
	mp_msg(MSGT_DEMUXER,MSGL_WARN,"Requested video stream id overflow (%d > %d)\n",
	    id, MAX_V_STREAMS);
	return NULL;
    }
    if(demuxer->v_streams[id]){
        mp_msg(MSGT_DEMUXER,MSGL_WARN,MSGTR_VideoStreamRedefined,id);
    } else {
        mp_msg(MSGT_DEMUXER,MSGL_V,MSGTR_FoundVideoStream,id);
        demuxer->v_streams[id]=malloc(sizeof(sh_video_t));
        memset(demuxer->v_streams[id],0,sizeof(sh_video_t));
        if (identify && !demux_aid_vid_mismatch)
          mp_msg(MSGT_GLOBAL, MSGL_INFO, "ID_VIDEO_ID=%d\n", id);
    }
    return demuxer->v_streams[id];
}

void free_sh_video(sh_video_t* sh){
    mp_msg(MSGT_DEMUXER,MSGL_DBG2,"DEMUXER: freeing sh_video at %p\n",sh);
    if(sh->bih) free(sh->bih);
    free(sh);
}

void free_demuxer(demuxer_t *demuxer){
    int i;
    mp_msg(MSGT_DEMUXER,MSGL_DBG2,"DEMUXER: freeing demuxer at %p\n",demuxer);
    if(demuxer->desc->close)
      demuxer->desc->close(demuxer);
    // Very ugly hack to make it behave like old implementation
    if (demuxer->desc->type == DEMUXER_TYPE_DEMUXERS)
      goto skip_streamfree;
    // free streams:
    for(i = 0; i < MAX_A_STREAMS; i++)
	if(demuxer->a_streams[i]) free_sh_audio(demuxer->a_streams[i]);
    for(i = 0; i < MAX_V_STREAMS; i++)
	if(demuxer->v_streams[i]) free_sh_video(demuxer->v_streams[i]);
    //if(sh_audio) free_sh_audio(sh_audio);
    //if(sh_video) free_sh_video(sh_video);
    // free demuxers:
    free_demuxer_stream(demuxer->audio);
    free_demuxer_stream(demuxer->video);
    free_demuxer_stream(demuxer->sub);
skip_streamfree:
    if(demuxer->info) {
      for(i=0;demuxer->info[i] != NULL; i++)
	free(demuxer->info[i]);
      free(demuxer->info);
    }
    if(demuxer->filename)
      free(demuxer->filename);
    free(demuxer);
}


void ds_add_packet(demux_stream_t *ds,demux_packet_t* dp){
//    demux_packet_t* dp=new_demux_packet(len);
//    stream_read(stream,dp->buffer,len);
//    dp->pts=pts; //(float)pts/90000.0f;
//    dp->pos=pos;
    // append packet to DS stream:
    ++ds->packs;
    ds->bytes+=dp->len;
    if(ds->last){
      // next packet in stream
      ds->last->next=dp;
      ds->last=dp;
    } else {
      // first packet in stream
      ds->first=ds->last=dp;
    }
    mp_dbg(MSGT_DEMUXER,MSGL_DBG2,"DEMUX: Append packet to %s, len=%d  pts=%5.3f  pos=%u  [packs: A=%d V=%d]\n",
        (ds==ds->demuxer->audio)?"d_audio":"d_video",
        dp->len,dp->pts,(unsigned int)dp->pos,ds->demuxer->audio->packs,ds->demuxer->video->packs);
}

void ds_read_packet(demux_stream_t *ds,stream_t *stream,int len,float pts,off_t pos,int flags){
    demux_packet_t* dp=new_demux_packet(len);
    len = stream_read(stream,dp->buffer,len);
    resize_demux_packet(dp, len);
    dp->pts=pts; //(float)pts/90000.0f;
    dp->pos=pos;
    dp->flags=flags;
    // append packet to DS stream:
    ds_add_packet(ds,dp);
}

// return value:
//     0 = EOF or no stream found or invalid type
//     1 = successfully read a packet

int demux_fill_buffer(demuxer_t *demux,demux_stream_t *ds){
  // Note: parameter 'ds' can be NULL!
//  printf("demux->type=%d\n",demux->type);
  return demux->desc->fill_buffer(demux, ds);
}

// return value:
//     0 = EOF
//     1 = succesfull
int ds_fill_buffer(demux_stream_t *ds){
  demuxer_t *demux=ds->demuxer;
  if(ds->current) free_demux_packet(ds->current);
  if(verbose>2){
    if(ds==demux->audio) mp_dbg(MSGT_DEMUXER,MSGL_DBG3,"ds_fill_buffer(d_audio) called\n");else
    if(ds==demux->video) mp_dbg(MSGT_DEMUXER,MSGL_DBG3,"ds_fill_buffer(d_video) called\n");else
    if(ds==demux->sub)   mp_dbg(MSGT_DEMUXER,MSGL_DBG3,"ds_fill_buffer(d_sub) called\n");else
                         mp_dbg(MSGT_DEMUXER,MSGL_DBG3,"ds_fill_buffer(unknown 0x%X) called\n",(unsigned int)ds);
  }
  while(1){
    if(ds->packs){
      demux_packet_t *p=ds->first;
      // copy useful data:
      ds->buffer=p->buffer;
      ds->buffer_pos=0;
      ds->buffer_size=p->len;
      ds->pos=p->pos;
      ds->dpos+=p->len; // !!!
      ++ds->pack_no;
      if(p->pts){
        ds->pts=p->pts;
        ds->pts_bytes=0;
      }
      ds->pts_bytes+=p->len; // !!!
      ds->flags=p->flags;
      // unlink packet:
      ds->bytes-=p->len;
      ds->current=p;
      ds->first=p->next;
      if(!ds->first) ds->last=NULL;
      --ds->packs;
      return 1; //ds->buffer_size;
    }
    if(demux->audio->packs>=MAX_PACKS || demux->audio->bytes>=MAX_PACK_BYTES){
      mp_msg(MSGT_DEMUXER,MSGL_ERR,MSGTR_TooManyAudioInBuffer,demux->audio->packs,demux->audio->bytes);
      mp_msg(MSGT_DEMUXER,MSGL_HINT,MSGTR_MaybeNI);
      break;
    }
    if(demux->video->packs>=MAX_PACKS || demux->video->bytes>=MAX_PACK_BYTES){
      mp_msg(MSGT_DEMUXER,MSGL_ERR,MSGTR_TooManyVideoInBuffer,demux->video->packs,demux->video->bytes);
      mp_msg(MSGT_DEMUXER,MSGL_HINT,MSGTR_MaybeNI);
      break;
    }
    if(!demux_fill_buffer(demux,ds)){
       mp_dbg(MSGT_DEMUXER,MSGL_DBG2,"ds_fill_buffer()->demux_fill_buffer() failed\n");
       break; // EOF
    }
  }
  ds->buffer_pos=ds->buffer_size=0;
  ds->buffer=NULL;
  ds->current=NULL;
  mp_msg(MSGT_DEMUXER,MSGL_V,"ds_fill_buffer: EOF reached (stream: %s)  \n",ds==demux->audio?"audio":"video");
  ds->eof=1;
  return 0;
}

int demux_read_data(demux_stream_t *ds,unsigned char* mem,int len){
int x;
int bytes=0;
while(len>0){
  x=ds->buffer_size-ds->buffer_pos;
  if(x==0){
    if(!ds_fill_buffer(ds)) return bytes;
  } else {
    if(x>len) x=len;
    if(mem) memcpy(mem+bytes,&ds->buffer[ds->buffer_pos],x);
    bytes+=x;len-=x;ds->buffer_pos+=x;
  }
}
return bytes;
}

int demux_read_data_pack(demux_stream_t *ds,unsigned char* mem,int len){
int x;
int bytes=0;
while(len>0){
  x=ds->buffer_size-ds->buffer_pos;
  if(x==0){
    if(!ds_fill_buffer(ds)) return bytes;
  } else {
    if(x>len) x=len;
    if(mem) memcpy(mem+bytes,&ds->buffer[ds->buffer_pos],x);
    bytes+=x;len-=x;ds->buffer_pos+=x;
    return bytes; // stop at end of package! (for correct timestamping)
  }
}
return bytes;
}


void ds_free_packs(demux_stream_t *ds){
  demux_packet_t *dp=ds->first;
  while(dp){
    demux_packet_t *dn=dp->next;
    free_demux_packet(dp);
    dp=dn;
  }
  if(ds->asf_packet){
    // free unfinished .asf fragments:
    free(ds->asf_packet->buffer);
    free(ds->asf_packet);
    ds->asf_packet=NULL;
  }
  ds->first=ds->last=NULL;
  ds->packs=0; // !!!!!
  ds->bytes=0;
  if(ds->current) free_demux_packet(ds->current);
  ds->current=NULL;
  ds->buffer=NULL;
  ds->buffer_pos=ds->buffer_size;
  ds->pts=0; ds->pts_bytes=0;
}

int ds_get_packet(demux_stream_t *ds,unsigned char **start){
    while(1){
        int len;
        if(ds->buffer_pos>=ds->buffer_size){
          if(!ds_fill_buffer(ds)){
            // EOF
            *start = NULL;
            return -1;
          }
        }
        len=ds->buffer_size-ds->buffer_pos;
        *start = &ds->buffer[ds->buffer_pos];
        ds->buffer_pos+=len;
        return len;
    }
}

int ds_get_packet_sub(demux_stream_t *ds,unsigned char **start){
    while(1){
        int len;
        if(ds->buffer_pos>=ds->buffer_size){
          *start = NULL;
          if(!ds->packs) return -1; // no sub
          if(!ds_fill_buffer(ds)) return -1; // EOF
        }
        len=ds->buffer_size-ds->buffer_pos;
        *start = &ds->buffer[ds->buffer_pos];
        ds->buffer_pos+=len;
        return len;
    }
}

float ds_get_next_pts(demux_stream_t *ds) {
  demuxer_t* demux = ds->demuxer;
  while(!ds->first) {
    if(demux->audio->packs>=MAX_PACKS || demux->audio->bytes>=MAX_PACK_BYTES){
      mp_msg(MSGT_DEMUXER,MSGL_ERR,MSGTR_TooManyAudioInBuffer,demux->audio->packs,demux->audio->bytes);
      mp_msg(MSGT_DEMUXER,MSGL_HINT,MSGTR_MaybeNI);
      return -1;
    }
    if(demux->video->packs>=MAX_PACKS || demux->video->bytes>=MAX_PACK_BYTES){
      mp_msg(MSGT_DEMUXER,MSGL_ERR,MSGTR_TooManyVideoInBuffer,demux->video->packs,demux->video->bytes);
      mp_msg(MSGT_DEMUXER,MSGL_HINT,MSGTR_MaybeNI);
      return -1;
    }
    if(!demux_fill_buffer(demux,ds))
      return -1;
  }
  return ds->first->pts;
}

// ====================================================================

// commandline options, flags:
extern int force_ni;
extern int pts_from_bps;

//extern int audio_id;
//extern int video_id;
//extern int dvdsub_id;

void demuxer_help(void)
{
  int i;

  mp_msg(MSGT_DEMUXER, MSGL_INFO, "Available demuxers:\n");
  mp_msg(MSGT_DEMUXER, MSGL_INFO, " demuxer:  type  info:  (comment)\n");
  for (i = 0; demuxer_list[i]; i++) {
    if (demuxer_list[i]->type > DEMUXER_TYPE_MAX) // Don't display special demuxers
      continue;
    if (demuxer_list[i]->comment && strlen(demuxer_list[i]->comment))
      mp_msg(MSGT_DEMUXER, MSGL_INFO, "%10s  %2d   %s (%s)\n",
             demuxer_list[i]->name, demuxer_list[i]->type, demuxer_list[i]->info, demuxer_list[i]->comment);
    else
      mp_msg(MSGT_DEMUXER, MSGL_INFO, "%10s  %2d   %s\n",
             demuxer_list[i]->name, demuxer_list[i]->type, demuxer_list[i]->info);
  }
}


/**
 * Get demuxer type for a given demuxer name
 *
 * @param demuxer_name    string with demuxer name of demuxer number
 * @param force           will be set if demuxer should be forced.
 *                        May be NULL.
 * @return                DEMUXER_TYPE_xxx, -1 if error or not found
 */
int get_demuxer_type_from_name(char *demuxer_name, int *force)
{
  int i;
  long type_int;
  char *endptr;

  if (!demuxer_name || !demuxer_name[0])
    return DEMUXER_TYPE_UNKNOWN;
  if (force) *force = demuxer_name[0] == '+';
  if (demuxer_name[0] == '+')
    demuxer_name = &demuxer_name[1];
  for (i = 0; demuxer_list[i]; i++) {
    if (demuxer_list[i]->type > DEMUXER_TYPE_MAX) // Can't select special demuxers from commandline
      continue;
    if (strcmp(demuxer_name, demuxer_list[i]->name) == 0)
      return  demuxer_list[i]->type;
  }

  // No match found, try to parse name as an integer (demuxer number)
  type_int = strtol(demuxer_name, &endptr, 0);
  if (*endptr) // Conversion failed
    return -1;
  if ((type_int > 0) && (type_int <= DEMUXER_TYPE_MAX))
    return (int)type_int;

  return -1;
}

int extension_parsing=1; // 0=off 1=mixed (used only for unstable formats)

/*
  NOTE : Several demuxers may be opened at the same time so
  demuxers should NEVER rely on an external var to enable them
  self. If a demuxer can't do any autodection it should only use
  file_format. The user can explictly set file_format with the -demuxer
  option so there is really no need for another extra var.
  For conivence an option can be added to set file_format directly
  to the right type (ex: rawaudio,rawvideo).
  Also the stream can override the file_format so a demuxer wich rely
  on a special stream type can set file_format at the stream level
  (ex: tv,mf).
*/

static demuxer_t* demux_open_stream(stream_t *stream, int file_format,
                    int force, int audio_id, int video_id, int dvdsub_id,
                    char* filename) {

//int file_format=(*file_format_ptr);

demuxer_t *demuxer=NULL;

sh_video_t *sh_video=NULL;

demuxer_desc_t *demuxer_desc;
int fformat;
int i;

//printf("demux_open(%p,%d,%d,%d,%d)  \n",stream,file_format,audio_id,video_id,dvdsub_id);

// If somebody requested a demuxer check it
if (file_format) {
  if ((demuxer_desc = get_demuxer_desc_from_type(file_format))) {
    demuxer = new_demuxer(stream,demuxer_desc->type,audio_id,video_id,dvdsub_id,filename);
    if (demuxer_desc->check_file) {
      fformat = demuxer_desc->check_file(demuxer);
      if (force)
        fformat = demuxer_desc->type;
      if (fformat != 0) {
        if (fformat == demuxer_desc->type) {
          // Move messages to demuxer detection code?
          mp_msg(MSGT_DEMUXER, MSGL_INFO, MSGTR_Detected_XXX_FileFormat, demuxer_desc->shortdesc);
          file_format = demuxer_desc->type = fformat;
        } else {
          // Format changed after check, recurse
          free_demuxer(demuxer);
          return demux_open_stream(stream, fformat, force,
                   audio_id, video_id, dvdsub_id, filename);
        }
      } else {
        // Check failed for forced demuxer, quit
        free_demuxer(demuxer);
        return NULL;
      }
    }
  }
}

if (demuxer)
  goto dmx_open;

// Test demuxers with safe file checks
for (i = 0; (demuxer_desc = demuxer_list[i]); i++) {
  if (demuxer_desc->safe_check) {
    demuxer = new_demuxer(stream,demuxer_desc->type,audio_id,video_id,dvdsub_id,filename);
    if ((fformat = demuxer_desc->check_file(demuxer)) != 0) {
      if (fformat == demuxer_desc->type) {
        mp_msg(MSGT_DEMUXER, MSGL_INFO, MSGTR_Detected_XXX_FileFormat, demuxer_desc->shortdesc);
        file_format = fformat;
        break;
      } else {
        if (fformat == DEMUXER_TYPE_PLAYLIST)
          return demuxer; // handled in mplayer.c
        // Format changed after check, recurse
        free_demuxer(demuxer);
        demuxer=demux_open_stream(stream, fformat, force,
                  audio_id, video_id, dvdsub_id, filename);
        if(demuxer) return demuxer; // done!
        file_format = DEMUXER_TYPE_UNKNOWN;
      }
    } else {
      free_demuxer(demuxer);
      demuxer = NULL;
    }
  }
}

if (demuxer)
  goto dmx_open;

// If no forced demuxer perform file extension based detection
// Ok. We're over the stable detectable fileformats, the next ones are a bit
// fuzzy. So by default (extension_parsing==1) try extension-based detection
// first:
if(file_format==DEMUXER_TYPE_UNKNOWN && filename && extension_parsing==1){
  file_format=demuxer_type_by_filename(filename);
  if(file_format!=DEMUXER_TYPE_UNKNOWN){
    // we like recursion :)
    demuxer=demux_open_stream(stream, file_format, force,
              audio_id, video_id, dvdsub_id, filename);
    if(demuxer) return demuxer; // done!
    file_format=DEMUXER_TYPE_UNKNOWN; // continue fuzzy guessing...
    mp_msg(MSGT_DEMUXER,MSGL_V,"demuxer: continue fuzzy content-based format guessing...\n");
  }
}

// Try detection for all other demuxers
for (i = 0; (demuxer_desc = demuxer_list[i]); i++) {
  if (!demuxer_desc->safe_check && demuxer_desc->check_file) {
    demuxer = new_demuxer(stream,demuxer_desc->type,audio_id,video_id,dvdsub_id,filename);
    if ((fformat = demuxer_desc->check_file(demuxer)) != 0) {
      if (fformat == demuxer_desc->type) {
        mp_msg(MSGT_DEMUXER, MSGL_INFO, MSGTR_Detected_XXX_FileFormat, demuxer_desc->shortdesc);
        file_format = fformat;
        break;
      } else {
        if (fformat == DEMUXER_TYPE_PLAYLIST)
          return demuxer; // handled in mplayer.c
        // Format changed after check, recurse
        free_demuxer(demuxer);
        demuxer=demux_open_stream(stream, fformat, force,
                  audio_id, video_id, dvdsub_id, filename);
        if(demuxer) return demuxer; // done!
        file_format = DEMUXER_TYPE_UNKNOWN;
      }
    } else {
      free_demuxer(demuxer);
      demuxer = NULL;
    }
  }
}

//=============== Unknown, exiting... ===========================
if(file_format==DEMUXER_TYPE_UNKNOWN || demuxer == NULL){
  //mp_msg(MSGT_DEMUXER,MSGL_ERR,MSGTR_FormatNotRecognized); // will be done by mplayer.c after fallback to playlist-parsing
  return NULL;
}
//====== File format recognized, set up these for compatibility: =========
dmx_open:

demuxer->file_format=file_format;

if (demuxer->desc->open)
  if (!(demuxer = demuxer->desc->open(demuxer)))
    return NULL;

if ((sh_video=demuxer->video->sh) && sh_video->bih)
  mp_msg(MSGT_DEMUX,MSGL_INFO,"VIDEO:  [%.4s]  %ldx%ld  %dbpp  %5.3f fps  %5.1f kbps (%4.1f kbyte/s)\n",
    (char *)&sh_video->bih->biCompression,
    sh_video->bih->biWidth,
    sh_video->bih->biHeight,
    sh_video->bih->biBitCount,
    sh_video->fps,
    sh_video->i_bps*0.008f,
    sh_video->i_bps/1024.0f );
return demuxer;
}

char* audio_stream = NULL;
char* sub_stream = NULL;
int demuxer_type = 0; // used by rawaudio and rawvideo
int audio_stream_cache = 0;

char *demuxer_name = NULL; // parameter from -demuxer
char *audio_demuxer_name = NULL; // parameter from -audio-demuxer
char *sub_demuxer_name = NULL; // parameter from -sub-demuxer

extern int hr_mp3_seek;

extern float stream_cache_min_percent;
extern float stream_cache_seek_min_percent;

demuxer_t* demux_open(stream_t *vs,int file_format,int audio_id,int video_id,int dvdsub_id,char* filename){
  stream_t *as = NULL,*ss = NULL;
  demuxer_t *vd,*ad = NULL,*sd = NULL;
  int afmt =DEMUXER_TYPE_UNKNOWN,sfmt = DEMUXER_TYPE_UNKNOWN ;
  int audio_demuxer_type = 0, sub_demuxer_type = 0;
  int demuxer_force = 0, audio_demuxer_force = 0,
      sub_demuxer_force = 0;

  demux_aid_vid_mismatch = 0;

  if ((demuxer_type = get_demuxer_type_from_name(demuxer_name, &demuxer_force)) < 0) {
    mp_msg(MSGT_DEMUXER,MSGL_ERR,"-demuxer %s does not exist.\n",demuxer_name);
  }
  if ((audio_demuxer_type = get_demuxer_type_from_name(audio_demuxer_name, &audio_demuxer_force)) < 0) {
    mp_msg(MSGT_DEMUXER,MSGL_ERR,"-audio-demuxer %s does not exist.\n",audio_demuxer_name);
  }
  if ((sub_demuxer_type = get_demuxer_type_from_name(sub_demuxer_name, &sub_demuxer_force)) < 0) {
    mp_msg(MSGT_DEMUXER,MSGL_ERR,"-sub-demuxer %s does not exist.\n",sub_demuxer_name);
  }

  if(audio_stream) {
    as = open_stream(audio_stream,0,&afmt);
    if(!as) {
      mp_msg(MSGT_DEMUXER,MSGL_ERR,MSGTR_CannotOpenAudioStream,audio_stream);
      return NULL;
    }
    if(audio_stream_cache) {
      if(!stream_enable_cache(as,audio_stream_cache*1024,audio_stream_cache*1024*(stream_cache_min_percent / 100.0),
			      audio_stream_cache*1024*(stream_cache_seek_min_percent / 100.0))) {
	free_stream(as);
	mp_msg(MSGT_DEMUXER,MSGL_ERR,"Can't enable audio stream cache\n");
	return NULL;
      }
    }
  }
  if(sub_stream) {
    ss = open_stream(sub_stream,0,&sfmt);
    if(!ss) {
      mp_msg(MSGT_DEMUXER,MSGL_ERR,MSGTR_CannotOpenSubtitlesStream,sub_stream);
      return NULL;
    }
  }

  vd = demux_open_stream(vs, demuxer_type ? demuxer_type : file_format,
         demuxer_force, audio_stream ? -2 : audio_id, video_id,
         sub_stream ? -2 : dvdsub_id, filename);
  if(!vd) {
    if(as) free_stream(as);
    if(ss) free_stream(ss);
    return NULL;
  }
  if(as) {
    ad = demux_open_stream(as, audio_demuxer_type ? audio_demuxer_type : afmt,
           audio_demuxer_force, audio_id, -2, -2, audio_stream);
    if(!ad) {
      mp_msg(MSGT_DEMUXER,MSGL_WARN,MSGTR_OpeningAudioDemuxerFailed,audio_stream);
      free_stream(as);
    }
    else if(ad->audio->sh && ((sh_audio_t*)ad->audio->sh)->format == 0x55) // MP3
      hr_mp3_seek=1; // Enable high res seeking
  }
  if(ss) {
    sd = demux_open_stream(ss, sub_demuxer_type ? sub_demuxer_type : sfmt,
           sub_demuxer_force, -2, -2, dvdsub_id, sub_stream);
    if(!sd) {
      mp_msg(MSGT_DEMUXER,MSGL_WARN,MSGTR_OpeningSubtitlesDemuxerFailed,sub_stream);
      free_stream(ss);
    }
  }

  if(ad && sd)
    return new_demuxers_demuxer(vd,ad,sd);
  else if(ad)
    return new_demuxers_demuxer(vd,ad,vd);
  else if(sd)
    return new_demuxers_demuxer(vd,vd,sd);
  else
    return vd;
}


int demux_seek(demuxer_t *demuxer,float rel_seek_secs,int flags){
    demux_stream_t *d_audio=demuxer->audio;
    demux_stream_t *d_video=demuxer->video;
    sh_audio_t *sh_audio=d_audio->sh;
    sh_video_t *sh_video=d_video->sh;

if(!demuxer->seekable){
    if(demuxer->file_format==DEMUXER_TYPE_AVI)
	mp_msg(MSGT_SEEK,MSGL_WARN,MSGTR_CantSeekRawAVI);
#ifdef USE_TV
    else if (demuxer->file_format==DEMUXER_TYPE_TV)
	mp_msg(MSGT_SEEK,MSGL_WARN,MSGTR_TVInputNotSeekable);
#endif
    else
	mp_msg(MSGT_SEEK,MSGL_WARN,MSGTR_CantSeekFile);
    return 0;
}

    // clear demux buffers:
    if(sh_audio){ ds_free_packs(d_audio);sh_audio->a_buffer_len=0;}
    ds_free_packs(d_video);
    
    demuxer->stream->eof=0; // clear eof flag
    demuxer->video->eof=0;
    demuxer->audio->eof=0;

#if 0
    if(sh_audio) sh_audio->timer=sh_video->timer;
#else
    if(sh_audio) sh_audio->delay=0;
    if(sh_video) sh_video->timer=0; // !!!!!!
#endif

if (demuxer->desc->seek)
    demuxer->desc->seek(demuxer,rel_seek_secs,flags);

return 1;
}

int demux_info_add(demuxer_t *demuxer, char *opt, char *param)
{
    char **info = demuxer->info;
    int n = 0;


    for(n = 0; info && info[2*n] != NULL; n++) 
      {
	if(!strcasecmp(opt,info[2*n]))
	  {
	    mp_msg(MSGT_DEMUX, MSGL_WARN,MSGTR_DemuxerInfoAlreadyPresent,opt);
	    return 0;
	  }
      }
    
    info = demuxer->info = (char**)realloc(info,(2*(n+2))*sizeof(char*));
    info[2*n] = strdup(opt);
    info[2*n+1] = strdup(param);
    memset(&info[2*(n+1)],0,2*sizeof(char*));

    return 1;
}

int demux_info_print(demuxer_t *demuxer)
{
    char **info = demuxer->info;
    int n;

    if(!info)
      return 0;

    mp_msg(MSGT_DEMUX, MSGL_INFO,MSGTR_ClipInfo);
    for(n = 0; info[2*n] != NULL ; n++)
    {
      mp_msg(MSGT_DEMUX, MSGL_INFO, " %s: %s\n",info[2*n],info[2*n+1]);
      if (identify) {
        mp_msg(MSGT_GLOBAL, MSGL_INFO, "ID_CLIP_INFO_NAME%d=%s\n", n, info[2*n]);
        mp_msg(MSGT_GLOBAL, MSGL_INFO, "ID_CLIP_INFO_VALUE%d=%s\n", n, info[2*n+1]);
      }
    }
    if (identify)
      mp_msg(MSGT_GLOBAL, MSGL_INFO, "ID_CLIP_INFO_N=%d\n", n);

    return 0;
}

char* demux_info_get(demuxer_t *demuxer, char *opt) {
  int i;
  char **info = demuxer->info;

  for(i = 0; info && info[2*i] != NULL; i++) {
    if(!strcasecmp(opt,info[2*i]))
      return info[2*i+1];
  }

  return NULL;
}

int demux_control(demuxer_t *demuxer, int cmd, void *arg) {

    if (demuxer->desc->control)
      return demuxer->desc->control(demuxer,cmd,arg);

    return DEMUXER_CTRL_NOTIMPL;
}



double demuxer_get_time_length(demuxer_t *demuxer){
    double get_time_ans;
    sh_video_t *sh_video = demuxer->video->sh;
    // <= 0 means DEMUXER_CTRL_NOTIMPL or DEMUXER_CTRL_DONTKNOW
    if (demux_control(demuxer, DEMUXER_CTRL_GET_TIME_LENGTH,(void *)&get_time_ans)<=0)  {
      if (sh_video && sh_video->i_bps)
        get_time_ans = (double)(demuxer->movi_end-demuxer->movi_start)/sh_video->i_bps;
      else
        get_time_ans=0;     
    }
    return get_time_ans;
}

int demuxer_get_percent_pos(demuxer_t *demuxer){     
    int ans = 0;
    int res = demux_control(demuxer, DEMUXER_CTRL_GET_PERCENT_POS, &ans);
    int len = (demuxer->movi_end - demuxer->movi_start) / 100;
    if (res <= 0) {
      if (len > 0)
      ans = (demuxer->filepos - demuxer->movi_start) / len;
      else
       ans = 0;
    }
    if (ans < 0) ans = 0;
    if (ans > 100) ans = 100;
    return ans;
}

int demuxer_switch_audio(demuxer_t *demuxer, int index){     
    int res = demux_control(demuxer, DEMUXER_CTRL_SWITCH_AUDIO, &index);
    if (res == DEMUXER_CTRL_NOTIMPL)
      index = demuxer->audio->id;
    return index;
}

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

#include "../libvo/fastmemcpy.h"

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

demuxer_t* new_demuxer(stream_t *stream,int type,int a_id,int v_id,int s_id){
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
        mp_msg(MSGT_DEMUXER,MSGL_V,MSGTR_FoundAudioStream,id);
        demuxer->a_streams[id]=malloc(sizeof(sh_audio_t));
        memset(demuxer->a_streams[id],0,sizeof(sh_audio_t));
    }
    return demuxer->a_streams[id];
}

void free_sh_audio(sh_audio_t* sh){
    mp_msg(MSGT_DEMUXER,MSGL_V,"DEMUXER: freeing sh_audio at %p  \n",sh);
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
    }
    return demuxer->v_streams[id];
}

void free_sh_video(sh_video_t* sh){
    mp_msg(MSGT_DEMUXER,MSGL_V,"DEMUXER: freeing sh_video at %p  \n",sh);
    if(sh->bih) free(sh->bih);
    free(sh);
}

extern void demux_close_vivo(demuxer_t *demuxer);
extern void demux_close_real(demuxer_t *demuxer);
extern void demux_close_y4m(demuxer_t *demuxer);
extern void demux_close_mf(demuxer_t* demuxer);
extern void demux_close_roq(demuxer_t* demuxer);
extern void demux_close_film(demuxer_t* demuxer);
extern void demux_close_bmp(demuxer_t* demuxer);
extern void demux_close_fli(demuxer_t* demuxer);
extern void demux_close_nuv(demuxer_t* demuxer);
extern void demux_close_audio(demuxer_t* demuxer);
extern void demux_close_ogg(demuxer_t* demuxer);
extern void demux_close_rtp(demuxer_t* demuxer);
extern void demux_close_demuxers(demuxer_t* demuxer);
extern void demux_close_avi(demuxer_t *demuxer);
extern void demux_close_rawdv(demuxer_t* demuxer);
extern void demux_close_pva(demuxer_t* demuxer);
extern void demux_close_smjpeg(demuxer_t* demuxer);
extern void demux_close_xmms(demuxer_t* demuxer);
extern void demux_close_gif(demuxer_t* demuxer);
extern void demux_close_lmlm4(demuxer_t* demuxer);
extern void demux_close_ts(demuxer_t* demuxer);
extern void demux_close_mkv(demuxer_t* demuxer);
extern void demux_close_ra(demuxer_t* demuxer);
extern void demux_close_ty(demuxer_t* demuxer);


#ifdef USE_TV
#include "tv.h"
extern int tv_param_on;

extern int demux_tv_fill_buffer(demuxer_t *demux, demux_stream_t *ds);
extern int demux_open_tv(demuxer_t *demuxer);
#if defined(USE_TV) && (defined(HAVE_TV_V4L) || defined(HAVE_TV_V4L2))
extern void demux_close_tv(demuxer_t *demuxer);
#endif
#endif

void free_demuxer(demuxer_t *demuxer){
    int i;
    mp_msg(MSGT_DEMUXER,MSGL_V,"DEMUXER: freeing demuxer at %p  \n",demuxer);
    switch(demuxer->type) {
    case DEMUXER_TYPE_PVA:
      demux_close_pva(demuxer); break;
    case DEMUXER_TYPE_VIVO:
      demux_close_vivo(demuxer); break;
    case DEMUXER_TYPE_REAL:
      demux_close_real(demuxer); break;
    case DEMUXER_TYPE_Y4M:
      demux_close_y4m(demuxer); break;
    case DEMUXER_TYPE_MF:
      demux_close_mf(demuxer); break;
    case DEMUXER_TYPE_ROQ:
      demux_close_roq(demuxer);  break;
    case DEMUXER_TYPE_FILM:
      demux_close_film(demuxer); break;
    case DEMUXER_TYPE_BMP:
      demux_close_bmp(demuxer); break;
    case DEMUXER_TYPE_FLI:
      demux_close_fli(demuxer); break;
    case DEMUXER_TYPE_NUV:
      demux_close_nuv(demuxer); break;
    case DEMUXER_TYPE_MPEG_TY:
      demux_close_ty(demuxer); break;
#if defined(USE_TV) && (defined(HAVE_TV_V4L) || defined(HAVE_TV_V4L2))
    case DEMUXER_TYPE_TV:
	demux_close_tv(demuxer); break;
#endif
#ifdef HAVE_LIBDV095
    case DEMUXER_TYPE_RAWDV:
      demux_close_rawdv(demuxer); break;
#endif
    case DEMUXER_TYPE_AUDIO:
      demux_close_audio(demuxer); break;
#ifdef HAVE_OGGVORBIS
    case DEMUXER_TYPE_OGG:
      demux_close_ogg(demuxer); break;
#endif
#ifdef HAVE_MATROSKA
    case DEMUXER_TYPE_MATROSKA:
      demux_close_mkv(demuxer); break;
#endif
#ifdef STREAMING_LIVE_DOT_COM
    case DEMUXER_TYPE_RTP:
      demux_close_rtp(demuxer); break;
#endif
    case DEMUXER_TYPE_SMJPEG:
      demux_close_smjpeg(demuxer); return;
    case DEMUXER_TYPE_DEMUXERS:
      demux_close_demuxers(demuxer); return;
    case DEMUXER_TYPE_AVI: 
    case DEMUXER_TYPE_AVI_NI:
    case DEMUXER_TYPE_AVI_NINI:
      demux_close_avi(demuxer); return;
#ifdef HAVE_XMMS
    case DEMUXER_TYPE_XMMS:
      demux_close_xmms(demuxer); break;
#endif
#ifdef HAVE_GIF
    case DEMUXER_TYPE_GIF:
      demux_close_gif(demuxer); break;
#endif
    case DEMUXER_TYPE_LMLM4:
      demux_close_lmlm4(demuxer); break;
    case DEMUXER_TYPE_MPEG_TS:
    case DEMUXER_TYPE_MPEG4_IN_TS:
      demux_close_ts(demuxer); break;
    case DEMUXER_TYPE_REALAUDIO:
      demux_close_ra(demuxer); break;

    }
    // free streams:
    for(i=0;i<256;i++){
	if(demuxer->a_streams[i]) free_sh_audio(demuxer->a_streams[i]);
	if(demuxer->v_streams[i]) free_sh_video(demuxer->v_streams[i]);
    }
    //if(sh_audio) free_sh_audio(sh_audio);
    //if(sh_video) free_sh_video(sh_video);
    // free demuxers:
    free_demuxer_stream(demuxer->audio);
    free_demuxer_stream(demuxer->video);
    free_demuxer_stream(demuxer->sub);
    if(demuxer->info) {
      for(i=0;demuxer->info[i] != NULL; i++)
	free(demuxer->info[i]);
      free(demuxer->info);
    }
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
int demux_mf_fill_buffer( demuxer_t *demux);
int demux_roq_fill_buffer(demuxer_t *demux);
int demux_film_fill_buffer(demuxer_t *demux);
int demux_bmp_fill_buffer(demuxer_t *demux);
int demux_fli_fill_buffer(demuxer_t *demux);
int demux_mpg_es_fill_buffer(demuxer_t *demux);
int demux_mpg_fill_buffer(demuxer_t *demux);
int demux_ty_fill_buffer(demuxer_t *demux);
int demux_avi_fill_buffer(demuxer_t *demux);
int demux_avi_fill_buffer_ni(demuxer_t *demux,demux_stream_t *ds);
int demux_avi_fill_buffer_nini(demuxer_t *demux,demux_stream_t *ds);
int demux_asf_fill_buffer(demuxer_t *demux);
int demux_mov_fill_buffer(demuxer_t *demux,demux_stream_t* ds);
int demux_vivo_fill_buffer(demuxer_t *demux);
int demux_real_fill_buffer(demuxer_t *demuxer);
int demux_nuv_fill_buffer(demuxer_t *demux);
int demux_rtp_fill_buffer(demuxer_t *demux, demux_stream_t* ds);
int demux_rawdv_fill_buffer(demuxer_t *demuxer);
int demux_y4m_fill_buffer(demuxer_t *demux);
int demux_audio_fill_buffer(demux_stream_t *ds);
int demux_pva_fill_buffer(demuxer_t *demux);
int demux_xmms_fill_buffer(demuxer_t *demux,demux_stream_t *ds);
int demux_gif_fill_buffer(demuxer_t *demux);
int demux_ts_fill_buffer(demuxer_t *demux);
int demux_ra_fill_buffer(demuxer_t *demux);

extern int demux_demuxers_fill_buffer(demuxer_t *demux,demux_stream_t *ds);
extern int demux_ogg_fill_buffer(demuxer_t *d);
extern int demux_rawaudio_fill_buffer(demuxer_t* demuxer, demux_stream_t *ds);
extern int demux_rawvideo_fill_buffer(demuxer_t* demuxer, demux_stream_t *ds);
extern int demux_smjpeg_fill_buffer(demuxer_t* demux);
extern int demux_lmlm4_fill_buffer(demuxer_t* demux);
extern int demux_mkv_fill_buffer(demuxer_t *d);

int demux_fill_buffer(demuxer_t *demux,demux_stream_t *ds){
  // Note: parameter 'ds' can be NULL!
//  printf("demux->type=%d\n",demux->type);
  switch(demux->type){
    case DEMUXER_TYPE_MF: return demux_mf_fill_buffer(demux);
    case DEMUXER_TYPE_ROQ: return demux_roq_fill_buffer(demux);
    case DEMUXER_TYPE_FILM: return demux_film_fill_buffer(demux);
    case DEMUXER_TYPE_BMP: return demux_bmp_fill_buffer(demux);
    case DEMUXER_TYPE_FLI: return demux_fli_fill_buffer(demux);
    case DEMUXER_TYPE_MPEG_TY: return demux_ty_fill_buffer( demux );
    case DEMUXER_TYPE_MPEG4_ES:
    case DEMUXER_TYPE_H264_ES:
    case DEMUXER_TYPE_MPEG_ES: return demux_mpg_es_fill_buffer(demux);
    case DEMUXER_TYPE_MPEG_PS: return demux_mpg_fill_buffer(demux);
    case DEMUXER_TYPE_AVI: return demux_avi_fill_buffer(demux);
    case DEMUXER_TYPE_AVI_NI: return demux_avi_fill_buffer_ni(demux,ds);
    case DEMUXER_TYPE_AVI_NINI: return demux_avi_fill_buffer_nini(demux,ds);
    case DEMUXER_TYPE_ASF: return demux_asf_fill_buffer(demux);
    case DEMUXER_TYPE_MOV: return demux_mov_fill_buffer(demux,ds);
    case DEMUXER_TYPE_VIVO: return demux_vivo_fill_buffer(demux);
    case DEMUXER_TYPE_PVA: return demux_pva_fill_buffer(demux);
#ifdef HAVE_LIBDV095
    case DEMUXER_TYPE_RAWDV: return demux_rawdv_fill_buffer(demux);
#endif
    case DEMUXER_TYPE_REAL: return demux_real_fill_buffer(demux);
    case DEMUXER_TYPE_NUV: return demux_nuv_fill_buffer(demux);
#ifdef USE_TV
    case DEMUXER_TYPE_TV: return demux_tv_fill_buffer(demux, ds);
#endif
    case DEMUXER_TYPE_Y4M: return demux_y4m_fill_buffer(demux);
    case DEMUXER_TYPE_AUDIO: return demux_audio_fill_buffer(ds);
#ifdef HAVE_XMMS
    case DEMUXER_TYPE_XMMS: return demux_xmms_fill_buffer(demux,ds);
#endif
    case DEMUXER_TYPE_DEMUXERS: return demux_demuxers_fill_buffer(demux,ds);
#ifdef HAVE_OGGVORBIS
    case DEMUXER_TYPE_OGG: return demux_ogg_fill_buffer(demux);
#endif
#ifdef HAVE_MATROSKA
    case DEMUXER_TYPE_MATROSKA: return demux_mkv_fill_buffer(demux);
#endif
    case DEMUXER_TYPE_RAWAUDIO: return demux_rawaudio_fill_buffer(demux,ds);
    case DEMUXER_TYPE_RAWVIDEO: return demux_rawvideo_fill_buffer(demux,ds);
#ifdef STREAMING_LIVE_DOT_COM
    case DEMUXER_TYPE_RTP: return demux_rtp_fill_buffer(demux, ds);
#endif
    case DEMUXER_TYPE_SMJPEG: return demux_smjpeg_fill_buffer(demux);
#ifdef HAVE_GIF
    case DEMUXER_TYPE_GIF: return demux_gif_fill_buffer(demux);
#endif
    case DEMUXER_TYPE_LMLM4: return demux_lmlm4_fill_buffer(demux);
    case DEMUXER_TYPE_MPEG_TS: 
    case DEMUXER_TYPE_MPEG4_IN_TS: 
	return demux_ts_fill_buffer(demux);
    case DEMUXER_TYPE_REALAUDIO: return demux_ra_fill_buffer(demux);
  }
  return 0;
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

// feed-back from demuxers:
extern int num_elementary_packets100; // for MPEG-ES fileformat detection
extern int num_elementary_packets101;
extern int num_elementary_packetsPES;
extern int num_elementary_packets1B6;
extern int num_elementary_packets12x;
extern int num_h264_slice; //combined slice
extern int num_h264_dpa; //DPA Slice
extern int num_h264_dpb; //DPB Slice
extern int num_h264_dpc; //DPC Slice
extern int num_h264_idr; //IDR Slice
extern int num_h264_sps;
extern int num_h264_pps;
extern int num_mp3audio_packets;

// commandline options, flags:
extern int force_ni;
extern int pts_from_bps;

//extern int audio_id;
//extern int video_id;
//extern int dvdsub_id;

int asf_check_header(demuxer_t *demuxer);
int read_asf_header(demuxer_t *demuxer);
demux_stream_t* demux_avi_select_stream(demuxer_t *demux,unsigned int id);
demuxer_t* demux_open_avi(demuxer_t* demuxer);
int mov_check_file(demuxer_t* demuxer);
int mov_read_header(demuxer_t* demuxer);
int demux_open_fli(demuxer_t* demuxer);
int demux_open_mf(demuxer_t* demuxer);
int demux_open_film(demuxer_t* demuxer);
int demux_open_bmp(demuxer_t* demuxer);
int demux_open_roq(demuxer_t* demuxer);
#ifdef HAVE_LIBDV095
int demux_open_rawdv(demuxer_t* demuxer);
extern int rawdv_check_file(demuxer_t *demuxer);
#endif

extern int vivo_check_file(demuxer_t *demuxer);
extern void demux_open_vivo(demuxer_t *demuxer);
extern int y4m_check_file(demuxer_t *demuxer);
extern void demux_open_y4m(demuxer_t *demuxer);
extern int roq_check_file(demuxer_t *demuxer);
extern int pva_check_file(demuxer_t * demuxer);
extern demuxer_t * demux_open_pva(demuxer_t * demuxer);
extern int real_check_file(demuxer_t *demuxer);
extern void demux_open_real(demuxer_t *demuxer);
extern int nuv_check_file(demuxer_t *demuxer);
extern void demux_open_nuv(demuxer_t *demuxer);
extern int demux_audio_open(demuxer_t* demuxer);
extern int demux_ogg_open(demuxer_t* demuxer);
extern int demux_rawaudio_open(demuxer_t* demuxer);
extern int demux_rawvideo_open(demuxer_t* demuxer);
extern int smjpeg_check_file(demuxer_t *demuxer);
extern int demux_open_smjpeg(demuxer_t* demuxer);
extern int bmp_check_file(demuxer_t *demuxer);
extern int demux_xmms_open(demuxer_t* demuxer);
extern int gif_check_file(demuxer_t *demuxer);
extern int demux_open_gif(demuxer_t* demuxer);
extern int lmlm4_check_file(demuxer_t* demuxer);
extern int demux_open_lmlm4(demuxer_t* demuxer);
extern int ts_check_file(demuxer_t * demuxer);
extern int demux_open_ts(demuxer_t *demuxer);
extern int demux_open_mkv(demuxer_t *demuxer);
extern int ra_check_file(demuxer_t *demuxer);
extern int demux_open_ra(demuxer_t* demuxer);
#ifdef HAVE_MATROSKA
extern int demux_mkv_open(demuxer_t *demuxer);
#endif

extern demuxer_t* init_avi_with_ogg(demuxer_t* demuxer);

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

static demuxer_t* demux_open_stream(stream_t *stream,int file_format,int audio_id,int video_id,int dvdsub_id,char* filename){

//int file_format=(*file_format_ptr);

demuxer_t *demuxer=NULL;

demux_stream_t *d_audio=NULL;
demux_stream_t *d_video=NULL;

sh_audio_t *sh_audio=NULL;
sh_video_t *sh_video=NULL;

//printf("demux_open(%p,%d,%d,%d,%d)  \n",stream,file_format,audio_id,video_id,dvdsub_id);

if(file_format == DEMUXER_TYPE_RAWAUDIO) {
  demuxer = new_demuxer(stream,DEMUXER_TYPE_RAWAUDIO,audio_id,video_id,dvdsub_id);
}
if(file_format == DEMUXER_TYPE_RAWVIDEO) {
  demuxer = new_demuxer(stream,DEMUXER_TYPE_RAWVIDEO,audio_id,video_id,dvdsub_id);
}

#ifdef USE_TV
//=============== Try to open as TV-input: =================
if(file_format==DEMUXER_TYPE_TV){
  demuxer=new_demuxer(stream,DEMUXER_TYPE_TV,audio_id,video_id,dvdsub_id);
  mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_DetectedTV);
}
#endif

//=============== Try to open as multi file: =================
if(file_format==DEMUXER_TYPE_MF){
     demuxer=new_demuxer(stream,DEMUXER_TYPE_MF,audio_id,video_id,dvdsub_id);
     mp_msg( MSGT_DEMUXER,MSGL_INFO,"[demuxer] mf support.\n" );
}

//=============== Try to open as AVI file: =================
if(file_format==DEMUXER_TYPE_UNKNOWN || file_format==DEMUXER_TYPE_AVI){
  demuxer=new_demuxer(stream,DEMUXER_TYPE_AVI,audio_id,video_id,dvdsub_id);
  { //---- RIFF header:
    int id=stream_read_dword_le(demuxer->stream); // "RIFF"
    if(id==mmioFOURCC('R','I','F','F')){
      stream_read_dword_le(demuxer->stream); //filesize
      id=stream_read_dword_le(demuxer->stream); // "AVI "
      if(id==formtypeAVI){ 
        mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat,"AVI");
        file_format=DEMUXER_TYPE_AVI;
      } else {
        free_demuxer(demuxer);
        demuxer = NULL;
      }	
    }
  }
}
//=============== Try to open as Y4M file: =================
if(file_format==DEMUXER_TYPE_UNKNOWN || file_format==DEMUXER_TYPE_Y4M){
  demuxer=new_demuxer(stream,DEMUXER_TYPE_Y4M,audio_id,video_id,dvdsub_id);
  if(y4m_check_file(demuxer)){
      mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat,"YUV4MPEG2");
      file_format=DEMUXER_TYPE_Y4M;
  } else {
      free_demuxer(demuxer);
      demuxer = NULL;
  }
}
//=============== Try to open as ASF file: =================
if(file_format==DEMUXER_TYPE_UNKNOWN || file_format==DEMUXER_TYPE_ASF){
  demuxer=new_demuxer(stream,DEMUXER_TYPE_ASF,audio_id,video_id,dvdsub_id);
  if(asf_check_header(demuxer)){
      mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat,"ASF");
      file_format=DEMUXER_TYPE_ASF;
  } else {
      free_demuxer(demuxer);
      demuxer = NULL;
  }
}
//=============== Try to open as NUV file: =================
if(file_format==DEMUXER_TYPE_UNKNOWN || file_format==DEMUXER_TYPE_NUV){
  demuxer=new_demuxer(stream,DEMUXER_TYPE_NUV,audio_id,video_id,dvdsub_id);
  if(nuv_check_file(demuxer)){
      mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat,"NuppelVideo");
      file_format=DEMUXER_TYPE_NUV;
  } else {
      free_demuxer(demuxer);
      demuxer = NULL;
  }
}
//=============== Try to open as REAL file: =================
if(file_format==DEMUXER_TYPE_UNKNOWN || file_format==DEMUXER_TYPE_REAL){
  demuxer=new_demuxer(stream,DEMUXER_TYPE_REAL,audio_id,video_id,dvdsub_id);
  if(real_check_file(demuxer)){
      mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat,"REAL");
      file_format=DEMUXER_TYPE_REAL;
  } else {
      free_demuxer(demuxer);
      demuxer = NULL;
  }
}
//=============== Try to open as SMJPEG file: =================
if(file_format==DEMUXER_TYPE_UNKNOWN || file_format==DEMUXER_TYPE_SMJPEG){
  demuxer=new_demuxer(stream,DEMUXER_TYPE_SMJPEG,audio_id,video_id,dvdsub_id);
  if(smjpeg_check_file(demuxer)){
      mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat,"SMJPEG");
      file_format=DEMUXER_TYPE_SMJPEG;
  } else {
      free_demuxer(demuxer);
      demuxer = NULL;
  }
}
#ifdef HAVE_MATROSKA
//=============== Try to open as Matroska file: =================
if(file_format==DEMUXER_TYPE_UNKNOWN || file_format==DEMUXER_TYPE_MATROSKA){
  demuxer=new_demuxer(stream,DEMUXER_TYPE_MATROSKA,audio_id,video_id,dvdsub_id);
  if(demux_mkv_open(demuxer)){
      mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat,"Matroska");
      file_format=DEMUXER_TYPE_MATROSKA;
  } else {
      free_demuxer(demuxer);
      demuxer = NULL;
  }
}
#endif
//=============== Try to open as REALAUDIO file: =================
if(file_format==DEMUXER_TYPE_UNKNOWN || file_format==DEMUXER_TYPE_REALAUDIO){
  demuxer=new_demuxer(stream,DEMUXER_TYPE_REALAUDIO,audio_id,video_id,dvdsub_id);
  if(ra_check_file(demuxer)){
      mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat,"REALAUDIO");
      file_format=DEMUXER_TYPE_REALAUDIO;
  } else {
      free_demuxer(demuxer);
      demuxer = NULL;
  }
}

//=============== Try based on filename EXTENSION: =================
// Ok. We're over the stable detectable fileformats, the next ones are a bit
// fuzzy. So by default (extension_parsing==1) try extension-based detection
// first:
if(file_format==DEMUXER_TYPE_UNKNOWN && filename && extension_parsing==1){
  file_format=demuxer_type_by_filename(filename);
  if(file_format!=DEMUXER_TYPE_UNKNOWN){
    // we like recursion :)
    demuxer=demux_open_stream(stream,file_format,audio_id,video_id,dvdsub_id,NULL);
    if(demuxer) return demuxer; // done!
    file_format=DEMUXER_TYPE_UNKNOWN; // continue fuzzy guessing...
    mp_msg(MSGT_DEMUXER,MSGL_V,"demuxer: continue fuzzy content-based format guessing...\n");
  }
}

//=============== Try to open as MOV file: =================
if(file_format==DEMUXER_TYPE_UNKNOWN || file_format==DEMUXER_TYPE_MOV){
  demuxer=new_demuxer(stream,DEMUXER_TYPE_MOV,audio_id,video_id,dvdsub_id);
  if(mov_check_file(demuxer)){
      mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat,"QuickTime/MOV");
      file_format=demuxer->type;
  } else {
      free_demuxer(demuxer);
      demuxer = NULL;
  }
}
//=============== Try to open as VIVO file: =================
if(file_format==DEMUXER_TYPE_UNKNOWN || file_format==DEMUXER_TYPE_VIVO){
  demuxer=new_demuxer(stream,DEMUXER_TYPE_VIVO,audio_id,video_id,dvdsub_id);
  if(vivo_check_file(demuxer)){
      mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat, "VIVO");
      file_format=DEMUXER_TYPE_VIVO;
  } else {
    free_demuxer(demuxer);
    demuxer = NULL;
  }
}
//=============== Try to open as FLI file: =================
if(file_format==DEMUXER_TYPE_UNKNOWN || file_format==DEMUXER_TYPE_FLI){
  demuxer=new_demuxer(stream,DEMUXER_TYPE_FLI,audio_id,video_id,dvdsub_id);
  {
    int id;
    stream_seek(demuxer->stream, 4);
    id=stream_read_word_le(demuxer->stream);
    // check for the FLI file magic number
    if((id==0xAF11) || (id==0xAF12)){ 
      mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat,"FLI");
      file_format=DEMUXER_TYPE_FLI;
    } else {
      free_demuxer(demuxer);
      demuxer = NULL;
    }
  }
}
//=============== Try to open as FILM file: =================
if(file_format==DEMUXER_TYPE_UNKNOWN || file_format==DEMUXER_TYPE_FILM){
  demuxer=new_demuxer(stream,DEMUXER_TYPE_FILM,audio_id,video_id,dvdsub_id);
  {
    int signature=stream_read_fourcc(demuxer->stream);
    // check for the FLI file magic number
    if(signature==mmioFOURCC('F', 'I', 'L', 'M')){ 
      mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat,"FILM");
      file_format=DEMUXER_TYPE_FILM;
    } else {
      free_demuxer(demuxer);
      demuxer = NULL;
    }
  }
}
//=============== Try to open as RoQ file: =================
if(file_format==DEMUXER_TYPE_UNKNOWN || file_format==DEMUXER_TYPE_ROQ){
  demuxer=new_demuxer(stream,DEMUXER_TYPE_ROQ,audio_id,video_id,dvdsub_id);
  if(roq_check_file(demuxer)){
      mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat,"ROQ");
      file_format=DEMUXER_TYPE_ROQ;
  } else {
      free_demuxer(demuxer);
      demuxer = NULL;
  }
}
#ifdef HAVE_GIF
//=============== Try to open as GIF file: =================
if(file_format==DEMUXER_TYPE_UNKNOWN || file_format==DEMUXER_TYPE_GIF){
  demuxer=new_demuxer(stream,DEMUXER_TYPE_GIF,audio_id,video_id,dvdsub_id);
  if(gif_check_file(demuxer)){
      mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat,"GIF");
      file_format=DEMUXER_TYPE_GIF;
  } else {
      free_demuxer(demuxer);
      demuxer = NULL;
  }
}
#endif
//=============== Try to open as BMP file: =================
if(file_format==DEMUXER_TYPE_UNKNOWN || file_format==DEMUXER_TYPE_BMP){
  demuxer=new_demuxer(stream,DEMUXER_TYPE_BMP,audio_id,video_id,dvdsub_id);
  if(bmp_check_file(demuxer)){
      mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat,"BMP");
      file_format=DEMUXER_TYPE_BMP;
  } else {
      free_demuxer(demuxer);
      demuxer = NULL;
  }
}
#ifdef HAVE_OGGVORBIS
//=============== Try to open as Ogg file: =================
if(file_format==DEMUXER_TYPE_UNKNOWN || file_format==DEMUXER_TYPE_OGG){
  demuxer=new_demuxer(stream,DEMUXER_TYPE_OGG,audio_id,video_id,dvdsub_id);
  if(demux_ogg_open(demuxer)){
      mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat,"OGG");
      file_format=DEMUXER_TYPE_OGG;
  } else {
      free_demuxer(demuxer);
      demuxer = NULL;
  }
}
#endif
//=============== Try to open as PVA file: =================
if(file_format == DEMUXER_TYPE_UNKNOWN || file_format==DEMUXER_TYPE_PVA){
	demuxer=new_demuxer(stream,DEMUXER_TYPE_PVA,audio_id,video_id,dvdsub_id);
	if(pva_check_file(demuxer)) {
		mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat,"PVA");
		file_format=DEMUXER_TYPE_PVA;
	} else {
		free_demuxer(demuxer);
		demuxer=NULL;
	}
}
//=============== Try to open as MPEG-TS file: =================
if(file_format == DEMUXER_TYPE_UNKNOWN || file_format==DEMUXER_TYPE_MPEG_TS){
	demuxer=new_demuxer(stream,DEMUXER_TYPE_MPEG_TS,audio_id,video_id,dvdsub_id);
	if(ts_check_file(demuxer)) {
		mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat,"TS");
		file_format=DEMUXER_TYPE_MPEG_TS;
	} else {
		free_demuxer(demuxer);
		demuxer=NULL;
	}
}
//=============== Try to open as LMLM4 file: =================
if(file_format==DEMUXER_TYPE_UNKNOWN || file_format==DEMUXER_TYPE_LMLM4){
  demuxer=new_demuxer(stream,DEMUXER_TYPE_LMLM4,audio_id,video_id,dvdsub_id);
  if(lmlm4_check_file(demuxer)){
      mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat,"RAW LMLM4");
      file_format=DEMUXER_TYPE_LMLM4;
  } else {
      free_demuxer(demuxer);
      demuxer = NULL;
  }
}
//=============== Try to open as MPEG-PS file: =================
if(file_format==DEMUXER_TYPE_UNKNOWN || file_format==DEMUXER_TYPE_MPEG_PS){
 int pes=1;
 int tmp;
 off_t tmppos;
 file_format=DEMUXER_TYPE_UNKNOWN;
 while(pes>=0){
  demuxer=new_demuxer(stream,DEMUXER_TYPE_MPEG_PS,audio_id,video_id,dvdsub_id);
  
  // try to pre-detect PES:
  tmppos=stream_tell(demuxer->stream);
  tmp=stream_read_dword(demuxer->stream);
  if(tmp==0x1E0 || tmp==0x1C0){
      tmp=stream_read_word(demuxer->stream);
      if(tmp>1 && tmp<=2048) pes=0; // demuxer->synced=3; // PES...
  }
  stream_seek(demuxer->stream,tmppos);
  
  if(!pes) demuxer->synced=3; // hack!

  num_elementary_packets100=0;
  num_elementary_packets101=0;
  num_elementary_packets1B6=0;
  num_elementary_packets12x=0;
  num_elementary_packetsPES=0;
  num_h264_slice=0; //combined slice
  num_h264_dpa=0; //DPA Slice
  num_h264_dpb=0; //DPB Slice
  num_h264_dpc=0; //DPC Slice
  num_h264_idr=0; //IDR Slice
  num_h264_sps=0;
  num_h264_pps=0;
  num_mp3audio_packets=0;

  if(ds_fill_buffer(demuxer->video)){
    if(!pes)
      mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat,"MPEG-PES");
    else
      mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat,"MPEG-PS");
    file_format=DEMUXER_TYPE_MPEG_PS;
  } else {
    mp_msg(MSGT_DEMUX,MSGL_V,"MPEG packet stats: p100: %d  p101: %d p1B6: %d p12x: %d sli: %d a: %d b: %d c: %d idr: %d sps: %d pps: %d PES: %d  MP3: %d \n",
	num_elementary_packets100,num_elementary_packets101,
	num_elementary_packets1B6,num_elementary_packets12x,
	num_h264_slice, num_h264_dpa,
	num_h264_dpb, num_h264_dpc=0,
	num_h264_idr,  num_h264_sps=0,
	num_h264_pps,
	num_elementary_packetsPES,num_mp3audio_packets);
//MPEG packet stats: p100: 458  p101: 458  PES: 0  MP3: 1103  (.m2v)
    if(num_mp3audio_packets>50 && num_mp3audio_packets>2*num_elementary_packets100
	&& abs(num_elementary_packets100-num_elementary_packets101)>2)
	break; // it's .MP3
    // some hack to get meaningfull error messages to our unhappy users:
    if(num_elementary_packets100>=2 && num_elementary_packets101>=2 &&
       abs(num_elementary_packets101+8-num_elementary_packets100)<16){
      if(num_elementary_packetsPES>=4 && num_elementary_packetsPES>=num_elementary_packets100-4){
        --pes;continue; // tricky...
      }
      file_format=DEMUXER_TYPE_MPEG_ES; //  <-- hack is here :)
    } else 
#if 1
    // fuzzy mpeg4-es detection. do NOT enable without heavy testing of mpeg formats detection!
    if(num_elementary_packets1B6>3 && num_elementary_packets12x>=1 &&
       num_elementary_packetsPES==0 && num_elementary_packets100<=num_elementary_packets12x &&
       demuxer->synced<2){
      file_format=DEMUXER_TYPE_MPEG4_ES;
    } else
#endif
#if 1
    // fuzzy h264-es detection. do NOT enable without heavy testing of mpeg formats detection!
    if((num_h264_slice>3 || (num_h264_dpa>3 && num_h264_dpb>3 && num_h264_dpc>3)) && 
       /* FIXME num_h264_sps>=1 && */ num_h264_pps>=1 && num_h264_idr>=1 &&
       num_elementary_packets1B6==0 && num_elementary_packetsPES==0 &&
       demuxer->synced<2){
      file_format=DEMUXER_TYPE_H264_ES;
    } else
#endif
    {
      if(demuxer->synced==2)
        mp_msg(MSGT_DEMUXER,MSGL_ERR,"MPEG: " MSGTR_MissingVideoStreamBug);
      else
        mp_msg(MSGT_DEMUXER,MSGL_V,MSGTR_NotSystemStream);
      free_demuxer(demuxer);
      demuxer = NULL;
    }
  }
  break;
 }
}
//=============== Try to open as MPEG-ES file: =================
if(file_format==DEMUXER_TYPE_MPEG_ES || file_format==DEMUXER_TYPE_MPEG4_ES  || file_format==DEMUXER_TYPE_H264_ES){ // little hack, see above!
  demuxer=new_demuxer(stream,file_format,audio_id,video_id,dvdsub_id);
  if(!ds_fill_buffer(demuxer->video)){
    mp_msg(MSGT_DEMUXER,MSGL_ERR,MSGTR_InvalidMPEGES);
    file_format=DEMUXER_TYPE_UNKNOWN;
    free_demuxer(demuxer);
    demuxer = NULL;
  } else {
    switch(file_format){
    case DEMUXER_TYPE_MPEG_ES: 
        mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat, "MPEG-ES"); break;
    case DEMUXER_TYPE_MPEG4_ES: 
        mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat, "MPEG4-ES"); break;
    case DEMUXER_TYPE_H264_ES: 
        mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat, "H264-ES"); break;
    }
  }
}
#ifdef HAVE_LIBDV095
//=============== Try to open raw DV file, as produced by dvgrab --format raw =================
if(file_format==DEMUXER_TYPE_UNKNOWN || file_format==DEMUXER_TYPE_RAWDV)
{
   demuxer=new_demuxer(stream,DEMUXER_TYPE_RAWDV,audio_id,video_id,dvdsub_id);
   if(rawdv_check_file(demuxer))
   {
      mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat,"RAWDV");
      file_format=DEMUXER_TYPE_RAWDV;
   }
   else {
      free_demuxer(demuxer);
      demuxer=NULL;
   }
}
#endif
//=============== Try to open as audio file: =================
if(file_format==DEMUXER_TYPE_UNKNOWN || file_format==DEMUXER_TYPE_AUDIO){
  demuxer=new_demuxer(stream,DEMUXER_TYPE_AUDIO,audio_id,video_id,dvdsub_id);
  if(demux_audio_open(demuxer)){
    mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_DetectedAudiofile);
    file_format=DEMUXER_TYPE_AUDIO;
  } else {
    free_demuxer(demuxer);
    demuxer = NULL;
  }
}
#ifdef HAVE_XMMS
//=============== Try to open as XMMS file: =================
if(file_format==DEMUXER_TYPE_UNKNOWN || file_format==DEMUXER_TYPE_XMMS){
  demuxer=new_demuxer(stream,DEMUXER_TYPE_XMMS,audio_id,video_id,dvdsub_id);
  if(demux_xmms_open(demuxer)){
    mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_DetectedAudiofile);
    file_format=DEMUXER_TYPE_XMMS;
  } else {
    free_demuxer(demuxer);
    demuxer = NULL;
  }
}
#endif
//=============== Try to open as MPEG-TY file: =================
if(file_format==DEMUXER_TYPE_UNKNOWN || file_format==DEMUXER_TYPE_MPEG_TY)
{
  demuxer=new_demuxer(stream,DEMUXER_TYPE_MPEG_TY,audio_id,video_id,dvdsub_id);
  if(ds_fill_buffer(demuxer->video)){
      mp_msg(MSGT_DEMUXER,MSGL_INFO,MSGTR_Detected_XXX_FileFormat,"TiVo");
      file_format=DEMUXER_TYPE_MPEG_TY;
  } else {
      free_demuxer(demuxer);
      demuxer = NULL;
  }
}
//=============== Try to open as a RTP stream: ===========
 if(file_format==DEMUXER_TYPE_RTP) {
   demuxer=new_demuxer(stream,DEMUXER_TYPE_RTP,audio_id,video_id,dvdsub_id);
 }
//=============== Unknown, exiting... ===========================
if(file_format==DEMUXER_TYPE_UNKNOWN || demuxer == NULL){
  //mp_msg(MSGT_DEMUXER,MSGL_ERR,MSGTR_FormatNotRecognized); // will be done by mplayer.c after fallback to playlist-parsing
  return NULL;
}
//====== File format recognized, set up these for compatibility: =========
d_audio=demuxer->audio;
d_video=demuxer->video;
//d_dvdsub=demuxer->sub;

demuxer->file_format=file_format;

switch(file_format){
#ifdef HAVE_LIBDV095
 case DEMUXER_TYPE_RAWDV:
 {
   if (!demux_open_rawdv(demuxer)) return NULL;
   break;
 }
#endif
 case DEMUXER_TYPE_RAWAUDIO: {
   demux_rawaudio_open(demuxer);
   break;
 }
 case DEMUXER_TYPE_RAWVIDEO: {
   demux_rawvideo_open(demuxer);
   break;
 }
 case DEMUXER_TYPE_MF: {
  if (!demux_open_mf(demuxer)) return NULL;
  break;
 }
 case DEMUXER_TYPE_FLI: {
  if (!demux_open_fli(demuxer)) return NULL;
  break;
 }
 case DEMUXER_TYPE_FILM: {
  if (!demux_open_film(demuxer)) return NULL;
  break;
 }
#ifdef HAVE_GIF
 case DEMUXER_TYPE_GIF: {
  if (!demux_open_gif(demuxer)) return NULL;
  break;
 }
#endif
 case DEMUXER_TYPE_BMP: {
  if (!demux_open_bmp(demuxer)) return NULL;
  break;
 }
 case DEMUXER_TYPE_ROQ: {
  if (!demux_open_roq(demuxer)) return NULL;
  break;
 }
 case DEMUXER_TYPE_SMJPEG: {
  if (!demux_open_smjpeg(demuxer)) return NULL;
  break;
 }
 case DEMUXER_TYPE_MOV: {
  if(!mov_read_header(demuxer)) return NULL;
//  sh_video=d_video->sh;if(sh_video) sh_video->ds=d_video;
//  sh_audio=d_audio->sh;if(sh_audio) sh_audio->ds=d_audio;
  break;
 }
 case DEMUXER_TYPE_AVI: {
   sh_audio_t* sh_a;
   demuxer = (demuxer_t*) demux_open_avi(demuxer);
   if(!demuxer) return NULL; // failed to open
   sh_a = (sh_audio_t*)demuxer->audio->sh;
   if(demuxer->audio->id != -2 && sh_a) {
#ifdef HAVE_OGGVORBIS
    // support for Ogg-in-AVI:
     if(sh_a->format == 0xFFFE)
       demuxer = init_avi_with_ogg(demuxer);
     else if(sh_a->format == 0x674F) {
       stream_t* s;
       demuxer_t  *od;
       s = new_ds_stream(demuxer->audio);
       od = new_demuxer(s,DEMUXER_TYPE_OGG,-1,-2,-2);
       if(!demux_ogg_open(od)) {
	 mp_msg( MSGT_DEMUXER,MSGL_ERR,MSGTR_ErrorOpeningOGGDemuxer);
	 free_stream(s);
	 demuxer->audio->id = -2;
       } else
	 demuxer = new_demuxers_demuxer(demuxer,od,demuxer);
     }
#endif
   }       
   return demuxer;
//  break;
 }
 case DEMUXER_TYPE_NUV: {
  demux_open_nuv(demuxer);
  break;
 }
 case DEMUXER_TYPE_VIVO: {
  demux_open_vivo(demuxer);
  break;
 }
 case DEMUXER_TYPE_PVA: {
  demux_open_pva(demuxer);
  break;
 }
 case DEMUXER_TYPE_Y4M: {
  demux_open_y4m(demuxer);
  break;
 }
 case DEMUXER_TYPE_LMLM4: {
  demux_open_lmlm4(demuxer);
  if(!ds_fill_buffer(d_video)){
    mp_msg(MSGT_DEMUXER,MSGL_INFO,"LMLM4: " MSGTR_MissingVideoStream);
    sh_video=NULL;
  } else {
    sh_video=d_video->sh;sh_video->ds=d_video;
  }
  if(audio_id!=-2) {
   if(!ds_fill_buffer(d_audio)){
    mp_msg(MSGT_DEMUXER,MSGL_INFO,"LMLM4: " MSGTR_MissingAudioStream);
    sh_audio=NULL;
   } else {
    sh_audio=d_audio->sh;sh_audio->ds=d_audio;
   }
  }
  break;
 }
 case DEMUXER_TYPE_REAL: {
  demux_open_real(demuxer);
  break;
 }
 case DEMUXER_TYPE_ASF: {
  //---- ASF header:
  read_asf_header(demuxer);
  stream_reset(demuxer->stream);
  stream_seek(demuxer->stream,demuxer->movi_start);
//  demuxer->idx_pos=0;
//  demuxer->endpos=avi_header.movi_end;
  if(d_video->id != -2) {
    if(!ds_fill_buffer(d_video)){
      mp_msg(MSGT_DEMUXER,MSGL_WARN,"ASF: " MSGTR_MissingVideoStream);
      sh_video=NULL;
      //printf("ASF: missing video stream!? contact the author, it may be a bug :(\n");
    } else {
      sh_video=d_video->sh;sh_video->ds=d_video;
      sh_video->fps=1000.0f; sh_video->frametime=0.001f; // 1ms
      mp_msg(MSGT_DEMUXER,MSGL_INFO,"VIDEO:  [%.4s]  %dx%d  %dbpp\n",
        (char *)&sh_video->bih->biCompression,
        sh_video->bih->biWidth,
        sh_video->bih->biHeight,
        sh_video->bih->biBitCount);
      //      sh_video->i_bps=10*asf_packetsize; // FIXME!
    }
  }
  if(d_audio->id!=-2){
    mp_msg(MSGT_DEMUXER,MSGL_V,MSGTR_ASFSearchingForAudioStream,d_audio->id);
    if(!ds_fill_buffer(d_audio)){
      mp_msg(MSGT_DEMUXER,MSGL_INFO,"ASF: " MSGTR_MissingAudioStream);
      sh_audio=NULL;
    } else {
      sh_audio=d_audio->sh;sh_audio->ds=d_audio;
      sh_audio->format=sh_audio->wf->wFormatTag;
    }
  }
  break;
 }
 case DEMUXER_TYPE_H264_ES:
 case DEMUXER_TYPE_MPEG4_ES:
 case DEMUXER_TYPE_MPEG_ES: {
   sh_audio=NULL;   // ES streams has no audio channel
   d_video->sh=new_sh_video(demuxer,0); // create dummy video stream header, id=0
   sh_video=d_video->sh;sh_video->ds=d_video;
   break;
 }

 case DEMUXER_TYPE_MPEG_TY: {
  sh_video=d_video->sh;sh_video->ds=d_video;

  if(audio_id!=-2) {
   if(!ds_fill_buffer(d_audio)){
    mp_msg(MSGT_DEMUXER,MSGL_INFO,"MPEG: " MSGTR_MissingAudioStream);
    sh_audio=NULL;
   } else {
    sh_audio=d_audio->sh;sh_audio->ds=d_audio;
    switch(d_audio->id & 0xE0){  // 1110 0000 b  (high 3 bit: type  low 5: id)
      case 0x00: sh_audio->format=0x50;break; // mpeg
      case 0xA0: sh_audio->format=0x10001;break;  // dvd pcm
      case 0x80: sh_audio->format=0x2000;break; // ac3
      default: sh_audio=NULL; // unknown type
    }
   }
  }
  break;
 }
 case DEMUXER_TYPE_MPEG_PS: {
  sh_video=d_video->sh;sh_video->ds=d_video;
//  if(demuxer->stream->type!=STREAMTYPE_VCD) demuxer->movi_start=0; // for VCD

  if(audio_id!=-2) {
   if(!ds_fill_buffer(d_audio)){
    mp_msg(MSGT_DEMUXER,MSGL_INFO,"MPEG: " MSGTR_MissingAudioStream);
    sh_audio=NULL;
   } else {
    sh_audio=d_audio->sh;sh_audio->ds=d_audio;
    switch(d_audio->id & 0xE0){  // 1110 0000 b  (high 3 bit: type  low 5: id)
      case 0x00: sh_audio->format=0x50;break; // mpeg
      case 0xA0: sh_audio->format=0x10001;break;  // dvd pcm
      case 0x80: sh_audio->format=0x2000;break; // ac3
      default: sh_audio=NULL; // unknown type
    }
   }
  }
  break;
 }
#ifdef USE_TV
 case DEMUXER_TYPE_TV: {
    if (!demux_open_tv(demuxer)) return(NULL);
    break;
 }
#endif
#ifdef STREAMING_LIVE_DOT_COM
 case DEMUXER_TYPE_RTP: {
   demuxer = demux_open_rtp(demuxer);
   break;
 }
#endif
 case DEMUXER_TYPE_MPEG_TS: 
 case DEMUXER_TYPE_MPEG4_IN_TS: {
  demux_open_ts(demuxer);
  break;
 }
 case DEMUXER_TYPE_REALAUDIO: {
  if (!demux_open_ra(demuxer)) return NULL;
  break;
 }
} // switch(file_format)
pts_from_bps=0; // !!!
return demuxer;
}

char* audio_stream = NULL;
char* sub_stream = NULL;
int demuxer_type = 0, audio_demuxer_type = 0, sub_demuxer_type = 0;
int audio_stream_cache = 0;

extern int hr_mp3_seek;

demuxer_t* demux_open(stream_t *vs,int file_format,int audio_id,int video_id,int dvdsub_id,char* filename){
  stream_t *as = NULL,*ss = NULL;
  demuxer_t *vd,*ad = NULL,*sd = NULL;
  int afmt = 0,sfmt = 0;

  if(audio_stream) {
    as = open_stream(audio_stream,0,&afmt);
    if(!as) {
      mp_msg(MSGT_DEMUXER,MSGL_ERR,MSGTR_CannotOpenAudioStream,audio_stream);
      return NULL;
    }
    if(audio_stream_cache) {
      if(!stream_enable_cache(as,audio_stream_cache*1024,audio_stream_cache*1024/5,
			      audio_stream_cache*1024/20)) {
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

  vd = demux_open_stream(vs,demuxer_type ? demuxer_type : file_format,audio_stream ? -2 : audio_id,video_id, sub_stream ? -2 : dvdsub_id, filename);
  if(!vd) {
    if(as) free_stream(as);
    if(ss) free_stream(ss);
    return NULL;
  }
  if(as) {
    ad = demux_open_stream(as,audio_demuxer_type ? audio_demuxer_type : afmt,audio_id,-2,-2, audio_stream);
    if(!ad) {
      mp_msg(MSGT_DEMUXER,MSGL_WARN,MSGTR_OpeningAudioDemuxerFailed,audio_stream);
      free_stream(as);
    }
    else if(ad->audio->sh && ((sh_audio_t*)ad->audio->sh)->format == 0x55) // MP3
      hr_mp3_seek=1; // Enable high res seeking
  }
  if(ss) {
    sd = demux_open_stream(ss,sub_demuxer_type ? sub_demuxer_type : sfmt,-2,-2,dvdsub_id, sub_stream);
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

int demux_seek_avi(demuxer_t *demuxer,float rel_seek_secs,int flags);
int demux_seek_asf(demuxer_t *demuxer,float rel_seek_secs,int flags);
int demux_seek_mpg(demuxer_t *demuxer,float rel_seek_secs,int flags);
int demux_seek_ty(demuxer_t *demuxer,float rel_seek_secs,int flags);
int demux_seek_y4m(demuxer_t *demuxer,float rel_seek_secs,int flags);
int demux_seek_fli(demuxer_t *demuxer,float rel_seek_secs,int flags);
int demux_seek_film(demuxer_t *demuxer,float rel_seek_secs,int flags);
int demux_seek_mf(demuxer_t *demuxer,float rel_seek_secs,int flags);
int demux_seek_nuv(demuxer_t *demuxer,float rel_seek_secs,int flags);
void demux_seek_mov(demuxer_t *demuxer,float pts,int flags);
int demux_seek_real(demuxer_t *demuxer,float rel_seek_secs,int flags);
int demux_seek_pva(demuxer_t *demuxer,float rel_seek_secs,int flags);
int demux_seek_ts(demuxer_t *demuxer,float rel_seek_secs,int flags);

#ifdef HAVE_LIBDV095
int demux_seek_rawdv(demuxer_t *demuxer, float pts, int flags);
#endif

extern void demux_audio_seek(demuxer_t *demuxer,float rel_seek_secs,int flags);
extern void demux_demuxers_seek(demuxer_t *demuxer,float rel_seek_secs,int flags);
extern void demux_ogg_seek(demuxer_t *demuxer,float rel_seek_secs,int flags);
extern void demux_rawaudio_seek(demuxer_t *demuxer,float rel_seek_secs,int flags);
extern void demux_rawvideo_seek(demuxer_t *demuxer,float rel_seek_secs,int flags);
extern void demux_xmms_seek(demuxer_t *demuxer,float rel_seek_secs,int flags);
extern void demux_mkv_seek(demuxer_t *demuxer,float rel_seek_secs,int flags);

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

switch(demuxer->file_format){

#ifdef HAVE_LIBDV095
  case DEMUXER_TYPE_RAWDV:
      demux_seek_rawdv(demuxer,rel_seek_secs,flags);  break;
#endif
  case DEMUXER_TYPE_AVI:
      demux_seek_avi(demuxer,rel_seek_secs,flags);  break;

  case DEMUXER_TYPE_ASF:
      demux_seek_asf(demuxer,rel_seek_secs,flags);  break;

  case DEMUXER_TYPE_MPEG_TY:
      demux_seek_ty(demuxer,rel_seek_secs,flags);  break;
  
  case DEMUXER_TYPE_H264_ES:
  case DEMUXER_TYPE_MPEG4_ES:
  case DEMUXER_TYPE_MPEG_ES:
  case DEMUXER_TYPE_MPEG_PS:
      demux_seek_mpg(demuxer,rel_seek_secs,flags);  break;

  case DEMUXER_TYPE_MOV:
      demux_seek_mov(demuxer,rel_seek_secs,flags);  break;

  case DEMUXER_TYPE_REAL:
      demux_seek_real(demuxer,rel_seek_secs,flags);  break;

  case DEMUXER_TYPE_Y4M:
      demux_seek_y4m(demuxer,rel_seek_secs,flags);  break;

  case DEMUXER_TYPE_MF:
      demux_seek_mf(demuxer,rel_seek_secs,flags);  break;

  case DEMUXER_TYPE_PVA:
      demux_seek_pva(demuxer,rel_seek_secs,flags); break;
      
  case DEMUXER_TYPE_FLI:
      demux_seek_fli(demuxer,rel_seek_secs,flags);  break;
  case DEMUXER_TYPE_FILM:
      demux_seek_film(demuxer,rel_seek_secs,flags);  break;
  case DEMUXER_TYPE_NUV:
      demux_seek_nuv(demuxer,rel_seek_secs,flags);  break;
  case DEMUXER_TYPE_AUDIO:
      demux_audio_seek(demuxer,rel_seek_secs,flags);  break;
 case DEMUXER_TYPE_DEMUXERS:
      demux_demuxers_seek(demuxer,rel_seek_secs,flags);  break;
#ifdef HAVE_OGGVORBIS
 case DEMUXER_TYPE_OGG:
      demux_ogg_seek(demuxer,rel_seek_secs,flags);  break;
#endif
 case DEMUXER_TYPE_RAWAUDIO:
      demux_rawaudio_seek(demuxer,rel_seek_secs,flags);  break;
 case DEMUXER_TYPE_RAWVIDEO:
      demux_rawvideo_seek(demuxer,rel_seek_secs,flags);  break;
#ifdef HAVE_XMMS
 case DEMUXER_TYPE_XMMS:
      demux_xmms_seek(demuxer,rel_seek_secs,flags); break;
#endif
#ifdef HAVE_MATROSKA
 case DEMUXER_TYPE_MATROSKA:
      demux_mkv_seek(demuxer,rel_seek_secs,flags);  break;
#endif
 case DEMUXER_TYPE_MPEG_TS:
 case DEMUXER_TYPE_MPEG4_IN_TS:
      demux_seek_ts(demuxer,rel_seek_secs,flags); break;

} // switch(demuxer->file_format)

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
      mp_msg(MSGT_DEMUX, MSGL_INFO, " %s: %s\n",info[2*n],info[2*n+1]);

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

extern int demux_ty_control(demuxer_t *demuxer, int cmd, void *arg);
extern int demux_mpg_control(demuxer_t *demuxer, int cmd, void *arg);
extern int demux_asf_control(demuxer_t *demuxer, int cmd, void *arg);
extern int demux_avi_control(demuxer_t *demuxer, int cmd, void *arg);
extern int demux_xmms_control(demuxer_t *demuxer, int cmd, void *arg);
extern int demux_mkv_control(demuxer_t *demuxer, int cmd, void *arg);
extern int demux_audio_control(demuxer_t *demuxer, int cmd, void *arg);
extern int demux_ogg_control(demuxer_t *demuxer, int cmd, void *arg);
extern int demux_real_control(demuxer_t *demuxer, int cmd, void *arg);

int demux_control(demuxer_t *demuxer, int cmd, void *arg) {
    switch(demuxer->type) {
	case DEMUXER_TYPE_MPEG_TY:
	    return demux_ty_control(demuxer,cmd,arg);
	case DEMUXER_TYPE_MPEG4_ES:
	case DEMUXER_TYPE_MPEG_ES:
	case DEMUXER_TYPE_MPEG_PS:
	case DEMUXER_TYPE_MPEG_TS:
	case DEMUXER_TYPE_MPEG4_IN_TS:
	    return demux_mpg_control(demuxer,cmd,arg);
	case DEMUXER_TYPE_ASF:
	    return demux_asf_control(demuxer,cmd,arg);
	case DEMUXER_TYPE_AVI:
	case DEMUXER_TYPE_AVI_NI:
	case DEMUXER_TYPE_AVI_NINI:
	    return demux_avi_control(demuxer,cmd,arg);
	case DEMUXER_TYPE_AUDIO:
	    return demux_audio_control(demuxer,cmd,arg);
#ifdef HAVE_OGGVORBIS
	case DEMUXER_TYPE_OGG:
	    return demux_ogg_control(demuxer,cmd,arg);
#endif
#ifdef HAVE_XMMS
	case DEMUXER_TYPE_XMMS:
	    return demux_xmms_control(demuxer,cmd,arg);
#endif
#ifdef HAVE_MATROSKA
        case DEMUXER_TYPE_MATROSKA:
	    return demux_mkv_control(demuxer,cmd,arg);
#endif
	case DEMUXER_TYPE_REAL:
	    return demux_real_control(demuxer, cmd, arg);

	default:
	    return DEMUXER_CTRL_NOTIMPL;
    }
}



unsigned long demuxer_get_time_length(demuxer_t *demuxer){     
    unsigned long get_time_ans;     
    if (demux_control(demuxer, DEMUXER_CTRL_GET_TIME_LENGTH,(void *)&get_time_ans)<=0)  {
        get_time_ans=0;     
    }
    return get_time_ans;
}

int demuxer_get_percent_pos(demuxer_t *demuxer){     
    int ans;     
    if (demux_control(demuxer, DEMUXER_CTRL_GET_PERCENT_POS, &ans)<=0)  {
        ans=0;     
    }
    if (ans>100 || ans<0) ans=0;
    return ans;
}


//  MPG/VOB file parser for DEMUXER v2.5  by A'rpi/ESP-team

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "config.h"
#include "mp_msg.h"
#include "help_mp.h"

#include "stream.h"
#include "demuxer.h"
#include "parse_es.h"
#include "stheader.h"
#include "mp3_hdr.h"

//#define MAX_PS_PACKETSIZE 2048
#define MAX_PS_PACKETSIZE (224*1024)

typedef struct mpg_demuxer {
  float last_pts;
  float final_pts;
  int has_valid_timestamps;
} mpg_demuxer_t;

static int mpeg_pts_error=0;

/// Open an mpg physical stream
int demux_mpg_open(demuxer_t* demuxer) {
  stream_t *s = demuxer->stream;
  off_t pos = stream_tell(s);
  off_t end_seq_start = demuxer->movi_end-500000; // 500000 is a wild guess
  float half_pts = 0.0;
  mpg_demuxer_t* mpg_d;

  if (!ds_fill_buffer(demuxer->video)) return 0;
  mpg_d = (mpg_demuxer_t*)calloc(1,sizeof(mpg_demuxer_t));
  demuxer->priv = mpg_d;
  mpg_d->final_pts = 0.0;
  mpg_d->has_valid_timestamps = 1;
  if (demuxer->seekable && stream_tell(demuxer->stream) < end_seq_start) {
    stream_seek(s,(pos + end_seq_start / 2));
    while ((!s->eof) && ds_fill_buffer(demuxer->video) && half_pts == 0.0) {
      half_pts = mpg_d->last_pts;
    }
    stream_seek(s,end_seq_start);
    while ((!s->eof) && ds_fill_buffer(demuxer->video)) {
      if (mpg_d->final_pts < mpg_d->last_pts) mpg_d->final_pts = mpg_d->last_pts;
    }
    // educated guess about validity of timestamps
    if (mpg_d->final_pts > 3 * half_pts || mpg_d->final_pts < 1.5 * half_pts) {
      mpg_d->has_valid_timestamps = 0;
    }
    ds_free_packs(demuxer->audio);
    ds_free_packs(demuxer->video);
    demuxer->stream->eof=0; // clear eof flag
    demuxer->video->eof=0;
    demuxer->audio->eof=0;
    
    stream_seek(s,pos);
    ds_fill_buffer(demuxer->video);
  }
  return 1;
}

void demux_close_mpg(demuxer_t* demuxer) {
  mpg_demuxer_t* mpg_d = demuxer->priv;
  if (mpg_d) free(mpg_d);
}


static unsigned int read_mpeg_timestamp(stream_t *s,int c){
  int d,e;
  unsigned int pts;
  d=stream_read_word(s);
  e=stream_read_word(s);
  if( ((c&1)!=1) || ((d&1)!=1) || ((e&1)!=1) ){
    ++mpeg_pts_error;
    return 0; // invalid pts
  }
  pts=(((c>>1)&7)<<30)|((d>>1)<<15)|(e>>1);
  mp_dbg(MSGT_DEMUX,MSGL_DBG3,"{%d}",pts);
  return pts;
}

static int demux_mpg_read_packet(demuxer_t *demux,int id){
  int d;
  int len;
  unsigned char c=0;
  unsigned int pts=0;
  unsigned int dts=0;
  demux_stream_t *ds=NULL;
  
  mp_dbg(MSGT_DEMUX,MSGL_DBG3,"demux_read_packet: %X\n",id);

//  if(id==0x1F0){
//    demux->synced=0; // force resync after 0x1F0
//    return -1;
//}

//  if(id==0x1BA) packet_start_pos=stream_tell(demux->stream);
  if(id<0x1BC || id>=0x1F0) return -1;
  if(id==0x1BE) return -1; // padding stream
  if(id==0x1BF) return -1; // private2

  len=stream_read_word(demux->stream);
  mp_dbg(MSGT_DEMUX,MSGL_DBG3,"PACKET len=%d",len);
//  if(len==62480){ demux->synced=0;return -1;} /* :) */
  if(len==0 || len>MAX_PS_PACKETSIZE){
    mp_dbg(MSGT_DEMUX,MSGL_DBG2,"Invalid PS packet len: %d\n",len);
    return -2;  // invalid packet !!!!!!
  }

  mpeg_pts_error=0;

  while(len>0){   // Skip stuFFing bytes
    c=stream_read_char(demux->stream);--len;
    if(c!=0xFF)break;
  }
  if((c>>6)==1){  // Read (skip) STD scale & size value
//    printf("  STD_scale=%d",(c>>5)&1);
    d=((c&0x1F)<<8)|stream_read_char(demux->stream);
    len-=2;
//    printf("  STD_size=%d",d);
    c=stream_read_char(demux->stream);
  }
  // Read System-1 stream timestamps:
  if((c>>4)==2){
    pts=read_mpeg_timestamp(demux->stream,c);
    len-=4;
  } else
  if((c>>4)==3){
    pts=read_mpeg_timestamp(demux->stream,c);
    c=stream_read_char(demux->stream);
    if((c>>4)!=1) pts=0; //printf("{ERROR4}");
    dts=read_mpeg_timestamp(demux->stream,c);
    len-=4+1+4;
  } else
  if((c>>6)==2){
    int pts_flags;
    int hdrlen;
    // System-2 (.VOB) stream:
    if((c>>4)&3) {
        mp_msg(MSGT_DEMUX,MSGL_WARN,MSGTR_EncryptedVOB);
    }
    c=stream_read_char(demux->stream); pts_flags=c>>6;
    c=stream_read_char(demux->stream); hdrlen=c;
    len-=2;
    mp_dbg(MSGT_DEMUX,MSGL_DBG3,"  hdrlen=%d  (len=%d)",hdrlen,len);
    if(hdrlen>len){ mp_msg(MSGT_DEMUX,MSGL_V,"demux_mpg: invalid header length  \n"); return -1;}
    if(pts_flags==2 && hdrlen>=5){
      c=stream_read_char(demux->stream);
      pts=read_mpeg_timestamp(demux->stream,c);
      len-=5;hdrlen-=5;
    } else
    if(pts_flags==3 && hdrlen>=10){
      c=stream_read_char(demux->stream);
      pts=read_mpeg_timestamp(demux->stream,c);
      c=stream_read_char(demux->stream);
      dts=read_mpeg_timestamp(demux->stream,c);
      len-=10;hdrlen-=10;
    }
    len-=hdrlen;
    if(hdrlen>0) stream_skip(demux->stream,hdrlen); // skip header bytes
    
    //============== DVD Audio sub-stream ======================
    if(id==0x1BD){
      int aid=stream_read_char(demux->stream);--len;
      if(len<3) return -1; // invalid audio packet
      
      // AID:
      // 0x20..0x3F  subtitle
      // 0x80..0x9F  AC3 audio
      // 0xA0..0xBF  PCM audio
      
      if((aid & 0xE0) == 0x20){
        // subtitle:
        aid&=0x1F;

        if(!demux->s_streams[aid]){
            mp_msg(MSGT_DEMUX,MSGL_V,"==> Found subtitle: %d\n",aid);
            demux->s_streams[aid]=1;
        }

        if(demux->sub->id==aid){
            ds=demux->sub;
        }
          
      } else if((aid & 0xC0) == 0x80 || (aid & 0xE0) == 0x00) {

//        aid=128+(aid&0x7F);
        // aid=0x80..0xBF

        if(!demux->a_streams[aid]) new_sh_audio(demux,aid);
        if(demux->audio->id==-1) demux->audio->id=aid;

      if(demux->audio->id==aid){
        int type;
        ds=demux->audio;
        if(!ds->sh) ds->sh=demux->a_streams[aid];
        // READ Packet: Skip additional audio header data:
        c=stream_read_char(demux->stream);//num of frames
        type=stream_read_char(demux->stream);//startpos hi
        type=(type<<8)|stream_read_char(demux->stream);//startpos lo
//        printf("\r[%02X][%04X]",c,type);
        len-=3;
        if((aid&0xE0)==0xA0 && len>=3){
	  unsigned char* hdr;
	  // save audio header as codecdata!
	  if(!((sh_audio_t*)(ds->sh))->codecdata_len){
	      ((sh_audio_t*)(ds->sh))->codecdata=malloc(3);
	      ((sh_audio_t*)(ds->sh))->codecdata_len=3;
	  }
	  hdr=((sh_audio_t*)(ds->sh))->codecdata;
          // read LPCM header:
	  // emphasis[1], mute[1], rvd[1], frame number[5]:
          hdr[0]=stream_read_char(demux->stream);
//          printf(" [%01X:%02d]",c>>5,c&31);
	  // quantization[2],freq[2],rvd[1],channels[3]
          hdr[1]=stream_read_char(demux->stream);
//          printf("[%01X:%01X] ",c>>4,c&15);
	  // dynamic range control (0x80=off):
          hdr[2]=stream_read_char(demux->stream);
//          printf("[%02X] ",c);
          len-=3;
          if(len<=0) mp_msg(MSGT_DEMUX,MSGL_V,"End of packet while searching for PCM header\n");
        }
//        printf("  \n");
      } //  if(demux->audio->id==aid)

      } else mp_msg(MSGT_DEMUX,MSGL_V,"Unknown 0x1BD substream: 0x%02X  \n",aid);

    } //if(id==0x1BD)

  } else {
    if(c!=0x0f){
      mp_msg(MSGT_DEMUX,MSGL_V,"  {ERROR5,c=%d}  \n",c);
      return -1;  // invalid packet !!!!!!
    }
  }
  if(mpeg_pts_error) mp_msg(MSGT_DEMUX,MSGL_V,"  {PTS_err:%d}  \n",mpeg_pts_error);
  mp_dbg(MSGT_DEMUX,MSGL_DBG3," => len=%d\n",len);

//  if(len<=0 || len>MAX_PS_PACKETSIZE) return -1;  // Invalid packet size
  if(len<=0 || len>MAX_PS_PACKETSIZE){
    mp_dbg(MSGT_DEMUX,MSGL_DBG2,"Invalid PS data len: %d\n",len);
    return -1;  // invalid packet !!!!!!
  }
  
  if(id>=0x1C0 && id<=0x1DF){
    // mpeg audio
    int aid=id-0x1C0;
    if(!demux->a_streams[aid]) new_sh_audio(demux,aid);
    if(demux->audio->id==-1) demux->audio->id=aid;
    if(demux->audio->id==aid){
      ds=demux->audio;
      if(!ds->sh) ds->sh=demux->a_streams[aid];
    }
  } else
  if(id>=0x1E0 && id<=0x1EF){
    // mpeg video
    int aid=id-0x1E0;
    if(!demux->v_streams[aid]) new_sh_video(demux,aid);
    if(demux->video->id==-1) demux->video->id=aid;
    if(demux->video->id==aid){
      ds=demux->video;
      if(!ds->sh) ds->sh=demux->v_streams[aid];
    }
  }

  if(ds){
    mp_dbg(MSGT_DEMUX,MSGL_DBG2,"DEMUX_MPG: Read %d data bytes from packet %04X\n",len,id);
//    printf("packet start = 0x%X  \n",stream_tell(demux->stream)-packet_start_pos);
    ds_read_packet(ds,demux->stream,len,pts/90000.0f,demux->filepos,0);
    if (demux->priv) ((mpg_demuxer_t*)demux->priv)->last_pts = pts/90000.0f;
//    if(ds==demux->sub) parse_dvdsub(ds->last->buffer,ds->last->len);
    return 1;
  }
  mp_dbg(MSGT_DEMUX,MSGL_DBG2,"DEMUX_MPG: Skipping %d data bytes from packet %04X\n",len,id);
  if(len<=2356) stream_skip(demux->stream,len);
  return 0;
}

int num_elementary_packets100=0;
int num_elementary_packets101=0;
int num_elementary_packets12x=0;
int num_elementary_packets1B6=0;
int num_elementary_packetsPES=0;
int num_h264_slice=0; //combined slice
int num_h264_dpa=0; //DPA Slice
int num_h264_dpb=0; //DPB Slice
int num_h264_dpc=0; //DPC Slice
int num_h264_idr=0; //IDR Slice
int num_h264_sps=0;
int num_h264_pps=0;

int num_mp3audio_packets=0;

int demux_mpg_es_fill_buffer(demuxer_t *demux){
  // Elementary video stream
  if(demux->stream->eof) return 0;
  demux->filepos=stream_tell(demux->stream);
  ds_read_packet(demux->video,demux->stream,STREAM_BUFFER_SIZE,0,demux->filepos,0);
  return 1;
}

int demux_mpg_fill_buffer(demuxer_t *demux){
unsigned int head=0;
int skipped=0;
int max_packs=256; // 512kbyte
int ret=0;

// System stream
do{
  demux->filepos=stream_tell(demux->stream);
  head=stream_read_dword(demux->stream);
  if((head&0xFFFFFF00)!=0x100){
   // sync...
   demux->filepos-=skipped;
   while(1){
    int c=stream_read_char(demux->stream);
    if(c<0) break; //EOF
    head<<=8;
    if(head!=0x100){
      head|=c;
      if(mp_check_mp3_header(head)) ++num_mp3audio_packets;
      ++skipped; //++demux->filepos;
      continue;
    }
    head|=c;
    break;
   }
   demux->filepos+=skipped;
  }
  if(stream_eof(demux->stream)) break;
  // sure: head=0x000001XX
  mp_dbg(MSGT_DEMUX,MSGL_DBG4,"*** head=0x%X\n",head);
  if(demux->synced==0){
    if(head==0x1BA) demux->synced=1; //else
//    if(head==0x1BD || (head>=0x1C0 && head<=0x1EF)) demux->synced=3; // PES?
  } else
  if(demux->synced==1){
    if(head==0x1BB || head==0x1BD || (head>=0x1C0 && head<=0x1EF)){
      demux->synced=2;
      mp_msg(MSGT_DEMUX,MSGL_V,"system stream synced at 0x%X (%d)!\n",demux->filepos,demux->filepos);
      num_elementary_packets100=0; // requires for re-sync!
      num_elementary_packets101=0; // requires for re-sync!
    } else demux->synced=0;
  } // else
  if(demux->synced>=2){
      ret=demux_mpg_read_packet(demux,head);
      if(!ret)
        if(--max_packs==0){
          demux->stream->eof=1;
          mp_msg(MSGT_DEMUX,MSGL_ERR,MSGTR_DoesntContainSelectedStream);
          return 0;
        }
      if(demux->synced==3) demux->synced=(ret==1)?2:0; // PES detect
  } else {
    if(head>=0x100 && head<0x1B0){
      if(head==0x100) ++num_elementary_packets100; else
      if(head==0x101) ++num_elementary_packets101; else
      if(head>=0x120 && head<=0x12F) ++num_elementary_packets12x;
      
      if((head&~0x60) == 0x101) ++num_h264_slice; else
      if((head&~0x60) == 0x102) ++num_h264_dpa; else
      if((head&~0x60) == 0x103) ++num_h264_dpb; else
      if((head&~0x60) == 0x104) ++num_h264_dpc; else
      if((head&~0x60) == 0x105 && head != 0x105) ++num_h264_idr; else
      if((head&~0x60) == 0x107 && head != 0x107) ++num_h264_sps; else
      if((head&~0x60) == 0x108 && head != 0x108) ++num_h264_pps;
      
      mp_msg(MSGT_DEMUX,MSGL_DBG3,"Opps... elementary video packet found: %03X\n",head);
    } else
    if((head>=0x1C0 && head<0x1F0) || head==0x1BD){
      ++num_elementary_packetsPES;
      mp_msg(MSGT_DEMUX,MSGL_DBG3,"Opps... PES packet found: %03X\n",head);
    } else
      if(head==0x1B6) ++num_elementary_packets1B6;
#if 1
    if( ( (num_elementary_packets100>50 && num_elementary_packets101>50) ||
          (num_elementary_packetsPES>50) ) && skipped>4000000){
        mp_msg(MSGT_DEMUX,MSGL_V,"sync_mpeg_ps: seems to be ES/PES stream...\n");
        demux->stream->eof=1;
        break;
    }
    if(num_mp3audio_packets>100 && num_elementary_packets100<10){
        mp_msg(MSGT_DEMUX,MSGL_V,"sync_mpeg_ps: seems to be MP3 stream...\n");
        demux->stream->eof=1;
        break;
    }
#endif
  }
} while(ret!=1);
  mp_dbg(MSGT_DEMUX,MSGL_DBG2,"demux: %d bad bytes skipped\n",skipped);
  if(demux->stream->eof){
    mp_msg(MSGT_DEMUX,MSGL_V,"MPEG Stream reached EOF\n");
    return 0;
  }
  return 1;
}

extern void resync_audio_stream(sh_audio_t *sh_audio);
extern void skip_audio_frame(sh_audio_t *sh_audio);

void demux_seek_mpg(demuxer_t *demuxer,float rel_seek_secs,int flags){
    demux_stream_t *d_audio=demuxer->audio;
    demux_stream_t *d_video=demuxer->video;
    sh_audio_t *sh_audio=d_audio->sh;
    sh_video_t *sh_video=d_video->sh;
    mpg_demuxer_t *mpg_d=(mpg_demuxer_t*)demuxer->priv;
    int precision = 1;
    float oldpts = 0;
    off_t oldpos = demuxer->filepos;
    float newpts = 0; 
    off_t newpos = (flags & 1) ? demuxer->movi_start : oldpos;

    if(mpg_d)
      oldpts = mpg_d->last_pts;
    newpts = (flags & 1) ? 0.0 : oldpts;
  //================= seek in MPEG ==========================
  //calculate the pts to seek to
    if(flags & 2) {
      if (mpg_d && mpg_d->final_pts > 0.0)
        newpts += mpg_d->final_pts * rel_seek_secs;
      else
        newpts += rel_seek_secs * (demuxer->movi_end - demuxer->movi_start) * oldpts / oldpos;
    } else
      newpts += rel_seek_secs;
    if (newpts < 0) newpts = 0;
	
    if(flags&2){
	// float seek 0..1
	newpos+=(demuxer->movi_end-demuxer->movi_start)*rel_seek_secs;
    } else {
	// time seek (secs)
        if (mpg_d && mpg_d->has_valid_timestamps) {
          if (mpg_d->final_pts > 0.0)
            newpos += rel_seek_secs * (demuxer->movi_end - demuxer->movi_start) / mpg_d->final_pts;
          else if (oldpts > 0.0)
            newpos += rel_seek_secs * (oldpos - demuxer->movi_start) / oldpts;
        } else if(!sh_video || !sh_video->i_bps) // unspecified or VBR
          newpos+=2324*75*rel_seek_secs; // 174.3 kbyte/sec
        else
          newpos+=sh_video->i_bps*rel_seek_secs;
    }

    while (1) {
        if(newpos<demuxer->movi_start){
	    if(demuxer->stream->type!=STREAMTYPE_VCD) demuxer->movi_start=0; // for VCD
	    if(newpos<demuxer->movi_start) newpos=demuxer->movi_start;
	}

#ifdef _LARGEFILE_SOURCE
        newpos&=~((long long)STREAM_BUFFER_SIZE-1);  /* sector boundary */
#else
        newpos&=~(STREAM_BUFFER_SIZE-1);  /* sector boundary */
#endif
        stream_seek(demuxer->stream,newpos);

        // re-sync video:
        videobuf_code_len=0; // reset ES stream buffer

	ds_fill_buffer(d_video);
	if(sh_audio){
	  ds_fill_buffer(d_audio);
	  resync_audio_stream(sh_audio);
	}

	while(1){
	  int i;
          if(sh_audio && !d_audio->eof && d_video->pts && d_audio->pts){
	    float a_pts=d_audio->pts;
            a_pts+=(ds_tell_pts(d_audio)-sh_audio->a_in_buffer_len)/(float)sh_audio->i_bps;
	    if(d_video->pts>a_pts){
	      skip_audio_frame(sh_audio);  // sync audio
	      continue;
	    }
          }
          i=sync_video_packet(d_video);
          if(sh_video->format == 0x10000004) {	//mpeg4
            if(i==0x1B6) break;			//vop (frame) startcode
          } else if(sh_video->format == 0x10000005){	//h264
            if((i & ~0x60) == 0x101 || (i & ~0x60) == 0x102 || (i & ~0x60) == 0x105) break;
          } else { 	//default mpeg1/2
	    if(i==0x1B3 || i==0x1B8) break; // found it!
	  }
          if(!i || !skip_video_packet(d_video)) break; // EOF?
        }
	if(!mpg_d)
          break;
        if (!precision || abs(newpts - mpg_d->last_pts) < 0.5 || (mpg_d->last_pts == oldpts)) break;
        if ((newpos - oldpos) * (mpg_d->last_pts - oldpts) < 0) { // invalid timestamps
          mpg_d->has_valid_timestamps = 0;
          break;
        }
        precision--;
        //prepare another seek because we are off by more than 0.5s
	if(mpg_d) {
        newpos += (newpts - mpg_d->last_pts) * (newpos - oldpos) / (mpg_d->last_pts - oldpts);
        ds_free_packs(d_audio);
        ds_free_packs(d_video);
        demuxer->stream->eof=0; // clear eof flag
        d_video->eof=0;
        d_audio->eof=0;
	}
    }
}

int demux_mpg_control(demuxer_t *demuxer,int cmd, void *arg){
/*    demux_stream_t *d_audio=demuxer->audio;*/
    demux_stream_t *d_video=demuxer->video;
/*    sh_audio_t *sh_audio=d_audio->sh;*/
    sh_video_t *sh_video=d_video->sh;
    mpg_demuxer_t *mpg_d=(mpg_demuxer_t*)demuxer->priv;

    switch(cmd) {
	case DEMUXER_CTRL_GET_TIME_LENGTH:
            if (mpg_d && mpg_d->has_valid_timestamps) {
              *((unsigned long *)arg)=(long)mpg_d->final_pts;
              return DEMUXER_CTRL_GUESS;
            }
    		return DEMUXER_CTRL_DONTKNOW;

	case DEMUXER_CTRL_GET_PERCENT_POS:
            if (mpg_d && mpg_d->has_valid_timestamps && mpg_d->final_pts > 0.0) {
              *((int *)arg)=(int)(100 * mpg_d->last_pts / mpg_d->final_pts);
              return DEMUXER_CTRL_OK;
            }
	    return DEMUXER_CTRL_DONTKNOW;

	default:
	    return DEMUXER_CTRL_NOTIMPL;
    }
}

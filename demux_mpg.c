//  MPG/VOB file parser for DEMUXER v2.5  by A'rpi/ESP-team

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "config.h"
#include "mp_msg.h"

#include "config.h"
#include "dvdauth.h"
#include "stream.h"
#include "demuxer.h"
#include "parse_es.h"

//#define MAX_PS_PACKETSIZE 2048
#define MAX_PS_PACKETSIZE (224*1024)

static int mpeg_pts_error=0;

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

//static unsigned int packet_start_pos=0;

//extern void *new_sh_audio(demuxer_t *demux,int id);
//extern void *new_sh_video(demuxer_t *demux,int id);
#include "wine/mmreg.h"
#include "wine/avifmt.h"
#include "wine/vfw.h"

#include "codec-cfg.h"
#include "stheader.h"

static int demux_mpg_read_packet(demuxer_t *demux,int id){
  int d;
  int len;
#ifdef HAVE_LIBCSS
  int css=0;
#endif
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
#ifdef HAVE_LIBCSS
        css=1;
#else
        mp_msg(MSGT_DEMUX,MSGL_WARN,"Encrypted VOB file (not compiled with libcss support)! Read file DOCS/DVD\n");
#endif
    }
    c=stream_read_char(demux->stream); pts_flags=c>>6;
    c=stream_read_char(demux->stream); hdrlen=c;
    len-=2;
    mp_dbg(MSGT_DEMUX,MSGL_DBG3,"  hdrlen=%d  (len=%d)",hdrlen,len);
    if(hdrlen>len){ mp_msg(MSGT_DEMUX,MSGL_V,"demux_mpg: invalid header length  \n"); return -1;}
    if(pts_flags==2){
      c=stream_read_char(demux->stream);
      pts=read_mpeg_timestamp(demux->stream,c);
      len-=5;hdrlen-=5;
    } else
    if(pts_flags==3){
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
          
      } else if((aid & 0xC0) == 0x80) {

//        aid=128+(aid&0x7F);
        // aid=0x80..0xBF

        if(!demux->a_streams[aid]) new_sh_audio(demux,aid);
        if(demux->audio->id==-1) demux->audio->id=aid;

      if(demux->audio->id==aid){
//        int type;
        ds=demux->audio;
        if(!ds->sh) ds->sh=demux->a_streams[aid];
        // READ Packet: Skip additional audio header data:
        c=stream_read_char(demux->stream);//type=c;
        c=stream_read_char(demux->stream);//type|=c<<8;
        c=stream_read_char(demux->stream);//type|=c<<16;
//        printf("[%06X]",type);
        len-=3;
        if((aid&0xE0)==0xA0 && len>=2){
          // read PCM header
          int head;
          head=stream_read_char(demux->stream); head=c<<8;
          c=stream_read_char(demux->stream);    head|=c;  len-=2;
          while(len>0 && head!=0x180){
            head=c<<8;
            c=stream_read_char(demux->stream);
            head|=c;--len;
          }
          if(!len) mp_msg(MSGT_DEMUX,MSGL_V,"End of packet while searching for PCM header\n");
        }
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
#ifdef HAVE_LIBCSS
    if (css) {
	    if (descrambling) CSSDescramble(demux->stream->buffer,key_title); else
		    mp_msg(MSGT_DEMUX,MSGL_WARN,"Encrypted stream but authentication was not requested by you!!\n");
    }
#endif
    ds_read_packet(ds,demux->stream,len,pts/90000.0f,0,0);
//    if(ds==demux->sub) parse_dvdsub(ds->last->buffer,ds->last->len);
    return 1;
  }
  mp_dbg(MSGT_DEMUX,MSGL_DBG2,"DEMUX_MPG: Skipping %d data bytes from packet %04X\n",len,id);
  if(len<=2356) stream_skip(demux->stream,len);
  return 0;
}

int num_elementary_packets100=0;
int num_elementary_packets101=0;
int num_elementary_packets1B6=0;
int num_elementary_packetsPES=0;

int demux_mpg_es_fill_buffer(demuxer_t *demux){
  // Elementary video stream
  if(demux->stream->eof) return 0;
  demux->filepos=stream_tell(demux->stream);
  ds_read_packet(demux->video,demux->stream,STREAM_BUFFER_SIZE,0,0,0);
  return 1;
}

int demux_mpg_fill_buffer(demuxer_t *demux){
unsigned int head=0;
int skipped=0;
int max_packs=128;
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
    if(head==0x1BA) demux->synced=1;
#if 0
    else if(head>=0x1C0 && head<=0x1EF){
      demux->synced=2;
      mp_msg(MSGT_DEMUX,MSGL_V,"Mpeg PES stream synced at 0x%X (%d)!\n",demux->filepos,demux->filepos);
      num_elementary_packets100=0; // requires for re-sync!
      num_elementary_packets101=0; // requires for re-sync!
    }
#endif
  } else
  if(demux->synced==1){
    if(head==0x1BB || head==0x1BD || (head>=0x1C0 && head<=0x1EF)){
      demux->synced=2;
      mp_msg(MSGT_DEMUX,MSGL_V,"system stream synced at 0x%X (%d)!\n",demux->filepos,demux->filepos);
      num_elementary_packets100=0; // requires for re-sync!
      num_elementary_packets101=0; // requires for re-sync!
    } else demux->synced=0;
  } // else
  if(demux->synced==2){
      ret=demux_mpg_read_packet(demux,head);
      if(!ret)
        if(--max_packs==0){
          demux->stream->eof=1;
          mp_msg(MSGT_DEMUX,MSGL_ERR,"demux: file doesn't contain the selected audio or video stream\n");
          return 0;
        }
  } else {
    if(head>=0x100 && head<0x1B0){
      if(head==0x100) ++num_elementary_packets100; else
      if(head==0x101) ++num_elementary_packets101;
      mp_msg(MSGT_DEMUX,MSGL_DBG3,"Opps... elementary video packet found: %03X\n",head);
    } else
    if(head>=0x1C0 && head<0x1F0){
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

//extern off_t seek_to_byte;

void demux_seek_mpg(demuxer_t *demuxer,float rel_seek_secs,int flags){
    demux_stream_t *d_audio=demuxer->audio;
    demux_stream_t *d_video=demuxer->video;
    sh_audio_t *sh_audio=d_audio->sh;
    sh_video_t *sh_video=d_video->sh;

  //================= seek in MPEG ==========================
        off_t newpos;
        if(!sh_video->i_bps) // unspecified?
          newpos=demuxer->filepos+2324*75*rel_seek_secs; // 174.3 kbyte/sec
        else
          newpos=demuxer->filepos+(sh_video->i_bps)*rel_seek_secs;

        if(newpos<demuxer->stream->start_pos) newpos=demuxer->stream->start_pos; // for VCD
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
          if(i==0x1B3 || i==0x1B8) break; // found it!
          if(!i || !skip_video_packet(d_video)) break; // EOF?
        }


}


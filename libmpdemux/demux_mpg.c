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

#define UNKNOWN         0
#define VIDEO_MPEG1     0x10000001
#define VIDEO_MPEG2     0x10000002
#define VIDEO_MPEG4     0x10000004
#define VIDEO_H264      0x10000005
#define AUDIO_MP2       0x50
#define AUDIO_A52       0x2000
#define AUDIO_LPCM_BE   0x10001
#define AUDIO_AAC       mmioFOURCC('M', 'P', '4', 'A')

typedef struct mpg_demuxer {
  float last_pts;
  float final_pts;
  int has_valid_timestamps;
  unsigned int es_map[0x40];	//es map of stream types (associated to the pes id) from 0xb0 to 0xef
  int num_a_streams;
  int a_stream_ids[MAX_A_STREAMS];
} mpg_demuxer_t;

static int mpeg_pts_error=0;

static int parse_psm(demuxer_t *demux, int len) {
  unsigned char c, id, type;
  unsigned int plen, prog_len, es_map_len;
  mpg_demuxer_t *priv = (mpg_demuxer_t *) demux->priv;
  
  mp_dbg(MSGT_DEMUX,MSGL_V, "PARSE_PSM, len=%d\n", len);
  if(! len)
    return 0;
  
  c = stream_read_char(demux->stream);
  if(! (c & 0x80)) {
    stream_skip(demux->stream, len - 1);  //not yet valid, discard
    return 0;
  }
  stream_skip(demux->stream, 1);
  prog_len = stream_read_word(demux->stream);		//length of program descriptors
  stream_skip(demux->stream, prog_len);			//.. that we ignore
  es_map_len = stream_read_word(demux->stream);		//length of elementary streams map
  es_map_len = min(es_map_len, len - prog_len - 8);	//sanity check
  while(es_map_len > 0) {
    type = stream_read_char(demux->stream);
    id = stream_read_char(demux->stream);
    if(id >= 0xB0 && id <= 0xEF && priv) {
      int idoffset = id - 0xB0;
      switch(type) {
        case 0x1:
          priv->es_map[idoffset] = VIDEO_MPEG1;
          break;
        case 0x2:
          priv->es_map[idoffset] = VIDEO_MPEG2;
          break;
        case 0x3:
        case 0x4:
          priv->es_map[idoffset] = AUDIO_MP2;
          break;
        case 0x0f:
        case 0x11:
          priv->es_map[idoffset] = AUDIO_AAC;
          break;
        case 0x10:
          priv->es_map[idoffset] = VIDEO_MPEG4;
          break;
        case 0x1b:
          priv->es_map[idoffset] = VIDEO_H264;
          break;
        case 0x81:
          priv->es_map[idoffset] = AUDIO_A52;
          break;
      }
      mp_dbg(MSGT_DEMUX,MSGL_V, "PSM ES, id=0x%x, type=%x, stype: %x\n", id, type, priv->es_map[idoffset]);
    }
    plen = stream_read_word(demux->stream);		//length of elementary stream descriptors
    plen = min(plen, es_map_len);			//sanity check
    stream_skip(demux->stream, plen);			//skip descriptors for now
    es_map_len -= 4 + plen;
  }
  stream_skip(demux->stream, 4);			//skip crc32
  return 1;
}

/// Open an mpg physical stream
static demuxer_t* demux_mpg_open(demuxer_t* demuxer) {
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
  mpg_d->num_a_streams = 0;
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
  return demuxer;
}

static void demux_close_mpg(demuxer_t* demuxer) {
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

static void new_audio_stream(demuxer_t *demux, int aid){
  if(!demux->a_streams[aid]){
    mpg_demuxer_t *mpg_d=(mpg_demuxer_t*)demux->priv;
    sh_audio_t* sh_a;
    new_sh_audio(demux,aid);
    sh_a = (sh_audio_t*)demux->a_streams[aid];
    switch(aid & 0xE0){  // 1110 0000 b  (high 3 bit: type  low 5: id)
      case 0x00: sh_a->format=0x50;break; // mpeg
      case 0xA0: sh_a->format=0x10001;break;  // dvd pcm
      case 0x80: if((aid & 0xF8) == 0x88) sh_a->format=0x2001;//dts
                  else sh_a->format=0x2000;break; // ac3
    }
    if (mpg_d) mpg_d->a_stream_ids[mpg_d->num_a_streams++] = aid;
  }
  if(demux->audio->id==-1) demux->audio->id=aid;
}

static int demux_mpg_read_packet(demuxer_t *demux,int id){
  int d;
  int len;
  unsigned char c=0;
  unsigned int pts=0;
  unsigned int dts=0;
  demux_stream_t *ds=NULL;
  mpg_demuxer_t *priv = (mpg_demuxer_t *) demux->priv;
  
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

  if(id==0x1BC) {
    parse_psm(demux, len);
    return 0;
  }

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
      int aid, rawa52 = 0;
      off_t tmppos;
      unsigned int tmp;

      tmppos = stream_tell(demux->stream);
      tmp = stream_read_word(demux->stream);
      stream_seek(demux->stream, tmppos);
      /// vdr stores A52 without the 4 header bytes, so we have to check this condition first
      if(tmp == 0x0B77) {
        aid = 128;
        rawa52 = 1;
      }
      else {
        aid=stream_read_char(demux->stream);--len;
        if(len<3) return -1; // invalid audio packet
      }
      
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
        new_audio_stream(demux, aid);
      if(demux->audio->id==aid){
        int type;
        ds=demux->audio;
        if(!ds->sh) ds->sh=demux->a_streams[aid];
        // READ Packet: Skip additional audio header data:
        if(!rawa52) {
        c=stream_read_char(demux->stream);//num of frames
        type=stream_read_char(demux->stream);//startpos hi
        type=(type<<8)|stream_read_char(demux->stream);//startpos lo
//        printf("\r[%02X][%04X]",c,type);
        len-=3;
        }
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
    new_audio_stream(demux, aid);
    if(demux->audio->id==aid){
      ds=demux->audio;
      if(!ds->sh) ds->sh=demux->a_streams[aid];
      if(priv && ds->sh) {
        sh_audio_t *sh = (sh_audio_t *)ds->sh;
        if(priv->es_map[id - 0x1B0])
          sh->format = priv->es_map[id - 0x1B0];
          mp_dbg(MSGT_DEMUX,MSGL_DBG2,"ASSIGNED TO STREAM %d CODEC %x\n", id, priv->es_map[id - 0x1B0]);
      }
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
      if(priv && ds->sh) {
        sh_video_t *sh = (sh_video_t *)ds->sh;
        if(priv->es_map[id - 0x1B0]) {
          sh->format = priv->es_map[id - 0x1B0];
          mp_dbg(MSGT_DEMUX,MSGL_DBG2,"ASSIGNED TO STREAM %d CODEC %x\n", id, priv->es_map[id - 0x1B0]);
        }
      }
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

static int num_elementary_packets100=0;
static int num_elementary_packets101=0;
static int num_elementary_packets12x=0;
static int num_elementary_packets1B6=0;
static int num_elementary_packetsPES=0;
static int num_h264_slice=0; //combined slice
static int num_h264_dpa=0; //DPA Slice
static int num_h264_dpb=0; //DPB Slice
static int num_h264_dpc=0; //DPC Slice
static int num_h264_idr=0; //IDR Slice
static int num_h264_sps=0;
static int num_h264_pps=0;

static int num_mp3audio_packets=0;

static int demux_mpg_probe(demuxer_t *demuxer) {
  int pes=1;
  int tmp;
  off_t tmppos;
  int file_format = DEMUXER_TYPE_UNKNOWN;

  tmppos=stream_tell(demuxer->stream);
  tmp=stream_read_dword(demuxer->stream);
  if(tmp==0x1E0 || tmp==0x1C0) {
    tmp=stream_read_word(demuxer->stream);
    if(tmp>1 && tmp<=2048) pes=0; // demuxer->synced=3; // PES...
  }
  stream_seek(demuxer->stream,tmppos);

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

  if(demux_mpg_open(demuxer))
    file_format=DEMUXER_TYPE_MPEG_PS;
  else {
    mp_msg(MSGT_DEMUX,MSGL_V,"MPEG packet stats: p100: %d  p101: %d p1B6: %d p12x: %d sli: %d a: %d b: %d c: %d idr: %d sps: %d pps: %d PES: %d  MP3: %d, synced: %d\n",
     num_elementary_packets100,num_elementary_packets101,
     num_elementary_packets1B6,num_elementary_packets12x,
     num_h264_slice, num_h264_dpa,
     num_h264_dpb, num_h264_dpc=0,
     num_h264_idr, num_h264_sps=0,
     num_h264_pps,
     num_elementary_packetsPES,num_mp3audio_packets, demuxer->synced);

     //MPEG packet stats: p100: 458  p101: 458  PES: 0  MP3: 1103  (.m2v)
     if(num_mp3audio_packets>50 && num_mp3audio_packets>2*num_elementary_packets100
        && abs(num_elementary_packets100-num_elementary_packets101)>2)
       return file_format;

      // some hack to get meaningfull error messages to our unhappy users:
      if(num_elementary_packets100>=2 && num_elementary_packets101>=2 &&
         abs(num_elementary_packets101+8-num_elementary_packets100)<16) {
         if(num_elementary_packetsPES>=4 && num_elementary_packetsPES>=num_elementary_packets100-4) {
           return file_format;
         }
         file_format=DEMUXER_TYPE_MPEG_ES; //  <-- hack is here :)
      } else
#if 1
          // fuzzy mpeg4-es detection. do NOT enable without heavy testing of mpeg formats detection!
        if(num_elementary_packets1B6>3 && num_elementary_packets12x>=1 &&
           num_elementary_packetsPES==0 && num_elementary_packets100<=num_elementary_packets12x &&
           demuxer->synced<2) {
             file_format=DEMUXER_TYPE_MPEG4_ES;
        } else
#endif
#if 1
         // fuzzy h264-es detection. do NOT enable without heavy testing of mpeg formats detection!
        if((num_h264_slice>3 || (num_h264_dpa>3 && num_h264_dpb>3 && num_h264_dpc>3)) && 
          /* FIXME num_h264_sps>=1 && */ num_h264_pps>=1 && num_h264_idr>=1 &&
          num_elementary_packets1B6==0 && num_elementary_packetsPES==0 &&
          demuxer->synced<2) {
            file_format=DEMUXER_TYPE_H264_ES;
        } else
#endif
        {
          if(demuxer->synced==2)
            mp_msg(MSGT_DEMUXER,MSGL_ERR,"MPEG: " MSGTR_MissingVideoStreamBug);
          else
            mp_msg(MSGT_DEMUXER,MSGL_V,MSGTR_NotSystemStream);
        }
  }
  return file_format;
}

static int demux_mpg_es_fill_buffer(demuxer_t *demux, demux_stream_t *ds){
  // Elementary video stream
  if(demux->stream->eof) return 0;
  demux->filepos=stream_tell(demux->stream);
  ds_read_packet(demux->video,demux->stream,STREAM_BUFFER_SIZE,0,demux->filepos,0);
  return 1;
}

/**
 * \brief discard until 0x100 header and return a filled buffer
 * \param b buffer-end pointer
 * \param pos current pos in stream, negative since b points to end of buffer
 * \param s stream to read from
 * \return new position, differs from original pos when eof hit and thus
 *             b was modified to point to the new end of buffer
 */
static int find_end(unsigned char **b, int pos, stream_t *s) {
  register int state = 0xffffffff;
  unsigned char *buf = *b;
  int start = pos;
  int read, unused;
  // search already read part
  while (state != 0x100 && pos) {
    state = state << 8 | buf[pos++];
  }
  // continue search in stream
  while (state != 0x100) {
    register int c = stream_read_char(s);
    if (c < 0) break;
    state = state << 8 | c;
  }
  // modify previous header (from 0x1bc or 0x1bf to 0x100)
  buf[start++] = 0;
  // copy remaining buffer part to current pos
  memmove(&buf[start], &buf[pos], -pos);
  unused = start + -pos; // -unused bytes in buffer
  read = stream_read(s, &buf[unused], -unused);
  unused += read;
  // fix buffer so it ends at pos == 0 (eof case)
  *b = &buf[unused];
  start -= unused;
  return start;
}

/**
 * This format usually uses an insane bitrate, which makes this function
 * performance-critical!
 * Be sure to benchmark any changes with different compiler versions.
 */
static int demux_mpg_gxf_fill_buffer(demuxer_t *demux, demux_stream_t *ds) {
  demux_packet_t *pack;
  int len;
  demux->filepos = stream_tell(demux->stream);
  pack = new_demux_packet(STREAM_BUFFER_SIZE);
  len = stream_read(demux->stream, pack->buffer, STREAM_BUFFER_SIZE);
  if (len <= 0)
    return 0;
  {
    register uint32_t state = (uint32_t)demux->priv;
    register int pos = -len;
    unsigned char *buf = &pack->buffer[len];
    do {
      state = state << 8 | buf[pos];
      if (unlikely((state | 3) == 0x1bf))
        pos = find_end(&buf, pos, demux->stream);
    } while (++pos);
    demux->priv = (void *)state;
    len = buf - pack->buffer;
  }
  if (len < STREAM_BUFFER_SIZE)
    resize_demux_packet(pack, len);
  ds_add_packet(ds, pack);
  return 1;
}

int demux_mpg_fill_buffer(demuxer_t *demux, demux_stream_t *ds){
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
            if(i==0x1B6) {			//vop (frame) startcode
	      int pos = videobuf_len;
	      if(!read_video_packet(d_video)) break; // EOF
	      if((videobuffer[pos+4] & 0x3F) == 0) break;	//I-frame
	    }
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
    demux_stream_t *d_video=demuxer->video;
    sh_video_t *sh_video=d_video->sh;
    mpg_demuxer_t *mpg_d=(mpg_demuxer_t*)demuxer->priv;

    switch(cmd) {
	case DEMUXER_CTRL_GET_TIME_LENGTH:
            if (mpg_d && mpg_d->has_valid_timestamps) {
              *((double *)arg)=(double)mpg_d->final_pts;
              return DEMUXER_CTRL_GUESS;
            }
    		return DEMUXER_CTRL_DONTKNOW;

	case DEMUXER_CTRL_GET_PERCENT_POS:
            if (mpg_d && mpg_d->has_valid_timestamps && mpg_d->final_pts > 0.0) {
              *((int *)arg)=(int)(100 * mpg_d->last_pts / mpg_d->final_pts);
              return DEMUXER_CTRL_OK;
            }
	    return DEMUXER_CTRL_DONTKNOW;

	case DEMUXER_CTRL_SWITCH_AUDIO:
            if (mpg_d && mpg_d->num_a_streams > 1 && demuxer->audio && demuxer->audio->sh) {
              demux_stream_t *d_audio = demuxer->audio;
              sh_audio_t *sh_audio = d_audio->sh;
              sh_audio_t *sh_a;
              int i;
              if (*((int*)arg) < 0)
              {
              for (i = 0; i < mpg_d->num_a_streams; i++) {
                if (d_audio->id == mpg_d->a_stream_ids[i]) break;
              }
              do {
                i = (i+1) % mpg_d->num_a_streams;
                sh_a = (sh_audio_t*)demuxer->a_streams[mpg_d->a_stream_ids[i]];
              } while (sh_a->format != sh_audio->format);
              }
              else {
                for (i = 0; i < mpg_d->num_a_streams; i++)
                  if (*((int*)arg) == mpg_d->a_stream_ids[i]) break;
                if (i < mpg_d->num_a_streams)
                  sh_a = (sh_audio_t*)demuxer->a_streams[*((int*)arg)];
                if (sh_a->format != sh_audio->format)
                  i = mpg_d->num_a_streams;
              }
              if (i < mpg_d->num_a_streams && d_audio->id != mpg_d->a_stream_ids[i]) {
                d_audio->id = mpg_d->a_stream_ids[i];
                d_audio->sh = sh_a;
                ds_free_packs(d_audio);
              }
            }
            *((int*)arg) = demuxer->audio->id;
            return DEMUXER_CTRL_OK;

	default:
	    return DEMUXER_CTRL_NOTIMPL;
    }
}


static int demux_mpg_pes_probe(demuxer_t *demuxer) {
   demuxer->synced = 3;
   return (demux_mpg_probe(demuxer) == DEMUXER_TYPE_MPEG_PS) ? DEMUXER_TYPE_MPEG_PES : 0;
}


static demuxer_t* demux_mpg_es_open(demuxer_t* demuxer)
{
    sh_video_t *sh_video=NULL;

    demuxer->audio->sh = NULL;   // ES streams has no audio channel
    demuxer->video->sh = new_sh_video(demuxer,0); // create dummy video stream header, id=0
    sh_video=demuxer->video->sh;sh_video->ds=demuxer->video;

    return demuxer;
}

static demuxer_t *demux_mpg_gxf_open(demuxer_t *demuxer) {
  demuxer->audio->sh = NULL;
  demuxer->video->sh = new_sh_video(demuxer,0);
  ((sh_video_t *)demuxer->video->sh)->ds = demuxer->video;
  demuxer->priv = (void *) 0xffffffff;
  return demuxer;
}

static demuxer_t* demux_mpg_ps_open(demuxer_t* demuxer)
{
    sh_audio_t *sh_audio=NULL;
    sh_video_t *sh_video=NULL;

    sh_video=demuxer->video->sh;sh_video->ds=demuxer->video;

    if(demuxer->audio->id!=-2) {
        if(!ds_fill_buffer(demuxer->audio)){
            mp_msg(MSGT_DEMUXER,MSGL_INFO,"MPEG: " MSGTR_MissingAudioStream);
            demuxer->audio->sh=NULL;
        } else {
            sh_audio=demuxer->audio->sh;sh_audio->ds=demuxer->audio;
        }
    }

    return demuxer;
}


demuxer_desc_t demuxer_desc_mpeg_ps = {
  "MPEG PS demuxer",
  "mpegps",
  "MPEG-PS",
  "Arpi?",
  "Mpeg",
  DEMUXER_TYPE_MPEG_PS,
  0, // unsafe autodetect
  demux_mpg_probe,
  demux_mpg_fill_buffer,
  demux_mpg_ps_open,
  demux_close_mpg,
  demux_seek_mpg,
  demux_mpg_control,
};


demuxer_desc_t demuxer_desc_mpeg_pes = {
  "MPEG PES demuxer",
  "mpegpes",
  "MPEG-PES",
  "Arpi?",
  "Mpeg",
  DEMUXER_TYPE_MPEG_PES,
  0, // unsafe autodetect
  demux_mpg_pes_probe,
  demux_mpg_fill_buffer,
  demux_mpg_ps_open,
  demux_close_mpg,
  demux_seek_mpg,
  demux_mpg_control,
};


demuxer_desc_t demuxer_desc_mpeg_gxf = {
  "MPEG ES in GXF demuxer",
  "mpeggxf",
  "MPEG-ES in GXF",
  "Reimar D�ffinger",
  "Mpeg",
  DEMUXER_TYPE_MPEG_GXF,
  0, // hack autodetection
  NULL,
  demux_mpg_gxf_fill_buffer,
  demux_mpg_gxf_open,
  NULL,
  NULL,
  NULL
};

demuxer_desc_t demuxer_desc_mpeg_es = {
  "MPEG ES demuxer",
  "mpeges",
  "MPEG-ES",
  "Arpi?",
  "Mpeg",
  DEMUXER_TYPE_MPEG_ES,
  0, // hack autodetection
  NULL,
  demux_mpg_es_fill_buffer,
  demux_mpg_es_open,
  demux_close_mpg,
  demux_seek_mpg,
  demux_mpg_control,
};


demuxer_desc_t demuxer_desc_mpeg4_es = {
  "MPEG4 ES demuxer",
  "mpeg4es",
  "MPEG-ES",
  "Arpi?",
  "Mpeg",
  DEMUXER_TYPE_MPEG4_ES,
  0, // hack autodetection
  NULL,
  demux_mpg_es_fill_buffer,
  demux_mpg_es_open,
  demux_close_mpg,
  demux_seek_mpg,
  demux_mpg_control,
};


demuxer_desc_t demuxer_desc_h264_es = {
  "H.264 ES demuxer",
  "h264es",
  "H264-ES",
  "Arpi?",
  "Mpeg",
  DEMUXER_TYPE_H264_ES,
  0, // hack autodetection
  NULL,
  demux_mpg_es_fill_buffer,
  demux_mpg_es_open,
  demux_close_mpg,
  demux_seek_mpg,
  demux_mpg_control,
};

//  VIVO file parser by A'rpi

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "config.h"
#include "mp_msg.h"
#include "help_mp.h"

#include "stream.h"
#include "demuxer.h"
#include "stheader.h"

#include "bswap.h"

typedef struct {
    float fps;
} vivo_priv_t;

int vivo_check_file(demuxer_t* demuxer){
    int flags=0;
    int i=0;
    int len;
    int len2;
    int c;
    unsigned char buf[2048+256];
    vivo_priv_t* priv;
    
    mp_msg(MSGT_DEMUX,MSGL_V,"Checking for VIVO\n");
    
    c=stream_read_char(demuxer->stream);
    if(c) return 0;
    len=0;
    while((c=stream_read_char(demuxer->stream))>=0x80){
	len+=0x80*(c-0x80);
	if(len>1024) return 0;
    }
    len+=c;
    printf("header block 1 size: %d\n",len);
    //stream_skip(demuxer->stream,len);
    stream_read(demuxer->stream,buf,len);
    buf[len]=0;
//    printf("VIVO header: '%s'\n",buf);
    
    // parse header:
    i=0;
    while(i<len && buf[i]==0x0D && buf[i+1]==0x0A) i+=2; // skip empty lines
    if(strncmp(buf+i,"Version:Vivo/",13)) return 0; // bad version/type!

    priv=malloc(sizeof(vivo_priv_t));
    memset(priv,0,sizeof(vivo_priv_t));
    demuxer->priv=priv;

    // TODO: parse FPS and other info (display title/author etc)
    priv->fps=10.0; // FIXME (parse from header)

#if 0
    c=stream_read_char(demuxer->stream);
    if(c) return 0;
    len2=0;
    while((c=stream_read_char(demuxer->stream))>=0x80){
	len2+=0x80*(c-0x80);
	if(len+len2>2048) return 0;
    }
    len2+=c;
    printf("header block 2 size: %d\n",len2);
    stream_skip(demuxer->stream,len2);
//    stream_read(demuxer->stream,buf+len,len2);
#endif
    
//    c=stream_read_char(demuxer->stream);
//    printf("first packet: %02X\n",c);

return 1;
}


// return value:
//     0 = EOF or no stream found
//     1 = successfully read a packet
int demux_vivo_fill_buffer(demuxer_t *demux){
  demux_stream_t *ds=NULL;
  int c;
  int len=0;
  int seq;
  demux->filepos=stream_tell(demux->stream);
  
  c=stream_read_char(demux->stream);
//  printf("c=%02X\n",c);
  switch(c&0xF0){
  case 0x00: // header - skip it!
      len=stream_read_char(demux->stream);
//      printf("header: %02X\n",len);
      if(len>=0x80) len=0x80*(len-0x80)+stream_read_char(demux->stream);
      printf("vivo extra header: %d bytes\n",len);
      break;
  case 0x10:  // video packet
      len=128;
      ds=demux->video;
      break;
  case 0x20:  // video packet
      len=stream_read_char(demux->stream);
      ds=demux->video;
      break;
  case 0x30:  // audio ?
      len=0x28;
      ds=demux->audio;
      break;
  case 0x40:  // audio packet
      len=24;
      ds=demux->audio;
      break;
  default:
      mp_msg(MSGT_DEMUX,MSGL_WARN,"VIVO - unknown ID found: %02X contact author!\n",c);
  }
  
  if(!ds || ds->id<-1){
      if(len) stream_skip(demux->stream,len);
      return 1;
  }
  
  seq=c&0x0F;

    if(ds->asf_packet){
      if(ds->asf_seq!=seq){
        // closed segment, finalize packet:
        ds_add_packet(ds,ds->asf_packet);
        ds->asf_packet=NULL;
//	printf("packet!\n");
      } else {
        // append data to it!
        demux_packet_t* dp=ds->asf_packet;
        dp->buffer=realloc(dp->buffer,dp->len+len);
        //memcpy(dp->buffer+dp->len,data,len);
	stream_read(demux->stream,dp->buffer+dp->len,len);
        mp_dbg(MSGT_DEMUX,MSGL_DBG4,"data appended! %d+%d\n",dp->len,len);
        dp->len+=len;
        // we are ready now.
	if((c&0xF0)==0x20) --ds->asf_seq; // hack!
        return 1;
      }
    }
    // create new packet:
    { demux_packet_t* dp;
      dp=new_demux_packet(len);
      //memcpy(dp->buffer,data,len);
      stream_read(demux->stream,dp->buffer,len);
//      dp->pts=time*0.001f;
//      dp->flags=keyframe;
//      if(ds==demux->video) printf("ASF time: %8d  dur: %5d  \n",time,dur);
      dp->pos=demux->filepos;
      ds->asf_packet=dp;
      ds->asf_seq=seq;
      // we are ready now.
      return 1;
    }

}

static const short h263_format[8][2] = {
    { 0, 0 },
    { 128, 96 },
    { 176, 144 },
    { 352, 288 },
    { 704, 576 },
    { 1408, 1152 },
    { 320, 240 }   // ???????
};

static unsigned char* buffer;
static int bufptr=0;
static int bitcnt=0;
static unsigned char buf=0;
static int format, width, height;

static unsigned int x_get_bits(int n){
    unsigned int x=0;
    while(n-->0){
	if(!bitcnt){
	    // fill buff
	    buf=buffer[bufptr++];
	    bitcnt=8;
	}
	//x=(x<<1)|(buf&1);buf>>=1;
	x=(x<<1)|(buf>>7);buf<<=1;
	--bitcnt;
    }
    return x;
}

#define get_bits(xxx,n) x_get_bits(n)
#define get_bits1(xxx) x_get_bits(1)
#define skip_bits(xxx,n) x_get_bits(n)
#define skip_bits1(xxx) x_get_bits(1)

/* most is hardcoded. should extend to handle all h263 streams */
static int h263_decode_picture_header(unsigned char *b_ptr)
{
    int i;
        
//    for(i=0;i<16;i++) printf(" %02X",b_ptr[i]); printf("\n");
    
    buffer=b_ptr;
    bufptr=bitcnt=buf=0;

    /* picture header */
    if (get_bits(&s->gb, 22) != 0x20){
	printf("bad picture header\n");
        return -1;
    }
    skip_bits(&s->gb, 8); /* picture timestamp */

    if (get_bits1(&s->gb) != 1){
	printf("bad marker\n");
        return -1;	/* marker */
    }
    if (get_bits1(&s->gb) != 0){
	printf("bad h263 id\n");
        return -1;	/* h263 id */
    }
    skip_bits1(&s->gb);	/* split screen off */
    skip_bits1(&s->gb);	/* camera  off */
    skip_bits1(&s->gb);	/* freeze picture release off */

    format = get_bits(&s->gb, 3);

    if (format != 7) {
        printf("h263_plus = 0  format = %d\n",format);
        /* H.263v1 */
        width = h263_format[format][0];
        height = h263_format[format][1];
	printf("%d x %d\n",width,height);
//        if (!width) return -1;

	printf("pict_type=%d\n",get_bits1(&s->gb));
	printf("unrestricted_mv=%d\n",get_bits1(&s->gb));
#if 1
	printf("SAC: %d\n",get_bits1(&s->gb));
	printf("advanced prediction mode: %d\n",get_bits1(&s->gb));
	printf("PB frame: %d\n",get_bits1(&s->gb));
#else
        if (get_bits1(&s->gb) != 0)
            return -1;	/* SAC: off */
        if (get_bits1(&s->gb) != 0)
            return -1;	/* advanced prediction mode: off */
        if (get_bits1(&s->gb) != 0)
            return -1;	/* not PB frame */
#endif
	printf("qscale=%d\n",get_bits(&s->gb, 5));
        skip_bits1(&s->gb);	/* Continuous Presence Multipoint mode: off */
    } else {
        printf("h263_plus = 1\n");
        /* H.263v2 */
        if (get_bits(&s->gb, 3) != 1){
	    printf("H.263v2 A error\n");
            return -1;
	}
        if (get_bits(&s->gb, 3) != 6){ /* custom source format */
	    printf("custom source format\n");
            return -1;
	}
        skip_bits(&s->gb, 12);
        skip_bits(&s->gb, 3);
	printf("pict_type=%d\n",get_bits(&s->gb, 3) + 1);
//        if (s->pict_type != I_TYPE &&
//            s->pict_type != P_TYPE)
//            return -1;
        skip_bits(&s->gb, 7);
        skip_bits(&s->gb, 4); /* aspect ratio */
        width = (get_bits(&s->gb, 9) + 1) * 4;
        skip_bits1(&s->gb);
        height = get_bits(&s->gb, 9) * 4;
	printf("%d x %d\n",width,height);
        //if (height == 0)
        //    return -1;
	printf("qscale=%d\n",get_bits(&s->gb, 5));
    }

    /* PEI */
    while (get_bits1(&s->gb) != 0) {
        skip_bits(&s->gb, 8);
    }
//    s->f_code = 1;
//    s->width = width;
//    s->height = height;
    return 0;
}



void demux_open_vivo(demuxer_t* demuxer){
    vivo_priv_t* priv=demuxer->priv;

  if(!ds_fill_buffer(demuxer->video)){
    mp_msg(MSGT_DEMUX,MSGL_ERR,"VIVO: " MSGTR_MissingVideoStreamBug);
    return;
  }
  
  h263_decode_picture_header(demuxer->video->buffer);
  

{		sh_video_t* sh=new_sh_video(demuxer,0);
    
		sh->format=0x6f766976; // "vivo"
		if(!sh->fps) sh->fps=priv->fps;
		sh->frametime=1.0f/sh->fps;
		sh->disp_w=width; // FIXME
		sh->disp_h=height;

		// emulate BITMAPINFOHEADER:
		sh->bih=malloc(sizeof(BITMAPINFOHEADER));
		memset(sh->bih,0,sizeof(BITMAPINFOHEADER));
		sh->bih->biSize=40;
		sh->bih->biWidth=sh->disp_w;
		sh->bih->biHeight=sh->disp_h;
		sh->bih->biPlanes=1;
		sh->bih->biBitCount=24;
		sh->bih->biCompression=sh->format;
		sh->bih->biSizeImage=sh->bih->biWidth*sh->bih->biHeight*3;
		demuxer->video->sh=sh; sh->ds=demuxer->video;
		demuxer->video->id=0;
}

if(demuxer->audio->id>=-1){
  if(!ds_fill_buffer(demuxer->audio)){
    mp_msg(MSGT_DEMUX,MSGL_ERR,"VIVO: " MSGTR_MissingAudioStream);
  } else
{		sh_audio_t* sh=new_sh_audio(demuxer,1);
		sh->format=0x111; // 0x112
		// Emulate WAVEFORMATEX struct:
		sh->wf=malloc(sizeof(WAVEFORMATEX));
		memset(sh->wf,0,sizeof(WAVEFORMATEX));
		sh->wf->nChannels=1;
		sh->wf->wBitsPerSample=16;
		sh->wf->nSamplesPerSec=22050;
		sh->wf->nAvgBytesPerSec=sh->wf->nChannels*sh->wf->wBitsPerSample*sh->wf->nSamplesPerSec/8;
		demuxer->audio->sh=sh; sh->ds=demuxer->audio;
		demuxer->audio->id=1;
}
}

}


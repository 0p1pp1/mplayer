#define PES_MAX_SIZE 2048
/* 
 * Based on:
 *
 * test_av.c - Test program for new API
 *
 * Copyright (C) 2000 Ralph  Metzler <ralph@convergence.de>
 *                  & Marcus Metzler <marcus@convergence.de>
 *                    for convergence integrated media GmbH
 *
 * libav - MPEG-PS multiplexer, part of ffmpeg
 * Copyright Gerard Lantau  (see http://ffmpeg.sf.net)
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "config.h"

#ifdef HAVE_DVB

#include <sys/poll.h>

#include <sys/ioctl.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h>

#include <ost/dmx.h>
#include <ost/frontend.h>
#include <ost/sec.h>
#include <ost/video.h>
#include <ost/audio.h>

#endif

#include "config.h"
#include "video_out.h"
#include "video_out_internal.h"

LIBVO_EXTERN (mpegpes)

int vo_mpegpes_fd=-1;
int vo_mpegpes_fd2=-1;

#ifdef USE_LIBAVCODEC

#ifdef USE_LIBAVCODEC_SO
#include <libffmpeg/avcodec.h>
#else
#include "../libavcodec/avcodec.h"
#endif
static unsigned char *picture_buf=NULL;
static unsigned char *outbuf=NULL;
static int outbuf_size = 100000;

static int s_pos_x,s_pos_y;
static int d_pos_x,d_pos_y;

static int osd_w,osd_h;

static AVPicture picture;
static AVCodec *codec=NULL;
static AVCodecContext codec_context;
extern int avcodec_inited;

#endif

static vo_info_t vo_info = 
{
#ifdef HAVE_DVB
	"Mpeg-PES to DVB card",
#else
	"Mpeg-PES file",
#endif
	"mpegpes",
	"A'rpi",
	""
};

static uint32_t
init(uint32_t s_width, uint32_t s_height, uint32_t width, uint32_t height, uint32_t fullscreen, char *title, uint32_t format)
{
#ifdef HAVE_DVB
    //|O_NONBLOCK
	if((vo_mpegpes_fd = open("/dev/ost/video",O_RDWR)) < 0){
		perror("DVB VIDEO DEVICE: ");
		return -1;
	}
	if((vo_mpegpes_fd2 = open("/dev/ost/audio",O_RDWR|O_NONBLOCK)) < 0){
		perror("DVB AUDIO DEVICE: ");
		return -1;
	}
	if ( (ioctl(vo_mpegpes_fd,VIDEO_SET_BLANK, false) < 0)){
		perror("DVB VIDEO SET BLANK: ");
		return -1;
	}
	if ( (ioctl(vo_mpegpes_fd,VIDEO_SELECT_SOURCE, VIDEO_SOURCE_MEMORY) < 0)){
		perror("DVB VIDEO SELECT SOURCE: ");
		return -1;
	}
#if 1
	if ( (ioctl(vo_mpegpes_fd2,AUDIO_SELECT_SOURCE, AUDIO_SOURCE_MEMORY) < 0)){
		perror("DVB AUDIO SELECT SOURCE: ");
		return -1;
	}
	if ( (ioctl(vo_mpegpes_fd2,AUDIO_PLAY) < 0)){
		perror("DVB AUDIO PLAY: ");
		return -1;
	}
#else
	if ( (ioctl(vo_mpegpes_fd2,AUDIO_STOP,0) < 0)){
		perror("DVB AUDIO STOP: ");
		return -1;
	}
#endif
	if ( (ioctl(vo_mpegpes_fd,VIDEO_PLAY) < 0)){
		perror("DVB VIDEO PLAY: ");
		return -1;
	}
	if ( (ioctl(vo_mpegpes_fd2,AUDIO_SET_AV_SYNC, false) < 0)){
		perror("DVB AUDIO SET AV SYNC: ");
		return -1;
	}
	if ( (ioctl(vo_mpegpes_fd2,AUDIO_SET_MUTE, false) < 0)){
		perror("DVB AUDIO SET MUTE: ");
		return -1;
	}

#else
    vo_mpegpes_fd=open("grab.mpg",O_WRONLY|O_CREAT);
    if(vo_mpegpes_fd<0){	
	perror("vo_mpegpes");
	return -1;
    }
#endif

#ifdef USE_LIBAVCODEC
    picture_buf=NULL;
if(format==IMGFMT_YV12){
    int size;

    if(!avcodec_inited){
      avcodec_init();
      avcodec_register_all();
      avcodec_inited=1;
    }

    /* find the mpeg1 video encoder */
    codec = avcodec_find_encoder(CODEC_ID_MPEG1VIDEO);
    if (!codec) {
        fprintf(stderr, "mpeg1 codec not found\n");
        return -1;
    }
    memset(&codec_context,0,sizeof(codec_context));
    codec_context.bit_rate=100000; // not used
    codec_context.frame_rate=25*FRAME_RATE_BASE; // !!!!!
    codec_context.gop_size=0; // I frames only
    codec_context.flags=CODEC_FLAG_QSCALE;
    codec_context.quality=3; // quality!  1..31  (1=best,slowest)

#if 0
    codec_context.width=width;
    codec_context.height=height;
#else  
    if(width<=352 && height<=288){
      codec_context.width=352;
      codec_context.height=288;
    } else
    if(width<=352 && height<=576){
      codec_context.width=352;
      codec_context.height=576;
    } else
    if(width<=480 && height<=576){
      codec_context.width=480;
      codec_context.height=576;
    } else
    if(width<=544 && height<=576){
      codec_context.width=544;
      codec_context.height=576;
    } else {
      codec_context.width=704;
      codec_context.height=576;
    }
#endif

    osd_w=s_width;
    d_pos_x=(codec_context.width-(int)s_width)/2;
    if(d_pos_x<0){
      s_pos_x=-d_pos_x;d_pos_x=0;
      osd_w=codec_context.width;
    } else s_pos_x=0;

    osd_h=s_height;
    d_pos_y=(codec_context.height-(int)s_height)/2;
    if(d_pos_y<0){
      s_pos_y=-d_pos_y;d_pos_y=0;
      osd_h=codec_context.height;
    } else s_pos_y=0;

    printf("[vo] position mapping: %d;%d => %d;%d\n",s_pos_x,s_pos_y,d_pos_x,d_pos_y);

    /* open it */
    if (avcodec_open(&codec_context, codec) < 0) {
        fprintf(stderr, "could not open codec\n");
        return -1;
    }
    
    outbuf_size=10000+width*height;  // must be enough!
    outbuf = malloc(outbuf_size);

    size = codec_context.width*codec_context.height;
    picture_buf = malloc((size * 3) / 2); /* size for YUV 420 */

    memset(picture_buf,0,size); // clear Y
    memset(picture_buf+size,128,size/2); // clear UV
    
    picture.data[0] = picture_buf;
    picture.data[1] = picture.data[0] + size;
    picture.data[2] = picture.data[1] + size / 4;
    picture.linesize[0] = codec_context.width;
    picture.linesize[1] = codec_context.width / 2;
    picture.linesize[2] = codec_context.width / 2;

}
#endif
    return 0;
}

static const vo_info_t*
get_info(void)
{
    return &vo_info;
}

#ifdef USE_LIBAVCODEC
static void draw_alpha(int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride){
    int x,y;
    vo_draw_alpha_yv12(w,h,src,srca,stride,
      picture.data[0]+(x0+d_pos_x)+(y0+d_pos_y)*picture.linesize[0],picture.linesize[0]);
}
#endif

static void draw_osd(void)
{
#ifdef USE_LIBAVCODEC
  if(picture_buf){ // YV12 only:
    vo_draw_text(osd_w,osd_h,draw_alpha);
  }
#endif
}


static void my_write(unsigned char* data,int len){
#ifdef HAVE_DVB
#define NFD   2
    struct pollfd pfd[NFD];

//    printf("write %d bytes  \n",len);

	pfd[0].fd = vo_mpegpes_fd;
	pfd[0].events = POLLOUT;
	
	pfd[1].fd = vo_mpegpes_fd2;
	pfd[1].events = POLLOUT;

    while(len>0){
	if (poll(pfd,NFD,1)){
	    if (pfd[0].revents & POLLOUT){
		int ret=write(vo_mpegpes_fd,data,len);
//		printf("ret=%d  \n",ret); 
		if(ret<=0){
		    perror("write");
		    usleep(0);
		} else {
		    len-=ret; data+=ret;
		}
	    } else usleep(1000);
	}
    }

#else
    write(vo_mpegpes_fd,data,len); // write to file
#endif
}

static unsigned char pes_header[PES_MAX_SIZE];

void send_pes_packet(unsigned char* data,int len,int id,int timestamp){
    int ptslen=timestamp?5:1;

	      // startcode:
	      pes_header[0]=pes_header[1]=0;
	      pes_header[2]=id>>8; pes_header[3]=id&255;
    
    while(len>0){
	    int payload_size=len;  // data + PTS
	    if(6+ptslen+payload_size>PES_MAX_SIZE) payload_size=PES_MAX_SIZE-(6+ptslen);
	    
    // construct PES header:  (code from ffmpeg's libav)
	      // packetsize:
	      pes_header[4]=(ptslen+payload_size)>>8;
	      pes_header[5]=(ptslen+payload_size)&255;

	if(ptslen==5){
	      int x;
	      // presentation time stamp:
	      x=(0x02 << 4) | (((timestamp >> 30) & 0x07) << 1) | 1;
	      pes_header[6]=x;
	      x=((((timestamp >> 15) & 0x7fff) << 1) | 1);
	      pes_header[7]=x>>8; pes_header[8]=x&255;
	      x=((((timestamp) & 0x7fff) << 1) | 1);
	      pes_header[9]=x>>8; pes_header[10]=x&255;
	} else {
	      // stuffing and header bits:
	      pes_header[6]=0x0f;
	}

	memcpy(&pes_header[6+ptslen],data,payload_size);
	my_write(pes_header,6+ptslen+payload_size);

	len-=payload_size; data+=payload_size;
	ptslen=1; // store PTS only once, at first packet!
    }

//    printf("PES: draw frame!  pts=%d   size=%d  \n",timestamp,len);

}

void send_lpcm_packet(unsigned char* data,int len,int id,unsigned int timestamp,int freq_id){

    int ptslen=timestamp?5:0;

	      // startcode:
	      pes_header[0]=pes_header[1]=0;
	      pes_header[2]=1; pes_header[3]=0xBD;
    
    while(len>=4){
	    int payload_size;
	    
	    payload_size=PES_MAX_SIZE-6-20; // max possible data len
	    if(payload_size>len) payload_size=len;
	    payload_size&=(~3); // align!

	    //if(6+payload_size>PES_MAX_SIZE) payload_size=PES_MAX_SIZE-6;
	    
	      // packetsize:
	      pes_header[4]=(payload_size+3+ptslen+7)>>8;
	      pes_header[5]=(payload_size+3+ptslen+7)&255;

	      // stuffing:
	      pes_header[6]=0x81;
	      pes_header[7]=0x80;

	      // hdrlen:
	      pes_header[8]=ptslen;
	      
	  if(ptslen){
	      int x;
	      // presentation time stamp:
	      x=(0x02 << 4) | (((timestamp >> 30) & 0x07) << 1) | 1;
	      pes_header[9]=x;
	      x=((((timestamp >> 15) & 0x7fff) << 1) | 1);
	      pes_header[10]=x>>8; pes_header[11]=x&255;
	      x=((((timestamp) & 0x7fff) << 1) | 1);
	      pes_header[12]=x>>8; pes_header[13]=x&255;
	  }
	      
// ============ LPCM header: (7 bytes) =================
// Info by mocm@convergence.de

//	   ID:
	      pes_header[ptslen+9]=id;

//	   number of frames:
	      pes_header[ptslen+10]=0x07;

//	   first acces unit pointer, i.e. start of audio frame:
	      pes_header[ptslen+11]=0x00;
	      pes_header[ptslen+12]=0x04;

//	   audio emphasis on-off                                  1 bit
//	   audio mute on-off                                      1 bit
//	   reserved                                               1 bit
//	   audio frame number                                     5 bit
	      pes_header[ptslen+13]=0x0C;

//	   quantization word length                               2 bit
//	   audio sampling frequency (48khz = 0, 96khz = 1)        2 bit
//	   reserved                                               1 bit
//	   number of audio channels - 1 (e.g. stereo = 1)         3 bit
	      pes_header[ptslen+14]=1|(freq_id<<4);

//	   dynamic range control (0x80 if off)
	      pes_header[ptslen+15]=0x80;

	memcpy(&pes_header[6+3+ptslen+7],data,payload_size);
	my_write(pes_header,6+3+ptslen+7+payload_size);

	len-=payload_size; data+=payload_size;
	ptslen=0; // store PTS only once, at first packet!
    }

//    printf("PES: draw frame!  pts=%d   size=%d  \n",timestamp,len);

}


static uint32_t draw_frame(uint8_t * src[])
{
    vo_mpegpes_t *p=(vo_mpegpes_t *)src[0];
    unsigned char *data=p->data;
//    int tmp=-1;
    send_pes_packet(p->data,p->size,p->id,p->timestamp);  // video data
//    send_pes_packet(&tmp,0,0x1C0,p->timestamp+30000); // fake audio data

    return 0;
}

static void flip_page (void)
{
#ifdef USE_LIBAVCODEC
  if(picture_buf){ // YV12 only:
    int out_size;
    static int fno=0;
    /* encode the image */
    out_size = avcodec_encode_video(&codec_context, outbuf, outbuf_size, &picture);
    send_pes_packet(outbuf,out_size,0x1E0,fno*(90000/25));++fno;
//    printf("frame size: %d  \n",out_size);
  }
#endif
}

static uint32_t draw_slice(uint8_t *srcimg[], int stride[], int w,int h,int x0,int y0)
{
#ifdef USE_LIBAVCODEC
    int y;
    unsigned char* s;
    unsigned char* d;
    
    x0+=d_pos_x;
    y0+=d_pos_y;
    if(x0+w>picture.linesize[0]) w=picture.linesize[0]-x0; // !!
    if(y0+h>codec_context.height) h=codec_context.height-y0;

    // Y
    s=srcimg[0]+s_pos_x+s_pos_y*stride[0];
    d=picture.data[0]+x0+y0*picture.linesize[0];
    for(y=0;y<h;y++){
	memcpy(d,s,w);
	s+=stride[0];
	d+=picture.linesize[0];
    }
    
    w/=2;h/=2;x0/=2;y0/=2;

    // U
    s=srcimg[1]+(s_pos_x/2)+(s_pos_y/2)*stride[1];
    d=picture.data[1]+x0+y0*picture.linesize[1];
    for(y=0;y<h;y++){
	memcpy(d,s,w);
	s+=stride[1];
	d+=picture.linesize[1];
    }

    // V
    s=srcimg[2]+(s_pos_x/2)+(s_pos_y/2)*stride[2];
    d=picture.data[2]+x0+y0*picture.linesize[2];
    for(y=0;y<h;y++){
	memcpy(d,s,w);
	s+=stride[2];
	d+=picture.linesize[2];
    }
#endif
    return 0;
}


static uint32_t
query_format(uint32_t format)
{
    if(format==IMGFMT_MPEGPES) return 1|256;
#ifdef USE_LIBAVCODEC
    if(format==IMGFMT_YV12) return 1|256;
#endif
    return 0;
}

static void
uninit(void)
{
#ifdef USE_LIBAVCODEC
  if(picture_buf){ // YV12 only:
    free(outbuf);
    free(picture_buf);
  }
#endif
    if(vo_mpegpes_fd>=0){ close(vo_mpegpes_fd);vo_mpegpes_fd=-1;}
    if(vo_mpegpes_fd2>=0){ close(vo_mpegpes_fd2);vo_mpegpes_fd2=-1;}
}


static void check_events(void)
{
}


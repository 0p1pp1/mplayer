#include <stdio.h>
#include <stdlib.h>

#include "audio_out.h"
#include "audio_out_internal.h"

#include "afmt.h"

static ao_info_t info = 
{
	"mpeg-pes audio output",
	"mpegpes",
	"A'rpi",
	""
};

LIBAO_EXTERN(mpegpes)


// to set/get/query special features/parameters
static int control(int cmd,int arg){
    return -1;
}

static int freq=0;
static int freq_id=0;

// open & setup audio device
// return: 1=success 0=fail
static int init(int rate,int channels,int format,int flags){

    ao_data.outburst=2000;
    ao_data.format=format;
freq=rate;
    
    switch(rate){
	case 48000:	freq_id=0;break;
	case 96000:	freq_id=1;break;
	case 44100:	freq_id=2;break;
	case 32000:	freq_id=3;break;
	default:
	    fprintf(stderr,"ao_mpegpes: %d Hz not supported, try to resample (RTFM)\n",rate);
	    return 0;
    }

    return 1;
}

// close audio device
static void uninit(){

}

// stop playing and empty buffers (for seeking/pause)
static void reset(){

}

// stop playing, keep buffers (for pause)
static void audio_pause()
{
    // for now, just call reset();
    reset();
}

// resume playing, after audio_pause()
static void audio_resume()
{
}

void send_pes_packet(unsigned char* data,int len,int id,int timestamp);
void send_lpcm_packet(unsigned char* data,int len,int id,int timestamp,int freq_id);
extern int vo_pts;

// return: how many bytes can be played without blocking
static int get_space(){
    float x=(float)(vo_pts-ao_data.pts)/90000.0;
    int y;
    if(x<=0) return 0;
    y=freq*4*x;y/=ao_data.outburst;y*=ao_data.outburst;
    if(y>32000) y=32000;
//    printf("diff: %5.3f -> %d  \n",x,y);
    return y;
}

// plays 'len' bytes of 'data'
// it should round it down to outburst*n
// return: number of bytes played
static int play(void* data,int len,int flags){
    if(ao_data.format==AFMT_MPEG)
	send_pes_packet(data,len,0x1C0,ao_data.pts);
    else {
	int i;
	unsigned short *s=data;
//	if(len>2000) len=2000;
//	printf("ao_mpegpes: len=%d  \n",len);
	for(i=0;i<len/2;i++) s[i]=(s[i]>>8)|(s[i]<<8); // le<->be
	send_lpcm_packet(data,len,0xA0,ao_data.pts,freq_id);
    }
    return len;
}

// return: delay in seconds between first and last sample in buffer
static float get_delay(){

    return 0.0;
}


#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bswap.h"
#include "afmt.h"
#include "audio_out.h"
#include "audio_out_internal.h"

static ao_info_t info = 
{
	"RAW PCM/WAVE file writer audio output",
	"pcm",
	"Atmosfear",
	""
};

LIBAO_EXTERN(pcm)

extern int vo_pts;

char *ao_outputfilename = NULL;
int ao_pcm_waveheader = 1;

#define WAV_ID_RIFF 0x46464952 /* "RIFF" */
#define WAV_ID_WAVE 0x45564157 /* "WAVE" */
#define WAV_ID_FMT  0x20746d66 /* "fmt " */
#define WAV_ID_DATA 0x61746164 /* "data" */
#define WAV_ID_PCM  0x0001

struct WaveHeader
{
	unsigned long riff;
	unsigned long file_length;
	unsigned long wave;
	unsigned long fmt;
	unsigned long fmt_length;
	short fmt_tag;
	short channels;
	unsigned long sample_rate;
	unsigned long bytes_per_second;
	short block_align;
	short bits;
	unsigned long data;
	unsigned long data_length;
};

/* init with default values */
static struct WaveHeader wavhdr = {
	le2me_32(WAV_ID_RIFF),
        /* same conventions than in sox/wav.c/wavwritehdr() */
	0, //le2me_32(0x7ffff024),
	le2me_32(WAV_ID_WAVE),
	le2me_32(WAV_ID_FMT),
	le2me_32(16),
	le2me_16(WAV_ID_PCM),
	le2me_16(2),
	le2me_32(44100),
	le2me_32(192000),
	le2me_16(4),
	le2me_16(16),
	le2me_32(WAV_ID_DATA),
	0, //le2me_32(0x7ffff000)
};

static FILE *fp = NULL;

// to set/get/query special features/parameters
static int control(int cmd,void *arg){
    return -1;
}

// open & setup audio device
// return: 1=success 0=fail
static int init(int rate,int channels,int format,int flags){
	int bits;
	if(!ao_outputfilename) {
		ao_outputfilename = strdup(ao_pcm_waveheader ? "audiodump.wav" : "audiodump.pcm");
	}

	/* bits is only equal to format if (format == 8) or (format == 16);
	   this means that the following "if" is a kludge and should
	   really be a switch to be correct in all cases */

	bits=8;
	switch(format){
	case AFMT_S8:
	    format=AFMT_U8;
	case AFMT_U8:
	    break;
	default:
	    format=AFMT_S16_LE;
	    bits=16;
	    break;
	}

	ao_data.outburst = 65536;
	ao_data.buffersize= 2*65536;
	ao_data.channels=channels;
	ao_data.samplerate=rate;
	ao_data.format=format;
	ao_data.bps=channels*rate*(bits/8);

	wavhdr.channels = le2me_16(ao_data.channels);
	wavhdr.sample_rate = le2me_32(ao_data.samplerate);
	wavhdr.bytes_per_second = le2me_32(ao_data.bps);
	wavhdr.bits = le2me_16(bits);
	
	wavhdr.data_length=le2me_32(0x7ffff000);
	wavhdr.file_length = wavhdr.data_length + sizeof(wavhdr) - 8;

	printf("PCM: File: %s (%s)\n"
	       "PCM: Samplerate: %iHz Channels: %s Format %s\n",
	       ao_outputfilename, (ao_pcm_waveheader?"WAVE":"RAW PCM"), rate,
	       (channels > 1) ? "Stereo" : "Mono", audio_out_format_name(format));
	printf("PCM: Info: fastest dumping is achieved with -vc dummy -vo null\n"
	       "PCM: Info: to write WAVE files use -waveheader (default); "
	       "for RAW PCM -nowaveheader.\n");

	fp = fopen(ao_outputfilename, "wb");
	if(fp) {
		if(ao_pcm_waveheader){ /* Reserve space for wave header */
			fwrite(&wavhdr,sizeof(wavhdr),1,fp);
			wavhdr.file_length=wavhdr.data_length=0;
		}
		return 1;
	}
	printf("PCM: Failed to open %s for writing!\n", ao_outputfilename);
	return 0;
}

// close audio device
static void uninit(){
	
	if(ao_pcm_waveheader && fseek(fp, 0, SEEK_SET) == 0){ /* Write wave header */
		wavhdr.file_length = wavhdr.data_length + sizeof(wavhdr) - 8;
		wavhdr.file_length = le2me_32(wavhdr.file_length);
		wavhdr.data_length = le2me_32(wavhdr.data_length);
		fwrite(&wavhdr,sizeof(wavhdr),1,fp);
	}
	fclose(fp);
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

// return: how many bytes can be played without blocking
static int get_space(){

    if(vo_pts)
      return ao_data.pts < vo_pts ? ao_data.outburst : 0;
    return ao_data.outburst;
}

// plays 'len' bytes of 'data'
// it should round it down to outburst*n
// return: number of bytes played
static int play(void* data,int len,int flags){

// let libaf to do the conversion...
#if 0
//#ifdef WORDS_BIGENDIAN
	if (ao_data.format == AFMT_S16_LE) {
	  unsigned short *buffer = (unsigned short *) data;
	  register int i;
	  for(i = 0; i < len/2; ++i) {
	    buffer[i] = le2me_16(buffer[i]);
	  }
	}
#endif 

	//printf("PCM: Writing chunk!\n");
	fwrite(data,len,1,fp);

	if(ao_pcm_waveheader)
		wavhdr.data_length += len;
	
	return len;
}

// return: delay in seconds between first and last sample in buffer
static float get_delay(){

    return 0.0;
}







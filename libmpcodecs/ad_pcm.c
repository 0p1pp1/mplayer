#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "config.h"
#include "ad_internal.h"
#include "../libaf/af_format.h"

static ad_info_t info = 
{
	"Uncompressed PCM audio decoder",
	"pcm",
	"Nick Kurshev",
	"A'rpi",
	""
};

LIBAD_EXTERN(pcm)

static int init(sh_audio_t *sh_audio)
{
  WAVEFORMATEX *h=sh_audio->wf;
  sh_audio->i_bps=h->nAvgBytesPerSec;
  sh_audio->channels=h->nChannels;
  sh_audio->samplerate=h->nSamplesPerSec;
  sh_audio->samplesize=(h->wBitsPerSample+7)/8;
  sh_audio->sample_format=AF_FORMAT_S16_LE; // default
  switch(sh_audio->format){ /* hardware formats: */
    case 0x0:
    case 0x1: // Microsoft PCM
       switch (sh_audio->samplesize) {
         case 1: sh_audio->sample_format=AF_FORMAT_U8; break;
         case 2: sh_audio->sample_format=AF_FORMAT_S16_LE; break;
         case 3: sh_audio->sample_format=AF_FORMAT_S24_LE; break;
         case 4: sh_audio->sample_format=AF_FORMAT_S32_LE; break;
       }
       break;
    case 0x6:  sh_audio->sample_format=AF_FORMAT_A_LAW;break;
    case 0x7:  sh_audio->sample_format=AF_FORMAT_MU_LAW;break;
    case 0x11: sh_audio->sample_format=AF_FORMAT_IMA_ADPCM;break;
    case 0x50: sh_audio->sample_format=AF_FORMAT_MPEG2;break;
/*    case 0x2000: sh_audio->sample_format=AFMT_AC3; */
    case 0x20776172: // 'raw '
       sh_audio->sample_format=AF_FORMAT_S16_BE;
       if(sh_audio->samplesize==1) sh_audio->sample_format=AF_FORMAT_U8;
       break;
    case 0x736F7774: // 'twos'
       sh_audio->sample_format=AF_FORMAT_S16_BE;
       // intended fall-through
    case 0x74776F73: // 'swot'
       if(sh_audio->samplesize==1) sh_audio->sample_format=AF_FORMAT_S8;
// Uncomment this if twos audio is broken for you
// (typically with movies made on sgi machines)
// This is just a workaround, the real bug is elsewhere
#if 0
       sh_audio->ds->ss_div= sh_audio->samplesize;
       sh_audio->ds->ss_mul= sh_audio->samplesize * sh_audio->channels;
#endif
       break;
    case 0x32336c66: // 'fl32', bigendian float32
       sh_audio->sample_format=AF_FORMAT_FLOAT_BE;
       sh_audio->samplesize=4;
       break;
    default: if(sh_audio->samplesize!=2) sh_audio->sample_format=AF_FORMAT_U8;
  }
  return 1;
}

static int preinit(sh_audio_t *sh)
{
  sh->audio_out_minsize=2048;
  return 1;
}

static void uninit(sh_audio_t *sh)
{
}

static int control(sh_audio_t *sh,int cmd,void* arg, ...)
{
  int skip;
    switch(cmd)
    {
      case ADCTRL_SKIP_FRAME:
	skip=sh->i_bps/16;
	skip=skip&(~3);
	demux_read_data(sh->ds,NULL,skip);
	return CONTROL_TRUE;
    }
  return CONTROL_UNKNOWN;
}

static int decode_audio(sh_audio_t *sh_audio,unsigned char *buf,int minlen,int maxlen)
{
  int len=sh_audio->channels*sh_audio->samplesize-1;
  len=(minlen+len)&(~len); // sample align
  len=demux_read_data(sh_audio->ds,buf,len);
  return len;
}

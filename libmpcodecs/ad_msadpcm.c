#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "config.h"
#include "ad_internal.h"

static ad_info_t info = 
{
	"MS ADPCM audio decoder",
	"msadpcm",
	AFM_MSADPCM,
	"Nick Kurshev",
	"Mike Melanson",
	""
};

LIBAD_EXTERN(msadpcm)

#define MS_ADPCM_PREAMBLE_SIZE 7

static int preinit(sh_audio_t *sh_audio)
{
  sh_audio->audio_out_minsize = sh_audio->wf->nBlockAlign * 4;
  sh_audio->ds->ss_div = 
    (sh_audio->wf->nBlockAlign - MS_ADPCM_PREAMBLE_SIZE) * 2;
  sh_audio->ds->ss_mul = sh_audio->wf->nBlockAlign;

  return 1;
}

static int init(sh_audio_t *sh_audio)
{
  sh_audio->channels=sh_audio->wf->nChannels;
  sh_audio->samplerate=sh_audio->wf->nSamplesPerSec;
  sh_audio->i_bps = sh_audio->wf->nBlockAlign *
    (sh_audio->channels*sh_audio->samplerate) / sh_audio->ds->ss_div;

  if ((sh_audio->a_in_buffer =
    (unsigned char *)malloc(sh_audio->ds->ss_mul)) == NULL)
    return 0;

  return 1;
}

static void uninit(sh_audio_t *sh_audio)
{
  free(sh_audio->a_in_buffer);
}

static int control(sh_audio_t *sh,int cmd,void* arg, ...)
{
    // TODO!!!
  return CONTROL_UNKNOWN;
}

static int decode_audio(sh_audio_t *sh_audio,unsigned char *buf,int minlen,int maxlen)
{
  if (demux_read_data(sh_audio->ds, sh_audio->a_in_buffer,
      sh_audio->ds->ss_mul) != 
      sh_audio->ds->ss_mul) 
         return -1; /* EOF */

  return 2 * ms_adpcm_decode_block(
          (unsigned short*)buf, sh_audio->a_in_buffer,
          sh_audio->wf->nChannels, sh_audio->wf->nBlockAlign);
}

/*
  ao_alsa9 - ALSA-0.9.x output plugin for MPlayer

  (C) Alex Beregszaszi <alex@naxine.org>
*/

#include <errno.h>
#include <sys/soundcard.h> /* AFMT_* */
#include <sys/asoundlib.h>

#include "../config.h"

#include "audio_out.h"
#include "audio_out_internal.h"

extern int verbose;

static ao_info_t info = 
{
    "ALSA-0.9.x audio output",
    "alsa9",
    "Alex Beregszaszi <alex@naxine.org>",
    "under developement"
};

LIBAO_EXTERN(alsa9)

/* global variables:
    ao_samplerate
    ao_channels
    ao_format
    ao_bps
    ao_outburst
    ao_buffersize
*/

static snd_pcm_t *alsa_handler;
static snd_pcm_format_t alsa_format;
static snd_pcm_hw_params_t *alsa_hwparams;
static snd_pcm_sw_params_t *alsa_swparams;
static char *alsa_device;
#define ALSA_DEVICE_SIZE 48

/* to set/get/query special features/parameters */
static int control(int cmd, int arg)
{
    switch(cmd)
    {
	case AOCONTROL_GET_DEVICE:
	    return(char *)alsa_device; /* egy kicsit brutalis, dehat :) */
	case AOCONTROL_SET_DEVICE:
	    strncpy(alsa_device, (char *)arg, ALSA_DEVICE_SIZE);
	    break;
    }
    return(CONTROL_UNKNOWN);
}

/*
    open & setup audio device
    return: 1=success 0=fail
*/
static int init(int rate_hz, int channels, int format, int flags)
{
    int err;
    int cards = -1;
    snd_pcm_info_t *alsa_info;
    int chunck_size;

    printf("alsa-init: requested format: %d Hz, %d channels, %s\n", rate_hz,
	channels, audio_out_format_name(format));

    alsa_handler = NULL;

    if (verbose)
	printf("alsa-init: compiled for ALSA-%s (%d)\n", SND_LIB_VERSION_STR,
	    SND_LIB_VERSION);

    if ((err = snd_card_next(&cards)) < 0 || cards < 0)
    {
	printf("alsa-init: no soundcards found: %s\n", snd_strerror(err));
	return(0);
    }

    ao_format = format;
    ao_channels = channels - 1;
    ao_samplerate = rate_hz;
    ao_bps = ao_samplerate*(ao_channels+1);
    ao_outburst = OUTBURST;
    ao_buffersize = 16384;

    memset(&alsa_format, 0, sizeof(alsa_format));
    switch (format)
    {
	case AFMT_S8:
	    alsa_format = SND_PCM_FORMAT_S8;
	    break;
	case AFMT_U8:
	    alsa_format = SND_PCM_FORMAT_U8;
	    break;
	case AFMT_U16_LE:
	    alsa_format = SND_PCM_FORMAT_U16_LE;
	    break;
	case AFMT_U16_BE:
	    alsa_format = SND_PCM_FORMAT_U16_BE;
	    break;
	case AFMT_S16_LE:
	    alsa_format = SND_PCM_FORMAT_S16_LE;
	    break;
	case AFMT_S16_BE:
	    alsa_format = SND_PCM_FORMAT_S16_BE;
	    break;
	default:
	    alsa_format = SND_PCM_FORMAT_MPEG;
	    break;
    }
    
    switch(alsa_format)
    {
	case SND_PCM_FORMAT_S16_LE:
	case SND_PCM_FORMAT_U16_LE:
	    ao_bps *= 2;
	    break;
	case -1:
	    printf("alsa-init: invalid format (%s) requested - output disabled\n",
		audio_out_format_name(format));
	    return(0);
    }

    if ((err = snd_pcm_info_malloc(&alsa_info)) < 0)
    {
	printf("alsa-init: memory allocation error: %s\n", snd_strerror(err));
	return(0);
    }

    if ((alsa_device = malloc(ALSA_DEVICE_SIZE)) == NULL)
    {
	printf("alsa-init: memory allocation error: %s\n", strerror(errno));
	return(0);
    }

    snprintf(alsa_device, ALSA_DEVICE_SIZE, "hw:%d,%d",
	snd_pcm_info_get_device(alsa_info),
	snd_pcm_info_get_subdevice(alsa_info));

    snd_pcm_info_free(alsa_info);

    printf("alsa-init: %d soundcard%s found, using: %s\n", cards+1,
	(cards >= 0) ? "" : "s", alsa_device);

    if ((err = snd_pcm_open(&alsa_handler, alsa_device, SND_PCM_STREAM_PLAYBACK,
	0)) < 0)
    {
	printf("alsa-init: playback open error: %s\n", snd_strerror(err));
	return(0);
    }

    snd_pcm_hw_params_malloc(&alsa_hwparams);
    snd_pcm_sw_params_malloc(&alsa_swparams);
    
    if ((err = snd_pcm_hw_params_any(alsa_handler, alsa_hwparams)) < 0)
    {
	printf("alsa-init: unable to get initial parameters: %s\n",
	    snd_strerror(err));
	return(0);
    }
    
    if ((err = snd_pcm_hw_params_set_access(alsa_handler, alsa_hwparams,
	SND_PCM_ACCESS_RW_INTERLEAVED)) < 0)
    {
	printf("alsa-init: unable to set access type: %s\n",
	    snd_strerror(err));
	return(0);
    }
    
    if ((err = snd_pcm_hw_params_set_format(alsa_handler, alsa_hwparams,
	alsa_format)) < 0)
    {
	printf("alsa-init: unable to set format: %s\n",
	    snd_strerror(err));
	return(0);
    }

    if ((err = snd_pcm_hw_params_set_channels(alsa_handler, alsa_hwparams,
	ao_channels)) < 0)
    {
	printf("alsa-init: unable to set channels: %s\n",
	    snd_strerror(err));
	return(0);
    }

    if ((err = snd_pcm_hw_params_set_rate(alsa_handler, alsa_hwparams,
	ao_samplerate, 0)) < 0)
    {
	printf("alsa-init: unable to set channels: %s\n",
	    snd_strerror(err));
	return(0);
    }

#ifdef buffersize
    if ((err = snd_pcm_hw_params_get_buffer_size(alsa_hwparams)) < 0)
    {
	printf("alsa-init: unable to get buffer size: %s\n",
	    snd_strerror(err));
	return(0);
    } else
	ao_buffersize = err;
#endif

#ifdef buffertime
    {
	int alsa_buffer_time = 60;

	if ((err = snd_pcm_hw_params_set_buffer_time_near(alsa_handler, alsa_hwparams,
	    alsa_buffer_time, 0)) < 0)
	{
	    printf("alsa-init: unable to set buffer time near: %s\n",
		snd_strerror(err));
	    return(0);
	} else
	    alsa_buffer_time = err;

	if ((err = snd_pcm_hw_params_set_period_time_near(alsa_handler, alsa_hwparams,
	    alsa_buffer_time/ao_bps, 0)) < 0)
	{
	    printf("alsa-init: unable to set period time: %s\n",
		snd_strerror(err));
	    return(0);
	}
    }
#endif

    if ((err = snd_pcm_hw_params(alsa_handler, alsa_hwparams)) < 0)
    {
	printf("alsa-init: unable to set parameters: %s\n",
	    snd_strerror(err));
	return(0);
    }
    
    if ((err = snd_pcm_sw_params_current(alsa_handler, alsa_swparams)) < 0)
    {
	printf("alsa-init: unable to get parameters: %s\n",
	    snd_strerror(err));
	return(0);
    }    

    if ((err = snd_pcm_sw_params_set_start_mode(alsa_handler, alsa_swparams,
	SND_PCM_START_DATA)) < 0)
    {
	printf("alsa-init: unable to set start mode: %s\n",
	    snd_strerror(err));
	return(0);
    }

    if ((err = snd_pcm_sw_params(alsa_handler, alsa_swparams)) < 0)
    {
	printf("alsa-init: unable to set parameters: %s\n",
	    snd_strerror(err));
	return(0);
    }

    snd_pcm_sw_params_default(alsa_handler, alsa_swparams);

    if ((err = snd_pcm_prepare(alsa_handler)) < 0)
    {
	printf("alsa-init: pcm prepare error: %s\n", snd_strerror(err));
	return(0);
    }

    if ((err = snd_pcm_start(alsa_handler)) < 0)
    {
	printf("alsa-init: pcm start error: %s\n", snd_strerror(err));
	if (err != -EPIPE)
	    return(0);
	if ((err = snd_pcm_start(alsa_handler)) < 0)
	{
	    printf("alsa-init: pcm start error: %s\n", snd_strerror(err));
		return(0);
	}
    }

    printf("AUDIO: %d Hz/%d channels/%d bps/%d bytes buffer/%s\n",
	ao_samplerate, ao_channels+1, ao_bps, ao_buffersize,
	snd_pcm_format_description(alsa_format));
    return(1);
}

/* close audio device */
static void uninit()
{
    int err;

    if (alsa_device != NULL)
	free(alsa_device);

    snd_pcm_hw_params_free(alsa_hwparams);
    snd_pcm_sw_params_free(alsa_swparams);

    if ((err = snd_pcm_drain(alsa_handler)) < 0)
    {
	printf("alsa-uninit: pcm drain error: %s\n", snd_strerror(err));
	return;
    }

    if ((err = snd_pcm_reset(alsa_handler)) < 0)
    {
	printf("alsa-uninit: pcm reset error: %s\n", snd_strerror(err));
	return;
    }

    if ((err = snd_pcm_close(alsa_handler)) < 0)
    {
	printf("alsa-uninit: pcm close error: %s\n", snd_strerror(err));
	return;
    }
}

static void audio_pause()
{
    int err;

    if ((err = snd_pcm_drain(alsa_handler)) < 0)
    {
	printf("alsa-pause: pcm drain error: %s\n", snd_strerror(err));
	return;
    }

    if ((err = snd_pcm_reset(alsa_handler)) < 0)
    {
	printf("alsa-pause: pcm reset error: %s\n", snd_strerror(err));
	return;
    }
}

static void audio_resume()
{
    int err;

    if ((err = snd_pcm_prepare(alsa_handler)) < 0)
    {
	printf("alsa-resume: pcm prepare error: %s\n", snd_strerror(err));
	return;
    }

    if ((err = snd_pcm_start(alsa_handler)) < 0)
    {
	printf("alsa-resume: pcm start error: %s\n", snd_strerror(err));
	return;
    }
}

/* stop playing and empty buffers (for seeking/pause) */
static void reset()
{
    int err;

    if ((err = snd_pcm_drain(alsa_handler)) < 0)
    {
	printf("alsa-reset: pcm drain error: %s\n", snd_strerror(err));
	return;
    }

    if ((err = snd_pcm_reset(alsa_handler)) < 0)
    {
	printf("alsa-reset: pcm reset error: %s\n", snd_strerror(err));
	return;
    }

    if ((err = snd_pcm_prepare(alsa_handler)) < 0)
    {
	printf("alsa-reset: pcm prepare error: %s\n", snd_strerror(err));
	return;
    }

    if ((err = snd_pcm_start(alsa_handler)) < 0)
    {
	printf("alsa-reset: pcm start error: %s\n", snd_strerror(err));
	return;
    }
}

/*
    plays 'len' bytes of 'data'
    returns: number of bytes played
*/
static int play(void* data, int len, int flags)
{
    if ((len = snd_pcm_writei(alsa_handler, data, len)) != len)
    {
	if (len == -EPIPE) /* underrun? */
	{
	    printf("alsa-play: alsa underrun, resetting stream\n");
	    if ((len = snd_pcm_prepare(alsa_handler)) < 0)
	    {
		printf("alsa-play: playback prepare error: %s\n", snd_strerror(len));
		return(0);
	    }
	    if ((len = snd_pcm_writei(alsa_handler, data, len)) != len)
	    {
		printf("alsa-play: write error after reset: %s - giving up\n",
		    snd_strerror(len));
		return(0);
	    }
	    return(len); /* 2nd write was ok */
	}
	printf("alsa-play: output error: %s\n", snd_strerror(len));
    }
    return(len);
}

/* how many byes are free in the buffer */
static int get_space()
{
    snd_pcm_status_t *status;
    int ret;
    
    if ((ret = snd_pcm_status_malloc(&status)) < 0)
    {
	printf("alsa-space: memory allocation error: %s\n", snd_strerror(ret));
	return(0);
    }
    
    if ((ret = snd_pcm_status(alsa_handler, status)) < 0)
    {
	printf("alsa-space: cannot get pcm status: %s\n", snd_strerror(ret));
	return(0);
    }
    
    switch(snd_pcm_status_get_state(status))
    {
	case SND_PCM_STATE_OPEN:
	case SND_PCM_STATE_PREPARED:
	case SND_PCM_STATE_RUNNING:
	    ret = snd_pcm_status_get_avail(status) * ao_bps;
	    break;
	default:
	    ret = 0;
    }
    
    snd_pcm_status_free(status);
    return(ret);
}

/* how many unplayed bytes are in the buffer */
static int get_delay()
{
    snd_pcm_status_t *status;
    int ret;
    
    if ((ret = snd_pcm_status_malloc(&status)) < 0)
    {
	printf("alsa-delay: memory allocation error: %s\n", snd_strerror(ret));
	return(0);
    }
    
    if ((ret = snd_pcm_status(alsa_handler, status)) < 0)
    {
	printf("alsa-delay: cannot get pcm status: %s\n", snd_strerror(ret));
	return(0);
    }
    
    switch(snd_pcm_status_get_state(status))
    {
	case SND_PCM_STATE_OPEN:
	case SND_PCM_STATE_PREPARED:
	case SND_PCM_STATE_RUNNING:
	    ret = snd_pcm_status_get_delay(status) * ao_bps;
	    break;
	default:
	    ret = 0;
    }
    
    snd_pcm_status_free(status);
    return(ret);
}

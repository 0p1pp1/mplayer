#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

#include "config.h"

#if defined(USE_TV) && (defined(HAVE_TV_V4L) || defined(HAVE_TV_V4L2)) && defined(HAVE_ALSA1X)

#include <alsa/asoundlib.h>
#include "audio_in.h"
#include "mp_msg.h"

int ai_alsa_setup(audio_in_t *ai)
{
    snd_pcm_hw_params_t *params;
    snd_pcm_sw_params_t *swparams;
    snd_pcm_uframes_t buffer_size, period_size;
    int err;
    int dir;
    unsigned int rate;

    snd_pcm_hw_params_alloca(&params);
    snd_pcm_sw_params_alloca(&swparams);

    err = snd_pcm_hw_params_any(ai->alsa.handle, params);
    if (err < 0) {
	mp_msg(MSGT_TV, MSGL_ERR, "Broken configuration for this PCM: no configurations available\n");
	return -1;
    }

    err = snd_pcm_hw_params_set_access(ai->alsa.handle, params,
				       SND_PCM_ACCESS_RW_INTERLEAVED);
    if (err < 0) {
	mp_msg(MSGT_TV, MSGL_ERR, "Access type not available\n");
	return -1;
    }

    err = snd_pcm_hw_params_set_format(ai->alsa.handle, params, SND_PCM_FORMAT_S16_LE);
    if (err < 0) {
	mp_msg(MSGT_TV, MSGL_ERR, "Sample format not available\n");
	return -1;
    }

    err = snd_pcm_hw_params_set_channels(ai->alsa.handle, params, ai->req_channels);
    if (err < 0) {
	snd_pcm_hw_params_get_channels(params, &ai->channels);
	mp_msg(MSGT_TV, MSGL_ERR, "Channel count not available - reverting to default: %d\n",
	       ai->channels);
    } else {
	ai->channels = ai->req_channels;
    }

    dir = 0;
    rate = ai->req_samplerate;
    err = snd_pcm_hw_params_set_rate_near(ai->alsa.handle, params, &rate, &dir);
    if (err < 0) {
	mp_msg(MSGT_TV, MSGL_ERR, "Cannot set samplerate\n");
    }
    ai->samplerate = rate;

    dir = 0;
    ai->alsa.buffer_time = 1000000;
    err = snd_pcm_hw_params_set_buffer_time_near(ai->alsa.handle, params,
						 &ai->alsa.buffer_time, &dir);
    if (err < 0) {
	mp_msg(MSGT_TV, MSGL_ERR, "Cannot set buffer time\n");
    }

    dir = 0;
    ai->alsa.period_time = ai->alsa.buffer_time / 4;
    err = snd_pcm_hw_params_set_period_time_near(ai->alsa.handle, params,
						 &ai->alsa.period_time, &dir);
    if (err < 0) {
	mp_msg(MSGT_TV, MSGL_ERR, "Cannot set period time\n");
    }

    err = snd_pcm_hw_params(ai->alsa.handle, params);
    if (err < 0) {
	mp_msg(MSGT_TV, MSGL_ERR, "Unable to install hw params: %s\n", snd_strerror(err));
	snd_pcm_hw_params_dump(params, ai->alsa.log);
	return -1;
    }

    dir = -1;
    snd_pcm_hw_params_get_period_size(params, &period_size, &dir);
    snd_pcm_hw_params_get_buffer_size(params, &buffer_size);
    ai->alsa.chunk_size = period_size;
    if (period_size == buffer_size) {
	mp_msg(MSGT_TV, MSGL_ERR, "Can't use period equal to buffer size (%u == %lu)\n", ai->alsa.chunk_size, (long)buffer_size);
	return -1;
    }

    snd_pcm_sw_params_current(ai->alsa.handle, swparams);
    err = snd_pcm_sw_params_set_sleep_min(ai->alsa.handle, swparams,0);
    assert(err >= 0);
    err = snd_pcm_sw_params_set_avail_min(ai->alsa.handle, swparams, ai->alsa.chunk_size);
    assert(err >= 0);

    err = snd_pcm_sw_params_set_start_threshold(ai->alsa.handle, swparams, 0);
    assert(err >= 0);
    err = snd_pcm_sw_params_set_stop_threshold(ai->alsa.handle, swparams, buffer_size);
    assert(err >= 0);

    assert(err >= 0);
    if (snd_pcm_sw_params(ai->alsa.handle, swparams) < 0) {
	mp_msg(MSGT_TV, MSGL_ERR, "unable to install sw params:\n");
	snd_pcm_sw_params_dump(swparams, ai->alsa.log);
	return -1;
    }

    if (mp_msg_test(MSGT_TV, MSGL_V)) {
	snd_pcm_dump(ai->alsa.handle, ai->alsa.log);
    }

    ai->alsa.bits_per_sample = snd_pcm_format_physical_width(SND_PCM_FORMAT_S16_LE);
    ai->alsa.bits_per_frame = ai->alsa.bits_per_sample * ai->channels;
    ai->blocksize = ai->alsa.chunk_size * ai->alsa.bits_per_frame / 8;
    ai->samplesize = ai->alsa.bits_per_sample;
    ai->bytes_per_sample = ai->alsa.bits_per_sample/8;

    return 0;
}

int ai_alsa_init(audio_in_t *ai)
{
    int err;
    
    err = snd_pcm_open(&ai->alsa.handle, ai->alsa.device, SND_PCM_STREAM_CAPTURE, 0);
    if (err < 0) {
	mp_msg(MSGT_TV, MSGL_ERR, "Error opening audio: %s\n", snd_strerror(err));
	return -1;
    }
    
    err = snd_output_stdio_attach(&ai->alsa.log, stderr, 0);
    
    if (err < 0) {
	return -1;
    }
    
    err = ai_alsa_setup(ai);

    return err;
}

#ifndef timersub
#define	timersub(a, b, result) \
do { \
	(result)->tv_sec = (a)->tv_sec - (b)->tv_sec; \
	(result)->tv_usec = (a)->tv_usec - (b)->tv_usec; \
	if ((result)->tv_usec < 0) { \
		--(result)->tv_sec; \
		(result)->tv_usec += 1000000; \
	} \
} while (0)
#endif

int ai_alsa_xrun(audio_in_t *ai)
{
    snd_pcm_status_t *status;
    int res;
	
    snd_pcm_status_alloca(&status);
    if ((res = snd_pcm_status(ai->alsa.handle, status))<0) {
	mp_msg(MSGT_TV, MSGL_ERR, "ALSA status error: %s", snd_strerror(res));
	return -1;
    }
    if (snd_pcm_status_get_state(status) == SND_PCM_STATE_XRUN) {
	struct timeval now, diff, tstamp;
	gettimeofday(&now, 0);
	snd_pcm_status_get_trigger_tstamp(status, &tstamp);
	timersub(&now, &tstamp, &diff);
	mp_msg(MSGT_TV, MSGL_ERR, "ALSA xrun!!! (at least %.3f ms long)\n",
	       diff.tv_sec * 1000 + diff.tv_usec / 1000.0);
	if (mp_msg_test(MSGT_TV, MSGL_V)) {
	    mp_msg(MSGT_TV, MSGL_ERR, "ALSA Status:\n");
	    snd_pcm_status_dump(status, ai->alsa.log);
	}
	if ((res = snd_pcm_prepare(ai->alsa.handle))<0) {
	    mp_msg(MSGT_TV, MSGL_ERR, "ALSA xrun: prepare error: %s", snd_strerror(res));
	    return -1;
	}
	return 0;		/* ok, data should be accepted again */
    }
    mp_msg(MSGT_TV, MSGL_ERR, "ALSA read/write error");
    return -1;
}

#endif /* HAVE_ALSA1X */

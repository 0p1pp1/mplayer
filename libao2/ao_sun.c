#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/audioio.h>
#ifdef	AUDIO_SWFEATURE_MIXER	/* solaris8 or newer? */
# define HAVE_SYS_MIXER_H 1
#endif
#if	HAVE_SYS_MIXER_H
# include <sys/mixer.h>
#endif
#ifdef	__svr4__
#include <stropts.h>
#endif

#include "../config.h"
#include "../mixer.h"

#include "audio_out.h"
#include "audio_out_internal.h"
#include "afmt.h"

static ao_info_t info = 
{
    "Sun audio output",
    "sun",
    "jk@tools.de",
    ""
};

LIBAO_EXTERN(sun)


/* These defines are missing on NetBSD */
#ifndef	AUDIO_PRECISION_8
#define AUDIO_PRECISION_8	8
#define AUDIO_PRECISION_16	16
#endif
#ifndef	AUDIO_CHANNELS_MONO
#define	AUDIO_CHANNELS_MONO	1
#define	AUDIO_CHANNELS_STEREO	2
#endif


static char *sun_mixer_device = NULL;
static char *audio_dev = NULL;
static int queued_bursts = 0;
static int queued_samples = 0;
static int bytes_per_sample = 0;
static int byte_per_sec = 0;
static int convert_u8_s8;
static int audio_fd = -1;
static enum {
    RTSC_UNKNOWN = 0,
    RTSC_ENABLED,
    RTSC_DISABLED
} enable_sample_timing;

extern int verbose;


// convert an OSS audio format specification into a sun audio encoding
static int oss2sunfmt(int oss_format)
{
    switch (oss_format){
    case AFMT_MU_LAW:
	return AUDIO_ENCODING_ULAW;
    case AFMT_A_LAW:
	return AUDIO_ENCODING_ALAW;
    case AFMT_S16_BE:
    case AFMT_S16_LE:
	return AUDIO_ENCODING_LINEAR;
#ifdef	AUDIO_ENCODING_LINEAR8	// Missing on SunOS 5.5.1...
    case AFMT_U8:
	return AUDIO_ENCODING_LINEAR8;
#endif
#ifdef	AUDIO_ENCODING_DVI	// Missing on NetBSD...
    case AFMT_IMA_ADPCM:
	return AUDIO_ENCODING_DVI;
#endif
    default:
	return AUDIO_ENCODING_NONE;
  }
}

// try to figure out, if the soundcard driver provides usable (precise)
// sample counter information
static int realtime_samplecounter_available(char *dev)
{
    int fd = -1;
    audio_info_t info;
    int rtsc_ok = RTSC_DISABLED;
    int len;
    void *silence = NULL;
    struct timeval start, end;
    struct timespec delay;
    int usec_delay;
    unsigned last_samplecnt;
    unsigned increment;
    unsigned min_increment;

    len = 44100 * 4 / 4;    /* amount of data for 0.25sec of 44.1khz, stereo,
			     * 16bit.  44kbyte can be sent to all supported
			     * sun audio devices without blocking in the
			     * "write" below.
			     */
    silence = calloc(1, len);
    if (silence == NULL)
	goto error;
    
    if ((fd = open(dev, O_WRONLY)) < 0)
	goto error;

    AUDIO_INITINFO(&info);
    info.play.sample_rate = 44100;
    info.play.channels = AUDIO_CHANNELS_STEREO;
    info.play.precision = AUDIO_PRECISION_16;
    info.play.encoding = AUDIO_ENCODING_LINEAR;
    info.play.samples = 0;
    if (ioctl(fd, AUDIO_SETINFO, &info)) {
	if (verbose)
	    printf("rtsc: SETINFO failed\n");
	goto error;
    }
    
    if (write(fd, silence, len) != len) {
	if (verbose)
	    printf("rtsc: write failed");
	goto error;
    }

    if (ioctl(fd, AUDIO_GETINFO, &info)) {
	if (verbose)
	    perror("rtsc: GETINFO1");
	goto error;
    }

    last_samplecnt = info.play.samples;
    min_increment = ~0;

    gettimeofday(&start, NULL);
    for (;;) {
	delay.tv_sec = 0;
	delay.tv_nsec = 10000000;
	nanosleep(&delay, NULL);
	gettimeofday(&end, NULL);
	usec_delay = (end.tv_sec - start.tv_sec) * 1000000
	    + end.tv_usec - start.tv_usec;

	// stop monitoring sample counter after 0.2 seconds
	if (usec_delay > 200000)
	    break;

	if (ioctl(fd, AUDIO_GETINFO, &info)) {
	    if (verbose)
		perror("rtsc: GETINFO2 failed");
	    goto error;
	}
	if (info.play.samples < last_samplecnt) {
	    if (verbose)
		printf("rtsc: %d > %d?\n", last_samplecnt, info.play.samples);
	    goto error;
	}

	if ((increment = info.play.samples - last_samplecnt) > 0) {
	    if (verbose)
		printf("ao_sun: sample counter increment: %d\n", increment);
	    if (increment < min_increment) {
		min_increment = increment;
		if (min_increment < 2000)
		    break;	// looks good
	    }
	}
	last_samplecnt = info.play.samples;
    }

    /*
     * For 44.1kkz, stereo, 16-bit format we would send sound data in 16kbytes
     * chunks (== 4096 samples) to the audio device.  If we see a minimum
     * sample counter increment from the soundcard driver of less than
     * 2000 samples,  we assume that the driver provides a useable realtime
     * sample counter in the AUDIO_INFO play.samples field.  Timing based
     * on sample counts should be much more accurate than counting whole 
     * 16kbyte chunks.
     */
    if (min_increment < 2000)
	rtsc_ok = RTSC_ENABLED;

    if (verbose)
	printf("ao_sun: minimum sample counter increment per 10msec interval: %d\n"
	       "\t%susing sample counter based timing code\n",
	       min_increment, rtsc_ok == RTSC_ENABLED ? "" : "not ");
    

error:
    if (silence != NULL) free(silence);
    if (fd >= 0) {
#ifdef	__svr4__
	// remove the 0 bytes from the above measurement from the
	// audio driver's STREAMS queue
	ioctl(fd, I_FLUSH, FLUSHW);
#endif
	//ioctl(fd, AUDIO_DRAIN, 0);
	close(fd);
    }

    return rtsc_ok;
}


// match the requested sample rate |sample_rate| against the
// sample rates supported by the audio device |dev|.  Return
// a supported sample rate,  if that sample rate is close to
// (< 1% difference) the requested rate; return 0 otherwise.

#define	MAX_RATE_ERR	1

static unsigned
find_close_samplerate_match(int dev, unsigned sample_rate)
{
#if	HAVE_SYS_MIXER_H
    am_sample_rates_t *sr;
    unsigned i, num, err, best_err, best_rate;

    for (num = 16; num < 1024; num *= 2) {
	sr = malloc(AUDIO_MIXER_SAMP_RATES_STRUCT_SIZE(num));
	if (!sr)
	    return 0;
	sr->type = AUDIO_PLAY;
	sr->flags = 0;
	sr->num_samp_rates = num;
	if (ioctl(dev, AUDIO_MIXER_GET_SAMPLE_RATES, sr)) {
	    free(sr);
	    return 0;
	}
	if (sr->num_samp_rates <= num)
	    break;
	free(sr);
    }

    if (sr->flags & MIXER_SR_LIMITS) {
	/*
	 * HW can playback any rate between 
	 * sr->samp_rates[0] .. sr->samp_rates[1]
	 */
	free(sr);
	return 0;
    } else {
	/* HW supports fixed sample rates only */

	best_err = 65535;
	best_rate = 0;

	for (i = 0; i < sr->num_samp_rates; i++) {
	    err = abs(sr->samp_rates[i] - sample_rate);
	    if (err == 0) {
		/*
		 * exact supported sample rate match, no need to
		 * retry something else
		 */
		best_rate = 0;
		break;
	    }
	    if (err < best_err) {
		best_err = err;
		best_rate = sr->samp_rates[i];
	    }
	}

	free(sr);

	if (best_rate > 0 && (100/MAX_RATE_ERR)*best_err < sample_rate) {
	    /* found a supported sample rate with <1% error? */
	    return best_rate;
	}
	return 0;
    }
#else	/* old audioio driver, cannot return list of supported rates */
    /* XXX: hardcoded sample rates */
    unsigned i, err;
    unsigned audiocs_rates[] = {
	5510, 6620, 8000, 9600, 11025, 16000, 18900, 22050,
	27420, 32000, 33075, 37800, 44100, 48000, 0
    };

    for (i = 0; audiocs_rates[i]; i++) {
	err = abs(audiocs_rates[i] - sample_rate);
	if (err == 0) {
	    /* 
	     * exact supported sample rate match, no need to
	     * retry something elise
	     */
	    return 0;
	}
	if ((100/MAX_RATE_ERR)*err < audiocs_rates[i]) {
	    /* <1% error? */
	    return audiocs_rates[i];
	}
    }

    return 0;
#endif
}


// return the highest sample rate supported by audio device |dev|.
static unsigned
find_highest_samplerate(int dev)
{
#if	HAVE_SYS_MIXER_H
    am_sample_rates_t *sr;
    unsigned i, num, max_rate;

    for (num = 16; num < 1024; num *= 2) {
	sr = malloc(AUDIO_MIXER_SAMP_RATES_STRUCT_SIZE(num));
	if (!sr)
	    return 0;
	sr->type = AUDIO_PLAY;
	sr->flags = 0;
	sr->num_samp_rates = num;
	if (ioctl(dev, AUDIO_MIXER_GET_SAMPLE_RATES, sr)) {
	    free(sr);
	    return 0;
	}
	if (sr->num_samp_rates <= num)
	    break;
	free(sr);
    }

    if (sr->flags & MIXER_SR_LIMITS) {
	/*
	 * HW can playback any rate between 
	 * sr->samp_rates[0] .. sr->samp_rates[1]
	 */
	max_rate = sr->samp_rates[1];
    } else {
	/* HW supports fixed sample rates only */
	max_rate = 0;
	for (i = 0; i < sr->num_samp_rates; i++) {
	    if (sr->samp_rates[i] > max_rate)
		max_rate = sr->samp_rates[i];
	}
    }
    free(sr);
    return max_rate;

#else	/* old audioio driver, cannot return list of supported rates */
    return 44100;	/* should be supported even on old ISA SB cards */
#endif
}


static void setup_device_paths()
{
    if (audio_dev == NULL) {
	if ((audio_dev = getenv("AUDIODEV")) == NULL)
	    audio_dev = "/dev/audio";
    }

    if (sun_mixer_device == NULL) {
	if ((sun_mixer_device = mixer_device) == NULL || !sun_mixer_device[0]) {
	    sun_mixer_device = malloc(strlen(audio_dev) + 4);
	    strcpy(sun_mixer_device, audio_dev);
	    strcat(sun_mixer_device, "ctl");
	}
    }

    if (ao_subdevice) audio_dev = ao_subdevice;
}

// to set/get/query special features/parameters
static int control(int cmd,int arg){
    switch(cmd){
    case AOCONTROL_SET_DEVICE:
	audio_dev=(char*)arg;
	return CONTROL_OK;
    case AOCONTROL_QUERY_FORMAT:
	return CONTROL_TRUE;
    case AOCONTROL_GET_VOLUME:
    {
        int fd;

	if ( !sun_mixer_device )    /* control function is used before init? */
	    setup_device_paths();

	fd=open( sun_mixer_device,O_RDONLY );
	if ( fd != -1 )
	{
	    ao_control_vol_t *vol = (ao_control_vol_t *)arg;
	    float volume;
	    struct audio_info info;
	    ioctl( fd,AUDIO_GETINFO,&info);
	    volume = info.play.gain * 100. / AUDIO_MAX_GAIN;
	    if ( info.play.balance == AUDIO_MID_BALANCE ) {
		vol->right = vol->left = volume;
	    } else if ( info.play.balance < AUDIO_MID_BALANCE ) {
		vol->left  = volume;
		vol->right = volume * info.play.balance / AUDIO_MID_BALANCE;
	    } else {
		vol->left  = volume * (AUDIO_RIGHT_BALANCE-info.play.balance)
							/ AUDIO_MID_BALANCE;
		vol->right = volume;
	    }
	    close( fd );
	    return CONTROL_OK;
	}	
	return CONTROL_ERROR;
    }
    case AOCONTROL_SET_VOLUME:
    {
	ao_control_vol_t *vol = (ao_control_vol_t *)arg;
        int fd;

	if ( !sun_mixer_device )    /* control function is used before init? */
	    setup_device_paths();

	fd=open( sun_mixer_device,O_RDONLY );
	if ( fd != -1 )
	{
	    struct audio_info info;
	    float volume;
	    AUDIO_INITINFO(&info);
	    volume = vol->right > vol->left ? vol->right : vol->left;
	    if ( volume != 0 ) {
		info.play.gain = volume * AUDIO_MAX_GAIN / 100;
		if ( vol->right == vol->left )
		    info.play.balance = AUDIO_MID_BALANCE;
		else
		    info.play.balance = (vol->right - vol->left + volume) * AUDIO_RIGHT_BALANCE / (2*volume);
	    }
#if !defined (__OpenBSD__) && !defined (__NetBSD__)
	    info.output_muted = (volume == 0);
#endif
	    ioctl( fd,AUDIO_SETINFO,&info );
	    close( fd );
	    return CONTROL_OK;
	}	
	return CONTROL_ERROR;
    }
    }
    return CONTROL_UNKNOWN;
}

// open & setup audio device
// return: 1=success 0=fail
static int init(int rate,int channels,int format,int flags){

    audio_info_t info;
    int pass;
    int ok;

    setup_device_paths();

    if (enable_sample_timing == RTSC_UNKNOWN
	&& !getenv("AO_SUN_DISABLE_SAMPLE_TIMING")) {
	enable_sample_timing = realtime_samplecounter_available(audio_dev);
    }

//    printf("ao2: %d Hz  %d chans  %s [0x%X]\n",
//	   rate,channels,audio_out_format_name(format),format);

    audio_fd=open(audio_dev, O_WRONLY);
    if(audio_fd<0){
	printf("Can't open audio device %s, %s  -> nosound\n", audio_dev, strerror(errno));
	return 0;
    }

    ioctl(audio_fd, AUDIO_DRAIN, 0);

    for (ok = pass = 0; pass <= 5; pass++) { /* pass 6&7 not useful */

	AUDIO_INITINFO(&info);
	info.play.encoding = oss2sunfmt(ao_data.format = format);
	info.play.precision =
	    (format==AFMT_S16_LE || format==AFMT_S16_BE
	     ? AUDIO_PRECISION_16
	     : AUDIO_PRECISION_8);
	info.play.channels = ao_data.channels = channels;
	info.play.sample_rate = ao_data.samplerate = rate;

	convert_u8_s8 = 0;

	if (pass & 1) {
	    /*
	     * on some sun audio drivers, 8-bit unsigned LINEAR8 encoding is 
	     * not supported, but 8-bit signed encoding is.
	     *
	     * Try S8, and if it works, use our own U8->S8 conversion before
	     * sending the samples to the sound driver.
	     */
	    if (info.play.encoding != AUDIO_ENCODING_LINEAR8)
		continue;
	    info.play.encoding = AUDIO_ENCODING_LINEAR;
	    convert_u8_s8 = 1;
	}

	if (pass & 2) {
	    /*
	     * on some sun audio drivers, only certain fixed sample rates are
	     * supported.
	     *
	     * In case the requested sample rate is very close to one of the
	     * supported rates,  use the fixed supported rate instead.
	     */
	    if (!(info.play.sample_rate =
		  find_close_samplerate_match(audio_fd, rate))) 
	      continue;

	    /*
	     * I'm not returning the correct sample rate in
	     * |ao_data.samplerate|, to avoid software resampling.
	     *
	     * ao_data.samplerate = info.play.sample_rate;
	     */
	}

	if (pass & 4) {
	    /* like "pass & 2", but use the highest supported sample rate */
	    if (!(info.play.sample_rate
		  = ao_data.samplerate
		  = find_highest_samplerate(audio_fd)))
		continue;
	}

	ok = ioctl(audio_fd, AUDIO_SETINFO, &info) >= 0;
	if (ok) {
	    /* audio format accepted by audio driver */
	    break;
	}

	/*
	 * format not supported?
	 * retry with different encoding and/or sample rate
	 */
    }

    if (!ok) {
	printf("audio_setup: your card doesn't support %d channel, %s, %d Hz samplerate\n",
	       channels, audio_out_format_name(format), rate);
	return 0;
    }

    bytes_per_sample = channels * info.play.precision / 8;
    byte_per_sec = bytes_per_sample * rate;
    ao_data.outburst = byte_per_sec > 100000 ? 16384 : 8192;

#ifdef	__not_used__
    /*
     * hmm, ao_data.buffersize is currently not used in this driver, do there's
     * no need to measure it
     */
    if(ao_data.buffersize==-1){
	// Measuring buffer size:
	void* data;
	ao_data.buffersize=0;
#ifdef HAVE_AUDIO_SELECT
	data = malloc(ao_data.outburst);
	memset(data, format==AFMT_U8 ? 0x80 : 0, ao_data.outburst);
	while(ao_data.buffersize<0x40000){
	    fd_set rfds;
	    struct timeval tv;
	    FD_ZERO(&rfds); FD_SET(audio_fd,&rfds);
	    tv.tv_sec=0; tv.tv_usec = 0;
	    if(!select(audio_fd+1, NULL, &rfds, NULL, &tv)) break;
	    write(audio_fd,data,ao_data.outburst);
	    ao_data.buffersize+=ao_data.outburst;
	}
	free(data);
	if(ao_data.buffersize==0){
	    printf("\n   ***  Your audio driver DOES NOT support select()  ***\n");
	    printf("Recompile mplayer with #undef HAVE_AUDIO_SELECT in config.h !\n\n");
	    return 0;
	}
#ifdef	__svr4__
	// remove the 0 bytes from the above ao_data.buffersize measurement from the
	// audio driver's STREAMS queue
	ioctl(audio_fd, I_FLUSH, FLUSHW);
#endif
	ioctl(audio_fd, AUDIO_DRAIN, 0);
#endif
    }
#endif	/* __not_used__ */

    AUDIO_INITINFO(&info);
    info.play.samples = 0;
    info.play.eof = 0;
    info.play.error = 0;
    ioctl (audio_fd, AUDIO_SETINFO, &info);

    queued_bursts = 0;
    queued_samples = 0;

    return 1;
}

// close audio device
static void uninit(){
#ifdef	__svr4__
    // throw away buffered data in the audio driver's STREAMS queue
    ioctl(audio_fd, I_FLUSH, FLUSHW);
#endif
    close(audio_fd);
}

// stop playing and empty buffers (for seeking/pause)
static void reset(){
    audio_info_t info;

    uninit();
    audio_fd=open(audio_dev, O_WRONLY);
    if(audio_fd<0){
	printf("\nFatal error: *** CANNOT RE-OPEN / RESET AUDIO DEVICE (%s) ***\n", strerror(errno));
	return;
    }

    ioctl(audio_fd, AUDIO_DRAIN, 0);

    AUDIO_INITINFO(&info);
    info.play.encoding = oss2sunfmt(ao_data.format);
    info.play.precision =
	(ao_data.format==AFMT_S16_LE || ao_data.format==AFMT_S16_BE 
	 ? AUDIO_PRECISION_16
	 : AUDIO_PRECISION_8);
    info.play.channels = ao_data.channels;
    info.play.sample_rate = ao_data.samplerate;
    info.play.samples = 0;
    info.play.eof = 0;
    info.play.error = 0;
    ioctl (audio_fd, AUDIO_SETINFO, &info);
    queued_bursts = 0;
    queued_samples = 0;
}

// stop playing, keep buffers (for pause)
static void audio_pause()
{
    struct audio_info info;
    AUDIO_INITINFO(&info);
    info.play.pause = 1;
    ioctl(audio_fd, AUDIO_SETINFO, &info);
}

// resume playing, after audio_pause()
static void audio_resume()
{
    struct audio_info info;
    AUDIO_INITINFO(&info);
    info.play.pause = 0;
    ioctl(audio_fd, AUDIO_SETINFO, &info);
}


// return: how many bytes can be played without blocking
static int get_space(){
    audio_info_t info;

    // check buffer
#ifdef HAVE_AUDIO_SELECT
    {
	fd_set rfds;
	struct timeval tv;
	FD_ZERO(&rfds);
	FD_SET(audio_fd, &rfds);
	tv.tv_sec = 0;
	tv.tv_usec = 0;
	if(!select(audio_fd+1, NULL, &rfds, NULL, &tv)) return 0; // not block!
    }
#endif

#if !defined (__OpenBSD__) && !defined(__NetBSD__)
    ioctl(audio_fd, AUDIO_GETINFO, &info);
    if (queued_bursts - info.play.eof > 2)
	return 0;
#endif

#if defined(__NetBSD__) || defined(__OpenBSD__)
    ioctl(audio_fd, AUDIO_GETINFO, &info);
    return info.hiwat * info.blocksize - info.play.seek;
#else
    return ao_data.outburst;
#endif

}

// plays 'len' bytes of 'data'
// it should round it down to outburst*n
// return: number of bytes played
static int play(void* data,int len,int flags){
#if	WORDS_BIGENDIAN
    int native_endian = AFMT_S16_BE;
#else
    int native_endian = AFMT_S16_LE;
#endif

    if (len < ao_data.outburst) return 0;
    len /= ao_data.outburst;
    len *= ao_data.outburst;

    /* 16-bit format using the 'wrong' byteorder?  swap words */
    if ((ao_data.format == AFMT_S16_LE || ao_data.format == AFMT_S16_BE)
	&& ao_data.format != native_endian) {
	static void *swab_buf;
	static int swab_len;
	if (len > swab_len) {
	    if (swab_buf)
		swab_buf = realloc(swab_buf, len);
	    else
		swab_buf = malloc(len);
	    swab_len = len;
	    if (swab_buf == NULL) return 0;
	}
	swab(data, swab_buf, len);
	data = swab_buf;
    } else if (ao_data.format == AFMT_U8 && convert_u8_s8) {
	int i;
	unsigned char *p = data;

	for (i = 0, p = data; i < len; i++, p++)
	    *p ^= 0x80;
    }

    len = write(audio_fd, data, len);
    if(len > 0) {
	queued_samples += len / bytes_per_sample;
	if (write(audio_fd,data,0) < 0)
	    perror("ao_sun: send EOF audio record");
	else
	    queued_bursts ++;
    }
    return len;
}


// return: delay in seconds between first and last sample in buffer
static float get_delay(){
    audio_info_t info;
    ioctl(audio_fd, AUDIO_GETINFO, &info);
#if defined (__OpenBSD__) || defined(__NetBSD__)
    return (float) info.play.seek/ (float)byte_per_sec ;
#else
    if (info.play.samples && enable_sample_timing == RTSC_ENABLED)
	return (float)(queued_samples - info.play.samples) / (float)byte_per_sec;
    else
	return (float)((queued_bursts - info.play.eof) * ao_data.outburst) / (float)byte_per_sec;
#endif
}


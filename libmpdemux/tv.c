/*
 TV Interface for MPlayer
 
 (C) Alex Beregszaszi <alex@naxine.org>
 
 API idea based on libvo2

 Feb 19, 2002: Significant rewrites by Charles R. Henrich (henrich@msu.edu)
				to add support for audio, and bktr *BSD support.

*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/time.h>

#include "config.h"

int tv_param_on = 0;

#ifdef USE_TV
#include "mp_msg.h"
#include "help_mp.h"

#include "stream.h"
#include "demuxer.h"
#include "stheader.h"

#include "../libao2/afmt.h"
#include "../libao2/audio_out.h"
#include "../libvo/img_format.h"
#include "../libvo/fastmemcpy.h"

#include "tv.h"

#include "frequencies.h"

/* some default values */
int tv_param_audiorate = 44100;
int tv_param_noaudio = 0;
int tv_param_immediate = 0;
char *tv_param_freq = NULL;
char *tv_param_channel = NULL;
char *tv_param_norm = "pal";
char *tv_param_chanlist = "europe-east";
char *tv_param_device = NULL;
char *tv_param_driver = "dummy";
int tv_param_width = -1;
int tv_param_height = -1;
int tv_param_input = 0; /* used in v4l and bttv */
char *tv_param_outfmt = "yv12";
float tv_param_fps = -1.0;
char **tv_param_channels = NULL;
#ifdef HAVE_TV_V4L
int tv_param_amode = -1;
int tv_param_audio_id = 0;
int tv_param_volume = 60000;
int tv_param_bass = -1;
int tv_param_treble = -1;
int tv_param_balance = -1;
int tv_param_forcechan = -1;
int tv_param_force_audio = 0;
int tv_param_buffer_size = -1;
int tv_param_mjpeg = 0;
int tv_param_decimation = 2;
int tv_param_quality = 90;
#ifdef HAVE_ALSA9
int tv_param_alsa = 0;
#endif
char* tv_param_adevice = NULL;
#endif
int tv_param_brightness = 0;
int tv_param_contrast = 0;
int tv_param_hue = 0;
int tv_param_saturation = 0;

/* ================== DEMUX_TV ===================== */
/*
  Return value:
    0 = EOF(?) or no stream
    1 = successfully read a packet
*/
/* fill demux->video and demux->audio */

int demux_tv_fill_buffer(demuxer_t *demux, demux_stream_t *ds)
{
    tvi_handle_t *tvh=(tvi_handle_t*)(demux->priv);
    demux_packet_t* dp;
    unsigned int len=0;

    /* ================== ADD AUDIO PACKET =================== */

    if (ds==demux->audio && tv_param_noaudio == 0 && 
        tvh->functions->control(tvh->priv, 
                                TVI_CONTROL_IS_AUDIO, 0) == TVI_CONTROL_TRUE)
        {
        len = tvh->functions->get_audio_framesize(tvh->priv);

        dp=new_demux_packet(len);
        dp->pts=tvh->functions->grab_audio_frame(tvh->priv, dp->buffer,len);
        ds_add_packet(demux->audio,dp);
        }

    /* ================== ADD VIDEO PACKET =================== */

    if (ds==demux->video && tvh->functions->control(tvh->priv, 
                            TVI_CONTROL_IS_VIDEO, 0) == TVI_CONTROL_TRUE)
        {
		len = tvh->functions->get_video_framesize(tvh->priv);
       	dp=new_demux_packet(len);
  		dp->pts=tvh->functions->grab_video_frame(tvh->priv, dp->buffer, len);
   		ds_add_packet(demux->video,dp);
	 }

    return 1;
}

 /* forward declarations */
int tv_set_freq(tvi_handle_t *tvh, unsigned long freq);
int tv_get_freq(tvi_handle_t *tvh, unsigned long *freq);

static int open_tv(tvi_handle_t *tvh)
{
    int i;
    tvi_functions_t *funcs = tvh->functions;
    int picture_format = 0;

    if (funcs->control(tvh->priv, TVI_CONTROL_IS_VIDEO, 0) != TVI_CONTROL_TRUE)
    {
	mp_msg(MSGT_TV, MSGL_ERR, "Error: no video input present!\n");
	return 0;
    }

    if (!strcasecmp(tv_param_outfmt, "yv12"))
	picture_format = IMGFMT_YV12;
    else if (!strcasecmp(tv_param_outfmt, "i420"))
	picture_format = IMGFMT_I420;
    else if (!strcasecmp(tv_param_outfmt, "uyvy"))
	picture_format = IMGFMT_UYVY;
    else if (!strcasecmp(tv_param_outfmt, "yuy2"))
	picture_format = IMGFMT_YUY2;
    else if (!strcasecmp(tv_param_outfmt, "rgb32"))
	picture_format = IMGFMT_RGB32;
    else if (!strcasecmp(tv_param_outfmt, "rgb24"))
	picture_format = IMGFMT_RGB24;
    else if (!strcasecmp(tv_param_outfmt, "rgb16"))
	picture_format = IMGFMT_RGB16;
    else if (!strcasecmp(tv_param_outfmt, "rgb15"))
	picture_format = IMGFMT_RGB15;
    else
    {
	mp_msg(MSGT_TV, MSGL_ERR, "Unknown format given: %s\n", tv_param_outfmt);
	mp_msg(MSGT_TV, MSGL_V, "Using default: Planar YV12\n");
	picture_format = IMGFMT_YV12;
    }
    funcs->control(tvh->priv, TVI_CONTROL_VID_SET_FORMAT, &picture_format);

    /* set some params got from cmdline */
    funcs->control(tvh->priv, TVI_CONTROL_SPC_SET_INPUT, &tv_param_input);

    /* select video norm */
    if (!strcasecmp(tv_param_norm, "pal"))
	tvh->norm = TV_NORM_PAL;
    else if (!strcasecmp(tv_param_norm, "ntsc"))
	tvh->norm = TV_NORM_NTSC;
    else if (!strcasecmp(tv_param_norm, "secam"))
	tvh->norm = TV_NORM_SECAM;
    else if (!strcasecmp(tv_param_norm, "palnc"))
	tvh->norm = TV_NORM_PALNC;
    else if (!strcasecmp(tv_param_norm, "palm"))
	tvh->norm = TV_NORM_PALM;
    else if (!strcasecmp(tv_param_norm, "paln"))
	tvh->norm = TV_NORM_PALN;
    else if (!strcasecmp(tv_param_norm, "ntscjp"))
	tvh->norm = TV_NORM_NTSCJP;
    else {
	mp_msg(MSGT_TV, MSGL_V, "Bogus norm parameter, setting PAL.\n");
	tvh->norm = TV_NORM_PAL;
    }

    mp_msg(MSGT_TV, MSGL_V, "Selected norm: %s\n", tv_param_norm);
    if (funcs->control(tvh->priv, TVI_CONTROL_TUN_SET_NORM, &tvh->norm) != TVI_CONTROL_TRUE) {
	mp_msg(MSGT_TV, MSGL_ERR, "Error: cannot set norm!\n");
	return 0;
    }


#ifdef HAVE_TV_V4L
    if ( tv_param_mjpeg )
    {
      /* set width to expected value */
      if (tv_param_width == -1)
        {
          tv_param_width = 704/tv_param_decimation;
        }
      if (tv_param_height == -1)
        {
	  if ( tvh->norm != TV_NORM_NTSC )
            tv_param_height = 576/tv_param_decimation; 
	  else
            tv_param_height = 480/tv_param_decimation; 
        }
      mp_msg(MSGT_TV, MSGL_INFO, 
	       "  MJP: width %d height %d\n", tv_param_width, tv_param_height);
    }
#endif

    /* limits on w&h are norm-dependent -- JM */
    /* set width */
    if (tv_param_width != -1)
    {
	if (funcs->control(tvh->priv, TVI_CONTROL_VID_CHK_WIDTH, &tv_param_width) == TVI_CONTROL_TRUE)
	    funcs->control(tvh->priv, TVI_CONTROL_VID_SET_WIDTH, &tv_param_width);
	else
	{
	    mp_msg(MSGT_TV, MSGL_ERR, "Unable set requested width: %d\n", tv_param_width);
	    funcs->control(tvh->priv, TVI_CONTROL_VID_GET_WIDTH, &tv_param_width);
	}    
    }

    /* set height */
    if (tv_param_height != -1)
    {
	if (funcs->control(tvh->priv, TVI_CONTROL_VID_CHK_HEIGHT, &tv_param_height) == TVI_CONTROL_TRUE)
	    funcs->control(tvh->priv, TVI_CONTROL_VID_SET_HEIGHT, &tv_param_height);
	else
	{
	    mp_msg(MSGT_TV, MSGL_ERR, "Unable set requested height: %d\n", tv_param_height);
	    funcs->control(tvh->priv, TVI_CONTROL_VID_GET_HEIGHT, &tv_param_height);
	}    
    }

    if (funcs->control(tvh->priv, TVI_CONTROL_IS_TUNER, 0) != TVI_CONTROL_TRUE)
    {
	mp_msg(MSGT_TV, MSGL_WARN, "Selected input hasn't got a tuner!\n");	
	goto done;
    }

    /* select channel list */
    for (i = 0; chanlists[i].name != NULL; i++)
    {
	if (!strcasecmp(chanlists[i].name, tv_param_chanlist))
	{
	    tvh->chanlist = i;
	    tvh->chanlist_s = chanlists[i].list;
	    break;
	}
    }

    if (tvh->chanlist == -1)
	mp_msg(MSGT_TV, MSGL_WARN, "Unable to find selected channel list! (%s)\n",
	    tv_param_chanlist);
    else
	mp_msg(MSGT_TV, MSGL_V, "Selected channel list: %s (including %d channels)\n",
	    chanlists[tvh->chanlist].name, chanlists[tvh->chanlist].count);

    if (tv_param_freq && tv_param_channel)
    {
	mp_msg(MSGT_TV, MSGL_WARN, "You can't set frequency and channel simultanly!\n");
	goto done;
    }

    /* Handle channels names */
    if (tv_param_channels) {
	char** channels = tv_param_channels;
	mp_msg(MSGT_TV, MSGL_INFO, "TV Channels names detected.\n");
	tv_channel_list = malloc(sizeof(tv_channels_t));
	tv_channel_list->index=1;
	tv_channel_list->next=NULL;
	tv_channel_list->prev=NULL;
	tv_channel_current = tv_channel_list;

	while (*channels) {
		char* tmp = *(channels++);
		char* sep = strchr(tmp,'-');
		int i;
		struct CHANLIST cl;

		if (!sep) continue; // Wrong syntax, but mplayer should not crash

		strcpy(tv_channel_current->name, sep + 1);
		sep[0] = '\0';
		strncpy(tv_channel_current->number, tmp, 5);

		while ((sep=strchr(tv_channel_current->name, '_')))
		    sep[0] = ' ';

		tv_channel_current->freq = 0;
		for (i = 0; i < chanlists[tvh->chanlist].count; i++) {
		    cl = tvh->chanlist_s[i];
		    if (!strcasecmp(cl.name, tv_channel_current->number)) {
			tv_channel_current->freq=cl.freq;
			break;
		    }
		}
	        if (tv_channel_current->freq == 0)
		    mp_msg(MSGT_TV, MSGL_ERR, "Couldn't find frequency for channel %s (%s)\n",
				    tv_channel_current->number, tv_channel_current->name);
		else {
		  sep = strchr(tv_channel_current->name, '-');
		  if ( !sep ) sep = strchr(tv_channel_current->name, '+');

		  if ( sep ) {
		    i = atoi (sep+1);
		    if ( sep[0] == '+' ) tv_channel_current->freq += i * 100;
		    if ( sep[0] == '-' ) tv_channel_current->freq -= i * 100;
		    sep[0] = '\0';
		  }
		}

		/*mp_msg(MSGT_TV, MSGL_INFO, "-- Detected channel %s - %s (%5.3f)\n",
				tv_channel_current->number, tv_channel_current->name,
				(float)tv_channel_current->freq/1000);*/

		tv_channel_current->next = malloc(sizeof(tv_channels_t));
		tv_channel_current->next->index = tv_channel_current->index + 1;
		tv_channel_current->next->prev = tv_channel_current;
		tv_channel_current->next->next = NULL;
		tv_channel_current = tv_channel_current->next;
	}
	if (tv_channel_current->prev)
  	  tv_channel_current->prev->next = NULL;
	free(tv_channel_current);
    } else 
	    tv_channel_last_real = malloc(sizeof(char)*5);

    if (tv_channel_list) {
	int i;
	int channel = 0;
	if (tv_param_channel)
	 {
	   if (isdigit(*tv_param_channel))
		/* if tv_param_channel begins with a digit interpret it as a number */
		channel = atoi(tv_param_channel);
	   else
	      {
		/* if tv_param_channel does not begin with a digit 
		   set the first channel that contains tv_param_channel in its name */

		tv_channel_current = tv_channel_list;
		while ( tv_channel_current ) {
			if ( strstr(tv_channel_current->name, tv_param_channel) )
			  break;
			tv_channel_current = tv_channel_current->next;
			}
		if ( !tv_channel_current ) tv_channel_current = tv_channel_list;
	      }
	 }
	else
		channel = 1;

	if ( channel ) {
	tv_channel_current = tv_channel_list;
	for (i = 1; i < channel; i++)
		if (tv_channel_current->next)
			tv_channel_current = tv_channel_current->next;
	}

	mp_msg(MSGT_TV, MSGL_INFO, "Selected channel: %s - %s (freq: %.3f)\n", tv_channel_current->number,
			tv_channel_current->name, (float)tv_channel_current->freq/1000);
	tv_set_freq(tvh, (unsigned long)(((float)tv_channel_current->freq/1000)*16));
	tv_channel_last = tv_channel_current;
    } else {
    /* we need to set frequency */
    if (tv_param_freq)
    {
	unsigned long freq = atof(tv_param_freq)*16;

        /* set freq in MHz */
	funcs->control(tvh->priv, TVI_CONTROL_TUN_SET_FREQ, &freq);

	funcs->control(tvh->priv, TVI_CONTROL_TUN_GET_FREQ, &freq);
	mp_msg(MSGT_TV, MSGL_V, "Selected frequency: %lu (%.3f)\n",
	    freq, (float)freq/16);
    }

	    if (tv_param_channel) {
	struct CHANLIST cl;

	mp_msg(MSGT_TV, MSGL_V, "Requested channel: %s\n", tv_param_channel);
	for (i = 0; i < chanlists[tvh->chanlist].count; i++)
	{
	    cl = tvh->chanlist_s[i];
		    //  printf("count%d: name: %s, freq: %d\n",
		    //	i, cl.name, cl.freq);
	    if (!strcasecmp(cl.name, tv_param_channel))
	    {
			strcpy(tv_channel_last_real, cl.name);
		tvh->channel = i;
		mp_msg(MSGT_TV, MSGL_INFO, "Selected channel: %s (freq: %.3f)\n",
		    cl.name, (float)cl.freq/1000);
		tv_set_freq(tvh, (unsigned long)(((float)cl.freq/1000)*16));
		break;
	    }
	}
    }
    }
    
    /* grep frequency in chanlist */
    {
	unsigned long i2;
	int freq;
	
	tv_get_freq(tvh, &i2);
	
	freq = (int) (((float)(i2/16))*1000)+250;
	
	for (i = 0; i < chanlists[tvh->chanlist].count; i++)
	{
	    if (tvh->chanlist_s[i].freq == freq)
	    {
		tvh->channel = i+1;
		break;
	    }
	}
    }

done:    
    /* also start device! */
	return 1;
}

int demux_open_tv(demuxer_t *demuxer)
{
    tvi_handle_t *tvh;
    sh_video_t *sh_video;
    sh_audio_t *sh_audio = NULL;
    tvi_functions_t *funcs;
    
    if(!(tvh=tv_begin())) return 0;
    if (!tv_init(tvh)) return 0;
    if (!open_tv(tvh)){
	tv_uninit(tvh);
	return 0;
    }
    funcs = tvh->functions;
    demuxer->priv=tvh;
    
    sh_video = new_sh_video(demuxer, 0);

    /* get IMAGE FORMAT */
    funcs->control(tvh->priv, TVI_CONTROL_VID_GET_FORMAT, &sh_video->format);
//    if (IMGFMT_IS_RGB(sh_video->format) || IMGFMT_IS_BGR(sh_video->format))
//	sh_video->format = 0x0;

    /* set FPS and FRAMETIME */

    if(!sh_video->fps)
    {
        float tmp;
        if (funcs->control(tvh->priv, TVI_CONTROL_VID_GET_FPS, &tmp) != TVI_CONTROL_TRUE)
             sh_video->fps = 25.0f; /* on PAL */
        else sh_video->fps = tmp;
    }

    if (tv_param_fps != -1.0f)
        sh_video->fps = tv_param_fps;

    sh_video->frametime = 1.0f/sh_video->fps;

    /* If playback only mode, go to immediate mode, fail silently */
    if(tv_param_immediate == 1)
        {
        funcs->control(tvh->priv, TVI_CONTROL_IMMEDIATE, 0);
        tv_param_noaudio = 1; 
        }

    /* disable TV audio if -nosound is present */
    if (!demuxer->audio || demuxer->audio->id == -2) {
        tv_param_noaudio = 1; 
    }

    /* set width */
    funcs->control(tvh->priv, TVI_CONTROL_VID_GET_WIDTH, &sh_video->disp_w);

    /* set height */
    funcs->control(tvh->priv, TVI_CONTROL_VID_GET_HEIGHT, &sh_video->disp_h);

    /* set color eq */
    tv_set_color_options(tvh, TV_COLOR_BRIGHTNESS, tv_param_brightness);
    tv_set_color_options(tvh, TV_COLOR_HUE, tv_param_hue);
    tv_set_color_options(tvh, TV_COLOR_SATURATION, tv_param_saturation);
    tv_set_color_options(tvh, TV_COLOR_CONTRAST, tv_param_contrast);

    demuxer->video->sh = sh_video;
    sh_video->ds = demuxer->video;
    demuxer->video->id = 0;

    demuxer->seekable = 0;

    /* here comes audio init */

    if (tv_param_noaudio == 0 && funcs->control(tvh->priv, TVI_CONTROL_IS_AUDIO, 0) == TVI_CONTROL_TRUE)
    {
	int audio_format;
	int sh_audio_format;

	/* yeah, audio is present */

	funcs->control(tvh->priv, TVI_CONTROL_AUD_SET_SAMPLERATE, 
				  &tv_param_audiorate);

	if (funcs->control(tvh->priv, TVI_CONTROL_AUD_GET_FORMAT, &audio_format) != TVI_CONTROL_TRUE)
	    goto no_audio;

	switch(audio_format)
	{
	    case AFMT_U8:
	    case AFMT_S8:
	    case AFMT_U16_LE:
	    case AFMT_U16_BE:
	    case AFMT_S16_LE:
	    case AFMT_S16_BE:
	    case AFMT_S32_LE:
	    case AFMT_S32_BE:
		sh_audio_format = 0x1; /* PCM */
		break;
	    case AFMT_IMA_ADPCM:
	    case AFMT_MU_LAW:
	    case AFMT_A_LAW:
	    case AFMT_MPEG:
	    case AFMT_AC3:
	    default:
		mp_msg(MSGT_TV, MSGL_ERR, "Audio type '%s (%x)' unsupported!\n",
		    audio_out_format_name(audio_format), audio_format);
		goto no_audio;
	}
	
	sh_audio = new_sh_audio(demuxer, 0);

	funcs->control(tvh->priv, TVI_CONTROL_AUD_GET_SAMPLERATE, 
                   &sh_audio->samplerate);
	funcs->control(tvh->priv, TVI_CONTROL_AUD_GET_SAMPLESIZE, 
                   &sh_audio->samplesize);
	funcs->control(tvh->priv, TVI_CONTROL_AUD_GET_CHANNELS, 
                   &sh_audio->channels);

	sh_audio->format = sh_audio_format;
	sh_audio->sample_format = audio_format;

	sh_audio->i_bps = sh_audio->o_bps =
	    sh_audio->samplerate * sh_audio->samplesize * 
	    sh_audio->channels;

	// emulate WF for win32 codecs:
	sh_audio->wf = (WAVEFORMATEX *)malloc(sizeof(WAVEFORMATEX));
	sh_audio->wf->wFormatTag = sh_audio->format;
	sh_audio->wf->nChannels = sh_audio->channels;
	sh_audio->wf->wBitsPerSample = sh_audio->samplesize * 8;
	sh_audio->wf->nSamplesPerSec = sh_audio->samplerate;
	sh_audio->wf->nBlockAlign = sh_audio->samplesize * sh_audio->channels;
	sh_audio->wf->nAvgBytesPerSec = sh_audio->i_bps;

	mp_msg(MSGT_DECVIDEO, MSGL_V, "  TV audio: %d channels, %d bits, %d Hz\n",
          sh_audio->wf->nChannels, sh_audio->wf->wBitsPerSample,
          sh_audio->wf->nSamplesPerSec);

	demuxer->audio->sh = sh_audio;
	sh_audio->ds = demuxer->audio;
	demuxer->audio->id = 0;
    }
no_audio:

    if(!(funcs->start(tvh->priv))){
	// start failed :(
	tv_uninit(tvh);
	return 0;
    }
    return 1;
}

int demux_close_tv(demuxer_t *demuxer)
{
    tvi_handle_t *tvh=(tvi_handle_t*)(demuxer->priv);
    return(tvh->functions->uninit(tvh->priv));
}

/* ================== STREAM_TV ===================== */
tvi_handle_t *tvi_init_dummy(char *device);
tvi_handle_t *tvi_init_v4l(char *device, char *adevice);
tvi_handle_t *tvi_init_bsdbt848(char *device);

tvi_handle_t *tv_begin(void)
{
    if (!strcmp(tv_param_driver, "dummy"))
	return tvi_init_dummy(tv_param_device);
#ifdef HAVE_TV_V4L
    if (!strcmp(tv_param_driver, "v4l"))
	return tvi_init_v4l(tv_param_device, tv_param_adevice);
#endif
#ifdef HAVE_TV_BSDBT848
    if (!strcmp(tv_param_driver, "bsdbt848"))
	return tvi_init_bsdbt848(tv_param_device);
#endif

    mp_msg(MSGT_TV, MSGL_ERR, "No such driver: %s\n", tv_param_driver); 
    return(NULL);
}

int tv_init(tvi_handle_t *tvh)
{
    mp_msg(MSGT_TV, MSGL_INFO, "Selected driver: %s\n", tvh->info->short_name);
    mp_msg(MSGT_TV, MSGL_INFO, " name: %s\n", tvh->info->name);
    mp_msg(MSGT_TV, MSGL_INFO, " author: %s\n", tvh->info->author);
    if (tvh->info->comment)
	mp_msg(MSGT_TV, MSGL_INFO, " comment: %s\n", tvh->info->comment);

    return(tvh->functions->init(tvh->priv));
}

int tv_uninit(tvi_handle_t *tvh)
{
    return(tvh->functions->uninit(tvh->priv));
}

/* utilities for mplayer (not mencoder!!) */
int tv_set_color_options(tvi_handle_t *tvh, int opt, int value)
{
    tvi_functions_t *funcs = tvh->functions;

    switch(opt)
    {
	case TV_COLOR_BRIGHTNESS:
	    funcs->control(tvh->priv, TVI_CONTROL_VID_SET_BRIGHTNESS, &value);
	    break;
	case TV_COLOR_HUE:
	    funcs->control(tvh->priv, TVI_CONTROL_VID_SET_HUE, &value);
	    break;
	case TV_COLOR_SATURATION:
	    funcs->control(tvh->priv, TVI_CONTROL_VID_SET_SATURATION, &value);
	    break;
	case TV_COLOR_CONTRAST:
	    funcs->control(tvh->priv, TVI_CONTROL_VID_SET_CONTRAST, &value);
	    break;
	default:
	    mp_msg(MSGT_TV, MSGL_WARN, "Unknown color option (%d) specified!\n", opt);
    }
    
    return(1);
}

int tv_get_freq(tvi_handle_t *tvh, unsigned long *freq)
{
    if (tvh->functions->control(tvh->priv, TVI_CONTROL_IS_TUNER, 0) == TVI_CONTROL_TRUE)
    {
	tvh->functions->control(tvh->priv, TVI_CONTROL_TUN_GET_FREQ, freq);
	mp_msg(MSGT_TV, MSGL_V, "Current frequency: %lu (%.3f)\n",
	    *freq, (float)*freq/16);
    }
    return(1);
}

int tv_set_freq(tvi_handle_t *tvh, unsigned long freq)
{
    if (tvh->functions->control(tvh->priv, TVI_CONTROL_IS_TUNER, 0) == TVI_CONTROL_TRUE)
    {
//	unsigned long freq = atof(tv_param_freq)*16;

        /* set freq in MHz */
	tvh->functions->control(tvh->priv, TVI_CONTROL_TUN_SET_FREQ, &freq);

	tvh->functions->control(tvh->priv, TVI_CONTROL_TUN_GET_FREQ, &freq);
	mp_msg(MSGT_TV, MSGL_V, "Current frequency: %lu (%.3f)\n",
	    freq, (float)freq/16);
    }
    return(1);
}

int tv_step_channel_real(tvi_handle_t *tvh, int direction)
{
    struct CHANLIST cl;

    if (direction == TV_CHANNEL_LOWER)
    {
	if (tvh->channel-1 >= 0)
	{
	    strcpy(tv_channel_last_real, tvh->chanlist_s[tvh->channel].name);
	    cl = tvh->chanlist_s[--tvh->channel];
	    mp_msg(MSGT_TV, MSGL_INFO, "Selected channel: %s (freq: %.3f)\n",
		cl.name, (float)cl.freq/1000);
	    tv_set_freq(tvh, (unsigned long)(((float)cl.freq/1000)*16));
	}	
    }

    if (direction == TV_CHANNEL_HIGHER)
    {
	if (tvh->channel+1 < chanlists[tvh->chanlist].count)
	{
	    strcpy(tv_channel_last_real, tvh->chanlist_s[tvh->channel].name);
	    cl = tvh->chanlist_s[++tvh->channel];
	    mp_msg(MSGT_TV, MSGL_INFO, "Selected channel: %s (freq: %.3f)\n",
		cl.name, (float)cl.freq/1000);
	    tv_set_freq(tvh, (unsigned long)(((float)cl.freq/1000)*16));
	}	
    }
    return(1);
}

int tv_step_channel(tvi_handle_t *tvh, int direction) {
	if (tv_channel_list) {
		if (direction == TV_CHANNEL_HIGHER) {
			if (tv_channel_current->next) {
				tv_channel_last = tv_channel_current;
				tv_channel_current = tv_channel_current->next;
				tv_set_freq(tvh, (unsigned long)(((float)tv_channel_current->freq/1000)*16));
				mp_msg(MSGT_TV, MSGL_INFO, "Selected channel: %s - %s (freq: %.3f)\n",
			tv_channel_current->number, tv_channel_current->name, (float)tv_channel_current->freq/1000);
			}
		}
		if (direction == TV_CHANNEL_LOWER) {
			if (tv_channel_current->prev) {
				tv_channel_last = tv_channel_current;
				tv_channel_current = tv_channel_current->prev;
				tv_set_freq(tvh, (unsigned long)(((float)tv_channel_current->freq/1000)*16));
				mp_msg(MSGT_TV, MSGL_INFO, "Selected channel: %s - %s (freq: %.3f)\n",
			tv_channel_current->number, tv_channel_current->name, (float)tv_channel_current->freq/1000);
			}
		}
	} else tv_step_channel_real(tvh, direction);
	return(1);
}

int tv_set_channel_real(tvi_handle_t *tvh, char *channel) {
	int i;
	struct CHANLIST cl;

        strcpy(tv_channel_last_real, tvh->chanlist_s[tvh->channel].name);
	for (i = 0; i < chanlists[tvh->chanlist].count; i++)
	{
	    cl = tvh->chanlist_s[i];
//	    printf("count%d: name: %s, freq: %d\n",
//		i, cl.name, cl.freq);
	    if (!strcasecmp(cl.name, channel))
	    {
		tvh->channel = i;
		mp_msg(MSGT_TV, MSGL_INFO, "Selected channel: %s (freq: %.3f)\n",
		    cl.name, (float)cl.freq/1000);
		tv_set_freq(tvh, (unsigned long)(((float)cl.freq/1000)*16));
		break;
	    }
	}
	return(1);
}

int tv_set_channel(tvi_handle_t *tvh, char *channel) {
	int i, channel_int;

	if (tv_channel_list) {
		tv_channel_last = tv_channel_current;
		channel_int = atoi(channel);
		tv_channel_current = tv_channel_list;
		for (i = 1; i < channel_int; i++)
			if (tv_channel_current->next)
				tv_channel_current = tv_channel_current->next;
		mp_msg(MSGT_TV, MSGL_INFO, "Selected channel: %s - %s (freq: %.3f)\n", tv_channel_current->number,
				tv_channel_current->name, (float)tv_channel_current->freq/1000);
		tv_set_freq(tvh, (unsigned long)(((float)tv_channel_current->freq/1000)*16));
	} else tv_set_channel_real(tvh, channel);
	return(1);
}

int tv_last_channel(tvi_handle_t *tvh) {

	if (tv_channel_list) {
		tv_channels_t *tmp;

		tmp = tv_channel_last;
		tv_channel_last = tv_channel_current;
		tv_channel_current = tmp;

		mp_msg(MSGT_TV, MSGL_INFO, "Selected channel: %s - %s (freq: %.3f)\n", tv_channel_current->number,
				tv_channel_current->name, (float)tv_channel_current->freq/1000);
		tv_set_freq(tvh, (unsigned long)(((float)tv_channel_current->freq/1000)*16));
	} else {
		int i;
		struct CHANLIST cl;

		for (i = 0; i < chanlists[tvh->chanlist].count; i++)
		{
		    cl = tvh->chanlist_s[i];
		    if (!strcasecmp(cl.name, tv_channel_last_real))
		    {
			strcpy(tv_channel_last_real, tvh->chanlist_s[tvh->channel].name);
			tvh->channel = i;
			mp_msg(MSGT_TV, MSGL_INFO, "Selected channel: %s (freq: %.3f)\n",
			    cl.name, (float)cl.freq/1000);
			tv_set_freq(tvh, (unsigned long)(((float)cl.freq/1000)*16));
			break;
		    }
		}
	}
	return(1);
}

int tv_step_norm(tvi_handle_t *tvh)
{
    return(1);
}

int tv_step_chanlist(tvi_handle_t *tvh)
{
    return(1);
}
#endif /* USE_TV */

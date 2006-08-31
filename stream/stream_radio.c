
#include "config.h"
/*
 *     Radio support
 * 
 *     Initially wrote by Vladimir Voroshilov <voroshil@univer.omsk.su>.
 *     Based on tv.c and tvi_v4l2.c code.
 *
 *     This program is free software; you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation; either version 2 of the License, or
 *     (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 *
 *
 *     Abilities:
 *     * Listening v4l compatible radio cards using line-in or
 *       similar cable
 *     * Grabbing audio data using -ao pcm or -dumpaudio
 *       (must be compiled with --enable-radio-capture).
 */
#if !defined(HAVE_ALSA9) && !defined(HAVE_ALSA1X) && !defined(USE_OSS_AUDIO) && defined(USE_RADIO_CAPTURE)
#warning "Neither alsa1x, alsa9 nor oss found. Radio capture disabled"
#undef USE_RADIO_CAPTURE
#endif

#if !defined(HAVE_RADIO_V4L) && !defined(HAVE_RADIO_V4L2)
#error "This driver requires V4L1 or V4L2!"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <unistd.h>
#include <linux/types.h>

#ifdef HAVE_RADIO_V4L2
#include <linux/videodev2.h>
#endif

#ifdef HAVE_RADIO_V4L
#include <linux/videodev.h>
#warning  "V4L is deprecated and will be removed in future"
#endif



#include "stream.h"
#include "libmpdemux/demuxer.h"
#include "m_struct.h"
#include "m_option.h"
#include "mp_msg.h"
#include "help_mp.h"
#include "stream_radio.h"

#ifdef USE_RADIO_CAPTURE
#include "audio_in.h"

#ifdef HAVE_SYS_SOUNDCARD_H
#include <sys/soundcard.h>
#else
#ifdef HAVE_SOUNDCARD_H
#include <soundcard.h>
#else
#include <linux/soundcard.h>
#endif
#endif

#endif

#define RADIO_DRIVER_UNKNOWN    0
#define RADIO_DRIVER_V4L        1
#define RADIO_DRIVER_V4L2       2

typedef struct radio_channels_s {
    int index;     ///< channel index in channels list
    float freq;    ///< frequency in MHz
    char name[20]; ///< channel name
    struct radio_channels_s * next;
    struct radio_channels_s * prev;
} radio_channels_t;

/** (device,string, "/dev/radio0") name of radio device file */
char*   radio_param_device="/dev/radio0";
/** (driver,string, "v4l2") radio driver (v4l,v4l2) */
char*   radio_param_driver="default";
/** radio_param_channels (channels,string,NULL) channels list (see man page) */
char**  radio_param_channels;
/** radio_param_volume (volume,number,100) initial volume for radio device */
int     radio_param_volume=100;
/** radio_param_adevice (adevice,string,NULL) name of audio device file to grab data from */
char*   radio_param_adevice;
/** radio_param_arate (arate,number,44100) audio framerate
(please also set -rawaudio rate parameter to the same value) */
int     radio_param_arate=44100;
/** radio_param_achannels (achannels,number,2) number of audio channels */
int     radio_param_achannels=2;
extern int demux_rawaudio_packs_per_sec;

static struct stream_priv_s {
    /* if channels parameter exist here will be channel number otherwise - frequency */
    float radio_param_freq_channel;
    char* capture;
} stream_priv_dflts = {
    0,
    NULL
};

typedef struct radio_priv_s {
    int                 radio_fd;          ///< radio device descriptor
    int                 frac;              ///< fraction value (see comment to init_frac)
    radio_channels_t*   radio_channel_list;
    radio_channels_t*   radio_channel_current;
    int                 driver;
#ifdef USE_RADIO_CAPTURE
    volatile int        do_capture;        ///< is capture enabled
    audio_in_t          audio_in;
    unsigned char*      audio_ringbuffer;
    int                 audio_head;        ///< start of meanfull data in ringbuffer
    int                 audio_tail;        ///< end of meanfull data in ringbuffer
    int                 audio_buffer_size; ///< size of ringbuffer
    int                 audio_cnt;         ///< size of meanfull data inringbuffer
    int                 audio_drop;        ///< number of dropped bytes
    int                 audio_inited;
#endif
} radio_priv_t;

#define ST_OFF(f) M_ST_OFF(struct stream_priv_s,f)
static m_option_t stream_opts_fields[] = {
    {"hostname", ST_OFF(radio_param_freq_channel), CONF_TYPE_FLOAT, 0, 0 ,0, NULL},
    {"filename", ST_OFF(capture), CONF_TYPE_STRING, 0, 0 ,0, NULL},
    { NULL, NULL, 0, 0, 0, 0,  NULL }
};

static struct m_struct_st stream_opts = {
    "radio",
    sizeof(struct stream_priv_s),
    &stream_priv_dflts,
    stream_opts_fields
};

static void close_s(struct stream_st * stream);
#ifdef USE_RADIO_CAPTURE
static int clear_buffer(radio_priv_t* priv);
#endif


/*****************************************************************
 * \brief parse radio_param_channels parameter and store result into list
 * \param freq_channel if radio_param_channels!=NULL this mean channel number, otherwise - frequency
 * \param pfreq selected frequency (from selected channel or from URL)
 * \result STREAM_OK if success, STREAM_ERROR otherwise
 *
 *  radio_param_channels (channels options) must be in the following format
 *  <frequency>-<name>,<frequency>-<name>,...
 *
 *  '_' will be replaced with spaces.
 *
 *  If radio_param_channels is not null, number in movie URL will be treated as
 *  channel position in channel list.
 */
static int parse_channels(radio_priv_t* priv,float freq_channel,float* pfreq){
    char** channels;
    int i;
    int channel = 0;
    if (radio_param_channels){
        /*parsing channels string*/
        channels =radio_param_channels;

        mp_msg(MSGT_RADIO, MSGL_INFO, MSGTR_RADIO_ChannelNamesDetected);
        priv->radio_channel_list = malloc(sizeof(radio_channels_t));
        priv->radio_channel_list->index=1;
        priv->radio_channel_list->next=NULL;
        priv->radio_channel_list->prev=NULL;
        priv->radio_channel_current = priv->radio_channel_list;

        while (*channels) {
            char* tmp = *(channels++);
            char* sep = strchr(tmp,'-');
            if (!sep) continue; // Wrong syntax, but mplayer should not crash
            strlcpy(priv->radio_channel_current->name, sep + 1,sizeof(priv->radio_channel_current->name)-1);

            sep[0] = '\0';

            priv->radio_channel_current->freq=atof(tmp);

            if (priv->radio_channel_current->freq == 0)
                mp_msg(MSGT_RADIO, MSGL_ERR, MSGTR_RADIO_WrongFreqForChannel,
                    priv->radio_channel_current->name);

            while ((sep=strchr(priv->radio_channel_current->name, '_'))) sep[0] = ' ';

            priv->radio_channel_current->next = malloc(sizeof(radio_channels_t));
            priv->radio_channel_current->next->index = priv->radio_channel_current->index + 1;
            priv->radio_channel_current->next->prev = priv->radio_channel_current;
            priv->radio_channel_current->next->next = NULL;
            priv->radio_channel_current = priv->radio_channel_current->next;
        }
        if (priv->radio_channel_current->prev)
            priv->radio_channel_current->prev->next = NULL;
        free(priv->radio_channel_current);

        if (freq_channel)
            channel = freq_channel;
        else
            channel = 1;

        priv->radio_channel_current = priv->radio_channel_list;
        for (i = 1; i < channel; i++)
            if (priv->radio_channel_current->next)
                priv->radio_channel_current = priv->radio_channel_current->next;
        if (priv->radio_channel_current->index!=channel){
            if (((float)((int)freq_channel))!=freq_channel)
                mp_msg(MSGT_RADIO, MSGL_ERR, MSGTR_RADIO_WrongChannelNumberFloat,freq_channel);
            else
                mp_msg(MSGT_RADIO, MSGL_ERR, MSGTR_RADIO_WrongChannelNumberInt,(int)freq_channel);
            return STREAM_ERROR;
        }
        mp_msg(MSGT_RADIO, MSGL_INFO, MSGTR_RADIO_SelectedChannel, priv->radio_channel_current->index,
            priv->radio_channel_current->name, priv->radio_channel_current->freq);
        *pfreq=priv->radio_channel_current->freq;
    }else{
        if (freq_channel){
            mp_msg(MSGT_RADIO, MSGL_INFO, MSGTR_RADIO_FreqParameterDetected);
            priv->radio_channel_list=malloc(sizeof(radio_channels_t));
            priv->radio_channel_list->next=NULL;
            priv->radio_channel_list->prev=NULL;
            priv->radio_channel_list->index=1;
            snprintf(priv->radio_channel_list->name,sizeof(priv->radio_channel_current->name)-1,"Freq: %.2f",freq_channel);

            priv->radio_channel_current=priv->radio_channel_list;
            *pfreq=freq_channel;
        }
    }
    mp_msg(MSGT_RADIO, MSGL_DBG2, MSGTR_RADIO_DoneParsingChannels);
    return STREAM_OK;
}

#ifdef HAVE_RADIO_V4L2
/*****************************************************************
 * \brief get fraction value for using in set_frequency and get_frequency
 * \return STREAM_OK if success, STREAM_ERROR otherwise
 *
 * V4L2_TUNER_CAP_LOW:
 * unit=62.5Hz
 * frac= 1MHz/unit=1000000/62.5 =16000
 *
 * otherwise:
 * unit=62500Hz
 * frac= 1MHz/unit=1000000/62500 =16
 */
static int init_frac_v4l2(radio_priv_t* priv){
    struct v4l2_tuner tuner;

    memset(&tuner,0,sizeof(tuner));
    tuner.index=0;
    if (ioctl(priv->radio_fd, VIDIOC_G_TUNER, &tuner)<0){
        mp_msg(MSGT_RADIO,MSGL_WARN,MSGTR_RADIO_GetTunerFailed,strerror(errno),priv->frac);
        return  STREAM_ERROR;
    }
    if(tuner.type!=V4L2_TUNER_RADIO){
        mp_msg(MSGT_RADIO,MSGL_ERR,MSGTR_RADIO_NotRadioDevice,radio_param_device);
        return STREAM_ERROR;
    }
    if(tuner.capability & V4L2_TUNER_CAP_LOW){
        priv->frac=16000;
        mp_msg(MSGT_RADIO,MSGL_DBG2,MSGTR_RADIO_TunerCapLowYes,priv->frac);
    }
    else{
        priv->frac=16;
        mp_msg(MSGT_RADIO,MSGL_DBG2,MSGTR_RADIO_TunerCapLowNo,priv->frac);
    }
    return STREAM_OK;
}

/*****************************************************************
 * \brief tune card to given frequency
 * \param frequency frequency in MHz
 * \return STREAM_OK if success, STREAM_ERROR otherwise
 */
static int set_frequency_v4l2(radio_priv_t* priv,float frequency){
    struct v4l2_frequency freq;

    memset(&freq,0,sizeof(freq));
    freq.tuner=0;
    freq.type=V4L2_TUNER_RADIO;
    freq.frequency=frequency*priv->frac;
    if(ioctl(priv->radio_fd,VIDIOC_S_FREQUENCY,&freq)<0){
        mp_msg(MSGT_RADIO,MSGL_ERR,MSGTR_RADIO_SetFreqFailed,freq.frequency,
        frequency,strerror(errno));
        return  STREAM_ERROR;
    }
#ifdef USE_RADIO_CAPTURE
    if(clear_buffer(priv)!=STREAM_OK){
        mp_msg(MSGT_RADIO,MSGL_ERR,MSGTR_RADIO_ClearBufferFailed,strerror(errno));
        return  STREAM_ERROR;
    }
#endif
    return STREAM_OK;
}

/*****************************************************************
 * \brief get current tuned frequency from card
 * \param frequency where to store frequency in MHz
 * \return STREAM_OK if success, STREAM_ERROR otherwise
 */
static int get_frequency_v4l2(radio_priv_t* priv,float* frequency){
    struct v4l2_frequency freq;
    memset(&freq,0,sizeof(freq));
    if (ioctl(priv->radio_fd, VIDIOC_G_FREQUENCY, &freq) < 0) {
        mp_msg(MSGT_RADIO,MSGL_ERR,MSGTR_RADIO_GetFreqFailed,strerror(errno));
        return  STREAM_ERROR;
    }
    *frequency=((float)freq.frequency)/priv->frac;
    return STREAM_OK;
}

/*****************************************************************
 * \brief set volume on radio card
 * \param volume volume level (0..100)
 * \return STREAM_OK if success, STREAM_ERROR otherwise
 */
static void set_volume_v4l2(radio_priv_t* priv,int volume){
    struct v4l2_queryctrl qctrl;
    struct v4l2_control control;

    /*arg must be between 0 and 100*/
    if (volume > 100) volume = 100;
    if (volume < 0) volume = 0;

    memset(&control,0,sizeof(control));
    control.id=V4L2_CID_AUDIO_MUTE;
    control.value = (volume==0?1:0);
    if (ioctl(priv->radio_fd, VIDIOC_S_CTRL, &control)<0){
        mp_msg(MSGT_RADIO,MSGL_WARN,MSGTR_RADIO_SetMuteFailed,strerror(errno));
    }

    memset(&qctrl,0,sizeof(qctrl));
    qctrl.id = V4L2_CID_AUDIO_VOLUME;
    if (ioctl(priv->radio_fd, VIDIOC_QUERYCTRL, &qctrl) < 0) {
        mp_msg(MSGT_RADIO, MSGL_WARN, MSGTR_RADIO_QueryControlFailed,strerror(errno));
        return;
    }

    memset(&control,0,sizeof(control));
    control.id=V4L2_CID_AUDIO_VOLUME;
    control.value=qctrl.minimum+volume*(qctrl.maximum-qctrl.minimum)/100;
    if (ioctl(priv->radio_fd, VIDIOC_S_CTRL, &control) < 0) {
        mp_msg(MSGT_RADIO, MSGL_WARN,MSGTR_RADIO_SetVolumeFailed,strerror(errno));
    }
}

/*****************************************************************
 * \brief get current volume from radio card
 * \param volume where to store volume level (0..100)
 * \return STREAM_OK if success, STREAM_ERROR otherwise
 */
static int get_volume_v4l2(radio_priv_t* priv,int* volume){
    struct v4l2_queryctrl qctrl;
    struct v4l2_control control;

    memset(&qctrl,0,sizeof(qctrl));
    qctrl.id = V4L2_CID_AUDIO_VOLUME;
    if (ioctl(priv->radio_fd, VIDIOC_QUERYCTRL, &qctrl) < 0) {
        mp_msg(MSGT_RADIO, MSGL_ERR, MSGTR_RADIO_QueryControlFailed,strerror(errno));
        return STREAM_ERROR;
    }

    memset(&control,0,sizeof(control));
    control.id=V4L2_CID_AUDIO_VOLUME;
    if (ioctl(priv->radio_fd, VIDIOC_G_CTRL, &control) < 0) {
        mp_msg(MSGT_RADIO, MSGL_ERR,MSGTR_RADIO_GetVolumeFailed,strerror(errno));
        return STREAM_ERROR;
    }

    if (qctrl.maximum==qctrl.minimum)
        *volume=qctrl.minimum;
    else
        *volume=100*(control.value-qctrl.minimum)/(qctrl.maximum-qctrl.minimum);

    /*arg must be between 0 and 100*/
    if (*volume > 100) *volume = 100;
    if (*volume < 0) *volume = 0;

    return STREAM_OK;
}
#endif //HAVE_RADIO_V4L2
#ifdef HAVE_RADIO_V4L
/*****************************************************************
 * \brief get fraction value for using in set_frequency and get_frequency
 * \return STREAM_OK if success, STREAM_ERROR otherwise
 *
 * V4L2_TUNER_CAP_LOW:
 * unit=62.5Hz
 * frac= 1MHz/unit=1000000/62.5 =16000
 *
 * otherwise:
 * unit=62500Hz
 * frac= 1MHz/unit=1000000/62500 =16
 *
 */
static int init_frac_v4l(radio_priv_t* priv){
    struct video_tuner tuner;
    memset(&tuner,0,sizeof(tuner));
    if (ioctl(priv->radio_fd, VIDIOCGTUNER, &tuner) <0){
        mp_msg(MSGT_RADIO,MSGL_WARN,MSGTR_RADIO_GetTunerFailed,strerror(errno),priv->frac);
        return  STREAM_ERROR;
    }
    if(tuner.flags & VIDEO_TUNER_LOW){
        priv->frac=16000;
        mp_msg(MSGT_RADIO,MSGL_DBG2,MSGTR_RADIO_TunerCapLowYes,priv->frac);
    }else{
        priv->frac=16;
        mp_msg(MSGT_RADIO,MSGL_DBG2,MSGTR_RADIO_TunerCapLowNo,priv->frac);
    }
    return STREAM_OK;
}

/*****************************************************************
 * \brief tune card to given frequency
 * \param frequency frequency in MHz
 * \return STREAM_OK if success, STREAM_ERROR otherwise
 */
static int set_frequency_v4l(radio_priv_t* priv,float frequency){
    __u32 freq;
    freq=frequency*priv->frac;
    if (ioctl(priv->radio_fd, VIDIOCSFREQ, &freq) < 0) {
        mp_msg(MSGT_RADIO,MSGL_ERR,MSGTR_RADIO_SetFreqFailed,freq,frequency,strerror(errno));
        return  STREAM_ERROR;
    }
#ifdef USE_RADIO_CAPTURE
    if(clear_buffer(priv)!=STREAM_OK){
        mp_msg(MSGT_RADIO,MSGL_ERR,MSGTR_RADIO_ClearBufferFailed,strerror(errno));
        return  STREAM_ERROR;
    }
#endif
    return STREAM_OK;
}
/*****************************************************************
 * \brief get current tuned frequency from card
 * \param frequency where to store frequency in MHz
 * \return STREAM_OK if success, STREAM_ERROR otherwise
 */
static int get_frequency_v4l(radio_priv_t* priv,float* frequency){
    __u32 freq;
    if (ioctl(priv->radio_fd, VIDIOCGFREQ, &freq) < 0) {
        mp_msg(MSGT_RADIO,MSGL_ERR,MSGTR_RADIO_GetFreqFailed,strerror(errno));
        return  STREAM_ERROR;
    }
    *frequency=((float)freq)/priv->frac;
    return STREAM_OK;
}

/*****************************************************************
 * \brief set volume on radio card
 * \param volume volume level (0..100)
 * \return STREAM_OK if success, STREAM_ERROR otherwise
 */
static void set_volume_v4l(radio_priv_t* priv,int volume){
    struct video_audio audio;

    /*arg must be between 0 and 100*/
    if (volume > 100) volume = 100;
    if (volume < 0) volume = 0;

    memset(&audio,0,sizeof(audio));
    audio.flags = (volume==0?VIDEO_AUDIO_MUTE:0);
    if (ioctl(priv->radio_fd, VIDIOCSAUDIO, &audio)<0){
        mp_msg(MSGT_RADIO,MSGL_WARN,MSGTR_RADIO_SetMuteFailed,strerror(errno));
    }

    memset(&audio,0,sizeof(audio));
    audio.flags = VIDEO_AUDIO_VOLUME;
    audio.mode = VIDEO_SOUND_STEREO;
    audio.audio = 0;
    audio.volume =  volume* (65535 / 100);

    if (ioctl(priv->radio_fd, VIDIOCSAUDIO, &audio) < 0){
        mp_msg(MSGT_RADIO,MSGL_ERR,MSGTR_RADIO_SetVolumeFailed,strerror(errno));
    }
}

/*****************************************************************
 * \brief get current volume from radio card
 * \param volume where to store volume level (0..100)
 * \return STREAM_OK if success, STREAM_ERROR otherwise
 */
static int get_volume_v4l(radio_priv_t* priv,int* volume){
    struct video_audio audio;

    memset(&audio,0,sizeof(audio));
    audio.audio=0;
    if (ioctl(priv->radio_fd, VIDIOCGAUDIO, &audio) < 0){
        mp_msg(MSGT_RADIO,MSGL_ERR,MSGTR_RADIO_GetVolumeFailed,strerror(errno));
        return STREAM_ERROR;
    }

    if (audio.flags & VIDEO_AUDIO_VOLUME){
        *volume=100*audio.volume/65535;
        /*arg must be between 0 and 100*/
        if (*volume > 100) *volume = 100;
        if (*volume < 0) *volume = 0;
        return STREAM_OK;
    }

    return STREAM_ERROR;
}
#endif //HAVE_RADIO_V4L

static inline int init_frac(radio_priv_t* priv){ 
    switch(priv->driver){
#ifdef HAVE_RADIO_V4L
        case RADIO_DRIVER_V4L:
            return init_frac_v4l(priv);
#endif
#ifdef HAVE_RADIO_V4L2
        case RADIO_DRIVER_V4L2:
            return init_frac_v4l2(priv);
#endif
    }
    mp_msg(MSGT_RADIO,MSGL_ERR,MSGTR_RADIO_DriverUnknownId,priv->driver);
    return STREAM_ERROR;
}
static inline int set_frequency(radio_priv_t* priv,float frequency){ 
    switch(priv->driver){
#ifdef HAVE_RADIO_V4L
        case RADIO_DRIVER_V4L:
            return set_frequency_v4l(priv,frequency);
#endif
#ifdef HAVE_RADIO_V4L2
        case RADIO_DRIVER_V4L2:
            return set_frequency_v4l2(priv,frequency);
#endif
    }
    mp_msg(MSGT_RADIO,MSGL_ERR,MSGTR_RADIO_DriverUnknownId,priv->driver);
    return STREAM_ERROR;
}
static inline int get_frequency(radio_priv_t* priv,float* frequency){ 
    switch(priv->driver){
#ifdef HAVE_RADIO_V4L
        case RADIO_DRIVER_V4L:
            return get_frequency_v4l(priv,frequency);
#endif
#ifdef HAVE_RADIO_V4L2
        case RADIO_DRIVER_V4L2:
            return get_frequency_v4l2(priv,frequency);
#endif
    }
    mp_msg(MSGT_RADIO,MSGL_ERR,MSGTR_RADIO_DriverUnknownId,priv->driver);
    return STREAM_ERROR;
}
static inline void set_volume(radio_priv_t* priv,int volume){ 
    switch(priv->driver){
#ifdef HAVE_RADIO_V4L
        case RADIO_DRIVER_V4L:
            set_volume_v4l(priv,volume);
            return;
#endif
#ifdef HAVE_RADIO_V4L2
        case RADIO_DRIVER_V4L2:
            set_volume_v4l2(priv,volume);
            return;
#endif
    }
    mp_msg(MSGT_RADIO,MSGL_ERR,MSGTR_RADIO_DriverUnknownId,priv->driver);
}
static inline int get_volume(radio_priv_t* priv,int* volume){ 
    switch(priv->driver){
#ifdef HAVE_RADIO_V4L
        case RADIO_DRIVER_V4L:
            return get_volume_v4l(priv,volume);
#endif
#ifdef HAVE_RADIO_V4L2
        case RADIO_DRIVER_V4L2:
            return get_volume_v4l2(priv,volume);
#endif
    }
    mp_msg(MSGT_RADIO,MSGL_ERR,MSGTR_RADIO_DriverUnknownId,priv->driver);
    return STREAM_ERROR;
}


#ifndef USE_RADIO_CAPTURE
/*****************************************************************
 * \brief stub, if capture disabled at compile-time
 * \return STREAM_OK
 */
static inline int init_audio(radio_priv_t *priv){ return STREAM_OK;}
#else
/*****************************************************************
 * \brief making buffer empty
 * \return STREAM_OK if success, STREAM_ERROR otherwise
 *
 * when changing channel we must clear buffer to avoid large switching delay
 */
static int clear_buffer(radio_priv_t* priv){
    if (!priv->do_capture) return STREAM_OK;
    priv->audio_tail = 0;
    priv->audio_head = 0;
    priv->audio_cnt=0;
    memset(priv->audio_ringbuffer,0,priv->audio_in.blocksize);
    return STREAM_OK;
}
/*****************************************************************
 * \brief read next part of data into buffer
 * \return -1 if error occured or no data available yet, otherwise - bytes read
 * NOTE: audio device works in non-blocking mode
 */
static int read_chunk(audio_in_t *ai, unsigned char *buffer)
{
    int ret;

    switch (ai->type) {
#if defined(HAVE_ALSA9) || defined(HAVE_ALSA1X)
    case AUDIO_IN_ALSA:
        //device opened in non-blocking mode
        ret = snd_pcm_readi(ai->alsa.handle, buffer, ai->alsa.chunk_size);
        if (ret != ai->alsa.chunk_size) {
            if (ret < 0) {
                if (ret==-EAGAIN) return -1;
                mp_msg(MSGT_RADIO, MSGL_ERR, MSGTR_MPDEMUX_AUDIOIN_ErrReadingAudio, snd_strerror(ret));
                if (ret == -EPIPE) {
                    if (ai_alsa_xrun(ai) == 0) {
                        mp_msg(MSGT_RADIO, MSGL_ERR, MSGTR_MPDEMUX_AUDIOIN_XRUNSomeFramesMayBeLeftOut);
                    } else {
                        mp_msg(MSGT_RADIO, MSGL_ERR, MSGTR_MPDEMUX_AUDIOIN_ErrFatalCannotRecover);
                    }
                }
            } else {
                mp_msg(MSGT_RADIO, MSGL_ERR, MSGTR_MPDEMUX_AUDIOIN_NotEnoughSamples);
            }
            return -1;
        }
        return ret;
#endif
#ifdef USE_OSS_AUDIO
    case AUDIO_IN_OSS:
    {
        int bt=0;
        /*
            we must return exactly blocksize bytes, so if we have got any bytes
            at first call to read, we will loop untils get all blocksize bytes
            otherwise we will return -1
        */
        while(bt<ai->blocksize){
        //device opened in non-blocking mode
            ret = read(ai->oss.audio_fd, buffer+bt, ai->blocksize-bt);
            if (ret==ai->blocksize) return ret;
            if (ret<0){
                if (errno==EAGAIN && bt==0) return -1; //no data avail yet
                if (errno==EAGAIN) { usleep(1000); continue;} //nilling buffer to blocksize size
                mp_msg(MSGT_RADIO, MSGL_ERR, MSGTR_MPDEMUX_AUDIOIN_ErrReadingAudio, strerror(errno));
                return -1;
            }
            bt+=ret;
        }
        return bt;
    }
#endif
    default:
        return -1;
    }
}
/*****************************************************************
 * \brief grab next frame from audio device
 * \parameter buffer - store buffer
 * \parameter len - store buffer size in bytes
 * \return number of bytes written into buffer
 *
 *     grabs len (or less) bytes from ringbuffer into buffer
 *     if ringbuffer is empty waits until len bytes of data will be available
 *
 *     priv->audio_cnt - size (in bytes) of ringbuffer's filled part
 *     priv->audio_drop - size (in bytes) of dropped data (if no space in ringbuffer)
 *     priv->audio_head - index of first byte in filled part
 *     priv->audio_tail - index of last byte in filled part
 *
 *     NOTE: audio_tail always aligned by priv->audio_in.blocksize
 *         audio_head may NOT.
 */
static int grab_audio_frame(radio_priv_t *priv, char *buffer, int len)
{
    int i;
    mp_msg(MSGT_RADIO, MSGL_DBG3, MSGTR_RADIO_BufferString,"grab_audio_frame",priv->audio_cnt,priv->audio_drop);
    /* Cache buffer must be filled by some audio packets when playing starts. 
       Otherwise MPlayer will quit with EOF error.
       Probably, there is need more carefull checking rather than simple 'for' loop
       (something like timer or similar).
       
       1000ms delay will happen only at first buffer filling. At next call function
       just fills buffer until either buffer full or no data from driver available.
    */
    for (i=0;i<1000 && priv->audio_cnt<priv->audio_buffer_size; i++){
        //read_chunk fills exact priv->blocksize bytes
        if(read_chunk(&priv->audio_in, priv->audio_ringbuffer+priv->audio_tail) < 0){
            //sleppeing only when waiting first block to fill empty buffer
            if (!priv->audio_cnt){
                usleep(1000);
                continue;
            }else
                break;
        }
        priv->audio_cnt+=priv->audio_in.blocksize;
        priv->audio_tail = (priv->audio_tail+priv->audio_in.blocksize) % priv->audio_buffer_size;
    }
    if(priv->audio_cnt<len)
        len=priv->audio_cnt;
    memcpy(buffer, priv->audio_ringbuffer+priv->audio_head,len);
    priv->audio_head = (priv->audio_head+len) % priv->audio_buffer_size;
    priv->audio_cnt-=len;
    return len;
}
/*****************************************************************
 * \brief init audio device
 * \return STREAM_OK if success, STREAM_ERROR otherwise
 */
static int init_audio(radio_priv_t *priv)
{
    int is_oss=1;
    int seconds=2;
    char* tmp;
    if (priv->audio_inited) return 1;

    /* do_capture==0 mplayer was not started with capture keyword, so disabling capture*/
    if(!priv->do_capture)
        return STREAM_OK;

    if (!radio_param_adevice){
        priv->do_capture=0;
        return STREAM_OK;
    }

    priv->do_capture=1;
    mp_msg(MSGT_RADIO,MSGL_V,MSGTR_RADIO_CaptureStarting);
#if defined(HAVE_ALSA9) || defined(HAVE_ALSA1X)
    while ((tmp = strrchr(radio_param_adevice, '='))){
        tmp[0] = ':';
        //radio_param_adevice looks like ALSA device name. Switching to ALSA
        is_oss=0;
    }
    while ((tmp = strrchr(radio_param_adevice, '.')))
        tmp[0] = ',';
#endif

    if(audio_in_init(&priv->audio_in, is_oss?AUDIO_IN_OSS:AUDIO_IN_ALSA)<0){
        mp_msg(MSGT_RADIO, MSGL_ERR, MSGTR_RADIO_AudioInInitFailed,strerror(errno));
    }

    audio_in_set_device(&priv->audio_in, radio_param_adevice);
    audio_in_set_channels(&priv->audio_in, radio_param_achannels);
    audio_in_set_samplerate(&priv->audio_in, radio_param_arate);

    if (audio_in_setup(&priv->audio_in) < 0) {
        mp_msg(MSGT_RADIO, MSGL_ERR, MSGTR_RADIO_AudioInSetupFailed, strerror(errno));
        return STREAM_ERROR;
    }
    if(is_oss)
        ioctl(priv->audio_in.oss.audio_fd, SNDCTL_DSP_NONBLOCK, 0);
#if defined(HAVE_ALSA9) || defined(HAVE_ALSA1X)
    else{
        snd_pcm_nonblock(priv->audio_in.alsa.handle,1);
    }
#endif

    priv->audio_buffer_size = seconds*priv->audio_in.samplerate*priv->audio_in.channels*
            priv->audio_in.bytes_per_sample+priv->audio_in.blocksize;
    if (priv->audio_buffer_size < 256*priv->audio_in.blocksize)
        priv->audio_buffer_size = 256*priv->audio_in.blocksize;
    mp_msg(MSGT_RADIO, MSGL_V, MSGTR_RADIO_AudioBuffer,
        priv->audio_buffer_size,priv->audio_in.blocksize);
    /* start capture */
    priv->audio_ringbuffer = calloc(1, priv->audio_buffer_size);
    if (!priv->audio_ringbuffer) {
        mp_msg(MSGT_RADIO, MSGL_ERR, MSGTR_RADIO_AllocateBufferFailed,priv->audio_in.blocksize, priv->audio_buffer_size, strerror(errno));
        return STREAM_ERROR;
    }
    priv->audio_head = 0;
    priv->audio_tail = 0;
    priv->audio_cnt = 0;
    priv->audio_drop = 0;


    priv->audio_inited = 1;

    return STREAM_OK;
}
#endif //USE_RADIO_CAPTURE

/*-------------------------------------------------------------------------
 for call from mplayer.c
--------------------------------------------------------------------------*/
/*****************************************************************
 * \brief public wrapper for set_frequency
 * \parameter frequency frequency in MHz
 * \return 1 if success,0 - otherwise
 */
int radio_set_freq(struct stream_st *stream, float frequency){
    radio_priv_t* priv=(radio_priv_t*)stream->priv;

    if (set_frequency(priv,frequency)!=STREAM_OK){
        return 0;
    }
    if (get_frequency(priv,&frequency)!=STREAM_OK){
        return 0;
    }
    mp_msg(MSGT_RADIO, MSGL_V, MSGTR_RADIO_CurrentFreq,frequency);
    return 1;
}

/*****************************************************************
 * \brief step channel up or down
 * \parameter direction RADIO_CHANNEL_LOWER - go to prev channel,RADIO_CHANNEL_HIGHER - to next
 * \return 1 if success,0 - otherwise
 *
 *  if radio_param_channel is NULL function prints error message and does nothing, otherwise
 *  changes channel to prev or next in list
 */
int radio_step_channel(struct stream_st *stream, int direction) {
    radio_priv_t* priv=(radio_priv_t*)stream->priv;

    if (priv->radio_channel_list) {
        switch (direction){
            case  RADIO_CHANNEL_HIGHER:
                if (priv->radio_channel_current->next)
                    priv->radio_channel_current = priv->radio_channel_current->next;
                else
                    priv->radio_channel_current = priv->radio_channel_list;
                if(!radio_set_freq(stream,priv->radio_channel_current->freq))
                    return 0;
                mp_msg(MSGT_RADIO, MSGL_V, MSGTR_RADIO_SelectedChannel,
                    priv->radio_channel_current->index, priv->radio_channel_current->name,
                    priv->radio_channel_current->freq);
            break;
            case RADIO_CHANNEL_LOWER:
                if (priv->radio_channel_current->prev)
                    priv->radio_channel_current = priv->radio_channel_current->prev;
                else
                    while (priv->radio_channel_current->next)
                        priv->radio_channel_current = priv->radio_channel_current->next;
                if(!radio_set_freq(stream,priv->radio_channel_current->freq))
                    return 0;
                mp_msg(MSGT_RADIO, MSGL_V, MSGTR_RADIO_SelectedChannel,
                priv->radio_channel_current->index, priv->radio_channel_current->name,
                priv->radio_channel_current->freq);
            break;
        }
    }else
        mp_msg(MSGT_RADIO, MSGL_ERR, MSGTR_RADIO_ChangeChannelNoChannelList);
    return 1;
}

/*****************************************************************
 * \brief change channel to one with given index
 * \parameter channel string, containing channel number
 * \return 1 if success,0 - otherwise
 *
 *  if radio_param_channel is NULL function prints error message and does nothing, otherwise
 *  changes channel to given
 */
int radio_set_channel(struct stream_st *stream, char *channel) {
    radio_priv_t* priv=(radio_priv_t*)stream->priv;
    int i, channel_int;
    radio_channels_t* tmp;

    if (priv->radio_channel_list) {
        channel_int = atoi(channel);
        tmp = priv->radio_channel_list;
        for (i = 1; i < channel_int; i++)
            if (tmp->next)
                tmp = tmp->next;
            else
                break;
        if (tmp->index!=channel_int){
            mp_msg(MSGT_RADIO,MSGL_ERR,MSGTR_RADIO_WrongChannelNumberInt,channel_int);
            return 0;
        }
        priv->radio_channel_current=tmp;
        mp_msg(MSGT_RADIO, MSGL_V, MSGTR_RADIO_SelectedChannel, priv->radio_channel_current->index,
            priv->radio_channel_current->name, priv->radio_channel_current->freq);
        if(!radio_set_freq(stream, priv->radio_channel_current->freq))
            return 0;
    } else
        mp_msg(MSGT_RADIO, MSGL_ERR, MSGTR_RADIO_ChangeChannelNoChannelList);
    return 1;
}

/*****************************************************************
 * \brief get current channel's name
 * \return pointer to string, containing current channel's name
 *
 *  NOTE: return value may be NULL (e.g. when channel list not initialized)
 */
char* radio_get_channel_name(struct stream_st *stream){
    radio_priv_t* priv=(radio_priv_t*)stream->priv;
    if (priv->radio_channel_current) {
        return priv->radio_channel_current->name;
    }
    return NULL;
}

/*****************************************************************
 * \brief fills given buffer with audio data
 * \return number of bytes, written into buffer
 */
static int fill_buffer_s(struct stream_st *s, char* buffer, int max_len){
    radio_priv_t* priv=(radio_priv_t*)s->priv;
    int len=max_len;

#ifdef USE_RADIO_CAPTURE
    if (priv->do_capture){
        len=grab_audio_frame(priv, buffer,max_len);
    }
    else
#endif
    memset(buffer,0,len);
    return len;
}

/*****************************************************************
 * Stream initialization
 * \return STREAM_OK if success, STREAM_ERROR otherwise
 */
static int open_s(stream_t *stream,int mode, void* opts, int* file_format) {
    struct stream_priv_s* p=(struct stream_priv_s*)opts;
    radio_priv_t* priv;
    float frequency=0;

    if (strncmp("radio://",stream->url,8) != 0)
        return STREAM_UNSUPORTED;

    if(mode != STREAM_READ)
        return STREAM_UNSUPORTED;

    priv=malloc(sizeof(radio_priv_t));

    if (!priv)
        return STREAM_ERROR;


    memset(priv,0,sizeof(radio_priv_t));

#ifdef USE_RADIO_CAPTURE
    if (p->capture && strncmp("capture",p->capture,7)==0)
        priv->do_capture=1;
    else
        priv->do_capture=0;
#endif



    if (strncmp(radio_param_driver,"default",7)==0)
#ifdef HAVE_RADIO_V4L2
        priv->driver=RADIO_DRIVER_V4L2;
#else
        priv->driver=RADIO_DRIVER_V4L;
#endif
    else
#ifdef HAVE_RADIO_V4L2
    if (strncmp(radio_param_driver,"v4l2",4)==0)
        priv->driver=RADIO_DRIVER_V4L2;
    else
#endif
#ifdef HAVE_RADIO_V4L
    if (strncmp(radio_param_driver,"v4l",3)==0)
        priv->driver=RADIO_DRIVER_V4L;
    else
#endif
    priv->driver=RADIO_DRIVER_UNKNOWN;


    switch(priv->driver){
        case RADIO_DRIVER_V4L:
            mp_msg(MSGT_RADIO, MSGL_INFO, MSGTR_RADIO_DriverV4L);
            break;
        case RADIO_DRIVER_V4L2:
            mp_msg(MSGT_RADIO, MSGL_INFO, MSGTR_RADIO_DriverV4L2);
            break;
        default:
            mp_msg(MSGT_RADIO, MSGL_INFO, MSGTR_RADIO_DriverUnknownStr,radio_param_driver);
            close_s(stream);
            return STREAM_ERROR;
    }

    stream->type = STREAMTYPE_RADIO;
    /* using rawaudio demuxer */
    *file_format =  DEMUXER_TYPE_RAWAUDIO;
    stream->flags = STREAM_READ;

    priv->radio_fd=-1;

    stream->start_pos=0;
    stream->end_pos=0;
    stream->priv=priv;
    stream->close=close_s;
    stream->fill_buffer=fill_buffer_s;

    priv->radio_fd = open(radio_param_device, O_RDWR);
    if (priv->radio_fd < 0) {
        mp_msg(MSGT_RADIO, MSGL_ERR, MSGTR_RADIO_UnableOpenDevice,
            radio_param_device, strerror(errno));
        close_s(stream);
        return STREAM_ERROR;
    }
    mp_msg(MSGT_RADIO, MSGL_V, MSGTR_RADIO_RadioDevice, priv->radio_fd,radio_param_device);
    fcntl(priv->radio_fd, F_SETFD, FD_CLOEXEC);

    set_volume(priv,0);

    if (init_frac(priv)!=STREAM_OK){
        close_s(stream);
        return STREAM_ERROR;
    };

    if (parse_channels(priv,p->radio_param_freq_channel,&frequency)!=STREAM_OK){
        close_s(stream);
        return STREAM_ERROR;
    }

    if (frequency==0){
        mp_msg(MSGT_RADIO, MSGL_ERR, MSGTR_RADIO_WrongFreq,frequency);
        close_s(stream);
        return STREAM_ERROR;
    }else
        mp_msg(MSGT_RADIO, MSGL_INFO, MSGTR_RADIO_UsingFreq,frequency);

    if(set_frequency(priv,frequency)!=STREAM_OK){
        close_s(stream);
        return STREAM_ERROR;
    }


    if (init_audio(priv)!=STREAM_OK){
        close_s(stream);
        return STREAM_ERROR;
    }

#if defined(USE_RADIO_CAPTURE) && defined(USE_STREAM_CACHE)
    if(priv->do_capture){
        //5 second cache
        if(!stream_enable_cache(stream,5*priv->audio_in.samplerate*priv->audio_in.channels*
                priv->audio_in.bytes_per_sample,2*priv->audio_in.samplerate*priv->audio_in.channels*
                priv->audio_in.bytes_per_sample,priv->audio_in.blocksize)) {
            mp_msg(MSGT_RADIO, MSGL_ERR, MSGTR_RADIO_StreamEnableCacheFailed,strerror(errno));
            close_s(stream);
            return STREAM_ERROR;
        }
    }
#endif

    set_volume(priv,radio_param_volume);

    return STREAM_OK;
}

/*****************************************************************
 * Close stream. Clear structures.
 */
static void close_s(struct stream_st * stream){
    radio_priv_t* priv=(radio_priv_t*)stream->priv;
    radio_channels_t * tmp;
    if (!priv) return;

#ifdef USE_RADIO_CAPTURE
    if(priv->audio_ringbuffer){
        free(priv->audio_ringbuffer);
        priv->audio_ringbuffer=NULL;
    }

    priv->do_capture=0;
#endif

    while (priv->radio_channel_list) {
        tmp=priv->radio_channel_list;
        priv->radio_channel_list=priv->radio_channel_list->next;
        free(tmp);
    }
    priv->radio_channel_current=NULL;
    priv->radio_channel_list=NULL;

    if (priv->radio_fd>0){
        close(priv->radio_fd);
    }

    free(priv);
    stream->priv=NULL;
}

stream_info_t stream_info_radio = {
    "Radio stream",
    "Radio",
    "Vladimir Voroshilov",
    "In development",
    open_s,
    { "radio", NULL },
    &stream_opts,
    1 // Urls are an option string
};


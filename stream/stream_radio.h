#ifndef _H_STREAM_RADIO_
#define _H_STREAM_RADIO_

#ifdef USE_RADIO
#define RADIO_CHANNEL_LOWER 1
#define RADIO_CHANNEL_HIGHER 2

extern char *radio_param_device;
extern char *radio_param_driver;
extern char **radio_param_channels;
extern int radio_param_volume;
extern char* radio_param_adevice;
extern int radio_param_arate;
extern int radio_param_achannels;

int radio_set_freq(struct stream_st *stream, float freq);
char* radio_get_channel_name(struct stream_st *stream);
int radio_set_channel(struct stream_st *stream, char *channel);
int radio_step_channel(struct stream_st *stream, int direction);

#endif

#endif

#include <string.h>
#ifndef __MINGW32__
#include <sys/ioctl.h>
#endif
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

#include "config.h"
#include "libao2/audio_out.h"
#include "libaf/af.h"
#include "mixer.h"

#include "help_mp.h"

char * mixer_device=NULL;
char * mixer_channel=NULL;

void mixer_getvolume(mixer_t *mixer, float *l, float *r)
{
  ao_control_vol_t vol;
  *l=0; *r=0;
  if(mixer->audio_out){
    if(CONTROL_OK != mixer->audio_out->control(AOCONTROL_GET_VOLUME,&vol)) {
      if (!mixer->afilter)
        return;
      else {
        float db_vals[AF_NCH];
        if (!af_control_any_rev(mixer->afilter,
               AF_CONTROL_VOLUME_LEVEL | AF_CONTROL_GET, db_vals))
          return;
        af_from_dB (2, db_vals, db_vals, 20.0, -200.0, 60.0);
        vol.left = db_vals[0] * 90.0;
        vol.right = db_vals[1] * 90.0;
      }
    }
    *r=vol.right;
    *l=vol.left;
  }
}

void mixer_setvolume(mixer_t *mixer, float l, float r)
{
  ao_control_vol_t vol;
  vol.right=r; vol.left=l;
  if(mixer->audio_out){
    if(CONTROL_OK != mixer->audio_out->control(AOCONTROL_SET_VOLUME,&vol)) {
      if (mixer->afilter)
        return;
      else {
        // af_volume uses values in dB
        float db_vals[AF_NCH];
        int i;
        // a volume of 90% will give 0 dB (no change)
        // like this, amplification is possible as well
        db_vals[0] = l / 90.0;
        db_vals[1] = r / 90.0;
        for (i = 2; i < AF_NCH; i++) {
          db_vals[i] = (l + r) / 180.0;
        }
        af_to_dB (AF_NCH, db_vals, db_vals, 20.0);
        if (!af_control_any_rev(mixer->afilter,
               AF_CONTROL_VOLUME_LEVEL | AF_CONTROL_SET, db_vals)) {
          mp_msg(MSGT_GLOBAL, MSGL_HINT, MSGTR_NeedAfVolume);
          return;
	}
      }
    }
  }
 mixer->muted=0;
}

void mixer_incvolume(mixer_t *mixer)
{
 float mixer_l, mixer_r;
 mixer_getvolume(mixer, &mixer_l, &mixer_r);
 mixer_l += mixer->volstep;
 if ( mixer_l > 100 ) mixer_l = 100;
 mixer_r += mixer->volstep;
 if ( mixer_r > 100 ) mixer_r = 100;
 mixer_setvolume(mixer, mixer_l, mixer_r);
}

void mixer_decvolume(mixer_t *mixer)
{
 float mixer_l, mixer_r;
 mixer_getvolume(mixer, &mixer_l, &mixer_r);
 mixer_l -= mixer->volstep;
 if ( mixer_l < 0 ) mixer_l = 0;
 mixer_r -= mixer->volstep;
 if ( mixer_r < 0 ) mixer_r = 0;
 mixer_setvolume(mixer, mixer_l, mixer_r);
}

void mixer_getbothvolume(mixer_t *mixer, float *b)
{
 float mixer_l, mixer_r;
 mixer_getvolume(mixer, &mixer_l, &mixer_r);
 *b = ( mixer_l + mixer_r ) / 2;
}

void mixer_mute(mixer_t *mixer)
{
 if (mixer->muted) mixer_setvolume(mixer, mixer->last_l, mixer->last_r);
  else
   { 
    mixer_getvolume(mixer, &mixer->last_l, &mixer->last_r);
    mixer_setvolume(mixer, 0, 0);
    mixer->muted=1;
   }
}

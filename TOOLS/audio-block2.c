// This small util discovers your audio driver's behaviour

#define OUTBURST 512
//#define OUTBURST 4096


#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/soundcard.h>

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
                     

// Returns current time in microseconds
unsigned int GetTimer(){
  struct timeval tv;
  struct timezone tz;
//  float s;
  gettimeofday(&tv,&tz);
//  s=tv.tv_usec;s*=0.000001;s+=tv.tv_sec;
  return (tv.tv_sec*1000000+tv.tv_usec);
}  

static unsigned char a_buffer[OUTBURST];

int main(){
  int audio_buffer_size=0;
  int r;
  int xxx=1024*2;
  int audio_fd;
  char *dsp="/dev/dsp";
  unsigned int t0,t1,t2;

  audio_fd=open(dsp, O_WRONLY);
  if(audio_fd<0){
    printf("Can't open audio device %s\n",dsp);
    return 1;
  }
  
  r=AFMT_S16_LE;ioctl (audio_fd, SNDCTL_DSP_SETFMT, &r);
  r=1; ioctl (audio_fd, SNDCTL_DSP_STEREO, &r);
  r=44100; if(ioctl (audio_fd, SNDCTL_DSP_SPEED, &r)==-1)
      printf("audio_setup: your card doesn't support %d Hz samplerate\n",r);

  t0=t1=GetTimer();

while(xxx-->0){
      char c='B';
      fd_set rfds;
      struct timeval tv;
      FD_ZERO(&rfds); FD_SET(audio_fd,&rfds);
      tv.tv_sec=0; tv.tv_usec = 0;
      if(select(audio_fd+1, NULL, &rfds, NULL, &tv)) c=' ';

    r=write(audio_fd,a_buffer,OUTBURST);
    t2=GetTimer();
    if(r<0) printf("Error writting to device\n"); else
    if(r==0) printf("EOF writting to device???\n"); else {
      printf("%c %6.3f %6.3f  [%6d] writting %3d of %3d bytes in %7d us\n",c,
        (float)audio_buffer_size/(44100.0f*4.0f),(float)(t1-t0)*0.000001f,
        audio_buffer_size,r,OUTBURST,t2-t1);
      audio_buffer_size+=r;
    }
    t1=t2;
}

close(audio_fd);

return 0;
}



#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "config.h"

#ifdef USE_REALCODECS

#include <stddef.h>
#include <dlfcn.h>

#include "ad_internal.h"

static ad_info_t info =  {
	"RealAudio decoder",
	"realaud",
	"A'rpi",
	"Florian Schneider",
	"binary real audio codecs"
};

LIBAD_EXTERN(realaud)

static void *handle=NULL;

void *__builtin_new(unsigned long size) {
	return malloc(size);
}

#if defined(__FreeBSD__) || defined(__NetBSD__)
void *__ctype_b=NULL;
#endif

static unsigned long (*raCloseCodec)(void*);
static unsigned long (*raDecode)(void*, char*,unsigned long,char*,unsigned int*,long);
static unsigned long (*raFlush)(unsigned long,unsigned long,unsigned long);
static unsigned long (*raFreeDecoder)(void*);
static void*         (*raGetFlavorProperty)(void*,unsigned long,unsigned long,int*);
//static unsigned long (*raGetNumberOfFlavors2)(void);
static unsigned long (*raInitDecoder)(void*, void*);
static unsigned long (*raOpenCodec2)(void*);
static unsigned long (*raSetFlavor)(void*,unsigned long);
//static void  (*raSetDLLAccessPath)(unsigned long);
static void  (*raSetPwd)(char*,char*);

typedef struct {
    int samplerate;
    short bits;
    short channels;
    int unk1;
    int unk2;
    int packetsize;
    int unk3;
    void* unk4;
} ra_init_t;

static int preinit(sh_audio_t *sh){
  // let's check if the driver is available, return 0 if not.
  // (you should do that if you use external lib(s) which is optional)
  unsigned int result;
  int len=0;
  void* prop;
  char path[4096];
  sprintf(path, REALCODEC_PATH "/%s", sh->codec->dll);
  handle = dlopen (path, RTLD_LAZY);
  if(!handle){
      mp_msg(MSGT_DECAUDIO,MSGL_WARN,"Cannot open dll: %s\n",dlerror());
      return 0;
  }

    raCloseCodec = dlsym(handle, "RACloseCodec");
    raDecode = dlsym(handle, "RADecode");
    raFlush = dlsym(handle, "RAFlush");
    raFreeDecoder = dlsym(handle, "RAFreeDecoder");
    raGetFlavorProperty = dlsym(handle, "RAGetFlavorProperty");
    raOpenCodec2 = dlsym(handle, "RAOpenCodec2");
    raInitDecoder = dlsym(handle, "RAInitDecoder");
    raSetFlavor = dlsym(handle, "RASetFlavor");
//    raSetDLLAccessPath = dlsym(handle, "SetDLLAccessPath");
    raSetPwd = dlsym(handle, "RASetPwd"); // optional, used by SIPR
    
  if(!raCloseCodec || !raDecode || !raFlush || !raFreeDecoder ||
     !raGetFlavorProperty || !raOpenCodec2 || !raSetFlavor ||
     /*!raSetDLLAccessPath ||*/ !raInitDecoder){
      mp_msg(MSGT_DECAUDIO,MSGL_WARN,"Cannot resolve symbols - incompatible dll\n");
      return 0;
  }

    result=raOpenCodec2(&sh->context);
    if(result){
      mp_msg(MSGT_DECAUDIO,MSGL_WARN,"Decoder open failed, error code: 0x%X\n",result);
      return 0;
    }

  sh->samplerate=sh->wf->nSamplesPerSec;
  sh->samplesize=sh->wf->wBitsPerSample/8;
  sh->channels=sh->wf->nChannels;

  { 
    ra_init_t init_data={
	sh->wf->nSamplesPerSec,sh->wf->wBitsPerSample,sh->wf->nChannels,
	100, // ???
	((short*)(sh->wf+1))[0],  // subpacket size
	sh->wf->nBlockAlign,
	16, // ??
	((char*)(sh->wf+1))+6+8
    };
    result=raInitDecoder(sh->context,&init_data);
    if(result){
      mp_msg(MSGT_DECAUDIO,MSGL_WARN,"Decoder init failed, error code: 0x%X\n",result);
      return 0;
    }
  }

    if(raSetPwd){
	// used by 'SIPR'
	raSetPwd(sh->context,"Ardubancel Quazanga"); // set password... lol.
    }
  
    result=raSetFlavor(sh->context,((short*)(sh->wf+1))[2]);
    if(result){
      mp_msg(MSGT_DECAUDIO,MSGL_WARN,"Decoder flavor setup failed, error code: 0x%X\n",result);
      return 0;
    }

    prop=raGetFlavorProperty(sh->context,((short*)(sh->wf+1))[2],0,&len);
    mp_msg(MSGT_DECAUDIO,MSGL_INFO,"Audio codec: [%d] %s\n",((short*)(sh->wf+1))[2],prop);

    prop=raGetFlavorProperty(sh->context,((short*)(sh->wf+1))[2],1,&len);
    sh->i_bps=((*((int*)prop))+4)/8;
    mp_msg(MSGT_DECAUDIO,MSGL_INFO,"Audio bitrate: %5.3f kbit/s (%d bps)  \n",(*((int*)prop))*0.001f,sh->i_bps);

//    prop=raGetFlavorProperty(sh->context,((short*)(sh->wf+1))[2],0x13,&len);
//    mp_msg(MSGT_DECAUDIO,MSGL_INFO,"Samples/block?: %d  \n",(*((int*)prop)));

  sh->audio_out_minsize=128000; // no idea how to get... :(
  sh->audio_in_minsize=((short*)(sh->wf+1))[1]*sh->wf->nBlockAlign;
  
  return 1; // return values: 1=OK 0=ERROR
}

static int init(sh_audio_t *sh_audio){
  // initialize the decoder, set tables etc...

  // you can store HANDLE or private struct pointer at sh->context
  // you can access WAVEFORMATEX header at sh->wf
  
  // set sample format/rate parameters if you didn't do it in preinit() yet.

  return 1; // return values: 1=OK 0=ERROR
}

static void uninit(sh_audio_t *sh){
  // uninit the decoder etc...
  // again: you don't have to free() a_in_buffer here! it's done by the core.
}

static unsigned char sipr_swaps[38][2]={
    {0,63},{1,22},{2,44},{3,90},{5,81},{7,31},{8,86},{9,58},{10,36},{12,68},
    {13,39},{14,73},{15,53},{16,69},{17,57},{19,88},{20,34},{21,71},{24,46},
    {25,94},{26,54},{28,75},{29,50},{32,70},{33,92},{35,74},{38,85},{40,56},
    {42,87},{43,65},{45,59},{48,79},{49,93},{51,89},{55,95},{61,76},{67,83},
    {77,80} };

static int decode_audio(sh_audio_t *sh,unsigned char *buf,int minlen,int maxlen){
  int result;
  int len=-1;
  int sps=((short*)(sh->wf+1))[0];
  int w=sh->wf->nBlockAlign; // 5
  int h=((short*)(sh->wf+1))[1];

//  printf("bs=%d  sps=%d  w=%d h=%d \n",sh->wf->nBlockAlign,sps,w,h);
  
#if 1
  if(sh->a_in_buffer_len<=0){
      // fill the buffer!
    if(!sps){
      // 'sipr' way
      int j,n;
      int bs=h*w*2/96; // nibbles per subpacket
      unsigned char *p=sh->a_in_buffer;
      demux_read_data(sh->ds, p, h*w);
      for(n=0;n<38;n++){
          int i=bs*sipr_swaps[n][0];
          int o=bs*sipr_swaps[n][1];
	  // swap nibbles of block 'i' with 'o'      TODO: optimize
	  for(j=0;j<bs;j++){
	      int x=(i&1) ? (p[(i>>1)]>>4) : (p[(i>>1)]&15);
	      int y=(o&1) ? (p[(o>>1)]>>4) : (p[(o>>1)]&15);
	      if(o&1) p[(o>>1)]=(p[(o>>1)]&0x0F)|(x<<4);
	        else  p[(o>>1)]=(p[(o>>1)]&0xF0)|x;
	      if(i&1) p[(i>>1)]=(p[(i>>1)]&0x0F)|(y<<4);
	        else  p[(i>>1)]=(p[(i>>1)]&0xF0)|y;
	      ++i;++o;
	  }
      }
      sh->a_in_buffer_size=
      sh->a_in_buffer_len=w*h;
    } else {
      // 'cook' way
      int x,y;
      w/=sps;
      for(y=0;y<h;y++)
        for(x=0;x<w;x++){
	    demux_read_data(sh->ds, sh->a_in_buffer+sps*(h*x+((h+1)/2)*(y&1)+(y>>1)), sps);
	}
      sh->a_in_buffer_size=
      sh->a_in_buffer_len=w*h*sps;
    }
  }

#else
  if(sh->a_in_buffer_len<=0){
      // fill the buffer!
      demux_read_data(sh->ds, sh->a_in_buffer, sh->wf->nBlockAlign);
      sh->a_in_buffer_size=
      sh->a_in_buffer_len=sh->wf->nBlockAlign;
  }
#endif
  
  result=raDecode(sh->context, sh->a_in_buffer+sh->a_in_buffer_size-sh->a_in_buffer_len, sh->wf->nBlockAlign,
       buf, &len, -1);
  sh->a_in_buffer_len-=sh->wf->nBlockAlign;
  
//  printf("radecode: %d bytes, res=0x%X  \n",len,result);

  return len; // return value: number of _bytes_ written to output buffer,
              // or -1 for EOF (or uncorrectable error)
}

static int control(sh_audio_t *sh,int cmd,void* arg, ...){
    // various optional functions you MAY implement:
    switch(cmd){
      case ADCTRL_RESYNC_STREAM:
        // it is called once after seeking, to resync.
	// Note: sh_audio->a_in_buffer_len=0; is done _before_ this call!
	return CONTROL_TRUE;
      case ADCTRL_SKIP_FRAME:
        // it is called to skip (jump over) small amount (1/10 sec or 1 frame)
	// of audio data - used to sync audio to video after seeking
	// if you don't return CONTROL_TRUE, it will defaults to:
	//      ds_fill_buffer(sh_audio->ds);  // skip 1 demux packet
	return CONTROL_TRUE;
    }
  return CONTROL_UNKNOWN;
}

#endif

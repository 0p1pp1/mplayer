// #define NEWBUFFERING
//#define DEBUG_RESYNC

/* 1 frame = 4608 byte PCM */

#ifdef __GNUC__
#define LOCAL static inline
#else
#define LOCAL static _inline
#endif

//#undef LOCAL
//#define LOCAL

#include        <stdlib.h>
#include        <stdio.h>
#include        <string.h>
#include        <signal.h>
#include        <math.h>

#define real float
// #define int long

#include "mpg123.h"
#include "huffman.h"
#include "mp3.h"
#include "bswap.h"
#include "d_cpu.h"

#include "fastmemcpy.h"
//static FILE* mp3_file=NULL;

int MP3_frames=0;
int MP3_eof=0;
int MP3_pause=0;
int MP3_filesize=0;
int MP3_fpos=0;      // current file position
int MP3_framesize=0; // current framesize
int MP3_bitrate=0;   // current bitrate
int MP3_samplerate=0;  // current samplerate
int MP3_resync=0;
int MP3_channels=0;
int MP3_bps=2;

static long outscale  = 32768;
#include "tabinit.c"

extern int mplayer_audio_read(char *buf,int size);

#if 1
LOCAL int mp3_read(char *buf,int size){
//  int len=fread(buf,1,size,mp3_file);
  int len=mplayer_audio_read(buf,size);
  if(len>0) MP3_fpos+=len;
//  if(len!=size) MP3_eof=1;
  return len;
}
#else
extern int mp3_read(char *buf,int size);
#endif

//void mp3_seek(int pos){
//  fseek(mp3_file,pos,SEEK_SET);
//  return (MP3_fpos=ftell(mp3_file));
//}

/*       Frame reader           */

#define MAXFRAMESIZE 1280
#define MAXFRAMESIZE2 (512+MAXFRAMESIZE)

static int fsizeold=0,ssize=0;
static unsigned char bsspace[2][MAXFRAMESIZE2]; /* !!!!! */
static unsigned char *bsbufold=bsspace[0]+512;
static unsigned char *bsbuf=bsspace[1]+512;
static int bsnum=0;

static int bitindex;
static unsigned char *wordpointer;
static int bitsleft;

unsigned char *pcm_sample;   /* az outbuffer CIME */
int pcm_point = 0;           /* ez az outbuffer pozicioja */

static struct frame fr;

static int tabsel_123[2][3][16] = {
   { {0,32,64,96,128,160,192,224,256,288,320,352,384,416,448,},
     {0,32,48,56, 64, 80, 96,112,128,160,192,224,256,320,384,},
     {0,32,40,48, 56, 64, 80, 96,112,128,160,192,224,256,320,} },

   { {0,32,48,56,64,80,96,112,128,144,160,176,192,224,256,},
     {0,8,16,24,32,40,48,56,64,80,96,112,128,144,160,},
     {0,8,16,24,32,40,48,56,64,80,96,112,128,144,160,} }
};

static long freqs[9] = { 44100, 48000, 32000, 22050, 24000, 16000 , 11025 , 12000 , 8000 };

LOCAL unsigned int getbits(short number_of_bits)
{
  unsigned long rval;
//  if(MP3_frames>=7741) printf("getbits: bits=%d  bitsleft=%d  wordptr=%x\n",number_of_bits,bitsleft,wordpointer);
  if((bitsleft-=number_of_bits)<0) return 0;
  if(!number_of_bits) return 0;
         rval = wordpointer[0];
         rval <<= 8;
         rval |= wordpointer[1];
         rval <<= 8;
         rval |= wordpointer[2];
         rval <<= bitindex;
         rval &= 0xffffff;
         bitindex += number_of_bits;
         rval >>= (24-number_of_bits);
         wordpointer += (bitindex>>3);
         bitindex &= 7;
  return rval;
}


LOCAL unsigned int getbits_fast(short number_of_bits)
{
  unsigned long rval;
//  if(MP3_frames>=7741) printf("getbits_fast: bits=%d  bitsleft=%d  wordptr=%x\n",number_of_bits,bitsleft,wordpointer);
  if((bitsleft-=number_of_bits)<0) return 0;
  if(!number_of_bits) return 0;
  rval = bswap_16(*((unsigned short *)wordpointer));
         rval <<= bitindex;
         rval &= 0xffff;
         bitindex += number_of_bits;
         rval >>= (16-number_of_bits);
         wordpointer += (bitindex>>3);
         bitindex &= 7;
  return rval;
}

LOCAL unsigned int get1bit(void)
{
  unsigned char rval;
//  if(MP3_frames>=7741) printf("get1bit: bitsleft=%d  wordptr=%x\n",bitsleft,wordpointer);
  if((--bitsleft)<0) return 0;
  rval = *wordpointer << bitindex;
  bitindex++;
  wordpointer += (bitindex>>3);
  bitindex &= 7;
  return ((rval>>7)&1);
}

LOCAL void set_pointer(long backstep)
{
//  if(backstep!=512 && backstep>fsizeold)
//    printf("\rWarning! backstep (%d>%d)                                         \n",backstep,fsizeold);
  wordpointer = bsbuf + ssize - backstep;
  if (backstep) memcpy(wordpointer,bsbufold+fsizeold-backstep,backstep);
  bitindex = 0;
  bitsleft+=8*backstep;
//  printf("Backstep %d  (bitsleft=%d)\n",backstep,bitsleft);
}

LOCAL int stream_head_read(unsigned char *hbuf,unsigned long *newhead){
  if(mp3_read(hbuf,4) != 4) return FALSE;
  *newhead = bswap_32(*((unsigned long *)hbuf));
  return TRUE;
}

LOCAL int stream_head_shift(unsigned char *hbuf,unsigned long *head){
  *((unsigned long *)hbuf) >>= 8;
  if(mp3_read(hbuf+3,1) != 1) return 0;
  *head <<= 8;
  *head |= hbuf[3];
  return 1;
}

/*
 * decode a header and write the information
 * into the frame structure
 */
LOCAL int decode_header(struct frame *fr,unsigned long newhead){

    // head_check:
    if( (newhead & 0xffe00000) != 0xffe00000 ||  
        (newhead & 0x0000fc00) == 0x0000fc00) return FALSE;

    fr->lay = 4-((newhead>>17)&3);
//    if(fr->lay!=3) return FALSE;

    if( newhead & ((long)1<<20) ) {
      fr->lsf = (newhead & ((long)1<<19)) ? 0x0 : 0x1;
      fr->mpeg25 = 0;
    } else {
      fr->lsf = 1;
      fr->mpeg25 = 1;
    }

    if(fr->mpeg25)
      fr->sampling_frequency = 6 + ((newhead>>10)&0x3);
    else
      fr->sampling_frequency = ((newhead>>10)&0x3) + (fr->lsf*3);

    if(fr->sampling_frequency>8) return FALSE;  // valid: 0..8

    fr->error_protection = ((newhead>>16)&0x1)^0x1;
    fr->bitrate_index = ((newhead>>12)&0xf);
    fr->padding   = ((newhead>>9)&0x1);
    fr->extension = ((newhead>>8)&0x1);
    fr->mode      = ((newhead>>6)&0x3);
    fr->mode_ext  = ((newhead>>4)&0x3);
    fr->copyright = ((newhead>>3)&0x1);
    fr->original  = ((newhead>>2)&0x1);
    fr->emphasis  = newhead & 0x3;

    MP3_channels = fr->stereo    = (fr->mode == MPG_MD_MONO) ? 1 : 2;

    if(!fr->bitrate_index){
//      fprintf(stderr,"Free format not supported.\n");
      return FALSE;
    }

switch(fr->lay){
  case 2:
    MP3_bitrate=tabsel_123[fr->lsf][1][fr->bitrate_index];
    MP3_samplerate=freqs[fr->sampling_frequency];
    fr->framesize = (long) MP3_bitrate * 144000;
    fr->framesize /= MP3_samplerate;
    MP3_framesize=fr->framesize;
    fr->framesize += fr->padding - 4;
    break;
  case 3:
    if(fr->lsf)
      ssize = (fr->stereo == 1) ? 9 : 17;
    else
      ssize = (fr->stereo == 1) ? 17 : 32;
    if(fr->error_protection) ssize += 2;

    MP3_bitrate=tabsel_123[fr->lsf][2][fr->bitrate_index];
    MP3_samplerate=freqs[fr->sampling_frequency];
    fr->framesize  = (long) MP3_bitrate * 144000;
    fr->framesize /= MP3_samplerate<<(fr->lsf);
    MP3_framesize=fr->framesize;
    fr->framesize += fr->padding - 4;
    break;
  default:
//    fprintf(stderr,"Sorry, unsupported layer type.\n");
    return 0;
}
    if(fr->framesize<=0 || fr->framesize>MAXFRAMESIZE) return FALSE;

    return 1;
}


LOCAL int stream_read_frame_body(int size){

  /* flip/init buffer for Layer 3 */
  bsbufold = bsbuf;
  bsbuf = bsspace[bsnum]+512;
  bsnum = (bsnum + 1) & 1;

  if( mp3_read(bsbuf,size) != size) return 0; // broken frame

  bitindex = 0;
  wordpointer = (unsigned char *) bsbuf;
  bitsleft=8*size;

  return 1;
}


/*****************************************************************
 * read next frame     return number of frames read.
 */
LOCAL int read_frame(struct frame *fr){
  unsigned long newhead;
  unsigned char hbuf[8];
  int skipped,resyncpos;
  int frames=0;

resync:
  skipped=MP3_fpos;
  resyncpos=MP3_fpos;

  set_pointer(512);
  fsizeold=fr->framesize;       /* for Layer3 */
  if(!stream_head_read(hbuf,&newhead)) return 0;
  if(!decode_header(fr,newhead)){
    // invalid header! try to resync stream!
#ifdef DEBUG_RESYNC
    printf("ReSync: searching for a valid header...  (pos=%X)\n",MP3_fpos);
#endif
retry1:
    while(!decode_header(fr,newhead)){
      if(!stream_head_shift(hbuf,&newhead)) return 0;
    }
    resyncpos=MP3_fpos-4;
    // found valid header
#ifdef DEBUG_RESYNC
    printf("ReSync: found valid hdr at %X  fsize=%ld  ",resyncpos,fr->framesize);
#endif
    if(!stream_read_frame_body(fr->framesize)) return 0; // read body
    set_pointer(512);
    fsizeold=fr->framesize;       /* for Layer3 */
    if(!stream_head_read(hbuf,&newhead)) return 0;
    if(!decode_header(fr,newhead)){
      // invalid hdr! go back...
#ifdef DEBUG_RESYNC
      printf("INVALID\n");
#endif
//      mp3_seek(resyncpos+1);
      if(!stream_head_read(hbuf,&newhead)) return 0;
      goto retry1;
    }
#ifdef DEBUG_RESYNC
    printf("OK!\n");
    ++frames;
#endif
  }

  skipped=resyncpos-skipped;
//  if(skipped && !MP3_resync) printf("\r%d bad bytes skipped  (resync at 0x%X)                          \n",skipped,resyncpos);

//  printf("%8X [%08X] %d %d (%d)%s%s\n",MP3_fpos-4,newhead,fr->framesize,fr->mode,fr->mode_ext,fr->error_protection?" CRC":"",fr->padding?" PAD":"");

  /* read main data into memory */
  if(!stream_read_frame_body(fr->framesize)){
    printf("\nBroken frame at 0x%X                                                  \n",resyncpos);
    return 0;
  }
  ++frames;

  if(MP3_resync){
    MP3_resync=0;
    if(frames==1) goto resync;
  }

  return frames;
}

#include "layer2.c"
#include "layer3.c"

/******************************************************************************/
/*           PUBLIC FUNCTIONS                  */
/******************************************************************************/

static int tables_done_flag=0;

/* It's hidden from gcc in assembler */
extern void dct64_MMX( void );
extern void dct64_MMX_3dnow( void );
extern void dct64_MMX_3dnowex( void );
void (*dct64_MMX_func)( void );

// Init decoder tables.  Call first, once!
#ifdef USE_FAKE_MONO
void MP3_Init(int fakemono){
#else
void MP3_Init(){
#endif
#ifdef ARCH_X86
    _CpuID=CpuDetect();
    _i586=ipentium();
#ifndef HAVE_MMX
    _i586 &= 1;
#endif
    _3dnow=a3dnow();
#ifndef HAVE_3DNOW
    _3dnow = 0;
#endif
#ifndef HAVE_3DNOWEX
    _3dnow &= 1;
#endif
    _isse=isse();
#ifndef HAVE_SSE
    _isse = 0;
#endif
#ifndef HAVE_SSE2
    _isse &= 1;
#endif
    _has_mmx=_i586>1||_3dnow||_isse;
    printf( "mp3lib: Processor ID: %x\n",_CpuID );
    if(_i586&&!_3dnow&&!_isse)
      printf( "mp3lib: Using Pentium%s optimized decore.\n",(_i586>1?"-MMX":""));
    else
    if(_isse) 
    /*
       Note: It's ok, Since K8 will have SSE2 support and will much faster
       of P4 ;) 
     */
//      printf( "mp3lib: Using SSE%s! optimized decore.\n",(_isse>1?"2":""));
      printf( "mp3lib: Using Pentium%s optimized decore.\n",(_i586>1?"-MMX":""));
    else
    if(_3dnow)
      printf( "mp3lib: Using AMD 3dnow%s! optimized decore.\n",(_3dnow>1?"-dsp(k7)":""));
#else
      _CpuID = _i586 = _3dnow = _isse = _has_mmx = 0;
      printf( "mp3lib: Using generic decore.\n");
#endif
/* Use it for any MMX cpu */
   if(_has_mmx)	make_decode_tables_MMX(outscale);
   else		make_decode_tables(outscale);
#ifdef USE_FAKE_MONO
    if (fakemono == 1)
        fr.synth=synth_1to1_l;
    else if (fakemono == 2)
        fr.synth=synth_1to1_r;
    else
        fr.synth=synth_1to1;
#else
    fr.synth=synth_1to1;
#endif
    fr.synth_mono=synth_1to1_mono2stereo;
    fr.down_sample=0;
    fr.down_sample_sblimit = SBLIMIT>>(fr.down_sample);
    init_layer2();
    init_layer3(fr.down_sample_sblimit);
    tables_done_flag=1;

    dct36_func=dct36;
  if(_isse)
  {
    synth_func=synth_1to1_MMX;
    dct64_MMX_func=dct64_MMX;
  }    
  else
  if ( _3dnow > 1 )
  {
     synth_func=synth_1to1_MMX;
     dct36_func=dct36_3dnowex;
     dct64_MMX_func=dct64_MMX_3dnowex;
  }
  else
  if ( _3dnow )
  {
    synth_func=synth_1to1_MMX;
    dct36_func=dct36_3dnow;
    dct64_MMX_func=dct64_MMX_3dnow;
  }
  else
  if ( _i586 > 1)
  {
    synth_func=synth_1to1_MMX;
    dct64_MMX_func=dct64_MMX;
  }    
  else
  if ( _i586 )
  {
    synth_func=synth_1to1_pent;
  }    
  else
  {
    synth_func = NULL;
  }
}

#if 0

void MP3_Close(){
  MP3_eof=1;
  if(mp3_file) fclose(mp3_file);
  mp3_file=NULL;
}

// Open a file, init buffers. Call once per file!
int MP3_Open(char *filename,int buffsize){
  MP3_eof=1;   // lock decoding
  MP3_pause=1; // lock playing
  if(mp3_file) MP3_Close(); // close prev. file
  MP3_frames=0;

  mp3_file=fopen(filename,"rb");
//  printf("MP3_Open: file='%s'",filename);
//  if(!mp3_file){ printf(" not found!\n"); return 0;} else printf("Ok!\n");
  if(!mp3_file) return 0;

  MP3_filesize=MP3_PrintTAG();
  fseek(mp3_file,0,SEEK_SET);

  MP3_InitBuffers(buffsize);
  if(!tables_done_flag) MP3_Init();
  MP3_eof=0;  // allow decoding
  MP3_pause=0; // allow playing
  return MP3_filesize;
}

#endif

// Read & decode a single frame. Called by sound driver.
int MP3_DecodeFrame(unsigned char *hova,short single){
   pcm_sample = hova;
   pcm_point = 0;
   if(!read_frame(&fr))return(0);
   if(single==-2){ set_pointer(512); return(1); }
   if(fr.error_protection) getbits(16); /* skip crc */
   fr.single=single;
   switch(fr.lay){
     case 2: do_layer2(&fr,single);break;
     case 3: do_layer3(&fr,single);break;
   }
//   ++MP3_frames;
   return(pcm_point?pcm_point:2);
}

#if 0

// Prints last frame header in ascii.
void MP3_PrintHeader(){
        static char *modes[4] = { "Stereo", "Joint-Stereo", "Dual-Channel", "Single-Channel" };
        static char *layers[4] = { "???" , "I", "II", "III" };

        printf("\rMPEG %s, Layer %s, %ld Hz %d kbit %s, BPF : %ld\n",
                fr.mpeg25 ? "2.5" : (fr.lsf ? "2.0" : "1.0"),
                layers[fr.lay],freqs[fr.sampling_frequency],
    tabsel_123[fr.lsf][fr.lay-1][fr.bitrate_index],
                modes[fr.mode],fr.framesize+4);
        printf("Channels: %d, copyright: %s, original: %s, CRC: %s, emphasis: %d.\n\n",
                fr.stereo,fr.copyright?"Yes":"No",
                fr.original?"Yes":"No",fr.error_protection?"Yes":"No",
                fr.emphasis);
}

#include "genre.h"

// Read & print ID3 TAG. Do not call when playing!!!  returns filesize.
int MP3_PrintTAG(){
        struct id3tag {
                char tag[3];
                char title[30];
                char artist[30];
                char album[30];
                char year[4];
                char comment[30];
                unsigned char genre;
        };
        struct id3tag tag;
        char title[31]={0,};
        char artist[31]={0,};
        char album[31]={0,};
        char year[5]={0,};
        char comment[31]={0,};
        char genre[31]={0,};
  int fsize;
  int ret;

  fseek(mp3_file,0,SEEK_END);
  fsize=ftell(mp3_file);
  if(fseek(mp3_file,-128,SEEK_END)) return fsize;
  ret=fread(&tag,128,1,mp3_file);
  if(ret!=1 || tag.tag[0]!='T' || tag.tag[1]!='A' || tag.tag[2]!='G') return fsize;

        strncpy(title,tag.title,30);
        strncpy(artist,tag.artist,30);
        strncpy(album,tag.album,30);
        strncpy(year,tag.year,4);
        strncpy(comment,tag.comment,30);

        if ( tag.genre <= sizeof(genre_table)/sizeof(*genre_table) ) {
                strncpy(genre, genre_table[tag.genre], 30);
        } else {
                strncpy(genre,"Unknown",30);
        }

//      printf("\n");
        printf("Title  : %30s  Artist: %s\n",title,artist);
        printf("Album  : %30s  Year  : %4s\n",album,year);
        printf("Comment: %30s  Genre : %s\n",comment,genre);
        printf("\n");
  return fsize-128;
}

#endif


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"

//#include "stream.h"
//#include "demuxer.h"
//#include "stheader.h"

#include "wine/mmreg.h"
#include "wine/avifmt.h"
#include "wine/vfw.h"

#include "aviwrite.h"

aviwrite_stream_t* aviwrite_new_stream(aviwrite_t *muxer,int type){
    aviwrite_stream_t* s;
    if(muxer->avih.dwStreams>=AVIWRITE_MAX_STREAMS){
	printf("Too many streams! increase AVIWRITE_MAX_STREAMS !\n");
	return NULL;
    }
    s=malloc(sizeof(aviwrite_stream_t));
    memset(s,0,sizeof(aviwrite_stream_t));
    if(!s) return NULL; // no mem!?
    muxer->streams[muxer->avih.dwStreams]=s;
    s->type=type;
    s->id=muxer->avih.dwStreams;
    s->timer=0.0;
    s->size=0;
    switch(type){
    case AVIWRITE_TYPE_VIDEO:
      s->ckid=mmioFOURCC(('0'+s->id/10),('0'+(s->id%10)),'d','c');
      s->h.fccType=streamtypeVIDEO;
      if(!muxer->def_v) muxer->def_v=s;
      break;
    case AVIWRITE_TYPE_AUDIO:
      s->ckid=mmioFOURCC(('0'+s->id/10),('0'+(s->id%10)),'w','b');
      s->h.fccType=streamtypeAUDIO;
      break;
    default:
      printf("WarninG! unknown stream type: %d\n",type);
      return NULL;
    }
    muxer->avih.dwStreams++;
    return s;
}

aviwrite_t* aviwrite_new_muxer(){
    aviwrite_t* muxer=malloc(sizeof(aviwrite_t));
    memset(muxer,0,sizeof(aviwrite_t));
    return muxer;
}

static void write_avi_chunk(FILE *f,unsigned int id,int len,void* data){
fwrite(&id,4,1,f);
fwrite(&len,4,1,f);
if(len>0){
  if(data){
    // DATA
    fwrite(data,len,1,f);
    if(len&1){  // padding
      unsigned char zerobyte=0;
      fwrite(&zerobyte,1,1,f);
    }
  } else {
    // JUNK
    char *avi_junk_data="[= MPlayer junk data! =]";
    if(len&1) ++len; // padding
    while(len>0){
      int l=strlen(avi_junk_data);
      if(l>len) l=len;
      fwrite(avi_junk_data,l,1,f);
      len-=l;
    }
  }
}
}

void aviwrite_write_chunk(aviwrite_t *muxer,aviwrite_stream_t *s, FILE *f,int len,unsigned int flags){

    // add to the index:
    if(muxer->idx_pos>=muxer->idx_size){
	muxer->idx_size+=256; // 4kB
	muxer->idx=realloc(muxer->idx,16*muxer->idx_size);
    }
    muxer->idx[muxer->idx_pos].ckid=s->ckid;
    muxer->idx[muxer->idx_pos].dwFlags=flags; // keyframe?
    muxer->idx[muxer->idx_pos].dwChunkOffset=ftell(f)-(muxer->movi_start-4);
    muxer->idx[muxer->idx_pos].dwChunkLength=len;
    ++muxer->idx_pos;

    // write out the chunk:
    write_avi_chunk(f,s->ckid,len,s->buffer);
    
    // alter counters:
    if(s->h.dwSampleSize){
	// CBR
	s->h.dwLength+=len/s->h.dwSampleSize;
	if(len%s->h.dwSampleSize) printf("Warning! len isn't divisable by samplesize!\n");
    } else {
	// VBR
	s->h.dwLength++;
    }
    s->timer=(double)s->h.dwLength*s->h.dwScale/s->h.dwRate;
    s->size+=len;
    if(len>s->h.dwSuggestedBufferSize) s->h.dwSuggestedBufferSize=len;

}

static void write_avi_list(FILE *f,unsigned int id,int len){
  unsigned int list_id=FOURCC_LIST;
  len+=4; // list fix
  fwrite(&list_id,4,1,f);
  fwrite(&len,4,1,f);
  fwrite(&id,4,1,f);
}

// muxer->streams[i]->wf->cbSize
#define WFSIZE(wf) (sizeof(WAVEFORMATEX)+(((wf)->cbSize)?((wf)->cbSize-2):0))

void aviwrite_write_header(aviwrite_t *muxer,FILE *f){
  unsigned int riff[3];
  int i;
  unsigned int hdrsize;
  // RIFF header:
  riff[0]=mmioFOURCC('R','I','F','F');
  riff[1]=muxer->file_end;  // filesize
  riff[2]=formtypeAVI; // 'AVI '
  fwrite(&riff,12,1,f);

  // update AVI header:
  if(muxer->def_v){
      muxer->avih.dwMicroSecPerFrame=1000000.0*muxer->def_v->h.dwScale/muxer->def_v->h.dwRate;
//      muxer->avih.dwMaxBytesPerSec=1000000; // dummy!!!!! FIXME
//      muxer->avih.dwPaddingGranularity=2; // ???
      muxer->avih.dwFlags|=AVIF_ISINTERLEAVED|AVIF_TRUSTCKTYPE;
      muxer->avih.dwTotalFrames=muxer->def_v->h.dwLength;
//      muxer->avih.dwSuggestedBufferSize=muxer->def_v->h.dwSuggestedBufferSize;
      muxer->avih.dwWidth=muxer->def_v->bih->biWidth;
      muxer->avih.dwHeight=muxer->def_v->bih->biHeight;
  }

  // AVI header:
  hdrsize=sizeof(muxer->avih)+8;
  // calc total header size:
  for(i=0;i<muxer->avih.dwStreams;i++){
      hdrsize+=12; // LIST
      hdrsize+=sizeof(muxer->streams[i]->h)+8; // strh
      switch(muxer->streams[i]->type){
      case AVIWRITE_TYPE_VIDEO:
          hdrsize+=muxer->streams[i]->bih->biSize+8; // strf
	  break;
      case AVIWRITE_TYPE_AUDIO:
          hdrsize+=WFSIZE(muxer->streams[i]->wf)+8; // strf
	  break;
      }
  }
  write_avi_list(f,listtypeAVIHEADER,hdrsize);
  write_avi_chunk(f,ckidAVIMAINHDR,sizeof(muxer->avih),&muxer->avih);

  // stream headers:
  for(i=0;i<muxer->avih.dwStreams;i++){
      hdrsize=sizeof(muxer->streams[i]->h)+8; // strh
      switch(muxer->streams[i]->type){
      case AVIWRITE_TYPE_VIDEO:
          hdrsize+=muxer->streams[i]->bih->biSize+8; // strf
	  break;
      case AVIWRITE_TYPE_AUDIO:
          hdrsize+=WFSIZE(muxer->streams[i]->wf)+8; // strf
	  break;
      }
      write_avi_list(f,listtypeSTREAMHEADER,hdrsize);
      write_avi_chunk(f,ckidSTREAMHEADER,sizeof(muxer->streams[i]->h),&muxer->streams[i]->h); // strh
      switch(muxer->streams[i]->type){
      case AVIWRITE_TYPE_VIDEO:
          write_avi_chunk(f,ckidSTREAMFORMAT,muxer->streams[i]->bih->biSize,muxer->streams[i]->bih);
	  break;
      case AVIWRITE_TYPE_AUDIO:
          write_avi_chunk(f,ckidSTREAMFORMAT,WFSIZE(muxer->streams[i]->wf),muxer->streams[i]->wf);
	  break;
      }
  }

  // JUNK:
  write_avi_chunk(f,ckidAVIPADDING,2048-(ftell(f)&2047)-8,NULL);
  // 'movi' header:
  write_avi_list(f,listtypeAVIMOVIE,muxer->movi_end-ftell(f)-12);
  muxer->movi_start=ftell(f);
}

void aviwrite_write_index(aviwrite_t *muxer,FILE *f){
  muxer->movi_end=ftell(f);
  if(muxer->idx && muxer->idx_pos>0){
      // fixup index entries:
//      int i;
//      for(i=0;i<muxer->idx_pos;i++) muxer->idx[i].dwChunkOffset-=muxer->movi_start-4;
      // write index chunk:
      write_avi_chunk(f,ckidAVINEWINDEX,16*muxer->idx_pos,muxer->idx);
      muxer->avih.dwFlags|=AVIF_HASINDEX;
  }
  muxer->file_end=ftell(f);
}


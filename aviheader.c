
#define MIN(a,b) (((a)<(b))?(a):(b))

void read_avi_header(int index_mode){
sh_audio_t *sh_audio=NULL;
sh_video_t *sh_video=NULL;
int stream_id=-1;
int idxfix_videostream=0;
int idxfix_divx=0;

//---- AVI header:
avi_header.idx_size=0;
while(1){
  int id=stream_read_dword_le(demuxer->stream);
  int chunksize,size2;
  static int last_fccType=0;
  //
  if(stream_eof(demuxer->stream)) break;
  //
  if(id==mmioFOURCC('L','I','S','T')){
    int len=stream_read_dword_le(demuxer->stream)-4; // list size
    id=stream_read_dword_le(demuxer->stream);        // list type
    if(verbose>=2) printf("LIST %.4s  len=%d\n",&id,len);
    if(id==listtypeAVIMOVIE){
      // found MOVI header
      avi_header.movi_start=stream_tell(demuxer->stream);
      avi_header.movi_end=avi_header.movi_start+len;
      if(verbose>=1) printf("Found movie at 0x%X - 0x%X\n",avi_header.movi_start,avi_header.movi_end);
      len=(len+1)&(~1);
      stream_skip(demuxer->stream,len);
    }
    continue;
  }
  size2=stream_read_dword_le(demuxer->stream);
  if(verbose>=2) printf("CHUNK %.4s  len=%d\n",&id,size2);
  chunksize=(size2+1)&(~1);
  switch(id){
    case ckidAVIMAINHDR:          // read 'avih'
      stream_read(demuxer->stream,(char*) &avi_header.avih,MIN(size2,sizeof(avi_header.avih)));
      chunksize-=MIN(size2,sizeof(avi_header.avih));
      if(verbose) print_avih(&avi_header.avih);
      break;
    case ckidSTREAMHEADER: {      // read 'strh'
      AVIStreamHeader h;
      stream_read(demuxer->stream,(char*) &h,MIN(size2,sizeof(h)));
      chunksize-=MIN(size2,sizeof(h));
      ++stream_id;
      if(h.fccType==streamtypeVIDEO){
        sh_video=new_sh_video(stream_id);
        memcpy(&sh_video->video,&h,sizeof(h));
      } else
      if(h.fccType==streamtypeAUDIO){
        sh_audio=new_sh_audio(stream_id);
        memcpy(&sh_audio->audio,&h,sizeof(h));
      }
      last_fccType=h.fccType;
      if(verbose>=1) print_strh(&h);
      break; }
    case ckidSTREAMFORMAT: {      // read 'strf'
      if(last_fccType==streamtypeVIDEO){
        sh_video->bih=calloc((chunksize<sizeof(BITMAPINFOHEADER))?sizeof(BITMAPINFOHEADER):chunksize,1);
//        sh_video->bih=malloc(chunksize); memset(sh_video->bih,0,chunksize);
        if(verbose>=1) printf("found 'bih', %d bytes of %d\n",chunksize,sizeof(BITMAPINFOHEADER));
        stream_read(demuxer->stream,(char*) sh_video->bih,chunksize);
        chunksize=0;
//        sh_video->fps=(float)sh_video->video.dwRate/(float)sh_video->video.dwScale;
//        sh_video->frametime=(float)sh_video->video.dwScale/(float)sh_video->video.dwRate;
//        if(demuxer->video->id==-1) demuxer->video->id=stream_id;
        // IdxFix:
        idxfix_videostream=stream_id;
        switch(sh_video->bih->biCompression){
	case mmioFOURCC('D', 'I', 'V', '3'):
	case mmioFOURCC('d', 'i', 'v', '3'):
	case mmioFOURCC('D', 'I', 'V', '4'):
        case mmioFOURCC('d', 'i', 'v', '4'):
	case mmioFOURCC('D', 'I', 'V', '5'):
	case mmioFOURCC('d', 'i', 'v', '5'):
	case mmioFOURCC('D', 'I', 'V', '6'):
        case mmioFOURCC('d', 'i', 'v', '6'):
	case mmioFOURCC('M', 'P', '4', '3'):
	case mmioFOURCC('m', 'p', '4', '3'):
        case mmioFOURCC('A', 'P', '4', '1'):
          idxfix_divx=1; // we can fix keyframes only for divx coded files!
        }
      } else
      if(last_fccType==streamtypeAUDIO){
        sh_audio->wf=calloc((chunksize<sizeof(WAVEFORMATEX))?sizeof(WAVEFORMATEX):chunksize,1);
//        sh_audio->wf=malloc(chunksize); memset(sh_audio->wf,0,chunksize);
        if(verbose>=1) printf("found 'wf', %d bytes of %d\n",chunksize,sizeof(WAVEFORMATEX));
        stream_read(demuxer->stream,(char*) sh_audio->wf,chunksize);
        chunksize=0;
        if(verbose>=1) print_wave_header(sh_audio->wf);
//        if(demuxer->audio->id==-1) demuxer->audio->id=stream_id;
      }
      break;
    }
    case ckidAVINEWINDEX: if(index_mode){
      avi_header.idx_size=size2>>4;
      if(verbose>=1) printf("Reading INDEX block, %d chunks for %d frames\n",
        avi_header.idx_size,avi_header.avih.dwTotalFrames);
      avi_header.idx=malloc(avi_header.idx_size<<4);
      stream_read(demuxer->stream,(char*)avi_header.idx,avi_header.idx_size<<4);
      chunksize-=avi_header.idx_size<<4;
      if(verbose>=2) print_index();
      break;
    }
  }
  if(chunksize>0) stream_skip(demuxer->stream,chunksize); else
  if(chunksize<0) printf("WARNING!!! chunksize=%d  (id=%.4s)\n",chunksize,&id);
  
}

if(index_mode>=2 || (avi_header.idx_size==0 && index_mode==1)){
  // build index for file:
  stream_reset(demuxer->stream);
  stream_seek(demuxer->stream,avi_header.movi_start);
  
  avi_header.idx_pos=0;
  avi_header.idx=NULL;

  while(1){
    int id,len,skip;
    AVIINDEXENTRY* idx;
    demuxer->filepos=stream_tell(demuxer->stream);
    if(demuxer->filepos>=avi_header.movi_end) break;
    id=stream_read_dword_le(demuxer->stream);
    len=stream_read_dword_le(demuxer->stream);
    if(id==mmioFOURCC('L','I','S','T')){
      id=stream_read_dword_le(demuxer->stream);      // list type
      continue;
    }
    if(stream_eof(demuxer->stream)) break;
    if(avi_header.idx_pos<=avi_header.idx_size){
      avi_header.idx_size+=32;
      avi_header.idx=realloc(avi_header.idx,avi_header.idx_size*sizeof(AVIINDEXENTRY));
      if(!avi_header.idx){avi_header.idx_pos=0; break;} // error!
    }
    idx=&avi_header.idx[avi_header.idx_pos++];
    idx->ckid=id;
    idx->dwFlags=AVIIF_KEYFRAME; // FIXME
    idx->dwChunkOffset=demuxer->filepos;
    idx->dwChunkLength=len;

    // Fix keyframes for DivX files:
    if(idxfix_divx)
      if(avi_stream_id(id)==idxfix_videostream){
        unsigned char c=stream_read_char(demuxer->stream);
//        --skip;
        if(!(c&0x40)) idx->dwFlags=0;
      }
    
    if(verbose>=2) printf("0x%08X  0x%08X %.4s  %X\n",demuxer->filepos,id,&id,idx->dwFlags);
#if 0
    { unsigned char tmp[64];
      int i;
      stream_read(demuxer->stream,tmp,64);
      printf("%.4s",&id);
      for(i=0;i<64;i++) printf(" %02X",tmp[i]);
      printf("\n");
    }
#endif
    skip=(len+1)&(~1); // total bytes in this chunk
    stream_seek(demuxer->stream,8+demuxer->filepos+skip);
  }
  avi_header.idx_size=avi_header.idx_pos;
  printf("AVI: Generated index table for %d chunks!\n",avi_header.idx_size);
}

}

#undef MIN


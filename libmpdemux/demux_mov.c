//  QuickTime MOV file parser by A'rpi
//  additional work by Atmos
//  based on TOOLS/movinfo.c by A'rpi & Al3x
//  compressed header support from moov.c of the openquicktime lib.
//  References: http://openquicktime.sf.net/, http://www.heroinewarrior.com/
//  http://www.geocities.com/SiliconValley/Lakes/2160/fformats/files/mov.pdf
//  (above url no longer works, file mirrored somewhere? ::atmos)
//  The QuickTime File Format PDF from Apple:
//  http://developer.apple.com/techpubs/quicktime/qtdevdocs/PDF/QTFileFormat.pdf
//  (Complete list of documentation at http://developer.apple.com/quicktime/)
//  MP4-Lib sources from http://mpeg4ip.sf.net/ might be usefull fot .mp4
//  aswell as .mov specific stuff.
//  All sort of Stuff about MPEG4:
//  http://www.cmlab.csie.ntu.edu.tw/~pkhsiao/thesis.html
//  I really recommend N4270-1.doc and N4270-2.doc which are exact specs
//  of the MP4-File Format and the MPEG4 Specific extensions. ::atmos

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "config.h"
#include "mp_msg.h"
#include "help_mp.h"

#include "stream.h"
#include "demuxer.h"
#include "stheader.h"

#include "bswap.h"

#include "qtpalette.h"
#include "parse_mp4.h" // .MP4 specific stuff

#ifdef MACOSX
#include <QuickTime/QuickTime.h>
#else
#include "../loader/qtx/qtxsdk/components.h"
#endif

#ifdef HAVE_ZLIB
#include <zlib.h>
#endif

#ifndef _FCNTL_H
#include <fcntl.h>
#endif

#define BE_16(x) (((unsigned char *)(x))[0] <<  8 | \
		  ((unsigned char *)(x))[1])
#define BE_32(x) (((unsigned char *)(x))[0] << 24 | \
		  ((unsigned char *)(x))[1] << 16 | \
		  ((unsigned char *)(x))[2] <<  8 | \
		  ((unsigned char *)(x))[3])

#define char2short(x,y)	BE_16(&(x)[(y)])
#define char2int(x,y) 	BE_32(&(x)[(y)])

typedef struct {
    unsigned int pts; // duration
    unsigned int size;
    off_t pos;
} mov_sample_t;

typedef struct {
    unsigned int sample; // number of the first sample in the chunk
    unsigned int size;   // number of samples in the chunk
    int desc;            // for multiple codecs mode - not used
    off_t pos;
} mov_chunk_t;

typedef struct {
    unsigned int first;
    unsigned int spc;
    unsigned int sdid;
} mov_chunkmap_t;

typedef struct {
    unsigned int num;
    unsigned int dur;
} mov_durmap_t;

typedef struct {
    unsigned int dur;
    unsigned int pos;
    int speed;
    //
    int frames;
    int start_sample;
    int start_frame;
    int pts_offset;
} mov_editlist_t;

#define MOV_TRAK_UNKNOWN 0
#define MOV_TRAK_VIDEO 1
#define MOV_TRAK_AUDIO 2
#define MOV_TRAK_FLASH 3
#define MOV_TRAK_GENERIC 4
#define MOV_TRAK_CODE 5

typedef struct {
    int id;
    int type;
    off_t pos;
    //
    unsigned int media_handler;
    unsigned int data_handler;
    //
    int timescale;
    unsigned int length;
    int samplesize;  // 0 = variable
    int duration;    // 0 = variable
    int width,height; // for video
    unsigned int fourcc;
    unsigned int nchannels;
    unsigned int samplebytes;
    //
    int tkdata_len;  // track data 
    unsigned char* tkdata;
    int stdata_len;  // stream data
    unsigned char* stdata;
    //
    unsigned char* stream_header;
    int stream_header_len; // if >0, this header should be sent before the 1st frame
    //
    int samples_size;
    mov_sample_t* samples;
    int chunks_size;
    mov_chunk_t* chunks;
    int chunkmap_size;
    mov_chunkmap_t* chunkmap;
    int durmap_size;
    mov_durmap_t* durmap;
    int keyframes_size;
    unsigned int* keyframes;
    int editlist_size;
    mov_editlist_t* editlist;
    int editlist_pos;
    //
    void* desc; // image/sound/etc description (pointer to ImageDescription etc)
} mov_track_t;

void mov_build_index(mov_track_t* trak,int timescale){
    int i,j,s;
    int last=trak->chunks_size;
    unsigned int pts=0;

#if 0
    if (trak->chunks_size <= 0)
    {
	mp_msg(MSGT_DEMUX, MSGL_WARN, "No chunk offset table, trying to build one!\n");
	
	trak->chunks_size = trak->samples_size; /* XXX: FIXME ! */
	trak->chunks = realloc(trak->chunks, sizeof(mov_chunk_t)*trak->chunks_size);
	
	for (i=0; i < trak->chunks_size; i++)
	    trak->chunks[i].pos = -1;
    }
#endif

    mp_msg(MSGT_DEMUX, MSGL_INFO, "MOV track #%d: %d chunks, %d samples\n",trak->id,trak->chunks_size,trak->samples_size);
    mp_msg(MSGT_DEMUX, MSGL_V, "pts=%d  scale=%d  time=%5.3f\n",trak->length,trak->timescale,(float)trak->length/(float)trak->timescale);

    // process chunkmap:
    i=trak->chunkmap_size;
    while(i>0){
	--i;
	for(j=trak->chunkmap[i].first;j<last;j++){
	    trak->chunks[j].desc=trak->chunkmap[i].sdid;
	    trak->chunks[j].size=trak->chunkmap[i].spc;
	}
	last=trak->chunkmap[i].first;
    }

#if 0
    for (i=0; i < trak->chunks_size; i++)
    {
	/* fixup position */
	if (trak->chunks[i].pos == -1)
	    if (i > 0)
		trak->chunks[i].pos = trak->chunks[i-1].pos + trak->chunks[i-1].size;
	    else
		trak->chunks[i].pos = 0; /* FIXME: set initial pos */
#endif

    // calc pts of chunks:
    s=0;
    for(j=0;j<trak->chunks_size;j++){
        trak->chunks[j].sample=s;
        s+=trak->chunks[j].size;
    }

    // workaround for fixed-size video frames (dv and uncompressed)
    if(!trak->samples_size && trak->type!=MOV_TRAK_AUDIO){
	trak->samples_size=s;
	trak->samples=malloc(sizeof(mov_sample_t)*s);
	for(i=0;i<s;i++)
	    trak->samples[i].size=trak->samplesize;
	trak->samplesize=0;
    }

    if(!trak->samples_size){
	// constant sampesize
	if(trak->durmap_size==1 || (trak->durmap_size==2 && trak->durmap[1].num==1)){
	    trak->duration=trak->durmap[0].dur;
	} else mp_msg(MSGT_DEMUX, MSGL_ERR, "*** constant samplesize & variable duration not yet supported! ***\nContact the author if you have such sample file!\n");
	return;
    }
    
    // calc pts:
    s=0;
    for(j=0;j<trak->durmap_size;j++){
	for(i=0;i<trak->durmap[j].num;i++){
	    trak->samples[s].pts=pts;
	    ++s;
	    pts+=trak->durmap[j].dur;
	}
    }
    
    // calc sample offsets
    s=0;
    for(j=0;j<trak->chunks_size;j++){
	off_t pos=trak->chunks[j].pos;
	for(i=0;i<trak->chunks[j].size;i++){
	    trak->samples[s].pos=pos;
	    mp_msg(MSGT_DEMUX, MSGL_DBG3, "Sample %5d: pts=%8d  off=0x%08X  size=%d\n",s,
		trak->samples[s].pts,
		(int)trak->samples[s].pos,
		trak->samples[s].size);
	    pos+=trak->samples[s].size;
	    ++s;
	}
    }

    // precalc editlist entries
    if(trak->editlist_size>0){
	int frame=0;
	int e_pts=0;
	for(i=0;i<trak->editlist_size;i++){
	    mov_editlist_t* el=&trak->editlist[i];
	    int sample=0;
	    int pts=el->pos;
	    el->start_frame=frame;
	    if(pts<0){
		// skip!
		el->frames=0; continue;
	    }
	    // find start sample
	    for(;sample<trak->samples_size;sample++){
		if(pts<=trak->samples[sample].pts) break;
	    }
	    el->start_sample=sample;
	    el->pts_offset=((long long)e_pts*(long long)trak->timescale)/(long long)timescale-trak->samples[sample].pts;
	    pts+=((long long)el->dur*(long long)trak->timescale)/(long long)timescale;
	    e_pts+=el->dur;
	    // find end sample
	    for(;sample<trak->samples_size;sample++){
		if(pts<=trak->samples[sample].pts) break;
	    }
	    el->frames=sample-el->start_sample;
	    frame+=el->frames;
	    mp_msg(MSGT_DEMUX,MSGL_V,"EL#%d: pts=%d  1st_sample=%d  frames=%d (%5.3fs)  pts_offs=%d\n",i,
		el->pos,el->start_sample, el->frames,
		(float)(el->dur)/(float)timescale, el->pts_offset);
	}
    }

}

#define MOV_MAX_TRACKS 256

typedef struct {
    off_t moov_start;
    off_t moov_end;
    off_t mdat_start;
    off_t mdat_end;
    int track_db;
    mov_track_t* tracks[MOV_MAX_TRACKS];
    int timescale; // movie timescale
    int duration;  // movie duration (in movie timescale units)
} mov_priv_t;

#define MOV_FOURCC(a,b,c,d) ((a<<24)|(b<<16)|(c<<8)|(d))

int mov_check_file(demuxer_t* demuxer){
    int flags=0;
    int no=0;
    mov_priv_t* priv=malloc(sizeof(mov_priv_t));
    
    mp_msg(MSGT_DEMUX,MSGL_V,"Checking for MOV\n");
    
    memset(priv,0,sizeof(mov_priv_t));
    
    while(1){
	int i;
	int skipped=8;
	off_t len=stream_read_dword(demuxer->stream);
	unsigned int id=stream_read_dword(demuxer->stream);
	if(stream_eof(demuxer->stream)) break; // EOF
	if (len == 1) /* real size is 64bits - cjb */
	{
#ifndef _LARGEFILE_SOURCE
	    if (stream_read_dword(demuxer->stream) != 0)
		mp_msg(MSGT_DEMUX, MSGL_WARN, "64bit file, but you've compiled MPlayer without LARGEFILE support!\n");
	    len = stream_read_dword(demuxer->stream);
#else
	    len = stream_read_qword(demuxer->stream);
#endif
	    skipped += 8;
	}
#if 0
	else if (len == 0) /* deleted chunk */
	{
	    /* XXX: CJB! is this right? - alex */
	    goto skip_chunk;
	}
#endif
	else if(len<8) break; // invalid chunk

	switch(id){
	case MOV_FOURCC('f','t','y','p'): {
	  unsigned int tmp;
	  // File Type Box (ftyp): 
	  // char[4]  major_brand	   (eg. 'isom')
	  // int      minor_version	   (eg. 0x00000000)
	  // char[4]  compatible_brands[]  (eg. 'mp41')
	  // compatible_brands list spans to the end of box
#if 1
	  tmp = stream_read_dword(demuxer->stream);
	  switch(tmp) {
	    case MOV_FOURCC('i','s','o','m'):
	      mp_msg(MSGT_DEMUX,MSGL_V,"ISO: File Type Major Brand: ISO Base Media\n");
     	      break;
	    case MOV_FOURCC('m','p','4','1'):
	      mp_msg(MSGT_DEMUX,MSGL_INFO,"ISO: File Type Major Brand: ISO/IEC 14496-1 (MPEG-4 system) v1\n");
     	      break;
	    case MOV_FOURCC('m','p','4','2'):
	      mp_msg(MSGT_DEMUX,MSGL_INFO,"ISO: File Type Major Brand: ISO/IEC 14496-1 (MPEG-4 system) v2\n");
     	      break;
	    case MOV_FOURCC('M','4','A',' '):
	      mp_msg(MSGT_DEMUX,MSGL_INFO,"ISO: File Type Major Brand: Apple iTunes AAC-LC Audio\n");
     	      break;
	    case MOV_FOURCC('M','4','P',' '):
	      mp_msg(MSGT_DEMUX,MSGL_INFO,"ISO: File Type Major Brand: Apple iTunes AAC-LC Protected Audio\n");
     	      break;
	    case MOV_FOURCC('q','t',' ',' '):
	      mp_msg(MSGT_DEMUX,MSGL_INFO,"ISO: File Type Major Brand: Original QuickTime\n");
     	      break;
	    case MOV_FOURCC('3','g','p','1'):
	      mp_msg(MSGT_DEMUX,MSGL_INFO,"ISO: File Type Major Brand: 3GPP Profile 1\n");
     	      break;
	    case MOV_FOURCC('3','g','p','2'):
	      mp_msg(MSGT_DEMUX,MSGL_INFO,"ISO: File Type Major Brand: 3GPP Profile 2\n");
     	      break;
	    case MOV_FOURCC('3','g','p','3'):
	      mp_msg(MSGT_DEMUX,MSGL_INFO,"ISO: File Type Major Brand: 3GPP Profile 3\n");
     	      break;
	    case MOV_FOURCC('3','g','p','4'):
	      mp_msg(MSGT_DEMUX,MSGL_INFO,"ISO: File Type Major Brand: 3GPP Profile 4\n");
     	      break;
	    case MOV_FOURCC('3','g','p','5'):
	      mp_msg(MSGT_DEMUX,MSGL_INFO,"ISO: File Type Major Brand: 3GPP Profile 5\n");
     	      break;
	    case MOV_FOURCC('m','m','p','4'):
	      mp_msg(MSGT_DEMUX,MSGL_INFO,"ISO: File Type Major Brand: Mobile ISO/IEC 14496-1 (MPEG-4 system)\n");
     	      break;
	    default:
	      tmp = be2me_32(tmp);  
	      mp_msg(MSGT_DEMUX,MSGL_WARN,"ISO: Unknown File Type Major Brand: %.4s\n",&tmp);
	  }
	  mp_msg(MSGT_DEMUX,MSGL_V,"ISO: File Type Minor Version: %d\n",
	      stream_read_dword(demuxer->stream));
	  skipped += 8;
	  // List all compatible brands
	  for(i = 0; i < ((len-16)/4); i++) {
	    tmp = be2me_32(stream_read_dword(demuxer->stream));
	    mp_msg(MSGT_DEMUX,MSGL_V,"ISO: File Type Compatible Brand #%d: %.4s\n",i,&tmp);
	    skipped += 4;
	  }
#endif	  
	  } break;
	case MOV_FOURCC('m','o','o','v'):
//	case MOV_FOURCC('c','m','o','v'):
	  mp_msg(MSGT_DEMUX,MSGL_V,"MOV: Movie header found!\n");
	  priv->moov_start=(off_t)stream_tell(demuxer->stream);
	  priv->moov_end=(off_t)priv->moov_start+len-skipped;
	  mp_msg(MSGT_DEMUX,MSGL_DBG2,"MOV: Movie header: start: %x end: %x\n",
	    priv->moov_start, priv->moov_end);
	  skipped+=8;
	  i = stream_read_dword(demuxer->stream)-8;
	  if(stream_read_dword(demuxer->stream)==MOV_FOURCC('r','m','r','a')){
	      int ref=0;
	      skipped+=i;
	      mp_msg(MSGT_DEMUX,MSGL_INFO,"MOV: Reference Media file!!!\n");
	      //set demuxer type to playlist ...
	      demuxer->type=DEMUXER_TYPE_PLAYLIST;
	      while(i>0){
	          int len=stream_read_dword(demuxer->stream)-8;
		  int fcc=stream_read_dword(demuxer->stream);
		  if(len<0) break; // EOF!?
		  i-=8;
//		  printf("i=%d  len=%d\n",i,len);
		  switch(fcc){
		  case MOV_FOURCC('r','m','d','a'):
		      continue;
		  case MOV_FOURCC('r','d','r','f'): {
		      int tmp=stream_read_dword(demuxer->stream);
		      int type=stream_read_dword_le(demuxer->stream);
	              int slen=stream_read_dword(demuxer->stream);
		      //char* s=malloc(slen+1);
		      //stream_read(demuxer->stream,s,slen);
		      
		      //FIXME: also store type & data_rate ?
		      ds_read_packet(demuxer->video, 
			demuxer->stream,
			slen,
			0,
			stream_tell(demuxer->stream),
			0 // no flags 
		      );
		      flags|=4;
		      mp_msg(MSGT_DEMUX,MSGL_V,"Added reference to playlist\n");
		      //s[slen]=0;
		      //mp_msg(MSGT_DEMUX,MSGL_INFO,"REF: [%.4s] %s\n",&type,s);
		      len-=12+slen;i-=12+slen; break;
		    }
		  case MOV_FOURCC('r','m','d','r'): {
		      int flags=stream_read_dword(demuxer->stream);
		      int rate=stream_read_dword(demuxer->stream);
		      mp_msg(MSGT_DEMUX,MSGL_V,"  min. data rate: %d bits/sec\n",rate);
		      len-=8; i-=8; break;
		    }
		  case MOV_FOURCC('r','m','q','u'): {
		      int q=stream_read_dword(demuxer->stream);
		      mp_msg(MSGT_DEMUX,MSGL_V,"  quality index: %d\n",q);
		      len-=4; i-=4; break;
		    }
		  }
		  i-=len;stream_skip(demuxer->stream,len);
	      }
	  }
	  flags|=1;
	  break;
	case MOV_FOURCC('w','i','d','e'):
	  mp_msg(MSGT_DEMUX,MSGL_V,"MOV: 'WIDE' chunk found!\n");
	  if(flags&2) break;
	case MOV_FOURCC('m','d','a','t'):
	  mp_msg(MSGT_DEMUX,MSGL_V,"MOV: Movie DATA found!\n");
	  priv->mdat_start=stream_tell(demuxer->stream);
	  priv->mdat_end=priv->mdat_start+len-skipped;
	  mp_msg(MSGT_DEMUX,MSGL_DBG2,"MOV: Movie data: start: %x end: %x\n",
	    priv->mdat_start, priv->mdat_end);
	  flags|=2;
	  if(flags==3){
	    // if we're over the headers, then we can stop parsing here!
	    demuxer->priv=priv;
	    return 1;
	  }
	  break;
	case MOV_FOURCC('f','r','e','e'):
	case MOV_FOURCC('s','k','i','p'):
	case MOV_FOURCC('j','u','n','k'):
	  mp_msg(MSGT_DEMUX,MSGL_DBG2,"MOV: free space (len: %d)\n", len);
	  /* unused, if you edit a mov, you can use space provided by free atoms (redefining it) */
	  break;
	case MOV_FOURCC('p','n','o','t'):
	case MOV_FOURCC('P','I','C','T'):
	  /* dunno what, but we shoudl ignore it */
	  break;
	default:
	  if(no==0){ free(priv); return 0;} // first chunk is bad!
	  id = be2me_32(id);
	  mp_msg(MSGT_DEMUX,MSGL_V,"MOV: unknown chunk: %.4s %d\n",&id,(int)len);
	}
skip_chunk:
	if(!stream_skip(demuxer->stream,len-skipped)) break;
	++no;
    }

    if(flags==3){
	demuxer->priv=priv;
	return 1;
    }
    free(priv);

    if ((flags==5) || (flags==7)) // reference & header sent
        return 1;

    if(flags==1)
	mp_msg(MSGT_DEMUX,MSGL_WARN,"MOV: missing data (mdat) chunk! Maybe broken file...\n");
    else if(flags==2)
	mp_msg(MSGT_DEMUX,MSGL_WARN,"MOV: missing header (moov/cmov) chunk! Maybe broken file...\n");

    return 0;
}

static void lschunks(demuxer_t* demuxer,int level,off_t endpos,mov_track_t* trak){
    mov_priv_t* priv=demuxer->priv;
//    printf("lschunks (level=%d,endpos=%x)\n", level, endpos);
    while(1){
	off_t pos;
	off_t len;
	unsigned int id;
	//
	pos=stream_tell(demuxer->stream);
//	printf("stream_tell==%d\n",pos);
	if(pos>=endpos) return; // END
	len=stream_read_dword(demuxer->stream);
//	printf("len==%d\n",len);
	if(len<8) return; // error
	len-=8;
	id=stream_read_dword(demuxer->stream);
	//
	mp_msg(MSGT_DEMUX,MSGL_DBG2,"lschunks %.4s  %d\n",&id,(int)len);
	//
	if(trak){
	  switch(id){
	    case MOV_FOURCC('m','d','a','t'): {
		mp_msg(MSGT_DEMUX,MSGL_WARN,"Hmm, strange MOV, parsing mdat in lschunks?\n");
		return;
	    }
	    case MOV_FOURCC('f','r','e','e'):
	    case MOV_FOURCC('u','d','t','a'):
		/* here not supported :p */
		break;
	    case MOV_FOURCC('t','k','h','d'): {
		mp_msg(MSGT_DEMUX,MSGL_V,"MOV: %*sTrack header!\n",level,"");
		// read codec data
		trak->tkdata_len=len;
		trak->tkdata=malloc(trak->tkdata_len);
		stream_read(demuxer->stream,trak->tkdata,trak->tkdata_len);
/*
0  1 Version
1  3 Flags
4  4 Creation time
8  4 Modification time
12 4 Track ID
16 4 Reserved
20 4 Duration
24 8 Reserved
32 2 Layer
34 2 Alternate group
36 2 Volume
38 2 Reserved
40 36 Matrix structure
76 4 Track width
80 4 Track height
*/
		mp_msg(MSGT_DEMUX,MSGL_V,"tkhd len=%d ver=%d flags=0x%X id=%d dur=%d lay=%d vol=%d\n",
		    trak->tkdata_len, trak->tkdata[0], trak->tkdata[1],
		    char2int(trak->tkdata,12), // id
		    char2int(trak->tkdata,20), // duration
		    char2short(trak->tkdata,32), // layer
		    char2short(trak->tkdata,36)); // volume
		break;
	    }
	    case MOV_FOURCC('m','d','h','d'): {
		unsigned int tmp;
		mp_msg(MSGT_DEMUX,MSGL_V,"MOV: %*sMedia header!\n",level,"");
#if 0
		tmp=stream_read_dword(demuxer->stream);
		printf("dword1: 0x%08X (%d)\n",tmp,tmp);
		tmp=stream_read_dword(demuxer->stream);
		printf("dword2: 0x%08X (%d)\n",tmp,tmp);
		tmp=stream_read_dword(demuxer->stream);
		printf("dword3: 0x%08X (%d)\n",tmp,tmp);
		tmp=stream_read_dword(demuxer->stream);
		printf("dword4: 0x%08X (%d)\n",tmp,tmp);
		tmp=stream_read_dword(demuxer->stream);
		printf("dword5: 0x%08X (%d)\n",tmp,tmp);
		tmp=stream_read_dword(demuxer->stream);
		printf("dword6: 0x%08X (%d)\n",tmp,tmp);
#endif
		stream_skip(demuxer->stream,12);
		// read timescale
		trak->timescale=stream_read_dword(demuxer->stream);
		// read length
		trak->length=stream_read_dword(demuxer->stream);
		break;
	    }
	    case MOV_FOURCC('h','d','l','r'): {
		unsigned int tmp=stream_read_dword(demuxer->stream);
		unsigned int type=stream_read_dword_le(demuxer->stream);
		unsigned int subtype=stream_read_dword_le(demuxer->stream);
		unsigned int manufact=stream_read_dword_le(demuxer->stream);
		unsigned int comp_flags=stream_read_dword(demuxer->stream);
		unsigned int comp_mask=stream_read_dword(demuxer->stream);
		int len=stream_read_char(demuxer->stream);
		char* str=malloc(len+1);
		stream_read(demuxer->stream,str,len);
		str[len]=0;
		mp_msg(MSGT_DEMUX,MSGL_V,"MOV: %*sHandler header: %.4s/%.4s (%.4s) %s\n",level,"",&type,&subtype,&manufact,str);
		free(str);
		switch(bswap_32(type)){
		case MOV_FOURCC('m','h','l','r'):
		    trak->media_handler=bswap_32(subtype); break;
		case MOV_FOURCC('d','h','l','r'):
		    trak->data_handler=bswap_32(subtype); break;
		default:
		    mp_msg(MSGT_DEMUX,MSGL_V,"MOV: unknown handler class: 0x%X (%.4s)\n",bswap_32(type),&type);
		}
		break;
	    }
	    case MOV_FOURCC('v','m','h','d'): {
		mp_msg(MSGT_DEMUX,MSGL_V,"MOV: %*sVideo header!\n",level,"");
		trak->type=MOV_TRAK_VIDEO;
		// read video data
		break;
	    }
	    case MOV_FOURCC('s','m','h','d'): {
		mp_msg(MSGT_DEMUX,MSGL_V,"MOV: %*sSound header!\n",level,"");
		trak->type=MOV_TRAK_AUDIO;
		// read audio data
		break;
	    }
	    case MOV_FOURCC('g','m','h','d'): {
		mp_msg(MSGT_DEMUX,MSGL_V,"MOV: %*sGeneric header!\n",level,"");
		trak->type=MOV_TRAK_GENERIC;
		break;
	    }
	    case MOV_FOURCC('n','m','h','d'): {
		mp_msg(MSGT_DEMUX,MSGL_V,"MOV: %*sGeneric header!\n",level,"");
		trak->type=MOV_TRAK_GENERIC;
		break;
	    }
	    case MOV_FOURCC('s','t','s','d'): {
		int i=stream_read_dword(demuxer->stream); // temp!
		int count=stream_read_dword(demuxer->stream);
		mp_msg(MSGT_DEMUX,MSGL_V,"MOV: %*sDescription list! (cnt:%d)\n",level,"",count);
		for(i=0;i<count;i++){
		    off_t pos=stream_tell(demuxer->stream);
		    off_t len=stream_read_dword(demuxer->stream);
		    unsigned int fourcc=stream_read_dword_le(demuxer->stream);
		    /* some files created with Broadcast 2000 (e.g. ilacetest.mov)
		       contain raw I420 video but have a yv12 fourcc */
		    if(fourcc==mmioFOURCC('y','v','1','2')) fourcc=mmioFOURCC('I','4','2','0');
		    if(len<8) break; // error
		    mp_msg(MSGT_DEMUX,MSGL_V,"MOV: %*s desc #%d: %.4s  (%d bytes)\n",level,"",i,&fourcc,len-16);
		    if(fourcc!=trak->fourcc && i)
			mp_msg(MSGT_DEMUX,MSGL_WARN,MSGTR_MOVvariableFourCC);
//		    if(!i)
		    {
			trak->fourcc=fourcc;
			// read type specific (audio/video/time/text etc) header
			// NOTE: trak type is not yet known at this point :(((
			trak->stdata_len=len-8;
			trak->stdata=malloc(trak->stdata_len);
			stream_read(demuxer->stream,trak->stdata,trak->stdata_len);
		    }
		    if(!stream_seek(demuxer->stream,pos+len)) break;
		}
		break;
	    }
	    case MOV_FOURCC('s','t','t','s'): {
		int temp=stream_read_dword(demuxer->stream);
		int len=stream_read_dword(demuxer->stream);
		int i;
		unsigned int pts=0;
		mp_msg(MSGT_DEMUX,MSGL_V,"MOV: %*sSample duration table! (%d blocks)\n",level,"",len);
		trak->durmap=malloc(sizeof(mov_durmap_t)*len);
		memset(trak->durmap,0,sizeof(mov_durmap_t)*len);
		trak->durmap_size=len;
		for(i=0;i<len;i++){
		    trak->durmap[i].num=stream_read_dword(demuxer->stream);
		    trak->durmap[i].dur=stream_read_dword(demuxer->stream);
		    pts+=trak->durmap[i].num*trak->durmap[i].dur;
		}
		if(trak->length!=pts) mp_msg(MSGT_DEMUX, MSGL_WARN, "Warning! pts=%d  length=%d\n",pts,trak->length);
		break;
	    }
	    case MOV_FOURCC('s','t','s','c'): {
		int temp=stream_read_dword(demuxer->stream);
		int len=stream_read_dword(demuxer->stream);
		int ver = (temp << 24);
		int flags = (temp << 16)|(temp<<8)|temp;
		int i;
		mp_msg(MSGT_DEMUX,MSGL_V,"MOV: %*sSample->Chunk mapping table!  (%d blocks) (ver:%d,flags:%ld)\n",
		    level,"",len,ver,flags);
		// read data:
		trak->chunkmap_size=len;
		trak->chunkmap=malloc(sizeof(mov_chunkmap_t)*len);
		for(i=0;i<len;i++){
		    trak->chunkmap[i].first=stream_read_dword(demuxer->stream)-1;
		    trak->chunkmap[i].spc=stream_read_dword(demuxer->stream);
		    trak->chunkmap[i].sdid=stream_read_dword(demuxer->stream);
		}
		break;
	    }
	    case MOV_FOURCC('s','t','s','z'): {
		int temp=stream_read_dword(demuxer->stream);
		int ss=stream_read_dword(demuxer->stream);
		int ver = (temp << 24);
		int flags = (temp << 16)|(temp<<8)|temp;
		int entries=stream_read_dword(demuxer->stream);
		int i;
		
		mp_msg(MSGT_DEMUX,MSGL_V,"MOV: %*sSample size table! (entries=%d ss=%d) (ver:%d,flags:%ld)\n",
		    level,"",entries,ss,ver,flags);
		trak->samplesize=ss;
		if (!ss) {
		  // variable samplesize
		  trak->samples=realloc(trak->samples,sizeof(mov_sample_t)*entries);
		  trak->samples_size=entries;
		  for(i=0;i<entries;i++)
		    trak->samples[i].size=stream_read_dword(demuxer->stream);
		}
		break;
	    }
	    case MOV_FOURCC('s','t','c','o'): {
		int temp=stream_read_dword(demuxer->stream);
		int len=stream_read_dword(demuxer->stream);
		int i;
		mp_msg(MSGT_DEMUX,MSGL_V,"MOV: %*sChunk offset table! (%d chunks)\n",level,"",len);
		// extend array if needed:
		if(len>trak->chunks_size){
		    trak->chunks=realloc(trak->chunks,sizeof(mov_chunk_t)*len);
		    trak->chunks_size=len;
		}
		// read elements:
		for(i=0;i<len;i++) trak->chunks[i].pos=stream_read_dword(demuxer->stream);
		break;
	    }
	    case MOV_FOURCC('c','o','6','4'): {
		int temp=stream_read_dword(demuxer->stream);
		int len=stream_read_dword(demuxer->stream);
		int i;
		mp_msg(MSGT_DEMUX,MSGL_V,"MOV: %*s64bit chunk offset table! (%d chunks)\n",level,"",len);
		// extend array if needed:
		if(len>trak->chunks_size){
		    trak->chunks=realloc(trak->chunks,sizeof(mov_chunk_t)*len);
		    trak->chunks_size=len;
		}
		// read elements:
		for(i=0;i<len;i++)
		{
#ifndef	_LARGEFILE_SOURCE
		    if (stream_read_dword(demuxer->stream) != 0)
			mp_msg(MSGT_DEMUX, MSGL_WARN, "Chunk %d has got 64bit address, but you've MPlayer compiled without LARGEFILE support!\n", i);
		    trak->chunks[i].pos = stream_read_dword(demuxer->stream);
#else
		    trak->chunks[i].pos = stream_read_qword(demuxer->stream);
#endif
		}
		break;
	    }
	    case MOV_FOURCC('s','t','s','s'): {
		int temp=stream_read_dword(demuxer->stream);
		int entries=stream_read_dword(demuxer->stream);
		int ver = (temp << 24);
		int flags = (temp << 16)|(temp<<8)|temp;
		int i;
		mp_msg(MSGT_DEMUX, MSGL_V,"MOV: %*sSyncing samples (keyframes) table! (%d entries) (ver:%d,flags:%ld)\n",
		    level, "",entries, ver, flags);
		trak->keyframes_size=entries;
		trak->keyframes=malloc(sizeof(unsigned int)*entries);
		for (i=0;i<entries;i++)
		    trak->keyframes[i]=stream_read_dword(demuxer->stream)-1;
//		for (i=0;i<entries;i++) printf("%3d: %d\n",i,trak->keyframes[i]);
		break;
	    }
	    case MOV_FOURCC('m','d','i','a'): {
		mp_msg(MSGT_DEMUX,MSGL_V,"MOV: %*sMedia stream!\n",level,"");
		lschunks(demuxer,level+1,pos+len,trak);
		break;
	    }
	    case MOV_FOURCC('m','i','n','f'): {
		mp_msg(MSGT_DEMUX,MSGL_V,"MOV: %*sMedia info!\n",level,"");
		lschunks(demuxer,level+1,pos+len,trak);
		break;
	    }
	    case MOV_FOURCC('s','t','b','l'): {
		mp_msg(MSGT_DEMUX,MSGL_V,"MOV: %*sSample info!\n",level,"");
		lschunks(demuxer,level+1,pos+len,trak);
		break;
	    }
	    case MOV_FOURCC('e','d','t','s'): {
		mp_msg(MSGT_DEMUX, MSGL_V, "MOV: %*sEdit atom!\n", level, "");
		lschunks(demuxer,level+1,pos+len,trak);
		break;
	    }
	    case MOV_FOURCC('e','l','s','t'): {
		int temp=stream_read_dword(demuxer->stream);
		int entries=stream_read_dword(demuxer->stream);
		int ver = (temp << 24);
		int flags = (temp << 16)|(temp<<8)|temp;
		int i;
	
		mp_msg(MSGT_DEMUX, MSGL_V,"MOV: %*sEdit list table (%d entries) (ver:%d,flags:%ld)\n",
		    level, "",entries, ver, flags);
#if 1
		trak->editlist_size=entries;
		trak->editlist=malloc(trak->editlist_size*sizeof(mov_editlist_t));
		for (i=0;i<entries;i++)
		{
		    int dur=stream_read_dword(demuxer->stream);
		    int mt=stream_read_dword(demuxer->stream);
		    int mr=stream_read_dword(demuxer->stream); // 16.16fp
		    trak->editlist[i].dur=dur;
		    trak->editlist[i].pos=mt;
		    trak->editlist[i].speed=mr;
		    mp_msg(MSGT_DEMUX, MSGL_V,"MOV: %*s  entry#%d: duration: %d  start time: %d  speed: %3.1fx\n",level,"",
			i,
			dur,mt,(float)mr/65536.0f);
		}
#endif
		break;
	    }
	    case MOV_FOURCC('c','o','d','e'):
	    {
	    /* XXX: Implement atom 'code' for FLASH support */
	    }
	    default:
		id = be2me_32(id);
		mp_msg(MSGT_DEMUX,MSGL_V,"MOV: unknown chunk: %.4s %d\n",&id,(int)len);
		break;
	  }//switch(id)
	} else { /* not in track */
	  switch(id) {
	    case MOV_FOURCC('m','v','h','d'): {
		stream_skip(demuxer->stream,12);
		priv->timescale=stream_read_dword(demuxer->stream);
		priv->duration=stream_read_dword(demuxer->stream);
		mp_msg(MSGT_DEMUX, MSGL_V,"MOV: %*sMovie header (%d bytes): tscale=%d  dur=%d\n",level,"",(int)len,
		    (int)priv->timescale,(int)priv->duration);
		break;
	    }
	    case MOV_FOURCC('t','r','a','k'): {
//	    if(trak) printf("MOV: Warning! trak in trak?\n");
	    if(priv->track_db>=MOV_MAX_TRACKS){
		mp_msg(MSGT_DEMUX,MSGL_WARN,MSGTR_MOVtooManyTrk);
		return;
	    }
	    if(!priv->track_db) mp_msg(MSGT_DEMUX, MSGL_INFO, "--------------\n");
	    trak=malloc(sizeof(mov_track_t));
	    memset(trak,0,sizeof(mov_track_t));
	    mp_msg(MSGT_DEMUX,MSGL_V,"MOV: Track #%d:\n",priv->track_db);
	    trak->id=priv->track_db;
	    priv->tracks[priv->track_db]=trak;
	    lschunks(demuxer,level+1,pos+len,trak);
	    mov_build_index(trak,priv->timescale);
	    switch(trak->type){
	    case MOV_TRAK_AUDIO: {
#if 0				   
		struct {
		   int16_t version;		// 0 or 1 (version 1 is qt3.0+)
		   int16_t revision;		// 0
		   int32_t vendor_id;		// 0
		   int16_t channels;		// 1 or 2  (Mono/Stereo)
		   int16_t samplesize;		// 8 or 16 (8Bit/16Bit)	
		   int16_t compression_id;	// if version 0 then 0
		  				// if version 1 and vbr then -2 else 0
		   int16_t packet_size;		// 0
		   uint16_t sample_rate;	// samplerate (Hz)
		   // qt3.0+ (version == 1)
		   uint32_t samples_per_packet;	// 0 or num uncompressed samples in a packet
		   				// if 0 below three values are also 0
		   uint32_t bytes_per_packet;	// 0 or num compressed bytes for one channel
		   uint32_t bytes_per_frame;	// 0 or num compressed bytes for all channels
		   				// (channels * bytes_per_packet)
		   uint32_t bytes_per_sample;	// 0 or size of uncompressed sample
		   // if samples_per_packet and bytes_per_packet are constant (CBR)
		   // then bytes_per_frame and bytes_per_sample must be 0 (else is VBR)
		   // ---
		   // optional additional atom-based fields
		   // ([int32_t size,int32_t type,some data ],repeat)
		} my_stdata;		  
#endif		
		sh_audio_t* sh=new_sh_audio(demuxer,priv->track_db);
		sh->format=trak->fourcc;

		switch( sh->format ) {
		    case 0x726D6173: /* samr */
			/* amr narrowband */
			trak->samplebytes=sh->samplesize=1;
			trak->nchannels=sh->channels=1;
			sh->samplerate=8000;
			break;
			
		    case 0x62776173: /* sawb */
			/* amr wideband */
			trak->samplebytes=sh->samplesize=1;
			trak->nchannels=sh->channels=1;
			sh->samplerate=16000;
			break;

		    default:
			
// assumptions for below table: short is 16bit, int is 32bit, intfp is 16bit
// XXX: 32bit fixed point numbers (intfp) are only 2 Byte!		
// short values are usually one byte leftpadded by zero		
//   int values are usually two byte leftpadded by zero		
//  stdata[]:
//	8   short	version
//	10  short	revision
//	12  int		vendor_id
//	16  short	channels
//	18  short	samplesize
//	20  short	compression_id
//	22  short	packet_size (==0)
//	24  intfp	sample_rate
//     (26  short)	unknown (==0)
//    ---- qt3.0+ (version>=1)
//	28  int		samples_per_packet
//	32  int		bytes_per_packet
//	36  int		bytes_per_frame
//	40  int		bytes_per_sample
// there may be additional atoms following at 28 (version 0)
// or 44 (version 1), eg. esds atom of .MP4 files		
// esds atom:
//      28  int		atom size (bytes of int size, int type and data)
//      32  char[4]	atom type (fourc charater code -> esds)		
//      36  char[]  	atom data (len=size-8)

		trak->samplebytes=sh->samplesize=char2short(trak->stdata,18)/8;
		trak->nchannels=sh->channels=char2short(trak->stdata,16);
		/*printf("MOV: timescale: %d samplerate: %d durmap: %d (%d) -> %d (%d)\n",
		    trak->timescale, char2short(trak->stdata,24), trak->durmap[0].dur,
		    trak->durmap[0].num, trak->timescale/trak->durmap[0].dur,
		    char2short(trak->stdata,24)/trak->durmap[0].dur);*/
		sh->samplerate=char2short(trak->stdata,24);
		if((sh->samplerate < 7000) && trak->durmap) {
		  switch(char2short(trak->stdata,24)/trak->durmap[0].dur) {
		    // TODO: add more cases.
		    case 31:
		      sh->samplerate = 32000; break;
		    case 43:
		      sh->samplerate = 44100; break;
		    case 47:
		      sh->samplerate = 48000; break;
		    default:
		      mp_msg(MSGT_DEMUX, MSGL_WARN,
			  "MOV: unable to determine audio samplerate, "
			  "assuming 44.1kHz (got %d)\n",
			  char2short(trak->stdata,24)/trak->durmap[0].dur);
		      sh->samplerate = 44100;
		  }  
		}  
		}
		mp_msg(MSGT_DEMUX, MSGL_INFO, "Audio bits: %d  chans: %d  rate: %d\n",
		    sh->samplesize*8,sh->channels,sh->samplerate);

		if(trak->stdata_len >= 44 && trak->stdata[9]>=1){
		  mp_msg(MSGT_DEMUX,MSGL_V,"Audio header: samp/pack=%d bytes/pack=%d bytes/frame=%d bytes/samp=%d  \n",
		    char2int(trak->stdata,28),
		    char2int(trak->stdata,32),
		    char2int(trak->stdata,36),
		    char2int(trak->stdata,40));
		  if(trak->stdata_len>=44+8){
		    int len=char2int(trak->stdata,44);
		    int fcc=char2int(trak->stdata,48);
		    // we have extra audio headers!!!
		    printf("Audio extra header: len=%d  fcc=0x%X\n",len,fcc);
		    sh->codecdata_len = len-8;
		    sh->codecdata = trak->stdata+44+8;
		  }
		}

		if((trak->stdata[9]==0 || trak->stdata[9]==1) && trak->stdata_len >= 36) { // version 0 with extra atoms
        int adjust = (trak->stdata[9]==1)?48:0;
		    int atom_len = char2int(trak->stdata,28+adjust);
		    switch(char2int(trak->stdata,32+adjust)) { // atom type
		      case MOV_FOURCC('e','s','d','s'): {
			mp_msg(MSGT_DEMUX, MSGL_INFO, "MOV: Found MPEG4 audio Elementary Stream Descriptor atom (%d)!\n", atom_len);
			if(atom_len > 8) {
			  esds_t esds; 				  
			  if(!mp4_parse_esds(&trak->stdata[36+adjust], atom_len-8, &esds)) {
			    
			    sh->i_bps = esds.avgBitrate/8; 

//			    printf("######## audio format = %d ########\n",esds.objectTypeId);
			    if(esds.objectTypeId==MP4OTI_MPEG1Audio || esds.objectTypeId==MP4OTI_MPEG2AudioPart3)
				sh->format=0x55; // .mp3

			    // dump away the codec specific configuration for the AAC decoder
			    if(esds.decoderConfigLen){
			    sh->codecdata_len = esds.decoderConfigLen;
			    sh->codecdata = (unsigned char *)malloc(sh->codecdata_len);
			    memcpy(sh->codecdata, esds.decoderConfig, sh->codecdata_len);
			    }
			  }
			  mp4_free_esds(&esds); // freeup esds mem
#if 0
	  		  { FILE* f=fopen("esds.dat","wb");
			  fwrite(&trak->stdata[36],atom_len-8,1,f);
			  fclose(f); }
#endif			  
			}
		      } break;
		      default:
			mp_msg(MSGT_DEMUX, MSGL_INFO, "MOV: Found unknown audio atom %c%c%c%c (%d)!\n",
			    trak->stdata[32+adjust],trak->stdata[33+adjust],trak->stdata[34+adjust],trak->stdata[35+adjust],
			    atom_len);
		    }
		}  
		mp_msg(MSGT_DEMUX, MSGL_INFO, "Fourcc: %.4s\n",&trak->fourcc);
#if 0
		{ FILE* f=fopen("stdata.dat","wb");
		  fwrite(trak->stdata,trak->stdata_len,1,f);
		  fclose(f); }
		{ FILE* f=fopen("tkdata.dat","wb");
		  fwrite(trak->tkdata,trak->tkdata_len,1,f);
		  fclose(f); }
#endif
		// Emulate WAVEFORMATEX struct:
		sh->wf=malloc(sizeof(WAVEFORMATEX));
		memset(sh->wf,0,sizeof(WAVEFORMATEX));
		sh->wf->nChannels=sh->channels;
		sh->wf->wBitsPerSample=(trak->stdata[18]<<8)+trak->stdata[19];
		// sh->wf->nSamplesPerSec=trak->timescale;
		sh->wf->nSamplesPerSec=(trak->stdata[24]<<8)+trak->stdata[25];
		if(trak->stdata_len >= 44 && trak->stdata[9]>=1 && char2int(trak->stdata,28)>0){
		//Audio header: samp/pack=4096 bytes/pack=743 bytes/frame=1486 bytes/samp=2
		  sh->wf->nAvgBytesPerSec=(sh->wf->nChannels*sh->wf->nSamplesPerSec*
		      char2int(trak->stdata,32)+char2int(trak->stdata,28)/2)
		      /char2int(trak->stdata,28);
		  sh->wf->nBlockAlign=char2int(trak->stdata,36);
		} else {
		  sh->wf->nAvgBytesPerSec=sh->wf->nChannels*sh->wf->wBitsPerSample*sh->wf->nSamplesPerSec/8;
		  // workaround for ms11 ima4
		  if (sh->format == 0x1100736d && trak->stdata_len >= 36)
		      sh->wf->nBlockAlign=char2int(trak->stdata,36);
		}
		// Selection:
//		if(demuxer->audio->id==-1 || demuxer->audio->id==priv->track_db){
//		    // (auto)selected audio track:
//		    demuxer->audio->id=priv->track_db;
//		    demuxer->audio->sh=sh; sh->ds=demuxer->audio;
//		}
		break;
	    }
	    case MOV_TRAK_VIDEO: {
		int i, entry;
		int flag, start, count_flag, end, palette_count, gray;
		int hdr_ptr = 76;  // the byte just after depth
		unsigned char *palette_map;
		sh_video_t* sh=new_sh_video(demuxer,priv->track_db);
		int depth = trak->stdata[75]|(trak->stdata[74]<<8);
		sh->format=trak->fourcc;

//  stdata[]:
//	8   short	version
//	10  short	revision
//	12  int		vendor_id
//	16  int		temporal_quality
//	20  int		spatial_quality
//	24  short	width
//	26  short	height
//	28  int		h_dpi
//	32  int		v_dpi
//	36  int		0
//	40  short	frames_per_sample
//	42  char[4]	compressor_name
//	74  short	depth
//	76  short	color_table_id
// additional atoms may follow,
// eg esds atom from .MP4 files
//      78  int		atom size
//      82  char[4]	atom type
//	86  ...		atom data

	{	ImageDescription* id=malloc(8+trak->stdata_len);
		trak->desc=id;
		id->idSize=8+trak->stdata_len;
//		id->cType=bswap_32(trak->fourcc);
		id->cType=le2me_32(trak->fourcc);
		id->version=char2short(trak->stdata,8);
		id->revisionLevel=char2short(trak->stdata,10);
		id->vendor=char2int(trak->stdata,12);
		id->temporalQuality=char2int(trak->stdata,16);
		id->spatialQuality=char2int(trak->stdata,20);
		id->width=char2short(trak->stdata,24);
		id->height=char2short(trak->stdata,26);
		id->hRes=char2int(trak->stdata,28);
		id->vRes=char2int(trak->stdata,32);
		id->dataSize=char2int(trak->stdata,36);
		id->frameCount=char2short(trak->stdata,40);
		memcpy(&id->name,trak->stdata+42,32);
		id->depth=char2short(trak->stdata,74);
		id->clutID=char2short(trak->stdata,76);
		if(trak->stdata_len>78)	memcpy(((char*)&id->clutID)+2,trak->stdata+78,trak->stdata_len-78);
		sh->ImageDesc=id;
#if 0
		{   FILE *f=fopen("ImageDescription","wb");
		    fwrite(id,id->idSize,1,f);
		    fclose(f);
		}
#endif
	}

		if(trak->stdata_len >= 86) { // extra atoms found
		  int pos=78;
		  int atom_len;
		  while(pos+8<=trak->stdata_len &&
		    (pos+(atom_len=char2int(trak->stdata,pos)))<=trak->stdata_len){
		   switch(char2int(trak->stdata,pos+4)) { // switch atom type
		    case MOV_FOURCC('g','a','m','a'):
		      // intfp with gamma value at which movie was captured
		      // can be used to gamma correct movie display
		      mp_msg(MSGT_DEMUX, MSGL_INFO, "MOV: Found unsupported Gamma-Correction movie atom (%d)!\n",
			  atom_len);
		      break;
		    case MOV_FOURCC('f','i','e','l'):
		      // 2 char-values (8bit int) that specify field handling
		      // see the Apple's QuickTime Fileformat PDF for more info
		      mp_msg(MSGT_DEMUX, MSGL_INFO, "MOV: Found unsupported Field-Handling movie atom (%d)!\n",
			  atom_len);
		      break;
		    case MOV_FOURCC('m','j','q','t'):
		      // Motion-JPEG default quantization table
		      mp_msg(MSGT_DEMUX, MSGL_INFO, "MOV: Found unsupported MJPEG-Quantization movie atom (%d)!\n",
			  atom_len);
		      break;
		    case MOV_FOURCC('m','j','h','t'):
		      // Motion-JPEG default huffman table
		      mp_msg(MSGT_DEMUX, MSGL_INFO, "MOV: Found unsupported MJPEG-Huffman movie atom (%d)!\n",
			  atom_len);
		      break;
		    case MOV_FOURCC('e','s','d','s'):
		      // MPEG4 Elementary Stream Descriptor header
		      mp_msg(MSGT_DEMUX, MSGL_INFO, "MOV: Found MPEG4 movie Elementary Stream Descriptor atom (%d)!\n", atom_len);
		      // add code here to save esds header of length atom_len-8
		      // beginning at stdata[86] to some variable to pass it
		      // on to the decoder ::atmos
		      if(atom_len > 8) {
      			esds_t esds; 				  
			if(!mp4_parse_esds(trak->stdata+pos+8, atom_len-8, &esds)) {
    
			  if(esds.objectTypeId==MP4OTI_MPEG2VisualSimple || esds.objectTypeId==MP4OTI_MPEG2VisualMain ||
			     esds.objectTypeId==MP4OTI_MPEG2VisualSNR || esds.objectTypeId==MP4OTI_MPEG2VisualSpatial ||
			     esds.objectTypeId==MP4OTI_MPEG2VisualHigh || esds.objectTypeId==MP4OTI_MPEG2Visual422)
			    sh->format=mmioFOURCC('m', 'p', 'g', '2');
			  else if(esds.objectTypeId==MP4OTI_MPEG1Visual)
			    sh->format=mmioFOURCC('m', 'p', 'g', '1');

			  // dump away the codec specific configuration for the AAC decoder
			  trak->stream_header_len = esds.decoderConfigLen;
			  trak->stream_header = (unsigned char *)malloc(trak->stream_header_len);
			  memcpy(trak->stream_header, esds.decoderConfig, trak->stream_header_len);
			}
			mp4_free_esds(&esds); // freeup esds mem
		      }	      
		      break;
		    case MOV_FOURCC('a','v','c','C'):
		      // AVC decoder configuration record
		      mp_msg(MSGT_DEMUX, MSGL_INFO, "MOV: AVC decoder configuration record atom (%d)!\n", atom_len);
		      if(atom_len > 8) {
		        int i, poffs, cnt;
		        // Parse some parts of avcC, just for fun :)
		        // real parsing is done by avc1 decoder
		        mp_msg(MSGT_DEMUX, MSGL_V, "MOV: avcC version: %d\n", *(trak->stdata+pos+8));
		        if (*(trak->stdata+pos+8) != 1)
		          mp_msg(MSGT_DEMUX, MSGL_ERR, "MOV: unknown avcC version (%d). Expexct problems.\n", *(trak->stdata+pos+9));
		        mp_msg(MSGT_DEMUX, MSGL_V, "MOV: avcC profile: %d\n", *(trak->stdata+pos+9));
		        mp_msg(MSGT_DEMUX, MSGL_V, "MOV: avcC profile compatibility: %d\n", *(trak->stdata+pos+10));
		        mp_msg(MSGT_DEMUX, MSGL_V, "MOV: avcC level: %d\n", *(trak->stdata+pos+11));
		        mp_msg(MSGT_DEMUX, MSGL_V, "MOV: avcC nal length size: %d\n", ((*(trak->stdata+pos+12))&0x03)+1);
		        mp_msg(MSGT_DEMUX, MSGL_V, "MOV: avcC number of sequence param sets: %d\n", cnt = (*(trak->stdata+pos+13) & 0x1f));
		        poffs = pos + 14;
		        for (i = 0; i < cnt; i++) {
		          mp_msg(MSGT_DEMUX, MSGL_V, "MOV: avcC sps %d have length %d\n", i, BE_16(trak->stdata+poffs));
		          poffs += BE_16(trak->stdata+poffs) + 2;
		        }
		        mp_msg(MSGT_DEMUX, MSGL_V, "MOV: avcC number of picture param sets: %d\n", *(trak->stdata+poffs));
		        poffs++;
		        for (i = 0; i < cnt; i++) {
		          mp_msg(MSGT_DEMUX, MSGL_V, "MOV: avcC pps %d have length %d\n", i, BE_16(trak->stdata+poffs));
		          poffs += BE_16(trak->stdata+poffs) + 2;
		        }
		        // Copy avcC for the AVC decoder
		        // This data will be put in extradata below, where BITMAPINFOHEADER is created
		        trak->stream_header_len = atom_len-8;
		        trak->stream_header = (unsigned char *)malloc(trak->stream_header_len);
		        memcpy(trak->stream_header, trak->stdata+pos+8, trak->stream_header_len);
		      }	      
		      break;
		    case 0:
		      break;
		    default:
	      	      mp_msg(MSGT_DEMUX, MSGL_INFO, "MOV: Found unknown movie atom %c%c%c%c (%d)!\n",
	      		  trak->stdata[pos+4],trak->stdata[pos+5],trak->stdata[pos+6],trak->stdata[pos+7],
	      		  atom_len);
		   }
		   if(atom_len<8) break;
		   pos+=atom_len;
//		   printf("pos=%d max=%d\n",pos,trak->stdata_len);
		  }
		}
		sh->fps=trak->timescale/
		    ((trak->durmap_size>=1)?(float)trak->durmap[0].dur:1);
		sh->frametime=1.0f/sh->fps;

		sh->disp_w=trak->stdata[25]|(trak->stdata[24]<<8);
		sh->disp_h=trak->stdata[27]|(trak->stdata[26]<<8);
		// if image size is zero, fallback to display size
		if(!sh->disp_w && !sh->disp_h) {
		  sh->disp_w=trak->tkdata[77]|(trak->tkdata[76]<<8);
		  sh->disp_h=trak->tkdata[81]|(trak->tkdata[80]<<8);
		} else if(sh->disp_w!=(trak->tkdata[77]|(trak->tkdata[76]<<8))){
		  // codec and display width differ... use display one for aspect
		  sh->aspect=trak->tkdata[77]|(trak->tkdata[76]<<8);
		  sh->aspect/=trak->tkdata[81]|(trak->tkdata[80]<<8);
		}
		
		if(depth>32+8) printf("*** depth = 0x%X\n",depth);

		// palettized?
		gray = 0;
		if (depth > 32) { depth&=31; gray = 1; } // depth > 32 means grayscale
		if ((depth == 2) || (depth == 4) || (depth == 8))
		  palette_count = (1 << depth);
		else
		  palette_count = 0;

		// emulate BITMAPINFOHEADER:
		if (palette_count)
		{
		  sh->bih=malloc(sizeof(BITMAPINFOHEADER) + palette_count * 4);
		  memset(sh->bih,0,sizeof(BITMAPINFOHEADER) + palette_count * 4);
		  sh->bih->biSize=40 + palette_count * 4;
		  // fetch the relevant fields
		  flag = BE_16(&trak->stdata[hdr_ptr]);
		  hdr_ptr += 2;
		  start = BE_32(&trak->stdata[hdr_ptr]);
		  hdr_ptr += 4;
		  count_flag = BE_16(&trak->stdata[hdr_ptr]);
		  hdr_ptr += 2;
		  end = BE_16(&trak->stdata[hdr_ptr]);
		  hdr_ptr += 2;
		  palette_map = (unsigned char *)sh->bih + 40;
		  mp_msg(MSGT_DEMUX, MSGL_INFO, "Allocated %d entries for palette\n",
		    palette_count);
		  mp_msg(MSGT_DEMUX, MSGL_DBG2, "QT palette: start: %x, end: %x, count flag: %d, flags: %x\n",
		    start, end, count_flag, flag);

		  /* XXX: problems with sample (statunit6.mov) with flag&0x4 set! - alex*/

		  // load default palette
		  if (flag & 0x08)
		  {
		    if (gray)
		    {
		      mp_msg(MSGT_DEMUX, MSGL_INFO, "Using default QT grayscale palette\n");
		      if (palette_count == 16)
		        memcpy(palette_map, qt_default_grayscale_palette_16, 16 * 4);
		      else if (palette_count == 256) {
		        memcpy(palette_map, qt_default_grayscale_palette_256, 256 * 4);
		        if (trak->fourcc == mmioFOURCC('c','v','i','d')) {
		          int i;
		          // Hack for grayscale CVID, negative palette
		          // If you have samples where this is not required contact me (rxt)
		          mp_msg(MSGT_DEMUX, MSGL_INFO, "MOV: greyscale cvid with default palette,"
		            " enabling negative palette hack.\n");
		          for (i = 0; i < 256 * 4; i++)
		            palette_map[i] = palette_map[i] ^ 0xff;
		        }
		      }
		    }
		    else
		    {
		      mp_msg(MSGT_DEMUX, MSGL_INFO, "Using default QT colour palette\n");
		      if (palette_count == 4)
		        memcpy(palette_map, qt_default_palette_4, 4 * 4);
		      else if (palette_count == 16)
		        memcpy(palette_map, qt_default_palette_16, 16 * 4);
		      else if (palette_count == 256)
		        memcpy(palette_map, qt_default_palette_256, 256 * 4);
		    }
		  }
		  // load palette from file
		  else
		  {
		    mp_msg(MSGT_DEMUX, MSGL_INFO, "Loading palette from file\n");
		    for (i = start; i <= end; i++)
		    {
		      entry = BE_16(&trak->stdata[hdr_ptr]);
		      hdr_ptr += 2;
		      // apparently, if count_flag is set, entry is same as i
		      if (count_flag & 0x8000)
		        entry = i;
		      // only care about top 8 bits of 16-bit R, G, or B value
		      if (entry <= palette_count && entry >= 0)
		      {
		        palette_map[entry * 4 + 2] = trak->stdata[hdr_ptr + 0];
		        palette_map[entry * 4 + 1] = trak->stdata[hdr_ptr + 2];
		        palette_map[entry * 4 + 0] = trak->stdata[hdr_ptr + 4];
		        mp_dbg(MSGT_DEMUX, MSGL_DBG2, "QT palette: added entry: %d of %d (colors: R:%x G:%x B:%x)\n",
			    entry, palette_count,
			    palette_map[entry * 4 + 2], 
			    palette_map[entry * 4 + 1],
			    palette_map[entry * 4 + 0]);
		      }
		      else
		        mp_msg(MSGT_DEMUX, MSGL_V, "QT palette: skipped entry (out of count): %d of %d\n",
			    entry, palette_count);
		      hdr_ptr += 6;
		    }
		  }
		}
		else
		{
		 if (trak->fourcc == mmioFOURCC('a','v','c','1')) {
		  sh->bih=malloc(sizeof(BITMAPINFOHEADER) + trak->stream_header_len);
		  memset(sh->bih,0,sizeof(BITMAPINFOHEADER) + trak->stream_header_len);
		  sh->bih->biSize=40  + trak->stream_header_len;
		  memcpy(((unsigned char *)sh->bih)+40,  trak->stream_header, trak->stream_header_len);
		  free (trak->stream_header);
		  trak->stream_header_len = 0;
		  trak->stream_header = NULL;
		 } else {
		  sh->bih=malloc(sizeof(BITMAPINFOHEADER));
		  memset(sh->bih,0,sizeof(BITMAPINFOHEADER));
		  sh->bih->biSize=40;
		 }
		}
		sh->bih->biWidth=sh->disp_w;
		sh->bih->biHeight=sh->disp_h;
		sh->bih->biPlanes=0;
		sh->bih->biBitCount=depth;
		sh->bih->biCompression=trak->fourcc;
		sh->bih->biSizeImage=sh->bih->biWidth*sh->bih->biHeight;

		mp_msg(MSGT_DEMUX, MSGL_INFO, "Image size: %d x %d (%d bpp)\n",sh->disp_w,sh->disp_h,sh->bih->biBitCount);
		if(trak->tkdata_len>81)
		mp_msg(MSGT_DEMUX, MSGL_INFO, "Display size: %d x %d\n",
		    trak->tkdata[77]|(trak->tkdata[76]<<8),
		    trak->tkdata[81]|(trak->tkdata[80]<<8));
		mp_msg(MSGT_DEMUX, MSGL_INFO, "Fourcc: %.4s  Codec: '%.*s'\n",&trak->fourcc,trak->stdata[42]&31,trak->stdata+43);
		
//		if(demuxer->video->id==-1 || demuxer->video->id==priv->track_db){
//		    // (auto)selected video track:
//		    demuxer->video->id=priv->track_db;
//		    demuxer->video->sh=sh; sh->ds=demuxer->video;
//		}
		break;
	    }
	    case MOV_TRAK_GENERIC:
		mp_msg(MSGT_DEMUX, MSGL_INFO, "Generic track - not completely understood! (id: %d)\n",
		    trak->id);
		/* XXX: Also this contains the FLASH data */

#if 0
	    {
		int pos = stream_tell(demuxer->stream);
		int i;
		int fd;
		char name[20];
	
		for (i=0; i<trak->samples_size; i++)
		{
		    char buf[trak->samples[i].size];
		    stream_seek(demuxer->stream, trak->samples[i].pos);
		    snprintf((char *)&name[0], 20, "samp%d", i);
		    fd = open((char *)&name[0], O_CREAT|O_WRONLY);
		    stream_read(demuxer->stream, &buf[0], trak->samples[i].size);
		    write(fd, &buf[0], trak->samples[i].size);
		    close(fd);
		 }
		for (i=0; i<trak->chunks_size; i++)
		{
		    char buf[trak->length];
		    stream_seek(demuxer->stream, trak->chunks[i].pos);
		    snprintf((char *)&name[0], 20, "chunk%d", i);
		    fd = open((char *)&name[0], O_CREAT|O_WRONLY);
		    stream_read(demuxer->stream, &buf[0], trak->length);
		    write(fd, &buf[0], trak->length);
		    close(fd);
		 }
		 if (trak->samplesize > 0)
		 {
		    char *buf;
		    
		    buf = malloc(trak->samplesize);
		    stream_seek(demuxer->stream, trak->chunks[0].pos);
		    snprintf((char *)&name[0], 20, "trak%d", trak->id);
		    fd = open((char *)&name[0], O_CREAT|O_WRONLY);
		    stream_read(demuxer->stream, buf, trak->samplesize);
		    write(fd, buf, trak->samplesize);
		    close(fd);
		 }
		 stream_seek(demuxer->stream, pos);
	    }		
#endif
		break;
	    default:
		mp_msg(MSGT_DEMUX, MSGL_INFO, "Unknown track type found (type: %d)\n", trak->type);
		break;
	    }
	    mp_msg(MSGT_DEMUX, MSGL_INFO, "--------------\n");
	    priv->track_db++;
	    trak=NULL;
	    break;
	}
#ifndef HAVE_ZLIB
	case MOV_FOURCC('c','m','o','v'): {
	    mp_msg(MSGT_DEMUX,MSGL_ERR,MSGTR_MOVcomprhdr);
	    return;
	}
#else
	case MOV_FOURCC('m','o','o','v'):
	case MOV_FOURCC('c','m','o','v'): {
//	    mp_msg(MSGT_DEMUX,MSGL_ERR,MSGTR_MOVcomprhdr);
	    lschunks(demuxer,level+1,pos+len,NULL);
	    break;
	}
	case MOV_FOURCC('d','c','o','m'): {
//	    int temp=stream_read_dword(demuxer->stream);
	    unsigned int algo=be2me_32(stream_read_dword(demuxer->stream));
	    mp_msg(MSGT_DEMUX, MSGL_INFO, "Compressed header uses %.4s algo!\n",&algo);
	    break;
	}
	case MOV_FOURCC('c','m','v','d'): {
//	    int temp=stream_read_dword(demuxer->stream);
	    unsigned int moov_sz=stream_read_dword(demuxer->stream);
	    unsigned int cmov_sz=len-4;
	    unsigned char* cmov_buf=malloc(cmov_sz);
	    unsigned char* moov_buf=malloc(moov_sz+16);
	    int zret;
	    z_stream zstrm;
	    stream_t* backup;

	    mp_msg(MSGT_DEMUX, MSGL_INFO, "Compressed header size: %d / %d\n",cmov_sz,moov_sz);

	    stream_read(demuxer->stream,cmov_buf,cmov_sz);

	      zstrm.zalloc          = (alloc_func)0;
	      zstrm.zfree           = (free_func)0;
	      zstrm.opaque          = (voidpf)0;
	      zstrm.next_in         = cmov_buf;
	      zstrm.avail_in        = cmov_sz;
	      zstrm.next_out        = moov_buf;
	      zstrm.avail_out       = moov_sz;
	    
	      zret = inflateInit(&zstrm);
	      if (zret != Z_OK)
		{ mp_msg(MSGT_DEMUX, MSGL_ERR, "QT cmov: inflateInit err %d\n",zret);
		return;
		}
	      zret = inflate(&zstrm, Z_NO_FLUSH);
	      if ((zret != Z_OK) && (zret != Z_STREAM_END))
		{ mp_msg(MSGT_DEMUX, MSGL_ERR, "QT cmov inflate: ERR %d\n",zret);
		return;
		}
#if 0
	      else {
		FILE *DecOut;
		DecOut = fopen("Out.bin", "w");
		fwrite(moov_buf, 1, moov_sz, DecOut);
		fclose(DecOut);
	      }
#endif
	      if(moov_sz != zstrm.total_out)
	        mp_msg(MSGT_DEMUX, MSGL_WARN, "Warning! moov size differs cmov: %d  zlib: %d\n",moov_sz,zstrm.total_out);
	      zret = inflateEnd(&zstrm);
	      
	      backup=demuxer->stream;
	       demuxer->stream=new_memory_stream(moov_buf,moov_sz);
	       stream_skip(demuxer->stream,8);
	       lschunks(demuxer,level+1,moov_sz,NULL); // parse uncompr. 'moov'
	       //free_stream(demuxer->stream);
	      demuxer->stream=backup;
	      free(cmov_buf);
	      free(moov_buf);	    
	      break;
	}
#endif
	case MOV_FOURCC('u','d','t','a'):
	{
	    unsigned int udta_id;
	    off_t udta_len;
	    off_t udta_size = len;
	
	    mp_msg(MSGT_DEMUX, MSGL_DBG2, "mov: user data record found\n");
	    mp_msg(MSGT_DEMUX, MSGL_V, "Quicktime Clip Info:\n");

	    while((len > 8) && (udta_size > 8))
	    {
		udta_len = stream_read_dword(demuxer->stream);
		udta_id = stream_read_dword(demuxer->stream);
		udta_size -= 8;
		mp_msg(MSGT_DEMUX, MSGL_DBG2, "udta_id: %.4s (len: %d)\n", &udta_id, udta_len);
		switch (udta_id)
		{
		    case MOV_FOURCC(0xa9,'c','p','y'):
		    case MOV_FOURCC(0xa9,'d','a','y'):
		    case MOV_FOURCC(0xa9,'d','i','r'):
		    /* 0xa9,'e','d','1' - '9' : edit timestamps */
		    case MOV_FOURCC(0xa9,'f','m','t'):
		    case MOV_FOURCC(0xa9,'i','n','f'):
		    case MOV_FOURCC(0xa9,'p','r','d'):
		    case MOV_FOURCC(0xa9,'p','r','f'):
		    case MOV_FOURCC(0xa9,'r','e','q'):
		    case MOV_FOURCC(0xa9,'s','r','c'):
		    case MOV_FOURCC('n','a','m','e'):
		    case MOV_FOURCC(0xa9,'n','a','m'):
		    case MOV_FOURCC(0xa9,'A','R','T'):
		    case MOV_FOURCC(0xa9,'c','m','t'):
		    case MOV_FOURCC(0xa9,'a','u','t'):
		    case MOV_FOURCC(0xa9,'s','w','r'):
		    {
			off_t text_len = stream_read_word(demuxer->stream);
			char text[text_len+2+1];
			stream_read(demuxer->stream, (char *)&text, text_len+2);
			text[text_len+2] = 0x0;
			switch(udta_id)
			{
			    case MOV_FOURCC(0xa9,'a','u','t'):
				demux_info_add(demuxer, "author", &text[2]);
				mp_msg(MSGT_DEMUX, MSGL_V, " Author: %s\n", &text[2]);
				break;
			    case MOV_FOURCC(0xa9,'c','p','y'):
				demux_info_add(demuxer, "copyright", &text[2]);
				mp_msg(MSGT_DEMUX, MSGL_V, " Copyright: %s\n", &text[2]);
				break;
			    case MOV_FOURCC(0xa9,'i','n','f'):
				mp_msg(MSGT_DEMUX, MSGL_V, " Info: %s\n", &text[2]);
				break;
			    case MOV_FOURCC('n','a','m','e'):
			    case MOV_FOURCC(0xa9,'n','a','m'):
				demux_info_add(demuxer, "name", &text[2]);
				mp_msg(MSGT_DEMUX, MSGL_V, " Name: %s\n", &text[2]);
				break;
			    case MOV_FOURCC(0xa9,'A','R','T'):
				mp_msg(MSGT_DEMUX, MSGL_V, " Artist: %s\n", &text[2]);
				break;
			    case MOV_FOURCC(0xa9,'d','i','r'):
				mp_msg(MSGT_DEMUX, MSGL_V, " Director: %s\n", &text[2]);
				break;
			    case MOV_FOURCC(0xa9,'c','m','t'):
				demux_info_add(demuxer, "comments", &text[2]);
				mp_msg(MSGT_DEMUX, MSGL_V, " Comment: %s\n", &text[2]);
				break;
			    case MOV_FOURCC(0xa9,'r','e','q'):
				mp_msg(MSGT_DEMUX, MSGL_V, " Requirements: %s\n", &text[2]);
				break;
			    case MOV_FOURCC(0xa9,'s','w','r'):
				demux_info_add(demuxer, "encoder", &text[2]);
				mp_msg(MSGT_DEMUX, MSGL_V, " Software: %s\n", &text[2]);
				break;
			    case MOV_FOURCC(0xa9,'d','a','y'):
				mp_msg(MSGT_DEMUX, MSGL_V, " Creation timestamp: %s\n", &text[2]);
				break;
			    case MOV_FOURCC(0xa9,'f','m','t'):
				mp_msg(MSGT_DEMUX, MSGL_V, " Format: %s\n", &text[2]);
				break;
			    case MOV_FOURCC(0xa9,'p','r','d'):
				mp_msg(MSGT_DEMUX, MSGL_V, " Producer: %s\n", &text[2]);
				break;
			    case MOV_FOURCC(0xa9,'p','r','f'):
				mp_msg(MSGT_DEMUX, MSGL_V, " Performer(s): %s\n", &text[2]);
				break;
			    case MOV_FOURCC(0xa9,'s','r','c'):
				mp_msg(MSGT_DEMUX, MSGL_V, " Source providers: %s\n", &text[2]);
				break;
			}
			udta_size -= 4+text_len;
			break;
		    }
		    /* some other shits:    WLOC - window location,
					    LOOP - looping style,
					    SelO - play only selected frames
					    AllF - play all frames
		    */
		    case MOV_FOURCC('W','L','O','C'):
		    case MOV_FOURCC('L','O','O','P'):
		    case MOV_FOURCC('S','e','l','O'):
		    case MOV_FOURCC('A','l','l','F'):
		    default:
		    {
			if( udta_len>udta_size)
				udta_len=udta_size;
			{
			char dump[udta_len-4];
			stream_read(demuxer->stream, (char *)&dump, udta_len-4-4);
			udta_size -= udta_len;
			}
		    }
		}
	    }
	    break;
	} /* eof udta */
	default:
	  id = be2me_32(id);
	  mp_msg(MSGT_DEMUX,MSGL_V,"MOV: unknown chunk: %.4s %d\n",&id,(int)len);
	} /* endof switch */
	} /* endof else */

	pos+=len+8;
	if(pos>=endpos) break;
	if(!stream_seek(demuxer->stream,pos)) break;
    }
}

int mov_read_header(demuxer_t* demuxer){
    mov_priv_t* priv=demuxer->priv;
    int t_no;
    int best_a_id=-1, best_a_len=0;
    int best_v_id=-1, best_v_len=0;
    
    mp_msg(MSGT_DEMUX, MSGL_DBG3, "mov_read_header!\n");

    // Parse header:    
    stream_reset(demuxer->stream);
    if(!stream_seek(demuxer->stream,priv->moov_start))
    {
	mp_msg(MSGT_DEMUX,MSGL_ERR,"MOV: Cannot seek to the beginning of the Movie header (0x%x)\n",
	    priv->moov_start);
	return 0;
    }
    lschunks(demuxer, 0, priv->moov_end, NULL);
    // just in case we have hit eof while parsing...
    demuxer->stream->eof = 0;
//    mp_msg(MSGT_DEMUX, MSGL_INFO, "--------------\n");

    // find the best (longest) streams:
    for(t_no=0;t_no<priv->track_db;t_no++){
        mov_track_t* trak=priv->tracks[t_no];
	int len=(trak->samplesize) ? trak->chunks_size : trak->samples_size;
	if(demuxer->a_streams[t_no]){ // need audio
	    if(len>best_a_len){	best_a_len=len; best_a_id=t_no; }
	}
	if(demuxer->v_streams[t_no]){ // need video
	    if(len>best_v_len){	best_v_len=len; best_v_id=t_no; }
	}
    }
    mp_msg(MSGT_DEMUX, MSGL_INFO, "MOV: longest streams: A: #%d (%d samples)  V: #%d (%d samples)\n",
	best_a_id,best_a_len,best_v_id,best_v_len);
    if(demuxer->audio->id==-1 && best_a_id>=0) demuxer->audio->id=best_a_id;
    if(demuxer->video->id==-1 && best_v_id>=0) demuxer->video->id=best_v_id;
    
    // setup sh pointers:
    if(demuxer->audio->id>=0){
	sh_audio_t* sh=demuxer->a_streams[demuxer->audio->id];
	if(sh){
	    demuxer->audio->sh=sh; sh->ds=demuxer->audio;
	} else {
	    mp_msg(MSGT_DEMUX, MSGL_ERR, "MOV: selected audio stream (%d) does not exists\n",demuxer->audio->id);
	    demuxer->audio->id=-2;
	}
    }
    if(demuxer->video->id>=0){
	sh_video_t* sh=demuxer->v_streams[demuxer->video->id];
	if(sh){
	    demuxer->video->sh=sh; sh->ds=demuxer->video;
	} else {
	    mp_msg(MSGT_DEMUX, MSGL_ERR, "MOV: selected video stream (%d) does not exists\n",demuxer->video->id);
	    demuxer->video->id=-2;
	}
    }

#if 1
    if(verbose>2){
	for(t_no=0;t_no<priv->track_db;t_no++){
	    mov_track_t* trak=priv->tracks[t_no];
	    if(trak->type==MOV_TRAK_GENERIC){
		int i;
		int fd;
		char name[20];
		mp_msg(MSGT_DEMUX, MSGL_INFO, "MOV: Track #%d: Extracting %d data chunks to files\n",t_no,trak->samples_size);
		for (i=0; i<trak->samples_size; i++)
		{
		    int len=trak->samples[i].size;
		    char buf[len];
		    stream_seek(demuxer->stream, trak->samples[i].pos);
		    snprintf(name, 20, "t%02d-s%03d.%s", t_no,i,
			(trak->media_handler==MOV_FOURCC('f','l','s','h')) ?
			    "swf":"dump");
		    fd = open(name, O_CREAT|O_WRONLY);
//		    { int j;
//			for(j=0;j<trak->stdata_len-3; j++)
//			    printf("stdata[%d]=0x%X ize=0x%X\n",j,char2int(trak->stdata,j),MOV_FOURCC('z','l','i','b'));
//		    }
		    if( //trak->media_handler==MOV_FOURCC('s','p','r','t') &&
			trak->stdata_len>=16 &&
			char2int(trak->stdata,12)==MOV_FOURCC('z','l','i','b') 
		    ){
			int newlen=stream_read_dword(demuxer->stream);
#ifdef HAVE_ZLIB
			// unzip:
			z_stream zstrm;
			int zret;
			char buf2[newlen];

			len-=4;
			stream_read(demuxer->stream, buf, len);

	                zstrm.zalloc          = (alloc_func)0;
	                zstrm.zfree           = (free_func)0;
	                zstrm.opaque          = (voidpf)0;
	                zstrm.next_in         = buf;
	                zstrm.avail_in        = len;
	                zstrm.next_out        = buf2;
	                zstrm.avail_out       = newlen;

	                zret = inflateInit(&zstrm);
			zret = inflate(&zstrm, Z_NO_FLUSH);
			if(newlen != zstrm.total_out)
	    		    mp_msg(MSGT_DEMUX, MSGL_WARN, "Warning! unzipped frame size differs hdr: %d  zlib: %d\n",newlen,zstrm.total_out);
			
			write(fd, buf2, newlen);
		    } else {
#else
			len-=4;
			printf("******* ZLIB COMPRESSED SAMPLE!!!!! (%d->%d bytes) *******\n",len,newlen);
		    }
		    {
#endif
			stream_read(demuxer->stream, buf, len);
			write(fd, buf, len);
		    }
		    close(fd);
		}
	    }
	}
    }
    demuxer->stream->eof = 0;
#endif

    return 1;
}

/**
 * \brief return the mov track that belongs to a demuxer stream
 * \param ds the demuxer stream, may be NULL
 * \return the mov track info structure belonging to the stream,
 *          NULL if not found
 */
static mov_track_t *stream_track(mov_priv_t *priv, demux_stream_t *ds) {
  if (ds && (ds->id >= 0) && (ds->id < priv->track_db))
    return priv->tracks[ds->id];
  return NULL;
}

// return value:
//     0 = EOF or no stream found
//     1 = successfully read a packet
int demux_mov_fill_buffer(demuxer_t *demuxer,demux_stream_t* ds){
    mov_priv_t* priv=demuxer->priv;
    mov_track_t* trak=NULL;
    float pts;
    int x;
    off_t pos;
    
    trak = stream_track(priv, ds);
    if (!trak) return 0;

if(trak->samplesize){
    // read chunk:
    if(trak->pos>=trak->chunks_size) return 0; // EOF
    stream_seek(demuxer->stream,trak->chunks[trak->pos].pos);
    pts=(float)(trak->chunks[trak->pos].sample*trak->duration)/(float)trak->timescale;
    if(trak->samplesize!=1)
    {
	mp_msg(MSGT_DEMUX, MSGL_DBG2, "WARNING! Samplesize(%d) != 1\n",
	    trak->samplesize);
	x=trak->chunks[trak->pos].size*trak->samplesize;
    }
    else
	x=trak->chunks[trak->pos].size;
//    printf("X = %d\n", x);
    /* the following stuff is audio related */
    if (trak->type == MOV_TRAK_AUDIO){
      if(trak->stdata_len>=44 && trak->stdata[9]>=1 && char2int(trak->stdata,28)>0){
        // stsd version 1 - we have audio compression ratio info:
	x/=char2int(trak->stdata,28); // samples/packet
//	x*=char2int(trak->stdata,32); // bytes/packet
	x*=char2int(trak->stdata,36); // bytes/frame
      } else {
	if(ds->ss_div && ds->ss_mul){
	    // workaround for buggy files like 7up-high-traffic-areas.mov,
	    // with missing stsd v1 header containing compression rate
	    x/=ds->ss_div; x*=ds->ss_mul; // compression ratio fix  ! HACK !
	} else {
	    x*=trak->nchannels;
	    x*=trak->samplebytes;
	}
      }
      mp_msg(MSGT_DEMUX, MSGL_DBG2, "Audio sample %d bytes pts %5.3f\n",trak->chunks[trak->pos].size*trak->samplesize,pts);
    } /* MOV_TRAK_AUDIO */
    pos=trak->chunks[trak->pos].pos;
} else {
    int frame=trak->pos;
    // editlist support:
    if(trak->type == MOV_TRAK_VIDEO && trak->editlist_size>=1){
	// find the right editlist entry:
	if(frame<trak->editlist[trak->editlist_pos].start_frame)
	    trak->editlist_pos=0;
	while(trak->editlist_pos<trak->editlist_size-1 &&
	    frame>=trak->editlist[trak->editlist_pos+1].start_frame)
		++trak->editlist_pos;
	if(frame>=trak->editlist[trak->editlist_pos].start_frame+
	    trak->editlist[trak->editlist_pos].frames) return 0; // EOF
	// calc real frame index:
	frame-=trak->editlist[trak->editlist_pos].start_frame;
	frame+=trak->editlist[trak->editlist_pos].start_sample;
	// calc pts:
	pts=(float)(trak->samples[frame].pts+
	    trak->editlist[trak->editlist_pos].pts_offset)/(float)trak->timescale;
    } else {
	if(frame>=trak->samples_size) return 0; // EOF
	pts=(float)trak->samples[frame].pts/(float)trak->timescale;
    }
    // read sample:
    stream_seek(demuxer->stream,trak->samples[frame].pos);
    x=trak->samples[frame].size;
    pos=trak->samples[frame].pos;
}
if(trak->pos==0 && trak->stream_header_len>0){
    // we have to append the stream header...
    demux_packet_t* dp=new_demux_packet(x+trak->stream_header_len);
    memcpy(dp->buffer,trak->stream_header,trak->stream_header_len);
    stream_read(demuxer->stream,dp->buffer+trak->stream_header_len,x);
    free(trak->stream_header);
    trak->stream_header = NULL;
    trak->stream_header_len = 0;
    dp->pts=pts;
    dp->flags=0;
    dp->pos=pos; // FIXME?
    ds_add_packet(ds,dp);
} else
    ds_read_packet(ds,demuxer->stream,x,pts,pos,0);
    
    ++trak->pos;

    return 1;
    
}

static float mov_seek_track(mov_track_t* trak,float pts,int flags){

//    printf("MOV track seek called  %5.3f  \n",pts);
    if(flags&2) pts*=trak->length; else pts*=(float)trak->timescale;

if(trak->samplesize){
    int sample=pts/trak->duration;
//    printf("MOV track seek - chunk: %d  (pts: %5.3f  dur=%d)  \n",sample,pts,trak->duration);
    if(!(flags&1)) sample+=trak->chunks[trak->pos].sample; // relative
    trak->pos=0;
    while(trak->pos<trak->chunks_size && trak->chunks[trak->pos].sample<sample) ++trak->pos;
    pts=(float)(trak->chunks[trak->pos].sample*trak->duration)/(float)trak->timescale;
} else {
    unsigned int ipts;
    if(!(flags&1)) pts+=trak->samples[trak->pos].pts;
    if(pts<0) pts=0;
    ipts=pts;
    //printf("MOV track seek - sample: %d  \n",ipts);
    for(trak->pos=0;trak->pos<trak->samples_size;++trak->pos){
	if(trak->samples[trak->pos].pts>=ipts) break; // found it!
    }
    if(trak->keyframes_size){
	// find nearest keyframe
	int i;
	for(i=0;i<trak->keyframes_size;i++){
	    if(trak->keyframes[i]>=trak->pos) break;
	}
	if(i>0 && (trak->keyframes[i]-trak->pos) > (trak->pos-trak->keyframes[i-1]))
	  --i;
	trak->pos=trak->keyframes[i];
//	printf("nearest keyframe: %d  \n",trak->pos);
    }
    pts=(float)trak->samples[trak->pos].pts/(float)trak->timescale;
}

//    printf("MOV track seek done:  %5.3f  \n",pts);

return pts;
}

void demux_seek_mov(demuxer_t *demuxer,float pts,int flags){
    mov_priv_t* priv=demuxer->priv;
    demux_stream_t* ds;
    mov_track_t* trak;

//    printf("MOV seek called  %5.3f  flag=%d  \n",pts,flags);
    
    ds=demuxer->video;
    trak = stream_track(priv, ds);
    if (trak) {
	//if(flags&2) pts*=(float)trak->length/(float)trak->timescale;
	//if(!(flags&1)) pts+=ds->pts;
	pts=ds->pts=mov_seek_track(trak,pts,flags);
	flags=1; // absolute seconds
    }

    ds=demuxer->audio;
    trak = stream_track(priv, ds);
    if (trak) {
	//if(flags&2) pts*=(float)trak->length/(float)trak->timescale;
	//if(!(flags&1)) pts+=ds->pts;
	ds->pts=mov_seek_track(trak,pts,flags);
    }

}

int demux_mov_control(demuxer_t *demuxer, int cmd, void *arg){
  mov_track_t* track;

  // try the video track
  track = stream_track(demuxer->priv, demuxer->video);
  if (!track || !track->length)
    // otherwise try to get the info from the audio track
    track = stream_track(demuxer->priv, demuxer->audio);

  if (!track || !track->length)
    return DEMUXER_CTRL_DONTKNOW;

  switch(cmd) {
    case DEMUXER_CTRL_GET_TIME_LENGTH:
      if (!track->timescale)
        return DEMUXER_CTRL_DONTKNOW;
      *((unsigned long *)arg) = track->length / track->timescale;
      return DEMUXER_CTRL_OK;

    case DEMUXER_CTRL_GET_PERCENT_POS:
      {
        off_t pos = track->pos;
        if (track->durmap_size >= 1)
          pos *= track->durmap[0].dur;
        *((int *)arg) = (int)(100 * pos / track->length);
        return DEMUXER_CTRL_OK;
      }
  }
  return DEMUXER_CTRL_NOTIMPL;
}


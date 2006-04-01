// .asf fileformat docs from http://divx.euro.ru


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "config.h"
#include "mp_msg.h"
#include "help_mp.h"

#include "stream.h"
#include "demuxer.h"
#include "stheader.h"

#include "asf.h"

#ifdef ARCH_X86
#define	ASF_LOAD_GUID_PREFIX(guid)	(*(uint32_t *)(guid))
#else
#define	ASF_LOAD_GUID_PREFIX(guid)	\
	((guid)[3] << 24 | (guid)[2] << 16 | (guid)[1] << 8 | (guid)[0])
#endif

#define ASF_GUID_PREFIX_audio_stream	0xF8699E40
#define ASF_GUID_PREFIX_video_stream	0xBC19EFC0
#define ASF_GUID_PREFIX_audio_conceal_none 0x49f1a440
#define ASF_GUID_PREFIX_audio_conceal_interleave 0xbfc3cd50
#define ASF_GUID_PREFIX_header		0x75B22630
#define ASF_GUID_PREFIX_data_chunk	0x75b22636
#define ASF_GUID_PREFIX_index_chunk	0x33000890
#define ASF_GUID_PREFIX_stream_header	0xB7DC0791
#define ASF_GUID_PREFIX_header_2_0	0xD6E229D1
#define ASF_GUID_PREFIX_file_header	0x8CABDCA1
#define	ASF_GUID_PREFIX_content_desc	0x75b22633
#define	ASF_GUID_PREFIX_stream_group	0x7bf875ce

/*
const char asf_audio_stream_guid[16] = {0x40, 0x9e, 0x69, 0xf8,
  0x4d, 0x5b, 0xcf, 0x11, 0xa8, 0xfd, 0x00, 0x80, 0x5f, 0x5c, 0x44, 0x2b};
const char asf_video_stream_guid[16] = {0xc0, 0xef, 0x19, 0xbc,
  0x4d, 0x5b, 0xcf, 0x11, 0xa8, 0xfd, 0x00, 0x80, 0x5f, 0x5c, 0x44, 0x2b};
*/
const char asf_stream_header_guid[16] = {0x91, 0x07, 0xdc, 0xb7,
  0xb7, 0xa9, 0xcf, 0x11, 0x8e, 0xe6, 0x00, 0xc0, 0x0c, 0x20, 0x53, 0x65};
const char asf_file_header_guid[16] = {0xa1, 0xdc, 0xab, 0x8c,
  0x47, 0xa9, 0xcf, 0x11, 0x8e, 0xe4, 0x00, 0xc0, 0x0c, 0x20, 0x53, 0x65};
const char asf_content_desc_guid[16] = {0x33, 0x26, 0xb2, 0x75,
  0x8e, 0x66, 0xcf, 0x11, 0xa6, 0xd9, 0x00, 0xaa, 0x00, 0x62, 0xce, 0x6c};
const char asf_stream_group_guid[16] = {0xce, 0x75, 0xf8, 0x7b,
  0x8d, 0x46, 0xd1, 0x11, 0x8d, 0x82, 0x00, 0x60, 0x97, 0xc9, 0xa2, 0xb2};
const char asf_data_chunk_guid[16] = {0x36, 0x26, 0xb2, 0x75,
  0x8e, 0x66, 0xcf, 0x11, 0xa6, 0xd9, 0x00, 0xaa, 0x00, 0x62, 0xce, 0x6c};


// the variable string is modify in this function
void pack_asf_string(char* string, int length) {
  int i,j;
  if( string==NULL ) return;
  for( i=0, j=0; i<length && string[i]!='\0'; i+=2, j++) {
    string[j]=string[i];
  }
  string[j]='\0';
}

// the variable string is modify in this function
void print_asf_string(const char* name, char* string, int length) {
  pack_asf_string(string, length);
  mp_msg(MSGT_HEADER,MSGL_V,"%s%s\n", name, string);
}

static char* asf_chunk_type(unsigned char* guid) {
  static char tmp[60];
  char *p;
  int i;

  switch(ASF_LOAD_GUID_PREFIX(guid)){
    case ASF_GUID_PREFIX_audio_stream:
      return "guid_audio_stream";
    case ASF_GUID_PREFIX_video_stream: 
      return "guid_video_stream";
    case ASF_GUID_PREFIX_audio_conceal_none:
      return "guid_audio_conceal_none";
    case ASF_GUID_PREFIX_audio_conceal_interleave:
      return "guid_audio_conceal_interleave";
    case ASF_GUID_PREFIX_header:
      return "guid_header";
    case ASF_GUID_PREFIX_data_chunk:
      return "guid_data_chunk";
    case ASF_GUID_PREFIX_index_chunk:
      return "guid_index_chunk";
    case ASF_GUID_PREFIX_stream_header:
      return "guid_stream_header";
    case ASF_GUID_PREFIX_header_2_0: 
      return "guid_header_2_0";
    case ASF_GUID_PREFIX_file_header:
      return "guid_file_header";
    case ASF_GUID_PREFIX_content_desc:
      return "guid_content_desc";
    default:
      strcpy(tmp, "unknown guid ");
      p = tmp + strlen(tmp);
      for (i = 0; i < 16; i++) {
	if ((1 << i) & ((1<<4) | (1<<6) | (1<<8))) *p++ = '-';
	sprintf(p, "%02x", guid[i]);
	p += 2;
      }
      return tmp;
  }
}

int asf_check_header(demuxer_t *demuxer){
  unsigned char asfhdrguid[16]={0x30,0x26,0xB2,0x75,0x8E,0x66,0xCF,0x11,0xA6,0xD9,0x00,0xAA,0x00,0x62,0xCE,0x6C};
  struct asf_priv* asf = calloc(1,sizeof(*asf));
  asf->scrambling_h=asf->scrambling_w=asf->scrambling_b=1;
  stream_read(demuxer->stream,(char*) &asf->header,sizeof(asf->header)); // header obj
  le2me_ASF_header_t(&asf->header);			// swap to machine endian
//  for(i=0;i<16;i++) printf(" %02X",temp[i]);printf("\n");
//  for(i=0;i<16;i++) printf(" %02X",asfhdrguid[i]);printf("\n");
  if(memcmp(asfhdrguid,asf->header.objh.guid,16)){
    mp_msg(MSGT_HEADER,MSGL_V,"ASF_check: not ASF guid!\n");
    free(asf);
    return 0; // not ASF guid
  }
  if(asf->header.cno>256){
    mp_msg(MSGT_HEADER,MSGL_V,"ASF_check: invalid subchunks_no %d\n",(int) asf->header.cno);
    free(asf);
    return 0; // invalid header???
  }
  demuxer->priv = asf;
  return DEMUXER_TYPE_ASF;
}

extern void print_wave_header(WAVEFORMATEX *h, int verbose_level);
extern void print_video_header(BITMAPINFOHEADER *h, int verbose_level);

int find_asf_guid(char *buf, const char *guid, int cur_pos, int buf_len)
{
  int i;
  for (i = cur_pos; i < buf_len - 19; i++) {
    if (memcmp(&buf[i], guid, 16) == 0)
      return i + 16 + 8; // point after guid + length
  }
  return -1;
}

int read_asf_header(demuxer_t *demuxer,struct asf_priv* asf){
  int hdr_len = asf->header.objh.size - sizeof(asf->header);
  char *hdr = NULL;
  char guid_buffer[16];
  int pos, start = stream_tell(demuxer->stream);
  uint32_t* streams = NULL;
  int audio_streams=0;
  int video_streams=0;
  uint16_t stream_count=0;
  int best_video = -1;
  int best_audio = -1;
  uint64_t data_len;

  if(hdr_len < 0) {
    mp_msg(MSGT_HEADER, MSGL_FATAL, "Header size is too small.\n");
    return 0;
  }
    
  if (hdr_len > 1024 * 1024) {
    mp_msg(MSGT_HEADER, MSGL_FATAL, MSGTR_MPDEMUX_ASFHDR_HeaderSizeOver1MB,
			hdr_len);
    return 0;
  }
  hdr = malloc(hdr_len);
  if (!hdr) {
    mp_msg(MSGT_HEADER, MSGL_FATAL, MSGTR_MPDEMUX_ASFHDR_HeaderMallocFailed,
            hdr_len);
    return 0;
  }
  stream_read(demuxer->stream, hdr, hdr_len);
  if (stream_eof(demuxer->stream)) {
    mp_msg(MSGT_HEADER, MSGL_FATAL, MSGTR_MPDEMUX_ASFHDR_EOFWhileReadingHeader);
    goto err_out;
  }

  // find stream headers
  pos = 0;
  while ((pos = find_asf_guid(hdr, asf_stream_header_guid, pos, hdr_len)) >= 0)
  {
    ASF_stream_header_t *streamh = (ASF_stream_header_t *)&hdr[pos];
    uint8_t *buffer;
    pos += sizeof(ASF_stream_header_t);
    if (pos > hdr_len) goto len_err_out;
    le2me_ASF_stream_header_t(streamh);
    mp_msg(MSGT_HEADER, MSGL_V, "stream type: %s\n",
            asf_chunk_type(streamh->type));
    mp_msg(MSGT_HEADER, MSGL_V, "stream concealment: %s\n",
            asf_chunk_type(streamh->concealment));
    mp_msg(MSGT_HEADER, MSGL_V, "type: %d bytes,  stream: %d bytes  ID: %d\n",
            (int)streamh->type_size, (int)streamh->stream_size,
            (int)streamh->stream_no);
    mp_msg(MSGT_HEADER, MSGL_V, "unk1: %lX  unk2: %X\n",
            (unsigned long)streamh->unk1, (unsigned int)streamh->unk2);
    mp_msg(MSGT_HEADER, MSGL_V, "FILEPOS=0x%X\n", pos + start);
    // type-specific data:
    buffer = &hdr[pos];
    pos += streamh->type_size;
    if (pos > hdr_len) goto len_err_out;
    switch(ASF_LOAD_GUID_PREFIX(streamh->type)){
      case ASF_GUID_PREFIX_audio_stream: {
        sh_audio_t* sh_audio=new_sh_audio(demuxer,streamh->stream_no & 0x7F);
        ++audio_streams;
        sh_audio->wf=calloc((streamh->type_size<sizeof(WAVEFORMATEX))?sizeof(WAVEFORMATEX):streamh->type_size,1);
        memcpy(sh_audio->wf,buffer,streamh->type_size);
	le2me_WAVEFORMATEX(sh_audio->wf);
        if( mp_msg_test(MSGT_HEADER,MSGL_V) ) print_wave_header(sh_audio->wf,MSGL_V);
	if(ASF_LOAD_GUID_PREFIX(streamh->concealment)==ASF_GUID_PREFIX_audio_conceal_interleave){
          buffer = &hdr[pos];
          pos += streamh->stream_size;
          if (pos > hdr_len) goto len_err_out;
          asf->scrambling_h=buffer[0];
          asf->scrambling_w=(buffer[2]<<8)|buffer[1];
          asf->scrambling_b=(buffer[4]<<8)|buffer[3];
          if(asf->scrambling_b>0){
            asf->scrambling_w/=asf->scrambling_b;
          }
	} else {
	  asf->scrambling_b=asf->scrambling_h=asf->scrambling_w=1;
	}
	mp_msg(MSGT_HEADER,MSGL_V,"ASF: audio scrambling: %d x %d x %d\n",asf->scrambling_h,asf->scrambling_w,asf->scrambling_b);
	//if(demuxer->audio->id==-1) demuxer->audio->id=streamh.stream_no & 0x7F;
        break;
        }
      case ASF_GUID_PREFIX_video_stream: {
        sh_video_t* sh_video=new_sh_video(demuxer,streamh->stream_no & 0x7F);
        unsigned int len=streamh->type_size-(4+4+1+2);
	++video_streams;
//        sh_video->bih=malloc(chunksize); memset(sh_video->bih,0,chunksize);
        sh_video->bih=calloc((len<sizeof(BITMAPINFOHEADER))?sizeof(BITMAPINFOHEADER):len,1);
        memcpy(sh_video->bih,&buffer[4+4+1+2],len);
	le2me_BITMAPINFOHEADER(sh_video->bih);
	if (sh_video->bih->biCompression == mmioFOURCC('D', 'V', 'R', ' '))
	  mp_msg(MSGT_DEMUXER, MSGL_WARN, MSGTR_MPDEMUX_ASFHDR_DVRWantsLibavformat);
        //sh_video->fps=(float)sh_video->video.dwRate/(float)sh_video->video.dwScale;
        //sh_video->frametime=(float)sh_video->video.dwScale/(float)sh_video->video.dwRate;
        if( mp_msg_test(MSGT_DEMUX,MSGL_V) ) print_video_header(sh_video->bih, MSGL_V);
        //asf_video_id=streamh.stream_no & 0x7F;
	//if(demuxer->video->id==-1) demuxer->video->id=streamh.stream_no & 0x7F;
        break;
        }
      }
      // stream-specific data:
      // stream_read(demuxer->stream,(char*) buffer,streamh.stream_size);
  }

  // find file header
  pos = find_asf_guid(hdr, asf_file_header_guid, 0, hdr_len);
  if (pos >= 0) {
      ASF_file_header_t *fileh = (ASF_file_header_t *)&hdr[pos];
      pos += sizeof(ASF_file_header_t);
      if (pos > hdr_len) goto len_err_out;
      le2me_ASF_file_header_t(fileh);
      mp_msg(MSGT_HEADER, MSGL_V, "ASF: packets: %d  flags: %d  "
              "max_packet_size: %d  min_packet_size: %d  max_bitrate: %d  "
              "preroll: %d\n",
              (int)fileh->num_packets, (int)fileh->flags, 
              (int)fileh->min_packet_size, (int)fileh->max_packet_size,
              (int)fileh->max_bitrate, (int)fileh->preroll);
      asf->packetsize=fileh->max_packet_size;
      asf->packet=malloc(asf->packetsize); // !!!
      asf->packetrate=fileh->max_bitrate/8.0/(double)asf->packetsize;
      asf->movielength=fileh->send_duration/10000000LL;
  }

  // find content header
  pos = find_asf_guid(hdr, asf_content_desc_guid, 0, hdr_len);
  if (pos >= 0) {
        ASF_content_description_t *contenth = (ASF_content_description_t *)&hdr[pos];
        char *string=NULL;
        pos += sizeof(ASF_content_description_t);
        if (pos > hdr_len) goto len_err_out;
	le2me_ASF_content_description_t(contenth);
	mp_msg(MSGT_HEADER,MSGL_V,"\n");
        // extract the title
        if( contenth->title_size!=0 ) {
          string = &hdr[pos];
          pos += contenth->title_size;
          if (pos > hdr_len) goto len_err_out;
          if( mp_msg_test(MSGT_HEADER,MSGL_V) )
            print_asf_string(" Title: ", string, contenth->title_size);
	  else
	    pack_asf_string(string, contenth->title_size);
	  demux_info_add(demuxer, "name", string);
        }
        // extract the author 
        if( contenth->author_size!=0 ) {
          string = &hdr[pos];
          pos += contenth->author_size;
          if (pos > hdr_len) goto len_err_out;
          if( mp_msg_test(MSGT_HEADER,MSGL_V) )
            print_asf_string(" Author: ", string, contenth->author_size);
	  else
	    pack_asf_string(string, contenth->author_size);
	  demux_info_add(demuxer, "author", string);
        }
        // extract the copyright
        if( contenth->copyright_size!=0 ) {
          string = &hdr[pos];
          pos += contenth->copyright_size;
          if (pos > hdr_len) goto len_err_out;
          if( mp_msg_test(MSGT_HEADER,MSGL_V) )
            print_asf_string(" Copyright: ", string, contenth->copyright_size);
	  else
	    pack_asf_string(string, contenth->copyright_size);
	  demux_info_add(demuxer, "copyright", string);
        }
        // extract the comment
        if( contenth->comment_size!=0 ) {
          string = &hdr[pos];
          pos += contenth->comment_size;
          if (pos > hdr_len) goto len_err_out;
          if( mp_msg_test(MSGT_HEADER,MSGL_V) )
            print_asf_string(" Comment: ", string, contenth->comment_size);
	  else
	    pack_asf_string(string, contenth->comment_size);
	  demux_info_add(demuxer, "comments", string);
        }
        // extract the rating
        if( contenth->rating_size!=0 ) {
          string = &hdr[pos];
          pos += contenth->rating_size;
          if (pos > hdr_len) goto len_err_out;
          if( mp_msg_test(MSGT_HEADER,MSGL_V) )
            print_asf_string(" Rating: ", string, contenth->rating_size);
        }
	mp_msg(MSGT_HEADER,MSGL_V,"\n");
  }
  
  // find content header
  pos = find_asf_guid(hdr, asf_stream_group_guid, 0, hdr_len);
  if (pos >= 0) {
        uint16_t stream_id, i;
        uint32_t max_bitrate;
        char *ptr = &hdr[pos];
        mp_msg(MSGT_HEADER,MSGL_V,"============ ASF Stream group == START ===\n");
        stream_count = le2me_16(*(uint16_t*)ptr);
        ptr += sizeof(uint16_t);
        if (ptr > &hdr[hdr_len]) goto len_err_out;
        if(stream_count > 0)
              streams = (uint32_t*)malloc(2*stream_count*sizeof(uint32_t));
        mp_msg(MSGT_HEADER,MSGL_V," stream count=[0x%x][%u]\n", stream_count, stream_count );
        for( i=0 ; i<stream_count ; i++ ) {
          stream_id = le2me_16(*(uint16_t*)ptr);
          ptr += sizeof(uint16_t);
          if (ptr > &hdr[hdr_len]) goto len_err_out;
          memcpy(&max_bitrate, ptr, sizeof(uint32_t));// workaround unaligment bug on sparc
          max_bitrate = le2me_32(max_bitrate);
          ptr += sizeof(uint32_t);
          if (ptr > &hdr[hdr_len]) goto len_err_out;
          mp_msg(MSGT_HEADER,MSGL_V,"   stream id=[0x%x][%u]\n", stream_id, stream_id );
          mp_msg(MSGT_HEADER,MSGL_V,"   max bitrate=[0x%x][%u]\n", max_bitrate, max_bitrate );
          streams[2*i] = stream_id;
          streams[2*i+1] = max_bitrate;
        }
        mp_msg(MSGT_HEADER,MSGL_V,"============ ASF Stream group == END ===\n");
  }
  free(hdr);
  hdr = NULL;
  start = stream_tell(demuxer->stream); // start of first data chunk
  stream_read(demuxer->stream, guid_buffer, 16);
  if (memcmp(guid_buffer, asf_data_chunk_guid, 16) != 0) {
    mp_msg(MSGT_HEADER, MSGL_FATAL, MSGTR_MPDEMUX_ASFHDR_NoDataChunkAfterHeader);
    return 0;
  }
  // read length of chunk
  stream_read(demuxer->stream, (char *)&data_len, sizeof(data_len));
  data_len = le2me_64(data_len);
  demuxer->movi_start = stream_tell(demuxer->stream) + 26;
  demuxer->movi_end = start + data_len;
  mp_msg(MSGT_HEADER, MSGL_V, "Found movie at 0x%X - 0x%X\n",
          (int)demuxer->movi_start, (int)demuxer->movi_end);

if(streams) {
  // stream selection is done in the network code, it shouldn't be done here
  // as the servers often do not care about what we requested.
#if 0
  uint32_t vr = 0, ar = 0,i;
#ifdef MPLAYER_NETWORK
  if( demuxer->stream->streaming_ctrl!=NULL ) {
	  if( demuxer->stream->streaming_ctrl->bandwidth!=0 && demuxer->stream->streaming_ctrl->data!=NULL ) {
		  best_audio = ((asf_http_streaming_ctrl_t*)demuxer->stream->streaming_ctrl->data)->audio_id;
		  best_video = ((asf_http_streaming_ctrl_t*)demuxer->stream->streaming_ctrl->data)->video_id;
	  }
  } else
#endif
  for(i = 0; i < stream_count; i++) {
    uint32_t id = streams[2*i];
    uint32_t rate = streams[2*i+1];
    if(demuxer->v_streams[id] && rate > vr) {
      vr = rate;
      best_video = id;
    } else if(demuxer->a_streams[id] && rate > ar) {
      ar = rate;
      best_audio = id;
    }
  }
#endif
  free(streams);
  streams = NULL;
}

mp_msg(MSGT_HEADER,MSGL_V,"ASF: %d audio and %d video streams found\n",audio_streams,video_streams);
if(!audio_streams) demuxer->audio->id=-2;  // nosound
else if(best_audio > 0 && demuxer->audio->id == -1) demuxer->audio->id=best_audio;
if(!video_streams){
    if(!audio_streams){
	mp_msg(MSGT_HEADER,MSGL_ERR,MSGTR_MPDEMUX_ASFHDR_AudioVideoHeaderNotFound);
	return 0; 
    }
    demuxer->video->id=-2; // audio-only
} else if (best_video > 0 && demuxer->video->id == -1) demuxer->video->id = best_video;

#if 0
if( mp_msg_test(MSGT_HEADER,MSGL_V) ){
    printf("ASF duration: %d\n",(int)fileh.duration);
    printf("ASF start pts: %d\n",(int)fileh.start_timestamp);
    printf("ASF end pts: %d\n",(int)fileh.end_timestamp);
}
#endif

return 1;

len_err_out:
  mp_msg(MSGT_HEADER, MSGL_FATAL, MSGTR_MPDEMUX_ASFHDR_InvalidLengthInASFHeader);
err_out:
  if (hdr) free(hdr);
  if (streams) free(streams);
  return 0;
}

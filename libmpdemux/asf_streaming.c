#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#include "config.h"

#include "url.h"
#include "http.h"
#include "asf.h"
#include "network.h"

#include "stream.h"

typedef struct {
	ASF_StreamType_e streaming_type;
	int request;
	int packet_size;
        int *audio_streams,n_audio,*video_streams,n_video;
} asf_http_streaming_ctrl_t;

#ifdef ARCH_X86
#define	ASF_LOAD_GUID_PREFIX(guid)	(*(uint32_t *)(guid))
#else
#define	ASF_LOAD_GUID_PREFIX(guid)	\
	((guid)[3] << 24 | (guid)[2] << 16 | (guid)[1] << 8 | (guid)[0])
#endif

extern int audio_id,video_id;
extern int verbose;

// ASF streaming support several network protocol.
// One use UDP, not known, yet!
// Another is HTTP, this one is known.
// So for now, we use the HTTP protocol.
// 
// We can try several protocol for asf streaming
// * first the UDP protcol, if there is a firewall, UDP
//   packets will not come back, so the mmsu will failed.
// * Then we can try TCP, but if there is a proxy for
//   internet connection, the TCP connection will not get
//   through
// * Then we can try HTTP.
int
asf_streaming_start( stream_t *stream ) {
	char proto_s[10];
	int fd = -1;
	
	strncpy( proto_s, stream->streaming_ctrl->url->protocol, 10 );
	
	if( !strncasecmp( proto_s, "mms", 3) && strncasecmp( proto_s, "mmst", 4) ) {
		printf("Trying ASF/UDP...\n");
		//fd = asf_mmsu_streaming_start( stream );
		if( fd!=-1 ) return fd;
		printf("  ===> ASF/UDP failed\n");
	}
	if( !strncasecmp( proto_s, "mms", 3) ) {
		printf("Trying ASF/TCP...\n");
		//fd = asf_mmst_streaming_start( stream );
		if( fd!=-1 ) return fd;
		printf("  ===> ASF/TCP failed\n");
	}
	if( 	!strncasecmp( proto_s, "http", 4) || 
		!strncasecmp( proto_s, "mms", 3)  ||
		!strncasecmp( proto_s, "http_proxy", 10)
		) {
		printf("Trying ASF/HTTP...\n");
		fd = asf_http_streaming_start( stream );
		if( fd!=-1 ) return fd;
		printf("  ===> ASF/HTTP failed\n");
	}

	printf("Unknown protocol: %s\n", proto_s );
	return -1;
}

int 
asf_streaming(ASF_stream_chunck_t *stream_chunck, int *drop_packet ) {
/*	
printf("ASF stream chunck size=%d\n", stream_chunck->size);
printf("length: %d\n", length );
printf("0x%02X\n", stream_chunck->type );
*/
	if( drop_packet!=NULL ) *drop_packet = 0;

	if( stream_chunck->size<8 ) {
		printf("Ahhhh, stream_chunck size is too small: %d\n", stream_chunck->size);
		return -1;
	}
	if( stream_chunck->size!=stream_chunck->size_confirm ) {
		printf("size_confirm mismatch!: %d %d\n", stream_chunck->size, stream_chunck->size_confirm);
		return -1;
	}
/*	
	printf("  type: 0x%02X\n", stream_chunck->type );
	printf("  size: %d (0x%02X)\n", stream_chunck->size, stream_chunck->size );
	printf("  sequence_number: 0x%04X\n", stream_chunck->sequence_number );
	printf("  unknown: 0x%02X\n", stream_chunck->unknown );
	printf("  size_confirm: 0x%02X\n", stream_chunck->size_confirm );
*/
	switch(stream_chunck->type) {
		case 0x4324:	// $C	Clear ASF configuration
			printf("=====> Clearing ASF stream configuration!\n");
			if( drop_packet!=NULL ) *drop_packet = 1;
			return stream_chunck->size;
			break;
		case 0x4424:    // $D	Data follows
//			printf("=====> Data follows\n");
			break;
		case 0x4524:    // $E	Transfer complete
			printf("=====> Transfer complete\n");
			if( drop_packet!=NULL ) *drop_packet = 1;
			return stream_chunck->size;
			break;
		case 0x4824:    // $H	ASF header chunk follows
			printf("=====> ASF header chunk follows\n");
			break;
		default:
			printf("=====> Unknown stream type 0x%x\n", stream_chunck->type );
	}
	return stream_chunck->size+4;
}

static int
asf_streaming_parse_header(int fd, streaming_ctrl_t* streaming_ctrl) {
  ASF_header_t asfh;
  ASF_obj_header_t objh;
  ASF_file_header_t fileh;
  ASF_stream_header_t streamh;
  ASF_stream_chunck_t chunk;
  asf_http_streaming_ctrl_t* asf_ctrl = (asf_http_streaming_ctrl_t*) streaming_ctrl->data;
  char* buffer=NULL, *chunk_buffer=NULL;
  int i,r,size,pos = 0;
  int buffer_size = 0;
  int chunk_size2read = 0;
  
  if(asf_ctrl == NULL) return -1;

	// The ASF header can be in several network chunks. For example if the content description
	// is big, the ASF header will be split in 2 network chunk.
	// So we need to retrieve all the chunk before starting to parse the header.
  do {
	  for( r=0; r < sizeof(ASF_stream_chunck_t) ; ) {
		i = nop_streaming_read(fd,((char*)&chunk)+r,sizeof(ASF_stream_chunck_t) - r,streaming_ctrl);
		if(i <= 0) return -1;
		r += i;
	  }
	  size = asf_streaming( &chunk, &r) - sizeof(ASF_stream_chunck_t);
	  if(r) printf("Warning : drop header ????\n");
	  if(size < 0){
	    printf("Error while parsing chunk header\n");
		return -1;
	  }
	  if (chunk.type != 0x4824) {
	    printf("Don't got a header as first chunk !!!!\n");
	    return -1;
	  }
	  
	  buffer = (char*) malloc(size+buffer_size);
	  if(buffer == NULL) {
	    printf("Error can't allocate %d bytes buffer\n",size+buffer_size);
	    return -1;
	  }
	  if( chunk_buffer!=NULL ) {
	  	memcpy( buffer, chunk_buffer, buffer_size );
		free( chunk_buffer );
	  }
	  chunk_buffer = buffer;
	  buffer += buffer_size;
	  buffer_size += size;
	  
	  for(r = 0; r < size;) {
	    i = nop_streaming_read(fd,buffer+r,size-r,streaming_ctrl);
	    if(i < 0) {
		    printf("Error while reading network stream\n");
		    return -1;
	    }
	    r += i;
	  }  

	  if( chunk_size2read==0 ) {
		if(size < (int)sizeof(asfh)) {
		    printf("Error chunk is too small\n");
		    return -1;
		} else printf("Got chunk\n");
	  	memcpy(&asfh,buffer,sizeof(asfh));
	  	le2me_ASF_header_t(&asfh);
		chunk_size2read = asfh.objh.size;
		printf("Size 2 read=%d\n", chunk_size2read);
	  }
  } while( buffer_size<chunk_size2read);
  buffer = chunk_buffer;
  size = buffer_size;
	  
  if(asfh.cno > 256) {
    printf("Error sub chunks number is invalid\n");
    return -1;
  }

  pos += sizeof(asfh);
  
  while(size - pos >= (int)sizeof(objh)) {
    memcpy(&objh,buffer+pos,sizeof(objh));
    le2me_ASF_obj_header_t(&objh);

    switch(ASF_LOAD_GUID_PREFIX(objh.guid)) {
    case 0x8CABDCA1 : // File header
      pos += sizeof(objh);
      memcpy(&fileh,buffer + pos,sizeof(fileh));
      le2me_ASF_file_header_t(&fileh);
/*
      if(fileh.packetsize != fileh.packetsize2) {
	printf("Error packetsize check don't match\n");
	return -1;
      }
*/
      asf_ctrl->packet_size = fileh.max_packet_size;
      // before playing. 
      // preroll: time in ms to bufferize before playing
      streaming_ctrl->prebuffer_size = (unsigned int)((double)((double)fileh.preroll/1000)*((double)fileh.max_bitrate/8));
      pos += sizeof(fileh);
      break;
    case 0xB7DC0791 : // stream header
      pos += sizeof(objh);
      memcpy(&streamh,buffer + pos,sizeof(streamh));
      le2me_ASF_stream_header_t(&streamh);
      pos += sizeof(streamh) + streamh.type_size;
      switch(ASF_LOAD_GUID_PREFIX(streamh.type)) {
      case 0xF8699E40 : // audio stream
	if(asf_ctrl->audio_streams == NULL){
	  asf_ctrl->audio_streams = (int*)malloc(sizeof(int));
	  asf_ctrl->n_audio = 1;
	} else {
	  asf_ctrl->n_audio++;
	  asf_ctrl->audio_streams = (int*)realloc(asf_ctrl->audio_streams,
						     asf_ctrl->n_audio*sizeof(int));
	}
	asf_ctrl->audio_streams[asf_ctrl->n_audio-1] = streamh.stream_no;
	pos += streamh.stream_size;
	break;
      case 0xBC19EFC0 : // video stream
	if(asf_ctrl->video_streams == NULL){
	  asf_ctrl->video_streams = (int*)malloc(sizeof(int));
	  asf_ctrl->n_video = 1;
	} else {
	  asf_ctrl->n_video++;
	  asf_ctrl->video_streams = (int*)realloc(asf_ctrl->video_streams,
						     asf_ctrl->n_video*sizeof(int));
	}
	asf_ctrl->video_streams[asf_ctrl->n_video-1] = streamh.stream_no;
	break;
      }
      break;
    default :
      pos += objh.size;
      break;
    }
  }
  free(buffer);
  return 1;
}

int
asf_http_streaming_read( int fd, char *buffer, int size, streaming_ctrl_t *streaming_ctrl ) {
  static ASF_stream_chunck_t chunk;
  int read,chunk_size = 0;
  static int rest = 0, drop_chunk = 0, waiting = 0,eof= 0;
  asf_http_streaming_ctrl_t *asf_http_ctrl = (asf_http_streaming_ctrl_t*)streaming_ctrl->data;

  while(1) {
    if (rest == 0 && waiting == 0) {
      read = 0;
      while(read < (int)sizeof(ASF_stream_chunck_t)){
	int r = nop_streaming_read( fd, ((char*)&chunk) + read, 
				    sizeof(ASF_stream_chunck_t)-read, 
				    streaming_ctrl );
	if(r <= 0){
	  if( r < 0) 
	    printf("Error while reading chunk header\n");
	  return -1;
	}
	read += r;
      }
      chunk_size = asf_streaming( &chunk, &drop_chunk );
      if(chunk_size < 0) {
	printf("Error while parsing chunk header\n");
	return -1;
      }
      chunk_size -= sizeof(ASF_stream_chunck_t);
	
      if(chunk.type != 0x4824 && (!drop_chunk)) {
	if (asf_http_ctrl->packet_size < chunk_size) {
	  printf("Error chunk_size > packet_size\n");
	  return -1;
	}
	waiting = asf_http_ctrl->packet_size;
      } else {
	waiting = chunk_size;
      }

    } else if (rest){
      chunk_size = rest;
      rest = 0;
    }

    read = 0;
    if ( waiting >= chunk_size) {
      if (chunk_size > size){
	rest = chunk_size - size;
	chunk_size = size;
      }
      while(read < chunk_size) {
	int got = nop_streaming_read( fd,buffer+read,chunk_size-read,streaming_ctrl );
	if(got <= 0) {
	  if(got < 0)
	    printf("Error while reading chunk\n");
	  return -1;
	}
	read += got;
      }
      waiting -= read;
      if (drop_chunk) continue;
    }
    if (rest == 0 && waiting > 0 && size-read > 0) {
      int s = MIN(waiting,size-read);
      memset(buffer+read,0,s);
      waiting -= s;
      read += s;
    }
    break;
  }

  return read;
}

int
asf_http_streaming_seek( int fd, off_t pos, streaming_ctrl_t *streaming_ctrl ) {
	return -1;
}

int
asf_http_streaming_type(char *content_type, char *features) {
	if( content_type==NULL ) return ASF_Unknown_e;
	if( !strcasecmp(content_type, "application/octet-stream") ) {
		if( features==NULL ) {
			printf("=====> ASF Prerecorded\n");
			return ASF_Prerecorded_e;
		} else if( strstr(features, "broadcast")) {
			printf("=====> ASF Live stream\n");
			return ASF_Live_e;
		} else {
			printf("=====> ASF Prerecorded\n");
			return ASF_Prerecorded_e;
		}
	} else {
		if(	(!strcasecmp(content_type, "audio/x-ms-wax")) ||
			(!strcasecmp(content_type, "audio/x-ms-wma")) ||
			(!strcasecmp(content_type, "video/x-ms-asf")) ||
			(!strcasecmp(content_type, "video/x-ms-afs")) ||
			(!strcasecmp(content_type, "video/x-ms-wvx")) ||
			(!strcasecmp(content_type, "video/x-ms-wmv")) ||
			(!strcasecmp(content_type, "video/x-ms-wma")) ) {
			printf("=====> ASF Redirector\n");
			return ASF_Redirector_e;
		} else if( !strcasecmp(content_type, "text/plain") ) {
			printf("=====> ASF Plain text\n");
			return ASF_PlainText_e;
		} else {
			printf("=====> ASF unknown content-type: %s\n", content_type );
			return ASF_Unknown_e;
		}
	}
	return ASF_Unknown_e;
}

HTTP_header_t *
asf_http_request(streaming_ctrl_t *streaming_ctrl) {
	HTTP_header_t *http_hdr;
	URL_t *url = NULL;
	URL_t *server_url = NULL;
	asf_http_streaming_ctrl_t *asf_http_ctrl;
	char str[250];
	char *ptr;
	int i,as = -1,vs = -1;

	int offset_hi=0, offset_lo=0, length=0;
	int asf_nb_stream=0;

	// Sanity check
	if( streaming_ctrl==NULL ) return NULL;
	url = streaming_ctrl->url;
	asf_http_ctrl = (asf_http_streaming_ctrl_t*)streaming_ctrl->data;
	if( url==NULL || asf_http_ctrl==NULL ) return NULL;

	// Common header for all requests.
	http_hdr = http_new_header();
	http_set_field( http_hdr, "Accept: */*" );
	http_set_field( http_hdr, "User-Agent: NSPlayer/4.1.0.3856" );

	// Check if we are using a proxy
	if( !strcasecmp( url->protocol, "http_proxy" ) ) {
		server_url = url_new( (url->file)+1 );
		if( server_url==NULL ) {
			printf("Invalid proxy URL\n");
			http_free( http_hdr );
			return NULL;
		}
		http_set_uri( http_hdr, server_url->url );
		sprintf( str, "Host: %s:%d", server_url->hostname, server_url->port );
		url_free( server_url );
	} else {
		http_set_uri( http_hdr, url->file );
		sprintf( str, "Host: %s:%d", url->hostname, url->port );
	}
	
	http_set_field( http_hdr, str );
	http_set_field( http_hdr, "Pragma: xClientGUID={c77e7400-738a-11d2-9add-0020af0a3278}" );
	sprintf(str, 
		"Pragma: no-cache,rate=1.000000,stream-time=0,stream-offset=%u:%u,request-context=%d,max-duration=%u",
		offset_hi, offset_lo, asf_http_ctrl->request, length );
	http_set_field( http_hdr, str );

	switch( asf_http_ctrl->streaming_type ) {
		case ASF_Live_e:
		case ASF_Prerecorded_e:
			http_set_field( http_hdr, "Pragma: xPlayStrm=1" );
			ptr = str;
			ptr += sprintf( ptr, "Pragma: stream-switch-entry=");
			if(asf_http_ctrl->n_audio > 0) {
				if(audio_id > 0) {
					for( i=0; i<asf_http_ctrl->n_audio ; i++ ) {
						if(asf_http_ctrl->audio_streams[i] == audio_id) {
							as = audio_id;
							break;
						}
					}
				}				
				if(as < 0) {
					if(audio_id > 0) 
		       				printf("Audio stream %d don't exist\n", as);
					as = asf_http_ctrl->audio_streams[0];
				}
				ptr += sprintf(ptr, " ffff:%d:0",as);
				asf_nb_stream++;
			}
			if(asf_http_ctrl->n_video > 0) {
				if(video_id > 0) {
					for( i=0; i<asf_http_ctrl->n_video ; i++ ) {
						if(asf_http_ctrl->video_streams[i] == video_id) {
							vs = video_id;
							break;
						}
					}
				}
				if(vs < 0) {
					if(video_id > 0) 
						printf("Video stream %d don't exist\n",vs);
					vs = asf_http_ctrl->video_streams[0];
		       		}
				ptr += sprintf( ptr, " ffff:%d:0",vs);
				asf_nb_stream++;
			}
			http_set_field( http_hdr, str );
			sprintf( str, "Pragma: stream-switch-count=%d", asf_nb_stream );
			http_set_field( http_hdr, str );
			break;
		case ASF_Redirector_e:
			break;
		case ASF_Unknown_e:
			// First request goes here.
			break;
		default:
			printf("Unknown asf stream type\n");
	}

	http_set_field( http_hdr, "Connection: Close" );
	http_build_request( http_hdr );

	return http_hdr;
}

int
asf_http_parse_response( HTTP_header_t *http_hdr ) {
	char *content_type, *pragma;
	char features[64] = "\0";
	int len;
	if( http_response_parse(http_hdr)<0 ) {
		printf("Failed to parse HTTP response\n");
		return -1;
	}
	if( http_hdr->status_code!=200 ) {
		printf("Server return %d:%s\n", http_hdr->status_code, http_hdr->reason_phrase);
		return -1;
	}

	content_type = http_get_field( http_hdr, "Content-Type");
//printf("Content-Type: [%s]\n", content_type);

	pragma = http_get_field( http_hdr, "Pragma");
	while( pragma!=NULL ) {
		char *comma_ptr=NULL;
		char *end;
//printf("Pragma: [%s]\n", pragma );
		// The pragma line can get severals attributes 
		// separeted with a comma ','.
		do {
			if( !strncasecmp( pragma, "features=", 9) ) {
				pragma += 9;
				end = strstr( pragma, "," );
				if( end==NULL ) {
				  int s = strlen(pragma);
				  if(s > sizeof(features)) {
				    printf("ASF HTTP PARSE WARNING : Pragma %s cuted from %d bytes to %d\n",pragma,s,sizeof(features));
				    len = sizeof(features);
				  } else {				   
				    len = s;
				  }
				} else { 
				  len = MIN(end-pragma,sizeof(features));
				}
				strncpy( features, pragma, len );
				features[len]='\0';
				break;
			}
			comma_ptr = strstr( pragma, "," );
			if( comma_ptr!=NULL ) {
				pragma = comma_ptr+1;
				if( pragma[0]==' ' ) pragma++;
			}
		} while( comma_ptr!=NULL );
		pragma = http_get_next_field( http_hdr );
	}

	return asf_http_streaming_type( content_type, features );
}

int
asf_http_streaming_start( stream_t *stream ) {
	HTTP_header_t *http_hdr=NULL;
	URL_t *url_next=NULL;
	URL_t *url = stream->streaming_ctrl->url;
	asf_http_streaming_ctrl_t *asf_http_ctrl;
	ASF_StreamType_e streaming_type;
	char buffer[BUFFER_SIZE];
	int i, ret;
	int fd = stream->fd;
	int done;

	asf_http_ctrl = (asf_http_streaming_ctrl_t*)malloc(sizeof(asf_http_streaming_ctrl_t));
	if( asf_http_ctrl==NULL ) {
		printf("Memory allocation failed\n");
		return -1;
	}
	asf_http_ctrl->streaming_type = ASF_Unknown_e;
	asf_http_ctrl->request = 1;
	asf_http_ctrl->audio_streams = asf_http_ctrl->video_streams = NULL;
	asf_http_ctrl->n_audio = asf_http_ctrl->n_video = 0;
	stream->streaming_ctrl->data = (void*)asf_http_ctrl;

	do {
		done = 1;
		if( fd>0 ) close( fd );

		if( !strcasecmp( url->protocol, "http_proxy" ) ) {
			if( url->port==0 ) url->port = 8080;
		} else {
			if( url->port==0 ) url->port = 80;
		}
		fd = connect2Server( url->hostname, url->port );
		if( fd<0 ) return -1;

		http_hdr = asf_http_request( stream->streaming_ctrl );
		if( verbose>0 ) {
			printf("Request [%s]\n", http_hdr->buffer );
		}
		for(i=0; i <  http_hdr->buffer_size ; ) {
			int r = write( fd, http_hdr->buffer+i, http_hdr->buffer_size-i );
			if(r <0) {
				printf("Socket write error : %s\n",strerror(errno));
				return -1;
			}
			i += r;
		}       
		http_free( http_hdr );
		http_hdr = http_new_header();
		do {
			i = read( fd, buffer, BUFFER_SIZE );
//printf("read: %d\n", i );
			if( i<0 ) {
				perror("read");
				http_free( http_hdr );
				return -1;
			}
			http_response_append( http_hdr, buffer, i );
		} while( !http_is_header_entire( http_hdr ) );
		if( verbose>0 ) {
			http_hdr->buffer[http_hdr->buffer_size]='\0';
			printf("Response [%s]\n", http_hdr->buffer );
		}
		streaming_type = asf_http_parse_response(http_hdr);
		if( streaming_type<0 ) {
			printf("Failed to parse header\n");
			http_free( http_hdr );
			return -1;
		}
		asf_http_ctrl->streaming_type = streaming_type;
		switch( streaming_type ) {
			case ASF_Live_e:
			case ASF_Prerecorded_e:
			case ASF_PlainText_e:
				if( http_hdr->body_size>0 ) {
					if( streaming_bufferize( stream->streaming_ctrl, http_hdr->body, http_hdr->body_size )<0 ) {
						http_free( http_hdr );
						return -1;
					}
				}
				if( asf_http_ctrl->request==1 ) {
					if( streaming_type!=ASF_PlainText_e ) {
						// First request, we only got the ASF header.
						ret = asf_streaming_parse_header(fd,stream->streaming_ctrl);
						if(ret < 0) return -1;
						if(asf_http_ctrl->n_audio == 0 && asf_http_ctrl->n_video == 0) {
							printf("No stream found\n");
							return -1;
						}
					}
					asf_http_ctrl->request++;
					done = 0;
				}
				break;
			case ASF_Redirector_e:
				if( http_hdr->body_size>0 ) {
					if( streaming_bufferize( stream->streaming_ctrl, http_hdr->body, http_hdr->body_size )<0 ) {
						http_free( http_hdr );
						return -1;
					}
				}
				stream->type = STREAMTYPE_PLAYLIST;
				done = 1;
				break;
			case ASF_Unknown_e:
			default:
				printf("Unknown ASF streaming type\n");
				close(fd);
				http_free( http_hdr );
				return -1;
		}
	// Check if we got a redirect.	
	} while(!done);

	stream->fd = fd;
	if( streaming_type==ASF_PlainText_e || streaming_type==ASF_Redirector_e ) {
		stream->streaming_ctrl->streaming_read = nop_streaming_read;
		stream->streaming_ctrl->streaming_seek = nop_streaming_seek;
	} else {
		stream->streaming_ctrl->streaming_read = asf_http_streaming_read;
		stream->streaming_ctrl->streaming_seek = asf_http_streaming_seek;
		stream->streaming_ctrl->buffering = 1;
	}
	stream->streaming_ctrl->status = streaming_playing_e;

	http_free( http_hdr );
	return 0;
}


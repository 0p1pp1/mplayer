/*
 * Network layer for MPlayer
 * by Bertrand BAUDET <bertrand_baudet@yahoo.com>
 * (C) 2001, MPlayer team.
 */

//#define DUMP2FILE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <errno.h>
#include <ctype.h>

#include "config.h"

#ifndef HAVE_WINSOCK2
#define closesocket close
#else
#include <winsock2.h>
#include <ws2tcpip.h>
#endif

#include "stream.h"
#include "demuxer.h"
#include "../m_config.h"

#include "network.h"
#include "http.h"
#include "cookies.h"
#include "url.h"
#include "asf.h"
#ifndef STREAMING_LIVE_DOT_COM
#include "rtp.h"
#endif
#include "pnm.h"
#include "realrtsp/rtsp_session.h"

#include "../version.h"

extern int verbose;
extern int stream_cache_size;

extern int mp_input_check_interrupt(int time);

int asf_streaming_start( stream_t *stream, int *demuxer_type );
int rtsp_streaming_start( stream_t *stream );

/* Variables for the command line option -user, -passwd, -bandwidth,
   -user-agent and -nocookies */

char *network_username=NULL;
char *network_password=NULL;
int   network_bandwidth=0;
int   network_cookies_enabled = 0;
char *network_useragent=NULL;

/* IPv6 options */
int   network_prefer_ipv4 = 0;
int   network_ipv4_only_proxy = 0;


static struct {
	char *mime_type;
	int demuxer_type;
} mime_type_table[] = {
	// MP3 streaming, some MP3 streaming server answer with audio/mpeg
	{ "audio/mpeg", DEMUXER_TYPE_AUDIO },
	// MPEG streaming
	{ "video/mpeg", DEMUXER_TYPE_MPEG_PS },
	{ "video/x-mpeg", DEMUXER_TYPE_MPEG_PS },
	{ "video/x-mpeg2", DEMUXER_TYPE_MPEG_PS },
	// AVI ??? => video/x-msvideo
	{ "video/x-msvideo", DEMUXER_TYPE_AVI },
	// MOV => video/quicktime
	{ "video/quicktime", DEMUXER_TYPE_MOV },
	// ASF
        { "audio/x-ms-wax", DEMUXER_TYPE_ASF },
	{ "audio/x-ms-wma", DEMUXER_TYPE_ASF },
	{ "video/x-ms-asf", DEMUXER_TYPE_ASF },
	{ "video/x-ms-afs", DEMUXER_TYPE_ASF },
	{ "video/x-ms-wvx", DEMUXER_TYPE_ASF },
	{ "video/x-ms-wmv", DEMUXER_TYPE_ASF },
	{ "video/x-ms-wma", DEMUXER_TYPE_ASF },
	// Playlists
	{ "video/x-ms-wmx", DEMUXER_TYPE_PLAYLIST },
	{ "audio/x-scpls", DEMUXER_TYPE_PLAYLIST },
	{ "audio/x-mpegurl", DEMUXER_TYPE_PLAYLIST },
	{ "audio/x-pls", DEMUXER_TYPE_PLAYLIST },
	// Real Media
	{ "audio/x-pn-realaudio", DEMUXER_TYPE_REAL },
	// OGG Streaming
	{ "application/x-ogg", DEMUXER_TYPE_OGG },
	// NullSoft Streaming Video
	{ "video/nsv", DEMUXER_TYPE_NSV}

};

/*
 * An autodetection based on the extension is not a good idea.
 * 
static struct {
	char *extension;
	int demuxer_type;
} extensions_table[] = {
	{ "mpeg", DEMUXER_TYPE_MPEG_PS },
	{ "mpg", DEMUXER_TYPE_MPEG_PS },
	{ "mpe", DEMUXER_TYPE_MPEG_ES },
	{ "avi", DEMUXER_TYPE_AVI },
	{ "mov", DEMUXER_TYPE_MOV },
	{ "qt", DEMUXER_TYPE_MOV },
	{ "asx", DEMUXER_TYPE_ASF },
	{ "asf", DEMUXER_TYPE_ASF },
	{ "wmv", DEMUXER_TYPE_ASF },
	{ "wma", DEMUXER_TYPE_ASF },
	{ "viv", DEMUXER_TYPE_VIVO },
	{ "rm", DEMUXER_TYPE_REAL },
	{ "ra", DEMUXER_TYPE_REAL },
	{ "y4m", DEMUXER_TYPE_Y4M },
	{ "mp3", DEMUXER_TYPE_AUDIO },
	{ "ogg", DEMUXER_TYPE_OGG },
	{ "wav", DEMUXER_TYPE_AUDIO },
	{ "pls", DEMUXER_TYPE_PLAYLIST },
	{ "m3u", DEMUXER_TYPE_PLAYLIST }
};
*/

streaming_ctrl_t *
streaming_ctrl_new( ) {
	streaming_ctrl_t *streaming_ctrl;
	streaming_ctrl = (streaming_ctrl_t*)malloc(sizeof(streaming_ctrl_t));
	if( streaming_ctrl==NULL ) {
		mp_msg(MSGT_NETWORK,MSGL_FATAL,"Failed to allocate memory\n");
		return NULL;
	}
	memset( streaming_ctrl, 0, sizeof(streaming_ctrl_t) );
	return streaming_ctrl;
}

void
streaming_ctrl_free( streaming_ctrl_t *streaming_ctrl ) {
	if( streaming_ctrl==NULL ) return;
	if( streaming_ctrl->url ) url_free( streaming_ctrl->url );
	if( streaming_ctrl->buffer ) free( streaming_ctrl->buffer );
	if( streaming_ctrl->data ) free( streaming_ctrl->data );
	free( streaming_ctrl );
}

#ifndef STREAMING_LIVE_DOT_COM
int
read_rtp_from_server(int fd, char *buffer, int length) {
	struct rtpheader rh;
	char *data;
	int len;
	static int got_first = 0;
	static unsigned short sequence;

	if( buffer==NULL || length<0 ) return -1;

	getrtp2(fd, &rh, &data, &len);
	if( got_first && rh.b.sequence != (unsigned short)(sequence+1) )
		mp_msg(MSGT_NETWORK,MSGL_ERR,"RTP packet sequence error!  Expected: %d, received: %d\n", 
			sequence+1, rh.b.sequence);
	got_first = 1;
	sequence = rh.b.sequence;
	memcpy(buffer, data, len);
	return(len);
}
#endif


// Converts an address family constant to a string

char *af2String(int af) {
	switch (af) {
		case AF_INET:	return "AF_INET";
		
#ifdef HAVE_AF_INET6
		case AF_INET6:	return "AF_INET6";
#endif
		default:	return "Unknown address family!";
	}
}



// Connect to a server using a TCP connection, with specified address family
// return -2 for fatal error, like unable to resolve name, connection timeout...
// return -1 is unable to connect to a particular port

int
connect2Server_with_af(char *host, int port, int af,int verb) {
	int socket_server_fd;
	int err, err_len;
	int ret,count = 0;
	fd_set set;
	struct timeval tv;
	union {
		struct sockaddr_in four;
#ifdef HAVE_AF_INET6
		struct sockaddr_in6 six;
#endif
	} server_address;
	size_t server_address_size;
	void *our_s_addr;	// Pointer to sin_addr or sin6_addr
	struct hostent *hp=NULL;
	char buf[255];
	
#ifdef HAVE_WINSOCK2
	u_long val;
#endif
	
	socket_server_fd = socket(af, SOCK_STREAM, 0);
	
	
	if( socket_server_fd==-1 ) {
//		mp_msg(MSGT_NETWORK,MSGL_ERR,"Failed to create %s socket:\n", af2String(af));
		return -2;
	}

	switch (af) {
		case AF_INET:  our_s_addr = (void *) &server_address.four.sin_addr; break;
#ifdef HAVE_AF_INET6
		case AF_INET6: our_s_addr = (void *) &server_address.six.sin6_addr; break;
#endif
		default:
			mp_msg(MSGT_NETWORK,MSGL_ERR, "Unknown address family %d:\n", af);
			return -2;
	}
	
	
	bzero(&server_address, sizeof(server_address));
	
#ifndef HAVE_WINSOCK2
#ifdef USE_ATON
	if (inet_aton(host, our_s_addr)!=1)
#else
	if (inet_pton(af, host, our_s_addr)!=1)
#endif
#else
	if ( inet_addr(host)==INADDR_NONE )
#endif
	{
		if(verb) mp_msg(MSGT_NETWORK,MSGL_STATUS,"Resolving %s for %s...\n", host, af2String(af));
		
#ifdef HAVE_GETHOSTBYNAME2
		hp=(struct hostent*)gethostbyname2( host, af );
#else
		hp=(struct hostent*)gethostbyname( host );
#endif
		if( hp==NULL ) {
			if(verb) mp_msg(MSGT_NETWORK,MSGL_ERR,"Couldn't resolve name for %s: %s\n", af2String(af), host);
			return -2;
		}
		
		memcpy( our_s_addr, (void*)hp->h_addr, hp->h_length );
	}
#ifdef HAVE_WINSOCK2
	else {
		unsigned long addr = inet_addr(host);
		memcpy( our_s_addr, (void*)&addr, sizeof(addr) );
	}
#endif
	
	switch (af) {
		case AF_INET:
			server_address.four.sin_family=af;
			server_address.four.sin_port=htons(port);			
			server_address_size = sizeof(server_address.four);
			break;
#ifdef HAVE_AF_INET6		
		case AF_INET6:
			server_address.six.sin6_family=af;
			server_address.six.sin6_port=htons(port);
			server_address_size = sizeof(server_address.six);
			break;
#endif
		default:
			mp_msg(MSGT_NETWORK,MSGL_ERR, "Unknown address family %d:\n", af);
			return -2;
	}

#if defined(USE_ATON) || defined(HAVE_WINSOCK2)
	strncpy( buf, inet_ntoa( *((struct in_addr*)our_s_addr) ), 255);
#else
	inet_ntop(af, our_s_addr, buf, 255);
#endif
	if(verb) mp_msg(MSGT_NETWORK,MSGL_STATUS,"Connecting to server %s[%s]:%d ...\n", host, buf , port );

	// Turn the socket as non blocking so we can timeout on the connection
#ifndef HAVE_WINSOCK2
	fcntl( socket_server_fd, F_SETFL, fcntl(socket_server_fd, F_GETFL) | O_NONBLOCK );
#else
	val = 1;
	ioctlsocket( socket_server_fd, FIONBIO, &val );
#endif
	if( connect( socket_server_fd, (struct sockaddr*)&server_address, server_address_size )==-1 ) {
#ifndef HAVE_WINSOCK2
		if( errno!=EINPROGRESS ) {
#else
		if( (WSAGetLastError() != WSAEINPROGRESS) && (WSAGetLastError() != WSAEWOULDBLOCK) ) {
#endif
			if(verb) mp_msg(MSGT_NETWORK,MSGL_ERR,"Failed to connect to server with %s\n", af2String(af));
			closesocket(socket_server_fd);
			return -1;
		}
	}
	tv.tv_sec = 0;
	tv.tv_usec = 500000;
	FD_ZERO( &set );
	FD_SET( socket_server_fd, &set );
	// When the connection will be made, we will have a writable fd
	while((ret = select(socket_server_fd+1, NULL, &set, NULL, &tv)) == 0) {
	      if( ret<0 ) mp_msg(MSGT_NETWORK,MSGL_ERR,"select failed\n");
	      else if(ret > 0) break;
	      else if(count > 30 || mp_input_check_interrupt(500)) {
		if(count > 30)
		  mp_msg(MSGT_NETWORK,MSGL_ERR,"Connection timeout\n");
		else
		  mp_msg(MSGT_NETWORK,MSGL_V,"Connection interuppted by user\n");
		return -3;
	      }
	      count++;
	      FD_ZERO( &set );
	      FD_SET( socket_server_fd, &set );
	      tv.tv_sec = 0;
	      tv.tv_usec = 500000;
	}

	// Turn back the socket as blocking
#ifndef HAVE_WINSOCK2
	fcntl( socket_server_fd, F_SETFL, fcntl(socket_server_fd, F_GETFL) & ~O_NONBLOCK );
#else
	val = 0;
	ioctlsocket( socket_server_fd, FIONBIO, &val );
#endif
	// Check if there were any error
	err_len = sizeof(int);
	ret =  getsockopt(socket_server_fd,SOL_SOCKET,SO_ERROR,&err,&err_len);
	if(ret < 0) {
		mp_msg(MSGT_NETWORK,MSGL_ERR,"getsockopt failed : %s\n",strerror(errno));
		return -2;
	}
	if(err > 0) {
		mp_msg(MSGT_NETWORK,MSGL_ERR,"Connect error : %s\n",strerror(err));
		return -1;
	}
	
	return socket_server_fd;
}

// Connect to a server using a TCP connection
// return -2 for fatal error, like unable to resolve name, connection timeout...
// return -1 is unable to connect to a particular port


int
connect2Server(char *host, int  port, int verb) {
#ifdef HAVE_AF_INET6
	int r;
	int s = -2;

	r = connect2Server_with_af(host, port, network_prefer_ipv4 ? AF_INET:AF_INET6,verb);	
	if (r > -1) return r;

	s = connect2Server_with_af(host, port, network_prefer_ipv4 ? AF_INET6:AF_INET,verb);
	if (s == -2) return r;
	return s;
#else	
	return connect2Server_with_af(host, port, AF_INET,verb);
#endif

	
}

URL_t*
check4proxies( URL_t *url ) {
	URL_t *url_out = NULL;
	if( url==NULL ) return NULL;
	url_out = url_new( url->url );
	if( !strcasecmp(url->protocol, "http_proxy") ) {
		mp_msg(MSGT_NETWORK,MSGL_V,"Using HTTP proxy: http://%s:%d\n", url->hostname, url->port );
		return url_out;
	}
	// Check if the http_proxy environment variable is set.
	if( !strcasecmp(url->protocol, "http") ) {
		char *proxy;
		proxy = getenv("http_proxy");
		if( proxy!=NULL ) {
			// We got a proxy, build the URL to use it
			int len;
			char *new_url;
			URL_t *tmp_url;
			URL_t *proxy_url = url_new( proxy );

			if( proxy_url==NULL ) {
				mp_msg(MSGT_NETWORK,MSGL_WARN,"Invalid proxy setting...Trying without proxy.\n");
				return url_out;
			}
			
#ifdef HAVE_AF_INET6
			if (network_ipv4_only_proxy && (gethostbyname(url->hostname)==NULL)) {
				mp_msg(MSGT_NETWORK,MSGL_WARN,
					"Could not find resolve remote hostname for AF_INET. Trying without proxy.\n");
				return url_out;
			}
#endif

			mp_msg(MSGT_NETWORK,MSGL_V,"Using HTTP proxy: %s\n", proxy_url->url );
			len = strlen( proxy_url->hostname ) + strlen( url->url ) + 20;	// 20 = http_proxy:// + port
			new_url = malloc( len+1 );
			if( new_url==NULL ) {
				mp_msg(MSGT_NETWORK,MSGL_FATAL,"Memory allocation failed\n");
				return url_out;
			}
			sprintf(new_url, "http_proxy://%s:%d/%s", proxy_url->hostname, proxy_url->port, url->url );
			tmp_url = url_new( new_url );
			if( tmp_url==NULL ) {
				return url_out;
			}
			url_free( url_out );
			url_out = tmp_url;
			free( new_url );
			url_free( proxy_url );
		}
	}
	return url_out;
}

int
http_send_request( URL_t *url, off_t pos ) {
	HTTP_header_t *http_hdr;
	URL_t *server_url;
	char str[256];
	int fd;
	int ret;
	int proxy = 0;		// Boolean

	http_hdr = http_new_header();

	if( !strcasecmp(url->protocol, "http_proxy") ) {
		proxy = 1;
		server_url = url_new( (url->file)+1 );
		http_set_uri( http_hdr, server_url->url );
	} else {
		server_url = url;
		http_set_uri( http_hdr, server_url->file );
	}
	snprintf(str, 256, "Host: %s", server_url->hostname );
	http_set_field( http_hdr, str);
	if (network_useragent)
	{
	    snprintf(str, 256, "User-Agent: %s", network_useragent);
	    http_set_field(http_hdr, str);
	}
	else
	    http_set_field( http_hdr, "User-Agent: MPlayer/"VERSION);

	if(pos>0) { 
	// Extend http_send_request with possibility to do partial content retrieval
	    snprintf(str, 256, "Range: bytes=%d-", pos);
	    http_set_field(http_hdr, str);
	}
	    
	if (network_cookies_enabled) cookies_set( http_hdr, server_url->hostname, server_url->url );
	
	http_set_field( http_hdr, "Connection: closed");
	http_add_basic_authentication( http_hdr, url->username, url->password );
	if( http_build_request( http_hdr )==NULL ) {
		return -1;
	}

	if( proxy ) {
		if( url->port==0 ) url->port = 8080;			// Default port for the proxy server
		fd = connect2Server( url->hostname, url->port,1 );
		url_free( server_url );
	} else {
		if( server_url->port==0 ) server_url->port = 80;	// Default port for the web server
		fd = connect2Server( server_url->hostname, server_url->port,1 );
	}
	if( fd<0 ) {
		return -1; 
	}
	mp_msg(MSGT_NETWORK,MSGL_DBG2,"Request: [%s]\n", http_hdr->buffer );
	
	ret = send( fd, http_hdr->buffer, http_hdr->buffer_size, 0 );
	if( ret!=(int)http_hdr->buffer_size ) {
		mp_msg(MSGT_NETWORK,MSGL_ERR,"Error while sending HTTP request: didn't sent all the request\n");
		return -1;
	}
	
	http_free( http_hdr );

	return fd;
}

HTTP_header_t *
http_read_response( int fd ) {
	HTTP_header_t *http_hdr;
	char response[BUFFER_SIZE];
	int i;

	http_hdr = http_new_header();
	if( http_hdr==NULL ) {
		return NULL;
	}

	do {
		i = recv( fd, response, BUFFER_SIZE, 0 ); 
		if( i<0 ) {
			mp_msg(MSGT_NETWORK,MSGL_ERR,"Read failed\n");
			http_free( http_hdr );
			return NULL;
		}
		if( i==0 ) {
			mp_msg(MSGT_NETWORK,MSGL_ERR,"http_read_response read 0 -ie- EOF\n");
			http_free( http_hdr );
			return NULL;
		}
		http_response_append( http_hdr, response, i );
	} while( !http_is_header_entire( http_hdr ) ); 
	http_response_parse( http_hdr );
	return http_hdr;
}

int
http_authenticate(HTTP_header_t *http_hdr, URL_t *url, int *auth_retry) {
	char *aut;

	if( *auth_retry==1 ) {
		mp_msg(MSGT_NETWORK,MSGL_ERR,"Authentication failed\n");
		mp_msg(MSGT_NETWORK,MSGL_ERR,"Please use the option -user and -passwd to provide your username/password for a list of URLs,\n");
		mp_msg(MSGT_NETWORK,MSGL_ERR,"or form an URL like: http://username:password@hostname/file\n");
		return -1;
	}
	if( *auth_retry>0 ) {
		if( url->username ) {
			free( url->username );
			url->username = NULL;
		}
		if( url->password ) {
			free( url->password );
			url->password = NULL;
		}
	}

	aut = http_get_field(http_hdr, "WWW-Authenticate");
	if( aut!=NULL ) {
		char *aut_space;
		aut_space = strstr(aut, "realm=");
		if( aut_space!=NULL ) aut_space += 6;
		mp_msg(MSGT_NETWORK,MSGL_INFO,"Authentication required for %s\n", aut_space);
	} else {
		mp_msg(MSGT_NETWORK,MSGL_INFO,"Authentication required\n");
	}
	if( network_username ) {
		url->username = strdup(network_username);
		if( url->username==NULL ) {
			mp_msg(MSGT_NETWORK,MSGL_FATAL,"Memory allocation failed\n");
			return -1;
		}
	} else {
		mp_msg(MSGT_NETWORK,MSGL_ERR,"Unable to read the username\n");
		mp_msg(MSGT_NETWORK,MSGL_ERR,"Please use the option -user and -passwd to provide your username/password for a list of URLs,\n");
		mp_msg(MSGT_NETWORK,MSGL_ERR,"or form an URL like: http://username:password@hostname/file\n");
		return -1;
	}
	if( network_password ) {
		url->password = strdup(network_password);
		if( url->password==NULL ) {
			mp_msg(MSGT_NETWORK,MSGL_FATAL,"Memory allocation failed\n");
			return -1;
		}
	} else {
		mp_msg(MSGT_NETWORK,MSGL_INFO,"No password provided, trying blank password\n");
	}
	(*auth_retry)++;
	return 0;
}

int
http_seek( stream_t *stream, off_t pos ) {
	HTTP_header_t *http_hdr = NULL;
	int fd;
	if( stream==NULL ) return 0;

	if( stream->fd>0 ) closesocket(stream->fd); // need to reconnect to seek in http-stream
	fd = http_send_request( stream->streaming_ctrl->url, pos ); 
	if( fd<0 ) return 0;

	http_hdr = http_read_response( fd );

	if( http_hdr==NULL ) return 0;

	switch( http_hdr->status_code ) {
		case 200:
		case 206: // OK
			mp_msg(MSGT_NETWORK,MSGL_V,"Content-Type: [%s]\n", http_get_field(http_hdr, "Content-Type") );
			mp_msg(MSGT_NETWORK,MSGL_V,"Content-Length: [%s]\n", http_get_field(http_hdr, "Content-Length") );
			if( http_hdr->body_size>0 ) {
				if( streaming_bufferize( stream->streaming_ctrl, http_hdr->body, http_hdr->body_size )<0 ) {
					http_free( http_hdr );
					return -1;
				}
			}
			break;
		default:
			mp_msg(MSGT_NETWORK,MSGL_ERR,"Server return %d: %s\n", http_hdr->status_code, http_hdr->reason_phrase );
			close( fd );
			fd = -1;
	}
	stream->fd = fd;

	if( http_hdr ) {
		http_free( http_hdr );
		stream->streaming_ctrl->data = NULL;
	}

	stream->pos=pos;

	return 1;
}


// By using the protocol, the extension of the file or the content-type
// we might be able to guess the streaming type.
int
autodetectProtocol(streaming_ctrl_t *streaming_ctrl, int *fd_out, int *file_format) {
	HTTP_header_t *http_hdr;
	unsigned int i;
	int fd=-1;
	int redirect;
	int auth_retry=0;
	int seekable=0;
	char *extension;
	char *content_type;
	char *next_url;

	URL_t *url = streaming_ctrl->url;
	*file_format = DEMUXER_TYPE_UNKNOWN;

	do {
		*fd_out = -1;
		next_url = NULL;
		extension = NULL;
		content_type = NULL;
		redirect = 0;

		if( url==NULL ) {
			return -1;
		}

		// Checking for PNM://
		if( !strcasecmp(url->protocol, "pnm") ) {
			*file_format = DEMUXER_TYPE_REAL;
			return 0;
		}
		
/* 
 * An autodetection based on the extension is not a good idea.
 * 
		// Get the extension of the file if present
		if( url->file!=NULL ) {
			for( i=strlen(url->file) ; i>0 ; i-- ) {
				if( url->file[i]=='.' ) {
					extension=(url->file)+i+1;
					break;
				}
			}
		}
extension=NULL;	
		if( extension!=NULL ) {
			mp_msg(MSGT_NETWORK,MSGL_DBG2,"Extension: %s\n", extension );
			// Look for the extension in the extensions table
			for( i=0 ; i<(sizeof(extensions_table)/sizeof(extensions_table[0])) ; i++ ) {
				if( !strcasecmp(extension, extensions_table[i].extension) ) {
					*file_format = extensions_table[i].demuxer_type;
					return 0;
				}
			}
		}
*/
		
		// Checking for RTSP
		if( !strcasecmp(url->protocol, "rtsp") ) {
			// Try Real rtsp:// first (it's always built in)
			// If it fails, try live.com (if compiled in)
			*file_format = DEMUXER_TYPE_REAL;
			return 0;
		// Checking for SIP
		} else if( !strcasecmp(url->protocol, "sip") ) {
#ifdef STREAMING_LIVE_DOT_COM
			*file_format = DEMUXER_TYPE_RTP;
			return 0;
#else
			mp_msg(MSGT_NETWORK,MSGL_ERR,"SIP support requires the \"LIVE.COM Streaming Media\" libraries!\n");
			return -1;
#endif
		}

#ifndef STREAMING_LIVE_DOT_COM
	// Old, hacked RTP support, which works for MPEG Program Streams
	//   RTP streams only:
		// Checking for RTP
		if( !strcasecmp(url->protocol, "rtp") ) {
			if( url->port==0 ) {
				mp_msg(MSGT_NETWORK,MSGL_ERR,"You must enter a port number for RTP streams!\n");
				return -1;
			}
			return 0;
		}
#endif

		// Checking for ASF
		if( !strncasecmp(url->protocol, "mms", 3) ) {
			*file_format = DEMUXER_TYPE_ASF;
			return 0;
		}

		// HTTP based protocol
		if( !strcasecmp(url->protocol, "http") || !strcasecmp(url->protocol, "http_proxy") ) {
			fd = http_send_request( url, 0 );
			if( fd<0 ) {
				return -1;
			}

			http_hdr = http_read_response( fd );
			if( http_hdr==NULL ) {
				closesocket( fd );
				http_free( http_hdr );
				return -1;
			}

			*fd_out=fd;
			if( verbose>0 ) {
				http_debug_hdr( http_hdr );
			}
			
			streaming_ctrl->data = (void*)http_hdr;

			// Check if we can make partial content requests and thus seek in http-streams
		        if( http_hdr!=NULL && http_hdr->status_code==200 ) {
			    char *accept_ranges;
			    if( (accept_ranges = http_get_field(http_hdr,"Accept-Ranges")) != NULL )
				seekable = strncmp(accept_ranges,"bytes",5)==0;
			} 

			// Check if the response is an ICY status_code reason_phrase
			if( !strcasecmp(http_hdr->protocol, "ICY") ) {
				switch( http_hdr->status_code ) {
					case 200: { // OK
						char *field_data = NULL;
						// note: I skip icy-notice1 and 2, as they contain html <BR>
						// and are IMHO useless info ::atmos
						if( (field_data = http_get_field(http_hdr, "icy-name")) != NULL )
							mp_msg(MSGT_NETWORK,MSGL_INFO,"Name   : %s\n", field_data); field_data = NULL;
						if( (field_data = http_get_field(http_hdr, "icy-genre")) != NULL )
							mp_msg(MSGT_NETWORK,MSGL_INFO,"Genre  : %s\n", field_data); field_data = NULL;
						if( (field_data = http_get_field(http_hdr, "icy-url")) != NULL )
							mp_msg(MSGT_NETWORK,MSGL_INFO,"Website: %s\n", field_data); field_data = NULL;
						// XXX: does this really mean public server? ::atmos
						if( (field_data = http_get_field(http_hdr, "icy-pub")) != NULL )
							mp_msg(MSGT_NETWORK,MSGL_INFO,"Public : %s\n", atoi(field_data)?"yes":"no"); field_data = NULL;
						if( (field_data = http_get_field(http_hdr, "icy-br")) != NULL )
							mp_msg(MSGT_NETWORK,MSGL_INFO,"Bitrate: %skbit/s\n", field_data); field_data = NULL;
						
						// If content-type == video/nsv we most likely have a winamp video stream 
						// otherwise it should be mp3. if there are more types consider adding mime type 
						// handling like later
						if( !strcmp((field_data = http_get_field(http_hdr, "content-type")),"video/nsv"))
							*file_format = DEMUXER_TYPE_NSV;
						else
							*file_format = DEMUXER_TYPE_AUDIO;
						return 0;
					}
					case 400: // Server Full
						mp_msg(MSGT_NETWORK,MSGL_ERR,"Error: ICY-Server is full, skipping!\n");
						return -1;
					case 401: // Service Unavailable
						mp_msg(MSGT_NETWORK,MSGL_ERR,"Error: ICY-Server return service unavailable, skipping!\n");
						return -1;
					case 403: // Service Forbidden
						mp_msg(MSGT_NETWORK,MSGL_ERR,"Error: ICY-Server return 'Service Forbidden'\n");
						return -1;
					case 404: // Resource Not Found
						mp_msg(MSGT_NETWORK,MSGL_ERR,"Error: ICY-Server couldn't find requested stream, skipping!\n");
						return -1;
					default:
						mp_msg(MSGT_NETWORK,MSGL_ERR,"Error: unhandled ICY-Errorcode, contact MPlayer developers!\n");
						return -1;
				}
			}

			// Assume standard http if not ICY			
			switch( http_hdr->status_code ) {
				case 200: // OK
					// Look if we can use the Content-Type
					content_type = http_get_field( http_hdr, "Content-Type" );
					if( content_type!=NULL ) {
						char *content_length = NULL;
						mp_msg(MSGT_NETWORK,MSGL_V,"Content-Type: [%s]\n", content_type );
						if( (content_length = http_get_field(http_hdr, "Content-Length")) != NULL)
							mp_msg(MSGT_NETWORK,MSGL_V,"Content-Length: [%s]\n", http_get_field(http_hdr, "Content-Length"));
						// Check in the mime type table for a demuxer type
						for( i=0 ; i<(sizeof(mime_type_table)/sizeof(mime_type_table[0])) ; i++ ) {
							if( !strcasecmp( content_type, mime_type_table[i].mime_type ) ) {
								*file_format = mime_type_table[i].demuxer_type;
								return seekable; // for streaming_start
							}
						}
					}
					// Not found in the mime type table, don't fail,
					// we should try raw HTTP
					return seekable; // for streaming_start
				// Redirect
				case 301: // Permanently
				case 302: // Temporarily
					// TODO: RFC 2616, recommand to detect infinite redirection loops
					next_url = http_get_field( http_hdr, "Location" );
					if( next_url!=NULL ) {
						closesocket( fd );
						url_free( url );
						streaming_ctrl->url = url = url_new( next_url );
						http_free( http_hdr );
						redirect = 1;	
					}
					break;
				case 401: // Authentication required
					if( http_authenticate(http_hdr, url, &auth_retry)<0 ) return -1;
					redirect = 1;
					break;
				default:
					mp_msg(MSGT_NETWORK,MSGL_ERR,"Server returned %d: %s\n", http_hdr->status_code, http_hdr->reason_phrase );
					return -1;
			}
		} else {
			mp_msg(MSGT_NETWORK,MSGL_V,"Unknown protocol '%s'\n", url->protocol );
			return -1;
		}
	} while( redirect );

	return -1;
}

int
streaming_bufferize( streaming_ctrl_t *streaming_ctrl, char *buffer, int size) {
//printf("streaming_bufferize\n");
	streaming_ctrl->buffer = (char*)malloc(size);
	if( streaming_ctrl->buffer==NULL ) {
		mp_msg(MSGT_NETWORK,MSGL_FATAL,"Memory allocation failed\n");
		return -1;
	}
	memcpy( streaming_ctrl->buffer, buffer, size );
	streaming_ctrl->buffer_size = size;
	return size;
}

int
nop_streaming_read( int fd, char *buffer, int size, streaming_ctrl_t *stream_ctrl ) {
	int len=0;
//printf("nop_streaming_read\n");
	if( stream_ctrl->buffer_size!=0 ) {
		int buffer_len = stream_ctrl->buffer_size-stream_ctrl->buffer_pos;
//printf("%d bytes in buffer\n", stream_ctrl->buffer_size);
		len = (size<buffer_len)?size:buffer_len;
		memcpy( buffer, (stream_ctrl->buffer)+(stream_ctrl->buffer_pos), len );
		stream_ctrl->buffer_pos += len;
//printf("buffer_pos = %d\n", stream_ctrl->buffer_pos );
		if( stream_ctrl->buffer_pos>=stream_ctrl->buffer_size ) {
			free( stream_ctrl->buffer );
			stream_ctrl->buffer = NULL;
			stream_ctrl->buffer_size = 0;
			stream_ctrl->buffer_pos = 0;
//printf("buffer cleaned\n");
		}
//printf("read %d bytes from buffer\n", len );
	}

	if( len<size ) {
		int ret;
		ret = recv( fd, buffer+len, size-len, 0 );
		if( ret<0 ) {
			mp_msg(MSGT_NETWORK,MSGL_ERR,"nop_streaming_read error : %s\n",strerror(errno));
		}
		len += ret;
//printf("read %d bytes from network\n", len );
	}
	
	return len;
}

int
nop_streaming_seek( int fd, off_t pos, streaming_ctrl_t *stream_ctrl ) {
	return -1;
	// To shut up gcc warning
	fd++;
	pos++;
	stream_ctrl=NULL;
}

int
nop_streaming_start( stream_t *stream ) {
	HTTP_header_t *http_hdr = NULL;
	int fd;
	if( stream==NULL ) return -1;

	fd = stream->fd;
	if( fd<0 ) {
		fd = http_send_request( stream->streaming_ctrl->url, 0 ); 
		if( fd<0 ) return -1;
		http_hdr = http_read_response( fd );
		if( http_hdr==NULL ) return -1;

		switch( http_hdr->status_code ) {
			case 200: // OK
				mp_msg(MSGT_NETWORK,MSGL_V,"Content-Type: [%s]\n", http_get_field(http_hdr, "Content-Type") );
				mp_msg(MSGT_NETWORK,MSGL_V,"Content-Length: [%s]\n", http_get_field(http_hdr, "Content-Length") );
				if( http_hdr->body_size>0 ) {
					if( streaming_bufferize( stream->streaming_ctrl, http_hdr->body, http_hdr->body_size )<0 ) {
						http_free( http_hdr );
						return -1;
					}
				}
				break;
			default:
				mp_msg(MSGT_NETWORK,MSGL_ERR,"Server return %d: %s\n", http_hdr->status_code, http_hdr->reason_phrase );
				closesocket( fd );
				fd = -1;
		}
		stream->fd = fd;
	} else {
		http_hdr = (HTTP_header_t*)stream->streaming_ctrl->data;
		if( http_hdr->body_size>0 ) {
			if( streaming_bufferize( stream->streaming_ctrl, http_hdr->body, http_hdr->body_size )<0 ) {
				http_free( http_hdr );
				stream->streaming_ctrl->data = NULL;
				return -1;
			}
		}
	}

	if( http_hdr ) {
		http_free( http_hdr );
		stream->streaming_ctrl->data = NULL;
	}

	stream->streaming_ctrl->streaming_read = nop_streaming_read;
	stream->streaming_ctrl->streaming_seek = nop_streaming_seek;
	stream->streaming_ctrl->prebuffer_size = 64*1024; // 64 KBytes
	stream->streaming_ctrl->buffering = 1;
	stream->streaming_ctrl->status = streaming_playing_e;
	return 0;
}

int
pnm_streaming_read( int fd, char *buffer, int size, streaming_ctrl_t *stream_ctrl ) {
	return pnm_read(stream_ctrl->data, buffer, size);
}


int
pnm_streaming_start( stream_t *stream ) {
	int fd;
	pnm_t *pnm;
	if( stream==NULL ) return -1;

	fd = connect2Server( stream->streaming_ctrl->url->hostname,
	    stream->streaming_ctrl->url->port ? stream->streaming_ctrl->url->port : 7070,1 );
	printf("PNM:// fd=%d\n",fd);
	if(fd<0) return -1;
	
	pnm = pnm_connect(fd,stream->streaming_ctrl->url->file);
	if(!pnm) return -1;

	stream->fd=fd;
	stream->streaming_ctrl->data=pnm;

	stream->streaming_ctrl->streaming_read = pnm_streaming_read;
//	stream->streaming_ctrl->streaming_seek = nop_streaming_seek;
	stream->streaming_ctrl->prebuffer_size = 8*1024;  // 8 KBytes
	stream->streaming_ctrl->buffering = 1;
	stream->streaming_ctrl->status = streaming_playing_e;
	return 0;
}


int
realrtsp_streaming_read( int fd, char *buffer, int size, streaming_ctrl_t *stream_ctrl ) {
	return rtsp_session_read(stream_ctrl->data, buffer, size);
}


int
realrtsp_streaming_start( stream_t *stream ) {
	int fd;
	rtsp_session_t *rtsp;
	char *mrl;
	int port;
	int redirected, temp;
	if( stream==NULL ) return -1;
	
	temp = 5; // counter so we don't get caught in infinite redirections (you never know)
	
	do {
	
		redirected = 0;

		fd = connect2Server( stream->streaming_ctrl->url->hostname,
			port = (stream->streaming_ctrl->url->port ? stream->streaming_ctrl->url->port : 554),1 );
		if(fd<0) return -1;
		
		mrl = malloc(sizeof(char)*(strlen(stream->streaming_ctrl->url->hostname)+strlen(stream->streaming_ctrl->url->file)+16));
		sprintf(mrl,"rtsp://%s:%i/%s",stream->streaming_ctrl->url->hostname,port,stream->streaming_ctrl->url->file);
		rtsp = rtsp_session_start(fd,&mrl, stream->streaming_ctrl->url->file,
			stream->streaming_ctrl->url->hostname, port, &redirected);

		if ( redirected == 1 ) {
			url_free(stream->streaming_ctrl->url);
			stream->streaming_ctrl->url = url_new(mrl);
			closesocket(fd);
		}

		free(mrl);
		temp--;

	} while( (redirected != 0) && (temp > 0) );	

	if(!rtsp) return -1;

	stream->fd=fd;
	stream->streaming_ctrl->data=rtsp;

	stream->streaming_ctrl->streaming_read = realrtsp_streaming_read;
//	stream->streaming_ctrl->streaming_seek = nop_streaming_seek;
	stream->streaming_ctrl->prebuffer_size = 128*1024;  // 8 KBytes
	stream->streaming_ctrl->buffering = 1;
	stream->streaming_ctrl->status = streaming_playing_e;
	return 0;
}


#ifndef STREAMING_LIVE_DOT_COM
// Start listening on a UDP port. If multicast, join the group.
int
rtp_open_socket( URL_t *url ) {
	int socket_server_fd, rxsockbufsz;
	int err, err_len;
	fd_set set;
	struct sockaddr_in server_address;
	struct ip_mreq mcast;
        struct timeval tv;

	mp_msg(MSGT_NETWORK,MSGL_V,"Listening for traffic on %s:%d ...\n", url->hostname, url->port );

	socket_server_fd = socket(AF_INET, SOCK_DGRAM, 0);
//	fcntl( socket_server_fd, F_SETFL, fcntl(socket_server_fd, F_GETFL) | O_NONBLOCK );
	if( socket_server_fd==-1 ) {
		mp_msg(MSGT_NETWORK,MSGL_ERR,"Failed to create socket\n");
		return -1;
	}

	if( isalpha(url->hostname[0]) ) {
		struct hostent *hp =(struct hostent*)gethostbyname( url->hostname );
		if( hp==NULL ) {
			mp_msg(MSGT_NETWORK,MSGL_ERR,"Counldn't resolve name: %s\n", url->hostname);
			return -1;
		}
		memcpy( (void*)&server_address.sin_addr.s_addr, (void*)hp->h_addr, hp->h_length );
	} else {
#ifndef HAVE_WINSOCK2
#ifdef USE_ATON
		inet_aton(url->hostname, &server_address.sin_addr);
#else
		inet_pton(AF_INET, url->hostname, &server_address.sin_addr);
#endif
#else
		unsigned int addr = inet_addr(url->hostname);
		memcpy( (void*)&server_address.sin_addr, (void*)&addr, sizeof(addr) );
#endif
	}
	server_address.sin_family=AF_INET;
	server_address.sin_port=htons(url->port);

	if( bind( socket_server_fd, (struct sockaddr*)&server_address, sizeof(server_address) )==-1 ) {
#ifndef HAVE_WINSOCK2
		if( errno!=EINPROGRESS ) {
#else
		if( WSAGetLastError() != WSAEINPROGRESS ) {
#endif
			mp_msg(MSGT_NETWORK,MSGL_ERR,"Failed to connect to server\n");
			closesocket(socket_server_fd);
			return -1;
		}
	}

	// Increase the socket rx buffer size to maximum -- this is UDP
	rxsockbufsz = 240 * 1024;
	if( setsockopt( socket_server_fd, SOL_SOCKET, SO_RCVBUF, &rxsockbufsz, sizeof(rxsockbufsz))) {
		mp_msg(MSGT_NETWORK,MSGL_ERR,"Couldn't set receive socket buffer size\n");
	}

	if((ntohl(server_address.sin_addr.s_addr) >> 28) == 0xe) {
		mcast.imr_multiaddr.s_addr = server_address.sin_addr.s_addr;
		//mcast.imr_interface.s_addr = inet_addr("10.1.1.2");
		mcast.imr_interface.s_addr = 0;
		if( setsockopt( socket_server_fd, IPPROTO_IP, IP_ADD_MEMBERSHIP, &mcast, sizeof(mcast))) {
			mp_msg(MSGT_NETWORK,MSGL_ERR,"IP_ADD_MEMBERSHIP failed (do you have multicasting enabled in your kernel?)\n");
			return -1;
		}
	}

	tv.tv_sec = 0;
	tv.tv_usec = (1 * 1000000);	// 1 second timeout
	FD_ZERO( &set );
	FD_SET( socket_server_fd, &set );
	if( select(socket_server_fd+1, &set, NULL, NULL, &tv)>0 ) {
        //if( select(socket_server_fd+1, &set, NULL, NULL, NULL)>0 ) {
		err_len = sizeof( err );
		getsockopt( socket_server_fd, SOL_SOCKET, SO_ERROR, &err, &err_len );
		if( err ) {
			mp_msg(MSGT_NETWORK,MSGL_ERR,"Timeout! No data from host %s\n", url->hostname );
			mp_msg(MSGT_NETWORK,MSGL_DBG2,"Socket error: %d\n", err );
			closesocket(socket_server_fd);
			return -1;
		}
	}
	return socket_server_fd;
}

int
rtp_streaming_read( int fd, char *buffer, int size, streaming_ctrl_t *streaming_ctrl ) {
    return read_rtp_from_server( fd, buffer, size );
}

int
rtp_streaming_start( stream_t *stream ) {
	streaming_ctrl_t *streaming_ctrl;
	int fd;

	if( stream==NULL ) return -1;
	streaming_ctrl = stream->streaming_ctrl;
	fd = stream->fd;
	
	if( fd<0 ) {
		fd = rtp_open_socket( (streaming_ctrl->url) ); 
		if( fd<0 ) return -1;
		stream->fd = fd;
	}

	streaming_ctrl->streaming_read = rtp_streaming_read;
	streaming_ctrl->streaming_seek = nop_streaming_seek;
	streaming_ctrl->prebuffer_size = 64*1024;	// 64 KBytes	
	streaming_ctrl->buffering = 0;
	streaming_ctrl->status = streaming_playing_e;
	return 0;
}
#endif

int
streaming_start(stream_t *stream, int *demuxer_type, URL_t *url) {
	int ret;
	if( stream==NULL ) return -1;

	stream->streaming_ctrl = streaming_ctrl_new();
	if( stream->streaming_ctrl==NULL ) {
		return -1;
	}
	stream->streaming_ctrl->url = check4proxies( url );
	ret = autodetectProtocol( stream->streaming_ctrl, &stream->fd, demuxer_type );
	if( ret<0 ) {
		return -1;
	}
	if( ret==1 ) {
		stream->flags |= STREAM_SEEK;
		stream->seek = http_seek;
	}

	ret = -1;
	
	// Get the bandwidth available
	stream->streaming_ctrl->bandwidth = network_bandwidth;
	
#ifndef STREAMING_LIVE_DOT_COM
	// For RTP streams, we usually don't know the stream type until we open it.
	if( !strcasecmp( stream->streaming_ctrl->url->protocol, "rtp")) {
		if(stream->fd >= 0) {
			if(closesocket(stream->fd) < 0)
				mp_msg(MSGT_NETWORK,MSGL_ERR,"streaming_start : Closing socket %d failed %s\n",stream->fd,strerror(errno));
		}
		stream->fd = -1;
		ret = rtp_streaming_start( stream );
	} else
#endif

	if( !strcasecmp( stream->streaming_ctrl->url->protocol, "pnm")) {
		stream->fd = -1;
		ret = pnm_streaming_start( stream );
	} else
	
	if( (!strcasecmp( stream->streaming_ctrl->url->protocol, "rtsp")) &&
			(*demuxer_type == DEMUXER_TYPE_REAL)) {
		stream->fd = -1;
		if ((ret = realrtsp_streaming_start( stream )) < 0) {
		    mp_msg(MSGT_NETWORK,MSGL_INFO,"Not a Realmedia rtsp url. Trying standard rtsp protocol.\n");
#ifdef STREAMING_LIVE_DOT_COM
		    *demuxer_type =  DEMUXER_TYPE_RTP;
		    goto try_livedotcom;
#else
		    mp_msg(MSGT_NETWORK,MSGL_ERR,"RTSP support requires the \"LIVE.COM Streaming Media\" libraries!\n");
		    return -1;
#endif
		}
	} else

	// For connection-oriented streams, we can usually determine the streaming type.
try_livedotcom:
	switch( *demuxer_type ) {
		case DEMUXER_TYPE_ASF:
			// Send the appropriate HTTP request
			// Need to filter the network stream.
			// ASF raw stream is encapsulated.
			// It can also be a playlist (redirector)
			// so we need to pass demuxer_type too
			ret = asf_streaming_start( stream, demuxer_type );
			if( ret<0 ) {
				mp_msg(MSGT_NETWORK,MSGL_ERR,"asf_streaming_start failed\n");
			}
			break;
#ifdef STREAMING_LIVE_DOT_COM
		case DEMUXER_TYPE_RTP:
			// RTSP/RTP streaming is handled separately:
			ret = rtsp_streaming_start( stream );
			if( ret<0 ) {
				mp_msg(MSGT_NETWORK,MSGL_ERR,"rtsp_streaming_start failed\n");
			}
			break;
#endif
		case DEMUXER_TYPE_MPEG_ES:
		case DEMUXER_TYPE_MPEG_PS:
		case DEMUXER_TYPE_AVI:
		case DEMUXER_TYPE_MOV:
		case DEMUXER_TYPE_VIVO:
		case DEMUXER_TYPE_FLI:
		case DEMUXER_TYPE_REAL:
		case DEMUXER_TYPE_Y4M:
		case DEMUXER_TYPE_FILM:
		case DEMUXER_TYPE_ROQ:
		case DEMUXER_TYPE_AUDIO:
		case DEMUXER_TYPE_OGG:
		case DEMUXER_TYPE_PLAYLIST:
		case DEMUXER_TYPE_UNKNOWN:
		case DEMUXER_TYPE_NSV: 
			// Generic start, doesn't need to filter
			// the network stream, it's a raw stream
			ret = nop_streaming_start( stream );
			if( ret<0 ) {
				mp_msg(MSGT_NETWORK,MSGL_ERR,"nop_streaming_start failed\n");
			}
			break;
		default:
			mp_msg(MSGT_NETWORK,MSGL_ERR,"Unable to detect the streaming type\n");
			ret = -1;
	}

	if( ret<0 ) {
		streaming_ctrl_free( stream->streaming_ctrl );
		stream->streaming_ctrl = NULL;
	} else if( stream->streaming_ctrl->buffering ) {
		if(stream_cache_size<0) {
			// cache option not set, will use our computed value.
			// buffer in KBytes, *5 because the prefill is 20% of the buffer.
			stream_cache_size = (stream->streaming_ctrl->prebuffer_size/1024)*5;
			if( stream_cache_size<64 ) stream_cache_size = 64;	// 16KBytes min buffer
		}
		mp_msg(MSGT_NETWORK,MSGL_INFO,"Cache size set to %d KBytes\n", stream_cache_size);
	}

	return ret;
}

int
streaming_stop( stream_t *stream ) {
	stream->streaming_ctrl->status = streaming_stopped_e;
	return 0;
}

/* 
 * URL Helper
 * by Bertrand Baudet <bertrand_baudet@yahoo.com>
 * (C) 2001, MPlayer team.
 */

#ifndef __URL_H
#define __URL_H

//#define __URL_DEBUG

typedef struct {
	char *url;
	char *protocol;
	char *hostname;
	char *file;
	unsigned int port;
	char *username;
	char *password;
} URL_t;

URL_t* url_new(char* url);
void   url_free(URL_t* url);

void url_unescape_string(char *outbuf, char *inbuf);
void url_escape_string(char *outbuf, char *inbuf);

#ifndef __URL_DEBUG
void url_debug(URL_t* url);
#endif // __URL_DEBUG

#endif // __URL_H

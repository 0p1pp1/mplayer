
//#define MSG_USE_COLORS

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "config.h"

#ifdef USE_LANGINFO
#include <locale.h>
#include <langinfo.h>
#endif
#ifdef USE_ICONV
#include <iconv.h>
#endif

#if	defined(FOR_MENCODER) || defined(CODECS2HTML)
#undef HAVE_NEW_GUI
#endif

#ifdef HAVE_NEW_GUI
#include "Gui/interface.h"
extern int use_gui;
#endif
#include "mp_msg.h"

/* maximum message length of mp_msg */
#define MSGSIZE_MAX 3072

int mp_msg_levels[MSGT_MAX]; // verbose level of this module. inited to -2
int mp_msg_level_all = MSGL_STATUS;
int verbose = 0;
#ifdef USE_ICONV
char *mp_msg_charset = NULL;
static char *old_charset = NULL;
static iconv_t msgiconv;
#endif

void mp_msg_init(void){
    int i;
    char *env = getenv("MPLAYER_VERBOSE");
    if (env)
        verbose = atoi(env);
#ifdef USE_I18N
#ifdef MP_DEBUG
    fprintf(stdout, "Using GNU internationalization\n");
    fprintf(stdout, "Original domain: %s\n", textdomain(NULL));
    fprintf(stdout, "Original dirname: %s\n", bindtextdomain(textdomain(NULL),NULL));
#endif
    bindtextdomain("mplayer", PREFIX"/share/locale");
    textdomain("mplayer");
#ifdef MP_DEBUG
    fprintf(stdout, "Current domain: %s\n", textdomain(NULL));
    fprintf(stdout, "Current dirname: %s\n\n", bindtextdomain(textdomain(NULL),NULL));
#endif
#endif
    for(i=0;i<MSGT_MAX;i++) mp_msg_levels[i] = -2;
#ifdef USE_ICONV
    mp_msg_charset = getenv("MPLAYER_CHARSET");
#ifdef USE_LANGINFO
    if (!mp_msg_charset) {
      setlocale(LC_CTYPE, "");
      mp_msg_charset = nl_langinfo(CODESET);
      setlocale(LC_CTYPE, "C");
    }
#endif
#endif
}

int mp_msg_test(int mod, int lev)
{
    return lev <= (mp_msg_levels[mod] == -2 ? mp_msg_level_all + verbose : mp_msg_levels[mod]);
}

void mp_msg(int mod, int lev, const char *format, ... ){
    va_list va;
    char tmp[MSGSIZE_MAX];
    
    if (!mp_msg_test(mod, lev)) return; // do not display
    va_start(va, format);
    vsnprintf(tmp, MSGSIZE_MAX, mp_gettext(format), va);
    va_end(va);
    tmp[MSGSIZE_MAX-2] = '\n';
    tmp[MSGSIZE_MAX-1] = 0;

#ifdef HAVE_NEW_GUI
    if(use_gui)
        guiMessageBox(lev, tmp);
#endif

#if defined(USE_ICONV) && defined(MSG_CHARSET)
    if (mp_msg_charset && strcasecmp(mp_msg_charset, "noconv")) {
      char tmp2[MSGSIZE_MAX];
      size_t inlen = strlen(tmp), outlen = MSGSIZE_MAX;
      char *in = tmp, *out = tmp2;
      if (!old_charset || strcmp(old_charset, mp_msg_charset)) {
        if (old_charset) {
          free(old_charset);
          iconv_close(msgiconv);
        }
        msgiconv = iconv_open(mp_msg_charset, MSG_CHARSET);
        old_charset = strdup(mp_msg_charset);
      }
      memset(tmp2, 0, MSGSIZE_MAX);
      while (iconv(msgiconv, &in, &inlen, &out, &outlen) == -1) {
        if (!inlen || !outlen)
          break;
        *out++ = *in++;
        outlen--; inlen--;
      }
      strncpy(tmp, tmp2, MSGSIZE_MAX);
      tmp[MSGSIZE_MAX-1] = 0;
      tmp[MSGSIZE_MAX-2] = '\n';
    }
#endif

#ifdef MSG_USE_COLORS
/* that's only a silly color test */
#ifdef MP_ANNOY_ME
    { int c;
      static int flag=1;
      if(flag)
      for(c=0;c<24;c++)
          printf("\033[%d;3%dm***  COLOR TEST %d  ***\n",(c>7),c&7,c);
      flag=0;
    }
#endif    
    {	unsigned char v_colors[10]={9,1,3,15,7,2,2,8,8,8};
        static const char *lev_text[]= {
                                "FATAL",
                                "ERROR",
                                "WARN",
                                "HINT",
                                "INFO",
                                "STATUS",
                                "V",
                                "DGB2",
                                "DGB3",
                                "DGB4"};
        static const char *mod_text[]= {
                                "GLOBAL",
                                "CPLAYER",
                                "GPLAYER",
                                "VIDEOOUT",
                                "AUDIOOUT",
                                "DEMUXER",
                                "DS",
                                "DEMUX",
                                "HEADER",
                                "AVSYNC",
                                "AUTOQ",
                                "CFGPARSER",
                                "DECAUDIO",
                                "DECVIDEO",
                                "SEEK",
                                "WIN32",
                                "OPEN",
                                "DVD",
                                "PARSEES",
                                "LIRC",
                                "STREAM",
                                "CACHE",
                                "MENCODER",
                                "XACODEC",
                                "TV",
                                "OSDEP",
                                "SPUDEC",
                                "PLAYTREE",
                                "INPUT",
                                "VFILTER",
                                "OSD",
                                "NETWORK",
                                "CPUDETECT",
                                "CODECCFG",
                                "SWS",
                                "VOBSUB",
                                "SUBREADER",
                                "AFILTER",
                                "NETST",
                                "MUXER"};

        int c=v_colors[lev];
        int c2=(mod+1)%15+1;
        static int header=1;
        FILE *stream= (lev) <= MSGL_WARN ? stderr : stdout;
        if(header){
            fprintf(stream, "\033[%d;3%dm%9s\033[0;37m: ",c2>>3,c2&7, mod_text[mod]);
        }
        fprintf(stream, "\033[%d;3%dm",c>>3,c&7);
        header=    tmp[strlen(tmp)-1] == '\n'
                 ||tmp[strlen(tmp)-1] == '\r';
    }
#endif
    if (lev <= MSGL_WARN){
	fprintf(stderr, "%s", tmp);fflush(stderr);
    } else {
	printf("%s", tmp);fflush(stdout);
    }
}

// Movie Player    v0.90   (C) 2000-2002. by A'rpi/ESP-team & `cat AUTHORS`

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/ioctl.h>
// #include <sys/mman.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/stat.h>

#include <signal.h>
#include <time.h>
#include <fcntl.h>

#include <errno.h>

#include "version.h"
#include "config.h"

#include "mp_msg.h"

#define HELP_MP_DEFINE_STATIC
#include "help_mp.h"

#include "cfgparser.h"
#include "cfg-mplayer-def.h"

#ifdef USE_SUB
#include "subreader.h"
#endif

#include "libvo/video_out.h"

#include "libvo/font_load.h"
#include "libvo/sub.h"

#include "libao2/audio_out.h"
#include "libao2/audio_plugin.h"

#include "codec-cfg.h"

#ifdef HAVE_LIBCSS
#include "libmpdemux/dvdauth.h"
#endif

#ifdef USE_DVDNAV
#include <dvdnav.h>
#endif

#include "spudec.h"
#include "vobsub.h"

#include "linux/getch2.h"
#include "linux/timer.h"

#include "cpudetect.h"

#ifdef HAVE_NEW_GUI
#include "Gui/interface.h"
#endif

#include "input/input.h"

int slave_mode=0;
int verbose=0;
static int quiet=0;

#define ABS(x) (((x)>=0)?(x):(-(x)))

#ifdef HAVE_RTC
#include <linux/rtc.h>
#endif

#ifdef USE_TV
#include "libmpdemux/tv.h"
extern int tv_param_on;
#endif

//**************************************************************************//
//             Playtree
//**************************************************************************//
#include "playtree.h"

play_tree_t* playtree;

#define PT_NEXT_ENTRY 1
#define PT_PREV_ENTRY -1
#define PT_NEXT_SRC 2
#define PT_PREV_SRC -2
#define PT_UP_NEXT 3
#define PT_UP_PREV -3

//**************************************************************************//
//             Config
//**************************************************************************//

m_config_t* mconfig;

//**************************************************************************//
//             Config file
//**************************************************************************//

static int cfg_inc_verbose(struct config *conf){ ++verbose; return 0;}

static int cfg_include(struct config *conf, char *filename){
	return m_config_parse_config_file(mconfig, filename);
}

#include "get_path.c"

//**************************************************************************//
//**************************************************************************//
//             Input media streaming & demultiplexer:
//**************************************************************************//

static int max_framesize=0;

#include "libmpdemux/stream.h"
#include "libmpdemux/demuxer.h"
#include "libmpdemux/stheader.h"
//#include "parse_es.h"

#include "libmpcodecs/dec_audio.h"
#include "libmpcodecs/dec_video.h"
//#include "libmpcodecs/vf.h"

//**************************************************************************//
//**************************************************************************//

// Common FIFO functions, and keyboard/event FIFO code
#include "fifo.c"
int use_stdin=0;
//**************************************************************************//

vo_functions_t *video_out=NULL;
ao_functions_t *audio_out=NULL;

// benchmark:
double video_time_usage=0;
double vout_time_usage=0;
static double audio_time_usage=0;
static int total_time_usage_start=0;
static int total_frame_cnt=0;
static int drop_frame_cnt=0; // total number of dropped frames
int benchmark=0;

// options:
       int auto_quality=0;
static int output_quality=0;

int use_gui=0;

int osd_level=1;
int osd_level_saved=-1;
int osd_visible=100;

// seek:
static char *seek_to_sec=NULL;
static off_t seek_to_byte=0;
static off_t step_sec=0;
static int loop_times=-1;

// may be changed by GUI:  (FIXME!)
float rel_seek_secs=0;
int abs_seek_pos=0;

// codecs:
char **audio_codec_list=NULL; // override audio codec
char **video_codec_list=NULL; // override video codec
char **audio_fm_list=NULL;    // override audio codec family 
char **video_fm_list=NULL;    // override video codec family 

// streaming:
int audio_id=-1;
int video_id=-1;
int dvdsub_id=-1;
int vobsub_id=-1;
static char* audio_lang=NULL;
static char* dvdsub_lang=NULL;
static char* spudec_ifo=NULL;
int vcd_track=0;
char* filename=NULL; //"MI2-Trailer.avi";

// cache2:
       int stream_cache_size=0;
#ifdef USE_STREAM_CACHE
extern int cache_fill_status;
#else
#define cache_fill_status 0
#endif

// dump:
static char *stream_dump_name="stream.dump";
       int stream_dump_type=0;

// A-V sync:
static float default_max_pts_correction=-1;//0.01f;
static float max_pts_correction=0;//default_max_pts_correction;
static float c_total=0;
       float audio_delay=0;

static int softsleep=0;

static float force_fps=0;
static int force_srate=0;
       int frame_dropping=0; // option  0=no drop  1= drop vo  2= drop decode
static int play_n_frames=-1;
static int play_n_frames_mf=-1;

// screen info:
char* video_driver=NULL; // OBSOLETE, FIXME gui
char* audio_driver=NULL; // OBSOLETE, FIXME gui
char** video_driver_list=NULL;
char** audio_driver_list=NULL;

extern char *vo_subdevice;
extern char *ao_subdevice;

// codec outfmt flags (defined in libmpcodecs/vd.c)
extern int vo_flags;

// sub:
char *font_name=NULL;
float font_factor=0.75;
char *sub_name=NULL;
float sub_delay=0;
float sub_fps=0;
int   sub_auto = 1;
char *vobsub_name=NULL;
/*DSP!!char *dsp=NULL;*/
int   subcc_enabled=0;
#ifdef USE_SUB
subtitle* subtitles=NULL;
float sub_last_pts = -303;
#endif

static stream_t* stream=NULL;
static demuxer_t *demuxer=NULL;

char* current_module=NULL; // for debugging

// also modified by Gui/mplayer/gtk/eq.c:
int vo_gamma_gamma = 1000;
int vo_gamma_brightness = 1000;
int vo_gamma_contrast = 1000;
int vo_gamma_saturation = 1000;
int vo_gamma_hue = 1000;

// ---

#ifdef HAVE_RTC
static int nortc;
#endif

static unsigned int inited_flags=0;
#define INITED_VO 1
#define INITED_AO 2
#define INITED_GUI 4
#define INITED_GETCH2 8
#define INITED_LIRC 16
#define INITED_SPUDEC 32
#define INITED_STREAM 64
#define INITED_INPUT    128
#define INITED_VOBSUB  256
#define INITED_DEMUXER  512
#define INITED_ALL 0xFFFF

static void uninit_player(unsigned int mask){
  mask=inited_flags&mask;

  mp_msg(MSGT_CPLAYER,MSGL_DBG2,"\n*** uninit(0x%X)\n",mask);

  // kill the cache process:
  if(mask&INITED_STREAM){
    inited_flags&=~INITED_STREAM;
    current_module="uninit_stream";
    if(stream) free_stream(stream);
    stream=NULL;
  }

  if(mask&INITED_DEMUXER){
    current_module="uninit_demuxer";
    if(demuxer) free_demuxer(demuxer);
    demuxer=NULL;
  }

  if(mask&INITED_VO){
    inited_flags&=~INITED_VO;
    current_module="uninit_vo";
    video_out->uninit();
    video_out=NULL;
  }

  // must be after libvo uninit, as few vo drivers (svgalib) has tty code
  if(mask&INITED_GETCH2){
    inited_flags&=~INITED_GETCH2;
    current_module="uninit_getch2";
    mp_msg(MSGT_CPLAYER,MSGL_DBG2,"\n[[[uninit getch2]]]\n");
  // restore terminal:
    getch2_disable();
  }

  if(mask&INITED_VOBSUB){
    inited_flags&=~INITED_VOBSUB;
    current_module="uninit_vobsub";
    vobsub_close(vo_vobsub);
    vo_vobsub=NULL;
  }

  if (mask&INITED_SPUDEC){
    inited_flags&=~INITED_SPUDEC;
    current_module="uninit_spudec";
    spudec_free(vo_spudec);
    vo_spudec=NULL;
  }

  if(mask&INITED_AO){
    inited_flags&=~INITED_AO;
    current_module="uninit_ao";
    audio_out->uninit(); audio_out=NULL;
  }

#ifdef HAVE_NEW_GUI
  if(mask&INITED_GUI){
    inited_flags&=~INITED_GUI;
    current_module="uninit_gui";
    guiDone();
  }
#endif

  if(mask&INITED_INPUT){
    inited_flags&=~INITED_INPUT;
    current_module="uninit_input";
    mp_input_uninit();
  }

  current_module=NULL;
}

void exit_player(char* how){

  uninit_player(INITED_ALL);
#ifdef X11_FULLSCREEN
#ifdef HAVE_NEW_GUI
  if ( !use_gui )
#endif
  vo_uninit();	// close the X11 connection (if any opened)
#endif

  current_module="exit_player";

  if(how) mp_msg(MSGT_CPLAYER,MSGL_INFO,MSGTR_Exiting,mp_gettext(how));
  mp_msg(MSGT_CPLAYER,MSGL_DBG2,"max framesize was %d bytes\n",max_framesize);

  exit(1);
}

static void exit_sighandler(int x){
  static int sig_count=0;
  ++sig_count;
  if(sig_count==5 || (inited_flags==0 && sig_count>1)) exit(1);
  if(sig_count>5){
    // can't stop :(
    kill(getpid(),SIGKILL);
  }
  mp_msg(MSGT_CPLAYER,MSGL_FATAL,"\n" MSGTR_IntBySignal,x,
      current_module?current_module:mp_gettext("unknown")
  );
  if(sig_count==1)
  switch(x){
  case SIGINT:
  case SIGQUIT:
  case SIGTERM:
  case SIGKILL:
      break;  // killed from keyboard (^C) or killed [-9]
  case SIGILL:
#ifdef RUNTIME_CPUDETECT
      mp_msg(MSGT_CPLAYER,MSGL_FATAL,"- MPlayer crashed by 'Illegal Instruction'. It may be a bug in our new runtime cpu-detection code... please read DOCS/bugreports.html\n");
#else
      mp_msg(MSGT_CPLAYER,MSGL_FATAL,"- MPlayer crashed by 'Illegal Instruction'. It usually happens when you run it on different CPU than it was compiled/optimized for. Verify this!\n");
#endif
  case SIGFPE:
  case SIGSEGV:
      mp_msg(MSGT_CPLAYER,MSGL_FATAL,"- MPlayer crashed by bad usage of CPU/FPU/RAM. Recompile MPlayer with --enable-debug and make a 'gdb' backtrace and disassembly. For details, see DOCS/bugreports.html section 5.b.\n");
  default:
      mp_msg(MSGT_CPLAYER,MSGL_FATAL,"- MPlayer crashed. This shouldn't happen. It can be a bug in the MPlayer code _or_ in your drivers _or_ in your gcc version. If you think it's MPlayer's fault, please read DOCS/bugreports.html and follow instructions there. We can't and won't help unless you provide these informations when reporting a possible bug.\n");
  }
  exit_player(NULL);
}

//extern void write_avi_header_1(FILE *f,int fcc,float fps,int width,int height);

extern void mp_register_options(m_config_t* cfg);

#include "mixer.h"
#include "cfg-mplayer.h"

void parse_cfgfiles( m_config_t* conf )
{
char *conffile;
int conffile_fd;
if (m_config_parse_config_file(conf, CONFDIR"/mplayer.conf") < 0)
  exit(1);
if ((conffile = get_path("")) == NULL) {
  mp_msg(MSGT_CPLAYER,MSGL_WARN,MSGTR_NoHomeDir);
} else {
  mkdir(conffile, 0777);
  free(conffile);
  if ((conffile = get_path("config")) == NULL) {
    mp_msg(MSGT_CPLAYER,MSGL_ERR,MSGTR_GetpathProblem);
  } else {
    if ((conffile_fd = open(conffile, O_CREAT | O_EXCL | O_WRONLY, 0666)) != -1) {
      mp_msg(MSGT_CPLAYER,MSGL_INFO,MSGTR_CreatingCfgFile, conffile);
      write(conffile_fd, default_config, strlen(default_config));
      close(conffile_fd);
    }
    if (m_config_parse_config_file(conf, conffile) < 0)
      exit(1);
    free(conffile);
  }
}
}

// When libmpdemux perform a blocking operation (network connection or cache filling)
// if the operation fail we use this function to check if it was interrupted by the user.
// The function return a new value for eof.
static int libmpdemux_was_interrupted(int eof) {
  mp_cmd_t* cmd;
  if((cmd = mp_input_get_cmd(0,0)) != NULL) {
       switch(cmd->id) {
       case MP_CMD_QUIT:
	 exit_player(MSGTR_Exit_quit);
       case MP_CMD_PLAY_TREE_STEP: {
	 eof = (cmd->args[0].v.i > 0) ? PT_NEXT_ENTRY : PT_PREV_ENTRY;
       } break;
       case MP_CMD_PLAY_TREE_UP_STEP: {
	 eof = (cmd->args[0].v.i > 0) ? PT_UP_NEXT : PT_UP_PREV;
       } break;	  
       case MP_CMD_PLAY_ALT_SRC_STEP: {
	 eof = (cmd->args[0].v.i > 0) ?  PT_NEXT_SRC : PT_PREV_SRC;
       } break;
       }
       mp_cmd_free(cmd);
  }
  return eof;
}

/*
 * In Mac OS X the SDL-lib is built upon Cocoa. The easiest way to
 * make it all work is to use the builtin SDL-bootstrap code, which 
 * will be done automatically by replacing our main() if we include SDL.h.
 */
#if defined(SYS_DARWIN) && defined(HAVE_SDL)
#include <SDL.h>
#endif

int main(int argc,char* argv[]){



static demux_stream_t *d_audio=NULL;
static demux_stream_t *d_video=NULL;
static demux_stream_t *d_dvdsub=NULL;

static sh_audio_t *sh_audio=NULL;
static sh_video_t *sh_video=NULL;


// for multifile support:
play_tree_iter_t* playtree_iter = NULL;

int file_format=DEMUXER_TYPE_UNKNOWN;

int delay_corrected=1;

// movie info:
int eof=0;

int osd_function=OSD_PLAY;
int osd_last_pts=-303;
int osd_show_av_delay = 0;
int osd_show_sub_delay = 0;

int rtc_fd=-1;

//float a_frame=0;    // Audio

int i;

int gui_no_filename=0;

  mp_msg_init();
  mp_msg_set_level(MSGL_STATUS);

  mp_msg(MSGT_CPLAYER,MSGL_INFO,banner_text);
  /* Test for cpu capabilities (and corresponding OS support) for optimizing */
#ifdef ARCH_X86
  GetCpuCaps(&gCpuCaps);
  mp_msg(MSGT_CPLAYER,MSGL_INFO,"CPUflags:  MMX: %d MMX2: %d 3DNow: %d 3DNow2: %d SSE: %d SSE2: %d\n",
      gCpuCaps.hasMMX,gCpuCaps.hasMMX2,
      gCpuCaps.has3DNow, gCpuCaps.has3DNowExt,
      gCpuCaps.hasSSE, gCpuCaps.hasSSE2);
#ifdef RUNTIME_CPUDETECT
  mp_msg(MSGT_CPLAYER,MSGL_INFO, MSGTR_CompiledWithRuntimeDetection);
#else
  mp_msg(MSGT_CPLAYER,MSGL_INFO, MSGTR_CompiledWithCPUExtensions);
#ifdef HAVE_MMX
  mp_msg(MSGT_CPLAYER,MSGL_INFO," MMX");
#endif
#ifdef HAVE_MMX2
  mp_msg(MSGT_CPLAYER,MSGL_INFO," MMX2");
#endif
#ifdef HAVE_3DNOW
  mp_msg(MSGT_CPLAYER,MSGL_INFO," 3DNow");
#endif
#ifdef HAVE_3DNOWEX
  mp_msg(MSGT_CPLAYER,MSGL_INFO," 3DNowEx");
#endif
#ifdef HAVE_SSE
  mp_msg(MSGT_CPLAYER,MSGL_INFO," SSE");
#endif
#ifdef HAVE_SSE2
  mp_msg(MSGT_CPLAYER,MSGL_INFO," SSE2");
#endif
  mp_msg(MSGT_CPLAYER,MSGL_INFO,"\n\n");
#endif

#endif

#ifdef USE_TV
  tv_param_immediate = 1;
#endif

  if ( argv[0] )
    if(!strcmp(argv[0],"gmplayer") ||
      (strrchr(argv[0],'/') && !strcmp(strrchr(argv[0],'/'),"/gmplayer") ) )
          use_gui=1;

    playtree = play_tree_new();

    mconfig = m_config_new(playtree);
    m_config_register_options(mconfig,mplayer_opts);
    // TODO : add something to let modules register their options
    mp_register_options(mconfig);
    parse_cfgfiles(mconfig);

#ifdef HAVE_NEW_GUI
    if ( use_gui ) cfg_read();
#endif

    if(m_config_parse_command_line(mconfig, argc, argv) < 0) exit(1); // error parsing cmdline

    playtree = play_tree_cleanup(playtree);
    if(playtree) {
      playtree_iter = play_tree_iter_new(playtree,mconfig);
      if(playtree_iter) {  
	if(play_tree_iter_step(playtree_iter,0,0) != PLAY_TREE_ITER_ENTRY) {
	  play_tree_iter_free(playtree_iter);
	  playtree_iter = NULL;
	}
	filename = play_tree_iter_get_file(playtree_iter,1);
      }
    }
	
#ifndef HAVE_NEW_GUI
    if(use_gui){
      mp_msg(MSGT_CPLAYER,MSGL_WARN,MSGTR_NoGui);
      use_gui=0;
    }
#else
    if(use_gui && !vo_init()){
      mp_msg(MSGT_CPLAYER,MSGL_WARN,MSGTR_GuiNeedsX);
      use_gui=0;
    }
#endif

    if(vo_plugin_args && vo_plugin_args[0] && strcmp(vo_plugin_args[0],"help")==0){
      mp_msg(MSGT_CPLAYER, MSGL_INFO, MSGTR_AvailableVideoOutputPlugins);
      vf_list_plugins();
      printf("\n");
      exit(0);
    }

    if(video_driver_list && strcmp(video_driver_list[0],"help")==0){
      list_video_out();
      exit(0);
    }

    if(audio_driver_list && strcmp(audio_driver_list[0],"help")==0){
      list_audio_out();
      exit(0);
    }

// check codec.conf
if(!parse_codec_cfg(get_path("codecs.conf"))){
  if(!parse_codec_cfg(CONFDIR"/codecs.conf")){
    mp_msg(MSGT_CPLAYER,MSGL_HINT,MSGTR_CopyCodecsConf);
    exit(0);  // From unknown reason a hangup occurs here :((((((
  }
}

#if 0
    if(video_codec_list){
	int i;
	video_codec=video_codec_list[0];
	for(i=0;video_codec_list[i];i++)
	    printf("vc#%d: '%s'\n",i,video_codec_list[i]);
    }
#endif
    if(audio_codec_list && strcmp(audio_codec_list[0],"help")==0){
      mp_msg(MSGT_CPLAYER, MSGL_INFO, MSGTR_AvailableAudioCodecs);
      list_codecs(1);
      printf("\n");
      exit(0);
    }
    if(video_codec_list && strcmp(video_codec_list[0],"help")==0){
      mp_msg(MSGT_CPLAYER, MSGL_INFO, MSGTR_AvailableVideoCodecs);
      list_codecs(0);
      printf("\n");
      exit(0);
    }
    if(video_fm_list && strcmp(video_fm_list[0],"help")==0){
      vfm_help();
      printf("\n");
      exit(0);
    }
    if(audio_fm_list && strcmp(audio_fm_list[0],"help")==0){
      afm_help();
      printf("\n");
      exit(0);
    }


    if(!filename && !vcd_track && !dvd_title && !dvd_nav && !tv_param_on){
      if(!use_gui){
	// no file/vcd/dvd -> show HELP:
	mp_msg(MSGT_CPLAYER, MSGL_INFO, help_text);
	exit(0);
      } else gui_no_filename=1;
    }

    // Many users forget to include command line in bugreports...
    if(verbose){
      mp_msg(MSGT_CPLAYER, MSGL_INFO, "CommandLine:");
      for(i=1;i<argc;i++)printf(" '%s'",argv[i]);
      printf("\n");
    }

    mp_msg_set_level(verbose+MSGL_STATUS);

//------ load global data first ------

#ifdef USE_OSD
#ifndef HAVE_FREETYPE
// check font
  if(font_name){
       vo_font=read_font_desc(font_name,font_factor,verbose>1);
       if(!vo_font) mp_msg(MSGT_CPLAYER,MSGL_ERR,MSGTR_CantLoadFont,font_name);
  } else {
      // try default:
       vo_font=read_font_desc(get_path("font/font.desc"),font_factor,verbose>1);
       if(!vo_font)
       vo_font=read_font_desc(DATADIR"/font/font.desc",font_factor,verbose>1);
  }
#else
  init_freetype();
#endif
#endif
  vo_init_osd();

#ifdef HAVE_RTC
  if(!nortc)
  {
    // seteuid(0); /* Can't hurt to try to get root here */
    if ((rtc_fd = open("/dev/rtc", O_RDONLY)) < 0)
	mp_msg(MSGT_CPLAYER, MSGL_WARN, "Failed to open /dev/rtc: %s (mplayer should be setuid root or /dev/rtc should be readable by the user.)\n", strerror(errno));
     else {
	unsigned long irqp = 1024; /* 512 seemed OK. 128 is jerky. */

	if (ioctl(rtc_fd, RTC_IRQP_SET, irqp) < 0) {
    	    mp_msg(MSGT_CPLAYER, MSGL_WARN, "Linux RTC init error in ioctl (rtc_irqp_set %lu): %s\n", irqp, strerror(errno));
	    mp_msg(MSGT_CPLAYER, MSGL_HINT, "Try adding \"echo %lu > /proc/sys/dev/rtc/max-user-freq\" to your system startup scripts.\n", irqp);
   	    close (rtc_fd);
    	    rtc_fd = -1;
	} else if (ioctl(rtc_fd, RTC_PIE_ON, 0) < 0) {
	    /* variable only by the root */
    	    mp_msg(MSGT_CPLAYER, MSGL_ERR, "Linux RTC init error in ioctl (rtc_pie_on): %s\n", strerror(errno));
    	    close (rtc_fd);
	    rtc_fd = -1;
	} else
	    mp_msg(MSGT_CPLAYER, MSGL_INFO, MSGTR_UsingRTCTiming, irqp);
    }
  }
#ifdef HAVE_NEW_GUI
// breaks DGA and SVGAlib and VESA drivers:  --A'rpi
// and now ? -- Pontscho
    if(use_gui) setuid( getuid() ); // strongly test, please check this.
#endif
    if(rtc_fd<0)
#endif
    mp_msg(MSGT_CPLAYER, MSGL_INFO, "Using %s timing\n",softsleep?"software":"usleep()");

#ifdef USE_TERMCAP
  if ( !use_gui ) load_termcap(NULL); // load key-codes
#endif

// ========== Init keyboard FIFO (connection to libvo) ============
make_pipe(&keyb_fifo_get,&keyb_fifo_put);

// Init input system
current_module = "init_input";
mp_input_init();
if(keyb_fifo_get > 0)
  mp_input_add_key_fd(keyb_fifo_get,1,NULL,NULL);
if(slave_mode)
   mp_input_add_cmd_fd(0,1,NULL,NULL);
else if(!use_stdin)
  mp_input_add_key_fd(0,1,NULL,NULL);
inited_flags|=INITED_INPUT;
current_module = NULL;


  //========= Catch terminate signals: ================
  // terminate requests:
  signal(SIGTERM,exit_sighandler); // kill
  signal(SIGHUP,exit_sighandler);  // kill -HUP  /  xterm closed

  signal(SIGINT,exit_sighandler);  // Interrupt from keyboard

  signal(SIGQUIT,exit_sighandler); // Quit from keyboard
#ifdef ENABLE_SIGHANDLER
  // fatal errors:
  signal(SIGBUS,exit_sighandler);  // bus error
  signal(SIGSEGV,exit_sighandler); // segfault
  signal(SIGILL,exit_sighandler);  // illegal instruction
  signal(SIGFPE,exit_sighandler);  // floating point exc.
  signal(SIGABRT,exit_sighandler); // abort()
#endif

#ifdef HAVE_NEW_GUI
  if(use_gui){
       guiInit();
       inited_flags|=INITED_GUI;
       guiGetEvent( guiCEvent,(char *)((gui_no_filename) ? 0 : 1) );
  }
#endif

// ******************* Now, let's see the per-file stuff ********************

play_next_file:

// We must enable getch2 here to be able to interrupt network connection
// or cache filling
if(!use_stdin && !slave_mode){
  if(inited_flags&INITED_GETCH2)
    mp_msg(MSGT_CPLAYER,MSGL_WARN,"WARNING: getch2_init called twice!\n");
  else
    getch2_enable();  // prepare stdin for hotkeys...
  inited_flags|=INITED_GETCH2;
  mp_msg(MSGT_CPLAYER,MSGL_DBG2,"\n[[[init getch2]]]\n");
}

// =================== GUI idle loop (STOP state) ===========================
#ifdef HAVE_NEW_GUI
    if ( use_gui ) {

      guiGetEvent( guiReDrawSubWindow,0 );
      while ( guiIntfStruct.Playing != 1 )
       {
        mp_cmd_t* cmd;                                                                                   
	usleep(20000);
	guiEventHandling();
	guiGetEvent( guiReDraw,NULL );
	if ( (cmd = mp_input_get_cmd(0,0)) != NULL) guiGetEvent( guiIEvent,(char *)cmd->id );
       } 

      guiGetEvent( guiSetDefaults,NULL );

      if ( ( guiIntfStruct.FilenameChanged || !filename )
//#ifdef USE_DVDREAD
//           && ( guiIntfStruct.StreamType != STREAMTYPE_DVD )
//#endif
       )      
       {
        play_tree_t * entry = play_tree_new();
        play_tree_add_file( entry,guiIntfStruct.Filename );
        if ( playtree ) play_tree_free_list( playtree->child,1 );
         else playtree=play_tree_new();
        play_tree_set_child( playtree,entry );
        if(playtree)
	 {
	  playtree_iter = play_tree_iter_new(playtree,mconfig);
	  if(playtree_iter)
	   {
	    if(play_tree_iter_step(playtree_iter,0,0) != PLAY_TREE_ITER_ENTRY)
	     {
	      play_tree_iter_free(playtree_iter);
	      playtree_iter = NULL;
	     }
	    filename = play_tree_iter_get_file(playtree_iter,1);
	   }
         }
   	guiIntfStruct.FilenameChanged=0;
       } 
    }
#endif
//---------------------------------------------------------------------------

    mp_msg(MSGT_CPLAYER,MSGL_INFO,"\n");
    if(filename) mp_msg(MSGT_CPLAYER,MSGL_INFO,MSGTR_Playing, filename);

//==================== Open VOB-Sub ============================

    current_module="vobsub";
    if (vobsub_name){
      vo_vobsub=vobsub_open(vobsub_name,spudec_ifo,1,&vo_spudec);
      if(vo_vobsub==NULL)
        mp_msg(MSGT_CPLAYER,MSGL_ERR,MSGTR_CantLoadSub,vobsub_name);
    }else if(sub_auto && filename && (strlen(filename)>=5)){
      /* try to autodetect vobsub from movie filename ::atmos */
      char *buf = malloc((strlen(filename)-3) * sizeof(char));
      memset(buf,0,strlen(filename)-3); // make sure string is terminated
      strncpy(buf, filename, strlen(filename)-4); 
      vo_vobsub=vobsub_open(buf,spudec_ifo,0,&vo_spudec);
      free(buf);
    }
    if(vo_vobsub)
      sub_auto=0; // don't do autosub for textsubs if vobsub found

//============ Open & Sync STREAM --- fork cache2 ====================

  stream=NULL;
  demuxer=NULL;
  d_audio=NULL;
  d_video=NULL;
  sh_audio=NULL;
  sh_video=NULL;

  current_module="open_stream";
  stream=open_stream(filename,vcd_track,&file_format);
  if(!stream) { // error...
    eof = libmpdemux_was_interrupted(PT_NEXT_ENTRY);
    goto goto_next_file;
  }
  inited_flags|=INITED_STREAM;

  if(stream->type == STREAMTYPE_PLAYLIST) {
    play_tree_t* entry;
    // Handle playlist
    current_module="handle_playlist";
    mp_msg(MSGT_CPLAYER,MSGL_V,"Parsing playlist %s...\n",filename);
    entry = parse_playtree(stream);
    if(!entry) {      
      entry = playtree_iter->tree;
      if(play_tree_iter_step(playtree_iter,1,0) != PLAY_TREE_ITER_ENTRY) {
	eof = PT_NEXT_ENTRY;
	goto goto_next_file;
      }
      if(playtree_iter->tree == entry ) { // Loop with a single file
	if(play_tree_iter_up_step(playtree_iter,1,0) != PLAY_TREE_ITER_ENTRY) {
	  eof = PT_NEXT_ENTRY;
	  goto goto_next_file;
	}
      }
      play_tree_remove(entry,1,1);
      eof = PT_NEXT_SRC;
      goto goto_next_file;
    }
    play_tree_insert_entry(playtree_iter->tree,entry);
    play_tree_set_params_from(entry,playtree_iter->tree);
    entry = playtree_iter->tree;
    if(play_tree_iter_step(playtree_iter,1,0) != PLAY_TREE_ITER_ENTRY) {
      eof = PT_NEXT_ENTRY;
      goto goto_next_file;
    }      
    play_tree_remove(entry,1,1);
    eof = PT_NEXT_SRC;
    goto goto_next_file;
  }
  stream->start_pos+=seek_to_byte;

#ifdef HAVE_LIBCSS
  current_module="libcss";
  if (dvdimportkey) {
    if (dvd_import_key(dvdimportkey)) {
	mp_msg(MSGT_CPLAYER,MSGL_FATAL,MSGTR_ErrorDVDkey);
	exit_player(MSGTR_Exit_error);
    }
    mp_msg(MSGT_CPLAYER,MSGL_INFO,MSGTR_CmdlineDVDkey);
  }
  if (dvd_auth_device) {
    if (dvd_auth(dvd_auth_device,filename)) {
	mp_msg(MSGT_CPLAYER,MSGL_FATAL,"Error in DVD auth...\n");
	exit_player(MSGTR_Exit_error);
      } 
    mp_msg(MSGT_CPLAYER,MSGL_INFO,MSGTR_DVDauthOk);
  }
#endif

if(stream_dump_type==5){
  unsigned char buf[4096];
  int len;
  FILE *f;
  current_module="dumpstream";
  if(stream->type==STREAMTYPE_STREAM && stream->fd<0){
    mp_msg(MSGT_CPLAYER,MSGL_FATAL,"Cannot dump this stream - no 'fd' available\n");
    exit_player(MSGTR_Exit_error);
  }
  stream_reset(stream);
  stream_seek(stream,stream->start_pos);
  f=fopen(stream_dump_name,"wb");
  if(!f){
    mp_msg(MSGT_CPLAYER,MSGL_FATAL,MSGTR_CantOpenDumpfile);
    exit_player(MSGTR_Exit_error);
  }
  while(!stream->eof){
      len=stream_read(stream,buf,4096);
      if(len>0) fwrite(buf,len,1,f);
  }
  fclose(f);
  mp_msg(MSGT_CPLAYER,MSGL_INFO,MSGTR_CoreDumped);
  exit_player(MSGTR_Exit_eof);
}

#ifdef USE_DVDREAD
if(stream->type==STREAMTYPE_DVD){
  current_module="dvd lang->id";
  if(audio_lang && audio_id==-1) audio_id=dvd_aid_from_lang(stream,audio_lang);
  if(dvdsub_lang && dvdsub_id==-1) dvdsub_id=dvd_sid_from_lang(stream,dvdsub_lang);
  current_module=NULL;
}
#endif

#ifdef USE_DVDNAV
  if (dvd_nav) stream_cache_size=0;	// must disable caching...
#endif

// CACHE2: initial prefill: 20%  later: 5%  (should be set by -cacheopts)
if(stream_cache_size){
  current_module="enable_cache";
  if(!stream_enable_cache(stream,stream_cache_size*1024,stream_cache_size*1024/5,stream_cache_size*1024/20))
    if((eof = libmpdemux_was_interrupted(PT_NEXT_ENTRY))) goto goto_next_file;
}

//============ Open DEMUXERS --- DETECT file type =======================

current_module="demux_open";

demuxer=demux_open(stream,file_format,audio_id,video_id,dvdsub_id);
if(!demuxer) goto goto_next_file; // exit_player(MSGTR_Exit_error); // ERROR

inited_flags|=INITED_DEMUXER;

current_module="demux_open2";

//file_format=demuxer->file_format;

d_audio=demuxer->audio;
d_video=demuxer->video;
d_dvdsub=demuxer->sub;

// DUMP STREAMS:
if((stream_dump_type)&&(stream_dump_type<4)){
  FILE *f;
  demux_stream_t *ds=NULL;
  current_module="dump";
  // select stream to dump
  switch(stream_dump_type){
  case 1: ds=d_audio;break;
  case 2: ds=d_video;break;
  case 3: ds=d_dvdsub;break;
  }
  if(!ds){        
      mp_msg(MSGT_CPLAYER,MSGL_FATAL,MSGTR_DumpSelectedSteramMissing);
      exit_player(MSGTR_Exit_error);
  }
  // disable other streams:
  if(d_audio && d_audio!=ds) {ds_free_packs(d_audio); d_audio->id=-2; }
  if(d_video && d_video!=ds) {ds_free_packs(d_video); d_video->id=-2; }
  if(d_dvdsub && d_dvdsub!=ds) {ds_free_packs(d_dvdsub); d_dvdsub->id=-2; }
  // let's dump it!
  f=fopen(stream_dump_name,"wb");
  if(!f){
    mp_msg(MSGT_CPLAYER,MSGL_FATAL,MSGTR_CantOpenDumpfile);
    exit_player(MSGTR_Exit_error);
  }
  while(!ds->eof){
    unsigned char* start;
    int in_size=ds_get_packet(ds,&start);
    if( (demuxer->file_format==DEMUXER_TYPE_AVI || demuxer->file_format==DEMUXER_TYPE_ASF || demuxer->file_format==DEMUXER_TYPE_MOV)
	&& stream_dump_type==2) fwrite(&in_size,1,4,f);
    if(in_size>0) fwrite(start,in_size,1,f);
  }
  fclose(f);
  mp_msg(MSGT_CPLAYER,MSGL_INFO,MSGTR_CoreDumped);
  exit_player(MSGTR_Exit_eof);
}

sh_audio=d_audio->sh;
sh_video=d_video->sh;

if(sh_video){

  current_module="video_read_properties";
  if(!video_read_properties(sh_video)) {
    mp_msg(MSGT_CPLAYER,MSGL_ERR,MSGTR_CannotReadVideoPropertiers);
    sh_video=d_video->sh=NULL;
  } else {
    mp_msg(MSGT_CPLAYER,MSGL_V,"[V] filefmt:%d  fourcc:0x%X  size:%dx%d  fps:%5.2f  ftime:=%6.4f\n",
	   demuxer->file_format,sh_video->format, sh_video->disp_w,sh_video->disp_h,
	   sh_video->fps,sh_video->frametime
	   );

    /* need to set fps here for output encoders to pick it up in their init */
    if(force_fps){
      sh_video->fps=force_fps;
      sh_video->frametime=1.0f/sh_video->fps;
    }
    vo_fps = sh_video->fps;
#ifdef X11_FULLSCREEN
    vo_mouse_timer_const=(int)sh_video->fps;
#endif

    if(!sh_video->fps && !force_fps){
      mp_msg(MSGT_CPLAYER,MSGL_ERR,MSGTR_FPSnotspecified);
      sh_video=d_video->sh=NULL;
    }
  }

}

fflush(stdout);

if(!sh_video && !sh_audio){
    mp_msg(MSGT_CPLAYER,MSGL_FATAL, MSGTR_NoStreamFound);
    goto goto_next_file; // exit_player(MSGTR_Exit_error);
}

/* display clip info */
demux_info_print(demuxer);

//================== Read SUBTITLES (DVD & TEXT) ==========================
if(d_dvdsub->id >= 0 && vo_spudec==NULL && sh_video){

if (spudec_ifo) {
  unsigned int palette[16], width, height;
  current_module="spudec_init_vobsub";
  if (vobsub_parse_ifo(NULL,spudec_ifo, palette, &width, &height, 1, -1, NULL) >= 0)
    vo_spudec=spudec_new_scaled(palette, sh_video->disp_w, sh_video->disp_h);
}

#ifdef USE_DVDNAV
if (vo_spudec==NULL && stream->type==STREAMTYPE_DVDNAV) {
  current_module="spudec_init_dvdnav";
  vo_spudec=spudec_new_scaled(dvdnav_stream_get_palette((dvdnav_priv_t*)(stream->priv)),
			    sh_video->disp_w, sh_video->disp_h);
}
#endif

#ifdef USE_DVDREAD
if (vo_spudec==NULL && stream->type==STREAMTYPE_DVD) {
  current_module="spudec_init_dvdread";
  vo_spudec=spudec_new_scaled(((dvd_priv_t *)(stream->priv))->cur_pgc->palette,
			    sh_video->disp_w, sh_video->disp_h);
}
#endif

if (vo_spudec==NULL) {
  current_module="spudec_init_normal";
  vo_spudec=spudec_new_scaled(NULL, sh_video->disp_w, sh_video->disp_h);
  spudec_set_font_factor(vo_spudec,font_factor);
}

if (vo_spudec!=NULL)
  inited_flags|=INITED_SPUDEC;

}

#ifdef USE_SUB
if(sh_video) {
// after reading video params we should load subtitles because
// we know fps so now we can adjust subtitles time to ~6 seconds AST
// check .sub
  current_module="read_subtitles_file";
  if(sub_name){
    subtitles=sub_read_file(sub_name, sh_video->fps);
    if(!subtitles) mp_msg(MSGT_CPLAYER,MSGL_ERR,MSGTR_CantLoadSub,sub_name);
  } else
  if(sub_auto) { // auto load sub file ...
    subtitles=sub_read_file( filename ? sub_filename( get_path("sub/"), filename )
                                     : "default.sub", sh_video->fps );
  }
  if(subtitles && stream_dump_type==3) list_sub_file(subtitles);
  if(subtitles && stream_dump_type==4) dump_mpsub(subtitles, sh_video->fps);
  if(subtitles && stream_dump_type==6) dump_srt(subtitles, sh_video->fps);
  if(subtitles && stream_dump_type==7) dump_microdvd(subtitles, sh_video->fps);
}
#endif

//================== Init AUDIO (codec) ==========================
if(sh_audio){
  // Go through the codec.conf and find the best codec...
  current_module="init_audio_codec";
  mp_msg(MSGT_CPLAYER,MSGL_INFO,"==========================================================================\n");
  if(!init_best_audio_codec(sh_audio,audio_codec_list,audio_fm_list)){
    sh_audio=d_audio->sh=NULL; // failed to init :(
  }
  mp_msg(MSGT_CPLAYER,MSGL_INFO,"==========================================================================\n");
}

if(!sh_video) goto main; // audio-only

//================== Init VIDEO (codec & libvo) ==========================
current_module="preinit_libvo";

vo_config_count=0;
//if((video_out->preinit(vo_subdevice))!=0){
if(!(video_out=init_best_video_out(video_driver_list))){
    mp_msg(MSGT_CPLAYER,MSGL_FATAL,MSGTR_ErrorInitializingVODevice);
    goto goto_next_file; // exit_player(MSGTR_Exit_error);
}
sh_video->video_out=video_out;
inited_flags|=INITED_VO;

current_module="init_video_filters";

sh_video->vfilter=(void*)vf_open_filter(NULL,"vo",video_out);
sh_video->vfilter=(void*)append_filters(sh_video->vfilter);

current_module="init_video_codec";

mp_msg(MSGT_CPLAYER,MSGL_INFO,"==========================================================================\n");
init_best_video_codec(sh_video,video_codec_list,video_fm_list);
mp_msg(MSGT_CPLAYER,MSGL_INFO,"==========================================================================\n");

if(!sh_video->inited){
    if(!sh_audio) goto goto_next_file;
    sh_video = d_video->sh = NULL;
    goto main; // exit_player(MSGTR_Exit_error);
}

if(auto_quality>0){
    // Auto quality option enabled
    output_quality=get_video_quality_max(sh_video);
    if(auto_quality>output_quality) auto_quality=output_quality;
    else output_quality=auto_quality;
    mp_msg(MSGT_CPLAYER,MSGL_V,"AutoQ: setting quality to %d\n",output_quality);
    set_video_quality(sh_video,output_quality);
}

// ========== Init display (sh_video->disp_w*sh_video->disp_h/out_fmt) ============

current_module="init_vo";
    if (sh_video)
    {
        if (vo_gamma_gamma != 1000)
          set_video_colors (sh_video, "gamma", vo_gamma_gamma);
	if (vo_gamma_brightness != 1000)
	    set_video_colors(sh_video, "brightness", vo_gamma_brightness);
	if (vo_gamma_contrast != 1000)
	    set_video_colors(sh_video, "contrast", vo_gamma_contrast);
	if (vo_gamma_saturation != 1000)
	    set_video_colors(sh_video, "saturation", vo_gamma_saturation);
	if (vo_gamma_hue != 1000)
	    set_video_colors(sh_video, "hue", vo_gamma_hue);
    }

   if(vo_flags & 0x08 && vo_spudec)
      spudec_set_hw_spu(vo_spudec,video_out);

#ifdef HAVE_FREETYPE
   force_load_font = 1;
#endif

//================== MAIN: ==========================
main:
current_module="main";

// If there is no video OSD has to be disabled.
// In case of playing a playtree we have to restore the
// old OSD level after playing one or more audio-only files.
if(!sh_video && osd_level > 0) { // save OSD level only once
    osd_level_saved = osd_level;
    osd_level = 0;
} else if (osd_level_saved > -1) { // if there is a saved OSD level, restore it
    osd_level = osd_level_saved;
    osd_level_saved = -1;
}

fflush(stdout);

#ifdef HAVE_NEW_GUI
   if ( use_gui )
    {
     guiGetEvent( guiSetStream,(char *)stream );
     guiGetEvent( guiSetFileName,filename );
     if ( sh_audio ) guiIntfStruct.AudioType=sh_audio->channels; else guiIntfStruct.AudioType=0;
     if ( !sh_video && sh_audio ) guiGetEvent( guiSetAudioOnly,(char *)1 ); else guiGetEvent( guiSetAudioOnly,(char *)0 );
     guiGetEvent( guiSetFileFormat,(char *)demuxer->file_format );
     if ( guiGetEvent( guiSetValues,(char *)sh_video ) ) goto goto_next_file;
    }
#endif

{
//int frame_corr_num=0;   //
//float v_frame=0;    // Video
float time_frame=0; // Timer
//float num_frames=0;      // number of frames played
int grab_frames=0;
char osd_text_buffer[64];
int drop_frame=0;     // current dropping status
int dropped_frames=0; // how many frames dropped since last non-dropped frame
int too_slow_frame_cnt=0;
int too_fast_frame_cnt=0;
// for auto-quality:
float AV_delay=0; // average of A-V timestamp differences
double vdecode_time;
unsigned int lastframeout_ts=0;
float time_frame_corr_avg=0;

float next_frame_time=0;
int frame_time_remaining=0; // flag
int blit_frame=0;

osd_text_buffer[0]=0;

//================ SETUP AUDIO ==========================

if(sh_audio){
  //const ao_info_t *info=audio_out->info;
  current_module="ao2_init";
  if(!(audio_out=init_best_audio_out(audio_driver_list,
      (ao_plugin_cfg.plugin_list), // plugin flag
      force_srate?force_srate:sh_audio->samplerate,
      sh_audio->channels,sh_audio->sample_format,0))){
    // FAILED:
    mp_msg(MSGT_CPLAYER,MSGL_ERR,MSGTR_CannotInitAO);
    sh_audio=d_audio->sh=NULL; // -> nosound
  } else {
    // SUCCESS:
    inited_flags|=INITED_AO;
    mp_msg(MSGT_CPLAYER,MSGL_INFO,"AO: [%s] %iHz %dch %s\n",
      audio_out->info->short_name,
      force_srate?force_srate:sh_audio->samplerate,
      sh_audio->channels,
      audio_out_format_name(sh_audio->sample_format));
    mp_msg(MSGT_CPLAYER,MSGL_V,MSGTR_AODescription_AOAuthor,
      audio_out->info->name, audio_out->info->author);
    if(strlen(audio_out->info->comment) > 0)
      mp_msg(MSGT_CPLAYER,MSGL_V,MSGTR_AOComment, audio_out->info->comment);
  }
}

current_module="av_init";

if(sh_video) sh_video->timer=0;
if(sh_audio) sh_audio->timer=-audio_delay;

if(!sh_audio){
  mp_msg(MSGT_CPLAYER,MSGL_INFO,MSGTR_NoSound);
  if(verbose) mp_msg(MSGT_CPLAYER,MSGL_V,"Freeing %d unused audio chunks\n",d_audio->packs);
  ds_free_packs(d_audio); // free buffered chunks
  d_audio->id=-2;         // do not read audio chunks
  uninit_player(INITED_AO); // close device
}
if(!sh_video){
   mp_msg(MSGT_CPLAYER,MSGL_INFO,MSGTR_Video_NoVideo);
   if(verbose) mp_msg(MSGT_CPLAYER,MSGL_V,"Freeing %d unused video chunks\n",d_video->packs);
   ds_free_packs(d_video);
   d_video->id=-2;
   uninit_player(INITED_VO);
}

if (!sh_video && !sh_audio)
    goto goto_next_file;

if(demuxer->file_format!=DEMUXER_TYPE_AVI) pts_from_bps=0; // it must be 0 for mpeg/asf!
if(force_fps){
  vo_fps = sh_video->fps=force_fps;
  sh_video->frametime=1.0f/sh_video->fps;
  mp_msg(MSGT_CPLAYER,MSGL_INFO,MSGTR_FPSforced,sh_video->fps,sh_video->frametime);
}

//==================== START PLAYING =======================

mp_msg(MSGT_CPLAYER,MSGL_INFO,MSGTR_StartPlaying);fflush(stdout);

InitTimer();

#ifdef USE_DVDNAV
if (dvd_nav && stream->type==STREAMTYPE_DVDNAV) {
  dvdnav_stream_fullstart((dvdnav_priv_t *)stream->priv);
}
#endif

total_time_usage_start=GetTimer();
audio_time_usage=0; video_time_usage=0; vout_time_usage=0;
total_frame_cnt=0; drop_frame_cnt=0; // fix for multifile fps benchmark
play_n_frames=play_n_frames_mf;

if(play_n_frames==0){
  eof=PT_NEXT_ENTRY; goto goto_next_file;
}

while(!eof){
    float aq_sleep_time=0;

    if(play_n_frames>=0){
      --play_n_frames;
      if(play_n_frames<0) eof = PT_NEXT_ENTRY;
    }

/*========================== PLAY AUDIO ============================*/

while(sh_audio){
  unsigned int t;
  double tt;
  int playsize;

  current_module="play_audio";
  
  ao_data.pts=sh_audio->timer*90000.0;
  playsize=audio_out->get_space();
  
  // handle audio-only case:
  if(!playsize && !sh_video) {  // buffer is full, do not block here!!!
    usec_sleep(10000); // Wait a tick before retry
    continue;
  }
  
  if(playsize>MAX_OUTBURST) playsize=MAX_OUTBURST; // we shouldn't exceed it!

  // Fill buffer if needed:
  current_module="decode_audio";   // Enter AUDIO decoder module
  t=GetTimer();
  while(sh_audio->a_buffer_len<playsize && !d_audio->eof){
    int ret=decode_audio(sh_audio,&sh_audio->a_buffer[sh_audio->a_buffer_len],
        playsize-sh_audio->a_buffer_len,sh_audio->a_buffer_size-sh_audio->a_buffer_len);
    if(ret<=0) break; // EOF?
    sh_audio->a_buffer_len+=ret;
  }
  t=GetTimer()-t;
  tt = t*0.000001f; audio_time_usage+=tt;
  if(playsize>sh_audio->a_buffer_len) playsize=sh_audio->a_buffer_len;

  // play audio:  
  current_module="play_audio";
  playsize=audio_out->play(sh_audio->a_buffer,playsize,0);

  if(playsize>0){
      sh_audio->a_buffer_len-=playsize;
      memmove(sh_audio->a_buffer,&sh_audio->a_buffer[playsize],sh_audio->a_buffer_len);
      sh_audio->timer+=playsize/(float)(sh_audio->o_bps);
  }

  break;
} // while(sh_audio)

if(!sh_video) {
  // handle audio-only case:
  if(!quiet) mp_msg(MSGT_AVSYNC,MSGL_STATUS,"A:%6.1f %4.1f%% %d%%   \r"
		    ,sh_audio->timer-audio_out->get_delay()
		    ,(sh_audio->timer>0.5)?100.0*audio_time_usage/(double)sh_audio->timer:0
		    ,cache_fill_status
		    );
  if(d_audio->eof) eof = PT_NEXT_ENTRY;

} else {

/*========================== PLAY VIDEO ============================*/

  float frame_time=next_frame_time;

  vo_pts=sh_video->timer*90000.0;
  vo_fps=sh_video->fps;

  if(!frame_time_remaining){
    //--------------------  Decode a frame: -----------------------
    vdecode_time=video_time_usage;
    while(1)
    {   unsigned char* start=NULL;
	int in_size;
	// get it!
	current_module="video_read_frame";
        in_size=video_read_frame(sh_video,&next_frame_time,&start,force_fps);
	if(in_size<0){ eof=1; break; }
	if(in_size>max_framesize) max_framesize=in_size; // stats
	sh_video->timer+=frame_time;
	time_frame+=frame_time;  // for nosound
	// check for frame-drop:
	current_module="check_framedrop";
	if(sh_audio && !d_audio->eof){
	    float delay=audio_out->get_delay();
	    float d=(sh_video->timer)-(sh_audio->timer-delay);
	    // we should avoid dropping to many frames in sequence unless we
	    // are too late. and we allow 100ms A-V delay here:
	    if(d<-dropped_frames*frame_time-0.100){
		drop_frame=frame_dropping;
		++drop_frame_cnt;
		++dropped_frames;
	    } else {
		drop_frame=dropped_frames=0;
	    }
	    ++total_frame_cnt;
	}
	// decode:
	current_module="decode_video";
//	printf("Decode! %p  %d  \n",start,in_size);
	blit_frame=decode_video(sh_video,start,in_size,drop_frame);
	break;
    }
    vdecode_time=video_time_usage-vdecode_time;
    //------------------------ frame decoded. --------------------

    mp_dbg(MSGT_AVSYNC,MSGL_DBG2,"*** ftime=%5.3f ***\n",frame_time);

    if(sh_video->vf_inited<0){
	mp_msg(MSGT_CPLAYER,MSGL_FATAL,MSGTR_NotInitializeVOPorVO);
	eof=1; goto goto_next_file;
    }

  }

// ==========================================================================
    
//    current_module="draw_osd";
//    if(vo_config_count) video_out->draw_osd();

#ifdef HAVE_NEW_GUI
    if(use_gui) guiEventHandling();
#endif

    current_module="calc_sleep_time";

#if 0
{	// debug frame dropping code
	  float delay=audio_out->get_delay();
	  mp_msg(MSGT_AVSYNC,MSGL_V,"\r[V] %5.3f [A] %5.3f => {%5.3f}  (%5.3f) [%d]   \n",
	      sh_video->timer,sh_audio->timer-delay,
	      sh_video->timer-(sh_audio->timer-delay),
	      delay,drop_frame);
}
#endif

    if(drop_frame && !frame_time_remaining){

      time_frame=0;	// don't sleep!
      blit_frame=0;	// don't display!
      
    } else {

      // It's time to sleep...
      
      frame_time_remaining=0;
      time_frame-=GetRelativeTime(); // reset timer

      if(sh_audio && !d_audio->eof){
	  float delay=audio_out->get_delay();
	  mp_dbg(MSGT_AVSYNC,MSGL_DBG2,"delay=%f\n",delay);

          time_frame=sh_video->timer;
          time_frame-=sh_audio->timer-delay;

	// delay = amount of audio buffered in soundcard/driver
	if(delay>0.25) delay=0.25; else
	if(delay<0.10) delay=0.10;
	if(time_frame>delay*0.6){
	    // sleep time too big - may cause audio drops (buffer underrun)
	    frame_time_remaining=1;
	    time_frame=delay*0.5;
	}

      } else {

          // NOSOUND:
          if( (time_frame<-3*frame_time || time_frame>3*frame_time) || benchmark)
	      time_frame=0;
	  
      }

//      if(verbose>1)printf("sleep: %5.3f  a:%6.3f  v:%6.3f  \n",time_frame,sh_audio->timer,sh_video->timer);

      aq_sleep_time+=time_frame;

    }	// !drop_frame
    
//============================== SLEEP: ===================================

// flag 256 means: libvo driver does its timing (dvb card)
if(time_frame>0.001 && !(vo_flags&256)){

#ifdef HAVE_RTC
    if(rtc_fd>=0){
	// -------- RTC -----------
	current_module="sleep_rtc";
        while (time_frame > 0.000) {
	    unsigned long rtc_ts;
	    if (read (rtc_fd, &rtc_ts, sizeof(rtc_ts)) <= 0)
		    mp_msg(MSGT_CPLAYER, MSGL_ERR, "Linux RTC read error: %s\n", strerror(errno));
    	    time_frame-=GetRelativeTime();
	}
    } else
#endif
    {
	// -------- USLEEP + SOFTSLEEP -----------
	float min=softsleep?0.021:0.005;
	current_module="sleep_usleep";
        while(time_frame>min){
          if(time_frame<=0.020)
             usec_sleep(0); // sleeps 1 clock tick (10ms)!
          else
             usec_sleep(1000000*(time_frame-0.020));
          time_frame-=GetRelativeTime();
        }
	if(softsleep){
	    current_module="sleep_soft";
	    if(time_frame<0) mp_msg(MSGT_AVSYNC, MSGL_WARN, "Warning! Softsleep underflow!\n");
	    while(time_frame>0) time_frame-=GetRelativeTime(); // burn the CPU
	}
    }

}

//if(!frame_time_remaining){	// should we display the frame now?

//====================== FLIP PAGE (VIDEO BLT): =========================

        current_module="vo_check_events";
	if(vo_config_count) video_out->check_events();

        current_module="flip_page";
        if(blit_frame && !frame_time_remaining){
	   unsigned int t2=GetTimer();
	   double tt;
	   float j;
#define	FRAME_LAG_WARN	0.2
	   j = ((float)t2 - lastframeout_ts) / 1000000;
	   lastframeout_ts = GetTimer();
	   if (j < frame_time + frame_time * -FRAME_LAG_WARN)
		too_fast_frame_cnt++;
		/* printf ("PANIC: too fast frame (%.3f)!\n", j); */
	   else if (j > frame_time + frame_time * FRAME_LAG_WARN)
		too_slow_frame_cnt++;
		/* printf ("PANIC: too slow frame (%.3f)!\n", j); */

	   if(vo_config_count) video_out->flip_page();
//        usec_sleep(50000); // test only!
	   t2=GetTimer()-t2;
	   tt = t2*0.000001f;
	   vout_time_usage+=tt;
	}

//====================== A-V TIMESTAMP CORRECTION: =========================

  current_module="av_sync";

  if(sh_audio){
    float a_pts=0;
    float v_pts=0;

    // unplayed bytes in our and soundcard/dma buffer:
    float delay=audio_out->get_delay()+(float)sh_audio->a_buffer_len/(float)sh_audio->o_bps;

    if(pts_from_bps){
	// PTS = sample_no / samplerate
        unsigned int samples=(sh_audio->audio.dwSampleSize)?
          ((ds_tell(d_audio)-sh_audio->a_in_buffer_len)/sh_audio->audio.dwSampleSize) :
          (d_audio->block_no); // <- used for VBR audio
	samples+=sh_audio->audio.dwStart; // offset
        a_pts=samples*(float)sh_audio->audio.dwScale/(float)sh_audio->audio.dwRate;
	delay_corrected=1;
    } else {
      // PTS = (last timestamp) + (bytes after last timestamp)/(bytes per sec)
      a_pts=d_audio->pts;
      if(!delay_corrected) if(a_pts) delay_corrected=1;
#if 0
      printf("\n#X# pts=%5.3f ds_pts=%5.3f buff=%5.3f total=%5.3f\n",
          a_pts,
	  ds_tell_pts(d_audio)/(float)sh_audio->i_bps,
	  -sh_audio->a_in_buffer_len/(float)sh_audio->i_bps,
	  a_pts+(ds_tell_pts(d_audio)-sh_audio->a_in_buffer_len)/(float)sh_audio->i_bps);
#endif	  
      a_pts+=(ds_tell_pts(d_audio)-sh_audio->a_in_buffer_len)/(float)sh_audio->i_bps;
    }
    v_pts=d_video->pts;

      mp_dbg(MSGT_AVSYNC,MSGL_DBG2,"### A:%8.3f (%8.3f)  V:%8.3f  A-V:%7.4f  \n",a_pts,a_pts-audio_delay-delay,v_pts,(a_pts-delay-audio_delay)-v_pts);

      if(delay_corrected){
	static int drop_message=0;
        float x;
	AV_delay=(a_pts-delay-audio_delay)-v_pts;
	if(drop_frame_cnt>50+drop_message*250 && AV_delay>0.5){
	  ++drop_message;
	  mp_msg(MSGT_AVSYNC,MSGL_ERR,MSGTR_SystemTooSlow);
	}
        x=AV_delay*0.1f;
        if(x<-max_pts_correction) x=-max_pts_correction; else
        if(x> max_pts_correction) x= max_pts_correction;
        if(default_max_pts_correction>=0)
          max_pts_correction=default_max_pts_correction;
        else
          max_pts_correction=sh_video->frametime*0.10; // +-10% of time
	if(!frame_time_remaining){ sh_audio->timer+=x; c_total+=x;} // correction
        if(!quiet) mp_msg(MSGT_AVSYNC,MSGL_STATUS,"A:%6.1f V:%6.1f A-V:%7.3f ct:%7.3f  %3d/%3d  %2d%% %2d%% %4.1f%% %d %d %d%%\r",
	  a_pts-audio_delay-delay,v_pts,AV_delay,c_total,
          (int)sh_video->num_frames,(int)sh_video->num_frames_decoded,
          (sh_video->timer>0.5)?(int)(100.0*video_time_usage/(double)sh_video->timer):0,
          (sh_video->timer>0.5)?(int)(100.0*vout_time_usage/(double)sh_video->timer):0,
          (sh_video->timer>0.5)?(100.0*audio_time_usage/(double)sh_video->timer):0
          ,drop_frame_cnt
	  ,output_quality
	  ,cache_fill_status
        );
        fflush(stdout);
      }
    
  } else {
    // No audio:
    
    if(!quiet)
      mp_msg(MSGT_AVSYNC,MSGL_STATUS,"V:%6.1f  %3d  %2d%% %2d%% %4.1f%% %d %d %d%%\r",d_video->pts,
        (int)sh_video->num_frames,
        (sh_video->timer>0.5)?(int)(100.0*video_time_usage/(double)sh_video->timer):0,
        (sh_video->timer>0.5)?(int)(100.0*vout_time_usage/(double)sh_video->timer):0,
        (sh_video->timer>0.5)?(100.0*audio_time_usage/(double)sh_video->timer):0
          ,drop_frame_cnt
	  ,output_quality
	  ,cache_fill_status
        );

      fflush(stdout);

  }

//============================ Auto QUALITY ============================

/*Output quality adjustments:*/
if(auto_quality>0){
  current_module="autoq";
//  float total=0.000001f * (GetTimer()-aq_total_time);
//  if(output_quality<auto_quality && aq_sleep_time>0.05f*total)
  if(output_quality<auto_quality && aq_sleep_time>0)
      ++output_quality;
  else
//  if(output_quality>0 && aq_sleep_time<-0.05f*total)
  if(output_quality>1 && aq_sleep_time<0)
      --output_quality;
  else
  if(output_quality>0 && aq_sleep_time<-0.050f) // 50ms
      output_quality=0;
//  printf("total: %8.6f  sleep: %8.6f  q: %d\n",(0.000001f*aq_total_time),aq_sleep_time,output_quality);
  set_video_quality(sh_video,output_quality);
}

} // end if(sh_video)

//============================ Handle PAUSE ===============================

  current_module="pause";

#ifdef USE_OSD
  if(osd_visible){
    if (!--osd_visible){
       vo_osd_progbar_type=-1; // disable
       vo_osd_changed(OSDTYPE_PROGBAR);
       if (osd_function != OSD_PAUSE)
	   osd_function = OSD_PLAY;
    }
  }
#endif

  if(osd_function==OSD_PAUSE){
    mp_cmd_t* cmd;
      if(!quiet) {
	mp_msg(MSGT_CPLAYER,MSGL_STATUS,MSGTR_Paused);
	fflush(stdout);
      }
#ifdef HAVE_NEW_GUI
      if(use_gui) guiGetEvent( guiCEvent,(char *)guiSetPause );
#endif
      if (video_out && sh_video && vo_config_count)
	 video_out->control(VOCTRL_PAUSE, NULL);

      if (audio_out && sh_audio)
         audio_out->pause();	// pause audio, keep data if possible

      while( (cmd = mp_input_get_cmd(20,1)) == NULL) {
	     if(sh_video && video_out && vo_config_count) video_out->check_events();
#ifdef HAVE_NEW_GUI
             if(use_gui){
		guiEventHandling();
		guiGetEvent( guiReDraw,NULL );
		if(guiIntfStruct.Playing!=2 || (rel_seek_secs || abs_seek_pos)) break;
             }
#endif
             usleep(20000);
         }
      mp_cmd_free(cmd);
         osd_function=OSD_PLAY;
      if (audio_out && sh_audio)
        audio_out->resume();	// resume audio
      if (video_out && sh_video && vo_config_count)
        video_out->control(VOCTRL_RESUME, NULL);	// resume video
      (void)GetRelativeTime();	// keep TF around FT in next cycle
#ifdef HAVE_NEW_GUI
      if (use_gui) 
       {
        if ( guiIntfStruct.Playing == guiSetStop ) goto goto_next_file;
        guiGetEvent( guiCEvent,(char *)guiSetPlay );
       }
#endif
  }

// handle -sstep
if(step_sec>0) {
	osd_function=OSD_FFW;
	rel_seek_secs+=step_sec;
}

#ifdef USE_DVDNAV
if (stream->type==STREAMTYPE_DVDNAV && dvd_nav_still)
    dvdnav_stream_sleeping((dvdnav_priv_t*)stream->priv);
#endif

//================= Keyboard events, SEEKing ====================

  current_module="key_events";

{
  mp_cmd_t* cmd;
  while( (cmd = mp_input_get_cmd(0,0)) != NULL) {
    switch(cmd->id) {
    case MP_CMD_SEEK : {
      int v,abs;
      v = cmd->args[0].v.i;
      abs = (cmd->nargs > 1) ? cmd->args[1].v.i : 0;
      if(abs==2) { /* Absolute seek to a specific timestamp in seconds */
        abs_seek_pos = 1;
	if(sh_video)
	  osd_function= (v > sh_video->timer) ? OSD_FFW : OSD_REW;
	rel_seek_secs = v;
      }
      else if(abs) { /* Absolute seek by percentage */
	abs_seek_pos = 3;
	if(sh_video)
	  osd_function= (v > sh_video->timer) ? OSD_FFW : OSD_REW;
	rel_seek_secs = v/100.0;
      }
      else {
	rel_seek_secs+= v;
	osd_function= (v > 0) ? OSD_FFW : OSD_REW;
      }
    } break;
    case MP_CMD_AUDIO_DELAY : {
      float v = cmd->args[0].v.f;
      audio_delay += v;
      osd_show_av_delay = 9;
      if(sh_audio) sh_audio->timer+= v;
    } break;
    case MP_CMD_PAUSE : {
      osd_function=OSD_PAUSE;
    } break;
    case MP_CMD_QUIT : {
      exit_player(MSGTR_Exit_quit);
    }
    case MP_CMD_GRAB_FRAMES : {
      grab_frames=2;
    } break;
    case MP_CMD_PLAY_TREE_STEP : {
      int n = cmd->args[0].v.i > 0 ? 1 : -1;
      int force = cmd->args[1].v.i;

      if(!force) {
	play_tree_iter_t* i = play_tree_iter_new_copy(playtree_iter);
	
	if(play_tree_iter_step(i,n,0) == PLAY_TREE_ITER_ENTRY)
	  eof = (n > 0) ? PT_NEXT_ENTRY : PT_PREV_ENTRY;
	play_tree_iter_free(i);
      } else
	eof = (n > 0) ? PT_NEXT_ENTRY : PT_PREV_ENTRY;
    } break;
    case MP_CMD_PLAY_TREE_UP_STEP : {
      int n = cmd->args[0].v.i > 0 ? 1 : -1;
      int force = cmd->args[1].v.i;

      if(!force) {
	play_tree_iter_t* i = play_tree_iter_new_copy(playtree_iter);
	if(play_tree_iter_up_step(i,n,0) == PLAY_TREE_ITER_ENTRY)
	  eof = (n > 0) ? PT_UP_NEXT : PT_UP_PREV;
	play_tree_iter_free(i);
      } else
	eof = (n > 0) ? PT_UP_NEXT : PT_UP_PREV;
    } break;
    case MP_CMD_PLAY_ALT_SRC_STEP : {
      if(playtree_iter->num_files > 1) {
	int v = cmd->args[0].v.i;
	if(v > 0 && playtree_iter->file < playtree_iter->num_files)
	  eof = PT_NEXT_SRC;
	else if(v < 0 && playtree_iter->file > 1)
	  eof = PT_PREV_SRC;
      }
    } break;
    case MP_CMD_SUB_DELAY : {
      int abs= cmd->args[1].v.i;
      float v = cmd->args[0].v.f;
      if(abs)
	sub_delay = v;
      else
	sub_delay += v;
      osd_show_sub_delay = 9; // show the subdelay in OSD
    } break;
    case MP_CMD_OSD : 
      if(sh_video) {
	int v = cmd->args[0].v.i;
	if(v < 0)
	  osd_level=(osd_level+1)%3;
	else
	  osd_level= v > 2 ? 2 : v;
      } break;
    case MP_CMD_VOLUME :  {
      int v = cmd->args[0].v.i;

		// start change for absolute volume value
		int abs = (cmd->nargs > 1) ? cmd->args[1].v.i : 0;
		
		if( abs )
		{
			mixer_setvolume( (float)v, (float)v );
		} else {
      if(v > 0)
	mixer_incvolume();
      else
	mixer_decvolume();
		}
	  
#ifdef USE_OSD
      if(osd_level){
	osd_visible=sh_video->fps; // 1 sec
	vo_osd_progbar_type=OSD_VOLUME;
	vo_osd_progbar_value=(mixer_getbothvolume()*256.0)/100.0;
	vo_osd_changed(OSDTYPE_PROGBAR);
      }
#endif
    } break;
    case MP_CMD_MUTE:
      mixer_mute();
      break;
    case MP_CMD_LOADFILE : {
      play_tree_t* e = play_tree_new();
      play_tree_add_file(e,cmd->args[0].v.s);

      // Go back to the start point
      while(play_tree_iter_up_step(playtree_iter,0,1) != PLAY_TREE_ITER_END)
	/* NOP */;
      play_tree_free_list(playtree->child,1);
      play_tree_set_child(playtree,e);
      play_tree_iter_step(playtree_iter,0,0);
      eof = PT_NEXT_SRC;
    } break;
    case MP_CMD_LOADLIST : {
      play_tree_t* e = parse_playlist_file(cmd->args[0].v.s);
      if(!e)
	mp_msg(MSGT_CPLAYER,MSGL_ERR,MSGTR_PlaylistLoadUnable,cmd->args[0].v.s);
      else {
	// Go back to the start point
	while(play_tree_iter_up_step(playtree_iter,0,1) != PLAY_TREE_ITER_END)
	  /* NOP */;
	play_tree_free_list(playtree->child,1);
	play_tree_set_child(playtree,e);
	play_tree_iter_step(playtree_iter,0,0);
	eof = PT_NEXT_SRC;	
      }
    } break;
    case MP_CMD_GAMMA :  {
      int v = cmd->args[0].v.i, abs = cmd->args[1].v.i;

      if (!sh_video)
	break;

      if (vo_gamma_gamma == 1000)
      {
	vo_gamma_gamma = 0;
	get_video_colors (sh_video, "gamma", &vo_gamma_gamma);
      }

      if (abs)
        vo_gamma_gamma = v;
      else
        vo_gamma_gamma += v;

      if (vo_gamma_gamma > 100)
        vo_gamma_gamma = 100;
      else if (vo_gamma_gamma < -100)
        vo_gamma_gamma = -100;
      set_video_colors(sh_video, "gamma", vo_gamma_gamma);
#ifdef USE_OSD
       if(osd_level){
	 osd_visible=sh_video->fps; // 1 sec
	 vo_osd_progbar_type=OSD_BRIGHTNESS;
	 vo_osd_progbar_value=(vo_gamma_gamma<<7)/100 + 128;
	 vo_osd_changed(OSDTYPE_PROGBAR);
       }
#endif // USE_OSD
    } break;
    case MP_CMD_BRIGHTNESS :  {
      int v = cmd->args[0].v.i, abs = cmd->args[1].v.i;
      
      if (!sh_video)
	break;
      
      if (vo_gamma_brightness == 1000)
      {
	vo_gamma_brightness = 0;
	get_video_colors(sh_video, "brightness", &vo_gamma_brightness);
      }

      if (abs)
        vo_gamma_brightness = v;
      else
        vo_gamma_brightness += v;

      if (vo_gamma_brightness > 100)
        vo_gamma_brightness = 100;
      else if (vo_gamma_brightness < -100)
        vo_gamma_brightness = -100;
      if(set_video_colors(sh_video, "brightness", vo_gamma_brightness)){
#ifdef USE_OSD
       if(osd_level){
	 osd_visible=sh_video->fps; // 1 sec
	 vo_osd_progbar_type=OSD_BRIGHTNESS;
	 vo_osd_progbar_value=(vo_gamma_brightness<<7)/100 + 128;
	 vo_osd_changed(OSDTYPE_PROGBAR);
       }
#endif // USE_OSD
      }
    } break;
    case MP_CMD_CONTRAST :  {
      int v = cmd->args[0].v.i, abs = cmd->args[1].v.i;

      if (!sh_video)
	break;
      
      if (vo_gamma_contrast == 1000)
      {
	vo_gamma_contrast = 0;
	get_video_colors(sh_video, "contrast", &vo_gamma_contrast);
      }
     
      if (abs)
        vo_gamma_contrast = v;
      else
        vo_gamma_contrast += v;

      if (vo_gamma_contrast > 100)
        vo_gamma_contrast = 100;
      else if (vo_gamma_contrast < -100)
        vo_gamma_contrast = -100;
      if(set_video_colors(sh_video, "contrast", vo_gamma_contrast)){
#ifdef USE_OSD
       if(osd_level){
	 osd_visible=sh_video->fps; // 1 sec
	 vo_osd_progbar_type=OSD_CONTRAST;
	 vo_osd_progbar_value=(vo_gamma_contrast<<7)/100 + 128;
	 vo_osd_changed(OSDTYPE_PROGBAR);
       }
#endif // USE_OSD
      }
    } break;
    case MP_CMD_SATURATION :  {
      int v = cmd->args[0].v.i, abs = cmd->args[1].v.i;

      if (!sh_video)
	break;
      
      if (vo_gamma_saturation == 1000)
      {
	vo_gamma_saturation = 0;
	get_video_colors(sh_video, "saturation", &vo_gamma_saturation);
      }

      if (abs)
        vo_gamma_saturation = v;
      else
        vo_gamma_saturation += v;

      if (vo_gamma_saturation > 100)
        vo_gamma_saturation = 100;
      else if (vo_gamma_saturation < -100)
        vo_gamma_saturation = -100;
      if(set_video_colors(sh_video, "saturation", vo_gamma_saturation)){
#ifdef USE_OSD
       if(osd_level){
	 osd_visible=sh_video->fps; // 1 sec
	 vo_osd_progbar_type=OSD_SATURATION;
	 vo_osd_progbar_value=(vo_gamma_saturation<<7)/100 + 128;
	 vo_osd_changed(OSDTYPE_PROGBAR);
       }
#endif // USE_OSD
      }
    } break;
    case MP_CMD_HUE :  {
      int v = cmd->args[0].v.i, abs = cmd->args[1].v.i;

      if (!sh_video)
	break;
      
      if (vo_gamma_hue == 1000)
      {
	vo_gamma_hue = 0;
	get_video_colors(sh_video, "hue", &vo_gamma_hue);
      }
     
      if (abs)
        vo_gamma_hue = v;
      else
        vo_gamma_hue += v;

      if (vo_gamma_hue > 100)
        vo_gamma_hue = 100;
      else if (vo_gamma_hue < -100)
        vo_gamma_hue = -100;
      if(set_video_colors(sh_video, "hue", vo_gamma_hue)){
#ifdef USE_OSD
       if(osd_level){
	 osd_visible=sh_video->fps; // 1 sec
	 vo_osd_progbar_type=OSD_HUE;
	 vo_osd_progbar_value=(vo_gamma_hue<<7)/100 + 128;
	 vo_osd_changed(OSDTYPE_PROGBAR);
       }
#endif // USE_OSD
      }
    } break;
    case MP_CMD_FRAMEDROPPING :  {
      int v = cmd->args[0].v.i;
      if(v < 0)
	frame_dropping = (frame_dropping+1)%3;
      else
	frame_dropping = v > 2 ? 2 : v;
    } break;
#ifdef USE_TV
    case MP_CMD_TV_STEP_CHANNEL :  {
      if (tv_param_on == 1) {
	int v = cmd->args[0].v.i;
	if(v > 0)
	  tv_step_channel((tvi_handle_t*)(demuxer->priv), TV_CHANNEL_HIGHER);
	else
	  tv_step_channel((tvi_handle_t*)(demuxer->priv), TV_CHANNEL_LOWER);
      }
    } break;
    case MP_CMD_TV_STEP_NORM :  {
      if (tv_param_on == 1)
	tv_step_norm((tvi_handle_t*)(demuxer->priv));
    } break;
    case MP_CMD_TV_STEP_CHANNEL_LIST :  {
      if (tv_param_on == 1)
	tv_step_chanlist((tvi_handle_t*)(demuxer->priv));
    } break;
#endif
    case MP_CMD_VO_FULLSCREEN:
    {
#ifdef HAVE_NEW_GUI
     if ( use_gui ) guiGetEvent( guiIEvent,(char *)MP_CMD_GUI_FULLSCREEN );
      else
#endif
	if(video_out && vo_config_count) video_out->control(VOCTRL_FULLSCREEN, 0);
    } break;
    case MP_CMD_PANSCAN : {
      if ( !video_out ) break;
      if ( video_out->control( VOCTRL_GET_PANSCAN,NULL ) == VO_TRUE )
       {
        int abs= cmd->args[1].v.i;
        float v = cmd->args[0].v.f;
        float res;
        if(abs) res = v;
          else res = vo_panscan+v;
        vo_panscan = res > 1 ? 1 : res < 0 ? 0 : res;
        video_out->control( VOCTRL_SET_PANSCAN,NULL );
#ifdef USE_OSD
        if(osd_level){
	  osd_visible=sh_video->fps; // 1 sec
	  vo_osd_progbar_type=OSD_PANSCAN;
	  vo_osd_progbar_value=vo_panscan*256;
	  vo_osd_changed(OSDTYPE_PROGBAR);
        }
#endif
       }
    } break;
    case MP_CMD_SUB_POS:
    {
        int v;
	v = cmd->args[0].v.i;
    
	sub_pos+=v;
	if(sub_pos >100) sub_pos=100;
	if(sub_pos <0) sub_pos=0;
	vo_osd_changed(OSDTYPE_SUBTITLE);
    }	break;
    case MP_CMD_SCREENSHOT :
      if(vo_config_count) video_out->control(VOCTRL_SCREENSHOT, NULL);
      break;
    case MP_CMD_VF_CHANGE_RECTANGLE:
	set_rectangle(sh_video, cmd->args[0].v.i, cmd->args[1].v.i);
	break;
#ifdef USE_DVDNAV
    case MP_CMD_DVDNAV_EVENT: {
      dvdnav_priv_t * dvdnav_priv = (dvdnav_priv_t*)(stream->priv);
      dvdnav_event_t * dvdnav_event = (dvdnav_event_t *)(cmd->args[0].v.v);

      /* ignore these events if we're not in dvd_nav mode */
      if (!dvd_nav) break;

      if (!dvdnav_event) {
        printf("DVDNAV Event NULL?!\n");
        break;
      }

      if (stream->type!=STREAMTYPE_DVDNAV) {
        printf("Got DVDNAV event when not running a DVDNAV stream!?\n");
        break;
      }

      //printf("mplayer: got event: %d\n",dvdnav_event->event);

      switch (dvdnav_event->event) {
      case DVDNAV_BLOCK_OK: {
          /* be silent about this one */
                break;
          }
      case DVDNAV_HIGHLIGHT: {
          dvdnav_highlight_event_t *hevent = (dvdnav_highlight_event_t*)(dvdnav_event->details);
          if (!hevent) {
                printf("DVDNAV Event: Highlight event broken\n");
                break;
          }

          if (hevent->display && hevent->buttonN>0)
          {
                //dvdnav_priv->seen_root_menu=1; /* if we got a highlight, we're on a menu */
                sprintf( dvd_nav_text, "Highlight button %d (%u,%u)-(%u,%u) PTS %d (now is %5.2f)",
                     hevent->buttonN,
                     hevent->sx,hevent->sy,
                     hevent->ex,hevent->ey,
                     hevent->pts, d_video->pts);
                printf("DVDNAV Event: %s\n",dvd_nav_text);
                //osd_show_dvd_nav_delay = 60;

                osd_show_dvd_nav_highlight=1;
                osd_show_dvd_nav_sx=hevent->sx;
                osd_show_dvd_nav_ex=hevent->ex;
                osd_show_dvd_nav_sy=hevent->sy;
                osd_show_dvd_nav_ey=hevent->ey;
          }
          else {
                  osd_show_dvd_nav_highlight=0;
                  printf("DVDNAV Event: Highlight Hide\n");
          }
        break;
        }
      case DVDNAV_STILL_FRAME: {
          dvdnav_still_event_t *still_event = (dvdnav_still_event_t*)(dvdnav_event->details);

          printf( "######################################## DVDNAV Event: Still Frame: %d sec(s)\n", still_event->length );
          while (dvdnav_stream_sleeping(dvdnav_priv)) {
            usleep(1000); /* 1ms */
          }
          dvdnav_stream_sleep(dvdnav_priv,still_event->length);
        break;
        }
      case DVDNAV_STOP: {
          printf( "DVDNAV Event: Nav Stop\n" );
        break;
        }
      case DVDNAV_NOP: {
        printf("DVDNAV Event: Nav NOP\n");
        break;
        }
      case DVDNAV_SPU_STREAM_CHANGE: {
#if DVDNAVVERSION > 012
        dvdnav_spu_stream_change_event_t *stream_change = (dvdnav_spu_stream_change_event_t*)(dvdnav_event->details);

        printf("DVDNAV Event: Nav SPU Stream Change: phys: %d/%d/%d logical: %d\n",
                stream_change->physical_wide,
                stream_change->physical_letterbox,
                stream_change->physical_pan_scan,
                stream_change->logical);

        if (vo_spudec && dvdsub_id!=stream_change->physical_wide) {
                mp_msg(MSGT_INPUT,MSGL_DBG2,"d_dvdsub->id change: was %d is now %d\n",
                        d_dvdsub->id,stream_change->physical_wide);
                // FIXME: need a better way to change SPU id
                d_dvdsub->id=dvdsub_id=stream_change->physical_wide;
                if (vo_spudec) spudec_reset(vo_spudec);
        }
#else
        dvdnav_stream_change_event_t *stream_change = (dvdnav_stream_change_event_t*)(dvdnav_event->details);

        printf("DVDNAV Event: Nav SPU Stream Change: phys: %d logical: %d\n",
                stream_change->physical,
                stream_change->logical);

        if (vo_spudec && dvdsub_id!=stream_change->physical) {
                mp_msg(MSGT_INPUT,MSGL_DBG2,"d_dvdsub->id change: was %d is now %d\n",
                        d_dvdsub->id,stream_change->physical);
                // FIXME: need a better way to change SPU id
                d_dvdsub->id=dvdsub_id=stream_change->physical;
                if (vo_spudec) spudec_reset(vo_spudec);
        }
#endif
        break;
        }
      case DVDNAV_AUDIO_STREAM_CHANGE: {
        int aid_temp;
#if DVDNAVVERSION > 012
        dvdnav_audio_stream_change_event_t *stream_change = (dvdnav_audio_stream_change_event_t*)(dvdnav_event->details);
#else
        dvdnav_stream_change_event_t *stream_change = (dvdnav_stream_change_event_t*)(dvdnav_event->details);
#endif

        printf("DVDNAV Event: Nav Audio Stream Change: phys: %d logical: %d\n",
                stream_change->physical,
                stream_change->logical);

        aid_temp=stream_change->physical;
        if (aid_temp>=0) aid_temp+=128; // FIXME: is this sane?
        if (d_audio && audio_id!=aid_temp) {
                mp_msg(MSGT_INPUT,MSGL_DBG2,"d_audio->id change: was %d is now %d\n",
                        d_audio->id,aid_temp);
                // FIXME: need a bettery way to change audio stream id
                d_audio->id=dvdsub_id=aid_temp;
                resync_audio_stream(sh_audio);
        }

        break;
      }
      case DVDNAV_VTS_CHANGE: {
        printf("DVDNAV Event: Nav VTS Change\n");
        break;
        }
      case DVDNAV_CELL_CHANGE: {
        dvdnav_cell_change_event_t *cell_change = (dvdnav_cell_change_event_t*)(dvdnav_event->details);
        cell_playback_t * cell_playback = cell_change->new_cell;

        printf("DVDNAV Event: Nav Cell Change\n");
        osd_show_dvd_nav_highlight=0; /* screen changed, disable menu */
        /*
        printf("new still time: %d\n",cell_playback->still_time);
        printf("new cell_cmd_nr: %d\n",cell_playback->cell_cmd_nr);
        printf("new playback_time: %02d:%02d:%02d.%02d\n",
                        cell_playback->playback_time.hour,
                        cell_playback->playback_time.minute,
                        cell_playback->playback_time.second,
                        cell_playback->playback_time.frame_u);

        */
        //rel_seek_secs=1; // not really: we can't seek, but it'll reset the muxer
        //abs_seek_pos=0;
        break;
        }
      case DVDNAV_NAV_PACKET: {
        // printf("DVDNAV Event: Nav Packet\n");
        break;
        }
      case DVDNAV_SPU_CLUT_CHANGE: {
        uint32_t * new_clut = (uint32_t *)(dvdnav_event->details);

        printf("DVDNAV Event: Nav SPU CLUT Change\n");
        // send new palette to SPU decoder
        if (vo_spudec) spudec_update_palette(vo_spudec,new_clut);

        break;
        }
      case DVDNAV_SEEK_DONE: {
        printf("DVDNAV Event: Nav Seek Done\n");
        break;
        }
      }

      // free the dvdnav event
      free(dvdnav_event->details);
      free(dvdnav_event);
      cmd->args[0].v.v=NULL;
    }
    case MP_CMD_DVDNAV: {
      dvdnav_priv_t * dvdnav_priv=(dvdnav_priv_t*)stream->priv;

      /* ignore these events if we're not in dvd_nav mode */
      if (!dvd_nav) break;

      switch (cmd->args[0].v.i) {
        case MP_CMD_DVDNAV_UP:
          dvdnav_upper_button_select(dvdnav_priv->dvdnav);
          break;
        case MP_CMD_DVDNAV_DOWN:
          dvdnav_lower_button_select(dvdnav_priv->dvdnav);
          break;
        case MP_CMD_DVDNAV_LEFT:
          dvdnav_left_button_select(dvdnav_priv->dvdnav);
          break;
        case MP_CMD_DVDNAV_RIGHT:
          dvdnav_right_button_select(dvdnav_priv->dvdnav);
          break;
        case MP_CMD_DVDNAV_MENU:
          printf("Menu call\n");
          dvdnav_menu_call(dvdnav_priv->dvdnav,DVD_MENU_Root);
          break;
        case MP_CMD_DVDNAV_SELECT:
          dvdnav_button_activate(dvdnav_priv->dvdnav);
          break;
        default:
          mp_msg(MSGT_CPLAYER, MSGL_V, "Weird DVD Nav cmd %d\n",cmd->args[0].v.i);
          break;
      }
      break;
    }
#endif
    default : {
#ifdef HAVE_NEW_GUI
      if ( ( use_gui )&&( cmd->id > MP_CMD_GUI_EVENTS ) ) guiGetEvent( guiIEvent,(char *)cmd->id );
       else
#endif
      mp_msg(MSGT_CPLAYER, MSGL_V, "Received unknow cmd %s\n",cmd->name);
    }
    }
    mp_cmd_free(cmd);
  }
}

  if (seek_to_sec) {
    int a,b; float d;
    
    if (sscanf(seek_to_sec, "%d:%d:%f", &a,&b,&d)==3)
	rel_seek_secs += 3600*a +60*b +d ;
    else if (sscanf(seek_to_sec, "%d:%f", &a, &d)==2)
	rel_seek_secs += 60*a +d;
    else if (sscanf(seek_to_sec, "%f", &d)==1)
	rel_seek_secs += d;

     seek_to_sec = NULL;
  }
  
  /* Looping. */
  if(eof==1 && loop_times>=0) {
    int l = loop_times;
    play_tree_iter_step(playtree_iter,0,0);
    loop_times = l;
    mp_msg(MSGT_CPLAYER,MSGL_V,"loop_times = %d, eof = %d\n", loop_times,eof);

    if(loop_times>1) loop_times--; else
    if(loop_times==1) loop_times=-1;

    eof=0;
    abs_seek_pos=3; rel_seek_secs=0; // seek to start of movie (0%)

  }

if(rel_seek_secs || abs_seek_pos){
  current_module="seek";
  if(demux_seek(demuxer,rel_seek_secs,abs_seek_pos)){
      // success:
      /* FIXME there should be real seeking for vobsub */
      if (vo_vobsub)
	vobsub_reset(vo_vobsub);
#if 0
      if(sh_video && d_video->packs == 0)
	ds_fill_buffer(d_video);
      if(sh_audio){
	if(d_audio->packs == 0)
	  ds_fill_buffer(d_audio);
	if(verbose){
	    float a_pts=d_audio->pts;
            a_pts+=(ds_tell_pts(d_audio)-sh_audio->a_in_buffer_len)/(float)sh_audio->i_bps;
	    mp_msg(MSGT_AVSYNC,MSGL_V,"SEEK: A: %5.3f  V: %5.3f  A-V: %5.3f   \n",a_pts,d_video->pts,a_pts-d_video->pts);
	}
        mp_msg(MSGT_AVSYNC,MSGL_STATUS,"A:%6.1f  V:%6.1f  A-V:%7.3f  ct: ?   \r",d_audio->pts,d_video->pts,0.0f);
      } else {
        mp_msg(MSGT_AVSYNC,MSGL_STATUS,"A: ---   V:%6.1f   \r",d_video->pts);
      }
#endif
      fflush(stdout);

      if(sh_video){
	 current_module="seek_video_reset";
         if(vo_config_count) video_out->control(VOCTRL_RESET,NULL);
      }
      
      if(sh_audio){
        current_module="seek_audio_reset";
        audio_out->reset(); // stop audio, throwing away buffered data
      }
#ifdef USE_OSD
        // Set OSD:
      if(osd_level){
        int len=((demuxer->movi_end-demuxer->movi_start)>>8);
        if (len>0){
	   osd_visible=sh_video->fps; // 1 sec
	   vo_osd_progbar_type=0;
	   vo_osd_progbar_value=(demuxer->filepos-demuxer->movi_start)/len;
	   vo_osd_changed(OSDTYPE_PROGBAR);
	}
      }
#endif
      if(sh_video) {
	c_total=0;
	max_pts_correction=0.1;
	osd_visible=sh_video->fps; // to rewert to PLAY pointer after 1 sec
	audio_time_usage=0; video_time_usage=0; vout_time_usage=0;
	drop_frame_cnt=0;
	too_slow_frame_cnt=0;
	too_fast_frame_cnt=0;

        if(vo_spudec) spudec_reset(vo_spudec);
      }
  }
  rel_seek_secs=0;
  abs_seek_pos=0;
  frame_time_remaining=0;
  current_module=NULL;
}

#ifdef HAVE_NEW_GUI
      if(use_gui){
        guiEventHandling();
	if(demuxer->file_format==DEMUXER_TYPE_AVI && sh_video->video.dwLength>2){
	  // get pos from frame number / total frames
	  guiIntfStruct.Position=(float)d_video->pack_no*100.0f/sh_video->video.dwLength;
	} else {
	 off_t len = ( demuxer->movi_end - demuxer->movi_start );
	 off_t pos = ( demuxer->file_format == DEMUXER_TYPE_AUDIO?stream->pos:demuxer->filepos );
	 guiIntfStruct.Position=(len <= 0? 0.0f : ( pos - demuxer->movi_start ) * 100.0f / len );
	}
	if ( sh_video ) guiIntfStruct.TimeSec=d_video->pts;
	  else if ( sh_audio ) guiIntfStruct.TimeSec=sh_audio->timer;
	guiGetEvent( guiReDraw,NULL );
	guiGetEvent( guiSetVolume,NULL );
	if(guiIntfStruct.Playing==0) break; // STOP
	if(guiIntfStruct.Playing==2) osd_function=OSD_PAUSE;
        if ( guiIntfStruct.DiskChanged || guiIntfStruct.FilenameChanged ) goto goto_next_file;
#ifdef USE_DVDREAD
        if ( stream->type == STREAMTYPE_DVD )
	 {
	  dvd_priv_t * dvdp = stream->priv;
	  guiIntfStruct.DVD.current_chapter=dvdp->cur_cell + 1;
	 }
#endif
      }
#endif


//================= Update OSD ====================
#ifdef USE_OSD
  if(osd_level>=1){
      int pts=d_video->pts;
      char osd_text_tmp[50];
      if(pts==osd_last_pts-1) ++pts; else osd_last_pts=pts;
      vo_osd_text=osd_text_buffer;
#ifdef USE_DVDNAV
      if (osd_show_dvd_nav_delay) {
          sprintf(osd_text_tmp, "DVDNAV: %s", dvd_nav_text);
          osd_show_dvd_nav_delay--;
      } else
#endif
      if (osd_show_sub_delay) {
	  sprintf(osd_text_tmp, "Sub delay: %d ms",(int)(sub_delay*1000));
	  osd_show_sub_delay--;
      } else
      if (osd_show_av_delay) {
	  sprintf(osd_text_tmp, "A-V delay: %d ms",(int)(audio_delay*1000));
	  osd_show_av_delay--;
      } else if(osd_level>=2)
          sprintf(osd_text_tmp,"%c %02d:%02d:%02d",osd_function,pts/3600,(pts/60)%60,pts%60);
      else osd_text_tmp[0]=0;
      
      if(strcmp(vo_osd_text, osd_text_tmp)) {
	      strcpy(vo_osd_text, osd_text_tmp);
	      vo_osd_changed(OSDTYPE_OSD);
      }
  } else {
      if(vo_osd_text) {
      vo_osd_text=NULL;
	  vo_osd_changed(OSDTYPE_OSD);
      }
  }
//  for(i=1;i<=11;i++) osd_text_buffer[10+i]=i;osd_text_buffer[10+i]=0;
//  vo_osd_text=osd_text_buffer;
#endif
  
#ifdef USE_SUB
  // find sub
  if(subtitles && d_video->pts>0){
      float pts=d_video->pts;
      if(sub_fps==0) sub_fps=sh_video->fps;
      current_module="find_sub";
      if (pts > sub_last_pts || pts < sub_last_pts-1.0 ) {
         find_sub(subtitles,sub_uses_time?(100*(pts+sub_delay)):((pts+sub_delay)*sub_fps)); // FIXME! frame counter...
         sub_last_pts = pts;
      }
      current_module=NULL;
  }
#endif
  
  // DVD sub:
if(vo_config_count && vo_spudec) {
  unsigned char* packet=NULL;
  int len,timestamp;
    // Get a sub packet from the dvd or a vobsub and make a timestamp relative to sh_video->timer
  int get_sub_packet(void) {
    // Vobsub
    len = 0;
    if(vo_vobsub) {
      if(d_video->pts+sub_delay>=0) {
	// The + next_frame_time is there because we'll display the sub at the next frame
	len = vobsub_get_packet(vo_vobsub,d_video->pts+sub_delay+next_frame_time,(void**)&packet,&timestamp);
	if(len > 0) {
	  timestamp -= (d_video->pts + sub_delay - sh_video->timer)*90000;
	  mp_dbg(MSGT_CPLAYER,MSGL_V,"\rVOB sub: len=%d v_pts=%5.3f v_timer=%5.3f sub=%5.3f ts=%d \n",len,d_video->pts,sh_video->timer,timestamp / 90000.0);
	}
      }
    } else {
      // DVD sub
      len = ds_get_packet_sub(d_dvdsub,(unsigned char**)&packet);
      if(len > 0) {
	timestamp = 90000*(sh_video->timer + d_dvdsub->pts + sub_delay - d_video->pts);
	mp_dbg(MSGT_CPLAYER,MSGL_V,"\rDVD sub: len=%d  v_pts=%5.3f  s_pts=%5.3f  ts=%d \n",len,d_video->pts,d_dvdsub->pts,timestamp);
      }
    }
    return len;
  }
  current_module="spudec";
  spudec_heartbeat(vo_spudec,90000*sh_video->timer);
  while(get_sub_packet()>0 && packet){
      if(timestamp < 0) timestamp = 0;
      spudec_assemble(vo_spudec,packet,len,timestamp);
  }
  
  /* detect wether the sub has changed or not */
  if(spudec_changed(vo_spudec))
    vo_osd_changed(OSDTYPE_SPU);
  current_module=NULL;
}
  
} // while(!eof)

mp_msg(MSGT_GLOBAL,MSGL_V,"EOF code: %d  \n",eof);

}

goto_next_file:  // don't jump here after ao/vo/getch initialization!

mp_msg(MSGT_CPLAYER,MSGL_INFO,"\n");

if(benchmark){
  double tot=video_time_usage+vout_time_usage+audio_time_usage;
  double total_time_usage;
  total_time_usage_start=GetTimer()-total_time_usage_start;
  total_time_usage = (float)total_time_usage_start*0.000001;
  mp_msg(MSGT_CPLAYER,MSGL_INFO,"\nBENCHMARKs: VC:%8.3fs VO:%8.3fs A:%8.3fs Sys:%8.3fs = %8.3fs\n",
	 video_time_usage,vout_time_usage,audio_time_usage,
	 total_time_usage-tot,total_time_usage);
  if(total_time_usage>0.0)
    mp_msg(MSGT_CPLAYER,MSGL_INFO,"BENCHMARK%%: VC:%8.4f%% VO:%8.4f%% A:%8.4f%% Sys:%8.4f%% = %8.4f%%\n",
	   100.0*video_time_usage/total_time_usage,
	   100.0*vout_time_usage/total_time_usage,
	   100.0*audio_time_usage/total_time_usage,
	   100.0*(total_time_usage-tot)/total_time_usage,
	   100.0);
  if(total_frame_cnt && frame_dropping)
    mp_msg(MSGT_CPLAYER,MSGL_INFO,"BENCHMARKn: disp: %d (%3.2f fps)  drop: %d (%d%%)  total: %d (%3.2f fps)\n",
	total_frame_cnt-drop_frame_cnt,
	(total_time_usage>0.5)?((total_frame_cnt-drop_frame_cnt)/total_time_usage):0,
	drop_frame_cnt,
	100*drop_frame_cnt/total_frame_cnt,
	total_frame_cnt,
	(total_time_usage>0.5)?(total_frame_cnt/total_time_usage):0);
  
}

// time to uninit all, except global stuff:
uninit_player(INITED_ALL-(INITED_GUI+INITED_LIRC+INITED_INPUT));

if(eof == PT_NEXT_ENTRY || eof == PT_PREV_ENTRY) {
  eof = eof == PT_NEXT_ENTRY ? 1 : -1;
  if(play_tree_iter_step(playtree_iter,eof,0) == PLAY_TREE_ITER_ENTRY) {
    eof = 1;
  } else {
    play_tree_iter_free(playtree_iter);
    playtree_iter = NULL;
  }
} else if (eof == PT_UP_NEXT || eof == PT_UP_PREV) {
  eof = eof == PT_UP_NEXT ? 1 : -1;
  if(play_tree_iter_up_step(playtree_iter,eof,0) == PLAY_TREE_ITER_ENTRY) {
    eof = 1;
  } else {
    play_tree_iter_free(playtree_iter);
    playtree_iter = NULL;
  }
} else { // NEXT PREV SRC
     eof = eof == PT_PREV_SRC ? -1 : 1;
}

if(eof == 0) eof = 1;

while(playtree_iter != NULL) {
  filename = play_tree_iter_get_file(playtree_iter,eof);
  if(filename == NULL) {
    if( play_tree_iter_step(playtree_iter,eof,0) != PLAY_TREE_ITER_ENTRY) {
      play_tree_iter_free(playtree_iter);
      playtree_iter = NULL;
    };
  } else
    break;
} 

#ifdef HAVE_NEW_GUI
 if( use_gui && !playtree_iter ) 
  {
#ifdef USE_DVDREAD
   if ( !guiIntfStruct.DiskChanged ) 
#endif
   mplEnd();
  }	
#endif

if(use_gui || playtree_iter != NULL){

#ifdef HAVE_FREETYPE
    current_module="uninit_font";
    if (vo_font) free_font_desc(vo_font);
    vo_font = NULL;
#endif

  current_module="uninit_acodec";
  if(sh_audio) uninit_audio(sh_audio);
  sh_audio=NULL;

  current_module="uninit_vcodec";
  if(sh_video) uninit_video(sh_video);
  sh_video=NULL;
 
  current_module="free_demuxer";
  if(demuxer) free_demuxer(demuxer);
  demuxer=NULL;

  current_module="free_stream";
  if(stream) free_stream(stream);
  stream=NULL;

#ifdef USE_SUB  
  current_module="sub_free";
  if ( subtitles ) 
   {
    sub_free( subtitles );
    if ( sub_name ) free( sub_name );
    sub_name=NULL;
    vo_sub=NULL;
    subtitles=NULL;
   }
#endif

  eof = 0;
  goto play_next_file;
}

#ifdef HAVE_FREETYPE
done_freetype();
#endif

exit_player(MSGTR_Exit_eof);

return 1;
}

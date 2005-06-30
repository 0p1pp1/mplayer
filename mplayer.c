#include <stdio.h>
#include <stdlib.h>
#include "config.h"

#ifdef WIN32
#define _UWIN 1  /*disable Non-underscored versions of non-ANSI functions as otherwise int eof would conflict with eof()*/
#include <windows.h>
#endif
#include <string.h>
#include <unistd.h>

// #include <sys/mman.h>
#include <sys/types.h>
#ifndef __MINGW32__
#include <sys/ioctl.h>
#include <sys/wait.h>
#else
#define	SIGHUP	1	/* hangup */
#define	SIGQUIT	3	/* quit */
#define	SIGKILL	9	/* kill (cannot be caught or ignored) */
#define	SIGBUS	10	/* bus error */
extern int mp_input_win32_slave_cmd_func(int fd,char* dest,int size);
#endif

#include <sys/time.h>
#include <sys/stat.h>

#include <signal.h>
#include <time.h>
#include <fcntl.h>
#include <limits.h>

#include <errno.h>

#include "version.h"

#include "mp_msg.h"

#define HELP_MP_DEFINE_STATIC
#include "help_mp.h"

#include "m_option.h"
#include "m_config.h"

#include "cfg-mplayer-def.h"

#ifdef USE_SUB
#include "subreader.h"
#endif

#include "libvo/video_out.h"

#include "libvo/font_load.h"
#include "libvo/sub.h"

#ifdef HAVE_X11
#include "libvo/x11_common.h"
#endif

#include "libao2/audio_out.h"

#include "codec-cfg.h"

#ifdef USE_DVDNAV
#include <dvdnav.h>
#endif

#ifdef USE_EDL
#include "edl.h"
#endif

#include "spudec.h"
#include "vobsub.h"

#include "osdep/getch2.h"
#include "osdep/timer.h"

#include "cpudetect.h"

#ifdef HAVE_NEW_GUI
#include "Gui/interface.h"
#endif

#include "input/input.h"

int slave_mode=0;
int verbose=0;
int identify=0;
int quiet=0;

#ifdef WIN32
char * proc_priority=NULL;
#endif

#define ABS(x) (((x)>=0)?(x):(-(x)))
#define ROUND(x) ((int)((x)<0 ? (x)-0.5 : (x)+0.5))

#ifdef HAVE_RTC
#ifdef __linux__
#include <linux/rtc.h>
#else
#include <rtc.h>
#define RTC_IRQP_SET RTCIO_IRQP_SET
#define RTC_PIE_ON   RTCIO_PIE_ON
#endif
#endif

#ifdef USE_TV
#include "libmpdemux/tv.h"
#endif

#ifdef HAS_DVBIN_SUPPORT
#include "libmpdemux/dvbin.h"
static int last_dvb_step = 1;
#endif

//**************************************************************************//
//             Playtree
//**************************************************************************//
#include "playtree.h"
#include "playtreeparser.h"

#ifdef HAVE_NEW_GUI
extern int import_playtree_playlist_into_gui(play_tree_t* my_playtree, m_config_t* config);
extern int import_initial_playtree_into_gui(play_tree_t* my_playtree, m_config_t* config, int enqueue);
#endif

play_tree_t* playtree;
play_tree_iter_t* playtree_iter = NULL;

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

extern play_tree_t*
m_config_parse_mp_command_line(m_config_t *config, int argc, char **argv);
extern int
m_config_parse_config_file(m_config_t* config, char *conffile);

//**************************************************************************//
//             Config file
//**************************************************************************//

static int cfg_inc_verbose(m_option_t *conf){ ++verbose; return 0;}

static int cfg_include(m_option_t *conf, char *filename){
	return m_config_parse_config_file(mconfig, filename);
}

#include "get_path.c"

//**************************************************************************//
//             XScreensaver
//**************************************************************************//

#ifdef HAVE_X11
void xscreensaver_heartbeat(void);
#endif

//**************************************************************************//
//**************************************************************************//
//             Input media streaming & demultiplexer:
//**************************************************************************//

static int max_framesize=0;

#include "libmpdemux/stream.h"
#include "libmpdemux/demuxer.h"
#include "libmpdemux/stheader.h"
//#include "parse_es.h"
#ifdef HAVE_MATROSKA
#include "libmpdemux/matroska.h"
#endif

#include "libmpcodecs/dec_audio.h"
#include "libmpcodecs/dec_video.h"
#include "libmpcodecs/mp_image.h"
#include "libmpcodecs/vf.h"

extern void vf_list_plugins();

//**************************************************************************//
//**************************************************************************//

// Common FIFO functions, and keyboard/event FIFO code
#include "fifo.c"
int noconsolecontrols=0;
//**************************************************************************//

vo_functions_t *video_out=NULL;
ao_functions_t *audio_out=NULL;

int fixed_vo=0;
int eof=0;

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

float playback_speed=1.0;

int use_gui=0;

#ifdef HAVE_NEW_GUI
int enqueue=0;
#endif

#define MAX_OSD_LEVEL 3

int osd_level=1;
int osd_level_saved=-1;
int osd_visible=100;

// seek:
static char *seek_to_sec=NULL;
static off_t seek_to_byte=0;
static off_t step_sec=0;
static int loop_times=-1;
static int loop_seek=0;

// A/V sync:
       int autosync=0; // 30 might be a good default value.

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
char* audio_lang=NULL;
char* dvdsub_lang=NULL;
static char* spudec_ifo=NULL;
char* filename=NULL; //"MI2-Trailer.avi";
int forced_subs_only=0;

// cache2:
       int stream_cache_size=-1;
#ifdef USE_STREAM_CACHE
extern int cache_fill_status;

float stream_cache_min_percent=20.0;
float stream_cache_prefill_percent=5.0;
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

       float force_fps=0;
static int force_srate=0;
static int audio_output_format=0;
       int frame_dropping=0; // option  0=no drop  1= drop vo  2= drop decode
static int play_n_frames=-1;
static int play_n_frames_mf=-1;

// screen info:
char** video_driver_list=NULL;
char** audio_driver_list=NULL;

extern char *vo_subdevice;
extern char *ao_subdevice;

// codec outfmt flags (defined in libmpcodecs/vd.c)
extern int vo_flags;

// sub:
char *font_name=NULL;
#ifdef HAVE_FONTCONFIG
extern int font_fontconfig;
#endif
float font_factor=0.75;
char **sub_name=NULL;
float sub_delay=0;
float sub_fps=0;
int   sub_auto = 1;
char *vobsub_name=NULL;
/*DSP!!char *dsp=NULL;*/
int   subcc_enabled=0;
int suboverlap_enabled = 1;
#ifdef USE_SUB
sub_data* set_of_subtitles[MAX_SUBTITLE_FILES];
int set_of_sub_size = 0;
int set_of_sub_pos = -1;
float sub_last_pts = -303;
#endif
int global_sub_size = 0; // this encompasses all subtitle sources
int global_sub_pos = -1; // this encompasses all subtitle sources
#define SUB_SOURCE_SUBS 0
#define SUB_SOURCE_VOBSUB 1
#define SUB_SOURCE_DEMUX 2
#define SUB_SOURCES 3
int global_sub_indices[SUB_SOURCES];
int global_sub_quiet_osd_hack = 0;

static stream_t* stream=NULL;
static demuxer_t *demuxer=NULL;
static sh_audio_t *sh_audio=NULL;
static sh_video_t *sh_video=NULL;

char* current_module=NULL; // for debugging

extern int vo_gamma_gamma;
extern int vo_gamma_brightness;
extern int vo_gamma_contrast;
extern int vo_gamma_saturation;
extern int vo_gamma_hue;

// ---

#ifdef HAVE_MENU
#include "m_struct.h"
#include "libmenu/menu.h"
extern void vf_menu_pause_update(struct vf_instance_s* vf);
extern vf_info_t vf_info_menu;
static vf_info_t* libmenu_vfs[] = {
  &vf_info_menu,
  NULL
};
static vf_instance_t* vf_menu = NULL;
static int use_menu = 0;
static char* menu_cfg = NULL;
static char* menu_root = "main";
#endif


#ifdef HAVE_RTC
static int nortc;
static char* rtc_device;
#endif

#ifdef USE_EDL
edl_record_ptr edl_records = NULL; ///< EDL entries memory area
edl_record_ptr next_edl_record = NULL; ///< only for traversing edl_records
short user_muted = 0; ///< Stores whether User wanted muted mode.
short edl_muted  = 0; ///< Stores whether EDL is currently in muted mode.
short edl_decision = 0; ///< 1 when an EDL operation has been made
FILE* edl_fd = NULL; ///< fd to write to when in -edlout mode
#endif

static unsigned int inited_flags=0;
#define INITED_VO 1
#define INITED_AO 2
#define INITED_GUI 4
#define INITED_GETCH2 8
#define INITED_SPUDEC 32
#define INITED_STREAM 64
#define INITED_INPUT    128
#define INITED_VOBSUB  256
#define INITED_DEMUXER 512
#define INITED_ACODEC  1024
#define INITED_VCODEC  2048
#define INITED_ALL 0xFFFF

static void uninit_player(unsigned int mask){
  mask=inited_flags&mask;

  mp_msg(MSGT_CPLAYER,MSGL_DBG2,"\n*** uninit(0x%X)\n",mask);

  if(mask&INITED_ACODEC){
    inited_flags&=~INITED_ACODEC;
    current_module="uninit_acodec";
    if(sh_audio) uninit_audio(sh_audio);
#ifdef HAVE_NEW_GUI
    guiGetEvent(guiSetAfilter, (char *)NULL);
#endif
    sh_audio=NULL;
  }

  if(mask&INITED_VCODEC){
    inited_flags&=~INITED_VCODEC;
    current_module="uninit_vcodec";
    if(sh_video) uninit_video(sh_video);
    sh_video=NULL;
#ifdef HAVE_MENU
    vf_menu=NULL;
#endif
  }
 
  if(mask&INITED_DEMUXER){
    inited_flags&=~INITED_DEMUXER;
    current_module="free_demuxer";
    if(demuxer){
	stream=demuxer->stream;
	free_demuxer(demuxer);
    }
    demuxer=NULL;
  }

  // kill the cache process:
  if(mask&INITED_STREAM){
    inited_flags&=~INITED_STREAM;
    current_module="uninit_stream";
    if(stream) free_stream(stream);
    stream=NULL;
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
    if(vo_vobsub) vobsub_close(vo_vobsub);
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
    audio_out->uninit(eof?0:1); audio_out=NULL;
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

static void exit_player_with_rc(char* how, int rc){

  uninit_player(INITED_ALL);
#ifdef HAVE_X11
#ifdef HAVE_NEW_GUI
  if ( !use_gui )
#endif
  vo_uninit();	// close the X11 connection (if any opened)
#endif

#ifdef HAVE_FREETYPE
  current_module="uninit_font";
  if (vo_font) free_font_desc(vo_font);
  vo_font = NULL;
  done_freetype();
#endif

  current_module="exit_player";

// free mplayer config
  free(mconfig);

#ifdef USE_EDL
  if(edl_records != NULL) free(edl_records); // free mem allocated for EDL
#endif
  if(how) mp_msg(MSGT_CPLAYER,MSGL_INFO,MSGTR_ExitingHow,mp_gettext(how));
  mp_msg(MSGT_CPLAYER,MSGL_DBG2,"max framesize was %d bytes\n",max_framesize);

  exit(rc);
}

void exit_player(char* how){
  exit_player_with_rc(how, 1);
}

#ifndef __MINGW32__
static void child_sighandler(int x){  
  pid_t pid;
  while((pid=waitpid(-1,NULL,WNOHANG)) > 0);
}
#endif

#ifdef CRASH_DEBUG
static char *prog_path;
static int crash_debug = 0;
#endif

static void exit_sighandler(int x){
  static int sig_count=0;
#ifdef CRASH_DEBUG
  if (!crash_debug || x != SIGTRAP)
#endif
  ++sig_count;
  if(inited_flags==0 && sig_count>1) exit(1);
  if(sig_count==5)
    {
      /* We're crashing bad and can't uninit cleanly :( 
       * by popular request, we make one last (dirty) 
       * effort to restore the user's 
       * terminal. */
      getch2_disable();
      exit(1);
    }
  if(sig_count==6) exit(1);
  if(sig_count>6){
    // can't stop :(
#ifndef __MINGW32__
    kill(getpid(),SIGKILL);
#endif
  }
  mp_msg(MSGT_CPLAYER,MSGL_FATAL,"\n" MSGTR_IntBySignal,x,
      current_module?current_module:mp_gettext("unknown")
  );
  if(sig_count<=1)
  switch(x){
  case SIGINT:
  case SIGQUIT:
  case SIGTERM:
  case SIGKILL:
      break;  // killed from keyboard (^C) or killed [-9]
  case SIGILL:
#ifdef RUNTIME_CPUDETECT
      mp_msg(MSGT_CPLAYER,MSGL_FATAL,MSGTR_Exit_SIGILL_RTCpuSel);
#else
      mp_msg(MSGT_CPLAYER,MSGL_FATAL,MSGTR_Exit_SIGILL);
#endif
  case SIGFPE:
  case SIGSEGV:
      mp_msg(MSGT_CPLAYER,MSGL_FATAL,MSGTR_Exit_SIGSEGV_SIGFPE);
  default:
      mp_msg(MSGT_CPLAYER,MSGL_FATAL,MSGTR_Exit_SIGCRASH);
#ifdef CRASH_DEBUG
      if (crash_debug) {
        int gdb_pid;
        char spid[20];
        snprintf(spid, 19, "%i", getpid());
        spid[19] = 0;
        mp_msg(MSGT_CPLAYER, MSGL_INFO, "Forking...\n");
        gdb_pid = fork();
        mp_msg(MSGT_CPLAYER, MSGL_INFO, "Forked...\n");
        if (gdb_pid == 0) { // We are the child
          if (execlp("gdb", "gdb", prog_path, spid, NULL) == -1)
            mp_msg(MSGT_CPLAYER, MSGL_ERR, "Couldn't start gdb\n");
        } else if (gdb_pid < 0) 
          mp_msg(MSGT_CPLAYER, MSGL_ERR, "Couldn't fork\n");
        else {
          waitpid(gdb_pid, NULL, 0);
        }
        if (x == SIGTRAP) return;
      }
#endif  
  }
  exit_player(NULL);
}

//extern void write_avi_header_1(FILE *f,int fcc,float fps,int width,int height);

extern void mp_input_register_options(m_config_t* cfg);

#include "mixer.h"
mixer_t mixer;
/// step size of mixer changes
int volstep = 3;

#include "cfg-mplayer.h"

void parse_cfgfiles( m_config_t* conf )
{
char *conffile;
int conffile_fd;
if (m_config_parse_config_file(conf, MPLAYER_CONFDIR "/mplayer.conf") < 0)
  exit_player(NULL);
if ((conffile = get_path("")) == NULL) {
  mp_msg(MSGT_CPLAYER,MSGL_WARN,MSGTR_NoHomeDir);
} else {
#ifdef __MINGW32__
  mkdir(conffile);
#else
  mkdir(conffile, 0777);
#endif
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
      exit_player(NULL);
    free(conffile);
  }
}
}

void load_per_file_config (m_config_t* conf, const char *const file)
{
    char *confpath;
    char cfg[strlen(file)+10];
    struct stat st;
    char *name;

    sprintf (cfg, "%s.conf", file);
    
    if (!stat (cfg, &st))
    {
	mp_msg(MSGT_CPLAYER,MSGL_INFO,MSGTR_LoadingConfig, cfg);
	m_config_parse_config_file (conf, cfg);
	return;
    }

    if ((name = strrchr (cfg, '/')) == NULL)
	name = cfg;
    else
	name++;

    if ((confpath = get_path (name)) != NULL)
    {
	if (!stat (confpath, &st))
	{
	    mp_msg(MSGT_CPLAYER,MSGL_INFO,MSGTR_LoadingConfig, confpath);
	    m_config_parse_config_file (conf, confpath);
	}

	free (confpath);
    }
}

// When libmpdemux perform a blocking operation (network connection or cache filling)
// if the operation fail we use this function to check if it was interrupted by the user.
// The function return a new value for eof.
static int libmpdemux_was_interrupted(int eof) {
  mp_cmd_t* cmd;
  if((cmd = mp_input_get_cmd(0,0,0)) != NULL) {
       switch(cmd->id) {
       case MP_CMD_QUIT:
	 exit_player_with_rc(MSGTR_Exit_quit, (cmd->nargs > 0)? cmd->args[0].v.i : 0);
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

#define mp_basename2(s) (strrchr(s,'/')==NULL?(char*)s:(strrchr(s,'/')+1))
#define mp_basename(s) (strrchr(s,'\\')==NULL?(mp_basename2(s)):(strrchr(s,'\\')+1))

int playtree_add_playlist(play_tree_t* entry)
{
  play_tree_add_bpf(entry,filename);

#ifdef HAVE_NEW_GUI
  if (use_gui) {
    if (entry) {
      import_playtree_playlist_into_gui(entry, mconfig);
      play_tree_free_list(entry,1);
    }
  } else
#endif
  {
  if(!entry) {      
    entry = playtree_iter->tree;
    if(play_tree_iter_step(playtree_iter,1,0) != PLAY_TREE_ITER_ENTRY) {
        return PT_NEXT_ENTRY;
    }
    if(playtree_iter->tree == entry ) { // Loop with a single file
      if(play_tree_iter_up_step(playtree_iter,1,0) != PLAY_TREE_ITER_ENTRY) {
	return PT_NEXT_ENTRY;
      }
    }
    play_tree_remove(entry,1,1);
    return PT_NEXT_SRC;
  }
  play_tree_insert_entry(playtree_iter->tree,entry);
  play_tree_set_params_from(entry,playtree_iter->tree);
  entry = playtree_iter->tree;
  if(play_tree_iter_step(playtree_iter,1,0) != PLAY_TREE_ITER_ENTRY) {
    return PT_NEXT_ENTRY;
  }      
  play_tree_remove(entry,1,1);
  }
  return PT_NEXT_SRC;
}

static int play_tree_step = 1;

int sub_source()
{
    int source = -1;
    int top = -1;
    int i;
    for (i = 0; i < SUB_SOURCES; i++) {
        int j = global_sub_indices[i];
        if ((j >= 0) && (j > top) && (global_sub_pos >= j)) {
            source = i;
            top = j;
        }
    }
    return source;
}

#ifdef USE_SUB

sub_data* subdata = NULL;
static subtitle* vo_sub_last = NULL;

void add_subtitles(char *filename, float fps, int silent)
{
    sub_data *subd;

    if (filename == NULL) {
	return;
    }

    subd = sub_read_file(filename, fps);
    if(!subd && !silent) 
        mp_msg(MSGT_CPLAYER, MSGL_ERR, MSGTR_CantLoadSub, filename);
    if (subd == NULL || set_of_sub_size >= MAX_SUBTITLE_FILES) return;
    set_of_subtitles[set_of_sub_size] = subd;
    if (identify)
    {
      mp_msg(MSGT_GLOBAL, MSGL_INFO, "ID_FILE_SUB_ID=%d\n", set_of_sub_size);
      mp_msg(MSGT_GLOBAL, MSGL_INFO, "ID_FILE_SUB_FILENAME=%s\n", filename);
    }
    ++set_of_sub_size;
    mp_msg(MSGT_CPLAYER, MSGL_INFO, MSGTR_AddedSubtitleFile, set_of_sub_size, filename);
}

// FIXME: if/when the GUI calls this, global sub numbering gets (potentially) broken.
void update_set_of_subtitles()
    // subdata was changed, set_of_sub... have to be updated.
{
    int i;
    if (set_of_sub_size > 0 && subdata == NULL) { // *subdata was deleted
        for (i = set_of_sub_pos + 1; i < set_of_sub_size; ++i)
            set_of_subtitles[i-1] = set_of_subtitles[i];
        set_of_subtitles[set_of_sub_size-1] = NULL;
        --set_of_sub_size;
        if (set_of_sub_size > 0) subdata = set_of_subtitles[set_of_sub_pos=0];
    }
    else if (set_of_sub_size > 0 && subdata != NULL) { // *subdata was changed
        set_of_subtitles[set_of_sub_pos] = subdata;
    }
    else if (set_of_sub_size <= 0 && subdata != NULL) { // *subdata was added
        set_of_subtitles[set_of_sub_pos=set_of_sub_size] = subdata;
        ++set_of_sub_size;
    }
}
#endif

/*
 * In Mac OS X the SDL-lib is built upon Cocoa. The easiest way to
 * make it all work is to use the builtin SDL-bootstrap code, which 
 * will be done automatically by replacing our main() if we include SDL.h.
 */
#if defined(SYS_DARWIN) && defined(HAVE_SDL)
#include <SDL.h>
#endif

/**
 * \brief append a formatted string
 * \param buf buffer to print into
 * \param pos position of terminating 0 in buf
 * \param len maximum number of characters in buf, not including terminating 0
 * \param format printf format string
 */
static void saddf(char *buf, unsigned *pos, int len, const char *format, ...)
{
  va_list va;
  va_start(va, format);
  *pos += vsnprintf(&buf[*pos], len - *pos, format, va);
  va_end(va);
  if (*pos >= len ) {
    buf[len] = 0;
    *pos = len;
  }
}

/**
 * \brief print the status line
 * \param a_pos audio position
 * \param a_v A-V desynchronization
 * \param corr amount out A-V synchronization
 */
static void print_status(float a_pos, float a_v, float corr)
{
  int width;
  char *line;
  unsigned pos = 0;
  get_screen_size();
  if (screen_width > 0)
    width = screen_width;
  else
  width = 80;
#ifdef WIN32
  // windows command line is broken (MinGW's rxvt works though, but we
  // should not depend on that).
  width--;
#endif
  line = malloc(width + 1); // one additional for terminating null
  
  // Audio time
  if (sh_audio) {
    saddf(line, &pos, width, "A:%6.1f ", a_pos);
    if (!sh_video) {
      // convert time to HH:MM:SS.F format
      long tenths = 10 * a_pos;
      int f1 = tenths % 10;
      int ss = (tenths /  10) % 60;
      int mm = (tenths / 600) % 60;
      int hh = (tenths / 36000) % 100;
      saddf(line, &pos, width, "(");
      if (hh > 0)
        saddf(line, &pos, width, "%2d:", hh);
      if (hh > 0 || mm > 0)
        saddf(line, &pos, width, "%02d:", mm);
      saddf(line, &pos, width, "%02d.", ss);
      saddf(line, &pos, width, "%1d", f1);
      saddf(line, &pos, width, ") ");
    }
  }

  // Video time
  if (sh_video)
    saddf(line, &pos, width, "V:%6.1f ", sh_video->pts);

  // A-V sync
  if (sh_audio && sh_video)
    saddf(line, &pos, width, "A-V:%7.3f ct:%7.3f ", a_v, corr);

  // Video stats
  if (sh_video)
    saddf(line, &pos, width, "%3d/%3d ",
      (int)sh_video->num_frames,
      (int)sh_video->num_frames_decoded);

  // CPU usage
  if (sh_video) {
    if (sh_video->timer > 0.5)
      saddf(line, &pos, width, "%2d%% %2d%% %4.1f%% ",
        (int)(100.0*video_time_usage*playback_speed/(double)sh_video->timer),
        (int)(100.0*vout_time_usage*playback_speed/(double)sh_video->timer),
        (100.0*audio_time_usage*playback_speed/(double)sh_video->timer));
    else
      saddf(line, &pos, width, "??%% ??%% ??,?%% ");
  } else if (sh_audio) {
    if (sh_audio->delay > 0.5)
      saddf(line, &pos, width, "%4.1f%% ",
        100.0*audio_time_usage/(double)sh_audio->delay);
    else
      saddf(line, &pos, width, "??,?%% ");
  }

  // VO stats
  if (sh_video) 
    saddf(line, &pos, width, "%d %d ", drop_frame_cnt, output_quality);

#ifdef USE_STREAM_CACHE
  // cache stats
  if (stream_cache_size > 0)
    saddf(line, &pos, width, "%d%% ", cache_fill_status);
#endif

  // other
  if (playback_speed != 1)
    saddf(line, &pos, width, "%4.2fx ", playback_speed);

  // end
  memset(&line[pos], ' ', width - pos);
  line[width] = 0;
  mp_msg(MSGT_AVSYNC, MSGL_STATUS, "%s\r", line);
  free(line);
}

/**
 * \brief build a chain of audio filters that converts the input format
 * to the ao's format, taking into account the current playback_speed.
 * \param sh_audio describes the requested input format of the chain.
 * \param ao_data describes the requested output format of the chain.
 */
static int build_afilter_chain(sh_audio_t *sh_audio, ao_data_t *ao_data)
{
  int new_srate;
  int result;
  if (!sh_audio)
  {
#ifdef HAVE_NEW_GUI
    guiGetEvent(guiSetAfilter, (char *)NULL);
#endif
    mixer.afilter = NULL;
    return 0;
  }
  new_srate = sh_audio->samplerate * playback_speed;
  if (new_srate != ao_data->samplerate) {
    // limits are taken from libaf/af_resample.c
    if (new_srate < 8000)
      new_srate = 8000;
    if (new_srate > 192000)
      new_srate = 192000;
    playback_speed = (float)new_srate / (float)sh_audio->samplerate;
  }
  result =  init_audio_filters(sh_audio, new_srate,
           sh_audio->channels, sh_audio->sample_format,
           &ao_data->samplerate, &ao_data->channels, &ao_data->format,
           ao_data->outburst * 4, ao_data->buffersize);
  mixer.afilter = sh_audio->afilter;
#ifdef HAVE_NEW_GUI
  guiGetEvent(guiSetAfilter, (char *)sh_audio->afilter);
#endif
  return result;
}

#ifdef USE_SUB
/**
 * \brief Log the currently displayed subtitle to a file
 * 
 * Logs the current or last displayed subtitle together with filename
 * and time information to ~/.mplayer/subtitle_log
 *
 * Intended purpose is to allow convenient marking of bogus subtitles
 * which need to be fixed while watching the movie.
 */

static void log_sub(){
    char *fname;
    FILE *f;
    int i;

    if (subdata == NULL || vo_sub_last == NULL) return;
    fname = get_path("subtitle_log");
    f = fopen(fname, "a");
    if (!f) return;
    fprintf(f, "----------------------------------------------------------\n");
    if (subdata->sub_uses_time) {
	fprintf(f, "N: %s S: %02d:%02d:%02d.%02d E: %02d:%02d:%02d.%02d\n", filename, 
		vo_sub_last->start/360000, (vo_sub_last->start/6000)%60,
		(vo_sub_last->start/100)%60, vo_sub_last->start%100,
		vo_sub_last->end/360000, (vo_sub_last->end/6000)%60,
		(vo_sub_last->end/100)%60, vo_sub_last->end%100);
    } else {
	fprintf(f, "N: %s S: %d E: %d\n", filename, vo_sub_last->start, vo_sub_last->end);
    }
    for (i = 0; i < vo_sub_last->lines; i++) {
	fprintf(f, "%s\n", vo_sub_last->text[i]);
    }
    fclose(f);
}
#endif

int main(int argc,char* argv[]){


char * mem_ptr;

static demux_stream_t *d_audio=NULL;
static demux_stream_t *d_video=NULL;
static demux_stream_t *d_dvdsub=NULL;

int file_format=DEMUXER_TYPE_UNKNOWN;

int delay_corrected=1;

// movie info:

int osd_function=OSD_PLAY;
int osd_last_pts=-303;
int osd_show_av_delay = 0;
int osd_show_text = 0;
int osd_show_speed = 0;
int osd_show_sub_delay = 0;
int osd_show_sub_pos = 0;
int osd_show_sub_visibility = 0;
int osd_show_sub_alignment = 0;
int osd_show_vobsub_changed = 0;
int osd_show_sub_changed = 0;
int osd_show_percentage = 0;
int osd_show_tv_channel = 25;
int osd_show_ontop = 0;
int osd_show_rootwin = 0;
int osd_show_framedropping = 0;
int osd_show_status = 0;

int rtc_fd=-1;

//float a_frame=0;    // Audio

int i;
char *tmp;

int gui_no_filename=0;


  srand((int) time(NULL)); 

  InitTimer();
  
  mp_msg_init();
  mp_msg_set_level(MSGL_STATUS);

  mp_msg(MSGT_CPLAYER,MSGL_INFO, "MPlayer " VERSION " (C) 2000-2005 MPlayer Team\n");
  /* Test for cpu capabilities (and corresponding OS support) for optimizing */
  GetCpuCaps(&gCpuCaps);
#if defined(ARCH_X86) || defined(ARCH_X86_64)
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

#if defined(WIN32) && defined(USE_WIN32DLL)
{  /*make our codec dirs available for LoadLibraryA()*/
  char tmppath[MAX_PATH*2 + 1];
  char win32path[MAX_PATH];
  char realpath[MAX_PATH];
#ifdef __CYGWIN__
  cygwin_conv_to_full_win32_path(WIN32_PATH,win32path);
  strcpy(tmppath,win32path);
#ifdef USE_REALCODECS
  cygwin_conv_to_full_win32_path(REALCODEC_PATH,realpath);
  sprintf(tmppath,"%s;%s",win32path,realpath);
#endif /*USE_REALCODECS*/
#else
  if(!strstr(WIN32_PATH,":")){
    GetModuleFileNameA(NULL, win32path, MAX_PATH);
    strcpy(win32path + strlen(win32path) - strlen("mplayer.exe"), WIN32_PATH);
  }
  else strcpy(win32path,WIN32_PATH);
  strcpy(tmppath,win32path);
#ifdef USE_REALCODECS
  if(!strstr(REALCODEC_PATH,":")){
    GetModuleFileNameA(NULL, realpath, MAX_PATH);
    strcpy(realpath + strlen(realpath) - strlen("mplayer.exe"), REALCODEC_PATH);
  }
  else strcpy(realpath,REALCODEC_PATH);
  sprintf(tmppath,"%s;%s",win32path,realpath);
#endif /*USE_REALCODECS*/
#endif /*__CYGWIN__*/
  SetEnvironmentVariableA("PATH", tmppath);
}
#endif /*WIN32 && USE_WIN32DLL*/

#ifdef USE_TV
  tv_param_immediate = 1;
#endif

  if ( argv[0] )
    if(!strcmp(argv[0],"gmplayer") ||
      (strrchr(argv[0],'/') && !strcmp(strrchr(argv[0],'/'),"/gmplayer") ) )
          use_gui=1;

    mconfig = m_config_new();
    m_config_register_options(mconfig,mplayer_opts);
    // TODO : add something to let modules register their options
    mp_input_register_options(mconfig);
    parse_cfgfiles(mconfig);

#ifdef HAVE_NEW_GUI
    if ( use_gui ) cfg_read();
#endif

    playtree = m_config_parse_mp_command_line(mconfig, argc, argv);
    if(playtree == NULL)
      exit_player(NULL);

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
	
#ifdef WIN32
	if(proc_priority){
		int i;
        	for(i=0; priority_presets_defs[i].name; i++){
        		if(strcasecmp(priority_presets_defs[i].name, proc_priority) == 0)
				break;
		}
		mp_msg(MSGT_CPLAYER,MSGL_STATUS,"Setting process priority: %s\n",
				priority_presets_defs[i].name);
		SetPriorityClass(GetCurrentProcess(), priority_presets_defs[i].prio);
	}
#endif	
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
    if (use_gui && playtree_iter){
      char cwd[PATH_MAX+2];
      // Remove Playtree and Playtree-Iter from memory as its not used by gui
      play_tree_iter_free(playtree_iter);
      playtree_iter=NULL;
      
      if (getcwd(cwd, PATH_MAX) != (char *)NULL)
      {
	  strcat(cwd, "/");
          // Prefix relative paths with current working directory
          play_tree_add_bpf(playtree, cwd);
      }      
      // Import initital playtree into gui
      import_initial_playtree_into_gui(playtree, mconfig, enqueue);
    }
#endif

    if(video_driver_list && strcmp(video_driver_list[0],"help")==0){
      list_video_out();
      exit_player_with_rc(NULL, 0);
    }

    if(audio_driver_list && strcmp(audio_driver_list[0],"help")==0){
      list_audio_out();
      exit_player_with_rc(NULL, 0);
    }

// check codec.conf
if(!codecs_file || !parse_codec_cfg(codecs_file)){
  if(!parse_codec_cfg(mem_ptr=get_path("codecs.conf"))){
    if(!parse_codec_cfg(MPLAYER_CONFDIR "/codecs.conf")){
      if(!parse_codec_cfg(NULL)){
	mp_msg(MSGT_CPLAYER,MSGL_HINT,MSGTR_CopyCodecsConf);
        exit_player_with_rc(NULL, 0);
      }
      mp_msg(MSGT_CPLAYER,MSGL_V,MSGTR_BuiltinCodecsConf);
    }
  }
  free( mem_ptr ); // release the buffer created by get_path()
}

#if 0
    if(video_codec_list){
	int i;
	video_codec=video_codec_list[0];
	for(i=0;video_codec_list[i];i++)
	    mp_msg(MSGT_FIXME,MSGL_FIXME,"vc#%d: '%s'\n",i,video_codec_list[i]);
    }
#endif
    if(audio_codec_list && strcmp(audio_codec_list[0],"help")==0){
      mp_msg(MSGT_CPLAYER, MSGL_INFO, MSGTR_AvailableAudioCodecs);
      if (identify)
        mp_msg(MSGT_GLOBAL, MSGL_INFO, "ID_AUDIO_CODECS\n");
      list_codecs(1);
      mp_msg(MSGT_FIXME, MSGL_FIXME, "\n");
      exit_player_with_rc(NULL, 0);
    }
    if(video_codec_list && strcmp(video_codec_list[0],"help")==0){
      mp_msg(MSGT_CPLAYER, MSGL_INFO, MSGTR_AvailableVideoCodecs);
      if (identify)
        mp_msg(MSGT_GLOBAL, MSGL_INFO, "ID_VIDEO_CODECS\n");
      list_codecs(0);
      mp_msg(MSGT_FIXME, MSGL_FIXME, "\n");
      exit_player_with_rc(NULL, 0);
    }
    if(video_fm_list && strcmp(video_fm_list[0],"help")==0){
      vfm_help();
      mp_msg(MSGT_FIXME, MSGL_FIXME, "\n");
      exit_player_with_rc(NULL, 0);
    }
    if(audio_fm_list && strcmp(audio_fm_list[0],"help")==0){
      afm_help();
      mp_msg(MSGT_FIXME, MSGL_FIXME, "\n");
      exit_player_with_rc(NULL, 0);
    }
    if(af_cfg.list && strcmp(af_cfg.list[0],"help")==0){
      af_help();
      printf("\n");
      exit_player_with_rc(NULL, 0);
    }
#ifdef HAVE_X11
    if(vo_fstype_list && strcmp(vo_fstype_list[0],"help")==0){
      fstype_help();
      mp_msg(MSGT_FIXME, MSGL_FIXME, "\n");
      exit_player_with_rc(NULL, 0);
    }
#endif

#ifdef USE_EDL
if (edl_check_mode() == EDL_ERROR && edl_filename)
{
    mp_msg(MSGT_CPLAYER, MSGL_ERR, MSGTR_EdlCantUseBothModes);
    exit_player(NULL);
} else if (edl_filename)
{
    if (edl_records) free_edl(edl_records);
    next_edl_record = edl_records = edl_parse_file();
} else if (edl_output_filename)
{
    if ((edl_fd = fopen(edl_output_filename, "w")) == NULL)
    {
        mp_msg(MSGT_CPLAYER, MSGL_ERR, MSGTR_EdlCantOpenForWrite,
               edl_output_filename);
        exit_player(NULL);
    }
}
#endif

    if(!filename){
      if(!use_gui){
	// no file/vcd/dvd -> show HELP:
	mp_msg(MSGT_CPLAYER, MSGL_INFO, help_text);
        exit_player_with_rc(NULL, 0);
      } else gui_no_filename=1;
    }

    // Many users forget to include command line in bugreports...
    if(verbose>0){
      mp_msg(MSGT_CPLAYER, MSGL_INFO, MSGTR_CommandLine);
      for(i=1;i<argc;i++)mp_msg(MSGT_CPLAYER, MSGL_INFO," '%s'",argv[i]);
      mp_msg(MSGT_CPLAYER, MSGL_INFO, "\n");
    }

    mp_msg_set_level(verbose+MSGL_STATUS);

//------ load global data first ------

// check font
#ifdef USE_OSD
#ifdef HAVE_FREETYPE
  init_freetype();
#endif
#ifdef HAVE_FONTCONFIG
  if(!font_fontconfig)
  {
#endif
  if(font_name){
       vo_font=read_font_desc(font_name,font_factor,verbose>1);
       if(!vo_font) mp_msg(MSGT_CPLAYER,MSGL_ERR,MSGTR_CantLoadFont,font_name);
  } else {
      // try default:
       vo_font=read_font_desc( mem_ptr=get_path("font/font.desc"),font_factor,verbose>1);
       free(mem_ptr); // release the buffer created by get_path()
       if(!vo_font)
       vo_font=read_font_desc(MPLAYER_DATADIR "/font/font.desc",font_factor,verbose>1);
  }
#ifdef HAVE_FONTCONFIG
  }
#endif
#endif
  vo_init_osd();

#ifdef HAVE_RTC
  if(!nortc)
  {
    // seteuid(0); /* Can't hurt to try to get root here */
    if ((rtc_fd = open(rtc_device ? rtc_device : "/dev/rtc", O_RDONLY)) < 0)
	mp_msg(MSGT_CPLAYER, MSGL_WARN, MSGTR_RTCDeviceNotOpenable,
	    rtc_device ? rtc_device : "/dev/rtc", strerror(errno));
     else {
	unsigned long irqp = 1024; /* 512 seemed OK. 128 is jerky. */

	if (ioctl(rtc_fd, RTC_IRQP_SET, irqp) < 0) {
    	    mp_msg(MSGT_CPLAYER, MSGL_WARN, MSGTR_LinuxRTCInitErrorIrqpSet, irqp, strerror(errno));
    	    mp_msg(MSGT_CPLAYER, MSGL_HINT, MSGTR_IncreaseRTCMaxUserFreq, irqp);
   	    close (rtc_fd);
    	    rtc_fd = -1;
	} else if (ioctl(rtc_fd, RTC_PIE_ON, 0) < 0) {
	    /* variable only by the root */
    	    mp_msg(MSGT_CPLAYER, MSGL_ERR, MSGTR_LinuxRTCInitErrorPieOn, strerror(errno));
    	    close (rtc_fd);
	    rtc_fd = -1;
	} else
	    mp_msg(MSGT_CPLAYER, MSGL_V, MSGTR_UsingRTCTiming, irqp);
    }
  }
#ifdef HAVE_NEW_GUI
// breaks DGA and SVGAlib and VESA drivers:  --A'rpi
// and now ? -- Pontscho
    if(use_gui) setuid( getuid() ); // strongly test, please check this.
#endif
    if(rtc_fd<0)
#endif
      mp_msg(MSGT_CPLAYER, MSGL_V, "Using %s timing\n",
	     softsleep?"software":timer_name);

#ifdef USE_TERMCAP
  if ( !use_gui ) load_termcap(NULL); // load key-codes
#endif

// ========== Init keyboard FIFO (connection to libvo) ============

// Init input system
current_module = "init_input";
mp_input_init();
#if 0
make_pipe(&keyb_fifo_get,&keyb_fifo_put);

if(keyb_fifo_get > 0)
  mp_input_add_key_fd(keyb_fifo_get,1,NULL,NULL);
#else
  mp_input_add_key_fd(-1,0,mplayer_get_key,NULL);
#endif
if(slave_mode)
#ifndef __MINGW32__
   mp_input_add_cmd_fd(0,1,NULL,NULL);
#else
  mp_input_add_cmd_fd(0,0,mp_input_win32_slave_cmd_func,NULL);
#endif
else if(!noconsolecontrols)
#ifndef HAVE_NO_POSIX_SELECT
  mp_input_add_key_fd(0,1,NULL,NULL);
#else
  mp_input_add_key_fd(0,0,NULL,NULL);
#endif

inited_flags|=INITED_INPUT;
current_module = NULL;

#ifdef HAVE_MENU
 if(use_menu) {
   if(menu_cfg && menu_init(menu_cfg))
     mp_msg(MSGT_CPLAYER,MSGL_INFO,MSGTR_MenuInitialized, menu_cfg);
   else {
     menu_cfg = get_path("menu.conf");
     if(menu_init(menu_cfg))
       mp_msg(MSGT_CPLAYER,MSGL_INFO,MSGTR_MenuInitialized, menu_cfg);
     else {
       if(menu_init(MPLAYER_CONFDIR "/menu.conf"))
         mp_msg(MSGT_CPLAYER,MSGL_INFO,MSGTR_MenuInitialized, MPLAYER_CONFDIR"/menu.conf");
       else {
         mp_msg(MSGT_CPLAYER,MSGL_INFO,MSGTR_MenuInitFailed);
         use_menu = 0;
       }
     }
   }
 }
#endif
  
  /// Catch signals
#ifndef __MINGW32__
  signal(SIGCHLD,child_sighandler);
#endif

#ifdef CRASH_DEBUG
  prog_path = argv[0];
#endif
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
#ifdef CRASH_DEBUG
  if (crash_debug)
    signal(SIGTRAP,exit_sighandler);
#endif
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

  // init global sub numbers
  global_sub_size = 0;
  { int i; for (i = 0; i < SUB_SOURCES; i++) global_sub_indices[i] = -1; }
  global_sub_quiet_osd_hack = 1;

  if (filename) load_per_file_config (mconfig, filename);

// We must enable getch2 here to be able to interrupt network connection
// or cache filling
if(!noconsolecontrols && !slave_mode){
  if(inited_flags&INITED_GETCH2)
    mp_msg(MSGT_CPLAYER,MSGL_WARN,MSGTR_Getch2InitializedTwice);
  else
    getch2_enable();  // prepare stdin for hotkeys...
  inited_flags|=INITED_GETCH2;
  mp_msg(MSGT_CPLAYER,MSGL_DBG2,"\n[[[init getch2]]]\n");
}

// =================== GUI idle loop (STOP state) ===========================
#ifdef HAVE_NEW_GUI
    if ( use_gui ) {
      file_format=DEMUXER_TYPE_UNKNOWN;
      guiGetEvent( guiSetDefaults,0 );
      while ( guiIntfStruct.Playing != 1 )
       {
        mp_cmd_t* cmd;                                                                                   
	usec_sleep(20000);
	guiEventHandling();
	guiGetEvent( guiReDraw,NULL );
	if ( (cmd = mp_input_get_cmd(0,0,0)) != NULL) guiGetEvent( guiIEvent,(char *)cmd->id );
       } 
      guiGetEvent( guiSetParameters,NULL );
      if ( guiIntfStruct.StreamType == STREAMTYPE_STREAM )
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
       } 
    }
#endif
//---------------------------------------------------------------------------

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
    if(vo_vobsub){
      inited_flags|=INITED_VOBSUB;
      vobsub_set_from_lang(vo_vobsub, dvdsub_lang);
      // check if vobsub requested only to display forced subtitles
      forced_subs_only=vobsub_get_forced_subs_flag(vo_vobsub);

      // setup global sub numbering
      global_sub_indices[SUB_SOURCE_VOBSUB] = global_sub_size; // the global # of the first vobsub.
      global_sub_size += vobsub_get_indexes_count(vo_vobsub);
    }

//============ Open & Sync STREAM --- fork cache2 ====================

  stream=NULL;
  demuxer=NULL;
  if (d_audio) {
    //free_demuxer_stream(d_audio);
    d_audio=NULL;
  }
  if (d_video) {
    //free_demuxer_stream(d_video);
    d_video=NULL;
  }
  sh_audio=NULL;
  sh_video=NULL;

  current_module="open_stream";
  stream=open_stream(filename,0,&file_format);
  if(!stream) { // error...
    eof = libmpdemux_was_interrupted(PT_NEXT_ENTRY);
    goto goto_next_file;
  }
  inited_flags|=INITED_STREAM;

#ifdef HAVE_NEW_GUI
  if ( use_gui ) guiGetEvent( guiSetStream,(char *)stream );
#endif

  if(file_format == DEMUXER_TYPE_PLAYLIST) {
    play_tree_t* entry;
    // Handle playlist
    current_module="handle_playlist";
    mp_msg(MSGT_CPLAYER,MSGL_V,"Parsing playlist %s...\n",filename);
    entry = parse_playtree(stream,0);
    eof=playtree_add_playlist(entry);
    goto goto_next_file;
  }
  stream->start_pos+=seek_to_byte;

if(stream_dump_type==5){
  unsigned char buf[4096];
  int len;
  FILE *f;
  current_module="dumpstream";
  if(stream->type==STREAMTYPE_STREAM && stream->fd<0){
    mp_msg(MSGT_CPLAYER,MSGL_FATAL,MSGTR_DumpstreamFdUnavailable);
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
      if(len>0) {
        if(fwrite(buf,len,1,f) != 1) {
          mp_msg(MSGT_MENCODER,MSGL_FATAL,MSGTR_ErrorWritingFile,stream_dump_name);
          exit_player(MSGTR_Exit_error);
        }
      }
  }
  if(fclose(f)) {
    mp_msg(MSGT_MENCODER,MSGL_FATAL,MSGTR_ErrorWritingFile,stream_dump_name);
    exit_player(MSGTR_Exit_error);
  }
  mp_msg(MSGT_CPLAYER,MSGL_INFO,MSGTR_CoreDumped);
  exit_player_with_rc(MSGTR_Exit_eof, 0);
}

#ifdef USE_DVDREAD
if(stream->type==STREAMTYPE_DVD){
  current_module="dvd lang->id";
  if(audio_id==-1) audio_id=dvd_aid_from_lang(stream,audio_lang);
  if(dvdsub_lang && dvdsub_id==-1) dvdsub_id=dvd_sid_from_lang(stream,dvdsub_lang);
  // setup global sub numbering
  global_sub_indices[SUB_SOURCE_DEMUX] = global_sub_size; // the global # of the first demux-specific sub.
  global_sub_size += dvd_number_of_subs(stream);
  current_module=NULL;
}
#endif

#ifdef USE_DVDNAV
  if (stream->type==STREAMTYPE_DVDNAV) stream_cache_size=0;	// must disable caching...
#endif

// CACHE2: initial prefill: 20%  later: 5%  (should be set by -cacheopts)
#ifdef HAS_DVBIN_SUPPORT
goto_enable_cache:
#endif
if(stream_cache_size>0){
  current_module="enable_cache";
  if(!stream_enable_cache(stream,stream_cache_size*1024,stream_cache_size*1024*(stream_cache_min_percent / 100.0),stream_cache_size*1024*(stream_cache_prefill_percent / 100.0)))
    if((eof = libmpdemux_was_interrupted(PT_NEXT_ENTRY))) goto goto_next_file;
}

//============ Open DEMUXERS --- DETECT file type =======================
current_module="demux_open";

demuxer=demux_open(stream,file_format,audio_id,video_id,dvdsub_id,filename);

// HACK to get MOV Reference Files working

if (demuxer && demuxer->type==DEMUXER_TYPE_PLAYLIST)
{ 
  unsigned char* playlist_entry;
  play_tree_t *list = NULL, *entry = NULL;

  current_module="handle_demux_playlist";
  while (ds_get_packet(demuxer->video,&playlist_entry)>0)
  {	 
    char *temp, *bname;
    
    mp_msg(MSGT_CPLAYER,MSGL_V,"Adding file %s to element entry.\n",playlist_entry);

    bname=mp_basename(playlist_entry);
    if ((strlen(bname)>10) && !strncmp(bname,"qt",2) && !strncmp(bname+3,"gateQT",6))
        continue;

    if (!strncmp(bname,mp_basename(filename),strlen(bname))) // ignoring self-reference
        continue;

    entry = play_tree_new();
    
    if (filename && !strcmp(mp_basename(playlist_entry),playlist_entry)) // add reference path of current file
    {
      temp=malloc((strlen(filename)-strlen(mp_basename(filename))+strlen(playlist_entry)+1)*sizeof(char));
      if (temp)
      {
	strncpy(temp, filename, strlen(filename)-strlen(mp_basename(filename)));
	temp[strlen(filename)-strlen(mp_basename(filename))]='\0';
	strcat(temp, playlist_entry);
	play_tree_add_file(entry,temp);
	mp_msg(MSGT_CPLAYER,MSGL_V,"Resolving reference to %s.\n",temp);
	free(temp);
      }
    }
    else
      play_tree_add_file(entry,playlist_entry);
    
    if(!list)
      list = entry;
    else
      play_tree_append_entry(list,entry);
  }
  free_demuxer(demuxer);
  demuxer = NULL;

  if (list)
  {
    entry = play_tree_new();
    play_tree_set_child(entry,list);
    eof=playtree_add_playlist(entry);
    goto goto_next_file;
  }
}

if(!demuxer) 
{
#if 0
  play_tree_t* entry;
  // Handle playlist
  current_module="handle_playlist";
  switch(stream->type){
  case STREAMTYPE_VCD:
  case STREAMTYPE_DVD:
  case STREAMTYPE_DVDNAV:
  case STREAMTYPE_CDDA:
  case STREAMTYPE_VCDBINCUE:
    // don't try to parse raw media as playlist, it's unlikely
    goto goto_next_file;
  }
  mp_msg(MSGT_CPLAYER,MSGL_INFO,MSGTR_FallingBackOnPlaylist,filename);
  stream_reset(stream);
  stream_seek(stream,stream->start_pos);
  entry = parse_playtree(stream,0);
  if(!entry)
    mp_msg(MSGT_DEMUXER,MSGL_ERR,MSGTR_FormatNotRecognized);
  else
    eof=playtree_add_playlist(entry);
#endif
  goto goto_next_file;
}
inited_flags|=INITED_DEMUXER;

#ifdef HAVE_MATROSKA
if (demuxer->type==DEMUXER_TYPE_MATROSKA) {
  // setup global sub numbering
  global_sub_indices[SUB_SOURCE_DEMUX] = global_sub_size; // the global # of the first demux-specific sub.
  global_sub_size += demux_mkv_num_subs(demuxer);
}
#endif
#ifdef HAVE_OGGVORBIS
if (demuxer->type==DEMUXER_TYPE_OGG) {
  // setup global sub numbering
  global_sub_indices[SUB_SOURCE_DEMUX] = global_sub_size; // the global # of the first demux-specific sub.
  global_sub_size += demux_ogg_num_subs(demuxer);
}
#endif

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
      mp_msg(MSGT_CPLAYER,MSGL_FATAL,MSGTR_DumpSelectedStreamMissing);
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
  exit_player_with_rc(MSGTR_Exit_eof, 0);
}

sh_audio=d_audio->sh;
sh_video=d_video->sh;

if(sh_video){

  current_module="video_read_properties";
  if(!video_read_properties(sh_video)) {
    mp_msg(MSGT_CPLAYER,MSGL_ERR,MSGTR_CannotReadVideoProperties);
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
#ifdef HAVE_X11
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
#ifdef HAS_DVBIN_SUPPORT
	if((stream->type == STREAMTYPE_DVB) && stream->priv)
	{
	  dvb_priv_t *priv = (dvb_priv_t*) stream->priv;
	  if(priv->is_on)
	  {
		int dir;
		int v = last_dvb_step;
		if(v > 0)
			dir = DVB_CHANNEL_HIGHER;
		else
			dir = DVB_CHANNEL_LOWER;
			
		if(dvb_step_channel(priv, dir))
		{
	  		uninit_player(INITED_ALL-(INITED_STREAM|INITED_INPUT));
			cache_uninit(stream);
			goto goto_enable_cache;
		}
	  }
	}
#endif	
    goto goto_next_file; // exit_player(MSGTR_Exit_error);
}

/* display clip info */
demux_info_print(demuxer);

//================== Read SUBTITLES (DVD & TEXT) ==========================
if(vo_spudec==NULL && sh_video &&
     (stream->type==STREAMTYPE_DVD || demuxer->type==DEMUXER_TYPE_MATROSKA ||
      d_dvdsub->id >= 0)){

if (spudec_ifo) {
  unsigned int palette[16], width, height;
  current_module="spudec_init_vobsub";
  if (vobsub_parse_ifo(NULL,spudec_ifo, palette, &width, &height, 1, -1, NULL) >= 0)
    vo_spudec=spudec_new_scaled(palette, width, height);
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

#ifdef HAVE_MATROSKA
if ((vo_spudec == NULL) && (demuxer->type == DEMUXER_TYPE_MATROSKA) &&
    (d_dvdsub->sh != NULL) && (((mkv_sh_sub_t *)d_dvdsub->sh)->type == 'v')) {
  mkv_sh_sub_t *mkv_sh_sub = (mkv_sh_sub_t *)d_dvdsub->sh;
  current_module = "spudec_init_matroska";
  vo_spudec =
    spudec_new_scaled_vobsub(mkv_sh_sub->palette, mkv_sh_sub->colors,
                             mkv_sh_sub->custom_colors, mkv_sh_sub->width,
                             mkv_sh_sub->height);
  forced_subs_only = mkv_sh_sub->forced_subs_only;
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

// Apply current settings for forced subs
if (vo_spudec!=NULL)
  spudec_set_forced_subs_only(vo_spudec,forced_subs_only);

#ifdef USE_SUB
if(sh_video) {
// after reading video params we should load subtitles because
// we know fps so now we can adjust subtitles time to ~6 seconds AST
// check .sub
  current_module="read_subtitles_file";
  if(sub_name){
    for (i = 0; sub_name[i] != NULL; ++i) 
        add_subtitles (sub_name[i], sh_video->fps, 0); 
  } 
  if(sub_auto) { // auto load sub file ...
    char *psub = get_path( "sub/" );
    char **tmp = sub_filenames((psub ? psub : ""), filename);
    int i = 0;
    free(psub); // release the buffer created by get_path() above
    while (tmp[i]) {
        add_subtitles (tmp[i], sh_video->fps, 0);
        free(tmp[i++]);
    }
    free(tmp);
    if (set_of_sub_size == 0)
    {
        add_subtitles (mem_ptr=get_path("default.sub"), sh_video->fps, 1);
    free(mem_ptr); // release the buffer created by get_path()
    }
  }
  if (set_of_sub_size > 0)  {
      // setup global sub numbering
      global_sub_indices[SUB_SOURCE_SUBS] = global_sub_size; // the global # of the first sub.
      global_sub_size += set_of_sub_size;
  }
}
#endif

if (global_sub_size) {
  // find the best sub to use
  if (vobsub_id >= 0) {
    // if user asks for a vobsub id, use that first.
    global_sub_pos = global_sub_indices[SUB_SOURCE_VOBSUB] + vobsub_id;
  } else if (dvdsub_id >= 0 && global_sub_indices[SUB_SOURCE_DEMUX] >= 0) {
    // if user asks for a dvd sub id, use that next.
    global_sub_pos = global_sub_indices[SUB_SOURCE_DEMUX] + dvdsub_id;
  } else if (global_sub_indices[SUB_SOURCE_SUBS] >= 0) {
    // if there are text subs to use, use those.  (autosubs come last here)
    global_sub_pos = global_sub_indices[SUB_SOURCE_SUBS];
/*
  } else if (global_sub_indices[SUB_SOURCE_DEMUX] >= 0) {
    // if nothing else works, get subs from the demuxer.
    global_sub_pos = global_sub_indices[SUB_SOURCE_DEMUX];
*/
  } else {
    // nothing worth doing automatically.
    global_sub_pos = -1;
  }
  // rather than duplicate code, use the SUB_SELECT handler to init the right one.
  global_sub_pos--;
  mp_input_queue_cmd(mp_input_parse_cmd("sub_select"));
}

//================== Init AUDIO (codec) ==========================
if(sh_audio){
  // Go through the codec.conf and find the best codec...
  current_module="init_audio_codec";
  mp_msg(MSGT_CPLAYER,MSGL_INFO,"==========================================================================\n");
  if(!init_best_audio_codec(sh_audio,audio_codec_list,audio_fm_list)){
    sh_audio=d_audio->sh=NULL; // failed to init :(
  } else
    inited_flags|=INITED_ACODEC;
  mp_msg(MSGT_CPLAYER,MSGL_INFO,"==========================================================================\n");
}

if(identify) {
  mp_msg(MSGT_GLOBAL,MSGL_INFO,"ID_FILENAME=%s\n", filename);
  if (sh_video) {
    /* Assume FOURCC if all bytes >= 0x20 (' ') */
    if (sh_video->format >= 0x20202020)
	mp_msg(MSGT_GLOBAL,MSGL_INFO,"ID_VIDEO_FORMAT=%.4s\n", &sh_video->format);
    else
	mp_msg(MSGT_GLOBAL,MSGL_INFO,"ID_VIDEO_FORMAT=0x%08X\n", sh_video->format);
    mp_msg(MSGT_GLOBAL,MSGL_INFO,"ID_VIDEO_BITRATE=%d\n", sh_video->i_bps*8);
    mp_msg(MSGT_GLOBAL,MSGL_INFO,"ID_VIDEO_WIDTH=%d\n", sh_video->disp_w);
    mp_msg(MSGT_GLOBAL,MSGL_INFO,"ID_VIDEO_HEIGHT=%d\n", sh_video->disp_h);
    mp_msg(MSGT_GLOBAL,MSGL_INFO,"ID_VIDEO_FPS=%5.3f\n", sh_video->fps);
    mp_msg(MSGT_GLOBAL,MSGL_INFO,"ID_VIDEO_ASPECT=%1.4f\n", sh_video->aspect);
  }
  if (sh_audio) {
    if (sh_audio->codec)
      mp_msg(MSGT_GLOBAL,MSGL_INFO, "ID_AUDIO_CODEC=%s\n", sh_audio->codec->name);
    /* Assume FOURCC if all bytes >= 0x20 (' ') */
    if (sh_audio->format >= 0x20202020)
      mp_msg(MSGT_GLOBAL,MSGL_INFO, "ID_AUDIO_FORMAT=%.4s\n", &sh_audio->format);
    else
      mp_msg(MSGT_GLOBAL,MSGL_INFO,"ID_AUDIO_FORMAT=%d\n", sh_audio->format);
    mp_msg(MSGT_GLOBAL,MSGL_INFO,"ID_AUDIO_BITRATE=%d\n", sh_audio->i_bps*8);
    mp_msg(MSGT_GLOBAL,MSGL_INFO,"ID_AUDIO_RATE=%d\n", sh_audio->samplerate);
    mp_msg(MSGT_GLOBAL,MSGL_INFO,"ID_AUDIO_NCH=%d\n", sh_audio->channels);
  }
  mp_msg(MSGT_GLOBAL,MSGL_INFO,"ID_LENGTH=%ld\n", demuxer_get_time_length(demuxer));
}

if(!sh_video) goto main; // audio-only

//================== Init VIDEO (codec & libvo) ==========================
if(!fixed_vo || !(inited_flags&INITED_VO)){
current_module="preinit_libvo";

vo_config_count=0;
//if((video_out->preinit(vo_subdevice))!=0){
if(!(video_out=init_best_video_out(video_driver_list))){
    mp_msg(MSGT_CPLAYER,MSGL_FATAL,MSGTR_ErrorInitializingVODevice);
    goto goto_next_file; // exit_player(MSGTR_Exit_error);
}
sh_video->video_out=video_out;
inited_flags|=INITED_VO;
}

current_module="init_video_filters";
{
  char* vf_arg[] = { "_oldargs_", (char*)video_out , NULL };
  sh_video->vfilter=(void*)vf_open_filter(NULL,"vo",vf_arg);
}
#ifdef HAVE_MENU
if(use_menu) {
  char* vf_arg[] = { "_oldargs_", menu_root, NULL };
  vf_menu = vf_open_plugin(libmenu_vfs,sh_video->vfilter,"menu",vf_arg);
  if(!vf_menu) {
    mp_msg(MSGT_CPLAYER,MSGL_ERR,MSGTR_CantOpenLibmenuFilterWithThisRootMenu,menu_root);
    use_menu = 0;
  }
}
if(vf_menu)
  sh_video->vfilter=(void*)append_filters(vf_menu);
else
#endif
sh_video->vfilter=(void*)append_filters(sh_video->vfilter);

current_module="init_video_codec";

mp_msg(MSGT_CPLAYER,MSGL_INFO,"==========================================================================\n");
init_best_video_codec(sh_video,video_codec_list,video_fm_list);
mp_msg(MSGT_CPLAYER,MSGL_INFO,"==========================================================================\n");

if(!sh_video->inited){
    if(!fixed_vo) uninit_player(INITED_VO);
    if(!sh_audio) goto goto_next_file;
    sh_video = d_video->sh = NULL;
    goto main; // exit_player(MSGTR_Exit_error);
}

inited_flags|=INITED_VCODEC;

if(auto_quality>0){
    // Auto quality option enabled
    output_quality=get_video_quality_max(sh_video);
    if(auto_quality>output_quality) auto_quality=output_quality;
    else output_quality=auto_quality;
    mp_msg(MSGT_CPLAYER,MSGL_V,"AutoQ: setting quality to %d.\n",output_quality);
    set_video_quality(sh_video,output_quality);
}

// ========== Init display (sh_video->disp_w*sh_video->disp_h/out_fmt) ============

current_module="init_vo";

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
if(!sh_video && osd_level >= 0) { // save OSD level only once
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
     if ( sh_audio ) guiIntfStruct.AudioType=sh_audio->channels; else guiIntfStruct.AudioType=0;
     if ( !sh_video && sh_audio ) guiGetEvent( guiSetAudioOnly,(char *)1 ); else guiGetEvent( guiSetAudioOnly,(char *)0 );
     guiGetEvent( guiSetFileFormat,(char *)demuxer->file_format );
     if ( guiGetEvent( guiSetValues,(char *)sh_video ) ) goto goto_next_file;
     guiGetEvent( guiSetDemuxer,(char *)demuxer );
    }
#endif

{
//int frame_corr_num=0;   //
//float v_frame=0;    // Video
float time_frame=0; // Timer
//float num_frames=0;      // number of frames played
int grab_frames=0;
char osd_text_buffer[64];
char osd_show_text_buffer[64];
int drop_frame=0;     // current dropping status
int dropped_frames=0; // how many frames dropped since last non-dropped frame
int too_slow_frame_cnt=0;
int too_fast_frame_cnt=0;
// for auto-quality:
float AV_delay=0; // average of A-V timestamp differences
double vdecode_time;
unsigned int lastframeout_ts=0;
/*float time_frame_corr_avg=0;*/ /* unused */

float next_frame_time=0;
int frame_time_remaining=0; // flag
int blit_frame=0;

osd_text_buffer[0]=0;

//================ SETUP AUDIO ==========================

if(sh_audio){
  //const ao_info_t *info=audio_out->info;
  current_module="af_preinit";
  ao_data.samplerate=force_srate;
  ao_data.channels=0;
  ao_data.format=audio_output_format;
#if 1
  // first init to detect best values
  if(!preinit_audio_filters(sh_audio,
        // input:
        (int)(sh_audio->samplerate*playback_speed),
	sh_audio->channels, sh_audio->sample_format,
	// output:
	&ao_data.samplerate, &ao_data.channels, &ao_data.format)){
      mp_msg(MSGT_CPLAYER,MSGL_ERR,MSGTR_AudioFilterChainPreinitError);
  }
#endif  
  current_module="ao2_init";
  if(!(audio_out=init_best_audio_out(audio_driver_list,
      0, // plugin flag
      ao_data.samplerate,
      ao_data.channels,
      ao_data.format,0))){
    // FAILED:
    mp_msg(MSGT_CPLAYER,MSGL_ERR,MSGTR_CannotInitAO);
    uninit_player(INITED_ACODEC); // close codec
    sh_audio=d_audio->sh=NULL; // -> nosound
  } else {
    // SUCCESS:
    inited_flags|=INITED_AO;
    mp_msg(MSGT_CPLAYER,MSGL_INFO,"AO: [%s] %dHz %dch %s (%d bps)\n",
      audio_out->info->short_name,
      ao_data.samplerate, ao_data.channels,
      af_fmt2str_short(ao_data.format),
      af_fmt2bits(ao_data.format)/8 );
    mp_msg(MSGT_CPLAYER,MSGL_V,"AO: Description: %s\nAO: Author: %s\n",
      audio_out->info->name, audio_out->info->author);
    if(strlen(audio_out->info->comment) > 0)
      mp_msg(MSGT_CPLAYER,MSGL_V,"AO: Comment: %s\n", audio_out->info->comment);
    // init audio filters:
#if 1
    current_module="af_init";
    if(!build_afilter_chain(sh_audio, &ao_data)) {
      mp_msg(MSGT_CPLAYER,MSGL_ERR,MSGTR_NoMatchingFilter);
//      mp_msg(MSGT_CPLAYER,MSGL_ERR,"Couldn't find matching filter / ao format! -> NOSOUND\n");
//      uninit_player(INITED_ACODEC|INITED_AO); // close codec & ao
//      sh_audio=d_audio->sh=NULL; // -> nosound
    }
#endif
  }
  mixer.audio_out = audio_out;
  mixer.volstep = volstep;
}

current_module="av_init";

if(sh_video) sh_video->timer=0;
if(sh_audio) sh_audio->delay=-audio_delay;

if(!sh_audio){
  mp_msg(MSGT_CPLAYER,MSGL_INFO,MSGTR_NoSound);
  mp_msg(MSGT_CPLAYER,MSGL_V,"Freeing %d unused audio chunks.\n",d_audio->packs);
  ds_free_packs(d_audio); // free buffered chunks
  d_audio->id=-2;         // do not read audio chunks
  //uninit_player(INITED_AO); // close device
}
if(!sh_video){
   mp_msg(MSGT_CPLAYER,MSGL_INFO,MSGTR_Video_NoVideo);
   mp_msg(MSGT_CPLAYER,MSGL_V,"Freeing %d unused video chunks.\n",d_video->packs);
   ds_free_packs(d_video);
   d_video->id=-2;
   //if(!fixed_vo) uninit_player(INITED_VO);
}

if (!sh_video && !sh_audio)
    goto goto_next_file;

//if(demuxer->file_format!=DEMUXER_TYPE_AVI) pts_from_bps=0; // it must be 0 for mpeg/asf!
if(force_fps && sh_video){
  vo_fps = sh_video->fps=force_fps;
  sh_video->frametime=1.0f/sh_video->fps;
  mp_msg(MSGT_CPLAYER,MSGL_INFO,MSGTR_FPSforced,sh_video->fps,sh_video->frametime);
}

//==================== START PLAYING =======================

if(loop_times>1) loop_times--; else
if(loop_times==1) loop_times = -1;

mp_msg(MSGT_CPLAYER,MSGL_INFO,MSGTR_StartPlaying);fflush(stdout);

#ifdef USE_DVDNAV
if (stream->type==STREAMTYPE_DVDNAV) {
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
  
  ao_data.pts=((sh_video?sh_video->timer:0)+sh_audio->delay)*90000.0;
  playsize=audio_out->get_space();
  
  // handle audio-only case:
  if(playsize < ao_data.outburst &&
      !sh_video) {  // buffer is full, do not block here!!!
    usec_sleep(10000); // Wait a tick before retry
    continue;
  }
  
  if(playsize>MAX_OUTBURST) playsize=MAX_OUTBURST; // we shouldn't exceed it!

  // Fill buffer if needed:
  current_module="decode_audio";   // Enter AUDIO decoder module
  t=GetTimer();
  while(sh_audio->a_out_buffer_len<playsize && !d_audio->eof){
    int ret=decode_audio(sh_audio,&sh_audio->a_out_buffer[sh_audio->a_out_buffer_len],
        playsize-sh_audio->a_out_buffer_len,sh_audio->a_out_buffer_size-sh_audio->a_out_buffer_len);
    if(ret<=0) break; // EOF?
    sh_audio->a_out_buffer_len+=ret;
  }
  t=GetTimer()-t;
  tt = t*0.000001f; audio_time_usage+=tt;
  if(playsize>sh_audio->a_out_buffer_len) playsize=sh_audio->a_out_buffer_len;

  // play audio:  
  current_module="play_audio";
  playsize=audio_out->play(sh_audio->a_out_buffer,playsize,0);

  if(playsize>0){
      sh_audio->a_out_buffer_len-=playsize;
      memmove(sh_audio->a_out_buffer,&sh_audio->a_out_buffer[playsize],sh_audio->a_out_buffer_len);
      sh_audio->delay+=playback_speed*playsize/((float)((ao_data.bps && sh_audio->afilter) ?
          ao_data.bps : sh_audio->o_bps));
  }

  break;
} // while(sh_audio)

if(!sh_video) {
  // handle audio-only case:
  if(!quiet) {
    float a_pos = sh_audio->delay - audio_out->get_delay() * playback_speed;
    print_status(a_pos, 0, 0);
  }
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
	if(sh_audio) sh_audio->delay-=frame_time;
	time_frame+=frame_time;  // for nosound
	// video_read_frame can change fps (e.g. for asf video)
	vo_fps = sh_video->fps;
#ifdef HAVE_X11
	vo_mouse_timer_const = (int)sh_video->fps;
#endif
	// check for frame-drop:
	current_module="check_framedrop";
	if(sh_audio && !d_audio->eof){
	    float delay=playback_speed*audio_out->get_delay();
	    float d=delay-sh_audio->delay;
	    // we should avoid dropping to many frames in sequence unless we
	    // are too late. and we allow 100ms A-V delay here:
	    if(d<-dropped_frames*frame_time-0.100 && osd_function != OSD_PAUSE){
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

    if(drop_frame && !frame_time_remaining && !autosync){
      /*
       * Note: time_frame should not be forced to 0 in autosync mode.
       * It is used as a cumulative counter to predict and correct the
       * delay measurements from the audio driver.  time_frame is already
       * < 0, so the "time to sleep" code does not actually sleep.  Also,
       * blit_frame is already 0 because drop_frame was true when
       * decode_video was called (which causes it to set blit_frame to 0.)
       * When autosync==0, the default behavior is still completely unchanged.
       */

      time_frame=0;	// don't sleep!
      blit_frame=0;	// don't display!
      
    } else {

      // It's time to sleep...
      
      frame_time_remaining=0;
      time_frame-=GetRelativeTime(); // reset timer

      if(sh_audio && !d_audio->eof){
	  float delay=playback_speed*audio_out->get_delay();
	  mp_dbg(MSGT_AVSYNC,MSGL_DBG2,"delay=%f\n",delay);

	  if (autosync){
	    /*
	     * Adjust this raw delay value by calculating the expected
	     * delay for this frame and generating a new value which is
	     * weighted between the two.  The higher autosync is, the
	     * closer to the delay value gets to that which "-nosound"
	     * would have used, and the longer it will take for A/V
	     * sync to settle at the right value (but it eventually will.)
	     * This settling time is very short for values below 100.
	     */
	    float predicted = sh_audio->delay+time_frame;
	    float difference = delay - predicted;
	    delay = predicted + difference / (float)autosync;
	  }

          time_frame=delay-sh_audio->delay;

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

time_frame/=playback_speed;

// flag 256 means: libvo driver does its timing (dvb card)
if(time_frame>0.001 && !(vo_flags&256)){

#ifdef HAVE_RTC
    if(rtc_fd>=0){
	// -------- RTC -----------
	current_module="sleep_rtc";
        while (time_frame > 0.000) {
	    unsigned long rtc_ts;
	    if (read (rtc_fd, &rtc_ts, sizeof(rtc_ts)) <= 0)
		    mp_msg(MSGT_CPLAYER, MSGL_ERR, MSGTR_LinuxRTCReadError, strerror(errno));
    	    time_frame-=GetRelativeTime();
	}
    } else
#endif
#ifdef SYS_DARWIN
    {
		current_module="sleep_darwin";
        while(time_frame>0.005) {
			usec_sleep(1000000*time_frame);
			time_frame-=GetRelativeTime();
        }
	}
#else
    {
	// -------- TIMER + SOFTSLEEP -----------
	float min=softsleep?0.021:0.005;
	current_module="sleep_timer";
        while(time_frame>min){
          if(time_frame<=0.020)
             usec_sleep(0); // sleeps 1 clock tick (10ms)!
          else
             usec_sleep(1000000*(time_frame-0.020));
          time_frame-=GetRelativeTime();
        }
	if(softsleep){
	    current_module="sleep_soft";
	    if(time_frame<0) mp_msg(MSGT_AVSYNC, MSGL_WARN, MSGTR_SoftsleepUnderflow);
	    while(time_frame>0) time_frame-=GetRelativeTime(); // burn the CPU
	}
    }
#endif
}

//if(!frame_time_remaining){	// should we display the frame now?

//====================== FLIP PAGE (VIDEO BLT): =========================

        current_module="vo_check_events";
	if(vo_config_count) video_out->check_events();

        current_module="flip_page";
        if (!frame_time_remaining) {
         if(blit_frame){
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
	 } else {
             /*
	     Well, no blitting is needed, but some devices (such as yuv4mpeg) must output frame
             otherwise A/V desync will occur. -- Alvieboy
	     */
	    if (vo_config_count)
		video_out->control(VOCTRL_DUPLICATE_FRAME, NULL);
         }
        }
//====================== A-V TIMESTAMP CORRECTION: =========================

  current_module="av_sync";

  if(sh_audio){
    float a_pts=0;
    float v_pts=0;

    // unplayed bytes in our and soundcard/dma buffer:
    float delay=playback_speed*audio_out->get_delay()+(float)sh_audio->a_buffer_len/(float)sh_audio->o_bps;

    if (autosync){
      /*
       * If autosync is enabled, the value for delay must be calculated
       * a bit differently.  It is set only to the difference between
       * the audio and video timers.  Any attempt to include the real
       * or corrected delay causes the pts_correction code below to
       * try to correct for the changes in delay which autosync is
       * trying to measure.  This keeps the two from competing, but still
       * allows the code to correct for PTS drift *only*.  (Using a delay
       * value here, even a "corrected" one, would be incompatible with
       * autosync mode.)
       */
      delay=sh_audio->delay;
      delay+=(float)sh_audio->a_buffer_len/(float)sh_audio->o_bps;
    }

#if 0
    if(pts_from_bps){
	// PTS = sample_no / samplerate
        unsigned int samples=
//	  (sh_audio->audio.dwSampleSize)?
//          ((ds_tell(d_audio)-sh_audio->a_in_buffer_len)/sh_audio->audio.dwSampleSize) :
          ds_tell_block(d_audio); // <- used for VBR audio
	samples+=sh_audio->audio.dwStart; // offset
        a_pts=samples*(float)sh_audio->audio.dwScale/(float)sh_audio->audio.dwRate;
	delay_corrected=1;
	a_pts-=(sh_audio->a_in_buffer_len)/(float)sh_audio->i_bps;
    } else 
#endif
    {
      // PTS = (last timestamp) + (bytes after last timestamp)/(bytes per sec)
      a_pts=d_audio->pts;
      if(!delay_corrected) if(a_pts) delay_corrected=1;
#if 0
      mp_msg(MSGT_FIXME, MSGL_FIXME, "\n#X# pts=%5.3f ds_pts=%5.3f buff=%5.3f total=%5.3f\n",
          a_pts,
	  ds_tell_pts(d_audio)/(float)sh_audio->i_bps,
	  -sh_audio->a_in_buffer_len/(float)sh_audio->i_bps,
	  a_pts+(ds_tell_pts(d_audio)-sh_audio->a_in_buffer_len)/(float)sh_audio->i_bps);
#endif	  
      a_pts+=(ds_tell_pts(d_audio)-sh_audio->a_in_buffer_len)/(float)sh_audio->i_bps;
    }
    v_pts=sh_video ? sh_video->pts : d_video->pts;

      mp_dbg(MSGT_AVSYNC,MSGL_DBG2,"### A:%8.3f (%8.3f)  V:%8.3f  A-V:%7.4f  \n",a_pts,a_pts-audio_delay-delay,v_pts,(a_pts-delay-audio_delay)-v_pts);

      if(delay_corrected){
	static int drop_message=0;
        float x;
	AV_delay=(a_pts-delay-audio_delay)-v_pts;
	if(AV_delay>0.5 && drop_frame_cnt>50 && drop_message==0){
	  ++drop_message;
	  mp_msg(MSGT_AVSYNC,MSGL_WARN,MSGTR_SystemTooSlow);
	}
        x=AV_delay*0.1f;
        if(x<-max_pts_correction) x=-max_pts_correction; else
        if(x> max_pts_correction) x= max_pts_correction;
        if(default_max_pts_correction>=0)
          max_pts_correction=default_max_pts_correction;
        else
          max_pts_correction=sh_video->frametime*0.10; // +-10% of time
	if(!frame_time_remaining){ sh_audio->delay+=x; c_total+=x;} // correction
        if(!quiet)
          print_status(a_pts - audio_delay - delay, AV_delay, c_total);
      }
    
  } else {
    // No audio:
    
    if(!quiet)
      print_status(0, 0, 0);

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
        if (identify)
          mp_msg(MSGT_GLOBAL, MSGL_INFO, "ID_PAUSED\n");
	fflush(stdout);
      }
#ifdef HAVE_NEW_GUI
      if(use_gui) guiGetEvent( guiCEvent,(char *)guiSetPause );
#endif
      if (video_out && sh_video && vo_config_count)
	 video_out->control(VOCTRL_PAUSE, NULL);

      if (audio_out && sh_audio)
         audio_out->pause();	// pause audio, keep data if possible

      while( (cmd = mp_input_get_cmd(20,1,1)) == NULL) {
	     if(sh_video && video_out && vo_config_count) video_out->check_events();
#ifdef HAVE_NEW_GUI
             if(use_gui){
		guiEventHandling();
		guiGetEvent( guiReDraw,NULL );
		if(guiIntfStruct.Playing!=2 || (rel_seek_secs || abs_seek_pos)) break;
             }
#endif
#ifdef HAVE_MENU
	     if(vf_menu)
	       vf_menu_pause_update(vf_menu);
#endif
             usec_sleep(20000);
         }
      if (cmd && cmd->id == MP_CMD_PAUSE) {
      cmd = mp_input_get_cmd(0,1,0);
      mp_cmd_free(cmd);
      }
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

//================= EDL =========================================

#ifdef USE_EDL
 if( next_edl_record ) { // Are we (still?) doing EDL?
  if ( !sh_video ) {
    mp_msg( MSGT_CPLAYER, MSGL_ERR, MSGTR_EdlNOsh_video );
    free_edl(edl_records);
    next_edl_record = NULL; 
    edl_records = NULL;
  } else {
   if( sh_video->pts >= next_edl_record->start_sec ) {
     if( next_edl_record->action == EDL_SKIP ) {
       osd_function = OSD_FFW;
       abs_seek_pos = 0;
       rel_seek_secs = next_edl_record->length_sec;
       mp_msg(MSGT_CPLAYER, MSGL_DBG4, "EDL_SKIP: start [%f], stop [%f], length [%f]\n", next_edl_record->start_sec, next_edl_record->stop_sec, next_edl_record->length_sec );
       edl_decision = 1;
     } else if( next_edl_record->action == EDL_MUTE ) {
       edl_muted = !edl_muted;
       if ((user_muted | edl_muted) != mixer.muted) mixer_mute(&mixer);
       mp_msg(MSGT_CPLAYER, MSGL_DBG4, "EDL_MUTE: [%f]\n", next_edl_record->start_sec );
     }
     next_edl_record=next_edl_record->next;
   }
  }
 }
#endif

//================= Keyboard events, SEEKing ====================

  current_module="key_events";

{
  mp_cmd_t* cmd;
  int brk_cmd = 0;
  while( !brk_cmd && (cmd = mp_input_get_cmd(0,0,0)) != NULL) {
    switch(cmd->id) {
    case MP_CMD_SEEK : {
      float v;
      int abs;
      osd_show_percentage = 25;
      v = cmd->args[0].v.f;
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
      brk_cmd = 1;
    } break;
#ifdef USE_EDL
    case MP_CMD_EDL_MARK:
      if( edl_fd ) {
	float v = sh_video->pts;
	fprintf( edl_fd, "%f %f %d\n", v-2, v, 0 );
      }
      break;
#endif
    case MP_CMD_SWITCH_RATIO : {
      if (cmd->nargs == 0)
	movie_aspect = (float) sh_video->disp_w / sh_video->disp_h;
      else
	movie_aspect = cmd->args[0].v.f;
      mpcodecs_config_vo (sh_video, sh_video->disp_w, sh_video->disp_h, 0);
    } break;
    case MP_CMD_AUDIO_DELAY : {
      float v = cmd->args[0].v.f;
      audio_delay += v;
      osd_show_av_delay = 9;
      if(sh_audio) sh_audio->delay+= v;
    } break;
    case MP_CMD_SPEED_INCR : {
      float v = cmd->args[0].v.f;
      playback_speed += v;
      if (sh_video)
      osd_show_speed = sh_video->fps;
      build_afilter_chain(sh_audio, &ao_data);
    } break;
    case MP_CMD_SPEED_MULT : {
      float v = cmd->args[0].v.f;
      playback_speed *= v;
      if (sh_video)
      osd_show_speed = sh_video->fps;
      build_afilter_chain(sh_audio, &ao_data);
    } break;
    case MP_CMD_SPEED_SET : {
      float v = cmd->args[0].v.f;
      playback_speed = v;
      if (sh_video)
      osd_show_speed = sh_video->fps;
      build_afilter_chain(sh_audio, &ao_data);
    } break;
    case MP_CMD_FRAME_STEP :
    case MP_CMD_PAUSE : {
      cmd->pausing = 1;
      brk_cmd = 1;
    } break;
    case MP_CMD_QUIT : {
      exit_player_with_rc(MSGTR_Exit_quit, (cmd->nargs > 0)? cmd->args[0].v.i : 0);
    }
    case MP_CMD_GRAB_FRAMES : {
      grab_frames=2;
    } break;
    case MP_CMD_PLAY_TREE_STEP : {
      int n = cmd->args[0].v.i == 0 ? 1 : cmd->args[0].v.i;
      int force = cmd->args[1].v.i;

#ifdef HAVE_NEW_GUI
     if (use_gui) {
	int i=0;
        if (n>0)
	  for (i=0;i<n;i++)
	    mplNext();
        else
	  for (i=0;i<-1*n;i++)
	    mplPrev();
     } else
#endif
     {
      if(!force && playtree_iter) {
	play_tree_iter_t* i = play_tree_iter_new_copy(playtree_iter);
	
	if(play_tree_iter_step(i,n,0) == PLAY_TREE_ITER_ENTRY)
	  eof = (n > 0) ? PT_NEXT_ENTRY : PT_PREV_ENTRY;
	play_tree_iter_free(i);
      } else
	eof = (n > 0) ? PT_NEXT_ENTRY : PT_PREV_ENTRY;
      if(eof)
	play_tree_step = n;
      brk_cmd = 1;
     }
    } break;
    case MP_CMD_PLAY_TREE_UP_STEP : {
      int n = cmd->args[0].v.i > 0 ? 1 : -1;
      int force = cmd->args[1].v.i;

      if(!force && playtree_iter) {
	play_tree_iter_t* i = play_tree_iter_new_copy(playtree_iter);
	if(play_tree_iter_up_step(i,n,0) == PLAY_TREE_ITER_ENTRY)
	  eof = (n > 0) ? PT_UP_NEXT : PT_UP_PREV;
	play_tree_iter_free(i);
      } else
	eof = (n > 0) ? PT_UP_NEXT : PT_UP_PREV;
      brk_cmd = 1;
    } break;
    case MP_CMD_PLAY_ALT_SRC_STEP : {
      if(playtree_iter && playtree_iter->num_files > 1) {
	int v = cmd->args[0].v.i;
	if(v > 0 && playtree_iter->file < playtree_iter->num_files)
	  eof = PT_NEXT_SRC;
	else if(v < 0 && playtree_iter->file > 1)
	  eof = PT_PREV_SRC;
      }
      brk_cmd = 1;
    } break;
    case MP_CMD_SUB_DELAY : {
#ifdef USE_SUB
    if (sh_video) {
      int abs= cmd->args[1].v.i;
      float v = cmd->args[0].v.f;
      if(abs)
	sub_delay = v;
      else
	sub_delay += v;
      osd_show_sub_delay = 9; // show the subdelay in OSD
    }
#endif
    } break;
    case MP_CMD_SUB_STEP : {
#ifdef USE_SUB
    if (sh_video) {
      int movement = cmd->args[0].v.i;
      step_sub(subdata, sh_video->pts, movement);
      osd_show_sub_delay = 9; // show the subdelay in OSD
    }
#endif
    } break;
    case MP_CMD_SUB_LOG : {
#ifdef USE_SUB
	log_sub();
#endif
    } break;
    case MP_CMD_OSD :  {
#ifdef USE_OSD
      if(sh_video) {
	int v = cmd->args[0].v.i;
	if(v < 0)
	  osd_level=(osd_level+1)%(MAX_OSD_LEVEL+1);
	else
	  osd_level= v > MAX_OSD_LEVEL ? MAX_OSD_LEVEL : v;
	/* Show OSD state when disabled, but not when an explicit
	   argument is given to the osd command, i.e. in slave mode. */
	if (v == -1 && osd_level <= 1)
	  osd_show_status = 9;
      }
#endif
    } break;
    case MP_CMD_OSD_SHOW_TEXT :  {
#ifdef USE_OSD
      if(osd_level && sh_video){
	osd_show_text=sh_video->fps; // 1 sec
        strncpy(osd_show_text_buffer, cmd->args[0].v.s, 64);
      }
#endif
    } break;
    case MP_CMD_VOLUME :  {
      int v = cmd->args[0].v.i;

		// start change for absolute volume value
		int abs = (cmd->nargs > 1) ? cmd->args[1].v.i : 0;
		
#ifdef USE_EDL
      if (edl_muted) break;
      user_muted = 0;
#endif
		if( abs )
		{
			mixer_setvolume(&mixer, (float)v, (float)v );
		} else {
      if(v > 0)
	mixer_incvolume(&mixer);
      else
	mixer_decvolume(&mixer);
		}
	  
#ifdef USE_OSD
      if(osd_level && sh_video){
        float vol;
	osd_visible=sh_video->fps; // 1 sec
	vo_osd_progbar_type=OSD_VOLUME;
	mixer_getbothvolume(&mixer, &vol);
	vo_osd_progbar_value=(vol*256.0)/100.0;
	vo_osd_changed(OSDTYPE_PROGBAR);
      }
#endif
    } break;
    case MP_CMD_MUTE:
#ifdef USE_EDL
      user_muted = !user_muted;
      if ((edl_muted | user_muted) != mixer.muted)
#endif
      mixer_mute(&mixer);
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
      brk_cmd = 1;
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
      brk_cmd = 1;
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
      if(v < 0){
	frame_dropping = (frame_dropping+1)%3;
#ifdef USE_OSD
       osd_show_framedropping=10;
       vo_osd_changed(OSDTYPE_SUBTITLE);
#endif
      }
      else
	frame_dropping = v > 2 ? 2 : v;
    } break;
#ifdef USE_TV
    case MP_CMD_TV_SET_FREQ :  {
      if (file_format == DEMUXER_TYPE_TV)
        tv_set_freq((tvi_handle_t*)(demuxer->priv), cmd->args[0].v.f * 16.0);
    } break;
    case MP_CMD_TV_SET_NORM :  {
      if (file_format == DEMUXER_TYPE_TV)
        tv_set_norm((tvi_handle_t*)(demuxer->priv), cmd->args[0].v.s);
    } break;
    case MP_CMD_TV_SET_BRIGHTNESS :  {
      if (file_format == DEMUXER_TYPE_TV)
        tv_set_color_options((tvi_handle_t*)(demuxer->priv), TV_COLOR_BRIGHTNESS, cmd->args[0].v.i);
    } break;
    case MP_CMD_TV_SET_HUE :  {
      if (file_format == DEMUXER_TYPE_TV)
        tv_set_color_options((tvi_handle_t*)(demuxer->priv), TV_COLOR_HUE, cmd->args[0].v.i);
    } break;
    case MP_CMD_TV_SET_SATURATION :  {
      if (file_format == DEMUXER_TYPE_TV)
        tv_set_color_options((tvi_handle_t*)(demuxer->priv), TV_COLOR_SATURATION, cmd->args[0].v.i);
    } break;
    case MP_CMD_TV_SET_CONTRAST :  {
      if (file_format == DEMUXER_TYPE_TV)
        tv_set_color_options((tvi_handle_t*)(demuxer->priv), TV_COLOR_CONTRAST, cmd->args[0].v.i);
    } break;
    case MP_CMD_TV_STEP_CHANNEL :  {
      if (file_format == DEMUXER_TYPE_TV) {
	int v = cmd->args[0].v.i;
	if(v > 0){
	  tv_step_channel((tvi_handle_t*)(demuxer->priv), TV_CHANNEL_HIGHER);
#ifdef USE_OSD
	  if (tv_channel_list) {
	    osd_show_tv_channel = sh_video->fps;
	    vo_osd_changed(OSDTYPE_SUBTITLE);
	  }
#endif
	} else {
	  tv_step_channel((tvi_handle_t*)(demuxer->priv), TV_CHANNEL_LOWER);
#ifdef USE_OSD
	  if (tv_channel_list) {
	    osd_show_tv_channel = sh_video->fps;
	    vo_osd_changed(OSDTYPE_SUBTITLE);
	  }
#endif
	}
      }
    } 
#ifdef HAS_DVBIN_SUPPORT
	if((stream->type == STREAMTYPE_DVB) && stream->priv)
	{
	  dvb_priv_t *priv = (dvb_priv_t*) stream->priv;
	  if(priv->is_on)
	  {
		int dir;
		int v = cmd->args[0].v.i;
	    
		last_dvb_step = v;	
		if(v > 0)
			dir = DVB_CHANNEL_HIGHER;
		else
			dir = DVB_CHANNEL_LOWER;
			
			
		if(dvb_step_channel(priv, dir))
		{
	  		uninit_player(INITED_ALL-(INITED_STREAM|INITED_INPUT));
			cache_uninit(stream);
			goto goto_enable_cache;
		}
	  }
	}
#endif	
    break;
    case MP_CMD_TV_SET_CHANNEL :  {
      if (file_format == DEMUXER_TYPE_TV) {
	tv_set_channel((tvi_handle_t*)(demuxer->priv), cmd->args[0].v.s);
#ifdef USE_OSD
	if (tv_channel_list) {
		osd_show_tv_channel = sh_video->fps;
		vo_osd_changed(OSDTYPE_SUBTITLE);
	}
#endif
      }
    } break;
#ifdef HAS_DVBIN_SUPPORT	
  case MP_CMD_DVB_SET_CHANNEL:  
  {
	if((stream->type == STREAMTYPE_DVB) && stream->priv)
	{
	  dvb_priv_t *priv = (dvb_priv_t*) stream->priv;
	  if(priv->is_on)
	  {
		if(priv->list->current <= cmd->args[0].v.i)
		    last_dvb_step = 1;
		else
		    last_dvb_step = -1;

  		if(dvb_set_channel(priv, cmd->args[1].v.i, cmd->args[0].v.i))
		{
	  	  uninit_player(INITED_ALL-(INITED_STREAM|INITED_INPUT));
		  cache_uninit(stream);
		  goto goto_enable_cache;
		}
	  }
	}
  }
#endif	
    case MP_CMD_TV_LAST_CHANNEL :  {
      if (file_format == DEMUXER_TYPE_TV) {
	tv_last_channel((tvi_handle_t*)(demuxer->priv));
#ifdef USE_OSD
	if (tv_channel_list) {
		osd_show_tv_channel = sh_video->fps;
		vo_osd_changed(OSDTYPE_SUBTITLE);
	}
#endif
      }
    } break;
    case MP_CMD_TV_STEP_NORM :  {
      if (file_format == DEMUXER_TYPE_TV)
	tv_step_norm((tvi_handle_t*)(demuxer->priv));
    } break;
    case MP_CMD_TV_STEP_CHANNEL_LIST :  {
      if (file_format == DEMUXER_TYPE_TV)
	tv_step_chanlist((tvi_handle_t*)(demuxer->priv));
    } break;
#endif
    case MP_CMD_SWITCH_VSYNC:  {
      vo_vsync = ( cmd->nargs > 0 )? cmd->args[0].v.i : !vo_vsync;
    } break;
    case MP_CMD_VO_FULLSCREEN:
    {
#ifdef HAVE_NEW_GUI
     if ( use_gui ) guiGetEvent( guiIEvent,(char *)MP_CMD_GUI_FULLSCREEN );
      else
#endif
	if(video_out && vo_config_count) video_out->control(VOCTRL_FULLSCREEN, 0);
    } break;
    case MP_CMD_VO_ONTOP:
    {
     if(video_out && vo_config_count) {
       video_out->control(VOCTRL_ONTOP, 0);
#ifdef USE_OSD
       osd_show_ontop=10;
       vo_osd_changed(OSDTYPE_SUBTITLE);
#endif
     }

    } break;
    case MP_CMD_VO_ROOTWIN:
    {
     if(video_out && vo_config_count) {
       video_out->control(VOCTRL_ROOTWIN, 0);
#ifdef USE_OSD
       osd_show_rootwin=10;
       vo_osd_changed(OSDTYPE_SUBTITLE);
#endif
     }

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
        if(osd_level && sh_video){
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
#ifdef USE_SUB
      if (sh_video) {
        int v;
	v = cmd->args[0].v.i;
    
	sub_pos+=v;
	if(sub_pos >100) sub_pos=100;
	if(sub_pos <0) sub_pos=0;
	vo_osd_changed(OSDTYPE_SUBTITLE);
        osd_show_sub_pos = 9;
      }
#endif
    } break;
    case MP_CMD_SUB_ALIGNMENT:
    {
#ifdef USE_SUB
      if (sh_video) {
    	if (cmd->nargs >= 1)
    	    sub_alignment = cmd->args[0].v.i;
    	else
            sub_alignment = (sub_alignment+1) % 3;
	osd_show_sub_alignment = 9;
	vo_osd_changed(OSDTYPE_SUBTITLE);
      }
#endif
    } break;
    case MP_CMD_SUB_VISIBILITY:
    {
#ifdef USE_SUB
      if (sh_video) {
	sub_visibility=1-sub_visibility;
	osd_show_sub_visibility = 9; // show state of subtitle visibility in OSD
	vo_osd_changed(OSDTYPE_SUBTITLE);
      }
#endif
    } break;
    case MP_CMD_SUB_LOAD:
    {
#ifdef USE_SUB
      if (sh_video) {
        int n = set_of_sub_size;
        add_subtitles(cmd->args[0].v.s, sh_video->fps, 0);
        if (n != set_of_sub_size) {
          if (global_sub_indices[SUB_SOURCE_SUBS] < 0)
            global_sub_indices[SUB_SOURCE_SUBS] = global_sub_size;
          ++global_sub_size;
        }
      }
#endif
    } break;
    case MP_CMD_SUB_REMOVE:
    {
#ifdef USE_SUB
      if (sh_video) {
        int v = cmd->args[0].v.i;
        sub_data *subd;
        if (v < 0) {
          for (v = 0; v < set_of_sub_size; ++v) {
            subd = set_of_subtitles[v];
            mp_msg(MSGT_CPLAYER, MSGL_STATUS, MSGTR_RemovedSubtitleFile, v + 1, subd->filename);
            sub_free(subd);
            set_of_subtitles[v] = NULL;
          }
          global_sub_indices[SUB_SOURCE_SUBS] = -1;
          global_sub_size -= set_of_sub_size;
          set_of_sub_size = 0;
          if (set_of_sub_pos >= 0) {
            global_sub_pos = -2;
            vo_sub_last = vo_sub = NULL;
            vo_osd_changed(OSDTYPE_SUBTITLE);
            vo_update_osd(sh_video->disp_w, sh_video->disp_h);
            mp_input_queue_cmd(mp_input_parse_cmd("sub_select"));
          }
        }
        else if (v < set_of_sub_size) {
          subd = set_of_subtitles[v];
          mp_msg(MSGT_CPLAYER, MSGL_STATUS, MSGTR_RemovedSubtitleFile, v + 1, subd->filename);
          sub_free(subd);
          if (set_of_sub_pos == v) {
            global_sub_pos = -2;
            vo_sub_last = vo_sub = NULL;
            vo_osd_changed(OSDTYPE_SUBTITLE);
            vo_update_osd(sh_video->disp_w, sh_video->disp_h);
            mp_input_queue_cmd(mp_input_parse_cmd("sub_select"));
          }
          else if (set_of_sub_pos > v) {
            --set_of_sub_pos;
            --global_sub_pos;
          }
          while (++v < set_of_sub_size)
            set_of_subtitles[v - 1] = set_of_subtitles[v];
          --set_of_sub_size;
          --global_sub_size;
          if (set_of_sub_size <= 0)
            global_sub_indices[SUB_SOURCE_SUBS] = -1;
          set_of_subtitles[set_of_sub_size] = NULL;
        }
      }
#endif
    } break;
    case MP_CMD_GET_SUB_VISIBILITY:
	{
#ifdef USE_SUB
	if (sh_video) {
		mp_msg(MSGT_GLOBAL,MSGL_INFO,MSGTR_AnsSubVisibility, sub_visibility);
	}
#endif
	} break;
    case MP_CMD_SUB_SELECT:
    if (global_sub_size) {
        int source = -1;
        int v = cmd->args[0].v.i;

        if (v < -1)
            global_sub_pos++;
        else
            global_sub_pos = v;
        if (global_sub_pos >= global_sub_size)
            global_sub_pos = -1;
        if (global_sub_pos >= 0)
            source = sub_source();

	mp_msg(MSGT_CPLAYER, MSGL_DBG3, "subtitles: %d subs, (v@%d s@%d d@%d), @%d, source @%d\n",
		global_sub_size, global_sub_indices[SUB_SOURCE_VOBSUB],
		global_sub_indices[SUB_SOURCE_SUBS], global_sub_indices[SUB_SOURCE_DEMUX],
		global_sub_pos, source);

#ifdef USE_SUB
        set_of_sub_pos = -1;
        subdata = NULL;
        vo_sub_last = vo_sub = NULL;
#endif
        vobsub_id = -1;
        dvdsub_id = -1;
        if (d_dvdsub) d_dvdsub->id = -1;

        // be careful!
        // if sub_changed is till on but subdata's been reset, bad things happen.
        osd_show_vobsub_changed = 0;
        osd_show_sub_changed = 0;

        if (source == SUB_SOURCE_VOBSUB) {
            vobsub_id = global_sub_pos - global_sub_indices[SUB_SOURCE_VOBSUB];
            if (!global_sub_quiet_osd_hack) osd_show_vobsub_changed = sh_video->fps;
#ifdef USE_SUB
        } else if (source == SUB_SOURCE_SUBS) {
            set_of_sub_pos = global_sub_pos - global_sub_indices[SUB_SOURCE_SUBS];
            subdata = set_of_subtitles[set_of_sub_pos];
            if (!global_sub_quiet_osd_hack) osd_show_sub_changed = sh_video->fps;
            vo_osd_changed(OSDTYPE_SUBTITLE); 

            // FIXME: is this the correct place for these?
            if(stream_dump_type==3) list_sub_file(subdata);
            if(stream_dump_type==4) dump_mpsub(subdata, sh_video->fps);
            if(stream_dump_type==6) dump_srt(subdata, sh_video->fps);
            if(stream_dump_type==7) dump_microdvd(subdata, sh_video->fps);
            if(stream_dump_type==8) dump_jacosub(subdata, sh_video->fps);
            if(stream_dump_type==9) dump_sami(subdata, sh_video->fps);
#endif
        } else if (source == SUB_SOURCE_DEMUX) {
            dvdsub_id = global_sub_pos - global_sub_indices[SUB_SOURCE_DEMUX];
            if (d_dvdsub) {
#ifdef USE_DVDREAD
                if (vo_spudec && stream->type == STREAMTYPE_DVD) {
                    d_dvdsub->id = dvdsub_id;
                    spudec_reset(vo_spudec);
                }
#endif
#ifdef HAVE_OGGVORBIS
                if (demuxer->type == DEMUXER_TYPE_OGG)
                    d_dvdsub->id = demux_ogg_sub_id(demuxer, dvdsub_id);
#endif
#ifdef HAVE_MATROSKA
                if (demuxer->type == DEMUXER_TYPE_MATROSKA) {
                    d_dvdsub->id = demux_mkv_change_subs(demuxer, dvdsub_id);
      if (d_dvdsub->id >= 0 && ((mkv_sh_sub_t *)d_dvdsub->sh)->type == 'v') {
        mkv_sh_sub_t *mkv_sh_sub = (mkv_sh_sub_t *)d_dvdsub->sh;
        if (vo_spudec != NULL)
          spudec_free(vo_spudec);
        vo_spudec =
          spudec_new_scaled_vobsub(mkv_sh_sub->has_palette ? mkv_sh_sub->palette : NULL, mkv_sh_sub->colors,
                                   mkv_sh_sub->custom_colors,
                                   mkv_sh_sub->width,
                                   mkv_sh_sub->height);
        if (!forced_subs_only)
          forced_subs_only = mkv_sh_sub->forced_subs_only;
        if (vo_spudec) {
          spudec_set_forced_subs_only(vo_spudec, forced_subs_only);
          inited_flags |= INITED_SPUDEC;
        }
      }
    }
#endif
            }
            if (!global_sub_quiet_osd_hack) osd_show_vobsub_changed = sh_video->fps;
        } else { // off
            if (!global_sub_quiet_osd_hack) osd_show_vobsub_changed = sh_video->fps;
#ifdef USE_SUB
            vo_osd_changed(OSDTYPE_SUBTITLE); 
#endif
	}
        // it's annoying and dumb to show osd saying "off" at every subless file...
        global_sub_quiet_osd_hack = 0;
    }
        break;
    case MP_CMD_SUB_FORCED_ONLY:
      if (vo_spudec) {
	forced_subs_only = forced_subs_only ? 0 : ~0; // toggle state
	spudec_set_forced_subs_only(vo_spudec,forced_subs_only);
      }    
      break;
    case MP_CMD_SCREENSHOT :
      if(vo_config_count) video_out->control(VOCTRL_SCREENSHOT, NULL);
      break;
    case MP_CMD_VF_CHANGE_RECTANGLE:
	set_rectangle(sh_video, cmd->args[0].v.i, cmd->args[1].v.i);
	break;
	
    case MP_CMD_GET_TIME_LENGTH : {
	mp_msg(MSGT_GLOBAL,MSGL_INFO,MSGTR_AnsLength, demuxer_get_time_length(demuxer));
    } break;

	case MP_CMD_GET_VO_FULLSCREEN : {
	if(video_out && vo_config_count)
		mp_msg(MSGT_GLOBAL,MSGL_INFO,MSGTR_AnsVoFullscreen, vo_fs);
	} break;
    
    case MP_CMD_GET_PERCENT_POS : {
	mp_msg(MSGT_GLOBAL,MSGL_INFO,MSGTR_AnsPercentPos, demuxer_get_percent_pos(demuxer));
    } break;
    case MP_CMD_GET_TIME_POS : {
      float pos = 0;
      if (sh_video)
        pos = sh_video->pts;
      else
      if (sh_audio && audio_out)
        pos = sh_audio->delay - audio_out->get_delay() * playback_speed;
      mp_msg(MSGT_GLOBAL, MSGL_INFO, MSGTR_AnsTimePos, pos);
    } break;
    case MP_CMD_SWITCH_AUDIO : {
        int v = demuxer_switch_audio(demuxer, cmd->args[0].v.i);
        if (identify)
          mp_msg(MSGT_GLOBAL, MSGL_INFO, "ID_AUDIO_TRACK=%d\n", v);
    } break;
    case MP_CMD_RUN : {
#ifndef __MINGW32__
        if(!fork()) {
          execl("/bin/sh","sh","-c",cmd->args[0].v.s,NULL);
          exit(0);
        }
#endif
    } break;

#ifdef USE_DVDNAV
    case MP_CMD_DVDNAV_EVENT: {
      dvdnav_priv_t * dvdnav_priv = (dvdnav_priv_t*)(stream->priv);
      dvdnav_event_t * dvdnav_event = (dvdnav_event_t *)(cmd->args[0].v.v);

      /* ignore these events if we're not in dvd_nav mode */
      if (stream->type != STREAMTYPE_DVDNAV) break;

      if (!dvdnav_event) {
        mp_msg(MSGT_FIXME, MSGL_FIXME, MSGTR_DvdnavNullEvent);
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
                mp_msg(MSGT_FIXME, MSGL_FIXME, MSGTR_DvdnavHighlightEventBroken);
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
                mp_msg(MSGT_FIXME, MSGL_FIXME, MSGTR_DvdnavEvent,dvd_nav_text);
                //osd_show_dvd_nav_delay = 60;

                osd_show_dvd_nav_highlight=1;
                osd_show_dvd_nav_sx=hevent->sx;
                osd_show_dvd_nav_ex=hevent->ex;
                osd_show_dvd_nav_sy=hevent->sy;
                osd_show_dvd_nav_ey=hevent->ey;
          }
          else {
                  osd_show_dvd_nav_highlight=0;
                  mp_msg(MSGT_FIXME, MSGL_FIXME, MSGTR_DvdnavHighlightHide);
          }
        break;
        }
      case DVDNAV_STILL_FRAME: {
          dvdnav_still_event_t *still_event = (dvdnav_still_event_t*)(dvdnav_event->details);

          mp_msg(MSGT_FIXME, MSGL_FIXME, MSGTR_DvdnavStillFrame, still_event->length );
          while (dvdnav_stream_sleeping(dvdnav_priv)) {
            usec_sleep(1000); /* 1ms */
          }
          dvdnav_stream_sleep(dvdnav_priv,still_event->length);
        break;
        }
      case DVDNAV_STOP: {
          mp_msg(MSGT_FIXME, MSGL_FIXME, MSGTR_DvdnavNavStop );
        break;
        }
      case DVDNAV_NOP: {
        mp_msg(MSGT_FIXME, MSGL_FIXME, MSGTR_DvdnavNavNOP);
        break;
        }
      case DVDNAV_SPU_STREAM_CHANGE: {
#if DVDNAVVERSION > 012
        dvdnav_spu_stream_change_event_t *stream_change = (dvdnav_spu_stream_change_event_t*)(dvdnav_event->details);

        mp_msg(MSGT_FIXME, MSGL_FIXME, MSGTR_DvdnavNavSpuStreamChangeVerbose,
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

        mp_msg(MSGT_FIXME, MSGL_FIXME, MSGTR_DvdnavNavSpuStreamChange,
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

        mp_msg(MSGT_FIXME, MSGL_FIXME, MSGTR_DvdnavNavAudioStreamChange,
                stream_change->physical,
                stream_change->logical);

        aid_temp=stream_change->physical;
        if (aid_temp>=0) aid_temp+=128; // FIXME: is this sane?
        if (d_audio && audio_id!=aid_temp) {
                mp_msg(MSGT_INPUT,MSGL_DBG2,"d_audio->id change: was %d is now %d\n",
                        d_audio->id,aid_temp);
                // FIXME: need a bettery way to change audio stream id
                d_audio->id=dvdsub_id=aid_temp;
                if(sh_audio) resync_audio_stream(sh_audio);
        }

        break;
      }
      case DVDNAV_VTS_CHANGE: {
        mp_msg(MSGT_FIXME, MSGL_FIXME, MSGTR_DvdnavNavVTSChange);
        break;
        }
      case DVDNAV_CELL_CHANGE: {
        dvdnav_cell_change_event_t *cell_change = (dvdnav_cell_change_event_t*)(dvdnav_event->details);
        cell_playback_t * cell_playback = cell_change->new_cell;

        mp_msg(MSGT_FIXME, MSGL_FIXME, MSGTR_DvdnavNavCellChange);
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

        mp_msg(MSGT_FIXME, MSGL_FIXME, MSGTR_DvdnavNavSpuClutChange);
        // send new palette to SPU decoder
        if (vo_spudec) spudec_update_palette(vo_spudec,new_clut);

        break;
        }
      case DVDNAV_SEEK_DONE: {
        mp_msg(MSGT_FIXME, MSGL_FIXME, MSGTR_DvdnavNavSeekDone);
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
      if (stream->type != STREAMTYPE_DVDNAV) break;

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
          mp_msg(MSGT_FIXME, MSGL_FIXME, MSGTR_MenuCall);
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
      mp_msg(MSGT_CPLAYER, MSGL_V, "Received unknown cmd %s\n",cmd->name);
    }
    }
    if (cmd->pausing)
      osd_function = OSD_PAUSE;
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
    play_n_frames=play_n_frames_mf;
    eof=0;
    abs_seek_pos=3; rel_seek_secs=0; // seek to start of movie (0%)
    loop_seek = 1;
  }

if(rel_seek_secs || abs_seek_pos){
  current_module="seek";
  if(demux_seek(demuxer,rel_seek_secs,abs_seek_pos)){
      // success:
      /* FIXME there should be real seeking for vobsub */
      if(sh_video) sh_video->pts=d_video->pts;
      if (vo_vobsub)
	//vobsub_reset(vo_vobsub);
	vobsub_seek(vo_vobsub,sh_video->pts);
#if 0
      if(sh_video && d_video->packs == 0)
	ds_fill_buffer(d_video);
      if(sh_audio){
	if(d_audio->packs == 0)
	  ds_fill_buffer(d_audio);
	if(verbose>0){
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
         resync_video_stream(sh_video);
         if(vo_config_count) video_out->control(VOCTRL_RESET,NULL);
      }
      
      if(sh_audio){
        current_module="seek_audio_reset";
        audio_out->reset(); // stop audio, throwing away buffered data
      }
#ifdef USE_OSD
        // Set OSD:
      if(osd_level && !loop_seek){
#ifdef USE_EDL
	if( !edl_decision ) {
#else
	  if( 1 ) { // Let the compiler optimize this out
#endif
	  if (sh_video) {
	    osd_visible=sh_video->fps; // 1 sec
	    vo_osd_progbar_type=0;
	    vo_osd_progbar_value=demuxer_get_percent_pos(demuxer) * 256 / 100;
	    vo_osd_changed(OSDTYPE_PROGBAR);
	  }
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
#ifdef USE_EDL
/*
 * We saw a seek, have to rewind the EDL operations stack
 * and find the next EDL action to take care of.
 */

edl_muted = 0;
next_edl_record = edl_records;

while (next_edl_record)
{
    /* Trying to remember if we need to mute/unmute first;
     * prior EDL implementation lacks this.
     */
  
    if (next_edl_record->start_sec >= sh_video->pts)
        break;

    if (next_edl_record->action == EDL_MUTE) edl_muted = !edl_muted;
    next_edl_record = next_edl_record->next;

}
if ((user_muted | edl_muted) != mixer.muted) mixer_mute(&mixer);
#endif
  rel_seek_secs=0;
  abs_seek_pos=0;
  frame_time_remaining=0;
  current_module=NULL;
  loop_seek=0;
}

#ifdef HAVE_NEW_GUI
      if(use_gui){
        guiEventHandling();
	if(demuxer->file_format==DEMUXER_TYPE_AVI && sh_video && sh_video->video.dwLength>2){
	  // get pos from frame number / total frames
	  guiIntfStruct.Position=(float)d_video->pack_no*100.0f/sh_video->video.dwLength;
	} else {
	 off_t len = ( demuxer->movi_end - demuxer->movi_start );
	 off_t pos = ( demuxer->file_format == DEMUXER_TYPE_AUDIO?stream->pos:demuxer->filepos );
	 guiIntfStruct.Position=(len <= 0? 0.0f : ( pos - demuxer->movi_start ) * 100.0f / len );
	}
	if ( sh_video ) guiIntfStruct.TimeSec=sh_video->pts;
	  else if ( sh_audio ) guiIntfStruct.TimeSec=sh_audio->delay;
	guiIntfStruct.LengthInSec=demuxer_get_time_length(demuxer);
	guiGetEvent( guiReDraw,NULL );
	guiGetEvent( guiSetVolume,NULL );
	if(guiIntfStruct.Playing==0) break; // STOP
	if(guiIntfStruct.Playing==2) osd_function=OSD_PAUSE;
        if ( guiIntfStruct.DiskChanged || guiIntfStruct.NewPlay ) goto goto_next_file;
#ifdef USE_DVDREAD
        if ( stream->type == STREAMTYPE_DVD )
	 {
	  dvd_priv_t * dvdp = stream->priv;
	  guiIntfStruct.DVD.current_chapter=dvd_chapter_from_cell(dvdp,guiIntfStruct.DVD.current_title-1, dvdp->cur_cell)+1;
	 }
#endif
      }
#endif


//================= Update OSD ====================
#ifdef USE_OSD
  if(osd_level>=1 && sh_video){
      int pts=sh_video->pts;
      char osd_text_tmp[64];
      if(pts==osd_last_pts-1) ++pts; else osd_last_pts=pts;
      vo_osd_text=osd_text_buffer;
#ifdef USE_DVDNAV
      if (osd_show_dvd_nav_delay) {
          snprintf(osd_text_tmp, 63, "DVDNAV: %s", dvd_nav_text);
          osd_show_dvd_nav_delay--;
      } else
#endif
#ifdef USE_TV
      if (osd_show_tv_channel && tv_channel_list) {
	  snprintf(osd_text_tmp, 63, "Channel: %s", tv_channel_current->name);
	  osd_show_tv_channel--;
      } else
#endif
      if (osd_show_text) {
	  snprintf(osd_text_tmp, 63, "%s", osd_show_text_buffer);
	  osd_show_text--;
      } else
      if (osd_show_sub_visibility) {
	  snprintf(osd_text_tmp, 63, "Subtitles: %sabled", sub_visibility?"en":"dis");
	  osd_show_sub_visibility--;
      } else
      if (osd_show_vobsub_changed) {
	  snprintf(osd_text_tmp, 63, "Subtitles: off");
	  if (vo_vobsub && vobsub_id >= 0) {
	      const char *language = "none";
	      language = vobsub_get_id(vo_vobsub, (unsigned int) vobsub_id);
	      snprintf(osd_text_tmp, 63, "Subtitles: (%d) %s", vobsub_id, language ? language : "unknown");
	  }
#ifdef HAVE_OGGVORBIS
	  if (d_dvdsub && demuxer->type == DEMUXER_TYPE_OGG) {
	      if (dvdsub_id >= 0) {
		char *lang = demux_ogg_sub_lang(demuxer, dvdsub_id);
		if (!lang) lang = "unknown";
		snprintf(osd_text_tmp, 63, "Subtitles: (%d) %s", dvdsub_id, lang);
	      }
	  }
#endif
#ifdef USE_DVDREAD
	  if (vo_spudec && (demuxer->type != DEMUXER_TYPE_MATROSKA)) {
	      if (dvdsub_id >= 0) {
		  char lang[5] = "none";
		  int code = 0;
		  code = dvd_lang_from_sid(stream, dvdsub_id);
		  if (code) {
		      lang[0] = code >> 8;
		      lang[1] = code;
		      lang[2] = 0;
		  }
		  snprintf(osd_text_tmp, 63, "Subtitles: (%d) %s", dvdsub_id, lang);
	      }
	  }
#endif
#ifdef HAVE_MATROSKA
    if (demuxer->type == DEMUXER_TYPE_MATROSKA) {
      char lang[10] = "unknown";
      if (dvdsub_id >= 0) {
        demux_mkv_get_sub_lang(demuxer, dvdsub_id, lang, 9);
        lang[9] = 0;
	snprintf(osd_text_tmp, 63, "Subtitles: (%d) %s", dvdsub_id, lang);
      }
    }
#endif
	  osd_show_vobsub_changed--;
      } else
#ifdef USE_SUB
      if (osd_show_sub_changed) {
	  char *tmp2;
          tmp = subdata->filename;
	  if ((tmp2 = strrchr(tmp, '/'))) {
	      tmp = tmp2+1;
	  }
	  snprintf(osd_text_tmp, 63, "Sub: (%d) %s%s", 
                                  set_of_sub_pos + 1,
                                  strlen(tmp) < 20 ? "" : "...",
                                  strlen(tmp) < 20 ? tmp : tmp+strlen(tmp)-19);
	  osd_show_sub_changed--;
      } else
#endif
      if (osd_show_sub_delay) {
	  snprintf(osd_text_tmp, 63, "Sub delay: %d ms", ROUND(sub_delay*1000));
	  osd_show_sub_delay--;
      } else
      if (osd_show_sub_pos) {
         snprintf(osd_text_tmp, 63, "Sub position: %d/100", sub_pos);
         osd_show_sub_pos--;
      } else
      if (osd_show_sub_alignment) {
         snprintf(osd_text_tmp, 63, "Sub alignment: %s",
	    (sub_alignment == 2 ? "bottom" :
	    (sub_alignment == 1 ? "center" : "top")));
         osd_show_sub_alignment--;
      } else
      if (osd_show_av_delay) {
	  snprintf(osd_text_tmp, 63, "A-V delay: %d ms", ROUND(audio_delay*1000));
	  osd_show_av_delay--;
      } else if (osd_show_speed) {
	  snprintf(osd_text_tmp, 63, "Speed: x %6.2f", playback_speed);
	  osd_show_speed--;
      } else if (osd_show_ontop) {
	  snprintf(osd_text_tmp, 63, "Stay on top: %sabled", vo_ontop?"en":"dis");
	  osd_show_ontop--;
      } else if (osd_show_rootwin) {
	  snprintf(osd_text_tmp, 63, "Rootwin: %sabled", vo_rootwin?"en":"dis");
	  osd_show_rootwin--;
      } else if (osd_show_framedropping) {
	  snprintf(osd_text_tmp, 63, "Framedropping: %s",
	     (frame_dropping == 1 ? "on" :
	     (frame_dropping == 2 ? "hard"  : "off")));
	  osd_show_framedropping--;
      } else if(osd_level>=2) {
          int len = demuxer_get_time_length(demuxer);
          int percentage = -1;
          char percentage_text[10];
          if (osd_show_percentage) {
            percentage = demuxer_get_percent_pos(demuxer);
            osd_show_percentage--;
          }
          if (percentage >= 0)
            snprintf(percentage_text, 9, " (%d%%)", percentage);
	  else
	    percentage_text[0] = 0;
          if (osd_level == 3) 
            snprintf(osd_text_tmp, 63, "%c %02d:%02d:%02d / %02d:%02d:%02d%s",osd_function,pts/3600,(pts/60)%60,pts%60,len/3600,(len/60)%60,len%60,percentage_text);
          else
            snprintf(osd_text_tmp, 63, "%c %02d:%02d:%02d%s",osd_function,pts/3600,(pts/60)%60,pts%60,percentage_text);
      } else osd_text_tmp[0]=0;
      
      if(strcmp(vo_osd_text, osd_text_tmp)) {
	      strncpy(vo_osd_text, osd_text_tmp, 63);
	      vo_osd_changed(OSDTYPE_OSD);
      }
  } else {
      if(vo_osd_text) {
         vo_osd_text=NULL;
	  vo_osd_changed(OSDTYPE_OSD);
      }
  }
  if (osd_level <= 1 && osd_show_status > 0 && sh_video) {
    vo_osd_text = osd_text_buffer;
    snprintf(vo_osd_text, 63, "OSD: %sabled", osd_level ? "en" : "dis");
    vo_osd_changed(OSDTYPE_OSD);
    osd_show_status--;
  }
//  for(i=1;i<=11;i++) osd_text_buffer[10+i]=i;osd_text_buffer[10+i]=0;
//  vo_osd_text=osd_text_buffer;
#endif
  
#ifdef USE_SUB
  // find sub
  if(subdata &&  sh_video && sh_video->pts>0){
      float pts=sh_video->pts;
      if(sub_fps==0) sub_fps=sh_video->fps;
      current_module="find_sub";
      if (pts > sub_last_pts || pts < sub_last_pts-1.0 ) {
         find_sub(subdata, (pts+sub_delay) * 
				 (subdata->sub_uses_time ? 100. : sub_fps)); 
	 if (vo_sub) vo_sub_last = vo_sub;
	 // FIXME! frame counter...
         sub_last_pts = pts;
      }
      current_module=NULL;
  }
#endif

#ifdef HAVE_X11
if (stop_xscreensaver && sh_video) {
  current_module="stop_xscreensaver";
  xscreensaver_heartbeat();
  current_module=NULL;
}
#endif
  
  // DVD sub:
if(vo_config_count && vo_spudec) {
  unsigned char* packet=NULL;
  int len,timestamp;
  current_module="spudec";
  spudec_heartbeat(vo_spudec,90000*sh_video->timer);
    // Get a sub packet from the dvd or a vobsub and make a timestamp relative to sh_video->timer
  while(1) {
    // Vobsub
    len = 0;
    if(vo_vobsub) {
      if(sh_video->pts+sub_delay>=0) {
	// The + next_frame_time is there because we'll display the sub at the next frame
	len = vobsub_get_packet(vo_vobsub,sh_video->pts+sub_delay+next_frame_time,(void**)&packet,&timestamp);
	if(len > 0) {
	  timestamp -= (sh_video->pts + sub_delay - sh_video->timer)*90000;
	  mp_dbg(MSGT_CPLAYER,MSGL_V,"\rVOB sub: len=%d v_pts=%5.3f v_timer=%5.3f sub=%5.3f ts=%d \n",len,sh_video->pts,sh_video->timer,timestamp / 90000.0,timestamp);
	}
      }
    } else {
      // DVD sub
      len = ds_get_packet_sub(d_dvdsub,(unsigned char**)&packet);
      if(len > 0) {
	float x = d_dvdsub->pts - sh_video->pts;
	if (x < -10 || x > 10) // prevent missing subs on pts reset
	  timestamp = 90000*(sh_video->timer + d_dvdsub->pts + sub_delay - sh_video->pts);
	else timestamp = 90000*(sh_video->timer + sub_delay);
	mp_dbg(MSGT_CPLAYER,MSGL_V,"\rDVD sub: len=%d  v_pts=%5.3f  s_pts=%5.3f  ts=%d \n",len,sh_video->pts,d_dvdsub->pts,timestamp);
      }
    }
      if(len<=0 || !packet) break;
      if(timestamp < 0) timestamp = 0;
      else spudec_assemble(vo_spudec,packet,len,timestamp);
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
uninit_player(INITED_ALL-(INITED_GUI+INITED_INPUT+(fixed_vo?INITED_VO:0)));

#ifdef USE_SUB  
  if ( set_of_sub_size > 0 ) 
   {
    current_module="sub_free";
    for (i = 0; i < set_of_sub_size; ++i)
        sub_free( set_of_subtitles[i] );
    set_of_sub_size = 0;
    vo_sub_last = vo_sub=NULL;
    subdata=NULL;
   }
#endif

if(eof == PT_NEXT_ENTRY || eof == PT_PREV_ENTRY) {
  eof = eof == PT_NEXT_ENTRY ? 1 : -1;
  if(play_tree_iter_step(playtree_iter,play_tree_step,0) == PLAY_TREE_ITER_ENTRY) {
    eof = 1;
  } else {
    play_tree_iter_free(playtree_iter);
    playtree_iter = NULL;
  }
  play_tree_step = 1;
} else if (eof == PT_UP_NEXT || eof == PT_UP_PREV) {
  eof = eof == PT_UP_NEXT ? 1 : -1;
  if ( playtree_iter ) {
    if(play_tree_iter_up_step(playtree_iter,eof,0) == PLAY_TREE_ITER_ENTRY) {
     eof = 1;
    } else {
      play_tree_iter_free(playtree_iter);
      playtree_iter = NULL;
    }
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

  eof = 0;
  goto play_next_file;
}


exit_player_with_rc(MSGTR_Exit_eof, 0);

return 1;
}

/*
 * config for cfgparser
 */

#include "cfg-common.h"

extern int noconsolecontrols;

#if defined(HAVE_FBDEV)||defined(HAVE_VESA)
extern char *monitor_hfreq_str;
extern char *monitor_vfreq_str;
extern char *monitor_dotclock_str;
#endif

#ifdef HAVE_FBDEV
extern char *fb_dev_name;
extern char *fb_mode_cfgfile;
extern char *fb_mode_name;
#endif
#ifdef HAVE_DIRECTFB
#if DIRECTFBVERSION > 912
extern char *dfb_params;
#endif
#endif
#ifdef HAVE_PNG
extern int z_compression;
#endif
#ifdef HAVE_SDL
//extern char *sdl_driver;
extern int sdl_noxv;
extern int sdl_forcexv;
//extern char *sdl_adriver;
#endif
#ifdef USE_FAKE_MONO
extern int fakemono; // defined in dec_audio.c
#endif

extern int volstep;

#ifdef HAVE_LIRC
extern char *lirc_configfile;
#endif

extern int vo_doublebuffering;
extern int vo_vsync;
extern int vo_fsmode;
extern int vo_dbpp;
extern int vo_directrendering;
extern float vo_panscan;
/* only used at startup (setting these values from configfile) */
extern int vo_gamma_brightness;
extern int vo_gamma_saturation;
extern int vo_gamma_contrast;
extern int vo_gamma_hue;
extern char *vo_geometry;
extern int vo_ontop;
extern int vo_keepaspect;
extern int vo_rootwin;

extern int opt_screen_size_x;
extern int opt_screen_size_y;
extern int fullscreen;
extern int vidmode;

#ifdef USE_OSD
extern int osd_level;
#endif

extern char *ao_outputfilename;
extern int ao_pcm_waveheader;

#ifdef HAVE_X11
extern char *mDisplayName;
extern int fs_layer;
extern int stop_xscreensaver;
extern char **vo_fstype_list;
extern int vo_nomouse_input;
extern int WinID;
#endif

#ifdef HAVE_MENU
extern int menu_startup;
#endif

#ifdef HAVE_AA
extern int vo_aa_parseoption(m_option_t* conf, char *opt, char * param);
extern void vo_aa_revertoption(m_option_t* opt,char* param);
#endif

#ifdef HAVE_ZR
extern int vo_zr_parseoption(m_option_t* conf, char *opt, char * param);
extern void vo_zr_revertoption(m_option_t* opt,char* pram);
#endif

#ifdef HAVE_DXR2
extern m_option_t dxr2_opts[];
#endif

#ifdef STREAMING_LIVE_DOT_COM
extern int isSDPFile;
extern int rtspStreamOverTCP;
#endif

#ifdef HAVE_NEW_GUI
extern char * skinName;
extern int enqueue;
extern int guiWinID;
#endif

#ifdef HAVE_ODIVX_POSTPROCESS
extern int use_old_pp;
#endif

#ifdef HAVE_XINERAMA
extern int xinerama_screen;
#endif

#ifdef HAVE_RTC
extern int nortc;
#endif

/* from libvo/aspect.c */
extern float monitor_aspect;

/* Options related to audio out plugins */
m_option_t ao_plugin_conf[]={
	{"list", &ao_plugin_cfg.plugin_list, CONF_TYPE_STRING, 0, 0, 0, NULL},
	{"delay", &ao_plugin_cfg.pl_delay_len, CONF_TYPE_INT, CONF_MIN, 0, 0, NULL},
	{"format", &ao_plugin_cfg.pl_format_type, CONF_TYPE_INT, CONF_MIN, 0, 0, NULL},
	{"fout", &ao_plugin_cfg.pl_resample_fout, CONF_TYPE_INT, CONF_MIN, 0, 0, NULL},
	{"volume", &ao_plugin_cfg.pl_volume_volume, CONF_TYPE_INT, CONF_RANGE, 0, 255, NULL},
	{"mul", &ao_plugin_cfg.pl_extrastereo_mul, CONF_TYPE_FLOAT, CONF_RANGE, -10.0, 10.0, NULL},
	{"softclip", &ao_plugin_cfg.pl_volume_softclip, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{NULL, NULL, 0, 0, 0, 0, NULL}
};

extern int sws_flags;
extern int readPPOpt(void *conf, char *arg);
extern void revertPPOpt(void *conf, char* opt);
extern char* pp_help;

/*
 * CONF_TYPE_FUNC_FULL :
 * allows own implementations for passing the params
 * 
 * the function receives parameter name and argument (if it does not start with - )
 * useful with a conf.name like 'aa*' to parse several parameters to a function
 * return 0 =ok, but we didn't need the param (could be the filename)
 * return 1 =ok, we accepted the param
 * negative values: see cfgparser.h, ERR_XXX
 *
 * by Folke
 */

m_option_t mplayer_opts[]={
	/* name, pointer, type, flags, min, max */

//---------------------- libao/libvo options ------------------------
	{"o", "Option -o has been renamed to -vo (video-out), use -vo.\n",
            CONF_TYPE_PRINT, CONF_NOCFG, 0, 0, NULL},
	{"vo", &video_driver_list, CONF_TYPE_STRING_LIST, 0, 0, 0, NULL},
	{"ao", &audio_driver_list, CONF_TYPE_STRING_LIST, 0, 0, 0, NULL},
	{"fixed-vo", &fixed_vo, CONF_TYPE_FLAG,CONF_GLOBAL , 0, 1, NULL},
	{"nofixed-vo", &fixed_vo, CONF_TYPE_FLAG,CONF_GLOBAL, 0, 0, NULL},
	{"ontop", &vo_ontop, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"noontop", &vo_ontop, CONF_TYPE_FLAG, 0, 1, 0, NULL},
	{"rootwin", &vo_rootwin, CONF_TYPE_FLAG, 0, 0, 1, NULL},

	{"aop", ao_plugin_conf, CONF_TYPE_SUBCONFIG, 0, 0, 0, NULL},
	{"dsp", "Use -ao oss:dsp_path.\n", CONF_TYPE_PRINT, CONF_NOCFG, 0, 0, NULL},
        {"mixer", &mixer_device, CONF_TYPE_STRING, 0, 0, 0, NULL},
        {"mixer-channel", &mixer_channel, CONF_TYPE_STRING, 0, 0, 0, NULL},
        {"softvol", &soft_vol, CONF_TYPE_FLAG, 0, 0, 1, NULL},
        {"nosoftvol", &soft_vol, CONF_TYPE_FLAG, 0, 1, 0, NULL},
        {"softvol-max", &soft_vol_max, CONF_TYPE_FLOAT, CONF_RANGE, 10, 10000, NULL},
	{"volstep", &volstep, CONF_TYPE_INT, CONF_RANGE, 0, 100, NULL},
	{"master", "Option -master has been removed, use -aop list=volume instead.\n", CONF_TYPE_PRINT, 0, 0, 0, NULL},
	// override audio buffer size (used only by -ao oss, anyway obsolete...)
	{"abs", &ao_data.buffersize, CONF_TYPE_INT, CONF_MIN, 0, 0, NULL},

	// -ao pcm options:
	{"aofile", &ao_outputfilename, CONF_TYPE_STRING, 0, 0, 0, NULL},
	{"waveheader", &ao_pcm_waveheader, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"nowaveheader", &ao_pcm_waveheader, CONF_TYPE_FLAG, 0, 1, 0, NULL},

	{"alsa", "-alsa has been removed. Remove it from your config file.\n",
            CONF_TYPE_PRINT, 0, 0, 0, NULL},
	{"noalsa", "-noalsa has been removed. Remove it from your config file.\n",
            CONF_TYPE_PRINT, 0, 0, 0, NULL},
#ifdef USE_EDL
	{"edl", &edl_filename,  CONF_TYPE_STRING, 0, 0, 0, NULL}, 
	{"edlout", &edl_output_filename,  CONF_TYPE_STRING, 0, 0, 0, NULL}, 
#else
	{"edl", "MPlayer was compiled without EDL support.\n", CONF_TYPE_PRINT, 0, 0, 0, NULL},
	{"edlout", "MPlayer was compiled without EDL support.\n", CONF_TYPE_PRINT, 0, 0, 0, NULL},
#endif

#ifdef HAVE_X11
	{"display", &mDisplayName, CONF_TYPE_STRING, 0, 0, 0, NULL},
#endif

	// -vo png only:
#ifdef HAVE_PNG
	{"z", &z_compression, CONF_TYPE_INT, CONF_RANGE, 0, 9, NULL},
#endif
	// -vo jpeg only:
#ifdef HAVE_JPEG
	{"jpeg", "-jpeg is deprecated. Use -vo jpeg:options instead.\n",
	    CONF_TYPE_PRINT, 0, 0, 0, NULL},
#endif
	// -vo sdl only:
#ifdef HAVE_SDL
	{"sdl", "Use -vo sdl:driver instead of -vo sdl -sdl driver.\n",
	    CONF_TYPE_PRINT, 0, 0, 0, NULL},
	{"noxv", &sdl_noxv, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"forcexv", &sdl_forcexv, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	// -ao sdl only:
	{"sdla", "Use -ao sdl:driver instead of -ao sdl -sdla driver.\n",
	    CONF_TYPE_PRINT, 0, 0, 0, NULL},
#endif

#if defined(HAVE_FBDEV)||defined(HAVE_VESA) 
       {"monitor-hfreq", &monitor_hfreq_str, CONF_TYPE_STRING, 0, 0, 0, NULL}, 
       {"monitor-vfreq", &monitor_vfreq_str, CONF_TYPE_STRING, 0, 0, 0, NULL}, 
       {"monitor-dotclock", &monitor_dotclock_str, CONF_TYPE_STRING, 0, 0, 0, NULL}, 
#endif 

#ifdef HAVE_FBDEV
	{"fb", &fb_dev_name, CONF_TYPE_STRING, 0, 0, 0, NULL},
	{"fbmode", &fb_mode_name, CONF_TYPE_STRING, 0, 0, 0, NULL},
	{"fbmodeconfig", &fb_mode_cfgfile, CONF_TYPE_STRING, 0, 0, 0, NULL},
#endif
#ifdef HAVE_DIRECTFB
#if DIRECTFBVERSION > 912
	{"dfbopts", &dfb_params, CONF_TYPE_STRING, 0, 0, 0, NULL},
#endif
#endif

	// force window width/height or resolution (with -vm)
	{"x", &opt_screen_size_x, CONF_TYPE_INT, CONF_RANGE, 0, 4096, NULL},
	{"y", &opt_screen_size_y, CONF_TYPE_INT, CONF_RANGE, 0, 4096, NULL},
	// set screen dimensions (when not detectable or virtual!=visible)
	{"screenw", &vo_screenwidth, CONF_TYPE_INT, CONF_RANGE|CONF_OLD, 0, 4096, NULL},
	{"screenh", &vo_screenheight, CONF_TYPE_INT, CONF_RANGE|CONF_OLD, 0, 4096, NULL},
	// Geometry string
	{"geometry", &vo_geometry, CONF_TYPE_STRING, 0, 0, 0, NULL},
	// set aspect ratio of monitor - useful for 16:9 TVout
	{"monitoraspect", &monitor_aspect, CONF_TYPE_FLOAT, CONF_RANGE, 0.2, 3.0, NULL},
	// video mode switching: (x11,xv,dga)
        {"vm", &vidmode, CONF_TYPE_FLAG, 0, 0, 1, NULL},
        {"novm", &vidmode, CONF_TYPE_FLAG, 0, 1, 0, NULL},
	// start in fullscreen mode:
	{"fs", &fullscreen, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"nofs", &fullscreen, CONF_TYPE_FLAG, 0, 1, 0, NULL},
	// set fullscreen switch method (workaround for buggy WMs)
	{"fsmode", "-fsmode is obsolete, avoid it and use -fstype instead.\nIf you really want it, try -fsmode-dontuse, but don't report bugs!\n", CONF_TYPE_PRINT, CONF_RANGE, 0, 31, NULL},
	{"fsmode-dontuse", &vo_fsmode, CONF_TYPE_INT, CONF_RANGE, 0, 31, NULL},
	// set bpp (x11+vm, dga, fbdev, vesa, svga?)
        {"bpp", &vo_dbpp, CONF_TYPE_INT, CONF_RANGE, 0, 32, NULL},
	{"colorkey", &vo_colorkey, CONF_TYPE_INT, 0, 0, 0, NULL},
	{"nocolorkey", &vo_colorkey, CONF_TYPE_FLAG, 0, 0, 0x1000000, NULL},
	// double buffering:  (mga/xmga, xv, vidix, vesa, fbdev)
	{"double", &vo_doublebuffering, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"nodouble", &vo_doublebuffering, CONF_TYPE_FLAG, 0, 1, 0, NULL},
	// wait for v-sync (vesa)
	{"vsync", &vo_vsync, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"novsync", &vo_vsync, CONF_TYPE_FLAG, 0, 1, 0, NULL},
	{"panscan", &vo_panscan, CONF_TYPE_FLOAT, CONF_RANGE, 0.0, 1.0, NULL},

	{"grabpointer", &vo_grabpointer, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"nograbpointer", &vo_grabpointer, CONF_TYPE_FLAG, 0, 1, 0, NULL},
	
    {"adapter", &vo_adapter_num, CONF_TYPE_INT, CONF_RANGE, 0, 5, NULL},
    {"refreshrate",&vo_refresh_rate,CONF_TYPE_INT,CONF_RANGE, 0,100, NULL},
#ifdef HAVE_X11
	// x11,xv,xmga,xvidix
	{"wid", &WinID, CONF_TYPE_INT, 0, 0, 0, NULL},
	{"icelayer", "-icelayer is obsolete. Use -fstype layer:<number> instead.\n", CONF_TYPE_PRINT, 0, 0, 0, NULL},
	{"stop-xscreensaver", &stop_xscreensaver, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"nostop-xscreensaver", &stop_xscreensaver, CONF_TYPE_FLAG, 0, 1, 0, NULL},
	{"stop_xscreensaver", "Use -stop-xscreensaver instead, options with _ have been obsoleted.\n", CONF_TYPE_PRINT, 0, 0, 0, NULL},
	{"fstype", &vo_fstype_list, CONF_TYPE_STRING_LIST, 0, 0, 0, NULL},
	{"nomouseinput", &vo_nomouse_input, CONF_TYPE_FLAG,0,0,-1,NULL},
#endif

#ifdef HAVE_XINERAMA
	{"xineramascreen", &xinerama_screen, CONF_TYPE_INT, CONF_RANGE, 0, 32, NULL},
#endif

	{"brightness",&vo_gamma_brightness, CONF_TYPE_INT, CONF_RANGE, -100, 100, NULL},
	{"saturation",&vo_gamma_saturation, CONF_TYPE_INT, CONF_RANGE, -100, 100, NULL},
	{"contrast",&vo_gamma_contrast, CONF_TYPE_INT, CONF_RANGE, -100, 100, NULL},
	{"hue",&vo_gamma_hue, CONF_TYPE_INT, CONF_RANGE, -100, 100, NULL},
	{"keepaspect", &vo_keepaspect, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"nokeepaspect", &vo_keepaspect, CONF_TYPE_FLAG, 0, 1, 0, NULL},

	// direct rendering (decoding to video out buffer)
	{"dr", &vo_directrendering, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"nodr", &vo_directrendering, CONF_TYPE_FLAG, 0, 1, 0, NULL},
	{"vaa_dr", "-vaa_dr is obsolete, use -dr.\n", CONF_TYPE_PRINT, 0, 0, 0, NULL},
	{"vaa_nodr", "-vaa_nodr is obsolete, use -nodr.\n", CONF_TYPE_PRINT, 0, 0, 0, NULL},

#ifdef HAVE_AA
	// -vo aa
	{"aa*",	vo_aa_parseoption,  CONF_TYPE_FUNC_FULL, 0, 0, 0 , &vo_aa_revertoption},
#endif

#ifdef HAVE_ZR
	// -vo zr
	{"zr*", vo_zr_parseoption, CONF_TYPE_FUNC_FULL, 0, 0, 0, &vo_zr_revertoption },
#endif

#ifdef HAVE_DXR2
	{"dxr2", &dxr2_opts, CONF_TYPE_SUBCONFIG, 0, 0, 0, NULL},
#endif

#ifdef STREAMING_LIVE_DOT_COM
        {"sdp", "-sdp is obsolete, use sdp://file instead.\n", CONF_TYPE_PRINT, 0, 0, 0, NULL},
	// -rtsp-stream-over-tcp option, specifying TCP streaming of RTP/RTCP
        {"rtsp-stream-over-tcp", &rtspStreamOverTCP, CONF_TYPE_FLAG, 0, 0, 1, NULL},
#else
	{"rtsp-stream-over-tcp", "RTSP support requires the \"LIVE.COM Streaming Media\" libraries.\n", CONF_TYPE_PRINT, CONF_NOCFG, 0, 0, NULL},
#endif

//---------------------- mplayer-only options ------------------------

#ifdef CRASH_DEBUG
	{"crash-debug", &crash_debug, CONF_TYPE_FLAG, CONF_GLOBAL, 0, 1, NULL},
	{"nocrash-debug", &crash_debug, CONF_TYPE_FLAG, CONF_GLOBAL, 1, 0, NULL},
#endif
	{"osdlevel", &osd_level, CONF_TYPE_INT, CONF_RANGE, 0, 3, NULL},
#ifdef HAVE_MENU
	{"menu", &use_menu, CONF_TYPE_FLAG, CONF_GLOBAL, 0, 1, NULL},
	{"nomenu", &use_menu, CONF_TYPE_FLAG, CONF_GLOBAL, 1, 0, NULL},
	{"menu-root", &menu_root, CONF_TYPE_STRING, CONF_GLOBAL, 0, 0, NULL},
	{"menu-cfg", &menu_cfg, CONF_TYPE_STRING, CONF_GLOBAL, 0, 0, NULL},
	{"menu-startup", &menu_startup, CONF_TYPE_FLAG, CONF_GLOBAL, 0, 1, NULL},
#else
	{"menu", "OSD menu support was not compiled in.\n", CONF_TYPE_PRINT,0, 0, 0, NULL},
#endif

	// these should be moved to -common, and supported in MEncoder
	{"vobsub", &vobsub_name, CONF_TYPE_STRING, 0, 0, 0, NULL},
	{"vobsubid", &vobsub_id, CONF_TYPE_INT, CONF_RANGE, 0, 31, NULL},

	{"sstep", &step_sec, CONF_TYPE_INT, CONF_MIN, 0, 0, NULL},

	// set a-v distance, should be moved to -common and supported in MEncoder
	{"delay", &audio_delay, CONF_TYPE_FLOAT, CONF_RANGE, -100.0, 100.0, NULL},

	{"speed", &playback_speed, CONF_TYPE_FLOAT, CONF_RANGE, 0.01, 100.0, NULL},

	{"framedrop", &frame_dropping, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"hardframedrop", &frame_dropping, CONF_TYPE_FLAG, 0, 0, 2, NULL},
	{"noframedrop", &frame_dropping, CONF_TYPE_FLAG, 0, 1, 0, NULL},

	{"autoq", &auto_quality, CONF_TYPE_INT, CONF_RANGE, 0, 100, NULL},

	{"benchmark", &benchmark, CONF_TYPE_FLAG, 0, 0, 1, NULL},

	// dump some stream out instead of playing the file
	// this really should be in MEncoder instead of MPlayer... -> TODO
	{"dumpfile", &stream_dump_name, CONF_TYPE_STRING, 0, 0, 0, NULL},
	{"dumpaudio", &stream_dump_type, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"dumpvideo", &stream_dump_type, CONF_TYPE_FLAG, 0, 0, 2, NULL},
	{"dumpsub", &stream_dump_type, CONF_TYPE_FLAG, 0, 0, 3, NULL},
	{"dumpmpsub", &stream_dump_type, CONF_TYPE_FLAG, 0, 0, 4, NULL},
	{"dumpstream", &stream_dump_type, CONF_TYPE_FLAG, 0, 0, 5, NULL},
	{"dumpsrtsub", &stream_dump_type, CONF_TYPE_FLAG, 0, 0, 6, NULL},
	{"dumpmicrodvdsub", &stream_dump_type, CONF_TYPE_FLAG, 0, 0, 7, NULL},
	{"dumpjacosub", &stream_dump_type, CONF_TYPE_FLAG, 0, 0, 8, NULL},
	{"dumpsami", &stream_dump_type, CONF_TYPE_FLAG, 0, 0, 9, NULL},

#ifdef HAVE_LIRC
	{"lircconf", &lirc_configfile, CONF_TYPE_STRING, CONF_GLOBAL, 0, 0, NULL},
#endif

	{"gui", "Please remove gui=yes from your config file. Run gmplayer if you want the GUI.\n", CONF_TYPE_PRINT, 0, 0, 0, NULL},
//	{"gui", &use_gui, CONF_TYPE_FLAG, CONF_GLOBAL|CONF_NOCMD, 0, 1, NULL},
//	{"nogui", &use_gui, CONF_TYPE_FLAG, CONF_GLOBAL, 1, 0, NULL},
      
#ifdef HAVE_NEW_GUI
	{"skin", &skinName, CONF_TYPE_STRING, CONF_GLOBAL, 0, 0, NULL},
	{"enqueue", &enqueue, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"noenqueue", &enqueue, CONF_TYPE_FLAG, 0, 0, 0, NULL},
	{"guiwid", &guiWinID, CONF_TYPE_INT, 0, 0, 0, NULL},
#endif

	{"noloop", &loop_times, CONF_TYPE_FLAG, 0, 0, -1, NULL},
	{"loop", &loop_times, CONF_TYPE_INT, CONF_RANGE, -1, 10000, NULL},
	{"playlist", NULL, CONF_TYPE_STRING, 0, 0, 0, NULL},

	// a-v sync stuff:
	{"noautosync", &autosync, CONF_TYPE_FLAG, 0, 0, -1, NULL},
	{"autosync", &autosync, CONF_TYPE_INT, CONF_RANGE, 0, 10000, NULL},
//	{"dapsync", &dapsync, CONF_TYPE_FLAG, 0, 0, 1, NULL},
//	{"nodapsync", &dapsync, CONF_TYPE_FLAG, 0, 1, 0, NULL},

	{"softsleep", &softsleep, CONF_TYPE_FLAG, 0, 0, 1, NULL},
#ifdef HAVE_RTC
	{"nortc", &nortc, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"rtc", &nortc, CONF_TYPE_FLAG, 0, 0, 0, NULL},
	{"rtc-device", &rtc_device, CONF_TYPE_STRING, 0, 0, 0, NULL},
#endif

	{"slave", &slave_mode, CONF_TYPE_FLAG,CONF_GLOBAL , 0, 1, NULL},
	{"use-stdin", "-use-stdin has been renamed to -noconsolecontrols, use that instead.", CONF_TYPE_PRINT, 0, 0, 0, NULL},
	{"noconsolecontrols", &noconsolecontrols, CONF_TYPE_FLAG, CONF_GLOBAL, 0, 1, NULL},
	{"consolecontrols", &noconsolecontrols, CONF_TYPE_FLAG, CONF_GLOBAL, 0, 0, NULL},

#define MAIN_CONF
#include "cfg-common.h"
#undef MAIN_CONF
        
	{"identify", &identify, CONF_TYPE_FLAG, CONF_GLOBAL, 0, 1, NULL},
	{"really-quiet", &verbose, CONF_TYPE_FLAG, CONF_GLOBAL, 0, -10, NULL},
	{"-help", help_text, CONF_TYPE_PRINT, CONF_NOCFG|CONF_GLOBAL, 0, 0, NULL},
	{"help", help_text, CONF_TYPE_PRINT, CONF_NOCFG|CONF_GLOBAL, 0, 0, NULL},
	{"h", help_text, CONF_TYPE_PRINT, CONF_NOCFG|CONF_GLOBAL, 0, 0, NULL},
	{NULL, NULL, 0, 0, 0, 0, NULL}
};

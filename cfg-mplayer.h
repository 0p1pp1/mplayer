/*
 * config for cfgparser
 */

#include "cfg-common.h"

extern char *playlist_file;

#ifdef HAVE_FBDEV
extern char *fb_dev_name;
extern char *fb_mode_cfgfile;
extern char *fb_mode_name;
extern char *monitor_hfreq_str;
extern char *monitor_vfreq_str;
extern char *monitor_dotclock_str;
#else
#ifdef HAVE_DIRECTFB
extern char *fb_dev_name;
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

#ifdef HAVE_LIRC
extern char *lirc_configfile;
#endif

#ifndef USE_LIBVO2
extern int vo_doublebuffering;
extern int vo_fsmode;
extern int vo_dbpp;
/* gamma correction */
extern int vo_gamma_brightness;
extern int vo_gamma_saturation;
extern int vo_gamma_contrast;
extern int vo_gamma_hue;
extern int vo_gamma_red_intense;
extern int vo_gamma_green_intense;
extern int vo_gamma_blue_intense;
#endif

#ifdef USE_SUB
extern int sub_unicode;
extern int sub_utf8;
#ifdef USE_ICONV
extern char *sub_cp;
#endif
#endif

#ifdef USE_OSD
extern int osd_level;
#endif

extern char *ao_outputfilename;
extern int ao_pcm_waveheader;

#ifdef HAVE_X11
extern char *mDisplayName;
extern int WinID;
#endif

#ifdef HAVE_AA
extern int vo_aa_parseoption(struct config * conf, char *opt, char * param);
extern void vo_aa_revertoption(config_t* opt,char* param);
#endif

#ifdef HAVE_ZR
extern int vo_zr_parseoption(struct config * conf, char *opt, char * param);
extern void vo_zr_revertoption(config_t* opt,char* pram);
#endif

#ifdef HAVE_NEW_GUI
extern char * skinName;
#endif

#ifdef HAVE_ODIVX_POSTPROCESS
extern int use_old_pp;
#endif

#ifdef HAVE_XINERAMA
extern int xinerama_screen;
#endif

/* from libvo/aspect.c */
extern float monitor_aspect;

/* from dec_audio, currently used for ac3surround decoder only */
extern int audio_output_channels;

/* Options related to audio out plugins */
struct config ao_plugin_conf[]={
	{"list", &ao_plugin_cfg.plugin_list, CONF_TYPE_STRING, 0, 0, 0, NULL},
	{"delay", &ao_plugin_cfg.pl_delay_len, CONF_TYPE_INT, CONF_MIN, 0, 0, NULL},
	{"format", &ao_plugin_cfg.pl_format_type, CONF_TYPE_INT, CONF_MIN, 0, 0, NULL},
	{"fout", &ao_plugin_cfg.pl_resample_fout, CONF_TYPE_INT, CONF_MIN, 0, 0, NULL},
	{NULL, NULL, 0, 0, 0, 0, NULL}
};

extern int sws_flags;
extern int readPPOpt(void *conf, char *arg);
extern int readNPPOpt(void *conf, char *arg);
extern void revertPPOpt(void *conf, char* opt);


/*
 * CONF_TYPE_FUNC_FULL :
 * allows own implemtations for passing the params
 * 
 * the function receives parameter name and argument (if it does not start with - )
 * useful with a conf.name like 'aa*' to parse several parameters to a function
 * return 0 =ok, but we didn't need the param (could be the filename)
 * return 1 =ok, we accepted the param
 * negative values: see cfgparser.h, ERR_XXX
 *
 * by Folke
 */

static config_t mplayer_opts[]={
	/* name, pointer, type, flags, min, max */
	{"include", cfg_include, CONF_TYPE_FUNC_PARAM, CONF_NOSAVE, 0, 0, NULL}, /* this don't need anymore to be the first!!! */

//---------------------- libao/libvo/mplayer options ------------------------
	{"o", "Option -o has been renamed to -vo (video-out), use -vo !\n",
            CONF_TYPE_PRINT, CONF_NOCFG, 0, 0, NULL},
	{"vo", &video_driver, CONF_TYPE_STRING, 0, 0, 0, NULL},
	{"ao", &audio_driver, CONF_TYPE_STRING, 0, 0, 0, NULL},
	{"aop", ao_plugin_conf, CONF_TYPE_SUBCONFIG, 0, 0, 0, NULL},
//	{"dsp", &dsp, CONF_TYPE_STRING, CONF_NOCFG, 0, 0, NULL},
	{"dsp", "Use -ao oss:dsp_path!\n", CONF_TYPE_PRINT, CONF_NOCFG, 0, 0, NULL},
        {"mixer", &mixer_device, CONF_TYPE_STRING, 0, 0, 0, NULL},
        {"master", &mixer_usemaster, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"channels", &audio_output_channels, CONF_TYPE_INT, CONF_RANGE, 2, 6, NULL},
#ifdef HAVE_X11
	{"display", &mDisplayName, CONF_TYPE_STRING, 0, 0, 0, NULL},
#endif
	{"osdlevel", &osd_level, CONF_TYPE_INT, CONF_RANGE, 0, 2 , NULL},

#ifdef HAVE_FBDEV
	{"fb", &fb_dev_name, CONF_TYPE_STRING, 0, 0, 0, NULL},
	{"fbmode", &fb_mode_name, CONF_TYPE_STRING, 0, 0, 0, NULL},
	{"fbmodeconfig", &fb_mode_cfgfile, CONF_TYPE_STRING, 0, 0, 0, NULL},
	{"monitor_hfreq", &monitor_hfreq_str, CONF_TYPE_STRING, 0, 0, 0, NULL},
	{"monitor_vfreq", &monitor_vfreq_str, CONF_TYPE_STRING, 0, 0, 0, NULL},
	{"monitor_dotclock", &monitor_dotclock_str, CONF_TYPE_STRING, 0, 0, 0, NULL},
#else
#ifdef HAVE_DIRECTFB
	{"fb", &fb_dev_name, CONF_TYPE_STRING, 0, 0, 0, NULL},
#endif
#endif
//	{"encode", &encode_name, CONF_TYPE_STRING, 0, 0, 0, NULL},
	{"vobsub", &vobsub_name, CONF_TYPE_STRING, 0, 0, 0, NULL},
	{"vobsubid", &vobsub_id, CONF_TYPE_INT, CONF_RANGE, 0, 31, NULL},
#ifdef USE_SUB
	{"sub", &sub_name, CONF_TYPE_STRING, 0, 0, 0, NULL},
#ifdef USE_ICONV
	{"subcp", &sub_cp, CONF_TYPE_STRING, 0, 0, 0, NULL},
#endif	
	{"subdelay", &sub_delay, CONF_TYPE_FLOAT, 0, 0.0, 10.0, NULL},
	{"subfps", &sub_fps, CONF_TYPE_FLOAT, 0, 0.0, 10.0, NULL},
        {"noautosub", &sub_auto, CONF_TYPE_FLAG, 0, 1, 0, NULL},
	{"unicode", &sub_unicode, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"nounicode", &sub_unicode, CONF_TYPE_FLAG, 0, 1, 0, NULL},
	{"utf8", &sub_utf8, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"noutf8", &sub_utf8, CONF_TYPE_FLAG, 0, 1, 0, NULL},
#endif
#ifdef USE_OSD
	{"font", &font_name, CONF_TYPE_STRING, 0, 0, 0, NULL},
	{"ffactor", &font_factor, CONF_TYPE_FLOAT, CONF_RANGE, 0.0, 10.0, NULL},
#endif
//	{"bg", &play_in_bg, CONF_TYPE_FLAG, 0, 0, 1, NULL},
//	{"nobg", &play_in_bg, CONF_TYPE_FLAG, 0, 1, 0, NULL},
	{"sb", &seek_to_byte, CONF_TYPE_INT, CONF_MIN, 0, 0, NULL},
	{"ss", &seek_to_sec, CONF_TYPE_STRING, CONF_MIN, 0, 0, NULL},
	{"sstep", &step_sec, CONF_TYPE_INT, CONF_MIN, 0, 0, NULL},
	{"noloop", &loop_times, CONF_TYPE_FLAG, 0, 0, -1, NULL},
	{"loop", &loop_times, CONF_TYPE_INT, CONF_RANGE, -1, 10000, NULL},
	{"sound", &has_audio, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"nosound", &has_audio, CONF_TYPE_FLAG, 0, 1, 0, NULL},
	{"abs", &ao_data.buffersize, CONF_TYPE_INT, CONF_MIN, 0, 0, NULL},
	{"delay", &audio_delay, CONF_TYPE_FLOAT, CONF_RANGE, -10.0, 10.0, NULL},

	{"alsa", "Option -alsa has been removed, new audio code doesn't need it! Remove it from your config file!\n",
            CONF_TYPE_PRINT, 0, 0, 0, NULL},
	{"noalsa", "Option -noalsa has been removed, new audio code doesn't need it! Remove it from your config file!\n",
            CONF_TYPE_PRINT, 0, 0, 0, NULL},

	{"framedrop", &frame_dropping, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"hardframedrop", &frame_dropping, CONF_TYPE_FLAG, 0, 0, 2, NULL},
	{"noframedrop", &frame_dropping, CONF_TYPE_FLAG, 0, 1, 0, NULL},

	{"autoq", &auto_quality, CONF_TYPE_INT, CONF_RANGE, 0, 100, NULL},

	{"benchmark", &benchmark, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	
	{"dumpfile", &stream_dump_name, CONF_TYPE_STRING, 0, 0, 0, NULL},
	{"dumpaudio", &stream_dump_type, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"dumpvideo", &stream_dump_type, CONF_TYPE_FLAG, 0, 0, 2, NULL},
	{"dumpsub", &stream_dump_type, CONF_TYPE_FLAG, 0, 0, 3, NULL},
	{"dumpmpsub", &stream_dump_type, CONF_TYPE_FLAG, 0, 0, 4, NULL},
	{"dumpstream", &stream_dump_type, CONF_TYPE_FLAG, 0, 0, 5, NULL},

	{"aofile", &ao_outputfilename, CONF_TYPE_STRING, 0, 0, 0, NULL},
	{"waveheader", &ao_pcm_waveheader, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"nowaveheader", &ao_pcm_waveheader, CONF_TYPE_FLAG, 0, 1, 0, NULL},

	{"dshow", &allow_dshow, CONF_TYPE_FLAG, 0, 0, 1, NULL}, // Is this still needed? atmos ::
	{"nodshow", &allow_dshow, CONF_TYPE_FLAG, 0, 1, 0, NULL},

#ifdef HAVE_PNG
	{"z", &z_compression, CONF_TYPE_INT, CONF_RANGE, 0, 9, NULL},
#endif
#ifdef HAVE_SDL
	{"sdl", "Use -vo sdl:driver instead of -vo sdl -sdl driver\n",
	    CONF_TYPE_PRINT, 0, 0, 0, NULL},
	{"noxv", &sdl_noxv, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"forcexv", &sdl_forcexv, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"sdla", "Use -ao sdl:driver instead of -ao sdl -sdla driver\n",
	    CONF_TYPE_PRINT, 0, 0, 0, NULL},
#endif	
	{"x", &opt_screen_size_x, CONF_TYPE_INT, CONF_RANGE, 0, 4096, NULL},
	{"y", &opt_screen_size_y, CONF_TYPE_INT, CONF_RANGE, 0, 4096, NULL},
	{"xy", &screen_size_xy, CONF_TYPE_INT, CONF_RANGE, 0, 4096, NULL},
	{"screenw", &vo_screenwidth, CONF_TYPE_INT, CONF_RANGE, 0, 4096, NULL},
	{"screenh", &vo_screenheight, CONF_TYPE_INT, CONF_RANGE, 0, 4096, NULL},
	{"aspect", &movie_aspect, CONF_TYPE_FLOAT, CONF_RANGE, 0.2, 3.0, NULL},
	{"noaspect", &movie_aspect, CONF_TYPE_FLAG, 0, 0, 0, NULL},
	{"monitoraspect", &monitor_aspect, CONF_TYPE_FLOAT, CONF_RANGE, 0.2, 3.0, NULL},
        {"vm", &vidmode, CONF_TYPE_FLAG, 0, 0, 1, NULL},
        {"novm", &vidmode, CONF_TYPE_FLAG, 0, 1, 0, NULL},
	{"fs", &fullscreen, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"nofs", &fullscreen, CONF_TYPE_FLAG, 0, 1, 0, NULL},
        {"zoom", &softzoom, CONF_TYPE_FLAG, 0, 0, 1, NULL},
        {"nozoom", &softzoom, CONF_TYPE_FLAG, 0, 1, 0, NULL},
        {"flip", &flip, CONF_TYPE_FLAG, 0, -1, 1, NULL},
        {"noflip", &flip, CONF_TYPE_FLAG, 0, -1, 0, NULL},
       
#ifndef USE_LIBVO2
        {"bpp", &vo_dbpp, CONF_TYPE_INT, CONF_RANGE, 0, 32, NULL},
	{"fsmode", &vo_fsmode, CONF_TYPE_INT, CONF_RANGE, 0, 15, NULL},
	{"double", &vo_doublebuffering, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"nodouble", &vo_doublebuffering, CONF_TYPE_FLAG, 0, 1, 0, NULL},
	{"brightness",&vo_gamma_brightness, CONF_TYPE_INT, CONF_RANGE, -1000, 1000, NULL},
	{"saturation",&vo_gamma_saturation, CONF_TYPE_INT, CONF_RANGE, -1000, 1000, NULL},
	{"contrast",&vo_gamma_contrast, CONF_TYPE_INT, CONF_RANGE, -1000, 1000, NULL},
	{"hue",&vo_gamma_hue, CONF_TYPE_INT, CONF_RANGE, -1000, 1000, NULL},
	{"red_intense",&vo_gamma_red_intense, CONF_TYPE_INT, CONF_RANGE, -1000, 1000, NULL},
	{"green_intense",&vo_gamma_green_intense, CONF_TYPE_INT, CONF_RANGE, -1000, 1000, NULL},
	{"blue_intense",&vo_gamma_blue_intense, CONF_TYPE_INT, CONF_RANGE, -1000, 1000, NULL},
#endif

#ifdef HAVE_AA
	{"aa*",	vo_aa_parseoption,  CONF_TYPE_FUNC_FULL, 0, 0, 0 , &vo_aa_revertoption},
#endif

#ifdef HAVE_ZR
	{"zr*", vo_zr_parseoption, CONF_TYPE_FUNC_FULL, 0, 0, 0, &vo_zr_revertoption },
#endif

#ifdef HAVE_LIRC
	{"lircconf", &lirc_configfile, CONF_TYPE_STRING, CONF_GLOBAL, 0, 0, NULL}, 
#endif

#ifdef USE_DVDREAD
	{"alang", &audio_lang, CONF_TYPE_STRING, 0, 0, 0, NULL},
	{"slang", &dvdsub_lang, CONF_TYPE_STRING, 0, 0, 0, NULL},
#endif

	{"gui", &use_gui, CONF_TYPE_FLAG, CONF_GLOBAL, 0, 1, NULL},
	{"nogui", &use_gui, CONF_TYPE_FLAG, CONF_GLOBAL, 1, 0, NULL},
      
#ifdef HAVE_NEW_GUI
	{"skin", &skinName, CONF_TYPE_STRING, CONF_GLOBAL, 0, 0, NULL},
#endif

	{"playlist", NULL, CONF_TYPE_STRING, 0, 0, 0, NULL},
	{"dapsync", &dapsync, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"nodapsync", &dapsync, CONF_TYPE_FLAG, 0, 1, 0, NULL},
	{"softsleep", &softsleep, CONF_TYPE_FLAG, 0, 0, 1, NULL},

	{"slave", &slave_mode, CONF_TYPE_FLAG,CONF_GLOBAL , 0, 1, NULL},

#ifdef HAVE_X11
	{"wid", &WinID, CONF_TYPE_INT, 0, 0, 0, NULL},
	{"rootwin", &WinID, CONF_TYPE_FLAG, 0, -1, 0, NULL},
#endif

#ifdef HAVE_XINERAMA
	{"xineramascreen", &xinerama_screen, CONF_TYPE_INT, CONF_RANGE, 0, 32, NULL},
#endif

#define MAIN_CONF
#include "cfg-common.h"
#undef MAIN_CONF
        
	{"quiet", &quiet, CONF_TYPE_FLAG, CONF_GLOBAL, 0, 1, NULL},
	{"verbose", &verbose, CONF_TYPE_INT, CONF_RANGE|CONF_GLOBAL, 0, 100, NULL},
	{"v", cfg_inc_verbose, CONF_TYPE_FUNC, CONF_GLOBAL|CONF_NOSAVE, 0, 0, NULL},
	{"-help", help_text, CONF_TYPE_PRINT, CONF_NOCFG|CONF_GLOBAL, 0, 0, NULL},
	{"help", help_text, CONF_TYPE_PRINT, CONF_NOCFG|CONF_GLOBAL, 0, 0, NULL},
	{"h", help_text, CONF_TYPE_PRINT, CONF_NOCFG|CONF_GLOBAL, 0, 0, NULL},
	{NULL, NULL, 0, 0, 0, 0, NULL}
};

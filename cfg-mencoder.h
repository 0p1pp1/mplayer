/*
 * config for cfgparser
 */

#include "cfg-common.h"

#ifdef USE_FAKE_MONO
extern int fakemono; // defined in dec_audio.c
#endif
#ifdef HAVE_ODIVX_POSTPROCESS
extern int use_old_pp;
#endif

extern int sws_flags;
extern int readPPOpt(void *, char *arg);
extern int readNPPOpt(void *conf, char *arg);
extern void revertPPOpt(void *conf, char* opt);

#ifdef HAVE_DIVX4ENCORE
struct config divx4opts_conf[]={
	{"br", &divx4_param.bitrate, CONF_TYPE_INT, CONF_RANGE, 4, 24000000, NULL},
	{"rc_period", &divx4_param.rc_period, CONF_TYPE_INT, 0,0,0, NULL},
	{"rc_reaction_period", &divx4_param.rc_reaction_period, CONF_TYPE_INT, 0,0,0, NULL},
	{"rc_reaction_ratio", &divx4_param.rc_reaction_ratio, CONF_TYPE_INT, 0,0,0, NULL},
	{"min_quant", &divx4_param.min_quantizer, CONF_TYPE_INT, CONF_RANGE,0,32, NULL},
	{"max_quant", &divx4_param.max_quantizer, CONF_TYPE_INT, CONF_RANGE,0,32, NULL},
	{"key", &divx4_param.max_key_interval, CONF_TYPE_INT, CONF_MIN,0,0, NULL},
	{"deinterlace", &divx4_param.deinterlace, CONF_TYPE_FLAG, 0,0,1, NULL},
	{"q", &divx4_param.quality, CONF_TYPE_INT, CONF_RANGE, 1, 5, NULL},
	{"crispness", &divx4_crispness, CONF_TYPE_INT, CONF_RANGE,0,100, NULL},
	{"help", "TODO: divx4opts help!\n", CONF_TYPE_PRINT, CONF_NOCFG, 0, 0, NULL},
	{NULL, NULL, 0, 0, 0, 0, NULL}
};
#endif

#ifdef HAVE_MP3LAME
struct config lameopts_conf[]={
	{"q", &lame_param_quality, CONF_TYPE_INT, CONF_RANGE, 0, 9, NULL},
	{"vbr", &lame_param_vbr, CONF_TYPE_INT, CONF_RANGE, 0, vbr_max_indicator, NULL},
	{"cbr", &lame_param_vbr, CONF_TYPE_FLAG, 0, 0, 0, NULL},
	{"abr", &lame_param_vbr, CONF_TYPE_FLAG, 0, 0, vbr_abr, NULL},
	{"mode", &lame_param_mode, CONF_TYPE_INT, CONF_RANGE, 0, MAX_INDICATOR, NULL},
	{"padding", &lame_param_padding, CONF_TYPE_INT, CONF_RANGE, 0, PAD_MAX_INDICATOR, NULL},
	{"br", &lame_param_br, CONF_TYPE_INT, CONF_RANGE, 0, 1024, NULL},
	{"ratio", &lame_param_ratio, CONF_TYPE_INT, CONF_RANGE, 0, 100, NULL},
	{"help", "TODO: lameopts help!\n", CONF_TYPE_PRINT, CONF_NOCFG, 0, 0, NULL},
	{NULL, NULL, 0, 0, 0, 0, NULL}
};
#endif

#ifdef USE_LIBAVCODEC
struct config lavcopts_conf[]={
	{"vcodec", &lavc_param_vcodec, CONF_TYPE_STRING, 0, 0, 0, NULL},
	{"vbitrate", &lavc_param_vbitrate, CONF_TYPE_INT, CONF_RANGE, 4, 24000000, NULL},
	{"vratetol", &lavc_param_vrate_tolerance, CONF_TYPE_INT, CONF_RANGE, 4, 24000000, NULL},
	{"vhq", &lavc_param_vhq, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"v4mv", &lavc_param_v4mv, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"vme", &lavc_param_vme, CONF_TYPE_INT, CONF_RANGE, 0, 5, NULL},
	{"vqscale", &lavc_param_vqscale, CONF_TYPE_INT, CONF_RANGE, 1, 31, NULL},
	{"vqmin", &lavc_param_vqmin, CONF_TYPE_INT, CONF_RANGE, 1, 31, NULL},
	{"vqmax", &lavc_param_vqmax, CONF_TYPE_INT, CONF_RANGE, 1, 31, NULL},
	{"vqdiff", &lavc_param_vqdiff, CONF_TYPE_INT, CONF_RANGE, 1, 31, NULL},
	{"vqcomp", &lavc_param_vqcompress, CONF_TYPE_FLOAT, CONF_RANGE, 0.0, 1.0, NULL},
	{"vqblur", &lavc_param_vqblur, CONF_TYPE_FLOAT, CONF_RANGE, 0.0, 1.0, NULL},
	{"keyint", &lavc_param_keyint, CONF_TYPE_INT, 0, 0, 0, NULL},
	{NULL, NULL, 0, 0, 0, 0, NULL}
};
#endif

#ifdef USE_WIN32DLL
struct config vfwopts_conf[]={
	{NULL, NULL, 0, 0, 0, 0, NULL}
};
#endif

struct config ovc_conf[]={
	{"copy", &out_video_codec, CONF_TYPE_FLAG, 0, 0, VCODEC_COPY, NULL},
	{"frameno", &out_video_codec, CONF_TYPE_FLAG, 0, 0, VCODEC_FRAMENO, NULL},
	{"divx4", &out_video_codec, CONF_TYPE_FLAG, 0, 0, VCODEC_DIVX4, NULL},
	{"raw", &out_video_codec, CONF_TYPE_FLAG, 0, 0, VCODEC_RAW, NULL},
	{"lavc", &out_video_codec, CONF_TYPE_FLAG, 0, 0, VCODEC_LIBAVCODEC, NULL},
	{"null", &out_video_codec, CONF_TYPE_FLAG, 0, 0, VCODEC_NULL, NULL},
	{"rawrgb", &out_video_codec, CONF_TYPE_FLAG, 0, 0, VCODEC_RAWRGB, NULL},
	{"vfw", &out_video_codec, CONF_TYPE_FLAG, 0, 0, VCODEC_VFW, NULL},
	{"help", "\nAvailable codecs:\n   copy\n   frameno\n   divx4\n   raw\n   lavc\n   rawrgb\n   vfw\n   null\n\n", CONF_TYPE_PRINT, CONF_NOCFG, 0, 0, NULL},
	{NULL, NULL, 0, 0, 0, 0, NULL}
};

struct config oac_conf[]={
	{"copy", &out_audio_codec, CONF_TYPE_FLAG, 0, 0, ACODEC_COPY, NULL},
	{"pcm", &out_audio_codec, CONF_TYPE_FLAG, 0, 0, ACODEC_PCM, NULL},
#ifdef HAVE_MP3LAME
	{"mp3lame", &out_audio_codec, CONF_TYPE_FLAG, 0, 0, ACODEC_VBRMP3, NULL},
	{"help", "\nAvailable codecs:\n   copy\n   pcm\n   mp3lame\n\n", CONF_TYPE_PRINT, CONF_NOCFG, 0, 0, NULL},
#else
	{"mp3lame", "MPlayer was compiled without libmp3lame support!\n", CONF_TYPE_PRINT, CONF_NOCFG, 0, 0, NULL},
	{"help", "\nAvailable codecs:\n   copy\n   pcm\n\n", CONF_TYPE_PRINT, CONF_NOCFG, 0, 0, NULL},
#endif
	{NULL, NULL, 0, 0, 0, 0, NULL}
};

static config_t mencoder_opts[]={
	/* name, pointer, type, flags, min, max */
	{"include", cfg_include, CONF_TYPE_FUNC_PARAM, CONF_NOSAVE, 0, 0, NULL}, /* this must be the first!!! */

//	{"sb", &seek_to_byte, CONF_TYPE_INT, CONF_MIN, 0, 0, NULL},
	{"ss", &seek_to_sec, CONF_TYPE_STRING, CONF_MIN, 0, 0, NULL},
	{"endpos", parse_end_at, CONF_TYPE_FUNC_PARAM, 0, 0, 0, NULL},
	
	{"ofps", &force_ofps, CONF_TYPE_FLOAT, CONF_MIN, 0, 0, NULL},
	{"o", &out_filename, CONF_TYPE_STRING, 0, 0, 0, NULL},

	{"x", &vo_w, CONF_TYPE_INT, CONF_RANGE, 0, 4096, NULL},
	{"y", &vo_h, CONF_TYPE_INT, CONF_RANGE, 0, 4096, NULL},

    {"x0", &crop_x0, CONF_TYPE_INT, CONF_RANGE, 0, 4096, NULL},
	{"y0", &crop_y0, CONF_TYPE_INT, CONF_RANGE, 0, 4096, NULL},

    {"xsize", &crop_width, CONF_TYPE_INT, CONF_RANGE, 0, 4096, NULL},
	{"ysize", &crop_height, CONF_TYPE_INT, CONF_RANGE, 0, 4096, NULL},

	{"mp3file", &mp3_filename, CONF_TYPE_STRING, 0, 0, 0, NULL},
	{"ac3file", &ac3_filename, CONF_TYPE_STRING, 0, 0, 0, NULL},

//	{"oac", &out_audio_codec, CONF_TYPE_STRING, 0, 0, 0, NULL},
//	{"ovc", &out_video_codec, CONF_TYPE_STRING, 0, 0, 0, NULL},
	{"oac", oac_conf, CONF_TYPE_SUBCONFIG, 0, 0, 0, NULL},
	{"ovc", ovc_conf, CONF_TYPE_SUBCONFIG, 0, 0, 0, NULL},

	{"ffourcc", &force_fourcc, CONF_TYPE_STRING, 0, 4, 4, NULL},

#ifdef HAVE_DIVX4ENCORE
	{"pass", &pass, CONF_TYPE_INT, CONF_RANGE,0,2, NULL},
	{"passlogfile", &passtmpfile, CONF_TYPE_STRING, 0, 0, 0, NULL},
	
	{"divx4opts", divx4opts_conf, CONF_TYPE_SUBCONFIG, 0, 0, 0, NULL},
#endif
#ifdef HAVE_MP3LAME
	{"lameopts", lameopts_conf, CONF_TYPE_SUBCONFIG, 0, 0, 0, NULL},
#endif
#ifdef USE_LIBAVCODEC
	{"lavcopts", lavcopts_conf, CONF_TYPE_SUBCONFIG, 0, 0, 0, NULL},
#endif
#ifdef USE_WIN32DLL
	{"vfwopts", vfwopts_conf, CONF_TYPE_SUBCONFIG, 0, 0, 0, NULL},
#endif

#define MAIN_CONF
#include "cfg-common.h"
#undef MAIN_CONF

//	{"quiet", &quiet, CONF_TYPE_FLAG, 0, 0, 1, NULL},
	{"verbose", &verbose, CONF_TYPE_INT, CONF_RANGE|CONF_GLOBAL, 0, 100, NULL},
	{"v", cfg_inc_verbose, CONF_TYPE_FUNC, CONF_GLOBAL, 0, 0, NULL},
//	{"-help", help_text, CONF_TYPE_PRINT, CONF_NOCFG, 0, 0, NULL},
//	{"help", help_text, CONF_TYPE_PRINT, CONF_NOCFG, 0, 0, NULL},
//	{"h", help_text, CONF_TYPE_PRINT, CONF_NOCFG, 0, 0, NULL},
	{NULL, NULL, 0, 0, 0, 0, NULL}
};

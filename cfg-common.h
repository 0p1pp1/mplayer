
// ------------------------- stream options --------------------

#ifdef USE_STREAM_CACHE
	{"cache", &stream_cache_size, CONF_TYPE_INT, CONF_RANGE, 4, 65536},
#else
	{"cache", "MPlayer was compiled WITHOUT cache2 support", CONF_TYPE_PRINT, CONF_NOCFG, 0, 0},
#endif
	{"vcd", &vcd_track, CONF_TYPE_INT, CONF_RANGE, 1, 99},
#ifdef USE_DVDREAD
	{"dvd", &dvd_title, CONF_TYPE_INT, CONF_RANGE, 1, 99},
	{"dvdangle", &dvd_angle, CONF_TYPE_INT, CONF_RANGE, 1, 99},
	{"chapter", &dvd_chapter, CONF_TYPE_INT, CONF_RANGE, 1, 99},
#else
	{"dvd", "MPlayer was compiled WITHOUT libdvdread support!\n", CONF_TYPE_PRINT, CONF_NOCFG, 0, 0},
#endif

#ifdef HAVE_LIBCSS
        {"dvdauth", &dvd_auth_device, CONF_TYPE_STRING, 0, 0, 0},
        {"dvdkey", &dvdimportkey, CONF_TYPE_STRING, 0, 0, 0},
//	{"dvd", "Option -dvd will be \"full disk\" mode, old meaning has been renamed to -dvdauth.\n", CONF_TYPE_PRINT, CONF_NOCFG, 0, 0},
#else
//        {"dvd", "DVD support was not compiled in. See file DOCS/DVD.\n",
//            CONF_TYPE_PRINT, CONF_NOCFG, 0 , 0},
#ifdef USE_DVDREAD
        {"dvdkey", "MPlayer was compiled with libdvdread support, this option not available.\n",
            CONF_TYPE_PRINT, CONF_NOCFG, 0 , 0},
        {"dvdauth", "MPlayer was compiled with libdvdread support! Use option -dvd !\n",
            CONF_TYPE_PRINT, CONF_NOCFG, 0 , 0},
#else
        {"dvdkey", "DVD support was not compiled in. See file DOCS/DVD.\n",
            CONF_TYPE_PRINT, CONF_NOCFG, 0 , 0},
        {"dvdauth", "DVD support was not compiled in. See file DOCS/DVD.\n",
            CONF_TYPE_PRINT, CONF_NOCFG, 0 , 0},
#endif
#endif

// ------------------------- demuxer options --------------------

	{"bps", &pts_from_bps, CONF_TYPE_FLAG, 0, 0, 1},
	{"nobps", &pts_from_bps, CONF_TYPE_FLAG, 0, 1, 0},

	{"ni", &force_ni, CONF_TYPE_FLAG, 0, 0, 1},
	{"noni", &force_ni, CONF_TYPE_FLAG, 0, 1, 0},

	{"noidx", &index_mode, CONF_TYPE_FLAG, 0, -1, 0},
	{"idx", &index_mode, CONF_TYPE_FLAG, 0, -1, 1},
	{"forceidx", &index_mode, CONF_TYPE_FLAG, 0, -1, 2},

	{"aid", &audio_id, CONF_TYPE_INT, CONF_RANGE, 0, 255},
	{"vid", &video_id, CONF_TYPE_INT, CONF_RANGE, 0, 255},
	{"sid", &dvdsub_id, CONF_TYPE_INT, CONF_RANGE, 0, 31},

// ------------------------- a-v sync options --------------------

	{"frames", &play_n_frames, CONF_TYPE_INT, CONF_MIN, 0, 0},

	{"mc", &default_max_pts_correction, CONF_TYPE_FLOAT, CONF_RANGE, 0, 10},
	{"fps", &force_fps, CONF_TYPE_FLOAT, CONF_MIN, 0, 0},
	{"srate", &force_srate, CONF_TYPE_INT, CONF_RANGE, 1000, 8*48000},

// ------------------------- codec/pp options --------------------

#ifdef USE_FAKE_MONO
	{"stereo", &fakemono, CONF_TYPE_INT, CONF_RANGE, 0, 2},
#endif

	{"afm", &audio_family, CONF_TYPE_INT, CONF_RANGE, 0, 13}, // keep ranges in sync
	{"vfm", &video_family, CONF_TYPE_INT, CONF_RANGE, 0, 10}, // with codec-cfg.c
	{"ac", &audio_codec, CONF_TYPE_STRING, 0, 0, 0},
	{"vc", &video_codec, CONF_TYPE_STRING, 0, 0, 0},

	{"divxq", "Option -divxq has been renamed to -pp (postprocessing), use -pp !\n",
            CONF_TYPE_PRINT, 0, 0, 0},
	{"pp", &divx_quality, CONF_TYPE_INT, CONF_MIN, 0, 63},
#ifdef HAVE_ODIVX_POSTPROCESS
        {"oldpp", &use_old_pp, CONF_TYPE_FLAG, 0, 0, 1},
#else
        {"oldpp", "MPlayer was compiled without opendivx library", CONF_TYPE_PRINT, CONF_NOCFG, 0, 0},
#endif


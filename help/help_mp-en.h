// $Revision$
// MASTER FILE. Use this file as base for translations.
// Translated files should be sent to the mplayer-DOCS mailing list or
// to the help messages maintainer, see DOCS/tech/MAINTAINERS.
// The header of the translated file should contain credits and contact
// information. Before major releases we will notify all translators to update
// their files. Please do not simply translate and forget this, outdated
// translations quickly become worthless. To help us spot outdated files put a
// note like "sync'ed with help_mp-en.h XXX" in the header of the translation.
// Do NOT translate the above lines, just follow the instructions.

// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"Usage:   mplayer [options] [url|path/]filename\n"
"\n"
"Basic options: (complete list in the man page)\n"
" -vo <drv[:dev]>  select video output driver & device ('-vo help' for a list)\n"
" -ao <drv[:dev]>  select audio output driver & device ('-ao help' for a list)\n"
#ifdef HAVE_VCD
" vcd://<trackno>   play (S)VCD (Super Video CD) track (raw device, no mount)\n"
#endif
#ifdef USE_DVDREAD
" dvd://<titleno>   play DVD title from device instead of plain file\n"
" -alang/-slang    select DVD audio/subtitle language (by 2-char country code)\n"
#endif
" -ss <timepos>    seek to given (seconds or hh:mm:ss) position\n"
" -nosound         do not play sound\n"
" -fs              fullscreen playback (or -vm, -zoom, details in the man page)\n"
" -x <x> -y <y>    set display resolution (for use with -vm or -zoom)\n"
" -sub <file>      specify subtitle file to use (also see -subfps, -subdelay)\n"
" -playlist <file> specify playlist file\n"
" -vid x -aid y    select video (x) and audio (y) stream to play\n"
" -fps x -srate y  change video (x fps) and audio (y Hz) rate\n"
" -pp <quality>    enable postprocessing filter (details in the man page)\n"
" -framedrop       enable frame dropping (for slow machines)\n"
"\n"
"Basic keys: (complete list in the man page, also check input.conf)\n"
" <-  or  ->       seek backward/forward 10 seconds\n"
" down or up       seek backward/forward  1 minute\n"
" pgdown or pgup   seek backward/forward 10 minutes\n"
" < or >           step backward/forward in playlist\n"
" p or SPACE       pause movie (press any key to continue)\n"
" q or ESC         stop playing and quit program\n"
" + or -           adjust audio delay by +/- 0.1 second\n"
" o                cycle OSD mode:  none / seekbar / seekbar + timer\n"
" * or /           increase or decrease PCM volume\n"
" x or z           adjust subtitle delay by +/- 0.1 second\n"
" r or t           adjust subtitle position up/down, also see -vf expand\n"
"\n"
" * * * SEE THE MAN PAGE FOR DETAILS, FURTHER (ADVANCED) OPTIONS AND KEYS * * *\n"
"\n";
#endif

#define MSGTR_SamplesWanted "Samples of this format are needed to improve support. Please contact the developers.\n"

// ========================= MPlayer messages ===========================

// mplayer.c:

#define MSGTR_Exiting "\nExiting...\n"
#define MSGTR_ExitingHow "\nExiting... (%s)\n"
#define MSGTR_Exit_quit "Quit"
#define MSGTR_Exit_eof "End of file"
#define MSGTR_Exit_error "Fatal error"
#define MSGTR_IntBySignal "\nMPlayer interrupted by signal %d in module: %s\n"
#define MSGTR_NoHomeDir "Cannot find HOME directory.\n"
#define MSGTR_GetpathProblem "get_path(\"config\") problem\n"
#define MSGTR_CreatingCfgFile "Creating config file: %s\n"
#define MSGTR_CopyCodecsConf "(Copy/link etc/codecs.conf from the MPlayer sources to ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "Using built-in default codecs.conf.\n"
#define MSGTR_CantLoadFont "Cannot load font: %s\n"
#define MSGTR_CantLoadSub "Cannot load subtitles: %s\n"
#define MSGTR_DumpSelectedStreamMissing "dump: FATAL: Selected stream missing!\n"
#define MSGTR_CantOpenDumpfile "Cannot open dump file.\n"
#define MSGTR_CoreDumped "Core dumped ;)\n"
#define MSGTR_FPSnotspecified "FPS not specified in the header or invalid, use the -fps option.\n"
#define MSGTR_TryForceAudioFmtStr "Trying to force audio codec driver family %s...\n"
#define MSGTR_CantFindAudioCodec "Cannot find codec for audio format 0x%X.\n"
#define MSGTR_RTFMCodecs "Read DOCS/HTML/en/codecs.html!\n"
#define MSGTR_TryForceVideoFmtStr "Trying to force video codec driver family %s...\n"
#define MSGTR_CantFindVideoCodec "Cannot find codec matching selected -vo and video format 0x%X.\n"
#define MSGTR_CannotInitVO "FATAL: Cannot initialize video driver.\n"
#define MSGTR_CannotInitAO "Could not open/initialize audio device -> no sound.\n"
#define MSGTR_StartPlaying "Starting playback...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"           ************************************************\n"\
"           **** Your system is too SLOW to play this!  ****\n"\
"           ************************************************\n\n"\
"Possible reasons, problems, workarounds:\n"\
"- Most common: broken/buggy _audio_ driver\n"\
"  - Try -ao sdl or use the OSS emulation of ALSA.\n"\
"  - Experiment with different values for -autosync, 30 is a good start.\n"\
"- Slow video output\n"\
"  - Try a different -vo driver (-vo help for a list) or try -framedrop!\n"\
"- Slow CPU\n"\
"  - Don't try to play a big DVD/DivX on a slow CPU! Try some of the lavdopts,\n"\
"    e.g. -vfm ffmpeg -lavdopts lowres=1:fast:skiploopfilter=all.\n"\
"- Broken file\n"\
"  - Try various combinations of -nobps -ni -forceidx -mc 0.\n"\
"- Slow media (NFS/SMB mounts, DVD, VCD etc)\n"\
"  - Try -cache 8192.\n"\
"- Are you using -cache to play a non-interleaved AVI file?\n"\
"  - Try -nocache.\n"\
"Read DOCS/HTML/en/video.html for tuning/speedup tips.\n"\
"If none of this helps you, read DOCS/HTML/en/bugreports.html.\n\n"

#define MSGTR_NoGui "MPlayer was compiled WITHOUT GUI support.\n"
#define MSGTR_GuiNeedsX "MPlayer GUI requires X11.\n"
#define MSGTR_Playing "Playing %s.\n"
#define MSGTR_NoSound "Audio: no sound\n"
#define MSGTR_FPSforced "FPS forced to be %5.3f  (ftime: %5.3f).\n"
#define MSGTR_CompiledWithRuntimeDetection "Compiled with runtime CPU detection.\n"
#define MSGTR_CompiledWithCPUExtensions "Compiled for x86 CPU with extensions:"
#define MSGTR_AvailableVideoOutputDrivers "Available video output drivers:\n"
#define MSGTR_AvailableAudioOutputDrivers "Available audio output drivers:\n"
#define MSGTR_AvailableAudioCodecs "Available audio codecs:\n"
#define MSGTR_AvailableVideoCodecs "Available video codecs:\n"
#define MSGTR_AvailableAudioFm "Available (compiled-in) audio codec families/drivers:\n"
#define MSGTR_AvailableVideoFm "Available (compiled-in) video codec families/drivers:\n"
#define MSGTR_AvailableFsType "Available fullscreen layer change modes:\n"
#define MSGTR_UsingRTCTiming "Using Linux hardware RTC timing (%ldHz).\n"
#define MSGTR_CannotReadVideoProperties "Video: Cannot read properties.\n"
#define MSGTR_NoStreamFound "No stream found.\n"
#define MSGTR_ErrorInitializingVODevice "Error opening/initializing the selected video_out (-vo) device.\n"
#define MSGTR_ForcedVideoCodec "Forced video codec: %s\n"
#define MSGTR_ForcedAudioCodec "Forced audio codec: %s\n"
#define MSGTR_Video_NoVideo "Video: no video\n"
#define MSGTR_NotInitializeVOPorVO "\nFATAL: Could not initialize video filters (-vf) or video output (-vo).\n"
#define MSGTR_Paused "\n  =====  PAUSE  =====\r" // no more than 23 characters (status line for audio files)
#define MSGTR_PlaylistLoadUnable "\nUnable to load playlist %s.\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- MPlayer crashed by an 'Illegal Instruction'.\n"\
"  It may be a bug in our new runtime CPU-detection code...\n"\
"  Please read DOCS/HTML/en/bugreports.html.\n"
#define MSGTR_Exit_SIGILL \
"- MPlayer crashed by an 'Illegal Instruction'.\n"\
"  It usually happens when you run it on a CPU different than the one it was\n"\
"  compiled/optimized for.\n"\
"  Verify this!\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- MPlayer crashed by bad usage of CPU/FPU/RAM.\n"\
"  Recompile MPlayer with --enable-debug and make a 'gdb' backtrace and\n"\
"  disassembly. Details in DOCS/HTML/en/bugreports_what.html#bugreports_crash.\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer crashed. This shouldn't happen.\n"\
"  It can be a bug in the MPlayer code _or_ in your drivers _or_ in your\n"\
"  gcc version. If you think it's MPlayer's fault, please read\n"\
"  DOCS/HTML/en/bugreports.html and follow the instructions there. We can't and\n"\
"  won't help unless you provide this information when reporting a possible bug.\n"
#define MSGTR_LoadingConfig "Loading config '%s'\n"
#define MSGTR_AddedSubtitleFile "SUB: added subtitle file (%d): %s\n"
#define MSGTR_RemovedSubtitleFile "SUB: removed subtitle file (%d): %s\n"
#define MSGTR_ErrorOpeningOutputFile "Error opening file [%s] for writing!\n"
#define MSGTR_CommandLine "CommandLine:"
#define MSGTR_RTCDeviceNotOpenable "Failed to open %s: %s (it should be readable by the user.)\n"
#define MSGTR_LinuxRTCInitErrorIrqpSet "Linux RTC init error in ioctl (rtc_irqp_set %lu): %s\n"
#define MSGTR_IncreaseRTCMaxUserFreq "Try adding \"echo %lu > /proc/sys/dev/rtc/max-user-freq\" to your system startup scripts.\n"
#define MSGTR_LinuxRTCInitErrorPieOn "Linux RTC init error in ioctl (rtc_pie_on): %s\n"
#define MSGTR_UsingTimingType "Using %s timing.\n"
#define MSGTR_NoIdleAndGui "The -idle option cannot be used with GMPlayer.\n"
#define MSGTR_MenuInitialized "Menu inited: %s\n"
#define MSGTR_MenuInitFailed "Menu init failed.\n"
#define MSGTR_Getch2InitializedTwice "WARNING: getch2_init called twice!\n"
#define MSGTR_DumpstreamFdUnavailable "Cannot dump this stream - no file descriptor available.\n"
#define MSGTR_FallingBackOnPlaylist "Falling back on trying to parse playlist %s...\n"
#define MSGTR_CantOpenLibmenuFilterWithThisRootMenu "Can't open libmenu video filter with root menu %s.\n"
#define MSGTR_AudioFilterChainPreinitError "Error at audio filter chain pre-init!\n"
#define MSGTR_LinuxRTCReadError "Linux RTC read error: %s\n"
#define MSGTR_SoftsleepUnderflow "Warning! Softsleep underflow!\n"
#define MSGTR_DvdnavNullEvent "DVDNAV Event NULL?!\n"
#define MSGTR_DvdnavHighlightEventBroken "DVDNAV Event: Highlight event broken\n"
#define MSGTR_DvdnavEvent "DVDNAV Event: %s\n"
#define MSGTR_DvdnavHighlightHide "DVDNAV Event: Highlight Hide\n"
#define MSGTR_DvdnavStillFrame "######################################## DVDNAV Event: Still Frame: %d sec(s)\n"
#define MSGTR_DvdnavNavStop "DVDNAV Event: Nav Stop\n"
#define MSGTR_DvdnavNavNOP "DVDNAV Event: Nav NOP\n"
#define MSGTR_DvdnavNavSpuStreamChangeVerbose "DVDNAV Event: Nav SPU Stream Change: phys: %d/%d/%d logical: %d\n"
#define MSGTR_DvdnavNavSpuStreamChange "DVDNAV Event: Nav SPU Stream Change: phys: %d logical: %d\n"
#define MSGTR_DvdnavNavAudioStreamChange "DVDNAV Event: Nav Audio Stream Change: phys: %d logical: %d\n"
#define MSGTR_DvdnavNavVTSChange "DVDNAV Event: Nav VTS Change\n"
#define MSGTR_DvdnavNavCellChange "DVDNAV Event: Nav Cell Change\n"
#define MSGTR_DvdnavNavSpuClutChange "DVDNAV Event: Nav SPU CLUT Change\n"
#define MSGTR_DvdnavNavSeekDone "DVDNAV Event: Nav Seek Done\n"
#define MSGTR_MenuCall "Menu call\n"

#define MSGTR_EdlOutOfMem "Can't allocate enough memory to hold EDL data.\n"
#define MSGTR_EdlRecordsNo "Read %d EDL actions.\n"
#define MSGTR_EdlQueueEmpty "There are no EDL actions to take care of.\n"
#define MSGTR_EdlCantOpenForWrite "Can't open EDL file [%s] for writing.\n"
#define MSGTR_EdlCantOpenForRead "Can't open EDL file [%s] for reading.\n"
#define MSGTR_EdlNOsh_video "Cannot use EDL without video, disabling.\n"
#define MSGTR_EdlNOValidLine "Invalid EDL line: %s\n"
#define MSGTR_EdlBadlyFormattedLine "Badly formatted EDL line [%d] Discarding.\n"
#define MSGTR_EdlBadLineOverlap "Last stop position was [%f]; next start is "\
"[%f]. Entries must be in chronological order, cannot overlap. Discarding.\n"
#define MSGTR_EdlBadLineBadStop "Stop time has to be after start time.\n"

// mplayer.c OSD

#define MSGTR_OSDenabled "enabled"
#define MSGTR_OSDdisabled "disabled"
#define MSGTR_OSDChannel "Channel: %s"
#define MSGTR_OSDSubDelay "Sub delay: %d ms"
#define MSGTR_OSDSpeed "Speed: x %6.2f"
#define MSGTR_OSDosd "OSD: %s"

// property values
#define MSGTR_Enabled "enabled"
#define MSGTR_EnabledEdl "enabled (edl)"
#define MSGTR_Disabled "disabled"
#define MSGTR_HardFrameDrop "hard"
#define MSGTR_Unknown "unknown"
#define MSGTR_Bottom "bottom"
#define MSGTR_Center "center"
#define MSGTR_Top "top"

// osd bar names
#define MSGTR_Volume "Volume"
#define MSGTR_Panscan "Panscan"
#define MSGTR_Gamma "Gamma"
#define MSGTR_Brightness "Brightness"
#define MSGTR_Contrast "Contrast"
#define MSGTR_Saturation "Saturation"
#define MSGTR_Hue "Hue"

// property state
#define MSGTR_MuteStatus "Mute: %s"
#define MSGTR_AVDelayStatus "A-V delay: %s"
#define MSGTR_OnTopStatus "Stay on top: %s"
#define MSGTR_RootwinStatus "Rootwin: %s"
#define MSGTR_BorderStatus "Border: %s"
#define MSGTR_FramedroppingStatus "Framedropping: %s"
#define MSGTR_VSyncStatus "VSync: %s"
#define MSGTR_SubSelectStatus "Subtitles: %s"
#define MSGTR_SubPosStatus "Sub position: %s/100"
#define MSGTR_SubAlignStatus "Sub alignment: %s"
#define MSGTR_SubDelayStatus "Sub delay: %s"
#define MSGTR_SubVisibleStatus "Subtitles: %s"
#define MSGTR_SubForcedOnlyStatus "Forced sub only: %s"

// mencoder.c:

#define MSGTR_UsingPass3ControllFile "Using pass3 control file: %s\n"
#define MSGTR_MissingFilename "\nFilename missing.\n\n"
#define MSGTR_CannotOpenFile_Device "Cannot open file/device.\n"
#define MSGTR_CannotOpenDemuxer "Cannot open demuxer.\n"
#define MSGTR_NoAudioEncoderSelected "\nNo audio encoder (-oac) selected. Select one (see -oac help) or use -nosound.\n"
#define MSGTR_NoVideoEncoderSelected "\nNo video encoder (-ovc) selected. Select one (see -ovc help).\n"
#define MSGTR_CannotOpenOutputFile "Cannot open output file '%s'.\n"
#define MSGTR_EncoderOpenFailed "Failed to open the encoder.\n"
#define MSGTR_MencoderWrongFormatAVI "\nWARNING: OUTPUT FILE FORMAT IS _AVI_. see -of help.\n"
#define MSGTR_MencoderWrongFormatMPG "\nWARNING: OUTPUT FILE FORMAT IS _MPEG_. see -of help.\n"
#define MSGTR_MissingOutputFilename "No output file specified, please see the -o option"
#define MSGTR_ForcingOutputFourcc "Forcing output fourcc to %x [%.4s]\n"
#define MSGTR_ForcingOutputAudiofmtTag "Forcing output audio format tag to 0x%x\n"
#define MSGTR_DuplicateFrames "\n%d duplicate frame(s)!\n"
#define MSGTR_SkipFrame "\nSkipping frame!\n"
#define MSGTR_ResolutionDoesntMatch "\nNew video file has different resolution or colorspace than the previous one.\n"
#define MSGTR_FrameCopyFileMismatch "\nAll video files must have identical fps, resolution, and codec for -ovc copy.\n"
#define MSGTR_AudioCopyFileMismatch "\nAll files must have identical audio codec and format for -oac copy.\n"
#define MSGTR_NoAudioFileMismatch "\nCannot mix video-only files with audio and video files. Try -nosound.\n"
#define MSGTR_NoSpeedWithFrameCopy "WARNING: -speed is not guaranteed to work correctly with -oac copy!\n"\
"Your encode might be broken!\n"
#define MSGTR_ErrorWritingFile "%s: Error writing file.\n"
#define MSGTR_RecommendedVideoBitrate "Recommended video bitrate for %s CD: %d\n"
#define MSGTR_VideoStreamResult "\nVideo stream: %8.3f kbit/s  (%d B/s)  size: %d bytes  %5.3f secs  %d frames\n"
#define MSGTR_AudioStreamResult "\nAudio stream: %8.3f kbit/s  (%d B/s)  size: %d bytes  %5.3f secs\n"
#define MSGTR_OpenedStream "success: format: %d  data: 0x%X - 0x%x\n"
#define MSGTR_VCodecFramecopy "videocodec: framecopy (%dx%d %dbpp fourcc=%x)\n"
#define MSGTR_ACodecFramecopy "audiocodec: framecopy (format=%x chans=%d rate=%d bits=%d B/s=%d sample-%d)\n"
#define MSGTR_CBRPCMAudioSelected "CBR PCM audio selected\n"
#define MSGTR_MP3AudioSelected "MP3 audio selected\n"
#define MSGTR_CannotAllocateBytes "Couldn't allocate %d bytes\n"
#define MSGTR_SettingAudioDelay "Setting audio delay to %5.3fs\n"
#define MSGTR_SettingVideoDelay "Setting video delay to %5.3fs\n"
#define MSGTR_SettingAudioInputGain "Setting audio input gain to %f\n"
#define MSGTR_LamePresetEquals "\npreset=%s\n\n"
#define MSGTR_LimitingAudioPreload "Limiting audio preload to 0.4s\n"
#define MSGTR_IncreasingAudioDensity "Increasing audio density to 4\n"
#define MSGTR_ZeroingAudioPreloadAndMaxPtsCorrection "Forcing audio preload to 0, max pts correction to 0\n"
#define MSGTR_CBRAudioByterate "\n\nCBR audio: %d bytes/sec, %d bytes/block\n"
#define MSGTR_LameVersion "LAME version %s (%s)\n\n"
#define MSGTR_InvalidBitrateForLamePreset "Error: The bitrate specified is out of the valid range for this preset\n"\
"\n"\
"When using this mode you must enter a value between \"8\" and \"320\"\n"\
"\n"\
"For further information try: \"-lameopts preset=help\"\n"
#define MSGTR_InvalidLamePresetOptions "Error: You did not enter a valid profile and/or options with preset\n"\
"\n"\
"Available profiles are:\n"\
"\n"\
"   <fast>        standard\n"\
"   <fast>        extreme\n"\
"                 insane\n"\
"   <cbr> (ABR Mode) - The ABR Mode is implied. To use it,\n"\
"                      simply specify a bitrate. For example:\n"\
"                      \"preset=185\" activates this\n"\
"                      preset and uses 185 as an average kbps.\n"\
"\n"\
"    Some examples:\n"\
"\n"\
"    \"-lameopts fast:preset=standard  \"\n"\
" or \"-lameopts  cbr:preset=192       \"\n"\
" or \"-lameopts      preset=172       \"\n"\
" or \"-lameopts      preset=extreme   \"\n"\
"\n"\
"For further information try: \"-lameopts preset=help\"\n"
#define MSGTR_LamePresetsLongInfo "\n"\
"The preset switches are designed to provide the highest possible quality.\n"\
"\n"\
"They have for the most part been subject to and tuned via rigorous double blind\n"\
"listening tests to verify and achieve this objective.\n"\
"\n"\
"These are continually updated to coincide with the latest developments that\n"\
"occur and as a result should provide you with nearly the best quality\n"\
"currently possible from LAME.\n"\
"\n"\
"To activate these presets:\n"\
"\n"\
"   For VBR modes (generally highest quality):\n"\
"\n"\
"     \"preset=standard\" This preset should generally be transparent\n"\
"                             to most people on most music and is already\n"\
"                             quite high in quality.\n"\
"\n"\
"     \"preset=extreme\" If you have extremely good hearing and similar\n"\
"                             equipment, this preset will generally provide\n"\
"                             slightly higher quality than the \"standard\"\n"\
"                             mode.\n"\
"\n"\
"   For CBR 320kbps (highest quality possible from the preset switches):\n"\
"\n"\
"     \"preset=insane\"  This preset will usually be overkill for most\n"\
"                             people and most situations, but if you must\n"\
"                             have the absolute highest quality with no\n"\
"                             regard to filesize, this is the way to go.\n"\
"\n"\
"   For ABR modes (high quality per given bitrate but not as high as VBR):\n"\
"\n"\
"     \"preset=<kbps>\"  Using this preset will usually give you good\n"\
"                             quality at a specified bitrate. Depending on the\n"\
"                             bitrate entered, this preset will determine the\n"\
"                             optimal settings for that particular situation.\n"\
"                             While this approach works, it is not nearly as\n"\
"                             flexible as VBR, and usually will not attain the\n"\
"                             same level of quality as VBR at higher bitrates.\n"\
"\n"\
"The following options are also available for the corresponding profiles:\n"\
"\n"\
"   <fast>        standard\n"\
"   <fast>        extreme\n"\
"                 insane\n"\
"   <cbr> (ABR Mode) - The ABR Mode is implied. To use it,\n"\
"                      simply specify a bitrate. For example:\n"\
"                      \"preset=185\" activates this\n"\
"                      preset and uses 185 as an average kbps.\n"\
"\n"\
"   \"fast\" - Enables the new fast VBR for a particular profile. The\n"\
"            disadvantage to the speed switch is that often times the\n"\
"            bitrate will be slightly higher than with the normal mode\n"\
"            and quality may be slightly lower also.\n"\
"   Warning: with the current version fast presets might result in too\n"\
"            high bitrate compared to regular presets.\n"\
"\n"\
"   \"cbr\"  - If you use the ABR mode (read above) with a significant\n"\
"            bitrate such as 80, 96, 112, 128, 160, 192, 224, 256, 320,\n"\
"            you can use the \"cbr\" option to force CBR mode encoding\n"\
"            instead of the standard abr mode. ABR does provide higher\n"\
"            quality but CBR may be useful in situations such as when\n"\
"            streaming an mp3 over the internet may be important.\n"\
"\n"\
"    For example:\n"\
"\n"\
"    \"-lameopts fast:preset=standard  \"\n"\
" or \"-lameopts  cbr:preset=192       \"\n"\
" or \"-lameopts      preset=172       \"\n"\
" or \"-lameopts      preset=extreme   \"\n"\
"\n"\
"\n"\
"A few aliases are available for ABR mode:\n"\
"phone => 16kbps/mono        phon+/lw/mw-eu/sw => 24kbps/mono\n"\
"mw-us => 40kbps/mono        voice => 56kbps/mono\n"\
"fm/radio/tape => 112kbps    hifi => 160kbps\n"\
"cd => 192kbps               studio => 256kbps"
#define MSGTR_LameCantInit "Cannot set LAME options, check bitrate/samplerate,"\
"some very low bitrates (<32) need lower samplerates (i.e. -srate 8000)."\
"If everything else fails, try a preset."
#define MSGTR_ConfigfileError "configfile error"
#define MSGTR_ErrorParsingCommandLine "error parsing cmdline"
#define MSGTR_VideoStreamRequired "Video stream is mandatory!\n"
#define MSGTR_ForcingInputFPS "input fps will be interpreted as %5.2f instead\n"
#define MSGTR_RawvideoDoesNotSupportAudio "Output file format RAWVIDEO does not support audio - disabling audio\n"
#define MSGTR_DemuxerDoesntSupportNosound "This demuxer doesn't support -nosound yet.\n"
#define MSGTR_MemAllocFailed "Memory allocation failed\n"
#define MSGTR_NoMatchingFilter "Couldn't find matching filter/ao format!\n"
#define MSGTR_MP3WaveFormatSizeNot30 "sizeof(MPEGLAYER3WAVEFORMAT)==%d!=30, maybe broken C compiler?\n"
#define MSGTR_NoLavcAudioCodecName "Audio LAVC, Missing codec name!\n"
#define MSGTR_LavcAudioCodecNotFound "Audio LAVC, couldn't find encoder for codec %s\n"
#define MSGTR_CouldntAllocateLavcContext "Audio LAVC, couldn't allocate context!\n"
#define MSGTR_CouldntOpenCodec "Couldn't open codec %s, br=%d\n"
#define MSGTR_CantCopyAudioFormat "Audio format 0x%x is incompatible with '-oac copy', please try '-oac pcm' instead or use '-fafmttag' to override it.\n"

// cfg-mencoder.h:

#define MSGTR_MEncoderMP3LameHelp "\n\n"\
" vbr=<0-4>     variable bitrate method\n"\
"                0: cbr\n"\
"                1: mt\n"\
"                2: rh(default)\n"\
"                3: abr\n"\
"                4: mtrh\n"\
"\n"\
" abr           average bitrate\n"\
"\n"\
" cbr           constant bitrate\n"\
"               Also forces CBR mode encoding on subsequent ABR presets modes.\n"\
"\n"\
" br=<0-1024>   specify bitrate in kBit (CBR and ABR only)\n"\
"\n"\
" q=<0-9>       quality (0-highest, 9-lowest) (only for VBR)\n"\
"\n"\
" aq=<0-9>      algorithmic quality (0-best/slowest, 9-worst/fastest)\n"\
"\n"\
" ratio=<1-100> compression ratio\n"\
"\n"\
" vol=<0-10>    set audio input gain\n"\
"\n"\
" mode=<0-3>    (default: auto)\n"\
"                0: stereo\n"\
"                1: joint-stereo\n"\
"                2: dualchannel\n"\
"                3: mono\n"\
"\n"\
" padding=<0-2>\n"\
"                0: no\n"\
"                1: all\n"\
"                2: adjust\n"\
"\n"\
" fast          Switch on faster encoding on subsequent VBR presets modes,\n"\
"               slightly lower quality and higher bitrates.\n"\
"\n"\
" preset=<value> Provide the highest possible quality settings.\n"\
"                 medium: VBR  encoding,  good  quality\n"\
"                 (150-180 kbps bitrate range)\n"\
"                 standard:  VBR encoding, high quality\n"\
"                 (170-210 kbps bitrate range)\n"\
"                 extreme: VBR encoding, very high quality\n"\
"                 (200-240 kbps bitrate range)\n"\
"                 insane:  CBR  encoding, highest preset quality\n"\
"                 (320 kbps bitrate)\n"\
"                 <8-320>: ABR encoding at average given kbps bitrate.\n\n"

//codec-cfg.c:
#define MSGTR_DuplicateFourcc "duplicated FourCC"
#define MSGTR_TooManyFourccs "too many FourCCs/formats..."
#define MSGTR_ParseError "parse error"
#define MSGTR_ParseErrorFIDNotNumber "parse error (format ID not a number?)"
#define MSGTR_ParseErrorFIDAliasNotNumber "parse error (format ID alias not a number?)"
#define MSGTR_DuplicateFID "duplicated format ID"
#define MSGTR_TooManyOut "too many out..."
#define MSGTR_InvalidCodecName "\ncodec(%s) name is not valid!\n"
#define MSGTR_CodecLacksFourcc "\ncodec(%s) does not have FourCC/format!\n"
#define MSGTR_CodecLacksDriver "\ncodec(%s) does not have a driver!\n"
#define MSGTR_CodecNeedsDLL "\ncodec(%s) needs a 'dll'!\n"
#define MSGTR_CodecNeedsOutfmt "\ncodec(%s) needs an 'outfmt'!\n"
#define MSGTR_CantAllocateComment "Can't allocate memory for comment. "
#define MSGTR_GetTokenMaxNotLessThanMAX_NR_TOKEN "get_token(): max >= MAX_MR_TOKEN!"
#define MSGTR_ReadingFile "Reading %s: "
#define MSGTR_CantOpenFileError "Can't open '%s': %s\n"
#define MSGTR_CantGetMemoryForLine "Can't get memory for 'line': %s\n"
#define MSGTR_CantReallocCodecsp "Can't realloc '*codecsp': %s\n"
#define MSGTR_CodecNameNotUnique "Codec name '%s' isn't unique."
#define MSGTR_CantStrdupName "Can't strdup -> 'name': %s\n"
#define MSGTR_CantStrdupInfo "Can't strdup -> 'info': %s\n"
#define MSGTR_CantStrdupDriver "Can't strdup -> 'driver': %s\n"
#define MSGTR_CantStrdupDLL "Can't strdup -> 'dll': %s"
#define MSGTR_AudioVideoCodecTotals "%d audio & %d video codecs\n"
#define MSGTR_CodecDefinitionIncorrect "Codec is not defined correctly."
#define MSGTR_OutdatedCodecsConf "This codecs.conf is too old and incompatible with this MPlayer release!"

// divx4_vbr.c:
#define MSGTR_OutOfMemory "out of memory"
#define MSGTR_OverridingTooLowBitrate "Specified bitrate is too low for this clip.\n"\
"Minimum possible bitrate for the clip is %.0f kbps. Overriding\n"\
"user-specified value.\n"

// fifo.c
#define MSGTR_CannotMakePipe "Cannot make PIPE!\n"

// m_config.c
#define MSGTR_SaveSlotTooOld "Too old save slot found from lvl %d: %d !!!\n"
#define MSGTR_InvalidCfgfileOption "The %s option can't be used in a config file.\n"
#define MSGTR_InvalidCmdlineOption "The %s option can't be used on the command line.\n"
#define MSGTR_InvalidSuboption "Error: option '%s' has no suboption '%s'.\n"
#define MSGTR_MissingSuboptionParameter "Error: suboption '%s' of '%s' must have a parameter!\n"
#define MSGTR_MissingOptionParameter "Error: option '%s' must have a parameter!\n"
#define MSGTR_OptionListHeader "\n Name                 Type            Min        Max      Global  CL    Cfg\n\n"
#define MSGTR_TotalOptions "\nTotal: %d options\n"
#define MSGTR_TooDeepProfileInclusion "WARNING: Too deep profile inclusion.\n"
#define MSGTR_NoProfileDefined "No profiles have been defined.\n"
#define MSGTR_AvailableProfiles "Available profiles:\n"
#define MSGTR_UnknownProfile "Unknown profile '%s'.\n"
#define MSGTR_Profile "Profile %s: %s\n"

// m_property.c
#define MSGTR_PropertyListHeader "\n Name                 Type            Min        Max\n\n"
#define MSGTR_TotalProperties "\nTotal: %d properties\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "CD-ROM Device '%s' not found.\n"
#define MSGTR_ErrTrackSelect "Error selecting VCD track."
#define MSGTR_ReadSTDIN "Reading from stdin...\n"
#define MSGTR_UnableOpenURL "Unable to open URL: %s\n"
#define MSGTR_ConnToServer "Connected to server: %s\n"
#define MSGTR_FileNotFound "File not found: '%s'\n"

#define MSGTR_SMBInitError "Cannot init the libsmbclient library: %d\n"
#define MSGTR_SMBFileNotFound "Could not open from LAN: '%s'\n"
#define MSGTR_SMBNotCompiled "MPlayer was not compiled with SMB reading support.\n"

#define MSGTR_CantOpenDVD "Couldn't open DVD device: %s\n"
#define MSGTR_NoDVDSupport "MPlayer was compiled without DVD support, exit\n"
#define MSGTR_DVDwait "Reading disc structure, please wait...\n"
#define MSGTR_DVDnumTitles "There are %d titles on this DVD.\n"
#define MSGTR_DVDinvalidTitle "Invalid DVD title number: %d\n"
#define MSGTR_DVDnumChapters "There are %d chapters in this DVD title.\n"
#define MSGTR_DVDinvalidChapter "Invalid DVD chapter number: %d\n"
#define MSGTR_DVDinvalidChapterRange "Invalid chapter range specification %s\n"
#define MSGTR_DVDinvalidLastChapter "Invalid DVD last chapter number: %d\n"
#define MSGTR_DVDnumAngles "There are %d angles in this DVD title.\n"
#define MSGTR_DVDinvalidAngle "Invalid DVD angle number: %d\n"
#define MSGTR_DVDnoIFO "Cannot open the IFO file for DVD title %d.\n"
#define MSGTR_DVDnoVMG "Can't open VMG info!\n"
#define MSGTR_DVDnoVOBs "Cannot open title VOBS (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDnoMatchingAudio "No matching DVD audio language found!\n"
#define MSGTR_DVDaudioChannel "Selected DVD audio channel: %d language: %c%c\n"
#define MSGTR_DVDnoMatchingSubtitle "No matching DVD subtitle language found!\n"
#define MSGTR_DVDsubtitleChannel "Selected DVD subtitle channel: %d language: %c%c\n"
#define MSGTR_DVDopenOk "DVD successfully opened.\n"

// muxer.c, muxer_*.c:
#define MSGTR_TooManyStreams "Too many streams!"
#define MSGTR_RawMuxerOnlyOneStream "Rawaudio muxer supports only one audio stream!\n"
#define MSGTR_IgnoringVideoStream "Ignoring video stream!\n"
#define MSGTR_UnknownStreamType "Warning! unknown stream type: %d\n"
#define MSGTR_WarningLenIsntDivisible "Warning! len isn't divisible by samplesize!\n"
#define MSGTR_MuxbufMallocErr "Muxer frame buffer cannot allocate memory!\n"
#define MSGTR_MuxbufReallocErr "Muxer frame buffer cannot reallocate memory!\n"
#define MSGTR_MuxbufSending "Muxer frame buffer sending %d frame(s) to muxer.\n"
#define MSGTR_WritingHeader "Writing header...\n"
#define MSGTR_WritingTrailer "Writing index...\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "WARNING: Audio stream header %d redefined.\n"
#define MSGTR_VideoStreamRedefined "WARNING: Video stream header %d redefined.\n"
#define MSGTR_TooManyAudioInBuffer "\nToo many audio packets in the buffer: (%d in %d bytes).\n"
#define MSGTR_TooManyVideoInBuffer "\nToo many video packets in the buffer: (%d in %d bytes).\n"
#define MSGTR_MaybeNI "Maybe you are playing a non-interleaved stream/file or the codec failed?\n" \
		      "For AVI files, try to force non-interleaved mode with the -ni option.\n"
#define MSGTR_SwitchToNi "\nBadly interleaved AVI file detected - switching to -ni mode...\n"
#define MSGTR_Detected_XXX_FileFormat "%s file format detected.\n"
#define MSGTR_DetectedAudiofile "Audio file detected.\n"
#define MSGTR_NotSystemStream "Not MPEG System Stream format... (maybe Transport Stream?)\n"
#define MSGTR_InvalidMPEGES "Invalid MPEG-ES stream??? Contact the author, it may be a bug :(\n"
#define MSGTR_FormatNotRecognized "============ Sorry, this file format is not recognized/supported =============\n"\
				  "=== If this file is an AVI, ASF or MPEG stream, please contact the author! ===\n"
#define MSGTR_MissingVideoStream "No video stream found.\n"
#define MSGTR_MissingAudioStream "No audio stream found -> no sound.\n"
#define MSGTR_MissingVideoStreamBug "Missing video stream!? Contact the author, it may be a bug :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: File doesn't contain the selected audio or video stream.\n"

#define MSGTR_NI_Forced "Forced"
#define MSGTR_NI_Detected "Detected"
#define MSGTR_NI_Message "%s NON-INTERLEAVED AVI file format.\n"

#define MSGTR_UsingNINI "Using NON-INTERLEAVED broken AVI file format.\n"
#define MSGTR_CouldntDetFNo "Could not determine number of frames (for absolute seek).\n"
#define MSGTR_CantSeekRawAVI "Cannot seek in raw AVI streams. (Index required, try with the -idx switch.)\n"
#define MSGTR_CantSeekFile "Cannot seek in this file.\n"

#define MSGTR_EncryptedVOB "Encrypted VOB file! Read DOCS/HTML/en/cd-dvd.html.\n"

#define MSGTR_MOVcomprhdr "MOV: Compressed headers support requires ZLIB!\n"
#define MSGTR_MOVvariableFourCC "MOV: WARNING: Variable FOURCC detected!?\n"
#define MSGTR_MOVtooManyTrk "MOV: WARNING: too many tracks"
#define MSGTR_FoundAudioStream "==> Found audio stream: %d\n"
#define MSGTR_FoundVideoStream "==> Found video stream: %d\n"
#define MSGTR_DetectedTV "TV detected! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "Unable to open the ogg demuxer.\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: Searching for audio stream (id:%d).\n"
#define MSGTR_CannotOpenAudioStream "Cannot open audio stream: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "Cannot open subtitle stream: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "Failed to open audio demuxer: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "Failed to open subtitle demuxer: %s\n"
#define MSGTR_TVInputNotSeekable "TV input is not seekable! (Seeking will probably be for changing channels ;)\n"
#define MSGTR_DemuxerInfoAlreadyPresent "Demuxer info %s already present!\n"
#define MSGTR_ClipInfo "Clip info:\n"

#define MSGTR_LeaveTelecineMode "\ndemux_mpg: 30000/1001fps NTSC content detected, switching framerate.\n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: 24000/1001fps progressive NTSC content detected, switching framerate.\n"

#define MSGTR_CacheFill "\rCache fill: %5.2f%% (%"PRId64" bytes)   "
#define MSGTR_NoBindFound "No bind found for key '%s'"
#define MSGTR_FailedToOpen "Failed to open %s\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "Could not open codec.\n"
#define MSGTR_CantCloseCodec "Could not close codec.\n"

#define MSGTR_MissingDLLcodec "ERROR: Could not open required DirectShow codec %s.\n"
#define MSGTR_ACMiniterror "Could not load/initialize Win32/ACM AUDIO codec (missing DLL file?).\n"
#define MSGTR_MissingLAVCcodec "Cannot find codec '%s' in libavcodec...\n"

#define MSGTR_MpegNoSequHdr "MPEG: FATAL: EOF while searching for sequence header.\n"
#define MSGTR_CannotReadMpegSequHdr "FATAL: Cannot read sequence header.\n"
#define MSGTR_CannotReadMpegSequHdrEx "FATAL: Cannot read sequence header extension.\n"
#define MSGTR_BadMpegSequHdr "MPEG: bad sequence header\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: bad sequence header extension\n"

#define MSGTR_ShMemAllocFail "Cannot allocate shared memory.\n"
#define MSGTR_CantAllocAudioBuf "Cannot allocate audio out buffer.\n"

#define MSGTR_UnknownAudio "Unknown/missing audio format -> no sound\n"

#define MSGTR_UsingExternalPP "[PP] Using external postprocessing filter, max q = %d.\n"
#define MSGTR_UsingCodecPP "[PP] Using codec's postprocessing, max q = %d.\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "Video attribute '%s' is not supported by selected vo & vd.\n"
#define MSGTR_VideoCodecFamilyNotAvailableStr "Requested video codec family [%s] (vfm=%s) not available.\nEnable it at compilation.\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "Requested audio codec family [%s] (afm=%s) not available.\nEnable it at compilation.\n"
#define MSGTR_OpeningVideoDecoder "Opening video decoder: [%s] %s\n"
#define MSGTR_SelectedVideoCodec "Selected video codec: [%s] vfm: %s (%s)\n"
#define MSGTR_OpeningAudioDecoder "Opening audio decoder: [%s] %s\n"
#define MSGTR_SelectedAudioCodec "Selected audio codec: [%s] afm: %s (%s)\n"
#define MSGTR_BuildingAudioFilterChain "Building audio filter chain for %dHz/%dch/%s -> %dHz/%dch/%s...\n"
#define MSGTR_UninitVideoStr "uninit video: %s\n"
#define MSGTR_UninitAudioStr "uninit audio: %s\n"
#define MSGTR_VDecoderInitFailed "VDecoder init failed :(\n"
#define MSGTR_ADecoderInitFailed "ADecoder init failed :(\n"
#define MSGTR_ADecoderPreinitFailed "ADecoder preinit failed :(\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: Allocating %d bytes for input buffer.\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: Allocating %d + %d = %d bytes for output buffer.\n"

// LIRC:
#define MSGTR_SettingUpLIRC "Setting up LIRC support...\n"
#define MSGTR_LIRCdisabled "You will not be able to use your remote control.\n"
#define MSGTR_LIRCopenfailed "Failed to open LIRC support.\n"
#define MSGTR_LIRCcfgerr "Failed to read LIRC config file %s.\n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "Couldn't find video filter '%s'.\n"
#define MSGTR_CouldNotOpenVideoFilter "Couldn't open video filter '%s'.\n"
#define MSGTR_OpeningVideoFilter "Opening video filter: "
#define MSGTR_CannotFindColorspace "Cannot find matching colorspace, even by inserting 'scale' :(\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDec: Codec did not set sh->disp_w and sh->disp_h, trying workaround.\n"
#define MSGTR_VoConfigRequest "VDec: vo config request - %d x %d (preferred colorspace: %s)\n"
#define MSGTR_CouldNotFindColorspace "Could not find matching colorspace - retrying with -vf scale...\n"
#define MSGTR_MovieAspectIsSet "Movie-Aspect is %.2f:1 - prescaling to correct movie aspect.\n"
#define MSGTR_MovieAspectUndefined "Movie-Aspect is undefined - no prescaling applied.\n"

// vd_dshow.c, vd_dmo.c
#define MSGTR_DownloadCodecPackage "You need to upgrade/install the binary codecs package.\nGo to http://www.mplayerhq.hu/dload.html\n"
#define MSGTR_DShowInitOK "INFO: Win32/DShow video codec init OK.\n"
#define MSGTR_DMOInitOK "INFO: Win32/DMO video codec init OK.\n"

// x11_common.c
#define MSGTR_EwmhFullscreenStateFailed "\nX11: Couldn't send EWMH fullscreen Event!\n"
#define MSGTR_CouldNotFindXScreenSaver "xscreensaver_disable: Could not find XScreenSaver window.\n"
#define MSGTR_SelectedVideoMode "XF86VM: Selected video mode %dx%d for image size %dx%d.\n"

#define MSGTR_InsertingAfVolume "[Mixer] No hardware mixing, inserting volume filter.\n"
#define MSGTR_NoVolume "[Mixer] No volume control available.\n"

// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "About"
#define MSGTR_FileSelect "Select file..."
#define MSGTR_SubtitleSelect "Select subtitle..."
#define MSGTR_OtherSelect "Select..."
#define MSGTR_AudioFileSelect "Select external audio channel..."
#define MSGTR_FontSelect "Select font..."
// Note: If you change MSGTR_PlayList please see if it still fits MSGTR_MENU_PlayList
#define MSGTR_PlayList "Playlist"
#define MSGTR_Equalizer "Equalizer"
#define MSGTR_ConfigureEqualizer "Configure Equalizer"
#define MSGTR_SkinBrowser "Skin Browser"
#define MSGTR_Network "Network streaming..."
// Note: If you change MSGTR_Preferences please see if it still fits MSGTR_MENU_Preferences
#define MSGTR_Preferences "Preferences"
#define MSGTR_AudioPreferences "Audio driver configuration"
#define MSGTR_NoMediaOpened "No media opened."
#define MSGTR_VCDTrack "VCD track %d"
#define MSGTR_NoChapter "No chapter"
#define MSGTR_Chapter "Chapter %d"
#define MSGTR_NoFileLoaded "No file loaded."

// --- buttons ---
#define MSGTR_Ok "OK"
#define MSGTR_Cancel "Cancel"
#define MSGTR_Add "Add"
#define MSGTR_Remove "Remove"
#define MSGTR_Clear "Clear"
#define MSGTR_Config "Config"
#define MSGTR_ConfigDriver "Configure driver"
#define MSGTR_Browse "Browse"

// --- error messages ---
#define MSGTR_NEMDB "Sorry, not enough memory for draw buffer."
#define MSGTR_NEMFMR "Sorry, not enough memory for menu rendering."
#define MSGTR_IDFGCVD "Sorry, I did not find a GUI compatible video output driver."
#define MSGTR_NEEDLAVCFAME "Sorry, you cannot play non-MPEG files with your DXR3/H+ device without reencoding.\nPlease enable lavc or fame in the DXR3/H+ configbox."
#define MSGTR_UNKNOWNWINDOWTYPE "Unknown window type found ..."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[skin] error in skin config file on line %d: %s"
#define MSGTR_SKIN_WARNING1 "[skin] warning in config file line %d:\nwidget (%s) found but no \"section\" found before"
#define MSGTR_SKIN_WARNING2 "[skin] warning in config file line %d:\nwidget (%s) found but no \"subsection\" found before"
#define MSGTR_SKIN_WARNING3 "[skin] warning in config file line %d:\nthis subsection is not supported by widget (%s)"
#define MSGTR_SKIN_SkinFileNotFound "[skin] file ( %s ) not found.\n"
#define MSGTR_SKIN_SkinFileNotReadable "[skin] file ( %s ) not readable.\n"
#define MSGTR_SKIN_BITMAP_16bit  "16 bits or less depth bitmap not supported (%s).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "file not found (%s)\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "BMP read error (%s)\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "TGA read error (%s)\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "PNG read error (%s)\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "RLE packed TGA not supported (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "unknown file type (%s)\n"
#define MSGTR_SKIN_BITMAP_ConvertError "24 bit to 32 bit convert error (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "unknown message: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "not enough memory\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "Too many fonts declared.\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "Font file not found.\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "Font image file not found.\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "non-existent font identifier (%s)\n"
#define MSGTR_SKIN_UnknownParameter "unknown parameter (%s)\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "Skin not found (%s).\n"
#define MSGTR_SKIN_SKINCFG_SelectedSkinNotFound "Selected skin ( %s ) not found, trying 'default'...\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "skin configfile read error (%s)\n"
#define MSGTR_SKIN_LABEL "Skins:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "About MPlayer"
#define MSGTR_MENU_Open "Open..."
#define MSGTR_MENU_PlayFile "Play file..."
#define MSGTR_MENU_PlayVCD "Play VCD..."
#define MSGTR_MENU_PlayDVD "Play DVD..."
#define MSGTR_MENU_PlayURL "Play URL..."
#define MSGTR_MENU_LoadSubtitle "Load subtitle..."
#define MSGTR_MENU_DropSubtitle "Drop subtitle..."
#define MSGTR_MENU_LoadExternAudioFile "Load external audio file..."
#define MSGTR_MENU_Playing "Playing"
#define MSGTR_MENU_Play "Play"
#define MSGTR_MENU_Pause "Pause"
#define MSGTR_MENU_Stop "Stop"
#define MSGTR_MENU_NextStream "Next stream"
#define MSGTR_MENU_PrevStream "Prev stream"
#define MSGTR_MENU_Size "Size"
#define MSGTR_MENU_HalfSize   "Half size"
#define MSGTR_MENU_NormalSize "Normal size"
#define MSGTR_MENU_DoubleSize "Double size"
#define MSGTR_MENU_FullScreen "Fullscreen"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "Open disc..."
#define MSGTR_MENU_ShowDVDMenu "Show DVD menu"
#define MSGTR_MENU_Titles "Titles"
#define MSGTR_MENU_Title "Title %2d"
#define MSGTR_MENU_None "(none)"
#define MSGTR_MENU_Chapters "Chapters"
#define MSGTR_MENU_Chapter "Chapter %2d"
#define MSGTR_MENU_AudioLanguages "Audio languages"
#define MSGTR_MENU_SubtitleLanguages "Subtitle languages"
#define MSGTR_MENU_PlayList MSGTR_PlayList
#define MSGTR_MENU_SkinBrowser "Skin browser"
#define MSGTR_MENU_Preferences MSGTR_Preferences
#define MSGTR_MENU_Exit "Exit..."
#define MSGTR_MENU_Mute "Mute"
#define MSGTR_MENU_Original "Original"
#define MSGTR_MENU_AspectRatio "Aspect ratio"
#define MSGTR_MENU_AudioTrack "Audio track"
#define MSGTR_MENU_Track "Track %d"
#define MSGTR_MENU_VideoTrack "Video track"
#define MSGTR_MENU_Subtitles "Subtitles"

// --- equalizer
// Note: If you change MSGTR_EQU_Audio please see if it still fits MSGTR_PREFERENCES_Audio
#define MSGTR_EQU_Audio "Audio"
// Note: If you change MSGTR_EQU_Video please see if it still fits MSGTR_PREFERENCES_Video
#define MSGTR_EQU_Video "Video"
#define MSGTR_EQU_Contrast "Contrast: "
#define MSGTR_EQU_Brightness "Brightness: "
#define MSGTR_EQU_Hue "Hue: "
#define MSGTR_EQU_Saturation "Saturation: "
#define MSGTR_EQU_Front_Left "Front Left"
#define MSGTR_EQU_Front_Right "Front Right"
#define MSGTR_EQU_Back_Left "Rear Left"
#define MSGTR_EQU_Back_Right "Rear Right"
#define MSGTR_EQU_Center "Center"
#define MSGTR_EQU_Bass "Bass"
#define MSGTR_EQU_All "All"
#define MSGTR_EQU_Channel1 "Channel 1:"
#define MSGTR_EQU_Channel2 "Channel 2:"
#define MSGTR_EQU_Channel3 "Channel 3:"
#define MSGTR_EQU_Channel4 "Channel 4:"
#define MSGTR_EQU_Channel5 "Channel 5:"
#define MSGTR_EQU_Channel6 "Channel 6:"

// --- playlist
#define MSGTR_PLAYLIST_Path "Path"
#define MSGTR_PLAYLIST_Selected "Selected files"
#define MSGTR_PLAYLIST_Files "Files"
#define MSGTR_PLAYLIST_DirectoryTree "Directory tree"

// --- preferences
#define MSGTR_PREFERENCES_Audio MSGTR_EQU_Audio
#define MSGTR_PREFERENCES_Video MSGTR_EQU_Video
#define MSGTR_PREFERENCES_SubtitleOSD "Subtitle & OSD"
#define MSGTR_PREFERENCES_Codecs "Codecs & demuxer"
// Note: If you change MSGTR_PREFERENCES_Misc see if it still fits MSGTR_PREFERENCES_FRAME_Misc
#define MSGTR_PREFERENCES_Misc "Misc"

#define MSGTR_PREFERENCES_None "None"
#define MSGTR_PREFERENCES_DriverDefault "driver default"
#define MSGTR_PREFERENCES_AvailableDrivers "Available drivers:"
#define MSGTR_PREFERENCES_DoNotPlaySound "Do not play sound"
#define MSGTR_PREFERENCES_NormalizeSound "Normalize sound"
#define MSGTR_PREFERENCES_EnEqualizer "Enable equalizer"
#define MSGTR_PREFERENCES_SoftwareMixer "Enable Software Mixer"
#define MSGTR_PREFERENCES_ExtraStereo "Enable extra stereo"
#define MSGTR_PREFERENCES_Coefficient "Coefficient:"
#define MSGTR_PREFERENCES_AudioDelay "Audio delay"
#define MSGTR_PREFERENCES_DoubleBuffer "Enable double buffering"
#define MSGTR_PREFERENCES_DirectRender "Enable direct rendering"
#define MSGTR_PREFERENCES_FrameDrop "Enable frame dropping"
#define MSGTR_PREFERENCES_HFrameDrop "Enable HARD frame dropping (dangerous)"
#define MSGTR_PREFERENCES_Flip "Flip image upside down"
#define MSGTR_PREFERENCES_Panscan "Panscan: "
#define MSGTR_PREFERENCES_OSDTimer "Timer and indicators"
#define MSGTR_PREFERENCES_OSDProgress "Progressbars only"
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "Timer, percentage and total time"
#define MSGTR_PREFERENCES_Subtitle "Subtitle:"
#define MSGTR_PREFERENCES_SUB_Delay "Delay: "
#define MSGTR_PREFERENCES_SUB_FPS "FPS:"
#define MSGTR_PREFERENCES_SUB_POS "Position: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "Disable subtitle autoloading"
#define MSGTR_PREFERENCES_SUB_Unicode "Unicode subtitle"
#define MSGTR_PREFERENCES_SUB_MPSUB "Convert the given subtitle to MPlayer's subtitle format"
#define MSGTR_PREFERENCES_SUB_SRT "Convert the given subtitle to the time based SubViewer (SRT) format"
#define MSGTR_PREFERENCES_SUB_Overlap "Toggle subtitle overlapping"
#define MSGTR_PREFERENCES_Font "Font:"
#define MSGTR_PREFERENCES_FontFactor "Font factor:"
#define MSGTR_PREFERENCES_PostProcess "Enable postprocessing"
#define MSGTR_PREFERENCES_AutoQuality "Auto quality: "
#define MSGTR_PREFERENCES_NI "Use non-interleaved AVI parser"
#define MSGTR_PREFERENCES_IDX "Rebuild index table, if needed"
#define MSGTR_PREFERENCES_VideoCodecFamily "Video codec family:"
#define MSGTR_PREFERENCES_AudioCodecFamily "Audio codec family:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "OSD level"
#define MSGTR_PREFERENCES_FRAME_Subtitle "Subtitle"
#define MSGTR_PREFERENCES_FRAME_Font "Font"
#define MSGTR_PREFERENCES_FRAME_PostProcess "Postprocessing"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "Codec & demuxer"
#define MSGTR_PREFERENCES_FRAME_Cache "Cache"
#define MSGTR_PREFERENCES_FRAME_Misc MSGTR_PREFERENCES_Misc
#define MSGTR_PREFERENCES_Audio_Device "Device:"
#define MSGTR_PREFERENCES_Audio_Mixer "Mixer:"
#define MSGTR_PREFERENCES_Audio_MixerChannel "Mixer channel:"
#define MSGTR_PREFERENCES_Message "Please remember that you need to restart playback for some options to take effect!"
#define MSGTR_PREFERENCES_DXR3_VENC "Video encoder:"
#define MSGTR_PREFERENCES_DXR3_LAVC "Use LAVC (FFmpeg)"
#define MSGTR_PREFERENCES_DXR3_FAME "Use FAME"
#define MSGTR_PREFERENCES_FontEncoding1 "Unicode"
#define MSGTR_PREFERENCES_FontEncoding2 "Western European Languages (ISO-8859-1)"
#define MSGTR_PREFERENCES_FontEncoding3 "Western European Languages with Euro (ISO-8859-15)"
#define MSGTR_PREFERENCES_FontEncoding4 "Slavic/Central European Languages (ISO-8859-2)"
#define MSGTR_PREFERENCES_FontEncoding5 "Esperanto, Galician, Maltese, Turkish (ISO-8859-3)"
#define MSGTR_PREFERENCES_FontEncoding6 "Old Baltic charset (ISO-8859-4)"
#define MSGTR_PREFERENCES_FontEncoding7 "Cyrillic (ISO-8859-5)"
#define MSGTR_PREFERENCES_FontEncoding8 "Arabic (ISO-8859-6)"
#define MSGTR_PREFERENCES_FontEncoding9 "Modern Greek (ISO-8859-7)"
#define MSGTR_PREFERENCES_FontEncoding10 "Turkish (ISO-8859-9)"
#define MSGTR_PREFERENCES_FontEncoding11 "Baltic (ISO-8859-13)"
#define MSGTR_PREFERENCES_FontEncoding12 "Celtic (ISO-8859-14)"
#define MSGTR_PREFERENCES_FontEncoding13 "Hebrew charsets (ISO-8859-8)"
#define MSGTR_PREFERENCES_FontEncoding14 "Russian (KOI8-R)"
#define MSGTR_PREFERENCES_FontEncoding15 "Ukrainian, Belarusian (KOI8-U/RU)"
#define MSGTR_PREFERENCES_FontEncoding16 "Simplified Chinese charset (CP936)"
#define MSGTR_PREFERENCES_FontEncoding17 "Traditional Chinese charset (BIG5)"
#define MSGTR_PREFERENCES_FontEncoding18 "Japanese charsets (SHIFT-JIS)"
#define MSGTR_PREFERENCES_FontEncoding19 "Korean charset (CP949)"
#define MSGTR_PREFERENCES_FontEncoding20 "Thai charset (CP874)"
#define MSGTR_PREFERENCES_FontEncoding21 "Cyrillic Windows (CP1251)"
#define MSGTR_PREFERENCES_FontEncoding22 "Slavic/Central European Windows (CP1250)"
#define MSGTR_PREFERENCES_FontNoAutoScale "No autoscale"
#define MSGTR_PREFERENCES_FontPropWidth "Proportional to movie width"
#define MSGTR_PREFERENCES_FontPropHeight "Proportional to movie height"
#define MSGTR_PREFERENCES_FontPropDiagonal "Proportional to movie diagonal"
#define MSGTR_PREFERENCES_FontEncoding "Encoding:"
#define MSGTR_PREFERENCES_FontBlur "Blur:"
#define MSGTR_PREFERENCES_FontOutLine "Outline:"
#define MSGTR_PREFERENCES_FontTextScale "Text scale:"
#define MSGTR_PREFERENCES_FontOSDScale "OSD scale:"
#define MSGTR_PREFERENCES_Cache "Cache on/off"
#define MSGTR_PREFERENCES_CacheSize "Cache size: "
#define MSGTR_PREFERENCES_LoadFullscreen "Start in fullscreen"
#define MSGTR_PREFERENCES_SaveWinPos "Save window position"
#define MSGTR_PREFERENCES_XSCREENSAVER "Stop XScreenSaver"
#define MSGTR_PREFERENCES_PlayBar "Enable playbar"
#define MSGTR_PREFERENCES_AutoSync "AutoSync on/off"
#define MSGTR_PREFERENCES_AutoSyncValue "Autosync: "
#define MSGTR_PREFERENCES_CDROMDevice "CD-ROM device:"
#define MSGTR_PREFERENCES_DVDDevice "DVD device:"
#define MSGTR_PREFERENCES_FPS "Movie FPS:"
#define MSGTR_PREFERENCES_ShowVideoWindow "Show video window when inactive"
#define MSGTR_PREFERENCES_ArtsBroken "Newer aRts versions are incompatible "\
           "with GTK 1.x and will crash GMPlayer!"

#define MSGTR_ABOUT_UHU "GUI development sponsored by UHU Linux\n"
#define MSGTR_ABOUT_Contributors "Code and documentation contributors\n"
#define MSGTR_ABOUT_Codecs_libs_contributions "Codecs and third party libraries\n"
#define MSGTR_ABOUT_Translations "Translations\n"
#define MSGTR_ABOUT_Skins "Skins\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "Fatal error!"
#define MSGTR_MSGBOX_LABEL_Error "Error!"
#define MSGTR_MSGBOX_LABEL_Warning "Warning!"

// bitmap.c

#define MSGTR_NotEnoughMemoryC32To1 "[c32to1] not enough memory for image\n"
#define MSGTR_NotEnoughMemoryC1To32 "[c1to32] not enough memory for image\n"

// cfg.c

#define MSGTR_ConfigFileReadError "[cfg] config file read error ...\n"
#define MSGTR_UnableToSaveOption "[cfg] Unable to save the '%s' option.\n"

// interface.c

#define MSGTR_DeletingSubtitles "[GUI] Deleting subtitles.\n"
#define MSGTR_LoadingSubtitles "[GUI] Loading subtitles: %s\n"
#define MSGTR_AddingVideoFilter "[GUI] Adding video filter: %s\n"
#define MSGTR_RemovingVideoFilter "[GUI] Removing video filter: %s\n"

// mw.c

#define MSGTR_NotAFile "This does not seem to be a file: %s !\n"

// ws.c

#define MSGTR_WS_CouldNotOpenDisplay "[ws] Could not open the display.\n"
#define MSGTR_WS_RemoteDisplay "[ws] Remote display, disabling XMITSHM.\n"
#define MSGTR_WS_NoXshm "[ws] Sorry, your system does not support the X shared memory extension.\n"
#define MSGTR_WS_NoXshape "[ws] Sorry, your system does not support the XShape extension.\n"
#define MSGTR_WS_ColorDepthTooLow "[ws] Sorry, the color depth is too low.\n"
#define MSGTR_WS_TooManyOpenWindows "[ws] There are too many open windows.\n"
#define MSGTR_WS_ShmError "[ws] shared memory extension error\n"
#define MSGTR_WS_NotEnoughMemoryDrawBuffer "[ws] Sorry, not enough memory for draw buffer.\n"
#define MSGTR_WS_DpmsUnavailable "DPMS not available?\n"
#define MSGTR_WS_DpmsNotEnabled "Could not enable DPMS.\n"

// wsxdnd.c

#define MSGTR_WS_NotAFile "This does not seem to be a file...\n"
#define MSGTR_WS_DDNothing "D&D: Nothing returned!\n"

#endif

// ======================= VO Video Output drivers ========================

#define MSGTR_VOincompCodec "The selected video_out device is incompatible with this codec.\n"\
                "Try adding the scale filter, e.g. -vf spp,scale instead of -vf spp.\n"
#define MSGTR_VO_GenericError "This error has occurred"
#define MSGTR_VO_UnableToAccess "Unable to access"
#define MSGTR_VO_ExistsButNoDirectory "already exists, but is not a directory."
#define MSGTR_VO_DirExistsButNotWritable "Output directory already exists, but is not writable."
#define MSGTR_VO_DirExistsAndIsWritable "Output directory already exists and is writable."
#define MSGTR_VO_CantCreateDirectory "Unable to create output directory."
#define MSGTR_VO_CantCreateFile "Unable to create output file."
#define MSGTR_VO_DirectoryCreateSuccess "Output directory successfully created."
#define MSGTR_VO_ParsingSuboptions "Parsing suboptions."
#define MSGTR_VO_SuboptionsParsedOK "Suboptions parsed OK."
#define MSGTR_VO_ValueOutOfRange "Value out of range"
#define MSGTR_VO_NoValueSpecified "No value specified."
#define MSGTR_VO_UnknownSuboptions "Unknown suboption(s)"

// vo_aa.c

#define MSGTR_VO_AA_HelpHeader "\n\nHere are the aalib vo_aa suboptions:\n"
#define MSGTR_VO_AA_AdditionalOptions "Additional options vo_aa provides:\n" \
"  help        print this help message\n" \
"  osdcolor    set osd color\n  subcolor    set subtitle color\n" \
"        the color parameters are:\n           0 : normal\n" \
"           1 : dim\n           2 : bold\n           3 : boldfont\n" \
"           4 : reverse\n           5 : special\n\n\n"

// vo_jpeg.c
#define MSGTR_VO_JPEG_ProgressiveJPEG "Progressive JPEG enabled."
#define MSGTR_VO_JPEG_NoProgressiveJPEG "Progressive JPEG disabled."
#define MSGTR_VO_JPEG_BaselineJPEG "Baseline JPEG enabled."
#define MSGTR_VO_JPEG_NoBaselineJPEG "Baseline JPEG disabled."

// vo_pnm.c
#define MSGTR_VO_PNM_ASCIIMode "ASCII mode enabled."
#define MSGTR_VO_PNM_RawMode "Raw mode enabled."
#define MSGTR_VO_PNM_PPMType "Will write PPM files."
#define MSGTR_VO_PNM_PGMType "Will write PGM files."
#define MSGTR_VO_PNM_PGMYUVType "Will write PGMYUV files."

// vo_yuv4mpeg.c
#define MSGTR_VO_YUV4MPEG_InterlacedHeightDivisibleBy4 "Interlaced mode requires image height to be divisible by 4."
#define MSGTR_VO_YUV4MPEG_InterlacedLineBufAllocFail "Unable to allocate line buffer for interlaced mode."
#define MSGTR_VO_YUV4MPEG_InterlacedInputNotRGB "Input not RGB, can't separate chrominance by fields!"
#define MSGTR_VO_YUV4MPEG_WidthDivisibleBy2 "Image width must be divisible by 2."
#define MSGTR_VO_YUV4MPEG_NoMemRGBFrameBuf "Not enough memory to allocate RGB framebuffer."
#define MSGTR_VO_YUV4MPEG_OutFileOpenError "Can't get memory or file handle to write \"%s\"!"
#define MSGTR_VO_YUV4MPEG_OutFileWriteError "Error writing image to output!"
#define MSGTR_VO_YUV4MPEG_UnknownSubDev "Unknown subdevice: %s"
#define MSGTR_VO_YUV4MPEG_InterlacedTFFMode "Using interlaced output mode, top-field first."
#define MSGTR_VO_YUV4MPEG_InterlacedBFFMode "Using interlaced output mode, bottom-field first."
#define MSGTR_VO_YUV4MPEG_ProgressiveMode "Using (default) progressive frame mode."

// sub.c
#define MSGTR_VO_SUB_Seekbar "Seekbar"
#define MSGTR_VO_SUB_Play "Play"
#define MSGTR_VO_SUB_Pause "Pause"
#define MSGTR_VO_SUB_Stop "Stop"
#define MSGTR_VO_SUB_Rewind "Rewind"
#define MSGTR_VO_SUB_Forward "Forward"
#define MSGTR_VO_SUB_Clock "Clock"
#define MSGTR_VO_SUB_Contrast "Contrast"
#define MSGTR_VO_SUB_Saturation "Saturation"
#define MSGTR_VO_SUB_Volume "Volume"
#define MSGTR_VO_SUB_Brightness "Brightness"
#define MSGTR_VO_SUB_Hue "Hue"

// vo_xv.c
#define MSGTR_VO_XV_ImagedimTooHigh "Source image dimensions are too high: %ux%u (maximum is %ux%u)\n"

// Old vo drivers that have been replaced

#define MSGTR_VO_PGM_HasBeenReplaced "The pgm video output driver has been replaced by -vo pnm:pgmyuv.\n"
#define MSGTR_VO_MD5_HasBeenReplaced "The md5 video output driver has been replaced by -vo md5sum.\n"

// ======================= AO Audio Output drivers ========================

// libao2 

// audio_out.c
#define MSGTR_AO_ALSA9_1x_Removed "audio_out: alsa9 and alsa1x modules were removed, use -ao alsa instead.\n"

// ao_oss.c
#define MSGTR_AO_OSS_CantOpenMixer "[AO OSS] audio_setup: Can't open mixer device %s: %s\n"
#define MSGTR_AO_OSS_ChanNotFound "[AO OSS] audio_setup: Audio card mixer does not have channel '%s' using default.\n"
#define MSGTR_AO_OSS_CantOpenDev "[AO OSS] audio_setup: Can't open audio device %s: %s\n"
#define MSGTR_AO_OSS_CantMakeFd "[AO OSS] audio_setup: Can't make file descriptor blocking: %s\n"
#define MSGTR_AO_OSS_CantSet "[AO OSS] Can't set audio device %s to %s output, trying %s...\n"
#define MSGTR_AO_OSS_CantSetChans "[AO OSS] audio_setup: Failed to set audio device to %d channels.\n"
#define MSGTR_AO_OSS_CantUseGetospace "[AO OSS] audio_setup: driver doesn't support SNDCTL_DSP_GETOSPACE :-(\n"
#define MSGTR_AO_OSS_CantUseSelect "[AO OSS]\n   ***  Your audio driver DOES NOT support select()  ***\n Recompile MPlayer with #undef HAVE_AUDIO_SELECT in config.h !\n\n"
#define MSGTR_AO_OSS_CantReopen "[AO OSS]\nFatal error: *** CANNOT RE-OPEN / RESET AUDIO DEVICE *** %s\n"
#define MSGTR_AO_OSS_UnknownUnsupportedFormat "[AO OSS] Unknown/Unsupported OSS format: %x.\n"

// ao_arts.c
#define MSGTR_AO_ARTS_CantInit "[AO ARTS] %s\n"
#define MSGTR_AO_ARTS_ServerConnect "[AO ARTS] Connected to sound server.\n"
#define MSGTR_AO_ARTS_CantOpenStream "[AO ARTS] Unable to open a stream.\n"
#define MSGTR_AO_ARTS_StreamOpen "[AO ARTS] Stream opened.\n"
#define MSGTR_AO_ARTS_BufferSize "[AO ARTS] buffer size: %d\n"

// ao_dxr2.c
#define MSGTR_AO_DXR2_SetVolFailed "[AO DXR2] Setting volume to %d failed.\n"
#define MSGTR_AO_DXR2_UnsupSamplerate "[AO DXR2] %d Hz not supported, try to resample.\n"

// ao_esd.c
#define MSGTR_AO_ESD_CantOpenSound "[AO ESD] esd_open_sound failed: %s\n"
#define MSGTR_AO_ESD_LatencyInfo "[AO ESD] latency: [server: %0.2fs, net: %0.2fs] (adjust %0.2fs)\n"
#define MSGTR_AO_ESD_CantOpenPBStream "[AO ESD] failed to open ESD playback stream: %s\n"

// ao_mpegpes.c
#define MSGTR_AO_MPEGPES_CantSetMixer "[AO MPEGPES] DVB audio set mixer failed: %s.\n"
#define MSGTR_AO_MPEGPES_UnsupSamplerate "[AO MPEGPES] %d Hz not supported, try to resample.\n"

// ao_null.c
// This one desn't even  have any mp_msg nor printf's?? [CHECK]

// ao_pcm.c
#define MSGTR_AO_PCM_FileInfo "[AO PCM] File: %s (%s)\nPCM: Samplerate: %iHz Channels: %s Format %s\n"
#define MSGTR_AO_PCM_HintInfo "[AO PCM] Info: Faster dumping is achieved with -vc null -vo null -ao pcm:fast\n[AO PCM] Info: To write WAVE files use -ao pcm:waveheader (default).\n"
#define MSGTR_AO_PCM_CantOpenOutputFile "[AO PCM] Failed to open %s for writing!\n"

// ao_sdl.c
#define MSGTR_AO_SDL_INFO "[AO SDL] Samplerate: %iHz Channels: %s Format %s\n"
#define MSGTR_AO_SDL_DriverInfo "[AO SDL] using %s audio driver.\n"
#define MSGTR_AO_SDL_UnsupportedAudioFmt "[AO SDL] Unsupported audio format: 0x%x.\n"
#define MSGTR_AO_SDL_CantInit "[AO SDL] Initializing of SDL Audio failed: %s\n"
#define MSGTR_AO_SDL_CantOpenAudio "[AO SDL] Unable to open audio: %s\n"

// ao_sgi.c
#define MSGTR_AO_SGI_INFO "[AO SGI] control.\n"
#define MSGTR_AO_SGI_InitInfo "[AO SGI] init: Samplerate: %iHz Channels: %s Format %s\n"
#define MSGTR_AO_SGI_InvalidDevice "[AO SGI] play: invalid device.\n"
#define MSGTR_AO_SGI_CantSetParms_Samplerate "[AO SGI] init: setparams failed: %s\nCould not set desired samplerate.\n"
#define MSGTR_AO_SGI_CantSetAlRate "[AO SGI] init: AL_RATE was not accepted on the given resource.\n"
#define MSGTR_AO_SGI_CantGetParms "[AO SGI] init: getparams failed: %s\n"
#define MSGTR_AO_SGI_SampleRateInfo "[AO SGI] init: samplerate is now %lf (desired rate is %lf)\n"
#define MSGTR_AO_SGI_InitConfigError "[AO SGI] init: %s\n"
#define MSGTR_AO_SGI_InitOpenAudioFailed "[AO SGI] init: Unable to open audio channel: %s\n"
#define MSGTR_AO_SGI_Uninit "[AO SGI] uninit: ...\n"
#define MSGTR_AO_SGI_Reset "[AO SGI] reset: ...\n"
#define MSGTR_AO_SGI_PauseInfo "[AO SGI] audio_pause: ...\n"
#define MSGTR_AO_SGI_ResumeInfo "[AO SGI] audio_resume: ...\n"

// ao_sun.c
#define MSGTR_AO_SUN_RtscSetinfoFailed "[AO SUN] rtsc: SETINFO failed.\n"
#define MSGTR_AO_SUN_RtscWriteFailed "[AO SUN] rtsc: write failed.\n"
#define MSGTR_AO_SUN_CantOpenAudioDev "[AO SUN] Can't open audio device %s, %s  -> nosound.\n"
#define MSGTR_AO_SUN_UnsupSampleRate "[AO SUN] audio_setup: your card doesn't support %d channel, %s, %d Hz samplerate.\n"
#define MSGTR_AO_SUN_CantUseSelect "[AO SUN]\n   ***  Your audio driver DOES NOT support select()  ***\nRecompile MPlayer with #undef HAVE_AUDIO_SELECT in config.h !\n\n"
#define MSGTR_AO_SUN_CantReopenReset "[AO SUN]\nFatal error: *** CANNOT RE-OPEN / RESET AUDIO DEVICE (%s) ***\n"

// ao_alsa5.c
#define MSGTR_AO_ALSA5_InitInfo "[AO ALSA5] alsa-init: requested format: %d Hz, %d channels, %s\n"
#define MSGTR_AO_ALSA5_SoundCardNotFound "[AO ALSA5] alsa-init: no soundcards found.\n"
#define MSGTR_AO_ALSA5_InvalidFormatReq "[AO ALSA5] alsa-init: invalid format (%s) requested - output disabled.\n"
#define MSGTR_AO_ALSA5_PlayBackError "[AO ALSA5] alsa-init: playback open error: %s\n"
#define MSGTR_AO_ALSA5_PcmInfoError "[AO ALSA5] alsa-init: pcm info error: %s\n"
#define MSGTR_AO_ALSA5_SoundcardsFound "[AO ALSA5] alsa-init: %d soundcard(s) found, using: %s\n"
#define MSGTR_AO_ALSA5_PcmChanInfoError "[AO ALSA5] alsa-init: pcm channel info error: %s\n"
#define MSGTR_AO_ALSA5_CantSetParms "[AO ALSA5] alsa-init: error setting parameters: %s\n"
#define MSGTR_AO_ALSA5_CantSetChan "[AO ALSA5] alsa-init: error setting up channel: %s\n"
#define MSGTR_AO_ALSA5_ChanPrepareError "[AO ALSA5] alsa-init: channel prepare error: %s\n"
#define MSGTR_AO_ALSA5_DrainError "[AO ALSA5] alsa-uninit: playback drain error: %s\n"
#define MSGTR_AO_ALSA5_FlushError "[AO ALSA5] alsa-uninit: playback flush error: %s\n"
#define MSGTR_AO_ALSA5_PcmCloseError "[AO ALSA5] alsa-uninit: pcm close error: %s\n"
#define MSGTR_AO_ALSA5_ResetDrainError "[AO ALSA5] alsa-reset: playback drain error: %s\n"
#define MSGTR_AO_ALSA5_ResetFlushError "[AO ALSA5] alsa-reset: playback flush error: %s\n"
#define MSGTR_AO_ALSA5_ResetChanPrepareError "[AO ALSA5] alsa-reset: channel prepare error: %s\n"
#define MSGTR_AO_ALSA5_PauseDrainError "[AO ALSA5] alsa-pause: playback drain error: %s\n"
#define MSGTR_AO_ALSA5_PauseFlushError "[AO ALSA5] alsa-pause: playback flush error: %s\n"
#define MSGTR_AO_ALSA5_ResumePrepareError "[AO ALSA5] alsa-resume: channel prepare error: %s\n"
#define MSGTR_AO_ALSA5_Underrun "[AO ALSA5] alsa-play: alsa underrun, resetting stream.\n"
#define MSGTR_AO_ALSA5_PlaybackPrepareError "[AO ALSA5] alsa-play: playback prepare error: %s\n"
#define MSGTR_AO_ALSA5_WriteErrorAfterReset "[AO ALSA5] alsa-play: write error after reset: %s - giving up.\n"
#define MSGTR_AO_ALSA5_OutPutError "[AO ALSA5] alsa-play: output error: %s\n"

// ao_plugin.c

#define MSGTR_AO_PLUGIN_InvalidPlugin "[AO PLUGIN] invalid plugin: %s\n"

// ======================= AF Audio Filters ================================

// libaf 

// af_ladspa.c

#define MSGTR_AF_LADSPA_AvailableLabels "available labels in"
#define MSGTR_AF_LADSPA_WarnNoInputs "WARNING! This LADSPA plugin has no audio inputs.\n  The incoming audio signal will be lost."
#define MSGTR_AF_LADSPA_ErrMultiChannel "Multi-channel (>2) plugins are not supported (yet).\n  Use only mono and stereo plugins."
#define MSGTR_AF_LADSPA_ErrNoOutputs "This LADSPA plugin has no audio outputs."
#define MSGTR_AF_LADSPA_ErrInOutDiff "The number of audio inputs and audio outputs of the LADSPA plugin differ."
#define MSGTR_AF_LADSPA_ErrFailedToLoad "failed to load"
#define MSGTR_AF_LADSPA_ErrNoDescriptor "Couldn't find ladspa_descriptor() function in the specified library file."
#define MSGTR_AF_LADSPA_ErrLabelNotFound "Couldn't find label in plugin library."
#define MSGTR_AF_LADSPA_ErrNoSuboptions "No suboptions specified"
#define MSGTR_AF_LADSPA_ErrNoLibFile "No library file specified"
#define MSGTR_AF_LADSPA_ErrNoLabel "No filter label specified"
#define MSGTR_AF_LADSPA_ErrNotEnoughControls "Not enough controls specified on the command line"
#define MSGTR_AF_LADSPA_ErrControlBelow "%s: Input control #%d is below lower boundary of %0.4f.\n"
#define MSGTR_AF_LADSPA_ErrControlAbove "%s: Input control #%d is above upper boundary of %0.4f.\n"

// format.c

#define MSGTR_AF_FORMAT_UnknownFormat "unknown-format "

// ========================== INPUT =========================================

// joystick.c

#define MSGTR_INPUT_JOYSTICK_Opening "Opening joystick device %s\n"
#define MSGTR_INPUT_JOYSTICK_CantOpen "Can't open joystick device %s: %s\n"
#define MSGTR_INPUT_JOYSTICK_ErrReading "Error while reading joystick device: %s\n"
#define MSGTR_INPUT_JOYSTICK_LoosingBytes "Joystick: we loose %d bytes of data\n"
#define MSGTR_INPUT_JOYSTICK_WarnLostSync "Joystick: warning init event, we have lost sync with driver\n"
#define MSGTR_INPUT_JOYSTICK_WarnUnknownEvent "Joystick warning unknown event type %d\n"

// input.c

#define MSGTR_INPUT_INPUT_ErrCantRegister2ManyCmdFds "Too many command file descriptors, cannot register file descriptor %d.\n"
#define MSGTR_INPUT_INPUT_ErrCantRegister2ManyKeyFds "Too many key file descriptors, cannot register file descriptor %d.\n"
#define MSGTR_INPUT_INPUT_ErrArgMustBeInt "Command %s: argument %d isn't an integer.\n"
#define MSGTR_INPUT_INPUT_ErrArgMustBeFloat "Command %s: argument %d isn't a float.\n"
#define MSGTR_INPUT_INPUT_ErrUnterminatedArg "Command %s: argument %d is unterminated.\n"
#define MSGTR_INPUT_INPUT_ErrUnknownArg "Unknown argument %d\n"
#define MSGTR_INPUT_INPUT_Err2FewArgs "Command %s requires at least %d arguments, we found only %d so far.\n"
#define MSGTR_INPUT_INPUT_ErrReadingCmdFd "Error while reading command file descriptor %d: %s\n"
#define MSGTR_INPUT_INPUT_ErrCmdBufferFullDroppingContent "Command buffer of file descriptor %d is full: dropping content\n"
#define MSGTR_INPUT_INPUT_ErrInvalidCommandForKey "Invalid command for bound key %s"
#define MSGTR_INPUT_INPUT_ErrSelect "Select error: %s\n"
#define MSGTR_INPUT_INPUT_ErrOnKeyInFd "Error on key input file descriptor %d\n"
#define MSGTR_INPUT_INPUT_ErrDeadKeyOnFd "Dead key input on file descriptor %d\n"
#define MSGTR_INPUT_INPUT_Err2ManyKeyDowns "Too many key down events at the same time\n"
#define MSGTR_INPUT_INPUT_ErrOnCmdFd "Error on command file descriptor %d\n"
#define MSGTR_INPUT_INPUT_ErrReadingInputConfig "Error while reading input config file %s: %s\n"
#define MSGTR_INPUT_INPUT_ErrUnknownKey "Unknown key '%s'\n"
#define MSGTR_INPUT_INPUT_ErrUnfinishedBinding "Unfinished binding %s\n"
#define MSGTR_INPUT_INPUT_ErrBuffer2SmallForKeyName "Buffer is too small for this key name: %s\n"
#define MSGTR_INPUT_INPUT_ErrNoCmdForKey "No command found for key %s"
#define MSGTR_INPUT_INPUT_ErrBuffer2SmallForCmd "Buffer is too small for command %s\n"
#define MSGTR_INPUT_INPUT_ErrWhyHere "What are we doing here?\n"
#define MSGTR_INPUT_INPUT_ErrCantInitJoystick "Can't init input joystick\n"
#define MSGTR_INPUT_INPUT_ErrCantStatFile "Can't stat %s: %s\n"
#define MSGTR_INPUT_INPUT_ErrCantOpenFile "Can't open %s: %s\n"

// ========================== LIBMPDEMUX ===================================

// url.c

#define MSGTR_MPDEMUX_URL_StringAlreadyEscaped "String appears to be already escaped in url_escape %c%c1%c2\n"

// ai_alsa1x.c

#define MSGTR_MPDEMUX_AIALSA1X_CannotSetSamplerate "Cannot set samplerate\n"
#define MSGTR_MPDEMUX_AIALSA1X_CannotSetBufferTime "Cannot set buffer time\n"
#define MSGTR_MPDEMUX_AIALSA1X_CannotSetPeriodTime "Cannot set period time\n"

// ai_alsa1x.c / ai_alsa.c

#define MSGTR_MPDEMUX_AIALSA_PcmBrokenConfig "Broken configuration for this PCM: no configurations available\n"
#define MSGTR_MPDEMUX_AIALSA_UnavailableAccessType "Access type not available\n"
#define MSGTR_MPDEMUX_AIALSA_UnavailableSampleFmt "Sample format not available\n"
#define MSGTR_MPDEMUX_AIALSA_UnavailableChanCount "Channel count not available - reverting to default: %d\n"
#define MSGTR_MPDEMUX_AIALSA_CannotInstallHWParams "Unable to install hw params: %s"
#define MSGTR_MPDEMUX_AIALSA_PeriodEqualsBufferSize "Can't use period equal to buffer size (%u == %lu)\n"
#define MSGTR_MPDEMUX_AIALSA_CannotInstallSWParams "Unable to install sw params:\n"
#define MSGTR_MPDEMUX_AIALSA_ErrorOpeningAudio "Error opening audio: %s\n"
#define MSGTR_MPDEMUX_AIALSA_AlsaStatusError "ALSA status error: %s"
#define MSGTR_MPDEMUX_AIALSA_AlsaXRUN "ALSA xrun!!! (at least %.3f ms long)\n"
#define MSGTR_MPDEMUX_AIALSA_AlsaStatus "ALSA Status:\n"
#define MSGTR_MPDEMUX_AIALSA_AlsaXRUNPrepareError "ALSA xrun: prepare error: %s"
#define MSGTR_MPDEMUX_AIALSA_AlsaReadWriteError "ALSA read/write error"

// ai_oss.c

#define MSGTR_MPDEMUX_AIOSS_Unable2SetChanCount "Unable to set channel count: %d\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2SetStereo "Unable to set stereo: %d\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2Open "Unable to open '%s': %s\n"
#define MSGTR_MPDEMUX_AIOSS_UnsupportedFmt "Not supported format\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2SetAudioFmt "Unable to set audio format."
#define MSGTR_MPDEMUX_AIOSS_Unable2SetSamplerate "Unable to set samplerate: %d\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2SetTrigger "Unable to set trigger: %d\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2GetBlockSize "Unable to get block size!\n"
#define MSGTR_MPDEMUX_AIOSS_AudioBlockSizeZero "Audio block size is zero, setting to %d!\n"
#define MSGTR_MPDEMUX_AIOSS_AudioBlockSize2Low "Audio block size too low, setting to %d!\n"

// asfheader.c

#define MSGTR_MPDEMUX_ASFHDR_HeaderSizeOver1MB "FATAL: header size bigger than 1 MB (%d)!\nPlease contact MPlayer authors, and upload/send this file.\n"
#define MSGTR_MPDEMUX_ASFHDR_HeaderMallocFailed "Could not allocate %d bytes for header\n"
#define MSGTR_MPDEMUX_ASFHDR_EOFWhileReadingHeader "EOF while reading asf header, broken/incomplete file?\n"
#define MSGTR_MPDEMUX_ASFHDR_DVRWantsLibavformat "DVR will probably only work with libavformat, try -demuxer 35 if you have problems\n"
#define MSGTR_MPDEMUX_ASFHDR_NoDataChunkAfterHeader "No data chunk following header!\n"
#define MSGTR_MPDEMUX_ASFHDR_AudioVideoHeaderNotFound "ASF: no audio or video headers found - broken file?\n"
#define MSGTR_MPDEMUX_ASFHDR_InvalidLengthInASFHeader "Invalid length in ASF header!\n"

// asf_mmst_streaming.c

#define MSGTR_MPDEMUX_MMST_WriteError "Write error\n"
#define MSGTR_MPDEMUX_MMST_EOFAlert "\nAlert! EOF\n"
#define MSGTR_MPDEMUX_MMST_PreHeaderReadFailed "pre-header read failed\n"
#define MSGTR_MPDEMUX_MMST_InvalidHeaderSize "Invalid header size, giving up\n"
#define MSGTR_MPDEMUX_MMST_HeaderDataReadFailed "Header data read failed\n"
#define MSGTR_MPDEMUX_MMST_packet_lenReadFailed "packet_len read failed\n"
#define MSGTR_MPDEMUX_MMST_InvalidRTSPPacketSize "Invalid rtsp packet size, giving up\n"
#define MSGTR_MPDEMUX_MMST_CmdDataReadFailed "Command data read failed\n"
#define MSGTR_MPDEMUX_MMST_HeaderObject "Header object\n"
#define MSGTR_MPDEMUX_MMST_DataObject "Data object\n"
#define MSGTR_MPDEMUX_MMST_FileObjectPacketLen "File object, packet length = %d (%d)\n"
#define MSGTR_MPDEMUX_MMST_StreamObjectStreamID "Stream object, stream id: %d\n"
#define MSGTR_MPDEMUX_MMST_2ManyStreamID "Too many id, stream skipped"
#define MSGTR_MPDEMUX_MMST_UnknownObject "Unknown object\n"
#define MSGTR_MPDEMUX_MMST_MediaDataReadFailed "Media data read failed\n"
#define MSGTR_MPDEMUX_MMST_MissingSignature "Missing signature\n"
#define MSGTR_MPDEMUX_MMST_PatentedTechnologyJoke "Everything done. Thank you for downloading a media file containing proprietary and patented technology.\n"
#define MSGTR_MPDEMUX_MMST_UnknownCmd "unknown command %02x\n"
#define MSGTR_MPDEMUX_MMST_GetMediaPacketErr "get_media_packet error : %s\n"
#define MSGTR_MPDEMUX_MMST_Connected "Connected\n"

// asf_streaming.c

#define MSGTR_MPDEMUX_ASF_StreamChunkSize2Small "Ahhhh, stream_chunck size is too small: %d\n"
#define MSGTR_MPDEMUX_ASF_SizeConfirmMismatch "size_confirm mismatch!: %d %d\n"
#define MSGTR_MPDEMUX_ASF_WarnDropHeader "Warning : drop header ????\n"
#define MSGTR_MPDEMUX_ASF_ErrorParsingChunkHeader "Error while parsing chunk header\n"
#define MSGTR_MPDEMUX_ASF_NoHeaderAtFirstChunk "Don't got a header as first chunk !!!!\n"
#define MSGTR_MPDEMUX_ASF_BufferMallocFailed "Error can't allocate %d bytes buffer\n"
#define MSGTR_MPDEMUX_ASF_ErrReadingNetworkStream "Error while reading network stream\n"
#define MSGTR_MPDEMUX_ASF_ErrChunk2Small "Error chunk is too small\n"
#define MSGTR_MPDEMUX_ASF_ErrSubChunkNumberInvalid "Error sub chunks number is invalid\n"
#define MSGTR_MPDEMUX_ASF_Bandwidth2SmallCannotPlay "Bandwidth too small, file cannot be played!\n"
#define MSGTR_MPDEMUX_ASF_Bandwidth2SmallDeselectedAudio "Bandwidth too small, deselected audio stream\n"
#define MSGTR_MPDEMUX_ASF_Bandwidth2SmallDeselectedVideo "Bandwidth too small, deselected video stream\n"
#define MSGTR_MPDEMUX_ASF_InvalidLenInHeader "Invalid length in ASF header!\n"
#define MSGTR_MPDEMUX_ASF_ErrReadingChunkHeader "Error while reading chunk header\n"
#define MSGTR_MPDEMUX_ASF_ErrChunkBiggerThanPacket "Error chunk_size > packet_size\n"
#define MSGTR_MPDEMUX_ASF_ErrReadingChunk "Error while reading chunk\n"
#define MSGTR_MPDEMUX_ASF_ASFRedirector "=====> ASF Redirector\n"
#define MSGTR_MPDEMUX_ASF_InvalidProxyURL "Invalid proxy URL\n"
#define MSGTR_MPDEMUX_ASF_UnknownASFStreamType "Unknown asf stream type\n"
#define MSGTR_MPDEMUX_ASF_Failed2ParseHTTPResponse "Failed to parse HTTP response\n"
#define MSGTR_MPDEMUX_ASF_ServerReturn "Server return %d:%s\n"
#define MSGTR_MPDEMUX_ASF_ASFHTTPParseWarnCuttedPragma "ASF HTTP PARSE WARNING : Pragma %s cuted from %d bytes to %d\n"
#define MSGTR_MPDEMUX_ASF_SocketWriteError "Socket write error : %s\n"
#define MSGTR_MPDEMUX_ASF_HeaderParseFailed "Failed to parse header\n"
#define MSGTR_MPDEMUX_ASF_NoStreamFound "No stream found\n"
#define MSGTR_MPDEMUX_ASF_UnknownASFStreamingType "Unknown ASF streaming type\n"
#define MSGTR_MPDEMUX_ASF_InfoStreamASFURL "STREAM_ASF, URL: %s\n"
#define MSGTR_MPDEMUX_ASF_StreamingFailed "Failed, exiting\n"

// audio_in.c

#define MSGTR_MPDEMUX_AUDIOIN_ErrReadingAudio "\nError reading audio: %s\n"
#define MSGTR_MPDEMUX_AUDIOIN_XRUNSomeFramesMayBeLeftOut "Recovered from cross-run, some frames may be left out!\n"
#define MSGTR_MPDEMUX_AUDIOIN_ErrFatalCannotRecover "Fatal error, cannot recover!\n"
#define MSGTR_MPDEMUX_AUDIOIN_NotEnoughSamples "\nNot enough audio samples!\n"

// aviheader.c

#define MSGTR_MPDEMUX_AVIHDR_EmptyList "** empty list?!\n"
#define MSGTR_MPDEMUX_AVIHDR_FoundMovieAt "Found movie at 0x%X - 0x%X\n"
#define MSGTR_MPDEMUX_AVIHDR_FoundBitmapInfoHeader "Found 'bih', %u bytes of %d\n"
#define MSGTR_MPDEMUX_AVIHDR_RegeneratingKeyfTableForMPG4V1 "Regenerating keyframe table for M$ mpg4v1 video\n"
#define MSGTR_MPDEMUX_AVIHDR_RegeneratingKeyfTableForDIVX3 "Regenerating keyframe table for DIVX3 video\n"
#define MSGTR_MPDEMUX_AVIHDR_RegeneratingKeyfTableForMPEG4 "Regenerating keyframe table for MPEG4 video\n"
#define MSGTR_MPDEMUX_AVIHDR_FoundWaveFmt "Found 'wf', %d bytes of %d\n"
#define MSGTR_MPDEMUX_AVIHDR_FoundAVIV2Header "AVI: dmlh found (size=%d) (total_frames=%d)\n"
#define MSGTR_MPDEMUX_AVIHDR_ReadingIndexBlockChunksForFrames "Reading INDEX block, %d chunks for %d frames (fpos=%"PRId64")\n"
#define MSGTR_MPDEMUX_AVIHDR_AdditionalRIFFHdr "Additional RIFF header...\n"
#define MSGTR_MPDEMUX_AVIHDR_WarnNotExtendedAVIHdr "** warning: this is no extended AVI header..\n"
#define MSGTR_MPDEMUX_AVIHDR_BrokenChunk "Broken chunk?  chunksize=%d  (id=%.4s)\n"
#define MSGTR_MPDEMUX_AVIHDR_BuildingODMLidx "AVI: ODML: Building odml index (%d superindexchunks)\n"
#define MSGTR_MPDEMUX_AVIHDR_BrokenODMLfile "AVI: ODML: Broken (incomplete?) file detected. Will use traditional index\n"
#define MSGTR_MPDEMUX_AVIHDR_CantReadIdxFile "Can't read index file %s: %s\n"
#define MSGTR_MPDEMUX_AVIHDR_NotValidMPidxFile "%s is not a valid MPlayer index file\n"
#define MSGTR_MPDEMUX_AVIHDR_FailedMallocForIdxFile "Could not allocate memory for index data from %s\n"
#define MSGTR_MPDEMUX_AVIHDR_PrematureEOF "Premature end of index file %s\n"
#define MSGTR_MPDEMUX_AVIHDR_IdxFileLoaded "Loaded index file: %s\n"
#define MSGTR_MPDEMUX_AVIHDR_GeneratingIdx "Generating Index: %3lu %s     \r"
#define MSGTR_MPDEMUX_AVIHDR_IdxGeneratedForHowManyChunks "AVI: Generated index table for %d chunks!\n"
#define MSGTR_MPDEMUX_AVIHDR_Failed2WriteIdxFile "Couldn't write index file %s: %s\n"
#define MSGTR_MPDEMUX_AVIHDR_IdxFileSaved "Saved index file: %s\n"

// cache2.c

#define MSGTR_MPDEMUX_CACHE2_NonCacheableStream "\rThis stream is non-cacheable.\n"
#define MSGTR_MPDEMUX_CACHE2_ReadFileposDiffers "!!! read_filepos differs!!! report this bug...\n"

// cdda.c

#define MSGTR_MPDEMUX_CDDA_CantOpenCDDADevice "Can't open CDDA device.\n"
#define MSGTR_MPDEMUX_CDDA_CantOpenDisc "Can't open disc.\n"
#define MSGTR_MPDEMUX_CDDA_AudioCDFoundWithNTracks "Found audio CD with %ld tracks.\n"

// cddb.c

#define MSGTR_MPDEMUX_CDDB_FailedToReadTOC "Failed to read TOC.\n"
#define MSGTR_MPDEMUX_CDDB_FailedToOpenDevice "Failed to open %s device.\n"
#define MSGTR_MPDEMUX_CDDB_NotAValidURL "Not a valid URL\n"
#define MSGTR_MPDEMUX_CDDB_FailedToSendHTTPRequest "Failed to send the HTTP request.\n"
#define MSGTR_MPDEMUX_CDDB_FailedToReadHTTPResponse "Failed to read the HTTP response.\n"
#define MSGTR_MPDEMUX_CDDB_HTTPErrorNOTFOUND "Not Found.\n"
#define MSGTR_MPDEMUX_CDDB_HTTPErrorUnknown "Unknown Error code\n"
#define MSGTR_MPDEMUX_CDDB_NoCacheFound "No cache found.\n"
#define MSGTR_MPDEMUX_CDDB_NotAllXMCDFileHasBeenRead "Not all the xmcd file has been read.\n"
#define MSGTR_MPDEMUX_CDDB_FailedToCreateDirectory "Failed to create directory %s.\n"
#define MSGTR_MPDEMUX_CDDB_NotAllXMCDFileHasBeenWritten "Not all of the xmcd file has been written.\n"
#define MSGTR_MPDEMUX_CDDB_InvalidXMCDDatabaseReturned "Invalid xmcd database file returned.\n"
#define MSGTR_MPDEMUX_CDDB_UnexpectedFIXME "Unexpected FIXME\n"
#define MSGTR_MPDEMUX_CDDB_UnhandledCode "Unhandled code\n"
#define MSGTR_MPDEMUX_CDDB_UnableToFindEOL "Unable to find end of line\n"
#define MSGTR_MPDEMUX_CDDB_ParseOKFoundAlbumTitle "Parse OK, found: %s\n"
#define MSGTR_MPDEMUX_CDDB_AlbumNotFound "Album not found\n"
#define MSGTR_MPDEMUX_CDDB_ServerReturnsCommandSyntaxErr "Server returns: Command syntax error\n"
#define MSGTR_MPDEMUX_CDDB_NoSitesInfoAvailable "No sites information available\n"
#define MSGTR_MPDEMUX_CDDB_FailedToGetProtocolLevel "Failed to get the protocol level\n"
#define MSGTR_MPDEMUX_CDDB_NoCDInDrive "No CD in the drive\n"

// cue_read.c

#define MSGTR_MPDEMUX_CUEREAD_UnexpectedCuefileLine "[bincue] Unexpected cuefile line: %s\n"
#define MSGTR_MPDEMUX_CUEREAD_BinFilenameTested "[bincue] bin filename tested: %s\n"
#define MSGTR_MPDEMUX_CUEREAD_CannotFindBinFile "[bincue] Couldn't find the bin file - giving up\n"
#define MSGTR_MPDEMUX_CUEREAD_UsingBinFile "[bincue] Using bin file %s\n"
#define MSGTR_MPDEMUX_CUEREAD_UnknownModeForBinfile "[bincue] unknown mode for binfile. Should not happen. Aborting.\n"
#define MSGTR_MPDEMUX_CUEREAD_CannotOpenCueFile "[bincue] Cannot open %s\n"
#define MSGTR_MPDEMUX_CUEREAD_ErrReadingFromCueFile "[bincue] Error reading from  %s\n"
#define MSGTR_MPDEMUX_CUEREAD_ErrGettingBinFileSize "[bincue] Error getting size of bin file\n"
#define MSGTR_MPDEMUX_CUEREAD_InfoTrackFormat "track %02d:  format=%d  %02d:%02d:%02d\n"
#define MSGTR_MPDEMUX_CUEREAD_UnexpectedBinFileEOF "[bincue] unexpected end of bin file\n"
#define MSGTR_MPDEMUX_CUEREAD_CannotReadNBytesOfPayload "[bincue] Couldn't read %d bytes of payload\n"
#define MSGTR_MPDEMUX_CUEREAD_CueStreamInfo_FilenameTrackTracksavail "CUE stream_open, filename=%s, track=%d, available tracks: %d -> %d\n"

// network.c

#define MSGTR_MPDEMUX_NW_UnknownAF "Unknown address family %d\n"
#define MSGTR_MPDEMUX_NW_ResolvingHostForAF "Resolving %s for %s...\n"
#define MSGTR_MPDEMUX_NW_CantResolv "Couldn't resolve name for %s: %s\n"
#define MSGTR_MPDEMUX_NW_ConnectingToServer "Connecting to server %s[%s]: %d...\n"
#define MSGTR_MPDEMUX_NW_CantConnect2Server "Failed to connect to server with %s\n"
#define MSGTR_MPDEMUX_NW_SelectFailed "Select failed.\n"
#define MSGTR_MPDEMUX_NW_ConnTimeout "Connection timeout.\n"
#define MSGTR_MPDEMUX_NW_GetSockOptFailed "getsockopt failed: %s\n"
#define MSGTR_MPDEMUX_NW_ConnectError "Connect error: %s\n"
#define MSGTR_MPDEMUX_NW_InvalidProxySettingTryingWithout "Invalid proxy setting... Trying without proxy.\n"
#define MSGTR_MPDEMUX_NW_CantResolvTryingWithoutProxy "Could not resolve remote hostname for AF_INET. Trying without proxy.\n"
#define MSGTR_MPDEMUX_NW_ErrSendingHTTPRequest "Error while sending HTTP request: didn't sent all the request.\n"
#define MSGTR_MPDEMUX_NW_ReadFailed "Read failed.\n"
#define MSGTR_MPDEMUX_NW_Read0CouldBeEOF "http_read_response read 0 (i.e. EOF)\n"
#define MSGTR_MPDEMUX_NW_AuthFailed "Authentication failed. Please use the -user and -passwd options to provide your\n"\
"username/password for a list of URLs, or form an URL like:\n"\
"http://username:password@hostname/file\n"
#define MSGTR_MPDEMUX_NW_AuthRequiredFor "Authentication required for %s\n"
#define MSGTR_MPDEMUX_NW_AuthRequired "Authentication required.\n"
#define MSGTR_MPDEMUX_NW_NoPasswdProvidedTryingBlank "No password provided, trying blank password.\n"
#define MSGTR_MPDEMUX_NW_ErrServerReturned "Server return %d: %s\n"
#define MSGTR_MPDEMUX_NW_CacheSizeSetTo "Cache size set to %d KBytes\n"

// ========================== LIBMPMENU ===================================

// libmenu/menu.c
#define MSGTR_LIBMENU_SyntaxErrorAtLine "[MENU] Syntax error at line: %d\n"
#define MSGTR_LIBMENU_MenuDefinitionsNeedANameAttrib "[MENU] Menu definitions need a name attribute (line %d)\n"
#define MSGTR_LIBMENU_BadAttrib "[MENU] Bad attribute %s=%s in menu '%s' at line %d\n"
#define MSGTR_LIBMENU_UnknownMenuType "[MENU] Unknown menu type '%s' at line %d\n"
#define MSGTR_LIBMENU_CantOpenConfigFile "[MENU] Can't open menu config file: %s\n"
#define MSGTR_LIBMENU_ConfigFileIsTooBig "[MENU] Config file is too big (> %d KB)\n"
#define MSGTR_LIBMENU_ConfigFileIsEmpty "[MENU] Config file is empty\n"
#define MSGTR_LIBMENU_MenuNotFound "[MENU] Menu %s not found.\n"
#define MSGTR_LIBMENU_MenuInitFailed "[MENU] Menu '%s': init failed\n"
#define MSGTR_LIBMENU_UnsupportedOutformat "[MENU] Unsupported output format!!!!\n"

// libmenu/menu_cmdlist.c
#define MSGTR_LIBMENU_NoEntryFoundInTheMenuDefinition "[MENU] No entry found in the menu definition.\n"
#define MSGTR_LIBMENU_ListMenuEntryDefinitionsNeedAName "[MENU] List menu entry definitions need a name (line %d).\n"
#define MSGTR_LIBMENU_ListMenuNeedsAnArgument "[MENU] List menu needs an argument.\n"

// libmenu/menu_console.c
#define MSGTR_LIBMENU_WaitPidError "[MENU] Waitpid error: %s.\n"
#define MSGTR_LIBMENU_SelectError "[MENU] Select error.\n"
#define MSGTR_LIBMENU_ReadErrorOnChilds "[MENU] Read error on child's: %s.\n"
#define MSGTR_LIBMENU_ConsoleRun "[MENU] Console run: %s ...\n"
#define MSGTR_LIBMENU_AChildIsAlreadyRunning "[MENU] A child is already running.\n"
#define MSGTR_LIBMENU_ForkFailed "[MENU] Fork failed !!!\n"
#define MSGTR_LIBMENU_WriteError "[MENU] Write error.\n"

// libmenu/menu_filesel.c
#define MSGTR_LIBMENU_OpendirError "[MENU] Opendir error: %s.\n"
#define MSGTR_LIBMENU_ReallocError "[MENU] Realloc error: %s.\n"
#define MSGTR_LIBMENU_MallocError "[MENU] Memory allocation error: %s.\n"
#define MSGTR_LIBMENU_ReaddirError "[MENU] Readdir error: %s.\n"
#define MSGTR_LIBMENU_CantOpenDirectory "[MENU] Can't open directory %s\n"

// libmenu/menu_param.c
#define MSGTR_LIBMENU_NoEntryFoundInTheMenuDefinition "[MENU] No entry found in the menu definition.\n"
#define MSGTR_LIBMENU_SubmenuDefinitionNeedAMenuAttribut "[MENU] Submenu definition needs a 'menu' attribute.\n"
#define MSGTR_LIBMENU_PrefMenuEntryDefinitionsNeed "[MENU] Pref menu entry definitions need a valid 'property' attribute (line %d).\n"
#define MSGTR_LIBMENU_PrefMenuNeedsAnArgument "[MENU] Pref menu needs an argument.\n"

// libmenu/menu_pt.c
#define MSGTR_LIBMENU_CantfindTheTargetItem "[MENU] Can't find the target item ????\n"
#define MSGTR_LIBMENU_FailedToBuildCommand "[MENU] Failed to build command: %s.\n"

// libmenu/menu_txt.c
#define MSGTR_LIBMENU_MenuTxtNeedATxtFileName "[MENU] Text menu needs a txt file name (param file).\n"
#define MSGTR_LIBMENU_MenuTxtCantOpen "[MENU] Can't open: %s.\n"
#define MSGTR_LIBMENU_WarningTooLongLineSplitting "[MENU] Warning, line too long. Splitting it.\n"
#define MSGTR_LIBMENU_ParsedLines "[MENU] Parsed %d lines.\n"

// libmenu/vf_menu.c
#define MSGTR_LIBMENU_UnknownMenuCommand "[MENU] Unknown command: '%s'.\n"
#define MSGTR_LIBMENU_FailedToOpenMenu "[MENU] Failed to open menu: '%s'.\n"

// ========================== LIBMPCODECS ===================================

// libmpcodecs/ad_libdv.c
#define MSGTR_MPCODECS_AudioFramesizeDiffers "[AD_LIBDV] Warning! Audio framesize differs! read=%d  hdr=%d.\n"

// libmpcodecs/vd_dmo.c vd_dshow.c vd_vfw.c
#define MSGTR_MPCODECS_CouldntAllocateImageForCinepakCodec "[VD_DMO] Couldn't allocate image for cinepak codec.\n"

// libmpcodecs/vd_ffmpeg.c
#define MSGTR_MPCODECS_XVMCAcceleratedCodec "[VD_FFMPEG] XVMC accelerated codec.\n"
#define MSGTR_MPCODECS_ArithmeticMeanOfQP "[VD_FFMPEG] Arithmetic mean of QP: %2.4f, Harmonic mean of QP: %2.4f\n"
#define MSGTR_MPCODECS_DRIFailure "[VD_FFMPEG] DRI failure.\n"
#define MSGTR_MPCODECS_CouldntAllocateImageForCodec "[VD_FFMPEG] Couldn't allocate image for codec.\n"
#define MSGTR_MPCODECS_XVMCAcceleratedMPEG2 "[VD_FFMPEG] XVMC accelerated MPEG2.\n"
#define MSGTR_MPCODECS_TryingPixfmt "[VD_FFMPEG] Trying pixfmt=%d.\n"
#define MSGTR_MPCODECS_McGetBufferShouldWorkOnlyWithXVMC "[VD_FFMPEG] The mc_get_buffer should work only with XVMC acceleration!!"
#define MSGTR_MPCODECS_UnexpectedInitVoError "[VD_FFMPEG] Unexpected init_vo error.\n"
#define MSGTR_MPCODECS_UnrecoverableErrorRenderBuffersNotTaken "[VD_FFMPEG] Unrecoverable error, render buffers not taken.\n"
#define MSGTR_MPCODECS_OnlyBuffersAllocatedByVoXvmcAllowed "[VD_FFMPEG] Only buffers allocated by vo_xvmc allowed.\n"

// libmpcodecs/ve_lavc.c
#define MSGTR_MPCODECS_HighQualityEncodingSelected "[VE_LAVC] High quality encoding selected (non real time)!\n"
#define MSGTR_MPCODECS_UsingConstantQscale "[VE_LAVC] Using constant qscale = %f (VBR).\n"

// libmpcodecs/ve_raw.c
#define MSGTR_MPCODECS_OutputWithFourccNotSupported "[VE_RAW] Raw output with fourcc [%x] not supported!\n"
#define MSGTR_MPCODECS_NoVfwCodecSpecified "[VE_RAW] Required VfW codec not specified!!\n"

// libmpcodecs/vf_crop.c
#define MSGTR_MPCODECS_CropBadPositionWidthHeight "[CROP] Bad position/width/height - cropped area outside of the original!\n"

// libmpcodecs/vf_cropdetect.c
#define MSGTR_MPCODECS_CropArea "[CROP] Crop area: X: %d..%d  Y: %d..%d  (-vf crop=%d:%d:%d:%d).\n"

// libmpcodecs/vf_format.c, vf_palette.c, vf_noformat.c
#define MSGTR_MPCODECS_UnknownFormatName "[VF_FORMAT] Unknown format name: '%s'.\n"

// libmpcodecs/vf_framestep.c vf_noformat.c vf_palette.c vf_tile.c
#define MSGTR_MPCODECS_ErrorParsingArgument "[VF_FRAMESTEP] Error parsing argument.\n"

// libmpcodecs/ve_vfw.c
#define MSGTR_MPCODECS_CompressorType "Compressor type: %.4lx\n"
#define MSGTR_MPCODECS_CompressorSubtype "Compressor subtype: %.4lx\n"
#define MSGTR_MPCODECS_CompressorFlags "Compressor flags: %lu, version %lu, ICM version: %lu\n"
#define MSGTR_MPCODECS_Flags "Flags:"
#define MSGTR_MPCODECS_Quality " quality"

// libmpcodecs/vf_expand.c
#define MSGTR_MPCODECS_FullDRNotPossible "Full DR not possible, trying SLICES instead!\n"
#define MSGTR_MPCODECS_WarnNextFilterDoesntSupportSlices  "WARNING! Next filter doesn't support SLICES, get ready for sig11...\n"
#define MSGTR_MPCODECS_FunWhydowegetNULL "Why do we get NULL??\n"

// libmpcodecs/vf_fame.c
#define MSGTR_MPCODECS_FatalCantOpenlibFAME "FATAL: cannot open libFAME!\n"

// libmpcodecs/vf_test.c, vf_yuy2.c, vf_yvu9.c
#define MSGTR_MPCODECS_WarnNextFilterDoesntSupport "%s not supported by next filter/vo :(\n"

#ifdef HELP_MP_DEFINE_STATIC
static char* banner_text=
"\n\n"
"MPlayer " VERSION "(C) 2000-2001 Arpad Gereoffy (see DOCS/AUTHORS)\n"
"\n";

static char help_text[]=
"Usage:   mplayer [options] [path/]name\n"
"\n"
"Options:\n"
" -vo <drv[:dev]> select video output driver & device (see '-vo help' for list)\n"
" -ao <drv[:dev]> select audio output driver & device (see '-ao help' for list)\n"
" -vcd <trackno>  play VCD (video cd) track from device instead of plain file\n"
#ifdef HAVE_LIBCSS
" -dvdauth <dev>  specify DVD device for authentication (for encrypted discs)\n"
#endif
" -ss <timepos>   seek to given (seconds or hh:mm:ss) position\n"
" -nosound        don't play sound\n"
#ifdef USE_FAKE_MONO
" -stereo         select MPEG1 stereo output (0:stereo 1:left 2:right)\n"
#endif
" -fs -vm -zoom   fullscreen playing options (fullscr,vidmode chg,softw.scale)\n"
" -x <x> -y <y>   scale image to <x> * <y> resolution [if -vo driver supports!]\n"
" -sub <file>     specify subtitle file to use (see also -subfps, -subdelay)\n"
" -vid x -aid y   options to select video (x) and audio (y) stream to play\n"
" -fps x -srate y options to change video (x fps) and audio (y Hz) rate\n"
" -pp <quality>   enable postprocessing filter (0-4 for DivX, 0-63 for mpegs)\n"
" -bps            use alternative A-V sync method for AVI files (may help!)\n"
" -framedrop      enable frame-dropping (for slow machines)\n"
"\n"
"Keys:\n"
" <-  or  ->      seek backward/forward 10 seconds\n"
" up or down      seek backward/forward  1 minute\n"
" p or SPACE      pause movie (press any key to continue)\n"
" q or ESC        stop playing and quit program\n"
" + or -          adjust audio delay by +/- 0.1 second\n"
" o               cycle OSD mode:  none / seekbar / seekbar+timer\n"
" * or /          increase or decrease volume (press 'm' to select master/pcm)\n"
" z or x          adjust subtitle delay by +/- 0.1 second\n"
"\n"
" * * * SEE MANPAGE FOR DETAILS, FURTHER OPTIONS AND KEYS ! * * *\n"
"\n";
#endif

// mplayer.c: 

#define MSGTR_Exiting "\nExiting... (%s)\n"
#define MSGTR_Exit_frames "Requested number of frames played"
#define MSGTR_Exit_quit "Quit"
#define MSGTR_Exit_eof "End of file"
#define MSGTR_IntBySignal "\nMPlayer interrupted by signal %d in module: %s \n"
#define MSGTR_NoHomeDir "Can't find HOME dir\n"
#define MSGTR_GetpathProblem "get_path(\"config\") problem\n"
#define MSGTR_CreatingCfgFile "Creating config file: %s\n"
#define MSGTR_InvalidVOdriver "Invalid video output driver name: %s\nUse '-vo help' to get a list of available video drivers.\n"
#define MSGTR_InvalidAOdriver "Invalid audio output driver name: %s\nUse '-ao help' to get a list of available audio drivers.\n"
#define MSGTR_CopyCodecsConf "(copy/link DOCS/codecs.conf to ~/.mplayer/codecs.conf)\n"
#define MSGTR_CantLoadFont "Can't load font: %s\n"
#define MSGTR_CantLoadSub "Can't load subtitles: %s\n"
#define MSGTR_ErrorDVDkey "Error processing DVD KEY.\n"
#define MSGTR_CmdlineDVDkey "DVD command line requested key is stored for descrambling.\n"
#define MSGTR_DVDauthOk "DVD auth sequence seems to be OK.\n"
#define MSGTR_DumpSelectedSteramMissing "dump: FATAL: selected stream missing!\n"
#define MSGTR_CantOpenDumpfile "Can't open dump file!!!\n"
#define MSGTR_CoreDumped "core dumped :)\n"
#define MSGTR_FPSnotspecified "FPS not specified (or invalid) in the header! Use the -fps option!\n"
#define MSGTR_NoVideoStream "Sorry, no video stream... it's unplayable yet\n"
#define MSGTR_TryForceAudioFmt "Trying to force audio codec driver family %d ...\n"
#define MSGTR_CantFindAfmtFallback "Can't find audio codec for forced driver family, fallback to other drivers.\n"
#define MSGTR_CantFindAudioCodec "Can't find codec for audio format 0x%X !\n"
#define MSGTR_TryUpgradeCodecsConfOrRTFM "*** Try to upgrade %s from DOCS/codecs.conf\n*** If it's still not OK, then read DOCS/CODECS!\n"
#define MSGTR_CouldntInitAudioCodec "Couldn't initialize audio codec! -> nosound\n"
#define MSGTR_TryForceVideoFmt "Trying to force video codec driver family %d ...\n"
#define MSGTR_CantFindVfmtFallback "Can't find video codec for forced driver family, fallback to other drivers.\n"
#define MSGTR_CantFindVideoCodec "Can't find codec for video format 0x%X !\n"
#define MSGTR_VOincompCodec "Sorry, selected video_out device is incompatible with this codec.\n"
#define MSGTR_CouldntInitVideoCodec "FATAL: Couldn't initialize video codec :(\n"
#define MSGTR_EncodeFileExists "File already exists: %s (don't overwrite your favourite AVI!)\n"
#define MSGTR_CantCreateEncodeFile "Cannot create file for encoding\n"
#define MSGTR_CannotInitVO "FATAL: Cannot initialize video driver!\n"
#define MSGTR_CannotInitAO "couldn't open/init audio device -> NOSOUND\n"
#define MSGTR_StartPlaying "Start playing...\n"
#define MSGTR_SystemTooSlow "\n************************************************************************"\
			    "\n** Your system is too SLOW to play this! try with -framedrop or RTFM! **"\
			    "\n************************************************************************\n"
//#define MSGTR_

// open.c: 
#define MSGTR_CdDevNotfound "A CD-ROM meghajt� (%s) nem tal�lhat�!\n"
#define MSGTR_ErrTrackSelect "Hiba a VCD s�v kiv�laszt�sakor!"
#define MSGTR_ReadSTDIN "Olvas�s a szabv�nyos bemenetr�l (stdin)...\n"
#define MSGTR_UnableOpenURL "Nem megnyithat� az URL: %s\n"
#define MSGTR_ConnToServer "Csatlakozom a szerverhez: %s\n"
#define MSGTR_FileNotFound "A f�jl nem tal�lhat�: '%s'\n"

// demuxer.c:
#define MSGTR_AudioStreamRedefined "Vigy�zat! T�bbsz�r�sen defin�lt Audio folyam! (Hib�s f�jl?)\n"
#define MSGTR_VideoStreamRedefined "Vigy�zat! T�bbsz�r�sen defin�lt Video folyam! (Hib�s f�jl?)\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: T�l sok (%d db, %d b�jt) audio csomag a pufferben!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: T�l sok (%d db, %d b�jt) video csomag a pufferben!\n"
#define MSGTR_MaybeNI "(tal�n ez egy nem �sszef�s�lt f�jl vagy a CODEC nem m�k�dik j�l)\n"
#define MSGTR_DetectedAVIfile "Ez egy AVI form�tum� f�jl!\n"
#define MSGTR_DetectedASFfile "Ez egy ASF form�tum� f�jl!\n"
#define MSGTR_DetectedMPEGPESfile "Ez egy MPEG-PES form�tum� f�jl!\n"
#define MSGTR_DetectedMPEGPSfile "Ez egy MPEG-PS form�tum� f�jl!\n"
#define MSGTR_DetectedMPEGESfile "Ez egy MPEG-ES form�tum� f�jl!\n"
#define MSGTR_DetectedQTMOVfile "Ez egy QuickTime/MOV form�tum� f�jl! (ez m�g nem t�mogatott)\n"
#define MSGTR_MissingMpegVideo "Nincs MPEG video folyam!? L�pj kapcsolatba a k�sz�t�kkel, lehet hogy hiba!\n"
#define MSGTR_InvalidMPEGES "Hib�s MPEG-ES folyam??? L�pj kapcsolatba a k�sz�t�kkel, lehet hogy hiba!\n"
#define MSGTR_FormatNotRecognized "========= Sajnos ez a f�jlform�tum ismeretlen vagy nem t�mogatott ===========\n"\
				  "= Ha ez egy AVI, ASF vagy MPEG f�jl, l�pj kapcsolatba a k�sz�t�kkel (hiba)! =\n"
#define MSGTR_MissingASFvideo "ASF: Nincs k�p folyam!\n"
#define MSGTR_MissingASFaudio "ASF: Nincs hang folyam...  -> hang n�lk�l\n"
#define MSGTR_MissingMPEGaudio "MPEG: Nincs hang folyam...  -> hang n�lk�l\n"

//#define MSGTR_



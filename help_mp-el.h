// Translated by: Ioannis Panteleakis <pioann@csd.auth.gr>

// Translated files should be uploaded to ftp://mplayerhq.hu/MPlayer/incoming
// and send a notify message to mplayer-dev-eng maillist.

// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char* banner_text=
"\n\n"
"MPlayer " VERSION "(C) 2000-2002 Arpad Gereoffy (����� DOCS!)\n"
"\n";

static char help_text[]=
#ifdef HAVE_NEW_GUI
"Usage:   mplayer [-gui] [��������] [��������/]�����_�������\n"
#else
"Usage:   mplayer [��������] [��������/]�����_�������\n"
#endif
"\n"
"��������:\n"
" -vo <drv[:dev]> �������� ��� ����� ������ ������ ��� �� ������� (����� '-vo help' ��� �� �����)\n"
" -ao <drv[:dev]> �������� ��� ����� ������ ���� ��� �� ������� (����� '-ao help' ��� �� �����)\n"
" -vcd <trackno>  ����������� VCD (video cd) track ��� ������� ���� ��� ������\n"
#ifdef HAVE_LIBCSS
" -dvdauth <dev>  ������ �� ������� DVD ��� ����������� (��� ����������������� �������)\n"
#endif
#ifdef USE_DVDREAD
" -dvd <titleno>  ����������� ��� ������/track DVD ��� �� ������� ���� ��� ������\n"
#endif
" -ss <timepos>   ��������� �� �������� ���� (������������ � hh:mm:ss)\n"
" -nosound        �� ����������� ��� ����\n"
#ifdef USE_FAKE_MONO
" -stereo <mode>  ������� ������ MPEG1 stereo (0:stereo 1:�������� 2:�����)\n"
#endif
" -channels <n>   � ������� ��� �������� ������ ��� ����\n"
" -fs -vm -zoom   �������� ��� ����������� �� ����� ����� (fullscr,vidmode chg,softw.scale)\n"
" -x <x> -y <y>   ��������� ������� �� <x> * <y> ��������� [�� � -vo ������ �� �����������!]\n"
" -sub <������>   ������� ��� ������� ��������� ��� ����� (����� ������ -subfps, -subdelay)\n"
" -playlist <������> ������ �� ������ ��� playlist\n"
" -vid x -aid y   �������� ��� ������� ������ (x) ��� ��� (y) stream ��� �����������\n"
" -fps x -srate y �������� ��� ��� ������ ��� ���������� ��� ������ (x fps) ��� ��� ���� (y Hz)\n"
" -pp <��������>  ������������ ��� ������� postprocessing (0-4 ��� DivX, 0-63 ��� mpegs)\n"
" -nobps          ����� ������������ ������� ������������ A-V ��� AVI ������ (������ �� ��������!)\n"
" -framedrop      ������������ ��� frame-dropping (��� ���� ����������)\n"
" -wid <id ���������> ����� ������ ��������� ��� ����� ������ (������� �� plugger!)\n"
"\n"
"Keys:\n"
" <-  or  ->      ��������� �����/���� ���� 10 ������������\n"
" up or down      ��������� �����/���� ���� 1 �����\n"
" < or >          ��������� �����/���� ���� playlist\n"
" p or SPACE      ����� ������� (������� ����������� ������� ��� �� ����������)\n"
" q or ESC        ���� ��� ����������� ��� ������ ������������\n"
" + or -          ������� ������������ ���� ���� +/- 0.1 ������������\n"
" o               ������ ��� OSD �������:  ������ / seekbar / seekbar+������\n"
" * or /          ������ � ������ ��� ������� ��� ���� (������� 'm' ��� ������� master/pcm)\n"
" z or x          ������� ������������ ��������� ���� +/- 0.1 ������������\n"
"\n"
" * * * ����� MANPAGE ��� ������������ ������������, ��� ��� ������������ �������� ��� KEYS ! * * *\n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// mplayer.c:

#define MSGTR_Exiting "\n������... (%s)\n"
#define MSGTR_Exit_frames "� ����������� ������� ��� frames ����� ������������"
#define MSGTR_Exit_quit "��������"
#define MSGTR_Exit_eof "����� ��� �������"
#define MSGTR_Exit_error "������� ������"
#define MSGTR_IntBySignal "\n�� MPlayer ������������ ��� �� ���� %d ��� module: %s \n"
#define MSGTR_NoHomeDir "�� ������ � ������ ��� HOME �������\n"
#define MSGTR_GetpathProblem "get_path(\"config\") ��������\n"
#define MSGTR_CreatingCfgFile "���������� ��� ������� config: %s\n"
#define MSGTR_InvalidVOdriver "����� ����� ��� ��� ����� ������ ������: %s\n�������������� '-vo help' ��� �� ����� �� ����� ��� ���������� ������ ������ ������.\n"
#define MSGTR_InvalidAOdriver "����� ����� ��� ��� ����� ������ ����: %s\n�������������� '-ao help' ��� �� ����� �� ����� ��� ���������� ������ ������ ����.\n"
#define MSGTR_CopyCodecsConf "(���������/ln etc/codecs.conf (��� ��� ������ ��� MPlayer) ��� ~/.mplayer/codecs.conf)\n"
#define MSGTR_CantLoadFont "�� ���������� �������� ��� ��������������: %s\n"
#define MSGTR_CantLoadSub "�� ���������� �������� ��� ���������: %s\n"
#define MSGTR_ErrorDVDkey "������ ���� ��� ����������� ��� DVD KEY.\n"
#define MSGTR_CmdlineDVDkey "�� ��������� ������ ��� �� DVD ������������ ��� descrambling.\n"
#define MSGTR_DVDauthOk "� ��������� ������������ ��� DVD �������� �������.\n"
#define MSGTR_DumpSelectedSteramMissing "dump: �������: ������ �� ���������� stream!\n"
#define MSGTR_CantOpenDumpfile "������� �� ������� ��� dump �������!!!\n"
#define MSGTR_CoreDumped "core dumped :)\n"
#define MSGTR_FPSnotspecified "�� �������� FPS (� �����) ��� header! �������������� ��� ������� -fps!\n"
#define MSGTR_NoVideoStream "�������, ��� ������� ������ stream... ��� ������ �� ������������ �����\n"
#define MSGTR_TryForceAudioFmt "���������� �� �������� ��� ����������� ��� ������ ��� codec ��� ���� %d ...\n"
#define MSGTR_CantFindAfmtFallback "��� ����� ������ � ������ ��� ����������� ��� ������ ��� codec ��� ����, ����� ����� ������.\n"
#define MSGTR_CantFindAudioCodec "��� ����� ������ � ������ ��� format ��� codec ��� ���� 0x%X !\n"
#define MSGTR_TryUpgradeCodecsConfOrRTFM "*** ��������� �� ������������� �� %s ��� �� etc/codecs.conf\n*** �� ����� ������� �������, �������� DOCS/codecs.html!\n"
#define MSGTR_CouldntInitAudioCodec "������� � ������������ ��� codec ��� ����! -> �����-���\n"
#define MSGTR_TryForceVideoFmt "���������� �� �������� ��� ����������� ��� ������ ��� codec ��� ������ %d ...\n"
#define MSGTR_CantFindVfmtFallback "��� ����� ������ � ������ ��� ����������� ��� ������ ��� codec ��� ������, ����� ����� ������.\n"
#define MSGTR_CantFindVideoCodec "��� ����� ������ � ������ ��� codec ��� ��� ������������ -vo ��� �� format ��� ������ 0x%X !\n"
#define MSGTR_VOincompCodec "�������, � ���������� ������� video_out ����� �������� �� ���� �� codec.\n"
#define MSGTR_CouldntInitVideoCodec "�������: ������� � ������������ ��� codec ��� ������ :(\n"
#define MSGTR_EncodeFileExists "�� ������ ������� ���: %s (��� ���������� �� ��������� ��� AVI!)\n"
#define MSGTR_CantCreateEncodeFile "������� � ���������� ��� ������� ��� ������������\n"
#define MSGTR_CannotInitVO "�������: ������� � ������������ ��� ������ ��� ������!\n"
#define MSGTR_CannotInitAO "������� �� �������/������������ ��� ������ ��� ���� -> �����-���\n"
#define MSGTR_StartPlaying "�������� ������������...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"         **************************************************************************\n"\
"         **** �� ������� ��� ����� ���� ���� ��� ��� ����������� ��� �������!  ****\n"\
"         **************************************************************************\n"\
"!!! ������� ������, ����������, ������: \n"\
"- ������ �����: �������� �� ��� ����� ��� ����. ����: ��������� -ao sdl � ��������������\n"\
"  ALSA 0.5 � oss emulation ��� ������ ALSA 0.9. �������� DOCS/sound.html ��� ������������ ������!\n"\
"- ���� ������ ��� ������. ��������� ����������� -vo ����� (��� �����: -vo help) � ���������\n"\
"  �� -framedrop !  �������� DOCS/video.html ��� �������/���������� ��� ������.\n"\
"- ����� ������������. ��� ����������� ������ dvd/divx �� ������ ������������! ���������� �� -hardframedrop\n"\
"- Broken file. ��������� �� ��������� ����������� ��� �� ��������: -nobps  -ni  -mc 0  -forceidx\n"\
"�� ������ ��� ���� ��� �����, ���� �������� DOCS/bugreports.html !\n\n"

#define MSGTR_NoGui "�� MPlayer ������������ ����� ���������� ��� GUI!\n"
#define MSGTR_GuiNeedsX "�� GUI ��� MPlayer ���������� X11!\n"
#define MSGTR_Playing "����������� ��� %s\n"
#define MSGTR_NoSound "����: �� ���������!!!\n"
#define MSGTR_FPSforced "�� FPS ����������� �� ����� %5.3f  (ftime: %5.3f)\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "� CD-ROM ������� '%s' ��� �������!\n"
#define MSGTR_ErrTrackSelect "������ ���� ������� ��� VCD track!"
#define MSGTR_ReadSTDIN "����������� ��� �� stdin...\n"
#define MSGTR_UnableOpenURL "������� �� ������� ��� URL: %s\n"
#define MSGTR_ConnToServer "���������������� ������� �� ��� server: %s\n"
#define MSGTR_FileNotFound "�� ������: '%s' ��� �������\n"

#define MSGTR_CantOpenDVD "��� ������� �� ������ ��� DVD �������: %s\n"
#define MSGTR_DVDwait "�������� ����� ��� ������, �������� ����������...\n"
#define MSGTR_DVDnumTitles "�������� %d ������ ��� DVD.\n"
#define MSGTR_DVDinvalidTitle "Invalid DVD title number: %d\n"
#define MSGTR_DVDnumChapters "�������� %d �������� �� ����� ��� ����� ��� DVD.\n"
#define MSGTR_DVDinvalidChapter "����� ������� ��� ��������� ��� DVD: %d\n"
#define MSGTR_DVDnumAngles "�������� %d �������� ���� ��� ����� ��� DVD.\n"
#define MSGTR_DVDinvalidAngle "����� ������� ��� ������ ��� DVD: %d\n"
#define MSGTR_DVDnoIFO "��� ����� ������ �� ������� ��� IFO ������ ��� ��� ����� ��� DVD %d.\n"
#define MSGTR_DVDnoVOBs "��� ����� ������ �� ������� ��� VOBS (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "�� DVD ������ �� ��������!\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "�������������! � ����������� ��� �������� ���� %d �������� ����!\n"
#define MSGTR_VideoStreamRedefined "�������������! � ����������� ��� �������� ������ %d �������� ����!\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: �������� (%d �� %d bytes) ������ ���� ���� buffer!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: �������� (%d �� %d bytes) ������ ������ ���� buffer!\n"
#define MSGTR_MaybeNI "(���� ������������ ���� non-interleaved ������/������ � ������� �� codec)\n"
#define MSGTR_DetectedFILMfile "������������� ������ ����� FILM!\n"
#define MSGTR_DetectedFLIfile "������������� ������ ����� FLI!\n"
#define MSGTR_DetectedROQfile "������������� ������ ����� RoQ!\n"
#define MSGTR_DetectedREALfile "������������� ������ ����� REAL!\n"
#define MSGTR_DetectedAVIfile "������������� ������ ����� AVI!\n"
#define MSGTR_DetectedASFfile "������������� ������ ����� ASF!\n"
#define MSGTR_DetectedMPEGPESfile "������������� ������ ����� MPEG-PES!\n"
#define MSGTR_DetectedMPEGPSfile "������������� ������ ����� MPEG-PS!\n"
#define MSGTR_DetectedMPEGESfile "������������� ������ ����� MPEG-ES!\n"
#define MSGTR_DetectedQTMOVfile "������������� ������ ����� QuickTime/MOV!\n"
#define MSGTR_MissingMpegVideo "������ �� ������ ������ MPEG!? ������������ �� ��� author, ������ �� ����� ��� bug :(\n"
#define MSGTR_InvalidMPEGES "�� ������������ ������ MPEG-ES??? ������������ �� ��� author, ������ �� ����� ��� bug :(\n"
#define MSGTR_FormatNotRecognized "============= �������, ���� �� ����� ������� ��� �������������/������������� ===============\n"\
				  "=== If this file is an AVI, ASF or MPEG stream, �������� ������������� �� ��� author! ===\n"
#define MSGTR_MissingVideoStream "��� ������� ������ ������!\n"
#define MSGTR_MissingAudioStream "��� ������� ������ ����...  ->�����-���\n"
#define MSGTR_MissingVideoStreamBug "������ �� ������ ������!? ������������ �� ��� author, ������ �� ����� ��� bug :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: �� ������ ��� �������� �� ���������� ������ ���� � ������\n"

#define MSGTR_NI_Forced "Forced"
#define MSGTR_NI_Detected "�������"
#define MSGTR_NI_Message "%s NON-INTERLEAVED AVI format �������!\n"

#define MSGTR_UsingNINI "����� ���� NON-INTERLEAVED ��������� ������� ����� AVI!\n"
#define MSGTR_CouldntDetFNo "��� ������� �� ������������ � ������� ��� frames (��� ������� ���������)  \n"
#define MSGTR_CantSeekRawAVI "�� ������ ��������� �� raw .AVI streams! (�� index ����� ����������, ��������� �� ��� ������� -idx!)  \n"
#define MSGTR_CantSeekFile "������� � ��������� �� ���� �� ������!  \n"

#define MSGTR_EncryptedVOB "�������������� VOB ������ (� ��������� ����� ����� ��� libcss ����������)! �������� DOCS/cd-dvd.html\n"
#define MSGTR_EncryptedVOBauth "�������������� stream ���� ��� �������� �����������!!\n"

#define MSGTR_MOVcomprhdr "MOV: ����������� headers ��� �������������� (�����)!\n"
#define MSGTR_MOVvariableFourCC "MOV: �������������! ��������� FOURCC �������!?\n"
#define MSGTR_MOVtooManyTrk "MOV: �������������! �������� ����� tracks!"
#define MSGTR_MOVnotyetsupp "\n****** �� Quicktime MOV format ��� ������������� �����!!!!!!! *******\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "������� �� ������� ��� codec\n"
#define MSGTR_CantCloseCodec "������� �� �������� ��� codec\n"

#define MSGTR_MissingDLLcodec "������: ��� ����� ������ �� ������� ��� ����������� DirectShow codec: %s\n"
#define MSGTR_ACMiniterror "��� ����� ������ �� ��������/������������� �� Win32/ACM codec ���� (������ �� DLL ������?)\n"
#define MSGTR_MissingLAVCcodec "��� ����� ������ �� ������ �� '%s' ��� libavcodec...\n"

#define MSGTR_NoDShowSupport "�� MPlayer ��������������� ����� ���������� ��� directshow!\n"
#define MSGTR_NoWfvSupport "���������������� � ���������� ��� �� win32 codecs, � �� ��������� ��� ��-x86 ����������!\n"
#define MSGTR_NoDivx4Support "�� MPlayer ��������������� ����� ���������� ��� DivX4Linux (libdivxdecore.so)!\n"
#define MSGTR_NoLAVCsupport "�� MPlayer ��������������� ����� ���������� ��� ffmpeg/libavcodec!\n"
#define MSGTR_NoACMSupport "���������������� � ���������� ��� Win32/ACM codec ����, � �� ��������� ��� ��-x86 ���������� -> �����-��� :(\n"
#define MSGTR_NoDShowAudio "�� MPlayer ��������������� ����� ���������� ��� DirectShow -> �����-��� :(\n"
#define MSGTR_NoOggVorbis "�� OggVorbis codec ���� ����� ���������������� -> �����-��� :(\n"
#define MSGTR_NoXAnimSupport "�� MPlayer ��������������� ����� ���������� ��� XAnim!\n"

#define MSGTR_MpegPPhint "�������������! �������� postprocessing ������� ��� MPEG 1/2 ������,\n" \
			 "         ���� �� MPlayer ��������������� ����� ���������� ��� MPEG 1/2 postprocessing!\n" \
			 "         ������� �� ������ #define MPEG12_POSTPROC ��� config.h, ��� ����������������� �� libmpeg2!\n"
#define MSGTR_MpegNoSequHdr "MPEG: �������: ������� EOF ���� ��������� ��� ��������� ��� ������������\n"
#define MSGTR_CannotReadMpegSequHdr "�������: ��� ����� ������ �� ��������� � ��������� ��� ������������!\n"
#define MSGTR_CannotReadMpegSequHdrEx "�������: ��� ����� ������ �� ��������� � ��������� ��� ��������� ��� ������������!\n"
#define MSGTR_BadMpegSequHdr "MPEG: ���� ��������� ��� ������������!\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: ���� ��������� ��� ��������� ��� ������������!\n"

#define MSGTR_ShMemAllocFail "��� ������ �� ������������� �������������� �����\n"
#define MSGTR_CantAllocAudioBuf "��� ������ �� ������������� buffer ��� ����� ����\n"
#define MSGTR_NoMemForDecodedImage "��� ������� ������ ����� ��� ��� ����������������� ������ ���� buffer (%ld bytes)\n"

#define MSGTR_AC3notvalid "�� ������ AC3 ��� ����� ������.\n"
#define MSGTR_AC3only48k "�������������� ����� ���� �� ������� ��� 48000 Hz.\n"
#define MSGTR_UnknownAudio "�������/���� format ����, ����� ��� �����-���\n"

// LIRC:
#define MSGTR_SettingUpLIRC "������������ ����������� ��� lirc...\n"
#define MSGTR_LIRCdisabled "�������������� ��� ����������� ������ �����������\n"
#define MSGTR_LIRCopenfailed "�������� ���� ������������ ��� ����������� ��� lirc!\n"
#define MSGTR_LIRCsocketerr "������� �������� �� �� lirc socket: %s\n"
#define MSGTR_LIRCcfgerr "�������� ���� �� �������� ��� LIRC config ������� %s !\n"


// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "����"
#define MSGTR_FileSelect "������� ������� ..."
#define MSGTR_SubtitleSelect "������� ��������� ..."
#define MSGTR_OtherSelect "������� ..."
#define MSGTR_MessageBox "MessageBox"
#define MSGTR_PlayList "PlayList"
#define MSGTR_SkinBrowser "Skin �����"

// --- buttons ---
#define MSGTR_Ok "Ok"
#define MSGTR_Cancel "�����"
#define MSGTR_Add "��������"
#define MSGTR_Remove "��������"

// --- error messages ---
#define MSGTR_NEMDB "�������, ��� ������� ������ ����� ��� ������� ���� buffer."
#define MSGTR_NEMFMR "�������, ��� ������� ������ ����� ��� ��� �������� ��� �����."
#define MSGTR_NEMFMM "�������, ��� ������� ������ ����� ��� ��������� ��� ������ ��� ������ ���������."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[skin] error in skin config file on line %d: %s" 
#define MSGTR_SKIN_WARNING1 "[skin] warning in skin config file on line %d: widget found but before \"section\" not found ( %s )"
#define MSGTR_SKIN_WARNING2 "[skin] warning in skin config file on line %d: widget found but before \"subsection\" not found (%s)"
#define MSGTR_SKIN_BITMAP_16bit  "16 bits or less depth bitmap not supported ( %s ).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "�� ������ ( %s ) ��� �������\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "������ ���� ��� �������� ��� bmp ( %s )\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "������ ���� ��� �������� ��� tga ( %s )\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "������ ���� ��� �������� ��� png ( %s )\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "�� RLE packed tga ��� ������������� ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "�� ������������ ����� ������� ( %s )\n"
#define MSGTR_SKIN_BITMAP_ConvertError "������ ���� �� ��������� ��� 24 bit �� 32 bit ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "�� ������������ ���n��: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "��� ������� ������ ����� ���������\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "��������� ��������� ��������������\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "��� ������� ������ ��������������\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "��� ������� ������ ��� �������� ��������������\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "��-������� font identifier ( %s )\n"
#define MSGTR_SKIN_UnknownParameter "�� ����������� ���������� ( %s )\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[����� skin] ��� ������� ������ ����� ���������.\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "��� ������� skin ( %s ).\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "������ ��������� ��� skin configfile ( %s ).\n"
#define MSGTR_SKIN_LABEL "Skins:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "���� MPlayer"
#define MSGTR_MENU_Open "������� ..."
#define MSGTR_MENU_PlayFile "����������� ������� ..."
#define MSGTR_MENU_PlayVCD "����������� VCD ..."
#define MSGTR_MENU_PlayDVD "����������� DVD ..."
#define MSGTR_MENU_PlayURL "����������� URL ..."
#define MSGTR_MENU_LoadSubtitle "������� ��������� ..."
#define MSGTR_MENU_Playing "�����������..."
#define MSGTR_MENU_Play "�����������"
#define MSGTR_MENU_Pause "�����"
#define MSGTR_MENU_Stop "����"
#define MSGTR_MENU_NextStream "������� stream"
#define MSGTR_MENU_PrevStream "����������� stream"
#define MSGTR_MENU_Size "�������"
#define MSGTR_MENU_NormalSize "�������� �������"
#define MSGTR_MENU_DoubleSize "�������� �������"
#define MSGTR_MENU_FullScreen "������ �����"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_PlayDisc "����������� ������ ..."
#define MSGTR_MENU_ShowDVDMenu "�������� ��� DVD �����"
#define MSGTR_MENU_Titles "������"
#define MSGTR_MENU_Title "������ %2d"
#define MSGTR_MENU_None "(������)"
#define MSGTR_MENU_Chapters "��������"
#define MSGTR_MENU_Chapter "�������� %2d"
#define MSGTR_MENU_AudioLanguages "������� ����"
#define MSGTR_MENU_SubtitleLanguages "������� ���������"
#define MSGTR_MENU_PlayList "Playlist"
#define MSGTR_MENU_SkinBrowser "Skin �����"
#define MSGTR_MENU_Preferences "���������"
#define MSGTR_MENU_Exit "������ ..."

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "������� ������ ..."
#define MSGTR_MSGBOX_LABEL_Error "������ ..."
#define MSGTR_MSGBOX_LABEL_Warning "������������� ..."

#endif

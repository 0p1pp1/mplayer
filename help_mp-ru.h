/* Translated by:  Nick Kurshev <nickols_k@mail.ru>
   Was synced with help_mp-en.h: rev 1.16
 ========================= MPlayer help =========================== */

#ifdef HELP_MP_DEFINE_STATIC
static char* banner_text=
"\n\n"
"MPlayer " VERSION "(C) 2000-2001 Arpad Gereoffy (��. DOCS!)\n"
"\n";

static char help_text[]=
#ifdef HAVE_NEW_GUI
"������:   mplayer [-gui] [�����] [path/]filename\n"
#else
"������:   mplayer [�����] [path/]filename\n"
#endif
"\n"
"�����:\n"
" -vo <drv[:dev]> ����� �������� � ���������� ����� ������ (������ ��. � '-vo help')\n"
" -ao <drv[:dev]> ����� �������� � ���������� ����� ������ (������ ��. � '-ao help')\n"
" -vcd <����� �����> ������ VCD (video cd) ���� � ���������� ������ �����\n"
#ifdef HAVE_LIBCSS
" -dvdauth <dev>  ����� ���������� DVD ��� ����������� (��� ����������� ������)\n"
#endif
#ifdef USE_DVDREAD
" -dvd <����� �����> ������ DVD ����/���� � ���������� ������ �����\n"
#endif
" -ss <�����>     ������������� �� �������� (������� ��� ��:��:��) �������\n"
" -nosound        ��� �����\n"
#ifdef USE_FAKE_MONO
" -stereo <�����> ����� MPEG1 ������ ������ (0:������ 1:����� 2:������)\n"
#endif
" -channels <n>   ����� �������� ������� �����\n"
" -fs -vm -zoom   ����� �������������� ������������ (fullscr,vidmode chg,softw.scale)\n"
" -x <x> -y <y>   �������������� �������� � <x> * <y> ���������� [���� -vo ������� ������������!]\n"
" -sub <file>     ������� ���� ��������� (��. ����� -subfps, -subdelay)\n"
" -playlist <file> ������� playlist\n"
" -vid x -aid y   ����� ��� ������ ����� (x) � ����� (y) ������ ��� ������������\n"
" -fps x -srate y ����� ��� ��������� ����� (x ����/���) � ����� (y Hz) ��������\n"
" -pp <quality>   ��������� �������������� ������ (0-4 ��� DivX, 0-63 ��� mpegs)\n"
" -nobps          ������������ �������������� ����� ������������� A-V ��� AVI ������ (����� ������!)\n"
" -framedrop      ��������� ������ ������ (��� ��������� �����)\n"
"\n"
"�����:\n"
" <-  ��� ->      ����������� ������/����� �� 10 ������\n"
" up ��� down     ����������� ������/����� ��  1 ������\n"
" < ��� >         ����������� ������/����� � playlist'�\n"
" p ��� ������    ������������� ����� (����� ������� - ����������)\n"
" q ��� ESC       ���������� ��������������� � �����\n"
" + ��� -         ������������ �������� ����� �� +/- 0.1 �������\n"
" o               ��������� ������� OSD �������:  ��� / ��������� / ���������+������\n"
" * ��� /         ��������� ��� ������� ��������� (������� 'm' �������� master/pcm)\n"
" z ��� x         ������������ �������� ��������� �� +/- 0.1 �������\n"
"\n"
" * * * ��������� ��. ������������, � �������������� ������ � ������ ! * * *\n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// mplayer.c: 

#define MSGTR_Exiting "\n�������... (%s)\n"
#define MSGTR_Exit_frames "����������� ���������� ������ ���������"
#define MSGTR_Exit_quit "�����"
#define MSGTR_Exit_eof "����� �����"
#define MSGTR_Exit_error "��������� ������"
#define MSGTR_IntBySignal "\nMPlayer ������� �������� %d � ������: %s \n"
#define MSGTR_NoHomeDir "�� ���� ����� HOME �������\n"
#define MSGTR_GetpathProblem "�������� � get_path(\"config\")\n"
#define MSGTR_CreatingCfgFile "�������� ����� ������������: %s\n"
#define MSGTR_InvalidVOdriver "������������ ��� �������� ����� ������: %s\n��. '-vo help' ����� �������� ������ ��������� ���������.\n"
#define MSGTR_InvalidAOdriver "������������ ��� �������� ����� ������: %s\n��. '-ao help' ����� �������� ������ ��������� ���������.\n"
#define MSGTR_CopyCodecsConf "(���������� etc/codecs.conf (�� ���������� MPlayer) � ~/.mplayer/codecs.conf)\n"
#define MSGTR_CantLoadFont "�� ���� ��������� �����: %s\n"
#define MSGTR_CantLoadSub "�� ���� ��������� ��������: %s\n"
#define MSGTR_ErrorDVDkey "������ ��������� DVD �����.\n"
#define MSGTR_CmdlineDVDkey "���������� ������ DVD ������� ���������� ���� ��� ������������.\n"
#define MSGTR_DVDauthOk "����������� DVD �������� OK.\n"
#define MSGTR_DumpSelectedSteramMissing "dump: FATAL: ��������� ����� �������!\n"
#define MSGTR_CantOpenDumpfile "�� ���� ������� ���� �����!!!\n"
#define MSGTR_CoreDumped "core dumped :)\n"
#define MSGTR_FPSnotspecified "����/��� �� ������� (��� ������������) � ���������! ����������� -fps �����!\n"
#define MSGTR_NoVideoStream "����� ����� �� ������... ��� ���������������� ����\n"
#define MSGTR_TryForceAudioFmt "������� ����������� ��������� ����� ������� %d ...\n"
#define MSGTR_CantFindAfmtFallback "�� ���� ����� ����� ����� ��� �������������� ���������, ������� �� ������ ��������.\n"
#define MSGTR_CantFindAudioCodec "�� ���� ����� ����� ��� ����� ������� 0x%X !\n"
#define MSGTR_TryUpgradeCodecsConfOrRTFM "*** ����������� �������� %s �� etc/codecs.conf\n*** ���� �� ������� - ������� DOCS/codecs.html!\n"
#define MSGTR_CouldntInitAudioCodec "�� ���� ������������������� ����� �����! -> ��� �����\n"
#define MSGTR_TryForceVideoFmt "������� ����������� �������� ����� ������� %d ...\n"
#define MSGTR_CantFindVfmtFallback "�� ���� ����� ����� ����� ��� �������������� ���������, ������� �� ������ ��������.\n"
#define MSGTR_CantFindVideoCodec "�� ���� ����� ����� ��� ����� ������� 0x%X !\n"
#define MSGTR_VOincompCodec "Sorry, ��������� video_out ���������� �� ���������� � ���� �������.\n"
#define MSGTR_CouldntInitVideoCodec "FATAL: �� ���� ������������������� ����� ����� :(\n"
#define MSGTR_EncodeFileExists "���� ��� ����������: %s (�� ������������� ��� ������� AVI!)\n"
#define MSGTR_CantCreateEncodeFile "�� ���� ������� ���� ��� �����������\n"
#define MSGTR_CannotInitVO "FATAL: �� ���� ������������������� ����� �������!\n"
#define MSGTR_CannotInitAO "�� ���� �������/������������������� ����� ���������� -> ��� �����\n"
#define MSGTR_StartPlaying "������ ��������������...\n"
#define MSGTR_SystemTooSlow "\n\n"\
"         *****************************************************************\n"\
"         **** ���� ������� ������� �������� ����� ������������� ���!  ****\n"\
"         *****************************************************************\n"\
"!!! ��������� �������, ��������, ������: \n"\
"- �������� �����: ������/����� _�����_ �������. �����: ����������� -ao sdl ���\n"\
"  ����������� ALSA 0.5 ��� �������� oss �� ALSA 0.9. ������� DOCS/sound.html!\n"\
"- ��������� ����� �����. ����������� ������ -vo driver (������: -vo help) ���\n"\
"  ����������� � -framedrop ! ������� DOCS/video.html.\n"\
"- ��������� ���. �� ��������� ������������� ������� dvd/divx �� ���������\n"\
"  �����������! ����������� -hardframedrop\n"\
"- ����� ����. ����������� ��������� ����������: -nobps  -ni  -mc 0  -forceidx\n"\
"���� ������ �� �������, ����� ������� DOCS/bugreports.html !\n\n"

#define MSGTR_NoGui "MPlayer ��� ���������� ��� ��������� GUI!\n"
#define MSGTR_GuiNeedsX "MPlayer GUI ������� X11!\n"
#define MSGTR_Playing "������������ %s\n"
#define MSGTR_NoSound "�����: ��� �����!!!\n"
#define MSGTR_FPSforced "�����/��� ����������� � %5.3f (ftime: %5.3f)\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "CD-ROM '%s' �� ������!\n"
#define MSGTR_ErrTrackSelect "������ ������ ����� VCD!"
#define MSGTR_ReadSTDIN "������ �� stdin...\n"
#define MSGTR_UnableOpenURL "�� ���� ������� URL: %s\n"
#define MSGTR_ConnToServer "���������� � ��������: %s\n"
#define MSGTR_FileNotFound "���� �� ������: '%s'\n"

#define MSGTR_CantOpenDVD "�� ���� ������� DVD: %s\n"
#define MSGTR_DVDwait "������ ��������� �����, ��������� ����������...\n"
#define MSGTR_DVDnumTitles "���� %d ������ �� ���� DVD.\n"
#define MSGTR_DVDinvalidTitle "������������ ����� DVD �����: %d\n"
#define MSGTR_DVDnumChapters "���� %d ���� � ���� DVD �����.\n"
#define MSGTR_DVDinvalidChapter "������������ ����� DVD �����: %d\n"
#define MSGTR_DVDnumAngles "���� %d ����� � ���� DVD �����.\n"
#define MSGTR_DVDinvalidAngle "������������ ����� DVD ����: %d\n"
#define MSGTR_DVDnoIFO "�� ���� ������� IFO ���� ��� DVD ����� %d.\n"
#define MSGTR_DVDnoVOBs "�� ���� ������� ���� VOBS (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD ������� ������!\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "��������������! ��������� ����� ������ %d �������������!\n"
#define MSGTR_VideoStreamRedefined "��������������! ��������� ����� ������ %d �������������!\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: ������� ����� (%d � %d ������) ����� ������� � ������!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: ������� ����� (%d � %d ������) ����� ������� � ������!\n"
#define MSGTR_MaybeNI "(�������� �� ������������ �������������� �����/���� ��� ��������� �����)\n"
#define MSGTR_DetectedFLIfile "��������� FLI ������ �����!\n"
#define MSGTR_DetectedAVIfile "��������� AVI ������ �����!\n"
#define MSGTR_DetectedASFfile "��������� ASF ������ �����!\n"
#define MSGTR_DetectedMPEGPESfile "��������� MPEG-PES ������ �����!\n"
#define MSGTR_DetectedMPEGPSfile "��������� MPEG-PS ������ �����!\n"
#define MSGTR_DetectedMPEGESfile "��������� MPEG-ES ������ �����!\n"
#define MSGTR_DetectedQTMOVfile "��������� QuickTime/MOV ������ �����!\n"
#define MSGTR_MissingMpegVideo "MPEG ����� ����� �������!? ��������� � �������, ��� ����� ���� ����� :(\n"
#define MSGTR_InvalidMPEGES "������������ MPEG-ES �����??? ��������� � �������, ��� ����� ���� ����� :(\n"
#define MSGTR_FormatNotRecognized "========= Sorry, ������ ����� ����� �� ���������/�� �������������� ===========\n"\
				  "===== ���� ��� AVI, ASF ��� MPEG �����, ���������� ��������� � �������! ======\n"
#define MSGTR_MissingVideoStream "����� ����� �� ������!\n"
#define MSGTR_MissingAudioStream "����� ����� �� ������...  ->��� �����\n"
#define MSGTR_MissingVideoStreamBug "����� ����� �������!? ��������� � �������, ��� ����� ���� ����� :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: ���� �� �������� ��������� ����� ��� ����� �����\n"

#define MSGTR_NI_Forced "����������"
#define MSGTR_NI_Detected "���������"
#define MSGTR_NI_Message "%s �������������� ������ AVI �����!\n"

#define MSGTR_UsingNINI "������������� ��������������� ������������ ������� AVI �����!\n"
#define MSGTR_CouldntDetFNo "�� ���� ���������� ����� ������ (��� ����������� �����������)\n"
#define MSGTR_CantSeekRawAVI "�� ���� ������������� � ����� ������ .AVI! (��������� ������, ���������� � ������ -idx!)\n"
#define MSGTR_CantSeekFile "�� ���� ������������ � ���� �����!\n"

#define MSGTR_EncryptedVOB "����������� VOB ���� (�� ��������� � ���������� libcss)! ��. DOCS/cd-dvd.html\n"
#define MSGTR_EncryptedVOBauth "����������� ����� �� ����������� ���� �� ���� �����������!!\n"

#define MSGTR_MOVcomprhdr "MOV: ������ ��������� (����) �� ��������������!\n"
#define MSGTR_MOVvariableFourCC "MOV: ��������������! ��������� ���������� FOURCC!?\n"
#define MSGTR_MOVtooManyTrk "MOV: ��������������! ������� ����� ������!"
#define MSGTR_MOVnotyetsupp "\n****** Quicktime MOV ������ ���� �� ��������������!!!!!!! *******\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "�� ���� ������� �����\n"
#define MSGTR_CantCloseCodec "�� ���� ������� �����\n"

#define MSGTR_MissingDLLcodec "������: �� ���� ������� ����������� DirectShow �����: %s\n"
#define MSGTR_ACMiniterror "�� ���� ���������/������������������� Win32/ACM AUDIO ����� (������� DLL ����?)\n"
#define MSGTR_MissingLAVCcodec "�� ���� ����� ����� '%s' � libavcodec...\n"

#define MSGTR_NoDShowSupport "MPlayer ��� ��������� ��� ��������� directshow!\n"
#define MSGTR_NoWfvSupport "��������� ��� win32 ������� ��������� ��� ���������� �� ��-x86 ����������!\n"
#define MSGTR_NoDivx4Support "MPlayer ��� ��������� ��� ��������� DivX4Linux (libdivxdecore.so)!\n"
#define MSGTR_NoLAVCsupport "MPlayer ��� ��������� ��� ��������� ffmpeg/libavcodec!\n"
#define MSGTR_NoACMSupport "Win32/ACM ����� ����� ��������, ��� ���������� �� ��-x86 ��� -> ���������� ���� :(\n"
#define MSGTR_NoDShowAudio "��������� ��� ��������� DirectShow -> ���������� ���� :(\n"
#define MSGTR_NoOggVorbis "OggVorbis ����� ����� �������� -> ���������� ���� :(\n"
#define MSGTR_NoXAnimSupport "MPlayer ��� ��������� ��� ��������� XAnim!\n"

#define MSGTR_MpegPPhint "��������������! �� ��������� �������������� ��� MPEG 1/2 �����,\n" \
			 "         �� ���������� MPlayer ��� ��������� MPEG 1/2 ���������������!\n" \
			 "         #define MPEG12_POSTPROC � config.h, � ������������� libmpeg2!\n"
#define MSGTR_MpegNoSequHdr "MPEG: FATAL: ����� ����� ��� ������ ������������������ ����������\n"
#define MSGTR_CannotReadMpegSequHdr "FATAL: �� ���� ������ ������������������ ����������!\n"
#define MSGTR_CannotReadMpegSequHdrEx "FATAL: �� ���� ������ ���������� ������������������ ���������!\n"
#define MSGTR_BadMpegSequHdr "MPEG: ������ ������������������ ����������!\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: ������ ���������� ������������������ ���������!\n"

#define MSGTR_ShMemAllocFail "�� ���� ��������� ����� ������\n"
#define MSGTR_CantAllocAudioBuf "�� ���� ��������� �������� ������ �����\n"
#define MSGTR_NoMemForDecodedImage "�� ���������� ������ ��� ������� ������������� �������� (%ld ����)\n"

#define MSGTR_AC3notvalid "�� ���������� AC3 �����.\n"
#define MSGTR_AC3only48k "�������������� ������ 48000 Hz ������.\n"
#define MSGTR_UnknownAudio "�����������/���������� ����� ������, ����� �� �����\n"

// LIRC:
#define MSGTR_SettingUpLIRC "��������� ��������� lirc...\n"
#define MSGTR_LIRCdisabled "�� �� ������� ������������ ���� ��������� ����������\n"
#define MSGTR_LIRCopenfailed "��������� �������� ��������� lirc!\n"
#define MSGTR_LIRCsocketerr "���-�� ����������� � ������� lirc: %s\n"
#define MSGTR_LIRCcfgerr "��������� ������ ����� ������������ LIRC %s !\n"


// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "� ����"
#define MSGTR_FileSelect "������� ���� ..."
#define MSGTR_SubtitleSelect "������� �������� ..."
#define MSGTR_OtherSelect "����� ..."
#define MSGTR_MessageBox "���������"
#define MSGTR_PlayList "PlayList"
#define MSGTR_SkinBrowser "������������ ������"

// --- buttons ---
#define MSGTR_Ok "��"
#define MSGTR_Cancel "������"
#define MSGTR_Add "��������"
#define MSGTR_Remove "�������"

// --- error messages ---
#define MSGTR_NEMDB "Sorry, �� ������� ������ ��� ��������� �������."
#define MSGTR_NEMFMR "Sorry, �� ������� ������ ��� ����������� ����."
#define MSGTR_NEMFMM "Sorry, �� ������� ������ ��� ����� ����� �������� ����."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[skin] ������ � ����� ������������ ����� �� ����� %d: %s" 
#define MSGTR_SKIN_WARNING1 "[skin] ��������������: � ����� ������������ ����� �� ����� %d: widget ������ �� �� ����� �� ������� \"section\" ( %s )"
#define MSGTR_SKIN_WARNING2 "[skin] ��������������: � ����� ������������ ����� �� ����� %d: widget ������ �� �� ����� �� ������� \"subsection\" (%s)"
#define MSGTR_SKIN_BITMAP_16bit  "������� bitmap � 16 ��� � ������ �� �������������� ( %s ).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "���� �� ������ ( %s )\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "������ ������ bmp ( %s )\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "������ ������ tga ( %s )\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "������ ������ png ( %s )\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "RLE ����������� tga �� �������������� ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "����������� ��� ����� ( %s )\n"
#define MSGTR_SKIN_BITMAP_ConvertError "������ �������������� 24-��� � 32-��� ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "����������� ���������: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "�� ������� ������\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "��������� ������� ����� �������\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "���� ������ �� ������\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "���� ������� ������ �� ������\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "�������������� ������������� ������ ( %s )\n"
#define MSGTR_SKIN_UnknownParameter "����������� �������� ( %s )\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[skinbrowser] �� ������� ������.\n"

#endif

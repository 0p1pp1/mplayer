/* Translated by:  Nick Kurshev <nickols_k@mail.ru>,
 *		Dmitry Baryshkov <mitya@school.ioffe.ru>

   Reworked by Savchenko Andrew aka Bircoph <Bircoph[at]list[dot]ru>
   Was synced with help_mp-en.h: rev 1.157
 ========================= MPlayer help =========================== */

#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"������:   mplayer [�����] [URL|����/]���_�����\n"
"\n"
"������� �����: (������ ������ ��. �� man-��������)\n"
" -vo <drv[:dev]> ����� �������� � ���������� ����� ������ (������ ��. � '-vo help')\n"
" -ao <drv[:dev]> ����� �������� � ���������� ����� ������ (������ ��. � '-ao help')\n"
#ifdef HAVE_VCD
" vcd://<����� �����> ������ ������� (S)VCD (Super Video CD) (���������� ����������,\n                 �� ���������� ���)\n"
#endif
#ifdef USE_DVDREAD
" dvd://<����� ������> ������ DVD ����� � ���������� ������ �����\n"
" -alang/-slang   ������� ���� �����/��������� DVD (������������ ��� ������)\n"
#endif
" -ss <�����>     ������������� �� �������� (������� ��� ��:��:��) �������\n"
" -nosound        ��� �����\n"
" -fs             ����� �������������� ������������ (��� -vm, -zoom, ����������� �� man-��������)\n"
" -x <x> -y <y>   ���������� ���������� ������� (������������ � -vm ��� -zoom)\n"
" -sub <����>     ������� ���� ��������� (��. ����� -subfps, -subdelay)\n"
" -playlist <����> ������� ������ ��������������� (playlist)\n"
" -vid x -aid y   ����� ��� ������ ����� (x) � ����� (y) ������ ��� ������������\n"
" -fps x -srate y ����� ��� ��������� ����� (x ����/���) � ����� (y ��) ��������\n"
" -pp <quality>   ��������� ������ ������������� (����������� �� man-��������)\n"
" -framedrop      �������� ������������ ������ (��� ��������� �����)\n"
"\n"
"�������� ������: (������ ������ � �������� man, ����� ������ input.conf)\n"
" <- ��� ->       ����������� ���ң�/����� �� 10 ������\n"
" up ��� down     ����������� ���ң�/����� ��  1 ������\n"
" pgup or pgdown  ����������� ���ң�/����� �� 10 �����\n"
" < ��� >         ����������� ���ң�/����� � ������ ��������������� (playlist'�)\n"
" p ��� ������    ������������� ����� (����� ������� - ����������)\n"
" q ��� ESC       ���������� ��������������� � �����\n"
" + ��� -         ������������ �������� ����� �� +/- 0.1 �������\n"
" o               ��������� ������� OSD �������:  ��� / ��������� / ���������+������\n"
" * ��� /         ��������� ��� ������� PCM ���������\n"
" z ��� x         ������������ �������� ��������� �� +/- 0.1 �������\n"
" r ��� t         ����������� ������������ ������� ���������, ��. ����� -vf expand\n"
"\n"
" * * * ��������� ��. ������������, � �������������� ������ � ������! * * *\n"
"\n";
#endif

#define MSGTR_SamplesWanted "��� ��������� ��������� ���������� ������� ����� �������.\n����������, ��������� � ��������������.\n"

// ========================= MPlayer messages ===========================

// mplayer.c:

#define MSGTR_Exiting "\n�������...\n"
#define MSGTR_ExitingHow "\n�������... (%s)\n"
#define MSGTR_Exit_quit "�����"
#define MSGTR_Exit_eof "����� �����"
#define MSGTR_Exit_error "��������� ������"
#define MSGTR_IntBySignal "\nMPlayer ������� �������� %d � ������: %s \n"
#define MSGTR_NoHomeDir "�� ���� ����� HOME(��������) �������\n"
#define MSGTR_GetpathProblem "�������� � get_path(\"config\")\n"
#define MSGTR_CreatingCfgFile "�������� ����� ������������: %s\n"
#define MSGTR_CopyCodecsConf "(����������/��������_������ etc/codecs.conf (�� ���������� MPlayer) � ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "������������ ���������� codecs.conf.\n"
#define MSGTR_CantLoadFont "�� ���� ��������� �����: %s\n"
#define MSGTR_CantLoadSub "�� ���� ��������� ��������: %s\n"
#define MSGTR_DumpSelectedStreamMissing "����: ��������� ������: ��������� ����� �������!\n"
#define MSGTR_CantOpenDumpfile "�� ���� ������� ���� ��� ������������!!!\n"
#define MSGTR_CoreDumped "������ ���� ���� ;)\n"
#define MSGTR_FPSnotspecified "� ��������� �����/��� �� ������� (��� ������������)! ����������� -fps �����!\n"
#define MSGTR_TryForceAudioFmtStr "������� ����������� ��������� ����� ������� %s...\n"
#define MSGTR_CantFindAudioCodec "�� ���� ����� ����� ��� ����� ������� 0x%X!\n"
#define MSGTR_RTFMCodecs "�������� DOCS/HTML/ru/codecs.html!\n"
#define MSGTR_TryForceVideoFmtStr "������� ����������� ��������� ����� ������� %s...\n"
#define MSGTR_CantFindVideoCodec "�� ���� ����� ����� ��� ���������� -vo � ����� ������� 0x%X!\n"
#define MSGTR_CannotInitVO "��������� ������: �� ���� ���������������� ����� �������!\n"
#define MSGTR_CannotInitAO "�� ���� �������/���������������� ����� ���������� -> ��� �����\n"
#define MSGTR_StartPlaying "������ ��c������������...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"         *****************************************************************\n"\
"         **** ���� ������� ������� �������� ����� �������������� ���! ****\n"\
"         *****************************************************************\n"\
"��������� �������, ��������, ������: \n"\
"- �������� ������: ������/����� _�����_ �������\n"\
"  - ���������� -ao sdl ��� ����������� �������� OSS �� ALSA.\n"\
"  - ������������������� � ���������� ���������� -autosync, ������� � 30.\n"\
"- ��������� ����� �����\n"\
"  - ����������� ������ -vo driver (������: -vo help) ��� ����������� � -framedrop!\n"\
"- ��������� ���������\n"\
"  - �� ��������� �������������� ������� DVD/DivX �� ��������� �����������!\n  ����������� -hardframedrop\n"\
"- ����� ����.\n"\
"  - ����������� ��������� ����������: -nobps  -ni  -mc 0  -forceidx\n"\
"- ��������� �������� (�������������� NFS/SMB, DVD, VCD � �.�.)\n"\
"  - ����������� -cache 8192.\n"\
"- ����������� �� �� -cache ��� ������������ ��-'��ϣ���'[non-interleaved] AVI ������?\n"\
"  - ����������� -nocache.\n"\
"������� DOCS/HTML/ru/devices.html ��� ������� �� ����������/���������.\n"\
"���� ������ �� �������, ����� ������� DOCS/HTML/ru/bugreports.html!\n\n"

#define MSGTR_NoGui "MPlayer ��� ������������� ��� ��������� GUI!\n"
#define MSGTR_GuiNeedsX "MPlayer GUI ������� X11!\n"
#define MSGTR_Playing "������������ %s.\n"
#define MSGTR_NoSound "�����: ��� �����!!!\n"
#define MSGTR_FPSforced "�����/��� ����������� � %5.3f (����� �����: %5.3f).\n"
#define MSGTR_CompiledWithRuntimeDetection "�������������� ��� ����������� ���� ���������� �� ����� ���������� - �������������� - ��� \n�� ����������! ��� ��������� ������������ ������������������, ���������������� MPlayer\nc --disable-runtime-cpudetection.\n"
#define MSGTR_CompiledWithCPUExtensions "�������������� ��� x86 CPU �� ���������� ������������:"
#define MSGTR_AvailableVideoOutputDrivers "��������� �������� ������ �����:\n"
#define MSGTR_AvailableAudioOutputDrivers "��������� �������� ������ �����:\n"
#define MSGTR_AvailableAudioCodecs "��������� ����� ������:\n"
#define MSGTR_AvailableVideoCodecs "��������� ����� ������:\n"
#define MSGTR_AvailableAudioFm "��������� (����������������) ���������/�������� ����� �������:\n"
#define MSGTR_AvailableVideoFm "��������� (����������������) ���������/�������� ����� �������:\n"
#define MSGTR_AvailableFsType "��������� ������ ��������� �������������� ����:\n"
#define MSGTR_UsingRTCTiming "������������ ���������� Linux RTC ������������� (%ld��).\n"
#define MSGTR_CannotReadVideoProperties "�����: �� ���� ��������� ��������.\n"
#define MSGTR_NoStreamFound "����� �� ������.\n"
#define MSGTR_ErrorInitializingVODevice "������ ��� ��������/������������� ���������� ���������� ����� ������ (-vo).\n"
#define MSGTR_ForcedVideoCodec "���������� ����� �����: %s\n"
#define MSGTR_ForcedAudioCodec "���������� ����� �����: %s\n"
#define MSGTR_Video_NoVideo "�����: ��� �����\n"
#define MSGTR_NotInitializeVOPorVO "\n��������� ������: �� ���� ���������������� ����� ������� (-vf) ��� ����� ����� (-vo).\n"
#define MSGTR_Paused "\n====��������������====\r" // no more than 23 characters (status line for audio files)
#define MSGTR_PlaylistLoadUnable "\n�� ���� ��������� ������ ��������������� (playlist) %s.\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- MPlayer �������� ��-�� '������������ ����������'.\n"\
"  ��� ����� ���� ������� ������ ������ ���� ����������� ���� CPU �� ����� ����������...\n"\
"  ����������, ������� DOCS/HTML/ru/bugreports.html.\n"
#define MSGTR_Exit_SIGILL \
"- MPlayer �������� ��-�� '������������ ����������'.\n"\
"  ������, ��� ���������� ����� �� ��� ���������� �� CPU, �������� �� ����, ��� ��������\n"\
"  �� ��� �������������/�������������.\n"\
"  ��������� ���!\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- MPlayer �������� ��-�� ������� ������������� CPU/FPU/RAM.\n"\
"  ���������������� MPlayer � --enable-debug � �������� 'gdb' backtrace �\n"\
"  ������������������. ��� ������������, ��. DOCS/HTML/ru/bugreports_what.html#bugreports_crash\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer ��������. ��� �� ������ �����������.\n"\
"  ��� ����� ���� ������� � ���� MPlayer'� _���_ � ����� �������� _���_\n"\
"  ����� ������ gcc. ���� �� �������, ��� � ���� ������� MPlayer, ����������,\n"\
"  �������� DOCS/HTML/ru/bugreports.html � �������� ����������� ������. �� �� ������\n"\
"  � �� ����� ��������, ���� �� �� ������������ ��� ����������, ������� � ��������� ������.\n"
#define MSGTR_LoadingConfig "�������� ���������������� ���� '%s'\n"
#define MSGTR_AddedSubtitleFile "��������: �������� ���� ��������� (%d): %s\n"
#define MSGTR_ErrorOpeningOutputFile "������ �������� ����� [%s] ��� ������!\n"
#define MSGTR_CommandLine "��������� ������:"
#define MSGTR_RTCDeviceNotOpenable "�� ���� ������� %s: %s (������������ ������ �������� ������ ������ �� ���� ����).\n"
#define MSGTR_LinuxRTCInitErrorIrqpSet "������ ������������� Linux RTC � ioctl (rtc_irqp_set %lu): %s\n"
#define MSGTR_IncreaseRTCMaxUserFreq "���������� �������� \"echo %lu > /proc/sys/dev/rtc/max-user-freq\" \n� ����������� ������� ����� �������.\n"
#define MSGTR_LinuxRTCInitErrorPieOn "������ ������������� Linux RTC � ioctl (rtc_pie_on): %s\n"
#define MSGTR_UsingTimingType "������������ %s �������������.\n"
#define MSGTR_MenuInitialized "���� ����������������: %s\n"
#define MSGTR_MenuInitFailed "�� ���� ���������������� ����.\n"
#define MSGTR_Getch2InitializedTwice "��������������: getch2_init ������ ������!\n"
#define MSGTR_DumpstreamFdUnavailable "�� ���� ������� ���� ����� ������ - ��� ��������� 'fd' (�������� ����������).\n"
#define MSGTR_FallingBackOnPlaylist "�� ���� ��������� ��������� ������ ��������������� %s...\n"
#define MSGTR_CantOpenLibmenuFilterWithThisRootMenu "�� ���� ������� ����������� libmenu � ���� �������� ���� %s.\n"
#define MSGTR_AudioFilterChainPreinitError "������ � ������� pre-init ������������!\n"
#define MSGTR_LinuxRTCReadError "������ ������ Linux RTC: %s\n"
#define MSGTR_SoftsleepUnderflow "��������������! ����������� ������ �������� ����������� ��������!\n"
#define MSGTR_DvdnavNullEvent "������� DVDNAV NULL?!\n"
#define MSGTR_DvdnavHighlightEventBroken "������� DVDNAV: ������� ��������� �������\n"
#define MSGTR_DvdnavEvent "������� DVDNAV: %s\n"
#define MSGTR_DvdnavHighlightHide "������� DVDNAV: ��������� ������\n"
#define MSGTR_DvdnavStillFrame "######################################## ������� DVDNAV: ����-����: %d ���\n"
#define MSGTR_DvdnavNavStop "������� DVDNAV: ��������� Nav \n"
#define MSGTR_DvdnavNavNOP "������� DVDNAV: Nav NOP\n"
#define MSGTR_DvdnavNavSpuStreamChangeVerbose "������� DVDNAV: ��������� SPU-������ Nav: ���������: %d/%d/%d ���������: %d\n"
#define MSGTR_DvdnavNavSpuStreamChange "������� DVDNAV: ��������� SPU-������ Nav: ���������: %d ���������: %d\n"
#define MSGTR_DvdnavNavAudioStreamChange "������� DVDNAV: ��������� ����������� Nav: ���������: %d ���������: %d\n"
#define MSGTR_DvdnavNavVTSChange "������� DVDNAV: ��������� Nav VTS\n"
#define MSGTR_DvdnavNavCellChange "������� DVDNAV: ��������� ������ Nav\n"
#define MSGTR_DvdnavNavSpuClutChange "������� DVDNAV: ��������� Nav SPU CLUT\n"
#define MSGTR_DvdnavNavSeekDone "������� DVDNAV: ��������� ���������������� Nav\n"
#define MSGTR_MenuCall "����� ����\n"
#define MSGTR_EdlOutOfMem "�� ���� �������� ����������� ��ߣ� ������ ��� �������� ������ EDL.\n"
#define MSGTR_EdlRecordsNo "������� %d EDL ��������.\n"
#define MSGTR_EdlQueueEmpty "��� �������� EDL, ������� ������� ��������� (������� �����).\n"
#define MSGTR_EdlCantOpenForWrite "�� ���� ������� ���� EDL [%s] ��� ������.\n"
#define MSGTR_EdlCantOpenForRead "�� ���� ������� ���� EDL [%s] ��� ������.\n"
#define MSGTR_EdlNOsh_video "������ ������������ EDL ��� �����, ��������.\n"
#define MSGTR_EdlNOValidLine "�������� ������ EDL: %s\n"
#define MSGTR_EdlBadlyFormattedLine "����� ��������������� ������ EDL [%d]. ���������.\n"
#define MSGTR_EdlBadLineOverlap "��������� ������� �������� ���� [%f]; ��������� ��������� "\
"������� [%f]. ������ ������ ���� � ��������������� �������, �� ���� ���������. ���������.\n"
#define MSGTR_EdlBadLineBadStop "����� �������� ������ ���� ����� ������� ������.\n"

// mencoder.c:

#define MSGTR_UsingPass3ControlFile "��������� ��������� ���� ��� �������� 3-�� �������: %s\n"
#define MSGTR_MissingFilename "\n��������� ��� �����.\n\n"
#define MSGTR_CannotOpenFile_Device "�� ���� ������� ����/����������.\n"
#define MSGTR_CannotOpenDemuxer "�� ���� ������� �������� [demuxer].\n"
#define MSGTR_NoAudioEncoderSelected "\n���������� ����� (-oac) �� ������. �������� �����-������ (��. -oac help) ��� ����������� -nosound.\n"
#define MSGTR_NoVideoEncoderSelected "\n���������� ����� (-ovc) �� ������. �������� �����-������ (��. -ovc help).\n"
#define MSGTR_CannotOpenOutputFile "�� ���� ������� ���� ������ '%s'.\n"
#define MSGTR_EncoderOpenFailed "�� ���� ������� ����������.\n"
#define MSGTR_ForcingOutputFourcc "�������� fourcc ���������� � %x [%.4s]\n"
#define MSGTR_DuplicateFrames "\n%d �������������(���) ����(�/��)!\n"
#define MSGTR_SkipFrame "\n��������� ����!\n"
#define MSGTR_ErrorWritingFile "%s: ������ ��� ������ �����.\n"
#define MSGTR_RecommendedVideoBitrate "������������� �������� ��� %s CD: %d\n"
#define MSGTR_VideoStreamResult "\n����� �����: %8.3f ����/�  (%d B/s)  ������: %"PRIu64" ����(�/��)  %5.3f ���.  %d ����(�/��)\n"
#define MSGTR_AudioStreamResult "\n����� �����: %8.3f ����/�  (%d B/s)  ������: %"PRIu64" ����(�/��)  %5.3f ���.\n"
#define MSGTR_OpenedStream "�����: ������: %d  ������: 0x%X - 0x%x\n"
#define MSGTR_VCodecFramecopy "����������: ����������� ������ (%dx%d %dbpp fourcc=%x)\n"
#define MSGTR_ACodecFramecopy "����������: ����������� ������ (������=%x �������=%d ��������=%d �����=%d B/s=%d �������=%d)\n"
#define MSGTR_CBRPCMAudioSelected "������� CBR PCM �����\n"
#define MSGTR_MP3AudioSelected "������� MP3 �����\n"
#define MSGTR_CannotAllocateBytes "�� ���� �������� ������ ��� %d ����\n"
#define MSGTR_SettingAudioDelay "������������ ����� �������� � %5.3f\n"
#define MSGTR_SettingAudioInputGain "������������ �������� ��������� ����������� � %f\n"
#define MSGTR_LamePresetEquals "\npreset=%s\n\n"
#define MSGTR_LimitingAudioPreload "����������� ������������ ����� �� 0.4�\n"
#define MSGTR_IncreasingAudioDensity "���������� ��������� ����� �� 4\n"
#define MSGTR_ZeroingAudioPreloadAndMaxPtsCorrection "�������� ������������ ����� � 0, ������������ ��������� pts � 0\n"
#define MSGTR_CBRAudioByterate "\n\nCBR �����: %d ����/���, %d ����/����\n"
#define MSGTR_LameVersion "������ LAME %s (%s)\n\n"
#define MSGTR_InvalidBitrateForLamePreset "������: �������� �������� ��� ����������� �������� ��� ������ �������������\n"\
"\n"\
"��� ������������� ����� ������ �� ������ ������� �������� ����� \"8\" � \"320\"\n"\
"\n"\
"��� �������������� ���������� �����������: \"-lameopts preset=help\"\n"
#define MSGTR_InvalidLamePresetOptions "������: �� �� ������� ������ ������� �/��� ����� �������������\n"\
"\n"\
"��������� �������:\n"\
"\n"\
"   <fast>        standard\n"\
"   <fast>        extreme\n"\
"                 insane\n"\
"   <cbr> (����� ABR) - ��������������� ����� ABR. ��� �������������\n"\
"                       ������ ������� ��������. ��������:\n"\
"                       \"preset=185\" ���������� ��� ������������� (preset)\n"\
"                       � ���������� 185 ��� ������� �������� ����/���.\n"\
"\n"\
"    ��������� ��������:\n"\
"\n"\
"     \"-lameopts fast:preset=standard  \"\n"\
" ��� \"-lameopts  cbr:preset=192       \"\n"\
" ��� \"-lameopts      preset=172       \"\n"\
" ��� \"-lameopts      preset=extreme   \"\n"\
"\n"\
"��� �������������� ���������� �����������: \"-lameopts preset=help\"\n"
#define MSGTR_LamePresetsLongInfo "\n"\
"����� ������������� ����������� � ����� �������������� ����������� ���������� ��������.\n"\
"\n"\
"��� ���� ��������������� ����������� � ��������� � ������� ���������� ������\n"\
"�������� ������������� ��� �������� � ���������� ���� ����.\n"\
"\n"\
"����� ������������� ��������� ����������� ��� ����������� ��������� �����������,\n"\
"� ���������� ���� �� ������ �������� ����������� ��������� ��������, \n"\
"��������� �� ������� ������ ��� ������������� LAME.\n"\
"\n"\
"����� ������������ ��� �������������:\n"\
"\n"\
"   ��� VBR ������� (������ ������ ��������):\n"\
"\n"\
"     \"preset=standard\" ������ ���� ������������� ������ ���� ����������\n"\
"                             ��� ����������� ����� � ���������� ������, � ���\n"\
"                             ��� ������������� ���������� ������� ��������.\n"\
"\n"\
"     \"preset=extreme\" ���� �� ��������� ����������� ������� ������ �\n"\
"                             ��������������� �������������, ��� �������������,\n"\
"                             ��� �������, ����������� ��������� ������ ��������,\n"\
"                             ��� ����� \"standard\".\n"\
"\n"\
"   ��� CBR 320kbps (����������� ��������� ��������, ����������\n                             ��� ������������� ������ �������������):\n"\
"\n"\
"     \"preset=insane\"  ������������� ���� ��������� �������� ��������� ���\n"\
"                             ����������� ����� � ����������� ��������, �� ����\n"\
"                             ��� ���������� ����������� ��������� ��������,\n"\
"                             �������� �� ������ ����� - ��� ������ ������� ���.\n"\
"\n"\
"   ��� ABR ������� (������� �������� ��� ��������� ���������, �� �� ����� �������, ��� VBR):\n"\
"\n"\
"     \"preset=<kbps>\"  �������������� ���� ������������� ������ ���� �������\n"\
"                             �������� ��� ��������� ���������. ����������� ��\n"\
"                             ���ģ���� ���������, ��� ������������� ���������\n"\
"                             ����������� ��������� ��� ������ ���������� ��������.\n"\
"                             �������� �� ��, ��� ���� ������ ��������, �� ������\n"\
"                             �� ����� ������ ��� VBR � ������ �� ���������\n"\
"                             ������ �� ������ �������� ��� VBR �� ������� ����������.\n"\
"\n"\
"����� �������� ��������� ����� ��� �������������� ��������:\n"\
"\n"\
"   <fast>        standard\n"\
"   <fast>        extreme\n"\
"                 insane\n"\
"   <cbr> (����� ABR) - ��������������� ����� ABR. ��� �������������\n"\
"                       ������ ������� ��������. ��������:\n"\
"                       \"preset=185\" ���������� ��� ������������� (preset)\n"\
"                       � ���������� 185 ��� ������� �������� ����/���.\n"\
"\n"\
"   \"fast\" - �������� ����� ������� VBR ��� ����������� �������.\n"\
"            ����������� ����� ����� �������� ��, ��� �����\n"\
"            �������� ����� ������� ������, ��� � ���������� ������;\n"\
"            ����� �������� ����� ���� ��������� ����.\n"\
"��������������: � ������� ������ ������� ������������� ����� �������� � �������\n"\
"                �������� ���������, �� ��������� � �������� ���������������.\n"\
"\n"\
"   \"cbr\"  - ���� �� ����������� ����� ABR (��. ����) � ����� \"�������\""\
"            ���������� ��� 80, 96, 112, 128, 160, 192, 224, 256, 320,\n"\
"            �� ������ ������������ ����� \"cbr\" ��� ������������ �����������\n"\
"            � ������ CBR ������ ������������ abr ������. ABR �������������\n"\
"            ����� ������� ��������, �� CBR ����� ���� �������� � �����\n"\
"            ��������� ��� �������� ������� mp3 ����� ��������.\n"\
"\n"\
"    ��������:\n"\
"\n"\
"     \"-lameopts fast:preset=standard  \"\n"\
" ��� \"-lameopts  cbr:preset=192       \"\n"\
" ��� \"-lameopts      preset=172       \"\n"\
" ��� \"-lameopts      preset=extreme   \"\n"\
"\n"\
"\n"\
"��������� ����������� �������� ��� ������ ABR:\n"\
"phone => 16kbps/mono        phon+/lw/mw-eu/sw => 24kbps/mono\n"\
"mw-us => 40kbps/mono        voice => 56kbps/mono\n"\
"fm/radio/tape => 112kbps    hifi => 160kbps\n"\
"cd => 192kbps               studio => 256kbps"
#define MSGTR_ConfigFileError "������ � ���������������� �����"
#define MSGTR_ErrorParsingCommandLine "������ ��� ������� ���������� ��������� ������"
#define MSGTR_VideoStreamRequired "������� ����������� �����������!\n"
#define MSGTR_ForcingInputFPS "������� �����/��� ����� �������� �� %5.2f\n"
#define MSGTR_RawvideoDoesNotSupportAudio "�������� ������ ����� RAWVIDEO �� ������������ ����� - �������� �����\n"
#define MSGTR_DemuxerDoesntSupportNosound "���� �������� [demuxer] ���� ��� �� ������������ -nosound.\n"
#define MSGTR_MemAllocFailed "�� ���� �������� ������"
#define MSGTR_NoMatchingFilter "�� ���� ����� ��������������� ������/ao ������!\n"
#define MSGTR_MP3WaveFormatSizeNot30 "sizeof(MPEGLAYER3WAVEFORMAT)==%d!=30, ��������, ��������� ���������� C?\n"
#define MSGTR_NoLavcAudioCodecName "����� LAVC, ��������� ��� ������!\n"
#define MSGTR_LavcAudioCodecNotFound "����� LAVC, �� ���� ����� ���������� ��� ������ %s\n"
#define MSGTR_CouldntAllocateLavcContext "����� LAVC, �� ���� ���������� ��������!\n"
#define MSGTR_CouldntOpenCodec "�� ���� ������� ����� %s, br=%d\n"

// cfg-mencoder.h:

#define MSGTR_MEncoderMP3LameHelp "\n\n"\
" vbr=<0-4>     ����� ��� ����������� � ���������� ����������\n"\
"                0: cbr\n"\
"                1: mt\n"\
"                2: rh(�� ���������)\n"\
"                3: abr\n"\
"                4: mtrh\n"\
"\n"\
" abr           �����Σ���� ��������\n"\
"\n"\
" cbr           ���������� ��������\n"\
"               ����� ��������� CBR ����� ����������� � ��������� ����������������� ABR �������\n"\
"\n"\
" br=<0-1024>   ������� �������� � ���� (������ CBR � ABR)\n"\
"\n"\
" q=<0-9>       �������� (0-������, 9-����������) (������ ��� VBR)\n"\
"\n"\
" aq=<0-9>      �������� ��������� (0-������/����� ���������, 9-������/����������)\n"\
"\n"\
" ratio=<1-100> ����������� ������\n"\
"\n"\
" vol=<0-10>    ���������� �������� ��������� �����\n"\
"\n"\
" mode=<0-3>    (��-���������: ���������������)\n"\
"                0: ������\n"\
"                1: ������Σ���� ������ [joint-stereo]\n"\
"                2: �������������\n"\
"                3: ����\n"\
"\n"\
" padding=<0-2>\n"\
"                0: ���\n"\
"                1: ���\n"\
"                2: ������������\n"\
"\n"\
" fast          ������������ �� ������� ����������� � ��������� ����������������� VBR\n"\
"               �������; ������������� ��������� ������ �������� � ���������� ���������.\n"\
"\n"\
" preset=<value> ������������� ��������� ����������� ���������� ��������.\n"\
"                 medium: VBR  �����������, ������� ��������\n"\
"                 (��������� ��������� - 150-180 kbps)\n"\
"                 standard: VBR �����������, ������� ��������\n"\
"                 (��������� ��������� - 170-210 kbps)\n"\
"                 extreme: VBR �����������, ����� ������� ��������\n"\
"                 (��������� ��������� - 200-240 kbps)\n"\
"                 insane:  CBR �����������, ������ ����������������� ��������\n"\
"                 (�������� 320 kbps)\n"\
"                 <8-320>: ABR ����������� � �������� � ���� ������� ����������.\n\n"

//codec-cfg.c:
#define MSGTR_DuplicateFourcc "������������� FourCC"
#define MSGTR_TooManyFourccs "������� ����� FourCCs/��������..."
#define MSGTR_ParseError "������ ������� ����������"
#define MSGTR_ParseErrorFIDNotNumber "������ ������� ���������� (ID ������� �� �����?)"
#define MSGTR_ParseErrorFIDAliasNotNumber "������ ������� ���������� (��������� ID ������� �� �����?)"
#define MSGTR_DuplicateFID "������������� ID �������"
#define MSGTR_TooManyOut "������� ����� �������� ��������..."
#define MSGTR_InvalidCodecName "\n��� ������ '%s' �� �����!\n"
#define MSGTR_CodecLacksFourcc "\n����� '%s' �� ����� FourCC/������!\n"
#define MSGTR_CodecLacksDriver "\n����� '%s' �� ����� ��������!\n"
#define MSGTR_CodecNeedsDLL "\n������ '%s' ���������� 'dll'!\n"
#define MSGTR_CodecNeedsOutfmt "\n������ '%s' ��������� 'outfmt'!\n"
#define MSGTR_CantAllocateComment "�� ���� �������� ������ ��� �����������. "
#define MSGTR_GetTokenMaxNotLessThanMAX_NR_TOKEN "get_token(): max >= MAX_MR_TOKEN!"
#define MSGTR_ReadingFile "����� '%s': "
#define MSGTR_CantOpenFileError "�� ���� ������� '%s': %s\n"
#define MSGTR_CantGetMemoryForLine "�� ���� �������� ������ ��� ������: %s\n"
#define MSGTR_CantReallocCodecsp "�� ���� ��������� realloc ��� '*codecsp': %s\n"
#define MSGTR_CodecNameNotUnique "��� ������ '%s' �� ���������."
#define MSGTR_CantStrdupName "�� ���� ��������� strdup -> 'name': %s\n"
#define MSGTR_CantStrdupInfo "�� ���� ��������� strdup -> 'info': %s\n"
#define MSGTR_CantStrdupDriver "�� ���� ��������� strdup -> 'driver': %s\n"
#define MSGTR_CantStrdupDLL "�� ���� ��������� strdup -> 'dll': %s"
#define MSGTR_AudioVideoCodecTotals "%d ����� & %d ����� �������\n"
#define MSGTR_CodecDefinitionIncorrect "����� ������̣� �����������."
#define MSGTR_OutdatedCodecsConf "���� codecs.conf ������� ���� � ����������� � ������ ������� MPlayer'�!"

// divx4_vbr.c:
#define MSGTR_OutOfMemory "�������� ������"
#define MSGTR_OverridingTooLowBitrate "��������� �������� ������� ������ ��� ������� �����.\n"\
"���������� ��������� �������� ��� ����� ���������� %.0f ����/���. �������������\n"\
"�������� ������������� ��������.\n"

// fifo.c
#define MSGTR_CannotMakePipe "�� ���� ������� �����!\n"

// m_config.c
#define MSGTR_SaveSlotTooOld "������ ������� ������ ���� ���������� � lvl %d: %d !!!\n"
#define MSGTR_InvalidCfgfileOption "����� %s �� ����� �������������� � ���������������� �����.\n"
#define MSGTR_InvalidCmdlineOption "����� %s �� ����� �������������� � ��������� ������.\n"
#define MSGTR_InvalidSuboption "������: � ����� '%s' ��� �������� '%s'.\n"
#define MSGTR_MissingSuboptionParameter "������: � �������� '%s' ����� '%s' ������ ���� ��������!\n"
#define MSGTR_MissingOptionParameter "������: � ����� '%s' ������ ���� ��������!\n"
#define MSGTR_OptionListHeader "\n ���                  ���             �������    �������� �����   CL    Cfg\n\n"
#define MSGTR_TotalOptions "\n�����: %d �����(�/�)\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "CD-ROM '%s' �� ������!\n"
#define MSGTR_ErrTrackSelect "������ ������ ������� VCD!"
#define MSGTR_ReadSTDIN "������ �� stdin (�� ������������ �����)...\n"
#define MSGTR_UnableOpenURL "�� ���� ������� URL: %s\n"
#define MSGTR_ConnToServer "���������� � ��������: %s\n"
#define MSGTR_FileNotFound "���� �� ������: '%s'\n"

#define MSGTR_SMBInitError "�� ���� ���������������� ���������� libsmbclient: %d\n"
#define MSGTR_SMBFileNotFound "�� ���� ������� �� ����: '%s'\n"
#define MSGTR_SMBNotCompiled "MPlayer �� ��� ������������� � ���������� ������ SMB.\n"

#define MSGTR_CantOpenDVD "�� ���� ������� DVD: %s\n"
#define MSGTR_DVDwait "������ ��������� �����, ���������, ����������...\n"
#define MSGTR_DVDnumTitles "�� ���� DVD %d �������.\n"
#define MSGTR_DVDinvalidTitle "������������ ����� DVD ������: %d\n"
#define MSGTR_DVDnumChapters "� ���� DVD ������ %d ������[�/��].\n"
#define MSGTR_DVDinvalidChapter "������������ ����� DVD �����: %d\n"
#define MSGTR_DVDnumAngles "� ���� DVD ������ %d �����.\n"
#define MSGTR_DVDinvalidAngle "������������ ����� DVD ����: %d\n"
#define MSGTR_DVDnoIFO "�� ���� ������� IFO ���� ��� DVD ������ %d.\n"
#define MSGTR_DVDnoVOBs "�� ���� ������� VOBS ������ (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD ������� ������!\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "��������������: ��������� ����� ������ %d ����������̣�!\n"
#define MSGTR_VideoStreamRedefined "��������������: ��������� ����� ������ %d ����������̣�!\n"
#define MSGTR_TooManyAudioInBuffer "\n������� ����� (%d � %d ������) ����� ������� � ������!\n"
#define MSGTR_TooManyVideoInBuffer "\n������� ����� (%d � %d ������) ����� ������� � ������!\n"
#define MSGTR_MaybeNI "�������� �� ������������ '����ϣ���' �����/���� ��� ��������� �����?\n" \
                     "��� AVI ������ ���������� ����������� '����ϣ���' ����� ������ -ni.\n"
#define MSGTR_SwitchToNi "\n��������� ����� '��ϣ���' AVI ���� - ������������ � -ni �����...\n"
#define MSGTR_Detected_XXX_FileFormat "��������� %s ������ �����!\n"
#define MSGTR_DetectedAudiofile "��������� ����� ����.\n"
#define MSGTR_NotSystemStream "�� MPEG System Stream ������... (�������� Transport Stream?)\n"
#define MSGTR_InvalidMPEGES "������������ MPEG-ES �����??? ��������� � �������, ��� ����� ���� ����� :(\n"
#define MSGTR_FormatNotRecognized "======= ��������, ������ ����� ����� �� ���������/�� �������������� ==========\n"\
				  "===== ���� ��� AVI, ASF ��� MPEG �����, ���������� ��������� � �������! ======\n"
#define MSGTR_MissingVideoStream "����� ����� �� ������!\n"
#define MSGTR_MissingAudioStream "����� ����� �� ������ -> ��� �����\n"
#define MSGTR_MissingVideoStreamBug "����� ����� �������!? ��������� � �������, ��� ����� ���� ����� :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: � ����� ��� ���������� ����� ��� ����� ������\n"

#define MSGTR_NI_Forced "����������"
#define MSGTR_NI_Detected "���������"
#define MSGTR_NI_Message "%s '��������' ������ AVI �����!\n"

#define MSGTR_UsingNINI "������������� '���������' ������������ ������� AVI �����!\n"
#define MSGTR_CouldntDetFNo "�� ���� ���������� ����� ������ (��� ����������� �����������).\n"
#define MSGTR_CantSeekRawAVI "�� ���� ������������� � ����� ������ AVI! (��������� ������, ���������� � ������ -idx!)\n"
#define MSGTR_CantSeekFile "�� ���� ������������ � ���� �����!\n"

#define MSGTR_EncryptedVOB "����������� VOB ����! ��. DOCS/HTML/ru/dvd.html\n"

#define MSGTR_MOVcomprhdr "MOV: ��� ��������� ������ ���������� ��������� zlib!\n"
#define MSGTR_MOVvariableFourCC "MOV: ��������������! ��������� ���������� FOURCC!?\n"
#define MSGTR_MOVtooManyTrk "MOV: ��������������! ������� ����� ������!"
#define MSGTR_FoundAudioStream "==> ��ۣ� ����� �����: %d\n"
#define MSGTR_FoundVideoStream "==> ��ۣ� ����� �����: %d\n"
#define MSGTR_DetectedTV "������ TV! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "�� ���� ������� ogg �������� [demuxer].\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: ��� ���������� (id:%d).\n"
#define MSGTR_CannotOpenAudioStream "�� ���� ������� ����������: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "�� ���� ������� ����� ���������: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "�� ���� ������� �������� [demuxer] �����: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "�� ���� ������� �������� [demuxer] ���������: %s\n"
#define MSGTR_TVInputNotSeekable "�� TV ����� ������ ������������! (�������� ����������� ����� ��� ����� ������� ;)\n"
#define MSGTR_DemuxerInfoAlreadyPresent "���������� ��������� [demuxer] %s ��� ����������!\n"
#define MSGTR_ClipInfo "���������� � �����:\n"

#define MSGTR_LeaveTelecineMode "\ndemux_mpg: ���������� 30 ������/��� NTSC ����������, ���������� ������� ������.\n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: ���������� 24 �����/��� �������������� [progressive] NTSC ����������,\n���������� ������� ������.\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "�� ���� ������� �����.\n"
#define MSGTR_CantCloseCodec "�� ���� ������� �����.\n"

#define MSGTR_MissingDLLcodec "������: �� ���� ������� ����������� DirectShow �����: %s\n"
#define MSGTR_ACMiniterror "�� ���� ���������/���������������� Win32/ACM AUDIO ����� (������� DLL ����?)\n"
#define MSGTR_MissingLAVCcodec "�� ���� ����� ����� '%s' � libavcodec...\n"

#define MSGTR_MpegNoSequHdr "MPEG: ��������� ������: ����� ����� ��� ������ ������������������ ����������\n"
#define MSGTR_CannotReadMpegSequHdr "��������� ������: �� ���� ������� ������������������ ����������!\n"
#define MSGTR_CannotReadMpegSequHdrEx "��������� ������: �� ���� ������� ���������� ������������������ ����������!\n"
#define MSGTR_BadMpegSequHdr "MPEG: ������ ������������������ ����������!\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: ������ ���������� ������������������ ����������!\n"

#define MSGTR_ShMemAllocFail "�� ���� ��������������� ����������� ������.\n"
#define MSGTR_CantAllocAudioBuf "�� ���� ��������������� �������� ����� �����.\n"

#define MSGTR_UnknownAudio "�����������/���������� ����� ������ -> ��� �����\n"

#define MSGTR_UsingExternalPP "[PP] ��������� ������� ������ �������������, max q = %d.\n"
#define MSGTR_UsingCodecPP "[PP] ��������� ������������� �� ������, max q = %d.\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "����� ������� '%s' �� �������������� ���������� vo � vd.\n"
#define MSGTR_VideoCodecFamilyNotAvailableStr "����������� ��������� ������������ [%s] (vfm=%s) �� ��������.\n�������� ��� �� ����� ����������.\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "����������� ��������� ������������ [%s] (afm=%s) �� ��������.\n�������� ��� �� ����� ����������.\n"
#define MSGTR_OpeningVideoDecoder "�������� ������� �����: [%s] %s\n"
#define MSGTR_OpeningAudioDecoder "�������� ������� �����: [%s] %s\n"
#define MSGTR_UninitVideoStr "��������������� �����: %s\n"
#define MSGTR_UninitAudioStr "��������������� �����: %s\n"
#define MSGTR_VDecoderInitFailed "������ ������������� �������� ����� :(\n"
#define MSGTR_ADecoderInitFailed "������ ������������� �������� ����� :(\n"
#define MSGTR_ADecoderPreinitFailed "������ ���������������� �������� ����� :(\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: ���������� %d ����(�/��) ��� �������� ������.\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: ���������� %d + %d = %d ����(�/��) ��� ������ ������.\n"

// LIRC:
#define MSGTR_SettingUpLIRC "��������� ��������� LIRC...\n"
#define MSGTR_LIRCdisabled "�� �� ������� ������������ ��� ����� ����������\n"
#define MSGTR_LIRCopenfailed "��������� �������� ��������� LIRC!\n"
#define MSGTR_LIRCcfgerr "��������� ������� ������ ����� ������������ LIRC '%s'!\n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "�� ���� ����� ����� ������ '%s'.\n"
#define MSGTR_CouldNotOpenVideoFilter "�� ���� ������� ����� ������ '%s'.\n"
#define MSGTR_OpeningVideoFilter "�������� ����� ������: "
#define MSGTR_CannotFindColorspace "�� ���� ����� ���������� �������� ������������, ���� ������� 'scale' :(\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDec: ����� �� ��������� sh->disp_w � sh->disp_h, ������� ������.\n"
#define MSGTR_VoConfigRequest "VDec: ������ vo config - %d x %d (�������������� csp: %s)\n"
#define MSGTR_CouldNotFindColorspace "�� ���� ����� ���������� �������� ������������ - ��������� � -vf scale...\n"
#define MSGTR_MovieAspectIsSet "Movie-Aspect - %.2f:1 - �������������� ��� ��������� ����������� ������ ������.\n"
#define MSGTR_MovieAspectUndefined "Movie-Aspect �� ������̣� - ������������������ �� �����������.\n"

// vd_dshow.c, vd_dmo.c
#define MSGTR_DownloadCodecPackage "��� ����� ��������/���������� ����� �������� �������.\n������� �� http://www.mplayerhq.hu/dload.html\n"
#define MSGTR_DShowInitOK "����������: Win32/DShow ����� ����� ������� ���������������.\n"
#define MSGTR_DMOInitOK "����������: Win32/DMO ����� ����� ������� ���������������.\n"

// x11_common.c
#define MSGTR_EwmhFullscreenStateFailed "\nX11: �� ���� ������� ������� EWMH fullscreen!\n"

#define MSGTR_InsertingAfVolume "[������] ��� ����������� ������������, �������� ������ ���������.\n"
#define MSGTR_NoVolume "[������] �������� ��������� �� ��������.\n"

// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "� ����"
#define MSGTR_FileSelect "������� ����..."
#define MSGTR_SubtitleSelect "������� ��������..."
#define MSGTR_OtherSelect "�����..."
#define MSGTR_AudioFileSelect "����� �������� ����� ������..."
#define MSGTR_FontSelect "����� ������..."
#define MSGTR_PlayList "��������"
#define MSGTR_Equalizer "����������"
#define MSGTR_ConfigureEqualizer "��������� �������"
#define MSGTR_SkinBrowser "����������� ������"
#define MSGTR_Network "������� ������..."
#define MSGTR_Preferences "���������"
#define MSGTR_AudioPreferences "������������ ����� ��������"
#define MSGTR_NoMediaOpened "�������� �� ������."
#define MSGTR_VCDTrack "VCD ������� %d"
#define MSGTR_NoChapter "��� �����"
#define MSGTR_Chapter "����� %d"
#define MSGTR_NoFileLoaded "���� �� ��������."

// --- buttons ---
#define MSGTR_Ok "��"
#define MSGTR_Cancel "������"
#define MSGTR_Add "��������"
#define MSGTR_Remove "�������"
#define MSGTR_Clear "��������"
#define MSGTR_Config "���������������"
#define MSGTR_ConfigDriver "��������������� �������"
#define MSGTR_Browse "�����������"

// --- error messages ---
#define MSGTR_NEMDB "��������, �� ������� ������ ��� ������ ����������."
#define MSGTR_NEMFMR "��������, �� ������� ������ ��� ����������� ����."
#define MSGTR_IDFGCVD "��������, �� ��ۣ� ����������� � GUI ������� ����� ������."
#define MSGTR_NEEDLAVCFAME "��������, �� �� ������ ����������� ��-MPEG ����� �� ����� DXR3/H+ ����������\n��� ���������������. ����������, �������� lavc ��� fame ��� ������������ DXR3/H+."
#define MSGTR_UNKNOWNWINDOWTYPE "������ ����������� ��� ����..."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[�����] ������ � ����� ������������ ����� �� ������ %d: %s" 
#define MSGTR_SKIN_WARNING1 "[�����] ��������������: � ����� ������������ ����� �� ������ %d:\n������� GUI ������, �� �� ����� �� ������� \"section\" (%s)"
#define MSGTR_SKIN_WARNING2 "[�����] ��������������: � ����� ������������ ����� �� ������ %d:\n������� GUI ������, �� �� ����� �� ������� \"subsection\" (%s)"
#define MSGTR_SKIN_WARNING3 "[�����] ��������������: � ����� ������������ ����� �� ������ %d:\n��� ��������� �� �������������� ���� ��������� GUI (%s)"
#define MSGTR_SKIN_SkinFileNotFound "[�����] ���� '%s' �� ������.\n"
#define MSGTR_SKIN_BITMAP_16bit  "������� ������� ������� � 16 ��� � ������ �� �������������� (%s).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "���� �� ������ (%s)\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "������ ������ BMP (%s)\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "������ ������ TGA (%s)\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "������ ������ PNG (%s)\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "RLE ����������� TGA �� �������������� (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "����������� ��� ����� (%s)\n"
#define MSGTR_SKIN_BITMAP_ConversionError "������ �������������� 24-��� � 32-��� (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "����������� ���������: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "�� ������� ������\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "��������� ������� ����� �������.\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "���� ������ �� ������.\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "���� ������� ������ �� ������.\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "�������������� ������������� ������ (%s)\n"
#define MSGTR_SKIN_UnknownParameter "����������� �������� (%s)\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "����� �� ������� (%s).\n"
#define MSGTR_SKIN_SKINCFG_SelectedSkinNotFound "��������� ����� '%s' �� �������, ������ 'default'...\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "������ ������ ���� ������������ ���� (%s)\n"
#define MSGTR_SKIN_LABEL "�����:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "� MPlayer"
#define MSGTR_MENU_Open "�������..."
#define MSGTR_MENU_PlayFile "������ ����..."
#define MSGTR_MENU_PlayVCD "������ VCD..."
#define MSGTR_MENU_PlayDVD "������ DVD..."
#define MSGTR_MENU_PlayURL "������ URL..."
#define MSGTR_MENU_LoadSubtitle "��������� ��������..."
#define MSGTR_MENU_DropSubtitle "������ ��������..."
#define MSGTR_MENU_LoadExternAudioFile "��������� ������� ����� ����..."
#define MSGTR_MENU_Playing "���������������"
#define MSGTR_MENU_Play "������"
#define MSGTR_MENU_Pause "�����"
#define MSGTR_MENU_Stop "�������"
#define MSGTR_MENU_NextStream "����. �����"
#define MSGTR_MENU_PrevStream "����. �����"
#define MSGTR_MENU_Size "������"
#define MSGTR_MENU_HalfSize   "���������� ������"
#define MSGTR_MENU_NormalSize "���������� ������"
#define MSGTR_MENU_DoubleSize "������� ������"
#define MSGTR_MENU_FullScreen "������ �����"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "������ ����..."
#define MSGTR_MENU_ShowDVDMenu "�������� DVD ����"
#define MSGTR_MENU_Titles "������"
#define MSGTR_MENU_Title "����� %2d"
#define MSGTR_MENU_None "(���)"
#define MSGTR_MENU_Chapters "�����"
#define MSGTR_MENU_Chapter "����� %2d"
#define MSGTR_MENU_AudioLanguages "����� �����"
#define MSGTR_MENU_SubtitleLanguages "���� ���������"
// TODO: Why is this different from MSGTR_PlayList?
#define MSGTR_MENU_PlayList "������ ���������������"
#define MSGTR_MENU_SkinBrowser "����������� ����"
#define MSGTR_MENU_Exit "�����..."
#define MSGTR_MENU_Mute "��������� ����"
#define MSGTR_MENU_Original "��������"
#define MSGTR_MENU_AspectRatio "����������� ��������"
#define MSGTR_MENU_AudioTrack "����� �������"
#define MSGTR_MENU_Track "������� %d"
#define MSGTR_MENU_VideoTrack "����� �������"
#define MSGTR_MENU_Subtitles "��������"

// --- equalizer
#define MSGTR_EQU_Audio "�����"
#define MSGTR_EQU_Video "�����"
#define MSGTR_EQU_Contrast "��������: "
#define MSGTR_EQU_Brightness "�������: "
#define MSGTR_EQU_Hue "����: "
#define MSGTR_EQU_Saturation "������������: "
#define MSGTR_EQU_Front_Left "�������� �����"
#define MSGTR_EQU_Front_Right "�������� ������"
#define MSGTR_EQU_Back_Left "������ �����"
#define MSGTR_EQU_Back_Right "������ ������"
#define MSGTR_EQU_Center "�����������"
#define MSGTR_EQU_Bass "����"
#define MSGTR_EQU_All "���"
#define MSGTR_EQU_Channel1 "����� 1:"
#define MSGTR_EQU_Channel2 "����� 2:"
#define MSGTR_EQU_Channel3 "����� 3:"
#define MSGTR_EQU_Channel4 "����� 4:"
#define MSGTR_EQU_Channel5 "����� 5:"
#define MSGTR_EQU_Channel6 "����� 6:"

// --- playlist
#define MSGTR_PLAYLIST_Path "����"
#define MSGTR_PLAYLIST_Selected "��������� �����"
#define MSGTR_PLAYLIST_Files "�����"
#define MSGTR_PLAYLIST_DirectoryTree "������ ���������"

// --- preferences
#define MSGTR_PREFERENCES_SubtitleOSD "�������� � OSD"
#define MSGTR_PREFERENCES_Codecs "������ � �������� [demuxer]"
#define MSGTR_PREFERENCES_Misc "������"

#define MSGTR_PREFERENCES_None "���"
#define MSGTR_PREFERENCES_DriverDefault "��������� ��������"
#define MSGTR_PREFERENCES_AvailableDrivers "��������� ��������:"
#define MSGTR_PREFERENCES_DoNotPlaySound "�� ����������� ����"
#define MSGTR_PREFERENCES_NormalizeSound "������������� ����"
#define MSGTR_PREFERENCES_EnableEqualizer "�������� ����������"
#define MSGTR_PREFERENCES_ExtraStereo "�������� �������������� ������"
#define MSGTR_PREFERENCES_Coefficient "�����������:"
#define MSGTR_PREFERENCES_AudioDelay "�������� �����"
#define MSGTR_PREFERENCES_DoubleBuffer "�������� ������� �����������"
#define MSGTR_PREFERENCES_DirectRender "�������� ������ �����������"
#define MSGTR_PREFERENCES_FrameDrop "�������� ������������ ������"
#define MSGTR_PREFERENCES_HFrameDrop "�������� ������� ������������ ������ (������)"
#define MSGTR_PREFERENCES_Flip "���������� ����������� ����� ������"
#define MSGTR_PREFERENCES_Panscan "�������� ������: "
#define MSGTR_PREFERENCES_OSDTimer "������ � ����������"
#define MSGTR_PREFERENCES_OSDProgress "������ ������ ����������"
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "������, �������� � ������ �����"
#define MSGTR_PREFERENCES_Subtitle "��������:"
#define MSGTR_PREFERENCES_SUB_Delay "��������: "
#define MSGTR_PREFERENCES_SUB_FPS "����/���:"
#define MSGTR_PREFERENCES_SUB_POS "�������: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "��������� ������������ ���������"
#define MSGTR_PREFERENCES_SUB_Unicode "���������� ��������"
#define MSGTR_PREFERENCES_SUB_MPSUB "�������������� ������ �������� � MPlayer'������ ������ ���������"
#define MSGTR_PREFERENCES_SUB_SRT "�������������� ������ �������� � ���������� �� ������� SubViewer (SRT) ������"
#define MSGTR_PREFERENCES_SUB_Overlap "�������� ������������ ���������"
#define MSGTR_PREFERENCES_Font "�����:"
#define MSGTR_PREFERENCES_FontFactor "����������� ������:"
#define MSGTR_PREFERENCES_PostProcess "�������� �������������"
#define MSGTR_PREFERENCES_AutoQuality "���� ��������: "
#define MSGTR_PREFERENCES_NI "������������ '����ϣ���' AVI ������"
#define MSGTR_PREFERENCES_IDX "���� ���������, ����������� ��������� �������"
#define MSGTR_PREFERENCES_VideoCodecFamily "��������� ����� �������:"
#define MSGTR_PREFERENCES_AudioCodecFamily "��������� ����� �������:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "������� OSD"
#define MSGTR_PREFERENCES_FRAME_Subtitle "��������"
#define MSGTR_PREFERENCES_FRAME_Font "�����"
#define MSGTR_PREFERENCES_FRAME_PostProcess "�������������"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "����� � �������� [demuxer]"
#define MSGTR_PREFERENCES_FRAME_Cache "���"
#define MSGTR_PREFERENCES_Audio_Device "����������:"
#define MSGTR_PREFERENCES_Audio_Mixer "������:"
#define MSGTR_PREFERENCES_Audio_MixerChannel "����� �������:"
#define MSGTR_PREFERENCES_Message "����������, ���������, ��� ��� ����� ������������� ������������,\n����� ��������� ��������� �������� � ����!"
#define MSGTR_PREFERENCES_DXR3_VENC "����� ����������:"
#define MSGTR_PREFERENCES_DXR3_LAVC "������������ LAVC (FFmpeg)"
#define MSGTR_PREFERENCES_DXR3_FAME "������������ FAME"
#define MSGTR_PREFERENCES_FontEncoding1 "������"
#define MSGTR_PREFERENCES_FontEncoding2 "������������������ ����� (ISO-8859-1)"
#define MSGTR_PREFERENCES_FontEncoding3 "������������������ ����� � ���� (ISO-8859-15)"
#define MSGTR_PREFERENCES_FontEncoding4 "����������/����������-����������� ����� (ISO-8859-2)"
#define MSGTR_PREFERENCES_FontEncoding5 "���������, �����������, �����������, �������� (ISO-8859-3)"
#define MSGTR_PREFERENCES_FontEncoding6 "������ ���������� ��������� (ISO-8859-4)"
#define MSGTR_PREFERENCES_FontEncoding7 "��������� (ISO-8859-5)"
#define MSGTR_PREFERENCES_FontEncoding8 "�������� (ISO-8859-6)"
#define MSGTR_PREFERENCES_FontEncoding9 "����������� ��������� (ISO-8859-7)"
#define MSGTR_PREFERENCES_FontEncoding10 "�������� (ISO-8859-9)"
#define MSGTR_PREFERENCES_FontEncoding11 "���������� (ISO-8859-13)"
#define MSGTR_PREFERENCES_FontEncoding12 "��������� (ISO-8859-14)"
#define MSGTR_PREFERENCES_FontEncoding13 "��������� ��������� (ISO-8859-8)"
#define MSGTR_PREFERENCES_FontEncoding14 "������� (KOI8-R)"
#define MSGTR_PREFERENCES_FontEncoding15 "����������, ����������� (KOI8-U/RU)"
#define MSGTR_PREFERENCES_FontEncoding16 "����ݣ���� ��������� ��������� (CP936)"
#define MSGTR_PREFERENCES_FontEncoding17 "������������ ��������� ��������� (BIG5)"
#define MSGTR_PREFERENCES_FontEncoding18 "�������� ��������� (SHIFT-JIS)"
#define MSGTR_PREFERENCES_FontEncoding19 "��������� ��������� (CP949)"
#define MSGTR_PREFERENCES_FontEncoding20 "������� ��������� (CP874)"
#define MSGTR_PREFERENCES_FontEncoding21 "��������� Window$ (CP1251)"
#define MSGTR_PREFERENCES_FontEncoding22 "����������/���������� ����������� Window$ (CP1250)"
#define MSGTR_PREFERENCES_FontNoAutoScale "�� ��������������"
#define MSGTR_PREFERENCES_FontPropWidth "��������������� ������ ������"
#define MSGTR_PREFERENCES_FontPropHeight "��������������� ������ ������"
#define MSGTR_PREFERENCES_FontPropDiagonal "��������������� ��������� ������"
#define MSGTR_PREFERENCES_FontEncoding "���������:"
#define MSGTR_PREFERENCES_FontBlur "��ޣ������:"
#define MSGTR_PREFERENCES_FontOutLine "�������:"
#define MSGTR_PREFERENCES_FontTextScale "������� ������:"
#define MSGTR_PREFERENCES_FontOSDScale "������� OSD:"
#define MSGTR_PREFERENCES_Cache "��� ���/����"
#define MSGTR_PREFERENCES_CacheSize "������ ����: "
#define MSGTR_PREFERENCES_LoadFullscreen "���������� � ������������� ������"
#define MSGTR_PREFERENCES_SaveWinPos "��������� ������� ����"
#define MSGTR_PREFERENCES_XSCREENSAVER "������������� XScreenSaver"
#define MSGTR_PREFERENCES_PlayBar "�������� ������ ���������������"
#define MSGTR_PREFERENCES_AutoSync "����������������� ���/����"
#define MSGTR_PREFERENCES_AutoSyncValue "�����������������: "
#define MSGTR_PREFERENCES_CDROMDevice "CD-ROM ����������:"
#define MSGTR_PREFERENCES_DVDDevice "DVD ����������:"
#define MSGTR_PREFERENCES_FPS "FPS ������:"
#define MSGTR_PREFERENCES_ShowVideoWindow "���������� ���� �����, ����� ���������"
#define MSGTR_ABOUT_UHU "���������� GUI ������������ UHU Linux\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "��������� ������!"
#define MSGTR_MSGBOX_LABEL_Error "������!"
#define MSGTR_MSGBOX_LABEL_Warning "��������������!" 

// bitmap.c

#define MSGTR_NotEnoughMemoryC32To1 "[c32to1] ������������ ������ ��� �����������\n"
#define MSGTR_NotEnoughMemoryC1To32 "[c1to32] ������������ ������ ��� �����������\n"

// cfg.c

#define MSGTR_ConfigFileReadError "[cfg] ������ ������ ����������������� �����...\n"
#define MSGTR_UnableToSaveOption "[cfg] �� ���� ��������� ����� '%s'.\n"

// interface.c

#define MSGTR_DeletingSubtitles "[GUI] ������ ��������.\n"
#define MSGTR_LoadingSubtitles "[GUI] �������� ��������: %s\n"
#define MSGTR_AddingVideoFilter "[GUI] �������� ����� ������: %s\n"
#define MSGTR_RemovingVideoFilter "[GUI] ������ ����� ������: %s\n"

// mw.c

#define MSGTR_NotAFile "��� �� ������ �� ����: '%s' !\n"

// ws.c

#define MSGTR_WS_CouldNotOpenDisplay "[ws] �� ���� ������� �������.\n"
#define MSGTR_WS_RemoteDisplay "[ws] ���̣���� �������, �������� XMITSHM.\n"
#define MSGTR_WS_NoXshm "[ws] ��������, ���� ������� �� ������������ ���������� ����������� ������ X'��.\n"
#define MSGTR_WS_NoXshape "[ws] ��������, ���� ������� �� ������������ ���������� XShape.\n"
#define MSGTR_WS_ColorDepthTooLow "[ws] ��������, ������� ����� ������� ����.\n"
#define MSGTR_WS_TooManyOpenWindows "[ws] ������� ����� �������� ����.\n"
#define MSGTR_WS_ShmError "[ws] ������ ���������� ����������� ������\n"
#define MSGTR_WS_NotEnoughMemoryDrawBuffer "[ws] ��������, ������������ ������ ��� ������ ����������.\n"
#define MSGTR_WS_DpmsUnavailable "DPMS �� ��������?\n"
#define MSGTR_WS_DpmsNotEnabled "�� ���� �������� DPMS.\n"

// wsxdnd.c

#define MSGTR_WS_NotAFile "��� �� ������ �� ����...\n"
#define MSGTR_WS_DDNothing "D&D: ������ �� ����������!\n"

#endif

// ======================= VO Video Output drivers ========================

#define MSGTR_VOincompCodec "��������, ��������� ���������� ����������� �� ���������� � ���� �������.\n"
#define MSGTR_VO_GenericError "��������� ��������� ������"
#define MSGTR_VO_UnableToAccess "�� ���� �������� ������"
#define MSGTR_VO_ExistsButNoDirectory "��� ����������, �� �� �������� �����������."
#define MSGTR_VO_DirExistsButNotWritable "���������� ������ ��� ����������, �� �� �������� ��� ������."
#define MSGTR_VO_DirExistsAndIsWritable "���������� ������ ��� ���������� � �������� ��� ������."
#define MSGTR_VO_CantCreateDirectory "�� ���� ������� ���������� ������."
#define MSGTR_VO_CantCreateFile "�� ���� ������� �������� ����."
#define MSGTR_VO_DirectoryCreateSuccess "���������� ������ ������� �������."
#define MSGTR_VO_ParsingSuboptions "�������� ��������� ��������."
#define MSGTR_VO_SuboptionsParsedOK "��������� �������� �������� �������."
#define MSGTR_VO_ValueOutOfRange "�������� ��� ����������� ���������"
#define MSGTR_VO_NoValueSpecified "�������� �� �������."
#define MSGTR_VO_UnknownSuboptions "�����������(��) ��������(�)"

// vo_jpeg.c
#define MSGTR_VO_JPEG_ProgressiveJPEG "������������� JPEG �������."
#define MSGTR_VO_JPEG_NoProgressiveJPEG "������������� JPEG ��������."
#define MSGTR_VO_JPEG_BaselineJPEG "������� JPEG �������."
#define MSGTR_VO_JPEG_NoBaselineJPEG "������� JPEG ��������."

// vo_pnm.c
#define MSGTR_VO_PNM_ASCIIMode "����� ASCII �������."
#define MSGTR_VO_PNM_RawMode "'�����' ����� �������."
#define MSGTR_VO_PNM_PPMType "����� �������� PPM �����."
#define MSGTR_VO_PNM_PGMType "����� �������� PGM �����."
#define MSGTR_VO_PNM_PGMYUVType "����� �������� PGMYUV �����."

// vo_yuv4mpeg.c
#define MSGTR_VO_YUV4MPEG_InterlacedHeightDivisibleBy4 "��� ������������� ������ ����������, ����� ������ ����������� �������� �� 4."
#define MSGTR_VO_YUV4MPEG_InterlacedLineBufAllocFail "�� ���� �������� ������ ��� ��������� ������ � ������������ ������."
#define MSGTR_VO_YUV4MPEG_InterlacedInputNotRGB "���� �� RGB, �� ���� ��������� ����������� ������ �� �����!"
#define MSGTR_VO_YUV4MPEG_WidthDivisibleBy2 "������ ����������� ������ �������� �� 2."
#define MSGTR_VO_YUV4MPEG_NoMemRGBFrameBuf "������������ ������ ��� ���������� ����������� RGB."
#define MSGTR_VO_YUV4MPEG_OutFileOpenError "�� ���� �������� ������ ��� �������� ��������� ��� ������ \"%s\"!"
#define MSGTR_VO_YUV4MPEG_OutFileWriteError "������ ������ ����������� � �����!"
#define MSGTR_VO_YUV4MPEG_UnknownSubDev "����������� �������������: %s"
#define MSGTR_VO_YUV4MPEG_InterlacedTFFMode "��������� ������������ ����� ������, ������� ���� ������."
#define MSGTR_VO_YUV4MPEG_InterlacedBFFMode "��������� ������������ ����� ������, ������ ���� ������."
#define MSGTR_VO_YUV4MPEG_ProgressiveMode "��������� (�� ���������) �������������� ����� ������."

// Old vo drivers that have been replaced

#define MSGTR_VO_PGM_HasBeenReplaced "������� ����������� pgm ��� ����Σ� -vo pnm:pgmyuv.\n"
#define MSGTR_VO_MD5_HasBeenReplaced "������� ����������� md5 ��� ����Σ� -vo md5sum.\n"

// ======================= AO Audio Output drivers ========================

// libao2 

// audio_out.c
#define MSGTR_AO_ALSA9_1x_Removed "����������: ������ alsa9 � alsa1x ���� �������, ����������� -ao alsa ������.\n"

// ao_oss.c
#define MSGTR_AO_OSS_CantOpenMixer "[AO OSS] ������������� �����: �� ���� ������� ���������� ������� %s: %s\n"
#define MSGTR_AO_OSS_ChanNotFound "[AO OSS] ������������� �����: � ������� ���������� ����������� ����� '%s',\n��������� ����� �� ���������.\n"
#define MSGTR_AO_OSS_CantOpenDev "[AO OSS] ������������� �����: �� ���� ������� ��������������� %s: %s\n"
#define MSGTR_AO_OSS_CantMakeFd "[AO OSS] ������������� �����: �� ���� ������������� �������� ���������: %s\n"
#define MSGTR_AO_OSS_CantSetChans "[AO OSS] ������������� �����: �� ���� ���������� ��������������� � %d-��������� �����.\n"
#define MSGTR_AO_OSS_CantUseGetospace "[AO OSS] ������������� �����: ������� �� ������������ SNDCTL_DSP_GETOSPACE :-(\n"
#define MSGTR_AO_OSS_CantUseSelect "[AO OSS]\n   ***  ��� ������������ �� ������������ select()  ***\n ���������������� MPlayer � #undef HAVE_AUDIO_SELECT � config.h !\n\n"
#define MSGTR_AO_OSS_CantReopen "[AO OSS]\n��������� ������: *** �� ���� �������� ������� / �������� ��������������� (%s) ***\n"

// ao_arts.c
#define MSGTR_AO_ARTS_CantInit "[AO ARTS] %s\n"
#define MSGTR_AO_ARTS_ServerConnect "[AO ARTS] ���������� � �������� ��������.\n"
#define MSGTR_AO_ARTS_CantOpenStream "[AO ARTS] �� ���� ������� �����.\n"
#define MSGTR_AO_ARTS_StreamOpen "[AO ARTS] ����� ������.\n"
#define MSGTR_AO_ARTS_BufferSize "[AO ARTS] ������ ������: %d\n"

// ao_dxr2.c
#define MSGTR_AO_DXR2_SetVolFailed "[AO DXR2] �� ���� ���������� ��������� � %d.\n"
#define MSGTR_AO_DXR2_UnsupSamplerate "[AO DXR2] dxr2: %d �� �� ��������������, ���������� \"-aop list=resample\"\n"

// ao_esd.c
#define MSGTR_AO_ESD_CantOpenSound "[AO ESD] ��������� esd_open_sound �� �������: %s\n"
#define MSGTR_AO_ESD_LatencyInfo "[AO ESD] ��������: [������: %0.2fs, ����: %0.2fs] (���������� %0.2fs)\n"
#define MSGTR_AO_ESD_CantOpenPBStream "[AO ESD] �� ���� ������� ����� ��������������� esd: %s\n"

// ao_mpegpes.c
#define MSGTR_AO_MPEGPES_CantSetMixer "[AO MPEGPES] DVB �����: �� ���� ���������� ������: %s\n"
#define MSGTR_AO_MPEGPES_UnsupSamplerate "[AO MPEGPES] %d �� �� ��������������, ���������� �������������...\n"

// ao_null.c
// This one desn't even  have any mp_msg nor printf's?? [CHECK]

// ao_pcm.c
#define MSGTR_AO_PCM_FileInfo "[AO PCM] ����: %s (%s)\nPCM: ������� ���������������: %i �� ������: %s ������ %s\n"
#define MSGTR_AO_PCM_HintInfo "[AO PCM] ����������: �������� ������� ������� ����������� � -vc dummy -vo null\nPCM: ����������: ��� ������ WAVE ������ ����������� -ao pcm:waveheader (�� ���������).\n"
#define MSGTR_AO_PCM_CantOpenOutputFile "[AO PCM] �� ���� ������� %s ��� ������!\n"

// ao_sdl.c
#define MSGTR_AO_SDL_INFO "[AO SDL] ������� ���������������: %i �� ������: %s ������ %s\n"
#define MSGTR_AO_SDL_DriverInfo "[AO SDL] ���������� %s ������������.\n"
#define MSGTR_AO_SDL_UnsupportedAudioFmt "[AO SDL] ���������������� �����������: 0x%x.\n"
#define MSGTR_AO_SDL_CantInit "[AO SDL] �� ���� ���������������� SDL �����: %s\n"
#define MSGTR_AO_SDL_CantOpenAudio "[AO SDL] �� ���� ������� �����: %s\n"

// ao_sgi.c
#define MSGTR_AO_SGI_INFO "[AO SGI] ����������.\n"
#define MSGTR_AO_SGI_InitInfo "[AO SGI] �������������: ������� ���������������: %i �� ������: %s ������ %s\n"
#define MSGTR_AO_SGI_InvalidDevice "[AO SGI] ���������������: �������� ����������.\n"
#define MSGTR_AO_SGI_CantSetParms_Samplerate "[AO SGI] �������������: ������ ��������� ����������: %s\n�� ���� ���������� ��������� ������� ���������������.\n"
#define MSGTR_AO_SGI_CantSetAlRate "[AO SGI] �������������: AL_RATE �� �������� �� �������� �������.\n"
#define MSGTR_AO_SGI_CantGetParms "[AO SGI] �������������: ������ ��������� ����������: %s\n"
#define MSGTR_AO_SGI_SampleRateInfo "[AO SGI] �������������: ������� ��������������� ������ %lf (��������� ������� %lf)\n"
#define MSGTR_AO_SGI_InitConfigError "[AO SGI] �������������: %s\n"
#define MSGTR_AO_SGI_InitOpenAudioFailed "[AO SGI] �������������: �� ���� ������� ����������: %s\n"
#define MSGTR_AO_SGI_Uninit "[AO SGI] ���������������: ...\n"
#define MSGTR_AO_SGI_Reset "[AO SGI] �����: ...\n"
#define MSGTR_AO_SGI_PauseInfo "[AO SGI] ����� �����: ...\n"
#define MSGTR_AO_SGI_ResumeInfo "[AO SGI] ������������� �����: ...\n"

// ao_sun.c
#define MSGTR_AO_SUN_RtscSetinfoFailed "[AO SUN] rtsc: ��������� SETINFO �� �������.\n"
#define MSGTR_AO_SUN_RtscWriteFailed "[AO SUN] rtsc: ������ �� �������."
#define MSGTR_AO_SUN_CantOpenAudioDev "[AO SUN] �� ���� ������� ��������������� %s, %s -> ��� �����.\n"
#define MSGTR_AO_SUN_UnsupSampleRate "[AO SUN] ������������� �����: ���� ����� �� ������������ ����� %d, %s,\n������� ��������������� %d ��.\n"
#define MSGTR_AO_SUN_CantUseSelect "[AO SUN]\n   ***  ��� ������������ �� ������������ select()  ***\n ���������������� MPlayer � #undef HAVE_AUDIO_SELECT � config.h !\n\n"
#define MSGTR_AO_SUN_CantReopenReset "[AO SUN]\n��������� ������: *** �� ���� �������� ������� / �������� ��������������� (%s) ***\n"

// ao_alsa5.c
#define MSGTR_AO_ALSA5_InitInfo "[AO ALSA5] ������������� alsa: ����������� ������: %d ��, %d �������, %s\n"
#define MSGTR_AO_ALSA5_SoundCardNotFound "[AO ALSA5] ������������� alsa: �� �������(�) ��������(��) �����(�).\n"
#define MSGTR_AO_ALSA5_InvalidFormatReq "[AO ALSA5] ������������� alsa: �������� �������� ������ (%s) - ����� ��������.\n"
#define MSGTR_AO_ALSA5_PlayBackError "[AO ALSA5] ������������� alsa: ������ �������� ������ ���������������: %s\n"
#define MSGTR_AO_ALSA5_PcmInfoError "[AO ALSA5] ������������� alsa: ������ ��������� pcm ����������: %s\n"
#define MSGTR_AO_ALSA5_SoundcardsFound "[AO ALSA5] ������������� alsa: �������(�) %d ��������(��) ����(�), ���������: %s\n"
#define MSGTR_AO_ALSA5_PcmChanInfoError "[AO ALSA5] ������������� alsa: ������ ��������� ���������� pcm ������: %s\n"
#define MSGTR_AO_ALSA5_CantSetParms "[AO ALSA5] ������������� alsa: ������ ��������� ����������: %s\n"
#define MSGTR_AO_ALSA5_CantSetChan "[AO ALSA5] ������������� alsa: ������ ��������� ������: %s\n"
#define MSGTR_AO_ALSA5_ChanPrepareError "[AO ALSA5] ������������� alsa: ������ ���������� ������: %s\n"
#define MSGTR_AO_ALSA5_DrainError "[AO ALSA5] ��������������� alsa: ������ ������� ������ ���������������: %s\n"
#define MSGTR_AO_ALSA5_FlushError "[AO ALSA5] ��������������� alsa: ������ ������ ������� ������ ���������������: %s\n"
#define MSGTR_AO_ALSA5_PcmCloseError "[AO ALSA5] ��������������� alsa: ������ �������� pcm: %s\n"
#define MSGTR_AO_ALSA5_ResetDrainError "[AO ALSA5] ����� alsa: ������ ������� ������ ���������������: %s\n"
#define MSGTR_AO_ALSA5_ResetFlushError "[AO ALSA5] ����� alsa: ������ ������ ������� ������ ���������������: %s\n"
#define MSGTR_AO_ALSA5_ResetChanPrepareError "[AO ALSA5] ����� alsa: ������ ���������� ������: %s\n"
#define MSGTR_AO_ALSA5_PauseDrainError "[AO ALSA5] ����� alsa: ������ ������� ������ ���������������: %s\n"
#define MSGTR_AO_ALSA5_PauseFlushError "[AO ALSA5] ����� alsa: ������ ������ ������� ������ ���������������: %s\n"
#define MSGTR_AO_ALSA5_ResumePrepareError "[AO ALSA5] ������������� alsa: ������ ���������� ������: %s\n"
#define MSGTR_AO_ALSA5_Underrun "[AO ALSA5] ��������������� alsa: alsa �����������, ��������� �����.\n"
#define MSGTR_AO_ALSA5_PlaybackPrepareError "[AO ALSA5] ��������������� alsa: ������ ���������� ������ ���������������: %s\n"
#define MSGTR_AO_ALSA5_WriteErrorAfterReset "[AO ALSA5] ��������������� alsa: ������ ������ ����� ������: %s - ���������.\n"
#define MSGTR_AO_ALSA5_OutPutError "[AO ALSA5] ��������������� alsa: ������ ������: %s\n"

// ao_plugin.c

#define MSGTR_AO_PLUGIN_InvalidPlugin "[AO ������] �������� ������: %s\n"

// ======================= AF Audio Filters ================================

// libaf 

// af_ladspa.c

#define MSGTR_AF_LADSPA_AvailableLabels "��������� ����� �"
#define MSGTR_AF_LADSPA_WarnNoInputs "��������������! � ����� LADSPA ������� ����������� ����������.\n  �������� ����������� ����� �������."
#define MSGTR_AF_LADSPA_ErrMultiChannel "������-��������� (>2) ������� ���� ��� �� ��������������.\n  ����������� ������ ����- � �������������."
#define MSGTR_AF_LADSPA_ErrNoOutputs "� ����� LADSPA ������� ����������� �����������."
#define MSGTR_AF_LADSPA_ErrInOutDiff "����� ����������� � ������������ � LADSPA ������� ����������."
#define MSGTR_AF_LADSPA_ErrFailedToLoad "�� ���� ���������"
#define MSGTR_AF_LADSPA_ErrNoDescriptor "�� ���� ����� ������� ladspa_descriptor() � ��������� ����� ����������."
#define MSGTR_AF_LADSPA_ErrLabelNotFound "�� ���� ����� ����� � ���������� �������."
#define MSGTR_AF_LADSPA_ErrNoSuboptions "�� ������� ��������"
#define MSGTR_AF_LADSPA_ErrNoLibFile "�� ������ ���� ����������"
#define MSGTR_AF_LADSPA_ErrNoLabel "�� ������� ����� �������"
#define MSGTR_AF_LADSPA_ErrNotEnoughControls "������������ �������� ������� � ��������� ������"
#define MSGTR_AF_LADSPA_ErrControlBelow "%s: ������� �������� #%d ������ ������ ������� %0.4f.\n"
#define MSGTR_AF_LADSPA_ErrControlAbove "%s: ������� �������� #%d ������ ������� ������� %0.4f.\n"


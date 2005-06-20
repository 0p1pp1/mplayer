// Sync'ed with help_mp-en.h 1.167
// Encoding/���������: MS CP1251
//
// ��������� �� �. ��������, plazmus@gmail.com
// ������ ����������� �� ����� �����.

// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"��������:   mplayer [�����] [url|���/]���_��_����\n"
"\n"
"������� �����:   (������� ������ � � ������������� - 'man mplayer')\n"
" -vo <���[:����]>  ����� �� ����� ������� & ���������� ('-vo help' ���� ������)\n"
" -ao <���[:����]>  ����� �� ������ ������� & ���������� ('-ao help' ���� ������)\n"
#ifdef HAVE_VCD
" vcd://<�������>   ����� (S)VCD (Super Video CD) ������� (��� ���������!)\n"
#endif
#ifdef USE_DVDREAD
" dvd://<�����>     ����� DVD �������� �� ����������, ������ �� ����\n"
" -alang/-slang     ����� �� ���� �� DVD ����o/�������� (���� 2-������ ���)\n"
#endif
" -ss <�������>     ���������� �� ������ (� ������� ��� ��:��:��) �������\n"
" -nosound          ���������� �� �����\n"
" -fs               ������������ ��������������� (��� -vm, -zoom, ��. manpage)\n"
" -x <x> -y <y>     ����� �� ��������� (�������� �� � -vm ��� -zoom)\n"
" -sub <����>       ������ ����� ��� �������� (����� ���� -subfps � -subdelay)\n"
" -playlist <����>  ������ playlist ����\n"
" -vid x -aid y     ����� �� ����� (x) � ����� (y) ����� �� ���������������\n"
" -fps x -srate y   ����� �� ����� (x ����� � �������) � ����� (y Hz) ���������\n"
" -pp <��������>    ������� ������ �� ������������ ��������� �� ������\n"
"                   ����� ������������� � �������������� �� �����������\n"
" -framedrop        ��������� ������������ �� ����� (��� ����� ������)\n"
"\n"
"������� �������:   (����� ������ ��� � �������������, ��������� ���� input.conf)\n"
" <-  ���  ->       �������� �����/������ � 10 �������\n"
" up ��� down       �������� �����/������ � 1 ������\n"
" pgup ��� pgdown   �������� �����/������ � 10 ������\n"
" < ��� >           ������ �����/������ � playlist �������\n"
" p ��� SPACE       ����� (��������� ���������� ������ �� �����������)\n"
" q ��� ESC         ������� �� ����������������� � ����� �� ����������\n"
" + ��� -           ������� ������������ �� ����� � +/- 0.1 �������\n"
" o                 ���������� OSD ������: ���/����� �� ����������/����� � ������\n"
" * ��� /           ��������� ��� �������� ������ �� ����� (PCM)\n"
" z ��� x           ������� ������������ �� ���������� � +/- 0.1 �������\n"
" r ��� t           ��������� ���������� ������/������, ����� � -vf expand\n"
"\n"
" * * * �� �����������, ������������ ����� � �������, ����� �������������! * * *\n"
"\n";
#endif

#define MSGTR_SamplesWanted "������ �� ���� ������ �� ����� �� ����������� �� �����������. �������� �� � ���!\n"

// ========================= MPlayer messages ===========================

// mplayer.c:

#define MSGTR_Exiting "\n�������� �� ����������...\n"
#define MSGTR_ExitingHow "\n�������� �� ����������... (%s)\n"
#define MSGTR_Exit_quit "�����"
#define MSGTR_Exit_eof "���� �� �����"
#define MSGTR_Exit_error "������� ������"
#define MSGTR_IntBySignal "\nMPlayer � ��������� �� ������ %d � �����: %s\n"
#define MSGTR_NoHomeDir "HOME ������������ �� ���� �� ���� �������.\n"
#define MSGTR_GetpathProblem "������� � ������� get_path(\"config\") \n"
#define MSGTR_CreatingCfgFile "������� �� ��������������� ����: %s\n"
#define MSGTR_InvalidAOdriver "��������� ����� �������: %s\n��������� '-ao help' �� ������� � ��������.\n"
#define MSGTR_CopyCodecsConf "(���������/�������� etc/codecs.conf �� ����� �� MPlayer � ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "�������� �� ��������� codecs.conf.\n"
#define MSGTR_CantLoadFont "�� ���� �� �� ������ �����: %s\n"
#define MSGTR_CantLoadSub "�� ����� �� ����� �������� ��������: %s\n"
#define MSGTR_DumpSelectedStreamMissing "dump: �������: ��������� ����� ������!\n"
#define MSGTR_CantOpenDumpfile "�� ���� �� �� ������ ���� �� ���������.\n"
#define MSGTR_CoreDumped "������� ��������� ;)\n"
#define MSGTR_FPSnotspecified "���� ����� � ������� �� � ������ ��� � ���������, ��������� ������� -fps .\n"
#define MSGTR_TryForceAudioFmtStr "���� �� �������� �� ������� ����� ������ %s...\n"
#define MSGTR_CantFindAudioCodec "�� ���� �� ���� ������� ����� �� ���� ����� ������ 0x%X.\n"
#define MSGTR_RTFMCodecs "��������� DOCS/HTML/en/codecs.html!\n"
#define MSGTR_TryForceVideoFmtStr "���� �� �������� �� ������� ����� ������ %s...\n"
#define MSGTR_CantFindVideoCodec "���� �������� ����� �� ��������� -vo � ����� ������ 0x%X.\n"
#define MSGTR_CannotInitVO "�������: ����� �������� �� ���� �� ���� �������������.\n"
#define MSGTR_CannotInitAO "����� ������������ �� ���� �� ���� ��������/�������������� -> ���� ����.\n"
#define MSGTR_StartPlaying "������� �����������������...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"           ************************************************\n"\
"           **** ������ ������� � ������ ����� �� ����!  ****\n"\
"           ************************************************\n\n"\
"�������� �������, ��������, �������:\n"\
"- ���-��������: ���������/����� _�����_ �������\n"\
"  - �������� -ao sdl ��� ��������� OSS ���������� �� ALSA.\n"\
"  - ����������������� � �������� ��������� �� -autosync, 30 � ����� ������.\n"\
"- ����� ����� ���������\n"\
"  - �������� ���� -vo ������� (-vo help �� ������) ��� ��������� -framedrop!\n"\
"- ����� ��������\n"\
"  - �� �������� ����� DVD/DivX ���� �� ����� ��������! ��������� -hardframedrop.\n"\
"- �������� ����\n"\
"  - �������� �������� ���������� ��  -nobps -ni -forceidx -mc 0.\n"\
"- ����� �������� (NFS/SMB, DVD, VCD � �.�.)\n"\
"  - �������� -cache 8192.\n"\
"- ���������� -cache �� non-interleaved AVI ����?\n"\
"  - �������� -nocache.\n"\
"��������� DOCS/HTML/en/video.html �� ������ ������� �����������.\n"\
"��� ���� �� ������, ��������� DOCS/HTML/en/bugreports.html.\n\n"

#define MSGTR_NoGui "MPlayer � ���������� ��� �������� ���������.\n"
#define MSGTR_GuiNeedsX "��������� ��������� �� MPlayer ������� X11.\n"
#define MSGTR_Playing "��������������� �� %s.\n"
#define MSGTR_NoSound "�����: ���� ����\n"
#define MSGTR_FPSforced "�������� �� %5.3f ������ � ������� (ftime: %5.3f).\n"
#define MSGTR_CompiledWithRuntimeDetection "���������� � ��������� ������������ �� ��������� - �������� - ���� �� � ���������!\n�� ���-����� ����������������, �������������� MPlayer � --disable-runtime-cpudetection.\n"
#define MSGTR_CompiledWithCPUExtensions "���������� �� x86 ��������� � ����������:"
#define MSGTR_AvailableVideoOutputDrivers "�������� ����� ��������:\n"
#define MSGTR_AvailableAudioOutputDrivers "�������� ����� ��������:\n"
#define MSGTR_AvailableAudioCodecs "�������� ����� ������:\n"
#define MSGTR_AvailableVideoCodecs "�������� ����� ������:\n"
#define MSGTR_AvailableAudioFm "�������� (��������) ������� ����� ������/��������:\n"
#define MSGTR_AvailableVideoFm "�������� (��������) ������� ����� ������/��������:\n"
#define MSGTR_AvailableFsType "�������� ������������ ������:\n"
#define MSGTR_UsingRTCTiming "�������� �� ���������� RTC ������ (%ldHz).\n"
#define MSGTR_CannotReadVideoProperties "�����: ����������� �� ����� �� ����� ���������.\n"
#define MSGTR_NoStreamFound "�� � ������ �����.\n"
#define MSGTR_ErrorInitializingVODevice "������ ��� ��������/�������������� �� ��������� ����� ���������� (-vo).\n"
#define MSGTR_ForcedVideoCodec "������� ����� �����: %s\n"
#define MSGTR_ForcedAudioCodec "������� ����� �����: %s\n"
#define MSGTR_Video_NoVideo "�����: ���� �����\n"
#define MSGTR_NotInitializeVOPorVO "\n�������: ����� ������� (-vf) ��� ������ (-vo) �� ����� �� ����� ��������������.\n"
#define MSGTR_Paused "\n  =====  �����  =====\r" // no more than 23 characters (status line for audio files)
#define MSGTR_PlaylistLoadUnable "\nPlaylist-�� �� ���� �� ���� ������� %s.\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- MPlayer ������������ ������ '��������� ����������'.\n"\
"  ���� �� � ��� � ���� �� ��������� ������������ �� ���������...\n"\
"  ���� ��������� DOCS/HTML/en/bugreports.html.\n"
#define MSGTR_Exit_SIGILL \
"- MPlayer ������������ ������ '��������� ����������'.\n"\
"  ���� ���������� �� ������ ������ ���� ������ �� ��������, �������� �� ����\n"\
"  �� ����� � ����������/�����������.\n"\
"  ��������� ����!\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- MPlayer ������������ ������ ���� �������� �� ���������/�����������/�������.\n"\
"  �������������� MPlayer � --enable-debug � ���������  backtrace �\n"\
"  �������������� � 'gdb'.\n�� ����������� - DOCS/HTML/en/bugreports_what.html#bugreports_crash.\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer ������������. T��� �� ������ �� �� ������.\n"\
"  ���� �� � ��� � ���� �� MPlayer _���_ ��� ���������� �� _���_ ���\n"\
"  �������� �� gcc. ��� �������, �� � �� ���� �� MPlayer, ���������\n"\
"  DOCS/HTML/en/bugreports.html � ��������� ������������ ���. ��� �� �����\n"\
"  � ���� �� ��������, ��� �� ��������� ���� ����������, ������ ���������� �� ���.\n"
#define MSGTR_LoadingConfig "������� �� ��������������� ���� '%s'\n"
#define MSGTR_AddedSubtitleFile "SUB: ������� � ���� ��� �������� (%d): %s\n"
#define MSGTR_ErrorOpeningOutputFile "������ ��� �������� �� ����� [%s] �� �����!\n"
#define MSGTR_CommandLine "�������� ���:"
#define MSGTR_RTCDeviceNotOpenable "������ ��� �������� �� %s: %s (���������� �� ����� �� ������).\n"
#define MSGTR_LinuxRTCInitErrorIrqpSet "Linux RTC ������ ��� ������������� � ioctl (rtc_irqp_set ��%lu): %s\n"
#define MSGTR_IncreaseRTCMaxUserFreq "�������� \"echo %lu > /proc/sys/dev/rtc/max-user-freq\" ��� ���������� �������� ���������.\n"
#define MSGTR_LinuxRTCInitErrorPieOn "Linux RTC init ������ � ioctl (rtc_pie_on): %s\n"
#define MSGTR_UsingTimingType "�������� ��  %s ������.\n"
#define MSGTR_MenuInitialized "������ � ��������������: %s\n"
#define MSGTR_MenuInitFailed "������ �� ���� �� ���� ��������������.\n"
#define MSGTR_Getch2InitializedTwice "��������: ��������� getch2_init � �������� ���������!\n"
#define MSGTR_DumpstreamFdUnavailable "������ �� ���� �� ���� �������� - ���� ������� 'fd'.\n"
#define MSGTR_FallingBackOnPlaylist "�������� ���� �� ��������� �� playlist %s...\n"
#define MSGTR_CantOpenLibmenuFilterWithThisRootMenu "����� ������� libmenu �� ���� �� ���� ������� ��� root ���� %s.\n"
#define MSGTR_AudioFilterChainPreinitError "������ ��� ������������� ������������� �� ����� ��������!\n"
#define MSGTR_LinuxRTCReadError "Linux RTC ������ ��� ������: %s\n"
#define MSGTR_SoftsleepUnderflow "��������! Softsleep underflow!\n"
#define MSGTR_AnsSubVisibility "ANS_SUB_VISIBILITY=%ld\n"
#define MSGTR_AnsLength "ANS_LENGTH=%ld\n"
#define MSGTR_AnsVoFullscreen "ANS_VO_FULLSCREEN=%ld\n"
#define MSGTR_AnsPercentPos "ANS_PERCENT_POSITION=%ld\n"
#define MSGTR_DvdnavNullEvent "DVDNAV ������� NULL?!\n"
#define MSGTR_DvdnavHighlightEventBroken "DVDNAV �������: Highlight event broken\n"
#define MSGTR_DvdnavEvent "DVDNAV �������: %s\n"
#define MSGTR_DvdnavHighlightHide "DVDNAV �������: Highlight Hide\n"
#define MSGTR_DvdnavStillFrame "###################################### DVDNAV �������: ���������� �����: %d ���\n"
#define MSGTR_DvdnavNavStop "DVDNAV �������: Nav ����\n"
#define MSGTR_DvdnavNavNOP "DVDNAV �������: Nav NOP\n"
#define MSGTR_DvdnavNavSpuStreamChangeVerbose "DVDNAV �������: Nav ����� �� SPU �����: ���: %d/%d/%d ���: %d\n"
#define MSGTR_DvdnavNavSpuStreamChange "DVDNAV �������: Nav ����� �� SPU �����: ���: %d ���: %d\n"
#define MSGTR_DvdnavNavAudioStreamChange "DVDNAV �������: Nav ����� �� ����� �����: ���: %d ���: %d\n"
#define MSGTR_DvdnavNavVTSChange "DVDNAV �������: Nav ����� �� VTS\n"
#define MSGTR_DvdnavNavCellChange "DVDNAV �������: Nav ����� �� ������\n"
#define MSGTR_DvdnavNavSpuClutChange "DVDNAV �������: Nav ����� �� SPU CLUT\n"
#define MSGTR_DvdnavNavSeekDone "DVDNAV �������: Nav ������������ ����������\n"
#define MSGTR_MenuCall "Menu call\n"

#define MSGTR_EdlCantUseBothModes "�� �� ������� -edl � -edlout �� �� ��������� ������������.\n"
#define MSGTR_EdlOutOfMem "�� ���� �� �� ������ ���������� ����� �� EDL �������.\n"
#define MSGTR_EdlRecordsNo "��������� �� %d EDL ��������.\n"
#define MSGTR_EdlQueueEmpty "���� EDL ��������, ����� �� ����� ���������.\n"
#define MSGTR_EdlCantOpenForWrite "EDL ����� [%s] �� ���� �� ���� ������� �� �����.\n"
#define MSGTR_EdlCantOpenForRead "EDL ����� [%s] �� ���� �� ���� ������� �� ������.\n"
#define MSGTR_EdlNOsh_video "EDL �� ���� �� �� ������ ��� �����, �������� ��.\n"
#define MSGTR_EdlNOValidLine "��������� ��� � EDL: %s\n"
#define MSGTR_EdlBadlyFormattedLine "��� ���������� EDL ��� [%d] �������� ��.\n"
#define MSGTR_EdlBadLineOverlap "���������� ������� �� ������� ���� [%f]; ���������� �� ������� � "\
"[%f]. ���������� � ������� ������ �� �� � ������������ ���, �� ����� �� �� �����������.\n"
#define MSGTR_EdlBadLineBadStop "������� �� ������� ������ �� � ���� ������� �� �������.\n"

// mencoder.c:

#define MSGTR_UsingPass3ControllFile "�������� �� ���� �� ������� �� pass3: %s\n"
#define MSGTR_MissingFilename "\n������ ��� �� ����.\n\n"
#define MSGTR_CannotOpenFile_Device "�����/������������ �� ���� �� ���� �������.\n"
#define MSGTR_CannotOpenDemuxer "�� ���� �� ���� ������� �������������.\n"
#define MSGTR_NoAudioEncoderSelected "\n�� � ������ ����� ������� (-oac). �������� ��� ������� (����� -oac help) ��� ������� -nosound.\n"
#define MSGTR_NoVideoEncoderSelected "\n�� � ������ ����� ������� (-ovc). �������� �� (����� -ovc help).\n"
#define MSGTR_CannotOpenOutputFile "�������� ���� '%s'�� ���� �� ���� �������.\n"
#define MSGTR_EncoderOpenFailed "��������� �� ���� �� ���� �������.\n"
#define MSGTR_ForcingOutputFourcc "�������� �� �������� fourcc ��� �� ���� %x [%.4s]\n"
#define MSGTR_WritingAVIHeader "����� �� AVI header...\n"
#define MSGTR_DuplicateFrames "\n%d ��������� �� ������!\n"
#define MSGTR_SkipFrame "\n��������� �����!\n"
#define MSGTR_ResolutionDoesntMatch "\n������ ����� ���� ��� �������� ��������� ��� ������� ������ �� ���������.\n" 
#define MSGTR_FrameCopyFileMismatch "\n������ ����� ������� ������ �� ���� ��������� ���������, ������� ������� � ������ �� -ovc copy.\n"
#define MSGTR_AudioCopyFileMismatch "\n������ ������� ������ �� ���� ��������� ����� ������ � ������� �� -oac copy.\n"
#define MSGTR_NoSpeedWithFrameCopy "��������������: -speed �� ������ ����������� �������� � -oac copy!\n"\
"���������� �� ���� �� �� ����� ���������!\n"
#define MSGTR_ErrorWritingFile "%s: ������ ��� ����� �� �����.\n"
#define MSGTR_WritingAVIIndex "\n������� �� AVI ������...\n"
#define MSGTR_FixupAVIHeader "�������� �� AVI header...\n"
#define MSGTR_RecommendedVideoBitrate "�������������� ������� �� %s CD: %d\n"
#define MSGTR_VideoStreamResult "\n����� �����: %8.3f �����/�  (%d bps)  ������: %d �����  %5.3f ���.  %d ������\n"
#define MSGTR_AudioStreamResult "\n����� �����: %8.3f �����/�  (%d bps)  ������: %d �����  %5.3f ���.\n"
#define MSGTR_OpenedStream "�����: ������: %d  �����: 0x%X - 0x%x\n"
#define MSGTR_VCodecFramecopy "videocodec: framecopy (%dx%d %dbpp fourcc=%x)\n"
#define MSGTR_ACodecFramecopy "audiocodec: framecopy (format=%x chans=%d rate=%ld bits=%d bps=%ld sample-%ld)\n"
#define MSGTR_CBRPCMAudioSelected "������� � CBR (��������� �������) PCM �����\n"
#define MSGTR_MP3AudioSelected "������� � MP3 �����\n"
#define MSGTR_CannotAllocateBytes "�� ���� �� �� ������� %d �����\n"
#define MSGTR_SettingAudioDelay "����� ������������ � ��������� �� %5.3f\n"
#define MSGTR_SettingAudioInputGain "����� ���������� � ��������� �� %f\n"
#define MSGTR_LamePresetEquals "\n������=%s\n\n"
#define MSGTR_LimitingAudioPreload "��������������� ����� ��������� � ���������� �� 0.4�\n"
#define MSGTR_IncreasingAudioDensity "��������� �� ����� � ��������� �� 4\n"
#define MSGTR_ZeroingAudioPreloadAndMaxPtsCorrection "�������� �� ������ ������������� ����� ���������, max pts correction to 0\n"
#define MSGTR_CBRAudioByterate "\n\nCBR �����: %ld �����/���, %d ����� �� ����\n"
#define MSGTR_LameVersion "LAME ������ %s (%s)\n\n"
#define MSGTR_InvalidBitrateForLamePreset "������: ��������� ������� � ����� ����������� ������� �� ���� ������\n"\
"\n"\
"������ ���������� ���� ����� ������ �� �������� �������� ����� \"8\" � \"320\"\n"\
"\n"\
"������������ ���������� ���� �� �������� �: \"-lameopts preset=help\"\n"
#define MSGTR_InvalidLamePresetOptions "������: �� ��� ������ ������� ������ �/��� ����� � preset\n"\
"\n"\
"���������� ������� ��:\n"\
"\n"\
"   <fast>        standard\n"\
"   <fast>        extreme\n"\
"                 insane\n"\
"   <cbr> (ABR �����) - �� � ����� ������� �� �������� ABR ������.\n"\
"                      �� �� �� ��������, ������ ������� �������. ��������:\n"\
"                      \"preset=185\" �������� ����\n"\
"                      ������ � ������ ������ 185 �������� � �������.\n"\
"\n"\
"    ������� �������:\n"\
"\n"\
"    \"-lameopts fast:preset=standard  \"\n"\
" ��� \"-lameopts  cbr:preset=192       \"\n"\
" ��� \"-lameopts      preset=172       \"\n"\
" ��� \"-lameopts      preset=extreme   \"\n"\
"\n"\
"������������ ���������� ������ �� �������� �: \"-lameopts preset=help\"\n"
#define MSGTR_LamePresetsLongInfo "\n"\
"��������� �� ��������� �� �� ���������� ���-������� �������� ��������.\n"\
"\n"\
"� ��-�������� �� ���� �� �� ���� ������� �� �������� �������\n"\
"�� �� �� ������� � �������� ���� ��������.\n"\
"\n"\
"������������ �� ���������, �������� ���-������ ����������\n"\
"� ���������� �������� �� �������� �� �� ������� ���-�������\n"\
"�������� ��������� � LAME.\n"\
"\n"\
"�� �� ���������� ���� �������:\n"\
"\n"\
"   �� VBR ������ (���-������ ��������):\n"\
"\n"\
"     \"preset=standard\" T��� ������ � �������� �� ������� ���� � ��������\n"\
"                             ������ ������ � ��������� ����� ������ ��������.\n"\
"\n"\
"     \"preset=extreme\" ��� ����� ������������ ����� ���� � ���������� ��\n"\
"                             ������ ����, ���� ������ �� �������\n"\
"                             ����� ��-����� �������� �� \"standard\"\n"\
"                             ������.\n"\
"\n"\
"   �� CBR 320�����/� (������ � ���-�������� �������� ��������):\n"\
"\n"\
"     \"preset=insane\"  ����������� � ���� ������ �� ��������� �� ��������\n"\
"                             ���� � ��������, �� ��� �� ������\n"\
"                             �� ���������� ��������� ���������� ��������\n"\
"                             ��� �������� �� ������� �� �����, ���� � ������.\n"\
"\n"\
"   �� ABR ����� (������ �������� ��� ������� �������, �� �� ������� ��� VBR):\n"\
"\n"\
"     \"preset=<�����/�>\"  ���� ������ ���������� ���� ����� �������� ��\n"\
"                             ��������� �������. � ���������� �� ��������\n"\
"                             �������, �������� �� �������� ����������� ��\n"\
"                             ������ ���������.\n"\
"                             ������� �� ���� ����� ����� ������, ��� �� �\n"\
"                             ������� ������, ������� VBR, � ���������� ��\n"\
"                             ������� ���������� �� VBR ��� ������ ����������.\n"\
"\n"\
"�������� ����� �� �������� �� ����������� �������:\n"\
"\n"\
"   <fast>        standard\n"\
"   <fast>        extreme\n"\
"                 insane\n"\
"   <cbr> (ABR �����) - �� � ����� ������� �� �������� ABR ������.\n"\
"                      �� �� �� ��������, ������ ������� �������. ��������:\n"\
"                      \"preset=185\" �������� ����\n"\
"                      ������ � ������ ������ 185 �������� � �������.\n"\
"\n"\
"   \"fast\" - ��������� �����, ���� VBR �� ��������� ������. ��������� �� ����\n"\
"            �, �� �� ������ �� ��������� ����� ���������� ������� � ��-�����,\n"\
"            � ���������� ���� ��-����� � �������� � ��������� ����� �� ������.\n"\
"   ��������: � ���������� ������, ���������� � ������ ����� �������, ���� �� ��\n"\
"            ����� ������ �����, � ��������� � ���������� �������.\n"\
"\n"\
"   \"cbr\"  - ��� �������� ABR ����� (��������� ��-����) ��� ����������\n"\
"            ������� ���� 80, 96, 112, 128, 160, 192, 224, 256, 320,\n"\
"            ���� �� �������� ������� \"cbr\" �� �� �������� �������� � CBR\n"\
"            �����, ������ ����������� abr mode. ABR ��������� ��-������\n"\
"            ��������, �� CBR ���� �� � ��-�������� � ��������, ����\n"\
"            ����������� �� mp3 ���� �������� �����.\n"\
"\n"\
"    ��������:\n"\
"\n"\
"    \"-lameopts fast:preset=standard  \"\n"\
" ��� \"-lameopts  cbr:preset=192       \"\n"\
" ��� \"-lameopts      preset=172       \"\n"\
" ��� \"-lameopts      preset=extreme   \"\n"\
"\n"\
"\n"\
"�������� �� ����� ���������� �� ABR �����:\n"\
"phone => 16kbps/����        phon+/lw/mw-eu/sw => 24kbps/����\n"\
"mw-us => 40kbps/����        voice => 56kbps/����\n"\
"fm/radio/tape => 112kbps    hifi => 160kbps\n"\
"cd => 192kbps               studio => 256kbps"
#define MSGTR_LameCantInit "�� ����� �� �� ������� LAME �������, ��������� ������������/��������� �� ����������,"\
"����� ����� ����� ���������� (<32) �������� ����� ������� �� ���������� (����. -srate 8000)."\
"��� ���� ����� �� ������ ��������� ����� preset."
#define MSGTR_ConfigfileError "������ � ���������������� ����"
#define MSGTR_ErrorParsingCommandLine "������ ��� ����������� �� ��������� ���"
#define MSGTR_VideoStreamRequired "������������ � �� ��� ����� �����!\n"
#define MSGTR_ForcingInputFPS "��������� ����� � ������� �� �� ������������� ���� %5.2f\n"
#define MSGTR_RawvideoDoesNotSupportAudio "�������� ������ RAWVIDEO �� �������� ����� - ����� �� ��������\n"
#define MSGTR_DemuxerDoesntSupportNosound "T��� ������������� ��� ��� �� �������� -nosound .\n"
#define MSGTR_MemAllocFailed "�� ���� �� ������ �����"
#define MSGTR_NoMatchingFilter "�� ���� �� ���� ������� �������� ������/������� ����� ������!\n"
#define MSGTR_MP3WaveFormatSizeNot30 "sizeof(MPEGLAYER3WAVEFORMAT)==%d!=30, ���� �� ������ C �����������?\n"
#define MSGTR_NoLavcAudioCodecName "LAVC �����, ������ ��� �� �����!\n"
#define MSGTR_LavcAudioCodecNotFound "A���� LAVC, �� ���� �� �� ������ ������� �� ������ %s\n"
#define MSGTR_CouldntAllocateLavcContext "A���� LAVC, �� ���� �� ������ ��������!\n"
#define MSGTR_CouldntOpenCodec "�� ���� �� ������ ����� %s, br=%d\n"

// cfg-mencoder.h:

#define MSGTR_MEncoderMP3LameHelp "\n\n"\
" vbr=<0-4>     ��������� ������� �����\n"\
"                0: cbr\n"\
"                1: mt\n"\
"                2: rh(���������� ��)\n"\
"                3: abr\n"\
"                4: mtrh\n"\
"\n"\
" abr           ������ �������\n"\
"\n"\
" cbr           ��������� bitrate\n"\
"               ���� ���� ������ CBR �������� �� �������������� ABR ������.\n"\
"\n"\
" br=<0-1024>   ������ �������� � ����� (���� �� CBR � ABR)\n"\
"\n"\
" q=<0-9>       �������� (0-����������, 9-���������) (���� �� VBR)\n"\
"\n"\
" aq=<0-9>      �������� �� ���������� (0-���-�����/�����, 9-���-����/�����)\n"\
"\n"\
" ratio=<1-100> ���������� �� ���������\n"\
"\n"\
" vol=<0-10>    �������� �� ������� ����\n"\
"\n"\
" mode=<0-3>    (��-������������: �����������)\n"\
"                0: stereo\n"\
"                1: joint-������\n"\
"                2: ����������\n"\
"                3: ����\n"\
"\n"\
" padding=<0-2>\n"\
"                0: ���\n"\
"                1: ������\n"\
"                2: ����������\n"\
"\n"\
" fast          ��-����� �������� �� �������������� VBR ������,\n"\
"               ����� ��-����� �������� � ��-������ ����������.\n"\
"\n"\
" preset=<value> ��������� ���-�������� �������� �������� ��� �������� ���������.\n"\
"                 medium: VBR  ��������,  ����� ��������\n"\
"                 (150-180 �����/� �������)\n"\
"                 standard:  VBR ��������, ������ ��������\n"\
"                 (170-210 �����/� �������)\n"\
"                 extreme: VBR ��������, �����-������ ��������\n"\
"                 (200-240 �����/� �������)\n"\
"                 insane:  CBR  ��������, ���-������ ��������\n"\
"                 (320 �����/� �������)\n"\
"                 <8-320>: ABR �������� ��� ������� ������ �������.\n\n"

//codec-cfg.c:
#define MSGTR_DuplicateFourcc "�������� FourCC ���"
#define MSGTR_TooManyFourccs "������ ����� FourCC ���o��/�������..."
#define MSGTR_ParseError "������ ��� ���������"
#define MSGTR_ParseErrorFIDNotNumber "������ ��� ��������� (ID �� ������� �� � �����?)"
#define MSGTR_ParseErrorFIDAliasNotNumber "������ ��� ��������� (ID ���������� �� ������� �� � �����?)"
#define MSGTR_DuplicateFID "��������� ID �� �������"
#define MSGTR_TooManyOut "������ ����� ������� �������..."
#define MSGTR_InvalidCodecName "\n�������(%s) ��� ��������� ���!\n"
#define MSGTR_CodecLacksFourcc "\n�������(%s) ���� FourCC ���/������!\n"
#define MSGTR_CodecLacksDriver "\n�������(%s) ���� �������!\n"
#define MSGTR_CodecNeedsDLL "\n�������(%s) �� ������ �� 'dll'!\n"
#define MSGTR_CodecNeedsOutfmt "\n�������(%s) �� ������ �� 'outfmt'!\n"
#define MSGTR_CantAllocateComment "�� ���� �� �� ������ ����� �� ��������. "
#define MSGTR_GetTokenMaxNotLessThanMAX_NR_TOKEN "get_token(): max >= MAX_MR_TOKEN!"
#define MSGTR_ReadingFile "������ �� %s: "
#define MSGTR_CantOpenFileError "'%s': %s �� ���� �� ���� �������\n"
#define MSGTR_CantGetMemoryForLine "���� ���������� ����� �� 'line': %s\n"
#define MSGTR_CantReallocCodecsp "�� ���� �� ��������� ����� �� '*codecsp': %s\n"
#define MSGTR_CodecNameNotUnique "����� �� ������ '%s' �� � ��������."
#define MSGTR_CantStrdupName "�� ���� �� �� ������� strdup -> 'name': %s\n"
#define MSGTR_CantStrdupInfo "�� ���� �� �� ������� strdup -> 'info': %s\n"
#define MSGTR_CantStrdupDriver "�� ���� �� �� ������� strdup -> 'driver': %s\n"
#define MSGTR_CantStrdupDLL "�� ���� �� �� ������� strdup -> 'dll': %s"
#define MSGTR_AudioVideoCodecTotals "%d ����� & %d ����� ������\n"
#define MSGTR_CodecDefinitionIncorrect "������� �� � ��������� ��������."
#define MSGTR_OutdatedCodecsConf "T��� codecs.conf � ������ ���� � ����������� � ���� ������ �� MPlayer!"

// divx4_vbr.c:
#define MSGTR_OutOfMemory "������������ �����"
#define MSGTR_OverridingTooLowBitrate "��������� ������� � ������������ �� ���� ����.\n"\
"����������� �������� ������� �� ���� ���� � %.0f �����/�. ��������� ��������\n"\
"�� ������.\n"

// fifo.c
#define MSGTR_CannotMakePipe "�� ���� �� �� ������� ��������� ����� (PIPE)!\n"

// m_config.c
#define MSGTR_SaveSlotTooOld "������ ���� save slot � ������ � lvl %d: %d !!!\n"
#define MSGTR_InvalidCfgfileOption "������� %s �� ���� �� �� �������� � ��������������� ����.\n"
#define MSGTR_InvalidCmdlineOption "������� %s �� ���� �� �� ������ �� ��������� ���.\n"
#define MSGTR_InvalidSuboption "������: ������� '%s' ���� �������� '%s'.\n"
#define MSGTR_MissingSuboptionParameter "������: ���������� '%s' �� '%s' ������� ���������!\n"
#define MSGTR_MissingOptionParameter "������: ������� '%s' ������� ���������!\n"
#define MSGTR_OptionListHeader "\n ���                 ���            ���        M���      Global  CL    ����\n\n"
#define MSGTR_TotalOptions "\n����: %d �����\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "CD-ROM ���������� '%s' �� � �������.\n"
#define MSGTR_ErrTrackSelect "������ ��� ����� �� VCD �������."
#define MSGTR_ReadSTDIN "������ �� ����������� ���� (stdin)...\n"
#define MSGTR_UnableOpenURL "URL ������ �� ���� �� ���� �������: %s\n"
#define MSGTR_ConnToServer "���������� � ������ ��� �������: %s\n"
#define MSGTR_FileNotFound "����� �� � �������: '%s'\n"

#define MSGTR_SMBInitError "������������ libsmbclient �� ���� �� ���� ��������������: %d\n"
#define MSGTR_SMBFileNotFound "'%s' �� ���� �� ���� ������� ���� LAN\n"
#define MSGTR_SMBNotCompiled "MPlayer �� � ���������� ��� ��������� �� ������ �� SMB.\n"

#define MSGTR_CantOpenDVD "�� ���� �� ���� �������� DVD ����������: %s\n"
#define MSGTR_DVDwait "������ �� ����������� �� �����, ���� ���������...\n"
#define MSGTR_DVDnumTitles "��� %d �������� �� ���� DVD.\n"
#define MSGTR_DVDinvalidTitle "��������� ����� �� DVD ��������: %d\n"
#define MSGTR_DVDnumChapters "��� %d ������� � ���� DVD ��������.\n"
#define MSGTR_DVDinvalidChapter "��������� ����� �� DVD ������: %d\n"
#define MSGTR_DVDnumAngles "��� %d ������ ����� � ���� DVD ��������..\n"
#define MSGTR_DVDinvalidAngle "��������� ����� �� ������ �����: %d\n"
#define MSGTR_DVDnoIFO "�� ���� �� ���� ������� IFO ����� �� ���� DVD �������� %d.\n"
#define MSGTR_DVDnoVOBs "���������� �� ���� �� ���� �������� (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD � ������� �������.\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "��������������: ���������� ���� �� ����� ������ %d � ������������.\n"
#define MSGTR_VideoStreamRedefined "��������������: ���������� ���� �� ����� ������ %d � ������������.\n"
#define MSGTR_TooManyAudioInBuffer "\nT����� ����� ����� ������ � ������: (%d � %d �����).\n"
#define MSGTR_TooManyVideoInBuffer "\n������ ����� ����� ������ � ������: (%d � %d �����).\n"
#define MSGTR_MaybeNI "���� �� ��������������� non-interleaved �����/���� ��� ������� �� �� � �������?\n" \
		      "�� AVI �������, �������� �� �������� non-interleaved ����� ��� ������� -ni.\n"
#define MSGTR_SwitchToNi "\n��� ������������ AVI ���� - ������������ ��� -ni �����...\n"
#define MSGTR_Detected_XXX_FileFormat "%s ������.\n"
#define MSGTR_DetectedAudiofile "����� ����.\n"
#define MSGTR_NotSystemStream "�� � MPEG System Stream... (���� �� Transport Stream?)\n"
#define MSGTR_InvalidMPEGES "��������� MPEG-ES �����??? �������� �� � ������, ���� �� � ��� :(\n"
#define MSGTR_FormatNotRecognized "============ �� ���������, ���� ������ �� �� ����������/�������� =============\n"\
				  "=== ��� ���� ���� � AVI, ASF ��� MPEG �����, ���� ��������� ������! ===\n"
#define MSGTR_MissingVideoStream "�� � ������ ����� �����.\n"
#define MSGTR_MissingAudioStream "�� � ������ ����� ����� -> ���� ����.\n"
#define MSGTR_MissingVideoStreamBug "������ ����� �����!? �������� �� � ������, ���� �� � ��� :(\n"

#define MSGTR_DoesntContainSelectedStream "�������������: ������ �� ������� �������� ����� ��� ����� �����.\n"

#define MSGTR_NI_Forced "�������"
#define MSGTR_NI_Detected "���������"
#define MSGTR_NI_Message "%s NON-INTERLEAVED AVI ����.\n"

#define MSGTR_UsingNINI "�������� �� NON-INTERLEAVED AVI ������.\n"
#define MSGTR_CouldntDetFNo "�� ���� �� �� �������� ���� �� ������� (�� ����������).\n"
#define MSGTR_CantSeekRawAVI "�� ����� �� �� ��������� ������ AVI ������. (������� �� ������, �������� � -idx .)\n"
#define MSGTR_CantSeekFile "���� ���� �� ���� �� �� ��������.\n"

#define MSGTR_EncryptedVOB "��������� VOB ����! ��������� DOCS/HTML/en/dvd.html.\n"

#define MSGTR_MOVcomprhdr "MOV: ����������� �� ������������ ������ ������� ZLIB!\n"
#define MSGTR_MOVvariableFourCC "MOV: ��������: ������ � ��������� FOURCC ���!?\n"
#define MSGTR_MOVtooManyTrk "MOV: ��������: ������ ����� �������"
#define MSGTR_FoundAudioStream "==> ������ � ����� �����: %d\n"
#define MSGTR_FoundVideoStream "==> ������ � ����� �����: %d\n"
#define MSGTR_DetectedTV "������� � ���������! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "�� ���� �� ���� ������� ogg �������������.\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: ������� �� ������ ����� (id:%d).\n"
#define MSGTR_CannotOpenAudioStream "�� ���� �� �� ������ ������ �����: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "�� ����� �� ����� �������� ��������: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "�� ���� �� ���� ������� ����� �������������: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "�� ���� �� ���� ������� ������������� �� ��������: %s\n"
#define MSGTR_TVInputNotSeekable "����������� �� ���� �� �� ��������! (����� �� �� ����������� ���������� �������� ;)\n"
#define MSGTR_DemuxerInfoAlreadyPresent "������������ �� ������������� %s � ���� ������� !\n"
#define MSGTR_ClipInfo "���������� �� �����:\n"

#define MSGTR_LeaveTelecineMode "\ndemux_mpg: 30000/1001fps NTSC ����������, ������������ �� ��������� �������.\n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: 24000/1001fps ����������� NTSC, ������������ �� ��������� �������.\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "�� ���� �� ���� ������� �����.\n"
#define MSGTR_CantCloseCodec "�� ���� �� ���� �������� �����.\n"

#define MSGTR_MissingDLLcodec "������: ������������ DirectShow ����� %s �� ���� �� ���� �������.\n"
#define MSGTR_ACMiniterror "�� ���� �� �� ������/������������ Win32/ACM ����� ����� (������� DLL ����?).\n"
#define MSGTR_MissingLAVCcodec "�� ���� �� ���� ������ ����� '%s' � libavcodec...\n"

#define MSGTR_MpegNoSequHdr "MPEG: �������: ��������� � ���� �� �����, ��-����� �� ������� �� sequence header.\n"
#define MSGTR_CannotReadMpegSequHdr "�������: �� ���� �� ���� �������� sequence header.\n"
#define MSGTR_CannotReadMpegSequHdrEx "FATAL: �� ���� �� ���� ��������� ������������ �� sequence header.\n"
#define MSGTR_BadMpegSequHdr "MPEG: ��� sequence header\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: ���� ���������� �� sequence header\n"

#define MSGTR_ShMemAllocFail "�� ���� �� �� ������ ��������� �����.\n"
#define MSGTR_CantAllocAudioBuf "�� ���� �� �� ������ ����� �����.\n"

#define MSGTR_UnknownAudio "����������/������� ����� ������ -> ���� ����\n"

#define MSGTR_UsingExternalPP "[PP] ���������� �� ������ ������ �� ������������ ���������, max q = %d.\n"
#define MSGTR_UsingCodecPP "[PP] ���������� �� ������������ ��������� �� ������ �� ������, max q = %d.\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "����� ������� '%s' �� �� �������� �� vo & vd.\n"
#define MSGTR_VideoCodecFamilyNotAvailableStr "��������� ������� ����� ������ [%s] (vfm=%s) �� � ��������.\n��������� � �� ����� �� ����������.\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "��������� ������� ����� ������ [%s] (afm=%s) �� � ��������.\n��������� � �� ����� �� ����������.\n"
#define MSGTR_OpeningVideoDecoder "�������� �� ����� �������: [%s] %s\n"
#define MSGTR_OpeningAudioDecoder "�������� �� ����� �������: [%s] %s\n"
#define MSGTR_UninitVideoStr "uninit video: %s\n"
#define MSGTR_UninitAudioStr "uninit audio: %s\n"
#define MSGTR_VDecoderInitFailed "��������������� �� VDecoder �� ������� :(\n"
#define MSGTR_ADecoderInitFailed "��������������� �� ADecoder �� ������� :(\n"
#define MSGTR_ADecoderPreinitFailed "��������������� ������������� �� ADecoder �� ������� :(\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: �������� �� %d ����� �� ������� �����.\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: �������� �� %d + %d = %d ����� �� �������� �����.\n"

// LIRC:
#define MSGTR_SettingUpLIRC "������������ �� LIRC ���������...\n"
#define MSGTR_LIRCdisabled "���� �� ������ �� �������� �������������� ����������.\n"
#define MSGTR_LIRCopenfailed "���� �� ��� LIRC ���������.\n"
#define MSGTR_LIRCcfgerr "����������������� ���� �� LIRC %s �� ���� �� ���� ��������.\n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "�� ���� �� ���� ������ ����� ������ '%s'.\n"
#define MSGTR_CouldNotOpenVideoFilter "�� ���� �� ���� ������� ����� ������ '%s'.\n"
#define MSGTR_OpeningVideoFilter "�������� �� ����� ������: "
#define MSGTR_CannotFindColorspace "�� ���� �� ���� ������ ������������ ������� ������, ���� � �������� �� 'scale':(\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDec: ������� �� � ������ sh->disp_w � sh->disp_h, ���� �� �������.\n"
#define MSGTR_VoConfigRequest "VDec: ������ �� vo config - %d x %d (preferred csp: %s)\n"
#define MSGTR_CouldNotFindColorspace "�� � ������ �������� ������� ������ - �������� ���� � -vf scale...\n"
#define MSGTR_MovieAspectIsSet "����������� �� ����� �� %.2f:1 - ���������� �� ���������� ��������� .\n"
#define MSGTR_MovieAspectUndefined "�� �� ���������� ��������� - ��� ������������� ����������.\n"

// vd_dshow.c, vd_dmo.c
#define MSGTR_DownloadCodecPackage "������ �� ��������/����������� ������ � ��������� ������.\n������� �� http://mplayerhq.hu/homepage/dload.html\n"
#define MSGTR_DShowInitOK "INFO: ���������� Win32/DShow � ������������� �������.\n"
#define MSGTR_DMOInitOK "INFO: ���������� Win32/DMO � ������������� �������.\n"

// x11_common.c
#define MSGTR_EwmhFullscreenStateFailed "\nX11: �� ���� �� ����� EWMH fullscreen Event!\n"

#define MSGTR_InsertingAfVolume "[��������] ���� ��������� ��������, �������� �� ������ �� ������ �� �����.\n"
#define MSGTR_NoVolume "[��������] �� � �������� ��������� �� �����.\n"

// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "����������"
#define MSGTR_FileSelect "����� �� ����..."
#define MSGTR_SubtitleSelect "����� �� ��������..."
#define MSGTR_OtherSelect "�����..."
#define MSGTR_AudioFileSelect "����� �� ������ ����� �����..."
#define MSGTR_FontSelect "����� �� �����..."
#define MSGTR_PlayList "������ �� ���������������"
#define MSGTR_Equalizer "����������"
#define MSGTR_SkinBrowser "����� �� Skin"
#define MSGTR_Network "����� �� �������..."
#define MSGTR_Preferences "�������������"
#define MSGTR_AudioPreferences "������������� �� ����� ��������"
#define MSGTR_NoMediaOpened "���� �������� ��������."
#define MSGTR_VCDTrack "VCD ����� %d"
#define MSGTR_NoChapter "���� �������"
#define MSGTR_Chapter "������ %d"
#define MSGTR_NoFileLoaded "�� � ������� ����."

// --- buttons ---
#define MSGTR_Ok "OK"
#define MSGTR_Cancel "�����"
#define MSGTR_Add "��������"
#define MSGTR_Remove "����������"
#define MSGTR_Clear "����������"
#define MSGTR_Config "������������"
#define MSGTR_ConfigDriver "������������� �� ��������"
#define MSGTR_Browse "�����"

// --- error messages ---
#define MSGTR_NEMDB "�� ���������, ���� ���������� ����� �� draw buffer."
#define MSGTR_NEMFMR "�� ���������, ���� ���������� ����� �� ������."
#define MSGTR_IDFGCVD "�� ���������, ���� ��������� � GUI ����� �������."
#define MSGTR_NEEDLAVCFAME "�� ���������, �� ������ �� ��������������� �������� �� MPEG\n������� � ������ DXR3/H+ ���������� ��� �����������.\n���� ��������� lavc ��� fame � ������ �� ������������ �� DXR3/H+ ."
#define MSGTR_UNKNOWNWINDOWTYPE "���������� ��� �� �������� ..."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[skin] ������ � ���������������� ���� �� skin-� �� ��� %d: %s"
#define MSGTR_SKIN_WARNING1 "[skin] �������� � ���������������� ���� �� ��� %d:\n������ widget (%s) ��� \"section\" ����� ����"
#define MSGTR_SKIN_WARNING2 "[skin] �������� � ��������������� ���� �� ��� %d:\n������ widget (%s) ��� \"subsection\" ����� ����"
#define MSGTR_SKIN_WARNING3 "[skin] �������� � ���������������� ���� �� ��� %d:\n���� ��������� �� �� �������� �� widget (%s)"
#define MSGTR_SKIN_SkinFileNotFound "[skin] ������ ( %s ) �� � �������.\n"
#define MSGTR_SKIN_BITMAP_16bit  "Bitmap � 16 � ��-����� ���� �� ���� �� �� �������� (%s).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "������ �� � ������� (%s)\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "������ ��� ������ �� BMP (%s)\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "������ ��� ������ �� TGA (%s)\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "������ ��� ������ �� PNG (%s)\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "TGA � RLE ��������� �� �� �������� (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "���������� ��� �� ����� (%s)\n"
#define MSGTR_SKIN_BITMAP_ConvertError "������ ��� ������������� �� 24 ��� 32 ���� (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "���������� ���������: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "������������ �����\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "����������� �� ������ ����� ��������.\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "������ ��� ������ �� � �������.\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "���� � ������������� �� ������ �� � �������.\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "������������� ������������� �� ����� (%s)\n"
#define MSGTR_SKIN_UnknownParameter "���������� ��������� (%s)\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "������ �� � ������� (%s).\n"
#define MSGTR_SKIN_SKINCFG_SelectedSkinNotFound "��������� ���� ( %s ) �� � �������, �� �� ������ 'default'...\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "������ � ��������������� ���� (%s)\n"
#define MSGTR_SKIN_LABEL "�������:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "������� MPlayer"
#define MSGTR_MENU_Open "O�������..."
#define MSGTR_MENU_PlayFile "������� �� ����..."
#define MSGTR_MENU_PlayVCD "������� �� VCD..."
#define MSGTR_MENU_PlayDVD "������� �� DVD..."
#define MSGTR_MENU_PlayURL "������� �� URL..."
#define MSGTR_MENU_LoadSubtitle "��������� �� ��������..."
#define MSGTR_MENU_DropSubtitle "���������� �� ��������..."
#define MSGTR_MENU_LoadExternAudioFile "��������� �� ������ ������ ����..."
#define MSGTR_MENU_Playing "Playing"
#define MSGTR_MENU_Play "�����"
#define MSGTR_MENU_Pause "�����"
#define MSGTR_MENU_Stop "����"
#define MSGTR_MENU_NextStream "�������"
#define MSGTR_MENU_PrevStream "��������"
#define MSGTR_MENU_Size "������"
#define MSGTR_MENU_HalfSize   "������� ������"
#define MSGTR_MENU_NormalSize "�������� ������"
#define MSGTR_MENU_DoubleSize "����� ������"
#define MSGTR_MENU_FullScreen "�� ��� �����"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "O������� �� ����..."
#define MSGTR_MENU_ShowDVDMenu "��������� �� DVD ����"
#define MSGTR_MENU_Titles "��������"
#define MSGTR_MENU_Title "�������� %2d"
#define MSGTR_MENU_None "(����)"
#define MSGTR_MENU_Chapters "�������"
#define MSGTR_MENU_Chapter "������ %2d"
#define MSGTR_MENU_AudioLanguages "���� �� �����"
#define MSGTR_MENU_SubtitleLanguages "���� �� ����������"
#define MSGTR_MENU_PlayList "Playlist"
#define MSGTR_MENU_SkinBrowser "����� �� Skin"
#define MSGTR_MENU_Preferences "���������"
#define MSGTR_MENU_Exit "�����..."
#define MSGTR_MENU_Mute "��� ����"
#define MSGTR_MENU_Original "��� �������"
#define MSGTR_MENU_AspectRatio "�����������"
#define MSGTR_MENU_AudioTrack "����� �����"
#define MSGTR_MENU_Track "����� %d"
#define MSGTR_MENU_VideoTrack "����� �����"

// --- equalizer
#define MSGTR_EQU_Audio "�����"
#define MSGTR_EQU_Video "�����"
#define MSGTR_EQU_Contrast "��������: "
#define MSGTR_EQU_Brightness "��������: "
#define MSGTR_EQU_Hue "���: "
#define MSGTR_EQU_Saturation "����������: "
#define MSGTR_EQU_Front_Left "������ ���"
#define MSGTR_EQU_Front_Right "������ �����"
#define MSGTR_EQU_Back_Left "����� ���"
#define MSGTR_EQU_Back_Right "����� �����"
#define MSGTR_EQU_Center "���������"
#define MSGTR_EQU_Bass "���"
#define MSGTR_EQU_All "������"
#define MSGTR_EQU_Channel1 "����� 1:"
#define MSGTR_EQU_Channel2 "����� 2:"
#define MSGTR_EQU_Channel3 "����� 3:"
#define MSGTR_EQU_Channel4 "����� 4:"
#define MSGTR_EQU_Channel5 "����� 5:"
#define MSGTR_EQU_Channel6 "����� 6:"

// --- playlist
#define MSGTR_PLAYLIST_Path "���"
#define MSGTR_PLAYLIST_Selected "������� �������"
#define MSGTR_PLAYLIST_Files "�������"
#define MSGTR_PLAYLIST_DirectoryTree "����������"

// --- preferences
#define MSGTR_PREFERENCES_Audio "�����"
#define MSGTR_PREFERENCES_Video "�����"
#define MSGTR_PREFERENCES_SubtitleOSD "�������� � OSD"
#define MSGTR_PREFERENCES_Codecs "������ & demuxer"
#define MSGTR_PREFERENCES_Misc "�����"

#define MSGTR_PREFERENCES_None "���"
#define MSGTR_PREFERENCES_DriverDefault "������������ �� �� ��������"
#define MSGTR_PREFERENCES_AvailableDrivers "�������� ��������:"
#define MSGTR_PREFERENCES_DoNotPlaySound "��� ����"
#define MSGTR_PREFERENCES_NormalizeSound "����������� �� �����"
#define MSGTR_PREFERENCES_EnEqualizer "��������� �� �����������"
#define MSGTR_PREFERENCES_SoftwareMixer "������� ��������� ��������"
#define MSGTR_PREFERENCES_ExtraStereo "��������� �� ������������ ������"
#define MSGTR_PREFERENCES_Coefficient "����������:"
#define MSGTR_PREFERENCES_AudioDelay "���������� �� �����"
#define MSGTR_PREFERENCES_DoubleBuffer "������ ����������"
#define MSGTR_PREFERENCES_DirectRender "��������� �� direct rendering"
#define MSGTR_PREFERENCES_FrameDrop "����������� �� ������������ �� �����"
#define MSGTR_PREFERENCES_HFrameDrop "����������� �� ���������� ���������� �� ����� (������)"
#define MSGTR_PREFERENCES_Flip "����������� �� ������"
#define MSGTR_PREFERENCES_Panscan "Panscan: "
#define MSGTR_PREFERENCES_OSDTimer "�������� � ����������"
#define MSGTR_PREFERENCES_OSDProgress "���� ���������� �� ����������"
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "��������, �������� � ���� �����"
#define MSGTR_PREFERENCES_Subtitle "��������:"
#define MSGTR_PREFERENCES_SUB_Delay "����������: "
#define MSGTR_PREFERENCES_SUB_FPS "FPS:"
#define MSGTR_PREFERENCES_SUB_POS "��������������: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "���������� �� ������������� ��������� �� ��������"
#define MSGTR_PREFERENCES_SUB_Unicode "�������� � Unicode ��������"
#define MSGTR_PREFERENCES_SUB_MPSUB "������������� �� ���������� � ������� �� MPlayer"
#define MSGTR_PREFERENCES_SUB_SRT "������������� �� ���������� � SubViewer (SRT) ������"
#define MSGTR_PREFERENCES_SUB_Overlap "������������ �� ����������"
#define MSGTR_PREFERENCES_Font "�����:"
#define MSGTR_PREFERENCES_FontFactor "�������� �� ������� �� ������:"
#define MSGTR_PREFERENCES_PostProcess "����������� �� ������������ ���������"
#define MSGTR_PREFERENCES_AutoQuality "����������� ������� �� ����������: "
#define MSGTR_PREFERENCES_NI "��������� �� non-interleaved AVI ������"
#define MSGTR_PREFERENCES_IDX "����������� �� ���������� ������� ������, ��� ������������"
#define MSGTR_PREFERENCES_VideoCodecFamily "������� ����� ������:"
#define MSGTR_PREFERENCES_AudioCodecFamily "������� ����� ������:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "OSD ������"
#define MSGTR_PREFERENCES_FRAME_Subtitle "��������"
#define MSGTR_PREFERENCES_FRAME_Font "�����"
#define MSGTR_PREFERENCES_FRAME_PostProcess "������������ ���������"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "����� & �������������"
#define MSGTR_PREFERENCES_FRAME_Cache "��������"
#define MSGTR_PREFERENCES_FRAME_Misc "�����"
#define MSGTR_PREFERENCES_Audio_Device "����������:"
#define MSGTR_PREFERENCES_Audio_Mixer "��������:"
#define MSGTR_PREFERENCES_Audio_MixerChannel "����� �� ���������:"
#define MSGTR_PREFERENCES_Message "�� ����������, �� ������������ ����������������� �� �� ������ � ���� ����� �����!"
#define MSGTR_PREFERENCES_DXR3_VENC "����� �������:"
#define MSGTR_PREFERENCES_DXR3_LAVC "���������� �� LAVC (FFmpeg)"
#define MSGTR_PREFERENCES_DXR3_FAME "���������� �� FAME"
#define MSGTR_PREFERENCES_FontEncoding1 "Unicode"
#define MSGTR_PREFERENCES_FontEncoding2 "����������������� ����� (ISO-8859-1)"
#define MSGTR_PREFERENCES_FontEncoding3 "����������������� ����� ��� Euro (ISO-8859-15)"
#define MSGTR_PREFERENCES_FontEncoding4 "���������/������������������� ����� (ISO-8859-2)"
#define MSGTR_PREFERENCES_FontEncoding5 "���������, ������, ���������, ������ (ISO-8859-3)"
#define MSGTR_PREFERENCES_FontEncoding6 "���� ��������� (ISO-8859-4)"
#define MSGTR_PREFERENCES_FontEncoding7 "�������� (ISO-8859-5)"
#define MSGTR_PREFERENCES_FontEncoding8 "������� (ISO-8859-6)"
#define MSGTR_PREFERENCES_FontEncoding9 "���������� ������ (ISO-8859-7)"
#define MSGTR_PREFERENCES_FontEncoding10 "������ (ISO-8859-9)"
#define MSGTR_PREFERENCES_FontEncoding11 "��������� (ISO-8859-13)"
#define MSGTR_PREFERENCES_FontEncoding12 "������� (ISO-8859-14)"
#define MSGTR_PREFERENCES_FontEncoding13 "Hebrew charsets (ISO-8859-8)"
#define MSGTR_PREFERENCES_FontEncoding14 "����� (KOI8-R)"
#define MSGTR_PREFERENCES_FontEncoding15 "���������, ��������� (KOI8-U/RU)"
#define MSGTR_PREFERENCES_FontEncoding16 "�������� �������� (CP936)"
#define MSGTR_PREFERENCES_FontEncoding17 "����������� �������� (BIG5)"
#define MSGTR_PREFERENCES_FontEncoding18 "������� (SHIFT-JIS)"
#define MSGTR_PREFERENCES_FontEncoding19 "K������� (CP949)"
#define MSGTR_PREFERENCES_FontEncoding20 "Thai charset (CP874)"
#define MSGTR_PREFERENCES_FontEncoding21 "�������� Windows (CP1251)"
#define MSGTR_PREFERENCES_FontEncoding22 "���������/������������������� Windows (CP1250)"
#define MSGTR_PREFERENCES_FontNoAutoScale "��� ����������� ����������"
#define MSGTR_PREFERENCES_FontPropWidth "�������������� �� ���������� �� �����"
#define MSGTR_PREFERENCES_FontPropHeight "�������������� �� ���������� �� �����"
#define MSGTR_PREFERENCES_FontPropDiagonal "�������������� �� ��������� �� ���������"
#define MSGTR_PREFERENCES_FontEncoding "���������:"
#define MSGTR_PREFERENCES_FontBlur "����������:"
#define MSGTR_PREFERENCES_FontOutLine "�����������:"
#define MSGTR_PREFERENCES_FontTextScale "����� �� ������:"
#define MSGTR_PREFERENCES_FontOSDScale "����� �� OSD:"
#define MSGTR_PREFERENCES_Cache "��������"
#define MSGTR_PREFERENCES_CacheSize "������ �� ����: "
#define MSGTR_PREFERENCES_LoadFullscreen "���������� �� ��� �����"
#define MSGTR_PREFERENCES_SaveWinPos "������������ �� ���������������� �� ���������"
#define MSGTR_PREFERENCES_XSCREENSAVER "���������� �� XScreenSaver"
#define MSGTR_PREFERENCES_PlayBar "����� �� ����������"
#define MSGTR_PREFERENCES_AutoSync "����������� �������������"
#define MSGTR_PREFERENCES_AutoSyncValue "������ �� ���������������: "
#define MSGTR_PREFERENCES_CDROMDevice "CD-ROM ����������:"
#define MSGTR_PREFERENCES_DVDDevice "DVD ����������:"
#define MSGTR_PREFERENCES_FPS "����� � �������:"
#define MSGTR_PREFERENCES_ShowVideoWindow "��������� �� ����� ��������� ��� �����������"

#define MSGTR_ABOUT_UHU "������������ �� ��������� ��������� �� ���������� �� UHU Linux\n"
#define MSGTR_ABOUT_CoreTeam "   MPlayer ������� ������������:\n"
#define MSGTR_ABOUT_AdditionalCoders "   ������������ �����������:\n"
#define MSGTR_ABOUT_MainTesters "   ������� �������:\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "������� ������!"
#define MSGTR_MSGBOX_LABEL_Error "������!"
#define MSGTR_MSGBOX_LABEL_Warning "��������!"

// bitmap.c

#define MSGTR_NotEnoughMemoryC32To1 "[c32to1] ������������ ����� �� �������������\n"
#define MSGTR_NotEnoughMemoryC1To32 "[c1to32] ������������ ����� �� �������������\n"

// cfg.c

#define MSGTR_ConfigFileReadError "[cfg] ������ ��� ������ �� ���������������� ���� ...\n"
#define MSGTR_UnableToSaveOption "�� ���� �� �� �������� ������� '%s'.\n"

// interface.c

#define MSGTR_DeletingSubtitles "[GUI] ��������� �� ����������.\n"
#define MSGTR_LoadingSubtitles "[GUI] ��������� �� ����������: %s\n"
#define MSGTR_AddingVideoFilter "[GUI] �������� �� ����� ������: %s\n"
#define MSGTR_RemovingVideoFilter "[GUI] ���������� �� ����� ������: %s\n"

// mw.c

#define MSGTR_NotAFile "���� �� ������� �� ����: %s !\n"

// ws.c

#define MSGTR_WS_CouldNotOpenDisplay "[ws] �� ���� �� ���� ������� DISPLAY.\n"
#define MSGTR_WS_RemoteDisplay "[ws] ��������� �������, ���������� ��  XMITSHM.\n"
#define MSGTR_WS_NoXshm "[ws] �� ��������� ������ ������� �� �������� ������������ �� X �� ��������� �����.\n"
#define MSGTR_WS_NoXshape "[ws] �� ��������� ������ ������� �� �������� ������������ XShape.\n"
#define MSGTR_WS_ColorDepthTooLow "[ws] ������ ����� ��������� �� ���������.\n"
#define MSGTR_WS_TooManyOpenWindows "[ws] ������ ����� �������� ��������.\n"
#define MSGTR_WS_ShmError "[ws] ������ � ������������ �� ��������� �����\n"
#define MSGTR_WS_NotEnoughMemoryDrawBuffer "[ws] ���� ���������� ����� �� draw buffer.\n"
#define MSGTR_WS_DpmsUnavailable "DPMS �� � ��������?\n"
#define MSGTR_WS_DpmsNotEnabled "DPMS �� ���� �� ���� �������.\n"

// wsxdnd.c

#define MSGTR_WS_NotAFile "���� �� ������� �� ����...\n"
#define MSGTR_WS_DDNothing "D&D: �� � ������ ��������!\n"

#endif

// ======================= VO Video Output drivers ========================

#define MSGTR_VOincompCodec "��������� ������� ����� ���������� � ������������ � ���� �����.\n"
#define MSGTR_VO_GenericError "T��� ������ � ����������"
#define MSGTR_VO_UnableToAccess "�������� � ����������"
#define MSGTR_VO_ExistsButNoDirectory "���� ����������, �� �� � ����������."
#define MSGTR_VO_DirExistsButNotWritable "������������ ����������, �� �� � �������� �����."
#define MSGTR_VO_DirExistsAndIsWritable "������������ ���������� � � ��������� �� �����."
#define MSGTR_VO_CantCreateDirectory "������������ �� ���� �� ���� ���������."
#define MSGTR_VO_CantCreateFile "������ �� ���� �� ���� ��������."
#define MSGTR_VO_DirectoryCreateSuccess "������������ � ������� ���������."
#define MSGTR_VO_ParsingSuboptions "��������� �� ����������."
#define MSGTR_VO_SuboptionsParsedOK "������� ����������� �� ����������."
#define MSGTR_VO_ValueOutOfRange "���������� � ����� ����������� �������"
#define MSGTR_VO_NoValueSpecified "�� � ������� ��������."
#define MSGTR_VO_UnknownSuboptions "���������� ��������(�)"

// vo_aa.c

#define MSGTR_VO_AA_HelpHeader "\n\n���� �� ���������� �� aalib vo_aa:\n"
#define MSGTR_VO_AA_AdditionalOptions "������������ ����� ���������� �� vo_aa:\n" \
"  help        ������� ���� ���������\n" \
"  osdcolor    ������ ���� �� osd\n  subcolor    ������ ����� �� ����������\n" \
"        ����������� �� ���� ��:\n           0 : ��������\n" \
"           1 : dim\n           2 : ��������\n           3 : �������� �����\n" \
"           4 : �������\n           5 : ���������\n\n\n" 


// vo_jpeg.c
#define MSGTR_VO_JPEG_ProgressiveJPEG "������� � progressive JPEG ������."
#define MSGTR_VO_JPEG_NoProgressiveJPEG "Progressive JPEG �������� � ��������."
#define MSGTR_VO_JPEG_BaselineJPEG "������� � baseline JPEG ������."
#define MSGTR_VO_JPEG_NoBaselineJPEG "Baseline JPEG �������� � ��������."

// vo_pnm.c
#define MSGTR_VO_PNM_ASCIIMode "������� � ASCII �����."
#define MSGTR_VO_PNM_RawMode "������� � \"�����\" �����."
#define MSGTR_VO_PNM_PPMType "�� ������� � PPM �������."
#define MSGTR_VO_PNM_PGMType "�� ������� � PGM �������."
#define MSGTR_VO_PNM_PGMYUVType "�� ������� � PGMYUV �������."

// vo_yuv4mpeg.c
#define MSGTR_VO_YUV4MPEG_InterlacedHeightDivisibleBy4 "������� interlaced ������� ���������� �� ������ �� � ������ ��  4."
#define MSGTR_VO_YUV4MPEG_InterlacedLineBufAllocFail "�� ���� �� �� ������ ����� �� �������� �� interlaced �����."
#define MSGTR_VO_YUV4MPEG_InterlacedInputNotRGB "������� ������ �� � RGB, �� ����� �� �� ������� ��������� ������!"
#define MSGTR_VO_YUV4MPEG_WidthDivisibleBy2 "���������� �� ������ ������ �� � ������ �� 2."
#define MSGTR_VO_YUV4MPEG_NoMemRGBFrameBuf "���� ���������� ����� �� RGB ������ �����."
#define MSGTR_VO_YUV4MPEG_OutFileOpenError "�� � �������� ����� ��� ������ ����������� �� ����� \"%s\"!"
#define MSGTR_VO_YUV4MPEG_OutFileWriteError "������ ��� ��������� �� �������������!"
#define MSGTR_VO_YUV4MPEG_UnknownSubDev "���������� �������������: %s"
#define MSGTR_VO_YUV4MPEG_InterlacedTFFMode "���������� �� interlaced ������� �����, �� ���� �� ����."
#define MSGTR_VO_YUV4MPEG_InterlacedBFFMode "���������� �� interlaced ������� �����, �� ���� �� ����."
#define MSGTR_VO_YUV4MPEG_ProgressiveMode "�������� �� (����������� ��) ����������� �����"

// Old vo drivers that have been replaced

#define MSGTR_VO_PGM_HasBeenReplaced "pgm ����� �������� � ������� �� -vo pnm:pgmyuv.\n"
#define MSGTR_VO_MD5_HasBeenReplaced "md5 ����� �������� � ������� �� -vo md5sum.\n"

// ======================= AO Audio Output drivers ========================

// libao2

// audio_out.c
#define MSGTR_AO_ALSA9_1x_Removed "audio_out: �������� alsa9 � alsa1x �� ����������, ����������� -ao alsa .\n"

// ao_oss.c
#define MSGTR_AO_OSS_CantOpenMixer "[AO OSS] audio_setup: �� ���� �� ������ ���������� �������� %s: %s\n"
#define MSGTR_AO_OSS_ChanNotFound "[AO OSS] audio_setup:\n���������� �� ��������� ����� ���� ����� '%s', �������� �� ����������� ��.\n"
#define MSGTR_AO_OSS_CantOpenDev "[AO OSS] audio_setup: ����� ���������� %s �� ���� �� ���� ��������: %s\n"
#define MSGTR_AO_OSS_CantMakeFd "[AO OSS] audio_setup: �� ���� �� ���� �������� ������ ����������: %s\n"
//#define MSGTR_AO_OSS_CantSetAC3 "[AO OSS] �� ���� �� �� ������ �� ���������� %s ������ AC3, ���� � S16...\n"
#define MSGTR_AO_OSS_CantSet "[AO OSS] ����� ���������� %s �� ���� �� ���� ��������� �� %s ���������, ����� � %s...\n"
#define MSGTR_AO_OSS_CantSetChans "[AO OSS] audio_setup: �� ���� �� ������� ��������� ����� �� %d ������.\n"
#define MSGTR_AO_OSS_CantUseGetospace "[AO OSS] audio_setup: ��������� �� �������� SNDCTL_DSP_GETOSPACE :-(\n"
#define MSGTR_AO_OSS_CantUseSelect "[AO OSS]\n   ***  ������ ����� ������� �� �������� ��������� select()  ***\n �������������� MPlayer � #undef HAVE_AUDIO_SELECT � config.h !\n\n"
#define MSGTR_AO_OSS_CantReopen "[AO OSS] ������� ������:\n *** �� ���� �� ���-������/���������� ����� ������������ *** %s\n"

// ao_arts.c
#define MSGTR_AO_ARTS_CantInit "[AO ARTS] %s\n"
#define MSGTR_AO_ARTS_ServerConnect "[AO ARTS] ���������� � ������ ��� ����� �������.\n"
#define MSGTR_AO_ARTS_CantOpenStream "[AO ARTS] ������� �� ���� �� ���� �������.\n"
#define MSGTR_AO_ARTS_StreamOpen "[AO ARTS] ������� � �������.\n"
#define MSGTR_AO_ARTS_BufferSize "[AO ARTS] ������ �� ������: %d\n"

// ao_dxr2.c
#define MSGTR_AO_DXR2_SetVolFailed "[AO DXR2] ������ �� ����� �� ���� �� ���� ������� �� %d.\n"
#define MSGTR_AO_DXR2_UnsupSamplerate "[AO DXR2] dxr2: %d Hz �� �� ���������, �������� \"-aop list=resample\"\n"

// ao_esd.c
#define MSGTR_AO_ESD_CantOpenSound "[AO ESD] esd_open_sound �� �������: %s\n"
#define MSGTR_AO_ESD_LatencyInfo "[AO ESD] ����������: [������: %0.2f�, �����: %0.2f�] (��������� %0.2f�)\n"
#define MSGTR_AO_ESD_CantOpenPBStream "[AO ESD] �� ���� �� ���� ������� esd ����� �� ���������������: %s\n"

// ao_mpegpes.c
#define MSGTR_AO_MPEGPES_CantSetMixer "[AO MPEGPES] DVB audio set mixer �� �������: %s\n"
#define MSGTR_AO_MPEGPES_UnsupSamplerate "[AO MPEGPES] %d Hz �� �� ���������, �������� � resample...\n"

// ao_null.c
// This one desn't even  have any mp_msg nor printf's?? [CHECK]

// ao_pcm.c
#define MSGTR_AO_PCM_FileInfo "[AO PCM] File: %s (%s)\nPCM: �������: %iHz ������: %s ������ %s\n"
#define MSGTR_AO_PCM_HintInfo "[AO PCM] Info: ���-����� ��������� �� ������� � -vc dummy -vo null\nPCM: Info: �� �� �������� WAVE ������� ��������� -waveheader (���������� ��).\n"
#define MSGTR_AO_PCM_CantOpenOutputFile "[AO PCM] %s �� ���� �� �� ������ �� �����!\n"

// ao_sdl.c
#define MSGTR_AO_SDL_INFO "[AO SDL] �������: %iHz ������: %s ������ %s\n"
#define MSGTR_AO_SDL_DriverInfo "[AO SDL] �������� �� %s ����� �������.\n"
#define MSGTR_AO_SDL_UnsupportedAudioFmt "[AO SDL] ����������� ����� ������: 0x%x.\n"
#define MSGTR_AO_SDL_CantInit "[AO SDL] ��������������� �� SDL ����� �� �������: %s\n"
#define MSGTR_AO_SDL_CantOpenAudio "[AO SDL] ������� �� ���� �� �� ������: %s\n"

// ao_sgi.c
#define MSGTR_AO_SGI_INFO "[AO SGI] �������.\n"
#define MSGTR_AO_SGI_InitInfo "[AO SGI] init: �������: %iHz ������: %s ������ %s\n"
#define MSGTR_AO_SGI_InvalidDevice "[AO SGI] play: ��������� ����������.\n"
#define MSGTR_AO_SGI_CantSetParms_Samplerate "[AO SGI] init: setparams �� �������: %s\n�� ���� �� �� ������ ����������� �������.\n"
#define MSGTR_AO_SGI_CantSetAlRate "[AO SGI] init: AL_RATE �� �� ��������� �� ���������� ����������.\n"
#define MSGTR_AO_SGI_CantGetParms "[AO SGI] init: getparams �� �������: %s\n"
#define MSGTR_AO_SGI_SampleRateInfo "[AO SGI] init: ��������� �� ������������� � %lf (����������� ������� � %lf)\n"
#define MSGTR_AO_SGI_InitConfigError "[AO SGI] init: %s\n"
#define MSGTR_AO_SGI_InitOpenAudioFailed "[AO SGI] init: �� ���� �� ���� ������� ����� �����: %s\n"
#define MSGTR_AO_SGI_Uninit "[AO SGI] uninit: ...\n"
#define MSGTR_AO_SGI_Reset "[AO SGI] reset: ...\n"
#define MSGTR_AO_SGI_PauseInfo "[AO SGI] audio_pause: ...\n"
#define MSGTR_AO_SGI_ResumeInfo "[AO SGI] audio_resume: ...\n"

// ao_sun.c
#define MSGTR_AO_SUN_RtscSetinfoFailed "[AO SUN] rtsc: SETINFO �� �������.\n"
#define MSGTR_AO_SUN_RtscWriteFailed "[AO SUN] rtsc: ������ �� ������."
#define MSGTR_AO_SUN_CantOpenAudioDev "[AO SUN] �� ���� �� ���� �������� ���������� %s, %s  -> ��� ����.\n"
#define MSGTR_AO_SUN_UnsupSampleRate "[AO SUN] audio_setup: ������ ������� ����� �� �������� %d �����, %s, %d Hz �������.\n"
#define MSGTR_AO_SUN_CantUseSelect "[AO SUN]\n   ***  ������ ����� ������� �� �������� ��������� select()  ***\n�������������� MPlayer � #undef HAVE_AUDIO_SELECT � config.h !\n\n"
#define MSGTR_AO_SUN_CantReopenReset "[AO SUN]������� ������:\n *** ����� ������������ (%s) �� ���� �� ���� ���-��������/������������ ***\n"

// ao_alsa5.c
#define MSGTR_AO_ALSA5_InitInfo "[AO ALSA5] alsa-init: ������ ������: %d Hz, %d ������, %s\n"
#define MSGTR_AO_ALSA5_SoundCardNotFound "[AO ALSA5] alsa-init: �� �� ������� ������� �����.\n"
#define MSGTR_AO_ALSA5_InvalidFormatReq "[AO ALSA5] alsa-init: ������ � ��������� ������ (%s) - ���������.\n"
#define MSGTR_AO_ALSA5_PlayBackError "[AO ALSA5] alsa-init: ������ ��� �������� �� ���������������: %s\n"
#define MSGTR_AO_ALSA5_PcmInfoError "[AO ALSA5] alsa-init: pcm info ������: %s\n"
#define MSGTR_AO_ALSA5_SoundcardsFound "[AO ALSA5] alsa-init: %d ������� ����� �� �������, ������ ��: %s\n"
#define MSGTR_AO_ALSA5_PcmChanInfoError "[AO ALSA5] alsa-init: pcm channel info ������: %s\n"
#define MSGTR_AO_ALSA5_CantSetParms "[AO ALSA5] alsa-init: ������ ��� ����������� �� �����������: %s\n"
#define MSGTR_AO_ALSA5_CantSetChan "[AO ALSA5] alsa-init: ������ ��� ��������� �� �����: %s\n"
#define MSGTR_AO_ALSA5_ChanPrepareError "[AO ALSA5] alsa-init: ������ ��� ���������� �� �����: %s\n"
#define MSGTR_AO_ALSA5_DrainError "[AO ALSA5] alsa-uninit: ������ ��� ���������� ������ �� ���������������: %s\n"
#define MSGTR_AO_ALSA5_FlushError "[AO ALSA5] alsa-uninit: ������ ��� �������������� �� �������� �� ���������������: %s\n"
#define MSGTR_AO_ALSA5_PcmCloseError "[AO ALSA5] alsa-uninit: ������ ��� ��������� �� pcm: %s\n"
#define MSGTR_AO_ALSA5_ResetDrainError "[AO ALSA5] alsa-reset: ������ ��� ���������� �� ������ �� ���������������: %s\n"
#define MSGTR_AO_ALSA5_ResetFlushError "[AO ALSA5] alsa-reset: ������ ��� �������������� �� �������� �� ���������������: %s\n"
#define MSGTR_AO_ALSA5_ResetChanPrepareError "[AO ALSA5] alsa-reset: ������ ��� ���������� �� �����: %s\n"
#define MSGTR_AO_ALSA5_PauseDrainError "[AO ALSA5] alsa-pause: ������ ��� ���������� �� ������ �� ���������������: %s\n"
#define MSGTR_AO_ALSA5_PauseFlushError "[AO ALSA5] alsa-pause: ������ ��� �������������� �� �������� �� ���������������: %s\n"
#define MSGTR_AO_ALSA5_ResumePrepareError "[AO ALSA5] alsa-resume: ������ ��� ���������� �� �����: %s\n"
#define MSGTR_AO_ALSA5_Underrun "[AO ALSA5] alsa-play: ������������ �� alsa, ������������ �� ������.\n"
#define MSGTR_AO_ALSA5_PlaybackPrepareError "[AO ALSA5] alsa-play: ������ ��� ���������� �� ���������������: %s\n"
#define MSGTR_AO_ALSA5_WriteErrorAfterReset "[AO ALSA5] alsa-play: ������ ��� ����� ���� ������������: %s - ����� �� ����������.\n"
#define MSGTR_AO_ALSA5_OutPutError "[AO ALSA5] alsa-play: ������ �� ������: %s\n"

// ao_plugin.c

#define MSGTR_AO_PLUGIN_InvalidPlugin "[AO PLUGIN] ��������� ������: %s\n"

// ======================= AF Audio Filters ================================

// libaf

// af_ladspa.c

#define MSGTR_AF_LADSPA_AvailableLabels "�������� ������� �"
#define MSGTR_AF_LADSPA_WarnNoInputs "��������! ���� LADSPA ������ �� ������ �����.\n  ������������ ����� ������ �� ���� �������."
#define MSGTR_AF_LADSPA_ErrMultiChannel "������������ (>2) ������� �� �� ��������� (��� ���).\n  ����������� ���� ���� � ������ �������."
#define MSGTR_AF_LADSPA_ErrNoOutputs "���� LADSPA ������ �� ������� ����."
#define MSGTR_AF_LADSPA_ErrInOutDiff "���� �� ����� ��������� �� ���� LADSPA ������ �� ��������� �� ���� �� ����� ��������."
#define MSGTR_AF_LADSPA_ErrFailedToLoad "�� ���� �� �� ������"
#define MSGTR_AF_LADSPA_ErrNoDescriptor "��������� ladspa_descriptor() �� ���� �� ���� ������� � �������� ����������� ����."
#define MSGTR_AF_LADSPA_ErrLabelNotFound "������� �� ���� �� ���� ������� � ������������."
#define MSGTR_AF_LADSPA_ErrNoSuboptions "�� �� ������� ��������"
#define MSGTR_AF_LADSPA_ErrNoLibFile "�� � ������ ���� � ����������"
#define MSGTR_AF_LADSPA_ErrNoLabel "�� � ������ ������ �� ������"
#define MSGTR_AF_LADSPA_ErrNotEnoughControls "�� �� ������� ���������� �������� �� ��������� ���"
#define MSGTR_AF_LADSPA_ErrControlBelow "%s: Input control #%d � ��� ������� ������� �� %0.4f.\n"
#define MSGTR_AF_LADSPA_ErrControlAbove "%s: Input control #%d � ��� ������� ������� �� %0.4f.\n"

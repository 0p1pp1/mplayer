// Translated by: Nick Kurshev <nickols_k@mail.ru>,
// Dmitry Baryshkov <mitya@school.ioffe.ru>
// Reworked by: Andrew Savchenko aka Bircoph <Bircoph[at]list[dot]ru>

// Synced with help_mp-en.h: r20440

// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"�������������:   mplayer [�����] [URL|����/]���_�����\n"
"\n"
"������� �����: (������ ������ ��. �� man-��������)\n"
" -vo <drv[:dev]> ����� �������� � ���������� ����������� (������ ��. � '-vo help')\n"
" -ao <drv[:dev]> ����� �������� � ���������� ����������� (������ ��. � '-ao help')\n"
#ifdef HAVE_VCD
" vcd://<����� �����> ������ ������� (S)VCD (Super Video CD) (���������� ����������,\n                 �� ���������� ���)\n"
#endif
#ifdef USE_DVDREAD
" dvd://<����� ������> ������ DVD ����� � ���������� ������ �����\n"
" -alang/-slang   ������� ���� �����/��������� DVD (������������ ��� ������)\n"
#endif
" -ss <�����>     ������������� �� �������� (������� ��� ��:��:��) �������\n"
" -nosound        ��� �����\n"
" -fs             ����� �������������� ������������ (��� -vm, -zoom, �����������\n                 �� man-��������)\n"
" -x <x> -y <y>   ���������� ���������� ������� (������������ � -vm ��� -zoom)\n"
" -sub <����>     ������� ���� ��������� (��. ����� -subfps, -subdelay)\n"
" -playlist <����> ������� ������ ��������������� (��������)\n"
" -vid x -aid y   ����� ��� ������ ����� (x) � ����� (y) ������ ��� ���������������\n"
" -fps x -srate y ����� ��� ��������� ������� ����� (x, ����/���) � ����� (y, ��)\n"
" -pp <quality>   ��������� ������ ������������� (����������� �� man-��������)\n"
" -framedrop      �������� ������������ ������ (��� ��������� �����)\n"
"\n"
"�������� ������: (������ ������ � �������� man, ����� ��. input.conf)\n"
" <- ��� ->       ����������� ���ң�/����� �� 10 ������\n"
" up ��� down     ����������� ���ң�/����� ��  1 ������\n"
" pgup or pgdown  ����������� ���ң�/����� �� 10 �����\n"
" < ��� >         ����������� ���ң�/����� � ������ ���������������\n"
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

// libmpcodecs/ad_dvdpcm.c:
#define MSGTR_SamplesWanted "��� ��������� ��������� ���������� ������� ����� �������.\n����������, ��������� � ��������������.\n"

// ========================= MPlayer messages ===========================

// mplayer.c:

#define MSGTR_Exiting "\n�������...\n"
#define MSGTR_ExitingHow "\n�������... (%s)\n"
#define MSGTR_Exit_quit "�����"
#define MSGTR_Exit_eof "����� �����"
#define MSGTR_Exit_error "��������� ������"
#define MSGTR_IntBySignal "\nMPlayer ������� �������� %d � ������: %s \n"
#define MSGTR_NoHomeDir "�� ���� ����� �������� �������\n"
#define MSGTR_GetpathProblem "�������� � get_path(\"config\")\n"
#define MSGTR_CreatingCfgFile "�������� ����� ������������: %s\n"
#define MSGTR_CopyCodecsConf "(����������/��������_������ etc/codecs.conf (�� ���������� MPlayer) � ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "������������ ���������� codecs.conf.\n"
#define MSGTR_CantLoadFont "�� ���� ��������� ��������� �����: %s\n"
#define MSGTR_CantLoadSub "�� ���� ��������� ��������: %s\n"
#define MSGTR_DumpSelectedStreamMissing "����: ��������� ������: ��������� ����� �������!\n"
#define MSGTR_CantOpenDumpfile "�� ���� ������� ���� �����!!!\n"
#define MSGTR_CoreDumped "������ ���� ���� ;)\n"
#define MSGTR_FPSnotspecified "� ��������� �����/��� �� ������� (��� ������������)! ����������� ����� -fps!\n"
#define MSGTR_TryForceAudioFmtStr "������� ����������� ��������� ������������ %s...\n"
#define MSGTR_CantFindAudioCodec "�� ���� ����� ����� ��� ������������ 0x%X!\n"
#define MSGTR_RTFMCodecs "�������� DOCS/HTML/ru/codecs.html!\n"
#define MSGTR_TryForceVideoFmtStr "������� ����������� ��������� ������������ %s...\n"
#define MSGTR_CantFindVideoCodec "�� ���� ����� ����� ��� ���������� -vo � ������������ 0x%X!\n"
#define MSGTR_CannotInitVO "��������� ������: �� ���� ���������������� ������������!\n"
#define MSGTR_CannotInitAO "�� ���� �������/���������������� ��������������� -> ��� �����\n"
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
"  - �� ��������� �������������� ������� DVD/DivX �� ��������� �����������!\n" \
"    ���������� ��������� ����� lavdopts, ��������:\n-vfm ffmpeg -lavdopts lowres=1:fast:skiploopfilter=all.\n"\
"- ����� ����\n"\
"  - ���������� ��������� ����������: -nobps -ni -forceidx -mc 0\n"\
"- ��������� �������� (�������������� NFS/SMB, DVD, VCD � �.�.)\n"\
"  - ����������� -cache 8192.\n"\
"- ����������� �� �� -cache ��� ������������ ����ϣ��� [non-interleaved] AVI ������?\n"\
"  - ����������� -nocache.\n"\
"������� DOCS/HTML/ru/video.html ��� ������� �� ����������/���������.\n"\
"���� ������ �� �������, ����� ������� DOCS/HTML/ru/bugreports.html!\n\n"

#define MSGTR_NoGui "MPlayer ��� ������������� ��� ��������� GUI!\n"
#define MSGTR_GuiNeedsX "GUI MPlayer ��������� X11!\n"
#define MSGTR_Playing "\n��������������� %s.\n"
#define MSGTR_NoSound "�����: ��� �����!!!\n"
#define MSGTR_FPSforced "�����/��� ����������� � %5.3f (����� �����: %5.3f).\n"
#define MSGTR_CompiledWithRuntimeDetection "�������������� ��� ����������� ���� ���������� �� ����� ����������.\n"
#define MSGTR_CompiledWithCPUExtensions "�������������� ��� x86 CPU �� ���������� ������������:"
#define MSGTR_AvailableVideoOutputDrivers "��������� �������� ������ �����:\n"
#define MSGTR_AvailableAudioOutputDrivers "��������� �������� ������ �����:\n"
#define MSGTR_AvailableAudioCodecs "��������� �����������:\n"
#define MSGTR_AvailableVideoCodecs "��������� �����������:\n"
#define MSGTR_AvailableAudioFm "��������� (����������������) ���������/�������� ������������:\n"
#define MSGTR_AvailableVideoFm "��������� (����������������) ���������/�������� ������������:\n"
#define MSGTR_AvailableFsType "��������� ������ ��������� �������������� ����:\n"
#define MSGTR_UsingRTCTiming "������������ ���������� Linux RTC ������������� (%ld��).\n"
#define MSGTR_CannotReadVideoProperties "�����: �� ���� ��������� ��������.\n"
#define MSGTR_NoStreamFound "����� �� ������.\n"
#define MSGTR_ErrorInitializingVODevice "������ ��� ��������/������������� ���������� ���������� ����������� (-vo).\n"
#define MSGTR_ForcedVideoCodec "���������� ����������: %s\n"
#define MSGTR_ForcedAudioCodec "���������� ����������: %s\n"
#define MSGTR_Video_NoVideo "�����: ��� �����\n"
#define MSGTR_NotInitializeVOPorVO "\n��������� ������: �� ���� ���������������� ������������ (-vf) ��� ���������� (-vo).\n"
#define MSGTR_Paused "\n=== �������������� ===\r" // no more than 23 characters (status line for audio files)
#define MSGTR_PlaylistLoadUnable "\n�� ���� ��������� ������ ��������������� (��������) %s.\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- MPlayer ������ ��-�� '������������ ����������'.\n"\
"  ��� ����� ���� ������� ������ ������ ���� ������������� ����������� ���� CPU...\n"\
"  ����������, ������� DOCS/HTML/ru/bugreports.html.\n"
#define MSGTR_Exit_SIGILL \
"- MPlayer ������ ��-�� '������������ ����������'.\n"\
"  ������, ��� ���������� ����� �� ��� ���������� �� CPU, �������� �� ����, ��� ��������\n"\
"  �� ��� �������������/�������������.\n"\
"  ��������� ���!\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- MPlayer ������ ��-�� ������� ������������� CPU/FPU/RAM.\n"\
"  ���������������� MPlayer � --enable-debug � �������� 'gdb' backtrace �\n"\
"  ������������������. ��� ������������, ��.\nDOCS/HTML/ru/bugreports_what.html#bugreports_crash\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer ������. ��� �� ������ �����������.\n"\
"  ��� ����� ���� ������� � ���� MPlayer _���_ � ����� ��������, _���_\n"\
"  � ����� ������ gcc. ���� �� �������, ��� � ���� ������� MPlayer, ����������,\n"\
"  �������� DOCS/HTML/ru/bugreports.html � �������� ����������� ������.\n"\
"  �� �� ������ � �� ����� ��������, ���� �� �� ������������ ��� ����������,\n������� � ��������� ������.\n"
#define MSGTR_LoadingConfig "�������� ���������������� ���� '%s'\n"
#define MSGTR_AddedSubtitleFile "��������: �������� ���� ��������� (%d): %s\n"
#define MSGTR_RemovedSubtitleFile "��������: ���̣� ���� ��������� (%d): %s\n"
#define MSGTR_ErrorOpeningOutputFile "������ �������� ����� [%s] ��� ������!\n"
#define MSGTR_CommandLine "��������� ������:"
#define MSGTR_RTCDeviceNotOpenable "�� ���� ������� %s: %s (������������ ������ �������� ������ ������ �� ���� ����).\n"
#define MSGTR_LinuxRTCInitErrorIrqpSet "������ ������������� Linux RTC � ioctl (rtc_irqp_set %lu): %s\n"
#define MSGTR_IncreaseRTCMaxUserFreq "���������� �������� \"echo %lu > /proc/sys/dev/rtc/max-user-freq\" \n� ����������� ������� ����� �������.\n"
#define MSGTR_LinuxRTCInitErrorPieOn "������ ������������� Linux RTC � ioctl (rtc_pie_on): %s\n"
#define MSGTR_UsingTimingType "������������ %s �������������.\n"
#define MSGTR_NoIdleAndGui "����� -idle �� ���� ���� ������������ ������ � GMPlayer.\n"
#define MSGTR_MenuInitialized "���� ����������������: %s\n"
#define MSGTR_MenuInitFailed "�� ���� ���������������� ����.\n"
#define MSGTR_Getch2InitializedTwice "��������������: getch2_init ������ ������!\n"
#define MSGTR_DumpstreamFdUnavailable "�� ���� ������� ���� ����� ������ - ��� ��������� �������� ����������.\n"
#define MSGTR_FallingBackOnPlaylist "�� ���� ���������������� ������ ��������������� %s...\n"
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
"������� [%f]. ������ ������ ���� � ��������������� �������, �� ���� ���������.\n���������.\n"
#define MSGTR_EdlBadLineBadStop "����� �������� ������ ���� ����� ������� ������.\n"
#define MSGTR_EdloutBadStop "EDL ������� ����Σ�, ��������� start > stop\n"
#define MSGTR_EdloutStartSkip "������ EDL ��������, ������� 'i' �ݣ ��� ��� ���������� �����.\n"
#define MSGTR_EdloutEndSkip "����� EDL ��������, ������ ��������.\n"
#define MSGTR_MPEndposNoSizeBased "� MPlayer ����� -endpos ���� �� ������������ ������� �������.\n"

// mplayer.c OSD

#define MSGTR_OSDenabled "��������"
#define MSGTR_OSDdisabled "���������"
#define MSGTR_OSDAudio "�����: %s"
#define MSGTR_OSDChannel "�����: %s"
#define MSGTR_OSDSubDelay "�������� ���������: %d ��"
#define MSGTR_OSDSpeed "��������: x %6.2f"
#define MSGTR_OSDosd "OSD: %s"
#define MSGTR_OSDChapter "������: (%d) %s"

// property values
#define MSGTR_Enabled "��������"
#define MSGTR_EnabledEdl "�������� (EDL)"
#define MSGTR_Disabled "���������"
#define MSGTR_HardFrameDrop "�����������"
#define MSGTR_Unknown "����������"
#define MSGTR_Bottom "���"
#define MSGTR_Center "�����"
#define MSGTR_Top "����"

// osd bar names
#define MSGTR_Volume "���������"
#define MSGTR_Panscan "�������� ������"
#define MSGTR_Gamma "�����"
#define MSGTR_Brightness "�������"
#define MSGTR_Contrast "�������������"
#define MSGTR_Saturation "������������"
#define MSGTR_Hue "����"

// property state
#define MSGTR_MuteStatus "����������: %s"
#define MSGTR_AVDelayStatus "A-V ��������: %s"
#define MSGTR_OnTopStatus "������ ���������: %s"
#define MSGTR_RootwinStatus "root-����: %s"
#define MSGTR_BorderStatus "�����: %s"
#define MSGTR_FramedroppingStatus "������� ������: %s"
#define MSGTR_VSyncStatus "������������ �������������: %s"
#define MSGTR_SubSelectStatus "��������: %s"
#define MSGTR_SubPosStatus "������� ���������: %s/100"
#define MSGTR_SubAlignStatus "������������ ���������: %s"
#define MSGTR_SubDelayStatus "�������� ���������: %s"
#define MSGTR_SubVisibleStatus "��������: %s"
#define MSGTR_SubForcedOnlyStatus "����������� ������ ��������: %s"

// mencoder.c:

#define MSGTR_UsingPass3ControlFile "��������� ��������� ���� ��� �������� 3-�� �������: %s\n"
#define MSGTR_MissingFilename "\n��������� ��� �����.\n\n"
#define MSGTR_CannotOpenFile_Device "�� ���� ������� ����/����������.\n"
#define MSGTR_CannotOpenDemuxer "�� ���� ������� ���������������.\n"
#define MSGTR_NoAudioEncoderSelected "\n���������� ����� (-oac) �� ������.\n�������� �����-������ (��. -oac help) ��� ����������� -nosound.\n"
#define MSGTR_NoVideoEncoderSelected "\n���������� ����� (-ovc) �� ������. �������� �����-������ (��. -ovc help).\n"
#define MSGTR_CannotOpenOutputFile "�� ���� ������� ���� ������ '%s'.\n"
#define MSGTR_EncoderOpenFailed "�� ���� ������� ����������.\n"
#define MSGTR_MencoderWrongFormatAVI "\n��������������: �������� ������ ����� _AVI_. ��. -of help.\n"
#define MSGTR_MencoderWrongFormatMPG "\n��������������: �������� ������ ����� _MPEG_. ��. -of help.\n"
#define MSGTR_MissingOutputFilename "�� ������ �������� ����, ����������� ����� -o."
#define MSGTR_ForcingOutputFourcc "�������� fourcc ���������� � %x [%.4s]\n"
#define MSGTR_ForcingOutputAudiofmtTag "�������� ��� ��������� ������������ � 0x%x.\n"
#define MSGTR_DuplicateFrames "\n%d �������������(���) ����(�/��)!\n"
#define MSGTR_SkipFrame "\n��������� ����!\n"
#define MSGTR_ResolutionDoesntMatch "\n����� ��������� �������� ���� ����������� ��� �������� �������������, ���\n����������.\n"
#define MSGTR_FrameCopyFileMismatch "\n��� ���������� ������ �������� ���������� fps, ����������� � ������� ���\n-ovc copy.\n"
#define MSGTR_AudioCopyFileMismatch "\n��� ����� ������ �������� ���������� ������������ � �������� ��� -oac copy.\n"
#define MSGTR_NoAudioFileMismatch "\n�� ���� ��������� �����, ���������� ������ �����, � �����- � ������������.\n���������� -nosound.\n"
#define MSGTR_NoSpeedWithFrameCopy "��������������: �� ������������� ���������� ������ -speed � -oac copy!\n"\
"���� ����������� ����� ���� ���������!\n"
#define MSGTR_ErrorWritingFile "%s: ������ ��� ������ �����.\n"
#define MSGTR_RecommendedVideoBitrate "������������� �������� ��� %s CD: %d\n"
#define MSGTR_VideoStreamResult "\n����� �����: %8.3f ����/�  (%d �/�)  ������: %"PRIu64" ����(�/��)  %5.3f ���.  %d ����(�/��)\n"
#define MSGTR_AudioStreamResult "\n����� �����: %8.3f ����/�  (%d �/�)  ������: %"PRIu64" ����(�/��)  %5.3f ���.\n"
#define MSGTR_OpenedStream "�����: ������: %d  ������: 0x%X - 0x%x\n"
#define MSGTR_VCodecFramecopy "����������: ����������� ������ (%dx%d %dbpp fourcc=%x)\n"
#define MSGTR_ACodecFramecopy "����������: ����������� ������ (������=%x �������=%d ��������=%d �����=%d �/�=%d �������=%d)\n"
#define MSGTR_CBRPCMAudioSelected "������� CBR PCM �����\n"
#define MSGTR_MP3AudioSelected "������� MP3 �����\n"
#define MSGTR_CannotAllocateBytes "�� ���� �������� ������ ��� %d ����\n"
#define MSGTR_SettingAudioDelay "������������ ������������� � %5.3f\n"
#define MSGTR_SettingVideoDelay "������������ ������������� � %5.3fs.\n"
#define MSGTR_SettingAudioInputGain "������������ �������� �������� ����������� � %f\n"
#define MSGTR_LamePresetEquals "\npreset=%s\n\n"
#define MSGTR_LimitingAudioPreload "����������� ������������ ����� �� 0.4�\n"
#define MSGTR_IncreasingAudioDensity "���������� ��������� ����� �� 4\n"
#define MSGTR_ZeroingAudioPreloadAndMaxPtsCorrection "�������� ������������ ����� � 0, ������������ ��������� pts � 0\n"
#define MSGTR_CBRAudioByterate "\n\nCBR �����: %d ����/���, %d ����/����\n"
#define MSGTR_LameVersion "������ LAME %s (%s)\n\n"
#define MSGTR_InvalidBitrateForLamePreset "������: �������� �������� ��� ����������� �������� ��� ������ �������������.\n"\
"\n"\
"��� ������������� ����� ������ �� ������ ������� �������� ����� \"8\" � \"320\"\n"\
"\n"\
"��� �������������� ���������� �����������: \"-lameopts preset=help\"\n"
#define MSGTR_InvalidLamePresetOptions "������: �� �� ������� ������ ������� �/��� ����� �������������.\n"\
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
"����� ������������� ����������� � ����� �������������� ����������� ����������\n��������.\n"\
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
"   ��� ABR ������� (������� �������� ��� ��������� ���������,\n�� �� ����� �������, ��� VBR):\n"\
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
"            ���������, ��� �������� ������� mp3 ����� ��������.\n"\
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
#define MSGTR_LameCantInit \
"�� ���� ������ ����� LAME, ��������� ��������/�������_�������������,\n"\
"��������� ����� ����� ��������� (<32) ��������� � ������� ��������\n������������� (��������, -srate 8000).\n"\
"���� �ӣ ������ �� �������, ���������� �������������."
#define MSGTR_ConfigFileError "������ � ���������������� �����"
#define MSGTR_ErrorParsingCommandLine "������ ������� ��������� ������"
#define MSGTR_VideoStreamRequired "������� ����������� �����������!\n"
#define MSGTR_ForcingInputFPS "������� �����/��� ����� �������� �� %5.2f\n"
#define MSGTR_RawvideoDoesNotSupportAudio "�������� ������ ����� RAWVIDEO �� ������������ ����� - �������� �����\n"
#define MSGTR_DemuxerDoesntSupportNosound "���� ��������������� ���� ��� �� ������������ -nosound.\n"
#define MSGTR_MemAllocFailed "�� ���� �������� ������"
#define MSGTR_NoMatchingFilter "�� ���� ����� ��������������� ������/������_�����������!\n"
#define MSGTR_MP3WaveFormatSizeNot30 "sizeof(MPEGLAYER3WAVEFORMAT)==%d!=30, ��������, ��������� ���������� C?\n"
#define MSGTR_NoLavcAudioCodecName "����� LAVC, ��������� ��� ������!\n"
#define MSGTR_LavcAudioCodecNotFound "����� LAVC, �� ���� ����� ���������� ��� ������ %s\n"
#define MSGTR_CouldntAllocateLavcContext "����� LAVC, �� ���� ���������� ��������!\n"
#define MSGTR_CouldntOpenCodec "�� ���� ������� ����� %s, br=%d\n"
#define MSGTR_CantCopyAudioFormat "����������� 0x%x ����������� � '-oac copy', ���������� '-oac pcm'\n��� ����������� '-fafmttag' ��� ��� ���������������.\n"

// cfg-mencoder.h:

#define MSGTR_MEncoderMP3LameHelp "\n\n"\
" vbr=<0-4>     ����� ����������� � ���������� ����������\n"\
"                0: cbr (���������� ��������)\n"\
"                1: mt (VBR �������� ����� ������� [Mark Taylor])\n"\
"                2: rh (VBR �������� ������� ��������� [Robert Hegemann]\n                   -- �� ���������)\n"\
"                3: abr (�����Σ���� ��������)\n"\
"                4: mtrh (VBR �������� ����� ������� � ������� ���������)\n"\
"\n"\
" abr           �����Σ���� ��������\n"\
"\n"\
" cbr           ���������� ��������\n"\
"               ����� ��������� ����� CBR ����������� �� ����������� �������\n               ������������� ABR\n"\
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
" fast          ������������ �� ������� ����������� �� ����������� �������\n"\
"               ������������� VBR; ��������� ������ �������� � ���������� ���������.\n"\
"\n"\
" preset=<value> ������������� ��������� ����������� ���������� ��������.\n"\
"                 medium: VBR �����������, ������� ��������\n"\
"                 (��������� ��������� 150-180 kbps)\n"\
"                 standard: VBR �����������, ������� ��������\n"\
"                 (��������� ��������� 170-210 kbps)\n"\
"                 extreme: VBR �����������, ����� ������� ��������\n"\
"                 (��������� ��������� 200-240 kbps)\n"\
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
#define MSGTR_OutdatedCodecsConf "���� codecs.conf ������� ���� � ����������� � ������ ������� MPlayer!"

// fifo.c
#define MSGTR_CannotMakePipe "�� ���� ������� �����!\n"

// m_config.c
#define MSGTR_SaveSlotTooOld "������ ������� ������ ���� ���������� �� lvl %d: %d !!!\n"
#define MSGTR_InvalidCfgfileOption "����� %s �� ����� �������������� � ���������������� �����.\n"
#define MSGTR_InvalidCmdlineOption "����� %s �� ����� �������������� � ��������� ������.\n"
#define MSGTR_InvalidSuboption "������: � ����� '%s' ��� �������� '%s'.\n"
#define MSGTR_MissingSuboptionParameter "������: � �������� '%s' ����� '%s' ������ ���� ��������!\n"
#define MSGTR_MissingOptionParameter "������: � ����� '%s' ������ ���� ��������!\n"
#define MSGTR_OptionListHeader "\n ���                  ���             �������    �������� �����   CL    ����\n\n"
#define MSGTR_TotalOptions "\n�����: %d �����(�/�)\n"
#define MSGTR_ProfileInclusionTooDeep "��������������: ��������� ������� ������� �������.\n"
#define MSGTR_NoProfileDefined "�� ���� ������� �� ��� ������̣�.\n"
#define MSGTR_AvailableProfiles "��������� �������:\n"
#define MSGTR_UnknownProfile "����������� ������� '%s'.\n"
#define MSGTR_Profile "������� %s: %s\n"

// m_property.c
#define MSGTR_PropertyListHeader "\n ���                  ���             �������    ��������\n\n"
#define MSGTR_TotalProperties "\n�����: %d �������\n"

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

// stream_dvd.c
#define MSGTR_NoDVDSupport "MPlayer ��� ������������� ��� ��������� DVD, �������.\n"
#define MSGTR_DVDnumTitles "�� ���� DVD %d �������.\n"
#define MSGTR_DVDinvalidTitle "������������ ����� DVD ������: %d\n"
#define MSGTR_DVDnumChapters "� ���� DVD ������ %d ������[�/��].\n"
#define MSGTR_DVDinvalidChapter "������������ ����� ������� DVD: %d\n"
#define MSGTR_DVDinvalidChapterRange "�������� �������� ��������� ������� %s\n"
#define MSGTR_DVDinvalidLastChapter "�������� ����� ���������� ������� DVD: %d\n"
#define MSGTR_DVDnumAngles "� ���� DVD ������ %d �����.\n"
#define MSGTR_DVDinvalidAngle "������������ ����� DVD ����: %d\n"
#define MSGTR_DVDnoIFO "�� ���� ������� IFO ���� ��� DVD ������ %d.\n"
#define MSGTR_DVDnoVMG "�� ���� ������� VMG ����������!\n"
#define MSGTR_DVDnoVOBs "�� ���� ������� VOBS ������ (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDnoMatchingAudio "�� ������ ���������� ����� ���� DVD!\n"
#define MSGTR_DVDaudioChannel "��������� ���������� DVD: %d ����: %c%c\n"
#define MSGTR_DVDaudioStreamInfo "����������: %d ������: %s (%s) ����: %s aid: %d.\n"
#define MSGTR_DVDnumAudioChannels "����� ������������ �� �����: %d.\n"
#define MSGTR_DVDnoMatchingSubtitle "�� ������ ���������� ���� ��������� DVD!\n"
#define MSGTR_DVDsubtitleChannel "��������� ����� ��������� DVD: %d ����: %c%c\n"
#define MSGTR_DVDsubtitleLanguage "�������� ( sid ): %d ����: %s\n"
#define MSGTR_DVDnumSubtitles "����� ��������� �� �����: %d\n"

// muxer.c, muxer_*.c:
#define MSGTR_TooManyStreams "������� ����� �������!"
#define MSGTR_RawMuxerOnlyOneStream "������������� rawaudio ������������ ������ ���� ����������!\n"
#define MSGTR_IgnoringVideoStream "��������� ����������!\n"
#define MSGTR_UnknownStreamType "��������������, ����������� ��� ������: %d\n"
#define MSGTR_WarningLenIsntDivisible "��������������: ����� �� ������ ������� �������!\n"
#define MSGTR_MuxbufMallocErr "������������� ����������� �� ����� �������� ������ (malloc)!\n"
#define MSGTR_MuxbufReallocErr "������������� ����������� �� ����� ���������������� ������ (realloc)!\n"
#define MSGTR_MuxbufSending "������������� ����������� �������� %d ����(�/��) � �������������.\n"
#define MSGTR_WritingHeader "������ ���������...\n"
#define MSGTR_WritingTrailer "������ �������...\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "��������������: ��������� ����� ������ %d ����������̣�!\n"
#define MSGTR_VideoStreamRedefined "��������������: ��������� ����� ������ %d ����������̣�!\n"
#define MSGTR_TooManyAudioInBuffer "\n������� ����� (%d � %d ������) ������������ � ������!\n"
#define MSGTR_TooManyVideoInBuffer "\n������� ����� (%d � %d ������) ������������ � ������!\n"
#define MSGTR_MaybeNI "�������� �� ������������ '����ϣ���' �����/���� ��� ��������� �����?\n" \
                      "��� AVI ������ ���������� ����������� '����ϣ���' ����� ������ -ni.\n"
#define MSGTR_SwitchToNi "\n��������� ����� '��ϣ���' AVI ���� - ������������ � -ni �����...\n"
#define MSGTR_Detected_XXX_FileFormat "��������� %s ������ �����!\n"
#define MSGTR_DetectedAudiofile "��������� ���������.\n"
#define MSGTR_NotSystemStream "�� MPEG System Stream ������... (��������, Transport Stream?)\n"
#define MSGTR_InvalidMPEGES "������������ MPEG-ES �����??? ��������� � �������, ��� ����� ���� ����� :(\n"
#define MSGTR_FormatNotRecognized "======= ��������, ������ ����� ����� �� ���������/�� �������������� ==========\n"\
				  "===== ���� ��� AVI, ASF ��� MPEG �����, ���������� ��������� � �������! ======\n"
#define MSGTR_MissingVideoStream "���������� �� ������!\n"
#define MSGTR_MissingAudioStream "���������� �� ������ -> ��� �����\n"
#define MSGTR_MissingVideoStreamBug "���������� �������!? ��������� � �������, ��� ����� ���� ����� :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: � ����� ��� ���������� ����� ��� �����������\n"

#define MSGTR_NI_Forced "����������"
#define MSGTR_NI_Detected "���������"
#define MSGTR_NI_Message "%s '��������' ������ AVI �����!\n"

#define MSGTR_UsingNINI "������������� '���������' ������������ ������� AVI �����!\n"
#define MSGTR_CouldntDetFNo "�� ���� ���������� ����� ������ (��� ����������� �����������).\n"
#define MSGTR_CantSeekRawAVI "�� ���� ������������� � ����� ������ AVI!\n(��������� ������, ���������� � ������ -idx!)\n"
#define MSGTR_CantSeekFile "�� ���� ������������ � ���� �����!\n"

#define MSGTR_MOVcomprhdr "MOV: ��� ��������� ������ ���������� ��������� zlib!\n"
#define MSGTR_MOVvariableFourCC "MOV: ��������������! ��������� ���������� FOURCC!?\n"
#define MSGTR_MOVtooManyTrk "MOV: ��������������! ������� ����� ������!"
#define MSGTR_FoundAudioStream "==> ��ۣ� ����������: %d\n"
#define MSGTR_FoundVideoStream "==> ��ۣ� ����������: %d\n"
#define MSGTR_DetectedTV "������ ��! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "�� ���� ������� ��������������� ogg.\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: ��� ���������� (id:%d).\n"
#define MSGTR_CannotOpenAudioStream "�� ���� ������� ����������: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "�� ���� ������� ����� ���������: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "�� ���� ������� ��������������� �����: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "�� ���� ������� ��������������� ���������: %s\n"
#define MSGTR_TVInputNotSeekable "�� �� ����� ������ ������������! (��������, ����������� ����� ��� ����� ������� ;)\n"
#define MSGTR_DemuxerInfoChanged "���������� ���������������� %s �������� � %s\n"
#define MSGTR_ClipInfo "���������� � �����:\n"

#define MSGTR_LeaveTelecineMode "\ndemux_mpg: ���������� 30 ������/��� NTSC ����������, ���������� ������� ������.\n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: ���������� 24 �����/��� �������������� [progressive] NTSC ����������,\n���������� ������� ������.\n"

#define MSGTR_CacheFill "\r���������� ����: %5.2f%% (%"PRId64" ����(�/��))   "
#define MSGTR_NoBindFound "�� ������� �������� � ������� '%s'."
#define MSGTR_FailedToOpen "�� ���� ������� %s.\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "�� ���� ������� �����.\n"
#define MSGTR_CantCloseCodec "�� ���� ������� �����.\n"

#define MSGTR_MissingDLLcodec "������: �� ���� ������� ����������� DirectShow �����: %s\n"
#define MSGTR_ACMiniterror "�� ���� ���������/���������������� Win32/ACM ���������� (������� DLL ����?)\n"
#define MSGTR_MissingLAVCcodec "�� ���� ����� ����� '%s' � libavcodec...\n"

#define MSGTR_MpegNoSequHdr "MPEG: ��������� ������: ����� ����� ��� ������ ������������������ ����������.\n"
#define MSGTR_CannotReadMpegSequHdr "��������� ������: �� ���� ������� ������������������ ����������.\n"
#define MSGTR_CannotReadMpegSequHdrEx "��������� ������: �� ���� ������� ���������� ������������������ ����������.\n"
#define MSGTR_BadMpegSequHdr "MPEG: ������ ������������������ ����������.\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: ������ ���������� ������������������ ����������.\n"

#define MSGTR_ShMemAllocFail "�� ���� ��������������� ����������� ������.\n"
#define MSGTR_CantAllocAudioBuf "�� ���� ��������������� �������� ����� �����.\n"

#define MSGTR_UnknownAudio "�����������/���������� ����� ������ -> ��� �����\n"

#define MSGTR_UsingExternalPP "[PP] ��������� ������� ������ �������������, max q = %d.\n"
#define MSGTR_UsingCodecPP "[PP] ��������� ������������� �� ������, max q = %d.\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "������������ '%s' �� �������������� ���������� vo � vd.\n"
#define MSGTR_VideoCodecFamilyNotAvailableStr "����������� ��������� ������������ [%s] (vfm=%s) �� ��������.\n�������� ��� �� ����� ����������.\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "����������� ��������� ������������ [%s] (afm=%s) �� ��������.\n�������� ��� �� ����� ����������.\n"
#define MSGTR_OpeningVideoDecoder "�������� ������� �����: [%s] %s\n"
#define MSGTR_SelectedVideoCodec "������ ����������: [%s] vfm: %s (%s)\n"
#define MSGTR_OpeningAudioDecoder "�������� ������� �����: [%s] %s\n"
#define MSGTR_SelectedAudioCodec "������ ����������: [%s] afm: %s (%s)\n"
#define MSGTR_BuildingAudioFilterChain "���������� ������� ������������ ��� %dHz/%dch/%s -> %dHz/%dch/%s...\n"
#define MSGTR_UninitVideoStr "��������������� �����: %s\n"
#define MSGTR_UninitAudioStr "��������������� �����: %s\n"
#define MSGTR_VDecoderInitFailed "������ ������������� �������� ����� :(\n"
#define MSGTR_ADecoderInitFailed "������ ������������� �������� ����� :(\n"
#define MSGTR_ADecoderPreinitFailed "������ ���������������� �������� ����� :(\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: ���������� %d ����(�/��) ��� �������� ������.\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: ���������� %d + %d = %d ����(�/��) ��� ������ ������.\n"

// LIRC:
#define MSGTR_SettingUpLIRC "��������� ��������� LIRC...\n"
#define MSGTR_LIRCopenfailed "��������� �������� ��������� LIRC.\n�� �� ������� ������������ ��� ����� ����������.\n"
#define MSGTR_LIRCcfgerr "��������� ������� ������ ����� ������������ LIRC '%s'!\n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "�� ���� ����� ����� ������ '%s'.\n"
#define MSGTR_CouldNotOpenVideoFilter "�� ���� ������� ����� ������ '%s'.\n"
#define MSGTR_OpeningVideoFilter "�������� ����� ������: "
#define MSGTR_CannotFindColorspace "�� ���� ����� ���������� �������� ������������, ���� ������� 'scale' :(\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDec: ����� �� ��������� sh->disp_w � sh->disp_h, ������� ������.\n"
#define MSGTR_VoConfigRequest "VDec: ������ vo config - %d x %d (������������ �������� ������������: %s)\n"
#define MSGTR_CouldNotFindColorspace "�� ���� ����� ���������� �������� ������������ - ��������� � -vf scale...\n"
#define MSGTR_MovieAspectIsSet "Movie-Aspect - %.2f:1 - �������������� ��� ��������� ����������� ������ ������.\n"
#define MSGTR_MovieAspectUndefined "Movie-Aspect �� ������̣� - ������������������ �� �����������.\n"

// vd_dshow.c, vd_dmo.c
#define MSGTR_DownloadCodecPackage "��� ����� ��������/���������� ����� �������� �������.\n������� �� http://www.mplayerhq.hu/dload.html\n"
#define MSGTR_DShowInitOK "����������: Win32/DShow ����� ����� ������� ���������������.\n"
#define MSGTR_DMOInitOK "����������: Win32/DMO ����� ����� ������� ���������������.\n"

// x11_common.c
#define MSGTR_EwmhFullscreenStateFailed "\nX11: �� ���� ������� ������� EWMH fullscreen!\n"
#define MSGTR_CouldNotFindXScreenSaver "xscreensaver_disable: �� ���� ����� ���� XScreenSaver'�.\n"
#define MSGTR_SelectedVideoMode "XF86VM: ������ ���������� %dx%d ��� ������� ����������� %dx%d.\n"

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
// Note: If you change MSGTR_PlayList please see if it still fits MSGTR_MENU_PlayList
#define MSGTR_PlayList "��������"
#define MSGTR_Equalizer "����������"
#define MSGTR_ConfigureEqualizer "��������� �������"
#define MSGTR_SkinBrowser "����������� ������"
#define MSGTR_Network "������� ������..."
// Note: If you change MSGTR_Preferences please see if it still fits MSGTR_MENU_Preferences
#define MSGTR_Preferences "���������"
#define MSGTR_AudioPreferences "������������ ����� ��������"
#define MSGTR_NoMediaOpened "�������� �� ������."
#define MSGTR_VCDTrack "������� VCD %d"
#define MSGTR_NoChapter "��� �������"
#define MSGTR_Chapter "������ %d"
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
#define MSGTR_SKIN_ERRORMESSAGE "[����] ������ � ����� ������������ ����� �� ������ %d: %s" 
#define MSGTR_SKIN_WARNING1 "[����] ��������������: � ����� ������������ ����� �� ������ %d:\n������� GUI ������, �� �� ����� �� ������� \"section\" (%s)"
#define MSGTR_SKIN_WARNING2 "[����] ��������������: � ����� ������������ ����� �� ������ %d:\n������� GUI ������, �� �� ����� �� ������� \"subsection\" (%s)"
#define MSGTR_SKIN_WARNING3 "[����] ��������������: � ����� ������������ ����� �� ������ %d:\n��� ��������� �� �������������� ���� ��������� GUI (%s)"
#define MSGTR_SKIN_SkinFileNotFound "[����] ���� '%s' �� ������.\n"
#define MSGTR_SKIN_SkinFileNotReadable "[����] ���� ( %s ) �� ������.\n"
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
#define MSGTR_SKIN_SKINCFG_SkinNotFound "���� �� ������ (%s).\n"
#define MSGTR_SKIN_SKINCFG_SelectedSkinNotFound "��������� ���� '%s' �� ������, ������ 'default'...\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "������ ������ ����� ������������ ������ (%s)\n"
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
#define MSGTR_MENU_Chapters "�������"
#define MSGTR_MENU_Chapter "������ %2d"
#define MSGTR_MENU_AudioLanguages "����� �����"
#define MSGTR_MENU_SubtitleLanguages "���� ���������"
#define MSGTR_MENU_PlayList MSGTR_PlayList
#define MSGTR_MENU_SkinBrowser "����������� ������"
#define MSGTR_MENU_Preferences MSGTR_Preferences
#define MSGTR_MENU_Exit "�����..."
#define MSGTR_MENU_Mute "����������"
#define MSGTR_MENU_Original "��������"
#define MSGTR_MENU_AspectRatio "����������� ������"
#define MSGTR_MENU_AudioTrack "����� �������"
#define MSGTR_MENU_Track "������� %d"
#define MSGTR_MENU_VideoTrack "����� �������"
#define MSGTR_MENU_Subtitles "��������"

// --- equalizer
// Note: If you change MSGTR_EQU_Audio please see if it still fits MSGTR_PREFERENCES_Audio
#define MSGTR_EQU_Audio "�����"
// Note: If you change MSGTR_EQU_Video please see if it still fits MSGTR_PREFERENCES_Video
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
#define MSGTR_EQU_Bass "���"
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
#define MSGTR_PREFERENCES_Audio MSGTR_EQU_Audio
#define MSGTR_PREFERENCES_Video MSGTR_EQU_Video
#define MSGTR_PREFERENCES_SubtitleOSD "�������� � OSD"
#define MSGTR_PREFERENCES_Codecs "������ � ���������������"
// Note: If you change MSGTR_PREFERENCES_Misc see if it still fits MSGTR_PREFERENCES_FRAME_Misc
#define MSGTR_PREFERENCES_Misc "������"

#define MSGTR_PREFERENCES_None "���"
#define MSGTR_PREFERENCES_DriverDefault "������� �� ���������"
#define MSGTR_PREFERENCES_AvailableDrivers "��������� ��������:"
#define MSGTR_PREFERENCES_DoNotPlaySound "�� ����������� ����"
#define MSGTR_PREFERENCES_NormalizeSound "������������� ����"
#define MSGTR_PREFERENCES_EnableEqualizer "�������� ����������"
#define MSGTR_PREFERENCES_SoftwareMixer "�������� ����������� ������"
#define MSGTR_PREFERENCES_ExtraStereo "�������� �������������� ������"
#define MSGTR_PREFERENCES_Coefficient "�����������:"
#define MSGTR_PREFERENCES_AudioDelay "�������� �����"
#define MSGTR_PREFERENCES_DoubleBuffer "�������� ������� �����������"
#define MSGTR_PREFERENCES_DirectRender "�������� ������ �����������"
#define MSGTR_PREFERENCES_FrameDrop "�������� ������� ������"
#define MSGTR_PREFERENCES_HFrameDrop "�������� ����������� ������� ������ (������)"
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
#define MSGTR_PREFERENCES_SUB_USE_ASS "SSA/ASS ��������� ���������"
#define MSGTR_PREFERENCES_SUB_ASS_USE_MARGINS "�������. ����"
#define MSGTR_PREFERENCES_SUB_ASS_TOP_MARGIN "����: "
#define MSGTR_PREFERENCES_SUB_ASS_BOTTOM_MARGIN "���: "
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
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "����� � ���������������"
#define MSGTR_PREFERENCES_FRAME_Cache "���"
#define MSGTR_PREFERENCES_FRAME_Misc MSGTR_PREFERENCES_Misc
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
#define MSGTR_PREFERENCES_FontEncoding22 "����������/����������-����������� Window$ (CP1250)"
#define MSGTR_PREFERENCES_FontNoAutoScale "�� ��������������"
#define MSGTR_PREFERENCES_FontPropWidth "��������������� ������ ������"
#define MSGTR_PREFERENCES_FontPropHeight "��������������� ������ ������"
#define MSGTR_PREFERENCES_FontPropDiagonal "��������������� ��������� ������"
#define MSGTR_PREFERENCES_FontEncoding "���������:"
#define MSGTR_PREFERENCES_FontBlur "����������:"
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
#define MSGTR_PREFERENCES_CDROMDevice "CD-ROM:"
#define MSGTR_PREFERENCES_DVDDevice "DVD:"
#define MSGTR_PREFERENCES_FPS "FPS ������:"
#define MSGTR_PREFERENCES_ShowVideoWindow "���������� ���� �����, ����� ���������"
#define MSGTR_PREFERENCES_ArtsBroken "����� ������ aRts ������������ � GTK 1.x "\
	   "� �������� � ��������� ������ GMPlayer!"

#define MSGTR_ABOUT_UHU "���������� GUI ������������ UHU Linux\n"
#define MSGTR_ABOUT_Contributors "������������ ���� � ������������\n"
#define MSGTR_ABOUT_Codecs_libs_contributions "������ � ��������� ����������\n"
#define MSGTR_ABOUT_Translations "��������\n"
#define MSGTR_ABOUT_Skins "�����\n"

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
#define MSGTR_AddingVideoFilter "[GUI] �������� �����������: %s\n"
#define MSGTR_RemovingVideoFilter "[GUI] ������ �����������: %s\n"

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

// vo_aa.c

#define MSGTR_VO_AA_HelpHeader "\n\n�������� vo_aa ���������� aalib:\n"
#define MSGTR_VO_AA_AdditionalOptions "�������������� �����, ��������������� vo_aa:\n" \
"  help        ������� ��� ���������\n" \
"  osdcolor    ���������� ���� OSD\n  subcolor    ���������� ���� ���������\n" \
"        ��������� �����:\n           0 : ����������\n" \
"           1 : �������\n           2 : ������\n           3 : ������ �����\n" \
"           4 : ���������\n           5 : �����������\n\n\n"

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

// sub.c
#define MSGTR_VO_SUB_Seekbar "���������"
#define MSGTR_VO_SUB_Play "���������������"
#define MSGTR_VO_SUB_Pause "�����"
#define MSGTR_VO_SUB_Stop "����"
#define MSGTR_VO_SUB_Rewind "�����"
#define MSGTR_VO_SUB_Forward "���ң�"
#define MSGTR_VO_SUB_Clock "�����"
#define MSGTR_VO_SUB_Contrast "��������"
#define MSGTR_VO_SUB_Saturation "������������"
#define MSGTR_VO_SUB_Volume "���������"
#define MSGTR_VO_SUB_Brightness "�������"
#define MSGTR_VO_SUB_Hue "����"

// vo_xv.c
#define MSGTR_VO_XV_ImagedimTooHigh "������� ����������� ��������� ������� ������: %ux%u (�������� %ux%u)\n"

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
#define MSGTR_AO_OSS_CantSet "[AO OSS] �� ���� ���������� ��������������� %s � %s �����, ������ %s...\n"
#define MSGTR_AO_OSS_CantSetChans "[AO OSS] ������������� �����: �� ���� ���������� ���������������\n� %d-��������� �����.\n"
#define MSGTR_AO_OSS_CantUseGetospace "[AO OSS] ������������� �����: ������� �� ������������ SNDCTL_DSP_GETOSPACE :-(\n"
#define MSGTR_AO_OSS_CantUseSelect "[AO OSS]\n   ***  ��� ������������ �� ������������ select()  ***\n ���������������� MPlayer � #undef HAVE_AUDIO_SELECT � config.h !\n\n"
#define MSGTR_AO_OSS_CantReopen "[AO OSS] ��������� ������:\n*** �� ���� �������� ������� / �������� ��������������� (%s) ***\n"
#define MSGTR_AO_OSS_UnknownUnsupportedFormat "[AO OSS] �����������/���������������� ������ OSS: %x.\n"

// ao_arts.c
#define MSGTR_AO_ARTS_CantInit "[AO ARTS] %s\n"
#define MSGTR_AO_ARTS_ServerConnect "[AO ARTS] ���������� � �������� ��������.\n"
#define MSGTR_AO_ARTS_CantOpenStream "[AO ARTS] �� ���� ������� �����.\n"
#define MSGTR_AO_ARTS_StreamOpen "[AO ARTS] ����� ������.\n"
#define MSGTR_AO_ARTS_BufferSize "[AO ARTS] ������ ������: %d\n"

// ao_dxr2.c
#define MSGTR_AO_DXR2_SetVolFailed "[AO DXR2] �� ���� ���������� ��������� � %d.\n"
#define MSGTR_AO_DXR2_UnsupSamplerate "[AO DXR2] dxr2: %d �� �� ��������������, ���������� ��������\n������� �������������.\n"

// ao_esd.c
#define MSGTR_AO_ESD_CantOpenSound "[AO ESD] ��������� esd_open_sound �� �������: %s\n"
#define MSGTR_AO_ESD_LatencyInfo "[AO ESD] ��������: [������: %0.2fs, ����: %0.2fs] (���������� %0.2fs)\n"
#define MSGTR_AO_ESD_CantOpenPBStream "[AO ESD] �� ���� ������� ����� ��������������� esd: %s\n"

// ao_mpegpes.c
#define MSGTR_AO_MPEGPES_CantSetMixer "[AO MPEGPES] DVB �����: �� ���� ���������� ������: %s\n"
#define MSGTR_AO_MPEGPES_UnsupSamplerate "[AO MPEGPES] %d �� �� ��������������, ���������� ��������\n������� �������������.\n"

// ao_null.c
// This one desn't even  have any mp_msg nor printf's?? [CHECK]

// ao_pcm.c
#define MSGTR_AO_PCM_FileInfo "[AO PCM] ����: %s (%s)\nPCM: ������� �������������: %i �� ������: %s ������ %s\n"
#define MSGTR_AO_PCM_HintInfo "[AO PCM] ����������: �������� ������� ������� ����������� � -vc null -vo null\n[AO PCM]: ����������: -ao pcm:fast. ��� ������ WAVE ������ �����������\n[AO PCM]: ����������: -ao pcm:waveheader (�� ���������).\n"
#define MSGTR_AO_PCM_CantOpenOutputFile "[AO PCM] �� ���� ������� %s ��� ������!\n"

// ao_sdl.c
#define MSGTR_AO_SDL_INFO "[AO SDL] ������� �������������: %i �� ������: %s ������ %s\n"
#define MSGTR_AO_SDL_DriverInfo "[AO SDL] ��������� %s ������������.\n"
#define MSGTR_AO_SDL_UnsupportedAudioFmt "[AO SDL] ���������������� �����������: 0x%x.\n"
#define MSGTR_AO_SDL_CantInit "[AO SDL] �� ���� ���������������� SDL �����: %s\n"
#define MSGTR_AO_SDL_CantOpenAudio "[AO SDL] �� ���� ������� �����: %s\n"

// ao_sgi.c
#define MSGTR_AO_SGI_INFO "[AO SGI] ����������.\n"
#define MSGTR_AO_SGI_InitInfo "[AO SGI] �������������: ������� �������������: %i �� ������: %s ������ %s\n"
#define MSGTR_AO_SGI_InvalidDevice "[AO SGI] ���������������: �������� ����������.\n"
#define MSGTR_AO_SGI_CantSetParms_Samplerate "[AO SGI] �������������: ������ ��������� ����������: %s\n�� ���� ���������� ��������� ������� �������������.\n"
#define MSGTR_AO_SGI_CantSetAlRate "[AO SGI] �������������: AL_RATE �� �������� �� �������� �������.\n"
#define MSGTR_AO_SGI_CantGetParms "[AO SGI] �������������: ������ ��������� ����������: %s\n"
#define MSGTR_AO_SGI_SampleRateInfo "[AO SGI] �������������: ������� ������������� ������ %lf (��������� ������� %lf)\n"
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
#define MSGTR_AO_SUN_UnsupSampleRate "[AO SUN] ������������� �����: ���� ����� �� ������������ ����� %d,\n%s, ������� ������������� %d ��.\n"
#define MSGTR_AO_SUN_CantUseSelect "[AO SUN]\n   ***  ��� ������������ �� ������������ select()  ***\n���������������� MPlayer � #undef HAVE_AUDIO_SELECT � config.h !\n\n"
#define MSGTR_AO_SUN_CantReopenReset "[AO SUN] ��������� ������:\n*** �� ���� �������� ������� / �������� ��������������� (%s) ***\n"

// ao_alsa5.c
#define MSGTR_AO_ALSA5_InitInfo "[AO ALSA5] ������������� alsa: ����������� ������: %d ��, %d �������, %s\n"
#define MSGTR_AO_ALSA5_SoundCardNotFound "[AO ALSA5] ������������� alsa: �� ������� �������� ����.\n"
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
#define MSGTR_AO_ALSA5_WriteErrorAfterReset "[AO ALSA5] ��������������� alsa: ������ ������ ����� ������: %s - �����ģ���.\n"
#define MSGTR_AO_ALSA5_OutPutError "[AO ALSA5] ��������������� alsa: ������ ������: %s\n"

// ao_plugin.c

#define MSGTR_AO_PLUGIN_InvalidPlugin "[AO ������] �������� ������: %s\n"

// ======================= AF Audio Filters ================================

// libaf 

// af_ladspa.c

#define MSGTR_AF_LADSPA_AvailableLabels "��������� ����� �"
#define MSGTR_AF_LADSPA_WarnNoInputs "��������������! � ����� LADSPA ������� ����������� ����������.\n  �������� ����������� ����� �������."
#define MSGTR_AF_LADSPA_ErrMultiChannel "��������������� (>2) ������� ���� ��� �� ��������������.\n  ����������� ������ ����- � �������������."
#define MSGTR_AF_LADSPA_ErrNoOutputs "� ����� LADSPA ������� ����������� �����������."
#define MSGTR_AF_LADSPA_ErrInOutDiff "����� ����������� � ������������ � LADSPA ������� ����������."
#define MSGTR_AF_LADSPA_ErrFailedToLoad "�� ���� ���������"
#define MSGTR_AF_LADSPA_ErrNoDescriptor "�� ���� ����� ������� ladspa_descriptor() � ��������� ����� ����������."
#define MSGTR_AF_LADSPA_ErrLabelNotFound "�� ���� ����� ����� � ���������� �������."
#define MSGTR_AF_LADSPA_ErrNoSuboptions "�� ������� ��������."
#define MSGTR_AF_LADSPA_ErrNoLibFile "�� ������ ���� ����������."
#define MSGTR_AF_LADSPA_ErrNoLabel "�� ������� ����� �������."
#define MSGTR_AF_LADSPA_ErrNotEnoughControls "������������ �������� ������� � ��������� ������."
#define MSGTR_AF_LADSPA_ErrControlBelow "%s: ������� �������� #%d ������ ������ ������� %0.4f.\n"
#define MSGTR_AF_LADSPA_ErrControlAbove "%s: ������� �������� #%d ������ ������� ������� %0.4f.\n"

// stream/stream_radio.c

#define MSGTR_RADIO_ChannelNamesDetected "[radio] ���������� ����� ������������.\n"
#define MSGTR_RADIO_WrongFreqForChannel "[radio] �������� ������� ��� ������� %s\n"
#define MSGTR_RADIO_WrongChannelNumberFloat "[radio] �������� ����� �������: %.2f\n"
#define MSGTR_RADIO_WrongChannelNumberInt "[radio] �������� ����� �������: %d\n"
#define MSGTR_RADIO_WrongChannelName "[radio] �������� �������� �������: %s\n"
#define MSGTR_RADIO_FreqParameterDetected "[radio] � ���������� ���������� �������.\n"
#define MSGTR_RADIO_DoneParsingChannels "[radio] ������ ���� ������������ ��������.\n"
#define MSGTR_RADIO_GetTunerFailed "[radio] ��������������: ���� ������ ioctl get tuner : %s. frac ���������� � %d.\n"
#define MSGTR_RADIO_NotRadioDevice "[radio] %s �� �������� ����������� �����!\n"
#define MSGTR_RADIO_TunerCapLowYes "[radio] �������������� �����: �� frac=%d\n"
#define MSGTR_RADIO_TunerCapLowNo "[radio] �������������� �����: ��� frac=%d\n"
#define MSGTR_RADIO_SetFreqFailed "[radio] ���� ������ ioctl set frequency 0x%x (%.2f): %s\n"
#define MSGTR_RADIO_GetFreqFailed "[radio] ���� ������ ioctl get frequency: %s\n"
#define MSGTR_RADIO_SetMuteFailed "[radio] ���� ������ ioctl set mute: %s\n"
#define MSGTR_RADIO_QueryControlFailed "[radio] ���� ������ ioctl query control: %s\n"
#define MSGTR_RADIO_GetVolumeFailed "[radio] ���� ������ ioctl get volume: %s\n"
#define MSGTR_RADIO_SetVolumeFailed "[radio] ���� ������ ioctl set volume: %s\n"
#define MSGTR_RADIO_DroppingFrame "\n[radio] ������� ����� ����� (����: %d)!\n"
#define MSGTR_RADIO_BufferEmpty "[radio] grab_audio_frame: ����� ����, �������� ������. ����: %d.\n"
#define MSGTR_RADIO_AudioInitFailed "[radio] ���� ������ audio_in_init: %s\n"
#define MSGTR_RADIO_AudioBuffer "[radio] ����� ������ - �����=%d ���� (����: %d ����).\n"
#define MSGTR_RADIO_AllocateBufferFailed "[radio] ���������� ������� ����� ����� (����=%d,������=%d): %s\n"
#define MSGTR_RADIO_CurrentFreq "[radio] ������� �������: %.2f\n"
#define MSGTR_RADIO_SelectedChannel "[radio] ������� �������: %d - %s (�������: %.2f)\n"
#define MSGTR_RADIO_ChangeChannelNoChannelList "[radio] ���������� �������� �������: �� ������� ������ ������������.\n"
#define MSGTR_RADIO_UnableOpenDevice "[radio] ���������� ������� '%s': %s\n"
#define MSGTR_RADIO_RadioDevice "[radio] Radio fd: %d, %s\n"
#define MSGTR_RADIO_InitFracFailed "[radio] ���� ������ init_frac\n"
#define MSGTR_RADIO_WrongFreq "[radio] �������� �������: %.2f\n"
#define MSGTR_RADIO_UsingFreq "[radio] ������������ �������: %.2f.\n"
#define MSGTR_RADIO_AudioInInitFailed "[radio] ���� ������ audio_in_init\n"
#define MSGTR_RADIO_BufferString "[radio] %s: � ������: %d ��������:%d\n"
#define MSGTR_RADIO_AudioInSetupFailed "[radio] ���� ������ audio_in_setup: %s\n"
#define MSGTR_RADIO_CaptureStarting "[radio] ������ ������ �������.\n"
#define MSGTR_RADIO_ClearBufferFailed "[radio] ������ ������� ������: %s\n"
#define MSGTR_RADIO_StreamEnableCacheFailed "[radio] ������ ������ stream_enable_cache: %s\n"
#define MSGTR_RADIO_DriverUnknownId "[radio] ����������� ��� �������: %d\n"
#define MSGTR_RADIO_DriverUnknownStr "[radio] ����������� �������: %s\n"
#define MSGTR_RADIO_DriverV4L "[radio] ������������ V4Lv1 ����� ���������.\n"
#define MSGTR_RADIO_DriverV4L2 "[radio] ������������ V4Lv2 ����� ���������.\n"

// format.c

#define MSGTR_AF_FORMAT_UnknownFormat "����������� ������ "

// ========================== INPUT =========================================

// joystick.c

#define MSGTR_INPUT_JOYSTICK_Opening "�������� ���������� ��������� %s\n"
#define MSGTR_INPUT_JOYSTICK_CantOpen "�� ���� ������ ���������� ��������� %s: %s\n"
#define MSGTR_INPUT_JOYSTICK_ErrReading "������ ������ ���������� ���������: %s\n"
#define MSGTR_INPUT_JOYSTICK_LoosingBytes "��������: �������� %d ����(�/��) ������\n"
#define MSGTR_INPUT_JOYSTICK_WarnLostSync "��������: �������������� � ������� �������������,\n������� ������������� � ���������.\n"
#define MSGTR_INPUT_JOYSTICK_WarnUnknownEvent "��������: �������������� � ����������� ���� ������� %d\n"

// input.c

#define MSGTR_INPUT_INPUT_ErrCantRegister2ManyCmdFds "������� ����� ���������� ������ ������,\n�� ���� ���������������� �������� ��������� %d.\n"
#define MSGTR_INPUT_INPUT_ErrCantRegister2ManyKeyFds "������� ����� ���������� ������ ������,\n�� ���� ���������������� �������� ��������� %d.\n"
#define MSGTR_INPUT_INPUT_ErrArgMustBeInt "�������� %s: �������� %d �� ����� �����.\n"
#define MSGTR_INPUT_INPUT_ErrArgMustBeFloat "�������� %s: �������� %d �� ������������.\n"
#define MSGTR_INPUT_INPUT_ErrUnterminatedArg "�������� %s: �������� %d �� ������̣�.\n"
#define MSGTR_INPUT_INPUT_ErrUnknownArg "����������� �������� %d\n"
#define MSGTR_INPUT_INPUT_Err2FewArgs "�������� %s ������� �� ����� %d ����������, �� ����� ���� ������ %d.\n"
#define MSGTR_INPUT_INPUT_ErrReadingCmdFd "������ ������ ��������� %d ����� ������: %s\n"
#define MSGTR_INPUT_INPUT_ErrCmdBufferFullDroppingContent "��������� ����� ��������� ��������� %d �����: ��������� ����������.\n"
#define MSGTR_INPUT_INPUT_ErrInvalidCommandForKey "�������� ������� ��� �������� � ������� %s"
#define MSGTR_INPUT_INPUT_ErrSelect "������ ������ select: %s\n"
#define MSGTR_INPUT_INPUT_ErrOnKeyInFd "������ � �������� ��������� %d ������ �����\n"
#define MSGTR_INPUT_INPUT_ErrDeadKeyOnFd "��������� ������ ������� ����� � �������� ��������� %d\n"
#define MSGTR_INPUT_INPUT_Err2ManyKeyDowns "������� ����� ������� �������������� ������� ������\n"
#define MSGTR_INPUT_INPUT_ErrOnCmdFd "������ � ��������� %d ����� ������\n"
#define MSGTR_INPUT_INPUT_ErrReadingInputConfig "������ ������ ����������������� ����� ����� %s: %s\n"
#define MSGTR_INPUT_INPUT_ErrUnknownKey "����������� ������� '%s'\n"
#define MSGTR_INPUT_INPUT_ErrUnfinishedBinding "������������ �������� %s\n"
#define MSGTR_INPUT_INPUT_ErrBuffer2SmallForKeyName "����� ������� ��� ��� �������� ���� �������: %s\n"
#define MSGTR_INPUT_INPUT_ErrNoCmdForKey "�� ������� ������� ��� ������� %s"
#define MSGTR_INPUT_INPUT_ErrBuffer2SmallForCmd "����� ������� ��� ��� ������� %s\n"
#define MSGTR_INPUT_INPUT_ErrWhyHere "��� �� ����� ������?\n"
#define MSGTR_INPUT_INPUT_ErrCantInitJoystick "�� ���� ���������������� �������� �����\n"
#define MSGTR_INPUT_INPUT_ErrCantStatFile "�� ���� ��������� stat %s: %s\n"
#define MSGTR_INPUT_INPUT_ErrCantOpenFile "�� ���� ��������� open %s: %s\n"

// ========================== LIBMPDEMUX ===================================

// url.c

#define MSGTR_MPDEMUX_URL_StringAlreadyEscaped "������, ��� ������ ��� ��������� � url_escape %c%c1%c2\n"

// ai_alsa1x.c

#define MSGTR_MPDEMUX_AIALSA1X_CannotSetSamplerate "�� ���� ������ ������� �������������.\n"
#define MSGTR_MPDEMUX_AIALSA1X_CannotSetBufferTime "�� ���� ������ ����� �����������.\n"
#define MSGTR_MPDEMUX_AIALSA1X_CannotSetPeriodTime "�� ���� ������ ����� �������.\n"

// ai_alsa1x.c / ai_alsa.c

#define MSGTR_MPDEMUX_AIALSA_PcmBrokenConfig "������������ ������������ ��� ������� PCM: ��� ��������� ������������.\n"
#define MSGTR_MPDEMUX_AIALSA_UnavailableAccessType "��� ������� �� ��������.\n"
#define MSGTR_MPDEMUX_AIALSA_UnavailableSampleFmt "������ ������� �� ��������.\n"
#define MSGTR_MPDEMUX_AIALSA_UnavailableChanCount "����� ������� �� �������� - ����������� � ���������: %d\n"
#define MSGTR_MPDEMUX_AIALSA_CannotInstallHWParams "�� ���� ���������� ���������� ���������: %s"
#define MSGTR_MPDEMUX_AIALSA_PeriodEqualsBufferSize "�� ���� ������������ ������, ������ ������� ������ (%u == %lu)\n"
#define MSGTR_MPDEMUX_AIALSA_CannotInstallSWParams "�� ���� ���������� ����������� ���������:\n"
#define MSGTR_MPDEMUX_AIALSA_ErrorOpeningAudio "������ �������� �����: %s\n"
#define MSGTR_MPDEMUX_AIALSA_AlsaStatusError "������ ������� ALSA: %s"
#define MSGTR_MPDEMUX_AIALSA_AlsaXRUN "ALSA xrun!!! (��� ������� ������������� %.3f ��)\n"
#define MSGTR_MPDEMUX_AIALSA_AlsaStatus "������ ALSA:\n"
#define MSGTR_MPDEMUX_AIALSA_AlsaXRUNPrepareError "ALSA xrun: ������ ����������: %s"
#define MSGTR_MPDEMUX_AIALSA_AlsaReadWriteError "������ ������/������ ALSA"

// ai_oss.c

#define MSGTR_MPDEMUX_AIOSS_Unable2SetChanCount "�� ���� ������ ����� �������: %d\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2SetStereo "�� ���� �������� ������: %d\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2Open "�� ���� ������� '%s': %s\n"
#define MSGTR_MPDEMUX_AIOSS_UnsupportedFmt "���������������� ������\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2SetAudioFmt "�� ���� ������ �����������."
#define MSGTR_MPDEMUX_AIOSS_Unable2SetSamplerate "�� ���� ������ ������� �������������: %d\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2SetTrigger "�� ���� ���������� �������: %d\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2GetBlockSize "�� ���� �������� ������ �����!\n"
#define MSGTR_MPDEMUX_AIOSS_AudioBlockSizeZero "������ ���������� �������, ������������ � %d!\n"
#define MSGTR_MPDEMUX_AIOSS_AudioBlockSize2Low "������ ���������� ������� ���, ������������ � %d!\n"

// asfheader.c

#define MSGTR_MPDEMUX_ASFHDR_HeaderSizeOver1MB "��������� ������: ������ ��������� ����� 1 MB (%d)!\n����������, ���������� � ������������� MPlayer � ���������/�������� ���� ����.\n"
#define MSGTR_MPDEMUX_ASFHDR_HeaderMallocFailed "�� ���� �������� %d ����(�/��) ��� ���������.\n"
#define MSGTR_MPDEMUX_ASFHDR_EOFWhileReadingHeader "EOF ��� ������ ��������� ASF, �����������/�������� ����?\n"
#define MSGTR_MPDEMUX_ASFHDR_DVRWantsLibavformat "DVR, ��������, ����� �������� ������ � libavformat,\n���������� -demuxer 35, ���� � ��� ���� ��������\n"
#define MSGTR_MPDEMUX_ASFHDR_NoDataChunkAfterHeader "��� ����� ������, ��������� �� ����������!\n"
#define MSGTR_MPDEMUX_ASFHDR_AudioVideoHeaderNotFound "ASF: �� ������ ����� ��� ����� ��������� - ����������� ����?\n"
#define MSGTR_MPDEMUX_ASFHDR_InvalidLengthInASFHeader "�������� ����� � ��������� ASF!\n"

// asf_mmst_streaming.c

#define MSGTR_MPDEMUX_MMST_WriteError "������ ������\n"
#define MSGTR_MPDEMUX_MMST_EOFAlert "\n�������! EOF\n"
#define MSGTR_MPDEMUX_MMST_PreHeaderReadFailed "������ ������������� �� �������\n"
#define MSGTR_MPDEMUX_MMST_InvalidHeaderSize "�������� ������ ���������, �����ģ���.\n"
#define MSGTR_MPDEMUX_MMST_HeaderDataReadFailed "�� ���� ��������� ������ ���������.\n"
#define MSGTR_MPDEMUX_MMST_packet_lenReadFailed "�� ���� ��������� packet_len.\n"
#define MSGTR_MPDEMUX_MMST_InvalidRTSPPacketSize "�������� ������ ������ RTSP, �����ģ���.\n"
#define MSGTR_MPDEMUX_MMST_CmdDataReadFailed "�� ���� ��������� ����������� ������.\n"
#define MSGTR_MPDEMUX_MMST_HeaderObject "������ ���������\n"
#define MSGTR_MPDEMUX_MMST_DataObject "������ ������\n"
#define MSGTR_MPDEMUX_MMST_FileObjectPacketLen "�������� ������, ����� ������ = %d (%d)\n"
#define MSGTR_MPDEMUX_MMST_StreamObjectStreamID "��������� ������, ID ������: %d\n"
#define MSGTR_MPDEMUX_MMST_2ManyStreamID "������� ����� ID, ����� ��������."
#define MSGTR_MPDEMUX_MMST_UnknownObject "����������� ������\n"
#define MSGTR_MPDEMUX_MMST_MediaDataReadFailed "�� ���� ��������� �����������.\n"
#define MSGTR_MPDEMUX_MMST_MissingSignature "��������� �������\n"
#define MSGTR_MPDEMUX_MMST_PatentedTechnologyJoke "�ӣ �������. ������� �� �������� ����������,\n����������� ������������� � ��������������� ����������.\n"
#define MSGTR_MPDEMUX_MMST_UnknownCmd "����������� ������� %02x\n"
#define MSGTR_MPDEMUX_MMST_GetMediaPacketErr "������ get_media_packet : %s\n"
#define MSGTR_MPDEMUX_MMST_Connected "�����������\n"

// asf_streaming.c

#define MSGTR_MPDEMUX_ASF_StreamChunkSize2Small "��, ������ stream_chunck ������� ���: %d\n"
#define MSGTR_MPDEMUX_ASF_SizeConfirmMismatch "������������� size_confirm!: %d %d\n"
#define MSGTR_MPDEMUX_ASF_WarnDropHeader "��������������: ������� ��������� ????\n"
#define MSGTR_MPDEMUX_ASF_ErrorParsingChunkHeader "������ ������� ��������� �����\n"
#define MSGTR_MPDEMUX_ASF_NoHeaderAtFirstChunk "�� ������� ��������� ��� ������ ����� !!!!\n"
#define MSGTR_MPDEMUX_ASF_BufferMallocFailed "������: �� ���� �������� ����� � %d ����(�/��).\n"
#define MSGTR_MPDEMUX_ASF_ErrReadingNetworkStream "������ ������ �������� ������.\n"
#define MSGTR_MPDEMUX_ASF_ErrChunk2Small "������: ����� ������� ����.\n"
#define MSGTR_MPDEMUX_ASF_ErrSubChunkNumberInvalid "������: ����� ���������� �������.\n"
#define MSGTR_MPDEMUX_ASF_Bandwidth2SmallCannotPlay "C������� �������� ������� ����, ���� �� ����� ���� ��������!\n"
#define MSGTR_MPDEMUX_ASF_Bandwidth2SmallDeselectedAudio "C������� �������� ������� ����, �������� ����������.\n"
#define MSGTR_MPDEMUX_ASF_Bandwidth2SmallDeselectedVideo "C������� �������� ������� ����, �������� ����������.\n"
#define MSGTR_MPDEMUX_ASF_InvalidLenInHeader "�������� ����� � ��������� ASF!\n"
#define MSGTR_MPDEMUX_ASF_ErrReadingChunkHeader "������ ������ ��������� �����.\n"
#define MSGTR_MPDEMUX_ASF_ErrChunkBiggerThanPacket "������: chunk_size > packet_size\n"
#define MSGTR_MPDEMUX_ASF_ErrReadingChunk "������ ������ �����.\n"
#define MSGTR_MPDEMUX_ASF_ASFRedirector "=====> ��������������� ASF\n"
#define MSGTR_MPDEMUX_ASF_InvalidProxyURL "�������� URL ������\n"
#define MSGTR_MPDEMUX_ASF_UnknownASFStreamType "����������� ��� ������ ASF\n"
#define MSGTR_MPDEMUX_ASF_Failed2ParseHTTPResponse "�� ���� ���������������� ����� HTTP.\n"
#define MSGTR_MPDEMUX_ASF_ServerReturn "������ ������ %d:%s\n"
#define MSGTR_MPDEMUX_ASF_ASFHTTPParseWarnCuttedPragma "�������������� ����������� HTTP ASF : Pragma %s ������� �� %d ���� �� %d\n"
#define MSGTR_MPDEMUX_ASF_SocketWriteError "������ ������ ������: %s\n"
#define MSGTR_MPDEMUX_ASF_HeaderParseFailed "�� ���� ��������� ���������.\n"
#define MSGTR_MPDEMUX_ASF_NoStreamFound "����� �� ������.\n"
#define MSGTR_MPDEMUX_ASF_UnknownASFStreamingType "����������� ��� ������ ASF\n"
#define MSGTR_MPDEMUX_ASF_InfoStreamASFURL "STREAM_ASF, URL: %s\n"
#define MSGTR_MPDEMUX_ASF_StreamingFailed "����, ������.\n"

// audio_in.c

#define MSGTR_MPDEMUX_AUDIOIN_ErrReadingAudio "\n������ ������ �����: %s\n"
#define MSGTR_MPDEMUX_AUDIOIN_XRUNSomeFramesMayBeLeftOut "�������������� ����� ���������, ��������� ����� ����� ���� ��������!\n"
#define MSGTR_MPDEMUX_AUDIOIN_ErrFatalCannotRecover "��������� ������, �� ���� ��������������!\n"
#define MSGTR_MPDEMUX_AUDIOIN_NotEnoughSamples "\n������������ �������������!\n"

// aviheader.c

#define MSGTR_MPDEMUX_AVIHDR_EmptyList "** ������ ������?!\n"
#define MSGTR_MPDEMUX_AVIHDR_FoundMovieAt "������ ����� �� 0x%X - 0x%X\n"
#define MSGTR_MPDEMUX_AVIHDR_FoundBitmapInfoHeader "������ 'bih', ������ ����� %u ����(�/��), ������ 'bih' %d ����(�/��)\n"
#define MSGTR_MPDEMUX_AVIHDR_RegeneratingKeyfTableForMPG4V1 "�������������� ������� ������� ������ ��� M$ mpg4v1 �����.\n"
#define MSGTR_MPDEMUX_AVIHDR_RegeneratingKeyfTableForDIVX3 "�������������� ������� ������� ������ ��� DIVX3 �����.\n"
#define MSGTR_MPDEMUX_AVIHDR_RegeneratingKeyfTableForMPEG4 "�������������� ������� ������� ������ ��� MPEG-4 �����.\n"
#define MSGTR_MPDEMUX_AVIHDR_FoundWaveFmt "������ 'wf', ������ ����� %d ����(�/��), ������ 'wh' %d ����(�/��)\n"
#define MSGTR_MPDEMUX_AVIHDR_FoundAVIV2Header "AVI: ������ dmlh (������=%d) (�����_������=%d)\n"
#define MSGTR_MPDEMUX_AVIHDR_ReadingIndexBlockChunksForFrames "����� ���� INDEX, %d ������� ��� %d ������ (fpos=%"PRId64").\n"
#define MSGTR_MPDEMUX_AVIHDR_AdditionalRIFFHdr "�������������� ��������� RIFF...\n"
#define MSGTR_MPDEMUX_AVIHDR_WarnNotExtendedAVIHdr "** ��������������: ��� �� ����������� ��������� AVI..\n"
#define MSGTR_MPDEMUX_AVIHDR_BrokenChunk "����������� �����?  chunksize=%d  (id=%.4s)\n"
#define MSGTR_MPDEMUX_AVIHDR_BuildingODMLidx "AVI: ODML: ���������� ������� ODML (%d ������� ������������).\n"
#define MSGTR_MPDEMUX_AVIHDR_BrokenODMLfile "AVI: ODML: ��������� ������ (��������?) ����. ��������� ������������ ������.\n"
#define MSGTR_MPDEMUX_AVIHDR_CantReadIdxFile "�� ���� ��������� ���� ������� %s: %s\n"
#define MSGTR_MPDEMUX_AVIHDR_NotValidMPidxFile "%s �� �������� ���������� ������ ������� MPlayer.\n"
#define MSGTR_MPDEMUX_AVIHDR_FailedMallocForIdxFile "�� ���� �������� ������ ��� ������ ������� �� %s.\n"
#define MSGTR_MPDEMUX_AVIHDR_PrematureEOF "��������������� ����� ���������� ����� %s\n"
#define MSGTR_MPDEMUX_AVIHDR_IdxFileLoaded "�������� ��������� ����: %s\n"
#define MSGTR_MPDEMUX_AVIHDR_GeneratingIdx "������ ������: %3lu %s     \r"
#define MSGTR_MPDEMUX_AVIHDR_IdxGeneratedForHowManyChunks "AVI: ������� ��������� ������� ��� %d �������!\n"
#define MSGTR_MPDEMUX_AVIHDR_Failed2WriteIdxFile "�� ���� �������� ���� ������ %s: %s\n"
#define MSGTR_MPDEMUX_AVIHDR_IdxFileSaved "�����Σ� ��������� ����: %s\n"

// cache2.c

#define MSGTR_MPDEMUX_CACHE2_NonCacheableStream "\r���� ����� �� ��������.\n"
#define MSGTR_MPDEMUX_CACHE2_ReadFileposDiffers "!!! read_filepos �����������!!! �������� �� ���� ������...\n"

// cdda.c

#define MSGTR_MPDEMUX_CDDA_CantOpenCDDADevice "�� ���� ������� ���������� CDDA.\n"
#define MSGTR_MPDEMUX_CDDA_CantOpenDisc "�� ���� ������� ����.\n"
#define MSGTR_MPDEMUX_CDDA_AudioCDFoundWithNTracks "������ ����� CD � %ld ���������.\n"

// cddb.c

#define MSGTR_MPDEMUX_CDDB_FailedToReadTOC "�� ���� ��������� TOC.\n"
#define MSGTR_MPDEMUX_CDDB_FailedToOpenDevice "�� ���� ������� ���������� %s.\n"
#define MSGTR_MPDEMUX_CDDB_NotAValidURL "�������� URL\n"
#define MSGTR_MPDEMUX_CDDB_FailedToSendHTTPRequest "�� ���� ��������� HTTP ������.\n"
#define MSGTR_MPDEMUX_CDDB_FailedToReadHTTPResponse "�� ���� �������� HTTP �����.\n"
#define MSGTR_MPDEMUX_CDDB_HTTPErrorNOTFOUND "�� ������.\n"
#define MSGTR_MPDEMUX_CDDB_HTTPErrorUnknown "����������� ��� ������\n"
#define MSGTR_MPDEMUX_CDDB_NoCacheFound "��� �� ������.\n"
#define MSGTR_MPDEMUX_CDDB_NotAllXMCDFileHasBeenRead "�� ��� xmcd ����� ���� ���������.\n"
#define MSGTR_MPDEMUX_CDDB_FailedToCreateDirectory "�� ���� ������� ������� %s.\n"
#define MSGTR_MPDEMUX_CDDB_NotAllXMCDFileHasBeenWritten "�� ��� xmcd ����� ���� ��������.\n"
#define MSGTR_MPDEMUX_CDDB_InvalidXMCDDatabaseReturned "������ݣ� �������� ���� ���� ������ xmcd.\n"
#define MSGTR_MPDEMUX_CDDB_UnexpectedFIXME "����������� FIXME\n"
#define MSGTR_MPDEMUX_CDDB_UnhandledCode "�������������� ���\n"
#define MSGTR_MPDEMUX_CDDB_UnableToFindEOL "���������� ����� ����� ������.\n"
#define MSGTR_MPDEMUX_CDDB_ParseOKFoundAlbumTitle "������ �������, �������: %s\n"
#define MSGTR_MPDEMUX_CDDB_AlbumNotFound "������ �� ������.\n"
#define MSGTR_MPDEMUX_CDDB_ServerReturnsCommandSyntaxErr "������ ������: ������ ���������� �������\n"
#define MSGTR_MPDEMUX_CDDB_NoSitesInfoAvailable "���������� ���������� � �����.\n"
#define MSGTR_MPDEMUX_CDDB_FailedToGetProtocolLevel "�� ���� �������� ������� ���������.\n"
#define MSGTR_MPDEMUX_CDDB_NoCDInDrive "��� CD � �������.\n"

// cue_read.c

#define MSGTR_MPDEMUX_CUEREAD_UnexpectedCuefileLine "[bincue] ����������� ������ ����� cue: %s\n"
#define MSGTR_MPDEMUX_CUEREAD_BinFilenameTested "[bincue] ����������� ��� ��������� �����: %s\n"
#define MSGTR_MPDEMUX_CUEREAD_CannotFindBinFile "[bincue] �� ���� ����� �������� ���� - �����ģ���.\n"
#define MSGTR_MPDEMUX_CUEREAD_UsingBinFile "[bincue] ��������� �������� ���� %s.\n"
#define MSGTR_MPDEMUX_CUEREAD_UnknownModeForBinfile "[bincue] ����������� ����� ��� ��������� �����.\n����� �� ������ �����������. ������.\n"
#define MSGTR_MPDEMUX_CUEREAD_CannotOpenCueFile "[bincue] �� ���� ������� %s.\n"
#define MSGTR_MPDEMUX_CUEREAD_ErrReadingFromCueFile "[bincue] ������ ������ �� %s\n"
#define MSGTR_MPDEMUX_CUEREAD_ErrGettingBinFileSize "[bincue] ������ ��������� ������� ��������� �����.\n"
#define MSGTR_MPDEMUX_CUEREAD_InfoTrackFormat "������� %02d:  ������=%d  %02d:%02d:%02d\n"
#define MSGTR_MPDEMUX_CUEREAD_UnexpectedBinFileEOF "[bincue] ����������� ����� ��������� �����\n"
#define MSGTR_MPDEMUX_CUEREAD_CannotReadNBytesOfPayload "[bincue] �� ���� ��������� %d ����(�/��) �������� ��������.\n"
#define MSGTR_MPDEMUX_CUEREAD_CueStreamInfo_FilenameTrackTracksavail "CUE ��������_������, ��� �����=%s, �������=%d, ��������� �������: %d -> %d\n"

// network.c

#define MSGTR_MPDEMUX_NW_UnknownAF "����������� ��������� ������� %d\n"
#define MSGTR_MPDEMUX_NW_ResolvingHostForAF "�������� %s ��� %s...\n"
#define MSGTR_MPDEMUX_NW_CantResolv "�� ���� ��������� ��� ��� %s: %s\n"
#define MSGTR_MPDEMUX_NW_ConnectingToServer "���������� � �������� %s[%s]: %d...\n"
#define MSGTR_MPDEMUX_NW_CantConnect2Server "�� ���� ���������� � ��������: %s\n"
#define MSGTR_MPDEMUX_NW_SelectFailed "Select �� ������.\n"
#define MSGTR_MPDEMUX_NW_ConnTimeout "������� ����������\n"
#define MSGTR_MPDEMUX_NW_GetSockOptFailed "getsockopt �� ������: %s\n"
#define MSGTR_MPDEMUX_NW_ConnectError "������ ����������: %s\n"
#define MSGTR_MPDEMUX_NW_InvalidProxySettingTryingWithout "�������� ��������� ������... ������ ��� ������.\n"
#define MSGTR_MPDEMUX_NW_CantResolvTryingWithoutProxy "�� ���� ��������� ���̣���� ��� ��� AF_INET. ������ ��� ������.\n"
#define MSGTR_MPDEMUX_NW_ErrSendingHTTPRequest "������ �������� HTTP �������: ������ �� ���� ������.\n"
#define MSGTR_MPDEMUX_NW_ReadFailed "������ �� �������.\n"
#define MSGTR_MPDEMUX_NW_Read0CouldBeEOF "http_read_response ��������� 0 (�.�. EOF).\n"
#define MSGTR_MPDEMUX_NW_AuthFailed "������ ��������������. ����������� ����� -user � -passwd ����� ������������ ����\n"\
"�����/������ ��� ������ URL, ��� ����������� URL ��������� �������:\n"\
"http://�����:������@���_�����/����\n"
#define MSGTR_MPDEMUX_NW_AuthRequiredFor "��� %s ��������� ��������������\n"
#define MSGTR_MPDEMUX_NW_AuthRequired "��������� ��������������.\n"
#define MSGTR_MPDEMUX_NW_NoPasswdProvidedTryingBlank "������ �� ������, ������ ������ ������.\n"
#define MSGTR_MPDEMUX_NW_ErrServerReturned "������ ������ %d: %s\n"
#define MSGTR_MPDEMUX_NW_CacheSizeSetTo "���������� ������ ���� %d �����(�/��)\n"

// demux_audio.c

#define MSGTR_MPDEMUX_AUDIO_UnknownFormat "���������������: ����������� ������ %d.\n"

// demux_demuxers.c

#define MSGTR_MPDEMUX_DEMUXERS_FillBufferError "������ ����������_������: ������ ���������������: �� vd, ad ��� sd.\n"

// demux_nuv.c

#define MSGTR_MPDEMUX_NUV_NoVideoBlocksInFile "� ����� ��� �����������.\n"

// demux_xmms.c

#define MSGTR_MPDEMUX_XMMS_FoundPlugin "������ ������: %s (%s).\n"
#define MSGTR_MPDEMUX_XMMS_ClosingPlugin "�������� ������: %s.\n"

// ========================== LIBMPMENU ===================================

// common

#define MSGTR_LIBMENU_NoEntryFoundInTheMenuDefinition "[����] �� ������� ��������� � �������� ����.\n"

// libmenu/menu.c
#define MSGTR_LIBMENU_SyntaxErrorAtLine "[����] ������ ���������� � ������: %d\n"
#define MSGTR_LIBMENU_MenuDefinitionsNeedANameAttrib "[����] �������� ���� ������� �������� �������� (������ %d).\n"
#define MSGTR_LIBMENU_BadAttrib "[����] ������ ������� %s=%s � ���� '%s' � ������ %d\n"
#define MSGTR_LIBMENU_UnknownMenuType "[����] ����������� ��� ���� '%s' � ������ %d\n"
#define MSGTR_LIBMENU_CantOpenConfigFile "[����] �� ���� ������� ���������������� ���� ����: %s\n"
#define MSGTR_LIBMENU_ConfigFileIsTooBig "[����] ���������������� ���� ������� ����� (> %d KB)\n"
#define MSGTR_LIBMENU_ConfigFileIsEmpty "[����] ���������������� ���� ����.\n"
#define MSGTR_LIBMENU_MenuNotFound "[����] ���� %s �� �������.\n"
#define MSGTR_LIBMENU_MenuInitFailed "[����] ���� '%s': ������ �������������.\n"
#define MSGTR_LIBMENU_UnsupportedOutformat "[����] ���������������� �������� ������!!!!\n"

// libmenu/menu_cmdlist.c
#define MSGTR_LIBMENU_ListMenuEntryDefinitionsNeedAName "[����] ��������� ��������� ���� ������ ����� ��� (������ %d).\n"
#define MSGTR_LIBMENU_ListMenuNeedsAnArgument "[����] ���� ������ ��������� ��������.\n"

// libmenu/menu_console.c
#define MSGTR_LIBMENU_WaitPidError "[����] ������ ������ waitpid: %s.\n"
#define MSGTR_LIBMENU_SelectError "[����] ������ ������ select.\n"
#define MSGTR_LIBMENU_ReadErrorOnChilds "[����] ������ ������ ��������� ��������� ���������: %s.\n"
#define MSGTR_LIBMENU_ConsoleRun "[����] ������ �������: %s ...\n"
#define MSGTR_LIBMENU_AChildIsAlreadyRunning "[����] �������� ������� ��� �������.\n"
#define MSGTR_LIBMENU_ForkFailed "[����] ����� fork �� ������ !!!\n"
#define MSGTR_LIBMENU_WriteError "[����] ������ ������\n"

// libmenu/menu_filesel.c
#define MSGTR_LIBMENU_OpendirError "[����] ������ �������� ��������: %s\n"
#define MSGTR_LIBMENU_ReallocError "[����] ������ ����������������� ������: %s\n"
#define MSGTR_LIBMENU_MallocError "[����] ������ ��������� ������: %s\n"
#define MSGTR_LIBMENU_ReaddirError "[����] ������ ������ ��������: %s\n"
#define MSGTR_LIBMENU_CantOpenDirectory "[����] �� ���� ������� ������� %s.\n"

// libmenu/menu_param.c
#define MSGTR_LIBMENU_SubmenuDefinitionNeedAMenuAttribut "[����] ��������� ������� ����� ������� 'menu'.\n"
#define MSGTR_LIBMENU_PrefMenuEntryDefinitionsNeed "[����] ��������� ��������� ���� ������������ ����� ���������� ������� 'property'\n(������ %d).\n"
#define MSGTR_LIBMENU_PrefMenuNeedsAnArgument "[����] ���� ������������ ����� ��������.\n"

// libmenu/menu_pt.c
#define MSGTR_LIBMENU_CantfindTheTargetItem "[����] �� ���� ����� ������� ����� ????\n"
#define MSGTR_LIBMENU_FailedToBuildCommand "[����] �� ���� ��������� �������: %s.\n"

// libmenu/menu_txt.c
#define MSGTR_LIBMENU_MenuTxtNeedATxtFileName "[����] ���������� ���� ����� ��� ���������� ����� (�������� file).\n"
#define MSGTR_LIBMENU_MenuTxtCantOpen "[����] �� ���� ������� %s.\n"
#define MSGTR_LIBMENU_WarningTooLongLineSplitting "[����] ��������������, ������ ������� �������. ��������.\n"
#define MSGTR_LIBMENU_ParsedLines "[����] ���������������� %d �����.\n"

// libmenu/vf_menu.c
#define MSGTR_LIBMENU_UnknownMenuCommand "[����] ����������� �������: '%s'.\n"
#define MSGTR_LIBMENU_FailedToOpenMenu "[����] �� ���� ������� ����: '%s'.\n"

// ========================== LIBMPCODECS ===================================

// libmpcodecs/ad_libdv.c
#define MSGTR_MPCODECS_AudioFramesizeDiffers "[AD_LIBDV] ��������������! ������ ������ ����� ����������! read=%d  hdr=%d.\n"

// libmpcodecs/vd_dmo.c vd_dshow.c vd_vfw.c
#define MSGTR_MPCODECS_CouldntAllocateImageForCinepakCodec "[VD_DMO] �� ���� �������� ����������� ��� ������ cinepak.\n"

// libmpcodecs/vd_ffmpeg.c
#define MSGTR_MPCODECS_XVMCAcceleratedCodec "[VD_FFMPEG] XVMC ���������� �����.\n"
#define MSGTR_MPCODECS_ArithmeticMeanOfQP "[VD_FFMPEG] �������������� ������� QP: %2.4f, ������������� ������� QP: %2.4f\n"
#define MSGTR_MPCODECS_DRIFailure "[VD_FFMPEG] ���� DRI.\n"
#define MSGTR_MPCODECS_CouldntAllocateImageForCodec "[VD_FFMPEG] �� ���� �������� ����������� ��� ������.\n"
#define MSGTR_MPCODECS_XVMCAcceleratedMPEG2 "[VD_FFMPEG] XVMC-���������� MPEG-2.\n"
#define MSGTR_MPCODECS_TryingPixfmt "[VD_FFMPEG] ������ pixfmt=%d.\n"
#define MSGTR_MPCODECS_McGetBufferShouldWorkOnlyWithXVMC "[VD_FFMPEG] ����� mc_get_buffer ������ �������������� ������ � XVMC ����������!!"
#define MSGTR_MPCODECS_UnexpectedInitVoError "[VD_FFMPEG] ����������� ������ init_vo.\n"
#define MSGTR_MPCODECS_UnrecoverableErrorRenderBuffersNotTaken "[VD_FFMPEG] ��������������� ������: �� �������� ������ ����������.\n"
#define MSGTR_MPCODECS_OnlyBuffersAllocatedByVoXvmcAllowed "[VD_FFMPEG] ��������� ������ ������, ���������� vo_xvmc.\n"

// libmpcodecs/ve_lavc.c
#define MSGTR_MPCODECS_HighQualityEncodingSelected "[VE_LAVC] ������� ������������������ ����������� (�� � �������� �������)!\n"
#define MSGTR_MPCODECS_UsingConstantQscale "[VE_LAVC] ��������� ���������� qscale = %f (VBR).\n"

// libmpcodecs/ve_raw.c
#define MSGTR_MPCODECS_OutputWithFourccNotSupported "[VE_RAW] ����� ����� � FourCC [%x] �� ��������������!\n"
#define MSGTR_MPCODECS_NoVfwCodecSpecified "[VE_RAW] ����������� VfW ����� �� ������̣�!!\n"

// libmpcodecs/vf_crop.c
#define MSGTR_MPCODECS_CropBadPositionWidthHeight "[CROP] ������ �������/������/������ - ��������� ������� ��� ���������!\n"

// libmpcodecs/vf_cropdetect.c
#define MSGTR_MPCODECS_CropArea "[CROP] ������� ��������: X: %d..%d  Y: %d..%d  (-vf crop=%d:%d:%d:%d).\n"

// libmpcodecs/vf_format.c, vf_palette.c, vf_noformat.c
#define MSGTR_MPCODECS_UnknownFormatName "[VF_FORMAT] ����������� ��� �������: '%s'.\n"

// libmpcodecs/vf_framestep.c vf_noformat.c vf_palette.c vf_tile.c
#define MSGTR_MPCODECS_ErrorParsingArgument "[VF_FRAMESTEP] ������ ������� ���������.\n"

// libmpcodecs/ve_vfw.c
#define MSGTR_MPCODECS_CompressorType "��� �����������: %.4lx\n"
#define MSGTR_MPCODECS_CompressorSubtype "������ �����������: %.4lx\n"
#define MSGTR_MPCODECS_CompressorFlags "����� �����������: %lu, ������ %lu, ������ ICM: %lu\n"
#define MSGTR_MPCODECS_Flags "�����:"
#define MSGTR_MPCODECS_Quality " ��������"

// libmpcodecs/vf_expand.c
#define MSGTR_MPCODECS_FullDRNotPossible "������ DR ����������, ������ SLICES ������!\n"
#define MSGTR_MPCODECS_WarnNextFilterDoesntSupportSlices  "��������������! ��������� ������ �� ������������ SLICES,\n������������� � ������ ����������� ������ (sig11)...\n"
#define MSGTR_MPCODECS_FunWhydowegetNULL "������ �� �������� NULL??\n"

// libmpcodecs/vf_fame.c
#define MSGTR_MPCODECS_FatalCantOpenlibFAME "��������� ������: �� ���� ������� libFAME!\n"

// libmpcodecs/vf_test.c, vf_yuy2.c, vf_yvu9.c
#define MSGTR_MPCODECS_WarnNextFilterDoesntSupport "%s �� �������������� ��������� ��������/������������ :(\n"

// ================================== LIBMPVO ====================================

// mga_common.c

#define MSGTR_LIBVO_MGA_ErrorInConfigIoctl "[MGA] ������ � mga_vid_config ioctl (�������� ������ mga_vid.o?)"
#define MSGTR_LIBVO_MGA_CouldNotGetLumaValuesFromTheKernelModule "[MGA] �� ���� �������� �������� ���� �� ������ ����!\n"
#define MSGTR_LIBVO_MGA_CouldNotSetLumaValuesFromTheKernelModule "[MGA] �� ���� ���������� �������� ���� �� ������ ����!\n"
#define MSGTR_LIBVO_MGA_ScreenWidthHeightUnknown "[MGA] ������/������ ������ ����������!\n"
#define MSGTR_LIBVO_MGA_InvalidOutputFormat "[MGA] �������� �������� ������ %0X\n"
#define MSGTR_LIBVO_MGA_IncompatibleDriverVersion "[MGA] ������ ������ �������� mga_vid ������������ � ���� ������� MPlayer!\n"
#define MSGTR_LIBVO_MGA_UsingBuffers "[MGA] ��������� %d �������.\n"
#define MSGTR_LIBVO_MGA_CouldntOpen "[MGA] �� ���� �������: %s\n"
#define MGSTR_LIBVO_MGA_ResolutionTooHigh "[MGA] ���������� ���������, �� ������� ���� � ����� ���������, ������ ���\n1023x1023. ����������������� ���������� ��� ����������� -lavdopts lowres=1\n"

// libvo/vesa_lvo.c

#define MSGTR_LIBVO_VESA_ThisBranchIsNoLongerSupported "[VESA_LVO] ��� ����� ������ �� ��������������.\n[VESA_LVO] ����������� -vo vesa:vidix ������.\n"
#define MSGTR_LIBVO_VESA_CouldntOpen "[VESA_LVO] �� ���� �������: '%s'\n"
#define MSGTR_LIBVO_VESA_InvalidOutputFormat "[VESA_LVI] �������� �������� ������: %s(%0X)\n"
#define MSGTR_LIBVO_VESA_IncompatibleDriverVersion "[VESA_LVO] ������ ������ �������� fb_vid ������������ � ���� ������� MPlayer!\n"

// libvo/vo_3dfx.c

#define MSGTR_LIBVO_3DFX_Only16BppSupported "[VO_3DFX] �������������� ������ 16bpp!"
#define MSGTR_LIBVO_3DFX_VisualIdIs "[VO_3DFX] ���������� ID  %lx.\n"
#define MSGTR_LIBVO_3DFX_UnableToOpenDevice "[VO_3DFX] �� ���� ������� /dev/3dfx.\n"
#define MSGTR_LIBVO_3DFX_Error "[VO_3DFX] ������: %d.\n"
#define MSGTR_LIBVO_3DFX_CouldntMapMemoryArea "[VO_3DFX] �� ���� ���������� ������� ������ 3dfx: %p,%p,%d.\n"
#define MSGTR_LIBVO_3DFX_DisplayInitialized "[VO_3DFX] ����������������: %p.\n"
#define MSGTR_LIBVO_3DFX_UnknownSubdevice "[VO_3DFX] ����������� �������������: %s.\n"

// libvo/vo_dxr3.c

#define MSGTR_LIBVO_DXR3_UnableToLoadNewSPUPalette "[VO_DXR3] �� ���� ��������� ����� ������� SPU!\n"
#define MSGTR_LIBVO_DXR3_UnableToSetPlaymode "[VO_DXR3] �� ���� ������ ����� ���������������!\n"
#define MSGTR_LIBVO_DXR3_UnableToSetSubpictureMode "[VO_DXR3] �� ���� ������ ����� �����������!\n"
#define MSGTR_LIBVO_DXR3_UnableToGetTVNorm "[VO_DXR3] �� ���� �������� ����� ��!\n"
#define MSGTR_LIBVO_DXR3_AutoSelectedTVNormByFrameRate "[VO_DXR3] ��������� ������ �� �� ������� ������: "
#define MSGTR_LIBVO_DXR3_UnableToSetTVNorm "[VO_DXR3] �� ���� ���������� ����� ��!\n"
#define MSGTR_LIBVO_DXR3_SettingUpForNTSC "[VO_DXR3] ������������ ��� NTSC.\n"
#define MSGTR_LIBVO_DXR3_SettingUpForPALSECAM "[VO_DXR3] ������������ ��� PAL/SECAM.\n"
#define MSGTR_LIBVO_DXR3_SettingAspectRatioTo43 "[VO_DXR3] ������������ ��������� 4:3.\n"
#define MSGTR_LIBVO_DXR3_SettingAspectRatioTo169 "[VO_DXR3] ������������ ��������� 16:9.\n"
#define MSGTR_LIBVO_DXR3_OutOfMemory "[VO_DXR3] �������� ������\n"
#define MSGTR_LIBVO_DXR3_UnableToAllocateKeycolor "[VO_DXR3] �� ���� ��������� �������� ����!\n"
#define MSGTR_LIBVO_DXR3_UnableToAllocateExactKeycolor "[VO_DXR3] �� ���� ��������� ������ �������� ����,\n��������� ��������� ����������� (0x%lx).\n"
#define MSGTR_LIBVO_DXR3_Uninitializing "[VO_DXR3] ���������������.\n"
#define MSGTR_LIBVO_DXR3_FailedRestoringTVNorm "[VO_DXR3] �� ���� ������������ ����� ��!\n"
#define MSGTR_LIBVO_DXR3_EnablingPrebuffering "[VO_DXR3] ������� ���������������.\n"
#define MSGTR_LIBVO_DXR3_UsingNewSyncEngine "[VO_DXR3] ��������� ����� �������� �������������.\n"
#define MSGTR_LIBVO_DXR3_UsingOverlay "[VO_DXR3] ��������� �������.\n"
#define MSGTR_LIBVO_DXR3_ErrorYouNeedToCompileMplayerWithX11 "[VO_DXR3] ������: ��� ������� ���������� ���������� � ��������������\n������������/����������� X11.\n"
#define MSGTR_LIBVO_DXR3_WillSetTVNormTo "[VO_DXR3] ��������� ����� �� �: "
#define MSGTR_LIBVO_DXR3_AutoAdjustToMovieFrameRatePALPAL60 "��������������� � ������� ������ ������ (PAL/PAL-60)"
#define MSGTR_LIBVO_DXR3_AutoAdjustToMovieFrameRatePALNTSC "��������������� � ������� ������ ������ (PAL/NTSC)"
#define MSGTR_LIBVO_DXR3_UseCurrentNorm "��������� ������� �����."
#define MSGTR_LIBVO_DXR3_UseUnknownNormSuppliedCurrentNorm "��������� ����������� �����. ��������� ������� �����."
#define MSGTR_LIBVO_DXR3_ErrorOpeningForWritingTrying "[VO_DXR3] ������ �������� %s ��� ������, ������ /dev/em8300 ������.\n"
#define MSGTR_LIBVO_DXR3_ErrorOpeningForWritingTryingMV "[VO_DXR3] ������ �������� %s ��� ������, ������ /dev/em8300_mv ������.\n"
#define MSGTR_LIBVO_DXR3_ErrorOpeningForWritingAsWell "[VO_DXR3] ����� ������ ������� /dev/em8300 ��� ������!\n������.\n"
#define MSGTR_LIBVO_DXR3_ErrorOpeningForWritingAsWellMV "[VO_DXR3] ����� ������ ������� /dev/em8300_mv ��� ������!\n������.\n"
#define MSGTR_LIBVO_DXR3_Opened "[VO_DXR3] �������: %s.\n"
#define MSGTR_LIBVO_DXR3_ErrorOpeningForWritingTryingSP "[VO_DXR3] ������ �������� %s ��� ������, ������ /dev/em8300_sp ������.\n"
#define MSGTR_LIBVO_DXR3_ErrorOpeningForWritingAsWellSP "[VO_DXR3] ����� ������ ������� /dev/em8300_sp ��� ������!\n������.\n"
#define MSGTR_LIBVO_DXR3_UnableToOpenDisplayDuringHackSetup "[VO_DXR3] �� ���� ������� ������� � �������� ��������� ���� �������!\n"
#define MSGTR_LIBVO_DXR3_UnableToInitX11 "[VO_DXR3] �� ���� ���������������� X11!\n"
#define MSGTR_LIBVO_DXR3_FailedSettingOverlayAttribute "[VO_DXR3] �� ���� ���������� ������� �������.\n"
#define MSGTR_LIBVO_DXR3_FailedSettingOverlayScreen "[VO_DXR3] �� ���� ���������� ����� �������!\n������.\n"
#define MSGTR_LIBVO_DXR3_FailedEnablingOverlay "[VO_DXR3] �� ���� ������������ �������!\n������.\n"
#define MSGTR_LIBVO_DXR3_FailedResizingOverlayWindow "[VO_DXR3] �� ���� �������� ������ ���� �������!\n"
#define MSGTR_LIBVO_DXR3_FailedSettingOverlayBcs "[VO_DXR3] �� ���� ���������� bcs �������!\n"
#define MSGTR_LIBVO_DXR3_FailedGettingOverlayYOffsetValues "[VO_DXR3] �� ���� �������� �������� Y-�������� �������!\n������.\n"
#define MSGTR_LIBVO_DXR3_FailedGettingOverlayXOffsetValues "[VO_DXR3] �� ���� �������� �������� X-�������� �������!\n������.\n"
#define MSGTR_LIBVO_DXR3_FailedGettingOverlayXScaleCorrection "[VO_DXR3] �� ���� �������� ��������� �� ��������������� �� X �������!\n������.\n"
#define MSGTR_LIBVO_DXR3_YOffset "[VO_DXR3] �������� �� Y: %d.\n"
#define MSGTR_LIBVO_DXR3_XOffset "[VO_DXR3] �������� �� X: %d.\n"
#define MSGTR_LIBVO_DXR3_XCorrection "[VO_DXR3] ��������� �� X: %d.\n"
#define MSGTR_LIBVO_DXR3_FailedSetSignalMix "[VO_DXR3] �� ���� ���������� ������ mix!\n"

// libvo/vo_mga.c

#define MSGTR_LIBVO_MGA_AspectResized "[VO_MGA] aspect(): ����Σ� ������ � %dx%d.\n"
#define MSGTR_LIBVO_MGA_Uninit "[VO] ���������������!\n"

// libvo/vo_null.c

#define MSGTR_LIBVO_NULL_UnknownSubdevice "[VO_NULL] ����������� �������������: %s.\n"

// libvo/vo_png.c

#define MSGTR_LIBVO_PNG_Warning1 "[VO_PNG] ��������������: ������� ������ ���������� � 0, ������ ���������!\n"
#define MSGTR_LIBVO_PNG_Warning2 "[VO_PNG] ����������: ����������� -vo png:z=<n> ��� ��������� ������\n������ �� 0 �� 9.\n"
#define MSGTR_LIBVO_PNG_Warning3 "[VO_PNG] ����������: (0 = ��� ������, 1 = �������, ������ - 9 ������,\n����� ��������� ������)\n"
#define MSGTR_LIBVO_PNG_ErrorOpeningForWriting "\n[VO_PNG] ������ �������� '%s' ��� ������!\n"
#define MSGTR_LIBVO_PNG_ErrorInCreatePng "[VO_PNG] ������ � create_png.\n"

// libvo/vo_sdl.c

#define MSGTR_LIBVO_SDL_CouldntGetAnyAcceptableSDLModeForOutput "[VO_SDL] �� ���� �������� �����-���� ���������� ����� SDL ��� ������.\n"
#define MSGTR_LIBVO_SDL_SetVideoModeFailed "[VO_SDL] set_video_mode: ���� SDL_SetVideoMode: %s.\n"
#define MSGTR_LIBVO_SDL_SetVideoModeFailedFull "[VO_SDL] Set_fullmode: ���� SDL_SetVideoMode: %s.\n"
#define MSGTR_LIBVO_SDL_MappingI420ToIYUV "[VO_SDL] ����������� I420 � IYUV.\n"
#define MSGTR_LIBVO_SDL_UnsupportedImageFormat "[VO_SDL] ���������������� ������ ����������� (0x%X).\n"
#define MSGTR_LIBVO_SDL_InfoPleaseUseVmOrZoom "[VO_SDL] ����������: ����������� -vm ��� -zoom ��� ������������\n� ������ ����������.\n"
#define MSGTR_LIBVO_SDL_FailedToSetVideoMode "[VO_SDL] �� ���� ���������� ����������: %s.\n"
#define MSGTR_LIBVO_SDL_CouldntCreateAYUVOverlay "[VO_SDL] �� ���� ������� ������� YUV: %s.\n"
#define MSGTR_LIBVO_SDL_CouldntCreateARGBSurface "[VO_SDL] �� ���� ������� ����������� RGB: %s.\n"
#define MSGTR_LIBVO_SDL_UsingDepthColorspaceConversion "[VO_SDL] ��������� �������������� �������/��������� ������������,\n��� �������� ������ (%ibpp -> %ibpp).\n"
#define MSGTR_LIBVO_SDL_UnsupportedImageFormatInDrawslice "[VO_SDL] ���������������� ������ ����������� � draw_slice,\n��������� � �������������� MPlayer!\n"
#define MSGTR_LIBVO_SDL_BlitFailed "[VO_SDL] ���� blit: %s.\n"
#define MSGTR_LIBVO_SDL_InitializationFailed "[VO_SDL] ���� ������������� SDL: %s.\n"
#define MSGTR_LIBVO_SDL_UsingDriver "[VO_SDL] ��������� �������: %s.\n"

// libvo/vobsub_vidix.c

#define MSGTR_LIBVO_SUB_VIDIX_CantStartPlayback "[VO_SUB_VIDIX] �� ���� ������ ���������������: %s\n"
#define MSGTR_LIBVO_SUB_VIDIX_CantStopPlayback "[VO_SUB_VIDIX] �� ���� ���������� ���������������: %s\n"
#define MSGTR_LIBVO_SUB_VIDIX_InterleavedUvForYuv410pNotSupported "[VO_SUB_VIDIX] ��ϣ��� UV ��� YUV410P �� ��������������.\n"
#define MSGTR_LIBVO_SUB_VIDIX_DummyVidixdrawsliceWasCalled "[VO_SUB_VIDIX] ��� ������ ��������� vidix_draw_slice().\n"
#define MSGTR_LIBVO_SUB_VIDIX_DummyVidixdrawframeWasCalled "[VO_SUB_VIDIX] ��� ������ ��������� vidix_draw_frame().\n"
#define MSGTR_LIBVO_SUB_VIDIX_UnsupportedFourccForThisVidixDriver "[VO_SUB_VIDIX] ���������������� FourCC ��� ����� �������� VIDIX: %x (%s).\n"
#define MSGTR_LIBVO_SUB_VIDIX_VideoServerHasUnsupportedResolution "[VO_SUB_VIDIX] � ������������ ���������� (%dx%d) �� ��������������,\n��������������: %dx%d-%dx%d.\n"
#define MSGTR_LIBVO_SUB_VIDIX_VideoServerHasUnsupportedColorDepth "[VO_SUB_VIDIX] ����������� �� ������������ ������� ����� vidix (%d).\n"
#define MSGTR_LIBVO_SUB_VIDIX_DriverCantUpscaleImage "[VO_SUB_VIDIX] ������� VIDIX �� ����� ��������� ����������� (%d%d -> %d%d).\n"
#define MSGTR_LIBVO_SUB_VIDIX_DriverCantDownscaleImage "[VO_SUB_VIDIX] ������� VIDIX �� ����� ��������� ����������� (%d%d -> %d%d).\n"
#define MSGTR_LIBVO_SUB_VIDIX_CantConfigurePlayback "[VO_SUB_VIDIX] �� ���� ��������� ���������������: %s.\n"
#define MSGTR_LIBVO_SUB_VIDIX_YouHaveWrongVersionOfVidixLibrary "[VO_SUB_VIDIX] � ��� �������� ������ ���������� VIDIX.\n"
#define MSGTR_LIBVO_SUB_VIDIX_CouldntFindWorkingVidixDriver "[VO_SUB_VIDIX] �� ���� ����� ���������� ������� VIDIX.\n"
#define MSGTR_LIBVO_SUB_VIDIX_CouldntGetCapability "[VO_SUB_VIDIX] �� ���� �������� �����������: %s.\n"
#define MSGTR_LIBVO_SUB_VIDIX_Description "[VO_SUB_VIDIX] ��������: %s.\n"
#define MSGTR_LIBVO_SUB_VIDIX_Author "[VO_SUB_VIDIX] �����: %s.\n"

// libvo/vo_svga.c

#define MSGTR_LIBVO_SVGA_ForcedVidmodeNotAvailable "[VO_SVGA] ������������� vid_mode %d (%s) �� ��������.\n"
#define MSGTR_LIBVO_SVGA_ForcedVidmodeTooSmall "[VO_SVGA] ������������� vid_mode %d (%s) ������� ���.\n"
#define MSGTR_LIBVO_SVGA_Vidmode "[VO_SVGA] Vid_mode: %d, %dx%d %dbpp.\n"
#define MSGTR_LIBVO_SVGA_VgasetmodeFailed "[VO_SVGA] ���� Vga_setmode(%d).\n"
#define MSGTR_LIBVO_SVGA_VideoModeIsLinearAndMemcpyCouldBeUsed "[VO_SVGA] ���������� �������� � ��� �������� ����������� ����� ����\n����������� memcpy.\n"
#define MSGTR_LIBVO_SVGA_VideoModeHasHardwareAcceleration "[VO_SVGA] ���������� �������� ���������� ���������� � ����� ����\n����������� put_image.\n"
#define MSGTR_LIBVO_SVGA_IfItWorksForYouIWouldLikeToKnow "[VO_SVGA] ���� ��� �������� � ���, ��� �������� �� �����.\n[VO_SVGA] (��������� ��� � ������� `mplayer test.avi -v -v -v -v &> svga.log`).\n�������!\n"
#define MSGTR_LIBVO_SVGA_VideoModeHas "[VO_SVGA] � ����������� %d �������(�/�).\n"
#define MSGTR_LIBVO_SVGA_CenteringImageStartAt "[VO_SVGA] ��������� �����������. ������� � (%d,%d)\n"
#define MSGTR_LIBVO_SVGA_UsingVidix "[VO_SVGA] ��������� VIDIX. w=%i h=%i  mw=%i mh=%i\n"

// libvo/vo_syncfb.c

#define MSGTR_LIBVO_SYNCFB_CouldntOpen "[VO_SYNCFB] �� ���� ������� /dev/syncfb ��� /dev/mga_vid.\n"
#define MSGTR_LIBVO_SYNCFB_UsingPaletteYuv420p3 "[VO_SYNCFB] ��������� ������� YUV420P3.\n"
#define MSGTR_LIBVO_SYNCFB_UsingPaletteYuv420p2 "[VO_SYNCFB] ��������� ������� YUV420P2.\n"
#define MSGTR_LIBVO_SYNCFB_UsingPaletteYuv420 "[VO_SYNCFB] ��������� ������� YUV420.\n"
#define MSGTR_LIBVO_SYNCFB_NoSupportedPaletteFound "[VO_SYNCFB] �� ������� �������������� ������.\n"
#define MSGTR_LIBVO_SYNCFB_BesSourcerSize "[VO_SYNCFB] ������ ���������� ���������� BES: %d x %d.\n"
#define MSGTR_LIBVO_SYNCFB_FramebufferMemory "[VO_SYNCFB] ������ �����������: %ld � %ld ������(��).\n"
#define MSGTR_LIBVO_SYNCFB_RequestingFirstBuffer "[VO_SYNCFB] ���������� ������ ����� #%d.\n"
#define MSGTR_LIBVO_SYNCFB_GotFirstBuffer "[VO_SYNCFB] ������� ������ ����� #%d.\n"
#define MSGTR_LIBVO_SYNCFB_UnknownSubdevice "[VO_SYNCFB] ����������� �������������: %s.\n"

// libvo/vo_tdfxfb.c

#define MSGTR_LIBVO_TDFXFB_CantOpen "[VO_TDFXFB] �� ���� ������� %s: %s.\n"
#define MSGTR_LIBVO_TDFXFB_ProblemWithFbitgetFscreenInfo "[VO_TDFXFB] �������� � ioctl FBITGET_FSCREENINFO: %s.\n"
#define MSGTR_LIBVO_TDFXFB_ProblemWithFbitgetVscreenInfo "[VO_TDFXFB] �������� � ioctl FBITGET_VSCREENINFO: %s.\n"
#define MSGTR_LIBVO_TDFXFB_ThisDriverOnlySupports "[VO_TDFXFB] ���� ������� ������������ ������ 3Dfx Banshee, Voodoo3 � Voodoo 5.\n"
#define MSGTR_LIBVO_TDFXFB_OutputIsNotSupported "[VO_TDFXFB] %d bpp ����� �� ��������������.\n"
#define MSGTR_LIBVO_TDFXFB_CouldntMapMemoryAreas "[VO_TDFXFB] �� ���� ���������� ������� ������: %s.\n"
#define MSGTR_LIBVO_TDFXFB_BppOutputIsNotSupported "[VO_TDFXFB] %d bpp ����� �� ��������������\n(����� ������� �� ������ �����������).\n"
#define MSGTR_LIBVO_TDFXFB_SomethingIsWrongWithControl "[VO_TDFXFB] ��! ���-�� �� � ������� � control().\n"
#define MSGTR_LIBVO_TDFXFB_NotEnoughVideoMemoryToPlay "[VO_TDFXFB] ������������ ������ ��� ��������������� ����� ������.\n���������� ������� ����������.\n"
#define MSGTR_LIBVO_TDFXFB_ScreenIs "[VO_TDFXFB] ����� %dx%d � %d bpp, ������� ������ %dx%d � %d bpp, ����� %dx%d.\n"

// libvo/vo_tdfx_vid.c

#define MSGTR_LIBVO_TDFXVID_Move "[VO_TDXVID] ��� %d(%d) x %d => %d.\n"
#define MSGTR_LIBVO_TDFXVID_AGPMoveFailedToClearTheScreen "[VO_TDFXVID] ���� ������� ������ ����� AGP.\n"
#define MSGTR_LIBVO_TDFXVID_BlitFailed "[VO_TDFXVID] ���� blit.\n"
#define MSGTR_LIBVO_TDFXVID_NonNativeOverlayFormatNeedConversion "[VO_TDFXVID] ��� ������������ �������� ������� ����� ���������.\n"
#define MSGTR_LIBVO_TDFXVID_UnsupportedInputFormat "[VO_TDFXVID] ���������������� ������� ������ 0x%x.\n"
#define MSGTR_LIBVO_TDFXVID_OverlaySetupFailed "[VO_TDFXVID] ���� ��������� �������.\n"
#define MSGTR_LIBVO_TDFXVID_OverlayOnFailed "[VO_TDFXVID] ���� ��������� �������.\n"
#define MSGTR_LIBVO_TDFXVID_OverlayReady "[VO_TDFXVID] ������� �����: %d(%d) x %d @ %d => %d(%d) x %d @ %d.\n"
#define MSGTR_LIBVO_TDFXVID_TextureBlitReady "[VO_TDFXVID] ����� blit �������: %d(%d) x %d @ %d => %d(%d) x %d @ %d.\n"
#define MSGTR_LIBVO_TDFXVID_OverlayOffFailed "[VO_TDFXVID] ���� ���������� �������\n"
#define MSGTR_LIBVO_TDFXVID_CantOpen "[VO_TDFXVID] �� ���� ������� %s: %s.\n"
#define MSGTR_LIBVO_TDFXVID_CantGetCurrentCfg "[VO_TDFXVID] �� ���� �������� ������� ������������: %s.\n"
#define MSGTR_LIBVO_TDFXVID_MemmapFailed "[VO_TDFXVID] ���� memmap !!!!!\n"
#define MSGTR_LIBVO_TDFXVID_GetImageTodo "����� �������� ��������� �����������.\n"
#define MSGTR_LIBVO_TDFXVID_AgpMoveFailed "[VO_TDFXVID] ���� ���� AGP.\n"
#define MSGTR_LIBVO_TDFXVID_SetYuvFailed "[VO_TDFXVID] ���� ��������� YUV.\n"
#define MSGTR_LIBVO_TDFXVID_AgpMoveFailedOnYPlane "[VO_TDFXVID] ���� ���� AGP �� �������� Y.\n"
#define MSGTR_LIBVO_TDFXVID_AgpMoveFailedOnUPlane "[VO_TDFXVID] ���� ���� AGP �� �������� U.\n"
#define MSGTR_LIBVO_TDFXVID_AgpMoveFailedOnVPlane "[VO_TDFXVID] ���� ���� AGP �� �������� V.\n"
#define MSGTR_LIBVO_TDFXVID_UnknownFormat "[VO_TDFXVID] ����������� ������: 0x%x.\n"

// libvo/vo_tga.c

#define MSGTR_LIBVO_TGA_UnknownSubdevice "[VO_TGA] ����������� �������������: %s.\n"

// libvo/vo_vesa.c

#define MSGTR_LIBVO_VESA_FatalErrorOccurred "[VO_VESA] �������� ��������� ������! �� ���� ����������.\n"
#define MSGTR_LIBVO_VESA_UnkownSubdevice "[VO_VESA] ����������� �������������: '%s'.\n"
#define MSGTR_LIBVO_VESA_YouHaveTooLittleVideoMemory "[VO_VESA] � ��� ������� ���� ����������� ��� ����� ������:\n[VO_VESA] ����������: %08lX �������: %08lX.\n"
#define MSGTR_LIBVO_VESA_YouHaveToSpecifyTheCapabilitiesOfTheMonitor "[VO_VESA] ��� ����� ������� ����������� ��������. �� ������� ������� ����������.\n"
#define MSGTR_LIBVO_VESA_UnableToFitTheMode "[VO_VESA] ����� �� ������������ ������������ ��������. �� ������� ������� ����������.\n"
#define MSGTR_LIBVO_VESA_DetectedInternalFatalError "[VO_VESA] ���������� ���������� ��������� ������: init ������ �� preinit.\n"
#define MSGTR_LIBVO_VESA_SwitchFlipIsNotSupported "[VO_VESA] ����� -flip �� ��������������.\n"
#define MSGTR_LIBVO_VESA_PossibleReasonNoVbe2BiosFound "[VO_VESA] ��������� �������: VBE2 BIOS �� ������.\n"
#define MSGTR_LIBVO_VESA_FoundVesaVbeBiosVersion "[VO_VESA] ������ VESA VBE BIOS, ������ %x.%x, �������: %x.\n"
#define MSGTR_LIBVO_VESA_VideoMemory "[VO_VESA] �����������: %u ��.\n"
#define MSGTR_LIBVO_VESA_Capabilites "[VO_VESA] ����������� VESA: %s %s %s %s %s.\n"
#define MSGTR_LIBVO_VESA_BelowWillBePrintedOemInfo "[VO_VESA] !!! ���� ����� �������� ���������� OEM !!!\n"
#define MSGTR_LIBVO_VESA_YouShouldSee5OemRelatedLines "[VO_VESA] �� ������ ���� ������ 5 ����� ���������� OEM.\n���� ���, � ��� ����������� vm86.\n"
#define MSGTR_LIBVO_VESA_OemInfo "[VO_VESA] ���������� OEM: %s.\n"
#define MSGTR_LIBVO_VESA_OemRevision "[VO_VESA] ������� OEM: %x.\n"
#define MSGTR_LIBVO_VESA_OemVendor "[VO_VESA] ��������� OEM: %s.\n"
#define MSGTR_LIBVO_VESA_OemProductName "[VO_VESA] ������������ �������� OEM: %s.\n"
#define MSGTR_LIBVO_VESA_OemProductRev "[VO_VESA] ������� �������� OEM: %s.\n"
#define MSGTR_LIBVO_VESA_Hint "[VO_VESA] ���������: ��� ������ ��-������ ��� ���������� ���������� ���ߣ� ��\n"\
"[VO_VESA] �� ��������, �.�. VESA BIOS ���������������� ������ � �������� POST.\n"
#define MSGTR_LIBVO_VESA_UsingVesaMode "[VO_VESA] ��������� ����� VESA (%u) = %x [%ux%u@%u]\n"
#define MSGTR_LIBVO_VESA_CantInitializeSwscaler "[VO_VESA] �� ���� ���������������� ����������� ���������������.\n"
#define MSGTR_LIBVO_VESA_CantUseDga "[VO_VESA] �� ���� ������������ DGA. �������� ����� ���������� ���������. :(\n"
#define MSGTR_LIBVO_VESA_UsingDga "[VO_VESA] ��������� DGA (���������� �������: %08lXh, %08lXh)"
#define MSGTR_LIBVO_VESA_CantUseDoubleBuffering "[VO_VESA] �� ���� ������������ ������� �����������: ������������ �����������.\n"
#define MSGTR_LIBVO_VESA_CantFindNeitherDga "[VO_VESA] �� ���� ����� �� DGA �� ������������� ������ ����.\n"
#define MSGTR_LIBVO_VESA_YouveForcedDga "[VO_VESA] �� ����������� DGA. ������\n"
#define MSGTR_LIBVO_VESA_CantFindValidWindowAddress "[VO_VESA] �� ���� ����� ���������� ����� ����.\n"
#define MSGTR_LIBVO_VESA_UsingBankSwitchingMode "[VO_VESA] ��������� ����� ���������� ���������\n(���������� �������: %08lXh, %08lXh).\n"
#define MSGTR_LIBVO_VESA_CantAllocateTemporaryBuffer "[VO_VESA] �� ���� �������� ��������� �����.\n"
#define MSGTR_LIBVO_VESA_SorryUnsupportedMode "[VO_VESA] ��������, ���������������� ����� -- ���������� -x 640 -zoom.\n"
#define MSGTR_LIBVO_VESA_OhYouReallyHavePictureOnTv "[VO_VESA] �, � ��� ������������� ���� ����������� �� ��!\n"
#define MSGTR_LIBVO_VESA_CantInitialozeLinuxVideoOverlay "[VO_VESA] �� ���� ���������������� ����������� Linux.\n"
#define MSGTR_LIBVO_VESA_UsingVideoOverlay "[VO_VESA] ��������� ������������: %s.\n"
#define MSGTR_LIBVO_VESA_CantInitializeVidixDriver "[VO_VESA] �� ���� ���������������� ������� VIDIX.\n"
#define MSGTR_LIBVO_VESA_UsingVidix "[VO_VESA] ��������� VIDIX.\n"
#define MSGTR_LIBVO_VESA_CantFindModeFor "[VO_VESA] �� ���� ����� ����� ���: %ux%u@%u.\n"
#define MSGTR_LIBVO_VESA_InitializationComplete "[VO_VESA] ������������� VESA ���������.\n"

// libvo/vo_x11.c

#define MSGTR_LIBVO_X11_DrawFrameCalled "[VO_X11] ������ draw_frame()!!!!!!\n"

// libvo/vo_xv.c

#define MSGTR_LIBVO_XV_DrawFrameCalled "[VO_XV] ������ draw_frame()!!!!!!\n"

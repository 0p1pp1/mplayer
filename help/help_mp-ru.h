/* Translated by:  Nick Kurshev <nickols_k@mail.ru>,
 *		Dmitry Baryshkov <lumag@qnc.ru>
   Was synced with help_mp-en.h: rev 1.111
 ========================= MPlayer help =========================== */

#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"������:   mplayer [�����] [URL|path/]filename\n"
"\n"
"������� �����: (������ ������ ��. �� man-��������)\n"
" -vo <drv[:dev]> ����� �������� � ���������� ����� ������ (������ ��. � '-vo help')\n"
" -ao <drv[:dev]> ����� �������� � ���������� ����� ������ (������ ��. � '-ao help')\n"
#ifdef HAVE_VCD
" vcd://<����� �����> ������ ������� VCD (video cd) � ���������� ������ �����\n"
#endif
#ifdef HAVE_LIBCSS
" -dvdauth <dev>  ����� ���������� DVD ��� ����������� (��� ����������� ������)\n"
#endif
#ifdef USE_DVDREAD
" dvd://<����� ������> ������ DVD ����� � ���������� ������ �����\n"
" -alang/-slang   ������� ���� �����/��������� DVD (������������ ��� ������)\n"
#endif
" -ss <�����>     ������������� �� �������� (������� ��� ��:��:��) �������\n"
" -nosound        ��� �����\n"
" -fs             ����� �������������� ������������ (fullscr,vidmode chg,softw.scale)\n"
" -x <x> -y <y>   ���������� ���������� ������� (������������ � -vm ��� -zoom)\n"
" -sub <����>     ������� ���� ��������� (��. ����� -subfps, -subdelay)\n"
" -playlist <����> ������� playlist\n"
" -vid x -aid y   ����� ��� ������ ����� (x) � ����� (y) ������ ��� ������������\n"
" -fps x -srate y ����� ��� ��������� ����� (x ����/���) � ����� (y Hz) ��������\n"
" -pp <quality>   ��������� ������ ������������� (����������� �� man-��������)\n"
" -framedrop      �������� ������������ ������ (��� ��������� �����)\n"
"\n"
"�������� ������: (������ ������ � �������� man, ����� ������ input.conf)\n"
" <-  ��� ->      ����������� ���ң�/����� �� 10 ������\n"
" up ��� down     ����������� ���ң�/����� ��  1 ������\n"
" pgup or pgdown  ����������� ���ң�/����� �� 10 �����\n"
" < ��� >         ����������� ���ң�/����� � playlist'�\n"
" p ��� ������    ������������� ����� (����� ������� - ����������)\n"
" q ��� ESC       ���������� ��������������� � �����\n"
" + ��� -         ������������ �������� ����� �� +/- 0.1 �������\n"
" o               ��������� ������� OSD �������:  ��� / ��������� / ���������+������\n"
" * ��� /         ��������� ��� ������� PCM ���������\n"
" z ��� x         ������������ �������� ��������� �� +/- 0.1 �������\n"
" r ��� t         ����������� ������������ ������� ���������,��. ����� -vf expand\n"
"\n"
" * * * ��������� ��. ������������, � �������������� ������ � ������! * * *\n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// mplayer.c: 

#define MSGTR_Exiting "\n�������... (%s)\n"
#define MSGTR_Exit_quit "�����"
#define MSGTR_Exit_eof "����� �����"
#define MSGTR_Exit_error "��������� ������"
#define MSGTR_IntBySignal "\nMPlayer ������� �������� %d � ������: %s \n"
#define MSGTR_NoHomeDir "�� ���� ����� HOME(��������) �������\n"
#define MSGTR_GetpathProblem "�������� � get_path(\"config\")\n"
#define MSGTR_CreatingCfgFile "�������� ����� ������������: %s\n"
#define MSGTR_InvalidVOdriver "������������ ��� �������� ����� ������: %s\n��. '-vo help' ����� �������� ������ ��������� ���������.\n"
#define MSGTR_InvalidAOdriver "������������ ��� �������� ����� ������: %s\n��. '-ao help' ����� �������� ������ ��������� ���������.\n"
#define MSGTR_CopyCodecsConf "(���������� etc/codecs.conf (�� ���������� MPlayer) � ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "������������ ���������� codecs.conf.\n"
#define MSGTR_CantLoadFont "�� ���� ��������� �����: %s\n"
#define MSGTR_CantLoadSub "�� ���� ��������� ��������: %s\n"
#define MSGTR_ErrorDVDkey "������ ��������� DVD �����.\n"
#define MSGTR_CmdlineDVDkey "��� ����������� ������������ ������������� DVD ����.\n"
#define MSGTR_DVDauthOk "����������� DVD �������� OK.\n"
#define MSGTR_DumpSelectedStreamMissing "dump: FATAL: ��������� ����� �������!\n"
#define MSGTR_CantOpenDumpfile "�� ���� ������� ���� ��� ������������!!!\n"
#define MSGTR_CoreDumped "Core dumped ;)\n"
#define MSGTR_FPSnotspecified "� ��������� �����/��� �� ������� (��� ������������)! ����������� -fps �����!\n"
#define MSGTR_TryForceAudioFmtStr "������� ����������� ��������� ����� ������� %s...\n"
#define MSGTR_CantFindAfmtFallback "�� ���� ����� ����� ����� ��� �������������� ��������� ���������, ������� �� ������ ��������.\n"
#define MSGTR_CantFindAudioCodec "�� ���� ����� ����� ��� ����� ������� 0x%X!\n"
#define MSGTR_RTFMCodecs "�������� DOCS/HTML/ru/codecs.html!\n"
#define MSGTR_CouldntInitAudioCodec "�� ���� ���������������� ����� �����! -> ��� �����\n"
#define MSGTR_TryForceVideoFmtStr "������� ����������� ��������� ����� ������� %s...\n"
#define MSGTR_CantFindVideoCodec "�� ���� ����� ����� ��� ����� ������� 0x%X!\n"
#define MSGTR_VOincompCodec "Sorry, ��������� video_out ���������� �� ���������� � ���� �������.\n"
#define MSGTR_CannotInitVO "FATAL: �� ���� ���������������� ����� �������!\n"
#define MSGTR_CannotInitAO "�� ���� �������/���������������� ����� ���������� -> ��� �����\n"
#define MSGTR_StartPlaying "������ ��c������������...\n"
#define MSGTR_SystemTooSlow "\n\n"\
"         *****************************************************************\n"\
"         **** ���� ������� ������� �������� ����� �������������� ���! ****\n"\
"         *****************************************************************\n"\
"��������� �������, ��������, ������: \n"\
"- �������� ������: ������/����� _�����_ �������\n"\
"  - ����������� -ao sdl ��� ����������� ALSA 0.5 ��� �������� oss �� ALSA 0.9.\n"\
"  - ������������������� � ���������� ���������� -autosync, ������� � 30.\n"\
"- ��������� ����� �����\n"\
"  - ����������� ������ -vo driver (������: -vo help) ��� ����������� � -framedrop!\n"\
"- ��������� ���\n"\
"  - �� ��������� �������������� ������� DVD/DivX �� ��������� �����������! ����������� -hardframedrop\n"\
"- ����� ����.\n"\
"  - ����������� ��������� ����������: -nobps  -ni  -mc 0  -forceidx\n"\
"- ��������� �������� (�������������� NFS/SMB, DVD, VCD � �. �.)\n"\
"  - ����������� -cache 8192.\n"\
"- ����������� �� �� -cache ��� ������������ ��-'��ϣ���'[non-interleaved] AVI ������?\n"\
"  - ����������� -nocache.\n"\
"������� DOCS/HTML/ru/devices.html ��� ������� �� ����������/���������.\n"\
"���� ������ �� �������, ����� ������� DOCS/HTML/ru/bugreports.html!\n\n"

#define MSGTR_NoGui "MPlayer ��� ������������� ��� ��������� GUI!\n"
#define MSGTR_GuiNeedsX "MPlayer GUI ������� X11!\n"
#define MSGTR_Playing "������������ %s.\n"
#define MSGTR_NoSound "�����: ��� �����!!!\n"
#define MSGTR_FPSforced "�����/��� ����������� � %5.3f (ftime: %5.3f).\n"
#define MSGTR_CompiledWithRuntimeDetection "�������������� � ������������ ���� ���������� �� ����� ���������� - �������������� - ��� �� ����������!\n��� ��������� ������������ ������������������, ���������������� MPlayer c --disable-runtime-cpudetection.\n"
#define MSGTR_CompiledWithCPUExtensions "�������������� ��� x86 CPU �� ���������� ������������:"
#define MSGTR_AvailableVideoOutputPlugins "��������� ������� ������ �����:\n"
#define MSGTR_AvailableVideoOutputDrivers "��������� �������� ������ �����:\n"
#define MSGTR_AvailableAudioOutputDrivers "��������� �������� ������ �����:\n"
#define MSGTR_AvailableAudioCodecs "��������� ����� ������:\n"
#define MSGTR_AvailableVideoCodecs "��������� ����� ������:\n"
#define MSGTR_AvailableAudioFm "\n��������� (����������������) ���������/�������� ����� �������:\n"
#define MSGTR_AvailableVideoFm "\n��������� (����������������) ���������/�������� ����� �������:\n"
#define MSGTR_AvailableFsType "��������� ������ ��������� �������������� ����:\n"
#define MSGTR_UsingRTCTiming "������������ ���������� Linux RTC ������������� (%ldHz).\n"
#define MSGTR_CannotReadVideoProperties "�����: �� ���� ��������� ��������.\n"
#define MSGTR_NoStreamFound "����� �� ������.\n"
#define MSGTR_ErrorInitializingVODevice "������ ��� ��������/������������� ���������� ���������� ����� ������ (-vo).\n"
#define MSGTR_ForcedVideoCodec "���������� ����� �����: %s\n"
#define MSGTR_ForcedAudioCodec "���������� ����� �����: %s\n"
#define MSGTR_AODescription_AOAuthor "AO: ��������: %s\nAO: �����: %s\n"
#define MSGTR_AOComment "AO: �����������: %s\n"
#define MSGTR_Video_NoVideo "�����: ��� �����\n"
#define MSGTR_NotInitializeVOPorVO "\nFATAL: �� ���� ���������������� ����� ������� (-vf) ��� ����� ����� (-vo).\n"
#define MSGTR_Paused "\n====��������������====\r" // no more than 23 characters (status line for audio files)
#define MSGTR_PlaylistLoadUnable "\n�� ���� ��������� playlist %s.\n"
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
"  � �� ����� ��������, ���� �� �� ������������ ��� ����������,\n"\
"  ������� � ��������� ������.\n"


// mencoder.c:

#define MSGTR_UsingPass3ControllFile "��������� ��������� ���� ��� �������� 3 �������: %s\n"
#define MSGTR_MissingFilename "\n��������� ��� �����.\n\n"
#define MSGTR_CannotOpenFile_Device "�� ���� ������� ����/����������.\n"
#define MSGTR_ErrorDVDAuth "������ ��� DVD �����������.\n"
#define MSGTR_CannotOpenDemuxer "�� ���� ������� ��������[demuxer].\n"
#define MSGTR_NoAudioEncoderSelected "\n���������� ����� (-oac) �� ������. �������� �����-������ (��. -oac help) ��� ����������� -nosound.\n"
#define MSGTR_NoVideoEncoderSelected "\n���������� ����� (-ovc) �� ������. �������� �����-������ (��. -ovc help), ����������� -ovc help!\n"
#define MSGTR_InitializingAudioCodec "������������� ����� ������...\n"
#define MSGTR_CannotOpenOutputFile "�� ���� ������� ���� '%s'��� ������.\n"
#define MSGTR_EncoderOpenFailed "�� ���� ������� ����������.\n"
#define MSGTR_ForcingOutputFourcc "�������� fourcc ���������� � %x [%.4s]\n"
#define MSGTR_WritingAVIHeader "���� ��������� AVI...\n"
#define MSGTR_DuplicateFrames "\n%d �������������(���) ����(�/��)!\n"
#define MSGTR_SkipFrame "\n��������� ����!\n"
#define MSGTR_ErrorWritingFile "%s: ������ ��� ������ �����.\n"
#define MSGTR_WritingAVIIndex "\n���� ������ AVI...\n"
#define MSGTR_FixupAVIHeader "���������� ��������� AVI...\n"
#define MSGTR_RecommendedVideoBitrate "������������� �������� ��� %s CD: %d\n"
#define MSGTR_VideoStreamResult "\n����� �����: %8.3f kbit/s  (%d bps)  ������: %d ����(�/��)  %5.3f ���.  %d ����(�/��)\n"
#define MSGTR_AudioStreamResult "\n����� �����: %8.3f kbit/s  (%d bps)  ������: %d ����(�/��)  %5.3f ���.\n"

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
" br=<0-1024>   ������� �������� � kBit (������ CBR � ABR)\n"\
"\n"\
" q=<0-9>       �������� (0-������, 9-����������) (������ ��� VBR)\n"\
"\n"\
" aq=<0-9>      �������� ��������� (0-������/����� ���������, 9-������/����������)\n"\
"\n"\
" ratio=<1-100> ����������� ������\n"\
"\n"\
" vol=<0-10>    ���������� �������� ��������� �����\n"\
"\n"\
" mode=<0-3>    (��-���������: ��������������)\n"\
"                0: ������\n"\
"                1: ������Σ���� ������[joint-stereo]\n"\
"                2: �������������\n"\
"                3: ����\n"\
"\n"\
" padding=<0-2>\n"\
"                0: ���\n"\
"                1: ���\n"\
"                2: ������������\n"\
"\n"\
" fast          ������������� �� ������� ����������� � ��������� ����������������� VBR\n"\
"               �������, ����������� ������ �������� � ������� ���������.\n"\
"\n"\
" preset=<value> ������������ ���������� ��������� ��������� ��������.\n"\
"                 medium: VBR  �����������,  ������� ��������\n"\
"                 (��������� ��������� - 150-180 kbps)\n"\
"                 standard:  VBR �����������, ������� ��������\n"\
"                 (��������� ��������� - 170-210 kbps)\n"\
"                 extreme: VBR �����������, ����� ������� ��������\n"\
"                 (��������� ��������� - 200-240 kbps)\n"\
"                 insane:  CBR �����������, ������ ����������������� ��������\n"\
"                 (�������� 320 kbps)\n"\
"                 <8-320>: ABR ����������� � �������� � kbit'�� ������� ����������.\n\n"



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
#define MSGTR_AudioStreamRedefined "��������������! ��������� ����� ������ %d ����������̣�!\n"
#define MSGTR_VideoStreamRedefined "��������������! ��������� ����� ������ %d ����������̣�!\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: ������� ����� (%d � %d ������) ����� ������� � ������!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: ������� ����� (%d � %d ������) ����� ������� � ������!\n"
#define MSGTR_MaybeNI "�������� �� ������������ �� '��ϣ���' �����/���� ��� ��������� �����?\n" \
                     "��� AVI ������ ���������� ����������� '����ϣ���' ����� ������ -ni.\n"
#define MSGTR_SwitchToNi "\n��������� ����� '��ϣ���' AVI ���� - ������������ � -ni �����...\n"
#define MSGTR_Detected_XXX_FileFormat "��������� %s ������ �����!\n"
#define MSGTR_DetectedAudiofile "��������� ����� ����.\n"
#define MSGTR_NotSystemStream "�� MPEG System Stream ������... (�������� Transport Stream?)\n"
#define MSGTR_InvalidMPEGES "������������ MPEG-ES �����??? ��������� � �������, ��� ����� ���� ����� :(\n"
#define MSGTR_FormatNotRecognized "========= Sorry, ������ ����� ����� �� ���������/�� �������������� ===========\n"\
				  "===== ���� ��� AVI, ASF ��� MPEG �����, ���������� ��������� � �������! ======\n"
#define MSGTR_MissingVideoStream "����� ����� �� ������!\n"
#define MSGTR_MissingAudioStream "����� ����� �� ������...  ->��� �����\n"
#define MSGTR_MissingVideoStreamBug "����� ����� �������!? ��������� � �������, ��� ����� ���� ����� :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: � �����  ��� ���������� ����� ��� ����� ������\n"

#define MSGTR_NI_Forced "����������"
#define MSGTR_NI_Detected "���������"
#define MSGTR_NI_Message "%s '��������' ������ AVI �����!\n"

#define MSGTR_UsingNINI "������������� '���������' ������������ ������� AVI �����!\n"
#define MSGTR_CouldntDetFNo "�� ���� ���������� ����� ������ (��� ����������� �����������).\n"
#define MSGTR_CantSeekRawAVI "�� ���� ������������� � ����� ������ AVI! (��������� ������, ���������� � ������ -idx!)\n"
#define MSGTR_CantSeekFile "�� ���� ������������ � ���� �����!\n"

#define MSGTR_EncryptedVOB "����������� VOB ����! ��. DOCS/HTML/ru/dvd.html\n"
#define MSGTR_EncryptedVOBauth "����������� �����, �� ����������� �� ���� ���� �����������!!\n"

#define MSGTR_MOVcomprhdr "MOV: ��� ��������� ������ ���������� ���������� zlib!\n"
#define MSGTR_MOVvariableFourCC "MOV: ��������������! ��������� ���������� FOURCC!?\n"
#define MSGTR_MOVtooManyTrk "MOV: ��������������! ������� ����� ������!"
#define MSGTR_FoundAudioStream "==> ��ۣ� ����� �����: %d\n"
#define MSGTR_FoundVideoStream "==> ��ۣ� ����� �����: %d\n"
#define MSGTR_DetectedTV "������ TV! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "�� ���� ������� ogg ��������[demuxer].\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: ��� ����� ����� (id:%d).\n"
#define MSGTR_CannotOpenAudioStream "�� ���� ������� ����� �����: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "�� ���� ������� ����� ���������: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "�� ���� ������� ��������[demuxer] �����: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "�� ���� ������� ��������[demuxer] ���������: %s\n"
#define MSGTR_TVInputNotSeekable "�� TV ����� ������ ������������! (�������� ����������� ����� ��� ����� ������� ;)\n"
#define MSGTR_DemuxerInfoAlreadyPresent "���������� ���������[demuxer] %s ��� ����������!\n"
#define MSGTR_ClipInfo "���������� � �����:\n"


// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "�� ���� ������� �����\n"
#define MSGTR_CantCloseCodec "�� ���� ������� �����\n"

#define MSGTR_MissingDLLcodec "������: �� ���� ������� ����������� DirectShow �����: %s\n"
#define MSGTR_ACMiniterror "�� ���� ���������/���������������� Win32/ACM AUDIO ����� (������� DLL ����?)\n"
#define MSGTR_MissingLAVCcodec "�� ���� ����� ����� '%s' � libavcodec...\n"

#define MSGTR_MpegNoSequHdr "MPEG: FATAL: ����� ����� ��� ������ ������������������ ����������\n"
#define MSGTR_CannotReadMpegSequHdr "FATAL: �� ���� ������� ������������������ ����������!\n"
#define MSGTR_CannotReadMpegSequHdrEx "FATAL: �� ���� ������� ���������� ������������������ ����������!\n"
#define MSGTR_BadMpegSequHdr "MPEG: ������ ������������������ ����������!\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: ������ ���������� ������������������ ����������!\n"

#define MSGTR_ShMemAllocFail "�� ���� ��������������� ����������� ������.\n"
#define MSGTR_CantAllocAudioBuf "�� ���� ��������������� �������� ����� �����.\n"

#define MSGTR_UnknownAudio "�����������/���������� ����� ������ -> ��� �����\n"

#define MSGTR_UsingExternalPP "[PP] ��������� ������� ������ �������������, max q = %d.\n"
#define MSGTR_UsingCodecPP "[PP] ��������� ������������� �� ������, max q = %d.\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "����� ������� '%s' �� �������������� ���������� vo & vd.\n"
#define MSGTR_VideoCodecFamilyNotAvailableStr "����������� ��������� ����� ������� [%s] (vfm=%s) �� ��������.\n�������� ��� �� ����� ����������.\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "����������� ��������� ����� ������� [%s] (afm=%s) �� ��������.\n�������� ��� �� ����� ����������.\n"
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
#define MSGTR_LIRCdisabled "�� �� ������� ������������ ���� ����� ����������\n"
#define MSGTR_LIRCopenfailed "��������� �������� ��������� LIRC!\n"
#define MSGTR_LIRCcfgerr "��������� ������� ������ ����� ������������ LIRC %s!\n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "�� ���� ����� ����� ������ '%s'.\n"
#define MSGTR_CouldNotOpenVideoFilter "�� ���� ������� ����� ������ '%s'.\n"
#define MSGTR_OpeningVideoFilter "�������� ����� ������: "
#define MSGTR_CannotFindColorspace "�� ���� ����� ����� �������� ������������, ���� ������� 'scale' :(\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDec: ����� �� ��������� sh->disp_w � sh->disp_h, ��������� ������.\n"
#define MSGTR_VoConfigRequest "VDec: vo config �������� - %d x %d (�������������� csp: %s)\n"
#define MSGTR_CouldNotFindColorspace "�� ���� ����� ���������� �������� ������������ - ��������� � -vf scale...\n"
#define MSGTR_MovieAspectIsSet "Movie-Aspect - %.2f:1 - �������������� ��� ��������� ����������� ������ ������.\n"
#define MSGTR_MovieAspectUndefined "Movie-Aspect �� ������̣� - ������������������ �� �����������.\n"


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
#define MSGTR_SkinBrowser "����������� ������"
#define MSGTR_Network "������� ������..."
#define MSGTR_Preferences "���������"
#define MSGTR_OSSPreferences "������������ OSS ��������"
#define MSGTR_SDLPreferences "������������ SDL ��������"
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
#define MSGTR_NEMDB "Sorry, �� ������� ������ ��� ���������� ������."
#define MSGTR_NEMFMR "Sorry, �� ������� ������ ��� ����������� ����."
#define MSGTR_IDFGCVD "Sorry, �� ��ۣ� ����������� � GUI ������� ����� ������."
#define MSGTR_NEEDLAVCFAME "Sorry, �� �� ������ ����������� ��-MPEG ����� �� ����� DXR3/H+ ���������� ��� ���������������.\n����������, �������� lavc ��� fame ��� ������������ DXR3/H+."


// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[skin] ������ � ����� ������������ ����� �� ������ %d: %s" 
#define MSGTR_SKIN_WARNING1 "[skin] ��������������: � ����� ������������ ����� �� ������ %d: widget ������ �� �� ����� �� ������� \"section\" (%s)"
#define MSGTR_SKIN_WARNING2 "[skin] ��������������: � ����� ������������ ����� �� ������ %d: widget ������ �� �� ����� �� ������� \"subsection\" (%s)"
#define MSGTR_SKIN_WARNING3 "[skin] ��������������: � ����� ������������ ����� �� ������ %d: ��� ��������� �� �������������� ���� ��������[widget] (%s)"
#define MSGTR_SKIN_BITMAP_16bit  "������� bitmap � 16 ��� � ������ �� �������������� (%s).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "���� �� ������ (%s)\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "������ ������ BMP (%s)\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "������ ������ TGA (%s)\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "������ ������ PNG (%s)\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "RLE ����������� TGA �� �������������� (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "����������� ��� ����� (%s)\n"
#define MSGTR_SKIN_BITMAP_ConvertError "������ �������������� 24-��� � 32-��� (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "����������� ���������: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "�� ������� ������\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "��������� ������� ����� �������.\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "���� ������ �� ������.\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "���� ������� ������ �� ������.\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "�������������� ������������� ������ (%s)\n"
#define MSGTR_SKIN_UnknownParameter "����������� �������� (%s)\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[skinbrowser] �� ������� ������\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "Skin �� ������ (%s).\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "������ ������ ���� ������������ skin (%s)\n"
#define MSGTR_SKIN_LABEL "Skins:"

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
#define MSGTR_MENU_AudioLanguages "���� ����"
#define MSGTR_MENU_SubtitleLanguages "���� ���������"
#define MSGTR_MENU_PlayList "Playlist"
#define MSGTR_MENU_SkinBrowser "����������� skin'��"
#define MSGTR_MENU_Preferences "���������"
#define MSGTR_MENU_Exit "�����..."
#define MSGTR_MENU_Mute "��������� ����"
#define MSGTR_MENU_Original "��������"
#define MSGTR_MENU_AspectRatio "����������� ��������"
#define MSGTR_MENU_AudioTrack "����� �������"
#define MSGTR_MENU_Track "������� %d"
#define MSGTR_MENU_VideoTrack "����� �������"


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
#define MSGTR_PREFERENCES_Audio "�����"
#define MSGTR_PREFERENCES_Video "�����"
#define MSGTR_PREFERENCES_SubtitleOSD "�������� & OSD"
#define MSGTR_PREFERENCES_Codecs "������ & ��������[demuxer]"
#define MSGTR_PREFERENCES_Misc "������"

#define MSGTR_PREFERENCES_None "���"
#define MSGTR_PREFERENCES_AvailableDrivers "��������� ��������:"
#define MSGTR_PREFERENCES_DoNotPlaySound "�� ����������� ����"
#define MSGTR_PREFERENCES_NormalizeSound "������������� ����"
#define MSGTR_PREFERENCES_EnEqualizer "�������� ����������"
#define MSGTR_PREFERENCES_ExtraStereo "�������� �������������� ������"
#define MSGTR_PREFERENCES_Coefficient "�����������:"
#define MSGTR_PREFERENCES_AudioDelay "�������� �����"
#define MSGTR_PREFERENCES_DoubleBuffer "�������� ������� �����������"
#define MSGTR_PREFERENCES_DirectRender "�������� ������ �����������"
#define MSGTR_PREFERENCES_FrameDrop "�������� ������������ ������"
#define MSGTR_PREFERENCES_HFrameDrop "�������� HARD ������������ ������ (������)"
#define MSGTR_PREFERENCES_Flip "���������� ����������� ����� ������"
#define MSGTR_PREFERENCES_Panscan "Panscan: "
#define MSGTR_PREFERENCES_OSDTimer "������ � ����������"
#define MSGTR_PREFERENCES_OSDProgress "������ progressbar'�"
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "������, �������� � ������ �����"
#define MSGTR_PREFERENCES_Subtitle "��������:"
#define MSGTR_PREFERENCES_SUB_Delay "��������: "
#define MSGTR_PREFERENCES_SUB_FPS "FPS:"
#define MSGTR_PREFERENCES_SUB_POS "�������: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "��������� ������������ ���������"
#define MSGTR_PREFERENCES_SUB_Unicode "Unicode'���� ��������"
#define MSGTR_PREFERENCES_SUB_MPSUB "�������������� ������ �������� � MPlayer'������ ������ ���������"
#define MSGTR_PREFERENCES_SUB_SRT "�������������� ������ �������� � ���������� �� ������� SubViewer (SRT) ������"
#define MSGTR_PREFERENCES_SUB_Overlap "�������� ������������ ���������"
#define MSGTR_PREFERENCES_Font "�����:"
#define MSGTR_PREFERENCES_FontFactor "����������� ������:"
#define MSGTR_PREFERENCES_PostProcess "�������� �������������"
#define MSGTR_PREFERENCES_AutoQuality "���� ��������: "
#define MSGTR_PREFERENCES_NI "������������ '����ϣ���' AVI ������"
#define MSGTR_PREFERENCES_IDX "���� ���������, ��������� ��������� �������"
#define MSGTR_PREFERENCES_VideoCodecFamily "��������� ����� �������:"
#define MSGTR_PREFERENCES_AudioCodecFamily "��������� ����� �������:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "������� OSD"
#define MSGTR_PREFERENCES_FRAME_Subtitle "��������"
#define MSGTR_PREFERENCES_FRAME_Font "�����"
#define MSGTR_PREFERENCES_FRAME_PostProcess "�������������"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "����� & ��������[demuxer]"
#define MSGTR_PREFERENCES_FRAME_Cache "���"
#define MSGTR_PREFERENCES_FRAME_Misc "������"
#define MSGTR_PREFERENCES_OSS_Device "����������:"
#define MSGTR_PREFERENCES_OSS_Mixer "������:"
#define MSGTR_PREFERENCES_SDL_Driver "�������:"
#define MSGTR_PREFERENCES_Message "����������, ���������, ��� ��� ����� ������������� ������������, ����� ��������� ��������� �������� � ����!"
#define MSGTR_PREFERENCES_DXR3_VENC "����� ����������:"
#define MSGTR_PREFERENCES_DXR3_LAVC "������������ LAVC (FFmpeg)"
#define MSGTR_PREFERENCES_DXR3_FAME "������������ FAME"
#define MSGTR_PREFERENCES_FontEncoding1 "Unicode"
#define MSGTR_PREFERENCES_FontEncoding2 "������������������ ����� (ISO-8859-1)"
#define MSGTR_PREFERENCES_FontEncoding3 "������������������ ����� � ���� (ISO-8859-15)"
#define MSGTR_PREFERENCES_FontEncoding4 "����������/����������-����������� ����� (ISO-8859-2)"
#define MSGTR_PREFERENCES_FontEncoding5 "���������, Galician, �����������, �������� (ISO-8859-3)"
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
#define MSGTR_PREFERENCES_FontEncoding21 "��������� Windows (CP1251)"
#define MSGTR_PREFERENCES_FontEncoding22 "����������/���������� ����������� Windows (CP1250)"
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
#define MSGTR_PREFERENCES_LoadFullscreen "���������� �� ������ �����"
#define MSGTR_PREFERENCES_SaveWinPos "��������� ������� ����"
#define MSGTR_PREFERENCES_XSCREENSAVER "������������� XScreenSaver"
#define MSGTR_PREFERENCES_PlayBar "�������� playbar"
#define MSGTR_PREFERENCES_AutoSync "AutoSync ���/����"
#define MSGTR_PREFERENCES_AutoSyncValue "Autosync: "
#define MSGTR_PREFERENCES_CDROMDevice "CD-ROM ����������:"
#define MSGTR_PREFERENCES_DVDDevice "DVD ����������:"
#define MSGTR_PREFERENCES_FPS "FPS ������:"
#define MSGTR_PREFERENCES_ShowVideoWindow "���������� ���� �����, ����� ���������"

#define MSGTR_ABOUT_UHU "���������� GUI ������������ UHU Linux\n"
#define MSGTR_ABOUT_CoreTeam "   �������� ������� MPlayer'�:\n"
#define MSGTR_ABOUT_AdditionalCoders "   �������������� ������:\n"
#define MSGTR_ABOUT_MainTesters "   ������� �������:\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "��������� ������..."
#define MSGTR_MSGBOX_LABEL_Error "������..."
#define MSGTR_MSGBOX_LABEL_Warning "��������������..." 

#endif

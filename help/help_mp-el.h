// Translated by: Ioannis Panteleakis <pioann@csd.auth.gr>
// Various corrections and additions by: ironhell3 <ironhell3@hotmail.com>

// Translated files should be uploaded to ftp://mplayerhq.hu/MPlayer/incoming
// and send a notify message to mplayer-dev-eng maillist.

// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char* banner_text=
"\n\n"
"MPlayer " VERSION "(C) 2000-2003 Arpad Gereoffy (����� DOCS!)\n"
"\n";

static char help_text[]=
"Usage:   mplayer [��������] [url|��������/]�����_�������\n"
"\n"
"������� ��������: (��������� ���  ������ ����������� ��� ������������ ����� �� ��������)\n"
" -vo <drv[:dev]> �������� ��� ����� ������ ������ ��� �� ������� (����� '-vo help' ��� �� �����)\n"
" -ao <drv[:dev]> �������� ��� ����� ������ ���� ��� �� ������� (����� '-ao help' ��� �� �����)\n"
#ifdef HAVE_VCD
" -vcd <������� track>  ����������� track VCD (video cd)  ��� ������� ���� ��� ������\n"
#endif
#ifdef HAVE_LIBCSS
" -dvdauth <dev>  ������ �� ������� DVD ��� ����������� (��� ����������������� �������)\n"
#endif
#ifdef USE_DVDREAD
" -dvd <titleno>  ����������� ��� ������/track DVD ��� �� ������� ���� ��� ������\n"
" -alang/-slang   ������� ��� ������� ��� ����/��������� ��� DVD (2 ���������� ��� ������� ��� �����)\n"
#endif
" -ss <timepos>   ��������� �� �������� ���� (������������ � ��:��:��)\n"
" -nosound        �� ����������� ��� ����\n"
" -fs -vm -zoom   �������� ��� ����������� �� ����� ����� (fullscr,vidmode chg,softw.scale)\n"
" -x <x> -y <y>   ��������� ������� �� <x> * <y> ��������� [�� � -vo ������ �� �����������!]\n"
" -sub <������>   ������� ��� ������� ��������� ��� ����� (����� ������ -subfps, -subdelay)\n"
" -playlist <������> ������ �� ������ ��� ������ ������������\n"
" -vid x -aid y   ������� �������� ������ (x) ��� ���� (y) ��� �����������\n"
" -fps x -srate y �������  ���������� ��� ������ (x fps) ���  ���� (y Hz)\n"
" -pp <��������>  ������������ ��� ������� postprocessing (0-4 ��� DivX, 0-63 ��� mpeg)\n"
" -framedrop      ������������ ��� frame-dropping (��� ���� ����������)\n"
"\n"
"������ �������: (��������� ���  ������ ����������� ��� ������������ ����� , ����� ������ ���  ��� ������ input.conf)\n"
" <-  �  ->      ��������� �����/���� ���� 10 ������������\n"
" up � down      ��������� �����/���� ���� 1 �����\n"
" pgup � pgdown  ��������� �����/���� ���� 10 �����\n"
" < � >          ��������� �����/���� ���� ����� ������������\n"
" p � SPACE      ����� ������� (������� ����������� ������� ��� �� ����������)\n"
" q � ESC        ���� ��� ����������� ��� ������ ������������\n"
" + � -          ������� ������������ ���� ���� +/- 0.1 ������������\n"
" o               ������ ��� OSD �������:  ������ / ����� ������� / ����� �������+������\n"
" * � /          ������ � ������ ��� ������� ��� ���� (������� 'm' ��� ������� master/pcm)\n"
" z � x          ������� ������������ ��������� ���� +/- 0.1 ������������\n"
" r � t          ������� ��� ����� ��� ��������� ����/����, ����� ������ -vop expand !\n"
"\n"
" ��������� ��� ������ ����������� ��� ������������ ������������, ��� ������������ �������� ���  ����� �� ������� \n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// mplayer.c:

#define MSGTR_Exiting "\n ������... (%s)\n"
#define MSGTR_Exit_quit "��������"
#define MSGTR_Exit_eof "����� ��� �������"
#define MSGTR_Exit_error "������� ������"
#define MSGTR_IntBySignal "\n �� MPlayer ������������ ��� �� ���� %d ��� module: %s \n"
#define MSGTR_NoHomeDir "�� ������ � ������ ��� HOME �������\n"
#define MSGTR_GetpathProblem "get_path(\"config\") ��������\n"
#define MSGTR_CreatingCfgFile "���������� ��� ������� config: %s\n"
#define MSGTR_InvalidVOdriver "����� ����� ��� ��� ����� ������ ������: %s\n�������������� '-vo help' ��� �� ����� �� ����� ��� ���������� ������ ������ ������.\n"
#define MSGTR_InvalidAOdriver "����� ����� ��� ��� ����� ������ ����: %s\n�������������� '-ao help' ��� �� ����� �� ����� ��� ���������� ������ ������ ����.\n"
#define MSGTR_CopyCodecsConf "(���������/���������� etc/codecs.conf (��� ��� ������ ������ ��� MPlayer) ��� ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "����� ��� ������������� �������������� codecs.conf\n"
#define MSGTR_CantLoadFont "�� ���������� �������� ��� ��������������: %s\n"
#define MSGTR_CantLoadSub "�� ���������� �������� ��� ���������: %s\n"
#define MSGTR_ErrorDVDkey "������ ���� ��� ����������� ��� DVD KEY.\n"
#define MSGTR_CmdlineDVDkey "�� ��������� ������ ��� �� DVD ������������ ��� ����������������.\n"
#define MSGTR_DVDauthOk "� ��������� ������������ ��� DVD �������� �������.\n"
#define MSGTR_DumpSelectedStreamMissing "dump: ������: ������ �� ���������� ������!\n"
#define MSGTR_CantOpenDumpfile "������� �� ������� ��� dump �������!!!\n"
#define MSGTR_CoreDumped "core dumped :)\n"
#define MSGTR_FPSnotspecified "�� �������� FPS (� �����) ���� �����������! �������������� ��� ������� -fps!\n"
#define MSGTR_TryForceAudioFmt "����������  �������� ��� ����������� ��� ������ ��� ���� %d ...\n"
#define MSGTR_CantFindAfmtFallback "��� ����� ������ � ������ ��� ����������� ��� ������  ��� ����, ����� ����� ������.\n"
#define MSGTR_CantFindAudioCodec "��� ����� ������ � ������ ��� format ��� ������ ��� ���� 0x%X !\n"
#define MSGTR_TryUpgradeCodecsConfOrRTFM "*** ��������� �� ������������ �� %s ��� �� etc/codecs.conf\n*** �� ����� ������� ��������, �������� �� DOCS/codecs.html!\n"
#define MSGTR_CouldntInitAudioCodec "������� � ������������ ��� ������ ��� ����! -> �����-���\n"
#define MSGTR_TryForceVideoFmt "����������  �������� ��� ����������� ��� ������ ��� ������ %d ...\n"
#define MSGTR_CantFindVideoCodec "��� ����� ������ � ������ ��� ������  ��� ��� ������������ -vo ��� �� format ��� ������ 0x%X !\n"
#define MSGTR_VOincompCodec "�������, � ���������� ������� video_out ��� ����� ������� �� ���� ��� �����.\n"
#define MSGTR_CannotInitVO "������: ������� � ������������ ��� ������ ��� ������!\n"
#define MSGTR_CannotInitAO "������� �� �������/������������ ��� ������ ��� ���� -> �����-���\n"
#define MSGTR_StartPlaying "�������� ������������...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"         **************************************************************************\n"\
"         **** �� ������� ��� ����� ���� ���� ��� ��� ����������� ��� �������!  ****\n"\
"         **************************************************************************\n\n"\
"������� ������, ����������, ������: \n"\
"- ������ �����: �������� �� ��� ����� ��� ����\n"\
"  - ��������� -ao sdl � �������������� ALSA 0.5 � oss ����������� ��� ������ ALSA 0.9. �������� �� DOCS/sound.html ��� ������������ ������!\n"\
"  - �������� ������ �� ��������������� �� �������� ����� ��� -autosync, � ����  30 ����� ��� ���� ����.\n"\
"- ���� ������ ��� ������\n"\
"  - ��������� ����������� -vo ����� (��� �����: -vo help) � ��������� �� -framedrop\n"\
"- ����� ������������\n"\
"  - ��� ����������� ������ DVD/DivX ������ �� ������ ������������! ��������� �� -hardframedrop\n"\
"- ������������ ������\n"\
"  - ��������� �� ��������� ����������� ��� ���� ��������: -nobps  -ni  -mc 0  -forceidx\n"\
"- ���� ���� ����������� (NFS/SMB mounts, DVD, VCD ���) \n"\
"  - ��������� -cache 8192\n"\
"- ����� ��������������� -cache ��� ��� ����������� ���� non-interleaved �������;\n"\
"  - ��������� �� -nocache\n"\
"�������� �� DOCS/video.html ��� �������/���������� ��� ������.\n"\
"�� ������ ��� ���� ��� �������, ���� �������� �� DOCS/bugreports.html !\n\n"

#define MSGTR_NoGui "�� MPlayer ������������ ����� ���������� ��� GUI!\n"
#define MSGTR_GuiNeedsX "�� GUI ��� MPlayer ���������� X11!\n"
#define MSGTR_Playing "����������� ��� %s\n"
#define MSGTR_NoSound "����: �� ���������!!!\n"
#define MSGTR_FPSforced "�� FPS ����������� �� ����� %5.3f  (ftime: %5.3f)\n"
#define MSGTR_CompiledWithRuntimeDetection "��������� �� �������� ���������� ����������� - �������, ��� ����� ��������! ��� ��������� ���������, ���������� �� mplayer ��� ��� ������ ������ �� --disable-runtime-cpudetection\n"
#define MSGTR_CompiledWithCPUExtensions "��������� ��� x86 ����������� �� ��� ��������� ����������:"
#define MSGTR_AvailableVideoOutputPlugins "��������� plugins ��� ����� ������:\n"
#define MSGTR_AvailableVideoOutputDrivers "���������� ������ ��� ����� ������:\n"
#define MSGTR_AvailableAudioOutputDrivers "���������� ������ ��� ����� ����:\n"
#define MSGTR_AvailableAudioCodecs "��������� codecs ����:\n"
#define MSGTR_AvailableVideoCodecs "��������� codecs ������:\n"
#define MSGTR_AvailableAudioFm "\n���������� (compiled-in) ������/����������� codec ����:\n"
#define MSGTR_AvailableVideoFm "\n���������� (compiled-in) ������/����������� codec ������:\n"
#define MSGTR_UsingRTCTiming "����� ��� hardware RTC ��� linux ��� (%ldHz)\n"
#define MSGTR_CannotReadVideoProperties "������: ������� � �������� ���������\n"
#define MSGTR_NoStreamFound "��� ������� ������\n"
#define MSGTR_InitializingAudioCodec "������������ ��� codec ����...\n"
#define MSGTR_ErrorInitializingVODevice "������ ���� �� �������/������������ ��� ����������� video_out (-vo) �������!\n"
#define MSGTR_ForcedVideoCodec "������������ ������ ��� ������ codec: %s\n"
#define MSGTR_ForcedAudioCodec "������������ ������ ��� codec ����: %s\n"
#define MSGTR_AODescription_AOAuthor "AO: ���������: %s\n AO: ����������: %s\n"
#define MSGTR_AOComment "AO: ������: %s\n"
#define MSGTR_Video_NoVideo "������: ��� ������� ������!!!\n"
#define MSGTR_NotInitializeVOPorVO "\n ������: ������� � ������������ ��� ������� ������ (-vop) � ��� ������ ������ (-vo) !\n"
#define MSGTR_Paused "\n------ ����� -------\r"
#define MSGTR_PlaylistLoadUnable "\n ������� � �������  ��� ������ ������������ %s\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- �� MPlayer ���������� ��� ��� 'Illegal Instruction'.\n"\
"  ������ �� ����� �������� ���� ��� ������ ��� runtime CPU-����������...\n"\
"  ����������� �������� �� DOCS/bugreports.html.\n"
#define MSGTR_Exit_SIGILL \
"- �� MPlayer ���������� ��� ��� 'Illegal Instruction'.\n"\
"  ������� ��������� ���� ������� �� ��������� �� ����������� ����������� ��� ����� ���� ����� �����\n"\
"  � ������������/��������������.\n  ������� ��!\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- �� Mplayer ����������  ��� ���� ����� ��� ����������� � ��� ������.\n"\
"  ���������������� �� MPlayer �� --enable-debug ��� ������ 'gdb' backtrace ���\n"\
"  disassembly. ��� ������������, ����� �� DOCS/bugreports.html#crash\n"
#define MSGTR_Exit_SIGCRASH \
"- �� MPlayer ����������. ���� ��� �� ������ �� ���� ������.\n"\
"  ������ �� ����� ��� �������� ���� ������ ��� MPlayer _�_ ����� ������� ��� _�_ ���� ������\n"\
"  ��� gcc ���. �� �������� ��� ������ �� MPlayer, �������� �������� �� DOCS/bugreports.html\n"\
"  ��� ����������� ��� �������. ��� �������� ��� ��� �� ����������� ������� ����� ��� �� ��������\n"\
"  ��� ����������� ���� ��������� �� ��������.\n"


// mencoder.c:

#define MSGTR_MEncoderCopyright "(C) 2000-2003 Arpad Gereoffy (����� DOCS!)\n"
#define MSGTR_UsingPass3ControllFile "����� ��� ������� ������� pass3: %s\n"
#define MSGTR_MissingFilename "\n ��������� �������� �������!\n\n"
#define MSGTR_CannotOpenFile_Device "������� �� ������� ��� �������/�������\n"
#define MSGTR_ErrorDVDAuth "������ ���� ��� ����������� ��� DVD...\n"
#define MSGTR_CannotOpenDemuxer "������� �� ������� ��� demuxer\n"
#define MSGTR_NoAudioEncoderSelected "\n ��� ���������� ������������� ���� (-oac)! �������� ���� � �������������� -nosound. �������������� -oac help !\n"
#define MSGTR_NoVideoEncoderSelected "\n ��� ���������� ������������� ������ (-ovc)! �������� ����, �������������� -ovc help !\n"
#define MSGTR_InitializingAudioCodec "������������ ��� codec ����...\n"
#define MSGTR_CannotOpenOutputFile "������� �� ������� ��� ������� ������ '%s'\n"
#define MSGTR_EncoderOpenFailed "�������� ���� �� ������� ��� ������������ \n"
#define MSGTR_ForcingOutputFourcc "������������ ������ ������ fourcc �� %x [%.4s]\n"
#define MSGTR_WritingAVIHeader "������� ������������ ��� AVI...\n"
#define MSGTR_DuplicateFrames "\n ������������ %d ����!!!    \n"
#define MSGTR_SkipFrame "\n ��������� ����!!!    \n"
#define MSGTR_ErrorWritingFile "%s: ������ �������� �������.\n"
#define MSGTR_WritingAVIIndex "\n ������� ��� index ��� AVI...\n"
#define MSGTR_FixupAVIHeader "�������� ������������ ��� AVI...\n"
#define MSGTR_RecommendedVideoBitrate "������������ bitrate ��� ������ ��� %s CD: %d\n"
#define MSGTR_VideoStreamResult "\n ������ ������: %8.3f kbit/s  (%d bps)  �������: %d bytes  %5.3f ������������  %d ����\n"
#define MSGTR_AudioStreamResult "\n ������ ����: %8.3f kbit/s  (%d bps)  �������: %d bytes  %5.3f ������������\n"

// cfg-mencoder.h:

#define MSGTR_MEncoderMP3LameHelp "\n\n"\
" vbr=<0-4>     ������� ���������� bitrate\n"\
"                0: cbr\n"\
"                1: mt\n"\
"                2: rh(�������������)\n"\
"                3: abr\n"\
"                4: mtrh\n"\
"\n"\
" abr           ���� bitrate\n"\
"\n"\
" cbr           ������� bitrate\n"\
"               ��������� ��� ������������ �� CBR mode �� subsequent ABR presets modes\n"\
"\n"\
" br=<0-1024>   ������� ��� bitrate �� kBit (CBR ��� ABR ����)\n"\
"\n"\
" q=<0-9>       �������� (0-���������, 9-����������) (���� ��� VBR)\n"\
"\n"\
" aq=<0-9>      ����������� �������� (0-��������/����, 9-���������/�����������)\n"\
"\n"\
" ratio=<1-100> �������� ���������\n"\
"\n"\
" vol=<0-10>    ������� ��� audio gain �������\n"\
"\n"\
" mode=<0-3>    (�������������: auto)\n"\
"                0: stereo\n"\
"                1: joint-stereo\n"\
"                2: dualchannel\n"\
"                3: mono\n"\
"\n"\
" padding=<0-2>\n"\
"                0: ���\n"\
"                1: ���\n"\
"                2: �������\n"\
"\n"\
" fast          �������� �� ����������� ������������ �� subsequent VBR presets modes,\n"\
"               ���������� ���������� �������� ��� ��������� bitrates.\n"\
"\n"\
" preset=<value> ��������� ��� ���������� ������� �������� ���������.\n"\
"                 �����: VBR  ������������, ���� ��������\n"\
"                 (150-180 kbps ����� bitrate)\n"\
"                 �������:  VBR ������������, ����� ��������\n"\
"                 (170-210 kbps ����� bitrate)\n"\
"                 extreme: VBR ������������, ���� ����� ��������\n"\
"                 (200-240 kbps ����� bitrate)\n"\
"                 insane:  CBR  ������������, ��������� preset ��������\n"\
"                 (320 kbps ����� bitrate)\n"\
"                 <8-320>: ABR ������������ ��� ���� bitrate ��� ������ �� kbps.\n\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "� CD-ROM ������� '%s' ��� �������!\n"
#define MSGTR_ErrTrackSelect "������ ���� ������� ��� VCD track!"
#define MSGTR_ReadSTDIN "����������� ��� �� stdin...\n"
#define MSGTR_UnableOpenURL "������� �� ������� ��� URL: %s\n"
#define MSGTR_ConnToServer "���������������� ������� �� ��� server: %s\n"
#define MSGTR_FileNotFound "�� ������: '%s' ��� �������\n"

#define MSGTR_SMBInitError "������� � ������������ ��� ����������� libsmbclient: %d\n"
#define MSGTR_SMBFileNotFound "��� ������� �� ������ ��� �� ������ ������: '%s'\n"
#define MSGTR_SMBNotCompiled "MPlayer ��� ������������ �� ���������� ��������� SMB\n"

#define MSGTR_CantOpenDVD "��� ������� �� ������ ��� ������� DVD: %s\n"
#define MSGTR_DVDwait "�������� ����� ��� ������, �������� ����������...\n"
#define MSGTR_DVDnumTitles "�������� %d ������ ��� DVD.\n"
#define MSGTR_DVDinvalidTitle "������ ������� ��� ��� ����� ��� DVD: %d\n"
#define MSGTR_DVDnumChapters "�������� %d �������� �� ����� ��� ����� ��� DVD.\n"
#define MSGTR_DVDinvalidChapter "����� ������� ��� ��������� ��� DVD: %d\n"
#define MSGTR_DVDnumAngles "�������� %d ������ �� ���� ��� ����� ��� DVD.\n"
#define MSGTR_DVDinvalidAngle "����� ������� ��� ������ ��� DVD: %d\n"
#define MSGTR_DVDnoIFO "��� ����� ������ �� ������� ��� IFO ������ ��� ��� ����� ��� DVD %d.\n"
#define MSGTR_DVDnoVOBs "��� ����� ������ �� ������� ��� VOB (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "�� DVD ������ �� ��������!\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "�������������! � ����������� ��� �������� ���� %d �������� ����!\n"
#define MSGTR_VideoStreamRedefined "�������������! � ����������� ��� �������� ������ %d �������� ����!\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: �������� (%d �� %d bytes) ������ ���� ���� buffer!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: �������� (%d �� %d bytes) ������ ������ ���� buffer!\n"
#define MSGTR_MaybeNI "(���� ������������ ���� non-interleaved ������/������ � ������� �� codec)\n" \
		      "��� .AVI ������, ������������� �� ������ non-interleaved �� ��� ������� -ni\n"
#define MSGTR_SwitchToNi "\n ������������� ����� interleaved .AVI - �������� ��� ������ -ni!\n"
#define MSGTR_Detected_XXX_FileFormat "������������� ������ ����� %s!\n"
#define MSGTR_DetectedAudiofile "������������� ������ ����!\n"
#define MSGTR_NotSystemStream "�� ������������ MPEG System Stream format... (����� ����� Transport Stream?)\n"
#define MSGTR_InvalidMPEGES "�� ������������ ������ MPEG-ES??? ������������ �� ��� ���������, ������ �� ����� ��� bug :(\n"
#define MSGTR_FormatNotRecognized "============= �������, ���� �� ����� ������� ��� �������������/������������� ===============\n"\
				  "=== �� �� ������ ����� ��� AVI, ASF � MPEG ������, �������� ������������� �� ��� ���������! ===\n"
#define MSGTR_MissingVideoStream "��� ������� ������ ������!\n"
#define MSGTR_MissingAudioStream "��� ������� ������ ����...  ->�����-���\n"
#define MSGTR_MissingVideoStreamBug "������ �� ������ ������!? ������������ �� ��� ���������, ������ �� ����� ��� bug :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: �� ������ ��� �������� �� ���������� ������ ���� � ������\n"

#define MSGTR_NI_Forced "�������������"
#define MSGTR_NI_Detected "�������"
#define MSGTR_NI_Message "%s NON-INTERLEAVED AVI format �������!\n"

#define MSGTR_UsingNINI "����� ���� NON-INTERLEAVED ��������� ������� ����� AVI!\n"
#define MSGTR_CouldntDetFNo "��� ������� �� ������������ � ������� ��� frames (��� ������� ���������)  \n"
#define MSGTR_CantSeekRawAVI "�� ������ ��������� �� raw .AVI �������! (�� index ����� ����������, ��������� �� ��� ������� -idx!)  \n"
#define MSGTR_CantSeekFile "������� � ��������� �� ���� �� ������!  \n"

#define MSGTR_EncryptedVOB "�������������� VOB ������ (� ��������� ����� ����� ��� libcss ����������)! �������� to DOCS/cd-dvd.html\n"
#define MSGTR_EncryptedVOBauth "�������������� ������ ���� ��� �������� �����������!!\n"

#define MSGTR_MOVcomprhdr "MOV: ������������ ������������ ��� �������������� (�����)!\n"
#define MSGTR_MOVvariableFourCC "MOV: �������������! ��������� FOURCC �������!?\n"
#define MSGTR_MOVtooManyTrk "MOV: �������������! �������� ����� tracks!"
#define MSGTR_FoundAudioStream "==> ������� ������ ����: %d\n"
#define MSGTR_FoundVideoStream "==> ������� ������ ������: %d\n"
#define MSGTR_DetectedTV "������� TV! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "��� ����� ������ �� ������� ��� ogg demuxer\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: ��������� ��� ������ ���� (id:%d)\n"
#define MSGTR_CannotOpenAudioStream "��� ����� ������ �� ������� ��� �������� ����: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "��� ����� ������ �� ������� ��� �������� ���������: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "�������� ���� �� ������� ��� demuxer ����: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "�������� ���� �� ������� ��� demuxer ���������: %s\n"
#define MSGTR_TVInputNotSeekable "TV input ��� ����� �����������! (������� � ��������� �� ����� ��� ��� ������ ������� ;)\n"
#define MSGTR_DemuxerInfoAlreadyPresent "�� ����������� ��� �� demuxer %s �������� ���\n!"
#define MSGTR_ClipInfo "����������� ��� �����: \n"

#define MSGTR_LeaveTelecineMode "\ndemux_mpg: ������� progressive seq, ��������� �� 3:2 TELECINE mode\n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: ������� 3:2 TELECINE, ������������ ��� inverse telecine fx. �� FPS ������� �� %5.3f!  \n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "������� �� ������� ��� codec\n"
#define MSGTR_CantCloseCodec "������� �� �������� ��� codec\n"

#define MSGTR_MissingDLLcodec "������: ��� ����� ������ �� ������� ��� ����������� DirectShow codec: %s\n"
#define MSGTR_ACMiniterror "��� ����� ������ �� ��������/������������� �� Win32/ACM codec ���� (������ �� DLL ������?)\n"
#define MSGTR_MissingLAVCcodec "��� ����� ������ �� ������ �� '%s' ��� libavcodec...\n"

#define MSGTR_MpegNoSequHdr "MPEG: ������: ������� ����� ������� ���� ��������� ��� ��������� ��� ������������\n"
#define MSGTR_CannotReadMpegSequHdr "������: ��� ����� ������ �� ��������� � ��������� ��� ������������!\n"
#define MSGTR_CannotReadMpegSequHdrEx "������: ��� ����� ������ �� ��������� � ��������� ��� ��������� ��� ������������!\n"
#define MSGTR_BadMpegSequHdr "MPEG: ���� ��������� ��� ������������!\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: ���� ��������� ��� ��������� ��� ������������!\n"

#define MSGTR_ShMemAllocFail "��� ������ �� ������������� �������������� �����\n"
#define MSGTR_CantAllocAudioBuf "��� ������ �� ������������� buffer ��� ����� ����\n"

#define MSGTR_UnknownAudio "�������/���� format ����, ����� ��� �����-���\n"

#define MSGTR_UsingExternalPP "[PP] ����� ���������� ������� ���������������, ������� q = %d\n"
#define MSGTR_UsingCodecPP "[PP] ����� ������� ��������������� ��� �� codec, ������� q = %d\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "� �������� ��� �� ������ '%s' ��� ������������� ��� �� ���������� vo ��� vd! \n"
#define MSGTR_VideoCodecFamilyNotAvailable "� ������ ��� ��� ����������� ��� codec ������ [%s] (vfm=%d) ��� ���������� (������������� �� ���� ��� ��������� ��� ������������!)\n"
#define MSGTR_AudioCodecFamilyNotAvailable "� ������ ��� ��� ����������� ��� codec ���� [%s] (afm=%d) ��� ���������� (������������� �� ���� ��� ��������� ��� ������������!)\n"
#define MSGTR_OpeningVideoDecoder "������� ��������������� ������: [%s] %s\n"
#define MSGTR_OpeningAudioDecoder "������� ��������������� ����: [%s] %s\n"
#define MSGTR_UninitVideo "uninit ������: %d  \n"
#define MSGTR_UninitAudio "uninit ���: %d  \n"
#define MSGTR_VDecoderInitFailed "�������� ������������� ��� VDecoder :(\n"
#define MSGTR_ADecoderInitFailed "�������� ������������� ��� ADecoder :(\n"
#define MSGTR_ADecoderPreinitFailed "�������� ���������������� ��� ADecoder :(\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: ������� %d bytes ��� ��� buffer �������\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: ������� %d + %d = %d bytes ��� ��� buffer ������\n"

// LIRC:
#define MSGTR_SettingUpLIRC "������������ ����������� ��� lirc...\n"
#define MSGTR_LIRCdisabled "�������������� ��� ����������� ������ �����������\n"
#define MSGTR_LIRCopenfailed "�������� ���� ������������ ��� ����������� ��� lirc!\n"
#define MSGTR_LIRCcfgerr "�������� ���� �� �������� ��� ������� ���������� ��� lirc %s !\n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "������� � ������ ��� ������� ������ '%s'\n"
#define MSGTR_CouldNotOpenVideoFilter "������� �� ������� ��� ������� ������ '%s'\n"
#define MSGTR_OpeningVideoFilter "������� ��� ������� ������: "
#define MSGTR_CannotFindColorspace "������� ������ ��� colorspace, ����� ��� �� ��� �������� 'scale' :(\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDec: �� codec ��� ����� sh->disp_w ��� sh->disp_h, ���������� ��������!\n"
#define MSGTR_VoConfigRequest "VDec: ������ ��� �������� vo - %d x %d (����������� csp: %s)\n"
#define MSGTR_CouldNotFindColorspace "��� ������� ���������� colorspace - ���������� �� -vop scale...\n"
#define MSGTR_MovieAspectIsSet "� �������� ��� ������� ����� %.2f:1 - ������������ ��� ��� �������� ��� ��������� ��� �������.\n"
#define MSGTR_MovieAspectUndefined "� �������� ��� ������� ��� ����� �������� - ��� ����������� ������������.\n"

// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "����"
#define MSGTR_FileSelect "������� ������� ..."
#define MSGTR_SubtitleSelect "������� ��������� ..."
#define MSGTR_OtherSelect "������� ..."
#define MSGTR_AudioFileSelect "������� ���������� ������� ���� ..."
#define MSGTR_FontSelect "������� �������������� ..."
#define MSGTR_PlayList "����� ������������"
#define MSGTR_Equalizer "Equalizer"
#define MSGTR_SkinBrowser "�����  skins"
#define MSGTR_Network "Streaming �������."
#define MSGTR_Preferences "���������"
#define MSGTR_OSSPreferences "����������� ��� ��� ����� OSS"
#define MSGTR_SDLPreferences "����������� ��� ��� ����� SDL"
#define MSGTR_NoMediaOpened "��� ���������� ������"
#define MSGTR_VCDTrack "VCD track %d"
#define MSGTR_NoChapter "�� ����� ���������"
#define MSGTR_Chapter "�������� %d"
#define MSGTR_NoFileLoaded "��� ��������� ������"

// --- buttons ---
#define MSGTR_Ok "�������"
#define MSGTR_Cancel "�����"
#define MSGTR_Add "��������"
#define MSGTR_Remove "��������"
#define MSGTR_Clear "���������"
#define MSGTR_Config "�����������"
#define MSGTR_ConfigDriver "��������� ������"
#define MSGTR_Browse "��������� �������"

// --- error messages ---
#define MSGTR_NEMDB "�������, ��� ������� ������ ����� ��� �������  ���� buffer."
#define MSGTR_NEMFMR "�������, ��� ������� ������ ����� ��� ��� �������� ��� �����."
#define MSGTR_IDFGCVD "�������, ��� ������� ������ ������ ������ ��� �� ����� �������� �� �� GUI."
#define MSGTR_NEEDLAVCFAME "�������, ��� �������� �� ����������� ������ ��� ��� ����� mpeg �� �� ������� DXR3/H+ ����� �����������������.\n �������� ������������� lavc � fame ��� DXR3/H+ �����-��������."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[skin] ������ ��� ������ ����������� ��� skin ��� ������ %d: %s"
#define MSGTR_SKIN_WARNING1 "[skin] ������������� ��� ������ ����������� ��� skin ��� ������ %d: �� widget ������� ���� ���� �� \"section\" ��� ������� ( %s )"
#define MSGTR_SKIN_WARNING2 "[skin] ������������� ��� ������ ����������� ��� skin ��� ������ %d: �� widget ������� ���� ���� �� \"subsection\" ��� ������� (%s)"
#define MSGTR_SKIN_WARNING3 "[skin] ������������� ��� ������ ����������� ��� skin ��� ������ %d: ���� �� subsection ��� ������������� ��� ���� �� widget (%s)"
#define MSGTR_SKIN_BITMAP_16bit  "�� ����� �������� ������� ��� 16 bits � �������� ��� ������������� ( %s ).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "�� ������ ( %s ) ��� �������\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "������ ���� ��� �������� ��� bmp ( %s )\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "������ ���� ��� �������� ��� tga ( %s )\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "������ ���� ��� �������� ��� png ( %s )\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "�� RLE packed tga ��� ������������� ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "�� ������������ ����� ������� ( %s )\n"
#define MSGTR_SKIN_BITMAP_ConvertError "������ ���� �� ��������� ��� 24 bit �� 32 bit ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "�� ������������ ������: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "��� ������� ������ ����� ���������\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "��������� ��������� ��������������\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "��� ������� ������ ��������������\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "��� ������� ������ ��� ������� ��������������\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "��-������� � ��������� ��� �������������� ( %s )\n"
#define MSGTR_SKIN_UnknownParameter "�� ������������ ���������� ( %s )\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[����� skin] ��� ������� ������ ����� ���������.\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "��� ������� skin ( %s ).\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "������ ��������� ��� skin configfile ( %s ).\n"
#define MSGTR_SKIN_LABEL "Skins:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "���� ��� MPlayer"
#define MSGTR_MENU_Open "������� ..."
#define MSGTR_MENU_PlayFile "����������� ������� ..."
#define MSGTR_MENU_PlayVCD "����������� VCD ..."
#define MSGTR_MENU_PlayDVD "����������� DVD ..."
#define MSGTR_MENU_PlayURL "����������� URL ..."
#define MSGTR_MENU_LoadSubtitle "������� ��������� ..."
#define MSGTR_MENU_DropSubtitle "����������� ��������� ..."
#define MSGTR_MENU_LoadExternAudioFile "������� ���������� ������� ���� ..."
#define MSGTR_MENU_Playing "�����������..."
#define MSGTR_MENU_Play "�����������"
#define MSGTR_MENU_Pause "�����"
#define MSGTR_MENU_Stop "����"
#define MSGTR_MENU_NextStream "������� ������"
#define MSGTR_MENU_PrevStream "����������� ������"
#define MSGTR_MENU_Size "�������"
#define MSGTR_MENU_NormalSize "�������� �������"
#define MSGTR_MENU_DoubleSize "�������� �������"
#define MSGTR_MENU_FullScreen "������ �����"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "����������� ������ ..."
#define MSGTR_MENU_ShowDVDMenu "�������� ��� ����� ��� DVD"
#define MSGTR_MENU_Titles "������"
#define MSGTR_MENU_Title "������ %2d"
#define MSGTR_MENU_None "(������)"
#define MSGTR_MENU_Chapters "��������"
#define MSGTR_MENU_Chapter "�������� %2d"
#define MSGTR_MENU_AudioLanguages "������� ����"
#define MSGTR_MENU_SubtitleLanguages "������� ���������"
#define MSGTR_MENU_PlayList "����� ������������"
#define MSGTR_MENU_SkinBrowser "����� skins"
#define MSGTR_MENU_Preferences "���������"
#define MSGTR_MENU_Exit "������ ..."
#define MSGTR_MENU_Mute "�������������� ����"
#define MSGTR_MENU_Original "������"
#define MSGTR_MENU_AspectRatio "�������� ���������"
#define MSGTR_MENU_AudioTrack "Track ����"
#define MSGTR_MENU_Track "Track %d"
#define MSGTR_MENU_VideoTrack "Track ������"

// --- equalizer
#define MSGTR_EQU_Audio "����"
#define MSGTR_EQU_Video "������"
#define MSGTR_EQU_Contrast "��������: "
#define MSGTR_EQU_Brightness "�����������: "
#define MSGTR_EQU_Hue "Hue: "
#define MSGTR_EQU_Saturation "Saturation: "
#define MSGTR_EQU_Front_Left "������� ��������"
#define MSGTR_EQU_Front_Right "������� �����"
#define MSGTR_EQU_Back_Left "���� ��������"
#define MSGTR_EQU_Back_Right "���� �����"
#define MSGTR_EQU_Center "������"
#define MSGTR_EQU_Bass "�����"
#define MSGTR_EQU_All "���"
#define MSGTR_EQU_Channel1 "������ 1:"
#define MSGTR_EQU_Channel2 "������ 2:"
#define MSGTR_EQU_Channel3 "������ 3:"
#define MSGTR_EQU_Channel4 "������ 4:"
#define MSGTR_EQU_Channel5 "������ 5:"
#define MSGTR_EQU_Channel6 "������ 6:"

// --- playlist
#define MSGTR_PLAYLIST_Path "��������"
#define MSGTR_PLAYLIST_Selected "���������� ������"
#define MSGTR_PLAYLIST_Files "������"
#define MSGTR_PLAYLIST_DirectoryTree "������ ���������"

// --- preferences
#define MSGTR_PREFERENCES_Audio "����"
#define MSGTR_PREFERENCES_Video "������"
#define MSGTR_PREFERENCES_SubtitleOSD "��������� ��� OSD"
#define MSGTR_PREFERENCES_Codecs "Codecs ��� demuxer"
#define MSGTR_PREFERENCES_Misc "�������"

#define MSGTR_PREFERENCES_None "������"
#define MSGTR_PREFERENCES_AvailableDrivers "���������� ������:"
#define MSGTR_PREFERENCES_DoNotPlaySound "��-����������� ����"
#define MSGTR_PREFERENCES_NormalizeSound "�������������� ����"
#define MSGTR_PREFERENCES_EnEqualizer "������������ ��� equalizer"
#define MSGTR_PREFERENCES_ExtraStereo "������������ ��� extra stereo"
#define MSGTR_PREFERENCES_Coefficient "Coefficient:"
#define MSGTR_PREFERENCES_AudioDelay "����������� ����"
#define MSGTR_PREFERENCES_DoubleBuffer "������������ double buffering"
#define MSGTR_PREFERENCES_DirectRender "������������ direct rendering"
#define MSGTR_PREFERENCES_FrameDrop "������������ ���������� ����"
#define MSGTR_PREFERENCES_HFrameDrop "������������ ������� ���������� ���� (����������)"
#define MSGTR_PREFERENCES_Flip "Flip ��� ������� ����-����"
#define MSGTR_PREFERENCES_Panscan "Panscan: "
#define MSGTR_PREFERENCES_OSDTimer "�������� ������ ��� �������"
#define MSGTR_PREFERENCES_OSDProgress "���� ������ �������"
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "������, ������� ��� ��� ����� ��� ��������� ������"
#define MSGTR_PREFERENCES_Subtitle "���������:"
#define MSGTR_PREFERENCES_SUB_Delay "�����������: "
#define MSGTR_PREFERENCES_SUB_FPS "FPS:"
#define MSGTR_PREFERENCES_SUB_POS "����: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "�������������� ��������� ���������� ���������"
#define MSGTR_PREFERENCES_SUB_Unicode "Unicode ���������"
#define MSGTR_PREFERENCES_SUB_MPSUB "��������� ����������� ��������� �� �������� ����� MPlayer"
#define MSGTR_PREFERENCES_SUB_SRT "��������� ����������� ��������� �� ���� SubViewer( SRT ) �����-����������"
#define MSGTR_PREFERENCES_SUB_Overlap "�������� ��� overlapping ���������"
#define MSGTR_PREFERENCES_Font "�������������:"
#define MSGTR_PREFERENCES_FontFactor "���������� ��� ��������������:"
#define MSGTR_PREFERENCES_PostProcess "������������ ���������������"
#define MSGTR_PREFERENCES_AutoQuality "�������� ��������: "
#define MSGTR_PREFERENCES_NI "����� ��� non-interleaved AVI parser"
#define MSGTR_PREFERENCES_IDX "������������� ��� ������ index, �� ����������"
#define MSGTR_PREFERENCES_VideoCodecFamily "���������� ��� ������ codec:"
#define MSGTR_PREFERENCES_AudioCodecFamily "���������� ��� codec ����:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "������� OSD"
#define MSGTR_PREFERENCES_FRAME_Subtitle "���������"
#define MSGTR_PREFERENCES_FRAME_Font "�������������"
#define MSGTR_PREFERENCES_FRAME_PostProcess "��������������"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "Codec ��� demuxer"
#define MSGTR_PREFERENCES_FRAME_Cache "Cache"
#define MSGTR_PREFERENCES_FRAME_Misc "�������"
#define MSGTR_PREFERENCES_OSS_Device "�������:"
#define MSGTR_PREFERENCES_OSS_Mixer "������:"
#define MSGTR_PREFERENCES_SDL_Driver "������:"
#define MSGTR_PREFERENCES_Message "�������, ������� ����������� ����������� ������������ ������������."
#define MSGTR_PREFERENCES_DXR3_VENC "������������� ������:"
#define MSGTR_PREFERENCES_DXR3_LAVC "����� ��� LAVC (ffmpeg)"
#define MSGTR_PREFERENCES_DXR3_FAME "����� ��� FAME"
#define MSGTR_PREFERENCES_FontEncoding1 "Unicode"
#define MSGTR_PREFERENCES_FontEncoding2 "������� ���������� ������� (ISO-8859-1)"
#define MSGTR_PREFERENCES_FontEncoding3 "������� ���������� ������� �� ���� (ISO-8859-15)"
#define MSGTR_PREFERENCES_FontEncoding4 "Slavic/Central European Languages (ISO-8859-2)"
#define MSGTR_PREFERENCES_FontEncoding5 "Esperanto, Galician, Maltese, �������� (ISO-8859-3)"
#define MSGTR_PREFERENCES_FontEncoding6 "����� ������� ����������� (ISO-8859-4)"
#define MSGTR_PREFERENCES_FontEncoding7 "��������� (ISO-8859-5)"
#define MSGTR_PREFERENCES_FontEncoding8 "������� (ISO-8859-6)"
#define MSGTR_PREFERENCES_FontEncoding9 "��� �������� (ISO-8859-7)"
#define MSGTR_PREFERENCES_FontEncoding10 "�������� (ISO-8859-9)"
#define MSGTR_PREFERENCES_FontEncoding11 "������� (ISO-8859-13)"
#define MSGTR_PREFERENCES_FontEncoding12 "������� (ISO-8859-14)"
#define MSGTR_PREFERENCES_FontEncoding13 "������� (ISO-8859-8)"
#define MSGTR_PREFERENCES_FontEncoding14 "������ (KOI8-R)"
#define MSGTR_PREFERENCES_FontEncoding15 "Ukrainian, Belarusian (KOI8-U/RU)"
#define MSGTR_PREFERENCES_FontEncoding16 "������������ �������� (CP936)"
#define MSGTR_PREFERENCES_FontEncoding17 "����������� �������� (BIG5)"
#define MSGTR_PREFERENCES_FontEncoding18 "����������� (SHIFT-JIS)"
#define MSGTR_PREFERENCES_FontEncoding19 "��������� (CP949)"
#define MSGTR_PREFERENCES_FontEncoding20 "������������ ����������� (CP874)"
#define MSGTR_PREFERENCES_FontEncoding21 "��������� Windows (CP1251)"
#define MSGTR_PREFERENCES_FontNoAutoScale "��� �������� ���������"
#define MSGTR_PREFERENCES_FontPropWidth "�������� �� �� ������ ��� �������"
#define MSGTR_PREFERENCES_FontPropHeight "�������� �� �� ���� ��� �������"
#define MSGTR_PREFERENCES_FontPropDiagonal "�������� �� �� �������� ��� �������"
#define MSGTR_PREFERENCES_FontEncoding "������������:"
#define MSGTR_PREFERENCES_FontBlur "Blur:"
#define MSGTR_PREFERENCES_FontOutLine "Outline:"
#define MSGTR_PREFERENCES_FontTextScale "��������� ��� ��������:"
#define MSGTR_PREFERENCES_FontOSDScale "OSD ���������:"
#define MSGTR_PREFERENCES_Cache "������������/�������������� ��� cache"
#define MSGTR_PREFERENCES_CacheSize "������� ��� cache: "
#define MSGTR_PREFERENCES_LoadFullscreen "�������� �� ������ �����"
#define MSGTR_PREFERENCES_CacheSize "������� ��� cache: "
#define MSGTR_PREFERENCES_XSCREENSAVER "�������������� ��� ���������� ������"
#define MSGTR_PREFERENCES_AutoSync "������������/�������������� ��� ��������� ������������"
#define MSGTR_PREFERENCES_AutoSyncValue "��������� ������������: "
#define MSGTR_PREFERENCES_CDROMDevice "CD-ROM �������:"
#define MSGTR_PREFERENCES_DVDDevice "DVD �������:"
#define MSGTR_PREFERENCES_FPS "FPS �������:"
#define MSGTR_PREFERENCES_ShowVideoWindow "�������� ��� Video Window ���� ��� ����� ��������������"

#define MSGTR_ABOUT_UHU "� �������� ��� GUI ���������� ��� ��� UHU Linux\n"
#define MSGTR_ABOUT_CoreTeam "   ����� ����� ��� MPlayer:\n"
#define MSGTR_ABOUT_AdditionalCoders "   �������� ���������������:\n"
#define MSGTR_ABOUT_MainTesters "   ������� ����������:\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "������� ������ ..."
#define MSGTR_MSGBOX_LABEL_Error "������ ..."
#define MSGTR_MSGBOX_LABEL_Warning "������������� ..."

#endif

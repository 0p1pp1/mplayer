/* Translated by:  Volodymyr M. Lisivka <lvm@mystery.lviv.net>,
		   Andriy Gritsenko <andrej@lucky.net>
   Was synced with help_mp-en.h: rev 1.105
 ========================= MPlayer help =========================== */

#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"������:   mplayer [��æ�] [path/]filename\n"
"\n"
"��æ�:\n"
" -vo <drv[:dev]> ��¦� �������� � �������� צ��� ������ (������ ���. � '-vo help')\n"
" -ao <drv[:dev]> ��¦� �������� � �������� ��Ħ� ������ (������ ���. � '-ao help')\n"
#ifdef HAVE_VCD
" vcd://<����� �����> ����� VCD (video cd) ���� � �������� ��ͦ��� �����\n"
#endif
#ifdef USE_DVDREAD
" dvd://<����� ���Ҧ�> ����� DVD �����/���� � �������� ��ͦ��� �����\n"
" -alang/-slang   ������� ���� DVD ��Ħ�/������Ҧ� (������������� ��� ������)\n"
#endif
" -ss <���>       ����ͦ������� �� ������ (������� ��� ��:��:��) ����æ�\n"
" -nosound        ��� �����\n"
" -stereo <�����> ��¦� MPEG1 ������ ������ (0:������ 1:̦��� 2:������)\n"
" -channels <n>   ����� ��Ȧ���� ����̦� �����\n"
" -fs -vm -zoom   ������������ ����������� (��������.,�ͦ�� צ���,�������������\n"
" -x <x> -y <y>   ����������� �������� �� <x> * <y> [���� -vo ������� Ц�����դ!]\n"
" -sub <file>     ������� ���� ������Ҧ� (���. ����� -subfps, -subdelay)\n"
" -playlist <file> ������� playlist\n"
" -vid x -aid y   ��æ� ��� ������ צ��� (x) � ��Ħ� (y) ������ ��� �����������\n"
" -fps x -srate y ��æ� ��� �ͦ�� צ��� (x ����/���) � ��Ħ� (y Hz) �������Ԧ\n"
" -pp <quality>   ��������� Ʀ���� (0-4 ��� DivX, 0-63 ��� mpegs)\n"
" -nobps          ��������������� �������������� ����� ������Φ��æ� A-V ��� AVI ���̦� (���� ���������!)\n"
" -framedrop      ��������� ������ ���Ҧ� (��� ��צ����� �����)\n"
" -wid <id צ���> ��������������� ������� צ��� ��� צ��� ������ (������� ��� plugger!)\n"
"\n"
"���צۦ:\n"
" <-  ��� ->      ������������� ������/����� �� 10 ������\n"
" ����� ��� ����  ������������� ������/����� ��  1 �������\n"
" pgup ��� pgdown ������������� ������/����� �� 10 ������\n"
" < ��� >         ������������� ������/����� � ������ �����������\n"
" p ��� �����    �������� Ʀ��� (����-��� ���צ�� - ����������)\n"
" q ��� ESC       �������� צ��������� � ��Ȧ�\n"
" + ��� -         ���������� �������� ����� �� +/- 0.1 �����Ħ\n"
" o               ���̦���� ����¦� OSD ����ͦ�:  ���� / ��צ��æ� / ��צ��æ�+������\n"
" * ��� /         ������ ��� �������� ���Φ��� (���������� 'm' ������� master/pcm)\n"
" z ��� x         ���������� �������� ������Ҧ� �� +/- 0.1 �����Ħ\n"
" r or t          �ͦ���� ��������� ������Ҧ� �����/����, ����� ���. -vf expand\n"
"\n"
" * * * ��������� ���. �����������, ��� ��������� ��㶷 � �����! * * *\n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// mplayer.c: 

#define MSGTR_Exiting "\n��������...\n"
#define MSGTR_ExitingHow "\n��������... (%s)\n"
#define MSGTR_Exit_quit "��Ȧ�"
#define MSGTR_Exit_eof "����� �����"
#define MSGTR_Exit_error "�������� �������"
#define MSGTR_IntBySignal "\nMPlayer ���������� �������� %d � ����̦: %s \n"
#define MSGTR_NoHomeDir "�� ���� ������ �����Φ� �������\n"
#define MSGTR_GetpathProblem "�������� � get_path(\"config\")\n"
#define MSGTR_CreatingCfgFile "��������� ����� ���Ʀ����æ�: %s\n"
#define MSGTR_InvalidAOdriver "������������ ��'� �������� ��Ħ� ������: %s\n���. '-ao help' ��� �������� ������ ��������� ������Ҧ�.\n"
#define MSGTR_CopyCodecsConf "(���Ц���� etc/codecs.conf (� ����Ԧ� MPlayer) � ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "������������ ���������� codecs.conf\n"
#define MSGTR_CantLoadFont "�� ���� ����������� �����: %s\n"
#define MSGTR_CantLoadSub "�� ���� ����������� ��������: %s\n"
#define MSGTR_DumpSelectedStreamMissing "dump: FATAL: ������� ��Ԧ� ����������!\n"
#define MSGTR_CantOpenDumpfile "�� ���� צ������ ���� �����!!!\n"
#define MSGTR_CoreDumped "core dumped :)\n"
#define MSGTR_FPSnotspecified "���˦��� ���Ҧ� �� ������� �� ������� (��� ������������ ��������) � ���������! �������������� ���� -fps!\n"
#define MSGTR_TryForceAudioFmt "������ ��������� ����������� Ӧ������� ��Ħ� ����˦� %d...\n"
#define MSGTR_CantFindAudioCodec "�� ���� ������ ����� ��� ��Ħ� ������� 0x%X!\n"
#define MSGTR_TryForceVideoFmt "������ ��������� ����������� Ӧ������� צ��� ����˦� %d...\n"
#define MSGTR_CantFindVideoCodec "�� ���� ������ ����� ��� צ��� ������� 0x%X!\n"
#define MSGTR_VOincompCodec "�������, ������� video_out �����Ҧ� �� ��ͦ���� � ��� �������.\n"
#define MSGTR_CannotInitVO "FATAL: �� ���� �Φæ�̦������ צ��� �������!\n"
#define MSGTR_CannotInitAO "�� ���� צ������/�Φæ�̦������ ��Ħ� �����Ҧ� -> ���� ��� �����\n"
#define MSGTR_StartPlaying "������� �����������...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"         ********************************************************\n"\
"         **** ���� ������� ����� �������� ��� צ�������� ��! ****\n"\
"         ********************************************************\n"\
"!!! �����צ �������, ��������, ��Ȧ�Φ �����: \n"\
"- ���¦��� ������Φ: �������/����� _��Ħ�_ ������� :\n"\
"  - ��������� -ao sdl ��� �������������� ALSA 0.5 ��� �����æ� oss �� ALSA 0.9.\n"\
"  - Experiment with different values for -autosync, 30 is a good start.\n"\
"- ��צ����� צ��� ��צ�.\n"\
"  - ��������� ����� -vo ������� (������: -vo help) ��� ��������� � -framedrop!\n"\
"- ��צ����� ��. �� ����������� צ���������� ����˦ dvd/divx �� ��צ�����\n"\
"  ����������! ��������� -hardframedrop\n"\
"- ����� ����. ��������� Ҧ�Φ ���¦��æ�: -nobps  -ni  -mc 0  -forceidx\n"\
"- ��צ����� ��Ӧ� (����� NFS/SMB, DVD, VCD �� ��.). ��������� -cache 8192.\n"\
"- �� ����������դ�� -cache ��� ����������� �������������� AVI �����?\n"\
"  - ��������� -nocache.\n"\
"������� ������ � ������ DOCS/HTML/en/video.html .\n"\
"���� Φ���� �� ���������, ��Ħ ������� DOCS/HTML/en/bugreports.html!\n\n"

#define MSGTR_NoGui "MPlayer ��� ����Ц�������� ��� Ц������� GUI!\n"
#define MSGTR_GuiNeedsX "MPlayer GUI ������� X11!\n"
#define MSGTR_Playing "����������� %s\n"
#define MSGTR_NoSound "��Ħ�: ��� �����!!!\n"
#define MSGTR_FPSforced "��������� �ͦ���� ˦��˦��� ���Ҧ� �� ������� �� %5.3f (ftime: %5.3f)\n"
#define MSGTR_CompiledWithRuntimeDetection "����Ц������ � ��������������� CPU - ����� - �� �� ����������!\n��� ��������� ������ ��������Ԧ� �������Ц����� MPlayer � --disable-runtime-cpudetection\n"
#define MSGTR_CompiledWithCPUExtensions "����Ц������� ��� x86 CPU � ������������:"
#define MSGTR_AvailableVideoOutputDrivers "������Φ ����̦ צ��� ������:\n"
#define MSGTR_AvailableAudioOutputDrivers "������Φ ����̦ ��Ħ� ������:\n"
#define MSGTR_AvailableAudioCodecs "������Φ ��Ħ� ������:\n"
#define MSGTR_AvailableVideoCodecs "������Φ צ��� ������:\n"
#define MSGTR_AvailableAudioFm "������Φ (�������Φ) �����/�������� ��Ħ� ����˦�:\n"
#define MSGTR_AvailableVideoFm "������Φ (�������Φ) �����/�������� צ��� ����˦�:\n"
#define MSGTR_AvailableFsType "������Φ ��Ҧ���� �������������� צ���������:\n"
#define MSGTR_UsingRTCTiming "������������ ��������� ������ RTC (%ld��).\n"
#define MSGTR_CannotReadVideoProperties "�����: ��������� �������� ���������Ԧ.\n"
#define MSGTR_NoStreamFound "��Ԧ� �� ��������.\n"
#define MSGTR_ErrorInitializingVODevice "������� צ�������/�Φæ�̦��æ� ��������� video_out (-vo) ��������.\n"
#define MSGTR_ForcedVideoCodec "���������� צ��� �����: %s\n"
#define MSGTR_ForcedAudioCodec "���������� ��Ħ� �����: %s\n"
#define MSGTR_Video_NoVideo "�����: ��� צ���\n"
#define MSGTR_NotInitializeVOPorVO "\nFATAL: ��������� �Φæ�̦������ צ��� Ʀ����� (-vf) ��� צ��� ��צ� (-vo).\n"
#define MSGTR_Paused "\n  =====  �����  =====\r"
#define MSGTR_PlaylistLoadUnable "\n��������� ����������� playlist %s.\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- MPlayer crashed by an 'Illegal Instruction'.\n"\
"  It may be a bug in our new runtime CPU-detection code...\n"\
"  Please read DOCS/HTML/en/bugreports.html.\n"
#define MSGTR_Exit_SIGILL \
"- MPlayer crashed by an 'Illegal Instruction'.\n"\
"  It usually happens when you run it on a CPU different than the one it was\n"\
"  compiled/optimized for.\n  Verify this!\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- MPlayer crashed by bad usage of CPU/FPU/RAM.\n"\
"  Recompile MPlayer with --enable-debug and make a 'gdb' backtrace and\n"\
"  disassembly. For details, see DOCS/HTML/en/bugreports_what.html#bugreports_crash\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer crashed. This shouldn't happen.\n"\
"  It can be a bug in the MPlayer code _or_ in your drivers _or_ in your\n"\
"  gcc version. If you think it's MPlayer's fault, please read\n"\
"  DOCS/HTML/en/bugreports.html and follow the instructions there. We can't and\n"\
"  won't help unless you provide this information when reporting a possible bug.\n"

// mencoder.c:
#define MSGTR_UsingPass3ControllFile "������������ pass3 ����: %s\n"
#define MSGTR_MissingFilename "\n������������ ����.\n\n"
#define MSGTR_CannotOpenFile_Device "��������� צ������ ����/�����Ҧ�.\n"
#define MSGTR_CannotOpenDemuxer "��������� צ������ demuxer.\n"
#define MSGTR_NoAudioEncoderSelected "\n�� �������� ��Ħ� ����� (-oac). ����Ҧ�� ��� �������������� -nosound. ��������� -oac help!\n"
#define MSGTR_NoVideoEncoderSelected "\n�� �������� צ��� ����� (-ovc). ����Ҧ��, ��������� -ovc help!\n"
#define MSGTR_CannotOpenOutputFile "��������� �������� ���� '%s'.\n"
#define MSGTR_EncoderOpenFailed "��������� צ������ �����.\n"
#define MSGTR_ForcingOutputFourcc "���������� ��Ȧ���� fourcc � %x [%.4s]\n"
#define MSGTR_WritingAVIHeader "������� ���������...\n"
#define MSGTR_DuplicateFrames "\n%d ��������� ���Ҧ�!\n"
#define MSGTR_SkipFrame "\n���� ���������!\n"
#define MSGTR_ErrorWritingFile "%s: ������� ������ �����.\n"
#define MSGTR_WritingAVIIndex "\n������� index...\n"
#define MSGTR_FixupAVIHeader "������� ���������...\n"
#define MSGTR_RecommendedVideoBitrate "�������������� ¦����� ��� %s CD: %d\n"
#define MSGTR_VideoStreamResult "\n����� ��Ԧ�: %8.3f kbit/s  (%d �/�)  ���ͦ�: %d ����  %5.3f ������  %d ���Ҧ�\n"
#define MSGTR_AudioStreamResult "\n��Ħ� ��Ԧ�: %8.3f kbit/s  (%d �/�)  ���ͦ�: %d ����  %5.3f ������\n"

// cfg-mencoder.h:
#define MSGTR_MEncoderMP3LameHelp "\n\n"\
" vbr=<0-4>     ����� �ͦ����� ¦������\n"\
"                0: cbr\n"\
"                1: mt\n"\
"                2: rh(default)\n"\
"                3: abr\n"\
"                4: mtrh\n"\
"\n"\
" abr           ���������� ¦�����\n"\
"\n"\
" cbr           ���Ԧ���� ¦�����\n"\
"               Forces also CBR mode encoding on subsequent ABR presets modes\n"\
"\n"\
" br=<0-1024>   ������� ¦����� � kBit (Ԧ���� ��� CBR �� ABR)\n"\
"\n"\
" q=<0-9>       �˦��� (0-�������, 9-��������) (Ԧ���� ��� VBR)\n"\
"\n"\
" aq=<0-9>      ����Ҧ������ �˦��� (0-�����/��צ��Φ�� 9-Ǧ���/����˦��)\n"\
"\n"\
" ratio=<1-100> ������� ���������\n"\
"\n"\
" vol=<0-10>    set audio input gain\n"\
"\n"\
" mode=<0-3>    (���� �� �������: auto)\n"\
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
" fast          ���������� �� ������ ��������� ��� ���̦������ VBR presets modes,\n"\
"               ����� ����� �˦��� �� ¦��ۦ ¦������.\n"\
"\n"\
" preset=<value> ����������� ���¦��ۦ ��������� ����Ԧ.\n"\
"                 �������:    VBR ���������, ����� �˦���\n"\
"                 (150-180 kbps ¦�����)\n"\
"                 ��������:   VBR ���������, ������ �˦���\n"\
"                 (170-210 kbps ¦�����)\n"\
"                 ������:     VBR ���������, ���� ������ �˦���\n"\
"                 (200-240 kbps ¦�����)\n"\
"                 ����צ����: CBR ���������, ������� ��������� ����Ԧ\n"\
"                 (320 kbps ¦�����)\n"\
"                 <8-320>:    ABR ��������� � �������� ���������� ¦�������.\n\n"
   
// open.c, stream.c:
#define MSGTR_CdDevNotfound "��������צ� \"%s\" �� ���������!\n"
#define MSGTR_ErrTrackSelect "������� ������ ����� �� VCD!"
#define MSGTR_ReadSTDIN "������� � stdin...\n"
#define MSGTR_UnableOpenURL "�� ���� צ������ URL: %s\n"
#define MSGTR_ConnToServer "�'������� � ��������: %s\n"
#define MSGTR_FileNotFound "���� �� ���������: '%s'\n"

#define MSGTR_SMBFileNotFound "������� צ������� � ����֦: '%s'\n"
#define MSGTR_SMBNotCompiled "MPlayer �� ��� ����Ц������ϧ Ц������� SMB\n"

#define MSGTR_CantOpenDVD "�� �ͦ� צ������ DVD: %s\n"
#define MSGTR_DVDwait "������� ��������� �����, ��������� ���� �����...\n"
#define MSGTR_DVDnumTitles "� %d ��Ҧ��� � ������� �� ����� DVD.\n"
#define MSGTR_DVDinvalidTitle "������������� ����� ��Ҧ��� ���Ҧ� �� DVD: %d\n"
#define MSGTR_DVDnumChapters "� %d ���Ħ̦� �� æ� ��Ҧ�æ � DVD �������.\n"
#define MSGTR_DVDinvalidChapter "������������� ����� DVD ���Ħ��: %d\n"
#define MSGTR_DVDnumAngles "� %d ��Ԧ� �� æ� ��Ҧ�æ � DVD �������.\n"
#define MSGTR_DVDinvalidAngle "������������� ����� DVD ����: %d\n"
#define MSGTR_DVDnoIFO "�� ���� צ������ IFO ���� ��� DVD ���Ҧ� %d.\n"
#define MSGTR_DVDnoVOBs "�� ���� צ������ ����� VOBS (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD ��Ц��� צ�������!\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "������������! ��������� ��Ħ� ������ %d ��������������!\n"
#define MSGTR_VideoStreamRedefined "������������! ��������� צ��� ������ %d ��������������!\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: ����� ������ (%d, %d ���Ԧ�) ��Ħ� ����Ԧ� � ����Ҧ!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: ����� ������ (%d, %d ���Ԧ�) צ��� ����Ԧ� � ����Ҧ!\n"
#define MSGTR_MaybeNI "(������� �� ��������� ������������� ��Ԧ�/���� ��� �������� �����)\n"
#define MSGTR_SwitchToNi "\n����������� ������ ����������� AVI ���� - ��������� � -ni �����...\n"
#define MSGTR_Detected_XXX_FileFormat "��������� %s ������ �����!\n"
#define MSGTR_DetectedAudiofile "��Ħ� ���� �����������.\n"
#define MSGTR_NotSystemStream "�� � �����Ԧ MPEG System Stream... (�������, Transport Stream?)\n"
#define MSGTR_FormatNotRecognized "========= �������, ������ ����� ����� �� ���Ц������ �� �� Ц�����դ���� ===========\n"\
				  "===== ���� �� AVI, ASF ��� MPEG ��Ԧ�, ���� ����� ��'�֦���� � �������! ======\n"
#define MSGTR_MissingVideoStream "����� ��Ԧ� �� ���������!\n"
#define MSGTR_MissingAudioStream "��Ħ� ��Ԧ� �� ���������...  -> ������� ��� �����\n"
#define MSGTR_MissingVideoStreamBug "����� ��Ԧ� ����������!? ��'�֦���� � �������, �� ������ ������� :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: ���� �� ͦ����� ������� ��Ħ� ��� צ��� ��Ԧ�\n"

#define MSGTR_NI_Forced "��������� ��������"
#define MSGTR_NI_Detected "���������"
#define MSGTR_NI_Message "%s ������������� ������ AVI �����!\n"

#define MSGTR_UsingNINI "������������ �������������� ��� ������������ ������� AVI �����!\n"
#define MSGTR_CouldntDetFNo "�� �ͦ� ��������� ����� ���Ҧ� (��� ����������� �����������)\n"
#define MSGTR_CantSeekRawAVI "�� ���� ����ͦ������� � ����Ϧ������������ ����æ .AVI! (����������� ������, ��������� � ������ -idx!)\n"
#define MSGTR_CantSeekFile "�� ���� ����ͦ�������� � ����� ���̦!\n"

#define MSGTR_MOVcomprhdr "MOV: ������Ԧ ��������� (���� ��) �� Ц�����������!\n"
#define MSGTR_MOVvariableFourCC "MOV: ������������! �������� ����ͦ���� FOURCC!?\n"
#define MSGTR_MOVtooManyTrk "MOV: ������������! ����� ������ ���˦�!"
#define MSGTR_FoundAudioStream "==> �������� ��Ħ� ��Ԧ�: %d\n"
#define MSGTR_FoundVideoStream "==> �������� צ��� ��Ԧ�: %d\n"
#define MSGTR_DetectedTV "����������� ��! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "��������� צ������ ogg demuxer.\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: ����� ��Ħ� ������ (id:%d).\n"
#define MSGTR_CannotOpenAudioStream "��������� צ������ ��Ħ� ��Ԧ�: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "��������� צ������ ��Ԧ� ������Ҧ�: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "�� ������� צ������ ��Ħ� demuxer: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "�� ������� צ������ demuxer ������Ҧ�: %s\n"
#define MSGTR_TVInputNotSeekable "TV input is not seekable! (Seeking will probably be for changing channels ;)\n"
#define MSGTR_DemuxerInfoAlreadyPresent "�������æ� ��������� %s ��� ��������!\n"
#define MSGTR_ClipInfo "�������æ� �̦��:\n"


// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "�� �ͦ� צ������ �����\n"
#define MSGTR_CantCloseCodec "�� �ͦ� ������� �����\n"

#define MSGTR_MissingDLLcodec "�������: �� �ͦ� צ������ ����Ȧ���� DirectShow �����: %s\n"
#define MSGTR_ACMiniterror "�� �ͦ� ����������� �� �Φæ�̦������ Win32/ACM AUDIO ����� (���������� DLL ����?)\n"
#define MSGTR_MissingLAVCcodec "�� ���� ������ ����� \"%s\" � libavcodec...\n"

#define MSGTR_MpegNoSequHdr "MPEG: FATAL: ����� ����� ��� ������ ���̦������Ԧ �������˦�\n"
#define MSGTR_CannotReadMpegSequHdr "FATAL: �� ���� ������ ���̦���Φ��� �������˦�!\n"
#define MSGTR_CannotReadMpegSequHdrEx "FATAL: �� ���� ������ ���������� ���̦������Ԧ �������˦�!\n"
#define MSGTR_BadMpegSequHdr "MPEG: ������ ���̦���Φ��� �������˦�!\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: ������ ���������� ���̦������Ԧ �������˦�!\n"

#define MSGTR_ShMemAllocFail "�� ���� �������� �������� ���'���\n"
#define MSGTR_CantAllocAudioBuf "�� ���� �������� ��Ȧ���� ����� ��Ħ�\n"

#define MSGTR_UnknownAudio "��צ����� �� ���������� ��Ħ� ������, ������� ��� �����\n"

#define MSGTR_UsingExternalPP "[PP] ������������ ���Φ�Φ� Ʀ���� �������, ���� q = %d.\n"
#define MSGTR_UsingCodecPP "[PP] ������������ ������� ������, ���� q = %d.\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "����� ������� '%s' �� Ц�����դ���� ��������� vo & vd.\n"
#define MSGTR_VideoCodecFamilyNotAvailableStr "���������� ������� צ��� ������ [%s] (vfm=%s) ���������� (�צ��Φ�� ���� Ц� ��� ���Ц��æ�)\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "���������� ������� ��Ħ� ������ [%s] (afm=%s) ���������� (�צ��Φ�� ���� Ц� ��� ���Ц��æ�)\n"
#define MSGTR_OpeningVideoDecoder "��������� צ��� �������: [%s] %s\n"
#define MSGTR_OpeningAudioDecoder "��������� ��Ħ� �������: [%s] %s\n"
#define MSGTR_UninitVideoStr "צ��������� צ���: %s\n"
#define MSGTR_UninitAudioStr "צ��������� ��Ħ�: %s\n"
#define MSGTR_VDecoderInitFailed "�¦� �Φæ�̦��æ� VDecoder :(\n"
#define MSGTR_ADecoderInitFailed "�¦� �Φæ�̦��æ� ADecoder :(\n"
#define MSGTR_ADecoderPreinitFailed "�¦� Ц���������� ADecoder :(\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: �����Ħ��� %d ���� �Ȧ����� ������\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: �����Ħ��� %d + %d = %d ���� ��Ȧ����� ������\n"

// LIRC:
#define MSGTR_SettingUpLIRC "������������ Ц������� lirc...\n"
#define MSGTR_LIRCdisabled "�� �� ������� ��������������� ���� צ������� ���������\n"
#define MSGTR_LIRCopenfailed "������� צ������� Ц������� lirc!\n"
#define MSGTR_LIRCcfgerr "������� ������� ����� ���Ʀ����æ� LIRC %s!\n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "��������� ������ צ��� Ʀ���� '%s'\n"
#define MSGTR_CouldNotOpenVideoFilter "��������� צ������ צ��� Ʀ���� '%s'\n"
#define MSGTR_OpeningVideoFilter "��������� צ��� Ʀ����: "
//-----------------------------
#define MSGTR_CannotFindColorspace "�� ���� ЦĦ����� �������� ����� �����Ҧ�, ��צ�� ������� 'scale' :(\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDec: ����� �� ��������� sh->disp_w �� sh->disp_h, ������� �¦��� ��.\n"
#define MSGTR_VoConfigRequest "VDec: vo config ����� - %d x %d (preferred csp: %s)\n"
#define MSGTR_CouldNotFindColorspace "�� ���� ЦĦ����� Ц������� ����� �����Ҧ� - ������ � -vf scale...\n"
#define MSGTR_MovieAspectIsSet "���������� ���Ҧ� %.2f:1 - ��������� ��� ������������.\n"
#define MSGTR_MovieAspectUndefined "���������� ���Ҧ� �� ������� - ������������� �� ����������դ����.\n"

// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "��� ��������"
#define MSGTR_FileSelect "������� ����..."
#define MSGTR_SubtitleSelect "������� ��������..."
#define MSGTR_OtherSelect "��¦�..."
#define MSGTR_AudioFileSelect "������� ���Φ�Φ� ��Ħ� �����..."
#define MSGTR_FontSelect "������� �����..."
#define MSGTR_PlayList "������ �����������"
#define MSGTR_Equalizer "����������"
#define MSGTR_SkinBrowser "���������� ����Φ�"
#define MSGTR_Network "����������� � ����֦..."
#define MSGTR_Preferences "������������"
#define MSGTR_NoMediaOpened "����� צ�������� ��Ӧ�."
#define MSGTR_VCDTrack "��Ҧ��� VCD %d"
#define MSGTR_NoChapter "No chapter"
#define MSGTR_Chapter "Chapter %d"
#define MSGTR_NoFileLoaded "����� ������������� �����."

// --- buttons ---
#define MSGTR_Ok "���"
#define MSGTR_Cancel "���������"
#define MSGTR_Add "������"
#define MSGTR_Remove "��������"
#define MSGTR_Clear "���������"
#define MSGTR_Config "���Ʀ��������"
#define MSGTR_ConfigDriver "���Ʀ�������� �������"
#define MSGTR_Browse "����������"

// --- error messages ---
#define MSGTR_NEMDB "�������, �� �������� ���'�Ԧ ��� צ������������� ������."
#define MSGTR_NEMFMR "�������, �� �������� ���'�Ԧ ��� צ���������� ����."
#define MSGTR_IDFGCVD "�������, �� �������� צ���צ����� �� GUI ��Ȧ����� צ��� ��������."
#define MSGTR_NEEDLAVCFAME "�������, �� �� ������ ����� ��-MPEG ����� �� ������ DXR3/H+ ������ϧ ��� �������������.\n���� �����, �צ��Φ�� lavc ��� fame � ����̦ ���Ʀ��������� DXR3/H+."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[�����] ������� � ���̦ ���Ʀ����æ� ������, ����� %d  : %s" 
#define MSGTR_SKIN_WARNING1 "[�����] ������������: � ���̦ ���Ʀ����æ� ������, ����� %d: widget ��������� ��� �� ����� �� �������� \"section\" (%s)"
#define MSGTR_SKIN_WARNING2 "[�����] ������������: � ���̦ ���Ʀ����æ� ������, ����� %d: widget ��������� ��� �� ����� �� �������� \"subsection\" (%s)"
#define MSGTR_SKIN_WARNING3 "[�����] ������������: � ���̦ ���Ʀ����æ� ������, ����� %d: ��� widget (%s) �� Ц�����դ �� subsection"
#define MSGTR_SKIN_BITMAP_16bit  "������� ������� ¦���ϧ ����� � 16 ¦� � ����� �� Ц�����դ���� (%s).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "���� �� ��������� (%s)\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "������� ������� BMP (%s)\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "������� ������� TGA (%s)\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "������� ������� PNG (%s)\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "RLE ����������� TGA �� Ц�����դ���� (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "��צ����� ��� ����� (%s)\n"
#define MSGTR_SKIN_BITMAP_ConvertError "������� ������������ 24-¦� � 32-¦� (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "��צ���� ��צ��������: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "�� �������� ���'�Ԧ\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "��������� ����� ������ ����Ԧ�\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "���� ������ �� ���������\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "���� ����ڦ� ������ �� ���������\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "�Ŧ������� ������Ʀ����� ������ (%s)\n"
#define MSGTR_SKIN_UnknownParameter "��צ����� �������� (%s)\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "����� �� �������� (%s).\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "������� ������� ����� ���Ʀ����æ� ������ (%s).\n"
#define MSGTR_SKIN_LABEL "������:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "��� ��������"
#define MSGTR_MENU_Open "��������..."
#define MSGTR_MENU_PlayFile "����� ����..."
#define MSGTR_MENU_PlayVCD "����� VCD..."
#define MSGTR_MENU_PlayDVD "����� DVD..."
#define MSGTR_MENU_PlayURL "����� URL..."
#define MSGTR_MENU_LoadSubtitle "����������� ��������..."
#define MSGTR_MENU_DropSubtitle "�������� ��������..."
#define MSGTR_MENU_LoadExternAudioFile "����������� ���Φ�Φ� ��Ħ� ����..."
#define MSGTR_MENU_Playing "�����������"
#define MSGTR_MENU_Play "�����"
#define MSGTR_MENU_Pause "�����"
#define MSGTR_MENU_Stop "��������"
#define MSGTR_MENU_NextStream "��������� ��Ԧ�"
#define MSGTR_MENU_PrevStream "�������Φ� ��Ԧ�"
#define MSGTR_MENU_Size "���ͦ�"
#define MSGTR_MENU_NormalSize "���������� ���ͦ�"
#define MSGTR_MENU_DoubleSize "���צ���� ���ͦ�"
#define MSGTR_MENU_FullScreen "������ �����"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "����� ����..."
#define MSGTR_MENU_ShowDVDMenu "�������� DVD ����"
#define MSGTR_MENU_Titles "�����"
#define MSGTR_MENU_Title "���� %2d"
#define MSGTR_MENU_None "(����)"
#define MSGTR_MENU_Chapters "���Ħ��"
#define MSGTR_MENU_Chapter "���Ħ� %2d"
#define MSGTR_MENU_AudioLanguages "���� ����"
#define MSGTR_MENU_SubtitleLanguages "���� ������Ҧ�"
#define MSGTR_MENU_SkinBrowser "���������� ����Φ�"
#define MSGTR_MENU_Exit "��Ȧ�..."
#define MSGTR_MENU_Mute "����"
#define MSGTR_MENU_Original "��Ȧ����"
#define MSGTR_MENU_AspectRatio "���������� ���Ҧ�"
#define MSGTR_MENU_AudioTrack "��Ħ� ��Ҧ���"
#define MSGTR_MENU_Track "��Ҧ��� %d"
#define MSGTR_MENU_VideoTrack "����� ��Ҧ���"

// --- equalizer
#define MSGTR_EQU_Audio "��Ħ�"
#define MSGTR_EQU_Video "�����"
#define MSGTR_EQU_Contrast "��������: "
#define MSGTR_EQU_Brightness "�����צ���: "
#define MSGTR_EQU_Hue "���: "
#define MSGTR_EQU_Saturation "������Φ���: "
#define MSGTR_EQU_Front_Left "�����Φ� ����"
#define MSGTR_EQU_Front_Right "�����Φ� ������"
#define MSGTR_EQU_Back_Left "���Φ� ����"
#define MSGTR_EQU_Back_Right "���Φ� ������"
#define MSGTR_EQU_Center "�����������"
#define MSGTR_EQU_Bass "���"
#define MSGTR_EQU_All "�Ӧ"
#define MSGTR_EQU_Channel1 "����� 1:"
#define MSGTR_EQU_Channel2 "����� 2:"
#define MSGTR_EQU_Channel3 "����� 3:"
#define MSGTR_EQU_Channel4 "����� 4:"
#define MSGTR_EQU_Channel5 "����� 5:"
#define MSGTR_EQU_Channel6 "����� 6:"

// --- playlist
#define MSGTR_PLAYLIST_Path "����"
#define MSGTR_PLAYLIST_Selected "�����Φ �����"
#define MSGTR_PLAYLIST_Files "�����"
#define MSGTR_PLAYLIST_DirectoryTree "������ ��������"

// --- preferences
#define MSGTR_PREFERENCES_SubtitleOSD "�������� � OSD"
#define MSGTR_PREFERENCES_Codecs "������ � demuxer"
#define MSGTR_PREFERENCES_Misc "����"

#define MSGTR_PREFERENCES_None "�����"
#define MSGTR_PREFERENCES_AvailableDrivers "������Φ ��������:"
#define MSGTR_PREFERENCES_DoNotPlaySound "�� ����� ����"
#define MSGTR_PREFERENCES_NormalizeSound "�����̦������ ����"
#define MSGTR_PREFERENCES_EnEqualizer "��������� ����������"
#define MSGTR_PREFERENCES_ExtraStereo "��������� ��������� ������"
#define MSGTR_PREFERENCES_Coefficient "���Ʀæ���:"
#define MSGTR_PREFERENCES_AudioDelay "�������� ��Ħ�"
#define MSGTR_PREFERENCES_DoubleBuffer "��������� ���צ��� �����������"
#define MSGTR_PREFERENCES_DirectRender "��������� ������ ��צ�"
#define MSGTR_PREFERENCES_FrameDrop "��������� ������� ���Ҧ�"
#define MSGTR_PREFERENCES_HFrameDrop "��������� ����������� ���Ҧ� (����������)"
#define MSGTR_PREFERENCES_Flip "����������� ���������� ������ ������"
#define MSGTR_PREFERENCES_Panscan "Panscan: "
#define MSGTR_PREFERENCES_OSDTimer "������ �� ��Ħ������"
#define MSGTR_PREFERENCES_OSDProgress "���� ̦Φ���"
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "������, �������� �� ��������� ���"
#define MSGTR_PREFERENCES_Subtitle "��������:"
#define MSGTR_PREFERENCES_SUB_Delay "��������: "
#define MSGTR_PREFERENCES_SUB_FPS "�/c:"
#define MSGTR_PREFERENCES_SUB_POS "���������: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "���������� ���������������� ������Ҧ�"
#define MSGTR_PREFERENCES_SUB_Unicode "Unicode ��������"
#define MSGTR_PREFERENCES_SUB_MPSUB "����������� �����Φ �������� �� ������� MPlayer"
#define MSGTR_PREFERENCES_SUB_SRT "����������� �����Φ �������� �� ������� SubViewer (SRT)"
#define MSGTR_PREFERENCES_SUB_Overlap "���������/���������� ���������� ������Ҧ�"
#define MSGTR_PREFERENCES_Font "�����:"
#define MSGTR_PREFERENCES_FontFactor "������ ������:"
#define MSGTR_PREFERENCES_PostProcess "��������� postprocessing"
#define MSGTR_PREFERENCES_AutoQuality "���� �˦���: "
#define MSGTR_PREFERENCES_NI "��������������� ������������� AVI ������"
#define MSGTR_PREFERENCES_IDX "������������ ������, ���� �����"
#define MSGTR_PREFERENCES_VideoCodecFamily "������� צ��� ������:"
#define MSGTR_PREFERENCES_AudioCodecFamily "������� ��Ħ� ������:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "����� OSD"
#define MSGTR_PREFERENCES_FRAME_Subtitle "��������"
#define MSGTR_PREFERENCES_FRAME_Font "�����"
#define MSGTR_PREFERENCES_FRAME_PostProcess "Postprocessing"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "����� � demuxer"
#define MSGTR_PREFERENCES_FRAME_Cache "���"
#define MSGTR_PREFERENCES_Message "�� ��������, �� ��� ����� ������������� ����������� ��� ������� ������Ԧ ������ �������Ҧ�!"
#define MSGTR_PREFERENCES_DXR3_VENC "����� �����:"
#define MSGTR_PREFERENCES_DXR3_LAVC "��������������� LAVC (FFmpeg)"
#define MSGTR_PREFERENCES_DXR3_FAME "��������������� FAME"
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
#define MSGTR_PREFERENCES_FontNoAutoScale "��� �����������������"
#define MSGTR_PREFERENCES_FontPropWidth "������æ��� ����Φ �����"
#define MSGTR_PREFERENCES_FontPropHeight "������æ��� ����Ԧ �����"
#define MSGTR_PREFERENCES_FontPropDiagonal "������æ��� Ħ�����̦ �����"
#define MSGTR_PREFERENCES_FontEncoding "���������:"
#define MSGTR_PREFERENCES_FontBlur "�����������:"
#define MSGTR_PREFERENCES_FontOutLine "���������:"
#define MSGTR_PREFERENCES_FontTextScale "������� ������:"
#define MSGTR_PREFERENCES_FontOSDScale "������� OSD:"
#define MSGTR_PREFERENCES_Cache "��� on/off"
#define MSGTR_PREFERENCES_CacheSize "���ͦ� ����: "
#define MSGTR_PREFERENCES_LoadFullscreen "���������� � ������ �����"
#define MSGTR_PREFERENCES_SaveWinPos "���Ҧ���� ��������� צ���"
#define MSGTR_PREFERENCES_XSCREENSAVER "Stop XScreenSaver"
#define MSGTR_PREFERENCES_PlayBar "��������� ̦Φ��� �����������"
#define MSGTR_PREFERENCES_AutoSync "AutoSync on/off"
#define MSGTR_PREFERENCES_AutoSyncValue "Autosync: "
#define MSGTR_PREFERENCES_CDROMDevice "CD-ROM �����Ҧ�:"
#define MSGTR_PREFERENCES_DVDDevice "DVD �����Ҧ�:"
#define MSGTR_PREFERENCES_FPS "���Ҧ� �� �������:"
#define MSGTR_PREFERENCES_ShowVideoWindow "���������� ��������� צ��� ����������"

#define MSGTR_ABOUT_UHU "GUI �������� ���������� UHU Linux\n"
#define MSGTR_ABOUT_CoreTeam "   MPlayer ������� ��������˦�:\n"
#define MSGTR_ABOUT_AdditionalCoders "   �������צ ������������:\n"
#define MSGTR_ABOUT_MainTesters "   �����Φ �������ަ:\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "�������� �������..."
#define MSGTR_MSGBOX_LABEL_Error "�������..."
#define MSGTR_MSGBOX_LABEL_Warning "������������..." 

#endif

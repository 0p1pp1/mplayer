// Sync'ed with help_mp-en.h 1.115
// Translated to Japanese Language file. Used encoding: EUC-JP

// Translated by smoker <http://smokerz.net/~smoker/>

// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"Usage:   mplayer [options] [url|path/]filename\n"
"\n"
"����Ū�ʥ��ץ����: (man page ���������夵��Ƥ��ޤ�)\n"
" -vo <drv[:dev]>  �������ϥɥ饤�еڤӥǥХ��������򤷤ޤ� ('-vo help'�ǰ���ɽ������ޤ�)\n"
" -ao <drv[:dev]>  �������ϥɥ饤�еڤӥǥХ��������򤷤ޤ� ('-ao help'�ǰ���ɽ������ޤ�)\n"
#ifdef HAVE_VCD
" vcd://<trackno>   play VCD (Video CD) track from device instead of plain file\n"
#endif
#ifdef USE_DVDREAD
" dvd://<titleno>   play DVD title from device instead of plain file\n"
" -alang/-slang    select DVD audio/subtitle language (by 2-char country code)\n"
#endif
" -ss <timepos>    timepos��Ϳ����줿��꤫��������ޤ�(seconds or hh:mm:ss)\n"
" -nosound         �������Ϥ��޻ߤ��ޤ�\n"
" -fs              �ե륹���꡼��ɽ�����ޤ�(�⤷���� -vm, -zoom, �ܺ٤�man�ˤ���ޤ�)\n"
" -x <x> -y <y>    ɽ������������ꤷ�ޤ� (���˼��Υ��ץ��������Ѳ����� -vm or -zoom)\n"
" -sub <file>      ���Ѥ��� subtitle �ե���������򤹤�(-subfps, -subdelay �����������)\n"
" -playlist <file> playlist�ե���������򤹤�\n"
" -vid x -aid y    select video (x) and audio (y) stream to play\n"
" -fps x -srate y  change video (x fps) and audio (y Hz) rate\n"
" -pp <quality>    postprocessing filter��ͭ���ˤ��� (�ܺ٤� man page �ˤ���ޤ�)\n"
" -framedrop       frame dropping��ͭ���ˤ��� (��®�ʥޥ�������Ǥ�)\n"
"\n"
"����Ū�ʥ��ޥ��: (man page���������夵��Ƥ��ޤ���������input.conf���ǧ���Ʋ�����)\n"
" <-  or  ->       10��ñ�̤�����˥��������ޤ�\n"
" up or down       1ʬñ�̤�����˥��������ޤ�\n"
" pgup or pgdown   10ʬñ�̤�����˥��������ޤ�\n"
" < or >           �ץ쥤�ꥹ�Ȥ򸵤�����Υե���������ܤ��ޤ�\n"
" p or SPACE       �������Żߤ��ޤ�(�����ܥ���򲡲�����Ⱥ����򳫻Ϥ��ޤ�)\n"
" q or ESC         �������Żߤ����ץ�������ߤ��ޤ�\n"
" + or -           ������ 0.1 ��ñ�̤���᤿���٤줵������Ĵ������\n"
" o                cycle OSD mode:  none / seekbar / seekbar + timer\n"
" * or /           PCM ���̤�夲���겼�����ꤹ��\n"
" z or x           subtitle�� 0.1 ��ñ�̤���᤿���٤줵������Ĵ������\n"
" r or t           subtitle�ΰ��֤�夲���겼������Ĵ������, -vf���ץ������ǧ���Ʋ�����\n"
"\n"
" * * * man page�˾ܺ٤�����ޤ��Τǡ���ǧ���Ʋ�����������˹��٤ǿʤ�����ץ����䥭���⵭�ܤ��Ƥޤ� * * *\n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// mplayer.c:

#define MSGTR_Exiting "\n��λ���Ƥ��ޤ�... (%s)\n"
#define MSGTR_Exit_quit "��λ"
#define MSGTR_Exit_eof "�ե��������ü�Ǥ�"
#define MSGTR_Exit_error "��̿Ū���顼"
#define MSGTR_IntBySignal "\nMPlayer �ϥ����ʥ� %d �ˤ�ä����Ǥ��ޤ��������Υ⥸�塼�뤫��Ǥ�: %s\n"
#define MSGTR_NoHomeDir "�ۡ���ǥ��쥯�ȥ���դ��뤳�Ȥ�����ޤ���Ǥ���.\n"
#define MSGTR_GetpathProblem "get_path(\"config\") �����꤬�����ޤ���\n"
#define MSGTR_CreatingCfgFile "config file��������ޤ���: %s\n"
#define MSGTR_InvalidVOdriver "̵���ʱ������ϥɥ饤��: %s\n '-vo help' ��ͭ���ʱ������ϥɥ饤�а���������Ǥ��ޤ�.\n"
#define MSGTR_InvalidAOdriver "̵���ʲ������ϥɥ饤��: %s\n '-ao help' ��ͭ���ʲ������ϥɥ饤�а���������Ǥ��ޤ�.\n"
#define MSGTR_CopyCodecsConf "MPlayer�Υ������� etc/codecs.conf�Υ��ԡ�����󥯤� ~/.mplayer/codecs.conf �˺������Ʋ�����)\n"
#define MSGTR_BuiltinCodecsConf "�Ȥ߹��ޤ줿�ǥե���Ȥ� codecs.conf �����Ѥ��Ƥޤ�\n"
#define MSGTR_CantLoadFont "�ե���Ȥ���ɽ���ޤ���: %s\n"
#define MSGTR_CantLoadSub "���֥����ȥ����ɽ���ޤ���: %s\n"
#define MSGTR_ErrorDVDkey "DVD key�ν����ǥ��顼\n"
#define MSGTR_CmdlineDVDkey "The requested DVD key is used for descrambling.\n"
#define MSGTR_DVDauthOk "DVDǧ�ڽ���������˴�λ���ޤ���\n"
#define MSGTR_DumpSelectedStreamMissing "dump: FATAL: selected stream missing!\n"
#define MSGTR_CantOpenDumpfile "dump file�򳫤��ޤ���\n"
#define MSGTR_CoreDumped "��������� ;)\n"
#define MSGTR_FPSnotspecified "FPS ���إå��˻��ꤵ��Ƥ��ʤ��������Ǥ�. -fps ���ץ��������Ѥ��Ʋ�����.\n"
#define MSGTR_TryForceAudioFmtStr "Trying to force audio codec driver family %s ...\n"
#define MSGTR_CantFindAfmtFallback "Cannot find audio codec for forced driver family, falling back to other drivers.\n"
#define MSGTR_CantFindAudioCodec "audio format 0x%X �����Υ����ǥå����դ����������ޤ���Ǥ���.\n"
#define MSGTR_RTFMCodecs "DOCS/HTML/en/codecs.html �����������\n"
#define MSGTR_CouldntInitAudioCodec "�������ϥ����ǥå��ν�����˼��Ԥ��ޤ��� -> ̵�����ˤʤ�ޤ�.\n"
#define MSGTR_TryForceVideoFmtStr "Trying to force video codec driver family %s ...\n"
#define MSGTR_CantFindVideoCodec "Cannot find codec matching selected -vo and video format 0x%X.\n"
#define MSGTR_VOincompCodec "���򤵤줿�������ϥǥХ����ϥ����ǥå��ȸߴ���������ޤ���\n"
#define MSGTR_CannotInitVO "FATAL: �������ϥɥ饤�Фν�����˼��Ԥ��ޤ���.\n"
#define MSGTR_CannotInitAO "�����ǥХ����ν�����˼��Ԥ��ޤ��� -> ̵�����ˤʤ�ޤ�.\n"
#define MSGTR_StartPlaying "��������...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"           ************************************************\n"\
"           ***  �����Υ����ƥ�Ϥ�����������ˤ��٤�  ***\n"\
"           ************************************************\n\n"\
"ͽ�ۤ���������Ķ���:\n"\
"- Most common: ����Ƥ뤫���Х����� �����ɥ饤�С�\n"\
"  - -ao ��Ȥ� sdl �� ALSA 0.5 �⤷����ALSA 0.9��OSS���ߥ�졼�������ߤ�.\n"\
"  - Experiment with different values for -autosync, 30 is a good start.\n"\
"- �������Ϥ��٤�\n"\
"  - �㤦 -vo �ɥ饤�Ф����Ѥ��뤫(-vo help�ǥꥹ�Ȥ������ޤ�) -framedrop���ߤ�\n"\
"- CPU���٤�\n"\
"  - �����DVD��DivX���٤�CPU�Ǻ������褦�Ȼ�ߤʤ� ;-) -hardframedrop���ߤ�\n"\
"- �ե����뤬����Ƥ�\n"\
"  - ���Υ��ץ������͡����ȹ礻���ߤƲ�����: -nobps -ni -forceidx -mc 0.\n"\
"- �٤���ǥ���(NFS/SMB ���ä���, DVD, VCD �ʤɤΥɥ饤�֤��٤��ä���)\n"\
"  -���Υ��ץ������ߤ� -cache 8192.\n"\
"- non-interleaved AVI �ե������ -cache���ץ�����ȤäƤޤ���?\n"\
"  - ���Υ��ץ������ߤ� -nocache.\n"\
"���塼�˥󥰡����ԡ��ɥ��åפΰ٤� DOCS/HTML/en/devices.html �����������.\n"\
"�⤷���������Ƥⲿ�⤳�����������ʤ����ϡ�DOCS/HTML/en/bugreports.html �����������.\n\n"

#define MSGTR_NoGui "MPlayer ��GUI���ݡ��Ȥ�̵���ˤ��ƥ���ѥ��뤵��ޤ���.\n"
#define MSGTR_GuiNeedsX "MPlayer��GUI��X11��ɬ�פȤ��ޤ�.\n"
#define MSGTR_Playing "%s �������\n"
#define MSGTR_NoSound "����: ̵��\n"
#define MSGTR_FPSforced "FPS forced to be %5.3f  (ftime: %5.3f)\n"
#define MSGTR_CompiledWithRuntimeDetection "Compiled with Runtime CPU Detection - WARNING - this is not optimal!\nTo get best performance, recompile MPlayer with --disable-runtime-cpudetection\n"
#define MSGTR_CompiledWithCPUExtensions "x86 CPU �����˥���ѥ��뤵��ޤ���:"
#define MSGTR_AvailableVideoOutputPlugins "ͭ���ʲ������ϥץ饰����:\n"
#define MSGTR_AvailableVideoOutputDrivers "ͭ���ʱ������ϥɥ饤��:\n"
#define MSGTR_AvailableAudioOutputDrivers "ͭ���ʲ������ϥɥ饤��:\n"
#define MSGTR_AvailableAudioCodecs "ͭ���ʲ��������ǥå�:\n"
#define MSGTR_AvailableVideoCodecs "ͭ���ʱ��������ǥå�:\n"
#define MSGTR_AvailableAudioFm "\nͭ����(�Ȥ߹��ޤ줿)���������ǥå� families/drivers:\n"
#define MSGTR_AvailableVideoFm "\nͭ����(�Ȥ߹��ޤ줿)���������ǥå� families/drivers:\n"
#define MSGTR_AvailableFsType "������ɽ���⡼�ɤؤ����ؤ��ϲ�ǽ�Ǥ�:\n"
#define MSGTR_UsingRTCTiming "Linux hardware RTC timing (%ldHz) ��ȤäƤ��ޤ�.\n"
#define MSGTR_CannotReadVideoProperties "Video: Cannot read properties.\n"
#define MSGTR_NoStreamFound "���ȥ꡼����դ��뤳�Ȥ�����ޤ���Ǥ���.\n"
#define MSGTR_InitializingAudioCodec "���������ǥå���������...\n"
#define MSGTR_ErrorInitializingVODevice "���򤵤줿��������(-vo)�ǥХ����򳫤���������ޤ���Ǥ���.\n"
#define MSGTR_ForcedVideoCodec "Forced video codec: %s\n"
#define MSGTR_ForcedAudioCodec "Forced audio codec: %s\n"
#define MSGTR_AODescription_AOAuthor "AO: �ܺ�: %s\nAO: ����: %s\n"
#define MSGTR_AOComment "AO: ������: %s\n"
#define MSGTR_Video_NoVideo "Video: ����������ޤ���\n"
#define MSGTR_NotInitializeVOPorVO "\nFATAL: �����ե��륿��(-vf)����������(-vo)�ν�����˼��Ԥ��ޤ���.\n"
#define MSGTR_Paused "\n  =====  ���  =====\r" // no more than 23 characters (status line for audio files)
#define MSGTR_PlaylistLoadUnable "\n�ץ쥤�ꥹ�Ȥ��ɤ߹��ߤ�����ޤ��� %s.\n"
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
"- Mplayer�����ɤ� CPU/FPU/RAM �ˤ�äƥ���å��夷�ޤ���.\n"\
"  Recompile MPlayer with --enable-debug and make a 'gdb' backtrace and\n"\
"  --enable-debug��Ĥ���MPlyaer�򥳥�ѥ��뤷�ʤ�����gdb��Ĵ�����ޤ��礦\n"\
"  �ܺ٤� DOCS/HTML/en/bugreports.html#bugreports_crash �ˤ���ޤ�\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer crashed. This shouldn't happen.\n"\
"  It can be a bug in the MPlayer code _or_ in your drivers _or_ in your\n"\
"  gcc version. If you think it's MPlayer's fault, please read\n"\
"  DOCS/HTML/en/bugreports.html and follow the instructions there. We can't and\n"\
"  won't help unless you provide this information when reporting a possible bug.\n"


// mencoder.c:

#define MSGTR_UsingPass3ControllFile "Using pass3 control file: %s\n"
#define MSGTR_MissingFilename "\nFilename missing.\n\n"
#define MSGTR_CannotOpenFile_Device "�ե�����ڤӥǥХ����������ޤ���.\n"
#define MSGTR_ErrorDVDAuth "Error in DVD authentication.\n"
#define MSGTR_CannotOpenDemuxer "demuxer�򳫤����Ȥ�����ޤ���.\n"
#define MSGTR_NoAudioEncoderSelected "\n�������󥳡���(-oac)�����ꤵ��Ƥ��ޤ��� �������ꤹ�뤫��̵����(-nosound)��Ϳ���Ʋ��������ܺ٤� '-oac help'\n"
#define MSGTR_NoVideoEncoderSelected "\n�������󥳡���(-ovc)�����ꤵ��Ƥ��ޤ��� �������ꤷ�Ʋ������� �ܺ٤� '-ovc help'\n"
// #define MSGTR_InitializingAudioCodec "Initializing audio codec...\n"
#define MSGTR_CannotOpenOutputFile "���ϥե�����'%s'�򳫤���������ޤ���.\n"
#define MSGTR_EncoderOpenFailed "���󥳡����򳫤����Ȥ˼��Ԥ��ޤ���.\n"
#define MSGTR_ForcingOutputFourcc "fourcc�� %x [%.4s] �˻��ꤷ�ޤ�\n"
#define MSGTR_WritingAVIHeader "AVI�إå���񤭤�����...\n"
#define MSGTR_DuplicateFrames "\n%d ��ʣ�����ե졼��\n"
#define MSGTR_SkipFrame "\n�ե졼��򥹥��åפ��Ƥ��ޤ�\n"
#define MSGTR_ErrorWritingFile "%s: �ե�����񤭹��ߥ��顼.\n"
#define MSGTR_WritingAVIIndex "\nAVI index��񤭹�����...\n"
#define MSGTR_FixupAVIHeader "AVI�إå�������...\n"
#define MSGTR_RecommendedVideoBitrate "Recommended video bitrate for %s CD: %d\n"
#define MSGTR_VideoStreamResult "\n�������ȥ꡼��: %8.3f kbit/s  (%d bps)  ������: %d bytes  %5.3f secs  %d �ե졼��\n"
#define MSGTR_AudioStreamResult "\n�������ȥ꡼��: %8.3f kbit/s  (%d bps)  ������: %d bytes  %5.3f secs\n"

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

// open.c, stream.c:
#define MSGTR_CdDevNotfound "CD-ROM �ǥХ��� '%s' ��¸�ߤ��ޤ���.\n"
#define MSGTR_ErrTrackSelect "Error selecting VCD track."
#define MSGTR_ReadSTDIN "ɸ�����Ϥ����ɤ߹���Ǥ��ޤ�...\n"
#define MSGTR_UnableOpenURL "���ꤵ�줿URL���ɤ߹���ޤ���: %s\n"
#define MSGTR_ConnToServer "�����Ф���³��: %s\n"
#define MSGTR_FileNotFound "�ե����뤬¸�ߤ��ޤ���: '%s'\n"

#define MSGTR_SMBInitError "libsmbclient �ν��������: %d\n"
#define MSGTR_SMBFileNotFound "�����륨�ꥢ�ͥåȥ�����鳫�����Ȥ�����ޤ���Ǥ���: '%s'\n"
#define MSGTR_SMBNotCompiled "MPlayer ��SMB reading support ��̵���ˤ��ƥ���ѥ��뤵��Ƥ��ޤ�\n"

#define MSGTR_CantOpenDVD "DVD�ǥХ����򳫤����Ȥ�����ޤ���Ǥ���: %s\n"
#define MSGTR_DVDwait "�ǥ��������ɤ߼�äƤޤ������Ԥ�������...\n"
#define MSGTR_DVDnumTitles "����DVD�ˤ� %d �����ȥ뵭Ͽ����Ƥ��ޤ�.\n"
#define MSGTR_DVDinvalidTitle "������ DVD �����ȥ��ֹ�Ǥ�: %d\n"
#define MSGTR_DVDnumChapters "����DVD�� %d ����ץ�������ޤ�.\n"
#define MSGTR_DVDinvalidChapter "������DVD����ץ����ֹ�Ǥ�r: %d\n"
#define MSGTR_DVDnumAngles "����DVD�ˤ� %d ���󥰥뤢��ޤ�.\n"
#define MSGTR_DVDinvalidAngle "������DVD���󥰥��ֹ�Ǥ�: %d\n"
#define MSGTR_DVDnoIFO "Cannot open the IFO file for DVD title %d.\n"
#define MSGTR_DVDnoVOBs "Cannot open title VOBS (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD�򳫤����Ȥ��������ޤ���.\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "�ٹ�: Audio stream header %d redefined.\n"
#define MSGTR_VideoStreamRedefined "�ٹ�: Video stream header %d redefined.\n"
#define MSGTR_TooManyAudioInBuffer "\nToo many audio packets in the buffer: (%d in %d bytes).\n"
#define MSGTR_TooManyVideoInBuffer "\nToo many video packets in the buffer: (%d in %d bytes).\n"
#define MSGTR_MaybeNI "Maybe you are playing a non-interleaved stream/file or the codec failed?\n" \
		      "For AVI files, try to force non-interleaved mode with the -ni option.\n"
#define MSGTR_SwitchToNi "\nBadly interleaved AVI file detected - switching to -ni mode...\n"
#define MSGTR_Detected_XXX_FileFormat "%s file format detected.\n"
#define MSGTR_DetectedAudiofile "Audio file detected.\n"
#define MSGTR_NotSystemStream "Not MPEG System Stream format... (maybe Transport Stream?)\n"
#define MSGTR_InvalidMPEGES "Invalid MPEG-ES stream??? Contact the author, it may be a bug :(\n"
#define MSGTR_FormatNotRecognized "============ ���Υե�����ե����ޥåȤ� ���ݡ��Ȥ��Ƥ��ޤ��� =============\n"\
				  "======= �⤷���Υե����뤬 AVI��ASF��MPEG�ʤ�����Ԥ�Ϣ���Ʋ����� ======\n"
#define MSGTR_MissingVideoStream "�������ȥ꡼�ब¸�ߤ��ޤ���.\n"
#define MSGTR_MissingAudioStream "�������ȥ꡼�ब¸�ߤ��ޤ��� -> ̵�����ˤʤ�ޤ�\n"
#define MSGTR_MissingVideoStreamBug "Missing video stream!? �����Ԥ�Ϣ���Ʋ����������餯����ϥХ��Ǥ� :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: ���򤵤줿 �������������Ǽ���뤳�Ȥ�����ޤ���.\n"

#define MSGTR_NI_Forced "Forced"
#define MSGTR_NI_Detected "Detected"
#define MSGTR_NI_Message "%s NON-INTERLEAVED AVI �ե����� �ե����ޥå�.\n"

#define MSGTR_UsingNINI "Using NON-INTERLEAVED broken AVI file format.\n"
#define MSGTR_CouldntDetFNo "Could not determine number of frames (for absolute seek).\n"
#define MSGTR_CantSeekRawAVI "Cannot seek in raw AVI streams. (Index��ɬ�פǤ�, -idx ���Ʋ�����.)\n"
#define MSGTR_CantSeekFile "���Υե�����ϥ��������뤳�Ȥ�����ޤ���.\n"

#define MSGTR_EncryptedVOB "�Ź沽���줿VOB(Encryoted VOB)�ե�����Ǥ���DOCS/HTML/en/cd-dvd.html �����������.\n"
#define MSGTR_EncryptedVOBauth "�Ź沽���줿���ȥ꡼��Ǥ�����ǧ�ڤ�ɬ�פȻ��ꤵ��Ƥ��ޤ���\n"

#define MSGTR_MOVcomprhdr "MOV: ���̤��줿�إå�(Compressd headers)�򥵥ݡ��Ȥ���ˤ� ZLIB ��ɬ�פǤ�\n"
#define MSGTR_MOVvariableFourCC "MOV: �ٹ�: Variable FOURCC detected!?\n"
#define MSGTR_MOVtooManyTrk "MOV: �ٹ�: too many tracks"
#define MSGTR_FoundAudioStream "==> �������ȥ꡼�ब���դ���ޤ���: %d\n"
#define MSGTR_FoundVideoStream "==> �������ȥ꡼�ब���դ���ޤ���: %d\n"
#define MSGTR_DetectedTV "TV detected! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "ogg demuxer �򳫤����Ȥ�����ޤ���.\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: �������ȥ꡼���õ���Ƥ��ޤ� (id:%d).\n"
#define MSGTR_CannotOpenAudioStream "�������ȥ꡼��򳫤����Ȥ�����ޤ���: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "���֥����ȥ륹�ȥ꡼��򳫤����Ȥ�����ޤ���: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "audio demuxer�򳫤����ȳ������Ȥ�����ޤ���: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "subtitle demuxer�򳫤����Ȥ�����ޤ���: %s\n"
#define MSGTR_TVInputNotSeekable "TV���Ϥϥ��������뤳�ȤϽ���ޤ���(�������϶��餯�����ͥ��������������ΤǤ�? ;)\n"
#define MSGTR_DemuxerInfoAlreadyPresent "Demuxer info %s already present!\n"
#define MSGTR_ClipInfo "Clip info:\n"

#define MSGTR_LeaveTelecineMode "\ndemux_mpg: 30fps NTSC content detected, switching framerate.\n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: 24fps progressive NTSC content detected, switching framerate.\n"


// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "�����ǥå��򳫤����Ȥ�����ޤ���.\n"
#define MSGTR_CantCloseCodec "�����ǥå����Ĥ��뤳�Ȥ�����ޤ���.\n"

#define MSGTR_MissingDLLcodec "���顼: �׵ᤵ�줿 DirectShow �����ǥå� %s �򳫤����Ȥ�����ޤ���.\n"
#define MSGTR_ACMiniterror "Win32/ACM���������ǥå����ɤ߹��ߵڤӽ�����򤹤뤳�Ȥ�����ޤ��� (DLL�ե����������פǤ���?).\n"
#define MSGTR_MissingLAVCcodec "'%s' �� libavcodec���鸫�դ��뤳�Ȥ�����ޤ��� ...\n"

#define MSGTR_MpegNoSequHdr "MPEG: FATAL: EOF while searching for sequence header.\n"
#define MSGTR_CannotReadMpegSequHdr "FATAL: �������󥹥إå�(sequence header)���ɤ߹���ޤ���.\n"
#define MSGTR_CannotReadMpegSequHdrEx "FATAL: ��ĥ�������󥹥إå�(sequence header extension)���ɤ߹���ޤ���.\n"
#define MSGTR_BadMpegSequHdr "MPEG: �����ʥ������󥹥إå�(sequence header)\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: �����ʳ�ĥ�������󥹥إå�(sequence header)\n"

#define MSGTR_ShMemAllocFail "��ͭ����γ��ݤ˼���\n"
#define MSGTR_CantAllocAudioBuf "�������ϥХåե����γ��ݤ˼���Cannot allocate audio out buffer\n"

#define MSGTR_UnknownAudio "̤�ΤΡ��⤷���ϲ��줿�����ե����ޥåȤǤ� -> ̵�����ˤʤ�ޤ�\n"

#define MSGTR_UsingExternalPP "[PP] Using external postprocessing filter, max q = %d.\n"
#define MSGTR_UsingCodecPP "[PP] Using codec's postprocessing, max q = %d.\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "���򤵤줿 vo �� vd �Ǥϱ���°�� '%s' �ϥ��ݡ��Ȥ���Ƥޤ���.\n"
#define MSGTR_VideoCodecFamilyNotAvailableStr "�׵ᤵ�줿���������ǥå� [%s] (vfm=%s) ��̵���Ǥ� (ͭ���ˤ���ˤϥ���ѥ�����˻��ꤷ�ޤ�)\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "�׵ᤵ�줿���������ǥå� [%s] (afm=%s) ��̵���Ǥ� (ͭ���ˤ���ˤϥ���ѥ�����˻��ꤷ�ޤ�)\n"
#define MSGTR_OpeningVideoDecoder "���������ǥå��򳫤��Ƥ��ޤ�: [%s] %s\n"
#define MSGTR_OpeningAudioDecoder "���������ǥå��򳫤��Ƥ��ޤ�: [%s] %s\n"
#define MSGTR_UninitVideoStr "uninit video: %s\n"
#define MSGTR_UninitAudioStr "uninit audio: %s\n"
#define MSGTR_VDecoderInitFailed "�����ǥ������ν�����˼��Ԥ��ޤ��� :(\n"
#define MSGTR_ADecoderInitFailed "�����ǥ������ν�����˼��Ԥ��ޤ��� :(\n"
#define MSGTR_ADecoderPreinitFailed "�����ǥ��������������˼��� :(\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: ���ϥХåե��� %d bytes ���ݤ��ޤ���\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: ���ϥХåե��� %d + %d = %d bytes ���ݤ��ޤ���\n"

// LIRC:
#define MSGTR_SettingUpLIRC "LIRC ���ݡ��Ȥ򥻥åƥ�����...\n"
#define MSGTR_LIRCdisabled "You will not be able to use your remote control.\n"
#define MSGTR_LIRCopenfailed "LIRC ���ݡ��Ȥ򳫤����˼���.\n"
#define MSGTR_LIRCcfgerr "LIRC ����ե����� %s �򳫤����Ȥ˼��Ԥ��ޤ���.\n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "�����ե��륿 '%s' �����դ���ޤ���\n"
#define MSGTR_CouldNotOpenVideoFilter "�����ե��륿 '%s' �����դ���ޤ���\n"
#define MSGTR_OpeningVideoFilter "�����ե��륿�򳫤��Ƥ��ޤ�: "
#define MSGTR_CannotFindColorspace "common colorspace�����դ���ޤ���, even by inserting 'scale' :(\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDec: Codec did not set sh->disp_w and sh->disp_h, trying workaround.\n"
#define MSGTR_VoConfigRequest "VDec: ������������ - %d x %d (preferred csp: %s)\n"
#define MSGTR_CouldNotFindColorspace "���פ��륫�顼���ڡ��������դ���ޤ��� - '-vop'��Ĥ��ƻ�ߤƲ�����...\n"
#define MSGTR_MovieAspectIsSet "Movie-Aspect is %.2f:1 - prescaling to correct movie aspect.\n"
#define MSGTR_MovieAspectUndefined "Movie-Aspect is undefined - no prescaling applied.\n"

// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "About"
#define MSGTR_FileSelect "�ե��������� ..."
#define MSGTR_SubtitleSelect "���֥����ȥ����� ..."
#define MSGTR_OtherSelect "���� ..."
#define MSGTR_AudioFileSelect "Select external audio channel ..."
#define MSGTR_FontSelect "�ե�������� ..."
#define MSGTR_PlayList "�ץ쥤�ꥹ��"
#define MSGTR_Equalizer "�����饤����"
#define MSGTR_SkinBrowser "������֥饦��"
#define MSGTR_Network "Network streaming..."
#define MSGTR_Preferences "����"
#define MSGTR_OSSPreferences "OSS �ɥ饤������"
#define MSGTR_SDLPreferences "SDL �ɥ饤������"
#define MSGTR_NoMediaOpened "��ǥ�����������Ƥ��ޤ���."
#define MSGTR_VCDTrack "VCD �ȥ�å� %d"
#define MSGTR_NoChapter "����ץ���������ޤ���"
#define MSGTR_Chapter "����ץ��� %d"
#define MSGTR_NoFileLoaded "�ե����뤬�ɤ߹��ޤ�Ƥ��ޤ���."

// --- buttons ---
#define MSGTR_Ok "OK"
#define MSGTR_Cancel "����󥻥�"
#define MSGTR_Add "�ɲ�"
#define MSGTR_Remove "���"
#define MSGTR_Clear "���ꥢ"
#define MSGTR_Config "����"
#define MSGTR_ConfigDriver "�ɥ饤������"
#define MSGTR_Browse "�֥饦��"

// --- error messages ---
#define MSGTR_NEMDB "�����ɬ�פʥХåե�����ݤ��뤿��Υ��꤬­��ޤ���."
#define MSGTR_NEMFMR "��˥塼�������ɬ�פʥ��꤬­��ޤ���."
#define MSGTR_IDFGCVD "Sorry, i did not find a GUI compatible video output driver."
#define MSGTR_NEEDLAVCFAME "Sorry, you cannot play non-MPEG files with your DXR3/H+ device without reencoding.\nPlease enable lavc or fame in the DXR3/H+ configbox."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[skin] ���顼: ����������ե����� %d ��: %s"
#define MSGTR_SKIN_WARNING1 "[skin] �ٹ�: ����������ե����� %d ��: widget found but before \"section\" not found ( %s )"
#define MSGTR_SKIN_WARNING2 "[skin] �ٹ�: ����������ե����� %d ��: widget found but before \"subsection\" not found (%s)"
#define MSGTR_SKIN_WARNING3 "[skin] �ٹ�: ����������ե����� %d ��: this subsection not supported by this widget (%s)"
#define MSGTR_SKIN_BITMAP_16bit  "16 bits or less depth bitmap not supported (%s).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "�ե����뤬¸�ߤ��ޤ��� (%s)\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "bmp �ɤ߹��ߥ��顼 (%s)\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "tga �ɤ߹��ߥ��顼 (%s)\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "png �ɤ߹��ߥ��顼 (%s)\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "RLE packed tga �ϥ��ݡ��Ȥ���Ƥ��ޤ��� (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "̤�ΤΥե����륿���פǤ� (%s)\n"
#define MSGTR_SKIN_BITMAP_ConvertError "24bit����32bit�ؤ��Ѵ����顼 (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "̤�ΤΥ�å�����: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "���꤬��­���Ƥ��ޤ�\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "too many fonts declared\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "�ե���ȥե����뤬¸�ߤ��ޤ���\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "�ե���ȥ��᡼���ե����뤬¸�ߤ��ޤ���\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "non-existent font identifier (%s)\n"
#define MSGTR_SKIN_UnknownParameter "̤�ΤΥѥ�᡼��(%s)\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[skinbrowser] ���꤬��­���Ƥ��ޤ�.\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "������¸�ߤ��ޤ���( %s ).\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "����������ե�������ɤ߹��ߥ��顼(%s).\n"
#define MSGTR_SKIN_LABEL "������:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "MPlayer�ˤĤ���"
#define MSGTR_MENU_Open "���� ..."
#define MSGTR_MENU_PlayFile "�ե�������� ..."
#define MSGTR_MENU_PlayVCD "VCD ���� ..."
#define MSGTR_MENU_PlayDVD "DVD ���� ..."
#define MSGTR_MENU_PlayURL "URL ���� ..."
#define MSGTR_MENU_LoadSubtitle "���֥����ȥ��ɤ߹��� ..."
#define MSGTR_MENU_DropSubtitle "���֥����ȥ��˴� ..."
#define MSGTR_MENU_LoadExternAudioFile "Load external audio file ..."
#define MSGTR_MENU_Playing "���ߤΥե�����"
#define MSGTR_MENU_Play "����"
#define MSGTR_MENU_Pause "������"
#define MSGTR_MENU_Stop "���"
#define MSGTR_MENU_NextStream "���Υ��ȥ꡼��"
#define MSGTR_MENU_PrevStream "���Υ��ȥ꡼��"
#define MSGTR_MENU_Size "������"
#define MSGTR_MENU_NormalSize "�̾掠����"
#define MSGTR_MENU_DoubleSize "2�ܥ�����"
#define MSGTR_MENU_FullScreen "�ե륹���꡼��"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "�ǥ��������� ..."
#define MSGTR_MENU_ShowDVDMenu "DVD ��˥塼��ɽ��"
#define MSGTR_MENU_Titles "�����ȥ�"
#define MSGTR_MENU_Title "�����ȥ� %2d"
#define MSGTR_MENU_None "(none)"
#define MSGTR_MENU_Chapters "Chapters"
#define MSGTR_MENU_Chapter "Chapter %2d"
#define MSGTR_MENU_AudioLanguages "��������"
#define MSGTR_MENU_SubtitleLanguages "���֥����ȥ����"
#define MSGTR_MENU_PlayList "�ץ쥤�ꥹ��"
#define MSGTR_MENU_SkinBrowser "������֥饦��"
#define MSGTR_MENU_Preferences "����"
#define MSGTR_MENU_Exit "��λ ..."
#define MSGTR_MENU_Mute "�ò�"
#define MSGTR_MENU_Original "���ꥸ�ʥ�"
#define MSGTR_MENU_AspectRatio "Aspect ratio"
#define MSGTR_MENU_AudioTrack "�����ȥ�å�"
#define MSGTR_MENU_Track "�ȥ�å� %d"
#define MSGTR_MENU_VideoTrack "�����ȥ�å�"

// --- equalizer
#define MSGTR_EQU_Audio "����"
#define MSGTR_EQU_Video "����"
#define MSGTR_EQU_Contrast "Contrast: "
#define MSGTR_EQU_Brightness "Brightness: "
#define MSGTR_EQU_Hue "Hue: "
#define MSGTR_EQU_Saturation "Saturation: "
#define MSGTR_EQU_Front_Left "���� ��"
#define MSGTR_EQU_Front_Right "���� ��"
#define MSGTR_EQU_Back_Left "���� ��"
#define MSGTR_EQU_Back_Right "���� ��"
#define MSGTR_EQU_Center "���"
#define MSGTR_EQU_Bass "�Х�"
#define MSGTR_EQU_All "All"
#define MSGTR_EQU_Channel1 "�����ͥ� 1:"
#define MSGTR_EQU_Channel2 "�����ͥ� 2:"
#define MSGTR_EQU_Channel3 "�����ͥ� 3:"
#define MSGTR_EQU_Channel4 "�����ͥ� 4:"
#define MSGTR_EQU_Channel5 "�����ͥ� 5:"
#define MSGTR_EQU_Channel6 "�����ͥ� 6:"

// --- playlist
#define MSGTR_PLAYLIST_Path "�ѥ�"
#define MSGTR_PLAYLIST_Selected "���򤵤줿�ե�����"
#define MSGTR_PLAYLIST_Files "�ե�����"
#define MSGTR_PLAYLIST_DirectoryTree "�ǥ��쥯�ȥ�ĥ꡼"

// --- preferences
#define MSGTR_PREFERENCES_Audio "����"
#define MSGTR_PREFERENCES_Video "����"
#define MSGTR_PREFERENCES_SubtitleOSD "���֥����ȥ� & OSD"
#define MSGTR_PREFERENCES_Codecs "�����ǥå� & demuxer"
#define MSGTR_PREFERENCES_Misc "Misc"

#define MSGTR_PREFERENCES_None "None"
#define MSGTR_PREFERENCES_AvailableDrivers "ͭ���ʥɥ饤��:"
#define MSGTR_PREFERENCES_DoNotPlaySound "Do Not Play Sound"
#define MSGTR_PREFERENCES_NormalizeSound "Normalize sound"
#define MSGTR_PREFERENCES_EnEqualizer "�����饤������ͭ��"
#define MSGTR_PREFERENCES_ExtraStereo "Enable extra stereo"
#define MSGTR_PREFERENCES_Coefficient "Coefficient:"
#define MSGTR_PREFERENCES_AudioDelay "Audio delay"
#define MSGTR_PREFERENCES_DoubleBuffer "double buffering ͭ��"
#define MSGTR_PREFERENCES_DirectRender "direct rendering ͭ��"
#define MSGTR_PREFERENCES_FrameDrop "frame dropping ͭ��"
#define MSGTR_PREFERENCES_HFrameDrop "HARD frame dropping (���Ǥ�) ͭ��"
#define MSGTR_PREFERENCES_Flip "Flip image upside down"
#define MSGTR_PREFERENCES_Panscan "Panscan: "
#define MSGTR_PREFERENCES_OSDTimer "Timer and indicators"
#define MSGTR_PREFERENCES_OSDProgress "�ץ��쥹�С�����"
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "Timer, percentage and total time"
#define MSGTR_PREFERENCES_Subtitle "Subtitle:"
#define MSGTR_PREFERENCES_SUB_Delay "Delay: "
#define MSGTR_PREFERENCES_SUB_FPS "FPS:"
#define MSGTR_PREFERENCES_SUB_POS "Position: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "subtitle ��ư�ɤ߹���̵��"
#define MSGTR_PREFERENCES_SUB_Unicode "Unicode subtitle"
#define MSGTR_PREFERENCES_SUB_MPSUB "Convert the given subtitle to MPlayer's subtitle format"
#define MSGTR_PREFERENCES_SUB_SRT "Convert the given subtitle to the time based SubViewer (SRT) format"
#define MSGTR_PREFERENCES_SUB_Overlap "Toggle subtitle overlapping"
#define MSGTR_PREFERENCES_Font "�ե����:"
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
#define MSGTR_PREFERENCES_FRAME_Misc "Misc"
#define MSGTR_PREFERENCES_OSS_Device "Device:"
#define MSGTR_PREFERENCES_OSS_Mixer "Mixer:"
#define MSGTR_PREFERENCES_SDL_Driver "Driver:"
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

#define MSGTR_ABOUT_UHU "GUI development sponsored by UHU Linux\n"
#define MSGTR_ABOUT_CoreTeam "   MPlayer ����������:\n"
#define MSGTR_ABOUT_AdditionalCoders "   Additional coders:\n"
#define MSGTR_ABOUT_MainTesters "   �ᥤ��ƥ�����:\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "��̿Ū���顼!"
#define MSGTR_MSGBOX_LABEL_Error "���顼"
#define MSGTR_MSGBOX_LABEL_Warning "�ٹ�"

#endif

// Translated to Japanese. Encoding: EUC-JP
// Translated by smoker <http://smokerz.net/~smoker/>

// This translation is not complete yet.

// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"�Ȥ���:   mplayer [���ץ����] [url|�ѥ�/]�ե�����̾\n"
"\n"
"����Ū�ʥ��ץ����: (man page ���������夵��Ƥ��ޤ�)\n"
" -vo <drv[:dev]>  �������ϥɥ饤�еڤӥǥХ��������򤷤ޤ� ('-vo help'�ǰ���ɽ������ޤ�)\n"
" -ao <drv[:dev]>  �������ϥɥ饤�еڤӥǥХ��������򤷤ޤ� ('-ao help'�ǰ���ɽ������ޤ�)\n"
#ifdef HAVE_VCD
" vcd://<trackno>   play VCD (Video CD) track from device instead of plain file\n"
#endif
#ifdef USE_DVDREAD
" dvd://<titleno>   play DVD title from device instead of plain file\n"
" -alang/-slang    DVD�� ����/���֥����ȥ� �������� (2ʸ���Υ���ȥ꡼�����ɤǻ���)\n"
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
#define MSGTR_DVDauthOk "DVDǧ�ڽ���������˴�λ���ޤ���\n"
#define MSGTR_CantOpenDumpfile "dump file�򳫤��ޤ���\n"
#define MSGTR_CoreDumped "��������� ;)\n"
#define MSGTR_FPSnotspecified "FPS ���إå��˻��ꤵ��Ƥ��ʤ��������Ǥ�. -fps ���ץ��������Ѥ��Ʋ�����.\n"
#define MSGTR_CantFindAudioCodec "audio format 0x%X �����Υ����ǥå����դ����������ޤ���Ǥ���.\n"
#define MSGTR_RTFMCodecs "DOCS/HTML/en/codecs.html �����������\n"
#define MSGTR_CouldntInitAudioCodec "�������ϥ����ǥå��ν�����˼��Ԥ��ޤ��� -> ̵�����ˤʤ�ޤ�.\n"
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
#define MSGTR_CompiledWithRuntimeDetection "����ѥ������Runtime CPU Detection�����Ѥ���Ƥ��ޤ�������Ϻ�Ŭ�ǤϤ���ޤ���\n��Ŭ�ʥѥե����ޥ�������ˤϡ�--disable-runtime-cpudetection��ͭ���ˤ���MPLayer��ƥ���ѥ��뤷�Ʋ�����\n"
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
#define MSGTR_CannotReadVideoProperties "Video: �ץ�ѥƥ������ɤ߼��ޤ���.\n"
#define MSGTR_NoStreamFound "���ȥ꡼����դ��뤳�Ȥ�����ޤ���Ǥ���.\n"
#define MSGTR_InitializingAudioCodec "���������ǥå���������...\n"
#define MSGTR_ErrorInitializingVODevice "���򤵤줿��������(-vo)�ǥХ����򳫤���������ޤ���Ǥ���.\n"
#define MSGTR_ForcedVideoCodec "���ꤵ�줿���������ǥå�: %s\n"
#define MSGTR_ForcedAudioCodec "���ꤵ�줿���������ǥå�: %s\n"
#define MSGTR_AODescription_AOAuthor "AO: �ܺ�: %s\nAO: ����: %s\n"
#define MSGTR_AOComment "AO: ������: %s\n"
#define MSGTR_Video_NoVideo "Video: ����������ޤ���\n"
#define MSGTR_NotInitializeVOPorVO "\nFATAL: �����ե��륿��(-vf)����������(-vo)�ν�����˼��Ԥ��ޤ���.\n"
#define MSGTR_Paused "\n  =====  ���  =====\r" // no more than 23 characters (status line for audio files)
#define MSGTR_PlaylistLoadUnable "\n�ץ쥤�ꥹ�Ȥ��ɤ߹��ߤ�����ޤ��� %s.\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- MPLayer��������̿��(Illegal Instruction)�ˤ�ꥯ��å��夷�ޤ���\n"\
"  ���餯����� �����餷��CPU-Detection code�˥Х�������ޤ�\n"\
"  DOCS/HTML/en/bugreports.html ���ɤ߲�����.\n"
#define MSGTR_Exit_SIGILL \
"- MPLayer��������̿��(Illegal Instruction)�ˤ�ꥯ��å��夷�ޤ���\n"\
"  It usually happens when you run it on a CPU different than the one it was\n"\
"  compiled/optimized for.\n"\
"  Verify this!\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- Mplayer�����ɤ� CPU/FPU/RAM �ˤ�äƥ���å��夷�ޤ���.\n"\
"  Recompile MPlayer with --enable-debug and make a 'gdb' backtrace and\n"\
"  --enable-debug��Ĥ���MPlyaer�򥳥�ѥ��뤷�ʤ�����gdb��Ĵ�����ޤ��礦\n"\
"  �ܺ٤� DOCS/HTML/en/bugreports.html#bugreports_crash �ˤ���ޤ�\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer �����ꤵ��Ƥ��ʤ�����å���򵯤����ޤ���.\n"\
"  It can be a bug in the MPlayer code _or_ in your drivers _or_ in your\n"\
"  gcc version. If you think it's MPlayer's fault, please read\n"\
"  DOCS/HTML/en/bugreports.html and follow the instructions there. We can't and\n"\
"  won't help unless you provide this information when reporting a possible bug.\n"


// mencoder.c:

#define MSGTR_CannotOpenFile_Device "�ե�����ڤӥǥХ����������ޤ���.\n"
#define MSGTR_ErrorDVDAuth "DVDǧ�ڤ˼��Ԥ��ޤ���.\n"
#define MSGTR_CannotOpenDemuxer "demuxer�򳫤����Ȥ�����ޤ���.\n"
#define MSGTR_NoAudioEncoderSelected "\n�������󥳡���(-oac)�����ꤵ��Ƥ��ޤ��� �������ꤹ�뤫��̵����(-nosound)��Ϳ���Ʋ��������ܺ٤� '-oac help'\n"
#define MSGTR_NoVideoEncoderSelected "\n�������󥳡���(-ovc)�����ꤵ��Ƥ��ޤ��� �������ꤷ�Ʋ������� �ܺ٤� '-ovc help'\n"
#define MSGTR_CannotOpenOutputFile "���ϥե�����'%s'�򳫤���������ޤ���.\n"
#define MSGTR_EncoderOpenFailed "���󥳡����򳫤����Ȥ˼��Ԥ��ޤ���.\n"
#define MSGTR_ForcingOutputFourcc "fourcc�� %x [%.4s] �˻��ꤷ�ޤ�\n"
#define MSGTR_WritingAVIHeader "AVI�إå���񤭤�����...\n"
#define MSGTR_DuplicateFrames "\n%d ��ʣ�����ե졼��\n"
#define MSGTR_SkipFrame "\n�ե졼��򥹥��åפ��Ƥ��ޤ�\n"
#define MSGTR_ErrorWritingFile "%s: �ե�����񤭹��ߥ��顼.\n"
#define MSGTR_WritingAVIIndex "\nAVI index��񤭹�����...\n"
#define MSGTR_FixupAVIHeader "AVI�إå�������...\n"
#define MSGTR_VideoStreamResult "\n�������ȥ꡼��: %8.3f kbit/s  (%d bps)  ������: %d bytes  %5.3f secs  %d �ե졼��\n"
#define MSGTR_AudioStreamResult "\n�������ȥ꡼��: %8.3f kbit/s  (%d bps)  ������: %d bytes  %5.3f secs\n"

// cfg-mencoder.h:

// open.c, stream.c:
#define MSGTR_CdDevNotfound "CD-ROM �ǥХ��� '%s' ��¸�ߤ��ޤ���.\n"
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
#define MSGTR_TooManyAudioInBuffer "\n�Хåե���¿�᤮�벻���ѥ��åȤ�Ϳ�����Ƥޤ�: (%d in %d bytes).\n"
#define MSGTR_TooManyVideoInBuffer "\n�Хåե���¿�᤮������ѥ��åȤ�Ϳ�����Ƥޤ�: (%d in %d bytes).\n"
#define MSGTR_Detected_XXX_FileFormat "%s �ե�����ե����ޥåȤ�Ƚ��.\n"
#define MSGTR_DetectedAudiofile "�����ե������Ƚ��.\n"
#define MSGTR_FormatNotRecognized "============ ���Υե�����ե����ޥåȤ� ���ݡ��Ȥ��Ƥ��ޤ��� =============\n"\
				  "======= �⤷���Υե����뤬 AVI��ASF��MPEG�ʤ�����Ԥ�Ϣ���Ʋ����� ======\n"
#define MSGTR_MissingVideoStream "�������ȥ꡼�ब¸�ߤ��ޤ���.\n"
#define MSGTR_MissingAudioStream "�������ȥ꡼�ब¸�ߤ��ޤ��� -> ̵�����ˤʤ�ޤ�\n"
#define MSGTR_MissingVideoStreamBug "Missing video stream!? �����Ԥ�Ϣ���Ʋ����������餯����ϥХ��Ǥ� :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: ���򤵤줿 �������������Ǽ���뤳�Ȥ�����ޤ���.\n"

#define MSGTR_NI_Forced "Forced"
#define MSGTR_NI_Detected "Detected"
#define MSGTR_NI_Message "%s NON-INTERLEAVED AVI �ե����� �ե����ޥå�.\n"

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
#define MSGTR_DemuxerInfoAlreadyPresent "Demuxer ���� %s already present!\n"
#define MSGTR_ClipInfo "����å׾���:\n"

#define MSGTR_LeaveTelecineMode "\ndemux_mpg: 30fps NTSC ����ƥ�ĸ���, �ե졼��졼���ѹ���.\n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: 24fps �ץ���å��� NTSC ����ƥ�ĸ���, �ե졼��졼���ѹ���.\n"


// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "�����ǥå��򳫤����Ȥ�����ޤ���.\n"
#define MSGTR_CantCloseCodec "�����ǥå����Ĥ��뤳�Ȥ�����ޤ���.\n"

#define MSGTR_MissingDLLcodec "���顼: �׵ᤵ�줿 DirectShow �����ǥå� %s �򳫤����Ȥ�����ޤ���.\n"
#define MSGTR_ACMiniterror "Win32/ACM���������ǥå����ɤ߹��ߵڤӽ�����򤹤뤳�Ȥ�����ޤ��� (DLL�ե����������פǤ���?).\n"
#define MSGTR_MissingLAVCcodec "'%s' �� libavcodec���鸫�դ��뤳�Ȥ�����ޤ��� ...\n"

#define MSGTR_CannotReadMpegSequHdr "FATAL: �������󥹥إå�(sequence header)���ɤ߹���ޤ���.\n"
#define MSGTR_CannotReadMpegSequHdrEx "FATAL: ��ĥ�������󥹥إå�(sequence header extension)���ɤ߹���ޤ���.\n"
#define MSGTR_BadMpegSequHdr "MPEG: �����ʥ������󥹥إå�(sequence header)\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: �����ʳ�ĥ�������󥹥إå�(sequence header)\n"

#define MSGTR_ShMemAllocFail "��ͭ����γ��ݤ˼���\n"
#define MSGTR_CantAllocAudioBuf "�������ϥХåե����γ��ݤ˼���\n"

#define MSGTR_UnknownAudio "̤�ΤΡ��⤷���ϲ��줿�����ե����ޥåȤǤ� -> ̵�����ˤʤ�ޤ�\n"

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
#define MSGTR_LIRCdisabled "��⡼�ȥ���ȥ������Ѥ��뤳�ȤϽ���ޤ���.\n"
#define MSGTR_LIRCopenfailed "LIRC ���ݡ��Ȥ򳫤����˼���.\n"
#define MSGTR_LIRCcfgerr "LIRC ����ե����� %s �򳫤����Ȥ˼��Ԥ��ޤ���.\n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "�����ե��륿 '%s' �����դ���ޤ���\n"
#define MSGTR_CouldNotOpenVideoFilter "�����ե��륿 '%s' �����դ���ޤ���\n"
#define MSGTR_OpeningVideoFilter "�����ե��륿�򳫤��Ƥ��ޤ�: "
#define MSGTR_CannotFindColorspace "common colorspace�����դ���ޤ���, even by inserting 'scale' :(\n"

// vd.c
#define MSGTR_VoConfigRequest "VDec: ������������ - %d x %d (preferred csp: %s)\n"
#define MSGTR_CouldNotFindColorspace "���פ��륫�顼���ڡ��������դ���ޤ��� - '-vop'��Ĥ��ƻ�ߤƲ�����...\n"

// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "���Х���"
#define MSGTR_FileSelect "�ե��������� ..."
#define MSGTR_SubtitleSelect "���֥����ȥ����� ..."
#define MSGTR_OtherSelect "���� ..."
#define MSGTR_FontSelect "�ե�������� ..."
#define MSGTR_PlayList "�ץ쥤�ꥹ��"
#define MSGTR_Equalizer "�����饤����"
#define MSGTR_SkinBrowser "������֥饦��"
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

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[skin] ���顼: ����������ե����� %d ��: %s"
#define MSGTR_SKIN_WARNING1 "[skin] �ٹ�: ����������ե����� %d ��: widget found but before \"section\" not found ( %s )"
#define MSGTR_SKIN_WARNING2 "[skin] �ٹ�: ����������ե����� %d ��: widget found but before \"subsection\" not found (%s)"
#define MSGTR_SKIN_WARNING3 "[skin] �ٹ�: ����������ե����� %d ��: this subsection not supported by this widget (%s)"
#define MSGTR_SKIN_BITMAP_16bit  "16 �ӥåȰʲ��β���٤ϥ��ݡ��Ȥ���Ƥ��ޤ��� (%s).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "�ե����뤬¸�ߤ��ޤ��� (%s)\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "bmp �ɤ߹��ߥ��顼 (%s)\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "tga �ɤ߹��ߥ��顼 (%s)\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "png �ɤ߹��ߥ��顼 (%s)\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "RLE packed tga �ϥ��ݡ��Ȥ���Ƥ��ޤ��� (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "̤�ΤΥե����륿���פǤ� (%s)\n"
#define MSGTR_SKIN_BITMAP_ConvertError "24bit����32bit�ؤ��Ѵ����顼 (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "̤�ΤΥ�å�����: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "���꤬��­���Ƥ��ޤ�\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "�ե���ȥե����뤬¸�ߤ��ޤ���\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "�ե���ȥ��᡼���ե����뤬¸�ߤ��ޤ���\n"
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
#define MSGTR_MENU_None "(̵��)"
#define MSGTR_MENU_AudioLanguages "��������"
#define MSGTR_MENU_SubtitleLanguages "���֥����ȥ����"
#define MSGTR_MENU_PlayList "�ץ쥤�ꥹ��"
#define MSGTR_MENU_SkinBrowser "������֥饦��"
#define MSGTR_MENU_Preferences "����"
#define MSGTR_MENU_Exit "��λ ..."
#define MSGTR_MENU_Mute "�ò�"
#define MSGTR_MENU_Original "���ꥸ�ʥ�"
#define MSGTR_MENU_Track "�ȥ�å� %d"
#define MSGTR_MENU_VideoTrack "�����ȥ�å�"

// --- equalizer
#define MSGTR_EQU_Audio "����"
#define MSGTR_EQU_Video "����"
#define MSGTR_EQU_Contrast "����: "
#define MSGTR_EQU_Brightness "����: "
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

#define MSGTR_PREFERENCES_None "̵��"
#define MSGTR_PREFERENCES_AvailableDrivers "ͭ���ʥɥ饤��:"
#define MSGTR_PREFERENCES_EnEqualizer "�����饤������ͭ��"
#define MSGTR_PREFERENCES_DoubleBuffer "double buffering ͭ��"
#define MSGTR_PREFERENCES_DirectRender "direct rendering ͭ��"
#define MSGTR_PREFERENCES_FrameDrop "frame dropping ͭ��"
#define MSGTR_PREFERENCES_HFrameDrop "HARD frame dropping (���Ǥ�) ͭ��"
#define MSGTR_PREFERENCES_OSDTimer "�����ޡ��ȥ���ǥ���������"
#define MSGTR_PREFERENCES_OSDProgress "�ץ��쥹�С�����"
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "�����ޡ� �ѡ�����ơ����ȥȡ��������"
#define MSGTR_PREFERENCES_Subtitle "���֥����ȥ�:"
#define MSGTR_PREFERENCES_SUB_Delay "Delay: "
#define MSGTR_PREFERENCES_SUB_FPS "FPS:"
#define MSGTR_PREFERENCES_SUB_POS "����: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "subtitle ��ư�ɤ߹���̵��"
#define MSGTR_PREFERENCES_SUB_Unicode "Unicode ���֥����ȥ�"
#define MSGTR_PREFERENCES_SUB_MPSUB "Ϳ����줿���֥����ȥ��MPLayer�Υ��֥����ȥ�ե����ޥåȤ��Ѵ�"
#define MSGTR_PREFERENCES_Font "�ե����:"
#define MSGTR_PREFERENCES_PostProcess "postprocessing ͭ��"
#define MSGTR_PREFERENCES_NI "non-interleaved AVI �ѡ�������"
#define MSGTR_PREFERENCES_IDX "ɬ�פʤ�index table�κƹ���"
#define MSGTR_PREFERENCES_VideoCodecFamily "���������ǥå�:"
#define MSGTR_PREFERENCES_AudioCodecFamily "���������ǥå�:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "OSD ��٥�"
#define MSGTR_PREFERENCES_FRAME_Subtitle "���֥����ȥ�"
#define MSGTR_PREFERENCES_FRAME_Font "�ե����"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "�����ǥå�& demuxer"
#define MSGTR_PREFERENCES_FRAME_Cache "����å���"
#define MSGTR_PREFERENCES_FRAME_Misc "Misc"
#define MSGTR_PREFERENCES_OSS_Device "�ǥХ���:"
#define MSGTR_PREFERENCES_OSS_Mixer "Mixer:"
#define MSGTR_PREFERENCES_SDL_Driver "�ɥ饤�С�:"
#define MSGTR_PREFERENCES_DXR3_VENC "Video ���󥳡���:"
#define MSGTR_PREFERENCES_DXR3_LAVC "LAVC (FFmpeg)����"
#define MSGTR_PREFERENCES_DXR3_FAME "FAME ����"
#define MSGTR_PREFERENCES_FontEncoding1 "��˥�����"
#define MSGTR_PREFERENCES_FontEncoding "���󥳡��ǥ���:"
#define MSGTR_PREFERENCES_Cache "����å��� on/off"
#define MSGTR_PREFERENCES_CacheSize "����å��奵����: "
#define MSGTR_PREFERENCES_LoadFullscreen "���ϻ��˥ե륹���꡼��"
#define MSGTR_PREFERENCES_SaveWinPos "������ɥ����֤��ݻ�"
#define MSGTR_PREFERENCES_XSCREENSAVER "XScreenSaver�򥹥ȥå�"
#define MSGTR_PREFERENCES_PlayBar "�ץ쥤�С�ͭ��"
#define MSGTR_PREFERENCES_CDROMDevice "CD-ROM �ǥХ���:"
#define MSGTR_PREFERENCES_DVDDevice "DVD �ǥХ���:"
#define MSGTR_PREFERENCES_FPS "Movie FPS:"

#define MSGTR_ABOUT_CoreTeam "   MPlayer ����������:\n"
#define MSGTR_ABOUT_MainTesters "   �ᥤ��ƥ�����:\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "��̿Ū���顼!"
#define MSGTR_MSGBOX_LABEL_Error "���顼"
#define MSGTR_MSGBOX_LABEL_Warning "�ٹ�"

#endif

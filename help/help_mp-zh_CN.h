// Translated by Lu Ran <hephooey@fastmail.fm>
// Synced with help_mp-en.h 1.100

// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char* banner_text=
"\n\n"
"MPlayer " VERSION "(C) 2000-2003 Arpad Gereoffy (��DOCS!)\n"
"\n";

static char help_text[]=
"�÷�:   mplayer [options] [url|path/]filename\n"
"\n"
"����ѡ��: (��������ѡ�����ϸ�б�μ�manpage!)\n"
" -vo <drv[:dev]> ѡ����Ƶ���ģʽ���豸(��'-vo help'�鿴�б�)\n"
" -ao <drv[:dev]> ѡ����Ƶ���ģʽ���豸(��'-ao help'�鿴�б�)\n"
#ifdef HAVE_VCD
" -vcd <trackno>  ���豸��������ͨ�ļ��ϲ���VCD(video cd)track\n"
#endif
#ifdef HAVE_LIBCSS
" -dvdauth <dev>  ΪDVD�豸������Ȩ��(���ڼ��ܹ���)\n"
#endif
#ifdef USE_DVDREAD
" -dvd <titleno>  ���豸��������ͨ�ļ��ϲ���DVD title/track\n"
" -alang/-slang   ѡ��DVD����/��Ļ������(ʹ����λ�Ĺ��Ҵ���)\n"
#endif
" -ss <timepos>   Ѱ��ָ����(�������hh:mm:ss)λ��\n"
" -nosound        ����������\n"
" -fs -vm -zoom   ȫ������ѡ��(fullscr,vidmode chg,softw.scale)\n"
" -x <x> -y <y>   ���ò��ŵķֱ���(���ڸı�vidmode���������)\n"
" -sub <file>     ָ��ʹ�õ���Ļ�ļ�(�μ�-subfps, -subdelay)\n"
" -playlist <file> ָ��ʹ�ò����б��ļ�\n"
" -vid x -aid y   ѡ�����ڲ��ŵ���Ƶ(x)����Ƶ(y)��\n"
" -fps x -srate y �ı���Ƶ(x fps)����Ƶ(y Hz)��\n"
" -pp <quality>   ʹ�ú��ڴ����˾�(��ϸ���ݲμ�manpage/docs)\n"
" -framedrop      ʹ�� frame-dropping (����������)\n"
"\n"
"�������Ƽ�: (�������б�μ�manpage, ͬʱҲҪ���һ�� input.conf)\n"
" <-  or  ->      ���/��ǰ����10��\n"
" up or down      ���/��ǰ����1����\n"
" pgup or pgdown  ���/��ǰ����10����\n"
" < or >          ���������б��е�ǰһ��/��һ��\n"
" p or SPACE      ��ͣ����(�����������)\n"
" q or ESC        ֹͣ���Ų��Ƴ�\n"
" + or -          ������Ƶ�ӳ�+/-0.1��\n"
" o               ѭ��OSDģʽ:  none/seekbar/seekbar+timer\n"
" * or /          ���ӻ����pcm����\n"
" z or x          ������Ļ�ӳ�+/-0.1��\n"
" r or t          ��/�µ�����Ļλ��, �μ�-vop expand !\n"
"\n"
" * * * ��ϸ���ݣ���һ��(�߼�)��ѡ��Ϳ��Ƽ��μ�MANPAGE��* * *\n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// mplayer.c:

#define MSGTR_Exiting "\n�����˳�... (%s)\n"
#define MSGTR_Exit_quit "�˳�"
#define MSGTR_Exit_eof "�ļ�����"
#define MSGTR_Exit_error "��������"
#define MSGTR_IntBySignal "\nMPlayer�� %s ģ���е� %d �ź��ж�\n"
#define MSGTR_NoHomeDir "�Ҳ���HOMEĿ¼\n"
#define MSGTR_GetpathProblem "get_path(\"config\")����\n"
#define MSGTR_CreatingCfgFile "����config�ļ�: %s\n"
#define MSGTR_InvalidVOdriver "��Ч����Ƶ�������: %s\n��'-vo help' t�鿴���õ���Ƶ�������б�"
#define MSGTR_InvalidAOdriver "��Ч����Ƶ�������: %s\n��'-vo help' t�鿴���õ���Ƶ�������б�"
#define MSGTR_CopyCodecsConf "(��etc/codecs.conf(��MPlayer��Դ������)����/���� ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "ʹ���ڽ�Ĭ�ϵ�codecs.conf\n"
#define MSGTR_CantLoadFont "�޷���������: %s\n"
#define MSGTR_CantLoadSub "�޷�������Ļ: %s\n"
#define MSGTR_ErrorDVDkey "DVD KEY�������.\n"
#define MSGTR_CmdlineDVDkey "DVD������Ҫ���key����������������.\n"
#define MSGTR_DVDauthOk "DVD��Ȩ�����ƺ�OK.\n"
#define MSGTR_DumpSelectedStreamMissing "dump: ��������: ָ������������!\n"
#define MSGTR_CantOpenDumpfile "�޷���dump�ļ�.\n"
#define MSGTR_CoreDumped "core dumped :)\n"
#define MSGTR_FPSnotspecified "FPS���ļ�ͷ��û��ָ��(��������Ч����)! ��-fpsѡ��!\n"
#define MSGTR_TryForceAudioFmtStr "����ָ����Ƶ������������ %s ...\n"
#define MSGTR_CantFindAfmtFallback "�Ҳ���ָ�����������Ƶ������, ֻ��ʹ����������.\n"
#define MSGTR_CantFindAudioCodec "�Ҳ�����Ƶ��ʽ 0x%X �Ľ�����.\n"
#define MSGTR_TryUpgradeCodecsConfOrRTFM "*** ������ etc/codecs.conf ���� %s \n*** �������������, �鿴DOCS/codecs.html!\n"
#define MSGTR_CouldntInitAudioCodec "�޷���ʼ����Ƶ������! -> nosound\n"
#define MSGTR_TryForceVideoFmtStr "����ָ����Ƶ������������ %s ...\n"
#define MSGTR_CantFindVideoCodec "�Ҳ����ʺ���ѡ��-vo����Ƶ��ʽ 0x%X �Ľ�����!\n"
#define MSGTR_VOincompCodec "��Ǹ, ��ѡ����Ƶ����豸�����������������.\n"
#define MSGTR_CannotInitVO "��������: �޷���ʼ����Ƶ����!\n"
#define MSGTR_CannotInitAO "�޷���/��ʼ����Ƶ�豸 -> NOSOUND\n"
#define MSGTR_StartPlaying "��ʼ����...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"         ************************************************\n"\
"         ****       ���ϵͳ̫���ˣ��Ų��������     ****\n"\
"         ************************************************\n"\
" ���ܵ�ԭ�����⣬����취��\n"\
"- ���ձ��ԭ���𻵵Ļ���bug��_��Ƶ_����\n"\
"  - ����-ao sdl��ʹ�� ALSA 0.5��ALSA 0.9��ossģ�⡣\n"\
"  - ���Բ�ͬ��-autosync��ֵ��������30��ʼ��\n"\
"- ��Ƶ���̫��\n"\
"  - ���Բ�ͬ��-vo driver(-vo help���б�)��������-framedrop��\n"\
"- cpu̫��\n"\
"  - ��Ҫ��ͼ������cpu�ϲ��Ŵ��dvd/divx! ����-hardframedrop��\n"\
"- �𻵵��ļ�\n"\
"  - ��������ѡ��Ĳ�ͬ��ϣ�-nobps  -ni  -mc 0  -forceidx\n"\
"- Slow media (NFS/SMB mounts, DVD, VCD etc)\n"\
"  - ���� -cache 8192��\n"\
"- ��ʹ��-cacheѡ���һ���ǽ����avi�ļ���\n"\
"  - ����-nocache\n"\
"�Ķ�DOCS/video.html��DOCS/sound.html��Ѱ�ҵ���/���ٵļ��ɡ�\n"\
"�����Щһ�����ò��ϣ��Ķ�DOCS/bugreports.html��\n\n"

#define MSGTR_NoGui "Mplayerû�б���GUI��֧��!\n"
#define MSGTR_GuiNeedsX "MPlayer GUI��ҪX11!\n"
#define MSGTR_Playing "���� %s\n"
#define MSGTR_NoSound "��Ƶ: no sound\n"
#define MSGTR_FPSforced "FPSָ��Ϊ %5.3f  (ftime: %5.3f)\n"
#define MSGTR_CompiledWithRuntimeDetection "����ʵʱCPU��� - ����, �ⲻ�����ѡ��! �����������ѵı���, ����--disable-runtime-cpudetectionѡ�����±���mplayer\n"
#define MSGTR_CompiledWithCPUExtensions "�������չָ�x86 CPU����:"
#define MSGTR_AvailableVideoOutputPlugins "���õ���Ƶ������:\n"
#define MSGTR_AvailableVideoOutputDrivers "���õ���Ƶ�������:\n"
#define MSGTR_AvailableAudioOutputDrivers "���õ���Ƶ�������:\n"
#define MSGTR_AvailableAudioCodecs "���õ���Ƶ������:\n"
#define MSGTR_AvailableVideoCodecs "���õ���Ƶ������:\n"
#define MSGTR_AvailableAudioFm "\n���õ�(�����˵�)��Ƶ��������/����:\n"
#define MSGTR_AvailableVideoFm "\n���õ�(�����˵�)��Ƶ��������/����:\n"
#define MSGTR_AvailableFsType "���õ�ȫ��ʵ��ģʽ:\n"
#define MSGTR_UsingRTCTiming "ʹ��Linux��Ӳ��RTC��ʱ(%ldHz)\n"
#define MSGTR_CannotReadVideoProperties "��Ƶ: �޷���ȡ����\n"
#define MSGTR_NoStreamFound "�Ҳ�����ý��\n"
#define MSGTR_InitializingAudioCodec "��ʼ����Ƶ������...\n"
#define MSGTR_ErrorInitializingVODevice "��/��ʼ����ѡ����Ƶ���(-vo)�豸�ǳ���!\n"
#define MSGTR_ForcedVideoCodec "ָ������Ƶ������: %s\n"
#define MSGTR_ForcedAudioCodec "ָ������Ƶ������: %s\n"
#define MSGTR_AODescription_AOAuthor "AO: ����: %s\nAO: ����: %s\n"
#define MSGTR_AOComment "AO: ��ע: %s\n"
#define MSGTR_Video_NoVideo "��Ƶ: no video\n"
#define MSGTR_NotInitializeVOPorVO "\n��������: �޷���ʼ����Ƶ���(-vop)����Ƶ���(-vo) !\n"
#define MSGTR_Paused "\n================= ��ͣ =================\r"
#define MSGTR_PlaylistLoadUnable "\n�޷�װ�ز����б� %s\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- ���Ƿ�ָ�����MPlayer������\n"\
"  ������������µ�����ʱCPU�������һ��bug...\n"\
"  ���Ķ�DOCS/bugreports.html\n"
#define MSGTR_Exit_SIGILL \
"- ���Ƿ�ָ�����MPlayer������\n"\
"  ��ͨ�����������������/�Ż�MPlayer��ͬ��CPU��ʹ��\n"\
"  MPlayer��ɵ�\n ���һ��!\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- ����ʹ��CPU/FPU/RAM����MPlayer����.\n"\
"  ʹ��--enable-debug���±���MPlayer�á�gdb��backtrace��\n"\
"  ����ࡣ����ϸ�ڿ�DOCS/bugreports.html#crash\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer�����ˡ��ⲻӦ�÷�����\n"\
"  �������MPlayer�����е� _����_ ��������е� _or_ ���gcc��\n"\
"  һ��bug��������������MPlayer�Ĵ����Ķ�DOCS/bugreports.html\n"\
"  ����ѭ����Ĳ��衣���ǲ���Ҳ���������������ڱ���һ������bug��ʱ��\n"\
"  �ṩ����Ҫ����Ϣ��\n"


// mencoder.c:

#define MSGTR_MEncoderCopyright "(C) 2000-2003 Arpad Gereoffy (��DOCS!)\n"
#define MSGTR_UsingPass3ControllFile "ʹ��pass3�����ļ�: %s\n"
#define MSGTR_MissingFilename "\nû���ļ���!\n\n"
#define MSGTR_CannotOpenFile_Device "�޷����ļ�/�豸\n"
#define MSGTR_ErrorDVDAuth "DVD��Ȩ����...\n"
#define MSGTR_CannotOpenDemuxer "�޷���demuxer\n"
#define MSGTR_NoAudioEncoderSelected "\nû��ѡ����Ƶ������(-oac)! ѡ��һ������ʹ��-nosound. ʹ��-oac help !\n"
#define MSGTR_NoVideoEncoderSelected "\nNo video encoder (-ovc) selected! Select one, use -ovc help !\n"
#define MSGTR_InitializingAudioCodec "��ʼ����Ƶ������...\n"
#define MSGTR_CannotOpenOutputFile "�޷�������ļ� '%s'\n"
#define MSGTR_EncoderOpenFailed "�޷��򿪱�����\n"
#define MSGTR_ForcingOutputFourcc "ָ�������fourccΪ %x [%.4s]\n"
#define MSGTR_WritingAVIHeader "����дAVI�ļ�ͷ...\n"
#define MSGTR_DuplicateFrames "\n�Ѹ��� %d ֡!\n"
#define MSGTR_SkipFrame "\n������һ֡!\n"
#define MSGTR_ErrorWritingFile "%s: д���ļ�����.\n"
#define MSGTR_WritingAVIIndex "\n����дAVI����...\n"
#define MSGTR_FixupAVIHeader "�޸�AVI�ļ�ͷ...\n"
#define MSGTR_RecommendedVideoBitrate "%s CD�Ƽ�����Ƶ������Ϊ: %d\n"
#define MSGTR_VideoStreamResult "\n��Ƶ��: %8.3f kbit/s  (%d bps)  ��С: %d bytes  %5.3f secs  %d frames\n"
#define MSGTR_AudioStreamResult "\n��Ƶ��: %8.3f kbit/s  (%d bps)  ��С: %d bytes  %5.3f secs\n"
// cfg-mencoder.h:

#define MSGTR_MEncoderMP3LameHelp "\n\n"\
" vbr=<0-4>     ������ʷ�ʽ\n"\
"                0: cbr\n"\
"                1: mt\n"\
"                2: rh(Ĭ��)\n"\
"                3: abr\n"\
"                4: mtrh\n"\
"\n"\
" abr           ƽ��������\n"\
"\n"\
" cbr           ��������\n"\
"               ���ں��ABRԤ��ģʽ��ǿ��ʹ��CBRģʽ\n"\
"\n"\
" br=<0-1024>   ��kBitΪ��λ���ñ����� (������CBR��ABR)\n"\
"\n"\
" q=<0-9>       �������� (0-���, 9-���) (������VBR)\n"\
"\n"\
" aq=<0-9>      �㷨���� (0-���/����, 9-���/���)\n"\
"\n"\
" ratio=<1-100> ѹ����\n"\
"\n"\
" vol=<0-10>    ������Ƶ��������\n"\
"\n"\
" mode=<0-3>    (Ĭ��: �Զ�)\n"\
"                0: ������\n"\
"                1: ����������\n"\
"                2: ˫����\n"\
"                3: ������\n"\
"\n"\
" padding=<0-2>\n"\
"                0: ��\n"\
"                1: ����\n"\
"                2: ����\n"\
"\n"\
" fast          ��������ĺ��VBRԤ��ģʽ���룬\n"\
"               ��΢������������߱����ʡ�\n"\
"\n"\
" preset=<value> �ṩ��ߵĿ��ܵ��������á�\n"\
"                 medium: VBR���룬��������\n"\
"                 (150-180 kbps�����ʷ�Χ)\n"\
"                 standard:  VBR����, ��������\n"\
"                 (170-210 kbps�����ʷ�Χ)\n"\
"                 extreme: VBR���룬�������ǳ���\n"\
"                 (200-240 kbps�����ʷ�Χ)\n"\
"                 insane:  CBR���룬���������\n"\
"                 (320 kbps bitrate)\n"\
"                 <8-320>: ������������Ϊƽ�������ʵ�ABR���롣\n\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "�Ҳ���CD-ROM�豸 '%s' !\n"
#define MSGTR_ErrTrackSelect "ѡ��VCD track����!"
#define MSGTR_ReadSTDIN "��stdin��ȡ...\n"
#define MSGTR_UnableOpenURL "�޷���URL: %s\n"
#define MSGTR_ConnToServer "���ӵ�������: %s\n"
#define MSGTR_FileNotFound "�Ҳ����ļ�: '%s'\n"

#define MSGTR_SMBInitError "�޷���ʼ��libsmbclient��: %d\n"
#define MSGTR_SMBFileNotFound "�޷��򿪾������ڵ�: '%s'\n"
#define MSGTR_SMBNotCompiled "MPlayerû�б���SMB��ȡ��֧��\n"

#define MSGTR_CantOpenDVD "�޷���DVD �豸: %s\n"
#define MSGTR_DVDwait "��ȡ���̽ṹ, ��ȴ�...\n"
#define MSGTR_DVDnumTitles "����DVD�� %d ��titles.\n"
#define MSGTR_DVDinvalidTitle "��Ч��DVD title��: %d\n"
#define MSGTR_DVDnumChapters "��� DVD title�� %d chapters.\n"
#define MSGTR_DVDinvalidChapter "I��Ч��DVD chapter��: %d\n"
#define MSGTR_DVDnumAngles "��� DVD title�� %d ���ӽ�.\n"
#define MSGTR_DVDinvalidAngle "��Ч��DVD�ӽǺ�: %d\n"
#define MSGTR_DVDnoIFO "�޷��� DVD title: %d ��IFO�ļ�.\n"
#define MSGTR_DVDnoVOBs "�޷���title��VOB(VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD�ɹ���!\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "����! ��Ƶ��ͷ�� %d �����¶���.\n"
#define MSGTR_VideoStreamRedefined "����! ��Ƶ��ͷ�� %d �����¶���.\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: buffer����Ƶ��̫��(%d in %d bytes) !\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: buffer����Ƶ��̫��(%d in %d bytes) !\n"
#define MSGTR_MaybeNI "(Ҳ���㲥����һ���ǽ������/�ļ������ǽ���ʧ��)?\n" \
		      "����AVI�ļ�, ������-niѡ��ָ���ǽ���ģʽ.\n"
#define MSGTR_SwitchToNi "\n��⵽���Ľ����ʽ��AVI - �л���-niģʽ...\n"
#define MSGTR_Detected_XXX_FileFormat "��⵽%s�ļ���ʽ��\n"
#define MSGTR_DetectedAudiofile "��⵽��Ƶ�ļ�!\n"
#define MSGTR_NotSystemStream "��MPEGϵͳ������ʽ... (������������?)\n"
#define MSGTR_MissingMpegVideo "�Ҳ���MPEG��Ƶ��!? ��ϵ����, ������Ǹ�bug :(\n"
#define MSGTR_InvalidMPEGES "��Ч��MPEG-ES��??? ��ϵ����, ������Ǹ�bug :(\n"
#define MSGTR_FormatNotRecognized "============= ��Ǹ, �����ļ���ʽ�޷����ϻ�֧�� ===============\n"\
				  "=== �������ļ���һ��AVI, ASF��MPEG��, ����ϵ����! ===\n"
#define MSGTR_MissingVideoStream "�Ҳ�����Ƶ��. \n"
#define MSGTR_MissingAudioStream "�Ҳ�����Ƶ��...  ->nosound\n"
#define MSGTR_MissingVideoStreamBug "û����Ƶ��!? ��ϵ����, ������Ǹ�bug :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: �ļ���û����ѡ�����Ƶ����Ƶ�� \n"

#define MSGTR_NI_Forced "ǿ��ָ��"
#define MSGTR_NI_Detected "��⵽"
#define MSGTR_NI_Message "%s �ǽ���AVI�ļ�ģʽ!\n"

#define MSGTR_UsingNINI "ʹ�÷ǽ�����𻵵�AVI�ļ���ʽ!\n"
#define MSGTR_CouldntDetFNo "�޷�����֡��(���ھ�������)  \n"
#define MSGTR_CantSeekRawAVI "�޷��ڲ�������.AVI��������. (��Ҫ����, ����ʹ��-idx ѡ��!)  \n"
#define MSGTR_CantSeekFile "�޷�������ļ�������.  \n"

#define MSGTR_EncryptedVOB "���ܵ�VOB�ļ�(û�б���libcss֧��)! �Ķ�DOCS/cd-dvd.html.\n"
#define MSGTR_EncryptedVOBauth "����������û��Ҫ��ʹ����Ȩ!\n"

#define MSGTR_MOVcomprhdr "MOV: ѹ�����ļ�ͷ(Ŀǰ)��֧��.\n"
#define MSGTR_MOVvariableFourCC "MOV: ����! ��⵽�ɱ��FOURCC!?\n"
#define MSGTR_MOVtooManyTrk "MOV: ����! ̫����."
#define MSGTR_FoundAudioStream "==> �ҵ���Ƶ��: %d\n"
#define MSGTR_FoundVideoStream "==> �ҵ���Ƶ��: %d\n"
#define MSGTR_DetectedTV "��⵽TV! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "�޷���ogg demuxer\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: Ѱ����Ƶ��(id:%d)\n"
#define MSGTR_CannotOpenAudioStream "�޷�����Ƶ��: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "�޷�����Ļ��: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "����Ƶdemuxer: %sʧ��\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "����Ļdemuxer: %sʧ��\n"
#define MSGTR_TVInputNotSeekable "TV���벻������! (��������Ӧ����������Ƶ��;)\n"
#define MSGTR_DemuxerInfoAlreadyPresent "Demuxer info %s �Ѿ���ʾ\n!"
#define MSGTR_ClipInfo "Clip info: \n"

#define MSGTR_LeaveTelecineMode "\ndemux_mpg: ��⵽��������, �뿪3:2 TELECINEģʽ\n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: ��⵽3:2 TELECINE, ���÷�תtelecine��Ч. FPS��Ϊ%5.3f!  \n"
// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "�޷��򿪽�����\n"
#define MSGTR_CantCloseCodec "�޷��رս�����\n"

#define MSGTR_MissingDLLcodec "����: �޷���Ҫ���DirectShow������: %s\n"
#define MSGTR_ACMiniterror "�޷�����/��ʼ��Win32/ACM��Ƶ������(ȱ��DLL�ļ�?)\n"
#define MSGTR_MissingLAVCcodec "��libavcodec���Ҳ��������� '%s'...\n"

#define MSGTR_MpegNoSequHdr "MPEG: ��������: ��������ͷʱ����EOF\n"
#define MSGTR_CannotReadMpegSequHdr "��������: �޷���ȡ����ͷ.\n"
#define MSGTR_CannotReadMpegSequHdrEx "��������: �޷���ȡ����ͷ��չ.\n"
#define MSGTR_BadMpegSequHdr "MPEG: ��������ͷ.\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: ��������ͷ��չ.\n"

#define MSGTR_ShMemAllocFail "�޷����乲���ڴ�\n"
#define MSGTR_CantAllocAudioBuf "�޷�������Ƶ���buffer\n"

#define MSGTR_UnknownAudio "δ֪��ȱ����Ƶ��ʽ, ʹ��nosound\n"

#define MSGTR_UsingExternalPP "[PP] ʹ���ⲿ�ĺ�����, max q = %d\n"
#define MSGTR_UsingCodecPP "[PP] ʹ�ý������ĺ�����, max q = %d\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "��ѡ��vo & vd��֧����Ƶ����'%s'. \n"
#define MSGTR_VideoCodecFamilyNotAvailableStr "Ҫ�����Ƶ�������� [%s] (vfm=%s) ������ (�ڱ���ʱ������)\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "Ҫ�����Ƶ�������� [%s] (afm=%s) ������ (�ڱ���ʱ������)\n"
#define MSGTR_OpeningVideoDecoder "����Ƶ������: [%s] %s\n"
#define MSGTR_OpeningAudioDecoder "����Ƶ������: [%s] %s\n"
#define MSGTR_UninitVideoStr "�ر���Ƶ: %s  \n"
#define MSGTR_UninitAudioStr "�ر���Ƶ: %s  \n"
#define MSGTR_VDecoderInitFailed "VDecoder��ʼ��ʧ�� :(\n"
#define MSGTR_ADecoderInitFailed "ADecoder��ʼ��ʧ�� :(\n"
#define MSGTR_ADecoderPreinitFailed "ADecoderԤ��ʼ��ʧ�� :(\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: Ϊ���뻺����� %d �ֽ�\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: Ϊ���������� %d + %d = %d �ֽ�\n"

// LIRC:
#define MSGTR_SettingUpLIRC "�𶯺���ң��֧��...\n"
#define MSGTR_LIRCdisabled "�㽫�޷�ʹ�����ң����\n"
#define MSGTR_LIRCopenfailed "����ң��֧����ʧ��!\n"
#define MSGTR_LIRCcfgerr "��ȡLIRC�����ļ� %s ʧ��!\n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "�Ҳ�����Ƶ�˾� '%s'\n"
#define MSGTR_CouldNotOpenVideoFilter "�޷�����Ƶ�˾� '%s'\n"
#define MSGTR_OpeningVideoFilter "����Ƶ�˾�: "
#define MSGTR_CannotFindColorspace "�޷��ҵ����õ�ɫ�ʿռ�, ����������'scale'Ҳ���� :(\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDec: �������޷�����sh->disp_w��sh->disp_h, �����ƹ�!\n"
#define MSGTR_VoConfigRequest "VDec: vo����Ҫ�� - %d x %d (ѡ�� csp: %s)\n"
#define MSGTR_CouldNotFindColorspace "�޷��ҵ�ƥ���ɫ�ʿռ� - ���³��� -vop scale...\n"
#define MSGTR_MovieAspectIsSet "��Ӱ��߱�Ϊ %.2f:1 - Ԥ�Ŵ���ȷ�ĵ�Ӱ��߱�.\n"
#define MSGTR_MovieAspectUndefined "��Ӱ��߱�δ���� - �޷�ʹ��Ԥ�Ŵ�.\n"

// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "����"
#define MSGTR_FileSelect "ѡ���ļ�..."
#define MSGTR_SubtitleSelect "ѡ����Ļ..."
#define MSGTR_OtherSelect "ѡ��..."
#define MSGTR_AudioFileSelect "ѡ���ⲿ��Ƶ���..."
#define MSGTR_FontSelect "ѡ������..."
#define MSGTR_PlayList "�����б�"
#define MSGTR_Equalizer "������"
#define MSGTR_SkinBrowser "Skin�����"
#define MSGTR_Network "������ý�� ..."
#define MSGTR_Preferences "��������"
#define MSGTR_OSSPreferences "OSS��������"
#define MSGTR_SDLPreferences "SDL��������"
#define MSGTR_NoMediaOpened "û�д�ý��"
#define MSGTR_VCDTrack "VCD %d ���"
#define MSGTR_NoChapter "û��chapter"
#define MSGTR_Chapter "chapter %d"
#define MSGTR_NoFileLoaded "û�������ļ�"

// --- buttons ---
#define MSGTR_Ok "ȷ��"
#define MSGTR_Cancel "ȡ��"
#define MSGTR_Add "���"
#define MSGTR_Remove "ɾ��"
#define MSGTR_Clear "���"
#define MSGTR_Config "����"
#define MSGTR_ConfigDriver "��������"
#define MSGTR_Browse "���"

// --- error messages ---
#define MSGTR_NEMDB "��Ǹ, û���㹻���ڴ����ڻ��ƻ���."
#define MSGTR_NEMFMR "��Ǹ, û���㹻���ڴ����ڲ˵���Ⱦ."
#define MSGTR_IDFGCVD "��Ǹ, �޷��ҵ�gui���ݵ���Ƶ�������."
#define MSGTR_NEEDLAVCFAME "��Ǹ, �㲻�������DXR3/H+�豸���������±�������ŷ�mpeg���ļ�.\n����DXR3/H+�����п���lavc����fame."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[skin] skin�����ļ��� %d: %s�г���"
#define MSGTR_SKIN_WARNING1 "[skin] ����, ��skin�����ļ��� %d��: �ҵ�widget������֮ǰû���ҵ�\"section\" ( %s )"
#define MSGTR_SKIN_WARNING2 "[skin] ����, ��skin�����ļ��� %d��: �ҵ�widget������֮ǰû���ҵ� \"subsection\" ( %s) "
#define MSGTR_SKIN_WARNING3 "[skin] ����, ��skin�����ļ��� %d��: ���widget��֧�����subsection(%s)"
#define MSGTR_SKIN_BITMAP_16bit  "��֧������16 bitsɫ���λͼ( %s ).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "�Ҳ����ļ�( %s )\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "bmp��ȡ����( %s )\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "tga��ȡ����( %s )\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "png��ȡ����( %s )\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "��֧��RLE��ʽѹ����tga( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "δ֪���ļ���ʽ( %s )\n"
#define MSGTR_SKIN_BITMAP_ConvertError "24 bit��32 bit��ת����������( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "δ֪��Ϣ: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "û���㹻�ڴ�\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "������̫������\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "�Ҳ��������ļ�\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "�Ҳ�������ͼ���ļ�\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "�����ڵ������ǩ( %s )\n"
#define MSGTR_SKIN_UnknownParameter "δ֪����( %s )\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[skinbrowser]û���㹻�ڴ�.\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "�Ҳ���Skin( %s ).\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "Skin�����ļ�( %s )��ȡ����.\n"
#define MSGTR_SKIN_LABEL "Skins:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "����MPlayer"
#define MSGTR_MENU_Open "�� ..."
#define MSGTR_MENU_PlayFile "�����ļ� ..."
#define MSGTR_MENU_PlayVCD "����VCD ..."
#define MSGTR_MENU_PlayDVD "����DVD ..."
#define MSGTR_MENU_PlayURL "����URL ..."
#define MSGTR_MENU_LoadSubtitle "������Ļ ..."
#define MSGTR_MENU_DropSubtitle "������Ļ ..."
#define MSGTR_MENU_LoadExternAudioFile "�����ⲿ��Ƶ�ļ� ..."
#define MSGTR_MENU_Playing "���ſ���"
#define MSGTR_MENU_Play "����"
#define MSGTR_MENU_Pause "��ͣ"
#define MSGTR_MENU_Stop "ֹͣ"
#define MSGTR_MENU_NextStream "��һ��"
#define MSGTR_MENU_PrevStream "��һ��"
#define MSGTR_MENU_Size "��С"
#define MSGTR_MENU_NormalSize "������С"
#define MSGTR_MENU_DoubleSize "˫����С"
#define MSGTR_MENU_FullScreen "ȫ��"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "�򿪵�Ƭ ..."
#define MSGTR_MENU_ShowDVDMenu "��ʾDVD�˵�"
#define MSGTR_MENU_Titles "Titles"
#define MSGTR_MENU_Title "Title %2d"
#define MSGTR_MENU_None "(none)"
#define MSGTR_MENU_Chapters "Chapters"
#define MSGTR_MENU_Chapter "Chapter %2d"
#define MSGTR_MENU_AudioLanguages "��Ƶ����"
#define MSGTR_MENU_SubtitleLanguages "��Ļ����"
#define MSGTR_MENU_PlayList "�����б�"
#define MSGTR_MENU_SkinBrowser "Skin�����"
#define MSGTR_MENU_Preferences "��������"
#define MSGTR_MENU_Exit "�˳� ..."
#define MSGTR_MENU_Mute "����"
#define MSGTR_MENU_Original "ԭʼ��"
#define MSGTR_MENU_AspectRatio "�����"
#define MSGTR_MENU_AudioTrack "��Ƶ���"
#define MSGTR_MENU_Track "��� %d"
#define MSGTR_MENU_VideoTrack "��Ƶ���"

// --- equalizer
#define MSGTR_EQU_Audio "��Ƶ"
#define MSGTR_EQU_Video "��Ƶ"
#define MSGTR_EQU_Contrast "�Աȶ�: "
#define MSGTR_EQU_Brightness "����: "
#define MSGTR_EQU_Hue "ɫ��: "
#define MSGTR_EQU_Saturation "���Ͷ�: "
#define MSGTR_EQU_Front_Left "ǰ��"
#define MSGTR_EQU_Front_Right "ǰ��"
#define MSGTR_EQU_Back_Left "����"
#define MSGTR_EQU_Back_Right "����"
#define MSGTR_EQU_Center "�м�"
#define MSGTR_EQU_Bass "����"
#define MSGTR_EQU_All "����"
#define MSGTR_EQU_Channel1 "���� 1:"
#define MSGTR_EQU_Channel2 "���� 2:"
#define MSGTR_EQU_Channel3 "���� 3:"
#define MSGTR_EQU_Channel4 "���� 4:"
#define MSGTR_EQU_Channel5 "���� 5:"
#define MSGTR_EQU_Channel6 "���� 6:"

// --- playlist
#define MSGTR_PLAYLIST_Path "·��"
#define MSGTR_PLAYLIST_Selected "��ѡ�ļ�"
#define MSGTR_PLAYLIST_Files "�����ļ�"
#define MSGTR_PLAYLIST_DirectoryTree "Ŀ¼��"

// --- preferences
#define MSGTR_PREFERENCES_Audio "��Ƶ"
#define MSGTR_PREFERENCES_Video "��Ƶ"
#define MSGTR_PREFERENCES_SubtitleOSD "��Ļ��OSD"
#define MSGTR_PREFERENCES_Misc "����"

#define MSGTR_PREFERENCES_None "None"
#define MSGTR_PREFERENCES_AvailableDrivers "��������:"
#define MSGTR_PREFERENCES_DoNotPlaySound "����������"
#define MSGTR_PREFERENCES_NormalizeSound "������׼��"
#define MSGTR_PREFERENCES_EnEqualizer "����������"
#define MSGTR_PREFERENCES_ExtraStereo "������������ǿ"
#define MSGTR_PREFERENCES_Coefficient "����:"
#define MSGTR_PREFERENCES_AudioDelay "��Ƶ�ӳ�"
#define MSGTR_PREFERENCES_DoubleBuffer "����˫�ػ���"
#define MSGTR_PREFERENCES_DirectRender "����ֱ����Ⱦ"
#define MSGTR_PREFERENCES_FrameDrop "������֡ѡ��"
#define MSGTR_PREFERENCES_HFrameDrop "����HARD��֡ѡ��(Σ��)"
#define MSGTR_PREFERENCES_Flip "���·�תͼ��"
#define MSGTR_PREFERENCES_Panscan "ͼ���и�: "
#define MSGTR_PREFERENCES_OSDTimer "��ʾ��ʱ����ָʾ��"
#define MSGTR_PREFERENCES_OSDProgress "ֻ��ʾ������"
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "��ʱ��, �ٷֱȺ���ʱ��"
#define MSGTR_PREFERENCES_Subtitle "��Ļ:"
#define MSGTR_PREFERENCES_SUB_Delay "�ӳ�: "
#define MSGTR_PREFERENCES_SUB_FPS "FPS:"
#define MSGTR_PREFERENCES_SUB_POS "λ��: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "������Ļ�Զ�װ��"
#define MSGTR_PREFERENCES_SUB_Unicode "Unicode��Ļ"
#define MSGTR_PREFERENCES_SUB_MPSUB "��������Ļת��ΪMplayer����Ļ�ļ�"
#define MSGTR_PREFERENCES_SUB_SRT "��������Ļת��Ϊ����ʱ���SubViewer(SRT) ��ʽ"
#define MSGTR_PREFERENCES_SUB_Overlap "������Ļ�ص�"
#define MSGTR_PREFERENCES_Font "����:"
#define MSGTR_PREFERENCES_Codecs "Codecs��demuxer"
#define MSGTR_PREFERENCES_FontFactor "����Ч��:"
#define MSGTR_PREFERENCES_PostProcess "�������ڴ���"
#define MSGTR_PREFERENCES_AutoQuality "�Զ���������: "
#define MSGTR_PREFERENCES_NI "ʹ�÷ǽ����AVI������"
#define MSGTR_PREFERENCES_IDX "�����Ҫ�Ļ�, �ؽ�������"
#define MSGTR_PREFERENCES_VideoCodecFamily "��Ƶ��������:"
#define MSGTR_PREFERENCES_AudioCodecFamily "��Ƶ��������:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "OSD����"
#define MSGTR_PREFERENCES_FRAME_Subtitle "��Ļ"
#define MSGTR_PREFERENCES_FRAME_Font "����"
#define MSGTR_PREFERENCES_FRAME_PostProcess "���ڴ���"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "Codec��demuxer"
#define MSGTR_PREFERENCES_FRAME_Cache "����"
#define MSGTR_PREFERENCES_FRAME_Misc "����"
#define MSGTR_PREFERENCES_OSS_Device "�豸:"
#define MSGTR_PREFERENCES_OSS_Mixer "������:"
#define MSGTR_PREFERENCES_SDL_Driver "����:"
#define MSGTR_PREFERENCES_Message "���ס, ��Щ����ֻ�����²��ź����Ч��."
#define MSGTR_PREFERENCES_DXR3_VENC "��Ƶ������:"
#define MSGTR_PREFERENCES_DXR3_LAVC "ʹ��LAVC(ffmpeg)"
#define MSGTR_PREFERENCES_DXR3_FAME "ʹ��FAME"
#define MSGTR_PREFERENCES_FontEncoding1 "Unicode"
#define MSGTR_PREFERENCES_FontEncoding2 "��ŷ(ISO-8859-1)"
#define MSGTR_PREFERENCES_FontEncoding3 "��ŷ(ISO-8859-15)"
#define MSGTR_PREFERENCES_FontEncoding4 "��ŷ(ISO-8859-2)"
#define MSGTR_PREFERENCES_FontEncoding5 "��ŷ(ISO-8859-3)"
#define MSGTR_PREFERENCES_FontEncoding6 "���޵���(ISO-8859-4)"
#define MSGTR_PREFERENCES_FontEncoding7 "˹������(ISO-8859-5)"
#define MSGTR_PREFERENCES_FontEncoding8 "��������(ISO-8859-6)"
#define MSGTR_PREFERENCES_FontEncoding9 "�ִ�ϣ����(ISO-8859-7)"
#define MSGTR_PREFERENCES_FontEncoding10 "��������(ISO-8859-9)"
#define MSGTR_PREFERENCES_FontEncoding11 "���޵���(ISO-8859-13)"
#define MSGTR_PREFERENCES_FontEncoding12 "��������(ISO-8859-14)"
#define MSGTR_PREFERENCES_FontEncoding13 "ϣ������(ISO-8859-8)"
#define MSGTR_PREFERENCES_FontEncoding14 "����(KOI8-R)"
#define MSGTR_PREFERENCES_FontEncoding15 "����(KOI8-U/RU)"
#define MSGTR_PREFERENCES_FontEncoding16 "��������(CP936)"
#define MSGTR_PREFERENCES_FontEncoding17 "��������(BIG5)"
#define MSGTR_PREFERENCES_FontEncoding18 "����(SHIFT-JIS)"
#define MSGTR_PREFERENCES_FontEncoding19 "����(CP949)"
#define MSGTR_PREFERENCES_FontEncoding20 "̩��(CP874)"
#define MSGTR_PREFERENCES_FontEncoding21 "Windows���������(CP1251)"
#define MSGTR_PREFERENCES_FontEncoding22 "Windows�������/��ŷ��(CP1250)"
#define MSGTR_PREFERENCES_FontNoAutoScale "���Զ�����"
#define MSGTR_PREFERENCES_FontPropWidth "��ȳɱ���"
#define MSGTR_PREFERENCES_FontPropHeight "�߶ȳɱ���"
#define MSGTR_PREFERENCES_FontPropDiagonal "�Խ��߳ɱ���"
#define MSGTR_PREFERENCES_FontEncoding "����:"
#define MSGTR_PREFERENCES_FontBlur "ģ��:"
#define MSGTR_PREFERENCES_FontOutLine "����:"
#define MSGTR_PREFERENCES_FontTextScale "��������:"
#define MSGTR_PREFERENCES_FontOSDScale "OSD����:"
#define MSGTR_PREFERENCES_Cache "��/�رջ���"
#define MSGTR_PREFERENCES_LoadFullscreen "��ȫ����ʽ��ʼ"
#define MSGTR_PREFERENCES_SaveWinPos "���洰��λ��"
#define MSGTR_PREFERENCES_CacheSize "�����С: "
#define MSGTR_PREFERENCES_XSCREENSAVER "ͣ��XScreenSaver"
#define MSGTR_PREFERENCES_PlayBar "ʹ�ò�����"
#define MSGTR_PREFERENCES_AutoSync "��ͬ�� ��/�ر�"
#define MSGTR_PREFERENCES_AutoSyncValue "��ͬ��: "
#define MSGTR_PREFERENCES_CDROMDevice "CD-ROM�豸:"
#define MSGTR_PREFERENCES_DVDDevice "DVD�豸:"
#define MSGTR_PREFERENCES_FPS "��Ӱ��FPS:"
#define MSGTR_PREFERENCES_ShowVideoWindow "�ڷǼ���״̬����ʾ��Ƶ����"

#define MSGTR_ABOUT_UHU "GUI������UHU Linux����\n"
#define MSGTR_ABOUT_CoreTeam "   MPlayer����С��:\n"
#define MSGTR_ABOUT_AdditionalCoders "   ����������:\n"
#define MSGTR_ABOUT_MainTesters "   ��Ҫ������:\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "�������� ..."
#define MSGTR_MSGBOX_LABEL_Error "���� ..."
#define MSGTR_MSGBOX_LABEL_Warning "���� ..."

#endif

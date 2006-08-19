// Sync'ed on 2003-07-26 with help_mp-en.h 1.121
// Translated by Kenneth Chan <chantk@ctk.sytes.net>
// With reference from help_mp-zh.h
// Synced by Lu Ran <hephooey@fastmail.fm>

// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"�Ϊk:   mplayer [options] [url|path/]filename\n"
"\n"
"�򥻿ﶵ: (���㪺�ﶵ�C��Ш� man page)\n"
" -vo <drv[:dev]>  ��ܵ��T��X�X�ʵ{���θ˸m (�� '-vo help' �d�ݦC��)\n"
" -ao <drv[:dev]>  ��ܭ��Ŀ�X�X�ʵ{���θ˸m (�� '-ao help' �d�ݦC��)\n"
#ifdef HAVE_VCD
" vcd://<trackno>   �q�˸m�ӨëD�@���ɮ׼��� VCD (Video CD) track\n"
#endif
#ifdef USE_DVDREAD
" dvd://<titleno>   �q�˸m�ӨëD�@���ɮ׼��� DVD title\n"
" -alang/-slang    ��� DVD ����/�r�����y�� (�ϥΨ�쪺��a�N��)\n"
#endif
" -ss <timepos>    �j���ܫ��w (��� hh:mm:ss) ����m\n"
" -nosound         �������n��\n"
" -fs              ���ù����� (�� -vm, -zoom�A�ԲӤ��e�Ш� man page)\n"
" -x <x> -y <y>    �]�w��ܸѪR�� (�P -vm �� -zoom �P�ɨϥ�)\n"
" -sub <file>      ���w�ϥΪ��r���� (�аѨ� -subfps, -subdelay)\n"
" -playlist <file> ���w����C��\n"
" -vid x -aid y    ��ܼ��񪺵��T (x) �έ��� (y) ��y\n"
" -fps x -srate y  ���ܵ��T (x fps) �� ���� (y Hz) �v\n"
" -pp <quality>    �ϥΫ���B�z�o�� (�ԲӤ��e�Ш� man page)\n"
" -framedrop       �ϥ� frame dropping (�Ω�C����)\n"
"\n"
"�򥻱�����: (���㪺�C��Ш� man page, �P�ɽЬd�\ input.conf)\n"
" <-  or  ->       �V�e/��j�� 10 ��\n"
" up or down       �V�e/��j�� 1 ����\n"
" pgup or pgdown   �V�e/��j�� 10 ����\n"
" < or >           ���ܼ���C�����e/��@��\n"
" p or SPACE       �Ȱ����� (�����N���~��)\n"
" q or ESC         ���������}\n"
" + or -           �վ㭵�ĩ��� +/- 0.1 ��\n"
" o                �`�� OSD �Ҧ�:  �L��� / �j�M�� / �j�M��+�p�ɾ�\n"
" * or /           �����έ��C PCM ���q\n"
" z or x           �վ�r������ +/- 0.1 ��\n"
" r or t           �W/�U�վ�r����m, �Ш� -vf expand\n"
"\n"
" * * * �ԲӤ��e, �i�@�B(�i��)�ﶵ�α�����Ш� MAN PAGE * * *\n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// mplayer.c:

#define MSGTR_Exiting "\n���b�h�X...\n"
#define MSGTR_ExitingHow "\n���b�h�X... (%s)\n"
#define MSGTR_Exit_quit "���}"
#define MSGTR_Exit_eof "�ɮץ���"
#define MSGTR_Exit_error "�P�R���~"
#define MSGTR_IntBySignal "\nMPlayer �Q %s �Ҳդ��� %d �T�� ���_\n"
#define MSGTR_NoHomeDir "�L�k��� HOME �ؿ�\n"
#define MSGTR_GetpathProblem "get_path(\"config\") ���D\n"
#define MSGTR_CreatingCfgFile "�إ� config ��: %s\n"
#define MSGTR_CopyCodecsConf "(�� etc/codecs.conf �q MPlayer ��{���X���ƻs/�إ߳s���� ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "�ϥΤ��عw�]�� codecs.conf�C\n"
#define MSGTR_CantLoadFont "�L�k���J�r��: %s\n"
#define MSGTR_CantLoadSub "�L�k���J�r��: %s\n"
#define MSGTR_DumpSelectedStreamMissing "dump: �P�R���~: ��ܪ���y�ä��s�b!\n"
#define MSGTR_CantOpenDumpfile "�L�k�}�� dump �ɡC\n"
#define MSGTR_CoreDumped "Core dumped ;)\n"
#define MSGTR_FPSnotspecified "FPS �å��b���Y�����w�άO�L�ġA�Шϥ� -fps �ﶵ�C\n"
#define MSGTR_TryForceAudioFmtStr "�����ձj����w���ĸѽX�X�ʵ{���ոs %s...\n"
#define MSGTR_CantFindAudioCodec "�L�k�����Į榡 0x%X ���ѽX���C\n"
#define MSGTR_RTFMCodecs "�Ѿ\DOCS/zh/codecs.html�T\n"
#define MSGTR_TryForceVideoFmtStr "�����ձj����w���T�ѽX�X�ʵ{���ոs %s...\n"
#define MSGTR_CantFindVideoCodec "�L�k���ҿ�ܪ� -vo �P���T�榡 0x%X ���A�X���ѽX���C\n"
#define MSGTR_VOincompCodec "�ҿ�ܪ� video_out �˸m�P�o�ӸѽX���ä��ݮe�C\n"
#define MSGTR_CannotInitVO "�P�R���~: �L�k��l�Ƶ��T�X�ʵ{���C\n"
#define MSGTR_CannotInitAO "�L�k�}��/��l�ƭ��ĸ˸m -> �S���n���C\n"
#define MSGTR_StartPlaying "�}�l����...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"           ************************************************\n"\
"           ****             �A���t�ΤӺC�F�T           ****\n"\
"           ************************************************\n\n"\
"�i�઺��]�B���D�B�ѨM��k:\n"\
"- �̴��M����]: �l�a�F/���Ϊ�_����_�X�ʵ{��\n"\
"  - �i���� -ao sdl �Ψϥ� ALSA 0.5 �� ALSA 0.9 �� OSS �������C\n"\
"  - �եΤ��P�� -autosync ��, �����q 30 �}�l�C\n"\
"- ���T��X�ӺC\n"\
"  - �i�եΤ��P�� -vo driver (-vo help ���C��) �θո� -framedrop!\n"\
"- CPU �ӺC\n"\
"  - ���n�չϦb�C��CPU�W���j�� DVD/DivX! �ե� -hardframedrop�C\n"\
"- �l�a�F���ɮ�\n"\
"  - �i�ոդ��P�զX�� -nobps -ni -forceidx -mc 0�C\n"\
"- �C��ӺC (NFS/SMB mounts, DVD, VCD ����)\n"\
"  - �i�ո� -cache 8192�C\n"\
"- �A�O�_���ϥ� -cache �ﶵ�Ӽ���@�ӫD����� AVI �ɮ�?\n"\
"  - �i�ո� -nocache�C\n"\
"�n���o�վ�/�[�t�����Z�аѾ\ DOCS/zh/video.html �P DOCS/zh/sound.html�C\n"\
"���p�H�W�S�@�����o�W�A�аѾ\ DOCS/zh/bugreports.html�C\n\n"

#define MSGTR_NoGui "MPlayer �sĶ�õL GUI �䴩�C\n"
#define MSGTR_GuiNeedsX "MPlayer GUI �ݭn X11�C\n"
#define MSGTR_Playing "���b���� %s�C\n"
#define MSGTR_NoSound "����: �S���n��\n"
#define MSGTR_FPSforced "FPS �Q���w�� %5.3f  (ftime: %5.3f)�C\n"
#define MSGTR_CompiledWithRuntimeDetection "�sĶ�]�A�F����ɴ�CPU���d - ĵ�i - �o�ëD�̨Τ�!\n�n��o�̨Ϊ�{�A�[�W --disable-runtime-cpudetection �ﶵ���s�sĶ MPlayer�C\n"
#define MSGTR_CompiledWithCPUExtensions "�� x86 CPU �sĶ�æ� extensions:\n"
#define MSGTR_AvailableVideoOutputDrivers "�i�Ϊ����T��X�X�ʵ{��:\n"
#define MSGTR_AvailableAudioOutputDrivers "�i�Ϊ����Ŀ�X�X�ʵ{��:\n"
#define MSGTR_AvailableAudioCodecs "�i�Ϊ����� codecs:\n"
#define MSGTR_AvailableVideoCodecs "�i�Ϊ����T codecs:\n"
#define MSGTR_AvailableAudioFm "\n�i�Ϊ�(�sĶ�F��)���� codec ��/�X�ʵ{��:\n"
#define MSGTR_AvailableVideoFm "\n�i�Ϊ�(�sĶ�F��)���T codec ��/�X�ʵ{��:\n"
#define MSGTR_AvailableFsType "�i�Ϊ��������h���ܼҦ�:\n"
#define MSGTR_UsingRTCTiming "���ϥ� Linux �w�� RTC �p��(%ldHz)�C\n"
#define MSGTR_CannotReadVideoProperties "���T: �L�kŪ�����e�C\n"
#define MSGTR_NoStreamFound "�䤣�� stream�C\n"
#define MSGTR_ErrorInitializingVODevice "�}��/��l�Ʃҿ�ܪ����T��X (-vo) �˸m�ɵo�Ϳ��~�C\n"
#define MSGTR_ForcedVideoCodec "�j��ϥΪ����T codec: %s\n"
#define MSGTR_ForcedAudioCodec "�j��ϥΪ����� codec: %s\n"
#define MSGTR_Video_NoVideo "���T: �S���v��\n"
#define MSGTR_NotInitializeVOPorVO "\n�P�R���~: �L�k��l�Ƽv���L�o�� (-vf) �� �v����X (-vo)�C\n"
#define MSGTR_Paused "\n  ====== �Ȱ� ======\r"
#define MSGTR_PlaylistLoadUnable "\n�L�k���J����C %s�C\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- '�D�k���O'�ɭP MPlayer ��F�C\n"\
"  �o�i��O�ڭ̷s������ɴ� CPU ���d�{���X�����@������...\n"\
"  �аѾ\ DOCS/zh/bugreports.html�C\n"
#define MSGTR_Exit_SIGILL \
"- '�D�k���O'�ɭP MPlayer ��F�C\n"\
"  �o�q�`�o�ͩ��A�b�@�ӻP�sĶ/�̨Τ� MPlayer ���P�� CPU �W�ϥΥ�\n"\
"  �ҳy�����C\n"\
"  �ˬd�@�U�a�T\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- ���}�� CPU/FPU/RAM ���ξɭP MPlayer ��F�C\n"\
"  �i�ϥ� --enable-debug �ӭ��s�sĶ MPlayer �ð� 'gdb' backtrace ��\n"\
"  disassembly�C����Ӹ`�аѾ\ DOCS/zh/bugreports.html#crash�C\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer ��F�C �o�O�����ӵo�ͪ��C\n"\
"  �o�i��O�b MPlayer �{���X _��_ �A���X�ʵ{�� _��_ �A�� gcc ����\n"\
"  �������ΡC���p�A�{���O MPlayer ����f�A�аѾ\ \n"\
"  DOCS/zh/bugreports.html �ø�q��B�J�C���D�A�b���i�h�ìO���ή�\n"\
"  �ണ�ѳo�Ǹ�ơA�_�h�ڭ̱N�L�k�Τ��|�����C\n"


// mencoder.c:

#define MSGTR_UsingPass3ControlFile "���b�ϥ� pass3 ������: %s\n"
#define MSGTR_MissingFilename "\n�S���ɮצW�١C\n\n"
#define MSGTR_CannotOpenFile_Device "�L�k�}���ɮ�/�˸m�C\n"
#define MSGTR_CannotOpenDemuxer "�L�k�}�� demuxer�C\n"
#define MSGTR_NoAudioEncoderSelected "\n�S����ܭ��Ľs�X�� (-oac)�C�п�ܤ@�� (�i�� -oac help) �Ψϥ� -nosound�C\n"
#define MSGTR_NoVideoEncoderSelected "\n�S����ܵ��T�s�X�� (-ovc)�C�п�ܤ@�� (�i�� -oac help)�C\n"
#define MSGTR_CannotOpenOutputFile "�L�k�}�ҿ�X�� '%s'�C\n"
#define MSGTR_EncoderOpenFailed "�L�k�}�ҽs�X���C\n"
#define MSGTR_ForcingOutputFourcc "�j���X fourcc �� %x [%.4s]\n"
#define MSGTR_DuplicateFrames "\n�� %d �歫�СT\n"
#define MSGTR_SkipFrame "\n���L�o�@��T\n"
#define MSGTR_ErrorWritingFile "%s: �g�J�ɮצ����~�C\n"
#define MSGTR_RecommendedVideoBitrate "%s CD �ҫ�ĳ�����T bitrate: %d\n"
#define MSGTR_VideoStreamResult "\n���T��y: %8.3f kbit/s  (%d B/s)  �j��: %"PRIu64" bytes  %5.3f �� %d ��\n"
#define MSGTR_AudioStreamResult "\n���Ħ�y: %8.3f kbit/s  (%d B/s)  �j��: %"PRIu64" bytes  %5.3f ��\n"

// cfg-mencoder.h:

#define MSGTR_MEncoderMP3LameHelp "\n\n"\
" vbr=<0-4>     �ܰ� bitrate �覡\n"\
"                0: cbr\n"\
"                1: mt\n"\
"                2: rh(�w�])\n"\
"                3: abr\n"\
"                4: mtrh\n"\
"\n"\
" abr           ���� bitrate\n"\
"\n"\
" cbr           �T�w bitrate\n"\
"               �b���᪺ ABR �w�ռҦ�����j��ϥ� CBR �s�X�Ҧ��C\n"\
"\n"\
" br=<0-1024>   �H kBit �������w bitrate (�ȾA�Ω� CBR �� ABR)\n"\
"\n"\
" q=<0-9>       ��� (0-�̰�, 9-�̧C) (�ȾA�Ω� VBR)\n"\
"\n"\
" aq=<0-9>      �p��k��� (0-�̦n/�̺C, 9-�̮t/�̧�)\n"\
"\n"\
" ratio=<1-100> ���Y��v\n"\
"\n"\
" vol=<0-10>    �]�w���Ŀ�J�W��\n"\
"\n"\
" mode=<0-3>    (�w�]: �۰�)\n"\
"                0: �����n\n"\
"                1: �s�������n\n"\
"                2: ���n�D\n"\
"                3: ���n�D\n"\
"\n"\
" padding=<0-2>\n"\
"                0: �L\n"\
"                1: ����\n"\
"                2: �ծ�\n"\
"\n"\
" fast          �b���᪺ VBR �w�ռҦ������ҥθ��֪� encoding�A\n"\
"               �ǷL���C����θ��� bitrates�C\n"\
"\n"\
" preset=<value> ���ѳ̰��i�઺����]�w�C\n"\
"                 medium: VBR  �s�X,  �n���\n"\
"                 (150-180 kbps bitrate �d��)\n"\
"                 standard:  VBR �s�X, �����\n"\
"                 (170-210 kbps bitrate �d��)\n"\
"                 extreme: VBR �s�X, �D�`�����\n"\
"                 (200-240 kbps bitrate �d��)\n"\
"                 insane:  CBR  �s�X, �̰����\n"\
"                 (320 kbps bitrate)\n"\
"                 <8-320>: �H�Ҵ��Ѫ������� kbps bitrate ABR �s�X�C\n\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "�䤣�� CD-ROM �˸m '%s'�C\n"
#define MSGTR_ErrTrackSelect "���~��� VCD �y�C"
#define MSGTR_ReadSTDIN "�q stdin Ū��...\n"
#define MSGTR_UnableOpenURL "�L�k�}�� URL: %s\n"
#define MSGTR_ConnToServer "�w�������A��: %s\n"
#define MSGTR_FileNotFound "�䤣���ɮ�: '%s'\n"

#define MSGTR_SMBInitError "�L�k��l libsmbclient library: %d\n"
#define MSGTR_SMBFileNotFound "�L�k�q LAN: '%s' �}��\n"
#define MSGTR_SMBNotCompiled "MPlayer �sĶ�õLŪ�� SMB �䴩�C\n"

#define MSGTR_CantOpenDVD "�L�k�}�� DVD �˸m: %s\n"
#define MSGTR_DVDnumTitles "�o�� DVD ���� %d �� titles�C\n"
#define MSGTR_DVDinvalidTitle "�L�Ī� DVD title ����: %d\n"
#define MSGTR_DVDnumChapters "�o�� DVD title ���� %d �� chapters�C\n"
#define MSGTR_DVDinvalidChapter "�L�Ī� DVD chapter ����: %d\n"
#define MSGTR_DVDnumAngles "�o�� DVD title ���� %d �Ө��סC\n"
#define MSGTR_DVDinvalidAngle "�L�Ī� DVD ���׸��X: %d\n"
#define MSGTR_DVDnoIFO "�L�k�� DVD title %d �}�� IFO �ɡC\n"
#define MSGTR_DVDnoVOBs "�L�k�}�� title VOBS (VTS_%02d_1.VOB)�C\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "ĵ�i: ���Ħ�y���Y %d �q�s�w�q�C\n"
#define MSGTR_VideoStreamRedefined "ĵ�i: ���T��y���Y %d �q�s�w�q�C\n"
#define MSGTR_TooManyAudioInBuffer "\n�w�İϦ��Ӧh���īʥ]: (%d bytes ���� %d ��)�C\n"
#define MSGTR_TooManyVideoInBuffer "\n�w�İϦ��Ӧh���T�ʥ]: (%d bytes ���� %d ��)�C\n"
#define MSGTR_MaybeNI "�i��A�b����@�ӫD���������y/�ɡA�Ϊ̬O codec ���ѤF�S\n" \
		      "�p�G�O AVI �ɡA�i�H�ե� -ni �ﶵ�Ӱ���D����Ҧ��C\n"
#define MSGTR_SwitchToNi "\n���������o�ܼF�`�� AVI �� - �ഫ�� -ni �Ҧ�...\n"
#define MSGTR_Detected_XXX_FileFormat "������ %s �ɮ榡�C\n"
#define MSGTR_DetectedAudiofile "�����쭵���ɡC\n"
#define MSGTR_NotSystemStream "�ëD MPEG �t�Φ�y�榡... (�i��O��e��y�S)\n"
#define MSGTR_InvalidMPEGES "�L�Ī� MPEG-ES ��y??? �o�i��O�@�����ΡA���p���@�� :(\n"
#define MSGTR_FormatNotRecognized "============ �ܩ�p�A�o���ɮ׮榡����Q��{/���䴩 =============\n"\
				  "=== �p�G�o�O�� AVI �ɡBASF �� MPEG ��y�A���p���@�̡I ===\n"
#define MSGTR_MissingVideoStream "�䤣����T��y�C\n"
#define MSGTR_MissingAudioStream "�䤣�쭵�Ħ�y -> �L�n���C\n"
#define MSGTR_MissingVideoStreamBug "�ʤ֤F���T��y!? �o�i��O�ӯ��ΡA���p���@�� :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: �ɮפ��õL�ҿ�ܪ����ĩε��T��y�C\n"

#define MSGTR_NI_Forced "�j��ϥ�"
#define MSGTR_NI_Detected "������"
#define MSGTR_NI_Message "%s �D����� AVI �ɮ׮榡�C\n"

#define MSGTR_UsingNINI "���ϥΫD������l�a�F�� AVI �ɮ׮榡�C\n"
#define MSGTR_CouldntDetFNo "�L�k (���ǽT�j�M) �T�w��ơC\n"
#define MSGTR_CantSeekRawAVI "�L�k�b�����㪺 AVI ��y���@�j�M�C(�ݭn���ޡA�ե� -idx �ﶵ�C)\n"
#define MSGTR_CantSeekFile "�L�k�b�o�ɮפ��@�j�M�C\n"

#define MSGTR_EncryptedVOB "�w�[�K�� VOB �ɡT�аѾ\ DOCS/zh/cd-dvd.html�C\n"

#define MSGTR_MOVcomprhdr "MOV: ���Y�����Y���䴩�ݭnZLIB�T\n"
#define MSGTR_MOVvariableFourCC "MOV: ĵ�i: �������ܰʪ� FOURCC!?\n"
#define MSGTR_MOVtooManyTrk "MOV: ĵ�i: ���Ӧh�����y"
#define MSGTR_FoundAudioStream "==> ��쭵�Ħ�y: %d\n"
#define MSGTR_FoundVideoStream "==> �����T��y: %d\n"
#define MSGTR_DetectedTV "�����즳 TV �T ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "�L�k�}�� ogg demuxer�C\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: ���b�j�M���Ħ�y (id:%d)�C\n"
#define MSGTR_CannotOpenAudioStream "�L�k�}�ҭ��Ħ�y: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "�L�k�}�Ҧr����y: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "�L�k���\�}�ҭ��� demuxer: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "�L�k���\�}�Ҧr�� demuxer: %s\n"
#define MSGTR_TVInputNotSeekable "TV ��J����j���T(�j���i��O�Ψ��ഫ�W�D ;)\n"
#define MSGTR_DemuxerInfoAlreadyPresent "�w�g�� demuxer ��T %s�T\n"
#define MSGTR_ClipInfo "���q���:\n"

#define MSGTR_LeaveTelecineMode "\ndemux_mpg: ������30fps��NTSC���e�A���ܴV�t�v�C\n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: ������24fps������NTSC���e�A���ܴV�t�v�C\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "�L�k�}�� codec�C\n"
#define MSGTR_CantCloseCodec "�L�k���� codec�C\n"

#define MSGTR_MissingDLLcodec "���~: �L�k�}�ҩһݪ� DirectShow codec %s�C\n"
#define MSGTR_ACMiniterror "�L�k���J/��l Win32/ACM AUDIO codec (DLL �ɥ��ܤF�S)�C\n"
#define MSGTR_MissingLAVCcodec "�L�k�b libavcodec ����� codec '%s'...\n"

#define MSGTR_MpegNoSequHdr "MPEG: �P�R���~: ��j�� sequence header �ɹJ���ɮץ� (EOF)�C\n"
#define MSGTR_CannotReadMpegSequHdr "�P�R���~: �L�kŪ�� sequence header�C\n"
#define MSGTR_CannotReadMpegSequHdrEx "�P�R���~: �L�kŪ�� sequence header extension�C\n"
#define MSGTR_BadMpegSequHdr "MPEG: �ܮt�� sequence header\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: �ܮt�� sequence header extension\n"

#define MSGTR_ShMemAllocFail "�L�k���t�@�ΰO�СC\n"
#define MSGTR_CantAllocAudioBuf "�L�k���t audio out �w�İϡC\n"

#define MSGTR_UnknownAudio "������/�䤣�쪺���Į榡 -> �S���n��\n"

#define MSGTR_UsingExternalPP "[PP] ���ϥΥ~�m������B�z�L�o���A�̤j�� q = %d�C\n"
#define MSGTR_UsingCodecPP "[PP] ���ϥ� codec ������B�z�A�̤j�� q = %d�C\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "�v���ݩ� '%s' ���Q�ҿ�ܪ� vo �� vd �䴩�C\n"
#define MSGTR_VideoCodecFamilyNotAvailableStr "�S���ҭn�D�� codec �ڸs [%s] (vfm=%s)�C\n�Цb�sĶ�ɿ�w�C\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "�S���ҭn�D�� codec �ڸs [%s] (afm=%s)�C\n�Цb�sĶ�ɿ�w�C\n"
#define MSGTR_OpeningVideoDecoder "���b�}�Ҽv���ѽX��: [%s] %s\n"
#define MSGTR_OpeningAudioDecoder "���b�}�ҭ��ĸѽX��: [%s] %s\n"
#define MSGTR_UninitVideoStr "����l���T: %s\n"
#define MSGTR_UninitAudioStr "����l����: %s\n"
#define MSGTR_VDecoderInitFailed "VDecoder ��l���� :(\n"
#define MSGTR_ADecoderInitFailed "ADecoder ��l���� :(\n"
#define MSGTR_ADecoderPreinitFailed "ADecoder �w����l���� :(\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: ���b���t %d bytes ����J�w�İ�\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: ���b���t %d + %d = %d bytes ����X�w�İ�\n"

// LIRC:
#define MSGTR_SettingUpLIRC "���b�]�w LIRC �䴩...\n"
#define MSGTR_LIRCdisabled "�N�L�k�ϥλդU���������C\n"
#define MSGTR_LIRCopenfailed "�L�k�}�� LIRC �䴩�C\n"
#define MSGTR_LIRCcfgerr "Ū�� LIRC �]�w�� %s ���ѡC\n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "�L�k���v���L�o�� '%s'\n"
#define MSGTR_CouldNotOpenVideoFilter "�L�k�}�Ҽv���L�o�� '%s'\n"
#define MSGTR_OpeningVideoFilter "�}�Ҽv���L�o��: "
#define MSGTR_CannotFindColorspace "�L�k���@�Ϊ� colorspace�A�Y�ϥ[�J 'scale' :(\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDec: Codec �S���]�w sh->disp_w �� sh->disp_h�A�����ոѨM��k�C\n"
#define MSGTR_VoConfigRequest "VDec: vo �]�w�n�D �X %d x %d (�ߦn�� csp: %s)\n"
#define MSGTR_CouldNotFindColorspace "�L�k���t�X�� colorspace �X �� -vf scale �A����...\n"
#define MSGTR_MovieAspectIsSet "�q�v��ҬO %.2f:1 �X �ϥ� prescaling �ծզܥ��T��ҡC\n"
#define MSGTR_MovieAspectUndefined "�q�v��ҥ������� �X �õL�ϥ� prescaling�C\n"

// vd_dshow.c, vd_dmo.c
#define MSGTR_DownloadCodecPackage "�A�ݭn�ɯ�/�w�ˤG�i��codecs�]�C\n�гX��http://www.mplayerhq.hu/dload.html\n"
#define MSGTR_DShowInitOK "INFO: Win32/DShow�v��codec��lOK�C\n"
#define MSGTR_DMOInitOK "INFO: Win32/DMO�v��codec��l�C\n"

// x11_common.c
#define MSGTR_EwmhFullscreenStateFailed "\nX11: �L�k�o�eEWMH���ù��ƥ�I\n"


// ====================== GUI messages/buttons ============================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "����"
#define MSGTR_FileSelect "����ɮ�..."
#define MSGTR_SubtitleSelect "��ܦr��..."
#define MSGTR_OtherSelect "���..."
#define MSGTR_AudioFileSelect "��ܥ~�m�����W�D..."
#define MSGTR_FontSelect "��ܦr��..."
#define MSGTR_PlayList "����C"
#define MSGTR_Equalizer "���ž�"
#define MSGTR_SkinBrowser "�G���s����"
#define MSGTR_Network "������y..."
#define MSGTR_Preferences "�ߦn�]�w"
#define MSGTR_NoMediaOpened "�S���C��}�ҡC"
#define MSGTR_VCDTrack "VCD �� %d �y"
#define MSGTR_NoChapter "�S�� chapter"
#define MSGTR_Chapter "Chapter %d"
#define MSGTR_NoFileLoaded "�S�����J�ɮסC"

// --- buttons ---
#define MSGTR_Ok "�T�w"
#define MSGTR_Cancel "����"
#define MSGTR_Add "�[�J"
#define MSGTR_Remove "����"
#define MSGTR_Clear "�M��"
#define MSGTR_Config "�]�w"
#define MSGTR_ConfigDriver "�]�w�X�ʵ{��"
#define MSGTR_Browse "�s��"

// --- error messages ---
#define MSGTR_NEMDB "�ܩ�p�Aø�ϴ��İϨS�������O�СC"
#define MSGTR_NEMFMR "�ܩ�p�A�ؿ���ܨS�������O�СC"
#define MSGTR_IDFGCVD "�ܩ�p�A�䤣��@�� GUI �ݮe�����T��X�X�ʵ{���C"
#define MSGTR_NEEDLAVCFAME "�ܩ�p�A�L�k�ΧA�� DXR3/H+ �˸m�Ӽ��񥼭��s�s�X���D MPEG �ɡC\n�ШϥΩ� DXR3/H+ �]�w�椧 lavc �� fame�C"

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[skin] �� skin �]�w�� %d: %s ��X��"
#define MSGTR_SKIN_WARNING1 "[skin] ĵ�i�A�� skin �]�w�� %d ��: ��� widget ���b�o���e�䤣�� \"section\" (%s)"
#define MSGTR_SKIN_WARNING2 "[skin] ĵ�i�A�� skin �]�w�� %d ��: ��� widget ���b�o���e�䤣�� \"subsection\" (%s)"
#define MSGTR_SKIN_WARNING3 "[skin] ĵ�i�A�� skin �]�w�� %d ��: �o�� widget �ä��䴩�o�� subsection (%s)"
#define MSGTR_SKIN_BITMAP_16bit  "���䴩 16 �줸�ΥH�U����m�I�} (%s)�C\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "�䤣���ɮ� (%s)\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "BMP Ū�����~ (%s)\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "TGA Ū�����~ (%s)\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "PNG Ū�����~ (%s)\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "RLE ���Y�� TGA �ä��䴩 (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "�������ɮ����O (%s)\n"
#define MSGTR_SKIN_BITMAP_ConversionError "24 �줸�� 32 �줸�ഫ���~ (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "�������T��: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "�O���餣��\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "�ŧi�F�Ӧh�r���C\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "�䤣��r���ɡC\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "�䤣��r���ι��ɡC\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "�ä��s�b���r���ѧO�� (%s)\n"
#define MSGTR_SKIN_UnknownParameter "�������Ѽ� (%s)\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "�䤣�� skin (%s)�C\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "Skin �]�w��Ū�����~ (%s)�C\n"
#define MSGTR_SKIN_LABEL "Skins:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "���� MPlayer"
#define MSGTR_MENU_Open "�}��..."
#define MSGTR_MENU_PlayFile "�����ɮ�..."
#define MSGTR_MENU_PlayVCD "���� VCD..."
#define MSGTR_MENU_PlayDVD "���� DVD..."
#define MSGTR_MENU_PlayURL "���� URL..."
#define MSGTR_MENU_LoadSubtitle "���J�r��..."
#define MSGTR_MENU_DropSubtitle "�M���r��..."
#define MSGTR_MENU_LoadExternAudioFile "���J�~�m������..."
#define MSGTR_MENU_Playing "���b����"
#define MSGTR_MENU_Play "����"
#define MSGTR_MENU_Pause "�Ȱ�"
#define MSGTR_MENU_Stop "����"
#define MSGTR_MENU_NextStream "�U�@�Ӧ�y"
#define MSGTR_MENU_PrevStream "�W�@�Ӧ�y"
#define MSGTR_MENU_Size "�j�p"
#define MSGTR_MENU_NormalSize "���q�j�p"
#define MSGTR_MENU_DoubleSize "�����j�p"
#define MSGTR_MENU_FullScreen "���ù�"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "�}�ҥ���..."
#define MSGTR_MENU_ShowDVDMenu "��� DVD �ؿ�"
#define MSGTR_MENU_Titles "���D"
#define MSGTR_MENU_Title "���D %2d"
#define MSGTR_MENU_None "(�L)"
#define MSGTR_MENU_Chapters "Chapters"
#define MSGTR_MENU_Chapter "Chapter %2d"
#define MSGTR_MENU_AudioLanguages "���Ļy��"
#define MSGTR_MENU_SubtitleLanguages "�r���y��"
// TODO: Why is this different from MSGTR_PlayList?
#define MSGTR_MENU_PlayList "����C��"
#define MSGTR_MENU_SkinBrowser "Skin �s����"
#define MSGTR_MENU_Exit "�h�X..."
#define MSGTR_MENU_Mute "�R��"
#define MSGTR_MENU_Original "��Ӫ�"
#define MSGTR_MENU_AspectRatio "�v����v"
#define MSGTR_MENU_AudioTrack "���y"
#define MSGTR_MENU_Track "�� %d ��"
#define MSGTR_MENU_VideoTrack "�v���y"

// --- equalizer
#define MSGTR_EQU_Audio "����"
#define MSGTR_EQU_Video "���T"
#define MSGTR_EQU_Contrast "����: "
#define MSGTR_EQU_Brightness "���t��: "
#define MSGTR_EQU_Hue "��m��: "
#define MSGTR_EQU_Saturation "���M��: "
#define MSGTR_EQU_Front_Left "���e"
#define MSGTR_EQU_Front_Right "�k�e"
#define MSGTR_EQU_Back_Left "����"
#define MSGTR_EQU_Back_Right "�k��"
#define MSGTR_EQU_Center "���m"
#define MSGTR_EQU_Bass "�C��"
#define MSGTR_EQU_All "����"
#define MSGTR_EQU_Channel1 "�n�D 1:"
#define MSGTR_EQU_Channel2 "�n�D 2:"
#define MSGTR_EQU_Channel3 "�n�D 3:"
#define MSGTR_EQU_Channel4 "�n�D 4:"
#define MSGTR_EQU_Channel5 "�n�D 5:"
#define MSGTR_EQU_Channel6 "�n�D 6:"

// --- playlist
#define MSGTR_PLAYLIST_Path "���|"
#define MSGTR_PLAYLIST_Selected "��ܪ��ɮ�"
#define MSGTR_PLAYLIST_Files "�ɮ�"
#define MSGTR_PLAYLIST_DirectoryTree "�ؿ���"

// --- preferences
#define MSGTR_PREFERENCES_SubtitleOSD "�r���� OSD"
#define MSGTR_PREFERENCES_Codecs "Codecs & demuxer"
#define MSGTR_PREFERENCES_Misc "����"

#define MSGTR_PREFERENCES_None "�L"
#define MSGTR_PREFERENCES_AvailableDrivers "�i�Ϊ��X�ʵ{��:"
#define MSGTR_PREFERENCES_DoNotPlaySound "�������n��"
#define MSGTR_PREFERENCES_NormalizeSound "���`���n��"
#define MSGTR_PREFERENCES_EnableEqualizer "�ĥνխ���"
#define MSGTR_PREFERENCES_ExtraStereo "�ĥ��B�~�����n"
#define MSGTR_PREFERENCES_Coefficient "�Y��:"
#define MSGTR_PREFERENCES_AudioDelay "���ĩ���"
#define MSGTR_PREFERENCES_DoubleBuffer "�ĥ� double buffering"
#define MSGTR_PREFERENCES_DirectRender "�ĥ� direct rendering"
#define MSGTR_PREFERENCES_FrameDrop "�ĥ� frame dropping"
#define MSGTR_PREFERENCES_HFrameDrop "�ĥ� HARD frame dropping (��M�I��)"
#define MSGTR_PREFERENCES_Flip "�W�U����v��"
#define MSGTR_PREFERENCES_Panscan "Panscan: "
#define MSGTR_PREFERENCES_OSDTimer "�p�ɾ�����ܾ�"
#define MSGTR_PREFERENCES_OSDProgress "�ȶi�״�"
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "�p�ɾ��B�ʥ�����`�@�ɶ�"
#define MSGTR_PREFERENCES_Subtitle "�r��:"
#define MSGTR_PREFERENCES_SUB_Delay "����: "
#define MSGTR_PREFERENCES_SUB_FPS "FPS:"
#define MSGTR_PREFERENCES_SUB_POS "��m: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "���|�۰ʸ��J�r��"
#define MSGTR_PREFERENCES_SUB_Unicode "�Τ@�X�r��"
#define MSGTR_PREFERENCES_SUB_MPSUB "�ഫ���Ѫ��r���� MPlayer ���r���榡"
#define MSGTR_PREFERENCES_SUB_SRT "�ഫ���Ѫ��r���ܮɶ��ʪ� SubViewer (SRT) �榡"
#define MSGTR_PREFERENCES_SUB_Overlap "�}���r�����|"
#define MSGTR_PREFERENCES_Font "�r��:"
#define MSGTR_PREFERENCES_FontFactor "�r���]��:"
#define MSGTR_PREFERENCES_PostProcess "�ĥΫ�m�B�z"
#define MSGTR_PREFERENCES_AutoQuality "�۰ʽ��: "
#define MSGTR_PREFERENCES_NI "�ϥΫD����� AVI �y�k���R��"
#define MSGTR_PREFERENCES_IDX "�p���ݭn�A���s�إ߯��ު�"
#define MSGTR_PREFERENCES_VideoCodecFamily "���T codec �a��:"
#define MSGTR_PREFERENCES_AudioCodecFamily "���� codec �a��:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "OSD ��"
#define MSGTR_PREFERENCES_FRAME_Subtitle "�r��"
#define MSGTR_PREFERENCES_FRAME_Font "�r��"
#define MSGTR_PREFERENCES_FRAME_PostProcess "��m�B�z"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "Codec & demuxer"
#define MSGTR_PREFERENCES_FRAME_Cache "�֨��O��"
#define MSGTR_PREFERENCES_Message "�аO�o�Y�ǿﶵ�n���s����~�|�ͮġT"
#define MSGTR_PREFERENCES_DXR3_VENC "���T encoder:"
#define MSGTR_PREFERENCES_DXR3_LAVC "�ϥ� LAVC (FFmpeg)"
#define MSGTR_PREFERENCES_DXR3_FAME "�ϥ� FAME"
#define MSGTR_PREFERENCES_FontEncoding1 "�Τ@�X"
#define MSGTR_PREFERENCES_FontEncoding2 "��ڻy�t (ISO-8859-1)"
#define MSGTR_PREFERENCES_FontEncoding3 "��ڻy�t�]�t��ù�Ÿ� (ISO-8859-15)"
#define MSGTR_PREFERENCES_FontEncoding4 "���Ԥ�/���ڻy�t (ISO-8859-2)"
#define MSGTR_PREFERENCES_FontEncoding5 "�@�ɻy�B�[����Ȼy�B�����L�y�B�g�ը�y (ISO-8859-3)"
#define MSGTR_PREFERENCES_FontEncoding6 "�ªiù�����r�� (ISO-8859-4)"
#define MSGTR_PREFERENCES_FontEncoding7 "���Ԥһy (ISO-8859-5)"
#define MSGTR_PREFERENCES_FontEncoding8 "���ԧB�y (ISO-8859-6)"
#define MSGTR_PREFERENCES_FontEncoding9 "�{�N��þ (ISO-8859-7)"
#define MSGTR_PREFERENCES_FontEncoding10 "�g�ը�y (ISO-8859-9)"
#define MSGTR_PREFERENCES_FontEncoding11 "�iù�����y (ISO-8859-13)"
#define MSGTR_PREFERENCES_FontEncoding12 "�J���S�y (ISO-8859-14)"
#define MSGTR_PREFERENCES_FontEncoding13 "�ƧB�Ӥ�r�� (ISO-8859-8)"
#define MSGTR_PREFERENCES_FontEncoding14 "�Xù���y (KOI8-R)"
#define MSGTR_PREFERENCES_FontEncoding15 "�Q�J���B�իXù���y (KOI8-U/RU)"
#define MSGTR_PREFERENCES_FontEncoding16 "²�餤��r�� (CP936)"
#define MSGTR_PREFERENCES_FontEncoding17 "�c�餤��r�� (BIG5)"
#define MSGTR_PREFERENCES_FontEncoding18 "���r�� (SHIFT-JIS)"
#define MSGTR_PREFERENCES_FontEncoding19 "����r�� (CP949)"
#define MSGTR_PREFERENCES_FontEncoding20 "����r�� (CP874)"
#define MSGTR_PREFERENCES_FontEncoding21 "���Ԥҵ��� (CP1251)"
#define MSGTR_PREFERENCES_FontEncoding22 "���Ԥ�/���ڵ��� (CP1250)"
#define MSGTR_PREFERENCES_FontNoAutoScale "�S���۰ʤ��"
#define MSGTR_PREFERENCES_FontPropWidth "�ھڹq�v��פ��"
#define MSGTR_PREFERENCES_FontPropHeight "�ھڹq�v���פ��"
#define MSGTR_PREFERENCES_FontPropDiagonal "�ھڹq�v�﨤���"
#define MSGTR_PREFERENCES_FontEncoding "�s�X:"
#define MSGTR_PREFERENCES_FontBlur "�ҽk��:"
#define MSGTR_PREFERENCES_FontOutLine "����:"
#define MSGTR_PREFERENCES_FontTextScale "��r���:"
#define MSGTR_PREFERENCES_FontOSDScale "OSD ���:"
#define MSGTR_PREFERENCES_Cache "�֨��O�ж}/��"
#define MSGTR_PREFERENCES_CacheSize "�֨��O�Фj�p: "
#define MSGTR_PREFERENCES_LoadFullscreen "���ù��}�l"
#define MSGTR_PREFERENCES_SaveWinPos "�x�s������m"
#define MSGTR_PREFERENCES_XSCREENSAVER "���� XScreenSaver"
#define MSGTR_PREFERENCES_PlayBar "�ϥμ����"
#define MSGTR_PREFERENCES_AutoSync "�۰ʦP�B�}/��"
#define MSGTR_PREFERENCES_AutoSyncValue "�۰ʦP�B: "
#define MSGTR_PREFERENCES_CDROMDevice "CD-ROM �˸m:"
#define MSGTR_PREFERENCES_DVDDevice "DVD �˸m:"
#define MSGTR_PREFERENCES_FPS "�q�v�� FPS:"
#define MSGTR_PREFERENCES_ShowVideoWindow "�����D����ܼv������"

#define MSGTR_ABOUT_UHU "GUI �}�o�� UHU Linux �٧U\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "�P�R���~�T"
#define MSGTR_MSGBOX_LABEL_Error "���~�T"
#define MSGTR_MSGBOX_LABEL_Warning "ĵ�i�T"

#endif

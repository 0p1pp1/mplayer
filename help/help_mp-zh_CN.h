// Translated by Emfox Zhou <EmfoxZhou@gmail.com>

// (Translator before 2005-10-12)
// Lu Ran <hephooey@fastmail.fm>

// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"�÷�:   mplayer [ѡ��] [URL|·��/]�ļ���\n"
"\n"
"����ѡ��: (�����б�μ�manpage)\n"
" -vo <drv[:dev]> ѡ����Ƶ���ģʽ���豸(��'-vo help'�鿴�б�)\n"
" -ao <drv[:dev]> ѡ����Ƶ���ģʽ���豸(��'-ao help'�鿴�б�)\n"
#ifdef HAVE_VCD
" vcd://<trackno>  ����(S)VCD(Super Video CD)track(ԭʼ�豸, ����mount)\n"
#endif
#ifdef USE_DVDREAD
" dvd://<titleno>  ���豸��������ͨ�ļ��ϲ���DVD title\n"
" -alang/-slang   ѡ��DVD����/��Ļ������(ʹ����λ�Ĺ��Ҵ���)\n"
#endif
" -ss <timepos>   Ѱ��ָ����(�������hh:mm:ss)λ��\n"
" -nosound        ����������\n"
" -fs             ȫ������(����-vm, -zoom, ���man�ֲ�ҳ)\n"
" -x <x> -y <y>   ������ʾ�ķֱ���(�ṩ��-vm����-zoomʹ��)\n"
" -sub <file>     ָ��ʹ�õ���Ļ�ļ�(�μ�-subfps, -subdelay)\n"
" -playlist <file> ָ��ʹ�ò����б��ļ�\n"
" -vid x -aid y   ѡ�����ڲ��ŵ���Ƶ(x)����Ƶ(y)��\n"
" -fps x -srate y �ı���Ƶ(x fps)����Ƶ(y Hz)��\n"
" -pp <quality>   ʹ�ú��ڴ����˾�(���man�ֲ�ҳ)\n"
" -framedrop      ʹ�� ȥ֡(frame dropping) (����������)\n"
"\n"
"�������Ƽ�: (�������б�μ�manpage, ͬʱҲҪ���һ�� input.conf)\n"
" <-  or  ->      ���/��ǰ����10��\n"
" down or up      ���/��ǰ����1����\n"
" pgdown or pgup  ���/��ǰ����10����\n"
" < or >          ���������б��е�ǰһ��/��һ��\n"
" p or SPACE      ��ͣ����(�����������)\n"
" q or ESC        ֹͣ���Ų��˳�����\n"
" + or -          ������Ƶ�ӳ�+/-0.1��\n"
" o               ѭ��OSDģʽ:  none/seekbar/seekbar+timer\n"
" * or /          ���ӻ����pcm����\n"
" x or z          ������Ļ�ӳ�+/-0.1��\n"
" r or t          ��/�µ�����Ļλ��, �μ�-vf expand\n"
"\n"
" * * * ��ϸ���ݣ���һ��(�߼�)��ѡ��Ϳ��Ƽ��μ�MANPAGE��* * *\n"
"\n";
#endif

#define MSGTR_SamplesWanted "�����ʽ�Ĳ�����Ҫ�����õ�֧��. ����ϵ������.\n"

// ========================= MPlayer messages ===========================

// mplayer.c:

#define MSGTR_Exiting "\n�����˳�..\n"
#define MSGTR_ExitingHow "\n�����˳�... (%s)\n"
#define MSGTR_Exit_quit "�˳�"
#define MSGTR_Exit_eof "�ļ�����"
#define MSGTR_Exit_error "��������"
#define MSGTR_IntBySignal "\nMPlayer�� %s ģ���е� %d �ź��ж�\n"
#define MSGTR_NoHomeDir "�Ҳ���HOMEĿ¼\n"
#define MSGTR_GetpathProblem "get_path(\"config\")����\n"
#define MSGTR_CreatingCfgFile "����config�ļ�: %s\n"
#define MSGTR_InvalidAOdriver "��Ч����Ƶ�������: %s\n��'-vo help' t�鿴���õ���Ƶ�������б�\n"
#define MSGTR_CopyCodecsConf "(��etc/codecs.conf(��MPlayer��Դ������)����/���� ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "ʹ���ڽ�Ĭ�ϵ�codecs.conf.\n"
#define MSGTR_CantLoadFont "�޷���������: %s\n"
#define MSGTR_CantLoadSub "�޷�������Ļ: %s\n"
#define MSGTR_DumpSelectedStreamMissing "dump: ��������: ָ������������!\n"
#define MSGTR_CantOpenDumpfile "�޷���dump�ļ�.\n"
#define MSGTR_CoreDumped "core dumped :)\n"
#define MSGTR_FPSnotspecified "FPS���ļ�ͷ��û��ָ��(��������Ч����)! ��-fpsѡ��!\n"
#define MSGTR_TryForceAudioFmtStr "����ָ����Ƶ������������ %s...\n"
#define MSGTR_CantFindAudioCodec "�Ҳ�����Ƶ��ʽ 0x%X �Ľ�����.\n"
#define MSGTR_RTFMCodecs "�뿴DOCS/zh/codecs.html!\n"
#define MSGTR_TryForceVideoFmtStr "����ָ����Ƶ������������ %s...\n"
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
"�Ķ�DOCS/zh/video.html��DOCS/zh/sound.html��Ѱ�ҵ���/���ٵļ��ɡ�\n"\
"�����Щһ�����ò��ϣ��Ķ�DOCS/zh/bugreports.html��\n\n"

#define MSGTR_NoGui "MPlayerû�б���GUI��֧��!\n"
#define MSGTR_GuiNeedsX "MPlayer GUI��ҪX11!\n"
#define MSGTR_Playing "���� %s.\n"
#define MSGTR_NoSound "��Ƶ: no sound\n"
#define MSGTR_FPSforced "FPSָ��Ϊ %5.3f  (ftime: %5.3f).\n"
#define MSGTR_CompiledWithRuntimeDetection "����ʵʱCPU��� - ����, �ⲻ�����ѡ��! �����������ѵı���, ����--disable-runtime-cpudetectionѡ�����±���mplayer.\n"
#define MSGTR_CompiledWithCPUExtensions "�������չָ�x86 CPU����:"
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
#define MSGTR_ErrorInitializingVODevice "��/��ʼ����ѡ����Ƶ���(-vo)�豸�ǳ���!\n"
#define MSGTR_ForcedVideoCodec "ָ������Ƶ������: %s\n"
#define MSGTR_ForcedAudioCodec "ָ������Ƶ������: %s\n"
#define MSGTR_Video_NoVideo "��Ƶ: no video\n"
#define MSGTR_NotInitializeVOPorVO "\n��������: �޷���ʼ����Ƶ���(-vf)����Ƶ���(-vo)!\n"
#define MSGTR_Paused "\n  =====  ��ͣ  =====\r"
#define MSGTR_PlaylistLoadUnable "\n�޷�װ�ز����б� %s\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- ���Ƿ�ָ�����MPlayer������\n"\
"  ������������µ�����ʱCPU�������һ��bug...\n"\
"  ���Ķ�DOCS/zh/bugreports.html\n"
#define MSGTR_Exit_SIGILL \
"- ���Ƿ�ָ�����MPlayer������\n"\
"  ��ͨ�����������������/�Ż�MPlayer��ͬ��CPU��ʹ��\n"\
"  MPlayer��ɵ�\n"\
"  ���һ��!\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- ����ʹ��CPU/FPU/RAM����MPlayer����.\n"\
"  ʹ��--enable-debug���±���MPlayer�á�gdb��backtrace��\n"\
"  ����ࡣ����ϸ�ڿ�DOCS/zh/bugreports.html#crash.\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer�����ˡ��ⲻӦ�÷�����\n"\
"  �������MPlayer�����е� _����_ ��������е� _or_ ���gcc��\n"\
"  һ��bug��������������MPlayer�Ĵ����Ķ�DOCS/zh/bugreports.html\n"\
"  ����ѭ����Ĳ��衣���ǲ���Ҳ���������������ڱ���һ������bug��ʱ��\n"\
"  �ṩ����Ҫ����Ϣ��\n"
#define MSGTR_LoadingConfig "���ڵ��������ļ� '%s'\n"
#define MSGTR_AddedSubtitleFile "��Ļ: ������Ļ�ļ�(%d): %s\n"
#define MSGTR_RemovedSubtitleFile "��Ļ: ɾ����Ļ�ļ�(%d): %s\n"
#define MSGTR_ErrorOpeningOutputFile "���ļ�[%s]д��ʧ��!\n"
#define MSGTR_CommandLine "������: "
#define MSGTR_RTCDeviceNotOpenable "��%sʧ��: %s (���ļ�Ӧ�ÿɱ��û���ȡ.)\n"
#define MSGTR_LinuxRTCInitErrorIrqpSet "ͨ��ioctl����Linux RTC����(rtc_irqp_set %lu): %s\n"
#define MSGTR_IncreaseRTCMaxUserFreq "��ͼ����\"echo %lu > /proc/sys/dev/rtc/max-user-freq\"�����ϵͳ�����ű�.\n"
#define MSGTR_LinuxRTCInitErrorPieOn "ͨ��ioctl����Linux RTC����(rtc_pie_on): %s\n"
#define MSGTR_UsingTimingType "����ʹ��%s��ʱ.\n"
#define MSGTR_NoIdleAndGui "GMPLayer����ʹ��ѡ��-idle.\n"
#define MSGTR_MenuInitialized "�˵�������: %s\n"
#define MSGTR_MenuInitFailed "�˵�����ʧ��.\n"
#define MSGTR_Getch2InitializedTwice "����: getch2_init ����������!\n"
#define MSGTR_DumpstreamFdUnavailable "�޷�ת������� - û�п��õ�'fd'.\n"
#define MSGTR_FallingBackOnPlaylist "���˵����Ž��������б� %s...\n"
#define MSGTR_CantOpenLibmenuFilterWithThisRootMenu "�����ø��˵�%s��libmenu video filter.\n"
#define MSGTR_AudioFilterChainPreinitError "��Ƶ��������Ԥ��������!\n"
#define MSGTR_LinuxRTCReadError "Linux RTC��ȡ����: %s\n"
#define MSGTR_SoftsleepUnderflow "����! Softsleep �������!\n"
#define MSGTR_DvdnavNullEvent "DVDNAV�¼�Ϊ��?!\n"
#define MSGTR_DvdnavHighlightEventBroken "DVDNAV�¼�: �����¼���\n"
#define MSGTR_DvdnavEvent "DVDNAV�¼�: %s\n"
#define MSGTR_DvdnavHighlightHide "DVDNAV�¼�: ��������\n"
#define MSGTR_DvdnavStillFrame "######################################## DVDNAV�¼�: ��ֹ֡: %d��\n"
#define MSGTR_DvdnavNavStop "DVDNAV�¼�: Navֹͣ\n"
#define MSGTR_DvdnavNavNOP "DVDNAV�¼�: Nav�޲���\n"
#define MSGTR_DvdnavNavSpuStreamChangeVerbose "DVDNAV�¼�: Nav SPU���ı�: ����: %d/%d/%d �߼�: %d\n"
#define MSGTR_DvdnavNavSpuStreamChange "DVDNAV�¼�: Nav SPU���ı�: ����: %d �߼�: %d\n"
#define MSGTR_DvdnavNavAudioStreamChange "DVDNAV�¼�: Nav��Ƶ���ı�: ����: %d �߼�: %d\n"
#define MSGTR_DvdnavNavVTSChange "DVDNAV�¼�: Nav VTS�ı�\n"
#define MSGTR_DvdnavNavCellChange "DVDNAV�¼�: Nav Cell�ı�\n"
#define MSGTR_DvdnavNavSpuClutChange "DVDNAV�¼�: Nav SPU CLUT�ı�\n"
#define MSGTR_DvdnavNavSeekDone "DVDNAV�¼�: Nav��Ѱ���\n"
#define MSGTR_MenuCall "�˵�����\n"

#define MSGTR_EdlCantUseBothModes "����ͬʱʹ��-edl��-edlout.\n"
#define MSGTR_EdlOutOfMem "���ܷ����㹻���ڴ�������EDL����.\n"
#define MSGTR_EdlRecordsNo "����%d EDL����.\n"
#define MSGTR_EdlQueueEmpty "û��EDL����Ҫ����.\n"
#define MSGTR_EdlCantOpenForWrite "���ܴ�EDL�ļ�[%s]д��.\n"
#define MSGTR_EdlCantOpenForRead "���ܴ�[%s]����.\n"
#define MSGTR_EdlNOsh_video "û����Ƶ����ʹ��EDL, ȡ����.\n"
#define MSGTR_EdlNOValidLine "��ЧEDL��: %s\n"
#define MSGTR_EdlBadlyFormattedLine "�����ʽ��EDL��[%d]. ����.\n"
#define MSGTR_EdlBadLineOverlap "��һ�ε�ֹͣλ����[%f]; ��һ�ο�ʼ��"\
"[%f]. ÿһ����밴ʱ��˳��, �����ص�. ����.\n"
#define MSGTR_EdlBadLineBadStop "ֹͣʱ������ǿ�ʼʱ��֮��.\n"

// mencoder.c:

#define MSGTR_UsingPass3ControllFile "ʹ��pass3�����ļ�: %s\n"
#define MSGTR_MissingFilename "\nû���ļ���!\n\n"
#define MSGTR_CannotOpenFile_Device "�޷����ļ�/�豸\n"
#define MSGTR_CannotOpenDemuxer "�޷���demuxer\n"
#define MSGTR_NoAudioEncoderSelected "\nû��ѡ����Ƶ������(-oac)! ѡ��һ��(�ο�-oac help)����ʹ��-nosound.\n"
#define MSGTR_NoVideoEncoderSelected "\nû��ѡ����Ƶ������(-ovc)! ѡ��һ��(�ο�-ovc help).\n"
#define MSGTR_CannotOpenOutputFile "�޷�������ļ� '%s'\n"
#define MSGTR_EncoderOpenFailed "�޷��򿪱�����\n"
#define MSGTR_ForcingOutputFourcc "ָ�������fourccΪ %x [%.4s]\n"
#define MSGTR_ForcingOutputAudiofmtTag "ǿ�������Ƶ��ʽ��ǩ(tag) 0x%x\n"
#define MSGTR_WritingAVIHeader "����дAVI�ļ�ͷ...\n"
#define MSGTR_DuplicateFrames "\n�Ѹ��� %d ֡!\n"
#define MSGTR_SkipFrame "\n������һ֡!\n"
#define MSGTR_ResolutionDoesntMatch "\n�µ���Ƶ�ļ���ǰһ���Ľ����Ȼ�ɫ�ʿռ䲻ͬ.\n"
#define MSGTR_FrameCopyFileMismatch "\n���е���Ƶ�ļ�����Ҫ��ͬ����֡��, �����Ⱥͱ����������ʹ��-ovc copy.\n"
#define MSGTR_AudioCopyFileMismatch "\n���е���Ƶ�ļ�����Ҫ��ͬ������Ƶ��������͸�ʽ����ʹ��-oac copy.\n"
#define MSGTR_NoAudioFileMismatch "\n�޷���ֻ����Ƶ���ļ�����Ƶ��Ƶ�ļ����. ���� -nosound.\n"
#define MSGTR_NoSpeedWithFrameCopy "����: -speed����֤�ܺ�-oac copyһ����������!\n"\
"��ı������ʧ��!\n"
#define MSGTR_ErrorWritingFile "%s: д���ļ�����.\n"
#define MSGTR_WritingAVIIndex "\n����дAVI����...\n"
#define MSGTR_FixupAVIHeader "�޸�AVI�ļ�ͷ...\n"
#define MSGTR_RecommendedVideoBitrate "%s CD�Ƽ�����Ƶ������Ϊ: %d\n"
#define MSGTR_VideoStreamResult "\n��Ƶ��: %8.3f kbit/s  (%d B/s)  ��С: %d bytes  %5.3f secs  %d frames\n"
#define MSGTR_AudioStreamResult "\n��Ƶ��: %8.3f kbit/s  (%d B/s)  ��С: %d bytes  %5.3f secs\n"
#define MSGTR_OpenedStream "�ɹ�: ��ʽ: %d����: 0x%X - 0x%x\n"
#define MSGTR_VCodecFramecopy "��Ƶ�������: ֡���� (%dx%d %dbpp fourcc=%x)\n"
#define MSGTR_ACodecFramecopy "��Ƶ�������: ֡���� (format=%x chans=%d rate=%ld bits=%d B/s=%ld sample-%ld)\n"
#define MSGTR_CBRPCMAudioSelected "ѡ��CBR PCM��Ƶ\n"
#define MSGTR_MP3AudioSelected "ѡ��MP3��Ƶ\n"
#define MSGTR_CannotAllocateBytes "�޷�����%d�ֽ�\n"
#define MSGTR_SettingAudioDelay "������Ƶ�ӳ�Ϊ%5.3f\n"
#define MSGTR_SettingAudioInputGain "������Ƶ�������(gain)Ϊ%f\n"
#define MSGTR_LamePresetEquals "\npreset=%s\n\n"
#define MSGTR_LimitingAudioPreload "������ƵԤ��ֵΪ0.4s\n"
#define MSGTR_IncreasingAudioDensity "������Ƶ�ܶ�(density)Ϊ4\n"
#define MSGTR_ZeroingAudioPreloadAndMaxPtsCorrection "ǿ����ƵԤ��ֵΪ0, ���ptsУ��Ϊ0\n"
#define MSGTR_CBRAudioByterate "\n\nCBR��Ƶ: %ld�ֽ�/��, %d�ֽ�/��\n"
#define MSGTR_LameVersion "LAME�汾 %s (%s)\n\n"
#define MSGTR_InvalidBitrateForLamePreset "����: �����Ԥ��ֵ��ָ���ı����ʳ����Ϸ��ķ�Χ\n"\
"\n"\
"��ʹ������ģʽʱ��������һ����\"8\"��\"320\"֮�����ֵ\n"\
"\n"\
"������Ϣ��������: \"-lameopts preset=help\"\n"
#define MSGTR_InvalidLamePresetOptions "����: ��û�и���һ���Ϸ������û�Ԥ��ֵѡ��\n"\
"\n"\
"���õ�����(profile)����:\n"\
"\n"\
"   <fast>        standard\n"\
"   <fast>        extreme\n"\
"                 insane\n"\
"   <cbr> (ABR Mode) - ABRģʽ�������. Ҫʹ�����ѡ��,\n"\
"                      �򵥵�ָ��һ�������ʾ�����. ����:\n"\
"                      \"preset=185\"�Ϳ��Լ������\n"\
"                      Ԥ��ֵ��ʹ��185��Ϊƽ��������.\n"\
"\n"\
"    һЩ����:\n"\
"\n"\
"    \"-lameopts fast:preset=standard  \"\n"\
" or \"-lameopts  cbr:preset=192       \"\n"\
" or \"-lameopts      preset=172       \"\n"\
" or \"-lameopts      preset=extreme   \"\n"\
"\n"\
"������Ϣ��������: \"-lameopts preset=help\"\n"
#define MSGTR_LamePresetsLongInfo "\n"\
"Ԥ�迪�ر����Ϊ�ṩ��õ�Ʒ��.\n"\
"\n"\
"���ǵĴ�����Ĳ����Ѿ�ͨ���ϸ�� double blind listening �����������ͼ�������\n"\
"�Դﵽ����Ԥ�ڵ�Ŀ��.\n"\
"\n"\
"���ǲ��ϵر������Ա�����µķ�չ����һ�µĲ���,\n"\
"����Ӧ���ܸ����ṩ��ȻLAME�����ṩ�Ľ�����õ�Ʒ��.\n"\
"\n"\
"��������Ԥ��ֵ:\n"\
"\n"\
"   VBRģʽ(ͨ������µ����Ʒ��):\n"\
"\n"\
"     \"preset=standard\" ����Ԥ��ֵ��ȻӦ���Ǵ�������ڴ������������ֵ�ʱ��\n"\
  "                                   ��Ҫ�õ���ѡ��, ����Ʒ���Ѿ��ǳ��ߵ���.\n" \
"\n"\
"     \"preset=extreme\" ������м��õ��������൱���豸, ����Ԥ��ֵһ����\n"\
"                             \"standard\"ģʽ��Ʒ�ʻ�Ҫ���һ��.\n"\
"\n"\
"   CBR 320kbps(Ԥ�迪��ѡ������������):\n"\
"\n"\
"     \"preset=insane\"  ���ڴ�����˺��ڴ���������, ���ѡ��Ե���Щ������.\n"\
"                             ���������һ��Ҫ�����Ʒ�ʲ�����ȫ�������ļ���С,\n"\
"                             ���������ʺ����.\n"\
"\n"\
"   ABRģʽ(high quality per given bitrate but not as high as VBR):\n"\
"\n"\
"     \"preset=<kbps>\"  ʹ�����Ԥ��ֵ���ǻ���һ�������ı������в����Ʒ��.\n"\
"                             ��ָ��һ��ȷ���ı�����, Ԥ��ֵ�������������������ܴ�\n"\
"                             �������Ч��������. \n"\
"                             ��Ȼ���ַ����ǿ��Ե�, ������û��VBRģʽ��ô���, ͬ��\n"\
"                             һ��Ҳ���ܴﵽVBR�ڸ߱������µ�ͬ��Ʒ��. \n"\
"\n"\
"����ѡ����һ�µ������ļ��������Ҳ��ʹ��:\n"\
"\n"\
"   <fast>        standard\n"\
"   <fast>        extreme\n"\
"                 insane\n"\
"   <cbr> (ABR Mode) - ABRģʽ�������. Ҫʹ�����ѡ��,\n"\
"                      �򵥵�ָ��һ�������ʾ�����. ����:\n"\
"                      \"preset=185\"�Ϳ��Լ������\n"\
"                      Ԥ��ֵ��ʹ��185��Ϊƽ��������.\n"\
"\n"\
"   \"fast\" - ��һ���ض��������ļ������µĿ���VBRģʽ. �ٶ��л�\n"\
"            �Ļ����Ǿ����Եı�����Ҫ��һ������µ�Ҫ��, Ʒ��Ҳ��\n"\
"            ��һ���.\n"\
"      ����: �ڵ�ǰ�汾��, ����Ԥ��ֵ�����е��һ��ģʽƫ�ߵ�̫����.\n"\
"\n"\
"   \"cbr\"  - ��������ض�������ʹ��ABRģʽ(����), ����80,\n"\
"            96, 112, 128, 160, 192, 224, 256, 320, ���\n"\
"            ��ʹ��\"cbr\"ѡ����ǿ��ʹ��CBRģʽ�����Դ����׼\n"\
"            abrģʽ. ABR���ṩ���ߵ�Ʒ��, ����cbr���ܻ��õ�,\n"\
"            ĳЩ����±����internet����һ��mp3����ʱ�ͻ���\n"\
"            ����Ҫ��.\n"\
"\n"\
"    ����:\n"\
"\n"\
"    \"-lameopts fast:preset=standard  \"\n"\
" or \"-lameopts  cbr:preset=192       \"\n"\
" or \"-lameopts      preset=172       \"\n"\
" or \"-lameopts      preset=extreme   \"\n"\
"\n"\
"\n"\
"ABRģʽ�µ�һЩ���õı���(alias):\n"\
"phone => 16kbps/mono        phon+/lw/mw-eu/sw => 24kbps/mono\n"\
"mw-us => 40kbps/mono        voice => 56kbps/mono\n"\
"fm/radio/tape => 112kbps    hifi => 160kbps\n"\
"cd => 192kbps               studio => 256kbps"
#define MSGTR_LameCantInit "�޷��趨LAMEѡ��, ��������/������,"\
"һЩ�ǳ��͵ı�����(<32)��Ҫ�Ͳ�����(�� -srate 8000)."\
"���������, ����ʹ��Ԥ��ֵ."
#define MSGTR_ConfigfileError "�����ļ�����"
#define MSGTR_ErrorParsingCommandLine "���������д���"
#define MSGTR_VideoStreamRequired "��Ƶ���Ǳ����!\n"
#define MSGTR_ForcingInputFPS "����֡�ʽ���%5.2f����\n"
#define MSGTR_RawvideoDoesNotSupportAudio "����ļ���ʽRAWVIDEO��֧����Ƶ - ȡ����Ƶ\n"
#define MSGTR_DemuxerDoesntSupportNosound "���demuxer��ǰ����֧�� -nosound.\n"
#define MSGTR_MemAllocFailed "�ڴ����ʧ��"
#define MSGTR_NoMatchingFilter "û�ҵ�ƥ���filter/ao��ʽ!\n"
#define MSGTR_MP3WaveFormatSizeNot30 "sizeof(MPEGLAYER3WAVEFORMAT)==%d!=30, C����������?\n"
#define MSGTR_NoLavcAudioCodecName "��ƵLAVC, û�б��������!\n"
#define MSGTR_LavcAudioCodecNotFound "��ƵLAVC, �޷��ҵ���Ӧ�ı����� %s\n"
#define MSGTR_CouldntAllocateLavcContext "��ƵLAVC, �޷�����������!\n"
#define MSGTR_CouldntOpenCodec "�޷��򿪱������ %s, br=%d\n"
#define MSGTR_CantCopyAudioFormat "��Ƶ��ʽ0x%x��'-oac copy'������, ��������'-oac pcm'����'-fafmttag'������������.\n"

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
"               Ҳ���ں��ABRԤ��ģʽ��ǿ��ʹ��CBRģʽ.\n"\
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

//codec-cfg.c:
#define MSGTR_DuplicateFourcc "�ظ���FourCC"
#define MSGTR_TooManyFourccs "̫���FourCCs/formats..."
#define MSGTR_ParseError "��������"
#define MSGTR_ParseErrorFIDNotNumber "��������(��ʽID����һ������?)"
#define MSGTR_ParseErrorFIDAliasNotNumber "��������(��ʽID�ǳ�(alias)����һ������?)"
#define MSGTR_DuplicateFID "�ظ��ĸ�ʽID"
#define MSGTR_TooManyOut "̫�����..."
#define MSGTR_InvalidCodecName "\n�������(%s)�����Ϸ�!\n"
#define MSGTR_CodecLacksFourcc "\n�������(%s)û��FourCC/format!\n"
#define MSGTR_CodecLacksDriver "\n�������(%s)û������!\n"
#define MSGTR_CodecNeedsDLL "\n�������(%s)��Ҫһ��'dll'!\n"
#define MSGTR_CodecNeedsOutfmt "\n�������(%s)��Ҫһ��'outfmt'!\n"
#define MSGTR_CantAllocateComment "����Ϊע�ͷ����ڴ�."
#define MSGTR_GetTokenMaxNotLessThanMAX_NR_TOKEN "get_token(): max >= MAX_MR_TOKEN!"
#define MSGTR_ReadingFile "���� %s: "
#define MSGTR_CantOpenFileError "�޷��� '%s': %s\n"
#define MSGTR_CantGetMemoryForLine "�޷�Ϊ 'line' ��ȡ�ڴ�: %s\n"
#define MSGTR_CantReallocCodecsp "�޷����·��� '*codecsp': %s\n"
#define MSGTR_CodecNameNotUnique "��������� '%s' ��Ψһ."
#define MSGTR_CantStrdupName "���� strdup -> 'name': %s\n"
#define MSGTR_CantStrdupInfo "���� strdup -> 'info': %s\n"
#define MSGTR_CantStrdupDriver "���� strdup -> 'driver': %s\n"
#define MSGTR_CantStrdupDLL "���� strdup -> 'dll': %s"
#define MSGTR_AudioVideoCodecTotals "%d ��Ƶ�� %d ��Ƶ�������\n"
#define MSGTR_CodecDefinitionIncorrect "�������û����ȷ����."
#define MSGTR_OutdatedCodecsConf "���codecs.conf̫�ϣ��뵱ǰ��MPlayer������!"

// divx4_vbr.c:
#define MSGTR_OutOfMemory "�ڴ����"
#define MSGTR_OverridingTooLowBitrate "ָ���ı����ʶ����˼���(clip)��˵̫����.\n"\
"�����������˵��С�ı������� %.0f kbps. �Դ����\n"\
"�û�ָ����ֵ.\n"

// fifo.c
#define MSGTR_CannotMakePipe "���ܽ���PIPE!\n"

// m_config.c
#define MSGTR_InvalidCfgfileOption "ѡ��%s�����������ļ���ʹ��.\n"
#define MSGTR_InvalidCmdlineOption "ѡ��%s����������ѡ��ʹ��.\n"
#define MSGTR_InvalidSuboption "����: ѡ��'%s'û����ѡ��'%s'.\n"
#define MSGTR_MissingSuboptionParameter "����: ��ѡ��'%s'(����ѡ��'%s')����Ҫ��һ������!\n"
#define MSGTR_MissingOptionParameter "����: ѡ��'%s'����Ҫ��һ������!\n"
#define MSGTR_OptionListHeader "\n ����                 ����            ��С       ���     ȫ��  ������ �����ļ�\n\n"
#define MSGTR_TotalOptions "\n�ܹ�: %d��ѡ��\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "�Ҳ���CD-ROM�豸 '%s'!\n"
#define MSGTR_ErrTrackSelect "ѡ��VCD track����!"
#define MSGTR_ReadSTDIN "��stdin��ȡ...\n"
#define MSGTR_UnableOpenURL "�޷���URL: %s\n"
#define MSGTR_ConnToServer "���ӵ�������: %s\n"
#define MSGTR_FileNotFound "�Ҳ����ļ�: '%s'\n"

#define MSGTR_SMBInitError "�޷���ʼ��libsmbclient��: %d\n"
#define MSGTR_SMBFileNotFound "�޷��򿪾������ڵ�: '%s'\n"
#define MSGTR_SMBNotCompiled "MPlayerû�б���SMB��ȡ��֧��.\n"

#define MSGTR_CantOpenDVD "�޷���DVD �豸: %s\n"
#define MSGTR_NoDVDSupport "MPlayer �Ǳ�����ɲ���DVD֧�ֵģ��˳�\n"
#define MSGTR_DVDwait "��ȡ���̽ṹ, ��ȴ�...\n"
#define MSGTR_DVDnumTitles "����DVD�� %d ��titles.\n"
#define MSGTR_DVDinvalidTitle "��Ч��DVD title��: %d\n"
#define MSGTR_DVDnumChapters "��� DVD title�� %d chapters.\n"
#define MSGTR_DVDinvalidChapter "��Ч��DVD chapter��: %d\n"
#define MSGTR_DVDinvalidChapterRange "��Ч�� chapter ��Χ %s\n"
#define MSGTR_DVDinvalidLastChapter "��Ч�� DVD ��� chapter ��: %d\n"
#define MSGTR_DVDnumAngles "��� DVD title�� %d ���ӽ�.\n"
#define MSGTR_DVDinvalidAngle "��Ч��DVD�ӽǺ�: %d\n"
#define MSGTR_DVDnoIFO "�޷��� DVD title %d ��IFO�ļ�.\n"
#define MSGTR_DVDnoVMG "�޷��� VMG ��Ϣ!\n"
#define MSGTR_DVDnoVOBs "�޷���title��VOB(VTS_%02d_1.VOB).\n"
#define MSGTR_DVDnoMatchingAudio "û���ҵ�ƥ��� DVD ��Ƶ����!\n"
#define MSGTR_DVDaudioChannel "ѡ�� DVD ��Ƶͨ��: %d ����: %c%c\n"
#define MSGTR_DVDnoMatchingSubtitle "û���ҵ�ƥ��� DVD ��Ļ����!\n"
#define MSGTR_DVDsubtitleChannel "ѡ�� DVD ��Ļͨ��: %d ����: %c%c\n"
#define MSGTR_DVDopenOk "DVD�ɹ���!\n"

// muxer_*.c:
#define MSGTR_TooManyStreams "̫�����!"
#define MSGTR_RawMuxerOnlyOneStream "Rawaudio muxer ֻ֧��һ����Ƶ��!\n"
#define MSGTR_IgnoringVideoStream "������Ƶ��!\n"
#define MSGTR_UnknownStreamType "����! δ֪��������: %d\n"
#define MSGTR_WarningLenIsntDivisible "����! ���Ȳ��ܱ�����������!\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "����! ��Ƶ��ͷ�� %d �����¶���.\n"
#define MSGTR_VideoStreamRedefined "����! ��Ƶ��ͷ�� %d �����¶���.\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: buffer����Ƶ��̫��(%d in %d bytes)!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: buffer����Ƶ��̫��(%d in %d bytes)!\n"
#define MSGTR_MaybeNI "(Ҳ���㲥����һ���ǽ������/�ļ������ǽ���ʧ��)?\n" \
		      "����AVI�ļ�, ������-niѡ��ָ���ǽ���ģʽ.\n"
#define MSGTR_SwitchToNi "\n��⵽���Ľ����ʽ��AVI - �л���-niģʽ...\n"
#define MSGTR_Detected_XXX_FileFormat "��⵽%s�ļ���ʽ��\n"
#define MSGTR_DetectedAudiofile "��⵽��Ƶ�ļ�!\n"
#define MSGTR_NotSystemStream "��MPEGϵͳ������ʽ... (������������?)\n"
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
#define MSGTR_CouldntDetFNo "�޷�����֡��(���ھ�������).\n"
#define MSGTR_CantSeekRawAVI "�޷��ڲ�������.AVI��������. (��Ҫ����, ����ʹ��-idx ѡ��!)  \n"
#define MSGTR_CantSeekFile "�޷�������ļ�������.  \n"

#define MSGTR_EncryptedVOB "���ܵ�VOB�ļ�! �Ķ�DOCS/zh/cd-dvd.html.\n"

#define MSGTR_MOVcomprhdr "MOV: ѹ�����ļ�ͷ��֧����ҪZLIB!\n"
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
#define MSGTR_DemuxerInfoAlreadyPresent "Demuxer info %s �Ѿ���ʾ!\n"
#define MSGTR_ClipInfo "Clip info: \n"
#define MSGTR_LeaveTelecineMode "\ndemux_mpg: ��⵽30fps��NTSC����, �ı�֡����.\n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: ��⵽24fps������NTSC����, �ı�֡����.\n"

#define MSGTR_CacheFill "\r�������: %5.2f%% (%d �ֽ�)   "
#define MSGTR_NoBindFound "û���ҵ��� '%s' �ļ���"
#define MSGTR_FailedToOpen "�� %s ʧ��\n"

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

#define MSGTR_ShMemAllocFail "�޷����乲���ڴ�.\n"
#define MSGTR_CantAllocAudioBuf "�޷�������Ƶ���buffer.\n"

#define MSGTR_UnknownAudio "δ֪��ȱ����Ƶ��ʽ, ʹ��nosound\n"

#define MSGTR_UsingExternalPP "[PP] ʹ���ⲿ�ĺ�����, max q = %d\n"
#define MSGTR_UsingCodecPP "[PP] ʹ�ý������ĺ�����, max q = %d\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "��ѡ��vo & vd��֧����Ƶ����'%s'. \n"
#define MSGTR_VideoCodecFamilyNotAvailableStr "Ҫ�����Ƶ�������� [%s] (vfm=%s) ������.\n�ڱ���ʱ������.\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "Ҫ�����Ƶ�������� [%s] (afm=%s) ������.\n�ڱ���ʱ������.\n"
#define MSGTR_OpeningVideoDecoder "����Ƶ������: [%s] %s\n"
#define MSGTR_SelectedVideoCodec "ѡ����Ƶ�������: [%s] vfm: %s (%s)\n"
#define MSGTR_OpeningAudioDecoder "����Ƶ������: [%s] %s\n"
#define MSGTR_SelectedAudioCodec "ѡ����Ƶ�������: [%s] afm: %s (%s)\n"
#define MSGTR_BuildingAudioFilterChain "Ϊ %dHz/%dch/%s -> %dHz/%dch/%s ������Ƶ������...\n"
#define MSGTR_UninitVideoStr "�ر���Ƶ: %s  \n"
#define MSGTR_UninitAudioStr "�ر���Ƶ: %s  \n"
#define MSGTR_VDecoderInitFailed "VDecoder��ʼ��ʧ�� :(\n"
#define MSGTR_ADecoderInitFailed "ADecoder��ʼ��ʧ�� :(\n"
#define MSGTR_ADecoderPreinitFailed "ADecoderԤ��ʼ��ʧ�� :(\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: Ϊ���뻺����� %d �ֽ�.\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: Ϊ���������� %d + %d = %d �ֽ�.\n"

// LIRC:
#define MSGTR_SettingUpLIRC "�𶯺���ң��֧��...\n"
#define MSGTR_LIRCdisabled "�㽫�޷�ʹ�����ң����\n"
#define MSGTR_LIRCopenfailed "����ң��֧����ʧ��!\n"
#define MSGTR_LIRCcfgerr "��ȡLIRC�����ļ� %s ʧ��!\n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "�Ҳ�����Ƶ�˾� '%s'.\n"
#define MSGTR_CouldNotOpenVideoFilter "�޷�����Ƶ�˾� '%s'.\n"
#define MSGTR_OpeningVideoFilter "����Ƶ�˾�: "
#define MSGTR_CannotFindColorspace "�޷��ҵ����ʵ�ɫ�ʿռ�, ����������'scale'Ҳ���� :(\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDec: �������޷�����sh->disp_w��sh->disp_h, �����ƹ�!\n"
#define MSGTR_VoConfigRequest "VDec: vo����Ҫ�� - %d x %d (ѡ��ɫ�ʿռ�: %s)\n"
#define MSGTR_CouldNotFindColorspace "�޷��ҵ�ƥ���ɫ�ʿռ� - ���³��� -vf scale...\n"
#define MSGTR_MovieAspectIsSet "��Ӱ��߱�Ϊ %.2f:1 - Ԥ�Ŵ���ȷ�ĵ�Ӱ��߱�.\n"
#define MSGTR_MovieAspectUndefined "��Ӱ��߱�δ���� - �޷�ʹ��Ԥ�Ŵ�.\n"

// vd_dshow.c, vd_dmo.c
#define MSGTR_DownloadCodecPackage "����Ҫ����/��װ�����Ʊ��������.\n�����http://mplayerhq.hu/homepage/dload.html\n"
#define MSGTR_DShowInitOK "INFO: Win32/DShow��Ƶ��������ʼ��OK.\n"
#define MSGTR_DMOInitOK "INFO: Win32/DMO��Ƶ��������ʼ��OK.\n"

// x11_common.c
#define MSGTR_EwmhFullscreenStateFailed "\nX11: ���ܷ���EWMHȫ���¼�!\n"
#define MSGTR_CouldNotFindXScreenSaver "xscreensaver_disable: �Ҳ�����Ļ�����Ĵ���.\n"
#define MSGTR_SelectedVideoMode "XF86VM: ѡ����Ƶģʽ %dx%d (ͼ���С %dx%d).\n"

#define MSGTR_InsertingAfVolume "[������] û��Ӳ������, ��������������.\n"
#define MSGTR_NoVolume "[������] û�п��õ���������.\n"

// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "����"
#define MSGTR_FileSelect "ѡ���ļ�..."
#define MSGTR_SubtitleSelect "ѡ����Ļ..."
#define MSGTR_OtherSelect "ѡ��..."
#define MSGTR_AudioFileSelect "ѡ���ⲿ��Ƶ���..."
#define MSGTR_FontSelect "ѡ������..."
// Note: If you change MSGTR_PlayList please see if it still fits MSGTR_MENU_PlayList
#define MSGTR_PlayList "�����б�"
#define MSGTR_Equalizer "������"
#define MSGTR_SkinBrowser "Skin�����"
#define MSGTR_Network "������ý��..."
// Note: If you change MSGTR_Preferences please see if it still fits MSGTR_MENU_Preferences
#define MSGTR_Preferences "��������"
#define MSGTR_AudioPreferences "��Ƶ��������"
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
#define MSGTR_UNKNOWNWINDOWTYPE "����δ֪�������� ..."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[skin] skin�����ļ��� %d: %s�г���"
#define MSGTR_SKIN_WARNING1 "[skin] ����, �������ļ��� %d��:\n�ҵ�widget������֮ǰû���ҵ�\"section\" (%s)"
#define MSGTR_SKIN_WARNING2 "[skin] ����, �������ļ��� %d��:\n�ҵ�widget������֮ǰû���ҵ� \"subsection\" (%s) "
#define MSGTR_SKIN_WARNING3 "[skin] ����, �������ļ��� %d��:\n���widget��֧�����subsection(%s)"
#define MSGTR_SKIN_SkinFileNotFound "[skin] �ļ�( %s )û�ҵ�.\n"
#define MSGTR_SKIN_SkinFileNotReadable "[skin] �ļ�( %s )���ɶ�.\n"
#define MSGTR_SKIN_BITMAP_16bit  "��֧������16 bitsɫ���λͼ(%s).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "�Ҳ����ļ�(%s)\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "BMP��ȡ����(%s)\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "TGA��ȡ����(%s)\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "PNG��ȡ����(%s)\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "��֧��RLE��ʽѹ����TGA(%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "δ֪���ļ���ʽ(%s)\n"
#define MSGTR_SKIN_BITMAP_ConvertError "24 bit��32 bit��ת����������(%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "δ֪��Ϣ: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "û���㹻�ڴ�\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "������̫������.\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "�Ҳ��������ļ�.\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "�Ҳ�������ͼ���ļ�.\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "�����ڵ������ǩ( %s )\n"
#define MSGTR_SKIN_UnknownParameter "δ֪����( %s )\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "�Ҳ���skin( %s ).\n"
#define MSGTR_SKIN_SKINCFG_SelectedSkinNotFound "ѡ����skin( %s )û�ҵ�, ����ʹ��'default'...\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "Skin�����ļ�( %s )��ȡ����.\n"
#define MSGTR_SKIN_LABEL "Skins:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "����MPlayer"
#define MSGTR_MENU_Open "��..."
#define MSGTR_MENU_PlayFile "�����ļ�..."
#define MSGTR_MENU_PlayVCD "����VCD..."
#define MSGTR_MENU_PlayDVD "����DVD..."
#define MSGTR_MENU_PlayURL "����URL..."
#define MSGTR_MENU_LoadSubtitle "������Ļ..."
#define MSGTR_MENU_DropSubtitle "������Ļ..."
#define MSGTR_MENU_LoadExternAudioFile "�����ⲿ��Ƶ�ļ�..."
#define MSGTR_MENU_Playing "���ſ���"
#define MSGTR_MENU_Play "����"
#define MSGTR_MENU_Pause "��ͣ"
#define MSGTR_MENU_Stop "ֹͣ"
#define MSGTR_MENU_NextStream "��һ��"
#define MSGTR_MENU_PrevStream "��һ��"
#define MSGTR_MENU_Size "��С"
#define MSGTR_MENU_HalfSize   "һ���С"
#define MSGTR_MENU_NormalSize "������С"
#define MSGTR_MENU_DoubleSize "˫����С"
#define MSGTR_MENU_FullScreen "ȫ��"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "�򿪵�Ƭ..."
#define MSGTR_MENU_ShowDVDMenu "��ʾDVD�˵�"
#define MSGTR_MENU_Titles "Titles"
#define MSGTR_MENU_Title "Title %2d"
#define MSGTR_MENU_None "(none)"
#define MSGTR_MENU_Chapters "Chapters"
#define MSGTR_MENU_Chapter "Chapter %2d"
#define MSGTR_MENU_AudioLanguages "��Ƶ����"
#define MSGTR_MENU_SubtitleLanguages "��Ļ����"
#define MSGTR_MENU_PlayList MSGTR_PlayList
#define MSGTR_MENU_SkinBrowser "Skin�����"
#define MSGTR_MENU_Preferences MSGTR_Preferences
#define MSGTR_MENU_Exit "�˳�..."
#define MSGTR_MENU_Mute "����"
#define MSGTR_MENU_Original "ԭʼ��"
#define MSGTR_MENU_AspectRatio "�����"
#define MSGTR_MENU_AudioTrack "��Ƶ���"
#define MSGTR_MENU_Track "��� %d"
#define MSGTR_MENU_VideoTrack "��Ƶ���"

// --- equalizer
// Note: If you change MSGTR_EQU_Audio please see if it still fits MSGTR_PREFERENCES_Audio
#define MSGTR_EQU_Audio "��Ƶ"
// Note: If you change MSGTR_EQU_Video please see if it still fits MSGTR_PREFERENCES_Video
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
#define MSGTR_PREFERENCES_Audio MSGTR_EQU_Audio
#define MSGTR_PREFERENCES_Video MSGTR_EQU_Video
#define MSGTR_PREFERENCES_SubtitleOSD "��Ļ��OSD"
#define MSGTR_PREFERENCES_Codecs "Codecs��demuxer"
// Note: If you change MSGTR_PREFERENCES_Misc see if it still fits MSGTR_PREFERENCES_FRAME_Misc
#define MSGTR_PREFERENCES_Misc "����"

#define MSGTR_PREFERENCES_None "None"
#define MSGTR_PREFERENCES_DriverDefault "Ĭ������"
#define MSGTR_PREFERENCES_AvailableDrivers "��������:"
#define MSGTR_PREFERENCES_DoNotPlaySound "����������"
#define MSGTR_PREFERENCES_NormalizeSound "������׼��"
#define MSGTR_PREFERENCES_EnEqualizer "����������"
#define MSGTR_PREFERENCES_SoftwareMixer "�������������"
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
#define MSGTR_PREFERENCES_SUB_MPSUB "��������Ļת��ΪMPlayer����Ļ�ļ�"
#define MSGTR_PREFERENCES_SUB_SRT "��������Ļת��Ϊ����ʱ���SubViewer(SRT) ��ʽ"
#define MSGTR_PREFERENCES_SUB_Overlap "������Ļ�ص�"
#define MSGTR_PREFERENCES_Font "����:"
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
#define MSGTR_PREFERENCES_FRAME_Misc MSGTR_PREFERENCES_Misc
#define MSGTR_PREFERENCES_Audio_Device "�豸:"
#define MSGTR_PREFERENCES_Audio_Mixer "������:"
#define MSGTR_PREFERENCES_Audio_MixerChannel "����ͨ��:"
#define MSGTR_PREFERENCES_Message "���ס, ��Щ����ֻ�����²��ź����Ч��."
#define MSGTR_PREFERENCES_DXR3_VENC "��Ƶ������:"
#define MSGTR_PREFERENCES_DXR3_LAVC "ʹ��LAVC(FFmpeg)"
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
#define MSGTR_PREFERENCES_ArtsBroken "�µ�aRts�汾��GTK 1.x������,"\
           "��ʹGMPlayer����!"

#define MSGTR_ABOUT_UHU "GUI������UHU Linux����\n"
#define MSGTR_ABOUT_CoreTeam "   MPlayer����С��:\n"
#define MSGTR_ABOUT_AdditionalCoders "   ����������:\n"
#define MSGTR_ABOUT_MainTesters "   ��Ҫ������:\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "��������!"
#define MSGTR_MSGBOX_LABEL_Error "����!"
#define MSGTR_MSGBOX_LABEL_Warning "����!"

// bitmap.c

#define MSGTR_NotEnoughMemoryC32To1 "[c32to1] �ڴ治��, �ݲ���ͼƬ\n"
#define MSGTR_NotEnoughMemoryC1To32 "[c1to32] �ڴ治��, �ݲ���ͼƬ\n"

// cfg.c

#define MSGTR_ConfigFileReadError "[cfg] �������ļ�����...\n"
#define MSGTR_UnableToSaveOption "[cfg] �޷�����'%s'ѡ��.\n"

// interface.c

#define MSGTR_DeletingSubtitles "[GUI] ɾ����Ļ.\n"
#define MSGTR_LoadingSubtitles "[GUI] ������Ļ: %s\n"
#define MSGTR_AddingVideoFilter "[GUI] ������Ƶ������: %s\n"
#define MSGTR_RemovingVideoFilter "[GUI] ɾ����Ƶ������: %s\n"

// mw.c

#define MSGTR_NotAFile "�⿴����������һ���ļ�: %s !\n"

// ws.c

#define MSGTR_WS_CouldNotOpenDisplay "[ws] �޷���display.\n"
#define MSGTR_WS_RemoteDisplay "[ws] Զ��display, ȡ��XMITSHM.\n"
#define MSGTR_WS_NoXshm "[ws] ��Ǹ, ���ϵͳ��֧��X�����ڴ���չ.\n"
#define MSGTR_WS_NoXshape "[ws] ��Ǹ, ���ϵͳ��֧��XShape��չ.\n"
#define MSGTR_WS_ColorDepthTooLow "[ws] ��Ǹ, ɫ�����̫����.\n"
#define MSGTR_WS_TooManyOpenWindows "[ws] �򿪵Ĵ���̫����.\n"
#define MSGTR_WS_ShmError "[ws] �����ڴ���չ����\n"
#define MSGTR_WS_NotEnoughMemoryDrawBuffer "[ws] ��Ǹ, �ڴ治����д����(draw buffer).\n"
#define MSGTR_WS_DpmsUnavailable "DPMS������?\n"
#define MSGTR_WS_DpmsNotEnabled "��������DPMS.\n"

// wsxdnd.c

#define MSGTR_WS_NotAFile "�⿴����������һ���ļ�...\n"
#define MSGTR_WS_DDNothing "D&D: û���κζ�������!\n"

#endif

// ======================= VO Video Output drivers ========================

#define MSGTR_VOincompCodec "ѡ������Ƶ����豸������������������.\n"\
                "���ż������Ź�����, ������ -vf spp,scale ������ -vf spp.\n"
#define MSGTR_VO_GenericError "��������Ѿ�����"
#define MSGTR_VO_UnableToAccess "�޷�����"
#define MSGTR_VO_ExistsButNoDirectory "�Ѿ�����, ������һ��Ŀ¼."
#define MSGTR_VO_DirExistsButNotWritable "���Ŀ¼�Ѿ�����, ���ǲ���д."
#define MSGTR_VO_DirExistsAndIsWritable "���Ŀ¼�Ѿ����ڲ��ҿ�д."
#define MSGTR_VO_CantCreateDirectory "�޷��������Ŀ¼."
#define MSGTR_VO_CantCreateFile "�޷���������ļ�."
#define MSGTR_VO_DirectoryCreateSuccess "���Ŀ¼�ɹ�����."
#define MSGTR_VO_ParsingSuboptions "������ѡ��."
#define MSGTR_VO_SuboptionsParsedOK "��ѡ������ɹ�."
#define MSGTR_VO_ValueOutOfRange "ֵ������Χ"
#define MSGTR_VO_NoValueSpecified "û��ָ��ֵ."
#define MSGTR_VO_UnknownSuboptions "δ֪��ѡ��"

// vo_aa.c

#define MSGTR_VO_AA_HelpHeader "\n\n������ aalib vo_aa ����ѡ��:\n"
#define MSGTR_VO_AA_AdditionalOptions "vo_aa �ṩ�ĸ���ѡ��:\n" \
"  help        ��ʾ�˰�����Ϣ\n" \
"  osdcolor    �趨osd��ɫ\n  subcolor    �趨��Ļ��ɫ\n" \
"        ��ɫ������:\n           0 : һ��\n" \
"           1 : ģ��\n           2 : ��\n           3 : ������\n" \
"           4 : ��ɫ\n           5 : ����\n\n\n"

// vo_jpeg.c
#define MSGTR_VO_JPEG_ProgressiveJPEG "����Progressive JPEG."
#define MSGTR_VO_JPEG_NoProgressiveJPEG "ȡ��Progressive JPEG."
#define MSGTR_VO_JPEG_BaselineJPEG "����Baseline JPEG."
#define MSGTR_VO_JPEG_NoBaselineJPEG "ȡ��Baseline JPEG."

// vo_pnm.c
#define MSGTR_VO_PNM_ASCIIMode "����ASCIIģʽ."
#define MSGTR_VO_PNM_RawMode "����Rawģʽ."
#define MSGTR_VO_PNM_PPMType "��Ҫд��PPM�ļ�."
#define MSGTR_VO_PNM_PGMType "��Ҫд��PGM�ļ�."
#define MSGTR_VO_PNM_PGMYUVType "��Ҫд��PGMYUV�ļ�."

// vo_yuv4mpeg.c
#define MSGTR_VO_YUV4MPEG_InterlacedHeightDivisibleBy4 "����ģʽ��Ҫͼ��߶��ܱ�4����."
#define MSGTR_VO_YUV4MPEG_InterlacedLineBufAllocFail "�޷�Ϊ����ģʽ�����߻���."
#define MSGTR_VO_YUV4MPEG_InterlacedInputNotRGB "���벻��RGB, ���ܰ���ֿ�ɫѶ!"
#define MSGTR_VO_YUV4MPEG_WidthDivisibleBy2 "ͼ���ȱ����ܱ�2����."
#define MSGTR_VO_YUV4MPEG_NoMemRGBFrameBuf "�ڴ治��, ���ܷ���RGB����."
#define MSGTR_VO_YUV4MPEG_OutFileOpenError "���ܵõ��ڴ���ļ������д��\"%s\"!"
#define MSGTR_VO_YUV4MPEG_OutFileWriteError "дͼ���������!"
#define MSGTR_VO_YUV4MPEG_UnknownSubDev "δ֪�����豸: %s"
#define MSGTR_VO_YUV4MPEG_InterlacedTFFMode "ʹ�ý������ģʽ(�ϲ�����ǰ)."
#define MSGTR_VO_YUV4MPEG_InterlacedBFFMode "ʹ�ý������ģʽ(�²�����ǰ)."
#define MSGTR_VO_YUV4MPEG_ProgressiveMode "ʹ��(Ĭ��)progressive֡ģʽ."

// Old vo drivers that have been replaced

#define MSGTR_VO_PGM_HasBeenReplaced "Pgm��Ƶ��������Ѿ��� -vo pnm:pgmyuv ����.\n"
#define MSGTR_VO_MD5_HasBeenReplaced "Md5��Ƶ��������Ѿ��� -vo md5sum ����.\n"

// ======================= AO Audio Output drivers ========================

// libao2

// audio_out.c
#define MSGTR_AO_ALSA9_1x_Removed "audio_out: alsa9��alsa1xģ���Ѿ���ɾ��,���� -ao alsa ����.\n"

// ao_oss.c
#define MSGTR_AO_OSS_CantOpenMixer "[AO OSS] audio_setup: �޷��򿪻������豸 %s: %s\n"
#define MSGTR_AO_OSS_ChanNotFound "[AO OSS] audio_setup: ����������û��'%s', ʹ��Ĭ��ͨ��.\n"
#define MSGTR_AO_OSS_CantOpenDev "[AO OSS] audio_setup: �޷�����Ƶ�豸 %s: %s\n"
#define MSGTR_AO_OSS_CantMakeFd "[AO OSS] audio_setup: �޷������ļ�������: %s\n"
#define MSGTR_AO_OSS_CantSet "[AO OSS] �޷��趨��Ƶ�豸 %s �� %s �����, ����ʹ�� %s...\n"
#define MSGTR_AO_OSS_CantSetChans "[AO OSS] audio_setup: �趨��Ƶ�豸�� %d ͨ��ʧ��.\n"
#define MSGTR_AO_OSS_CantUseGetospace "[AO OSS] audio_setup: ������֧�� SNDCTL_DSP_GETOSPACE :-(\n"
#define MSGTR_AO_OSS_CantUseSelect "[AO OSS]\n   ***  �����Ƶ������֧�� select()  ***\n ���� #undef HAVE_AUDIO_SELECT in config.h �ر���MPlayer!\n\n"
#define MSGTR_AO_OSS_CantReopen "[AO OSS]\n���ش���: *** �޷����´򿪻�������Ƶ�豸 *** %s\n"

// ao_arts.c
#define MSGTR_AO_ARTS_CantInit "[AO ARTS] %s\n"
#define MSGTR_AO_ARTS_ServerConnect "[AO ARTS] �����ӵ������豸.\n"
#define MSGTR_AO_ARTS_CantOpenStream "[AO ARTS] �޷���һ����.\n"
#define MSGTR_AO_ARTS_StreamOpen "[AO ARTS] ���Ѿ���.\n"
#define MSGTR_AO_ARTS_BufferSize "[AO ARTS] �����С: %d\n"

// ao_dxr2.c
#define MSGTR_AO_DXR2_SetVolFailed "[AO DXR2] �趨����Ϊ %d ʧ�� .\n"
#define MSGTR_AO_DXR2_UnsupSamplerate "[AO DXR2] dxr2: ��֧�� %d Hz, ������ \"-aop list=resample\"\n"

// ao_esd.c
#define MSGTR_AO_ESD_CantOpenSound "[AO ESD] esd_open_sound ʧ��: %s\n"
#define MSGTR_AO_ESD_LatencyInfo "[AO ESD] �ӳ�: [server: %0.2fs, net: %0.2fs] (adjust %0.2fs)\n"
#define MSGTR_AO_ESD_CantOpenPBStream "[AO ESD] �� esd ������ʧ��: %s\n"

// ao_mpegpes.c
#define MSGTR_AO_MPEGPES_CantSetMixer "[AO MPEGPES] DVB ��Ƶ���û���������: %s\n"
#define MSGTR_AO_MPEGPES_UnsupSamplerate "[AO MPEGPES] ��֧�� %d Hz, �����ز���...\n"

// ao_null.c
// This one desn't even  have any mp_msg nor printf's?? [CHECK]

// ao_pcm.c
#define MSGTR_AO_PCM_FileInfo "[AO PCM] �ļ�: %s (%s)\nPCM: ������: %iHz ͨ��: %s ��ʽ %s\n"
#define MSGTR_AO_PCM_HintInfo "[AO PCM] ��Ϣ: �� -vc null -vo null ���Դﵽ����ٵ�ת��\nPCM: ��Ϣ: ���Ҫд WAVE �ļ�, ʹ�� -ao pcm:waveheader (Ĭ��).\n"
#define MSGTR_AO_PCM_CantOpenOutputFile "[AO PCM] �� %s дʧ��!\n"

// ao_sdl.c
#define MSGTR_AO_SDL_INFO "[AO SDL] ������: %iHz ͨ��: %s ��ʽ %s\n"
#define MSGTR_AO_SDL_DriverInfo "[AO SDL] ʹ�� %s ��Ƶ����.\n"
#define MSGTR_AO_SDL_UnsupportedAudioFmt "[AO SDL] ��֧�ֵ���Ƶ��ʽ: 0x%x.\n"
#define MSGTR_AO_SDL_CantInit "[AO SDL] SDL ��Ƶ����ʧ��: %s\n"
#define MSGTR_AO_SDL_CantOpenAudio "[AO SDL] �޷�����Ƶ: %s\n"

// ao_sgi.c
#define MSGTR_AO_SGI_INFO "[AO SGI] ����.\n"
#define MSGTR_AO_SGI_InitInfo "[AO SGI] ����: ������: %iHz ͨ��: %s ��ʽ %s\n"
#define MSGTR_AO_SGI_InvalidDevice "[AO SGI] ����: �Ƿ��豸.\n"
#define MSGTR_AO_SGI_CantSetParms_Samplerate "[AO SGI] ����: �趨����ʧ��: %s\n�޷��趨��Ҫ�Ĳ�����.\n"
#define MSGTR_AO_SGI_CantSetAlRate "[AO SGI] ����: AL_RATE �ڸ�����Դ�ϲ�����.\n"
#define MSGTR_AO_SGI_CantGetParms "[AO SGI] ����: ��ȡ����ʧ��: %s\n"
#define MSGTR_AO_SGI_SampleRateInfo "[AO SGI] ����: ��ǰ�Ĳ�����Ϊ %lf (��Ҫ�������� %lf)\n"
#define MSGTR_AO_SGI_InitConfigError "[AO SGI] ����: %s\n"
#define MSGTR_AO_SGI_InitOpenAudioFailed "[AO SGI] ����: �޷�����Ƶͨ��: %s\n"
#define MSGTR_AO_SGI_Uninit "[AO SGI] uninit: ...\n"
#define MSGTR_AO_SGI_Reset "[AO SGI] reset: ...\n"
#define MSGTR_AO_SGI_PauseInfo "[AO SGI] audio_pause: ...\n"
#define MSGTR_AO_SGI_ResumeInfo "[AO SGI] audio_resume: ...\n"

// ao_sun.c
#define MSGTR_AO_SUN_RtscSetinfoFailed "[AO SUN] rtsc: SETINFO ʧ��.\n"
#define MSGTR_AO_SUN_RtscWriteFailed "[AO SUN] rtsc: дʧ��."
#define MSGTR_AO_SUN_CantOpenAudioDev "[AO SUN] �޷�����Ƶ�豸 %s, %s  -> nosound.\n"
#define MSGTR_AO_SUN_UnsupSampleRate "[AO SUN] audio_setup: ���������֧�� %d ͨ��, %s, %d Hz ������.\\n"
#define MSGTR_AO_SUN_CantUseSelect "[AO SUN]\n   ***  �����Ƶ������֧�� select()  ***\n�� #undef HAVE_AUDIO_SELECT in config.h ���±���MPlayer!\n\n"
#define MSGTR_AO_SUN_CantReopenReset "[AO SUN]\nFatal error: *** �޷����´򿪻�������Ƶ�豸 (%s) ***\n"

// ao_alsa5.c
#define MSGTR_AO_ALSA5_InitInfo "[AO ALSA5] alsa-init: Ҫ��ĸ�ʽ: %d Hz, %d ͨ��, %s\n"
#define MSGTR_AO_ALSA5_SoundCardNotFound "[AO ALSA5] alsa-init: û�з�������.\n"
#define MSGTR_AO_ALSA5_InvalidFormatReq "[AO ALSA5] alsa-init: Ҫ��ĸ�ʽ (%s) �Ƿ� - ȡ�����.\n"
#define MSGTR_AO_ALSA5_PlayBackError "[AO ALSA5] alsa-init: �򿪻طŴ���: %s\n"
#define MSGTR_AO_ALSA5_PcmInfoError "[AO ALSA5] alsa-init: pcm ��Ϣ����: %s\n"
#define MSGTR_AO_ALSA5_SoundcardsFound "[AO ALSA5] alsa-init: ���� %d ����, ʹ��: %s\n"
#define MSGTR_AO_ALSA5_PcmChanInfoError "[AO ALSA5] alsa-init: pcm ͨ����Ϣ����: %s\n"
#define MSGTR_AO_ALSA5_CantSetParms "[AO ALSA5] alsa-init: �趨��������: %s\n"
#define MSGTR_AO_ALSA5_CantSetChan "[AO ALSA5] alsa-init: �趨ͨ������: %s\n"
#define MSGTR_AO_ALSA5_ChanPrepareError "[AO ALSA5] alsa-init: ͨ��׼������: %s\n"
#define MSGTR_AO_ALSA5_DrainError "[AO ALSA5] alsa-uninit: �ط� drain ����: %s\n"
#define MSGTR_AO_ALSA5_FlushError "[AO ALSA5] alsa-uninit: �ط� flush ����: %s\n"
#define MSGTR_AO_ALSA5_PcmCloseError "[AO ALSA5] alsa-uninit: pcm �رմ���: %s\n"
#define MSGTR_AO_ALSA5_ResetDrainError "[AO ALSA5] alsa-reset: �ط� drain ����: %s\n"
#define MSGTR_AO_ALSA5_ResetFlushError "[AO ALSA5] alsa-reset: �ط� flush ����: %s\n"
#define MSGTR_AO_ALSA5_ResetChanPrepareError "[AO ALSA5] alsa-reset: ͨ��׼������: %s\n"
#define MSGTR_AO_ALSA5_PauseDrainError "[AO ALSA5] alsa-pause: �ط� drain ����: %s\n"
#define MSGTR_AO_ALSA5_PauseFlushError "[AO ALSA5] alsa-pause: �ط� flush ����: %s\n"
#define MSGTR_AO_ALSA5_ResumePrepareError "[AO ALSA5] alsa-resume: ͨ��׼������: %s\n"
#define MSGTR_AO_ALSA5_Underrun "[AO ALSA5] alsa-play: alsa δ����, ����������.\n"
#define MSGTR_AO_ALSA5_PlaybackPrepareError "[AO ALSA5] alsa-play: �ط�׼������: %s\n"
#define MSGTR_AO_ALSA5_WriteErrorAfterReset "[AO ALSA5] alsa-play: ������д����: %s - ����.\n"
#define MSGTR_AO_ALSA5_OutPutError "[AO ALSA5] alsa-play: �������: %s\n"

// ao_plugin.c

#define MSGTR_AO_PLUGIN_InvalidPlugin "[AO PLUGIN] �Ƿ����: %s\n"

// ======================= AF Audio Filters ================================

// libaf 

// af_ladspa.c

#define MSGTR_AF_LADSPA_AvailableLabels "���õı�ǩ"
#define MSGTR_AF_LADSPA_WarnNoInputs "����! ��� LADSPA ���û����Ƶ����.\n �Ժ����Ƶ�źŽ��ᶪʧ."
#define MSGTR_AF_LADSPA_ErrMultiChannel "���ڻ���֧�ֶ�ͨ��(>2)���.\n ֻ��ʹ�õ������������������."
#define MSGTR_AF_LADSPA_ErrNoOutputs "��� LADSPA ���û����Ƶ���."
#define MSGTR_AF_LADSPA_ErrInOutDiff "LADSPA �������Ƶ�������Ƶ�������Ŀ�����."
#define MSGTR_AF_LADSPA_ErrFailedToLoad "����ʧ��"
#define MSGTR_AF_LADSPA_ErrNoDescriptor "��ָ���Ŀ��ļ����Ҳ��� ladspa_descriptor() ����."
#define MSGTR_AF_LADSPA_ErrLabelNotFound "�ڲ�������Ҳ�����ǩ."
#define MSGTR_AF_LADSPA_ErrNoSuboptions "û����ѡ���ǩ"
#define MSGTR_AF_LADSPA_ErrNoLibFile "û��ָ�����ļ�"
#define MSGTR_AF_LADSPA_ErrNoLabel "û��ָ����������ǩ"
#define MSGTR_AF_LADSPA_ErrNotEnoughControls "�����и����Ŀ������"
#define MSGTR_AF_LADSPA_ErrControlBelow "%s: ������� #%d ������ %0.4f ֮��.\n"
#define MSGTR_AF_LADSPA_ErrControlAbove "%s: ������� #%d ������ %0.4f ֮��.\n"


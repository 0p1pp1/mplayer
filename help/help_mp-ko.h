// Translated by: DongCheon Park <pdc@kaist.ac.kr>

// Translated files should be uploaded to ftp://mplayerhq.hu/MPlayer/incoming
// and send a notify message to mplayer-dev-eng maillist.

// ========================= MPlayer ���� ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char* banner_text=
"\n\n"
"MPlayer " VERSION "(C) 2000-2002 Arpad Gereoffy (DOCS ����!)\n"
"\n";

static char help_text[]=
"����:   mplayer [���û���] [���/]���ϸ�\n"
"\n"
"���û��׵�:\n"
" -vo <drv[:dev]>  ���� ��� ����̹� �� ��ġ ���� (��Ϻ���� '-vo help')\n"
" -ao <drv[:dev]>  ����� ��� ����̹� �� ��ġ ���� (��Ϻ���� '-ao help')\n"
" -vcd <trackno>   ������ �ƴ� ��ġ�κ��� VCD (���� cd) Ʈ�� ���\n"
#ifdef HAVE_LIBCSS
" -dvdauth <dev>   ������ ���� DVD ��ġ ���� (��ȣȭ�� ��ũ��)\n"
#endif
#ifdef USE_DVDREAD
" -dvd <titleno>   ������ �ƴ� ��ġ�κ��� DVD Ÿ��Ʋ/Ʈ�� ���\n"
#endif
" -ss <timepos>    Ư�� ��ġ�� ã�ư��� (�� �Ǵ� ��:��:��)\n"
" -nosound         �Ҹ� ��� ����\n"
#ifdef USE_FAKE_MONO
" -stereo <mode>   MPEG1 ���׷��� ��� ���� (0:���׷��� 1:���� 2:������)\n"
#endif
" -channels <n>    ����� ��� ä�� ���� ����\n"
" -fs -vm -zoom    ȭ�� ũ�� ���� (��üȭ��, �������, s/wȮ��)\n"
" -x <x> -y <y>    ȭ���� <x>*<y>�ػ󵵷� ���� [-vo ����̹��� �����ϴ� ��츸!]\n"
" -sub <file>      ����� �ڸ����� ���� (-subfps, -subdelay�� ����)\n"
" -playlist <file> ���������� ����\n"
" -vid x -aid y    ����� ����(x) �� �����(y) ��Ʈ�� ����\n"
" -fps x -srate y  ����(x fps)�� �����(y Hz) ���� ����\n"
" -pp <quality>    �켱ó�� ���� ��� (DivX�� 0-4, mpegs�� 0-63)\n"
" -nobps           AVI ������ ���� �ٸ� A-V ����ȭ ��� ���\n"
" -framedrop       ������ ���߸��� ��� (���� machine��)\n"
" -wid <window id> ���� â���� ���� ��� (plugger�� ȿ����!)\n"
"\n"
"����Ű:\n"
" <-  �Ǵ�  ->     10�� �ڷ�/������ �̵�\n"
" up �Ǵ� dn       1�� �ڷ�/������ �̵�\n"
" < �Ǵ� >         �����Ͽ��� �ڷ�/������ �̵�\n"
" p �Ǵ� SPACE     ��� ���� (�ƹ�Ű�� ������ ���)\n"
" q �Ǵ� ESC       ����� ���߰� ���α׷��� ����\n"
" + �Ǵ� -         +/- 0.1�� ����� ���� ����\n"
" o                OSD��� ����:  ����/Ž����/Ž����+Ÿ�̸�\n"
" * �Ǵ� /         ���� ����/���� ('m'�� ���� master/pcm ����)\n"
" z �Ǵ� x         +/- 0.1�� �ڸ� ���� ����\n"
"\n"
" * * * �ڼ��� ����(�� ���� ���û��� �� ����Ű��)�� MANPAGE�� �����ϼ��� ! * * *\n"
"\n";
#endif

// ========================= MPlayer �޼��� ===========================

// mplayer.c: 

#define MSGTR_Exiting "\n�����մϴ�... (%s)\n"
#define MSGTR_Exit_quit "����"
#define MSGTR_Exit_eof "������ ��"
#define MSGTR_Exit_error "ġ���� ����"
// FIXME: %d must be before %s !!!
// #define MSGTR_IntBySignal "\nMPlayer�� %s��⿡�� %d��ȣ�� ���ͷ�Ʈ�Ǿ����ϴ�.\n"
#define MSGTR_NoHomeDir "Ȩ���丮�� ã�� �� �����ϴ�.\n"
#define MSGTR_GetpathProblem "get_path(\"config\") ���� �߻�\n"
#define MSGTR_CreatingCfgFile "�������� %s�� ����ϴ�.\n"
#define MSGTR_InvalidVOdriver "%s�� �߸��� ���� ��� ����̹��Դϴ�.\n������ ���� ��� ����̹� ����� ������ '-vo help' �ϼ���.\n"
#define MSGTR_InvalidAOdriver "%s�� �߸��� ����� ��� ����̹��Դϴ�.\n������ ����� ��� ����̹� ����� ������ '-ao help' �ϼ���.\n"
#define MSGTR_CopyCodecsConf "((MPlayer �ҽ� Ʈ����) etc/codecs.conf�� ~/.mplayer/codecs.conf�� ���� �Ǵ� ��ũ�ϼ���.)\n"
#define MSGTR_CantLoadFont "%s ��Ʈ�� ã�� �� �����ϴ�.\n"
#define MSGTR_CantLoadSub "%s �ڸ��� ã�� �� �����ϴ�.\n"
#define MSGTR_ErrorDVDkey "DVD Ű�� ó���ϴ� ���� ������ �߻��߽��ϴ�.\n"
#define MSGTR_CmdlineDVDkey "��û�� DVD ����� Ű�� �ص��� ���� �����߽��ϴ�.\n"
#define MSGTR_DVDauthOk "DVD ���� ����� �������ε� �մϴ�.\n"
#define MSGTR_DumpSelectedStreamMissing "dump: ġ����: ���õ� ��Ʈ���� �����ϴ�!\n"
#define MSGTR_CantOpenDumpfile "dump������ �� �� �����ϴ�!!!\n"
#define MSGTR_CoreDumped "core dumped :)\n"
#define MSGTR_FPSnotspecified "����� FPS�� ���������ʾҰų� �߸��Ǿ����ϴ�! -fps �ɼ��� ����ϼ���!\n"
#define MSGTR_TryForceAudioFmt "����� �ڵ� ����̹� %d���� �õ��ϰ� �ֽ��ϴ�...\n"
#define MSGTR_CantFindAfmtFallback "�õ��� ����̹������� ����� �ڵ��� ã�� �� �����ϴ�. �ٸ� ����̹��� ��ü�ϼ���.\n"
#define MSGTR_CantFindAudioCodec "����� ���� 0x%X�� ���� �ڵ��� ã�� �� �����ϴ�!\n"
#define MSGTR_TryUpgradeCodecsConfOrRTFM "*** etc/codecs.conf�� ���� %s�� ���׷��̵��غ�����.\n*** ������ �۵�����������, DOCS/codecs.html�� �о����!\n"
#define MSGTR_CouldntInitAudioCodec "����� �ڵ��� �ʱ�ȭ�� �� �����ϴ�! -> �Ҹ�����\n"
#define MSGTR_TryForceVideoFmt "���� �ڵ� ����̹� %d���� �õ��ϰ� �ֽ��ϴ�...\n"
#define MSGTR_CantFindVideoCodec "���� ���� 0x%X�� ���� �ڵ��� ã�� �� �����ϴ�!\n"
#define MSGTR_VOincompCodec "�˼��մϴ�, ������ ���� ��� ��ġ�� �� �ڵ��� ȣȯ���� �ʽ��ϴ�.\n"
#define MSGTR_CannotInitVO "ġ����: ���� ����̹��� �ʱ�ȭ�� �� �����ϴ�!\n"
#define MSGTR_CannotInitAO "����� ��ġ�� ���ų� �ʱ�ȭ�� �� �����ϴ�. -> �Ҹ�����\n"
#define MSGTR_StartPlaying "����� �����մϴ�...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"         ************************************************\n"\
"         **** ����ϱ⿡�� �ý����� �ʹ� �����ϴ�.!  ****\n"\
"         ************************************************\n"\
"!!! ������ ����, ����, ��ó���: \n"\
"- ��κ��� ���: ����/���װ� ���� ����� ����̹�. ��ó���: -ao sdl �� ����غ�����.\n"\
"  ALSA 0.5 �� ALSA 0.9�� oss ���ķ��̼�. �� ���� ���� DOCS/sound.html �� �����ϼ���!\n"\
"- ���� ����� ����. �ٸ� -vo driver (����� -vo help)�� ����غ�����.\n"\
"  -framedrop ���!  ���� ����/�ӵ���� ���� DOCS/video.html �� �����ϼ���!\n"\
"- ���� cpu. ��ġ ū dvd�� divx�� �������������! -hardframedrop �� ����غ�����.\n"\
"- ���� ����. �������� ������ ����غ�����: -nobps  -ni  -mc 0  -forceidx\n"\
"���� � ���׵� ������� �ʴ´ٸ�, DOCS/bugreports.html �� �����ϼ���!\n\n"

#define MSGTR_NoGui "MPlayer�� GUI �������� �����ϵǾ����ϴ�!\n"
#define MSGTR_GuiNeedsX "MPlayer GUI�� X11�� �ʿ��մϴ�!\n"
#define MSGTR_Playing "%s �����\n"
#define MSGTR_NoSound "�����: �Ҹ�����!!!\n"
#define MSGTR_FPSforced "FPS�� %5.3f (ftime: %5.3f)�� �ǵ��� �Ͽ����ϴ�.\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "CD-ROM ��ġ '%s'�� ã�� �� �����ϴ�!\n"
#define MSGTR_ErrTrackSelect "VCD Ʈ���� �����ϴ� ���� ������ �߻��߽��ϴ�!"
#define MSGTR_ReadSTDIN "ǥ���Է�(stdin)���� ���� �а� �ֽ��ϴ�...\n"
#define MSGTR_UnableOpenURL "%s URL�� �� �� �����ϴ�.\n"
#define MSGTR_ConnToServer "%s ������ ����Ǿ����ϴ�.\n"
#define MSGTR_FileNotFound "'%s'������ ã�� �� �����ϴ�.\n"

#define MSGTR_CantOpenDVD "DVD ��ġ %s�� �� �� �����ϴ�.\n"
#define MSGTR_DVDwait "��ũ ������ �а��ֽ��ϴ�, ��ٷ� �ּ���...\n"
#define MSGTR_DVDnumTitles "�� DVD���� %d Ÿ��Ʋ�� �ֽ��ϴ�.\n"
#define MSGTR_DVDinvalidTitle "�߸��� DVD Ÿ��Ʋ ��ȣ: %d\n"
#define MSGTR_DVDnumChapters "�� DVD Ÿ��Ʋ���� %d é�Ͱ� �ֽ��ϴ�.\n"
#define MSGTR_DVDinvalidChapter "�߸��� DVD é�� ��ȣ: %d\n"
#define MSGTR_DVDnumAngles "�� DVD Ÿ��Ʋ���� %d �ޱ��� �ֽ��ϴ�.\n"
#define MSGTR_DVDinvalidAngle "�߸��� DVD �ޱ� ��ȣ: %d\n"
#define MSGTR_DVDnoIFO "DVD Ÿ��Ʋ %d�� ���� IFO������ �� �� �����ϴ�.\n"
#define MSGTR_DVDnoVOBs "Ÿ��Ʋ VOBS (VTS_%02d_1.VOB)�� �� �� �����ϴ�.\n"
#define MSGTR_DVDopenOk "���������� DVD�� ���Ƚ��ϴ�!\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "���! ����� ��Ʈ�� ��� %d�� �����ǵǾ����ϴ�!\n"
#define MSGTR_VideoStreamRedefined "���! ���� ��Ʈ�� ��� %d�� �����ǵǾ����ϴ�!\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: ���ۿ� �ʹ� ���� (%d in %d bytes) ����� ��Ŷ�� �ֽ��ϴ�!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: ���ۿ� �ʹ� ���� (%d in %d bytes) ���� ��Ŷ�� �ֽ��ϴ�!\n"
#define MSGTR_MaybeNI "(non-interleaved ��Ʈ��/������ ����ϰ��ְų� �ڵ��� �߸��Ǿ����ϴ�.)\n"
#define MSGTR_Detected_XXX_FileFormat "%s ���� ������ �߰��߽��ϴ�!\n"
#define MSGTR_InvalidMPEGES "�߸��� MPEG-ES ��Ʈ��??? �����ڿ��� �����ϼ���, ���������� �𸨴ϴ�. :(\n"
#define MSGTR_FormatNotRecognized "============= �˼��մϴ�, �� ���������� �νĵ������߰ų� ���������ʽ��ϴ� ===============\n"\
				  "=== ���� �� ������ AVI, ASF �Ǵ� MPEG ��Ʈ���̶��, �����ڿ��� �����ϼ���! ===\n"
#define MSGTR_MissingVideoStream "���� ��Ʈ���� ã�� ���߽��ϴ�!\n"
#define MSGTR_MissingAudioStream "����� ��Ʈ���� ã�� ���߽��ϴ�...  ->�Ҹ�����\n"
#define MSGTR_MissingVideoStreamBug "ã�� �� ���� ���� ��Ʈ��!? �����ڿ��� �����ϼ���, ���������� �𸨴ϴ�. :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: ���Ͽ� ���õ� ����� �� ���� ��Ʈ���� �����ϴ�.\n"

#define MSGTR_NI_Forced "������"
#define MSGTR_NI_Detected "�߰ߵ�"
#define MSGTR_NI_Message "%s�� NON-INTERLEAVED AVI ���� �����Դϴ�!\n"

#define MSGTR_UsingNINI "NON-INTERLEAVED ���� AVI ���� ������ ����մϴ�!\n"
#define MSGTR_CouldntDetFNo "������ ���� ������ �� �����ϴ�.\n"
#define MSGTR_CantSeekRawAVI "raw .AVI ��Ʈ�������� Ž���� �� �����ϴ�! (�ε����� �ʿ��մϴ�, -idx ����ġ�� �õ��غ�����!)  \n"
#define MSGTR_CantSeekFile "�� ���Ͽ����� Ž���� �� �����ϴ�!  \n"

#define MSGTR_EncryptedVOB "��ȣȭ�� VOB �����Դϴ�(libcss �������� �����ϵǾ���)! DOCS/cd-dvd.html�� �����ϼ���\n"
#define MSGTR_EncryptedVOBauth "��ȣȭ�� ��Ʈ���ε�, ���� ��û�� �����ʾҽ��ϴ�!!\n"

#define MSGTR_MOVcomprhdr "MOV: ����� ����� (����) ���������ʽ��ϴ�!\n"
#define MSGTR_MOVvariableFourCC "MOV: ���! FOURCC ���� �߰�!?\n"
#define MSGTR_MOVtooManyTrk "MOV: ���! Ʈ���� �ʹ� �����ϴ�!"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "�ڵ��� �� �� �����ϴ�.\n"
#define MSGTR_CantCloseCodec "�ڵ��� ���� �� �����ϴ�.\n"

#define MSGTR_MissingDLLcodec "����: ��û�� DirectShow �ڵ� %s�� �� �� �����ϴ�.\n"
#define MSGTR_ACMiniterror "Win32/ACM ����� �ڵ��� ���ų� �ʱ�ȭ�� �� �����ϴ�. (DLL ������ ������?)\n"
#define MSGTR_MissingLAVCcodec "libavcodec���� '%s' �ڵ��� ã�� �� �����ϴ�...\n"

#define MSGTR_MpegNoSequHdr "MPEG: ġ����: ������ ����� ã�� ���� EOF.\n"
#define MSGTR_CannotReadMpegSequHdr "ġ����: ������ ����� ã�� �� �����ϴ�!\n"
#define MSGTR_CannotReadMpegSequHdrEx "ġ����: ������ ��� Ȯ���� ���� �� �����ϴ�!\n"
#define MSGTR_BadMpegSequHdr "MPEG: �ҷ��� ������ ���!\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: �ҷ��� ������ ��� Ȯ��!\n"

#define MSGTR_ShMemAllocFail "���� �޸𸮸� �Ҵ��� �� �����ϴ�.\n"
#define MSGTR_CantAllocAudioBuf "����� ��� ���۸� �Ҵ��� �� �����ϴ�.\n"

#define MSGTR_UnknownAudio "�� �� ���� ����� �����Դϴ�. -> �Ҹ�����\n"

// LIRC:
#define MSGTR_SettingUpLIRC "lirc ������ �����մϴ�...\n"
#define MSGTR_LIRCdisabled "�������� ����� �� �����ϴ�.\n"
#define MSGTR_LIRCopenfailed "lirc ���� ���� ����!\n"
#define MSGTR_LIRCcfgerr "LIRC �������� %s�� �дµ� �����߽��ϴ�!\n"


// ====================== GUI �޼���/��ư ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "����"
#define MSGTR_FileSelect "���� ���� ..."
#define MSGTR_SubtitleSelect "�ڸ� ���� ..."
#define MSGTR_OtherSelect "���� ..."
#define MSGTR_PlayList "������"
#define MSGTR_SkinBrowser "��Ų ã��"

// --- buttons ---
#define MSGTR_Ok "Ȯ��"
#define MSGTR_Cancel "���"
#define MSGTR_Add "�߰�"
#define MSGTR_Remove "����"

// --- error messages ---
#define MSGTR_NEMDB "�˼��մϴ�, draw ���ۿ� ����� �޸𸮰� �����ϴ�."
#define MSGTR_NEMFMR "�˼��մϴ�, �޴� �������� ���� ����� �޸𸮰� �����ϴ�."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[��Ų] ��Ų �������� %s�� %d��° �ٿ� ������ �ֽ��ϴ�." 
#define MSGTR_SKIN_WARNING1 "[��Ų] ��Ų ���������� %d��° �ٿ� ���: ������ ã������ \"section\"�տ� ( %s )�� ã�� �� �����ϴ�."
#define MSGTR_SKIN_WARNING2 "[��Ų] ��Ų ���������� %d��° �ٿ� ���: ������ ã������ \"subsection\"�տ� ( %s )�� ã�� �� �����ϴ�."
#define MSGTR_SKIN_BITMAP_16bit  "16 ��Ʈ Ȥ�� �� ���� depth�� ��Ʈ���� �������� �ʽ��ϴ� ( %s ).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "������ ã�� �� �����ϴ� ( %s ).\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "bmp �б� ���� ( %s )\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "tga �б� ���� ( %s )\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "png �б� ���� ( %s )\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "RLE �ѵ� tga�� �������� �ʽ��ϴ� ( %s ).\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "�� �� ���� ���� ���� ( %s )\n"
#define MSGTR_SKIN_BITMAP_ConvertError "24 ��Ʈ���� 32 ��Ʈ�� ��ȯ ���� ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "�� �� ���� �޼���: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "�޸𸮰� �����մϴ�.\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "��Ʈ�� �ʹ� ���� ����Ǿ� �ֽ��ϴ�.\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "��Ʈ������ ã�� �� �����ϴ�.\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "��Ʈ �̹��������� ã�� �� �����ϴ�.\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "�������� �ʴ� ��Ʈ identifier ( %s )\n"
#define MSGTR_SKIN_UnknownParameter "�� �� ���� �Ű����� ( %s )\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[��Ų����] �޸𸮰� �����մϴ�.\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "��Ų�� ã�� �� �����ϴ� ( %s ).\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "��Ų �������� �б� ���� ( %s )\n"
#define MSGTR_SKIN_LABEL "��Ų:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "MPlayer ����"
#define MSGTR_MENU_Open "���� ..."
#define MSGTR_MENU_PlayFile "���� ��� ..."
#define MSGTR_MENU_PlayVCD "VCD ��� ..."
#define MSGTR_MENU_PlayDVD "DVD ��� ..."
#define MSGTR_MENU_PlayURL "URL ��� ..."
#define MSGTR_MENU_LoadSubtitle "�ڸ� ���� ..."
#define MSGTR_MENU_Playing "�����"
#define MSGTR_MENU_Play "���"
#define MSGTR_MENU_Pause "����"
#define MSGTR_MENU_Stop "����"
#define MSGTR_MENU_NextStream "����"
#define MSGTR_MENU_PrevStream "����"
#define MSGTR_MENU_Size "ũ��"
#define MSGTR_MENU_NormalSize "���� ũ��"
#define MSGTR_MENU_DoubleSize "�ι� ũ��"
#define MSGTR_MENU_FullScreen "��ü ȭ��"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "��ũ ��� ..."
#define MSGTR_MENU_ShowDVDMenu "DVD �޴�����"
#define MSGTR_MENU_Titles "Ÿ��Ʋ"
#define MSGTR_MENU_Title "Ÿ��Ʋ %2d"
#define MSGTR_MENU_None "(����)"
#define MSGTR_MENU_Chapters "é��"
#define MSGTR_MENU_Chapter "é�� %2d"
#define MSGTR_MENU_AudioLanguages "����� ���"
#define MSGTR_MENU_SubtitleLanguages "�ڸ� ���"
#define MSGTR_MENU_PlayList "������"
#define MSGTR_MENU_SkinBrowser "��Ų����"
#define MSGTR_MENU_Preferences "���û���"
#define MSGTR_MENU_Exit "���� ..."

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "ġ������ ���� ..."
#define MSGTR_MSGBOX_LABEL_Error "���� ..."
#define MSGTR_MSGBOX_LABEL_Warning "��� ..." 

#endif



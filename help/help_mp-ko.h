// Translated by: DongCheon Park <pdc@kaist.ac.kr>

// Translated files should be uploaded to ftp://mplayerhq.hu/MPlayer/incoming
// and send a notify message to mplayer-dev-eng maillist.

// ========================= MPlayer ���� ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"����:   mplayer [���û���] [���/]���ϸ�\n"
"\n"
"���û��׵�:\n"
" -vo <drv[:dev]>  ���� ��� ����̹� �� ��ġ ���� (��Ϻ���� '-vo help')\n"
" -ao <drv[:dev]>  ����� ��� ����̹� �� ��ġ ���� (��Ϻ���� '-ao help')\n"
" vcd://<trackno>   ������ �ƴ� ��ġ�κ��� VCD (���� cd) Ʈ�� ���\n"
#ifdef HAVE_LIBCSS
" -dvdauth <dev>   ������ ���� DVD ��ġ ���� (��ȣȭ�� ��ũ��)\n"
#endif
#ifdef USE_DVDREAD
" dvd://<titleno>   ������ �ƴ� ��ġ�κ��� DVD Ÿ��Ʋ/Ʈ�� ���\n"
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
#define MSGTR_IntBySignal "\nMPlayer�� %d�ñ׳��� ���� %s��⿡�� ���ͷ�Ʈ�Ǿ����ϴ�.\n"
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
#define MSGTR_Playing "%s ��� ��...\n"
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
#define MSGTR_FormatNotRecognized "============= �˼��մϴ�, �� ���������� �ν��������߰ų� ���������ʽ��ϴ� ===============\n"\
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
#define MSGTR_MENU_Playing "�۵�"
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


// --- the diffs between help_mp-ko and help_mp-en in MPlayer-0.90pre9.
#define MSGTR_TryForceAudioFmtStr "����� �ڵ� ����̹� %s��(��) ������ �õ��մϴ�...\n"
#define MSGTR_TryForceVideoFmtStr "���� �ڵ� ����̹� %s��(��) ������ �õ��մϴ�...\n"
#define MSGTR_CompiledWithRuntimeDetection "��Ÿ�� CPU ������ �����ϰ� �����ϵǾ����ϴ�. - ���, ������ ������ �ƴմϴ�!���� ���� ������ ������ؼ�, MPlayer�� --disable-runtime-cpudetection �ɼ����� �ٽ� �������ϼ���.\n"
#define MSGTR_CompiledWithCPUExtensions "Ȯ�� x86 CPU������ ������ �Ǿ����ϴ�.:"
#define MSGTR_AvailableVideoOutputPlugins "������ ���� ��� �÷�����:\n"
#define MSGTR_AvailableVideoOutputDrivers "������ ���� ��� ����̹�:\n"
#define MSGTR_AvailableAudioOutputDrivers "������ ����� ��� �帮�ƹ�:\n"
#define MSGTR_AvailableAudioCodecs "������ ����� �ڵ�:\n"
#define MSGTR_AvailableVideoCodecs "������ ���� �ڵ�:\n"
#define MSGTR_AvailableAudioFm "\n������ (�����ϵ�) ����� �ڵ� ����/����̹�:\n"
#define MSGTR_AvailableVideoFm "\n������ (�����ϵ�) ���� �ڵ� ����/����̹�:\n"
#define MSGTR_UsingRTCTiming "������ �ϵ���� RTC Ÿ�̹� ��� (%ldHz)\n"
#define MSGTR_CannotReadVideoProperties "����: �Ӽ��� ���� �� �����ϴ�.\n"
#define MSGTR_NoStreamFound "��Ƽ���� ã�� �� �����ϴ�.\n"
#define MSGTR_InitializingAudioCodec "����� �ڵ��� �ʱ�ȭ�մϴ�...\n"
#define MSGTR_ErrorInitializingVODevice "������ ���� ��� (-vo) ��ġ�� ���ų� �ʱ�ȭ�� �� �����ϴ�!\n"
#define MSGTR_ForcedVideoCodec "������ ���� ���� �ڵ�: %s\n"
#define MSGTR_ForcedAudioCodec "������ ���� ����� �ڵ�: %s\n"
#define MSGTR_AODescription_AOAuthor "AO: ����: %s\nAO: ������: %s\n"
#define MSGTR_AOComment "AO: ����: %s\n"
#define MSGTR_Video_NoVideo "����: ���� ����!!\n"
#define MSGTR_NotInitializeVOPorVO "\nġ����: ���� ����(-vop) �Ǵ� ���� ���(-vo)�� �ʱ�ȭ�� �� �����ϴ�!\n"
#define MSGTR_Paused "\n------ ��ø��� -------\r"
#define MSGTR_PlaylistLoadUnable "\n������ %s��(��) �� �� �����ϴ�\n"
#define MSGTR_UsingPass3ControllFile "pass3 ��Ʈ�� ���� ���: %s\n"
#define MSGTR_MissingFilename "\n������ ã�� �� �����ϴ�!\n\n"
#define MSGTR_CannotOpenFile_Device "����/��ġ�� �� �� �����ϴ�.\n"
#define MSGTR_ErrorDVDAuth "DVD ���� ����...\n"
#define MSGTR_CannotOpenDemuxer "�ؼ��⸦ �� �� �����ϴ�.\n"
#define MSGTR_NoAudioEncoderSelected "\n���õ� ����� ���ڴ�(-oac)�� �����ϴ�! �ϳ��� �����ϰų�, -nosound �ɼ��� ����ϼ���. -oac help ����!\n"
#define MSGTR_NoVideoEncoderSelected "\n���õ� ���� ���ڴ�(-ovc)�� �����ϴ�! �ϳ��� ���ðų�, -ovc help�� �����ϼ���!\n"
#define MSGTR_InitializingAudioCodec "����� �ڵ��� �ʱ�ȭ�մϴ�...\n"
#define MSGTR_CannotOpenOutputFile "��� ���� '%s'��(��) �� �� �����ϴ�.\n"
#define MSGTR_EncoderOpenFailed "���ڴ� ���⿡ �����߽��ϴ�.\n"
#define MSGTR_ForcingOutputFourcc "fourcc�� %x [%.4s](��)�� ��������մϴ�.\n"
#define MSGTR_WritingAVIHeader "AVI �ش��� ���� �ֽ��ϴ�...\n"
#define MSGTR_DuplicateFrames "\n%d ������(��) �ߺ�!!!    \n"
#define MSGTR_SkipFrame "\n������ �ǳʶٱ�!!!    \n"
#define MSGTR_ErrorWritingFile "%s: ���� ���� ����.\n"
#define MSGTR_WritingAVIIndex "\nAVI �ε����� ���� �ֽ��ϴ�...\n"
#define MSGTR_FixupAVIHeader "AVI �ش��� ��ġ�� �ֽ��ϴ�...\n"
#define MSGTR_RecommendedVideoBitrate "%s CD������ ��õ�� ���� ���� �ֻ���: %d\n"
#define MSGTR_VideoStreamResult "\n���� ��Ʈ��: %8.3f kbit/s  (%d bps)  ũ��: %d ����Ʈ  %5.3f ��  %d ������\n"
#define MSGTR_AudioStreamResult "\n����� ��Ʈ��: %8.3f kbit/s  (%d bps)  ũ��: %d ����Ʈ  %5.3f ��\n"
#define MSGTR_SMBInitError "libsmbclient ���̺귯���� �ʱ�ȭ�� �� �����ϴ�.: %d\n"
#define MSGTR_SMBFileNotFound "lan���� ���� �� �� �����ϴ�.: '%s'\n"
#define MSGTR_SMBNotCompiled "MPlayer�� SMB �б� �����ϵ��� �����ϵ��� �ʾҽ��ϴ�.\n"
#define MSGTR_SwitchToNi "\n�߸��� interleaved .AVI�� �����Ͽ����ϴ�. -ni ���� �ٲߴϴ�!\n"
#define MSGTR_DetectedAudiofile "����� ������ �����Ͽ����ϴ�!\n"
#define MSGTR_NotSystemStream "MPEG �ý��� ��Ʈ�� ������ �ƴմϴ�... (���� ��Ʈ�������� ��?)\n"
#define MSGTR_FoundAudioStream "==> ����� ��Ʈ�� �߰�: %d\n"
#define MSGTR_FoundVideoStream "==> ���� ��Ʈ�� �߰�: %d\n"
#define MSGTR_DetectedTV "TV�� �����Ͽ����ϴ�! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "ogg �ؼ��⸦ �� �� �����ϴ�.\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: ����� ��Ʈ���� ã�� �ֽ��ϴ�... (id:%d)\n"
#define MSGTR_CannotOpenAudioStream "����� ��Ʈ���� �� �� �����ϴ�.: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "�ڸ� ��Ʈ���� �� �� �����ϴ�.: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "����� �ؼ��⸦ ���µ� �����߽��ϴ�.: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "�ڸ� �ؼ��⸦ ���µ� �����߽��ϴ�.: %s\n"
#define MSGTR_TVInputNotSeekable "TV �Է��� ã�� �� �����ϴ�! (ä���� �ٲٰ� �ϸ� �ɼ��� �ֽ��ϴ�. ;)\n"
#define MSGTR_DemuxerInfoAlreadyPresent "�ؼ��� ���� %s(��)�� �̹� �����մϴ�!\n"
#define MSGTR_ClipInfo "Ŭ�� ����: \n"
#define MSGTR_UsingExternalPP "[PP] �ܺ� ����ó�� ���� ���, max q = %d\n"
#define MSGTR_UsingCodecPP "[PP] �ڵ��� ����ó�� ���, max q = %d\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "���õ� vo & vd�� ���� �Ӽ� '%s'��(��) �������� �ʽ��ϴ�! \n"
#define MSGTR_VideoCodecFamilyNotAvailableStr "��û�� ���� �ڵ� ���� [%s] (vfm=%s)��(��) ����� �� �����ϴ�. (�����Ͻÿ� �����ϵ��� �����Ͻʽÿ�!)\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "��û�� ����� ���� ���� [%s] (afm=%s)��(��) ����� �� �����ϴ�. (�����Ͻÿ� �����ϵ��� �����Ͻʽÿ�!)\n"
#define MSGTR_OpeningVideoDecoder "���� ���ڴ� [%s] %s��(��) ���� �ֽ��ϴ�.\n"
#define MSGTR_OpeningAudioDecoder "����� ���ڴ� [%s] %s��(��) ���� �ֽ��ϴ�.\n"
#define MSGTR_UninitVideoStr "���� %s �ʱ�ȭ ���  \n"
#define MSGTR_UninitAudioStr "����� %s �ʱ�ȭ ���  \n"
#define MSGTR_VDecoderInitFailed "���� ���ڴ� �ʱ�ȭ ����  :(\n"
#define MSGTR_ADecoderInitFailed "����� ���ڴ� �ʱ�ȭ ����  :(\n"
#define MSGTR_ADecoderPreinitFailed "����� ���ڴ� �̸� �ʱ�ȭ�ϱ� ���� :(\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: �Է� ���۷� %d ����Ʈ�� �Ҵ��մϴ�.\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: ��� ���۷� %d + %d = %d ����Ʈ�� �Ҵ��մϴ�.\n"
#define MSGTR_CouldNotFindVideoFilter "���� ���� '%s'��(��) ã�� �� �����ϴ�.\n"
#define MSGTR_CouldNotOpenVideoFilter "���� ���� '%s'��(��) �� �� �����ϴ�.\n"
#define MSGTR_OpeningVideoFilter "���� ���͸� ���� �ֽ��ϴ�.: "
#define MSGTR_CannotFindColorspace "���� �÷������� ã�� �� �����ϴ�. :(\n"
#define MSGTR_CodecDidNotSet "VDec: �ڵ��� �������� �ʾҽ��ϴ�. sh->disp_w and sh->disp_h�� �õ��� ���ʽÿ�!!\n"
#define MSGTR_VoConfigRequest "VDec: vo ���� ��û - %d x %d (��ȣ�ϴ� csp: %s)\n"
#define MSGTR_CouldNotFindColorspace "��︮�� �÷������� ã�� �� �����ϴ�. -vop ũ�������� �ٽ� �õ��մϴ�...\n"
#define MSGTR_MovieAspectIsSet "ȭ������� %.2f:1 �Դϴ�.- ȭ�� ������ �����ϱ����� �̸� ũ�������� �մϴ�.\n"
#define MSGTR_MovieAspectUndefined "ȭ������� ���ǵ��� �ʾҽ��ϴ�. - �̸� ũ�������� ������� �ʾҽ��ϴ�.\n"
#define MSGTR_AudioFileSelect "���� ���� ���� ..."
#define MSGTR_FontSelect "�۲� ���� ..."
#define MSGTR_Equalizer "����������"
#define MSGTR_Network "��Ʈ��ũ ��Ʈ���� ..."
#define MSGTR_Preferences "���û���"
#define MSGTR_OSSPreferences "OSS ����̹� ����"
#define MSGTR_NoMediaOpened "�̵�� ����"
#define MSGTR_VCDTrack "VCD Ʈ�� %d"
#define MSGTR_NoChapter "é�� ����"
#define MSGTR_Chapter "é�� %d"
#define MSGTR_NoFileLoaded "���� ����"
#define MSGTR_Clear "����"
#define MSGTR_Config "����"
#define MSGTR_ConfigDriver "����̹� ����"
#define MSGTR_Browse "����"
#define MSGTR_IDFGCVD "�˼��մϴ�. GUI ȣȯ ���� ��� ����̹��� ã�� ���߽��ϴ�."
#define MSGTR_NEEDLAVCFAME "�˼��մϴ�. MPEG�� �ƴ� ������ DXR3/H+ ��ġ�� ����Ϸ��� �ٽ� ���ڵ��ؾ߸� �մϴ�.\nDXR3/H+ ���� ��ȭ���ڿ��� lavc�� frame�� �����ϵ��� �Ͻʽÿ�."
#define MSGTR_MENU_DropSubtitle "�ڸ� ���� ..."
#define MSGTR_MENU_LoadExternAudioFile "���� ���� ..."
#define MSGTR_MENU_Mute "���Ұ�"
#define MSGTR_MENU_Original "�������"
#define MSGTR_MENU_AspectRatio "ȭ�����"
#define MSGTR_EQU_Audio "�����"
#define MSGTR_EQU_Video "����"
#define MSGTR_EQU_Contrast "���: "
#define MSGTR_EQU_Brightness "���: "
#define MSGTR_EQU_Hue "����: "
#define MSGTR_EQU_Saturation "ä��: "
#define MSGTR_EQU_Front_Left "���� ��"
#define MSGTR_EQU_Front_Right "������ ��"
#define MSGTR_EQU_Back_Left "���� ��"
#define MSGTR_EQU_Back_Right "������ ��"
#define MSGTR_EQU_Center "���"
#define MSGTR_EQU_Bass "���̽�"
#define MSGTR_EQU_All "���"
#define MSGTR_PLAYLIST_Path "���"
#define MSGTR_PLAYLIST_Selected "���õ� ����"
#define MSGTR_PLAYLIST_Files "����"
#define MSGTR_PLAYLIST_DirectoryTree "���͸�"
#define MSGTR_PREFERENCES_None "����"
#define MSGTR_PREFERENCES_AvailableDrivers "������ ����̹�:"
#define MSGTR_PREFERENCES_DoNotPlaySound "���� ��� ����"
#define MSGTR_PREFERENCES_NormalizeSound "���� ǥ��ȭ"
#define MSGTR_PREFERENCES_EnEqualizer "���������� ���"
#define MSGTR_PREFERENCES_ExtraStereo "�ܺ� ���׷��� ���"
#define MSGTR_PREFERENCES_Coefficient "���:"
#define MSGTR_PREFERENCES_AudioDelay "����� ����:"
#define MSGTR_PREFERENCES_Audio "�����"
#define MSGTR_PREFERENCES_DoubleBuffer "���� ���۸� ���"
#define MSGTR_PREFERENCES_DirectRender "���̷�Ʈ ������ ���"
#define MSGTR_PREFERENCES_FrameDrop "������ �ǳʶٱ� ���"
#define MSGTR_PREFERENCES_HFrameDrop "���� ������ �ǳʶٱ� ���(������)"
#define MSGTR_PREFERENCES_Flip "�̹��� ���� ����"
#define MSGTR_PREFERENCES_Panscan "�ҽ�ĵ: "
#define MSGTR_PREFERENCES_Video "����"
#define MSGTR_PREFERENCES_OSDTimer "Ÿ�̸� �� ǥ�ñ�"
#define MSGTR_PREFERENCES_OSDProgress "���� ���븸 ǥ��"
#define MSGTR_PREFERENCES_Subtitle "�ڸ�:"
#define MSGTR_PREFERENCES_SUB_Delay "����: "
#define MSGTR_PREFERENCES_SUB_FPS "FPS:"
#define MSGTR_PREFERENCES_SUB_POS "��ġ: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "�ڵ����� �ڸ� �����ʱ�"
#define MSGTR_PREFERENCES_SUB_Unicode "�����ڵ� �ڸ�"
#define MSGTR_PREFERENCES_SUB_MPSUB "�־��� �ڸ��� MPlayer�� �ڸ� �������� �ٲߴϴ�."
#define MSGTR_PREFERENCES_SUB_SRT "�־��� �ڸ��� SRT �������� �ٲߴϴ�."
#define MSGTR_PREFERENCES_Font "�۲�:"
#define MSGTR_PREFERENCES_FontFactor "�۲� ����:"
#define MSGTR_PREFERENCES_PostProcess "����ó�� ���"
#define MSGTR_PREFERENCES_AutoQuality "�ڵ� ǰ������: "
#define MSGTR_PREFERENCES_NI "non-interleaved AVI �ļ� ���"
#define MSGTR_PREFERENCES_IDX "�ʿ��� ���, �ε��� ���̺� �ٽ� �����"
#define MSGTR_PREFERENCES_VideoCodecFamily "���� �ڵ� ����:"
#define MSGTR_PREFERENCES_AudioCodecFamily "����� �ڵ� ����:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "OSD ����"
#define MSGTR_PREFERENCES_FRAME_Subtitle "�ڸ�"
#define MSGTR_PREFERENCES_FRAME_Font "�۲�"
#define MSGTR_PREFERENCES_FRAME_PostProcess "����ó��"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "�ڵ� & �ؼ���"
#define MSGTR_PREFERENCES_FRAME_Cache "ĳ��"
#define MSGTR_PREFERENCES_FRAME_Misc "��Ÿ"
#define MSGTR_PREFERENCES_OSS_Device "��ġ:"
#define MSGTR_PREFERENCES_OSS_Mixer "�ͼ�:"
#define MSGTR_PREFERENCES_Message "���û��׵��� �����Ϸ��� ����⸦ �ٽ� �����ؾ� �մϴ�!"
#define MSGTR_PREFERENCES_DXR3_VENC "���� ���ڴ�:"
#define MSGTR_PREFERENCES_DXR3_LAVC "LAVC(ffmpeg) ���"
#define MSGTR_PREFERENCES_DXR3_FAME "FAME ���"
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
#define MSGTR_PREFERENCES_FontNoAutoScale "�ڵ� ũ������ ��"
#define MSGTR_PREFERENCES_FontPropWidth "��ũ���� �ʺ� ���"
#define MSGTR_PREFERENCES_FontPropHeight "��ũ���� ���̿� ���"
#define MSGTR_PREFERENCES_FontPropDiagonal "��ũ���� �밢���� ���"
#define MSGTR_PREFERENCES_FontEncoding "���ڵ�:"
#define MSGTR_PREFERENCES_FontBlur "�帲:"
#define MSGTR_PREFERENCES_FontOutLine "�ܰ���:"
#define MSGTR_PREFERENCES_FontTextScale "�ؽ�Ʈ ũ������:"
#define MSGTR_PREFERENCES_FontOSDScale "OSD ũ������:"
#define MSGTR_PREFERENCES_SubtitleOSD "�ڸ� & OSD"
#define MSGTR_PREFERENCES_Cache "ĳ�� �ѱ�/����"
#define MSGTR_PREFERENCES_LoadFullscreen "��üȭ������ ����"


// --- the diffs between help_mp-ko and help_mp-en in MPlayer-20021202
#define MSGTR_PREFERENCES_Misc "��Ÿ"
#define MSGTR_PREFERENCES_CacheSize "ĳ�� ũ��: "
#define MSGTR_PREFERENCES_XSCREENSAVER "X��ũ�����̹� ����"
#define MSGTR_PREFERENCES_AutoSync "�ڵ� ����ȭ �ѱ�/����"
#define MSGTR_PREFERENCES_AutoSyncValue "�ڵ� ����ȭ: "


// --- the diffs between help_mp-ko and help_mp-en in MPlayer-0.90rc1
#define MSGTR_MENU_AudioTrack "����� Ʈ��"
#define MSGTR_MENU_Track "Ʈ�� %d"
#define MSGTR_MENU_VideoTrack "���� Ʈ��"


// --- the diffs between help_mp-ko and help_mp-en in MPlayer-0.90rc2
#define MSGTR_BuiltinCodecsConf "����� �⺻ codecs.conf ���\n"

// --- the diffs between help_mp-ko and help_mp-en in MPlayer-0.90rc3
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- MPlayer�� '�߸��� ����'���� ����Ǿ����ϴ�.\n"\
"  ��Ÿ�� CPU ���� �ڵ忡 ���װ� ���� ���� �𸨴ϴ�...\n"\
"  DOCS/bugreports.html�� �����ϼ���.\n"
#define MSGTR_Exit_SIGILL \
"- MPlayer�� '�߸��� ����'���� ����Ǿ����ϴ�.\n"\
"  ������/����ȭ�� CPU�� �ٸ� �𵨿��� ������ ��\n"\
"  ���� �Ͼ�� �����Դϴ�.\n  üũ�� ������!\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- MPlayer�� �߸��� CPU/FPU/RAM�� ������� ����Ǿ����ϴ�.\n"\
"  MPlayer�� --enable-debug �ɼ����� �ٽ� �������ϰ�, 'gdb' ��Ʈ���̽� ��\n"\
"  �𽺾�����غ�����. �ڼ��� ���� DOCS/bugreports.html#crash�� �����ϼ���.\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer�� �� �� ���� ������ ����Ǿ����ϴ�.\n"\
"  MPlayer �ڵ峪 ����̹��� ����, Ȥ�� gcc������ ������ ���� �ֽ��ϴ�.\n"\
"  MPlayer�� ������� �����Ѵٸ�, DOCS/bugreports.html�� �а� �ű��ִ�\n"\
"  ������ �Ͻñ� �ٶ��ϴ�. ������ ���׸� ������ ��, �� ������ �����ϼ���.\n"\
"  �׷��� ������, ������ ����� �����ϴ�.\n"
#define MSGTR_MEncoderMP3LameHelp "\n\n"\
" vbr=<0-4>     ���� ��Ʈ����Ʈ ���\n"\
"                0: cbr\n"\
"                1: mt\n"\
"                2: rh(�⺻��)\n"\
"                3: abr\n"\
"                4: mtrh\n"\
"\n"\
" abr           ��� ��Ʈ����Ʈ\n"\
"\n"\
" cbr           ���� ��Ʈ����Ʈ\n"\
"               �Ϸ��� ABR ������ ���鿡�� CBR��� ���� �����.\n"\
"\n"\
" br=<0-1024>   ��Ʈ����Ʈ�� kBit������ ���� (CBR �� ABR������)\n"\
"\n"\
" q=<0-9>       ���� (0-�ְ�, 9-����) (VBR������)\n"\
"\n"\
" aq=<0-9>      ���� ���� (0-�ְ�/����, 9-����/����)\n"\
"\n"\
" ratio=<1-100> �����\n"\
"\n"\
" vol=<0-10>    ����� �Է� ���� ����\n"\
"\n"\
" mode=<0-3>    (�⺻��: �ڵ�)\n"\
"                0: ���׷���\n"\
"                1: ����Ʈ-���׷���\n"\
"                2: ���ä��\n"\
"                3: ���\n"\
"\n"\
" padding=<0-2>\n"\
"                0: ����\n"\
"                1: ���\n"\
"                2: ����\n"\
"\n"\
" fast          �Ϸ��� VBR ������ ���鿡�� �� ���� ���ڵ� ���,\n"\
"               ������ ���� ���ϵǰ� ��Ʈ����Ʈ�� ���� �� ������.\n"\
"\n"\
" preset=<value> ������ ������ ���� ���õ�.\n"\
"                 medium: VBR  ���ڵ�, ���� ����\n"\
"                 (150-180 kbps ��Ʈ����Ʈ ����)\n"\
"                 standard:  VBR ���ڵ�, ���� ����\n"\
"                 (170-210 kbps ��Ʈ����Ʈ ����)\n"\
"                 extreme: VBR ���ڵ�, �ſ� ���� ����\n"\
"                 (200-240 kbps ��Ʈ����Ʈ ����)\n"\
"                 insane:  CBR  ���ڵ�, ���� ���� ����\n"\
"                 (320 kbps ��Ʈ����Ʈ ����)\n"\
"                 <8-320>: �־��� kbps ��Ʈ����Ʈ�� ���ġ�� ABR ���ڵ�.\n\n"
#define MSGTR_LeaveTelecineMode "\ndemux_mpg: Progressive ������ ����, 3:2 TELECINE ��带 �����մϴ�.\n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: 3:2 TELECINE ����. ������ telecine fx�� ����մϴ�. FPS�� %5.3f�� ����!\n"
#define MSGTR_SDLPreferences "SDL ����̹� ����"
#define MSGTR_SKIN_WARNING3 "[��Ų] ��Ų ���� ������ %d ���� ���: �� ������ ���� �������� �������� �ʽ��ϴ�. (%s)"
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "Ÿ�̸�, �ۼ�Ʈ�� ��ü�ð�"
#define MSGTR_PREFERENCES_SUB_Overlap "�ڸ� ��ħ �ѱ�"
#define MSGTR_PREFERENCES_Codecs "�ڵ� & �ؼ���"
#define MSGTR_PREFERENCES_SDL_Driver "����̹�:"
#define MSGTR_PREFERENCES_PlayBar "���ǥ���� ���"
#define MSGTR_PREFERENCES_CDROMDevice "CD-ROM ��ġ:"
#define MSGTR_PREFERENCES_DVDDevice "DVD ��ġ:"

// --- the diffs between help_mp-ko and help_mp-en in MPlayer-0.90rc4
#define MSGTR_AvailableFsType "������ ��üȭ�� ���̾� ���� ���:\n"
#define MSGTR_EQU_Channel1 "ä�� 1:"
#define MSGTR_EQU_Channel2 "ä�� 2:"
#define MSGTR_EQU_Channel3 "ä�� 3:"
#define MSGTR_EQU_Channel4 "ä�� 4:"
#define MSGTR_EQU_Channel5 "ä�� 5:"
#define MSGTR_EQU_Channel6 "ä�� 6:"
#define MSGTR_PREFERENCES_FPS "������ FPS:"
#define MSGTR_PREFERENCES_ShowVideoWindow "���� ���� �� ���� â ���̱�"
#define MSGTR_ABOUT_UHU "GUI ���� ���� by UHU Linux\n"
#define MSGTR_ABOUT_CoreTeam "   MPlayer �ھ� ��:\n"
#define MSGTR_ABOUT_AdditionalCoders "   �� ���� �ڴ�:\n"
#define MSGTR_ABOUT_MainTesters "   ���� �׽���:\n"


/* Translated by:  Volodymyr M. Lisivka <lvm@mystery.lviv.net>
   Was synced with help_mp-en.h: rev 1.20
 ========================= MPlayer help =========================== */

#ifdef HELP_MP_DEFINE_STATIC
static char* banner_text=
"\n\n"
"MPlayer " VERSION "(C) 2000-2003 Arpad Gereoffy (���. DOCS!)\n"
"\n";

static char help_text[]=
"������:   mplayer [��æ�] [path/]filename\n"
"\n"
"��æ�:\n"
" -vo <drv[:dev]> ��¦� �������� � �������� צ��� ������ (������ ���. � '-vo help')\n"
" -ao <drv[:dev]> ��¦� �������� � �������� ��Ħ� ������ (������ ���. � '-ao help')\n"
" -vcd <����� �����> ����� VCD (video cd) ���� � �������� ��ͦ��� �����\n"
#ifdef HAVE_LIBCSS
" -dvdauth <dev>  ��¦� �������� DVD ��� ��������æ� (��� ���������� ���˦�)\n"
#endif
#ifdef USE_DVDREAD
" -dvd <����� ���Ҧ�> ����� DVD �����/���� � �������� ��ͦ��� �����\n"
#endif
" -ss <���>     ����ͦ������� �� ������ (������� ��� ��:��:��) ����æ�\n"
" -nosound        ��� �����\n"
#ifdef USE_FAKE_MONO
" -stereo <�����> ��¦� MPEG1 ������ ������ (0:������ 1:̦��� 2:������)\n"
#endif
" -channels <n>   ����� ��Ȧ���� ����̦� �����\n"
" -fs -vm -zoom   ��æ� �������������� ����������� (fullscr,vidmode chg,softw.scale)\n"
" -x <x> -y <y>   ����������� �������� �� <x> * <y> [���� -vo ������� Ц�����դ!]\n"
" -sub <file>     ������� ���� ������Ҧ� (���. ����� -subfps, -subdelay)\n"
" -playlist <file> ������� playlist\n"
" -vid x -aid y   ��æ� ��� ������ צ��� (x) � ��Ħ� (y) ������ ��� �����������\n"
" -fps x -srate y ��æ� ��� �ͦ�� צ��� (x ����/���) � ��Ħ� (y Hz) �������Ԧ\n"
" -pp <quality>   ��������� Ʀ���� (0-4 ��� DivX, 0-63 ��� mpegs)\n"
" -nobps          ��������������� �������������� ����� ������Φ��æ� A-V ��� AVI ���̦� (���� ���������!)\n"
" -framedrop      ��������� ������ ���Ҧ� (��� ��צ����� �����)\n"
" -wid <id צ���>  ��������������� ������� צ��� ��� צ��� ������ (������� ��� plugger!)\n"
"\n"
"���צۦ:\n"
" <-  ��� ->      ������������� ������/����� �� 10 ������\n"
" ����� ��� ����  ������������� ������/����� ��  1 �������\n"
" < ��� >         ������������� ������/����� � ������ �����������\n"
" p ��� �����    �������� Ʀ��� (����-��� ���צ�� - ����������)\n"
" q ��� ESC       �������� צ��������� � ��Ȧ�\n"
" + ��� -         ���������� �������� ����� �� +/- 0.1 �����Ħ\n"
" o               ���̦���� ����¦� OSD ����ͦ�:  ���� / ��צ��æ� / ��צ��æ�+������\n"
" * ��� /         ������ ��� �������� ���Φ��� (���������� 'm' ������� master/pcm)\n"
" z ��� x         ���������� �������� ������Ҧ� �� +/- 0.1 �����Ħ\n"
"\n"
" * * * ��������� ���. �����������, ��� ��������� ��㶷 � ����� ! * * *\n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// mplayer.c: 

#define MSGTR_Exiting "\n��������... (%s)\n"
#define MSGTR_Exit_quit "��Ȧ�"
#define MSGTR_Exit_eof "����� �����"
#define MSGTR_Exit_error "�������� �������"
#define MSGTR_IntBySignal "\nMPlayer ���������� �������� %d � ����̦: %s \n"
#define MSGTR_NoHomeDir "�� ���� ������ �����Φ� �������\n"
#define MSGTR_GetpathProblem "�������� � get_path(\"config\")\n"
#define MSGTR_CreatingCfgFile "��������� ����� ���Ʀ����æ�: %s\n"
#define MSGTR_InvalidVOdriver "������������ ��'� �������� צ��� ������: %s\n���. '-vo help' ��� �������� ������ ��������� ������Ҧ�.\n"
#define MSGTR_InvalidAOdriver "������������ ��'� �������� ��Ħ� ������: %s\n���. '-ao help' ��� �������� ������ ��������� ������Ҧ�.\n"
#define MSGTR_CopyCodecsConf "(���Ц���� etc/codecs.conf (� ����Ԧ� MPlayer) � ~/.mplayer/codecs.conf)\n"
#define MSGTR_CantLoadFont "�� ���� ����������� �����: %s\n"
#define MSGTR_CantLoadSub "�� ���� ����������� ��������: %s\n"
#define MSGTR_ErrorDVDkey "������� ������� DVD �����.\n"
#define MSGTR_CmdlineDVDkey "��������� ����� DVD ������� ��������� ���� ��� ������������.\n"
#define MSGTR_DVDauthOk "��������æ� DVD - ��� ������.\n"
#define MSGTR_DumpSelectedStreamMissing "dump: FATAL: ������� ��Ԧ� ����������!\n"
#define MSGTR_CantOpenDumpfile "�� ���� צ������ ���� �����!!!\n"
#define MSGTR_CoreDumped "core dumped :)\n"
#define MSGTR_FPSnotspecified "���˦��� ���Ҧ� �� ������� �� ������� (��� ������������ ��������) � ���������! �������������� ���� -fps!\n"
#define MSGTR_TryForceAudioFmt "������ ��������� ����������� Ӧ������� ��Ħ� ����˦� %d ...\n"
#define MSGTR_CantFindAfmtFallback "�� ���� ������ ��Ħ� ����� ��� ��������� Ӧ�������, ����Ȧ� �� ��ۦ ��������.\n"
#define MSGTR_CantFindAudioCodec "�� ���� ������ ����� ��� ��Ħ� ������� 0x%X !\n"
#define MSGTR_TryUpgradeCodecsConfOrRTFM "*** ��������� ������� %s � etc/codecs.conf\n*** ���� �� ��������� - ������� DOCS/codecs.html!\n"
#define MSGTR_CouldntInitAudioCodec "�� �ͦ� �Φæ�̦������ ��Ħ� �����! -> ���� ��� �����\n"
#define MSGTR_TryForceVideoFmt "������ ��������� ����������� Ӧ������� צ��� ����˦� %d ...\n"
#define MSGTR_CantFindVideoCodec "�� ���� ������ ����� ��� צ��� ������� 0x%X !\n"
#define MSGTR_VOincompCodec "�������, ������� video_out �����Ҧ� �� ��ͦ���� � ��� �������.\n"
#define MSGTR_CannotInitVO "FATAL: �� ���� �Φæ�̦������ צ��� �������!\n"
#define MSGTR_CannotInitAO "�� ���� צ������/�Φæ�̦������ ��Ħ� �����Ҧ� -> ���� ��� �����\n"
#define MSGTR_StartPlaying "������� �����������...\n"
#define MSGTR_SystemTooSlow "\n\n"\
"         *****************************************************************\n"\
"         **** ���� ������� ����� �������� ��� צ�������� ��! ****\n"\
"         *****************************************************************\n"\
"!!! �����צ �������, ��������, ��Ȧ�Φ �����: \n"\
"- ���¦��� ������Φ: �������/����� _��Ħ�_ ������� : ��������� -ao sdl ���\n"\
"  �������������� ALSA 0.5 ��� �����æ� oss �� ALSA 0.9. ������� DOCS/sound.html!\n"\
"- ��צ����� צ��� ��צ�. ��������� ����� -vo ������� (������: -vo help) ���\n"\
"  ��������� � -framedrop ! ������� DOCS/video.html.\n"\
"- ��צ����� ��. �� ����������� צ���������� ����˦ dvd/divx �� ��צ�����\n"\
"  ����������! ��������� -hardframedrop\n"\
"- ����� ����. ��������� Ҧ�Φ ���¦��æ�: -nobps  -ni  -mc 0  -forceidx\n"\
"���� Φ���� �� ���������, ��Ħ ������� DOCS/bugreports.html !\n\n"

#define MSGTR_NoGui "MPlayer ��� ����Ц�������� ��� Ц������� GUI!\n"
#define MSGTR_GuiNeedsX "MPlayer GUI ������� X11!\n"
#define MSGTR_Playing "����������� %s\n"
#define MSGTR_NoSound "��Ħ�: ��� �����!!!\n"
#define MSGTR_FPSforced "��������� �ͦ���� ˦��˦��� ���Ҧ� �� ������� �� %5.3f (ftime: %5.3f)\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "��������צ� \"%s\" �� ���������!\n"
#define MSGTR_ErrTrackSelect "������� ������ ����� �� VCD!"
#define MSGTR_ReadSTDIN "������� � stdin...\n"
#define MSGTR_UnableOpenURL "�� ���� צ������ URL: %s\n"
#define MSGTR_ConnToServer "�'������� � ��������: %s\n"
#define MSGTR_FileNotFound "���� �� ���������: '%s'\n"

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
#define MSGTR_MaybeNI "(������� �� ��������� ��������������� ��Ԧ�/���� ��� �������� �����)\n"
#define MSGTR_Detected_XXX_FileFormat "��������� %s ������ �����!\n"
#define MSGTR_InvalidMPEGES "������������� MPEG-ES ��Ԧ�??? ��'�֦���� � �������, �� ������ ������� :(\n"
#define MSGTR_FormatNotRecognized "========= �������, ������ ����� ����� �� ���Ц������ �� �� Ц�����դ���� ===========\n"\
				  "===== ���� �� AVI, ASF ��� MPEG ��Ԧ�, ���� ����� ��'�֦���� � �������! ======\n"
#define MSGTR_MissingVideoStream "����� ��Ԧ� �� ���������!\n"
#define MSGTR_MissingAudioStream "��Ħ� ��Ԧ� �� ���������...  -> ������� ��� �����\n"
#define MSGTR_MissingVideoStreamBug "����� ��Ԧ� ����������!? ��'�֦���� � �������, �� ������ ������� :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: ���� �� ͦ����� ������� ��Ħ� ��� צ��� ��Ԧ�\n"

#define MSGTR_NI_Forced "��������� ��������"
#define MSGTR_NI_Detected "���������"
#define MSGTR_NI_Message "%s ��������������� ������ AVI �����!\n"

#define MSGTR_UsingNINI "������������ ���������������� ��� ������������ ������� AVI �����!\n"
#define MSGTR_CouldntDetFNo "�� �ͦ� ��������� ����� ���Ҧ� (��� ����������� �����������)\n"
#define MSGTR_CantSeekRawAVI "�� ���� ����ͦ������� � ����Ϧ������������ ����æ .AVI! (����������� ������, ��������� � ������ -idx!)\n"
#define MSGTR_CantSeekFile "�� ���� ����ͦ�������� � ����� ���̦!\n"

#define MSGTR_EncryptedVOB "���������� VOB ���� (mplayer �� ����Ц�������� � Ц�������� libcss)! ���. DOCS/cd-dvd.html\n"
#define MSGTR_EncryptedVOBauth "���������� ��Ԧ� ��� �� �� �������� ��������æ�!!\n"

#define MSGTR_MOVcomprhdr "MOV: ������Ԧ ��������� (���� ��) �� Ц�����������!\n"
#define MSGTR_MOVvariableFourCC "MOV: ������������! �������� ����ͦ���� FOURCC!?\n"
#define MSGTR_MOVtooManyTrk "MOV: ������������! ����� ������ ���˦�!"

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

// LIRC:
#define MSGTR_SettingUpLIRC "������������ Ц������� lirc...\n"
#define MSGTR_LIRCdisabled "�� �� ������� ��������������� ���� צ������� ���������\n"
#define MSGTR_LIRCopenfailed "������� צ������� Ц������� lirc!\n"
#define MSGTR_LIRCcfgerr "������� ������� ����� ���Ʀ����æ� LIRC %s !\n"


// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "��� ��������"
#define MSGTR_FileSelect "������� ���� ..."
#define MSGTR_SubtitleSelect "������� �������� ..."
#define MSGTR_OtherSelect "��¦� ..."
#define MSGTR_PlayList "������ �����������"
#define MSGTR_SkinBrowser "���������� ����Φ�"

// --- buttons ---
#define MSGTR_Ok "���"
#define MSGTR_Cancel "���������"
#define MSGTR_Add "������"
#define MSGTR_Remove "��������"

// --- error messages ---
#define MSGTR_NEMDB "�������, �� �������� ���'�Ԧ ��� צ������������� ������."
#define MSGTR_NEMFMR "�������, �� �������� ���'�Ԧ ��� צ���������� ����."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[�����] ������� � ���̦ ���Ʀ����æ� ������, ����� %d  : %s" 
#define MSGTR_SKIN_WARNING1 "[�����] ������������: � ���̦ ���Ʀ����æ� ������, ����� %d: widget ��������� ��� �� ����� �� �������� \"section\" ( %s )"
#define MSGTR_SKIN_WARNING2 "[�����] ������������: � ���̦ ���Ʀ����æ� ������, ����� %d: widget ��������� ��� �� ����� �� �������� \"subsection\" (%s)"
#define MSGTR_SKIN_BITMAP_16bit  "������� ������� ¦���ϧ ����� � 16 ¦� � ����� �� Ц�����դ���� ( %s ).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "���� �� ��������� ( %s )\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "������� ������� bmp ( %s )\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "������� ������� tga ( %s )\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "������� ������� png ( %s )\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "RLE ����������� tga �� Ц�����դ���� ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "��צ����� ��� ����� ( %s )\n"
#define MSGTR_SKIN_BITMAP_ConvertError "������� ������������ 24-¦� � 32-¦� ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "��צ���� ��צ��������: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "�� �������� ���'�Ԧ\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "��������� ����� ������ ����Ԧ�\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "���� ������ �� ���������\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "���� ����ڦ� ������ �� ���������\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "�Ŧ������� ������Ʀ����� ������ ( %s )\n"
#define MSGTR_SKIN_UnknownParameter "��צ����� �������� ( %s )\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[���������� ����Φ�] �� �������� ���'�Ԧ.\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "����� �� �������� ( %s ).\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "������� ������� ����� ���Ʀ����æ� ������ ( %s ).\n"
#define MSGTR_SKIN_LABEL "������:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "��� ��������"
#define MSGTR_MENU_Open "�������� ..."
#define MSGTR_MENU_PlayFile "����� ���� ..."
#define MSGTR_MENU_PlayVCD "����� VCD ..."
#define MSGTR_MENU_PlayDVD "����� DVD ..."
#define MSGTR_MENU_PlayURL "����� URL ..."
#define MSGTR_MENU_LoadSubtitle "����������� �������� ..."
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
#define MSGTR_MENU_PlayDisc "����� ���� ..."
#define MSGTR_MENU_ShowDVDMenu "�������� DVD ����"
#define MSGTR_MENU_Titles "�����"
#define MSGTR_MENU_Title "���� %2d"
#define MSGTR_MENU_None "(����)"
#define MSGTR_MENU_Chapters "���Ħ��"
#define MSGTR_MENU_Chapter "���Ħ� %2d"
#define MSGTR_MENU_AudioLanguages "���� ����"
#define MSGTR_MENU_SubtitleLanguages "���� ������Ҧ�"
#define MSGTR_MENU_PlayList "������ �����������"
#define MSGTR_MENU_SkinBrowser "���������� ����Φ�"
#define MSGTR_MENU_Preferences "������������"
#define MSGTR_MENU_Exit "��Ȧ� ..."

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "�������� ������� ..."
#define MSGTR_MSGBOX_LABEL_Error "������� ..."
#define MSGTR_MSGBOX_LABEL_Warning "������������ ..." 

#endif

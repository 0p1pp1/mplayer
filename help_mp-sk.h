// Translated by:  Daniel Be�a, benad@centrum.cz
// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char* banner_text=
"\n\n"
"MPlayer " VERSION "(C) 2000-2002 Arpad Gereoffy (vi� DOCS!)\n"
"\n";

// Preklad do sloven�iny 

static char help_text[]=
#ifdef HAVE_NEW_GUI
"Pou�itie:   mplayer [-gui] [prep�na�e] [cesta/]menos�boru\n"
#else
"Pou�itie:   mplayer [prep�na�e] [cesta/]menos�boru\n"
#endif
"\n"
"Prep�na�e:\n"
" -vo <drv[:dev]> v�ber v�stup. video ovl�da�a&zariadenia (-vo help pre zoznam)\n"
" -ao <drv[:dev]> v�ber v�stup. audio ovl�da�a&zariadenia (-ao help pre zoznam)\n"
" -vcd <trackno>  prehra� VCD (video cd) stopu zo zariadenia namiesto zo s�boru\n"
#ifdef HAVE_LIBCSS
" -dvdauth <dev>  ur�enie DVD zariadenia pre overenie autenticity (pre k�dovan� disky)\n"
#endif
#ifdef USE_DVDREAD
" -dvd <titleno>  prehra� DVD titul/stopu zo zariadenia (mechaniky) namiesto s�boru\n"
#endif
" -ss <timepos>   posun na poz�ciu (sekundy alebo hh:mm:ss)\n"
" -nosound        prehr�va� bez zvuku\n"
#ifdef USE_FAKE_MONO
" -stereo <mode>  v�ber audiokan�lu pre MPEG1 (0:stereo 1:�av� 2:prav�)\n"
#endif
" -channels <n>   po�et v�stupn�ch zvukov�ch kan�lov\n"
" -fs -vm -zoom   vo�by pre prehr�vanie na cel� obrazovku (cel� obrazovka\n                 meni� videore�im, softv�rov� zoom)\n"
" -x <x> -y <y>   zvet�enie obrazu na rozmer <x>*<y> (pokia� to vie -vo ovl�da�!)\n"
" -sub <file>     vo�ba s�boru s titulkami (vi� tie� -subfps, -subdelay)\n"
" -playlist <file> ur�enie s�boru so zoznamom prehr�van�ch s�borov\n"
" -vid x -aid y   v�ber ��sla video (x) a audio (y) pr�du pre prehr�vanie\n"
" -fps x -srate y vo�ba pre zmenu video (x fps) a audio (y Hz) frekvencie\n"
" -pp <quality>   aktiv�cia postprocesing filtra (0-4 pre DivX, 0-63 pre mpegy)\n"
" -nobps          pou�i� alternat�vnu A-V synchroniza�n� met�du pre Avi s�bory\n"
" -framedrop      povoli� zahadzovanie sn�mkov (pre pomal� stroje)\n"
" -wid <window id> pou�i� existuj�ce okno pre v�stup videa\n"
"\n"
"Kl�vesy:\n"
" <-  alebo  ->   posun vzad/vpred o 10 sekund\n"
" hore / dole     posun vzad/vpred o  1 min�tu\n"
" < alebo >       posun vzad/vpred v zozname prehr�van�ch s�borov\n"
" p al. medzern�k pauza pri prehr�van� (pokra�ovan� stla�en�m niektorej kl�vesy)\n"
" q alebo ESC     koniec prehr�vania a ukon�enie programu\n"
" + alebo -       upravi� spozdenie zvuku v krokoch +/- 0.1 sekundy\n"
" o               cyklick� zmena re�imu OSD:  ni� / poz�cia / poz�cia+�as\n"
" * alebo /       prida� alebo ubra� hlasitos� (stla�en�m 'm' v�ber master/pcm)\n"
" z alebo x       upravi� spozdenie titulkov v krokoch +/- 0.1 sekundy\n"
"\n"
" * * * * PRE��TAJTE SI MAN STR�NKU PRE DETAILY (�AL�IE VO�BY A KL�VESY)! * * * *\n"
"\n";
#endif

// ========================= MPlayer messages ===========================
// mplayer.c:

#define MSGTR_Exiting "\nKon��m... (%s)\n"
#define MSGTR_Exit_frames "Po�adovan� po�et sn�mkov prehran�"
#define MSGTR_Exit_quit "Koniec"
#define MSGTR_Exit_eof "Koniec s�boru"
#define MSGTR_Exit_error "Z�va�n� chyba"
#define MSGTR_IntBySignal "\nMPlayer preru�en� sign�lom %d v module: %s \n"
#define MSGTR_NoHomeDir "Nem��em najs� dom�ci (HOME) adres�r\n"
#define MSGTR_GetpathProblem "get_path(\"config\") probl�m\n"
#define MSGTR_CreatingCfgFile "Vytv�ram konfigura�n� s�bor: %s\n"
#define MSGTR_InvalidVOdriver "Neplatn� meno v�stupn�ho videoovl�da�a: %s\nPou�ite '-vo help' pre zoznam dostupn�ch ovl�da�ov.\n"
#define MSGTR_InvalidAOdriver "Neplatn� meno v�stupn�ho audioovl�da�a: %s\nPou�ite '-ao help' pre zoznam dostupn�ch ovl�da�ov.\n"
#define MSGTR_CopyCodecsConf "(copy/ln etc/codecs.conf (zo zdrojov�ch k�dov MPlayeru) do ~/.mplayer/codecs.conf)\n"
#define MSGTR_CantLoadFont "Nem��em na��ta� font: %s\n"
#define MSGTR_CantLoadSub "Nem��em na��ta� titulky: %s\n"
#define MSGTR_ErrorDVDkey "Chyba pri spracovan� k���a DVD.\n"
#define MSGTR_CmdlineDVDkey "DVD k��� po�adovan� na pr�kazovom riadku je uschovan� pre rozk�dovanie.\n"
#define MSGTR_DVDauthOk "DVD sekvencia overenia autenticity vypad� v poriadku.\n"
#define MSGTR_DumpSelectedSteramMissing "dump: FATAL: po�adovan� pr�d ch�ba!\n"
#define MSGTR_CantOpenDumpfile "Nejde otvori� s�bor pre dump!!!\n"
#define MSGTR_CoreDumped "jadro vyp�san� :)\n"
#define MSGTR_FPSnotspecified "V hlavi�ke s�boru nie je udan� (alebo je zl�) FPS! Pou�ite vo�bu -fps !\n"
#define MSGTR_NoVideoStream "�ia�, �iadny videopr�d... to sa zatia� ned� prehra�.\n"
#define MSGTR_TryForceAudioFmt "Pok��am sa vyn�ti� rodinu audiokodeku %d ...\n"
#define MSGTR_CantFindAfmtFallback "Nem��em n�js� audio kodek pre po�adovan� rodinu, pou�ijem ostatn�.\n"
#define MSGTR_CantFindAudioCodec "Nem��em n�js� kodek pre audio form�t 0x%X !\n"
#define MSGTR_TryUpgradeCodecsConfOrRTFM "*** Pok�ste sa upgradova� %s z etc/codecs.conf\n*** Pokia� probl�m pretrv�, pre��tajte si DOCS/CODECS!\n"
#define MSGTR_CouldntInitAudioCodec "Nejde inicializova� audio kodek! -> bez zvuku\n"
#define MSGTR_TryForceVideoFmt "Pok��am se vn�ti� rodinu videokodeku %d ...\n"
#define MSGTR_CantFindVfmtFallback "Nem��em najs� video kodek pre po�adovan� rodinu, pou�ijem ostatn�.\n"
#define MSGTR_CantFindVideoCodec "Nem��em najs� kodek pre video form�t 0x%X !\n"
#define MSGTR_VOincompCodec "�ia�, vybran� video_out zariadenie je nekompatibiln� s t�mto kodekom.\n"
#define MSGTR_CouldntInitVideoCodec "FATAL: Nem��em inicializova� videokodek :(\n"
#define MSGTR_EncodeFileExists "S�bor u� existuje: %s (neprep�te si svoj ob��ben� AVI s�bor!)\n"
#define MSGTR_CantCreateEncodeFile "Nem��em vytvori� s�bor pre encoding\n" 
#define MSGTR_CannotInitVO "FATAL: Nem��em inicializova� video driver!\n"
#define MSGTR_CannotInitAO "nem��em otvori�/inicializova� audio driver -> TICHO\n"
#define MSGTR_StartPlaying "Za��nam prehr�va�...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"         ***********************************************************\n"\
"         ****  Na prehratie tohoto je v� syst�m pr�li� POMAL�!  ****\n"\
"         ***********************************************************\n"\
"!!! Mo�n� pr��iny, probl�my a rie�enia:\n"\
"- Nej�astej�ie: nespr�vny/chybn� _zvukov�_ ovl�da�. Rie�enie: sk�ste -ao sdl al. pou�ite\n"\
"  ALSA 0.5 nebo oss emul�ciu z ALSA 0.9. viac tipov sa dozviete v DOCS/sound.html!\n"\
"- Pomal� video v�stup. Sk�ste in� -vo ovl�da� (pre zoznam: -vo help) alebo sk�ste\n"\
"  s vo�bou -framedrop !  Tipy pre ladenie/zr�chlenie videa s� v DOCS/video.html\n"\
"- Pomal� cpu. Nesk��ajte prehr�va� ve�k� dvd/divx na pomalom cpu! Sk�ste -hardframedrop\n"\
"- Po�koden� s�bor. Sk�ste r�zne kombin�cie t�chto volieb: -nobps  -ni  -mc 0  -forceidx\n"\
"Pokia� ni� z toho nie je pravda, pre��tajte si DOCS/bugreports.html !\n\n"

#define MSGTR_NoGui "MPlayer bol prelo�en� BEZ podpory GUI!\n"
#define MSGTR_GuiNeedsX "MPlayer GUI vy�aduje X11!\n"
#define MSGTR_Playing "Prehr�vam %s\n"
#define MSGTR_NoSound "Audio: bez zvuku!!!\n"
#define MSGTR_FPSforced "FPS vn�ten� na hodnotu %5.3f  (ftime: %5.3f)\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "CD-ROM zariadenie '%s' nen�jden�!\n"
#define MSGTR_ErrTrackSelect "Chyba pri v�bere VCD stopy!"
#define MSGTR_ReadSTDIN "��tam z stdin...\n"
#define MSGTR_UnableOpenURL "Nejde otvori� URL: %s\n"
#define MSGTR_ConnToServer "Pripojen� k servru: %s\n"
#define MSGTR_FileNotFound "S�bor nen�jden�: '%s'\n"

#define MSGTR_CantOpenDVD "Nejde otvori� DVD zariadenie: %s\n"
#define MSGTR_DVDwait "��tam �trukt�ru disku, pros�m �akajte...\n"
#define MSGTR_DVDnumTitles "Na tomto DVD je %d titulov.\n"
#define MSGTR_DVDinvalidTitle "Neplatn� ��slo DVD titulu: %d\n"
#define MSGTR_DVDnumChapters "Na tomto DVD je %d kapitol.\n"
#define MSGTR_DVDinvalidChapter "Neplatn� ��slo kapitoly DVD: %d\n"
#define MSGTR_DVDnumAngles "Na tomto DVD je %d �hlov poh�adov.\n"
#define MSGTR_DVDinvalidAngle "Neplatn� ��slo uhlu poh�adu DVD: %d\n"
#define MSGTR_DVDnoIFO "Nem��em otvori� s�bor IFO pre DVD titul %d.\n"
#define MSGTR_DVDnoVOBs "Nem��em otvori� VOB s�bor (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD �spe�ne otvoren�!\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "Upozornenie! Hlavi�ka audio pr�du %d predefinovan�!\n"
#define MSGTR_VideoStreamRedefined "Upozornenie! Hlavi�ka video pr�du %d predefinovan�!\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: Pr�li� mnoho (%d v %d bajtech) audio paketov v bufferi!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: Pr�li� mnoho (%d v %d bajtech) video paketov v bufferi!\n"
#define MSGTR_MaybeNI "(mo�no prehr�vate neprekladan� pr�d/s�bor alebo kodek zlyhal)\n"
#define MSGTR_DetectedFILMfile "Detekovan� FILM form�t s�boru!\n"
#define MSGTR_DetectedFLIfile "Detekovan� FLI form�t s�boru!\n"
#define MSGTR_DetectedROQfile "Detekovan� ROQ form�t s�boru!\n"
#define MSGTR_DetectedREALfile "Detekovan� REAL form�t s�boru!\n"
#define MSGTR_DetectedAVIfile "Detekovan� AVI form�t s�boru!\n"
#define MSGTR_DetectedASFfile "Detekovan� ASF form�t s�boru!\n"
#define MSGTR_DetectedMPEGPESfile "Detekovan� MPEG-PES form�t s�boru!\n"
#define MSGTR_DetectedMPEGPSfile "Detekovan� MPEG-PS form�t s�boru!\n"
#define MSGTR_DetectedMPEGESfile "Detekovan� MPEG-ES form�t s�boru!\n"
#define MSGTR_DetectedQTMOVfile "Detekovan� QuickTime/MOV form�t s�boru!\n"
#define MSGTR_MissingMpegVideo "Ch�baj�ci MPEG video pr�d!? kontaktujte autora, mo�no je to chyba (bug) :(\n"
#define MSGTR_InvalidMPEGES "Neplatn� MPEG-ES pr�d??? kontaktujte autora, mo�no je to chyba (bug) :(\n"
#define MSGTR_FormatNotRecognized "========== �ia�, tento form�t s�boru nie je rozpoznan�/podporovan� =======\n"\
				  "==== Pokia� je tento s�bor AVI, ASF alebo MPEG pr�d, kontaktujte autora! ====\n"
#define MSGTR_MissingVideoStream "�iadny video pr�d nen�jden�!\n"
#define MSGTR_MissingAudioStream "�iadny audio pr�d nen�jden�...  -> bez zvuku\n"
#define MSGTR_MissingVideoStreamBug "Ch�baj�ci video pr�d!? Kontaktujte autora, mo�no to je chyba (bug) :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: s�bor neobsahuje vybran� audio alebo video pr�d\n"

#define MSGTR_NI_Forced "Vn�ten�"
#define MSGTR_NI_Detected "Detekovan�"
#define MSGTR_NI_Message "%s NEPREKLADAN� form�t s�boru AVI!\n"

#define MSGTR_UsingNINI "Pou��vam NEPREKLADAN� po�koden� form�t s�boru AVI!\n" 
#define MSGTR_CouldntDetFNo "Nem��em ur�i� po�et sn�mkov (pre absol�tny posun)  \n"
#define MSGTR_CantSeekRawAVI "Nem��em sa pos�va� v surov�ch (raw) .AVI pr�doch! (Potrebujem index, zkuste pou�� vo�bu -idx !)  \n"
#define MSGTR_CantSeekFile "Nem��em sa pos�va� v tomto s�bore!  \n"

#define MSGTR_EncryptedVOB "K�dovan� VOB s�bor (prelo�en� bez podpory libcss)! Pre��tajte si DOCS/DVD\n"
#define MSGTR_EncryptedVOBauth "Zak�dovan� pr�d, ale overenie autenticity ste nepo�adovali!!\n"

#define MSGTR_MOVcomprhdr "MOV: Komprimovan� hlavi�ky nie s� (e�te) podporovan�!\n"
#define MSGTR_MOVvariableFourCC "MOV: Upozornenie! premenn� FOURCC detekovan�!?\n"
#define MSGTR_MOVtooManyTrk "MOV: Upozornenie! Pr�li� ve�a st�p!"
#define MSGTR_MOVnotyetsupp "\n****** Quicktime MOV form�t nie je e�te podporovan� !!! *******\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "nem��em otvori� kodek\n"
#define MSGTR_CantCloseCodec "nem��em uzavie� kodek\n"

#define MSGTR_MissingDLLcodec "CHYBA: Nem��em otvori� potrebn� DirectShow kodek: %s\n"
#define MSGTR_ACMiniterror "Nem��em na��ta�/inicializova� Win32/ACM AUDIO kodek (ch�baj�ci s�bor DLL?)\n"
#define MSGTR_MissingLAVCcodec "Nem��em najs� kodek '%s' v libavcodec...\n"

#define MSGTR_NoDShowSupport "MPlayer bol prelo�en� BEZ podpory directshow!\n"
#define MSGTR_NoWfvSupport "Podpora pre kodeky win32 neakt�vna alebo nedostupn� mimo platformy x86!\n"
#define MSGTR_NoDivx4Support "MPlayer bol prelo�en� BEZ podpory DivX4Linux (libdivxdecore.so)!\n"
#define MSGTR_NoLAVCsupport "MPlayer bol prelo�en� BEZ podpory ffmpeg/libavcodec!\n"
#define MSGTR_NoACMSupport "Win32/ACM audio kodek neakt�vny nebo nedostupn� mimo platformy x86 -> bez zvuku :(\n"
#define MSGTR_NoDShowAudio "Prelo�en� BEZ podpory DirectShow -> bez zvuku :(\n"
#define MSGTR_NoOggVorbis "OggVorbis audio kodek neakt�vny -> bez zvuku :(\n"
#define MSGTR_NoXAnimSupport "MPlayer bol prelo�en� BEZ podpory XAnim!\n"

#define MSGTR_MpegPPhint "Upozornenie! Po�adujete video postprocesing pre MPEG 1/2, ale MPlayer bol\n" \
			 "         prelo�en� bez podpory postprocesingu MPEG 1/2!\n" \
			 "         #define MPEG12_POSTPROC v config.h a prelo�te znovu libmpeg2!\n"
#define MSGTR_MpegNoSequHdr "MPEG: FATAL: EOF - koniec s�boru v priebehu vyh�ad�vania hlavi�ky sekvencie\n"
#define MSGTR_CannotReadMpegSequHdr "FATAL: Nem��em pre��ta� hlavi�ku sekvencie!\n"
#define MSGTR_CannotReadMpegSequHdrEx "FATAL: Nem��em pre��ta� roz��renie hlavi�ky sekvencie!\n"
#define MSGTR_BadMpegSequHdr "MPEG: Zl� hlavi�ka sekvencie!\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: Zl� roz��renie hlavi�ky sekvencie!\n"

#define MSGTR_ShMemAllocFail "Nem��em alokova� zdie�an� pam�\n"
#define MSGTR_CantAllocAudioBuf "Nem��em alokova� pam� pre v�stupn� audio buffer\n"
#define MSGTR_NoMemForDecodedImage "nedostatok pam�te pre buffer na dek�dovanie obrazu (%ld bytes)\n"

#define MSGTR_AC3notvalid "Neplatn� AC3 pr�d.\n"
#define MSGTR_AC3only48k "Iba pr�dy o frekvencii 48000 Hz s� podporovan�.\n"
#define MSGTR_UnknownAudio "Nezn�my/ch�baj�ci audio form�t -> bez zvuku\n"

// LIRC:
#define MSGTR_SettingUpLIRC "Nastavujem podporu lirc ...\n"
#define MSGTR_LIRCdisabled "Nebudete m�c� pou��va� dia�kov� ovl�da�.\n"
#define MSGTR_LIRCopenfailed "Zlyhal pokus o otvorenie podpory LIRC!\n"
#define MSGTR_LIRCsocketerr "Nejak� chyba so soketom lirc: %s\n"
#define MSGTR_LIRCcfgerr "Zlyhalo ��tanie konfigura�n�ho s�boru LIRC %s !\n"


// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "O aplik�cii"
#define MSGTR_FileSelect "Vybra� s�bor ..."
#define MSGTR_SubtitleSelect "Vybra� titulky ..."
#define MSGTR_MessageBox "MessageBox"
#define MSGTR_PlayList "PlayList"
#define MSGTR_SkinBrowser "Prehliada� t�m"
#define MSGTR_OtherSelect "Vybra� ..."

// --- buttons ---
#define MSGTR_Ok "Ok"
#define MSGTR_Cancel "Zru�i�"
#define MSGTR_Add "Prida�"
#define MSGTR_Remove "Odobra�"

// --- error messages ---
#define MSGTR_NEMDB "�ia�, nedostatok pam�te pre buffer na kreslenie."
#define MSGTR_NEMFMR "�ia�, nedostatok pam�te pre vytv�ranie menu."
#define MSGTR_NEMFMM "�ia�, nedostatok pam�te pre masku hlavn�ho okna."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[t�my] chyba v konfigura�nom s�bore t�m %d: %s"
#define MSGTR_SKIN_WARNING1 "[t�my] v konfigura�nom s�bore t�m na riadku %d: widget najden� ale pred  \"section\" nen�jden� ( %s )"
#define MSGTR_SKIN_WARNING2 "[t�my] v konfigura�nom s�bore t�m na riadku %d: widget najden� ale pred \"subsection\" nen�jden� (%s)"
#define MSGTR_SKIN_BITMAP_16bit  "bitmapa s h�bkou 16 bit a menej je nepodporovan� ( %s ).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "s�bor nen�jden� ( %s )\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "chyba ��tania bmp ( %s )\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "chyba ��tania tga ( %s )\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "chyba ��tania png ( %s )\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "form�t RLE packed tga nepodporovan� ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "nezn�my typ s�boru ( %s )\n"
#define MSGTR_SKIN_BITMAP_ConvertError "chyba konverzie z 24 bit do 32 bit ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "nezn�ma spr�va: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "nedostatok pam�te\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "pr�li� mnoho fontov deklarovan�ch\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "s�bor fontov nen�jden�\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "s�bor obrazov fontu nen�jden�\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "neexistuj�ci identifik�tor fontu ( %s )\n"
#define MSGTR_SKIN_UnknownParameter "nezn�my parameter ( %s )\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[prehliada� t�m] nedostatok pam�te.\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "Skin nen�jden� ( %s ).\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "Chyba pri ��tan� konfigura�n�ho s�boru t�m ( %s ).\n"
#define MSGTR_SKIN_LABEL "T�my:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "O aplik�cii MPlayer"
#define MSGTR_MENU_Open "Otvori� ..."
#define MSGTR_MENU_PlayFile "Prehra� s�bor ..."
#define MSGTR_MENU_PlayVCD "Prehra� VCD ..."
#define MSGTR_MENU_PlayDVD "Prehra� DVD ..."
#define MSGTR_MENU_PlayURL "Prehra� URL ..."
#define MSGTR_MENU_LoadSubtitle "Na��ta� titulky ..."
#define MSGTR_MENU_Playing "Prehr�vam"
#define MSGTR_MENU_Play "Prehra�"
#define MSGTR_MENU_Pause "Pauza"
#define MSGTR_MENU_Stop "Zastavi�"
#define MSGTR_MENU_NextStream "�al�� pr�d"
#define MSGTR_MENU_PrevStream "Predch�dzaj�ci pr�d"
#define MSGTR_MENU_Size "Ve�kos�"
#define MSGTR_MENU_NormalSize "Norm�lna ve�kos�"
#define MSGTR_MENU_DoubleSize "Dvojn�sobn� ve�kos�"
#define MSGTR_MENU_FullScreen "Cel� obrazovka"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_PlayDisc "Prehra� disk ..."
#define MSGTR_MENU_ShowDVDMenu "Zobrazi� DVD menu"
#define MSGTR_MENU_Titles "Tituly"
#define MSGTR_MENU_Title "Titul %2d"
#define MSGTR_MENU_None "(ni�)"
#define MSGTR_MENU_Chapters "Kapitoly"
#define MSGTR_MENU_Chapter "Kapitola %2d"
#define MSGTR_MENU_AudioLanguages "Jazyk zvuku"
#define MSGTR_MENU_SubtitleLanguages "Jazyk titulkov"
#define MSGTR_MENU_PlayList "Playlist"
#define MSGTR_MENU_SkinBrowser "Prehliada� t�m"
#define MSGTR_MENU_Preferences "Predvo�by"
#define MSGTR_MENU_Exit "Koniec ..."

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "fat�lna chyba ..."
#define MSGTR_MSGBOX_LABEL_Error "chyba ..."
#define MSGTR_MSGBOX_LABEL_Warning "upozornenie ..."

#endif


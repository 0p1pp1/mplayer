// Translated by:  Jiri Svoboda, jiri.svoboda@seznam.cz

// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char* banner_text=
"\n\n"
"MPlayer " VERSION "(C) 2000-2001 Arpad Gereoffy (viz DOCS!)\n"
"\n";

// P�eklad do �e�tiny Ji�� Svoboda

static char help_text[]=
#ifdef HAVE_NEW_GUI
"Pou�it�:   mplayer [-gui] [p�ep�na�e] [cesta/]jmenosouboru\n"
#else
"Pou�it�:   mplayer [p�ep�na�e] [cesta/]jmenosouboru\n"
#endif
"\n"
"P�ep�na�e:\n"
" -vo <drv[:dev]> v�b�r v�stupn�ho video ovlada�&za��zen� ('-vo help' pro seznam)\n"
" -ao <drv[:dev]> v�b�r v�stupn�ho audio ovlada�&za��zen� ('-ao help' pro seznam)\n"
" -vcd <trackno>  p�ehr�t VCD (video cd) stopu ze za��zen� m�sto souboru\n"
#ifdef HAVE_LIBCSS
" -dvdauth <dev>  ur�en� DVD za��zen� pro authentifikaci (pro k�dovan� disky)\n"
#endif
#ifdef USE_DVDREAD
" -dvd <titleno>  p�ehr�t DVD titul/stopu ze za��zen� (mechaniky) m�sto souboru\n"
#endif
" -ss <timepos>   posun na pozici (sekundy nebo hh:mm:ss)\n"
" -nosound        p�ehr�vat beze zvuku\n"
#ifdef USE_FAKE_MONO
" -stereo <mode>  v�b�r audiokan�lu pro MPEG1 (0:stereo 1:lev� 2:prav�)\n"
#endif
" -fs -vm -zoom   volby pro p�ehr�v�n� p�es celou obrazovku (cel� obrazovka\n                 m�nit videore�im, softwarov� zoom)\n"
" -x <x> -y <y>   zv�t�en� obrazu na rozm�r <x>*<y>[pokud je podpora -vo driver!]\n"
" -sub <file>     volba souboru titulku (viz tak� -subfps, -subdelay)\n"
" -vid x -aid y   v�b�r ��sla video (x) a audio (y) proudu pro p�ehr�n�\n"
" -fps x -srate y volba pro zm�nu video (x fps) a audio (y Hz) frekvence\n"
" -pp <quality>   aktivace postprocesing filtru (0-4 pro DivX, 0-63 pro mpegy)\n"
" -nobps          pou��t alternativn� A-V synchroniza�n� metodu pro Avi soubory\n"
" -framedrop      povolit zahazov�n� sn�mk� (pro pomale stroje)\n"
"\n"
"Kl�vesy:\n"
" <-  nebo  ->    posun vzad/vp�ed o 10 sekund\n"
" up nebo down    posun vzad/vp�ed o  1 minutu\n"
" p nebo SPACE    pauza p�i p�ehr�v�n� (pokra�ov�n� stiskem kter�koliv kl�vesy)\n"
" q nebo ESC      konec p�ehr�v�n� a ukon�en� programu\n"
" + nebo -        upravit zpo�d�n� zvuku v kroc�ch +/- 0.1 sekundy\n"
" o               cyklick� zm�na re�imu OSD:  nic / pozice / pozice+�as\n"
" * nebo /        p�idat nebo ubrat hlasitost (stiskem 'm' v�b�r master/pcm)\n"
" z nebo x        upravit zpo�d�n� titulk� v kroc�ch +/- 0.1 sekundy\n"
"\n"
" * * * * P�E�T�TE SI STR�NY MAN PRO DETAILY (DAL�� VOLBY A KL�VESY)! * * * * \n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// mplayer.c: 

#define MSGTR_Exiting "\nKon��m... (%s)\n"
#define MSGTR_Exit_frames "Po�adovan� po�et sn�mk� p�ehr�n"
#define MSGTR_Exit_quit "Konec"
#define MSGTR_Exit_eof "Konec souboru"
#define MSGTR_Exit_error "Z�va�n� chyba"
#define MSGTR_IntBySignal "\nMPlayer p�eru�en sign�lem %d v modulu: %s \n"
#define MSGTR_NoHomeDir "Nemohu nal�zt dom�c� (HOME) adres��\n"
#define MSGTR_GetpathProblem "get_path(\"config\") probl�m\n"
#define MSGTR_CreatingCfgFile "Vytv���m konfigura�n� soubor: %s\n"
#define MSGTR_InvalidVOdriver "Neplan� jm�no v�stupn�ho videoovlada�e: %s\nPou�ijte '-vo help' pro seznam dostupn�ch ovlada��.\n"
#define MSGTR_InvalidAOdriver "Neplan� jm�no v�stupn�ho audioovlada�e: %s\nPou�ijte '-ao help' pro seznam dostupn�ch ovlada��.\n"
#define MSGTR_CopyCodecsConf "(copy/ln etc/codecs.conf (ze zdrojov�ch k�d� MPlayeru) do ~/.mplayer/codecs.conf)\n"
#define MSGTR_CantLoadFont "Nemohu na��st font: %s\n"
#define MSGTR_CantLoadSub "Nemohu na��st titulky: %s\n"
#define MSGTR_ErrorDVDkey "Chyba p�i zpracov�n� kl��e DVD.\n"
#define MSGTR_CmdlineDVDkey "DVD kl�� po�adovan� na p��kazov� ��dce je uschov�n pro descrambling.\n"
#define MSGTR_DVDauthOk "DVD authentifika�n� sekvence vypad� vpo��dku.\n"
#define MSGTR_DumpSelectedSteramMissing "dump: FATAL: po�adovan� proud chyb�!\n"
#define MSGTR_CantOpenDumpfile "Nelze otev��t soubor pro dump!!!\n"
#define MSGTR_CoreDumped "core dumped :)\n"
#define MSGTR_FPSnotspecified "FPS neud�no (nebo neplatn�) v hlavi�ce souboru! Pou�ijte volbu -fps !\n"
#define MSGTR_NoVideoStream "Bohu�el, ��dn� videoproud... to se zat�m ned� p�ehr�t.\n"
#define MSGTR_TryForceAudioFmt "Poku��m se vynutit rodinu audiokodeku %d ...\n"
#define MSGTR_CantFindAfmtFallback "Nemohu nal�zt audio kodek pro po�adovanou rodinu, pou�iji ostatn�.\n"
#define MSGTR_CantFindAudioCodec "Nemohu nal�zt kodek pro audio form�t 0x%X !\n"
#define MSGTR_TryUpgradeCodecsConfOrRTFM "*** Pokuste se upgradovat %s z etc/codecs.conf\n*** Pokud probl�m p�etrv�, pak si p�e�t�te DOCS/CODECS!\n"
#define MSGTR_CouldntInitAudioCodec "Nelze inicializovat audio kodek! -> beze zvuku\n"
#define MSGTR_TryForceVideoFmt "Poku��m se vynutit rodinu videokodeku %d ...\n"
#define MSGTR_CantFindVfmtFallback "Nemohu nal�zt video kodek pro po�adovanou rodinu, pou�iji ostatn�.\n"
#define MSGTR_CantFindVideoCodec "Nemohu nal�zt kodek pro video form�t 0x%X !\n"
#define MSGTR_VOincompCodec "Bohu�el, vybran� video_out za��zen� je nekompatibiln� s t�mto kodekem.\n"
#define MSGTR_CouldntInitVideoCodec "FATAL: Nemohu inicializovat videokodek :(\n"
#define MSGTR_EncodeFileExists "Soubor ji� existuje: %s (nep�epi�te si sv�j obl�ben� AVI soubor!)\n"
#define MSGTR_CantCreateEncodeFile "Nemohu vytvo�it soubor pro encoding\n" // tohle ur�it� opravit
#define MSGTR_CannotInitVO "FATAL: Nemohu inicializovat video driver!\n"
#define MSGTR_CannotInitAO "nemohu otev��t/inicializovat audio driver -> TICHO\n"
#define MSGTR_StartPlaying "Za��n�m p�ehr�vat...\n"
#define MSGTR_SystemTooSlow "\n************************************************************************"\
			    "\n*** V� syst�m je p��li� pomal�! M��ete zkusit -framedrop nebo RTFM! ***"\
			    "\n************************************************************************\n"

#define MSGTR_NoGui "MPlayer byl p�elo�en BEZ podpory GUI!\n"
#define MSGTR_GuiNeedsX "MPlayer GUI vy�aduje X11!\n"
#define MSGTR_Playing "P�ehr�v�m %s\n"
#define MSGTR_NoSound "Audio: beze zvuku!!!\n"
#define MSGTR_FPSforced "FPS vynuceno na hodnotu %5.3f  (ftime: %5.3f)\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "CD-ROM za��zen� '%s' nenalezeno!\n"
#define MSGTR_ErrTrackSelect "Chyba p�i v�b�ru VCD stopy!"
#define MSGTR_ReadSTDIN "�tu z stdin...\n"
#define MSGTR_UnableOpenURL "Nelze otev��t URL: %s\n"
#define MSGTR_ConnToServer "P�ipojen k serveru: %s\n"
#define MSGTR_FileNotFound "Soubor nenalezen: '%s'\n"

#define MSGTR_CantOpenDVD "Nelze otev��t DVD za��zen�: %s\n"
#define MSGTR_DVDwait "�tu strukturu disku, pros�m �ekejte...\n"
#define MSGTR_DVDnumTitles "Na tomto DVD je %d titul�.\n"
#define MSGTR_DVDinvalidTitle "Neplatn� ��slo DVD titulu: %d\n"
#define MSGTR_DVDnumChapters "Na tomto DVD je %d kapitol.\n"
#define MSGTR_DVDinvalidChapter "Neplatn� ��slo kapitoly DVD: %d\n"
#define MSGTR_DVDnumAngles "Na tomto DVD je %d �hl� pohledu.\n"
#define MSGTR_DVDinvalidAngle "Neplatn� ��slo �hly pohledu DVD: %d\n"
#define MSGTR_DVDnoIFO "Nemohu otev��t soubor IFO pro DVD titul %d.\n"
#define MSGTR_DVDnoVOBs "Nemohu otev��t VOB soubor (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD �sp�n� otev�eno!\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "Upozorn�n�! Hlavi�ka audio proudu %d p�edefinov�na!\n"
#define MSGTR_VideoStreamRedefined "Upozorn�n�! Hlavi�ka video proudu %d p�edefinov�na!\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: P��li� mnoho (%d v %d bytes) audio paket� v bufferu!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: P��li� mnoho (%d v %d bytes) video paket� v bufferu!\n"
#define MSGTR_MaybeNI "(mo�n� p�ehr�v�te neprokl�dan� proud/soubor nebo kodek selhal)\n"
#define MSGTR_DetectedAVIfile "Detekov�n AVI form�t souboru!\n"
#define MSGTR_DetectedASFfile "Detekov�n ASF form�t souboru!\n"
#define MSGTR_DetectedMPEGPESfile "Detekov�n MPEG-PES form�t souboru!\n"
#define MSGTR_DetectedMPEGPSfile "Detekov�n MPEG-PS form�t souboru!\n"
#define MSGTR_DetectedMPEGESfile "Detekov�n MPEG-ES form�t souboru!\n"
#define MSGTR_DetectedQTMOVfile "Detekov�n QuickTime/MOV form�t souboru!\n"
#define MSGTR_MissingMpegVideo "Chyb�j�c� MPEG video proud!? kontaktujte autora, mo�n� to je chyba (bug) :(\n"
#define MSGTR_InvalidMPEGES "Neplatn� MPEG-ES proud??? kontaktuje autora, mo�n� to je chyba (bug) :(\n"
#define MSGTR_FormatNotRecognized "========== Bohu�el, tento form�t souboru nen� rozpozn�n/podporov�n =========\n"\
				  "==== Pokud je tento sounor AVI, ASF nebo MPEG proud, kontaktuje autora! ====\n"
#define MSGTR_MissingVideoStream "��dn� video proud nenalezen!\n"
#define MSGTR_MissingAudioStream "��dn� audio proud nenalezen...  ->beze zvuku\n"
#define MSGTR_MissingVideoStreamBug "Chyb�j�c� video proud!? Kontaktuje autora, mo�n� to je chyba (bug) :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: soubor neobsahuje vybran� audio nebo video proud\n"

#define MSGTR_NI_Forced "Vynucen"
#define MSGTR_NI_Detected "Detekov�n"
#define MSGTR_NI_Message "%s NEPROKL�DAN� form�t souboru AVI!\n"

#define MSGTR_UsingNINI "Pou��v�m NEPROKL�DAN� rozbit� form�t souboru AVI!\n" //tohle taky n�jak opravit
#define MSGTR_CouldntDetFNo "Nemohu ur�it po�et sn�mk� (pro absolutn� posun)  \n"
#define MSGTR_CantSeekRawAVI "Nelze posouvat v raw .AVI proudech! (index vy�adov�n, zkuste pou��t volbu -idx !)  \n"
#define MSGTR_CantSeekFile "Nemohu posouvat v tomto souboru!  \n"

#define MSGTR_EncryptedVOB "K�dovan� VOB soubor (p�elo�eno bez podpory libcss)! P�e�t�te si DOCS/DVD\n"
#define MSGTR_EncryptedVOBauth "Zak�dovan� proud, ale authentifikace nebyla V�mi po�adov�na!!\n"

#define MSGTR_MOVcomprhdr "MOV: Komprimovan� hlavi�ky nejsou (je�t�) podporov�ny!\n"
#define MSGTR_MOVvariableFourCC "MOV: Upozorn�n�! prom�nn� FOURCC detekov�na!?\n"
#define MSGTR_MOVtooManyTrk "MOV: Upozorn�n�! P��li� mnoho stop!"
#define MSGTR_MOVnotyetsupp "\n****** Quicktime MOV form�t nen� je�t� podporov�n !!! *******\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "nemohu otev��t kodek\n"
#define MSGTR_CantCloseCodec "nemohu uzav��t kodek\n"

#define MSGTR_MissingDLLcodec "CHYBA: Nemohu otev��t po�adovan� DirectShow kodek: %s\n"
#define MSGTR_ACMiniterror "Nemohu na��st/inicializovat Win32/ACM AUDIO kodek (chyb�j�c� soubor DLL?)\n"
#define MSGTR_MissingLAVCcodec "Nemohu naj�t kodek '%s' v libavcodec...\n"

#define MSGTR_NoDShowSupport "MPlayer byl p�elo�en BEZ podpory directshow!\n"
#define MSGTR_NoWfvSupport "Podpora pro kodeky win32 neaktivn� nebo nedostupn� mimo platformy x86!\n"
#define MSGTR_NoDivx4Support "MPlayer byl p�elo�en BEZ podpory DivX4Linux (libdivxdecore.so)!\n"
#define MSGTR_NoLAVCsupport "MPlayer byl p�elo�en BEZ podpory ffmpeg/libavcodec!\n"
#define MSGTR_NoACMSupport "Win32/ACM audio kodek neaktivn� nebo nedostupn� mimo platformy x86 -> vynuceno beze zvuku :(\n"
#define MSGTR_NoDShowAudio "P�elo�eno BEZ podpory DirectShow -> vynuceno beze zvuku :(\n"
#define MSGTR_NoOggVorbis "OggVorbis audio kodek neaktivn� -> vynuceno beze zvuku :(\n"

#define MSGTR_MpegPPhint "Upozorn�n�! Po�adujete video postprocesing pro MPEG 1/2, ale MPlayer byl\n" \
			 "         p�elo�en bez podpory posprocesingu MPEG 1/2!\n" \
			 "         #define MPEG12_POSTPROC v config.h a p�elo�te znovu libmpeg2!\n"
#define MSGTR_MpegNoSequHdr "MPEG: FATAL: EOF - konec souboru v pr�b�hu vyhled�v�n� hlavi�ky sekvence\n"
#define MSGTR_CannotReadMpegSequHdr "FATAL: Nelze p�e��st hlavi�ku sekvence!\n"
#define MSGTR_CannotReadMpegSequHdrEx "FATAL: Nelze p�e��st roz���en� hlavi�ky sekvence!\n"
#define MSGTR_BadMpegSequHdr "MPEG: �patn� hlavi�ka sekvence!\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: �patn� roz���en� hlavi�ky sekvence!\n"

#define MSGTR_ShMemAllocFail "Nemohu alokovat sd�lenou pam�\n"
#define MSGTR_CantAllocAudioBuf "Nemohu alokovat pam� pro v�stupn� audio buffer\n"
#define MSGTR_NoMemForDecodedImage "nedostatek pam�ti pro buffer pro dek�dov�n� obrazu (%ld bytes)\n"

#define MSGTR_AC3notvalid "Neplatn� AC3 proud.\n"
#define MSGTR_AC3only48k "Pouze proudy o frekvenci 48000 Hz podporov�ny.\n"
#define MSGTR_UnknownAudio "Nezn�m�/chyb�j�c� audio form�t -> beze zvuku\n"

// LIRC:
#define MSGTR_SettingUpLIRC "Nastavuji podporu lirc ...\n"
#define MSGTR_LIRCdisabled "Nebude moci pou��vat d�lkov� ovlada�.\n"
#define MSGTR_LIRCopenfailed "Selhal pokus o otev�en� podpory LIRC!\n"
#define MSGTR_LIRCsocketerr "N�jak� chyba se soketem lirc: %s\n"
#define MSGTR_LIRCcfgerr "Selhalo �teni konfigura�n�ho souboru LIRC %s !\n"


// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "O aplikaci"
#define MSGTR_FileSelect "V�b�r souboru ..."
#define MSGTR_MessageBox "MessageBox"
#define MSGTR_PlayList "PlayList"
#define MSGTR_SkinBrowser "Prohl�e� skin�"

// --- buttons ---
#define MSGTR_Ok "Ok"
#define MSGTR_Cancel "Zru�it"
#define MSGTR_Add "P�idat"
#define MSGTR_Remove "Odebrat"

// --- error messages ---
#define MSGTR_NEMDB "Bohu�el, nedostatek pam�ti pro buffer pro kreslen�."
#define MSGTR_NEMFMR "Bohu�el, nedostatek pam�ti pro vytv��en� menu."
#define MSGTR_NEMFMM "Bohu�el, nedostatek pam�ti pro masku hlavn�ho okna."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[skin] chyba v konfigura�n�m soubory skin� %d: %s" 
#define MSGTR_SKIN_WARNING1 "[skin] v konfigura�n�m soubory skin� na ��dce %d: widget nalezen ale p�ed  \"section\" nenalezen ( %s )"
#define MSGTR_SKIN_WARNING2 "[skin] v konfigura�n�m soubory skin� na ��dce %d: widget nalezen ale p�ed \"subsection\" nenalezen (%s)"
#define MSGTR_SKIN_BITMAP_16bit  "bitmapa s hloubkou 16 bitov� a m�n� nepodporov�na ( %s ).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "soubor nenalezen ( %s )\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "chyba �ten� bmp ( %s )\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "chyba �ten� tga ( %s )\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "chyba �ten� png ( %s )\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "form�t RLE packed tga nepodporov�n ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "nezn�m� typ souboru ( %s )\n"
#define MSGTR_SKIN_BITMAP_ConvertError "chyba konverze z 24 bit do 32 bit ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "nezn�m� zpr�va: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "nedostatek pam�ti\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "deklarov�no p��li� mnoho font�\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "soubor fontu nenalezen\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "soubor obraz� fontu nenalezen\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "neexistuj�c� identifik�tor fontu ( %s )\n"
#define MSGTR_SKIN_UnknownParameter "nezn�m� parametr ( %s )\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[skinbrowser] nedostatek pam�ti.\n"

#endif


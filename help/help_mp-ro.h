// Translated by:  Codre Adrian <codreadrian@softhome.net> (address bounces)

// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"Folosire:   mplayer [op�iuni] [cale/]fi�ier\n"
"\n"
"Op�iuni:\n"
" -vo <drv[:disp]> Ie�irea video: driver&dispozitiv ('-vo help' pentru o list�)\n"
" -ao <drv[:disp]> Ie�irea audio: driver&dispozitiv ('-ao help' pentru o list�)\n"
" vcd://<num�r pist�>  folose�te <pista> de pe dispozitivul VCD �n loc de fi�ier\n"
#ifdef HAVE_LIBCSS
" -dvdauth <disp>  dispozitivul DVD pentru autentificare (la discuri encriptate)\n"
#endif
#ifdef USE_DVDREAD
" dvd://<titlu>  folose�te titlu/pista de pe dispozitivul DVD �n loc de fi�ier\n"
#endif
" -ss <pozi�ia>   sare la pozi�ia (secunde sau oo:mm:ss)\n"
" -nosound        f�r� sunet\n"
#ifdef USE_FAKE_MONO
" -stereo <mod>   modul stereo la MPEG (0:stereo 1:canalul st�ng 2:canalul drept)\n"
#endif
" -fs -vm -zoom   mod tot ecranul (tot ecranul,schimb� modul,scalat prin software)\n"
" -x <x> -y <y>   scaleaz� imaginea la <x> * <y> [dac� driver-ul -vo suport�!]\n"
" -sub <fi�ier>   specific� fi�ierul cu subtitr�ri (vezi �i -subfps, -subdelay)\n"
" -vid x -aid y   op�iuni pentru selectarea pistei video (x) sau audio (y)\n"
" -fps x -srate y op�iuni pentru schimbarea ratei video (x fps) sau audio (y Hz)\n"
" -pp <calitate>  activeaz� filtrul de postprocesare (0-4 la DivX, 0-63 la MPEG)\n"
" -nobps          folose�te metoda alternativ� de sicronizare A-V (poate ajuta!)\n"
" -framedrop      activeaz� s�ritul cadrelor (pentru calculatoare lente)\n"
"\n"
"Taste:\n"
" <-  sau  ->      caut� fa��/spate cu 10 secunde\n"
" sus sau jos      caut� fa��/spate cu 1 minut\n"
" p sau SPACE      pune filmul pe pauz� (orice tast� pentru a continua)\n"
" q sau ESC        opre�te filmul �i iese din program\n"
" + sau -          ajusteaz� decalajul audio cu +/- 0.1 secunde\n"
" o                rote�te modurile OSD: nimic / bar� progres / bar� progres+ceas\n"
" * sau /          cre�te sau scade volumul (ap�sa�i 'm' pentru principal/wav)\n"
" z sau x          ajusteaz� decalajul subtitr�rii cu +/- 0.1 secunde\n"
"\n"
" * * * VEDE�I MANUALUL PENTRU DETALII,(ALTE) OP�IUNI AVANSATE �I TASTE ! * * *\n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// mplayer.c: 

#define MSGTR_Exiting "\nIes... (%s)\n"
#define MSGTR_Exit_quit "Ie�ire"
#define MSGTR_Exit_eof "Sf�r�itul fi�ierului"
#define MSGTR_Exit_error "Eroare fatal�"
#define MSGTR_IntBySignal "\nMPlayer a fost intrerupt de semnalul %d �n modulul: %s \n"
#define MSGTR_NoHomeDir "Nu g�sesc directorul HOME\n"
#define MSGTR_GetpathProblem "get_path(\"config\") cu probleme\n"
#define MSGTR_CreatingCfgFile "Creez fi�ierul de configurare: %s\n"
#define MSGTR_InvalidVOdriver "Ie�ire video invalid�: %s\nFolosi�i '-vo help' pentru o list� de ie�iri video disponibile.\n"
#define MSGTR_InvalidAOdriver "Ie�ire audio invalid�: %s\nFolosi�i '-ao help' pentru o list� de ie�iri audio disponibile.\n"
#define MSGTR_CopyCodecsConf "(copia�i etc/codecs.conf (din directorul surs� MPlayer) �n  ~/.mplayer/codecs.conf)\n"
#define MSGTR_CantLoadFont "Nu pot inc�rca fontul: %s\n"
#define MSGTR_CantLoadSub "Nu pot incarc� subtitrarea: %s\n"
#define MSGTR_ErrorDVDkey "Eroare la procesarea cheii DVD.\n"
#define MSGTR_CmdlineDVDkey "Cheia DVD specificat� �n linia de comand� este p�strat� pentru decodificare.\n"
#define MSGTR_DVDauthOk "Secven�a de autentificare DVD pare s� fie OK.\n"
#define MSGTR_DumpSelectedStreamMissing "dump: FATALA: pista selectat� lipse�te!\n"
#define MSGTR_CantOpenDumpfile "Nu pot deschide fi�ierul (dump)!!!\n"
#define MSGTR_CoreDumped "core aruncat :)\n"
#define MSGTR_FPSnotspecified "FPS nespecificat (sau invalid) �n antet! Folosi�i op�iunea -fps!\n"
#define MSGTR_TryForceAudioFmt "�ncerc s� for�ez utilizarea unui codec audio din familia %d ...\n"
#define MSGTR_CantFindAfmtFallback "Nu pot s� g�sesc un codec audio pentru familia for�at�, revin la alte drivere.\n"
#define MSGTR_CantFindAudioCodec "Nu g�sesc un codec audio pentru formatul 0x%X !\n"
#define MSGTR_TryUpgradeCodecsConfOrRTFM "*** �ncerca�i s� �nnoi�i %s din etc/codecs.conf\n*** Dac� nu ajut� citi�i DOCS/CODECS!\n"
#define MSGTR_CouldntInitAudioCodec "Nu pot s� ini�ializez codec-ul audio! -> f�r� sunet\n"
#define MSGTR_TryForceVideoFmt "�ncerc s� for�ez utilizarea unui codec video din familia %d ...\n"
#define MSGTR_CantFindVideoCodec "Nu g�sesc un codec video pentru formatul 0x%X !\n"
#define MSGTR_VOincompCodec "�mi pare r�u, ie�irea video selectat� este incompatibil� cu acest codec.\n"
#define MSGTR_CouldntInitVideoCodec "FATAL�: Nu pot ini�ializa codec-ul video :(\n"
#define MSGTR_CannotInitVO "FATAL�: Nu pot ini�ializa diver-ul video!\n"
#define MSGTR_CannotInitAO "nu pot deschide/ini�ializa dispozitivul audio -> f�r� sunet\n"
#define MSGTR_StartPlaying "�ncep afi�area...\n"
#define MSGTR_SystemTooSlow "\n*******************************************************************************"\
			    "\n** Sistemul dumneavoastr� este prea LENT ! �ncerca�i cu -framedrop sau RTFM! **"\
			    "\n*******************************************************************************\n"

#define MSGTR_NoGui "MPlayer a fost compilat f�r� interfa�� grafic�!\n"
#define MSGTR_GuiNeedsX "Interfa�a grafica necesit� X11!\n"
#define MSGTR_Playing "Afi�ez %s\n"
#define MSGTR_NoSound "Audio: f�r� sunet!!!\n"
#define MSGTR_FPSforced "FPS for�at la %5.3f  (ftime: %5.3f)\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "Dispozitivul CD-ROM '%s' nu a fost g�sit!\n"
#define MSGTR_ErrTrackSelect "Eroare la selectarea pistei VCD!"
#define MSGTR_ReadSTDIN "Citesc de la intrarea standard...\n"
#define MSGTR_UnableOpenURL "Nu pot accesa adresa: %s\n"
#define MSGTR_ConnToServer "Conectat la serverul: %s\n"
#define MSGTR_FileNotFound "Fi�ier neg�sit: '%s'\n"

#define MSGTR_CantOpenDVD "Nu am putut deschide dispozitivul DVD: %s\n"
#define MSGTR_DVDwait "Citesc structura discului, v� rog a�tepta�i...\n"
#define MSGTR_DVDnumTitles "Pe acest DVD sunt %d titluri.\n"
#define MSGTR_DVDinvalidTitle "Num�r titlu DVD invalid: %d\n"
#define MSGTR_DVDnumChapters "�n acest titlu DVD sunt %d capitole.\n"
#define MSGTR_DVDinvalidChapter "Num�r capitol DVD invalid: %d\n"
#define MSGTR_DVDnumAngles "Sunt %d unghiuri �n acest titlu DVD.\n"
#define MSGTR_DVDinvalidAngle "Num�r unghi DVD invalid: %d\n"
#define MSGTR_DVDnoIFO "Nu pot deschide fi�ierul IFO pentru titlul DVD %d.\n"
#define MSGTR_DVDnoVOBs "Nu pot deschide fi�ierul titlu (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD deschis cu succes!\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "Aten�ie! Antet pist� audio %d redefinit!\n"
#define MSGTR_VideoStreamRedefined "Aten�ie! Antet pist� video %d redefinit!\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: Prea multe (%d �n %d bytes) pachete audio �n tampon!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: Prea multe (%d �n %d bytes) pachete video �n tampon!\n"
#define MSGTR_MaybeNI "(poate afi�a�i un film/pist� ne-�ntre�esut sau codec-ul a dat eroare)\n"
#define MSGTR_Detected_XXX_FileFormat "Format fi�ier detectat: %s!\n"
#define MSGTR_InvalidMPEGES "Pist� MPEG-ES invalid�??? contacta�i autorul, poate fi un bug :(\n"
#define MSGTR_FormatNotRecognized "============= �mi pare r�u, acest format de fi�ier nu este recunoscut/suportat ===============\n"\
				  "======== Dac� acest fi�ier este o pist� AVI, ASF sau MPEG , contacta�i v� rog autorul! ========\n"
#define MSGTR_MissingVideoStream "Nu am g�sit piste video!\n"
#define MSGTR_MissingAudioStream "Nu am g�sit piste audio...  -> f�r� sunet\n"
#define MSGTR_MissingVideoStreamBug "Lipse�te pista video!? Contacta�i autorul, poate fi un bug :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: fi�ierul nu con�ine pista audio sau video specificat�\n"

#define MSGTR_NI_Forced "For�at"
#define MSGTR_NI_Detected "Detectat"
#define MSGTR_NI_Message "%s fi�ier AVI NE-�NTRE�ESUT!\n"

#define MSGTR_UsingNINI "Folosesc fi�ier AVI NE-�NTRE�ESUT eronat!\n"
#define MSGTR_CouldntDetFNo "Nu pot determina num�rul de cadre (pentru c�utare absolut�)\n"
#define MSGTR_CantSeekRawAVI "Nu pot c�uta �n fi�iere .AVI neindexate! (am nevoie de index, �ncerca�i cu -idx!)  \n"
#define MSGTR_CantSeekFile "Nu pot c�uta �n fi�ier!  \n"

#define MSGTR_EncryptedVOB "Fi�ier VOB encriptat (necompilat cu suport libcss)! Citi�i fi�ierul DOCS/DVD\n"
#define MSGTR_EncryptedVOBauth "Fi�ier encriptat dar autentificarea nu a fost cerut� de dumneavoastr�.!!\n"

#define MSGTR_MOVcomprhdr "MOV: Antetele compresate nu sunt (�nc�) suportate!\n"
#define MSGTR_MOVvariableFourCC "MOV: Aten�ie! variabil� FOURCC detectat�!?\n"
#define MSGTR_MOVtooManyTrk "MOV: Aten�ie! prea multe piste!"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "nu pot deschide codec-ul audio\n"
#define MSGTR_CantCloseCodec "nu pot deschide codec-ul video\n"

#define MSGTR_MissingDLLcodec "EROARE: Nu pot deschide codec-ul DirectShow necesar: %s\n"
#define MSGTR_ACMiniterror "Nu pot �nc�rca/ini�ializa codec-ul audio Win32/ACM (lipse�te fi�ierul DLL?)\n"
#define MSGTR_MissingLAVCcodec "Nu g�sesc codec-ul '%s' �n libavcodec...\n"

#define MSGTR_MpegNoSequHdr "MPEG: FATAL�: EOF �n timpul c�ut�rii antetului secven�ei\n"
#define MSGTR_CannotReadMpegSequHdr "FATAL�: Nu pot citi antetul secven�ei!\n"
#define MSGTR_CannotReadMpegSequHdrEx "FATAL�: Nu pot citi extensia antetului secven�ei!\n"
#define MSGTR_BadMpegSequHdr "MPEG: Antet secven�� eronat!\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: Extensie antet secven�� eronat�!\n"

#define MSGTR_ShMemAllocFail "Nu pot aloca memoria partajat�\n"
#define MSGTR_CantAllocAudioBuf "Nu pot aloca tamponul pentru ie�irea audio\n"

#define MSGTR_UnknownAudio "Format audio necunoscut/lips�, folosesc f�r� sunet\n"

// LIRC:
#define MSGTR_SettingUpLIRC "Setez suportul pentru LIRC...\n"
#define MSGTR_LIRCdisabled "Nu ve�i putea utiliza telecomanda\n"
#define MSGTR_LIRCopenfailed "Nu pot deschide suportul pentru LIRC!\n"
#define MSGTR_LIRCcfgerr "Nu pot citi fi�ierul de configurare LIRC %s !\n"


// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "Despre..."
#define MSGTR_FileSelect "Selectare fi�ier..."
#define MSGTR_PlayList "List� de redare..."
#define MSGTR_SkinBrowser "Navigator tematici..."

// --- buttons ---
#define MSGTR_Ok "Ok"
#define MSGTR_Cancel "Anulare"
#define MSGTR_Add "Adaug�"
#define MSGTR_Remove "Scoate"

// --- error messages ---
#define MSGTR_NEMDB "�mi pare r�u, memorie insuficient� pentru tamponul de desenare."
#define MSGTR_NEMFMR "�mi pare r�u, memorie insuficient� pentru desenarea meniului."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[tematic�] eroare �n fi�ierul de tematic� la linia %d: %s" 
#define MSGTR_SKIN_WARNING1 "[tematic�] eroare �n fi�ierul de tematic� la linia %d: component� gasit� dar �nainte \"section\" neg�sit� ( %s )"
#define MSGTR_SKIN_WARNING2 "[tematic�] eroare �n fi�ierul de tematic� la linia %d: component� gasit� dar �nainte \"subsection\" neg�sit� (%s)"
#define MSGTR_SKIN_BITMAP_16bit  "ad�ncimea de culoare de 16 bi�i sau mai pu�in pentru imagini nesuportat� ( %s ).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "fi�ier neg�sit ( %s )\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "eroare la citire bmp ( %s )\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "eroare la citire tga ( %s )\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "eroare la citire png ( %s )\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "imagini tga �mpachetate RLE nesuportate ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "tip fi�ier necunoscut ( %s )\n"
#define MSGTR_SKIN_BITMAP_ConvertError "eroare la conversia de la 24 bi�i la 32 bi�i ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "mesaj necunoscut: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "memorie insuficient�\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "prea multe font-uri declarate\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "fi�ier cu font neg�sit\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "fi�ier imagine font neg�sit\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "identificator font inexistent ( %s )\n"
#define MSGTR_SKIN_UnknownParameter "parametru necunoscut ( %s )\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[Navigator tematici] memorie insuficient�.\n"

#endif

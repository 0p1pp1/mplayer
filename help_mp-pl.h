// Translated by:  Bohdan Horst <nexus@hoth.amu.edu.pl>

// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char* banner_text=
"\n\n"
"MPlayer " VERSION "(C) 2000-2002 Arpad Gereoffy (see DOCS!)\n"
"\n";

static char help_text[]=
#ifdef HAVE_NEW_GUI
"U�ycie:   mplayer [-gui] [opcje] [�cie�ka/]nazwa\n"
#else
"U�ycie:   mplayer [opcje] [�cie�ka/]nazwa\n"
#endif
"\n"
"Opcje:\n"
" -vo <drv[:dev]> wyb�r sterownika[:urz�dzenia] video (lista po '-vo help')\n"
" -ao <drv[:dev]> wyb�r sterownika[:urz�dzenia] audio (lista po '-ao help')\n"
" -vcd <trackno>  odtwarzanie bezpo�rednio �cie�ki VCD (video cd)\n"
#ifdef HAVE_LIBCSS
" -dvdauth <dev>  urz�dzenie DVD do autentykacji (dla zaszyfrowanych dysk�w)\n"
#endif
#ifdef USE_DVDREAD
" -dvd <titleno>  odtwarzanie bezpo�rednio �cie�ki DVD\n"
#endif
" -ss <timepos>   skok do podanej pozycji (sekundy albo hh:mm:ss)\n"
" -nosound        odtwarzanie bez d�wi�ku\n"
#ifdef USE_FAKE_MONO
" -stereo <mode>  wyb�r trybu stereo dla MPEG1 (0:stereo 1:lewo 2:prawo)\n"
#endif
" -channels <n>   docelowa liczba kana��w audio\n"
" -fs -vm -zoom   opcje pe�noekranowe (pe�en ekran,zmiana trybu,skalowanie)\n"
" -x <x> -y <y>   skalowanie do rozdzielczo�ci <x>*<y> [je�li -vo pozwala!]\n"
" -sub <file>     wyb�r pliku z napisami (zobacz tak�e -subfps, -subdelay)\n"
" -playlist <file>wyb�r pliku z playlist�\n"
" -vid x -aid y   wyb�r odtwarzanego strumienia video (x) i audio (y)\n"
" -fps x -srate y wyb�r pr�dko�ci odtwarzania video (x fps) i audio (y Hz)\n"
" -pp <quality>   wyb�r filtra wyg�adzaj�cego (0-4 dla DivX, 0-63 dla mpeg)\n"
" -nobps          inna metoda synchronizacji A-V dla plik�w AVI (mo�e pom�c!)\n"
" -framedrop      gubienie klatek (dla wolnych maszyn)\n"
" -wid <window id>u�yj istniej�cgo okna dla wyj�cia video (np. dla pluggera!)\n"
"\n"
"Klawisze:\n"
" Right,Up,PgUp   skok naprz�d o 10 sekund, 1 minut�, 10 minut\n"
" Left,Down,PgDn  skok do ty�u o 10 sekund, 1 minut�, 10 minut\n"
" < lub >         przeskok o jedn� pozycj� w playli�cie\n"
" p lub SPACE     zatrzymanie filmu (kontynuacja - dowolny klawisz)\n"
" q lub ESC       zatrzymanie odtwarzania i wyj�cie z programu\n"
" + lub -         regulacja op�nienia d�wi�ku o +/- 0.1 sekundy\n"
" o               prze��czanie tryb�w OSD: pusty / belka / belka i zegar\n"
" * lub /         zwi�kszenie lub zmniejszenie nat�enia d�wi�ku\n"
"                 (naci�nij 'm' �eby wybra� master/pcm)\n"
" z lub x         regulacja op�nienia napis�w o +/- 0.1 sekundy\n"
"\n"
" **** DOK�ADNY SPIS WSZYSTKICH DOST�PNYCH OPCJI ZNAJDUJE SI� W MANUALU! ****\n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// mplayer.c: 

#define MSGTR_Exiting "\nWychodz�... (%s)\n"
#define MSGTR_Exit_frames "Zadana liczba klatek odtworzona"
#define MSGTR_Exit_quit "Wyj�cie"
#define MSGTR_Exit_eof "Koniec pliku"
#define MSGTR_Exit_error "B��d krytyczny"
#define MSGTR_IntBySignal "\nMPlayer przerwany sygna�em %d w module: %s \n"
#define MSGTR_NoHomeDir "Nie mog� znale�� katalogu HOME\n"
#define MSGTR_GetpathProblem "problem z get_path(\"config\")\n"
#define MSGTR_CreatingCfgFile "Stwarzam plik z konfiguracj�: %s\n"
#define MSGTR_InvalidVOdriver "Nieprawid�owa nazwa sterownika video: %s\nU�yj '-vo help' aby dosta� list� dost�pnych streownik�w video.\n"
#define MSGTR_InvalidAOdriver "Nieprawid�owa nazwa sterownika audio: %s\nU�yj '-ao help' aby dosta� list� dost�pnych sterownik�w audio.\n"
#define MSGTR_CopyCodecsConf "(skopiuj/zlinkuj etc/codecs.conf do ~/.mplayer/codecs.conf)\n"
#define MSGTR_CantLoadFont "Nie mog� za�adowa� fontu: %s\n"
#define MSGTR_CantLoadSub "Nie mog� za�adowa� napis�w: %s\n"
#define MSGTR_ErrorDVDkey "B��d w przetwarzaniu DVD KEY.\n"
#define MSGTR_CmdlineDVDkey "Linia komend DVD wymaga zapisanego klucza do descramblingu.\n"
#define MSGTR_DVDauthOk "Sekwencja autoryzacji DVD wygl�da OK.\n"
#define MSGTR_DumpSelectedSteramMissing "dump: FATAL: nie ma wybranego strumienia!\n"
#define MSGTR_CantOpenDumpfile "Nie mog� otworzy� pliku dump!!!\n"
#define MSGTR_CoreDumped "core dumped :)\n"
#define MSGTR_FPSnotspecified "FPS nie podane (lub b��dne) w nag��wku! U�yj opcji -fps!\n"
#define MSGTR_NoVideoStream "Przepraszam, brak strumienia video... nie dzia�a to na razie\n"
#define MSGTR_TryForceAudioFmt "Wymuszam zastosowanie kodeka audio z rodziny %d ...\n"
#define MSGTR_CantFindAfmtFallback "Nie mog� znale�� kodeka audio dla wymuszonej rodziny, wracam do standardowych.\n"
#define MSGTR_CantFindAudioCodec "Nie mog� znale�� kodeka dla formatu audio 0x%X !\n"
#define MSGTR_TryUpgradeCodecsConfOrRTFM "*** Spr�buj uaktualni� %s etc/codecs.conf\n*** Je�li to nie pomaga, przeczytaj DOCS/codecs.html !\n"
#define MSGTR_CouldntInitAudioCodec "Nie moge zainicjowa� sterownika audio! -> nosound\n"
#define MSGTR_TryForceVideoFmt "Wymuszam zastosowanie kodeka video z rodziny %d ...\n"
#define MSGTR_CantFindVfmtFallback "Nie mog� znale�� kodeka video dla wymuszonej rodziny, wracam do standardowych..\n"
#define MSGTR_CantFindVideoCodec "Nie mog� znale�� kodeka dla formatu video 0x%X !\n"
#define MSGTR_VOincompCodec "Przepraszam, wybrany sterownik video_out jest niekompatybilny z tym kodekiem.\n"
#define MSGTR_CouldntInitVideoCodec "FATAL: Nie mog� zainicjowa� kodeka video :(\n"
#define MSGTR_EncodeFileExists "Plik ju� istnieje: %s (nie nadpisz swojego ulubionego AVI!)\n"
#define MSGTR_CantCreateEncodeFile "Nie mog� stworzy� pliku do zakodowania\n"
#define MSGTR_CannotInitVO "FATAL: Nie mog� zainicjowa� sterownika video!\n"
#define MSGTR_CannotInitAO "Nie mog� otworzy�/zainicjowa� urz�dzenia audio -> NOSOUND\n"
#define MSGTR_StartPlaying "Pocz�tek odtwarzania...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"         ************************************\n"\
"         *** Tw�j system jest zbyt wolny! ***\n"\
"         ************************************\n"\
"- Najcz�stsza przyczyna: uszkodzony/obarczony b�edami sterownik _audio_.\n"\
"  Rozwi�zanie: spr�buj -ao sdl lub u�yj ALSA 0.5 lub emulacj� OSS w ALSA 0.9\n"\
"  Przeczytaj DOCS/sound.html!\n"\
"- Wolny sterownik video. Spr�buj z inny sterownikiem -vo (lista: -vo help)\n"\
"  lub odtwarzaj z opcj� -framedrop ! Przeczytaj DOCS/video.html!\n"\
"- Wolny procesor. Nie odtwarzaj du�ych dvd/divx na wolnych procesorach!\n"\
"  Spr�buj z opcj� -hardframedrop\n"\
"- Uszkodzony plik. Spr�buj r�nych kombinacji: -nobps -ni -mc 0 -forceidx\n"\
"- Je�li nic z powy�szego nie pomaga, przeczytaj DOCS/bugreports.html !\n\n"

#define MSGTR_NoGui "MPlayer zosta� skompilowany BEZ obs�ugi GUI!\n"
#define MSGTR_GuiNeedsX "GUI MPlayera wymaga X11!\n"
#define MSGTR_Playing "Odtwarzam %s\n"
#define MSGTR_NoSound "Audio: brak d�wi�ku!!!\n"
#define MSGTR_FPSforced "FPS wymuszone na %5.3f  (ftime: %5.3f)\n"

// open.c:, stream.c
#define MSGTR_CdDevNotfound "Urz�dzenie CD-ROM '%s' nie znalezione!\n"
#define MSGTR_ErrTrackSelect "B��d wyboru �cie�ki VCD!"
#define MSGTR_ReadSTDIN "Odczytuj� ze stdin...\n"
#define MSGTR_UnableOpenURL "Nie mog� otworzy� URL: %s\n"
#define MSGTR_ConnToServer "Po��czony z serwerem: %s\n"
#define MSGTR_FileNotFound "Plik nie znaleziony: '%s'\n"

#define MSGTR_CantOpenDVD "Nie mog� otworzy� urz�dzenia DVD: %s\n"
#define MSGTR_DVDwait "Odczytuj� struktur� dysku, prosz� czeka�...\n"
#define MSGTR_DVDnumTitles "Na tym DVD znajduje si� %d tytu��w.\n"
#define MSGTR_DVDinvalidTitle "Nieprawid�owy numer tytu�u DVD: %d\n"
#define MSGTR_DVDnumChapters "W tym tytule DVD znajduje si� %d rozdzia��w.\n"
#define MSGTR_DVDinvalidChapter "Nieprawid�owy numer rozdzia�u DVD: %d\n"
#define MSGTR_DVDnumAngles "W tym tytule DVD znajduje si� %d ustawie� kamery.\n"
#define MSGTR_DVDinvalidAngle "Nieprawid�owy numer ustawienia kamery DVD: %d\n"
#define MSGTR_DVDnoIFO "Nie mog� otworzy� pliku IFO dla tytu�u DVD %d.\n"
#define MSGTR_DVDnoVOBs "Nie mog� otworzy� tytu�u VOBS (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD otwarte poprawnie!\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "Uwaga! Nag��wek strumienia audio %d przedefiniowany!\n"
#define MSGTR_VideoStreamRedefined "Uwaga! Nag��wek strumienia video %d przedefiniowany!\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: Zbyt wiele (%d w %d bajtach) pakiet�w audio w buforze!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: Zbyt wiele (%d w %d bajtach) pakiet�w video w buforze!\n"
#define MSGTR_MaybeNI "(mo�e odtwarzasz strumie�/plik non-interleaved lub kodek nie zadzia�a�)\n"
#define MSGTR_DetectedFLIfile "Wykryto format FLI!\n"
#define MSGTR_DetectedAVIfile "Wykryto format AVI!\n"
#define MSGTR_DetectedASFfile "Wykryto format ASF!\n"
#define MSGTR_DetectedMPEGPESfile "Wykryto format MPEG-PES!\n"
#define MSGTR_DetectedMPEGPSfile "Wykryto format MPEG-PS!\n"
#define MSGTR_DetectedMPEGESfile "Wykryto format MPEG-ES!\n"
#define MSGTR_DetectedQTMOVfile "Wykryto format QuickTime/MOV!\n"
#define MSGTR_MissingMpegVideo "Zagubiony strumie� video MPEG !? skontaktuj si� z autorem, mo�e to b��d:(\n"
#define MSGTR_InvalidMPEGES "B��dny strumie� MPEG-ES ??? skontaktuj si� z autorem, mo�e to b��d:(\n"
#define MSGTR_FormatNotRecognized "========== Przepraszam,  format pliku nierozpoznany/nieobs�ugiwany ==========\n"\
				  "=== Je�li to strumie� AVI, ASF lub MPEG, prosz� skontaktuj si� z autorem! ===\n"
#define MSGTR_MissingVideoStream "Nie znaleziono strumienia video!\n"
#define MSGTR_MissingAudioStream "Nie znaleziono strumienia audio... -> nosound\n"
#define MSGTR_MissingVideoStreamBug "Zgubiony strumie� video!? skontaktuj si� z autorem, mo�e to b��d:(\n"

#define MSGTR_DoesntContainSelectedStream "demux: plik nie zawiera wybranego strumienia audio lub video\n"

#define MSGTR_NI_Forced "Wymuszony"
#define MSGTR_NI_Detected "Wykryty"
#define MSGTR_NI_Message "%s format pliku NON-INTERLEAVED AVI !\n"

#define MSGTR_UsingNINI "U�ywa uszkodzonego formatu pliku NON-INTERLEAVED AVI !\n"
#define MSGTR_CouldntDetFNo "Nie mog� okre�li� liczby klatek (dla przeszukiwania)\n"
#define MSGTR_CantSeekRawAVI "Nie mog� przeszukiwa� nieindeksowanych strumieni .AVI! (sprawd� opcj� -idx !)\n"
#define MSGTR_CantSeekFile "Nie mog� przeszukiwa� tego pliku!  \n"

#define MSGTR_EncryptedVOB "Zaszyfrowany plik VOB (nie wkompilowano obs�ugi libcss)! Przeczytaj plik DOCS/cd-dvd.html\n"
#define MSGTR_EncryptedVOBauth "Zaszyfrowany strumie�, nie za�yczy�e� sobie autentykacji!!\n"

#define MSGTR_MOVcomprhdr "MOV: Spakowane nag��wki nie s� obs�ugiwane (na razie)!\n"
#define MSGTR_MOVvariableFourCC "MOV: Uwaga! wykryto zmienn� FOURCC!?\n"
#define MSGTR_MOVtooManyTrk "MOV: Uwaga! zbyt du�o scie�ek!"
#define MSGTR_MOVnotyetsupp "\n**** Format Quicktime MOV nie jest na razie obs�ugiwany !!!!!!! ****\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "nie mog� otworzy� kodeka\n"
#define MSGTR_CantCloseCodec "nie mog� zamkn�� kodeka\n"

#define MSGTR_MissingDLLcodec "ERROR: Nie mog� otworzy� wymaganego kodeka DirectShow: %s\n"
#define MSGTR_ACMiniterror "Nie mog� za�adowa�/zainicjalizowa� kodeka Win32/ACM AUDIO (brakuje pliku DLL?)\n"
#define MSGTR_MissingLAVCcodec "Nie moge znale�� w libavcodec kodeka '%s' ...\n"

#define MSGTR_NoDShowSupport "MPlayer skompilowany BEZ obs�ugi directshow!\n"
#define MSGTR_NoWfvSupport "Obs�uga kodek�w win32 wy��czona lub niedost�pna na platformach nie-x86!\n"
#define MSGTR_NoDivx4Support "MPlayer skompilowany BEZ obs�ugi DivX4Linux (libdivxdecore.so)!\n"
#define MSGTR_NoLAVCsupport "MPlayer skompilowany BEZ obs�ugi ffmpeg/libavcodec!\n"
#define MSGTR_NoACMSupport "Kodek audio Win32/ACM wy��czony lub niedost�pny dla nie-x86 CPU -> wymuszam brak d�wi�ku :(\n"
#define MSGTR_NoDShowAudio "Skompilowane bez obs�ugi DirectShow -> wymuszam brak d�wi�ku :(\n"
#define MSGTR_NoOggVorbis "Kodek audio OggVorbis wy��czony -> wymuszam brak d�wi�ku :(\n"
#define MSGTR_NoXAnimSupport "MPlayer skompilowany BEZ obs�ugi XAnim!\n"

#define MSGTR_MpegPPhint "UWAGA! Za��da�e� u�ycia filtra wyg�adzaj�cego dla video MPEG 1/2,\n" \
			 "       ale skompilowa�e� MPlayera bez obs�ugi wyg�adzania dla MPEG 1/2!\n" \
			 "       #define MPEG12_POSTPROC w config.h, i przekompiluj libmpeg2!\n"
#define MSGTR_MpegNoSequHdr "MPEG: FATAL: EOF podczas przeszukiwania nag��wka sekwencji\n"
#define MSGTR_CannotReadMpegSequHdr "FATAL: Nie mog� odczyta� nag��wka sekwencji!\n"
#define MSGTR_CannotReadMpegSequHdrEx "FATAL: Nie mog� odczyta� rozszerzenia nag��wka sekwencji!!\n"
#define MSGTR_BadMpegSequHdr "MPEG: Nieprawid�owy nag��wek sekwencji!\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: Nieprawid�owe rozszerzenie nag��wka sekwencji!\n"

#define MSGTR_ShMemAllocFail "Nie mog� zaalokowa� pami�ci dzielonej\n"
#define MSGTR_CantAllocAudioBuf "Nie mog� zaalokowa� buforu wyj�ciowego audio\n"
#define MSGTR_NoMemForDecodedImage "Za ma�o pami�ci dla zdekodowanego bufora obrazu (%ld bajt�w)\n"

#define MSGTR_AC3notvalid "Nieprawid�owy strumie� AC3.\n"
#define MSGTR_AC3only48k "Obs�ugiwane s� tylko strumienie 48000 Hz.\n"
#define MSGTR_UnknownAudio "Nieznany/zgubiony format audio, nie u�ywam d�wi�ku\n"

// LIRC:
#define MSGTR_SettingUpLIRC "W��czam obs�ug� lirc...\n"
#define MSGTR_LIRCdisabled "Nie b�dziesz m�g� u�ywa� twojego pilota\n"
#define MSGTR_LIRCopenfailed "Nieudane otwarcie obs�ugi lirc!\n"
#define MSGTR_LIRCsocketerr "Co� jest nie tak z socketem lirc: %s\n"
#define MSGTR_LIRCcfgerr "Nieudane odczytanie pliku konfiguracyjnego LIRC %s !\n"


// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "O programie"
#define MSGTR_FileSelect "Wyb�r pliku ..."
#define MSGTR_SubtitleSelect "Wyb�r napis�w ..."
#define MSGTR_OtherSelect "Wyb�r ..."
#define MSGTR_MessageBox "Komunikat"
#define MSGTR_PlayList "Playlista"
#define MSGTR_SkinBrowser "Przegl�darka Sk�rek"

// --- buttons ---
#define MSGTR_Ok "Tak"
#define MSGTR_Cancel "Anuluj"
#define MSGTR_Add "Dodaj"
#define MSGTR_Remove "Usu�"

// --- error messages ---
#define MSGTR_NEMDB "Przepraszam, za ma�o pami�ci na bufor rysowania."
#define MSGTR_NEMFMR "Przepraszam, za ma�o pami�ci na renderowanie menu."
#define MSGTR_NEMFMM "Przepraszam, za ma�o pami�ci na mask� kszta�tu g��wnego okna."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[skin] b��d w pliku konfiguracyjnym sk�rki w linii %d: %s"
#define MSGTR_SKIN_WARNING1 "[skin] ostrze�enie w pliku konfiguracyjnym sk�rki w linii %d: widget znaleziony, ale poprzednia \"sekcja\" nie znaleziona ( %s )"
#define MSGTR_SKIN_WARNING2 "[skin] ostrze�enie w pliku konfiguracyjnym sk�rki w linii %d: widget znaleziony, ale poprzednia \"subsekcja\" nie znaleziona (%s)"
#define MSGTR_SKIN_BITMAP_16bit "Bitmapy 16 bitowe lub mniejsze nie obs�ugiwane ( %s ).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound "plik nie znaleziony ( %s )\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "b��d odczytu bmp ( %s )\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "b��d odczytu tga ( %s )\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "b��d odczytu png ( %s )\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "tga pakowane RLE nie obs�ugiwane ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "nieznany typ pliku ( %s )\n"
#define MSGTR_SKIN_BITMAP_ConvertError "b��d konwersji 24 bit�w na 32 bity ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "nieznany komunikat: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "za ma�o pami�ci\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "za du�o zadeklarowanych font�w\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "nie znaleziono pliku z fontami\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "nie znaleziono pliku z obrazem fontu\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "nie istniej�cy identyfikator fontu ( %s )\n"
#define MSGTR_SKIN_UnknownParameter "nieznany parametr ( %s )\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[skinbrowser] za ma�o pami�ci.\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "Sk�rka nie znaleziona ( %s ).\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "B�ad odczytu pliku konfiguracyjnego sk�rki ( %s ).\n"
#define MSGTR_SKIN_LABEL "Sk�rki:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "O MPlayerze"
#define MSGTR_MENU_Open "Otw�rz ..."
#define MSGTR_MENU_PlayFile "Odtwarzaj plik ..."
#define MSGTR_MENU_PlayVCD "Odtwarzaj VCD ..."
#define MSGTR_MENU_PlayDVD "Odtwarzaj DVD ..."
#define MSGTR_MENU_PlayURL "Odtwarzaj URL ..."
#define MSGTR_MENU_LoadSubtitle "Za�aduj napisy ..."
#define MSGTR_MENU_Playing "Odtwarzanie"
#define MSGTR_MENU_Play "Odtwarzaj"
#define MSGTR_MENU_Pause "Pauza"
#define MSGTR_MENU_Stop "Stop"
#define MSGTR_MENU_NextStream "Nast�pny strumie�"
#define MSGTR_MENU_PrevStream "Poprzedni strumie�"
#define MSGTR_MENU_Size "Wielko��"
#define MSGTR_MENU_NormalSize "Normalna wielko��"
#define MSGTR_MENU_DoubleSize "Podw�jna wielko��"
#define MSGTR_MENU_FullScreen "Pe�en Ekran"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_PlayDisc "Odtwarzaj dysk ..."
#define MSGTR_MENU_ShowDVDMenu "Poka� menu DVD"
#define MSGTR_MENU_Titles "Tytu�y"
#define MSGTR_MENU_Title "Tytu� %2d"
#define MSGTR_MENU_None "(puste)"
#define MSGTR_MENU_Chapters "Rozdzia�y"
#define MSGTR_MENU_Chapter "Rozdzia� %2d"
#define MSGTR_MENU_AudioLanguages "J�zyki audio"
#define MSGTR_MENU_SubtitleLanguages "J�zyki napis�w"
#define MSGTR_MENU_PlayList "Playlista"
#define MSGTR_MENU_SkinBrowser "Przegl�darka sk�rek"
#define MSGTR_MENU_Preferences "Preferencje"
#define MSGTR_MENU_Exit "Wyj�cie ..."

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "b��d krytyczny ..."
#define MSGTR_MSGBOX_LABEL_Error "b��d ..."
#define MSGTR_MSGBOX_LABEL_Warning "ostrze�enie ..." 

#endif

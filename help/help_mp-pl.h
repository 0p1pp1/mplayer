// Translated by:  Bohdan Horst <nexus@hoth.amu.edu.pl>
// Wszelkie uwagi i poprawki mile widziane :)
//
// Fixes and updates: Wojtek Kaniewski <wojtekka@bydg.pdi.net>
// Last sync on 2003-07-30 with help_mp-en.h 1.105

// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"U�ycie:   mplayer [opcje] [url|�cie�ka/]nazwa\n"
"\n"
"Podstawowe opcje: (pe�na lista w manualu)\n"
" -vo <drv[:dev]> wyb�r sterownika[:urz�dzenia] video (lista po '-vo help')\n"
" -ao <drv[:dev]> wyb�r sterownika[:urz�dzenia] audio (lista po '-ao help')\n"
#ifdef HAVE_VCD
" vcd://<trackno>  odtwarzanie bezpo�rednio �cie�ki VCD (video cd)\n"
#endif
#ifdef HAVE_LIBCSS
" -dvdauth <dev>  urz�dzenie DVD do autoryzacji (dla zaszyfrowanych dysk�w)\n"
#endif
#ifdef USE_DVDREAD
" dvd://<titleno>  odtwarzanie bezpo�rednio tytu�u DVD\n"
" -alang/-slang   j�zyk dla d�wi�ku/napis�w (poprzez 2-znakowy kod kraju)\n"
#endif
" -ss <timepos>   skok do podanej pozycji (sekundy albo hh:mm:ss)\n"
" -nosound        odtwarzanie bez d�wi�ku\n"
" -fs -vm -zoom   opcje pe�noekranowe (pe�en ekran,zmiana trybu,skalowanie)\n"
" -x <x> -y <y>   wyb�r rozdzielczo�ci ekranu (dla -vm lub -zoom)\n"
" -sub <plik>     wyb�r pliku z napisami (zobacz tak�e -subfps, -subdelay)\n"
" -playlist <plik>wyb�r pliku z playlist�\n"
" -vid x -aid y   wyb�r odtwarzanego strumienia video (x) i audio (y)\n"
" -fps x -srate y wyb�r pr�dko�ci odtwarzania video (x fps) i audio (y Hz)\n"
" -pp <opcje>     wyb�r postprocesingu (zobacz manual)\n"
" -framedrop      gubienie klatek (dla wolnych maszyn)\n"
"\n"
"Podstawowe klawisze: (pe�na lista w manualu, sprawd� tak�e input.conf)\n"
" Right,Up,PgUp   skok naprz�d o 10 sekund, 1 minut�, 10 minut\n"
" Left,Down,PgDn  skok do ty�u o 10 sekund, 1 minut�, 10 minut\n"
" < lub >         przeskok o jedn� pozycj� w playli�cie\n"
" p lub SPACE     zatrzymanie filmu (kontynuacja - dowolny klawisz)\n"
" q lub ESC       zatrzymanie odtwarzania i wyj�cie z programu\n"
" + lub -         regulacja op�nienia d�wi�ku o +/- 0,1 sekundy\n"
" o               prze��czanie tryb�w OSD: pusty / belka / belka i zegar\n"
" * lub /         zwi�kszenie lub zmniejszenie nat�enia d�wi�ku\n"
" z lub x         regulacja op�nienia napis�w o +/- 0,1 sekundy\n"
" r lub t         regulacja po�o�enia napis�w (zobacz tak�e -vf expand)\n"
"\n"
" **** DOK�ADNY SPIS WSZYSTKICH DOST�PNYCH OPCJI ZNAJDUJE SI� W MANUALU! ****\n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// mplayer.c: 

#define MSGTR_Exiting "\nWychodz�... (%s)\n"
#define MSGTR_Exit_quit "Wyj�cie"
#define MSGTR_Exit_eof "Koniec pliku"
#define MSGTR_Exit_error "B��d krytyczny"
#define MSGTR_IntBySignal "\nMPlayer przerwany sygna�em %d w module: %s \n"
#define MSGTR_NoHomeDir "Nie mog� znale�� katalogu domowego\n"
#define MSGTR_GetpathProblem "problem z get_path(\"config\")\n"
#define MSGTR_CreatingCfgFile "Tworz� plik z konfiguracj�: %s\n"
#define MSGTR_InvalidVOdriver "Nieprawid�owa nazwa sterownika video: %s\nU�yj '-vo help' aby dosta� list� dost�pnych sterownik�w video.\n"
#define MSGTR_InvalidAOdriver "Nieprawid�owa nazwa sterownika audio: %s\nU�yj '-ao help' aby dosta� list� dost�pnych sterownik�w audio.\n"
#define MSGTR_CopyCodecsConf "(skopiuj/zlinkuj etc/codecs.conf do ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "U�ywam domy�lnego (wkompilowanego) codecs.conf\n"
#define MSGTR_CantLoadFont "Nie mog� za�adowa� czcionki: %s\n"
#define MSGTR_CantLoadSub "Nie mog� za�adowa� napis�w: %s\n"
#define MSGTR_ErrorDVDkey "B��d w przetwarzaniu DVD KEY.\n"
#define MSGTR_CmdlineDVDkey "��dany klucz DVD u�ywany jest do dekodowania.\n"
#define MSGTR_DVDauthOk "Sekwencja autoryzacji DVD wygl�da OK.\n"
#define MSGTR_DumpSelectedStreamMissing "dump: FATAL: nie ma wybranego strumienia!\n"
#define MSGTR_CantOpenDumpfile "Nie mog� otworzy� pliku dump.\n"
#define MSGTR_CoreDumped "Core dumped ;)\n"
#define MSGTR_FPSnotspecified "FPS nie podane (lub b��dne) w nag��wku. U�yj opcji -fps.\n"
#define MSGTR_TryForceAudioFmtStr "Wymuszam zastosowanie kodeka audio z rodziny %s...\n"
#define MSGTR_CantFindAfmtFallback "Nie mog� znale�� kodeka audio dla wymuszonej rodziny, wracam do standardowych.\n"
#define MSGTR_CantFindAudioCodec "Nie mog� znale�� kodeka dla formatu audio 0x%X.\n"
#define MSGTR_RTFMCodecs "Przeczytaj DOCS/pl/codecs.html!\n"
#define MSGTR_CouldntInitAudioCodec "Nie moge zainicjowa� sterownika audio -> brak d�wi�ku.\n"
#define MSGTR_TryForceVideoFmtStr "Wymuszam zastosowanie kodeka video z rodziny %s...\n"
#define MSGTR_CantFindVideoCodec "Nie mog� znale�� kodeka dla wybranego -vo i formatu video 0x%X.\n"
#define MSGTR_VOincompCodec "Przykro mi, wybrany sterownik video_out jest niekompatybilny z tym kodekiem.\n"
#define MSGTR_CannotInitVO "B��D: Nie mog� zainicjowa� sterownika video.\n"
#define MSGTR_CannotInitAO "Nie mog� otworzy�/zainicjowa� urz�dzenia audio -> brak d�wi�ku.\n"
#define MSGTR_StartPlaying "Pocz�tek odtwarzania...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"         ************************************\n"\
"         *** Tw�j system jest zbyt wolny! ***\n"\
"         ************************************\n"\
"Mo�liwe przyczyny, problemy, rozwi�zania: \n"\
"- Najcz�stsza przyczyna: uszkodzony/obarczony b��dami sterownik _audio_.\n"\
"  Rozwi�zanie: spr�buj -ao sdl lub u�yj ALSA 0.5 lub emulacj� OSS w ALSA 0.9\n"\
"  Przeczytaj DOCS/pl/sound.html!\n"\
"  Mo�esz tak�e eksperymentowa� z -autosync 30 (lub innymi warto�ciami).\n"\
"- Wolny sterownik video. Spr�buj z inny sterownikiem -vo (lista: -vo help)\n"\
"  lub odtwarzaj z opcj� -framedrop!\n"\
"- Wolny procesor. Nie odtwarzaj du�ych DVD/DivX na wolnych procesorach!\n"\
"  Spr�buj z opcj� -hardframedrop.\n"\
"- Uszkodzony plik. Spr�buj r�nych kombinacji: -nobps -ni -mc 0 -forceidx.\n"\
"- Odtwarzaj�c z wolnego �r�d�a (zamontowane partycje NFS/SMB, DVD, VCD itp)\n"\
"  spr�buj z opcj� -cache 8192\n"\
"- U�ywasz -cache do odtwarzania plikow non-interleaved? Spr�buj z -nocache\n"\
"Przeczytaj DOCS/pl/video.html i DOCS/pl/sound.html -- znajdziesz tam\n"\
"wskaz�wki jak przyspieszy� dzia�anie. Je�li nic z powy�szego nie pomaga,\n"\
"przeczytaj DOCS/pl/bugreports.html.\n\n"

#define MSGTR_NoGui "MPlayer zosta� skompilowany BEZ obs�ugi GUI.\n"
#define MSGTR_GuiNeedsX "GUI MPlayera wymaga X11.\n"
#define MSGTR_Playing "Odtwarzam %s\n"
#define MSGTR_NoSound "Audio: brak d�wi�ku\n"
#define MSGTR_FPSforced "FPS wymuszone na %5.3f  (ftime: %5.3f)\n"
#define MSGTR_CompiledWithRuntimeDetection "Skompilowane z wykrywaniem procesora - UWAGA - to nie jest optymalne!\nAby uzyska� lepsz� wydajno��, przekompiluj MPlayera z opcj�\n--disable-runtime-cpudetection\n"
#define MSGTR_CompiledWithCPUExtensions "Skompilowane dla x86 CPU z rozszerzeniami:"
#define MSGTR_AvailableVideoOutputPlugins "Dost�pne pluginy video:\n"
#define MSGTR_AvailableVideoOutputDrivers "Dost�pne sterowniki video:\n"
#define MSGTR_AvailableAudioOutputDrivers "Dost�pne sterowniki audio:\n"
#define MSGTR_AvailableAudioCodecs "Dost�pne kodeki audio:\n"
#define MSGTR_AvailableVideoCodecs "Dost�pne kodeki video:\n"
#define MSGTR_AvailableAudioFm "\nDost�pne (wkompilowane) rodziny kodek�w audio/sterowniki:\n"
#define MSGTR_AvailableVideoFm "\nDost�pne (wkompilowane) rodziny kodek�w video/sterowniki:\n"
#define MSGTR_AvailableFsType "Dost�pne tryby pe�noekranowe:\n"
#define MSGTR_UsingRTCTiming "U�ywam Linux's hardware RTC timing (%ldHz)\n"
#define MSGTR_CannotReadVideoProperties "Video: nie mog� odczyta� w�a�ciwo�ci\n"
#define MSGTR_NoStreamFound "Nie znaleziono strumienia.\n"
#define MSGTR_InitializingAudioCodec "Inicjalizuj� kodek audio...\n"
#define MSGTR_ErrorInitializingVODevice "B��d otwierania/inicjalizacji wybranego video_out (-vo) urz�dzenia!\n"
#define MSGTR_ForcedVideoCodec "Wymuszony kodek video: %s\n"
#define MSGTR_ForcedAudioCodec "Wymuszony kodek audio: %s\n"
#define MSGTR_AODescription_AOAuthor "AO: Opis: %s\nAO: Autor: %s\n"
#define MSGTR_AOComment "AO: Komentarz: %s\n"
#define MSGTR_Video_NoVideo "Video: brak video\n"
#define MSGTR_NotInitializeVOPorVO "\nFATAL: Nie mog� zainicjowa� filtr�w video (-vf) lub wyj�cia video (-vo).\n"
#define MSGTR_Paused "\n  =====  PAUZA  =====\r"
#define MSGTR_PlaylistLoadUnable "\nNie mo�na za�adowa� playlisty %s.\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- MPlayer wykona� nieprawid�ow� operacj�.\n"\
"  By� mo�e to b��d w nowym kodzie wykrywania procesora...\n"\
"  Przeczytaj DOCS/pl/bugreports.html.\n"
#define MSGTR_Exit_SIGILL \
"- MPlayer wykona� nieprawid�ow� operacj�.\n"\
"  Zwykle zdarza si� to, gdy uruchamiasz go na innym procesorze ni� ten,\n"\
"  dla kt�rego zosta� skompilowany/zoptymalizowany.\n  Sprawd� to!\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- MPlayer wykona� nieprawid�ow� operacj� zwi�zan� z pami�ci�/koprocesorem.\n"\
"  Przekompiluj MPlayera z --enable-debug i wykonad backtrace 'gdb' i\n"\
"  deasemblacj�. Szczeg�y w pliku DOCS/pl/bugreports.html#crash\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer wykona� nieprawid�ow� operacj�. To nie powinno si� zdarzy�.\n"\
"  By� mo�e to b��d w kodzie MPlayera _lub_ w Twoich sterownikach _lub_\n"\
"  w u�ywanej przez Ciebie wersji gcc. Je�li my�lisz, �e to wina MPlayera,\n"\
"  przeczytaj prosz� DOCS/pl/bugreports.html i post�puj zgodnie z instrukacjami.\n"\
"  Nie mo�emy pom�c i nie pomo�emy je�li nie dostarczysz tych informacji przy\n"\
"  zg�aszaniu b��du.\n"
	

// mencoder.c:

#define MSGTR_UsingPass3ControllFile "U�ywam pliku kontrolnego pass3: %s\n"
#define MSGTR_MissingFilename "\nBrak nazwy pliku.\n\n"
#define MSGTR_CannotOpenFile_Device "Nie mo�na otworzy� pliku/urz�dzenia.\n"
#define MSGTR_ErrorDVDAuth "B�ad w autoryzacji DVD.\n"
#define MSGTR_CannotOpenDemuxer "Nie mo�na otworzy� demuxera\n"
#define MSGTR_NoAudioEncoderSelected "\nNie wybrano enkodera audio (-oac). Wybierz jeden lub u�yj -nosound. U�yj -oac help!\n"
#define MSGTR_NoVideoEncoderSelected "\nNie wybrano enkodera video (-ovc). Wybierz jeden, u�yj -ovc help!\n"
#define MSGTR_InitializingAudioCodec "Inicjalizuje kodek audio...\n"
#define MSGTR_CannotOpenOutputFile "Nie mog� otworzy� pliku wynikowego: '%s'.\n"
#define MSGTR_EncoderOpenFailed "Nie mog� otworzy� encodera.\n"
#define MSGTR_ForcingOutputFourcc "Wymuszam fourcc wynikowe na %x [%.4s]\n"
#define MSGTR_WritingAVIHeader "Zapisuje nag��wek AVI...\n"
#define MSGTR_DuplicateFrames "\npowt�rzone %d ramek!\n"
#define MSGTR_SkipFrame "\nOpuszczam ramk�!\n"
#define MSGTR_ErrorWritingFile "%s: B��d zapisu pliku.\n"
#define MSGTR_WritingAVIIndex "\nZapisuj� indeks AVI...\n"
#define MSGTR_FixupAVIHeader "Naprawiam nag��wek AVI...\n"
#define MSGTR_RecommendedVideoBitrate "Zalecane video bitrate dla %s CD: %d\n"
#define MSGTR_VideoStreamResult "\nStrumie� video: %8.3f kbit/s (%d bps) rozmiar: %d bajt�w %5.3f sekund %d ramek\n"
#define MSGTR_AudioStreamResult "\nStrumie� audio: %8.3f kbit/s (%d bps) rozmiar: %d bajt�w %5.3f sekund\n"

// cfg-mencoder.h:

#define MSGTR_MEncoderMP3LameHelp "\n\n"\
" vbr=<0-4>     metoda zmiennego bitrate\n"\
"                0: cbr\n"\
"                1: mt\n"\
"                2: rh(domy�lna)\n"\
"                3: abr\n"\
"                4: mtrh\n"\
"\n"\
" abr           �redni bitrate\n"\
"\n"\
" cbr           sta�y bitrate\n"\
"               Wymusza r�wnie� kodowanie CBR w nast�puj�cych po tej opcji\n"\
"               ustawieniach ABR\n"\
"\n"\
" br=<0-1024>   podaje bitrate w kilobitach (tylko CBR i ABR)\n"\
"\n"\
" q=<0-9>       jako�� (0-najwy�sza, 9-najni�sza) (tylko VBR)\n"\
"\n"\
" aq=<0-9>      jako�� algorytmu (0-najlepsza/najwolniejsza,\n"\
"               9-najgorsza/najszybsza)\n"\
"\n"\
" ratio=<1-100> wsp�czynnik kompresji\n"\
"\n"\
" vol=<0-10>    wzmocnienie sygna�u audio\n"\
"\n"\
" mode=<0-3>    (domy�lnie: auto)\n"\
"                0: stereo\n"\
"                1: joint-stereo\n"\
"                2: dualchannel\n"\
"                3: mono\n"\
"\n"\
" padding=<0-2>\n"\
"                0: no\n"\
"                1: all\n"\
"                2: adjust\n"\
"\n"\
" fast          w��cza szybsze kodowanie w nast�puj�cych po tej opcji\n"\
"               ustawieniach VBR, nieznacznie ni�sza jako�� i wy�szy bitrate.\n"\
"\n"\
" preset=<value>  w��cza najwy�sze mo�liwe ustawienia jako�ci.\n"\
"                 medium: kodowanie VBR, dobra jako��\n"\
"                 (bitrate w zakresie 150-180 kbps)\n"\
"                 standard: kodowanie VBR, wysoka jako��\n"\
"                 (bitrate w zakresie 170-210 kbps)\n"\
"                 extreme: kodowanie VBR, bardzo wysoka jako��\n"\
"                 (bitrate w zakresie 200-240 kbps)\n"\
"                 insane: kodowanie CBR, najwy�sza mo�liwa jako��\n"\
"                 (bitrate 320 kbps)\n"\
"                 <8-320>: kodowanie ABR przy podanym �rednim bitrate.\n\n"
	
// open.c, stream.c:
#define MSGTR_CdDevNotfound "Urz�dzenie CD-ROM '%s' nie znalezione.\n"
#define MSGTR_ErrTrackSelect "B��d wyboru �cie�ki VCD."
#define MSGTR_ReadSTDIN "Odczytuj� ze stdin...\n"
#define MSGTR_UnableOpenURL "Nie mog� otworzy� URL: %s\n"
#define MSGTR_ConnToServer "Po��czony z serwerem: %s\n"
#define MSGTR_FileNotFound "Plik nieznaleziony: '%s'\n"

#define MSGTR_SMBInitError "Nie mog� zainicjowa� biblioteki libsmbclient: %d\n"
#define MSGTR_SMBFileNotFound "Nie mog� otworzy� z sieci: '%s'\n"
#define MSGTR_SMBNotCompiled "MPlayer nie zosta� skompilowany z obs�ug� SMB\n"

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
#define MSGTR_AudioStreamRedefined "Uwaga! Nag��wek strumienia audio %d zdefiniowany ponownie.\n"
#define MSGTR_VideoStreamRedefined "Uwaga! Nag��wek strumienia video %d zdefiniowany ponownie.\n"
#define MSGTR_TooManyAudioInBuffer "\nZbyt wiele (%d w %d bajtach) pakiet�w audio w buforze.\n"
#define MSGTR_TooManyVideoInBuffer "\nZbyt wiele (%d w %d bajtach) pakiet�w video w buforze.\n"
#define MSGTR_MaybeNI "Mo�e odtwarzasz strumie�/plik non-interleaved lub kodek nie zadzia�a�?\n"\
		      "Dla plik�w .AVI spr�buj wymusi� tryb non-interleaved poprzez opcje -ni\n"
#define MSGTR_SwitchToNi "\nWykryto non-interleaved .AVI - prze��czam na tryb -ni...\n"
#define MSGTR_Detected_XXX_FileFormat "Wykryto format %s.\n"
#define MSGTR_DetectedAudiofile "Wykryto plik audio.\n"
#define MSGTR_NotSystemStream "Nie jest to MPEG System Stream... (mo�e Transport Stream?)\n"
#define MSGTR_InvalidMPEGES "B��dny strumie� MPEG-ES??? Skontaktuj si� z autorem, mo�e to b��d :(\n"
#define MSGTR_FormatNotRecognized "=========== Przykro mi, nierozpoznany/nieobs�ugiwany format pliku ===========\n"\
				  "=== Je�li to strumie� AVI, ASF lub MPEG, prosz� skontaktuj si� z autorem! ===\n"
#define MSGTR_MissingVideoStream "Nie znaleziono strumienia video.\n"
#define MSGTR_MissingAudioStream "Nie znaleziono strumienia audio -> brak d�wi�ku.\n"
#define MSGTR_MissingVideoStreamBug "Brakuj�cy strumie� video?! Skontaktuj si� z autorem, mo�e to b��d :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: Plik nie zawiera wybranego strumienia audio lub video\n"

#define MSGTR_NI_Forced "Wymuszony"
#define MSGTR_NI_Detected "Wykryty"
#define MSGTR_NI_Message "%s format pliku NON-INTERLEAVED AVI.\n"

#define MSGTR_UsingNINI "U�ywa uszkodzonego formatu pliku NON-INTERLEAVED AVI.\n"
#define MSGTR_CouldntDetFNo "Nie mog� okre�li� liczby klatek (dla przeszukiwania)\n"
#define MSGTR_CantSeekRawAVI "Nie mog� przeszukiwa� nieindeksowanych strumieni .AVI! (sprawd� opcj� -idx!)\n"
#define MSGTR_CantSeekFile "Nie mog� przeszukiwa� tego pliku.\n"

#define MSGTR_EncryptedVOB "Zaszyfrowany plik VOB (nie wkompilowano obs�ugi libcss)! Przeczytaj DOCS/pl/cd-dvd.html\n"
#define MSGTR_EncryptedVOBauth "Zaszyfrowany strumie�, nie za��da�e� autoryzacji!\n"

#define MSGTR_MOVcomprhdr "MOV: Skompresowane nag��wki nie s� obs�ugiwane (na razie).\n"
#define MSGTR_MOVvariableFourCC "MOV: Uwaga! Wykryto zmienn� FOURCC!?\n"
#define MSGTR_MOVtooManyTrk "MOV: Uwaga! Zbyt du�o scie�ek!"
#define MSGTR_FoundAudioStream "==> Znaleziono strumie� audio: %d\n"
#define MSGTR_FoundVideoStream "==> Znaleziono strumie� video: %d\n"
#define MSGTR_DetectedTV "Wykryto TV! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "Nie mo�na otworzy� demuxera ogg.\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: Szukanie strumieni audio (id:%d)\n"
#define MSGTR_CannotOpenAudioStream "Nie mo�na otworzy� strumienia audio: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "Nie mo�na otworzy� strumienia z napisami: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "Nieudane otwarcie demuxera audio: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "Nieudane otwarcie demuxera napis�w: %s\n"
#define MSGTR_TVInputNotSeekable "Wej�cia TV nie mo�na przeszukiwa�! (Prawdopodobnie wyszukiwanie bedzie zmienia�o kana�y ;)\n"
#define MSGTR_DemuxerInfoAlreadyPresent "Demuxer info %s already present!\n"
#define MSGTR_ClipInfo "Informacja o klipie: \n"

#define MSGTR_LeaveTelecineMode "\ndemux_mpg: Wykryto progresywn� sekwencj�, wy��czam 3:2 TELECINE\n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: Wykryto 3:2 TELECINE, w��czam odwrotne telecine. Zmieniono FPS do %5.3f!  \n"


// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "Nie mog� otworzy� kodeka.\n"
#define MSGTR_CantCloseCodec "Nie mog� zamkn�� kodeka.\n"

#define MSGTR_MissingDLLcodec "B��D: Nie mog� otworzy� wymaganego kodeka DirectShow: %s.\n"
#define MSGTR_ACMiniterror "Nie mog� za�adowa�/zainicjalizowa� kodeka Win32/ACM AUDIO (brakuje pliku DLL?)\n"
#define MSGTR_MissingLAVCcodec "Nie moge znale�� w libavcodec kodeka '%s'...\n"
#define MSGTR_MpegNoSequHdr "MPEG: B��D: Koniec pliku podczas przeszukiwania nag��wka sekwencji.\n"
#define MSGTR_CannotReadMpegSequHdr "B��D: Nie mog� odczyta� nag��wka sekwencji.\n"
#define MSGTR_CannotReadMpegSequHdrEx "B��D: Nie mog� odczyta� rozszerzenia nag��wka sekwencji.\n"
#define MSGTR_BadMpegSequHdr "MPEG: Nieprawid�owy nag��wek sekwencji.\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: Nieprawid�owe rozszerzenie nag��wka sekwencji.\n"

#define MSGTR_ShMemAllocFail "Nie mog� zaalokowa� pami�ci dzielonej\n"
#define MSGTR_CantAllocAudioBuf "Nie mog� zaalokowa� bufora wyj�ciowego audio\n"

#define MSGTR_UnknownAudio "Nieznany/brakuj�cy format audio -> brak d�wi�ku\n"

#define MSGTR_UsingExternalPP "[PP] U�ywam zewn�trznego filtra postprocessingu, max q = %d.\n"
#define MSGTR_UsingCodecPP "[PP] U�ywam postprocessingu w kodeku, max q = %d.\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "Atrybut video '%s' nie jest obs�ugiwany przez wybrane vo & vd.\n"
#define MSGTR_VideoCodecFamilyNotAvailableStr "Wybrana rodzina kodek�w video [%s] (vfm=%d) niedost�pna (w��cz j� podczas kompilacji)\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "Wybrana rodzina kodek�w audio [%s] (afm=%d) niedost�pna (w��cz j� podczas kompilacji)\n"
#define MSGTR_OpeningVideoDecoder "Otwieram dekoder video: [%s] %s\n"
#define MSGTR_OpeningAudioDecoder "Otwieram dekoder audio: [%s] %s\n"
#define MSGTR_UninitVideoStr "uninit video: %s\n"
#define MSGTR_UninitAudioStr "uninit audio: %s\n"
#define MSGTR_VDecoderInitFailed "Nieudana inicjalizacja VDecodera :(\n"
#define MSGTR_ADecoderInitFailed "Nieudana inicjalizacja ADecodera :(\n"
#define MSGTR_ADecoderPreinitFailed "Nieudana preinicjalizacja ADecodera :(\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: Przydzielam %d bajt�w dla bufora wej�ciowego\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: Przydzielam %d + %d = %d bajt�w dla bufora wyj�ciowego\n"

// LIRC:
#define MSGTR_SettingUpLIRC "W��czam obs�ug� LIRC...\n"
#define MSGTR_LIRCdisabled "Nie b�dziesz m�g� u�ywa� swojego pilota.\n"
#define MSGTR_LIRCopenfailed "Nieudane otwarcie obs�ugi LIRC.\n"
#define MSGTR_LIRCcfgerr "Nieudane odczytanie pliku konfiguracyjnego LIRC %s.\n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "Nie mog� znale�� filtra video '%s'\n"
#define MSGTR_CouldNotOpenVideoFilter "Nie mog� otworzy� filtra video '%s'\n"
#define MSGTR_OpeningVideoFilter "Otwieram filtr video: "
#define MSGTR_CannotFindColorspace "Nie mog� znale�� wsp�lnej przestrzeni koloru, nawet przez wstawienie 'scale' :(\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDec: Kodek nie ustawia sh->disp_w i sh->disp_h, probuj� to rozwi�za�.\n"
#define MSGTR_VoConfigRequest "VDec: ��danie konfiguracji vo - %d x %d (preferowana przestrze� kolor�w: %s)\n"
#define MSGTR_CouldNotFindColorspace "Nie mog� znale�� pasuj�cej przestrzeni koloru - ponawiam pr�b� z -vf scale...\n"
#define MSGTR_MovieAspectIsSet "Proporcje filmu to %.2f:1 - skaluj� do prawid�owych propocji.\n"
#define MSGTR_MovieAspectUndefined "Proporcje filmu nie s� zdefiniowane - nie skaluj�.\n"

// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "O programie"
#define MSGTR_FileSelect "Wyb�r pliku..."
#define MSGTR_SubtitleSelect "Wyb�r napis�w..."
#define MSGTR_OtherSelect "Wyb�r..."
#define MSGTR_AudioFileSelect "Wyb�r zewn�trznego kana�u..."
#define MSGTR_FontSelect "Wyb�r czcionki..."
#define MSGTR_PlayList "Playlista"
#define MSGTR_Equalizer "Equalizer"
#define MSGTR_SkinBrowser "Przegl�darka Sk�rek"
#define MSGTR_Network "Strumie� sieciowy..."
#define MSGTR_Preferences "Preferencje"
#define MSGTR_OSSPreferences "Konfiguracja sterownika OSS"
#define MSGTR_SDLPreferences "Konfiguracja sterownika SDL"
#define MSGTR_NoMediaOpened "Nie otwarto no�nika."
#define MSGTR_VCDTrack "�cie�ka VCD: %d"
#define MSGTR_NoChapter "Brak rozdzia�u"
#define MSGTR_Chapter "Rozdzia� %d"
#define MSGTR_NoFileLoaded "Nie za�adowano pliku."

// --- buttons ---
#define MSGTR_Ok "OK"
#define MSGTR_Cancel "Anuluj"
#define MSGTR_Add "Dodaj"
#define MSGTR_Remove "Usu�"
#define MSGTR_Clear "Wyczy��"
#define MSGTR_Config "Konfiguracja"
#define MSGTR_ConfigDriver "Konfiguracja sterownika"
#define MSGTR_Browse "Przegl�daj"

// --- error messages ---
#define MSGTR_NEMDB "Przykro mi, za ma�o pami�ci na bufor rysowania."
#define MSGTR_NEMFMR "Przykro mi, za ma�o pami�ci na renderowanie menu."
#define MSGTR_IDFGCVD "Przykro mi, nie znalaz�em kompatybilnego z GUI sterownika video."
#define MSGTR_NEEDLAVCFAME "Przykro mi, nie mo�esz odtwarza� plik�w innych ni� MPEG za pomoc� twojego urz�dzenia DXR3/H+ bez przekodowania.\nProsz� w��cz lavc lub fame w konfiguracji DXR3/H+."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[skin] b��d w pliku konfiguracyjnym sk�rki w linii %d: %s"
#define MSGTR_SKIN_WARNING1 "[skin] ostrze�enie w pliku konfiguracyjnym sk�rki w linii %d: kontrolka znaleziona, ale poprzednia \"sekcja\" nie znaleziona (%s)"
#define MSGTR_SKIN_WARNING2 "[skin] ostrze�enie w pliku konfiguracyjnym sk�rki w linii %d: kontrolka znaleziona, ale poprzednia \"podsekcja\" nie znaleziona (%s)"
#define MSGTR_SKIN_WARNING3 "[skin] ostrze�enie w pliku konfiguracyjnym sk�rki w linii %d: podsekcja nie jest obs�ugiwana przez t� kontrolk� (%s)"
#define MSGTR_SKIN_BITMAP_16bit "Bitmapy 16 bitowe lub mniejsze nie obs�ugiwane (%s).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound "plik nie znaleziony (%s)\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "b��d odczytu BMP (%s)\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "b��d odczytu TGA (%s)\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "b��d odczytu PNG (%s)\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "TGA kompresowane RLE nie obs�ugiwane (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "nieznany typ pliku (%s)\n"
#define MSGTR_SKIN_BITMAP_ConvertError "b��d konwersji 24 bit�w na 32 bity (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "nieznany komunikat: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "za ma�o pami�ci\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "za du�o zadeklarowanych czcionek\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "nie znaleziono pliku z czcionk�\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "nie znaleziono pliku z obrazem czcionki\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "nie istniej�cy identyfikator czcionki (%s)\n"
#define MSGTR_SKIN_UnknownParameter "nieznany parametr (%s)\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[skinbrowser] za ma�o pami�ci.\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "Sk�rka nie znaleziona (%s).\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "B�ad odczytu pliku konfiguracyjnego sk�rki (%s).\n"
#define MSGTR_SKIN_LABEL "Sk�rki:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "O MPlayerze"
#define MSGTR_MENU_Open "Otw�rz..."
#define MSGTR_MENU_PlayFile "Odtwarzaj plik..."
#define MSGTR_MENU_PlayVCD "Odtwarzaj VCD..."
#define MSGTR_MENU_PlayDVD "Odtwarzaj DVD..."
#define MSGTR_MENU_PlayURL "Odtwarzaj URL..."
#define MSGTR_MENU_LoadSubtitle "Za�aduj napisy..."
#define MSGTR_MENU_DropSubtitle "Wy�aduj napisy..."
#define MSGTR_MENU_LoadExternAudioFile "Za�aduj zewn�trzny plik audio..."
#define MSGTR_MENU_Playing "Odtwarzanie"
#define MSGTR_MENU_Play "Odtwarzaj"
#define MSGTR_MENU_Pause "Pauza"
#define MSGTR_MENU_Stop "Stop"
#define MSGTR_MENU_NextStream "Nast�pny strumie�"
#define MSGTR_MENU_PrevStream "Poprzedni strumie�"
#define MSGTR_MENU_Size "Wielko��"
#define MSGTR_MENU_NormalSize "Normalna wielko��"
#define MSGTR_MENU_DoubleSize "Podw�jna wielko��"
#define MSGTR_MENU_FullScreen "Pe�en ekran"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "Odtwarzaj dysk..."
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
#define MSGTR_MENU_Exit "Wyj�cie..."
#define MSGTR_MENU_Mute "Mute"
#define MSGTR_MENU_Original "Oryginalne"
#define MSGTR_MENU_AspectRatio "Proporcje obrazu"
#define MSGTR_MENU_AudioTrack "�cie�ka Audio"
#define MSGTR_MENU_Track "�cie�ka %d"
#define MSGTR_MENU_VideoTrack "�cie�ka Video"

// --- equalizer
#define MSGTR_EQU_Audio "Audio"
#define MSGTR_EQU_Video "Video"
#define MSGTR_EQU_Contrast "Kontrast: "
#define MSGTR_EQU_Brightness "Jasno��: "
#define MSGTR_EQU_Hue "Hue: "
#define MSGTR_EQU_Saturation "Nasycenie: "
#define MSGTR_EQU_Front_Left "Lewy Przedni"
#define MSGTR_EQU_Front_Right "Prawy Przedni"
#define MSGTR_EQU_Back_Left "Lewy Tylny"
#define MSGTR_EQU_Back_Right "Prawy Tylny"
#define MSGTR_EQU_Center "Centralny"
#define MSGTR_EQU_Bass "Basowy"
#define MSGTR_EQU_All "Wszystkie"
#define MSGTR_EQU_Channel1 "Kana� 1:"
#define MSGTR_EQU_Channel2 "Kana� 2:"
#define MSGTR_EQU_Channel3 "Kana� 3:"
#define MSGTR_EQU_Channel4 "Kana� 4:"
#define MSGTR_EQU_Channel5 "Kana� 5:"
#define MSGTR_EQU_Channel6 "Kana� 6:"

// --- playlist
#define MSGTR_PLAYLIST_Path "�cie�ka"
#define MSGTR_PLAYLIST_Selected "Wybrane pliki"
#define MSGTR_PLAYLIST_Files "Pliki"
#define MSGTR_PLAYLIST_DirectoryTree "Drzewo katalog�w"

// --- preferences
#define MSGTR_PREFERENCES_Audio "D�wi�k"
#define MSGTR_PREFERENCES_Video "Video"
#define MSGTR_PREFERENCES_SubtitleOSD "Napisy i OSD"
#define MSGTR_PREFERENCES_Codecs "Kodeki i demuxery"
#define MSGTR_PREFERENCES_Misc "R�ne"

#define MSGTR_PREFERENCES_None "Puste"
#define MSGTR_PREFERENCES_AvailableDrivers "Dost�pne sterowniki:"
#define MSGTR_PREFERENCES_DoNotPlaySound "Nie odtwarzaj d�wi�ku"
#define MSGTR_PREFERENCES_NormalizeSound "Normalizuj d�wi�k"
#define MSGTR_PREFERENCES_EnEqualizer "W��cz equalizer"
#define MSGTR_PREFERENCES_ExtraStereo "W��cz extra stereo"
#define MSGTR_PREFERENCES_Coefficient "Wsp�czynnik:"
#define MSGTR_PREFERENCES_AudioDelay "Op�nienie d�wi�ku"
#define MSGTR_PREFERENCES_DoubleBuffer "W��cz podw�jne buforowanie"
#define MSGTR_PREFERENCES_DirectRender "W��cz rysowanie bezpo�rednie"
#define MSGTR_PREFERENCES_FrameDrop "W��cz zrzucanie ramek"
#define MSGTR_PREFERENCES_HFrameDrop "W��cz gwa�towne zrzucanie ramek (niebezpieczne)"
#define MSGTR_PREFERENCES_Flip "Odwr�� obraz do g�ry nogami"
#define MSGTR_PREFERENCES_Panscan "Panscan: "
#define MSGTR_PREFERENCES_OSDTimer "Timer i wska�niki"
#define MSGTR_PREFERENCES_OSDProgress "Tylko belki"
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "Timer, czas procentowy i ca�kowity"
#define MSGTR_PREFERENCES_Subtitle "Napisy:"
#define MSGTR_PREFERENCES_SUB_Delay "Op�nienie napis�w: "
#define MSGTR_PREFERENCES_SUB_FPS "FPS:"
#define MSGTR_PREFERENCES_SUB_POS "Pozycja: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "Wy��cz automatyczne �adowanie napis�w"
#define MSGTR_PREFERENCES_SUB_Unicode "Napisy w Unikodzie"
#define MSGTR_PREFERENCES_SUB_MPSUB "Konwertuj podane napisy do formatu napis�w Mplayera"
#define MSGTR_PREFERENCES_SUB_SRT "Konwertuj podane napisy do formatu SRT (time based SubViewer)"
#define MSGTR_PREFERENCES_SUB_Overlap "Prze��cz nak�adanie si� napis�w"
#define MSGTR_PREFERENCES_Font "Czcionka:"
#define MSGTR_PREFERENCES_FontFactor "Skala czcionki:"
#define MSGTR_PREFERENCES_PostProcess "W��cz postprocesing"
#define MSGTR_PREFERENCES_AutoQuality "Automatyczna jako��: "
#define MSGTR_PREFERENCES_NI "U�yj parsera dla non-interleaved AVI"
#define MSGTR_PREFERENCES_IDX "Przebuduj tablice indeks�w jesli to potrzebne"
#define MSGTR_PREFERENCES_VideoCodecFamily "Rodzina kodek�w video:"
#define MSGTR_PREFERENCES_AudioCodecFamily "Rodzina kodek�w audio:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "Poziom OSD"
#define MSGTR_PREFERENCES_FRAME_Subtitle "Napisy"
#define MSGTR_PREFERENCES_FRAME_Font "Czcionka"
#define MSGTR_PREFERENCES_FRAME_PostProcess "Postprocesing"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "Kodek i demuxer"
#define MSGTR_PREFERENCES_FRAME_Cache "Pami�� podr�czna"
#define MSGTR_PREFERENCES_FRAME_Misc "R�ne"
#define MSGTR_PREFERENCES_OSS_Device "Urz�dzenie:"
#define MSGTR_PREFERENCES_OSS_Mixer "Mikser:"
#define MSGTR_PREFERENCES_SDL_Driver "Sterownik:"
#define MSGTR_PREFERENCES_Message "Prosz� pami�ta�, �e niekt�re funkcje wymagaja restartowania odtwarzania!"
#define MSGTR_PREFERENCES_DXR3_VENC "Enkoder video:"
#define MSGTR_PREFERENCES_DXR3_LAVC "U�yj LAVC (FFmpeg)"
#define MSGTR_PREFERENCES_DXR3_FAME "U�yj FAME"
#define MSGTR_PREFERENCES_FontEncoding1 "Unikod"
#define MSGTR_PREFERENCES_FontEncoding2 "J�zyki zachodnioeuropejskie (ISO-8859-1)"
#define MSGTR_PREFERENCES_FontEncoding3 "J�zyki zachodnioeuropejskiez Euro (ISO-8859-15)"
#define MSGTR_PREFERENCES_FontEncoding4 "J�zyki s�owia�skie i �rodkowoeuropejskie (ISO-8859-2)"
#define MSGTR_PREFERENCES_FontEncoding5 "Esperanto, Galician, malta�ski, turecki (ISO-8859-3)"
#define MSGTR_PREFERENCES_FontEncoding6 "Stare znaki ba�tyckie (ISO-8859-4)"
#define MSGTR_PREFERENCES_FontEncoding7 "Cyrylica (ISO-8859-5)"
#define MSGTR_PREFERENCES_FontEncoding8 "Arabski (ISO-8859-6)"
#define MSGTR_PREFERENCES_FontEncoding9 "Wsp�czesna greka (ISO-8859-7)"
#define MSGTR_PREFERENCES_FontEncoding10 "Turecki (ISO-8859-9)"
#define MSGTR_PREFERENCES_FontEncoding11 "Ba�tycki (ISO-8859-13)"
#define MSGTR_PREFERENCES_FontEncoding12 "Celtycki (ISO-8859-14)"
#define MSGTR_PREFERENCES_FontEncoding13 "Znaki hebrajskie (ISO-8859-8)"
#define MSGTR_PREFERENCES_FontEncoding14 "Rosyjski (KOI8-R)"
#define MSGTR_PREFERENCES_FontEncoding15 "Ukrai�ski, bia�oruski (KOI8-U/RU)"
#define MSGTR_PREFERENCES_FontEncoding16 "Uproszczone znaki chi�skie (CP936)"
#define MSGTR_PREFERENCES_FontEncoding17 "Tradycyjne znaki chi�skie (BIG5)"
#define MSGTR_PREFERENCES_FontEncoding18 "Znaki japo�skie (SHIFT-JIS)"
#define MSGTR_PREFERENCES_FontEncoding19 "Znaki korea�skie (CP949)"
#define MSGTR_PREFERENCES_FontEncoding20 "Znaki tajskie (CP874)"
#define MSGTR_PREFERENCES_FontEncoding21 "Cyrylica Windows (CP1251)"
#define MSGTR_PREFERENCES_FontEncoding22 "J�zyki s�owia�skie i �rodkowoeuropejskie Windows (CP1250)"
#define MSGTR_PREFERENCES_FontNoAutoScale "Bez autoskalowania"
#define MSGTR_PREFERENCES_FontPropWidth "Proporcjonalnie do szeroko�ci filmu"
#define MSGTR_PREFERENCES_FontPropHeight "Proporcjonalnie do wysoko�ci filmu"
#define MSGTR_PREFERENCES_FontPropDiagonal "Proporcjonalnie do przek�tnej filmu"
#define MSGTR_PREFERENCES_FontEncoding "Kodowanie:"
#define MSGTR_PREFERENCES_FontBlur "Rozmazanie:"
#define MSGTR_PREFERENCES_FontOutLine "Obramowanie:"
#define MSGTR_PREFERENCES_FontTextScale "Skalowanie tekstu:"
#define MSGTR_PREFERENCES_FontOSDScale "Skalowanie OSD:"
#define MSGTR_PREFERENCES_Cache "Pami�� podr�czna"
#define MSGTR_PREFERENCES_CacheSize "Wielko�� pami�ci podr�cznej: "
#define MSGTR_PREFERENCES_LoadFullscreen "Rozpocznij na pe�nym ekranie"
#define MSGTR_PREFERENCES_SaveWinPos "Zapisz po�o�enie okna"
#define MSGTR_PREFERENCES_XSCREENSAVER "Zatrzymaj wygaszacz ekranu"
#define MSGTR_PREFERENCES_PlayBar "W��cz podr�czny pasek"
#define MSGTR_PREFERENCES_AutoSync "Autosynchronizacja w��cz/wy��cz"
#define MSGTR_PREFERENCES_AutoSyncValue "Autosynchronizacja: "
#define MSGTR_PREFERENCES_CDROMDevice "Urz�dzenie CD-ROM:"
#define MSGTR_PREFERENCES_DVDDevice "Urz�dzenie DVD:"
#define MSGTR_PREFERENCES_FPS "FPS:"
#define MSGTR_PREFERENCES_ShowVideoWindow "Poka� okno video gdy nieaktywne"

#define MSGTR_ABOUT_UHU "Rozwijanie GUI sponsorowane przez UHU Linux\n"
#define MSGTR_ABOUT_CoreTeam "   G��wni cz�onkowie zespo�u MPlayera:\n"
#define MSGTR_ABOUT_AdditionalCoders "   Dodatkowki koderzy:\n"
#define MSGTR_ABOUT_MainTesters "   G��wni testerzy:\n"
				  
// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "B��d krytyczny!"
#define MSGTR_MSGBOX_LABEL_Error "B��d!"
#define MSGTR_MSGBOX_LABEL_Warning "Ostrze�enie!" 

#endif

// Translated by:  Kuba "Qba" Misiorny <jim85@wp.pl>
// Wszelkie uwagi i poprawki mile widziane :)
//
// Synced with help_mp-en.h 1.123

// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"U�ycie:   mplayer [opcje] [url|�cie�ka/]nazwa_pliku\n"
"\n"
"Podstawowe opcje: (Pe�na lista w man)\n"
" -vo <drv[:dev]>  wybierz wyj�ciowy sterownik video [:urz�dzenie (device)] (lista: '-vo help')\n"
" -ao <drv[:dev]>  wybierz wyj�ciowy sterownik audio [:urz�dzenie (device)] (lista: '-ao help')\n"
#ifdef HAVE_VCD
" vcd://<numer_�cie�ki>  odtw�rz �cie�k� (S)VCD (Super Video CD) (bezpo�rednio z nap�du, bez montowania)\n"
#endif
#ifdef USE_DVDREAD
" dvd://<tytu�>    odtw�rz tytu� bezpo�rednio z p�yty DVD \n"
" -alang/-slang    wybierz j�zyk d�wi�ku/napis�w (dwuznakowy kod kraju)\n"
#endif
" -ss <pozycja>    skok do pozycji (sekundy lub hh:mm:ss)\n"
" -nosound         nie odtwarzaj d�wi�ku\n"
" -fs              odtwarzaj na pe�nym ekranie (-vm, -zoom, szczeg�y w man)\n"
" -x <x> -y <y>    ustaw rozmiar obrazu wyj�ciowego (u�ywaj z -vm, -zoom)\n"
" -sub <plik>      wybierz plik z napisami (patrz tak�e -subfps, -subdelay)\n"
" -playlist <plik> wybierz playlist� \n"
" -vid x -aid y    wybierz strumie� video (x) lub audio (y)\n"
" -fps x -srate y  zmie� pr�dko�� odtwarzania video (x fps) i audio (y Hz) \n"
" -pp <jako��>     w��cz filtr postprocessing (szczeg�y w man)\n"
" -framedrop       w��cz gubienie ramek (dla wolnych maszyn)\n"
"\n"
"Podstawowe klawisze: (Pe�na lista w man, sprawd� te� input.conf)\n"
" <-   lub  ->      skok w ty�/prz�d o 10 sekund\n"
" g�ra lub d�      skok w ty�/prz�d o 1 minut�\n"
" pgup lub pgdown   skok w ty�/prz�d o 10 minut\n"
" < lub >           poprzednia/nast�pna pozycja w playli�cie\n"
" p lub SPACE       pauza (dowolny klawisz aby kontynuowa�)\n"
" q lub ESC         wyj�cie\n"
" + lub -           zmie� op�nienie d�wi�ku o +/- 0.1 sekundy\n"
" o                 tryb OSD (On Screen Display): brak / belka / belka + timer\n"
" * lub /           zwi�ksz/zmniejsz g�o�no�� (PCM)\n"
" z lub x           zmie� op�nienie napis�w o +/- 0.1 sekundy\n"
" r lub t           zmie� po�o�enie napis�w wy�ej/ni�ej, spr�buj te� -vf expand\n"
"\n"
" * * * DOK�ADNY SPIS WSZYSTKICH OPCJI ZNAJDUJE SI� W MAN * * *\n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// MPlayer.c:

#define MSGTR_Exiting "\nWychodz�...\n"
#define MSGTR_ExitingHow "\nWychodz�...(%s)\n"
#define MSGTR_Exit_quit "Wyj�cie"
#define MSGTR_Exit_eof "Koniec pliku"
#define MSGTR_Exit_error "Krytyczny b��d"
#define MSGTR_IntBySignal "\nMPlayer przerwany sygna�em %d w module: %s\n"
#define MSGTR_NoHomeDir "Nie mog� znale�� katalogu domowego\n"
#define MSGTR_GetpathProblem "Problem z get_path (\"config\")\n"
#define MSGTR_CreatingCfgFile "Tworz� plik konfiguracyjny: %s\n"
#define MSGTR_InvalidVOdriver "Nieprawid�owa nazwa wyj�ciowego sterownika video -> %s\n(lista: '-vo help').\n"
#define MSGTR_InvalidAOdriver "Nieprawid�owa nazwa wyj�ciowego sterownika audio -> %s\n(lista: '-ao help').\n"
#define MSGTR_CopyCodecsConf "(Skopiuj etc/codecs.conf ze �r�de� MPlayera do ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "U�ywam wbudowanego (domy�lnego) pliku codecs.conf.\n"
#define MSGTR_CantLoadFont "Nie mog� za�adowa� czcionki: %s\n"
#define MSGTR_CantLoadSub "Nie mog� za�adowa� napis�w: %s\n"
#define MSGTR_ErrorDVDkey "B��d w przetwarzaniu klucza DVD (DVD key)\n"
#define MSGTR_CmdlineDVDkey "��dany klucz DVD jest u�ywany do dekodowania\n"
#define MSGTR_DVDauthOk "Sekwencja autoryzacji DVD wygl�da OK.\n"
#define MSGTR_DumpSelectedStreamMissing "dump: FATAL: Nie ma wybranego strumienia\n"
#define MSGTR_CantOpenDumpfile "Nie mog� otworzy� pliku dump.\n"
#define MSGTR_CoreDumped "Core dumped (Zrzut pami�ci)\n"
#define MSGTR_FPSnotspecified "FPS nie podane (lub b��dne) w nag��wku, u�yj opcji -fps <ilo��_ramek_na_sekund�>.\n"
#define MSGTR_TryForceAudioFmtStr "Wymuszam zastosowanie kodeka audio z rodziny %s...\n"
#define MSGTR_CantFindAfmtFallback "Nie mog� znale�� kodeka audio dla wymuszonej rodziny, pr�buj� innych...\n"
#define MSGTR_CantFindAudioCodec "Nie mog� znale�� kodeka dla formatu audio 0x%X.\n"
#define MSGTR_RTFMCodecs "Przeczytaj DOCS/HTML/pl/codecs.html!\n"
#define MSGTR_CouldntInitAudioCodec "Nie mog� zainicjalizowa� kodeka audio -> brak d�wi�ku\n"
#define MSGTR_TryForceVideoFmtStr "Wymuszam zastosowanie kodeka video z rodziny %s...\n"
#define MSGTR_CantFindVideoCodec "Nie mog� znale�� kodeka pasuj�cego do wybranego -vo i formatu video 0x%X.\n"
#define MSGTR_VOincompCodec "Wybrane urz�dzenie wyj�cia video(video_out) nie jest kompatybilne z tym kodekiem\n"
#define MSGTR_CannotInitVO "FATAL: Nie mog� zainicjalizowa� sterownika video.\n"
#define MSGTR_CannotInitAO "Nie mog� otworzy�/zainicjalizowa� urz�dzenia audio -> brak d�wi�ku.\n"
#define MSGTR_StartPlaying "Zaczynam odtwarzanie... \n"

#define MSGTR_SystemTooSlow "\n\n"\
"           ************************************************\n"\
"           ********* Tw�j system jest ZA WOLNY!!! ********\n"\
"           ************************************************\n\n"\
"Prawdopodobne problemy, rozwi�zania:\n"\
"- Najbardziej powszechne: wadliwe/b��dne _sterowniki_audio_\n"\
"  - Spr�buj u�y� -ao sdl, u�yj ALSA 0.5 lub emulacji OSS w ALSA 0.9\n"\
"  - Poeksperymentuj z r�nymi warto�ciami -autosync, \"30\" to dobry pocz�tek.\n"\
"- Za wolny sterownik wyj�ciowy:\n"\
"  - Spr�buj innego sterownika -vo (lista: -vo help) albo -framedrop!\n"\
"- Za wolny procesor\n"\
"  - Nie pr�buj odtwarza� du�ych DVD/DivX�w na wolnym procesorze! Spr�buj -hardframedrop.\n"\
"- Zepsuty plik\n"\
"  - Spr�buj r�nych kombinacji -nobps, -ni, forceidx, -mc 0.\n"\
"- Za wolne �r�d�o (zamontowane NFS/SMB, DVD, VCD itd.)\n"\
"  - Spr�buj: -cache 8192.\n"\
"- Czy u�ywasz cache'u do odtwarzania plik�w bez przeplotu? Spr�buj -nocache\n"\
"Przeczytaj DOCS/HTML/pl/devices.html gdzie znajdziesz wskaz�wki\n"\
"jak przy�pieszy� dzia�anie MPlayera\n"\
"Je�li nic nie pomaga przeczytaj DOCS/HTML/pl/bugreports.html.\n\n"

#define MSGTR_NoGui "MPlayer zosta� skompilowany BEZ obs�ugi GUI.\n"
#define MSGTR_GuiNeedsX "GUI MPlayera potrzebuje X11.\n"
#define MSGTR_Playing "Odtwarzam %s.\n"
#define MSGTR_NoSound "Audio: brak d�wi�ku\n"
#define MSGTR_FPSforced "FPS wymuszone na %5.3f  (ftime: %5.3f).\n"
#define MSGTR_CompiledWithRuntimeDetection "Skompilowany z wykrywaniem procesora podczas pracy - UWAGA - W ten spos�b nie uzyskasz\n najlepszej wydajno�ci, przekompiluj MPlayera z opcj� --disable-runtime-cpudetection.\n"
#define MSGTR_CompiledWithCPUExtensions "Skompilowany dla procesora z rozszerzeniami:"
#define MSGTR_AvailableVideoOutputPlugins "Dost�pne wtyczki video:\n"
#define MSGTR_AvailableVideoOutputDrivers "Dost�pne sterowniki video:\n"
#define MSGTR_AvailableAudioOutputDrivers "Dost�pne sterowniki audio:\n"
#define MSGTR_AvailableAudioCodecs "Dost�pne kodeki audio:\n"
#define MSGTR_AvailableVideoCodecs "Dost�pne kodeki video:\n"
#define MSGTR_AvailableAudioFm "\nDost�pne (wkompilowane) rodziny kodek�w/sterownik�w audio:\n"
#define MSGTR_AvailableVideoFm "\nDost�pne (wkompilowane) rodziny kodek�w/sterownik�w video:\n"
#define MSGTR_AvailableFsType "Dost�pne tryby pe�noekranowe:\n"
#define MSGTR_UsingRTCTiming "U�ywam sprz�towego zegara czasu rzeczywistego (Linux RTC) (%ldHz).\n"
#define MSGTR_CannotReadVideoProperties "Video: nie mog� odczyta� w�a�ciwo�ci.\n"
#define MSGTR_NoStreamFound "Nie znalaz�em �adnego strumienia\n"
#define MSGTR_InitializingAudioCodec "Inicjalizuj� kodek audio...\n"
#define MSGTR_ErrorInitializingVODevice "B��d przy otwieraniu/inicjalizacji wybranego urz�dzenia video (-vo).\n"
#define MSGTR_ForcedVideoCodec "Wymuszony kodek video: %s\n"
#define MSGTR_ForcedAudioCodec "Wymuszony kodek audio: %s\n"
#define MSGTR_AODescription_AOAuthor "AO: Opis: %s\nAO: Autor: %s\n"
#define MSGTR_AOComment "AO: Kometarz: %s\n"
#define MSGTR_Video_NoVideo "Video: brak video\n"
#define MSGTR_NotInitializeVOPorVO "\nFATAL: Nie mog� zainicjalizowa� filtra video (-vf) lub wyj�cia video (-vo).\n"
#define MSGTR_Paused "\n  =====  PAUZA  =====\r" // no more than 23 characters (status line for audio files)
#define MSGTR_PlaylistLoadUnable "\nNie mog� za�adowa� playlisty %s.\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- MPlayer zako�czy� prac� z powodu b��du 'Nieprawid�owa operacja'\n"\
"  Mo�e to by� b��d w naszym nowym kodzie wykrywaj�cym procesor\n"\
"  Przeczytaj prosz� DOCS/HTML/pl/bugreports.html.\n"
#define MSGTR_Exit_SIGILL \
"- MPlayer zako�czy� prac� z powodu b��du  'Nieprawid�owa operacja'\n"\
"  Zdarza si� to najcz�ciej gdy uruchamiasz MPlayera na innym procesorze ni� ten\n"\
"  dla kt�rego by� on skompilowany/zoptymalizowany.\n"\
"  Sprawd� to!\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- MPlayer nieoczekiwanie zako�czy� prac� przez z�y u�ytek procesora/pami�ci/kooprocesora\n"\
"  Przekompiluj MPlayera z opcj� '--enable-debug' i wykonaj 'gdb' backtrace \n"\
"  i zdeassembluj. Szczeg�y w DOCS/HTML/pl/bugreports_what.html#bugreports_crash.\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer nieoczekiwanie zako�czy� prac�. To nie powinno si� zdarzy�! :).\n"\
"  Mo�e to by� b��d w kodzie MPlayera albo w Twoim sterowniku,\n"\
"  lub z�ej wersji gcc. Je�li uwa�asz, �e to wina MPlayera, przeczytaj\n"\
"  DOCS/HTML/pl/bugreports.html. Nie mo�emy i nie pomo�emy je�li nie przedstawisz tych informacji\n"\
"  zg�aszaj�c prawdopodobny b��d.\n"


// mencoder.c:

#define MSGTR_UsingPass3ControllFile "U�ywam pliku kontrolnego pass3: %s\n"
#define MSGTR_MissingFilename "\nBrak nazwy pliku.\n\n"
#define MSGTR_CannotOpenFile_Device "Nie mog� otworzy� pliku/urz�dzenia\n"
#define MSGTR_ErrorDVDAuth "B��d przy autoryzacji DVD.\n"
#define MSGTR_CannotOpenDemuxer "Nie mog� otworzy� demuxera.\n"
#define MSGTR_NoAudioEncoderSelected "\nNie wybrano enkodera audio (-oac). Wybierz jaki� (Lista: -oac help) albo u�yj opcji '-nosound' \n"
#define MSGTR_NoVideoEncoderSelected "\nNie wybrano enkodera video (-ovc). Wybierz jaki� (Lista: -ovc help)\n"
#define MSGTR_InitializingAudioCodec "Inicjalizuj� kodek audio...\n"
#define MSGTR_CannotOpenOutputFile "Nie mog� otworzy� pliku wyj�ciowego '%s'.\n"
#define MSGTR_EncoderOpenFailed "Nie mog� otworzy� enkodera.\n"
#define MSGTR_ForcingOutputFourcc "Wymuszam wyj�ciowe fourcc na %x [%.4s]\n"
#define MSGTR_WritingAVIHeader "Zapisuj� nag��wek AVI...\n"
#define MSGTR_DuplicateFrames "\n%d powt�rzona(e) ramka(i)!\n"
#define MSGTR_SkipFrame "\nOpuszczam ramk�!"
#define MSGTR_ErrorWritingFile "%s B��d przy zapisie pliku.\n"
#define MSGTR_WritingAVIIndex "\nZapisuj� indeks AVI...\n"
#define MSGTR_FixupAVIHeader "Naprawiam nag��wek AVI...\n"
#define MSGTR_RecommendedVideoBitrate "Zalecany video bitrate dla tego %s CD: %d\n"
#define MSGTR_VideoStreamResult "\nStrumie� video: %8.3f kbit/s (%d bps) rozmiar: %d bajt�w %5.3f s %d ramek\n"
#define MSGTR_AudioStreamResult "\nStrumie� audio: %8.3f kbit/s (%d bps) rozmiar: %d bajt�w %5.3f s\n"

// cfg-mencoder.h:

#define MSGTR_MEncoderMP3LameHelp "\n\n"\
" vbr=<0-4>     metoda zmiennego bitrate\n"\
"                0: cbr\n"\
"                1: mt\n"\
"                2: rh(default)\n"\
"                3: abr\n"\
"                4: mtrh\n"\
"\n"\
" abr           �redni bitrate\n"\
"\n"\
" cbr           sta�y bitrate\n"\
"               Wymusza tak�e kodowanie CBR (sta�y bitrate) w nast�puj�cych po tej opcji ustawieniach ABR\n"\
"\n"\
" br=<0-1024>   podaj bitrate w kBit (tylko CBR i ABR)\n"\
"\n"\
" q=<0-9>       jako�� (0-najwy�sza, 9-najni�sza) (tylko VBR)\n"\
"\n"\
" aq=<0-9>      jako�� algorytmu (0-najlepsza/najwolniejsze, 9-najgorsza/najszybsze)\n"\
"\n"\
" ratio=<1-100> wsp�czynnik kompresji\n"\
"\n"\
" vol=<0-10>    ustaw wzmocnienie sygna�u audio\n"\
"\n"\
" mode=<0-3>    (domy�lnie: auto)\n"\
"                0: stereo\n"\
"                1: joint-stereo\n"\
"                2: dualchannel\n"\
"                3: mono\n"\
"\n"\
" padding=<0-2>\n"\
"                0: nie\n"\
"                1: wszystkie\n"\
"                2: ustaw\n"\
"\n"\
" fast          Prze��cz na szybsze kodowanie, na poni�szych ustawieniach VBR,\n"\
"		nieznacznie ni�sza jako�� i wy�szy bitrate.\n"\
"\n"\
" preset=<value>  W��cza najwy�sz� mo�liw� jako��.\n"\
"                 medium: kodowanie VBR, dobra jako��\n"\
"                 (bitrate: 150-180 kb/s)\n"\
"                 standard:  kodowanie VBR, wysoka jako��\n"\
"                 (bitrate: 170-210 kb/s)\n"\
"                 extreme: kodowanie VBR, bardzo wysoka jako��\n"\
"                 (bitrate: 200-240 kb/s)\n"\
"                 insane:  kodowanie CBR, najwy�sza jako��\n"\
"                 (bitrate: 320 kb/s)\n"\
"                 <8-320>: kodowanie ABR przy podanym �rednim bitrate.\n\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "Nie znaleziono urz�dzenia CD-ROM '%s'\n"
#define MSGTR_ErrTrackSelect "B��d przy wybieraniu �cie�ki VCD."
#define MSGTR_ReadSTDIN "Czytam ze stdin (standardowego wej�cia)...\n"
#define MSGTR_UnableOpenURL "Nie mog� otworzy� URL: %s\n"
#define MSGTR_ConnToServer "Po��czy�em si� z serwerem: %s\n"
#define MSGTR_FileNotFound "Nie znaleziono pliku '%s'\n"

#define MSGTR_SMBInitError "Nie mog� zainicjalizowa� biblioteki libsmbclient: %d\n"
#define MSGTR_SMBFileNotFound "Nie mog� odczyta� z LANu: '%s'\n"
#define MSGTR_SMBNotCompiled "MPlayer nie zosta� skompilowany z obs�ug� SMB.\n"

#define MSGTR_CantOpenDVD "Nie mog� otworzy� DVD: %s\n"
#define MSGTR_DVDwait "Odczytuj� struktur� dysku, prosz� czeka�...\n"
#define MSGTR_DVDnumTitles "Na tym DVD jest %d tytu��w.\n"
#define MSGTR_DVDinvalidTitle "Nieprawid�owy numer tytu�u: %d\n"
#define MSGTR_DVDnumChapters "W tym tytule DVD jest %d rozdzia��w.\n"
#define MSGTR_DVDinvalidChapter "Nieprawid�owy numer rozdzia�u (DVD): %d\n"
#define MSGTR_DVDnumAngles "W tym tytule DVD znajduje si� %d ustawie� (k�t�w) kamery.\n"
#define MSGTR_DVDinvalidAngle "Nieprawid�owy numer ustawienia kamery: %d\n"
#define MSGTR_DVDnoIFO "Nie mog� otworzy� pliku IFO dla tytu�u DVD %d.\n"
#define MSGTR_DVDnoVOBs "Nie mog� otworzy� tytu�u VOBS (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD otwarte prawid�owo.\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "UWAGA: Redefiniowano nag��wek strumienia audio %d!\n"
#define MSGTR_VideoStreamRedefined "UWAGA: Redefiniowano nag��wek strumienia video %d!\n"
#define MSGTR_TooManyAudioInBuffer "\nZa du�o pakiet�w audio w buforze (%d w %d bajtach)\n"
#define MSGTR_TooManyVideoInBuffer "\nZa du�o pakiet�w video w buforze (%d w %d bajtach)\n"

#define MSGTR_MaybeNI "Mo�e odtwarzasz plik/strumie� bez przeplotu (non-interleaved) albo kodek nie zadzia�a� (failed)?\n"\
		      "Dla plik�w AVI spr�buj wymusi� tryb bez przeplotu z opcj� '-ni'\n"
#define MSGTR_SwitchToNi "\nWykryto zbi�r AVI z b��dnym przeplotem - prze��czam w tryb -ni\n"
#define MSGTR_Detected_XXX_FileFormat "Wykryto format %s.\n"
#define MSGTR_DetectedAudiofile "Wykryto plik audio.\n"
#define MSGTR_NotSystemStream "Nie jest to format MPEG System Stream... (mo�e Transport Stream?)\n"
#define MSGTR_InvalidMPEGES "Nieprawid�owy strumie� MPEG-ES??? Skontaktuj si� z autorem to mo�e by� b��d :(\n"
#define MSGTR_FormatNotRecognized "============ Przykro mi, nierozpoznany/nieobs�ugiwany format pliku =============\n"\
				  "=== Je�li jest to strumie� AVI, ASF albo MPEG skontaktuj si� z autorem! ===\n"
#define MSGTR_MissingVideoStream "Nie znaleziono strumienia video.\n"
#define MSGTR_MissingAudioStream "Nie znaleziono strumienia audio -> brak d�wi�ku.\n"
#define MSGTR_MissingVideoStreamBug "Brakuje strumienia video!? Skontaktuj si� z autorem, to mo�e by� b��d:(\n"

#define MSGTR_DoesntContainSelectedStream "demux: Plik nie zawiera wybranego strumienia audio i video.\n"

#define MSGTR_NI_Forced "Wymuszony"
#define MSGTR_NI_Detected "Wykryty"
#define MSGTR_NI_Message "%s plik formatu NON-INTERLEAVED AVI (bez przeplotu).\n"

#define MSGTR_UsingNINI "U�ywam uszkodzonego formatu pliku NON-INTERLAVED AVI.\n"
#define MSGTR_CouldntDetFNo "Nie mog� okre�li� liczby ramek (dla przeszukiwania bezwzgl�dnego).\n"
#define MSGTR_CantSeekRawAVI "Nie mog� przeszukiwa� nieindeksowanych strumieni AVI (Index jest wymagany, spr�buj z opcja '-idx')\n"
#define MSGTR_CantSeekFile "Nie mog� przeszukiwa� tego pliku\n"

#define MSGTR_EncryptedVOB "Zaszyfrowany plik VOB! Przeczytaj DOCS/HTML/pl/dvd.html.\n"

#define MSGTR_MOVcomprhdr "MOV: Obs�uga skompresowanych nag��wk�w wymaga ZLIB!\n"
#define MSGTR_MOVvariableFourCC "MOV: UWAGA: Zmienna FOURCC wykryta!?\n"
#define MSGTR_MOVtooManyTrk "MOV: UWAGA: za du�o �cie�ek"
#define MSGTR_FoundAudioStream "==> Wykryto strumie� audio: %d\n"
#define MSGTR_FoundVideoStream "==> Wykryto strumie� video: %d\n"
#define MSGTR_DetectedTV "Wykryto TV! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "Nie mog� otworzy� demuxera ogg.\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: Szukam strumienia audio (id:%d).\n"
#define MSGTR_CannotOpenAudioStream "Nie mog� otworzy� strumienia audio %s\n"
#define MSGTR_CannotOpenSubtitlesStream "Nie mog� otworzy� strumienia z napisami: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "Nie mog� otworzy� demuxera audio: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "Nie mog� otworzy� demuxera napis�w: %s\n"
#define MSGTR_TVInputNotSeekable "Wej�cia TV nie mo�na przeszukiwa� (Przeszukiwanie b�dzie s�u�y�o do zmiany kana��w ;)\n"
#define MSGTR_DemuxerInfoAlreadyPresent "Informacje %s o demuxerze s� ju� obecne!\n"
#define MSGTR_ClipInfo "Informacje klipu:\n" // Clip info:\n"

#define MSGTR_LeaveTelecineMode "\ndemux_mpg: Wykryto zawarto�� 30fps NTSC, zmieniam liczb� ramek na sekund�.\n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: Wykryto progresywn� zawarto�� 24fps NTSC, zmieniam liczb� ramek na sekund�"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "Nie mog� otworzy� kodeka.\n"
#define MSGTR_CantCloseCodec "Nie mog� zamkn�� kodeka.\n"

#define MSGTR_MissingDLLcodec "B��d: Nie mog� otworzy� wymaganego kodeka DirectShow %s.\n"
#define MSGTR_ACMiniterror "Nie mog� za�adowa�/zainicjalizowa� kodeka Win32/ACM AUDIO (Brakuje pliku DLL?).\n"
#define MSGTR_MissingLAVCcodec "Nie mog� znale�� kodeka '%s' w libavcodec...\n"

#define MSGTR_MpegNoSequHdr "MPEG: FATAL: EOF (koniec pliku) podczas szukania nag��wka sekwencji.\n"
#define MSGTR_CannotReadMpegSequHdr "FATAL: Nie mog� odczyta� nag��wka sekwencji.\n"
#define MSGTR_CannotReadMpegSequHdrEx "FATAL: Nie mog� odczyta� rozszerzenia nag��wka sekwencji.\n"
#define MSGTR_BadMpegSequHdr "MPEG: Nieprawid�owy nag��wek sekwencji\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: Nieprawid�owe rozszerzenie nag��wka sekwencji\n"

#define MSGTR_ShMemAllocFail "Nie mog� zaalokowa� pami�ci dzielonej.\n"
#define MSGTR_CantAllocAudioBuf "Nie mog� zaalokowa� bufora wyj�ciowego audio.\n"

#define MSGTR_UnknownAudio "Nieznany/brakuj�cy format audio -> brak d�wi�ku\n"

#define MSGTR_UsingExternalPP "[PP] U�ywam zewn�trznego filtra postprocessingu, max q = %d.\n"
#define MSGTR_UsingCodecPP "[PP] U�ywam postprocessingu kodeka, max q = %d.\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "Atrybut video '%s' nie jest obs�ugiwany przez wybrane vo & vd.\n"
#define MSGTR_VideoCodecFamilyNotAvailableStr "Wybrana rodzina kodek�w video [%s](vfm=%s) jest niedost�pna.\nW��cz j� przy kompilacji.\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "Wybrana rodzina kodek�w audio [%s](vfm=%s) jest niedost�pna.\nW��cz j� przy kompilacji.\n"
#define MSGTR_OpeningVideoDecoder "Otwieram dekoder video: [%s] %s\n"
#define MSGTR_OpeningAudioDecoder "Otwieram dekoder audio: [%s] %s\n"
#define MSGTR_UninitVideoStr "deinicjalizacja video: %s\n"
#define MSGTR_UninitAudioStr "deinicjalizacja audio: %s\n"
#define MSGTR_VDecoderInitFailed "Inicjalizacja VDecodera nie powiod�a si� :(\n"
#define MSGTR_ADecoderInitFailed "Inicjalizacja ADecodera nie powiod�a si� :(\n"
#define MSGTR_ADecoderPreinitFailed "Nieudana preinicjalizacja ADecodera :(\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: Alokuj� %d bajt�w dla bufora wej�ciowego.\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: Alokuj� %d + %d = %d bajt�w dla bufora wyj�ciowego.\n"

// LIRC:
#define MSGTR_SettingUpLIRC "W��czam obs�ug� LIRC...\n"
#define MSGTR_LIRCdisabled "Nie b�dziesz m�g� u�ywa� swojego pilota.\n"
#define MSGTR_LIRCopenfailed "Nie mog� uruchomi� obs�ugi LIRC.\n"
#define MSGTR_LIRCcfgerr "Nie mog� odczyta� pliku konfiguracyjnego LIRC %s.\n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "Nie mog� znale�� filtra video '%s'.\n"
#define MSGTR_CouldNotOpenVideoFilter "Nie mog� otworzy� filtra video '%s'.\n"
#define MSGTR_OpeningVideoFilter "Otwieram filtr video: "
#define MSGTR_CannotFindColorspace "Nie mog� znale�� odpowiedniej przestrzenii kolor�w (colorspace), nawet poprzez wstawienie 'scale' :(\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDec: Kodek nie ustawi� sh->disp_w i sh->disp_h, pr�buj� to rozwi�za�.\n"
#define MSGTR_VoConfigRequest "VDec: wymagana konfiguracja vo - %d x %d (preferowana csp: %s)\n"
#define MSGTR_CouldNotFindColorspace "Nie mog� znale�� pasuj�cej przestrzeni koloru - pr�buj� ponownie z -vf scale...\n"
#define MSGTR_MovieAspectIsSet "Proporcje filmu (obrazu) to %.2f:1 - skaluj� do prawid�owych proporcji.\n"
#define MSGTR_MovieAspectUndefined "Proporcje filmu (obrazu) nie s� zdefiniowane - nie skaluj�.\n"

// vd_dshow.c, vd_dmo.c
#define MSGTR_DownloadCodecPackage "Musisz zainstalowa�/zaktualizowa� pakiet binarnych kodek�w.\nId� do http://mplayerhq.hu/homepage/dload.html\n"
#define MSGTR_DShowInitOK "INFORMACJA: Inicjalizacja kodeka video Win32/DShow przebieg�a pomy�lnie.\n"
#define MSGTR_DMOInitOK "INFORMACJA: Inicjalizacja kodeka video Win32/DMO przebieg�a pomy�lnie.\n"

// x11_common.c
#define MSGTR_EwmhFullscreenStateFailed "\nX11: Nie mog�em wys�a� zdarzenia pe�nego ekranu EWMH!\n"

#define MSGTR_InsertingAfVolume "[Mixer] Nie ma sprz�towego miksowania, w��czam filtr g�o�no�ci.\n"
#define MSGTR_NoVolume "[Mixer] Regulacja g�o�no�ci niedost�pna.\n"


// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "O programie"
#define MSGTR_FileSelect "Wybierz plik..."
#define MSGTR_SubtitleSelect "Wybierz napisy..."
#define MSGTR_OtherSelect "Wybierz..."
#define MSGTR_AudioFileSelect "Wybierz zewn�trzny kana� audio..."
#define MSGTR_FontSelect "Wybierz czcionk�..."
#define MSGTR_PlayList "Playlista"
#define MSGTR_Equalizer "Equalizer (korektor)"
#define MSGTR_SkinBrowser "Przegl�darka sk�rek"
#define MSGTR_Network "Strumieniowanie sieciowe..."
#define MSGTR_Preferences "Preferencje"
#define MSGTR_AudioPreferences "Konfiguracja sterownika audio"
#define MSGTR_NoMediaOpened "Nie otwarto �adnego no�nika."
#define MSGTR_VCDTrack "�cie�ka VCD: %d"
#define MSGTR_NoChapter "Brak rozdzia�u"
#define MSGTR_Chapter "Rozdzia� %d"
#define MSGTR_NoFileLoaded "Nie za�adowano �adnego pliku."

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
#define MSGTR_NEMFMR "Przykro mi, za ma�o pami�ci do wyrenderowania menu."
#define MSGTR_IDFGCVD "Przykro mi, nie znalaz�em kompatybilnego z GUI sterownika video."
#define MSGTR_NEEDLAVCFAME "Przykro mi, nie mo�esz odtwarza� plik�w innych ni� MPEG za pomoc� urz�dzenia (device) DXR3/H+ bez przekodowania.\nW��cz lavc albo fame w konfiguracji DXR3/H+"

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[skin] b��d w pliku konfiguracyjnym sk�rki, w linii %d: %s"
#define MSGTR_SKIN_WARNING1 "[skin] ostrze�enie w pliku konfiguracyjnym w linii %d:\nznaleziono znacznik widget (%s) ale nie ma przed nim \"section\""
#define MSGTR_SKIN_WARNING2 "[skin] ostrze�enie w pliku konfiguracyjnym w linii %d:\nznaleziono znacznik widget (%s) ale nie ma przednim found \"subsection\""
#define MSGTR_SKIN_WARNING3 "[skin] ostrze�enie w pliku konfiguracyjnym w linii %d::\nta podsekcja nie jest obs�ugiwana this subsection przed widget (%s)"
#define MSGTR_SKIN_BITMAP_16bit  "Bitmapy 16 bitowe lub mniejsze nie s� obs�ugiwane (%s).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "nie znaleziono pliku (%s)\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "b��d odczytu bmp (%s)\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "b��d odczytu tga (%s)\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "b��d odczytu png (%s)\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "tga kompresowane przez RLE nie obs�ugiwane (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "nieznany typ pliku (%s)\n"
#define MSGTR_SKIN_BITMAP_ConvertError "b��d przy konwersji 24 bit�w  na 32 bity (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "nieznany komunikat: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "za ma�o pami�ci\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "Za du�o zadeklarowanych czcionek\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "Nie znaleziono pliku z czcionk�\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "Nie znaleziono pliku z obrazem czcionki\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "nieistniej�cy identyfikator czcionki (%s)\n"
#define MSGTR_SKIN_UnknownParameter "nieznany parametr (%s)\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[skinbrowser] za ma�o pami�ci\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "sk�rka nie znaleziona (%s)\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "B��d odczytu pliku konfiguracyjnego sk�rki (%s).\n"
#define MSGTR_SKIN_LABEL "Sk�rki:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "O MPlayerze"
#define MSGTR_MENU_Open "Otw�rz..."
#define MSGTR_MENU_PlayFile "Odtw�rz plik..."
#define MSGTR_MENU_PlayVCD "Odtw�rz VCD..."
#define MSGTR_MENU_PlayDVD "Odtw�rz DVD..."
#define MSGTR_MENU_PlayURL "Odtw�rz URL..."
#define MSGTR_MENU_LoadSubtitle "Za�aduj napisy..."
#define MSGTR_MENU_DropSubtitle "Wy��cz napisy..."
#define MSGTR_MENU_LoadExternAudioFile "Za�aduj zewn�trzny plik audio..."
#define MSGTR_MENU_Playing "Odtwarzanie"
#define MSGTR_MENU_Play "Odtwarzaj"
#define MSGTR_MENU_Pause "Pauza"
#define MSGTR_MENU_Stop "Stop"
#define MSGTR_MENU_NextStream "Nast�pny strumie�"
#define MSGTR_MENU_PrevStream "Poprzedni strumie�"
#define MSGTR_MENU_Size "Rozmiar"
#define MSGTR_MENU_NormalSize "Normalny rozmiar"
#define MSGTR_MENU_DoubleSize "Podw�jny rozmiar"
#define MSGTR_MENU_FullScreen "Pe�ny ekran"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "Otw�rz dysk..."
#define MSGTR_MENU_ShowDVDMenu "Poka� menu DVD"
#define MSGTR_MENU_Titles "Tytu�y"
#define MSGTR_MENU_Title "Tytu� %2d"
#define MSGTR_MENU_None "(brak)"
#define MSGTR_MENU_Chapters "Rozdzia�y"
#define MSGTR_MENU_Chapter "Rozdzia� %2d"
#define MSGTR_MENU_AudioLanguages "J�zyki audio"
#define MSGTR_MENU_SubtitleLanguages "J�zyki napis�w"
#define MSGTR_MENU_PlayList "Playlista"
#define MSGTR_MENU_SkinBrowser "Przegl�darka sk�rek"
#define MSGTR_MENU_Preferences "Preferencje"
#define MSGTR_MENU_Exit "Wyj�cie..."
#define MSGTR_MENU_Mute "Wyciszenie (mute)"
#define MSGTR_MENU_Original "Oryginalny"
#define MSGTR_MENU_AspectRatio "Proporcje obrazu"
#define MSGTR_MENU_AudioTrack "�cie�ka audio"
#define MSGTR_MENU_Track "�cie�ka %d"
#define MSGTR_MENU_VideoTrack "�cie�ka video"

// --- equalizer
#define MSGTR_EQU_Audio "Audio"
#define MSGTR_EQU_Video "Video"
#define MSGTR_EQU_Contrast "Kontrast: "
#define MSGTR_EQU_Brightness "Jasno��: "
#define MSGTR_EQU_Hue "Barwa (hue): "
#define MSGTR_EQU_Saturation "Nasycenie (saturation): "
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
#define MSGTR_PLAYLIST_DirectoryTree "Drzewo katalogu"

// --- preferences
#define MSGTR_PREFERENCES_Audio "Audio"
#define MSGTR_PREFERENCES_Video "Video"
#define MSGTR_PREFERENCES_SubtitleOSD "Napisy i OSD"
#define MSGTR_PREFERENCES_Codecs "kodeki i demuxer"
#define MSGTR_PREFERENCES_Misc "Inne"

#define MSGTR_PREFERENCES_None "Brak"
#define MSGTR_PREFERENCES_DriverDefault "domy�lne ustawienia sterownika"
#define MSGTR_PREFERENCES_AvailableDrivers "Dost�pne sterowniki:"
#define MSGTR_PREFERENCES_DoNotPlaySound "Nie odtwarzaj d�wi�ku"
#define MSGTR_PREFERENCES_NormalizeSound "Normalizuj d�wi�k"
#define MSGTR_PREFERENCES_EnEqualizer "W��cz equalizer (korektor)"
#define MSGTR_PREFERENCES_ExtraStereo "W��cz extra stereo"
#define MSGTR_PREFERENCES_Coefficient "Wsp�czynnik:"
#define MSGTR_PREFERENCES_AudioDelay "Op�nienie d�wi�ku"
#define MSGTR_PREFERENCES_DoubleBuffer "W��cz podw�jne buforowanie"
#define MSGTR_PREFERENCES_DirectRender "W��cz bezpo�rednie renderowanie (direct rendering)"
#define MSGTR_PREFERENCES_FrameDrop "W��cz gubienie ramek"
#define MSGTR_PREFERENCES_HFrameDrop "W��cz gubienie du�ej ilo�ci ramek (niebezpieczne)"
#define MSGTR_PREFERENCES_Flip "Odwr�� obraz do g�ry nogami"
#define MSGTR_PREFERENCES_Panscan "Panscan: "
#define MSGTR_PREFERENCES_OSDTimer "Timer i wska�niki"
#define MSGTR_PREFERENCES_OSDProgress "Tylko belka"
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "Timer, czas w procentach i ca�kowity"
#define MSGTR_PREFERENCES_Subtitle "Napisy:"
#define MSGTR_PREFERENCES_SUB_Delay "Op�nienie: "
#define MSGTR_PREFERENCES_SUB_FPS "FPS:"
#define MSGTR_PREFERENCES_SUB_POS "Pozycja: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "Wy��cz automatyczne �adowanie napis�w"
#define MSGTR_PREFERENCES_SUB_Unicode "Napisy w Unicode"
#define MSGTR_PREFERENCES_SUB_MPSUB "Konwertuj dane napisy na format napis�w MPlayera"
#define MSGTR_PREFERENCES_SUB_SRT "Konwertuj dane napisy na format SRT (bazowany na czasie SubViewer)"
#define MSGTR_PREFERENCES_SUB_Overlap "Prze��cz nak�adanie (overlapping) napis�w"
#define MSGTR_PREFERENCES_Font "Czcionka:"
#define MSGTR_PREFERENCES_FontFactor "Skala czcionki:"
#define MSGTR_PREFERENCES_PostProcess "W��cz postprocessing"
#define MSGTR_PREFERENCES_AutoQuality "Automatyczna jako��:"
#define MSGTR_PREFERENCES_NI "U�yj parsera odpowiedniego dla AVI bez przeplotu (non-interleaved)"
#define MSGTR_PREFERENCES_IDX "Przebuduj tablic� indeks�w, je�li to potrzebne"
#define MSGTR_PREFERENCES_VideoCodecFamily "Rodzina kodek�w video:"
#define MSGTR_PREFERENCES_AudioCodecFamily "Rodzina kodek�w audio:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "Poziom OSD"
#define MSGTR_PREFERENCES_FRAME_Subtitle "Napisy"
#define MSGTR_PREFERENCES_FRAME_Font "Czcionka"
#define MSGTR_PREFERENCES_FRAME_PostProcess "Postprocessing"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "Kodeki & demuxer"
#define MSGTR_PREFERENCES_FRAME_Cache "Cache (pami�� podr�czna)"
#define MSGTR_PREFERENCES_FRAME_Misc "Inne"
#define MSGTR_PREFERENCES_Audio_Device "Urz�dzenie:"
#define MSGTR_PREFERENCES_Audio_Mixer "Mikser:"
#define MSGTR_PREFERENCES_Audio_MixerChannel "Kana� miksera:"
#define MSGTR_PREFERENCES_Message "Pami�taj, �e niekt�re opcje wymagaj� zrestartowania odtwarzania!"
#define MSGTR_PREFERENCES_DXR3_VENC "Enkoder video:"
#define MSGTR_PREFERENCES_DXR3_LAVC "U�yj LAVC (FFmpeg)"
#define MSGTR_PREFERENCES_DXR3_FAME "U�yj FAME"
#define MSGTR_PREFERENCES_FontEncoding1 "Unicode"
#define MSGTR_PREFERENCES_FontEncoding2 "Zachodnioeuropejskie j�zyki (ISO-8859-1)"
#define MSGTR_PREFERENCES_FontEncoding3 "Zachodnioeuropejskie j�zyki z Euro (ISO-8859-15)"
#define MSGTR_PREFERENCES_FontEncoding4 "J�zyki s�owia�skie i �rodkowoeuropejskie (ISO-8859-2)"
#define MSGTR_PREFERENCES_FontEncoding5 "Esperanto, Galijski, Malta�ski, Turecki (ISO-8859-3)"
#define MSGTR_PREFERENCES_FontEncoding6 "Stary, ba�tycki zestaw znak�w (ISO-8859-4)"
#define MSGTR_PREFERENCES_FontEncoding7 "Cyrlica (ISO-8859-5)"
#define MSGTR_PREFERENCES_FontEncoding8 "Arabski (ISO-8859-6)"
#define MSGTR_PREFERENCES_FontEncoding9 "Wsp�czesna Greka (ISO-8859-7)"
#define MSGTR_PREFERENCES_FontEncoding10 "Turecki (ISO-8859-9)"
#define MSGTR_PREFERENCES_FontEncoding11 "J�zyki ba�tyckie (ISO-8859-13)"
#define MSGTR_PREFERENCES_FontEncoding12 "Celtycki (ISO-8859-14)"
#define MSGTR_PREFERENCES_FontEncoding13 "Znaki hebrajskie (ISO-8859-8)"
#define MSGTR_PREFERENCES_FontEncoding14 "Rosyjski (KOI8-R)"
#define MSGTR_PREFERENCES_FontEncoding15 "Ukrai�ski, Bia�oruski (KOI8-U/RU)"
#define MSGTR_PREFERENCES_FontEncoding16 "Uproszczone znaki chi�skie (CP936)"
#define MSGTR_PREFERENCES_FontEncoding17 "Tradycyjne znaki chi�skie (BIG5)"
#define MSGTR_PREFERENCES_FontEncoding18 "Znaki japo�skie (SHIFT-JIS)"
#define MSGTR_PREFERENCES_FontEncoding19 "Znaki korea�skie (CP949)"
#define MSGTR_PREFERENCES_FontEncoding20 "Znaki tajskie (CP874)"
#define MSGTR_PREFERENCES_FontEncoding21 "Cyrlica Windows (CP1251)"
#define MSGTR_PREFERENCES_FontEncoding22 "J�zyki s�owia�skie i �rodkowoeuropejskie Windows (CP1250)"
#define MSGTR_PREFERENCES_FontNoAutoScale "Bez autoskalowania"
#define MSGTR_PREFERENCES_FontPropWidth "Proporcjonalnie do szeroko�ci obrazu"
#define MSGTR_PREFERENCES_FontPropHeight "Proporcjonalnie do wysoko�ci obrazu"
#define MSGTR_PREFERENCES_FontPropDiagonal "Proporcjonalnie do przek�tnej obrazu"
#define MSGTR_PREFERENCES_FontEncoding "Kodowanie:"
#define MSGTR_PREFERENCES_FontBlur "Rozmycie:"
#define MSGTR_PREFERENCES_FontOutLine "Obramowanie (Outline):"
#define MSGTR_PREFERENCES_FontTextScale "Skala tekstu:"
#define MSGTR_PREFERENCES_FontOSDScale "Skala OSD:"
#define MSGTR_PREFERENCES_Cache "Pami�� podr�czna"
#define MSGTR_PREFERENCES_CacheSize "Rozmiar pami�ci podr�cznej: "
#define MSGTR_PREFERENCES_LoadFullscreen "Rozpocznij w trybie pe�noekranowym"
#define MSGTR_PREFERENCES_SaveWinPos "Zapisz pozycj� okna"
#define MSGTR_PREFERENCES_XSCREENSAVER "Wy��cz XScreenSaver"
#define MSGTR_PREFERENCES_PlayBar "W�acz podr�czny pasek odtwarzania"
#define MSGTR_PREFERENCES_AutoSync "W��cz/Wy��cz autosynchronizacj�"
#define MSGTR_PREFERENCES_AutoSyncValue "Autosynchronizacja: "
#define MSGTR_PREFERENCES_CDROMDevice "Urz�dzenie CD-ROM:"
#define MSGTR_PREFERENCES_DVDDevice "Urz�dzenie DVD:"
#define MSGTR_PREFERENCES_FPS "FPS:"
#define MSGTR_PREFERENCES_ShowVideoWindow "Pokazuj okno video gdy nieaktywne"

#define MSGTR_ABOUT_UHU "Rozw�j GUI sponsorowany przez UHU Linux\n"
#define MSGTR_ABOUT_CoreTeam "   G��wni cz�onkowie zespo�u MPlayera:\n"
#define MSGTR_ABOUT_AdditionalCoders "   Dodatkowi koderzy (programi�ci):\n"
#define MSGTR_ABOUT_MainTesters "   G�owni testerzy:\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "B��d krytyczny!"
#define MSGTR_MSGBOX_LABEL_Error "B��d!" 
#define MSGTR_MSGBOX_LABEL_Warning "Ostrze�enie!"

#endif

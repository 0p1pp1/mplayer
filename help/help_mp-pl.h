// Translated by:  Kuba "Qba" Misiorny <jim85@wp.pl>
// Wszelkie uwagi i poprawki mile widziane :)
//
// Synced with help_mp-en.h 1.140

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
" -alang/-slang    wybierz j�zyk �cie�ki d�wi�kowej/napis�w (dwuznakowy kod kraju)\n"
#endif
" -ss <pozycja>    skok do pozycji (sekundy albo hh:mm:ss)\n"
" -nosound         nie odtwarzaj d�wi�ku\n"
" -fs              odtwarzaj na pe�nym ekranie (-vm, -zoom, szczeg�y w man)\n"
" -x <x> -y <y>    ustaw rozmiar obrazu wyj�ciowego (u�ywaj z -vm, -zoom)\n"
" -sub <plik>      wybierz plik z napisami (patrz tak�e -subfps, -subdelay)\n"
" -playlist <plik> wybierz list� odtwarzania \n"
" -vid x -aid y    wybierz strumie� video (x) lub audio (y)\n"
" -fps x -srate y  zmie� pr�dko�� odtwarzania video (x fps) i audio (y Hz) \n"
" -pp <jako��>     w��cz filtr postprocessingu (szczeg�y w man)\n"
" -framedrop       w��cz gubienie ramek (dla wolnych maszyn)\n"
"\n"
"Podstawowe klawisze: (Pe�na lista na stronie man, sprawd� te� input.conf)\n"
" <-   lub  ->      skok w ty�/prz�d o 10 sekund\n"
" g�ra lub d�      skok w ty�/prz�d o 1 minut�\n"
" pgup lub pgdown   skok w ty�/prz�d o 10 minut\n"
" < lub >           poprzednia/nast�pna pozycja na li�cie odtwarzania\n"
" p lub SPACE       pauza (dowolny klawisz aby kontynuowa�)\n"
" q lub ESC         wyj�cie\n"
" + lub -           zmie� op�nienie d�wi�ku o +/- 0.1 sekundy\n"
" o                 tryb OSD (On Screen Display): brak / belka / belka + timer\n"
" * lub /           zwi�ksz/zmniejsz g�o�no�� (PCM)\n"
" z lub x           zmie� op�nienie napis�w o +/- 0.1 sekundy\n"
" r lub t           zmie� po�o�enie napis�w wy�ej/ni�ej, spr�buj te� -vf expand\n"
"\n"
" * * * DOK�ADNY SPIS WSZYSTKICH OPCJI ZNAJDUJE SI� NA STRONIE MAN * * *\n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// MPlayer.c:

#define MSGTR_Exiting "\nWychodz�...\n"
#define MSGTR_ExitingHow "\nWychodz�...(%s)\n"
#define MSGTR_Exit_quit "Wyj�cie"
#define MSGTR_Exit_eof "Koniec pliku"
#define MSGTR_Exit_error "B��d Krytyczny"
#define MSGTR_IntBySignal "\nMPlayer przerwany sygna�em %d w module: %s\n"
#define MSGTR_NoHomeDir "Nie mog� znale�� katalogu domowego\n"
#define MSGTR_GetpathProblem "Problem z get_path (\"config\")\n"
#define MSGTR_CreatingCfgFile "Tworz� plik konfiguracyjny: %s\n"
#define MSGTR_InvalidAOdriver "Nieprawid�owa nazwa wyj�ciowego sterownika audio -> %s\n(lista: '-ao help').\n"
#define MSGTR_CopyCodecsConf "(Skopiuj etc/codecs.conf ze �r�de� MPlayera do ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "U�ywam wbudowanego (domy�lnego) pliku codecs.conf.\n"
#define MSGTR_CantLoadFont "Nie mog� za�adowa� czcionki: %s\n"
#define MSGTR_CantLoadSub "Nie mog� za�adowa� napis�w: %s\n"
#define MSGTR_DumpSelectedStreamMissing "dump: B��D KRYTYCZNY: Brak wybranego strumienia\n"
#define MSGTR_CantOpenDumpfile "Nie mog� otworzy� pliku dump.\n"
#define MSGTR_CoreDumped "Core dumped (Zrzut pami�ci)\n"
#define MSGTR_FPSnotspecified "Warto�� FPS nie podana (lub b��dna) w nag��wku, u�yj opcji -fps <ilo��_ramek_na_sekund�>.\n"
#define MSGTR_TryForceAudioFmtStr "Wymuszam zastosowanie kodeka audio z rodziny %s...\n"
#define MSGTR_CantFindAudioCodec "Nie mog� znale�� kodeka dla formatu audio 0x%X.\n"
#define MSGTR_RTFMCodecs "Przeczytaj DOCS/HTML/pl/codecs.html!\n"
#define MSGTR_TryForceVideoFmtStr "Wymuszam zastosowanie kodeka video z rodziny %s...\n"
#define MSGTR_CantFindVideoCodec "Nie mog� znale�� kodeka pasuj�cego do wybranego -vo i formatu video 0x%X.\n"
#define MSGTR_CannotInitVO "B��D KRYTYCZNY: Nie mog� zainicjalizowa� sterownika video.\n"
#define MSGTR_CannotInitAO "Nie mog� otworzy�/zainicjalizowa� urz�dzenia audio -> brak d�wi�ku.\n"
#define MSGTR_StartPlaying "Zaczynam odtwarzanie... \n"

#define MSGTR_SystemTooSlow "\n\n"\
"           ************************************************\n"\
"           ********* Tw�j system jest ZA WOLNY!!! ********\n"\
"           ************************************************\n\n"\
"Prawdopodobne przyczyny, rozwi�zania:\n"\
"- Najbardziej powszechne: wadliwe/b��dne _sterowniki_audio_\n"\
"  - Spr�buj u�y� -ao sdl, u�yj ALSA 0.5 lub emulacji OSS w ALSA 0.9\n"\
"  - Poeksperymentuj z r�nymi warto�ciami -autosync, \"30\" na dobry pocz�tek.\n"\
"- Za wolny sterownik wyj�ciowy:\n"\
"  - Spr�buj innego sterownika -vo (lista: -vo help) albo -framedrop!\n"\
"- Za wolny procesor\n"\
"  - Nie pr�buj odtwarza� du�ych DVD/DivX�w na wolnym procesorze! Spr�buj -hardframedrop.\n"\
"- Zepsuty plik\n"\
"  - Spr�buj r�nych kombinacji -nobps, -ni, forceidx, -mc 0.\n"\
"- Za wolne �r�d�o (zamontowane NFS/SMB, DVD, VCD itd.)\n"\
"  - Spr�buj: -cache 8192.\n"\
"- Czy u�ywasz pami�ci podr�cznej do odtwarzania plik�w bez przeplotu? Spr�buj -nocache\n"\
"Przeczytaj DOCS/HTML/pl/devices.html gdzie znajdziesz wskaz�wki\n"\
"jak przy�pieszy� dzia�anie MPlayera\n"\
"Je�li nic nie pomaga przeczytaj DOCS/HTML/pl/bugreports.html.\n\n"

#define MSGTR_NoGui "MPlayer zosta� skompilowany BEZ obs�ugi GUI.\n"
#define MSGTR_GuiNeedsX "GUI MPlayera potrzebuje X11.\n"
#define MSGTR_Playing "Odtwarzam %s.\n"
#define MSGTR_NoSound "Audio: brak d�wi�ku\n"
#define MSGTR_FPSforced "Warto�� FPS wymuszona na %5.3f  (ftime: %5.3f).\n"
#define MSGTR_CompiledWithRuntimeDetection "Skompilowany z wykrywaniem procesora podczas pracy - UWAGA - W ten spos�b nie uzyskasz\n najlepszej wydajno�ci, przekompiluj MPlayera z opcj� --disable-runtime-cpudetection.\n"
#define MSGTR_CompiledWithCPUExtensions "Skompilowany dla procesora z rozszerzeniami:"
#define MSGTR_AvailableVideoOutputDrivers "Dost�pne sterowniki video:\n"
#define MSGTR_AvailableAudioOutputDrivers "Dost�pne sterowniki audio:\n"
#define MSGTR_AvailableAudioCodecs "Dost�pne kodeki audio:\n"
#define MSGTR_AvailableVideoCodecs "Dost�pne kodeki video:\n"
#define MSGTR_AvailableAudioFm "Dost�pne (wkompilowane) rodziny kodek�w/sterownik�w audio:\n"
#define MSGTR_AvailableVideoFm "Dost�pne (wkompilowane) rodziny kodek�w/sterownik�w video:\n"
#define MSGTR_AvailableFsType "Dost�pne tryby pe�noekranowe:\n"
#define MSGTR_UsingRTCTiming "U�ywam sprz�towego zegara czasu rzeczywistego (Linux RTC) (%ldHz).\n"
#define MSGTR_CannotReadVideoProperties "Video: nie mog� odczyta� w�a�ciwo�ci.\n"
#define MSGTR_NoStreamFound "Nie znalaz�em �adnego strumienia\n"
#define MSGTR_ErrorInitializingVODevice "B��d przy otwieraniu/inicjalizacji wybranego urz�dzenia video (-vo).\n"
#define MSGTR_ForcedVideoCodec "Wymuszony kodek video: %s\n"
#define MSGTR_ForcedAudioCodec "Wymuszony kodek audio: %s\n"
#define MSGTR_Video_NoVideo "Video: brak video\n"
#define MSGTR_NotInitializeVOPorVO "\nB��D KRYTYCZNY: Nie mog� zainicjalizowa� filtra video (-vf) lub wyj�cia video (-vo).\n"
#define MSGTR_Paused "\n  =====  PAUZA  =====\r" // no more than 23 characters (status line for audio files)
#define MSGTR_PlaylistLoadUnable "\nNie mog� za�adowa� listy odtwarzania %s.\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- MPlayer zako�czy� prac� z powodu b��du 'Nieprawid�owa operacja'\n"\
"  Mo�e to by� b��d w naszym nowym kodzie wykrywaj�cym procesor\n"\
"  Przeczytaj prosz� DOCS/HTML/pl/bugreports.html.\n"
#define MSGTR_Exit_SIGILL \
"- MPlayer zako�czy� prac� z powodu b��du  'Nieprawid�owa operacja'\n"\
"  Zdarza si� to najcz�ciej, gdy uruchamiasz MPlayera na innym procesorze ni� ten\n"\
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
"  DOCS/HTML/pl/bugreports.html. Nie mo�emy i nie pomo�emy, je�li nie przedstawisz tych informacji\n"\
"  zg�aszaj�c mo�liwy b��d.\n"

#define MSGTR_EdlCantUseBothModes "Nie mo�na uzywa� -edl i -edlout w tym samym czasie.\n"
#define MSGTR_EdlOutOfMem "Nie mog� zaalokowa� wystarczaj�co du�o pami�ci dla danych EDL.\n"
#define MSGTR_EdlRecordsNo "Odczyta�em %d akcji EDL.\n"
#define MSGTR_EdlQueueEmpty "Nie ma �adnych akcji EDL do wykonania.\n"
#define MSGTR_EdlCantOpenForWrite "Nie mog� otworzy� pliku EDL [%s] do zapisu.\n"
#define MSGTR_EdlCantOpenForRead "Nie mog� otworzy� pliku EDL [%s] do odczytu.\n" 
#define MSGTR_EdlNOsh_video "Nie mo�na u�ywa� EDL bez strumienia video, wy��czam.\n"
#define MSGTR_EdlNOValidLine "Nieprawid�owa komenda EDL: %s\n"
#define MSGTR_EdlBadlyFormattedLine "�le sformatowana komenda [%d], odrzucam.\n"
#define MSGTR_EdlBadLineOverlap "Ostatnia pozycja stopu [%f]; nast�pny start to "\
"[%f]. Wpisy musz� by� w kolejno�ci chronologicznej, nie mo�na przeskakiwa�. Odrzucam.\n"
#define MSGTR_EdlBadLineBadStop "Czas stopu musi si� znale�� za ustawionym czasem startu.\n"
 
// mencoder.c:

#define MSGTR_UsingPass3ControllFile "U�ywam pliku kontrolnego pass3: %s\n" 
#define MSGTR_MissingFilename "\nBrak nazwy pliku.\n\n"
#define MSGTR_CannotOpenFile_Device "Nie mog� otworzy� pliku/urz�dzenia\n"
#define MSGTR_CannotOpenDemuxer "Nie mog� otworzy� demuxera.\n"
#define MSGTR_NoAudioEncoderSelected "\nNie wybrano kodera audio (-oac). Wybierz jaki� (Lista: -oac help) albo u�yj opcji '-nosound' \n"
#define MSGTR_NoVideoEncoderSelected "\nNie wybrano kodera video (-ovc). Wybierz jaki� (Lista: -ovc help)\n"
#define MSGTR_CannotOpenOutputFile "Nie mog� otworzy� pliku wyj�ciowego '%s'.\n"
#define MSGTR_EncoderOpenFailed "Nie mog� otworzy� kodera.\n"
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

#define MSGTR_OpenedStream "sukces: format: %d  dane: 0x%X - 0x%x\n"
#define MSGTR_VCodecFramecopy "videocodec: framecopy (%dx%d %dbpp fourcc=%x)\n"
#define MSGTR_ACodecFramecopy "audiocodec: framecopy (format=%x chans=%d rate=%ld bits=%d bps=%ld sample-%ld)\n"
#define MSGTR_CBRPCMAudioSelected "Wybrano d�wi�k CBR PCM\n"
#define MSGTR_MP3AudioSelected "Wybrano d�wi�k MP3\n"
#define MSGTR_CannotAllocateBytes "Nie mo�na by�o zaalokowa� %d bajt�w\n"
#define MSGTR_SettingAudioDelay "Ustawiam OPӬNIENIE D�WI�KU na %5.3f\n"
#define MSGTR_SettingAudioInputGain "Ustawiam podbicie wej�cia d�wi�ku na %f\n"
#define MSGTR_LamePresetEquals "\nustawienie=%s\n\n"
#define MSGTR_LimitingAudioPreload "Ograniczam buforowanie d�wi�ku do 0.4s\n"
#define MSGTR_IncreasingAudioDensity "Zwi�kszam g�sto�� d�wi�ku do 4\n"
#define MSGTR_ZeroingAudioPreloadAndMaxPtsCorrection "Wymuszam buforowanie d�wi�ku na 0, maksymaln� korekcj� pts na 0\n"
#define MSGTR_CBRAudioByterate "\n\nCBR audio: %ld bajt�w/sek, %d bajt�w/blok\n"
#define MSGTR_LameVersion "Wersja LAME %s (%s)\n\n"
#define MSGTR_InvalidBitrateForLamePreset "B��d: Wybrany bitrate jest poza prawid�owym zasiegiem tego ustawienia\n"\
"\n"\
"Podczas u�ywania tego trybu musisz wpisa� warto�� pomi�dzy \"8\" i \"320\"\n"\
"\n"\
"Dalsze informacje: \"-lameopts preset=help\"\n"
#define MSGTR_InvalidLamePresetOptions "B��d: Nie wprowadzi�e� odpowiedniego profilu lub/i opcji dla tego ustawienia\n"\
"\n"\
"Dost�pne profile:\n"\
"\n"\
"   <fast>        standard\n"\
"   <fast>        extreme\n"\
"                      insane\n"\
"   <cbr> (ABR Mode) - Tryb ABR jest domy�lny. Aby go u�y�,\n"\
"                      podaj po prostu bitrate. Na przyk�ad:\n"\
"                      \"preset=185\" aktywuje to ustawienie\n"\
"                      i u�ywa 185 jako �rednie kbps.\n"\
"\n"\
"    Kilka przyk�ad�w:\n"\
"\n"\
"    \"-lameopts fast:preset=standard  \"\n"\
" lub \"-lameopts  cbr:preset=192       \"\n"\
" lub \"-lameopts      preset=172       \"\n"\
" lub \"-lameopts      preset=extreme   \"\n"\
"\n"\
"Dalsze informacje: \"-lameopts preset=help\"\n"
#define MSGTR_LamePresetsLongInfo "\n"\
"Zestawy ustawie� zaprojektowane s� w celu uzysakania jak najwy�szej jako�ci.\n"\
"\n"\
"Byly poddawane i dopracowywane przez rygorystyczne testy\n"\
"odsluchowe, aby osiagnac ten cel.\n"\
"\n"\
"S� one bez przerwy aktualizowane, aby nad��y� za naj�wie�szymi nowinkami\n"\
"co powinno przynosi� prawie najwy�sz� osi�galn� w LAME jako��.\n"\
"\n"\
"Aby aktywowa� te ustawienia:\n"\
"\n"\
"   Dla tryb�w VBR (zazwyczaj najlepsza jako��):\n"\
"\n"\
"     \"preset=standard\" To ustawienie powinno by� prze�roczyste\n"\
"                             dla wi�kszo�ci ludzi przy odtwarzaniu muzyki i odrazu\n"\
"                             jest w niez�ej jako�ci.\n"\
"\n"\
"     \"preset=extreme\" Je�li masz bardzo dobry s�uch i r�wnie dobry sprz�t,\n"\
"                             to ustawienie, daje troch� lepsz� jako� ni� \n"\
"                             tryb \"standard\".\n"\
"\n"\
"   Dla trybu CBR 320kbps (najwy�sza mo�liwa jako�� ze wszystkich mo�liwych ustawie�):\n"\
"\n"\
"     \"preset=insane\"  To ustawienie b�dzie przesad�\n"\
"                             dla wi�kszo�ci ludzi w wi�kszo�ci przypadk�w,\n"\
"                             ale je�eli musisz mie� najwy�sz� jako� niezale�nie\n"\
"                             od wielko�ci pliku, to jest w�a�ciwa droga.\n"\
"\n"\
"   Dla tryb�w ABR (wysoka jako�� z ustalonym bitratem, ale tak wysoka jak VBR):\n"\
"\n"\
"     \"preset=<kbps>\"  Okre�lenie tego parametru da Tobie dobr� jako��\n"\
"                             przy ustalonym bitrate'cie. pieraj�c si� na niej,\n"\
"                             okre�li ono optymalne ustawienia dla danej sytuacji.\n"\
"			      Niestety nie jest ono tak elastyczne jak VBR i przewa�nie nie\n"\
"                            zapewni takiego samego poziomu jako�ci jak VBR\n"\
"                            dla wy�szych warto�ci bitrate.\n"\
"\n"\
"Poni�sze opcje s� r�wnie� dost�pne dla odpowiadaj�cych profili:\n"\
"\n"\
"   <fast>        standard\n"\
"   <fast>        extreme\n"\
"                 insane\n"\
"   <cbr> (ABR Mode) - Tryb ABR jest domy�lny. Aby go u�y�,\n"\
"                      podaj po prostu bitrate. Na przyk�ad:\n"\
"                      \"preset=185\" aktywuje to ustawienie\n"\
"                      i u�ywa 185 jako �rednie kbps.\n"\
"\n"\
"   \"fast\" - Uruchamia nowe szybkie VBR dla danego profilu. Wad� \n"\
"            w stosunku do ustawienia szybko�ci jest to, i� cz�sto bitrate jest\n"\
"            troszk� wy�szy ni� przy normalnym trybie, a jako�� \n"\
"            mo�e by� troche ni�sza.\n"\
"   Uwaga: obecna wersja ustawienia \"fast\" mo�e skutkowa� wyskomi warto�ciami\n"\
"            bitrate w stosunku do tego z normalnego ustawienia.\n"\
"\n"\
"   \"cbr\"  - Je�eli u�ywasz trybu ABR (przeczytaj powy�ej) ze znacznym bitratem\n"\
"            jak 80, 96, 112, 128, 160, 192, 224, 256, 320,\n"\
"            mo�esz u�y� opcji  \"cbr\", aby wymusi� enkodowanie w trybie CBR\n"\
"            zamiast standardowego trybu abr. ABR daje wy�sz� jako��, ale CBR\n"\
"            mo�e si� przyda� w sytuacjach, gdy strumieniowanie mp3 przez\n"\
"            Internet jest wa�ne\n"\
"\n"\
"    Na przyk�ad:\n"\
"\n"\
"    \"-lameopts fast:preset=standard  \"\n"\
" or \"-lameopts  cbr:preset=192       \"\n"\
" or \"-lameopts      preset=172       \"\n"\
" or \"-lameopts      preset=extreme   \"\n"\
"\n"\
"\n"\
"Dost�pnych jest kilka synonim�w dla trybu ABR:\n"\
"phone => 16kbps/mono        phon+/lw/mw-eu/sw => 24kbps/mono\n"\
"mw-us => 40kbps/mono        voice => 56kbps/mono\n"\
"fm/radio/tape => 112kbps    hifi => 160kbps\n"\
"cd => 192kbps               studio => 256kbps"
#define MSGTR_ConfigfileError "b��� pliku konfiguracyjnego"
#define MSGTR_ErrorParsingCommandLine "b��� przy przetwarzaniu lini komend"
#define MSGTR_VideoStreamRequired "Strumie� video jest wymagany!\n"
#define MSGTR_ForcingInputFPS "wej�ciowa warto�� fps b�dzie zinterpretowana jako %5.2f\n"
#define MSGTR_RawvideoDoesNotSupportAudio "Format wyj�ciowy RAWVIDEO nie wspiera audio - wy��czam audio\n"
#define MSGTR_DemuxerDoesntSupportNosound "Ten demuxer jeszcze nie wspiera -nosound.\n"
#define MSGTR_MemAllocFailed "nie uda�o sie zaalokowa� pami�ci"
#define MSGTR_NoMatchingFilter "Nie mo�na znale�� pasuj�cego formatu filtra/ao!\n"
#define MSGTR_MP3WaveFormatSizeNot30 "sizeof(MPEGLAYER3WAVEFORMAT)==%d!=30, zepsuty kompilator C?\n"
#define MSGTR_NoLavcAudioCodecName "Audio LAVC - brakuj�ca nazwa kodeka!\n"
#define MSGTR_LavcAudioCodecNotFound "Audio LAVC - nie mog� znale�� kodeka dla %s\n"
#define MSGTR_CouldntAllocateLavcContext "Audio LAVC - nie mog� zaalokowa� tre�ci!\n"
#define MSGTR_CouldntOpenCodec "Nie mog� otworzy� kodeka %s, br=%d\n"
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
" aq=<0-9>      jako�� algorytmu (0-najlepsza/najwolniejszy, 9-najgorsza/najszybszy)\n"\
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
" fast          Prze��cz na szybsze kodowanie na poni�szych ustawieniach VBR,\n"\
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

#define MSGTR_MaybeNI "Mo�e odtwarzasz plik/strumie� bez przeplotu (non-interleaved) albo kodek nie zadzia�a�?\n"\
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
#define MSGTR_ClipInfo "Informacje o klipie:\n"

#define MSGTR_LeaveTelecineMode "\ndemux_mpg: Wykryto zawarto�� 30fps NTSC, zmieniam liczb� ramek na sekund�.\n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: Wykryto progresywn� zawarto�� 24fps NTSC, zmieniam liczb� ramek na sekund�"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "Nie mog� otworzy� kodeka.\n"
#define MSGTR_CantCloseCodec "Nie mog� zamkn�� kodeka.\n"

#define MSGTR_MissingDLLcodec "B��d: Nie mog� otworzy� wymaganego kodeka DirectShow %s.\n"
#define MSGTR_ACMiniterror "Nie mog� za�adowa�/zainicjalizowa� kodeka Win32/ACM AUDIO (Brakuje pliku DLL?).\n"
#define MSGTR_MissingLAVCcodec "Nie mog� znale�� kodeka '%s' w libavcodec...\n"

#define MSGTR_MpegNoSequHdr "MPEG: B��D KRYTYCZNY: EOF (koniec pliku) podczas szukania nag��wka sekwencji.\n"
#define MSGTR_CannotReadMpegSequHdr "B��D KRYTYCZNY: Nie mog� odczyta� nag��wka sekwencji.\n"
#define MSGTR_CannotReadMpegSequHdrEx "B��D KRYTYCZNY: Nie mog� odczyta� rozszerzenia nag��wka sekwencji.\n"
#define MSGTR_BadMpegSequHdr "MPEG: Nieprawid�owy nag��wek sekwencji\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: Nieprawid�owe rozszerzenie nag��wka sekwencji\n"

#define MSGTR_ShMemAllocFail "Nie mog� zaalokowa� pami�ci wsp�dzielonej.\n"
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
#define MSGTR_CannotFindColorspace "Nie mog� znale�� odpowiedniej przestrzenii kolor�w, nawet poprzez wstawienie 'scale' :(\n"

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
#define MSGTR_PlayList "Lista Odtwarzania"
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
#define MSGTR_NEEDLAVCFAME "Przykro mi, nie mo�esz odtwarza� plik�w innych ni� MPEG za pomoc� urz�dzenia DXR3/H+ bez przekodowania.\nW��cz lavc albo fame w konfiguracji DXR3/H+"

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[skin] b��d w pliku konfiguracyjnym sk�rki, w linii %d: %s"
#define MSGTR_SKIN_WARNING1 "[skin] ostrze�enie w pliku konfiguracyjnym w linii %d:\nznaleziono znacznik widget (%s) ale nie ma przed nim \"section\""
#define MSGTR_SKIN_WARNING2 "[skin] ostrze�enie w pliku konfiguracyjnym w linii %d:\nznaleziono znacznik widget (%s) ale nie ma przednim \"subsection\""
#define MSGTR_SKIN_WARNING3 "[skin] ostrze�enie w pliku konfiguracyjnym w linii %d::\nta podsekcja nie jest obs�ugiwana przez widget (%s)"
#define MSGTR_SKIN_BITMAP_16bit  "Bitmapy 16 bitowe lub mniejsze nie s� obs�ugiwane (%s).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "nie znaleziono pliku (%s)\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "b��d odczytu bmp (%s)\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "b��d odczytu tga (%s)\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "b��d odczytu png (%s)\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "tga kompresowane przez RLE nie obs�ugiwane (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "nieznany typ pliku (%s)\n"
#define MSGTR_SKIN_BITMAP_ConvertError "b��d przy konwersji 24 bit�w na 32 bity (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "nieznany komunikat: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "za ma�o pami�ci\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "za du�o zadeklarowanych czcionek\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "nie znaleziono pliku z czcionk�\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "nie znaleziono pliku z obrazem czcionki\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "nieistniej�cy identyfikator czcionki (%s)\n"
#define MSGTR_SKIN_UnknownParameter "nieznany parametr (%s)\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "nie znalaz�em sk�rki (%s)\n"
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
#define MSGTR_MENU_AudioLanguages "J�zyki �cie�ki d�wi�kowej"
#define MSGTR_MENU_SubtitleLanguages "J�zyki napis�w"
#define MSGTR_MENU_PlayList "Lista odtwarzania"
#define MSGTR_MENU_SkinBrowser "Przegl�darka sk�rek"
#define MSGTR_MENU_Preferences "Preferencje"
#define MSGTR_MENU_Exit "Wyj�cie..."
#define MSGTR_MENU_Mute "Wyciszenie"
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
#define MSGTR_EQU_Hue "Barwa: "
#define MSGTR_EQU_Saturation "Nasycenie: "
#define MSGTR_EQU_Front_Left "Lewy przedni"
#define MSGTR_EQU_Front_Right "Prawy przedni"
#define MSGTR_EQU_Back_Left "Lewy tylny"
#define MSGTR_EQU_Back_Right "Prawy tylny"
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
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "Kodeki i demuxer"
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
#define MSGTR_PREFERENCES_FontOutLine "Obramowanie:"
#define MSGTR_PREFERENCES_FontTextScale "Skala tekstu:"
#define MSGTR_PREFERENCES_FontOSDScale "Skala OSD:"
#define MSGTR_PREFERENCES_Cache "Pami�� podr�czna"
#define MSGTR_PREFERENCES_CacheSize "Rozmiar pami�ci podr�cznej: "
#define MSGTR_PREFERENCES_LoadFullscreen "Rozpocznij w trybie pe�noekranowym"
#define MSGTR_PREFERENCES_SaveWinPos "Zapisz pozycj� okna"
#define MSGTR_PREFERENCES_XSCREENSAVER "Wy��cz XScreenSaver"
#define MSGTR_PREFERENCES_PlayBar "W��cz podr�czny pasek odtwarzania"
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


// ======================= VO Video Output drivers ========================

#define MSGTR_VOincompCodec "Wybrane urz�dzenie wyj�cia video jest niekompatybilne z tym kodekiem.\n"
#define MSGTR_VO_GenericError "Wyst�pi� b��d"
#define MSGTR_VO_UnableToAccess "Brak dost�pu"
#define MSGTR_VO_ExistsButNoDirectory "ju� istnieje, ale nie jest katalogiem."
#define MSGTR_VO_DirExistsButNotWritable "Wyj�ciowy katalog ju� istnieje, ale nie jest zapisywalny."
#define MSGTR_VO_DirExistsAndIsWritable "Wyj�ciowy katalog ju� istnieje i nie jest zapisywalny."
#define MSGTR_VO_CantCreateDirectory "Nie mo�na utworzy� wyj�ciowego katalogu."
#define MSGTR_VO_CantCreateFile "Nie mo�na utworzy� pliku wyj�ciowego."
#define MSGTR_VO_DirectoryCreateSuccess "Katalog wyj�ciowy stworzony."
#define MSGTR_VO_ParsingSuboptions "Interpretuj� podopcje."
#define MSGTR_VO_SuboptionsParsedOK "Podopcje zinterpretowane poprawnie."
#define MSGTR_VO_ValueOutOfRange "Warto�� poza zakresem"
#define MSGTR_VO_NoValueSpecified "Nie podano �adnej warto�ci."
#define MSGTR_VO_UnknownSuboptions "Nieznana podopcja(e)"

// vo_jpeg.c
#define MSGTR_VO_JPEG_ProgressiveJPEG "Progresywny JPEG w��czony."
#define MSGTR_VO_JPEG_NoProgressiveJPEG "Progresywny JPEG wy��czony."
#define MSGTR_VO_JPEG_BaselineJPEG "Baseline JPEG w��czony."
#define MSGTR_VO_JPEG_NoBaselineJPEG "Baseline JPEG wy��czony."

// vo_pnm.c
#define MSGTR_VO_PNM_ASCIIMode "Tryb ASCII w��czony."
#define MSGTR_VO_PNM_RawMode "Surowy tryb w��czony."
#define MSGTR_VO_PNM_PPMType "Zapisz� pliki PPM."
#define MSGTR_VO_PNM_PGMType "Zapisz� pliki PGM" 
//Will write PGM files.
#define MSGTR_VO_PNM_PGMYUVType "Zapisz� pliki PGMYUV."


// vo_yuv4mpeg.c
#define MSGTR_VO_YUV4MPEG_InterlacedHeightDivisibleBy4 "Tryb przeplotu wymaga aby wysoko�� obrazka by�a podzielna przez 4."
#define MSGTR_VO_YUV4MPEG_InterlacedLineBufAllocFail "Nie mog� zaalokowa� lini bufora dla trybu przeplotu."
#define MSGTR_VO_YUV4MPEG_InterlacedInputNotRGB "Wej�cie nie jest w formacie RGB, nie mog� oddzieli� jasno�ci przez pola."
#define MSGTR_VO_YUV4MPEG_WidthDivisibleBy2 "Szeroko�� obrazka musi by� podzielna przez 2."
#define MSGTR_VO_YUV4MPEG_NoMemRGBFrameBuf "Za ma�o pami�ci aby zaalokowa� bufor ramek RGB."
#define MSGTR_VO_YUV4MPEG_OutFileOpenError "Nie mog� dosta� pami�ci lub pliku aby zapisa� \"stream.yuv\"!"
#define MSGTR_VO_YUV4MPEG_OutFileWriteError "B��d przy zapisie pliku do wyj�cia!"
#define MSGTR_VO_YUV4MPEG_UnknownSubDev "Nieznane podurz�dzenie: %s"
#define MSGTR_VO_YUV4MPEG_InterlacedTFFMode "U�ywam wyj�cia w trybie przeplotu, najwy�sze pola najpierw."
#define MSGTR_VO_YUV4MPEG_InterlacedBFFMode "U�ywam wyj�cia w trybie przeplotu, najni�sze pola najpierw."
#define MSGTR_VO_YUV4MPEG_ProgressiveMode "U�ywam (domy�lnego) trybu progresywnych ramek."

// Old vo drivers that have been replaced

#define MSGTR_VO_PGM_HasBeenReplaced "Sterownki wyj�cia video pgm zosta� zast�piony przez -vo pnm:pgmyuv.\n"
#define MSGTR_VO_MD5_HasBeenReplaced "Sterownik wyj�cia video md5 zosta� zast�piony przez -vo md5sum.\n"


// ======================= AO Audio Output drivers ========================

// libao2 

// ao_oss.c
#define MSGTR_AO_OSS_CantOpenMixer "[AO OSS] audio_setup: Nie mog� otworzy� miksera %s: %s\n"
#define MSGTR_AO_OSS_ChanNotFound "[AO OSS] audio_setup: Mikser karty d�wi�kowej nie ma kana�u '%s', u�ywam domy�lnego.\n"
#define MSGTR_AO_OSS_CantOpenDev "[AO OSS] audio_setup: Nie mog� otworzy� urz�dzenia audio %s: %s\n"
#define MSGTR_AO_OSS_CantMakeFd "[AO OSS] audio_setup: Nie mog� utworzy� deskryptora blokuj�cego: %s\n"
#define MSGTR_AO_OSS_CantSetAC3 "[AO OSS] Nie mog� ustawi� urz�dzenia audio %s na wyj�cie AC3, pr�buj� S16...\n"
#define MSGTR_AO_OSS_CantSetChans "[AO OSS] audio_setup: Nie mog� ustawi� urz�dzenia audio na %d kana��w.\n"
#define MSGTR_AO_OSS_CantUseGetospace "[AO OSS] audio_setup: sterownik nie obs�uguje SNDCTL_DSP_GETOSPACE :-(\n"
#define MSGTR_AO_OSS_CantUseSelect "[AO OSS]\n   ***  Tw�j sterownik d�wi�ku NIE obs�uguje select()  ***\n Przekompiluj mplayera z #undef HAVE_AUDIO_SELECT w config.h !\n\n"
#define MSGTR_AO_OSS_CantReopen "[AO OSS]\nB��d krytyczny: *** NIE MO�NA PONOWNIE OTWORZY� / ZRESETOWA� URZ�DZENIA AUDIO *** %s\n"

// ao_arts.c
#define MSGTR_AO_ARTS_CantInit "[AO ARTS] %s\n"
#define MSGTR_AO_ARTS_ServerConnect "[AO ARTS] Po��czono z serwerem d�wi�ku.\n"
#define MSGTR_AO_ARTS_CantOpenStream "[AO ARTS] Nie mo�na otworzy� strumienia.\n"
#define MSGTR_AO_ARTS_StreamOpen "[AO ARTS] Strumie� otwarty.\n"
#define MSGTR_AO_ARTS_BufferSize "[AO ARTS] rozmiar bufora: %d\n"

// ao_dxr2.c
#define MSGTR_AO_DXR2_SetVolFailed "[AO DXR2] Ustawienie g�o�no�ci na %d nie powiod�o si�.\n"
#define MSGTR_AO_DXR2_UnsupSamplerate "[AO DXR2] dxr2: %d Hz nie obs�ugiwana, spr�buj \"-aop list=resample\"\n"

// ao_esd.c
#define MSGTR_AO_ESD_CantOpenSound "[AO ESD] esd_open_sound zawi�d�: %s\n"
#define MSGTR_AO_ESD_LatencyInfo "op�nienie [AO ESD]: [server: %0.2fs, net: %0.2fs] (adjust %0.2fs)\n"
#define MSGTR_AO_ESD_CantOpenPBStream "[AO ESD] Nie uda�o si� otworzy� strumienia esd: %s\n"

// ao_mpegpes.c
#define MSGTR_AO_MPEGPES_CantSetMixer "[AO MPEGPES] Ustawienie miksera DVB nie powiod�o si�: %s\n" 
#define MSGTR_AO_MPEGPES_UnsupSamplerate "[AO MPEGPES] %d Hz nie obs�ugiwana, spr�buj resamplowa�...\n"

// ao_null.c
// This one desn't even  have any mp_msg nor printf's?? [CHECK]

// ao_pcm.c
#define MSGTR_AO_PCM_FileInfo "[AO PCM] Plik: %s (%s)\nPCM: Cz�stotliwo�� pr�bkowamia: %iHz Kana�y: %s Format %s\n"
#define MSGTR_AO_PCM_HintInfo "[AO PCM] Info: najszybsze zrzucanie jest osi�gane poprzez -vc dummy -vo null\nPCM: Info: aby zapisa� plik WAVE u�yj -waveheader (default)."
#define MSGTR_AO_PCM_CantOpenOutputFile "[AO PCM] Nie powiod�o si� otwarcie %s do zapisu!\n"

// ao_sdl.c
#define MSGTR_AO_SDL_INFO "[AO SDL] Cz�stotliwo�� pr�bkowania: %iHz Kana�y: %s Format %s\n"
#define MSGTR_AO_SDL_DriverInfo "[AO SDL] u�ywam sterownika audio %s.\n"
#define MSGTR_AO_SDL_UnsupportedAudioFmt "[AO SDL] Nieobs�ugiwany format d�wi�ku: 0x%x.\n"
#define MSGTR_AO_SDL_CantInit "[AO SDL] Inicjalizacja SDL Audio nie powiod�a si�: %s\n"
#define MSGTR_AO_SDL_CantOpenAudio "[AO SDL] Nie mo�na otworzy� audio: %s\n"

// ao_sgi.c
#define MSGTR_AO_SGI_INFO "[AO SGI] - kontrola.\n"
#define MSGTR_AO_SGI_InitInfo "[AO SGI] init: Cz�stotliwo�� pr�bkowania: %iHz Kana�y: %s Format %s\n"
#define MSGTR_AO_SGI_InvalidDevice "[AO SGI] play: niepoprawne urz�dzenie.\n"
#define MSGTR_AO_SGI_CantSetParms_Samplerate "[AO SGI] init: setparams zawi�d�: %s\nNie mo�na ustawi� po��danej cz�stotliwo�ci pr�bkowania.\n"
#define MSGTR_AO_SGI_CantSetAlRate "[AO SGI] init: AL_RATE nie zosta� zakceptowany przy podanym �r�dle.\n"
#define MSGTR_AO_SGI_CantGetParms "[AO SGI] init: getparams zawi�d�: %s\n"
#define MSGTR_AO_SGI_SampleRateInfo "[AO SGI] init: cz�stotliwo�� pr�bkowania ustawiona na %lf (po��dana skala to %lf)\n"
#define MSGTR_AO_SGI_InitConfigError "[AO SGI] init: %s\n"
#define MSGTR_AO_SGI_InitOpenAudioFailed "[AO SGI] init: Nie mo�na otworzy� kana�u audio: %s\n"
#define MSGTR_AO_SGI_Uninit "[AO SGI] uninit: ...\n"
#define MSGTR_AO_SGI_Reset "[AO SGI] reset: ...\n"
#define MSGTR_AO_SGI_PauseInfo "[AO SGI] audio_pause: ...\n"
#define MSGTR_AO_SGI_ResumeInfo "[AO SGI] audio_resume: ...\n"

// ao_sun.c
#define MSGTR_AO_SUN_RtscSetinfoFailed "[AO SUN] rtsc: SETINFO zawi�d�.\n"
#define MSGTR_AO_SUN_RtscWriteFailed "[AO SUN] rtsc: zapis nie powi�d� si�."
#define MSGTR_AO_SUN_CantOpenAudioDev "[AO SUN] Nie mo�na otworzy� urz�dzenia audio %s, %s  -> brak d�wi�ku.\n"
#define MSGTR_AO_SUN_UnsupSampleRate "[AO SUN] audio_setup: Twoja karta nie obs�uguje %d kana�ul, %s, %d Hz samplerate.\n"
#define MSGTR_AO_SUN_CantUseSelect "[AO SUN]\n   ***  Tw�j sterownik d�wi�ku NIE obs�uguje select()  ***\n Przekompiluj mplayera z #undef HAVE_AUDIO_SELECT w config.h !\n\n"
#define MSGTR_AO_SUN_CantReopenReset "[AO OSS]\nB��d krytyczny: *** NIE MO�NA PONOWNIE OTWORZY� / ZRESETOWA� URZ�DZENIA AUDIO *** %s\n"

// ao_alsa5.c
#define MSGTR_AO_ALSA5_InitInfo "[AO ALSA5] alsa-init: ��dany format: %d Hz, %d kana�y, %s\n"
#define MSGTR_AO_ALSA5_SoundCardNotFound "[AO ALSA5] alsa-init: nie znaleziono �adnych kart d�wi�kowych.\n"
#define MSGTR_AO_ALSA5_InvalidFormatReq "[AO ALSA5] alsa-init: nieprawid�owy format (%s) ��dany - wyj�cie wy��czone.\n"
#define MSGTR_AO_ALSA5_PlayBackError "[AO ALSA5] alsa-init: B��d przy odtwarzaniu: %s\n"
#define MSGTR_AO_ALSA5_PcmInfoError "[AO ALSA5] alsa-init: b��d informacji pcm: %s\n"
#define MSGTR_AO_ALSA5_SoundcardsFound "[AO ALSA5] alsa-init: znaleziono %d kart d�wi�kowych, u�ywam: %s\n"
#define MSGTR_AO_ALSA5_PcmChanInfoError "[AO ALSA5] alsa-init: b��d informacji kana�u pcm: %s\n"
#define MSGTR_AO_ALSA5_CantSetParms "[AO ALSA5] alsa-init: b�ad przy ustawianiu parametru: %s\n"
#define MSGTR_AO_ALSA5_CantSetChan "[AO ALSA5] alsa-init: b��d przy ustawianiu kana�u: %s\n"
#define MSGTR_AO_ALSA5_ChanPrepareError "[AO ALSA5] alsa-init: b��d przy przygotowywaniu kana�u: %s\n"
#define MSGTR_AO_ALSA5_DrainError "[AO ALSA5] alsa-uninit: b��d przy ods�czaniu odtwarzania: %s\n"
//[FIXME] heheh jakies propoycje
#define MSGTR_AO_ALSA5_FlushError "[AO ALSA5] alsa-uninit: b��d przy wy��czaniu odtwarzania: %s\n"
#define MSGTR_AO_ALSA5_PcmCloseError "[AO ALSA5] alsa-uninit: b��d przy zamykaniu pcm: %s\n"
#define MSGTR_AO_ALSA5_ResetDrainError "[AO ALSA5] alsa-reset: b��d przy ods�czaniu odtwarzania: %s\n"
#define MSGTR_AO_ALSA5_ResetFlushError "[AO ALSA5] alsa-reset: b��d przy wy��czaniu odtwarzania: %s\n"
#define MSGTR_AO_ALSA5_ResetChanPrepareError "[AO ALSA5] alsa-reset: b��d przy przygotowywaniu kana�u: %s\n"
#define MSGTR_AO_ALSA5_PauseDrainError "[AO ALSA5] alsa-pause: b��d przy ods�czaniu odtwarzania: %s\n"
#define MSGTR_AO_ALSA5_PauseFlushError "[AO ALSA5] alsa-pause: b��d przy wy��czaniu odtwarzania: %s\n"
#define MSGTR_AO_ALSA5_ResumePrepareError "[AO ALSA5] alsa-resume: b��d przy przygotowywaniu kana�u: %s\n"
#define MSGTR_AO_ALSA5_Underrun "[AO ALSA5] alsa-play: b��d alsa, resetuj� strumie�.\n"
#define MSGTR_AO_ALSA5_PlaybackPrepareError "[AO ALSA5] alsa-play: b��d przy przytowywaniu odtwarzania: %s\n"
#define MSGTR_AO_ALSA5_WriteErrorAfterReset "[AO ALSA5] alsa-play: b��� zapisu po resecie: %s - poddaje si�.\n"
#define MSGTR_AO_ALSA5_OutPutError "[AO ALSA5] alsa-play: b��d wyj�cia: %s\n"


// ao_plugin.c

#define MSGTR_AO_PLUGIN_InvalidPlugin "[AO PLUGIN] nieprawid�owa wtyczka: %s\n"

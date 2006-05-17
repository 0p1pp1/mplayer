// Translated by:  Daniel Be�a, benad (at) centrum.cz
// last sync on 2006-04-28 with 1.249
// but not compleated

// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
// Preklad do sloven�iny 

static char help_text[]=
"Pou�itie:   mplayer [prep�na�e] [url|cesta/]menos�boru\n"
"\n"
"Z�kladn� prep�na�e: (Kompletn� zoznam n�jdete v man str�nke)\n"
" -vo <drv[:dev]> v�ber v�stup. video ovl�da�a&zariadenia (-vo help pre zoznam)\n"
" -ao <drv[:dev]> v�ber v�stup. audio ovl�da�a&zariadenia (-ao help pre zoznam)\n"
#ifdef HAVE_VCD
" vcd://<trackno>  prehra� VCD (video cd) stopu zo zariadenia namiesto zo s�boru\n"
#endif
#ifdef USE_DVDREAD
" dvd://<titleno>  prehra� DVD titul/stopu zo zariadenia (mechaniky) namiesto s�boru\n"
" -alang/-slang   vybra� jazyk DVD zvuku/titulkov(pomocou 2-miest. k�du krajiny)\n"
#endif
" -ss <timepos>   posun na poz�ciu (sekundy alebo hh:mm:ss)\n"
" -nosound        prehr�va� bez zvuku\n"
" -fs             vo�by pre cel� obrazovku (alebo -vm -zoom, detaily vi�. man str�nku)\n"
" -x <x> -y <y>   zv��enie obrazu na rozmer <x>*<y> (pokia� to vie -vo ovl�da�!)\n"
" -sub <file>     vo�ba s�boru s titulkami (vi� tie� -subfps, -subdelay)\n"
" -playlist <file> ur�enie s�boru so zoznamom prehr�van�ch s�borov\n"
" -vid x -aid y   v�ber ��sla video (x) a audio (y) pr�du pre prehr�vanie\n"
" -fps x -srate y vo�ba pre zmenu video (x fps) a audio (y Hz) frekvencie\n"
" -pp <quality>   aktiv�cia postprocesing filtra (0-4 pre DivX, 0-63 pre mpegy)\n"
" -framedrop      povoli� zahadzovanie sn�mkov (pre pomal� stroje)\n"
"\n"
"Z�kl. kl�vesy:   (pre kompl. pozrite aj man str�nku a input.conf)\n"
" <-  alebo  ->   posun vzad/vpred o 10 sekund\n"
" hore / dole     posun vzad/vpred o  1 min�tu\n"
" pgup alebo pgdown  posun vzad/vpred o 10 min�t\n"
" < alebo >       posun vzad/vpred v zozname prehr�van�ch s�borov\n"
" p al. medzern�k pauza (pokra�ovanie stla�en�m kl�vesy)\n"
" q alebo ESC     koniec prehr�vania a ukon�enie programu\n"
" + alebo -       upravi� spozdenie zvuku v krokoch +/- 0.1 sekundy\n"
" o               cyklick� zmena re�imu OSD:  ni� / poz�cia / poz�cia+�as\n"
" * alebo /       prida� alebo ubra� hlasitos� (stla�en�m 'm' v�ber master/pcm)\n"
" z alebo x       upravi� spozdenie titulkov v krokoch +/- 0.1 sekundy\n"
" r alebo t       upravi� poz�ciu titulkov hore/dole, pozrite tie� -vf!\n"
"\n"
" * * * * PRE��TAJTE SI MAN STR�NKU PRE DETAILY (�AL�IE VO�BY A KL�VESY)! * * * *\n"
"\n";
#endif

#define MSGTR_SamplesWanted "Potrebujeme vzorky tohto form�tu, aby sme zlep�ili podporu. Pros�m kontaktujte v�voj�rov.\n"

// ========================= MPlayer messages ===========================
// mplayer.c:

#define MSGTR_Exiting "\nKon��m...\n"
#define MSGTR_ExitingHow "\nKon��m... (%s)\n"
#define MSGTR_Exit_quit "Koniec"
#define MSGTR_Exit_eof "Koniec s�boru"
#define MSGTR_Exit_error "Z�va�n� chyba"
#define MSGTR_IntBySignal "\nMPlayer preru�en� sign�lom %d v module: %s \n"
#define MSGTR_NoHomeDir "Nem��em najs� dom�ci (HOME) adres�r\n"
#define MSGTR_GetpathProblem "get_path(\"config\") probl�m\n"
#define MSGTR_CreatingCfgFile "Vytv�ram konfigura�n� s�bor: %s\n"
#define MSGTR_CopyCodecsConf "(copy/ln etc/codecs.conf (zo zdrojov�ch k�dov MPlayeru) do ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "Pou��vam vstavan� defaultne codecs.conf\n"
#define MSGTR_CantLoadFont "Nem��em na��ta� font: %s\n"
#define MSGTR_CantLoadSub "Nem��em na��ta� titulky: %s\n"
#define MSGTR_DumpSelectedStreamMissing "dump: FATAL: po�adovan� pr�d ch�ba!\n"
#define MSGTR_CantOpenDumpfile "Nejde otvori� s�bor pre dump!!!\n"
#define MSGTR_CoreDumped "jadro vyp�san� :)\n"
#define MSGTR_FPSnotspecified "V hlavi�ke s�boru nie je udan� (alebo je zl�) FPS! Pou�ite vo�bu -fps!\n"
#define MSGTR_TryForceAudioFmtStr "Pok��am sa vyn�ti� rodinu audiokodeku %s...\n"
#define MSGTR_CantFindAudioCodec "Nem��em n�js� kodek pre audio form�t 0x%X!\n"
#define MSGTR_RTFMCodecs "Pre��tajte si DOCS/HTML/en/codecs.html!\n"
#define MSGTR_TryForceVideoFmtStr "Pok��am se vn�ti� rodinu videokodeku %s...\n"
#define MSGTR_CantFindVideoCodec "Nem��em najs� kodek pre video form�t 0x%X!\n"
#define MSGTR_CannotInitVO "FATAL: Nem��em inicializova� video driver!\n"
#define MSGTR_CannotInitAO "nem��em otvori�/inicializova� audio driver -> TICHO\n"
#define MSGTR_StartPlaying "Za��nam prehr�va�...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"         ***********************************************************\n"\
"         ****  Na prehratie tohoto je v� syst�m pr�li� POMAL�!  ****\n"\
"         ***********************************************************\n"\
"!!! Mo�n� pr��iny, probl�my a rie�enia:\n"\
"- Nej�astej�ie: nespr�vny/chybn� _zvukov�_ ovl�da�.\n"\
"  - Sk�ste -ao sdl alebo pou�ite OSS emul�ciu ALSA.\n"\
"  - Experimentujte s r�znymi hodnotami -autosync, 30 je dobr� za�iatok.\n"\
"- Pomal� video v�stup\n"\
"  - Sk�ste in� -vo ovl�da� (pre zoznam: -vo help) alebo sk�ste -framedrop!\n"\
"- Pomal� CPU\n"\
"  - Nesk��ajte prehr�va� ve�k� dvd/divx na pomalom cpu! Sk�ste lavdopts,\n"\
"    napr. -vfm ffmpeg -lavdopts lowres=1:fast:skiploopfilter=all.\n"\
"- Po�koden� s�bor\n"\
"  - Sk�ste r�zne kombin�cie t�chto volieb -nobps -ni -forceidx -mc 0.\n"\
"- Pomal� m�dium (NFS/SMB, DVD, VCD...)\n"\
"  - Sk�ste -cache 8192.\n"\
"- Pou��vate -cache na prehr�vanie non-interleaved s�boru?\n"\
"  - Sk�ste -nocache.\n"\
"Pre��tajte si DOCS/HTML/en/video.html s� tam tipy na vyladenie/zr�chlenie.\n"\
"Ak ni� z tohto nepomohlo, pre��tajte si DOCS/HTML/en/bugreports.html.\n\n"

#define MSGTR_NoGui "MPlayer bol zostaven� BEZ podpory GUI!\n"
#define MSGTR_GuiNeedsX "MPlayer GUI vy�aduje X11!\n"
#define MSGTR_Playing "Prehr�vam %s\n"
#define MSGTR_NoSound "Audio: bez zvuku!!!\n"
#define MSGTR_FPSforced "FPS vn�ten� na hodnotu %5.3f  (ftime: %5.3f)\n"
#define MSGTR_CompiledWithRuntimeDetection "Skompilovn� s RUNTIME CPU Detection - varovanie, nie je to optim�lne! Na z�skanie max. v�konu, rekompilujte mplayer zo zdrojakov s --disable-runtime-cpudetection\n"
#define MSGTR_CompiledWithCPUExtensions "Skompilovan� pre x86 CPU s roz��reniami:"
#define MSGTR_AvailableVideoOutputDrivers "Dostupn� video v�stupn� ovl�da�e:\n"
#define MSGTR_AvailableAudioOutputDrivers "Dostupn� audio v�stupn� ovl�da�e:\n"
#define MSGTR_AvailableAudioCodecs "Dostupn� audio kodeky:\n"
#define MSGTR_AvailableVideoCodecs "Dostupn� video kodeky:\n"
#define MSGTR_AvailableAudioFm "Dostupn� (vkompilovan�) audio rodiny kodekov/ovl�da�e:\n"
#define MSGTR_AvailableVideoFm "Dostupn� (vkompilovan�) video rodiny kodekov/ovl�da�e:\n"
#define MSGTR_AvailableFsType "Dostupn� zmeny plnoobrazovkov�ch m�dov:\n"
#define MSGTR_UsingRTCTiming "Pou��vam Linuxov� hardv�rov� RTC �asovanie (%ldHz)\n"
#define MSGTR_CannotReadVideoProperties "Video: nem��em ��ta� vlastnosti\n"
#define MSGTR_NoStreamFound "Nen�jden� pr�d\n"
#define MSGTR_ErrorInitializingVODevice "Chyba pri otv�ran�/inicializ�cii vybran�ch video_out (-vo) zariaden�!\n"
#define MSGTR_ForcedVideoCodec "Vn�ten� video kodek: %s\n"
#define MSGTR_ForcedAudioCodec "Vn�ten� video kodek: %s\n"
#define MSGTR_Video_NoVideo "Video: �iadne video!!!\n"
#define MSGTR_NotInitializeVOPorVO "\nFATAL: Nem��em inicializova� video filtre (-vf) alebo video v�stup (-vo)!\n"
#define MSGTR_Paused "\n  =====  PAUZA  =====\r"
#define MSGTR_PlaylistLoadUnable "\nNem��em na��ta� playlist %s\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- MPlayer zhavaroval na 'Illegal Instruction'.\n"\
"  M��e to by� chyba v na�om novom k�de na detekciu procesora...\n"\
"  Pros�m pre��tajte si DOCS/HTML/en/bugreports.html.\n"
#define MSGTR_Exit_SIGILL \
"- MPlayer zhavaroval na 'Illegal Instruction'.\n"\
"  Oby�ajne sa to st�va, ke� ho pou��vate na inom procesore ako pre ktor� bol\n"\
"  skompilovan�/optimalizovan�.\n"\
"  Skontrolujte si to!\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- MPlayer zhavaroval nespr�vnym pou�it�m CPU/FPU/RAM.\n"\
"  Prekompilujte MPlayer s --enable-debug a urobte 'gdb' backtrace a\n"\
"  disassemblujte. Pre detaily, pozrite DOCS/HTML/en/bugreports_what.html#bugreports_crash.b.\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer zhavaroval. To sa nemalo sta�.\n"\
"  M��e to by� chyba v MPlayer k�de _alebo_ vo Va��ch ovl�da�och _alebo_ gcc\n"\
"  verzii. Ak si mysl�te, �e je to chyba MPlayeru, pros�m pre��tajte si DOCS/HTML/en/bugreports.html\n"\
"  a postupujte pod�a in�trukcii. Nem��eme V�m pom�c�, pokia� neposkytnete\n"\
"  tieto inform�cie pri ohlasovan� mo�nej chyby.\n"
#define MSGTR_LoadingConfig "��tam konfigur�ciu '%s'\n"
#define MSGTR_AddedSubtitleFile "SUB: pridan� s�bor titulkov (%d): %s\n"
#define MSGTR_RemovedSubtitleFile "SUB: odobrat� s�bor titulkov (%d): %s\n"
#define MSGTR_ErrorOpeningOutputFile "Chyba pri otv�ran� s�boru [%s] pre z�pis!\n"
#define MSGTR_CommandLine "Pr�kazov� riadok:"
#define MSGTR_RTCDeviceNotOpenable "Nepodarilo sa otvori� %s: %s (malo by to by� �itate�n� pre pou��vate�a.)\n"
#define MSGTR_LinuxRTCInitErrorIrqpSet "Chyba pri inicializ�cii Linuxov�ch RTC v ioctl (rtc_irqp_set %lu): %s\n"
#define MSGTR_IncreaseRTCMaxUserFreq "Sk�ste prida� \"echo %lu > /proc/sys/dev/rtc/max-user-freq\" do �tartovac�ch skriptov v�ho syst�mu.\n"
#define MSGTR_LinuxRTCInitErrorPieOn "Chyba pri inicializ�cii Linuxov�ch RTC v ioctl (rtc_pie_on): %s\n"
#define MSGTR_UsingTimingType "Pou��vam %s �asovanie.\n"
#define MSGTR_NoIdleAndGui "Vo�ba -idle sa ned� pou�i� pre GMPlayer.\n"
#define MSGTR_MenuInitialized "Menu inicializovan�: %s\n"
#define MSGTR_MenuInitFailed "Zlyhala inicializ�cia menu.\n"
#define MSGTR_Getch2InitializedTwice "VAROVANIE: getch2_init je volan� dvakr�t!\n"
#define MSGTR_DumpstreamFdUnavailable "Nem��em ulo�i� (dump) tento pr�d - nie je dostupn� �iaden deskriptor s�boru.\n"
#define MSGTR_FallingBackOnPlaylist "Ustupujem od pokusu o spracovanie playlistu %s...\n"
#define MSGTR_CantOpenLibmenuFilterWithThisRootMenu "Nem��em otvori� video filter libmenu s kore�ov�m menu %s.\n"
#define MSGTR_AudioFilterChainPreinitError "Chyba pri predinicializ�cii re�azca audio filtrov!\n"
#define MSGTR_LinuxRTCReadError "Chyba pri ��tan� z Linuxov�ch RTC: %s\n"
#define MSGTR_SoftsleepUnderflow "Pozor! Podte�enie softsleep!\n"
#define MSGTR_DvdnavNullEvent "DVDNAV udalos� NULL?!\n"
#define MSGTR_DvdnavHighlightEventBroken "DVDNAV udalos�: Vadn� zv�raznenie udalost�\n"
#define MSGTR_DvdnavEvent "DVDNAV udalos�: %s\n"
#define MSGTR_DvdnavHighlightHide "DVDNAV udalos�: skry� zv�raznenie\n"
#define MSGTR_DvdnavStillFrame "######################################## DVDNAV udalos�: Stojac� sn�mok: %d sec(s)\n"
#define MSGTR_DvdnavNavStop "DVDNAV udalos�: Nav Stop\n"
#define MSGTR_DvdnavNavNOP "DVDNAV udalos�: Nav NOP\n"
#define MSGTR_DvdnavNavSpuStreamChangeVerbose "DVDNAV udalos�: Zmena Nav SPU pr�du: fyz: %d/%d/%d logicky: %d\n"
#define MSGTR_DvdnavNavSpuStreamChange "DVDNAV udalos�: Zmena Nav SPU pr�du: fyz: %d logicky: %d\n"
#define MSGTR_DvdnavNavAudioStreamChange "DVDNAV udalos�: Zmena Nav Audio pr�du: fyz: %d logicky: %d\n"
#define MSGTR_DvdnavNavVTSChange "DVDNAV udalos�: Zmena Nav VTS\n"
#define MSGTR_DvdnavNavCellChange "DVDNAV udalos�: Zmena Nav bunky \n"
#define MSGTR_DvdnavNavSpuClutChange "DVDNAV udalos�: Zmena Nav SPU CLUT\n"
#define MSGTR_DvdnavNavSeekDone "DVDNAV udalos�: Prev�janie Nav dokon�en�\n"
#define MSGTR_MenuCall "Volanie menu\n"

#define MSGTR_EdlOutOfMem "Ned� sa alokova� dostatok pam�te pre EDL d�ta.\n"
#define MSGTR_EdlRecordsNo "��tam %d EDL akcie.\n"
#define MSGTR_EdlQueueEmpty "V�etky EDL akcie boly u� vykonan�.\n"
#define MSGTR_EdlCantOpenForWrite "Ned� sa otvori� EDL s�bor [%s] pre z�pis.\n"
#define MSGTR_EdlCantOpenForRead "Ned� sa otvori� EDL s�bor [%s] na ��tanie.\n"
#define MSGTR_EdlNOsh_video "EDL sa ned� pou�i� bez videa, vyp�nam.\n"
#define MSGTR_EdlNOValidLine "Chyba EDL na riadku: %s\n"
#define MSGTR_EdlBadlyFormattedLine "Zle form�tovan� EDL riadok [%d] Zahadzujem.\n"
#define MSGTR_EdlBadLineOverlap "Posledn� stop zna�ka bola [%f]; �al�� �tart je "\
"[%f]. Z�znamy musia by� chronologicky, a nesm� sa prekr�va�. Zahadzujem.\n"
#define MSGTR_EdlBadLineBadStop "�asov� zna�ka stop m� by� za zna�kou start.\n"

// mplayer.c OSD

#define MSGTR_OSDenabled "zapnut�"
#define MSGTR_OSDdisabled "vypnut�"
#define MSGTR_OSDChannel "Kan�l: %s"
#define MSGTR_OSDSubDelay "Zpozdenie tit: %d ms"
#define MSGTR_OSDSpeed "R�chlos�: x %6.2f"
#define MSGTR_OSDosd "OSD: %s"

// property values
#define MSGTR_Enabled "zapnut�"
#define MSGTR_EnabledEdl "zapnut� (edl)"
#define MSGTR_Disabled "vypnut�"
#define MSGTR_HardFrameDrop "hard"
#define MSGTR_Unknown "nezn�me"
#define MSGTR_Bottom "dole"
#define MSGTR_Center "stred"
#define MSGTR_Top "hore"

// osd bar names
#define MSGTR_Volume "Hlasitos�"
#define MSGTR_Panscan "Panscan"
#define MSGTR_Gamma "Gama"
#define MSGTR_Brightness "Jas"
#define MSGTR_Contrast "Kontrast"
#define MSGTR_Saturation "S�tos�"
#define MSGTR_Hue "T�n"

// property state
#define MSGTR_MuteStatus "Utlmenie zvuku: %s"
#define MSGTR_AVDelayStatus "A-V odchylka: %s"
#define MSGTR_OnTopStatus "V�dy navrchu: %s"
#define MSGTR_RootwinStatus "Hlavn� okno: %s"
#define MSGTR_BorderStatus "Ohrani�enie: %s"
#define MSGTR_FramedroppingStatus "Zahadzovanie sn�mkov: %s"
#define MSGTR_VSyncStatus "VSync: %s"
#define MSGTR_SubSelectStatus "Titulky: %s"
#define MSGTR_SubPosStatus "Poz�cia tit.: %s/100"
#define MSGTR_SubAlignStatus "Zarovnanie tit.: %s"
#define MSGTR_SubDelayStatus "Spozdenie tit.: %s"
#define MSGTR_SubVisibleStatus "Zobr. titulkov: %s"
#define MSGTR_SubForcedOnlyStatus "Iba vyn�ten� tit.: %s"

// mencoder.c:

#define MSGTR_UsingPass3ControlFile "Pou��vam pass3 ovl�dac� s�bor: %s\n"
#define MSGTR_MissingFilename "\nCh�baj�ce meno s�boru.\n\n"
#define MSGTR_CannotOpenFile_Device "Nem��em otvori� s�bor/zariadenie\n"
#define MSGTR_CannotOpenDemuxer "Nem��em otvori� demuxer\n"
#define MSGTR_NoAudioEncoderSelected "\nNevybran� encoder (-oac)! Vyberte jeden alebo -nosound. Pou�itie -oac help!\n"
#define MSGTR_NoVideoEncoderSelected "\nNevybran� encoder (-ovc)! Vyberte jeden, pou�itie -ovc help!\n"
#define MSGTR_CannotOpenOutputFile "Nem��em otvori� s�bor '%s'\n"
#define MSGTR_EncoderOpenFailed "Zlyhalo spustenie enk�deru\n"
#define MSGTR_MencoderWrongFormatAVI "\nVAROVANIE: FORM�T V�STUPN�HO S�BORU JE _AVI_. viz -of help.\n"
#define MSGTR_MencoderWrongFormatMPG "\nVAROVANIE: FORM�T V�STUPN�HO S�BORU JE _MPEG_. viz -of help.\n"
#define MSGTR_MissingOutputFilename "Nebol nastaven� v�stupn� s�bor, pre�tudujte si volbu -o"
#define MSGTR_ForcingOutputFourcc "Vnucujem v�stupn� form�t (fourcc) na %x [%.4s]\n"
#define MSGTR_ForcingOutputAudiofmtTag "Vynucujem zna�ku v�stupn�ho zvukov�ho form�tu 0x%x\n"
#define MSGTR_DuplicateFrames "\nduplikujem %d sn�mkov!!!    \n"
#define MSGTR_SkipFrame "\npresko�i� sn�mok!!!    \n"
#define MSGTR_ResolutionDoesntMatch "\nNov� video s�bor m� in� rozli�en� alebo farebn� priestor ako jeho predchodca.\n"
#define MSGTR_FrameCopyFileMismatch "\nV�etky video soubory mus� m�t shodn� fps, rozli�en� a kodek pro -ovc copy.\n"
#define MSGTR_AudioCopyFileMismatch "\nV�etky s�bory mus� pou��va� identick� audio k�dek a form�t pro -oac copy.\n"
#define MSGTR_NoAudioFileMismatch "\nNem��ete mixova� iba video s audio a video s�bormi. Sk�ste -nosound.\n"
#define MSGTR_NoSpeedWithFrameCopy "VAROVANIE: -speed nem� zaru�en� funk�nos� s -oac copy!\n"\
"V�sledny s�bor m��e by� vadn�!\n"
#define MSGTR_ErrorWritingFile "%s: chyba pri z�pise s�boru.\n"
#define MSGTR_RecommendedVideoBitrate "Odpor��an� d�tov� tok videa pre CD %s: %d\n"
#define MSGTR_VideoStreamResult "\nVideo pr�d: %8.3f kbit/s  (%d B/s)  velkos�: %"PRIu64" bytov  %5.3f sekund  %d sn�mkov\n"
#define MSGTR_AudioStreamResult "\nAudio pr�d: %8.3f kbit/s  (%d B/s)  velkos�: %"PRIu64" bytov  %5.3f sekund\n"
#define MSGTR_OpenedStream "�spech: form�t: %d  d�ta: 0x%X - 0x%x\n"
#define MSGTR_VCodecFramecopy "videok�dek: framecopy (%dx%d %dbpp fourcc=%x)\n"
#define MSGTR_ACodecFramecopy "audiok�dek: framecopy (form�t=%x kan�lov=%d frekvencia=%d bitov=%d B/s=%d vzorka-%d)\n"
#define MSGTR_CBRPCMAudioSelected "zvolen� CBR PCM zvuk\n"
#define MSGTR_MP3AudioSelected "zvolen� MP3 zvuk\n"
#define MSGTR_CannotAllocateBytes "Ned� sa alokova� %d bajtov\n"
#define MSGTR_SettingAudioDelay "Nastavujem spozdenie zvuku na %5.3f\n"
#define MSGTR_SettingVideoDelay "Nastavujem spozd�nie videa na %5.3fs\n"
#define MSGTR_SettingAudioInputGain "Nastavujem predzosilnenie zvukov�ho vstupu na %f\n"
#define MSGTR_LamePresetEquals "\npreset=%s\n\n"
#define MSGTR_LimitingAudioPreload "Obmedzujem predna��tanie zvuku na 0.4s\n"
#define MSGTR_IncreasingAudioDensity "Zvy�ujem hustotu audia na 4\n"
#define MSGTR_ZeroingAudioPreloadAndMaxPtsCorrection "Vnucujem predna��tanie zvuku na 0, max korekciu pts na 0\n"
#define MSGTR_CBRAudioByterate "\n\nCBR zvuk: %d bajtov/s, %d bajtov/blok\n"
#define MSGTR_LameVersion "LAME verzia %s (%s)\n\n"
#define MSGTR_InvalidBitrateForLamePreset "Chyba: �pecifikovan� d�tov� tok je mimo rozsah pre tento preset.\n"\
"\n"\
"Pokia� pou��vate tento re�im, mus�te zadat hodnotu od \"8\" do \"320\".\n"\
"\n"\
"Dal�ie inform�cie viz: \"-lameopts preset=help\"\n"
#define MSGTR_InvalidLamePresetOptions "Chyba: Nezadali ste platn� profil a/alebo vo�by s presetom.\n"\
"\n"\
"Dostupn� profily s�:\n"\
"\n"\
"   <fast>        standard\n"\
"   <fast>        extreme\n"\
"                 insane\n"\
"   <cbr> (ABR Mode) - Implikuje re�im ABR. Pre jeho pou�itie,\n"\
"                      jednoduche zadejte d�tov� tok. Napr�klad:\n"\
"                      \"preset=185\" aktivuje tento preset\n"\
"                      a pou�ije priemern� d�tov� tok 185 kbps.\n"\
"\n"\
"    Niekolko pr�kladov:\n"\
"\n"\
"    \"-lameopts fast:preset=standard  \"\n"\
" or \"-lameopts  cbr:preset=192       \"\n"\
" or \"-lameopts      preset=172       \"\n"\
" or \"-lameopts      preset=extreme   \"\n"\
"\n"\
"Dal�ie inform�cie viz: \"-lameopts preset=help\"\n"
#define MSGTR_LamePresetsLongInfo "\n"\
"Preset prep�na�e s� navrhnut� tak, aby poskytovaly �o najvy��iu mo�n� kvalitu.\n"\
"\n"\
"V��ina z nich bola testovan� a vyladen� pomocou d�kladn�ch zdvojen�ch slep�ch\n"\
"posluchov�ch testov, za ��elom dosiahnutia a overenia tohto ciela.\n"\
"\n"\
"Nastavenia s� neust�le aktualizovan� v s�lade s najnov��m v�vojom\n"\
"a mali by poskytova� prakticky najvy��iu mo�n� kvalitu, ak� je v s��asnosti \n"\
"s k�dekom LAME dosa�ite�n�.\n"\
"\n"\
"Aktiv�cia presetov:\n"\
"\n"\
"   Pre re�imy VBR (vo v�eobecnosti najvy��ia kvalita):\n"\
"\n"\
"     \"preset=standard\" Tento preset by mal b�� jasnou vo�bou\n"\
"                             pre v��inu lud� a hudobn�ch ��nrov a m�\n"\
"                             u� vysok� kvalitu.\n"\
"\n"\
"     \"preset=extreme\" Pokia� m�te v�nimo�ne dobr� sluch a zodpovedaj�ce\n"\
"                             vybavenie, tento preset vo v�eob. poskytuje\n"\
"                             miern� vy��� kvalitu ako re�im \"standard\".\n"\
"\n"\
"   Pre CBR 320kbps (najvy��ia mo�n� kvalita ze v�etk�ch presetov):\n"\
"\n"\
"     \"preset=insane\"  Tento preset je pre v��inu lud� a situ�cii\n"\
"                             predimenzovan�, ale pokia� vy�adujete\n"\
"                             absolutne najvy��iu kvalitu bez oh�adu na\n"\
"                             velkos� s�boru, je toto va�a vo�ba.\n"\
"\n"\
"   Pre re�imy ABR (vysok� kvalita pri danom d�tov�m toku, ale nie tak ako VBR):\n"\
"\n"\
"     \"preset=<kbps>\"  Pou�it�m tohoto presetu obvykle dosiahnete dobr�\n"\
"                             kvalitu pri danom d�tov�m toku. V z�vislosti\n"\
"                             na zadanom toku tento preset odvod� optim�lne\n"\
"                             nastavenie pre dan� situ�ciu.\n"\
"                             Hoci tento pr�stup funguje, nie je ani z�aleka\n"\
"                             tak flexibiln� ako VBR, a obvykle nedosahuje\n"\
"                             �rovne kvality ako VBR na vy���ch d�tov�ch tokoch.\n"\
"\n"\
"Pre zodpovedaj�ce profily s� k dispoz�cii tie� nasleduj�ce vo�by:\n"\
"\n"\
"   <fast>        standard\n"\
"   <fast>        extreme\n"\
"                 insane\n"\
"   <cbr> (ABR re�im) - Implikuje re�im ABR. Pre jeho pou�itie\n"\
"                      jednoducho zadajte d�tov� tok. Napr�klad:\n"\
"                      \"preset=185\" aktivuje tento preset\n"\
"                      a pou�ije priemern� d�tov� tok 185 kbps.\n"\
"\n"\
"   \"fast\" - V danom profile aktivuje nov� r�chlu VBR kompresiu.\n"\
"            Nev�hodou je obvykle mierne vy��� d�tov� tok ako v norm�lnom\n"\
"            re�ime a tie� m��e d�js� k miernemu poklesu kvality.\n"\
"   Varovanie:v aktu�lnej verzi m��e nastavenie \"fast\" vies� k pr�li�\n"\
"            vysok�mu d�tov�mu toku v porovnan� s norm�lnym nastaven�m.\n"\
"\n"\
"   \"cbr\"  - Pokia� pou�ijete re�im ABR (viz vy��ie) s v�znamn�m\n"\
"            d�tov�m tokom, napr. 80, 96, 112, 128, 160, 192, 224, 256, 320,\n"\
"            m��ete pou�� vo�bu \"cbr\" k vn�teniu k�dov�nia v re�ime CBR\n"\
"            (kon�tantn� tok) namiesto �tandardn�ho ABR re�imu. ABR poskytuje\n"\
"            lep�iu kvalitu, ale CBR m��e by� u�ito�n� v situ�ciach ako je\n"\
"            vysielanie mp3 pr�du po internete.\n"\
"\n"\
"    Napr�klad:\n"\
"\n"\
"      \"-lameopts fast:preset=standard  \"\n"\
" alebo \"-lameopts  cbr:preset=192       \"\n"\
" alebo \"-lameopts      preset=172       \"\n"\
" alebo \"-lameopts      preset=extreme   \"\n"\
"\n"\
"\n"\
"Pre ABR re�im je k dispoz�cii niekolko skratiek:\n"\
"phone => 16kbps/mono        phon+/lw/mw-eu/sw => 24kbps/mono\n"\
"mw-us => 40kbps/mono        voice => 56kbps/mono\n"\
"fm/radio/tape => 112kbps    hifi => 160kbps\n"\
"cd => 192kbps               studio => 256kbps"
#define MSGTR_LameCantInit "Ned� sa nastavi� vo�ba pre LAME, overte d�tov�_tok/vzorkovaciu_frekv.,"\
"niektor� ve�mi n�zke d�tov� toky (<32) vy�aduj� ni��iu vzorkovaciu frekv. (napr. -srate 8000)."\
"Pokud v�etko ostan� zlyh�, zk�ste prednastavenia (presets)."
#define MSGTR_ConfigFileError "chyba konfigura�n�ho s�boru"
#define MSGTR_ErrorParsingCommandLine "chyba pri spracov�van� pr�kazov�ho riadku"
#define MSGTR_VideoStreamRequired "Video pr�d je povinn�!\n"
#define MSGTR_ForcingInputFPS "vstupn� fps bude interpretovan� ako %5.2f\n"
#define MSGTR_RawvideoDoesNotSupportAudio "V�stupn� form�t s�boru RAWVIDEO nepodporuje zvuk - vyp�nam ho\n"
#define MSGTR_DemuxerDoesntSupportNosound "Tento demuxer zatia� nepodporuje -nosound.\n"
#define MSGTR_MemAllocFailed "Alok�cia pam�te zlyhala\n"
#define MSGTR_NoMatchingFilter "Nemo�em n�js� zodpovedaj�ci filter/ao form�t!\n"
#define MSGTR_MP3WaveFormatSizeNot30 "sizeof(MPEGLAYER3WAVEFORMAT)==%d!=30, mo�no je vadn� preklada� C?\n"
#define MSGTR_NoLavcAudioCodecName "Audio LAVC, ch�ba meno k�deku!\n"
#define MSGTR_LavcAudioCodecNotFound "Audio LAVC, nem��em n�js� enk�der pre k�dek %s\n"
#define MSGTR_CouldntAllocateLavcContext "Audio LAVC, nem��em alokova� kontext!\n"
#define MSGTR_CouldntOpenCodec "Ned� sa otvori� k�dek %s, br=%d\n"
#define MSGTR_CantCopyAudioFormat "Audio form�t 0x%x je nekompatibiln� s '-oac copy', sk�ste pros�m '-oac pcm',\n alebo pou�ite '-fafmttag' pre jeho prep�sanie.\n"

// cfg-mencoder.h:

#define MSGTR_MEncoderMP3LameHelp "\n\n"\
" vbr=<0-4>     met�da variabilnej bit. r�chlosti \n"\
"                0: cbr (kon�tantn� bit.r�chlos�)\n"\
"                1: mt (Mark Taylor VBR alg.)\n"\
"                2: rh(Robert Hegemann VBR alg. - default)\n"\
"                3: abr (priemern� bit.r�chlos�)\n"\
"                4: mtrh (Mark Taylor Robert Hegemann VBR alg.)\n"\
"\n"\
" abr           priemern� bit. r�chlos�\n"\
"\n"\
" cbr           kon�tantn� bit. r�chlos�\n"\
"               Vn�ti tie� CBR m�d na podsekvenci�ch ABR m�dov\n"\
"\n"\
" br=<0-1024>   �pecifikova� bit. r�chlos� v kBit (plat� iba pre CBR a ABR)\n"\
"\n"\
" q=<0-9>       kvalita (0-najvy��ia, 9-najni��ia) (iba pre VBR)\n"\
"\n"\
" aq=<0-9>      algoritmick� kvalita (0-najlep./najpomal�ia, 9-najhor�ia/najr�chl.)\n"\
"\n"\
" ratio=<1-100> kompresn� pomer\n"\
"\n"\
" vol=<0-10>    nastavenie audio zosilnenia\n"\
"\n"\
" mode=<0-3>    (default: auto)\n"\
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
" fast          prepn�� na r�chlej�ie k�dovanie na podsekvenci�ch VBR m�dov,\n"\
"               mierne ni��ia kvalita and vy��ia bit. r�chlos�.\n"\
"\n"\
" preset=<value> umo��uje najvy��ie mo�n� nastavenie kvality.\n"\
"                 medium: VBR  k�dovanie,  dobr� kvalita\n"\
"                 (150-180 kbps rozp�tie bit. r�chlosti)\n"\
"                 standard:  VBR k�dovanie, vysok� kvalita\n"\
"                 (170-210 kbps rozp�tie bit. r�chlosti)\n"\
"                 extreme: VBR k�dovanie, velmi vysok� kvalita\n"\
"                 (200-240 kbps rozp�tie bit. r�chlosti)\n"\
"                 insane:  CBR  k�dovanie, najvy��ie nastavenie kvality\n"\
"                 (320 kbps bit. r�chlos�)\n"\
"                 <8-320>: ABR k�dovanie na zadanej kbps bit. r�chlosti.\n\n"

//codec-cfg.c:
#define MSGTR_DuplicateFourcc "zdvojen� FourCC"
#define MSGTR_TooManyFourccs "pr�li� vela FourCCs/form�tov..."
#define MSGTR_ParseError "chyba spracovania (parse)"
#define MSGTR_ParseErrorFIDNotNumber "chyba spracovania (parse) (ID form�tu nie je ��slo?)"
#define MSGTR_ParseErrorFIDAliasNotNumber "chyba spracovania (parse) (alias ID form�tu nie je ��slo?)"
#define MSGTR_DuplicateFID "duplik�tne format ID"
#define MSGTR_TooManyOut "pr�li� mnoho v�stupu..."
#define MSGTR_InvalidCodecName "\nmeno k�deku(%s) nie je platn�!\n"
#define MSGTR_CodecLacksFourcc "\nmeno k�deku(%s) nem� FourCC/form�t!\n"
#define MSGTR_CodecLacksDriver "\nmeno k�deku(%s) nem� ovl�da�!\n"
#define MSGTR_CodecNeedsDLL "\nk�dek(%s) vy�aduje 'dll'!\n"
#define MSGTR_CodecNeedsOutfmt "\nk�dek(%s) vy�aduje 'outfmt'!\n"
#define MSGTR_CantAllocateComment "Ned� sa alokova� pam� pre pozn�mku. "
#define MSGTR_GetTokenMaxNotLessThanMAX_NR_TOKEN "get_token(): max >= MAX_MR_TOKEN!"
#define MSGTR_ReadingFile "��tam %s: "
#define MSGTR_CantOpenFileError "Ned� sa otvori� '%s': %s\n"
#define MSGTR_CantGetMemoryForLine "Nejde z�ska� pam� pre 'line': %s\n"
#define MSGTR_CantReallocCodecsp "Ned� sa realokova� '*codecsp': %s\n"
#define MSGTR_CodecNameNotUnique " Meno k�deku '%s' nie je jedine�n�."
#define MSGTR_CantStrdupName "Ned� sa spravi� strdup -> 'name': %s\n"
#define MSGTR_CantStrdupInfo "Ned� sa spravi� strdup -> 'info': %s\n"
#define MSGTR_CantStrdupDriver "Ned� sa spravi� strdup -> 'driver': %s\n"
#define MSGTR_CantStrdupDLL "Ned� sa spravi� strdup -> 'dll': %s"
#define MSGTR_AudioVideoCodecTotals "%d audio & %d video codecs\n"
#define MSGTR_CodecDefinitionIncorrect "K�dek nie je definovan� korektne."
#define MSGTR_OutdatedCodecsConf "S�bor codecs.conf je pr�li� star� a nekompatibiln� s touto verziou MPlayer-u!"

// divx4_vbr.c:
#define MSGTR_OutOfMemory "nedostatok pam�te"
#define MSGTR_OverridingTooLowBitrate "Zadan� d�tov� tok je pr�li� n�zky pre tento klip.\n"\
"Minim�lny mo�n� d�tov� tok pre tento klip je %.0f kbps. Prepisujem\n"\
"pou��vate�om nastaven� hodnotu.\n"

// fifo.c
#define MSGTR_CannotMakePipe "Ned� sa vytvori� PIPE!\n"

// m_config.c
#define MSGTR_SaveSlotTooOld "Pr�li� star� save slot n�jden� z lvl %d: %d !!!\n"
#define MSGTR_InvalidCfgfileOption "Vo�ba %s sa ned� pou�i� v konfigura�nom s�bore.\n"
#define MSGTR_InvalidCmdlineOption "Vo�ba %s sa ned� pou�i� z pr�kazov�ho riadku.\n"
#define MSGTR_InvalidSuboption "Chyba: vo�ba '%s' nem� �iadnu podvo�bu '%s'.\n"
#define MSGTR_MissingSuboptionParameter "Chyba: podvo�ba '%s' vo�by '%s' mus� ma� parameter!\n"
#define MSGTR_MissingOptionParameter "Chyba: vo�ba '%s' mus� ma� parameter!\n"
#define MSGTR_OptionListHeader "\n N�zov                Typ             Min        Max      Glob�l  CL    Konfig\n\n"
#define MSGTR_TotalOptions "\nCelkovo: %d volieb\n"
#define MSGTR_TooDeepProfileInclusion "VAROVANIE: Pr�li� hlbok� vnorovanie profilov.\n"
#define MSGTR_NoProfileDefined "�iadny profil nebol definovan�.\n"
#define MSGTR_AvailableProfiles "Dostupn� profily:\n"
#define MSGTR_UnknownProfile "Nezn�my profil '%s'.\n"
#define MSGTR_Profile "Profil %s: %s\n"

// m_property.c
#define MSGTR_PropertyListHeader "\n Meno                 Typ             Min        Max\n\n"
#define MSGTR_TotalProperties "\nCelkovo: %d vlastnost�\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "CD-ROM zariadenie '%s' nen�jden�!\n"
#define MSGTR_ErrTrackSelect "Chyba pri v�bere VCD stopy!"
#define MSGTR_ReadSTDIN "��tam z stdin...\n"
#define MSGTR_UnableOpenURL "Nejde otvori� URL: %s\n"
#define MSGTR_ConnToServer "Pripojen� k servru: %s\n"
#define MSGTR_FileNotFound "S�bor nen�jden�: '%s'\n"

#define MSGTR_SMBInitError "Nem��em inicializova� kni�nicu libsmbclient: %d\n"
#define MSGTR_SMBFileNotFound "Nem��em otvori� z LAN: '%s'\n"
#define MSGTR_SMBNotCompiled "MPlayer mebol skompilovan� s podporou ��tania z SMB\n"

#define MSGTR_CantOpenDVD "Nejde otvori� DVD zariadenie: %s\n"
#define MSGTR_NoDVDSupport "MPlayer bol skompilovan� bez podpory DVD, koniec\n"
#define MSGTR_DVDwait "��tam �trukt�ru disku, pros�m �akajte...\n"
#define MSGTR_DVDnumTitles "Na tomto DVD je %d titulov.\n"
#define MSGTR_DVDinvalidTitle "Neplatn� ��slo DVD titulu: %d\n"
#define MSGTR_DVDnumChapters "Na tomto DVD je %d kapitol.\n"
#define MSGTR_DVDinvalidChapter "Neplatn� ��slo kapitoly DVD: %d\n"
#define MSGTR_DVDinvalidChapterRange "Nespr�vn� nastaven� rozsah kapitol %s\n"
#define MSGTR_DVDinvalidLastChapter "Neplatn� ��slo poslednej DVD kapitoly: %d\n"
#define MSGTR_DVDnumAngles "Na tomto DVD je %d uhlov poh�adov.\n"
#define MSGTR_DVDinvalidAngle "Neplatn� ��slo uhlu poh�adu DVD: %d\n"
#define MSGTR_DVDnoIFO "Nem��em otvori� s�bor IFO pre DVD titul %d.\n"
#define MSGTR_DVDnoVMG "Ned� sa otvori� VMG info!\n"
#define MSGTR_DVDnoVOBs "Nem��em otvori� VOB s�bor (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDnoMatchingAudio "DVD zvuk v po�adovanom jazyku nebyl n�jden�!\n"
#define MSGTR_DVDaudioChannel "Zvolen� DVD zvukov� kan�l: %d jazyk: %c%c\n"
#define MSGTR_DVDnoMatchingSubtitle "DVD titulky v po�adovanom jazyku neboli n�jden�!\n"
#define MSGTR_DVDsubtitleChannel "Zvolen� DVD titulkov� kan�l: %d jazyk: %c%c\n"
#define MSGTR_DVDopenOk "DVD �spe�ne otvoren�.\n"

// muxer.c, muxer_*.c:
#define MSGTR_TooManyStreams "Pr�li� ve�a pr�dov!"
#define MSGTR_RawMuxerOnlyOneStream "Rawaudio muxer podporuje iba jeden audio pr�d!\n"
#define MSGTR_IgnoringVideoStream "Ignorujem video pr�d!\n"
#define MSGTR_UnknownStreamType "Varovanie! nezn�my typ pr�du: %d\n"
#define MSGTR_WarningLenIsntDivisible "Varovanie! d�ka nie je delite�n� velkos�ou vzorky!\n"
#define MSGTR_MuxbufMallocErr "Ned� sa alokova� pam� pre frame buffer muxeru!\n"
#define MSGTR_MuxbufReallocErr "Ned� sa realokova� pam� pre frame buffer muxeru!\n"
#define MSGTR_MuxbufSending "Frame buffer muxeru posiela %d sn�mkov do muxeru.\n"
#define MSGTR_WritingHeader "Zapisujem header...\n"
#define MSGTR_WritingTrailer "Zapisujem index...\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "Upozornenie! Hlavi�ka audio pr�du %d predefinovan�!\n"
#define MSGTR_VideoStreamRedefined "Upozornenie! Hlavi�ka video pr�du %d predefinovan�!\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: Pr�li� mnoho (%d v %d bajtoch) audio paketov v bufferi!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: Pr�li� mnoho (%d v %d bajtoch) video paketov v bufferi!\n"
#define MSGTR_MaybeNI "(mo�no prehr�vate neprekladan� pr�d/s�bor alebo kodek zlyhal)\n" \
		      "Pre .AVI s�bory sk�ste vyn�ti� neprekladan� m�d vo�bou -ni\n"
#define MSGTR_SwitchToNi "\nDetekovan� zle prekladan� .AVI - prepnite -ni m�d!\n"
#define MSGTR_Detected_XXX_FileFormat "Detekovan� %s form�t s�boru!\n"
#define MSGTR_DetectedAudiofile "Detekovan� audio s�bor!\n"
#define MSGTR_NotSystemStream "Nie je to MPEG System Stream form�t... (mo�no Transport Stream?)\n"
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
#define MSGTR_CantSeekRawAVI "Nem��em sa pos�va� v surov�ch (raw) .AVI pr�doch! (Potrebujem index, zkuste pou�� vo�bu -idx!)  \n"
#define MSGTR_CantSeekFile "Nem��em sa pos�va� v tomto s�bore!  \n"

#define MSGTR_EncryptedVOB "�ifrovan� s�bor VOB! Pre��tajte si DOCS/HTML/en/cd-dvd.html.\n"

#define MSGTR_MOVcomprhdr "MOV: Komprimovan� hlavi�ky nie s� (e�te) podporovan�!\n"
#define MSGTR_MOVvariableFourCC "MOV: Upozornenie! premenn� FOURCC detekovan�!?\n"
#define MSGTR_MOVtooManyTrk "MOV: Upozornenie! Pr�li� ve�a st�p!"
#define MSGTR_FoundAudioStream "==> N�jden� audio pr�d: %d\n"
#define MSGTR_FoundVideoStream "==> N�jden� video pr�d: %d\n"
#define MSGTR_DetectedTV "TV detekovan�! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "Nem��em otvori� ogg demuxer\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: H�ad�m audio pr�d (id:%d)\n"
#define MSGTR_CannotOpenAudioStream "Nem��em otvori� audio pr�d: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "Nem��em otvori� pr�d titulkov: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "Nem��em otvori� audio demuxer: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "Nem��em otvori� demuxer titulkov: %s\n"
#define MSGTR_TVInputNotSeekable "v TV vstupe nie je mo�n� sa pohybova�! (mo�no posun bude na zmenu kan�lov ;)\n"
#define MSGTR_DemuxerInfoAlreadyPresent "Demuxer info %s u� pr�tomn�!\n"
#define MSGTR_ClipInfo "Inform�cie o klipe: \n"

#define MSGTR_LeaveTelecineMode "\ndemux_mpg: detekovan� 30000/1001 fps NTSC, prep�nam frekvenciu sn�mkov.\n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: detekovan� 24000/1001 fps progres�vny NTSC, prep�nam frekvenciu sn�mkov.\n"

#define MSGTR_CacheFill "\rNaplnenie cache: %5.2f%% (%"PRId64" bajtov)   "
#define MSGTR_NoBindFound "Tla�idlo '%s' nem� priraden� �iadnu funkciu."
#define MSGTR_FailedToOpen "Zlyhalo otvorenie %s\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "nem��em otvori� kodek\n"
#define MSGTR_CantCloseCodec "nem��em uzavie� kodek\n"

#define MSGTR_MissingDLLcodec "CHYBA: Nem��em otvori� potrebn� DirectShow kodek: %s\n"
#define MSGTR_ACMiniterror "Nem��em na��ta�/inicializova� Win32/ACM AUDIO kodek (ch�baj�ci s�bor DLL?)\n"
#define MSGTR_MissingLAVCcodec "Nem��em najs� kodek '%s' v libavcodec...\n"

#define MSGTR_MpegNoSequHdr "MPEG: FATAL: EOF - koniec s�boru v priebehu vyh�ad�vania hlavi�ky sekvencie\n"
#define MSGTR_CannotReadMpegSequHdr "FATAL: Nem��em pre��ta� hlavi�ku sekvencie!\n"
#define MSGTR_CannotReadMpegSequHdrEx "FATAL: Nem��em pre��ta� roz��renie hlavi�ky sekvencie!\n"
#define MSGTR_BadMpegSequHdr "MPEG: Zl� hlavi�ka sekvencie!\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: Zl� roz��renie hlavi�ky sekvencie!\n"

#define MSGTR_ShMemAllocFail "Nem��em alokova� zdie�an� pam�\n"
#define MSGTR_CantAllocAudioBuf "Nem��em alokova� pam� pre v�stupn� audio buffer\n"

#define MSGTR_UnknownAudio "Nezn�my/ch�baj�ci audio form�t -> bez zvuku\n"

#define MSGTR_UsingExternalPP "[PP] Pou��vam extern� postprocessing filter, max q = %d\n"
#define MSGTR_UsingCodecPP "[PP] Po��vam postprocessing z kodeku, max q = %d\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "Video atrib�t '%s' nie je podporovan� v�berom vo & vd! \n"
#define MSGTR_VideoCodecFamilyNotAvailableStr "Po�adovan� rodina video kodekov [%s] (vfm=%s) nie je dostupn� (zapnite ju pri kompil�cii!)\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "Po�adovan� rodina audio kodekov [%s] (afm=%s) nie je dostupn� (zapnite ju pri kompil�cii!)\n"
#define MSGTR_OpeningVideoDecoder "Otv�ram video dek�der: [%s] %s\n"
#define MSGTR_SelectedVideoCodec "Zvolen� video k�dek: [%s] vfm: %s (%s)\n"
#define MSGTR_OpeningAudioDecoder "Otv�ram audio dek�der: [%s] %s\n"
#define MSGTR_SelectedAudioCodec "Zvolen� audio k�dek: [%s] afm: %s (%s)\n"
#define MSGTR_BuildingAudioFilterChain "Vytv�ram re�azec audio filterov pre %dHz/%dch/%s -> %dHz/%dch/%s...\n"
#define MSGTR_UninitVideoStr "odinicializova� video: %s  \n"
#define MSGTR_UninitAudioStr "odinicializova� audio: %s  \n"
#define MSGTR_VDecoderInitFailed "VDecoder init zlyhal :(\n"
#define MSGTR_ADecoderInitFailed "ADecoder init zlyhal :(\n"
#define MSGTR_ADecoderPreinitFailed "ADecoder preinit zlyhal :(\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: Alokujem %d bytov pre vstupn� buffer\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: Alokujem %d + %d = %d bytov pre v�stupn� buffer\n"
			 
// LIRC:
#define MSGTR_SettingUpLIRC "Zap�nam podporu LIRC...\n"
#define MSGTR_LIRCdisabled "Nebudete m�c� pou��va� dia�kov� ovl�da�.\n"
#define MSGTR_LIRCopenfailed "Zlyhal pokus o otvorenie podpory LIRC!\n"
#define MSGTR_LIRCcfgerr "Zlyhalo ��tanie konfigura�n�ho s�boru LIRC %s!\n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "Nem��em n�js� video filter '%s'\n"
#define MSGTR_CouldNotOpenVideoFilter "Nem��em otvori� video filter '%s'\n"
#define MSGTR_OpeningVideoFilter "Otv�ram video filter: "
#define MSGTR_CannotFindColorspace "Nem��em n�js� be�n� priestor farieb, ani vlo�en�m 'scale' :(\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDec: kodek nenastavil sh->disp_w a sh->disp_h, sk��am to ob�s�!\n"
#define MSGTR_VoConfigRequest "VDec: vo konfigura�n� po�iadavka - %d x %d (preferovan� csp: %s)\n"
#define MSGTR_CouldNotFindColorspace "Nem��em n�js� zhodn� priestor farieb - sk��am znova s -vf scale...\n"
#define MSGTR_MovieAspectIsSet "Movie-Aspect je %.2f:1 - men�m rozmery na spr�vne.\n"
#define MSGTR_MovieAspectUndefined "Movie-Aspect je nedefinovn� - nemenia sa rozmery.\n"

// vd_dshow.c, vd_dmo.c
#define MSGTR_DownloadCodecPackage "Potrebujete aktualizova� alebo nain�talova� bin�rne k�deky.\nChodte na http://www.mplayerhq.hu/dload.html\n"
#define MSGTR_DShowInitOK "INFO: Inicializ�cia Win32/DShow videok�deku OK.\n"
#define MSGTR_DMOInitOK "INFO: Inicializ�cia Win32/DMO videok�deku OK.\n"

// x11_common.c
#define MSGTR_EwmhFullscreenStateFailed "\nX11: Nem��em posla� udalos� EWMH fullscreen!\n"
#define MSGTR_CouldNotFindXScreenSaver "xscreensaver_disable: Ned� sa n�js� XScreenSaveru.\n"
#define MSGTR_SelectedVideoMode "XF86VM: Zvolen� videore�im %dx%d pre obraz velkosti %dx%d.\n"

#define MSGTR_InsertingAfVolume "[Mixer] Hardv�rov� mix�r nie je k dispozic�, vklad�m filter pre hlasitos�.\n"
#define MSGTR_NoVolume "[Mixer] Ovl�danie hlasitosti nie je dostupn�.\n"

// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "O aplik�cii"
#define MSGTR_FileSelect "Vybra� s�bor..."
#define MSGTR_SubtitleSelect "Vybra� titulky..."
#define MSGTR_OtherSelect "Vybra�..."
#define MSGTR_AudioFileSelect "Vybra� extern� audio kan�l..."
#define MSGTR_FontSelect "Vybra� font..."
// Note: If you change MSGTR_PlayList please see if it still fits MSGTR_MENU_PlayList
#define MSGTR_PlayList "PlayList"
#define MSGTR_Equalizer "Equalizer"
#define MSGTR_ConfigureEqualizer "Konfigurova� Equalizer"
#define MSGTR_SkinBrowser "Prehliada� t�m"
#define MSGTR_Network "Sie�ov� prehr�vanie (streaming)..."
// Note: If you change MSGTR_Preferences please see if it still fits MSGTR_MENU_Preferences
#define MSGTR_Preferences "Preferencie"
#define MSGTR_AudioPreferences "Konfiguracia ovlada�a zvuku"
#define MSGTR_NoMediaOpened "Ni� nie je otvoren�"
#define MSGTR_VCDTrack "VCD stopa %d"
#define MSGTR_NoChapter "�iadna kapitola"
#define MSGTR_Chapter "Kapitola %d"
#define MSGTR_NoFileLoaded "Nenahran� �iaden s�bor"

// --- buttons ---
#define MSGTR_Ok "Ok"
#define MSGTR_Cancel "Zru�i�"
#define MSGTR_Add "Prida�"
#define MSGTR_Remove "Odobra�"
#define MSGTR_Clear "Vy�isti�"
#define MSGTR_Config "Konfigur�cia"
#define MSGTR_ConfigDriver "Konfigurova� ovl�da�"
#define MSGTR_Browse "Prehliada�"

// --- error messages ---
#define MSGTR_NEMDB "�ia�, nedostatok pam�te pre buffer na kreslenie."
#define MSGTR_NEMFMR "�ia�, nedostatok pam�te pre vytv�ranie menu."
#define MSGTR_IDFGCVD "�ia�, nem��em n�js� gui kompatibiln� ovl�da� video v�stupu."
#define MSGTR_NEEDLAVCFAME "�ia�, nem��ete prehr�va� nie mpeg s�bory s DXR3/H+ zariaden�m bez prek�dovania.\nPros�m zapnite lavc alebo fame v DXR3/H+ konfig. okne."
#define MSGTR_UNKNOWNWINDOWTYPE "Nezn�my typ okna n�jden� ..."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[t�my] chyba v konfig. s�bore t�m %d: %s"
#define MSGTR_SKIN_WARNING1 "[t�my] varovanie v konfig. s�bore t�m na riadku %d: widget najden� ale pred  \"section\" nen�jden� (%s)"
#define MSGTR_SKIN_WARNING2 "[t�my] varovanie v konfig. s�bore t�m na riadku %d: widget najden� ale pred \"subsection\" nen�jden� (%s)"
#define MSGTR_SKIN_WARNING3 "[t�my] varovanie v konfig. s�bore t�m na riadku %d: t�to subsekcia nie je podporovan� t�mto widget (%s)"
#define MSGTR_SKIN_SkinFileNotFound "[skin] s�bor ( %s ) nen�jden�.\n"
#define MSGTR_SKIN_SkinFileNotReadable "[skin] s�bor ( %s ) sa ned� pre��ta�.\n"
#define MSGTR_SKIN_BITMAP_16bit  "bitmapa s h�bkou 16 bit a menej je nepodporovan� (%s).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "s�bor nen�jden� (%s)\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "chyba ��tania BMP (%s)\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "chyba ��tania TGA (%s)\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "chyba ��tania PNG (%s)\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "form�t RLE packed TGA nepodporovan� (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "nezn�my typ s�boru (%s)\n"
#define MSGTR_SKIN_BITMAP_ConvertError "chyba konverzie z 24 bit do 32 bit (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "nezn�ma spr�va: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "nedostatok pam�te\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "pr�li� mnoho fontov deklarovan�ch\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "s�bor fontov nen�jden�\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "s�bor obrazov fontu nen�jden�\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "neexistuj�ci identifik�tor fontu (%s)\n"
#define MSGTR_SKIN_UnknownParameter "nezn�my parameter (%s)\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "T�ma nen�jden� (%s).\n"
#define MSGTR_SKIN_SKINCFG_SelectedSkinNotFound "Vybran� t�ma ( %s ) nen�jden�, sk��am 'prednastaven�'...\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "Chyba pri ��tan� konfigura�n�ho s�boru t�m (%s).\n"
#define MSGTR_SKIN_LABEL "T�my:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "O aplik�cii MPlayer"
#define MSGTR_MENU_Open "Otvori�..."
#define MSGTR_MENU_PlayFile "Prehra� s�bor..."
#define MSGTR_MENU_PlayVCD "Prehra� VCD..."
#define MSGTR_MENU_PlayDVD "Prehra� DVD..."
#define MSGTR_MENU_PlayURL "Prehra� URL..."
#define MSGTR_MENU_LoadSubtitle "Na��ta� titulky..."
#define MSGTR_MENU_DropSubtitle "Zahodi� titulky..."
#define MSGTR_MENU_LoadExternAudioFile "Na��ta� extern� audio s�bor..."
#define MSGTR_MENU_Playing "Prehr�vam"
#define MSGTR_MENU_Play "Prehra�"
#define MSGTR_MENU_Pause "Pauza"
#define MSGTR_MENU_Stop "Zastavi�"
#define MSGTR_MENU_NextStream "�al�� pr�d"
#define MSGTR_MENU_PrevStream "Predch�dzaj�ci pr�d"
#define MSGTR_MENU_Size "Ve�kos�"
#define MSGTR_MENU_HalfSize   "Polovi�n� velikos�"
#define MSGTR_MENU_NormalSize "Norm�lna ve�kos�"
#define MSGTR_MENU_DoubleSize "Dvojn�sobn� ve�kos�"
#define MSGTR_MENU_FullScreen "Cel� obrazovka"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "Prehra� disk..."
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
#define MSGTR_MENU_Preferences MSGTR_Preferences
#define MSGTR_MENU_Exit "Koniec..."
#define MSGTR_MENU_Mute "Stlmi� zvuk"
#define MSGTR_MENU_Original "Origin�l"
#define MSGTR_MENU_AspectRatio "Pomer str�n obrazu"
#define MSGTR_MENU_AudioTrack "Audio stopa"
#define MSGTR_MENU_Track "Stopa %d"
#define MSGTR_MENU_VideoTrack "Video stopa"
#define MSGTR_MENU_Subtitles "Titulky"

// --- equalizer
// Note: If you change MSGTR_EQU_Audio please see if it still fits MSGTR_PREFERENCES_Audio
#define MSGTR_EQU_Audio "Audio"
// Note: If you change MSGTR_EQU_Video please see if it still fits MSGTR_PREFERENCES_Video
#define MSGTR_EQU_Video "Video"
#define MSGTR_EQU_Contrast "Kontrast: "
#define MSGTR_EQU_Brightness "Jas: "
#define MSGTR_EQU_Hue "Odtie�: "
#define MSGTR_EQU_Saturation "Nas�tenie: "
#define MSGTR_EQU_Front_Left "Predn� �av�"
#define MSGTR_EQU_Front_Right "Predn� Prav�"
#define MSGTR_EQU_Back_Left "Zadn� �av�"
#define MSGTR_EQU_Back_Right "Zadn� Prav�"
#define MSGTR_EQU_Center "Stredn�"
#define MSGTR_EQU_Bass "Basov�"
#define MSGTR_EQU_All "V�etko"
#define MSGTR_EQU_Channel1 "Kan�l 1:"
#define MSGTR_EQU_Channel2 "Kan�l 2:"
#define MSGTR_EQU_Channel3 "Kan�l 3:"
#define MSGTR_EQU_Channel4 "Kan�l 4:"
#define MSGTR_EQU_Channel5 "Kan�l 5:"
#define MSGTR_EQU_Channel6 "Kan�l 6:"

// --- playlist
#define MSGTR_PLAYLIST_Path "Cesta"
#define MSGTR_PLAYLIST_Selected "Vybran� s�bory"
#define MSGTR_PLAYLIST_Files "S�bory"
#define MSGTR_PLAYLIST_DirectoryTree "Adres�rov� strom"

// --- preferences
#define MSGTR_PREFERENCES_Audio MSGTR_EQU_Audio
#define MSGTR_PREFERENCES_Video MSGTR_EQU_Video
#define MSGTR_PREFERENCES_SubtitleOSD "Titulky a OSD"
#define MSGTR_PREFERENCES_Codecs "K�deky a demuxer"
// Pozn�mka: Pokia� zmen�te MSGTR_PREFERENCES_Misc, uistite sa pros�m, �e vyhovuje aj pre MSGTR_PREFERENCES_FRAME_Misc
#define MSGTR_PREFERENCES_Misc "R�zne"

#define MSGTR_PREFERENCES_None "Ni�"
#define MSGTR_PREFERENCES_DriverDefault "v�chodzie nastavenie"
#define MSGTR_PREFERENCES_AvailableDrivers "Dostupn� ovl�da�e:"
#define MSGTR_PREFERENCES_DoNotPlaySound "Nehra� zvuk"
#define MSGTR_PREFERENCES_NormalizeSound "Normalizova� zvuk"
#define MSGTR_PREFERENCES_EnEqualizer "Zapn�� equalizer"
#define MSGTR_PREFERENCES_SoftwareMixer "Aktivova� softv�rov� mix�r"
#define MSGTR_PREFERENCES_ExtraStereo "Zapn�� extra stereo"
#define MSGTR_PREFERENCES_Coefficient "Koeficient:"
#define MSGTR_PREFERENCES_AudioDelay "Audio oneskorenie"
#define MSGTR_PREFERENCES_DoubleBuffer "Zapn�� dvojt� buffering"
#define MSGTR_PREFERENCES_DirectRender "Zapn�� direct rendering"
#define MSGTR_PREFERENCES_FrameDrop "Povoli� zahadzovanie r�mcov"
#define MSGTR_PREFERENCES_HFrameDrop "Povoli� TVRD� zahadzovanie r�mcov (nebezpe�n�)"
#define MSGTR_PREFERENCES_Flip "prehodi� obraz horn� strana-dole"
#define MSGTR_PREFERENCES_Panscan "Panscan: "
#define MSGTR_PREFERENCES_OSDTimer "�as a indik�tor"
#define MSGTR_PREFERENCES_OSDProgress "Iba ukazovate� priebehu a nastavenie"
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "�as, percent� and celkov� �as"
#define MSGTR_PREFERENCES_Subtitle "Titulky:"
#define MSGTR_PREFERENCES_SUB_Delay "Oneskorenie: "
#define MSGTR_PREFERENCES_SUB_FPS "FPS:"
#define MSGTR_PREFERENCES_SUB_POS "Poz�cia: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "Zak�za� automatick� nahr�vanie titulkov"
#define MSGTR_PREFERENCES_SUB_Unicode "Titulky v Unicode"
#define MSGTR_PREFERENCES_SUB_MPSUB "Konvertova� dan� titulky do MPlayer form�tu"
#define MSGTR_PREFERENCES_SUB_SRT "Konvertova� dan� titulky do �asovo-ur�en�ho SubViewer (SRT) form�tu"
#define MSGTR_PREFERENCES_SUB_Overlap "Zapn�� prekr�vanie titulkov"
#define MSGTR_PREFERENCES_Font "Font:"
#define MSGTR_PREFERENCES_FontFactor "Font faktor:"
#define MSGTR_PREFERENCES_PostProcess "Zapn�� postprocess"
#define MSGTR_PREFERENCES_AutoQuality "Automatick� qualita: "
#define MSGTR_PREFERENCES_NI "Pou�i� neprekladan� AVI parser"
#define MSGTR_PREFERENCES_IDX "Obnovi� index tabulku, ak je potrebn�"
#define MSGTR_PREFERENCES_VideoCodecFamily "Rodina video kodekov:"
#define MSGTR_PREFERENCES_AudioCodecFamily "Rodina audeo kodekov:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "OSD �rove�"
#define MSGTR_PREFERENCES_FRAME_Subtitle "Titulky"
#define MSGTR_PREFERENCES_FRAME_Font "Font"
#define MSGTR_PREFERENCES_FRAME_PostProcess "Postprocess"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "K�dek & demuxer"
#define MSGTR_PREFERENCES_FRAME_Cache "Vyrovn�vacia pam�"
#define MSGTR_PREFERENCES_FRAME_Misc MSGTR_PREFERENCES_Misc
#define MSGTR_PREFERENCES_Audio_Device "Zariadenie:"
#define MSGTR_PREFERENCES_Audio_Mixer "Mix�r:"
#define MSGTR_PREFERENCES_Audio_MixerChannel "Kan�l mix�ru:"
#define MSGTR_PREFERENCES_Message "Pros�m pam�tajte, nietor� vo�by potrebuj� re�tart prehr�vania!"
#define MSGTR_PREFERENCES_DXR3_VENC "Video k�der:"
#define MSGTR_PREFERENCES_DXR3_LAVC "Pou�i� LAVC (FFmpeg)"
#define MSGTR_PREFERENCES_DXR3_FAME "Pou�i� FAME"
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
#define MSGTR_PREFERENCES_FontEncoding22 "Slavic/Central European Windows (CP1250)"
#define MSGTR_PREFERENCES_FontNoAutoScale "Nemeni� rozmery"
#define MSGTR_PREFERENCES_FontPropWidth "Proporcion�lne k ��rke obrazu"
#define MSGTR_PREFERENCES_FontPropHeight "Proporcion�lne k v��ke obrazu"
#define MSGTR_PREFERENCES_FontPropDiagonal "Proporcion�lne k diagon�le obrazu"
#define MSGTR_PREFERENCES_FontEncoding "K�dovanie:"
#define MSGTR_PREFERENCES_FontBlur "Rozmazanie:"
#define MSGTR_PREFERENCES_FontOutLine "Obrys:"
#define MSGTR_PREFERENCES_FontTextScale "Mierka textu:"
#define MSGTR_PREFERENCES_FontOSDScale "OSD mierka:"
#define MSGTR_PREFERENCES_Cache "Vyrovn�vacia pam� zap./vyp."
#define MSGTR_PREFERENCES_CacheSize "Ve�kos� vyr. pam�te: "
#define MSGTR_PREFERENCES_LoadFullscreen "Na�tartova� v re�ime celej obrazovky"
#define MSGTR_PREFERENCES_SaveWinPos "Ulo�i� poz�ciu okna"
#define MSGTR_PREFERENCES_XSCREENSAVER "Zastavi� XScreenSaver"
#define MSGTR_PREFERENCES_PlayBar "Zapn�� playbar"
#define MSGTR_PREFERENCES_AutoSync "Automatick� synchroniz�cia zap./vyp."
#define MSGTR_PREFERENCES_AutoSyncValue "Automatick� synchroniz�cia: "
#define MSGTR_PREFERENCES_CDROMDevice "CD-ROM zariadenie:"
#define MSGTR_PREFERENCES_DVDDevice "DVD zariadenie:"
#define MSGTR_PREFERENCES_FPS "Sn�mkov� r�chlos� (FPS):"
#define MSGTR_PREFERENCES_ShowVideoWindow "Uk�za� video okno pri neaktivite"
#define MSGTR_PREFERENCES_ArtsBroken "Nov�ie verze aRts s� nekompatibiln� "\
           "s GTK 1.x a zhavaruj� GMPlayer!"

#define MSGTR_ABOUT_UHU "v�voj GUI sponzoroval UHU Linux\n"
#define MSGTR_ABOUT_Contributors "P�ispievatelia k�du a dokumentacie\n"
#define MSGTR_ABOUT_Codecs_libs_contributions "K�deky a kni�nice tret�ch str�n\n"
#define MSGTR_ABOUT_Translations "Preklady\n"
#define MSGTR_ABOUT_Skins "T�my\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "Fat�lna chyba!"
#define MSGTR_MSGBOX_LABEL_Error "Chyba!"
#define MSGTR_MSGBOX_LABEL_Warning "Upozornenie!"

#endif


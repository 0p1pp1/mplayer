// Last sync on 2004-10-20 with help_mp-en.h 1.147
// Translated by:  Carl F�rstenberg <azatoth@gmail.com>
// ========================= MPlayer hj�lp ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"Anv�ndning:   mplayer [argument] [url|s�kv�g/]filnamn\n"
"\n"
"Grundl�ggande argument: (komplett lista �terfinns i `man mplayer`)\n"
" -vo <drv[:enhet]>   v�lj video-ut drivrutin & enhet ('-vo help' f�r lista)\n"
" -ao <drv[:enhet]>   v�lj audio-ut drivrutin & enhet ('-ao help' f�r lista)\n"
#ifdef HAVE_VCD
" vcd://<sp�rnr>      spela (S)VCD (Super Video CD) sp�r (r� enhet, ingen montering)\n"
#endif
#ifdef USE_DVDREAD
" dvd://<titlenr>     spela DVD titel fr�n enhet ist�llet f�r ifr�n en enkel fil\n"
" -alang/-slang       v�lj DVD audio/textningsspr�k (m.h.a. ett 2-teckens landskod)\n"
#endif
" -ss <tidpos>        s�k till given position (sekunder eller hh:mm:ss)\n"
" -nosound            spela inte upp ljud\n"
" -fs                 fullsk�rmsuppspelning (eller -vm, -zoom, detaljer p� manualsidan)\n"
" -x <x> -y <y>       s�tt sk�rmuppl�sning (f�r anv�ndning med -vm eller -zoom)\n"
" -sub <fil>          specifiera textningsfil att anv�nda (se ocks� -subfps, -subdelay)\n"
" -playlist <fil>     specifiera spellistefil\n"
" -vid x -aid y       v�lj video (x) och audio (y) str�m att spela\n"
" -fps x -srate y     �ndra video (x bps) och audio (y Hz) frekvens\n"
" -pp <kvalit�>       aktivera postredigeringsfilter (detaljer p� manualsidan)\n"
" -framedrop          aktivera reducering av antalet bildrutor (f�r l�ngsamma maskiner)\n" 
"\n"
"Grundl�ggande navigering: (komplett lista �terfinns p� manualsidan, l�s �ven input.conf)\n"
" <-  eller  ->       s�k bak�t/fram�t 10 sekunder\n"
" upp eller ner       s�k bak�t/fram�t 1 minut\n"
" pgup eller pgdown   s�k bak�t/fram�t 10 minuter\n"
" < eller >           stega bak�t/fram�t i spellistan\n"
" p eller SPACE       pausa filmen (tryck p� valfri tagent f�r att forts�tta)\n"
" q eller ESC         stanna spelningen och avsluta programmet\n"
" + eller -           st�ll in audiof�rdr�jning med � 0.1 sekund\n"
" o                   v�xla OSD l�ge:  ingen / l�gesindikator / l�gesindikator + tidtagare\n"
" * eller /           �ka eller s�nk PCM-volym\n"
" z eller x           st�ll in textningsf�rdr�jning med � 0.1 sekund\n"
" r or t              st�ll in textningsposition upp/ner, se ocks� '-vf expand'\n"
"\n"
" * * * L�S MANUALEN F�R FLER DETALJER, MER AVANCERADE ARGUMENT OCH KOMMANDON * * *\n"
"\n";
#endif

#define MSGTR_SamplesWanted "Fler exempel p� detta format beh�vs f�r att vidare �ka support. Var v�nlig kontakta utvecklarna.\n"

// ========================= MPlayer messages ===========================

// mplayer.c:

#define MSGTR_Exiting "\nSt�nger ner...\n"
#define MSGTR_ExitingHow "\nSt�nger ner... (%s)\n"
#define MSGTR_Exit_quit "Avsluta"
#define MSGTR_Exit_eof "Slut p� fil"
#define MSGTR_Exit_error "O�verkomligt fel"
#define MSGTR_IntBySignal "\nMPlayer var avbruten av signal %d i modul: %s\n"
#define MSGTR_NoHomeDir "Kan inte lokalisera $HOME-katalog.\n"
#define MSGTR_GetpathProblem "get_path(\"config\") problem\n"
#define MSGTR_CreatingCfgFile "Skapar konfigfil: %s\n"
#define MSGTR_InvalidAOdriver "Ej godk�nd audio-ut-drivrutinsnamn: %s\n"\
    "Anv�nd '-ao help' f�r att f� en lista med tillg�ngliga audio-ut-drivrutiner.\n"
#define MSGTR_CopyCodecsConf "(Kopiera/l�nka etc/codecs.conf fr�n MPlayer's k�llkod till ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "Anv�nder standardinbyggd codecs.conf.\n"
#define MSGTR_CantLoadFont "Kan inte ladda font: %s\n"
#define MSGTR_CantLoadSub "Kan inte ladda vald textning: %s\n"
#define MSGTR_DumpSelectedStreamMissing "dump: FATALT: Vald str�m ej tillg�nglig!\n"
#define MSGTR_CantOpenDumpfile "Kan inte �ppna dumpfil.\n"
#define MSGTR_CoreDumped "Core dumpad ;)\n"
#define MSGTR_FPSnotspecified "FPS ej specifierad i filhuvudet eller �r icke godk�nd, anv�nd argument -fps.\n"
#define MSGTR_TryForceAudioFmtStr "F�rs�ker att forcera audiocodecfamilj %s...\n"
#define MSGTR_CantFindAudioCodec "Kan inte finna codec f�r audioformat 0x%X.\n"
#define MSGTR_RTFMCodecs "L�s DOCS/HTML/en/codecs.html!\n"
#define MSGTR_TryForceVideoFmtStr "F�rs�ker att forcera videocodecfamilj %s...\n"
#define MSGTR_CantFindVideoCodec "Kan inte finna codec f�r vald -vo och videoformat 0x%X.\n"
#define MSGTR_CannotInitVO "FATALT: Kan inte initiera videodrivrutin.\n"
#define MSGTR_CannotInitAO "Kan inte �ppna/initiera audioenhet -> inget ljud.\n"
#define MSGTR_StartPlaying "P�b�rjar uppspelning...\n"

#define MSGTR_SystemTooSlow "\n\n"\
    "           ***********************************************************\n"\
"           **** Ditt system �r f�r sl�tt f�r att spela upp detta! ****\n"\
"           ***********************************************************\n\n"\
"Troliga orsaker, problem, samt s�tt att fixa det:\n"\
"- Troligast: trasig/buggig _audio_drivrutin\n"\
"  - F�rs�k -ao sdl eller anv�nd OSS-emulatorn i ALSA.\n"\
"  - Experimentera med olika v�rden f�r -autosync, 30 �r en bra start.\n"\
"- Seg video-ut\n"\
"  - F�rs�k en annan -vo drivrutin (-vo help f�r en lista) eller f�rs�k -framedrop!\n"\
"- Seg CPU\n"\
"  - F�rs�k att inte spela upp allt f�r stora DVD/DivX p� en seg CPU! Testa med -hardframedrop.\n"\
"- Trasig fil\n"\
"  - F�rs�k med olika kombinationer av -nobps -ni -forceidx -mc 0.\n"\
"- Segt media (NFS/SMB mounts, DVD, VCD etc.)\n"\
"  - F�rs�k med -cache 8192.\n"\
"- Anv�nder du -cache till att spela upp en ickeinterleaved AVIfil?\n"\
"  - F�rs�k -nocache.\n"\
"L�s DOCS/HTML/en/devices.html f�r optimeringstips.\n"\
"Om inget av dessa hj�lper, l�s DOCS/HTML/en/bugreports.html.\n\n"

#define MSGTR_NoGui "MPlayer var kompilerad UTAN GUI-support.\n"
#define MSGTR_GuiNeedsX "MPlayer GUI kr�ver X11.\n"
#define MSGTR_Playing "Spelar %s.\n"
#define MSGTR_NoSound "Audio: inget ljud\n"
#define MSGTR_FPSforced "FPS forcerad att vara %5.3f  (ftime: %5.3f).\n"
#define MSGTR_CompiledWithRuntimeDetection "Kompilerad med \"runtime CPU detection\" - VARNING - detta �r inte optimalt!\n"\
    "F�r att f� b�st prestanda, omkompilera med '--disable-runtime-cpudetection'.\n"
#define MSGTR_CompiledWithCPUExtensions "Kompilerad f�r x86 med till�gg:"
#define MSGTR_AvailableVideoOutputDrivers "Tillg�ngliga video-ut-drivrutiner:\n"
#define MSGTR_AvailableAudioOutputDrivers "Tillg�ngliga audio-ut-drivrutiner:\n"
#define MSGTR_AvailableAudioCodecs "Tillg�ngliga audiocodec:\n"
#define MSGTR_AvailableVideoCodecs "Tillg�ngliga videocodec:\n"
#define MSGTR_AvailableAudioFm "Tillg�ngliga (inkompilerade) audiocodec familjer/drivrutiner:\n"
#define MSGTR_AvailableVideoFm "Tillg�ngliga (inkompilerade) videocodec familjer/drivrutiner:\n"
#define MSGTR_AvailableFsType "Tillg�ngliga l�gen f�r fullsk�rmslager:\n"
#define MSGTR_UsingRTCTiming "Anv�nder Linux's h�rdvaru-RTC-tidtagning (%ldHz).\n"
#define MSGTR_CannotReadVideoProperties "Video: Kan inte l�sa inst�llningar.\n"
#define MSGTR_NoStreamFound "Ingen str�m funnen.\n"
#define MSGTR_ErrorInitializingVODevice "Fel vid �ppning/initiering av vald video_out-enhet (-vo).\n"
#define MSGTR_ForcedVideoCodec "Forcerad videocodec: %s\n"
#define MSGTR_ForcedAudioCodec "Forcerad audiocodec: %s\n"
#define MSGTR_Video_NoVideo "Video: ingen video\n"
#define MSGTR_NotInitializeVOPorVO "\nFATALT: Kunde inte initiera videofilter (-vf) eller video-ut (-vo).\n"
#define MSGTR_Paused "\n  =====  PAUSE  =====\r" // no more than 23 characters (status line for audio files)
#define MSGTR_PlaylistLoadUnable "\nOf�rm�gen att ladda spellista %s.\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
    "- MPlayer krachade av en 'Illegal Instruction'.\n"\
"  Det kan vare en bugg i v�r nya \"runtime CPU-detection\" kod...\n"\
"  Var god l�s DOCS/HTML/en/bugreports.html.\n"
#define MSGTR_Exit_SIGILL \
    "- MPlayer krashade av en 'Illegal Instruction'.\n"\
"  Detta h�nder vanligast om du k�r koden p� en annan CPU �n den var\n"\
"  kompilerad/optimerad f�r\n"\
"  Verifiera detta!\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
    "- MPlayer krashade p� grund utav d�lig anv�ndning av CPU/FPU/RAM.\n"\
"  Omkompilera MPlayer med '--enable-debug' och k�r en \"'gdb' backtrace\" och\n"\
"  deassemblera. Detaljer �terfinns i DOCS/HTML/en/bugreports_what.html#bugreports_crash.\n"
#define MSGTR_Exit_SIGCRASH \
    "- MPlayer krashade. Detta borde inte intr�ffa.\n"\
"  Det kan vara en bugg i MPlayers kod, eller i din drivrutin, eller i din\n"\
"  gcc version. Om du tror det �r MPlayers fel, var v�nlig l�s\n"\
"  DOCS/HTML/en/bugreports.html och f�lj instruktionerna d�r, Vi kan inte och\n"\
"  kommer inte att hj�lpa dig, om du inte kan befodra denna information n�r \n"\
"  du rapporterar en trolig bugg.\n"

#define MSGTR_LoadingConfig "Laddar konfiguration '%s'\n"
#define MSGTR_AddedSubtitleFile "SUB: lade till textningsfil %d: %s \n"
#define MSGTR_ErrorOpeningOutputFile "Fel vid �ppning av fil [%s] f�r skrivning!\n"
#define MSGTR_CommandLine "Kommandorad:"
#define MSGTR_RTCDeviceNotOpenable "Misslyckades att �ppna %s: %s (den borde vara l�sbar av anv�ndaren.)\n"
#define MSGTR_LinuxRTCInitErrorIrqpSet "'Linux RTC' initieringsfel i 'ioctl' rtc_irqp_set %lu: %s\n"
#define MSGTR_IncreaseRTCMaxUserFreq "F�rs�k l�gg till \"echo %lu > /proc/sys/dev/rtc/max-user-freq\" till ditt systems uppstartningsscript.\n"
#define MSGTR_LinuxRTCInitErrorPieOn "'Linux RTC init' fel i 'ioctl' [rtc_pie_on]: %s\n"
#define MSGTR_UsingTimingType "Anv�nder %s tidtagning.\n"
#define MSGTR_MenuInitialized "Meny initierad: %s\n"
#define MSGTR_MenuInitFailed "Menyinitiering misslyckades.\n"
#define MSGTR_Getch2InitializedTwice "VARNING: getch2_init anropad dubbelt!\n"
#define MSGTR_DumpstreamFdUnavailable "Kan inte dumpa denna str�m - ingen 'fd' tillg�nglig.\n"
#define MSGTR_FallingBackOnPlaylist "Faller tillbaka med att f�rs�ka tolka spellista %s...\n"
#define MSGTR_CantOpenLibmenuFilterWithThisRootMenu "Kan inte �ppna 'libmenu video filter' med rotmeny %s.\n"
#define MSGTR_AudioFilterChainPreinitError "Fel vid f�rinitiering av audiofilter!\n"
#define MSGTR_LinuxRTCReadError "'Linux RTC' l�sfel: %s\n"
#define MSGTR_SoftsleepUnderflow "Varning! Softsleep underflow!\n"
#define MSGTR_EDLSKIPStartStopLength "\nEDL_SKIP: start [%f], stopp [%f], l�ngd [%f]\n"
#define MSGTR_AnsSubVisibility "ANS_SUB_VISIBILITY=%ld\n"
#define MSGTR_AnsLength "ANS_LENGTH=%ld\n"
#define MSGTR_AnsVoFullscreen "ANS_VO_FULLSCREEN=%ld\n"
#define MSGTR_AnsPercentPos "ANS_PERCENT_POSITION=%ld\n" 
#define MSGTR_MenuCall "Menyanrop\n"
#define MSGTR_EdlCantUseBothModes "Kan inte anv�nda -edl och -edlout samtidigt.\n"
#define MSGTR_EdlOutOfMem "Kan inte allokera tillr�ckligt med minne f�r att h�lla EDL-data.\n"
#define MSGTR_EdlRecordsNo "L�st %d EDL-funtioner.\n"
#define MSGTR_EdlQueueEmpty "Det �r inga EDL-funktioner att ta hand om.\n"
#define MSGTR_EdlCantOpenForWrite "Kan inte �ppna EDL-fil [%s] f�r skrivning.\n"
#define MSGTR_EdlCantOpenForRead "Kan inte �ppna EDL-fil [%s] f�r l�sning.\n"
#define MSGTR_EdlNOsh_video "Kan inte anv�nda EDL utan video, inaktiverar.\n"
#define MSGTR_EdlNOValidLine "Icke godk�nd EDL-rad: %s\n"
#define MSGTR_EdlBadlyFormattedLine "D�ligt formaterad EDL-rad [%d]. Kastar bort.\n"
#define MSGTR_EdlBadLineOverlap "Senaste stopposition var [%f] ; n�sta start �r [%f]. Noteringar m�ste vara i kronologisk ordning, kan inte lappa �ver. Kastar bort.\n"
#define MSGTR_EdlBadLineBadStop "Stopptid m�ste vara efter starttid.\n"

#define MSGTR_DvdnavNullEvent "DVDNAV-h�ndelse NULL?!\n"
#define MSGTR_DvdnavHighlightEventBroken "DVDNAV-h�ndelse: Highlight-h�ndelse trasig\n" // FIXME highlight
#define MSGTR_DvdnavEvent "DVDNAV-h�ndelse Event: %s\n"
#define MSGTR_DvdnavHighlightHide "DVDNAV-h�ndelse: Highlight g�md\n"
#define MSGTR_DvdnavStillFrame "######################################## DVDNAV-h�ndelse: Fortfarande bildruta: %d sekunder\n"
#define MSGTR_DvdnavNavStop "DVDNAV-h�ndelse: Nav Stop\n" // FIXME Nav Stop?
#define MSGTR_DvdnavNavNOP "DVDNAV-h�ndelse: Nav NOP\n"
#define MSGTR_DvdnavNavSpuStreamChangeVerbose "DVDNAV-h�ndelse: 'Nav SPU'-str�mnings�ndring: fysisk: %d/%d/%d logisk: %d\n"
#define MSGTR_DvdnavNavSpuStreamChange "DVDNAV-h�ndelse: 'Nav SPU'-str�mnings�ndring: fysisk: %d logisk: %d\n"
#define MSGTR_DvdnavNavAudioStreamChange "DVDNAV-h�ndelse: 'Nav Audio'-str�mnings�ndring: fysisk: %d logisk: %d\n"
#define MSGTR_DvdnavNavVTSChange "DVDNAV-h�ndelse: 'Nav VTS' �ndrad\n"
#define MSGTR_DvdnavNavCellChange "DVDNAV-h�ndelse: 'Nav Cell' �ndrad\n"
#define MSGTR_DvdnavNavSpuClutChange "DVDNAV-h�ndelse: 'Nav SPU CLUT' �ndrad\n"
#define MSGTR_DvdnavNavSeekDone "DVDNAV-h�ndelse: 'Nav Seek' �ndrad\n"
/*
 * FIXME A lot of shorted words, not translating atm
 */

#define MSGTR_DvdnavNullEvent "DVDNAV-händelse NULL?!\n"
#define MSGTR_DvdnavHighlightEventBroken "DVDNAV-händelse: Highlight-händelse trasig\n" // FIXME highlight
#define MSGTR_DvdnavEvent "DVDNAV-händelse Event: %s\n"
#define MSGTR_DvdnavHighlightHide "DVDNAV-händelse: Highlight gömd\n"
#define MSGTR_DvdnavStillFrame "######################################## DVDNAV-händelse: Fortfarande bildruta: %d sekunder\n"
#define MSGTR_DvdnavNavStop "DVDNAV-händelse: Nav Stop\n" // FIXME Nav Stop?
#define MSGTR_DvdnavNavNOP "DVDNAV-händelse: Nav NOP\n"
#define MSGTR_DvdnavNavSpuStreamChangeVerbose "DVDNAV-händelse: 'Nav SPU'-strömningsändring: fysisk: %d/%d/%d logisk: %d\n"
#define MSGTR_DvdnavNavSpuStreamChange "DVDNAV-händelse: 'Nav SPU'-strömningsändring: fysisk: %d logisk: %d\n"
#define MSGTR_DvdnavNavAudioStreamChange "DVDNAV-händelse: 'Nav Audio'-strömningsändring: fysisk: %d logisk: %d\n"
#define MSGTR_DvdnavNavVTSChange "DVDNAV-händelse: 'Nav VTS' ändrad\n"
#define MSGTR_DvdnavNavCellChange "DVDNAV-händelse: 'Nav Cell' ändrad\n"
#define MSGTR_DvdnavNavSpuClutChange "DVDNAV-händelse: 'Nav SPU CLUT' ändrad\n"
#define MSGTR_DvdnavNavSeekDone "DVDNAV-händelse: 'Nav Seek' ändrad\n"
/*
 * FIXME A lot of shorted words, not translating atm
 */

// mencoder.c:

#define MSGTR_UsingPass3ControllFile "Anv�nder pass3-kontrollfil: %s\n"
#define MSGTR_MissingFilename "\nFilnamn saknas.\n\n"
#define MSGTR_CannotOpenFile_Device "Kan inte �ppna fil/enhet.\n"
#define MSGTR_CannotOpenDemuxer "Kan inte �ppna demuxer.\n"
#define MSGTR_NoAudioEncoderSelected "\nIngen audioencoder (-oac) vald. V�lj en (se -oac help) eller anv�nd -nosound.\n"
#define MSGTR_NoVideoEncoderSelected "\nIngen videoencoder (-ovc) vald. V�lj en (se -ovc help).\n"
#define MSGTR_CannotOpenOutputFile "Kan inte �ppna utfil '%s'.\n"
#define MSGTR_EncoderOpenFailed "Misslyckade att �ppna encodern.\n"
#define MSGTR_ForcingOutputFourcc "Forcerar utmatning 'fourcc' till %x [%.4s]\n" // FIXME fourcc?
#define MSGTR_WritingAVIHeader "Skriver AVI-filhuvud...\n"
#define MSGTR_DuplicateFrames "\n%d duplicerad bildruta/or!\n"
#define MSGTR_SkipFrame "\nHoppar �ver bildruta!\n"
#define MSGTR_ErrorWritingFile "%s: Fel vid skrivning till fil.\n"
#define MSGTR_WritingAVIIndex "\nSkriver AVI-index...\n"
#define MSGTR_FixupAVIHeader "Fixering AVI-filhuvud...\n" // FIXME fixing?
#define MSGTR_RecommendedVideoBitrate "Rekommenderad videobitrate f�r %s CD: %d\n"
#define MSGTR_VideoStreamResult "\nVideost�m: %8.3f kbit/s  (%d bps)  storlek: %d byte  %5.3f sekunder  %d bildrutor\n"
#define MSGTR_AudioStreamResult "\nAudiost�m: %8.3f kbit/s  (%d bps)  storlek: %d byte  %5.3f sekunder\n"
#define MSGTR_OpenedStream "klart: format: %d  data: 0x%X - 0x%x\n"
#define MSGTR_VCodecFramecopy "videocodec: framecopy (%dx%d %dbpp fourcc=%x)\n" // FIXME translate?
#define MSGTR_ACodecFramecopy "audiocodec: framecopy (format=%x chans=%d rate=%ld bits=%d bps=%ld sample-%ld)\n" // -''-
#define MSGTR_CBRPCMAudioSelected "CBR PCM audio valt\n"
#define MSGTR_MP3AudioSelected "MP3 audio valt\n"
#define MSGTR_CannotAllocateBytes "Kunde inte allokera %d byte\n"
#define MSGTR_SettingAudioDelay "S�tter AUDIO DELAY till %5.3f\n"
#define MSGTR_SettingAudioInputGain "S�tter 'audio input gain' till %f\n" // FIXME to translate?
#define MSGTR_LamePresetEquals "\npreset=%s\n\n" // FIXME translate?
#define MSGTR_LimitingAudioPreload "Begr�nsar audiof�rinladdning till 0.4s\n" // preload?
#define MSGTR_IncreasingAudioDensity "H�jer audiodensitet till 4\n"
#define MSGTR_ZeroingAudioPreloadAndMaxPtsCorrection "Forcerar audiof�rinladdning till 0, 'max pts correction' till 0\n"
#define MSGTR_CBRAudioByterate "\n\nCBR audio: %ld byte/sec, %d byte/block\n"
#define MSGTR_LameVersion "LAME version %s (%s)\n\n"
#define MSGTR_InvalidBitrateForLamePreset "Fel: Angiven bitrate �r utanf�r godk�nd rymd f�r detta val\n"\
    "\n"\
"Vid anv�ndning av detta val s� m�ste du ange ett v�rde mellan \"8\" och \"320\"\n"\
"\n"\
"F�r vidare information testa: \"-lameopts preset=help\"\n"
#define MSGTR_InvalidLamePresetOptions "Fel: du angav inte en godk�nd profil och/eller f�rinst�llda val\n"\
    "\n"\
"Tillg�ngliga profiler �r:\n"\
"\n"\
"   <fast>        standard\n"\
"   <fast>        extreme\n"\
"                 insane\n"\
"   <cbr> (ABR Mode) - ABR-mode �r underf�rst�tt. F�r att anv�nda det,,\n"\
"                      helpt enkelt ange en bitrate. F�r exempel:\n"\
"                      \"preset=185\" aktiverar detta\n"\
"                      f�rinst�llda val, och anv�nder 185 som ett genomsnittlig kbps.\n"\
"\n"\
"    N�gra exempel:\n"\
"\n"\
"       \"-lameopts fast:preset=standard  \"\n"\
" eller \"-lameopts  cbr:preset=192       \"\n"\
" eller \"-lameopts      preset=172       \"\n"\
" eller \"-lameopts      preset=extreme   \"\n"\
"\n"\
"F�r vidare information, f�rs�k: \"-lameopts preset=help\"\n"
#define MSGTR_LamePresetsLongInfo "\n"\
    "De f�rinst�llda switcharna �r designade f�r att f�rs�rja den h�gsta m�jliga kvalit�.\n"\
"\n"\
"De har f�r mestadels blivit utsatta f�r och inst�mmnt via rigor�sa dubbelblindslystningstester\n"\
"f�r att verifiera och �stakomma detta m�l.\n"\
"\n"\
"Dessa �r ideligen uppdaterade f�r att sammantr�ffa med de senaste utveckling som\n"\
"f�rekommer, och som result skulle f�rs�rja dig med bort�t den b�sta kvalit�\n"\
"f�r stunden m�jligt fr�n LAME.\n"\
"\n"\
"F�r att aktivera dessa f�rinst�llda v�rden:\n"\
"\n"\
"   F�r VBR-modes (generellt h�gsta kvalit�) \b:\n"\
"\n"\
"     \"preset=standard\" Denna f�rinst�llning torde generellt vara transparent\n"\
"                             f�r de flesta f�r den mesta musik och har redan\n"\
"                             relativt h�g kvalit�.\n"\
"\n"\
"     \"preset=extreme\" Om du har extremt god h�rsel och liknande utrustning,\n"\
"                             d� kommer denna inst�llning generellt att tillgodose\n"\
"                             n�got h�gre kvalit� �n \"standard\"-inst�llningen\n"\
"\n"\
"   F�r 'CBR 320kbps' (h�gsta m�jliga kvalit� fr�n f�rinst�llningsswitcharna):\n"\
                                                                               "\n"\
"     \"preset=insane\"  Denna f�rinst�llning kommer troligen att vara f�r mycket f�r de\n"\
"                             flesta och de flesta situationer, men om du m�ste absolut\n"\
"                             ha den h�gsta m�jliga kvalit� med inga inv�ndningar om\n"\
"                             filstorleken s� �r detta den v�g att g�.\n"\
"\n"\
"   F�r ABR-modes (h�g kvalit� per given bitrate, men inte s� h�g som f�r VBR) \b:\n"\
"\n"\
"     \"preset=<kbps>\"  Anv�ndning av denna inst�llning vill f�r det mesta ge dig god\n"\
"                             kvalit� vid specifik bitrate, Beroende p� angiven bitrate,\n"\
"                             denna inst�llning kommer att anta den mest optimala inst�llning\n"\
"                             f�r en optimal situation. Fast detta tillv�gag�ngss�tt fungerar,\n"\
"                             s� �r den inte tilln�rmandesvis s� flexibelt som VBR, och f�r det\n"\
"                             mesta s� kommer den inte att komma �t samma niv� av kvalit� som\n"\
"                             VBR vid h�gre bitrate.\n"\
"\n"\
"F�ljande inst�llningar �r �ven tillg�ngliga f�r motsvarande profil:\n"\
"\n"\
"   <fast>        standard\n"\
"   <fast>        extreme\n"\
"                 insane\n"\
"   <cbr> (ABR Mode) - ABR-mode �r underf�rst�tt. F�r att anv�nda det,\n"\
"                      helt enkelt ange en bitrate. F�r exempel:\n"\
"                      \"preset=185\" aktiverar denna inst�llning\n"\
"                      och anv�nder 185 som ett genomsnittlig kbps.\n"\
"\n"\
"   \"fast\" - Aktiverar den nya snabba VBR f�e en speciell profil.\n"\
"            Nackdel till snabbhetsswitchen �r att oftast kommer\n"\
"            bitrate att vara n�got h�gre �n vid 'normal'-mode\n"\
"            och kvalit�n kan �ven bil n�got l�gre.\n"\
"   Varning: Med aktuell version kan 'fast'-inst�llningen resultera i\n"\
"            f�r h�r bitrate i j�mf�relse med ordinarie inst�llning.\n"\
"\n"\
"   \"cbr\"  - Om du anv�nder ABR-mode (l�s ovanst�ende) med en signifikant\n"\
"            bitrate, s�som 80, 96, 112, 128, 160, 192, 224, 256, 320,\n"\
"            du kan anv�nda \"cbr\"-argument f�r att forcera CBR-modeskodning\n"\
"            ist�llet f�r som standard ABR-mode. ABR g�r h�gre kvalit�\n"\
"            men CBR kan vara anv�ndbar i situationer s�som vid str�mmande\n"\
"            av mp3 �ver internet.\n"\
"\n"\
"    Till exempel:\n"\
"\n"\
"    \"-lameopts fast:preset=standard  \"\n"\
" or \"-lameopts  cbr:preset=192       \"\n"\
" or \"-lameopts      preset=172       \"\n"\
" or \"-lameopts      preset=extreme   \"\n"\
"\n"\
"\n"\
"Ett par alias �r tillg�ngliga f�r ABR-mode:\n"\
"phone => 16kbps/mono        phon+/lw/mw-eu/sw => 24kbps/mono\n"\
"mw-us => 40kbps/mono        voice => 56kbps/mono\n"\
"fm/radio/tape => 112kbps    hifi => 160kbps\n"\
"cd => 192kbps               studio => 256kbps"
#define MSGTR_ConfigfileError "konfigurationsfilsfel"
#define MSGTR_ErrorParsingCommandLine "fel vid tolkning av cmdline"
#define MSGTR_VideoStreamRequired "Videostr�m �r obligatoriskt!\n"
#define MSGTR_ForcingInputFPS "'input fps' kommer att bli tolkad som %5.2f ist�llet\n"
#define MSGTR_RawvideoDoesNotSupportAudio "Ut-filformat RAWVIDEO st�djer inte audio - deaktiverar audio\n"
#define MSGTR_DemuxerDoesntSupportNosound "Denna demuxer st�djer inte -nosound �nnu.\n"
#define MSGTR_MemAllocFailed "minnesallokering misslyckades"
#define MSGTR_NoMatchingFilter "Kunde inte finna matchande filter/ao-format!\n"
#define MSGTR_MP3WaveFormatSizeNot30 "sizeof(MPEGLAYER3WAVEFORMAT)==%d!=30, kanske trasig C-kompilator?\n"
#define MSGTR_NoLavcAudioCodecName "Audio LAVC, f�rkommet codecsnamn!\n"
#define MSGTR_LavcAudioCodecNotFound "Audio LAVC, kunde inte finna encoder f�r codec %s\n"
#define MSGTR_CouldntAllocateLavcContext "Audio LAVC, kunde inte allokera kontext!\n"
#define MSGTR_CouldntOpenCodec "Kunde inte �ppna codec %s, br=%d\n"

// cfg-mencoder.h:

#define MSGTR_MEncoderMP3LameHelp "\n\n"\
    " vbr=<0-4>     variabel bitrate metod\n"\
"                0: cbr\n"\
"                1: mt\n"\
"                2: rh(default)\n"\
"                3: abr\n"\
"                4: mtrh\n"\
"\n"\
" abr           medelbitrate\n"\
"\n"\
" cbr           konstant bitrate\n"\
"               �ven forcerar CBR-modeskodning p� subsequentiellt ABR-inst�llningsl�ge.\n"\
"\n"\
" br=<0-1024>   specifierar bitrate i kBit (CBR och ABR endast)\n"\
"\n"\
" q=<0-9>       kvalit� (0-h�gst, 9-l�gst) (endast f�r VBR)\n"\
"\n"\
" aq=<0-9>      algoritmiskt kvalit� (0-b�st/segast, 9-s�mst/snabbast)\n"\
"\n"\
" ratio=<1-100> kompressionsratio\n"\
"\n"\
" vol=<0-10>    s�tt audio-in-�kning\n"\
"\n"\
" mode=<0-3>    (standard: auto)\n"\
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
" fast          Aktivera snabbare kodning p� subsequentiellt VBR-inst�llningsl�ge,\n"\
"               n�got l�gre kvalit� och h�gre bitrate.\n"\
"\n"\
" preset=<value> Tillhandah�ller den h�gsta tillg�ngliga kvalit�tsinst�llning.\n"\
"                 medium: VBR  kodning, godkvalit�\n"\
"                 (150-180 kbps bitratesrymd)\n"\
"                 standard:  VBR kodning, h�g kvalit�\n"\
"                 (170-210 kbps bitratesrymd)\n"\
"                 extreme: VBR kodning, mycket h�g kvalit�\n"\
"                 (200-240 kbps bitratesrymd)\n"\
"                 insane:  CBR  kodning, h�gsta f�rinst�lld kvalit�\n"\
"                 (320 kbps bitrate)\n"\
"                 <8-320>: ABR kodning vid i medeltal angiven bitrate (kbps).\n\n"

//codec-cfg.c:

#define MSGTR_DuplicateFourcc "duplicerad FourCC"
#define MSGTR_TooManyFourccs "f�r m�nga FourCCs/format..."
#define MSGTR_ParseError "tolkningsfel"
#define MSGTR_ParseErrorFIDNotNumber "tolkningsfel (format-ID �r inget nummer?)"
#define MSGTR_ParseErrorFIDAliasNotNumber "tolkningsfel (format-ID-alias �r inget nummer?)"
#define MSGTR_DuplicateFID "duplicerade format-ID"
#define MSGTR_TooManyOut "f�r m�nga ut..." //FIXME "to many out"?
#define MSGTR_InvalidCodecName "\ncodec(%s) namn �r icke godk�nt!\n"
#define MSGTR_CodecLacksFourcc "\ncodec(%s) har inte FourCC/format!\n"
#define MSGTR_CodecLacksDriver "\ncodec(%s) har ingen drivrutin!\n"
#define MSGTR_CodecNeedsDLL "\ncodec(%s) beh�ver en 'dll'!\n"
#define MSGTR_CodecNeedsOutfmt "\ncodec(%s) beh�ver en 'outfmt'!\n"
#define MSGTR_CantAllocateComment "Kan inte allokera minne flr kommentar. "
#define MSGTR_GetTokenMaxNotLessThanMAX_NR_TOKEN "get_token() \b: max >= MAX_MR_TOKEN!" //FIXME translate?
#define MSGTR_ReadingFile "L�ser %s: "
#define MSGTR_CantOpenFileError "Kan inte �ppna '%s': %s\n"
#define MSGTR_CantGetMemoryForLine "Kan inte f� minne f�r 'line': %s\n"
#define MSGTR_CantReallocCodecsp "Kan inte realloc '*codecsp': %s\n"
#define MSGTR_CodecNameNotUnique "Codec namn '%s' �r inte unikt."
#define MSGTR_CantStrdupName "Kan inte strdup -> 'name': %s\n"
#define MSGTR_CantStrdupInfo "Kan inte strdup -> 'info': %s\n"
#define MSGTR_CantStrdupDriver "Kan inte strdup -> 'driver': %s\n"
#define MSGTR_CantStrdupDLL "Kan inte strdup -> 'dll': %s"
#define MSGTR_AudioVideoCodecTotals "%d audio & %d video codecs\n"
#define MSGTR_CodecDefinitionIncorrect "Codec �r inte definerad korrekt."
#define MSGTR_OutdatedCodecsConf "Denna codecs.conf �r f�r gammal och inkompatibel med denna MPlayer version!" // release is more like 'sl�pp', sounds wrong, using version instead

// open.c, stream.c:
#define MSGTR_CdDevNotfound "CD-ROM-enhet '%s' ej funnet.\n"
#define MSGTR_ErrTrackSelect "Fel vid val av VCD-sp�r."
#define MSGTR_ReadSTDIN "L�ser fr�n stdin...\n"
#define MSGTR_UnableOpenURL "Of�rm�gen att �ppna URL: %s\n"
#define MSGTR_ConnToServer "Ansluten till server: %s\n"
#define MSGTR_FileNotFound "Fil ej funnen: '%s'\n"

#define MSGTR_SMBInitError "Kan inte initiera libsmbclient-bilioteket: %d\n"
#define MSGTR_SMBFileNotFound "Kunde inte �ppna fr�n LAN: '%s'\n"
#define MSGTR_SMBNotCompiled "MPlayer var inte kompilerad med SMB-l�sst�d.\n"

#define MSGTR_CantOpenDVD "Kunde inte �ppna DVD-enhet: %s\n"
#define MSGTR_DVDwait "L�ser diskstruktur, var god dr�j...\n"
#define MSGTR_DVDnumTitles "Det �r %d titlar p� denna DVD.\n"
#define MSGTR_DVDinvalidTitle "Icke godk�nt DVD-titelnummer: %d\n"
#define MSGTR_DVDnumChapters "Der �r %d kapitel p� denna DVD-titel.\n"
#define MSGTR_DVDinvalidChapter "Ej godk�nt DVD-kapitelnummer: %d\n"
#define MSGTR_DVDnumAngles "Det �r %d vinkar p� denna DVD-titel.\n"
#define MSGTR_DVDinvalidAngle "Ej godk�nd DVD-vinkelsnummer: %d\n"
#define MSGTR_DVDnoIFO "Kan inte �ppna IFO-fil f�r DVD-titel %d.\n"
#define MSGTR_DVDnoVOBs "Kunde inte �ppna titel VOBS (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD �ppnades problemfritt.\n"

// demuxer.c, demux_*.c:

#define MSGTR_AudioStreamRedefined "VARNING: Audiostr�msfilhuvud %d omdefinerad.\n"
#define MSGTR_VideoStreamRedefined "WARNING: Videostr�msfilhuvud %d omdefinerad.\n"
#define MSGTR_TooManyAudioInBuffer "\nAllt f�r m�nga audiopaket i bufferten: (%d i %d byte).\n"
#define MSGTR_TooManyVideoInBuffer "\nAllt f�r m�nga videopaket i bufferten: (%d i %d byte).\n"
#define MSGTR_MaybeNI "Kanske f�rs�ker du spela upp en icke-interleaved str�m/fil, eller s� har decodern falierat?\n" \
    "F�r AVI-filer, f�rs�k med att forcera icke-interleaved-l�gen med -ni argumentet.\n" // FIXME non-interleaved
#define MSGTR_SwitchToNi "\nSv�rt interleaved AVI-fil detekterad, g�r �ver till '-ni'-l�ge...\n"
#define MSGTR_Detected_XXX_FileFormat "%s filformat detekterat.\n"
#define MSGTR_DetectedAudiofile "Audiofilformat detekterat.\n"
#define MSGTR_NotSystemStream "Icke 'MPEG System Stream'-format... (kanske Transport Stream?)\n"
#define MSGTR_InvalidMPEGES "Icke godk�nd 'MPEG-ES'-str�m??? Kontakta upphovsmannen, det kanske �r en bugg :(\n" //FIXME author???
#define MSGTR_FormatNotRecognized "================ Tyv�rr, detta filformat �r inte rekogniserbart/st�tt ==================\n"\
    "=== Om denna fil �r en AVi, ASF eller MPEG-str�m, var v�nlig kontakta upphovsmannen! ===\n" //FIXME author???
#define MSGTR_MissingVideoStream "Ingen videostr�m funnen.\n"
#define MSGTR_MissingAudioStream "Ingen audiostr�m funnen -> inget ljud.\n"
#define MSGTR_MissingVideoStreamBug "Saknar videostr�m!? Kontakta upphovsmannen, det kan vara en bugg :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: Fil inneh�ller ej den valda audio- eller videostr�mmen.\n"

#define MSGTR_NI_Forced "Forcerad"
#define MSGTR_NI_Detected "P�visad" // FIXME right to say?
#define MSGTR_NI_Message "%s 'NON-INTERLEAVED AVI'-filformat.\n"

#define MSGTR_UsingNINI "Anv�nder trasig 'NON-INTERLEAVED AVI'-filformat.\n"
#define MSGTR_CouldntDetFNo "Kunde inte avg�ra antalet bildrutor (f�r absolut s�kning).\n"
#define MSGTR_CantSeekRawAVI "Kan inte s�ka i r�a AVI-str�mmar. (Index kr�vs, f�rs�k med '-idx'-switchen.)\n"
#define MSGTR_CantSeekFile "Kan inte s�ka i denna fil.\n"

#define MSGTR_EncryptedVOB "Krypterad VOB-fil! Read DOCS/HTML/en/dvd.html.\n"

#define MSGTR_MOVcomprhdr "MOV: filhuvudkomprimeringssupport kr�ver ZLIB!\n"
#define MSGTR_MOVvariableFourCC "MOV: VARNING: Variabel FOURCC p�visad!?\n"
#define MSGTR_MOVtooManyTrk "MOV: VARNING: allt f�rm�nga sp�r"
#define MSGTR_FoundAudioStream "==> Fann audiostr�m: %d\n"
#define MSGTR_FoundVideoStream "==> Fann videostr�m: %d\n"
#define MSGTR_DetectedTV "TV p�visad! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "Of�rm�gen att �ppna oggdemuxern.\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: S�ker efter audiostr�m (id:%d).\n"
#define MSGTR_CannotOpenAudioStream "Kan inte �ppna audiostr�m: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "Kan inte �ppna textningsstr�m: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "Misslyckades att �ppna audiodemuxern: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "Misslyckades att �ppna textningsdemuxern: %s\n"
#define MSGTR_TVInputNotSeekable "TV-in �r inte s�kbar! (S�kning kommer troligen bli f�r att �ndra kanal ;)\n"
#define MSGTR_DemuxerInfoAlreadyPresent "Demuxerinfo %s redan inst�lld!\n"
#define MSGTR_ClipInfo "Clip-info:\n"

#define MSGTR_LeaveTelecineMode "\ndemux_mpg: '30fps NTSC'-inneh�ll uppt�ckt, �ndrar framerate.\n" // FIXME framerate?
#define MSGTR_EnterTelecineMode "\ndemux_mpg: '24fps progressive NTSC'-inneh�ll uppt�ckt, �ndrar framerate.\n" // -''-

// dec_video.c & dec_audio.c:

#define MSGTR_CantOpenCodec "Kunde inte �ppna codec.\n"
#define MSGTR_CantCloseCodec "Kunde inte st�nga codec\n"

#define MSGTR_MissingDLLcodec "FEL: Kunde inte �ppna obligatorisk DirecShow-codec %s.\n"
#define MSGTR_ACMiniterror "Kunde inte ladda/initiera 'Win32/ACM AUDIO'-codec (saknas Dll-fil?).\n"
#define MSGTR_MissingLAVCcodec "Kunde inte finna codec '%s' i libavcodec...\n"

#define MSGTR_MpegNoSequHdr "MPEG: FATAL: EOF under s�kning efter sequencefilhuvuden\n" // FIXME sequence?
#define MSGTR_CannotReadMpegSequHdr "FATAL: Kunde inte l�sa sequencefilhuvud\n"
#define MSGTR_CannotReadMpegSequHdrEx "FATAL: Kunde inte l�sa sequencefilhuvudstill�gg.\n"
#define MSGTR_BadMpegSequHdr "MPEG: d�lig sequencefilhuvud\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: d�lig sequencefilhuvudstill�gg\n"

#define MSGTR_ShMemAllocFail "Kunde inte allokera delat minne.\n"
#define MSGTR_CantAllocAudioBuf "Kunde inte allokera audio-ut-buffert.\n"

#define MSGTR_UnknownAudio "Ok�nd/saknad audioformat -> inget ljud\n"

#define MSGTR_UsingExternalPP "[PP] Anv�nder externt postprocesseringsfiler, max q = %d.\n"
#define MSGTR_UsingCodecPP "[PP] Anv�nder codecens postprocessing, max q = %d.\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "Videoattribut '%s' har inget st�d hos vald vo & vd.\n" // FIXME more info? vo & vd
#define MSGTR_VideoCodecFamilyNotAvailableStr "Beg�rd videocodecfamilj [%s] (vfm=%s) �r ej tillg�nglig.\nAktivera det vil kompilation.\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "Beg�rd audiocodecfamilj [%s] (afm=%s) �r ej tillg�nglig.\nAktivera det vil kompilation.\n"
#define MSGTR_OpeningVideoDecoder "�ppnar videodecoder: [%s] %s\n"
#define MSGTR_OpeningAudioDecoder "�ppnar audiodecoder: [%s] %s\n"
#define MSGTR_UninitVideoStr "uninit video: %s\n" // FIXME translate?
#define MSGTR_UninitAudioStr "uninit audio: %s\n" // -''-
#define MSGTR_VDecoderInitFailed "VDecoder-initiering misslyckades :(\n" // FIXME VDecoder something special or just a shortcut?
#define MSGTR_ADecoderInitFailed "ADecoder-initiering misslyckades :(\n" // -''-
#define MSGTR_ADecoderPreinitFailed "ADecoder-preinitiering misslyckades :(\n" // -''-
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: Allokerar %d byte f�r inbuffert.\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: Allokerar %d + %d = %d byte f�r utbuffert.\n"

// LIRC:

#define MSGTR_SettingUpLIRC "Aktiverar LIRC-st�d...\n"
#define MSGTR_LIRCdisabled "Du kommer inte att kunna anv�nda din fj�rrkontroll.\n"
#define MSGTR_LIRCopenfailed "Misslyckades med att aktivera LIRC-st�d.\n"
#define MSGTR_LIRCcfgerr "Misslyckades med att l�sa LIRC-konfigurationsfil %s.\n"

// vf.c

#define MSGTR_CouldNotFindVideoFilter "Kunde inte finna videofilter '%s'.\n"
#define MSGTR_CouldNotOpenVideoFilter "Kunde inte �ppna videofilter '%s'.\n"
#define MSGTR_OpeningVideoFilter "�ppnar videofilter: "
#define MSGTR_CannotFindColorspace "Kunde inte hitta matchande f�rgrymder, t.o.m. vid ins�ttning av 'scale' :(\n" // FIXME colorspace

// vd.c

#define MSGTR_CodecDidNotSet "VDec: Codec satt inte sh->disp_w samt sh->disp_h, f�rs�ker g� runt problemet.\n"
#define MSGTR_VoConfigRequest "VDec: vo-konfigurationsbeg�ran - %d x %d (preferred csp: %s)\n"
#define MSGTR_CouldNotFindColorspace "Kunde inte finna matchande f�rgrymder - f�rs�ker �ter med -vf scale...\n" // -''-
#define MSGTR_MovieAspectIsSet "Movie-Aspect �r %.2f:1 - prescaling till korrekt film-aspect.\n"
#define MSGTR_MovieAspectUndefined "Film-Aspect �r ej definerad - ingen prescaling kommer att �ga rum.\n"

// vd_dshow.c, vd_dmo.c

#define MSGTR_DownloadCodecPackage "Du m�ste uppgradera/installera de bin�ra codecspaketen.\nG� till http://mplayerhq.hu/homepage/dload.html\n"
#define MSGTR_DShowInitOK "INFO: 'Win32/DShow'-videocodecinitiering: OK.\n"
#define MSGTR_DMOInitOK "INFO: 'Win32/DMO'-videocodecinitiering: OK.\n"

// x11_common.c

#define MSGTR_EwmhFullscreenStateFailed "\nX11: Kunde inte s�nda EWMH-fullsk�rmsh�ndelse!\n"

#define MSGTR_InsertingAfVolume "[Mixer] Ingen h�rdvarumixning, l�gger till volymfilter.\n"
#define MSGTR_NoVolume "[Mixer] Ingen volymkontroll tillg�nglig.\n"


// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "Om"
#define MSGTR_FileSelect "V�lj fil..."
#define MSGTR_SubtitleSelect "V�lj textning..."
#define MSGTR_OtherSelect "V�lj..."
#define MSGTR_AudioFileSelect "V�lj extern audiokanal..."
#define MSGTR_FontSelect "V�lj font..."
#define MSGTR_PlayList "Spellista"
#define MSGTR_Equalizer "Equalizer" 
#define MSGTR_SkinBrowser "Skinl�sare"
#define MSGTR_Network "N�tverksstr�mning..."
#define MSGTR_Preferences "Inst�llningar"
#define MSGTR_AudioPreferences "Audiodirvrutinskonfiguration"
#define MSGTR_NoMediaOpened "Inget media �ppnad"
#define MSGTR_VCDTrack "VCD-sp�r %d"
#define MSGTR_NoChapter "Inget kapitel"
#define MSGTR_Chapter "Kapitel %d"
#define MSGTR_NoFileLoaded "Ingen fil laddad"

// --- buttons ---

#define MSGTR_Ok "OK"
#define MSGTR_Cancel "Avbryt"
#define MSGTR_Add "L�gg till"
#define MSGTR_Remove "Radera"
#define MSGTR_Clear "Rensa"
#define MSGTR_Config "Konfiguration"
#define MSGTR_ConfigDriver "Konfigurera drivrution"
#define MSGTR_Browse "Bl�ddra"

// --- error messages ---

#define MSGTR_NEMDB "Tyv�rr, inte tillr�ckligt minne f�r ritbuffert."
#define MSGTR_NEMFMR "Tyv�rr, inte tillr�ckligt minne f�r menyrendering."
#define MSGTR_IDFGCVD "Tyv�rr, jag hittade inte en GUI-kompatibel video-ut-drivrutin."
#define MSGTR_NEEDLAVCFAME "Tyv�rr, du kan inte spela icke-MPEG-filer med ditt DXR3/H+-enhet utan omkodning.\nVar god aktivera lavc eller fame i 'DXR3/H+'-konfigurationsboxen."

// --- skin loader error messages

#define MSGTR_SKIN_ERRORMESSAGE "[skin] fel i skinkonfigureringsfil p� rad %d: %s"
#define MSGTR_SKIN_WARNING1 "[skin] varning i konfigurationsfil p� rad %d:\nwidget (%s) funnen, men ingen \"section\" funnen f�re"
#define MSGTR_SKIN_WARNING2 "[skin] varning i konfigurationsfil p� rad %d:\nwidget (%s) funnen, men ingen \"subsection\" funnen f�re"
#define MSGTR_SKIN_WARNING3 "[skin] varning i konfigurationsfil p� rad %d:\ndenna undersektion st�djs inte av widget (%s)"
#define MSGTR_SKIN_BITMAP_16bit  "16-bitar eller l�gre bitmappar st�djs inte (%s).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "fil ej funnen (%s)\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "BMP l�sfel (%s)\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "TGA l�sfel (%s)\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "PNG l�sfel (%s)\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "RLE-packad TGA st�djs ej (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "ok�nd filtyp (%s)\n"
#define MSGTR_SKIN_BITMAP_ConvertError "24-bitars till 32-bitars konverteringsfel (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "ok�nt meddelande: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "ej tillr�ckligt minne\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "Allt f�r m�nga fonter deklarerade.\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "Fontfil ej funnen.\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "Fontbildsfil ej funnen.\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "icke-existerande fontidentifkator (%s)\n"
#define MSGTR_SKIN_UnknownParameter "ok�nd parameter (%s)\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "Skin ej funnen (%s).\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "skinkonfigurationsfilsl�sfel (%s)\n"
#define MSGTR_SKIN_LABEL "Skin:"

// --- gtk menus

#define MSGTR_MENU_AboutMPlayer "Om MPlayer"
#define MSGTR_MENU_Open "�ppna..."
#define MSGTR_MENU_PlayFile "Spela fil..."
#define MSGTR_MENU_PlayVCD "Spela VCD..."
#define MSGTR_MENU_PlayDVD "Spela DVD..."
#define MSGTR_MENU_PlayURL "Spela URL..."
#define MSGTR_MENU_LoadSubtitle "Ladda textning..."
#define MSGTR_MENU_DropSubtitle "Droppa textning..."
#define MSGTR_MENU_LoadExternAudioFile "Ladda extern audiofil..."
#define MSGTR_MENU_Playing "Spelar"
#define MSGTR_MENU_Play "Spela"
#define MSGTR_MENU_Pause "Pausa"
#define MSGTR_MENU_Stop "Stopp"
#define MSGTR_MENU_NextStream "N�sta str�m"
#define MSGTR_MENU_PrevStream "F�reg�ende str�m"
#define MSGTR_MENU_Size "Storlek"
#define MSGTR_MENU_NormalSize "Normal storlek"
#define MSGTR_MENU_DoubleSize "Dubbel storlek"
#define MSGTR_MENU_FullScreen "Fullsk�rm"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "�ppnar disk..." // FIXME to open or is opening?
#define MSGTR_MENU_ShowDVDMenu "Visa DVD-meny"
#define MSGTR_MENU_Titles "Titlar"
#define MSGTR_MENU_Title "Titel %2d"
#define MSGTR_MENU_None "(ingen)"
#define MSGTR_MENU_Chapters "Kapitel"
#define MSGTR_MENU_Chapter "Kapitel %2d"
#define MSGTR_MENU_AudioLanguages "Audiospr�k"
#define MSGTR_MENU_SubtitleLanguages "Textningsspr�k"
#define MSGTR_MENU_PlayList "Spellista"
#define MSGTR_MENU_SkinBrowser "Skinl�sare"
#define MSGTR_MENU_Preferences "Inst�llningar"
#define MSGTR_MENU_Exit "Avsluta..."
#define MSGTR_MENU_Mute "D�mpa"
#define MSGTR_MENU_Original "Orginal"
#define MSGTR_MENU_AspectRatio "Aspect ratio" // FIXME translate?
#define MSGTR_MENU_AudioTrack "Audiosp�r"
#define MSGTR_MENU_Track "Sp�r %d"
#define MSGTR_MENU_VideoTrack "Videosp�r"

// --- equalizer

#define MSGTR_EQU_Audio "Audio"
#define MSGTR_EQU_Video "Video"
#define MSGTR_EQU_Contrast "Kontrast: "
#define MSGTR_EQU_Brightness "Ljusstyrka: "
#define MSGTR_EQU_Hue "Hue: "
#define MSGTR_EQU_Saturation "Saturation: "
#define MSGTR_EQU_Front_Left "V�nster fram"
#define MSGTR_EQU_Front_Right "H�ger fram"
#define MSGTR_EQU_Back_Left "V�nster bak"
#define MSGTR_EQU_Back_Right "H�ger bak"
#define MSGTR_EQU_Center "Center"
#define MSGTR_EQU_Bass "Bass"
#define MSGTR_EQU_All "Allt"
#define MSGTR_EQU_Channel1 "Kanal 1:"
#define MSGTR_EQU_Channel2 "Kanal 2:"
#define MSGTR_EQU_Channel3 "Kanal 3:"
#define MSGTR_EQU_Channel4 "Kanal 4:"
#define MSGTR_EQU_Channel5 "Kanal 5:"
#define MSGTR_EQU_Channel6 "Kanal 6:"

// --- playlist

#define MSGTR_PLAYLIST_Path "S�kv�g"
#define MSGTR_PLAYLIST_Selected "Valda filer"
#define MSGTR_PLAYLIST_Files "Filer"
#define MSGTR_PLAYLIST_DirectoryTree "Katalogtr�d"

// --- preferences

#define MSGTR_PREFERENCES_Audio "Audio"
#define MSGTR_PREFERENCES_Video "Video"
#define MSGTR_PREFERENCES_SubtitleOSD "Textning & OSD"
#define MSGTR_PREFERENCES_Codecs "Codecs & demuxer"
#define MSGTR_PREFERENCES_Misc "Diverse"

#define MSGTR_PREFERENCES_None "Inget"
#define MSGTR_PREFERENCES_DriverDefault "standarddrivrutin"
#define MSGTR_PREFERENCES_AvailableDrivers "Tillg�ngliga drivrutioner:"
#define MSGTR_PREFERENCES_DoNotPlaySound "Spela inte upp ljud"
#define MSGTR_PREFERENCES_NormalizeSound "Normalizera ljud"
#define MSGTR_PREFERENCES_EnEqualizer "AKtivera equalizer"
#define MSGTR_PREFERENCES_ExtraStereo "Aktivera extra stereo"
#define MSGTR_PREFERENCES_Coefficient "Koefficient:"
#define MSGTR_PREFERENCES_AudioDelay "Audiof�rdr�jning"
#define MSGTR_PREFERENCES_DoubleBuffer "Aktivera double buffering"
#define MSGTR_PREFERENCES_DirectRender "Aktivera direct rendering"
#define MSGTR_PREFERENCES_FrameDrop "Aktivera frame dropping"
#define MSGTR_PREFERENCES_HFrameDrop "Aktivera H�RD frame dropping (dangerous)"
#define MSGTR_PREFERENCES_Flip "Flippa bilden uppochner"
#define MSGTR_PREFERENCES_Panscan "Panscan: "
#define MSGTR_PREFERENCES_OSDTimer "Timers och indikatorer"
#define MSGTR_PREFERENCES_OSDProgress "Tillst�ndsrad endast"
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "Timer, procent och total tid"
#define MSGTR_PREFERENCES_Subtitle "Textning:"
#define MSGTR_PREFERENCES_SUB_Delay "F�rdr�jning: "
#define MSGTR_PREFERENCES_SUB_FPS "FPS:"
#define MSGTR_PREFERENCES_SUB_POS "Position: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "Deaktivera automatisk laddning av textning"
#define MSGTR_PREFERENCES_SUB_Unicode "Unicodetextning"
#define MSGTR_PREFERENCES_SUB_MPSUB "Konvertera given text till MPlayers egna textningsformat"
#define MSGTR_PREFERENCES_SUB_SRT "Konvertera given text till det tidbaserade SubViewer (SRT) formatet"
#define MSGTR_PREFERENCES_SUB_Overlap "Aktivera textnings�verlappning"
#define MSGTR_PREFERENCES_Font "Font:"
#define MSGTR_PREFERENCES_FontFactor "Fontfaktor:"
#define MSGTR_PREFERENCES_PostProcess "Aktivera postprocessing"
#define MSGTR_PREFERENCES_AutoQuality "Autokvalit�: "
#define MSGTR_PREFERENCES_NI "Anv�nd non-interleaved AVI tolk"
#define MSGTR_PREFERENCES_IDX "�terbygg indextabell, om s� beh�vs"
#define MSGTR_PREFERENCES_VideoCodecFamily "Videocodecfamilj:"
#define MSGTR_PREFERENCES_AudioCodecFamily "Audiocodecfamilj:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "OSD-niv�"
#define MSGTR_PREFERENCES_FRAME_Subtitle "Textning"
#define MSGTR_PREFERENCES_FRAME_Font "Font"
#define MSGTR_PREFERENCES_FRAME_PostProcess "Postprocessing"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "Codec & demuxer"
#define MSGTR_PREFERENCES_FRAME_Cache "Cache"
#define MSGTR_PREFERENCES_FRAME_Misc "Diverse"
#define MSGTR_PREFERENCES_Audio_Device "Enhet:"
#define MSGTR_PREFERENCES_Audio_Mixer "Mixer:"
#define MSGTR_PREFERENCES_Audio_MixerChannel "Mixerkanal:"
#define MSGTR_PREFERENCES_Message "Var god komih�g att du m�ste starta om uppspelning f�r att vissa �ndringar ska ta effekt!"
#define MSGTR_PREFERENCES_DXR3_VENC "Videoencoder:"
#define MSGTR_PREFERENCES_DXR3_LAVC "ANv�nd LAVC (FFmpeg)"
#define MSGTR_PREFERENCES_DXR3_FAME "Anv�nd FAME"

#define MSGTR_PREFERENCES_FontEncoding1 "Unicode"
#define MSGTR_PREFERENCES_FontEncoding2 "V�steuropeiska spr�k (ISO-8859-1)"
#define MSGTR_PREFERENCES_FontEncoding3 "V�steuropeiska spr�k med Euro (ISO-8859-15)"
#define MSGTR_PREFERENCES_FontEncoding4 "Slaviska/Centraleuropeiska spr�k (ISO-8859-2)"
#define MSGTR_PREFERENCES_FontEncoding5 "Esperanto, Galician, Maltese, Turkiska (ISO-8859-3)" // FIXME Galician, Maltese
#define MSGTR_PREFERENCES_FontEncoding6 "�ldre baltisk teckenupps�ttning (ISO-8859-4)"
#define MSGTR_PREFERENCES_FontEncoding7 "Kyrilliska (ISO-8859-5)"
#define MSGTR_PREFERENCES_FontEncoding8 "Arabiska (ISO-8859-6)"
#define MSGTR_PREFERENCES_FontEncoding9 "Modern grekiska (ISO-8859-7)"
#define MSGTR_PREFERENCES_FontEncoding10 "Turkiska (ISO-8859-9)"
#define MSGTR_PREFERENCES_FontEncoding11 "Baltiska (ISO-8859-13)"
#define MSGTR_PREFERENCES_FontEncoding12 "Celtiska (ISO-8859-14)"
#define MSGTR_PREFERENCES_FontEncoding13 "Hebrew teckenupps�ttningar (ISO-8859-8)"
#define MSGTR_PREFERENCES_FontEncoding14 "Ryska (KOI8-R)"
#define MSGTR_PREFERENCES_FontEncoding15 "Ukrainska, Vitrysska (KOI8-U/RU)"
#define MSGTR_PREFERENCES_FontEncoding16 "Enkel Kinesisk teckenupps�ttning (CP936)"
#define MSGTR_PREFERENCES_FontEncoding17 "Traditionell Kinesisk teckenupps�ttning (BIG5)"
#define MSGTR_PREFERENCES_FontEncoding18 "Japansk teckenupps�ttning (SHIFT-JIS)"
#define MSGTR_PREFERENCES_FontEncoding19 "Koreansk teckenupps�ttning (CP949)"
#define MSGTR_PREFERENCES_FontEncoding20 "Thail�nsk teckenupps�ttning (CP874)"
#define MSGTR_PREFERENCES_FontEncoding21 "Kyrilliska Windown (CP1251)"
#define MSGTR_PREFERENCES_FontEncoding22 "Slaviska/Centraleuropeiska Windows (CP1250)"

#define MSGTR_PREFERENCES_FontNoAutoScale "Ingen autoskalning"
#define MSGTR_PREFERENCES_FontPropWidth "Propotionellt mot filmbredd"
#define MSGTR_PREFERENCES_FontPropHeight "Propotionellt mot filmh�jd"
#define MSGTR_PREFERENCES_FontPropDiagonal "Propotionellt mot filmdiagonalen"
#define MSGTR_PREFERENCES_FontEncoding "Kodning:"
#define MSGTR_PREFERENCES_FontBlur "Blur:"
#define MSGTR_PREFERENCES_FontOutLine "Outline:"
#define MSGTR_PREFERENCES_FontTextScale "Textskalning:"
#define MSGTR_PREFERENCES_FontOSDScale "OSDskalning:"
#define MSGTR_PREFERENCES_Cache "Cache p�/av"
#define MSGTR_PREFERENCES_CacheSize "Cachestorlek: "
#define MSGTR_PREFERENCES_LoadFullscreen "Starta i fullsk�rm"
#define MSGTR_PREFERENCES_SaveWinPos "Spara f�nsterposition"
#define MSGTR_PREFERENCES_XSCREENSAVER "Stoppa XScreenSaver"
#define MSGTR_PREFERENCES_PlayBar "Aktivera spelindikator"
#define MSGTR_PREFERENCES_AutoSync "AutoSync p�/av"
#define MSGTR_PREFERENCES_AutoSyncValue "Autosync: "
#define MSGTR_PREFERENCES_CDROMDevice "CD-ROM-enhet:"
#define MSGTR_PREFERENCES_DVDDevice "DVD-enhet:"
#define MSGTR_PREFERENCES_FPS "Film-FPS:"
#define MSGTR_PREFERENCES_ShowVideoWindow "Visa videof�nster n�r den �r inaktiv"

#define MSGTR_ABOUT_UHU "GUI-utveckling sponstrat av UHU Linux\n"
#define MSGTR_ABOUT_CoreTeam "   MPlayers k�rngrupp:\n"
#define MSGTR_ABOUT_AdditionalCoders "   �vriga kodare:\n"
#define MSGTR_ABOUT_MainTesters "    Huvudsakliga testare:\n"

// --- messagebox

#define MSGTR_MSGBOX_LABEL_FatalError "O�verkomligt fel!"
#define MSGTR_MSGBOX_LABEL_Error "Fel!"
#define MSGTR_MSGBOX_LABEL_Warning "Varning!"

#endif

// ======================= VO Video Output drivers ========================

#define MSGTR_VOincompCodec "Vald video-ut-enhet �r inte kompatibel med denna codec.\n"
#define MSGTR_VO_GenericError "Detta fel har intr�ffat"
#define MSGTR_VO_UnableToAccess "Kan inte accessa"
#define MSGTR_VO_ExistsButNoDirectory "finns redan, men �r inte en katalog."
#define MSGTR_VO_DirExistsButNotWritable "Ut-katalog finns redan, men �r inte skrivbar."
#define MSGTR_VO_DirExistsAndIsWritable "Utkatalog finns redan och �r skrivbar."
#define MSGTR_VO_CantCreateDirectory "Of�rm�gen att skapa ut-katalog."
#define MSGTR_VO_CantCreateFile "Of�rm�gen att skapa utfil."
#define MSGTR_VO_DirectoryCreateSuccess "Ut-katalog skapad."
#define MSGTR_VO_ParsingSuboptions "Tolkar suboptions." // FIXME suboptions?
#define MSGTR_VO_SuboptionsParsedOK "Suboptions tolkad OK." // -''-
#define MSGTR_VO_ValueOutOfRange "V�rden utanf�r godk�nd rymd"
#define MSGTR_VO_NoValueSpecified "Inget v�rde angett."
#define MSGTR_VO_UnknownSuboptions "Ok�nd suboption" // -''-

// vo_jpeg.c

#define MSGTR_VO_JPEG_ProgressiveJPEG "'Progressive JPEG' aktiverat."
#define MSGTR_VO_JPEG_NoProgressiveJPEG "'Progressive JPEG' deaktiverat."
#define MSGTR_VO_JPEG_BaselineJPEG "'Baseline JPEG' aktiverat."
#define MSGTR_VO_JPEG_NoBaselineJPEG "'Baseline JPEG' deaktiverat."

// vo_pnm.c

#define MSGTR_VO_PNM_ASCIIMode "ASCII-mode aktiverat."
#define MSGTR_VO_PNM_RawMode "R�tt-mode aktiverat." // FIXME R�tt sounds strange
#define MSGTR_VO_PNM_PPMType "Kommer att skriva PPM-filer."
#define MSGTR_VO_PNM_PGMType "Kommer att skriva PGM-filer."
#define MSGTR_VO_PNM_PGMYUVType "Kommer att skriva PGMYUV-filer."

// vo_yuv4mpeg.c

#define MSGTR_VO_YUV4MPEG_InterlacedHeightDivisibleBy4 "'Interlaced'-mode kr�ver bildh�jd som �r delbar med 4." // FIXME interlaced?
#define MSGTR_VO_YUV4MPEG_InterlacedLineBufAllocFail "Of�rm�gen att allokera linjebufferrt f�r interlaced-mode."
#define MSGTR_VO_YUV4MPEG_InterlacedInputNotRGB "indata �r ej i RGB-format, kan inte separera 'chrominance' via f�lt!" // FIXME chrominance
#define MSGTR_VO_YUV4MPEG_WidthDivisibleBy2 "Bildbredd m�ste vara delbart med 2."
#define MSGTR_VO_YUV4MPEG_NoMemRGBFrameBuf "Ej tillr�ckligt med minne f�r att allokera RGB-bildramsbuffert."
#define MSGTR_VO_YUV4MPEG_OutFileOpenError "Kan inte f� minnes- eller filhanterare att skriva till \"stream.yuv\"!"
#define MSGTR_VO_YUV4MPEG_OutFileWriteError "Fel vid skrivning av bild till ut!" // FIXME output here?
#define MSGTR_VO_YUV4MPEG_UnknownSubDev "Ok�nd subdevice: %s" // FIXME subdevice
#define MSGTR_VO_YUV4MPEG_InterlacedTFFMode "Anv�nder 'interlaced output mode', �vre f�ltet f�rst." // FIXME top-field first? && 'interlaced output mode'
#define MSGTR_VO_YUV4MPEG_InterlacedBFFMode "Anv�nder 'interlaced output mode',nedre f�ltet f�rst." // FIXME bottom-field first? && 'interlaced output mode'

#define MSGTR_VO_YUV4MPEG_ProgressiveMode "Anv�nder (som standard) progressiv bildramsinst�llning."

// Old vo drivers that have been replaced

#define MSGTR_VO_PGM_HasBeenReplaced "pgm-video-ut-drivrutinen har blivit utbytt av '-vo pnm:pgmyuv'.\n"
#define MSGTR_VO_MD5_HasBeenReplaced "md5-video-ut-drivrutinen har blivit utbytt av '-vo md5sum'.\n"


// ======================= AO Audio Output drivers ========================

// libao2 

// audio_out.c

#define MSGTR_AO_ALSA9_1x_Removed "audio_out: alsa9- samt alsa1xmodulerna har blivit borttagna, anv�nd -ao ist�llet.\n"

// ao_oss.c

#define MSGTR_AO_OSS_CantOpenMixer "[AO OSS] audio_setup: Kan inte �ppna mixernehet %s: %s\n"
#define MSGTR_AO_OSS_ChanNotFound "[AO OSS] audio_setup: Audiokortsmixer har inte kanal '%s' anv�nder standard.\n"
#define MSGTR_AO_OSS_CantOpenDev "[AO OSS] audio_setup: Kan inte �ppna audioenhet %s: %s\n"
#define MSGTR_AO_OSS_CantMakeFd "[AO OSS] audio_setup: Kan inte f� till 'filedescriptor'sblockning: %s\n" // FIXME filedescriptor
#define MSGTR_AO_OSS_CantSetAC3 "[AO OSS] Kan inte s�tta audioenhet %s till AC3-ut, pr�var S16...\n"
#define MSGTR_AO_OSS_CantSetChans "[AO OSS] audio_setup: Misslyckades att s�tta audioenhet till %d kanaler.\n"
#define MSGTR_AO_OSS_CantUseGetospace "[AO OSS] audio_setup: dirvrutin hanerar ej SNDCTL_DSP_GETOSPACE :-(\n"
#define MSGTR_AO_OSS_CantUseSelect "[AO OSS]\n   ***  Din audiodrivrutin hanterar inte select()  ***\n Komplilera om med '#undef HAVE_AUDIO_SELECT' i config.h !\n\n" // TODO shoud be a better way to do this
#define MSGTR_AO_OSS_CantReopen "[AO OSS]\nFatalt fel: *** CAN INTE BLI �TER�PPNAD / �TERST�LLER AUDIOENHET *** %s\n"

// ao_arts.c

#define MSGTR_AO_ARTS_CantInit "[AO ARTS] %s\n" // FIXME nothing?
#define MSGTR_AO_ARTS_ServerConnect "[AO ARTS] Anslutet till ljudserver.\n"
#define MSGTR_AO_ARTS_CantOpenStream "[AO ARTS] Of�rm�gen att �ppna en str�m.\n" // FIXME 'str�m' or 'ljudstr�m'?
#define MSGTR_AO_ARTS_StreamOpen "[AO ARTS] Str�m �ppnad.\n" // -''-
#define MSGTR_AO_ARTS_BufferSize "[AO ARTS] buffertstorlek: %d\n"

// ao_dxr2.c

#define MSGTR_AO_DXR2_SetVolFailed "[AO DXR2] S�ttning av volym till %d misslyckades.\n"
#define MSGTR_AO_DXR2_UnsupSamplerate "[AO DXR2] dxr2: %d Hz �r ej tillg�nglig, f�rs�k med \"-aop list=resample\"\n"

// ao_esd.c

#define MSGTR_AO_ESD_CantOpenSound "[AO ESD] esd_open_sound misslyckades: %s\n"
#define MSGTR_AO_ESD_LatencyInfo "[AO ESD] latency: [server: %0.2fs, net: %0.2fs] (adjust %0.2fs)\n" // FIXME translate?
#define MSGTR_AO_ESD_CantOpenPBStream "[AO ESD] misslyckades att �ppna uppspelningsstr�m: %s\n" 

// ao_mpegpes.c

#define MSGTR_AO_MPEGPES_CantSetMixer "[AO MPEGPES] DVB-audio-s�ttningsmixer misslyckades: %s\n" // set ~= s�ttning?
#define MSGTR_AO_MPEGPES_UnsupSamplerate "[AO MPEGPES] %d Hz ej tillg�nglig, f�rs�ker resampla...\n"

// ao_null.c
// This one desn't even have any mp_msg nor printf's?? [CHECK]

// ao_pcm.c

#define MSGTR_AO_PCM_FileInfo "[AO PCM] Fil: %s (%s)\nPCM: Samplerate: %iHz Kanaler: %s Format %s\n" // FIXME Samplerate?
#define MSGTR_AO_PCM_HintInfo "[AO PCM] Info: snabbaste dumplning �r tillg�nglig via -vc dummy -vo null\nPCM: Info: f�r att skriva WAVE-filer anv�nd -waveheader (standard).\n"
#define MSGTR_AO_PCM_CantOpenOutputFile "[AO PCM] Misslyckades att �ppna %s f�r skrivning!\n"

// ao_sdl.c

#define MSGTR_AO_SDL_INFO "[AO SDL] Samplerate: %iHz Kanaler: %s Format %s\n" // -''-
#define MSGTR_AO_SDL_DriverInfo "[AO SDL] anv�nder %s som audioenhet.\n"
#define MSGTR_AO_SDL_UnsupportedAudioFmt "[AO SDL] Icke tillg�ngligt audioformat: 0x%x.\n" // support?
#define MSGTR_AO_SDL_CantInit "[AO SDL] Initialisering av 'SDL Audio' misslyckades: %s\n"
#define MSGTR_AO_SDL_CantOpenAudio "[AO SDL] Of�rm�gen att �ppna audio: %s\n" // audio what?

// ao_sgi.c

#define MSGTR_AO_SGI_INFO "[AO SGI] kontroll.\n"
#define MSGTR_AO_SGI_InitInfo "[AO SGI] init: Samplerate: %iHz Kanaler: %s Format %s\n" // FIXME Samplerate
#define MSGTR_AO_SGI_InvalidDevice "[AO SGI] play: icke godk�nd enhet.\n"
#define MSGTR_AO_SGI_CantSetParms_Samplerate "[AO SGI] init: setparams misslyckades: %s\nKunde inte s�tta �nskad samplerate.\n" // -''-
#define MSGTR_AO_SGI_CantSetAlRate "[AO SGI] init: AL_RATE var inte accepterad p� given resurs.\n"
#define MSGTR_AO_SGI_CantGetParms "[AO SGI] init: getparams misslyckades: %s\n"
#define MSGTR_AO_SGI_SampleRateInfo "[AO SGI] init: samplerate �r nu %lf (�nskad rate var %lf)\n" // -''- also rate?
#define MSGTR_AO_SGI_InitConfigError "[AO SGI] init: %s\n"
#define MSGTR_AO_SGI_InitOpenAudioFailed "[AO SGI] init: Of�rm�gen att �ppna audiokanal: %s\n"
#define MSGTR_AO_SGI_Uninit "[AO SGI] uninit: ...\n" // FIXME translate?
#define MSGTR_AO_SGI_Reset "[AO SGI] reset: ...\n" // -''-
#define MSGTR_AO_SGI_PauseInfo "[AO SGI] audio_pause: ...\n" // -''-
#define MSGTR_AO_SGI_ResumeInfo "[AO SGI] audio_resume: ...\n" // -''-

// ao_sun.c

#define MSGTR_AO_SUN_RtscSetinfoFailed "[AO SUN] rtsc: SETINFO misslyckades.\n"
#define MSGTR_AO_SUN_RtscWriteFailed "[AO SUN] rtsc: skrivning misslyckades."
#define MSGTR_AO_SUN_CantOpenAudioDev "[AO SUN] Kan inte �ppna audioenhet %s, %s  -> inget ljud.\n"
#define MSGTR_AO_SUN_UnsupSampleRate "[AO SUN] audio_setup: ditt kort hanterar inte %d kanaler, %s, %d Hz samplerate.\n" // FIXME samplerate
#define MSGTR_AO_SUN_CantUseSelect "[AO SUN]\n   ***  Din ljudkortsenhet hanterar inte select()  ***\nKompilera om med '#undef HAVE_AUDIO_SELECT' i config.h !\n\n" // same as for MSGTR_AO_OSS_CantUseSelect
#define MSGTR_AO_SUN_CantReopenReset "[AO SUN]\nFatalt fel: *** KAN INTE �TER�PPNA / �TERST�LLA AUDIOENHET (%s) ***\n"

// ao_alsa5.c

#define MSGTR_AO_ALSA5_InitInfo "[AO ALSA5] alsa-init: �nskat format: %d Hz, %d kanaler, %s\n"
#define MSGTR_AO_ALSA5_SoundCardNotFound "[AO ALSA5] alsa-init: inga ljudkort funna.\n"
#define MSGTR_AO_ALSA5_InvalidFormatReq "[AO ALSA5] alsa-init: icke godk�nt format (%s) �nskat - ut deaktiverat.\n" // FIXME output -> ut here?
#define MSGTR_AO_ALSA5_PlayBackError "[AO ALSA5] alsa-init: uppspelnings�ppningsfel: %s\n" 
#define MSGTR_AO_ALSA5_PcmInfoError "[AO ALSA5] alsa-init: pcm-infofel: %s\n"
#define MSGTR_AO_ALSA5_SoundcardsFound "[AO ALSA5] alsa-init: %d ljurtkort funna, anv�nder: %s\n"
#define MSGTR_AO_ALSA5_PcmChanInfoError "[AO ALSA5] alsa-init: pcm-kanalinfofel: %s\n"
#define MSGTR_AO_ALSA5_CantSetParms "[AO ALSA5] alsa-init: fel vid s�ttning av parametrarna: %s\n" // FIXME setting?
#define MSGTR_AO_ALSA5_CantSetChan "[AO ALSA5] alsa-init: fel vid initiering av kanal: %s\n"
#define MSGTR_AO_ALSA5_ChanPrepareError "[AO ALSA5] alsa-init: kanalprepareringsfel: %s\n"
#define MSGTR_AO_ALSA5_DrainError "[AO ALSA5] alsa-uninit: uppspelningsl�nsningsfel: %s\n" // FIXME drain -> l�nsning?
#define MSGTR_AO_ALSA5_FlushError "[AO ALSA5] alsa-uninit: uppspelningsspolningsfel: %s\n" // FIXME flush -> spolning?
#define MSGTR_AO_ALSA5_PcmCloseError "[AO ALSA5] alsa-uninit: pcm-st�ngningsfel: %s\n"
#define MSGTR_AO_ALSA5_ResetDrainError "[AO ALSA5] alsa-reset: uppspelningsl�nsningsfel: %s\n"
#define MSGTR_AO_ALSA5_ResetFlushError "[AO ALSA5] alsa-reset: uppspelningsspolningsfel: %s\n"
#define MSGTR_AO_ALSA5_ResetChanPrepareError "[AO ALSA5] alsa-reset: kanalprepareringsfel: %s\n"
#define MSGTR_AO_ALSA5_PauseDrainError "[AO ALSA5] alsa-pause: uppspelningsl�nsningsfel: %s\n"
#define MSGTR_AO_ALSA5_PauseFlushError "[AO ALSA5] alsa-pause: uppspelningsspolningsfel: %s\n"
#define MSGTR_AO_ALSA5_ResumePrepareError "[AO ALSA5] alsa-resume: kanalprepareringsfel: %s\n"
#define MSGTR_AO_ALSA5_Underrun "[AO ALSA5] alsa-play: alsa underrun, �terst�ller str�m.\n" // FIXME underun - translate?
#define MSGTR_AO_ALSA5_PlaybackPrepareError "[AO ALSA5] alsa-play: uppspelningsprepareringsfel: %s\n"
#define MSGTR_AO_ALSA5_WriteErrorAfterReset "[AO ALSA5] alsa-play: skrivfel efter �terst�llning: %s - ger upp.\n"
#define MSGTR_AO_ALSA5_OutPutError "[AO ALSA5] alsa-play: utfel: %s\n" // FIXME output -> ut her?

// ao_plugin.c

#define MSGTR_AO_PLUGIN_InvalidPlugin "[AO PLUGIN] icke godk�nd plugin: %s\n" // FIXME plugin - translate?


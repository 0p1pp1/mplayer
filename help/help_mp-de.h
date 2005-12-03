// Translated by: Johannes Feigl <johannes.feigl@aon.at>
// Reworked by Klaus Umbach <klaus.umbach@gmx.net>
// Moritz Bunkus <moritz@bunkus.org>
// Alexander Strasser <eclipse7@gmx.net>
// Sebastian Kr�mer <mplayer@skraemer.de>

// In synch with rev 1.205

// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"Verwendung:   mplayer [Optionen] [URL|Verzeichnis/]Dateiname\n"
"\n"
"Basisoptionen: (siehe Manpage f�r eine vollst�ndige Liste aller Optionen!)\n"
" -vo <drv[:dev]>  Videoausgabetreiber & -ger�t ('-vo help' f�r eine Liste)\n"
" -ao <drv[:dev]>  Audioausgabetreiber & -ger�t ('-ao help' f�r eine Liste)\n"
#ifdef HAVE_VCD
" vcd://<tracknr>   Spiele einen (S)VCD-Titel (Super Video CD) ab\n"
"                   ( direkter Ger�tezugriff, kein mount! )\n"
#endif
#ifdef USE_DVDREAD
" dvd://<titelnr>   Spiele DVD-Titel direkt vom Ger�t anstelle einer Datei\n"
" -alang/-slang    W�hle DVD Audio/Untertitel Sprache (2-Zeichen-L�ndercode)\n"
#endif
" -ss <zeitpos>    Spiele ab Position (Sekunden oder hh:mm:ss)\n"
" -nosound         Ohne Ton abspielen\n"
" -fs              Im Vollbildmodus abspielen (oder -vm, -zoom, siehe Manpage)\n"
" -x <x> -y <y>    Setze Bildschirmaufl�sung (f�r Benutzung mit -vm oder -zoom)\n"
" -sub <datei>     Benutze Untertitel-Datei (siehe auch -subfps, -subdelay)\n"
" -playlist <datei> Benutze Playlist aus Datei\n"
" -vid x -aid y    W�hle Video- (x) und Audiostream (y) zum Abspielen\n"
" -fps x -srate y  �ndere Videoframerate (x fps) und Audiosamplingrate (y Hz)\n"
" -pp <Qualit�t>   Aktiviere Postprocessing-Filter (siehe Manpage f�r Details)\n"
" -framedrop       Verwerfe einzelne Frames (bei langsamen Rechnern)\n"
"\n"
"Grundlegende Tasten: (vollst�ndige Liste in der Manpage, siehe auch input.conf)\n"
" <- oder ->       Springe 10 Sekunden vor/zur�ck\n"
" hoch/runter      Springe  1 Minute vor/zur�ck\n"
" Bild hoch/runter Springe 10 Minuten vor/zur�ck\n"
" < oder >         Gehe in der Playlist vor/zur�ck\n"
" p oder LEER      Pause (dr�cke eine beliebige Taste zum Fortsetzen)\n"
" q oder ESC       Abspielen stoppen und Programm beenden\n"
" + oder -         Audioverz�gerung um +/- 0.1 Sekunde anpassen\n"
" o                OSD-Modus:  Aus / Suchleiste / Suchleiste + Zeitangabe\n"
" * oder /         PCM-Lautst�rke erh�hen oder erniedrigen\n"
" z oder x         Untertitelverz�gerung um +/- 0.1 Sekunde anpassen\n"
" r oder t         Verschiebe die Untertitel-Position, siehe auch '-vf expand'\n"
"\n"
" * * * SIEHE MANPAGE F�R DETAILS, WEITERE OPTIONEN UND TASTEN * * *\n"
"\n";
#endif

#define MSGTR_SamplesWanted "Beispiele f�r dieses Format werden gebraucht, um die Unterst�tzung zu verbessern. Bitte kontaktiere die Entwickler.\n"

// ========================= MPlayer Ausgaben ===========================

// mplayer.c: 
#define MSGTR_Exiting "\nBeenden...\n"
#define MSGTR_ExitingHow "\nBeenden... (%s)\n"
#define MSGTR_Exit_quit "Ende"
#define MSGTR_Exit_eof "Dateiende erreicht"
#define MSGTR_Exit_error "Fataler Fehler"
#define MSGTR_IntBySignal "\nMPlayer wurde durch Signal %d im Modul %s unterbrochen.\n"
#define MSGTR_NoHomeDir "Kann Homeverzeichnis nicht finden.\n"
#define MSGTR_GetpathProblem "get_path(\"config\") fehlgeschlagen.\n"
#define MSGTR_CreatingCfgFile "Erstelle Konfigurationsdatei: %s\n"
#define MSGTR_InvalidAOdriver "Ung�ltiger Audioausgabetreibername: %s\nBenutze '-vo help' f�r eine Liste der verf�gbaren Audiotreiber.\n"
#define MSGTR_CopyCodecsConf "(Kopiere/verlinke etc/codecs.conf aus dem MPlayer-Quelltext nach ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "Benutze eingebaute Standardwerte f�r codecs.conf.\n"
#define MSGTR_CantLoadFont "Kann Schriftdatei nicht laden: %s\n"
#define MSGTR_CantLoadSub "Kann Untertitel nicht laden: %s\n"
#define MSGTR_DumpSelectedStreamMissing "dump: FATAL: Ausgew�hlter Stream fehlt!\n"
#define MSGTR_CantOpenDumpfile "Kann dump-Datei nicht �ffnen!\n"
#define MSGTR_CoreDumped "Core dumped ;)\n"
#define MSGTR_FPSnotspecified "FPS ist im Header nicht angegeben (oder ung�ltig)! Benutze die -fps Option!\n"
#define MSGTR_TryForceAudioFmtStr "Versuche Audiocodecfamilie %s zu erzwingen...\n"
#define MSGTR_CantFindAudioCodec "Kann Codec f�r Audioformat 0x%X nicht finden!\n"
#define MSGTR_RTFMCodecs "Lies DOCS/HTML/en/codecs.html!\n"
#define MSGTR_TryForceVideoFmtStr "Versuche Videocodecfamilie %s zu erzwingen...\n"
#define MSGTR_CantFindVideoCodec "Kann keinen Codec finden, der  zur gew�hlten -vo-Option und Videoformat 0x%X passt!\n"
#define MSGTR_CannotInitVO "FATAL: Kann Videoausgabetreiber nicht initialisieren!\n"
#define MSGTR_CannotInitAO "Kann Audiotreiber/Soundkarte nicht �ffnen/initialisieren -> kein Ton\n"
#define MSGTR_StartPlaying "Starte Wiedergabe...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"         ***************************************************\n"\
"         **** Dein System ist zu LANGSAM zum Abspielen! ****\n"\
"         ***************************************************\n"\
"M�gliche Gr�nde, Probleme, Workarounds: \n"\
"- H�ufigste Ursache: defekter/fehlerhafter _Audio_treiber.\n"\
"  - Versuche -ao sdl oder die OSS-Emulation von ALSA.\n"\
"  - Experimentiere mit verschiedenen Werten f�r -autosync, 30 ist ein guter Startwert."\
"- Langsame Videoausgabe\n"\
"  - Versuche einen anderen -vo Treiber (-vo help f�r eine Liste)\n"\
"    oder probiere -framedrop!\n"\
"- Langsame CPU\n"\
"  - Versuche nicht, DVDs/gro�e DivX-Filme auf langsamen CPUs abzuspielen.\n"\
"    Probiere Optionen von lavdopts, z.B.\n"\
"    -vfm ffmpeg -lavdopts lowres=1:fast:skiploopfilter=all.\n"\
"- Defekte Datei\n"\
"  - Versuche verschiedene Kombinationen von: -nobps -ni -forceidx -mc 0.\n"\
"- F�r die Wiedergabe von langsamen Medien (NFS/SMB, DVD, VCD usw)\n"\
"  - Versuche -cache 8192.\n"\
"- Benutzt du -cache zusammen mit einer nicht-interleavten AVI-Datei?\n"\
"  - Versuche -nocache.\n"\
"Lies DOCS/HTML/en/video.html; dort stehen Tipps f�r optimale Einstellungen.\n"\
"(Schau evtl. auch bei den entsprechenden englischen Seiten.)\n"\
"Wenn dies nicht hilft, lies DOCS/HTML/en/bugreports.html!\n\n"

#define MSGTR_NoGui "MPlayer wurde OHNE GUI-Unterst�tzung kompiliert.\n"
#define MSGTR_GuiNeedsX "MPlayer GUI erfordert X11.\n"
#define MSGTR_Playing "Spiele %s.\n"
#define MSGTR_NoSound "Audio: kein Ton!\n"
#define MSGTR_FPSforced "FPS von %5.3f erzwungen (ftime: %5.3f).\n"
#define MSGTR_CompiledWithRuntimeDetection "MPlayer mit CPU-Erkennung zur Laufzeit kompiliert - WARNUNG, das ist nicht optimal!\nKompiliere MPlayer mit --disable-runtime-cpudetection f�r beste Performance.\n"
#define MSGTR_CompiledWithCPUExtensions "Kompiliert f�r x86 CPU mit folgenden Erweiterungen:"
#define MSGTR_AvailableVideoOutputDrivers "Verf�gbare Videoausgabetreiber:\n"
#define MSGTR_AvailableAudioOutputDrivers "Verf�gbare Audioausgabetreiber:\n"
#define MSGTR_AvailableAudioCodecs "Verf�gbare Audiocodecs:\n"
#define MSGTR_AvailableVideoCodecs "Verf�gbare Videocodecs:\n"
#define MSGTR_AvailableAudioFm "Verf�gbare (in das Binary kompilierte) Audio Codec Familien:\n"
#define MSGTR_AvailableVideoFm "Verf�gbare (in das Binary kompilierte) Video Codec Familien:\n"
#define MSGTR_AvailableFsType "Verf�gbare Vollbildschirm-Modi:\n"
#define MSGTR_UsingRTCTiming "Verwende Linux Hardware RTC-Timing (%ldHz).\n"
#define MSGTR_CannotReadVideoProperties "Video: Kann Eigenschaften nicht lesen.\n"
#define MSGTR_NoStreamFound "Keine Streams gefunden.\n"
#define MSGTR_ErrorInitializingVODevice "Fehler beim �ffnen/Initialisieren des ausgew�hlten Videoausgabetreibers (-vo).\n"
#define MSGTR_ForcedVideoCodec "Erzwungener Videocodec: %s\n"
#define MSGTR_ForcedAudioCodec "Erzwungener Audiocodec: %s\n"
#define MSGTR_Video_NoVideo "Video: kein Video\n"
#define MSGTR_NotInitializeVOPorVO "\nFATAL: Konnte Videofilter (-vf) oder -ausgabetreiber (-vo) nicht initialisieren.\n"
#define MSGTR_Paused "\n  =====  PAUSE  =====\r"
#define MSGTR_PlaylistLoadUnable "\nKann Playlist %s nicht laden.\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- MPlayer st�rzte aufgrund einer 'ung�ltigen Anweisung' ab.\n"\
"  Es kann sich um einen Fehler im unserem neuen Code f�r\n"\
"  die CPU-Erkennung zur Laufzeit handeln...\n"\
"  Bitte lies DOCS/HTML/en/bugreports.html.\n"
#define MSGTR_Exit_SIGILL \
"- MPlayer st�rzte aufgrund einer 'ung�ltigen Anweisung' ab.\n"\
"  Das passiert normalerweise, wenn du MPlayer auf einer anderen CPU\n"\
"  ausf�hrst als auf der, f�r die er kompiliert/optimiert wurde.\n"\
"  �berpr�fe das!\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- MPlayer st�rzte wegen falscher Benutzung der CPU/FPU/des RAMs ab.\n"\
"  Kompiliere MPlayer erneut mit --enable-debug und erstelle mit 'gdb'\n"\
"  einen Backtrace und eine Disassemblierung. Details dazu findest du\n"\
"  in DOCS/HTML/en/bugreports_what.html#bugreports_crash.\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer ist abgest�rzt. Das sollte nicht passieren.\n"\
"  Es kann sich um einen Fehler im MPlayer-Code _oder_ in deinen Treibern\n"\
"  _oder_ in deinem gcc handeln. Wenn du meinst, es sei MPlayers Fehler, dann\n"\
"  lies DOCS/HTML/en/bugreports.html und folge den dortigen Anweisungen.\n"\
"  Wir k�nnen und werden dir nicht helfen, wenn du nicht alle dort aufgef�hrten\n"\
"  Informationen zur Verf�gung stellst.\n"
#define MSGTR_LoadingConfig "Lade Konfiguration '%s'\n"
#define MSGTR_AddedSubtitleFile "SUB: Untertiteldatei (%d) hinzugef�gt: %s\n"
#define MSGTR_RemovedSubtitleFile "SUB: Untertiteldatei (%d) entfernt: %s\n"
#define MSGTR_ErrorOpeningOutputFile "Fehler beim �ffnen von Datei [%s] zum Schreiben!\n"
#define MSGTR_CommandLine "Kommandozeile:"
#define MSGTR_RTCDeviceNotOpenable "Konnte %s nicht �ffnen: %s (sollte f�r den Benutzer lesbar sein).\n"
#define MSGTR_LinuxRTCInitErrorIrqpSet "Linux RTC-Initialisierungsfehler in ioctl (rtc_irqp_set %lu): %s\n"
#define MSGTR_IncreaseRTCMaxUserFreq "Versuche, \"echo %lu > /proc/sys/dev/rtc/max-user-freq\" zu deinen Systemstartskripten hinzuzuf�gen.\n"
#define MSGTR_LinuxRTCInitErrorPieOn "Linux RTC-Initialisierungsfehler in ioctl (rtc_pie_on): %s\n"
#define MSGTR_UsingTimingType "Benutze %s-Zeitgeber.\n"
#define MSGTR_NoIdleAndGui "Die Option -idle kann mit GMPlayer nicht verwendet werden.\n"
#define MSGTR_MenuInitialized "Men� initialisiert: %s\n"
#define MSGTR_MenuInitFailed "Initialisierung des Men�s fehlgeschlagen.\n"
#define MSGTR_Getch2InitializedTwice "WARNUNG: getch2_init doppelt aufgerufen!\n"
#define MSGTR_DumpstreamFdUnavailable "Kann Dump dieses Streams nicht anlegen - kein 'fd' verf�gbar.\n"
#define MSGTR_FallingBackOnPlaylist "Falle zur�ck auf den Versuch, Playlist %s einzulesen...\n"
#define MSGTR_CantOpenLibmenuFilterWithThisRootMenu "Kann den libmenu-Videofilter nicht mit dem Ursprungsmen� %s �ffnen.\n"
#define MSGTR_AudioFilterChainPreinitError "Fehler bei der Vorinitialisierung der Audiofilterkette!\n"
#define MSGTR_LinuxRTCReadError "Linux RTC-Lesefehler: %s\n"
#define MSGTR_SoftsleepUnderflow "Warnung! Unterlauf des Softsleep!\n"
#define MSGTR_DvdnavNullEvent "DVDNAV-Ereignis NULL?!\n"
#define MSGTR_DvdnavHighlightEventBroken "DVDNAV-Ereignis: Hervorhebungs-Ereignis kaputt\n"
#define MSGTR_DvdnavEvent "DVDNAV-Ereignis: %s\n"
#define MSGTR_DvdnavHighlightHide "DVDNAV-Ereignis: Hervorhebung verbergen\n"
#define MSGTR_DvdnavStillFrame "######################################## DVDNAV-Ereignis: Standbild: %d Sekunde(n)\n"
#define MSGTR_DvdnavNavStop "DVDNAV-Ereignis: Nav Stop\n"
#define MSGTR_DvdnavNavNOP "DVDNAV-Ereignis: Nav NOP\n"
#define MSGTR_DvdnavNavSpuStreamChangeVerbose "DVDNAV-Ereignis: Nav SPU Stream Change: phys: %d/%d/%d logisch: %d\n"
#define MSGTR_DvdnavNavSpuStreamChange "DVDNAV-Ereignis: Nav SPU Stream-�nderung: phys: %d logisch: %d\n"
#define MSGTR_DvdnavNavAudioStreamChange "DVDNAV-Ereignis: Nav Audio Stream-�nderung: phys: %d logisch: %d\n"
#define MSGTR_DvdnavNavVTSChange "DVDNAV-Ereignis: Nav VTS-�nderung\n"
#define MSGTR_DvdnavNavCellChange "DVDNAV-Ereignis: Nav Cell-�nderung\n"
#define MSGTR_DvdnavNavSpuClutChange "DVDNAV-Ereignis: Nav SPU CLUT-�nderung\n"
#define MSGTR_DvdnavNavSeekDone "DVDNAV-Ereignis: Nav Suche beendet.\n"
#define MSGTR_MenuCall "Men�-Aufruf\n"

#define MSGTR_EdlCantUseBothModes "Kann -edl und -edlout nicht zur selben Zeit benutzen.\n"
#define MSGTR_EdlOutOfMem "Kann nicht genug Speicher f�r EDL-Daten reservieren.\n"
#define MSGTR_EdlRecordsNo "%d EDL-Aktionen gelesen.\n"
#define MSGTR_EdlQueueEmpty "Es gibt keine auszuf�hrenden EDL-Aktionen.\n"
#define MSGTR_EdlCantOpenForWrite "Kann EDL-Datei [%s] nicht zum Schreiben �ffnen.\n"
#define MSGTR_EdlCantOpenForRead "Kann EDL-Datei [%s] nicht zum Lesen �ffnen.\n"
#define MSGTR_EdlNOsh_video "Kann EDL nicht ohne Video verwenden, deaktiviere.\n"
#define MSGTR_EdlNOValidLine "Ung�ltige EDL-Zeile: %s\n"
#define MSGTR_EdlBadlyFormattedLine "Schlecht formatierte EDL-Zeile [%d]. Verwerfe.\n"
#define MSGTR_EdlBadLineOverlap "Letzte Stop-Position war [%f]; n�chster Start ist "\
"[%f]. Eintr�ge m�ssen in chronologischer Reihenfolge sein, ohne �berschneidung. Verwerfe.\n"
#define MSGTR_EdlBadLineBadStop "Zeit des Stops mu� nach der Startzeit sein.\n"

// mplayer.c OSD

#define MSGTR_OSDenabled "aktiviert"
#define MSGTR_OSDdisabled "deaktiviert"
#define MSGTR_OSDnone "nicht verf�gbar"
#define MSGTR_OSDunknown "unbekannt"
#define MSGTR_OSDDVDNAV "DVDNAV: %s"
#define MSGTR_OSDChannel "Kanal: %s"
#define MSGTR_OSDSubtitles "Untertitel: %s"
#define MSGTR_OSDSubtitlesOff "Untertitel: deaktiviert"
#define MSGTR_OSDSubtitlesLanguage "Untertitelsprache: (%d) %s"
#define MSGTR_OSDSub "Untertitel: (%d) %s%s"
#define MSGTR_OSDSubDelay "Untertitelverz�gerung: %dms"
#define MSGTR_OSDSubPosition "Untertitelposition: %d/100"
#define MSGTR_OSDSubAlignment "Untertitelausrichtung: %s"
#define MSGTR_OSDAVDelay "A/V-Verz�gerung: %dms"
#define MSGTR_OSDSpeed "Geschwindigkeit: %6.2f-fach"
#define MSGTR_OSDStayOnTop "Immer im Vordergrund: %s"
#define MSGTR_OSDRootwin "Anzeige auf dem Desktop: %s"
#define MSGTR_OSDBorder "Rand: %s"
#define MSGTR_OSDFramedrop "Framedropping: %s"
#define MSGTR_OSDFramedropOn "an"
#define MSGTR_OSDFramedropHard "hart"
#define MSGTR_OSDFramedropOff "aus"
#define MSGTR_OSDosd "OSD: %s"
#define MSGTR_OSDSubBottom "unten"
#define MSGTR_OSDSubCenter "mitte"
#define MSGTR_OSDSubTop "oben"

// mencoder.c:

#define MSGTR_UsingPass3ControllFile "Verwende Pass 3 Kontrolldatei: %s\n"
#define MSGTR_MissingFilename "\nDateiname nicht angegeben.\n\n"
#define MSGTR_CannotOpenFile_Device "Kann Datei/Ger�t nicht �ffnen.\n"
#define MSGTR_CannotOpenDemuxer "Kann Demuxer nicht �ffnen.\n"
#define MSGTR_NoAudioEncoderSelected "\nKein Audioencoder (-oac)  ausgew�hlt. \nW�hle einen aus (siehe -oac help) oder verwende  -nosound.\n"
#define MSGTR_NoVideoEncoderSelected "\nKein Videoencoder (-ovc) ausgew�hlt. \nW�hle einen aus (siehe -ovc help).\n"
#define MSGTR_CannotOpenOutputFile "Kann Ausgabedatei '%s' nicht �ffnen.\n"
#define MSGTR_EncoderOpenFailed "�ffnen des Encoders fehlgeschlagen.\n"
#define MSGTR_ForcingOutputFourcc "Erzwinge Output-Fourcc %x [%.4s].\n"
#define MSGTR_ForcingOutputAudiofmtTag "Erzwinge Audioformatkennzeichnung 0x%x in der Ausgabe.\n"
#define MSGTR_DuplicateFrames "\n%d doppelte(r) Frame(s)!\n"
#define MSGTR_SkipFrame "\nFrame �bersprungen!\n"
#define MSGTR_ResolutionDoesntMatch "\nNeue Videodatei hat eine andere Aufl�sung oder anderen Farbraum als die vorige.\n"
#define MSGTR_FrameCopyFileMismatch "\nAlle Videodateien m�ssen f�r -ovc copy identische fps, Aufl�sung und Codec haben.\n"
#define MSGTR_AudioCopyFileMismatch "\nAlle Videodateien m�ssen f�r -oac copy identischen Audiocodec und Format haben.\n"
#define MSGTR_NoAudioFileMismatch "\nNur-Video-Dateien k�nnen nicht mit Audio/Video-Dateien gemischt werden.  Versuche -nosound.\n"
#define MSGTR_NoSpeedWithFrameCopy "WARNUNG: Korrektes Funktionieren von -speed kann zusammen mit -oac copy nicht garantiert werden!\n"\
"Das Ergebnis der Encodierung k�nnte defekt sein!\n"
#define MSGTR_ErrorWritingFile "%s: Fehler beim Schreiben der Datei.\n"
#define MSGTR_RecommendedVideoBitrate "Empfohlene Videobitrate f�r %s CD(s): %d\n"
#define MSGTR_VideoStreamResult "\nVideostream: %8.3f kbit/s  (%d B/s)  Gr��e: %d Bytes  %5.3f Sek.  %d Frames\n"
#define MSGTR_AudioStreamResult "\nAudiostream: %8.3f kbit/s  (%d B/s)  Gr��e: %d Bytes  %5.3f Sek.\n"
#define MSGTR_OpenedStream "Erfolg: Format: %d  Daten: 0x%X - 0x%x\n"
#define MSGTR_VCodecFramecopy "Videocodec: Framecopy (%dx%d %dbpp fourcc=%x)\n"
#define MSGTR_ACodecFramecopy "Audiocodec: Framecopy (Format=%x chans=%d Rate=%ld Bits=%d B/s=%ld Sample-%ld)\n"
#define MSGTR_CBRPCMAudioSelected "CBR PCM Audio ausgew�hlt.\n"
#define MSGTR_MP3AudioSelected "MP3 Audio ausgew�hlt.\n"
#define MSGTR_CannotAllocateBytes "Konnte %d Bytes nicht reservieren.\n"
#define MSGTR_SettingAudioDelay "Setze AUDIOVERZ�GERUNG auf %5.3f.\n"
#define MSGTR_SettingAudioInputGain "Setze Audioeingangsverst�rkung auf %f.\n"
#define MSGTR_LamePresetEquals "\nPreset=%s\n\n"
#define MSGTR_LimitingAudioPreload "Limitiere Audio-Preload auf 0.4s.\n"
#define MSGTR_IncreasingAudioDensity "Erh�he Audiodichte auf 4.\n"
#define MSGTR_ZeroingAudioPreloadAndMaxPtsCorrection "Erzwinge Audio-Preload von 0, maximale pts-Korrektur von 0.\n"
#define MSGTR_CBRAudioByterate "\n\nCBR Audio: %ld Bytes/Sek, %d Bytes/Block.\n"
#define MSGTR_LameVersion "LAME-Version %s (%s)\n\n"
#define MSGTR_InvalidBitrateForLamePreset "Fehler: Die angegebene Bitrate ist au�erhalb des g�ltigen Bereichs\nf�r dieses Preset.\n"\
"\n"\
"Bei Benutzung dieses Modus mu�t du einen Wert zwischen \"8\" und \"320\" angeben.\n"\
"\n"\
"F�r weitere Informationen hierzu versuche: \"-lameopts preset=help\"\n"
#define MSGTR_InvalidLamePresetOptions "Fehler: Du hast kein g�ltiges Profil und/oder ung�ltige Optionen mit\n        dem Preset angegeben.\n"\
"\n"\
"Verf�gbare Profile sind folgende:\n"\
"\n"\
"   <fast>        standard\n"\
"   <fast>        extreme\n"\
"                 insane\n"\
"   <cbr> (ABR-Modus) - Der ABR-Modus ist impliziert. Um ihn zu benutzen,\n"\
"                 gib einfach die Bitrate an. Zum Beispiel:\n"\
"                 \"preset=185\" aktiviert dieses Preset\n"\
"                 und benutzt 185 als durchschnittliche kbps.\n"\
"\n"\
"    Ein paar Beispiele:\n"\
"\n"\
"      \"-lameopts fast:preset=standard  \"\n"\
" oder \"-lameopts  cbr:preset=192       \"\n"\
" oder \"-lameopts      preset=172       \"\n"\
" oder \"-lameopts      preset=extreme   \"\n"\
"\n"\
"F�r weitere Informationen hierzu versuche: \"-lameopts preset=help\"\n"
#define MSGTR_LamePresetsLongInfo "\n"\
"Die Preset-Schalter sind angelegt, die h�chstm�gliche Qualit�t zur Verf�gung\nzu stellen.\n"\
"\n"\
"Sie waren Thema von gro�angelegten Doppelblind-H�rtests und wurden\n"\
"dementsprechend verfeinert, um diese Objektivit�t zu erreichen.\n"\
"\n"\
"Diese werden kontinuierlich aktualisiert, um den neuesten Entwicklungen zu\n"\
"entsprechen, die stattfinden. Daher sollte dir das Resultat die fast beste\n"\
"Qualit�t liefern, die zur Zeit mit LAME m�glich ist.\n"\
"\n"\
"Um diese Presets zu aktivieren:\n"\
"\n"\
"   F�r VBR-Modi (generell h�chste Qualit�t):\n"\
"\n"\
"     \"preset=standard\" Dieses Preset sollte generell anwendbar sein f�r\n"\
"                            die meisten Leute und die meiste Musik und hat\n"\
"                            schon eine recht hohe Qualit�t.\n"\
"\n"\
"     \"preset=extreme\" Wenn du einen extrem guten H�rsinn und �hnlich gute\n"\
"                            Ausstattung hast, wird dir dieses Preset generell\n"\
"                            eine leicht h�here Qualit�t bieten als der\n"\
"                            \"standard\"-Modus.\n"\
"\n"\
"   F�r CBR 320kbps (h�chstm�gliche Qualit�t mit diesen Preset-Schaltern):\n"\
"\n"\
"     \"preset=insane\"  Dieses Preset ist vermutlich Overkill f�r die meisten\n"\
"                            Leute und die meisten Situationen, aber wenn Du\n"\
"                            die absolut h�chste Qualit�t brauchst und die\n"\
"                            Dateigr��e egal ist, ist dies der Weg.\n"\
"\n"\
"   F�r ABR-Modi (hohe Qualit�t f�r gegebenene Bitrate, geringer als bei VBR):\n"\
"\n"\
"     \"preset=<kbps>\"  Benutzung dieses Presets wird dir normalerweise gute\n"\
"                            Qualit�t zu einer angegebenen Bitrate liefern.\n"\
"                            Je nach Bitrate wird dieses Preset optimale\n"\
"                            Einstellungen f�r diese bestimmte Situation\n"\
"                            ermitteln. Obwohl dieser Ansatz funktioniert,\n"\
"                            ist er nicht ansatzweise so flexibel wie VBR\n"\
"                            und wird f�r gew�hnlich nicht das gleiche\n"\
"                            Qualit�tslevel wie VBR bei hohen Bitraten\n"\
"                            erreichen.\n"\
"\n"\
"Die folgenden Optionen sind auch bei den zugeh�rigen Profilen verf�gbar:\n"\
"\n"\
"   <fast>        standard\n"\
"   <fast>        extreme\n"\
"                 insane\n"\
"   <cbr> (ABR-Modus) - Der ABR-Modus ist impliziert. Um ihn zu benutzen,\n"\
"                 gib einfach die Bitrate an. Zum Beispiel:\n"\
"                 \"preset=185\" aktiviert dieses Preset\n"\
"                 und benutzt 185 als durchschnittliche kbps.\n"\
"\n"\
"   \"fast\" - Aktiviert die neue schnelle VBR f�r ein bestimmtes Profil. Der\n"\
"            Nachteil des fast-Schalters ist, da� die Bitrate oft leicht h�her\n"\
"            als im normalen Modus ist, au�erdem kann die Qualit�t auch leicht\n"\
"            geringer ausfallen.\n"\
"   Warnung: In der aktuellen Version k�nnen fast-Presets im Vergleich zu\n"\
"            regul�ren Presets in zu hoher Bitrate resultieren.\n"\
"\n"\
"   \"cbr\"  - Bei Benutzung des ABR-Modus (siehe oben) und signifikanter\n"\
"            Bitrate wie 80, 96, 112, 128, 160, 192, 224, 256, 320\n"\
"            kannst du die \"cbr\"-Option benutzen, um Encoding im CBR-Modus"\
"            anstelle des Standard-ABR-Modus zu erzwingen. ABR bietet h�here\n"\
"            Qualit�t, doch CBR kann n�tzlich sein in Situationen, bei denen\n"\
"            MP3-Streaming �ber das Internet wichtig sind.\n"\
"\n"\
"    Zum Beispiel:\n"\
"\n"\
"      \"-lameopts fast:preset=standard  \"\n"\
" oder \"-lameopts  cbr:preset=192       \"\n"\
" oder \"-lameopts      preset=172       \"\n"\
" oder \"-lameopts      preset=extreme   \"\n"\
"\n"\
"\n"\
"Ein paar Pseudonyme sind f�r den ABR-Modus verf�gbar:\n"\
"phone => 16kbps/Mono        phon+/lw/mw-eu/sw => 24kbps/Mono\n"\
"mw-us => 40kbps/Mono        voice => 56kbps/Mono\n"\
"fm/radio/tape => 112kbps    hifi => 160kbps\n"\
"cd => 192kbps               studio => 256kbps"
#define MSGTR_LameCantInit "Kann Optionen f�r Lame nicht setzen, �berpr�fe Bitrate/Samplerate. "\
"Manche sehr niedrige Bitraten (<32) ben�tigen niedrigere Sampleraten (z.B. -srate 8000). "\
"Wenn alles andere nicht funktioniert, versuche es mit einem Preset."
#define MSGTR_ConfigfileError "Konfigurationsdatei-Fehler"
#define MSGTR_ErrorParsingCommandLine "Fehler beim Parsen der Kommandozeile."
#define MSGTR_VideoStreamRequired "Videostream zwingend notwendig!\n"
#define MSGTR_ForcingInputFPS "Erzwungene Bildrate der Eingabedatei: %5.2ffps\n"
#define MSGTR_RawvideoDoesNotSupportAudio "Ausgabedateiformat RAWVIDEO unterst�tzt kein Audio - Audio wird deaktiviert.\n"
#define MSGTR_DemuxerDoesntSupportNosound "Dieser Demuxer unterst�tzt -nosound no nicht.\n"
#define MSGTR_MemAllocFailed "Speicherreservierung fehlgeschlagen."
#define MSGTR_NoMatchingFilter "Konnte passenden Filter/passendes ao-Format nicht finden!\n"
#define MSGTR_MP3WaveFormatSizeNot30 "sizeof(MPEGLAYER3WAVEFORMAT)==%d!=30, vielleicht fehlerhafter C-Compiler?\n"
#define MSGTR_NoLavcAudioCodecName "Audio LAVC, Fehlender Codec-Name!\n"
#define MSGTR_LavcAudioCodecNotFound "Audio LAVC, konnte Encoder f�r Codec %s nicht finden.\n"
#define MSGTR_CouldntAllocateLavcContext "Audio LAVC, konnte Kontext nicht zuordnen!\n"
#define MSGTR_CouldntOpenCodec "Konnte Codec %s nicht �ffnen, br=%d.\n"
#define MSGTR_CantCopyAudioFormat "Audioformat 0x%x ist nicht mit '-oac copy' kompatibel. Versuche bitte stattdessen '-oac pcm' oder benutze '-fafmttag', um ein anderes Format zu erzwingen.\n"

// cfg-mencoder.h:

#define MSGTR_MEncoderMP3LameHelp "\n\n"\
" vbr=<0-4>     Modus f�r variable Bitrate\n"\
"                0: cbr\n"\
"                1: mt\n"\
"                2: rh (Standard)\n"\
"                3: abr\n"\
"                4: mtrh\n"\
"\n"\
" abr           durchschnittliche Bitrate\n"\
"\n"\
" cbr           konstante Bitrate\n"\
"               Erzwingt auch den CBR-Modus bei nachfolgenden ABR-Voreinstellungen:\n"\
"\n"\
" br=<0-1024>   gibt die Bitrate in kBit an (nur bei CBR und ABR)\n"\
"\n"\
" q=<0-9>       Qualit�t (0-h�chste, 9-niedrigste) (nur bei VBR)\n"\
"\n"\
" aq=<0-9>      Qualit�t des Algorithmus (0-beste/am langsamsten,\n"\
"               9-schlechteste/am schnellsten)\n"\
"\n"\
" ratio=<1-100> Kompressionsverh�ltnis\n"\
"\n"\
" vol=<0-10>    Setzt die Audioeingangsverst�rkung\n"\
"\n"\
" mode=<0-3>    (Standard: auto)\n"\
"                0: Stereo\n"\
"                1: Joint-stereo\n"\
"                2: Dualchannel\n"\
"                3: Mono\n"\
"\n"\
" padding=<0-2>\n"\
"                0: kein Padding\n"\
"                1: alles\n"\
"                2: angepasst\n"\
"\n"\
" fast          Schaltet die schnellere Codierung bei nachfolgenden VBR-Presets\n"\
"               ein, liefert leicht schlechtere Qualit�t und h�here Bitraten.\n"\
"\n"\
" preset=<wert> Bietet die bestm�glichen Qualit�tseinstellungen.\n"\
"                 medium: VBR-Encodierung, gute Qualit�t\n"\
"                 (150-180 kbps Bitratenbereich)\n"\
"                 standard:  VBR-Encodierung, hohe Qualit�t\n"\
"                 (170-210 kbps Bitratenbereich)\n"\
"                 extreme: VBR-Encodierung, sehr hohe Qualit�t\n"\
"                 (200-240 kbps Bitratenbereich)\n"\
"                 insane:  CBR-Encodierung, h�chste Preset-Qualit�t\n"\
"                 (320 kbps Bitrate)\n"\
"                 <8-320>: ABR-Encodierung mit der angegebenen durchschnittlichen\n"\
"                          Bitrate\n\n"

//codec-cfg.c:
#define MSGTR_DuplicateFourcc "Doppelter FourCC."
#define MSGTR_TooManyFourccs "Zu viele FourCCs/Formate..."
#define MSGTR_ParseError "Fehler beim Parsen."
#define MSGTR_ParseErrorFIDNotNumber "Fehler beim Parsen (Format-ID keine Nummer?)."
#define MSGTR_ParseErrorFIDAliasNotNumber "Fehler beim Parsen (Alias der Format-ID keine Nummer?)."
#define MSGTR_DuplicateFID "Doppelte Format-ID."
#define MSGTR_TooManyOut "Zu viele Ausgabeformate..."
#define MSGTR_InvalidCodecName "\nCodecname(%s) ist ung�ltig!\n"
#define MSGTR_CodecLacksFourcc "\nCodec(%s) hat kein FourCC/Format!\n"
#define MSGTR_CodecLacksDriver "\nCodec(%s) hat keinen Treiber!\n"
#define MSGTR_CodecNeedsDLL "\nCodec(%s) braucht eine 'dll'!\n"
#define MSGTR_CodecNeedsOutfmt "\nCodec(%s) braucht ein 'outfmt'!\n"
#define MSGTR_CantAllocateComment "Kann Speicher f�r Kommentar nicht allozieren. "
#define MSGTR_GetTokenMaxNotLessThanMAX_NR_TOKEN "get_token(): max >= MAX_MR_TOKEN!"
#define MSGTR_ReadingFile "Lese %s: "
#define MSGTR_CantOpenFileError "Kann '%s' nicht �ffnen: %s\n"
#define MSGTR_CantGetMemoryForLine "Bekomme keinen Speicher f�r 'line': %s\n"
#define MSGTR_CantReallocCodecsp "Kann '*codecsp' nicht erneut allozieren: %s\n"
#define MSGTR_CodecNameNotUnique "Codecname '%s' ist nicht eindeutig."
#define MSGTR_CantStrdupName "Kann strdup nicht ausf�hren -> 'name': %s\n"
#define MSGTR_CantStrdupInfo "Kann strdup nicht ausf�hren -> 'info': %s\n"
#define MSGTR_CantStrdupDriver "Kann strdup nicht ausf�hren -> 'driver': %s\n"
#define MSGTR_CantStrdupDLL "Kann strdup nicht ausf�hren -> 'dll': %s"
#define MSGTR_AudioVideoCodecTotals "%d Audio- & %d Videocodecs\n"
#define MSGTR_CodecDefinitionIncorrect "Codec ist nicht korrekt definiert."
#define MSGTR_OutdatedCodecsConf "Diese codecs.conf ist zu alt und nicht kompatibel mit dieser Version von MPlayer!"

// divx4_vbr.c:
#define MSGTR_OutOfMemory "Kein Speicher mehr verf�gbar!"
#define MSGTR_OverridingTooLowBitrate "Angegebene Bitrate ist zu niedrig f�r diesen Clip.\n"\
"Minimal m�gliche Bitrate f�r den Clip ist %.0f kbps. Hebe\n"\
"den vom Benutzer angegebenen Wert auf.\n"

// fifo.c
#define MSGTR_CannotMakePipe "Kann PIPE nicht anlegen!\n"

// m_config.c
#define MSGTR_SaveSlotTooOld "Zu alte Speicherstelle gefunden von lvl %d: %d !!!\n"
#define MSGTR_InvalidCfgfileOption "Die Option %s kann in Konfigurationsdateien nicht verwendet werden.\n"
#define MSGTR_InvalidCmdlineOption "Die Option %s kann auf der Kommandozeile nicht verwendet werden.\n"
#define MSGTR_InvalidSuboption "Fehler: Option '%s' hat keine Unteroption '%s'.\n"
#define MSGTR_MissingSuboptionParameter "Fehler: Unteroption '%s' von '%s' ben�tigt einen Parameter!\n"
#define MSGTR_MissingOptionParameter "Fehler: Option '%s' ben�tigt einen Parameter!\n"
#define MSGTR_OptionListHeader "\n Name                 Typ             Min        Max      Global  CL    Cfg\n\n"
#define MSGTR_TotalOptions "\nInsgesamt: %d Optionen.\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "CD-ROM-Laufwerk '%s' nicht gefunden.\n"
#define MSGTR_ErrTrackSelect "Fehler beim Ausw�hlen des VCD Tracks."
#define MSGTR_ReadSTDIN "Lese von Standardeingabe (stdin)...\n"
#define MSGTR_UnableOpenURL "Kann URL nicht �ffnen: %s\n"
#define MSGTR_ConnToServer "Verbunden mit Server: %s\n"
#define MSGTR_FileNotFound "Datei nicht gefunden: '%s'\n"

#define MSGTR_SMBInitError "Kann die Bibliothek libsmbclient nicht �ffnen: %d\n"
#define MSGTR_SMBFileNotFound "Konnte '%s' nicht �ber das Netzwerk �ffnen.\n"
#define MSGTR_SMBNotCompiled "MPlayer wurde ohne SMB-Unterst�tzung kompiliert.\n"

#define MSGTR_CantOpenDVD "Kann DVD-Laufwerk nicht �ffnen: %s\n"
#define MSGTR_NoDVDSupport "MPlayer wurde ohne DVD-Unterst�tzung �bersetzt.\n"
#define MSGTR_DVDwait "Lese Disc-Struktur, bitte warten...\n"
#define MSGTR_DVDnumTitles "Es sind %d Titel auf dieser DVD.\n"
#define MSGTR_DVDinvalidTitle "Ung�ltige DVD-Titelnummer: %d\n"
#define MSGTR_DVDnumChapters "Es sind %d Kapitel in diesem DVD-Titel.\n"
#define MSGTR_DVDinvalidChapter "Ung�ltige DVD-Kapitelnummer: %d\n"
#define MSGTR_DVDinvalidChapterRange "Ung�ltige Kapitelbereichsangabe: %s\n"
#define MSGTR_DVDinvalidLastChapter "Ung�ltiger Wert f�r das letzte DVD-Kapitel: %d\n"
#define MSGTR_DVDnumAngles "Es sind %d Kameraeinstellungen diesem DVD-Titel.\n"
#define MSGTR_DVDinvalidAngle "Ung�ltige DVD-Kameraeinstellungsnummer %d.\n"
#define MSGTR_DVDnoIFO "Kann die IFO-Datei f�r den DVD-Titel %d nicht �ffnen.\n"
#define MSGTR_DVDnoVMG "Kann VMG-Informationen nicht �ffnen!\n"
#define MSGTR_DVDnoVOBs "Kann VOB-Dateien des Titels  (VTS_%02d_1.VOB) nicht �ffnen.\n"
#define MSGTR_DVDnoMatchingAudio "Keine passende DVD-Tonspur gefunden!\n"
#define MSGTR_DVDaudioChannel "Ausgew�hlte DVD-Audiospur: %d Sprache: %c%c\n"
#define MSGTR_DVDnoMatchingSubtitle "Keine passende Untertitelspur gefunden!\n"
#define MSGTR_DVDsubtitleChannel "Ausgew�hlte DVD-Untertitelspur: %d Sprache: %c%c\n"
#define MSGTR_DVDopenOk "DVD erfolgreich ge�ffnet.\n"

// muxer.c, muxer_*.c:
#define MSGTR_TooManyStreams "Zu viele Streams!"
#define MSGTR_RawMuxerOnlyOneStream "Der rawaudio-Muxer unterst�tzt nur einen Audiostream!\n"
#define MSGTR_IgnoringVideoStream "Ignoriere Videostream!\n"
#define MSGTR_UnknownStreamType "Warnung! Unbekannter Streamtyp: %d\n"
#define MSGTR_WarningLenIsntDivisible "Warnung! 'len' ist nicht durch 'samplesize' teilbar!\n"

#define MSGTR_MuxbufMallocErr "Speicher f�r Muxer-Framepuffer konnte nicht alloziert werden!\n"
#define MSGTR_MuxbufReallocErr "Speicher f�r Muxer-Framepuffer konnte nicht vergr��ert werden!\n"
#define MSGTR_MuxbufSending "Muxer-Framepuffer: Sende %d Frame(s) zum Muxer.\n"
#define MSGTR_WritingHeader "Schreibe Dateikopf...\n"
#define MSGTR_WritingTrailer "Schreibe Dateiindex...\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "Warnung! Audiostream-Header %d neu definiert!\n"
#define MSGTR_VideoStreamRedefined "Warnung! Videostream-Header %d neu definiert!\n"
#define MSGTR_TooManyAudioInBuffer "\nZu viele Audiopakete im Puffer: (%d in %d bytes).\n"
#define MSGTR_TooManyVideoInBuffer "\nZu viele Videopakete im Puffer: (%d in %d bytes).\n"
#define MSGTR_MaybeNI "Vielleicht spielst du eine(n) nicht-interleaved Stream/Datei, oder der Codec funktioniert nicht.\n" \
                      "Versuche bei AVI-Dateien, den nicht-interleaved Modus mit der Option -ni zu erzwingen.\n"
#define MSGTR_SwitchToNi "\nSchlecht interleavte AVI-Datei erkannt, wechsele in den -ni Modus!\n"
#define MSGTR_Detected_XXX_FileFormat "%s-Dateiformat erkannt!\n"
#define MSGTR_DetectedAudiofile "Audiodatei erkannt!\n"
#define MSGTR_NotSystemStream "Kein MPEG System Stream... (vielleicht ein Transport Stream?)\n"
#define MSGTR_InvalidMPEGES "Ung�ltiger MPEG-ES Stream??? Kontaktiere den Autor, das k�nnte ein Bug sein :(\n"
#define MSGTR_FormatNotRecognized "========== Sorry, dieses Dateiformat wird nicht erkannt/unterst�tzt ==========\n"\
                                  "============== Sollte dies ein AVI, ASF oder MPEG Stream sein, ===============\n"\
                                  "====================== dann kontaktiere bitte den Autor. =====================\n"
#define MSGTR_MissingVideoStream "Kein Videostream gefunden.\n"
#define MSGTR_MissingAudioStream "Kein Audiostream gefunden. -> kein Ton.\n"
#define MSGTR_MissingVideoStreamBug "Fehlender Videostream!? Kontaktiere den Autor, dies k�nnte ein Bug sein :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: Datei enth�lt den gew�hlten Audio- oder Videostream nicht.\n"

#define MSGTR_NI_Forced "erzwungen"
#define MSGTR_NI_Detected "erkannt"
#define MSGTR_NI_Message "NICHT-INTERLEAVETES AVI-Dateiformat %s.\n"

#define MSGTR_UsingNINI "Verwende defektes NICHT-INTERLEAVED AVI-Dateiformat.\n"
#define MSGTR_CouldntDetFNo "Konnte die Anzahl der Frames (f�r absolute Suche) nicht feststellen.\n"
#define MSGTR_CantSeekRawAVI "Suche in reinen AVI-Streams nicht durchf�hrbar (Index erforderlich, probiere die '-idx'-Option.).\n"
#define MSGTR_CantSeekFile "Kann diese Datei nicht durchsuchen.\n"

#define MSGTR_EncryptedVOB "Verschl�sselte VOB-Datei! Lies DOCS/HTML/en/dvd.html.\n"

#define MSGTR_MOVcomprhdr "MOV: komprimierte Header ben�tigen ZLIB-Unterst�tzung.\n"
#define MSGTR_MOVvariableFourCC "MOV: Warnung: Variable FOURCC erkannt!?\n"
#define MSGTR_MOVtooManyTrk "MOV: WARNUNG: Zu viele Tracks."
#define MSGTR_FoundAudioStream "==> Audiostream gefunden: %d\n"
#define MSGTR_FoundVideoStream "==> Videostream gefunden: %d\n"
#define MSGTR_DetectedTV "TV erkannt! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "�ffnen des OGG-Demuxers fehlgeschlagen.\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: Suche nach Audiostream (Id:%d).\n"
#define MSGTR_CannotOpenAudioStream "Kann Audiostream nicht �ffnen: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "Kann Untertitelstream nicht �ffnen: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "�ffnen des Audio-Demuxers fehlgeschlagen: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "�ffnen des Untertitel-Demuxers fehlgeschlagen: %s\n"
#define MSGTR_TVInputNotSeekable "TV-Input ist nicht durchsuchbar (Suche des Kanals?).\n"
#define MSGTR_DemuxerInfoAlreadyPresent "Demuxerinfo %s schon vorhanden.\n"
#define MSGTR_ClipInfo "Clip-Info:\n"

#define MSGTR_LeaveTelecineMode "\ndemux_mpg: 30000/1001fps NTSC-Inhalt erkannt, wechsele Framerate.\n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: 24000/1001fps progressiver NTSC-Inhalt erkannt, wechsele Framerate.\n"

#define MSGTR_CacheFill "\rF�lle Zwischenpuffer: %5.2f%% (%d Byte)   "
#define MSGTR_NoBindFound "Taste '%s' ist nicht gebunden."
#define MSGTR_FailedToOpen "Konnte '%s' nicht �ffnen.\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "Konnte Codec nicht �ffnen.\n"
#define MSGTR_CantCloseCodec "Konnte Codec nicht schlie�en.\n"

#define MSGTR_MissingDLLcodec "FEHLER: Kann erforderlichen DirectShow-Codec nicht �ffnen: %s\n"
#define MSGTR_ACMiniterror "Kann Win32/ACM-Audiocodec nicht laden/initialisieren (fehlende DLL-Datei?).\n"
#define MSGTR_MissingLAVCcodec "Kann Codec '%s' von libavcodec nicht finden...\n"

#define MSGTR_MpegNoSequHdr "MPEG: FATAL: EOF (Ende der Datei) w�hrend der Suche nach Sequenzheader.\n"
#define MSGTR_CannotReadMpegSequHdr "FATAL: Kann Sequenzheader nicht lesen.\n"
#define MSGTR_CannotReadMpegSequHdrEx "FATAL: Kann Sequenzheader-Erweiterung nicht lesen.\n"
#define MSGTR_BadMpegSequHdr "MPEG: Schlechter Sequenzheader.\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: Schlechte Sequenzheader-Erweiterung.\n"

#define MSGTR_ShMemAllocFail "Kann keinen gemeinsamen Speicher reservieren.\n"
#define MSGTR_CantAllocAudioBuf "Kann keinen Audioausgabe-Puffer reservieren.\n"

#define MSGTR_UnknownAudio "Unbekanntes/fehlendes Audioformat -> kein Ton\n"

#define MSGTR_UsingExternalPP "[PP] Verwende externe Postprocessing-Filter, max q = %d.\n"
#define MSGTR_UsingCodecPP "[PP] Verwende Postprocessing-Routinen des Codecs, max q = %d.\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "Videoeigenschaft '%s' wird von ausgew�hlten vo & vd nicht unterst�tzt.\n"
#define MSGTR_VideoCodecFamilyNotAvailableStr "Erforderliche Videocodec Familie [%s] (vfm=%s) nicht verf�gbar.\nAktiviere sie beim Kompilieren.\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "Erforderliche Audiocodec-Familie [%s] (afm=%s) nicht verf�gbar.\nAktiviere sie beim Kompilieren.\n"
#define MSGTR_OpeningVideoDecoder "�ffne Videodecoder: [%s] %s\n"
#define MSGTR_SelectedVideoCodec "Ausgew�hlter Videocodec: [%s] vfm: %s (%s)\n"
#define MSGTR_OpeningAudioDecoder "�ffne Audiodecoder: [%s] %s\n"
#define MSGTR_SelectedAudioCodec "Ausgew�hlter Audiocodec: [%s] afm: %s (%s)\n"
#define MSGTR_BuildingAudioFilterChain "Baue Tonfilterkette von %dHz/%dch/%s nach %dHz/%dch/%s auf...\n"
#define MSGTR_UninitVideoStr "Deinitialisiere Video: %s\n"
#define MSGTR_UninitAudioStr "Deinitialisiere Audio: %s\n"
#define MSGTR_VDecoderInitFailed "Initialisierung des Videodecoders fehlgeschlagen :(\n"
#define MSGTR_ADecoderInitFailed "Initialisierung des Audiodecoders fehlgeschlagen :(\n"
#define MSGTR_ADecoderPreinitFailed "Vorinitialisierung des Audiodecoders fehlgeschlagen :(\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: Reserviere %d Bytes f�r den Eingangspuffer.\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: Reserviere %d + %d = %d Bytes f�r den Ausgabepuffer.\n"

// LIRC:
#define MSGTR_SettingUpLIRC "Initialisiere LIRC-Unterst�tzung...\n"
#define MSGTR_LIRCdisabled "Verwendung der Fernbedienung nicht m�glich.\n"
#define MSGTR_LIRCopenfailed "Fehler beim �ffnen der LIRC-Unterst�tzung.\n"
#define MSGTR_LIRCcfgerr "Kann LIRC-Konfigurationsdatei %s nicht lesen.\n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "Konnte Videofilter '%s' nicht finden.\n"
#define MSGTR_CouldNotOpenVideoFilter "Konnte Videofilter '%s' nicht �ffnen.\n"
#define MSGTR_OpeningVideoFilter "�ffne Videofilter: "
#define MSGTR_CannotFindColorspace "Konnte keinen passenden Farbraum finden, auch nicht mit '-vf scale'. :-(\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDec: Codec hat sh->disp_w und sh->disp_h nicht gesetzt!\nVersuche Problem zu umgehen..\n"
#define MSGTR_VoConfigRequest "VDec: VO wird versucht, auf %d x %d (Bevorzugter Farbraum: %s) zu setzen.\n"
#define MSGTR_CouldNotFindColorspace "Konnte keinen passenden Farbraum finden - neuer Versuch mit '-vf scale'...\n"
#define MSGTR_MovieAspectIsSet "Film-Aspekt ist %.2f:1 - Vorskalierung zur Korrektur der Seitenverh�ltnisse.\n"
#define MSGTR_MovieAspectUndefined "Film-Aspekt ist undefiniert - keine Vorskalierung durchgef�hrt.\n"

// vd_dshow.c, vd_dmo.c
#define MSGTR_DownloadCodecPackage "Du mu�t das Bin�rcodec-Paket aktualisieren/installieren.\nGehe dazu auf http://mplayerhq.hu/homepage/dload.html\n"
#define MSGTR_DShowInitOK "INFO: Win32/DShow Videocodec-Initialisierung OK.\n"
#define MSGTR_DMOInitOK "INFO: Win32/DMO Videocodec-Initialisierung OK.\n"

// x11_common.c
#define MSGTR_EwmhFullscreenStateFailed "\nX11: Konnte EWMH-Fullscreen-Event nicht senden!\n"
#define MSGTR_CouldNotFindXScreenSaver "xscreensaver_disable: Konnte das XScreeSaver-Fenster nicht finden.\n"
#define MSGTR_SelectedVideoMode "XF86VM: Ausgew�hlter Videomodus %dx%d f�r Bildgr��e %dx%d.\n"

#define MSGTR_InsertingAfVolume "[Mixer] Kein Hardware-Mixing, f�ge Lautst�rkefilter ein.\n"
#define MSGTR_NoVolume "[Mixer] Keine Lautst�rkeregelung verf�gbar.\n"

// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "�ber..."
#define MSGTR_FileSelect "W�hle Datei..."
#define MSGTR_SubtitleSelect "W�hle Untertitel..."
#define MSGTR_OtherSelect "W�hle..."
#define MSGTR_AudioFileSelect "W�hle externen Audiokanal..."
#define MSGTR_FontSelect "W�hle Schrift..."
// Beachte: Wenn du MSGTR_PlayList �nderst, �berpr�fe bitte, ob der Eintrag noch zu MSGTR_MENU_PlayList passt.
#define MSGTR_PlayList "Playlist"
#define MSGTR_Equalizer "Equalizer"
#define MSGTR_SkinBrowser "Skin-Browser"
#define MSGTR_Network "Netzwerk-Streaming..."
// Beachte: Wenn du MSGTR_Preferences �nderst, �berpr�fe bitte, ob der Eintrag noch zu MSGTR_MENU_Preferences passt.
#define MSGTR_Preferences "Einstellungen"
#define MSGTR_AudioPreferences "Audio-Treiberkonfiguration"
#define MSGTR_NoMediaOpened "Keine Medien ge�ffnet."
#define MSGTR_VCDTrack "VCD-Titel %d"
#define MSGTR_NoChapter "kein Kapitel"
#define MSGTR_Chapter "Kapitel %d"
#define MSGTR_NoFileLoaded "Keine Datei geladen."

// --- buttons ---
#define MSGTR_Ok "Ok"
#define MSGTR_Cancel "Abbrechen"
#define MSGTR_Add "Hinzuf�gen"
#define MSGTR_Remove "Entfernen"
#define MSGTR_Clear "L�schen"
#define MSGTR_Config "Konfiguration"
#define MSGTR_ConfigDriver "Konfiguriere Treiber"
#define MSGTR_Browse "Durchsuchen"

// --- error messages ---
#define MSGTR_NEMDB "Sorry, nicht genug Speicher f�r den Zeichnungs-Puffer."
#define MSGTR_NEMFMR "Sorry, nicht genug Speicher f�r Men�-Rendering."
#define MSGTR_IDFGCVD "Sorry, kann keinen GUI-kompatiblen Ausgabetreiber finden."
#define MSGTR_NEEDLAVCFAME "Sorry, du versuchst, Nicht-MPEG Dateien ohne erneute Encodierung abzuspielen.\nBitte aktiviere lavc oder fame in der DXR3/H+-Configbox."
#define MSGTR_UNKNOWNWINDOWTYPE "Unbekannten Fenstertyp gefunden ..."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[Skin] Fehler in Skin-Konfigurationsdatei in Zeile %d: %s" 
#define MSGTR_SKIN_WARNING1 "[Skin] Warnung in Skin-Konfigurationsdatei in Zeile %d:\nWidget (%s) gefunden, aber davor wurde \"section\" nicht gefunden"
#define MSGTR_SKIN_WARNING2 "[Skin] Warnung in Skin-Konfigurationsdatei in Zeile %d:\nWidget (%s) gefunden, aber davor wurde \"subsection\" nicht gefunden (%s)"
#define MSGTR_SKIN_WARNING3 "[skin] Warnung in Skin-Konfigurationsdatei in Zeile %d:\nDiese Untersektion wird vom Widget nicht unterst�tzt (%s)."
#define MSGTR_SKIN_SkinFileNotFound "[skin] Datei ( %s ) nicht gefunden.\n"
#define MSGTR_SKIN_SkinFileNotReadable "[skin] Datei ( %s ) nicht lesbar.\n"
#define MSGTR_SKIN_BITMAP_16bit  "Bitmaps mit 16 Bits oder weniger werden nicht unterst�tzt (%s).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "Datei nicht gefunden (%s)\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "BMP-Lesefehler (%s)\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "TGA-Lesefehler (%s)\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "PNG-Lesefehler (%s)\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "RLE-gepacktes TGA wird nicht unterst�tzt (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "unbekannter Dateityp (%s)\n"
#define MSGTR_SKIN_BITMAP_ConvertError "Konvertierungsfehler von 24 Bit auf 32 Bit (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "unbekannte Nachricht: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "nicht genug Speicher\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "Zu viele Schriften deklariert.\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "Schriftdatei nicht gefunden.\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "Schriftbilddatei nicht gefunden.\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "nicht existierende Schriftbezeichnung (%s)\n"
#define MSGTR_SKIN_UnknownParameter "unbekannter Parameter (%s)\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "Skin nicht gefunden (%s).\n"
#define MSGTR_SKIN_SKINCFG_SelectedSkinNotFound "Ausgew�hltes Skin ( %s ) wurde nicht gefunden, versuche 'Standard'...\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "Skin-Konfigurationsdatei: Lesefehler (%s)\n"
#define MSGTR_SKIN_LABEL "Skins:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "�ber MPlayer"
#define MSGTR_MENU_Open "�ffnen..."
#define MSGTR_MENU_PlayFile "Spiele Datei..."
#define MSGTR_MENU_PlayVCD "Spiele VCD..."
#define MSGTR_MENU_PlayDVD "Spiele DVD..."
#define MSGTR_MENU_PlayURL "Spiele URL..."
#define MSGTR_MENU_LoadSubtitle "Lade Untertitel..."
#define MSGTR_MENU_DropSubtitle "Entferne Untertitel..."
#define MSGTR_MENU_LoadExternAudioFile "Lade externe Audiodatei..."
#define MSGTR_MENU_Playing "Spiele"
#define MSGTR_MENU_Play "Abspielen"
#define MSGTR_MENU_Pause "Pause"
#define MSGTR_MENU_Stop "Stop"
#define MSGTR_MENU_NextStream "N�chster Stream"
#define MSGTR_MENU_PrevStream "Vorheriger Stream"
#define MSGTR_MENU_Size "Gr��e"
#define MSGTR_MENU_HalfSize "Halbe Gr��e"
#define MSGTR_MENU_NormalSize "Normale Gr��e"
#define MSGTR_MENU_DoubleSize "Doppelte Gr��e"
#define MSGTR_MENU_FullScreen "Vollbild"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "�ffne CD/DVD..."
#define MSGTR_MENU_ShowDVDMenu "Zeige DVD Men�"
#define MSGTR_MENU_Titles "Titel"
#define MSGTR_MENU_Title "Titel %2d"
#define MSGTR_MENU_None "(nichts)"
#define MSGTR_MENU_Chapters "Kapitel"
#define MSGTR_MENU_Chapter "Kapitel %2d"
#define MSGTR_MENU_AudioLanguages "Audio-Sprachen"
#define MSGTR_MENU_SubtitleLanguages "Untertitel-Sprachen"
#define MSGTR_MENU_PlayList MSGTR_PlayList
#define MSGTR_MENU_SkinBrowser "Skinbrowser"
#define MSGTR_MENU_Preferences MSGTR_Preferences
#define MSGTR_MENU_Exit "Beenden..."
#define MSGTR_MENU_Mute "Stummschaltung"
#define MSGTR_MENU_Original "Original"
#define MSGTR_MENU_AspectRatio "Seitenverh�ltnis"
#define MSGTR_MENU_AudioTrack "Audiospur"
#define MSGTR_MENU_Track "Spur %d"
#define MSGTR_MENU_VideoTrack "Videospur"

// --- equalizer
// Beachte: Wenn du MSGTR_EQU_Audio �nderst, �berpr�fe bitte, ob der Eintrag noch zu MSGTR_PREFERENCES_Audio passt.
#define MSGTR_EQU_Audio "Audio"
// Beachte: Wenn du MSGTR_EQU_Video �nderst, �berpr�fe bitte, ob der Eintrag noch zu MSGTR_PREFERENCES_Video passt.
#define MSGTR_EQU_Video "Video"
#define MSGTR_EQU_Contrast "Kontrast: "
#define MSGTR_EQU_Brightness "Helligkeit: "
#define MSGTR_EQU_Hue "Farbton: "
#define MSGTR_EQU_Saturation "S�ttigung: "
#define MSGTR_EQU_Front_Left "Vorne Links"
#define MSGTR_EQU_Front_Right "Vorne Rechts"
#define MSGTR_EQU_Back_Left "Hinten Links"
#define MSGTR_EQU_Back_Right "Hinten Rechts"
#define MSGTR_EQU_Center "Mitte"
#define MSGTR_EQU_Bass "Bass" // LFE
#define MSGTR_EQU_All "Alle"
#define MSGTR_EQU_Channel1 "Kanal 1:"
#define MSGTR_EQU_Channel2 "Kanal 2:"
#define MSGTR_EQU_Channel3 "Kanal 3:"
#define MSGTR_EQU_Channel4 "Kanal 4:"
#define MSGTR_EQU_Channel5 "Kanal 5:"
#define MSGTR_EQU_Channel6 "Kanal 6:"

// --- playlist
#define MSGTR_PLAYLIST_Path "Pfad"
#define MSGTR_PLAYLIST_Selected "Ausgew�hlte Dateien"
#define MSGTR_PLAYLIST_Files "Dateien"
#define MSGTR_PLAYLIST_DirectoryTree "Verzeichnisbaum"

// --- preferences
#define MSGTR_PREFERENCES_Audio MSGTR_EQU_Audio
#define MSGTR_PREFERENCES_Video MSGTR_EQU_Video
#define MSGTR_PREFERENCES_SubtitleOSD "Untertitel & OSD"
#define MSGTR_PREFERENCES_Codecs "Codecs & Demuxer"
// Beachte: Wenn du MSGTR_PREFERENCES_Misc �nderst, �berpr�fe bitte, ob der Eintrag noch zu MSGTR_PREFERENCES_FRAME_Misc passt.
#define MSGTR_PREFERENCES_Misc "Sonstiges"

#define MSGTR_PREFERENCES_None "Nichts"
#define MSGTR_PREFERENCES_DriverDefault "Treiber-Standardeinstellung"
#define MSGTR_PREFERENCES_AvailableDrivers "Verf�gbare Treiber:"
#define MSGTR_PREFERENCES_DoNotPlaySound "Spiele keinen Ton"
#define MSGTR_PREFERENCES_NormalizeSound "Normalisiere Ton"
#define MSGTR_PREFERENCES_EnEqualizer "Equalizer verwenden"
#define MSGTR_PREFERENCES_SoftwareMixer "Aktiviere Software-Mixer"
#define MSGTR_PREFERENCES_ExtraStereo "Extra Stereo verwenden"
#define MSGTR_PREFERENCES_Coefficient "Koeffizient:"
#define MSGTR_PREFERENCES_AudioDelay "Audio-Verz�gerung"
#define MSGTR_PREFERENCES_DoubleBuffer "Doublebuffering verwenden"
#define MSGTR_PREFERENCES_DirectRender "Direct-Rendering verwenden"
#define MSGTR_PREFERENCES_FrameDrop "Frame-Dropping aktivieren"
#define MSGTR_PREFERENCES_HFrameDrop "HARTES Frame-Dropping aktivieren (gef�hrlich)"
#define MSGTR_PREFERENCES_Flip "Bild horizontal spiegeln"
#define MSGTR_PREFERENCES_Panscan "Panscan: "
#define MSGTR_PREFERENCES_OSDTimer "Zeit und Indikatoren"
#define MSGTR_PREFERENCES_OSDProgress "Nur Fortschrittsbalken"
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "Timer, prozentuale und absolute Zeit"
#define MSGTR_PREFERENCES_Subtitle "Untertitel:"
#define MSGTR_PREFERENCES_SUB_Delay "Verz�gerung: "
#define MSGTR_PREFERENCES_SUB_FPS "FPS:"
#define MSGTR_PREFERENCES_SUB_POS "Position: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "Automatisches Laden der Untertitel ausschalten"
#define MSGTR_PREFERENCES_SUB_Unicode "Unicode-Untertitel"
#define MSGTR_PREFERENCES_SUB_MPSUB "Konvertiere Untertitel in das MPlayer-Untertitelformat"
#define MSGTR_PREFERENCES_SUB_SRT "Konvertiere Untertitel in das zeitbasierte SubViewer-Untertitelformat (SRT)"
#define MSGTR_PREFERENCES_SUB_Overlap "Schalte Untertitel�berlappung ein/aus"
#define MSGTR_PREFERENCES_Font "Schrift:"
#define MSGTR_PREFERENCES_FontFactor "Schriftfaktor:"
#define MSGTR_PREFERENCES_PostProcess "Postprocessing aktivieren:"
#define MSGTR_PREFERENCES_AutoQuality "Auto-Qualit�t: "
#define MSGTR_PREFERENCES_NI "Nicht-Interleaved AVI Parser verwenden"
#define MSGTR_PREFERENCES_IDX "Indextabelle neu erstellen, falls ben�tigt"
#define MSGTR_PREFERENCES_VideoCodecFamily "Videocodec-Familie:"
#define MSGTR_PREFERENCES_AudioCodecFamily "Audiocodec-Familie:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "OSD-Modus"
#define MSGTR_PREFERENCES_FRAME_Subtitle "Untertitel"
#define MSGTR_PREFERENCES_FRAME_Font "Schrift"
#define MSGTR_PREFERENCES_FRAME_PostProcess "Postprocessing"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "Codec & Demuxer"
#define MSGTR_PREFERENCES_FRAME_Cache "Cache"
#define MSGTR_PREFERENCES_FRAME_Misc MSGTR_PREFERENCES_Misc
#define MSGTR_PREFERENCES_Audio_Device "Ger�t:"
#define MSGTR_PREFERENCES_Audio_Mixer "Mixer:"
#define MSGTR_PREFERENCES_Audio_MixerChannel "Mixer-Kanal:"
#define MSGTR_PREFERENCES_Message "Bitte bedenke, dass manche Optionen einen Neustart der Wiedergabe erfordern."
#define MSGTR_PREFERENCES_DXR3_VENC "Videoencoder:"
#define MSGTR_PREFERENCES_DXR3_LAVC "Verwende LAVC (FFmpeg)"
#define MSGTR_PREFERENCES_DXR3_FAME "Verwende FAME"
#define MSGTR_PREFERENCES_FontEncoding1 "Unicode"
#define MSGTR_PREFERENCES_FontEncoding2 "Westeurop�ische Sprachen (ISO-8859-1)"
#define MSGTR_PREFERENCES_FontEncoding3 "Westeurop�ische Sprachen mit Euro (ISO-8859-15)"
#define MSGTR_PREFERENCES_FontEncoding4 "Slavische / Westeurop�ische Sprache (ISO-8859-2)"
#define MSGTR_PREFERENCES_FontEncoding5 "Esperanto, Galizisch, Maltesisch, T�rkisch (ISO-8859-3)"
#define MSGTR_PREFERENCES_FontEncoding6 "Alte Baltische Schriftzeichen (ISO-8859-4)"
#define MSGTR_PREFERENCES_FontEncoding7 "Kyrillisch (ISO-8859-5)"
#define MSGTR_PREFERENCES_FontEncoding8 "Arabisch (ISO-8859-6)"
#define MSGTR_PREFERENCES_FontEncoding9 "Modernes Griechisch (ISO-8859-7)"
#define MSGTR_PREFERENCES_FontEncoding10 "T�rkisch (ISO-8859-9)"
#define MSGTR_PREFERENCES_FontEncoding11 "Baltisch (ISO-8859-13)"
#define MSGTR_PREFERENCES_FontEncoding12 "Keltisch (ISO-8859-14)"
#define MSGTR_PREFERENCES_FontEncoding13 "Hebr�ische Schriftzeichen (ISO-8859-8)"
#define MSGTR_PREFERENCES_FontEncoding14 "Russisch (KOI8-R)"
#define MSGTR_PREFERENCES_FontEncoding15 "Ukrainisch, Belarussisch (KOI8-U/RU)"
#define MSGTR_PREFERENCES_FontEncoding16 "Vereinfachte chinesische Schriftzeichen (CP936)"
#define MSGTR_PREFERENCES_FontEncoding17 "Traditionelle chinesische Schriftzeichen (BIG5)"
#define MSGTR_PREFERENCES_FontEncoding18 "Japanische Schriftzeichen (SHIFT-JIS)"
#define MSGTR_PREFERENCES_FontEncoding19 "Koreanische Schriftzeichen (CP949)"
#define MSGTR_PREFERENCES_FontEncoding20 "Thail�ndische Schriftzeichen (CP874)"
#define MSGTR_PREFERENCES_FontEncoding21 "Kyrillisch Windows (CP1251)"
#define MSGTR_PREFERENCES_FontEncoding22 "Slavisch / Zentraleurop�isch Windows (CP1250)"
#define MSGTR_PREFERENCES_FontNoAutoScale "Keine automatische Skalierung"
#define MSGTR_PREFERENCES_FontPropWidth "Proportional zur Breite des Films"
#define MSGTR_PREFERENCES_FontPropHeight "Proportional zur H�he des Films"
#define MSGTR_PREFERENCES_FontPropDiagonal "Proportional zur Diagonale des Films"
#define MSGTR_PREFERENCES_FontEncoding "Codierung:"
#define MSGTR_PREFERENCES_FontBlur "Unsch�rfe:"
#define MSGTR_PREFERENCES_FontOutLine "Zeichenumriss (Outline):"
#define MSGTR_PREFERENCES_FontTextScale "Textskalierung:"
#define MSGTR_PREFERENCES_FontOSDScale "OSD-Skalierung:"
#define MSGTR_PREFERENCES_Cache "Cache ein/aus"
#define MSGTR_PREFERENCES_CacheSize "Cachegr��e: "
#define MSGTR_PREFERENCES_LoadFullscreen "Im Vollbildmodus starten"
#define MSGTR_PREFERENCES_SaveWinPos "Speichere Fensterposition"
#define MSGTR_PREFERENCES_XSCREENSAVER "Deaktiviere XScreenSaver"
#define MSGTR_PREFERENCES_PlayBar "Aktiviere die Playbar"
#define MSGTR_PREFERENCES_AutoSync "AutoSync ein/aus"
#define MSGTR_PREFERENCES_AutoSyncValue "Autosyncwert: "
#define MSGTR_PREFERENCES_CDROMDevice "CD-ROM-Ger�t:"
#define MSGTR_PREFERENCES_DVDDevice "DVD-Ger�t:"
#define MSGTR_PREFERENCES_FPS "FPS des Films:"
#define MSGTR_PREFERENCES_ShowVideoWindow "Zeige Videofenster, wenn inaktiv"
#define MSGTR_PREFERENCES_ArtsBroken "Neuere Versionen von aRts sind mit GTK 1.x nicht kompatibel\n"\
           "und bringen GMPlayer zum Absturz!"

#define MSGTR_ABOUT_UHU "GUI-Entwicklung wurde von UHU Linux gesponsert.\n"
#define MSGTR_ABOUT_CoreTeam "   MPlayers Kernentwickler-Team:\n"
#define MSGTR_ABOUT_AdditionalCoders " Weitere Programmierer:\n"
#define MSGTR_ABOUT_MainTesters "     Haupttester:\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "Fataler Fehler!"
#define MSGTR_MSGBOX_LABEL_Error "Fehler!"
#define MSGTR_MSGBOX_LABEL_Warning "Warnung!"

// bitmap.c

#define MSGTR_NotEnoughMemoryC32To1 "[c32to1] F�r das Bild ist nicht gen�gend Speicher vorhanden.\n"
#define MSGTR_NotEnoughMemoryC1To32 "[c1to32] F�r das Bild ist nicht gen�gend Speicher vorhanden.\n"

// cfg.c

#define MSGTR_ConfigFileReadError "[cfg] Fehler beim Lesen der Konfigurationsdatei ...\n"
#define MSGTR_UnableToSaveOption "[cfg] Kann die Option '%s' nicht speichern.\n"

// interface.c

#define MSGTR_DeletingSubtitles "[GUI] L�sche Untertitel.\n"
#define MSGTR_LoadingSubtitles "[GUI] Lade Untertitel: %s\n"
#define MSGTR_AddingVideoFilter "[GUI] F�ge Videofilter %s hinzu.\n"
#define MSGTR_RemovingVideoFilter "[GUI] Entferne Videofilter: %s\n"

// mw.c

#define MSGTR_NotAFile "Dies scheint keine Datei zu sein: %s !\n"

// ws.c

#define MSGTR_WS_CouldNotOpenDisplay "[ws] Konnte das Display nicht �ffnen.\n"
#define MSGTR_WS_RemoteDisplay "[ws] Ferngesteuertes Display, deaktiviere XMITSHM.\n"
#define MSGTR_WS_NoXshm "[ws] Sorry, dein System unterst�tzt die Shared-Memory-Erweiterung von X nicht.\n"
#define MSGTR_WS_NoXshape "[ws] Sorry, dein System unterst�tzt die XShape-Erweiterung nicht.\n"
#define MSGTR_WS_ColorDepthTooLow "[ws] Sorry, die Farbtiefe ist zu niedrig.\n"
#define MSGTR_WS_TooManyOpenWindows "[ws] Es sind zu viele Fenster ge�ffnet.\n"
#define MSGTR_WS_ShmError "[ws] Fehler der Shared-Memory-Erweiterung\n"
#define MSGTR_WS_NotEnoughMemoryDrawBuffer "[ws] Sorry, nicht gen�gend Speicher zum Schreiben in den Buffer.\n"
#define MSGTR_WS_DpmsUnavailable "DPMS nicht verf�gbar?\n"
#define MSGTR_WS_DpmsNotEnabled "Konnte DPMS nicht aktivieren.\n"
 
// wsxdnd.c

#define MSGTR_WS_NotAFile "Dies scheint keine Datei zu sein...\n"
#define MSGTR_WS_DDNothing "D&D: Nichts zur�ckgegeben!\n"
 
#endif

 // ======================= VO Video Output drivers ========================

#define MSGTR_VOincompCodec "Der ausgew�hlte Videoausgabetreiber ist nicht kompatibel mit diesem Codec.\n"\
                "Versuche auch das Hinzuf�gen des Filters scale, z.B. -vf spp,scale an Stelle von -vf spp.\n"
#define MSGTR_VO_GenericError "Dieser Fehler ist aufgetreten"
#define MSGTR_VO_UnableToAccess "Zugriff nicht m�glich."
#define MSGTR_VO_ExistsButNoDirectory "existiert schon, ist aber kein Verzeichnis."
#define MSGTR_VO_DirExistsButNotWritable "Ausgabeverzeichnis existiert schon, ist aber nicht beschreibbar."
#define MSGTR_VO_DirExistsAndIsWritable "Ausgabeverzeichnis existiert schon und ist beschreibbar."
#define MSGTR_VO_CantCreateDirectory "Kann Ausgabeverzeichnis nicht erstellen."
#define MSGTR_VO_CantCreateFile "Kann Ausgabedatei nicht erstellen."
#define MSGTR_VO_DirectoryCreateSuccess "Ausgabeverzeichnis erfolgreich erstellt."
#define MSGTR_VO_ParsingSuboptions "Unteroptionen werden geparst."
#define MSGTR_VO_SuboptionsParsedOK "Parsen der Unteroptionen OK."
#define MSGTR_VO_ValueOutOfRange "Wert au�erhalb des g�ltigen Bereichs"
#define MSGTR_VO_NoValueSpecified "Kein Wert angegeben."
#define MSGTR_VO_UnknownSuboptions "Unbekannte Unteroption(en)"

// vo_aa.c

#define MSGTR_VO_AA_HelpHeader "\n\nDies sind die Unteroptionen von aalib vo_aa:\n"
#define MSGTR_VO_AA_AdditionalOptions "Zus�tzlich von vo_aa bereitgestellte Optionen:\n" \
" �help � � � �gib diese Hilfsnachricht aus\n" \
" �osdcolor � �setze OSD-Farbe\n �subcolor � �setze Untertitelfarbe\n" \
" � � � �die Farbparameter sind folgende:\n � � � � � 0 : normal\n" \
" � � � � � 1 : dim\n � � � � � 2 : bold\n � � � � � 3 : boldfont\n" \
" � � � � � 4 : reverse\n � � � � � 5 : special\n\n\n"

 // vo_jpeg.c
#define MSGTR_VO_JPEG_ProgressiveJPEG "Progressives JPEG aktiviert."
#define MSGTR_VO_JPEG_NoProgressiveJPEG "Progressives JPEG deaktiviert."
#define MSGTR_VO_JPEG_BaselineJPEG "Baseline-JPEG aktiviert."
#define MSGTR_VO_JPEG_NoBaselineJPEG "Baseline-JPEG deaktiviert."

// vo_pnm.c
#define MSGTR_VO_PNM_ASCIIMode "ASCII-Modus aktiviert."
#define MSGTR_VO_PNM_RawMode "Raw-Modus aktiviert."
#define MSGTR_VO_PNM_PPMType "Werde PPM-Dateien schreiben."
#define MSGTR_VO_PNM_PGMType "Werde PGM-Dateien schreiben."
#define MSGTR_VO_PNM_PGMYUVType "Werde PGMYUV-Dateien schreiben."

// vo_yuv4mpeg.c
#define MSGTR_VO_YUV4MPEG_InterlacedHeightDivisibleBy4 "Interlaced-Modus ben�tigt eine durch 4 teilbare Bildh�he."
#define MSGTR_VO_YUV4MPEG_InterlacedLineBufAllocFail "Kann Linien-Puffer f�r den Interlaced-Modus nicht allozieren."
#define MSGTR_VO_YUV4MPEG_InterlacedInputNotRGB "Eingabe ist nicht RGB, kann Chrominanz nicht in Felder separieren!"
#define MSGTR_VO_YUV4MPEG_WidthDivisibleBy2 "Bildh�he muss durch 2 teilbar sein."
#define MSGTR_VO_YUV4MPEG_NoMemRGBFrameBuf "Nicht genug Speicher, um RGB-Framebuffer zu allozieren."
#define MSGTR_VO_YUV4MPEG_OutFileOpenError "Bekomme keinen Speicher oder Datei-Handle, um \"%s\" zu schreiben!"
#define MSGTR_VO_YUV4MPEG_OutFileWriteError "Fehler beim Schreiben des Bildes auf die Ausgabe!"
#define MSGTR_VO_YUV4MPEG_UnknownSubDev "Unbekanntes Subdevice: %s"
#define MSGTR_VO_YUV4MPEG_InterlacedTFFMode "Benutze Interlaced-Ausgabemodus, oberes Feld (top-field) zuerst."
#define MSGTR_VO_YUV4MPEG_InterlacedBFFMode "Benutze Interlaced-Ausgabemodus, unteres Feld (bottom-field) zuerst."
#define MSGTR_VO_YUV4MPEG_ProgressiveMode "Benutze (Standard-) Progressive-Frame-Modus."

// sub.c

#define MSGTR_VO_SUB_Seekbar "Suchlaufleiste"
#define MSGTR_VO_SUB_Play "Play"
#define MSGTR_VO_SUB_Pause "Pause"
#define MSGTR_VO_SUB_Stop "Stop"
#define MSGTR_VO_SUB_Rewind "R�cklauf"
#define MSGTR_VO_SUB_Forward "Vorlauf"
#define MSGTR_VO_SUB_Clock "Spielzeit"
#define MSGTR_VO_SUB_Contrast "Kontrast"
#define MSGTR_VO_SUB_Saturation "S�ttigung"
#define MSGTR_VO_SUB_Volume "Lautst�rke"
#define MSGTR_VO_SUB_Brightness "Helligkeit"
#define MSGTR_VO_SUB_Hue "Farbwertkorrektur"

// vo_xv.c
#define MSGTR_VO_XV_ImagedimTooHigh "Dimensionen des Quellbildes sind zu gro�: %ux%u (Maximalgr��e ist %ux%u)\n"

// Old vo drivers that have been replaced

#define MSGTR_VO_PGM_HasBeenReplaced "Der pgm-Videoausgabetreiber wurde ersetzt durch -vo pnm:pgmyuv.\n"
#define MSGTR_VO_MD5_HasBeenReplaced "Der md5-Videoausgabetreiber wurde ersetzt durch -vo md5sum.\n"

// ======================= AO Audio Output drivers ========================

// libao2

// audio_out.c
#define MSGTR_AO_ALSA9_1x_Removed "audio_out: Die Module alsa9 und alsa1x wurden entfernt, benutze stattdessen -ao alsa.\n"

// ao_oss.c
#define MSGTR_AO_OSS_CantOpenMixer "[AO OSS] audio_setup: Kann Mixer %s: %s nicht �ffnen.\n"
#define MSGTR_AO_OSS_ChanNotFound "[AO OSS] audio_setup: Soundkartenmixer hat Kanal '%s' nicht, benutze Standard.\n"
#define MSGTR_AO_OSS_CantOpenDev "[AO OSS] audio_setup: Kann Audioger�t %s nicht �ffnen: %s\n"
#define MSGTR_AO_OSS_CantMakeFd "[AO OSS] audio_setup: Kann Dateideskriptor nicht anlegen, blockiert: %s\n"
#define MSGTR_AO_OSS_CantSet "[AO OSS] Kann Audioger�t %s nicht auf %s-Ausgabe setzen, versuche %s...\n"
#define MSGTR_AO_OSS_CantSetChans "[AO OSS] audio_setup: Audioger�t auf %d Kan�le zu setzen ist fehlgeschlagen.\n"
#define MSGTR_AO_OSS_CantUseGetospace "[AO OSS] audio_setup: Treiber unterst�tzt SNDCTL_DSP_GETOSPACE nicht :-(\n"
#define MSGTR_AO_OSS_CantUseSelect "[AO OSS]\n � *** Dein Audiotreiber unterst�tzt select() NICHT�***\nKompiliere MPlayer mit #undef HAVE_AUDIO_SELECT in der Datei config.h !\n\n"
#define MSGTR_AO_OSS_CantReopen "[AO OSS]\nKritischer Fehler: *** KANN AUDIO-GER�T NICHT ERNEUT �FFNEN / ZUR�CKSETZEN *** %s\n"

// ao_arts.c
#define MSGTR_AO_ARTS_CantInit "[AO ARTS] %s\n"
#define MSGTR_AO_ARTS_ServerConnect "[AO ARTS] Verbindung zum Soundserver hergestellt.\n"
#define MSGTR_AO_ARTS_CantOpenStream "[AO ARTS] Kann keinen Stream �ffnen.\n"
#define MSGTR_AO_ARTS_StreamOpen "[AO ARTS] Stream ge�ffnet.\n"
#define MSGTR_AO_ARTS_BufferSize "[AO ARTS] Gr��e des Buffers: %d\n"

// ao_dxr2.c
#define MSGTR_AO_DXR2_SetVolFailed "[AO DXR2] Die Lautst�rke auf %d zu setzen ist fehlgeschlagen.\n"
#define MSGTR_AO_DXR2_UnsupSamplerate "[AO DXR2] %d Hz nicht unterst�tzt, versuche Resampling.\n"

// ao_esd.c
#define MSGTR_AO_ESD_CantOpenSound "[AO ESD] esd_open_sound fehlgeschlagen: %s\n"
#define MSGTR_AO_ESD_LatencyInfo "[AO ESD] Latenz: [Server: %0.2fs, Netz: %0.2fs] (Anpassung %0.2fs)\n"
#define MSGTR_AO_ESD_CantOpenPBStream "[AO ESD] �ffnen des ESD-Wiedergabestreams fehlgeschlagen: %s\n"

// ao_mpegpes.c
#define MSGTR_AO_MPEGPES_CantSetMixer "[AO MPEGPES] Setzen des DVB-Audiomixers fehlgeschlagen: %s\n"
#define MSGTR_AO_MPEGPES_UnsupSamplerate "[AO MPEGPES] %d Hz nicht unterst�tzt, versuche Resampling.\n"

// ao_null.c
// Der hier hat weder mp_msg noch printf's?? [CHECK]

// ao_pcm.c
#define MSGTR_AO_PCM_FileInfo "[AO PCM] Datei: %s (%s)\nPCM: Samplerate: %iHz Kan�le: %s Format %s\n"
#define MSGTR_AO_PCM_HintInfo "[AO PCM] Info: Das Anlegen von Dump-Dateien wird am Schnellsten mit -vc dummy -vo null erreicht.\n[AO PCM] Info: Um WAVE-Dateien zu schreiben, benutze -ao pcm:waveheader (Standard).\n"
#define MSGTR_AO_PCM_CantOpenOutputFile "[AO PCM] �ffnen von %s zum Schreiben fehlgeschlagen!\n"

// ao_sdl.c
#define MSGTR_AO_SDL_INFO "[AO SDL] Samplerate: %iHz Kan�le: %s Format %s\n"
#define MSGTR_AO_SDL_DriverInfo "[AO SDL] benutze Audiotreiber %s.\n"
#define MSGTR_AO_SDL_UnsupportedAudioFmt "[AO SDL] Nichtunterst�tztes Audioformat: 0x%x.\n"
#define MSGTR_AO_SDL_CantInit "[AO SDL] Initialisierung von SDL-Audio fehlgeschlagen: %s\n"
#define MSGTR_AO_SDL_CantOpenAudio "[AO SDL] Kann Audio nicht �ffnen: %s\n"

// ao_sgi.c
#define MSGTR_AO_SGI_INFO "[AO SGI] Kontrolle.\n"
#define MSGTR_AO_SGI_InitInfo "[AO SGI] init: Samplerate: %iHz Kan�le: %s Format %s\n"
#define MSGTR_AO_SGI_InvalidDevice "[AO SGI] Wiedergabe: Ung�ltiges Ger�t.\n"
#define MSGTR_AO_SGI_CantSetParms_Samplerate "[AO SGI] init: setparams fehlgeschlagen: %s\nKonnte gew�nschte Samplerate nicht setzen.\n"
#define MSGTR_AO_SGI_CantSetAlRate "[AO SGI] init: AL_RATE wurde von der angegebenen Ressource nicht akzeptiert.\n"
#define MSGTR_AO_SGI_CantGetParms "[AO SGI] init: getparams fehlgeschlagen: %s\n"
#define MSGTR_AO_SGI_SampleRateInfo "[AO SGI] init: Samplerate ist jetzt %lf (gew�nschte Rate ist %lf).\n"
#define MSGTR_AO_SGI_InitConfigError "[AO SGI] init: %s\n"
#define MSGTR_AO_SGI_InitOpenAudioFailed "[AO SGI] init: Konnte Audiokanal nicht �ffnen: %s\n"
#define MSGTR_AO_SGI_Uninit "[AO SGI] uninit: ...\n"
#define MSGTR_AO_SGI_Reset "[AO SGI] reset: ...\n"
#define MSGTR_AO_SGI_PauseInfo "[AO SGI] audio_pause: ...\n"
#define MSGTR_AO_SGI_ResumeInfo "[AO SGI] audio_resume: ...\n"

// ao_sun.c
#define MSGTR_AO_SUN_RtscSetinfoFailed "[AO SUN] rtsc: SETINFO fehlgeschlagen.\n"
#define MSGTR_AO_SUN_RtscWriteFailed "[AO SUN] rtsc: Schreiben fehlgeschlagen.\n"
#define MSGTR_AO_SUN_CantOpenAudioDev "[AO SUN] Kann Audioger�t %s nicht �ffnen, %s �-> nosound.\n"
#define MSGTR_AO_SUN_UnsupSampleRate "[AO SUN] audio_setup: Deine Karte unterst�tzt %d Kan�le nicht, %s, %d Hz Samplerate.\n"
#define MSGTR_AO_SUN_CantUseSelect "[AO SUN]\n � *** Dein Audiotreiber unterst�tzt select() NICHT�***\nKompiliere MPlayer mit #undef HAVE_AUDIO_SELECT in der Datei config.h !\n\n"
#define MSGTR_AO_SUN_CantReopenReset "[AO SUN]\nKritischer Fehler: *** KANN AUDIO-GER�T NICHT ERNEUT �FFNEN / ZUR�CKSETZEN *** %s\n"

// ao_alsa5.c
#define MSGTR_AO_ALSA5_InitInfo "[AO ALSA5] alsa-init: angefordertes Format: %d Hz, %d Kan�le, %s\n"
#define MSGTR_AO_ALSA5_SoundCardNotFound "[AO ALSA5] alsa-init: Keine Soundkarten gefunden.\n"
#define MSGTR_AO_ALSA5_InvalidFormatReq "[AO ALSA5] alsa-init: ung�ltiges Format (%s) angefordert - Ausgabe deaktiviert.\n"
#define MSGTR_AO_ALSA5_PlayBackError "[AO ALSA5] alsa-init: Fehler beim �ffnen der Wiedergabe: %s\n"
#define MSGTR_AO_ALSA5_PcmInfoError "[AO ALSA5] alsa-init: PCM-Informatationsfehler: %s\n"
#define MSGTR_AO_ALSA5_SoundcardsFound "[AO ALSA5] alsa-init: %d Soundkarte(n) gefunden, benutze: %s\n"
#define MSGTR_AO_ALSA5_PcmChanInfoError "[AO ALSA5] alsa-init: PCM-Kanal-Informationsfehler: %s\n"
#define MSGTR_AO_ALSA5_CantSetParms "[AO ALSA5] alsa-init: Fehler beim Setzen der Parameter: %s\n"
#define MSGTR_AO_ALSA5_CantSetChan "[AO ALSA5] alsa-init: Fehler beim Setzen des Kanals: %s\n"
#define MSGTR_AO_ALSA5_ChanPrepareError "[AO ALSA5] alsa-init: Fehler beim Vorbereiten des Kanals: %s\n"
#define MSGTR_AO_ALSA5_DrainError "[AO ALSA5] alsa-uninit: Fehler beim Ablauf der Wiedergabe: %s\n"
#define MSGTR_AO_ALSA5_FlushError "[AO ALSA5] alsa-uninit: Wiedergabe-Flush-Fehler: %s\n"
#define MSGTR_AO_ALSA5_PcmCloseError "[AO ALSA5] alsa-uninit: Fehler beim Schlie�en von PCM: %s\n"
#define MSGTR_AO_ALSA5_ResetDrainError "[AO ALSA5] alsa-reset: Fehler beim Ablauf der Wiedergabe: %s\n"
#define MSGTR_AO_ALSA5_ResetFlushError "[AO ALSA5] alsa-reset: Wiedergabe-Flush-Fehler: %s\n"
#define MSGTR_AO_ALSA5_ResetChanPrepareError "[AO ALSA5] alsa-reset: Fehler beim Vorbereiten des Kanals: %s\n"
#define MSGTR_AO_ALSA5_PauseDrainError "[AO ALSA5] alsa-pause: Fehler beim Ablauf der Wiedergabe: %s\n"
#define MSGTR_AO_ALSA5_PauseFlushError "[AO ALSA5] alsa-pause: Wiedergabe-Flush-Fehler: %s\n"
#define MSGTR_AO_ALSA5_ResumePrepareError "[AO ALSA5] alsa-resume: Fehler beim Vorbereiten des Kanals: %s\n"
#define MSGTR_AO_ALSA5_Underrun "[AO ALSA5] alsa-play: Alsa-Underrun, setze Stream zur�ck.\n"
#define MSGTR_AO_ALSA5_PlaybackPrepareError "[AO ALSA5] alsa-play: Fehler beim Vorbereiten der Wiedergabe: %s\n"
#define MSGTR_AO_ALSA5_WriteErrorAfterReset "[AO ALSA5] alsa-play: Schreibfehler nach R�cksetzen: %s - gebe auf.\n"
#define MSGTR_AO_ALSA5_OutPutError "[AO ALSA5] alsa-play: Ausgabefehler: %s\n"

// ao_plugin.c

#define MSGTR_AO_PLUGIN_InvalidPlugin "[AO PLUGIN] ung�ltiges Plugin: %s\n"

// ======================= AF Audio Filters ================================

// libaf

// af_ladspa.c

#define MSGTR_AF_LADSPA_AvailableLabels "verf�gbare Label in"
#define MSGTR_AF_LADSPA_WarnNoInputs "WARNUNG! Dieses LADSPA-Plugin hat keine Audio-Inputs.\n  Das eingehende Audiosignal wird verlorengehen."
#define MSGTR_AF_LADSPA_ErrMultiChannel "Multi-channel (>2) Plugins werden (noch) nicht unterst�tzt.\n  Benutze nur Mono- und Stereo-Plugins."
#define MSGTR_AF_LADSPA_ErrNoOutputs "Dieses LADSPA-Plugin hat keine Audio-Outputs."
#define MSGTR_AF_LADSPA_ErrInOutDiff "Die Anzahl der Audio-Inputs und Audio-Outputs des LADSPA-Plugins sind verschieden."
#define MSGTR_AF_LADSPA_ErrFailedToLoad "Datei konnte nicht geladen werden:"
#define MSGTR_AF_LADSPA_ErrNoDescriptor "Konnte die Funktion ladspa_descriptor() in der angegebenen Bibliotheksdatei nicht finden."
#define MSGTR_AF_LADSPA_ErrLabelNotFound "Konnte Label in der Plugin-Bibliothek nicht finden."
#define MSGTR_AF_LADSPA_ErrNoSuboptions "Keine Suboptionen angegeben."
#define MSGTR_AF_LADSPA_ErrNoLibFile "Keine Bibliotheksdatei angegeben."
#define MSGTR_AF_LADSPA_ErrNoLabel "Keine Filterlabel angegeben."
#define MSGTR_AF_LADSPA_ErrNotEnoughControls "Nicht gen�gend Bedienelemente in der Kommandozeile angegeben."
#define MSGTR_AF_LADSPA_ErrControlBelow "%s: Input-Kontrolle #%d ist unterhalb der Grenze von %0.4f.\n"
#define MSGTR_AF_LADSPA_ErrControlAbove "%s: Input-Kontrolle #%d ist �berhalb der Grenze von %0.4f.\n"

// ========================== INPUT =========================================

// joystick.c

#define MSGTR_INPUT_JOYSTICK_Opening "�ffne Joystick-Ger�tedatei '%s'.\n"
#define MSGTR_INPUT_JOYSTICK_CantOpen "Konnte Joystick-Ger�tedatei '%s' nicht �ffnen: %s\n"
#define MSGTR_INPUT_JOYSTICK_ErrReading "Fehler beim Lesen von Joystick-Ger�tedatei: %s\n"
#define MSGTR_INPUT_JOYSTICK_LoosingBytes "Joystick: %d Byte Daten verloren.\n"
#define MSGTR_INPUT_JOYSTICK_WarnLostSync "Joystick: Warnung: Initialisierungsereignis, Synchronisierung mit Treiber verloren.\n"
#define MSGTR_INPUT_JOYSTICK_WarnUnknownEvent "Joystick: Warnung: Unbekannter Ereignistyp %d.\n"

// input.c

#define MSGTR_INPUT_INPUT_ErrCantRegister2ManyCmdFds "Zu viele Kommandodateideskriptoren, konnte Deskriptor %d nicht registrieren.\n"
#define MSGTR_INPUT_INPUT_ErrCantRegister2ManyKeyFds "Zu viele Tastendateidesktriptoren, konnte Deskriptor %d nicht registrieren.\n"
#define MSGTR_INPUT_INPUT_ErrArgMustBeInt "Kommando %s: Argument %d ist keine Integerzahl.\n"
#define MSGTR_INPUT_INPUT_ErrArgMustBeFloat "Kommando %s: Argument %d ist keine Flie�kommazahl.\n"
#define MSGTR_INPUT_INPUT_ErrUnterminatedArg "Kommando %s: Argument %d ist nicht abgeschlossen.\n"
#define MSGTR_INPUT_INPUT_ErrUnknownArg "Unbekanntes Argument %d.\n"
#define MSGTR_INPUT_INPUT_Err2FewArgs "Kommando %s ben�tigt mindestens %d Argumente, es wurden aber nur %d gefunden.\n"
#define MSGTR_INPUT_INPUT_ErrReadingCmdFd "Fehler beim Lesen von Kommandodateideskriptor %d: %s\n"
#define MSGTR_INPUT_INPUT_ErrCmdBufferFullDroppingContent "Kommandopuffer von Dateideskriptor %d ist voll: Verwerfe Inhalt.\n"
#define MSGTR_INPUT_INPUT_ErrInvalidCommandForKey "Ung�ltiges Kommando f�r gebundene Taste '%s'."
#define MSGTR_INPUT_INPUT_ErrSelect "Select gab Fehler zur�ck: %s\n"
#define MSGTR_INPUT_INPUT_ErrOnKeyInFd "Fehler beim Lesen von Tasteneingabedateideskriptor %d.\n"
#define MSGTR_INPUT_INPUT_ErrDeadKeyOnFd "Fataler Fehler beim Lesen von Tasteneingabedateideskriptor %d.\n"
#define MSGTR_INPUT_INPUT_Err2ManyKeyDowns "Zu viele gleichzeitige Tastendruckereignisse.\n"
#define MSGTR_INPUT_INPUT_ErrOnCmdFd "Fehler beim Lesen von Kommandodateideskriptor %d.\n"
#define MSGTR_INPUT_INPUT_ErrReadingInputConfig "Fehler beim Lesen von Eingabekonfigurationsdatei '%s': %s\n"
#define MSGTR_INPUT_INPUT_ErrUnknownKey "Unbekannte Taste '%s'.\n"
#define MSGTR_INPUT_INPUT_ErrUnfinishedBinding "Konnte Bindung '%s' nicht fertigstellen.\n"
#define MSGTR_INPUT_INPUT_ErrBuffer2SmallForKeyName "Der Puffer ist zu klein f�r diesen Tastennamen: %s\n"
#define MSGTR_INPUT_INPUT_ErrNoCmdForKey "Kein Kommando f�r Taste '%s' gefunden."
#define MSGTR_INPUT_INPUT_ErrBuffer2SmallForCmd "Der Puffer ist zu klein f�r Kommando '%s'.\n"
#define MSGTR_INPUT_INPUT_ErrWhyHere "Interner Fehler: What are we doing here?\n"
#define MSGTR_INPUT_INPUT_ErrCantInitJoystick "Konnte Joystick nicht initialisieren.\n"
#define MSGTR_INPUT_INPUT_ErrCantStatFile "Stat auf Datei '%s' fehlgeschlagen: %s\n"
#define MSGTR_INPUT_INPUT_ErrCantOpenFile "Konnte Datei '%s' nicht �ffnen: %s\n"

// ========================== LIBMPDEMUX ===================================

// url.c

#define MSGTR_MPDEMUX_URL_StringAlreadyEscaped "Zeichenkette scheint bereits im URL-Format %c%c1%c2 'escaped' zu sein.\n"

// ai_alsa1x.c

#define MSGTR_MPDEMUX_AIALSA1X_CannotSetSamplerate "Konnte Samplingrate nicht setzen.\n"
#define MSGTR_MPDEMUX_AIALSA1X_CannotSetBufferTime "Konnte Pufferzeit nicht setzen.\n"
#define MSGTR_MPDEMUX_AIALSA1X_CannotSetPeriodTime "Konnte Periode nicht setzen.\n"

// ai_alsa.c

#define MSGTR_MPDEMUX_AIALSA_PcmBrokenConfig "Kaputte Konfiguration f�r diesen PCM-Kanal: Keine Konfiguration verf�gbar.\n"
#define MSGTR_MPDEMUX_AIALSA_UnavailableAccessType "Zugriffstyp nicht verf�gbar.\n"
#define MSGTR_MPDEMUX_AIALSA_UnavailableSampleFmt "Sampleformat nicht verf�gbar.\n"
#define MSGTR_MPDEMUX_AIALSA_UnavailableChanCount "Kanalzahl nicht verf�gbar - falle auf Standard zur�ck: %d\n"
#define MSGTR_MPDEMUX_AIALSA_CannotInstallHWParams "Konnte Hardwareparameter nicht einrichten:"
#define MSGTR_MPDEMUX_AIALSA_PeriodEqualsBufferSize "Periode darf nicht gleich der Puffergr��e sein (%u == %lu).\n"
#define MSGTR_MPDEMUX_AIALSA_CannotInstallSWParams "Konnte Softwareparameter nicht einrichten:\n"
#define MSGTR_MPDEMUX_AIALSA_ErrorOpeningAudio "Konnte Ton nicht �ffnen: %s\n"
#define MSGTR_MPDEMUX_AIALSA_AlsaStatusError "ALSA-Statusfehler: %s"
#define MSGTR_MPDEMUX_AIALSA_AlsaXRUN "ALSA xrun!!! (mindestens %.3f ms lang)\n"
#define MSGTR_MPDEMUX_AIALSA_AlsaStatus "ALSA-Status:\n"
#define MSGTR_MPDEMUX_AIALSA_AlsaXRUNPrepareError "ALSA xrun: Fehler bei Vorbereitung: %s"
#define MSGTR_MPDEMUX_AIALSA_AlsaReadWriteError "ALSA-Ein/Ausgabefehler."

// ai_oss.c

#define MSGTR_MPDEMUX_AIOSS_Unable2SetChanCount "Konnte Kanalzahl nicht setzen: %d\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2SetStereo "Konnte Stereo nicht setzen: %d\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2Open "Konnte '%s' nicht �ffnen: %s\n"
#define MSGTR_MPDEMUX_AIOSS_UnsupportedFmt "Format ist nicht unterst�tzt.\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2SetAudioFmt "Konnte Tonformat nicht setzen."
#define MSGTR_MPDEMUX_AIOSS_Unable2SetSamplerate "Konnte Samplerate nicht setzen: %d\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2SetTrigger "Konnte Trigger nicht setzen: %d\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2GetBlockSize "Konnte Blockgr��e nicht ermitteln!\n"
#define MSGTR_MPDEMUX_AIOSS_AudioBlockSizeZero "Blockgr��e des Tons ist null, setze auf %d!\n"
#define MSGTR_MPDEMUX_AIOSS_AudioBlockSize2Low "Blockgr��e des Tons ist zu klein, setze auf %d!\n"

// asfheader.c

#define MSGTR_MPDEMUX_ASFHDR_HeaderSizeOver1MB "FATAL: Dateikopf gr��er als 1 MB (%d)!\nSetze dich mit den MPlayer-Autoren in Verbindung und sende oder lade diese Datei hoch.\n"
#define MSGTR_MPDEMUX_ASFHDR_HeaderMallocFailed "%d Byte konnten nicht f�r den Kopf reserviert werden.\n"
#define MSGTR_MPDEMUX_ASFHDR_EOFWhileReadingHeader "Dateiende beim lesen des Kopfes, kaputte/unvollst�ndige Datei?\n"
#define MSGTR_MPDEMUX_ASFHDR_DVRWantsLibavformat "DVR funktioniert vermutlich nur mit libavformat, versuche -demuxer 35, falls du Probleme hast.\n"
#define MSGTR_MPDEMUX_ASFHDR_NoDataChunkAfterHeader "Auf den Kopf folgt kein Datenblock!\n"
#define MSGTR_MPDEMUX_ASFHDR_AudioVideoHeaderNotFound "ASF: Kein Ton/Bild-Kopf gefunden - kaputte Datei?\n"
#define MSGTR_MPDEMUX_ASFHDR_InvalidLengthInASFHeader "Ung�ltige L�ngenangabe im ASF-Kopf!\n"

// asf_mmst_streaming.c

#define MSGTR_MPDEMUX_MMST_WriteError "Schreibfehler.\n"
#define MSGTR_MPDEMUX_MMST_EOFAlert "\nAchtung! Dateiende.\n"
#define MSGTR_MPDEMUX_MMST_PreHeaderReadFailed "Konnte 'pre-header' nicht lesen.\n"
#define MSGTR_MPDEMUX_MMST_InvalidHeaderSize "Ung�ltige Kopfgr��e, gebe auf.\n"
#define MSGTR_MPDEMUX_MMST_HeaderDataReadFailed "Konnte Kopfdaten nicht lesen.\n"
#define MSGTR_MPDEMUX_MMST_packet_lenReadFailed "Konnte 'packet_len' nicht lesen.\n"
#define MSGTR_MPDEMUX_MMST_InvalidRTSPPacketSize "Ung�ltige RTSP-Paketgr��e, gebe auf.\n"
#define MSGTR_MPDEMUX_MMST_CmdDataReadFailed "Fehler beim Lesen der Kommandodaten.\n"
#define MSGTR_MPDEMUX_MMST_HeaderObject "Kopfobjekt.\n"
#define MSGTR_MPDEMUX_MMST_DataObject "Datenobjekt.\n"
#define MSGTR_MPDEMUX_MMST_FileObjectPacketLen "Dateiobjekt, Paketgr��e = %d (%d).\n"
#define MSGTR_MPDEMUX_MMST_StreamObjectStreamID "Datenstromobjekt, Identifikationsnummer: %d\n"
#define MSGTR_MPDEMUX_MMST_2ManyStreamID "Zu viele Identifikationsnummern, Datenstrom �bersprungen."
#define MSGTR_MPDEMUX_MMST_UnknownObject "Unbekannter Objekttyp.\n"
#define MSGTR_MPDEMUX_MMST_MediaDataReadFailed "Konnte Mediendaten nicht lesen.\n"
#define MSGTR_MPDEMUX_MMST_MissingSignature "Fehlende Signatur.\n"
#define MSGTR_MPDEMUX_MMST_PatentedTechnologyJoke "Alles fertig. Vielen dank, da� Du eine proriet�re und patentierte Technologie beinhaltende Mediendatei heruntergeladen hast.\n"
#define MSGTR_MPDEMUX_MMST_UnknownCmd "Unbekanntes Kommando %02x\n"
#define MSGTR_MPDEMUX_MMST_GetMediaPacketErr "get_media_packet lieferte Fehler zur�ck: %s\n"
#define MSGTR_MPDEMUX_MMST_Connected "Verbunden.\n"

// asf_streaming.c

#define MSGTR_MPDEMUX_ASF_StreamChunkSize2Small "Ahhhh, stream_chunk-Gr��e ist zu klein: %d\n"
#define MSGTR_MPDEMUX_ASF_SizeConfirmMismatch "size_confirm passt nicht!: %d %d\n"
#define MSGTR_MPDEMUX_ASF_WarnDropHeader "Warnung: 'header' verloren ????\n"
#define MSGTR_MPDEMUX_ASF_ErrorParsingChunkHeader "Fehler beim Parsen des Blockkopfes.\n"
#define MSGTR_MPDEMUX_ASF_NoHeaderAtFirstChunk "Habe keinen 'header' als ersten Block bekommen!!!!\n"
#define MSGTR_MPDEMUX_ASF_BufferMallocFailed "Konnte %d Byte Puffer nicht reservieren.\n"
#define MSGTR_MPDEMUX_ASF_ErrReadingNetworkStream "Fehler beim Lesen des Datenstroms �ber das Netzwerk.\n"
#define MSGTR_MPDEMUX_ASF_ErrChunk2Small "Fehler: Block ist zu klein.\n"
#define MSGTR_MPDEMUX_ASF_ErrSubChunkNumberInvalid "Fehler: Unterblocknummer ist ung�ltig.\n"
#define MSGTR_MPDEMUX_ASF_Bandwidth2SmallCannotPlay "Bandbreite reicht nicht aus, kann Datei nicht abspielen!\n"
#define MSGTR_MPDEMUX_ASF_Bandwidth2SmallDeselectedAudio "Bandbreite reicht nicht aus, Tonspur deaktiviert.\n"
#define MSGTR_MPDEMUX_ASF_Bandwidth2SmallDeselectedVideo "Bandbreite reicht nicht aus, Videospur deaktiviert.\n"
#define MSGTR_MPDEMUX_ASF_InvalidLenInHeader "Ung�ltige L�ngenangabe im ASF-Kopf!\n"
#define MSGTR_MPDEMUX_ASF_ErrReadingChunkHeader "Fehler beim Lesen des Blockkopfes.\n"
#define MSGTR_MPDEMUX_ASF_ErrChunkBiggerThanPacket "Fehler: Blockgr��e > Paketgr��e.\n"
#define MSGTR_MPDEMUX_ASF_ErrReadingChunk "Fehler beim Lesen des Blocks.\n"
#define MSGTR_MPDEMUX_ASF_ASFRedirector "=====> ASF Redirector\n"
#define MSGTR_MPDEMUX_ASF_InvalidProxyURL "Ung�ltige Proxy-URL.\n"
#define MSGTR_MPDEMUX_ASF_UnknownASFStreamType "Unbekannter ASF-Datenstromtyp.\n"
#define MSGTR_MPDEMUX_ASF_Failed2ParseHTTPResponse "Konnte HTTP-Antworte nicht parsen.\n"
#define MSGTR_MPDEMUX_ASF_ServerReturn "Server liefert %d: %s\n"
#define MSGTR_MPDEMUX_ASF_ASFHTTPParseWarnCuttedPragma "ASF-HTTP-Parser Warnung: Pragma '%s' von %d auf %d Byte abgeschnitten.\n"
#define MSGTR_MPDEMUX_ASF_SocketWriteError "Fehler beim Schreiben in Socket: %s\n"
#define MSGTR_MPDEMUX_ASF_HeaderParseFailed "Konnte Kopf nicht parsen.\n"
#define MSGTR_MPDEMUX_ASF_NoStreamFound "Kein Datenstrom gefunden.\n"
#define MSGTR_MPDEMUX_ASF_UnknownASFStreamingType "Unbekannte ASF-'streaming'-Variante.\n"
#define MSGTR_MPDEMUX_ASF_InfoStreamASFURL "STREAM_ASF, URL: %s\n"
#define MSGTR_MPDEMUX_ASF_StreamingFailed "Fehlgeschlagen, breche ab.\n"

// audio_in.c

#define MSGTR_MPDEMUX_AUDIOIN_ErrReadingAudio "\nFehler beim Lesen des Tons: %s\n"
#define MSGTR_MPDEMUX_AUDIOIN_XRUNSomeFramesMayBeLeftOut "Es gab einen 'cross-run', einige 'Frames' k�nnten fehlen!\n"
#define MSGTR_MPDEMUX_AUDIOIN_ErrFatalCannotRecover "Fataler Fehler, Rettung nicht m�glich!\n"
#define MSGTR_MPDEMUX_AUDIOIN_NotEnoughSamples "\nNicht genug Tonsamples!\n"

// aviheader.c

#define MSGTR_MPDEMUX_AVIHDR_EmptyList "** leere Liste?!\n"
#define MSGTR_MPDEMUX_AVIHDR_FoundMovieAt "Film von 0x%X - 0x%X gefunden.\n"
#define MSGTR_MPDEMUX_AVIHDR_FoundBitmapInfoHeader "'bih' gefunden, %u Byte von %d.\n"
#define MSGTR_MPDEMUX_AVIHDR_RegeneratingKeyfTableForMPG4V1 "Erstelle Keyframe-Tabelle f�r MS-mpg4v1-Video neu...\n"
#define MSGTR_MPDEMUX_AVIHDR_RegeneratingKeyfTableForDIVX3 "Erstelle Keyframe-Tabelle f�r DIVX3-Video neu...\n"
#define MSGTR_MPDEMUX_AVIHDR_RegeneratingKeyfTableForMPEG4 "Erstelle Keyframe-Tabelle f�r MPEG4-Video neu...\n"
#define MSGTR_MPDEMUX_AVIHDR_FoundWaveFmt "'wf' gefunden, %d Byte von %d.\n"
#define MSGTR_MPDEMUX_AVIHDR_FoundAVIV2Header "AVI: dmlh gefunden (size=%d) (total_frames=%d).\n"
#define MSGTR_MPDEMUX_AVIHDR_ReadingIndexBlockChunksForFrames  "Lese INDEX-Block, %d Bl�cke f�r %ld Frames (fpos=%p).\n"
#define MSGTR_MPDEMUX_AVIHDR_AdditionalRIFFHdr "zus�tzlicher RIFF-Kopf...\n"
#define MSGTR_MPDEMUX_AVIHDR_WarnNotExtendedAVIHdr "** Warnung: Dies ist kein erweiterter AVI-Kopf...\n"
#define MSGTR_MPDEMUX_AVIHDR_BrokenChunk "Kaputter Block?  Blockgr��e=%d  (id=%.4s)\n"
#define MSGTR_MPDEMUX_AVIHDR_BuildingODMLidx "AVI: ODML: Erstelle ODML-Index (%d Superindexbl�cke).\n"
#define MSGTR_MPDEMUX_AVIHDR_BrokenODMLfile "AVI: ODML: Kaputte (unfertige?) Datei erkannt.  Benutze den herk�mmlichen Index.\n"
#define MSGTR_MPDEMUX_AVIHDR_CantReadIdxFile "Konnte Index-Datei '%s' nicht lesen: %s\n"
#define MSGTR_MPDEMUX_AVIHDR_NotValidMPidxFile "'%s' ist keine g�ltige MPlayer-Index-Datei.\n"
#define MSGTR_MPDEMUX_AVIHDR_FailedMallocForIdxFile "Konnte Speicher f�r Indexdaten von '%s' nicht reservieren.\n"
#define MSGTR_MPDEMUX_AVIHDR_PrematureEOF "Vorzeitiges Ende der Indexdatei '%s'.\n"
#define MSGTR_MPDEMUX_AVIHDR_IdxFileLoaded "Indexdatei '%s' geladen.\n"
#define MSGTR_MPDEMUX_AVIHDR_GeneratingIdx "Erzeuge Index: %3lu %s     \r"
#define MSGTR_MPDEMUX_AVIHDR_IdxGeneratedForHowManyChunks "AVI: Erzeuge Indextabelle f�r %d Bl�cke!\n"
#define MSGTR_MPDEMUX_AVIHDR_Failed2WriteIdxFile "Konnte Indexdatei '%s' nicht schreiben: %s\n"
#define MSGTR_MPDEMUX_AVIHDR_IdxFileSaved "Indexdatei '%s' gespeichert.\n"

// cache2.c

#define MSGTR_MPDEMUX_CACHE2_NonCacheableStream "\rDieser Datenstrom l�sst sich nicht zwischenspeichern.\n"
#define MSGTR_MPDEMUX_CACHE2_ReadFileposDiffers "!!!Unterschied in read_filepos!!! Bitte Melde diesen Fehler...\n"

// cdda.c

#define MSGTR_MPDEMUX_CDDA_CantOpenCDDADevice "Kann CDDA-Ger�t nicht �ffnen.\n"
#define MSGTR_MPDEMUX_CDDA_CantOpenDisc "Kann CD nicht �ffnen.\n"
#define MSGTR_MPDEMUX_CDDA_AudioCDFoundWithNTracks "Musik-CD mit %d Titeln gefunden.\n"

// cddb.c

#define MSGTR_MPDEMUX_CDDB_FailedToReadTOC "Konnte Inhaltsverzeichnis der CD nicht lesen.\n"
#define MSGTR_MPDEMUX_CDDB_FailedToOpenDevice "Konnte Ger�t '%s' nicht �ffnen.\n"
#define MSGTR_MPDEMUX_CDDB_NotAValidURL "Ung�ltige URL.\n"
#define MSGTR_MPDEMUX_CDDB_FailedToSendHTTPRequest "Konnte die HTTP-Anfrage nicht senden.\n"
#define MSGTR_MPDEMUX_CDDB_FailedToReadHTTPResponse "Konnte die HTTP-Antwort nicht lesen.\n"
#define MSGTR_MPDEMUX_CDDB_HTTPErrorNOTFOUND "Nicht gefunden.\n"
#define MSGTR_MPDEMUX_CDDB_HTTPErrorUnknown "Unbekannter Fehlercode.\n"
#define MSGTR_MPDEMUX_CDDB_NoCacheFound "Kein Cache gefunden.\n"
#define MSGTR_MPDEMUX_CDDB_NotAllXMCDFileHasBeenRead "Konnte die XMCD-Datei nicht komplett lesen.\n"
#define MSGTR_MPDEMUX_CDDB_FailedToCreateDirectory "Konnte Verzeichnis '%s' nicht erstellen.\n"
#define MSGTR_MPDEMUX_CDDB_NotAllXMCDFileHasBeenWritten "Konnte die XMCD-Datei nicht komplett schreiben.\n"
#define MSGTR_MPDEMUX_CDDB_InvalidXMCDDatabaseReturned "XMCD-Datenbankdatei ist ung�ltig.\n"
#define MSGTR_MPDEMUX_CDDB_UnexpectedFIXME "Unerwarteter Fehler.\n"
#define MSGTR_MPDEMUX_CDDB_UnhandledCode "Unbehandelter Statuscode.\n"
#define MSGTR_MPDEMUX_CDDB_UnableToFindEOL "Konnte Zeilenendemarkierung nicht finden.\n"
#define MSGTR_MPDEMUX_CDDB_ParseOKFoundAlbumTitle "Album '%s' gefunden.\n"
#define MSGTR_MPDEMUX_CDDB_AlbumNotFound "Album nicht gefunden.\n"
#define MSGTR_MPDEMUX_CDDB_ServerReturnsCommandSyntaxErr "Fehlercode des Servers: Kommandosyntaxfehler.\n"
#define MSGTR_MPDEMUX_CDDB_NoSitesInfoAvailable "Liste der CDDB-Seiten nicht verf�gbar.\n"
#define MSGTR_MPDEMUX_CDDB_FailedToGetProtocolLevel "Konnte aktuelle Protokollebene nicht ermitteln.\n"
#define MSGTR_MPDEMUX_CDDB_NoCDInDrive "Keine CD im Laufwerk.\n"

// cue_read.c

#define MSGTR_MPDEMUX_CUEREAD_UnexpectedCuefileLine "[bincue] Unerwartete Zeile in der Cue-Datei: %s\n"
#define MSGTR_MPDEMUX_CUEREAD_BinFilenameTested "[bincue] Zugeh�rige Bin-Datei unter dem Namen '%s' nicht gefunden.\n"
#define MSGTR_MPDEMUX_CUEREAD_CannotFindBinFile "[bincue] Konnte Bin-Datei nicht finden. Gebe auf.\n"
#define MSGTR_MPDEMUX_CUEREAD_UsingBinFile "[bincue] Benutze Bin-Datei '%s'.\n"
#define MSGTR_MPDEMUX_CUEREAD_UnknownModeForBinfile "[bincue] Unbekannter Typ der Bin-Datei. Dies sollte nicht auftreten, breche ab.\n"
#define MSGTR_MPDEMUX_CUEREAD_CannotOpenCueFile "[bincue] Kann Cue-Datei '%s' nicht �ffnen.\n"
#define MSGTR_MPDEMUX_CUEREAD_ErrReadingFromCueFile "[bincue] Fehler beim Lesen von '%s'.\n"
#define MSGTR_MPDEMUX_CUEREAD_ErrGettingBinFileSize "[bincue] Konnte Gr��e der Bin-Datei nicht ermitteln.\n"
#define MSGTR_MPDEMUX_CUEREAD_InfoTrackFormat "Titel %02d: Format=%d  %02d:%02d:%02d\n"
#define MSGTR_MPDEMUX_CUEREAD_UnexpectedBinFileEOF "[bincue] Unerwartetes Ende der Bin-Datei.\n"
#define MSGTR_MPDEMUX_CUEREAD_CannotReadNBytesOfPayload "[bincue] Konnte Block der Gr��e %dByte nicht lesen.\n"
#define MSGTR_MPDEMUX_CUEREAD_CueStreamInfo_FilenameTrackTracksavail "CUE stream_open: Dateiname='%s', Titel=%d, verf�gbare Titel: %d -> %d\n"

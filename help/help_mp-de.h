// Translated by: Johannes Feigl <johannes.feigl@aon.at>
// Reworked by Klaus Umbach <klaus.umbach@gmx.net>
// Moritz Bunkus <moritz@bunkus.org>
// Alexander Strasser <eclipse7@gmx.net>
// Sebastian Kr�mer <mail@skraemer.de>

// In synch with rev 1.123

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
" * * * SIEHE MANPAGE F�R DETAILS, WEITERE OPTIONEN UND TASTEN* * *\n"
"\n";
#endif

// ========================= MPlayer Ausgaben ===========================

// mplayer.c: 
#define MSGTR_Exiting "\nBeenden...\n"
#define MSGTR_ExitingHow "\nBeenden... (%s)\n"
#define MSGTR_Exit_quit "Ende"
#define MSGTR_Exit_eof "Dateiende erreicht."
#define MSGTR_Exit_error "Fataler Fehler"
#define MSGTR_IntBySignal "\nMPlayer wurde durch Signal %d im Modul %s unterbrochen\n"
#define MSGTR_NoHomeDir "Kann Homeverzeichnis nicht finden.\n"
#define MSGTR_GetpathProblem "get_path(\"config\")-Problem\n"
#define MSGTR_CreatingCfgFile "Erstelle Konfigurationsdatei: %s\n"
#define MSGTR_InvalidVOdriver "Ung�ltiger Videoausgabetreibername: %s\nBenutze '-vo help' f�r eine Liste der Videotreiber.\n"
#define MSGTR_InvalidAOdriver "Ung�ltiger Audioausgabetreibername: %s\nBenutze '-vo help' f�r eine Liste der verf�gbaren Audiotreiber.\n"
#define MSGTR_CopyCodecsConf "(Kopiere/verlinke etc/codecs.conf aus dem MPlayer-Quelltext nach ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "Benutze eingebaute Standardwerte f�r codecs.conf.\n"
#define MSGTR_CantLoadFont "Kann Schriftdatei nicht laden: %s\n"
#define MSGTR_CantLoadSub "Kann Untertitel nicht laden: %s\n"
#define MSGTR_ErrorDVDkey "Fehler beim Verarbeiten des DVD-Schl�ssels..\n"
#define MSGTR_CmdlineDVDkey "Der gew�nschte DVD-Schl�ssel wird f�r das Entschl�sseln benutzt.\n"
#define MSGTR_DVDauthOk "DVD Authentifizierungsvorgang scheint OK zu sein.\n"
#define MSGTR_DumpSelectedStreamMissing "dump: FATAL: Ausgew�hlter Stream fehlt!\n"
#define MSGTR_CantOpenDumpfile "Kann dump-Datei nicht �ffnen!\n"
#define MSGTR_CoreDumped "Core dumped ;)\n"
#define MSGTR_FPSnotspecified "FPS ist im Header nicht angegeben (oder ung�ltig)! Benutze die -fps Option!\n"
#define MSGTR_TryForceAudioFmtStr "Versuche Audiocodecfamilie %s zu erzwingen...\n"
#define MSGTR_CantFindAfmtFallback "Gew�nschte Codecfamilie nicht gefunden, greife auf andere zur�ck.\n"
#define MSGTR_CantFindAudioCodec "Kann Codec f�r Audioformat 0x%X nicht finden!\n"
#define MSGTR_RTFMCodecs "Lies DOCS/de/codecs.html!\n"
#define MSGTR_CouldntInitAudioCodec "Kann Audiocodec nicht initialisieren! -> kein Ton\n"
#define MSGTR_TryForceVideoFmtStr "Versuche Videocodecfamilie %s zu erzwingen...\n"
#define MSGTR_CantFindVideoCodec "Kann keinen Codec finden, der  zur gew�hlten -vo-Option und Videoformat 0x%X passt!\n"
#define MSGTR_VOincompCodec "Der ausgew�hlte Videoausgabetreiber ist nicht kompatibel mit diesem Codec.\n"
#define MSGTR_CannotInitVO "FATAL: Kann Videoausgabetreiber nicht initialisieren!\n"
#define MSGTR_CannotInitAO "Kann Audiotreiber/Soundkarte nicht �ffnen/initialisieren -> kein Ton\n"
#define MSGTR_StartPlaying "Starte Wiedergabe...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"         ***************************************************\n"\
"         **** Dein System ist zu LANGSAM zum Abspielen! ****\n"\
"         ***************************************************\n"\
"M�gliche Gr�nde, Probleme, Workarounds: \n"\
"- H�ufigste Ursache: defekter/fehlerhafter _Audio_treiber.\n"\
"  - Versuche -ao sdl, verwende ALSA 0.5 oder die OSS Emulation von ALSA 0.9.\n"\
"  - Experimentiere mit verschiedenen Werten f�r -autosync, 30 ist ein guter Startwert."\
"- Langsame Videoausgabe\n"\
"  - Versuche einen anderen -vo Treiber (-vo help f�r eine Liste)\n"\
"    oder probiere -framedrop!\n"\
"- Langsame CPU\n"\
"  - Versuche nicht, DVDs/gro�e DivX-Filme auf langsamen CPUs abzuspielen.\n"\
"    Probiere -hardframedrop.\n"\
"- Defekte Datei\n"\
"  - Versuche verschiedene Kombinationen von: -nobps -ni -forceidx -mc 0.\n"\
"- F�r die Wiedergabe von langsamen Medien (NFS/SMB, DVD, VCD usw)\n"\
"  - Versuche -cache 8192.\n"\
"- Benutzt du -cache zusammen mit einer nicht-interleavten AVI-Datei?\n"\
"  - Versuche -nocache.\n"\
"Lies DOCS/de/video.html und DOCS/de/sound.html; dort stehen \n"\
"Tipps und Kniffe f�r optimale Einstellungen. \n"\
"(Schau evtl. auch bei den entsprechenden englischen Seiten.)\n"\
"Wenn dies nicht hilft, lies DOCS/de/bugreports.html!\n\n"

#define MSGTR_NoGui "MPlayer wurde OHNE GUI-Unterst�tzung kompiliert.\n"
#define MSGTR_GuiNeedsX "MPlayer GUI erfordert X11.\n"
#define MSGTR_Playing "Spiele %s\n"
#define MSGTR_NoSound "Audio: kein Ton!\n"
#define MSGTR_FPSforced "FPS von %5.3f erzwungen (ftime: %5.3f)\n"
#define MSGTR_CompiledWithRuntimeDetection "MPlayer mit CPU-Erkennung zur Laufzeit kompiliert - WARNUNG, das ist nicht optimal!\nKompiliere MPlayer mit --disable-runtime-cpudetection f�r beste Performance.\n"
#define MSGTR_CompiledWithCPUExtensions "Kompiliert f�r x86 CPU mit folgenden Erweiterungen:"
#define MSGTR_AvailableVideoOutputPlugins "Verf�gbare Videoausgabeplugins:\n"
#define MSGTR_AvailableVideoOutputDrivers "Verf�gbare Videoausgabetreiber:\n"
#define MSGTR_AvailableAudioOutputDrivers "Verf�gbare Audioausgabetreiber:\n"
#define MSGTR_AvailableAudioCodecs "Verf�gbare Audiocodecs:\n"
#define MSGTR_AvailableVideoCodecs "Verf�gbare Videocodecs:\n"
#define MSGTR_AvailableAudioFm "\nVerf�gbare (in das Binary kompilierte) Audio Codec Familien:\n"
#define MSGTR_AvailableVideoFm "\nVerf�gbare (in das Binary kompilierte) Video Codec Familien:\n"
#define MSGTR_AvailableFsType "Verf�gbare Vollbildschirm-Modi:\n"
#define MSGTR_UsingRTCTiming "Verwende Linux Hardware RTC-Timing (%ldHz).\n"
#define MSGTR_CannotReadVideoProperties "Video: Kann Eigenschaften nicht lesen.\n"
#define MSGTR_NoStreamFound "Keine Streams gefunden.\n"
#define MSGTR_InitializingAudioCodec "Initialisiere Audiocodec...\n"
#define MSGTR_ErrorInitializingVODevice "Fehler beim �ffnen/Initialisieren des ausgew�hlten Videoausgabetreibers (-vo).\n"
#define MSGTR_ForcedVideoCodec "Erzwungener Videocodec: %s\n"
#define MSGTR_ForcedAudioCodec "Erzwungener Audiocodec: %s\n"
#define MSGTR_AODescription_AOAuthor "AO: Beschreibung: %s\nAO: Autor: %s\n"
#define MSGTR_AOComment "AO: Kommentar: %s\n"
#define MSGTR_Video_NoVideo "Video: kein Video\n"
#define MSGTR_NotInitializeVOPorVO "\nFATAL: Konnte Videofilter (-vf) oder -ausgabetreiber (-vo) nicht initialisieren.\n"
#define MSGTR_Paused "\n  =====  PAUSE  =====\r"
#define MSGTR_PlaylistLoadUnable "\nKann Playlist %s nicht laden.\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- MPlayer st�rzte aufgrund einer 'ung�ltigen Anweisung' ab.\n"\
"  Es kann sich um einen Fehler im unserem neuen Code f�r\n"\
"  die CPU-Erkennung zur Laufzeit handeln...\n"\
"  Bitte lies DOCS/de/bugreports.html.\n"
#define MSGTR_Exit_SIGILL \
"- MPlayer st�rzte aufgrund einer 'ung�ltigen Anweisung' ab.\n"\
"  Das passiert normalerweise, wenn du MPlayer auf einer anderen CPU\n"\
"  ausf�hrst als auf der, f�r die er kompiliert/optimiert wurde.\n"\
"  �berpr�fe das!\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- MPlayer st�rzte wegen falscher Benutzung der CPU/FPU/des RAMs ab.\n"\
"  Kompiliere MPlayer erneut mit --enable-debug und erstelle mit 'gdb'\n"\
"  einen Backtrace und eine Disassemblierung. Details dazu findest du\n"\
"  in DOCS/de/bugreports.html.\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer ist abgest�rzt. Das sollte nicht passieren.\n"\
"  Es kann sich um einen Fehler im MPlayer-Code _oder_ in deinen Treibern\n"\
"  _oder_ in deinem gcc handeln. Wenn du meinst, es sei MPlayers Fehler, dann\n"\
"  lies DOCS/de/bugreports.html und folge den dortigen Anweisungen.\n"\
"  Wir k�nnen und werden dir nicht helfen, wenn du nicht alle dort aufgef�hrten\n"\
"  Informationen zur Verf�gung stellst.\n"

// mencoder.c:

#define MSGTR_UsingPass3ControllFile "Verwende Pass 3 Kontrolldatei: %s\n"
#define MSGTR_MissingFilename "\nDateiname nicht angegeben.\n\n"
#define MSGTR_CannotOpenFile_Device "Kann Datei/Ger�t nicht �ffnen\n"
#define MSGTR_ErrorDVDAuth "Fehler bei der DVD-Authentifizierung.\n"
#define MSGTR_CannotOpenDemuxer "Kann Demuxer nicht �ffnen.\n"
#define MSGTR_NoAudioEncoderSelected "\nKein Audioencoder (-oac)  ausgew�hlt. \nW�hle einen aus (siehe -oac help) oder verwende  -nosound.\n"
#define MSGTR_NoVideoEncoderSelected "\nKein Videoencoder (-ovc) ausgew�hlt. \nW�hle einen aus (siehe -ovc help).\n"
#define MSGTR_CannotOpenOutputFile "Kann Ausgabedatei '%s' nicht �ffnen.\n"
#define MSGTR_EncoderOpenFailed "�ffnen des Encoders fehlgeschlagen.\n"
#define MSGTR_ForcingOutputFourcc "Erzwinge Output-Fourcc %x  [%.4s].\n"
#define MSGTR_WritingAVIHeader "Schreibe AVI-Header...\n"
#define MSGTR_DuplicateFrames "\n%d doppelte(r) Frame(s)!\n"
#define MSGTR_SkipFrame "\nFrame �bersprungen!\n"
#define MSGTR_ErrorWritingFile "%s: Fehler beim Schreiben der Datei.\n"
#define MSGTR_WritingAVIIndex "\nSchreibe AVI-Index...\n"
#define MSGTR_FixupAVIHeader "Korrigiere AVI-Header...\n"
#define MSGTR_RecommendedVideoBitrate "Empfohlene Videobitrate f�r %s CD(s): %d\n"
#define MSGTR_VideoStreamResult "\nVideostream: %8.3f kbit/s  (%d bps)  Gr��e: %d Bytes  %5.3f Sek.  %d Frames\n"
#define MSGTR_AudioStreamResult "\nAudiostream: %8.3f kbit/s  (%d bps)  Gr��e: %d Bytes  %5.3f Sek.\n"
	
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
"                 medium: VBR-Enkodierung, gute Qualit�t\n"\
"                 (150-180 kbps Bitratenbereich)\n"\
"                 standard:  VBR-Enkodierung, hohe Qualit�t\n"\
"                 (170-210 kbps Bitratenbereich)\n"\
"                 extreme: VBR-Enkodierung, sehr hohe Qualit�t\n"\
"                 (200-240 kbps Bitratenbereich)\n"\
"                 insane:  CBR-Enkodierung, h�chste Preset-Qualit�t\n"\
"                 (320 kbps Bitrate)\n"\
"                 <8-320>: ABR-Enkodierung mit der angegebenen durchschnittlichen\n"\
"                          Bitrate\n\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "CD-ROM-Laufwerk '%s' nicht gefunden.\n"
#define MSGTR_ErrTrackSelect "Fehler beim Ausw�hlen des VCD Tracks.\n"
#define MSGTR_ReadSTDIN "Lese von Standardeingabe (stdin)...\n"
#define MSGTR_UnableOpenURL "Kann URL nicht �ffnen: %s\n"
#define MSGTR_ConnToServer "Verbunden mit Server: %s\n"
#define MSGTR_FileNotFound "Datei nicht gefunden: '%s'\n"

#define MSGTR_SMBInitError "Kann die Bibliothek libsmbclient nicht �ffnen: %d\n"
#define MSGTR_SMBFileNotFound "Konnte '%s' nicht �ber das Netzwerk �ffnen\n"
#define MSGTR_SMBNotCompiled "MPlayer wurde ohne  SMB-Unterst�tzung  kompiliert.\n"

#define MSGTR_CantOpenDVD "Kann DVD-Laufwerk nicht �ffnen: %s\n"
#define MSGTR_DVDwait "Lese Disk-Struktur, bitte warten...\n"
#define MSGTR_DVDnumTitles "Es sind %d Titel auf dieser DVD.\n"
#define MSGTR_DVDinvalidTitle "Ung�ltige DVD-Titelnummer: %d\n"
#define MSGTR_DVDnumChapters "Es sind %d Kapitel in diesem DVD-Titel.\n"
#define MSGTR_DVDinvalidChapter "Ung�ltige DVD-Kapitelnummer: %d\n"
#define MSGTR_DVDnumAngles "Es sind %d Kameraeinstellungen diesem DVD-Titel.\n"
#define MSGTR_DVDinvalidAngle "Ung�ltige DVD-Kameraeinstellungsnummer %d.\n"
#define MSGTR_DVDnoIFO "Kann die IFO-Datei f�r den DVD-Titel %d nicht �ffnen.\n"
#define MSGTR_DVDnoVOBs "Kann VOB-Dateien des Titels  (VTS_%02d_1.VOB) nicht �ffnen.\n"
#define MSGTR_DVDopenOk "DVD erfolgreich ge�ffnet.\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "Warnung! Audiostream-Header %d neu definiert!\n"
#define MSGTR_VideoStreamRedefined "Warnung! Videostream-Header %d neu definiert!\n"
#define MSGTR_TooManyAudioInBuffer "\nZu viele Audiopakete im Puffer: (%d in %d bytes).\n"
#define MSGTR_TooManyVideoInBuffer "\nZu viele Videopakete im Puffer: (%d in %d bytes).\n"
#define MSGTR_MaybeNI "Vielleicht spielst du eine(n) nicht-interleaved Stream/Datei, oder der Codec funktioniert nicht.\n" \
                      "Versuche bei AVI-Dateien, den nicht-interleaved Modus mit der Option -ni zu erzwingen\n"
#define MSGTR_SwitchToNi "\nSchlecht interleavte AVI-Datei erkannt, wechsele in den -ni Modus!\n"
#define MSGTR_Detected_XXX_FileFormat "%s-Dateiformat erkannt!\n"
#define MSGTR_DetectedAudiofile "Audiodatei erkannt!\n"
#define MSGTR_NotSystemStream "Kein MPEG System Stream... (vielleicht ein Transport Stream?)\n"
#define MSGTR_InvalidMPEGES "Ung�ltiger MPEG-ES Stream??? Kontaktiere den Autor, das k�nnte ein Bug sein :(\n"
#define MSGTR_FormatNotRecognized "========= Sorry, dieses Dateiformat wird nicht erkannt/unterst�tzt ============\n"\
				  "============== Sollte dies ein AVI, ASF oder MPEG Stream sein, ===============\n"\
				  "================== dann kontaktiere bitte den Autor. ========================\n"
#define MSGTR_MissingVideoStream "Kein Videostream gefunden.\n"
#define MSGTR_MissingAudioStream "Kein Audiostream gefunden. -> kein Ton.\n"
#define MSGTR_MissingVideoStreamBug "Fehlender Videostream!? Kontaktiere den Autor, dies k�nnte ein Bug sein :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: Datei enth�lt den gew�hlten Audio- oder Videostream nicht.\n"

#define MSGTR_NI_Forced "Erzwungen"
#define MSGTR_NI_Detected "Erkannt"
#define MSGTR_NI_Message "%s NICHT-INTERLEAVED AVI-Dateiformat.\n"

#define MSGTR_UsingNINI "Verwende defektes NICHT-INTERLEAVED AVI Dateiformat.\n"
#define MSGTR_CouldntDetFNo "Konnte die Anzahl der Frames (f�r absolute Suche) nicht feststellen.\n"
#define MSGTR_CantSeekRawAVI "Suche in reinen AVI-Streams nicht durchf�hrbar. (Index erforderlich, probiere die '-idx'-Option.)\n"
#define MSGTR_CantSeekFile "Kann diese Datei nicht durchsuchen.\n"

#define MSGTR_EncryptedVOB "Verschl�sselte VOB-Datei! Lies DOCS/de/cd-dvd.html\n"

#define MSGTR_MOVcomprhdr "MOV: komprimierte Header ben�tigen ZLIB-Unterst�tzung.\n"
#define MSGTR_MOVvariableFourCC "MOV: Warnung: Variable FOURCC erkannt!?\n"
#define MSGTR_MOVtooManyTrk "MOV: WARNUNG: Zu viele Tracks."
#define MSGTR_FoundAudioStream "==> Audiostream gefunden: %d\n"
#define MSGTR_FoundVideoStream "==> Videostream gefunden: %d\n"
#define MSGTR_DetectedTV "TV erkannt! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "�ffnen des OGG-Demuxers fehlgeschlagen.\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: Suche nach Audiostream (Id:%d)\n"
#define MSGTR_CannotOpenAudioStream "Kann Audiostream nicht �ffnen: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "Kann Untertitelstream nicht �ffnen: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "�ffnen des Audio-Demuxers fehlgeschlagen: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "�ffnen des Untertitel-Demuxers fehlgeschlagen: %s\n"
#define MSGTR_TVInputNotSeekable "TV-Input ist nicht durchsuchbar. (Suche des Kanals?)\n"
#define MSGTR_DemuxerInfoAlreadyPresent "Demuxerinfo %s schon vorhanden.\n"
#define MSGTR_ClipInfo "Clip-Info: \n"

#define MSGTR_LeaveTelecineMode "demux_mpg: 30fps NTSC-Inhalt erkannt, wechsele Framerate.\n"
#define MSGTR_EnterTelecineMode "demux_mpg: 24fps progressiven NTSC-Inhalt erkannt, wechsele Framerate.\n"
				  
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

#define MSGTR_UsingExternalPP "[PP] Verwende externe Postprocessing-Filter, max q = %d\n"
#define MSGTR_UsingCodecPP "[PP] Verwende Postprocessing-Routinen des Codecs, max q = %d\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "Videoeigenschaft '%s' wird von ausgew�hlten vo & vd nicht unterst�tzt.\n"
#define MSGTR_VideoCodecFamilyNotAvailableStr "Erforderliche Videocodec Familie [%s] (vfm=%s) nicht verf�gbar.\nAktiviere sie beim Kompilieren.\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "Erforderliche Audiocodec-Familie [%s] (afm=%s) nicht verf�gbar.\nAktiviere sie beim Kompilieren.\n"
#define MSGTR_OpeningVideoDecoder "�ffne Videodecoder: [%s] %s\n"
#define MSGTR_OpeningAudioDecoder "�ffne Audiodecoder: [%s] %s\n"
#define MSGTR_UninitVideoStr "Deinitialisiere Video: %s  \n"
#define MSGTR_UninitAudioStr "Deinitialisiere Audio: %s  \n"
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
#define MSGTR_CannotFindColorspace "Konnte keinen passenden Farbraum finden, auch nicht mit '-vf scale' :-(\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDec: Codec hat sh->disp_w und sh->disp_h nicht gesetzt!\nVersuche Problem zu umgehen..\n"
#define MSGTR_VoConfigRequest "VDec: VO wird versucht, auf %d x %d (Bevorzugter Farbraum: %s) zu setzen.\n"
#define MSGTR_CouldNotFindColorspace "Konnte keinen passenden Farbraum finden - neuer Versuch mit '-vf scale'..\n"
#define MSGTR_MovieAspectIsSet "Film-Aspekt ist %.2f:1 - Vorskalierung zur Korrektur der Seitenverh�ltnisse.\n"
#define MSGTR_MovieAspectUndefined "Film-Aspekt ist undefiniert - keine Vorskalierung durchgef�hrt.\n"

// vd_dshow.c, vd_dmo.c
#define MSGTR_DownloadCodecPackage "Du mu�t das Bin�rcodec-Paket aktualisieren/installieren.\nGehe dazu auf http://mplayerhq.hu/homepage/dload.html\n"
#define MSGTR_DShowInitOK "INFO: Win32/DShow Videocodec-Initialisierung OK.\n"
#define MSGTR_DMOInitOK "INFO: Win32/DMO Videocodec-Initialisierung OK.\n"

// x11_common.c
#define MSGTR_EwmhFullscreenStateFailed "\nX11: Konnte EWMH-Fullscreen-Event nicht senden!\n"
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
#define MSGTR_PlayList "Playlist"
#define MSGTR_Equalizer "Equalizer"
#define MSGTR_SkinBrowser "Skin-Browser"
#define MSGTR_Network "Netzwerk-Streaming..."
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
#define MSGTR_NEEDLAVCFAME "Sorry, du versuchst, Nicht-MPEG Dateien ohne erneute Enkodierung abzuspielen.\nBitte aktiviere lavc oder fame in der DXR3/H+-Configbox."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[Skin] Fehler in Skin-Konfigurationsdatei in Zeile %d: %s" 
#define MSGTR_SKIN_WARNING1 "[Skin] Warnung in Skin-Konfigurationsdatei in Zeile %d:\nWidget (%s) gefunden, aber davor wurde \"section\" nicht gefunden"
#define MSGTR_SKIN_WARNING2 "[Skin] Warnung in Skin-Konfigurationsdatei in Zeile %d:\nWidget (%s) gefunden, aber davor wurde \"subsection\" nicht gefunden (%s)"
#define MSGTR_SKIN_WARNING3 "[skin] Warnung in Skin-Konfigurationsdatei in Zeile %d:\nDiese Untersektion wird vom Widget nicht unterst�tzt (%s).\n"
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
#define MSGTR_SKINBROWSER_NotEnoughMemory "[skinbrowser] nicht genug Speicher.\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "Skin nicht gefunden (%s).\n"
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
#define MSGTR_MENU_PlayList "Playlist"
#define MSGTR_MENU_SkinBrowser "Skinbrowser"
#define MSGTR_MENU_Preferences "Einstellungen"
#define MSGTR_MENU_Exit "Beenden..."
#define MSGTR_MENU_Mute "Stummschaltung"
#define MSGTR_MENU_Original "Original"
#define MSGTR_MENU_AspectRatio "Seitenverh�ltnis"
#define MSGTR_MENU_AudioTrack "Audiospur"
#define MSGTR_MENU_Track "Spur %d"
#define MSGTR_MENU_VideoTrack "Videospur"

// --- equalizer
#define MSGTR_EQU_Audio "Audio"
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
#define MSGTR_PREFERENCES_Audio "Audio"
#define MSGTR_PREFERENCES_Video "Video"
#define MSGTR_PREFERENCES_SubtitleOSD "Untertitel & OSD"
#define MSGTR_PREFERENCES_Codecs "Codecs & Demuxer"
#define MSGTR_PREFERENCES_Misc "Sonstiges"

#define MSGTR_PREFERENCES_None "Nichts"
#define MSGTR_PREFERENCES_DriverDefault "Treiber-Standardeinstellung"
#define MSGTR_PREFERENCES_AvailableDrivers "Verf�gbare Treiber:"
#define MSGTR_PREFERENCES_DoNotPlaySound "Spiele keinen Ton"
#define MSGTR_PREFERENCES_NormalizeSound "Normalisiere Ton"
#define MSGTR_PREFERENCES_EnEqualizer "Equalizer verwenden"
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
#define MSGTR_PREFERENCES_FRAME_Misc "Sonstiges"
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
#define MSGTR_PREFERENCES_FontEncoding "Kodierung:"
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

#define MSGTR_ABOUT_UHU "GUI-Entwicklung wurde von UHU Linux gesponsert.\n"
#define MSGTR_ABOUT_CoreTeam "   MPlayers Kernentwickler-Team:\n"
#define MSGTR_ABOUT_AdditionalCoders " Weitere Programmierer:\n"
#define MSGTR_ABOUT_MainTesters "     Haupttester:\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "Fataler Fehler!"
#define MSGTR_MSGBOX_LABEL_Error "Fehler!"
#define MSGTR_MSGBOX_LABEL_Warning "Warnung!"

#endif

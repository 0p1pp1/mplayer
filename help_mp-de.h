// Transated by: Johannes Feigl, johannes.feigl@aon.at
// Reworked by Klaus Umbach, klaus.umbach@gmx.net

// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char* banner_text=
"\n\n"
"MPlayer " VERSION "(C) 2000-2002 Arpad Gereoffy (siehe DOCS!)\n"
"\n";

static char help_text[]=
#ifdef HAVE_NEW_GUI
"Verwendung:   mplayer [-gui] [optionen] [url|verzeichnis/]dateiname\n"
#else
"Verwendung:   mplayer [optionen] [url|verzeichnis/]dateiname\n"
#endif
"\n"
"Grundlegende Optionen: (siehe Manpage f�r eine vollst�ndige Liste ALLER Optionen!)\n"
" -vo <drv[:dev]>  Videoausgabetreiber & -Ger�t (siehe '-vo help' f�r eine Liste)\n"
" -ao <drv[:dev]>  Audioausgabetreiber & -Ger�t (siehe '-ao help' f�r eine Liste)\n"
#ifdef HAVE_VCD
" -vcd <tracknr>   Spiele VCD (Video CD) Titel anstelle eines Dateinames\n"
#endif
#ifdef HAVE_LIBCSS
" -dvdauth <dev>   Benutze DVD Ger�t f�r die Authentifizierung (f�r verschl. DVD's)\n"
#endif
#ifdef USE_DVDREAD
" -dvd <titelnr>   Spiele DVD Titel/Track von Ger�t anstelle eines Dateinames\n"
" -alang/-slang    W�hle DVD Audio/Untertitel Sprache (2-Zeichen L�ndercode)\n"
#endif
" -ss <zeitpos>    Starte Abspielen ab Position (Sekunden oder hh:mm:ss)\n"
" -nosound         Spiele keinen Sound\n"
" -fs -vm -zoom    Vollbild Optionen (Vollbild, Videomode, Softwareskalierung)\n"
" -x <x> -y <y>    Setze Bildschirmaufl�sung (f�r Vidmode-Wechsel oder sw-Skalierung)\n"
" -sub <datei>     Benutze Untertitle-Datei (siehe auch -subfps, -subdelay)\n"
" -playlist <datei> Benutze Playlist-Datei\n"
" -vid x -aid y    Spiele Videostream (x) und Audiostream (y)\n"
" -fps x -srate y  Benutze Videoframerate (x fps) und Audiosamplingrate (y Hz)\n"
" -pp <Qualit�t>   Aktiviere Nachbearbeitungsfilter (siehe Manpage f�r Details)\n"
" -framedrop       Benutze frame-dropping (f�r langsame Rechner)\n"
"\n"
"Grundlegende Tasten:\n"
" <- oder ->       Springe zehn Sekunden vor/zur�ck\n"
" rauf / runter    Springe eine Minute vor/zur�ck\n"
" pgup / pgdown    Springe 10 Minuten vor/zur�ck\n"
" < oder >         Springe in der Playliste vor/zur�ck\n"
" p oder LEER      PAUSE (beliebige Taste zum Fortsetzen)\n"
" q oder ESC       Abspielen stoppen und Programm beenden\n"
" + oder -         Audioverz�gerung um +/- 0.1 Sekunde ver�ndern\n"
" o                OSD Mode:  Aus / Suchleiste / Suchleiste + Zeit\n"
" * oder /         PCM-Lautst�rke verstellen\n"
" z oder x         Untertitelverz�gerung um +/- 0.1 Sekunde ver�ndern\n"
" r oder t         Verschiebe die Untertitel-Position, siehe auch -vop expand!\n"
"\n"
" * * * IN DER MANPAGE STEHEN WEITERE KEYS UND OPTIONEN ! * * *\n"
"\n";
#endif

// ========================= MPlayer Ausgaben ===========================

// mplayer.c: 

#define MSGTR_Exiting "\nBeende... (%s)\n"
#define MSGTR_Exit_frames "Angeforderte Anzahl an Frames gespielt"
#define MSGTR_Exit_quit "Ende"
#define MSGTR_Exit_eof "Ende der Datei"
#define MSGTR_Exit_error "Schwerer Fehler"
#define MSGTR_IntBySignal "\nMPlayer wurde durch Signal %d von Modul %s beendet\n"
#define MSGTR_NoHomeDir "Kann Homeverzeichnis nicht finden\n"
#define MSGTR_GetpathProblem "get_path(\"config\") Problem\n"
#define MSGTR_CreatingCfgFile "Erstelle Konfigurationsdatei: %s\n"
#define MSGTR_InvalidVOdriver "Ung�ltiger Videoausgabetreibername: %s\n'-vo help' zeigt eine Liste an.\n"
#define MSGTR_InvalidAOdriver "Ung�ltiger Audioausgabetreibername: %s\n'-ao help' zeigt eine Liste an.\n"
#define MSGTR_CopyCodecsConf "(kopiere/linke etc/codecs.conf nach ~/.mplayer/codecs.conf)\n"
#define MSGTR_CantLoadFont "Kann Schriftdatei %s nicht laden\n"
#define MSGTR_CantLoadSub "Kann Untertitel nicht laden: %s\n"
#define MSGTR_ErrorDVDkey "Fehler beim Bearbeiten des DVD-Schl�ssels..\n"
#define MSGTR_CmdlineDVDkey "Der DVD-Schl�ssel der Kommandozeile wurde f�r das Descrambeln gespeichert.\n"
#define MSGTR_DVDauthOk "DVD Authentifizierungssequenz scheint OK zu sein.\n"
#define MSGTR_DumpSelectedSteramMissing "dump: FATAL: Ausgew�hlter Stream fehlt!\n"
#define MSGTR_CantOpenDumpfile "Kann dump-Datei nicht �ffnen!!!\n"
#define MSGTR_CoreDumped "core dumped :)\n"
#define MSGTR_FPSnotspecified "FPS ist im Header nicht angegeben (oder ung�ltig)! Benutze -fps Option!\n"
#define MSGTR_NoVideoStream "Sorry, kein Videostream... ist nicht abspielbar\n"
#define MSGTR_TryForceAudioFmt "Erzwinge Audiocodecgruppe %d ...\n"
#define MSGTR_CantFindAfmtFallback "Kann keinen Audiocodec f�r gew�nschte Gruppe finden, verwende anderen.\n"
#define MSGTR_CantFindAudioCodec "Kann Codec f�r Audioformat 0x%X nicht finden!\n"
#define MSGTR_TryUpgradeCodecsConfOrRTFM "*** Versuche %s mit etc/codecs.conf zu erneuern\n*** Sollte es weiterhin nicht gehen, dann lies DOCS/CODECS!\n"
#define MSGTR_CouldntInitAudioCodec "Kann Audiocodec nicht finden! -> Kein Ton\n"
#define MSGTR_TryForceVideoFmt "Erzwinge Videocodecgruppe %d ...\n"
#define MSGTR_CantFindVfmtFallback "Kann keinen Videocodec f�r gew�nschte Gruppe finden, verwende anderen.\n"
#define MSGTR_CantFindVideoCodec "Kann keinen Codec passend zum gew�hlten -vo und Videoformat 0x%X finden!\n"
#define MSGTR_VOincompCodec "Sorry, der ausgew�hlte Videoausgabetreiber ist nicht kompatibel mit diesem Codec.\n"
#define MSGTR_CouldntInitVideoCodec "FATAL: Kann Videocodec nicht initialisieren :(\n"
#define MSGTR_EncodeFileExists "Datei existiert: %s (�berschreibe nicht deine sch�nsten AVI's!)\n"
#define MSGTR_CantCreateEncodeFile "Kann Datei zum Encoden nicht �ffnen\n"
#define MSGTR_CannotInitVO "FATAL: Kann Videoausgabetreiber nicht initialisieren!\n"
#define MSGTR_CannotInitAO "Kann Audiotreiber/Soundkarte nicht initialisieren -> Kein Ton\n"
#define MSGTR_StartPlaying "Starte Wiedergabe...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"         ***************************************************\n"\
"         **** Dein System ist zu LANGSAM zum Abspielen! ****\n"\
"         ***************************************************\n"\
"!!! M�gliche Gr�nde, Probleme, Abhilfen: \n"\
"- Meistens: defekter/fehlerhafter _Audio_ Treiber. Abhilfe: versuche -ao sdl,\n"\
"  verwende ALSA 0.5 oder die OSS Emulation von ALSA 0.9. Lese DOCS/sound.html!\n"\
"- Langsame Videoausgabe. Versuche einen anderen -vo Treiber (Liste: -vo help)\n"\
"  oder versuche es mit -framedrop ! Lese DOCS/video.html f�r Tipps.\n"\
"- Langsame CPU. Keine DVD/DIVX auf einer langsamen CPU. Versuche -hardframedrop\n"\
"- Defekte Datei. Versuche verschiede Kombinationen: -nobps  -ni  -mc 0  -forceidx\n"\
"- Wird -cache verwendet, um eine nicht-interleaved Datei abzuspielen? Versuche -nocache\n"\
"Wenn nichts davon hilft, lies DOCS/bugreports.html !\n\n"

#define MSGTR_NoGui "MPlayer wurde OHNE GUI-Unterst�tzung kompiliert!\n"
#define MSGTR_GuiNeedsX "MPlayer GUI erfordert X11!\n"
#define MSGTR_Playing "Spiele %s\n"
#define MSGTR_NoSound "Audio: kein Ton!!!\n"
#define MSGTR_FPSforced "FPS fixiert auf %5.3f  (ftime: %5.3f)\n"
#define MSGTR_CompiledWithRuntimeDetection "Kompiliert mit RUNTIME CPU Detection - Warnung, das ist nicht optimal! Um die beste Performance zu erhalten kompiliere MPlayer von den Sourcen mit --disable-runtime-cpudetection\n"
#define MSGTR_CompiledWithCPUExtensions "Kompiliert f�r x86 CPU mit folgenden Erweiterungen:"
#define MSGTR_AvailableVideoOutputPlugins "Verf�gbare Videoausgabe-Plugins:\n"
#define MSGTR_AvailableVideoOutputDrivers "Verf�gbare Videoausgabe-Treiber:\n"
#define MSGTR_AvailableAudioOutputDrivers "Verf�gbare Audioausgabe-Treiber:\n"
#define MSGTR_AvailableAudioCodecs "Verf�gbare Audiocodocs:\n"
#define MSGTR_AvailableVideoCodecs "Verf�gbare Videocodecs:\n"
#define MSGTR_UsingRTCTiming "Verwende Linux Hardware RTC-Timing (%ldHz)\n"
#define MSGTR_CannotReadVideoPropertiers "Video: Kann Eigenschaften nicht lesen\n"
#define MSGTR_NoStreamFound "Kein Streams gefunden\n"
#define MSGTR_InitializingAudioCodec "Initialisiere Audiocodec...\n"
#define MSGTR_ErrorInitializingVODevice "Fehler beim �ffenen/Initialisieren des ausgew�hlten Videoausgabe (-vo) Treibers!\n"
#define MSGTR_ForcedVideoCodec "Videocodec fixiert: %s\n"
#define MSGTR_AODescription_AOAuthor "AO: Beschreibung: %s\nAO: Autor: %s\n"
#define MSGTR_AOComment "AO: Hinweis: %s\n"
#define MSGTR_Video_NoVideo "Video: kein Video!!!\n"
#define MSGTR_NotInitializeVOPorVO "\nFATAL: Konnte Videofilter (-vop) oder Videoausgabe-Treiber (-vo) nicht initialisieren!\n"
#define MSGTR_Paused "\n------ PAUSE -------\r"
#define MSGTR_PlaylistLoadUnable "\nKann Playliste %s nicht laden\n"

// mencoder.c:

#define MSGTR_MEncoderCopyright "(C) 2000-2002 Arpad Gereoffy (siehe DOCS!)\n"
#define MSGTR_UsingPass3ControllFile "Verwende Pass 3 Kontrolldatei: %s\n"
#define MSGTR_MissingFilename "\nDateiname nicht angegeben!\n\n"
#define MSGTR_CannotOpenFile_Device "Kann Datei/Ger�t nicht �ffnen\n"
#define MSGTR_ErrorDVDAuth "Fehler bei der DVD Authentifizierung...\n"
#define MSGTR_CannotOpenDemuxer "Kann Demuxer nicht �ffnen\n"
#define MSGTR_NoAudioEncoderSelected "\nKein Audioencoder (-oac) ausgew�hlt! W�hle einen aus oder verwende -nosound. Verwende -oac help !\n"
#define MSGTR_NoVideoEncoderSelected "\nKein Videoencoder (-ovc) selected! W�hle einen aus, verwende -ovc help !\n"
#define MSGTR_CannotOpenOutputFile "Kann Ausgabedatei '%s' nicht �ffnen\n"
#define MSGTR_EncoderOpenFailed "�ffnen des Encoders fehlgeschlagen\n"
#define MSGTR_ForcingOutputFourcc "Output-Fourcc auf %x [%.4s] gestellt\n"
#define MSGTR_WritingAVIHeader "Schreibe AVI Header...\n"
#define MSGTR_DuplicateFrames "\n%d doppelte(r) Frame(s)!!!    \n"
#define MSGTR_SkipFrame "\nFrame ausgelassen!!!    \n"
#define MSGTR_ErrorWritingFile "%s: Fehler beim Schreiben der Datei.\n"
#define MSGTR_WritingAVIIndex "\nSchreibe AVI Index...\n"
#define MSGTR_FixupAVIHeader "Fixiere AVI Header...\n"
#define MSGTR_RecommendedVideoBitrate "Empfohlene Videobitrate f�r %s CD: %d\n"
#define MSGTR_VideoStreamResult "\nVideostream: %8.3f kbit/s  (%d bps)  Gr��e: %d Bytes  %5.3f Sek.  %d Frames\n"
#define MSGTR_AudioStreamResult "\nAudiostream: %8.3f kbit/s  (%d bps)  Gr��e: %d Bytes  %5.3f Sek.\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "CD-ROM Ger�t '%s' nicht gefunden!\n"
#define MSGTR_ErrTrackSelect "Fehler beim Ausw�hlen des VCD Tracks!"
#define MSGTR_ReadSTDIN "Lese von stdin...\n"
#define MSGTR_UnableOpenURL "Kann URL nicht �ffnen: %s\n"
#define MSGTR_ConnToServer "Verbunden mit Server: %s\n"
#define MSGTR_FileNotFound "Datei nicht gefunden: '%s'\n"

#define MSGTR_CantOpenDVD "Kann DVD Ger�t nicht �ffnen: %s\n"
#define MSGTR_DVDwait "Lese Disk-Struktur, bitte warten...\n"
#define MSGTR_DVDnumTitles "Es sind %d Titel auf dieser DVD.\n"
#define MSGTR_DVDinvalidTitle "Ung�ltige DVD Titelnummer: %d\n"
#define MSGTR_DVDnumChapters "Es sind %d Kapitel auf diesem DVD Titel.\n"
#define MSGTR_DVDinvalidChapter "Ung�ltige DVD Kapitelnummer: %d\n"
#define MSGTR_DVDnumAngles "Es sind %d Sequenzen auf diesem DVD Titel.\n"
#define MSGTR_DVDinvalidAngle "Ung�ltige DVD Sequenznummer: %d\n"
#define MSGTR_DVDnoIFO "Kann die IFO-Datei f�r den DVD-Titel nicht �ffnen %d.\n"
#define MSGTR_DVDnoVOBs "Kann Titel-VOBS (VTS_%02d_1.VOB) nicht �ffnen.\n"
#define MSGTR_DVDopenOk "DVD erfolgreich ge�ffnet!\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "Warnung! Audiostreamheader %d redefiniert!\n"
#define MSGTR_VideoStreamRedefined "Warnung! Videostreamheader %d redefiniert!\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: Zu viele (%d in %d bytes) Audiopakete im Puffer!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: Zu viele (%d in %d bytes) Videopakete im Puffer!\n"
#define MSGTR_MaybeNI "Vielleicht spielst du einen non-interleaved Stream/Datei oder der Codec funktioniert nicht.\n" \
                     "Versuche f�r .AVI Dateien den nicht-interleaved Modus mit der Option -ni zu erzwingen\n"
#define MSGTR_SwitchToNi "\nSchlecht Interleaved .AVI erkannt, schalte in das -ni Modus!\n"
#define MSGTR_DetectedFILMfile "FILM Dateiformat erkannt!\n"
#define MSGTR_DetectedFLIfile "FLI Dateiformat erkannt!\n"
#define MSGTR_DetectedROQfile "RoQ Dateiformat erkannt!\n"
#define MSGTR_DetectedREALfile "REAL Dateiformat erkannt!\n"
#define MSGTR_DetectedAVIfile "AVI Dateiformat erkannt!\n"
#define MSGTR_DetectedASFfile "ASF Dateiformat erkannt!\n"
#define MSGTR_DetectedMPEGPESfile "MPEG-PES Dateiformat erkannt!\n"
#define MSGTR_DetectedMPEGPSfile "MPEG-PS Dateiformat erkannt!\n"
#define MSGTR_DetectedMPEGESfile "MPEG-ES Dateiformat erkannt!\n"
#define MSGTR_DetectedQTMOVfile "QuickTime/MOV Dateiformat erkannt!\n"
#define MSGTR_MissingMpegVideo "Vermisse MPEG Videostream!? Kontaktiere den Author, das k�nnte ein Bug sein :(\n"
#define MSGTR_InvalidMPEGES "Ung�ltiger MPEG-ES Stream??? Kontaktiere den Author, das k�nnte ein Bug sein :(\n"
#define MSGTR_FormatNotRecognized "=========== Sorry, das Dateiformat/Codec wird nicht unterst�tzt ==============\n"\
				  "============== Sollte dies ein AVI, ASF oder MPEG Stream sein, ===============\n"\
				  "================== dann kontaktiere bitte den Author =========================\n"
#define MSGTR_MissingVideoStream "kann keinen Videostream finden!\n"
#define MSGTR_MissingAudioStream "kann keinen Audiostream finden...  -> kein Ton\n"
#define MSGTR_MissingVideoStreamBug "Vermisse Videostream!? Kontaktiere den Author, m�glicherweise ein Bug :(\n"

#define MSGTR_DoesntContainSelectedStream "Demux: Datei enth�lt nicht den gew�hlen Audio- oder Videostream\n"

#define MSGTR_NI_Forced "Erzwungen"
#define MSGTR_NI_Detected "Erkannt"
#define MSGTR_NI_Message "%s NON-INTERLEAVED AVI Dateiformat!\n"

#define MSGTR_UsingNINI "Verwende defektes NON-INTERLEAVED AVI Dateiformat!\n"
#define MSGTR_CouldntDetFNo "Konnte die Anzahl der Frames (f�r absulute Suche) nicht finden  \n"
#define MSGTR_CantSeekRawAVI "Kann keine RAW .AVI-Streams durchsuchen! (Index erforderlich, versuche es mit der -idx Option!)  \n"
#define MSGTR_CantSeekFile "Kann diese Datei nicht durchsuchen!  \n"

#define MSGTR_EncryptedVOB "Verschl�sselte VOB-Datei (wurde ohne libcss Unterst�tzung kompiliert)! Lese DOCS\n"
#define MSGTR_EncryptedVOBauth "Verschl�sselter Stream, jedoch wurde die Authentifizierung nicht von Dir gefordert!!\n"

#define MSGTR_MOVcomprhdr "MOV: Komprimierte Header werden (zur Zeit) nicht unterst�tzt!\n"
#define MSGTR_MOVvariableFourCC "MOV: Warnung! Variable FOURCC erkannt!?\n"
#define MSGTR_MOVtooManyTrk "MOV: Warnung! Zu viele Tracks!"
#define MSGTR_MOVnotyetsupp "\n******** Quicktime MOV Format wird zu Zeit nicht unterst�tzt!!!!!!! *********\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "kann Codec nicht �ffnen\n"
#define MSGTR_CantCloseCodec "kann Codec nicht schlie�en\n"

#define MSGTR_MissingDLLcodec "FEHLER: Kann erforderlichen DirectShow Codec nicht finden: %s\n"
#define MSGTR_ACMiniterror "Kann Win32/ACM AUDIO Codec nicht finden (fehlende DLL-Datei?)\n"
#define MSGTR_MissingLAVCcodec "Kann Codec '%s' von libavcodec nicht finden...\n"

#define MSGTR_NoDShowSupport "MPlayer wurde OHNE DirectShow Unterst�tzung kompiliert!\n"
#define MSGTR_NoWfvSupport "Unterst�tzung f�r Win32 Codecs ausgeschaltet oder nicht verf�gbar auf nicht-x86 Plattformen!\n"
#define MSGTR_NoDivx4Support "MPlayer wurde OHNE DivX4Linux (libdivxdecore.so) Unterst�tzung kompiliert!\n"
#define MSGTR_NoLAVCsupport "MPlayer wurde OHNE ffmpeg/libavcodec Unterst�tzung kompiliert!\n"
#define MSGTR_NoACMSupport "Win32/ACM Audiocodecs ausgeschaltet oder nicht verf�gbar auf nicht-x86 Plattformen -> erzwinge -nosound :(\n"
#define MSGTR_NoDShowAudio "MPlayer wurde ohne DirectShow Unterst�tzung kompiliert -> erzwinge -nosound :(\n"
#define MSGTR_NoOggVorbis "OggVorbis Audiocodec ausgeschaltet -> erzwinge -nosound :(\n"
#define MSGTR_NoXAnimSupport "MPlayer wurde ohne XAnim Unterst�tzung kompiliert!\n"

#define MSGTR_MpegPPhint "WARNUNG! Du hast Bild-Postprocessing erbeten f�r ein MPEG 1/2 Video,\n" \
			 "         aber Du hast MPlayer ohne MPEG 1/2 Postprocessing-Support kompiliert!\n" \
			 "         #define MPEG12_POSTPROC in config.h und kompiliere libmpeg2 neu!\n"
#define MSGTR_MpegNoSequHdr "MPEG: FATAL: Ende der Datei w�hrend der Suche f�r Sequenzheader\n"
#define MSGTR_CannotReadMpegSequHdr "FATAL: Kann Sequenzheader nicht lesen!\n"
#define MSGTR_CannotReadMpegSequHdrEx "FATAL: Kann Sequenzheader-Erweiterung nicht lesen!\n"
#define MSGTR_BadMpegSequHdr "MPEG: Schlechte Sequenzheader!\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: Schlechte Sequenzheader-Erweiterung!\n"

#define MSGTR_ShMemAllocFail "Kann keinen gemeinsamen Speicher zuweisen\n"
#define MSGTR_CantAllocAudioBuf "Kann keinen Audioausgabe-Puffer zuweisen\n"
#define MSGTR_NoMemForDecodedImage "nicht genug Speicher f�r den Puffer der dekodierten Bilder (%ld Bytes)\n"

#define MSGTR_AC3notvalid "AC3 Stream ung�ltig.\n"
#define MSGTR_AC3only48k "Nur 48000 Hz Streams werden unterst�tzt.\n"
#define MSGTR_UnknownAudio "Unbekanntes/fehlendes Audioformat -> kein Ton\n"

// LIRC:
#define MSGTR_SettingUpLIRC "Initialisiere LIRC Unterst�tzung...\n"
#define MSGTR_LIRCdisabled "Verwenden der Fernbedienung nicht m�glich\n"
#define MSGTR_LIRCopenfailed "Fehler beim �ffnen der LIRC Unterst�tzung!\n"
#define MSGTR_LIRCsocketerr "Fehler im LIRC Socket: %s\n"
#define MSGTR_LIRCcfgerr "Kann LIRC Konfigurationsdatei nicht lesen %s !\n"


// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "�ber ..."
#define MSGTR_FileSelect "W�hle Datei ..."
#define MSGTR_SubtitleSelect "W�hle Untertitel ..."
#define MSGTR_OtherSelect "W�hle ..."
#define MSGTR_AudioFileSelect "W�hle externen Audiokanal ..."
#define MSGTR_FontSelect "W�hle Schrift ..."
#define MSGTR_MessageBox "Message-Box"
#define MSGTR_PlayList "Playlist"
#define MSGTR_Equalizer "Equalizer"
#define MSGTR_SkinBrowser "Skin-Browser"
#define MSGTR_Network "Netzwerk-Streaming ..."
#define MSGTR_Preferences "Einstellungen"
#define MSGTR_OSSPreferences "OSS Treibereinstellungen"
			 

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
#define MSGTR_NEMDB "Sorry, nicht genug Speicher f�r den Zeichen-Puffer."
#define MSGTR_NEMFMR "Sorry, nicht genug Speicher f�r Men�-Rendering."
#define MSGTR_NEMFMM "Sorry, nicht genug Speicher f�r die Hauptfenster-Maske."
#define MSGTR_IDFGCVD "Sorry, kann keinen GUI-kompatiblen Ausgabetreiber finden"
			 
// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[Skin] Fehler in Skin-Konfigurationsdatei in Zeile %d: %s" 
#define MSGTR_SKIN_WARNING1 "[Skin] Warnung in Skin-Konfigurationsdatei in Zeile %d: Widget gefunden, aber davor wurde \"section\" nicht gefunden ( %s )"
#define MSGTR_SKIN_WARNING2 "[Skin] Warnung in Skin-Konfigurationsdatei in Zeile %d: Widget gefunden, aber davor wurde \"subsection\" nicht gefunden (%s)"
#define MSGTR_SKIN_BITMAP_16bit  "Bitmaps mit 16 Bits oder weniger werden nicht unterst�tzt ( %s ).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "Datei nicht gefunden ( %s )\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "BMP Lesefehler ( %s )\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "TGA Lesefehler ( %s )\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "PNG Lesefehler ( %s )\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "RLE gepackte TGA werden nicht unterst�tzt ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "unbekanntes Dateiformat ( %s )\n"
#define MSGTR_SKIN_BITMAP_ConvertError "Konvertierungsfehler von 24 Bit auf 32 Bit ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "unbekannte Nachricht: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "nicht genug Speicher\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "zu viele Schriften eingestellt\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "Schriftdatei nicht gefunden\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "Schriftbilddatei nicht gefunden\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "nicht existierende Schriftbezeichnung ( %s )\n"
#define MSGTR_SKIN_UnknownParameter "unbekannter Parameter ( %s )\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[Skin Browser] nicht genug Speicher.\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "Skin nicht gefunden ( %s ).\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "Lesefehler beim Lesen der Skin-Configdatei ( %s ).\n"
#define MSGTR_SKIN_LABEL "Skins:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "�ber MPlayer"
#define MSGTR_MENU_Open "�ffnen ..."
#define MSGTR_MENU_PlayFile "Spiele Datei ..."
#define MSGTR_MENU_PlayVCD "Spiele VCD ..."
#define MSGTR_MENU_PlayDVD "Spiele DVD ..."
#define MSGTR_MENU_PlayURL "Spiele URL ..."
#define MSGTR_MENU_LoadSubtitle "Lade Untertitel ..."
#define MSGTR_MENU_LoadExternAudioFile "Lade externe Audiodatei ..."
#define MSGTR_MENU_Playing "Spiele"
#define MSGTR_MENU_Play "Spiele"
#define MSGTR_MENU_Pause "Pause"
#define MSGTR_MENU_Stop "Stop"
#define MSGTR_MENU_NextStream "N�chster Stream"
#define MSGTR_MENU_PrevStream "Verheriger Stream"
#define MSGTR_MENU_Size "Gr��e"
#define MSGTR_MENU_NormalSize "Normale Gr��e"
#define MSGTR_MENU_DoubleSize "Doppelte Gr��e"
#define MSGTR_MENU_FullScreen "Vollbild"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "Spiele Disk ..."
#define MSGTR_MENU_ShowDVDMenu "Zeige DVD Men�"
#define MSGTR_MENU_Titles "Titel"
#define MSGTR_MENU_Title "Titel %2d"
#define MSGTR_MENU_None "(nichts)"
#define MSGTR_MENU_Chapters "Kapitel"
#define MSGTR_MENU_Chapter "Kapitel %2d"
#define MSGTR_MENU_AudioLanguages "Audio-Sprachen"
#define MSGTR_MENU_SubtitleLanguages "Untertitel-Sprachen"
#define MSGTR_MENU_PlayList "Playliste"
#define MSGTR_MENU_SkinBrowser "Skinbrowser"
#define MSGTR_MENU_Preferences "Einstellungen"
#define MSGTR_MENU_Exit "Beenden ..."

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
#define MSGTR_EQU_Bass "Tiefton" // LFE
#define MSGTR_EQU_All "Alle"

// --- playlist
#define MSGTR_PLAYLIST_Path "Pfad"
#define MSGTR_PLAYLIST_Selected "ausgew�hlte Dateien"
#define MSGTR_PLAYLIST_Files "Dateien"
#define MSGTR_PLAYLIST_DirectoryTree "Verzeichnisbaum"

// --- preferences
#define MSGTR_PREFERENCES_None "Nichts"
#define MSGTR_PREFERENCES_Codec1 "Verwende VFW (Win32) Codecs"
#define MSGTR_PREFERENCES_Codec2 "Verwende OpenDivX/DivX4 Codec (YV12)"
#define MSGTR_PREFERENCES_Codec3 "Verwende DirectShow (Win32) Codecs"
#define MSGTR_PREFERENCES_Codec4 "Verwende ffmpeg (libavcodec) Codecs"
#define MSGTR_PREFERENCES_Codec5 "Verwende DivX4 Codec (YUY2)"
#define MSGTR_PREFERENCES_Codec6 "Verwende XAnim Codecs"
#define MSGTR_PREFERENCES_AvailableDrivers "Verf�gbare Treiber:"
#define MSGTR_PREFERENCES_DoNotPlaySound "Spiele keinen Ton"
#define MSGTR_PREFERENCES_NormalizeSound "Normalisiere Ton"
#define MSGTR_PREFERENCES_EnEqualizer "Equalizer verwenden"
#define MSGTR_PREFERENCES_ExtraStereo "Extra Stereo verwenden"
#define MSGTR_PREFERENCES_Coefficient "Koeffizient:"
#define MSGTR_PREFERENCES_AudioDelay "Audio-Verz�gerung"
#define MSGTR_PREFERENCES_Audio "Audio"
#define MSGTR_PREFERENCES_VideoEqu "Video Equalizer verwenden"
#define MSGTR_PREFERENCES_DoubleBuffer "Double-Buffering verwenden"
#define MSGTR_PREFERENCES_DirectRender "Direct-Rendering verwenden"
#define MSGTR_PREFERENCES_FrameDrop "Frame-Dropping verwenden"
#define MSGTR_PREFERENCES_HFrameDrop "HARD Frame-Dropping verwenden ( gef�hrlich )"
#define MSGTR_PREFERENCES_Flip "Bild spiegeln"
#define MSGTR_PREFERENCES_Panscan "Panscan: "
#define MSGTR_PREFERENCES_Video "Video"
#define MSGTR_PREFERENCES_OSDTimer "Zeit und Indikatoren"
#define MSGTR_PREFERENCES_OSDProgress "nur Progressbar"
#define MSGTR_PREFERENCES_Subtitle "Untertitel:"
#define MSGTR_PREFERENCES_SUB_Delay "Verz�gerung: "
#define MSGTR_PREFERENCES_SUB_FPS "FPS:"
#define MSGTR_PREFERENCES_SUB_POS "Position: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "Automatisches Laden der Untertitel ausschalten"
#define MSGTR_PREFERENCES_SUB_Unicode "Unicode Subtitle"
#define MSGTR_PREFERENCES_SUB_MPSUB "Konvertiere Untertitel zum MPlayer Untertitelformat"
#define MSGTR_PREFERENCES_SUB_SRT "Konvertiere Untertitel zum zeitbasierenden SubViewer (SRT) Untertitelformat"
#define MSGTR_PREFERENCES_Font "Schrift:"
#define MSGTR_PREFERENCES_FontFactor "Schriftfaktor:"
#define MSGTR_PREFERENCES_PostProcess "Postprocess verwenden"
#define MSGTR_PREFERENCES_AutoQuality "Auto-Qualit�t: "
#define MSGTR_PREFERENCES_NI "Non-Interleaved AVI Parser verwenden"
#define MSGTR_PREFERENCES_IDX "Index Table neu aufbauen, falls ben�tigt"
#define MSGTR_PREFERENCES_VideoCodecFamily "Videocodec Familie:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "OSD Level"
#define MSGTR_PREFERENCES_FRAME_Subtitle "Untertitel"
#define MSGTR_PREFERENCES_FRAME_Font "Schrift"
#define MSGTR_PREFERENCES_FRAME_PostProcess "Postprocess"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "Codec & Demuxer"
#define MSGTR_PREFERENCES_OSS_Device "Ger�t:"
#define MSGTR_PREFERENCES_OSS_Mixer "Mixer:"
#define MSGTR_PREFERENCES_Message "Bitte bedenke, mache Funktionen erfordern einen Neustart der Wiedergabe."
			 
// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "Fataler Fehler ..."
#define MSGTR_MSGBOX_LABEL_Error "Fehler ..."
#define MSGTR_MSGBOX_LABEL_Warning "Warnung ..."

#endif

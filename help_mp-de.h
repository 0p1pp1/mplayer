#ifdef HELP_MP_DEFINE_STATIC
static char* banner_text=
"\n\n"
"MPlayer " VERSION "(C) 2000-2001 Arpad Gereoffy (siehe DOCS/AUTHORS)\n"
"\n";

static char help_text[]=
"Benutzung:   mplayer [optionen] [verzeichnis/]dateiname\n"
"\n"
"Optionen:\n"
" -vo <drv[:dev]> Videoausgabetreiber & -Ger�t (siehe '-vo help' f�r eine Liste)\n"
" -ao <drv[:dev]> Audioausgabetreiber & -Ger�t (siehe '-ao help' f�r eine Liste)\n"
" -vcd <tracknr>  Spiele VCD (video cd) Titel anstelle eines Dateinames\n"
#ifdef HAVE_LIBCSS
" -dvdauth <dev>  Benutze DVD Ger�t f�r die Authentifizierung (f�r verschl. DVD's)\n"
#endif
" -ss <timepos>   Starte abspielen ab Position (Sekunden oder hh:mm:ss)\n"
" -nosound        Spiele keinen Sound\n"
#ifdef USE_FAKE_MONO
" -stereo         Auswahl der MPEG1-Stereoausgabe (0:stereo 1:links 2:rechts)\n"
#endif
" -fs -vm -zoom   Vollbild Optionen (Vollbild, Videomode, Softwareskalierung)\n"
" -x <x> -y <y>   Skaliere Bild auf <x> * <y> [wenn vo-Treiber mitmacht]\n"
" -sub <file>     Benutze Untertitledatei (siehe auch -subfps, -subdelay)\n"
" -vid x -aid y   Spiele Videostream (x) und Audiostream (y)\n"
" -fps x -srate y Benutze Videoframerate (x fps) und Audiosamplingrate (y Hz)\n"
" -pp <quality>   Aktiviere Nachbearbeitungsfilter (0-4 bei DivX, 0-63 bei MPEG)\n"
" -bps            Benutze alternative A-V sync Methode f�r AVI's (k�nnte helfen!)\n"
" -framedrop      Benutze frame-dropping (f�r langsame Rechner)\n"
"\n"
"Tasten:\n"
" <- oder ->      Springe zehn Sekunden vor/zur�ck\n"
" rauf / runter   Springe eine Minute vor/zur�ck\n"
" p or LEER       PAUSE (beliebige Taste zum fortsetzen)\n"
" q or ESC        Abspielen stoppen und Programm beenden\n"
" + or -          Audioverz�gerung um +/- 0.1 Sekunde ver�ndern\n"
" o               OSD Mode:  Aus / Zuchleiste / Zuchleiste+Zeit\n"
" * or /          Lautst�rke verstellen ('m' f�r Auswahl Master/Wave)\n"
" z or x          Untertitelverz�gerung um +/- 0.1 Sekunde ver�ndern\n"
"\n"
" * * * IN DER MANPAGE STEHEN WEITERE KEYS UND OPTIONEN ! * * *\n"
"\n";
#endif

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
#define MSGTR_InvalidVOdriver "Ung�ltiger Videoausgabetreibername: %s\n'-vo help' zeigt eine Liste aller.\n"
#define MSGTR_InvalidAOdriver "Ung�ltiger Audioausgabetreibername: %s\n'-vo help' zeigt eine Liste aller.\n"
#define MSGTR_CopyCodecsConf "(kopiere/linke etc/codecs.conf nach ~/.mplayer/codecs.conf)\n"
#define MSGTR_CantLoadFont "Kann Schriftdatei %s nicht laden\n"
#define MSGTR_CantLoadSub "Kann Untertitel nicht laden: %s\n"
#define MSGTR_ErrorDVDkey "Fehler beim bearbeiten des DVD-Schl�ssels.\n"
#define MSGTR_CmdlineDVDkey "der DVD-Schl�ssel der Kommandozeile wurde f�r das descrambeln gespeichert.\n"
#define MSGTR_DVDauthOk "DVD Authentifizierungssequenz scheint OK zu sein.\n"
#define MSGTR_DumpSelectedSteramMissing "dump: FATAL: Ausgew�hlter Stream fehlt!\n"
#define MSGTR_CantOpenDumpfile "Kann dump-Datei nicht �ffnen!!!\n"
#define MSGTR_CoreDumped "core dumped :)\n"
#define MSGTR_FPSnotspecified "FPS ist im Header nicht angegeben (oder ung�ltig)! Benutze -fps Option!\n"
#define MSGTR_NoVideoStream "Sorry, kein video stream... ist unabspielbar\n"
#define MSGTR_TryForceAudioFmt "Erzwinge Audiocodecgruppe %d ...\n"
#define MSGTR_CantFindAfmtFallback "Kann keinen Audiocodec f�r gew�nschte Gruppe finden, verwende anderen\n"
#define MSGTR_CantFindAudioCodec "Kann Codec f�r Audioformat 0x%X nicht finden!\n"
#define MSGTR_TryUpgradeCodecsConfOrRTFM "*** Versuche %s mit etc/codecs.conf zu erneuern\n*** Sollte es weiterhin nicht gehen, dann lese DOCS/CODECS!\n"
#define MSGTR_CouldntInitAudioCodec "Kann Audiocodec nicht finden! -> Stummfilm\n"
#define MSGTR_TryForceVideoFmt "Erzwinge Videocodecgruppe %d ...\n"
#define MSGTR_CantFindVfmtFallback "Kann keinen Videocodec f�r gew�nschte Gruppe finden, verwende anderen\n"
#define MSGTR_CantFindVideoCodec "Kann Videocodec f�r Format 0x%X nicht finden!\n"
#define MSGTR_VOincompCodec "Sorry, der ausgew�hlte Videoausgabetreiber ist nicht kompatibel mit diesem Codec.\n"
#define MSGTR_CouldntInitVideoCodec "FATAL: Kann Videocodec nicht initialisieren :(\n"
#define MSGTR_EncodeFileExists "Datei existiert: %s (�berschreibe nicht deine sch�nsten AVI's!)\n"
#define MSGTR_CantCreateEncodeFile "Kann Datei zum Encoden nicht �ffnen\n"
#define MSGTR_CannotInitVO "FATAL: Kann Videoausgabetreiber nicht initialisieren!\n"
#define MSGTR_CannotInitAO "Kann Audiotreiber/Soundkarte nicht initialisieren -> Stummfilm\n"
#define MSGTR_StartPlaying "Starte Abspielen...\n"
#define MSGTR_SystemTooSlow "\n************************************************************************"\
			    "\n* Dein System ist zu langsam. Versuche die -framedrop Option oder RTFM!*"\
			    "\n************************************************************************\n"
//#define MSGTR_

// open.c: 
#define MSGTR_CdDevNotfound "CD-ROM Ger�t '%s' nicht gefunden!\n"
#define MSGTR_ErrTrackSelect "Fehler beim ausw�hlen des VCD Tracks!"
#define MSGTR_ReadSTDIN "Lese von stdin...\n"
#define MSGTR_UnableOpenURL "Kann URL nicht �ffnen: %s\n"
#define MSGTR_ConnToServer "Verbunden mit Server: %s\n"
#define MSGTR_FileNotFound "Datei nicht gefunden: '%s'\n"

// demuxer.c:
#define MSGTR_AudioStreamRedefined "Warnung! Audiostreamheader %d redefiniert!\n"
#define MSGTR_VideoStreamRedefined "WarnUng! Aideostreamheader %d redefiniert!\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: Zu viele (%d in %d bytes) Audiopakete im Puffer!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: Zu viele (%d in %d bytes) Videopakete im Puffer\n"
#define MSGTR_MaybeNI "Vielleicht spielst du einen non-interleaved Stream/Datei oder der Codec geht nicht\n"
#define MSGTR_DetectedAVIfile "AVI Dateiformat erkannt!\n"
#define MSGTR_DetectedASFfile "ASF Dateiformat erkannt!\n"
#define MSGTR_DetectedMPEGPESfile "MPEG-PES Dateiformat erkannt!\n"
#define MSGTR_DetectedMPEGPSfile "MPEG-PS Dateiformat erkannt!\n"
#define MSGTR_DetectedMPEGESfile "MPEG-ES Dateiformat erkannt!\n"
#define MSGTR_DetectedQTMOVfile "QuickTime/MOV Dateiformat erkannt!\n"
#define MSGTR_MissingMpegVideo "Vermisse MPEG Videostream!? Kontaktiere den Author, das k�nnte ein Bug sein :(\n"
#define MSGTR_InvalidMPEGES "Ung�ltiger MPEG-ES Stream??? Kontaktiere den Author, das k�nnte ein Bug sein :(\n"
#define MSGTR_FormatNotRecognized \
"=========== Sorry, das Dateiformat/Codec wird nicht unterst�tzt =============\n"\
"============== Sollte dies ein AVI, ASF oder MPEG Stream sein, ==============\n"\
"================== dann kontaktiere bitte den Author ========================\n"
#define MSGTR_MissingASFvideo "ASF: kann keinen Videostream finden\n"
#define MSGTR_MissingASFaudio "ASF: kann keinen Audiostream finden...  ->Stummfilm\n"
#define MSGTR_MissingMPEGaudio "MPEG: kann keinen Audiostream finden...  ->Stummfilm\n"

//#define MSGTR_

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "About"
#define MSGTR_FileSelect "Select file ..."
#define MSGTR_MessageBox "MessageBox"
#define MSGTR_PlayList "PlayList"
#define MSGTR_SkinBrowser "Skin Browser"

// --- buttons ---
#define MSGTR_Ok "Ok"
#define MSGTR_Cancel "Cancel"
#define MSGTR_Add "Add"
#define MSGTR_Remove "Remove"

// --- error messages ---
#define MSGTR_NEMDB "Sorry, not enough memory for draw buffer."
#define MSGTR_NEMFMR "Sorry, not enough memory for menu rendering."
#define MSGTR_NEMFMM "Sorry, not enough memory for main window shape mask."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[skin] error in skin config file on line %d: %s"
#define MSGTR_SKIN_WARNING1 "[skin] warning in skin config file on line %d: widget found but before \"section\" n
#define MSGTR_SKIN_WARNING2 "[skin] warning in skin config file on line %d: widget found but before \"subsection\
#define MSGTR_SKIN_BITMAP_16bit  "16 bits or less depth bitmap not supported ( %s ).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "file not found ( %s )\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "bmp read error ( %s )\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "tga read error ( %s )\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "png read error ( %s )\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "RLE packed tga not supported ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "unknown file type ( %s )\n"
#define MSGTR_SKIN_BITMAP_ConvertError "24 bit to 32 bit convert error ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "unknown message: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "not enought memory\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "too many fonts declared\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "font file not found\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "font image file not found\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "non-existent font identifier ( %s )\n"
#define MSGTR_SKIN_UnknownParameter "unknown parameter ( %s )\n"

#endif

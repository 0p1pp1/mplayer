// Translated by:  Panagiotis Issaris <takis@lumumba.luc.ac.be>

#ifdef HELP_MP_DEFINE_STATIC
static char* banner_text=
"\n\n"
"MPlayer " VERSION "(C) 2000-2001 Arpad Gereoffy (zie DOCS!)\n"
"\n";

static char help_text[]=
#ifdef HAVE_NEW_GUI
"Gebruik:   mplayer [-gui] [opties] [pad/]bestandsnaam\n"
#else
"Gebruik:   mplayer [opties] [pad/]bestandsnaam\n"
#endif
"\n"
"Opties:\n"
" -vo <drv[:dev]>  selecteer video uitvoer driver & device (zie '-vo help' voor lijst)\n"
" -ao <drv[:dev]>  selecteer audio uitvoer driver & device (zie '-ao help' voor lijst)\n"
" -vcd <trackno>   speel VCD (Video CD) track van device in plaats van standaard bestand\n"
#ifdef HAVE_LIBCSS
" -dvdauth <dev>   specificeer DVD device voor authenticatie (voor geencrypteerde schijven)\n"
#endif
#ifdef USE_DVDREAD
" -dvd <titelnr>   speel DVD titel/track van device in plaats van gewoon bestand\n"
#endif
" -ss <timepos>    ga naar opgegeven (seconden of hh:mm:ss) positie\n"
" -nosound         speel het geluid niet af\n"
#ifdef USE_FAKE_MONO
" -stereo <mode>   selecteer MPEG1 stereo uitvoer (0:stereo 1:links 2:rechts)\n"
#endif
" -fs -vm -zoom    volledig scherm afspeel opties (fullscr,vidmode chg,softw.scale)\n"
" -x <x> -y <y>    herschaal beeld naar <x> * <y> resolutie [als -vo driver het ondersteunt!]\n"
" -sub <bestand>   specificeer het te gebruiken ondertitel bestand (zie ook -subfps, -subdelay)\n"
" -vid x -aid y    opties om te spelen video (x) en audio stream te selecteren\n"
" -fps x -srate y  opties om video (x fps) en audio (y Hz) tempo te veranderen\n"
" -pp <kwaliteit>  activeer postprocessing filter (0-4 voor DivX, 0-63 voor mpegs)\n"
" -nobps           gebruik alternatieve A-V sync methode voor AVI bestand (kan helpen!)\n"
" -framedrop       activeer frame-dropping (voor trage machines)\n"
"\n"
"Toetsen:\n"
" <-  of  ->       ga 10 seconden achterwaards/voorwaards\n"
" omhoog of omlaag ga 1 minuut achterwaards/voorwaards\n"
" PGUP of PGDOWN   ga 10 minuten achterwaards/voorwaards\n"
" p of SPACE       pauzeer film (druk eender welke toets om verder te gaan)\n"
" q of ESC         stop afspelen en sluit programma af\n"
" + of -           pas audio vertraging aan met +/- 0.1 second\n"
" o                cycle OSD mode:  geen / zoekbalk / zoekbalk+tijd\n"
" * of /           verhoog of verlaag volume (druk 'm' om master/pcm te selecteren)\n"
" z of x           pas ondertiteling vertraging aan met +/- 0.1 second\n"
"\n"
" * * * ZIE MANPAGE VOOR DETAILS, OVERIGE (GEAVANCEERDE) OPTIES EN TOETSEN ! * * *\n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// mplayer.c: 

#define MSGTR_Exiting "\nExiting... (%s)\n"
#define MSGTR_Exit_frames "Gevraagde aantal frames afgespeeld"
#define MSGTR_Exit_quit "Stop"
#define MSGTR_Exit_eof "Einde van bestand"
#define MSGTR_Exit_error "Fatale fout"
#define MSGTR_IntBySignal "\nMPlayer onderbroken door signal %d in module: %s \n"
#define MSGTR_NoHomeDir "Kan HOME dir niet vinden\n"
#define MSGTR_GetpathProblem "get_path(\"config\") probleem\n"
#define MSGTR_CreatingCfgFile "Bezig met het creeren van config bestand: %s\n"
#define MSGTR_InvalidVOdriver "Foutieve video uitvoer driver naam: %s\nGebruik '-vo help' om een lijst met beschikbare video drivers te verkrijgen.\n"
#define MSGTR_InvalidAOdriver "Foutieve audio uitvoer driver naam: %s\nGebruik '-ao help' om een lijst met beschikbare audio drivers te verkrijgen.\n"
#define MSGTR_CopyCodecsConf "(copy/ln etc/codecs.conf (van MPlayer source tree) naar ~/.mplayer/codecs.conf)\n"
#define MSGTR_CantLoadFont "Kan font niet laden: %s\n"
#define MSGTR_CantLoadSub "Kan ondertitels niet lezen: %s\n"
#define MSGTR_ErrorDVDkey "Fout bij het verwerken van DVD KEY.\n"
#define MSGTR_CmdlineDVDkey "DVD command line aangevraagde sleutel is opgeslaan voor descrambling.\n"
#define MSGTR_DVDauthOk "DVD auth sequence lijkt OK te zijn.\n"
#define MSGTR_DumpSelectedSteramMissing "dump: FATAL: geselecteerde stream ontbreekt!\n"
#define MSGTR_CantOpenDumpfile "Kan dump bestand niet openen!!!\n"
#define MSGTR_CoreDumped "core dumped :)\n"
#define MSGTR_FPSnotspecified "FPS niet gespecificeerd (of foutief) in de header! Gebruik de optie -fps!\n"
#define MSGTR_NoVideoStream "Sorry, geen video stream... het is nog niet afspeelbaar\n"
#define MSGTR_TryForceAudioFmt "Probeer audio codec driver familie %d te forceren...\n"
#define MSGTR_CantFindAfmtFallback "Kan audio codec voor geforceerde driver familie niet vinden, val terug op andere drivers.\n"
#define MSGTR_CantFindAudioCodec "Kan codec voor audio format 0x%X niet vinden!\n"
#define MSGTR_TryUpgradeCodecsConfOrRTFM "*** Probeer %s te upgraden van etc/codecs.conf\n*** Als het nog steeds niet OK is, lees dan DOCS/CODECS!\n"
#define MSGTR_CouldntInitAudioCodec "Kon audio codec niet initialiseren! -> nosound\n"
#define MSGTR_TryForceVideoFmt "Probeer video codec driver familie %d te forceren...\n"
#define MSGTR_CantFindVfmtFallback "Kan video codec voor geforceerde driver familie niet vinden, val terug op andere drivers.\n"
#define MSGTR_CantFindVideoCodec "Kan codec voor video format 0x%X niet vinden!\n"
#define MSGTR_VOincompCodec "Sorry, geselecteerde video_out device is incompatibel met deze codec.\n"
#define MSGTR_CouldntInitVideoCodec "FATAL: Kon video codec niet initialiseren :(\n"
#define MSGTR_EncodeFileExists "Bestand bestaat reeds: %s (overschrijf uw favoriete AVI niet!)\n"
#define MSGTR_CantCreateEncodeFile "Kan bestand voor encoding niet creeren\n"
#define MSGTR_CannotInitVO "FATAL: Kan video driver niet initialiseren!\n"
#define MSGTR_CannotInitAO "Kon audio device niet open/init -> NOSOUND\n"
#define MSGTR_StartPlaying "Start afspelen...\n"
#define MSGTR_SystemTooSlow "\n*********************************************************************************"\
			    "\n** Je systeem is te TRAAG om dit af te spelen! Probeer met -framedrop of RTFM! **"\
			    "\n*********************************************************************************\n"

#define MSGTR_NoGui "MPlayer werd gecompileerd ZONDER GUI ondersteuning!\n"
#define MSGTR_GuiNeedsX "MPlayer GUI heeft X11 nodig!\n"
#define MSGTR_Playing "Bezig met het afspelen van %s\n"
#define MSGTR_NoSound "Audio: geen geluid!!!\n"
#define MSGTR_FPSforced "FPS geforceerd om %5.3f te zijn (ftime: %5.3f)\n"

// open.c, stream.c: 
#define MSGTR_CdDevNotfound "CD-ROM Device '%s' niet gevonden!\n"
#define MSGTR_ErrTrackSelect "Fout bij het selecteren van VCD track!"
#define MSGTR_ReadSTDIN "Lezen van stdin...\n"
#define MSGTR_UnableOpenURL "Onmogelijk om URL te openen: %s\n"
#define MSGTR_ConnToServer "Verbonden met server: %s\n"
#define MSGTR_FileNotFound "Bestand niet gevonden: '%s'\n"

#define MSGTR_CantOpenDVD "Kon DVD device niet openen: %s\n"
#define MSGTR_DVDwait "Bezig met het lezen van de schijf structuur, gelieve te wachten...\n"
#define MSGTR_DVDnumTitles "Er zijn %d titels op deze DVD.\n"
#define MSGTR_DVDinvalidTitle "Foutieve DVD titelnummer: %d\n"
#define MSGTR_DVDnumChapters "Er zijn %d hoofdstukken in deze DVD titel.\n"
#define MSGTR_DVDinvalidChapter "Foutief DVD hoofdstuknummer: %d\n"
#define MSGTR_DVDnumAngles "Er zijn %d gezichtspunten in deze DVD titel.\n"
#define MSGTR_DVDinvalidAngle "Foutief DVD gezichtspuntnummer: %d\n"
#define MSGTR_DVDnoIFO "Kan het IFO bestand voor DVD titel %d niet openen.\n"
#define MSGTR_DVDnoVOBs "Kan titel VOBS niet openen (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD openen geslaagd!\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "Waarschuwing! Audio stream header %d hergedefinieerd!\n"
#define MSGTR_VideoStreamRedefined "Waarschuwing! Video stream header %d hergedefinieerd!\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: Te veel (%d in %d bytes) audio packets in de buffer!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: Te veel (%d in %d bytes) video packets in de buffer!\n"
#define MSGTR_MaybeNI "(misschien speel je een non-interleaved stream/bestand of werkte de codec niet)\n"
#define MSGTR_DetectedAVIfile "AVI bestandsformaat gedetecteerd!\n"
#define MSGTR_DetectedASFfile "ASF bestandsformaat gedetecteerd!\n"
#define MSGTR_DetectedMPEGPESfile "MPEG-PES bestandsformaat gedetecteerd!\n"
#define MSGTR_DetectedMPEGPSfile "MPEG-PS bestandsformaat gedetecteerd!\n"
#define MSGTR_DetectedMPEGESfile "MPEG-ES bestandsformaat gedetecteerd!\n"
#define MSGTR_DetectedQTMOVfile "QuickTime/MOV bestandsformaat gedetecteerd!\n"
#define MSGTR_MissingMpegVideo "Ontbrekende MPEG video stream!? Contacteer de auteur, het zou een bug kunnen zijn :(\n"
#define MSGTR_InvalidMPEGES "Invalid MPEG-ES stream??? Contacteer de auteur, het zou een bug kunnen zijn :(\n"
#define MSGTR_FormatNotRecognized "============= Sorry, dit bestandsformaat niet herkend/ondersteund ===============\n"\
				  "=== Als dit een AVI bestand, ASF bestand of MPEG stream is, contacteer dan aub de auteur! ===\n"
/*#define MSGTR_MissingASFvideo "ASF: Geen Video stream gevonden!\n"
#define MSGTR_MissingASFaudio "ASF: Geen Audio stream gevonden...  ->nosound\n"
#define MSGTR_MissingMPEGaudio "MPEG: Geen Audio stream gevonden...  ->nosound\n"
*/
#define MSGTR_MissingVideoStream "Geen video stream gevonden!\n"
#define MSGTR_MissingAudioStream "Geen audio stream gevonden...  ->nosound\n"
#define MSGTR_MissingVideoStreamBug "Ontbrekende video stream!? Contacteer de auteur, het zou een bug kunnen zijn :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: bestand bevat de geselecteerde audio- of videostream niet\n"

#define MSGTR_NI_Forced "Geforceerd"
#define MSGTR_NI_Detected "Gedetecteerd"
#define MSGTR_NI_Message "%s NON-INTERLEAVED AVI bestandsformaat!\n"

#define MSGTR_UsingNINI "NON-INTERLEAVED Broken AVI bestandsformaat wordt gebruikt!\n"
#define MSGTR_CouldntDetFNo "Kon het aantal frames niet bepalen (voor absolute verplaatsing)  \n"
#define MSGTR_CantSeekRawAVI "Kan niet in raw .AVI streams verplaatsen! (index nodig, probeer met de -idx optie!)  \n"
#define MSGTR_CantSeekFile "Kan niet verplaatsen in dit bestand!  \n"

#define MSGTR_EncryptedVOB "Geencrypteerd VOB bestand (niet gecompileerd met libcss ondersteuning)! Lees DOCS/DVD bestand\n"
#define MSGTR_EncryptedVOBauth "Geencrypteerde stream maar autenticatie was niet aangevraagd door u!!\n"

#define MSGTR_MOVcomprhdr "MOV: Gecomprimeerde headers (nog) niet ondersteund!\n"
#define MSGTR_MOVvariableFourCC "MOV: Waarschuwing! variabele FOURCC gedetecteerd!?\n"
#define MSGTR_MOVtooManyTrk "MOV: Waarschuwing! te veel tracks!"
#define MSGTR_MOVnotyetsupp "\n****** Quicktime MOV formaat nog niet ondersteund!!!!!!! *******\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "kon codec niet openen\n"
#define MSGTR_CantCloseCodec "kon codec niet sluiten\n"

#define MSGTR_MissingDLLcodec "FOUT: Kon de nodige DirectShow codec niet openen: %s\n"
#define MSGTR_ACMiniterror "Kon jWin32/ACM AUDIO codec niet laden/initialiseren (ontbrekend DLL bestand?)\n"
#define MSGTR_MissingLAVCcodec "Kan codec codec '%s' niet vinden in libavcodec...\n"

#define MSGTR_NoDShowSupport "MPlayer werd gecompileerd ZONDER directshow ondersteuning!\n"
#define MSGTR_NoWfvSupport "Ondersteuning voor win32 codecs uitgeschakeld, of niet beschikbaar op niet-x86 platformen!\n"
#define MSGTR_NoDivx4Support "MPlayer werd gecompileerd ZONDER DivX4Linux (libdivxdecore.so) ondersteuning!\n"
#define MSGTR_NoLAVCsupport "MPlayer werd gecompileerd ZONDER ffmpeg/libavcodec ondersteuning!\n"
#define MSGTR_NoACMSupport "Win32/ACM audio codec uitgeschakeld, of niet beschikbaar op niet-x86 CPU -> forceer nosound :(\n"
#define MSGTR_NoDShowAudio "Gecompileerd zonder DirectShow ondersteuning -> forceer nosound :(\n"
#define MSGTR_NoOggVorbis "OggVorbis audio codec uigeschakeld -> forceer nosound :(\n"

#define MSGTR_MpegPPhint "WAARSCHUWING! Je vroeg beeld postprocessing aan voor een MPEG 1/2 video,\n" \
			 "         maar compileerde MPlayer zonder MPEG 1/2 postprocessing ondersteuning!\n" \
			 "         #define MPEG12_POSTPROC in config.h, en hercompileer libmpeg2!\n"
#define MSGTR_MpegNoSequHdr "MPEG: FATAAL: EOF tijdens het zoeken naar sequence header\n"
#define MSGTR_CannotReadMpegSequHdr "FATAAL: Kan sequence header niet lezen!\n"
#define MSGTR_CannotReadMpegSequHdrEx "FATAAL: Kan sequence header extension niet lezen!\n"
#define MSGTR_BadMpegSequHdr "MPEG: Foutieve sequence header!\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: Foutieve sequence header extension!\n"

#define MSGTR_ShMemAllocFail "Kan gedeeld geheugen niet alloceren\n"
#define MSGTR_CantAllocAudioBuf "Kan audio uitvoer buffer niet alloceren\n"
#define MSGTR_NoMemForDecodedImage "niet genoeg geheugen voor buffer voor gedecodeerd beeld (%ld bytes)\n"

#define MSGTR_AC3notvalid "AC3 stream ongeldig.\n"
#define MSGTR_AC3only48k "Alleen 48000 Hz streams ondersteund.\n"
#define MSGTR_UnknownAudio "Onbekend/ontbrekend audio formaat, gebruik -nosound\n"

// LIRC:
#define MSGTR_SettingUpLIRC "Bezig met configuratie van lirc ondersteuning...\n"
#define MSGTR_LIRCdisabled "Je zal je afstandsbediening niet kunnen gebruiken\n"
#define MSGTR_LIRCopenfailed "Openen van lirc ondersteuning mislukt!\n"
#define MSGTR_LIRCsocketerr "Er is iets mis met de lirc socket: %s\n"
#define MSGTR_LIRCcfgerr "Lezen van LIRC config bestand mislukt %s !\n"


// ====================== GUI messages/buttons ========================


//#define MSGTR_

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "Info over"
#define MSGTR_FileSelect "Selecteer bestand ..."
#define MSGTR_MessageBox "MessageBox"
#define MSGTR_PlayList "AfspeelLijst"
#define MSGTR_SkinBrowser "Skin Browser"

// --- buttons ---
#define MSGTR_Ok "Ok"
#define MSGTR_Cancel "Annuleer"
#define MSGTR_Add "Toevoegen"
#define MSGTR_Remove "Verwijderen"

// --- error messages ---
#define MSGTR_NEMDB "Sorry, niet genoeg geheugen voor tekenbuffer."
#define MSGTR_NEMFMR "Sorry, niet genoeg geheugen voor menu rendering."
#define MSGTR_NEMFMM "Sorry, niet genoeg geheugen voor hoofdvenster shape mask."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[skin] fout skin config bestand op regel %d: %s" 
#define MSGTR_SKIN_WARNING1 "[skin] waarschuwing in skin config bestand op regel %d: widget gevonden maar voordien \"section\" niet gevonden ( %s )"
#define MSGTR_SKIN_WARNING2 "[skin] waarschuwing in skin config bestand op regel %d: widget gevonden maar voordien \"subsection\" niet gevonden (%s)"
#define MSGTR_SKIN_BITMAP_16bit  "16 bits of minder kleurendiepte bitmap niet ondersteund ( %s ).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "bestand niet gevonden ( %s )\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "bmp lees fout ( %s )\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "tga lees fout ( %s )\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "png lees fout ( %s )\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "RLE packed tga niet ondersteund ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "onbekend bestandstype ( %s )\n"
#define MSGTR_SKIN_BITMAP_ConvertError "24 bit to 32 bit convert error ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "onbekende boodschap: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "onvoldoende geheugen\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "te veel fonts gedeclareerd\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "font bestand niet gevonden\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "font image bestand niet gevonden\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "onbestaande font identifier ( %s )\n"
#define MSGTR_SKIN_UnknownParameter "onbekende parameter ( %s )\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[skinbrowser] onvoldoende geheugen.\n"

#endif


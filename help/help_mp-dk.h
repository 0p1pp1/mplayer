// Translated by:  Anders Rune Jensen <anders@gnulinux.dk>
// S�rlig tak til: Tomas Groth <tomasgroth@hotmail.com>
//                 Dan Christiansen <danchr@daimi.au.dk>
// Sync'ed with help_mp-en.h 1.105


// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"Benyt:   mplayer [indstillinger] [URL|sti/]filnavn\n"
"\n"
"Basale indstillinger (se manualen for en komplet liste):\n"
" -vo <drv[:enhed]> v�lg videodriver og enhed (detaljer, se '-vo help')\n"
" -ao <drv[:enhed]> v�lg lyddriver og enhed (detaljer, se '-ao help')\n"
#ifdef HAVE_VCD
" vcd://<spor>  afspil et VCD (Video CD) spor fra et drev i stedet for en fil\n"
#endif
#ifdef USE_DVDREAD
" dvd://<titelnr> afspil DVD titel fra et drev i stedet for en fil\n"
" -alang/-slang   v�lg sprog til lyd og undertekster (vha. landekode p� 2 tegn)\n"
#endif
" -ss <tidspos>   s�g til en given position (sekund eller hh:mm:ss)\n"
" -nosound        sl� lyd fra\n"
" -fs             afspil i fuldsk�rm (el. -vm, -zoom, se manualen)\n"
" -x <x> -y <y>   sk�rmopl�sning til -vm eller -zoom)\n"
" -sub <fil>      angiv fil med undertekster (se ogs� -subfps, -subdelay)\n"
" -playlist <fil> angiv afspilningsliste\n"
" -vid x -aid y   v�lg filmspor (x) og lydspor (y)\n"
" -fps x -srate y s�t billedfrekvensen til x billeder pr. sekund og lydfrekvensen til y Hz\n"
" -pp <kvalitet>  benyt efterbehandlingsfiltre (detaljer, se manualen)\n"
" -framedrop      spring enkelte billeder over hvis n�dvendigt (til langsomme maskiner)\n"
"\n"
"Basale taster: (se manualen for en fuldst�ndig liste, check ogs� input.conf)\n"
" <-  eller  ->   s�g 10 sekunder frem eller tilbage\n"
" up eller down   s�g 1 minut frem eller tilbage \n"
" pgup el. pgdown s�g 10 minutter frem eller tilbage\n"
" < eller >       s�g frem eller tilbage i afspilningslisten\n"
" p eller SPACE   pause filmen (starter igen ved tryk p� en vilk�rlig tast)\n"
" q eller ESC     stop afspilning og afslut program\n"
" + eller -       juster lydens forsinkelse med +/- 0.1 sekundt\n"
" o               v�lg OSD type:  ingen / s�gebj�lke / s�gebj�lke+tid\n"
" * eller /       juster lydstyrken op og ned\n"
" z eller x       tilpas underteksters forsinkelse med +/- 0.1 sekund\n"
" r eller t       tilpas underteksters position op/ned, se ogs� -vf expand\n"
"\n"
" * * * SE MANUALEN FOR DETALJER, FLERE (AVANCEREDE) MULIGHEDER OG TASTER * * *\n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// mplayer.c: 

#define MSGTR_Exiting "\n Afslutter...\n"
#define MSGTR_ExitingHow "\n Afslutter... (%s)\n"
#define MSGTR_Exit_quit "Afslut"
#define MSGTR_Exit_eof "Slut p� filen"
#define MSGTR_Exit_error "Fatal fejl"
#define MSGTR_IntBySignal "\nMPlayer afbrudt af signal %d i modul: %s \n"
#define MSGTR_NoHomeDir "Kunne ikke finde hjemmekatalog\n"
#define MSGTR_GetpathProblem "get_path(\"config\") problem\n"
#define MSGTR_CreatingCfgFile "Genererer konfigurationsfil: %s\n"
#define MSGTR_InvalidVOdriver "Ugyldig videodriver: %s\nBrug '-vo help' for at f� en komplet liste over gyldige videodrivere.\n"
#define MSGTR_InvalidAOdriver "Ugyldig lyddriver: %s\nBrug '-ao help' for at f� en komplet liste over gyldige lyddrivere.\n"
#define MSGTR_CopyCodecsConf "(kopier/link etc/codecs.conf (fra MPlayer kilde katalog) til ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "Benytter indbyggede standardv�rdier for codecs.conf\n"
#define MSGTR_CantLoadFont "Kunne ikke indl�se skrifttype: %s\n"
#define MSGTR_CantLoadSub "Kunne ikke indl�se undertekstfil: %s\n"
#define MSGTR_ErrorDVDkey "Fejl under behandling af DVD-n�gle.\n"
#define MSGTR_CmdlineDVDkey "Den valgte DVD-n�gle vil blive anvendt til dekryptering.\n"
#define MSGTR_DVDauthOk "DVD-verifikationssekvens synes OK.\n"
#define MSGTR_DumpSelectedStreamMissing "dump: FATALT: Kunne ikke finde det valgte spor!\n"
#define MSGTR_CantOpenDumpfile "Kunne ikke �bne dump filen.\n"
#define MSGTR_CoreDumped "kernen dumpede ;)\n"
#define MSGTR_FPSnotspecified "Billedfrekvensen er enten ikke angivet i filen eller ugyldig. Brug -fps!\n"
#define MSGTR_TryForceAudioFmtStr "Gennemtvinger lyd-codec driverfamilie %s...\n"
#define MSGTR_CantFindAfmtFallback "Kunne ikke finde lyd-codec for driverfamilien, pr�ver en anden driver.\n"
#define MSGTR_CantFindAudioCodec "Kunne ikke finde codec til lydformatet 0x%X!\n"
#define MSGTR_CouldntInitAudioCodec "Kunne ikke initialisere lyd-codec -> ingen lyd\n"
#define MSGTR_TryForceVideoFmtStr "Gennemtvinger video-codec driver familie %s...\n"
#define MSGTR_CantFindVideoCodec "Kunne ikke finde video-codec til den valgte -vo og formatet 0x%X!\n"
#define MSGTR_VOincompCodec "Beklager, den valgte video-driverenhed er ikke kompatibel med dette codec.\n"
#define MSGTR_CannotInitVO "FATAL: Kunne ikke initialisere videodriveren\n"
#define MSGTR_CannotInitAO "Kunne ikke �bne/initialisere lydkortet -> INGEN LYD\n"
#define MSGTR_StartPlaying "P�begynder afspilning...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"      ************************************************************\n"\
"      **** Din computer er for LANGSOM til at afspille dette! ****\n"\
"      ************************************************************\n\n"\
"Mulige �rsager, problemer, l�sninger:\n"\
"- Oftest: d�rlig driver til lydkort\n"\
"  - Pr�v -ao sdl eller brug ALSA 0.5 eller OSS emulation af ALSA 0.9.\n"\
"  - Experimenter med forskellige v�rdier for -autosync, 30 er en god start.\n"\
"- Langsom video output\n"\
"  - Pr�v en anden -vo driver (se -vo help) eller pr�v med -framedrop!\n"\
"- Langsom processor\n"\
"  - Undlad at afspille h�jopl�snings DVD eller DivX p� en langsom processor! Pr�v -hardframedrop.\n"\
"- �delagt fil\n"\
"  - Pr�v med forskellige sammens�tninger af -nobps -ni -forceidx -mc 0.\n"\
"- Langsomt medie (NFS/SMB, DVD, VCD osv.)\n"\
"  - Pr�v -cache 8192.\n"\
"- Bruger du -cache til at afspille en non-interleaved AVI fil?\n"\
"  - Pr�v -nocache.\n"\
"Se DOCS/HTML/en/devices.html for tuning/optimerings tips.\n"\
"Hvis intet af dette hjalp, s� l�s DOCS/HTML/en/bugreports.html!\n\n"

#define MSGTR_NoGui "MPlayer blev kompileret UDEN grafisk brugergr�nseflade!\n"
#define MSGTR_GuiNeedsX "MPlayer grafisk brugergr�nseflade kr�ver X11!\n"
#define MSGTR_Playing "Afspiller %s\n"
#define MSGTR_NoSound "Lyd: ingen lyd\n"
#define MSGTR_FPSforced "Billedfrekvens sat til %5.3f  (ftime: %5.3f)\n"
#define MSGTR_CompiledWithRuntimeDetection "Kompileret med dynamisk processoroptimering - NB: dette er ikke optimalt! For at f� den bedre ydelse kompiler MPlayer fra kildekode med --disable-runtime-cpudetection\n"
#define MSGTR_CompiledWithCPUExtensions "Kompileret til x86 CPU med udvidelser:"
#define MSGTR_AvailableVideoOutputPlugins "Tilg�ngelige videoudvidelser:\n"
#define MSGTR_AvailableVideoOutputDrivers "Tilg�ngelige videodrivere:\n"
#define MSGTR_AvailableAudioOutputDrivers "Tilg�ngelige lyddrivere:\n"
#define MSGTR_AvailableAudioCodecs "Tilg�ngelige lyd-codecs:\n"
#define MSGTR_AvailableVideoCodecs "Tilg�ngelige video-codecs:\n"
#define MSGTR_AvailableAudioFm "\nTilg�ngelige (pr�kompilerede) lyd-codec familier/drivere:\n"
#define MSGTR_AvailableVideoFm "\nTilg�ngelige (pr�kompilerede) video-codec familier/drivere:\n"
#define MSGTR_AvailableFsType "Tilg�ngelige fuldsk�rmstilstande:\n"
#define MSGTR_UsingRTCTiming "Benytter Linux' hardware RTC timer (%ldHz)\n"
#define MSGTR_CannotReadVideoProperties "Video: Kunne ikke l�se egenskaber\n"
#define MSGTR_NoStreamFound "Ingen spor fundet\n"
#define MSGTR_InitializingAudioCodec "Initialiserer lyd-codec...\n"
#define MSGTR_ErrorInitializingVODevice "Fejl under �bning/initialisering af den valgte videodriver (-vo)!\n"
#define MSGTR_ForcedVideoCodec "Gennemtvunget video-codec: %s\n"
#define MSGTR_ForcedAudioCodec "Gennemtvunget lyd-codec: %s\n"
#define MSGTR_AODescription_AOAuthor "AO: Beskrivelse: %s\nAO: forfatter: %s\n"
#define MSGTR_AOComment "AO: Kommentar: %s\n"
#define MSGTR_Video_NoVideo "Video: ingen video\n"
#define MSGTR_NotInitializeVOPorVO "\nFATALT: Kunne ikke initialisere videofiltre (-vf) eller videodriver (-vo)!\n"
#define MSGTR_Paused "\n  =====  PAUSE  =====\r"
#define MSGTR_PlaylistLoadUnable "\nKunne ikke indl�se afspilningslisten %s\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- MPlayer fik en alvorlig fejl af typen 'ulovlig instruktion'.\n"\
"  Det kan v�re en fejl i den nye dynamiske processoroptimeringskode...\n"\
"  Se DOCS/HTML/en/bugreports.html.\n"
#define MSGTR_Exit_SIGILL \
"- MPlayer fik en alvorlig fejl af typen 'ulovlig instruktion'.\n"\
"  Dette sker oftest kun hvis du k�rer p� en processor forskellig fra den\n"\
"  MPlayer var kompileret til.\n Check venligst dette!\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- MPlayer fik en alvorlig fejl pga. forkert brug af CPU/FPU/RAM.\n"\
"  Rekompiler MPlayer med --enable-debug og lav et 'gdb' backtrace og\n"\
"  disassemling. For detaljer l�s venligst DOCS/HTML/en/bugreports_what.html#bugreports_crash.b.\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer fik en alvorlig fejl af ukendt type. Dette burde ikke ske.\n"\
"  Det kan v�re en fejl i MPlayer koden _eller_ i andre drivere _ eller_ i \n"\
"  den version af gcc du k�rer. Hvis du tror det er en fejl i MPlayer l�s da \n"\
"  venligst DOCS/HTML/en/bugreports.html og f�lg instruktionerne der. Vi kan ikke \n"\
"  og vil ikke hj�lpe medmindre du f�lger instruktionerne n�r du rapporterer \n"\
"  en mulig fejl.\n"


// mencoder.c:

#define MSGTR_UsingPass3ControllFile "Benytter 3. pass kontrolfilen: %s\n"
#define MSGTR_MissingFilename "\nFilnavn mangler\n\n"
#define MSGTR_CannotOpenFile_Device "Kunne ikke �bne fil/enhed\n"
#define MSGTR_ErrorDVDAuth "Fejl i DVD-verificering...\n"
#define MSGTR_CannotOpenDemuxer "Kunne ikke �bne demuxer\n"
#define MSGTR_NoAudioEncoderSelected "\nIngen lydenkoder (-oac) valgt! V�lg en eller brug -nosound. Se ogs� -oac help!\n"
#define MSGTR_NoVideoEncoderSelected "\nIngen videoenkoder (-ovc) valgt! V�lg en, se -ovc help!\n"
#define MSGTR_InitializingAudioCodec "Initialiserer lyd-codec...\n"
#define MSGTR_CannotOpenOutputFile "Kunne ikke �bne '%s' til skrivning\n"
#define MSGTR_EncoderOpenFailed "Kunne ikke �bne enkoderen\n"
#define MSGTR_ForcingOutputFourcc "Tvinger udgangskode (fourcc) til %x [%.4s]\n"
#define MSGTR_WritingAVIHeader "Skriver AVI header...\n"
#define MSGTR_DuplicateFrames "\n%d ens billede(r)!!!    \n"
#define MSGTR_SkipFrame "\nSpringer over billede!!!    \n"
#define MSGTR_ErrorWritingFile "%s: Fejl ved skrivning til fil.\n"
#define MSGTR_WritingAVIIndex "\nSkriver AVI index...\n"
#define MSGTR_FixupAVIHeader "Fixup AVI header...\n"
#define MSGTR_RecommendedVideoBitrate "Anbefalet video bitrate til %s CD: %d\n"
#define MSGTR_VideoStreamResult "\nVideospor: %8.3f kbit/s  (%d bps)  st�rrelse: %d bytes  %5.3f sek.  %d billeder\n"
#define MSGTR_AudioStreamResult "\nAudiospor: %8.3f kbit/s  (%d bps)  st�rrelse: %d bytes  %5.3f sek.\n"

// cfg-mencoder.h:

#define MSGTR_MEncoderMP3LameHelp "\n\n"\
" vbr=<0-4>     bitratemetode (vbr)\n"\
"                0: cbr\n"\
"                1: mt\n"\
"                2: rh(default)\n"\
"                3: abr\n"\
"                4: mtrh\n"\
"\n"\
" abr           gennemsnitlig bitrate\n"\
"\n"\
" cbr           konstant bitrate\n"\
"               Gennemtvinger ogs� CBR p� efterf�lgende ABR indstilligner\n"\
"\n"\
" br=<0-1024>   specificer bitrate i kBit (kun CBR og ABR)\n"\
"\n"\
" q=<0-9>       kvalitet (0-h�jest, 9-lavest) (kun VBR)\n"\
"\n"\
" aq=<0-9>      algoritmisk kvalitet (0-bedst/langsomst, 9-v�rst/hurtigst)\n"\
"\n"\
" ratio=<1-100> komprimeringsforhold\n"\
"\n"\
" vol=<0-10>    indstil lydstyrke\n"\
"\n"\
" mode=<0-3>    (standard: auto)\n"\
"                0: stereo\n"\
"                1: joint-stereo\n"\
"                2: dobbelt mono\n"\
"                3: mono\n"\
"\n"\
" padding=<0-2>\n"\
"                0: ingen\n"\
"                1: alle\n"\
"                2: justeret\n"\
"\n"\
" fast          v�lg hastighed fremfor kvalitet i efterf�lgende VBR indstillinger,\n"\
"               giver lavere kvalitet og h�jere bitrate.\n"\
"\n"\
" preset=<value> tunede indstillinger til h�j kvalitet.\n"\
"                 medium: VBR,  god  kvalitet\n"\
"                 (150-180 kbps bitrate interval)\n"\
"                 standard: VBR, h�j kvalitet\n"\
"                 (170-210 kbps bitrate interval)\n"\
"                 extreme: VBR, meget h�j kvalitet\n"\
"                 (200-240 kbps bitrate interval)\n"\
"                 insane:  CBR, h�jeste pr�sets kvalitet\n"\
"                 (320 kbps bitrate)\n"\
"                 <8-320>: ABR med den angivne, gennemsnitlige bitrate.\n\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "CD-ROM drev '%s' ikke fundet!\n"
#define MSGTR_ErrTrackSelect "Fejl i valg af VCD nummer!"
#define MSGTR_ReadSTDIN "L�ser fra stdin...\n"
#define MSGTR_UnableOpenURL "Kunne ikke �bne adressen: %s\n"
#define MSGTR_ConnToServer "Forbundet til serveren: %s\n"
#define MSGTR_FileNotFound "Filen blev ikke fundet: '%s'\n"

#define MSGTR_SMBInitError "Kunne ikke initialisere libsmbclient bibliotek: %d\n"
#define MSGTR_SMBFileNotFound "Kunne ikke �bne netv�rksadressen '%s'\n"
#define MSGTR_SMBNotCompiled "MPlayer er ikke blevet kompileret med SMB l�se-underst�ttelse\n"

#define MSGTR_CantOpenDVD "Kunne ikke �bne DVD drev: %s\n"
#define MSGTR_DVDwait "Indl�ser diskstruktur, vent venligst...\n"
#define MSGTR_DVDnumTitles "Der er %d titler p� denne DVD.\n"
#define MSGTR_DVDinvalidTitle "Ugyldig DVD-titel: %d\n"
#define MSGTR_DVDnumChapters "Der er %d kapitler i denne DVD-titel.\n"
#define MSGTR_DVDinvalidChapter "Ugyldigt DVD-kapitel: %d\n"
#define MSGTR_DVDnumAngles "Der er %d vinkler i denne DVD-titel.\n"
#define MSGTR_DVDinvalidAngle "Ugyldig DVD-vinkel: %d\n"
#define MSGTR_DVDnoIFO "Kunne ikke finde IFO filen for DVD-titel %d.\n"
#define MSGTR_DVDnoVOBs "Kunne ikke �bne titlen VOBS (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD indl�st!\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "Advarsel! Lydfilens header %d er blevet omdefineret!\n"
#define MSGTR_VideoStreamRedefined "Advarsel! Videofilens header %d er blevet omdefineret!\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: For mange (%d i %d bytes) lydpakker i bufferen!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: For mange (%d i %d bytes) videopakker i bufferen!\n"
#define MSGTR_MaybeNI "M�ske afspiller du en 'ikke-interleaved' stream/fil ellers der kan v�re en fejl i afspilleren\n"\
		      "For AVI filer, pr�v at p�tvinge non-interleaved tilstand med -ni.\n"
#define MSGTR_SwitchToNi "\nDefekt .AVI - skifter til ikke-interleaved (-ni)...\n"
#define MSGTR_Detected_XXX_FileFormat "Filformat er %s\n"
#define MSGTR_DetectedAudiofile "Filen er en lydfil!\n"
#define MSGTR_NotSystemStream "Ikke MPEG System Stream format... (m�ske Transport Stream?)\n"
#define MSGTR_InvalidMPEGES "Ugyldig MPEG-ES stream??? Rapporter venligst dette, det kunne v�re en fejl i programmet :(\n"
#define MSGTR_FormatNotRecognized "============ Desv�rre, dette filformat kunne ikke genkendes =================\n"\
"=== Er denne fil af typen AVI, ASF eller MPEG, s� rapporter venligst dette, det kan skyldes en fejl. ==\n"
#define MSGTR_MissingVideoStream "Ingen videospor fundet.\n"
#define MSGTR_MissingAudioStream "Ingen lydspor fundet -> ingen lyd\n"
#define MSGTR_MissingVideoStreamBug "Manglende videospor!? Rapporter venligst dette, det kunne skyldes en fejl i programmet :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: filen indeholder ikke det angivne lyd- eller videospor\n"

#define MSGTR_NI_Forced "Gennemtvang"
#define MSGTR_NI_Detected "Opdagede"
#define MSGTR_NI_Message "%s IKKE-INTERLEAVED AVI fil-format!\n"

#define MSGTR_UsingNINI "Bruger IKKE-INTERLEAVED (-ni), <F8>delagt AVI fil-format!\n"
#define MSGTR_CouldntDetFNo "Kunne ikke beregne antallet af billeder (til s�gning)  \n"
#define MSGTR_CantSeekRawAVI "S�gning i r� AVI-filer ikke mulig. (Index kr�ves, pr�v -idx.)  \n"
#define MSGTR_CantSeekFile "Kan ikke s�ge i denne fil.\n"

#define MSGTR_MOVcomprhdr "MOV: Komprimerede headers (endnu) ikke underst�ttet!\n"
#define MSGTR_MOVvariableFourCC "MOV: Advarsel! variabel FOURCC!?\n"
#define MSGTR_MOVtooManyTrk "MOV: Advarsel! For mange spor"
#define MSGTR_FoundAudioStream "==> Fandt lydspor: %d\n"
#define MSGTR_FoundVideoStream "==> Fandt videospor: %d\n"
#define MSGTR_DetectedTV "TV genkendt! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "Kan ikke �bne ogg demuxe.r\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: S�ger efter lydspor (id:%d)\n"
#define MSGTR_CannotOpenAudioStream "Kan ikke �bne lydsspor: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "Kan ikke �bne spor %s af underteksterne\n"
#define MSGTR_OpeningAudioDemuxerFailed "Kan ikke �bne lyddemuxer: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "Kunne ikke �bne undertekstsdemuxer: %s\n"
#define MSGTR_TVInputNotSeekable "TV input er ikke s�gbart! (Kunne v�re du skulle skifte kanal ;)\n"
#define MSGTR_DemuxerInfoAlreadyPresent "Demuxer info %s findes allerede!\n"
#define MSGTR_ClipInfo "Klip info: \n"


// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "kunne ikke �bne codec\n"
#define MSGTR_CantCloseCodec "kunne ikke afslutte codec\n"

#define MSGTR_MissingDLLcodec "FEJL: Kunne ikke �bne DirectShow codec: %s\n"
#define MSGTR_ACMiniterror "Kunne ikke loade/initialisere Win32/ACM lyd-codec (manglende DLL fil?)\n"
#define MSGTR_MissingLAVCcodec "Kunne ikke finde codec '%s' i libavcodec...\n"

#define MSGTR_MpegNoSequHdr "MPEG: FATAL: EOF under s�gning efter sekvensheader\n"
#define MSGTR_CannotReadMpegSequHdr "FATAL: Kunne ikke l�se sekvensheader!\n"
#define MSGTR_CannotReadMpegSequHdrEx "FATAL: Kunne ikke l�se sekvensheaderudvidelse!\n"
#define MSGTR_BadMpegSequHdr "MPEG: Ugyldig sekvensheader!\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: Ugyldig sekvensheaderudvidelse!\n"

#define MSGTR_ShMemAllocFail "Kunne ikke allokere delt ram\n"
#define MSGTR_CantAllocAudioBuf "Kunne ikke allokere lyd buffer\n"

#define MSGTR_UnknownAudio "Ukendt/manglende lyd format, sl�r over til ingen lyd\n"

#define MSGTR_UsingExternalPP "[PP] Benytter ekstern efterprocesseringsfilter, max q = %d\n"
#define MSGTR_UsingCodecPP "[PP] Benytter codec's efterprocessering, max q = %d\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "Video egenskab '%s' ikke underst�ttet af den valgte vo & vd! \n"
#define MSGTR_VideoCodecFamilyNotAvailableStr "Anmodede video-codec familie [%s] (vfm=%s) ikke tilg�ngelig (aktiver f�r kompilering!)\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "Anmodede lyd-codec familie [%s] (afm=%s) ikke tilg�ngelig (aktiver f�r kompilering!)\n"
#define MSGTR_OpeningVideoDecoder "�bner videodekoder: [%s] %s\n"
#define MSGTR_OpeningAudioDecoder "�bner audiodekoder: [%s] %s\n"
#define MSGTR_UninitVideoStr "deinit video: %s  \n"
#define MSGTR_UninitAudioStr "deinit audio: %s  \n"
#define MSGTR_VDecoderInitFailed "Videodekoder initialisering fejlede :(\n"
#define MSGTR_ADecoderInitFailed "Lyddekoder initialisering fejlede :(\n"
#define MSGTR_ADecoderPreinitFailed "Lyddekoder pr�initialisering fejlede :(\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: Allokerer %d bytes til input buffer\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: Allokerer %d + %d = %d bytes til output buffer\n"

// LIRC:
#define MSGTR_SettingUpLIRC "S�tter LIRC underst�ttelse op...\n"
#define MSGTR_LIRCdisabled "Du vil ikke v�re i stand til at bruge din fjernbetjening\n"
#define MSGTR_LIRCopenfailed "Ingen lirc underst�ttelse fundet!\n"
#define MSGTR_LIRCcfgerr "Kunne ikke l�se LIRC konfigurationsfil %s!\n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "Kunne ikke finde videofilter '%s'\n"
#define MSGTR_CouldNotOpenVideoFilter "Kunne ikke �bne videofilter '%s'\n"
#define MSGTR_OpeningVideoFilter "�bner videofilter: "
#define MSGTR_CannotFindColorspace "Kunne ikke finde f�lles colorspace, selv med 'scale' :(\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDek: codec satte ikke sh->disp_w og sh->disp_h, pr�ver en anden l�sning!\n"
#define MSGTR_VoConfigRequest "VDek: vo konfig. anmodning - %d x %d (foretrukket csp: %s)\n"
#define MSGTR_CouldNotFindColorspace "Kunne ikke finde colorspace som matcher - pr�ver med -vf scale...\n"
#define MSGTR_MovieAspectIsSet "St�rrelsesforhold er %.2f:1 - pr�skalerer for at rette st�rrelsesforholdet.\n"
#define MSGTR_MovieAspectUndefined "St�rrelsesforholdet er ikke defineret - ingen pr�skalering benyttet.\n"

// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "Om"
#define MSGTR_FileSelect "V�lg fil..."
#define MSGTR_SubtitleSelect "V�lg undertekst-fil..."
#define MSGTR_OtherSelect "V�lg..."
#define MSGTR_AudioFileSelect "V�lg ekstern lydkanal..."
#define MSGTR_FontSelect "V�lg font..."
#define MSGTR_PlayList "Afspilningsliste"
#define MSGTR_Equalizer "Equalizer"
#define MSGTR_SkinBrowser "V�lg tema"
#define MSGTR_Network "Netv�rksstreaming..."
#define MSGTR_Preferences "Indstillinger"
#define MSGTR_OSSPreferences "OSS driver konfiguration"
#define MSGTR_SDLPreferences "SDL driver konfiguration"
#define MSGTR_NoMediaOpened "Medie ikke �bnet"
#define MSGTR_VCDTrack "VCD nummer %d"
#define MSGTR_NoChapter "Ingen kapitel"
#define MSGTR_Chapter "kapitel %d"
#define MSGTR_NoFileLoaded "Ingen fil indl�st"

// --- buttons ---
#define MSGTR_Ok "Ok"
#define MSGTR_Cancel "Annuller"
#define MSGTR_Add "Tilf�j"
#define MSGTR_Remove "Fjern"
#define MSGTR_Clear "Nulstil"
#define MSGTR_Config "Konfigurer"
#define MSGTR_ConfigDriver "Konfigurer driver"
#define MSGTR_Browse "Gennemse"

// --- error messages ---
#define MSGTR_NEMDB "Desv�rre, ikke nok ram til at vise bufferen."
#define MSGTR_NEMFMR "Desv�rre, ikke nok ram til at vise menuen."
#define MSGTR_IDFGCVD "Desv�rre, kunne ikke finde GUI kompabitel video driver."
#define MSGTR_NEEDLAVCFAME "For at afspille ikke-mpeg filer med dit DXR3/H+ skal du kode filmen igen.\nVenligst aktiver lavc eller fame i DXR3/H+ configboxen."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[tema] fejl i konfigurationsfilen til temaet p� linje %d: %s" 
#define MSGTR_SKIN_WARNING1 "[tema] advarsel i konfigurationsfilen til temaet p� linje %d: widget fundet men f�r \"section\" ikke fundet (%s)"
#define MSGTR_SKIN_WARNING2 "[tema] advarsel i konfigurationsfilen til temaet p� linje %d: widget fundet men f�r \"subsection\" ikke fundet (%s)"
#define MSGTR_SKIN_WARNING3 "[tema] advarsel i konfigurationsfilen til temaet p� linje %d: denne undersektion er ikke underst�ttet af dette widget (%s)"
#define MSGTR_SKIN_BITMAP_16bit  "16 bits eller mindre ikke underst�ttet (%s).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "filen ikke fundet (%s)\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "BMP l�se fejl (%s)\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "TGA l�se fejl (%s)\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "PNG l�se fejl (%s)\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "RLE pakket TGA ikke supporteret (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "ukendt filtype (%s)\n"
#define MSGTR_SKIN_BITMAP_ConvertError "Fejl i 24 bit to 32 bit convertering (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "ukendt besked: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "ikke nok ram\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "for mange skrifttyper specificeret\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "skriftypefilen ikke fundet\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "skrifttypebilled ikke fundet\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "ikke eksisterende font (%s)\n"
#define MSGTR_SKIN_UnknownParameter "ukendt parameter (%s)\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[temabrowser] ikke nok ram.\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "Tema blev ikke fundet (%s).\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "Tema config-fil l�se fejl (%s).\n"
#define MSGTR_SKIN_LABEL "Temaer:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "Om MPlayer"
#define MSGTR_MENU_Open "�ben..."
#define MSGTR_MENU_PlayFile "Afspil fil..."
#define MSGTR_MENU_PlayVCD "Afspil VCD..."
#define MSGTR_MENU_PlayDVD "Afspil DVD..."
#define MSGTR_MENU_PlayURL "Afspil URL..."
#define MSGTR_MENU_LoadSubtitle "Indl�s undertekst..."
#define MSGTR_MENU_DropSubtitle "Drop undertekst..."
#define MSGTR_MENU_LoadExternAudioFile "Indl�s extern lyd fil..."
#define MSGTR_MENU_Playing "Afspilning"
#define MSGTR_MENU_Play "Afspil"
#define MSGTR_MENU_Pause "Pause"
#define MSGTR_MENU_Stop "Stop"
#define MSGTR_MENU_NextStream "N�ste stream"
#define MSGTR_MENU_PrevStream "Forrige stream"
#define MSGTR_MENU_Size "St�rrelse"
#define MSGTR_MENU_NormalSize "Normal st�rrelse"
#define MSGTR_MENU_DoubleSize "Dobbelt st�rrelse"
#define MSGTR_MENU_FullScreen "Fuld sk�rm"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "Afspil disk..."
#define MSGTR_MENU_ShowDVDMenu "Vis DVD menu"
#define MSGTR_MENU_Titles "Titler"
#define MSGTR_MENU_Title "Titel %2d"
#define MSGTR_MENU_None "(ingen)"
#define MSGTR_MENU_Chapters "Kapitler"
#define MSGTR_MENU_Chapter "Kapitel %2d"
#define MSGTR_MENU_AudioLanguages "Lyd sprog"
#define MSGTR_MENU_SubtitleLanguages "Undertekst sprog"
#define MSGTR_MENU_PlayList "Afspilningslisten"
#define MSGTR_MENU_SkinBrowser "V�lg udseende"
#define MSGTR_MENU_Preferences "Indstillinger"
#define MSGTR_MENU_Exit "Forlad..."
#define MSGTR_MENU_Mute "Mute"
#define MSGTR_MENU_Original "Original"
#define MSGTR_MENU_AspectRatio "St�rrelsesforhold"
#define MSGTR_MENU_AudioTrack "Lydspor"
#define MSGTR_MENU_Track "Spor %d"
#define MSGTR_MENU_VideoTrack "Videospor"

// --- equalizer
#define MSGTR_EQU_Audio "Lyd"
#define MSGTR_EQU_Video "Video"
#define MSGTR_EQU_Contrast "Kontrast: "
#define MSGTR_EQU_Brightness "Lysstyrke: "
#define MSGTR_EQU_Hue "Farve: "
#define MSGTR_EQU_Saturation "M�tning: "
#define MSGTR_EQU_Front_Left "Venstre Front"
#define MSGTR_EQU_Front_Right "H�jre Front"
#define MSGTR_EQU_Back_Left "Venstre Bagh�jtaler"
#define MSGTR_EQU_Back_Right "H�jre Bagh�jtaler"
#define MSGTR_EQU_Center "Center"
#define MSGTR_EQU_Bass "Bass"
#define MSGTR_EQU_All "Alle"
#define MSGTR_EQU_Channel1 "Kanal 1:"
#define MSGTR_EQU_Channel2 "Kanal 2:"
#define MSGTR_EQU_Channel3 "Kanal 3:"
#define MSGTR_EQU_Channel4 "Kanal 4:"
#define MSGTR_EQU_Channel5 "Kanal 5:"
#define MSGTR_EQU_Channel6 "Kanal 6:"

// --- playlist
#define MSGTR_PLAYLIST_Path "Sti"
#define MSGTR_PLAYLIST_Selected "Valgte filer"
#define MSGTR_PLAYLIST_Files "Filer"
#define MSGTR_PLAYLIST_DirectoryTree "Katalog tr�"

// --- preferences
#define MSGTR_PREFERENCES_Audio "Lyd"
#define MSGTR_PREFERENCES_Video "Video"
#define MSGTR_PREFERENCES_SubtitleOSD "undertekster og OSD"
#define MSGTR_PREFERENCES_Codecs "Codecs & demuxer"
#define MSGTR_PREFERENCES_Misc "Forskelligt"

#define MSGTR_PREFERENCES_None "Ingen"
#define MSGTR_PREFERENCES_AvailableDrivers "Tilg�ngelige drivere:"
#define MSGTR_PREFERENCES_DoNotPlaySound "Afspil ikke lyd"
#define MSGTR_PREFERENCES_NormalizeSound "Normaliser lydstyrke"
#define MSGTR_PREFERENCES_EnEqualizer "Anvend equalizer"
#define MSGTR_PREFERENCES_ExtraStereo "Anvend extra stereo"
#define MSGTR_PREFERENCES_Coefficient "Koefficient:"
#define MSGTR_PREFERENCES_AudioDelay "Lydforsinkelse"
#define MSGTR_PREFERENCES_DoubleBuffer "Anvend double buffering"
#define MSGTR_PREFERENCES_DirectRender "Anvend 'direct rendering'"
#define MSGTR_PREFERENCES_FrameDrop "Anvend billed-skip"
#define MSGTR_PREFERENCES_HFrameDrop "Anvend meget billed-skip (farlig)"
#define MSGTR_PREFERENCES_Flip "Flip billede"
#define MSGTR_PREFERENCES_Panscan "Panscan: "
#define MSGTR_PREFERENCES_OSDTimer "Statuslinje og indikator"
#define MSGTR_PREFERENCES_OSDProgress "Kun statuslinje"
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "Timer, procent og total tid"
#define MSGTR_PREFERENCES_Subtitle "Undertekst:"
#define MSGTR_PREFERENCES_SUB_Delay "Forsinkelse: "
#define MSGTR_PREFERENCES_SUB_FPS "Billedfrekvens:"
#define MSGTR_PREFERENCES_SUB_POS "Position: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "Deaktiver automatisk undertekster"
#define MSGTR_PREFERENCES_SUB_Unicode "Unicode undertekst"
#define MSGTR_PREFERENCES_SUB_MPSUB "Konverter en given undertekst til MPlayer's undertekst format"
#define MSGTR_PREFERENCES_SUB_SRT "Konverter den angivne undertekst til et tidsbaseret SubViewer (SRT) format"
#define MSGTR_PREFERENCES_SUB_Overlap "sl� (til/fra) undertekst overlapning"
#define MSGTR_PREFERENCES_Font "Font:"
#define MSGTR_PREFERENCES_FontFactor "Font factor:"
#define MSGTR_PREFERENCES_PostProcess "Anvend efterprocesseringsfilter"
#define MSGTR_PREFERENCES_AutoQuality "Auto kvalitet: "
#define MSGTR_PREFERENCES_NI "Benyt non-interleaved AVI parser"
#define MSGTR_PREFERENCES_IDX "Genopbyg index tabel, hvis n�dvendig"
#define MSGTR_PREFERENCES_VideoCodecFamily "Video-codec familie:"
#define MSGTR_PREFERENCES_AudioCodecFamily "Lyd-codec familie:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "OSD level"
#define MSGTR_PREFERENCES_FRAME_Subtitle "Undertekst"
#define MSGTR_PREFERENCES_FRAME_Font "Skriftype"
#define MSGTR_PREFERENCES_FRAME_PostProcess "Efterprocesseringsfilter"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "Codec & demuxer"
#define MSGTR_PREFERENCES_FRAME_Cache "Cache"
#define MSGTR_PREFERENCES_FRAME_Misc "Misc"
#define MSGTR_PREFERENCES_OSS_Device "Enhed:"
#define MSGTR_PREFERENCES_OSS_Mixer "Mixer:"
#define MSGTR_PREFERENCES_SDL_Driver "Driver:"
#define MSGTR_PREFERENCES_Message "Husk, nogle funktioner kr�ver at MPlayer bliver genstartet for at de virker."
#define MSGTR_PREFERENCES_DXR3_VENC "Video enkoder:"
#define MSGTR_PREFERENCES_DXR3_LAVC "Brug LAVC (FFmpeg)"
#define MSGTR_PREFERENCES_DXR3_FAME "Brug FAME"
#define MSGTR_PREFERENCES_FontEncoding1 "Unicode"
#define MSGTR_PREFERENCES_FontEncoding2 "Vesteurop�riske sprog (ISO-8859-1)"
#define MSGTR_PREFERENCES_FontEncoding3 "Vesteurop�riske sprog med euro (ISO-8859-15)"
#define MSGTR_PREFERENCES_FontEncoding4 "�st-/Centraleurop�riske sprog (ISO-8859-2)"
#define MSGTR_PREFERENCES_FontEncoding5 "Esperanto, Galician, Maltisk, Tyrkisk (ISO-8859-3)"
#define MSGTR_PREFERENCES_FontEncoding6 "Gammel Baltisk tegns�t (ISO-8859-4)"
#define MSGTR_PREFERENCES_FontEncoding7 "Cyrillisk (ISO-8859-5)"
#define MSGTR_PREFERENCES_FontEncoding8 "Arabisk (ISO-8859-6)"
#define MSGTR_PREFERENCES_FontEncoding9 "Moderne Gr�sk (ISO-8859-7)"
#define MSGTR_PREFERENCES_FontEncoding10 "Tyrkisk (ISO-8859-9)"
#define MSGTR_PREFERENCES_FontEncoding11 "Baltisk (ISO-8859-13)"
#define MSGTR_PREFERENCES_FontEncoding12 "Keltisk (ISO-8859-14)"
#define MSGTR_PREFERENCES_FontEncoding13 "Hebr�isk tegns�t (ISO-8859-8)"
#define MSGTR_PREFERENCES_FontEncoding14 "Russisk (KOI8-R)"
#define MSGTR_PREFERENCES_FontEncoding15 "Ukrainsk, Belarusian (KOI8-U/RU)"
#define MSGTR_PREFERENCES_FontEncoding16 "Simplificeret kinesisk tegns�t (CP936)"
#define MSGTR_PREFERENCES_FontEncoding17 "Traditionel kinesisk tegns�t (BIG5)"
#define MSGTR_PREFERENCES_FontEncoding18 "Japansk tegns�t (SHIFT-JIS)"
#define MSGTR_PREFERENCES_FontEncoding19 "Koreansk tegns�t (CP949)"
#define MSGTR_PREFERENCES_FontEncoding20 "Thai tegns�t (CP874)"
#define MSGTR_PREFERENCES_FontEncoding21 "Cyrillic Windows (CP1251)"
#define MSGTR_PREFERENCES_FontEncoding22 "Slavisk/Central Europ�isk Windows (CP1250)"
#define MSGTR_PREFERENCES_FontNoAutoScale "Ingen autoskalering"
#define MSGTR_PREFERENCES_FontPropWidth "Proportional med film bredde"
#define MSGTR_PREFERENCES_FontPropHeight "Proportional med film h�jde"
#define MSGTR_PREFERENCES_FontPropDiagonal "Proportional med film diagonalt"
#define MSGTR_PREFERENCES_FontEncoding "Enkodning:"
#define MSGTR_PREFERENCES_FontBlur "Uskarp:"
#define MSGTR_PREFERENCES_FontOutLine "Omrids:"
#define MSGTR_PREFERENCES_FontTextScale "Text skalering:"
#define MSGTR_PREFERENCES_FontOSDScale "OSD skalering:"
#define MSGTR_PREFERENCES_Cache "Cache til/fra"
#define MSGTR_PREFERENCES_LoadFullscreen "Start i fullsk�rm"
#define MSGTR_PREFERENCES_CacheSize "Cache st�rrelse: "
#define MSGTR_PREFERENCES_SaveWinPos "Gem vinduets position"
#define MSGTR_PREFERENCES_XSCREENSAVER "Stop XScreenSaver"
#define MSGTR_PREFERENCES_PlayBar "Anvend afspilningsbar"
#define MSGTR_PREFERENCES_AutoSync "AutoSynk. til/fra"
#define MSGTR_PREFERENCES_AutoSyncValue "Autosynk.: "
#define MSGTR_PREFERENCES_CDROMDevice "CD-ROM drev:"
#define MSGTR_PREFERENCES_DVDDevice "DVD drev:"
#define MSGTR_PREFERENCES_FPS "Filmens billedfrekvens:"
#define MSGTR_PREFERENCES_ShowVideoWindow "Vis video vindue n�r inaktiv"

#define MSGTR_ABOUT_UHU "GUI udvikling sponsereret af UHU Linux\n"
#define MSGTR_ABOUT_CoreTeam "   MPlayer kernen:\n"
#define MSGTR_ABOUT_AdditionalCoders "   Yderligere kodere:\n"
#define MSGTR_ABOUT_MainTesters "   Hovedtestere:\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "Fatal fejl!"
#define MSGTR_MSGBOX_LABEL_Error "Fejl!"
#define MSGTR_MSGBOX_LABEL_Warning "Advarsel!" 

#endif

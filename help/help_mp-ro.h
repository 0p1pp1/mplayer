// FIXME: This needs to be redone properly.
// Partially sync'ed with help_mp-en.h $Revision$
// This is a retranslation of the file by Bogdan Butnaru <bogdanb@fastmail.fm>,
// based on the previous translation by Codre Adrian
// <codreadrian@softhome.net> (address bounces).
// The translation is partial and should be completed eventually, also it
// should be checked that messages are 80-column wrapped
//
// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"Folosire: mplayer [op�iuni] [url|cale/]numefi�ier\n"
"\n"
"Op�iuni principale: (lista complet� �n pagina man)\n"
" -vo <drv[:dev]>  alege driver-ul �i device-ul de ie�ire video\n"
"                  ('-vo help' pentru list�)\n"
" -ao <drv[:dev]>  alege driver-ul �i device-ul de ie�ire audio\n"
"                  ('-ao help' pentru list�)\n"

#ifdef HAVE_VCD
" vcd://<nrpist�>  ruleaz� pista VCD (Video CD) de pe device �n loc de fi�ier\n"
#endif

#ifdef USE_DVDREAD
" dvd://<nrtitlu>  ruleaz� titlul/pista de pe dispozitivul DVD �n loc de fi�ier\n"
" -aLMB/-sLMB      alege limba pentru audio/subtitr�ri DVD\n"
"                  (cu codul de 2 caractere, ex. RO)\n"
#endif
" -ss <timp>       deruleaz� la pozi�ia dat� (secunde sau hh:mm:ss)\n"
" -nosound         rulare f�r� sunet\n"
" -fs              afi�are pe tot ecranul (sau -vm, -zoom, detalii �n pagina man)\n"
" -x <x> -y <y>    alege rezolu�ia (folosit pentru -vm sau -zoom)\n"
" -sub <fi�ier>    specific� fi�ierul cu subtitr�ri folosit\n"
"                  (vezi �i -subfps, -subdelay)\n"
" -playlist <fi�>  specific� playlist-ul\n"
" -vid x -aid y    alege pista video (x) �i audio (y)\n"
" -fps x -srate y  schimb� rata video (x fps) �i audio (y Hz)\n"
" -pp <calitate>   activeaz� filtrul de postprocesare (detalii �n pagina man)\n"
" -framedrop       activeaz� s�ritul cadrelor (pentru calculatoare lente)\n"
"\n"
"Taste principale: (lista complet� �n pagina man, vezi �i input.conf)\n"
" <-  sau  ->      deruleaz� spate/fa�� 10 secunde\n"
" sus sau jos      deruleaz� spate/fa�� un minut\n"
" pgup or pgdown   deruleaz� spate/fa�� 10 minute\n"
" < or >           salt spate/fa�� �n playlist\n"
" p or SPACE       pauz� (ap�sa�i orice tast� pentru continuare)\n"
" q or ESC         opre�te filmul �i iese din program\n"
" + or -           modific� decalajul audio cu +/- 0,1 secunde\n"
" o                schimb� modul OSD �ntre: nimic / bar� derulare / bar� + ceas\n"
" * or /           cre�te sau scade volumul PCM\n"
" z or x           modific� decalajul subtitr�rii cu +/- 0,1 secunde\n"
" r or t           modific� pozi�ia subtitr�rii sus/jos, vezi �i -vf expand\n"
"\n"
" * * * VEZI PAGINA MAN PENTRU DETALII, ALTE OP�IUNI (AVANSATE) �I TASTE * * *\n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// mplayer.c:
#define MSGTR_Exiting "\nIe�ire...\n"
#define MSGTR_ExitingHow "\nIe�ire... (%s)\n"
#define MSGTR_Exit_quit "Ie�ire"
#define MSGTR_Exit_eof "Sf�r�it fi�ier"
#define MSGTR_Exit_error "Eroare fatal�"
#define MSGTR_IntBySignal "\nMPlayer a fost �ntrerupt de semnalul %d �n modulul: %s\n"
#define MSGTR_NoHomeDir "Nu g�sesc directorul HOME.\n"
#define MSGTR_GetpathProblem "get_path(\"config\") problem\n"
#define MSGTR_CreatingCfgFile "Creez fi�ierul de configurare: %s\n"\
	"�ncearc� '-vo help' pentru o list� cu driveri video disponibili.\n"
#define MSGTR_InvalidAOdriver "Numele driverului de ie�ire audio e gre�it: %s\n"\
	"Folose�te '-ao help' pentru lista cu driveri audio disponibili.\n"
#define MSGTR_BuiltinCodecsConf "Folosesc 'codecs.conf' built-in.\n"
#define MSGTR_CantLoadFont "Nu pot �nc�rca fontul: %s\n"
#define MSGTR_CantLoadSub "Nu pot �nc�rca subtitrarea: %s\n"
#define MSGTR_FPSnotspecified "FPS (nr. de cadre pe secund�) nu e specificat �n header sau e gre�it; folose�te op�iunea '-fps'.\n"
#define MSGTR_TryForceAudioFmtStr "For�ez familia de codec audio %s...\n"
#define MSGTR_CantFindAudioCodec "Nu g�sesc codec pentru formatul audio 0x%X.\n"
#define MSGTR_RTFMCodecs "Cite�te DOCS/HTML/en/codecs.html!\n" //lang
#define MSGTR_TryForceVideoFmtStr "For�ez familia de codecuri video %s...\n"
#define MSGTR_CantFindVideoCodec "Nu g�sesc codec potrivit pentru ie�irea '-vo' aleas� �i formatul video 0x%X.\n"
#define MSGTR_VOincompCodec "Dispozitivul de ie�ire video ales e incompatibil cu acest codec.\n"
#define MSGTR_CannotInitVO "FATAL: Nu pot activa driverul video.\n"
#define MSGTR_CannotInitAO "Nu pot deschide/ini�ializa audio -> rulez f�r� sunet.\n"
#define MSGTR_StartPlaying "Rulez...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"     *****************************************************\n"\
"     **** Sistemul t�u e prea LENT pentru acest film! ****\n"\
"     *****************************************************\n\n"\
"Posibile motive, probleme, rezolv�ri:\n"\
"- Cel mai des �nt�lnit caz: drivere _audio_ defecte\n"\
"  - �ncearc� '-ao sdl' sau folose�te ALSA 0.5 / emularea OSS pentru ALSA 0.9.\n"\
"  - Experimenteaz� cu diferite valori pentru '-autosync', �ncep�nd cu 30.\n"\
"- Ie�ire video lent�\n"\
"  - �ncearc� alt driver '-vo' ('-vo help' pentru o list�) sau \n"\
"    �ncearc� '-framedrop'.\n"\
"- Procesor lent\n"\
"  - Nu rula filme DVD/DivX mari pe un procesor lent! �ncearc� -hardframedrop.\n"\
"- Fi�ier stricat\n"\
"  - �ncearc� diferite combina�ii de '-nobps', '-ni', '-forceidx' sau '-mc 0'.\n"\
"- Surse lente (NFS/SMB, DVD, VCD etc.)\n"\
"  - �ncearc� '-cache 8192'.\n"\
"- Folose�ti -cache pentru fi�iere AVI neinterleaved?\n"\
"  - �ncearc� '-nocache'.\n"\
"Cite�te DOCS/HTML/en/video.html pentru idei de reglare/accelerare.\n"\
"Dac� tot nu reu�e�ti, cite�te DOCS/HTML/en/bugreports.html.\n\n" //lang

#define MSGTR_NoGui "MPlayer a fost compilat F�R� suport pentru GUI.\n"
#define MSGTR_GuiNeedsX "MPlayer GUI necesit� X11.\n"
#define MSGTR_Playing "Rulez %s.\n"
#define MSGTR_NoSound "Audio: f�r� sunet\n"
#define MSGTR_FPSforced "FPS for�at la %5.3f  (ftime: %5.3f).\n"
#define MSGTR_AvailableVideoOutputDrivers "Plugin-uri de ie�ire video disponibile:\n"
#define MSGTR_AvailableAudioOutputDrivers "Plugin-uri de ie�ire audio disponibile:\n"
#define MSGTR_AvailableAudioCodecs "Codec-uri audio disponibile:\n"
#define MSGTR_AvailableVideoCodecs "Codec-uri video disponibile:\n"
#define MSGTR_AvailableFsType "Moduri fullscreen disponibile:\n"
#define MSGTR_UsingRTCTiming "Using Linux hardware RTC timing (%ldHz).\n"
#define MSGTR_CannotReadVideoProperties "Video: Nu pot citi propriet��ile.\n"
#define MSGTR_NoStreamFound "Nu am g�sit nici un canal.\n"
#define MSGTR_ErrorInitializingVODevice "Eroare la activarea ie�irii video (-vo) aleas�.\n"
#define MSGTR_ForcedVideoCodec "Codec video for�at: %s\n"
#define MSGTR_ForcedAudioCodec "Codec audio for�at: %s\n"
#define MSGTR_Video_NoVideo "Video: nu exist� video\n"
#define MSGTR_NotInitializeVOPorVO "\nFATAL: Nu pot ini�ializa filtrele video (-vf) sau ie�irea video (-vo).\n"
#define MSGTR_Paused "\n  =====  PAUZ�  =====\r" // no more than 23 characters (status line for audio files)
#define MSGTR_PlaylistLoadUnable "\nNu pot s� �ncarc playlistul %s.\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer a murit. Nu ar trebui s� se �nt�mple asta.\n"\
"  S-ar putea s� fie un bug �n sursa MPlayer _sau_ �n driverele tale _sau_ �n\n"\
"  versiunea ta de gcc. Dac� crezi c� e vina MPlayer, te rog cite�te\n"\
"  DOCS/HTML/en/bugreports.html �i urmeaz� instruc�iunile de acolo. Nu putem\n"\
"  �i nu te vom ajuta decat dac� asiguri informa�ia cerut� acolo cand anun�i\n"\
"  un posibil bug.\n"

// mencoder.c:

#define MSGTR_UsingPass3ControllFile "Folosesc fi�ierul de control pass3: %s\n"
#define MSGTR_MissingFilename "\nLipse�te numele fi�ierului.\n\n"
#define MSGTR_CannotOpenFile_Device "Nu pot deschide fi�ierul/dispozitivul.\n"
#define MSGTR_CannotOpenDemuxer "Nu pot deschide demultiplexorul.\n"
#define MSGTR_NoAudioEncoderSelected "\nNu e ales nici un encoder audio (-oac). Alege unul (vezi '-oac help') sau folose�te '-nosound'.\n"
#define MSGTR_NoVideoEncoderSelected "\nNu e ales nici un encoder video (-ovc). Alege te rog unul (vezi '-ovc help').\n"
#define MSGTR_CannotOpenOutputFile "Nu pot deschide fi�ierul de ie�ire '%s'.\n"
#define MSGTR_EncoderOpenFailed "Nu pot deschide encoderul.\n"
#define MSGTR_ForcingOutputFourcc "For�ez ie�irea fourcc la %x [%.4s]\n"
#define MSGTR_WritingAVIHeader "Scriu header-ul AVI...\n"
#define MSGTR_DuplicateFrames "\n%d cadre duplicate!\n"
#define MSGTR_SkipFrame "\nSkipping frame!\n"
#define MSGTR_ErrorWritingFile "%s: Eroare la scrierea fi�ierului.\n"
#define MSGTR_WritingAVIIndex "\nScriu indexul AVI...\n"
#define MSGTR_FixupAVIHeader "Repar header-ul AVI...\n"
#define MSGTR_RecommendedVideoBitrate "Bitrate-ul video recomandatpentru %s CD: %d\n"
#define MSGTR_VideoStreamResult "\nCanal video: %8.3f kbit/s (%d bps)  dimensiune: %d bytes %5.3f sec %d cadre\n"
#define MSGTR_AudioStreamResult "\nCanal audio: %8.3f kbit/s (%d bps)  dimensiune: %d bytes %5.3f sec\n"

// cfg-mencoder.h:

#define MSGTR_MEncoderMP3LameHelp "\n\n"\
" vbr=<0-4>     metoda de bitrate variabil\n"\
"                0: cbr\n"\
"                1: mt\n"\
"                2: rh(default)\n"\
"                3: abr\n"\
"                4: mtrh\n"\
"\n"\
" abr           bitrate mediu\n"\
"\n"\
" cbr           bitrate constant\n"\
"               For�eaz� �i codarea �n mod CBR la preseturile ABR urm�toare.\n"\
"\n"\
" br=<0-1024>   alege bitrate-ul �n kBit (doar la CBR �i ABR)\n"\
"\n"\
" q=<0-9>       calitate (0-maxim�, 9-minim�) (doar pentru VBR)\n"\
"\n"\
" aq=<0-9>      calitate algoritmic� (0-maxim�/lent�, 9-minim�/rapid�)\n"\
"\n"\
" ratio=<1-100> rata de compresie\n"\
"\n"\
" vol=<0-10>    amplificarea intr�rii audio\n"\
"\n"\
" mode=<0-3>    (default: auto)\n"\
"                0: stereo\n"\
"                1: joint-stereo\n"\
"                2: dualchannel\n"\
"                3: mono\n"\
"\n"\
" padding=<0-2>\n"\
"                0: de loc\n"\
"                1: tot\n"\
"                2: ajusteaz�\n"\
"\n"\
" fast          Activeaz� codare rapid� pentru urm�toarele preseturi VBR,\n"\
"               la calitate pu�in redus� �i bitrate-uri crescute.\n"\
"\n"\
" preset=<value> Asigur� reglajele de calitate maxim posibile.\n"\
"                medium: codare VBR, calitate bun�\n"\
"                 (150-180 kbps bitrate)\n"\
"                standard:  codare VBR, calitate mare\n"\
"                 (170-210 kbps bitrate)\n"\
"                extreme: codare VBR calitate foarte mare\n"\
"                 (200-240 kbps bitrate)\n"\
"                insane:  codare CBR, calitate maxim�\n"\
"                 (320 kbps bitrate)\n"\
"                 <8-320>: codare ABR la bitrate-ul dat �n kbps.\n\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "Nu g�sesc CD-ROM-ul '%s'.\n"
#define MSGTR_ErrTrackSelect "Eroare la alegerea pistei VCD."
#define MSGTR_ReadSTDIN "Citesc din stdin...\n"
#define MSGTR_UnableOpenURL "Nu pot deschide URL-ul: %s\n"
#define MSGTR_ConnToServer "Conectat la serverul: %s\n"
#define MSGTR_FileNotFound "Nu g�sesc fi�ierul: '%s'\n"

#define MSGTR_SMBFileNotFound "Nu pot deschide de pe LAN: '%s'\n"

#define MSGTR_CantOpenDVD "Nu pot deschide DVD-ul: %s\n"
#define MSGTR_DVDwait "Citesc structura discului, te rog a�teapt�...\n"
#define MSGTR_DVDnumTitles "Sunt %d titluri pe acest DVD.\n"
#define MSGTR_DVDinvalidTitle "Num�rul titlului DVD gre�it: %d\n"
#define MSGTR_DVDnumChapters "Sunt %d capitole �n acest titlu.\n"
#define MSGTR_DVDinvalidChapter "Num�rul capitolului e gre�it: %d\n"
#define MSGTR_DVDnumAngles "Sunt %d unghiuri �n acest titlu DVD.\n"
#define MSGTR_DVDinvalidAngle "Num�rul unghiului gre�it: %d\n"
#define MSGTR_DVDnoIFO "Nu pot deschide fi�ierul IFO pentru titlul %d.\n"
#define MSGTR_DVDnoVOBs "Nu pot deschide VOB-ul pentru titlu (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD deschis OK.\n"

// demuxer.c, demux_*.c:
#define MSGTR_FormatNotRecognized \
"====== Scuze, formatul acestui fi�ier nu e cunoscut/suportat =======\n"\
"=== Dac� fi�ierul este AVI, ASF sau MPEG, te rog anun�� autorul! ===\n"

#define MSGTR_MissingVideoStream "Nu am g�sit canal video.\n"
#define MSGTR_MissingAudioStream "Nu am g�sit canal audio -> rulez f�r� sunet.\n"
#define MSGTR_MissingVideoStreamBug "Canal video lips�!? Intreab� autorul, ar putea fi un bug :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: Fi�ierul nu con�ine canalul video sau audio ales.\n"

#define MSGTR_NI_Forced "For�at"
#define MSGTR_NI_Detected "Detectat"

#define MSGTR_UsingNINI "Folosesc formatul AVI NON-INTERLEAVED (incorect).\n"
#define MSGTR_CouldntDetFNo "Nu pot determina num�rul de cadre (pentru seek absolut).\n"
#define MSGTR_CantSeekRawAVI "Nu pot derula �n stream-uri AVI pure. (E nevoie de index, �ncearc� cu op�iunea '-idx'.)\n"
#define MSGTR_CantSeekFile "Nu pot derula �n acest fi�ier.\n"

#define MSGTR_EncryptedVOB "Fi�ier VOB criptat! Cite�te DOCS/HTML/en/dvd.html.\n" // lang

#define MSGTR_MOVcomprhdr "MOV: Pentru a folosi headere compresate e nevoie de ZLIB!\n"
#define MSGTR_MOVvariableFourCC "MOV: ATENTIE: Am detectat FOURCC variabil!?\n"
#define MSGTR_MOVtooManyTrk "MOV: ATENTIE: prea multe piste"
#define MSGTR_FoundAudioStream "==> Canal audio g�sit: %d\n"
#define MSGTR_FoundVideoStream "==> Canal video g�sit: %d\n"
#define MSGTR_DetectedTV "TV detectat! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "Nu pot deschide demultiplexorul ogg.\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: Caut canalul audio (id:%d).\n"
#define MSGTR_CannotOpenAudioStream "Nu pot deschide canalul audio: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "Nu pot deschide canalul de subtitrare: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "Nu am reu�it s� deschid demultiplexorul audio: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "Nu am reu�it s� deschid demultiplexorul subtitr�rii: %s\n"
#define MSGTR_TVInputNotSeekable "Nu se poate derula TV! (Derularea probabil va schimba canalul ;)\n"
#define MSGTR_ClipInfo "Info despre clip:\n"

#define MSGTR_LeaveTelecineMode "\ndemux_mpg: am detectat con�inut NTSC la 30fps, schimb framerate-ul.\n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: am detectat con�inut NTSC progresiv la 24fps, schimb framerate-ul.\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "Nu pot deschide codecul.\n"
#define MSGTR_CantCloseCodec "Nu pot �nchide codecul.\n"

#define MSGTR_MissingDLLcodec "ERROR: Nu pot deschide codecul DirectShow necesar %s.\n"
#define MSGTR_ACMiniterror "Nu pot �nc�rca codecul audio Win32/ACM (lipse�te un DLL?).\n"
#define MSGTR_MissingLAVCcodec "Nu g�sesc codecul '%s' �n libavcodec...\n"

#define MSGTR_UsingExternalPP "[PP] Folosesc filtru de postprocesare extern, q max = %d.\n"
#define MSGTR_UsingCodecPP "[PP] Folosesc postprocesarea codecului, q max = %d.\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "Atributul video '%s' nu e suportat de vo & vd alese.\n"
#define MSGTR_OpeningVideoDecoder "Deschid decodorul video: [%s] %s\n"
#define MSGTR_OpeningAudioDecoder "Deschid decodorul audio: [%s] %s\n"
#define MSGTR_VDecoderInitFailed "VDecoder init e�uat :(\n"
#define MSGTR_ADecoderInitFailed "ADecoder init e�uat :(\n"
#define MSGTR_ADecoderPreinitFailed "ADecoder preinit e�uat :(\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: Aloc %d bytes pentru bufferul de intrare.\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: Aloc %d + %d = %d bytes pentru bufferul de ie�ire.\n"

// LIRC:
#define MSGTR_SettingUpLIRC "Preg�tesc folosirea LIRC...\n"
#define MSGTR_LIRCdisabled "Nu-�i vei putea folosi telecomanda.\n"
#define MSGTR_LIRCopenfailed "Nu am reu�it s� activez LIRC.\n"
#define MSGTR_LIRCcfgerr "Nu am putut citi fi�ierul de configurare LIRC %s.\n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "Nu g�sesc filtrul video '%s'.\n"
#define MSGTR_CouldNotOpenVideoFilter "Nu pot deschide filtrul video '%s'.\n"
#define MSGTR_OpeningVideoFilter "Deschid filtrul video: "

// vd.c

// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "Despre MPlayer"
#define MSGTR_FileSelect "Alege fi�ierul..."
#define MSGTR_SubtitleSelect "Alege subtitrarea..."
#define MSGTR_OtherSelect "Alege..."
#define MSGTR_AudioFileSelect "Alege canalul audio extern..."
#define MSGTR_FontSelect "Alege fontul..."
#define MSGTR_PlayList "Playlist"
#define MSGTR_Equalizer "Egalizator"
#define MSGTR_SkinBrowser "Alegere Skin-uri"
#define MSGTR_Network "Streaming �n re�ea..."
#define MSGTR_Preferences "Preferin�e"
#define MSGTR_NoMediaOpened "Nu e deschis nici un fi�ier."
#define MSGTR_VCDTrack "Pista VCD %d"
#define MSGTR_NoChapter "Nici un capitol"
#define MSGTR_Chapter "Capitol %d"
#define MSGTR_NoFileLoaded "Nici un fi�ier �nc�rcat."

// --- buttons ---
#define MSGTR_Ok "OK"
#define MSGTR_Cancel "Anulare"
#define MSGTR_Add "Adaug�"
#define MSGTR_Remove "Elimin�"
#define MSGTR_Clear "Sterge tot"
#define MSGTR_Config "Configurare"
#define MSGTR_ConfigDriver "Configurare driver"

// --- error messages ---
#define MSGTR_NEMFMR "Scuze, nu am memorie destul� pentru afi�area meniului."
#define MSGTR_IDFGCVD "Scuze, nu am g�sit un driver video compatibil cu GUI."
#define MSGTR_NEEDLAVCFAME "Scuze, nu po�i afi�a fi�iere ne-MPEG cu dispozitivul DXR3/H+ f�r� recodare.\n"\
"Activeaz� 'lavc' sau 'fame' �n c�su�a de configurare pentru DXR3/H+."

// --- skin loader error messages

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "Despre MPlayer"
#define MSGTR_MENU_Open "Deschide..."
#define MSGTR_MENU_PlayFile "Ruleaz� fi�ierul..."
#define MSGTR_MENU_PlayVCD "Ruleaz� VCD..."
#define MSGTR_MENU_PlayDVD "Ruleaz� DVD..."
#define MSGTR_MENU_PlayURL "Ruleaz� URL..."
#define MSGTR_MENU_LoadSubtitle "�ncarc� subtitrare..."
#define MSGTR_MENU_DropSubtitle "Scoate subtitreare..."
#define MSGTR_MENU_LoadExternAudioFile "�ncarc� fi�ier audio extern..."
#define MSGTR_MENU_Playing "Rulez"
#define MSGTR_MENU_Play "Play"
#define MSGTR_MENU_Pause "Pauz�"
#define MSGTR_MENU_Stop "Stop"
#define MSGTR_MENU_NextStream "Pista urm�toare"
#define MSGTR_MENU_PrevStream "Pista precedent�"
#define MSGTR_MENU_Size "Dimensiune"
#define MSGTR_MENU_NormalSize "Dimensiune normal�"
#define MSGTR_MENU_DoubleSize "Dimensiune dubl�"
#define MSGTR_MENU_FullScreen "�ntreg ecranul"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "Deschide discul..."
#define MSGTR_MENU_ShowDVDMenu "Afi�eaz� meniul DVD"
#define MSGTR_MENU_Titles "Titluri"
#define MSGTR_MENU_Title "Titlu %2d"
#define MSGTR_MENU_None "(nimic)"
#define MSGTR_MENU_Chapters "Capitole"
#define MSGTR_MENU_Chapter "Capitolul %2d"
#define MSGTR_MENU_AudioLanguages "Limbi pentru audio"
#define MSGTR_MENU_SubtitleLanguages "Limbi pentru subtitr�ri"
#define MSGTR_MENU_PlayList "Playlist"
#define MSGTR_MENU_SkinBrowser "Alegere skin"
#define MSGTR_MENU_Preferences "Preferin�e"
#define MSGTR_MENU_Exit "Ie�ire..."
#define MSGTR_MENU_Mute "F�r� sunet"
#define MSGTR_MENU_Original "Original"
#define MSGTR_MENU_AspectRatio "Raport dimensiuni"
#define MSGTR_MENU_AudioTrack "Pista audio"
#define MSGTR_MENU_Track "Pista %d"
#define MSGTR_MENU_VideoTrack "Pista video"

// --- equalizer
#define MSGTR_EQU_Audio "Audio"
#define MSGTR_EQU_Video "Video"
#define MSGTR_EQU_Contrast "Contrast: "
#define MSGTR_EQU_Brightness "Luuminozitate: "
#define MSGTR_EQU_Hue "Nuan��: "
#define MSGTR_EQU_Saturation "Satura�ie: "
#define MSGTR_EQU_Front_Left "Fa�� St�nga"
#define MSGTR_EQU_Front_Right "Fa�� Dreapta"
#define MSGTR_EQU_Back_Left "Spate St�nga"
#define MSGTR_EQU_Back_Right "Spate Dreapta"
#define MSGTR_EQU_Center "Centru"
#define MSGTR_EQU_Bass "Bass"
#define MSGTR_EQU_All "Toate"
#define MSGTR_EQU_Channel1 "Canalul 1:"
#define MSGTR_EQU_Channel2 "Canalul 2:"
#define MSGTR_EQU_Channel3 "Canalul 3:"
#define MSGTR_EQU_Channel4 "Canalul 4:"
#define MSGTR_EQU_Channel5 "Canalul 5:"
#define MSGTR_EQU_Channel6 "Canalul 6:"

// --- playlist
#define MSGTR_PLAYLIST_Path "Cale"
#define MSGTR_PLAYLIST_Selected "Fi�iere alese"
#define MSGTR_PLAYLIST_Files "Fi�iere"
#define MSGTR_PLAYLIST_DirectoryTree "Arbore de directoare"

// --- preferences
#define MSGTR_PREFERENCES_Audio "Audio"
#define MSGTR_PREFERENCES_Video "Video"
#define MSGTR_PREFERENCES_SubtitleOSD "Subtitr�ri & OSD"
#define MSGTR_PREFERENCES_Codecs "Codecuri & demuxer"
#define MSGTR_PREFERENCES_Misc "Altele"

#define MSGTR_PREFERENCES_None "Nimic"
#define MSGTR_PREFERENCES_AvailableDrivers "Drivere disponibile:"
#define MSGTR_PREFERENCES_DoNotPlaySound "Nu reda sunetul"
#define MSGTR_PREFERENCES_NormalizeSound "Normalizeaz� sunetul"
#define MSGTR_PREFERENCES_EnEqualizer "Activeaz� egalizatorul"
#define MSGTR_PREFERENCES_ExtraStereo "Activeaz� extra stereo"
#define MSGTR_PREFERENCES_Coefficient "Coeficient:"
#define MSGTR_PREFERENCES_AudioDelay "Decalaj audio"
#define MSGTR_PREFERENCES_DoubleBuffer "Activeaz� double buffering"
#define MSGTR_PREFERENCES_DirectRender "Activeaz� direct rendering"
#define MSGTR_PREFERENCES_FrameDrop "Activeaz� s�ritul cadrelor"
#define MSGTR_PREFERENCES_HFrameDrop "Activeaz� s�ritul dur de cadre (PERICULOS)"
#define MSGTR_PREFERENCES_Flip "Inverseaz� imaginea sus/jos"
#define MSGTR_PREFERENCES_OSDTimer "Ceas �i indicatori"
#define MSGTR_PREFERENCES_OSDProgress "Doar bara de derulare"
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "Ceas, procent �i timp total"
#define MSGTR_PREFERENCES_Subtitle "Subtitrare:"
#define MSGTR_PREFERENCES_SUB_Delay "Decalaj: "
#define MSGTR_PREFERENCES_SUB_FPS "FPS:"
#define MSGTR_PREFERENCES_SUB_POS "Pozitie: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "F�r� auto-�nc�rcarea subtitr�rii"
#define MSGTR_PREFERENCES_SUB_Unicode "Subtitrare Unicode"
#define MSGTR_PREFERENCES_SUB_MPSUB "Converte�te subtitrarea dat� la formatul MPlayer"
#define MSGTR_PREFERENCES_SUB_SRT "Converte�te subtitrarea la formatul time based SubViewer (SRT)"
#define MSGTR_PREFERENCES_SUB_Overlap "Alege suprapunerea subtitrarilor"
#define MSGTR_PREFERENCES_Font "Font:"
#define MSGTR_PREFERENCES_PostProcess "Activeaz� postprocesarea"
#define MSGTR_PREFERENCES_AutoQuality "Calitate auto: "
#define MSGTR_PREFERENCES_NI "Folose�te parser AVI non-interleaved"
#define MSGTR_PREFERENCES_IDX "Reconstruie�te tabela de index, dac� e nevoie"
#define MSGTR_PREFERENCES_VideoCodecFamily "Familia codecului video:"
#define MSGTR_PREFERENCES_AudioCodecFamily "Familia codecului audio:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "Nivelul OSD"
#define MSGTR_PREFERENCES_FRAME_Subtitle "Subtitrare"
#define MSGTR_PREFERENCES_FRAME_Font "Font"
#define MSGTR_PREFERENCES_FRAME_PostProcess "Postprocesare"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "Codec & demuxer"
#define MSGTR_PREFERENCES_FRAME_Cache "Cache"
#define MSGTR_PREFERENCES_FRAME_Misc "Altele"
#define MSGTR_PREFERENCES_Message "Nu uita c� rularea trebuie repornit� pentru ca unele op�iuni s�-�i fac� efectul!"
#define MSGTR_PREFERENCES_DXR3_VENC "Encoder video:"
#define MSGTR_PREFERENCES_DXR3_LAVC "Folose�te LAVC (FFmpeg)"
#define MSGTR_PREFERENCES_DXR3_FAME "Folose�te FAME"
#define MSGTR_PREFERENCES_FontEncoding1 "Unicode"

// lang 
#define MSGTR_PREFERENCES_FontEncoding2 "Limbi vest-europene (ISO-8859-1)"
#define MSGTR_PREFERENCES_FontEncoding3 "Limbi vest-europene cu Euro (ISO-8859-15)"
#define MSGTR_PREFERENCES_FontEncoding4 "Limbi central-europene sau slavice (ISO-8859-2)"
#define MSGTR_PREFERENCES_FontEncoding5 "Esperanto, galic�, maltez�, turc� (ISO-8859-3)"
#define MSGTR_PREFERENCES_FontEncoding6 "Vechiul charset baltic (ISO-8859-4)"
#define MSGTR_PREFERENCES_FontEncoding7 "Chrilic (ISO-8859-5)"
#define MSGTR_PREFERENCES_FontEncoding8 "Arab� (ISO-8859-6)"
#define MSGTR_PREFERENCES_FontEncoding9 "Greac� modern� (ISO-8859-7)"
#define MSGTR_PREFERENCES_FontEncoding10 "Turc� (ISO-8859-9)"
#define MSGTR_PREFERENCES_FontEncoding11 "Baltic (ISO-8859-13)"
#define MSGTR_PREFERENCES_FontEncoding12 "Celtic (ISO-8859-14)"
#define MSGTR_PREFERENCES_FontEncoding13 "Charseturi ebraice (ISO-8859-8)"
#define MSGTR_PREFERENCES_FontEncoding14 "Rus� (KOI8-R)"
#define MSGTR_PREFERENCES_FontEncoding15 "Ucrainian�, belarus� (KOI8-U/RU)"
#define MSGTR_PREFERENCES_FontEncoding16 "Chinez� simplificat� (CP936)"
#define MSGTR_PREFERENCES_FontEncoding17 "Chinez� tradi�ional� (BIG5)"
#define MSGTR_PREFERENCES_FontEncoding18 "Japonez� (SHIFT-JIS)"
#define MSGTR_PREFERENCES_FontEncoding19 "Corean� (CP949)"
#define MSGTR_PREFERENCES_FontEncoding20 "Tailandez� (CP874)"
#define MSGTR_PREFERENCES_FontEncoding21 "Chirilic de Windows (CP1251)"
#define MSGTR_PREFERENCES_FontEncoding22 "Central-european �i slavic de Windows (CP1250)"
#define MSGTR_PREFERENCES_FontNoAutoScale "F�r� scalare automat�"
#define MSGTR_PREFERENCES_FontPropWidth "Propor�ional cu l��imea filmului"
#define MSGTR_PREFERENCES_FontPropHeight "Propor�ional cu �n�l�imea filmului"
#define MSGTR_PREFERENCES_FontPropDiagonal "Proportional cu diagonala filmului"
#define MSGTR_PREFERENCES_FontEncoding "Codare:"
#define MSGTR_PREFERENCES_FontTextScale "Scara textului:"
#define MSGTR_PREFERENCES_FontOSDScale "Scara OSD:"
#define MSGTR_PREFERENCES_Cache "Cache on/off"
#define MSGTR_PREFERENCES_CacheSize "Dimensiune cache: "
#define MSGTR_PREFERENCES_LoadFullscreen "Porne�te fullscreen"
#define MSGTR_PREFERENCES_SaveWinPos "Salveaz� pozi�ia ferestrei"
#define MSGTR_PREFERENCES_XSCREENSAVER "Opre�te XScreenSaver"
#define MSGTR_PREFERENCES_PlayBar "Activeaz� playbar"
#define MSGTR_PREFERENCES_AutoSync "AutoSync pornit/oprit"
#define MSGTR_PREFERENCES_AutoSyncValue "Autosync: "
#define MSGTR_PREFERENCES_CDROMDevice "CD-ROM:"
#define MSGTR_PREFERENCES_DVDDevice "DVD:"
#define MSGTR_PREFERENCES_FPS "Cadre pe secund�:"
#define MSGTR_PREFERENCES_ShowVideoWindow "Afi�eaz� fereastra video cand e inactiv�"

#define MSGTR_ABOUT_UHU "Dezvoltare GUI sponsorizat� de UHU Linux\n"
#define MSGTR_ABOUT_CoreTeam "   echipa MPlayer principal�:\n"
#define MSGTR_ABOUT_AdditionalCoders "   Al�i progamatori:\n"
#define MSGTR_ABOUT_MainTesters "   Testeri principali:\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "Eroare fatal�!"
#define MSGTR_MSGBOX_LABEL_Error "Eroare!"
#define MSGTR_MSGBOX_LABEL_Warning "Aten�ie!"

#endif

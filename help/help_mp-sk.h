// Translated by:  Daniel Be�a, benad@centrum.cz
// Translated files should be uploaded to ftp://mplayerhq.hu/MPlayer/incoming
// synced  with 1.95, 1.4.03

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
#ifdef HAVE_LIBCSS
" -dvdauth <dev>  ur�enie DVD zariadenia pre overenie autenticity (pre k�dovan� disky)\n"
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
" p al. medzern�k pauza pri prehr�van� (pokra�ovan� stla�en�m niektorej kl�vesy)\n"
" q alebo ESC     koniec prehr�vania a ukon�enie programu\n"
" + alebo -       upravi� spozdenie zvuku v krokoch +/- 0.1 sekundy\n"
" o               cyklick� zmena re�imu OSD:  ni� / poz�cia / poz�cia+�as\n"
" * alebo /       prida� alebo ubra� hlasitos� (stla�en�m 'm' v�ber master/pcm)\n"
" z alebo x       upravi� spozdenie titulkov v krokoch +/- 0.1 sekundy\n"
" r alebo t       upravi� poz�ciu titulkov hore/dole, pozrite tie� -vop !\n"
"\n"
" * * * * PRE��TAJTE SI MAN STR�NKU PRE DETAILY (�AL�IE VO�BY A KL�VESY)! * * * *\n"
"\n";
#endif

// ========================= MPlayer messages ===========================
// mplayer.c:

#define MSGTR_Exiting "\nKon��m... (%s)\n"
#define MSGTR_Exit_quit "Koniec"
#define MSGTR_Exit_eof "Koniec s�boru"
#define MSGTR_Exit_error "Z�va�n� chyba"
#define MSGTR_IntBySignal "\nMPlayer preru�en� sign�lom %d v module: %s \n"
#define MSGTR_NoHomeDir "Nem��em najs� dom�ci (HOME) adres�r\n"
#define MSGTR_GetpathProblem "get_path(\"config\") probl�m\n"
#define MSGTR_CreatingCfgFile "Vytv�ram konfigura�n� s�bor: %s\n"
#define MSGTR_InvalidVOdriver "Neplatn� meno v�stupn�ho videoovl�da�a: %s\nPou�ite '-vo help' pre zoznam dostupn�ch ovl�da�ov.\n"
#define MSGTR_InvalidAOdriver "Neplatn� meno v�stupn�ho audioovl�da�a: %s\nPou�ite '-ao help' pre zoznam dostupn�ch ovl�da�ov.\n"
#define MSGTR_CopyCodecsConf "(copy/ln etc/codecs.conf (zo zdrojov�ch k�dov MPlayeru) do ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "Pou��vam vstavan� defaultne codecs.conf\n"
#define MSGTR_CantLoadFont "Nem��em na��ta� font: %s\n"
#define MSGTR_CantLoadSub "Nem��em na��ta� titulky: %s\n"
#define MSGTR_ErrorDVDkey "Chyba pri spracovan� k���a DVD.\n"
#define MSGTR_CmdlineDVDkey "DVD k��� po�adovan� na pr�kazovom riadku je uschovan� pre rozk�dovanie.\n"
#define MSGTR_DVDauthOk "DVD sekvencia overenia autenticity vypad� v poriadku.\n"
#define MSGTR_DumpSelectedStreamMissing "dump: FATAL: po�adovan� pr�d ch�ba!\n"
#define MSGTR_CantOpenDumpfile "Nejde otvori� s�bor pre dump!!!\n"
#define MSGTR_CoreDumped "jadro vyp�san� :)\n"
#define MSGTR_FPSnotspecified "V hlavi�ke s�boru nie je udan� (alebo je zl�) FPS! Pou�ite vo�bu -fps !\n"
#define MSGTR_TryForceAudioFmtStr "Pok��am sa vyn�ti� rodinu audiokodeku %s ...\n"
#define MSGTR_CantFindAfmtFallback "Nem��em n�js� audio kodek pre po�adovan� rodinu, pou�ijem ostatn�.\n"
#define MSGTR_CantFindAudioCodec "Nem��em n�js� kodek pre audio form�t 0x%X !\n"
#define MSGTR_TryUpgradeCodecsConfOrRTFM "*** Pok�ste sa upgradova� %s z etc/codecs.conf\n*** Pokia� probl�m pretrv�, pre��tajte si DOCS/CODECS!\n"
#define MSGTR_CouldntInitAudioCodec "Nejde inicializova� audio kodek! -> bez zvuku\n"
#define MSGTR_TryForceVideoFmtStr "Pok��am se vn�ti� rodinu videokodeku %s ...\n"
#define MSGTR_CantFindVideoCodec "Nem��em najs� kodek pre video form�t 0x%X !\n"
#define MSGTR_VOincompCodec "�ia�, vybran� video_out zariadenie je nekompatibiln� s t�mto kodekom.\n"
#define MSGTR_CannotInitVO "FATAL: Nem��em inicializova� video driver!\n"
#define MSGTR_CannotInitAO "nem��em otvori�/inicializova� audio driver -> TICHO\n"
#define MSGTR_StartPlaying "Za��nam prehr�va�...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"         ***********************************************************\n"\
"         ****  Na prehratie tohoto je v� syst�m pr�li� POMAL�!  ****\n"\
"         ***********************************************************\n"\
"!!! Mo�n� pr��iny, probl�my a rie�enia:\n"\
"- Nej�astej�ie: nespr�vny/chybn� _zvukov�_ ovl�da�. Rie�enie: sk�ste -ao sdl al. pou�ite\n"\
"  ALSA 0.5 alebo oss emul�ciu z ALSA 0.9. viac tipov sa dozviete v DOCS/sound.html!\n"\
"- Pomal� video v�stup. Sk�ste in� -vo ovl�da� (pre zoznam: -vo help) alebo sk�ste\n"\
"  s vo�bou -framedrop !  Tipy pre ladenie/zr�chlenie videa s� v DOCS/video.html\n"\
"- Pomal� cpu. Nesk��ajte prehr�va� ve�k� dvd/divx na pomalom cpu! Sk�ste -hardframedrop\n"\
"- Po�koden� s�bor. Sk�ste r�zne kombin�cie t�chto volieb: -nobps  -ni  -mc 0  -forceidx\n"\
"- Pomal� m�dium (NFS/SMB, DVD, VCD ...). Sk�ste -cache 8192.\n"\
"- Pou��vate -cache na prehr�vanie non-interleaved s�boru? sk�ste -nocache\n"\
"Pokia� ni� z toho nie je pravda, pre��tajte si DOCS/bugreports.html !\n\n"

#define MSGTR_NoGui "MPlayer bol prelo�en� BEZ podpory GUI!\n"
#define MSGTR_GuiNeedsX "MPlayer GUI vy�aduje X11!\n"
#define MSGTR_Playing "Prehr�vam %s\n"
#define MSGTR_NoSound "Audio: bez zvuku!!!\n"
#define MSGTR_FPSforced "FPS vn�ten� na hodnotu %5.3f  (ftime: %5.3f)\n"
#define MSGTR_CompiledWithRuntimeDetection "Skompilovn� s RUNTIME CPU Detection - varovanie, nie je to optim�lne! Na z�skanie max. v�konu, rekompilujte mplayer zo zdrojakov s --disable-runtime-cpudetection\n"
#define MSGTR_CompiledWithCPUExtensions "Skompilovan� pre x86 CPU s roz��reniami:"
#define MSGTR_AvailableVideoOutputPlugins "Dostupn� video v�stupn� pluginy:\n"
#define MSGTR_AvailableVideoOutputDrivers "Dostupn� video v�stupn� ovl�da�e:\n"
#define MSGTR_AvailableAudioOutputDrivers "Dostupn� audio v�stupn� ovl�da�e:\n"
#define MSGTR_AvailableAudioCodecs "Dostupn� audio kodeky:\n"
#define MSGTR_AvailableVideoCodecs "Dostupn� video kodeky:\n"
#define MSGTR_AvailableAudioFm "\nDostupn� (vkompilovan�) audio rodiny kodekov/ovl�da�e:\n"
#define MSGTR_AvailableVideoFm "\nDostupn� (vkompilovan�) video rodiny kodekov/ovl�da�e:\n"
#define MSGTR_AvailableFsType "Dostupn� zmeny plnoobrazovkov�ch m�dov:\n"
#define MSGTR_UsingRTCTiming "Pou��vam Linuxov� hardv�rov� RTC �asovanie (%ldHz)\n"
#define MSGTR_CannotReadVideoProperties "Video: nem��em ��ta� vlastnosti\n"
#define MSGTR_NoStreamFound "Nen�jden� pr�d\n"
#define MSGTR_InitializingAudioCodec "Initializujem audio kodek...\n"
#define MSGTR_ErrorInitializingVODevice "Chyba pri otv�ran�/inicializ�cii vybran�ch video_out (-vo) zariaden�!\n"
#define MSGTR_ForcedVideoCodec "Vn�ten� video kodek: %s\n"
#define MSGTR_ForcedAudioCodec "Vn�ten� video kodek: %s\n"
#define MSGTR_AODescription_AOAuthor "AO: Popis: %s\nAO: Autor: %s\n"
#define MSGTR_AOComment "AO: Koment�r: %s\n"
#define MSGTR_Video_NoVideo "Video: �iadne video!!!\n"
#define MSGTR_NotInitializeVOPorVO "\nFATAL: Nem��em inicializova� video filtre (-vop) alebo video v�stup (-vo) !\n"
#define MSGTR_Paused "\n------ PAUZA -------\r"
#define MSGTR_PlaylistLoadUnable "\nNem��em na��ta� playlist %s\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- MPlayer zhavaroval na 'Illegal Instruction'.\n"\
"  M��e to by� chyba v na�om novom k�de na detekciu procesora...\n"\
"  Pros�m pre��tajte si DOCS/bugreports.html.\n"
#define MSGTR_Exit_SIGILL \
"- MPlayer zhavaroval na 'Illegal Instruction'.\n"\
"  Oby�ajne sa to st�va, ke� ho pou��vate na inom procesore ako pre ktor� bol\n"\
"  skompilovan�/optimalizovan�.\n  Skontrolujte si to!\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- MPlayer zhavaroval nespr�vnym pou�it�m CPU/FPU/RAM.\n"\
"  Prekompilujte MPlayer s --enable-debug a urobte 'gdb' backtrace a\n"\
"  disassemblujte. Pre detaily, pozrite DOCS/bugreports.html#crash.b.\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer zhavaroval. To sa nemalo sta�.\n"\
"  M��e to by� chyba v MPlayer k�de _alebo_ vo Va��ch ovl�da�och _alebo_ gcc\n"\
"  verzii. Ak si mysl�te, �e je to chyba MPlayeru, pros�m pre��tajte si DOCS/bugreports.html\n"\
"  a postupujte pod�a in�trukcii. Nem��eme V�m pom�c�, pokia� neposkytnete\n"\
"  tieto inform�cie pri ohlasovan� mo�nej chyby.\n"

// mencoder.c:

#define MSGTR_UsingPass3ControllFile "Pou��vam pass3 ovl�dac� s�bor: %s\n"
#define MSGTR_MissingFilename "\nCh�baj�ce meno s�boru!\n\n"
#define MSGTR_CannotOpenFile_Device "Nem��em otvori� s�bor/zariadenie\n"
#define MSGTR_ErrorDVDAuth "Chyba v DVD auth...\n"
#define MSGTR_CannotOpenDemuxer "Nem��em otvori� demuxer\n"
#define MSGTR_NoAudioEncoderSelected "\n�iaden encoder (-oac) vybran�! Vyberte jeden alebo -nosound. Pou�ite -oac help !\n"
#define MSGTR_NoVideoEncoderSelected "\n�iaden encoder (-ovc) vybran�! Vyberte jeden, pou�ite -ovc help !\n"
#define MSGTR_InitializingAudioCodec "Inicializujem audio kodek...\n"
#define MSGTR_CannotOpenOutputFile "Nem��em otvori� s�bor '%s'\n"
#define MSGTR_EncoderOpenFailed "Zlyhal to open the encoder\n"
#define MSGTR_ForcingOutputFourcc "Vnucujem v�stupn� form�t (fourcc) na %x [%.4s]\n"
#define MSGTR_WritingAVIHeader "Zapisujem AVI hlavi�ku...\n"
#define MSGTR_DuplicateFrames "\nduplikujem %d snimkov!!!    \n"
#define MSGTR_SkipFrame "\npresko�i� sn�mok!!!    \n"
#define MSGTR_ErrorWritingFile "%s: chyba pri z�pise s�boru.\n"
#define MSGTR_WritingAVIIndex "\nzapisujem AVI index...\n"
#define MSGTR_FixupAVIHeader "Opravujem AVI hlavi�ku...\n"
#define MSGTR_RecommendedVideoBitrate "Doporu�en� r�chlost bit. toku videa pre %s CD: %d\n"
#define MSGTR_VideoStreamResult "\nVideo pr�d: %8.3f kbit/s  (%d bps)  velkos�: %d bytov  %5.3f sekund  %d sn�mkov\n"
#define MSGTR_AudioStreamResult "\nAudio pr�d: %8.3f kbit/s  (%d bps)  velkos�: %d bytov  %5.3f sekund\n"

// cfg-mencoder.h:

#define MSGTR_MEncoderMP3LameHelp "\n\n"\
" vbr=<0-4>     met�da variabilnej bit. r�chlosti \n"\
"                0: cbr\n"\
"                1: mt\n"\
"                2: rh(default)\n"\
"                3: abr\n"\
"                4: mtrh\n"\
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
" fast          prepn�� na r�chlej�ie k�dovanie na na podsekvenci�ch VBR m�dov,\n"\
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

// open.c, stream.c:
#define MSGTR_CdDevNotfound "CD-ROM zariadenie '%s' nen�jden�!\n"
#define MSGTR_ErrTrackSelect "Chyba pri v�bere VCD stopy!"
#define MSGTR_ReadSTDIN "��tam z stdin...\n"
#define MSGTR_UnableOpenURL "Nejde otvori� URL: %s\n"
#define MSGTR_ConnToServer "Pripojen� k servru: %s\n"
#define MSGTR_FileNotFound "S�bor nen�jden�: '%s'\n"

#define MSGTR_SMBInitError "Nem��em inicializova� kni�nicu libsmbclient: %d\n"
#define MSGTR_SMBFileNotFound "Nem��em otvori� z lan: '%s'\n"
#define MSGTR_SMBNotCompiled "MPlayer mebol skompilovan� s podporou ��tania z SMB\n"

#define MSGTR_CantOpenDVD "Nejde otvori� DVD zariadenie: %s\n"
#define MSGTR_DVDwait "��tam �trukt�ru disku, pros�m �akajte...\n"
#define MSGTR_DVDnumTitles "Na tomto DVD je %d titulov.\n"
#define MSGTR_DVDinvalidTitle "Neplatn� ��slo DVD titulu: %d\n"
#define MSGTR_DVDnumChapters "Na tomto DVD je %d kapitol.\n"
#define MSGTR_DVDinvalidChapter "Neplatn� ��slo kapitoly DVD: %d\n"
#define MSGTR_DVDnumAngles "Na tomto DVD je %d �hlov poh�adov.\n"
#define MSGTR_DVDinvalidAngle "Neplatn� ��slo uhlu poh�adu DVD: %d\n"
#define MSGTR_DVDnoIFO "Nem��em otvori� s�bor IFO pre DVD titul %d.\n"
#define MSGTR_DVDnoVOBs "Nem��em otvori� VOB s�bor (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD �spe�ne otvoren�.\n"

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
#define MSGTR_CantSeekRawAVI "Nem��em sa pos�va� v surov�ch (raw) .AVI pr�doch! (Potrebujem index, zkuste pou�� vo�bu -idx !)  \n"
#define MSGTR_CantSeekFile "Nem��em sa pos�va� v tomto s�bore!  \n"

#define MSGTR_EncryptedVOB "K�dovan� VOB s�bor (prelo�en� bez podpory libcss)! Pre��tajte si DOCS/DVD\n"
#define MSGTR_EncryptedVOBauth "Zak�dovan� pr�d, ale overenie autenticity ste nepo�adovali!!\n"

#define MSGTR_MOVcomprhdr "MOV: Komprimovan� hlavi�ky nie s� (e�te) podporovan�!\n"
#define MSGTR_MOVvariableFourCC "MOV: Upozornenie! premenn� FOURCC detekovan�!?\n"
#define MSGTR_MOVtooManyTrk "MOV: Upozornenie! Pr�li� ve�a st�p!"
#define MSGTR_FoundAudioStream "==> N�jden� audio pr�d: %d\n"
#define MSGTR_FoundVideoStream "==> N�jden� video pr�d: %d\n"
#define MSGTR_DetectedTV "TV detekovan� ! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "Nem��em otvori� ogg demuxer\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: H�ad�m audio pr�d (id:%d)\n"
#define MSGTR_CannotOpenAudioStream "Nem��em otvori� audio pr�d: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "Nem��em otvori� pr�d titulkov: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "Nem��em otvori� audio demuxer: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "Nem��em otvori� demuxer titulkov: %s\n"
#define MSGTR_TVInputNotSeekable "v TV vstupe nie je mo�n� sa pohybova�! (mo�no posun bude na zmenu kan�lov ;)\n"
#define MSGTR_DemuxerInfoAlreadyPresent "Demuxer info %s u� pr�tomn�\n!"
#define MSGTR_ClipInfo "Inform�cie o klipe: \n"

#define MSGTR_LeaveTelecineMode "\ndemux_mpg: Progres�vna seq detekovan�, nech�vam m�d 3:2 TELECINE \n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: 3:2 TELECINE detekovan�, zap�nam inverzn� telecine fx. FPS zmenen� na %5.3f!  \n"

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
#define MSGTR_OpeningAudioDecoder "Otv�ram audio dek�der: [%s] %s\n"
#define MSGTR_UninitVideoStr "uninit video: %s  \n"
#define MSGTR_UninitAudioStr "uninit audio: %s  \n"
#define MSGTR_VDecoderInitFailed "VDecoder init zlyhal :(\n"
#define MSGTR_ADecoderInitFailed "ADecoder init zlyhal :(\n"
#define MSGTR_ADecoderPreinitFailed "ADecoder preinit zlyhal :(\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: Alokujem %d bytov pre vstupn� buffer\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: Alokujem %d + %d = %d bytov pre v�stupn� buffer\n"
			 
// LIRC:
#define MSGTR_SettingUpLIRC "Nastavujem podporu LIRC ...\n"
#define MSGTR_LIRCdisabled "Nebudete m�c� pou��va� dia�kov� ovl�da�.\n"
#define MSGTR_LIRCopenfailed "Zlyhal pokus o otvorenie podpory LIRC!\n"
#define MSGTR_LIRCcfgerr "Zlyhalo ��tanie konfigura�n�ho s�boru LIRC %s !\n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "Nem��em n�js� video filter '%s'\n"
#define MSGTR_CouldNotOpenVideoFilter "Nem��em otvori� video filter '%s'\n"
#define MSGTR_OpeningVideoFilter "Otv�ram video filter: "
#define MSGTR_CannotFindColorspace "Nem��em n�js� be�n� priestor farieb, ani vlo�en�m 'scale' :(\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDec: kodek nenastavil sh->disp_w a sh->disp_h, sk��am to ob�s�!\n"
#define MSGTR_VoConfigRequest "VDec: vo konfigura�n� po�iadavka - %d x %d (preferovan� csp: %s)\n"
#define MSGTR_CouldNotFindColorspace "Nem��em n�js� zhodn� priestor farieb - sk��am znova s -vop scale...\n"
#define MSGTR_MovieAspectIsSet "Movie-Aspect je %.2f:1 - men�m rozmery na spr�vne.\n"
#define MSGTR_MovieAspectUndefined "Movie-Aspect je nedefinovn� - nemenia sa rozmery.\n"


// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "O aplik�cii"
#define MSGTR_FileSelect "Vybra� s�bor ..."
#define MSGTR_SubtitleSelect "Vybra� titulky ..."
#define MSGTR_OtherSelect "Vybra� ..."
#define MSGTR_AudioFileSelect "Vybra� extern� audio kan�l ..."
#define MSGTR_FontSelect "Vybra� font ..."
#define MSGTR_PlayList "PlayList"
#define MSGTR_Equalizer "Equalizer"
#define MSGTR_SkinBrowser "Prehliada� t�m"
#define MSGTR_Network "Sie�ov� prehr�vanie (streaming) ..."
#define MSGTR_Preferences "Preferencie"
#define MSGTR_OSSPreferences "konfigur�cia OSS ovl�da�a"
#define MSGTR_SDLPreferences "konfigur�cia SDL ovl�da�a"
#define MSGTR_NoMediaOpened "�iadne m�dium otvoren�"
#define MSGTR_VCDTrack "VCD stopa %d"
#define MSGTR_NoChapter "�iadna kapitola"
#define MSGTR_Chapter "kapitola %d"
#define MSGTR_NoFileLoaded "nenahran� �iaden s�bor"

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
   
// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[t�my] chyba v konfig. s�bore t�m %d: %s"
#define MSGTR_SKIN_WARNING1 "[t�my] varovanie v konfig. s�bore t�m na riadku %d: widget najden� ale pred  \"section\" nen�jden� ( %s )"
#define MSGTR_SKIN_WARNING2 "[t�my] varovanie v konfig. s�bore t�m na riadku %d: widget najden� ale pred \"subsection\" nen�jden� (%s)"
#define MSGTR_SKIN_WARNING3 "[skin] varovanie v konfig. s�bore t�m na riadku %d: t�to subsekcia nie je podporovan� t�mto widget (%s)"
#define MSGTR_SKIN_BITMAP_16bit  "bitmapa s h�bkou 16 bit a menej je nepodporovan� ( %s ).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "s�bor nen�jden� ( %s )\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "chyba ��tania bmp ( %s )\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "chyba ��tania tga ( %s )\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "chyba ��tania png ( %s )\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "form�t RLE packed tga nepodporovan� ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "nezn�my typ s�boru ( %s )\n"
#define MSGTR_SKIN_BITMAP_ConvertError "chyba konverzie z 24 bit do 32 bit ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "nezn�ma spr�va: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "nedostatok pam�te\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "pr�li� mnoho fontov deklarovan�ch\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "s�bor fontov nen�jden�\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "s�bor obrazov fontu nen�jden�\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "neexistuj�ci identifik�tor fontu ( %s )\n"
#define MSGTR_SKIN_UnknownParameter "nezn�my parameter ( %s )\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[prehliada� t�m] nedostatok pam�te.\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "Skin nen�jden� ( %s ).\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "Chyba pri ��tan� konfigura�n�ho s�boru t�m ( %s ).\n"
#define MSGTR_SKIN_LABEL "T�my:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "O aplik�cii MPlayer"
#define MSGTR_MENU_Open "Otvori� ..."
#define MSGTR_MENU_PlayFile "Prehra� s�bor ..."
#define MSGTR_MENU_PlayVCD "Prehra� VCD ..."
#define MSGTR_MENU_PlayDVD "Prehra� DVD ..."
#define MSGTR_MENU_PlayURL "Prehra� URL ..."
#define MSGTR_MENU_LoadSubtitle "Na��ta� titulky ..."
#define MSGTR_MENU_DropSubtitle "Zahodi� titulky ..."
#define MSGTR_MENU_LoadExternAudioFile "Na��ta� extern� audio s�bor ..."
#define MSGTR_MENU_Playing "Prehr�vam"
#define MSGTR_MENU_Play "Prehra�"
#define MSGTR_MENU_Pause "Pauza"
#define MSGTR_MENU_Stop "Zastavi�"
#define MSGTR_MENU_NextStream "�al�� pr�d"
#define MSGTR_MENU_PrevStream "Predch�dzaj�ci pr�d"
#define MSGTR_MENU_Size "Ve�kos�"
#define MSGTR_MENU_NormalSize "Norm�lna ve�kos�"
#define MSGTR_MENU_DoubleSize "Dvojn�sobn� ve�kos�"
#define MSGTR_MENU_FullScreen "Cel� obrazovka"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "Prehra� disk ..."
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
#define MSGTR_MENU_Preferences "Nastavenia"
#define MSGTR_MENU_Exit "Koniec ..."
#define MSGTR_MENU_Mute "Stlmi� zvuk"
#define MSGTR_MENU_Original "Origin�l"
#define MSGTR_MENU_AspectRatio "Pomer str�n obrazu"
#define MSGTR_MENU_AudioTrack "Audio stopa"
#define MSGTR_MENU_Track "Stopa %d"
#define MSGTR_MENU_VideoTrack "Video stopa"

// --- equalizer
#define MSGTR_EQU_Audio "Audio"
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
#define MSGTR_PREFERENCES_Audio "Audio"
#define MSGTR_PREFERENCES_Video "Video"
#define MSGTR_PREFERENCES_SubtitleOSD "Titulky a OSD"
#define MSGTR_PREFERENCES_Codecs "K�deky a demuxer"
#define MSGTR_PREFERENCES_Misc "R�zne"

#define MSGTR_PREFERENCES_None "Ni�"
#define MSGTR_PREFERENCES_AvailableDrivers "Dostupn� ovl�da�e:"
#define MSGTR_PREFERENCES_DoNotPlaySound "Nehra� zvuk"
#define MSGTR_PREFERENCES_NormalizeSound "Normalizova� zvuk"
#define MSGTR_PREFERENCES_EnEqualizer "Zapn�� equalizer"
#define MSGTR_PREFERENCES_ExtraStereo "Zapn�� extra stereo"
#define MSGTR_PREFERENCES_Coefficient "Koeficient:"
#define MSGTR_PREFERENCES_AudioDelay "Audio oneskorenie"
#define MSGTR_PREFERENCES_DoubleBuffer "Zapn�� dvojt� buffering"
#define MSGTR_PREFERENCES_DirectRender "Zapn�� direct rendering"
#define MSGTR_PREFERENCES_FrameDrop "Povoli� zahadzovanie r�mcov"
#define MSGTR_PREFERENCES_HFrameDrop "Povoli� TVRD� zahadzovanie r�mcov (nebezpe�n�)"
#define MSGTR_PREFERENCES_Flip "prehodi� obraz horn� strana-dole"
#define MSGTR_PREFERENCES_Panscan "Panscan: "
#define MSGTR_PREFERENCES_OSDTimer "�asova� a indik�tor"
#define MSGTR_PREFERENCES_OSDProgress "Iba ukazovate� priebehu"
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
#define MSGTR_PREFERENCES_FRAME_Misc "R�zne"
#define MSGTR_PREFERENCES_OSS_Device "Zariadenie:"
#define MSGTR_PREFERENCES_OSS_Mixer "Mixer:"
#define MSGTR_PREFERENCES_SDL_Driver "Ovl�da�:"
#define MSGTR_PREFERENCES_Message "Pros�m pam�tajte, nietor� vo�by potrebuj� re�tart prehr�vania!"
#define MSGTR_PREFERENCES_DXR3_VENC "Video k�der:"
#define MSGTR_PREFERENCES_DXR3_LAVC "Pou�i� LAVC (ffmpeg)"
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
#define MSGTR_PREFERENCES_FPS "Film FPS:"
#define MSGTR_PREFERENCES_ShowVideoWindow "Uk�za� video okno pri neaktivite"

#define MSGTR_ABOUT_UHU "v�voj GUI sponoroval UHU Linux\n"
#define MSGTR_ABOUT_CoreTeam "   MPlayer z�kladn� t�m:\n"
#define MSGTR_ABOUT_AdditionalCoders "   �al�� v�voj�ri:\n"
#define MSGTR_ABOUT_MainTesters "   Hlavn� testeri:\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "fat�lna chyba ..."
#define MSGTR_MSGBOX_LABEL_Error "chyba ..."
#define MSGTR_MSGBOX_LABEL_Warning "upozornenie ..."

#endif
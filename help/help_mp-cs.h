// Translated by:  Jiri Svoboda, jiri.svoboda@seznam.cz
// Updated by:     Tomas Blaha,  tomas.blaha at kapsa.club.cz
// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char* banner_text=
"\n\n"
"MPlayer " VERSION "(C) 2000-2003 MPlayer Team\n"
"\n";

static char help_text[]=
"Pou�it�:          mplayer [p�ep�na�e] [url|cesta/]jm�no_souboru\n"
"\n"
"Z�kladn� p�ep�na�e: (kompletn� seznam je v manu�lov� str�nce)\n"
" -vo <ovl[:za�]>  v�b�r v�st. video ovlada�e a za��zen� (-vo help pro seznam)\n"
" -ao <ovl[:za�]>  v�b�r v�st. audio ovlada�e a za��zen� (-ao help pro seznam)\n"
#ifdef HAVE_VCD
" vcd://<��slo>     p�ehr�t VCD (Video CD) stopu ze za��zen� m�sto ze souboru\n"
#endif
#ifdef HAVE_LIBCSS
" -dvdauth <za�>   ur�en� DVD za��zen� pro autentizaci (pro k�dovan� disky)\n"
#endif
#ifdef USE_DVDREAD
" dvd://<��slo>     p�ehr�t DVD titul ze za��zen� (mechaniky), m�sto ze souboru\n"
" -alang/-slang    zvolit jazyk audia/titulk� na DVD (dvouznakov� k�d zem�)\n"
#endif
" -ss <timepos>    posun na danou pozici (sekundy nebo hh:mm:ss)\n"
" -nosound         p�ehr�vat beze zvuku\n"
" -fs              celoobrazovkov� p�ehr�v�n� (nebo -vm -zoom, viz manu�l)\n"
" -x <x> -y <y>    rozli�en� zobrazov�n� (pro pou�it� s -vm �i -zoom)\n"
" -sub <soubor>    volba souboru s titulky (viz tak� -subfps, -subdelay)\n"
" -playlist <soubor> ur�en� souboru s playlistem\n"
" -vid x -aid y    v�b�r ��sla video (x) a audio (y) proudu pro p�ehr�n�\n"
" -fps x -srate y  zm�na video (x fps) a audio (y Hz) frekvence\n"
" -pp <quality>    aktivace postprocesing filtru (podrobnosti v manu�lu)\n"
" -framedrop       povolit zahazov�n� sn�mk� (pro pomal� stroje)\n"
"\n"
"Z�kladn� kl�vesy: (kompletn� seznam je v manu�lu, viz tak� input.conf)\n"
" <-  nebo  ->     posun vzad/vp�ed o 10 sekund\n"
" nahoru �i dol�   posun vzad/vp�ed o  1 minutu\n"
" pgup �i pgdown   posun vzad/vp�ed o 10 minut\n"
" < nebo >         posun vzad/vp�ed v playlistu\n"
" p nebo mezern�k  pauza p�i p�ehr�v�n� (pokra�ov�n� stiskem kter�koliv kl�vesy)\n"
" q nebo ESC       konec p�ehr�v�n� a ukon�en� programu\n"
" + nebo -         upravit zpo�d�n� zvuku v kroc�ch +/- 0,1 sekundy\n"
" o                cyklick� zm�na re�imu OSD: nic / pozice / pozice a �as/\n"
" * nebo /         p�idat nebo ubrat PCM hlasitost\n"
" z nebo x         upravit zpo�d�n� titulk� v kroc�ch +/- 0,1 sekundy\n"
" r nebo t         upravit polohu titulk� nahoru/dol�, viz tak� -vop expand\n"
"\n"
" * * * V MAN STR�NCE NAJDETE PODROBNOSTI, DAL�� PARAMETRY A KL�VESY * * *\n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// mplayer.c:

#define MSGTR_Exiting "\nKon��m... (%s)\n"
#define MSGTR_Exit_quit "Konec"
#define MSGTR_Exit_eof "Konec souboru"
#define MSGTR_Exit_error "Z�va�n� chyba"
#define MSGTR_IntBySignal "\nMPlayer p�eru�en sign�lem %d v modulu: %s \n"
#define MSGTR_NoHomeDir "Nemohu nal�zt dom�c� (HOME) adres��.\n"
#define MSGTR_GetpathProblem "probl�m s get_path(\"config\")\n"
#define MSGTR_CreatingCfgFile "Vytv���m konfigura�n� soubor: %s\n"
#define MSGTR_InvalidVOdriver "Neplatn� jm�no v�stupn�ho video ovlada�e: %s\nPou�ijte '-vo help' pro seznam dostupn�ch ovlada��.\n"
#define MSGTR_InvalidAOdriver "Neplatn� jm�no v�stupn�ho audio ovlada�e: %s\nPou�ijte '-ao help' pro seznam dostupn�ch ovlada��.\n"
#define MSGTR_CopyCodecsConf "(copy/ln etc/codecs.conf (ze zdrojov�ch k�d� MPlayeru) do ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "Pou��v�m v�choz� (zabudovan�) codecs.conf\n"
#define MSGTR_CantLoadFont "Nemohu na��st font: %s\n"
#define MSGTR_CantLoadSub "Nemohu na��st titulky: %s\n"
#define MSGTR_ErrorDVDkey "Chyba p�i zpracov�n� kl��e DVD.\n"
#define MSGTR_CmdlineDVDkey "DVD kl�� po�adovan� na p��kazov� ��dce je uschov�n pro rozk�dov�n�.\n"
#define MSGTR_DVDauthOk "DVD autentiza�n� sekvence vypad� vpo��dku.\n"
#define MSGTR_DumpSelectedStreamMissing "dump: Kritick� chyba: po�adovan� proud chyb�!\n"
#define MSGTR_CantOpenDumpfile "Nelze otev��t soubor pro dump!!!\n"
#define MSGTR_CoreDumped "J�dro vydumpov�no ;)\n"
#define MSGTR_FPSnotspecified "V hlavi�ce souboru nen� ud�no (nebo je �patn�) FPS! Pou�ijte volbu -fps !\n"
#define MSGTR_TryForceAudioFmtStr "Pokou��m se vynutit rodinu audiokodeku %s ...\n"
#define MSGTR_CantFindAfmtFallback "Nemohu nal�zt audio kodek pro po�adovanou rodinu, pou�iji ostatn�.\n"
#define MSGTR_CantFindAudioCodec "Nemohu nal�zt kodek pro audio form�t 0x%X !\n"
#define MSGTR_TryUpgradeCodecsConfOrRTFM "*** Pokuste se upgradovat %s z etc/codecs.conf\n*** Pokud probl�m p�etrv�, pak si p�e�t�te DOCS/CODECS!\n"
#define MSGTR_CouldntInitAudioCodec "Nelze inicializovat audio kodek! -> beze zvuku\n"
#define MSGTR_TryForceVideoFmtStr "Poku��m se vynutit rodinu videokodeku %s ...\n"
#define MSGTR_CantFindVideoCodec "Nemohu nal�zt kodek pro vybran� -vo a video form�t 0x%X !\n"
#define MSGTR_VOincompCodec "Bohu�el, vybran� video_out za��zen� je nekompatibiln� s t�mto kodekem.\n"
#define MSGTR_CannotInitVO "Kritick� chyba: Nemohu inicializovat video driver!\n"
#define MSGTR_CannotInitAO "nemohu otev��t/inicializovat audio driver -> TICHO\n"
#define MSGTR_StartPlaying "Za��n�m p�ehr�vat...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"         ***********************************************************\n"\
"         ****  V� syst�m je p��li� POMAL� pro toto p�ehr�v�n�! ****\n"\
"         ***********************************************************\n\n"\
"Mo�n� p���iny, probl�my a �e�en�:\n"\
"- Nej�ast�j��: �patn�/chybn� _zvukov�_ ovlada�\n!"\
"  - Zkuste -ao sdl nebo pou�ijte ALSA 0.5 �i oss emulaci z ALSA 0.9.\n"\
"  - Pohrajte si s r�zn�mi hodnotami -audiosync, pro za��tek t�eba 30.\n"\
"- Pomal� video v�stup\n"\
"  - Zkuste jin� -vo ovlada� (pro seznam: -vo help) nebo zkuste -framedrop!\n"\
"- Pomal� CPU\n"\
"  - Nezkou�ejte p�ehr�vat velk� DVD/DivX na pomal�m procesoru! Zkuste -hardframedrop.\n"\
"- Po�kozen� soubor.\n"\
"  - Zkuste r�zn� kombinace t�chto voleb: -nobps -ni -forceidx -mc 0.\n"\
"- P�i p�ehr�v�n� z pomal�ch m�di� (NFS/SMB, DVD, VCD, atd.)\n"\
"  - Zkuste -cache 8192.\n"\
"- Pou��v�te -cache pro neprokl�dan� AVI soubory?\n"\
"  - Zkuste -nocache.\n"\
"Tipy na vylad�n� a zrychlen� najdete v DOCS/video.html a DOCS/sound.html.\n"\
"Pokud nic z toho nepom��e, p�e�t�te si DOCS/bugreports.html.\n\n"

#define MSGTR_NoGui "MPlayer byl p�elo�en BEZ podpory GUI!\n"
#define MSGTR_GuiNeedsX "GUI MPlayeru vy�aduje X11.\n"
#define MSGTR_Playing "P�ehr�v�m %s\n"
#define MSGTR_NoSound "Audio: beze zvuku!!!\n"
#define MSGTR_FPSforced "FPS vynuceno na hodnotu %5.3f  (ftime: %5.3f)\n"
#define MSGTR_CompiledWithRuntimeDetection "P�elo�eno s detekc� CPU ZA B�HU - upozorn�n�, toto nen� optim�ln�!\nAbyste z�skali co nejv�t�� v�kon, p�elo�te znovu mplayer ze zdrojov�ho k�du\ns p�ep�na�em --disable-runtime-cpudetection\n"
#define MSGTR_CompiledWithCPUExtensions "P�elo�eno pro CPU x86 s roz���en�mi:"
#define MSGTR_AvailableVideoOutputPlugins "Dostupn� z�suvn� video moduly:\n"
#define MSGTR_AvailableVideoOutputDrivers "Dostupn� ovlada�e pro video:\n"
#define MSGTR_AvailableAudioOutputDrivers "Dostupn� ovlada�e pro audio:\n"
#define MSGTR_AvailableAudioCodecs "Dostupn� audio kodeky:\n"
#define MSGTR_AvailableVideoCodecs "Dostupn� video kodeky:\n"
#define MSGTR_AvailableAudioFm "\nDostupn� (p�ikompilovan�) rodiny audio kodek�/ovlada�e:\n"
#define MSGTR_AvailableVideoFm "\nDostupn� (p�ikompilovan�) rodiny video kodek�/ovlada�e:\n"
#define MSGTR_AvailableFsType "Dostupn� zp�soby zm�ny hladiny p�i zobrazen� p�es celou obrazovku:\n"
#define MSGTR_UsingRTCTiming "Pou�ito �asov�n� pomoc� Linux hardware RTC (%ldHz)\n"
#define MSGTR_CannotReadVideoProperties "Video: nelze p�e��st vlastnosti\n"
#define MSGTR_NoStreamFound "Nenalezen ��dn� proud\n"
#define MSGTR_InitializingAudioCodec "Inicializuji audio kodek...\n"
#define MSGTR_ErrorInitializingVODevice "Chyba p�� otv�r�n�/inicializaci vybran�ho video_out (-vo) za��zen�!\n"
#define MSGTR_ForcedVideoCodec "Vynucen video kodek: %s\n"
#define MSGTR_ForcedAudioCodec "Vynucen audio kodek: %s\n"
#define MSGTR_AODescription_AOAuthor "AO: Popis: %s\nAO: Autor: %s\n"
#define MSGTR_AOComment "AO: Pozn�mka: %s\n"
#define MSGTR_Video_NoVideo "Video: ��dn� video!!!\n"
#define MSGTR_NotInitializeVOPorVO "\nKritick� chyba: Nemohu inicializovat video filtry (-vop) nebo video ovlada� (-vo) !\n"
#define MSGTR_Paused "\n------ M�M PAUZU :-P -------\r"
#define MSGTR_PlaylistLoadUnable "\nNemohu na��st playlist %s\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- MPlayer havaroval kv�li 'Illegal Instruction'.\n"\
"  To m��e b�t chyba v k�du pro rozpozn�n� CPU za b�hu...\n"\
"  Pros�m, p�e�t�te si DOCS/bugreports.html.\n"
#define MSGTR_Exit_SIGILL \
"- MPlayer havaroval kv�li 'Illegal Instruction'.\n"\
"  To se obvykle st�v�, kdy� se ho pokus�te spustit na CPU odli�n�m, ne� pro kter�\n"\
"  byl p�elo�en/optimalizov�n.\n  Ov��te to!\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- MPlayer havaroval kv�li �patn�mu pou�it� CPU/FPU/RAM.\n"\
"  P�elo�te MPlayer s volbou --enable-debug , prove�te 'gdb' backtrace\n"\
"  a disassembly. Pro detaily se pod�vejte do DOCS/bugreports.html#crash\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer havaroval. To by se nem�lo st�t.\n"\
"  M��e to b�t chyba v k�du MPlayeru _nebo_ ve va�ich ovlada��ch _nebo_ ve verzi\n"\
"  va�eho gcc. Pokud si mysl�te, �e je to chyba MPlayeru, p�e�t�te si, pros�m, DOCS/bugreports.html\n"\
"  a pokra�ujte dle tam uveden�ho n�vodu. My v�m nem��eme pomoci, pokud informace, zji�t�n�\n"\
"  dle n�vodu, neuvedete p�i ohla�ov�n� mo�n� chyby.\n"

// mencoder.c:

#define MSGTR_MEncoderCopyright "(C) 2000-2003 Arpad Gereoffy (viz DOCS!)\n"
#define MSGTR_UsingPass3ControllFile "��d�c� soubor pro t�et� pr�b�h (pass3): %s\n"
#define MSGTR_MissingFilename "\nChyb�j�c� jm�no souboru!\n\n"
#define MSGTR_CannotOpenFile_Device "Nelze otev��t soubor/za��zen�\n"
#define MSGTR_ErrorDVDAuth "Chyba p�i autentizaci DVD...\n"
#define MSGTR_CannotOpenDemuxer "Nemohu otev��t demuxer\n"
#define MSGTR_NoAudioEncoderSelected "\nNebyl vybr�n enkoder zvuku (-oac)! Vyberte jeden nebo pou�ijte volbu -nosound. Pro n�pov�du pou�ijte -oac help !\n"
#define MSGTR_NoVideoEncoderSelected "\nNebyl vybr�n enkoder videa (-ovc)! Vyberte jeden. Pro n�pov�du pou�ijte -ovc help !\n"
#define MSGTR_InitializingAudioCodec "Inicializuji audio kodek...\n"
#define MSGTR_CannotOpenOutputFile "Nemohu otev��t v�stupn� soubor '%s'\n"
#define MSGTR_EncoderOpenFailed "Nepovedlo se otev��t enkod�r\n"
#define MSGTR_ForcingOutputFourcc "Vynucuji v�stupn� form�t (fourcc) na %x [%.4s]\n"
#define MSGTR_WritingAVIHeader "Zapisuji hlavi�ku AVI...\n"
#define MSGTR_DuplicateFrames "\n%d duplicitn�ch sn�mk�!    \n"
#define MSGTR_SkipFrame "\nP�eskakuji sn�mek!\n"
#define MSGTR_ErrorWritingFile "%s: chyba p�i z�pisu souboru.\n"
#define MSGTR_WritingAVIIndex "\nZapisuji AVI index...\n"
#define MSGTR_FixupAVIHeader "Opravuji AVI hlavi�ku...\n"
#define MSGTR_RecommendedVideoBitrate "Doporu�en� datov� tok videa pro %s CD: %d\n"
#define MSGTR_VideoStreamResult "\nVideo proud: %8.3f kbit/s  (%d bps)  velikost: %d bajt�  %5.3f sekund  %d sn�mk�\n"
#define MSGTR_AudioStreamResult "\nAudio proud: %8.3f kbit/s  (%d bps)  velikost: %d bajt�  %5.3f sekund\n"

// cfg-mencoder.h:

#define MSGTR_MEncoderMP3LameHelp "\n\n"\
" vbr=<0-4>     metoda prom�nn�ho bitov�ho toku\n"\
"                0: cbr\n"\
"                1: mt\n"\
"                2: rh(v�choz�)\n"\
"                3: abr\n"\
"                4: mtrh\n"\
"\n"\
" abr           pr�m�rn� bitov� tok\n"\
"\n"\
" cbr           konstantn� bitov� tok\n"\
"               Vynut� tak� metodu CBR pro n�sledn� p�ednastaven� ABR mody\n"\
"\n"\
" br=<0-1024>   ur�en� bitov�ho toku v kBit (pouze CBR a ABR)\n"\
"\n"\
" q=<0-9>       kvalita (0-nejvy���, 9-nejni���) (pouze pro VBR)\n"\
"\n"\
" aq=<0-9>      algorimus pro kvalitu (0-nejlep��/nejpomalej��, 9-nejhor��/nejpomalej��)\n"\
"\n"\
" ratio=<1-100> kompresn� pom�r\n"\
"\n"\
" vol=<0-10>    zes�len� zvuku\n"\
"\n"\
" mode=<0-3>    (v�hoz�: auto)\n"\
"                0: stereo\n"\
"                1: joint-stereo\n"\
"                2: dualchannel\n"\
"                3: mono\n"\
"\n"\
" padding=<0-2>\n"\
"                0: ne\n"\
"                1: v�e\n"\
"                2: upravit\n"\
"\n"\
" fast          zapnout rychlej�� k�dov�n� pro n�sledn� p�ednastaven� VBR m�dy.\n"\
"               O n�co ni��� kvalita a vy��� bitv� tok.\n"\
"\n"\
" preset=<value> p�enastaven� profily poskytuj�c� maxim�n� kvalitu.\n"\
"                 medium: k�dov�n� metodou VBR, dobr� kvalita\n"\
"                 (bitov� tok 150-180 kbps)\n"\
"                 standard: k�dov�n� metodou VBR, vysok� kvalita\n"\
"                 (bitov� tok 170-210 kbps)\n"\
"                 extreme: k�dov�n� metodou VBR, velmi vysok� kvalita\n"\
"                 (bitov� tok 200-240 kbps)\n"\
"                 insane: k�dov�n� metodou CBR, nevy��� p�ednastaven� kvalita\n"\
"                 (bitov� tok 320 kbps)\n"\
"                 <8-320>: hodnota bitov�ho toku pro metodu ABR.\n\n"


// open.c, stream.c:
#define MSGTR_CdDevNotfound "CD-ROM za��zen� '%s' nenalezeno!\n"
#define MSGTR_ErrTrackSelect "Chyba p�i v�b�ru VCD stopy!"
#define MSGTR_ReadSTDIN "�tu ze stdin...\n"
#define MSGTR_UnableOpenURL "Nelze otev��t URL: %s\n"
#define MSGTR_ConnToServer "P�ipojen k serveru: %s\n"
#define MSGTR_FileNotFound "Soubor nenalezen: '%s'\n"

#define MSGTR_SMBInitError "Nemohu inicializovat knihovnu libsmbclient: %d\n"
#define MSGTR_SMBFileNotFound "Nemohu otev��t soubor ze s�t�: '%s'\n"
#define MSGTR_SMBNotCompiled "MPlayer nebyl p�elo�en s podporou SMB\n"

#define MSGTR_CantOpenDVD "Nelze otev��t DVD za��zen�: %s\n"
#define MSGTR_DVDwait "�tu strukturu disku, pros�m �ekejte...\n"
#define MSGTR_DVDnumTitles "Na tomto DVD je %d titul�.\n"
#define MSGTR_DVDinvalidTitle "Neplatn� ��slo DVD titulu: %d\n"
#define MSGTR_DVDnumChapters "Na tomto DVD je %d kapitol.\n"
#define MSGTR_DVDinvalidChapter "Neplatn� ��slo kapitoly DVD: %d\n"
#define MSGTR_DVDnumAngles "Na tomto DVD je %d �hl� pohledu.\n"
#define MSGTR_DVDinvalidAngle "Neplatn� ��slo �hlu pohledu DVD: %d\n"
#define MSGTR_DVDnoIFO "Nemohu otev��t soubor IFO pro DVD titul %d.\n"
#define MSGTR_DVDnoVOBs "Nemohu otev��t VOB soubor (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD �sp�n� otev�eno!\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "Upozorn�n�! Hlavi�ka audio proudu %d p�edefinov�na!\n"
#define MSGTR_VideoStreamRedefined "Upozorn�n�! Hlavi�ka video proudu %d p�edefinov�na!\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: P��li� mnoho (%d v %d bajtech) audio paket� v bufferu!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: P��li� mnoho (%d v %d bajtech) video paket� v bufferu!\n"
#define MSGTR_MaybeNI "(mo�n� p�ehr�v�te neprokl�dan� proud/soubor nebo kodek selhal)\n"
#define MSGTR_SwitchToNi "\nDetekov�n �patn� prokl�dan� soubor .AVI - p�epnuto do -ni modu!\n"
#define MSGTR_Detected_XXX_FileFormat "Detekov�n %s form�t souboru!\n"
#define MSGTR_DetectedAudiofile "Detekov�n audio soubor!\n"
#define MSGTR_NotSystemStream "Toto nen� proud ve form�tu MPEG System... (mo�n� Transportn� proud?)\n"
#define MSGTR_InvalidMPEGES "Neplatn� MPEG-ES proud!? Kontaktujte autora, mo�n� to je chyba (bug) :(\n"
#define MSGTR_FormatNotRecognized "======== Bohu�el, form�t tohoto souboru nebyl rozpozn�n/nen� podporov�n =======\n"\
                                 "= Pokud je soubor typu AVI, ASF nebo obsahuje MPEG proud, kontaktujte autora! =\n"
#define MSGTR_MissingVideoStream "��dn� video proud nenalezen!\n"
#define MSGTR_MissingAudioStream "��dn� audio proud nenalezen...  ->beze zvuku\n"
#define MSGTR_MissingVideoStreamBug "Chyb�j�c� video proud!? Kontaktujte autora, mo�n� to je chyba (bug) :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: soubor neobsahuje vybran� audio nebo video proud\n"

#define MSGTR_NI_Forced "Vynucen"
#define MSGTR_NI_Detected "Detekov�n"
#define MSGTR_NI_Message "%s NEPROKL�DAN� form�t souboru AVI!\n"

#define MSGTR_UsingNINI "Pou��v�m NEPROKL�DAN� po�kozen� form�t souboru AVI!\n" //tohle taky n�jak opravit
#define MSGTR_CouldntDetFNo "Nemohu ur�it po�et sn�mk� (pro absolutn� posun)  \n"
#define MSGTR_CantSeekRawAVI "Nelze se posouvat v surov�ch (raw) .AVI proudech! (Pot�ebuji index, zkuste pou��t volbu -idx !)  \n"
#define MSGTR_CantSeekFile "Nemohu posouvat v tomto souboru!  \n"

#define MSGTR_EncryptedVOB "K�dovan� VOB soubor (p�elo�eno bez podpory libcss)! P�e�t�te si DOCS/DVD\n"
#define MSGTR_EncryptedVOBauth "Zak�dovan� proud, ale autentizaci jste nepo�adoval!!\n"

#define MSGTR_MOVcomprhdr "MOV: Komprimovan� hlavi�ky nejsou (je�t�) podporov�ny!\n"
#define MSGTR_MOVvariableFourCC "MOV: Upozorn�n�! prom�nn� FOURCC detekov�na!?\n"
#define MSGTR_MOVtooManyTrk "MOV: Upozorn�n�! P��li� mnoho stop!"
#define MSGTR_FoundAudioStream "==> Nalezen audio proud: %d\n"
#define MSGTR_FoundVideoStream "==> Nalezen video proud: %d\n"
#define MSGTR_DetectedTV "Detekov�na TV! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "Nemohu otev��t ogg demuxer\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: Hled�m audio proud (id:%d)\n"
#define MSGTR_CannotOpenAudioStream "Nemohu otev��t audio proud: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "Nemohu otev��t proud s titulky: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "Nepovedlo se otev��t audio demuxer: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "Nepovedlo se otev��t demuxer pro titulky: %s\n"
#define MSGTR_TVInputNotSeekable "TV vstup neumo��uje posun (seek)! (Pravd�podobn� \"posun\" bude pou�it pro zm�nu kan�l� ;)\n"
#define MSGTR_DemuxerInfoAlreadyPresent "Informace o demuxeru %s ji� p��tomno\n!"
#define MSGTR_ClipInfo "Informace o klipu: \n"

#define MSGTR_LeaveTelecineMode "\ndemux_mpg: Detekov�na progresivn� sekvence, opou�t�m m�d 3:2 TELECINE\n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: Detekov�no 3:2 TELECINE, aktivuji inverzn� telecine fx. FPS zm�n�nno na %5.3f!\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "nemohu otev��t kodek\n"
#define MSGTR_CantCloseCodec "nemohu uzav��t kodek\n"

#define MSGTR_MissingDLLcodec "CHYBA: Nemohu otev��t pot�ebn� DirectShow kodek: %s\n"
#define MSGTR_ACMiniterror "Nemohu na��st/inicializovat Win32/ACM AUDIO kodek (chyb�j�c� soubor DLL?)\n"
#define MSGTR_MissingLAVCcodec "Nemohu naj�t kodek '%s' v libavcodec...\n"

#define MSGTR_MpegNoSequHdr "MPEG: Kritick� chyba: EOF - konec souboru v pr�b�hu vyhled�v�n� hlavi�ky sekvence\n"
#define MSGTR_CannotReadMpegSequHdr "Kritick� chyba: Nelze p�e��st hlavi�ku sekvence!\n"
#define MSGTR_CannotReadMpegSequHdrEx "Kritick� chyba: Nelze p�e��st roz���en� hlavi�ky sekvence!\n"
#define MSGTR_BadMpegSequHdr "MPEG: �patn� hlavi�ka sekvence!\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: �patn� roz���en� hlavi�ky sekvence!\n"

#define MSGTR_ShMemAllocFail "Nemohu alokovat sd�lenou pam�\n"
#define MSGTR_CantAllocAudioBuf "Nemohu alokovat pam� pro v�stupn� audio buffer\n"

#define MSGTR_UnknownAudio "Nezn�m�/chyb�j�c� audio form�t -> beze zvuku\n"

#define MSGTR_UsingExternalPP "[PP] Pou��v�m extern� filtr pro postprocessing , max q = %d\n"
#define MSGTR_UsingCodecPP "[PP] Pou��v�m integrovan� postprocesing kodeku, max q = %d\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "Video atribut '%s' nen� podporov�n vybran�m vo & vd! \n"
#define MSGTR_VideoCodecFamilyNotAvailableStr "Po�adovan� rodina video kodeku [%s] (vfm=%d) nen� dostupn� (aktivujte ji p�i kompilace!)\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "Po�adovan� rodina audio kodeku [%s] (afm=%d) not available (aktivujte ji p�i kompilace!)\n"
#define MSGTR_OpeningVideoDecoder "Otev�r�m viedo dekod�r: [%s] %s\n"
#define MSGTR_OpeningAudioDecoder "Otev�r�m audio decod�r: [%s] %s\n"
#define MSGTR_UninitVideoStr "uninit video: %d  \n"
#define MSGTR_UninitAudioStr "uninit audio: %d  \n"
#define MSGTR_VDecoderInitFailed "VDecoder - inicializace selhala :(\n"
#define MSGTR_ADecoderInitFailed "ADecoder - inicializace selhala :(\n"
#define MSGTR_ADecoderPreinitFailed "ADecoder preinit selhal :(\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: Alokuji %d byt� pro vstupn� buffer\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: Alokuji %d + %d = %d byt� pro v�stupn� buffer\n"

// LIRC:
#define MSGTR_SettingUpLIRC "Nastavuji podporu lirc ...\n"
#define MSGTR_LIRCdisabled "Nebudete moci pou��vat d�lkov� ovlada�.\n"
#define MSGTR_LIRCopenfailed "Selhal pokus o otev�en� podpory LIRC!\n"
#define MSGTR_LIRCcfgerr "Selhalo �ten� konfigura�n�ho souboru LIRC %s !\n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "Nemohu nal�zt video filtr '%s'\n"
#define MSGTR_CouldNotOpenVideoFilter "Nemohu otev��tvideo filtr '%s'\n"
#define MSGTR_OpeningVideoFilter "Otev�r�m video filtr: "
#define MSGTR_CannotFindColorspace "Nemohu nal�zt spole�n� barevn� prostor, ani p�i vlo�en� 'scale' :(\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDec: kodek nenastavil sh->disp_w a sh->disp_h, pokou��m se to p�ekonat!\n"
#define MSGTR_VoConfigRequest "VDec: vo po�adovan� konfigurace - %d x %d (preferovan� csp: %s)\n"
#define MSGTR_CouldNotFindColorspace "Nemohu nal�zt spole�n� barevn� prostor - zkou��m to znovu s -vop scale...\n"
#define MSGTR_MovieAspectIsSet "Obraz - pom�r stran je %.2f:1 - p�edb�n� m�n�m velikost obrazu.\n"
#define MSGTR_MovieAspectUndefined "Obraz - pom�r stran nen� definov�n - nem�n�m velikost.\n"

// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "O aplikaci"
#define MSGTR_FileSelect "V�b�r souboru ..."
#define MSGTR_SubtitleSelect "Vybrat titulky ..."
#define MSGTR_PlayList "Soubory pro p�ehr�n�"
#define MSGTR_Equalizer "Ekvaliz�r"
#define MSGTR_SkinBrowser "Prohl�e� t�mat"
#define MSGTR_OtherSelect "Vybrat ..."
#define MSGTR_AudioFileSelect "Vybrat extern� zvukov� kan�l ..."
#define MSGTR_Network "S� ..." //opravit
#define MSGTR_Preferences "Nastaven�" // P�edvolby?
#define MSGTR_FontSelect "Vybrat font ..."
#define MSGTR_OSSPreferences "Konfigurace ovlada�e OSS"
#define MSGTR_SDLPreferences "Konfigurace ovlada�e SDL"
#define MSGTR_NoMediaOpened "nic nen� otev�eno"
#define MSGTR_VCDTrack "VCD stopa %d"
#define MSGTR_NoChapter "��dn� kapitola" //bez kapitoly?
#define MSGTR_Chapter "kapitola %d"
#define MSGTR_NoFileLoaded "��dn� soubor nena�ten"

// --- buttons ---
#define MSGTR_Ok "Ok"
#define MSGTR_Cancel "Zru�it"
#define MSGTR_Add "P�idat"
#define MSGTR_Remove "Odebrat"
#define MSGTR_Clear "Vynulovat"
#define MSGTR_Config "Konfigurace"
#define MSGTR_ConfigDriver "Konfigurovat ovlada�"
#define MSGTR_Browse "Prohl�et"

// --- error messages ---
#define MSGTR_NEMDB "Bohu�el, nedostatek pam�ti pro buffer pro kreslen�."
#define MSGTR_NEMFMR "Bohu�el, nedostatek pam�ti pro vytv��en� menu."
#define MSGTR_IDFGCVD "Bohu�el, nebyl nalezen video ovlada� kompatibiln� s GUI."
#define MSGTR_NEEDLAVCFAME "Bohu�el, nen� mo�no p�ehr�vat jin� soubory ne� mpeg s kartou DXR3/H+ bez p�ek�dov�n�.\nPros�m, aktivujte lavc nebo fame v konfiguraci DXR3/H+."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[t�mata] chyba v konfigura�n�m soubory t�mat %d: %s"
#define MSGTR_SKIN_WARNING1 "[t�mata] v konfigura�n�m soubory t�mat na ��dce %d: widget nalezen ale p�ed  \"section\" nenalezen ( %s )"
#define MSGTR_SKIN_WARNING2 "[t�mata] v konfigura�n�m soubory t�mat na ��dce %d: widget nalezen ale p�ed \"subsection\" nenalezen (%s)"
#define MSGTR_SKIN_WARNING3 "[t�mata] v konfigura�n�m soubory t�mat na ��dce %d: widget (%s) nepodporuje tuto subsekci"
#define MSGTR_SKIN_BITMAP_16bit  "bitmapa s hloubkou 16 bitov� a m�n� nepodporov�na ( %s ).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "soubor nenalezen ( %s )\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "chyba �ten� bmp ( %s )\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "chyba �ten� tga ( %s )\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "chyba �ten� png ( %s )\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "form�t RLE packed tga nepodporov�n ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "nezn�m� typ souboru ( %s )\n"
#define MSGTR_SKIN_BITMAP_ConvertError "chyba konverze z 24 bit do 32 bit ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "nezn�m� zpr�va: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "nedostatek pam�ti\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "deklarov�no p��li� mnoho font�\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "soubor fontu nenalezen\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "soubor obraz� fontu nenalezen\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "neexistuj�c� identifik�tor fontu ( %s )\n"
#define MSGTR_SKIN_UnknownParameter "nezn�m� parametr ( %s )\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[prohl�e� t�mat] nedostatek pam�ti.\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "Skin nenalezen ( %s ).\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "Chyba p�i �ten� konfigura�n�ho souboru t�mat ( %s ).\n"
#define MSGTR_SKIN_LABEL "T�mata:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "O aplikaci MPlayer"
#define MSGTR_MENU_Open "Otev��t ..."
#define MSGTR_MENU_PlayFile "P�ehr�t soubor ..."
#define MSGTR_MENU_PlayVCD "P�ehr�t VCD ..."
#define MSGTR_MENU_PlayDVD "P�ehr�t DVD ..."
#define MSGTR_MENU_PlayURL "�ten� URL ..."
#define MSGTR_MENU_LoadSubtitle "Na��st titulky ..."
#define MSGTR_MENU_DropSubtitle "Zahodit titulky ..."
#define MSGTR_MENU_LoadExternAudioFile "Na��st extern� soubor se zvukem ..."
#define MSGTR_MENU_Playing "Ovl�d�n� p�ehr�v�n�"
#define MSGTR_MENU_Play "P�ehr�t"
#define MSGTR_MENU_Pause "Pauza"
#define MSGTR_MENU_Stop "Zastavit"
#define MSGTR_MENU_NextStream "Dal�� proud"
#define MSGTR_MENU_PrevStream "P�edchoz� proud"
#define MSGTR_MENU_Size "Velikost"
#define MSGTR_MENU_NormalSize "Norm�ln� velikost"
#define MSGTR_MENU_DoubleSize "Dvojn�sobn� velikost"
#define MSGTR_MENU_FullScreen "Cel� obrazovka"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "P�ehr�t disk ..."
#define MSGTR_MENU_ShowDVDMenu "Zobrazit DVD menu"
#define MSGTR_MENU_Titles "Tituly"
#define MSGTR_MENU_Title "Titul %2d"
#define MSGTR_MENU_None "(nic)"
#define MSGTR_MENU_Chapters "Kapitoly"
#define MSGTR_MENU_Chapter "Kapitola %2d"
#define MSGTR_MENU_AudioLanguages "Jazyk zvuku"
#define MSGTR_MENU_SubtitleLanguages "Jazyk titulk�"
#define MSGTR_MENU_PlayList "Soubory pro p�ehr�n�"
#define MSGTR_MENU_SkinBrowser "Prohli�e� t�mat"
#define MSGTR_MENU_Preferences "P�edvolby"
#define MSGTR_MENU_Exit "Konec ..."
#define MSGTR_MENU_Mute "Ztlumit"
#define MSGTR_MENU_Original "P�vodn�"
#define MSGTR_MENU_AspectRatio "Pom�r stran"
#define MSGTR_MENU_AudioTrack "Audio stopa"
#define MSGTR_MENU_Track "Stopa %d"
#define MSGTR_MENU_VideoTrack "Video stopa"

// --- equalizer
#define MSGTR_EQU_Audio "Zvuk"
#define MSGTR_EQU_Video "Obraz"
#define MSGTR_EQU_Contrast "Kontrast: "
#define MSGTR_EQU_Brightness "Jas: "
#define MSGTR_EQU_Hue "Odst�n: "
#define MSGTR_EQU_Saturation "Sytost: "
#define MSGTR_EQU_Front_Left "Lev� p�edn�"
#define MSGTR_EQU_Front_Right "Prav� p�edn�"
#define MSGTR_EQU_Back_Left "Lev� zadn�"
#define MSGTR_EQU_Back_Right "Prav� zadn�"
#define MSGTR_EQU_Center "Centr�ln�"
#define MSGTR_EQU_Bass "Basov�"
#define MSGTR_EQU_All "V�e"
#define MSGTR_EQU_Channel1 "Kan�l 1:"
#define MSGTR_EQU_Channel2 "Kan�l 2:"
#define MSGTR_EQU_Channel3 "Kan�l 3:"
#define MSGTR_EQU_Channel4 "Kan�l 4:"
#define MSGTR_EQU_Channel5 "Kan�l 5:"
#define MSGTR_EQU_Channel6 "Kan�l 6:"

// --- playlist
#define MSGTR_PLAYLIST_Path "Cesta"
#define MSGTR_PLAYLIST_Selected "Vybran� soubory"
#define MSGTR_PLAYLIST_Files "Soubory"
#define MSGTR_PLAYLIST_DirectoryTree "Adres��e"

// --- preferences
#define MSGTR_PREFERENCES_None "Nic"
#define MSGTR_PREFERENCES_AvailableDrivers "Dostupn� ovlada�e:"
#define MSGTR_PREFERENCES_DoNotPlaySound "Nep�ehr�vat zvuk"
#define MSGTR_PREFERENCES_NormalizeSound "Normalizovat zvuk"
#define MSGTR_PREFERENCES_EnEqualizer "Aktivovat ekvaliz�r"
#define MSGTR_PREFERENCES_ExtraStereo "Aktivovat extra stereo"
#define MSGTR_PREFERENCES_Coefficient "Koeficient:"
#define MSGTR_PREFERENCES_AudioDelay "Zpo�d�n� zvuku"
#define MSGTR_PREFERENCES_Audio "Zvuk"
#define MSGTR_PREFERENCES_DoubleBuffer "Aktivovat double buffering"
#define MSGTR_PREFERENCES_DirectRender "Aktivovat direct rendering"
#define MSGTR_PREFERENCES_FrameDrop "Aktivovat zahazov�n� sn�mk�"
#define MSGTR_PREFERENCES_HFrameDrop "Aktivovat TVRD� zahazov�n� sn�mk� (nebezpe�n�)"
#define MSGTR_PREFERENCES_Flip "Obr�tit obraz vzh�ru nohama"
#define MSGTR_PREFERENCES_Panscan "Panscan:"
#define MSGTR_PREFERENCES_Video "Video"
#define MSGTR_PREFERENCES_OSDTimer "�as a ostatn� ukazatele"
#define MSGTR_PREFERENCES_OSDProgress "Pouze ukazatel pozice" // progressbar
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "�as, procenta a celkov� �as"
#define MSGTR_PREFERENCES_Subtitle "Titulky:"
#define MSGTR_PREFERENCES_SUB_Delay "Zpo�d�n�: "
#define MSGTR_PREFERENCES_SUB_FPS "FPS:"
#define MSGTR_PREFERENCES_SUB_POS "Pozice: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "Vypnout automatick� na�ten� titulk�"
#define MSGTR_PREFERENCES_SUB_Unicode "Titulky v UNICODE"
#define MSGTR_PREFERENCES_SUB_MPSUB "P�ev�st dan� titulky do vlastn�ho form�tu titulk� MPlayeru"
#define MSGTR_PREFERENCES_SUB_SRT "P�ev�st dan� titulky do �asov� orientovan�ho form�tu SubViewer (SRT)"
#define MSGTR_PREFERENCES_SUB_Overlap "Zapnout \"p�ekr�v�n�\" titulk�"
#define MSGTR_PREFERENCES_Font "Font:"
#define MSGTR_PREFERENCES_Codecs "Kodeky & demuxer"
#define MSGTR_PREFERENCES_FontFactor "Font faktor:" //????
#define MSGTR_PREFERENCES_PostProcess "Aktivovat postprocessing"
#define MSGTR_PREFERENCES_AutoQuality "Automatick� kontrola kvality:"
#define MSGTR_PREFERENCES_NI "Pou��t parser pro neprokl�dan� AVI form�t"
#define MSGTR_PREFERENCES_IDX "Vytvo�it tabulku index�, pokud je to t�eba"
#define MSGTR_PREFERENCES_VideoCodecFamily "Rodina video kodeku:"
#define MSGTR_PREFERENCES_AudioCodecFamily "Rodina audio kodeku:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "Typ OSD"
#define MSGTR_PREFERENCES_FRAME_Subtitle "Titulky"
#define MSGTR_PREFERENCES_FRAME_Font "Font"
#define MSGTR_PREFERENCES_FRAME_PostProcess "Postprocessing"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "Kodek & demuxer"
#define MSGTR_PREFERENCES_OSS_Device "Za��zen�:"
#define MSGTR_PREFERENCES_OSS_Mixer "Mixer:"
#define MSGTR_PREFERENCES_SDL_Driver "Ovlada�:"
#define MSGTR_PREFERENCES_Message "Pozor, n�kter� nastaven� pot�ebuj� pro svou funkci restartovat p�ehr�v�n�."
#define MSGTR_PREFERENCES_DXR3_VENC "Video enkoder:"
#define MSGTR_PREFERENCES_DXR3_LAVC "Pou��t LAVC (ffmpeg)"
#define MSGTR_PREFERENCES_DXR3_FAME "Pou��t FAME"
#define MSGTR_PREFERENCES_FontEncoding1 "Unicode"
#define MSGTR_PREFERENCES_FontEncoding2 "Z�padoevropsk� jazyky (ISO-8859-1)"
#define MSGTR_PREFERENCES_FontEncoding3 "Z�padoevropsk� jazyky s Eurem (ISO-8859-15)"
#define MSGTR_PREFERENCES_FontEncoding4 "Slovansk�/st�edoevropsk� jazyky (ISO-8859-2)"
#define MSGTR_PREFERENCES_FontEncoding5 "Esperanto, gal�tina, malt�z�tina, ture�tina (ISO-8859-3)"
#define MSGTR_PREFERENCES_FontEncoding6 "Star� Baltsk� k�dov�n� (ISO-8859-4)"
#define MSGTR_PREFERENCES_FontEncoding7 "Cyrilice (ISO-8859-5)"
#define MSGTR_PREFERENCES_FontEncoding8 "Arab�tina (ISO-8859-6)"
#define MSGTR_PREFERENCES_FontEncoding9 "Modern� �e�tina (ISO-8859-7)"
#define MSGTR_PREFERENCES_FontEncoding10 "Ture�tina (ISO-8859-9)"
#define MSGTR_PREFERENCES_FontEncoding11 "Baltick� (ISO-8859-13)"
#define MSGTR_PREFERENCES_FontEncoding12 "Celtic (ISO-8859-14)"
#define MSGTR_PREFERENCES_FontEncoding13 "Hebrej�tina (ISO-8859-8)"
#define MSGTR_PREFERENCES_FontEncoding14 "Ru�tina (KOI8-R)"
#define MSGTR_PREFERENCES_FontEncoding15 "Ukrajin�tina, b�loru�tina (KOI8-U/RU)"
#define MSGTR_PREFERENCES_FontEncoding16 "Jednoduch� ��n�tina (CP936)"
#define MSGTR_PREFERENCES_FontEncoding17 "Tradi�n� ��n�tina (BIG5)"
#define MSGTR_PREFERENCES_FontEncoding18 "Japon�tina (SHIFT-JIS)"
#define MSGTR_PREFERENCES_FontEncoding19 "Korej�tina (CP949)"
#define MSGTR_PREFERENCES_FontEncoding20 "Thaj�tina (CP874)"
#define MSGTR_PREFERENCES_FontEncoding21 "Cyrilice Windows (CP1251)"
#define MSGTR_PREFERENCES_FontNoAutoScale "Bez automatick� velikosti"
#define MSGTR_PREFERENCES_FontPropWidth "Propor�n� dle ���ky obrazu"
#define MSGTR_PREFERENCES_FontPropHeight "Propor�n� dle v��ky obrazu"
#define MSGTR_PREFERENCES_FontPropDiagonal "Propor�n� dle �hlop���ky"
#define MSGTR_PREFERENCES_FontEncoding "K�dov�n�:"
#define MSGTR_PREFERENCES_FontBlur "Rozmaz�n�:"
#define MSGTR_PREFERENCES_FontOutLine "Obrys:"
#define MSGTR_PREFERENCES_FontTextScale "Velikost textu:"
#define MSGTR_PREFERENCES_FontOSDScale "Velikost OSD:"
#define MSGTR_PREFERENCES_SubtitleOSD "Titulky & OSD"
#define MSGTR_PREFERENCES_FRAME_Cache "Vyrovn�vac� pam�"
#define MSGTR_PREFERENCES_FRAME_Misc "Ostatn�"
#define MSGTR_PREFERENCES_Cache "Zapnout vyrovn�vac� pam�"
#define MSGTR_PREFERENCES_LoadFullscreen "Startovat p�es celou obrazovku"
#define MSGTR_PREFERENCES_CacheSize "Velikost vyrovn�vac� pam�ti: "
#define MSGTR_PREFERENCES_XSCREENSAVER "Zastavit XScreenSaver"
#define MSGTR_PREFERENCES_PlayBar "Aktivovat playbar"
#define MSGTR_PREFERENCES_AutoSync "Zapnout automatickou synchronizaci"
#define MSGTR_PREFERENCES_AutoSyncValue "Automatick� synchronizace: "
#define MSGTR_PREFERENCES_Misc "Ostatn�"
#define MSGTR_PREFERENCES_CDROMDevice "Za��zen� CD-ROM:"
#define MSGTR_PREFERENCES_DVDDevice "Za��zen� DVD:"
#define MSGTR_PREFERENCES_FPS "Sn�mkov� rychlost (FPS):"
#define MSGTR_PREFERENCES_ShowVideoWindow "Zobrazovat video okno p�i ne�innosti"

#define MSGTR_ABOUT_UHU "V�voj GUI je sponzorov�n firmou UHU Linux\n"
#define MSGTR_ABOUT_CoreTeam "   Hlavn� v�voj��i programu MPlayer:\n"
#define MSGTR_ABOUT_AdditionalCoders "   Dal�� v�voj��i:\n"
#define MSGTR_ABOUT_MainTesters "   Hlavn� teste�i:\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "Kritick� chyba ..."
#define MSGTR_MSGBOX_LABEL_Error "Chyba ..."
#define MSGTR_MSGBOX_LABEL_Warning "Upozorn�n� ..."

#endif



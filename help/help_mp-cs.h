// Translated by:  Jiri Svoboda, jiri.svoboda@seznam.cz
// Updated by:     Tomas Blaha,  tomas.blaha at kapsa.club.cz
// Synced to 1.105
// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"Pou�it�:          mplayer [p�ep�na�e] [url|cesta/]jm�no_souboru\n"
"\n"
"Z�kladn� p�ep�na�e: (Kompletn� seznam najdete v manu�lov� str�nce.)\n"
" -vo <ovl[:za�]>  ur�it v�st. video ovlada� a za��zen� (seznam: -vo help)\n"
" -ao <ovl[:za�]>  ur�it v�st. audio ovlada� a za��zen� (seznam: -ao help)\n"
#ifdef HAVE_VCD
" vcd://<��slo>    p�ehr�t VCD (Video CD) stopu ze za��zen� m�sto ze souboru\n"
#endif
#ifdef HAVE_LIBCSS
" -dvdauth <za�>   ur�it DVD za��zen� pro autentizaci (pro �ifrovan� disky)\n"
#endif
#ifdef USE_DVDREAD
" dvd://<��slo>    p�ehr�t DVD titul ze za��zen� (mechaniky), m�sto ze souboru\n"
" -alang/-slang    zvolit jazyk zvuku/titulk� na DVD (dvouznakov� k�d zem�)\n"
#endif
" -ss <pozice>     posunout na danou pozici (sekundy nebo hh:mm:ss)\n"
" -nosound         p�ehr�t beze zvuku\n"
" -fs              celoobrazovkov� p�ehr�v�n� (nebo -vm -zoom, viz manu�l)\n"
" -x <x> -y <y>    rozli�en� obrazu (pro pou�it� s -vm �i -zoom)\n"
" -sub <soubor>    zvolit soubor s titulky (viz tak� -subfps, -subdelay)\n"
" -playlist <soubor> ur�it soubor s playlistem\n"
" -vid x -aid y    vybrat video (x) a audio (y) proud pro p�ehr�n�\n"
" -fps x -srate y  zm�nit video (x fps) a audio (y Hz) frekvence\n"
" -pp <kvalita>    aktivovat n�sledn� zpracov�n� (podrobnosti v manu�lu)\n"
" -framedrop       povolit zahazov�n� sn�mk� (pro pomal� stroje)\n"
"\n"
"Z�kladn� kl�vesy: (Kompletn� seznam je v manu�lu a tak� v input.conf.)\n"
" <-  nebo  ->     posun vzad/vp�ed o 10 sekund\n"
" nahoru �i dol�   posun vzad/vp�ed o 1 minutu\n"
" pgup �i pgdown   posun vzad/vp�ed o 10 minut\n"
" < nebo >         posun vzad/vp�ed v playlistu\n"
" p nebo mezern�k  pauza p�i p�ehr�v�n� (pokra�ov�n� stiskem kter�koliv kl�vesy)\n"
" q nebo ESC       konec p�ehr�v�n� a ukon�en� programu\n"
" + nebo -         upravit zpo�d�n� zvuku v kroc�ch +/- 0,1 sekundy\n"
" o                cyklick� zm�na re�imu OSD: nic / pozice / pozice a �as\n"
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
#define MSGTR_IntBySignal "\nMPlayer p�eru�en sign�lem %d v modulu %s.\n"
#define MSGTR_NoHomeDir "Nemohu nal�zt dom�c� adres��.\n"
#define MSGTR_GetpathProblem "Nastal probl�m s get_path(\"config\")\n"
#define MSGTR_CreatingCfgFile "Vytv���m konfigura�n� soubor: %s\n"
#define MSGTR_InvalidVOdriver "�patn� jm�no v�stupn�ho video ovlada�e: %s\nSeznam dostupn�ch ovlada�� zobraz�te pomoc� '-vo help'.\n"
#define MSGTR_InvalidAOdriver "�patn� jm�no v�stupn�ho audio ovlada�e: %s\nSeznam dostupn�ch ovlada�� zobraz�te pomoc� '-ao help'.\n"
#define MSGTR_CopyCodecsConf "(Zkop�rujte nebo vytvo�te odkaz na etc/codecs.conf (ze zdrojov�ch k�d� MPlayeru) do ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "Pou��v�m v�choz� (zabudovan�) codecs.conf.\n"
#define MSGTR_CantLoadFont "Nemohu na��st p�smo: %s\n"
#define MSGTR_CantLoadSub "Nemohu na��st titulky: %s\n"
#define MSGTR_ErrorDVDkey "P�i zpracov�n� DVD kl��e do�lo k chyb�.\n"
#define MSGTR_CmdlineDVDkey "Roz�ifrov�v�m pomoc� zadan�ho DVD kl��e.\n"
#define MSGTR_DVDauthOk "Autentiza�n� sekvence na DVD vypad� vpo��dku.\n"
#define MSGTR_DumpSelectedStreamMissing "dump: Kritick� chyba: po�adovan� proud chyb�!\n"
#define MSGTR_CantOpenDumpfile "Nelze otev��t soubor pro dump!!!\n"
#define MSGTR_CoreDumped "J�dro vydumpov�no ;)\n"
#define MSGTR_FPSnotspecified "V hlavi�ce souboru nen� ud�n (nebo je �patn�) FPS! Pou�ijte volbu -fps !\n"
#define MSGTR_TryForceAudioFmtStr "Pokou��m se vynutit rodinu audiokodeku %s...\n"
#define MSGTR_CantFindAfmtFallback "Nemohu nal�zt audio kodek v po�adovan� rodin�, pou�iji ostatn� rodiny.\n"
#define MSGTR_CantFindAudioCodec "Nemohu nal�zt kodek pro audio form�t 0x%X !\n"
#define MSGTR_CouldntInitAudioCodec "Nelze inicializovat audio kodek - nebude zvuk!\n"
#define MSGTR_TryForceVideoFmtStr "Poku��m se vynutit rodinu videokodeku %s...\n"
#define MSGTR_CantFindVideoCodec "Nemohu nal�zt kodek pro vybran� -vo a video form�t 0x%X.\n"
#define MSGTR_VOincompCodec "Bohu�el, vybran� video_out za��zen� nen� kompatibiln� s t�mto kodekem.\n"
#define MSGTR_CannotInitVO "Kritick� chyba: Nemohu inicializovat video ovlada�!\n"
#define MSGTR_CannotInitAO "Nemohu otev��t/inicializovat audio za��zen� -> nebude zvuk.\n"
#define MSGTR_StartPlaying "Za��n�m p�ehr�vat...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"         ***********************************************************\n"\
"         ****  V� syst�m je p��li� POMAL� pro toto p�ehr�v�n�! ****\n"\
"         ***********************************************************\n\n"\
"Mo�n� p���iny, probl�my a �e�en�:\n"\
"- Nej�ast�j��: �patn�/chybn� _zvukov�_ ovlada�\n!"\
"  - Zkuste -ao sdl nebo pou�ijte ALSA 0.5 �i oss emulaci z ALSA 0.9.\n"\
"  - Pohrajte si s r�zn�mi hodnotami -autosync, pro za��tek t�eba 30.\n"\
"- Pomal� obrazov� v�stup\n"\
"  - Zkuste jin� -vo ovlada� (seznam: -vo help) nebo zkuste -framedrop!\n"\
"- Pomal� CPU\n"\
"  - Nezkou�ejte p�ehr�t velk� DVD/DivX na pomal� CPU! Zkuste -hardframedrop.\n"\
"- Po�kozen� soubor.\n"\
"  - Zkuste r�zn� kombinace t�chto voleb: -nobps -ni -forceidx -mc 0.\n"\
"- P�i p�ehr�v�n� z pomal�ch m�di� (NFS/SMB, DVD, VCD, atd.)\n"\
"  - Zkuste -cache 8192.\n"\
"- Pou��v�te -cache pro neprokl�dan� AVI soubory?\n"\
"  - Zkuste -nocache.\n"\
"Tipy na vylad�n� a zrychlen� najdete v DOCS/en/video.html a DOCS/en/sound.html.\n"\
"Pokud nic z toho nepom��e, p�e�t�te si DOCS/en/bugreports.html.\n\n"

#define MSGTR_NoGui "MPlayer byl p�elo�en BEZ podpory GUI.\n"
#define MSGTR_GuiNeedsX "GUI MPlayeru vy�aduje X11.\n"
#define MSGTR_Playing "P�ehr�v�m %s\n"
#define MSGTR_NoSound "Audio: beze zvuku!!!\n"
#define MSGTR_FPSforced "FPS vynuceno na hodnotu %5.3f  (ftime: %5.3f)\n"
#define MSGTR_CompiledWithRuntimeDetection "P�elo�eno s detekc� CPU za b�hu - UPOZORN�N� - toto nen� optim�ln�!\nAbyste z�skali co nejv�t�� v�kon, p�elo�te znovu mplayer ze zdrojov�ho k�du\ns p�ep�na�em --disable-runtime-cpudetection\n"
#define MSGTR_CompiledWithCPUExtensions "P�elo�eno pro CPU x86 s roz���en�mi:"
#define MSGTR_AvailableVideoOutputPlugins "Dostupn� z�suvn� moduly video v�stupu:\n"
#define MSGTR_AvailableVideoOutputDrivers "Dostupn� ovlada�e video v�stupu:\n"
#define MSGTR_AvailableAudioOutputDrivers "Dostupn� ovlada�e audio v�stupu:\n"
#define MSGTR_AvailableAudioCodecs "Dostupn� audio kodeky:\n"
#define MSGTR_AvailableVideoCodecs "Dostupn� video kodeky:\n"
#define MSGTR_AvailableAudioFm "\nDostupn� (p�ikompilovan�) rodiny audio kodek�/ovlada��:\n"
#define MSGTR_AvailableVideoFm "\nDostupn� (p�ikompilovan�) rodiny video kodek�/ovlada��:\n"
#define MSGTR_AvailableFsType "Dostupn� zp�soby zm�ny hladiny p�i celoobrazovkov�m zobrazen�:\n"
#define MSGTR_UsingRTCTiming "Pou�ito linuxov� hardwarov� �asov�n� RTC (%ldHz).\n"
#define MSGTR_CannotReadVideoProperties "Video: nelze p�e��st vlastnosti.\n"
#define MSGTR_NoStreamFound "Nenalezen ��dn� proud.\n"
#define MSGTR_InitializingAudioCodec "Inicializuji audio kodek...\n"
#define MSGTR_ErrorInitializingVODevice "Chyba p�i otev�r�n�/inicializaci vybran�ho video_out (-vo) za��zen�.\n"
#define MSGTR_ForcedVideoCodec "Vynucen video kodek: %s\n"
#define MSGTR_ForcedAudioCodec "Vynucen audio kodek: %s\n"
#define MSGTR_AODescription_AOAuthor "AO: Popis: %s\nAO: Autor: %s\n"
#define MSGTR_AOComment "AO: Pozn�mka: %s\n"
#define MSGTR_Video_NoVideo "Video: ��dn� video\n"
#define MSGTR_NotInitializeVOPorVO "\nKritick� chyba: Nemohu inicializovat video filtry (-vop) nebo video ovlada� (-vo) !\n"
#define MSGTR_Paused "\n===== POZASTAVENO =====\r"
#define MSGTR_PlaylistLoadUnable "\nNemohu na��st playlist %s.\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- MPlayer havaroval kv�li 'Illegal Instruction'.\n"\
"  To m��e b�t chyba v k�du pro rozpozn�n� CPU za b�hu...\n"\
"  Pros�m, p�e�t�te si DOCS/en/bugreports.html.\n"
#define MSGTR_Exit_SIGILL \
"- MPlayer havaroval kv�li 'Illegal Instruction'.\n"\
"  To se obvykle st�v�, kdy� se ho pokus�te spustit na CPU odli�n�m, ne� pro kter�\n"\
"  byl p�elo�en/optimalizov�n.\n  Ov��te to!\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- MPlayer havaroval kv�li �patn�mu pou�it� CPU/FPU/RAM.\n"\
"  P�elo�te MPlayer s volbou --enable-debug , prove�te 'gdb' backtrace\n"\
"  a disassembly. Detaily najdete v DOCS/en/bugreports.html#crash.\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer havaroval. To by se nem�lo st�t.\n"\
"  M��e to b�t chyba v k�du MPlayeru _nebo_ ve va�ich ovlada��ch _nebo_ ve verzi\n"\
"  va�eho gcc. Pokud si mysl�te, �e je to chyba MPlayeru, p�e�t�te si, pros�m,\n"\
"  DOCS/en/bugreports.html a pokra�ujte dle tam uveden�ho n�vodu. My v�m nem��eme\n"\
"  pomoci, pokud tyto informace neuvedete p�i ohla�ov�n� mo�n� chyby.\n"

// mencoder.c:

#define MSGTR_UsingPass3ControllFile "��dic� soubor pro t�et� pr�b�h (pass3): %s\n"
#define MSGTR_MissingFilename "\nChyb� jm�no souboru.\n\n"
#define MSGTR_CannotOpenFile_Device "Nelze otev��t soubor/za��zen�.\n"
#define MSGTR_ErrorDVDAuth "Chyba p�i autentizaci DVD.\n"
#define MSGTR_CannotOpenDemuxer "Nemohu otev��t demuxer.\n"
#define MSGTR_NoAudioEncoderSelected "\nNebyl vybr�n enkod�r zvuku (-oac). N�jak� vyberte nebo pou�ijte -nosound. Pou�ijte -oac help!\n"
#define MSGTR_NoVideoEncoderSelected "\nNebyl vybr�n enkod�r videa (-ovc). N�jak� vyberte. Pou�ijte -ovc help!\n"
#define MSGTR_InitializingAudioCodec "Inicializuji audio kodek...\n"
#define MSGTR_CannotOpenOutputFile "Nemohu otev��t v�stupn� soubor '%s'\n"
#define MSGTR_EncoderOpenFailed "Nepovedlo se otev��t enkod�r\n"
#define MSGTR_ForcingOutputFourcc "Vynucuji v�stupn� fourcc na %x [%.4s]\n"
#define MSGTR_WritingAVIHeader "Zapisuji hlavi�ku AVI...\n"
#define MSGTR_DuplicateFrames "\n%d opakuj�c�ch se sn�mk�!\n"
#define MSGTR_SkipFrame "\nP�eskakuji sn�mek!\n"
#define MSGTR_ErrorWritingFile "%s: chyba p�i z�pisu souboru.\n"
#define MSGTR_WritingAVIIndex "\nZapisuji AVI index...\n"
#define MSGTR_FixupAVIHeader "Opravuji hlavi�ku AVI...\n"
#define MSGTR_RecommendedVideoBitrate "Doporu�en� datov� tok videa pro CD %s: %d\n"
#define MSGTR_VideoStreamResult "\nVideo proud: %8.3f kbit/s  (%d bps)  velikost: %d bajt�  %5.3f sekund  %d sn�mk�\n"
#define MSGTR_AudioStreamResult "\nAudio proud: %8.3f kbit/s  (%d bps)  velikost: %d bajt�  %5.3f sekund\n"

// cfg-mencoder.h:

#define MSGTR_MEncoderMP3LameHelp "\n\n"\
" vbr=<0-4>     metoda prom�nn�ho datov�ho toku\n"\
"                0: cbr\n"\
"                1: mt\n"\
"                2: rh(v�choz�)\n"\
"                3: abr\n"\
"                4: mtrh\n"\
"\n"\
" abr           pr�m�rn� datov� tok\n"\
"\n"\
" cbr           konstantn� datov� tok\n"\
"               Vynut� tak� metodu CBR pro n�sledn� p�ednastaven� ABR m�dy\n"\
"\n"\
" br=<0-1024>   ur�en� datov�ho toku v kBit (pouze CBR a ABR)\n"\
"\n"\
" q=<0-9>       kvalita (0-nejvy���, 9-nejni���) (pouze pro VBR)\n"\
"\n"\
" aq=<0-9>      kvalita algoritmu (0-nejlep��/nejpomalej��, 9-nejhor��/nejrychlej��)\n"\
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
"               O n�co ni��� kvalita a vy��� datov� tok.\n"\
"\n"\
" preset=<value> p�ednastaven� profily poskytuj�c� maxim�n� kvalitu.\n"\
"                 medium: k�dov�n� metodou VBR, dobr� kvalita\n"\
"                 (datov� tok 150-180 kbps)\n"\
"                 standard: k�dov�n� metodou VBR, vysok� kvalita\n"\
"                 (datov� tok 170-210 kbps)\n"\
"                 extreme: k�dov�n� metodou VBR, velmi vysok� kvalita\n"\
"                 (datov� tok 200-240 kbps)\n"\
"                 insane: k�dov�n� metodou CBR, nejvy��� p�ednastaven� kvalita\n"\
"                 (datov� tok 320 kbps)\n"\
"                 <8-320>: hodnota pr�m�rn�ho datov�ho toku pro metodu ABR.\n\n"


// open.c, stream.c:
#define MSGTR_CdDevNotfound "CD-ROM za��zen� '%s' nenalezeno.\n"
#define MSGTR_ErrTrackSelect "Chyba p�i v�b�ru VCD stopy."
#define MSGTR_ReadSTDIN "�tu ze stdin...\n"
#define MSGTR_UnableOpenURL "Nelze otev��t URL: %s\n"
#define MSGTR_ConnToServer "P�ipojen k serveru: %s\n"
#define MSGTR_FileNotFound "Soubor nenalezen: '%s'\n"

#define MSGTR_SMBInitError "Nemohu inicializovat knihovnu libsmbclient: %d\n"
#define MSGTR_SMBFileNotFound "Nemohu otev��t soubor ze s�t�: '%s'\n"
#define MSGTR_SMBNotCompiled "MPlayer nebyl p�elo�en s podporou SMB\n"

#define MSGTR_CantOpenDVD "Nelze otev��t DVD za��zen�: %s\n"
#define MSGTR_DVDwait "�tu strukturu disku, pros�m �ekejte...\n"
#define MSGTR_DVDnumTitles "Po�et titul� na tomto DVD: %d\n"
#define MSGTR_DVDinvalidTitle "�patn� ��slo DVD titulu: %d\n"
#define MSGTR_DVDnumChapters "Po�et kapitol na tomto DVD: %d\n"
#define MSGTR_DVDinvalidChapter "�patn� ��slo kapitoly DVD: %d\n"
#define MSGTR_DVDnumAngles "Po�et �hl� pohledu na tomto DVD: %d\n"
#define MSGTR_DVDinvalidAngle "�patn� ��slo �hlu pohledu DVD: %d\n"
#define MSGTR_DVDnoIFO "Nemohu otev��t soubor IFO pro DVD titul %d.\n"
#define MSGTR_DVDnoVOBs "Nemohu otev��t VOB soubor titulu (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD �sp�n� otev�eno.\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "UPOZORN�N�: Hlavi�ka audio proudu %d p�edefinov�na!\n"
#define MSGTR_VideoStreamRedefined "UPOZORN�N�: Hlavi�ka video proudu %d p�edefinov�na!\n"
#define MSGTR_TooManyAudioInBuffer "\nP��li� mnoho audio paket� ve vyrovn�vac� pam�ti: (%d v %d bajtech)\n"
#define MSGTR_TooManyVideoInBuffer "\nP��li� mnoho video paket� ve vyrovn�vac� pam�ti: (%d v %d bajtech)\n"
#define MSGTR_MaybeNI "Mo�n� p�ehr�v�te neprokl�dan� proud/soubor nebo kodek selhal?\n"\
		      "V AVI souborech zkuste vynutit neprokl�dan� re�im pomoc� volby -ni.\n"
#define MSGTR_SwitchToNi "\nDetekov�n �patn� prokl�dan� AVI soubor - p�ep�n�m do -ni m�du...\n"
#define MSGTR_Detected_XXX_FileFormat "Detekov�n form�t souboru %s.\n"
#define MSGTR_DetectedAudiofile "Detekov�n zvukov� soubor.\n"
#define MSGTR_NotSystemStream "Toto nen� form�t MPEG System Stream... (mo�n� Transport Stream?)\n"
#define MSGTR_InvalidMPEGES "�patn� MPEG-ES proud??? Kontaktujte autora, mo�n� to je chyba :(\n"
#define MSGTR_FormatNotRecognized "======= Bohu�el, form�t tohoto souboru nebyl rozpozn�n/nen� podporov�n =======\n"\
                                  "==== Pokud je soubor AVI, ASF nebo MPEG proud, kontaktujte prosim autora! ====\n"
#define MSGTR_MissingVideoStream "��dn� video proud nenalezen.\n"
#define MSGTR_MissingAudioStream "��dn� audio proud nenalezen -> nebude zvuk.\n"
#define MSGTR_MissingVideoStreamBug "Chyb� video proud!? Kontaktujte autora, mo�n� to je chyba :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: soubor neobsahuje vybran� audio nebo video proud.\n"

#define MSGTR_NI_Forced "Vynucen"
#define MSGTR_NI_Detected "Detekov�n"
#define MSGTR_NI_Message "%s NEPROKL�DAN� form�t souboru AVI.\n"

#define MSGTR_UsingNINI "Pou��v�m NEPROKL�DAN� po�kozen� form�t souboru AVI.\n" //tohle taky n�jak opravit
#define MSGTR_CouldntDetFNo "Nemohu ur�it po�et sn�mk� (pro absolutn� posun)\n"
#define MSGTR_CantSeekRawAVI "Nelze se posouvat v surov�ch (raw) AVI proudech! (Pot�ebuji index, zkuste pou��t volbu -idx.)\n"
#define MSGTR_CantSeekFile "Nemohu se posouvat v tomto souboru.\n"

#define MSGTR_EncryptedVOB "�ifrovan� VOB soubor (P�elo�eno bez podpory libcss!) P�e�t�te si DOCS/en/cd-dvd.html.\n"
#define MSGTR_EncryptedVOBauth "Za�ifrovan� proud, ale autentizaci jste nepo�adoval!\n"

#define MSGTR_MOVcomprhdr "MOV: Komprimovan� hlavi�ky nejsou (je�t�) podporov�ny.\n"
#define MSGTR_MOVvariableFourCC "MOV: UPOZORN�N�: Prom�nn� FOURCC detekov�na!?\n"
#define MSGTR_MOVtooManyTrk "MOV: UPOZORN�N�: P��li� mnoho stop"
#define MSGTR_FoundAudioStream "==> Nalezen audio proud: %d\n"
#define MSGTR_FoundVideoStream "==> Nalezen video proud: %d\n"
#define MSGTR_DetectedTV "Detekov�na TV! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "Nemohu otev��t ogg demuxer.\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: Hled�m audio proud (id: %d).\n"
#define MSGTR_CannotOpenAudioStream "Nemohu otev��t audio proud: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "Nemohu otev��t proud s titulky: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "Nepovedlo se otev��t audio demuxer: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "Nepovedlo se otev��t demuxer pro titulky: %s\n"
#define MSGTR_TVInputNotSeekable "TV vstup neumo��uje posun! (\"Posun\" bude pravd�podobn� pou�it pro zm�nu kan�l� ;)\n"
#define MSGTR_DemuxerInfoAlreadyPresent "Informace o demuxeru %s ji� p��tomna!\n"
#define MSGTR_ClipInfo "Informace o klipu:\n"

#define MSGTR_LeaveTelecineMode "\ndemux_mpg: Detekov�na progresivn� sekvence, opou�t�m m�d 3:2 TELECINE\n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: Detekov�no 3:2 TELECINE, aktivuji inverzn� telecine fx. FPS zm�n�nno na %5.3f!\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "Nemohu otev��t kodek.\n"
#define MSGTR_CantCloseCodec "Nemohu uzav��t kodek.\n"

#define MSGTR_MissingDLLcodec "CHYBA: Nemohu otev��t po�adovan� DirectShow kodek %s.\n"
#define MSGTR_ACMiniterror "Nemohu na��st/inicializovat Win32/ACM AUDIO kodek. (Chyb� DLL soubor?)\n"
#define MSGTR_MissingLAVCcodec "Nemohu naj�t kodek '%s' v libavcodec...\n"

#define MSGTR_MpegNoSequHdr "MPEG: KRITICK� CHYBA: Konec souboru v pr�b�hu vyhled�v�n� hlavi�ky sekvence.\n"
#define MSGTR_CannotReadMpegSequHdr "KRITICK� CHYBA: Nelze p�e��st hlavi�ku sekvence.\n"
#define MSGTR_CannotReadMpegSequHdrEx "KRITICK� CHYBA: Nelze p�e��st roz���en� hlavi�ky sekvence.\n"
#define MSGTR_BadMpegSequHdr "MPEG: �patn� hlavi�ka sekvence.\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: �patn� roz���en� hlavi�ky sekvence.\n"

#define MSGTR_ShMemAllocFail "Nemohu alokovat sd�lenou pam�\n"
#define MSGTR_CantAllocAudioBuf "Nemohu alokovat vyrovn�vac� pam� pro v�stup zvuku\n"

#define MSGTR_UnknownAudio "Nezn�m�/chyb�j�c� audio form�t -> nebude zvuk.\n"

#define MSGTR_UsingExternalPP "[PP] Pou��v�m extern� filtr pro postprocessing, max q = %d.\n"
#define MSGTR_UsingCodecPP "[PP] Pou��v�m integrovan� postprocessing kodeku, max q = %d.\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "Video atribut '%s' nen� podporov�n vybran�m vo & vd.\n"
#define MSGTR_VideoCodecFamilyNotAvailableStr "Po�adovan� rodina video kodeku [%s] (vfm=%s) nen� dostupn�. (Aktivujte ji p�i kompilaci.)\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "Po�adovan� rodina audio kodeku [%s] (afm=%s) nen� dostupn�. (Aktivujte ji p�i kompilaci.)\n"
#define MSGTR_OpeningVideoDecoder "Otev�r�m video dekod�r: [%s] %s\n"
#define MSGTR_OpeningAudioDecoder "Otev�r�m audio dekod�r: [%s] %s\n"
#define MSGTR_UninitVideoStr "uninit video: %s\n"
#define MSGTR_UninitAudioStr "uninit audio: %s\n"
#define MSGTR_VDecoderInitFailed "VDecoder - inicializace selhala :(\n"
#define MSGTR_ADecoderInitFailed "ADecoder - inicializace selhala :(\n"
#define MSGTR_ADecoderPreinitFailed "ADecoder - preinit selhal :(\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: Alokuji %d byt� pro vstupn� vyrovn�vac� pam�\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: Alokuji %d + %d = %d byt� pro v�stupn� vyrovn�vac� pam�\n"

// LIRC:
#define MSGTR_SettingUpLIRC "Zap�n�m podporu lirc...\n"
#define MSGTR_LIRCdisabled "Nebudete moci pou��vat d�lkov� ovlada�.\n"
#define MSGTR_LIRCopenfailed "Nepovedlo se zapnout podporu LIRC.\n"
#define MSGTR_LIRCcfgerr "Nepovedlo se p�e��st konfigura�n� soubor LIRC %s.\n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "Nemohu nal�zt video filtr '%s'\n"
#define MSGTR_CouldNotOpenVideoFilter "Nemohu otev��t video filtr '%s'\n"
#define MSGTR_OpeningVideoFilter "Otev�r�m video filtr: "
#define MSGTR_CannotFindColorspace "Ani p�i vlo�en� 'scale' nemohu nal�zt spole�n� barevn� prostor :(\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDec: Kodek nenastavil sh->disp_w a sh->disp_h, pokou��m se to obej�t.\n"
#define MSGTR_VoConfigRequest "VDec: Po�adovan� konfigurace vo - %d x %d (preferovan� csp: %s)\n"
#define MSGTR_CouldNotFindColorspace "Nemohu nal�zt spole�n� barevn� prostor - zkou��m to znovu s -vop scale...\n"
#define MSGTR_MovieAspectIsSet "Pom�r stran obrazu filmu je %.2f:1 - �k�luji na spr�vn� pom�r.\n"
#define MSGTR_MovieAspectUndefined "Pom�r stran obrazu filmu nen� definov�n - nem�n�m velikost.\n"

// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "O aplikaci"
#define MSGTR_FileSelect "Vybrat soubor..."
#define MSGTR_SubtitleSelect "Vybrat titulky..."
#define MSGTR_OtherSelect "Vybrat..."
#define MSGTR_AudioFileSelect "Vybrat extern� zvukov� kan�l..."
#define MSGTR_PlayList "Soubory pro p�ehr�n�"
#define MSGTR_Equalizer "Ekvaliz�r"
#define MSGTR_SkinBrowser "Prohl�e� t�mat"
#define MSGTR_Network "S�ov� vys�l�n�..."
#define MSGTR_Preferences "Nastaven�" // P�edvolby?
#define MSGTR_FontSelect "Vybrat font..."
#define MSGTR_OSSPreferences "Konfigurace ovlada�e OSS"
#define MSGTR_SDLPreferences "Konfigurace ovlada�e SDL"
#define MSGTR_NoMediaOpened "Nic nen� otev�eno."
#define MSGTR_VCDTrack "VCD stopa %d"
#define MSGTR_NoChapter "��dn� kapitola" //bez kapitoly?
#define MSGTR_Chapter "Kapitola %d"
#define MSGTR_NoFileLoaded "Nen� na�ten ��dn� soubor."

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
#define MSGTR_NEMDB "Bohu�el nen� dostatek pam�ti pro vykreslovac� mezipam�."
#define MSGTR_NEMFMR "Bohu�el nen� dostatek pam�ti pro vytvo�en� menu."
#define MSGTR_IDFGCVD "Bohu�el nebyl nalezen video ovlada� kompatibiln� s GUI."
#define MSGTR_NEEDLAVCFAME "Bohu�el nelze p�ehr�vat jin� soubory ne� MPEG s kartou DXR3/H+ bez p�ek�dov�n�.\nPros�m, zapn�te lavc nebo fame v konfiguraci DXR3/H+."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[t�mata] chyba v konfigura�n�m souboru t�mat na ��dce %d: %s"
#define MSGTR_SKIN_WARNING1 "[t�mata] varov�n� v konfigura�n�m souboru t�mat na ��dce %d: widget nalezen ale p�ed  \"section\" nenalezen ( %s )"
#define MSGTR_SKIN_WARNING2 "[t�mata] varov�n� v konfigura�n�m souboru t�mat na ��dce %d: widget nalezen ale p�ed \"subsection\" nenalezen (%s)"
#define MSGTR_SKIN_WARNING3 "[t�mata] varov�n� v konfigura�n�m souboru t�mat na ��dce %d: widget (%s) nepodporuje tuto subsekci"
#define MSGTR_SKIN_BITMAP_16bit  "bitmapa s hloubkou 16 bit� a m�n� nen� podporov�na ( %s ).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "soubor nenalezen ( %s )\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "chyba �ten� bmp ( %s )\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "chyba �ten� tga ( %s )\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "chyba �ten� png ( %s )\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "form�t RLE packed tga nen� podporov�n ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "nezn�m� typ souboru ( %s )\n"
#define MSGTR_SKIN_BITMAP_ConvertError "chyba konverze z 24 do 32 bit� ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "nezn�m� zpr�va: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "nedostatek pam�ti\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "deklarov�no p��li� mnoho p�sem\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "soubor p�sma nebyl nalezen\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "soubor obrazu p�sma nebyl nalezen\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "neexistuj�c� identifik�tor p�sma ( %s )\n"
#define MSGTR_SKIN_UnknownParameter "nezn�m� parametr ( %s )\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[prohl�e� t�mat] nedostatek pam�ti.\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "T�ma nenalezeno ( %s ).\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "Chyba p�i �ten� konfigura�n�ho souboru t�mat ( %s ).\n"
#define MSGTR_SKIN_LABEL "T�mata:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "O aplikaci MPlayer"
#define MSGTR_MENU_Open "Otev��t..."
#define MSGTR_MENU_PlayFile "P�ehr�t soubor..."
#define MSGTR_MENU_PlayVCD "P�ehr�t VCD..."
#define MSGTR_MENU_PlayDVD "P�ehr�t DVD..."
#define MSGTR_MENU_PlayURL "P�ehr�t z URL..."
#define MSGTR_MENU_LoadSubtitle "Na��st titulky..."
#define MSGTR_MENU_DropSubtitle "Zahodit titulky..."
#define MSGTR_MENU_LoadExternAudioFile "Na��st extern� soubor se zvukem..."
#define MSGTR_MENU_Playing "Ovl�d�n� p�ehr�v�n�"
#define MSGTR_MENU_Play "P�ehr�t"
#define MSGTR_MENU_Pause "Pozastavit"
#define MSGTR_MENU_Stop "Zastavit"
#define MSGTR_MENU_NextStream "Dal�� proud"
#define MSGTR_MENU_PrevStream "P�edchoz� proud"
#define MSGTR_MENU_Size "Velikost"
#define MSGTR_MENU_NormalSize "Norm�ln� velikost"
#define MSGTR_MENU_DoubleSize "Dvojn�sobn� velikost"
#define MSGTR_MENU_FullScreen "Cel� obrazovka"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "P�ehr�t disk..."
#define MSGTR_MENU_ShowDVDMenu "Zobrazit DVD menu"
#define MSGTR_MENU_Titles "Tituly"
#define MSGTR_MENU_Title "Titul %2d"
#define MSGTR_MENU_None "(��dn�)"
#define MSGTR_MENU_Chapters "Kapitoly"
#define MSGTR_MENU_Chapter "Kapitola %2d"
#define MSGTR_MENU_AudioLanguages "Jazyk zvuku"
#define MSGTR_MENU_SubtitleLanguages "Jazyk titulk�"
#define MSGTR_MENU_PlayList "Soubory pro p�ehr�n�"
#define MSGTR_MENU_SkinBrowser "Prohl�e� t�mat"
#define MSGTR_MENU_Preferences "P�edvolby"
#define MSGTR_MENU_Exit "Konec..."
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
#define MSGTR_EQU_Center "St�edov�"
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
#define MSGTR_PREFERENCES_Audio "Zvuk"
#define MSGTR_PREFERENCES_Video "Obraz"
#define MSGTR_PREFERENCES_SubtitleOSD "Titulky & OSD"
#define MSGTR_PREFERENCES_Codecs "Kodeky & demuxer"
#define MSGTR_PREFERENCES_Misc "Ostatn�"

#define MSGTR_PREFERENCES_None "Nic"
#define MSGTR_PREFERENCES_AvailableDrivers "Dostupn� ovlada�e:"
#define MSGTR_PREFERENCES_DoNotPlaySound "Nep�ehr�vat zvuk"
#define MSGTR_PREFERENCES_NormalizeSound "Normalizovat zvuk"
#define MSGTR_PREFERENCES_EnEqualizer "Aktivovat ekvaliz�r"
#define MSGTR_PREFERENCES_ExtraStereo "Aktivovat extra stereo"
#define MSGTR_PREFERENCES_Coefficient "Koeficient:"
#define MSGTR_PREFERENCES_AudioDelay "Zpo�d�n� zvuku"
#define MSGTR_PREFERENCES_DoubleBuffer "Aktivovat double buffering"
#define MSGTR_PREFERENCES_DirectRender "Aktivovat direct rendering"
#define MSGTR_PREFERENCES_FrameDrop "Aktivovat zahazov�n� sn�mk�"
#define MSGTR_PREFERENCES_HFrameDrop "Aktivovat TVRD� zahazov�n� sn�mk� (nebezpe�n�)"
#define MSGTR_PREFERENCES_Flip "Obr�tit obraz vzh�ru nohama"
#define MSGTR_PREFERENCES_Panscan "Panscan:"
#define MSGTR_PREFERENCES_OSDTimer "�as a ostatn� ukazatele"
#define MSGTR_PREFERENCES_OSDProgress "Pouze ukazatel pozice" // progressbar
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "�as, procenta a celkov� �as"
#define MSGTR_PREFERENCES_Subtitle "Titulky:"
#define MSGTR_PREFERENCES_SUB_Delay "Zpo�d�n�: "
#define MSGTR_PREFERENCES_SUB_FPS "FPS:"
#define MSGTR_PREFERENCES_SUB_POS "Pozice: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "Vypnout automatick� na�ten� titulk�"
#define MSGTR_PREFERENCES_SUB_Unicode "Titulky v UNICODE"
#define MSGTR_PREFERENCES_SUB_MPSUB "P�ev�st dan� titulky do vlastn�ho form�tu MPlayeru"
#define MSGTR_PREFERENCES_SUB_SRT "P�ev�st dan� titulky do �asov� orientovan�ho form�tu SubViewer (SRT)"
#define MSGTR_PREFERENCES_SUB_Overlap "Zapnout p�ekr�v�n� titulk�"
#define MSGTR_PREFERENCES_Font "P�smo:"
#define MSGTR_PREFERENCES_FontFactor "�initel p�sma:" //???? asi zv�t�en�? Kde to v�bec je?
#define MSGTR_PREFERENCES_PostProcess "Aktivovat postprocessing"
#define MSGTR_PREFERENCES_AutoQuality "Automatick� ��zen� kvality:"
#define MSGTR_PREFERENCES_NI "Pou��t parser pro neprokl�dan� AVI form�t"
#define MSGTR_PREFERENCES_IDX "P�etvo�it tabulku index�, pokud je to t�eba"
#define MSGTR_PREFERENCES_VideoCodecFamily "Rodina video kodeku:"
#define MSGTR_PREFERENCES_AudioCodecFamily "Rodina audio kodeku:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "Typ OSD"
#define MSGTR_PREFERENCES_FRAME_Subtitle "Titulky"
#define MSGTR_PREFERENCES_FRAME_Font "P�smo"
#define MSGTR_PREFERENCES_FRAME_PostProcess "Postprocessing"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "Kodek & demuxer"
#define MSGTR_PREFERENCES_FRAME_Cache "Vyrovn�vac� pam�"
#define MSGTR_PREFERENCES_FRAME_Misc "Ostatn�"
#define MSGTR_PREFERENCES_OSS_Device "Za��zen�:"
#define MSGTR_PREFERENCES_OSS_Mixer "Sm�ova�:"
#define MSGTR_PREFERENCES_SDL_Driver "Ovlada�:"
#define MSGTR_PREFERENCES_Message "Pozor, n�kter� nastaven� pot�ebuj� pro svou funkci restartovat p�ehr�v�n�!"
#define MSGTR_PREFERENCES_DXR3_VENC "Video enkod�r:"
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
#define MSGTR_PREFERENCES_FontEncoding12 "Kelt�tina (ISO-8859-14)"
#define MSGTR_PREFERENCES_FontEncoding13 "Hebrej�tina (ISO-8859-8)"
#define MSGTR_PREFERENCES_FontEncoding14 "Ru�tina (KOI8-R)"
#define MSGTR_PREFERENCES_FontEncoding15 "Ukrajin�tina, b�loru�tina (KOI8-U/RU)"
#define MSGTR_PREFERENCES_FontEncoding16 "Jednoduch� ��n�tina (CP936)"
#define MSGTR_PREFERENCES_FontEncoding17 "Tradi�n� ��n�tina (BIG5)"
#define MSGTR_PREFERENCES_FontEncoding18 "Japon�tina (SHIFT-JIS)"
#define MSGTR_PREFERENCES_FontEncoding19 "Korej�tina (CP949)"
#define MSGTR_PREFERENCES_FontEncoding20 "Thaj�tina (CP874)"
#define MSGTR_PREFERENCES_FontEncoding21 "Cyrilick� Windows (CP1251)"
#define MSGTR_PREFERENCES_FontEncoding22 "Slovansk�/st�edoevropsk� Windows (CP1250)"
#define MSGTR_PREFERENCES_FontNoAutoScale "Bez automatick� velikosti"
#define MSGTR_PREFERENCES_FontPropWidth "Propor�n� dle ���ky obrazu"
#define MSGTR_PREFERENCES_FontPropHeight "Propor�n� dle v��ky obrazu"
#define MSGTR_PREFERENCES_FontPropDiagonal "Propor�n� dle �hlop���ky"
#define MSGTR_PREFERENCES_FontEncoding "K�dov�n�:"
#define MSGTR_PREFERENCES_FontBlur "Rozmaz�n�:"
#define MSGTR_PREFERENCES_FontOutLine "Obrys:"
#define MSGTR_PREFERENCES_FontTextScale "Velikost textu:"
#define MSGTR_PREFERENCES_FontOSDScale "Velikost OSD:"
#define MSGTR_PREFERENCES_Cache "Zapnout vyrovn�vac� pam�"
#define MSGTR_PREFERENCES_CacheSize "Velikost vyrovn�vac� pam�ti: "
#define MSGTR_PREFERENCES_LoadFullscreen "Spustit p�es celou obrazovku"
#define MSGTR_PREFERENCES_SaveWinPos "Ulo�it pozici okna"
#define MSGTR_PREFERENCES_XSCREENSAVER "Zastavit XScreenSaver"
#define MSGTR_PREFERENCES_PlayBar "Aktivovat playbar"
#define MSGTR_PREFERENCES_AutoSync "Zapnout automatickou synchronizaci"
#define MSGTR_PREFERENCES_AutoSyncValue "Automatick� synchronizace: "
#define MSGTR_PREFERENCES_CDROMDevice "Za��zen� CD-ROM:"
#define MSGTR_PREFERENCES_DVDDevice "Za��zen� DVD:"
#define MSGTR_PREFERENCES_FPS "Sn�mkov� rychlost (FPS):"
#define MSGTR_PREFERENCES_ShowVideoWindow "Zobrazovat video okno p�i ne�innosti"

#define MSGTR_ABOUT_UHU "V�voj GUI je sponzorov�n firmou UHU Linux\n"
#define MSGTR_ABOUT_CoreTeam "   Hlavn� v�voj��i programu MPlayer:\n"
#define MSGTR_ABOUT_AdditionalCoders "   Dal�� v�voj��i:\n"
#define MSGTR_ABOUT_MainTesters "   Hlavn� teste�i:\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "Kritick� chyba!"
#define MSGTR_MSGBOX_LABEL_Error "Chyba!"
#define MSGTR_MSGBOX_LABEL_Warning "Upozorn�n�!"

#endif

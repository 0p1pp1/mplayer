// Translated by:  Jiri Svoboda, jiri.svoboda@seznam.cz
// Updated by:     Tomas Blaha,  tomas.blaha at kapsa.club.cz
// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char* banner_text=
"\n\n"
"MPlayer " VERSION "(C) 2000-2002 Arpad Gereoffy (viz DOCS!)\n"
"\n";

static char help_text[]=
"Pou�it�:   mplayer [p�ep�na�e] [cesta/]jmenosouboru\n"
"\n"
"P�ep�na�e:\n"
" -vo <drv[:dev]> v�b�r v�stupn�ho video ovlada�e&za��zen� (-vo help pro seznam)\n"
" -ao <drv[:dev]> v�b�r v�stupn�ho audio ovlada�e&za��zen� (-ao help pro seznam)\n"
" -vcd <trackno>  p�ehr�t VCD (video cd) stopu ze za��zen� m�sto ze souboru\n"
#ifdef HAVE_LIBCSS
" -dvdauth <dev>  ur�en� DVD za��zen� pro autentizaci (pro k�dovan� disky)\n"
#endif
#ifdef USE_DVDREAD
" -dvd <titleno>  p�ehr�t DVD titul/stopu ze za��zen� (mechaniky) m�sto souboru\n"
#endif
" -ss <timepos>   posun na pozici (sekundy nebo hh:mm:ss)\n"
" -nosound        p�ehr�vat beze zvuku\n"
#ifdef USE_FAKE_MONO
" -stereo <mode>  v�b�r audiokan�lu pro MPEG1 (0:stereo 1:lev� 2:prav�)\n"
#endif
" -channels <n>   c�lov� po�et zvukov�ch v�stupn�ch kan�l�\n"
" -fs -vm -zoom   volby pro p�ehr�v�n� p�es celou obrazovku (cel� obrazovka\n                 m�nit videore�im, softwarov� zoom)\n"
" -x <x> -y <y>   zv�t�en� obrazu na rozm�r <x>*<y> (pokud to um� -vo ovlada�!)\n"
" -sub <file>     volba souboru s titulky (viz tak� -subfps, -subdelay)\n"
" -playlist <file> ur�en� souboru se seznamem p�ehr�van�ch soubor�\n"
" -vid x -aid y   v�b�r ��sla video (x) a audio (y) proudu pro p�ehr�n�\n"
" -fps x -srate y volba pro zm�nu video (x fps) a audio (y Hz) frekvence\n"
" -pp <quality>   aktivace postprocesing filtru (0-4 pro DivX, 0-63 pro mpegy)\n"
" -nobps          pou��t alternativn� A-V synchroniza�n� metodu pro Avi soubory\n"
" -framedrop      povolit zahazov�n� sn�mk� (pro pomale stroje)\n"
" -wid <window id> pou��t existuj�c� okno pro v�stup videa\n"
"\n"
"Kl�vesy:\n"
" <-  nebo  ->    posun vzad/vp�ed o 10 sekund\n"
" nahoru �i dol�  posun vzad/vp�ed o  1 minutu\n"
" < nebo >        posun vzad/vp�ed v seznamu p�ehr�van�ch soubor�\n"
" p nebo mezern�k pauza p�i p�ehr�v�n� (pokra�ov�n� stiskem kter�koliv kl�vesy)\n"
" q nebo ESC      konec p�ehr�v�n� a ukon�en� programu\n"
" + nebo -        upravit zpo�d�n� zvuku v kroc�ch +/- 0.1 sekundy\n"
" o               cyklick� zm�na re�imu OSD:  nic / pozice / pozice+�as\n"
" * nebo /        p�idat nebo ubrat hlasitost (stiskem 'm' v�b�r master/pcm)\n"
" z nebo x        upravit zpo�d�n� titulk� v kroc�ch +/- 0.1 sekundy\n"
"\n"
" * * * * P�E�T�TE SI MAN STR�NKU PRO DETAILY (DAL�� VOLBY A KL�VESY)! * * * *\n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// mplayer.c: 

#define MSGTR_Exiting "\nKon��m... (%s)\n"
#define MSGTR_Exit_quit "Konec"
#define MSGTR_Exit_eof "Konec souboru"
#define MSGTR_Exit_error "Z�va�n� chyba"
#define MSGTR_IntBySignal "\nMPlayer p�eru�en sign�lem %d v modulu: %s \n"
#define MSGTR_NoHomeDir "Nemohu nal�zt dom�c� (HOME) adres��\n"
#define MSGTR_GetpathProblem "get_path(\"config\") probl�m\n"
#define MSGTR_CreatingCfgFile "Vytv���m konfigura�n� soubor: %s\n"
#define MSGTR_InvalidVOdriver "Neplatn� jm�no v�stupn�ho video ovlada�e: %s\nPou�ijte '-vo help' pro seznam dostupn�ch ovlada��.\n"
#define MSGTR_InvalidAOdriver "Neplatn� jm�no v�stupn�ho audio ovlada�e: %s\nPou�ijte '-ao help' pro seznam dostupn�ch ovlada��.\n"
#define MSGTR_CopyCodecsConf "(copy/ln etc/codecs.conf (ze zdrojov�ch k�d� MPlayeru) do ~/.mplayer/codecs.conf)\n"
#define MSGTR_CantLoadFont "Nemohu na��st font: %s\n"
#define MSGTR_CantLoadSub "Nemohu na��st titulky: %s\n"
#define MSGTR_ErrorDVDkey "Chyba p�i zpracov�n� kl��e DVD.\n"
#define MSGTR_CmdlineDVDkey "DVD kl�� po�adovan� na p��kazov� ��dce je uschov�n pro rozk�dov�n�.\n"
#define MSGTR_DVDauthOk "DVD autentiza�n� sekvence vypad� vpo��dku.\n"
#define MSGTR_DumpSelectedStreamMissing "dump: Kritick� chyba: po�adovan� proud chyb�!\n"
#define MSGTR_CantOpenDumpfile "Nelze otev��t soubor pro dump!!!\n"
#define MSGTR_CoreDumped "j�dro vyps�no :)\n"
#define MSGTR_FPSnotspecified "V hlavi�ce souboru nen� ud�no (nebo je �patn�) FPS! Pou�ijte volbu -fps !\n"
#define MSGTR_TryForceAudioFmtStr "Pokou��m se vynutit rodinu audiokodeku %s ...\n"
#define MSGTR_CantFindAfmtFallback "Nemohu nal�zt audio kodek pro po�adovanou rodinu, pou�iji ostatn�.\n"
#define MSGTR_CantFindAudioCodec "Nemohu nal�zt kodek pro audio form�t 0x%X !\n"
#define MSGTR_TryUpgradeCodecsConfOrRTFM "*** Pokuste se upgradovat %s z etc/codecs.conf\n*** Pokud probl�m p�etrv�, pak si p�e�t�te DOCS/CODECS!\n"
#define MSGTR_CouldntInitAudioCodec "Nelze inicializovat audio kodek! -> beze zvuku\n"
#define MSGTR_TryForceVideoFmtStr "Poku��m se vynutit rodinu videokodeku %s ...\n"
#define MSGTR_CantFindVideoCodec "Nemohu nal�zt kodek pro video form�t 0x%X !\n"
#define MSGTR_VOincompCodec "Bohu�el, vybran� video_out za��zen� je nekompatibiln� s t�mto kodekem.\n"
#define MSGTR_CannotInitVO "Kritick� chyba: Nemohu inicializovat video driver!\n"
#define MSGTR_CannotInitAO "nemohu otev��t/inicializovat audio driver -> TICHO\n"
#define MSGTR_StartPlaying "Za��n�m p�ehr�vat...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"         ***********************************************************\n"\
"         ****  V� syst�m je p��li� POMAL� pro toto p�ehr�v�n�! ****\n"\
"         ***********************************************************\n"\
"!!! Mo�n� p���iny, probl�my a �e�en�:\n"\
"- Nej�ast�j��: �patn�/chybn� _zvukov�_ ovlada�. �e�en�: zkuste -ao sdl\n"\
"  nebo pou�ijte ALSA 0.5 �i oss emulaci z ALSA 0.9. V�ce tip� se dozv�te\n"\
"  v souboru DOCS/sound.html!\n"\
"- Pomal� video v�stup. Zkuste jin� -vo ovlada� (pro seznam: -vo help) nebo zkuste\n"\
"  volbu -framedrop ! Tipy pro lad�n�/zrychlen� videa jsou v DOCS/video.html\n"\
"- Pomal� cpu. Nezkou�ejte p�ehr�vat velk� dvd/divx na pomal�m cpu!\n"\
"  Zkuste volbu -hardframedrop\n"\
"- Po�kozen� soubor. Zkuste r�zn� kombinace t�chto voleb:\n"\
"  -nobps  -ni  -mc 0  -forceidx\n"\
"- P�i p�ehr�v�n� z pomal�ch m�di� (nfs/smb, dvd, vcd etc) zkuste -cache 8192\n"\
"- Pou��v�te -cache pro neprokl�dan� AVI soubory? Zkuste -nocache.\n"\
"Pokud nic z toho nen� pravda, p�e�t�te si DOCS/bugreports.html!\n\n"

#define MSGTR_NoGui "MPlayer byl p�elo�en BEZ podpory GUI!\n"
#define MSGTR_GuiNeedsX "MPlayer GUI vy�aduje X11!\n"
#define MSGTR_Playing "P�ehr�v�m %s\n"
#define MSGTR_NoSound "Audio: beze zvuku!!!\n"
#define MSGTR_FPSforced "FPS vynuceno na hodnotu %5.3f  (ftime: %5.3f)\n"
#define MSGTR_CompiledWithRuntimeDetection "P�elo�eno s detekc� CPU ZA B�HU - upozorn�n�, toto nen� optim�ln�!\nAbyste z�skali co nejv�t�� v�kon, p�eolo�te znovu mplayer ze zdrojov�ho k�du\ns �p�ep�na�em --disable-runtime-cpudetection\n"
#define MSGTR_CompiledWithCPUExtensions "P�elo�eno pro architekturu x86 pro CPU s roz���en�m:"
#define MSGTR_AvailableVideoOutputPlugins "Dostupn� z�suvn� video moduly:\n"
#define MSGTR_AvailableVideoOutputDrivers "Dostupn� ovlada�e pro video:\n"
#define MSGTR_AvailableAudioOutputDrivers "Dostupn� ovlada�e pro audio:\n"
#define MSGTR_AvailableAudioCodecs "Dostupn� audio kodeky:\n"
#define MSGTR_AvailableVideoCodecs "Dostupn� video kodeky:\n"
#define MSGTR_AvailableAudioFm "\nDostupn� (p�ikompilovan�) rodiny audio kodek�/ovlada�e:\n"
#define MSGTR_AvailableVideoFm "\nDostupn� (p�ikompilovan�) rodiny video kodek�/ovlada�e:\n"
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
#define MSGTR_PlaylistLoadUnable "\nNemohu na��st seznam soubor� pro p�ehr�n� %s\n"

// mencoder.c:

#define MSGTR_MEncoderCopyright "(C) 2000-2002 Arpad Gereoffy (viz DOCS!)\n"
#define MSGTR_UsingPass3ControllFile "��d�c� soubor pro t�et� pr�b�h (pass3): %s\n"
#define MSGTR_MissingFilename "\nChyb�j�c� jm�no souboru!\n\n"
#define MSGTR_CannotOpenFile_Device "Nelze otev��t soubor/za��zen�\n"
#define MSGTR_ErrorDVDAuth "Chyba p�i autentizaci DVD...\n"
#define MSGTR_CannotOpenDemuxer "Nemohu otev��t demuxer\n"
#define MSGTR_NoAudioEncoderSelected "\nNebyl vybr�n enkoder zvuku (-oac)! Vyberte jeden nebo pou�ijte volbu -nosound. Pro n�pov�du pou�ijte -oac help !\n"
#define MSGTR_NoVideoEncoderSelected "\nNebyl vybr�n enkoder videa (-ovc)! Vyberte jeden. Pro n�pov�du pou�ijte -ovc help !\n"
#define MSGTR_InitializingAudioCodec "Inicializuji audio kodek...\n"
#define MSGTR_CannotOpenOutputFile "Nemohu otev��t v�stupn� soubor '%s'\n"
#define MSGTR_EncoderOpenFailed "Nepovedlo se otev��t enkoder\n"
#define MSGTR_ForcingOutputFourcc "Vynucuji v�stupn� form�t (fourcc) na %x [%.4s]\n"
#define MSGTR_WritingAVIHeader "Zapisuji hlavi�ku AVI...\n"
#define MSGTR_DuplicateFrames "\nduplikovat %d sn�mek/sn�mky!!!    \n"
#define MSGTR_SkipFrame "\np�esko�it sn�mek!!!    \n"
#define MSGTR_ErrorWritingFile "%s: chyba p�i z�pisu souboru.\n"
#define MSGTR_WritingAVIIndex "\nZapisuji AVI index...\n"
#define MSGTR_FixupAVIHeader "Opravuji AVI hlavi�ku...\n"
#define MSGTR_RecommendedVideoBitrate "Doporu�en� datov� tok videa pro %s CD: %d\n"
#define MSGTR_VideoStreamResult "\nVideo proud: %8.3f kbit/s  (%d bps)  velikost: %d bajt�  %5.3f sekund  %d sn�mk�\n"
#define MSGTR_AudioStreamResult "\nAudio proud: %8.3f kbit/s  (%d bps)  velikost: %d bajt�  %5.3f sekund\n"


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
#define MSGTR_PREFERENCES_Subtitle "Titulky:"
#define MSGTR_PREFERENCES_SUB_Delay "Zpo�d�n�: "
#define MSGTR_PREFERENCES_SUB_FPS "FPS:"
#define MSGTR_PREFERENCES_SUB_POS "Pozice: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "Vypnout automatick� na�ten� titulk�"
#define MSGTR_PREFERENCES_SUB_Unicode "Titulky v UNICODE"
#define MSGTR_PREFERENCES_SUB_MPSUB "P�ev�st dan� titulky do vlastn�ho form�tu titulk� MPlayeru"
#define MSGTR_PREFERENCES_SUB_SRT "P�ev�st dan� titulky do �asov� orientovan�ho form�tu SubViewer (SRT)"
#define MSGTR_PREFERENCES_Font "Font:"
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
#define MSGTR_PREFERENCES_Cache "Vyrovn�vac� pam� zap/vyp"
#define MSGTR_PREFERENCES_LoadFullscreen "Startovat p�es celou obrazovku"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "Kritick� chyba ..."
#define MSGTR_MSGBOX_LABEL_Error "Chyba ..."
#define MSGTR_MSGBOX_LABEL_Warning "Upozorn�n� ..."

#endif



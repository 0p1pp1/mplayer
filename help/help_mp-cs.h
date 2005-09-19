// Translated by:  Jiri Svoboda, jiri.svoboda@seznam.cz
// Updated by:     Tomas Blaha,  tomas.blaha at kapsa.club.cz
//                 Jiri Heryan
// Synced to 1.182
// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"Pou�it�:          mplayer [volby] [url|cesta/]jm�no_souboru\n"
"\n"
"Z�kladn� volby: (�pln� seznam najdete v manu�lov� str�nce)\n"
" -vo <roz[:za�]>  vybere v�st. video rozhran� a za��zen� (seznam: -vo help)\n"
" -ao <roz[:za�]>  vybere v�st. audio rozhran� a za��zen� (seznam: -ao help)\n"
#ifdef HAVE_VCD
" vcd://<�_stopy>  p�ehraje (S)VCD (Super Video CD) stopu (z nep�ipojen�ho\n"
"                  za��zen�)\n"
#endif
#ifdef USE_DVDREAD
" dvd://<�_tit>    p�ehraje DVD titul ze za��zen� (mechaniky), m�sto ze souboru\n"
" -alang/-slang    zvol� jazyk zvuku/titulk� na DVD (dvouznakov� k�d zem�)\n"
#endif
" -ss <pozice>     p�evine na zadanou pozici (sekundy nebo hh:mm:ss)\n"
" -nosound         p�ehr�v�n� beze zvuku\n"
" -fs              celoobrazovkov� p�ehr�v�n� (nebo -vm -zoom, viz manu�l)\n"
" -x <x> -y <y>    rozli�en� obrazu (pro pou�it� s -vm nebo -zoom)\n"
" -sub <soubor>    zvol� soubor s titulky (viz tak� -subfps, -subdelay)\n"
" -playlist <soubor> ur�� soubor s playlistem\n"
" -vid x -aid y    vybere video (x) a audio (y) proud pro p�ehr�n�\n"
" -fps x -srate y  zm�nit video (x fps) a audio (y Hz) frekvence\n"
" -pp <kvalita>    aktivovat postprocessing (podrobnosti v manu�lu)\n"
" -framedrop       povolit zahazov�n� sn�mk� (pro pomal� stroje)\n"
"\n"
"Z�kladn� kl�vesy: (�pln� seznam je v manu�lu, viz tak� input.conf)\n"
" <-  nebo  ->     p�ev�jen� vzad/vp�ed o 10 sekund\n"
" nahoru �i dol�   p�ev�jen� vzad/vp�ed o 1 minutu\n"
" pgup �i pgdown   p�ev�jen� vzad/vp�ed o 10 minut\n"
" < nebo >         posun na p�edchoz�/dal�� soubor v playlistu\n"
" p nebo mezern�k  pozastav� p�ehr�v�n� (pokra�uje po stisku jak�koliv kl�vesy)\n"
" q nebo ESC       konec p�ehr�v�n� a ukon�en� programu\n"
" + nebo -         uprav� zpo�d�n� zvuku v kroc�ch +/- 0,1 sekundy\n"
" o                cyklick� zm�na re�imu OSD: nic / pozice / pozice a �as\n"
" * nebo /         p�id� nebo ubere PCM hlasitost\n"
" z nebo x         uprav� zpo�d�n� titulk� v kroc�ch +/- 0,1 sekundy\n"
" r nebo t         uprav� polohu titulk� nahoru/dol�, viz tak� -vf expand\n"
"\n"
" * * * V MAN STR�NCE NAJDETE PODROBNOSTI, DAL�� VOLBY A KL�VESY * * *\n"
"\n";
#endif

#define MSGTR_SamplesWanted "Vzorky tohoto form�tu jsou pot�eba pro zlep�en� podpory. Kontaktujte pros�m\n v�vojov� t�m.\n"

// ========================= MPlayer messages ===========================

// mplayer.c:

#define MSGTR_Exiting "\nKon��m...\n"
#define MSGTR_ExitingHow "\nKon��m... (%s)\n"
#define MSGTR_Exit_quit "Konec"
#define MSGTR_Exit_eof "Konec souboru"
#define MSGTR_Exit_error "Kritick� chyba"
#define MSGTR_IntBySignal "\nMPlayer p�eru�en sign�lem %d v modulu %s.\n"
#define MSGTR_NoHomeDir "Nemohu nal�zt dom�c� adres��.\n"
#define MSGTR_GetpathProblem "Nastal probl�m s get_path(\"config\")\n"
#define MSGTR_CreatingCfgFile "Vytv���m konfigura�n� soubor: %s\n"
#define MSGTR_InvalidAOdriver "Neplatn� jm�no v�stupn�ho audio rozhran�: %s\nSeznam dostupn�ch rozhran� zobraz�te pomoc� '-ao help'.\n"
#define MSGTR_CopyCodecsConf "(Zkop�rujte/nalinkujte etc/codecs.conf ze zdrojov�ch k�d� MPlayeru do ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "Pou��v�m zabudovan� v�choz� codecs.conf.\n"
#define MSGTR_CantLoadFont "Nemohu na��st font: %s\n"
#define MSGTR_CantLoadSub "Nemohu na��st titulky: %s\n"
#define MSGTR_DumpSelectedStreamMissing "dump: Kritick� chyba: Chyb� po�adovan� datov� proud!\n"
#define MSGTR_CantOpenDumpfile "Nelze otev��t soubor pro dump.\n"
#define MSGTR_CoreDumped "J�dro odhozeno ;)\n"
#define MSGTR_FPSnotspecified "�daj o FPS v hlavi�ce souboru je �patn� nebo chyb�, pou�ijte volbu -fps!\n"
#define MSGTR_TryForceAudioFmtStr "Pokou��m se vynutit rodinu audiokodeku %s...\n"
#define MSGTR_CantFindAudioCodec "Nemohu nal�zt kodek pro audio form�t 0x%X!\n"
#define MSGTR_RTFMCodecs "P�e�t�te si DOCS/HTML/en/codecs.html!\n"
#define MSGTR_TryForceVideoFmtStr "Pokou��m se vynutit rodinu videokodeku %s...\n"
#define MSGTR_CantFindVideoCodec "Nemohu nal�zt kodek pro vybran� -vo a video form�t 0x%X.\n"
#define MSGTR_CannotInitVO "Kritick� chyba: Nemohu inicializovat video rozhran�!\n"
#define MSGTR_CannotInitAO "Nepoda�ilo se otev��t/inicializovat audio za��zen� -> nebude zvuk.\n"
#define MSGTR_StartPlaying "Za��n�m p�ehr�vat...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"         ***********************************************************\n"\
"         ****  V� syst�m je p��li� POMAL� pro toto p�ehr�v�n�! ****\n"\
"         ***********************************************************\n\n"\
"Mo�n� p���iny, probl�my a �e�en�:\n"\
"- Nej�ast�j��: �patn�/chybn� _zvukov�_ ovlada�!\n"\
"  - Zkuste -ao sdl nebo pou�ijte OSS emulaci z ALSA.\n"\
"  - Pohrajte si s r�zn�mi hodnotami -autosync, pro za��tek t�eba 30.\n"\
"- Pomal� obrazov� v�stup\n"\
"  - Zkuste jin� -vo ovlada� (seznam: -vo help) nebo zkuste -framedrop!\n"\
"- Pomal� CPU\n"\
"  - Nezkou�ejte p�ehr�t velk� DVD/DivX na pomal� CPU! Zkuste n�kter� lavdopts,\n"\
"    jako -vfm ffmpeg -lavdopts lowres=1:fast:skiploopfilter=all.\n"\
"- Po�kozen� soubor.\n"\
"  - Zkuste r�zn� kombinace voleb -nobps -ni -forceidx -mc 0.\n"\
"- P�ehr�v�te z pomal�ho m�dia (NFS/SMB, DVD, VCD, atd.)\n"\
"  - Zkuste -cache 8192.\n"\
"- Pou��v�te -cache pro neprokl�dan� AVI soubory?\n"\
"  - Zkuste -nocache.\n"\
"Tipy na vylad�n� a zrychlen� najdete v DOCS/HTML/en/devices.html.\n"\
"Pokud nic z toho nepom��e, p�e�t�te si DOCS/HTML/en/bugreports.html.\n\n"

#define MSGTR_NoGui "MPlayer byl p�elo�en BEZ podpory GUI.\n"
#define MSGTR_GuiNeedsX "GUI MPlayeru vy�aduje X11.\n"
#define MSGTR_Playing "P�ehr�v�m %s\n"
#define MSGTR_NoSound "Audio: ��dn� zvuk\n"
#define MSGTR_FPSforced "FPS vynuceno na hodnotu %5.3f  (vyn. �as: %5.3f)\n"
#define MSGTR_CompiledWithRuntimeDetection "P�elo�eno s detekc� CPU za b�hu - VAROV�N� - toto nen� optim�ln�!\nAbyste z�skali co nejv�t�� v�kon, p�elo�te znovu mplayer ze zdrojov�ho k�du\ns volbou --disable-runtime-cpudetection\n"
#define MSGTR_CompiledWithCPUExtensions "P�elo�eno pro CPU x86 s roz���en�mi:"
#define MSGTR_AvailableVideoOutputDrivers "Dostupn� video rozhran�:\n"
#define MSGTR_AvailableAudioOutputDrivers "Dostupn� audio rozhran�:\n"
#define MSGTR_AvailableAudioCodecs "Dostupn� audio kodeky:\n"
#define MSGTR_AvailableVideoCodecs "Dostupn� video kodeky:\n"
#define MSGTR_AvailableAudioFm "Dostupn� (zakompilovan�) rodiny audio kodek�/ovlada��:\n"
#define MSGTR_AvailableVideoFm "Dostupn� (zakompilovan�) rodiny video kodek�/ovlada��:\n"
#define MSGTR_AvailableFsType "Dostupn� re�imy zm�ny hladiny p�i celoobrazovkov�m zobrazen�:\n"
#define MSGTR_UsingRTCTiming "Pro �asov�n� pou�ity linuxov� hardwarov� RTC (%ldHz).\n"
#define MSGTR_CannotReadVideoProperties "Video: Nelze p�e��st vlastnosti.\n"
#define MSGTR_NoStreamFound "Nenalezen ��dn� datov� proud.\n"
#define MSGTR_ErrorInitializingVODevice "Chyba p�i otev�r�n�/inicializaci vybran�ho video_out (-vo) za��zen�.\n"
#define MSGTR_ForcedVideoCodec "Vynucen video kodek: %s\n"
#define MSGTR_ForcedAudioCodec "Vynucen audio kodek: %s\n"
#define MSGTR_Video_NoVideo "Video: ��dn� video\n"
#define MSGTR_NotInitializeVOPorVO "\nKritick� chyba: Nemohu inicializovat video filtry (-vf) nebo video v�stup (-vo)!\n"
#define MSGTR_Paused "\n===== POZASTAVENO =====\r"
#define MSGTR_PlaylistLoadUnable "\nNemohu na��st playlist %s.\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- MPlayer havaroval kv�li 'Illegal Instruction'.\n"\
"  To m��e b�t chyba v k�du pro rozpozn�n� CPU za b�hu...\n"\
"  Pros�m, p�e�t�te si DOCS/HTML/en/bugreports.html.\n"
#define MSGTR_Exit_SIGILL \
"- MPlayer havaroval kv�li 'Illegal Instruction'.\n"\
"  To se obvykle st�v�, kdy� se ho pokus�te spustit na CPU odli�n�m, ne� pro kter�\n"\
"  byl p�elo�en/optimalizov�n.\n  Ov��te si to!\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- MPlayer havaroval kv�li �patn�mu pou�it� CPU/FPU/RAM.\n"\
"  P�elo�te MPlayer s volbou --enable-debug , prove�te 'gdb' backtrace\n"\
"  a disassembly. Detaily najdete v DOCS/HTML/en/bugreports_what.html#bugreports_crash.\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer havaroval. To by se nem�lo st�t.\n"\
"  M��e to b�t chyba v k�du MPlayeru _nebo_ ve va�ich ovlada��ch _nebo_ ve verzi\n"\
"  va�eho gcc. Pokud si mysl�te, �e je to chyba MPlayeru, p�e�t�te si, pros�m,\n"\
"  DOCS/HTML/en/bugreports.html a pokra�ujte podle tam uveden�ho n�vodu. My v�m nem��eme\n"\
"  pomoci, pokud tyto informace neuvedete p�i ohla�ov�n� mo�n� chyby.\n"
#define MSGTR_LoadingConfig "Na��t�m konfiguraci '%s'\n"
#define MSGTR_AddedSubtitleFile "SUB: p�id�n soubor s titulky (%d): %s\n"
#define MSGTR_RemovedSubtitleFile "SUB: odebr�n soubor s titulky (%d): %s\n"
#define MSGTR_ErrorOpeningOutputFile "Chyba p�i otev�r�n� souboru [%s] pro z�pis!\n"
#define MSGTR_CommandLine "P��kazov� ��dek:"
#define MSGTR_RTCDeviceNotOpenable "Selhalo otev�en� %s: %s (by m�lo b�t �iteln� u�ivatelem.)\n"
#define MSGTR_LinuxRTCInitErrorIrqpSet "Chyba inicializace Linuxov�ch RTC v ioctl (rtc_irqp_set %lu): %s\n"
#define MSGTR_IncreaseRTCMaxUserFreq "Zkuste p�idat \"echo %lu > /proc/sys/dev/rtc/max-user-freq\" do startovac�ch\n skript� va�eho syst�mu.\n"
#define MSGTR_LinuxRTCInitErrorPieOn "Chyba inicializace Linuxov�ch RTC v ioctl (rtc_pie_on): %s\n"
#define MSGTR_UsingTimingType "Pou��v�m %s �asov�n�.\n"
#define MSGTR_NoIdleAndGui "Volbu -idle nelze pou��t pro GMPlayer.\n"
#define MSGTR_MenuInitialized "Menu inicializov�no: %s\n"
#define MSGTR_MenuInitFailed "Selhala inicializace menu.\n"
#define MSGTR_Getch2InitializedTwice "VAROV�N�: getch2_init vol�na dvakr�t!\n"
#define MSGTR_DumpstreamFdUnavailable "Nemohu ulo�it (dump) tento proud - ��dn� 'fd' nen� dostupn�.\n"
#define MSGTR_FallingBackOnPlaylist "Ustupuji od pokusu o zpracov�n� playlistu %s...\n"
#define MSGTR_CantOpenLibmenuFilterWithThisRootMenu "Nemohu otev��t video filtr libmenu s ko�enov�m menu %s.\n"
#define MSGTR_AudioFilterChainPreinitError "Chyba p�i p�edinicializaci �et�zce audio filtr�!\n"
#define MSGTR_LinuxRTCReadError "Chyba p�i �ten� z Linuxov�ch RTC: %s\n"
#define MSGTR_SoftsleepUnderflow "Varov�n�! Podte�en� softsleep!\n"
#define MSGTR_DvdnavNullEvent "Nedefinovan� DVDNAV ud�lost?!\n"
#define MSGTR_DvdnavHighlightEventBroken "DVDNAV ud�lost: Vadn� zv�raz�ov�n� ud�lost�\n"
#define MSGTR_DvdnavEvent "DVDNAV ud�lost: %s\n"
#define MSGTR_DvdnavHighlightHide "DVDNAV ud�lost: Highlight Hide\n"
#define MSGTR_DvdnavStillFrame "######################################## DVDNAV ud�lost: Stoj�c� sn�mek: %d sek.\n"
#define MSGTR_DvdnavNavStop "DVDNAV ud�lost: Nav Stop\n"
#define MSGTR_DvdnavNavNOP "DVDNAV ud�lost: Nav NOP\n"
#define MSGTR_DvdnavNavSpuStreamChangeVerbose "DVDNAV ud�lost: Nav Zm�na SPU proudu: fyz: %d/%d/%d logick�: %d\n"
#define MSGTR_DvdnavNavSpuStreamChange "DVDNAV ud�lost: Nav Zm�na SPU proudu: fyz: %d logick�: %d\n"
#define MSGTR_DvdnavNavAudioStreamChange "DVDNAV ud�lost: Nav Zm�na audio proudu: fyz: %d logick�: %d\n"
#define MSGTR_DvdnavNavVTSChange "DVDNAV ud�lost: Nav Zm�na VTS\n"
#define MSGTR_DvdnavNavCellChange "DVDNAV ud�lost: Nav Cell Change\n"
#define MSGTR_DvdnavNavSpuClutChange "DVDNAV ud�lost: Nav Zm�na SPU CLUT\n"
#define MSGTR_DvdnavNavSeekDone "DVDNAV ud�lost: Nav P�ev�jen� Dokon�eno\n"
#define MSGTR_MenuCall "Vol�n� menu\n"

#define MSGTR_EdlCantUseBothModes "Nelze pou��t -edl a -edlout z�rove�.\n"
#define MSGTR_EdlOutOfMem "Nelze alokovat dostatek pam�ti pro vlo�en� EDL dat.\n"
#define MSGTR_EdlRecordsNo "Na��t�m %d EDL akc�.\n"
#define MSGTR_EdlQueueEmpty "Ve�ker� EDL akce ji� byly provedeny.\n"
#define MSGTR_EdlCantOpenForWrite "Nelze otev��t EDL soubor [%s] pro z�pis.\n"
#define MSGTR_EdlCantOpenForRead "Nelze otev��t EDL soubor [%s] pro �ten�.\n"
#define MSGTR_EdlNOsh_video "EDL nelze pou��t bez videa, vyp�n�m.\n"
#define MSGTR_EdlNOValidLine "Chybn� EDL na ��dku: %s\n"
#define MSGTR_EdlBadlyFormattedLine "�patn� form�tovan� EDL na ��dku [%d] Zahazuji.\n"
#define MSGTR_EdlBadLineOverlap "Posledn� stop zna�ka byla [%f]; dal�� start je "\
"[%f]. Vstupy mus� b�t v chronologick�m po�ad� a nesm� se p�ekr�vat. Zahazuji.\n"
#define MSGTR_EdlBadLineBadStop "�asov� zna�ka stop m� b�t za zna�kou start.\n"

// mencoder.c:

#define MSGTR_UsingPass3ControllFile "��dic� soubor pro t��pr�chodov� re�im: %s\n"
#define MSGTR_MissingFilename "\nChyb� jm�no souboru.\n\n"
#define MSGTR_CannotOpenFile_Device "Nelze otev��t soubor/za��zen�.\n"
#define MSGTR_CannotOpenDemuxer "Nelze otev��t demuxer.\n"
#define MSGTR_NoAudioEncoderSelected "\nNebyl vybr�n audio enkod�r (-oac). N�jak� vyberte (viz -oac help) nebo pou�ijte -nosound.\n"
#define MSGTR_NoVideoEncoderSelected "\nNebyl vybr�n video enkod�r (-ovc). N�jak� vyberte (viz  -ovc help).\n"
#define MSGTR_CannotOpenOutputFile "Nelze otev��t v�stupn� soubor '%s'\n"
#define MSGTR_EncoderOpenFailed "Selhalo spu�t�n� enkod�ru\n"
#define MSGTR_ForcingOutputFourcc "Vynucuji v�stupn� fourcc na %x [%.4s]\n"
#define MSGTR_ForcingOutputAudiofmtTag "Vynucuji zna�ku v�stupn�ho zvukov�ho form�tu 0x%x\n"
#define MSGTR_WritingAVIHeader "Zapisuji AVI hlavi�ku...\n"
#define MSGTR_DuplicateFrames "\n%d opakuj�c�ch se sn�mk�!\n"
#define MSGTR_SkipFrame "\nP�eskakuji sn�mek!\n"
#define MSGTR_ResolutionDoesntMatch "\nNov� video soubor m� jin� rozli�en� nebo barevn� prostor ne� jeho p�edch�dce.\n"
#define MSGTR_FrameCopyFileMismatch "\nV�echny video soubory mus� m�t shodn� fps, rozli�en� a kodek pro -ovc copy.\n"
#define MSGTR_AudioCopyFileMismatch "\nV�echny soubory mus� pou��vat identick� audio kodek a form�t pro -oac copy.\n"
#define MSGTR_NoSpeedWithFrameCopy "VAROV�N�: volba -speed nem� zaru�enou spr�vnou funk�nost spolu s -oac copy!\n"\
"V�sledn� film m��e b�t vadn�!\n"
#define MSGTR_ErrorWritingFile "%s: chyba p�i z�pisu souboru.\n"
#define MSGTR_WritingAVIIndex "\nZapisuji AVI index...\n"
#define MSGTR_FixupAVIHeader "Opravuji AVI hlavi�ku...\n"
#define MSGTR_RecommendedVideoBitrate "Doporu�en� datov� tok videa pro CD %s: %d\n"
#define MSGTR_VideoStreamResult "\nVideo proud: %8.3f kbit/s  (%d B/s)  velikost: %d bajt�  %5.3f sekund  %d sn�mk�\n"
#define MSGTR_AudioStreamResult "\nAudio proud: %8.3f kbit/s  (%d B/s)  velikost: %d bajt�  %5.3f sekund\n"
#define MSGTR_OpenedStream "�sp�ch: form�t: %d  data: 0x%X - 0x%x\n"
#define MSGTR_VCodecFramecopy "videokodek: framecopy (%dx%d %dbpp fourcc=%x)\n"
#define MSGTR_ACodecFramecopy "audiokodek: framecopy (form�t=%x kan�l�=%d frekvence=%ld bit�=%d B/s=%ld vzorek-%ld)\n"
#define MSGTR_CBRPCMAudioSelected "vybr�n CBR PCM zvuk\n"
#define MSGTR_MP3AudioSelected "vybr�n MP3 zvuk\n"
#define MSGTR_CannotAllocateBytes "Nelze alokovat %d bajt�\n"
#define MSGTR_SettingAudioDelay "Nastavuji ZPO�D�N� ZVUKU na %5.3f\n"
#define MSGTR_SettingAudioInputGain "Nastavuji p�edzes�len� zvukov�ho vstupu na %f\n"
#define MSGTR_LamePresetEquals "\npreset=%s\n\n"
#define MSGTR_LimitingAudioPreload "Omezuji p�edna��t�n� zvuku na 0.4s\n"
#define MSGTR_IncreasingAudioDensity "Zvy�uji hustotu audia na 4\n"
#define MSGTR_ZeroingAudioPreloadAndMaxPtsCorrection "Vynucuji p�edna��t�n� zvuku na 0, max korekci pts  na 0\n"
#define MSGTR_CBRAudioByterate "\n\nCBR zvuk: %ld bajt�/s, %d bajt�/blok\n"
#define MSGTR_LameVersion "LAME ve verzi %s (%s)\n\n"
#define MSGTR_InvalidBitrateForLamePreset "Chyba: Specifikovan� datov� tok je mimo rozsah pro tento preset re�im.\n"\
"\n"\
"Pokud pou��v�te tento re�im, mus�te zadat hodnotu od \"8\" do \"320\".\n"\
"\n"\
"Dal�� informace viz: \"-lameopts preset=help\"\n"
#define MSGTR_InvalidLamePresetOptions "Chyba: Nezadali jste platn� profil a/nebo volby s preset re�imem.\n"\
"\n"\
"Dostupn� profily jsou:\n"\
"\n"\
"   <fast>        standard\n"\
"   <fast>        extreme\n"\
"                 insane\n"\
"   <cbr> (ABR Mode) - Implikuje re�im ABR. Pro jeho pou�it�,\n"\
"                      jednodu�e zadejte datov� tok. Nap��klad:\n"\
"                      \"preset=185\" aktivuje tento re�im\n"\
"                      a pou�ije pr�m�rn� datov� tok 185 kbps.\n"\
"\n"\
"    N�kolik p��klad�:\n"\
"\n"\
"    \"-lameopts fast:preset=standard  \"\n"\
" or \"-lameopts  cbr:preset=192       \"\n"\
" or \"-lameopts      preset=172       \"\n"\
" or \"-lameopts      preset=extreme   \"\n"\
"\n"\
"Dal�� informace viz: \"-lameopts preset=help\"\n"
#define MSGTR_LamePresetsLongInfo "\n"\
"Preset re�imy jsou navr�eny tak, aby poskytovaly co nejvy��� mo�nou kvalitu.\n"\
"\n"\
"V�t�ina z nich byla testov�na a vylad�na pomoc� zevrubn�ch zdvojen�ch slep�ch\n"\
"poslechov�ch test�, za ��elem dosa�en� a ov��en� t�to kvality.\n"\
"\n"\
"Nastaven� jsou neust�le aktualizov�na v souladu s nejnov�j��m v�vojem\n"\
"a m�la by poskytovat prakticky nejvy��� mo�nou kvalitu, jak� je v sou�asnosti \n"\
"s kodekem LAME dosa�iteln�.\n"\
"\n"\
"Aktivace preset re�im�:\n"\
"\n"\
"   Pro re�imy VBR (v�eobecn� nejvy��� kvalita):\n"\
"\n"\
"     \"preset=standard\" Tento re�im by m�l b�t jasnou volbou\n"\
"                             pro v�t�inu lid� a hudebn�ch ��nr� a m�\n"\
"                             ji� vysokou kvalitu.\n"\
"\n"\
"     \"preset=extreme\" Pokud m�te v�jime�n� dobr� sluch a odpov�daj�c�\n"\
"                             vybaven�, tento re�im obecn� poskytuje\n"\
"                             m�rn� vy��� kvalitu ne� re�im \"standard\".\n"\
"\n"\
"   Pro CBR 320kbps (nejvy��� mo�n� kvalita ze v�ech preset re�im�):\n"\
"\n"\
"     \"preset=insane\"  Tento re�im je pro v�t�inu lid� a situac�\n"\
"                             p�edimenzovan�, ale pokud vy�adujete\n"\
"                             absolutn� nejvy��� kvalitu bez ohledu na\n"\
"                             velikost souboru, je toto va�e volba.\n"\
"\n"\
"   Pro re�imy ABR (vysok� kvalita p�i dan�m datov�m toku, ale ne jako VBR):\n"\
"\n"\
"     \"preset=<kbps>\"  Pou�it�m tohoto re�imu obvykle dos�hnete dobr�\n"\
"                             kvality p�i dan�m datov�m toku. V z�vislosti\n"\
"                             na zadan�m toku tento preset odvod� optim�ln�\n"\
"                             nastaven� pro danou situaci.\n"\
"                             A�koli tento p��stup funguje, nen� ani zdaleka\n"\
"                             tak flexibiln� jako VBR, a obvykle nedosahuje\n"\
"                             stejn� �rovn� kvality jako VBR na vy���ch dato-\n"\
"                             v�ch toc�ch.\n"\
"\n"\
"Pro odpov�daj�c� profily jsou tak� dostupn� n�sleduj�c� volby:\n"\
"\n"\
"   <fast>        standard\n"\
"   <fast>        extreme\n"\
"                 insane\n"\
"   <cbr> (ABR re�im) - Implikuje re�im ABR. Pro jeho pou�it�,\n"\
"                      jednodu�e zadejte datov� tok. Nap��klad:\n"\
"                      \"preset=185\" aktivuje tento re�im\n"\
"                      a pou�ije pr�m�rn� datov� tok 185 kbps.\n"\
"\n"\
"   \"fast\" - V dan�m profilu aktivuje novou rychlou VBR kompresi.\n"\
"            Nev�hodou je obvykle m�rn� vy��� datov� tok ne� v norm�ln�m\n"\
"            re�imu a tak� m��e doj�t k m�rn�mu poklesu kvality.\n"\
"   Varov�n�:v sou�asn� verzi m��e nastaven� \"fast\" v�st k p��li�\n"\
"            vysok�mu datov�mu toku ve srovn�n� s norm�ln�m nastaven�m.\n"\
"\n"\
"   \"cbr\"  - Pokud pou�ijete re�im ABR (viz v��e) s v�znamn�m\n"\
"            datov�m tokem, nap�. 80, 96, 112, 128, 160, 192, 224, 256, 320,\n"\
"            m��ete pou��t volbu \"cbr\" k vynucen� k�dov�n� v re�imu CBR\n"\
"            (konstantn� tok) nam�sto standardn�ho ABR re�imu. ABR poskytuje\n"\
"            lep�� kvalitu, ale CBR m��e b�t u�ite�n� v situac�ch jako je\n"\
"            vys�l�n� mp3 proudu po internetu.\n"\
"\n"\
"    Nap��klad:\n"\
"\n"\
"      \"-lameopts fast:preset=standard  \"\n"\
" nebo \"-lameopts  cbr:preset=192       \"\n"\
" nebo \"-lameopts      preset=172       \"\n"\
" nebo \"-lameopts      preset=extreme   \"\n"\
"\n"\
"\n"\
"Pro ABR re�im je k dispozici n�kolik zkratek:\n"\
"phone => 16kbps/mono        phon+/lw/mw-eu/sw => 24kbps/mono\n"\
"mw-us => 40kbps/mono        voice => 56kbps/mono\n"\
"fm/radio/tape => 112kbps    hifi => 160kbps\n"\
"cd => 192kbps               studio => 256kbps"
#define MSGTR_LameCantInit "Nelze nastavit volby pro LAME, ov��te datov�_tok/vzorkovou_rychlost,"\
"n�kter� velmi n�zk� datov� toky (<32) vy�aduj� ni��� vzorkovou rychlost (nap�. -srate 8000)."\
"Pokud v�e sel�e, zkuste p�ednastaven�."
#define MSGTR_ConfigfileError "chyba konfigura�n�ho souboru"
#define MSGTR_ErrorParsingCommandLine "chyba p�i zpracov�v�n� p��kazov�ho ��dku"
#define MSGTR_VideoStreamRequired "Videoproud je povinn�!\n"
#define MSGTR_ForcingInputFPS "m�sto toho bude vstupn� fps interpretov�no jako %5.2f\n"
#define MSGTR_RawvideoDoesNotSupportAudio "V�stupn� form�t souboru RAWVIDEO nepodporuje zvuk - vyp�n�m ho\n"
#define MSGTR_DemuxerDoesntSupportNosound "Tento demuxer zat�m nepodporuje -nosound.\n"
#define MSGTR_MemAllocFailed "alokace pam�ti selhala"
#define MSGTR_NoMatchingFilter "Nemohu naj�t odpov�daj�c� filtr/ao form�t!\n"
#define MSGTR_MP3WaveFormatSizeNot30 "sizeof(MPEGLAYER3WAVEFORMAT)==%d!=30, mo�n� je vadn� p�eklada� C?\n"
#define MSGTR_NoLavcAudioCodecName "Audio LAVC, chyb� jm�no kodeku!\n"
#define MSGTR_LavcAudioCodecNotFound "Audio LAVC, nemohu naj�t enkod�r pro kodek %s\n"
#define MSGTR_CouldntAllocateLavcContext "Audio LAVC, nemohu alokovat kontext!\n"
#define MSGTR_CouldntOpenCodec "Nelze otev��t kodek %s, br=%d\n"
#define MSGTR_CantCopyAudioFormat "Audio form�t 0x%x je nekompatibiln� s '-oac copy', zkuste pros�m '-oac pcm',\n nebo pou�ijte '-fafmttag' pro jeho p�eps�n�.\n"

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
"               Vynut� tak� metodu CBR pro n�sledn� ABR preset re�imy\n"\
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
" mode=<0-3>    (v�choz�: auto)\n"\
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
" fast          Zap�n� rychlej�� enk�dov�n� pro n�sledn� VBR preset re�imy,\n"\
"               poskytuje o n�co ni��� kvalitu a vy��� datov� tok.\n"\
"\n"\
" preset=<hodnota> P�ednastaven� profily poskytuj�c� maxim�ln� kvalitu.\n"\
"                  medium: enk�dov�n� metodou VBR, dobr� kvalita\n"\
"                   (datov� tok 150-180 kbps)\n"\
"                  standard: enk�dov�n� metodou VBR, vysok� kvalita\n"\
"                   (datov� tok 170-210 kbps)\n"\
"                  extreme: enk�dov�n� metodou VBR, velmi vysok� kvalita\n"\
"                   (datov� tok 200-240 kbps)\n"\
"                  insane: enk�dov�n� metodou CBR, nejvy��� preset kvalita\n"\
"                   (datov� tok 320 kbps)\n"\
"                  <8-320>: hodnota pr�m�rn�ho datov�ho toku pro metodu ABR.\n\n"

//codec-cfg.c:
#define MSGTR_DuplicateFourcc "zdvojen� FourCC"
#define MSGTR_TooManyFourccs "p��li� mnoho FourCC/form�t�..."
#define MSGTR_ParseError "chyba interpretace (parse)"
#define MSGTR_ParseErrorFIDNotNumber "chyba interpretace (ID form�tu, nikoli ��slo?)"
#define MSGTR_ParseErrorFIDAliasNotNumber "chyba interpretace (alias ID form�tu, nikoli ��slo?)"
#define MSGTR_DuplicateFID "zdvojen� ID form�tu"
#define MSGTR_TooManyOut "p��li� mnoho v�stupu..."
#define MSGTR_InvalidCodecName "\njm�no kodeku(%s) nen� platn�!\n"
#define MSGTR_CodecLacksFourcc "\nkodek(%s) nem� FourCC/form�t!\n"
#define MSGTR_CodecLacksDriver "\nkodek(%s) nem� driver!\n"
#define MSGTR_CodecNeedsDLL "\nkodek(%s) vy�aduje 'dll'!\n"
#define MSGTR_CodecNeedsOutfmt "\nkodek(%s) vy�aduje 'outfmt'!\n"
#define MSGTR_CantAllocateComment "Nelze alokovat pam� pro koment��. "
#define MSGTR_GetTokenMaxNotLessThanMAX_NR_TOKEN "get_token(): max >= MAX_MR_TOKEN!"
#define MSGTR_ReadingFile "Na��t�m %s: "
#define MSGTR_CantOpenFileError "Nelze otev��t '%s': %s\n"
#define MSGTR_CantGetMemoryForLine "Nem�m pam� pro 'line': %s\n"
#define MSGTR_CantReallocCodecsp "Nelze realokovat '*codecsp': %s\n"
#define MSGTR_CodecNameNotUnique "Jm�no kodeku '%s' nen� jedine�n�."
#define MSGTR_CantStrdupName "Nelze prov�st strdup -> 'name': %s\n"
#define MSGTR_CantStrdupInfo "Nelze prov�st strdup -> 'info': %s\n"
#define MSGTR_CantStrdupDriver "Nelze prov�st strdup -> 'driver': %s\n"
#define MSGTR_CantStrdupDLL "Nelze prov�st strdup -> 'dll': %s"
#define MSGTR_AudioVideoCodecTotals "%d audio & %d video kodek�\n"
#define MSGTR_CodecDefinitionIncorrect "Kodek nen� spr�vn� definov�n."
#define MSGTR_OutdatedCodecsConf "Tento codecs.conf je p��li� star� a nekompatibiln� s t�mto sestaven�m  MPlayeru!"

// divx4_vbr.c:
#define MSGTR_OutOfMemory "nedostatek pam�ti"
#define MSGTR_OverridingTooLowBitrate "Nastaven� datov� tok je p��li� mal� pro tento klip.\n"\
"Minim�ln� mo�n� datov� tok pro tento klip  je %.0f kbps. P�episuji\n"\
"u�ivatelem nastavenou hodnotu.\n"

// fifo.c
#define MSGTR_CannotMakePipe "Nelze vytvo�it ROURU!\n"

// m_config.c
#define MSGTR_SaveSlotTooOld "Nalezen p��li� star� save slot z lvl %d: %d !!!\n"
#define MSGTR_InvalidCfgfileOption "Volbu %s nelze pou��t v konfigura�n�m souboru\n"
#define MSGTR_InvalidCmdlineOption "Volbu %s nelze pou��t z p��kazov�ho ��dku\n"
#define MSGTR_InvalidSuboption "Chyba: volba '%s' nem� ��dnou podvolbu '%s'\n"
#define MSGTR_MissingSuboptionParameter "Chyba: podvolba '%s' volby '%s' mus� m�t parametr!\n"
#define MSGTR_MissingOptionParameter "Chyba: volba '%s' mus� m�t parametr!\n"
#define MSGTR_OptionListHeader "\n N�zev                Typ             Min        Max      Glob�l  CL    Konfig\n\n"
#define MSGTR_TotalOptions "\nCelkem: %d voleb\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "CD-ROM za��zen� '%s' nebylo nalezeno.\n"
#define MSGTR_ErrTrackSelect "Chyba p�i v�b�ru VCD stopy."
#define MSGTR_ReadSTDIN "�tu ze std. vstupu...\n"
#define MSGTR_UnableOpenURL "Nelze otev��t URL: %s\n"
#define MSGTR_ConnToServer "P�ipojeno k serveru: %s\n"
#define MSGTR_FileNotFound "Soubor nenalezen: '%s'\n"

#define MSGTR_SMBInitError "Nelze inicializovat knihovnu libsmbclient: %d\n"
#define MSGTR_SMBFileNotFound "Nemohu otev��t soubor ze s�t�: '%s'\n"
#define MSGTR_SMBNotCompiled "MPlayer nebyl p�elo�en s podporou �ten� SMB.\n"

#define MSGTR_CantOpenDVD "Nelze otev��t DVD za��zen�: %s\n"
#define MSGTR_DVDwait "Na��t�m strukturu disku, �ekejte pros�m...\n"
#define MSGTR_DVDnumTitles "Na tomto DVD je %d titul(�).\n"
#define MSGTR_DVDinvalidTitle "Neplatn� ��slo DVD titulu: %d\n"
#define MSGTR_DVDnumChapters "V tomto DVD titulu je %d kapitol.\n"
#define MSGTR_DVDinvalidChapter "Neplatn� ��slo DVD kapitoly: %d\n"
#define MSGTR_DVDnumAngles "Tento DVD titul m� %d �hl� pohledu.\n"
#define MSGTR_DVDinvalidAngle "Neplatn� ��slo DVD �hlu pohledu: %d\n"
#define MSGTR_DVDnoIFO "Nelze otev��t IFO soubor pro DVD titul %d.\n"
#define MSGTR_DVDnoVOBs "Nelze otev��t VOBy titulu (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD �sp�n� otev�eno.\n"

// muxer_*.c:
#define MSGTR_TooManyStreams "P��li� mnoho datov�ch proud�!"
#define MSGTR_RawMuxerOnlyOneStream "Muxer surov�ho zvuku podporuje pouze jeden zvukov� proud!\n"
#define MSGTR_IgnoringVideoStream "Ignoruji video proud!\n"
#define MSGTR_UnknownStreamType "Varov�n�! Nezn�m� typ datov�ho proudu: %d\n"
#define MSGTR_WarningLenIsntDivisible "Varov�n�! D�lka nen� n�sobkem velikosti vzorku!\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "VAROV�N�: Hlavi�ka audio proudu %d p�edefinov�na!\n"
#define MSGTR_VideoStreamRedefined "VAROV�N�: Hlavi�ka video proudu %d p�edefinov�na!\n"
#define MSGTR_TooManyAudioInBuffer "\nP��li� mnoho audio paket� ve vyrovn�vac� pam�ti: (%d v %d bajtech)\n"
#define MSGTR_TooManyVideoInBuffer "\nP��li� mnoho video paket� ve vyrovn�vac� pam�ti: (%d v %d bajtech)\n"
#define MSGTR_MaybeNI "Mo�n� p�ehr�v�te neprokl�dan� proud/soubor nebo kodek selhal?\n"\
		      "V AVI souborech zkuste vynutit neprokl�dan� re�im pomoc� volby -ni.\n"
#define MSGTR_SwitchToNi "\nDetekov�n �patn� prokl�dan� AVI soubor - p�ep�n�m do -ni re�imu...\n"
#define MSGTR_Detected_XXX_FileFormat "Detekov�n form�t souboru %s.\n"
#define MSGTR_DetectedAudiofile "Detekov�n zvukov� soubor.\n"
#define MSGTR_NotSystemStream "Toto nen� form�t MPEG System Stream... (mo�n� Transport Stream?)\n"
#define MSGTR_InvalidMPEGES "�patn� MPEG-ES proud??? Kontaktujte autora, mo�n� to je chyba :(\n"
#define MSGTR_FormatNotRecognized "======= Bohu�el, form�t tohoto souboru nebyl rozpozn�n/nen� podporov�n =======\n"\
                                  "==== Pokud je soubor AVI, ASF nebo MPEG proud, kontaktujte pros�m autora! ====\n"
#define MSGTR_MissingVideoStream "Nebyl nalezen video proud.\n"
#define MSGTR_MissingAudioStream "Nebyl nalezen audio proud -> bez zvuku.\n"
#define MSGTR_MissingVideoStreamBug "Chyb� video proud!? Kontaktujte autora, m��e to b�t chyba :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: Soubor neobsahuje zvolen� audio nebo video proud.\n"

#define MSGTR_NI_Forced "Vynucen"
#define MSGTR_NI_Detected "Detekov�n"
#define MSGTR_NI_Message "%s NEPROKL�DAN� form�t AVI souboru.\n"

#define MSGTR_UsingNINI "Pou��v�m NEPROKL�DAN� vadn� form�tov�n� AVI souboru.\n"
#define MSGTR_CouldntDetFNo "Nelze ur�it po�et sn�mk� (pro absolutn� posun)\n"
#define MSGTR_CantSeekRawAVI "Nelze se posouvat v surov�ch (raw) AVI proudech! (Pot�ebuji index, zkuste pou��t volbu -idx.)\n"
#define MSGTR_CantSeekFile "Nemohu se posouvat v tomto souboru.\n"

#define MSGTR_EncryptedVOB "�ifrovan� soubor VOB! P�e�t�te si DOCS/HTML/en/dvd.html.\n"

#define MSGTR_MOVcomprhdr "MOV: Komprimovan� hlavi�ky vy�aduj� ZLIB!\n"
#define MSGTR_MOVvariableFourCC "MOV: VAROV�N�: Prom�nn� FOURCC detekov�na!?\n"
#define MSGTR_MOVtooManyTrk "MOV: VAROV�N�: p��li� mnoho stop"
#define MSGTR_FoundAudioStream "==> Nalezen audio proud: %d\n"
#define MSGTR_FoundVideoStream "==> Nalezen video proud: %d\n"
#define MSGTR_DetectedTV "Detekov�na TV! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "Nelze otev��t ogg demuxer.\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: Hled�m audio proud (id: %d).\n"
#define MSGTR_CannotOpenAudioStream "Nemohu otev��t audio proud: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "Nemohu otev��t proud s titulky: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "Nepovedlo se otev��t audio demuxer: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "Nepovedlo se otev��t demuxer pro titulky: %s\n"
#define MSGTR_TVInputNotSeekable "TV vstup neumo��uje posun! (\"Posun\" bude pravd�podobn� pou�it pro zm�nu kan�l� ;)\n"
#define MSGTR_DemuxerInfoAlreadyPresent "Informace o demuxeru %s je ji� p��tomna!\n"
#define MSGTR_ClipInfo "Informace o klipu:\n"

#define MSGTR_LeaveTelecineMode "\ndemux_mpg: detekov�no 30000/1001 fps NTSC, p�ep�n�m frekvenci sn�mk�.\n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: detekov�no 24000/1001 fps progresivn� NTSC, p�ep�n�m frekvenci sn�mk�.\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "Nelze otev��t kodek.\n"
#define MSGTR_CantCloseCodec "Nelze uzav��t kodek.\n"

#define MSGTR_MissingDLLcodec "CHYBA: Nelze otev��t po�adovan� DirectShow kodek %s.\n"
#define MSGTR_ACMiniterror "Nemohu na��st/inicializovat Win32/ACM AUDIO kodek (chyb� DLL soubor?).\n"
#define MSGTR_MissingLAVCcodec "Nemohu naj�t kodek '%s' v libavcodec...\n"

#define MSGTR_MpegNoSequHdr "MPEG: KRITICK� CHYBA: Konec souboru v pr�b�hu vyhled�v�n� hlavi�ky sekvence.\n"
#define MSGTR_CannotReadMpegSequHdr "KRITICK� CHYBA: Nelze p�e��st hlavi�ku sekvence.\n"
#define MSGTR_CannotReadMpegSequHdrEx "KRITICK� CHYBA: Nelze p�e��st roz���en� hlavi�ky sekvence.\n"
#define MSGTR_BadMpegSequHdr "MPEG: �patn� hlavi�ka sekvence.\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: �patn� roz���en� hlavi�ky sekvence.\n"

#define MSGTR_ShMemAllocFail "Nelze alokovat sd�lenou pam�\n"
#define MSGTR_CantAllocAudioBuf "Nelze alokovat vyrovn�vac� pam� pro zvukov� v�stup\n"

#define MSGTR_UnknownAudio "Nezn�m�/chyb�j�c� audio form�t -> nebude zvuk.\n"

#define MSGTR_UsingExternalPP "[PP] Pou��v�m extern� filtr pro postprocessing, max q = %d.\n"
#define MSGTR_UsingCodecPP "[PP] Pou��v�m integrovan� postprocessing kodeku, max q = %d.\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "Video atribut '%s' nen� podporov�n vybran�m vo & vd.\n"
#define MSGTR_VideoCodecFamilyNotAvailableStr "Po�adovan� rodina video kodeku [%s] (vfm=%s) nen� dostupn�.\nAktivujte ji p�i kompilaci.\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "Po�adovan� rodina audio kodeku [%s] (afm=%s) nen� dostupn�.\nAktivujte ji p�i kompilaci.\n"
#define MSGTR_OpeningVideoDecoder "Otev�r�m video dekod�r: [%s] %s\n"
#define MSGTR_OpeningAudioDecoder "Otev�r�m audio dekod�r: [%s] %s\n"
#define MSGTR_UninitVideoStr "uninit video: %s\n"
#define MSGTR_UninitAudioStr "uninit audio: %s\n"
#define MSGTR_VDecoderInitFailed "Video dekod�r - inicializace selhala :(\n"
#define MSGTR_ADecoderInitFailed "Audio dekod�r - inicializace selhala :(\n"
#define MSGTR_ADecoderPreinitFailed "Audio dekod�r - p�edinicializace selhala :(\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: Alokuji %d byt� pro vstupn� vyrovn�vac� pam�\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: Alokuji %d + %d = %d byt� pro v�stupn� vyrovn�vac� pam�\n"

// LIRC:
#define MSGTR_SettingUpLIRC "Zap�n�m podporu LIRC...\n"
#define MSGTR_LIRCdisabled "Nebudete moci pou��vat d�lkov� ovlada�.\n"
#define MSGTR_LIRCopenfailed "Nepovedlo se zapnout podporu LIRC.\n"
#define MSGTR_LIRCcfgerr "Nepovedlo se p�e��st konfigura�n� soubor LIRC %s.\n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "Nemohu nal�zt video filtr '%s'\n"
#define MSGTR_CouldNotOpenVideoFilter "Nemohu otev��t video filtr '%s'\n"
#define MSGTR_OpeningVideoFilter "Otev�r�m video filtr: "
#define MSGTR_CannotFindColorspace "Ani p�i vlo�en� 'scale' nemohu nal�zt spole�n� barevn� prostor :(\n"

// vd.c
#define MSGTR_CodecDidNotSet "V. dekod�r: Kodek nenastavil sh->disp_w a sh->disp_h, pokou��m se to obej�t.\n"
#define MSGTR_VoConfigRequest "V. dekod�r: Po�adovan� konfigurace vo - %d x %d (preferovan� csp: %s)\n"
#define MSGTR_CouldNotFindColorspace "Nemohu nal�zt spole�n� barevn� prostor - zkou��m to znovu s -vf scale...\n"
#define MSGTR_MovieAspectIsSet "Pom�r stran obrazu filmu je %.2f:1 - �k�luji na spr�vn� pom�r.\n"
#define MSGTR_MovieAspectUndefined "Pom�r stran obrazu filmu nen� definov�n - nem�n�m velikost.\n"

// vd_dshow.c, vd_dmo.c
#define MSGTR_DownloadCodecPackage "Pot�ebujete aktualizovat nebo nainstalovat bin�rn� kodeky.\nJd�te na http://mplayerhq.hu/homepage/dload.html\n"
#define MSGTR_DShowInitOK "INFO: Inicializace Win32/DShow videokodeku OK.\n"
#define MSGTR_DMOInitOK "INFO: Inicializace Win32/DMO videokodeku OK.\n"

// x11_common.c
#define MSGTR_EwmhFullscreenStateFailed "\nX11: Nemohu poslat ud�lost EWMH fullscreen!\n"

#define MSGTR_InsertingAfVolume "[Mixer] Hardwarov� mix�r nen� k dispozici, vkl�d�m filtr pro hlasitost.\n"
#define MSGTR_NoVolume "[Mixer] ��zen� hlasitosti nen� dostupn�.\n"

// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "O aplikaci"
#define MSGTR_FileSelect "Vybrat soubor..."
#define MSGTR_SubtitleSelect "Vybrat titulky..."
#define MSGTR_OtherSelect "Vybrat..."
#define MSGTR_AudioFileSelect "Vybrat extern� zvukov� kan�l..."
#define MSGTR_FontSelect "Vybrat font..."
// Pozn�mka: Pokud zm�n�te MSGTR_PlayList, ujist�te se pros�m, �e vyhovuje i pro  MSGTR_MENU_PlayList
#define MSGTR_PlayList "Playlist"
#define MSGTR_Equalizer "Ekvaliz�r"
#define MSGTR_SkinBrowser "Prohl�e� t�mat"
#define MSGTR_Network "S�ov� vys�l�n�..."
// Pozn�mka: Pokud zm�n�te MSGTR_Preferences, ujist�te se pros�m, �e vyhovuje i pro  MSGTR_MENU_Preferences
#define MSGTR_Preferences "Nastaven�" // P�edvolby?
#define MSGTR_AudioPreferences "Konfigurace ovlada�e zvuku"
#define MSGTR_NoMediaOpened "Nic nen� otev�eno."
#define MSGTR_VCDTrack "VCD stopa %d"
#define MSGTR_NoChapter "��dn� kapitola" //bez kapitoly?
#define MSGTR_Chapter "Kapitola %d"
#define MSGTR_NoFileLoaded "Nen� na�ten ��dn� soubor."

// --- buttons ---
#define MSGTR_Ok "OK"
#define MSGTR_Cancel "Zru�it"
#define MSGTR_Add "P�idat"
#define MSGTR_Remove "Odebrat"
#define MSGTR_Clear "Vynulovat"
#define MSGTR_Config "Konfigurace"
#define MSGTR_ConfigDriver "Konfigurovat ovlada�"
#define MSGTR_Browse "Prohl�et"

// --- error messages ---
#define MSGTR_NEMDB "Bohu�el nen� dostatek pam�ti pro vykreslovac� mezipam�."
#define MSGTR_NEMFMR "Bohu�el nen� dostatek pam�ti pro vykreslen� menu."
#define MSGTR_IDFGCVD "Bohu�el nebyl nalezen video ovlada� kompatibiln� s GUI."
#define MSGTR_NEEDLAVCFAME "Bohu�el nelze p�ehr�vat ne-MPEG s kartou DXR3/H+ bez p�eenk�dov�n�.\nPros�m, zapn�te lavc nebo fame v konfiguraci DXR3/H+."
#define MSGTR_UNKNOWNWINDOWTYPE "Nalezen nezn�m� typ okna ..."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[t�mata] chyba v konfigura�n�m souboru t�mat na ��dce %d: %s"
#define MSGTR_SKIN_WARNING1 "[t�mata] varov�n� v konfigura�n�m souboru t�mat na ��dce %d:\nwidget nalezen ale p�ed n�m nebyla nalezena ��dn� \"section\" (%s)"
#define MSGTR_SKIN_WARNING2 "[t�mata] varov�n� v konfigura�n�m souboru t�mat na ��dce %d:\nwidget nalezen ale p�ed n�m nebyla nalezena ��dn� \"subsection\" (%s)"
#define MSGTR_SKIN_WARNING3 "[t�mata] varov�n� v konfigura�n�m souboru t�mat na ��dce %d:\nwidget (%s) nepodporuje tuto subsekci"
#define MSGTR_SKIN_SkinFileNotFound "[skin] soubor ( %s ) nenalezen.\n"
#define MSGTR_SKIN_SkinFileNotReadable "[skin] soubor ( %s ) nelze p�e��st.\n"
#define MSGTR_SKIN_BITMAP_16bit  "bitmapa s hloubkou 16 bit� a m�n� nen� podporov�na (%s).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "soubor nenalezen (%s)\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "chyba �ten� BMP (%s)\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "chyba �ten� TGA (%s)\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "chyba �ten� PNG (%s)\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "form�t TGA zapouzd�en� v RLE nen� podporov�n (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "nezn�m� typ souboru (%s)\n"
#define MSGTR_SKIN_BITMAP_ConvertError "chyba konverze z 24 do 32 bit� (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "nezn�m� zpr�va: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "nedostatek pam�ti\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "deklarov�no p��li� mnoho font�\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "soubor fontu nebyl nalezen\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "soubor obrazu fontu nebyl nalezen\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "neexistuj�c� identifik�tor fontu (%s)\n"
#define MSGTR_SKIN_UnknownParameter "nezn�m� parametr (%s)\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "T�ma nenalezeno (%s).\n"
#define MSGTR_SKIN_SKINCFG_SelectedSkinNotFound "Vybran� skin ( %s ) nenalezen, zkou��m 'v�choz�'...\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "chyba p�i �ten� konfigura�n�ho souboru t�mat (%s)\n"
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
#define MSGTR_MENU_Stop "Stop"
#define MSGTR_MENU_NextStream "Dal�� proud"
#define MSGTR_MENU_PrevStream "P�edchoz� proud"
#define MSGTR_MENU_Size "Velikost"
#define MSGTR_MENU_HalfSize   "Polovi�n� velikost"
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
#define MSGTR_MENU_PlayList MSGTR_PlayList
#define MSGTR_MENU_SkinBrowser "Prohl�e� t�mat"
#define MSGTR_MENU_Preferences MSGTR_Preferences
#define MSGTR_MENU_Exit "Konec..."
#define MSGTR_MENU_Mute "Ztlumit"
#define MSGTR_MENU_Original "P�vodn�"
#define MSGTR_MENU_AspectRatio "Pom�r stran"
#define MSGTR_MENU_AudioTrack "Audio stopa"
#define MSGTR_MENU_Track "Stopa %d"
#define MSGTR_MENU_VideoTrack "Video stopa"

// --- equalizer
// Pozn�mka: Pokud zm�n�te MSGTR_EQU_Audio, ujist�te se pros�m, �e vyhovuje i pro MSGTR_PREFERENCES_Audio
#define MSGTR_EQU_Audio "Zvuk"
// Pozn�mka: Pokud zm�n�te MSGTR_EQU_Video, ujist�te se pros�m, �e vyhovuje i pro MSGTR_PREFERENCES_Video
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
#define MSGTR_PREFERENCES_Audio MSGTR_EQU_Audio
#define MSGTR_PREFERENCES_Video MSGTR_EQU_Video
#define MSGTR_PREFERENCES_SubtitleOSD "Titulky & OSD"
#define MSGTR_PREFERENCES_Codecs "Kodeky & demuxer"
// Pozn�mka: Pokud zm�n�te MSGTR_PREFERENCES_Misc, ujist�te se pros�m, �e vyhovuje i pro MSGTR_PREFERENCES_FRAME_Misc
#define MSGTR_PREFERENCES_Misc "Ostatn�"

#define MSGTR_PREFERENCES_None "Nic"
#define MSGTR_PREFERENCES_DriverDefault "v�choz� nastaven�"
#define MSGTR_PREFERENCES_AvailableDrivers "Dostupn� ovlada�e:"
#define MSGTR_PREFERENCES_DoNotPlaySound "Nep�ehr�vat zvuk"
#define MSGTR_PREFERENCES_NormalizeSound "Normalizovat zvuk"
#define MSGTR_PREFERENCES_EnEqualizer "Aktivovat ekvaliz�r"
#define MSGTR_PREFERENCES_SoftwareMixer "Aktivovat softwarov� sm�ova�"
#define MSGTR_PREFERENCES_ExtraStereo "Aktivovat extra stereo"
#define MSGTR_PREFERENCES_Coefficient "Koeficient:"
#define MSGTR_PREFERENCES_AudioDelay "Zpo�d�n� zvuku"
#define MSGTR_PREFERENCES_DoubleBuffer "Aktivovat dvojitou vyrovn�vac� pam�"
#define MSGTR_PREFERENCES_DirectRender "Aktivovat direct rendering"
#define MSGTR_PREFERENCES_FrameDrop "Aktivovat zahazov�n� sn�mk�"
#define MSGTR_PREFERENCES_HFrameDrop "Aktivovat TVRD� zahazov�n� sn�mk� (nebezpe�n�)"
#define MSGTR_PREFERENCES_Flip "P�evr�tit obraz vzh�ru nohama"
#define MSGTR_PREFERENCES_Panscan "Panscan:"
#define MSGTR_PREFERENCES_OSDTimer "�as a ostatn� ukazatele"
#define MSGTR_PREFERENCES_OSDProgress "Pouze ukazatele pozice a nastaven�"
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
#define MSGTR_PREFERENCES_Font "Font:"
#define MSGTR_PREFERENCES_FontFactor "Zv�t�en� Fontu:"
#define MSGTR_PREFERENCES_PostProcess "Aktivovat postprocessing"
#define MSGTR_PREFERENCES_AutoQuality "Automatick� ��zen� kvality:"
#define MSGTR_PREFERENCES_NI "Pou��t parser pro neprokl�dan� AVI form�t"
#define MSGTR_PREFERENCES_IDX "Znovu sestavit tabulku index�, pokud je to t�eba"
#define MSGTR_PREFERENCES_VideoCodecFamily "Rodina video kodeku:"
#define MSGTR_PREFERENCES_AudioCodecFamily "Rodina audio kodeku:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "Typ OSD"
#define MSGTR_PREFERENCES_FRAME_Subtitle "Titulky"
#define MSGTR_PREFERENCES_FRAME_Font "Font"
#define MSGTR_PREFERENCES_FRAME_PostProcess "Postprocessing"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "Kodek & demuxer"
#define MSGTR_PREFERENCES_FRAME_Cache "Vyrovn�vac� pam�"
#define MSGTR_PREFERENCES_FRAME_Misc MSGTR_PREFERENCES_Misc
#define MSGTR_PREFERENCES_Audio_Device "Za��zen�:"
#define MSGTR_PREFERENCES_Audio_Mixer "Mix�r:"
#define MSGTR_PREFERENCES_Audio_MixerChannel "Kan�l mix�ru:"
#define MSGTR_PREFERENCES_Message "Pozor, n�kter� nastaven� pot�ebuj� pro svou funkci restartovat p�ehr�v�n�!"
#define MSGTR_PREFERENCES_DXR3_VENC "Video enkod�r:"
#define MSGTR_PREFERENCES_DXR3_LAVC "Pou��t LAVC (FFmpeg)"
#define MSGTR_PREFERENCES_DXR3_FAME "Pou��t FAME"
#define MSGTR_PREFERENCES_FontEncoding1 "Unicode"
#define MSGTR_PREFERENCES_FontEncoding2 "Z�padoevropsk� jazyky (ISO-8859-1)"
#define MSGTR_PREFERENCES_FontEncoding3 "Z�padoevropsk� jazyky s Eurem (ISO-8859-15)"
#define MSGTR_PREFERENCES_FontEncoding4 "Slovansk�/st�edoevropsk� jazyky (ISO-8859-2)"
#define MSGTR_PREFERENCES_FontEncoding5 "Esperanto, gael�tina, malt�z�tina, ture�tina (ISO-8859-3)"
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
#define MSGTR_PREFERENCES_ArtsBroken "Nov�j�� verze aRts jsou nekompatibiln� "\
           "s GTK 1.x a zhavaruj� GMPlayer!"

#define MSGTR_ABOUT_UHU "V�voj GUI je sponzorov�n firmou UHU Linux\n"
#define MSGTR_ABOUT_CoreTeam "   Hlavn� v�voj��i programu MPlayer:\n"
#define MSGTR_ABOUT_AdditionalCoders "   Dal�� v�voj��i:\n"
#define MSGTR_ABOUT_MainTesters "   Hlavn� teste�i:\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "Kritick� chyba!"
#define MSGTR_MSGBOX_LABEL_Error "Chyba!"
#define MSGTR_MSGBOX_LABEL_Warning "Varov�n�!"

// bitmap.c

#define MSGTR_NotEnoughMemoryC32To1 "[c32to1] nedostatek pam�ti pro obr�zek\n"
#define MSGTR_NotEnoughMemoryC1To32 "[c1to32] nedostatek pam�ti pro obr�zek\n"

// cfg.c

#define MSGTR_ConfigFileReadError "[cfg] chyba p�i �ten� konfigura�n�ho souboru...\n"
#define MSGTR_UnableToSaveOption "[cfg] Nelze ulo�it volbu '%s'.\n"

// interface.c

#define MSGTR_DeletingSubtitles "[GUI] Ma�u titulky.\n"
#define MSGTR_LoadingSubtitles "[GUI] Na��t�m titulky: %s\n"
#define MSGTR_AddingVideoFilter "[GUI] P�id�v�m video filtr: %s\n"
#define MSGTR_RemovingVideoFilter "[GUI] Odstra�uji video filtr: %s\n"

// mw.c

#define MSGTR_NotAFile "Toto nevypad� jako soubor: %s !\n"

// ws.c

#define MSGTR_WS_CouldNotOpenDisplay "[ws] Nelze otev��t display.\n"
#define MSGTR_WS_RemoteDisplay "[ws] Vzd�len� display, vyp�n�m XMITSHM.\n"
#define MSGTR_WS_NoXshm "[ws] Promi�te, ale v� syst�m nepodporuje roz���en� X shared memory.\n"
#define MSGTR_WS_NoXshape "[ws] Promi�te, ale v� syst�m nepodporuje roz���en� XShape.\n"
#define MSGTR_WS_ColorDepthTooLow "[ws] Promi�te, ale barevn� hloubka je p��li� mal�.\n"
#define MSGTR_WS_TooManyOpenWindows "[ws] P��li� mnoho otev�en�ch oken.\n"
#define MSGTR_WS_ShmError "[ws] chyba roz���en� shared memory\n"
#define MSGTR_WS_NotEnoughMemoryDrawBuffer "[ws] Promi�te, nedostatek pam�ti pro vykreslen� bufferu.\n"
#define MSGTR_WS_DpmsUnavailable "DPMS nen� k dispozici?\n"
#define MSGTR_WS_DpmsNotEnabled "Nelze zapnout DPMS.\n"

// wsxdnd.c

#define MSGTR_WS_NotAFile "Toto nevypad� jako soubor...\n"
#define MSGTR_WS_DDNothing "D&D: Nic se nevr�tilo!\n"

#endif

// ======================= VO Video Output drivers ========================

#define MSGTR_VOincompCodec "Vybran� video_out za��zen� je nekompatibiln� s t�mto kodekem.\n"
#define MSGTR_VOincompCodec "Vybran� video_out za��zen� je nekompatibiln� s t�mto kodekem.\n"\
                "Zkuste rovn� p�idat filtr scale, �ili -vf spp,scale nam�sto -vf spp.\n"
#define MSGTR_VO_GenericError "Tato chyba nastala"
#define MSGTR_VO_UnableToAccess "Nem�m p��stup k"
#define MSGTR_VO_ExistsButNoDirectory "ji� existuje, ale nen� to adres��."
#define MSGTR_VO_DirExistsButNotWritable "V�stupn� adres�� ji� existuje, ale nelze do n�j zapisovat."
#define MSGTR_VO_DirExistsAndIsWritable "V�stupn� adres�� ji� existuje a lze do n�j zapisovat."
#define MSGTR_VO_CantCreateDirectory "Nelze vytvo�it v�stupn� adres��."
#define MSGTR_VO_CantCreateFile "Nelze vytvo�it v�stupn� soubor."
#define MSGTR_VO_DirectoryCreateSuccess "�sp�n� vytvo�en v�stupn� adres��."
#define MSGTR_VO_ParsingSuboptions "Interpretuji podvolby."
#define MSGTR_VO_SuboptionsParsedOK "Podvolby interpretov�ny OK."
#define MSGTR_VO_ValueOutOfRange "Hodnota je mimo rozsah"
#define MSGTR_VO_NoValueSpecified "Nebyla zad�na hodnota."
#define MSGTR_VO_UnknownSuboptions "Nezn�m�(�) podvolba(y)"

// vo_aa.c

#define MSGTR_VO_AA_HelpHeader "\n\nZde jsou podvolby aalib vo_aa:\n"
#define MSGTR_VO_AA_AdditionalOptions "Dodate�n� volby vo_aa zaji��uj�:\n" \
"  help        vyp�e tuto n�pov�du\n" \
"  osdcolor    nastav� barvu osd\n  subcolor    nastav� barvu titulk�\n" \
"        parametry barev jsou:\n           0 : normal\n" \
"           1 : dim\n           2 : bold\n           3 : boldfont\n" \
"           4 : reverse\n           5 : special\n\n\n"

// vo_jpeg.c
#define MSGTR_VO_JPEG_ProgressiveJPEG "Zapnut progresivn� JPEG."
#define MSGTR_VO_JPEG_NoProgressiveJPEG "Vypnut progresivn� JPEG."
#define MSGTR_VO_JPEG_BaselineJPEG "Zapnut z�kladn� JPEG."
#define MSGTR_VO_JPEG_NoBaselineJPEG "Vypnut z�kladn� JPEG."

// vo_pnm.c
#define MSGTR_VO_PNM_ASCIIMode "Zapnut ASCII re�im."
#define MSGTR_VO_PNM_RawMode "Zapnut surov� (Raw) re�im."
#define MSGTR_VO_PNM_PPMType "Budou zapisov�ny PPM soubory."
#define MSGTR_VO_PNM_PGMType "Budou zapisov�ny PGM soubory."
#define MSGTR_VO_PNM_PGMYUVType "Budou zapisov�ny PGMYUV soubory."

// vo_yuv4mpeg.c
#define MSGTR_VO_YUV4MPEG_InterlacedHeightDivisibleBy4 "Prokl�dan� re�im obrazu vy�aduje v��ku obrazu d�litelnou 4."
#define MSGTR_VO_YUV4MPEG_InterlacedLineBufAllocFail "Nelze alokovat ��dkovou vyrovn�vac� pam� pro re�im prokl�dan�ho obrazu."
#define MSGTR_VO_YUV4MPEG_InterlacedInputNotRGB "Vstup nen� RGB, nelze odd�lit jasovou slo�ku podle pol�!"
#define MSGTR_VO_YUV4MPEG_WidthDivisibleBy2 "���ka obrazu mus� b�t d�liteln� 2."
#define MSGTR_VO_YUV4MPEG_NoMemRGBFrameBuf "Nen� dostatek pam�ti pro alokaci RGB framebufferu."
#define MSGTR_VO_YUV4MPEG_OutFileOpenError "Nelze z�skat pam� nebo ukazatel souboru pro z�pis \"%s\"!"
#define MSGTR_VO_YUV4MPEG_OutFileWriteError "Chyba p�i z�pisu obr�zku na v�stup!"
#define MSGTR_VO_YUV4MPEG_UnknownSubDev "Nezn�m� podza��zen�: %s"
#define MSGTR_VO_YUV4MPEG_InterlacedTFFMode "Pou��v�m prokl�dan� v�stupn� re�im, horn� pole nap�ed."
#define MSGTR_VO_YUV4MPEG_InterlacedBFFMode "Pou��v�m prokl�dan� v�stupn� re�im, doln� pole nap�ed."
#define MSGTR_VO_YUV4MPEG_ProgressiveMode "Pou��v�m (v�choz�) neprokl�dan� sn�mkov� re�im."

// Old vo drivers that have been replaced

#define MSGTR_VO_PGM_HasBeenReplaced "V�stupn� videorozhran� pgm bylo nahrazeno -vo pnm:pgmyuv.\n"
#define MSGTR_VO_MD5_HasBeenReplaced "V�stupn� videorozhran� md5 bylo nahrazeno -vo md5sum.\n"

// ======================= AO Audio Output drivers ========================

// libao2 

// audio_out.c
#define MSGTR_AO_ALSA9_1x_Removed "audio_out: moduly alsa9 a alsa1x byly odstran�ny, m�sto nich pou�ijte -ao alsa.\n"

// ao_oss.c
#define MSGTR_AO_OSS_CantOpenMixer "[AO OSS] audio_setup: Nelze otev��t mix�n� za��zen� %s: %s\n"
#define MSGTR_AO_OSS_ChanNotFound "[AO OSS] audio_setup: Mixer zvukov� karty nem� kan�l '%s', pou��v�m v�choz�.\n"
#define MSGTR_AO_OSS_CantOpenDev "[AO OSS] audio_setup: Nelze otev��t zvukov� za��zen� %s: %s\n"
#define MSGTR_AO_OSS_CantMakeFd "[AO OSS] audio_setup: Nelze prov�st blokov�n� souborov�ho deskriptoru: %s\n"
#define MSGTR_AO_OSS_CantSet "[AO OSS] Zvukov� za��zen� %s nelze nastavit na v�stup %s, zkou��m %s...\n"
#define MSGTR_AO_OSS_CantSetChans "[AO OSS] audio_setup: Selhalo nastaven� v�stupn�ho zvukov�ho za��zen� na %d kan�l�.\n"
#define MSGTR_AO_OSS_CantUseGetospace "[AO OSS] audio_setup: Ovlada� nepodporuje SNDCTL_DSP_GETOSPACE :-(\n"
#define MSGTR_AO_OSS_CantUseSelect "[AO OSS]\n   ***  Ovlada� Va�� zvukov� karty NEPODPORUJE select()  ***\n P�ekompilujte MPlayer s #undef HAVE_AUDIO_SELECT v config.h !\n\n"
#define MSGTR_AO_OSS_CantReopen "[AO OSS]\nKritick� chyba: *** NELZE ZNOVUOTEV��T / RESTARTOVAT ZVUKOV� ZA��ZEN� *** %s\n"

// ao_arts.c
#define MSGTR_AO_ARTS_CantInit "[AO ARTS] %s\n"
#define MSGTR_AO_ARTS_ServerConnect "[AO ARTS] P�ipojen ke zvukov�mu serveru.\n"
#define MSGTR_AO_ARTS_CantOpenStream "[AO ARTS] Nelze otev��t datov� proud.\n"
#define MSGTR_AO_ARTS_StreamOpen "[AO ARTS] Datov� proud otev�en.\n"
#define MSGTR_AO_ARTS_BufferSize "[AO ARTS] velikost vyrovn�vac� pam�ti: %d\n"

// ao_dxr2.c
#define MSGTR_AO_DXR2_SetVolFailed "[AO DXR2] Nastaven� hlasitosti na %d selhalo.\n"
#define MSGTR_AO_DXR2_UnsupSamplerate "[AO DXR2] dxr2: %d Hz nen� podporov�no, zkuste \"-aop list=resample\"\n"

// ao_esd.c
#define MSGTR_AO_ESD_CantOpenSound "[AO ESD] esd_open_sound selhalo: %s\n"
#define MSGTR_AO_ESD_LatencyInfo "[AO ESD] latence: [server: %0.2fs, s�: %0.2fs] (upravuji %0.2fs)\n"
#define MSGTR_AO_ESD_CantOpenPBStream "[AO ESD] selhalo otev�en� datov�ho proudu esd pro p�ehr�v�n�: %s\n"

// ao_mpegpes.c
#define MSGTR_AO_MPEGPES_CantSetMixer "[AO MPEGPES] selhalo nastaven� DVB zvukov�ho mixeru: %s\n" 
#define MSGTR_AO_MPEGPES_UnsupSamplerate "[AO MPEGPES] %d Hz nen� podporov�no, zkuste p�evzorkovat...\n"

// ao_null.c
// This one desn't even  have any mp_msg nor printf's?? [CHECK]

// ao_pcm.c
#define MSGTR_AO_PCM_FileInfo "[AO PCM] Soubor: %s (%s)\nPCM: Vzorkov�n�: %iHz Kan�l(y): %s Form�t %s\n"
#define MSGTR_AO_PCM_HintInfo "[AO PCM] Info:  nejrychlej��ho dumpingu dos�hnete s -vc dummy -vo null\nPCM: Info: pro z�pis WAVE soubor� pou�ijte -waveheader (v�choz�).\n" // v� n�kdo co je dumping?
#define MSGTR_AO_PCM_CantOpenOutputFile "[AO PCM] Selhalo otev�en� %s pro z�pis!\n"

// ao_sdl.c
#define MSGTR_AO_SDL_INFO "[AO SDL] Vzorkov�n�: %iHz Kan�l(y): %s Form�t %s\n"
#define MSGTR_AO_SDL_DriverInfo "[AO SDL] pou��v�m zvukov� ovlada� %s.\n"
#define MSGTR_AO_SDL_UnsupportedAudioFmt "[AO SDL] Nepodporovan� form�t zvuku: 0x%x.\n"
#define MSGTR_AO_SDL_CantInit "[AO SDL] Inicializace SDL Audio selhala: %s\n"
#define MSGTR_AO_SDL_CantOpenAudio "[AO SDL] Nelze otev��t zvuk: %s\n"

// ao_sgi.c
#define MSGTR_AO_SGI_INFO "[AO SGI] ovl�d�n�.\n"
#define MSGTR_AO_SGI_InitInfo "[AO SGI] init: Vzorkov�n�: %iHz Kan�l(y): %s Form�t %s\n"
#define MSGTR_AO_SGI_InvalidDevice "[AO SGI] p�ehr�v�n�: neplatn� za��zen�.\n"
#define MSGTR_AO_SGI_CantSetParms_Samplerate "[AO SGI] init: selhalo setparams: %s\nNelze nastavit po�adovan� vzorkov�n�.\n"
#define MSGTR_AO_SGI_CantSetAlRate "[AO SGI] init: AL_RATE nebyl p�ijat dan�m zdrojem.\n"
#define MSGTR_AO_SGI_CantGetParms "[AO SGI] init: selhalo getparams: %s\n"
#define MSGTR_AO_SGI_SampleRateInfo "[AO SGI] init: vzorkov�n� je nyn� %lf (po�adovan� kmito�et je %lf)\n"
#define MSGTR_AO_SGI_InitConfigError "[AO SGI] init: %s\n"
#define MSGTR_AO_SGI_InitOpenAudioFailed "[AO SGI] init: Nelze otev��t zvukov� kan�l: %s\n"
#define MSGTR_AO_SGI_Uninit "[AO SGI] uninit: ...\n"
#define MSGTR_AO_SGI_Reset "[AO SGI] reset: ...\n"
#define MSGTR_AO_SGI_PauseInfo "[AO SGI] audio_pause: ...\n"
#define MSGTR_AO_SGI_ResumeInfo "[AO SGI] audio_resume: ...\n"

// ao_sun.c
#define MSGTR_AO_SUN_RtscSetinfoFailed "[AO SUN] rtsc: selhalo SETINFO.\n"
#define MSGTR_AO_SUN_RtscWriteFailed "[AO SUN] rtsc: z�pis selhal."
#define MSGTR_AO_SUN_CantOpenAudioDev "[AO SUN] Nelze otev��t zvukov� za��zen� %s, %s  -> nebude zvuk.\n"
#define MSGTR_AO_SUN_UnsupSampleRate "[AO SUN] audio_setup: Va�e karta nepodporuje %d kan�lov�, %s, %d Hz vzorkov�n�.\n"
#define MSGTR_AO_SUN_CantUseSelect "[AO SUN]\n   ***  Ovlada� Va�� zvukov� karty NEPODPORUJE select()  ***\n P�ekompilujte MPlayer s #undef HAVE_AUDIO_SELECT v config.h !\n\n"
#define MSGTR_AO_SUN_CantReopenReset "[AO SUN]\nKritick� chyba: *** NELZE ZNOVUOTEV��T / RESTARTOVAT ZVUKOV� ZA��ZEN� (%s) ***\n"

// ao_alsa5.c
#define MSGTR_AO_ALSA5_InitInfo "[AO ALSA5] alsa-init: po�adovan� form�t: %d Hz, %d kan�l(�), %s\n"
#define MSGTR_AO_ALSA5_SoundCardNotFound "[AO ALSA5] alsa-init: ��dn� zvukov� karta nebyla nalezena.\n"
#define MSGTR_AO_ALSA5_InvalidFormatReq "[AO ALSA5] alsa-init: po�adov�n neplatn� form�t (%s) - v�stup odpojen.\n"
#define MSGTR_AO_ALSA5_PlayBackError "[AO ALSA5] alsa-init: chyba otev�en� p�ehr�v�n� zvuku: %s\n"
#define MSGTR_AO_ALSA5_PcmInfoError "[AO ALSA5] alsa-init: chyba v pcm info: %s\n"
#define MSGTR_AO_ALSA5_SoundcardsFound "[AO ALSA5] alsa-init: nalezeno %d zvukov�ch karet, pou��v�m: %s\n"
#define MSGTR_AO_ALSA5_PcmChanInfoError "[AO ALSA5] alsa-init: chyba info v pcm kan�lu: %s\n"
#define MSGTR_AO_ALSA5_CantSetParms "[AO ALSA5] alsa-init: chyba p�i nastavov�n� parametr�: %s\n"
#define MSGTR_AO_ALSA5_CantSetChan "[AO ALSA5] alsa-init: chyba p�i nastavov�n� kan�lu: %s\n"
#define MSGTR_AO_ALSA5_ChanPrepareError "[AO ALSA5] alsa-init: chyba p�i p��prav� kan�lu: %s\n"
#define MSGTR_AO_ALSA5_DrainError "[AO ALSA5] alsa-uninit: chyba playback drain: %s\n"
#define MSGTR_AO_ALSA5_FlushError "[AO ALSA5] alsa-uninit: chyba playback flush: %s\n" //to jsou n�zvy �e by jeden pad
#define MSGTR_AO_ALSA5_PcmCloseError "[AO ALSA5] alsa-uninit: chyba uzav�en� pcm: %s\n"
#define MSGTR_AO_ALSA5_ResetDrainError "[AO ALSA5] alsa-reset: chyba playback drain: %s\n"
#define MSGTR_AO_ALSA5_ResetFlushError "[AO ALSA5] alsa-reset: chyba playback flush: %s\n"
#define MSGTR_AO_ALSA5_ResetChanPrepareError "[AO ALSA5] alsa-reset: chyba p�i p��prav� kan�l�: %s\n"
#define MSGTR_AO_ALSA5_PauseDrainError "[AO ALSA5] alsa-pause: chyba playback drain: %s\n"
#define MSGTR_AO_ALSA5_PauseFlushError "[AO ALSA5] alsa-pause: chyba playback flush: %s\n"
#define MSGTR_AO_ALSA5_ResumePrepareError "[AO ALSA5] alsa-resume: chyba p�i p��prav� kan�l�: %s\n"
#define MSGTR_AO_ALSA5_Underrun "[AO ALSA5] alsa-play: podte�en� v alsa, restartuji proud.\n"
#define MSGTR_AO_ALSA5_PlaybackPrepareError "[AO ALSA5] alsa-play: chyba p��pravy p�ehr�v�n� zvuku: %s\n"
#define MSGTR_AO_ALSA5_WriteErrorAfterReset "[AO ALSA5] alsa-play: chyba p�i z�pisu po restartu: %s - vzd�v�m to.\n"
#define MSGTR_AO_ALSA5_OutPutError "[AO ALSA5] alsa-play: chyba v�stupu: %s\n"

// ao_plugin.c

#define MSGTR_AO_PLUGIN_InvalidPlugin "[AO PLUGIN] neplatn� z�suvn� modul: %s\n"

// ======================= AF Audio Filters ================================

// libaf 

// af_ladspa.c

#define MSGTR_AF_LADSPA_AvailableLabels "dostupn� n�zvy v"
#define MSGTR_AF_LADSPA_WarnNoInputs "VAROV�N�! Tento LADSPA plugin nem� audio vstupy.\n  Vstupn� audio sign�l bude ztracen."
#define MSGTR_AF_LADSPA_ErrMultiChannel "V�cekan�lov� (>2) pluginy nejsou podporov�ny (zat�m).\n  Pou��vejte pouze mono a stereo pluginy."
#define MSGTR_AF_LADSPA_ErrNoOutputs "Tento LADSPA plugin nem� audio v�stupy."
#define MSGTR_AF_LADSPA_ErrInOutDiff "Po�et audio vstup� LADSPA pluginu je odli�n� od po�tu audio v�stup�."
#define MSGTR_AF_LADSPA_ErrFailedToLoad "selhalo na�ten�"
#define MSGTR_AF_LADSPA_ErrNoDescriptor "Nelze nal�zt funkci ladspa_descriptor() v uveden� knihovn�."
#define MSGTR_AF_LADSPA_ErrLabelNotFound "Nelze nal�zt po�adovan� n�zev v knihovn� plugin�."
#define MSGTR_AF_LADSPA_ErrNoSuboptions "Nebyla zad�ny ��dn� podvolby"
#define MSGTR_AF_LADSPA_ErrNoLibFile "Nebyla zad�na ��dn� knihovna"
#define MSGTR_AF_LADSPA_ErrNoLabel "Nebyl zad�n n�zev ��dn�ho filtru"
#define MSGTR_AF_LADSPA_ErrNotEnoughControls "Na p��kazov�m ��dku bylo uvedeno m�lo voli��"
#define MSGTR_AF_LADSPA_ErrControlBelow "%s: Vstupn� voli� #%d je ni��� ne� minim�ln� hodnota %0.4f.\n"
#define MSGTR_AF_LADSPA_ErrControlAbove "%s: Vstupn� voli� #%d je vy��� ne� maxim�ln� hodnota %0.4f.\n"

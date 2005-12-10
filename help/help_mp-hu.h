// Originally translated by: Gabucino the Almighty! <gabucino@mplayerhq.hu>
// Send me money/hw/babes!
//... Okay enough of the hw, now send the other two!
//
// Updated by: Gabrov <gabrov@freemail.hu>
// Sync'ed with help_mp-en.h 1.201 (2005. 12. 11.)

// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"Ind�t�s:   mplayer [opci�k] [url|�tvonal/]f�jln�v\n"
"\n"
"Alapvet� opci�k: (az �sszes opci� list�j�hoz l�sd a man lapot!)\n"
" -vo <drv[:dev]> videomeghajt� �s -alegys�g kiv�laszt�sa (lista: '-vo help')\n"
" -ao <drv[:dev]> audiomeghajt� �s -alegys�g kiv�laszt�sa (lista: '-ao help')\n"
#ifdef HAVE_VCD
" vcd://<s�vsz�m>  lej�tsz�s (S)VCD (super video cd)-s�vb�l, k�zvetlen�l\n"
#endif
#ifdef USE_DVDREAD
" dvd://<titleno>  a megadott DVD s�v lej�tsz�sa, f�jl helyett\n"
" -alang/-slang    DVD audio/felirat nyelv kiv�laszt�sa (2 bet�s orsz�gk�ddal)\n"
#endif
" -ss <id�poz>     a megadott (m�sodperc v. �ra:perc:mperc) poz�ci�ra teker�s\n"
" -nosound         hanglej�tsz�s kikapcsol�sa\n"
" -fs              teljesk�perny�s lej�tsz�s (vagy -vm, -zoom, b�vebben l�sd man)\n"
" -x <x> -y <y>    felbont�s be�ll�t�sa (-vm vagy -zoom haszn�lata eset�n)\n"
" -sub <f�jl>      felhaszn�land� felirat-f�jl megad�sa (l�sd -subfps, -subdelay)\n"
" -playlist <f�jl> lej�tsz�si lista f�jl megad�sa\n"
" -vid x -aid y    lej�tszand� video- (x) �s audio- (y) streamek kiv�laszt�sa\n"
" -fps x -srate y  video (x k�pkocka/mp) �s audio (y Hz) r�ta megad�sa\n"
" -pp <min�s�g>    k�pjav�t�s fokozatainak be�ll�t�sa (l�sd a man lapot)\n"
" -framedrop       k�pkock�k eldob�s�nak enged�lyez�se (lass� g�pekhez)\n"
"\n"
"Fontosabb billenty�k: (a teljes lista a man-ban �s n�zd meg az input.conf f�jlt)\n"
" <-  vagy  ->     10 m�sodperces h�tra/el�re ugr�s\n"
" le vagy fel      1 percnyi h�tra/el�re ugr�s\n"
" pgdown v. pgup   10 percnyi h�tra/el�re ugr�s\n"
" < vagy >         1 f�jllal el�re/h�tra l�p�s a lej�tsz�si list�ban\n"
" p vagy SPACE     pillanat�llj (b�rmely billenty�re tov�bbmegy)\n"
" q vagy ESC       lej�tsz�s v�ge �s kil�p�s\n"
" + vagy -         audio k�sleltet�se � 0.1 m�sodperccel\n"
" o                OSD-m�d v�lt�sa:  nincs / keres�s�v / keres�s�v + id�\n"
" * vagy /         hanger� fel/le\n"
" x vagy z         felirat k�sleltet�se � 0.1 m�sodperccel\n"
" r vagy t         felirat poz�ci�j�nak megv�ltoztat�sa, l�sd -vf expand-ot is\n"
"\n"
" * * * A MANPAGE TOV�BBI R�SZLETEKET, OPCI�KAT, BILLENTY�KET TARTALMAZ! * * *\n"
"\n";
#endif

#define MSGTR_SamplesWanted "P�lda f�jlokra van sz�ks�g�nk ilyen form�tummal, hogy jobb legyen a t�mogat�sa. Ha neked van ilyened, keresd meg a fejleszt�ket.\n"

// ========================= MPlayer messages ===========================

// mplayer.c: 

#define MSGTR_Exiting "\nKil�p�s...\n"
#define MSGTR_ExitingHow "\nKil�p�s... (%s)\n"
#define MSGTR_Exit_quit "Kil�p�s"
#define MSGTR_Exit_eof "V�ge a f�jlnak"
#define MSGTR_Exit_error "V�gzetes hiba"
#define MSGTR_IntBySignal "\nAz MPlayer fut�sa %d-es szign�l miatt megszakadt a(z) %s modulban\n"
#define MSGTR_NoHomeDir "Nem tal�lom a HOME k�nyvt�rat.\n"
#define MSGTR_GetpathProblem "get_path(\"config\") probl�ma\n"
#define MSGTR_CreatingCfgFile "Konfigur�ci�s f�jl l�trehoz�sa: %s\n"
#define MSGTR_InvalidAOdriver "Nem l�tez� audio meghajt�: %s\nHaszn�ld az '-ao help' opci�t, hogy list�t kapj a haszn�lhat� ao meghajt�kr�l.\n"
#define MSGTR_CopyCodecsConf "(m�sold/linkeld az etc/codecs.conf f�jlt ~/.mplayer/codecs.conf-ba)\n"
#define MSGTR_BuiltinCodecsConf "Beford�tott codecs.conf haszn�lata.\n"
#define MSGTR_CantLoadFont "Nem tudom bet�lteni a k�vetkez� fontot: %s\n"
#define MSGTR_CantLoadSub "Nem tudom bet�lteni a feliratot: %s\n"
#define MSGTR_DumpSelectedStreamMissing "dump: V�GZETES HIBA: a k�rt stream nem tal�lhat�!\n"
#define MSGTR_CantOpenDumpfile "Nem tudom megnyitni a dump f�jlt!\n"
#define MSGTR_CoreDumped "Kinyomattam a cuccost, j�l.\n"
#define MSGTR_FPSnotspecified "Az FPS (k�pkocka/mp) �rt�k nincs megadva, vagy hib�s! Haszn�ld az -fps opci�t!\n"
#define MSGTR_TryForceAudioFmtStr "Megpr�b�lom a(z) %s audio codec-csal�dot haszn�lni...\n"
#define MSGTR_CantFindAudioCodec "Nem tal�lok codecet a(z) 0x%X audio-form�tumhoz!\n"
#define MSGTR_RTFMCodecs "Olvasd el a DOCS/HTML/hu/codecs.html f�jlt!\n"
#define MSGTR_TryForceVideoFmtStr "Megpr�b�lom a(z) %s video codec-csal�dot haszn�lni...\n"
#define MSGTR_CantFindVideoCodec "Nem tal�lok codec-et ami megfelel a kivalasztott vo-hoz �s 0x%X video-form�tumhoz!\n"
#define MSGTR_CannotInitVO "V�GZETES HIBA: Nem tudom elind�tani a video-meghajt�t!\n"
#define MSGTR_CannotInitAO "Nem tudom megnyitni az audio-egys�get -> nincs hang.\n"
#define MSGTR_StartPlaying "Lej�tsz�s ind�t�sa...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"         ***************************************\n"\
"         **** A rendszered t�l LASS� ehhez! ****\n"\
"         ***************************************\n"\
"Lehets�ges okok, �s megold�saik:\n"\
"- Legyakrabban : hib�s _audio_ meghajt�\n"\
"  - Pr�b�ld ki az -ao sdl opci�t, vagy haszn�ld az ALSA OSS emul�ci�j�t.\n"\
"  - Adj k�l�nb�z� �rt�keket az -autosync opci�nak, kezdetnek a 30 megteszi.\n"\
"- Lass� videokimenet\n"\
"  - Egy m�sik -vo meghajt� kipr�b�l�sa eredm�nyre vezethet (a list�hoz l�sd\n"\
"    -vo help), �s/vagy haszn�ld a -framedrop opci�t!\n"\
"- Lass� CPU\n"\
"  - Nagy felbont�s� DivX/DVD lej�tsz�s�val ne pr�b�lkozz gyenge processzoron!\n"\
"    Esetleg pr�b�lj ki lavdopts opci�kat, pl.\n"\
"    -vfm ffmpeg -lavdopts lowres=1:fast:skiploopfilter=all.\n"\
"- Hib�s f�jl\n"\
"  - A -nobps -ni -forceidx -mc 0 opci�k kombin�ci�val �rdemes sz�rakozni.\n"\
"- Lass� m�dia (NFS/SMB, DVD, VCD, stb)\n"\
"  - Pr�b�ld ki a -cache 8192 opci�t.\n"\
"- Tal�n egy non-interleaved AVI f�jlt pr�b�lsz -cache opci�val lej�tszani?\n"\
"  - Haszn�ld a -nocache opci�t.\n"\
"Tuninghoz tippeket a DOCS/HTML/hu/video.html f�jlban tal�lsz.\n"\
"Ha ez sem seg�t, olvasd el a DOCS/HTML/hu/bugreports.html f�jlt.\n\n"

#define MSGTR_NoGui "Az MPlayer grafikus fel�let N�LK�L lett ford�tva!\n"
#define MSGTR_GuiNeedsX "Az MPlayer grafikus fel�let�nek X11-re van sz�ks�ge!\n"
#define MSGTR_Playing "%s lej�tsz�sa\n"
#define MSGTR_NoSound "Audio: nincs hang!!!\n"
#define MSGTR_FPSforced "FPS k�nyszer�tve %5.3f  (ftime: %5.3f)\n"
#define MSGTR_CompiledWithRuntimeDetection "Fut�sidej� CPU detekt�l�s haszn�lata - FIGYELEM - ez nem optim�lis!\nA legjobb teljes�tm�ny el�r�s�hez ford�tsd �jra az MPlayer-t a --disable-runtime-cpudetection\n opci�val!\n"
#define MSGTR_CompiledWithCPUExtensions "x86-os CPU - a k�vetkez� kiterjeszt�sekkel:"
#define MSGTR_AvailableVideoOutputDrivers "Rendelkez�sre �ll� video meghajt�k:\n"
#define MSGTR_AvailableAudioOutputDrivers "Rendelkez�sre �ll� audio meghajt�k:\n"
#define MSGTR_AvailableAudioCodecs "Rendelkez�sre �ll� audio codec-ek:\n"
#define MSGTR_AvailableVideoCodecs "Rendelkez�sre �ll� video codec-ek:\n"
#define MSGTR_AvailableAudioFm "Rendelkez�sre �ll� (beford�tott) audio codec csal�dok/meghajt�k:\n"
#define MSGTR_AvailableVideoFm "Rendelkez�sre �ll� (beford�tott) video codec csal�dok/meghajt�k:\n"
#define MSGTR_AvailableFsType "A haszn�lhat� teljesk�perny�s r�teg-m�dok:\n"
#define MSGTR_UsingRTCTiming "Linux hardveres RTC id�z�t�s haszn�lata (%ldHz)\n"
#define MSGTR_CannotReadVideoProperties "Video: tulajdons�gok beolvas�sa nem lehets�ges.\n"
#define MSGTR_NoStreamFound "Nem tal�lhat� stream\n"
#define MSGTR_ErrorInitializingVODevice "Hiba a kiv�lasztott video_out (-vo) egys�g inicializ�sakor!\n"
#define MSGTR_ForcedVideoCodec "K�nyszer�tett video codec: %s\n"
#define MSGTR_ForcedAudioCodec "K�nyszer�tett audio codec: %s\n"
#define MSGTR_Video_NoVideo "Video: nincs video!!!\n"
#define MSGTR_NotInitializeVOPorVO "\nHIBA: Nem siker�lt a video filterek (-vf) vagy a video kimenet (-vo) inicializ�l�sa!\n"
#define MSGTR_Paused "\n  =====  SZ�NET  =====\r"
#define MSGTR_PlaylistLoadUnable "\nLej�tsz�si lista (%s) bet�lt�se sikertelen.\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- Az MPlayer egy 'illeg�lis utas�t�st' hajtott v�gre.\n"\
"  Lehet hogy a fut�sidej� CPU detekt�l� k�d hib�ja...\n"\
"  Olvasd el a DOCS/HTML/hu/bugreports.html f�jlt!\n"
#define MSGTR_Exit_SIGILL \
"- Az MPlayer egy 'illeg�lis utas�t�st' hajtott v�gre.\n"\
"  Ez akkor t�rt�nik amikor m�s CPU-n futtatod az MPlayer-t mint amire a\n"\
"  ford�t�s/optimaliz�l�s t�rt�nt.\n"\
"  Ellen�rizd!\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- Az MPlayer r�pke f�lrel�p�se miatt hiba l�pett fel a CPU/FPU/RAM-ban.\n"\
"  Ford�tsd �jra az MPlayer-t az --enable-debug opci�val, �s k�sz�ts egy\n"\
"  'gdb' backtrace-t. B�vebben: DOCS/HTML/hu/bugreports.html#bugreports_crash.\n"
#define MSGTR_Exit_SIGCRASH \
"- Az MPlayer �sszeomlott. Ennek nem lenne szabad megt�rt�nnie. Az ok lehet\n"\
"  egy hiba az MPlayer k�dj�ban _vagy_ a Te meghajt�idban, _vagy_ a gcc-ben.\n"\
"  Ha �gy v�led hogy ez egy MPlayer hiba, �gy olvasd el a\n"\
"  DOCS/HTML/hu/bugreports.html f�jlt �s k�vesd az utas�t�sait! Nem tudunk �s\n"\
"  nem fogunk seg�teni, am�g nem szolg�lsz megfelel� inform�ci�kkal a hiba\n"\
"  bejelent�sekor.\n"
#define MSGTR_LoadingConfig "'%s' konfigur�ci� bet�lt�se\n"
#define MSGTR_AddedSubtitleFile "SUB: felirat f�jl (%d) hozz�adva: %s\n"
#define MSGTR_RemovedSubtitleFile "SUB: felirat f�jl (%d) elt�vol�tva: %s\n"
#define MSGTR_ErrorOpeningOutputFile "Hiba a(z) [%s] f�jl �r�sakor!\n"
#define MSGTR_CommandLine "Parancs sor:"
#define MSGTR_RTCDeviceNotOpenable "%s megnyit�sa nem siker�lt: %s (a felhaszn�l� �ltal olvashat�nak kell lennie.)\n"
#define MSGTR_LinuxRTCInitErrorIrqpSet "Linux RTC inicializ�l�si hiba az ioctl-ben (rtc_irqp_set %lu): %s\n"
#define MSGTR_IncreaseRTCMaxUserFreq "Pr�b�ld ki ezt: \"echo %lu > /proc/sys/dev/rtc/max-user-freq\" hozz�adni a rendszer ind�t� script-jeidhez!\n"
#define MSGTR_LinuxRTCInitErrorPieOn "Linux RTC inicializ�l�si hiba az ioctl-ben (rtc_pie_on): %s\n"
#define MSGTR_UsingTimingType "%s id�z�t�s haszn�lata.\n"
#define MSGTR_NoIdleAndGui "Az -idle opci� nem haszn�lhat� a GMPlayerrel.\n"
#define MSGTR_MenuInitialized "Men� inicializ�lva: %s\n"
#define MSGTR_MenuInitFailed "Men� inicializ�l�s nem siker�lt.\n"
#define MSGTR_Getch2InitializedTwice "FIGYELEM: getch2_init k�tszer lett megh�vva!\n"
#define MSGTR_DumpstreamFdUnavailable "Ezt a folyamot nem lehet dump-olni - a f�jlle�r� nem el�rhet�.\n"
#define MSGTR_FallingBackOnPlaylist "Visszal�p�s a(z) %s lej�tsz�si lista �rtelmez�se k�zben...\n"
#define MSGTR_CantOpenLibmenuFilterWithThisRootMenu "A libmenu video sz�r�t nem siker�lt a(z) %s f�men�vel megnyitni.\n"
#define MSGTR_AudioFilterChainPreinitError "Hiba az audio sz�r� l�nc el�-inicializ�l�s�ban!\n"
#define MSGTR_LinuxRTCReadError "Linux RTC olvas�si hiba: %s\n"
#define MSGTR_SoftsleepUnderflow "Figyelem! Softsleep alulcsordul�s!\n"
#define MSGTR_DvdnavNullEvent "DVDNAV esem�ny NULL (NINCS)?!\n"
#define MSGTR_DvdnavHighlightEventBroken "DVDNAV esem�ny: Kiemel�s esem�ny hib�s\n"
#define MSGTR_DvdnavEvent "DVDNAV esem�ny: %s\n"
#define MSGTR_DvdnavHighlightHide "DVDNAV esem�ny: Kiemel�s elrejt�se\n"
#define MSGTR_DvdnavStillFrame "######################################## DVDNAV esem�ny: Still Frame: %d mp\n"
#define MSGTR_DvdnavNavStop "DVDNAV esem�ny: Nav Stop\n"
#define MSGTR_DvdnavNavNOP "DVDNAV esem�ny: Nav NOP\n"
#define MSGTR_DvdnavNavSpuStreamChangeVerbose "DVDNAV esem�ny: Nav SPU folyam v�lt�s: fizikai: %d/%d/%d logikai: %d\n"
#define MSGTR_DvdnavNavSpuStreamChange "DVDNAV esem�ny: Nav SPU folyam v�lt�s: fizikai: %d logikai: %d\n"
#define MSGTR_DvdnavNavAudioStreamChange "DVDNAV esem�ny: Nav Audio folyam v�lt�s: fizikai: %d logikai: %d\n"
#define MSGTR_DvdnavNavVTSChange "DVDNAV esem�ny: Nav VTS v�lt�s\n"
#define MSGTR_DvdnavNavCellChange "DVDNAV esem�ny: Nav cella v�lt�s\n"
#define MSGTR_DvdnavNavSpuClutChange "DVDNAV esem�ny: Nav SPU CLUT v�lt�s\n"
#define MSGTR_DvdnavNavSeekDone "DVDNAV esem�ny: Nav keres�s k�sz\n"
#define MSGTR_MenuCall "Men� h�v�s\n"

#define MSGTR_EdlCantUseBothModes "Nem haszn�lhatod az -edl �s -edlout kapcsol�kat egyszerre!\n"
#define MSGTR_EdlOutOfMem "Nem lehet elegend� mem�ri�t foglalni az EDL adatoknak.\n"
#define MSGTR_EdlRecordsNo "%d EDL akci�k olvas�sa.\n"
#define MSGTR_EdlQueueEmpty "Nincs olyan EDL akci�, amivel foglalkozni kellene.\n"
#define MSGTR_EdlCantOpenForWrite "Az EDL f�jlba [%s] nem lehet �rni.\n"
#define MSGTR_EdlCantOpenForRead "Az EDL f�jlt [%s] nem lehet olvasni.\n"
#define MSGTR_EdlNOsh_video "Az EDL nem haszn�lhat� video n�lk�l, letiltva.\n"
#define MSGTR_EdlNOValidLine "Hib�s EDL sor: %s\n"
#define MSGTR_EdlBadlyFormattedLine "Hib�s form�tum� EDL sor [%d], kihagyva.\n"
#define MSGTR_EdlBadLineOverlap "Az utols� meg�ll�t�si poz�ci� [%f] volt; a k�vetkez� indul�si "\
"[%f]. A bejegyz�seknek id�rendben kell lenni�k, nem �tlapolhat�ak. Kihagyva.\n"
#define MSGTR_EdlBadLineBadStop "A meg�ll�t�si id�nek a kezd�si id� ut�n kell lennie.\n"

// mplayer.c OSD

#define MSGTR_OSDenabled "bekapcsolva"
#define MSGTR_OSDdisabled "kikapcsolva"
#define MSGTR_OSDnone "nincs"
#define MSGTR_OSDunknown "ismeretlen"
#define MSGTR_OSDDVDNAV "DVDNAV: %s"
#define MSGTR_OSDChannel "Csatorna: %s"
#define MSGTR_OSDSubtitles "Felirat: %s"
#define MSGTR_OSDSubtitlesOff "Felirat: ki"
#define MSGTR_OSDSubtitlesLanguage "Felirat: (%d) %s"
#define MSGTR_OSDSub "Sub: (%d) %s%s"
#define MSGTR_OSDSubDelay "Sub k�s�s: %d ms"
#define MSGTR_OSDSubPosition "Sub poz�ci�: %d/100"
#define MSGTR_OSDSubAlignment "Sub igaz�t�s: %s"
#define MSGTR_OSDAVDelay "A-V k�s�s: %d ms"
#define MSGTR_OSDSpeed "Sebess�g: x %6.2f"
#define MSGTR_OSDStayOnTop "Mindig fel�l: %s"
#define MSGTR_OSDRootwin "F�ablak: %s"
#define MSGTR_OSDBorder "Hat�r: %s"
#define MSGTR_OSDFramedrop "K�pkocka dob�s: %s"
#define MSGTR_OSDFramedropOn "be"
#define MSGTR_OSDFramedropHard "er�s"
#define MSGTR_OSDFramedropOff "ki"
#define MSGTR_OSDosd "OSD: %s"
#define MSGTR_OSDSubBottom "alul"
#define MSGTR_OSDSubCenter "k�z�pen"
#define MSGTR_OSDSubTop "fent"

// mencoder.c:

#define MSGTR_UsingPass3ControllFile "Pass3 vez�rl� f�jl haszn�lata: %s\n"
#define MSGTR_MissingFilename "\nHi�nyz� f�jln�v!\n\n"
#define MSGTR_CannotOpenFile_Device "F�jl/eszk�z megnyit�sa sikertelen.\n"
#define MSGTR_CannotOpenDemuxer "Demuxer megh�v�sa sikertelen.\n"
#define MSGTR_NoAudioEncoderSelected "\nNem v�lasztott�l ki audio enk�dert (-oac)! V�lassz egyet (l�sd -oac help), vagy haszn�ld a -nosound opci�t!\n"
#define MSGTR_NoVideoEncoderSelected "\nNem v�lasztott�l ki video enk�dert (-ovc)! V�lassz egyet (l�sd -ovc help)!\n"
#define MSGTR_CannotOpenOutputFile "Nem tudom a kimeneti f�jlt (%s) megnyitni.\n"
#define MSGTR_EncoderOpenFailed "Enk�der h�v�sa sikertelen.\n"
#define MSGTR_ForcingOutputFourcc "Kimeneti fourcc k�nyszer�t�se: %x [%.4s]\n"
#define MSGTR_ForcingOutputAudiofmtTag "Audi� form�tum tag k�nyszer�t�se: 0x%x\n"
#define MSGTR_WritingAVIHeader "AVI fejl�c �r�sa...\n"
#define MSGTR_DuplicateFrames "\n%d darab k�pkocka dupl�z�sa!!!\n"
#define MSGTR_SkipFrame "\nk�pkocka �tugr�sa!!!\n"
#define MSGTR_ResolutionDoesntMatch "\nAz �j vide� f�jl felbont�sa vagy sz�ntere k�l�nb�zik az el�z��t�l.\n"
#define MSGTR_FrameCopyFileMismatch "\nAz �sszes vide� f�jlnak azonos fps-sel, felbont�ssal, �s codec-kel kell rendelkeznie az -ovc copy-hoz.\n"
#define MSGTR_AudioCopyFileMismatch "\nAz �sszes f�jlnak azonos audi� codec-kel �s form�tummal kell rendelkeznie az -oac copy-hoz.\n"
#define MSGTR_NoAudioFileMismatch "\nNem lehet a csak vide�t tartalmaz� f�jlokat �sszekeverni audi� �s vide� f�jlokkal. Pr�b�ld a -nosound kapcsol�t.\n"
#define MSGTR_NoSpeedWithFrameCopy "FIGYELEM: A -speed nem biztos, hogy j�l m�k�dik az -oac copy-val!\n"\
"A k�dol�sod hib�s lehet!\n"
#define MSGTR_ErrorWritingFile "%s: hiba a f�jl �r�s�n�l.\n"
#define MSGTR_WritingAVIIndex "\nAVI index �r�sa...\n"
#define MSGTR_FixupAVIHeader "AVI fejl�c jav�t�sa...\n"
#define MSGTR_RecommendedVideoBitrate "Aj�nlott video bitr�ta %s CD-hez: %d\n"
#define MSGTR_VideoStreamResult "\nVideo stream: %8.3f kbit/mp  (%d B/s)  m�ret: %d byte  %5.3f mp  %d k�pkocka\n"
#define MSGTR_AudioStreamResult "\nAudio stream: %8.3f kbit/mp  (%d B/s)  m�ret: %d byte  %5.3f mp\n"
#define MSGTR_OpenedStream "k�sz: form�tum: %d  adat: 0x%X - 0x%x\n"
#define MSGTR_VCodecFramecopy "videocodec: framecopy (%dx%d %dbpp fourcc=%x)\n"
#define MSGTR_ACodecFramecopy "audiocodec: framecopy (form�tum=%x csati=%d r�ta=%ld bit=%d B/s=%ld sample-%ld)\n"
#define MSGTR_CBRPCMAudioSelected "CBR PCM audio kiv�lasztva\n"
#define MSGTR_MP3AudioSelected "MP3 audio kiv�lasztva\n"
#define MSGTR_CannotAllocateBytes "%d byte nem foglalhat� le\n"
#define MSGTR_SettingAudioDelay "Audi� k�sleltet�s be�ll�t�sa: %5.3f\n"
#define MSGTR_SettingAudioInputGain "Audio input er�s�t�se %f\n"
#define MSGTR_LamePresetEquals "\npreset=%s\n\n"
#define MSGTR_LimitingAudioPreload "Audio el�ret�lt�s korl�tozva 0.4 mp-re\n"
#define MSGTR_IncreasingAudioDensity "Audio t�m�r�t�s n�vel�se 4-re\n"
#define MSGTR_ZeroingAudioPreloadAndMaxPtsCorrection "Audio el�ret�lt�s 0-ra �ll�tva, max pts jav�t�s 0\n"
#define MSGTR_CBRAudioByterate "\n\nCBR audio: %ld byte/mp, %d byte/blokk\n"
#define MSGTR_LameVersion "LAME %s (%s) verzi�\n\n"
#define MSGTR_InvalidBitrateForLamePreset "Hiba: A megadott bitr�ta az ezen be�ll�t�shoz tartoz� hat�rokon k�v�l van\n"\
"\n"\
"Ha ezt a m�dot haszn�lod, \"8\" �s \"320\" k�z�tti �rt�ket kell megadnod\n"\
"\n"\
"Tov�bbi inform�ci�k�rt l�sd a \"-lameopts preset=help\" kapcsol�t!\n"
#define MSGTR_InvalidLamePresetOptions "Hiba: Nem adt�l meg �rv�nyes profilt �s/vagy opci�kat a preset-tel\n"\
"\n"\
"Az el�rhet� profilok:\n"\
"\n"\
"   <fast>        alap�rtelmezett\n"\
"   <fast>        extr�m\n"\
"                 �r�lt\n"\
"   <cbr> (ABR M�d) - Az ABR Mode be�p�tett. Haszn�lat�hoz\n"\
"                      csak adj meg egy bitr�t�t. P�ld�ul:\n"\
"                      \"preset=185\" aktiv�lja ezt a\n"\
"                      be�ll�t�st �s 185 lesz az �tlagos kbps.\n"\
"\n"\
"    N�h�ny p�lda:\n"\
"\n"\
"    \"-lameopts fast:preset=standard  \"\n"\
" or \"-lameopts  cbr:preset=192       \"\n"\
" or \"-lameopts      preset=172       \"\n"\
" or \"-lameopts      preset=extreme   \"\n"\
"\n"\
"Tov�bbi inform�ci�k�rt l�sd a \"-lameopts preset=help\" kapcsol�t!\n"
#define MSGTR_LamePresetsLongInfo "\n"\
"A preset kapcsol�k az�rt lettek l�trehozva, hogy a lehet� legjobb min�s�get biztos�ts�k.\n"\
"\n"\
"Legt�bbsz�r elvakult, k�ny�rtelen v�jtf�l�ek t�rgyalj�k ki �s �ll�tgatj�k �ket,\n"\
"hogy el�rj�k a c�ljaikat.\n"\
"\n"\
"Ezen v�ltoztat�sok folyamatosan friss�t�sre ker�lnek, hogy illeszkedjenek a\n"\
"leg�jabb fejleszt�sekhez �s az eredm�ny majdnem a legjobb min�s�get biztos�tsa\n"\
"Neked, ami el�rhet� a LAME-mel.\n"\
"\n"\
"Preset-ek aktiv�l�sa:\n"\
"\n"\
"   VBR (v�ltoz� bitr�ta) m�dokhoz (�ltal�ban a legjobb min�s�g):\n"\
"\n"\
"     \"preset=standard\" Ez a be�ll�t�s aj�nlott a legt�bb felhaszn�l�nak\n"\
"                             a zen�k nagy r�sz�n�l, �s m�r ez is megfelel�en\n"\
"                             j� min�s�get biztos�t.\n"\
"\n"\
"     \"preset=extreme\" Ha k�l�n�sen j� hall�sod �s hasonl�an j� felszerel�sed\n"\
"                             van, ez a be�ll�t�s meglehet�sen jobb min�s�get\n"\
"                             fog biztos�tani mint a \"standard\" m�d.\n"\
"                             \n"\
"\n"\
"   CBR (�lland� bitr�ta) 320kbps (a preset-tel el�rhet� legjobb min�s�g):\n"\
"\n"\
"     \"preset=insane\"  Ez a be�ll�t�s \"�gyuval ver�bre\" eset a legt�bb\n"\
"                             embern�l �s a legt�bb esetben, de ha abszol�t a\n"\
"                             legjobb min�s�gre van sz�ks�ged a f�jl m�ret�t�l\n"\
"                             f�ggetlen�l, akkor ez kell neked.\n"\
"\n"\
"   ABR (�tlagos bitr�ta) m�d (kiv�l� min�s�g az adott bitr�t�hoz de nem VBR):\n"\
"\n"\
"     \"preset=<kbps>\"  Ezen preset haszn�lat�val legt�bbsz�r j� min�s�get\n"\
"                             kapsz a megadott bitr�t�val. Az adott bitr�t�t�l\n"\
"                             f�gg�en ez a preset meghat�rozza az optim�lis\n"\
"                             be�ll�t�sokat.\n"\
"                             Am�g ez a megk�zel�t�s m�k�dik, messze nem olyan\n"\
"                             rugalmas, mint a VBR, �s legt�bbsz�r nem lesz\n"\
"                             olyan min�s�g�, mint a magas bitr�t�j� VBR-rel.\n"\
"\n"\
"A k�vetkez� opci�k is el�rhet�ek a megfelel� profilokhoz:\n"\
"\n"\
"   <fast>        standard\n"\
"   <fast>        extr�m\n"\
"                 �r�lt\n"\
"   <cbr> (ABR m�d) - Az ABR m�d be�p�tett. Haszn�lat�hoz egyszer�en\n"\
"                     csak add meg a bitr�t�t. P�ld�ul:\n"\
"                     \"preset=185\" ezt a preset-et aktiv�lja\n"\
"                     �s 185-�s �tlagos kbps-t haszn�l.\n"\
"\n"\
"   \"fast\" - Enged�lyezi az �j, gyors VBR-t a megfelel� profilban.\n"\
"            H�tr�nya, hogy a sebess�g miatt a bitr�ta gyakran \n"\
"            kicsit nagyobb lesz, mint a norm�l m�dban, a min�s�g pedig\n"\
"            kicsit rosszabb.\n"\
"   Figyelem: a jelenlegi �llapotban a gyors preset-ek t�l nagy bitr�t�t\n"\
"             produk�lnak a norm�lis preset-hez k�pest.\n"\
"\n"\
"   \"cbr\"  - Ha az ABR m�dot haszn�lod (l�sd feljebb) egy olyan bitr�t�val\n"\
"            mint a 80, 96, 112, 128, 160, 192, 224, 256, 320, haszn�lhatod\n"\
"            a \"cbr\" opci�t hogy el��rd a CBR m�dot a k�dol�sn�l az\n"\
"            alap�rtelmezett abr m�d helyett. Az ABR jobb min�s�get biztos�t,\n"\
"            de a CBR hasznosabb lehet olyan esetekben, mint pl. amikor fontos\n"\
"            az MP3 Interneten t�rt�n� streamelhet�s�ge.\n"\
"\n"\
"    P�ld�ul:\n"\
"\n"\
"    \"-lameopts fast:preset=standard  \"\n"\
" or \"-lameopts  cbr:preset=192       \"\n"\
" or \"-lameopts      preset=172       \"\n"\
" or \"-lameopts      preset=extreme   \"\n"\
"\n"\
"\n"\
"P�r �ln�v, ami el�rhet� az ABR m�dban:\n"\
"phone => 16kbps/mono        phon+/lw/mw-eu/sw => 24kbps/mono\n"\
"mw-us => 40kbps/mono        voice => 56kbps/mono\n"\
"fm/radio/tape => 112kbps    hifi => 160kbps\n"\
"cd => 192kbps               studio => 256kbps"
#define MSGTR_LameCantInit "A Lame opci�k nem �ll�that�ak be, ellen�rizd a"\
"bitr�t�t/mintav�teli r�t�t, n�h�ny nagyon alacsony bitr�t�hoz (<32) alacsonyabb"\
"mintav�teli r�ta kell (pl. -srate 8000)."\
"Ha minden m�s sikertelen, pr�b�lj ki egy preset-et."
#define MSGTR_ConfigfileError "konfigur�ci�s f�jl hib�ja"
#define MSGTR_ErrorParsingCommandLine "hiba a parancssor �rtelmez�sekor"
#define MSGTR_VideoStreamRequired "Video stream sz�ks�ges!\n"
#define MSGTR_ForcingInputFPS "az input fps ink�bb %5.2f-k�nt lesz �rtelmezve\n"
#define MSGTR_RawvideoDoesNotSupportAudio "A RAWVIDEO kimeneti f�jl form�tum nem t�mogatja a hangot - audio letiltva\n"
#define MSGTR_DemuxerDoesntSupportNosound "Ez a demuxer m�g nem t�mogatja a -nosound kapcsol�t.\n"
#define MSGTR_MemAllocFailed "Nem siker�lt a mem�riafoglal�s\n"
#define MSGTR_NoMatchingFilter "Nem tal�ltam megfelel� sz�r�t/ao form�tumot!\n"
#define MSGTR_MP3WaveFormatSizeNot30 "sizeof(MPEGLAYER3WAVEFORMAT)==%d!=30, tal�n hib�s C ford�t�?\n"
#define MSGTR_NoLavcAudioCodecName "Audio LAVC, hi�nyz� codec n�v!\n"
#define MSGTR_LavcAudioCodecNotFound "Audio LAVC, nem tal�lhat� k�dol� a(z) %s codec-hez.\n"
#define MSGTR_CouldntAllocateLavcContext "Audio LAVC, nem tal�lhat� a kontextus!\n"
#define MSGTR_CouldntOpenCodec "A(z) %s codec nem nyithat� meg, br=%d\n"
#define MSGTR_CantCopyAudioFormat "A(z) 0x%x audi� form�tum nem kompatibilis a '-oac copy'-val, k�rlek pr�b�ld meg a '-oac pcm' helyette vagy haszn�ld a '-fafmttag'-ot a fel�lb�r�l�s�hoz.\n"

// cfg-mencoder.h:

#define MSGTR_MEncoderMP3LameHelp "\n\n"\
" vbr=<0-4>     a v�ltoz� bitr�t�j� k�dol�s m�dja\n"\
"                0: cbr\n"\
"                1: mt\n"\
"                2: rh(alap�rtelmezett)\n"\
"                3: abr\n"\
"                4: mtrh\n"\
"\n"\
" abr           �tlagos bitr�ta\n"\
"\n"\
" cbr           konstans bitr�ta\n"\
"               El��rja a CBR m�d� k�dol�st a k�s�bbi ABR m�dokban is.\n"\
"\n"\
" br=<0-1024>   bitr�ta kBit-ben (csak CBR �s ABR)\n"\
"\n"\
" q=<0-9>       min�s�g (0-legjobb, 9-legrosszabb) (csak VBR-n�l)\n"\
"\n"\
" aq=<0-9>      algoritmikus min�s�g (0-legjobb, 9-legrosszabb/leggyorsabb)\n"\
"\n"\
" ratio=<1-100> t�m�r�t�s ar�nya\n"\
"\n"\
" vol=<0-10>    audio bemenet hangereje\n"\
"\n"\
" mode=<0-3>    (alap: automatikus)\n"\
"                0: stereo\n"\
"                1: joint-stereo\n"\
"                2: dualchannel\n"\
"                3: mono\n"\
"\n"\
" padding=<0-2>\n"\
"                0: nincs\n"\
"                1: mind\n"\
"                2: �ll�t�s\n"\
"\n"\
" fast          valamivel gyorsabb VBR k�dol�s, kicsit rosszabb min�s�g �s\n"\
"               magasabb bitr�ta.\n"\
"\n"\
" preset=<�rt�k> A lehet� legjobb min�s�get biztos�tja.\n"\
"                 medium: VBR  k�dol�s,  kellemes min�s�g\n"\
"                 (150-180 kbps bitr�ta tartom�ny)\n"\
"                 standard:  VBR k�dol�s, j� min�s�g\n"\
"                 (170-210 kbps bitr�ta tartom�ny)\n"\
"                 extreme: VBR k�dol�s, nagyon j� min�s�g\n"\
"                 (200-240 kbps bitr�ta tartom�ny)\n"\
"                 insane:  CBR  k�dol�s, legjobb min�s�g\n"\
"                 (320 kbps bitr�ta)\n"\
"                 <8-320>: ABR k�dol�s �tlagban a megadott bitr�t�val.\n\n"

//codec-cfg.c:
#define MSGTR_DuplicateFourcc "dupla FourCC"
#define MSGTR_TooManyFourccs "t�l sok FourCCs/form�tum..."
#define MSGTR_ParseError "�rtelmez�si hiba"
#define MSGTR_ParseErrorFIDNotNumber "�rtelmez�si hiba (a form�tum ID nem sz�m?)"
#define MSGTR_ParseErrorFIDAliasNotNumber "�rtelmez�si hiba (a form�tum ID �ln�v nem sz�m?)"
#define MSGTR_DuplicateFID "duplik�lt form�tum ID"
#define MSGTR_TooManyOut "t�l sok kiesett..."
#define MSGTR_InvalidCodecName "\na codec(%s) n�v hib�s!\n"
#define MSGTR_CodecLacksFourcc "\na codec(%s) nem FourCC/form�tum�!\n"
#define MSGTR_CodecLacksDriver "\na codec(%s) nem rendelkezik vez�l�vel!\n"
#define MSGTR_CodecNeedsDLL "\na codec(%s) 'dll'-t ig�nyel!\n"
#define MSGTR_CodecNeedsOutfmt "\ncodec(%s) 'outfmt'-t ig�nyel!\n"
#define MSGTR_CantAllocateComment "Nem tudok mem�ri�t foglalni a megjegyz�snek. "
#define MSGTR_GetTokenMaxNotLessThanMAX_NR_TOKEN "get_token(): max >= MAX_MR_TOKEN!"
#define MSGTR_ReadingFile "%s olvas�sa: "
#define MSGTR_CantOpenFileError "Nem tudom megnyitni '%s'-t: %s\n"
#define MSGTR_CantGetMemoryForLine "Nem tudok mem�ri�t foglalni a 'line'-nak: %s\n"
#define MSGTR_CantReallocCodecsp "A '*codecsp' nem foglalhat� le �jra: %s\n"
#define MSGTR_CodecNameNotUnique "A(z) '%s' codec n�v nem egyedi."
#define MSGTR_CantStrdupName "Nem v�gezhet� el: strdup -> 'name': %s\n"
#define MSGTR_CantStrdupInfo "Nem v�gezhet� el: strdup -> 'info': %s\n"
#define MSGTR_CantStrdupDriver "Nem v�gezhet� el: strdup -> 'driver': %s\n"
#define MSGTR_CantStrdupDLL "Nem v�gezhet� el: strdup -> 'dll': %s"
#define MSGTR_AudioVideoCodecTotals "%d audi� & %d vide� codec\n"
#define MSGTR_CodecDefinitionIncorrect "A codec nincs megfelel�en defini�lva."
#define MSGTR_OutdatedCodecsConf "Ez a codecs.conf t�l r�gi �s nem kompatibilis az MPlayer ezen kiad�s�val!"

// divx4_vbr.c:
#define MSGTR_OutOfMemory "elfogyott a mem�ria"
#define MSGTR_OverridingTooLowBitrate "A megadott bitr�ta t�l alacsony ehhez a klipphez.\n"\
"A minim�lis lehets�ges bitr�ta ehhez a klipphez %.0f kbps. A felhaszn�l�i\n"\
"�rt�k fel�lb�r�lva.\n"

// fifo.c
#define MSGTR_CannotMakePipe "Nem hozhat� l�tre PIPE!\n"

// m_config.c
#define MSGTR_SaveSlotTooOld "T�l r�gi ment�si slotot tal�ltam az %d lvl-b�l: %d !!!\n"
#define MSGTR_InvalidCfgfileOption "A(z) %s kapcsol� nem haszn�lhat� konfigur�ci�s f�jlban.\n"
#define MSGTR_InvalidCmdlineOption "A(z) %s kapcsol� nem haszn�lhat� parancssorb�l.\n"
#define MSGTR_InvalidSuboption "Hiba: '%s' kapcsol�nak nincs '%s' alopci�ja.\n"
#define MSGTR_MissingSuboptionParameter "Hiba: a(z) '%s' '%s' alkapcsol�j�hoz param�ter kell!\n"
#define MSGTR_MissingOptionParameter "Hiba: a(z) '%s' kapcsol�hoz kell egy param�ter!\n"
#define MSGTR_OptionListHeader "\n N�v                  T�pus           Min        Max      Glob�l  CL    Cfg\n\n"
#define MSGTR_TotalOptions "\n�sszesen: %d kapcsol�\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "A CD-ROM meghajt� (%s) nem tal�lhat�!\n"
#define MSGTR_ErrTrackSelect "Hiba a VCD-s�v kiv�laszt�sakor!"
#define MSGTR_ReadSTDIN "Olvas�s a szabv�nyos bemenetr�l (stdin)...\n"
#define MSGTR_UnableOpenURL "Nem megnyithat� az URL: %s\n"
#define MSGTR_ConnToServer "Csatlakozom a szerverhez: %s\n"
#define MSGTR_FileNotFound "A f�jl nem tal�lhat�: '%s'\n"

#define MSGTR_SMBInitError "Samba kliens k�nyvt�r nem inicializ�lhat�: %d\n"
#define MSGTR_SMBFileNotFound "Nem nyithat� meg a h�l�zatr�l: '%s'\n"
#define MSGTR_SMBNotCompiled "Nincs beford�tva az MPlayerbe az SMB t�mogat�s\n"

#define MSGTR_CantOpenDVD "Nem tudom megnyitni a DVD eszk�zt: %s\n"
#define MSGTR_NoDVDSupport "Az MPlayer DVD t�mogat�s n�lk�l lett leford�tva, kil�p�s\n"
#define MSGTR_DVDwait "A lemez strukt�r�j�nak olvas�sa, k�rlek v�rj...\n"
#define MSGTR_DVDnumTitles "%d s�v van a DVD-n.\n"
#define MSGTR_DVDinvalidTitle "Helytelen DVD s�v: %d\n"
#define MSGTR_DVDnumChapters "Az adott DVD s�vban %d fejezet van.\n"
#define MSGTR_DVDinvalidChapter "Helytelen DVD fejezet: %d\n"
#define MSGTR_DVDinvalidChapterRange "Helytelen fejezet tartom�ny specifik�ci�: %s\n"
#define MSGTR_DVDinvalidLastChapter "Helytelen DVD utols� fejezet sz�m: %d\n"
#define MSGTR_DVDnumAngles "%d darab kamera�ll�s van ezen a DVD s�von.\n"
#define MSGTR_DVDinvalidAngle "Helytelen DVD kamera�ll�s: %d\n"
#define MSGTR_DVDnoIFO "Nem tudom a(z) %d. DVD s�vhoz megnyitni az IFO f�jlt.\n"
#define MSGTR_DVDnoVMG "A VMG inf�t nem lehet megnyitni!\n"
#define MSGTR_DVDnoVOBs "Nem tudom megnyitni a VOBS s�vokat (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDnoMatchingAudio "Nem tal�lhat� megfelel� nyelv� DVD audi�!\n"
#define MSGTR_DVDaudioChannel "Kiv�lasztott DVD audi� csatorna: %d nyelv: %c%c\n"
#define MSGTR_DVDnoMatchingSubtitle "Nincs megfelel� nyelv� DVD felirat f�jl!\n"
#define MSGTR_DVDsubtitleChannel "Kiv�lasztott DVD felirat csatorna: %d nyelv: %c%c\n"
#define MSGTR_DVDopenOk "DVD sikeresen megnyitva!\n"

// muxer_*.c:
#define MSGTR_TooManyStreams "T�l sok stream!"
#define MSGTR_RawMuxerOnlyOneStream "A rawaudio muxer csak egy audi� folyamot t�mogat!\n"
#define MSGTR_IgnoringVideoStream "Vide� folyam figyelmen k�v�l hagyva!\n"
#define MSGTR_UnknownStreamType "Figyelem! Ismeretlen folyam t�pus: %d\n"
#define MSGTR_WarningLenIsntDivisible "Figyelem! A len nem oszthat� a samplesize-zal!\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "Vigy�zat! T�bbsz�r�sen defini�lt Audio-folyam: %d (Hib�s f�jl?)\n"
#define MSGTR_VideoStreamRedefined "Vigy�zat! T�bbsz�r�sen defini�lt Video-folyam: %d (Hib�s f�jl?)\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: T�l sok (%d db, %d b�jt) audio-csomag a pufferben!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: T�l sok (%d db, %d b�jt) video-csomag a pufferben!\n"
#define MSGTR_MaybeNI "Tal�n ez egy nem �sszef�s�lt (interleaved) f�jl vagy a codec nem m�k�dik j�l?\n" \
		      "AVI f�jlokn�l pr�b�ld meg a non-interleaved m�d k�nyszer�t�s�t a -ni opci�val.\n"
#define MSGTR_SwitchToNi "\nRosszul �sszef�s�lt (interleaved) f�jl, �tv�lt�s -ni m�dba!\n"
#define MSGTR_Detected_XXX_FileFormat "Ez egy %s form�tum� f�jl!\n"
#define MSGTR_DetectedAudiofile "Audio f�jl detekt�lva!\n"
#define MSGTR_NotSystemStream "Nem MPEG System Stream form�tum... (tal�n Transport Stream?)\n"
#define MSGTR_InvalidMPEGES "Hib�s MPEG-ES-folyam? L�pj kapcsolatba a k�sz�t�kkel, lehet, hogy hiba!\n"
#define MSGTR_FormatNotRecognized "========= Sajnos ez a f�jlform�tum ismeretlen vagy nem t�mogatott ===========\n"\
				  "= Ha ez egy AVI, ASF vagy MPEG f�jl, l�pj kapcsolatba a k�sz�t�kkel (hiba)! =\n"
#define MSGTR_MissingVideoStream "Nincs k�pfolyam!\n"
#define MSGTR_MissingAudioStream "Nincs hangfolyam... -> hang n�lk�l\n"
#define MSGTR_MissingVideoStreamBug "Nincs k�pfolyam?! �rj a szerz�nek, lehet hogy hiba :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: a f�jl nem tartalmazza a k�rt hang vagy k�p folyamot\n"

#define MSGTR_NI_Forced "K�nyszer�tve"
#define MSGTR_NI_Detected "Detekt�lva"
#define MSGTR_NI_Message "%s NON-INTERLEAVED AVI form�tum!\n"

#define MSGTR_UsingNINI "NON-INTERLEAVED hib�s AVI form�tum haszn�lata!\n"
#define MSGTR_CouldntDetFNo "Nem tudom meghat�rozni a k�pkock�k sz�m�t (abszolut teker�shez)   \n"
#define MSGTR_CantSeekRawAVI "Nem tudok nyers .AVI-kban tekerni! (index kell, pr�b�ld az -idx kapcsol�val!)\n"
#define MSGTR_CantSeekFile "Nem tudok ebben a f�jlban tekerni!\n"

#define MSGTR_EncryptedVOB "Titkos�t�tt VOB f�jl! Olvasd el a DOCS/HTML/hu/dvd.html f�jlt!\n"

#define MSGTR_MOVcomprhdr "MOV: A t�m�r�tett fejl�cek t�mogat�s�hoz ZLIB kell!\n"
#define MSGTR_MOVvariableFourCC "MOV: Vigy�zat: v�ltoz� FOURCC detekt�lva!?\n"
#define MSGTR_MOVtooManyTrk "MOV: Vigy�zat: t�l sok s�v!"
#define MSGTR_FoundAudioStream "==> Megtal�lt audio folyam: %d\n"
#define MSGTR_FoundVideoStream "==> Megtal�lt video folyam: %d\n"
#define MSGTR_DetectedTV "TV detekt�lva! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "OGG demuxer megh�v�sa nem siker�lt\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: Audio folyam keres�se (id:%d)\n"
#define MSGTR_CannotOpenAudioStream "Audio folyam megnyit�sa sikertelen: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "Felirat folyam megnyit�sa sikertelen: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "Audio demuxer megh�v�sa sikertelen: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "Felirat demuxer megh�v�sa sikertelen: %s\n"
#define MSGTR_TVInputNotSeekable "TV bemenet nem tekerhet�! (Meg k�ne csin�lni hogy most v�ltson csatorn�t ;)\n"
#define MSGTR_DemuxerInfoAlreadyPresent "%s demuxer info m�r jelen van!\n"
#define MSGTR_ClipInfo "Klipp info: \n"

#define MSGTR_LeaveTelecineMode "\ndemux_mpg: 30000/1001fps NTSC form�tumot tal�ltam, framer�ta v�lt�s.\n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: 24000/1001fps progressz�v NTSC form�tumot tal�ltam, framer�ta v�lt�s.\n"

#define MSGTR_CacheFill "\rCache felt�lt�s: %5.2f%% (%d b�jt)   "
#define MSGTR_NoBindFound "Nincs semmi sem �sszerendelve a(z) '%s' gombbal"
#define MSGTR_FailedToOpen "Nem lehet megnyitni: %s\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "Nem tudom megnyitni a kodeket.\n"
#define MSGTR_CantCloseCodec "Nem tudom lez�rni a kodeket.\n"

#define MSGTR_MissingDLLcodec "HIBA: Nem tudom megnyitni a k�rt DirectShow kodeket: %s\n"
#define MSGTR_ACMiniterror "Nem tudom bet�lteni/inicializ�lni a Win32/ACM kodeket (hi�nyz� DLL f�jl?)\n"
#define MSGTR_MissingLAVCcodec "Nem tal�lom a(z) '%s' nev� kodeket a libavcodec-ben...\n"

#define MSGTR_MpegNoSequHdr "MPEG: V�GZETES: v�ge lett a f�jlnak mik�zben a szekvencia fejl�cet kerestem\n"
#define MSGTR_CannotReadMpegSequHdr "V�GZETES: Nem tudom olvasni a szekvencia fejl�cet!\n"
#define MSGTR_CannotReadMpegSequHdrEx "V�GZETES: Nem tudom olvasni a szekvencia fejl�c kiterjeszt�s�t!\n"
#define MSGTR_BadMpegSequHdr "MPEG: Hib�s szekvencia fejl�c!\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: Hib�s szekvencia fejl�c kiterjeszt�s!\n"

#define MSGTR_ShMemAllocFail "Nem tudok megosztott mem�ri�t lefoglalni\n"
#define MSGTR_CantAllocAudioBuf "Nem tudok kimeneti hangbuffer lefoglalni\n"

#define MSGTR_UnknownAudio "Ismeretlen/hi�nyz� hangform�tum, hang kikapcsolva\n"

#define MSGTR_UsingExternalPP "[PP] K�ls� min�s�gjav�t� sz�r� haszn�lata, max min�s�g = %d\n"
#define MSGTR_UsingCodecPP "[PP] Codecbeli min�s�gjav�t�s haszn�lata, max min�s�g = %d\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "'%s' video tulajdons�g nem t�mogatott a kiv�lasztott vo & vd meghajt�k �ltal!\n"
#define MSGTR_VideoCodecFamilyNotAvailableStr "A k�rt [%s] video codec csal�d (vfm=%s) nem kiv�laszthat� (ford�t�sn�l kapcsold be!)\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "A k�rt [%s] audio codec csal�d (afm=%s) nem kiv�laszthat� (ford�t�sn�l kapcsold be!)\n"
#define MSGTR_OpeningVideoDecoder "Video dek�der megh�v�sa: [%s] %s\n"
#define MSGTR_SelectedVideoCodec "Kiv�lasztott vide� codec: [%s] vfm: %s (%s)\n"
#define MSGTR_OpeningAudioDecoder "Audio dek�der megh�v�sa: [%s] %s\n"
#define MSGTR_SelectedAudioCodec "Kiv�lasztott audi� codec: [%s] afm: %s (%s)\n"
#define MSGTR_BuildingAudioFilterChain "Audi� sz�r� l�nc fel�p�t�se %dHz/%dch/%s -> %dHz/%dch/%s form�tumhoz...\n"
#define MSGTR_UninitVideoStr "uninit video: %s\n"
#define MSGTR_UninitAudioStr "uninit audio: %s\n"
#define MSGTR_VDecoderInitFailed "VDecoder init nem siker�lt :(\n"
#define MSGTR_ADecoderInitFailed "ADecoder init nem siker�lt :(\n"
#define MSGTR_ADecoderPreinitFailed "ADecoder preinit nem siker�lt :(\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: %d byte allok�l�sa bemeneti buffernek.\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: %d + %d = %d byte allok�l�sa bemeneti buffernek.\n"

// LIRC:
#define MSGTR_SettingUpLIRC "LIRC t�mogat�s ind�t�sa...\n"
#define MSGTR_LIRCdisabled "Nem fogod tudni haszn�lni a t�vir�ny�t�t.\n"
#define MSGTR_LIRCopenfailed "Nem tudtam megnyitni a lirc t�mogat�st!\n"
#define MSGTR_LIRCcfgerr "Nem tudom olvasni a LIRC konfigur�ci�s f�jlt: %s \n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "Nem tal�lhat� a k�vetkez� video sz�r�: '%s'.\n"
#define MSGTR_CouldNotOpenVideoFilter "A k�vetkez� video sz�r� megnyit�sa nem siker�lt: '%s'.\n"
#define MSGTR_OpeningVideoFilter "Video sz�r� megnyit�sa: "
#define MSGTR_CannotFindColorspace "Nem tal�lhat� k�z�s colorspace, m�g a 'scale' filterrel sem :(\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDec: a codec nem �ll�totta be az sh->disp_w �s az sh_disp_h iz�ket, megpr�b�lom workaroundolni!\n"
#define MSGTR_VoConfigRequest "VDec: vo config k�r�s - %d x %d (prefer�lt sz�nt�r: %s)\n"
#define MSGTR_CouldNotFindColorspace "Nem tal�lok egyez� colorspace-t - �jra pr�b�lom a -vf scale filterrel...\n"
#define MSGTR_MovieAspectIsSet "A film aspect �rt�ke %.2f:1 - aspect ar�ny jav�t�sa.\n"
#define MSGTR_MovieAspectUndefined "A film aspect �rt�ke nem defini�lt - nincs ar�nyjav�t�s.\n"

// vd_dshow.c, vd_dmo.c
#define MSGTR_DownloadCodecPackage "Friss�tened/install�lnod kell a bin�ris codec csomagot.\nItt megtal�lod: http://mplayerhq.hu/homepage/dload.html\n"
#define MSGTR_DShowInitOK "INFO: Win32/DShow video codec inicializ�l�sa OK.\n"
#define MSGTR_DMOInitOK "INFO: Win32/DMO video codec init OK.\n"

// x11_common.c
#define MSGTR_EwmhFullscreenStateFailed "\nX11: Nem lehet EWMH fullscreen esem�nyt k�ldeni!\n"
#define MSGTR_CouldNotFindXScreenSaver "xscreensaver_disable: Nem tal�lhat� az XScreenSaver ablak.\n"
#define MSGTR_SelectedVideoMode "XF86VM: %dx%d kiv�lasztott vide� m�d a(z) %dx%d k�pm�rethez.\n"

#define MSGTR_InsertingAfVolume "[Mixer] Nincs hardveres kever�s, hanger� sz�r� haszn�lata.\n"
#define MSGTR_NoVolume "[Mixer] Hanger� �ll�t�s nem lehets�ges.\n"

// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "Az MPlayerr�l"
#define MSGTR_FileSelect "F�jl kiv�laszt�sa..."
#define MSGTR_SubtitleSelect "Felirat kiv�laszt�sa..."
#define MSGTR_OtherSelect "F�jl kiv�laszt�sa..."
#define MSGTR_AudioFileSelect "K�ls� audio csatorna v�laszt�sa..."
#define MSGTR_FontSelect "Bet�t�pus kiv�laszt�sa..."
// Megjegyz�s: Ha megv�ltoztatod az MSGTR_PlayList-et, n�zd meg, hogy megfelel-e az MSGTR_MENU_PlayList-nek is!
#define MSGTR_PlayList "Lej�tsz�si lista"
#define MSGTR_Equalizer "Equalizer"
#define MSGTR_SkinBrowser "Skin b�ng�sz�"
#define MSGTR_Network "H�l�zati stream-el�s..."
// Megjegyz�s: Ha megv�ltoztatod az MSGTR_Preferences-t, n�zd meg, hogy megfelel-e az MSGTR_MENU_Preferences-nek is!
#define MSGTR_Preferences "Be�ll�t�sok"
#define MSGTR_AudioPreferences "Audio vez�rl� be�ll�t�sa"
#define MSGTR_NoMediaOpened "nincs megnyitva semmi"
#define MSGTR_VCDTrack "%d. VCD track"
#define MSGTR_NoChapter "nincs megnyitott fejezet"
#define MSGTR_Chapter "%d. fejezet"
#define MSGTR_NoFileLoaded "nincs f�jl bet�ltve"

// --- buttons ---
#define MSGTR_Ok "Ok"
#define MSGTR_Cancel "M�gse"
#define MSGTR_Add "Hozz�ad"
#define MSGTR_Remove "Kivesz"
#define MSGTR_Clear "T�rl�s"
#define MSGTR_Config "Be�ll�t�s"
#define MSGTR_ConfigDriver "Vez�rl� be�ll�t�sa"
#define MSGTR_Browse "Tall�z�s"

// --- error messages ---
#define MSGTR_NEMDB "Nincs el�g mem�ria a buffer kirajzol�s�hoz."
#define MSGTR_NEMFMR "Nincs el�g mem�ria a men� renderel�s�hez."
#define MSGTR_IDFGCVD "Nem talaltam GUI kompatibilis video meghajt�t."
#define MSGTR_NEEDLAVCFAME "Nem MPEG f�jl lej�tsz�sa nem lehets�ges a DXR3/H+ hardverrel �jrak�dol�s n�lk�l.\nKapcsold be a lavc vagy fame opci�t a DXR3/H+ konfigur�ci�s panelen."
#define MSGTR_UNKNOWNWINDOWTYPE "Ismeretlen ablak t�pust tal�ltam ..."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[skin] hiba a skin konfigur�ci�s f�jlj�nak %d. sor�ban: %s"
#define MSGTR_SKIN_WARNING1 "[skin] figyelmeztet�s a skin konfigur�ci�s f�jlj�nak %d. sor�ban: widget (%S) megvan, de nincs el�tte \"section\""
#define MSGTR_SKIN_WARNING2 "[skin] figyelmeztet�s a skin konfigur�ci�s f�jlj�nak %d. sor�ban: widget (%S) megvan, de nincs el�tte \"subsection\""
#define MSGTR_SKIN_WARNING3 "[skin] figyelmeztet�s a skin konfigur�ci�s f�jlj�nak %d. sor�ban: ez az elem nem haszn�lhat� ebben az alr�szben (%s)"
#define MSGTR_SKIN_SkinFileNotFound "[skin] a f�jl ( %s ) nem tal�lhat�.\n"
#define MSGTR_SKIN_SkinFileNotReadable "[skin] f�jl ( %s ) nem olvashat�.\n"
#define MSGTR_SKIN_BITMAP_16bit  "16 vagy kevesebb bites bitmap nem t�mogatott (%s).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "f�jl nem tal�lhat� (%s)\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "BMP olvas�si hiba (%s)\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "TGA olvas�si hiba (%s)\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "PNG olvas�si hiba (%s)\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "RLE t�m�r�tett TGA-k nincsenek t�mogatva (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "ismeretlen tipus� f�jl (%s)\n"
#define MSGTR_SKIN_BITMAP_ConvertError "hiba a 24-r�l 32 bitre konvert�l�s k�zben (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "ismeretlen �zenet: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "nincs el�g mem�ria\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "T�l sok bet�tipus van deklar�lva.\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "Nem tal�lom a bet�tipus f�jlt.\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "Nem tal�lom a bet�tipus k�pf�jlt.\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "neml�tez� bet�tipus azonos�t� (%s)\n"
#define MSGTR_SKIN_UnknownParameter "ismeretlen param�ter (%s)\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "Skin nem tal�lhat� (%s).\n"
#define MSGTR_SKIN_SKINCFG_SelectedSkinNotFound "A kiv�lasztott skin ( %s ) nem tal�lhat�, a 'default'-ot pr�b�lom meg...\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "Skin konfigur�ci�s f�jl olvas�si hiba (%s).\n"
#define MSGTR_SKIN_LABEL "Skin-ek:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "Az MPlayer-r�l"
#define MSGTR_MENU_Open "Megnyit�s..."
#define MSGTR_MENU_PlayFile "F�jl lej�tsz�s..."
#define MSGTR_MENU_PlayVCD "VCD lej�tsz�s..."  
#define MSGTR_MENU_PlayDVD "DVD lej�tsz�s..."  
#define MSGTR_MENU_PlayURL "URL lej�tsz�s..."  
#define MSGTR_MENU_LoadSubtitle "Felirat bet�lt�se..."
#define MSGTR_MENU_DropSubtitle "Felirat eldob�sa..."
#define MSGTR_MENU_LoadExternAudioFile "K�ls� hang bet�lt�se..."
#define MSGTR_MENU_Playing "Lej�tsz�s"
#define MSGTR_MENU_Play "Lej�tsz�s"
#define MSGTR_MENU_Pause "Pillanat�llj"
#define MSGTR_MENU_Stop "�llj"  
#define MSGTR_MENU_NextStream "K�vetkez� f�jl"
#define MSGTR_MENU_PrevStream "El�z� f�jl"
#define MSGTR_MENU_Size "M�ret"
#define MSGTR_MENU_HalfSize   "F�l m�ret"
#define MSGTR_MENU_NormalSize "Norm�l m�ret"
#define MSGTR_MENU_DoubleSize "Dupla m�ret"
#define MSGTR_MENU_FullScreen "Teljesk�perny�" 
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "Lemez megnyit�sa..."
#define MSGTR_MENU_ShowDVDMenu "DVD men�"
#define MSGTR_MENU_Titles "S�vok"
#define MSGTR_MENU_Title "%2d. s�v"
#define MSGTR_MENU_None "(nincs)"
#define MSGTR_MENU_Chapters "Fejezetek"
#define MSGTR_MENU_Chapter "%2d. fejezet"
#define MSGTR_MENU_AudioLanguages "Szinkron nyelvei"
#define MSGTR_MENU_SubtitleLanguages "Feliratok nyelvei"
#define MSGTR_MENU_PlayList MSGTR_PlayList
#define MSGTR_MENU_SkinBrowser "Skin b�ng�sz�"
#define MSGTR_MENU_Preferences MSGTR_Preferences
#define MSGTR_MENU_Exit "Kil�p�s..."
#define MSGTR_MENU_Mute "N�ma"
#define MSGTR_MENU_Original "Eredeti"
#define MSGTR_MENU_AspectRatio "K�par�ny"
#define MSGTR_MENU_AudioTrack "Audio track"
#define MSGTR_MENU_Track "%d. s�v"
#define MSGTR_MENU_VideoTrack "Video track"

// --- equalizer
// Megjegyz�s: Ha megv�ltoztatod az MSGTR_EQU_Audio-t, n�zd meg, hogy megfelel-e az MSGTR_PREFERENCES_Audio-nak is!
#define MSGTR_EQU_Audio "Audi�"
// Megjegyz�s: Ha megv�ltoztatod az MSGTR_EQU_Video-t, n�zd meg, hogy megfelel-e az MSGTR_PREFERENCES_Video-nak is!
#define MSGTR_EQU_Video "Vide�"
#define MSGTR_EQU_Contrast "Kontraszt: "
#define MSGTR_EQU_Brightness "F�nyer�: "
#define MSGTR_EQU_Hue "Szin�rnyalat: "
#define MSGTR_EQU_Saturation "Tel�tetts�g: "
#define MSGTR_EQU_Front_Left "Bal Els�"
#define MSGTR_EQU_Front_Right "Jobb Els�"
#define MSGTR_EQU_Back_Left "Bal H�ts�"
#define MSGTR_EQU_Back_Right "Jobb H�ts�"
#define MSGTR_EQU_Center "K�z�ps�"
#define MSGTR_EQU_Bass "Basszus"
#define MSGTR_EQU_All "Mindegyik"
#define MSGTR_EQU_Channel1 "1. Csatorna:"
#define MSGTR_EQU_Channel2 "2. Csatorna:"
#define MSGTR_EQU_Channel3 "3. Csatorna:"
#define MSGTR_EQU_Channel4 "4. Csatorna:"
#define MSGTR_EQU_Channel5 "5. Csatorna:"
#define MSGTR_EQU_Channel6 "6. Csatorna:"

// --- playlist
#define MSGTR_PLAYLIST_Path "�tvonal"
#define MSGTR_PLAYLIST_Selected "Kiv�lasztott f�jlok"
#define MSGTR_PLAYLIST_Files "F�jlok"
#define MSGTR_PLAYLIST_DirectoryTree "K�nyvt�r lista"

// --- preferences
#define MSGTR_PREFERENCES_Audio MSGTR_EQU_Audio
#define MSGTR_PREFERENCES_Video MSGTR_EQU_Video
#define MSGTR_PREFERENCES_SubtitleOSD "Felirat & OSD"
#define MSGTR_PREFERENCES_Codecs "Kodekek �s demuxerek"
// Megjegyz�s: Ha megv�ltoztatod az MSGTR_PREFERENCES_Misc-et, n�zd meg, hogy megfelel-e az MSGTR_PREFERENCES_FRAME_Misc-nek is!
#define MSGTR_PREFERENCES_Misc "Egy�b"

#define MSGTR_PREFERENCES_None "Egyik sem"
#define MSGTR_PREFERENCES_DriverDefault "alap�rtelmezett vez�rl�"
#define MSGTR_PREFERENCES_AvailableDrivers "Driverek:"
#define MSGTR_PREFERENCES_DoNotPlaySound "Hang n�lk�l"
#define MSGTR_PREFERENCES_NormalizeSound "Hang normaliz�l�sa"
#define MSGTR_PREFERENCES_EnEqualizer "Audio equalizer"
#define MSGTR_PREFERENCES_SoftwareMixer "Szoftveres kever�s"
#define MSGTR_PREFERENCES_ExtraStereo "Extra stereo"
#define MSGTR_PREFERENCES_Coefficient "Egy�tthat�:"
#define MSGTR_PREFERENCES_AudioDelay "Hang k�sleltet�s"
#define MSGTR_PREFERENCES_DoubleBuffer "Dupla bufferel�s"
#define MSGTR_PREFERENCES_DirectRender "Direct rendering"
#define MSGTR_PREFERENCES_FrameDrop "K�p eldob�s"
#define MSGTR_PREFERENCES_HFrameDrop "Er�szakos k�p eldob�"
#define MSGTR_PREFERENCES_Flip "K�p fejjel lefel�"
#define MSGTR_PREFERENCES_Panscan "Panscan: "
#define MSGTR_PREFERENCES_OSDTimer "�ra es indik�torok"
#define MSGTR_PREFERENCES_OSDProgress "Csak a sz�zal�k jelz�k"
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "�ra, sz�zal�k �s a teljes id�"
#define MSGTR_PREFERENCES_Subtitle "Felirat:"
#define MSGTR_PREFERENCES_SUB_Delay "K�sleltet�s: "
#define MSGTR_PREFERENCES_SUB_FPS "FPS:"
#define MSGTR_PREFERENCES_SUB_POS "Poz�ci�ja: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "Felirat automatikus bet�lt�s�nek tilt�sa"
#define MSGTR_PREFERENCES_SUB_Unicode "Unicode felirat"
#define MSGTR_PREFERENCES_SUB_MPSUB "A film felirat�nak konvert�l�sa MPlayer felirat form�tumba"
#define MSGTR_PREFERENCES_SUB_SRT "A film felirat�nak konvert�l�sa SubViewer (SRT) form�tumba"
#define MSGTR_PREFERENCES_SUB_Overlap "Felirat �tlapol�s"
#define MSGTR_PREFERENCES_Font "Bet�k:"
#define MSGTR_PREFERENCES_FontFactor "Bet� egy�tthat�:"
#define MSGTR_PREFERENCES_PostProcess "K�pjav�t�s"
#define MSGTR_PREFERENCES_AutoQuality "Aut�matikus min�s�g �ll�t�s: "
#define MSGTR_PREFERENCES_NI "non-interleaved  AVI  felt�telez�se (hib�s AVI-kn�l seg�thet"
#define MSGTR_PREFERENCES_IDX "Az AVI index�nek �jra�p�t�se, ha sz�ks�ges"
#define MSGTR_PREFERENCES_VideoCodecFamily "Video kodek csal�d:"
#define MSGTR_PREFERENCES_AudioCodecFamily "Audio kodek csal�d:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "OSD szint"
#define MSGTR_PREFERENCES_FRAME_Subtitle "Felirat"
#define MSGTR_PREFERENCES_FRAME_Font "Bet�"
#define MSGTR_PREFERENCES_FRAME_PostProcess "K�pjav�t�s"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "Codec & demuxer"
#define MSGTR_PREFERENCES_FRAME_Cache "Gyors�t�t�r"
#define MSGTR_PREFERENCES_FRAME_Misc MSGTR_PREFERENCES_Misc
#define MSGTR_PREFERENCES_Audio_Device "Eszk�z:"
#define MSGTR_PREFERENCES_Audio_Mixer "Mixer:"
#define MSGTR_PREFERENCES_Audio_MixerChannel "Mixer csatorna:"
#define MSGTR_PREFERENCES_Message "K�rlek eml�kezz, n�h�ny opci� ig�nyli a lej�tsz�s �jraind�t�s�t."
#define MSGTR_PREFERENCES_DXR3_VENC "Video k�dol�:"
#define MSGTR_PREFERENCES_DXR3_LAVC "LAVC haszn�lata (FFmpeg)"
#define MSGTR_PREFERENCES_DXR3_FAME "FAME haszn�lata"
#define MSGTR_PREFERENCES_FontEncoding1 "Unicode"
#define MSGTR_PREFERENCES_FontEncoding2 "Nyugat-Eur�pai karakterk�szlet (ISO-8859-1)"
#define MSGTR_PREFERENCES_FontEncoding3 "Nyugat-Eur�pai karakterk�szlet eur�val (ISO-8859-15)"
#define MSGTR_PREFERENCES_FontEncoding4 "Szl�v �s k�z�p-eur�pai karakterk�szlet (ISO-8859-2)"
#define MSGTR_PREFERENCES_FontEncoding5 "Eszperant�, gall, m�ltai �s t�r�k karakterk�szlet (ISO-8859-3)"
#define MSGTR_PREFERENCES_FontEncoding6 "R�gi balti karakterk�szlet (ISO-8859-4)"
#define MSGTR_PREFERENCES_FontEncoding7 "Cirill karakterk�szlet (ISO-8859-5)"
#define MSGTR_PREFERENCES_FontEncoding8 "Arab karakterk�szlet (ISO-8859-6)"
#define MSGTR_PREFERENCES_FontEncoding9 "Modern g�r�g karakterk�szlet (ISO-8859-7)"
#define MSGTR_PREFERENCES_FontEncoding10 "T�r�k karakterk�szlet (ISO-8859-9)"
#define MSGTR_PREFERENCES_FontEncoding11 "Baltik karakterk�szlet (ISO-8859-13)"
#define MSGTR_PREFERENCES_FontEncoding12 "Kelta karakterk�szlet (ISO-8859-14)"
#define MSGTR_PREFERENCES_FontEncoding13 "H�ber karakterk�szlet (ISO-8859-8)"
#define MSGTR_PREFERENCES_FontEncoding14 "Orosz karakterk�szlet (KOI8-R)"
#define MSGTR_PREFERENCES_FontEncoding15 "Ukr�n, Belorusz karakterk�szlet (KOI8-U/UR)"
#define MSGTR_PREFERENCES_FontEncoding16 "Egyszer� k�nai karakterk�szlet (CP936)"
#define MSGTR_PREFERENCES_FontEncoding17 "Tradicion�lis k�nai karakterk�szlet (BIG5)"
#define MSGTR_PREFERENCES_FontEncoding18 "Jap�n karakterk�szlet (SHIFT-JIS)"
#define MSGTR_PREFERENCES_FontEncoding19 "Koreai karakterk�szlet (CP949)"
#define MSGTR_PREFERENCES_FontEncoding20 "Thai karakterk�szlet (CP874)"
#define MSGTR_PREFERENCES_FontEncoding21 "Cirill karakterk�szlet (Windows) (CP1251)"
#define MSGTR_PREFERENCES_FontEncoding22 "Szl�v �s k�z�p-eur�pai karakterk�szlet (Windows) (CP1250)"
#define MSGTR_PREFERENCES_FontNoAutoScale "Nincs automata karakterm�ret v�laszt�s"
#define MSGTR_PREFERENCES_FontPropWidth "Karakterm�ret film sz�less�g�hez val� �ll�t�sa"
#define MSGTR_PREFERENCES_FontPropHeight "Karakterm�ret film magass�g�hoz val� �ll�t�sa"
#define MSGTR_PREFERENCES_FontPropDiagonal "Karakterm�ret film �tl�j�hoz val� �ll�t�sa"
#define MSGTR_PREFERENCES_FontEncoding "K�dol�s:"
#define MSGTR_PREFERENCES_FontBlur "Blur:"
#define MSGTR_PREFERENCES_FontOutLine "K�rvonal:"
#define MSGTR_PREFERENCES_FontTextScale "Sz�veg sk�la:"
#define MSGTR_PREFERENCES_FontOSDScale "OSD sk�la:"
#define MSGTR_PREFERENCES_Cache "Gyors�t�t�r be/ki"
#define MSGTR_PREFERENCES_CacheSize "Gyors�t�t�r merete:"
#define MSGTR_PREFERENCES_LoadFullscreen "Ind�t�s teljes k�perny�n"
#define MSGTR_PREFERENCES_SaveWinPos "Ablakok poz�ci�j�nak ment�se"
#define MSGTR_PREFERENCES_XSCREENSAVER "XScreenSaver le�ll�t�sa film lej�tsz�sakor"
#define MSGTR_PREFERENCES_PlayBar "PlayBar enged�lyez�se"
#define MSGTR_PREFERENCES_AutoSync "AutoSync ki/be"
#define MSGTR_PREFERENCES_AutoSyncValue "�rt�ke:"
#define MSGTR_PREFERENCES_CDROMDevice "CD meghajt�:"
#define MSGTR_PREFERENCES_DVDDevice "DVD meghajt�:"
#define MSGTR_PREFERENCES_FPS "Film FPS:"
#define MSGTR_PREFERENCES_ShowVideoWindow "Lej�tsz� ablak megjelen�t�se ha inakt�v"
#define MSGTR_PREFERENCES_ArtsBroken "Az �jabb aRts verzi�k inkompatibilisek "\
           "a GTK 1.x-szel �s �sszeomlasztj�k a GMPlayert!"

#define MSGTR_ABOUT_UHU "GUI fejleszt�st az UHU Linux t�mogatta\n"
#define MSGTR_ABOUT_CoreTeam "   MPlayer csapat:\n"
#define MSGTR_ABOUT_AdditionalCoders "   Tov�bbi k�derek:\n"
#define MSGTR_ABOUT_MainTesters "   Teszterek:\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "V�gzetes hiba!"
#define MSGTR_MSGBOX_LABEL_Error "Hiba!"
#define MSGTR_MSGBOX_LABEL_Warning "Figyelmeztet�s!"

// bitmap.c

#define MSGTR_NotEnoughMemoryC32To1 "[c32to1] nincs el�g mem�ria a k�phez\n"
#define MSGTR_NotEnoughMemoryC1To32 "[c1to32] nincs el�g mem�ria a k�phez\n"

// cfg.c

#define MSGTR_ConfigFileReadError "[cfg] hiba a konfigur�ci�s f�jl olvas�sakor ...\n"
#define MSGTR_UnableToSaveOption "[cfg] A(z) '%s' opci�t nem siker�lt elmenteni.\n"

// interface.c

#define MSGTR_DeletingSubtitles "[GUI] Feliratok t�rl�se.\n"
#define MSGTR_LoadingSubtitles "[GUI] Feliratok bet�lt�se: %s\n"
#define MSGTR_AddingVideoFilter "[GUI] Vide� sz�r� hozz�ad�sa: %s\n"
#define MSGTR_RemovingVideoFilter "[GUI] Vide� sz�r� elt�vol�t�sa: %s\n"

// mw.c

#define MSGTR_NotAFile "�gy t�nik, hogy ez nem f�jl: %s !\n"

// ws.c

#define MSGTR_WS_CouldNotOpenDisplay "[ws] A k�perny� nem nyithat� meg.\n"
#define MSGTR_WS_RemoteDisplay "[ws] T�voli k�perny�, XMITSHM kikapcsolva.\n"
#define MSGTR_WS_NoXshm "[ws] Bocs, a rendszered nem t�mogatja az X osztott mem�ria kiterjeszt�st.\n"
#define MSGTR_WS_NoXshape "[ws] Bocs, a rendszered nem t�mogatja az XShape kiterjeszt�st.\n"
#define MSGTR_WS_ColorDepthTooLow "[ws] Bocs, a sz�n m�lys�g t�l kicsi.\n"
#define MSGTR_WS_TooManyOpenWindows "[ws] T�l sok nyitott ablak van.\n"
#define MSGTR_WS_ShmError "[ws] osztott mem�ria kiterjeszt�s hib�ja\n"
#define MSGTR_WS_NotEnoughMemoryDrawBuffer "[ws] Bocs, nincs el�g mem�ria a rajz buffernek.\n"
#define MSGTR_WS_DpmsUnavailable "A DPMS nem el�rhet�?\n"
#define MSGTR_WS_DpmsNotEnabled "A DPMS nem enged�lyezhet�.\n"

// wsxdnd.c

#define MSGTR_WS_NotAFile "�gy t�nik, hogy ez nem f�jl...\n"
#define MSGTR_WS_DDNothing "D&D: Semmi sem j�tt vissza!\n"

#endif

// ======================= VO Video Output drivers ========================

#define MSGTR_VOincompCodec "A kiv�lasztott video_out eszk�z nem kompatibilis ezzel a codec-kel.\n"\
                "Pr�b�ld meg hozz�adni a scale sz�r�t, pl. -vf spp,scale a -vf spp helyett.\n"
#define MSGTR_VO_GenericError "Hiba t�rt�nt"
#define MSGTR_VO_UnableToAccess "Nem el�rhet�"
#define MSGTR_VO_ExistsButNoDirectory "m�r l�tezik, de nem k�nyvt�r."
#define MSGTR_VO_DirExistsButNotWritable "A c�lk�nyvt�r m�r l�tezik, de nem �rhat�."
#define MSGTR_VO_DirExistsAndIsWritable "A c�lk�nyvt�r m�r l�tezik �s �rhat�."
#define MSGTR_VO_CantCreateDirectory "Nem tudtam l�trehozni a c�lk�nyvt�rat."
#define MSGTR_VO_CantCreateFile "A kimeneti f�jl nem hozhat� l�tre."
#define MSGTR_VO_DirectoryCreateSuccess "A c�lk�nyvt�rat sikeresen l�trehoztam."
#define MSGTR_VO_ParsingSuboptions "Alopci�k �rtelmez�se."
#define MSGTR_VO_SuboptionsParsedOK "Alopci�k �rtelmez�se rendben."
#define MSGTR_VO_ValueOutOfRange "�rt�k hat�ron k�v�l"
#define MSGTR_VO_NoValueSpecified "Nincs �rt�k megadva."
#define MSGTR_VO_UnknownSuboptions "Ismeretlen alopci�(k)"

// vo_aa.c

#define MSGTR_VO_AA_HelpHeader "\n\nEzek az aalib vo_aa alopci�i:\n"
#define MSGTR_VO_AA_AdditionalOptions "A vo_aa �ltal biztos�tott opci�k:\n" \
"  help        ki�rja ezt a s�g�t\n" \
"  osdcolor    osd sz�n be�ll�t�sa\n  subcolor    feliratsz�n be�ll�t�sa\n" \
"        a sz�n param�terek:\n           0 : norm�l\n" \
"           1 : dim\n           2 : f�lk�v�r\n           3 : boldfont\n" \
"           4 : ford�tott\n           5 : speci�lis\n\n\n"

// vo_jpeg.c
#define MSGTR_VO_JPEG_ProgressiveJPEG "Progressz�v JPEG enged�lyezve."
#define MSGTR_VO_JPEG_NoProgressiveJPEG "Progressz�v JPEG letiltva."
#define MSGTR_VO_JPEG_BaselineJPEG "Baseline JPEG enged�lyezve."
#define MSGTR_VO_JPEG_NoBaselineJPEG "Baseline JPEG letiltva."

// vo_pnm.c
#define MSGTR_VO_PNM_ASCIIMode "ASCII m�d enged�lyezve."
#define MSGTR_VO_PNM_RawMode "Raw m�d enged�lyezve."
#define MSGTR_VO_PNM_PPMType "PPM f�jlok �r�sa."
#define MSGTR_VO_PNM_PGMType "PGM f�jlok �r�sa."
#define MSGTR_VO_PNM_PGMYUVType "PGMYUV f�jlok �r�sa."

// vo_yuv4mpeg.c
#define MSGTR_VO_YUV4MPEG_InterlacedHeightDivisibleBy4 "Az interlaced m�dhoz n�ggyel oszthat� k�p magass�g sz�ks�ges."
#define MSGTR_VO_YUV4MPEG_InterlacedLineBufAllocFail "Nem siker�lt sor buffert foglalni az interlaced m�dhoz."
#define MSGTR_VO_YUV4MPEG_InterlacedInputNotRGB "Input nem RGB, nem lehet sz�tv�lasztani a sz�neket mez�nk�nt!"
#define MSGTR_VO_YUV4MPEG_WidthDivisibleBy2 "A k�p sz�less�gnek kett�vel oszthat�nak kell lennie."
#define MSGTR_VO_YUV4MPEG_NoMemRGBFrameBuf "Nincs el�g mem�ria az RGB framebuffer lefoglal�s�hoz."
#define MSGTR_VO_YUV4MPEG_OutFileOpenError "Nincs elegend� mem�ria vagy f�jl handle a(z) \"%s\" �r�s�hoz!"
#define MSGTR_VO_YUV4MPEG_OutFileWriteError "Hiba a k�p kimenetre �r�sa k�zben!"
#define MSGTR_VO_YUV4MPEG_UnknownSubDev "Ismeretlen aleszk�z: %s"
#define MSGTR_VO_YUV4MPEG_InterlacedTFFMode "Interlaced kimeneti m�d haszn�lata, top-field el�sz�r."
#define MSGTR_VO_YUV4MPEG_InterlacedBFFMode "Interlaced kimeneti m�d haszn�lata, bottom-field el�sz�r."
#define MSGTR_VO_YUV4MPEG_ProgressiveMode "Progressz�v (alap�rtelmezett) frame m�d haszn�lata."

// sub.c
#define MSGTR_VO_SUB_Seekbar "Keres�s�v"
#define MSGTR_VO_SUB_Play "Lej�tsz�s"
#define MSGTR_VO_SUB_Pause "Pillanat �llj"
#define MSGTR_VO_SUB_Stop "�llj"
#define MSGTR_VO_SUB_Rewind "Vissza"
#define MSGTR_VO_SUB_Forward "El�re"
#define MSGTR_VO_SUB_Clock "�ra"
#define MSGTR_VO_SUB_Contrast "Kontraszt"
#define MSGTR_VO_SUB_Saturation "Tel�tetts�g"
#define MSGTR_VO_SUB_Volume "Hanger�"
#define MSGTR_VO_SUB_Brightness "F�nyer�"
#define MSGTR_VO_SUB_Hue "Sz�n�rnyalat"

// vo_xv.c
#define MSGTR_VO_XV_ImagedimTooHigh "A forr�s k�p m�retei " \
                                    "t�l nagyok: %ux%u (a maximum %ux%u)\n"

// Old vo drivers that have been replaced

#define MSGTR_VO_PGM_HasBeenReplaced "A pgm video kimeneti vez�rl�t lecser�lte a -vo pnm:pgmyuv.\n"
#define MSGTR_VO_MD5_HasBeenReplaced "Az md5 video kimeneti vez�rl�t lecser�lte a -vo md5sum.\n"

// ======================= AO Audio Output drivers ========================

// libao2 

// audio_out.c
#define MSGTR_AO_ALSA9_1x_Removed "audio_out: alsa9 �s alsa1x modulok t�r�lve lettek, haszn�ld a -ao alsa kapcsol�t helyett�k.\n"

// ao_oss.c
#define MSGTR_AO_OSS_CantOpenMixer "[AO OSS] audio_setup: Nem tudom megnyitni a(z) %s kever� eszk�zt: %s\n"
#define MSGTR_AO_OSS_ChanNotFound "[AO OSS] audio_setup: A hangk�rtya kever�j�nek nincs '%s' csatorn�ja, az alap�rtelmezettet haszn�lom.\n"
#define MSGTR_AO_OSS_CantOpenDev "[AO OSS] audio_setup: A(z) %s audio eszk�zt nem tudom megnyitni: %s\n"
#define MSGTR_AO_OSS_CantMakeFd "[AO OSS] audio_setup: Nem lehet f�jl le�r� blokkol�st v�gezni: %s\n"
#define MSGTR_AO_OSS_CantSet "[AO OSS] A(z) %s audio eszk�z nem �ll�that� be %s kimenetre, %s-t pr�b�lok...\n"
#define MSGTR_AO_OSS_CantSetChans "[AO OSS] audio_setup: Nem siker�lt az audio eszk�zt %d csatorn�ra �ll�tani.\n"
#define MSGTR_AO_OSS_CantUseGetospace "[AO OSS] audio_setup: a vez�rl� nem t�mogatja a SNDCTL_DSP_GETOSPACE-t :-(\n"
#define MSGTR_AO_OSS_CantUseSelect "[AO OSS]\n   ***  Az audio vez�rl�d NEM t�mogatja a select() -et ***\n Ford�tsd �jra az MPlayer-t az #undef HAVE_AUDIO_SELECT sorral a config.h-ban!\n\n"
#define MSGTR_AO_OSS_CantReopen "[AO OSS]\nV�gzetes hiba: *** NEM LEHET �JRA MEGNYITNI / BE�LL�TANI AZ AUDIO ESZK�ZT *** %s\n"

// ao_arts.c
#define MSGTR_AO_ARTS_CantInit "[AO ARTS] %s\n"
#define MSGTR_AO_ARTS_ServerConnect "[AO ARTS] Csatlakoztam a hang szerverhez.\n"
#define MSGTR_AO_ARTS_CantOpenStream "[AO ARTS] Nem lehet megnyitni a folyamot.\n"
#define MSGTR_AO_ARTS_StreamOpen "[AO ARTS] Folyam megnyitva.\n"
#define MSGTR_AO_ARTS_BufferSize "[AO ARTS] buffer m�rete: %d\n"

// ao_dxr2.c
#define MSGTR_AO_DXR2_SetVolFailed "[AO DXR2] Hanger� be�ll�t�sa %d-re sikertelen.\n"
#define MSGTR_AO_DXR2_UnsupSamplerate "[AO DXR2] dxr2: %d Hz nem t�mogatott, pr�b�ld meg ezt: \"-aop list=resample\"\n"

// ao_esd.c
#define MSGTR_AO_ESD_CantOpenSound "[AO ESD] esd_open_sound sikertelen: %s\n"
#define MSGTR_AO_ESD_LatencyInfo "[AO ESD] latency: [szerver: %0.2fs, net: %0.2fs] (igaz�t�s %0.2fs)\n"
#define MSGTR_AO_ESD_CantOpenPBStream "[AO ESD] nem siker�lt megnyitni az esd playback folyamot: %s\n"

// ao_mpegpes.c
#define MSGTR_AO_MPEGPES_CantSetMixer "[AO MPEGPES] DVB audio kever� be�ll�t�sa sikertelen: %s\n" 
#define MSGTR_AO_MPEGPES_UnsupSamplerate "[AO MPEGPES] %d Hz nem t�mogatott, megpr�b�lom �jrak�dolni...\n"

// ao_null.c
// This one desn't even  have any mp_msg nor printf's?? [CHECK]

// ao_pcm.c
#define MSGTR_AO_PCM_FileInfo "[AO PCM] F�jl: %s (%s)\nPCM: Samplerate: %iHz Csatorna: %s Form�tum: %s\n"
#define MSGTR_AO_PCM_HintInfo "[AO PCM] Info: Gyorsabb dump-ol�s a -vc null -vo null kapcsol�val �rhet� el\nPCM: Info: WAVE f�jlok �r�s�hoz haszn�ld a -ao pcm:waveheader kapcsol�t (alap�rtelmezett)!\n"
#define MSGTR_AO_PCM_CantOpenOutputFile "[AO PCM] %s megnyit�sa �r�sra nem siker�lt!\n"

// ao_sdl.c
#define MSGTR_AO_SDL_INFO "[AO SDL] Samplerate: %iHz Csatorn�k: %s Form�tum: %s\n"
#define MSGTR_AO_SDL_DriverInfo "[AO SDL] %s audio vez�rl� haszn�lata.\n"
#define MSGTR_AO_SDL_UnsupportedAudioFmt "[AO SDL] Nem t�mogatott audio form�tum: 0x%x.\n"
#define MSGTR_AO_SDL_CantInit "[AO SDL] SDL Audio inicializ�l�sa nem siker�lt: %s\n"
#define MSGTR_AO_SDL_CantOpenAudio "[AO SDL] audio megnyit�sa nem siker�lt: %s\n"

// ao_sgi.c
#define MSGTR_AO_SGI_INFO "[AO SGI] vez�rl�s.\n"
#define MSGTR_AO_SGI_InitInfo "[AO SGI] init: Samplerate: %iHz Csatorna: %s Form�tum: %s\n"
#define MSGTR_AO_SGI_InvalidDevice "[AO SGI] lej�tsz�s: hib�s eszk�z.\n"
#define MSGTR_AO_SGI_CantSetParms_Samplerate "[AO SGI] init: setparams sikertelen: %s\nNem siker�lt be�ll�tani az el��rt samplerate-et.\n"
#define MSGTR_AO_SGI_CantSetAlRate "[AO SGI] init: AL_RATE-et nem fogadta el a kiv�lasztott er�forr�s.\n"
#define MSGTR_AO_SGI_CantGetParms "[AO SGI] init: getparams sikertelen: %s\n"
#define MSGTR_AO_SGI_SampleRateInfo "[AO SGI] init: samplerate most m�r %lf (el��rt r�ta: %lf)\n"
#define MSGTR_AO_SGI_InitConfigError "[AO SGI] init: %s\n"
#define MSGTR_AO_SGI_InitOpenAudioFailed "[AO SGI] init: Nem tudom megnyitni az audio csatorn�t: %s\n"
#define MSGTR_AO_SGI_Uninit "[AO SGI] uninit: ...\n"
#define MSGTR_AO_SGI_Reset "[AO SGI] reset: ...\n"
#define MSGTR_AO_SGI_PauseInfo "[AO SGI] audio_pause: ...\n"
#define MSGTR_AO_SGI_ResumeInfo "[AO SGI] audio_resume: ...\n"

// ao_sun.c
#define MSGTR_AO_SUN_RtscSetinfoFailed "[AO SUN] rtsc: SETINFO sikertelen.\n"
#define MSGTR_AO_SUN_RtscWriteFailed "[AO SUN] rtsc: �r�s sikertelen."
#define MSGTR_AO_SUN_CantOpenAudioDev "[AO SUN] %s audio eszk�z nem el�rhet�, %s  -> nincs hang.\n"
#define MSGTR_AO_SUN_UnsupSampleRate "[AO SUN] audio_setup: a k�rty�d nem t�mogat %d csatorn�t, %s, %d Hz samplerate-t.\n"
#define MSGTR_AO_SUN_CantUseSelect "[AO SUN]\n   ***  A hangk�rty�d NEM t�mogatja a select()-et ***\nFord�tsd �jra az MPlayer-t az #undef HAVE_AUDIO_SELECT sorral a config.h-ban !\n\n"
#define MSGTR_AO_SUN_CantReopenReset "[AO SUN]\nV�gzetes hiba: *** NEM LEHET �JRA MEGNYITNI / BE�LL�TANI AZ AUDIO ESZK�ZT (%s) ***\n"

// ao_alsa5.c
#define MSGTR_AO_ALSA5_InitInfo "[AO ALSA5] alsa-init: k�rt form�tum: %d Hz, %d csatorna, %s\n"
#define MSGTR_AO_ALSA5_SoundCardNotFound "[AO ALSA5] alsa-init: nem tal�ltam hangk�rty�t.\n"
#define MSGTR_AO_ALSA5_InvalidFormatReq "[AO ALSA5] alsa-init: hib�s form�tumot (%s) k�rt�l - kimenet letiltva.\n"
#define MSGTR_AO_ALSA5_PlayBackError "[AO ALSA5] alsa-init: playback megnyit�si hiba: %s\n"
#define MSGTR_AO_ALSA5_PcmInfoError "[AO ALSA5] alsa-init: pcm info hiba: %s\n"
#define MSGTR_AO_ALSA5_SoundcardsFound "[AO ALSA5] alsa-init: %d hangk�rty�t tal�ltam, ezt haszn�lom: %s\n"
#define MSGTR_AO_ALSA5_PcmChanInfoError "[AO ALSA5] alsa-init: pcm csatorna info hiba: %s\n"
#define MSGTR_AO_ALSA5_CantSetParms "[AO ALSA5] alsa-init: hiba a param�terek be�ll�t�sakor: %s\n"
#define MSGTR_AO_ALSA5_CantSetChan "[AO ALSA5] alsa-init: hiba a csatorna be�ll�t�sakor: %s\n"
#define MSGTR_AO_ALSA5_ChanPrepareError "[AO ALSA5] alsa-init: csatorna el�k�sz�t�si hiba: %s\n"
#define MSGTR_AO_ALSA5_DrainError "[AO ALSA5] alsa-uninit: lej�tsz�s drain hiba: %s\n"
#define MSGTR_AO_ALSA5_FlushError "[AO ALSA5] alsa-uninit: lej�tsz�s �r�t�si hiba: %s\n"
#define MSGTR_AO_ALSA5_PcmCloseError "[AO ALSA5] alsa-uninit: pcm lez�r�si hiba: %s\n"
#define MSGTR_AO_ALSA5_ResetDrainError "[AO ALSA5] alsa-reset: lej�tsz�s drain hiba: %s\n"
#define MSGTR_AO_ALSA5_ResetFlushError "[AO ALSA5] alsa-reset: lej�tsz�s �r�t�si hiba: %s\n"
#define MSGTR_AO_ALSA5_ResetChanPrepareError "[AO ALSA5] alsa-reset: csatorna el�k�sz�t�si hiba: %s\n"
#define MSGTR_AO_ALSA5_PauseDrainError "[AO ALSA5] alsa-pause: lej�tsz�s drain hiba: %s\n"
#define MSGTR_AO_ALSA5_PauseFlushError "[AO ALSA5] alsa-pause: lej�tsz�s �r�t�si hiba: %s\n"
#define MSGTR_AO_ALSA5_ResumePrepareError "[AO ALSA5] alsa-resume: csatorna el�k�sz�t�si hiba: %s\n"
#define MSGTR_AO_ALSA5_Underrun "[AO ALSA5] alsa-play: alsa underrun, folyam be�ll�t�sa.\n"
#define MSGTR_AO_ALSA5_PlaybackPrepareError "[AO ALSA5] alsa-play: lej�tsz�s el�k�sz�t�si hiba: %s\n"
#define MSGTR_AO_ALSA5_WriteErrorAfterReset "[AO ALSA5] alsa-play: �r�si hiba a be�ll�t�s ut�n: %s - feladom.\n"
#define MSGTR_AO_ALSA5_OutPutError "[AO ALSA5] alsa-play: kimeneti hiba: %s\n"

// ao_plugin.c

#define MSGTR_AO_PLUGIN_InvalidPlugin "[AO PLUGIN] hib�s plugin: %s\n"

// ======================= AF Audio Filters ================================

// libaf 

// af_ladspa.c

#define MSGTR_AF_LADSPA_AvailableLabels "haszn�lhat� cimk�k"
#define MSGTR_AF_LADSPA_WarnNoInputs "FIGYELEM! Ennek a LADSPA pluginnak nincsenek audio bemenetei.\n  A bej�v� audi� jelek elvesznek."
#define MSGTR_AF_LADSPA_ErrMultiChannel "A t�bb-csatorn�s (>2) plugin (m�g) nem t�mogatott.\n  Csak a mono �s sztereo plugin-okat haszn�ld."
#define MSGTR_AF_LADSPA_ErrNoOutputs "Ennek a LADSPA pluginnak nincsenek audi� bemenetei."
#define MSGTR_AF_LADSPA_ErrInOutDiff "K�l�nb�zik a LADSPA plugin audi� bemenetek �s kimenetek sz�ma."
#define MSGTR_AF_LADSPA_ErrFailedToLoad "nem siker�lt bet�lteni"
#define MSGTR_AF_LADSPA_ErrNoDescriptor "A ladspa_descriptor() f�ggv�ny nem tal�lhat� a megadott f�ggv�nyk�nyvt�r f�jlban."
#define MSGTR_AF_LADSPA_ErrLabelNotFound "A c�mke nem tal�lhat� a plugin k�nyvt�rban."
#define MSGTR_AF_LADSPA_ErrNoSuboptions "Nincs alopci� megadva"
#define MSGTR_AF_LADSPA_ErrNoLibFile "Nincs k�nyvt�rf�jl megadva"
#define MSGTR_AF_LADSPA_ErrNoLabel "Nincs sz�r� c�mke megadva"
#define MSGTR_AF_LADSPA_ErrNotEnoughControls "Nincs el�g vez�rl� megadva a parancssorban"
#define MSGTR_AF_LADSPA_ErrControlBelow "%s: A(z) #%d bemeneti vez�rl� a(z) %0.4f als� hat�r alatt van.\n"
#define MSGTR_AF_LADSPA_ErrControlAbove "%s: A(z) #%d bemeneti vez�rl� a(z) %0.4f fels� hat�r felett van.\n"

// ========================== INPUT =========================================

// joystick.c

#define MSGTR_INPUT_JOYSTICK_Opening "Botkorm�ny eszk�z megnyit�sa: %s\n"
#define MSGTR_INPUT_JOYSTICK_CantOpen "Nem siker�lt a(z) %s botkorm�ny eszk�zt megnyitni: %s\n"
#define MSGTR_INPUT_JOYSTICK_ErrReading "Hiba a botkorm�ny eszk�z olvas�sa k�zben: %s\n"
#define MSGTR_INPUT_JOYSTICK_LoosingBytes "Botkorm�ny: elvesztett�nk %d b�jtnyi adatot\n"
#define MSGTR_INPUT_JOYSTICK_WarnLostSync "Botkorm�ny: figyelmeztet� init esem�ny, elvesztett�k a szinkront a vez�rl�vel\n"
#define MSGTR_INPUT_JOYSTICK_WarnUnknownEvent "Botkorm�ny ismeretlen figyelmeztet� esem�ny t�pus: %d\n"

// input.c

#define MSGTR_INPUT_INPUT_ErrCantRegister2ManyCmdFds "T�l sok parancs f�jl le�r�, nem siker�lt a(z) %d f�jl le�r� regiszt�l�sa.\n"
#define MSGTR_INPUT_INPUT_ErrCantRegister2ManyKeyFds "T�l sok gomb f�jl le�r�, nem siker�lt a(z) %d f�jl le�r� regiszt�l�sa.\n"
#define MSGTR_INPUT_INPUT_ErrArgMustBeInt "%s parancs: %d argumentum nem eg�sz.\n"
#define MSGTR_INPUT_INPUT_ErrArgMustBeFloat "%s parancs: %d argumentum nem lebeg�pontos.\n"
#define MSGTR_INPUT_INPUT_ErrUnterminatedArg "%s parancs: %d argumentum lez�ratlan.\n"
#define MSGTR_INPUT_INPUT_ErrUnknownArg "Ismeretlen argumentum: %d\n"
#define MSGTR_INPUT_INPUT_Err2FewArgs "A(z) %s parancsnak legal�bb %d argumentum kell, de csak %d-t tal�ltunk eddig.\n"
#define MSGTR_INPUT_INPUT_ErrReadingCmdFd "Hiba a(z) %d parancs f�jl le�r� olvas�sa k�zben: %s\n"
#define MSGTR_INPUT_INPUT_ErrCmdBufferFullDroppingContent "%d parancs f�jl le�r� buffere tele van: tartalom eldob�sa\n"
#define MSGTR_INPUT_INPUT_ErrInvalidCommandForKey "Hib�s parancs a(z) %s gombn�l"
#define MSGTR_INPUT_INPUT_ErrSelect "Kiv�laszt�si hiba: %s\n"
#define MSGTR_INPUT_INPUT_ErrOnKeyInFd "Hiba a(z) %d gomb input f�jl le�r�j�ban\n"
#define MSGTR_INPUT_INPUT_ErrDeadKeyOnFd "Halott gomb input a(z) %d f�jl le�r�n�l\n"
#define MSGTR_INPUT_INPUT_Err2ManyKeyDowns "T�l sok gomblenyom�si esem�ny egy id�ben\n"
#define MSGTR_INPUT_INPUT_ErrOnCmdFd "Hiba a(z) %d parancs f�jlle�r�ban\n"
#define MSGTR_INPUT_INPUT_ErrReadingInputConfig "Hiba a(z) %s input konfigur�ci�s f�jl olvas�sa k�zben: %s\n"
#define MSGTR_INPUT_INPUT_ErrUnknownKey "Ismeretlen gomb '%s'\n"
#define MSGTR_INPUT_INPUT_ErrUnfinishedBinding "Nem befejezett hozz�rendel�s: %s\n"
#define MSGTR_INPUT_INPUT_ErrBuffer2SmallForKeyName "A buffer t�l kicsi ehhez a gomb n�vhez: %s\n"
#define MSGTR_INPUT_INPUT_ErrNoCmdForKey "A(z) %s gombhoz nem tal�lhat� parancs"
#define MSGTR_INPUT_INPUT_ErrBuffer2SmallForCmd "A buffer t�l kicsi a(z) %s parancshoz\n"
#define MSGTR_INPUT_INPUT_ErrWhyHere "Mit keres�nk mi itt?\n"
#define MSGTR_INPUT_INPUT_ErrCantInitJoystick "A bemeneti borkorm�ny inicializ�l�sa nem siker�lt\n"
#define MSGTR_INPUT_INPUT_ErrCantStatFile "Nem stat-olhat� %s: %s\n"
#define MSGTR_INPUT_INPUT_ErrCantOpenFile "Nem nyithat� meg %s: %s\n"

// ========================== LIBMPDEMUX ===================================

// url.c

#define MSGTR_MPDEMUX_URL_StringAlreadyEscaped "a karakterl�nc m�r escape-ltnek t�nik az url_escape-ben %c%c1%c2\n"

// ai_alsa1x.c

#define MSGTR_MPDEMUX_AIALSA1X_CannotSetSamplerate "Nem �ll�that� be a mintav�teli r�ta\n"
#define MSGTR_MPDEMUX_AIALSA1X_CannotSetBufferTime "Nem �ll�that� be a buffer id�\n"
#define MSGTR_MPDEMUX_AIALSA1X_CannotSetPeriodTime "Nem �ll�that� be a peri�dus id�\n"

// ai_alsa1x.c / ai_alsa.c

#define MSGTR_MPDEMUX_AIALSA_PcmBrokenConfig "Hib�s konfigur�ci� ehhez a PCM-hez: nincs el�rhet� konfigur�ci�\n"
#define MSGTR_MPDEMUX_AIALSA_UnavailableAccessType "El�r�si t�pus nem haszn�lhat�\n"
#define MSGTR_MPDEMUX_AIALSA_UnavailableSampleFmt "Minta form�tum nem el�rhet�\n"
#define MSGTR_MPDEMUX_AIALSA_UnavailableChanCount "Csatorna sz�ml�l� nem el�rhet� - visszat�r�s az alap�rtelmezetthez: %d\n"
#define MSGTR_MPDEMUX_AIALSA_CannotInstallHWParams "Sikertelen a hw param�terek be�ll�t�sa:"
#define MSGTR_MPDEMUX_AIALSA_PeriodEqualsBufferSize "Nem haszn�lhat� a buffer m�rettel egyez� peri�dus (%u == %lu)\n"
#define MSGTR_MPDEMUX_AIALSA_CannotInstallSWParams "Sikertelen az sw param�terek be�ll�t�sa:\n"
#define MSGTR_MPDEMUX_AIALSA_ErrorOpeningAudio "Hiba az audi� megnyit�sakor: %s\n"
#define MSGTR_MPDEMUX_AIALSA_AlsaStatusError "ALSA st�tusz hiba: %s"
#define MSGTR_MPDEMUX_AIALSA_AlsaXRUN "ALSA xrun!!! (legal�bb %.3f ms hosszan)\n"
#define MSGTR_MPDEMUX_AIALSA_AlsaStatus "ALSA St�tusz:\n"
#define MSGTR_MPDEMUX_AIALSA_AlsaXRUNPrepareError "ALSA xrun: el�k�sz�t�si hiba: %s"
#define MSGTR_MPDEMUX_AIALSA_AlsaReadWriteError "ALSA olvas�s/�r�s hiba"

// ai_oss.c

#define MSGTR_MPDEMUX_AIOSS_Unable2SetChanCount "Sikertelen a csatorna sz�ml�l� be�ll�t�sa: %d\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2SetStereo "Sikertelen a sztere� be�ll�t�sa: %d\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2Open "'%s' nem nyithat� meg: %s\n"
#define MSGTR_MPDEMUX_AIOSS_UnsupportedFmt "nem t�mogatott form�tum\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2SetAudioFmt "Az audi� form�tum nem �ll�that� be."
#define MSGTR_MPDEMUX_AIOSS_Unable2SetSamplerate "A mintav�teli r�ta nem �ll�that� be: %d\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2SetTrigger "A trigger nem �ll�that� be: %d\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2GetBlockSize "Nem siker�lt lek�rdezni a blokkm�retet!\n"
#define MSGTR_MPDEMUX_AIOSS_AudioBlockSizeZero "az audi� blokk m�ret nulla, be�ll�tva: %d!\n"
#define MSGTR_MPDEMUX_AIOSS_AudioBlockSize2Low "az audi� blokk m�ret t�l alacsony, be�ll�tva: %d!\n"

// asfheader.c

#define MSGTR_MPDEMUX_ASFHDR_HeaderSizeOver1MB "V�GZETES HIBA: fejl�c m�ret nagyobb, mint 1 MB (%d)!\nKeresd meg az MPlayer k�sz�t�it �s t�ltsd fel/k�ldd el ezt a f�jlt.\n"
#define MSGTR_MPDEMUX_ASFHDR_HeaderMallocFailed "Nem siker�lt %d b�jt lefoglal�sa a fejl�chez\n"
#define MSGTR_MPDEMUX_ASFHDR_EOFWhileReadingHeader "EOF az asf fejl�c olvas�sa k�zben, hib�s/nem teljes f�jl?\n"
#define MSGTR_MPDEMUX_ASFHDR_DVRWantsLibavformat "A DVR val�sz�n�leg csak libavformat-tal m�k�dik, pr�b�ld ki a -demuxer 35 -�t probl�ma eset�n\n"
#define MSGTR_MPDEMUX_ASFHDR_NoDataChunkAfterHeader "Nincs adat r�sz a fejl�c ut�n!\n"
#define MSGTR_MPDEMUX_ASFHDR_AudioVideoHeaderNotFound "ASF: nem tal�lhat� audi� vagy vide� fejl�c - hib�s f�jl?\n"
#define MSGTR_MPDEMUX_ASFHDR_InvalidLengthInASFHeader "Hib�s hossz az ASF fejl�cben!\n"

// asf_mmst_streaming.c

#define MSGTR_MPDEMUX_MMST_WriteError "�r�si hiba\n"
#define MSGTR_MPDEMUX_MMST_EOFAlert "\nriad�! eof\n"
#define MSGTR_MPDEMUX_MMST_PreHeaderReadFailed "el�-fejl�c olvas�s sikertelen\n"
#define MSGTR_MPDEMUX_MMST_InvalidHeaderSize "Hib�s fejl�c m�ret, feladom\n"
#define MSGTR_MPDEMUX_MMST_HeaderDataReadFailed "fejl�c adat olvas�si hiba\n"
#define MSGTR_MPDEMUX_MMST_packet_lenReadFailed "packet_len olvas�si hiba\n"
#define MSGTR_MPDEMUX_MMST_InvalidRTSPPacketSize "Hib�s rtsp csomag m�ret, feladom\n"
#define MSGTR_MPDEMUX_MMST_CmdDataReadFailed "parancs adat olvas�si hiba\n"
#define MSGTR_MPDEMUX_MMST_HeaderObject "fejl�c objektum\n"
#define MSGTR_MPDEMUX_MMST_DataObject "adat objektum\n"
#define MSGTR_MPDEMUX_MMST_FileObjectPacketLen "f�jl objektum, csomag m�ret = %d (%d)\n"
#define MSGTR_MPDEMUX_MMST_StreamObjectStreamID "folyam objektum, folyam id: %d\n"
#define MSGTR_MPDEMUX_MMST_2ManyStreamID "t�l sok id, a folyam figyelmen k�v�l hagyva"
#define MSGTR_MPDEMUX_MMST_UnknownObject "ismeretlen objektum\n"
#define MSGTR_MPDEMUX_MMST_MediaDataReadFailed "m�dia adat olvas�si hiba\n"
#define MSGTR_MPDEMUX_MMST_MissingSignature "hi�nyz� al��r�s\n"
#define MSGTR_MPDEMUX_MMST_PatentedTechnologyJoke "Minden k�sz. K�sz�nj�k, hogy szabadalmazott technol�gi�t alkalmaz� m�di�t t�lt�tt�l le.\n"
#define MSGTR_MPDEMUX_MMST_UnknownCmd "ismeretlen parancs %02x\n"
#define MSGTR_MPDEMUX_MMST_GetMediaPacketErr "get_media_packet hiba : %s\n"
#define MSGTR_MPDEMUX_MMST_Connected "csatlakozva\n"

// asf_streaming.c

#define MSGTR_MPDEMUX_ASF_StreamChunkSize2Small "Ahhhh, stream_chunck m�ret t�l kicsi: %d\n"
#define MSGTR_MPDEMUX_ASF_SizeConfirmMismatch "size_confirm hib�s!: %d %d\n"
#define MSGTR_MPDEMUX_ASF_WarnDropHeader "Figyelmeztet�s : fejl�c eldobva ????\n"
#define MSGTR_MPDEMUX_ASF_ErrorParsingChunkHeader "Hiba a fejl�c chunk �rtelmez�sekor\n"
#define MSGTR_MPDEMUX_ASF_NoHeaderAtFirstChunk "Ne a fejl�c legyen az els� chunk !!!!\n"
#define MSGTR_MPDEMUX_ASF_BufferMallocFailed "Hiba, nem lehet allok�lni a(z) %d b�jtos buffert\n"
#define MSGTR_MPDEMUX_ASF_ErrReadingNetworkStream "Hiba a h�l�zati folyam olvas�sa k�zben\n"
#define MSGTR_MPDEMUX_ASF_ErrChunk2Small "Hiba, a chunk t�l kicsi\n"
#define MSGTR_MPDEMUX_ASF_ErrSubChunkNumberInvalid "Hiba, az al-chunk-ok sz�ma hib�s\n"
#define MSGTR_MPDEMUX_ASF_Bandwidth2SmallCannotPlay "Kicsi a s�vsz�less�g, a f�jl nem lej�tszhat�!\n"
#define MSGTR_MPDEMUX_ASF_Bandwidth2SmallDeselectedAudio "A s�vsz�less�g t�l kicsi, audi� folyam kikapcsolva\n"
#define MSGTR_MPDEMUX_ASF_Bandwidth2SmallDeselectedVideo "A s�vsz�less�g t�l kicsi, vide� folyam kikapcsolva\n"
#define MSGTR_MPDEMUX_ASF_InvalidLenInHeader "Hib�s hossz az ASF fejl�cben!\n"
#define MSGTR_MPDEMUX_ASF_ErrReadingChunkHeader "Hiba a fejl�c chunk olvas�sakor\n"
#define MSGTR_MPDEMUX_ASF_ErrChunkBiggerThanPacket "Hiba: chunk_size > packet_size\n"
#define MSGTR_MPDEMUX_ASF_ErrReadingChunk "Hiba a chunk olvas�sa k�zben\n"
#define MSGTR_MPDEMUX_ASF_ASFRedirector "=====> ASF Redirector\n"
#define MSGTR_MPDEMUX_ASF_InvalidProxyURL "Hib�s proxy URL\n"
#define MSGTR_MPDEMUX_ASF_UnknownASFStreamType "Ismeretlen asf folyam t�pus\n"
#define MSGTR_MPDEMUX_ASF_Failed2ParseHTTPResponse "Sikertelen a HTTP v�lasz �rtelmez�se\n"
#define MSGTR_MPDEMUX_ASF_ServerReturn "Szerver v�lasz %d:%s\n"
#define MSGTR_MPDEMUX_ASF_ASFHTTPParseWarnCuttedPragma "ASF HTTP �RTELMEZ�SI HIBA : %s pragma elv�gva %d b�jtt�l %d b�jtig\n"
#define MSGTR_MPDEMUX_ASF_SocketWriteError "Socket �r�si hiba : %s\n"
#define MSGTR_MPDEMUX_ASF_HeaderParseFailed "Sikertelen a f�jl�c �rtelmez�se\n"
#define MSGTR_MPDEMUX_ASF_NoStreamFound "Nem tal�lhat� folyam\n"
#define MSGTR_MPDEMUX_ASF_UnknownASFStreamingType "Ismeretlen ASF folyam t�pus\n"
#define MSGTR_MPDEMUX_ASF_InfoStreamASFURL "STREAM_ASF, URL: %s\n"
#define MSGTR_MPDEMUX_ASF_StreamingFailed "sikertelen, kil�p�s\n"

// audio_in.c

#define MSGTR_MPDEMUX_AUDIOIN_ErrReadingAudio "\nhiba az audi� olvas�sakor: %s\n"
#define MSGTR_MPDEMUX_AUDIOIN_XRUNSomeFramesMayBeLeftOut "Visszat�r�s a cross-run-b�l, n�h�ny k�pkocka kimaradhatott!\n"
#define MSGTR_MPDEMUX_AUDIOIN_ErrFatalCannotRecover "V�gzetes hiba, nem lehet visszat�rni!\n"
#define MSGTR_MPDEMUX_AUDIOIN_NotEnoughSamples "\nnincs el�g audi� minta!\n"

// aviheader.c

#define MSGTR_MPDEMUX_AVIHDR_EmptyList "** �res lista?!\n"
#define MSGTR_MPDEMUX_AVIHDR_FoundMovieAt "Film megtal�lva: 0x%X - 0x%X\n"
#define MSGTR_MPDEMUX_AVIHDR_FoundBitmapInfoHeader "'bih' megtal�lva, %u b�jt %d b�jtb�l\n"
#define MSGTR_MPDEMUX_AVIHDR_RegeneratingKeyfTableForMPG4V1 "A kulcs k�pkocka t�bla �jragener�lva az M$ mpg4v1 vide�hoz\n"
#define MSGTR_MPDEMUX_AVIHDR_RegeneratingKeyfTableForDIVX3 "Kulcs k�pkocka t�bla �jragener�l�sa a DIVX3 vide�hoz\n"
#define MSGTR_MPDEMUX_AVIHDR_RegeneratingKeyfTableForMPEG4 "Kulcs k�pkocka t�bla �jragener�l�sa az MPEG4 vide�hoz\n"
#define MSGTR_MPDEMUX_AVIHDR_FoundWaveFmt "'wf' megtal�lva, %d b�jt %d b�jtb�l\n"
#define MSGTR_MPDEMUX_AVIHDR_FoundAVIV2Header "AVI: dmlh megtal�lva (size=%d) (total_frames=%d)\n"
#define MSGTR_MPDEMUX_AVIHDR_ReadingIndexBlockChunksForFrames  "INDEX blokk olvas�sa, %d chunks %ld k�pkock�hoz (fpos=%p)\n"
#define MSGTR_MPDEMUX_AVIHDR_AdditionalRIFFHdr "kieg�sz�t� RIFF fejl�c...\n"
#define MSGTR_MPDEMUX_AVIHDR_WarnNotExtendedAVIHdr "** figyelmeztet�s: ez nem kiterjesztett AVI fejl�c..\n"
#define MSGTR_MPDEMUX_AVIHDR_BrokenChunk "Hib�s chunk?  chunksize=%d  (id=%.4s)\n"
#define MSGTR_MPDEMUX_AVIHDR_BuildingODMLidx "AVI: ODML: odml index fel�p�t�se (%d superindexchunks)\n"
#define MSGTR_MPDEMUX_AVIHDR_BrokenODMLfile "AVI: ODML: Hib�s (nem teljes?) f�jlt tal�ltam. Trad�cion�lis index haszn�lata\n"
#define MSGTR_MPDEMUX_AVIHDR_CantReadIdxFile "A(z) %s index f�jl nem olvashat�: %s\n"
#define MSGTR_MPDEMUX_AVIHDR_NotValidMPidxFile "%s nem �rv�nyes MPlayer index f�jl\n"
#define MSGTR_MPDEMUX_AVIHDR_FailedMallocForIdxFile "Nem lehet mem�ri�t foglalni az index adatoknak %s-b�l\n"
#define MSGTR_MPDEMUX_AVIHDR_PrematureEOF "Korai index f�jlv�g %s f�jlban\n"
#define MSGTR_MPDEMUX_AVIHDR_IdxFileLoaded "Bet�lt�tt index f�jl: %s\n"
#define MSGTR_MPDEMUX_AVIHDR_GeneratingIdx "Index gener�l�sa: %3lu %s     \r"
#define MSGTR_MPDEMUX_AVIHDR_IdxGeneratedForHowManyChunks "AVI: Index t�bla legener�lva %d chunk-hoz!\n"
#define MSGTR_MPDEMUX_AVIHDR_Failed2WriteIdxFile "Nem siker�lt a(z) %s index f�jl �r�sa: %s\n"
#define MSGTR_MPDEMUX_AVIHDR_IdxFileSaved "Elmentett index f�jl: %s\n"

// cache2.c

#define MSGTR_MPDEMUX_CACHE2_NonCacheableStream "\rEz a folyam nem cache-elhet�.\n"
#define MSGTR_MPDEMUX_CACHE2_ReadFileposDiffers "!!! read_filepos k�l�nb�zik!!! jelentsd ezt a hib�t...\n"

// cdda.c

#define MSGTR_MPDEMUX_CDDA_CantOpenCDDADevice "Nem nyithat� meg a CDDA eszk�z.\n"
#define MSGTR_MPDEMUX_CDDA_CantOpenDisc "Nem nyithat� meg a lemez.\n"
#define MSGTR_MPDEMUX_CDDA_AudioCDFoundWithNTracks "Audi� CD-t tal�ltam %d s�vval.\n"

// cddb.c

#define MSGTR_MPDEMUX_CDDB_FailedToReadTOC "Hiba a TOC olvas�sa k�zben.\n"
#define MSGTR_MPDEMUX_CDDB_FailedToOpenDevice "Hiba a(z) %s eszk�z megnyit�sakor.\n"
#define MSGTR_MPDEMUX_CDDB_NotAValidURL "Hib�s URL\n"
#define MSGTR_MPDEMUX_CDDB_FailedToSendHTTPRequest "HTTP k�r�s elk�ld�se nem siker�lt.\n"
#define MSGTR_MPDEMUX_CDDB_FailedToReadHTTPResponse "HTTP v�lasz olvas�sa nem siker�lt.\n"
#define MSGTR_MPDEMUX_CDDB_HTTPErrorNOTFOUND "Nem tal�lhat�.\n"
#define MSGTR_MPDEMUX_CDDB_HTTPErrorUnknown "Ismeretlen hibak�d\n"
#define MSGTR_MPDEMUX_CDDB_NoCacheFound "Nem tal�ltam cache-t.\n"
#define MSGTR_MPDEMUX_CDDB_NotAllXMCDFileHasBeenRead "Nem minden xmcd f�jl lett elolvasva.\n"
#define MSGTR_MPDEMUX_CDDB_FailedToCreateDirectory "Sikertelen a(z) %s k�nyvt�r l�trehoz�sa.\n"
#define MSGTR_MPDEMUX_CDDB_NotAllXMCDFileHasBeenWritten "Nem minden xmcd f�jl lett ki�rva.\n"
#define MSGTR_MPDEMUX_CDDB_InvalidXMCDDatabaseReturned "Hib�s xmcd adatb�zis f�jl �rkezett vissza.\n"
#define MSGTR_MPDEMUX_CDDB_UnexpectedFIXME "Nem v�rt FIXME\n"
#define MSGTR_MPDEMUX_CDDB_UnhandledCode "Kezeletlen k�d\n"
#define MSGTR_MPDEMUX_CDDB_UnableToFindEOL "Nem tal�lhat� a sor v�ge\n"
#define MSGTR_MPDEMUX_CDDB_ParseOKFoundAlbumTitle "�rtelmez�s OK, tal�ltam: %s\n"
#define MSGTR_MPDEMUX_CDDB_AlbumNotFound "Album nem tal�lhat�\n"
#define MSGTR_MPDEMUX_CDDB_ServerReturnsCommandSyntaxErr "Szerver v�lasza: Parancs szintaxis hib�s\n"
#define MSGTR_MPDEMUX_CDDB_NoSitesInfoAvailable "Nincs el�rhet� oldal inform�ci�\n"
#define MSGTR_MPDEMUX_CDDB_FailedToGetProtocolLevel "Sikertelen a protokol szint lek�rdez�se\n"
#define MSGTR_MPDEMUX_CDDB_NoCDInDrive "Nincs CD a meghajt�ban\n"

// cue_read.c

#define MSGTR_MPDEMUX_CUEREAD_UnexpectedCuefileLine "[bincue] Nem v�rt cuef�jl sor: %s\n"
#define MSGTR_MPDEMUX_CUEREAD_BinFilenameTested "[bincue] tesztelt bin f�jln�v: %s\n"
#define MSGTR_MPDEMUX_CUEREAD_CannotFindBinFile "[bincue] Nem tal�lhat� a bin f�jl - feladom\n"
#define MSGTR_MPDEMUX_CUEREAD_UsingBinFile "[bincue] %s bin f�jl haszn�lata\n"
#define MSGTR_MPDEMUX_CUEREAD_UnknownModeForBinfile "[bincue] ismeretlen m�d a binf�jlhoz. Nem kellene megt�rt�nnie. Megszak�t�s.\n"
#define MSGTR_MPDEMUX_CUEREAD_CannotOpenCueFile "[bincue] %s nem nyithat� meg\n"
#define MSGTR_MPDEMUX_CUEREAD_ErrReadingFromCueFile "[bincue] Hiba %s f�jlb�l t�rt�n� olvas�skor\n"
#define MSGTR_MPDEMUX_CUEREAD_ErrGettingBinFileSize "[bincue] Hiba a bin f�jl m�ret�nek lek�rdez�sekor\n"
#define MSGTR_MPDEMUX_CUEREAD_InfoTrackFormat "s�v %02d:  form�tum=%d  %02d:%02d:%02d\n"
#define MSGTR_MPDEMUX_CUEREAD_UnexpectedBinFileEOF "[bincue] nem v�rt bin f�jl v�ge\n"
#define MSGTR_MPDEMUX_CUEREAD_CannotReadNBytesOfPayload "[bincue] Nem olvashat� %d b�jtnyi payload\n"
#define MSGTR_MPDEMUX_CUEREAD_CueStreamInfo_FilenameTrackTracksavail "CUE stream_open, f�jln�v=%s, s�v=%d, el�rhet� s�vok: %d -> %d\n"

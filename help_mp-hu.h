// Translated by:  Gabucino <gabucino@mplayerhq.hu>

// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char* banner_text=
"\n\n"
"MPlayer " VERSION "(C) 2000-2002  Gere�ffy �rp�d  (l�sd DOCS!)\n"
"\n";

static char help_text[]=
#ifdef HAVE_NEW_GUI
"Ind�t�s:   mplayer [-gui] [opci�k] [�tvonal/]filen�v\n"
#else
"Ind�t�s:   mplayer [opci�k] [�tvonal/]filen�v\n"
#endif
"\n"
"Opci�k:\n"
" -vo <drv[:dev]> videomeghajt� �s -alegys�g kiv�laszt�sa (lista: '-vo help')\n"
" -ao <drv[:dev]> audiomeghajt� �s -alegys�g kiv�laszt�sa (lista: '-ao help')\n"
" -vcd <s�vsz�m>  lej�tsz�s VCD (video cd)-s�vb�l, k�zvetlen�l az eszk�zr�l\n"
#ifdef HAVE_LIBCSS
" -dvdauth <megh> DVD-meghajt� el�r�si �tj�nak megad�sa (k�dolt lemezekhez)\n"
#endif
#ifdef USE_DVDREAD
" -dvd <titleno>  a megadott DVD s�v lej�tsz�sa, file helyett\n"
#endif
" -ss <id�poz>    a megadott (m�sodperc v. �ra:perc:mperc) poz�ci�ra teker�s\n"
" -nosound        hanglej�tsz�s kikapcsol�sa\n"
#ifdef USE_FAKE_MONO
" -stereo <m�d>   MPEG1 sztere� szab�lyoz�sa (0:sztere�, 1:bal, 2:jobb)\n"
#endif
" -fs -vm -zoom   teljesk�perny�s lej�tsz�s opci�i (teljk�p,m�dv�lt,szoft.nagy)\n"
" -x <x> -y <y>   k�p nagy�t�sa <x> * <y> m�ret�re [ha -vo <meghajt�> t�mogatja]\n"
" -sub <file>     felhaszn�land� felirat-file megad�sa (l�sd -subfps, -subdelay)\n"
" -vid x -aid y   lej�tszand� video- (x) �s audio- (y) stream-ek kiv�laszt�sa\n"
" -fps x -srate y video (x k�pkocka/mp) �s audio (y Hz) r�ta megad�sa\n"
" -pp <min�s�g>   ut�kezel�si fokozatok be�ll�t�sa (0-63)\n"
" -nobps          alternat�v A/V szinkron m�dszer�nek kiv�laszt�sa\n"
" -framedrop      k�pkock�k eldob�s�nak enged�lyez�se (lass� g�pekhez)\n"
"\n"
"Billenty�k:\n"
" <-  vagy  ->    10 m�sodperces h�tra/el�re ugr�s\n"
" fel vagy le     1 percnyi h�tra/el�re ugr�s\n"
" pgup v. pgdown  10 percnyi h�tra/el�re ugr�s\n"
" p vagy SPACE    pillanat�llj (b�rmely billenty�re tov�bbmegy)\n"
" q vagy ESC      kil�p�s\n"
" + vagy -        audio k�sleltet�se +/- 0.1 m�sodperccel\n"
" o               OSD-m�d v�lt�sa:  nincs / keres�s�v / keres�s�v+id�\n"
" * vagy /        hanger� fel/le ('m' billenty� master/pcm k�z�tt v�lt)\n"
" z vagy x        felirat k�sleltet�se +/- 0.1 m�sodperccel\n"
"\n"
" * * * A�MANPAGE�TOV�BBI�R�SZLETEKET, OPCI�KAT, BILLENTY�KET TARTALMAZ ! * * *\n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// mplayer.c: 

#define MSGTR_Exiting "\nKil�pek... (%s)\n"
#define MSGTR_Exit_frames "K�rt sz�m� k�pkocka lej�tsz�sra ker�lt"
#define MSGTR_Exit_quit "Kil�p�s"
#define MSGTR_Exit_eof "V�ge a file-nak"
#define MSGTR_Exit_error "V�gzetes hiba"
#define MSGTR_IntBySignal "\nAz MPlayer fut�sa a %s modulban kapott %d szign�l miatt megszakadt \n"
#define MSGTR_NoHomeDir "Nem tal�lom a HOME konyvt�rat\n"
#define MSGTR_GetpathProblem "get_path(\"config\") probl�ma\n"
#define MSGTR_CreatingCfgFile "Konfigur�ci�s file l�trehoz�sa: %s\n"
#define MSGTR_InvalidVOdriver "Nem l�tez� video drivern�v: %s\nHaszn�ld a '-vo help' opci�t, hogy list�t kapj a haszn�lhato vo meghajt�kr�l.\n"
#define MSGTR_InvalidAOdriver "Nem l�tez� audio drivern�v: %s\nHaszn�ld az '-ao help' opci�t, hogy list�t kapj a haszn�lhato ao meghajt�kr�l.\n"
#define MSGTR_CopyCodecsConf "(m�sold/linkeld az etc/codecs.conf file-t ~/.mplayer/codecs.conf-ba)\n"
#define MSGTR_CantLoadFont "Nem tudom bet�lteni a k�vetkez� fontot: %s\n"
#define MSGTR_CantLoadSub "Nem tudom bet�lteni a feliratot: %s\n"
#define MSGTR_ErrorDVDkey "Hiba a DVD-KULCS feldolgoz�sa k�zben.\n"
#define MSGTR_CmdlineDVDkey "A parancssorban megadott DVD-kulcs tov�bbi dek�dol�s c�lj�b�l elt�rol�sra ker�lt.\n"
#define MSGTR_DVDauthOk "DVD-autentik�ci�s folyamat, �gy t�nik, sikerrel v�gz�d�tt.\n"
#define MSGTR_DumpSelectedSteramMissing "dump: V�GZETES�HIBA: a k�rt stream nem tal�lhat�!\n"
#define MSGTR_CantOpenDumpfile "Nem tudom megnyitni a dump file-t!\n"
#define MSGTR_CoreDumped "Kinyomattam a cuccost, j�l.\n"
#define MSGTR_FPSnotspecified "Az FPS (k�pkocka/mp) �rt�k nincs megadva, vagy hib�s! Haszn�ld az -fps opci�t!\n"
#define MSGTR_NoVideoStream "Ebben nincs video stream... egyel�re lej�tszhatatlan\n"
#define MSGTR_TryForceAudioFmt "Megpr�b�lom a(z) %d audio codec-csal�dot haszn�lni ...\n"
#define MSGTR_CantFindAfmtFallback "A megadott audio codec-csal�dban nem tal�ltam ideval� meghajt�t, pr�b�lkozok m�s meghajt�val.\n"
#define MSGTR_CantFindAudioCodec "Nem tal�lok codecet a(z) 0x%X audio-form�tumhoz !\n"
#define MSGTR_TryUpgradeCodecsConfOrRTFM "*** Friss�tsd a %s-t az etc/codecs.conf-b�l\n*** Ha m�g mindig nem j�, olvasd el a DOCS/CODECS-et!\n"
#define MSGTR_CouldntInitAudioCodec "Nem tudom ind�tani az audio codecet! -> nincshang ;)\n"
#define MSGTR_TryForceVideoFmt "Megpr�b�lom a(z) %d video codec-csal�dot haszn�lni ...\n"
#define MSGTR_CantFindVfmtFallback "A megadott video codec-csal�dban nem tal�ltam ideval� meghajt�t, pr�b�lkozok m�s meghajt�val.\n"
#define MSGTR_CantFindVideoCodec "Nem tal�lok codec-et ami megfelel a kivalasztott vo-hoz es 0x%X video-form�tumhoz !\n"
#define MSGTR_VOincompCodec "A kiv�lasztott video_out meghajt� inkompatibilis ezzel a codec-kel.\n"
#define MSGTR_CouldntInitVideoCodec "V�GZETES�HIBA: Nem siker�lt a video codecet elind�tani :(\n"
#define MSGTR_EncodeFileExists "A %s file m�r l�tezik (nehogy let�r�ld a kedvenc AVI-dat!)\n"
#define MSGTR_CantCreateEncodeFile "Nem tudom enk�dol�s c�lj�b�l l�trehozni a filet\n"
#define MSGTR_CannotInitVO "V�GZETES�HIBA:�Nem tudom elind�tani a video-meghajt�t!\n"
#define MSGTR_CannotInitAO "nem tudom megnyitni az audio-egys�get -> NOSOUND\n"
#define MSGTR_StartPlaying "Lej�tsz�s ind�t�sa...\n"
#define MSGTR_SystemTooSlow "\n\n"\
"         ***************************************\n"\
"         **** A rendszered t�l LASS� ehhez! ****\n"\
"         ***************************************\n"\
"!!! Lehets�ges okok, �s megold�saik: \n"\
"- Legyakrabban : hib�s _audio_ meghajt�. Workaround: pr�b�ld az -ao sdl\n"\
"  opci�t, vagy 0.5-�s ALSA-t, vagy ALSA 0.9-et oss emul�ci�val.\n"\
"  Tov�bbi info a DOCS/sound.html file-ban!\n"\
"- Lass� video kimenet. Pr�b�lj m�sik -vo meghajt�t (lista: -vo help) vagy\n"\
"  a -framedrop opci�t ! Sebess�gn�vel� tippekhez l�sd DOCS/video.html.\n"\
"- Lass� CPU. F�l�sleges gyenge CPU-n DVD-t vagy nagy DivX-et lej�tszani.\n"\
"  Tal�n -hardframedrop opci�val.\n"\
"- Hib�s file. A k�vetkez�k kombin�ci�jaival prob�lkozz: -nobps -ni -mc 0\n"\
"  -forceidx\n"\
"Ha egyik se m�xik, olvasd el a DOCS/bugreports.html file-t !\n\n"

#define MSGTR_NoGui "Az MPlayer grafikus fel�let N�LK�L lett ford�tva!\n"
#define MSGTR_GuiNeedsX "Az MPlayer grafikus fel�let�nek X11-re van sz�ks�ge!\n"
#define MSGTR_Playing "%s lej�tsz�sa\n"
#define MSGTR_NoSound "Audio: nincs hang!!!\n"
#define MSGTR_FPSforced "FPS k�nyszer�tve %5.3f  (ftime: %5.3f)\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "A CD-ROM meghajt� (%s) nem tal�lhat�!\n"
#define MSGTR_ErrTrackSelect "Hiba a VCD-s�v kiv�laszt�sakor!"
#define MSGTR_ReadSTDIN "Olvas�s a szabv�nyos bemenetr�l (stdin)...\n"
#define MSGTR_UnableOpenURL "Nem megnyithat� az URL: %s\n"
#define MSGTR_ConnToServer "Csatlakozom a szerverhez: %s\n"
#define MSGTR_FileNotFound "A file nem tal�lhat�: '%s'\n"

#define MSGTR_CantOpenDVD "Nem tudom megnyitni a DVD eszk�zt: %s\n"
#define MSGTR_DVDwait "A lemez strukt�r�j�nak olvas�sa, k�rlek v�rj...\n"
#define MSGTR_DVDnumTitles "%d s�v van a DVD-n.\n"
#define MSGTR_DVDinvalidTitle "Helytelen DVD s�v: %d\n"
#define MSGTR_DVDnumChapters "Az adott DVD s�vban %d fejezet van.\n"
#define MSGTR_DVDinvalidChapter "Helytelen DVD fejezet: %d\n"
#define MSGTR_DVDnumAngles "%d darab kamera�ll�s van ezen a DVD s�von.\n"
#define MSGTR_DVDinvalidAngle "Helytelen DVD kamera�ll�s: %d\n"
#define MSGTR_DVDnoIFO "Nem tudom a(z) %d. DVD s�vhoz megnyitni az IFO file-t.\n"
#define MSGTR_DVDnoVOBs "Nem tudom megnyitni a s�vot (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD sikeresen megnyitva!\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "Vigy�zat! T�bbsz�r�sen defini�lt Audio-folyam! (Hib�s file?)\n"
#define MSGTR_VideoStreamRedefined "Vigy�zat! T�bbsz�r�sen defini�lt Video-folyam! (Hib�s file?)\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: T�l sok (%d db, %d b�jt) audio-csomag a pufferben!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: T�l sok (%d db, %d b�jt) video-csomag a pufferben!\n"
#define MSGTR_MaybeNI "(tal�n ez egy nem �sszef�s�lt file vagy a CODEC nem m�k�dik j�l)\n"
#define MSGTR_DetectedFILMfile "Ez egy FILM form�tum� file!\n"
#define MSGTR_DetectedFLIfile "Ez egy FLI form�tum� file!\n"
#define MSGTR_DetectedROQfile "Ez egy RoQ form�tum� file!\n"
#define MSGTR_DetectedREALfile "Ez egy REAL form�tum� file!\n"
#define MSGTR_DetectedAVIfile "Ez egy AVI form�tum� file!\n"
#define MSGTR_DetectedASFfile "Ez egy ASF form�tum� file!\n"
#define MSGTR_DetectedMPEGPESfile "Ez egy MPEG-PES form�tum� file!\n"
#define MSGTR_DetectedMPEGPSfile "Ez egy MPEG-PS form�tum� file!\n"
#define MSGTR_DetectedMPEGESfile "Ez egy MPEG-ES form�tum� file!\n"
#define MSGTR_DetectedQTMOVfile "Ez egy QuickTime/MOV form�tum� file! (ez m�g nem t�mogatott)\n"
#define MSGTR_MissingMpegVideo "Nincs MPEG video-folyam? L�pj kapcsolatba a k�sz�t�kkel, lehet, hogy hiba!\n"
#define MSGTR_InvalidMPEGES "Hib�s MPEG-ES-folyam? L�pj kapcsolatba a k�sz�t�kkel, lehet, hogy hiba!\n"
#define MSGTR_FormatNotRecognized "========= Sajnos ez a fileform�tum ismeretlen vagy nem t�mogatott ===========\n"\
				  "= Ha ez egy AVI, ASF vagy MPEG file, l�pj kapcsolatba a k�sz�t�kkel (hiba)! =\n"
#define MSGTR_MissingVideoStream "Nincs k�pfolyam!\n"
#define MSGTR_MissingAudioStream "Nincs hangfolyam... -> hang n�lk�l\n"
#define MSGTR_MissingVideoStreamBug "Nincs k�pfolyam?! �rj a szerz�nek, lehet hogy hiba :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: a file nem tartalmazza a k�rt hang vagy k�p folyamot\n"

#define MSGTR_NI_Forced "K�nyszer�tve"
#define MSGTR_NI_Detected "Detekt�lva"
#define MSGTR_NI_Message "%s NON-INTERLEAVED AVI form�tum!\n"

#define MSGTR_UsingNINI "NON-INTERLEAVED hib�s AVI form�tum haszn�lata!\n"
#define MSGTR_CouldntDetFNo "Nem tudom meghat�rozni a k�pkock�k sz�m�t (abszolut teker�shez)   \n"
#define MSGTR_CantSeekRawAVI "Nem tudok nyers .AVI-kban tekerni! (index kell, pr�b�ld az -idx kapcsol�val!)\n"
#define MSGTR_CantSeekFile "Nem tudok ebben a fileban tekerni!  \n"

#define MSGTR_EncryptedVOB "K�dolt VOB file (libcss t�mogat�s nincs beford�tva!) Olvasd el a doksit\n"
#define MSGTR_EncryptedVOBauth "K�dolt folyam, de nem k�rt�l autentik�l�st!!\n"

#define MSGTR_MOVcomprhdr "MOV: T�m�r�tett fejl�cek (m�g) nincsenek t�mogatva!\n"
#define MSGTR_MOVvariableFourCC "MOV: Vigy�zat! v�ltoz� FOURCC detekt�lva!?\n"
#define MSGTR_MOVtooManyTrk "MOV: Vigy�zat! t�l sok s�v!"
#define MSGTR_MOVnotyetsupp "\n****** Quicktime MOV form�tum m�g nincs t�mogatva!!!!!! *******\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "nem tudom megnyitni a kodeket\n"
#define MSGTR_CantCloseCodec "nem tudom lez�rni a kodeket\n"

#define MSGTR_MissingDLLcodec "HIBA: Nem tudom megnyitni a k�rt DirectShow kodeket: %s\n"
#define MSGTR_ACMiniterror "Nem tudom bet�lteni/inicializ�lni a Win32/ACM kodeket (hi�nyz� DLL file?)\n"
#define MSGTR_MissingLAVCcodec "Nem tal�lom a(z) '%s' nev� kodeket a libavcodec-ben...\n"

#define MSGTR_NoDShowSupport "Az MPlayer DirectShow t�mogat�s N�LK�L lett ford�tva!\n"
#define MSGTR_NoWfvSupport "A win32-es kodekek t�mogat�sa ki van kapcsolva, vagy nem l�tezik nem-x86-on!\n"
#define MSGTR_NoDivx4Support "Az MPlayer DivX4Linux t�mogat�s (libdivxdecore.so) N�LK�L lett ford�tva!\n"
#define MSGTR_NoLAVCsupport "Az MPlayer ffmpeg/libavcodec t�mogat�s N�LK�L lett ford�tva!\n"
#define MSGTR_NoACMSupport "Win32/ACM hang kodek t�mogat�s ki van kapcsolva, vagy nem l�tezik nem-x86 CPU-n -> hang kikapcsolva :(\n"
#define MSGTR_NoDShowAudio "DirectShow t�mogat�s nincs leford�tva -> hang kikapcsolva :(\n"
#define MSGTR_NoOggVorbis "OggVorbis hang kodek kikapcsolva -> hang kikapcsolva :(\n"

#define MSGTR_MpegPPhint "FIGYELEM! K�pjav�t�st k�rt�l egy MPEG1/2 filmre, de az MPlayer-t\n" \
                         "          MPEG1/2 jav�t�si t�mogat�s n�lk�l ford�tottad!\n" \
                         "          #define MPEG12_POSTPROC a config.h-ba, �s ford�tsd �jra libmpeg2-t!\n"
#define MSGTR_MpegNoSequHdr "MPEG: V�GZETES: v�ge lett a filenak mik�zben a szekvencia fejl�cet kerestem\n"
#define MSGTR_CannotReadMpegSequHdr "V�GZETES: Nem tudom olvasni a szekvencia fejl�cet!\n"
#define MSGTR_CannotReadMpegSequHdrEx "V�GZETES: Nem tudom olvasni a szekvencia fejl�c kiterjeszt�s�t!\n"
#define MSGTR_BadMpegSequHdr "MPEG: Hib�s szekvencia fejl�c!\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: Hib�s szekvencia fejl�c kiterjeszt�s!\n"

#define MSGTR_ShMemAllocFail "Nem tudok megosztott mem�ri�t lefoglalni\n"
#define MSGTR_CantAllocAudioBuf "Nem tudok kimeneti hangbuffer lefoglalni\n"
#define MSGTR_NoMemForDecodedImage "nincs el�g mem�ria a dek�dolt k�phez (%ld b�jt)\n"

#define MSGTR_AC3notvalid "AC3 folyam hib�s.\n"
#define MSGTR_AC3only48k "Csak 48000 Hz-es folyamok vannak t�mogatva.\n"
#define MSGTR_UnknownAudio "Ismeretlen/hi�nyz� hangform�tum, hang kikapcsolva\n"

// LIRC:
#define MSGTR_SettingUpLIRC "lirc t�mogat�s ind�t�sa...\n"
#define MSGTR_LIRCdisabled "Nem fogod tudni haszn�lni a t�vir�ny�t�t\n"
#define MSGTR_LIRCopenfailed "Nem tudtam megnyitni a lirc t�mogat�st!\n"
#define MSGTR_LIRCsocketerr "Valami baj van a lirc socket-tel: %s\n"
#define MSGTR_LIRCcfgerr "Nem tudom olvasni a LIRC konfigur�ci�s file-t : %s \n"

//  ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "A GUI-r�l"
#define MSGTR_FileSelect "File kiv�laszt�sa ..."
#define MSGTR_SubtitleSelect "Felirat kiv�laszt�sa ..."
#define MSGTR_OtherSelect "File kiv�laszt�sa ..."
#define MSGTR_MessageBox "�zenetablak"
#define MSGTR_PlayList "Lej�tsz�si lista"
#define MSGTR_SkinBrowser "Skin b�ng�sz�"

// --- buttons ---
#define MSGTR_Ok "Ok"
#define MSGTR_Cancel "M�gse"
#define MSGTR_Add "Hozz�ad"
#define MSGTR_Remove "Kivesz"

// --- error messages ---
#define MSGTR_NEMDB "Nincs el�g mem�ria a buffer kirajzol�s�hoz."
#define MSGTR_NEMFMR "Nincs el�g mem�ria a men� renderel�s�hez."
#define MSGTR_NEMFMM "Nincs el�g mem�ria a f�ablak alakj�nak maszkol�s�hoz."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[skin] hiba a skin konfigur�ci�s file-j�nak %d. sor�ban: %s"
#define MSGTR_SKIN_WARNING1 "[skin] figyelmeztet�s a skin konfigur�ci�s file-j�nak %d. sor�ban: widget megvan, de nincs el�tte \"section\" ( %s )"
#define MSGTR_SKIN_WARNING2 "[skin] figyelmeztet�s a skin konfigur�ci�s file-j�nak %d. sor�ban: widget megvan, de nincs el�tte \"subsection\" ( %s )"
#define MSGTR_SKIN_BITMAP_16bit  "16 vagy kevesebb bites bitmap nem t�mogatott ( %s ).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "file nem tal�lhat� ( %s )\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "bmp olvas�si hiba ( %s )\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "tga olvas�si hiba ( %s )\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "png olvas�si hiba ( %s )\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "RLE t�m�r�tett tga-k nincsenek t�mogatva ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "ismeretlen tipus� file ( %s )\n"
#define MSGTR_SKIN_BITMAP_ConvertError "hiba a 24-r�l 32bitre konvert�l�s k�zben ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "ismeretlen �zenet: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "nincs el�g mem�ria\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "t�l sok bet�tipus van deklar�lva\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "nem tal�lom a bet�tipus file-t\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "nem tal�lom a bet�tipus k�pfile-t"
#define MSGTR_SKIN_FONT_NonExistentFontID "neml�tez� bet�tipus azonos�t� ( %s )\n"
#define MSGTR_SKIN_UnknownParameter "ismeretlen param�ter ( %s )\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[skinb�ng�sz�] nincs el�g mem�ria.\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "Skin nem tal�lhat� ( %s ).\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "Skin configfile olvas�si hiba ( %s ).\n"
#define MSGTR_SKIN_LABEL "Skin-ek:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "Az MPlayer-r�l"
#define MSGTR_MENU_Open "Megnyit�s ..."
#define MSGTR_MENU_PlayFile "File lej�tsz�s ..."
#define MSGTR_MENU_PlayVCD "VCD lej�tsz�s ..."  
#define MSGTR_MENU_PlayDVD "DVD lej�tsz�s ..."  
#define MSGTR_MENU_PlayURL "URL lej�tsz�s ..."  
#define MSGTR_MENU_LoadSubtitle "Felirat bet�lt�se ..."
#define MSGTR_MENU_Playing "Lej�tsz�s"
#define MSGTR_MENU_Play "Lej�tsz�s"
#define MSGTR_MENU_Pause "Pillanat�llj"
#define MSGTR_MENU_Stop "�llj"  
#define MSGTR_MENU_NextStream "K�vetkez� file"
#define MSGTR_MENU_PrevStream "El�z� file"
#define MSGTR_MENU_Size "M�ret"
#define MSGTR_MENU_NormalSize "Norm�l m�ret"
#define MSGTR_MENU_DoubleSize "Dupla m�ret"
#define MSGTR_MENU_FullScreen "Teljesk�perny�" 
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_PlayDisc "Lemez lej�tsz�sa ..."
#define MSGTR_MENU_ShowDVDMenu "DVD men�"
#define MSGTR_MENU_Titles "S�vok"
#define MSGTR_MENU_Title "S�v %2d"
#define MSGTR_MENU_None "(nincs)"
#define MSGTR_MENU_Chapters "Fejezetek"
#define MSGTR_MENU_Chapter "Fejezet %2d"
#define MSGTR_MENU_AudioLanguages "Szinkron nyelvei"
#define MSGTR_MENU_SubtitleLanguages "Feliratok nyelvei"
#define MSGTR_MENU_PlayList "Playlist"
#define MSGTR_MENU_SkinBrowser "Skin b�ng�sz�"
#define MSGTR_MENU_Preferences "Be�ll�t�sok" 
#define MSGTR_MENU_Exit "Kil�p�s ..."

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "v�gzetes hiba ..."
#define MSGTR_MSGBOX_LABEL_Error "hiba ..."
#define MSGTR_MSGBOX_LABEL_Warning "figyelmeztet�s ..."

#endif

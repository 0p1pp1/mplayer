#ifdef HELP_MP_DEFINE_STATIC
static char* banner_text=
"\n\n"
"MPlayer " VERSION "(C) 2000-2001  Gere�ffy �rp�d  (l�sd DOCS/AUTHORS)\n"
"\n";

static char help_text[]=
"Ind�t�s:   mplayer [opci�k] [�tvonal/]filen�v\n"
"\n"
"Opci�k:\n"
" -vo <drv[:dev]> video meghajt� �s alegys�g kiv�laszt�sa (lista: '-vo help')\n"
" -ao <drv[:dev]> audio meghajt� �s alegys�g kiv�laszt�sa (lista: '-ao help')\n"
" -vcd <s�vsz�m>  lej�tsz�s VCD (video cd) s�vb�l, k�zvetlen�l az eszk�zr�l\n"
#ifdef HAVE_LIBCSS
" -dvdauth <megh> DVD meghajt� el�r�si utj�nak megad�sa (k�dolt lemezekhez)\n"
#endif
" -ss <id�poz>   a megadott (m�sodperc v. �ra:perc:mperc) poz�ci�ra teker�s\n"
" -nosound        hanglej�tsz�s kikapcsol�sa\n"
#ifdef USE_FAKE_MONO
" -stereo         MPEG1 sztere� szab�lyoz�s (0:sztere� 1:bal 2:jobb)\n"
#endif
" -fs -vm -zoom   teljesk�perny�s lej�tsz�s opci�i (teljk�p,m�dv�lt,szoft.nagy)\n"
" -x <x> -y <y>   k�p nagy�t�sa <x> * <y> m�ret�re [ha -vo meghajt� t�mogatja!]\n"
" -sub <file>     felhaszn�land� felirat file megad�sa (l�sd -subfps, -subdelay)\n"
" -vid x -aid y   lej�tszand� video (x) �s audio (y) streamek sz�mai\n"
" -fps x -srate y video (x k�pkocka/mp) �s audio (y Hz) r�ta megad�sa\n"
" -pp <min�s�g>   ut�kezel�si fokozatok be�ll�t�sa (0-63)\n"
" -bps            alternat�v A/V szinkron m�dszer kiv�laszt�sa\n"
" -framedrop      frame-eldob�s bekapcsol�sa (lass� g�pekhez)\n"
"\n"
"Billenty�k:\n"
" <-  vagy  ->    10 m�sodperces h�tra/el�re ugr�s\n"
" fel vagy le     1 percnyi h�tra/el�re ugr�s\n"
" pgup v. pgdown  10 percnyi h�tra/el�re ugr�s\n"
" p vagy SPACE    pillanat�llj (b�rmely billenty�re tov�bbmegy)\n"
" q vagy ESC      kil�p�s\n"
" + vagy -        audio k�sleltet�se +/- 0.1 m�sodperccel\n"
" o               OSD m�d v�lt�sa:  nincs / keres�s�v / keres�s�v+id�\n"
" * vagy /        hanger� fel/le ('m' billenty� master/pcm k�z�tt v�lt)\n"
" z vagy x        felirat k�sleltet�se +/- 0.1 m�sodperccel\n"
"\n"
" * * * A�MANPAGE�TOV�BBI�R�SZLETEKET, OPCI�KAT, BILLENTY�KET TARTALMAZ ! * * *\n"
"\n";
#endif

// mplayer.c: 

#define MSGTR_Exiting "\nKil�pek... (%s)\n"
#define MSGTR_Exit_frames "K�rt sz�m� k�pkocka lej�tsz�sra ker�lt"
#define MSGTR_Exit_quit "Kil�p�s"
#define MSGTR_Exit_eof "V�ge a file-nak"
#define MSGTR_IntBySignal "\nAz MPlayer fut�sa a %s modulban kapott %d szign�l miatt megszakadt \n"
#define MSGTR_NoHomeDir "Nem tal�lom a HOME konyvt�rat\n"
#define MSGTR_GetpathProblem "get_path(\"config\") probl�ma\n"
#define MSGTR_CreatingCfgFile "Konfigur�ci�s file l�trehoz�sa: %s\n"
#define MSGTR_InvalidVOdriver "Nem l�tez� video driver n�v: %s\nHaszn�ld a '-vo help' opci�t hogy list�t kapj a haszn�lhato vo meghajt�kr�l.\n"
#define MSGTR_InvalidAOdriver "Nem l�tez� audio driver n�v: %s\nHaszn�ld az '-ao help' opci�t hogy list�t kapj a haszn�lhato ao meghajt�kr�l.\n"
#define MSGTR_CopyCodecsConf "(m�sold/linkeld a DOCS/codecs.conf filet ~/.mplayer/codecs.conf-ba)\n"
#define MSGTR_CantLoadFont "Nem tudom bet�lteni a k�vetkez� fontot: %s\n"
#define MSGTR_CantLoadSub "Nem tudom bet�lteni a feliratot: %s\n"
#define MSGTR_ErrorDVDkey "Hiba a DVD KULCS feldolgoz�sa k�zben.\n"
#define MSGTR_CmdlineDVDkey "A parancssorban megadott DVD�kulcs tov�bbi dek�dol�s c�lj�b�l elt�rol�sra ker�lt.\n"
#define MSGTR_DVDauthOk "DVD autentik�ci�s folyamat �gy t�nik sikerrel v�gz�d�tt.\n"
#define MSGTR_DumpSelectedSteramMissing "dump: V�GZETES�HIBA: a k�rt stream nem tal�lhat�!\n"
#define MSGTR_CantOpenDumpfile "Nem tudom megnyitni a dump file-t!\n"
#define MSGTR_CoreDumped "Kinyomattam a cuccost, j�l.\n"
#define MSGTR_FPSnotspecified "A fejl�cben tal�lhat� FPS �rt�k nincs megadva, vagy hib�s! Haszn�ld az -fps opci�t!\n"
#define MSGTR_NoVideoStream "Ebben nincs video stream... egyel�re lej�tszhatatlan\n"
#define MSGTR_TryForceAudioFmt "Megpr�b�lom a(z) %d audio codec csal�dot haszn�lni ...\n"
#define MSGTR_CantFindAfmtFallback "A megadott audio codec csal�dban nem tal�ltam ideval� meghajt�t, pr�b�lkozok m�s meghajt�val.\n"
#define MSGTR_CantFindAudioCodec "Nem tal�lok codecet a(z) 0x%X audio form�tumhoz !\n"
#define MSGTR_TryUpgradeCodecsConfOrRTFM "*** Friss�tsd a %s-t a DOCS/codecs.conf-b�l\n*** Ha m�g mindig nem j�, olvasd el a DOCS/CODECS-et!\n"
#define MSGTR_CouldntInitAudioCodec "Nem tudom ind�tani az audio codecet! -> nincshang ;)\n"
#define MSGTR_TryForceVideoFmt "Megpr�b�lom a(z) %d video codec csal�dot haszn�lni ...\n"
#define MSGTR_CantFindVfmtFallback "A megadott video codec csal�dban nem tal�ltam ideval� meghajt�t, pr�b�lkozok m�s meghajt�val.\n"
#define MSGTR_CantFindVideoCodec "Nem tal�lok codecet a(z) 0x%X video form�tumhoz !\n"
#define MSGTR_VOincompCodec "A kiv�lasztott video_out meghajt� inkompatibilis ezzel a codec-kel.\n"
#define MSGTR_CouldntInitVideoCodec "V�GZETES�HIBA: Nem siker�lt a video codecet elind�tani :(\n"
#define MSGTR_EncodeFileExists "A %s file m�r l�tezik (nehogy let�r�ld a kedvenc AVI-dat!)\n"
#define MSGTR_CantCreateEncodeFile "Nem tudom enk�dol�s c�lj�b�l l�trehozni a file-t\n"
#define MSGTR_CannotInitVO "V�GZETES�HIBA:�Nem tudom elind�tani a video meghajt�t!\n"
#define MSGTR_CannotInitAO "nem tudom megnyitni az audio egys�get -> NOSOUND\n"
#define MSGTR_StartPlaying "Lej�tsz�s ind�t�sa...\n"
#define MSGTR_SystemTooSlow "\n************************************************************************"\
			    "\n** A rendszered t�l LASS� ehhez! Pr�b�ld -framedrop-pal, vagy RTFM�!  **"\
			    "\n************************************************************************\n"
//#define MSGTR_

// open.c: 
#define MSGTR_CdDevNotfound "A CD-ROM meghajt� (%s) nem tal�lhat�!\n"
#define MSGTR_ErrTrackSelect "Hiba a VCD s�v kiv�laszt�sakor!"
#define MSGTR_ReadSTDIN "Olvas�s a szabv�nyos bemenetr�l (stdin)...\n"
#define MSGTR_UnableOpenURL "Nem megnyithat� az URL: %s\n"
#define MSGTR_ConnToServer "Csatlakozom a szerverhez: %s\n"
#define MSGTR_FileNotFound "A f�jl nem tal�lhat�: '%s'\n"

// demuxer.c:
#define MSGTR_AudioStreamRedefined "Vigy�zat! T�bbsz�r�sen defin�lt Audio folyam! (Hib�s f�jl?)\n"
#define MSGTR_VideoStreamRedefined "Vigy�zat! T�bbsz�r�sen defin�lt Video folyam! (Hib�s f�jl?)\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: T�l sok (%d db, %d b�jt) audio csomag a pufferben!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: T�l sok (%d db, %d b�jt) video csomag a pufferben!\n"
#define MSGTR_MaybeNI "(tal�n ez egy nem �sszef�s�lt f�jl vagy a CODEC nem m�k�dik j�l)\n"
#define MSGTR_DetectedAVIfile "Ez egy AVI form�tum� f�jl!\n"
#define MSGTR_DetectedASFfile "Ez egy ASF form�tum� f�jl!\n"
#define MSGTR_DetectedMPEGPESfile "Ez egy MPEG-PES form�tum� f�jl!\n"
#define MSGTR_DetectedMPEGPSfile "Ez egy MPEG-PS form�tum� f�jl!\n"
#define MSGTR_DetectedMPEGESfile "Ez egy MPEG-ES form�tum� f�jl!\n"
#define MSGTR_DetectedQTMOVfile "Ez egy QuickTime/MOV form�tum� f�jl! (ez m�g nem t�mogatott)\n"
#define MSGTR_MissingMpegVideo "Nincs MPEG video folyam!? L�pj kapcsolatba a k�sz�t�kkel, lehet hogy hiba!\n"
#define MSGTR_InvalidMPEGES "Hib�s MPEG-ES folyam??? L�pj kapcsolatba a k�sz�t�kkel, lehet hogy hiba!\n"
#define MSGTR_FormatNotRecognized "========= Sajnos ez a f�jlform�tum ismeretlen vagy nem t�mogatott ===========\n"\
				  "= Ha ez egy AVI, ASF vagy MPEG f�jl, l�pj kapcsolatba a k�sz�t�kkel (hiba)! =\n"
#define MSGTR_MissingASFvideo "ASF: Nincs k�p folyam!\n"
#define MSGTR_MissingASFaudio "ASF: Nincs hang folyam...  -> hang n�lk�l\n"
#define MSGTR_MissingMPEGaudio "MPEG: Nincs hang folyam...  -> hang n�lk�l\n"

//#define MSGTR_


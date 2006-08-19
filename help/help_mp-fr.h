// Last sync on 2006-06-23 with help_mp-en.h 1.238
// Additionnal updates, fixes and translations by P Lombard <p_l@gmx.fr> 
//        & G Pelletier <pellgill@gmail.com>
// Updates & fixes by pl <p_l@gmx.fr> & n.le gaillart <n@tourmentine.com>
// Original translation by Firebird <firebird@chez.com>

// ========================= Aide MPlayer ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"Utilisation:      mplayer [options] [url|r�pertoire/]fichier\n"
"\n"
"Options de base:  (liste compl�te dans la page de man)\n"
" -vo <pil[:p�r]>  pilote et p�riph. vid�o de sortie ('-vo help' pour liste)\n"
" -ao <pil[:p�r]>  pilote et p�riph. audio de sortie ('-ao help' pour liste)\n"
#ifdef HAVE_VCD
" vcd://<n�piste>  lit piste (S)VCD (Super Video CD) (p�rif. brut, non-mont�)\n"
#endif
#ifdef USE_DVDREAD
" dvd://<n�titre>  lit titre DVD du p�rif. plut�t que d'un fichier\n"
" -alang/-slang    langue audio/sous-titres du DVD (code pays 2 lettres)\n"
#endif
" -ss <pos>        d�marre lecture � 'pos' (temps en secondes ou hh:mm:ss)\n"
" -nosound         ne joue aucun son\n"
" -fs              plein-�cran (ou -vm, -zoom, d�tails dans page man)\n"
" -x <x> -y <y>    r�solution de l'affichage (� utiliser avec -vm ou -zoom)\n"
" -sub <fich>      fichier sous-titres � utiliser (cf. -subfps, -subdelay)\n"
" -playlist <fich> fichier des titres audios � lire\n"
" -vid x -aid y    sp�cifie les flux vid�o (x) et audio (y) � lire\n"
" -fps x -srate y  change fr�quences vid�o (x fps) et audio (y Hz)\n"
" -pp <qualit�>    active le filtre de post-traitement (d�tails page man)\n"
" -framedrop       active saut d'images (pour machines lentes)\n"
"\n"
"Fonctions au clavier: (liste compl�te dans page man, voir aussi input.conf)\n"
" <- ou ->         arri�re/avant 10 secondes\n"
" haut ou bas      arri�re/avant 1 minute\n"
" PgUp ou PgDown   arri�re/avant 10 minutes\n"
" < ou >           fichier pr�c�dent/suivant dans liste audio � lire\n"
" p ou ESPACE      pause film (presser n'importe quelle touche pour continuer)\n"
" q ou ESC         arr�te la lecture et quitte le programme\n"
" + ou -           ajuste d�lai audio: +/- 0.1 seconde\n"
" o                cycle mode OSD: aucun/barre recherche/barre rech. + temps\n"
" * ou /           augmente/diminue le volume PCM\n"
" x ou z           ajuste d�lai des sous-titres: +/- 0.1 seconde\n"
" r ou t           ajuste position sous-titres: +haut/+bas, cf. -vf expand\n"
"\n"
" * * * VOIR PAGE MAN POUR D�TAILS, AUTRES OPTIONS (AVANC�ES) ET TOUCHES * * *\n"
"\n";
#endif

#define MSGTR_SamplesWanted "�chantillons ce format demand�s pour am�liorer support. Contacter developpeurs.\n"

// ========================= Messages MPlayer ===========================

// mplayer.c: 

#define MSGTR_Exiting "\nSortie...\n"
#define MSGTR_ExitingHow "\nSortie... (%s)\n"
#define MSGTR_Exit_quit "Fin"
#define MSGTR_Exit_eof "Fin du fichier"
#define MSGTR_Exit_error "Erreur fatale"
#define MSGTR_IntBySignal "\nMPlayer interrompu par le signal %d dans le module: %s\n"
#define MSGTR_NoHomeDir "Impossible de trouver le r�pertoire HOME.\n"
#define MSGTR_GetpathProblem "Probl�me get_path(\"config\")\n"
#define MSGTR_CreatingCfgFile "Cr�ation du fichier config: %s\n"
#define MSGTR_CopyCodecsConf "Copiez/liez etc/codecs.conf des sources de MPlayer vers ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "Utilisation du codecs.conf int�gr� par d�faut\n"
#define MSGTR_CantLoadFont "Ne peut charger la police: %s\n"
#define MSGTR_CantLoadSub "Ne peut charger les sous-titres: %s\n"
#define MSGTR_DumpSelectedStreamMissing "Vidage de la m�moire (dump): FATAL: flux s�lectionn� manquant !\n"
#define MSGTR_CantOpenDumpfile "Impossible d'ouvrir le fichier pour le vidage de la m�moire (dump).\n"
#define MSGTR_CoreDumped "Vidage de la m�moire du noyeau (core dump) effectu� ;)\n"
#define MSGTR_FPSnotspecified "FPS non sp�cifi� dans l'ent�te ou invalide! Utilisez l'option -fps.\n"
#define MSGTR_TryForceAudioFmtStr "Tente de forcer la famille de codecs audio %s ...\n"
#define MSGTR_CantFindAudioCodec "Ne peut trouver de codec pour le format audio 0x%X.\n"
#define MSGTR_RTFMCodecs "Veuillez lire DOCS/HTML/fr/codecs.html !\n"
#define MSGTR_TryForceVideoFmtStr "Tente de forcer la famille de codecs vid�o %s  ...\n"
#define MSGTR_CantFindVideoCodec "Ne peut trouver codec pour format -vo s�lectionn� et vid�o 0x%X.\n"
#define MSGTR_CannotInitVO "FATAL: Ne peut initialiser le pilote vid�o.\n"
#define MSGTR_CannotInitAO "Ne peut ouvrir/initialiser le p�riph�rique audio -> pas de son.\n"
#define MSGTR_StartPlaying "D�marre la lecture...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"         ************************************************************\n"\
"         **** Votre syst�me est trop LENT pour jouer ce fichier! ****\n"\
"         ************************************************************\n\n"\
"Raisons possibles, probl�mes, solutions:\n"\
"- Le plus courant: pilote _audio_ corrompu/bogu�\n"\
"  - Essayez -ao sdl ou l'�mulation OSS d'ALSA.\n"\
"  - Essayez diff�rentes valeurs pour -autosync, 30 est un bon d�but.\n"\
"- Sortie vid�o lente\n"\
"  - Essayez avec un pilote -vo diff�rent (-vo help pour la liste) ou\n"\
"    essayez avec -framedrop !\n"\
"- CPU lent\n"\
"  - N'essayez pas de lire de gros DVD/DivX sur un CPU lent!\n"\
"    Essayez une des options -lavdopts,\n"\
"    e.g. -vfm ffmpeg -lavdopts lowres=1:fast:skiploopfilter=all.\n"\
"- Fichier corrompu\n"\
"  - Essayez diff�rentes combinaisons de -nobps -ni -forceidx -mc 0.\n"\
"- Pour jouer depuis un m�dia lent (NFS/SMB, DVD, VCD, etc.)\n"\
"  - Essayez -cache 8192\n"\
"- Utilisez-vous -cache avec un fichier AVI non multiplex� ? \n"\
"  - Essayez avec -nocache\n"\
"Lisez DOCS/HTML/fr/video.html pour les astuces de r�glage/acc�l�ration.\n"\
"Si rien de tout cela ne vous aide, lisez DOCS/HTML/fr/bugreports.html.\n\n"

#define MSGTR_NoGui "MPlayer a �t� compil� SANS support GUI.\n"
#define MSGTR_GuiNeedsX "MPlayer GUI a besoin de X11.\n"
#define MSGTR_Playing "Lecture de %s\n"
#define MSGTR_NoSound "Audio: pas de son\n"
#define MSGTR_FPSforced "FPS forc� � %5.3f  (ftime: %5.3f)\n"
#define MSGTR_CompiledWithRuntimeDetection "Compil� avec d�tection du CPU � l'ex�cution."
#define MSGTR_CompiledWithCPUExtensions "Compil� pour CPU x86 avec les extensions:"
#define MSGTR_AvailableVideoOutputDrivers "Pilotes de sortie vid�o disponibles:\n"
#define MSGTR_AvailableAudioOutputDrivers "Pilotes de sortie audio disponibles:\n"
#define MSGTR_AvailableAudioCodecs "Codecs audio disponibles:\n"
#define MSGTR_AvailableVideoCodecs "Codecs vid�o disponibles:\n"
#define MSGTR_AvailableAudioFm "Familles/pilotes de codecs audio disponibles (inclus � la compilation):\n"
#define MSGTR_AvailableVideoFm "Familles/pilotes de codecs vid�o disponibles (inclus � la compilation):\n"
#define MSGTR_AvailableFsType "Modes de changement de couches plein �cran disponibles:\n"
#define MSGTR_UsingRTCTiming "Utilisation de la synchronisation mat�rielle par RTC (%ldHz)\n"
#define MSGTR_CannotReadVideoProperties "Vid�o: impossible de lire les propri�t�s\n"
#define MSGTR_NoStreamFound "Aucun flux trouv�.\n"
#define MSGTR_ErrorInitializingVODevice "Erreur � l'ouverture/initialisation de la sortie vid�o choisie (-vo).\n"
#define MSGTR_ForcedVideoCodec "Codec vid�o forc�: %s\n"
#define MSGTR_ForcedAudioCodec "Codec audio forc�: %s\n"
#define MSGTR_Video_NoVideo "Vid�o: pas de vid�o\n"
#define MSGTR_NotInitializeVOPorVO "\nFATAL: impossible d'initialiser filtres vid�o (-vf) ou sortie vid�o (-vo).\n"
#define MSGTR_Paused "\n  =====  PAUSE  =====\r" // no more than 23 characters (status line for audio files)
#define MSGTR_PlaylistLoadUnable "\nImpossible de charger la liste de lecture %s.\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- MPlayer a plant� � cause d'une 'Instruction Ill�gale'.\n"\
"  Il y a peut-�tre un bogue dans notre nouveau code de d�tection CPU...\n"\
"  Veuillez lire DOCS/HTML/fr/bugreports.html.\n"
#define MSGTR_Exit_SIGILL \
"- MPlayer a plant� � cause d'une 'Instruction Ill�gale'.\n"\
"  Cela se produit g�n�ralement quand vous le lancez sur un CPU diff�rent\n"\
"  de celui pour lequel il a �t� compil�/optimis�.\n V�rifiez !\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- MPlayer a plant� � cause d'une mauvaise utilisation de CPU/FPU/RAM.\n"\
"  Recompilez MPlayer avec --enable-debug et faites un backtrace 'gdb' et\n"\
"  d�sassemblage. D�tails: DOCS/HTML/fr/bugreports_what.html#bugreports_crash\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer a plant�. Cela n'aurait pas d� arriver.\n"\
"  Peut-�tre un bogue dans code de MPlayer _ou_ dans vos pilotes _ou_ dans votre\n"\
"  version de gcc. C'est la faute de MPlayer? Lire DOCS/HTML/fr/bugreports.html\n"\
"  et suivre les instructions. Nous pourrons et voudrons vous aider si vous\n"\
"  fournissiez ces informations en rapportant un bogue possible.\n"
#define MSGTR_LoadingConfig "Chargement du config '%s'\n"
#define MSGTR_AddedSubtitleFile "SUB: fichier sous-titres ajout� (%d): %s\n"
#define MSGTR_RemovedSubtitleFile "SUB: fichier sous-titres enlev� (%d): %s\n"
#define MSGTR_ErrorOpeningOutputFile "Erreur d'ouverture du fichier [%s] en �criture!\n"
#define MSGTR_CommandLine "Ligne de commande:"
#define MSGTR_RTCDeviceNotOpenable "�chec � l'ouverture de %s: %s (devrait �tre lisible par l'utilisateur.)\n"
#define MSGTR_LinuxRTCInitErrorIrqpSet "Erreur init RTC Linux dans ioctl (rtc_irqp_set %lu): %s\n"
#define MSGTR_IncreaseRTCMaxUserFreq "Essayer ajout \"echo %lu > /proc/sys/dev/rtc/max-user-freq\" au script d�mar de votre sys.\n"
#define MSGTR_LinuxRTCInitErrorPieOn "Erreur init RTC Linux dans ioctl (rtc_pie_on): %s\n"
#define MSGTR_UsingTimingType "Utilisation de minuterie %s.\n"
#define MSGTR_NoIdleAndGui "L'option -idle ne peut �tre utilis�e avec GMPlayer.\n"
#define MSGTR_MenuInitialized "Menu initialis�: %s\n"
#define MSGTR_MenuInitFailed "�chec d'initialisation du Menu.\n"
#define MSGTR_Getch2InitializedTwice "ATTENTION: getch2_init appel� deux fois!\n"
#define MSGTR_DumpstreamFdUnavailable "Impossible de vider ce flux - Aucun descripteur de fichier disponible.\n"
#define MSGTR_FallingBackOnPlaylist "Replie sur essaie d'analyse de liste de lecture (playlist) %s...\n"
#define MSGTR_CantOpenLibmenuFilterWithThisRootMenu "Impossible d'ouvrir filtre vid�o libmenu avec menu root %s.\n"
#define MSGTR_AudioFilterChainPreinitError "Erreur de pr�-initialisation de la cha�ne de filtres audio !\n"
#define MSGTR_LinuxRTCReadError "Erreur de lecture horloge temps r�el (RTC) Linux: %s\n"
#define MSGTR_SoftsleepUnderflow "Attention! Soupassement sommeil l�ger (time_frame n�gatif)!\n"
#define MSGTR_DvdnavNullEvent "�v�nement DVDNAV NUL?!\n"
#define MSGTR_DvdnavHighlightEventBroken "�v�nement DVDNAV : �v�nement surbrillance rompu\n"
#define MSGTR_DvdnavEvent "�v�nement DVDNAV : %s\n"
#define MSGTR_DvdnavHighlightHide "�v�nement DVDNAV : Cache surbrillance\n"
#define MSGTR_DvdnavStillFrame "#################################### �v�nement DVDNAV : Image fixe : %d sec(s)\n"
#define MSGTR_DvdnavNavStop "�v�nement DVDNAV : Arret de navigation \n"
#define MSGTR_DvdnavNavNOP "�v�nement DVDNAV : Pas d'op�ration (NOP) navigation \n"
#define MSGTR_DvdnavNavSpuStreamChangeVerbose "�v�nement DVDNAV: Changement flux SPU nav : phys: %d/%d/%d log: %d\n"
#define MSGTR_DvdnavNavSpuStreamChange "�v�nement DVDNAV: Changement de flux de navigation SPU: phys: %d logique: %d\n"
#define MSGTR_DvdnavNavAudioStreamChange "�v�nement DVDNAV: Changement de flux de navigation Audio: phys: %d logique: %d\n"
#define MSGTR_DvdnavNavVTSChange "�v�nement DVDNAV: Changement de navigation VTS\n"
#define MSGTR_DvdnavNavCellChange "�v�nement DVDNAV: Changement de cellule de navigation\n"
#define MSGTR_DvdnavNavSpuClutChange "�v�nement DVDNAV: Changement de navigation SPU CLUT\n"
#define MSGTR_DvdnavNavSeekDone "�v�nement DVDNAV: Cherche navigation faite\n"
#define MSGTR_MenuCall "Appel menu\n"

#define MSGTR_EdlOutOfMem "Impossible d'allouer assez de m�moire pour contenir les donn�es EDL.\n"
#define MSGTR_EdlRecordsNo "Lu %d actions EDL.\n"
#define MSGTR_EdlQueueEmpty "Aucune action EDL � g�rer.\n"
#define MSGTR_EdlCantOpenForWrite "Impossible d'ouvrir fichier EDL [%s] en �criture.\n"
#define MSGTR_EdlCantOpenForRead "Impossible d'ouvrir fichier EDL [%s] en lecture.\n"
#define MSGTR_EdlNOsh_video "Impossible d'utiliser EDL sans video, d�sactive.\n"
#define MSGTR_EdlNOValidLine "Ligne EDL invalide: %s\n"
#define MSGTR_EdlBadlyFormattedLine "Ligne EDL mal format�e [%d] Rejet.\n"
#define MSGTR_EdlBadLineOverlap "Derni�re position d'arret: [%f]; d�part suivant: "\
"[%f]. Entr�es doivent �tre en ordre chrono, ne peuvent se chevaucher. Rejet.\n"
#define MSGTR_EdlBadLineBadStop "Temps d'arret doit �tre apr�s temps de d�part.\n"

// mplayer.c OSD

#define MSGTR_OSDenabled "activ�"
#define MSGTR_OSDdisabled "d�sactiv�"
#define MSGTR_OSDChannel "Canal: %s"
#define MSGTR_OSDSubDelay "D�calage: %d ms"
#define MSGTR_OSDSpeed "Vitesse: x %6.2f"
#define MSGTR_OSDosd "OSD: %s"

// property values
#define MSGTR_Enabled "activ�"
#define MSGTR_EnabledEdl "activ� (edl)"
#define MSGTR_Disabled "d�sactiv�"
#define MSGTR_HardFrameDrop "dur"
#define MSGTR_Unknown "inconnu"
#define MSGTR_Bottom "bas"
#define MSGTR_Center "centre"
#define MSGTR_Top "haut"

// osd bar names
#define MSGTR_Volume "Volume"
#define MSGTR_Panscan "Recadrage"
#define MSGTR_Gamma "Gamma"
#define MSGTR_Brightness "Brillance"
#define MSGTR_Contrast "Contraste"
#define MSGTR_Saturation "Saturation"
#define MSGTR_Hue "Tonalit�"

// property state
#define MSGTR_MuteStatus "Silence: %s"
#define MSGTR_AVDelayStatus "delai A-V: %s"
#define MSGTR_OnTopStatus "Reste au dessus: %s"
#define MSGTR_RootwinStatus "Fen�tre racine: %s"
#define MSGTR_BorderStatus "Bordure: %s"
#define MSGTR_FramedroppingStatus "Saut d'images: %s"
#define MSGTR_VSyncStatus "Sync vertical: %s"
#define MSGTR_SubSelectStatus "Sous-titres: %s"
#define MSGTR_SubPosStatus "Sous position: %s/100"
#define MSGTR_SubAlignStatus "Sous alignment: %s"
#define MSGTR_SubDelayStatus "D�calage: %s"
#define MSGTR_SubVisibleStatus "Sous-titres: %s"
#define MSGTR_SubForcedOnlyStatus "Sub forc� seulement: %s"

// mencoder.c

#define MSGTR_UsingPass3ControllFile "Utilisation du fichier de contr�le de la passe 3: %s\n"
#define MSGTR_MissingFilename "\nNom de fichier manquant.\n\n"
#define MSGTR_CannotOpenFile_Device "Impossible d'ouvrir le fichier/p�riph.\n"
#define MSGTR_CannotOpenDemuxer "Impossible d'ouvrir le d�muxeur.\n"
#define MSGTR_NoAudioEncoderSelected "\nAucun encodeur audio (-oac) choisi. Choisir un (voir aide -oac) ou -nosound.\n"
#define MSGTR_NoVideoEncoderSelected "\nAucun encodeur vid�o (-ovc) choisi ! Choisissez-en un (voir l'aide pour -ovc).\n"
#define MSGTR_CannotOpenOutputFile "Impossible d'ouvrir le fichier de sortie '%s'\n"
#define MSGTR_EncoderOpenFailed "Impossible d'ouvrir l'encodeur\n"
#define MSGTR_MencoderWrongFormatAVI "\nATTENTION: LE FORMAT DU FICHIER DE SORTIE EST _AVI_. Voir '-of help'.\n"
#define MSGTR_MencoderWrongFormatMPG "\nATTENTION: LE FICHIER DU FICHIER DE SORTIE EST _MPEG_. Voir '-of help'.\n"
#define MSGTR_MissingOutputFilename "Aucun fichier de sortie sp�cifi�, veuillez voir l'option -o"
#define MSGTR_ForcingOutputFourcc "Code fourcc de sortie forc� � %x [%.4s]\n"
#define MSGTR_ForcingOutputAudiofmtTag "For�age du tag du format audio de sortie � 0x%x\n"
#define MSGTR_DuplicateFrames "\n%d image(s) r�p�t�e(s)!\n"
#define MSGTR_SkipFrame "\nImage saut�e!\n"
#define MSGTR_ResolutionDoesntMatch "\nLe nouveau fichier vid�o a r�solution ou espace couleur diff�rent du pr�c�dent.\n"
#define MSGTR_FrameCopyFileMismatch "\nTous fichiers vid�o doivent utiliser m�mes fps, r�solution, codec pour copie -ovc.\n"
#define MSGTR_AudioCopyFileMismatch "\nTous fichiers audio doivent utiliser m�mes codec et format pour copie -oac.\n"
#define MSGTR_NoAudioFileMismatch "\nNe peut m�langer fichiers vid�o seul et fichiers vid�o/audio. -nosound?\n"
#define MSGTR_NoSpeedWithFrameCopy "ATTENTION: -speed peut ne pas fonctionner correctement avec -oac copy!\n"\
"Votre encodement pourrait �tre bris�!\n"
#define MSGTR_ErrorWritingFile "%s: Erreur durant l'�criture du fichier.\n"
#define MSGTR_RecommendedVideoBitrate "D�bit binaire (bitrate) vid�o recommand� pour le CD %s: %d\n"
#define MSGTR_VideoStreamResult "\nFlux vid�o: %8.3f kbit/s  (%d B/s)  taille: %d octets  %5.3f secs  %d images\n"
#define MSGTR_AudioStreamResult "\nFlux audio: %8.3f kbit/s  (%d B/s)  taille: %d octets  %5.3f secs\n"
#define MSGTR_OpenedStream "succ�s: format: %d  data: 0x%X - 0x%x\n"
#define MSGTR_VCodecFramecopy "codec vid�o: copie de trame (%dx%d %dbpp fourcc=%x)\n"
#define MSGTR_ACodecFramecopy "codec audio: copie img (format=%x canaux=%d taux=%d bits=%d B/s=%d �chant-%d)\n"
#define MSGTR_CBRPCMAudioSelected "Audio CBR PCM selectionn�\n"
#define MSGTR_MP3AudioSelected "Audio MP3 s�lectionn�\n"
#define MSGTR_CannotAllocateBytes "N'a pu allouer %d octets\n"
#define MSGTR_SettingAudioDelay "R�glage du d�lai audio � %5.3fs\n"
#define MSGTR_SettingVideoDelay "R�glage du d�lai vid�o � %5.3fs\n"
#define MSGTR_SettingAudioInputGain "R�glage du gain audio en entr�e � %f\n"
#define MSGTR_LamePresetEquals "\npr�-r�glages=%s\n\n"
#define MSGTR_LimitingAudioPreload "Limitation du pr�chargement audio � 0.4s\n"
#define MSGTR_IncreasingAudioDensity "Augmentation de la densit� audio � 4\n"
#define MSGTR_ZeroingAudioPreloadAndMaxPtsCorrection "For�age du pr�-chargement audio � 0 et de la correction max des pts � 0\n"
#define MSGTR_CBRAudioByterate "\n\nAudio CBR: %d octets/s, %d octets/bloc\n"
#define MSGTR_LameVersion "LAME version %s (%s)\n\n"
#define MSGTR_InvalidBitrateForLamePreset "Erreur: le bitrate sp�cifi� est hors de l'intervalle valide pour ce pr�-r�glage\n"\
"\n"\
"Lorsque vous utilisez ce mode, la valeur doit �tre entre \"8\" et \"320\"\n"\
"\n"\
"Pour plus d'information, essayez: \"-lameopts preset=help\"\n"
#define MSGTR_InvalidLamePresetOptions "Erreur: vous n'avez pas entr� de profil valide et/ou d'option avec preset (pr�-r�glage)\n"\
"\n"\
"Les profils disponibles sont:\n"\
"\n"\
"   <fast>        standard\n"\
"   <fast>        extreme\n"\
"                 insane\n"\
"   <cbr> (Mode ABR) - C'est le mode par d�faut. Pour l'utiliser,\n"\
"                      il suffit de pr�ciser un bitrate. Par exemple:\n"\
"                      \"preset=185\" active ce pr�-r�glage\n"\
"                      et utilise un bitrate moyen de 185kbps.\n"\
"\n"\
"    Quelques exemples:\n"\
"\n"\
"    \"-lameopts fast:preset=standard  \"\n"\
" ou \"-lameopts  cbr:preset=192       \"\n"\
" ou \"-lameopts      preset=172       \"\n"\
" ou \"-lameopts      preset=extreme   \"\n"\
"\n"\
"Pour plus d'informations, essayez: \"-lameopts preset=help\"\n"
#define MSGTR_LamePresetsLongInfo "\n"\
"Les pr�-r�glages ont �t� con�us pour offrir la plus haute qualit� possible.\n"\
"\n"\
"Ces r�glages ont �t� optimis�s par le biais de double �coute en aveugle\n"\
"pour v�rifier qu'ils atteignaient leurs objectifs.\n"\
"\n"\
"Ils sont continuellement mis � jour pour tirer partie des derniers d�veloppements\n"\
"et offrent donc par cons�quent la meilleure qualit� possible avec LAME.\n"\
"\n"\
"Pour activer ces pr�-r�glages:\n"\
"\n"\
"   Pour les modes VBR (en g�neral, la plus haute qualit�):\n"\
"\n"\
"     \"preset=standard\" Ce mode devrait �tre transparent pour la plupart\n"\
"                             des gens, sur la plupart des musiques. Sa\n"\
"                             qualit� est vraiment �lev�e.\n"\
"\n"\
"     \"preset=extreme\" Si vous avez une tr�s bonne audition, ainsi que du\n"\
"                             mat�riel de qualit�, ce pr�-r�glage offrira\n"\
"                             une qualit� l�g�rement sup�rieure � celle du\n"\
"                             mode \"standard\"\n"\
"\n"\
"   Pour le CBR � 320kbps (la plus haute qualit� possible avec les pr�-r�glages):\n"\
"\n"\
"     \"preset=insane\"  Ce r�glage sera excessif pour la plupart des gens\n"\
"                             et des situations mais, si vous devez absolument\n"\
"                             avoir la plus haute qualit� et que vous n'avez pas\n"\
"                             de contrainte de taille, choisissez cette option.\n"\
"\n"\
"   Pour les modes ABR (haute qualit� pour un bitrate donn�e - mais moins que pour du VBR):\n"\
"\n"\
"     \"preset=<kbps>\"  Utiliser ce pr�-r�glage fournira une bonne qualit�\n"\
"                             pour un bitrate sp�cifi�. Selon le bitrate\n"\
"                             entr�, ce pr�-r�glage d�terminera les r�glages\n"\
"                             optimaux pour cette situation particuli�re.\n"\
"                             Bien que cette approche fonctionne, elle n'est pas\n"\
"                             aussi flexible que le VBR, et n'offrira pas en g�n�ral\n"\
"                             les m�mes niveaux que ceux du VBR aux bitrates �lev�s.\n"\
"\n"\
"Les options suivantes sont aussi disponibles pour les profils correspondants:\n"\
"\n"\
"   <fast>        standard\n"\
"   <fast>        extreme\n"\
"                 insane\n"\
"   <cbr> (Mode ABR) - C'est le mode par d�faut. Pour l'utiliser,\n"\
"                      il suffit de pr�ciser un bitrate. Par exemple:\n"\
"                      \"preset=185\" active ce pr�-r�glage\n"\
"                      et utilise un bitrate moyen de 185kbps.\n"\
"\n"\
"   \"fast\" - Active le nouveau mode rapide VBR pour un profil donn�. Les\n"\
"              d�savantages de cette option sont que, souvent, le bitrate\n"\
"              final sera l�g�rement plus �lev� que pour le mode normal\n"\
"              et que la qualit� peut aussi �tre l�g�rement inf�rieure.\n"\
"   Attention: avec la version actuelle, les pr�-r�glages en mode 'fast'\n"\
"              peuvent utiliser des bitrates trop �lev�s par rapport � ceux\n"\
"              des pr�-r�glages normaux.\n"\
"\n"\
"   \"cbr\"  - Si vous utilisez le mode ABR (voir ci-dessus) avec un bitrate\n"\
"              sp�cial tel que 80, 96, 112, 128, 160, 192, 224, 256, 320,\n"\
"              vous pouvez utiliser l'option \"cbr\" pour forcer une compression\n"\
"              en CBR au lieu d'ABR. ABR fournit une qualit� plus �lev�e\n"\
"              mais le CBR peut �tre utile dans certains cas comme par exemple, pour\n"\
"              pour distribuer un flux MP3 sur Internet.\n"\
"\n"\
"    Par exemple:\n"\
"\n"\
"    \"-lameopts fast:preset=standard  \"\n"\
" ou \"-lameopts  cbr:preset=192       \"\n"\
" ou \"-lameopts      preset=172       \"\n"\
" ou \"-lameopts      preset=extreme   \"\n"\
"\n"\
"\n"\
"Quelques noms de pr�-r�glages sont disponibles pour le mode ABR:\n"\
"phone => 16kbps/mono        phon+/lw/mw-eu/sw => 24kbps/mono\n"\
"mw-us => 40kbps/mono        voice => 56kbps/mono\n"\
"fm/radio/tape => 112kbps    hifi => 160kbps\n"\
"cd => 192kbps               studio => 256kbps"
#define MSGTR_LameCantInit "Ne peux pas r�gler les options de LAME, v�rifiez dans bitrate/samplerate,"\
"certains bitrates tr�s bas (<32) requi�rent des taux d'�chantillonages plus bas (i.e. -srate 8000)."\
"Si rien ne marche, essayez un pr�-r�glage (preset)."
#define MSGTR_ConfigfileError "Erreur du fichier de configuration"
#define MSGTR_ErrorParsingCommandLine "Erreur en parsant la ligne de commande"
#define MSGTR_VideoStreamRequired "La pr�sence d'un flux vid�o est obligatoire !\n"
#define MSGTR_ForcingInputFPS "Le fps d'entr�e sera plut�t interpr�t� comme %5.2f\n"
#define MSGTR_RawvideoDoesNotSupportAudio "Le format de sortie RAWVIDEO ne supporte pas l'audio - audio d�sactiv�\n"
#define MSGTR_DemuxerDoesntSupportNosound "Ce demuxer ne supporte pas encore l'option -nosound.\n"
#define MSGTR_MemAllocFailed "Une allocation m�moire a �chou�\n"
#define MSGTR_NoMatchingFilter "N'a pas pu trouver une correspondance filtre/ao!\n"
#define MSGTR_MP3WaveFormatSizeNot30 "sizeof(MPEGLAYER3WAVEFORMAT)==%d!=30, peut-�tre un compilateur C cass�?\n"
#define MSGTR_NoLavcAudioCodecName "Audio LAVC, nom de codec manquant!\n"
#define MSGTR_LavcAudioCodecNotFound "Audio LAVC, encodeur pour le codec %s introuvable\n"
#define MSGTR_CouldntAllocateLavcContext "Audio LAVC, �chec lors de l'allocation du contexte!\n"
#define MSGTR_CouldntOpenCodec "�chec de l'ouvertur du codec %s, br=%d\n"
#define MSGTR_CantCopyAudioFormat "Le format audio 0x%x est incompatible avec '-oac copy', veuillez essayer '-oac pcm' � la place, ou bien utilisez '-fafmttag' pour forcer ce mode.\n"

// cfg-mencoder.h:

#define MSGTR_MEncoderMP3LameHelp "\n\n"\
" vbr=<0-4>     m�thode � d�bit binaire (bitrate) variable\n"\
"                0: cbr (d�bit binaire constant)\n"\
"                1: mt (Mark Taylor)\n"\
"                2: rh (Robert Hegemann) (par d�faut)\n"\
"                3: abr (d�bit binaire disponible)\n"\
"                4: mtrh (Mark Taylor Robert Hegemann)\n"\
"\n"\
" abr           d�bit binaire (bitrate) disponible\n"\
"\n"\
" cbr           d�bit binaire (bitrate) constant\n"\
"               Force �galement l'encodage en mode CBR sur les modes pr�r�gl�s ABR subsequents\n"\
"\n"\
" br=<0-1024>   sp�cifie le d�bit binaire (bitrate) en kbits (CBR et ABR uniquement)\n"\
"\n"\
" q=<0-9>       qualit� (0-plus haute, 9-plus basse) (uniquement pour VBR)\n"\
"\n"\
" aq=<0-9>      qualit� algorithmique (0-meilleure/plus lente, 9-pire/plus rapide)\n"\
"\n"\
" ratio=<1-100> rapport de compression\n"\
"\n"\
" vol=<0-10>    d�finit le gain d'entr�e audio\n"\
"\n"\
" mode=<0-3>    (par d�faut: auto)\n"\
"                0: stereo\n"\
"                1: st�r�o commune (joint-stereo)\n"\
"                2: canal double (dualchannel)\n"\
"                3: mono\n"\
"\n"\
" padding=<0-2>\n"\
"                0: non\n"\
"                1: tout\n"\
"                2: ajuste\n"\
"\n"\
" fast          acc�l�re l'encodage pour les modes pr�r�gl�s VBR subs�quents,\n"\
"               qualit� l�g�rement inf�rieure et d�bits binaires (bitrates) plus �lev�s.\n"\
"\n"\
" preset=<valeur> fournit les plus hauts param�tres de qualit� possibles.\n"\
"                 medium: encodage VBR, bonne qualit�\n"\
"                 (intervalle de d�bit binaire (bitrate) 150-180 kbps)\n"\
"                 standard: encodage VBR, haute qualit�\n"\
"                 (intervalle de d�bit binaire (bitrate) 170-210 kbps)\n"\
"                 extreme: encodage VBR, tr�s haute qualit�\n"\
"                 (intervalle de d�bit binaire (bitrate) 200-240 kbps)\n"\
"                 insane: encodage CBR, plus haute qualit� pr�r�gl�e\n"\
"                 (bitrate 320 kbps)\n"\
"                 <8-320>: encodage ABR au d�bit binaire (bitrate) moyen indiqu� en kbps.\n\n"

//codec-cfg.c:
#define MSGTR_DuplicateFourcc "code FourCC dupliqu�"
#define MSGTR_TooManyFourccs "trop de FourCCs..."
#define MSGTR_ParseError "erreur de syntaxe"
#define MSGTR_ParseErrorFIDNotNumber "erreur de syntaxe (l'ID du format n'est pas un nombre ?)"
#define MSGTR_ParseErrorFIDAliasNotNumber "erreur de syntaxe (l'ID de l'alias n'est pas un nombre ?)"
#define MSGTR_DuplicateFID "ID du format dupliqu�"
#define MSGTR_TooManyOut "Trop de 'out'..."
#define MSGTR_InvalidCodecName "\nLe nom de codec (%s) n'est pas valide!\n"
#define MSGTR_CodecLacksFourcc "\nLe nom de codec(%s) n'a pas de FourCC!\n"
#define MSGTR_CodecLacksDriver "\nLe codec (%s) n'a pas de pilote!\n"
#define MSGTR_CodecNeedsDLL "\nLe codec (%s) requiert une 'dll'!\n"
#define MSGTR_CodecNeedsOutfmt "\nLe codec (%s) requier un 'outfmt'!\n"
#define MSGTR_CantAllocateComment "Ne peut allouer de m�moire pour le commentaire. "
#define MSGTR_GetTokenMaxNotLessThanMAX_NR_TOKEN "get_token(): max >= MAX_MR_TOKEN!"
#define MSGTR_ReadingFile "Lecture de %s: "
#define MSGTR_CantOpenFileError "Ne peut ouvrir '%s': %s\n"
#define MSGTR_CantGetMemoryForLine "Ne peut allouer de m�moire pour 'line': %s\n"
#define MSGTR_CantReallocCodecsp "Ne peut pas effectuer de realloc() pour '*codecsp': %s\n"
#define MSGTR_CodecNameNotUnique "Le nom du codec '%s' n'est pas unique."
#define MSGTR_CantStrdupName "Ne peut appeler strdup() -> 'name': %s\n"
#define MSGTR_CantStrdupInfo "Ne peut appler strdup() -> 'info': %s\n"
#define MSGTR_CantStrdupDriver "Ne peut appeler strdup() -> 'driver': %s\n"
#define MSGTR_CantStrdupDLL "Ne peut appeler strdup() -> 'dll': %s"
#define MSGTR_AudioVideoCodecTotals "%d codecs audio & %d codecs vid�o\n"
#define MSGTR_CodecDefinitionIncorrect "Le codec n'est pas d�fini correctement."
#define MSGTR_OutdatedCodecsConf "Ce fichier codecs.conf est trop vieux et est incompatible avec cette version de MPlayer !"

// divx4_vbr.c:
#define MSGTR_OutOfMemory "plus de m�moire libre"
#define MSGTR_OverridingTooLowBitrate "Le d�bit binaire demand� est trop bas pour ce clip.\n"\
"Le d�bit binaire minimum pour ce clip est %.0f kbps.\n"\
"Les valeurs demand�es par l'utilisateur vont �tre ignor�es.\n"

// fifo.c
#define MSGTR_CannotMakePipe "Ne peut cr�er de canal de communication (pipe) !\n"

// m_config.c
#define MSGTR_SaveSlotTooOld "Case de sauvegarde trop ancienne trouv�e lvl %d: %d !!!\n"
#define MSGTR_InvalidCfgfileOption "L'option '%s' ne peut �tre utilis�e dans un fichier de configuration.\n"
#define MSGTR_InvalidCmdlineOption "L'option '%s' ne peut �tre utilis�e sur la ligne de commande.\n"
#define MSGTR_InvalidSuboption "Erreur: l'option '%s' n'a pas de sous-option '%s'.\n"
#define MSGTR_MissingSuboptionParameter "Erreur: la sous-option '%s' de '%s' doit avoir un param�tre!\n"
#define MSGTR_MissingOptionParameter "Erreur: l'option '%s' doit avoir un param�tre!\n"
#define MSGTR_OptionListHeader "\n Nom                  Type            Min        Max      Global  CL    Cfg\n\n"
#define MSGTR_TotalOptions "\nTotal: %d options\n"
#define MSGTR_TooDeepProfileInclusion "ATTENTION: inclusion de profils trop imbriqu�e.\n"
#define MSGTR_NoProfileDefined "Aucun profil n'a �t� d�fini.\n"
#define MSGTR_AvailableProfiles "Profils disponibles:\n"
#define MSGTR_UnknownProfile "Profil inconnu '%s'.\n"
#define MSGTR_Profile "Profil %s: %s\n"
// m_property.c
#define MSGTR_PropertyListHeader "\n Nom                  Type            Min        Max\n\n"
#define MSGTR_TotalProperties "\nTotal: %d propri�t�s\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "Lecteur CD-ROM '%s' non trouv�.\n"
#define MSGTR_ErrTrackSelect "Erreur lors du choix de la piste VCD.\n"
#define MSGTR_ReadSTDIN "Lecture depuis stdin...\n"
#define MSGTR_UnableOpenURL "Impossible d'ouvrir l'URL: %s\n"
#define MSGTR_ConnToServer "Connect� au serveur: %s\n"
#define MSGTR_FileNotFound "Fichier non trouv�: '%s'\n"

#define MSGTR_SMBInitError "Impossible d'initialiser libsmbclient: %d\n"
#define MSGTR_SMBFileNotFound "Impossible d'ouvrir depuis le r�seau local: '%s'\n"
#define MSGTR_SMBNotCompiled "MPlayer n'a pas �t� compil� avec le support de lecture SMB\n"

#define MSGTR_CantOpenDVD "Impossible d'ouvrir le lecteur DVD: %s\n"
#define MSGTR_NoDVDSupport "MPlayer a �t� compil� sans support pour les DVD - terminaison\n"
#define MSGTR_DVDnumTitles "Il y a %d titres sur ce DVD.\n"
#define MSGTR_DVDinvalidTitle "Num�ro de titre DVD invalide: %d\n"
#define MSGTR_DVDnumChapters "Il y a %d chapitres sur ce titre DVD.\n"
#define MSGTR_DVDinvalidChapter "Num�ro de chapitre DVD invalide: %d\n"
#define MSGTR_DVDinvalidChapterRange "Intervalle des chapitre invalide %s\n"
#define MSGTR_DVDinvalidLastChapter "Num�ro de dernier chapitre du DVD invalide: %d\n"
#define MSGTR_DVDnumAngles "Il y a %d angles sur ce titre DVD.\n"
#define MSGTR_DVDinvalidAngle "Num�ro d'angle DVD invalide: %d\n"
#define MSGTR_DVDnoIFO "Impossible d'ouvrir le fichier IFO pour le titre DVD %d.\n"
#define MSGTR_DVDnoVMG "Ne peut ouvrir les informations VMG!\n"
#define MSGTR_DVDnoVOBs "Impossible d'ouvrir le titre VOBS (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDnoMatchingAudio "Aucun canal audio correspondant sur ce DVD!\n"
#define MSGTR_DVDaudioChannel "Canal audio du DVD choisi: %d langage: %c%c\n"
#define MSGTR_DVDnoMatchingSubtitle "Aucun sous-titre correspondant sur ce DVD!\n"
#define MSGTR_DVDsubtitleChannel "Canal de sous-titres du DVD choisi: %d language: %c%c\n"

// muxer.c, muxer_*.c:
#define MSGTR_TooManyStreams "Trop de flux!"
#define MSGTR_RawMuxerOnlyOneStream "Le multiplexeur RAWAUDIO ne supporte qu'un seul flux audio!\n"
#define MSGTR_IgnoringVideoStream "Flux vid�o non pris en compte!\n"
#define MSGTR_UnknownStreamType "Attention! flux de type inconnu: %d\n"
#define MSGTR_WarningLenIsntDivisible "Attention! la longueur 'len' n'est pas divisible par la taille de l'�chantillon (!\n"
#define MSGTR_MuxbufMallocErr "Tampon d'image Muxeur ne peut allouer de la m�moire!\n"
#define MSGTR_MuxbufReallocErr "Tampon d'image Muxeur ne peut r�allouer de la m�moire!\n"
#define MSGTR_MuxbufSending "Tampon d'image Muxeur envoie %d image(s) au muxeur.\n"
#define MSGTR_WritingHeader "�criture de l'ent�te...\n"
#define MSGTR_WritingTrailer "�criture de l'index...\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "ATTENTION: Ent�te du flux audio %d red�fini.\n"
#define MSGTR_VideoStreamRedefined "ATTENTION: Ent�te du flux vid�o %d red�fini.\n"
#define MSGTR_TooManyAudioInBuffer "\nTrop de paquets audio dans le tampon (%d dans %d octets)\n"
#define MSGTR_TooManyVideoInBuffer "\nTrop de paquets vid�o dans le tampon (%d dans %d octets)\n"
#define MSGTR_MaybeNI "Peut-�tre que vous jouez un flux/fichier non entrelac�, ou que le codec a �chou� ?\n"\
                      "Pour les fichier AVI, essayez de forcer le mode non-entrelac� avec l'option -ni.\n"
#define MSGTR_SwitchToNi "\nFichier AVI mal entrelac� - passage en mode -ni...\n"
#define MSGTR_Detected_XXX_FileFormat "Fichier de type %s d�tect�.\n"
#define MSGTR_DetectedAudiofile "Fichier audio d�tect�.\n"
#define MSGTR_NotSystemStream "Pas un flux de type MPEG System... (peut-�tre un Flux de Transport?)\n"
#define MSGTR_InvalidMPEGES "Flux MPEG-ES invalide ??? Contactez l'auteur, c'est peut-�tre un bogue :(\n"
#define MSGTR_FormatNotRecognized "========== D�sol�, ce format de fichier n'est pas reconnu/support� ============\n"\
                                  "== Si ce fichier est un flux AVI, ASF ou MPEG, merci de contacter l'auteur ! ==\n"
#define MSGTR_MissingVideoStream "Aucun flux vid�o trouv�.\n"
#define MSGTR_MissingAudioStream "Aucun flux audio trouv� -> pas de son\n"
#define MSGTR_MissingVideoStreamBug "Flux vid�o manquant !? Contactez l'auteur, c'est peut-�tre un bogue :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: le fichier ne contient pas le flux audio ou vid�o s�lectionn�.\n"

#define MSGTR_NI_Forced "Forc�"
#define MSGTR_NI_Detected "D�tect�"
#define MSGTR_NI_Message "format de fichier AVI NON-ENTRELAC� %s.\n"

#define MSGTR_UsingNINI "Utilise le format des fichiers AVI endommag�s NON-ENTRELAC�.\n"
#define MSGTR_CouldntDetFNo "Impossible de d�terminer le nombre d'images (pour recherche absolue)\n"
#define MSGTR_CantSeekRawAVI "Impossible de chercher dans un flux AVI brut ! (Index requis, essayez l'option -idx.)\n"
#define MSGTR_CantSeekFile "Impossible de chercher dans ce fichier.\n"
#define MSGTR_EncryptedVOB "Fichier VOB chiffr�! Veuillez lire DOCS/HTML/fr/cd-dvd.html.\n"

#define MSGTR_MOVcomprhdr "MOV: Le support d'ent�tes compress�es n�cessite ZLIB !\n"
#define MSGTR_MOVvariableFourCC "MOV: ATTENTION: FOURCC Variable d�tect� !?\n"
#define MSGTR_MOVtooManyTrk "MOV: ATTENTION: Trop de pistes"
#define MSGTR_FoundAudioStream "==> Flux audio trouv�: %d\n"
#define MSGTR_FoundVideoStream "==> Flux vid�o trouv�: %d\n"
#define MSGTR_DetectedTV "TV d�tect�e ! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "Impossible d'ouvrir le demuxer ogg\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: recherche du flux audio (id:%d)\n"
#define MSGTR_CannotOpenAudioStream "Impossible d'ouvrir le flux audio: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "Impossible d'ouvrir le flux des sous-titres: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "Echec � l'ouverture du demuxer audio: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "Echec � l'ouverture du demuxer de sous-titres: %s\n"
#define MSGTR_TVInputNotSeekable "Impossible de rechercher sur l'entr�e TV ! (cette op�ration correspondra s�rement � un changement de chaines ;)\n"
#define MSGTR_DemuxerInfoAlreadyPresent "Info du demuxer %s d�j� pr�sente!\n"
#define MSGTR_ClipInfo "Information sur le clip: \n"

#define MSGTR_LeaveTelecineMode "\ndemux_mpg: contenu NTSC 30000/1001fps d�tect�, ajustement du d�bit.\n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: contenu NTSC 24000/1001fps progressif d�tect�, ajustement du d�bit.\n"
#define MSGTR_CacheFill "\rRemplissage du cache: %5.2f%% (%"PRId64" octets)   "
#define MSGTR_NoBindFound "Aucune action attach�e � la touche '%s'"
#define MSGTR_FailedToOpen "�chec � l'ouverture de '%s'\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "Impossible d'ouvrir le codec.\n"
#define MSGTR_CantCloseCodec "Impossible de fermer le codec.\n"

#define MSGTR_MissingDLLcodec "ERREUR: Impossible d'ouvrir le codec DirectShow requis: %s\n"
#define MSGTR_ACMiniterror "Impossible de charger/initialiser le codec AUDIO Win32/ACM (fichier DLL manquant ?)\n"
#define MSGTR_MissingLAVCcodec "Impossible de trouver le codec '%s' dans libavcodec...\n"

#define MSGTR_MpegNoSequHdr "MPEG: FATAL: Fin du fichier lors de la recherche d'ent�te de s�quence\n"
#define MSGTR_CannotReadMpegSequHdr "FATAL: Ne peut lire l'ent�te de s�quence.\n"
#define MSGTR_CannotReadMpegSequHdrEx "FATAL: Ne peut lire l'extension d'ent�te de s�quence.\n"
#define MSGTR_BadMpegSequHdr "MPEG: Mauvaise ent�te de s�quence\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: Mauvaise extension d'ent�te de s�quence\n"

#define MSGTR_ShMemAllocFail "Impossible d'allouer la m�moire partag�e\n"
#define MSGTR_CantAllocAudioBuf "Impossible d'allouer le tampon de sortie audio\n"

#define MSGTR_UnknownAudio "Format audio inconnu/manquant -> pas de son\n"

#define MSGTR_UsingExternalPP "[PP] Utilisation de filtres de postprocessing externes, max q = %d\n"
#define MSGTR_UsingCodecPP "[PP] Utilisation du postprocessing du codec, max q = %d\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "L'attribut vid�o '%s' n'est pas support� par ce vo & ce vd. \n"
#define MSGTR_VideoCodecFamilyNotAvailableStr "Famille de codecs vid�o demand�e [%s] (vfm=%s) non disponible (activez-la � la compilation)\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "Famille de codecs audio demand�e [%s] (afm=%s) non disponible (activez-la � la compilation)\n"
#define MSGTR_OpeningVideoDecoder "Ouverture du d�codeur vid�o: [%s] %s\n"
#define MSGTR_SelectedVideoCodec "Codec vid�o choisi: [%s] vfm: %s (%s)\n"
#define MSGTR_UninitVideoStr "D�sinitialisation vid�o: %s  \n"
#define MSGTR_UninitAudioStr "D�sinitialisation audio: %s  \n"
#define MSGTR_VDecoderInitFailed "Echec de l'initialisation de VDecoder :(\n"
#define MSGTR_ADecoderInitFailed "Echec de l'initialisation de ADecoder :(\n"
#define MSGTR_ADecoderPreinitFailed "Echec de la pr�-initialisation de l'ADecoder :(\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: allocation de %d octets comme tampon d'entr�e\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: allocation %d + %d = %d octets comme tampon de sortie\n"

// LIRC:
#define MSGTR_SettingUpLIRC "Mise en place du support LIRC...\n"
#define MSGTR_LIRCdisabled "Vous ne pourrez pas utiliser votre t�l�commande\n"
#define MSGTR_LIRCopenfailed "Impossible d'activer le support LIRC.\n"
#define MSGTR_LIRCcfgerr "Impossible de lire le fichier de config de LIRC %s.\n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "Impossible de trouver le filtre vid�o '%s'\n"
#define MSGTR_CouldNotOpenVideoFilter "Impossible d'ouvrir le filtre vid�o '%s'\n"
#define MSGTR_OpeningVideoFilter "Ouverture du filtre vid�o: "
#define MSGTR_CannotFindColorspace "Impossible de trouver espace colorim�trique assorti, m�me en utilisant 'scale' :(\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDec: le codec n'a pas d�fini sh->disp_w et sh->disp_h, essai de contournement !\n"
#define MSGTR_VoConfigRequest "VDec: requ�te de config de vo - %d x %d (espace colorim�trique pr�fer�: %s)\n"
#define MSGTR_CouldNotFindColorspace "N'a pas pu trouver espace colorim�trique correspondant - nouvel essai avec -vf scale...\n"
#define MSGTR_MovieAspectIsSet "L'aspect du film est %.2f:1 - pr�-redimensionnement � l'aspect correct.\n"
#define MSGTR_MovieAspectUndefined "L'aspect du film est ind�fini - pas de pr�-dimensionnement appliqu�.\n"
// vd_dshow.c, vd_dmo.c
#define MSGTR_DownloadCodecPackage "Vous devez mettre � jour/installer le package contenant les codecs binaires.\nAllez sur http://www.mplayerhq.hu/dload.html\n"
#define MSGTR_DShowInitOK "INFO: initialisation r�ussie du codec vid�o Win32/DShow.\n"
#define MSGTR_DMOInitOK "INFO: initialisation r�ussie du codec vid�o Win32/DMO.\n"

// x11_common.c
#define MSGTR_EwmhFullscreenStateFailed "\nX11: n'a pas pu envoyer l'�v�nement EWMH pour passer en plein �cran!\n"
#define MSGTR_CouldNotFindXScreenSaver "xscreensaver_disable: n'a pas pu trouver de fen�tre XScreenSaver.\n"
#define MSGTR_SelectedVideoMode "XF86VM: le mode vid�o %dx%d a �t� choisi pour une taille d'image %dx%d.\n"

#define MSGTR_InsertingAfVolume "[Mixer] Pas de support mat�riel pour le mixage, insertion du filtre logiciel de volume.\n"
#define MSGTR_NoVolume "[Mixer] Aucun contr�le de volume disponible.\n"

// ====================== messages/boutons GUI ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "� propos..."
#define MSGTR_FileSelect "Choisir un fichier..."
#define MSGTR_SubtitleSelect "Choisir un sous-titre..."
#define MSGTR_OtherSelect "Choisir..."
#define MSGTR_AudioFileSelect "Choisir une source audio ext�rieure..."
#define MSGTR_FontSelect "Choisir une police..."
#define MSGTR_PlayList "Liste de lecture"
#define MSGTR_Equalizer "�galisateur"
#define MSGTR_ConfigureEqualizer "Configure �galisateur"
#define MSGTR_SkinBrowser "Navigateur de peaux"
#define MSGTR_Network "Streaming depuis le r�seau ..."
#define MSGTR_Preferences "Pr�f�rences"
#define MSGTR_AudioPreferences "Configuration de pilote Audio"
#define MSGTR_NoMediaOpened "Aucun m�dia ouvert"
#define MSGTR_VCDTrack "Piste du VCD %d"
#define MSGTR_NoChapter "Aucun chapitre"
#define MSGTR_Chapter "Chapitre %d"
#define MSGTR_NoFileLoaded "Aucun fichier charg�"

// --- boutons ---
#define MSGTR_Ok "OK"
#define MSGTR_Cancel "Annuler"
#define MSGTR_Add "Ajouter"
#define MSGTR_Remove "Supprimer"
#define MSGTR_Clear "Effacer"
#define MSGTR_Config "Configurer"
#define MSGTR_ConfigDriver "Configuration du pilote"
#define MSGTR_Browse "Naviger"

// --- messages d'erreur ---
#define MSGTR_NEMDB "D�sol�, pas assez de m�moire pour le tampon de dessin."
#define MSGTR_NEMFMR "D�sol�, pas assez de m�moire pour le rendu des menus."
#define MSGTR_IDFGCVD "D�sol�, aucun pilote de sortie vid�o compatible avec la GUI."
#define MSGTR_NEEDLAVCFAME "D�sol�, vous ne pouvez pas jouer de fichier non-MPEG avec votre p�riph�rique DXR3/H+ sans r�encodage.\nVeuillez activer lavc ou fame dans la bo�te de configuration DXR3/H+."
#define MSGTR_UNKNOWNWINDOWTYPE "Genre de fen�tre inconnue trouv� ..."

// --- messages d'erreurs du chargement de peau ---
#define MSGTR_SKIN_ERRORMESSAGE "[Peau] erreur � la ligne %d du fichier de config de peau: %s"
#define MSGTR_SKIN_WARNING1 "[Peau] attention � la ligne %d du fichier de config de peau: Widget (%s) trouv� mais aucune \"section\" trouv� avant lui."
#define MSGTR_SKIN_WARNING2 "[Peau] attention � la ligne %d du fichier de config de peau: Widget (%s) trouv� mais aucune \"subsection\" trouv� avant lui."
#define MSGTR_SKIN_WARNING3 "[Peau] attention � la ligne %d du fichier de config de peau: cette sous-section n'est pas support� par le widget (%s)"
#define MSGTR_SKIN_SkinFileNotFound "[peau] fichier ( %s ) non trouv�.\n"
#define MSGTR_SKIN_SkinFileNotReadable "[peau] fichier ( %s ) non lisible.\n"
#define MSGTR_SKIN_BITMAP_16bit  "les images bitmaps 16 bits ou moins ne sont pas support�es ( %s ).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "Fichier non trouv� (%s)\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "erreur de lecture BMP (%s)\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "erreur de lecture TGA (%s)\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "erreur de lecture PNG (%s)\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "tga compact� en RLE non support� (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "format de fichier inconnu (%s)\n"
#define MSGTR_SKIN_BITMAP_ConvertError "erreur de conversion de 24 bit en 32 bit (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "message inconnu: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "pas assez de m�moire\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "trop de polices d�clar�es.\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "fichier de police introuvable.\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "fichier d'image de police introuvable\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "identificateur de fonte in�xistant (%s)\n"
#define MSGTR_SKIN_UnknownParameter "param�tre inconnu (%s)\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "Skin non trouv�e (%s).\n"
#define MSGTR_SKIN_SKINCFG_SelectedSkinNotFound "Peau choisi ( %s ) non trouv�, essaie de 'par d�faut'...\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "erreur de lecture du fichier de configuration du peau (%s)\n"
#define MSGTR_SKIN_LABEL "Peaux:"

// --- menus gtk
#define MSGTR_MENU_AboutMPlayer "� propos de MPlayer"
#define MSGTR_MENU_Open "Ouvrir..."
#define MSGTR_MENU_PlayFile "Lire un fichier..."
#define MSGTR_MENU_PlayVCD "Lire un VCD..."
#define MSGTR_MENU_PlayDVD "Lire un DVD..."
#define MSGTR_MENU_PlayURL "Lire une URL..."
#define MSGTR_MENU_LoadSubtitle "Charger un sous-titre..."
#define MSGTR_MENU_DropSubtitle "Laisser tomber un sous-titre..."
#define MSGTR_MENU_LoadExternAudioFile "Chargement d'un fichier audio externe..."
#define MSGTR_MENU_Playing "En cours de lecture"
#define MSGTR_MENU_Play "Lecture"
#define MSGTR_MENU_Pause "Pause"
#define MSGTR_MENU_Stop "Arr�t"
#define MSGTR_MENU_NextStream "Flux suivant"
#define MSGTR_MENU_PrevStream "Flux pr�c�dent"
#define MSGTR_MENU_Size "Taille"
#define MSGTR_MENU_HalfSize   "Demi taille"
#define MSGTR_MENU_NormalSize "Taille normale"
#define MSGTR_MENU_DoubleSize "Taille double"
#define MSGTR_MENU_FullScreen "Plein �cran"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "Ouvrir un disque..."
#define MSGTR_MENU_ShowDVDMenu "Afficher le menu DVD"
#define MSGTR_MENU_Titles "Titres"
#define MSGTR_MENU_Title "Titre %2d"
#define MSGTR_MENU_None "(aucun)"
#define MSGTR_MENU_Chapters "Chapitres"
#define MSGTR_MENU_Chapter "Chapitre %2d"
#define MSGTR_MENU_AudioLanguages "Langues audio"
#define MSGTR_MENU_SubtitleLanguages "Langues des sous-titres"
#define MSGTR_MENU_PlayList MSGTR_PlayList
#define MSGTR_MENU_SkinBrowser "Navigateur de peaux"
#define MSGTR_MENU_Preferences MSGTR_Preferences
#define MSGTR_MENU_Exit "Quitter..."
#define MSGTR_MENU_Mute "Silence"
#define MSGTR_MENU_Original "Original"
#define MSGTR_MENU_AspectRatio "rapport hauteur/largeur"
#define MSGTR_MENU_AudioTrack "Piste audio"
#define MSGTR_MENU_Track "Piste %d"
#define MSGTR_MENU_VideoTrack "Piste Vid�o"

// --- equalizer
#define MSGTR_EQU_Audio "Audio"
#define MSGTR_EQU_Video "Vid�o"
#define MSGTR_EQU_Contrast "Contraste: "
#define MSGTR_EQU_Brightness "Luminosit�: "
#define MSGTR_EQU_Hue "Tonalit�: "
#define MSGTR_EQU_Saturation "Saturation: "
#define MSGTR_EQU_Front_Left "Avant Gauche"
#define MSGTR_EQU_Front_Right "Avant Droit"
#define MSGTR_EQU_Back_Left "Arri�re Gauche"
#define MSGTR_EQU_Back_Right "Arri�re Droit"
#define MSGTR_EQU_Center "Centre"
#define MSGTR_EQU_Bass "Basses"
#define MSGTR_EQU_All "Tout"
#define MSGTR_EQU_Channel1 "Canal 1:"
#define MSGTR_EQU_Channel2 "Canal 2:"
#define MSGTR_EQU_Channel3 "Canal 3:"
#define MSGTR_EQU_Channel4 "Canal 4:"
#define MSGTR_EQU_Channel5 "Canal 5:"
#define MSGTR_EQU_Channel6 "Canal 6:"

// --- playlist
#define MSGTR_PLAYLIST_Path "Chemin"
#define MSGTR_PLAYLIST_Selected "Fichiers choisis"
#define MSGTR_PLAYLIST_Files "Fichiers"
#define MSGTR_PLAYLIST_DirectoryTree "Hi�rarchie des dossiers"

// --- preferences
#define MSGTR_PREFERENCES_Audio MSGTR_EQU_Audio
#define MSGTR_PREFERENCES_Video MSGTR_EQU_Video
#define MSGTR_PREFERENCES_SubtitleOSD "Sous-titres & OSD"
#define MSGTR_PREFERENCES_Codecs "Codecs & demuxeur"
#define MSGTR_PREFERENCES_Misc "Divers"

#define MSGTR_PREFERENCES_None "Aucun"
#define MSGTR_PREFERENCES_DriverDefault "Pilote par d�faut"
#define MSGTR_PREFERENCES_AvailableDrivers "Pilotes disponibles:"
#define MSGTR_PREFERENCES_DoNotPlaySound "Ne pas jouer le son"
#define MSGTR_PREFERENCES_NormalizeSound "Normaliser le son"
#define MSGTR_PREFERENCES_EnEqualizer "Activer l'�qualiseur"
#define MSGTR_PREFERENCES_SoftwareMixer "Activer mixeur logiciel"
#define MSGTR_PREFERENCES_ExtraStereo "Activer st�r�o suppl�mentaire"
#define MSGTR_PREFERENCES_Coefficient "Coefficient:"
#define MSGTR_PREFERENCES_AudioDelay "Retard audio"
#define MSGTR_PREFERENCES_DoubleBuffer "Activer tampon double"
#define MSGTR_PREFERENCES_DirectRender "Activer le rendu direct"
#define MSGTR_PREFERENCES_FrameDrop "Activer les sauts d'images"
#define MSGTR_PREFERENCES_HFrameDrop "Activer saut DUR d'images (dangereux)"
#define MSGTR_PREFERENCES_Flip "Mirroir vertical"
#define MSGTR_PREFERENCES_Panscan "Recadrage: "
#define MSGTR_PREFERENCES_OSDTimer "Minuteur et indicateurs"
#define MSGTR_PREFERENCES_OSDProgress "Barres de progression seulement"
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "Minuteur, pourcentage et temps total"
#define MSGTR_PREFERENCES_Subtitle "Sous-titre:"
#define MSGTR_PREFERENCES_SUB_Delay "D�calage: "
#define MSGTR_PREFERENCES_SUB_FPS "FPS:"
#define MSGTR_PREFERENCES_SUB_POS "Position: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "D�sactiver le chargement automatique des sous-titres"
#define MSGTR_PREFERENCES_SUB_Unicode "Sous-titre en Unicode"
#define MSGTR_PREFERENCES_SUB_MPSUB "Convertir le sous-titre au format MPlayer"
#define MSGTR_PREFERENCES_SUB_SRT "Convertir le sous-titre au format SubViewer (SRT) bas� sur le temps"
#define MSGTR_PREFERENCES_SUB_Overlap "Bascule le recouvrement des sous-titres"
#define MSGTR_PREFERENCES_Font "Police:"
#define MSGTR_PREFERENCES_FontFactor "Facteur de police:"
#define MSGTR_PREFERENCES_PostProcess "Activer le postprocessing"
#define MSGTR_PREFERENCES_AutoQuality "Qualit� auto.: "
#define MSGTR_PREFERENCES_NI "Utiliser le parseur d'AVI non entrelac�"
#define MSGTR_PREFERENCES_IDX "Reconstruire l'index, si n�cessaire"
#define MSGTR_PREFERENCES_VideoCodecFamily "Famille de codecs vid�o:"
#define MSGTR_PREFERENCES_AudioCodecFamily "Famille de codecs audio:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "Niveau OSD"
#define MSGTR_PREFERENCES_FRAME_Subtitle "Sous-titre"
#define MSGTR_PREFERENCES_FRAME_Font "Police"
#define MSGTR_PREFERENCES_FRAME_PostProcess "post-traitement"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "Codec & demuxer"
#define MSGTR_PREFERENCES_FRAME_Cache "Cache"
#define MSGTR_PREFERENCES_FRAME_Misc MSGTR_PREFERENCES_Misc
#define MSGTR_PREFERENCES_Audio_Device "P�rif�rique:"
#define MSGTR_PREFERENCES_Audio_Mixer "Mixeur:"
#define MSGTR_PREFERENCES_Audio_MixerChannel "Canal de mixeur:"
#define MSGTR_PREFERENCES_Message "ATTENTION: certaines options requi�rent un red�marrage de la lecture!"
#define MSGTR_PREFERENCES_DXR3_VENC "Encodeur vid�o:"
#define MSGTR_PREFERENCES_DXR3_LAVC "Utiliser LAVC (FFmpeg)"
#define MSGTR_PREFERENCES_DXR3_FAME "Utiliser FAME"
#define MSGTR_PREFERENCES_FontEncoding1 "Unicode"
#define MSGTR_PREFERENCES_FontEncoding2 "Langues Europ�ennes Occidentales (ISO-8859-1)"
#define MSGTR_PREFERENCES_FontEncoding3 "Langues Europ�eenes Occidentales avec Euro (ISO-8859-15)"
#define MSGTR_PREFERENCES_FontEncoding4 "Langues Europ�eenes Slaves/Centrales (ISO-8859-2)"
#define MSGTR_PREFERENCES_FontEncoding5 "Esperanto, Galicien, Maltais, Turc (ISO-8859-3)"
#define MSGTR_PREFERENCES_FontEncoding6 "Caract�res Old Baltic (ISO-8859-4)"
#define MSGTR_PREFERENCES_FontEncoding7 "Cyrillique (ISO-8859-5)"
#define MSGTR_PREFERENCES_FontEncoding8 "Arabe (ISO-8859-6)"
#define MSGTR_PREFERENCES_FontEncoding9 "Grec Moderne (ISO-8859-7)"
#define MSGTR_PREFERENCES_FontEncoding10 "Turc (ISO-8859-9)"
#define MSGTR_PREFERENCES_FontEncoding11 "Balte (ISO-8859-13)"
#define MSGTR_PREFERENCES_FontEncoding12 "Celte (ISO-8859-14)"
#define MSGTR_PREFERENCES_FontEncoding13 "Hebreu (ISO-8859-8)"
#define MSGTR_PREFERENCES_FontEncoding14 "Russe (KOI8-R)"
#define MSGTR_PREFERENCES_FontEncoding15 "Ukrainien, Bi�lorusse (KOI8-U/RU)"
#define MSGTR_PREFERENCES_FontEncoding16 "Chinois Simplifi� (CP936)"
#define MSGTR_PREFERENCES_FontEncoding17 "Chinois Traditionnel (BIG5)"
#define MSGTR_PREFERENCES_FontEncoding18 "Japonais (SHIFT-JIS)"
#define MSGTR_PREFERENCES_FontEncoding19 "Cor�en (CP949)"
#define MSGTR_PREFERENCES_FontEncoding20 "Tha�landais (CP874)"
#define MSGTR_PREFERENCES_FontEncoding21 "Cyrillique Windows (CP1251)"
#define MSGTR_PREFERENCES_FontEncoding22 "Slave/Europe Centrale Windows (CP1250)"
#define MSGTR_PREFERENCES_FontNoAutoScale "Pas d'agrandissement auto"
#define MSGTR_PREFERENCES_FontPropWidth "Proportionnel � la largeur du film"
#define MSGTR_PREFERENCES_FontPropHeight "Proportionnel � la hauteur du film"
#define MSGTR_PREFERENCES_FontPropDiagonal "Proportionnel � la diagonale du film"
#define MSGTR_PREFERENCES_FontEncoding "Encodage:"
#define MSGTR_PREFERENCES_FontBlur "Flou:"
#define MSGTR_PREFERENCES_FontOutLine "Contour:"
#define MSGTR_PREFERENCES_FontTextScale "Echelle du texte:"
#define MSGTR_PREFERENCES_FontOSDScale "Echelle de l'OSD:"
#define MSGTR_PREFERENCES_Cache "Cache activ�/d�sactiv�"
#define MSGTR_PREFERENCES_CacheSize "Taille du cache: "
#define MSGTR_PREFERENCES_LoadFullscreen "D�marrer en plein �cran"
#define MSGTR_PREFERENCES_SaveWinPos "Enr�gistrer position de la fen�tre"
#define MSGTR_PREFERENCES_XSCREENSAVER "Arr�ter XScreenSaver"
#define MSGTR_PREFERENCES_PlayBar "Active la barre de lecture"
#define MSGTR_PREFERENCES_AutoSync "AutoSynchro on/off"
#define MSGTR_PREFERENCES_AutoSyncValue "Autosynchro: "
#define MSGTR_PREFERENCES_CDROMDevice "P�riph�rique CD-ROM:"
#define MSGTR_PREFERENCES_DVDDevice "P�riph�rique DVD:"
#define MSGTR_PREFERENCES_FPS "FPS du film:"
#define MSGTR_PREFERENCES_ShowVideoWindow "Affiche la fen�tre vid�o inactive"
#define MSGTR_PREFERENCES_ArtsBroken "Versions aRts plus r�centes sont incompatibles "\
           "avec GTK 1.x et feront planter GMPlayer!"

#define MSGTR_ABOUT_UHU "Le d�veloppement de la GUI est commandit� par UHU Linux\n"
#define MSGTR_ABOUT_Contributors "contribuants de code et de documentation\n"
#define MSGTR_ABOUT_Codecs_libs_contributions "Codecs et libraries tiers\n"
#define MSGTR_ABOUT_Translations "Traductions\n"
#define MSGTR_ABOUT_Skins "Peaux\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "Erreur fatale !"
#define MSGTR_MSGBOX_LABEL_Error "Erreur !"
#define MSGTR_MSGBOX_LABEL_Warning "Attention !"

// bitmap.c

#define MSGTR_NotEnoughMemoryC32To1 "[c32to1] m�moire insuffisante pour image\n"
#define MSGTR_NotEnoughMemoryC1To32 "[c1to32] m�moire insuffisante pour image\n"

// cfg.c

#define MSGTR_ConfigFileReadError "[cfg] erreur lecture fichier config ...\n"
#define MSGTR_UnableToSaveOption "[cfg] impossible de sauvegarder l'option '%s'.\n"

// interface.c

#define MSGTR_DeletingSubtitles "[GUI] Suppression des sous-titres.\n"
#define MSGTR_LoadingSubtitles "[GUI] Chargement des soustitres: %s\n"
#define MSGTR_AddingVideoFilter "[GUI] Ajout de filtre vid�o: %s\n"
#define MSGTR_RemovingVideoFilter "[GUI] Enl�vement de filtre video: %s\n"

// mw.c

#define MSGTR_NotAFile "Ceci ne semble pas �tre un fichier: %s !\n"

// ws.c

#define MSGTR_WS_CouldNotOpenDisplay "[ws] Impossible d'ouvrir l'affichage.\n"
#define MSGTR_WS_RemoteDisplay "[ws] Affichage � distance, d�sactive XMITSHM.\n"
#define MSGTR_WS_NoXshm "[ws] D�sol�, votre syst�me ne supporte pas l'extension de m�moire partag�e X.\n"
#define MSGTR_WS_NoXshape "[ws] D�sol�, votre syst�me ne supporte pas l'extension XShape.\n"
#define MSGTR_WS_ColorDepthTooLow "[ws] D�sol�, la profondeur d'�chantillonnage est trop basse.\n"
#define MSGTR_WS_TooManyOpenWindows "[ws] Trop de fen�tres ouvertes.\n"
#define MSGTR_WS_ShmError "[ws] erreur d'extension de m�moire partag�e\n"
#define MSGTR_WS_NotEnoughMemoryDrawBuffer "[ws] D�sol�, m�moire insuffisante pour tampon de dessin.\n"
#define MSGTR_WS_DpmsUnavailable "DPMS non disponible?\n"
#define MSGTR_WS_DpmsNotEnabled "Imposssible d'activer DPMS.\n"

// wsxdnd.c

#define MSGTR_WS_NotAFile "Ceci ne semble pas �tre un fichier...\n"
#define MSGTR_WS_DDNothing "D&D: Rien de retourn�!\n"

#endif

// ======================= VO Video Output drivers ========================

#define MSGTR_VOincompCodec "Le p�riph�rique de sortie vid�o s�lectionn� est incompatible avec ce codec.\n"\
                "Essayer d'ajouter le filtre d'�chelle, e.g. -vf spp,scale plut�t que -vf spp.\n"
#define MSGTR_VO_GenericError "Cette erreur s'est produite"
#define MSGTR_VO_UnableToAccess "Impossible d'acc�der"
#define MSGTR_VO_ExistsButNoDirectory "existe d�j�, mais n'est pas un r�pertoire."
#define MSGTR_VO_DirExistsButNotWritable "R�pertoire de sortie existe d�j�, mais n'est pas en �criture."
#define MSGTR_VO_DirExistsAndIsWritable "R�pertoire de sortie existe d�j� et n'est pas en �criture."
#define MSGTR_VO_CantCreateDirectory "Impossible de cr�er r�pertoire de sortie."
#define MSGTR_VO_CantCreateFile "Impossible de cr�er fichier de sortie."
#define MSGTR_VO_DirectoryCreateSuccess "R�pertoire de sorti cr�e avec succ�s."
#define MSGTR_VO_ParsingSuboptions "Analyse de sous-options."
#define MSGTR_VO_SuboptionsParsedOK "sous-options analys�es OK."
#define MSGTR_VO_ValueOutOfRange "Valeur hors plage"
#define MSGTR_VO_NoValueSpecified "Nulle valeur sp�cifi�e."
#define MSGTR_VO_UnknownSuboptions "Sous-option(s) inconnue(s)"

// vo_aa.c

#define MSGTR_VO_AA_HelpHeader "\n\nVoici les sous-options aalib vo_aa:\n"
#define MSGTR_VO_AA_AdditionalOptions "Options suppl�mentaires fournies par vo_aa:\n" \
"  help        imprime ce message d'aide\n" \
"  osdcolor    met couleur osd\n  subcolor    met couleur sous-titre\n" \
"        les param�tres de couleur sont:\n           0 : normal\n" \
"           1 : faible\n           2 : fort\n           3 : police forte\n" \
"           4 : invers�\n           5 : sp�cial\n\n\n"

// vo_jpeg.c
#define MSGTR_VO_JPEG_ProgressiveJPEG "JPEG progressif activ�."
#define MSGTR_VO_JPEG_NoProgressiveJPEG "JPEG progressif d�sactiv�."
#define MSGTR_VO_JPEG_BaselineJPEG "Ligne de base JPEG activ�e."
#define MSGTR_VO_JPEG_NoBaselineJPEG "Ligne de base JPEG d�sactiv�e."

// vo_pnm.c
#define MSGTR_VO_PNM_ASCIIMode "Mode ASCII activ�."
#define MSGTR_VO_PNM_RawMode "Mode cru activ�."
#define MSGTR_VO_PNM_PPMType "�criture de fichiers PPM."
#define MSGTR_VO_PNM_PGMType "�criture de fichiers PGM."
#define MSGTR_VO_PNM_PGMYUVType "�criture de fichiers PGMYUV."

// vo_yuv4mpeg.c
#define MSGTR_VO_YUV4MPEG_InterlacedHeightDivisibleBy4 "Mode entrelac� requiert hauteur d'image divisible par 4."
#define MSGTR_VO_YUV4MPEG_InterlacedLineBufAllocFail "Impossible d'allouer tampon de ligne pour mode entrelac�."
#define MSGTR_VO_YUV4MPEG_InterlacedInputNotRGB "Entr� non RGB, impossible d�composer chrominance!"
#define MSGTR_VO_YUV4MPEG_WidthDivisibleBy2 "Largeur d'image doit �tre divisible par 2."
#define MSGTR_VO_YUV4MPEG_NoMemRGBFrameBuf "M�moire insuffisante pour allouer tampon d'image RGB."
#define MSGTR_VO_YUV4MPEG_OutFileOpenError "Impossible d'obtenir ident. de fichier ou m�moire pour �criture \"%s\"!"
#define MSGTR_VO_YUV4MPEG_OutFileWriteError "Erreur d'�criture d'image vers sortie!"
#define MSGTR_VO_YUV4MPEG_UnknownSubDev "Sous-p�rif�rique inconnu: %s"
#define MSGTR_VO_YUV4MPEG_InterlacedTFFMode "Mode sortie entrelac�e utilis�e, champ haut au d�but."
#define MSGTR_VO_YUV4MPEG_InterlacedBFFMode "Mode sortie entrelac�e utilis�e, champ bas au d�but."
#define MSGTR_VO_YUV4MPEG_ProgressiveMode "Mode image progressive (par defaut) utilis�."

// sub.c
#define MSGTR_VO_SUB_Seekbar "Recherche"
#define MSGTR_VO_SUB_Play "Lecture"
#define MSGTR_VO_SUB_Pause "Pause"
#define MSGTR_VO_SUB_Stop "Arret"
#define MSGTR_VO_SUB_Rewind "Rembobine"
#define MSGTR_VO_SUB_Forward "Avant"
#define MSGTR_VO_SUB_Clock "Horloge"
#define MSGTR_VO_SUB_Contrast "Contraste"
#define MSGTR_VO_SUB_Saturation "Saturation"
#define MSGTR_VO_SUB_Volume "Volume"
#define MSGTR_VO_SUB_Brightness "Luminosit�"
#define MSGTR_VO_SUB_Hue "Tonalit�"

// vo_xv.c
#define MSGTR_VO_XV_ImagedimTooHigh "Dimensions d'image source trop �lev�es: %ux%u (maximum %ux%u)\n"

// Old vo drivers that have been replaced

#define MSGTR_VO_PGM_HasBeenReplaced "Pilote sortie vid�o pgm remplac� par -vo pnm:pgmyuv.\n"
#define MSGTR_VO_MD5_HasBeenReplaced "Pilote sortie vid�o md5 remplac� par -vo md5sum.\n"

// ======================= AO Audio Output drivers ========================

// libao2 

// audio_out.c
#define MSGTR_AO_ALSA9_1x_Removed "audio_out: modules alsa9 et alsa1x enlev�s, utiliser -ao alsa.\n"

// ao_oss.c
#define MSGTR_AO_OSS_CantOpenMixer "[AO OSS] audio_setup: Impossible d'ouvrir mixeur %s: %s\n"
#define MSGTR_AO_OSS_ChanNotFound "[AO OSS] audio_setup: Mixeur de carte audio n'a pas canal '%s' utilise default.\n"
#define MSGTR_AO_OSS_CantOpenDev "[AO OSS] audio_setup: Impossible ouvrir p�rif�rique audio %s: %s\n"
#define MSGTR_AO_OSS_CantMakeFd "[AO OSS] audio_setup: Impossible identifier desc de fichier gel�: %s\n"
#define MSGTR_AO_OSS_CantSet "[AO OSS] Impossible r�gler p�rif audio %s � sortie %s, essaie %s...\n"
#define MSGTR_AO_OSS_CantSetChans "[AO OSS] audio_setup: N'a pu r�gler p�rif audio � %d canaux.\n"
#define MSGTR_AO_OSS_CantUseGetospace "[AO OSS] audio_setup: Pilote ne supporte pas SNDCTL_DSP_GETOSPACE :-(\n"
#define MSGTR_AO_OSS_CantUseSelect "[AO OSS]\n   ***  Votre pilote audio ne supporte PAS select()  ***\n Recompiler MPlayer avec #undef HAVE_AUDIO_SELECT dans config.h !\n\n"
#define MSGTR_AO_OSS_CantReopen "[AO OSS]\nErreur fatale: *** IMPOSSIBLE R�OUVRIR / REPARTIR P�RIF AUDIO *** %s\n"
#define MSGTR_AO_OSS_UnknownUnsupportedFormat "[AO OSS] Format OSS inconnu/non-support�: %x.\n"

// ao_arts.c
#define MSGTR_AO_ARTS_CantInit "[AO ARTS] %s\n"
#define MSGTR_AO_ARTS_ServerConnect "[AO ARTS] Connect� au serveur de son.\n"
#define MSGTR_AO_ARTS_CantOpenStream "[AO ARTS] Impossible ouvrir flux.\n"
#define MSGTR_AO_ARTS_StreamOpen "[AO ARTS] Flux ouvert.\n"
#define MSGTR_AO_ARTS_BufferSize "[AO ARTS] Grandeur tampon: %d\n"

// ao_dxr2.c
#define MSGTR_AO_DXR2_SetVolFailed "[AO DXR2] N'a pu r�gler volume � %d.\n"
#define MSGTR_AO_DXR2_UnsupSamplerate "[AO DXR2] %d Hz non-support�, essayer r��chantillonnage.\n"

// ao_esd.c
#define MSGTR_AO_ESD_CantOpenSound "[AO ESD] �chec de esd_open_sound: %s\n"
#define MSGTR_AO_ESD_LatencyInfo "[AO ESD] latence: [serveur: %0.2fs, net: %0.2fs] (ajuste %0.2fs)\n"
#define MSGTR_AO_ESD_CantOpenPBStream "[AO ESD] �chec d'ouverture de flux rappel ESD: %s\n"

// ao_mpegpes.c
#define MSGTR_AO_MPEGPES_CantSetMixer "[AO MPEGPES] �chec mixeur ensemble audio DVB: %s.\n"
#define MSGTR_AO_MPEGPES_UnsupSamplerate "[AO MPEGPES] %d Hz non support�, essayer r��chantillonnage.\n"

// ao_null.c
// This one desn't even  have any mp_msg nor printf's?? [CHECK]

// ao_pcm.c
#define MSGTR_AO_PCM_FileInfo "[AO PCM] Fichier: %s (%s)\nPCM: �chantillonnement: %iHz Canaux: %s Format %s\n"
#define MSGTR_AO_PCM_HintInfo "[AO PCM] Info: Acc�l�rer d�chargement avec -vc null -vo null\n[AO PCM] Info: Pour �crire fichers WAVE utiliser -ao pcm:waveheader (par defaut).\n"
#define MSGTR_AO_PCM_CantOpenOutputFile "[AO PCM] �chec ouverture %s en �criture!\n"

// ao_sdl.c
#define MSGTR_AO_SDL_INFO "[AO SDL] �chantillonnage: %iHz Canaux: %s Format %s\n"
#define MSGTR_AO_SDL_DriverInfo "[AO SDL] pilote audio %s utilis�.\n"
#define MSGTR_AO_SDL_UnsupportedAudioFmt "[AO SDL] format audio non support�: 0x%x.\n"
#define MSGTR_AO_SDL_CantInit "[AO SDL] �chec initialiation audio SDL: %s\n"
#define MSGTR_AO_SDL_CantOpenAudio "[AO SDL] Impossible ouvrir audio: %s\n"

// ao_sgi.c
#define MSGTR_AO_SGI_INFO "[AO SGI] contr�le.\n"
#define MSGTR_AO_SGI_InitInfo "[AO SGI] init: �chantillonnage: %iHz Canaux: %s Format %s\n"
#define MSGTR_AO_SGI_InvalidDevice "[AO SGI] lecture: p�riph�rique invalide.\n"
#define MSGTR_AO_SGI_CantSetParms_Samplerate "[AO SGI] init: �chec setparams: %s\nImpossible r�gler �chantillonnage d�sir�.\n"
#define MSGTR_AO_SGI_CantSetAlRate "[AO SGI] init: AL_RATE non accept�e sur la ressource donn�e.\n"
#define MSGTR_AO_SGI_CantGetParms "[AO SGI] init: �chec getparams: %s\n"
#define MSGTR_AO_SGI_SampleRateInfo "[AO SGI] init: �chantillonnage maintenant %lf (taux d�sir�: %lf)\n"
#define MSGTR_AO_SGI_InitConfigError "[AO SGI] init: %s\n"
#define MSGTR_AO_SGI_InitOpenAudioFailed "[AO SGI] init: Impossible ouvrir canal audio: %s\n"
#define MSGTR_AO_SGI_Uninit "[AO SGI] desinit: ...\n"
#define MSGTR_AO_SGI_Reset "[AO SGI] repart: ...\n"
#define MSGTR_AO_SGI_PauseInfo "[AO SGI] pause_audio: ...\n"
#define MSGTR_AO_SGI_ResumeInfo "[AO SGI] repart_audio: ...\n"

// ao_sun.c
#define MSGTR_AO_SUN_RtscSetinfoFailed "[AO SUN] rtsc: �chec SETINFO.\n"
#define MSGTR_AO_SUN_RtscWriteFailed "[AO SUN] rtsc: �chec �criture.\n"
#define MSGTR_AO_SUN_CantOpenAudioDev "[AO SUN] Impossible ouvrir p�riph�rique audio %s, %s  -> aucun son.\n"
#define MSGTR_AO_SUN_UnsupSampleRate "[AO SUN] audio_setup: votre carte ne supporte pas canal %d, %s, %d Hz �chantillonnage.\n"
#define MSGTR_AO_SUN_CantUseSelect "[AO SUN]\n   ***  Votre pilote audio ne supporte PAS select()  ***\nRecompiler MPlayer avec #undef HAVE_AUDIO_SELECT dans config.h !\n\n"
#define MSGTR_AO_SUN_CantReopenReset "[AO SUN]\n�rreur fatale: *** IMPOSSIBLE R�OUVRIR/REPARTIR P�RIPH�RIQUE AUDIO (%s) ***\n"

// ao_alsa5.c
#define MSGTR_AO_ALSA5_InitInfo "[AO ALSA5] alsa-init: format requis: %d Hz, %d canaux, %s\n"
#define MSGTR_AO_ALSA5_SoundCardNotFound "[AO ALSA5] alsa-init: nulle carte son trouv�e.\n"
#define MSGTR_AO_ALSA5_InvalidFormatReq "[AO ALSA5] alsa-init: format invalide (%s) requis - sortie d�sactiv�e.\n"
#define MSGTR_AO_ALSA5_PlayBackError "[AO ALSA5] alsa-init: erreur ouverture lecture: %s\n"
#define MSGTR_AO_ALSA5_PcmInfoError "[AO ALSA5] alsa-init: erreur pcm info: %s\n"
#define MSGTR_AO_ALSA5_SoundcardsFound "[AO ALSA5] alsa-init: %d carte(s) son trouv�e(s), utilise: %s\n"
#define MSGTR_AO_ALSA5_PcmChanInfoError "[AO ALSA5] alsa-init: erreur info canal pcm: %s\n"
#define MSGTR_AO_ALSA5_CantSetParms "[AO ALSA5] alsa-init: erreur parametrage: %s\n"
#define MSGTR_AO_ALSA5_CantSetChan "[AO ALSA5] alsa-init: erreur ouverture canal: %s\n"
#define MSGTR_AO_ALSA5_ChanPrepareError "[AO ALSA5] alsa-init: erreur pr�paration canal: %s\n"
#define MSGTR_AO_ALSA5_DrainError "[AO ALSA5] alsa-uninit: erreur drain de lecture: %s\n"
#define MSGTR_AO_ALSA5_FlushError "[AO ALSA5] alsa-uninit: erreur vidage de lecture: %s\n"
#define MSGTR_AO_ALSA5_PcmCloseError "[AO ALSA5] alsa-uninit: erreur fermeture pcm: %s\n"
#define MSGTR_AO_ALSA5_ResetDrainError "[AO ALSA5] alsa-reset: erreur drain de lecture: %s\n"
#define MSGTR_AO_ALSA5_ResetFlushError "[AO ALSA5] alsa-reset: erreur vidage de lecture: %s\n"
#define MSGTR_AO_ALSA5_ResetChanPrepareError "[AO ALSA5] alsa-reset: erreur pr�paration canal: %s\n"
#define MSGTR_AO_ALSA5_PauseDrainError "[AO ALSA5] alsa-pause: erreur drain de lecture: %s\n"
#define MSGTR_AO_ALSA5_PauseFlushError "[AO ALSA5] alsa-pause: erreur vidage de lecture: %s\n"
#define MSGTR_AO_ALSA5_ResumePrepareError "[AO ALSA5] alsa-resume: erreur pr�paration canal: %s\n"
#define MSGTR_AO_ALSA5_Underrun "[AO ALSA5] alsa-play: sous-passement alsa, r�init flux.\n"
#define MSGTR_AO_ALSA5_PlaybackPrepareError "[AO ALSA5] alsa-play: erreur pr�paration lecture: %s\n"
#define MSGTR_AO_ALSA5_WriteErrorAfterReset "[AO ALSA5] alsa-play: erreur �criture apr�s r�init: %s - abandon.\n"
#define MSGTR_AO_ALSA5_OutPutError "[AO ALSA5] alsa-play: erreur de sortie: %s\n"

// ao_plugin.c

#define MSGTR_AO_PLUGIN_InvalidPlugin "[AO PLUGIN] plugiciel invalide: %s\n"

// ======================= AF Audio Filters ================================

// libaf 

// af_ladspa.c

#define MSGTR_AF_LADSPA_AvailableLabels "labels disponibles dans"
#define MSGTR_AF_LADSPA_WarnNoInputs "GARE! Plugiciel LADSPA sans entr� audio.\n  Le signal entr�e audio sera perdu."
#define MSGTR_AF_LADSPA_ErrMultiChannel "Plugiciels multi-canal (>2) non (encore) support�s.\n  Utiliser plugiciels mono ou st�r�o."
#define MSGTR_AF_LADSPA_ErrNoOutputs "Plugiciel LADSPA sans sortie audio."
#define MSGTR_AF_LADSPA_ErrInOutDiff "D�saccord entre le nombre d'entr�es et de sorties audio du plugiciel LADSPA."
#define MSGTR_AF_LADSPA_ErrFailedToLoad "�chec de chargement"
#define MSGTR_AF_LADSPA_ErrNoDescriptor "Fonction ladspa_descriptor() introuvable dans fichier lib sp�cifi�."
#define MSGTR_AF_LADSPA_ErrLabelNotFound "Label introuvable dans lib du plugiciel."
#define MSGTR_AF_LADSPA_ErrNoSuboptions "Nulle sous-option sp�cifi�e"
#define MSGTR_AF_LADSPA_ErrNoLibFile "Nul fichier lib sp�cifi�"
#define MSGTR_AF_LADSPA_ErrNoLabel "Nul label de filtre sp�cifi�"
#define MSGTR_AF_LADSPA_ErrNotEnoughControls "Pas assez de contr�les sp�cifi�s sur ligne de commande"
#define MSGTR_AF_LADSPA_ErrControlBelow "%s: Contr�le d'entr� #%d sous limite inf�rieure de %0.4f.\n"
#define MSGTR_AF_LADSPA_ErrControlAbove "%s: Contr�le d'entr� #%d sous limite sup�rieure de %0.4f.\n"

// format.c

#define MSGTR_AF_FORMAT_UnknownFormat "format inconnu "

// ========================== INPUT =========================================

// joystick.c

#define MSGTR_INPUT_JOYSTICK_Opening "Ouverture p�riph�rique manette de jeux %s\n"
#define MSGTR_INPUT_JOYSTICK_CantOpen "Impossible ouvrir p�riph�rique manette de jeux %s: %s\n"
#define MSGTR_INPUT_JOYSTICK_ErrReading "Erreur lecture p�riph�rique manette de jeux: %s\n"
#define MSGTR_INPUT_JOYSTICK_LoosingBytes "Manette de jeux: perdons %d bytes de donn�es\n"
#define MSGTR_INPUT_JOYSTICK_WarnLostSync "Manette de jeux: alerte �vennement init, perdu sync avec pilote\n"
#define MSGTR_INPUT_JOYSTICK_WarnUnknownEvent "Alerte manette de jeux �v�nement inconnu de type %d\n"

// input.c

#define MSGTR_INPUT_INPUT_ErrCantRegister2ManyCmdFds "Trop de descripteurs de fichiers de commande, Impossible enr�gister descripteur fichier %d.\n"
#define MSGTR_INPUT_INPUT_ErrCantRegister2ManyKeyFds "Trop de descripteurs de fichiers touche, Impossible enr�gister descripteur fichier %d.\n"
#define MSGTR_INPUT_INPUT_ErrArgMustBeInt "Commande %s: argument %d pas un nombre entier.\n"
#define MSGTR_INPUT_INPUT_ErrArgMustBeFloat "Commande %s: argument %d pas un nombre r�el.\n"
#define MSGTR_INPUT_INPUT_ErrUnterminatedArg "Commande %s: argument %d non termin�.\n"
#define MSGTR_INPUT_INPUT_ErrUnknownArg "Argument inconnu %d\n"
#define MSGTR_INPUT_INPUT_Err2FewArgs "Commande %s requiert au moins %d arguments, trouv� seulement %d.\n"
#define MSGTR_INPUT_INPUT_ErrReadingCmdFd "Erreur lecture descripteur fichier commande %d: %s\n"
#define MSGTR_INPUT_INPUT_ErrCmdBufferFullDroppingContent "Tampon de commande du descripteur de fichier %d plein: omet contenu\n"
#define MSGTR_INPUT_INPUT_ErrInvalidCommandForKey "Commande invalide pour touche li�e %s"
#define MSGTR_INPUT_INPUT_ErrSelect "Erreur s�lection: %s\n"
#define MSGTR_INPUT_INPUT_ErrOnKeyInFd "Erreur sur descripteur de fichier entr� touche %d\n"
#define MSGTR_INPUT_INPUT_ErrDeadKeyOnFd "Entr� couche morte sur descripteur fichier %d\n"
#define MSGTR_INPUT_INPUT_Err2ManyKeyDowns "Trop �v�nements touche appuy� en m�me temps\n"
#define MSGTR_INPUT_INPUT_ErrOnCmdFd "Erreur sur descripteur fichier commande %d\n"
#define MSGTR_INPUT_INPUT_ErrReadingInputConfig "Erreur lecture fichier config entr� %s: %s\n"
#define MSGTR_INPUT_INPUT_ErrUnknownKey "Cl� inconnue '%s'\n"
#define MSGTR_INPUT_INPUT_ErrUnfinishedBinding "Liaison non termin�e %s\n"
#define MSGTR_INPUT_INPUT_ErrBuffer2SmallForKeyName "Tampon trop petit pour nom de touche: %s\n"
#define MSGTR_INPUT_INPUT_ErrNoCmdForKey "Aucune commande trouv�e pour touche %s"
#define MSGTR_INPUT_INPUT_ErrBuffer2SmallForCmd "Tampon trop petit pour commande %s\n"
#define MSGTR_INPUT_INPUT_ErrWhyHere "Que faisons-nous ici?\n"
#define MSGTR_INPUT_INPUT_ErrCantInitJoystick "Impossible initier manette entr�e\n"
#define MSGTR_INPUT_INPUT_ErrCantStatFile "Impossible lire %s: %s\n"
#define MSGTR_INPUT_INPUT_ErrCantOpenFile "Impossible ouvrir %s: %s\n"

// ========================== LIBMPDEMUX ===================================

// url.c

#define MSGTR_MPDEMUX_URL_StringAlreadyEscaped "Cha�ne semble d�j� �chapp�e dans url_escape %c%c1%c2\n"

// ai_alsa1x.c

#define MSGTR_MPDEMUX_AIALSA1X_CannotSetSamplerate "Impossible r�ger taux �chantillon\n"
#define MSGTR_MPDEMUX_AIALSA1X_CannotSetBufferTime "Impossible r�gler heure tampon\n"
#define MSGTR_MPDEMUX_AIALSA1X_CannotSetPeriodTime "Impossible r�gler heure p�riode\n"

// ai_alsa1x.c / ai_alsa.c

#define MSGTR_MPDEMUX_AIALSA_PcmBrokenConfig "Configuration bris�e pour ce PCM: nulle configuration disponible\n"
#define MSGTR_MPDEMUX_AIALSA_UnavailableAccessType "Type acc�s non disponible\n"
#define MSGTR_MPDEMUX_AIALSA_UnavailableSampleFmt "Format �chantillon non disponible\n"
#define MSGTR_MPDEMUX_AIALSA_UnavailableChanCount "Compte de canaux non disp. - retour � valeur d�faut: %d\n"
#define MSGTR_MPDEMUX_AIALSA_CannotInstallHWParams "Impossible installer params hw: %s"
#define MSGTR_MPDEMUX_AIALSA_PeriodEqualsBufferSize "Impossible utiliser p�riode �gale � grandeur tampon (%u == %lu)\n"
#define MSGTR_MPDEMUX_AIALSA_CannotInstallSWParams "Impossible installer params sw:n"
#define MSGTR_MPDEMUX_AIALSA_ErrorOpeningAudio "Erreur ouverture audio: %s\n"
#define MSGTR_MPDEMUX_AIALSA_AlsaStatusError "Erreur statut ALSA: %s"
#define MSGTR_MPDEMUX_AIALSA_AlsaXRUN "ALSA xrun!!! (au moins %.3f ms long)\n"
#define MSGTR_MPDEMUX_AIALSA_AlsaStatus "Statut ALSA:\n"
#define MSGTR_MPDEMUX_AIALSA_AlsaXRUNPrepareError "ALSA xrun: erreur pr�paration: %s"
#define MSGTR_MPDEMUX_AIALSA_AlsaReadWriteError "Erreur �crit/lect ALSA"

// ai_oss.c

#define MSGTR_MPDEMUX_AIOSS_Unable2SetChanCount "Impossible mettre compte canaux: %d\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2SetStereo "Impossible mettre st�r�o: %d\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2Open "Impossible ouvrir '%s': %s\n"
#define MSGTR_MPDEMUX_AIOSS_UnsupportedFmt "Format non support�\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2SetAudioFmt "Impossible mettre format audio."
#define MSGTR_MPDEMUX_AIOSS_Unable2SetSamplerate "Impossible mettre taux �chantillon: %d\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2SetTrigger "Impossible mettre d�clencheur: %d\n"
#define MSGTR_MPDEMUX_AIOSS_Unable2GetBlockSize "Impossible obtenir grandeur bloc!\n"
#define MSGTR_MPDEMUX_AIOSS_AudioBlockSizeZero "Grandeur bloc audio z�ro, met � %d!\n"
#define MSGTR_MPDEMUX_AIOSS_AudioBlockSize2Low "Grandeur bloc audio trop bas, met � %d!\n"

// asfheader.c

#define MSGTR_MPDEMUX_ASFHDR_HeaderSizeOver1MB "FATAL: grandeur ent�te plus grande que 1 MB (%d)!\nContacter auteurs MPlayer, et t�l�charge/envoie ce fichier.\n"
#define MSGTR_MPDEMUX_ASFHDR_HeaderMallocFailed "Impossible allouer %d octets pour ent�te\n"
#define MSGTR_MPDEMUX_ASFHDR_EOFWhileReadingHeader "EOF en lisant ent�te asf, fichier bris�/incomplet?\n"
#define MSGTR_MPDEMUX_ASFHDR_DVRWantsLibavformat "DVR pourrait fonctionner seulement avec libavformat, en cas probl�me, essayer -demuxer 35\n"
#define MSGTR_MPDEMUX_ASFHDR_NoDataChunkAfterHeader "Nul morceau donn�es suit ent�te!\n"
#define MSGTR_MPDEMUX_ASFHDR_AudioVideoHeaderNotFound "ASF: nul ent�te audio ou vid�o trouv� - fichier bris�?\n"
#define MSGTR_MPDEMUX_ASFHDR_InvalidLengthInASFHeader "Longueur ent�te ASF invalide!\n"

// asf_mmst_streaming.c

#define MSGTR_MPDEMUX_MMST_WriteError "Erreur �criture\n"
#define MSGTR_MPDEMUX_MMST_EOFAlert "\nAlerte! EOF\n"
#define MSGTR_MPDEMUX_MMST_PreHeaderReadFailed "�chec lecture pr�-ent�te\n"
#define MSGTR_MPDEMUX_MMST_InvalidHeaderSize "Grandeur ent�te invalide, abandon\n"
#define MSGTR_MPDEMUX_MMST_HeaderDataReadFailed "�chec lecture donn�es ent�te\n"
#define MSGTR_MPDEMUX_MMST_packet_lenReadFailed "�chec lecture packet_len\n"
#define MSGTR_MPDEMUX_MMST_InvalidRTSPPacketSize "Grandeur paquet rtsp invalide, abandon\n"
#define MSGTR_MPDEMUX_MMST_CmdDataReadFailed "�chec lecture donn�es commande\n"
#define MSGTR_MPDEMUX_MMST_HeaderObject "Objet ent�te\n"
#define MSGTR_MPDEMUX_MMST_DataObject "Objet donn�es\n"
#define MSGTR_MPDEMUX_MMST_FileObjectPacketLen "Objet fichier, longueur paquet = %d (%d)\n"
#define MSGTR_MPDEMUX_MMST_StreamObjectStreamID "Objet flux, id flux: %d\n"
#define MSGTR_MPDEMUX_MMST_2ManyStreamID "trop de id, flux saut�"
#define MSGTR_MPDEMUX_MMST_UnknownObject "Objet inconnu\n"
#define MSGTR_MPDEMUX_MMST_MediaDataReadFailed "�chec lecture donn�es m�dia\n"
#define MSGTR_MPDEMUX_MMST_MissingSignature "Signature manquante\n"
#define MSGTR_MPDEMUX_MMST_PatentedTechnologyJoke "Tout est fait. Merci pour t�l�chargement de fichier contenant technologie propri�taire et patent�.\n"
#define MSGTR_MPDEMUX_MMST_UnknownCmd "commande inconnue %02x\n"
#define MSGTR_MPDEMUX_MMST_GetMediaPacketErr "erreur get_media_packet : %s\n"
#define MSGTR_MPDEMUX_MMST_Connected "Connect�\n"

// asf_streaming.c

#define MSGTR_MPDEMUX_ASF_StreamChunkSize2Small "Ahhhh, grandeur bloc flux trop petite: %d\n"
#define MSGTR_MPDEMUX_ASF_SizeConfirmMismatch "d�saccord confirme grandeur!: %d %d\n"
#define MSGTR_MPDEMUX_ASF_WarnDropHeader "Alerte : omet ent�te ????\n"
#define MSGTR_MPDEMUX_ASF_ErrorParsingChunkHeader "�chec analyse ent�te morceau\n"
#define MSGTR_MPDEMUX_ASF_NoHeaderAtFirstChunk "Nul ent�te comme premier morceau !!!!\n"
#define MSGTR_MPDEMUX_ASF_BufferMallocFailed "Erreur ne peux allouer tampon %d octets\n"
#define MSGTR_MPDEMUX_ASF_ErrReadingNetworkStream "Erreur lecture flux r�seau\n"
#define MSGTR_MPDEMUX_ASF_ErrChunk2Small "Erreur morceau trop petit\n"
#define MSGTR_MPDEMUX_ASF_ErrSubChunkNumberInvalid "Erreur nombre sous-morceaus invalide\n"
#define MSGTR_MPDEMUX_ASF_Bandwidth2SmallCannotPlay "Bande passante trop petite, ne peux lire fichier!\n"
#define MSGTR_MPDEMUX_ASF_Bandwidth2SmallDeselectedAudio "Bande passante trop petite, flux audio d�s�lectionn�\n"
#define MSGTR_MPDEMUX_ASF_Bandwidth2SmallDeselectedVideo "Bande passante trop petite, flux vid�o d�s�lectionn�\n"
#define MSGTR_MPDEMUX_ASF_InvalidLenInHeader "Longueur ent�te ASF invalide!\n"
#define MSGTR_MPDEMUX_ASF_ErrReadingChunkHeader "Erreur lecture ent�te morceau\n"
#define MSGTR_MPDEMUX_ASF_ErrChunkBiggerThanPacket "Erreur grandeur morceau > grandeur paquet\n"
#define MSGTR_MPDEMUX_ASF_ErrReadingChunk "Erreur lecture morceau\n"
#define MSGTR_MPDEMUX_ASF_ASFRedirector "=====> Redirecteur ASF\n"
#define MSGTR_MPDEMUX_ASF_InvalidProxyURL "Proxy URL invalide\n"
#define MSGTR_MPDEMUX_ASF_UnknownASFStreamType "Genre de flux asf inconnu\n"
#define MSGTR_MPDEMUX_ASF_Failed2ParseHTTPResponse "�chec analyse r�ponse HTTP\n"
#define MSGTR_MPDEMUX_ASF_ServerReturn "Retour de serveur %d:%s\n"
#define MSGTR_MPDEMUX_ASF_ASFHTTPParseWarnCuttedPragma "ALERTE ANALYSE ASF HTTP: Pragma %s coup� de %d octets � %d\n"
#define MSGTR_MPDEMUX_ASF_SocketWriteError "Erreur lecture interface (socket): %s\n"
#define MSGTR_MPDEMUX_ASF_HeaderParseFailed "�chec analyse ent�te\n"
#define MSGTR_MPDEMUX_ASF_NoStreamFound "nul flux trouv�\n"
#define MSGTR_MPDEMUX_ASF_UnknownASFStreamingType "genre flux ASF inconnu\n"
#define MSGTR_MPDEMUX_ASF_InfoStreamASFURL "STREAM_ASF, URL: %s\n"
#define MSGTR_MPDEMUX_ASF_StreamingFailed "�chec, abandon\n"

// audio_in.c

#define MSGTR_MPDEMUX_AUDIOIN_ErrReadingAudio "\nErreur lecture audio: %s\n"
#define MSGTR_MPDEMUX_AUDIOIN_XRUNSomeFramesMayBeLeftOut "R�tabli de cross-run, quelques images pourraient �tre �chapp�es!\n"
#define MSGTR_MPDEMUX_AUDIOIN_ErrFatalCannotRecover "Erreur fatale, impossible de se r�tablir!\n"
#define MSGTR_MPDEMUX_AUDIOIN_NotEnoughSamples "\nNombre insuffisant �chantillons audio!\n"

// aviheader.c

#define MSGTR_MPDEMUX_AVIHDR_EmptyList "** liste vide?!\n"
#define MSGTR_MPDEMUX_AVIHDR_FoundMovieAt "Trouv� film � 0x%X - 0x%X\n"
#define MSGTR_MPDEMUX_AVIHDR_FoundBitmapInfoHeader "Trouv� 'bih', %u octets de %d\n"
#define MSGTR_MPDEMUX_AVIHDR_RegeneratingKeyfTableForMPG4V1 "Regeneration de table image cl� pour vid�o M$ mpg4v1\n"
#define MSGTR_MPDEMUX_AVIHDR_RegeneratingKeyfTableForDIVX3 "Regeneration de table image cl� pour DIVX3 video\n"
#define MSGTR_MPDEMUX_AVIHDR_RegeneratingKeyfTableForMPEG4 "Regeneration de table image cl� pour MPEG4 video\n"
#define MSGTR_MPDEMUX_AVIHDR_FoundWaveFmt "Trouv� 'wf', %d octets de %d\n"
#define MSGTR_MPDEMUX_AVIHDR_FoundAVIV2Header "AVI: dmlh trouv� (grandeur=%d) (total images=%d)\n"
#define MSGTR_MPDEMUX_AVIHDR_ReadingIndexBlockChunksForFrames "Lecture morceau INDEX, %d morceaux pour %d images (fpos=%"PRId64")\n"
#define MSGTR_MPDEMUX_AVIHDR_AdditionalRIFFHdr "Ent�te RIFF additionnel...\n"
#define MSGTR_MPDEMUX_AVIHDR_WarnNotExtendedAVIHdr "** alerte: pas un ent�te AVI �tendu..\n"
#define MSGTR_MPDEMUX_AVIHDR_BrokenChunk "Morceau bris�?  grandeur morceau=%d  (id=%.4s)\n"
#define MSGTR_MPDEMUX_AVIHDR_BuildingODMLidx "AVI: ODML: Construction index odml (%d super moreaux index)\n"
#define MSGTR_MPDEMUX_AVIHDR_BrokenODMLfile "AVI: ODML: Fichier bris� (incomplet?) d�tect�. Utilise index traditionnel\n"
#define MSGTR_MPDEMUX_AVIHDR_CantReadIdxFile "Impossible lire fichier index %s: %s\n"
#define MSGTR_MPDEMUX_AVIHDR_NotValidMPidxFile "%s n'est pas un fichier index MPlayer valide\n"
#define MSGTR_MPDEMUX_AVIHDR_FailedMallocForIdxFile "Impossible allouer m�moire pour donn�es index de %s\n"
#define MSGTR_MPDEMUX_AVIHDR_PrematureEOF "Fin de fichier index inattendu %s\n"
#define MSGTR_MPDEMUX_AVIHDR_IdxFileLoaded "Fichier index charg�: %s\n"
#define MSGTR_MPDEMUX_AVIHDR_GeneratingIdx "G�n�ration Index: %3lu %s     \r"
#define MSGTR_MPDEMUX_AVIHDR_IdxGeneratedForHowManyChunks "AVI: table index g�n�r�e pour %d morceaux!\n"
#define MSGTR_MPDEMUX_AVIHDR_Failed2WriteIdxFile "Impossible �crire fichier index %s: %s\n"
#define MSGTR_MPDEMUX_AVIHDR_IdxFileSaved "Sauvegard� fichier index: %s\n"

// cache2.c

#define MSGTR_MPDEMUX_CACHE2_NonCacheableStream "\rFlux non enr�gistrable en m�moire cache.\n"
#define MSGTR_MPDEMUX_CACHE2_ReadFileposDiffers "!!! diff lecture position fichier!!! rapporter ce bogue...\n"

// cdda.c

#define MSGTR_MPDEMUX_CDDA_CantOpenCDDADevice "Impossible ouvrir p�riph�rique CDDA.\n"
#define MSGTR_MPDEMUX_CDDA_CantOpenDisc "Impossible ouvrir disque.\n"
#define MSGTR_MPDEMUX_CDDA_AudioCDFoundWithNTracks "trouv� CD audio avec %ld pistes.\n"

// cddb.c

#define MSGTR_MPDEMUX_CDDB_FailedToReadTOC "�chec lecture TDM.\n"
#define MSGTR_MPDEMUX_CDDB_FailedToOpenDevice "�chec ouverture p�riph�rique %s.\n"
#define MSGTR_MPDEMUX_CDDB_NotAValidURL "URL non valide\n"
#define MSGTR_MPDEMUX_CDDB_FailedToSendHTTPRequest "�chec envoie requ�te HTTP.\n"
#define MSGTR_MPDEMUX_CDDB_FailedToReadHTTPResponse "�chec lecture r�ponse HTTP.\n"
#define MSGTR_MPDEMUX_CDDB_HTTPErrorNOTFOUND "Non trouv�.\n"
#define MSGTR_MPDEMUX_CDDB_HTTPErrorUnknown "Code erreur inconnu\n"
#define MSGTR_MPDEMUX_CDDB_NoCacheFound "Nul tampon trouv�.\n"
#define MSGTR_MPDEMUX_CDDB_NotAllXMCDFileHasBeenRead "Lecture incompl�te de fichier xmcd.\n"
#define MSGTR_MPDEMUX_CDDB_FailedToCreateDirectory "�chec cr�ation r�pertoire %s.\n"
#define MSGTR_MPDEMUX_CDDB_NotAllXMCDFileHasBeenWritten "�criture incompl�te de fichier xmcd.\n"
#define MSGTR_MPDEMUX_CDDB_InvalidXMCDDatabaseReturned "Retour invalide de fichier base de donn�es xmcd.\n"
#define MSGTR_MPDEMUX_CDDB_UnexpectedFIXME "FIXME inattendu\n"
#define MSGTR_MPDEMUX_CDDB_UnhandledCode "Code non g�r�\n"
#define MSGTR_MPDEMUX_CDDB_UnableToFindEOL "Impossible trouver fin de ligne\n"
#define MSGTR_MPDEMUX_CDDB_ParseOKFoundAlbumTitle "Analyse OK, trouv�: %s\n"
#define MSGTR_MPDEMUX_CDDB_AlbumNotFound "Album non trouv�\n"
#define MSGTR_MPDEMUX_CDDB_ServerReturnsCommandSyntaxErr "R�ponse serveur: Erreur syntaxe commande\n"
#define MSGTR_MPDEMUX_CDDB_NoSitesInfoAvailable "Nulle information sites disponible\n"
#define MSGTR_MPDEMUX_CDDB_FailedToGetProtocolLevel "�chec obtention niveau de protocol\n"
#define MSGTR_MPDEMUX_CDDB_NoCDInDrive "Nul CD dans lecteur\n"

// cue_read.c

#define MSGTR_MPDEMUX_CUEREAD_UnexpectedCuefileLine "[bincue] Ligne de fichier signal inattendue: %s\n"
#define MSGTR_MPDEMUX_CUEREAD_BinFilenameTested "[bincue] nom fichier bin test�: %s\n"
#define MSGTR_MPDEMUX_CUEREAD_CannotFindBinFile "[bincue] Impossible trouver fichier bin - abandon\n"
#define MSGTR_MPDEMUX_CUEREAD_UsingBinFile "[bincue] Utilise fichier bin %s\n"
#define MSGTR_MPDEMUX_CUEREAD_UnknownModeForBinfile "[bincue] mode inconnu pour fichier bin. Improbable. Fin.\n"
#define MSGTR_MPDEMUX_CUEREAD_CannotOpenCueFile "[bincue] Impossible ouvrir %s\n"
#define MSGTR_MPDEMUX_CUEREAD_ErrReadingFromCueFile "[bincue] Erreur lecture %s\n"
#define MSGTR_MPDEMUX_CUEREAD_ErrGettingBinFileSize "[bincue] Erreur lecture grandeur fichier bin\n"
#define MSGTR_MPDEMUX_CUEREAD_InfoTrackFormat "piste %02d:  format=%d  %02d:%02d:%02d\n"
#define MSGTR_MPDEMUX_CUEREAD_UnexpectedBinFileEOF "[bincue] fin inattendue de fichier bin\n"
#define MSGTR_MPDEMUX_CUEREAD_CannotReadNBytesOfPayload "[bincue] Impossible lire %d octets de donn�es\n"
#define MSGTR_MPDEMUX_CUEREAD_CueStreamInfo_FilenameTrackTracksavail "Signal flux ouvert, nom fichier=%s, piste=%d, pistes disponibles: %d -> %d\n"

// network.c

#define MSGTR_MPDEMUX_NW_UnknownAF "Famille d'adresses inconnue %d\n"
#define MSGTR_MPDEMUX_NW_ResolvingHostForAF "Solution de %s pour %s...\n"
#define MSGTR_MPDEMUX_NW_CantResolv "Impossible trouver nom pour %s: %s\n"
#define MSGTR_MPDEMUX_NW_ConnectingToServer "Connexion au serveur %s[%s]: %d...\n"
#define MSGTR_MPDEMUX_NW_CantConnect2Server "�chec connexion au serveur avec %s\n"
#define MSGTR_MPDEMUX_NW_SelectFailed "�chec s�lection.\n"
#define MSGTR_MPDEMUX_NW_ConnTimeout "D�passement de temps pour connecter.\n"
#define MSGTR_MPDEMUX_NW_GetSockOptFailed "�chec getsockopt: %s\n"
#define MSGTR_MPDEMUX_NW_ConnectError "Erreur de connection: %s\n"
#define MSGTR_MPDEMUX_NW_InvalidProxySettingTryingWithout "R�glage proxy invalide... Essaie sans proxy.\n"
#define MSGTR_MPDEMUX_NW_CantResolvTryingWithoutProxy "Impossible r�soudre nom h�te distant pour AF_INET. Essaie sans proxy.\n"
#define MSGTR_MPDEMUX_NW_ErrSendingHTTPRequest "Erreur lors envoie requ�te HTTP: envoie incompl�t.\n"
#define MSGTR_MPDEMUX_NW_ReadFailed "�chec lecture.\n"
#define MSGTR_MPDEMUX_NW_Read0CouldBeEOF "http_read_response = 0 (i.e. EOF)\n"
#define MSGTR_MPDEMUX_NW_AuthFailed "�chec authentication. Utiliser options -user et -passwd pour donner votre\n"\
"nom_utilisateur/mot_passe pour une liste de URLs, ou donner un URL tel que:\n"\
"http://nom_utilisateur:mot_passe@nom_h�te/fichier\n"
#define MSGTR_MPDEMUX_NW_AuthRequiredFor "Authentication requise pour %s\n"
#define MSGTR_MPDEMUX_NW_AuthRequired "Authentication requise.\n"
#define MSGTR_MPDEMUX_NW_NoPasswdProvidedTryingBlank "Nul mot_passe fourni, essaie mot_passe vide.\n"
#define MSGTR_MPDEMUX_NW_ErrServerReturned "Serveur retourne %d: %s\n"
#define MSGTR_MPDEMUX_NW_CacheSizeSetTo "Grandeur cache r�gl� � %d K octets\n"

// ========================== LIBMPMENU ===================================

// libmenu/menu.c
#define MSGTR_LIBMENU_SyntaxErrorAtLine "[MENU] Erreur syntaxe ligne: %d\n"
#define MSGTR_LIBMENU_MenuDefinitionsNeedANameAttrib "[MENU] D�finitions de menu exigent attribut de nom (ligne %d)\n"
#define MSGTR_LIBMENU_BadAttrib "[MENU] Mauvais attribut %s=%s dans menu '%s', ligne %d\n"
#define MSGTR_LIBMENU_UnknownMenuType "[MENU] Genre menu inconnu '%s' ligne %d\n"
#define MSGTR_LIBMENU_CantOpenConfigFile "[MENU] Ne peux ouvrir fichier config menu: %s\n"
#define MSGTR_LIBMENU_ConfigFileIsTooBig "[MENU] Fichier config trop gros (> %d KO)\n"
#define MSGTR_LIBMENU_ConfigFileIsEmpty "[MENU] Fichier config vide\n"
#define MSGTR_LIBMENU_MenuNotFound "[MENU] Menu %s non trouv�.\n"
#define MSGTR_LIBMENU_MenuInitFailed "[MENU] Menu '%s': �chec init\n"
#define MSGTR_LIBMENU_UnsupportedOutformat "[MENU] Format de sortie non support�!!!!\n"

// libmenu/menu_cmdlist.c
#define MSGTR_LIBMENU_NoEntryFoundInTheMenuDefinition "[MENU] Nulle entr�e trouv�e dans d�finition menu.\n"
#define MSGTR_LIBMENU_ListMenuEntryDefinitionsNeedAName "[MENU] Besoin de nom pour d�finitions entr�e menu liste (ligne %d).\n"
#define MSGTR_LIBMENU_ListMenuNeedsAnArgument "[MENU] menu liste exige argument.\n"

// libmenu/menu_console.c
#define MSGTR_LIBMENU_WaitPidError "[MENU] Erreur attente identificateur processus: %s.\n"
#define MSGTR_LIBMENU_SelectError "[MENU] Erreur s�lection.\n"
#define MSGTR_LIBMENU_ReadErrorOnChilds "[MENU] Erreur lecture sur processus enfant: %s.\n"
#define MSGTR_LIBMENU_ConsoleRun "[MENU] Console run: %s ...\n"
#define MSGTR_LIBMENU_AChildIsAlreadyRunning "[MENU] Processus enfant d�j� en cours.\n"
#define MSGTR_LIBMENU_ForkFailed "[MENU] �chec branchement !!!\n"
#define MSGTR_LIBMENU_WriteError "[MENU] Erreur �criture.\n"

// libmenu/menu_filesel.c
#define MSGTR_LIBMENU_OpendirError "[MENU] Erreur ouverture r�pertoire: %s.\n"
#define MSGTR_LIBMENU_ReallocError "[MENU] Erreur r�allocation m�moire: %s.\n"
#define MSGTR_LIBMENU_MallocError "[MENU] Erreur allocation m�moire: %s.\n"
#define MSGTR_LIBMENU_ReaddirError "[MENU] Erreur lecture r�pertoire: %s.\n"
#define MSGTR_LIBMENU_CantOpenDirectory "[MENU] Ne peux ouvrir r�pertoire %s\n"

// libmenu/menu_param.c
#define MSGTR_LIBMENU_NoEntryFoundInTheMenuDefinition "[MENU] Nulle entr�e trouv�e dans d�finition menu.\n"
#define MSGTR_LIBMENU_SubmenuDefinitionNeedAMenuAttribut "[MENU] D�finition sous-menu exige attribut 'menu'.\n"
#define MSGTR_LIBMENU_PrefMenuEntryDefinitionsNeed "[MENU] D�finition entr� menu pref exige attribut 'propri�t�' valide (ligne %d).\n"
#define MSGTR_LIBMENU_PrefMenuNeedsAnArgument "[MENU] Menu pref exige argument.\n"

// libmenu/menu_pt.c
#define MSGTR_LIBMENU_CantfindTheTargetItem "[MENU] Ne peux trouver item cible ????\n"
#define MSGTR_LIBMENU_FailedToBuildCommand "[MENU] Echec composition commande: %s.\n"

// libmenu/menu_txt.c
#define MSGTR_LIBMENU_MenuTxtNeedATxtFileName "[MENU] Menu texte exibe nom fichier txt (fichier param).\n"
#define MSGTR_LIBMENU_MenuTxtCantOpen "[MENU] Ne peux ouvrir: %s.\n"
#define MSGTR_LIBMENU_WarningTooLongLineSplitting "[MENU] Alerte, ligne trop longue. Je la coupe.\n"
#define MSGTR_LIBMENU_ParsedLines "[MENU] %d lignes analis�es.\n"

// libmenu/vf_menu.c
#define MSGTR_LIBMENU_UnknownMenuCommand "[MENU] Commande inconnue: '%s'.\n"
#define MSGTR_LIBMENU_FailedToOpenMenu "[MENU] �chec ouverture menu: '%s'.\n"

// ========================== LIBMPCODECS ===================================

// libmpcodecs/ad_libdv.c
#define MSGTR_MPCODECS_AudioFramesizeDiffers "[AD_LIBDV] Alerte! Diff�rence grandeur trame audio ! lu=%d  hdr=%d.\n"

// libmpcodecs/vd_dmo.c vd_dshow.c vd_vfw.c
#define MSGTR_MPCODECS_CouldntAllocateImageForCinepakCodec "[VD_DMO] Impossible allouer image pour codec cinepak.\n"

// libmpcodecs/vd_ffmpeg.c
#define MSGTR_MPCODECS_XVMCAcceleratedCodec "[VD_FFMPEG] codec acc�l�r� XVMC .\n"
#define MSGTR_MPCODECS_ArithmeticMeanOfQP "[VD_FFMPEG] Moyenne arithm�tique de QP: %2.4f, moyenne harmonique de QP: %2.4f\n"
#define MSGTR_MPCODECS_DRIFailure "[VD_FFMPEG] �chec DRI.\n"
#define MSGTR_MPCODECS_CouldntAllocateImageForCodec "[VD_FFMPEG] Impossible allouer image pour codec.\n"
#define MSGTR_MPCODECS_XVMCAcceleratedMPEG2 "[VD_FFMPEG] MPEG2 acc�l�r� XVMC.\n"
#define MSGTR_MPCODECS_TryingPixfmt "[VD_FFMPEG] Essaie pixfmt=%d.\n"
#define MSGTR_MPCODECS_McGetBufferShouldWorkOnlyWithXVMC "[VD_FFMPEG] Le mc_get_buffer devrait fonctionner seulement avec acc�l�ration XVMC!!"
#define MSGTR_MPCODECS_UnexpectedInitVoError "[VD_FFMPEG] Erreur init_vo inattendue.\n"
#define MSGTR_MPCODECS_UnrecoverableErrorRenderBuffersNotTaken "[VD_FFMPEG] Erreur fatale, tampons de rendement non pris.\n"
#define MSGTR_MPCODECS_OnlyBuffersAllocatedByVoXvmcAllowed "[VD_FFMPEG] Seuls les tampons allou�s par vo_xvmc permis.\n"

// libmpcodecs/ve_lavc.c
#define MSGTR_MPCODECS_HighQualityEncodingSelected "[VE_LAVC] Codage haute qualit� s�lectionn� (non temps r�el)!\n"
#define MSGTR_MPCODECS_UsingConstantQscale "[VE_LAVC] Utilise qscale constant = %f (VBR).\n"

// libmpcodecs/ve_raw.c
#define MSGTR_MPCODECS_OutputWithFourccNotSupported "[VE_RAW] Sortie brut avec fourcc [%x] non support�!\n"
#define MSGTR_MPCODECS_NoVfwCodecSpecified "[VE_RAW] Codec VfW requis non sp�cifi�!!\n"

// libmpcodecs/vf_crop.c
#define MSGTR_MPCODECS_CropBadPositionWidthHeight "[CROP] Mauvaise position/largeur/hauteur - aire coup�e hors original!\n"

// libmpcodecs/vf_cropdetect.c
#define MSGTR_MPCODECS_CropArea "[CROP] Aire coup�e: X: %d..%d  Y: %d..%d  (-vf crop=%d:%d:%d:%d).\n"

// libmpcodecs/vf_format.c, vf_palette.c, vf_noformat.c
#define MSGTR_MPCODECS_UnknownFormatName "[VF_FORMAT] Nom de format inconnu: '%s'.\n"

// libmpcodecs/vf_framestep.c vf_noformat.c vf_palette.c vf_tile.c
#define MSGTR_MPCODECS_ErrorParsingArgument "[VF_FRAMESTEP] Erreur transmission arguments.\n"

// libmpcodecs/ve_vfw.c
#define MSGTR_MPCODECS_CompressorType "Genre compresseur %.4lx\n"
#define MSGTR_MPCODECS_CompressorSubtype "Sous-genre compresseur: %.4lx\n"
#define MSGTR_MPCODECS_CompressorFlags "Indicateurs de compresseur: %lu, version %lu, ICM version: %lu\n"
#define MSGTR_MPCODECS_Flags "Indicateurs:"
#define MSGTR_MPCODECS_Quality " qualit�"

// libmpcodecs/vf_expand.c
#define MSGTR_MPCODECS_FullDRNotPossible "Plein DR impossible, essaie plut�t TRANCHES!\n"
#define MSGTR_MPCODECS_WarnNextFilterDoesntSupportSlices  "Alerte! Filtre suivant ne supporte pas TRANCHES, gare au sig11...\n"
#define MSGTR_MPCODECS_FunWhydowegetNULL "Pourquoi ce NULL??\n"

// libmpcodecs/vf_fame.c
#define MSGTR_MPCODECS_FatalCantOpenlibFAME "FATAL: ne peux ouvrir libFAME!\n"

// libmpcodecs/vf_test.c, vf_yuy2.c, vf_yvu9.c
#define MSGTR_MPCODECS_WarnNextFilterDoesntSupport "%s non support� par filtre suivant/vo :(\n"

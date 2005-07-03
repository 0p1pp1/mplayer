// Last sync on 2003-11-10 with help_mp-en.h 1.114
// Updates & fixes by pl <p_l@gmx.fr> & n.le gaillart <n@tourmentine.com>
// Original translation by Firebird <firebird@chez.com>

// ========================= Aide MPlayer ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"Utilisation:      mplayer [options] [url|r�pertoire/]fichier\n"
"\n"
"Options de base:  (liste compl�te dans la page de man)\n"
" -vo <pil[:p�r]>  s�lectionne le pilote et le p�riph. vid�o ('-vo help' pour la liste)\n"
" -ao <pil[:p�r]>  s�lectionne le pilote et le p�riph. audio ('-ao help' pour la liste)\n"
#ifdef HAVE_VCD
" vcd://<n�piste>  lit une piste VCD (Video CD) plut�t qu'un fichier\n"
#endif
#ifdef USE_DVDREAD
" dvd://<n�titre>  joue un titre DVD plut�t qu'un fichier\n"
" -alang/-slang    s�lectionne la langue audio/sous-titres (code pays � 2 lettres)\n"
#endif
" -ss <temps>      d�marre la lecture � 'temps' (temps en secondes ou hh:mm:ss)\n"
" -nosound         ne joue aucun son\n"
" -fs              lecture plein-�cran (ou -vm, -zoom, d�tails dans la page de man)\n"
" -x <x> -y <y>    r�solution de l'affichage (� utiliser avec -vm ou -zoom)\n"
" -sub <fich>      sp�cifie les sous-titres � utiliser (cf. -subfps, -subdelay)\n"
" -playlist <fich> sp�cifie la liste des fichiers � jouer\n"
" -vid x -aid y    sp�cifie les flux vid�o (x) et audio (y) � jouer\n"
" -fps x -srate y  options pour changer les fr�q. vid�o (x fps) et audio (y Hz)\n"
" -pp <qualit�>    active le filtre de postprocessing (d�tails dans la page de man)\n"
" -framedrop       saut d'images (pour les machines lentes)\n"
"\n"
"Fonctions au clavier: (liste compl�te dans la page de man, voir aussi input.conf)\n"
" <- ou ->         + / - 10 secondes\n"
" haut ou bas      + / - 1 minute\n"
" PgUp ou PgDown   + / - 10 minutes\n"
" < ou >           fichier suivant / pr�c�dent dans la playlist\n"
" p ou ESPACE      pause (presser n'importe quelle touche pour continuer)\n"
" q ou ESC         arr�te la lecture et quitte le programme\n"
" + ou -           synchro audio / vid�o: +/- 0.1 seconde\n"
" o                change l'OSD: rien / barre de recherche / barre rech. + temps\n"
" * ou /           augmente/diminue le volume PCM\n"
" z ou x           synchro des sous-titres: +/- 0.1 seconde\n"
" r ou t           pos. des sous-titres: plus haut/plus bas, voir aussi -vf expand\n"
"\n"
" * * * VOIR LA PAGE MAN POUR LES D�TAILS ET LES AUTRES OPTIONS ET TOUCHES * * *\n"
"\n";
#endif

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
#define MSGTR_CreatingCfgFile "Cr�ation du fichier de config: %s\n"
#define MSGTR_InvalidAOdriver "Nom du pilote de sortie audio invalide: %s\nUtilisez '-ao help' pour avoir une liste des pilotes disponibles.\n"
#define MSGTR_CopyCodecsConf "Copiez/liez etc/codecs.conf depuis le source de MPlayer vers ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "Utilisation du codecs.conf int�gr� par d�faut\n"
#define MSGTR_CantLoadFont "Ne peut charger la police: %s\n"
#define MSGTR_CantLoadSub "Ne peut charger les sous-titres: %s\n"
#define MSGTR_DumpSelectedStreamMissing "dump: FATAL: flux s�lectionn� manquant !\n"
#define MSGTR_CantOpenDumpfile "Impossible d'ouvrir le fichier dump.\n"
#define MSGTR_CoreDumped "Core dumped ;)\n"
#define MSGTR_FPSnotspecified "FPS non sp�cifi� ou invalide dans l'ent�te! Utilisez l'option -fps.\n"
#define MSGTR_TryForceAudioFmtStr "Tente de forcer la famille de codecs audio %s ...\n"
#define MSGTR_CantFindAudioCodec "Ne peut trouver de codec pour le format audio 0x%X.\n"
#define MSGTR_RTFMCodecs "Veuillez lire DOCS/HTML/fr/codecs.html !\n"
#define MSGTR_TryForceVideoFmtStr "Tente de forcer la famille de codecs vid�o %s  ...\n"
#define MSGTR_CantFindVideoCodec "Ne peut trouver codec pour format vid�o 0x%X.\n"
#define MSGTR_VOincompCodec "Le pilote de sortie vid�o choisi n'est pas compatible avec ce codec.\n"
#define MSGTR_CannotInitVO "FATAL: Ne peut initialiser le pilote vid�o.\n"
#define MSGTR_CannotInitAO "Ne peut ouvrir/initialiser le p�riph�rique audio -> pas de son.\n"
#define MSGTR_StartPlaying "D�marre la lecture...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"         ************************************************************\n"\
"         **** Votre syst�me est trop LENT pour jouer ce fichier! ****\n"\
"         ************************************************************\n\n"\
"Raisons possibles, probl�mes, solutions:\n"\
"- Le plus courant: pilote _audio_ cass�/bogu�\n"\
"  - Essayez -ao sdl ou utilisez ALSA 0.5 ou l'�mulation OSS d'ALSA 0.9.\n"\
"  - Essayez diff�rentes valeurs pour -autosync, 30 est un bon d�but.\n"\
"- Vid�o lente\n"\
"  - Essayez avec un pilotes -vo diff�rent (-vo help pour la liste) ou\n"\
"    essayez avec -framedrop !\n"\
"- CPU lent\n"\
"  - N'essayez pas de lire de gros DVD/DivX sur un CPU lent ! Essayez -hardframedrop.\n"\
"- Fichier corrompu\n"\
"  - Essayez des m�langes de -nobps -ni -forceidx -mc 0.\n"\
"- Pour jouer depuis un m�dia lent (NFS/SMB, DVD, VCD, etc.)\n"\
"  - Essayez -cache 8192\n"\
"- Utilisez-vous -cache avec un fichier AVI mal multiplex� ? \n"\
"  - Essayez avec -nocache\n"\
"Lisez DOCS/HTML/fr/video.html pour les astuces de r�glage/acc�l�ration.\n"\
"Si rien de tout cela ne r�soud le probl�me, lisez DOCS/HTML/fr/bugreports.html.\n\n"

#define MSGTR_NoGui "MPlayer a �t� compil� SANS support GUI.\n"
#define MSGTR_GuiNeedsX "MPlayer GUI a besoin de X11.\n"
#define MSGTR_Playing "Joue %s\n"
#define MSGTR_NoSound "Audio: pas de son\n"
#define MSGTR_FPSforced "FPS forc� � %5.3f  (ftime: %5.3f)\n"
#define MSGTR_CompiledWithRuntimeDetection "Compil� avec d�tection du CPU � l'ex�cution - ATTENTION, ce n'est pas optimal! Pour de meilleures performances, recompilez MPlayer avec l'option --disable-runtime-cpudetection\n"
#define MSGTR_CompiledWithCPUExtensions "Compil� pour CPU x86 avec les extensions:"
#define MSGTR_AvailableVideoOutputDrivers "Pilotes de sortie vid�o disponibles:\n"
#define MSGTR_AvailableAudioOutputDrivers "Pilotes de sortie audio disponibles:\n"
#define MSGTR_AvailableAudioCodecs "Codecs audio disponibles:\n"
#define MSGTR_AvailableVideoCodecs "Codecs vid�o disponibles:\n"
#define MSGTR_AvailableAudioFm "\nFamilles/pilotes de codecs audio disponibles (inclus � la compilation):\n"
#define MSGTR_AvailableVideoFm "\nFamilles/pilotes de codecs vid�o disponibles (inclus � la compilation):\n"
#define MSGTR_AvailableFsType "Modes plein �cran disponibles:\n"
#define MSGTR_UsingRTCTiming "Utilisation de la synchronisation mat�rielle par RTC (%ldHz)\n"
#define MSGTR_CannotReadVideoProperties "Vid�o: impossible de lire les propri�t�s\n"
#define MSGTR_NoStreamFound "Aucun flux trouv�.\n"
#define MSGTR_ErrorInitializingVODevice "Erreur � l'ouverture/initialisation de la sortie vid�o choisie (-vo).\n"
#define MSGTR_ForcedVideoCodec "Codec vid�o forc�: %s\n"
#define MSGTR_ForcedAudioCodec "Codec audio forc�: %s\n"
#define MSGTR_Video_NoVideo "Vid�o: pas de vid�o\n"
#define MSGTR_NotInitializeVOPorVO "\nFATAL: impossible d'initialiser les filtres vid�o (-vf) ou la sortie vid�o (-vo).\n"
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
"  Recompilez MPlayer avec --enable-debug et faites un backtrace 'gdb'\n"\
"  et d�sassemblez. Pour les d�tails, voir DOCS/HTML/fr/bugreports_what.html#bugreports_crash\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer a plant�. Cela n'aurait pas d� arriver.\n"\
"  Il y a peut-�tre un bogue dans le code de MPlayer _ou_ dans vos pilotes _ou_ dans votre\n"\
"  version de gcc. Si vous pensez que c'est la faute de MPlayer, merci de lire DOCS/HTML/fr/bugreports.html\n"\
"  et d'en suivre les instructions. Nous ne pourrons et ne voudrons pas vous aider � moins que vous\n"\
"  ne fournissiez ces informations en rapportant un bogue possible.\n"


// mencoder.c

#define MSGTR_UsingPass3ControllFile "Utilisation du fichier de contr�le de la passe 3: %s\n"
#define MSGTR_MissingFilename "\nNom de fichier manquant.\n\n"
#define MSGTR_CannotOpenFile_Device "Impossible d'ouvrir le fichier/p�riph.\n"
#define MSGTR_CannotOpenDemuxer "Impossible d'ouvrir le d�muxeur.\n"
#define MSGTR_NoAudioEncoderSelected "\nAucun encodeur audio (-oac) choisi ! Choisissez-en un ou utilisez -nosound. Utilisez -oac help !\n"
#define MSGTR_NoVideoEncoderSelected "\nAucun encodeur vid�o (-ovc) choisi ! Choisissez-en un, utilisez -ovc help !\n"
#define MSGTR_CannotOpenOutputFile "Impossible d'ouvrir le fichier de sortie '%s'\n"
#define MSGTR_EncoderOpenFailed "Impossible d'ouvrir l'encodeur\n"
#define MSGTR_ForcingOutputFourcc "Code fourcc de sortie forc� � %x [%.4s]\n"
#define MSGTR_WritingAVIHeader "Ecriture de l'ent�te AVI...\n"
#define MSGTR_DuplicateFrames "\n%d trame(s) dupliqu�e(s)!\n"
#define MSGTR_SkipFrame "\nTrame supprim�e!\n"
#define MSGTR_ErrorWritingFile "%s: Erreur durant l'�criture du fichier.\n"
#define MSGTR_WritingAVIIndex "\nEcriture de l'index AVI...\n"
#define MSGTR_FixupAVIHeader "Correction de l'ent�te AVI...\n"
#define MSGTR_RecommendedVideoBitrate "Bitrate vid�o recommand� pour le CD %s: %d\n"
#define MSGTR_VideoStreamResult "\nFlux vid�o: %8.3f kbit/s  (%d bps)  taille: %d octets  %5.3f secs  %d trames\n"
#define MSGTR_AudioStreamResult "\nFlux audio: %8.3f kbit/s  (%d bps)  taille: %d octets  %5.3f secs\n"

// cfg-mencoder.h:

#define MSGTR_MEncoderMP3LameHelp "\n\n"\
" vbr=<0-4>     m�thode � bitrate variable\n"\
"                0: cbr\n"\
"                1: mt\n"\
"                2: rh (par d�faut)\n"\
"                3: abr\n"\
"                4: mtrh\n"\
"\n"\
" abr           bitrate moyen\n"\
"\n"\
" cbr           bitrate constant\n"\
"               Force �galement l'encodage en mode CBR sur les modes ABR pr�s�lectionn�s suivants\n"\
"\n"\
" br=<0-1024>   sp�cifie le bitrate en kbits (CBR et ABR uniquement)\n"\
"\n"\
" q=<0-9>       qualit� (0-plus haut, 9-plus bas) (uniquement pour VBR)\n"\
"\n"\
" aq=<0-9>      qualit� algorithmique (0-meilleure/plus lent, 9-pire/plus rapide)\n"\
"\n"\
" ratio=<1-100> rapport de compression\n"\
"\n"\
" vol=<0-10>    d�finit le gain d'entr�e audio\n"\
"\n"\
" mode=<0-3>    (par d�faut: auto)\n"\
"                0: stereo\n"\
"                1: joint-stereo\n"\
"                2: dualchannel\n"\
"                3: mono\n"\
"\n"\
" padding=<0-2>\n"\
"                0: non\n"\
"                1: tout\n"\
"                2: ajuste\n"\
"\n"\
" fast          acc�l�re l'encodage pour les modes VBR pr�selectionn�s suivants,\n"\
"               qualit� l�g�rement inf�rieure et bitrates plus �lev�s.\n"\
"\n"\
" preset=<valeur> fournit les plus hauts param�tres de qualit� possibles.\n"\
"                 medium: encodage VBR, bonne qualit�\n"\
"                 (intervalle de bitrate 150-180 kbps)\n"\
"                 standard: encodage VBR, haute qualit�\n"\
"                 (intervalle de bitrate 170-210 kbps)\n"\
"                 extreme: encodage VBR, tr�s haute qualit�\n"\
"                 (intervalle de bitrate 200-240 kbps)\n"\
"                 insane: encodage CBR, plus haute qualit� pr�selectionn�e\n"\
"                 (bitrate 320 kbps)\n"\
"                 <8-320>: encodage ABR au bitrate moyen indiqu� en kbps.\n\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "Lecteur CD-ROM '%s' non trouv�.\n"
#define MSGTR_ErrTrackSelect "Erreur lors du choix de la piste VCD.\n"
#define MSGTR_ReadSTDIN "Lecture depuis stdin...\n"
#define MSGTR_UnableOpenURL "Impossible d'ouvrir l'URL: %s\n"
#define MSGTR_ConnToServer "Connect� au serveur: %s\n"
#define MSGTR_FileNotFound "Fichier non trouv�: '%s'\n"

#define MSGTR_SMBInitError "Impossible d'initialiser libsmbclient: %d\n"
#define MSGTR_SMBFileNotFound "Impossible d'ouvrir depuis le r�seau: '%s'\n"
#define MSGTR_SMBNotCompiled "MPlayer n'a pas �t� compil� avec le support SMB\n"

#define MSGTR_CantOpenDVD "Impossible d'ouvrir le lecteur DVD: %s\n"
#define MSGTR_DVDwait "Lecture de la structure du disque, veuillez patienter...\n"
#define MSGTR_DVDnumTitles "Il y a %d titres sur ce DVD.\n"
#define MSGTR_DVDinvalidTitle "Num�ro de titre DVD invalide: %d\n"
#define MSGTR_DVDnumChapters "Il y a %d chapitres sur ce titre DVD.\n"
#define MSGTR_DVDinvalidChapter "Num�ro de chapitre DVD invalide: %d\n"
#define MSGTR_DVDnumAngles "Il y a %d angles sur ce titre DVD.\n"
#define MSGTR_DVDinvalidAngle "Num�ro d'angle DVD invalide: %d\n"
#define MSGTR_DVDnoIFO "Impossible d'ouvrir le fichier IFO pour le titre DVD %d.\n"
#define MSGTR_DVDnoVOBs "Impossible d'ouvrir le titre VOBS (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD ouvert avec succ�s.\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "ATTENTION: Ent�te du flux audio %d red�fini.\n"
#define MSGTR_VideoStreamRedefined "ATTENTION: Ent�te du flux vid�o %d red�fini.\n"
#define MSGTR_TooManyAudioInBuffer "\nTrop de paquets audio dans le tampon (%d dans %d octets)\n"
#define MSGTR_TooManyVideoInBuffer "\nTrop de paquets vid�o dans le tampon (%d dans %d octets)\n"
#define MSGTR_MaybeNI "Peut-�tre que vous jouez un flux/fichier mal multiplex�, ou que le codec a �chou� ?\n"\
                      "Pour les fichier AVI, essayez de forcer le mode non-entrelac� avec l'option -ni.\n"
#define MSGTR_SwitchToNi "\nFichier AVI non entrelac� - passage en mode -ni...\n"
#define MSGTR_Detected_XXX_FileFormat "Fichier de type %s d�tect�.\n"
#define MSGTR_DetectedAudiofile "Fichier audio d�tect�.\n"
#define MSGTR_NotSystemStream "Pas un flux de type MPEG System... (peut-�tre un Flux de Transport?)\n"
#define MSGTR_InvalidMPEGES "Flux MPEG-ES invalide ??? Contactez l'auteur, c'est peut-�tre un bug :(\n"
#define MSGTR_FormatNotRecognized "========== D�sol�, ce format de fichier n'est pas reconnu/support� ============\n"\
                                  "== Si ce fichier est un flux AVI, ASF ou MPEG, merci de contacter l'auteur ! ==\n"
#define MSGTR_MissingVideoStream "Aucun flux vid�o trouv�.\n"
#define MSGTR_MissingAudioStream "Aucun flux audio trouv� -> pas de son\n"
#define MSGTR_MissingVideoStreamBug "Flux vid�o manquant !? Contactez l'auteur, c'est peut-�tre un bug :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: le fichier ne contient pas le flux audio ou vid�o s�lectionn�.\n"

#define MSGTR_NI_Forced "Forc�"
#define MSGTR_NI_Detected "D�tect�"
#define MSGTR_NI_Message "format de fichier AVI NON-ENTRELAC� %s.\n"

#define MSGTR_UsingNINI "Utilise le support des fichiers AVI mal multiplex�s.\n"
#define MSGTR_CouldntDetFNo "Impossible de d�terminer le nombre de trames (pour recherche absolue)\n"
#define MSGTR_CantSeekRawAVI "Impossible de chercher dans un flux AVI brut ! (Index requis, essayez l'option -idx.)\n"
#define MSGTR_CantSeekFile "Impossible de chercher dans ce fichier.\n"

#define MSGTR_MOVcomprhdr "MOV: Les ent�tes compress�es n�cessitent ZLIB !\n"
#define MSGTR_MOVvariableFourCC "MOV: ATTENTION: FOURCC Variable d�tect� !?\n"
#define MSGTR_MOVtooManyTrk "MOV: ATTENTION: Trop de pistes"
#define MSGTR_FoundAudioStream "==> Flux audio trouv�: %d\n"
#define MSGTR_FoundVideoStream "==> Flux vid�o trouv�: %d\n"
#define MSGTR_DetectedTV "TV d�tect�e ! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "Impossible d'ouvrir le demuxer ogg\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: recherche du flux audio (id:%d)\n"
#define MSGTR_CannotOpenAudioStream "Impossible d'ouvrir le flux audio: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "Impossible d'ouvrir le flux des sous-titres: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "Impossible d'ouvrir le demuxer audio: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "Impossible d'ouvrir le demuxer de sous-titres: %s\n"
#define MSGTR_TVInputNotSeekable "Impossible de rechercher sur l'entr�e TV ! (cette op�ration correspondra s�rement � un changement de chaines ;)\n"
#define MSGTR_DemuxerInfoAlreadyPresent "Info du demuxer %s d�j� pr�sente!\n"
#define MSGTR_ClipInfo "Information sur le clip: \n"

#define MSGTR_LeaveTelecineMode "\ndemux_mpg: contenu NTSC 30fps d�tect�, ajustement du d�bit.\n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: contenu NTSC 24fps progressif d�tect�, ajustement du d�bit.\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "Impossible d'ouvrir le codec.\n"
#define MSGTR_CantCloseCodec "Impossible de fermer le codec.\n"

#define MSGTR_MissingDLLcodec "ERREUR: Impossible de trouver le codec DirectShow n�cessaire: %s\n"
#define MSGTR_ACMiniterror "Impossible de charger/initialiser le codec AUDIO Win32/ACM (fichier DLL manquant ?)\n"
#define MSGTR_MissingLAVCcodec "Impossible de trouver le codec '%s' de libavcodec...\n"

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
#define MSGTR_OpeningAudioDecoder "Ouverture du d�codeur audio: [%s] %s\n"
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
#define MSGTR_CannotFindColorspace "Ne peut pas trouver d'espace de couleurs commun, m�me en utilisant 'scale' :(\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDec: le codec n'a pas d�fini sh->disp_w et sh->disp_h, essai de contournement !\n"
#define MSGTR_VoConfigRequest "VDec: requ�te de configuration de vo - %d x %d (csp pr�fer�: %s)\n"
#define MSGTR_CouldNotFindColorspace "N'a pas pu trouver d'espace de couleur correspondant - nouvel essai avec -vf scale...\n"
#define MSGTR_MovieAspectIsSet "L'aspect du film est %.2f:1 - pr�-redimensionnement � l'aspect correct.\n"
#define MSGTR_MovieAspectUndefined "L'aspect du film est ind�fini - pas de pr�-redimensionnement appliqu�.\n"

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
#define MSGTR_Equalizer "Equaliseur"
#define MSGTR_SkinBrowser "Navigateur de skins"
#define MSGTR_Network "Streaming depuis le r�seau ..."
#define MSGTR_Preferences "Pr�f�rences"
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
#define MSGTR_NEEDLAVCFAME "D�sol�, vous ne pouvez pas jouer de fichier non-MPEG avec votre p�riph�rique DXR3/H+ sans r�encodage\nVeuillez activer lavc ou fame dans la bo�te de configuration DXR3/H+."

// --- messages d'erreurs du chargement de skin ---
#define MSGTR_SKIN_ERRORMESSAGE "[Skin] erreur � la ligne %d du fichier de config de skin: %s"
#define MSGTR_SKIN_WARNING1 "[skin] attention � la ligne %d du fichier de config de skin: Widget trouv� mais \"section\" n'a pas �t� trouv� avant (%s)"
#define MSGTR_SKIN_WARNING2 "[skin] attention � la ligne %d du fichier de config de skin: Widget trouv� mais \"subsection\" n'a pas �t� trouv� avant (%s)"
#define MSGTR_SKIN_WARNING3 "[skin] attention � la ligne %d du fichier de config de skin: cette sous-section n'a pas support� ce widget (%s)"
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
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "erreur de lecture du fichier de configuration du skin (%s)\n"
#define MSGTR_SKIN_LABEL "Skins:"

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
// TODO: Why is this different from MSGTR_PlayList?
#define MSGTR_MENU_PlayList "Playlist"
#define MSGTR_MENU_SkinBrowser "Navigateur de skins"
#define MSGTR_MENU_Exit "Quitter..."
#define MSGTR_MENU_Mute "Silence"
#define MSGTR_MENU_Original "Original"
#define MSGTR_MENU_AspectRatio "Aspect (ratio)"
#define MSGTR_MENU_AudioTrack "Piste audio"
#define MSGTR_MENU_Track "Piste %d"
#define MSGTR_MENU_VideoTrack "Piste Vid�o"

// --- equalizer
#define MSGTR_EQU_Audio "Audio"
#define MSGTR_EQU_Video "Vid�o"
#define MSGTR_EQU_Contrast "Contraste: "
#define MSGTR_EQU_Brightness "Luminosit�: "
#define MSGTR_EQU_Hue "Hue: "
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
#define MSGTR_PREFERENCES_SubtitleOSD "Sous-titres & OSD"
#define MSGTR_PREFERENCES_Codecs "Codecs & demuxer"
#define MSGTR_PREFERENCES_Misc "Divers"

#define MSGTR_PREFERENCES_None "Aucun"
#define MSGTR_PREFERENCES_AvailableDrivers "Pilotes disponibles:"
#define MSGTR_PREFERENCES_DoNotPlaySound "Impossible de jouer le son"
#define MSGTR_PREFERENCES_NormalizeSound "Normaliser le son"
#define MSGTR_PREFERENCES_EnEqualizer "Activer l'�qualiseur"
#define MSGTR_PREFERENCES_ExtraStereo "Activer l'extra-st�r�o"
#define MSGTR_PREFERENCES_Coefficient "Coefficient:"
#define MSGTR_PREFERENCES_AudioDelay "Retard audio"
#define MSGTR_PREFERENCES_DoubleBuffer "Activer le doublebuffering"
#define MSGTR_PREFERENCES_DirectRender "Activer le rendu direct"
#define MSGTR_PREFERENCES_FrameDrop "Activer le saut de trame"
#define MSGTR_PREFERENCES_HFrameDrop "Activer le saut de trame VIOLENT (dangereux)"
#define MSGTR_PREFERENCES_Flip "Mirroir vertical"
#define MSGTR_PREFERENCES_Panscan "Panscan: "
#define MSGTR_PREFERENCES_OSDTimer "Minuteur et indicateurs"
#define MSGTR_PREFERENCES_OSDProgress "Barres de progression seulement"
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "Minuteur, pourcentage et temps total"
#define MSGTR_PREFERENCES_Subtitle "Sous-titre:"
#define MSGTR_PREFERENCES_SUB_Delay "D�calage: "
#define MSGTR_PREFERENCES_SUB_FPS "FPS:"
#define MSGTR_PREFERENCES_SUB_POS "Position: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "D�sactiver le chargement automatique des sous-titres"
#define MSGTR_PREFERENCES_SUB_Unicode "Sous-titre en Unicode"
#define MSGTR_PREFERENCES_SUB_MPSUB "Convertit le sous-titre dans le format MPlayer"
#define MSGTR_PREFERENCES_SUB_SRT "Convertit le sous-titre vers le format SubViewer (SRT) bas� sur le temps"
#define MSGTR_PREFERENCES_SUB_Overlap "Bascule le recouvrement des sous-titres"
#define MSGTR_PREFERENCES_Font "Police:"
#define MSGTR_PREFERENCES_FontFactor "Facteur de la police:"
#define MSGTR_PREFERENCES_PostProcess "Activer le postprocessing"
#define MSGTR_PREFERENCES_AutoQuality "Qualit� auto.: "
#define MSGTR_PREFERENCES_NI "Utiliser le parseur d'AVI mal multiplex�s"
#define MSGTR_PREFERENCES_IDX "Reconstruire l'index, si n�cessaire"
#define MSGTR_PREFERENCES_VideoCodecFamily "Famille de codecs vid�o:"
#define MSGTR_PREFERENCES_AudioCodecFamily "Famille de codecs audio:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "Niveau OSD"
#define MSGTR_PREFERENCES_FRAME_Subtitle "Sous-titre"
#define MSGTR_PREFERENCES_FRAME_Font "Police"
#define MSGTR_PREFERENCES_FRAME_PostProcess "Postprocessing"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "Codec & demuxer"
#define MSGTR_PREFERENCES_FRAME_Cache "Cache"
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
#define MSGTR_PREFERENCES_LoadFullscreen "D�marrer en plein �cran"
#define MSGTR_PREFERENCES_SaveWinPos "Sauver la position de la fen�tre"
#define MSGTR_PREFERENCES_CacheSize "Taille du cache: "
#define MSGTR_PREFERENCES_XSCREENSAVER "Stopper XScreenSaver"
#define MSGTR_PREFERENCES_PlayBar "Active la barre de lecture"
#define MSGTR_PREFERENCES_AutoSync "AutoSynchro on/off"
#define MSGTR_PREFERENCES_AutoSyncValue "Autosynchro: "
#define MSGTR_PREFERENCES_CDROMDevice "P�riph�rique CD-ROM:"
#define MSGTR_PREFERENCES_DVDDevice "P�riph�rique DVD:"
#define MSGTR_PREFERENCES_FPS "FPS du film:"
#define MSGTR_PREFERENCES_ShowVideoWindow "Affiche la fen�tre vid�o inactive"

#define MSGTR_ABOUT_UHU "Le d�veloppement de la GUI est sponsoris� par UHU Linux\n"
#define MSGTR_ABOUT_CoreTeam "   Equipe principale MPlayer:\n"
#define MSGTR_ABOUT_AdditionalCoders "   Programmeurs additionnels:\n"
#define MSGTR_ABOUT_MainTesters "   Principaux testeurs:\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "Erreur fatale !"
#define MSGTR_MSGBOX_LABEL_Error "Erreur !"
#define MSGTR_MSGBOX_LABEL_Warning "Attention !"

#endif

// Original transation by Firebird <firebird@chez.com>
// Updates & fixes by pl <p_l@gmx.fr>

// ========================= Aide MPlayer ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char* banner_text=
"\n\n"
"MPlayer " VERSION "(C) 2000-2002 Arpad Gereoffy (lisez les DOCS!)\n"
"\n";

static char help_text[]=
#ifdef HAVE_NEW_GUI
"Utilisation:   mplayer [-gui] [options] [r�pertoire/]fichier\n"
#else
"Utilisation:   mplayer [options] [r�pertoire/]fichier\n"
#endif
"\n"
"Options de base (voir la page man pour TOUTES les autres options):\n"
" -vo <pil[:p�r]>  S�l. le pil. et le p�riph. vid�o ('-vo help' pour la liste)\n"
" -ao <pil[:p�r]>  S�l. le pil. et le p�riph. audio ('-ao help' pour la liste)\n"
#ifdef HAVE_VCD
" -vcd <n�piste>   Joue � partir d'un VCD plut�t que d'un fichier\n"
#endif
#ifdef HAVE_LIBCSS
" -dvdauth <p�r>   Pr�cise le chemin du lecteur DVD (pour les DVD crypt�s)\n"
#endif
#ifdef USE_DVDREAD
" -dvd <nrtitre>   Joue � partir du lecteur DVD plut�t que d'un fichier\n"
" -alang/-slang    S�lectionne la langue pour l'audio/les sous-titres (fr,en,...)\n"
#endif
" -ss <temps>      D�marre la lecture � 'temps' (temps en secondes ou hh:mm:ss)\n"
" -nosound         Ne joue aucun son\n"
" -fs -vm -zoom    Options plein-�cran (fs: plein-�cran, vm: changement de mode\n"
"                  vid�o, zoom: changement de taille software)\n"
" -x <x> -y <y>    R�solution de l'affichage (chgts. de mode vid�o ou zoom soft)\n"
" -sub <fich>      Sp�cifie les sous-titres � utiliser (cf. -subfps, -subdelay)\n"
" -playlist <fich> Sp�cifie la liste des fichiers � jouer\n"
" -vid x -aid y    Sp�cifie les flux vid�os (x) et audio (y) � jouer\n"
" -fps x -srate y  Options pour changer les fr�q. vid�o (x fps) et audio (y Hz)\n"
" -pp <qualit�>    Filtres de sorties (voir page man et les docs)\n"
" -framedrop       \"Drop\" d'images (pour les machines lentes)\n"
"\n"
"Fonctions au clavier: (voir la page man et regarder aussi dans input.conf)\n"
" <- ou ->         + / - 10 secondes\n"
" haut ou bas      + / - 1 minute\n"
" PgUp ou PgDown   + / - de 10 minutes\n"
" < ou >           Fichier suivant / pr�c�dent dans la playlist\n"
" p ou ESPACE      Pause (presser n'importe quelle touche pour continuer)\n"
" q ou ESC         Quitter\n"
" + ou -           Synchro audio / vid�o: +/- 0.1 seconde\n"
" o                Change l'OSD: rien / barre de recherche / barre rech. + temps\n"
" * ou /           Augmente/diminue le volume PCM\n"
" z ou x           Synchro des sous-titres: +/- 0.1 seconde\n"
" r ou t           Pos. des sous-titres: plus haut/plus bas (cf. -vop expand !)\n"
"\n"
" *** VOIR LA PAGE MAN POUR LES DETAILS ET LES AUTRES OPTIONS (AVANCEES) ***\n"
"\n";
#endif

// ========================= Messages MPlayer ===========================

// mplayer.c: 

#define MSGTR_Exiting "\nSortie... (%s)\n"
#define MSGTR_Exit_frames "Nombre demand� de frames jou�"
#define MSGTR_Exit_quit "Fin"
#define MSGTR_Exit_eof "Fin du fichier"
#define MSGTR_Exit_error "Erreur fatale"
#define MSGTR_IntBySignal "\nMPlayer interrompu par le signal %d dans le module: %s \n"
#define MSGTR_NoHomeDir "Ne peut trouver r�pertoire home\n"
#define MSGTR_GetpathProblem "Probl�me get_path(\"config\")\n"
#define MSGTR_CreatingCfgFile "Cr�ation du fichier de config: %s\n"
#define MSGTR_InvalidVOdriver "Nom du pilote de sortie vid�o invalide: %s\nUtiliser '-vo help' pour avoir une liste des pilotes disponibles.\n"
#define MSGTR_InvalidAOdriver "Nom du pilote de sortie audio invalide: %s\nUtiliser '-ao help' pour avoir une liste des pilotes disponibles.\n"
#define MSGTR_CopyCodecsConf "(Copiez/liez etc/codecs.conf (dans le source de MPlayer) vers ~/.mplayer/codecs.conf)\n"
#define MSGTR_CantLoadFont "Ne peut charger la police: %s\n"
#define MSGTR_CantLoadSub "Ne peut charger les sous-titres: %s\n"
#define MSGTR_ErrorDVDkey "Erreur avec la cl� du DVD.\n"
#define MSGTR_CmdlineDVDkey "La cl� DVD demand�e sur la ligne de commande a �t� sauvegard�e pour le d�cryptage.\n"
#define MSGTR_DVDauthOk "La s�quence d'authentification DVD semble OK.\n"
#define MSGTR_DumpSelectedSteramMissing "dump: FATAL: le flux s�lectionn� est manquant\n"
#define MSGTR_CantOpenDumpfile "Ne peut ouvrir un fichier dump!!!\n"
#define MSGTR_CoreDumped "core dumped :)\n"
#define MSGTR_FPSnotspecified "FPS non sp�cifi� (ou invalide) dans l'ent�te! Utiliser l'option -fps!\n"
#define MSGTR_NoVideoStream "D�sol�, aucun flux vid�o... c'est injouable\n"
#define MSGTR_TryForceAudioFmt "Tente de forcer famille de pilotes codec audio de famille %d ...\n"
#define MSGTR_CantFindAfmtFallback "Ne peut trouver de codec audio pour famille de pilotes choisie, utilise d'autres.\n"
#define MSGTR_CantFindAudioCodec "Ne peut trouver codec pour format audio 0x%X !\n"
#define MSGTR_TryUpgradeCodecsConfOrRTFM "*** Tentez de mettre � jour %s � partir de etc/codecs.conf\n*** Si ce n'est toujours pas bon, alors lisez DOCS/codecs.html!\n"
#define MSGTR_CouldntInitAudioCodec "Ne peut trouver de codec audio! -> Aucun son\n"
#define MSGTR_TryForceVideoFmt "Tente de forcer famille de pilotes codec vid�o %d ...\n"
#define MSGTR_CantFindVfmtFallback "Ne peut trouver de codec vid�o pour famille de pil. choisie, utilise d'autres.\n"
#define MSGTR_CantFindVideoCodec "Ne peut trouver codec pour format vid�o 0x%X !\n"
#define MSGTR_VOincompCodec "D�sol�, le pilote de sortie vid�o choisi n'est pas compatible avec ce codec.\n"
#define MSGTR_CouldntInitVideoCodec "FATAL: Ne peut initialiser le codec vid�o :(\n"
#define MSGTR_EncodeFileExists "fichier d�j� existant: %s (N'effacez pas vos AVIs pr�f�r�s!)\n"
#define MSGTR_CantCreateEncodeFile "Ne peut ouvrir fichier pour encodage\n"
#define MSGTR_CannotInitVO "FATAL: Ne peut initialiser le pilote vid�o!\n"
#define MSGTR_CannotInitAO "Ne peut ouvrir/initialiser le p�riph�rique audio -> Aucun son\n"
#define MSGTR_StartPlaying "D�marre la reproduction...\n"
#define MSGTR_SystemTooSlow "\n***********************************************************************"\
			    "\n** Votre syst�me est trop lent. Essayez l'option -framedrop ou RTFM! **"\
			    "\n***********************************************************************\n"\
			    "!!! Raisons possibles, probl�mes, solutions: \n"\
			    "- Le plus probable: pilote audio _bugg�_ => essayer -ao sdl ou\n"\
			    "  ALSA 0.5 ou l'�mulation OSS d'ALSA 0.9 => lire DOCS/sound.html\n"\
			    "- Vid�o lente => essayer avec plusieurs pilotes -vo (pour la liste: -vo help) ou\n"\
			    "  avec -framedrop => lire DOCS/video.html\n"\
			    "- CPU lent => �viter les gros DVD/DivX => essayer -hardframedrop\n"\
			    "- Fichier corrompu => essayer des m�langes de -nobps -ni -mc 0 -forceidx\n"\
			    "- -cache est utilis� avec un fichier mal multiplex� => essayer avec -nocache\n"\
			    "Si rien de tout cela ne r�sout le probl�me, lisez DOCS/bugreports.html !\n\n"

#define MSGTR_NoGui "MPlayer a �t� compil� SANS support GUI!\n"
#define MSGTR_GuiNeedsX "MPlayer GUI a besoin de X11!\n"
#define MSGTR_Playing "Joue %s\n"
#define MSGTR_NoSound "Audio: Aucun son!!!\n"
#define MSGTR_FPSforced "FPS forc� � %5.3f  (ftime: %5.3f)\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "Lecteur CD-ROM '%s' non trouv�!\n"
#define MSGTR_ErrTrackSelect "Erreur lors du choix de la piste VCD!"
#define MSGTR_ReadSTDIN "Lecture depuis stdin...\n"
#define MSGTR_UnableOpenURL "Ne peut ouvrir l'URL: %s\n"
#define MSGTR_ConnToServer "Connect� au serveur: %s\n"
#define MSGTR_FileNotFound "Fichier non trouv�: '%s'\n"

#define MSGTR_CantOpenDVD "Ne peut ouvrir le lecteur DVD: %s\n"
#define MSGTR_DVDwait "Lecture de la structure du disque, veuillez attendre...\n"
#define MSGTR_DVDnumTitles "Il y a %d titres sur ce DVD.\n"
#define MSGTR_DVDinvalidTitle "Num�ro de titre DVD invalide: %d\n"
#define MSGTR_DVDnumChapters "Il y a %d chapitres sur ce titre DVD.\n"
#define MSGTR_DVDinvalidChapter "Num�ro de chapitre DVD invalide: %d\n"
#define MSGTR_DVDnumAngles "Il y a %d s�quences sur ce titre DVD.\n"
#define MSGTR_DVDinvalidAngle "Num�ro de s�quence DVD invalide: %d\n"
#define MSGTR_DVDnoIFO "Ne peut ouvrir le fichier IFO pour le titre DVD %d.\n"
#define MSGTR_DVDnoVOBs "Ne peut ouvrir titre VOBS (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD ouvert avec succ�s!\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "Attention! Ent�te du flux audio %d red�fini!\n"
#define MSGTR_VideoStreamRedefined "Attention! Ent�te du flux vid�o %d red�fini!\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: Trop (%d dans %d octets) de packets audio dans le tampon!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: Trop (%d dans %d octets) de packets vid�o dans le tampon!\n"
#define MSGTR_MaybeNI "(Peut-�tre jouez-vous un flux/fichier mal multiplex�, ou le codec manque...)\n"\
                      "Pour les fichier .AVI, essayez l'option -ni."
#define MSGTR_DetectedFILMfile "Format de fichier FILM d�tect�!\n"
#define MSGTR_DetectedFLIfile "Format de fichier FLI d�tect�!\n"
#define MSGTR_DetectedROQfile "Format de fichier RoQ d�tect�!\n"
#define MSGTR_DetectedREALfile "Format de fichier REAL d�tect�!\n"
#define MSGTR_DetectedAVIfile "Format de fichier AVI d�tect�!\n"
#define MSGTR_DetectedASFfile "Format de fichier ASF d�tect�!\n"
#define MSGTR_DetectedMPEGPESfile "Format de fichier MPEG-PES d�tect�!\n"
#define MSGTR_DetectedMPEGPSfile "Format de fichier MPEG-PS d�tect�!\n"
#define MSGTR_DetectedMPEGESfile "Format de fichier MPEG-ES d�tect�!\n"
#define MSGTR_DetectedQTMOVfile "Format de fichier QuickTime/MOV d�tect�!\n"
#define MSGTR_MissingMpegVideo "Flux vid�o MPEG manquant!? Contactez l'auteur, c'est peut-�tre un bug :(\n"
#define MSGTR_InvalidMPEGES "Flux MPEG-ES invalide??? Contactez l'auteur, c'est peut-�tre un bug :(\n"
#define MSGTR_FormatNotRecognized "========== D�sol�, ce format de fichier n'est pas reconnu/support� ===========\n"\
				  "========= Si ce fichier est un fichier AVI, ASF ou MPEG en bon �tat, =========\n"\
				  "======================= merci de contacter l'auteur ! ========================\n"
#define MSGTR_MissingVideoStream "Ne peut trouver de flux vid�o!\n"
#define MSGTR_MissingAudioStream "Ne peut trouver de flux audio...  -> pas de son\n"
#define MSGTR_MissingVideoStreamBug "Flux vid�o manquant!? Contactez l'auteur, c'est peut-�tre un bug :(\n"

#define MSGTR_DoesntContainSelectedStream "Demux: le fichier ne contient pas le flux audio ou vid�o s�lectionn�\n"

#define MSGTR_NI_Forced "Forc�"
#define MSGTR_NI_Detected "D�tect�"
#define MSGTR_NI_Message "%s format de fichier AVI mal multiplex�!\n"

#define MSGTR_UsingNINI "Utilise le support des fichiers AVI mal multiplex�s!\n"
#define MSGTR_CouldntDetFNo "Ne peut d�terminer le nombre de frames (pour recherche absolue)\n"
#define MSGTR_CantSeekRawAVI "Ne peut chercher dans un flux .AVI brut! (index requis, essayez l'option -idx!)\n"
#define MSGTR_CantSeekFile "Ne peut chercher dans ce fichier!  \n"

#define MSGTR_EncryptedVOB "Fichier VOB crypt� (support libcss NON compil�!) Lire DOCS/cd-dvd.html\n"
#define MSGTR_EncryptedVOBauth "Flux crypt� mais l'authentification n'a pas �t� demand�e explicitement!\n"

#define MSGTR_MOVcomprhdr "MOV: Les ent�tes compress�es ne sont pas (encore) support�s!\n"
#define MSGTR_MOVvariableFourCC "MOV: Attention! Variable FOURCC d�tect�e!?\n"
#define MSGTR_MOVtooManyTrk "MOV: Attention! Trop de pistes!"
#define MSGTR_MOVnotyetsupp "\n******** Format Quicktime MOV pas encore support�!!!!!!! *********\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "Ne peut ouvrir le codec\n"
#define MSGTR_CantCloseCodec "Ne peut fermer le codec\n"

#define MSGTR_MissingDLLcodec "ERREUR: Ne peut trouver le codec DirectShow requis: %s\n"
#define MSGTR_ACMiniterror "Ne peut charger/initialiser le codec AUDIO Win32/ACM (fichier DLL manquant?)\n"
#define MSGTR_MissingLAVCcodec "Ne peut trouver le codec '%s' de libavcodec...\n"

#define MSGTR_NoDShowSupport "MPlayer a �t� compil� SANS support DirectShow!\n"
#define MSGTR_NoWfvSupport "Support des codecs Win32 d�sactiv�, ou non disponible sur plateformes non-x86!\n"
#define MSGTR_NoDivx4Support "MPlayer a �t� compil� SANS le support DivX4Linux (libdivxdecore.so)!\n"
#define MSGTR_NoLAVCsupport "MPlayer a �t� compil� SANS le support ffmpeg/libavcodec!\n"
#define MSGTR_NoACMSupport "Codecs audio Win32/ACM d�sactiv�s ou non disponibles sur plateformes non-x86 -> force -nosound :(\n"
#define MSGTR_NoDShowAudio "MPlayer a �t� compil� sans support DirectShow -> force -nosound :(\n"
#define MSGTR_NoOggVorbis "Codec audio OggVorbis d�sactiv� -> force -nosound :(\n"
#define MSGTR_NoXAnimSupport "MPlayer a �t� compil� SANS support XAnim!\n"

#define MSGTR_MpegPPhint "ATTENTION! Vous avez demand� un filtre de sortie pour une vid�o MPEG 1/2,\n" \
			 "           mais avez compil� MPlayer sans support de filtre MPEG 1/2!\n" \
			 "           #define MPEG12_POSTPROC dans config.h et recompilez libmpeg2!\n"
#define MSGTR_MpegNoSequHdr "MPEG: FATAL: Fin du fichier lors de la recherche d'ent�te de s�quence\n"
#define MSGTR_CannotReadMpegSequHdr "FATAL: Ne peut lire l'ent�te de s�quence!\n"
#define MSGTR_CannotReadMpegSequHdrEx "FATAL: Ne peut lire l'extension d'ent�te de s�quence!\n"
#define MSGTR_BadMpegSequHdr "MPEG: Mauvais ent�te de s�quence!\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: Mauvaise extension d'ent�te de s�quence!\n"

#define MSGTR_ShMemAllocFail "Ne peut allouer de m�moire partag�e\n"
#define MSGTR_CantAllocAudioBuf "Ne peut allouer de tampon de sortie audio\n"
#define MSGTR_NoMemForDecodedImage "pas assez de m�moire pour le tampon d'image d�cod�e (%ld octets)\n"

#define MSGTR_AC3notvalid "Flux AC3 non-valide.\n"
#define MSGTR_AC3only48k "Seuls les flux � 48000 Hz sont support�s.\n"
#define MSGTR_UnknownAudio "Format audio inconnu/manquant -> pas de son\n"

// LIRC:
#define MSGTR_SettingUpLIRC "d�finition du support LIRC...\n"
#define MSGTR_LIRCdisabled "Vous ne pourrez pas utiliser votre t�l�commande\n"
#define MSGTR_LIRCopenfailed "Impossible d'ouvrir le support LIRC!\n"
#define MSGTR_LIRCsocketerr "Il y a un probl�me avec le socket LIRC: %s\n"
#define MSGTR_LIRCcfgerr "Impossible de lire le fichier de config LIRC %s !\n"


// ====================== messages/boutons GUI ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "� propos ..."
#define MSGTR_FileSelect "Choisir un fichier ..."
#define MSGTR_SubtitleSelect "Choisir un sous-titre ..."
#define MSGTR_OtherSelect "Choisir ..."
#define MSGTR_MessageBox "BoiteMessage"
#define MSGTR_PlayList "Playlist"
#define MSGTR_SkinBrowser "Browser de skins"

// --- boutons ---
#define MSGTR_Ok "Ok"
#define MSGTR_Cancel "Annuler"
#define MSGTR_Add "Ajouter"
#define MSGTR_Remove "Retirer"

// --- messages d'erreur ---
#define MSGTR_NEMDB "D�sol�, pas assez de m�moire pour le tampon de dessin."
#define MSGTR_NEMFMR "D�sol�, pas assez de m�moire pour le rendu des menus."
#define MSGTR_NEMFMM "D�sol�, pas assez de m�moire pour le masque de la fen�tre principale."

// --- messages d'erreurs du chargement de skin ---
#define MSGTR_SKIN_ERRORMESSAGE "[Skin] Erreur � la ligne %d du fichier de config de skin: %s" 
#define MSGTR_SKIN_WARNING1 "[Skin] Attention � la ligne %d du fichier de config de skin: Widget trouv� mais \"section\" n'a pas �t� trouv� avant (%s)"
#define MSGTR_SKIN_WARNING2 "[Skin] Attention � la ligne %d du fichier de config de skin: Widget trouv� mais \"subsection\" n'a pas �t� trouv� avant (%s)"
#define MSGTR_SKIN_BITMAP_16bit  "Bitmaps de 16 bits ou moins ne sont pas support�s ( %s ).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "Fichier non trouv� ( %s )\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "Erreur de lecture BMP ( %s )\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "Erreur de lecture TGA ( %s )\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "Erreur de lecture PNG ( %s )\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "tga compact� en RLE non support�s ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "format de fichier inconnu ( %s )\n"
#define MSGTR_SKIN_BITMAP_ConvertError "erreur de conversion de 24 bit � 32 bit ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "Message inconnu: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "pas assez de m�moire\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "trop de polices d�clar�es\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "fichier de police introuvable\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "fichier d'image de police introuvable\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "identificateur de fonte inexistant ( %s )\n"
#define MSGTR_SKIN_UnknownParameter "param�tre inconnu ( %s )\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[Browser de skins] pas assez de m�moire.\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "Skin non trouv� ( %s ).\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "Erreur de lecture du fichier de configuration du skin ( %s ).\n"
#define MSGTR_SKIN_LABEL "Skins:"

// --- menus gtk
#define MSGTR_MENU_AboutMPlayer "� propos de MPlayer"
#define MSGTR_MENU_Open "Ouvrir ..."
#define MSGTR_MENU_PlayFile "Lire un fichier ..."
#define MSGTR_MENU_PlayVCD "Lire un VCD ..."
#define MSGTR_MENU_PlayDVD "Lire un DVD ..."
#define MSGTR_MENU_PlayURL "Lire une URL ..."
#define MSGTR_MENU_LoadSubtitle "Charger un sous-titre ..."
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
#define MSGTR_MENU_PlayDisc "Lire un disque..."
#define MSGTR_MENU_ShowDVDMenu "Afficher le menu DVD"
#define MSGTR_MENU_Titles "Titres"
#define MSGTR_MENU_Title "Titre %2d"
#define MSGTR_MENU_None "(aucun)"
#define MSGTR_MENU_Chapters "Chapitres"
#define MSGTR_MENU_Chapter "Chapitre %2d"
#define MSGTR_MENU_AudioLanguages "Langues (audio)"
#define MSGTR_MENU_SubtitleLanguages "Langues (sous-titres)"
#define MSGTR_MENU_PlayList "Playlist"
#define MSGTR_MENU_SkinBrowser "Browser de skins"
#define MSGTR_MENU_Preferences "Pr�f�rences"
#define MSGTR_MENU_Exit "Quitter ..."

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "Erreur fatale ..."
#define MSGTR_MSGBOX_LABEL_Error "erreur ..."
#define MSGTR_MSGBOX_LABEL_Warning "attention ..." 

#endif

// Translated by: Leandro Lucarella <leandro@lucarella.com.ar>
// Translated by: Jes�s Climent <jesus.climent@hispalinux.es>
// 
// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char* banner_text=
"\n\n"
"MPlayer " VERSION "(C) 2000-2001 Arpad Gereoffy (vea DOCS!)\n"
"\n";

static char help_text[]=
#ifdef HAVE_NEW_GUI
"Uso:   mplayer [-gui] [opciones] [ruta/]archivo\n"
#else
"Uso:   mplayer [opciones] [ruta/]archivo\n"
#endif
"\n"
"Opciones:\n"
" -vo <drv[:dev]> selecciona el driver de salida de video y el dispositivo ('-vo help' para obtener una lista)\n"
" -ao <drv[:dev]> selecciona el driver de salida de audio y el dispositivo ('-ao help' para obtener una lista)\n"
" -vcd <trackno>  reproduce pista de VCD (video cd) desde un dispositivo en vez de un archivo regular\n"
#ifdef HAVE_LIBCSS
" -dvdauth <dev>  especifica el dispositivo DVD para autenticaci�n (para discos encriptados)\n"
#endif
#ifdef USE_DVDREAD
" -dvd <titleno>  reproduce t�tulo/pista de DVD desde un dispositivo en vez de un archivo regular\n"
#endif
" -ss <timepos>   busca una determindad posicion (en segundos o hh:mm:ss)\n"
" -nosound        no reproduce el sonido\n"
#ifdef USE_FAKE_MONO
" -stereo <mode>  selecciona la salida est�reo MPEG1 (0:est�reo 1:izquierda 2:derecha)\n"
#endif
" -channels <n>   n�mero de canales de salida de audio\n"
" -fs -vm -zoom   opciones de pantalla completa (pantalla completa,cambio de modo de video,escalado por software)\n"
" -x <x> -y <y>   escala la imagen a resoluci�n <x> * <y> [si -vo driver lo soporta!]\n"
" -sub <file>     especifica el archivo de subtitulos a usar (vea tambi�n -subfps, -subdelay)\n"
" -playlist <file> especifica el archivo con la lista de reproducci�n\n"
" -vid x -aid y   opciones para especificar el stream de video (x) y el audio (y) a reproducir\n"
" -fps x -srate y opciones para cambiar la tasa de video (x fps) y de audio (y Hz)\n"
" -pp <quality>   activa filtro de postprocesado (0-4 para DivX, 0-63 para mpegs)\n"
" -nobps          usa sincron�a A-V alternativa para AVIs (puede ayudar!)\n"
" -framedrop      activa frame-dropping (para m�quinas lentas)\n"
" -wid <window id> usa una ventana activa para dirigir la salida de video (�til conjuntamente con el \"plugger\"\n"
"\n"
"Teclas:\n"
" <-  o  ->      avanza/retrocede 10 segundos\n"
" arriba o abajo avanza/retrocede 1 minuto\n"
" < o >          avanza/retrocede en la lista de reproducci�n\n"
" p o ESPACIO    pausa el video (presione cualquier tecla para continuar)\n"
" q o ESC        detiene la reproducci�n y sale del programa\n"
" + o -          ajusta el retardo de audio en +/- 0.1 segundos\n"
" o              cambia el modo OSD:  nada / b�squeda / b�squeda+tiempo\n"
" * o /          aumenta o disminuye el volumen (presione 'm' para elegir entre master/pcm)\n"
" z o x          ajusta el retardo del subt�tulo en +/- 0.1 segundos\n"
"\n"
" * * * VEA LA P�GINA DE MANUAL PARA M�S DETALLES, OPCIONES AVANZADAS Y TECLAS ! * * *\n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// mplayer.c: 

#define MSGTR_Exiting "\nSaliendo... (%s)\n"
#define MSGTR_Exit_frames "N�mero de cuadros requeridos reproducidos"
#define MSGTR_Exit_quit "Salida"
#define MSGTR_Exit_eof "Fin del archivo"
#define MSGTR_Exit_error "Error fatal"
#define MSGTR_IntBySignal "\nMPlayer interrumpido por se�al %d en el m�dulo: %s \n"
#define MSGTR_NoHomeDir "No se puede encontrar el directorio HOME\n"
#define MSGTR_GetpathProblem "problema en get_path(\"config\")\n"
#define MSGTR_CreatingCfgFile "Creando archivo de configuraci�n: %s\n"
#define MSGTR_InvalidVOdriver "Nombre del driver de salida de video incorrecto: %s\nUse '-vo help' para obtener la lista de drivers de salida de video disponibles.\n"
#define MSGTR_InvalidAOdriver "Nombre del driver de salida de audio incorrecto: %s\nUse '-ao help' para obtener la lista de drivers de salida de audio disponibles.\n"
#define MSGTR_CopyCodecsConf "(copie/ln etc/codecs.conf (en el �rbol del codigo fuente de MPlayer) a ~/.mplayer/codecs.conf)\n"
#define MSGTR_CantLoadFont "No se puede cargar la fuente: %s\n"
#define MSGTR_CantLoadSub "No se puede cargar el subt�tulo: %s\n"
#define MSGTR_ErrorDVDkey "Error procesando la clave del DVD.\n"
#define MSGTR_CmdlineDVDkey "Clave de DVD requerida en la l�nea de comandos esta almacenada para 'descrambling'.\n"
#define MSGTR_DVDauthOk "La secuencia de autorizaci�n del DVD parece estar bien.\n"
#define MSGTR_DumpSelectedSteramMissing "dump: FATAL: no se encuentra el stream seleccionado!\n"
#define MSGTR_CantOpenDumpfile "No se puede abrir el archivo de dump!!!\n"
#define MSGTR_CoreDumped "core dumped :)\n"
#define MSGTR_FPSnotspecified "FPS no especificado (o inv�lido) en la cabecera! Use la opci�n -fps!\n"
#define MSGTR_NoVideoStream "Disculpe, no tiene stream de video... no es reproducible todav�a\n"
#define MSGTR_TryForceAudioFmt "Tratando de forzar la familia del codec de audio %d ...\n"
#define MSGTR_CantFindAfmtFallback "No se encuentra codec de audio para la familia forzada, se usan otros drivers.\n"
#define MSGTR_CantFindAudioCodec "No se encuentra codec para el formato de audio 0x%X !\n"
#define MSGTR_TryUpgradeCodecsConfOrRTFM "*** Intente actualizar %s en etc/codecs.conf\n*** Si todav�a no funciona, lea DOCS/codecs.html!\n"
#define MSGTR_CouldntInitAudioCodec "No se pudo inicializar el codec de audio! -> sin sonido\n"
#define MSGTR_TryForceVideoFmt "Tratando de forzar la familia del codec de video %d ...\n"
#define MSGTR_CantFindVfmtFallback "No se encuentra codec de video para la familia forzada, se usan otros drivers.\n"
#define MSGTR_CantFindVideoCodec "No se encuentra codec para el formato de video 0x%X !\n"
#define MSGTR_VOincompCodec "Disculpe, el dispositivo de salida de video es incompatible con este codec.\n"
#define MSGTR_CouldntInitVideoCodec "FATAL: No se puede inicializar el codec de video :(\n"
#define MSGTR_EncodeFileExists "El archivo ya existe: %s (no sobrescriba su AVI favorito!)\n"
#define MSGTR_CantCreateEncodeFile "No se puede crear el archivo para 'encodear'\n"
#define MSGTR_CannotInitVO "FATAL: No se puede inicializar el driver de video!\n"
#define MSGTR_CannotInitAO "no se puede abrir/inicializar dispositivo de audio -> SIN SONIDO\n"
#define MSGTR_StartPlaying "Empezando a reproducir...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"  **************************************************************\n"\
"  *  Su sistema es demasiado lento para reproducir el video!   *\n"\
"  **************************************************************\n"\
" Posibles razones, problemas, soluciones: \n"\
"- M�s com�n: controlador de _audio_ con errores. Soluci�n: use\n"\
"  -ao sdl o ALSA 0.5 o ALSA 0.9 con emulacion OSS. Lea DOCS/soung.html\n"\
"  para m�s ayuda\n"\
"- Salida de video lenta: pruebe otro -vo driver (para obtener una lista,\n"\
"   -vo help) o pruebe -framedrop ! Lea DOCS/video.html para mas ayuda.\n"\
"- CPU lenta: no reproduzca DVD/DivX grandes en una CPU lenta. pruebe\n"\
"  -hardframedrop !\n"\
"- Fichero err�neo: pruebe combinaciones de: -nobps -ni -mc 0 -forceidx\n"\
"Si niguna funciona, lea DOCS/bugreports.html !\n\n"

#define MSGTR_NoGui "MPlayer fue compilado sin soporte de GUI!\n"
#define MSGTR_GuiNeedsX "El GUI de MPlayer requiere X11!\n"
#define MSGTR_Playing "Reproduciendo %s\n"
#define MSGTR_NoSound "Audio: sin sonido!!!\n"
#define MSGTR_FPSforced "FPS forzado en %5.3f  (ftime: %5.3f)\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "Dispositivo de CD-ROM '%s' no encontrado!\n"
#define MSGTR_ErrTrackSelect "Error seleccionando la pista de VCD!"
#define MSGTR_ReadSTDIN "Leyendo desde la entrada est�ndar (stdin)...\n"
#define MSGTR_UnableOpenURL "No es posible abrir URL: %s\n"
#define MSGTR_ConnToServer "Connectado al servidor: %s\n"
#define MSGTR_FileNotFound "Archivo no encontrado: '%s'\n"

#define MSGTR_CantOpenDVD "No se puede abrir el dispositivo de DVD: %s\n"
#define MSGTR_DVDwait "Leyendo la estructura del disco, espere por favor...\n"
#define MSGTR_DVDnumTitles "Hay %d t�tulos en este DVD.\n"
#define MSGTR_DVDinvalidTitle "N�mero de t�tulo de DVD inv�lido: %d\n"
#define MSGTR_DVDnumChapters "Hay %d cap�tulos en este t�tulo de DVD.\n"
#define MSGTR_DVDinvalidChapter "N�mero de cap�tulo de DVD inv�lido: %d\n"
#define MSGTR_DVDnumAngles "Hay %d �ngulos en este t�tulo de DVD.\n"
#define MSGTR_DVDinvalidAngle "N�mero de �ngulo de DVD inv�lido: %d\n"
#define MSGTR_DVDnoIFO "No se puede abrir archivo IFO para el t�tulo de DVD %d.\n"
#define MSGTR_DVDnoVOBs "No se puede abrir VOBS del t�tulo (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD abierto existosamente!\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "Advertencia! Cabecera de stream de audio %d redefinida!\n"
#define MSGTR_VideoStreamRedefined "Advertencia! Cabecera de stream de video %d redefinida!\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: Demasiados (%d en %d bytes) paquetes de audio en el buffer!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: Demasiados (%d en %d bytes) paquetes de video en el buffer!\n"
#define MSGTR_MaybeNI "(tal vez est� reproduciendo un stream/archivo 'non-interleaved' o fall� el codec)\n"
#define MSGTR_DetectedFILMfile "Detectado formato de archivo FILM!\n"
#define MSGTR_DetectedFLIfile "Detectado formato de archivo FLI!\n"
#define MSGTR_DetectedROQfile "Detectado formato de archivo RoQ!\n"
#define MSGTR_DetectedREALfile "Detectado formato de archivo REAL!\n"
#define MSGTR_DetectedAVIfile "Detectado formato de archivo AVI!\n"
#define MSGTR_DetectedASFfile "Detectado formato de archivo ASF!\n"
#define MSGTR_DetectedMPEGPESfile "Detectado formato de archivo MPEG-PES!\n"
#define MSGTR_DetectedMPEGPSfile "Detectado formato de archivo MPEG-PS!\n"
#define MSGTR_DetectedMPEGESfile "Detectado formato de archivo MPEG-ES!\n"
#define MSGTR_DetectedQTMOVfile "Detectado formato de archivo QuickTime/MOV!\n"
#define MSGTR_MissingMpegVideo "Stream de video MPEG no encontrado!? contacte al autor, puede ser un bug :(\n"
#define MSGTR_InvalidMPEGES "Stream MPEG-ES inv�lido??? contacte al autor, puede ser un bug :(\n"
#define MSGTR_FormatNotRecognized "============ Disculpe, este formato no est� soportado/reconocido =============\n"\
				  "==== Si este archivo es un AVI, ASF o MPEG, por favor contacte al autor! =====\n"
#define MSGTR_MissingVideoStream "No se encontr� stream de video!\n"
#define MSGTR_MissingAudioStream "No se encontr� stream de audio...  -> sin sonido\n"
#define MSGTR_MissingVideoStreamBug "Stream de video perdido!? Contacte al autor, puede ser un bug :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: el archivo no contiene el stream de audio o video seleccionado\n"

#define MSGTR_NI_Forced "Forzado"
#define MSGTR_NI_Detected "Detectado"
#define MSGTR_NI_Message "%s formato de AVI 'NON-INTERLEAVED'!\n"

#define MSGTR_UsingNINI "Usando formato de AVI roto 'NON-INTERLEAVED'!\n"
#define MSGTR_CouldntDetFNo "No se puede determinar el n�mero de cuadros (para una b�squeda absoluta)\n"
#define MSGTR_CantSeekRawAVI "No se puede avanzar/retroceder en un stream crudo .AVI! (se requiere �ndice, pruebe con -idx!)  \n"
#define MSGTR_CantSeekFile "No se puede avanzar/retroceder en este archivo!  \n"

#define MSGTR_EncryptedVOB "Archivos VOB encriptados (no se compil� con soporte de libcss)! Lea DOCS/cd-dvd.html\n"
#define MSGTR_EncryptedVOBauth "Stream encriptado pero usted no pidi� autenticaci�n!!\n"

#define MSGTR_MOVcomprhdr "MOV: Cabecera comprimida no suportada (por ahora)!\n"
#define MSGTR_MOVvariableFourCC "MOV: Advertencia! FOURCC variable detectada!?\n"
#define MSGTR_MOVtooManyTrk "MOV: Advertencia! demasiadas pistas!"
#define MSGTR_MOVnotyetsupp "\n****** Formato Quicktime MOV todav�a no soportado!!!!!!! *******\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "no se pudo abrir codec\n"
#define MSGTR_CantCloseCodec "no se pudo cerrar codec\n"

#define MSGTR_MissingDLLcodec "ERROR: No se pudo abrir el codec DirectShow requerido: %s\n"
#define MSGTR_ACMiniterror "No se puede cargar/inicializar codecs de audio Win32/ACM (falta archivo DLL?)\n"
#define MSGTR_MissingLAVCcodec "No se encuentra codec '%s' en libavcodec...\n"

#define MSGTR_NoDShowSupport "MPlayer fue compilado SIN soporte para directshow!\n"
#define MSGTR_NoWfvSupport "Soporte para codecs win32 desactivado, o no disponible en plataformas no-x86!\n"
#define MSGTR_NoDivx4Support "MPlayer fue compilado SIN soporte para DivX4Linux (libdivxdecore.so)!\n"
#define MSGTR_NoLAVCsupport "MPlayer fue compilado SIN soporte ffmpeg/libavcodec!\n"
#define MSGTR_NoACMSupport "Codec Win32/ACM desactivado, o no disponible en plataformas no-x86 -> forzado sin sonido :(\n"
#define MSGTR_NoDShowAudio "Compilado sin soporte para DirectShow -> forzado sin sonido :(\n"
#define MSGTR_NoOggVorbis "Codec de audio OggVorbis desactivado -> forzado sin sonido :(\n"
#define MSGTR_NoXAnimSupport "MPlayer fue compilado SIN soporte para XAnim!\n"

#define MSGTR_MpegPPhint "ADVERTENCIA! Selecciono postprocesado de im�genes para un video MPEG 1/2,\n" \
			 "         pero compil� MPlayer sin soporte para postprocesado de MPEG 1/2!\n" \
			 "         #define MPEG12_POSTPROC en config.h, y recompile libmpeg2!\n"
#define MSGTR_MpegNoSequHdr "MPEG: FATAL: EOF mientras buscaba la cabecera de secuencia\n"
#define MSGTR_CannotReadMpegSequHdr "FATAL: No se puede leer cabecera de secuencia!\n"
#define MSGTR_CannotReadMpegSequHdrEx "FATAL: No se puede leer la extensi�n de la cabecera de secuencia!\n"
#define MSGTR_BadMpegSequHdr "MPEG: Mala cabecera de secuencia!\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: Mala extensi�n de la cabecera de secuencia!\n"

#define MSGTR_ShMemAllocFail "No se puede alocar memoria compartida\n"
#define MSGTR_CantAllocAudioBuf "No se puede alocar buffer de la salida de audio\n"
#define MSGTR_NoMemForDecodedImage "no hay memoria suficiente para decodificar el buffer de las im�genes (%ld bytes)\n"

#define MSGTR_AC3notvalid "Stream AC3 inv�lido.\n"
#define MSGTR_AC3only48k "S�lo streams de 48000 Hz soportados.\n"
#define MSGTR_UnknownAudio "Formato de audio desconocido/perdido, usando sin sonido\n"

// LIRC:
#define MSGTR_SettingUpLIRC "Configurando soporte para lirc ...\n"
#define MSGTR_LIRCdisabled "No podr� usar el control remoto\n"
#define MSGTR_LIRCopenfailed "Fall� al abrir el soporte para lirc!\n"
#define MSGTR_LIRCsocketerr "Algo falla con el socket de lirc: %s\n"
#define MSGTR_LIRCcfgerr "Fall� al leer archivo de configuraci�n de LIRC %s !\n"


// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "Acerca de ..."
#define MSGTR_FileSelect "Seleccionar archivo ..."
#define MSGTR_SubtitleSelect "Seleccionar subt�tulos..."
#define MSGTR_OtherSelect "Seleccionar..."
#define MSGTR_MessageBox "CajaDeMensaje"
#define MSGTR_PlayList "ListaDeReproducci�n"
#define MSGTR_SkinBrowser "Navegador de Skins"

// --- buttons ---
#define MSGTR_Ok "Ok"
#define MSGTR_Cancel "Cancelar"
#define MSGTR_Add "Agregar"
#define MSGTR_Remove "Quitar"

// --- error messages ---
#define MSGTR_NEMDB "Disculpe, no hay suficiente memoria para dibujar el buffer."
#define MSGTR_NEMFMR "Disculpe, no hay suficiente memoria para dibujar el men�."
#define MSGTR_NEMFMM "Disculpe, no hay suficiente memoria para la m�scara de la forma de la ventana principal."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[skin] error en configuraci�n de skin en la l�nea %d: %s" 
#define MSGTR_SKIN_WARNING1 "[skin] advertencia en configuraci�n de skin en la l�nea %d: widget encontrado pero \"section\" posterior no encontrada ( %s )"
#define MSGTR_SKIN_WARNING2 "[skin] advertencia en configuraci�n de skin en la l�nea %d: widget encontrado pero \"subsection\" posterior no encontrada (%s)"
#define MSGTR_SKIN_BITMAP_16bit  "Mapa de bits de 16 bits o menos no soportado ( %s ).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "archivo no encontrado ( %s )\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "error al leer bmp ( %s )\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "error al leer tga ( %s )\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "error al leer png ( %s )\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "RLE packed tga no soportado ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "tipo de archivo desconocido ( %s )\n"
#define MSGTR_SKIN_BITMAP_ConvertError "error de conversi�n de 24 bit a 32 bit ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "mensaje desconocido: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "no hay suficiente memoria\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "demasiadas fuentes declaradas\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "archivo de fuentes no encontrado\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "archivo de imagen de fuente noi encontrado\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "identificador de fuente no existente ( %s )\n"
#define MSGTR_SKIN_UnknownParameter "parametro desconocido ( %s )\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[skinbrowser] no hay suficiente memoria.\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "Skin no encontrado( %s ).\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "Error de lectura de la configuraci�n del skin ( %s ).\n"
#define MSGTR_SKIN_LABEL "Skins:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "Sobre MPlayer"
#define MSGTR_MENU_Open "Abrir ..."
#define MSGTR_MENU_PlayFile "Reproducir file ..."
#define MSGTR_MENU_PlayVCD "Reproducir VCD ..."
#define MSGTR_MENU_PlayDVD "Reproducir DVD ..."
#define MSGTR_MENU_PlayURL "Reproducir URL ..."
#define MSGTR_MENU_LoadSubtitle "Cargar subt�tulos ..."
#define MSGTR_MENU_Playing "Reproduciendo"
#define MSGTR_MENU_Play "Reproducir"
#define MSGTR_MENU_Pause "Pausa"
#define MSGTR_MENU_Stop "Parar"
#define MSGTR_MENU_NextStream "Siguiente stream"
#define MSGTR_MENU_PrevStream "Anterior stream"
#define MSGTR_MENU_Size "Tama�o"
#define MSGTR_MENU_NormalSize "Tama�o normal"
#define MSGTR_MENU_DoubleSize "Tama�o doble"
#define MSGTR_MENU_FullScreen "Fullscreen"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_PlayDisc "Reproducir disco ..."
#define MSGTR_MENU_ShowDVDMenu "Mostrar men� DVD"
#define MSGTR_MENU_Titles "T�tulos"
#define MSGTR_MENU_Title "T�tulo %2d"
#define MSGTR_MENU_None "(ninguno)"
#define MSGTR_MENU_Chapters "Cap�tulos"
#define MSGTR_MENU_Chapter "Cap�tulo %2d"
#define MSGTR_MENU_AudioLanguages "Idiomas audio"
#define MSGTR_MENU_SubtitleLanguages "Idiomas de subt�tulos"
#define MSGTR_MENU_PlayList "Lista de Reproducci�n"
#define MSGTR_MENU_SkinBrowser "Navegador de Skins"
#define MSGTR_MENU_Preferences "Preferencias"
#define MSGTR_MENU_Exit "Salir ..."

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "error fatal ..."
#define MSGTR_MSGBOX_LABEL_Error "error ..."
#define MSGTR_MSGBOX_LABEL_Warning "advertencia ..." 

#endif

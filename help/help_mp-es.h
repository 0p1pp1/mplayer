// Spanish translation by:
// Leandro Lucarella <leandro at lucarella.com.ar>,
// Jes�s Climent <jesus.climent at hispalinux.es>,
// Sefanja Ruijsenaars <sefanja at gmx.net>,
// Andoni Zubimendi <andoni at lpsat.net>

// Updated to help_mp-en.h v1.126

// ========================= MPlayer help ===========================


#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"Uso:   mplayer [opciones] [url o ruta del archivo]\n"
"\n"
"Opciones b�sicas: ('man mplayer' para una lista completa)\n"
" -vo <driver[:disp]>  Seleccionar driver de salida de v�deo y dispositivo ('-vo help' para obtener una lista).\n"
" -ao <driver[:disp]>  Seleccionar driver de salida de audio y dispositivo ('-ao help' para obtener una lista).\n"
#ifdef HAVE_VCD
" vcd://<numpista>      Reproducir pista de (S)VCD (Super Video CD) (acceso directo al dispositivo, no montado)\n"
#endif
#ifdef USE_DVDREAD
" dvd://<n�mero>        Reproducir t�tulo o pista de DVD desde un dispositivo en vez de un archivo regular.\n"
" -alang <lengua>      Seleccionar lengua para el audio del DVD (con c�digo de pa�s de dos caracteres. p. ej. 'es').\n"
" -alang <lengua>      Seleccionar lengua para los subt�tulos del DVD.\n"
#endif
" -ss <tiempo>         Saltar a una posici�n determindada (en segundos o hh:mm:ss).\n"
" -nosound             No reproducir sonido.\n"
" -fs, -vm, -zoom      Opciones de pantalla completa (pantalla completa, cambio de modo de v�deo, escalado por software).\n"
" -x <x> -y <y>        Escalar imagen a resoluci�n dada (para usar con -vm o -zoom).\n"
" -sub <archivo>       Especificar archivo de subtitulos a usar (mira tambi�n -subfps, -subdelay).\n"
" -playlist <archivo>  Especificar archivo con la lista de reproducci�n.\n"
" -vid <x> -aid <y>    Opciones para especificar el stream de v�deo (x) y de audio (y) a reproducir.\n"
" -fps <x> -srate <y>  Opciones para cambiar la tasa de v�deo (x fps) y de audio (y Hz).\n"
" -pp <calidad>        Activar filtro de postprocesado (lee la p�gina man para m�s informaci�n).\n"
" -framedrop           Activar frame dropping (para m�quinas lentas).\n\n"

"Teclas b�sicas ('man mplayer' para una lista completa, mira tambi�n input.conf):\n"
" <-  �  ->       Avanzar o retroceder diez segundos.\n"
" arriba � abajo  Avanzar o retroceder un minuto.\n"
" pgup � pgdown   Avanzar o retroceder diez minutos.\n"
" < � >           Avanzar o retroceder en la lista de reproducci�n.\n"
" p � ESPACIO     Pausar el v�deo (presione cualquier tecla para continuar).\n"
" q � ESC         Detener la reproducci�n y salir del programa.\n"
" + � -           Ajustar el retardo de audio en m�s o menos 0.1 segundos.\n"
" o               Cambiar modo OSD:  nada / b�squeda / b�squeda + tiempo.\n"
" * � /           Aumentar o disminuir el volumen (presione 'm' para elegir entre master/pcm).\n"
" z � x           Ajustar el retardo de la subt�tulaci�n en m�s o menos 0.1 segundos.\n"
" r � t           Ajustar posici�n de la subt�tulaci�n (mira tambi�n -vf expand).\n"
"\n"
" *** Lee la p�gina man para m�s opciones (avanzadas) y teclas! ***\n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// mplayer.c: 

#define MSGTR_Exiting "\nSaliendo...\n"
#define MSGTR_ExitingHow "\nSaliendo... (%s)\n"
#define MSGTR_Exit_quit "Salida."
#define MSGTR_Exit_eof "Fin del archivo."
#define MSGTR_Exit_error "Error fatal."
#define MSGTR_IntBySignal "\nMPlayer ha sido interrumpido por se�al %d en el m�dulo: %s \n"
#define MSGTR_NoHomeDir "No se puede encontrar el directorio HOME.\n"
#define MSGTR_GetpathProblem "Problema en get_path(\"config\").\n"
#define MSGTR_CreatingCfgFile "Creando archivo de configuraci�n: %s.\n"
#define MSGTR_InvalidVOdriver "Nombre del driver de salida de v�deo incorrecto: %s\nUsa '-vo help' para obtener la lista de drivers de salida de v�deo disponibles.\n"
#define MSGTR_InvalidAOdriver "Nombre del driver de salida de audio incorrecto: %s\nUsa '-ao help' para obtener la lista de drivers de salida de audio disponibles.\n"
#define MSGTR_CopyCodecsConf "Copia o enlaza <�rbol del c�digo fuente de MPlayer>/etc/codecs.conf a ~/.mplayer/codecs.conf\n"
#define MSGTR_BuiltinCodecsConf "Usando codecs.conf interno por defecto.\n" 
#define MSGTR_CantLoadFont "No se puede cargar fuente: %s.\n"
#define MSGTR_CantLoadSub "No se puede cargar la subt�tulaci�n: %s.\n"
#define MSGTR_ErrorDVDkey "Error procesando la clave del DVD.\n"
#define MSGTR_CmdlineDVDkey "Se usar� la clave DVD solicitada para la descodificaci�n.\n"
#define MSGTR_DVDauthOk "La secuencia de autorizaci�n del DVD parece estar bien.\n"
#define MSGTR_DumpSelectedStreamMissing "dump: FATAL: No se encuentra el stream seleccionado.\n"
#define MSGTR_CantOpenDumpfile "No se puede abrir el archivo de dump.\n"
#define MSGTR_CoreDumped "Core dumped.\n"
#define MSGTR_FPSnotspecified "FPS no especificado (o inv�lido) en la cabecera! Usa la opci�n -fps.\n"
#define MSGTR_TryForceAudioFmtStr "Tratando de forzar la familia de codecs de audio %d...\n"
#define MSGTR_CantFindAfmtFallback "No se encuentra codec de audio para la familia forzada, se usar�n otros drivers.\n"
#define MSGTR_RTFMCodecs "Lea el archivo DOCS/HTML/es/codecs.html!\n"
#define MSGTR_CantFindAudioCodec "No se encuentra codec para el formato de audio 0x%X!\n"
#define MSGTR_CouldntInitAudioCodec "No se pudo inicializar el codec de audio, no se reproducir� sonido!\n"
#define MSGTR_TryForceVideoFmtStr "Tratando de forzar la familia de codecs de v�deo %d...\n"
#define MSGTR_CantFindVideoCodec "No se encuentra codec para el formato de v�deo 0x%X!\n"
#define MSGTR_CannotInitVO "FATAL: No se puede inicializar el driver de v�deo!\n"
#define MSGTR_CannotInitAO "No se puede abrir o inicializar dispositivo de audio, no se reproducir� sonido.\n"
#define MSGTR_StartPlaying "Empezando reproducci�n...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"       **************************************************************\n"\
"       **** Tu sistema es demasiado lento para reproducir esto!  ****\n"\
"       **************************************************************\n"\
"Posibles razones, problemas, soluciones:\n"\
"- M�s com�n: driver de _audio_ con errores o roto.\n"\
"  - Intenta con -ao sdl, o usa ALSA 0.5 o la emulaci�n OSS de ALSA 0.9.\n"\
"  - Proba con diferentes valores para -autosync, 30 es un buen comienzo.\n"\
"- Salida de v�deo lenta\n"\
"  - Proba otro driver -vo (para obtener una lista, -vo help) o intenta\n"\
"    iniciar con la opci�n -framedrop!\n"\
"- CPU lenta\n"\
"  - No intente reproducir DVDs o DivX grandes en una CPU lenta. Intenta\n"\
"    iniciar con la opci�n -hardframedrop.\n"\
"- Archivo err�neo o roto\n"\
"  - Proba combinaciones de -nobps -ni -mc 0.\n"\
"- Medios lentos (unidad NFS/SMB, DVD, VCD, etc)\n"\
"  - Intente con -cache 8192.\n"\
"- Esta usando -cache para reproducir archivos AVI no entrelazados?\n"\
"  - Intente con -nocache.\n"\
"Lea DOCS/HTML/es/devices.html para consejos de ajuste o mayor velocidad.\n"\
"Si nada de eso sirve, lee DOCS/HTML/es/bugreports.html\n\n"

#define MSGTR_NoGui "MPlayer fue compilado sin soporte para interfaz gr�fica.\n"
#define MSGTR_GuiNeedsX "La interfaz gr�fica de MPlayer requiere X11!\n"
#define MSGTR_Playing "Reproduciendo %s.\n"
#define MSGTR_NoSound "Audio: sin sonido.\n"
#define MSGTR_FPSforced "FPS forzado en %5.3f  (ftime: %5.3f).\n"

#define MSGTR_CompiledWithRuntimeDetection "Compilado con detecci�n en tiempo de ejecuci�n de CPU - esto no es �ptimo! Para obtener mejor rendimiento, recompila MPlayer con --disable-runtime-cpudetection.\n"
#define MSGTR_CompiledWithCPUExtensions "Compilado para CPU x86 con extensiones:"
#define MSGTR_AvailableVideoOutputDrivers "Drivers de salida de v�deo disponibles:\n"
#define MSGTR_AvailableAudioOutputDrivers "Drivers de salida de audio disponibles:\n"
#define MSGTR_AvailableAudioCodecs "Codecs de audio disponibles:\n"
#define MSGTR_AvailableVideoCodecs "Codecs de v�deo disponibles:\n"
#define MSGTR_AvailableAudioFm "\nFamilias/drivers de codecs de audio (compilados dentro de MPlayer) disponibles:\n"
#define MSGTR_AvailableVideoFm "\nFamilias/drivers de codecs de v�deo (compilados dentro de MPlayer) disponibles:\n"
#define MSGTR_AvailableFsType "Modos disponibles de cambio a pantalla completa:\n"
#define MSGTR_UsingRTCTiming "Usando el RTC timing por hardware de Linux (%ldHz).\n"
#define MSGTR_CannotReadVideoProperties "V�deo: no se puede leer las propiedades.\n"
#define MSGTR_NoStreamFound "No se ha encontrado stream.\n"
#define MSGTR_InitializingAudioCodec "Inicializando codec de audio...\n"
#define MSGTR_ErrorInitializingVODevice "Error abriendo/inicializando el dispositivo de la salida de v�deo (-vo)!\n"
#define MSGTR_ForcedVideoCodec "Forzado el codec de v�deo: %s.\n"
#define MSGTR_ForcedAudioCodec "Forzado el codec de audio: %s\n"
#define MSGTR_AODescription_AOAuthor "AO: Descripci�n: %s\nAO: Autor: %s.\n"
#define MSGTR_AOComment "AO: Comentario: %s.\n"
#define MSGTR_Video_NoVideo "V�deo: no hay v�deo!\n"
#define MSGTR_NotInitializeVOPorVO "\nFATAL: No se pudieron inicializar los filtros de v�deo (-vf) o de salida de v�deo (-vo)!\n"
#define MSGTR_Paused "\n  =====  PAUSA  =====\r"
#define MSGTR_PlaylistLoadUnable "\nNo se puede cargar la lista de reproducci�n %s.\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- MPlayer se detuvo por una 'Instrucci�n Ilegal'.\n"\
"  Esto puede ser un defecto en nuestra rutina nueva de autodetecci�n de CPU...\n"\
"  Por favor lee DOCS/HTML/es/bugreports.html.\n"
#define MSGTR_Exit_SIGILL \
"- MPlayer se detuvo por una 'Instrucci�n Ilegal'.\n"\
"  Esto ocurre normalmente cuando ejecuta el programa en una CPU diferente de\n"\
"  la cual MPlayer fue compilado/optimizado.\n"\
"  �Verifica eso!\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- MPlayer se detuvo por mal uso de CPU/FPU/RAM.\n"\
"  Recompila MPlayer con la opci�n --enable-debug y hace un backtrace en\n"\
"  'gdb' y un desensamblado. Para m�s detalles, vea\n"\
"  DOCS/HTML/es/bugreports_what.html#bugreports_crash\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer se detuvo. Esto no deber�a haber pasado.\n"\
"  Puede ser un defecto en el c�digo de MPlayer _o_ en sus controladores\n"\
"  _o_ en su versi�n de gcc. Si piensa que es la culpa de MPlayer, por\n"\
"  favor lea DOCS/HTML/es/bugreports.html y siga las instrucciones que all�\n"\
"  se encuentran. No podemos y no lo ayudaremos a menos que nos provea esa\n"\
"  informaci�n cuando este reportando alg�n posible defecto.\n"

#define MSGTR_EdlCantUseBothModes "Imposible usar -edl y -edlout al mismo tiempo.\n"
#define MSGTR_EdlOutOfMem "No hay memoria suficiente para almacenar los datos EDL.\n"
#define MSGTR_EdlRecordsNo "Leidas %d acciones EDL.\n"
#define MSGTR_EdlQueueEmpty "No hay acciones EDL de las que ocuparse.\n"
#define MSGTR_EdlCantOpenForWrite "Error tratando de escribir en [%s].\n"
#define MSGTR_EdlCantOpenForRead "Error tratando de leer desde [%s].\n"
#define MSGTR_EdlNOsh_video "Imposible usar EDL sin video.\n"
#define MSGTR_EdlNOValidLine "Linea EDL inv�lida: %s\n"
#define MSGTR_EdlBadlyFormattedLine "Ignorando linea EDL mal formateada [%d].\n"
#define MSGTR_EdlBadLineOverlap "Ultima posici�n de parada fue [%f]; pr�xima "\
"posici�n de partida es [%f]. Las operaciones deben estar en orden cronol�gico"\
", sin sobreponerse, ignorando.\n"
#define MSGTR_EdlBadLineBadStop "La posici�n de parada debe ser posterior a la"\
" posici�n de partida.\n"

// mencoder.c:

#define MSGTR_UsingPass3ControllFile "Usando el archivo de control pass3: %s\n"
#define MSGTR_MissingFilename "\nFalta el nombre del archivo.\n\n"
#define MSGTR_CannotOpenFile_Device "No se pudo abrir el archivo o el dispositivo.\n"
#define MSGTR_ErrorDVDAuth "Error en la autorizaci�n DVD...\n"
#define MSGTR_CannotOpenDemuxer "No se pudo abrir el demuxer.\n"
#define MSGTR_NoAudioEncoderSelected \
"\nNo se ha selecciono codificador de audio (-oac).\n"\
"  Escoge uno (mira -oac help) o usa -nosound.\n"
#define MSGTR_NoVideoEncoderSelected \
"\nNo se ha selecciono codificador de video (-ovc). \n"\
"  Escoge uno (mira -ovc help)\n"
#define MSGTR_InitializingAudioCodec "Inicializando codec de audio...\n"
#define MSGTR_CannotOpenOutputFile "No se puede abrir el archivo de salida'%s'.\n"
#define MSGTR_EncoderOpenFailed "No se pudo abrir el codificador.\n"
#define MSGTR_ForcingOutputFourcc "Forzando salida fourcc a %x [%.4s].\n"
#define MSGTR_WritingAVIHeader "Escribiendo cabecera AVI...\n"
#define MSGTR_DuplicateFrames "\n%d frame(s) duplicados.\n"
#define MSGTR_SkipFrame "\nse salta frame...\n"
#define MSGTR_ErrorWritingFile "%s: error escribiendo el archivo.\n"
#define MSGTR_WritingAVIIndex "\nEscribiendo index AVI...\n"
#define MSGTR_FixupAVIHeader "Arreglando cabecera AVI..\n"
#define MSGTR_RecommendedVideoBitrate "Bitrate recomendado para %s CD: %d.\n"
#define MSGTR_VideoStreamResult "\nStream de v�deo: %8.3f kbit/s (%d bps), tama�o: %d bytes, %5.3f segundos, %d frames\n"
#define MSGTR_AudioStreamResult "\nStream de audio: %8.3f kbit/s (%d bps), tama�o: %d bytes, %5.3f segundos\n"
#define MSGTR_OpenedStream "exito: formato: %d  datos: 0x%X - 0x%x\n"
#define MSGTR_VCodecFramecopy "codec de video: copia de cuadro (%dx%d %dbpp fourcc=%x)\n"
#define MSGTR_ACodecFramecopy "codec de audio: copia de cuadro (formato=%x canales=%d raz�n=%ld bits=%d bps=%ld muestra-%ld)\n"
#define MSGTR_CBRPCMAudioSelected "audio PCM CBR seleccionado\n"
#define MSGTR_MP3AudioSelected "audio MP3 seleccionado\n"
#define MSGTR_CannotAllocateBytes "No se pueden asignar %d bytes\n"
#define MSGTR_SettingAudioDelay "Ajustando el RETRASO DEL AUDIO a %5.3f\n"
#define MSGTR_SettingAudioInputGain "Ajustando la ganancia de entrada de audio input a %f\n"
#define MSGTR_LamePresetEquals "\npreconfiguraci�n=%s\n\n"
#define MSGTR_LimitingAudioPreload "Limitando la pre-carda de audio a 0.4s\n"
#define MSGTR_IncreasingAudioDensity "Incrementando la densidad de audio a 4\n"
#define MSGTR_ZeroingAudioPreloadAndMaxPtsCorrection "Forzando la precarga de audio a 0, puntos m�ximos de correcci�n ajustado a 0\n"
#define MSGTR_CBRAudioByterate "\n\naudio CBR: %ld bytes/seg, %d bytes/bloque\n"
#define MSGTR_LameVersion "versi�n de LAME %s (%s)\n\n"
#define MSGTR_InvalidBitrateForLamePreset "Error: La tasa de bits especificada esta fuera de los rangos de valor para esta preconfiguraci�n\n"\
"\n"\
"Cuando usa este modo debe ingresar un valor entre \"8\" y \"320\"\n"\
"\n"\
"Para mayor informci�n prueba: \"-lameopts preset=help\"\n"
#define MSGTR_InvalidLamePresetOptions "Error: No ingreso un perfil valido y/o opciones con una preconfiguraci�n\n"\
"\n"\
"Los perfiles disponibles son:\n"\
"\n"\
"   <fast>        standard\n"\
"   <fast>        extreme\n"\
"                 insane\n"\
"   <cbr> (Modo ABR) - Implica el Modo ABR Mode. Para usarlo,\n"\
"                      solamente especifique la tasa de bits.. Por ejemplo:\n"\
"                      \"preset=185\" activates esta\n"\
"                      preconfiguraci�n y usa 185 como el kbps promedio.\n"\
"\n"\
"    Algunos ejemplos:\n"\
"\n"\
"    \"-lameopts fast:preset=standard  \"\n"\
" o  \"-lameopts  cbr:preset=192       \"\n"\
" o  \"-lameopts      preset=172       \"\n"\
" o  \"-lameopts      preset=extreme   \"\n"\
"\n"\
"Para mayor informaci�n pruebe: \"-lameopts preset=help\"\n"
#define MSGTR_LamePresetsLongInfo "\n"\
"Las opciones de preconfiguraci�n estan hechas para proveer la mayor calidad posible.\n"\
"\n"\
"Estas han sido en su mayor sometidas y ajustadas por medio de pruebas rigurosas de\n"\
"doble escucha ciega (double blind listening) para verificar y lograr este objetivo\n"\
"\n"\
"Son continuamente actualizadas para con el desarrollo actual que esta\n"\
"ocurriendo y como resultado deber�a proveer practicamente la mejor calidad\n"\
"actualmente posible de LAME.\n"\
"\n"\
"Para activar estas preconfiguraci�n:\n"\
"\n"\
"   For modos VBR (en general mayor calidad):\n"\
"\n"\
"     \"preset=standard\" Esta preconfiguraci�n generalmente deber�a ser transparente\n"\
"                             para la mayor�a de la genta en la m�sica y ya es bastante\n"\
"                             buena en calidad.\n"\
"\n"\
"     \"preset=extreme\" Si tiene un oido extramademente bueno y un equipo\n"\
"                             similar, esta preconfiguraci�n normalmente le\n"\
"                             proveera una calidad levemente superior al modo "\
                               "\"standard\"\n"\
"\n"\
"   Para 320kbps CBR (la mayor calidad posible desde las preconfiguraciones):\n"\
"\n"\
"     \"preset=insane\"  Esta preconfiguraci�n ser� excesiva para la mayoria\n"\
"                             de la gente en la mayoria de las ocasiones, pero si debe\n"\
"                             tener la mayor calidad posible sin tener en cuenta el\n"\
"                             tama�o del archivo, esta es la opci�n definitiva.\n"\
"\n"\
"   Para modos ABR (alta calidad por tasa de bits dado pero no tan alto como modo VBR):\n"\
"\n"\
"     \"preset=<kbps>\"  Usando esta preconfiguraci�n normalmente obtendr� buena\n"\
"                             calidad a la tasa de bits especificada. Dependiendo de\n"\
"                             la tasa de bits ingresada, esta preconfiguraci�n determinar�\n"\
"                             las opciones optimas para esa situaci�n particular.\n"\
"                             A pessar que funciona, no es tan flexible como el modo\n"\
"                             VBR, y normalmente no llegar�n a obtener el mismo nivel\n"\
"                             de calidad del modo VBR a mayores tasas de bits.\n"\
"\n"\
"Las siguientes opciones tambi�n est�n disponibles para los correspondientes perfiles:\n"\
"\n"\
"   <fast>        standard\n"\
"   <fast>        extreme\n"\
"                 insane\n"\
"   <cbr> (Modo ABR) - Implica el Modo ABR Mode. Para usarlo,\n"\
"                      solamente especifique la tasa de bits.. Por ejemplo:\n"\
"                      \"preset=185\" activates esta\n"\
"                      preconfiguraci�n y usa 185 como el kbps promedio.\n"\
"\n"\
"   \"fast\" - Activa el nuevo modo r�pido VBR para un perfil en particular. La\n"\
"            desventaja al cambio de velocidad es que muchas veces la tasa de\n"\
"            bits ser� levemente m�s alta respecto del modo normal y la calidad\n"\
"            puede llegar a ser un poco m�s baja tambi�n.\n"\
"Advertencia: con la versi�n actual las preconfiruaciones \"fast\" puede llegar a\n"\
"             dar como resultado tasas de bits demasiado altas comparadas a los normales.\n"\
"\n"\
"   \"cbr\"  - Si usa el modo ABR (ver m�s arriba) con una tasa de bits significativa\n"\
"            como 80, 96, 112, 128, 160, 192, 224, 256, 320,\n"\
"            puede usar la opci�n \"cbr\" para forzar la codificaci�n en modo CBR\n"\
"            en lugar del modo por omisi�n ABR. ABR provee mayor calidad pero\n"\
"            CBR podr�a llegar a ser util en  situaciones tales como cuando se\n"\
"            recibe un flujo mp3 a trav�s de internet.\n"\
"\n"\
"    Por ejemplo:\n"\
"\n"\
"    \"-lameopts fast:preset=standard  \"\n"\
" o  \"-lameopts  cbr:preset=192       \"\n"\
" o  \"-lameopts      preset=172       \"\n"\
" o  \"-lameopts      preset=extreme   \"\n"\
"\n"\
"\n"\
"Unos poco alias estan disponibles para el modo ABR:\n"\
"phone => 16kbps/mono        phon+/lw/mw-eu/sw => 24kbps/mono\n"\
"mw-us => 40kbps/mono        voice => 56kbps/mono\n"\
"fm/radio/tape => 112kbps    hifi => 160kbps\n"\
"cd => 192kbps               studio => 256kbps"
#define MSGTR_ConfigfileError "error en archivo de configuraci�n"
#define MSGTR_ErrorParsingCommandLine "error en parametros de la l�nea de comando"
#define MSGTR_VideoStreamRequired "�El flujo de video es obligatorio!\n"
#define MSGTR_ForcingInputFPS "en su lugar los cuadros por segundos de entrada ser�n interpretados como %5.2f\n"
#define MSGTR_RawvideoDoesNotSupportAudio "El formato de archivo de salida RAWVIDEO no soporta audio - desactivando audio\n"
#define MSGTR_DemuxerDoesntSupportNosound "Este demuxer todav�a no soporta -nosound.\n"
#define MSGTR_MemAllocFailed "fall� la asignaci�n de memoria"
#define MSGTR_NoMatchingFilter "�No se encontr� filtro o formato de salida concordante!\n"
#define MSGTR_MP3WaveFormatSizeNot30 "sizeof(MPEGLAYER3WAVEFORMAT)==%d!=30, �quiza este fallado el compilador de C?\n"
#define MSGTR_NoLavcAudioCodecName "LAVC Audio,�falta el nombre del codec!\n"
#define MSGTR_LavcAudioCodecNotFound "LAVC Audio, no se encuentra el codificador para el codec %s\n"
#define MSGTR_CouldntAllocateLavcContext "LAVC Audio, �no se puede asignar contexto!\n"
#define MSGTR_CouldntOpenCodec "No se puede abrir el codec %s, br=%d\n"
#define MSGTR_FramesizeBufsizeTag "TAMA�O_DEL_CUADRO: %d, TAMA�O_DEL_BUFFER: %d, ETIQUETA: 0x%x\n"

// cfg-mencoder.h

#define MSGTR_MEncoderMP3LameHelp "\n\n"\
" vbr=<0-4>     m�todo de tasa de bits variable\n"\
"                0: cbr\n"\
"                1: mt\n"\
"                2: rh(default)\n"\
"                3: abr\n"\
"                4: mtrh\n"\
"\n"\
" abr           tasa de bits media\n"\
"\n"\
" cbr           tasa de bits constante\n"\
"               Forzar tambi�n modo de codificaci�n CBR en modos ABR \n"\
"               preseleccionados subsecuentemente.\n"\
"\n"\
" br=<0-1024>   especifica tasa de bits en kBit (solo CBR y ABR)\n"\
"\n"\
" q=<0-9>       calidad (0-mejor, 9-peor) (solo para VBR)\n"\
"\n"\
" aq=<0-9>      calidad del algoritmo (0-mejor/lenta, 9-peor/r�pida)\n"\
"\n"\
" ratio=<1-100> raz�n de compresi�n\n"\
"\n"\
" vol=<0-10>    configura ganancia de entrada de audio\n"\
"\n"\
" mode=<0-3>    (por defecto: auto)\n"\
"                0: est�reo\n"\
"                1: est�reo-junto\n"\
"                2: canal dual\n"\
"                3: mono\n"\
"\n"\
" padding=<0-2>\n"\
"                0: no\n"\
"                1: todo\n"\
"                2: ajustar\n"\
"\n"\
" fast          Activa codificaci�n r�pida en modos VBR preseleccionados\n"\
"               subsecuentes, m�s baja calidad y tasas de bits m�s altas.\n"\
"\n"\
" preset=<value> Provee configuracion con la mejor calidad posible.\n"\
"                 medium: codificaci�n VBR, buena calidad\n"\
"                 (rango de 150-180 kbps de tasa de bits)\n"\
"                 standard:  codificaci�n VBR, alta calidad\n"\
"                 (rango de 170-210 kbps de tasa de bits)\n"\
"                 extreme: codificaci�n VBR, muy alta calidad\n"\
"                 (rango de 200-240 kbps de tasa de bits)\n"\
"                 insane:  codificaci�n CBR, la mejor calidad configurable\n"\
"                 (320 kbps de tasa de bits)\n"\
"                 <8-320>: codificaci�n ABR con tasa de bits en promedio en los kbps dados.\n\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "Dispositivo de CD-ROM '%s' no encontrado.\n"
#define MSGTR_ErrTrackSelect "Error seleccionando la pista de VCD!"
#define MSGTR_ReadSTDIN "Leyendo desde la entrada est�ndar (stdin)...\n"
#define MSGTR_UnableOpenURL "No se puede abrir URL: %s\n"
#define MSGTR_ConnToServer "Connectado al servidor: %s\n"
#define MSGTR_FileNotFound "Archivo no encontrado: '%s'\n"

#define MSGTR_SMBInitError "No se puede inicializar la librer�a libsmbclient: %d\n"
#define MSGTR_SMBFileNotFound "No se puede abrir desde la RED: '%s'\n"
#define MSGTR_SMBNotCompiled "MPlayer no fue compilado con soporte de lectura de SMB.\n"

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
#define MSGTR_DVDopenOk "DVD abierto.\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "Advertencia! Cabecera de stream de audio %d redefinida!\n"
#define MSGTR_VideoStreamRedefined "Advertencia! Cabecera de stream de video %d redefinida!\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: Demasiados (%d en %d bytes) paquetes de audio en el buffer!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: Demasiados (%d en %d bytes) paquetes de video en el buffer!\n"
#define MSGTR_MaybeNI "�Est�s reproduciendo un stream o archivo 'non-interleaved' o fall� el codec?\n " \
		"Para archivos .AVI, intente forzar el modo 'non-interleaved' con la opci�n -ni.\n"
#define MSGTR_SwitchToNi "\nDetectado .AVI mal interleaveado - cambiando al modo -ni!\n"
#define MSGTR_Detected_XXX_FileFormat "Detectado formato de archivo %s.\n"
#define MSGTR_DetectedAudiofile "Detectado archivo de audio.\n"
#define MSGTR_NotSystemStream "Esto no es formato MPEG System Stream... (tal vez Transport Stream?)\n"
#define MSGTR_InvalidMPEGES "Stream MPEG-ES inv�lido? Contacta con el autor, podr�a ser un fallo.\n"
#define MSGTR_FormatNotRecognized "Este formato no est� soportado o reconocido. Si este archivo es un AVI, ASF o MPEG, por favor contacte con el autor.\n"
#define MSGTR_MissingVideoStream "�No se encontr� stream de video!\n"
#define MSGTR_MissingAudioStream "No se encontr� el stream de audio, no se reproducir� sonido.\n"
#define MSGTR_MissingVideoStreamBug "��Stream de video perdido!? Contacta con el autor, podr�a ser un fallo.\n"

#define MSGTR_DoesntContainSelectedStream "demux: El archivo no contiene el stream de audio o video seleccionado.\n"

#define MSGTR_NI_Forced "Forzado"
#define MSGTR_NI_Detected "Detectado"
#define MSGTR_NI_Message "%s formato de AVI 'NON-INTERLEAVED'.\n"

#define MSGTR_UsingNINI "Usando formato de AVI roto 'NON-INTERLEAVED'.\n"
#define MSGTR_CouldntDetFNo "No se puede determinar el n�mero de cuadros (para una b�squeda absoluta).\n"
#define MSGTR_CantSeekRawAVI "No se puede avanzar o retroceder en un stream crudo .AVI (se requiere �ndice, prueba con -idx).\n"
#define MSGTR_CantSeekFile "No se puede avanzar o retroceder en este archivo.\n"
#define MSGTR_EncryptedVOB "�Archivo VOB encriptado! Lea DOCS/HTML/es/dvd.html.\n"
#define MSGTR_MOVcomprhdr "MOV: �Soporte de Cabecera comprimida requiere ZLIB!.\n"
#define MSGTR_MOVvariableFourCC "MOV: Advertencia. �Variable FOURCC detectada!\n"
#define MSGTR_MOVtooManyTrk "MOV: Advertencia. �Demasiadas pistas!"
#define MSGTR_FoundAudioStream "==> Encontrado stream de audio: %d\n"
#define MSGTR_FoundVideoStream "==> Encontrado stream de v�deo: %d\n"
#define MSGTR_DetectedTV "Detectado TV.\n"
#define MSGTR_ErrorOpeningOGGDemuxer "No se puede abrir el demuxer ogg.\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: Buscando stream de audio (id:%d)\n"
#define MSGTR_CannotOpenAudioStream "No se puede abrir stream de audio: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "No se puede abrir stream de subt�tulos: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "No se pudo abrir el demuxer de audio: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "No se pudo abrir demuxer de subt�tulos: %s\n"
#define MSGTR_TVInputNotSeekable "Entrada de TV no es buscable.\n"
#define MSGTR_DemuxerInfoAlreadyPresent "Informaci�n de demuxer %s ya est� disponible.\n"
#define MSGTR_ClipInfo "Informaci�n de clip: \n"
#define MSGTR_LeaveTelecineMode "\ndemux_mpg: contenido NTSC de 30cps detectado, cambiando cuadros por segundo.\n"
#define MSGTR_EnterTelecineMode "\ndemux_mpg: contenido NTSC progresivo de 24cps detectado, cambiando cuadros por segundo.\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "No se pudo abrir codec.\n"
#define MSGTR_CantCloseCodec "No se pudo cerrar codec.\n"

#define MSGTR_MissingDLLcodec "ERROR: No se pudo abrir el codec DirectShow requerido: %s\n"
#define MSGTR_ACMiniterror "No se puede cargar/inicializar codecs de audio Win32/ACM (falta archivo DLL?)\n"
#define MSGTR_MissingLAVCcodec "No se encuentra codec '%s' en libavcodec...\n"


#define MSGTR_MpegNoSequHdr "MPEG: FATAL: EOF mientras buscaba la cabecera de secuencia.\n"
#define MSGTR_CannotReadMpegSequHdr "FATAL: No se puede leer cabecera de secuencia.\n"
#define MSGTR_CannotReadMpegSequHdrEx "FATAL: No se puede leer la extensi�n de la cabecera de secuencia.\n"
#define MSGTR_BadMpegSequHdr "MPEG: Mala cabecera de secuencia.\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: Mala extensi�n de la cabecera de secuencia.\n"

#define MSGTR_ShMemAllocFail "No se puede alocar memoria compartida.\n"
#define MSGTR_CantAllocAudioBuf "No se puede alocar buffer de la salida de audio.\n"
#define MSGTR_UnknownAudio "Formato de audio desconocido/perdido, no se reproducir� sonido.\n"


#define MSGTR_UsingExternalPP "[PP] Usando filtro de postprocesado externo, max q = %d.\n"
#define MSGTR_UsingCodecPP "[PP] Usando postprocesado del codec, max q = %d.\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "Atributo de v�deo '%s' no es soportado por -vo y -vd actuales. \n"
#define MSGTR_VideoCodecFamilyNotAvailableStr "Familia de codec de v�deo solicitada [%s] (vfm=%s) no est� disponible (act�valo al compilar).\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "Familia de codec de audio solicitada [%s] (afm=%s) no est� disponible (act�valo al compilar).\n"
#define MSGTR_OpeningVideoDecoder "Abriendo descodificador de v�deo: [%s] %s.\n"
#define MSGTR_OpeningAudioDecoder "Abriendo descodificador de audio: [%s] %s.\n"
#define MSGTR_UninitVideoStr "uninit video: %s.\n"
#define MSGTR_UninitAudioStr "uninit audio: %s.\n"
#define MSGTR_VDecoderInitFailed "Inicializaci�n del VDecoder ha fallado.\n"
#define MSGTR_ADecoderInitFailed "Inicializaci�n del ADecoder ha fallado.\n"
#define MSGTR_ADecoderPreinitFailed "Preinicializaci�n del ADecoder ha fallado.\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: Alocando %d bytes para el b�fer de entrada.\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: Allocating %d + %d = %d bytes para el b�fer de salida.\n"


// LIRC:
#define MSGTR_SettingUpLIRC "Configurando soporte para LIRC...\n"
#define MSGTR_LIRCdisabled "No podr�s usar el control remoto.\n"
#define MSGTR_LIRCopenfailed "Fallo al abrir el soporte para LIRC.\n"
#define MSGTR_LIRCcfgerr "Fallo al leer archivo de configuraci�n de LIRC %s.\n"


// vf.c
#define MSGTR_CouldNotFindVideoFilter "No se pudo encontrar el filtro de v�deo '%s'.\n"
#define MSGTR_CouldNotOpenVideoFilter "No se pudo abrir el filtro de v�deo '%s'.\n"
#define MSGTR_OpeningVideoFilter "Abriendo filtro de v�deo: "
#define MSGTR_CannotFindColorspace "No se pudo encontrar espacio de color concordante, ni siquiera insertando 'scale' :(.\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDec: el codec no declar� sh->disp_w y sh->disp_h, intentando solucionarlo!\n"
#define MSGTR_VoConfigRequest "VDec: vo solicitud de config - %d x %d (csp preferida: %s).\n"
#define MSGTR_CouldNotFindColorspace "No se pudo encontrar colorspace concordante - reintentando escalado -vf...\n"
#define MSGTR_MovieAspectIsSet "Aspecto es %.2f:1 - prescalando a aspecto correcto.\n"
#define MSGTR_MovieAspectUndefined "Aspecto de pel�cula no es definido - no se ha aplicado prescalado.\n"

// vd_dshow.c, vd_dmo.c
#define MSGTR_DownloadCodecPackage "Necesita actualizar/instalar el paquete binario con codecs.\n Dirijase a http://mplayerhq.hu/homepage/dload.html\n"
#define MSGTR_DShowInitOK "INFO: Inicializaci�n correcta de codec de v�deo Win32/DShow.\n"
#define MSGTR_DMOInitOK "INFO: Inicializaci�n correcta de codec de v�deo Win32/DMO.\n"

// x11_common.c
#define MSGTR_EwmhFullscreenStateFailed "\nX11: �No se pudo enviar evento de pantalla completa EWMH!\n"

#define MSGTR_InsertingAfVolume "[Mixer] No mezclador de volumen por hardware, insertando filtro de volumen.\n"
#define MSGTR_NoVolume "[Mixer] Na hay control de volumen disponible.\n"

// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "Acerca de"
#define MSGTR_FileSelect "Seleccionar archivo..."
#define MSGTR_SubtitleSelect "Seleccionar subt�tulos..."
#define MSGTR_OtherSelect "Seleccionar..."
#define MSGTR_AudioFileSelect "Seleccionar canal de audio externo..."
#define MSGTR_FontSelect "Seleccionar fuente..."
#define MSGTR_PlayList "Lista de reproducci�n"
#define MSGTR_Equalizer "Equalizador"
#define MSGTR_SkinBrowser "Navegador de skins"
#define MSGTR_Network "Streaming por red..."
#define MSGTR_Preferences "Preferencias"
#define MSGTR_AudioPreferences "Configuraci�n de controlador de Audio"
#define MSGTR_NoMediaOpened "no se abri� audio/v�deo"
#define MSGTR_VCDTrack "pista VCD %d"
#define MSGTR_NoChapter "sin cap�tulo"
#define MSGTR_Chapter "cap�tulo %d"
#define MSGTR_NoFileLoaded "no se ha cargado ning�n archivo"


// --- buttons ---
#define MSGTR_Ok "Ok"
#define MSGTR_Cancel "Cancelar"
#define MSGTR_Add "Agregar"
#define MSGTR_Remove "Quitar"
#define MSGTR_Clear "Limpiar"
#define MSGTR_Config "Configurar"
#define MSGTR_ConfigDriver "Configurar driver"
#define MSGTR_Browse "Navegar"

// --- error messages ---
#define MSGTR_NEMDB "No hay suficiente memoria para dibujar el b�fer."
#define MSGTR_NEMFMR "No hay suficiente memoria para dibujar el men�."
#define MSGTR_IDFGCVD "No se encuentra driver -vo compatible con la interfaz gr�fica."
#define MSGTR_NEEDLAVCFAME "No puede reproducir archivos no MPEG con su DXR3/H+ sin recodificaci�n. Activa lavc o fame en la configuraci�n del DXR3/H+."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[skin] error en configuraci�n de skin en la l�nea %d: %s"
#define MSGTR_SKIN_WARNING1 "[skin] advertencia en archivo de configuraci�n en la l�nea %d:\n control (%s) encontrado pero no se encontro \"section\" antes"
#define MSGTR_SKIN_WARNING2 "[skin] advertencia en archivo de configuraci�n en la l�nea %d:\n control (%s) encontrado pero no se encontro \"subsection\" antes"
#define MSGTR_SKIN_WARNING3 "[skin] advertencia en archivo de configuraci�n en la linea %d:\nesta subsecci�n no esta soportada por control (%s)"
#define MSGTR_SKIN_BITMAP_16bit  "Mapa de bits de 16 bits o menos no soportado (%s).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "Archivo no encontrado (%s).\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "Error al leer BMP (%s).\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "Error al leer TGA (%s).\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "Error al leer PNG (%s).\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "RLE packed TGA no soportado (%s).\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "Tipo de archivo desconocido (%s)\n"
#define MSGTR_SKIN_BITMAP_ConvertError "Error de conversi�n de 24 bit a 32 bit (%s).\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "Mensaje desconocido: %s.\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "No hay suficiente memoria.\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "Demasiadas fuentes declaradas.\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "Archivo de fuentes no encontrado.\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "Archivo de imagen de fuente noi encontrado.\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "identificador de fuente no existente (%s).\n"
#define MSGTR_SKIN_UnknownParameter "par�metro desconocido (%s)\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[skinbrowser] no hay suficiente memoria\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "Skin no encontrado (%s).\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "Error de lectura del archivo de configuraci�n del skin (%s).\n"
#define MSGTR_SKIN_LABEL "Skins:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "Sobre MPlayer"
#define MSGTR_MENU_Open "Abrir..."
#define MSGTR_MENU_PlayFile "Reproducir archivo..."
#define MSGTR_MENU_PlayVCD "Reproducir VCD..."
#define MSGTR_MENU_PlayDVD "Reproducir DVD..."
#define MSGTR_MENU_PlayURL "Reproducir URL..."
#define MSGTR_MENU_LoadSubtitle "Cargar subt�tulos..."
#define MSGTR_MENU_DropSubtitle "Cancelar subtitulos..."
#define MSGTR_MENU_LoadExternAudioFile "Cargar archivo de audio externo..."
#define MSGTR_MENU_Playing "Reproduciendo"
#define MSGTR_MENU_Play "Reproducir"
#define MSGTR_MENU_Pause "Pausa"
#define MSGTR_MENU_Stop "Parar"
#define MSGTR_MENU_NextStream "Siguiente stream"
#define MSGTR_MENU_PrevStream "Anterior stream"
#define MSGTR_MENU_Size "Tama�o"
#define MSGTR_MENU_NormalSize "Tama�o normal"
#define MSGTR_MENU_DoubleSize "Tama�o doble"
#define MSGTR_MENU_FullScreen "Pantalla completa"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "Reproducir disco..."
#define MSGTR_MENU_ShowDVDMenu "Mostrar men� DVD"
#define MSGTR_MENU_Titles "T�tulos"
#define MSGTR_MENU_Title "T�tulo %2d"
#define MSGTR_MENU_None "(ninguno)"
#define MSGTR_MENU_Chapters "Cap�tulos"
#define MSGTR_MENU_Chapter "Cap�tulo %2d"
#define MSGTR_MENU_AudioLanguages "Idiomas de audio"
#define MSGTR_MENU_SubtitleLanguages "Idiomas de subt�tulos"
#define MSGTR_MENU_PlayList "Lista de reproducci�n"
#define MSGTR_MENU_SkinBrowser "Navegador de skins"
#define MSGTR_MENU_Preferences "Preferencias"
#define MSGTR_MENU_Exit "Salir..."
#define MSGTR_MENU_Mute "Mudo"
#define MSGTR_MENU_Original "Original"
#define MSGTR_MENU_AspectRatio "Relaci�n de aspecto"
#define MSGTR_MENU_AudioTrack "Pista de Audio"
#define MSGTR_MENU_Track "Pista %d"
#define MSGTR_MENU_VideoTrack "Pista de Video"


// --- equalizer
#define MSGTR_EQU_Audio "Audio"
#define MSGTR_EQU_Video "Video"
#define MSGTR_EQU_Contrast "Contraste: "
#define MSGTR_EQU_Brightness "Brillo: "
#define MSGTR_EQU_Hue "Hue: "
#define MSGTR_EQU_Saturation "Saturaci�n: "
#define MSGTR_EQU_Front_Left "Frente izquierdo"
#define MSGTR_EQU_Front_Right "Frente derecho"
#define MSGTR_EQU_Back_Left "Fondo izquierdo"
#define MSGTR_EQU_Back_Right "Fondo dercho"
#define MSGTR_EQU_Center "Centro"
#define MSGTR_EQU_Bass "Bajo"
#define MSGTR_EQU_All "Todos"
#define MSGTR_EQU_Channel1 "Canal 1:"
#define MSGTR_EQU_Channel2 "Canal 2:"
#define MSGTR_EQU_Channel3 "Canal 3:"
#define MSGTR_EQU_Channel4 "Canal 4:"
#define MSGTR_EQU_Channel5 "Canal 5:"
#define MSGTR_EQU_Channel6 "Canal 6:"

// --- playlist
#define MSGTR_PLAYLIST_Path "Ubicaci�n"
#define MSGTR_PLAYLIST_Selected "Archivos seleccionados"
#define MSGTR_PLAYLIST_Files "Archivos"
#define MSGTR_PLAYLIST_DirectoryTree "�rbol de directorios"

// --- preferences
#define MSGTR_PREFERENCES_Audio "Audio"
#define MSGTR_PREFERENCES_Video "V�deo"
#define MSGTR_PREFERENCES_SubtitleOSD "Subt�tulos y OSD"
#define MSGTR_PREFERENCES_Codecs "Codecs y demuxer"
#define MSGTR_PREFERENCES_Misc "Misc"

#define MSGTR_PREFERENCES_None "Ninguno"
#define MSGTR_PREFERENCES_DriverDefault "controlador por omisi�n"
#define MSGTR_PREFERENCES_AvailableDrivers "Drivers disponibles:"
#define MSGTR_PREFERENCES_DoNotPlaySound "No reproducir sonido"
#define MSGTR_PREFERENCES_NormalizeSound "Normalizar sonido"
#define MSGTR_PREFERENCES_EnEqualizer "Activar equalizer"
#define MSGTR_PREFERENCES_ExtraStereo "Activar estereo extra"
#define MSGTR_PREFERENCES_Coefficient "Coeficiente:"
#define MSGTR_PREFERENCES_AudioDelay "Retraso de audio"
#define MSGTR_PREFERENCES_DoubleBuffer "Activar buffering doble"
#define MSGTR_PREFERENCES_DirectRender "Activar renderizaci�n directa"
#define MSGTR_PREFERENCES_FrameDrop "Activar frame dropping"
#define MSGTR_PREFERENCES_HFrameDrop "Activar frame dropping DURO (peligroso)"
#define MSGTR_PREFERENCES_Flip "Visualizar imagen al rev�s"
#define MSGTR_PREFERENCES_Panscan "Panscan: "
#define MSGTR_PREFERENCES_OSDTimer "Timer e indicadores"
#define MSGTR_PREFERENCES_OSDProgress "S�lo barra de progreso"
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "Timer, porcentaje y tiempo total"
#define MSGTR_PREFERENCES_Subtitle "Subt�tulo:"
#define MSGTR_PREFERENCES_SUB_Delay "Retraso: "
#define MSGTR_PREFERENCES_SUB_FPS "FPS:"
#define MSGTR_PREFERENCES_SUB_POS "Posici�n: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "Desactivar carga autom�tica de subt�tulos"
#define MSGTR_PREFERENCES_SUB_Unicode "Subt�tulo unicode"
#define MSGTR_PREFERENCES_SUB_MPSUB "Convertir el subt�tulo dado al formato de subt�tulos de MPlayer"
#define MSGTR_PREFERENCES_SUB_SRT "Convertir el subt�tulo dado al formato basado en tiempo SubViewer (SRT)"
#define MSGTR_PREFERENCES_SUB_Overlap "Superposici�n de subtitulos"
#define MSGTR_PREFERENCES_Font "Fuente:"
#define MSGTR_PREFERENCES_FontFactor "Factor de fuente:"
#define MSGTR_PREFERENCES_PostProcess "Activar postprocesado"
#define MSGTR_PREFERENCES_AutoQuality "Calidad autom�tica: "
#define MSGTR_PREFERENCES_NI "Usar non-interleaved AVI parser"
#define MSGTR_PREFERENCES_IDX "Reconstruir tabla de indices, si se necesita"
#define MSGTR_PREFERENCES_VideoCodecFamily "Familia de codec de v�deo:"
#define MSGTR_PREFERENCES_AudioCodecFamily "Familia de codec de audio:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "Nivel OSD"
#define MSGTR_PREFERENCES_FRAME_Subtitle "Subt�tulos"
#define MSGTR_PREFERENCES_FRAME_Font "Fuente"
#define MSGTR_PREFERENCES_FRAME_PostProcess "Postprocesado"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "Codec y demuxer"
#define MSGTR_PREFERENCES_FRAME_Cache "Cache"
#define MSGTR_PREFERENCES_FRAME_Misc "Misc"
#define MSGTR_PREFERENCES_Audio_Device "Dispositivo:"
#define MSGTR_PREFERENCES_Audio_Mixer "Mezclador:"
#define MSGTR_PREFERENCES_Audio_MixerChannel "Canal del Mezclador:"
#define MSGTR_PREFERENCES_Message "Algunas opciones requieren reiniciar la reproducci�n."
#define MSGTR_PREFERENCES_DXR3_VENC "Codificador de v�deo:"
#define MSGTR_PREFERENCES_DXR3_LAVC "Usar LAVC (FFmpeg)"
#define MSGTR_PREFERENCES_DXR3_FAME "Usar FAME"
#define MSGTR_PREFERENCES_FontEncoding1 "Unicode"
#define MSGTR_PREFERENCES_FontEncoding2 "Occidental (ISO-8859-1)"
#define MSGTR_PREFERENCES_FontEncoding3 "Occidental con euro (ISO-8859-15)"
#define MSGTR_PREFERENCES_FontEncoding4 "Eslavo/Centroeuropeo (ISO-8859-2)"
#define MSGTR_PREFERENCES_FontEncoding5 "Esperanto, Gallego, Malt�s, Turco (ISO-8859-3)"
#define MSGTR_PREFERENCES_FontEncoding6 "B�ltico (ISO-8859-4)"
#define MSGTR_PREFERENCES_FontEncoding7 "Cir�lico (ISO-8859-5)"
#define MSGTR_PREFERENCES_FontEncoding8 "�rabe (ISO-8859-6)"
#define MSGTR_PREFERENCES_FontEncoding9 "Griego moderno (ISO-8859-7)"
#define MSGTR_PREFERENCES_FontEncoding10 "Turco (ISO-8859-9)"
#define MSGTR_PREFERENCES_FontEncoding11 "B�ltico (ISO-8859-13)"
#define MSGTR_PREFERENCES_FontEncoding12 "C�ltico (ISO-8859-14)"
#define MSGTR_PREFERENCES_FontEncoding13 "Hebreo (ISO-8859-8)"
#define MSGTR_PREFERENCES_FontEncoding14 "Ruso (KOI8-R)"
#define MSGTR_PREFERENCES_FontEncoding15 "Belaruso (KOI8-U/RU)"
#define MSGTR_PREFERENCES_FontEncoding16 "Chino simplificado (CP936)"
#define MSGTR_PREFERENCES_FontEncoding17 "Chino tradicional (BIG5)"
#define MSGTR_PREFERENCES_FontEncoding18 "Japan�s(SHIFT-JIS)"
#define MSGTR_PREFERENCES_FontEncoding19 "Coreano (CP949)"
#define MSGTR_PREFERENCES_FontEncoding20 "Thai (CP874)"
#define MSGTR_PREFERENCES_FontEncoding21 "Cir�lico (Windows) (CP1251)"
#define MSGTR_PREFERENCES_FontEncoding22 "Eslavo/Centroeuropeo (Windows) (CP1250)"
#define MSGTR_PREFERENCES_FontNoAutoScale "Sin autoescalado"
#define MSGTR_PREFERENCES_FontPropWidth "Proporcional a la anchura de pel�cula"
#define MSGTR_PREFERENCES_FontPropHeight "Proporcional a la altura de pel�cula"
#define MSGTR_PREFERENCES_FontPropDiagonal "Proporcional al diagonal de pel�cula"
#define MSGTR_PREFERENCES_FontEncoding "Codificaci�n:"
#define MSGTR_PREFERENCES_FontBlur "Blur:"
#define MSGTR_PREFERENCES_FontOutLine "Outline:"
#define MSGTR_PREFERENCES_FontTextScale "Escalado de texto:"
#define MSGTR_PREFERENCES_FontOSDScale "Escalado de OSD:"
#define MSGTR_PREFERENCES_SubtitleOSD "Subt�tulos y OSD"
#define MSGTR_PREFERENCES_Cache "Cache si/no"
#define MSGTR_PREFERENCES_CacheSize "Tama�o de Cache: "
#define MSGTR_PREFERENCES_LoadFullscreen "Empezar en pantalla completa"
#define MSGTR_PREFERENCES_SaveWinPos "Guardar posici�n de la ventana"
#define MSGTR_PREFERENCES_XSCREENSAVER "Detener Salvador de Pantallas de X"
#define MSGTR_PREFERENCES_PlayBar "Habilitar barra de reproducci�n"
#define MSGTR_PREFERENCES_AutoSync "AutoSync si/no"
#define MSGTR_PREFERENCES_AutoSyncValue "Autosync: "
#define MSGTR_PREFERENCES_CDROMDevice "Dispositivo de CD-ROM:"
#define MSGTR_PREFERENCES_DVDDevice "Dispositivo de DVD:"
#define MSGTR_PREFERENCES_FPS "Cuadros por segundo de la Pelicula:"
#define MSGTR_PREFERENCES_ShowVideoWindow "Mostrar Ventana de Video cuando este inactiva"

#define MSGTR_ABOUT_UHU " Desarrollo de GUI patrocinado por UHU Linux\n"
#define MSGTR_ABOUT_CoreTeam "   Equipo principal de MPlayer:\n"
#define MSGTR_ABOUT_AdditionalCoders "   Otros programadores:\n"
#define MSGTR_ABOUT_MainTesters "   Testeadores m�s importantes:\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "Error fatal"
#define MSGTR_MSGBOX_LABEL_Error "Error"
#define MSGTR_MSGBOX_LABEL_Warning "Advertencia"

#endif

// ======================= VO Video Output drivers ========================

#define MSGTR_VOincompCodec "Disculpe, el dispositivo de salida de v�deo es incompatible con este codec.\n"
#define MSGTR_VO_GenericError "Este error ha ocurrido"
#define MSGTR_VO_UnableToAccess "No es posible acceder"
#define MSGTR_VO_ExistsButNoDirectory "ya existe, pero no es un directorio."
#define MSGTR_VO_DirExistsButNotWritable "El directorio ya existe, pero no se puede escribir en �l."
#define MSGTR_VO_DirExistsAndIsWritable "El directorio ya existe y se puede escribir en �l."
#define MSGTR_VO_CantCreateDirectory "No es posible crear el directorio de salida."
#define MSGTR_VO_DirectoryCreateSuccess "Directorio de salida creado exitosamente."

// vo_jpeg.c


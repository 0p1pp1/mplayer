// Translated by Fabio Pugliese Ornellas <fabio.ornellas@poli.usp.br>
// Portuguese from Brazil Translation
// GPLed code
// in sync version 1.87 from CVS 2002-02-04

// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char help_text[]=
"Uso:   mplayer [op��es] [url|caminho/]nome-do-arquivo\n"
"\n"
"Op��es b�sicas: (lista completa na p�gina do manual)\n"
" -vo <drv[:dev]> seleciona o driver de sa�da de v�deo & dispositivo\n"
"                 ('-vo help' para listar)\n"
" -ao <drv[:dev]> seleciona o driver de sa�da de audio & dispositivo\n"
"                 ('-vo help' para listar)\n"
#ifdef HAVE_VCD
" vcd://<numtrilha> reproduz trilha de VCD (Video CD) do dispositivo em vez de um\n"
"                 arquivo\n"
#endif
#ifdef USE_DVDREAD
" dvd://<numt�tilo> reproduz t�tulo de DVD do dispositivo em vez de um arquivo\n"
" -alang/-slang   seleciona o idioma/legenda do DVD (pelo c�digo pa�s de duas\n"
"                 letras)\n"
#endif
" -ss <tempopos>  busca para a posi��o dada (segundos ou hh:mm:ss)\n"
" -nosound        n�o reproduz som\n"
" -fs             reprodu��o em tela cheia (ou -vm, -zoom, detalhes na p�gina do\n"
"                 manual)\n"
" -x <x> -y <y>   especifica a resolu��o da tela (para uso com -vm ou -zoom)\n"
" -sub <arquivo>  especifica o arquivo de legenda a usar (veja tamb�m -subfps,\n"
"                 -subdelay)\n"
" -playlist <arquivo> especifica o aruqivo com a lista de reprodu��o\n"
" -vid x -aid y   seleciona a trilha de v�deo (x) e audio (y) a reproduzir\n"
" -fps x -srate y muda a taxa do v�deo (x quadros por segundo) e audio (y Hz)\n"
" -pp <qualidade> habilita filtro de p�s processamento (veja detalhes na p�gina\n"
"                 do manual)\n"
" -framedrop      habilita descarte de quadros (para m�quinas lentas)\n"
"\n"
"Teclas b�sicas: (lista completa na p�ginal do manual, cheque tamb�m input.conf)\n"
" <-  ou  ->      retorna/avan�a 10 segundos\n"
" cima ou baixo   retorna/avan�a 1 minuto\n"
" pgup ou pgdown  retorna/avan�a 10 minutos\n"
" < ou >          retorna/avan�a na lista de reprodu��o\n"
" p ou ESPA�O     pausa o filme (pressione qualquer tecla para continuar)\n"
" q ou ESC        para a reprodu��o e sai do programa\n"
" + ou -          ajusta o atraso do audio de +/- 0.1 segundo\n"
" o               alterna modo OSD: nenhum / busca / busca+cron�metro\n"
" * ou /          aumenta ou diminui o volume pcm\n"
" z ou x          ajusta o atraso da legenda de +/- 0.1 segundo\n"
" r ou t          posi��o da legenda para cima/baixo, veja tamb�m -vf expand\n"
"\n"
"* VEJA A P�GINA DO MANUAL PARA DETALHES, FUTURAS (AVAN�ADAS) OP��ES E TECLAS *\n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// mplayer.c:

#define MSGTR_Exiting "\nSaindo...\n"
#define MSGTR_ExitingHow "\nSaindo... (%s)\n"
#define MSGTR_Exit_quit "Sair"
#define MSGTR_Exit_eof "Fim do arquivo"
#define MSGTR_Exit_error "Erro fatal"
#define MSGTR_IntBySignal "\nMPlayer interrompido por sinal %d no m�dulo %s\n"
#define MSGTR_NoHomeDir "Diret�rio HOME n�o encontrado.\n"
#define MSGTR_GetpathProblem "Problema em get_path(\"config\")\n"
#define MSGTR_CreatingCfgFile "Criando arquivo de configura��o: %s\n"
#define MSGTR_InvalidAOdriver "Nome do driver de sa�da de audio inv�lido: %s\nUse '-ao help' para listar os drivers dispon�veis.\n"
#define MSGTR_CopyCodecsConf "(Copie/link etc/codecs.conf da fonte do MPlayer para ~/.mplayer/codecs.conf)\n"
#define MSGTR_BuiltinCodecsConf "Usando codecs.conf interno padr�o\n"
#define MSGTR_CantLoadFont "Imposs�vel carregar fonte: %s\n"
#define MSGTR_CantLoadSub "Imposs�vel carregar legendas: %s\n"
#define MSGTR_DumpSelectedStreamMissing "dump: FATAL: faltando trilha selecionada!\n"
#define MSGTR_CantOpenDumpfile "Imposs�vel abrir arquivo de dump.\n"
#define MSGTR_CoreDumped "core dumped ;)\n"
#define MSGTR_FPSnotspecified "Quadros por segundo n�o especificado no cabe�alho ou inv�lido, use a op��o -fps.\n"
#define MSGTR_TryForceAudioFmtStr "Tentando for�ar a fam�lia %s do driver do codec de audio...\n"
#define MSGTR_CantFindAudioCodec "Codec para o format de audio 0x%X n�o encontrado!\n"
#define MSGTR_TryForceVideoFmtStr "Tentando for�ar a fam�lia %s do driver do codec de v�deo...\n"
#define MSGTR_CantFindVideoCodec "Imposs�vel encontrar codec que se iguale ao -vo selecionado e ao formato de video 0x%X!\n"
#define MSGTR_VOincompCodec "O dispositivo de sa�da de v�deo selecionado � incompat�vel com este codec.\n"
#define MSGTR_CannotInitVO "FATAL: Imposs�vel inicializar o driver de v�deo.\n"
#define MSGTR_CannotInitAO "Imposs�vel abrir/inicializar o dispositivo de audio -> sem som\n"
#define MSGTR_StartPlaying "In�ciando reprodu��o...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"          ***************************************************\n"\
"          * Seu sistema � muito LENTO para reproduzir isto! *\n"\
"          ***************************************************\n\n"\
"Poss�veis raz�es, problemas, solu��es:\n"\
"- Mais comum: driver de _audio_ quebrado/falho\n"\
"  - Tente -ao sdl ou use ALSA 0.5 ou a emula��o OSS do ALSA 0.9.\n"\
"  - Experimente com diferente valores para -autosync, 30 � um bom come�o.\n"\
"- Sa�da de v�deo lenta\n"\
"  - Tente um driver diferente -vo (-vo help para listar) ou tente -framedrop!\n"\
"- CPU Lento\n"\
"  - N�o tente reproduzir um DVD/DivX grande em um CPU lento! Tente\n"\
"    -hardframedrop.\n"\
"- Arquivo corrompido\n"\
"  - Tente v�rias combina��es de -nobps -ni -forceidx -mc 0.\n"\
"- M�dia lenta (montagens NFS/SMB, DVD, VCD etc...)\n"\
"  - Tente -cache 8192.\n"\
"- Voc� est� usando -cache para reproduzir um arquivo AVI n�o-entrela�ado?\n"\
"  - Tente -nocache.\n"\
"Leia DOCS/HTML/en/devices.html para dicas de ajuste/velocidade.\n"\
"Se nenhum destes ajudar voc�, leia DOCS/HTML/en/bugreports.html.\n\n"

#define MSGTR_NoGui "MPlayer foi compilado SEM suporte a GUI (interface gr�fica com o usu�rio)!\n"
#define MSGTR_GuiNeedsX "MPlayer GUI (interface gr�fica com o usu�rio) requer X11!\n"
#define MSGTR_Playing "Reproduzindo %s\n"
#define MSGTR_NoSound "Audio: sem som.\n"
#define MSGTR_FPSforced "FPS (quadros por segundo) for�ado a ser %5.3f (ftime: %5.3f)\n"
#define MSGTR_CompiledWithRuntimeDetection "Compilado com detec��o de CPU em tempo real - AVISO - isto n�o � ideal! Para obter a melhor performance, recompile MPlayer com --disable-runtime-cpudetection\n"
#define MSGTR_CompiledWithCPUExtensions "Compilado para CPU x86 com exten��es:"
#define MSGTR_AvailableVideoOutputDrivers "Drivers de sa�da de v�deo dispon�veis:\n"
#define MSGTR_AvailableAudioOutputDrivers "Drivers de sa�da de audio dispon�veis:\n"
#define MSGTR_AvailableAudioCodecs "Codecs de audio dispon�veis:\n"
#define MSGTR_AvailableVideoCodecs "Codecs de v�deo dispon�veis:\n"
#define MSGTR_AvailableAudioFm "\nFam�lias/drivers de codec de audio dispon�veis (compilados):\n"
#define MSGTR_AvailableVideoFm "\nFam�lias/drivers de codec de v�deo dispon�veis (compilados):\n"
#define MSGTR_UsingRTCTiming "Usando regula��o de tempo Linux hardware RTC (%ldHz)\n"
#define MSGTR_CannotReadVideoProperties "Video: imposs�vel ler propriedades\n"
#define MSGTR_NoStreamFound "Trilha n�o encontrada\n"
#define MSGTR_ErrorInitializingVODevice "Erro abrindo/inicializando o dispositivo da sa�da de v�deo (-vo)!\n"
#define MSGTR_ForcedVideoCodec "Codec de v�deo for�ado: %s\n"
#define MSGTR_ForcedAudioCodec "Codec de audio for�ado: %s\n"
#define MSGTR_AODescription_AOAuthor "AO: Descri��o: %s\nAO: Autor: %s\n"
#define MSGTR_AOComment "AO: Coment�rio: %s\n"
#define MSGTR_Video_NoVideo "V�deo: sem v�deo\n"
#define MSGTR_NotInitializeVOPorVO "\nFATAL: Imposs�vel inicializar os filtros de v�deo (-vf) ou a sa�da de v�deo (-vo)!\n"
#define MSGTR_Paused "\n================= PAUSADO =================\r"
#define MSGTR_PlaylistLoadUnable "\nIncapaz de carregar a lista de reprodu��o %s.\n"
#define MSGTR_Exit_SIGILL_RTCpuSel \
"- MPlayer falhou por uma 'Instru��o Ilegal'.\n"\
"  Pode ser um erro no nosso novo c�digo de detec��o de CPU em tempo real...\n"\
"  Por favor leia DOCS/HTML/en/bugreports.html.\n"
#define MSGTR_Exit_SIGILL \
"- MPlayer falhou por uma 'Instru��o Ilegal'.\n"\
"  Isso frequentemente acontece quando voc� o exucuta em um CPU diferente do que\n"\
"  aquele para o qual foi compilado/otimizado.\n  Verifique isso!\n"
#define MSGTR_Exit_SIGSEGV_SIGFPE \
"- MPlayer falhou por mau uso do CPU/FPU/RAM.\n"\
"  Recompile o MPlayer com --enable-debug e fa�a um 'gdb backtrace' e\n"\
"  'disassembly'. Para detalhes, veja DOCS/HTML/en/bugreports_what.html#bugreports_crash\n"
#define MSGTR_Exit_SIGCRASH \
"- MPlayer falhou. Isso n�o deveria acontecer.\n"\
"  Pode ser um erro no c�digo do MPlayer _ou_ em seus drivers _ou_ em sua vers�o\n"\
"  do gcc. Se voc� acha que � culpa do MPlayer, por favor leia\n"\
"  DOCS/HTML/en/bugreports.html e siga as instru��es. N�s n�o podemos e n�o vamos ajudar\n"\
"  a n�o ser que voc� proveja esta informa��o quando reportar um poss�vel erro.\n"

// mencoder.c:

#define MSGTR_UsingPass3ControllFile "Usando controle de arquivo pass3: %s\n"
#define MSGTR_MissingFilename "\nFaltando nome do arquivo!\n\n"
#define MSGTR_CannotOpenFile_Device "Imposs�vel abrir arquivo/dispositivo\n"
#define MSGTR_CannotOpenDemuxer "Imposs�vel abrir \"demuxer\"\n"
#define MSGTR_NoAudioEncoderSelected "\nNenhum codificador de audio (-oac) selecionado! Selecione um ou use -nosound. Use -oac help para listar!\n"
#define MSGTR_NoVideoEncoderSelected "\nNenhum codificador de v�deo (-ovc) selecionado! Selecione um, use -ovc help para listar!\n"
#define MSGTR_CannotOpenOutputFile "Imposs�vel abrir arquivo de sa�da '%s'\n"
#define MSGTR_EncoderOpenFailed "Falha ao abrir o codificador\n"
#define MSGTR_ForcingOutputFourcc "For�ando sa�da fourcc para %x [%.4s]\n"
#define MSGTR_WritingAVIHeader "Gravando cabe�alho do AVI...\n"
#define MSGTR_DuplicateFrames "\n%d quadro(s) duplicado(s)!\n"
#define MSGTR_SkipFrame "\npulando frame!!!    \n"
#define MSGTR_ErrorWritingFile "%s: erro gravando arquivo.\n"
#define MSGTR_WritingAVIIndex "\nGravando �ndice do AVI...\n"
#define MSGTR_FixupAVIHeader "Corrigindo cabe�alho do AVI...\n"
#define MSGTR_RecommendedVideoBitrate "Bitrate do v�deo recomendado para CD de %s: %d\n"
#define MSGTR_VideoStreamResult "\nTrilha de v�deo: %8.3f kbit/s  (%d bps)  tamanho: %d bytes  %5.3f segundos  %d quadros\n"
#define MSGTR_AudioStreamResult "\nTrilha de audio: %8.3f kbit/s  (%d bps)  tamanho: %d bytes  %5.3f segundos\n"

// cfg-mencoder.h:

#define MSGTR_MEncoderMP3LameHelp "\n\n"\
" vbr=<0-4>     m�todo da taxa de bits vari�vel\n"\
"                0: cbr\n"\
"                1: mt\n"\
"                2: rh(padr�o)\n"\
"                3: abr\n"\
"                4: mtrh\n"\
"\n"\
" abr           taxa de bits m�dia\n"\
"\n"\
" cbr           taxa de bits constante\n"\
"               For�a tamb�m modo de codifica��o CBR nos modos ABR\n"\
"               pr�-selecionados subsequentes.\n"\
"\n"\
" br=<0-1024>   especifica a taxa de bits em kBit (somente CBR e ABR)\n"\
"\n"\
" q=<0-9>       qualidade (0-melhor, 9-pior) (somente para VBR)\n"\
"\n"\
" aq=<0-9>      qualidade do algor�tmo (0-melhor/mais lento, 9-pior/mais r�pido)\n"\
"\n"\
" ratio=<1-100> taxa de compress�o\n"\
"\n"\
" vol=<0-10>    configura ganho da entrada de audio\n"\
"\n"\
" mode=<0-3>    (padr�o: auto)\n"\
"                0: est�reo\n"\
"                1: est�reo-junto\n"\
"                2: canal duplo\n"\
"                3: mono\n"\
"\n"\
" padding=<0-2>\n"\
"                0: n�o\n"\
"                1: tudo\n"\
"                2: adaptar\n"\
"\n"\
" fast          aciona codifica��o r�pida nos modos VBR pr�-selecionados\n"\
"               subsequentes, qualidade muito baixa e altas taxas de bit.\n"\
"\n"\
" preset=<value> prov� os ajustes com a mais alta qualidade.\n"\
"                 medium: codifica��o VBR, qualidade boa\n"\
"                 (taxa de bits entre 150-180 kbps)\n"\
"                 standard:  codifica��o VBR, qualidade alta\n"\
"                 (taxa de bits entre 170-210 kbps)\n"\
"                 extreme: codifica��o VBR, qualidade muito alta\n"\
"                 (taxa de bits entre 200-240 kbps)\n"\
"                 insane:  codifica��o CBR, ajuste para a mais alta qualidade\n"\
"                 (taxa de bits fixa em 320 kbps)\n"\
"                 <8-320>: codifica��o ABR com a taxa de bits em kbps dada.\n\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "Dispositivo de CD-ROM '%s' n�o encontrado!\n"
#define MSGTR_ErrTrackSelect "Erro selecionando trilha do VCD!"
#define MSGTR_ReadSTDIN "Lendo de stdin...\n"
#define MSGTR_UnableOpenURL "Imposs�vel abrir URL: %s\n"
#define MSGTR_ConnToServer "Conecatado ao servidor: %s\n"
#define MSGTR_FileNotFound "Arquivo n�o encontrado: '%s'\n"

#define MSGTR_SMBInitError "Imposs�vel inicializar biblioteca libsmbclient: %d\n"
#define MSGTR_SMBFileNotFound "Imposs�vel abrir da \"lan\": '%s'\n"
#define MSGTR_SMBNotCompiled "MPlayer n�o foi compilado com suporte a leitura de SMB\n"

#define MSGTR_CantOpenDVD "Imposs�vel abrir dispositivo de DVD: %s\n"
#define MSGTR_DVDwait "Lendo estrutura do disco, por favor aguarde...\n"
#define MSGTR_DVDnumTitles "Existem %d t�tulos neste DVD.\n"
#define MSGTR_DVDinvalidTitle "N�mero do t�tulo do DVD inv�lido: %d\n"
#define MSGTR_DVDnumChapters "Existem %d cap�tulos neste t�tulo de DVD.\n"
#define MSGTR_DVDinvalidChapter "N�mero do cap�tulo do DVD inv�lido: %d\n"
#define MSGTR_DVDnumAngles "Existem %d anglos neste t�tulo de DVD.\n"
#define MSGTR_DVDinvalidAngle "N�mero do anglo do DVD inv�lido: %d\n"
#define MSGTR_DVDnoIFO "Imposs�vel abrir o arquivo IFO para o t�tulo de DVD %d.\n"
#define MSGTR_DVDnoVOBs "Imposs�vel abrir t�tulo VOBS (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD aberto com �xito!\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "AVISO! Cabe�alho %d da trilha de audio redefinido!\n"
#define MSGTR_VideoStreamRedefined "AVISO! Cabe�alho %d da trilha de v�deo redefinido!\n"
#define MSGTR_TooManyAudioInBuffer "\nMuitos pacotes de audio no buffer: (%d em %d bytes).\n"
#define MSGTR_TooManyVideoInBuffer "\nMuitos pacotes de audio no buffer: (%d em %d bytes).\n"
#define MSGTR_MaybeNI "Talvez voc� esteja reproduzindo um fluxo/arquivo n�o-entrela�ado ou o codec falhou?\n" \
		      "Para arquivos .AVI, tente for�ar um modo n�o-entrela�ado com a op��o -ni.\n"
#define MSGTR_SwitchToNi "\nDetectado .AVI mau entrela�ado - mudando para o modo -ni!\n"
#define MSGTR_Detected_XXX_FileFormat "Detectado formato de arquivo %s!\n"
#define MSGTR_DetectedAudiofile "Detectado arquivo de audio!\n"
#define MSGTR_NotSystemStream "Formato do fluxo n�o MPEG System... (pode ser um fluxo de transporte?)\n"
#define MSGTR_InvalidMPEGES "Fluxo MPEG-ES inv�lido??? Contacte o autor, pode ser um bug :(\n"
#define MSGTR_FormatNotRecognized "======= Desculpe, este formato de arquivo n�o � reconhecido/suportado ========\n"\
				  "== Se este arquivo � um fluxo AVI, ASF ou MPEG, por favor contacte o autor ==\n"
#define MSGTR_MissingVideoStream "Nenhuma trilha de v�deo encontrado!\n"
#define MSGTR_MissingAudioStream "Nenhuma trilha de audio encontrado -> sem som\n"
#define MSGTR_MissingVideoStreamBug "Trilha de v�deo faltando!? Contacte o autor, pode ser um bug :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: Arquivo n�o cont�m a trilha de audio ou v�deo selecionado.\n"

#define MSGTR_NI_Forced "For�ado"
#define MSGTR_NI_Detected "Detectado"
#define MSGTR_NI_Message "%s formato de arquivo AVI N�O ENTRELA�ADO!\n"

#define MSGTR_UsingNINI "Usando formato quebrado n�o-entrela�ado do arquivo AVI!\n"
#define MSGTR_CouldntDetFNo "Imposs�vel determinar o n�mero de quadros (para busca absoluta)  \n"
#define MSGTR_CantSeekRawAVI "Imposs�vel buscar em fluxos de .AVI brutos! (�ndice requerido, tente com a op��o -idx!)  \n"
#define MSGTR_CantSeekFile "Imposs�vel buscar neste arquivo!  \n"

#define MSGTR_MOVcomprhdr "MOV: Cabe�alhos comprimidos n�o suportados (ainda)!\n"
#define MSGTR_MOVvariableFourCC "MOV: Advert�ncia! Vari�vel FOURCC detectada!?\n"
#define MSGTR_MOVtooManyTrk "MOV: Advert�ncia! Trilhas demais!"
#define MSGTR_FoundAudioStream "==> Trilha de audio encontrada: %d\n"
#define MSGTR_FoundVideoStream "==> Trilha de video encontrada: %d\n"
#define MSGTR_DetectedTV "TV detectada! ;-)\n"
#define MSGTR_ErrorOpeningOGGDemuxer "Imposs�vel abrir o demuxer ogg\n"
#define MSGTR_ASFSearchingForAudioStream "ASF: Procurando por trilha de audio (id:%d)\n"
#define MSGTR_CannotOpenAudioStream "Imposs�vel abrir trilha de audio: %s\n"
#define MSGTR_CannotOpenSubtitlesStream "Imposs�vel abrir trilha de legendas: %s\n"
#define MSGTR_OpeningAudioDemuxerFailed "Falha ao abrir demuxer de audio: %s\n"
#define MSGTR_OpeningSubtitlesDemuxerFailed "Falha ao abrir demuxer de legendas: %s\n"
#define MSGTR_TVInputNotSeekable "Entrada de TV n�o aceita busca! (Provavelmente a busca ser� pra mudar de canal ;)\n"
#define MSGTR_DemuxerInfoAlreadyPresent "Informa��o %s do demuxer j� presente!\n"
#define MSGTR_ClipInfo "Informa��es do clip:\n"


// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "Imposs�vel abrir codec\n"
#define MSGTR_CantCloseCodec "Imposs�vel fechar codec\n"

#define MSGTR_MissingDLLcodec "ERRO: Imposs�vel abrir o codec DirectShow %s requerido.\n"
#define MSGTR_ACMiniterror "Imposs�vel carregar/inicializar o codec Win32/ACM AUDIO (arquivo DLL faltando?).\n"
#define MSGTR_MissingLAVCcodec "Imposs�vel encontrar codec '%s' em libavcodec...\n"

#define MSGTR_MpegNoSequHdr "MPEG: FATAL: EOF enquanto procurando pelo cabe�alho da sequ�ncia\n"
#define MSGTR_CannotReadMpegSequHdr "FATAL: Imposs�vel ler cabe�alho da sequ�ncia!\n"
#define MSGTR_CannotReadMpegSequHdrEx "FATAL: Imposs�vel ler exten��o do cabe�alho da sequ�ncia!\n"
#define MSGTR_BadMpegSequHdr "MPEG: Cabe�alho da sequ�ncia mau!\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: Extens�o do cabe�alho da sequ�ncia mau!\n"

#define MSGTR_ShMemAllocFail "Imposs�vel alocar mem�ria compartilhada\n"
#define MSGTR_CantAllocAudioBuf "Imposs�vel alocate buffer da sa�da de audio\n"

#define MSGTR_UnknownAudio "Desconhecido/faltando formato de audio -> sem som\n"

#define MSGTR_UsingExternalPP "[PP] Usando filtro de p�s processamento externo, m�ximo q = %d\n"
#define MSGTR_UsingCodecPP "[PP] Usando p�s processamento do codec, m�ximo q = = %d\n"
#define MSGTR_VideoAttributeNotSupportedByVO_VD "Atributo de v�deo '%s' n�o � suportado pelo vo & vd selecionado! \n"
#define MSGTR_VideoCodecFamilyNotAvailableStr "Fam�lia [%s] (vfm=%s) do codec de video n�o dispon�vel (habilite na hora da compila��o!)\n"
#define MSGTR_AudioCodecFamilyNotAvailableStr "Fam�lia [%s] (afm=%s) do codec de audio n�o dispon�vel (habilite na hora da compila��o!)\n"
#define MSGTR_OpeningVideoDecoder "Abrindo decodificador de v�deo: [%s] %s\n"
#define MSGTR_OpeningAudioDecoder "Abrindo decodificador de audio: [%s] %s\n"
#define MSGTR_UninitVideoStr "finalizando v�deo: %s\n"
#define MSGTR_UninitAudioStr "finalizando audio: %s\n"
#define MSGTR_VDecoderInitFailed "Falha na incializa��o do VDecoder :(\n"
#define MSGTR_ADecoderInitFailed "Falha na incializa��o do ADecoder :(\n"
#define MSGTR_ADecoderPreinitFailed "Falha na pr�-inicializa��o do ADecoder :(\n"
#define MSGTR_AllocatingBytesForInputBuffer "dec_audio: Aclocando %d bytes para o buffer de entrtada\n"
#define MSGTR_AllocatingBytesForOutputBuffer "dec_audio: Alocando %d + %d = %d bytes para o buffer de sa�da\n"

// LIRC:
#define MSGTR_SettingUpLIRC "Configurando o suporte a lirc...\n"
#define MSGTR_LIRCdisabled "N�o ser� poss�vel utilizar o seu controle remoto\n"
#define MSGTR_LIRCopenfailed "Falha na abertura do suporte a lirc!\n"
#define MSGTR_LIRCcfgerr "Falha ao ler o arquivo de configura��o do LIRC %s.\n"

// vf.c
#define MSGTR_CouldNotFindVideoFilter "Filtro de v�deo '%s' n�o encontrado\n"
#define MSGTR_CouldNotOpenVideoFilter "Imposs�vel abrir o filtro de v�deo '%s'\n"
#define MSGTR_OpeningVideoFilter "Abrindo filtro de v�deo: "
#define MSGTR_CannotFindColorspace "Imposs�vel encontrar um \"colorspace\" comum, mesmo inserindo \"scale\" :(\n"

// vd.c
#define MSGTR_CodecDidNotSet "VDec: codec n�o configurou sh->disp_w e sh->disp_h, tentando solu��o alternativa!\n"
#define MSGTR_VoConfigRequest "VDec: configura��o vo pedida - %d x %d (preferido csp: %s)\n"
#define MSGTR_CouldNotFindColorspace "Imposs�vel encotrar \"colorspace\" similar - retentando com -vf scale...\n"
#define MSGTR_MovieAspectIsSet "Aspecto do filme �  %.2f:1 - pr�-redimensionando para corrigir o aspecto do filme.\n"
#define MSGTR_MovieAspectUndefined "Aspecto do filme � indefinido - nenhum pr�-redimensionamento aplicado.\n"

// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "Sobre"
#define MSGTR_FileSelect "Selecionar arquivo..."
#define MSGTR_SubtitleSelect "Selecionar legenda..."
#define MSGTR_OtherSelect "Selecionar..."
#define MSGTR_AudioFileSelect "Selecionar canal de audio externo..."
#define MSGTR_FontSelect "Selecionar fonte..."
#define MSGTR_PlayList "Lista de reprodu��o"
#define MSGTR_Equalizer "Equalizador"
#define MSGTR_SkinBrowser "Skins"
#define MSGTR_Network "Rede..."
#define MSGTR_Preferences "Prefer�ncias"
#define MSGTR_NoMediaOpened "Nenhuma m�dia aberta."
#define MSGTR_VCDTrack "Trilha do VCD %d"
#define MSGTR_NoChapter "Nenhum cap�tulo"
#define MSGTR_Chapter "Cap�tulo %d"
#define MSGTR_NoFileLoaded "Nenhum arquivo carregado"

// --- buttons ---
#define MSGTR_Ok "OK"
#define MSGTR_Cancel "Cancelar"
#define MSGTR_Add "Adicionar"
#define MSGTR_Remove "Remover"
#define MSGTR_Clear "Limpar"
#define MSGTR_Config "Configura��es"
#define MSGTR_ConfigDriver "Configura��es do driver"
#define MSGTR_Browse "Procurar"

// --- error messages ---
#define MSGTR_NEMDB "Desculpe, sem mem�ria suficiente para desenhar o buffer."
#define MSGTR_NEMFMR "Desculpe, sem mem�ria suficiente para rendenizar o menu."
#define MSGTR_IDFGCVD "Desculpe, eu n�o encontrei um driver sa�da de v�deo compat�vel com GUI."
#define MSGTR_NEEDLAVCFAME "Desculpe, voc� n�o pode reproduzir arquivos n�o-MPEG com o seu dispositivo DXR3/H+ sem recodificar.\nPor favor habilite lavc ou fame na configura��o do DXR3/H+."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[skin] erro no arquivo de configura��o do skin na linha: %s"
#define MSGTR_SKIN_WARNING1 "[skin] aviso no arquivo de configura��o do skin na linha %d: widget encontrado mas antes de \"section\" n�o encontrado (%s)"
#define MSGTR_SKIN_WARNING2 "[skin] aviso no arquivo de configura��o do skin na linha %d: widget encontrado mas antes de \"subsection\" n�o encontrtado (%s)"
#define MSGTR_SKIN_WARNING3 "[skin] aviso no arquivo de configura��o do skin na linha %d: esta sub-se��o n�o � suportada por este widget (%s)"
#define MSGTR_SKIN_BITMAP_16bit  "16 bit ou menor profundidade de cores n�o suportado (%s).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "arquivo n�o encontrado (%s)\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "erro na leitura do BMP (%s)\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "erro na leitura do TGA (%s)\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "erro na leitura do PNG (%s)\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "TGA empacotado RLE n�o suportado (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "tipo de arquivo desconhecido (%s)\n"
#define MSGTR_SKIN_BITMAP_ConvertError "erro na convers�o 24 bit para 32 bit (%s)\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "mensagem desconhecida: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "mem�ria insuficiente\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "muitas fontes declaradas\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "arquivo da fonte n�o encontrado\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "arquivo de imagem da fonte n�o encontrado\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "indentificador de fonte n�o existente (%s)\n"
#define MSGTR_SKIN_UnknownParameter "par�metro desconhecido (%s)\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "Skin n�o encontrado (%s).\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "Erro na leitura do arquivo de configura��o do skin (%s).\n"
#define MSGTR_SKIN_LABEL "Skins:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "Sobre o MPlayer"
#define MSGTR_MENU_Open "Abrir..."
#define MSGTR_MENU_PlayFile "Reproduzir arquivo..."
#define MSGTR_MENU_PlayVCD "Reproduzir VCD..."
#define MSGTR_MENU_PlayDVD "Reproduzir DVD..."
#define MSGTR_MENU_PlayURL "Reproduzir URL..."
#define MSGTR_MENU_LoadSubtitle "Carregar legenda..."
#define MSGTR_MENU_DropSubtitle "Descartar legenda..."
#define MSGTR_MENU_LoadExternAudioFile "Carregar arquivo de audio externo..."
#define MSGTR_MENU_Playing "Reprodu��o"
#define MSGTR_MENU_Play "Reproduzir"
#define MSGTR_MENU_Pause "Pausar"
#define MSGTR_MENU_Stop "Parar"
#define MSGTR_MENU_NextStream "Pr�xima faixa"
#define MSGTR_MENU_PrevStream "Faixa anterior"
#define MSGTR_MENU_Size "Tamanho"
#define MSGTR_MENU_NormalSize "Tamanho normal"
#define MSGTR_MENU_DoubleSize "Tamanho dobrado"
#define MSGTR_MENU_FullScreen "Tela cheia"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "Abrir disco..."
#define MSGTR_MENU_ShowDVDMenu "Mostrar menu do DVD"
#define MSGTR_MENU_Titles "T�tulos"
#define MSGTR_MENU_Title "T�tulo %2d"
#define MSGTR_MENU_None "(nenhum)"
#define MSGTR_MENU_Chapters "Cap�tulos"
#define MSGTR_MENU_Chapter "Cap�tulo %2d"
#define MSGTR_MENU_AudioLanguages "Idiomas do audio"
#define MSGTR_MENU_SubtitleLanguages "Idiomas da legenda"
#define MSGTR_MENU_PlayList "Lista de reprodu��o"
#define MSGTR_MENU_SkinBrowser "Skins"
#define MSGTR_MENU_Preferences "Prefer�ncias"
#define MSGTR_MENU_Exit "Sair..."
#define MSGTR_MENU_Mute "Mudo"
#define MSGTR_MENU_Original "Original"
#define MSGTR_MENU_AspectRatio "Aspecto"
#define MSGTR_MENU_AudioTrack "Trilha de audio"
#define MSGTR_MENU_Track "Trilha %d"
#define MSGTR_MENU_VideoTrack "Trilha de v�deo"

// --- equalizer
#define MSGTR_EQU_Audio "Audio"
#define MSGTR_EQU_Video "V�deo"
#define MSGTR_EQU_Contrast "Contraste: "
#define MSGTR_EQU_Brightness "Brilho: "
#define MSGTR_EQU_Hue "Cor: "
#define MSGTR_EQU_Saturation "Satura��o: "
#define MSGTR_EQU_Front_Left "Frente Esquerda"
#define MSGTR_EQU_Front_Right "Frente Direita"
#define MSGTR_EQU_Back_Left "Fundo Esquerda"
#define MSGTR_EQU_Back_Right "Fundo Direita"
#define MSGTR_EQU_Center "Centro"
#define MSGTR_EQU_Bass "Grave"
#define MSGTR_EQU_All "Todos"
#define MSGTR_EQU_Channel1 "Canal 1:"
#define MSGTR_EQU_Channel2 "Canal 2:"
#define MSGTR_EQU_Channel3 "Canal 3:"
#define MSGTR_EQU_Channel4 "Canal 4:"
#define MSGTR_EQU_Channel5 "Canal 5:"
#define MSGTR_EQU_Channel6 "Canal 6:"

// --- playlist
#define MSGTR_PLAYLIST_Path "Caminho"
#define MSGTR_PLAYLIST_Selected "Arquivos selecionados"
#define MSGTR_PLAYLIST_Files "Arquivos"
#define MSGTR_PLAYLIST_DirectoryTree "�rvore de diret�rios"

// --- preferences
#define MSGTR_PREFERENCES_Audio "Audio"
#define MSGTR_PREFERENCES_Video "V�deo"
#define MSGTR_PREFERENCES_SubtitleOSD "Legenda & OSD"
#define MSGTR_PREFERENCES_Codecs "Codecs & demuxer"
#define MSGTR_PREFERENCES_Misc "Misc"

#define MSGTR_PREFERENCES_None "Nenhum"
#define MSGTR_PREFERENCES_AvailableDrivers "Drivers dispon�veis:"
#define MSGTR_PREFERENCES_DoNotPlaySound "N�o reproduzir som"
#define MSGTR_PREFERENCES_NormalizeSound "Normalizar som"
#define MSGTR_PREFERENCES_EnEqualizer "Habilitar equalizador"
#define MSGTR_PREFERENCES_ExtraStereo "Habilitar extra est�reo"
#define MSGTR_PREFERENCES_Coefficient "Coeficiente:"
#define MSGTR_PREFERENCES_AudioDelay "Atraso do audio"
#define MSGTR_PREFERENCES_DoubleBuffer "Habilitar duplo buffer"
#define MSGTR_PREFERENCES_DirectRender "Habilitar direct rendering"
#define MSGTR_PREFERENCES_FrameDrop "Habilitar descarte de quadros"
#define MSGTR_PREFERENCES_HFrameDrop "Habilitar descarte de quadros SEVERO (perigoso)"
#define MSGTR_PREFERENCES_Flip "Inverter imagem verticalmente"
#define MSGTR_PREFERENCES_Panscan "Panscan: "
#define MSGTR_PREFERENCES_OSDTimer "Temporizador e indicadores"
#define MSGTR_PREFERENCES_OSDProgress "Barras de progresso apenas"
#define MSGTR_PREFERENCES_OSDTimerPercentageTotalTime "Temporizador, porcentagem e tempo total"
#define MSGTR_PREFERENCES_Subtitle "Legenda:"
#define MSGTR_PREFERENCES_SUB_Delay "Atrtaso: "
#define MSGTR_PREFERENCES_SUB_FPS "FPS:"
#define MSGTR_PREFERENCES_SUB_POS "Posi��o: "
#define MSGTR_PREFERENCES_SUB_AutoLoad "Desabilitar auto carregamento de legendas"
#define MSGTR_PREFERENCES_SUB_Unicode "Legenda unicode"
#define MSGTR_PREFERENCES_SUB_MPSUB "Converter a legenda dada para o formato de legenda do MPlayer"
#define MSGTR_PREFERENCES_SUB_SRT "Converter a legenda dada para o formato baseado em tempo SubViewer (SRT)"
#define MSGTR_PREFERENCES_SUB_Overlap "Sobreposi��o da legenda"
#define MSGTR_PREFERENCES_Font "Fonte"
#define MSGTR_PREFERENCES_FontFactor "Fator da fonte:"
#define MSGTR_PREFERENCES_PostProcess "Habilitar p�s-processamento"
#define MSGTR_PREFERENCES_AutoQuality "Qualidade do audio: "
#define MSGTR_PREFERENCES_NI "Usar leitor de AVI n�o-entrela�ado"
#define MSGTR_PREFERENCES_IDX "Reconstruir tabela �ndice, se necess�rio"
#define MSGTR_PREFERENCES_VideoCodecFamily "Fam�lia do codec de v�deo:"
#define MSGTR_PREFERENCES_AudioCodecFamily "Fam�lia do codec de audio:"
#define MSGTR_PREFERENCES_FRAME_OSD_Level "N�vel do OSD"
#define MSGTR_PREFERENCES_FRAME_Subtitle "Legenda"
#define MSGTR_PREFERENCES_FRAME_Font "Fonte"
#define MSGTR_PREFERENCES_FRAME_PostProcess "P�s-processamento"
#define MSGTR_PREFERENCES_FRAME_CodecDemuxer "Codec & demuxer"
#define MSGTR_PREFERENCES_FRAME_Cache "Cache"
#define MSGTR_PREFERENCES_FRAME_Misc "Misc"
#define MSGTR_PREFERENCES_Message "Por favor lembre que voc� precisa reiniciar a reprodu��o para algumas op��es fazerem efeito!"
#define MSGTR_PREFERENCES_DXR3_VENC "Codificador de video:"
#define MSGTR_PREFERENCES_DXR3_LAVC "Usar LAVC (FFmpeg)"
#define MSGTR_PREFERENCES_DXR3_FAME "Usar FAME"
#define MSGTR_PREFERENCES_FontEncoding1 "Unicode"
#define MSGTR_PREFERENCES_FontEncoding2 "L�nguas Europ�ias Ocidentais (ISO-8859-1)"
#define MSGTR_PREFERENCES_FontEncoding3 "L�nguas Europ�ias Ocidentais com Euro (ISO-8859-15)"
#define MSGTR_PREFERENCES_FontEncoding4 "Linguas Europ�ias Esl�vicas/Centrais (ISO-8859-2)"
#define MSGTR_PREFERENCES_FontEncoding5 "Esperanto, Galego, Malt�s, Turco (ISO-8859-3)"
#define MSGTR_PREFERENCES_FontEncoding6 "Caracteres B�lticos Antigos (ISO-8859-4)"
#define MSGTR_PREFERENCES_FontEncoding7 "Cir�lico (ISO-8859-5)"
#define MSGTR_PREFERENCES_FontEncoding8 "�rabe (ISO-8859-6)"
#define MSGTR_PREFERENCES_FontEncoding9 "Grego Moderno (ISO-8859-7)"
#define MSGTR_PREFERENCES_FontEncoding10 "Turco (ISO-8859-9)"
#define MSGTR_PREFERENCES_FontEncoding11 "B�ltico (ISO-8859-13)"
#define MSGTR_PREFERENCES_FontEncoding12 "Celta (ISO-8859-14)"
#define MSGTR_PREFERENCES_FontEncoding13 "Caracteres Hebraicos (ISO-8859-8)"
#define MSGTR_PREFERENCES_FontEncoding14 "Russo (KOI8-R)"
#define MSGTR_PREFERENCES_FontEncoding15 "Ucraniano, Bielo-Russo (KOI8-U/RU)"
#define MSGTR_PREFERENCES_FontEncoding16 "Caracteres Chineses Simplificados (CP936)"
#define MSGTR_PREFERENCES_FontEncoding17 "Caracteres Chineses Tradicionais (BIG5)"
#define MSGTR_PREFERENCES_FontEncoding18 "Caracteres Japoneses (SHIFT-JIS)"
#define MSGTR_PREFERENCES_FontEncoding19 "Caracteres Coreanos (CP949)"
#define MSGTR_PREFERENCES_FontEncoding20 "Caracteres Tailandeses (CP874)"
#define MSGTR_PREFERENCES_FontEncoding21 "Windows Cir�lico (CP1251)"
#define MSGTR_PREFERENCES_FontEncoding22 "Europ�ias Esl�vicas/Centrais Windows (CP1250)"
#define MSGTR_PREFERENCES_FontNoAutoScale "Sem auto redimensionamento"
#define MSGTR_PREFERENCES_FontPropWidth "Proporcional a largura do filme"
#define MSGTR_PREFERENCES_FontPropHeight "Proporcional a altura do filme"
#define MSGTR_PREFERENCES_FontPropDiagonal "Proporcional a diagonal do filme"
#define MSGTR_PREFERENCES_FontEncoding "Codifica��o:"
#define MSGTR_PREFERENCES_FontBlur "Embassar:"
#define MSGTR_PREFERENCES_FontOutLine "Contorno:"
#define MSGTR_PREFERENCES_FontTextScale "Tamanho do texto:"
#define MSGTR_PREFERENCES_FontOSDScale "Tamanho do OSD:"
#define MSGTR_PREFERENCES_Cache "Cache ligado/desligado"
#define MSGTR_PREFERENCES_CacheSize "Tamaho do cache: "
#define MSGTR_PREFERENCES_LoadFullscreen "Iniciar em tela cheia"
#define MSGTR_PREFERENCES_XSCREENSAVER "Parar XScreenSaver"
#define MSGTR_PREFERENCES_PlayBar "Habilitar barra de reprodu��o"
#define MSGTR_PREFERENCES_AutoSync "AutoSync ligado/desligado"
#define MSGTR_PREFERENCES_AutoSyncValue "Autosync: "
#define MSGTR_PREFERENCES_CDROMDevice "Dispositivo de CD-ROM:"
#define MSGTR_PREFERENCES_DVDDevice "Dispositivo de DVD:"
#define MSGTR_PREFERENCES_FPS "Quadros por segundo do filme:"
#define MSGTR_PREFERENCES_ShowVideoWindow "Mostrar janela do v�deo quando inativo"

#define MSGTR_ABOUT_UHU "Desenvolvimento do GUI patrocinado por UHU Linux\n"
#define MSGTR_ABOUT_CoreTeam "   N�cleo do time do MPlayer:\n"
#define MSGTR_ABOUT_AdditionalCoders "   Programadores adicionais:\n"
#define MSGTR_ABOUT_MainTesters "   Testadores principais:\n"

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "Erro fatal!"
#define MSGTR_MSGBOX_LABEL_Error "Erro!"
#define MSGTR_MSGBOX_LABEL_Warning "Aten��o!"

#endif

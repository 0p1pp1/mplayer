// Translated by Fabio Pugliese Ornellas <fabio.ornellas@poli.usp.br>
// Portuguese from Brazil Translation
// GPLed code

// ========================= MPlayer help ===========================

#ifdef HELP_MP_DEFINE_STATIC
static char* banner_text=
"\n\n"
"MPlayer " VERSION "(C) 2000-2002 Arpad Gereoffy (veja DOCS!)\n"
"\n";

static char help_text[]=
#ifdef HAVE_NEW_GUI
"Uso:   mplayer [-gui] [op��es] [caminho/]nomedoarquivo\n"
#else
"Uso:   mplayer [op��es] [caminho/]nomedoarquivo\n"
#endif
"\n"
"Op��es:\n"
" -vo <drv[:dev]> seleciona driver de sa�da de video & dispositivo (veja '-vo help' para obter uma lista)\n"
" -ao <drv[:dev]> seleciona driver de sa�da de audio & dispositivo (veja '-ao help' para obter uma lista)\n"
" -vcd <trackno>  reproduz faixa de VCD (video cd) do dispositivo ao inv�s de arquivo regular\n"
#ifdef HAVE_LIBCSS
" -dvdauth <dev>  especifica dispositivo de DVD para autentica��o (para discos encriptados)\n"
#endif
#ifdef USE_DVDREAD
" -dvd <titleno>  reproduz t�tulo/faixa do dispositivo de DVD ao inves de arquivo regular\n"
#endif
" -ss <timepos>   busca uma determinada posi��o (segundos ou hh:mm:ss)\n"
" -nosound        n�o reproduz som\n"
#ifdef USE_FAKE_MONO
" -stereo <mode>  seleciona a sa�da est�reo MPEG1 (0:est�reo 1:esquerda 2:direita)\n"
#endif
" -channels <n>   n�mero de canais de sa�da de audio\n"
" -fs -vm -zoom   op��es de reprodu��o em tela cheia (tela cheia,muda modo de v�deo,redimensionamento por software)\n"
" -x <x> -y <y>   redimensiona a imagem para a resolu��o <x> * <y> [se o dispositivo -vo suporta!]\n"
" -sub <file>     especifica o arquivo de legenda a usar (veja tamb�m -subfps, -subdelay)\n"
" -playlist <file> especifica o aruqivo com a lista de reprodu��o\n"
" -vid x -aid y   op��es para selecionar o fluxo (stream) de v�deo (x) e audio (y) a reproduzir\n"
" -fps x -srate y op��es para mudar quadros por segundo (fps) do v�deo (x) e frequ�ncia (em Hz) do audio\n"
" -pp <quality>   habilita filtro de p�s-processamento (0-4 para DivX, 0-63 para mpegs)\n"
" -nobps          usa um m�todo alternativo de sincronia audio/v�deo para arquivos AVI (pode ajudar!)\n"
" -framedrop      habilita descarte de frames (para maquinas lentas)\n"
" -wid <window id> usa a janela existente para a sa�da de v�deo (�til com plugger!)\n"
"\n"
"Teclas:\n"
" <-  ou  ->      avan�a/retorna 10 segundos\n"
" cima ou baixo   avan�a/retorna 1 minuto\n"
" < ou >          avan�a/retorna na lista de reprodu��o\n"
" p ou ESPA�O     paraliza o filme (pressione qualqer tecla para continuar)\n"
" q ou ESC        para de reproduzir e sai do programa\n"
" + ou -          ajusta o atraso do audio de +/- 0.1 segundo\n"
" o               muda o modo OSD: nenhum / busca / busca+tempo\n"
" * ou /          incrementa ou decrementa o volume (pressione 'm' para selecionar entre master/pcm)\n"
" z ou x          ajusta o atraso da legenda de +/- 0.1 segundo\n"
"\n"
" * * * VEJA A PAGINA DO MANUAL PARA DETALHES, FUTURAS OP��ES (AVAN�ADAS) E TECLAS ! * * *\n"
"\n";
#endif

// ========================= MPlayer messages ===========================

// mplayer.c:

#define MSGTR_Exiting "\nSaindo... (%s)\n"
#define MSGTR_Exit_frames "N�mero de frames requisitados reprodizidos"
#define MSGTR_Exit_quit "Sair"
#define MSGTR_Exit_eof "Fim da linha"
#define MSGTR_Exit_error "Erro fatal"
#define MSGTR_IntBySignal "\nMPlayer interrompido com sinal %d no m�dulo: %s \n"
#define MSGTR_NoHomeDir "Diret�rio HOME n�o encontrado\n"
#define MSGTR_GetpathProblem "Problema em get_path(\"config\")\n"
#define MSGTR_CreatingCfgFile "Criando arquivo de configura��o: %s\n"
#define MSGTR_InvalidVOdriver "Dispositivo de sa�da de v�deo inv�lido: %s\nUse '-vo help' para obter uma lista dos dispositivos de v�deo dispon�veis.\n"
#define MSGTR_InvalidAOdriver "Dispositivo de sa�da de �udio inv�lido: %s\nUse '-ao help' para obter uma lista dos dispositivos de �udio dispon�veis.\n"
#define MSGTR_CopyCodecsConf "(copie/ln etc/codecs.conf (da �rvore fonte do MPlayer) para ~/.mplayer/codecs.conf)\n"
#define MSGTR_CantLoadFont "N�o pode-se carregar a fonte: %s\n"
#define MSGTR_CantLoadSub "N�o pode-se carregar a legenda: %s\n"
#define MSGTR_ErrorDVDkey "Erro processado a cahve do DVD.\n"
#define MSGTR_CmdlineDVDkey "Linha de comando requisitada do DVD est� armazenada para \"descrambling\".\n"
#define MSGTR_DVDauthOk "Sequ�ncia de autentica��o do DVD parece estar OK.\n"
#define MSGTR_DumpSelectedSteramMissing "dump: FATAL: fluxo (stream) selecionado faltando!\n"
#define MSGTR_CantOpenDumpfile "N�o pode-se abrir o arquivo dump!!!\n"
#define MSGTR_CoreDumped "core dumped :)\n"
#define MSGTR_FPSnotspecified "Quadros por segundo (FPS) n�o especificado (ou inv�lido) no cabe�alho! User a op��o -fps!\n"
#define MSGTR_NoVideoStream "Desculpe, sem fluxo (stream) de v�deo... ainda n�o � reproduz�vel\n"
#define MSGTR_TryForceAudioFmt "Tentando for�ar a fam�lia do codec do dispositivo de �udio %d ...\n"
#define MSGTR_CantFindAfmtFallback "Imposs�vel encontrar codec de �udio para a fam�lia de disposit�vo for�ada, voltando a outros disposit�vos.\n"
#define MSGTR_CantFindAudioCodec "Imposs�vel encontrar codec para o formato de �udio 0x%X !\n"
#define MSGTR_TryUpgradeCodecsConfOrRTFM "*** Tente atualizar $s de etc/codecs.conf\n*** Se ainda n�o estiver OK, ent�o leia DOCS/codecs.html!\n"
#define MSGTR_CouldntInitAudioCodec "Imposs�vel inicializar o codec de �udio! -> nosound\n"
#define MSGTR_TryForceVideoFmt "Tentando for�ar fam�lia do codec do dispositivo de v�deo %d ...\n"
#define MSGTR_CantFindVfmtFallback "Imposs�vel encontrar codec de v�deo para a fam�lia de dispositivo for�ada, voltando a outros dispositivos.\n"
#define MSGTR_CantFindVideoCodec "Imposs�vel encontrar codec que bata com o selecionado -vo e o formato de v�deo 0x%X !\n"
#define MSGTR_VOincompCodec "Desculpe, o dispositivo de sa�da de v�deo video_out � incompat�vel com este codec.\n"
#define MSGTR_EncodeFileExists "Arquivo j� exixte: %s (n�o sobreescreva sui AVI favorito!)\n"
#define MSGTR_CantCreateEncodeFile "Imposs�vel criar arquivo para codifica��o\n"
#define MSGTR_CannotInitVO "FATAL: Imposs�vel inicializar o dispositivo de v�deo!\n"
#define MSGTR_CannotInitAO "Imposs�vel abrir/inicializar o disposit�vo de �udio -> NOSOUND\n"
#define MSGTR_StartPlaying "In�cio da reprodu��o...\n"

#define MSGTR_SystemTooSlow "\n\n"\
"         ************************************************\n"\
"         * Seu sistema � muito LENTO para reproduzir isto!*\n"\
"         ************************************************\n"\
"!!! Poss�veis causas, problemas, solu��es:  \n"\
"- Mais comum: dispositivo de �udio quebrado/buggy. Solu��o: tente -ao sdl\n"\
"  ou use ALSA 0.5 ou emula��o do OSS para ALSA 0.9. Leia DOCS/sound.html\n"\
"  para mais dicas!\n"\
"- Sa�da de v�deo lenta. Tende um dispositivo diferente com -vo (para lista:\n"\
"  -vo help) ou tente com -framedrop ! Leia DOCS/video.html para mais dicas\n"\
"  de como aumentar a velocidade do v�deo.\n"\
"- CPU lento. N�o tente reproduzir grandes DVD/DivX em CPU lento! Tente\n"\
"  -hardframedrop\n"\
"- Arquivo corrompido. Tente v�rias combina�oes destes: -nobps  -ni  -mc 0\n"\
"  -forceidx. Se nenhum destes resolver, leia DOCS/bugreports.html !\n"\

#define MSGTR_NoGui "MPlayer foi compilado sem suporte a GUI (interface gr�fica)!\n"
#define MSGTR_GuiNeedsX "MPlayer GUI requer X11!\n"
#define MSGTR_Playing "Reproduzindo %s\n"
#define MSGTR_NoSound "�udio: nosound!!!\n"
#define MSGTR_FPSforced "FPS for�ado a ser %5.3f  (ftime: %5.3f)\n"

// open.c, stream.c:
#define MSGTR_CdDevNotfound "Dispositivo de CD-ROM '%s' n�o encontrado!\n"
#define MSGTR_ErrTrackSelect "Erro selecionando a faixa do VCD!"
#define MSGTR_ReadSTDIN "Lendo de stdin...\n"
#define MSGTR_UnableOpenURL "Imposs�vel abrir URL: %s\n"
#define MSGTR_ConnToServer "Conectado ao servidor: %s\n"
#define MSGTR_FileNotFound "Arquivo n�o encontrado: '%s'\n"

#define MSGTR_CantOpenDVD "Imposs�vel abrir o dispositivo de DVD: %s\n"
#define MSGTR_DVDwait "Lendo estrutura do disco, por favor espere...\n"
#define MSGTR_DVDnumTitles "Existem %d t�tulos neste DVD.\n"
#define MSGTR_DVDinvalidTitle "N�mero do t�tulo do DVD inv�lido: %d\n"
#define MSGTR_DVDnumChapters "Existem %d cap�tulos neste t�tulod de DVD.\n"
#define MSGTR_DVDinvalidChapter "N�mero do cap�tulo do DVD inv�lido: %d\n"
#define MSGTR_DVDnumAngles "Existem %d angulos neste t�tulod e DVD.\n"
#define MSGTR_DVDinvalidAngle "N�mero do angulo do DVD inv�lido: %d\n"
#define MSGTR_DVDnoIFO "Imposs�vel abrir arquivo IFO para o t�tulo %d do DVD.\n"
#define MSGTR_DVDnoVOBs "Imposs�vel abrir os t�tulos VOBS (VTS_%02d_1.VOB).\n"
#define MSGTR_DVDopenOk "DVD aberto com sucesso!\n"

// demuxer.c, demux_*.c:
#define MSGTR_AudioStreamRedefined "Aviso! Cabe�alho do fluxo (stream) de �udio %d redefinido!\n"
#define MSGTR_VideoStreamRedefined "Aviso! Cabe�alho do fluxo (strean) de v�deo %d redefinido!\n"
#define MSGTR_TooManyAudioInBuffer "\nDEMUXER: Muitos (%d em %d bytes) pacotes de �udio no buffer!\n"
#define MSGTR_TooManyVideoInBuffer "\nDEMUXER: Muitos (%d em %d bytes) pacotes de v�deo no buffer!\n"
#define MSGTR_MaybeNI "(pode ser que voc� reprodiziu um n�o-\"interleaved\" fluxo(stream)/arquivo ou o coded falhou)\n"
#define MSGTR_DetectedFILMfile "Detectado formato de arquivo FILM!\n"
#define MSGTR_DetectedFLIfile "Detectado formato de arquivo FLI!\n"
#define MSGTR_DetectedROQfile "Detectado formato de arquivo RoQ!\n"
#define MSGTR_DetectedREALfile "Detectado formato de arquivo REAL!\n"
#define MSGTR_DetectedAVIfile "Detectado formato de arquivo AVI!\n"
#define MSGTR_DetectedASFfile "Detectado formato de arquivo ASF!\n"
#define MSGTR_DetectedMPEGPESfile "Detectado formato de arquivo MPEG-PES!\n"
#define MSGTR_DetectedMPEGPSfile "Detectado formato de arquivo MPEG-PS!\n"
#define MSGTR_DetectedMPEGESfile "Detectado formato de arquivo MPEG-ES!\n"
#define MSGTR_DetectedQTMOVfile "Detectado formato de arquivo QuickTime/MOV!\n"
#define MSGTR_MissingMpegVideo "Fluxo (stream) de v�deo MPEG faltando!? Contate o autor, pode ser um bug :(\n"
#define MSGTR_InvalidMPEGES "Fluxo (stream) de v�deo MPEG-ES faltando!? Contate o autor, pode ser um bug :(\n"
#define MSGTR_FormatNotRecognized "============= Desculpe, este formato n�o � rconhecido/suportado ===============\n"\
				  "=== Se este arquivo � um AVI, ASF ou MPEG, por favor contate o autor! ===\n"
#define MSGTR_MissingVideoStream "Fluxo (stream) de v�deo n�o encontrado!\n"



#define MSGTR_MissingAudioStream "Fluxo (stream) de �udio n�o encontrado...  -> nosound\n"
#define MSGTR_MissingVideoStreamBug "Fluxo (stream) de v�deo faltando!? Contate o autor, pode ser um bug :(\n"

#define MSGTR_DoesntContainSelectedStream "demux: arquivo n�o cont�m o fluxo (stream) de �udio ou v�deo selecionado\n"

#define MSGTR_NI_Forced "For�ado"
#define MSGTR_NI_Detected "Detectado"
#define MSGTR_NI_Message "Formato do arquivo AVI %s N�O-\"INTERLEAVED\"!\n"

#define MSGTR_UsingNINI "Usando formato de arquivo AVI N�O-\"INTERLEAVED\" quebrado!\n"
#define MSGTR_CouldntDetFNo "Imposs�vel determinar o n�mero de frames (para busca absoluta)  \n"
#define MSGTR_CantSeekRawAVI "Imposs�vel buscar em fluxos (streams) de .AVI raw! (�ndice requerido, tente com a op��o -idx ativada!)  \n"
#define MSGTR_CantSeekFile "Imposs�vel buscar neste arquivo!  \n"

#define MSGTR_EncryptedVOB "Arquivo VOB encriptado (n�o compilado com suporte a libcss!) Leia o arquivo DOCS/cd-dvd.html\n"
#define MSGTR_EncryptedVOBauth "Fluxo (stream) encriptado mas a autentica��o nao foi requisitada por voc�!!\n"

#define MSGTR_MOVcomprhdr "MOV: Cabe�alhos comprimidos (ainda) n�o suportados!\n"
#define MSGTR_MOVvariableFourCC "MOV: Aviso! vari�vel FOURCC detectada!?\n"
#define MSGTR_MOVtooManyTrk "MOV: Aviso! muitas trilhas!"
#define MSGTR_MOVnotyetsupp "\n****** Formato Quicktime MOV ainda n�o suportado!!!!!!! *******\n"

// dec_video.c & dec_audio.c:
#define MSGTR_CantOpenCodec "imposs�vel abrir codec\n"
#define MSGTR_CantCloseCodec "imposs�vel fechar codec\n"

#define MSGTR_MissingDLLcodec "ERRO: Imposs�vel abrir o codec DirectShow requerido: %s\n"
#define MSGTR_ACMiniterror "Imposs�vel carregar/inicializar o codec Win32/ACM AUDIO (faltando o arquivo DLL?)\n"
#define MSGTR_MissingLAVCcodec "Imposs�vel encontrar codec '%s' em libavcodec...\n"

#define MSGTR_NoDShowSupport "MPlayer foi compilado SEM suporte a DirectShow!\n"
#define MSGTR_NoWfvSupport "Suporte aos codecs win32 deshabilitado, ou indispon�vel em  plataformas n�o-x86!\n"
#define MSGTR_NoDivx4Support "MPlayer foi compilado SEM suporte a DivX4Linux (libdivxdecore.so)!\n"
#define MSGTR_NoLAVCsupport "MPlayer foi compilado SEM suporte a ffmpeg/libavcodec!\n"
#define MSGTR_NoACMSupport "Codec de �udio Win32/ACM deshabilitado, ou indispon�vel em CPU n�o-x86 -> force nosound :(\n"
#define MSGTR_NoDShowAudio "Compilado sem suporte a DirectShow -> force nosound :(\n"
#define MSGTR_NoOggVorbis "Codec de �udio OggVorbis deshabilitado -> force nosound :(\n"
#define MSGTR_NoXAnimSupport "MPlayer foi compilado SEM suporte a XAnim!\n"

#define MSGTR_MpegPPhint "AVISO! Voc� requisitou um p�s-processamento de imagem para um\n" \
			 "         v�deo MPEG 1/2, mas compilou o MPlayer sem suporte a p�s-processametno\n" \
			 "         para MPEG 1/2!\n" \
			 "         #define MPEG12_POSTPROC em config.h, e recompile a libmpeg2!\n"
#define MSGTR_MpegNoSequHdr "MPEG: FATAL: EOF enquanto procurava pela sequ�ncia de cabe�alho\n"
#define MSGTR_CannotReadMpegSequHdr "FATAL: Imposs�vel ler a sequ�ncia do cabe�alho!\n"
#define MSGTR_CannotReadMpegSequHdrEx "FATAL: Imposs�vel ler a extens�o da sequ�ncia de cabe�alhon"
#define MSGTR_BadMpegSequHdr "MPEG: Sequ�ncia de cabe�alho ruim!\n"
#define MSGTR_BadMpegSequHdrEx "MPEG: Exten��o da sequ�ncia de cabe�alho ruim!\n"

#define MSGTR_ShMemAllocFail "Imposs�vel alocar mem�ria compartilahda\n"
#define MSGTR_CantAllocAudioBuf "Imposs�vel alocar a sa�da de �udio no buffer\n"
#define MSGTR_NoMemForDecodedImage "Sem mem�ria suficiente para alocar o buffer de imagem (%ld bytes)\n"

#define MSGTR_AC3notvalid "Fluxo (stream) AC3 inv�lido.\n"
#define MSGTR_AC3only48k "Somente fluxos (streams) de 48000 Hz s�o suportadas.\n"
#define MSGTR_UnknownAudio "Formato de �udio desconhecido/faltando, usando nosound\n"

// LIRC:
#define MSGTR_SettingUpLIRC "Configurando suporte a lirc...\n"
#define MSGTR_LIRCdisabled "Voc� n�o poder� usar seu controle remoto\n"
#define MSGTR_LIRCopenfailed "Falha abrindo o suporte a lirc\n"
#define MSGTR_LIRCsocketerr "Algo est� errado com o socket lirc: %s\n"
#define MSGTR_LIRCcfgerr "Falha ao ler o arquivo de configura��o do LIRC %s !\n"


// ====================== GUI messages/buttons ========================

#ifdef HAVE_NEW_GUI

// --- labels ---
#define MSGTR_About "Sobre"
#define MSGTR_FileSelect "Selecionar arquivo ..."
#define MSGTR_SubtitleSelect "Selecionar legenda ..."
#define MSGTR_OtherSelect "Selecionar ..."
#define MSGTR_MessageBox "Caixa de Mensagem"
#define MSGTR_PlayList "Lista de Reprocu��o"
#define MSGTR_SkinBrowser "Visualizador de texturas"

// --- buttons ---
#define MSGTR_Ok "Ok"
#define MSGTR_Cancel "Cancelar"
#define MSGTR_Add "Add"
#define MSGTR_Remove "Remover"

// --- error messages ---
#define MSGTR_NEMDB "Desculpe, sem mem�ria suficiente para desenhar o buffer."
#define MSGTR_NEMFMR "Desculpe, sem mem�ria suficiente para rendenizar o menu."
#define MSGTR_NEMFMM "Desculpe, sem mem�ria suficiente para a mascara da forma da janela principal."

// --- skin loader error messages
#define MSGTR_SKIN_ERRORMESSAGE "[skin] erro no arquivo de configura��o da textura na linha %d: %s"
#define MSGTR_SKIN_WARNING1 "[skin] aviso no arquivo de configura��o da textura na linha %d: widget encontrado mas antes de \"section\" n�o encontrado ( %s )"
#define MSGTR_SKIN_WARNING2 "[skin] aviso no arquivo de configura��o da textura na linha %d: widget encontrado mas antes de \"subsection\" n�o encontrado (%s)"
#define MSGTR_SKIN_BITMAP_16bit  "16 bits ou menos cores n�o suportado ( %s ).\n"
#define MSGTR_SKIN_BITMAP_FileNotFound  "arquivo n�o encontrado ( %s )\n"
#define MSGTR_SKIN_BITMAP_BMPReadError "erro na leitura do bmp ( %s )\n"
#define MSGTR_SKIN_BITMAP_TGAReadError "erro na leitura do tga ( %s )\n"
#define MSGTR_SKIN_BITMAP_PNGReadError "erro na leitura do png ( %s )\n"
#define MSGTR_SKIN_BITMAP_RLENotSupported "Pacote RLE no tga n�o suportado ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownFileType "tipo de arquivo desconhecido ( %s )\n"
#define MSGTR_SKIN_BITMAP_ConvertError "erro na convers�o de 24 bit para 32 bit ( %s )\n"
#define MSGTR_SKIN_BITMAP_UnknownMessage "menssagem desconhecida: %s\n"
#define MSGTR_SKIN_FONT_NotEnoughtMemory "sem memoria suficiente\n"
#define MSGTR_SKIN_FONT_TooManyFontsDeclared "fontes de mais declaradas\n"
#define MSGTR_SKIN_FONT_FontFileNotFound "arquivo da fonte n�o encontrado\n"
#define MSGTR_SKIN_FONT_FontImageNotFound "arquivo da imagem da fonte n�o encontrado\n"
#define MSGTR_SKIN_FONT_NonExistentFontID "identificador de fonte inexistente ( %s )\n"
#define MSGTR_SKIN_UnknownParameter "par�metro desconhecido ( %s )\n"
#define MSGTR_SKINBROWSER_NotEnoughMemory "[skinbrowser] sem mem�ra suficiente.\n"
#define MSGTR_SKIN_SKINCFG_SkinNotFound "Textura n�o encontrada ( %s ).\n"
#define MSGTR_SKIN_SKINCFG_SkinCfgReadError "Erro na leitura do arquivo de configura��o da textura ( %s ).\n"
#define MSGTR_SKIN_LABEL "Texturas:"

// --- gtk menus
#define MSGTR_MENU_AboutMPlayer "Sobre MPlayer"
#define MSGTR_MENU_Open "Abrir ..."
#define MSGTR_MENU_PlayFile "Reproduzir arquivo ..."
#define MSGTR_MENU_PlayVCD "Reproduzir VCD ..."
#define MSGTR_MENU_PlayDVD "Reproduzir DVD ..."
#define MSGTR_MENU_PlayURL "Reproduzir URL ..."
#define MSGTR_MENU_LoadSubtitle "Carregar legenda ..."
#define MSGTR_MENU_Playing "Reproduzindo"
#define MSGTR_MENU_Play "Reproduzir"
#define MSGTR_MENU_Pause "Paralizar"
#define MSGTR_MENU_Stop "Parar"
#define MSGTR_MENU_NextStream "Proximo arquivo"
#define MSGTR_MENU_PrevStream "Arquivo anterior"
#define MSGTR_MENU_Size "Tamanho"
#define MSGTR_MENU_NormalSize "Tamanho normal"
#define MSGTR_MENU_DoubleSize "Tamanho dobrado"
#define MSGTR_MENU_FullScreen "Tela cheia"
#define MSGTR_MENU_DVD "DVD"
#define MSGTR_MENU_VCD "VCD"
#define MSGTR_MENU_PlayDisc "Reproduzir disco ..."
#define MSGTR_MENU_ShowDVDMenu "Mostrar menu do DVD"
#define MSGTR_MENU_Titles "T�tulos"
#define MSGTR_MENU_Title "T�tulo %2d"
#define MSGTR_MENU_None "(vazio)"
#define MSGTR_MENU_Chapters "Cap�tulos"
#define MSGTR_MENU_Chapter "Cap�tulo %2d"
#define MSGTR_MENU_AudioLanguages "Idiomas do �udio"
#define MSGTR_MENU_SubtitleLanguages "Idioma das legendas"
#define MSGTR_MENU_PlayList "Lista de reprodu��o"
#define MSGTR_MENU_SkinBrowser "Visualizador de texturas"
#define MSGTR_MENU_Preferences "Prefer�ncias"
#define MSGTR_MENU_Exit "Sair ..."

// --- messagebox
#define MSGTR_MSGBOX_LABEL_FatalError "erro fatal ..."
#define MSGTR_MSGBOX_LABEL_Error "erro ..."
#define MSGTR_MSGBOX_LABEL_Warning "aviso ..."

#endif

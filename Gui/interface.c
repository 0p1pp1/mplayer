
#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>

#include "wm/ws.h"
#include "wm/wsxdnd.h"
#include "interface.h"
#include "skin/skin.h"

#include "mplayer/gtk/eq.h"
#include "mplayer/widgets.h"
#include "mplayer/mplayer.h"

#include "../mplayer.h"
#include "app.h"
#include "cfg.h"
#include "../help_mp.h"
#include "../subreader.h"
#include "../libvo/x11_common.h"
#include "../libvo/video_out.h"
#include "../libvo/font_load.h"
#include "../libvo/sub.h"
#include "../input/input.h"
#include "../libao2/audio_out.h"
#include "../mixer.h"
#include "../libao2/audio_plugin.h"
#include "../libao2/eq.h"

#ifdef USE_ICONV
#include <iconv.h>
#endif

#include "../libmpdemux/stream.h"
#include "../libmpdemux/demuxer.h"
#include "../libmpdemux/stheader.h"
#include "../libmpcodecs/dec_video.h"

guiInterface_t guiIntfStruct;

char * gstrcat( char ** dest,char * src )
{
 char * tmp = NULL;

 if ( !src ) return NULL;

 if ( *dest )
  {
   tmp=malloc( strlen( *dest ) + strlen( src ) + 1 );
   strcpy( tmp,*dest ); strcat( tmp,src ); free( *dest ); 
  }
  else
   { tmp=malloc( strlen( src ) + 1 ); strcpy( tmp,src ); }
 *dest=tmp;
 return tmp;
}

int gstrcmp( char * a,char * b )
{
 if ( !a && !b ) return 0;
 if ( !a || !b ) return -1;
 return strcmp( a,b );
}

int gstrncmp( char * a,char * b,int size )
{
 if ( !a && !b ) return 0;
 if ( !a || !b ) return -1;
 return strncmp( a,b,size );
}

char * gstrdup( char * str )
{
 if ( !str ) return NULL;
 return strdup( str );
}

char * gstrchr( char * str,int c )
{
 if ( !str ) return NULL;
 return strchr( str,c );
}

void gfree( void ** p )
{
 if ( *p == NULL ) return;
 free( *p ); *p=NULL;
}

void gset( char ** str,char * what )
{
 if ( *str ) { if ( !strstr( *str,what ) ) { gstrcat( str,"," ); gstrcat( str,what ); }}
   else gstrcat( str,what );
}

void gaddlist( char *** list,char * entry )
{
 int i;

 if ( (*list) )
  {
   for ( i=0;(*list)[i];i++ ) free( (*list)[i] );
   free( (*list) );
  }

 (*list)=malloc( 8 );
 (*list)[0]=gstrdup( entry );
 (*list)[1]=NULL;
}

#ifdef USE_ICONV
char * gconvert_uri_to_filename( char * str )
{
 iconv_t   d;
 char    * out = strdup( str );
 char	 * tmp = NULL;
 char    * ize;
 size_t    inb,outb;
 char    * charset = "ISO8859-1";
 char    * cs;

 if ( !strchr( str,'%' ) ) return str;
	     
 {
  char * t = calloc( 1,strlen( out ) );
  int    i,c = 0;
  for ( i=0;i < (int)strlen( out );i++ )
   if ( out[i] != '%' ) t[c++]=out[i];
    else
     {
      char tmp[4] = "0xXX"; 
//	  if ( out[++i] == '%' ) { t[c++]='%'; continue; };
      tmp[2]=out[++i]; tmp[3]=out[++i]; 
      t[c++]=(char)strtol( tmp,(char **)NULL,0 );
     }
  free( out );
  out=t;
 }

 if ( (cs=getenv( "CHARSET" )) && *cs ) charset=cs;

 inb=outb=strlen( out );
 tmp=calloc( 1,outb + 1 );
 ize=tmp;
 d=iconv_open( charset,"UTF-8" );
 if ( (iconv_t)(-1) == d ) return str;
 iconv( d,&out,&inb,&tmp,&outb ); 
 iconv_close( d ); 
 free( out );
 return ize;
}
#endif
							    
void guiInit( void )
{
 int i;

 memset( &guiIntfStruct,0,sizeof( guiIntfStruct ) );
 guiIntfStruct.Balance=50.0f;
 guiIntfStruct.StreamType=-1;

 memset( &gtkEquChannels,0,sizeof( gtkEquChannels ) );
 if ( !gtkAOOSSMixer ) gtkAOOSSMixer=strdup( PATH_DEV_MIXER );
 if ( !gtkAOOSSDevice ) gtkAOOSSDevice=strdup( PATH_DEV_DSP );
#ifdef HAVE_DXR3
 if ( !gtkDXR3Device ) gtkDXR3Device=strdup( "/dev/em8300-0" );
#endif
 fullscreen=gtkLoadFullscreen;       
   
 gtkInit();
// --- initialize X 
 wsXInit( (void *)mDisplay );
// --- load skin
 skinDirInHome=get_path("Skin");
 skinMPlayerDir=DATADIR "/Skin";
 printf("SKIN dir 1: '%s'\n",skinDirInHome);
 printf("SKIN dir 2: '%s'\n",skinMPlayerDir);
 if ( !skinName ) skinName=strdup( "default" );
 switch ( skinRead( skinName ) )
  {
   case -1: mp_msg( MSGT_GPLAYER,MSGL_ERR,MSGTR_SKIN_SKINCFG_SkinNotFound,skinName ); exit( 0 );
   case -2: mp_msg( MSGT_GPLAYER,MSGL_ERR,MSGTR_SKIN_SKINCFG_SkinCfgReadError,skinName ); exit( 0 );
  }
// --- initialize windows
 if ( ( mplDrawBuffer = (unsigned char *)calloc( 1,appMPlayer.main.Bitmap.ImageSize ) ) == NULL )
  {
   fprintf( stderr,MSGTR_NEMDB );
   exit( 0 );
  }

 wsCreateWindow( &appMPlayer.subWindow,
  appMPlayer.sub.x,appMPlayer.sub.y,appMPlayer.sub.width,appMPlayer.sub.height,
  wsNoBorder,wsShowMouseCursor|wsHandleMouseButton|wsHandleMouseMove,wsShowFrame|wsHideWindow,"ViDEO" );

 wsDestroyImage( &appMPlayer.subWindow );
 wsCreateImage( &appMPlayer.subWindow,appMPlayer.sub.Bitmap.Width,appMPlayer.sub.Bitmap.Height );
 wsXDNDMakeAwareness(&appMPlayer.subWindow);

 vo_setwindow( appMPlayer.subWindow.WindowID, appMPlayer.subWindow.wGC );

// i=wsHideFrame|wsMaxSize|wsHideWindow;
// if ( appMPlayer.mainDecoration ) i=wsShowFrame|wsMaxSize|wsHideWindow;
 i=wsShowFrame|wsMaxSize|wsHideWindow;
 wsCreateWindow( &appMPlayer.mainWindow,
  appMPlayer.main.x,appMPlayer.main.y,appMPlayer.main.width,appMPlayer.main.height,
  wsNoBorder,wsShowMouseCursor|wsHandleMouseButton|wsHandleMouseMove,i,"MPlayer" ); //wsMinSize|

 wsSetShape( &appMPlayer.mainWindow,appMPlayer.main.Mask.Image );
 wsXDNDMakeAwareness(&appMPlayer.mainWindow);

 mplMenuInit();

 #ifdef DEBUG
  mp_msg( MSGT_GPLAYER,MSGL_DBG2,"[main] Depth on screen: %d\n",wsDepthOnScreen );
  mp_msg( MSGT_GPLAYER,MSGL_DBG2,"[main] parent: 0x%x\n",(int)appMPlayer.mainWindow.WindowID );
  mp_msg( MSGT_GPLAYER,MSGL_DBG2,"[main] sub: 0x%x\n",(int)appMPlayer.subWindow.WindowID );
 #endif

 appMPlayer.mainWindow.ReDraw=(void *)mplMainDraw;
 appMPlayer.mainWindow.MouseHandler=mplMainMouseHandle;
 appMPlayer.mainWindow.KeyHandler=mplMainKeyHandle;
 appMPlayer.mainWindow.DandDHandler=mplDandDHandler;

 appMPlayer.subWindow.ReDraw=(void *)mplSubDraw;
 appMPlayer.subWindow.MouseHandler=mplSubMouseHandle;
 appMPlayer.subWindow.KeyHandler=mplMainKeyHandle;
 appMPlayer.subWindow.DandDHandler=mplDandDHandler;

 wsSetBackgroundRGB( &appMPlayer.subWindow,appMPlayer.subR,appMPlayer.subG,appMPlayer.subB );
 wsClearWindow( appMPlayer.subWindow );
 if ( appMPlayer.sub.Bitmap.Image ) wsConvert( &appMPlayer.subWindow,appMPlayer.sub.Bitmap.Image,appMPlayer.sub.Bitmap.ImageSize );

 btnModify( evSetVolume,guiIntfStruct.Volume );
 btnModify( evSetBalance,guiIntfStruct.Balance );
 btnModify( evSetMoviePosition,guiIntfStruct.Position );

 wsSetIcon( wsDisplay,appMPlayer.mainWindow.WindowID,guiIcon,guiIconMask );
 wsSetIcon( wsDisplay,appMPlayer.subWindow.WindowID,guiIcon,guiIconMask );
 
 guiIntfStruct.Playing=0;

 if ( !appMPlayer.mainDecoration ) wsWindowDecoration( &appMPlayer.mainWindow,0 );
 
 wsVisibleWindow( &appMPlayer.mainWindow,wsShowWindow );
#if 1
 wsVisibleWindow( &appMPlayer.subWindow,wsShowWindow );

 {
  XEvent xev;
  do { XNextEvent( wsDisplay,&xev ); } while ( xev.type != MapNotify || xev.xmap.event != appMPlayer.subWindow.WindowID );
  appMPlayer.subWindow.Mapped=wsMapped;
 }

 if ( fullscreen )
  {
   mplFullScreen();
   btnModify( evFullScreen,btnPressed );
  }
#endif
 mplSubRender=1;
// ---

 if ( plCurrent && !filename ) mplSetFileName( plCurrent->path,plCurrent->name,STREAMTYPE_FILE );
 if ( sub_name ) guiSetFilename( guiIntfStruct.Subtitlename,sub_name );
#if defined( USE_OSD ) || defined( USE_SUB )
 guiLoadFont();
#endif
}

void guiDone( void )
{
 mp_msg( MSGT_GPLAYER,MSGL_V,"[gui] done.\n" );
 cfg_write();
 wsXDone();
}

int guiCMDArray[] =
 {
  evLoad,
  evLoadSubtitle,
  evAbout,
  evPlay,
  evStop,
  evPlayList,
  evPreferences,
  evFullScreen,
  evSkinBrowser
 };

extern ao_functions_t * audio_out;
extern vo_functions_t * video_out;
extern int    		frame_dropping;
extern int              stream_dump_type;
extern char **          vo_plugin_args;

#if defined( USE_OSD ) || defined( USE_SUB )
void guiLoadFont( void )
{
#ifdef HAVE_FREETYPE
  load_font(vo_image_width, vo_image_height);
#else
 if ( vo_font )
  {
   int i;
   if ( vo_font->name ) free( vo_font->name );
   if ( vo_font->fpath ) free( vo_font->fpath );
   for ( i=0;i<16;i++ )
    if ( vo_font->pic_a[i] )
     {
      if ( vo_font->pic_a[i]->bmp ) free( vo_font->pic_a[i]->bmp );
      if ( vo_font->pic_a[i]->pal ) free( vo_font->pic_a[i]->pal );
     }
   for ( i=0;i<16;i++ )
    if ( vo_font->pic_b[i] )
     {
      if ( vo_font->pic_b[i]->bmp ) free( vo_font->pic_b[i]->bmp );
      if ( vo_font->pic_b[i]->pal ) free( vo_font->pic_b[i]->pal );
     }
   free( vo_font ); vo_font=NULL;
  }
 if ( font_name )
  {
   vo_font=read_font_desc( font_name,font_factor,0 );
   if ( !vo_font ) mp_msg( MSGT_CPLAYER,MSGL_ERR,MSGTR_CantLoadFont,font_name );
  } 
  else
   {
    font_name=gstrdup( get_path( "font/font.desc" ) );
    vo_font=read_font_desc( font_name,font_factor,0 );
    if ( !vo_font )
     {
      gfree( (void **)&font_name ); font_name=gstrdup( DATADIR"/font/font.desc" );
      vo_font=read_font_desc( font_name,font_factor,0 );
     }
   }
#endif
}
#endif

#ifdef USE_SUB
extern mp_osd_obj_t* vo_osd_list;

void guiLoadSubtitle( char * name )
{
 if ( guiIntfStruct.Playing == 0 )
  {
   guiIntfStruct.SubtitleChanged=1;
   return;
  }
 if ( subtitles )
  {
   mp_msg( MSGT_GPLAYER,MSGL_INFO,"[gui] Delete subtitles.\n" );
   sub_free( subtitles );
   subtitles=NULL;
   gfree( (void **)&sub_name );
   vo_sub=NULL;
   if ( vo_osd_list )
    {
     int len;
     mp_osd_obj_t * osd = vo_osd_list;
     while ( osd )
      {
       if ( osd->type == OSDTYPE_SUBTITLE ) break;
       osd=osd->next;
      }
     if ( osd && osd->flags&OSDFLAG_VISIBLE )
      {
       len=osd->stride * ( osd->bbox.y2 - osd->bbox.y1 );
       memset( osd->bitmap_buffer,0,len );
       memset( osd->alpha_buffer,0,len );
      }
    }
  }
 if ( name )
  {
   mp_msg( MSGT_GPLAYER,MSGL_INFO,"[gui] Delete Load subtitle: %s\n",name );
   sub_name=gstrdup( name );
   subtitles=sub_read_file( sub_name,guiIntfStruct.FPS );
  }
}
#endif

static void add_vop( char * str )
{
 mp_msg( MSGT_GPLAYER,MSGL_STATUS,"[gui] add video filter: %s\n",str );
 if ( vo_plugin_args )
  {
   int i = 0;
   while ( vo_plugin_args[i] ) if ( !gstrcmp( vo_plugin_args[i++],str ) ) { i=-1; break; }
   if ( i != -1 )
     { vo_plugin_args=realloc( vo_plugin_args,( i + 2 ) * sizeof( char * ) ); vo_plugin_args[i]=strdup( str ); vo_plugin_args[i+1]=NULL; }
  } else { vo_plugin_args=malloc( 2 * sizeof( char * ) ); vo_plugin_args[0]=strdup( str ); vo_plugin_args[1]=NULL; }
}

static void remove_vop( char * str )
{
 int n = 0;

 if ( !vo_plugin_args ) return;

 mp_msg( MSGT_GPLAYER,MSGL_STATUS,"[gui] remove video filter: %s\n",str );

 while ( vo_plugin_args[n++] ); n--;
 if ( n > -1 )
  {
   int i = 0,m = -1;
   while ( vo_plugin_args[i] ) if ( !gstrcmp( vo_plugin_args[i++],str ) ) { m=i - 1; break; }
   i--;
   if ( m > -1 )
    {
     if ( n == 1 ) { free( vo_plugin_args[0] ); free( vo_plugin_args ); vo_plugin_args=NULL; }
      else memcpy( &vo_plugin_args[i],&vo_plugin_args[i + 1],( n - i ) * sizeof( char * ) );
    }
  }
}

int guiGetEvent( int type,char * arg )
{
 stream_t * stream = (stream_t *) arg;
#ifdef USE_DVDREAD
 dvd_priv_t * dvdp = (dvd_priv_t *) arg;
#endif 

 switch ( type )
  {
   case guiXEvent:
        wsEvents( wsDisplay,(XEvent *)arg,NULL );
        gtkEventHandling();
        break;
   case guiCEvent:
        switch ( (int)arg )
	 {
          case guiSetPlay:  guiIntfStruct.Playing=1; mplState(); break;
          case guiSetStop:  guiIntfStruct.Playing=0; mplState(); break;
          case guiSetPause: guiIntfStruct.Playing=2; mplState(); break;
	 }
        break;
   case guiSetState:
	mplState();
        break;
   case guiSetFileName:
        if ( arg ) guiSetFilename( guiIntfStruct.Filename,arg );
        break;
   case guiSetAudioOnly:
	guiIntfStruct.AudioOnly=(int)arg;
	if ( (int)arg ) { guiIntfStruct.NoWindow=True; wsVisibleWindow( &appMPlayer.subWindow,wsHideWindow ); }
	  else wsVisibleWindow( &appMPlayer.subWindow,wsShowWindow );
	break;
   case guiReDrawSubWindow:
	wsPostRedisplay( &appMPlayer.subWindow );
	break;
   case guiSetShVideo:
	 {
	  if ( !appMPlayer.subWindow.isFullScreen )
	   {
	    wsResizeWindow( &appMPlayer.subWindow,vo_dwidth,vo_dheight );
            wsMoveWindow( &appMPlayer.subWindow,True,appMPlayer.sub.x,appMPlayer.sub.y );
	   }
	  guiIntfStruct.MovieWidth=vo_dwidth;
	  guiIntfStruct.MovieHeight=vo_dheight;
         }
	break;
#ifdef USE_DVDREAD
   case guiSetDVD:
        guiIntfStruct.DVD.titles=dvdp->vmg_file->tt_srpt->nr_of_srpts;
        guiIntfStruct.DVD.chapters=dvdp->vmg_file->tt_srpt->title[dvd_title].nr_of_ptts;
        guiIntfStruct.DVD.angles=dvdp->vmg_file->tt_srpt->title[dvd_title].nr_of_angles;
        guiIntfStruct.DVD.nr_of_audio_channels=dvdp->nr_of_channels;
        memcpy( guiIntfStruct.DVD.audio_streams,dvdp->audio_streams,sizeof( dvdp->audio_streams ) );
        guiIntfStruct.DVD.nr_of_subtitles=dvdp->nr_of_subtitles;
        memcpy( guiIntfStruct.DVD.subtitles,dvdp->subtitles,sizeof( dvdp->subtitles ) );
        guiIntfStruct.DVD.current_title=dvd_title + 1;
        guiIntfStruct.DVD.current_chapter=dvd_chapter + 1;
        guiIntfStruct.DVD.current_angle=dvd_angle + 1;
        guiIntfStruct.Track=dvd_title + 1;
        break;
#endif
   case guiSetStream:
	guiIntfStruct.StreamType=stream->type;
	switch( stream->type )
	 {
#ifdef USE_DVDREAD
	  case STREAMTYPE_DVD: 
	       guiGetEvent( guiSetDVD,(char *)stream->priv );
	       break;
#endif
#ifdef HAVE_VCD
	  case STREAMTYPE_VCD: 
	       {
	        int i;
		for ( i=1;i < 100;i++ )
		  if ( vcd_seek_to_track( stream->fd,i ) < 0 ) break;
		vcd_seek_to_track( stream->fd,vcd_track );
		guiIntfStruct.VCDTracks=--i;
		mp_msg( MSGT_GPLAYER,MSGL_INFO,"[gui] vcd tracks: %d\n",guiIntfStruct.VCDTracks );
		guiIntfStruct.Track=vcd_track;
	        break;
	       }
#endif
	  default: break;
	 }
	break;
   case guiIEvent:
        printf( "cmd: %d\n",(int)arg );
	switch( (int)arg )
	 {
          case MP_CMD_QUIT:
	       mplEventHandling( evExit,0 );
	       break;
	  case MP_CMD_VO_FULLSCREEN:
	       mplEventHandling( evFullScreen,0 );
	       break;
          default:
	       mplEventHandling( guiCMDArray[ (int)arg - MP_CMD_GUI_EVENTS - 1 ],0 );
	 }
	break;
   case guiReDraw:
	mplEventHandling( evRedraw,0 );
	break;
   case guiSetVolume:
        if ( audio_out )
	{
	 float l,r;
	 mixer_getvolume( &l,&r );
	 guiIntfStruct.Volume=(r>l?r:l);
	 if ( r != l ) guiIntfStruct.Balance=( ( r - l ) + 100 ) * 0.5f;
	   else guiIntfStruct.Balance=50.0f;
	 btnModify( evSetVolume,guiIntfStruct.Volume );
	 btnModify( evSetBalance,guiIntfStruct.Balance );
	}
	break;
   case guiSetFileFormat:
        guiIntfStruct.FileFormat=(int)arg;
	break;
   case guiSetValues:
// -- video
	guiIntfStruct.sh_video=arg;
	if ( arg )
	 {
	  sh_video_t * sh = (sh_video_t *)arg;
	  guiIntfStruct.FPS=sh->fps;
	 }

	if ( guiIntfStruct.NoWindow ) wsVisibleWindow( &appMPlayer.subWindow,wsHideWindow );
	
	if ( guiIntfStruct.StreamType == STREAMTYPE_STREAM ) btnSet( evSetMoviePosition,btnDisabled );
	 else btnSet( evSetMoviePosition,btnReleased );
	 
	if ( gtkCacheOn ) stream_cache_size=gtkCacheSize;
	 else stream_cache_size=-1;

// -- audio
        if ( audio_out )
	{
	 float l,r;
	 mixer_getvolume( &l,&r );
	 guiIntfStruct.Volume=(r>l?r:l);
	 if ( r != l ) guiIntfStruct.Balance=( ( r - l ) + 100 ) * 0.5f;
	   else guiIntfStruct.Balance=50.0f;
	 btnModify( evSetVolume,guiIntfStruct.Volume );
	 btnModify( evSetBalance,guiIntfStruct.Balance );
	}

	if ( gtkAONoSound ) { if ( !muted ) mixer_mute(); }
	 else if ( muted ) mixer_mute();

	if ( gtkEnableAudioEqualizer )
	 {
	  equalizer_t eq;
	  int i,j;
	  for ( i=0;i<6;i++ )
	    for ( j=0;j<10;j++ )
	     {
	      eq.channel=i; eq.band=j; eq.gain=gtkEquChannels[i][j];
	      gtkSet( gtkSetEqualizer,0,&eq );
	     }
	 }
// -- subtitle
#ifdef HAVE_DXR3
	if ( video_driver_list && !gstrcmp( video_driver_list[0],"dxr3" ) && guiIntfStruct.FileFormat != DEMUXER_TYPE_MPEG_PS
#ifdef USE_LIBAVCODEC
	 && !gtkVopLAVC
#endif
#ifdef USE_LIBFAME
	 && !gtkVopFAME 
#endif
	 )
	 {
	  gtkMessageBox( GTK_MB_FATAL,MSGTR_NEEDLAVCFAME );
	  guiIntfStruct.Playing=0;
	  return True;
	 }
#endif
	break;
   case guiSetDefaults:
	if ( filename && !guiIntfStruct.Filename )
	 {
	  gtkSet( gtkDelPl,0,NULL ); guiIntfStruct.StreamType=STREAMTYPE_FILE;
	  guiSetFilename( guiIntfStruct.Filename,filename );
	 }

       guiIntfStruct.DiskChanged=0;

// --- video opts
       
       if ( !video_driver_list )
	{
         int i = 0;
           while ( video_out_drivers[i++] )
	    if ( video_out_drivers[i - 1]->control( VOCTRL_GUISUPPORT,NULL ) == VO_TRUE ) 
	     {
	      gaddlist( &video_driver_list,(char *)video_out_drivers[i - 1]->info->short_name );
	      break;
	     }
	 }
	
	if ( !video_driver_list && !video_driver_list[0] ) { gtkMessageBox( GTK_MB_FATAL,MSGTR_IDFGCVD ); exit_player( "gui init" ); }

	{
	 int i = 0;
         guiIntfStruct.NoWindow=False;
         while ( video_out_drivers[i++] )
	  if ( video_out_drivers[i - 1]->control( VOCTRL_GUISUPPORT,NULL ) == VO_TRUE ) 
	   {
	    if  ( ( video_driver_list && !gstrcmp( video_driver_list[0],(char *)video_out_drivers[i - 1]->info->short_name ) )&&( video_out_drivers[i - 1]->control( VOCTRL_GUI_NOWINDOW,NULL ) == VO_TRUE ) ) 
	      { guiIntfStruct.NoWindow=True; break; }
	   }
	}

#ifdef HAVE_DXR3
#ifdef USE_LIBAVCODEC
	remove_vop( "lavc" );
#endif
#ifdef USE_LIBFAME
	remove_vop( "fame" );
#endif
	if ( video_driver_list && !gstrcmp( video_driver_list[0],"dxr3" ) )
	 {
	  if ( ( guiIntfStruct.StreamType != STREAMTYPE_DVD)&&( guiIntfStruct.StreamType != STREAMTYPE_VCD ) )
	   {
#ifdef USE_LIBAVCODEC
	    if ( gtkVopLAVC ) add_vop( "lavc" );
#endif
#ifdef USE_LIBFAME
	    if ( gtkVopFAME ) add_vop( "fame" );
#endif
	   }
	 }
#endif
// ---	 
	if ( gtkVopPP ) add_vop( "pp" );
	 else remove_vop( "pp" );
		 
// --- audio opts
//	if ( ao_plugin_cfg.plugin_list ) { free( ao_plugin_cfg.plugin_list ); ao_plugin_cfg.plugin_list=NULL; }
	if ( gtkAONorm ) 	       gset( &ao_plugin_cfg.plugin_list,"volnorm" );
	if ( gtkEnableAudioEqualizer ) gset( &ao_plugin_cfg.plugin_list,"eq" );
	if ( gtkAOExtraStereo )
	 {
	  gset( &ao_plugin_cfg.plugin_list,"extrastereo" );
	  ao_plugin_cfg.pl_extrastereo_mul=gtkAOExtraStereoMul;
	 }
	mixer_device=gtkAOOSSMixer;
	if ( audio_driver_list && !gstrncmp( audio_driver_list[0],"oss",3 ) && gtkAOOSSDevice )
	 {
	  char * tmp = calloc( 1,strlen( gtkAOOSSDevice ) + 5 );
	  sprintf( tmp,"oss:%s",gtkAOOSSDevice );
	  gaddlist( &audio_driver_list,tmp );
	 }

// -- subtitle
#ifdef USE_SUB
	sub_name=gstrdup( guiIntfStruct.Subtitlename );
	stream_dump_type=0;
	if ( gtkSubDumpMPSub ) stream_dump_type=4;
	if ( gtkSubDumpSrt ) stream_dump_type=6;
	gtkSubDumpMPSub=gtkSubDumpSrt=0;
#endif
#if defined( USE_OSD ) || defined( USE_SUB )
        guiLoadFont();
#endif

// --- misc		    
        if ( guiIntfStruct.AudioFile ) audio_stream=guiIntfStruct.AudioFile;
	  else if ( guiIntfStruct.FilenameChanged ) audio_stream=NULL;
	
	break;
  }
 return False;
}

void guiEventHandling( void )
{
 if ( !guiIntfStruct.Playing || guiIntfStruct.NoWindow ) wsHandleEvents();
 gtkEventHandling();
}

// --- 

float gtkEquChannels[6][10];

plItem * plCurrent = NULL;
plItem * plList = NULL;
plItem * plLastPlayed = NULL;

URLItem *URLList = NULL;

#if defined( MP_DEBUG ) && 0
void list( void )
{
 plItem * next = plList;
 printf( "--- list ---\n" );
 while( next || next->next )
  {
   printf( "item: %s/%s\n",next->path,next->name );
   if ( next->next ) next=next->next; else break;
  }
 printf( "--- end of list ---\n" );
}
#else
#define list();
#endif

void * gtkSet( int cmd,float fparam, void * vparam )
{
 equalizer_t * eq = (equalizer_t *)vparam;
 plItem      * item = (plItem *)vparam;
 
 URLItem     * url_item = (URLItem *)vparam;
 int           is_added = True;

 switch ( cmd )
  {
// --- handle playlist
   case gtkAddPlItem: // add item to playlist
	if ( plList )
	 {
	  plItem * next = plList;
	  while ( next->next ) { /*printf( "%s\n",next->name );*/ next=next->next; }
	  next->next=item; item->prev=next;
	 } else { item->prev=item->next=NULL; plCurrent=plList=item; }
        list();
        return NULL;
   case gtkGetNextPlItem: // get current item from playlist
	if ( plCurrent )
	 {
	  plCurrent=plCurrent->next;
	  if ( !plCurrent && plList ) 
	   {
	    plItem * next = plList;
	    while ( next->next ) { if ( !next->next ) break; next=next->next; }
	    plCurrent=next;
	   }
	  return plCurrent;
	 }
        return NULL;
   case gtkGetPrevPlItem:
	if ( plCurrent )
	 {
	  plCurrent=plCurrent->prev;
	  if ( !plCurrent && plList ) plCurrent=plList;
	  return plCurrent;
	 }
	return NULL;
   case gtkGetCurrPlItem: // get current item
        return plCurrent;
   case gtkDelPl: // delete list
        {
	 plItem * curr = plList;
	 plItem * next;
	 if ( !plList ) return NULL;
	 if ( !curr->next )
	  {
	   if ( curr->path ) free( curr->path );
	   if ( curr->name ) free( curr->name );
	   free( curr ); 
	  }
	  else
	   {
	    while ( curr->next )
	     {
	      next=curr->next;
	      if ( curr->path ) free( curr->path );
	      if ( curr->name ) free( curr->name );
	      free( curr ); 
	      curr=next;
	     }
	   }
	  plList=NULL; plCurrent=NULL;
	}
        return NULL;
   // ----- Handle url
   case gtkAddURLItem:
        if ( URLList )
	 {
          URLItem * next_url = URLList;
          is_added = False;
          while ( next_url->next )
           {
            if ( !gstrcmp( next_url->url,url_item->url ) )
             {
              is_added=True;
              break;
             }
            next_url=next_url->next;
           }
          if ( ( !is_added )&&( gstrcmp( next_url->url,url_item->url ) ) ) next_url->next=url_item;
         } else { url_item->next=NULL; URLList=url_item; }
        return NULL;
// --- subtitle
#if defined( USE_OSD ) || defined( USE_SUB )
#ifndef HAVE_FREETYPE
   case gtkSetFontFactor:
        font_factor=fparam;
	guiLoadFont();
	return NULL;
#else
   case gtkSetFontOutLine:
        subtitle_font_thickness=( 8.0f / 100.0f ) * fparam;
	guiLoadFont();
	return NULL;
   case gtkSetFontBlur:
	subtitle_font_radius=( 8.0f / 100.0f ) * fparam;
	guiLoadFont();
	return NULL;
   case gtkSetFontTextScale:
	text_font_scale_factor=fparam;
	guiLoadFont();
	return NULL;
   case gtkSetFontOSDScale:
	osd_font_scale_factor=fparam;
	guiLoadFont();
	return NULL;
   case gtkSetFontEncoding:
        if ( subtitle_font_encoding ) free( subtitle_font_encoding );
	subtitle_font_encoding=gstrdup( (char *)vparam );
	guiLoadFont();
	return NULL;
   case gtkSetFontAutoScale:
	subtitle_autoscale=(int)fparam;
	guiLoadFont();
	return NULL;
#endif
#endif
// --- misc
   case gtkClearStruct:
        if ( (unsigned int)vparam & guiFilenames )
	 {
	  gfree( (void **)&guiIntfStruct.Filename );
	  gfree( (void **)&guiIntfStruct.Subtitlename );
	  gfree( (void **)&guiIntfStruct.AudioFile );
	  gtkSet( gtkDelPl,0,NULL );
	 }
#ifdef USE_DVDREAD
	if ( (unsigned int)vparam & guiDVD ) memset( &guiIntfStruct.DVD,0,sizeof( guiDVDStruct ) );
#endif
#ifdef HAVE_VCD
	if ( (unsigned int)vparam & guiVCD ) guiIntfStruct.VCDTracks=0;
#endif
	return NULL;
   case gtkSetExtraStereo:
        gtkAOExtraStereoMul=fparam;
	audio_plugin_extrastereo.control( AOCONTROL_PLUGIN_ES_SET,(int)&gtkAOExtraStereoMul );
        return NULL;
   case gtkSetPanscan:
        {
	 mp_cmd_t * mp_cmd;
         mp_cmd=(mp_cmd_t *)calloc( 1,sizeof( *mp_cmd ) );
         mp_cmd->id=MP_CMD_PANSCAN;    mp_cmd->name=strdup( "panscan" );
	 mp_cmd->args[0].v.f=fparam;   mp_cmd->args[1].v.i=1;
	 mp_input_queue_cmd( mp_cmd );
	}
        return NULL;
   case gtkSetAutoq:
	auto_quality=(int)fparam;
	return NULL;
// --- set equalizers
   case gtkSetContrast:
        if ( guiIntfStruct.sh_video ) set_video_colors( guiIntfStruct.sh_video,"contrast",(int)fparam );
	return NULL;
   case gtkSetBrightness:
        if ( guiIntfStruct.sh_video ) set_video_colors( guiIntfStruct.sh_video,"brightness",(int)fparam );
	return NULL;
   case gtkSetHue:
        if ( guiIntfStruct.sh_video ) set_video_colors( guiIntfStruct.sh_video,"hue",(int)fparam );
	return NULL;
   case gtkSetSaturation:
        if ( guiIntfStruct.sh_video ) set_video_colors( guiIntfStruct.sh_video,"saturation",(int)fparam );
	return NULL;
   case gtkSetEqualizer:
        if ( eq )
	 {
          gtkEquChannels[eq->channel][eq->band]=eq->gain;
	  audio_plugin_eq.control( AOCONTROL_PLUGIN_EQ_SET_GAIN,(int)eq );
	 }
	 else
	  {
	   int i,j; equalizer_t tmp; tmp.gain=0.0f;
	   memset( gtkEquChannels,0,sizeof( gtkEquChannels ) );
	   for ( i=0;i<6;i++ )
	    for ( j=0;j<10;j++ )
	     { tmp.channel=i; tmp.band=j; audio_plugin_eq.control( AOCONTROL_PLUGIN_EQ_SET_GAIN,(int)&tmp ); }
	  }
	return NULL;
  }
 return NULL;
}

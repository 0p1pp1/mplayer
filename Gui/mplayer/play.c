
#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <unistd.h>
#include <signal.h>

#include "../wm/ws.h"
#include "../../config.h"
#include "../../help_mp.h"
#include "../../mplayer.h"
#include "../../libvo/x11_common.h"
#include "../../input/input.h"

#include "../app.h"

#include "../wm/wskeys.h"
#include "../interface.h"

#include "widgets.h"
#include "./mplayer.h"
#include "play.h"

#include "../skin/skin.h"
#include "../skin/font.h"

#include "../libmpdemux/stream.h"

extern float rel_seek_secs;
extern int abs_seek_pos;

static int mplGotoTheNext = 1;

void mplFullScreen( void )
{
 if ( guiIntfStruct.NoWindow && guiIntfStruct.Playing ) return;
#if 0
 static int sx,sy;
// if ( !guiIntfStruct.Playing )
  {
   wsVisibleWindow( &appMPlayer.subWindow,wsHideWindow );
   if ( appMPlayer.subWindow.isFullScreen )
    {
     wsResizeWindow( &appMPlayer.subWindow,sx,sy );
     wsMoveWindow( &appMPlayer.subWindow,True,appMPlayer.sub.x,appMPlayer.sub.y );
     wsWindowDecoration( &appMPlayer.subWindow,appMPlayer.subWindow.Decorations );
     appMPlayer.subWindow.isFullScreen=0;
    }
    else
     {
      sx=appMPlayer.subWindow.Width; sy=appMPlayer.subWindow.Height;
      wsResizeWindow( &appMPlayer.subWindow,wsMaxX,wsMaxY );
      wsMoveWindow( &appMPlayer.subWindow,True,0,0 );
      wsWindowDecoration( &appMPlayer.subWindow,0 );
      appMPlayer.subWindow.isFullScreen=1;
     }
   vo_fs=appMPlayer.subWindow.isFullScreen;     
   wsVisibleWindow( &appMPlayer.subWindow,wsShowWindow );
  }// else { vo_x11_fullscreen(); appMPlayer.subWindow.isFullScreen=vo_fs; }
#else
  if ( ( guiIntfStruct.Playing )&&( appMPlayer.subWindow.isFullScreen ) )
   { 
    appMPlayer.subWindow.OldWidth=guiIntfStruct.MovieWidth; appMPlayer.subWindow.OldHeight=guiIntfStruct.MovieHeight; 
    switch ( appMPlayer.sub.x )
     {
      case -1: appMPlayer.subWindow.OldX=( wsMaxX / 2 ) - ( appMPlayer.subWindow.OldWidth / 2 ); break;
      case -2: appMPlayer.subWindow.OldX=wsMaxX - appMPlayer.subWindow.OldWidth; break;
      default: appMPlayer.subWindow.OldX=appMPlayer.sub.x; break;
     }
    switch ( appMPlayer.sub.y )
     {
      case -1: appMPlayer.subWindow.OldY=( wsMaxY / 2 ) - ( appMPlayer.subWindow.OldHeight / 2 ); break;
      case -2: appMPlayer.subWindow.OldY=wsMaxY - appMPlayer.subWindow.OldHeight; break;
      default: appMPlayer.subWindow.OldY=appMPlayer.sub.y; break;
     }
   }
  wsFullScreen( &appMPlayer.subWindow );
  vo_fs=appMPlayer.subWindow.isFullScreen;
  wsSetLayer( wsDisplay,appMPlayer.mainWindow.WindowID,appMPlayer.subWindow.isFullScreen );
  wsSetLayer( wsDisplay,appMPlayer.menuWindow.WindowID,appMPlayer.subWindow.isFullScreen );
#endif

 fullscreen=appMPlayer.subWindow.isFullScreen;
 if ( guiIntfStruct.Playing ) wsSetBackgroundRGB( &appMPlayer.subWindow,0,0,0 );
  else wsSetBackgroundRGB( &appMPlayer.subWindow,appMPlayer.subR,appMPlayer.subG,appMPlayer.subB );
}

extern int mplSubRender;

void mplEnd( void )
{
 plItem * next;

 if ( !mplGotoTheNext ) { mplGotoTheNext=1; return; }

 if ( guiIntfStruct.Playing && (next=gtkSet( gtkGetNextPlItem,0,NULL )) && plLastPlayed != next )
  {
   plLastPlayed=next;
   guiSetDF( guiIntfStruct.Filename,next->path,next->name );
   guiIntfStruct.StreamType=STREAMTYPE_FILE;
   guiIntfStruct.FilenameChanged=guiIntfStruct.NewPlay=1;
   gfree( (void **)&guiIntfStruct.AudioFile );
   gfree( (void **)&guiIntfStruct.Subtitlename );
  } 
  else
    {
     if ( guiIntfStruct.FilenameChanged || guiIntfStruct.NewPlay ) return;

     guiIntfStruct.TimeSec=0;
     guiIntfStruct.Position=0;
     guiIntfStruct.AudioType=0;
     guiIntfStruct.NoWindow=False;

#ifdef USE_DVDREAD
     guiIntfStruct.DVD.current_title=1;
     guiIntfStruct.DVD.current_chapter=1;
     guiIntfStruct.DVD.current_angle=1;
#endif

     if ( !appMPlayer.subWindow.isFullScreen )
      {
       wsResizeWindow( &appMPlayer.subWindow,appMPlayer.sub.width,appMPlayer.sub.height );
       wsMoveWindow( &appMPlayer.subWindow,True,appMPlayer.sub.x,appMPlayer.sub.y );
      }
     guiGetEvent( guiCEvent,guiSetStop );
     mplSubRender=1;
     wsSetBackgroundRGB( &appMPlayer.subWindow,appMPlayer.subR,appMPlayer.subG,appMPlayer.subB );
     wsClearWindow( appMPlayer.subWindow );
     wsPostRedisplay( &appMPlayer.subWindow );
    }
}

void mplPlay( void )
{
 if ( ( !guiIntfStruct.Filename )||
      ( guiIntfStruct.Filename[0] == 0 )||
      ( guiIntfStruct.Playing == 1 ) ) return;
 if ( guiIntfStruct.Playing == 2 ) { mplPause(); return; }
 guiGetEvent( guiCEvent,(void *)guiSetPlay );
 mplSubRender=0;
 wsSetBackgroundRGB( &appMPlayer.subWindow,0,0,0 );
 wsClearWindow( appMPlayer.subWindow );
}

void mplPause( void )
{
 if ( !guiIntfStruct.Playing ) return;
 if ( guiIntfStruct.Playing == 1 )
  {
   mp_cmd_t * cmd = (mp_cmd_t *)calloc( 1,sizeof( *cmd ) );
   cmd->id=MP_CMD_PAUSE;
   cmd->name=strdup("pause");
   mp_input_queue_cmd(cmd);
  } else guiIntfStruct.Playing=1;
}

void mplState( void )
{
 if ( ( guiIntfStruct.Playing == 0 )||( guiIntfStruct.Playing == 2 ) )
  {
   btnModify( evPlaySwitchToPause,btnReleased );
   btnModify( evPauseSwitchToPlay,btnDisabled );
  }
  else
   {
    btnModify( evPlaySwitchToPause,btnDisabled );
    btnModify( evPauseSwitchToPlay,btnReleased );
   }
}

float mplGetPosition( void )
{ // return 0.0 ... 100.0
 return guiIntfStruct.Position;
}

void mplRelSeek( float s )
{ // -+s
 rel_seek_secs=s; abs_seek_pos=0;
}

void mplAbsSeek( float s )
{ // 0.0 ... 100.0
 if ( guiIntfStruct.StreamType == STREAMTYPE_STREAM ) return;
 rel_seek_secs=0.01*s; abs_seek_pos=3;
}

listItems tmpList;

void ChangeSkin( char * name )
{
 int ret;
 int prev = appMPlayer.menuIsPresent;

 mainVisible=0;

 appInitStruct( &tmpList );
 skinAppMPlayer=&tmpList;
 fntFreeFont();
 ret=skinRead( name );

 appInitStruct( &tmpList );
 skinAppMPlayer=&appMPlayer;
 appInitStruct( &appMPlayer );
 if ( ret ) name=skinName;
 if ( skinRead( name ) )
  {
   mainVisible=1;
   return;
  }

 if ( prev && appMPlayer.menuIsPresent )
  {
   if ( mplMenuDrawBuffer ) free( mplMenuDrawBuffer );
   if ( ( mplMenuDrawBuffer = (unsigned char *)calloc( 1,appMPlayer.menuBase.Bitmap.ImageSize ) ) == NULL )
    { mp_msg( MSGT_GPLAYER,MSGL_STATUS,MSGTR_NEMDB ); return; }
   wsResizeWindow( &appMPlayer.menuWindow,appMPlayer.menuBase.width,appMPlayer.menuBase.height );
   wsResizeImage( &appMPlayer.menuWindow,appMPlayer.menuBase.width,appMPlayer.menuBase.height );
   wsSetShape( &appMPlayer.menuWindow,appMPlayer.menuBase.Mask.Image );
   wsVisibleWindow( &appMPlayer.menuWindow,wsHideWindow );
  } else { mplMenuInit(); }

 if ( appMPlayer.sub.Bitmap.Image ) wsResizeImage( &appMPlayer.subWindow,appMPlayer.sub.Bitmap.Width,appMPlayer.sub.Bitmap.Height );
 if ( ( !appMPlayer.subWindow.isFullScreen )&&( !guiIntfStruct.Playing ) )
  {
   wsResizeWindow( &appMPlayer.subWindow,appMPlayer.sub.width,appMPlayer.sub.height );
   wsMoveWindow( &appMPlayer.subWindow,True,appMPlayer.sub.x,appMPlayer.sub.y );
  }
 if ( appMPlayer.sub.Bitmap.Image ) wsConvert( &appMPlayer.subWindow,appMPlayer.sub.Bitmap.Image,appMPlayer.sub.Bitmap.ImageSize );
 if ( !guiIntfStruct.Playing )
  {
   mplSubRender=1;
   wsSetBackgroundRGB( &appMPlayer.subWindow,appMPlayer.subR,appMPlayer.subG,appMPlayer.subB );
   wsClearWindow( appMPlayer.subWindow );
   wsPostRedisplay( &appMPlayer.subWindow );
  }

 if ( mplDrawBuffer ) free( mplDrawBuffer );
 if ( ( mplDrawBuffer = (unsigned char *)calloc( 1,appMPlayer.main.Bitmap.ImageSize ) ) == NULL )
  { mp_msg( MSGT_GPLAYER,MSGL_STATUS,MSGTR_NEMDB ); return; }

#if 0
// if ( vo_wm_type == vo_wm_Unknown ) 
  wsVisibleWindow( &appMPlayer.mainWindow,wsHideWindow );
 wsResizeWindow( &appMPlayer.mainWindow,appMPlayer.main.width,appMPlayer.main.height );
 wsMoveWindow( &appMPlayer.mainWindow,True,appMPlayer.main.x,appMPlayer.main.y );
 wsResizeImage( &appMPlayer.mainWindow,appMPlayer.main.width,appMPlayer.main.height );
 wsSetShape( &appMPlayer.mainWindow,appMPlayer.main.Mask.Image );
 wsWindowDecoration( &appMPlayer.mainWindow,appMPlayer.mainDecoration );
 mainVisible=1; mplMainRender=1; wsPostRedisplay( &appMPlayer.mainWindow );
 wsVisibleWindow( &appMPlayer.mainWindow,wsShowWindow );
#else
 wsDestroyWindow( &appMPlayer.mainWindow );

 wsCreateWindow( &appMPlayer.mainWindow,
   appMPlayer.main.x,appMPlayer.main.y,appMPlayer.main.width,appMPlayer.main.height,
   wsNoBorder,wsShowMouseCursor|wsHandleMouseButton|wsHandleMouseMove,wsShowFrame|wsMaxSize|wsHideWindow,"MPlayer" );
 wsCreateImage( &appMPlayer.mainWindow,appMPlayer.main.Bitmap.Width,appMPlayer.main.Bitmap.Height );
 wsSetShape( &appMPlayer.mainWindow,appMPlayer.main.Mask.Image );
 wsSetIcon( wsDisplay,appMPlayer.mainWindow.WindowID,guiIcon,guiIconMask );

 appMPlayer.mainWindow.ReDraw=(void *)mplMainDraw;
 appMPlayer.mainWindow.MouseHandler=mplMainMouseHandle;
 appMPlayer.mainWindow.KeyHandler=mplMainKeyHandle;
 appMPlayer.mainWindow.DandDHandler=mplDandDHandler;

 wsXDNDMakeAwareness( &appMPlayer.mainWindow );
 if ( !appMPlayer.mainDecoration ) wsWindowDecoration( &appMPlayer.mainWindow,0 );
 wsVisibleWindow( &appMPlayer.mainWindow,wsShowWindow );
 mainVisible=1;
#endif
 btnModify( evSetVolume,guiIntfStruct.Volume );
 btnModify( evSetBalance,guiIntfStruct.Balance );
 btnModify( evSetMoviePosition,guiIntfStruct.Position );
 btnModify( evFullScreen,!appMPlayer.subWindow.isFullScreen );
}

void mplSetFileName( char * dir,char * name,int type )
{
 if ( !name ) return;
 
 if ( !dir ) guiSetFilename( guiIntfStruct.Filename,name )
  else guiSetDF( guiIntfStruct.Filename,dir,name );

// filename=guiIntfStruct.Filename;
 guiIntfStruct.StreamType=type;
 gfree( (void **)&guiIntfStruct.AudioFile );
 gfree( (void **)&guiIntfStruct.Subtitlename );
}

void mplPrev( void )
{
 plItem * prev;
 int      stop = 0;
 
 if ( guiIntfStruct.Playing == 2 ) return;
 switch ( guiIntfStruct.StreamType )
  {
#ifdef USE_DVDREAD
   case STREAMTYPE_DVD:
	if ( --guiIntfStruct.DVD.current_chapter == 0 )
	 {
	  guiIntfStruct.DVD.current_chapter=1;
	  if ( --guiIntfStruct.DVD.current_title <= 0 ) { guiIntfStruct.DVD.current_title=1; stop=1; }
	 }
	guiIntfStruct.Track=guiIntfStruct.DVD.current_title;
	break;
#endif
#ifdef HAVE_VCD
   case STREAMTYPE_VCD:
	if ( --guiIntfStruct.Track == 0 ) { guiIntfStruct.Track=1; stop=1; }
	break;
#endif
   default: 
	if ( (prev=gtkSet( gtkGetPrevPlItem,0,NULL)) )
	 {
	  mplSetFileName( prev->path,prev->name,STREAMTYPE_FILE );
	  mplGotoTheNext=0;
	  break;
	 }
	return;
  }
 if ( stop ) mplEventHandling( evStop,0 );
 if ( guiIntfStruct.Playing == 1 ) mplEventHandling( evPlay,0 );
}

void mplNext( void )
{
 int      stop = 0;
 plItem * next;

 if ( guiIntfStruct.Playing == 2 ) return;
 switch ( guiIntfStruct.StreamType )
  {
#ifdef USE_DVDREAD
   case STREAMTYPE_DVD:
	if ( guiIntfStruct.DVD.current_chapter++ == guiIntfStruct.DVD.chapters )
	 {
	  guiIntfStruct.DVD.current_chapter=1;
	  if ( ++guiIntfStruct.DVD.current_title > guiIntfStruct.DVD.titles ) { guiIntfStruct.DVD.current_title=guiIntfStruct.DVD.titles; stop=1; }
	 }
	guiIntfStruct.Track=guiIntfStruct.DVD.current_title;
	break;
#endif
#ifdef HAVE_VCD
   case STREAMTYPE_VCD:
	if ( ++guiIntfStruct.Track > guiIntfStruct.VCDTracks ) { guiIntfStruct.Track=guiIntfStruct.VCDTracks; stop=1; }
	break;
#endif
   default:
	if ( (next=gtkSet( gtkGetNextPlItem,0,NULL)) ) 
	 { 
	  mplSetFileName( next->path,next->name,STREAMTYPE_FILE );
	  mplGotoTheNext=0;
	  break;
	 }
	return;
  }
 if ( stop ) mplEventHandling( evStop,0 );
 if ( guiIntfStruct.Playing == 1 ) mplEventHandling( evPlay,0 );
}

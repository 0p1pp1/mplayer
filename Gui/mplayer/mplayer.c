
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>

#include "./mplayer.h"
#include "../events.h"
#include "../app.h"
#include "../interface.h"
#include "../skin/skin.h"
#include "../skin/font.h"
#include "../wm/ws.h"
#include "../wm/wskeys.h"
#include "../wm/widget.h"
#include "../bitmap/bitmap.h"

#include "../../config.h"
#include "../../help_mp.h"
#include "../../libvo/x11_common.h"
#include "../../libmpdemux/stream.h"
#include "../../mp_msg.h"

#define mplRedrawTimerConst 5

int mplRedrawTimer = mplRedrawTimerConst;
int mplTimer = 0;

void mplEventHandling( int msg,float param );

#include "widgets.h"
#include "play.h"
#include "menu.h"
#include "mw.h"
#include "sw.h"
#include "widgets.h"

void mplTimerHandler( void )
{
 mplTimer++;
 mplRedrawTimer--;
 if ( mplRedrawTimer == 0 ) mplEventHandling( evRedraw,0 );
}

void mplInit( void * disp )
{
 int i;

 // init fields of this struct to default values
 guiIntfStruct.Balance=50.0f;
 guiIntfStruct.StreamType=-1;

 // fork() a process which runs gtkThreadProc()  [gtkPID]
 gtkInit();

 // opens X display, checks for extensions (XShape, DGA etc)
 wsXInit( disp );

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

 vo_setwindow( appMPlayer.subWindow.WindowID, appMPlayer.subWindow.wGC );

// i=wsHideFrame|wsMaxSize|wsHideWindow;
// if ( appMPlayer.mainDecoration ) i=wsShowFrame|wsMaxSize|wsHideWindow;
 i=wsShowFrame|wsMaxSize|wsHideWindow;
 wsCreateWindow( &appMPlayer.mainWindow,
  appMPlayer.main.x,appMPlayer.main.y,appMPlayer.main.width,appMPlayer.main.height,
  wsNoBorder,wsShowMouseCursor|wsHandleMouseButton|wsHandleMouseMove,i,"MPlayer" ); //wsMinSize|

 wsSetShape( &appMPlayer.mainWindow,appMPlayer.main.Mask.Image );

 mplMenuInit();

 #ifdef DEBUG
  mp_msg( MSGT_GPLAYER,MSGL_DBG2,"[main] Depth on screen: %d\n",wsDepthOnScreen );
  mp_msg( MSGT_GPLAYER,MSGL_DBG2,"[main] parent: 0x%x\n",(int)appMPlayer.mainWindow.WindowID );
  mp_msg( MSGT_GPLAYER,MSGL_DBG2,"[main] sub: 0x%x\n",(int)appMPlayer.subWindow.WindowID );
 #endif

 appMPlayer.mainWindow.ReDraw=mplMainDraw;
 appMPlayer.mainWindow.MouseHandler=mplMainMouseHandle;
 appMPlayer.mainWindow.KeyHandler=mplMainKeyHandle;

 appMPlayer.subWindow.ReDraw=mplSubDraw;
 appMPlayer.subWindow.MouseHandler=mplSubMouseHandle;
 appMPlayer.subWindow.KeyHandler=mplMainKeyHandle;

 wsSetBackgroundRGB( &appMPlayer.subWindow,appMPlayer.subR,appMPlayer.subG,appMPlayer.subB );
 wsClearWindow( appMPlayer.subWindow );
 if ( appMPlayer.sub.Bitmap.Image ) wsConvert( &appMPlayer.subWindow,appMPlayer.sub.Bitmap.Image,appMPlayer.sub.Bitmap.ImageSize );

 wsPostRedisplay( &appMPlayer.mainWindow );
 wsPostRedisplay( &appMPlayer.subWindow );

 btnModify( evSetVolume,guiIntfStruct.Volume );
 btnModify( evSetBalance,guiIntfStruct.Balance );
 btnModify( evSetMoviePosition,guiIntfStruct.Position );
 
 if ( fullscreen )
  {
   btnModify( evFullScreen,btnPressed );
   mplFullScreen();
  }

 guiIntfStruct.Playing=0;

 if ( !appMPlayer.mainDecoration ) wsWindowDecoration( &appMPlayer.mainWindow,0 );
 
 wsVisibleWindow( &appMPlayer.mainWindow,wsShowWindow );
 wsVisibleWindow( &appMPlayer.subWindow,wsShowWindow );
}


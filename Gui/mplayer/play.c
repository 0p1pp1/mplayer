
#include <stdlib.h>
#include <stdio.h>

#include <unistd.h>
#include <signal.h>

int    mplParent = 1;

int    moviex,moviey,moviewidth,movieheight;

#include "../app.h"

#include "../wm/ws.h"
#include "../wm/wskeys.h"
#include "../wm/widget.h"

#include "../../config.h"
#include "../../libvo/x11_common.h"

#include "widgets.h"
#include "./mplayer.h"
#include "psignal.h"
#include "play.h"

mplCommStruct * mplShMem;
char          * Filename = NULL;

extern float rel_seek_secs;
extern int abs_seek_pos;


void mplPlayerThread( void )
{
// mplayer( 0,NULL,NULL );
}

void mplFullScreen( void )
{
// if ( appMPlayer.subWindow.isFullScreen )
//  {
//  }
 wsFullScreen( &appMPlayer.subWindow ); 
 mplResize( 0,0,appMPlayer.subWindow.Width,appMPlayer.subWindow.Height );
}

extern int mplSubRender;

void mplStop()
{
 if ( !mplShMem->Playing ) return;
// ---
printf("%%%%%% STOP  \n");
// ---
 mplShMem->Playing=0;
 if ( !appMPlayer.subWindow.isFullScreen )
  {
   wsMoveWindow( &appMPlayer.subWindow,appMPlayer.sub.x,appMPlayer.sub.y );
   wsResizeWindow( &appMPlayer.subWindow,appMPlayer.sub.width,appMPlayer.sub.height );
  }
 mplSubRender=1;
 wsPostRedisplay( &appMPlayer.subWindow );
}

void mplPlay( void )
{
 if ( mplShMem->Filename[0] == 0 ) return;
 if ( mplShMem->Playing ) mplStop();
// ---
printf("%%%%%% PLAY  \n");
// ---
 mplShMem->Playing=1;
 mplSubRender=0;
}

void mplPause( void )
{
 if ( mplShMem->Playing != 1 ) return;
// ---
printf("%%%%%% PAUSE  \n");
// ---
 mplShMem->Playing=2;
 mplSubRender=0;
}

void mplResize( unsigned int X,unsigned int Y,unsigned int width,unsigned int height )
{
 vo_setwindowsize( width,height );
 vo_resize=1;
}

void mplMPlayerInit( int argc,char* argv[], char *envp[] )
{
#if 0
 mplShMem=shmem_alloc( ShMemSize );
#else
 mplShMem=calloc( 1,ShMemSize );
#endif
 signal( SIGTYPE,mplMainSigHandler );
// signal( SIGCHLD,SIG_IGN );

 mplShMem->Playing=0;
 mplShMem->Volume=0.0f;
 mplShMem->Position=0.0f;
 mplShMem->Balance=50.0f;
 mplShMem->Track=0;
 mplShMem->AudioType=0;
 mplShMem->StreamType=0;
 mplShMem->TimeSec=0;
 mplShMem->LengthInSec=0;

// ---
// ---
}

float mplGetPosition( void )
{ // return 0.0 ... 100.0
 return mplShMem->Position;
}

void mplRelSeek( float s )
{ // -+s
// ---
//printf("%%%%%% RelSEEK=%5.3f  \n",s);
// ---
 rel_seek_secs=s; abs_seek_pos=0;
}

void mplAbsSeek( float s )
{ // 0.0 ... 100.0
// ---
//printf("%%%%%% AbsSEEK=%5.3f  \n",s);
 rel_seek_secs=0.01*s; abs_seek_pos=3;
// ---
}


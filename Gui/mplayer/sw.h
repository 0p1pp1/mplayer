
// sub window

int             mplSubRender = 1;
int             SubVisible = 0;

void mplSubDraw( wsParamDisplay )
{
 if ( appMPlayer.subWindow.State == wsWindowFocusIn ) SubVisible++;

 if ( !appMPlayer.subWindow.Mapped ||
      appMPlayer.subWindow.Visible == wsWindowNotVisible ) return;

 if ( mplShMem->Playing )
  {
   vo_expose=1;
   mplSubRender=0;
  }

 if ( mplSubRender )
  {
   if ( appMPlayer.sub.Bitmap.Image ) wsPutImage( &appMPlayer.subWindow );
//   XFlush( wsDisplay );
  }
 appMPlayer.subWindow.State=0;
}

void mplSubMouseHandle( int Button,int X,int Y,int RX,int RY )
{
 static int mplSubMoved = 0;
 static int msButton = 0;

 mplMouseTimer=mplMouseTimerConst;
 wsVisibleMouse( &appMPlayer.subWindow,wsShowMouseCursor );
 

 switch( Button )
  {
   case wsPMMouseButton:
          gtkSendMessage( evShowPopUpMenu );
          break;
   case wsPRMouseButton:
	  if ( gtkShMem->visiblepopupmenu ) gtkSendMessage( evHidePopUpMenu );
          mplShowMenu( RX,RY );
          msButton=wsPRMouseButton;
          break;
   case wsRRMouseButton:
          mplHideMenu( RX,RY );
          msButton=0;
          break;
// ---
   case wsPLMouseButton:
	  if ( gtkShMem->visiblepopupmenu ) gtkSendMessage( evHidePopUpMenu );
          sx=X; sy=Y;
          msButton=wsPLMouseButton;
          mplSubMoved=0;
          break;
   case wsMoveMouse:
          switch ( msButton )
           {
            case wsPLMouseButton:
                   mplSubMoved=1;
                   if ( !appMPlayer.subWindow.isFullScreen ) wsMoveWindow( &appMPlayer.subWindow,False,RX - sx,RY - sy );
                   break;
            case wsPRMouseButton:
                   mplMenuMouseHandle( X,Y,RX,RY );
                   mplMouseTimer=mplMouseTimerConst;
                   break;
           }
          break;
   case wsRLMouseButton:
//          if ( ( !mplSubMoved )&&( ( SubVisible++%2 ) ) ) wsMoveTopWindow( &appMPlayer.mainWindow );
          if ( !mplSubMoved )
           {
            if( SubVisible++%2 )
             {
              wsMoveTopWindow( &appMPlayer.mainWindow );
              fprintf( stderr,"[sw] MAIN TOP\n" );
             }
             else
              {
               wsMoveTopWindow( &appMPlayer.subWindow );
               fprintf( stderr,"[sw] SUB TOP\n" );
              }
           }
          msButton=0;
          mplSubMoved=0;
          break;
  }
}


// main window

#include "mixer.h"

unsigned char * mplDrawBuffer = NULL;
int             mplMainRender = 1;
int             mplMainAutoPlay = 0;

int             mainVisible = 1;

int             boxMoved = 0;
int             msButton = 0;
int             sx = 0,sy = 0;
int             i,pot = 0;

char * Translate( char * str )
{
 static char   trbuf[512];
        char   tmp[128];
        int    i,c;
        int    t,h,m,s;
 memset( trbuf,0,512 );
 memset( tmp,0,128 );
 for ( c=0,i=0;i < strlen( str );i++ )
  {
   if ( str[i] != '$' ) { trbuf[c++]=str[i]; trbuf[c]=0; }
    else
    {
     switch ( str[++i] )
      {
       case 't':
            if ( mplShMem->Track < 10 ) strcat( trbuf,"0" );
            sprintf( tmp,"%d",mplShMem->Track ); strcat( trbuf,tmp );
            break;
       case 'f':
            if ( strlen( gtkShMem->fs.filename ) )
             {
              int i;
              strcpy( tmp,gtkShMem->fs.filename );
              for ( i=0;i < strlen( tmp );i++ )
               {
                t=0;
                if ( ( tmp[i] >= 'A' )&&( tmp[i] <= 'Z' ) ) t=32;
                tmp[i]=(char)( tmp[i] + t );
               }
              if ( tmp[strlen( tmp ) - 4] == '.' ) tmp[strlen( tmp ) - 4]=0;
              if ( tmp[strlen( tmp ) - 5] == '.' ) tmp[strlen( tmp ) - 5]=0;
             } else strcpy( tmp,"no file loaded" );
            strcat( trbuf,tmp );
            break;
       case 'F':
            if ( strlen( gtkShMem->fs.filename ) )
             {
              int i;
              strcpy( tmp,gtkShMem->fs.filename );
              for ( i=0;i < strlen( tmp );i++ )
               {
                char t = 0;
                if ( ( tmp[i] >= 'a' )&&( tmp[i] <= 'z' ) ) t=32;
                tmp[i]=tmp[i] - t;
               }
              if ( tmp[strlen( tmp ) - 4] == '.' ) tmp[strlen( tmp ) - 4]=0;
              if ( tmp[strlen( tmp ) - 5] == '.' ) tmp[strlen( tmp ) - 5]=0;
             } else strcpy( tmp,"NO FILE LOADED" );
            strcat( trbuf,tmp );
            break;
       case 'o':
            if ( strlen( gtkShMem->fs.filename ) )
             {
              strcat( trbuf,gtkShMem->fs.filename );
              if ( trbuf[strlen( trbuf ) - 4] == '.' ) trbuf[strlen( trbuf ) - 4]=0;
              if ( trbuf[strlen( trbuf ) - 5] == '.' ) trbuf[strlen( trbuf ) - 5]=0;
             } else strcat( trbuf,"no file loaded" );
            break;
       case '6': t=mplShMem->LengthInSec; goto calclengthhhmmss;
       case '1': t=mplShMem->TimeSec;
calclengthhhmmss:
            s=t%60; t=( t - s ) / 60; m=t%60; h=t/60;
            sprintf( tmp,"%02d:%02d:%02d",h,m,s ); strcat( trbuf,tmp );
            break;
       case '7': t=mplShMem->LengthInSec; goto calclengthmmmmss;
       case '2': t=mplShMem->TimeSec;
calclengthmmmmss:
            s=t%60; m=( ( t - s ) / 60 ) % 60;
            sprintf( tmp,"%04d:%02d",m,s ); strcat( trbuf,tmp );
            break;
       case '3':
            sprintf( tmp,"%02d",( mplShMem->TimeSec - ( mplShMem->TimeSec % 60 ) ) / 3600 ); strcat( trbuf,tmp );
            break;
       case '4':
            sprintf( tmp,"%02d",( ( mplShMem->TimeSec - ( mplShMem->TimeSec % 60 ) ) / 60 ) % 60 ); strcat( trbuf,tmp );
            break;
       case '5':
            sprintf( tmp,"%02d",mplShMem->TimeSec % 60 ); strcat( trbuf,tmp );
            break;
       case 'v':
            sprintf( tmp,"%3.2f%%",mplShMem->Volume ); strcat( trbuf,tmp );
            break;
       case 'V':
            sprintf( tmp,"%3.1f",mplShMem->Volume ); strcat( trbuf,tmp );
            break;
       case 'b':
            sprintf( tmp,"%3.2f%%",mplShMem->Balance ); strcat( trbuf,tmp );
            break;
       case 'B':
            sprintf( tmp,"%3.1f",mplShMem->Balance ); strcat( trbuf,tmp );
            break;
       case 's':
            if ( mplShMem->Playing == 0 ) strcat( trbuf,"s" );
            break;
       case 'l':
            if ( mplShMem->Playing == 1 ) strcat( trbuf,"p" );
            break;
       case 'e':
            if ( mplShMem->Playing == 2 ) strcat( trbuf,"e" );
            break;
       case '$':
            strcat( trbuf,"$" );
            break;
       default: continue;
      }
     c=strlen( trbuf );
    }
  }
 return trbuf;
}

void PutImage( txSample * bf,int x,int y,int max,int ofs )
{
 int i=0,ix,iy;
 unsigned long * buf = NULL;
 unsigned long * drw = NULL;
 unsigned long   tmp;

 if ( ( !bf )||( bf->Image == NULL ) ) return;

 i=( bf->Width * ( bf->Height / max ) ) * ofs;
 buf=(unsigned long *)mplDrawBuffer;
 drw=(unsigned long *)bf->Image;

 for ( iy=y;iy < y+bf->Height / max;iy++ )
  for ( ix=x;ix < x+bf->Width;ix++ )
   {
    tmp=drw[i++];
    if ( tmp != 0x00ff00ff )
     buf[ iy*appMPlayer.main.Bitmap.Width+ix ]=tmp;
   }
}

void mplMainDraw( wsParamDisplay )
{
 wItem    * item;
 txSample * image = NULL;
 int        i;
 char     * tmp;

 if ( appMPlayer.mainWindow.Visible == wsWindowNotVisible ||
      !mainVisible ||
      !appMPlayer.mainWindow.Mapped ) return;

 if ( mplMainRender )
  {
   memcpy( mplDrawBuffer,appMPlayer.main.Bitmap.Image,appMPlayer.main.Bitmap.ImageSize );
   for( i=0;i < appMPlayer.NumberOfItems + 1;i++ )
    {
     item=&appMPlayer.Items[i];
     switch( item->type )
      {
       case itButton:
            PutImage( &item->Bitmap,item->x,item->y,3,item->pressed );
            break;
       case itHPotmeter:
            PutImage( &item->Bitmap,item->x,item->y,item->phases,item->phases * ( item->value / 100.0f ) );
            PutImage( &item->Mask,item->x + (int)( ( item->width - item->psx ) * item->value / 100.0f ),item->y,3,item->pressed );
            break;
       case itPotmeter:
            PutImage( &item->Bitmap,item->x,item->y,item->phases,
             item->phases * ( item->value / 100.0f ) );
            break;
       case itSLabel:
            image=fntRender( item->fontid,0,item->width,"%s",item->label );
            goto drawrenderedtext;
       case itDLabel:
            image=fntRender( item->fontid,( mplTimer / 10 )%item->width,item->width,"%s",Translate( item->label ) );
drawrenderedtext:
            PutImage( image,item->x,item->y,1,0 );
            if ( image )
             {
              if ( image->Image ) free( image->Image );
              free( image );
             }
            break;
      }
    }
   wsConvert( &appMPlayer.mainWindow,mplDrawBuffer,appMPlayer.main.Bitmap.ImageSize );
   mplMainRender=0;
  }
 wsPutImage( &appMPlayer.mainWindow );
}

void mplMsgHandle( int msg,float param )
{
 int j;

 switch( msg )
  {
// --- user events
   case evExit:
        wsDoExit();
        break;
   case evIconify:
        wsIconify( appMPlayer.mainWindow );
        break;
   case evFullScreen:
        for ( j=0;j<appMPlayer.NumberOfItems + 1;j++ )
         {
          if ( appMPlayer.Items[j].msg == evFullScreen )
           {
            appMPlayer.Items[j].tmp=!appMPlayer.Items[j].tmp;
            appMPlayer.Items[j].pressed=appMPlayer.Items[j].tmp;
           }
         }
        mplMainRender=1;
        mplFullScreen();
        break;

   case evPlaySwitchToPause:
        if ( Filename )
         {
          btnModify( evPlaySwitchToPause,btnDisabled );
          btnModify( evPauseSwitchToPlay,btnReleased );
         }
        if ( mplShMem->Playing == 1 ) goto NoPause;
   case evPlay:
        mplMainRender=1;
        mplPlay();
        break;

   case evPauseSwitchToPlay:
        btnModify( evPlaySwitchToPause,btnReleased );
        btnModify( evPauseSwitchToPlay,btnDisabled );
   case evPause:
NoPause:
        mplMainRender=1;
        mplPause();
        break;

   case evStop:
        btnModify( evPlaySwitchToPause,btnReleased );
        btnModify( evPauseSwitchToPlay,btnDisabled );
        mplMainRender=1;
        mplStop();
        break;

   case evLoadPlay:
        mplMainAutoPlay=1;
   case evLoad:
        mplMainRender=1;
        gtkSendMessage( evLoad );
        break;
   case evPrev:
        mplMainRender=1;
        #ifdef DEBUG
         dbprintf( 1,"[mw.h] previous stream ...\n" );
        #endif
        break;
   case evNext:
        mplMainRender=1;
        #ifdef DEBUG
         dbprintf( 1,"[mw.h] next stream ...\n" );
        #endif
        break;

   case evPlayList:
        mplMainRender=1;
        if ( gtkVisiblePlayList )
         {
          btnModify( evPlayList,btnReleased );
          gtkShMem->vs.window=evPlayList;
          gtkSendMessage( evHideWindow );
          gtkVisiblePlayList=0;
         }
         else
          {
           gtkSendMessage( evPlayList );
           btnModify( evPlayList,btnPressed );
           gtkVisiblePlayList=1;
          }
        break;

   case evSkinBrowser: gtkSendMessage( evSkinBrowser ); break;
   case evAbout:       gtkSendMessage( evAbout ); break;
   case evPreferences: gtkSendMessage( evPreferences ); break;

   case evForward1min:      mplRelSeek( 60 );  break;
   case evBackward1min:     mplRelSeek( -60 ); break;
   case evForward10sec:     mplRelSeek( 10 );  break;
   case evBackward10sec:    mplRelSeek( -10 ); break;
   case evSetMoviePosition: mplAbsSeek( param ); break;

   case evIncVolume:  mixerIncVolume(); break;
   case evDecVolume:  mixerDecVolume(); break;
   case evSetVolume:  mixerSetVolume( param ); break;
   case evSetBalance: mixerSetBalance( param ); break;
   case evMute:       mixerMute();       break;

   case evIncAudioBufDelay: mplIncAudioBufDelay(); break;
   case evDecAudioBufDelay: mplDecAudioBufDelay(); break;

   case evNormalSize: if ( mplShMem->Playing ) wsResizeWindow( &appMPlayer.subWindow,mplwidth,mplheight ); break;
   case evDoubleSize: if ( mplShMem->Playing ) wsResizeWindow( &appMPlayer.subWindow,mplwidth * 2,mplheight * 2 ); break;

// --- timer events
   case evHideMouseCursor:
        wsVisibleMouse( &appMPlayer.subWindow,wsHideMouseCursor );
        break;
   case evRedraw:
        mplMainRender=1;
        wsPostRedisplay( &appMPlayer.mainWindow );
        if ( !mplShMem->Playing ) wsPostRedisplay( &appMPlayer.subWindow );
        XFlush( wsDisplay );
        mplRedrawTimer=mplRedrawTimerConst;
        break;
   case evGeneralTimer:
        if ( mplMainAutoPlay )
         {
          mplMainRender=1;
          mplMainAutoPlay=0;
          mplPlay();
         }
        break;
// --- system events
   case evNone:
        dbprintf( 1,"[mw] event none received.\n" );
        break;
   default:
        dbprintf( 1,"[mw] unknown event received ( %d,%.2f ).\n",msg,param );
        break;
  }
}

void mplMainMouseHandle( int Button,int X,int Y,int RX,int RY )
{
 static int     itemtype = 0;
 static int     SelectedButton = -1;
        wItem * item = NULL;
        float   value = 0.0f;
 wsVisibleMouse( &appMPlayer.subWindow,wsShowMouseCursor );
 switch ( Button )
  {
   case wsPRMouseButton:
          mplShowMenu( RX,RY );
          msButton=wsPRMouseButton;
          break;
   case wsRRMouseButton:
          mplHideMenu( RX,RY );
          msButton=0;
          break;
   case wsPLMouseButton:
          sx=X; sy=Y;
          boxMoved=1;
          msButton=wsPLMouseButton;
          for ( i=0;i < appMPlayer.NumberOfItems + 1;i++ )
           {
            item=&appMPlayer.Items[i];
            if ( item->pressed != btnDisabled )
             {
              switch( item->type )
               {
                case itButton:
                     if ( wgIsRect( X,Y,
                          item->x,item->y,
                          item->x+item->width,item->y+item->height ) )
                      {
                       item->pressed=btnPressed;
                       mplMainRender=1;
                       SelectedButton=i;
                       msButton=0;
                       boxMoved=0;
                      }
                     if ( ( SelectedButton > -1 ) &&
                        ( ( ( appMPlayer.Items[SelectedButton].msg == evPlaySwitchToPause && item->msg == evPauseSwitchToPlay ) ) ||
                          ( ( appMPlayer.Items[SelectedButton].msg == evPauseSwitchToPlay && item->msg == evPlaySwitchToPause ) ) ) )
                      {
                       appMPlayer.Items[SelectedButton].pressed=btnDisabled;
                       SelectedButton=i;
                       appMPlayer.Items[SelectedButton].pressed=btnPressed;
                      }
                     itemtype=itButton;
                     break;
                case itPotmeter:
                     if ( wgIsRect( X,Y,
                          item->x,item->y,
                          item->x+item->width,item->y+item->height ) )
                      {
                       item->pressed=btnPressed;
                       mplMainRender=1;
                       SelectedButton=i;
                       boxMoved=0;
                       msButton=itPotmeter;
                       itemtype=itPotmeter;
                      }
                      break;
                case itHPotmeter:
                     if ( wgIsRect( X,Y,
                          item->x,item->y,
                          item->x+item->width,item->y+item->height ) )
                      {
                       item->pressed=btnPressed;
                       mplMainRender=1;
                       SelectedButton=i;
                       boxMoved=0;
                       msButton=itHPotmeter;
                       itemtype=itHPotmeter;
                      }
                     break;
               }
             }
           }
          break;
   case wsMoveMouse:
          item=&appMPlayer.Items[SelectedButton];
          switch ( msButton )
           {
            case wsPLMouseButton:
                 wsMoveWindow( &appMPlayer.mainWindow,RX - abs( sx ),RY - abs( sy ) );
                 mplMainRender=0;
                 break;
            case wsPRMouseButton:
                 mplMenuMouseHandle( X,Y,RX,RY );
                 break;
            case itPotmeter:
                 value=(float)( X - item->x ) / item->width * 100.0f;
                 goto potihandled;
            case itHPotmeter:
                 value=(float)( X - item->x ) / item->width * 100.0f;
potihandled:
                 btnModify( item->msg,value );
                 if ( ( item->msg == evSetVolume )||( item->msg == evSetBalance ) ) mplMsgHandle( item->msg,item->value );
                 mplMainRender=1; wsPostRedisplay( &appMPlayer.mainWindow );
                 break;
           }
          break;
   case wsRLMouseButton:
          msButton=0;
          boxMoved=0;
          item=&appMPlayer.Items[SelectedButton];
          item->pressed=btnReleased;
          switch( itemtype )
           {
            case itButton:
                 if ( wgIsRect( X,Y,
                        item->x,item->y,
                        item->x+item->width,item->y+item->height ) ) value=0;
                  break;
            case itPotmeter:
            case itHPotmeter:
                 btnModify( item->msg,(float)( X - item->x ) / item->width * 100.0f );
                 value=item->value;
                 break;
           }
          if ( SelectedButton != -1 ) mplMsgHandle( item->msg,value );
          SelectedButton=-1;
          mplMainRender=1;
          itemtype=0;
          break;
   case wsPMMouseButton: break;
   case wsRMMouseButton: break;
  }
 if ( Button != wsMoveMouse ) wsPostRedisplay( &appMPlayer.mainWindow );
}

int keyPressed = 0;

void mplMainKeyHandle( int State,int Type,int Key )
{
 int msg = evNone;
 switch ( Key )
  {
   case '.':
   case '>':         msg=evNext; break;
   case ',':
   case '<':         msg=evPrev; break;

   case wsx:
   case wsX:
   case wsEscape:    msg=evExit; break;

   case wsUp:        msg=evForward1min; break;
   case wsDown:      msg=evBackward1min; break;
   case wsRight:     msg=evForward10sec; break;
   case wsLeft:      msg=evBackward10sec; break;

   case wsGrayMul:   msg=evIncVolume; break;
   case wsGrayDiv:   msg=evDecVolume; break;

   case wsGrayPlus:  msg=evIncAudioBufDelay; break;
   case wsGrayMinus: msg=evDecAudioBufDelay; break;

   case wsEnter:     msg=evPlay; break;
   case wsSpace:     msg=evPause; break;
   case wsa:
   case wsA:         msg=evAbout; break;
   case wsb:
   case wsB:         msg=evSkinBrowser; break;
   case wse:
   case wsE:         msg=evEqualeaser; break;
   case wsf:
   case wsF:         msg=evFullScreen; break;
   case wsl:
   case wsL:         msg=evLoad; break;
   case wsm:
   case wsM:         msg=evMute; break;
   case wss:
   case wsS:         msg=evStop; break;
   case wsp:
   case wsP:         msg=evPlayList; break;
  }
 if ( ( msg != evNone )&&( Type == wsKeyPressed ) )
  {
   mplMsgHandle( msg,0 );
   mplMainRender=1;
   wsPostRedisplay( &appMPlayer.mainWindow );
  }
}


// main window

#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <sys/stat.h>
#include <unistd.h>

#include "../app.h"
#include "../skin/font.h"
#include "../skin/skin.h"
#include "../wm/ws.h"

#include "../../config.h"
#include "../../help_mp.h"
#include "../../libvo/x11_common.h"

#include "../../libmpdemux/stream.h"
#include "../../mixer.h"
#include "../../libvo/sub.h"
#include "../../mplayer.h"

#include "../../libmpdemux/demuxer.h"
#include "../../libmpdemux/stheader.h"
#include "../../codec-cfg.h"


#include "play.h"
#include "widgets.h"

extern unsigned int GetTimerMS( void );

unsigned char * mplDrawBuffer = NULL;
int             mplMainRender = 1;

int             mplMainAutoPlay = 0;
int             mplMiddleMenu = 0;

int             mainVisible = 1;

int             boxMoved = 0;
int             sx = 0,sy = 0;
int             i,pot = 0;

inline void TranslateFilename( int c,char * tmp )
{
 int i;
 switch ( guiIntfStruct.StreamType )
  {
   case STREAMTYPE_STREAM:
        strcpy( tmp,guiIntfStruct.Filename );
        break;
   case STREAMTYPE_FILE:
          if ( ( guiIntfStruct.Filename )&&( guiIntfStruct.Filename[0] ) )
           {
	    if ( strrchr( guiIntfStruct.Filename,'/' ) ) strcpy( tmp,strrchr( guiIntfStruct.Filename,'/' ) + 1 );
	     else strcpy( tmp,guiIntfStruct.Filename );
            if ( tmp[strlen( tmp ) - 4] == '.' ) tmp[strlen( tmp ) - 4]=0;
            if ( tmp[strlen( tmp ) - 5] == '.' ) tmp[strlen( tmp ) - 5]=0;
           } else strcpy( tmp,MSGTR_NoFileLoaded );
          break;
#ifdef USE_DVDREAD
   case STREAMTYPE_DVD:
          if ( guiIntfStruct.DVD.current_chapter ) sprintf( tmp,MSGTR_Chapter,guiIntfStruct.DVD.current_chapter );
            else strcat( tmp,MSGTR_NoChapter );
          break;
#endif
#ifdef HAVE_VCD
   case STREAMTYPE_VCD:
        sprintf( tmp,MSGTR_VCDTrack,guiIntfStruct.Track );
	break;
#endif
   default: strcpy( tmp,MSGTR_NoMediaOpened );
  }
 if ( c )
  {
   for ( i=0;i < (int)strlen( tmp );i++ )
    {
     int t=0;
     if ( c == 1 ) { if ( ( tmp[i] >= 'A' )&&( tmp[i] <= 'Z' ) ) t=32; }
     if ( c == 2 ) { if ( ( tmp[i] >= 'a' )&&( tmp[i] <= 'z' ) ) t=-32; }
     tmp[i]=(char)( tmp[i] + t );
    }
  }
}

char * Translate( char * str )
{
 static char   trbuf[512];
        char   tmp[512];
        int    i,c;
        int    t;
 memset( trbuf,0,512 );
 memset( tmp,0,128 );
 for ( c=0,i=0;i < (int)strlen( str );i++ )
  {
   if ( str[i] != '$' ) { trbuf[c++]=str[i]; trbuf[c]=0; }
    else
    {
     switch ( str[++i] )
      {
       case 't': sprintf( tmp,"%02d",guiIntfStruct.Track ); strcat( trbuf,tmp ); break;
       case 'o': TranslateFilename( 0,tmp ); strcat( trbuf,tmp ); break;
       case 'f': TranslateFilename( 1,tmp ); strcat( trbuf,tmp ); break;
       case 'F': TranslateFilename( 2,tmp ); strcat( trbuf,tmp ); break;
       case '6': t=guiIntfStruct.LengthInSec; goto calclengthhhmmss;
       case '1': t=guiIntfStruct.TimeSec;
calclengthhhmmss:
            sprintf( tmp,"%02d:%02d:%02d",t/3600,t/60%60,t%60 ); strcat( trbuf,tmp );
            break;
       case '7': t=guiIntfStruct.LengthInSec; goto calclengthmmmmss;
       case '2': t=guiIntfStruct.TimeSec;
calclengthmmmmss:
            sprintf( tmp,"%04d:%02d",t/60,t%60 ); strcat( trbuf,tmp );
            break;
       case '3': sprintf( tmp,"%02d",guiIntfStruct.TimeSec / 3600 ); strcat( trbuf,tmp ); break;
       case '4': sprintf( tmp,"%02d",( ( guiIntfStruct.TimeSec / 60 ) % 60 ) ); strcat( trbuf,tmp ); break;
       case '5': sprintf( tmp,"%02d",guiIntfStruct.TimeSec % 60 ); strcat( trbuf,tmp ); break;
       case '8': sprintf( tmp,"%01d:%02d:%02d",guiIntfStruct.TimeSec / 3600,( guiIntfStruct.TimeSec / 60 ) % 60,guiIntfStruct.TimeSec % 60 ); strcat( trbuf,tmp ); break;
       case 'v': sprintf( tmp,"%3.2f%%",guiIntfStruct.Volume ); strcat( trbuf,tmp ); break;
       case 'V': sprintf( tmp,"%3.1f",guiIntfStruct.Volume ); strcat( trbuf,tmp ); break;
       case 'b': sprintf( tmp,"%3.2f%%",guiIntfStruct.Balance ); strcat( trbuf,tmp ); break;
       case 'B': sprintf( tmp,"%3.1f",guiIntfStruct.Balance ); strcat( trbuf,tmp ); break;
       case 'd': sprintf( tmp,"%d",guiIntfStruct.FrameDrop ); strcat( trbuf,tmp ); break;
       case 'x': sprintf( tmp,"%d",guiIntfStruct.MovieWidth ); strcat( trbuf,tmp ); break;
       case 'y': sprintf( tmp,"%d",guiIntfStruct.MovieHeight ); strcat( trbuf,tmp ); break;
       case 'C': sprintf( tmp,"%s", guiIntfStruct.sh_video? ((sh_video_t *)guiIntfStruct.sh_video)->codec->name : "");
                 strcat( trbuf,tmp ); break;
       case 's': if ( guiIntfStruct.Playing == 0 ) strcat( trbuf,"s" ); break;
       case 'l': if ( guiIntfStruct.Playing == 1 ) strcat( trbuf,"p" ); break;
       case 'e': if ( guiIntfStruct.Playing == 2 ) strcat( trbuf,"e" ); break;
       case 'a':
            if ( muted ) { strcat( trbuf,"n" ); break; }
            switch ( guiIntfStruct.AudioType )
             {
              case 0: strcat( trbuf,"n" ); break;
              case 1: strcat( trbuf,"m" ); break;
              case 2: strcat( trbuf,"t" ); break;
             }
            break;
       case 'T':
           switch ( guiIntfStruct.StreamType )
            {
             case STREAMTYPE_FILE:   strcat( trbuf,"f" ); break;
#ifdef HAVE_VCD
             case STREAMTYPE_VCD:    strcat( trbuf,"v" ); break;
#endif
             case STREAMTYPE_STREAM: strcat( trbuf,"u" ); break;
#ifdef USE_DVDREAD
             case STREAMTYPE_DVD:    strcat( trbuf,"d" ); break;
#endif
             default:                strcat( trbuf," " ); break;
            }
           break;
       case '$': strcat( trbuf,"$" ); break;
       default: continue;
      }
     c=strlen( trbuf );
    }
  }
 return trbuf;
}

inline void PutImage( txSample * bf,int x,int y,int max,int ofs )
{
 int i=0,ix,iy;
 uint32_t * buf = NULL;
 uint32_t * drw = NULL;
 uint32_t   tmp;

 if ( ( !bf )||( bf->Image == NULL ) ) return;

 i=( bf->Width * ( bf->Height / max ) ) * ofs;
 buf=(uint32_t *)mplDrawBuffer;
 drw=(uint32_t *)bf->Image;

 for ( iy=y;iy < (int)(y+bf->Height / max);iy++ )
  for ( ix=x;ix < (int)(x+bf->Width);ix++ )
   {
    tmp=drw[i++];
    if ( tmp != 0x00ff00ff )
     buf[ iy*appMPlayer.main.Bitmap.Width+ix ]=tmp;
   }
}

void mplMainDraw( void )
{
 wItem    * item;
 txSample * image = NULL;
 int        i, type;

 if ( appMPlayer.mainWindow.State == wsWindowClosed ) exit_player( MSGTR_Exit_quit );
 
 if ( appMPlayer.mainWindow.Visible == wsWindowNotVisible ||
      !mainVisible ) return;
//      !appMPlayer.mainWindow.Mapped ) return;

 btnModify( evSetMoviePosition,guiIntfStruct.Position );
 btnModify( evSetVolume,guiIntfStruct.Volume );

 if ( mplMainRender && appMPlayer.mainWindow.State == wsWindowExpose )
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
       case itPotmeter:
            PutImage( &item->Bitmap,item->x,item->y,item->phases,( item->phases - 1 ) * ( item->value / 100.0f ) );
            break;
       case itHPotmeter:
            PutImage( &item->Bitmap,item->x,item->y,item->phases,item->phases * ( item->value / 100.0f ) );
            PutImage( &item->Mask,item->x + (int)( ( item->width - item->psx ) * item->value / 100.0f ),item->y,3,item->pressed );
            break;
       case itSLabel:
            image=fntRender( item,0,"%s",item->label );
            goto drawrenderedtext;
       case itDLabel:
            {
	     char * t = Translate( item->label );
	     int    l = fntTextWidth( item->fontid,t );
             image=fntRender( item,(GetTimerMS() / 20)%(l?l:item->width),"%s",t );
	    }
drawrenderedtext:
            if ( image )
             {
              PutImage( image,item->x,item->y,1,0 );
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
// XFlush( wsDisplay );
}

extern void exit_player(char* how);
extern char * dvd_device;
extern int vcd_track;
extern char * cdrom_device;
extern int osd_visible;

void mplEventHandling( int msg,float param )
{
 int j;
 int iparam = (int)param;

 switch( msg )
  {
// --- user events
   case evExit:
        exit_player( "Exit" );
        break;

   case evPlayNetwork:
        if ( guiIntfStruct.Subtitlename ) { free( guiIntfStruct.Subtitlename ); guiIntfStruct.Subtitlename=NULL; }
	if ( guiIntfStruct.AudioFile ) { free( guiIntfStruct.AudioFile ); guiIntfStruct.AudioFile=NULL; }
	guiIntfStruct.StreamType=STREAMTYPE_STREAM;
        goto play;
   case evSetURL:
        gtkShow( evPlayNetwork,NULL );
	break;

   case evSetAudio:
        if ( !guiIntfStruct.demuxer || audio_id == iparam ) break;
	audio_id=iparam;
	goto play;

   case evSetVideo:
        if ( !guiIntfStruct.demuxer || video_id == iparam ) break;
	video_id=iparam;
	goto play;

#ifdef HAVE_VCD
   case evSetVCDTrack:
        guiIntfStruct.Track=iparam;
   case evPlayVCD:
 	gtkSet( gtkClearStruct,0,(void *)guiALL );
	guiIntfStruct.StreamType=STREAMTYPE_VCD;
	goto play;
#endif
#ifdef USE_DVDREAD
   case evPlayDVD:
        guiIntfStruct.DVD.current_title=1;
        guiIntfStruct.DVD.current_chapter=1;
        guiIntfStruct.DVD.current_angle=1;
play_dvd_2:
 	gtkSet( gtkClearStruct,0,(void *)(guiALL - guiDVD) );
        guiIntfStruct.StreamType=STREAMTYPE_DVD;
	goto play;
#endif
   case evPlay:
   case evPlaySwitchToPause:
play:

        if ( ( msg == evPlaySwitchToPause )&&( guiIntfStruct.Playing == 2 ) ) goto NoPause;

	if ( gtkSet( gtkGetCurrPlItem,0,NULL ) &&( guiIntfStruct.StreamType == STREAMTYPE_FILE ) )
	 {
	  plItem * next = gtkSet( gtkGetCurrPlItem,0,NULL );
	  plLastPlayed=next;
	  mplSetFileName( next->path,next->name,STREAMTYPE_FILE );
	 }

        switch ( guiIntfStruct.StreamType )
         {
	  case STREAMTYPE_STREAM:
	  case STREAMTYPE_FILE:
	       gtkSet( gtkClearStruct,0,(void *)(guiALL - guiFilenames) );
	       break;
#ifdef HAVE_VCD
          case STREAMTYPE_VCD:
	       gtkSet( gtkClearStruct,0,(void *)(guiALL - guiVCD - guiFilenames) );
	       if ( !cdrom_device ) cdrom_device=DEFAULT_CDROM_DEVICE;
	       mplSetFileName( NULL,cdrom_device,STREAMTYPE_VCD );
	       if ( guiIntfStruct.Playing != 2 )
	        {
		 if ( !guiIntfStruct.Track )
		  {
		   if ( guiIntfStruct.VCDTracks > 1 ) guiIntfStruct.Track=2;
		    else guiIntfStruct.Track=1;
		  }
                 guiIntfStruct.DiskChanged=1;
		}
	       break;
#endif
#ifdef USE_DVDREAD
          case STREAMTYPE_DVD:
	       gtkSet( gtkClearStruct,0,(void *)(guiALL - guiDVD - guiFilenames) );
	       if ( !dvd_device ) dvd_device=DEFAULT_DVD_DEVICE;
	       mplSetFileName( NULL,dvd_device,STREAMTYPE_DVD );
	       if ( guiIntfStruct.Playing != 2 )
	        {
		 guiIntfStruct.Title=guiIntfStruct.DVD.current_title;
		 guiIntfStruct.Chapter=guiIntfStruct.DVD.current_chapter;
		 guiIntfStruct.Angle=guiIntfStruct.DVD.current_angle;
                 guiIntfStruct.DiskChanged=1;
		} 
               break;
#endif
         }
	guiIntfStruct.NewPlay=1;
        mplPlay();
        break;
#ifdef USE_DVDREAD
   case evSetDVDSubtitle:
        dvdsub_id=iparam;
        goto play_dvd_2;
        break;
   case evSetDVDAudio:
        audio_id=iparam;
        goto play_dvd_2;
        break;
   case evSetDVDChapter:
        guiIntfStruct.DVD.current_chapter=iparam;
        goto play_dvd_2;
        break;
   case evSetDVDTitle:
        guiIntfStruct.DVD.current_title=iparam;
	guiIntfStruct.DVD.current_chapter=1;
	guiIntfStruct.DVD.current_angle=1;
        goto play_dvd_2;
        break;
#endif

   case evPause:
   case evPauseSwitchToPlay:
NoPause:
        mplPause();
        break;

   case evStop: 
	guiIntfStruct.Playing=guiSetStop; 
	mplState(); 
	guiIntfStruct.NoWindow=False;
	break;

   case evLoadPlay:
        mplMainAutoPlay=1;
//	guiIntfStruct.StreamType=STREAMTYPE_FILE;
   case evLoad:
	gtkSet( gtkDelPl,0,NULL );
        gtkShow( evLoad,NULL );
        break;
   case evLoadSubtitle:  gtkShow( evLoadSubtitle,NULL );  break;
   case evDropSubtitle:
	gfree( (void **)&guiIntfStruct.Subtitlename );
	guiLoadSubtitle( NULL );
	break;
   case evLoadAudioFile: gtkShow( evLoadAudioFile,NULL ); break;
   case evPrev: mplPrev(); break;
   case evNext: mplNext(); break;

   case evPlayList:    gtkShow( evPlayList,NULL );        break;
   case evSkinBrowser: gtkShow( evSkinBrowser,skinName ); break;
   case evAbout:       gtkShow( evAbout,NULL );           break;
   case evPreferences: gtkShow( evPreferences,NULL );     break;
   case evEqualizer:   gtkShow( evEqualizer,NULL );       break;

   case evForward10min:	    mplRelSeek( 600 ); break;
   case evBackward10min:    mplRelSeek( -600 );break;
   case evForward1min:      mplRelSeek( 60 );  break;
   case evBackward1min:     mplRelSeek( -60 ); break;
   case evForward10sec:     mplRelSeek( 10 );  break;
   case evBackward10sec:    mplRelSeek( -10 ); break;
   case evSetMoviePosition: mplAbsSeek( param ); break;

   case evIncVolume:  vo_x11_putkey( wsGrayMul ); break;
   case evDecVolume:  vo_x11_putkey( wsGrayDiv ); break;
   case evMute:       mixer_mute(); break;

   case evSetVolume:
        guiIntfStruct.Volume=param;
	goto set_volume;
   case evSetBalance: 
        guiIntfStruct.Balance=param;
set_volume:
        {
	 float l = guiIntfStruct.Volume * ( ( 100.0 - guiIntfStruct.Balance ) / 50.0 );
	 float r = guiIntfStruct.Volume * ( ( guiIntfStruct.Balance ) / 50.0 );
	 if ( l > guiIntfStruct.Volume ) l=guiIntfStruct.Volume;
	 if ( r > guiIntfStruct.Volume ) r=guiIntfStruct.Volume;
//	 printf( "!!! v: %.2f b: %.2f -> %.2f x %.2f\n",guiIntfStruct.Volume,guiIntfStruct.Balance,l,r );
         mixer_setvolume( l,r );
	}
#ifdef USE_OSD
	if ( osd_level )
	 {
	  osd_visible=vo_mouse_timer_const;
	  vo_osd_progbar_type=OSD_VOLUME;
	  vo_osd_progbar_value=( ( guiIntfStruct.Volume ) * 256.0 ) / 100.0;
	  vo_osd_changed( OSDTYPE_PROGBAR );
	 }
#endif
        break;


   case evIconify:
        switch ( iparam )
         {
          case 0: wsIconify( appMPlayer.mainWindow ); break;
          case 1: wsIconify( appMPlayer.subWindow ); break;
         }
        break;
   case evDoubleSize:
        if ( guiIntfStruct.Playing )
         {
          appMPlayer.subWindow.isFullScreen=True;
          appMPlayer.subWindow.OldX=( wsMaxX - guiIntfStruct.MovieWidth * 2 ) / 2;
          appMPlayer.subWindow.OldY=( wsMaxY - guiIntfStruct.MovieHeight * 2 ) / 2;
          appMPlayer.subWindow.OldWidth=guiIntfStruct.MovieWidth * 2; appMPlayer.subWindow.OldHeight=guiIntfStruct.MovieHeight * 2;
          wsFullScreen( &appMPlayer.subWindow );
	  vo_fs=0;
         }
        break;
   case evNormalSize:
        if ( guiIntfStruct.Playing )
         {
          appMPlayer.subWindow.isFullScreen=True;
          appMPlayer.subWindow.OldX=( wsMaxX - guiIntfStruct.MovieWidth ) / 2;
          appMPlayer.subWindow.OldY=( wsMaxY - guiIntfStruct.MovieHeight ) / 2;
          appMPlayer.subWindow.OldWidth=guiIntfStruct.MovieWidth; appMPlayer.subWindow.OldHeight=guiIntfStruct.MovieHeight;
          wsFullScreen( &appMPlayer.subWindow );
	  vo_fs=0;
	  break;
         } else if ( !appMPlayer.subWindow.isFullScreen ) break;
   case evFullScreen:
        for ( j=0;j<appMPlayer.NumberOfItems + 1;j++ )
         {
          if ( appMPlayer.Items[j].msg == evFullScreen )
           {
            appMPlayer.Items[j].tmp=!appMPlayer.Items[j].tmp;
            appMPlayer.Items[j].pressed=appMPlayer.Items[j].tmp;
           }
         }
        mplFullScreen();
        break;

   case evSetAspect:
	switch ( iparam )
	 {
	  case 2:  movie_aspect=16.0f / 9.0f; break;
	  case 3:  movie_aspect=4.0f / 3.0f;  break;
	  case 4:  movie_aspect=2.35;         break;
	  case 1:
	  default: movie_aspect=-1;
	 }
	wsClearWindow( appMPlayer.subWindow );
#ifdef USE_DVDREAD
	if ( guiIntfStruct.StreamType == STREAMTYPE_DVD || guiIntfStruct.StreamType == STREAMTYPE_VCD ) goto play_dvd_2;
	 else 
#endif
	 guiIntfStruct.NewPlay=1;
	break;

// --- timer events
   case evRedraw:
        mplMainRender=1;
        wsPostRedisplay( &appMPlayer.mainWindow );
        break;
// --- system events
#ifdef MP_DEBUG
   case evNone:
        mp_msg( MSGT_GPLAYER,MSGL_STATUS,"[mw] event none received.\n" );
        break;
   default:
        mp_msg( MSGT_GPLAYER,MSGL_STATUS,"[mw] unknown event received ( %d,%.2f ).\n",msg,param );
        break;
#endif
  }
}

#define itPLMButton (itNULL - 1)
#define itPRMButton (itNULL - 2)

void mplMainMouseHandle( int Button,int X,int Y,int RX,int RY )
{
 static int     itemtype = 0;
        int     i;
        wItem * item = NULL;
        float   value = 0.0f;

 static int     SelectedItem = -1;
        int     currentselected = -1;

 for ( i=0;i < appMPlayer.NumberOfItems + 1;i++ )
  if ( ( appMPlayer.Items[i].pressed != btnDisabled )&&
       ( wgIsRect( X,Y,appMPlayer.Items[i].x,appMPlayer.Items[i].y,appMPlayer.Items[i].x+appMPlayer.Items[i].width,appMPlayer.Items[i].y+appMPlayer.Items[i].height ) ) )
   { currentselected=i; break; }

 switch ( Button )
  {
   case wsPMMouseButton:
	  gtkShow( evHidePopUpMenu,NULL );
          mplShowMenu( RX,RY );
          itemtype=itPRMButton;
          break;
   case wsRMMouseButton:
          mplHideMenu( RX,RY,0 );
          break;

   case wsPLMouseButton:
	  gtkShow( evHidePopUpMenu,NULL );
          sx=X; sy=Y; boxMoved=1; itemtype=itPLMButton; // if move the main window
          SelectedItem=currentselected;
          if ( SelectedItem == -1 ) break; // yeees, i'm move the fucking window
          boxMoved=0; //mplMainRender=1; // No, not move the window, i'm pressed one button
          item=&appMPlayer.Items[SelectedItem];
          itemtype=item->type;
          item->pressed=btnPressed;
          switch( item->type )
           {
            case itButton:
                 if ( ( SelectedItem > -1 ) &&
                    ( ( ( appMPlayer.Items[SelectedItem].msg == evPlaySwitchToPause && item->msg == evPauseSwitchToPlay ) ) ||
                      ( ( appMPlayer.Items[SelectedItem].msg == evPauseSwitchToPlay && item->msg == evPlaySwitchToPause ) ) ) )
                  { appMPlayer.Items[SelectedItem].pressed=btnDisabled; }
                 break;
           }
          break;
   case wsRLMouseButton:
          boxMoved=0;
          item=&appMPlayer.Items[SelectedItem];
          item->pressed=btnReleased;
          SelectedItem=-1;
          if ( currentselected == - 1 ) { itemtype=0; break; }
          value=0;
          switch( itemtype )
           {
            case itPotmeter:
            case itHPotmeter:
                 btnModify( item->msg,(float)( X - item->x ) / item->width * 100.0f );
		 mplEventHandling( item->msg,item->value );
                 value=item->value;
                 break;
           }
          mplEventHandling( item->msg,value );
//          mplMainRender=1;
          itemtype=0;
          break;

   case wsPRMouseButton:
        gtkShow( evShowPopUpMenu,NULL );
        break;

// --- rolled mouse ... de szar :)))
   case wsP5MouseButton: value=-2.5f; goto rollerhandled;
   case wsP4MouseButton: value= 2.5f;
rollerhandled:
          item=&appMPlayer.Items[currentselected];
          if ( ( item->type == itHPotmeter )||( item->type == itVPotmeter )||( item->type == itPotmeter ) )
           {
            item->value+=value;
            btnModify( item->msg,item->value );
            mplEventHandling( item->msg,item->value );
           }
          break;

// --- moving
   case wsMoveMouse:
          item=&appMPlayer.Items[SelectedItem];
          switch ( itemtype )
           {
            case itPLMButton:
                 wsMoveWindow( &appMPlayer.mainWindow,False,RX - abs( sx ),RY - abs( sy ) );
                 mplMainRender=0;
                 break;
            case itPRMButton:
                 mplMenuMouseHandle( X,Y,RX,RY );
                 break;
            case itPotmeter:
                 item->value=(float)( X - item->x ) / item->width * 100.0f;
                 goto potihandled;
            case itHPotmeter:
                 item->value=(float)( X - item->x ) / item->width * 100.0f;
potihandled:
                 if ( item->value > 100.0f ) item->value=100.0f;
                 if ( item->value < 0.0f ) item->value=0.0f;
                 mplEventHandling( item->msg,item->value );
                 break;
           }
          break;
  }
// if ( Button != wsMoveMouse ) wsPostRedisplay( &appMPlayer.mainWindow );
}

int keyPressed = 0;

void mplMainKeyHandle( int KeyCode,int Type,int Key )
{
 int msg = evNone;

 if ( Type != wsKeyPressed ) return;
 
 if ( !Key )
  {
   switch ( KeyCode )
    {
     case wsXFMMPrev:     msg=evPrev;              break;
     case wsXFMMStop:	  msg=evStop;              break;
     case wsXFMMPlay:	  msg=evPlaySwitchToPause; break;
     case wsXFMMNext:	  msg=evNext;	           break;
     case wsXFMMVolUp:	  msg=evIncVolume;         break;
     case wsXFMMVolDown:  msg=evDecVolume;         break;
     case wsXFMMMute: 	  msg=evMute;	           break;
    }
  }
  else
   {
    switch ( Key )
     {
      case wsEnter:            msg=evPlay; break;
      case wsXF86LowerVolume:  msg=evDecVolume; break;
      case wsXF86RaiseVolume:  msg=evIncVolume; break;
      case wsXF86Mute:         msg=evMute; break;
      case wsXF86Play:         msg=evPlaySwitchToPause; break;
      case wsXF86Stop:         msg=evStop; break;
      case wsXF86Prev:         msg=evPrev; break;
      case wsXF86Next:         msg=evNext; break;
      case wsXF86Media:        msg=evLoad; break;
      case wsEscape:
    	    if ( appMPlayer.subWindow.isFullScreen )
	     { 
	      if ( guiIntfStruct.event_struct )
	       { memset( guiIntfStruct.event_struct,0,sizeof( XEvent ) ); guiIntfStruct.event_struct=NULL; }
	      mplEventHandling( evNormalSize,0 ); 
	      break; 
	     }

      default:          vo_x11_putkey( Key ); return;
     }
   }
 if ( msg != evNone ) mplEventHandling( msg,0 );
}

/* this will be used to handle Drag&Drop files */
void mplDandDHandler(int num,char** files)
{
  struct stat buf;
  int f = 0;

  char* subtitles = NULL;
  char* filename = NULL;

  if (num <= 0)
    return;


  /* now fill it with new items */
  for(f=0; f < num; f++){
    char* str = strdup( files[f] );
    plItem* item;

#ifdef USE_ICONV
    if ( strchr( str,'%' ) )
     {
      char * tmp=gconvert_uri_to_filename( str );
      free( str ); str=tmp;
     }
#endif

    if(stat(str,&buf) == 0 && S_ISDIR(buf.st_mode) == 0) {
      /* this is not a directory so try to play it */
      printf("Received D&D %s\n",str);
      
      /* check if it is a subtitle file */
      {
	char* ext = strrchr(str,'.');
	if (ext) {
	  static char supported[] = "utf/sub/srt/smi/rt//txt/ssa/aqt/";
	  char* type;
	  int len;
	  if((len=strlen(++ext)) && (type=strstr(supported,ext)) &&\
	     (type-supported)%4 == 0 && *(type+len) == '/'){
	    /* handle subtitle file */
	    gfree((void**)&subtitles);
	    subtitles = str;
	    continue;
	  }
	}
      }

      /* clear playlist */
      if (filename == NULL) {
	filename = files[f];
	gtkSet(gtkDelPl,0,NULL);
      }

      item = calloc(1,sizeof(plItem));
      
      /* FIXME: decompose file name ? */
      /* yes -- Pontscho */
      if ( strrchr( str,'/' ) ) {
	char * s = strrchr( str,'/' ); *s=0; s++;
	item->name = gstrdup( s );
	item->path = gstrdup( str );
      } else {
	item->name = strdup(str);
	item->path = strdup("");
      }
      gtkSet(gtkAddPlItem,0,(void*)item);
    } else {
      printf("Received not a file: %s !\n",str);
    }
    free( str );
  }

  if (filename) {
    mplSetFileName( NULL,filename,STREAMTYPE_FILE );
    if ( guiIntfStruct.Playing == 1 ) mplEventHandling( evStop,0 );
    mplEventHandling( evPlay,0 );
  }
  if (subtitles) {
    gfree((void**)&guiIntfStruct.Subtitlename);
    guiIntfStruct.Subtitlename = subtitles;
    guiLoadSubtitle(guiIntfStruct.Subtitlename);
  }
}

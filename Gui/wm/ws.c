
// --------------------------------------------------------------------------
//  AutoSpace Window System for Linux/Win32 v0.85
//   Writed by pontscho/fresh!mindworkz
// --------------------------------------------------------------------------

#include <X11/Xlib.h>
#include <X11/Xproto.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/Xatom.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

#include "ws.h"
#include "wsconv.h"
#include "../../config.h"

#include <X11/extensions/XShm.h>
#include <X11/extensions/shape.h>
#include <sys/ipc.h>
#include <sys/shm.h>

typedef struct
{
 long flags;
 long functions;
 long decorations;
 long input_mode;
 long status;
} MotifWmHints;

Atom                 wsMotifHints;

unsigned int         wsMaxX         = 0; // Screen width.
unsigned int         wsMaxY         = 0; // Screen height.

Display            * wsDisplay;
int                  wsScreen;
Window               wsRootWin;
XEvent               wsEvent;
int                  wsWindowDepth;
GC                   wsHGC;
MotifWmHints         wsMotifWmHints;
Atom                 wsTextProperlyAtom = None;

int                  wsDepthOnScreen = 0;
int                  wsRedMask = 0;
int                  wsGreenMask = 0;
int                  wsBlueMask = 0;
int                  wsOutMask = 0;

int                  wsTrue    = True;

wsTWindow          * wsWindowList[5] = { NULL,NULL,NULL,NULL,NULL };
int                  wsWLCount = 0;

unsigned long        wsKeyTable[512];

int                  wsUseXShm = 1;
int                  wsUseDGA = 1;
int                  wsUseXShape = 1;

int XShmGetEventBase( Display* );
inline int wsSearch( Window win );

#define MWM_HINTS_FUNCTIONS     (1L << 0)
#define MWM_HINTS_DECORATIONS   (1L << 1)
#define MWM_HINTS_INPUT_MODE    (1L << 2)
#define MWM_HINTS_STATUS        (1L << 3)

#define MWM_FUNC_ALL            (1L << 0)
#define MWM_FUNC_RESIZE         (1L << 1)
#define MWM_FUNC_MOVE           (1L << 2)
#define MWM_FUNC_MINIMIZE       (1L << 3)
#define MWM_FUNC_MAXIMIZE       (1L << 4)
#define MWM_FUNC_CLOSE          (1L << 5)

#define MWM_DECOR_ALL           (1L << 0)
#define MWM_DECOR_BORDER        (1L << 1)
#define MWM_DECOR_RESIZEH       (1L << 2)
#define MWM_DECOR_TITLE         (1L << 3)
#define MWM_DECOR_MENU          (1L << 4)
#define MWM_DECOR_MINIMIZE      (1L << 5)
#define MWM_DECOR_MAXIMIZE      (1L << 6)

#define MWM_INPUT_MODELESS 0
#define MWM_INPUT_PRIMARY_APPLICATION_MODAL 1
#define MWM_INPUT_SYSTEM_MODAL 2
#define MWM_INPUT_FULL_APPLICATION_MODAL 3
#define MWM_INPUT_APPLICATION_MODAL MWM_INPUT_PRIMARY_APPLICATION_MODAL

#define MWM_TEAROFF_WINDOW      (1L<<0)

void wsWindowDecoration( wsTWindow * win,long d )
{
 wsMotifHints=XInternAtom( wsDisplay,"_MOTIF_WM_HINTS",0 );
 if ( wsMotifHints != None )
  {
   memset( &wsMotifWmHints,0,sizeof( MotifWmHints ) );
   wsMotifWmHints.flags=( d?0:MWM_HINTS_FUNCTIONS | MWM_HINTS_DECORATIONS );
   wsMotifWmHints.functions=( d?0:MWM_FUNC_MOVE | MWM_FUNC_CLOSE | MWM_FUNC_MINIMIZE | MWM_FUNC_MAXIMIZE | MWM_FUNC_RESIZE );
   wsMotifWmHints.decorations=( d?MWM_DECOR_ALL:0 );
   XChangeProperty( wsDisplay,win->WindowID,wsMotifHints,wsMotifHints,32,
                    PropModeReplace,(unsigned char *)&wsMotifWmHints,5 );
  }
}

// ----------------------------------------------------------------------------------------------
//   Init X Window System.
// ----------------------------------------------------------------------------------------------

int wsIOErrorHandler( Display * dpy )
{
 fprintf( stderr,"[ws] io error in display.\n" );
 exit( 0 );
}

int wsErrorHandler( Display * dpy,XErrorEvent * Event )
{
 char type[128];
 XGetErrorText( wsDisplay,Event->error_code,type,128 );
 fprintf(stderr,"[ws] Error in display.\n");
 fprintf(stderr,"[ws]  Error code: %d ( %s )\n",Event->error_code,type );
 fprintf(stderr,"[ws]  Request code: %d\n",Event->request_code );
 fprintf(stderr,"[ws]  Minor code: %d\n",Event->minor_code );
 exit( 0 );
}

void wsXInit( void* mDisplay )
{
 int    eventbase;
 int    errorbase;

if(mDisplay){
 wsDisplay=mDisplay;
} else {
 char * DisplayName = ":0.0";
 if ( getenv( "DISPLAY" ) ) DisplayName=getenv( "DISPLAY" );
 wsDisplay=XOpenDisplay( DisplayName );
 if ( !wsDisplay )
  {
   fprintf( stderr,"[ws] couldn't open the display !\n" );
   exit( 0 );
  }
}

{ /* on remote display XShm will be disabled - LGB */
 char *dispname=DisplayString(wsDisplay);
 int localdisp=1;
 if (dispname&&*dispname!=':') {
    localdisp=0;
    wsUseXShm=0;
 }
 fprintf(stderr,"[ws] Display name: %s => %s display.\n",dispname,localdisp?"local":"REMOTE");
 if (!localdisp) fprintf(stderr,"[ws] Remote display, disabling XMITSHM\n");
}

 if ( !XShmQueryExtension( wsDisplay ) )
  {
   fprintf( stderr,"[ws] sorry, your system is not supported X shared memory extension.\n" );
   wsUseXShm=0;
  }
#ifdef HAVE_DGA2
 if ( !XDGAQueryExtension( wsDisplay,&eventbase,&errorbase ) )
  {
   fprintf( stderr,"[ws] sorry, your system is not supported DGA extension.\n" );
   wsUseDGA=0;
  }
#else
   wsUseDGA=0;
#endif
#ifdef HAVE_XSHAPE
  if ( !XShapeQueryExtension( wsDisplay,&eventbase,&errorbase ) )
   {
    fprintf( stderr,"[ws] sorry, your system is not supported XShape extension.\n" );
    wsUseXShape=0;
   }
#else
  wsUseXShape=0;
#endif

 XSynchronize( wsDisplay,True );

 wsScreen=DefaultScreen( wsDisplay );
 wsRootWin=RootWindow( wsDisplay,wsScreen );
 wsMaxX=DisplayWidth( wsDisplay,wsScreen );
 wsMaxY=DisplayHeight( wsDisplay,wsScreen );

 wsGetDepthOnScreen();
#ifdef DEBUG
  {
   int minor,major,shp;
   fprintf( stderr,"[ws] Screen depth: %d\n",wsDepthOnScreen );
   fprintf( stderr,"[ws]  size: %dx%d\n",wsMaxX,wsMaxY );
   fprintf( stderr,"[ws]  red mask: 0x%x\n",wsRedMask );
   fprintf( stderr,"[ws]  green mask: 0x%x\n",wsGreenMask );
   fprintf( stderr,"[ws]  blue mask: 0x%x\n",wsBlueMask );
   if ( wsUseXShm )
    {
     XShmQueryVersion( wsDisplay,&major,&minor,&shp );
     fprintf( stderr,"[ws] XShm version is %d.%d\n",major,minor );
    }
//   if ( wsUseDGA )
//    {
//     XDGAQueryVersion( wsDisplay,&major,&minor );
//     fprintf( stderr,"[ws] DGA version is %d.%d\n",major,minor );
//    }
   #ifdef HAVE_XSHAPE
    if ( wsUseXShape )
     {
      XShapeQueryVersion( wsDisplay,&major,&minor );
      fprintf( stderr,"[ws] XShape version is %d.%d\n",major,minor );
     }
   #endif
  }
#endif
 initConverter();
 wsOutMask=wsGetOutMask();
 switch ( wsOutMask )
  {
   case wsRGB32:
     wsConvFunc=BGR8880_to_RGB8880_c;
     break;
   case wsBGR32:
     wsConvFunc=BGR8880_to_BGR8880_c;
     break;
   case wsRGB24:
     #ifdef xHAVE_MMX
      wsConvFunc=BGR8880_to_RGB888_mmx;
     #else
      wsConvFunc=BGR8880_to_RGB888_c;
     #endif
     break;
   case wsBGR24:
     wsConvFunc=BGR8880_to_BGR888_c;
     break;
   case wsRGB16:
     wsConvFunc=BGR8880_to_RGB565_c;
     break;
   case wsBGR16:
     wsConvFunc=BGR8880_to_BGR565_c;
     break;
   case wsRGB15:
     wsConvFunc=BGR8880_to_RGB555_c;
     break;
   case wsBGR15:
     wsConvFunc=BGR8880_to_BGR555_c;
     break;
  }
 XSetIOErrorHandler( wsIOErrorHandler );
 XSetErrorHandler( wsErrorHandler );
}

// ----------------------------------------------------------------------------------------------
//   Create window.
//     X,Y   : window position
//     wX,wY : size of window
//     bW    : border width
//     cV    : visible mouse cursor on window
//     D     : visible frame, title, etc.
//     sR    : screen ratio
// ----------------------------------------------------------------------------------------------

XClassHint           wsClassHint;
XTextProperty        wsTextProperty;
Window               LeaderWindow;

void wsCreateWindow( wsTWindow * win,int X,int Y,int wX,int hY,int bW,int cV,unsigned char D,char * label )
{
 win->Property=D;
 if ( D & wsShowFrame ) win->Decorations=1;
 wsHGC=DefaultGC( wsDisplay,wsScreen );
// The window position and size.
 switch ( X )
  {
   case -1: win->X=( wsMaxX / 2 ) - ( wX / 2 ); break;
   case -2: win->X=wsMaxX - wX - 1; break;
   default: win->X=X; break;
  }
 switch ( Y )
  {
   case -1: win->Y=( wsMaxY / 2 ) - ( hY / 2 ); break;
   case -2: win->Y=wsMaxY - hY - 1; break;
   default: win->Y=Y; break;
  }
 win->Width=wX;
 win->Height=hY;
 win->OldX=win->X;
 win->OldY=win->Y;
 win->OldWidth=win->Width;
 win->OldHeight=win->Height;

// Border size for window.
 win->BorderWidth=bW;
// Hide Mouse Cursor
 win->wsCursor=None;
 win->wsMouseEventType=cV;
 win->wsCursorData[0]=0;
 win->wsCursorPixmap=XCreateBitmapFromData( wsDisplay,wsRootWin,win->wsCursorData,1,1 );
 if ( !(cV & wsShowMouseCursor) ) win->wsCursor=XCreatePixmapCursor( wsDisplay,win->wsCursorPixmap,win->wsCursorPixmap,&win->wsColor,&win->wsColor,0,0 );

 XGetWindowAttributes( wsDisplay,wsRootWin,&win->Attribs );
 if ( win->Attribs.depth < 15 )
  {
   fprintf( stderr,"[ws] sorry, this color depth is not enough.\n" );
   exit( 0 );
  }
 XMatchVisualInfo( wsDisplay,wsScreen,win->Attribs.depth,TrueColor,&win->VisualInfo );

// ---
 win->AtomLeaderClient=XInternAtom( wsDisplay,"WM_CLIENT_LEADER",False );
 win->AtomDeleteWindow=XInternAtom( wsDisplay,"WM_DELETE_WINDOW",False );
 win->AtomTakeFocus=XInternAtom( wsDisplay,"WM_TAKE_FOCUS",False );
 win->AtomRolle=XInternAtom( wsDisplay,"WM_WINDOW_ROLE",False );
 win->AtomProtocols=XInternAtom( wsDisplay,"WM_PROTOCOLS",False );
 {
  char buf[32]; int i;
  sprintf( buf,"_%s_REMOTE",label );
  for( i=0;i<strlen( buf );i++ )
    if ( ( buf[i] >= 'a' )&&( buf[i] <= 'z' ) ) buf[i]=buf[i] - 32;
  for( i=0;i<strlen( buf );i++ )
    if ( buf[i] == ' ' ) buf[i]='_';
  fprintf( stderr,"[ws] atomname: %s\n",buf );
  win->AtomRemote=XInternAtom( wsDisplay,buf,False );
 }
 win->AtomsProtocols[0]=win->AtomDeleteWindow;
 win->AtomsProtocols[1]=win->AtomTakeFocus;
 win->AtomsProtocols[2]=win->AtomRolle;
// ---

// win->WindowAttrib.background_pixel=BlackPixel( wsDisplay,wsScreen );
// win->WindowAttrib.border_pixel=BlackPixel( wsDisplay,wsScreen );
 win->WindowAttrib.background_pixel=BlackPixel( wsDisplay,wsScreen );
 win->WindowAttrib.border_pixel=WhitePixel( wsDisplay,wsScreen );
 win->WindowAttrib.colormap=XCreateColormap( wsDisplay,wsRootWin,win->VisualInfo.visual,AllocNone );
 win->WindowAttrib.event_mask=StructureNotifyMask | FocusChangeMask |
                              //SubstructureRedirectMask |
                              //SubstructureNotifyMask |
                              //ResizeRedirectMask |
                              //GCGraphicsExposures |
                              ExposureMask | PropertyChangeMask |
                              EnterWindowMask | LeaveWindowMask |
                              VisibilityChangeMask |
                              KeyPressMask | KeyReleaseMask;
 if ( ( cV & wsHandleMouseButton ) ) win->WindowAttrib.event_mask|=ButtonPressMask | ButtonReleaseMask;
 if ( ( cV & wsHandleMouseMove ) ) win->WindowAttrib.event_mask|=PointerMotionMask;
 win->WindowAttrib.cursor=win->wsCursor;
 win->WindowAttrib.override_redirect=False;
 if ( D & wsOverredirect ) win->WindowAttrib.override_redirect=True;

//  win->WindowAttrib.save_under=True;
//  win->WindowAttrib.do_not_propagate_mask = True;

 win->WindowMask=CWBackPixel | CWBorderPixel |
                 CWColormap | CWEventMask | CWCursor |
                 CWX | CWY | CWWidth | CWHeight |
                 CWOverrideRedirect;

 win->WindowID=XCreateWindow( wsDisplay,
  (win->Parent != 0?win->Parent:wsRootWin),
  win->X,win->Y,win->Width,win->Height,win->BorderWidth,
  win->VisualInfo.depth,
  InputOutput,
  win->VisualInfo.visual,
  win->WindowMask,&win->WindowAttrib );

 wsClassHint.res_name=label;
 wsClassHint.res_class="MPlayer";
 XSetClassHint( wsDisplay,win->WindowID,&wsClassHint );

 win->SizeHint.flags=PPosition | PSize | PResizeInc; // | PBaseSize
 win->SizeHint.x=win->X;
 win->SizeHint.y=win->Y;
 win->SizeHint.width=win->Width;
 win->SizeHint.height=win->Height;
 if ( D & wsMinSize )
  {
   win->SizeHint.flags|=PMinSize;
   win->SizeHint.min_width=win->Width;
   win->SizeHint.min_height=win->Height;
  }
 if ( D & wsMaxSize )
  {
   win->SizeHint.flags|=PMaxSize;
   win->SizeHint.max_width=win->Width;
   win->SizeHint.max_height=win->Height;
  }
 win->SizeHint.height_inc=1;
 win->SizeHint.width_inc=1;
// win->SizeHint.base_width=win->Width;
// win->SizeHint.base_height=win->Height;
 XSetWMNormalHints( wsDisplay,win->WindowID,&win->SizeHint );

 win->WMHints.flags=InputHint | StateHint;
 win->WMHints.input=True;
 win->WMHints.initial_state=NormalState;
 XSetWMHints( wsDisplay,win->WindowID,&win->WMHints );

 wsWindowDecoration( win,win->Decorations );
 XStoreName( wsDisplay,win->WindowID,label );
 XmbSetWMProperties( wsDisplay,win->WindowID,label,label,NULL,0,NULL,NULL,NULL );

 XSetWMProtocols( wsDisplay,win->WindowID,win->AtomsProtocols,3 );
 XChangeProperty( wsDisplay,win->WindowID,
                  win->AtomLeaderClient,
                  XA_WINDOW,32,PropModeReplace,
                  (unsigned char *)&LeaderWindow,1 );

 wsTextProperty.value=label;
 wsTextProperty.encoding=XA_STRING;
 wsTextProperty.format=8;
 wsTextProperty.nitems=strlen( label );
 XSetWMIconName( wsDisplay,win->WindowID,&wsTextProperty );

 XChangeProperty( wsDisplay,win->WindowID,
                  win->AtomRemote,XA_STRING,
                  8,PropModeReplace,
                  "REALIZED",8 );

//  win->Font=XLoadQueryFont( wsDisplay,"-adobe-helvetica-bold-r-normal--14-140-75-75-p-77-iso8859-1" );
//  -adobe-times-medium-r-normal--14-140-75-75-p-77-iso8859-1" );
//  -misc-fixed-bold-r-normal--13-120-75-75-C-80-iso8859-1" );
//  -misc-fixed-bold-r-normal--15-140-75-75-C-90-iso8859-1" );
//  -misc-fixed-medium-r-normal--15-140-75-75-C-90-iso8859-1" );
//  -adobe-times-medium-r-normal--24-240-75-75-p-124-iso8859-1" );
//  -adobe-helvetica-medium-r-normal--10-100-75-75-p-56-iso8859-1" );
//  -*-helvetica-bold-o-normal--14-*-*-*-p-*-iso8859-1" );
//  if ( !win->Font ) win->Font=XLoadQueryFont( wsDisplay,"fixed" );
//  if ( !win->Font )
//   {
//    fprintf( stderr,"[main] could not load font.\n" );
//    exit( 0 );
//   }
//  win->FontHeight=win->Font->ascent + win->Font->descent;
//
//  #ifdef DEBUG
//   fprintf( stderr,"[ws] font height: %d\n",win->FontHeight );
//  #endif

//  win->wGCV.font=win->Font->fid;
//  win->wGCV.foreground=wsBlack;
//  win->wGCV.background=wsBlack;

 win->wGC=XCreateGC( wsDisplay,win->WindowID,
  GCForeground | GCBackground,
  &win->wGCV );

 win->Visible=0;
 win->Focused=0;
 win->Mapped=0;
 win->Rolled=0;
 if ( D & wsShowWindow ) XMapWindow( wsDisplay,win->WindowID );

 wsCreateImage( win,win->Width,win->Height );
// --- End of creating --------------------------------------------------------------------------

 wsWindowList[wsWLCount++]=win;

 XFlush( wsDisplay );
 XSync( wsDisplay,False );

 win->ReDraw=NULL;
 win->ReSize=NULL;
 win->Idle=NULL;
 win->MouseHandler=NULL;
 win->KeyHandler=NULL;
 #ifdef DEBUG
  fprintf( stderr,"[ws] window is created. ( %s ).\n",label );
 #endif
}

void wsDestroyWindow( wsTWindow * win )
{
 int l;
 l=wsSearch( win->WindowID );
 wsWindowList[l]=NULL;
 if ( win->wsCursor != None )
   {
    XFreeCursor( wsDisplay,win->wsCursor );
    win->wsCursor=None;
   }
 XUnmapWindow( wsDisplay,win->WindowID );
 wsDestroyImage( win );
 XDestroyWindow( wsDisplay,win->WindowID );
 win->ReDraw=NULL;
 win->ReSize=NULL;
 win->Idle=NULL;
 win->MouseHandler=NULL;
 win->KeyHandler=NULL;
 win->Visible=0;
 win->Focused=0;
 win->Mapped=0;
 win->Rolled=0;
}

// ----------------------------------------------------------------------------------------------
//   Handle events.
// ----------------------------------------------------------------------------------------------

inline int wsSearch( Window win )
{
 int i;
 for ( i=0;i<wsWLCount;i++ ) if ( wsWindowList[i]->WindowID == win ) return i;
 return -1;
}

Bool wsEvents( Display * display,XEvent * Event,XPointer arg )
{
 KeySym        keySym;
 unsigned long i = 0;
 int           l;
 int           x,y;
 Window        child_window = 0;

 l=wsSearch( Event->xany.window );
 if ( l == -1 ) return !wsTrue;
 wsWindowList[l]->State=0;
 switch( Event->type )
  {
   case ClientMessage:
        if ( Event->xclient.message_type == wsWindowList[l]->AtomProtocols )
         {
          if ( Event->xclient.data.l[0] == wsWindowList[l]->AtomDeleteWindow )
           { wsTrue=False; break; }
          if ( Event->xclient.data.l[0] == wsWindowList[l]->AtomTakeFocus )
           { i=wsWindowFocusIn;  wsWindowList[l]->Focused=wsFocused; goto expose; }
          if ( Event->xclient.data.l[0] == wsWindowList[l]->AtomRolle )
           { fprintf( stderr,"[ws] rolled.\n" ); }
         }
        break;

//   case CirculateRequest:fprintf( stderr,"[ws,r] win: 0x%x\n",(int)Event->xcirculaterequest.window ); break;
//   case CirculateNotify: fprintf( stderr,"[ws,c] win: 0x%x\n",(int)Event->xcirculate.window );        break;

   case MapNotify:   i=wsWindowMapped;   wsWindowList[l]->Mapped=wsMapped;   goto expose;
   case UnmapNotify: i=wsWindowUnmapped; wsWindowList[l]->Mapped=wsNone;     goto expose;
   case FocusIn:
        if ( wsWindowList[l]->Focused == wsFocused ) break;
        i=wsWindowFocusIn; 
	wsWindowList[l]->Focused=wsFocused; 
	goto expose;
   case FocusOut:
        if ( wsWindowList[l]->Focused == wsNone ) break;
        i=wsWindowFocusOut; 
	wsWindowList[l]->Focused=wsNone;   
	goto expose;
   case VisibilityNotify:
        switch( Event->xvisibility.state )
         {
          case VisibilityUnobscured:        i=wsWindowVisible;        wsWindowList[l]->Visible=wsVisible;    goto expose;
          case VisibilityFullyObscured:     i=wsWindowNotVisible;     wsWindowList[l]->Visible=wsNotVisible; goto expose;
          case VisibilityPartiallyObscured: i=wsWindowPartialVisible; wsWindowList[l]->Visible=wsPVisible;   goto expose;
         }
expose:
        wsWindowList[l]->State=i;
        if ( wsWindowList[l]->ReDraw ) wsWindowList[l]->ReDraw( wsDisplay,Event->xany.window );
        break;

   case Expose:
        wsWindowList[l]->State=wsWindowExpose;
        if ( ( wsWindowList[l]->ReDraw )&&( !Event->xexpose.count ) ) wsWindowList[l]->ReDraw( wsDisplay,Event->xany.window );
        break;

   case ConfigureNotify:
        XTranslateCoordinates( wsDisplay,wsWindowList[l]->WindowID,wsRootWin,0,0,&x,&y,&child_window );
        if ( ( wsWindowList[l]->X != x )||( wsWindowList[l]->Y != y )||( wsWindowList[l]->Width != Event->xconfigure.width )||( wsWindowList[l]->Height != Event->xconfigure.height ) )
          {
           wsWindowList[l]->X=x; wsWindowList[l]->Y=y;
           wsWindowList[l]->Width=Event->xconfigure.width; wsWindowList[l]->Height=Event->xconfigure.height;
//           fprintf( stderr,"[ws] resize: %d,%d %dx%d\n",wsWindowList[l]->X,wsWindowList[l]->Y,Event->xconfigure.width,Event->xconfigure.height );
           if ( wsWindowList[l]->ReSize ) wsWindowList[l]->ReSize( wsWindowList[l]->X,wsWindowList[l]->Y,wsWindowList[l]->Width,wsWindowList[l]->Height );
          }

        wsWindowList[l]->Rolled=wsNone;
        if ( Event->xconfigure.y < 0 )
          { i=wsWindowRolled; wsWindowList[l]->Rolled=wsRolled; goto expose; }

        break;

   case KeyPress:   i=wsKeyPressed;  goto keypressed;
   case KeyRelease: i=wsKeyReleased;
keypressed:
        wsWindowList[l]->Alt=0;
        wsWindowList[l]->Shift=0;
        wsWindowList[l]->NumLock=0;
        wsWindowList[l]->Control=0;
        wsWindowList[l]->CapsLock=0;
        if ( Event->xkey.state & Mod1Mask ) wsWindowList[l]->Alt=1;
        if ( Event->xkey.state & Mod2Mask ) wsWindowList[l]->NumLock=1;
        if ( Event->xkey.state & ControlMask ) wsWindowList[l]->Control=1;
        if ( Event->xkey.state & ShiftMask ) wsWindowList[l]->Shift=1;
        if ( Event->xkey.state & LockMask ) wsWindowList[l]->CapsLock=1;
        keySym=XKeycodeToKeysym( wsDisplay,Event->xkey.keycode,0 );
        if ( keySym != NoSymbol )
         {
          keySym=( (keySym&0xff00) != 0?( (keySym&0x00ff) + 256 ):( keySym ) );
          wsKeyTable[ keySym ]=i;
          if ( wsWindowList[l]->KeyHandler )
            wsWindowList[l]->KeyHandler( Event->xkey.state,i,keySym );
         }
        break;

   case MotionNotify:  i=wsMoveMouse;                 goto buttonreleased;
   case ButtonRelease: i=Event->xbutton.button + 128; goto buttonreleased;
   case ButtonPress:   i=Event->xbutton.button;       goto buttonreleased;
   case EnterNotify:   i=wsEnterWindow;               goto buttonreleased;
   case LeaveNotify:   i=wsLeaveWindow;
buttonreleased:
        if ( wsWindowList[l]->MouseHandler )
          wsWindowList[l]->MouseHandler( i,Event->xbutton.x,Event->xbutton.y,Event->xmotion.x_root,Event->xmotion.y_root );
        break;

   case PropertyNotify:
	break;
        fprintf(stderr,"[ws] PropertyNotify %s\n",XGetAtomName( wsDisplay,Event->xproperty.atom ) );
        if ( Event->xproperty.atom == wsWindowList[l]->AtomRemote )
         {
          Atom            type;
          int             format;
          unsigned long   nitems, bytesafter;
          unsigned char * args = NULL;

//          fprintf( stderr,"[ws] remote property notify.\n" );
          XGetWindowProperty( wsDisplay,
                              Event->xproperty.window,
                              Event->xproperty.atom,
                              0,( 65536 / sizeof( long ) ),
                              False,XA_STRING,
                              &type,&format,&nitems,&bytesafter,
                              &args );
          if ( ( nitems )&&( wsWindowList[l]->RemoteHandler ) )
           {
            args[strlen( args ) - 1]=0;
            wsWindowList[l]->RemoteHandler( args );
            args[strlen( args ) - 1]=1;
            XFree( args );
           }
         }
        break;

  }
 XFlush( wsDisplay );
 XSync( wsDisplay,False );
 return !wsTrue;
// return True;
}

Bool wsDummyEvents( Display * display,XEvent * Event,XPointer arg )
{ return True; }

// mplTimerHandler(0); // handle timer event
void wsHandleEvents(){
 // handle pending events
 while ( XPending(wsDisplay) ){
   XNextEvent( wsDisplay,&wsEvent );
//   printf("### X event: %d  [%d]\n",wsEvent.type,delay);
   wsEvents( wsDisplay,&wsEvent,NULL );
 }
}

void wsMainLoop( void )
{
 int delay=20;
 fprintf( stderr,"[ws] init threads: %d\n",XInitThreads() );
 XSynchronize( wsDisplay,False );
 XLockDisplay( wsDisplay );
// XIfEvent( wsDisplay,&wsEvent,wsEvents,NULL );

#if 1

while(wsTrue){
 // handle pending events
 while ( XPending(wsDisplay) ){
   XNextEvent( wsDisplay,&wsEvent );
//   printf("### X event: %d  [%d]\n",wsEvent.type,delay);
   wsEvents( wsDisplay,&wsEvent,NULL );
   delay=0;
 }
 mplTimerHandler(0); // handle timer event
 usleep(delay*1000); // FIXME!
 if(delay<10*20) delay+=20; // pump up delay up to 0.2 sec (low activity)
}

#else

 while( wsTrue )
  {
   XIfEvent( wsDisplay,&wsEvent,wsDummyEvents,NULL );
   wsEvents( wsDisplay,&wsEvent,NULL );
  }
#endif

 XUnlockDisplay( wsDisplay );
}

// ----------------------------------------------------------------------------------------------
//    Switch to fullscreen.
// ----------------------------------------------------------------------------------------------
void wsFullScreen( wsTWindow * win )
{
 int decoration = 0;
 XUnmapWindow( wsDisplay,win->WindowID );
 win->SizeHint.flags=0;
 if ( win->isFullScreen )
  {
   win->X=win->OldX;
   win->Y=win->OldY;
   win->Width=win->OldWidth;
   win->Height=win->OldHeight;
   win->isFullScreen=False;
   decoration=win->Decorations;
   wsScreenSaverOn( wsDisplay );
  }
  else
   {
    win->OldX=win->X; win->OldY=win->Y;
    win->OldWidth=win->Width; win->OldHeight=win->Height;
    win->X=0; win->Y=0;
    win->Width=wsMaxX; win->Height=wsMaxY;
    win->isFullScreen=True;
    wsScreenSaverOff( wsDisplay );
   }

 win->SizeHint.flags|=PPosition | PSize;
 win->SizeHint.x=win->X;
 win->SizeHint.y=win->Y;
 win->SizeHint.width=win->Width;
 win->SizeHint.height=win->Height;
 if ( win->Property & wsMaxSize )
  {
   win->SizeHint.flags|=PMaxSize;
   win->SizeHint.max_width=win->Width;
   win->SizeHint.max_height=win->Height;
  }
 if ( win->Property & wsMinSize )
  {
   win->SizeHint.flags|=PMinSize;
   win->SizeHint.min_width=win->Width;
   win->SizeHint.min_height=win->Height;
  }
 XSetWMNormalHints( wsDisplay,win->WindowID,&win->SizeHint );

 XMoveResizeWindow( wsDisplay,win->WindowID,win->X,win->Y,win->Width,win->Height );
 wsWindowDecoration( win,decoration );
 XRaiseWindow( wsDisplay,win->WindowID );
 XMapWindow( wsDisplay,win->WindowID );
}

// ----------------------------------------------------------------------------------------------
//    Redraw screen.
// ----------------------------------------------------------------------------------------------
void wsPostRedisplay( wsTWindow * win )
{
 if ( win->ReDraw )
  {
   win->ReDraw( wsDisplay,win->WindowID );
   XFlush( wsDisplay );
  }
}

// ----------------------------------------------------------------------------------------------
//    Do Exit.
// ----------------------------------------------------------------------------------------------
void wsDoExit( void )
{ wsTrue=False; wsResizeWindow( wsWindowList[0],32,32 ); }

// ----------------------------------------------------------------------------------------------
//    Put 'Image' to window.
// ----------------------------------------------------------------------------------------------
void wsConvert( wsTWindow * win,unsigned char * Image,unsigned int Size )
{ if ( wsConvFunc ) wsConvFunc( Image,win->ImageData,win->xImage->width * win->xImage->height ); }

void wsPutImage( wsTWindow * win )
{
 if ( wsUseXShm )
  {
   XShmPutImage( wsDisplay,win->WindowID,win->wGC,win->xImage,
    0,0,
    ( win->Width - win->xImage->width ) / 2,( win->Height - win->xImage->height ) / 2,
    win->xImage->width,win->xImage->height,0 );
//    win->Width,win->Height,0 );
  }
  else
   {
    XPutImage( wsDisplay,win->WindowID,win->wGC,win->xImage,
    0,0,
    ( win->Width - win->xImage->width ) / 2,( win->Height - win->xImage->height ) / 2,
    win->xImage->width,win->xImage->height );
   }
}

// ----------------------------------------------------------------------------------------------
//    Move window to x, y.
// ----------------------------------------------------------------------------------------------
void wsMoveWindow( wsTWindow * win,int x, int y )
{
 switch ( x )
  {
   case -1: win->X=( wsMaxX / 2 ) - ( win->Width / 2 ); break;
   case -2: win->X=wsMaxX - win->Width; break;
   default: win->X=x; break;
  }
 switch ( y )
  {
   case -1: win->Y=( wsMaxY / 2 ) - ( win->Height / 2 ); break;
   case -2: win->Y=wsMaxY - win->Height; break;
   default: win->Y=y; break;
  }

 win->SizeHint.flags=PPosition;
 win->SizeHint.x=win->X;
 win->SizeHint.y=win->Y;
 XSetWMNormalHints( wsDisplay,win->WindowID,&win->SizeHint );

 XMoveWindow( wsDisplay,win->WindowID,win->X,win->Y );
 if ( win->ReSize ) win->ReSize( win->X,win->Y,win->Width,win->Height );
}

// ----------------------------------------------------------------------------------------------
//    Resize window to sx, sy.
// ----------------------------------------------------------------------------------------------
void wsResizeWindow( wsTWindow * win,int sx, int sy )
{
 win->Width=sx;
 win->Height=sy;

 win->SizeHint.flags=PSize;
 win->SizeHint.width=win->Width;
 win->SizeHint.height=win->Height;
 if ( win->Property & wsMinSize )
  {
   win->SizeHint.flags|=PMinSize;
   win->SizeHint.min_width=win->Width;
   win->SizeHint.min_height=win->Height;
  }
 if ( win->Property & wsMaxSize )
  {
   win->SizeHint.flags|=PMaxSize;
   win->SizeHint.max_width=win->Width;
   win->SizeHint.max_height=win->Height;
  }
 XSetWMNormalHints( wsDisplay,win->WindowID,&win->SizeHint );
 XResizeWindow( wsDisplay,win->WindowID,sx,sy );
 if ( win->ReSize ) win->ReSize( win->X,win->Y,win->Width,win->Height );
}

// ----------------------------------------------------------------------------------------------
//    Iconify window.
// ----------------------------------------------------------------------------------------------
void wsIconify( wsTWindow win )
{ XIconifyWindow( wsDisplay,win.WindowID,0 ); }

// ----------------------------------------------------------------------------------------------
//    Move top the window.
// ----------------------------------------------------------------------------------------------
void wsMoveTopWindow( wsTWindow * win )
//{ XRaiseWindow( wsDisplay,win->WindowID ); }
{ XUnmapWindow( wsDisplay,win->WindowID ); XMapWindow( wsDisplay,win->WindowID ); }

// ----------------------------------------------------------------------------------------------
//    Set window background to 'color'.
// ----------------------------------------------------------------------------------------------
void wsSetBackground( wsTWindow * win,int color )
{ XSetWindowBackground( wsDisplay,win->WindowID,color ); }

void wsSetBackgroundRGB( wsTWindow * win,int r,int g,int b )
{
 int color = 0;
 switch ( wsOutMask )
  {
   case wsRGB32:
   case wsRGB24: color=( r << 16 ) + ( g << 8 ) + b;  break;
   case wsBGR32:
   case wsBGR24: color=( b << 16 ) + ( g << 8 ) + r;  break;
   case wsRGB16: PACK_RGB16( r,g,b,color ); break;
   case wsBGR16: PACK_RGB16( b,g,r,color ); break;
   case wsRGB15: PACK_RGB15( r,g,b,color ); break;
   case wsBGR15: PACK_RGB15( b,g,r,color ); break;
  }
 XSetWindowBackground( wsDisplay,win->WindowID,color );
}

void wsSetForegroundRGB( wsTWindow * win,int r,int g,int b )
{
 int color = 0;
 switch ( wsOutMask )
  {
   case wsRGB32:
   case wsRGB24: color=( r << 16 ) + ( g << 8 ) + b;  break;
   case wsBGR32:
   case wsBGR24: color=( b << 16 ) + ( g << 8 ) + r;  break;
   case wsRGB16: PACK_RGB16( r,g,b,color ); break;
   case wsBGR16: PACK_RGB16( b,g,r,color ); break;
   case wsRGB15: PACK_RGB15( r,g,b,color ); break;
   case wsBGR15: PACK_RGB15( b,g,r,color ); break;
  }
 XSetForeground( wsDisplay,win->wGC,color );
// XSetWindowBackground( wsDisplay,win->WindowID,color );
}

// ----------------------------------------------------------------------------------------------
//    Draw string at x,y with fc ( foreground color ) and bc ( background color ).
// ----------------------------------------------------------------------------------------------
void wsDrawString( wsTWindow win,int x,int y,char * str,int fc,int bc )
{
 XSetForeground( wsDisplay,win.wGC,bc );
 XFillRectangle( wsDisplay,win.WindowID,win.wGC,x,y,
   XTextWidth( win.Font,str,strlen( str ) ) + 20,
   win.FontHeight + 2 );
 XSetForeground( wsDisplay,win.wGC,fc );
 XDrawString( wsDisplay,win.WindowID,win.wGC,x + 10,y + 13,str,strlen( str ) );
}

// ----------------------------------------------------------------------------------------------
//    Calculation string width.
// ----------------------------------------------------------------------------------------------
int wsTextWidth( wsTWindow win,char * str )
{ return XTextWidth( win.Font,str,strlen( str ) ) + 20; }

// ----------------------------------------------------------------------------------------------
//    Show / hide mouse cursor.
// ----------------------------------------------------------------------------------------------
void wsVisibleMouse( wsTWindow * win,int m )
{
 switch ( m )
  {
   case wsShowMouseCursor:
    if ( win->wsCursor != None )
     {
      XFreeCursor( wsDisplay,win->wsCursor );
      win->wsCursor=None;
     }
    XDefineCursor( wsDisplay,win->WindowID,0 );
    break;
   case wsHideMouseCursor:
    win->wsCursor=XCreatePixmapCursor( wsDisplay,win->wsCursorPixmap,win->wsCursorPixmap,&win->wsColor,&win->wsColor,0,0 );
    XDefineCursor( wsDisplay,win->WindowID,win->wsCursor );
    break;
  }
 XFlush( wsDisplay );
}

int wsGetDepthOnScreen( void )
{
 int                 bpp,ibpp;
 XImage            * mXImage;
 XWindowAttributes   attribs;

 mXImage=XGetImage( wsDisplay,wsRootWin,0,0,1,1,AllPlanes,ZPixmap );
 bpp=mXImage->bits_per_pixel;

 XGetWindowAttributes( wsDisplay,wsRootWin,&attribs );
 ibpp=attribs.depth;
 mXImage=XGetImage( wsDisplay,wsRootWin,0,0,1,1,AllPlanes,ZPixmap );
 bpp=mXImage->bits_per_pixel;
 if ( ( ibpp + 7 ) / 8 != ( bpp + 7 ) / 8 ) ibpp=bpp;
 wsDepthOnScreen=ibpp;
 wsRedMask=mXImage->red_mask;
 wsGreenMask=mXImage->green_mask;
 wsBlueMask=mXImage->blue_mask;
 XDestroyImage( mXImage );
 return ibpp;
}

void wsXDone( void )
{
// if ( wsSwitchedAnotherVideoMode ) wsChangeVideoMode( wsOldXResolution,wsOldYResolution );
// if ( wsUseDGA ) XF86DGADirectVideo( wsDisplay,wsScreen,0 );
 XCloseDisplay( wsDisplay );
}

void wsVisibleWindow( wsTWindow * win,int show )
{
 switch( show )
  {
   case wsShowWindow: XMapWindow( wsDisplay,win->WindowID ); break;
   case wsHideWindow: XUnmapWindow( wsDisplay,win->WindowID ); break;
  }
 XFlush( wsDisplay );
}

void wsDestroyImage( wsTWindow * win )
{
 if ( win->xImage )
  {
   XDestroyImage( win->xImage );
   if ( wsUseXShm )
    {
     XShmDetach( wsDisplay,&win->Shminfo );
     shmdt( win->Shminfo.shmaddr );
    }
  }
 win->xImage=NULL;
}

void wsCreateImage( wsTWindow * win,int Width,int Height )
{
 int CompletionType = -1;
 if ( wsUseXShm )
  {
   CompletionType=XShmGetEventBase( wsDisplay ) + ShmCompletion;
   win->xImage=XShmCreateImage( wsDisplay,win->VisualInfo.visual,
                   win->Attribs.depth,ZPixmap,NULL,&win->Shminfo,Width,Height );
   if ( win->xImage == NULL )
    {
     fprintf( stderr,"[ws] shared memory extension error.\n" );
     exit( 0 );
    }
//   #ifdef DEBUG
//    fprintf( stderr,"[ws] Screen depth: %d\n",win->xImage->bits_per_pixel );
//   #endif
   win->Shminfo.shmid=shmget( IPC_PRIVATE,win->xImage->bytes_per_line * win->xImage->height,IPC_CREAT|0777 );
   if ( win->Shminfo.shmid < 0 )
    {
     XDestroyImage( win->xImage );
     fprintf( stderr,"[ws] shared memory extension error.\n" );
     exit( 0 );
    }
   win->Shminfo.shmaddr=(char *)shmat( win->Shminfo.shmid,0,0 );

   if ( win->Shminfo.shmaddr == ((char *) -1) )
    {
     XDestroyImage( win->xImage );
     if ( win->Shminfo.shmaddr != ((char *) -1) ) shmdt( win->Shminfo.shmaddr );
     fprintf( stderr,"[ws] shared memory extension error.\n" );
     exit( 0 );
    }
   win->xImage->data=win->Shminfo.shmaddr;
   win->Shminfo.readOnly=0;
   XShmAttach( wsDisplay,&win->Shminfo );
   shmctl( win->Shminfo.shmid,IPC_RMID,0 );
  }
  else
   {
    win->xImage=XCreateImage( wsDisplay,win->VisualInfo.visual,win->Attribs.depth,
                              ZPixmap,0,0,Width,Height,
                              (wsDepthOnScreen == 3) ? 32 : wsDepthOnScreen,
                              0 );
    if ( ( win->xImage->data=malloc( win->xImage->bytes_per_line * win->xImage->height ) ) == NULL )
     {
      fprintf( stderr,"[ws] sorry, not enough memory for draw buffer.\n" );
      exit( 0 );
     }
   }
 win->ImageData=(unsigned char *)win->xImage->data;
 win->ImageDataw=(unsigned short int *)win->xImage->data;
 win->ImageDatadw=(unsigned int *)win->xImage->data;
}

void wsResizeImage( wsTWindow * win,int Width,int Height )
{ wsDestroyImage( win ); wsCreateImage( win,Width,Height ); }

int wsGetOutMask( void )
{
 if ( ( wsDepthOnScreen == 32 )&&( wsRedMask == 0xff0000 )&&( wsGreenMask == 0x00ff00 )&&( wsBlueMask == 0x0000ff ) ) return wsRGB32;
 if ( ( wsDepthOnScreen == 32 )&&( wsRedMask == 0x0000ff )&&( wsGreenMask == 0x00ff00 )&&( wsBlueMask == 0xff0000 ) ) return wsBGR32;
 if ( ( wsDepthOnScreen == 24 )&&( wsRedMask == 0xff0000 )&&( wsGreenMask == 0x00ff00 )&&( wsBlueMask == 0x0000ff ) ) return wsRGB24;
 if ( ( wsDepthOnScreen == 24 )&&( wsRedMask == 0x0000ff )&&( wsGreenMask == 0x00ff00 )&&( wsBlueMask == 0xff0000 ) ) return wsBGR24;
 if ( ( wsDepthOnScreen == 16 )&&( wsRedMask == 0xf800 )&&( wsGreenMask == 0x7e0 )&&( wsBlueMask ==   0x1f ) ) return wsRGB16;
 if ( ( wsDepthOnScreen == 16 )&&( wsRedMask ==   0x1f )&&( wsGreenMask == 0x7e0 )&&( wsBlueMask == 0xf800 ) ) return wsBGR16;
 if ( ( wsDepthOnScreen == 15 )&&( wsRedMask == 0x7c00 )&&( wsGreenMask == 0x3e0 )&&( wsBlueMask ==   0x1f ) ) return wsRGB15;
 if ( ( wsDepthOnScreen == 15 )&&( wsRedMask ==   0x1f )&&( wsGreenMask == 0x3e0 )&&( wsBlueMask == 0x7c00 ) ) return wsBGR15;
 return 0;
}

void wsSetTitle( wsTWindow * win,char * name )
{ XStoreName( wsDisplay,win->WindowID,name ); }

void wsSetMousePosition( wsTWindow * win,int x, int y )
{ XWarpPointer( wsDisplay,wsRootWin,win->WindowID,0,0,0,0,x,y ); }

static int dpms_disabled=0;
static int timeout_save=0;

void wsScreenSaverOn( Display *mDisplay )
{
 int nothing;
 if ( dpms_disabled )
  {
   if ( DPMSQueryExtension( mDisplay,&nothing,&nothing ) )
    {
     if ( !DPMSEnable( mDisplay ) ) fprintf( stderr,"DPMS not available ?\n" ); // restoring power saving settings
      else
       {
        // DPMS does not seem to be enabled unless we call DPMSInfo
        BOOL onoff;
        CARD16 state;
        DPMSInfo( mDisplay,&state,&onoff );
        if ( onoff ) fprintf( stderr,"Successfully enabled DPMS.\n" );
         else fprintf( stderr,"Could not enable DPMS.\n" );
       }
    }
  }

 if ( timeout_save )
  {
   int dummy, interval, prefer_blank, allow_exp;
   XGetScreenSaver( mDisplay,&dummy,&interval,&prefer_blank,&allow_exp );
   XSetScreenSaver( mDisplay,timeout_save,interval,prefer_blank,allow_exp );
   XGetScreenSaver( mDisplay,&timeout_save,&interval,&prefer_blank,&allow_exp );
  }
}

void wsScreenSaverOff( Display * mDisplay )
{
 int interval,prefer_blank,allow_exp,nothing;

 if ( DPMSQueryExtension( mDisplay,&nothing,&nothing ) )
  {
   BOOL onoff;
   CARD16 state;
   DPMSInfo( mDisplay,&state,&onoff );
   if ( onoff )
    {
      Status stat;
      fprintf( stderr,"Disabling DPMS.\n" );
      dpms_disabled=1;
      stat=DPMSDisable( mDisplay );  // monitor powersave off
      fprintf( stderr,"stat: %d.\n",stat );
   }
  }
 XGetScreenSaver( mDisplay,&timeout_save,&interval,&prefer_blank,&allow_exp );
 if ( timeout_save ) XSetScreenSaver( mDisplay,0,interval,prefer_blank,allow_exp ); // turning off screensaver
}

void wsSetShape( wsTWindow * win,char * data )
{
#ifdef HAVE_XSHAPE
 if ( !wsUseXShape ) return;
 if ( data )
  {
   win->Mask=XCreateBitmapFromData( wsDisplay,win->WindowID,data,win->Width,win->Height );
   XShapeCombineMask( wsDisplay,win->WindowID,ShapeBounding,0,0,win->Mask,ShapeSet );
   XFreePixmap( wsDisplay,win->Mask );
  }
  else XShapeCombineMask( wsDisplay,win->WindowID,ShapeBounding,0,0,None,ShapeSet );
#endif
}

#include "wsmkeys.h"

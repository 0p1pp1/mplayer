
#ifdef X11_FULLSCREEN

extern int vo_depthonscreen;
extern int vo_screenwidth;
extern int vo_screenheight;
extern int vo_dwidth;
extern int vo_dheight;

extern char *mDisplayName;
extern Display *mDisplay;
extern Window mRootWin;
extern int mScreen;
extern int mLocalDisplay;
extern int WinID;

int vo_init( void );
int vo_hidecursor ( Display* , Window );
void vo_x11_decoration( Display * vo_Display,Window w,int d );
void vo_x11_classhint( Display * display,Window window,char *name );
int vo_x11_check_events(Display *mydisplay);
#endif

extern Window    vo_window;
extern GC        vo_gc;

#ifdef HAVE_NEW_GUI
 extern void vo_setwindow( Window w,GC g );
 extern void vo_x11_putkey(int key);
#endif
#ifdef HAVE_GUI
 extern Display * vo_display;
#endif

void saver_off( Display * );
void saver_on( Display * );

#ifdef HAVE_XINERAMA
void vo_x11_xinerama_move(Display *dsp, Window w);
#endif

#ifdef HAVE_XF86VM
void vo_vm_switch(uint32_t, uint32_t, int*, int*);
void vo_vm_close(Display*);
#endif

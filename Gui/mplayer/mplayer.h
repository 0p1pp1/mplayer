
#ifndef __MYMPLAYERHANDLER
#define __MYMPLAYERHANDLER

extern int             mplSubRender;
extern int             mplMainRender;

extern unsigned char * mplDrawBuffer;
extern unsigned char * mplMenuDrawBuffer;
extern int             mainVisible;

extern int             mplMainAutoPlay;
extern int             mplMiddleMenu;

extern void mplInit( int argc,char* argv[], char *envp[], void* disp );
extern void mplEventHandling( int msg,float param );
extern void mplTimerHandler( void );

#endif


#ifndef __CFG_H
#define __CFG_H

extern int gtkEnableAudioEqualizer;

extern int    gtkVopPP;
extern int    gtkVopLAVC;
extern int    gtkVopFAME;

extern int    gtkAONorm;
extern int    gtkAOFakeSurround;
extern int    gtkAOExtraStereo;
extern float  gtkAOExtraStereoMul;
#ifdef USE_OSS_AUDIO
extern char * gtkAOOSSMixer;
extern char * gtkAOOSSMixerChannel;
extern char * gtkAOOSSDevice;
#endif
#if defined(HAVE_ALSA9) || defined (HAVE_ALSA1X)
extern char * gtkAOALSAMixer;
extern char * gtkAOALSAMixerChannel;
extern char * gtkAOALSADevice;
#endif
#ifdef HAVE_SDL
extern char * gtkAOSDLDriver;
#endif
#ifdef HAVE_DXR3
extern char * gtkDXR3Device;
#endif

extern int    gtkCacheOn;
extern int    gtkCacheSize;

extern int    gtkAutoSyncOn;
extern int    gtkAutoSync;

extern int    gtkSubDumpMPSub;
extern int    gtkSubDumpSrt;

extern char * gtkEquChannel1;
extern char * gtkEquChannel2;
extern char * gtkEquChannel3;
extern char * gtkEquChannel4;
extern char * gtkEquChannel5;
extern char * gtkEquChannel6;
extern int    gtkLoadFullscreen;
extern int    gtkShowVideoWindow;
extern int    gtkEnablePlayBar;

extern int    gui_save_pos;
extern int    gui_main_pos_x;
extern int    gui_main_pos_y;
extern int    gui_sub_pos_x;
extern int    gui_sub_pos_y;

extern int cfg_read( void );
extern int cfg_write( void );

#endif

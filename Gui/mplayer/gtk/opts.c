
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>

#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>

#include "../../../config.h"
#include "../../../help_mp.h"
#include "../../../mixer.h"
#include "../../../libao2/audio_out.h"
#include "../../../libvo/video_out.h"
#include "../../cfg.h"
#include "../../interface.h"
#include "../widgets.h"
#include "opts.h"
#include "fs.h"

       GtkWidget * Preferences;
static GtkWidget * AConfig;
static GtkWidget * VConfig;
//static GtkWidget * BLoadSubtitle;
static GtkWidget * BLoadFont;
static GtkWidget * BOk;
static GtkWidget * BCancel;

static GtkWidget * CLADrivers;
static GtkWidget * CLVDrivers;

//static GtkWidget * ESubtitleName;
       GtkWidget * prEFontName;
static GtkWidget * EVFM;

static GtkWidget * CBVFM;
static GtkWidget * CBAudioEqualizer;
//static GtkWidget * CBSurround;
static GtkWidget * CBExtraStereo;
static GtkWidget * CBNoSound;
static GtkWidget * CBNormalize;
static GtkWidget * CBDoubleBuffer;
static GtkWidget * CBDR;
static GtkWidget * CBFramedrop;
static GtkWidget * CBHFramedrop;
//static GtkWidget * CBFullScreen;
static GtkWidget * CBNonInterlaved;
static GtkWidget * CBIndex;
static GtkWidget * CBFlip;
static GtkWidget * CBNoAutoSub;
static GtkWidget * CBSubUnicode;
static GtkWidget * CBDumpMPSub;
static GtkWidget * CBDumpSrt;
static GtkWidget * CBPostprocess;

static GtkWidget * RBOSDNone;
static GtkWidget * RBOSDTandP;
static GtkWidget * RBOSDIndicator;

static GtkWidget * HSAudioDelay;
static GtkWidget * HSExtraStereoMul;
static GtkWidget * HSPanscan;
static GtkWidget * HSSubDelay;
static GtkWidget * HSSubPosition;
static GtkWidget * HSSubFPS;
static GtkWidget * HSPPQuality;

static GtkAdjustment * HSExtraStereoMuladj, * HSAudioDelayadj, * HSPanscanadj, * HSSubDelayadj;
static GtkAdjustment * HSSubPositionadj, * HSSubFPSadj, * HSPPQualityadj;

#ifndef HAVE_FREETYPE
static GtkWidget     * HSFontFactor;
static GtkAdjustment * HSFontFactoradj;
#else
static GtkWidget     * HSFontBlur, * HSFontOutLine, * HSFontTextScale, * HSFontOSDScale;
static GtkAdjustment * HSFontBluradj, * HSFontOutLineadj, * HSFontTextScaleadj, * HSFontOSDScaleadj;
static GtkWidget     * CBFontEncoding, * EFontEncoding;
static GtkWidget     * RBFontNoAutoScale, * BRFontAutoScaleWidth, * RBFontAutoScaleHeight, * RBFontAutoScaleDiagonal;
//static GtkWidget     * AutoScale;
#endif

static struct
{
 int    vfm;
 char * name;
} lVFM[] =
 { 
  { -1,MSGTR_PREFERENCES_None   },
  {  2,MSGTR_PREFERENCES_Codec1 },
  {  3,MSGTR_PREFERENCES_Codec2 },
  {  4,MSGTR_PREFERENCES_Codec3 },
  {  5,MSGTR_PREFERENCES_Codec4 },
  {  7,MSGTR_PREFERENCES_Codec5 },
  { 10,MSGTR_PREFERENCES_Codec6 },
  { 0,NULL }
 };

#ifdef HAVE_FREETYPE
static struct 
{
 char * name;
 char * comment;
} lEncoding[] =
 {
  { "unicode",     MSGTR_PREFERENCES_FontEncoding1 },
  { "iso-8859-1",  MSGTR_PREFERENCES_FontEncoding2 },
  { "iso-8859-15", MSGTR_PREFERENCES_FontEncoding3 },
  { "iso-8859-2",  MSGTR_PREFERENCES_FontEncoding4 },
  { "iso-8859-3",  MSGTR_PREFERENCES_FontEncoding5 },
  { "iso-8859-4",  MSGTR_PREFERENCES_FontEncoding6 },
  { "iso-8859-5",  MSGTR_PREFERENCES_FontEncoding7 },
  { "iso-8859-6",  MSGTR_PREFERENCES_FontEncoding8 },
  { "iso-8859-7",  MSGTR_PREFERENCES_FontEncoding9 },
  { "iso-8859-9",  MSGTR_PREFERENCES_FontEncoding10 },
  { "iso-8859-13", MSGTR_PREFERENCES_FontEncoding11 },
  { "iso-8859-14", MSGTR_PREFERENCES_FontEncoding12 },
  { "iso-8859-8",  MSGTR_PREFERENCES_FontEncoding13 },
  { "koi8-r",      MSGTR_PREFERENCES_FontEncoding14 },
  { "koi8-u/ru",   MSGTR_PREFERENCES_FontEncoding15 },
  { "cp936",       MSGTR_PREFERENCES_FontEncoding16 },
  { "big5",        MSGTR_PREFERENCES_FontEncoding17 },
  { "shift-jis",   MSGTR_PREFERENCES_FontEncoding18 },
  { "cp949",       MSGTR_PREFERENCES_FontEncoding19 },
  { "cp874",       MSGTR_PREFERENCES_FontEncoding20 },
  { NULL,NULL } 
 };
char * lCEncoding = NULL;
#endif
	    
       int    gtkVPreferences = 0;
static int    old_audio_driver = 0;
static char * ao_driver[3];
static char * vo_driver[3];
static int    old_video_driver = 0;

#ifdef USE_OSS_AUDIO
 void ShowOSSConfig( void );
 void HideOSSConfig( void );
#endif
#ifdef HAVE_DXR3
 void ShowDXR3Config( void );
 void HideDXR3Config( void );
#endif
static gboolean prHScaler( GtkWidget * widget,GdkEventMotion  * event,gpointer user_data );
static void prToggled( GtkToggleButton * togglebutton,gpointer user_data );
static void prCListRow( GtkCList * clist,gint row,gint column,GdkEvent * event,gpointer user_data );
#ifdef HAVE_FREETYPE
static void prEntry( GtkContainer * container,GtkWidget * widget,gpointer user_data );
#endif

extern int    muted;

void ShowPreferences( void )
{
 if ( gtkVPreferences ) gtkActive( Preferences );
   else Preferences=create_Preferences();

// -- 1. page 
 gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( CBAudioEqualizer ),gtkEnableAudioEqualizer );
 gtkAONoSound=muted;
 gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( CBNoSound ),gtkAONoSound );
#if 0
 gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( CBSurround ),gtkAOSurround );
#endif
 gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( CBExtraStereo ),gtkAOExtraStereo );
 gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( CBNormalize ),gtkAONorm );
 gtk_adjustment_set_value( HSExtraStereoMuladj,gtkAOExtraStereoMul );
 {
  int    i = 0;
  char * tmp[3]; tmp[2]="";
  old_audio_driver=0;
  while ( audio_out_drivers[i] )
   {
    const ao_info_t *info = audio_out_drivers[i++]->info;
    if ( !strcmp( info->short_name,"plugin" ) ) continue;
    if ( !gstrcmp( audio_driver,(char *)info->short_name ) ) old_audio_driver=i - 1;
    tmp[0]=(char *)info->short_name; tmp[1]=(char *)info->name; gtk_clist_append( GTK_CLIST( CLADrivers ),tmp );
   }
  gtk_clist_select_row( GTK_CLIST( CLADrivers ),old_audio_driver,0 );
  gtk_clist_get_text( GTK_CLIST( CLADrivers ),old_audio_driver,0,(char **)&ao_driver );
  gtk_widget_set_sensitive( AConfig,FALSE );
#ifdef USE_OSS_AUDIO
  if ( !gstrcmp( ao_driver[0],"oss" ) ) gtk_widget_set_sensitive( AConfig,TRUE );
#endif
 }

// -- 2. page
 gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( CBDoubleBuffer ),vo_doublebuffering );
 gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( CBDR ),vo_directrendering );

 gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( CBFramedrop ),FALSE );
 gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( CBHFramedrop ),FALSE );
 switch ( frame_dropping )
  {
   case 2: gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( CBHFramedrop ),TRUE );
   case 1: gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( CBFramedrop ),TRUE );
  }

 gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( CBFlip ),flip );
 gtk_adjustment_set_value( HSPanscanadj,vo_panscan );
 {
  int i = 0, c = 0;
  char * tmp[3]; tmp[2]="";
  old_video_driver=0; 
  while ( video_out_drivers[i] )
   if ( video_out_drivers[i++]->control( VOCTRL_GUISUPPORT,NULL ) == VO_TRUE )
    { 
     const vo_info_t *info = video_out_drivers[i - 1]->get_info();
     if ( !gstrcmp( video_driver,(char *)info->short_name ) ) old_video_driver=c; c++;
     tmp[0]=(char *)info->short_name; tmp[1]=(char *)info->name; gtk_clist_append( GTK_CLIST( CLVDrivers ),tmp );
    }
  gtk_clist_select_row( GTK_CLIST( CLVDrivers ),old_video_driver,0 );
  gtk_clist_get_text( GTK_CLIST( CLVDrivers ),old_video_driver,0,(char **)&vo_driver );
  gtk_widget_set_sensitive( VConfig,FALSE );
#ifdef HAVE_DXR3
  if ( !gstrcmp( vo_driver[0],"dxr3" ) ) gtk_widget_set_sensitive( VConfig,TRUE );
#endif
 }

// -- 3. page
 gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( CBNoAutoSub ),!sub_auto );
 gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( CBDumpMPSub ),gtkSubDumpMPSub );
 gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( CBDumpSrt ),gtkSubDumpSrt );
 gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( CBSubUnicode ),sub_unicode );
 gtk_adjustment_set_value( HSSubDelayadj,sub_delay );
 gtk_adjustment_set_value( HSSubFPSadj,sub_fps );
 gtk_adjustment_set_value( HSSubPositionadj,sub_pos );
 switch ( osd_level )
  {
   case 0: gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( RBOSDNone ),TRUE ); break;
   case 1: gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( RBOSDIndicator ),TRUE ); break;
   case 2: gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( RBOSDTandP ),TRUE ); break;
  }
#if 0
 if ( guiIntfStruct.Subtitlename ) gtk_entry_set_text( GTK_ENTRY( ESubtitleName ),guiIntfStruct.Subtitlename );
#endif
 // font ...
 if ( font_name ) gtk_entry_set_text( GTK_ENTRY( prEFontName ),font_name );
#ifndef HAVE_FREETYPE
 gtk_adjustment_set_value( HSFontFactoradj,font_factor );
#else
 gtk_adjustment_set_value( HSFontBluradj,subtitle_font_radius );
 gtk_adjustment_set_value( HSFontOutLineadj,subtitle_font_thickness );
 gtk_adjustment_set_value( HSFontTextScaleadj,text_font_scale_factor );
 gtk_adjustment_set_value( HSFontOSDScaleadj,osd_font_scale_factor );
 if ( subtitle_font_encoding )
  {
   int i;
   for ( i=0;lEncoding[i].name;i++ ) 
    if ( !gstrcmp( subtitle_font_encoding,lEncoding[i].name ) ) break;
   if ( lEncoding[i].name ) lCEncoding=lEncoding[i].comment;
   gtk_entry_set_text( GTK_ENTRY( EFontEncoding ),lCEncoding );
  }
 switch ( subtitle_autoscale )
  {
   case 0: gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( RBFontNoAutoScale ),TRUE ); break;
   case 1: gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( BRFontAutoScaleWidth ),TRUE ); break;
   case 2: gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( RBFontAutoScaleHeight ),TRUE ); break;
   case 3: gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( RBFontAutoScaleDiagonal ),TRUE ); break;
  }
#endif

// -- 4. page
 gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( CBNonInterlaved ),force_ni );
 gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( CBIndex ),index_mode );
 gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( CBPostprocess ),gtkVopPP );
 gtk_adjustment_set_value( HSPPQualityadj,auto_quality );
 {
  int i = 0;
  for ( i=0;i<7;i++ )
    if ( lVFM[i].vfm == video_family ) break;
  gtk_entry_set_text( GTK_ENTRY( EVFM ),lVFM[i].name );
 }

// -- disables
#ifndef USE_SUB
 gtk_widget_set_sensitive( AConfig,FALSE );
 gtk_widget_set_sensitive( CBNoAutoSub,FALSE );
 gtk_widget_set_sensitive( CBSubUnicode,FALSE );
 gtk_widget_set_sensitive( CBDumpMPSub,FALSE );
 gtk_widget_set_sensitive( CBDumpSrt,FALSE );
 gtk_widget_set_sensitive( HSSubDelay,FALSE );
 gtk_widget_set_sensitive( HSSubPosition,FALSE );
 gtk_widget_set_sensitive( HSSubFPS,FALSE );
#endif

#ifndef USE_OSD
 gtk_widget_set_sensitive( RBOSDNone,FALSE );
 gtk_widget_set_sensitive( RBOSDTandP,FALSE );
 gtk_widget_set_sensitive( RBOSDIndicator,FALSE );
#endif

#if !defined( USE_OSD ) && !defined( USE_SUB )
 gtk_widget_set_sensitive( HSFontFactor,FALSE );
 gtk_widget_set_sensitive( prEFontName,FALSE );
 gtk_widget_set_sensitive( BLoadFont,FALSE );
#endif

// -- signals
 gtk_signal_connect( GTK_OBJECT( CBExtraStereo ),"toggled",GTK_SIGNAL_FUNC( prToggled ),(void*)0 );
 gtk_signal_connect( GTK_OBJECT( CBNormalize ),"toggled",GTK_SIGNAL_FUNC( prToggled ),(void*)1 );
 gtk_signal_connect( GTK_OBJECT( CBAudioEqualizer ),"toggled",GTK_SIGNAL_FUNC( prToggled ),(void*)2 );
 gtk_signal_connect( GTK_OBJECT( CBNoSound ),"toggled",GTK_SIGNAL_FUNC( prToggled ),(void*)3 );
#ifdef HAVE_FREETYPE
 gtk_signal_connect( GTK_OBJECT( RBFontNoAutoScale ),"toggled",GTK_SIGNAL_FUNC( prToggled ),(void*)4 );
 gtk_signal_connect( GTK_OBJECT( BRFontAutoScaleWidth ),"toggled",GTK_SIGNAL_FUNC( prToggled ),(void*)5 );
 gtk_signal_connect( GTK_OBJECT( RBFontAutoScaleHeight ),"toggled",GTK_SIGNAL_FUNC( prToggled ),(void*)6 );
 gtk_signal_connect( GTK_OBJECT( RBFontAutoScaleDiagonal ),"toggled",GTK_SIGNAL_FUNC( prToggled ),(void*)7 );
#endif

 gtk_signal_connect( GTK_OBJECT( HSExtraStereoMul ),"motion_notify_event",GTK_SIGNAL_FUNC( prHScaler ),(void*)0 );
 gtk_signal_connect( GTK_OBJECT( HSAudioDelay ),"motion_notify_event",GTK_SIGNAL_FUNC( prHScaler ),(void*)1 );
 gtk_signal_connect( GTK_OBJECT( HSPanscan ),"motion_notify_event",GTK_SIGNAL_FUNC( prHScaler ),(void*)2 );
 gtk_signal_connect( GTK_OBJECT( HSSubDelay ),"motion_notify_event",GTK_SIGNAL_FUNC( prHScaler ),(void*)3 );
 gtk_signal_connect( GTK_OBJECT( HSSubPosition ),"motion_notify_event",GTK_SIGNAL_FUNC( prHScaler ),(void*)4 );
#ifndef HAVE_FREETYPE
 gtk_signal_connect( GTK_OBJECT( HSFontFactor ),"motion_notify_event",GTK_SIGNAL_FUNC( prHScaler ),(void*)5 );
#else
 gtk_signal_connect( GTK_OBJECT( HSFontBlur ),"motion_notify_event",GTK_SIGNAL_FUNC( prHScaler ),(void*)6 );
 gtk_signal_connect( GTK_OBJECT( HSFontOutLine ),"motion_notify_event",GTK_SIGNAL_FUNC( prHScaler ),(void*)7 );
 gtk_signal_connect( GTK_OBJECT( HSFontTextScale ),"motion_notify_event",GTK_SIGNAL_FUNC( prHScaler ),(void*)8 );
 gtk_signal_connect( GTK_OBJECT( HSFontOSDScale ),"motion_notify_event",GTK_SIGNAL_FUNC( prHScaler ),(void*)9 );
 gtk_signal_connect( GTK_OBJECT( EFontEncoding ),"changed",GTK_SIGNAL_FUNC( prEntry ),NULL );
#endif
 
 gtk_signal_connect( GTK_OBJECT( CLADrivers ),"select_row",GTK_SIGNAL_FUNC( prCListRow ),(void*)0 );
 gtk_signal_connect( GTK_OBJECT( CLVDrivers ),"select_row",GTK_SIGNAL_FUNC( prCListRow ),(void*)1 );

 gtkVPreferences=1;
 gtk_widget_show( Preferences );
 gtkSetLayer( Preferences );
 gtkMessageBox( GTK_MB_WARNING,MSGTR_PREFERENCES_Message );
}

void HidePreferences( void )
{
 if ( !gtkVPreferences ) return;
 gtkVPreferences=0;
 gtk_widget_hide( Preferences ); gtk_widget_destroy( Preferences );
#ifdef USE_OSS_AUDIO
 HideOSSConfig();
#endif
#ifdef HAVE_DXR3
 HideDXR3Config();
#endif
}

#ifdef HAVE_FREETYPE
static void prEntry( GtkContainer * container,GtkWidget * widget,gpointer user_data )
{
 char * comment = gtk_entry_get_text( GTK_ENTRY( EFontEncoding ) );
 int    i;
 
 for ( i=0;lEncoding[i].name;i++ )
  if ( !gstrcmp( lEncoding[i].comment,comment ) ) break;
  
 if ( lEncoding[i].comment ) gtkSet( gtkSetFontEncoding,0,lEncoding[i].name );
}
#endif

static void prDestroy( GtkObject * object,gpointer user_data )
{ HidePreferences(); }

static void prShow( GtkWidget * widget,gpointer user_data )
{ gtkVPreferences=(int)user_data; }

#define bAConfig   0
#define bVconfig   1
#define bOk	   2
#define bCancel    3
#define bLSubtitle 4
#define bLFont     5

void prButton( GtkButton * button,gpointer user_data )
{
 switch ( (int)user_data )
  {
   case bOk:
	// -- 1. page
        gtkEnableAudioEqualizer=gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( CBAudioEqualizer ) );
	gtkAOExtraStereo=gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( CBExtraStereo ) );
	gtkAONorm=gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( CBNormalize ) );
	gtkAONoSound=gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( CBNoSound ) );
	gtkSet( gtkSetExtraStereo,HSExtraStereoMuladj->value,NULL );
	gtkSet( gtkSetAudioDelay,HSAudioDelayadj->value,NULL );
        gfree( (void **)&audio_driver );
	audio_driver=gstrdup( ao_driver[0] );
	gfree( (void **)&video_driver );
	video_driver=gstrdup( vo_driver[0] );

	// -- 2. page
	vo_doublebuffering=gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( CBDoubleBuffer ) );
	vo_directrendering=gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( CBDR ) );

        frame_dropping=0;
	if ( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( CBFramedrop ) ) == TRUE ) frame_dropping=1;
	if ( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( CBHFramedrop ) ) == TRUE ) frame_dropping=2;

	flip=gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( CBFlip ) );
	
	// -- 3. page
	gtkSet( gtkSetSubAuto,!gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( CBNoAutoSub ) ),NULL );
	gtkSubDumpMPSub=gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( CBDumpMPSub ) );
	gtkSubDumpSrt=gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( CBDumpSrt ) );
	sub_unicode=gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( CBSubUnicode ) );
	gtkSet( gtkSetSubDelay,HSSubDelayadj->value,NULL );
	gtkSet( gtkSetSubFPS,HSSubFPSadj->value,NULL );
	gtkSet( gtkSetSubPos,HSSubPositionadj->value,NULL );
	if ( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( RBOSDNone ) ) ) osd_level=0;
	if ( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( RBOSDIndicator ) ) ) osd_level=1;
	if ( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( RBOSDTandP ) ) ) osd_level=2;

        // font ...	
	guiSetFilename( font_name,gtk_entry_get_text( GTK_ENTRY( prEFontName ) ) );
#ifndef HAVE_FREETYPE
	gtkSet( gtkSetFontFactor,HSFontFactoradj->value,NULL );
#else
	gtkSet( gtkSetFontBlur,HSFontBluradj->value,NULL );
	gtkSet( gtkSetFontOutLine,HSFontOutLineadj->value,NULL );
	gtkSet( gtkSetFontTextScale,HSFontTextScaleadj->value,NULL );
	gtkSet( gtkSetFontOSDScale,HSFontOSDScaleadj->value,NULL );
	if ( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( RBFontNoAutoScale ) ) ) gtkSet( gtkSetFontAutoScale,0,NULL );
	if ( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( BRFontAutoScaleWidth ) ) ) gtkSet( gtkSetFontAutoScale,1,NULL );
	if ( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( RBFontAutoScaleHeight ) ) ) gtkSet( gtkSetFontAutoScale,2,NULL );
	if ( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( RBFontAutoScaleDiagonal ) ) ) gtkSet( gtkSetFontAutoScale,3,NULL );
#endif

	// -- 4. page
	force_ni=gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( CBNonInterlaved ) );
	index_mode=gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( CBIndex ) );
	gtkVopPP=gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( CBPostprocess ) ); 
	gtkSet( gtkSetAutoq,HSPPQualityadj->value,NULL );
	{
	 int i;
	 char * tmp = gtk_entry_get_text( GTK_ENTRY( EVFM ) );
	 video_family=-1;
	 for ( i=0;i<7;i++ )
	  if ( !strcmp( tmp,lVFM[i].name ) ) { video_family=lVFM[i].vfm; break; }
	}

   case bCancel:
	HidePreferences();
	break;
   case bAConfig:
        gtk_widget_set_sensitive( AConfig,FALSE );
#ifdef USE_OSS_AUDIO
        if ( !strcmp( ao_driver[0],"oss" ) ) { ShowOSSConfig(); gtk_widget_set_sensitive( AConfig,TRUE ); }
#endif
	break;
   case bVconfig:
        gtk_widget_set_sensitive( VConfig,FALSE );
#ifdef HAVE_DXR3
	if ( !gstrcmp( vo_driver[0],"dxr3" ) ) { ShowDXR3Config(); gtk_widget_set_sensitive( VConfig,TRUE ); }
#endif
	break;
#if 0
   case bLSubtitle:
	break;
#endif
   case bLFont:
        ShowFileSelect( fsFontSelector,FALSE );
	gtkSetLayer( fsFileSelect );
	break;
  }
}

static gboolean prHScaler( GtkWidget * widget,GdkEventMotion  * event,gpointer user_data )
{
 switch ( (int)user_data )
  {
   case 0: // extra stereo coefficient
	if ( !guiIntfStruct.Playing ) break;
	gtkSet( gtkSetExtraStereo,HSExtraStereoMuladj->value,NULL );
	break;
   case 1: // audio delay
	gtkSet( gtkSetAudioDelay,HSAudioDelayadj->value,NULL );
	break;
   case 2: // panscan
        gtkSet( gtkSetPanscan,HSPanscanadj->value,NULL );
	break;
   case 3: // sub delay
        gtkSet( gtkSetSubDelay,HSSubDelayadj->value,NULL );
	break;
   case 4: // sub position
        gtkSet( gtkSetSubPos,HSSubPositionadj->value,NULL );
	break;
   case 5: // font factor
#ifndef HAVE_FREETYPE
        gtkSet( gtkSetFontFactor,HSFontFactoradj->value,NULL );
	break;
#else
   case 6: // font blur
	gtkSet( gtkSetFontBlur,HSFontBluradj->value,NULL );
        break;
   case 7: // font outline
        gtkSet( gtkSetFontOutLine,HSFontOutLineadj->value,NULL );
        break;
   case 8: // text scale
        gtkSet( gtkSetFontTextScale,HSFontTextScaleadj->value,NULL );
	break;
   case 9: // osd scale
        gtkSet( gtkSetFontOSDScale,HSFontOSDScaleadj->value,NULL );
	break;
#endif
  }
 return FALSE;
}

static void prToggled( GtkToggleButton * togglebutton,gpointer user_data )
{
 switch ( (int)user_data )
  {
   case 0: // extra stereo coefficient
	if ( guiIntfStruct.Playing ) 
	gtk_widget_set_sensitive( HSExtraStereoMul,gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( CBExtraStereo ) ) );
	break;
//   case 1: // normalize
//   case 2: // equalizer
//	if ( guiIntfStruct.Playing ) gtkMessageBox( GTK_MB_WARNING,"Please remember, this function need restart the playing." );
//	break;
   case 3: // no sound
        mixer_mute();
	break;
   case 4:
   case 5:
   case 6:
   case 7:
	gtkSet( gtkSetFontAutoScale,(float)((int)user_data - 4 ),NULL );
	break;
  }
}

static void prCListRow( GtkCList * clist,gint row,gint column,GdkEvent * event,gpointer user_data )
{
 switch ( (int)user_data )
  {
   case 0: // audio driver 
	gtk_clist_get_text( GTK_CLIST( CLADrivers ),row,0,(char **)&ao_driver ); 
	gtk_widget_set_sensitive( AConfig,FALSE );
#ifdef USE_OSS_AUDIO
	if ( !strcmp( ao_driver[0],"oss" ) ) gtk_widget_set_sensitive( AConfig,TRUE );
#endif
	break;
   case 1: // video driver 
	gtk_clist_get_text( GTK_CLIST( CLVDrivers ),row,0,(char **)&vo_driver ); 
	gtk_widget_set_sensitive( VConfig,FALSE );
#ifdef HAVE_DXR3
	if ( !gstrcmp( vo_driver[0],"dxr3" ) ) gtk_widget_set_sensitive( VConfig,TRUE );
#endif
	break;
  } 
}

GtkWidget * create_Preferences( void )
{
  GtkWidget * frame1;
  GtkWidget * frame2;
  GtkWidget * frame3;
  GtkWidget * frame4;
  GtkWidget * vbox1;
  GtkWidget * notebook1;
  GtkWidget * hbox1;
  GtkWidget * frame9;
  GtkWidget * vbox2;
  GtkWidget * scrolledwindow3;
  GtkWidget * label8;
  GtkWidget * hbuttonbox2;
  GtkWidget * frame10;
  GtkWidget * vbox3;
  GtkWidget * hseparator2;
  GtkWidget * hbox8;
  GtkWidget * label17;
  GtkWidget * label1;
  GtkWidget * hbox2;
  GtkWidget * frame7;
  GtkWidget * vbox4;
  GtkWidget * scrolledwindow2;
  GtkWidget * label7;
  GtkWidget * hbuttonbox3;
  GtkWidget * frame8;
  GtkWidget * vbox5;
  GtkWidget * hbox3;
  GtkWidget * label9;
  GtkWidget * label2;
  GtkWidget * vbox6;
  GtkWidget * frame5;
  GtkWidget * vbox600;
  GSList    * OSD_group = NULL;
  GSList    * Font_group = NULL;
  GList	    * CBVFM_items = NULL;
  GList     * CBFontEncoding_items = NULL;
  GtkWidget * frame6;
  GtkWidget * vbox7;
  GtkWidget * vbox8;
  GtkWidget * table1;
  GtkWidget * label11;
  GtkWidget * label12;
  GtkWidget * label13;
  GtkWidget * vbox9;
  GtkWidget * frame12;
  GtkWidget * vbox603;
  GtkWidget * hbox6;
  GtkWidget * label15;
  GtkWidget * hbuttonbox5;
#ifndef HAVE_FREETYPE
  GtkWidget * hbox7;
#endif
  GtkWidget * label16;
  GtkWidget * label3;
  GtkWidget * vbox601;
  GtkWidget * frame11;
  GtkWidget * vbox602;
  GtkWidget * hbox5;
  GtkWidget * label14;
  GtkWidget * label4;
  GtkWidget * hseparator1;
  GtkWidget * hbuttonbox1;
  GtkWidget * frame;
  GtkAccelGroup * accel_group;

  accel_group=gtk_accel_group_new();

  Preferences=gtk_window_new( GTK_WINDOW_DIALOG );
  gtk_widget_set_name( Preferences,"Preferences" );
  gtk_object_set_data( GTK_OBJECT( Preferences ),"Preferences",Preferences );
#ifndef HAVE_FREETYPE
  gtk_widget_set_usize( Preferences,512,400 );
#else
  gtk_widget_set_usize( Preferences,612,565 );
#endif
  gtk_window_set_title( GTK_WINDOW( Preferences ),MSGTR_Preferences );
  gtk_window_set_position( GTK_WINDOW( Preferences ),GTK_WIN_POS_CENTER );
  gtk_window_set_policy( GTK_WINDOW( Preferences ),FALSE,FALSE,FALSE );
  gtk_window_set_wmclass( GTK_WINDOW( Preferences ),"Preferences","MPlayer" );
  
  gtk_widget_realize( Preferences );
  gtkAddIcon( Preferences );

  frame1=gtk_frame_new( NULL );
  gtk_widget_set_name( frame1,"frame1" );
  gtk_widget_ref( frame1 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"frame1",frame1,     (GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame1 );
  gtk_container_add( GTK_CONTAINER( Preferences ),frame1 );
  gtk_container_set_border_width( GTK_CONTAINER( frame1 ),1 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame1 ),GTK_SHADOW_IN );

  frame2=gtk_frame_new( NULL );
  gtk_widget_set_name( frame2,"frame2" );
  gtk_widget_ref( frame2 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"frame2",frame2,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame2 );
  gtk_container_add( GTK_CONTAINER( frame1 ),frame2 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame2 ),GTK_SHADOW_NONE );

  frame3=gtk_frame_new( NULL );
  gtk_widget_set_name( frame3,"frame3" );
  gtk_widget_ref( frame3 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"frame3",frame3,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame3 );
  gtk_container_add( GTK_CONTAINER( frame2 ),frame3 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame3 ),GTK_SHADOW_ETCHED_OUT );

  frame4=gtk_frame_new( NULL );
  gtk_widget_set_name( frame4,"frame4" );
  gtk_widget_ref( frame4 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"frame4",frame4,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame4 );
  gtk_container_add( GTK_CONTAINER( frame3 ),frame4 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame4 ),GTK_SHADOW_NONE );

  vbox1=gtk_vbox_new( FALSE,0 );
  gtk_widget_set_name( vbox1,"vbox1" );
  gtk_widget_ref( vbox1 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"vbox1",vbox1,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( vbox1 );
  gtk_container_add( GTK_CONTAINER( frame4 ),vbox1 );

  notebook1=gtk_notebook_new();
  gtk_widget_set_name( notebook1,"notebook1" );
  gtk_widget_ref( notebook1 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"notebook1",notebook1,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( notebook1 );
  gtk_box_pack_start( GTK_BOX( vbox1 ),notebook1,TRUE,TRUE,0 );

  hbox1=gtk_hbox_new( FALSE,0 );
  gtk_widget_set_name( hbox1,"hbox1" );
  gtk_widget_ref( hbox1 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"hbox1",hbox1,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( hbox1 );
  gtk_container_add( GTK_CONTAINER( notebook1 ),hbox1 );

  frame9=gtk_frame_new( NULL );
  gtk_widget_set_name( frame9,"frame9" );
  gtk_widget_ref( frame9 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"frame9",frame9,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame9 );
  gtk_box_pack_start( GTK_BOX( hbox1 ),frame9,TRUE,TRUE,0 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame9 ),GTK_SHADOW_ETCHED_OUT );

  frame=gtk_frame_new( NULL );
  gtk_widget_set_name( frame,"frame" );
  gtk_widget_ref( frame );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"frame",frame,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame );
  gtk_container_add( GTK_CONTAINER( frame9 ),frame );
  gtk_container_set_border_width( GTK_CONTAINER( frame ),0 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame ),GTK_SHADOW_NONE );

  vbox2=gtk_vbox_new( FALSE,0 );
  gtk_widget_set_name( vbox2,"vbox2" );
  gtk_widget_ref( vbox2 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"vbox2",vbox2,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( vbox2 );
  gtk_container_add( GTK_CONTAINER( frame ),vbox2 );

  scrolledwindow3=gtk_scrolled_window_new( NULL,NULL );
  gtk_widget_set_name( scrolledwindow3,"scrolledwindow3" );
  gtk_widget_ref( scrolledwindow3 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"scrolledwindow3",scrolledwindow3,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( scrolledwindow3 );
  gtk_box_pack_start( GTK_BOX( vbox2 ),scrolledwindow3,TRUE,TRUE,0 );
  gtk_scrolled_window_set_policy( GTK_SCROLLED_WINDOW( scrolledwindow3 ),GTK_POLICY_AUTOMATIC,GTK_POLICY_AUTOMATIC );

  CLADrivers=gtk_clist_new( 2 );
  gtk_widget_set_name( CLADrivers,"CLADrivers" );
  gtk_widget_ref( CLADrivers );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"CLADrivers",CLADrivers,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( CLADrivers );
  gtk_container_add( GTK_CONTAINER( scrolledwindow3 ),CLADrivers );
  gtk_clist_set_column_width( GTK_CLIST( CLADrivers ),0,50 );
  gtk_clist_column_titles_show( GTK_CLIST( CLADrivers ) );
  gtk_clist_set_shadow_type( GTK_CLIST( CLADrivers ),GTK_SHADOW_NONE );
  gtk_widget_set_usize( CLADrivers,200,-2 );

  label8=gtk_label_new( MSGTR_PREFERENCES_AvailableDrivers );
  gtk_widget_set_name( label8,"label8" );
  gtk_widget_ref( label8 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"label8",label8,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( label8 );
  gtk_clist_set_column_widget( GTK_CLIST( CLADrivers ),0,label8 );
  gtk_misc_set_alignment( GTK_MISC( label8 ),0,0.5 );
  gtk_misc_set_padding( GTK_MISC( label8 ),4,0 );

  hbuttonbox2=gtk_hbutton_box_new();
  gtk_widget_set_name( hbuttonbox2,"hbuttonbox2" );
  gtk_widget_ref( hbuttonbox2 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"hbuttonbox2",hbuttonbox2,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( hbuttonbox2 );
  gtk_box_pack_start( GTK_BOX( vbox2 ),hbuttonbox2,FALSE,FALSE,0 );
  gtk_button_box_set_child_size( GTK_BUTTON_BOX( hbuttonbox2 ),85,20 );

  AConfig=gtk_button_new_with_label( MSGTR_ConfigDriver );
  gtk_widget_set_name( AConfig,"AConfig" );
  gtk_widget_ref( AConfig );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"AConfig",AConfig,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( AConfig );
  gtk_container_add( GTK_CONTAINER( hbuttonbox2 ),AConfig );

  frame10=gtk_frame_new( NULL );
  gtk_widget_set_name( frame10,"frame10" );
  gtk_widget_ref( frame10 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"frame10",frame10,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame10 );
  gtk_box_pack_start( GTK_BOX( hbox1 ),frame10,TRUE,TRUE,0 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame10 ),GTK_SHADOW_ETCHED_OUT );

  frame=gtk_frame_new( NULL );
  gtk_widget_set_name( frame,"frame" );
  gtk_widget_ref( frame );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"frame",frame,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame );
  gtk_container_add( GTK_CONTAINER( frame10 ),frame );
  gtk_container_set_border_width( GTK_CONTAINER( frame ),0 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame ),GTK_SHADOW_NONE );

  vbox3=gtk_vbox_new( FALSE,0 );
  gtk_widget_set_name( vbox3,"vbox3" );
  gtk_widget_ref( vbox3 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"vbox3",vbox3,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( vbox3 );
  gtk_container_add( GTK_CONTAINER( frame ),vbox3 );
  gtk_widget_set_usize( vbox3,250,-2 );

  CBNoSound=gtk_check_button_new_with_label( MSGTR_PREFERENCES_DoNotPlaySound );
  gtk_widget_set_name( CBNoSound,"CBNoSound" );
  gtk_widget_ref( CBNoSound );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"CBNoSound",CBNoSound,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( CBNoSound );
  gtk_box_pack_start( GTK_BOX( vbox3 ),CBNoSound,FALSE,FALSE,0 );

  hseparator2=gtk_hseparator_new();
  gtk_widget_set_name( hseparator2,"hseparator2" );
  gtk_widget_ref( hseparator2 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"hseparator2",hseparator2,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( hseparator2 );
  gtk_box_pack_start( GTK_BOX( vbox3 ),hseparator2,FALSE,FALSE,0 );
  gtk_widget_set_usize( hseparator2,-2,4 );

  CBNormalize=gtk_check_button_new_with_label( MSGTR_PREFERENCES_NormalizeSound );
  gtk_widget_set_name( CBNormalize,"CBNormalize" );
  gtk_widget_ref( CBNormalize );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"CBNormalize",CBNormalize,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( CBNormalize );
  gtk_box_pack_start( GTK_BOX( vbox3 ),CBNormalize,FALSE,FALSE,0 );

  CBAudioEqualizer=gtk_check_button_new_with_label( MSGTR_PREFERENCES_EnEqualizer );
  gtk_widget_set_name( CBAudioEqualizer,"CBAudioEqualizer" );
  gtk_widget_ref( CBAudioEqualizer );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"CBAudioEqualizer",CBAudioEqualizer,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( CBAudioEqualizer );
  gtk_box_pack_start( GTK_BOX( vbox3 ),CBAudioEqualizer,FALSE,FALSE,0 );

#if 0
  CBSurround=gtk_check_button_new_with_label( "Enable surround" );
  gtk_widget_set_name( CBSurround,"CBSurround" );
  gtk_widget_ref( CBSurround );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"CBSurround",CBSurround,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( CBSurround );
  gtk_box_pack_start( GTK_BOX( vbox3 ),CBSurround,FALSE,FALSE,0 );
#endif

  CBExtraStereo=gtk_check_button_new_with_label( MSGTR_PREFERENCES_ExtraStereo );
  gtk_widget_set_name( CBExtraStereo,"CBExtraStereo" );
  gtk_widget_ref( CBExtraStereo );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"CBExtraStereo",CBExtraStereo,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( CBExtraStereo );
  gtk_box_pack_start( GTK_BOX( vbox3 ),CBExtraStereo,FALSE,FALSE,0 );

  hbox8=gtk_hbox_new( FALSE,0 );
  gtk_widget_set_name( hbox8,"hbox8" );
  gtk_widget_ref( hbox8 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"hbox8",hbox8,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( hbox8 );
  gtk_box_pack_start( GTK_BOX( vbox3 ),hbox8,FALSE,FALSE,0 );

  label17=gtk_label_new( MSGTR_PREFERENCES_Coefficient );
  gtk_widget_set_name( label17,"label17" );
  gtk_widget_ref( label17 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"label17",label17,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( label17 );
  gtk_box_pack_start( GTK_BOX( hbox8 ),label17,FALSE,FALSE,0 );
  gtk_misc_set_alignment( GTK_MISC( label17 ),7.45058e-09,0.5 );
  gtk_misc_set_padding( GTK_MISC( label17 ),20,0 );

  HSExtraStereoMuladj=GTK_ADJUSTMENT( gtk_adjustment_new( 0,-10,10,0.1,0,0 ) );
  HSExtraStereoMul=gtk_hscale_new( HSExtraStereoMuladj );
  gtk_widget_set_name( HSExtraStereoMul,"HSExtraStereoMul" );
  gtk_widget_ref( HSExtraStereoMul );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"HSExtraStereoMul",HSExtraStereoMul,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( HSExtraStereoMul );
  gtk_box_pack_start( GTK_BOX( hbox8 ),HSExtraStereoMul,TRUE,TRUE,0 );
  gtk_scale_set_value_pos( GTK_SCALE( HSExtraStereoMul ),GTK_POS_RIGHT );
  gtk_scale_set_digits( GTK_SCALE( HSExtraStereoMul ),1 );

  hbox8=gtk_hbox_new( FALSE,0 );
  gtk_widget_set_name( hbox8,"hbox8" );
  gtk_widget_ref( hbox8 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"hbox8",hbox8,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( hbox8 );
  gtk_box_pack_start( GTK_BOX( vbox3 ),hbox8,FALSE,FALSE,0 );

  label17=gtk_label_new( MSGTR_PREFERENCES_AudioDelay );
  gtk_widget_set_name( label17,"label17" );
  gtk_widget_ref( label17 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"label17",label17,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( label17 );
  gtk_box_pack_start( GTK_BOX( hbox8 ),label17,FALSE,FALSE,0 );
  gtk_misc_set_alignment( GTK_MISC( label17 ),7.45058e-09,0.5 );
  gtk_misc_set_padding( GTK_MISC( label17 ),4,0 );

  HSAudioDelayadj=GTK_ADJUSTMENT( gtk_adjustment_new( 0,-100,100,0.01,0,0 ) );
  HSAudioDelay=gtk_hscale_new( HSAudioDelayadj );
  gtk_widget_set_name( HSAudioDelay,"HSAudioDelay" );
  gtk_widget_ref( HSAudioDelay );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"HSAudioDelay",HSAudioDelay,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( HSAudioDelay );
  gtk_box_pack_start( GTK_BOX( hbox8 ),HSAudioDelay,TRUE,TRUE,0 );
  gtk_scale_set_value_pos( GTK_SCALE( HSAudioDelay ),GTK_POS_RIGHT );
  gtk_scale_set_digits( GTK_SCALE( HSAudioDelay ),2 );

  label1=gtk_label_new( MSGTR_PREFERENCES_Audio );
  gtk_widget_set_name( label1,"label1" );
  gtk_widget_ref( label1 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"label1",label1,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( label1 );
  gtk_notebook_set_tab_label( GTK_NOTEBOOK( notebook1 ),gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook1 ),0 ),label1 );

  hbox2=gtk_hbox_new( FALSE,0 );
  gtk_widget_set_name( hbox2,"hbox2" );
  gtk_widget_ref( hbox2 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"hbox2",hbox2,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( hbox2 );
  gtk_container_add( GTK_CONTAINER( notebook1 ),hbox2 );

  frame7=gtk_frame_new( NULL );
  gtk_widget_set_name( frame7,"frame7" );
  gtk_widget_ref( frame7 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"frame7",frame7,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame7 );
  gtk_box_pack_start( GTK_BOX( hbox2 ),frame7,TRUE,TRUE,0 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame7 ),GTK_SHADOW_ETCHED_OUT );

  frame=gtk_frame_new( NULL );
  gtk_widget_set_name( frame,"frame" );
  gtk_widget_ref( frame );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"frame",frame,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame );
  gtk_container_add( GTK_CONTAINER( frame7 ),frame );
  gtk_container_set_border_width( GTK_CONTAINER( frame ),0 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame ),GTK_SHADOW_NONE );

  vbox4=gtk_vbox_new( FALSE,0 );
  gtk_widget_set_name( vbox4,"vbox4" );
  gtk_widget_ref( vbox4 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"vbox4",vbox4,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( vbox4 );
  gtk_container_add( GTK_CONTAINER( frame ),vbox4 );

  scrolledwindow2=gtk_scrolled_window_new( NULL,NULL );
  gtk_widget_set_name( scrolledwindow2,"scrolledwindow2" );
  gtk_widget_ref( scrolledwindow2 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"scrolledwindow2",scrolledwindow2,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( scrolledwindow2 );
  gtk_box_pack_start( GTK_BOX( vbox4 ),scrolledwindow2,TRUE,TRUE,0 );
  gtk_scrolled_window_set_policy( GTK_SCROLLED_WINDOW( scrolledwindow2 ),GTK_POLICY_AUTOMATIC,GTK_POLICY_AUTOMATIC );

  CLVDrivers=gtk_clist_new( 2 );
  gtk_widget_set_name( CLVDrivers,"CLVDrivers" );
  gtk_widget_ref( CLVDrivers );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"CLVDrivers",CLVDrivers,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( CLVDrivers );
  gtk_container_add( GTK_CONTAINER( scrolledwindow2 ),CLVDrivers );
  gtk_clist_set_column_width( GTK_CLIST( CLVDrivers ),0,50 );
  gtk_clist_column_titles_show( GTK_CLIST( CLVDrivers ) );
  gtk_clist_set_shadow_type( GTK_CLIST( CLVDrivers ),GTK_SHADOW_NONE );
  gtk_widget_set_usize( CLVDrivers,200,-2 );

  label7=gtk_label_new( MSGTR_PREFERENCES_AvailableDrivers );
  gtk_widget_set_name( label7,"label7" );
  gtk_widget_ref( label7 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"label7",label7,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( label7 );
  gtk_clist_set_column_widget( GTK_CLIST( CLVDrivers ),0,label7 );
  gtk_misc_set_alignment( GTK_MISC( label7 ),0,0.5 );
  gtk_misc_set_padding( GTK_MISC( label7 ),4,0 );

  hbuttonbox3=gtk_hbutton_box_new();
  gtk_widget_set_name( hbuttonbox3,"hbuttonbox3" );
  gtk_widget_ref( hbuttonbox3 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"hbuttonbox3",hbuttonbox3,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( hbuttonbox3 );
  gtk_box_pack_start( GTK_BOX( vbox4 ),hbuttonbox3,FALSE,FALSE,0 );
  gtk_button_box_set_child_size( GTK_BUTTON_BOX( hbuttonbox3 ),85,20 );

  VConfig=gtk_button_new_with_label( MSGTR_ConfigDriver );
  gtk_widget_set_name( VConfig,"VConfig" );
  gtk_widget_ref( VConfig );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"VConfig",VConfig,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( VConfig );
  gtk_container_add( GTK_CONTAINER( hbuttonbox3 ),VConfig );

  frame8=gtk_frame_new( NULL );
  gtk_widget_set_name( frame8,"frame8" );
  gtk_widget_ref( frame8 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"frame8",frame8,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame8 );
  gtk_box_pack_start( GTK_BOX( hbox2 ),frame8,TRUE,TRUE,0 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame8 ),GTK_SHADOW_ETCHED_OUT );

  frame=gtk_frame_new( NULL );
  gtk_widget_set_name( frame,"frame" );
  gtk_widget_ref( frame );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"frame",frame,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame );
  gtk_container_add( GTK_CONTAINER( frame8 ),frame );
  gtk_container_set_border_width( GTK_CONTAINER( frame ),0 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame ),GTK_SHADOW_NONE );

  vbox5=gtk_vbox_new( FALSE,0 );
  gtk_widget_set_name( vbox5,"vbox5" );
  gtk_widget_ref( vbox5 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"vbox5",vbox5,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( vbox5 );
  gtk_container_add( GTK_CONTAINER( frame ),vbox5 );
  gtk_widget_set_usize( vbox5,250,-2 );

  CBDoubleBuffer=gtk_check_button_new_with_label( MSGTR_PREFERENCES_DoubleBuffer );
  gtk_widget_set_name( CBDoubleBuffer,"CBDoubleBuffer" );
  gtk_widget_ref( CBDoubleBuffer );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"CBDoubleBuffer",CBDoubleBuffer,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( CBDoubleBuffer );
  gtk_box_pack_start( GTK_BOX( vbox5 ),CBDoubleBuffer,FALSE,FALSE,0 );
  gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( CBDoubleBuffer ),TRUE );

  CBDR=gtk_check_button_new_with_label( MSGTR_PREFERENCES_DirectRender );
  gtk_widget_set_name( CBDR,"CBDR" );
  gtk_widget_ref( CBDR );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"CBDR",CBDR,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( CBDR );
  gtk_box_pack_start( GTK_BOX( vbox5 ),CBDR,FALSE,FALSE,0 );

  CBFramedrop=gtk_check_button_new_with_label( MSGTR_PREFERENCES_FrameDrop );
  gtk_widget_set_name( CBFramedrop,"CBFramedrop" );
  gtk_widget_ref( CBFramedrop );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"CBFramedrop",CBFramedrop,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( CBFramedrop );
  gtk_box_pack_start( GTK_BOX( vbox5 ),CBFramedrop,FALSE,FALSE,0 );
  gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( CBFramedrop ),TRUE );

  CBHFramedrop=gtk_check_button_new_with_label( MSGTR_PREFERENCES_HFrameDrop );
  gtk_widget_set_name( CBHFramedrop,"CBHFramedrop" );
  gtk_widget_ref( CBHFramedrop );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"CBHFramedrop",CBHFramedrop,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( CBHFramedrop );
  gtk_box_pack_start( GTK_BOX( vbox5 ),CBHFramedrop,FALSE,FALSE,0 );

  CBFlip=gtk_check_button_new_with_label( MSGTR_PREFERENCES_Flip );
  gtk_widget_set_name( CBFlip,"CBFlip" );
  gtk_widget_ref( CBFlip );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"CBFlip",CBFlip,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( CBFlip );
  gtk_box_pack_start( GTK_BOX( vbox5 ),CBFlip,FALSE,FALSE,0 );

  hbox3=gtk_hbox_new( FALSE,0 );
  gtk_widget_set_name( hbox3,"hbox3" );
  gtk_widget_ref( hbox3 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"hbox3",hbox3,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( hbox3 );
  gtk_box_pack_start( GTK_BOX( vbox5 ),hbox3,FALSE,FALSE,0 );

  label9=gtk_label_new( MSGTR_PREFERENCES_Panscan );
  gtk_widget_set_name( label9,"label9" );
  gtk_widget_ref( label9 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"label9",label9,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( label9 );
  gtk_box_pack_start( GTK_BOX( hbox3 ),label9,FALSE,FALSE,0 );
  gtk_misc_set_alignment( GTK_MISC( label9 ),7.45058e-09,0.5 );
  gtk_misc_set_padding( GTK_MISC( label9 ),4,0 );

  HSPanscanadj=GTK_ADJUSTMENT( gtk_adjustment_new( 0,0,1,0.001,0,0 ) );
  HSPanscan=gtk_hscale_new( HSPanscanadj );
  gtk_widget_set_name( HSPanscan,"HSPanscan" );
  gtk_widget_ref( HSPanscan );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"HSPanscan",HSPanscan,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( HSPanscan );
  gtk_box_pack_start( GTK_BOX( hbox3 ),HSPanscan,TRUE,TRUE,0 );
  gtk_scale_set_value_pos( GTK_SCALE( HSPanscan ),GTK_POS_RIGHT );

  label2=gtk_label_new( MSGTR_PREFERENCES_Video );
  gtk_widget_set_name( label2,"label2" );
  gtk_widget_ref( label2 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"label2",label2,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( label2 );
  gtk_notebook_set_tab_label( GTK_NOTEBOOK( notebook1 ),gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook1 ),1 ),label2 );

  vbox6=gtk_vbox_new( FALSE,0 );
  gtk_widget_set_name( vbox6,"vbox6" );
  gtk_widget_ref( vbox6 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"vbox6",vbox6,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( vbox6 );
  gtk_container_add( GTK_CONTAINER( notebook1 ),vbox6 );

  frame5=gtk_frame_new( MSGTR_PREFERENCES_FRAME_OSD_Level );
  gtk_widget_set_name( frame5,"frame5" );
  gtk_widget_ref( frame5 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"frame5",frame5,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame5 );
  gtk_box_pack_start( GTK_BOX( vbox6 ),frame5,FALSE,FALSE,0 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame5 ),GTK_SHADOW_ETCHED_OUT );

  frame=gtk_frame_new( NULL );
  gtk_widget_set_name( frame,"frame" );
  gtk_widget_ref( frame );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"frame",frame,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame );
  gtk_container_add( GTK_CONTAINER( frame5 ),frame );
  gtk_container_set_border_width( GTK_CONTAINER( frame ),0 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame ),GTK_SHADOW_NONE );

  vbox600=gtk_vbox_new( FALSE,0 );
  gtk_widget_set_name( vbox600,"vbox600" );
  gtk_widget_ref( vbox600 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"vbox600",vbox600,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( vbox600 );
  gtk_container_add( GTK_CONTAINER( frame ),vbox600 );

  RBOSDNone=gtk_radio_button_new_with_label( OSD_group,MSGTR_PREFERENCES_None );
  OSD_group=gtk_radio_button_group( GTK_RADIO_BUTTON( RBOSDNone ) );
  gtk_widget_set_name( RBOSDNone,"RBOSDNone" );
  gtk_widget_ref( RBOSDNone );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"RBOSDNone",RBOSDNone,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( RBOSDNone );
  gtk_box_pack_start( GTK_BOX( vbox600 ),RBOSDNone,FALSE,FALSE,0 );

  RBOSDTandP=gtk_radio_button_new_with_label( OSD_group,MSGTR_PREFERENCES_OSDTimer );
  OSD_group=gtk_radio_button_group( GTK_RADIO_BUTTON( RBOSDTandP ) );
  gtk_widget_set_name( RBOSDTandP,"RBOSDTandP" );
  gtk_widget_ref( RBOSDTandP );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"RBOSDTandP",RBOSDTandP,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( RBOSDTandP );
  gtk_box_pack_start( GTK_BOX( vbox600 ),RBOSDTandP,FALSE,FALSE,0 );

  RBOSDIndicator=gtk_radio_button_new_with_label( OSD_group,MSGTR_PREFERENCES_OSDProgress );
  OSD_group=gtk_radio_button_group( GTK_RADIO_BUTTON( RBOSDIndicator ) );
  gtk_widget_set_name( RBOSDIndicator,"RBOSDIndicator" );
  gtk_widget_ref( RBOSDIndicator );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"RBOSDIndicator",RBOSDIndicator,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( RBOSDIndicator );
  gtk_box_pack_start( GTK_BOX( vbox600 ),RBOSDIndicator,FALSE,FALSE,0 );
  gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( RBOSDIndicator ),TRUE );

  frame6=gtk_frame_new( MSGTR_PREFERENCES_FRAME_Subtitle );
  gtk_widget_set_name( frame6,"frame6" );
  gtk_widget_ref( frame6 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"frame6",frame6,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame6 );
  gtk_box_pack_start( GTK_BOX( vbox6 ),frame6,FALSE,FALSE,0 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame6 ),GTK_SHADOW_ETCHED_OUT );

  frame=gtk_frame_new( NULL );
  gtk_widget_set_name( frame,"frame" );
  gtk_widget_ref( frame );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"frame",frame,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame );
  gtk_container_add( GTK_CONTAINER( frame6 ),frame );
  gtk_container_set_border_width( GTK_CONTAINER( frame ),0 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame ),GTK_SHADOW_NONE );

  vbox7=gtk_vbox_new( FALSE,0 );
  gtk_widget_set_name( vbox7,"vbox7" );
  gtk_widget_ref( vbox7 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"vbox7",vbox7,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( vbox7 );
  gtk_container_add( GTK_CONTAINER( frame ),vbox7 );
#if 0
  hbox4=gtk_hbox_new( FALSE,0 );
  gtk_widget_set_name( hbox4,"hbox4" );
  gtk_widget_ref( hbox4 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"hbox4",hbox4,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( hbox4 );
  gtk_box_pack_start( GTK_BOX( vbox7 ),hbox4,FALSE,FALSE,0 );

  label10=gtk_label_new( MSGTR_PREFERENCES_Subtitle );
  gtk_widget_set_name( label10,"label10" );
  gtk_widget_ref( label10 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"label10",label10,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( label10 );
  gtk_box_pack_start( GTK_BOX( hbox4 ),label10,FALSE,FALSE,0 );
  gtk_misc_set_alignment( GTK_MISC( label10 ),0,0.5 );
  gtk_misc_set_padding( GTK_MISC( label10 ),4,0 );

  ESubtitleName=gtk_entry_new();
  gtk_widget_set_name( ESubtitleName,"ESubtitleName" );
  gtk_widget_ref( ESubtitleName );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"ESubtitleName",ESubtitleName,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( ESubtitleName );
  gtk_box_pack_start( GTK_BOX( hbox4 ),ESubtitleName,TRUE,TRUE,0 );

  hbuttonbox4=gtk_hbutton_box_new();
  gtk_widget_set_name( hbuttonbox4,"hbuttonbox4" );
  gtk_widget_ref( hbuttonbox4 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"hbuttonbox4",hbuttonbox4,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( hbuttonbox4 );
  gtk_box_pack_start( GTK_BOX( hbox4 ),hbuttonbox4,FALSE,FALSE,0 );
  gtk_container_set_border_width( GTK_CONTAINER( hbuttonbox4 ),3 );
  gtk_button_box_set_spacing( GTK_BUTTON_BOX( hbuttonbox4 ),0 );
  gtk_button_box_set_child_size( GTK_BUTTON_BOX( hbuttonbox4 ),85,20 );

  BLoadSubtitle=gtk_button_new_with_label( MSGTR_Browse );
  gtk_widget_set_name( BLoadSubtitle,"BLoadSubtitle" );
  gtk_widget_ref( BLoadSubtitle );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"BLoadSubtitle",BLoadSubtitle,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( BLoadSubtitle );
  gtk_container_add( GTK_CONTAINER( hbuttonbox4 ),BLoadSubtitle );
#endif
  vbox8=gtk_vbox_new( FALSE,0 );
  gtk_widget_set_name( vbox8,"vbox8" );
  gtk_widget_ref( vbox8 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"vbox8",vbox8,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( vbox8 );
  gtk_box_pack_start( GTK_BOX( vbox7 ),vbox8,FALSE,FALSE,0 );

  table1=gtk_table_new( 3,2,FALSE );
  gtk_widget_set_name( table1,"table1" );
  gtk_widget_ref( table1 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"table1",table1,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( table1 );
  gtk_box_pack_start( GTK_BOX( vbox8 ),table1,FALSE,FALSE,0 );

  label11=gtk_label_new( MSGTR_PREFERENCES_SUB_Delay );
  gtk_widget_set_name( label11,"label11" );
  gtk_widget_ref( label11 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"label11",label11,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( label11 );
  gtk_table_attach( GTK_TABLE( table1 ),label11,0,1,0,1,(GtkAttachOptions)( GTK_FILL ),(GtkAttachOptions)( 0 ),0,0 );
  gtk_misc_set_alignment( GTK_MISC( label11 ),0,0.5 );
  gtk_misc_set_padding( GTK_MISC( label11 ),4,0 );

  label12=gtk_label_new( MSGTR_PREFERENCES_SUB_FPS );
  gtk_widget_set_name( label12,"label12" );
  gtk_widget_ref( label12 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"label12",label12,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( label12 );
  gtk_table_attach( GTK_TABLE( table1 ),label12,0,1,1,2,(GtkAttachOptions)( GTK_FILL ),(GtkAttachOptions)( 0 ),0,0 );
  gtk_misc_set_alignment( GTK_MISC( label12 ),0,0.5 );
  gtk_misc_set_padding( GTK_MISC( label12 ),4,0 );

  HSSubDelayadj=GTK_ADJUSTMENT( gtk_adjustment_new( 0,-10.0,10,0.01,0,0 ) );
  HSSubDelay=gtk_hscale_new( HSSubDelayadj );
  gtk_widget_set_name( HSSubDelay,"HSSubDelay" );
  gtk_widget_ref( HSSubDelay );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"HSSubDelay",HSSubDelay,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( HSSubDelay );
  gtk_table_attach( GTK_TABLE( table1 ),HSSubDelay,1,2,0,1,(GtkAttachOptions)( GTK_EXPAND | GTK_FILL ),(GtkAttachOptions)( 0 ),0,0 );
  gtk_scale_set_value_pos( GTK_SCALE( HSSubDelay ),GTK_POS_RIGHT );

  label13=gtk_label_new( MSGTR_PREFERENCES_SUB_POS );
  gtk_widget_set_name( label13,"label13" );
  gtk_widget_ref( label13 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"label13",label13,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( label13 );
  gtk_table_attach( GTK_TABLE( table1 ),label13,0,1,2,3,(GtkAttachOptions)( GTK_FILL ),(GtkAttachOptions)( 0 ),0,0 );
  gtk_misc_set_alignment( GTK_MISC( label13 ),0,0.5 );
  gtk_misc_set_padding( GTK_MISC( label13 ),4,0 );

  HSSubPositionadj=GTK_ADJUSTMENT( gtk_adjustment_new( 100,0,100,1,0,0 ) );
  HSSubPosition=gtk_hscale_new( HSSubPositionadj );
  gtk_widget_set_name( HSSubPosition,"HSSubPosition" );
  gtk_widget_ref( HSSubPosition );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"HSSubPosition",HSSubPosition,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( HSSubPosition );
  gtk_table_attach( GTK_TABLE( table1 ),HSSubPosition,1,2,2,3,(GtkAttachOptions)( GTK_EXPAND | GTK_FILL ),(GtkAttachOptions)( 0 ),0,0 );
  gtk_scale_set_value_pos( GTK_SCALE( HSSubPosition ),GTK_POS_RIGHT );
  gtk_scale_set_digits( GTK_SCALE( HSSubPosition ),0 );

  HSSubFPSadj=GTK_ADJUSTMENT( gtk_adjustment_new( 0,0,100,0.01,0,0 ) );
  HSSubFPS=gtk_hscale_new( HSSubFPSadj );
  gtk_widget_set_name( HSSubFPS,"HSSubFPS" );
  gtk_widget_ref( HSSubFPS );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"HSSubFPS",HSSubFPS,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( HSSubFPS );
  gtk_table_attach( GTK_TABLE( table1 ),HSSubFPS,1,2,1,2,(GtkAttachOptions)( GTK_FILL ),(GtkAttachOptions)( 0 ),0,0 );
  gtk_scale_set_value_pos( GTK_SCALE( HSSubFPS ),GTK_POS_RIGHT );

  vbox9=gtk_vbox_new( FALSE,0 );
  gtk_widget_set_name( vbox9,"vbox9" );
  gtk_widget_ref( vbox9 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"vbox9",vbox9,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( vbox9 );
  gtk_box_pack_start( GTK_BOX( vbox8 ),vbox9,FALSE,FALSE,0 );

  CBNoAutoSub=gtk_check_button_new_with_label( MSGTR_PREFERENCES_SUB_AutoLoad );
  gtk_widget_set_name( CBNoAutoSub,"CBNoAutoSub" );
  gtk_widget_ref( CBNoAutoSub );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"CBNoAutoSub",CBNoAutoSub,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( CBNoAutoSub );
  gtk_box_pack_start( GTK_BOX( vbox9 ),CBNoAutoSub,FALSE,FALSE,0 );

  CBSubUnicode=gtk_check_button_new_with_label( MSGTR_PREFERENCES_SUB_Unicode );
  gtk_widget_set_name( CBSubUnicode,"CBSubUnicode" );
  gtk_widget_ref( CBSubUnicode );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"CBSubUnicode",CBSubUnicode,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( CBSubUnicode );
  gtk_box_pack_start( GTK_BOX( vbox9 ),CBSubUnicode,FALSE,FALSE,0 );

  CBDumpMPSub=gtk_check_button_new_with_label( MSGTR_PREFERENCES_SUB_MPSUB );
  gtk_widget_set_name( CBDumpMPSub,"CBDumpMPSub" );
  gtk_widget_ref( CBDumpMPSub );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"CBDumpMPSub",CBDumpMPSub,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( CBDumpMPSub );
  gtk_box_pack_start( GTK_BOX( vbox9 ),CBDumpMPSub,FALSE,FALSE,0 );

  CBDumpSrt=gtk_check_button_new_with_label( MSGTR_PREFERENCES_SUB_SRT );
  gtk_widget_set_name( CBDumpSrt,"CBDumpSrt" );
  gtk_widget_ref( CBDumpSrt );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"CBDumpSrt",CBDumpSrt,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( CBDumpSrt );
  gtk_box_pack_start( GTK_BOX( vbox9 ),CBDumpSrt,FALSE,FALSE,0 );

  frame12=gtk_frame_new( MSGTR_PREFERENCES_FRAME_Font );
  gtk_widget_set_name( frame12,"frame12" );
  gtk_widget_ref( frame12 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"frame12",frame12,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame12 );
  gtk_box_pack_start( GTK_BOX( vbox6 ),frame12,TRUE,TRUE,0 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame12 ),GTK_SHADOW_ETCHED_OUT );

  frame=gtk_frame_new( NULL );
  gtk_widget_set_name( frame,"frame" );
  gtk_widget_ref( frame );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"frame",frame,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame );
  gtk_container_add( GTK_CONTAINER( frame12 ),frame );
  gtk_container_set_border_width( GTK_CONTAINER( frame ),0 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame ),GTK_SHADOW_NONE );
  
  vbox603=gtk_vbox_new( FALSE,0 );
  gtk_widget_set_name( vbox603,"vbox603" );
  gtk_widget_ref( vbox603 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"vbox603",vbox603,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( vbox603 );
  gtk_container_add( GTK_CONTAINER( frame ),vbox603 );

  hbox6=gtk_hbox_new( FALSE,0 );
  gtk_widget_set_name( hbox6,"hbox6" );
  gtk_widget_ref( hbox6 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"hbox6",hbox6,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( hbox6 );
  gtk_box_pack_start( GTK_BOX( vbox603 ),hbox6,FALSE,FALSE,0 );

  label15=gtk_label_new( MSGTR_PREFERENCES_Font );
  gtk_widget_set_name( label15,"label15" );
  gtk_widget_ref( label15 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"label15",label15,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( label15 );
  gtk_box_pack_start( GTK_BOX( hbox6 ),label15,FALSE,FALSE,0 );
  gtk_misc_set_alignment( GTK_MISC( label15 ),0,0.5 );
  gtk_misc_set_padding( GTK_MISC( label15 ),4,0 );

  prEFontName=gtk_entry_new();
  gtk_widget_set_name( prEFontName,"prEFontName" );
  gtk_widget_ref( prEFontName );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"prEFontName",prEFontName,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( prEFontName );
  gtk_box_pack_start( GTK_BOX( hbox6 ),prEFontName,TRUE,TRUE,0 );

  hbuttonbox5=gtk_hbutton_box_new();
  gtk_widget_set_name( hbuttonbox5,"hbuttonbox5" );
  gtk_widget_ref( hbuttonbox5 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"hbuttonbox5",hbuttonbox5,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( hbuttonbox5 );
  gtk_box_pack_start( GTK_BOX( hbox6 ),hbuttonbox5,FALSE,FALSE,0 );
  gtk_container_set_border_width( GTK_CONTAINER( hbuttonbox5 ),3 );
  gtk_button_box_set_spacing( GTK_BUTTON_BOX( hbuttonbox5 ),0 );
  gtk_button_box_set_child_size( GTK_BUTTON_BOX( hbuttonbox5 ),85,20 );
  gtk_button_box_set_child_ipadding( GTK_BUTTON_BOX( hbuttonbox5 ),0,0 );

  BLoadFont=gtk_button_new_with_label( MSGTR_Browse );
  gtk_widget_set_name( BLoadFont,"BLoadFont" );
  gtk_widget_ref( BLoadFont );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"BLoadFont",BLoadFont,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( BLoadFont );
  gtk_container_add( GTK_CONTAINER( hbuttonbox5 ),BLoadFont );

#ifndef HAVE_FREETYPE
  hbox7=gtk_hbox_new( FALSE,0 );
  gtk_widget_set_name( hbox7,"hbox7" );
  gtk_widget_ref( hbox7 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"hbox7",hbox7,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( hbox7 );
  gtk_box_pack_start( GTK_BOX( vbox603 ),hbox7,FALSE,FALSE,0 );

  label16=gtk_label_new( MSGTR_PREFERENCES_FontFactor );
  gtk_widget_set_name( label16,"label16" );
  gtk_widget_ref( label16 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"label16",label16,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( label16 );
  gtk_box_pack_start( GTK_BOX( hbox7 ),label16,FALSE,FALSE,0 );
  gtk_misc_set_alignment( GTK_MISC( label16 ),7.45058e-09,0.5 );
  gtk_misc_set_padding( GTK_MISC( label16 ),4,0 );

  HSFontFactoradj=GTK_ADJUSTMENT( gtk_adjustment_new( 0,0,10,0.05,0,0 ) );
  HSFontFactor=gtk_hscale_new( HSFontFactoradj );
  gtk_widget_set_name( HSFontFactor,"HSFontFactor" );
  gtk_widget_ref( HSFontFactor );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"HSFontFactor",HSFontFactor,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( HSFontFactor );
  gtk_box_pack_start( GTK_BOX( hbox7 ),HSFontFactor,TRUE,TRUE,0 );
  gtk_scale_set_value_pos( GTK_SCALE( HSFontFactor ),GTK_POS_RIGHT );
  gtk_scale_set_digits( GTK_SCALE( HSFontFactor ),2 );
#else

  
//	static GtkWidget     * RBFontNoAutoScale, * BRFontAutoScaleWidth, * RBFontAutoScaleHeight, * RBFontAutoScaleDiagonal;

  RBFontNoAutoScale=gtk_radio_button_new_with_label( Font_group,MSGTR_PREFERENCES_FontNoAutoScale );
  Font_group=gtk_radio_button_group( GTK_RADIO_BUTTON( RBFontNoAutoScale ) );
  gtk_widget_set_name( RBFontNoAutoScale,"RBFontNoAutoScale" );
  gtk_widget_ref( RBFontNoAutoScale );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"RBFontNoAutoScale",RBFontNoAutoScale,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( RBFontNoAutoScale );
  gtk_box_pack_start( GTK_BOX( vbox603 ),RBFontNoAutoScale,FALSE,FALSE,0 );

  BRFontAutoScaleWidth=gtk_radio_button_new_with_label( Font_group,MSGTR_PREFERENCES_FontPropWidth );
  Font_group=gtk_radio_button_group( GTK_RADIO_BUTTON( BRFontAutoScaleWidth ) );
  gtk_widget_set_name( BRFontAutoScaleWidth,"BRFontAutoScaleWidth" );
  gtk_widget_ref( BRFontAutoScaleWidth );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"BRFontAutoScaleWidth",BRFontAutoScaleWidth,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( BRFontAutoScaleWidth );
  gtk_box_pack_start( GTK_BOX( vbox603 ),BRFontAutoScaleWidth,FALSE,FALSE,0 );

  RBFontAutoScaleHeight=gtk_radio_button_new_with_label( Font_group,MSGTR_PREFERENCES_FontPropHeight );
  Font_group=gtk_radio_button_group( GTK_RADIO_BUTTON( RBFontAutoScaleHeight ) );
  gtk_widget_set_name( RBFontAutoScaleHeight,"RBFontAutoScaleHeight" );
  gtk_widget_ref( RBFontAutoScaleHeight );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"RBFontAutoScaleHeight",RBFontAutoScaleHeight,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( RBFontAutoScaleHeight );
  gtk_box_pack_start( GTK_BOX( vbox603 ),RBFontAutoScaleHeight,FALSE,FALSE,0 );

  RBFontAutoScaleDiagonal=gtk_radio_button_new_with_label( Font_group,MSGTR_PREFERENCES_FontPropDiagonal );
  Font_group=gtk_radio_button_group( GTK_RADIO_BUTTON( RBFontAutoScaleDiagonal ) );
  gtk_widget_set_name( RBFontAutoScaleDiagonal,"RBFontAutoScaleDiagonal" );
  gtk_widget_ref( RBFontAutoScaleDiagonal );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"RBFontAutoScaleDiagonal",RBFontAutoScaleDiagonal,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( RBFontAutoScaleDiagonal );
  gtk_box_pack_start( GTK_BOX( vbox603 ),RBFontAutoScaleDiagonal,FALSE,FALSE,0 );

  table1=gtk_table_new( 3,2,FALSE );
  gtk_widget_set_name( table1,"table1" );
  gtk_widget_ref( table1 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"table1",table1,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( table1 );
  gtk_box_pack_start( GTK_BOX( vbox603 ),table1,FALSE,FALSE,0 );

  label16=gtk_label_new( MSGTR_PREFERENCES_FontEncoding );
  gtk_widget_set_name( label16,"label16" );
  gtk_widget_ref( label16 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"label16",label16,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( label16 );
  gtk_table_attach( GTK_TABLE( table1 ),label16,0,1,0,1,(GtkAttachOptions)( GTK_FILL ),(GtkAttachOptions)( 0 ),0,0 );
  gtk_misc_set_alignment( GTK_MISC( label16 ),7.45058e-09,0.5 );
  gtk_misc_set_padding( GTK_MISC( label16 ),4,0 );
  
  CBFontEncoding=gtk_combo_new();
  gtk_widget_set_name( CBFontEncoding,"CBFontEncoding" );
  gtk_widget_ref( CBFontEncoding );
  gtk_widget_show( CBFontEncoding );
  gtk_table_attach( GTK_TABLE( table1 ),CBFontEncoding,1,2,0,1,(GtkAttachOptions)( GTK_FILL ),(GtkAttachOptions)( 0 ),0,0 );
  {
   int i;
   for ( i=0;lEncoding[i].name;i++ ) CBFontEncoding_items=g_list_append( CBFontEncoding_items,lEncoding[i].comment );
  }
  gtk_combo_set_popdown_strings( GTK_COMBO( CBFontEncoding ),CBFontEncoding_items );
  g_list_free( CBFontEncoding_items );

  EFontEncoding=GTK_COMBO( CBFontEncoding )->entry;
  gtk_widget_set_name( EFontEncoding,"EFontEncoding" );
  gtk_entry_set_editable( GTK_ENTRY( EFontEncoding ),FALSE );
  gtk_widget_ref( EFontEncoding );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"EFontEncoding",EFontEncoding,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( EFontEncoding );

  label16=gtk_label_new( MSGTR_PREFERENCES_FontBlur );
  gtk_widget_set_name( label16,"label16" );
  gtk_widget_ref( label16 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"label16",label16,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( label16 );
  gtk_table_attach( GTK_TABLE( table1 ),label16,0,1,1,2,(GtkAttachOptions)( GTK_FILL ),(GtkAttachOptions)( 0 ),0,0 );
  gtk_misc_set_alignment( GTK_MISC( label16 ),7.45058e-09,0.5 );
  gtk_misc_set_padding( GTK_MISC( label16 ),4,0 );

  HSFontBluradj=GTK_ADJUSTMENT( gtk_adjustment_new( 0,0,100,0.1,0,0 ) );
  HSFontBlur=gtk_hscale_new( HSFontBluradj );
  gtk_widget_set_name( HSFontBlur,"HSFontBlur" );
  gtk_widget_ref( HSFontBlur );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"HSFontBlur",HSFontBlur,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( HSFontBlur );
  gtk_table_attach( GTK_TABLE( table1 ),HSFontBlur,1,2,1,2,(GtkAttachOptions)( GTK_EXPAND | GTK_FILL ),(GtkAttachOptions)( 0 ),0,0 );
  gtk_scale_set_value_pos( GTK_SCALE( HSFontBlur ),GTK_POS_RIGHT );
  gtk_scale_set_digits( GTK_SCALE( HSFontBlur ),2 );

  label16=gtk_label_new( MSGTR_PREFERENCES_FontOutLine );
  gtk_widget_set_name( label16,"label16" );
  gtk_widget_ref( label16 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"label16",label16,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( label16 );
  gtk_table_attach( GTK_TABLE( table1 ),label16,0,1,2,3,(GtkAttachOptions)( GTK_FILL ),(GtkAttachOptions)( 0 ),0,0 );
  gtk_misc_set_alignment( GTK_MISC( label16 ),7.45058e-09,0.5 );
  gtk_misc_set_padding( GTK_MISC( label16 ),4,0 );

  HSFontOutLineadj=GTK_ADJUSTMENT( gtk_adjustment_new( 0,0,100,0.1,0,0 ) );
  HSFontOutLine=gtk_hscale_new( HSFontOutLineadj );
  gtk_widget_set_name( HSFontOutLine,"HSFontOutLine" );
  gtk_widget_ref( HSFontOutLine );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"HSFontOutLine",HSFontOutLine,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( HSFontOutLine );
  gtk_table_attach( GTK_TABLE( table1 ),HSFontOutLine,1,2,2,3,(GtkAttachOptions)( GTK_EXPAND | GTK_FILL ),(GtkAttachOptions)( 0 ),0,0 );
  gtk_scale_set_value_pos( GTK_SCALE( HSFontOutLine ),GTK_POS_RIGHT );
  gtk_scale_set_digits( GTK_SCALE( HSFontOutLine ),2 );

  label16=gtk_label_new( MSGTR_PREFERENCES_FontTextScale );
  gtk_widget_set_name( label16,"label16" );
  gtk_widget_ref( label16 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"label16",label16,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( label16 );
  gtk_table_attach( GTK_TABLE( table1 ),label16,0,1,3,4,(GtkAttachOptions)( GTK_FILL ),(GtkAttachOptions)( 0 ),0,0 );
  gtk_misc_set_alignment( GTK_MISC( label16 ),7.45058e-09,0.5 );
  gtk_misc_set_padding( GTK_MISC( label16 ),4,0 );

  HSFontTextScaleadj=GTK_ADJUSTMENT( gtk_adjustment_new( 0,0,100,0.1,0,0 ) );
  HSFontTextScale=gtk_hscale_new( HSFontTextScaleadj );
  gtk_widget_set_name( HSFontTextScale,"HSFontTextScale" );
  gtk_widget_ref( HSFontTextScale );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"HSFontTextScale",HSFontTextScale,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( HSFontTextScale );
  gtk_table_attach( GTK_TABLE( table1 ),HSFontTextScale,1,2,3,4,(GtkAttachOptions)( GTK_EXPAND | GTK_FILL ),(GtkAttachOptions)( 0 ),0,0 );
  gtk_scale_set_value_pos( GTK_SCALE( HSFontTextScale ),GTK_POS_RIGHT );
  gtk_scale_set_digits( GTK_SCALE( HSFontTextScale ),2 );

  label16=gtk_label_new( MSGTR_PREFERENCES_FontOSDScale );
  gtk_widget_set_name( label16,"label16" );
  gtk_widget_ref( label16 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"label16",label16,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( label16 );
  gtk_table_attach( GTK_TABLE( table1 ),label16,0,1,4,5,(GtkAttachOptions)( GTK_FILL ),(GtkAttachOptions)( 0 ),0,0 );
  gtk_misc_set_alignment( GTK_MISC( label16 ),7.45058e-09,0.5 );
  gtk_misc_set_padding( GTK_MISC( label16 ),4,0 );

  HSFontOSDScaleadj=GTK_ADJUSTMENT( gtk_adjustment_new( 0,0,100,0.1,0,0 ) );
  HSFontOSDScale=gtk_hscale_new( HSFontOSDScaleadj );
  gtk_widget_set_name( HSFontOSDScale,"HSFontOSDScale" );
  gtk_widget_ref( HSFontOSDScale );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"HSFontOSDScale",HSFontOSDScale,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( HSFontOSDScale );
  gtk_table_attach( GTK_TABLE( table1 ),HSFontOSDScale,1,2,4,5,(GtkAttachOptions)( GTK_EXPAND | GTK_FILL ),(GtkAttachOptions)( 0 ),0,0 );
  gtk_scale_set_value_pos( GTK_SCALE( HSFontOSDScale ),GTK_POS_RIGHT );
  gtk_scale_set_digits( GTK_SCALE( HSFontOSDScale ),2 );

#endif

  label3=gtk_label_new( MSGTR_PREFERENCES_SubtitleOSD );
  gtk_widget_set_name( label3,"label3" );
  gtk_widget_ref( label3 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"label3",label3,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( label3 );
  gtk_notebook_set_tab_label( GTK_NOTEBOOK( notebook1 ),gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook1 ),2 ),label3 );

  vbox601=gtk_vbox_new( FALSE,0 );
  gtk_widget_set_name( vbox601,"vbox601" );
  gtk_widget_ref( vbox601 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"vbox601",vbox601,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( vbox601 );
  gtk_container_add( GTK_CONTAINER( notebook1 ),vbox601 );

  frame11=gtk_frame_new( MSGTR_PREFERENCES_FRAME_PostProcess );
  gtk_widget_set_name( frame11,"frame11" );
  gtk_widget_ref( frame11 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"frame11",frame11,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame11 );
  gtk_box_pack_start( GTK_BOX( vbox601 ),frame11,FALSE,FALSE,0 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame11 ),GTK_SHADOW_ETCHED_OUT );

  frame=gtk_frame_new( NULL );
  gtk_widget_set_name( frame,"frame" );
  gtk_widget_ref( frame );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"frame",frame,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame );
  gtk_container_add( GTK_CONTAINER( frame11 ),frame );
  gtk_container_set_border_width( GTK_CONTAINER( frame ),0 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame ),GTK_SHADOW_NONE );

  vbox602=gtk_vbox_new( FALSE,0 );
  gtk_widget_set_name( vbox602,"vbox602" );
  gtk_widget_ref( vbox602 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"vbox602",vbox602,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( vbox602 );
  gtk_container_add( GTK_CONTAINER( frame ),vbox602 );

  CBPostprocess=gtk_check_button_new_with_label( MSGTR_PREFERENCES_PostProcess );
  gtk_widget_set_name( CBPostprocess,"CBPostprocess" );
  gtk_widget_ref( CBPostprocess );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"CBPostprocess",CBPostprocess,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( CBPostprocess );
  gtk_box_pack_start( GTK_BOX( vbox602 ),CBPostprocess,FALSE,FALSE,0 );

  hbox5=gtk_hbox_new( FALSE,0 );
  gtk_widget_set_name( hbox5,"hbox5" );
  gtk_widget_ref( hbox5 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"hbox5",hbox5,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( hbox5 );
  gtk_box_pack_start( GTK_BOX( vbox602 ),hbox5,FALSE,FALSE,0 );

  label14=gtk_label_new( MSGTR_PREFERENCES_AutoQuality );
  gtk_widget_set_name( label14,"label14" );
  gtk_widget_ref( label14 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"label14",label14,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( label14 );
  gtk_box_pack_start( GTK_BOX( hbox5 ),label14,FALSE,FALSE,0 );
  gtk_misc_set_alignment( GTK_MISC( label14 ),7.45058e-09,0.5 );
  gtk_misc_set_padding( GTK_MISC( label14 ),4,0 );

  HSPPQualityadj=GTK_ADJUSTMENT( gtk_adjustment_new( 0,0,100,0,0,0 ) );
  HSPPQuality=gtk_hscale_new( HSPPQualityadj );
  gtk_widget_set_name( HSPPQuality,"HSPPQuality" );
  gtk_widget_ref( HSPPQuality );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"HSPPQuality",HSPPQuality,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( HSPPQuality );
  gtk_box_pack_start( GTK_BOX( hbox5 ),HSPPQuality,TRUE,TRUE,0 );
  gtk_scale_set_value_pos( GTK_SCALE( HSPPQuality ),GTK_POS_RIGHT );
  gtk_scale_set_digits( GTK_SCALE( HSPPQuality ),0 );

  frame11=gtk_frame_new( MSGTR_PREFERENCES_FRAME_CodecDemuxer );
  gtk_widget_set_name( frame11,"frame11" );
  gtk_widget_ref( frame11 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"frame11",frame11,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame11 );
  gtk_box_pack_start( GTK_BOX( vbox601 ),frame11,FALSE,FALSE,0 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame11 ),GTK_SHADOW_ETCHED_OUT );

  frame=gtk_frame_new( NULL );
  gtk_widget_set_name( frame,"frame" );
  gtk_widget_ref( frame );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"frame",frame,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame );
  gtk_container_add( GTK_CONTAINER( frame11 ),frame );
  gtk_container_set_border_width( GTK_CONTAINER( frame ),0 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame ),GTK_SHADOW_NONE );

  vbox602=gtk_vbox_new( FALSE,0 );
  gtk_widget_set_name( vbox602,"vbox602" );
  gtk_widget_ref( vbox602 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"vbox602",vbox602,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( vbox602 );
  gtk_container_add( GTK_CONTAINER( frame ),vbox602 );

  CBNonInterlaved=gtk_check_button_new_with_label( MSGTR_PREFERENCES_NI );
  gtk_widget_set_name( CBNonInterlaved,"CBNonInterlaved" );
  gtk_widget_ref( CBNonInterlaved );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"CBNonInterlaved",CBNonInterlaved,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( CBNonInterlaved );
  gtk_box_pack_start( GTK_BOX( vbox602 ),CBNonInterlaved,FALSE,FALSE,0 );

  CBIndex=gtk_check_button_new_with_label( MSGTR_PREFERENCES_IDX );
  gtk_widget_set_name( CBIndex,"CBIndex" );
  gtk_widget_ref( CBIndex );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"CBIndex",CBIndex,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( CBIndex );
  gtk_box_pack_start( GTK_BOX( vbox602 ),CBIndex,FALSE,FALSE,0 );

  hbox5=gtk_hbox_new( FALSE,0 );
  gtk_widget_set_name( hbox5,"hbox5" );
  gtk_widget_ref( hbox5 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"hbox5",hbox5,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( hbox5 );
  gtk_box_pack_start( GTK_BOX( vbox602 ),hbox5,FALSE,FALSE,0 );

  label16=gtk_label_new( MSGTR_PREFERENCES_VideoCodecFamily );
  gtk_widget_set_name( label16,"label16" );
  gtk_widget_ref( label16 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"label16",label16,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( label16 );
  gtk_box_pack_start( GTK_BOX( hbox5 ),label16,FALSE,FALSE,0 );
  gtk_misc_set_alignment( GTK_MISC( label16 ),7.45058e-09,0.5 );
  gtk_misc_set_padding( GTK_MISC( label16 ),4,0 );

  CBVFM=gtk_combo_new();
  gtk_widget_set_name( CBVFM,"CBVFM" );
  gtk_widget_ref( CBVFM );
  gtk_widget_show( CBVFM );
  gtk_box_pack_start( GTK_BOX( hbox5 ),CBVFM,TRUE,TRUE,0 );
  {
   int i;
   for ( i=0;lVFM[i].name;i++ ) CBVFM_items=g_list_append( CBVFM_items,lVFM[i].name );
  }
  gtk_combo_set_popdown_strings( GTK_COMBO( CBVFM ),CBVFM_items );
  g_list_free( CBVFM_items );

  EVFM=GTK_COMBO( CBVFM )->entry;
  gtk_widget_set_name( EVFM,"CEVFM" );
  gtk_entry_set_editable( GTK_ENTRY( EVFM ),FALSE );
  gtk_widget_ref( EVFM );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"EVFM",EVFM,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( EVFM );

  label4=gtk_label_new( "Misc" );
  gtk_widget_set_name( label4,"label4" );
  gtk_widget_ref( label4 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"label4",label4,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( label4 );
  gtk_notebook_set_tab_label( GTK_NOTEBOOK( notebook1 ),gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook1 ),3 ),label4 );

  hseparator1=gtk_hseparator_new();
  gtk_widget_set_name( hseparator1,"hseparator1" );
  gtk_widget_ref( hseparator1 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"hseparator1",hseparator1,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( hseparator1 );
  gtk_box_pack_start( GTK_BOX( vbox1 ),hseparator1,FALSE,FALSE,0 );
  gtk_widget_set_usize( hseparator1,-2,6 );

  hbuttonbox1=gtk_hbutton_box_new();
  gtk_widget_set_name( hbuttonbox1,"hbuttonbox1" );
  gtk_widget_ref( hbuttonbox1 );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"hbuttonbox1",hbuttonbox1,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( hbuttonbox1 );
  gtk_box_pack_start( GTK_BOX( vbox1 ),hbuttonbox1,FALSE,FALSE,0 );
  gtk_button_box_set_layout( GTK_BUTTON_BOX( hbuttonbox1 ),GTK_BUTTONBOX_END );
  gtk_button_box_set_spacing( GTK_BUTTON_BOX( hbuttonbox1 ),10 );
  gtk_button_box_set_child_size( GTK_BUTTON_BOX( hbuttonbox1 ),85,20 );

  BOk=gtk_button_new_with_label( MSGTR_Ok );
  gtk_widget_set_name( BOk,"BOk" );
  gtk_widget_ref( BOk );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"BOk",BOk,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( BOk );
  gtk_container_add( GTK_CONTAINER( hbuttonbox1 ),BOk );

  BCancel=gtk_button_new_with_label( MSGTR_Cancel );
  gtk_widget_set_name( BCancel,"BCancel" );
  gtk_widget_ref( BCancel );
  gtk_object_set_data_full( GTK_OBJECT( Preferences ),"BCancel",BCancel,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( BCancel );
  gtk_container_add( GTK_CONTAINER( hbuttonbox1 ),BCancel );
  
  gtk_widget_add_accelerator( BOk,"released",accel_group,GDK_Return,0,GTK_ACCEL_VISIBLE );
  gtk_widget_add_accelerator( BCancel,"released",accel_group,GDK_Escape,0,GTK_ACCEL_VISIBLE );

  gtk_signal_connect( GTK_OBJECT( Preferences ),"destroy",GTK_SIGNAL_FUNC( prDestroy ),NULL );
  gtk_signal_connect( GTK_OBJECT( Preferences ),"show",GTK_SIGNAL_FUNC( prShow ),(void*)1 );
  gtk_signal_connect( GTK_OBJECT( Preferences ),"hide",GTK_SIGNAL_FUNC( prShow ),(void*)0 );
  
  gtk_signal_connect( GTK_OBJECT( AConfig ),"released",GTK_SIGNAL_FUNC( prButton ),(void*)bAConfig );
  gtk_signal_connect( GTK_OBJECT( BOk ),"released",GTK_SIGNAL_FUNC( prButton ),(void*)bOk );
  gtk_signal_connect( GTK_OBJECT( BCancel ),"released",GTK_SIGNAL_FUNC( prButton ),(void*)bCancel );
  gtk_signal_connect( GTK_OBJECT( VConfig ),"released",GTK_SIGNAL_FUNC( prButton ),(void*)bVconfig );
#if 0
  gtk_signal_connect( GTK_OBJECT( BLoadSubtitle ),"released",GTK_SIGNAL_FUNC( prButton ),(void*)bLSubtitle );
#endif
  gtk_signal_connect( GTK_OBJECT( BLoadFont ),"released",GTK_SIGNAL_FUNC( prButton ),(void*)bLFont );

#if 0
  gtk_signal_connect( GTK_OBJECT( CBNoSound ),"toggled",GTK_SIGNAL_FUNC( on_CBNoSound_toggled ),NULL );
  gtk_signal_connect( GTK_OBJECT( CBNormalize ),"toggled",GTK_SIGNAL_FUNC( on_CBNormalize_toggled ),NULL );
  gtk_signal_connect( GTK_OBJECT( CBSurround ),"toggled",GTK_SIGNAL_FUNC( on_CBSurround_toggled ),NULL );
  gtk_signal_connect( GTK_OBJECT( CBExtraStereo ),"toggled",GTK_SIGNAL_FUNC( on_CBExtraStereo_toggled ),NULL );
  gtk_signal_connect( GTK_OBJECT( CBDoubleBuffer ),"toggled",GTK_SIGNAL_FUNC( on_CBDoubleBuffer_toggled ),NULL );
  gtk_signal_connect( GTK_OBJECT( CBDR ),"toggled",GTK_SIGNAL_FUNC( on_CBDR_toggled ),NULL );
  gtk_signal_connect( GTK_OBJECT( CBFramedrop ),"toggled",GTK_SIGNAL_FUNC( on_CBFramedrop_toggled ),NULL );
  gtk_signal_connect( GTK_OBJECT( CBHFramedrop ),"toggled",GTK_SIGNAL_FUNC( on_CBHFramedrop_toggled ),NULL );
  gtk_signal_connect( GTK_OBJECT( CBFullScreen ),"toggled",GTK_SIGNAL_FUNC( on_CBFullScreen_toggled ),NULL );
  gtk_signal_connect( GTK_OBJECT( CBNonInterlaved ),"toggled",GTK_SIGNAL_FUNC( on_CBNonInterlaved_toggled ),NULL );
  gtk_signal_connect( GTK_OBJECT( CBFlip ),"toggled",GTK_SIGNAL_FUNC( on_CBFlip_toggled ),NULL );
  gtk_signal_connect( GTK_OBJECT( CBPostprocess ),"toggled",GTK_SIGNAL_FUNC( on_CBPostprocess_toggled ),NULL );
  gtk_signal_connect( GTK_OBJECT( CBNoAutoSub ),"toggled",GTK_SIGNAL_FUNC( on_CBNoAutoSub_toggled ),NULL );
  gtk_signal_connect( GTK_OBJECT( CBSubUnicode ),"toggled",GTK_SIGNAL_FUNC( on_CNSubUnicode_toggled ),NULL );
  gtk_signal_connect( GTK_OBJECT( CBDumpMPSub ),"toggled",GTK_SIGNAL_FUNC( on_CBDumpMPSub_toggled ),NULL );
  gtk_signal_connect( GTK_OBJECT( CBDumpSrt ),"toggled",GTK_SIGNAL_FUNC( on_CBDumpSrt_toggled ),NULL );
#endif
#if 0
  gtk_signal_connect( GTK_OBJECT( RBOSDNone ),"toggled",GTK_SIGNAL_FUNC( on_RBOSDNone_toggled ),NULL );
  gtk_signal_connect( GTK_OBJECT( RBOSDTandP ),"toggled",GTK_SIGNAL_FUNC( on_RBOSDTandP_toggled ),NULL );
  gtk_signal_connect( GTK_OBJECT( RBOSDIndicator ),"toggled",GTK_SIGNAL_FUNC( on_RBOSDIndicator_toggled ),NULL );
  gtk_signal_connect( GTK_OBJECT( CBAudioEqualizer ),"toggled",GTK_SIGNAL_FUNC( on_CBAudioEqualizer_toggled ),NULL );
#endif
#if 0
  gtk_signal_connect( GTK_OBJECT( HSAudioDelay ),"motion_notify_event",GTK_SIGNAL_FUNC( on_HSAudioDelay_motion_notify_event ),NULL );
  gtk_signal_connect( GTK_OBJECT( HSPanscan ),"motion_notify_event",GTK_SIGNAL_FUNC( on_HSPanscan_motion_notify_event ),NULL );
  gtk_signal_connect( GTK_OBJECT( label2 ),"motion_notify_event",GTK_SIGNAL_FUNC( on_label2_motion_notify_event ),NULL );
  gtk_signal_connect( GTK_OBJECT( HSSubDelay ),"motion_notify_event",GTK_SIGNAL_FUNC( on_HSSubDelay_motion_notify_event ),NULL );
  gtk_signal_connect( GTK_OBJECT( HSSubPosition ),"motion_notify_event",GTK_SIGNAL_FUNC( on_HSSubPosition_motion_notify_event ),NULL );
  gtk_signal_connect( GTK_OBJECT( HSSubFPS ),"motion_notify_event",GTK_SIGNAL_FUNC( on_HSSubFPS_motion_notify_event ),NULL );
  gtk_signal_connect( GTK_OBJECT( HSFontFactor ),"motion_notify_event",GTK_SIGNAL_FUNC( on_HSFontFactor_motion_notify_event ),NULL );
  gtk_signal_connect( GTK_OBJECT( HSPPQuality ),"motion_notify_event",GTK_SIGNAL_FUNC( on_HSPPQuality_motion_notify_event ),NULL );
#endif

  gtk_notebook_set_page( GTK_NOTEBOOK( notebook1 ),2 );

  gtk_window_add_accel_group( GTK_WINDOW( Preferences ),accel_group );

  return Preferences;
}

#ifdef USE_OSS_AUDIO
       GtkWidget * OSSConfig;
static GtkWidget * CEOssDevice;
static GtkWidget * CEOssMixer;
static GtkWidget * CBOssMixer;
static GtkWidget * CBOssDevice;
static GtkWidget * BOssOk;
static GtkWidget * BOssCancel;

       int         gtkVOSSConfig = 0;

void ShowOSSConfig( void )
{
 if ( gtkVOSSConfig ) gtkActive( OSSConfig );
   else OSSConfig=create_OSSConfig();

 if ( gtkAOOSSMixer ) gtk_entry_set_text( GTK_ENTRY( CEOssMixer ),gtkAOOSSMixer );
   else gtk_entry_set_text( GTK_ENTRY( CEOssMixer ),PATH_DEV_MIXER );
 if ( gtkAOOSSDevice ) gtk_entry_set_text( GTK_ENTRY( CEOssDevice ),gtkAOOSSDevice );
   else gtk_entry_set_text( GTK_ENTRY( CEOssDevice ),PATH_DEV_DSP );

 gtk_widget_show( OSSConfig );
 gtkSetLayer( OSSConfig );
 gtkVOSSConfig=1;
}

void HideOSSConfig( void )
{
 if ( !gtkVOSSConfig ) return;
 gtk_widget_hide( OSSConfig );
 gtk_widget_destroy( OSSConfig ); 
 gtkVOSSConfig=0;
}

static void ossDestroy( GtkObject * object,gpointer user_data )
{ HideOSSConfig(); }

static void ossShow( GtkWidget * widget,gpointer user_data )
{ gtkVOSSConfig=(int)user_data; }

static void ossButton( GtkButton * button,gpointer user_data )
{
 switch( (int)user_data )
  {
   case 1:
        if ( gtkAOOSSMixer ) free( gtkAOOSSMixer );   gtkAOOSSMixer=strdup( gtk_entry_get_text( GTK_ENTRY( CEOssMixer ) ) );
        if ( gtkAOOSSDevice ) free( gtkAOOSSDevice ); gtkAOOSSDevice=strdup( gtk_entry_get_text( GTK_ENTRY( CEOssDevice ) ) );
   case 0:
	HideOSSConfig();
	break;
  }
}

GtkWidget * create_OSSConfig( void )
{
  GList 	* CBOssDevice_items=NULL;
  GList 	* CBOssMixer_items=NULL;
  GtkWidget * frame13;
  GtkWidget * frame14;
  GtkWidget * frame15;
  GtkWidget * frame16;
  GtkWidget * vbox604;
  GtkWidget * table2;
  GtkWidget * label18;
  GtkWidget * label19;
  GtkWidget * hseparator3;
  GtkWidget * hbuttonbox6;
  GtkAccelGroup * accel_group;

  accel_group=gtk_accel_group_new();

  OSSConfig=gtk_window_new( GTK_WINDOW_DIALOG );
  gtk_widget_set_name( OSSConfig,"OSSConfig" );
  gtk_object_set_data( GTK_OBJECT( OSSConfig ),"OSSConfig",OSSConfig );
  gtk_widget_set_usize( OSSConfig,270,92 );
  gtk_window_set_title( GTK_WINDOW( OSSConfig ),MSGTR_OSSPreferences );
  gtk_window_set_position( GTK_WINDOW( OSSConfig ),GTK_WIN_POS_CENTER );
  gtk_window_set_policy( GTK_WINDOW( OSSConfig ),FALSE,FALSE,FALSE );
  gtk_window_set_wmclass( GTK_WINDOW( OSSConfig ),"OSS Config","MPlayer" );

  gtk_widget_realize( OSSConfig );
  gtkAddIcon( OSSConfig );
    
  frame13=gtk_frame_new( NULL );
  gtk_widget_set_name( frame13,"frame13" );
  gtk_widget_ref( frame13 );
  gtk_object_set_data_full( GTK_OBJECT( OSSConfig ),"frame13",frame13,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame13 );
  gtk_container_add( GTK_CONTAINER( OSSConfig ),frame13 );
  gtk_container_set_border_width( GTK_CONTAINER( frame13 ),1 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame13 ),GTK_SHADOW_IN );

  frame14=gtk_frame_new( NULL );
  gtk_widget_set_name( frame14,"frame14" );
  gtk_widget_ref( frame14 );
  gtk_object_set_data_full( GTK_OBJECT( OSSConfig ),"frame14",frame14,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame14 );
  gtk_container_add( GTK_CONTAINER( frame13 ),frame14 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame14 ),GTK_SHADOW_NONE );

  frame15=gtk_frame_new( NULL );
  gtk_widget_set_name( frame15,"frame15" );
  gtk_widget_ref( frame15 );
  gtk_object_set_data_full( GTK_OBJECT( OSSConfig ),"frame15",frame15,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame15 );
  gtk_container_add( GTK_CONTAINER( frame14 ),frame15 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame15 ),GTK_SHADOW_ETCHED_OUT );

  frame16=gtk_frame_new( NULL );
  gtk_widget_set_name( frame16,"frame16" );
  gtk_widget_ref( frame16 );
  gtk_object_set_data_full( GTK_OBJECT( OSSConfig ),"frame16",frame16,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( frame16 );
  gtk_container_add( GTK_CONTAINER( frame15 ),frame16 );
  gtk_widget_set_usize( frame16,384,256 );
  gtk_frame_set_shadow_type( GTK_FRAME( frame16 ),GTK_SHADOW_NONE );

  vbox604=gtk_vbox_new( FALSE,0 );
  gtk_widget_set_name( vbox604,"vbox604" );
  gtk_widget_ref( vbox604 );
  gtk_object_set_data_full( GTK_OBJECT( OSSConfig ),"vbox604",vbox604,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( vbox604 );
  gtk_container_add( GTK_CONTAINER( frame16 ),vbox604 );

  table2=gtk_table_new( 2,2,FALSE );
  gtk_widget_set_name( table2,"table2" );
  gtk_widget_ref( table2 );
  gtk_object_set_data_full( GTK_OBJECT( OSSConfig ),"table2",table2,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( table2 );
  gtk_box_pack_start( GTK_BOX( vbox604 ),table2,TRUE,TRUE,0 );

  label18=gtk_label_new( MSGTR_PREFERENCES_OSS_Device );
  gtk_widget_set_name( label18,"label18" );
  gtk_widget_ref( label18 );
  gtk_object_set_data_full( GTK_OBJECT( OSSConfig ),"label18",label18,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( label18 );
  gtk_table_attach( GTK_TABLE( table2 ),label18,0,1,0,1,(GtkAttachOptions)( GTK_FILL ),(GtkAttachOptions)( 0 ),0,0 );
  gtk_misc_set_alignment( GTK_MISC( label18 ),0,0.5 );
  gtk_misc_set_padding( GTK_MISC( label18 ),4,0 );

  label19=gtk_label_new( MSGTR_PREFERENCES_OSS_Mixer );
  gtk_widget_set_name( label19,"label19" );
  gtk_widget_ref( label19 );
  gtk_object_set_data_full( GTK_OBJECT( OSSConfig ),"label19",label19,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( label19 );
  gtk_table_attach( GTK_TABLE( table2 ),label19,0,1,1,2,(GtkAttachOptions)( GTK_FILL ),(GtkAttachOptions)( 0 ),0,0 );
  gtk_misc_set_alignment( GTK_MISC( label19 ),0,0.5 );
  gtk_misc_set_padding( GTK_MISC( label19 ),4,0 );

  CBOssDevice=gtk_combo_new();
  gtk_widget_set_name( CBOssDevice,"CBOssDevice" );
  gtk_widget_ref( CBOssDevice );
  gtk_object_set_data_full( GTK_OBJECT( OSSConfig ),"CBOssDevice",CBOssDevice,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( CBOssDevice );
  gtk_table_attach( GTK_TABLE( table2 ),CBOssDevice,1,2,0,1,(GtkAttachOptions)( GTK_EXPAND | GTK_FILL ),(GtkAttachOptions)( 0 ),0,0 );
  CBOssDevice_items=g_list_append( CBOssDevice_items,(gpointer)"/dev/dsp" );
  CBOssDevice_items=g_list_append( CBOssDevice_items,(gpointer)"/dev/dsp0" );
  CBOssDevice_items=g_list_append( CBOssDevice_items,(gpointer)"/dev/dsp1" );
  CBOssDevice_items=g_list_append( CBOssDevice_items,(gpointer)"/dev/dsp2" );
  CBOssDevice_items=g_list_append( CBOssDevice_items,(gpointer)"/dev/dsp3" );
  gtk_combo_set_popdown_strings( GTK_COMBO( CBOssDevice ),CBOssDevice_items );
  g_list_free( CBOssDevice_items );

  CEOssDevice=GTK_COMBO( CBOssDevice )->entry;
  gtk_widget_set_name( CEOssDevice,"CEOssDevice" );
  gtk_widget_ref( CEOssDevice );
  gtk_object_set_data_full( GTK_OBJECT( OSSConfig ),"CEOssDevice",CEOssDevice,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( CEOssDevice );
//  gtk_entry_set_text( GTK_ENTRY( CEOssDevice ),"/dev/dsp" );

  CBOssMixer=gtk_combo_new();
  gtk_widget_set_name( CBOssMixer,"CBOssMixer" );
  gtk_widget_ref( CBOssMixer );
  gtk_object_set_data_full( GTK_OBJECT( OSSConfig ),"CBOssMixer",CBOssMixer,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( CBOssMixer );
  gtk_table_attach( GTK_TABLE( table2 ),CBOssMixer,1,2,1,2,(GtkAttachOptions)( GTK_EXPAND | GTK_FILL ),(GtkAttachOptions)( 0 ),0,0 );
  CBOssMixer_items=g_list_append( CBOssMixer_items,(gpointer)"/dev/mixer" );
  CBOssMixer_items=g_list_append( CBOssMixer_items,(gpointer)"/dev/mixer0" );
  CBOssMixer_items=g_list_append( CBOssMixer_items,(gpointer)"/dev/mixer1" );
  CBOssMixer_items=g_list_append( CBOssMixer_items,(gpointer)"/dev/mixer2" );
  CBOssMixer_items=g_list_append( CBOssMixer_items,(gpointer)"/dev/mixer3" );
  gtk_combo_set_popdown_strings( GTK_COMBO( CBOssMixer ),CBOssMixer_items );
  g_list_free( CBOssMixer_items );

  CEOssMixer=GTK_COMBO( CBOssMixer )->entry;
  gtk_widget_set_name( CEOssMixer,"CEOssMixer" );
  gtk_widget_ref( CEOssMixer );
  gtk_object_set_data_full( GTK_OBJECT( OSSConfig ),"CEOssMixer",CEOssMixer,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( CEOssMixer );
//  gtk_entry_set_text( GTK_ENTRY( CEOssMixer ),"/dev/mixer" );

  hseparator3=gtk_hseparator_new();
  gtk_widget_set_name( hseparator3,"hseparator3" );
  gtk_widget_ref( hseparator3 );
  gtk_object_set_data_full( GTK_OBJECT( OSSConfig ),"hseparator3",hseparator3,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( hseparator3 );
  gtk_box_pack_start( GTK_BOX( vbox604 ),hseparator3,FALSE,FALSE,0 );
  gtk_widget_set_usize( hseparator3,-2,8 );

  hbuttonbox6=gtk_hbutton_box_new();
  gtk_widget_set_name( hbuttonbox6,"hbuttonbox6" );
  gtk_widget_ref( hbuttonbox6 );
  gtk_object_set_data_full( GTK_OBJECT( OSSConfig ),"hbuttonbox6",hbuttonbox6,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( hbuttonbox6 );
  gtk_box_pack_start( GTK_BOX( vbox604 ),hbuttonbox6,FALSE,FALSE,0 );
  gtk_button_box_set_layout( GTK_BUTTON_BOX( hbuttonbox6 ),GTK_BUTTONBOX_END );
  gtk_button_box_set_spacing( GTK_BUTTON_BOX( hbuttonbox6 ),10 );
  gtk_button_box_set_child_size( GTK_BUTTON_BOX( hbuttonbox6 ),85,20 );
  gtk_button_box_set_child_ipadding( GTK_BUTTON_BOX( hbuttonbox6 ),0,0 );

  BOssOk=gtk_button_new_with_label( MSGTR_Ok );
  gtk_widget_set_name( BOssOk,"BOssOk" );
  gtk_widget_ref( BOssOk );
  gtk_object_set_data_full( GTK_OBJECT( OSSConfig ),"BOssOk",BOssOk,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( BOssOk );
  gtk_container_add( GTK_CONTAINER( hbuttonbox6 ),BOssOk );
  GTK_WIDGET_UNSET_FLAGS( BOssOk,GTK_CAN_FOCUS );

  BOssCancel=gtk_button_new_with_label( MSGTR_Cancel );
  gtk_widget_set_name( BOssCancel,"BOssCancel" );
  gtk_widget_ref( BOssCancel );
  gtk_object_set_data_full( GTK_OBJECT( OSSConfig ),"BOssCancel",BOssCancel,(GtkDestroyNotify)gtk_widget_unref );
  gtk_widget_show( BOssCancel );
  gtk_container_add( GTK_CONTAINER( hbuttonbox6 ),BOssCancel );
  GTK_WIDGET_UNSET_FLAGS( BOssCancel,GTK_CAN_FOCUS );

  gtk_signal_connect( GTK_OBJECT( OSSConfig ),"destroy",GTK_SIGNAL_FUNC( ossDestroy ),NULL );
  gtk_signal_connect( GTK_OBJECT( OSSConfig ),"hide",GTK_SIGNAL_FUNC( ossShow ),(void*)0 );
  gtk_signal_connect( GTK_OBJECT( OSSConfig ),"show",GTK_SIGNAL_FUNC( ossShow ),(void*)1 );
  
  gtk_signal_connect( GTK_OBJECT( BOssOk ),"released",GTK_SIGNAL_FUNC( ossButton ),(void*)1 );
  gtk_signal_connect( GTK_OBJECT( BOssCancel ),"released",GTK_SIGNAL_FUNC( ossButton ),(void*)0 );

  gtk_widget_add_accelerator( BOssOk,"released",accel_group,GDK_Return,0,GTK_ACCEL_VISIBLE );
  gtk_widget_add_accelerator( BOssCancel,"released",accel_group,GDK_Escape,0,GTK_ACCEL_VISIBLE );

  gtk_window_add_accel_group( GTK_WINDOW( OSSConfig ),accel_group );

  return OSSConfig;
}

#endif

#ifdef HAVE_DXR3
// --- dxr3 config box

static GtkWidget * DXR3Config;
static GtkWidget * CBDevice;
static GtkWidget * CEDXR3Device;
static GtkWidget * RBVNone;
static GtkWidget * RBVLavc;
static GtkWidget * RBVFame;
static GtkWidget * dxr3BOk;
static GtkWidget * dxr3BCancel;

static int gtkVDXR3Config = 0;

GtkWidget * create_DXR3Config( void );

void ShowDXR3Config( void )
{
 if ( gtkVDXR3Config ) gtkActive( DXR3Config );
  else DXR3Config=create_DXR3Config();

 gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( RBVNone ),TRUE );
 if ( gtkVopLAVC ) gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( RBVLavc ),TRUE );
 if ( gtkVopFAME ) gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( RBVFame ),TRUE );
 
 gtk_widget_show( DXR3Config );
 gtkSetLayer( DXR3Config );
 gtkVDXR3Config=1;
}

void HideDXR3Config( void )
{
 if ( !gtkVDXR3Config ) return;
 gtk_widget_hide( DXR3Config );
 gtk_widget_destroy( DXR3Config );
 gtkVDXR3Config=0;
}

static void dxr3Button( GtkButton * button,gpointer user_data )
{
 switch ( (int)user_data )
 {
  case 0: // Ok
       gtkVopLAVC=gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( RBVLavc ) );
       gtkVopFAME=gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( RBVFame ) );
  case 2: // Destroy
  case 1: // Cancel
       HideDXR3Config();
       break;
  case 3: // Show
       gtkVDXR3Config=1;
       break;
  case 4: // Hide
       gtkVDXR3Config=0;
       break;
 }
}

GtkWidget * create_DXR3Config( void )
{
 GtkWidget * frame1;
 GtkWidget * frame2;
 GtkWidget * frame3;
 GtkWidget * frame4;
 GtkWidget * vbox1;
 GtkWidget * vbox2;
 GtkWidget * hbox1;
 GtkWidget * label1;
 GList     * CBDevice_items = NULL;
 GtkWidget * hseparator2;
 GtkWidget * vbox3;
 GtkWidget * label2;
 GSList    * VEncoder_group = NULL;
 GtkWidget * hseparator1;
 GtkWidget * hbuttonbox1;
 GtkAccelGroup * accel_group;

 accel_group=gtk_accel_group_new();

 DXR3Config=gtk_window_new( GTK_WINDOW_DIALOG );
 gtk_widget_set_name( DXR3Config,"DXR3Config" );
 gtk_object_set_data( GTK_OBJECT( DXR3Config ),"DXR3Config",DXR3Config );
 gtk_widget_set_usize( DXR3Config,300,156 );
 GTK_WIDGET_SET_FLAGS( DXR3Config,GTK_CAN_DEFAULT );
 gtk_window_set_title( GTK_WINDOW( DXR3Config ),"DXR3/H+" );
 gtk_window_set_position( GTK_WINDOW( DXR3Config ),GTK_WIN_POS_CENTER );
 gtk_window_set_modal( GTK_WINDOW( DXR3Config ),TRUE );
 gtk_window_set_policy( GTK_WINDOW( DXR3Config ),FALSE,FALSE,FALSE );
 gtk_window_set_wmclass( GTK_WINDOW( DXR3Config ),"DXR3","MPlayer" );

 gtk_widget_realize( DXR3Config );
 gtkAddIcon( DXR3Config );

 frame1=gtk_frame_new( NULL );
 gtk_widget_set_name( frame1,"frame1" );
 gtk_widget_ref( frame1 );
 gtk_object_set_data_full( GTK_OBJECT( DXR3Config ),"frame1",frame1,(GtkDestroyNotify)gtk_widget_unref );
 gtk_widget_show( frame1 );
 gtk_container_add( GTK_CONTAINER( DXR3Config ),frame1 );
 gtk_container_set_border_width( GTK_CONTAINER( frame1 ),1 );
 gtk_frame_set_shadow_type( GTK_FRAME( frame1 ),GTK_SHADOW_IN );

 frame2=gtk_frame_new( NULL );
 gtk_widget_set_name( frame2,"frame2" );
 gtk_widget_ref( frame2 );
 gtk_object_set_data_full( GTK_OBJECT( DXR3Config ),"frame2",frame2,(GtkDestroyNotify)gtk_widget_unref );
 gtk_widget_show( frame2 );
 gtk_container_add( GTK_CONTAINER( frame1 ),frame2 );
 gtk_frame_set_shadow_type( GTK_FRAME( frame2 ),GTK_SHADOW_NONE );

 frame3=gtk_frame_new( NULL );
 gtk_widget_set_name( frame3,"frame3" );
 gtk_widget_ref( frame3 );
 gtk_object_set_data_full( GTK_OBJECT( DXR3Config ),"frame3",frame3,(GtkDestroyNotify)gtk_widget_unref );
 gtk_widget_show( frame3 );
 gtk_container_add( GTK_CONTAINER( frame2 ),frame3 );
 gtk_frame_set_shadow_type( GTK_FRAME( frame3 ),GTK_SHADOW_ETCHED_OUT );

 frame4=gtk_frame_new( NULL );
 gtk_widget_set_name( frame4,"frame4" );
 gtk_widget_ref( frame4 );
 gtk_object_set_data_full( GTK_OBJECT( DXR3Config ),"frame4",frame4,(GtkDestroyNotify)gtk_widget_unref );
 gtk_widget_show( frame4 );
 gtk_container_add( GTK_CONTAINER( frame3 ),frame4 );
 gtk_frame_set_shadow_type( GTK_FRAME( frame4 ),GTK_SHADOW_NONE );

 vbox1=gtk_vbox_new( FALSE,0 );
 gtk_widget_set_name( vbox1,"vbox1" );
 gtk_widget_ref( vbox1 );
 gtk_object_set_data_full( GTK_OBJECT( DXR3Config ),"vbox1",vbox1,(GtkDestroyNotify)gtk_widget_unref );
 gtk_widget_show( vbox1 );
 gtk_container_add( GTK_CONTAINER( frame4 ),vbox1 );

 vbox2=gtk_vbox_new( FALSE,0 );
 gtk_widget_set_name( vbox2,"vbox2" );
 gtk_widget_ref( vbox2 );
 gtk_object_set_data_full( GTK_OBJECT( DXR3Config ),"vbox2",vbox2,(GtkDestroyNotify)gtk_widget_unref );
 gtk_widget_show( vbox2 );
 gtk_box_pack_start( GTK_BOX( vbox1 ),vbox2,TRUE,TRUE,0 );

 hbox1=gtk_hbox_new( FALSE,0 );
 gtk_widget_set_name( hbox1,"hbox1" );
 gtk_widget_ref( hbox1 );
 gtk_object_set_data_full( GTK_OBJECT( DXR3Config ),"hbox1",hbox1,(GtkDestroyNotify)gtk_widget_unref );
 gtk_widget_show( hbox1 );
 gtk_box_pack_start( GTK_BOX( vbox2 ),hbox1,FALSE,FALSE,0 );

 label1=gtk_label_new( MSGTR_PREFERENCES_OSS_Device );
 gtk_widget_set_name( label1,"label1" );
 gtk_widget_ref( label1 );
 gtk_object_set_data_full( GTK_OBJECT( DXR3Config ),"label1",label1,(GtkDestroyNotify)gtk_widget_unref );
 gtk_widget_show( label1 );
 gtk_box_pack_start( GTK_BOX( hbox1 ),label1,FALSE,FALSE,0 );
 gtk_misc_set_alignment( GTK_MISC( label1 ),7.45058e-09,0.5 );
 gtk_misc_set_padding( GTK_MISC( label1 ),4,0 );

 CBDevice=gtk_combo_new();
 gtk_widget_set_name( CBDevice,"CBDevice" );
 gtk_widget_ref( CBDevice );
 gtk_object_set_data_full( GTK_OBJECT( DXR3Config ),"CBDevice",CBDevice,(GtkDestroyNotify)gtk_widget_unref );
 gtk_widget_show( CBDevice );
 gtk_box_pack_start( GTK_BOX( hbox1 ),CBDevice,TRUE,TRUE,0 );
 CBDevice_items=g_list_append( CBDevice_items,( gpointer ) "/dev/em8300" );
 gtk_combo_set_popdown_strings( GTK_COMBO( CBDevice ),CBDevice_items );
 g_list_free( CBDevice_items );

 CEDXR3Device=GTK_COMBO( CBDevice )->entry;
 gtk_widget_set_name( CEDXR3Device,"CEDXR3Device" );
 gtk_widget_ref( CEDXR3Device );
 gtk_object_set_data_full( GTK_OBJECT( DXR3Config ),"CEDXR3Device",CEDXR3Device,(GtkDestroyNotify)gtk_widget_unref );
 gtk_widget_show( CEDXR3Device );
 gtk_entry_set_text( GTK_ENTRY( CEDXR3Device ),"/dev/em8300" );

 hseparator2=gtk_hseparator_new();
 gtk_widget_set_name( hseparator2,"hseparator2" );
 gtk_widget_ref( hseparator2 );
 gtk_object_set_data_full( GTK_OBJECT( DXR3Config ),"hseparator2",hseparator2,(GtkDestroyNotify)gtk_widget_unref );
 gtk_widget_show( hseparator2 );
 gtk_box_pack_start( GTK_BOX( vbox2 ),hseparator2,FALSE,FALSE,0 );
 gtk_widget_set_usize( hseparator2,-2,6 );

 vbox3=gtk_vbox_new( FALSE,0 );
 gtk_widget_set_name( vbox3,"vbox3" );
 gtk_widget_ref( vbox3 );
 gtk_object_set_data_full( GTK_OBJECT( DXR3Config ),"vbox3",vbox3,(GtkDestroyNotify)gtk_widget_unref );
 gtk_widget_show( vbox3 );
 gtk_box_pack_start( GTK_BOX( vbox2 ),vbox3,TRUE,TRUE,0 );

 label2=gtk_label_new( MSGTR_PREFERENCES_DXR3_VENC );
 gtk_widget_set_name( label2,"label2" );
 gtk_widget_ref( label2 );
 gtk_object_set_data_full( GTK_OBJECT( DXR3Config ),"label2",label2,(GtkDestroyNotify)gtk_widget_unref );
 gtk_widget_show( label2 );
 gtk_box_pack_start( GTK_BOX( vbox3 ),label2,FALSE,FALSE,0 );
 gtk_misc_set_alignment( GTK_MISC( label2 ),7.45058e-09,0.5 );
 gtk_misc_set_padding( GTK_MISC( label2 ),4,0 );

 RBVNone=gtk_radio_button_new_with_label( VEncoder_group,MSGTR_PREFERENCES_None );
 VEncoder_group=gtk_radio_button_group( GTK_RADIO_BUTTON( RBVNone ) );
 gtk_widget_set_name( RBVNone,"RBVNone" );
 gtk_widget_ref( RBVNone );
 gtk_object_set_data_full( GTK_OBJECT( DXR3Config ),"RBVNone",RBVNone,(GtkDestroyNotify)gtk_widget_unref );
 gtk_widget_show( RBVNone );
 gtk_box_pack_start( GTK_BOX( vbox3 ),RBVNone,FALSE,FALSE,0 );
 gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( RBVNone ),TRUE );

 RBVLavc=gtk_radio_button_new_with_label( VEncoder_group,MSGTR_PREFERENCES_DXR3_LAVC );
 VEncoder_group=gtk_radio_button_group( GTK_RADIO_BUTTON( RBVLavc ) );
 gtk_widget_set_name( RBVLavc,"RBVLavc" );
 gtk_widget_ref( RBVLavc );
 gtk_object_set_data_full( GTK_OBJECT( DXR3Config ),"RBVLavc",RBVLavc,(GtkDestroyNotify)gtk_widget_unref );
 gtk_widget_show( RBVLavc );
 gtk_box_pack_start( GTK_BOX( vbox3 ),RBVLavc,FALSE,FALSE,0 );

 RBVFame=gtk_radio_button_new_with_label( VEncoder_group,MSGTR_PREFERENCES_DXR3_FAME );
 VEncoder_group=gtk_radio_button_group( GTK_RADIO_BUTTON( RBVFame ) );
 gtk_widget_set_name( RBVFame,"RBVFame" );
 gtk_widget_ref( RBVFame );
 gtk_object_set_data_full( GTK_OBJECT( DXR3Config ),"RBVFame",RBVFame,(GtkDestroyNotify)gtk_widget_unref );
 gtk_widget_show( RBVFame );
 gtk_box_pack_start( GTK_BOX( vbox3 ),RBVFame,FALSE,FALSE,0 );

 hseparator1=gtk_hseparator_new();
 gtk_widget_set_name( hseparator1,"hseparator1" );
 gtk_widget_ref( hseparator1 );
 gtk_object_set_data_full( GTK_OBJECT( DXR3Config ),"hseparator1",hseparator1,(GtkDestroyNotify)gtk_widget_unref );
 gtk_widget_show( hseparator1 );
 gtk_box_pack_start( GTK_BOX( vbox1 ),hseparator1,FALSE,FALSE,0 );
 gtk_widget_set_usize( hseparator1,-2,6 );

 hbuttonbox1=gtk_hbutton_box_new();
 gtk_widget_set_name( hbuttonbox1,"hbuttonbox1" );
 gtk_widget_ref( hbuttonbox1 );
 gtk_object_set_data_full( GTK_OBJECT( DXR3Config ),"hbuttonbox1",hbuttonbox1,(GtkDestroyNotify)gtk_widget_unref );
 gtk_widget_show( hbuttonbox1 );
 gtk_box_pack_start( GTK_BOX( vbox1 ),hbuttonbox1,FALSE,FALSE,0 );
 gtk_button_box_set_layout( GTK_BUTTON_BOX( hbuttonbox1 ),GTK_BUTTONBOX_END );
 gtk_button_box_set_spacing( GTK_BUTTON_BOX( hbuttonbox1 ),10 );
 gtk_button_box_set_child_size( GTK_BUTTON_BOX( hbuttonbox1 ),85,20 );
 gtk_button_box_set_child_ipadding( GTK_BUTTON_BOX( hbuttonbox1 ),0,0 );

 dxr3BOk=gtk_button_new_with_label( MSGTR_Ok );
 gtk_widget_set_name( dxr3BOk,"dxr3BOk" );
 gtk_widget_ref( dxr3BOk );
 gtk_object_set_data_full( GTK_OBJECT( DXR3Config ),"dxr3BOk",dxr3BOk,(GtkDestroyNotify)gtk_widget_unref );
 gtk_widget_show( dxr3BOk );
 gtk_container_add( GTK_CONTAINER( hbuttonbox1 ),dxr3BOk );
// GTK_WIDGET_UNSET_FLAGS( bOk,GTK_CAN_FOCUS );

 dxr3BCancel=gtk_button_new_with_label( MSGTR_Cancel );
 gtk_widget_set_name( dxr3BCancel,"dxr3BCancel" );
 gtk_widget_ref( dxr3BCancel );
 gtk_object_set_data_full( GTK_OBJECT( DXR3Config ),"dxr3BCancel",dxr3BCancel,(GtkDestroyNotify)gtk_widget_unref );
 gtk_widget_show( dxr3BCancel );
 gtk_container_add( GTK_CONTAINER( hbuttonbox1 ),dxr3BCancel );
// GTK_WIDGET_UNSET_FLAGS( bCancel,GTK_CAN_FOCUS );

 gtk_widget_add_accelerator( dxr3BOk,"released",accel_group,GDK_Return,0,GTK_ACCEL_VISIBLE );
 gtk_widget_add_accelerator( dxr3BCancel,"released",accel_group,GDK_Escape,0,GTK_ACCEL_VISIBLE );

 gtk_signal_connect( GTK_OBJECT( DXR3Config ),"destroy",GTK_SIGNAL_FUNC( dxr3Button ),(void *)2 );
 gtk_signal_connect( GTK_OBJECT( DXR3Config ),"show",GTK_SIGNAL_FUNC( dxr3Button ),(void *)3 );
 gtk_signal_connect( GTK_OBJECT( DXR3Config ),"hide",GTK_SIGNAL_FUNC( dxr3Button ),(void *)4 );
 
 gtk_signal_connect( GTK_OBJECT( dxr3BOk ),"released",GTK_SIGNAL_FUNC( dxr3Button ),(void *)0 );
 gtk_signal_connect( GTK_OBJECT( dxr3BCancel ),"released",GTK_SIGNAL_FUNC( dxr3Button ),(void *)1 );

 gtk_window_add_accel_group( GTK_WINDOW( DXR3Config ),accel_group );

 return DXR3Config;
}

#endif


#ifndef __MY_WIDGET
#define __MY_WIDGET

#include <stdio.h>
#include <stdlib.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>

#include "../../config.h"
#include "../../linux/shmem.h"
#include "play.h"
#include "mplayer.h"
#include "../interface.h"

#define GTK_MB_SIMPLE 0
#define GTK_MB_MODAL 1
#define GTK_MB_FATAL 2
#define GTK_MB_ERROR 4
#define GTK_MB_WARNING 8

extern GtkWidget     * SkinBrowser;
extern GtkWidget     * PlayList;
extern GtkWidget     * FileSelect;
extern GtkWidget     * AboutBox;
extern GtkWidget     * Options;
extern GtkWidget     * PopUpMenu;

extern GtkWidget     * MessageBox;

extern GtkWidget     * WarningPixmap;
extern GtkWidget     * ErrorPixmap;

extern GtkWidget     * SkinList;
extern GtkWidget     * gtkMessageBoxText;

extern int             gtkPopupMenu;
extern int             gtkPopupMenuParam;

extern char          * sbMPlayerDirInHome;
extern char          * sbMPlayerPrefixDir;

extern void widgetsCreate( void );

extern void gtkInit( int argc,char* argv[], char *envp[] );
extern void gtkDone( void );

extern int  gtkFillSkinList( gchar * dir );
extern void gtkClearList( GtkWidget * list );
extern void gtkSetDefaultToCList( GtkWidget * list,char * item );
extern int  gtkFindCList( GtkWidget * list,char * item );

extern void gtkEventHandling( void );

extern void gtkShow( int type,char * param );
extern void gtkMessageBox( int type,gchar * str );

#endif

#ifndef __GUI_SB_H
#define __GUI_SB_H

#include <gtk/gtk.h>

extern GtkWidget * SkinList;
extern char      * sbSelectedSkin;
extern char      * sbMPlayerDirInHome;
extern char      * sbMPlayerPrefixDir;

extern void HideSkinBrowser( void );
extern int gtkFillSkinList( gchar * mdir );
extern GtkWidget * create_SkinBrowser( void );

#endif

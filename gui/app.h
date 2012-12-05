/*
 * This file is part of MPlayer.
 *
 * MPlayer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * MPlayer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with MPlayer; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef MPLAYER_GUI_APP_H
#define MPLAYER_GUI_APP_H

#include "util/bitmap.h"
#include "wm/ws.h"

/// GUI messages (user events)
enum {
    evNone,
    evPlay,
    evStop,
    evPause,
    evPrev,
    evNext,
    evLoad,
    evLoadPlay,
    evLoadAudioFile,
    evLoadSubtitle,
    evDropSubtitle,
    evPlaylist,
    evPlayCD,
    evPlayVCD,
    evPlayDVD,
    evLoadURL,
    evPlaySwitchToPause,
    evPauseSwitchToPlay,
    evBackward10sec,
    evForward10sec,
    evBackward1min,
    evForward1min,
    evBackward10min,
    evForward10min,
    evSetMoviePosition,
    evHalfSize,
    evDoubleSize,
    evFullScreen,
    evNormalSize,
    evSetAspect,
    evIncVolume,
    evDecVolume,
    evSetVolume,
    evMute,
    evSetBalance,
    evEqualizer,
    evAbout,
    evPreferences,
    evSkinBrowser,
    evMenu,
    evIconify,
    evExit = 100
};

/// Internal messages (events)
enum {
    ivSetVideo = 1000,
    ivSetAudio,
    ivSetSubtitle,
    ivSetCDTrack,
    ivSetVCDTrack,
    ivSetDVDTitle,
    ivSetDVDChapter,
    ivSetDVDAudio,
    ivSetDVDSubtitle,
    ivPlayDVD,
    ivShowPopUpMenu,
    ivHidePopUpMenu,
    ivRedraw
};

typedef struct {
    int message;
    const char *name;
} evName;

/* Skin items */

#define itNone      0
#define itButton    101
#define itHPotmeter 102
#define itVPotmeter 103
#define itSLabel    104
#define itDLabel    105
#define itBase      106
#define itPotmeter  107
#define itMenu      108

#define itPLMButton (itNone - 1)
#define itPRMButton (itNone - 2)

/* Button states */

#define btnDisabled 0
#define btnReleased 1
#define btnPressed  2

/* Item definition */

#define MAX_ITEMS 64

typedef struct {
    int type;

    int x, y;
    int width, height;

    guiImage Bitmap;
    guiImage Mask;

    int fontid;
    int align;
    char *label;

    int pwidth, pheight;
    int numphases;
    float value;

    int message;

    int R, G, B;

    char *text;
    int textwidth;
    unsigned int starttime;
    int last_x;

    int pressed;
} wItem;

typedef struct {
    wItem main;
    wsTWindow mainWindow;
    int mainDecoration;

    wItem video;
    wsTWindow videoWindow;

    wItem playbar;
    wsTWindow playbarWindow;
    int playbarIsPresent;

    wItem menu;
    wItem menuSelected;
    wsTWindow menuWindow;
    int menuIsPresent;

    int IndexOfMainItems;
    wItem mainItems[MAX_ITEMS];

    int IndexOfPlaybarItems;
    wItem playbarItems[MAX_ITEMS];

    int IndexOfMenuItems;
    wItem menuItems[MAX_ITEMS];
} guiItems;

extern guiItems guiApp;

wItem *appFindItem(int event);
int appFindMessage(const char *name);
void appFreeStruct(void);
void btnModify(int event, float state);
void btnSet(int event, int set);

#endif /* MPLAYER_GUI_APP_H */

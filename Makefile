# LINUX Makefile made by A'rpi / Astral
# Some cleanup by LGB: 	* 'make -C dir' instead of 'cd dir;make;cd..'
#			* for loops instead of linear sequence of make directories
#			* some minor problems with make clean and distclean were corrected
#			* DVD support

include config.mak

PRG_CFG = codec-cfg

LIBAV_INC =
ifeq ($(CONFIG_LIBAVUTIL),yes)
LIBAV_INC += -I./libavutil
endif
ifeq ($(CONFIG_LIBAVCODEC),yes)
LIBAV_INC += -I./libavcodec
endif

# Do not strip the binaries at installation
ifeq ($(STRIPBINARIES),yes)
INSTALLSTRIP = -s
endif

# These subdirectories require installation due to binaries within them.
ifeq ($(VIDIX),yes)
SUBDIRS += libdha vidix
DO_MAKE = @ for i in $(SUBDIRS); do $(MAKE) -C $$i $@; done
endif
ifeq ($(HAVE_LIBCDIO),yes)
CFLAGS += $(LIBCDIO_INC) 
COMMON_LIBS += $(LIBCDIO_LIB)
endif

SRCS_COMMON = asxparser.c \
              codec-cfg.c \
              cpudetect.c \
              edl.c \
              find_sub.c \
              m_config.c \
              m_option.c \
              m_struct.c \
              parser-cfg.c \
              playtree.c \
              playtreeparser.c \
              spudec.c \
              sub_cc.c \
              subreader.c \
              vobsub.c \

SRCS_MENCODER = mencoder.c \
                mp_msg-mencoder.c \
                $(SRCS_COMMON) \
                divx4_vbr.c \
                libvo/aclib.c \
                libvo/font_load.c \
                libvo/font_load_ft.c \
                libvo/osd.c \
                libvo/sub.c \
                parser-mecmd.c \
                xvid_vbr.c \

SRCS_MPLAYER = mplayer.c \
               mp_msg.c \
               $(SRCS_COMMON) \
               mixer.c \
               parser-mpcmd.c \
               subopt-helper.c \

ifeq ($(UNRARLIB),yes)
SRCS_COMMON += unrarlib.c
endif

OBJS_MENCODER = $(SRCS_MENCODER:.c=.o)
OBJS_MPLAYER = $(SRCS_MPLAYER:.c=.o)

VO_LIBS = $(AA_LIB) \
          $(X_LIB) \
          $(SDL_LIB) \
          $(GGI_LIB) \
          $(MP1E_LIB) \
          $(MLIB_LIB) \
          $(SVGA_LIB) \
          $(DIRECTFB_LIB) \
          $(CACA_LIB) \
	  $(VESA_LIB) \

AO_LIBS = $(ARTS_LIB) \
          $(ESD_LIB) \
          $(JACK_LIB) \
          $(NAS_LIB) \
          $(SGIAUDIO_LIB) \
          $(POLYP_LIB) \

CODEC_LIBS = $(AV_LIB) \
             $(FAME_LIB) \
             $(MAD_LIB) \
             $(VORBIS_LIB) \
             $(THEORA_LIB) \
             $(FAAD_LIB) \
             $(LIBLZO_LIB) \
             $(DECORE_LIB) \
             $(XVID_LIB) \
             $(DTS_LIB) \
             $(PNG_LIB) \
             $(Z_LIB) \
             $(JPEG_LIB) \
             $(ALSA_LIB) \
             $(XMMS_LIB) \
             $(X264_LIB) \
             $(MUSEPACK_LIB) \
             $(SPEEX_LIB) \

COMMON_LIBS = libmpcodecs/libmpcodecs.a \
              $(W32_LIB) \
              $(DS_LIB) \
              libaf/libaf.a \
              libmpdemux/libmpdemux.a \
              input/libinput.a \
              postproc/libswscale.a \
              osdep/libosdep.a \
              $(DVDREAD_LIB) \
              $(CODEC_LIBS) \
              $(FREETYPE_LIB) \
              $(TERMCAP_LIB) \
              $(CDPARANOIA_LIB) \
              $(MPLAYER_NETWORK_LIB) \
              $(WIN32_LIB) \
              $(GIF_LIB) \
              $(MACOSX_FRAMEWORKS) \
              $(SMBSUPPORT_LIB) \
              $(FRIBIDI_LIB) \
              $(FONTCONFIG_LIB) \
              $(ENCA_LIB) \

CFLAGS = $(OPTFLAGS) -I. \
         $(CACA_INC) \
         $(CDPARANOIA_INC) \
         $(DVB_INC) \
         $(EXTRA_INC) \
         $(FONTCONFIG_INC) \
         $(FREETYPE_INC) \
         $(FRIBIDI_INC) \
         $(SDL_INC) \
         $(X11_INC) \
         $(XVID_INC) \
         $(LIBAV_INC) \

#CFLAGS += -Wall

ifeq ($(TOOLAME),yes)
CFLAGS += $(TOOLAME_EXTRAFLAGS) 
CODEC_LIBS += $(TOOLAME_LIB)
endif

ifeq ($(TWOLAME),yes)
CODEC_LIBS += $(TWOLAME_LIB)
endif

ifeq ($(FAAC),yes)
CODEC_LIBS += $(FAAC_LIB)
endif

PARTS = libmpdemux \
        libmpcodecs \
        libavutil \
        libavcodec \
        libavformat \
        libao2 \
        osdep \
        postproc \
        input \
        libvo \
        libaf \

ifeq ($(MP3LIB),yes)
PARTS += mp3lib
endif
ifeq ($(LIBA52),yes)
PARTS += liba52
endif
ifeq ($(LIBMPEG2),yes)
PARTS += libmpeg2
endif
ifeq ($(INTERNAL_FAAD),yes)
COMMON_LIBS += libfaad2/libfaad2.a 
PARTS += libfaad2
endif
ifeq ($(VIDIX),yes)
PARTS += libdha vidix
endif
ifeq ($(FAME),yes)
PARTS += libfame
endif
ifeq ($(DVDKIT2),yes)
PARTS += libmpdvdkit2
else
ifeq ($(DVDKIT),yes)
PARTS += libmpdvdkit
endif
endif
ifeq ($(GUI),yes)
PARTS += Gui
endif
ifneq ($(W32_LIB),)
PARTS += loader loader/dshow loader/dmo
endif
ifeq ($(LIBMENU),yes)
PARTS += libmenu
endif
ifeq ($(TREMOR),yes)
PARTS += tremor
endif

ALL_PRG = $(PRG)
ifeq ($(MENCODER),yes)
ALL_PRG += $(PRG_MENCODER)
endif

COMMON_DEPS = $(W32_DEP) \
              $(DS_DEP) \
              $(MP1E_DEP) \
              $(AV_DEP) \
              libmpdemux/libmpdemux.a \
              libmpcodecs/libmpcodecs.a \
              libao2/libao2.a \
              osdep/libosdep.a \
              postproc/libswscale.a \
              input/libinput.a \
              libvo/libvo.a \
              libaf/libaf.a \

ifeq ($(MP3LIB),yes)
COMMON_DEPS += mp3lib/libMP3.a
COMMON_LIBS += mp3lib/libMP3.a
endif
ifeq ($(LIBA52),yes)
COMMON_DEPS += liba52/liba52.a
COMMON_LIBS += liba52/liba52.a
endif
ifeq ($(LIBMPEG2),yes)
COMMON_DEPS += libmpeg2/libmpeg2.a
COMMON_LIBS += libmpeg2/libmpeg2.a
endif
ifeq ($(INTERNAL_FAAD),yes)
COMMON_DEPS += libfaad2/libfaad2.a
endif
ifeq ($(TREMOR),yes)
COMMON_DEPS += tremor/libvorbisidec.a
COMMON_LIBS += tremor/libvorbisidec.a
endif
ifeq ($(VIDIX),yes)
COMMON_DEPS += libdha/libdha.so vidix/libvidix.a
endif
ifeq ($(FAME),yes)
COMMON_DEPS += libfame/libfame.a
endif
ifeq ($(DVDKIT2),yes)
ifeq ($(DVDKIT_SHARED),yes)
COMMON_DEPS += libmpdvdkit2/libmpdvdkit.so
else
COMMON_DEPS += libmpdvdkit2/libmpdvdkit.a
endif
endif

ifeq ($(GUI),yes)
COMMON_DEPS += Gui/libgui.a
GUI_LIBS = Gui/libgui.a
endif

.SUFFIXES: .cc .c .o

#.PHONY: $(COMMON_DEPS)

all:	version.h $(ALL_PRG)

.c.o:
	$(CC) -c $(CFLAGS) -o $@ $<

libaf/libaf.a:
	$(MAKE) -C libaf

libmpdvdkit2/libmpdvdkit.a:
	$(MAKE) -C libmpdvdkit2

libmpdvdkit2/libmpdvdkit.so:
	$(MAKE) -C libmpdvdkit2 libmpdvdkit.so

loader/libloader.a:
	$(MAKE) -C loader

libfame/libfame.a:
	$(MAKE) -C libfame

libmpdemux/libmpdemux.a:
	$(MAKE) -C libmpdemux

libmpcodecs/libmpcodecs.a:
	$(MAKE) -C libmpcodecs

loader/dshow/libDS_Filter.a:
	$(MAKE) -C loader/dshow

loader/dmo/libDMO_Filter.a:
	$(MAKE) -C loader/dmo

libavutil/libavutil.a:
	$(MAKE) -C libavutil LIBPREF=lib LIBSUF=.a

libavcodec/libavcodec.a:
	$(MAKE) -C libavcodec LIBPREF=lib LIBSUF=.a

libavformat/libavformat.a:
	$(MAKE) -C libavformat LIBPREF=lib LIBSUF=.a

libmpeg2/libmpeg2.a:
	$(MAKE) -C libmpeg2

libvo/libvo.a:
	$(MAKE) -C libvo

libao2/libao2.a:
	$(MAKE) -C libao2

liba52/liba52.a:
	$(MAKE) -C liba52

libfaad2/libfaad2.a:
	$(MAKE) -C libfaad2

mp3lib/libMP3.a:
	$(MAKE) -C mp3lib

tremor/libvorbisidec.a:
	$(MAKE) -C tremor

libdha/libdha.so:
	$(MAKE) -C libdha

vidix/libvidix.a: libdha/libdha.so
	$(MAKE) -C vidix

Gui/libgui.a:
	$(MAKE) -C Gui

osdep/libosdep.a:
	$(MAKE) -C osdep

postproc/libswscale.a:
	$(MAKE) -C postproc

input/libinput.a:
	$(MAKE) -C input

libmenu/libmenu.a:
	$(MAKE) -C libmenu

MPLAYER_DEP = $(OBJS_MPLAYER) $(COMMON_DEPS)

ifeq ($(LIBMENU),yes)
MPLAYER_DEP += libmenu/libmenu.a
MENU_LIBS = libmenu/libmenu.a
PARTS += libmenu
else
MENU_LIBS =
endif

MENCODER_DEP = $(OBJS_MENCODER) $(COMMON_DEPS) libmpcodecs/libmpencoders.a

ifeq ($(VIDIX),yes)
VIDIX_LIBS = vidix/libvidix.a
else
VIDIX_LIBS =
endif

ifeq ($(TARGET_WIN32),yes)
OBJS_MPLAYER += osdep/mplayer-rc.o
endif

LIBS_MPLAYER = libvo/libvo.a \
               libao2/libao2.a \
               $(MENU_LIBS) \
               $(VIDIX_LIBS) \
               $(GUI_LIBS) \
               $(COMMON_LIBS) \
               $(GTK_LIBS) \
               $(VO_LIBS) \
               $(AO_LIBS) \
               $(EXTRA_LIB)\
               $(LIRC_LIB) \
               $(LIRCC_LIB) \
               $(STATIC_LIB) \
               $(ARCH_LIB) \
               $(I18NLIBS) \
               $(MATH_LIB) \
               $(LIBC_LIB) \

$(PRG):	$(MPLAYER_DEP)
    ifeq ($(TARGET_WIN32),yes)
	windres -o osdep/mplayer-rc.o osdep/mplayer.rc
    endif
	$(CC) $(CFLAGS) -o $(PRG) $(OBJS_MPLAYER) $(LIBS_MPLAYER)

mplayer.exe.spec.c: libmpcodecs/libmpcodecs.a
	winebuild -fPIC -o mplayer.exe.spec.c -exe mplayer.exe -mcui \
	libmpcodecs/ad_qtaudio.o libmpcodecs/vd_qtvideo.o \
	-L/usr/local/lib/wine -lkernel32

mplayer.exe.so:	$(MPLAYER_DEP) mplayer.exe.spec.c
	$(CC) $(CFLAGS) -Wall -shared  -Wl,-rpath,/usr/local/lib -Wl,-Bsymbolic  -o mplayer.exe.so $(OBJS_MPLAYER) mplayer.exe.spec.c libvo/libvo.a libao2/libao2.a $(MENU_LIBS) $(VIDIX_LIBS) $(GUI_LIBS) $(COMMON_LIBS) $(GTK_LIBS) $(VO_LIBS) $(AO_LIBS) $(EXTRA_LIB) $(LIRC_LIB) $(LIRCC_LIB) $(STATIC_LIB) $(ARCH_LIB) -lwine $(MATH_LIB) 

mplayer_wine.so:	$(MPLAYER_DEP)
	$(CC) $(CFLAGS) -shared -Wl,-Bsymbolic -o mplayer_wine.so mplayer_wine.spec.c $(OBJS_MPLAYER) libvo/libvo.a libao2/libao2.a $(MENU_LIBS) $(VIDIX_LIBS) $(GUI_LIBS) $(COMMON_LIBS) $(GTK_LIBS) $(VO_LIBS) $(AO_LIBS) $(EXTRA_LIB) $(LIRC_LIB) $(LIRCC_LIB) $(STATIC_LIB) -lwine $(ARCH_LIB) $(MATH_LIB)

ifeq ($(MENCODER),yes)
LIBS_MENCODER = libmpcodecs/libmpencoders.a \
                $(ENCORE_LIB) \
                $(COMMON_LIBS) \
                $(EXTRA_LIB) \
                $(MLIB_LIB) \
                $(LIRC_LIB) \
                $(LIRCC_LIB) \
                $(ARCH_LIB) \
                $(I18NLIBS) \
                $(MATH_LIB) \
                $(LIBC_LIB) \

$(PRG_MENCODER): $(MENCODER_DEP)
	$(CC) $(CFLAGS) -o $(PRG_MENCODER) $(OBJS_MENCODER) $(LIBS_MENCODER)
endif

codecs.conf.h: $(PRG_CFG) etc/codecs.conf
	./$(PRG_CFG) ./etc/codecs.conf > $@

codec-cfg.o: codecs.conf.h

# Every mplayer dependency depends on version.h, to force building version.h
# first (in serial mode) before any other of the dependencies for a parallel make
# run.  This is necessary, because the make rule for version.h removes objects
# in a recursive "make distclean" and we must wait for this "make distclean" to
# finish before we can start building new object files.
# help_mp.h is also required by a lot of files, so force generating it early.
$(MPLAYER_DEP): version.h help_mp.h
$(MENCODER_DEP): version.h help_mp.h

$(PRG_CFG): version.h codec-cfg.c codec-cfg.h
	$(HOST_CC) $(HOST_CFLAGS) -I. codec-cfg.c mp_msg.c -o $(PRG_CFG) -DCODECS2HTML $(EXTRA_LIB) $(EXTRA_INC) $(I18NLIBS)

install: $(ALL_PRG)
ifeq ($(VIDIX),yes)
	$(DO_MAKE)
endif
	if test ! -d $(BINDIR) ; then mkdir -p $(BINDIR) ; fi
	$(INSTALL) -m 755 $(INSTALLSTRIP) $(PRG) $(BINDIR)/$(PRG)
ifeq ($(GUI),yes)
	-ln -sf $(PRG) $(BINDIR)/gmplayer
endif
	if test ! -d $(MANDIR)/man1 ; then mkdir -p $(MANDIR)/man1; fi
	for i in $(MAN_LANG); do \
		if test "$$i" = en ; then \
			$(INSTALL) -c -m 644 DOCS/man/en/mplayer.1 $(MANDIR)/man1/mplayer.1 ; \
		else \
			mkdir -p $(MANDIR)/$$i/man1 ; \
			$(INSTALL) -c -m 644 DOCS/man/$$i/mplayer.1 $(MANDIR)/$$i/man1/mplayer.1 ; \
		fi ; \
	done
ifeq ($(MENCODER),yes)
	$(INSTALL) -m 755 $(INSTALLSTRIP) $(PRG_MENCODER) $(BINDIR)/$(PRG_MENCODER)
	for i in $(MAN_LANG); do \
		if test "$$i" = en ; then \
			ln -sf mplayer.1 $(MANDIR)/man1/mencoder.1 ; \
		else \
			ln -sf mplayer.1 $(MANDIR)/$$i/man1/mencoder.1 ; \
		fi ; \
	done
endif
	@if test ! -d $(DATADIR) ; then mkdir -p $(DATADIR) ; fi
	@if test ! -d $(DATADIR)/font ; then mkdir -p $(DATADIR)/font ; fi
	@if test ! -f $(DATADIR)/font/font.desc ; then \
	echo "*** Download font at http://www.mplayerhq.hu/homepage/dload.html" ; \
	echo "*** for OSD/Subtitles support and extract to $(DATADIR)/font/" ; \
	fi
ifeq ($(GUI),yes)
	@if test ! -d $(DATADIR)/Skin ; then mkdir -p $(DATADIR)/Skin ; fi
	@echo "*** Download skin(s) at http://www.mplayerhq.hu/homepage/dload.html"
	@echo "*** for GUI, and extract to $(DATADIR)/Skin/"
	@if test ! -d $(prefix)/share/pixmaps ; then mkdir -p $(prefix)/share/pixmaps ; fi
	$(INSTALL) -m 644 Gui/mplayer/pixmaps/mplayer-desktop.xpm $(prefix)/share/pixmaps/mplayer-desktop.xpm
	@if test ! -d $(prefix)/share/applications ; then mkdir -p $(prefix)/share/applications ; fi
	$(INSTALL) -m 644 etc/mplayer.desktop $(prefix)/share/applications/mplayer.desktop
endif
	@if test ! -d $(CONFDIR) ; then mkdir -p $(CONFDIR) ; fi
	@if test -f $(CONFDIR)/codecs.conf ; then mv -f $(CONFDIR)/codecs.conf $(CONFDIR)/codecs.conf.old ; fi
ifeq ($(DVDKIT_SHARED),yes)
ifeq ($(DVDKIT2),yes)
	if test ! -d $(LIBDIR) ; then mkdir -p $(LIBDIR) ; fi
	$(INSTALL) -m 755 $(INSTALLSTRIP) libmpdvdkit2/libmpdvdkit.so $(LIBDIR)/libmpdvdkit.so
else
ifeq ($(DVDKIT),yes)
	if test ! -d $(LIBDIR) ; then mkdir -p $(LIBDIR) ; fi
	$(INSTALL) -m 755 $(INSTALLSTRIP) libmpdvdkit/libmpdvdkit.so $(LIBDIR)/libmpdvdkit.so
endif
endif
endif

uninstall:
	-rm -f $(BINDIR)/$(PRG) $(BINDIR)/gmplayer $(MANDIR)/man1/mplayer.1
	-rm -f  $(BINDIR)/$(PRG_MENCODER) $(MANDIR)/man1/mencoder.1
	-rm -f $(prefix)/share/pixmaps/mplayer-desktop.xpm
	-rm -f $(prefix)/share/applications/mplayer.desktop
	-rm -f $(LIBDIR)/libmpdvdkit.so
	for l in $(MAN_LANG); do \
	  if test "$$l" != "en"; then \
	    rm -f $(MANDIR)/$$l/man1/mplayer.1    \
	          $(MANDIR)/$$l/man1/mencoder.1   \
	          $(MANDIR)/$$l/man1/gmplayer.1 ; \
	  fi ; \
	done
ifeq ($(VIDIX),yes)
	$(DO_MAKE)
endif
	@echo "Uninstall completed"

clean:
	-rm -f *.o *.a *~ codecs.conf.h

distclean: clean doxygen_clean
	-rm -f *~ $(PRG) $(PRG_MENCODER) $(PRG_CFG)
	-rm -f .depend configure.log codecs.conf.h help_mp.h
	@for a in $(PARTS); do $(MAKE) -C $$a distclean; done

strip:
	strip -s $(ALL_PRG)

dep:	depend

depend: help_mp.h
	./version.sh `$(CC) -dumpversion`
	$(CC) -MM $(CFLAGS) -DCODECS2HTML mplayer.c mencoder.c $(SRCS_MPLAYER) $(SRCS_MENCODER) 1>.depend
	@for a in $(PARTS); do $(MAKE) -C $$a dep; done

# ./configure must be run if it changed in CVS
config.h: configure
	@echo "############################################################"
	@echo "####### Please run ./configure again - it's changed! #######"
	@echo "############################################################"
ifeq ($(wildcard .developer),)
	@exit 1
endif

# do not rebuild after cvs commits if .developer file is present!

# rebuild at every config.h/config.mak change:
version.h:
	./version.sh `$(CC) -dumpversion`
ifeq ($(wildcard .developer),)
	$(MAKE) distclean
endif
	$(MAKE) depend

doxygen:
	doxygen DOCS/tech/Doxyfile

doxygen_clean:
	-rm -rf DOCS/tech/doxygen

help_mp.h: help/help_mp-en.h $(HELP_FILE)
	@echo '// WARNING! This is a generated file. Do NOT edit.' > help_mp.h
	@echo '// See the help/ subdir for the editable files.' >> help_mp.h
ifeq ($(CHARSET),)
	@echo '#include "$(HELP_FILE)"' >> help_mp.h
else
	iconv -f `cat $(HELP_FILE).charset` -t $(CHARSET) "$(HELP_FILE)" >> help_mp.h
endif

ifneq ($(HELP_FILE),help/help_mp-en.h)
	@echo "Adding untranslated messages to help_mp.h"
	@echo '// untranslated messages from the English master file:' >> help_mp.h
	@help/help_diff.sh $(HELP_FILE) < help/help_mp-en.h >> help_mp.h
endif

# rebuild at every CVS update or config/makefile change:
ifeq ($(wildcard .developer),)
ifneq ($(wildcard CVS/Entries),)
version.h: CVS/Entries
endif
version.h: config.h config.mak Makefile
endif

#
# include dependencies to get make to recurse into lib dirs,
# if the user desires such behavior
#
ifneq ($(wildcard .libdeps),)
include .libdeps
endif

#
# include dependency files if they exist
#
ifneq ($(wildcard .depend),)
include .depend
endif

# LINUX Makefile made by A'rpi / Astral
# Some cleanup by LGB: 	* 'make -C dir' instead of 'cd dir;make;cd..'
#			* for loops instead of linear sequence of make directories
#			* some minor problems with make clean and distclean were corrected
#			* DVD support

include config.mak

COMMON_LDFLAGS += $(EXTRA_LIB)\
                  $(EXTRALIBS) \

LDFLAGS_MPLAYER = $(EXTRALIBS_MPLAYER) \
                  $(COMMON_LDFLAGS) \

LDFLAGS_MENCODER = $(EXTRALIBS_MENCODER) \
                   $(COMMON_LDFLAGS) \

SRCS_COMMON = asxparser.c \
              codec-cfg.c \
              cpudetect.c \
              edl.c \
              find_sub.c \
              get_path.c \
              m_config.c \
              m_option.c \
              m_struct.c \
              mpcommon.c \
              parser-cfg.c \
              playtree.c \
              playtreeparser.c \
              spudec.c \
              sub_cc.c \
              subopt-helper.c \
              subreader.c \
              vobsub.c \
              libaf/af.c \
              libaf/af_center.c \
              libaf/af_channels.c \
              libaf/af_comp.c \
              libaf/af_delay.c \
              libaf/af_dummy.c \
              libaf/af_equalizer.c \
              libaf/af_extrastereo.c \
              libaf/af_format.c \
              libaf/af_gate.c \
              libaf/af_hrtf.c \
              libaf/af_karaoke.c \
              libaf/af_pan.c \
              libaf/af_resample.c \
              libaf/af_scaletempo.c \
              libaf/af_sinesuppress.c \
              libaf/af_sub.c \
              libaf/af_surround.c \
              libaf/af_sweep.c \
              libaf/af_tools.c \
              libaf/af_volnorm.c \
              libaf/af_volume.c \
              libaf/filter.c \
              libaf/format.c \
              libaf/reorder_ch.c \
              libaf/window.c \
              osdep/$(GETCH) \
              osdep/$(TIMER) \

SRCS_COMMON-$(HAVE_SYS_MMAN_H)       += osdep/mmap_anon.c
SRCS_COMMON-$(HAVE_SYS_MMAN_H)       += libaf/af_export.c
SRCS_COMMON-$(LADSPA)                += libaf/af_ladspa.c
SRCS_COMMON-$(LIBASS)                += libass/ass.c \
                                        libass/ass_bitmap.c \
                                        libass/ass_cache.c \
                                        libass/ass_font.c \
                                        libass/ass_fontconfig.c \
                                        libass/ass_library.c \
                                        libass/ass_mp.c \
                                        libass/ass_render.c \
                                        libass/ass_utils.c \

SRCS_COMMON-$(LIBAVCODEC)            += libaf/af_lavcresample.c
SRCS_COMMON-$(LIBAVCODEC_A)          += libaf/af_lavcac3enc.c
SRCS_COMMON-$(MACOSX_FINDER_SUPPORT) += osdep/macosx_finder_args.c
SRCS_COMMON-$(NEED_GETTIMEOFDAY)     += osdep/gettimeofday.c
SRCS_COMMON-$(NEED_GLOB)             += osdep/glob-win.c
SRCS_COMMON-$(NEED_MMAP)             += osdep/mmap-os2.c
SRCS_COMMON-$(NEED_SETENV)           += osdep/setenv.c
SRCS_COMMON-$(NEED_SHMEM)            += osdep/shmem.c
SRCS_COMMON-$(NEED_STRSEP)           += osdep/strsep.c
SRCS_COMMON-$(NEED_SWAB)             += osdep/swab.c
SRCS_COMMON-$(NEED_VSSCANF)          += osdep/vsscanf.c
SRCS_COMMON-$(UNRAR_EXEC)            += unrar_exec.c

SRCS_MPLAYER = mplayer.c \
               m_property.c \
               mp_fifo.c \
               mp_msg.c \
               mixer.c \
               parser-mpcmd.c \
               command.c \
               input/input.c \

SRCS_MPLAYER-$(APPLE_REMOTE) += input/ar.c
SRCS_MPLAYER-$(GUI_GTK)      += gui/app.c \
                                gui/bitmap.c \
                                gui/cfg.c \
                                gui/interface.c \
                                gui/mplayer/gui_common.c \
                                gui/mplayer/menu.c \
                                gui/mplayer/mw.c \
                                gui/mplayer/pb.c \
                                gui/mplayer/play.c \
                                gui/mplayer/sw.c \
                                gui/mplayer/widgets.c \
                                gui/mplayer/gtk/about.c \
                                gui/mplayer/gtk/eq.c \
                                gui/mplayer/gtk/fs.c \
                                gui/mplayer/gtk/gtk_common.c \
                                gui/mplayer/gtk/gtk_url.c \
                                gui/mplayer/gtk/mb.c \
                                gui/mplayer/gtk/menu.c \
                                gui/mplayer/gtk/opts.c \
                                gui/mplayer/gtk/pl.c \
                                gui/mplayer/gtk/sb.c \
                                gui/skin/cut.c \
                                gui/skin/font.c \
                                gui/skin/skin.c \
                                gui/wm/ws.c \
                                gui/wm/wsxdnd.c \

SRCS_MPLAYER-$(GUI_WIN32)    += gui/bitmap.c \
                                gui/win32/dialogs.c \
                                gui/win32/gui.c \
                                gui/win32/interface.c \
                                gui/win32/playlist.c \
                                gui/win32/preferences.c \
                                gui/win32/skinload.c \
                                gui/win32/widgetrender.c \
                                gui/win32/wincfg.c \

SRCS_MPLAYER-$(JOYSTICK)     += input/joystick.c
SRCS_MPLAYER-$(LIBMENU)      += libmenu/menu.c \
                                libmenu/menu_chapsel.c \
                                libmenu/menu_cmdlist.c  \
                                libmenu/menu_console.c \
                                libmenu/menu_filesel.c \
                                libmenu/menu_list.c  \
                                libmenu/menu_param.c \
                                libmenu/menu_pt.c \
                                libmenu/menu_txt.c \
                                libmenu/vf_menu.c \

SRCS_MPLAYER-$(LIBMENU_DVBIN) += libmenu/menu_dvbin.c
SRCS_MPLAYER-$(LIRC)         += input/lirc.c

OBJS_MPLAYER-$(PE_EXECUTABLE) += osdep/mplayer-rc.o

SRCS_MENCODER = mencoder.c \
                mp_msg-mencoder.c \
                parser-mecmd.c \
                xvid_vbr.c \

COMMON_LIBS = libmpcodecs/libmpcodecs.a \
              libmpdemux/libmpdemux.a \
              stream/stream.a \
              libswscale/libswscale.a \
              libvo/libosd.a \

COMMON_LIBS-$(LIBAVFORMAT_A)      += libavformat/libavformat.a
COMMON_LIBS-$(LIBAVCODEC_A)       += libavcodec/libavcodec.a
COMMON_LIBS-$(LIBAVUTIL_A)        += libavutil/libavutil.a
COMMON_LIBS-$(LIBPOSTPROC_A)      += libpostproc/libpostproc.a
COMMON_LIBS-$(WIN32DLL)           += loader/libloader.a
COMMON_LIBS-$(MP3LIB)             += mp3lib/libmp3.a
COMMON_LIBS-$(LIBA52)             += liba52/liba52.a
COMMON_LIBS-$(LIBMPEG2)           += libmpeg2/libmpeg2.a
COMMON_LIBS-$(FAAD_INTERNAL)      += libfaad2/libfaad2.a
COMMON_LIBS-$(TREMOR_INTERNAL)    += tremor/libvorbisidec.a
COMMON_LIBS-$(DVDREAD_INTERNAL)   += dvdread/libdvdread.a
COMMON_LIBS-$(DVDCSS_INTERNAL)    += libdvdcss/libdvdcss.a

LIBS_MPLAYER = libvo/libvo.a \
               libao2/libao2.a \

LIBS_MPLAYER-$(VIDIX)             += vidix/libvidix.a

LIBS_MENCODER = libmpcodecs/libmpencoders.a \
                libmpdemux/libmpmux.a \

ALL_PRG-$(MPLAYER)  += mplayer$(EXESUF)
ALL_PRG-$(MENCODER) += mencoder$(EXESUF)

COMMON_LIBS  += $(COMMON_LIBS-yes)
LIBS_MPLAYER += $(LIBS_MPLAYER-yes)
OBJS_MPLAYER += $(OBJS_MPLAYER-yes)
ALL_PRG      += $(ALL_PRG-yes)

MPLAYER_DEPS  = $(OBJS_MPLAYER)  $(OBJS_COMMON) $(LIBS_MPLAYER)  $(COMMON_LIBS)
MENCODER_DEPS = $(OBJS_MENCODER) $(OBJS_COMMON) $(LIBS_MENCODER) $(COMMON_LIBS)

INSTALL_TARGETS-$(MPLAYER)  += install-mplayer  install-mplayer-man
INSTALL_TARGETS-$(MENCODER) += install-mencoder install-mplayer-man
INSTALL_TARGETS-$(GUI)      += install-gui
INSTALL_TARGETS             += $(INSTALL_TARGETS-yes)

PARTS = dvdread \
        liba52 \
        libao2 \
        libavcodec \
        libavformat \
        libavutil \
        libdvdcss \
        libfaad2 \
        libmpcodecs \
        libmpdemux \
        libmpeg2 \
        libpostproc \
        libswscale \
        libvo \
        loader \
        mp3lib \
        stream \
        tremor \
        vidix \

DIRS =  gui \
        gui/mplayer \
        gui/mplayer/gtk \
        gui/skin \
        gui/wm \
        gui/win32 \
        input \
        libaf \
        libass \
        libmenu \
        osdep \

all:	$(ALL_PRG)

DEPS = $(SRCS_COMMON:.c=.d) $(SRCS_MPLAYER:.c=.d) $(SRCS_MENCODER:.c=.d)
$(DEPS): help_mp.h version.h codecs.conf.h
dep depend: $(DEPS)
	for part in $(PARTS); do $(MAKE) -C $$part .depend; done

include mpcommon.mak

CFLAGS := $(subst -I..,-I.,$(CFLAGS))

dvdread/libdvdread.a:
	$(MAKE) -C dvdread

liba52/liba52.a:
	$(MAKE) -C liba52

libao2/libao2.a:
	$(MAKE) -C libao2

libavcodec/libavcodec.a:
	$(MAKE) -C libavcodec

libavformat/libavformat.a:
	$(MAKE) -C libavformat

libavutil/libavutil.a:
	$(MAKE) -C libavutil

libdvdcss/libdvdcss.a:
	$(MAKE) -C libdvdcss

libfaad2/libfaad2.a:
	$(MAKE) -C libfaad2

libmpcodecs/libmpcodecs.a:
	$(MAKE) -C libmpcodecs

libmpdemux/libmpdemux.a:
	$(MAKE) -C libmpdemux libmpdemux.a

libmpdemux/libmpmux.a:
	$(MAKE) -C libmpdemux libmpmux.a

libmpeg2/libmpeg2.a:
	$(MAKE) -C libmpeg2

libpostproc/libpostproc.a:
	$(MAKE) -C libpostproc

libswscale/libswscale.a:
	$(MAKE) -C libswscale

libvo/libvo.a:
	$(MAKE) -C libvo libvo.a

libvo/libosd.a:
	$(MAKE) -C libvo libosd.a

loader/libloader.a:
	$(MAKE) -C loader

mp3lib/libmp3.a:
	$(MAKE) -C mp3lib

stream/stream.a:
	$(MAKE) -C stream

tremor/libvorbisidec.a:
	$(MAKE) -C tremor

vidix/libvidix.a:
	$(MAKE) -C vidix

mplayer$(EXESUF): $(MPLAYER_DEPS)
	$(CC) -o $@ $^ $(LDFLAGS_MPLAYER)

mencoder$(EXESUF): $(MENCODER_DEPS)
	$(CC) -o $@ $^ $(LDFLAGS_MENCODER)

codec-cfg$(EXESUF): codec-cfg.c codec-cfg.h help_mp.h
	$(HOST_CC) -O -I. -DCODECS2HTML $< -o $@

codecs.conf.h: codec-cfg$(EXESUF) etc/codecs.conf
	./codec-cfg$(EXESUF) ./etc/codecs.conf > $@

codec-cfg.o: codecs.conf.h

codecs2html$(EXESUF): mp_msg.o
	$(CC) -DCODECS2HTML codec-cfg.c $^ -o $@

codec-cfg-test$(EXESUF): codecs.conf.h codec-cfg.h mp_msg.o osdep/getch2.o
	$(CC) -I. -DTESTING codec-cfg.c mp_msg.o osdep/getch2.o -ltermcap -o $@

osdep/mplayer-rc.o: osdep/mplayer.rc version.h
	$(WINDRES) -o $@ $<

install: install-dirs $(INSTALL_TARGETS)

install-dirs:
	$(INSTALL) -d $(BINDIR)
	$(INSTALL) -d $(DATADIR)
	$(INSTALL) -d $(MANDIR)/man1
	$(INSTALL) -d $(CONFDIR)
	if test -f $(CONFDIR)/codecs.conf ; then mv -f $(CONFDIR)/codecs.conf $(CONFDIR)/codecs.conf.old ; fi

install-mplayer: mplayer$(EXESUF)
	$(INSTALL) -m 755 $(INSTALLSTRIP) mplayer$(EXESUF) $(BINDIR)

install-mplayer-man:
	for lang in $(MAN_LANG); do \
		if test "$$lang" = en ; then \
			$(INSTALL) -c -m 644 DOCS/man/en/mplayer.1 $(MANDIR)/man1/ ; \
		else \
			$(INSTALL) -d $(MANDIR)/$$lang/man1 ; \
			$(INSTALL) -c -m 644 DOCS/man/$$lang/mplayer.1 $(MANDIR)/$$lang/man1/ ; \
		fi ; \
	done

install-mencoder: mencoder$(EXESUF)
	$(INSTALL) -m 755 $(INSTALLSTRIP) mencoder$(EXESUF) $(BINDIR)
	for lang in $(MAN_LANG); do \
		if test "$$lang" = en ; then \
			cd $(MANDIR)/man1 && ln -sf mplayer.1 mencoder.1 ; \
		else \
			cd $(MANDIR)/$$lang/man1 && ln -sf mplayer.1 mencoder.1 ; \
		fi ; \
	done

install-gui:
	-ln -sf mplayer$(EXESUF) $(BINDIR)/gmplayer$(EXESUF)
	$(INSTALL) -d $(DATADIR)/skins
	@echo "*** Download skin(s) at http://www.mplayerhq.hu/design7/dload.html"
	@echo "*** for GUI, and extract to $(DATADIR)/skins/"
	$(INSTALL) -d $(prefix)/share/pixmaps
	$(INSTALL) -m 644 etc/mplayer.xpm $(prefix)/share/pixmaps/
	$(INSTALL) -d $(prefix)/share/applications
	$(INSTALL) -m 644 etc/mplayer.desktop $(prefix)/share/applications/

uninstall:
	-rm -f $(BINDIR)/mplayer$(EXESUF) $(BINDIR)/gmplayer$(EXESUF)
	-rm -f $(BINDIR)/mencoder$(EXESUF)
	-rm -f $(MANDIR)/man1/mencoder.1 $(MANDIR)/man1/mplayer.1
	-rm -f $(prefix)/share/pixmaps/mplayer.xpm
	-rm -f $(prefix)/share/applications/mplayer.desktop
	for lang in $(MAN_LANG); do \
	  if test "$$lang" != "en"; then \
	    rm -f $(MANDIR)/$$lang/man1/mplayer.1    \
	          $(MANDIR)/$$lang/man1/mencoder.1   \
	          $(MANDIR)/$$lang/man1/gmplayer.1 ; \
	  fi ; \
	done

clean::
	-rm -f mplayer$(EXESUF) mencoder$(EXESUF) codec-cfg$(EXESUF) \
	  codecs2html$(EXESUF) codec-cfg-test$(EXESUF) cpuinfo$(EXESUF) \
	  codecs.conf.h help_mp.h version.h TAGS tags
	for part in $(PARTS); do $(MAKE) -C $$part clean; done
	rm -f $(foreach dir,$(DIRS),$(foreach suffix,/*.o /*.a /*.ho /*~, $(addsuffix $(suffix),$(dir))))

distclean:: doxygen_clean
	for part in $(PARTS); do $(MAKE) -C $$part distclean; done
	$(MAKE) -C TOOLS distclean
	-rm -f configure.log config.mak config.h
	rm -f $(foreach dir,$(DIRS),$(foreach suffix,/*.d, $(addsuffix $(suffix),$(dir))))

strip:
	strip -s $(ALL_PRG)

TAGS:
	rm -f $@; ( find -name '*.[chS]' -print ) | xargs etags -a

tags:
	rm -f $@; ( find -name '*.[chS]' -print ) | xargs ctags -a

# ./configure must be rerun if it changed
config.mak: configure
	@echo "############################################################"
	@echo "####### Please run ./configure again - it's changed! #######"
	@echo "############################################################"

# rebuild at every config.h/config.mak/Makefile change:
version.h: config.h config.mak Makefile
	./version.sh `$(CC) -dumpversion`

doxygen:
	doxygen DOCS/tech/Doxyfile

doxygen_clean:
	-rm -rf DOCS/tech/doxygen

help_mp.h: help/help_mp-en.h $(HELP_FILE)
	@echo '// WARNING! This is a generated file. Do NOT edit.' > help_mp.h
	@echo '// See the help/ subdir for the editable files.' >> help_mp.h
	@echo '#ifndef MPLAYER_HELP_MP_H' >> help_mp.h
	@echo '#define MPLAYER_HELP_MP_H' >> help_mp.h
ifeq ($(CHARSET),)
	@echo '#include "$(HELP_FILE)"' >> help_mp.h
else
	iconv -f UTF-8 -t $(CHARSET) "$(HELP_FILE)" >> help_mp.h
endif
	@echo '#endif /* MPLAYER_HELP_MP_H */' >> help_mp.h

ifneq ($(HELP_FILE),help/help_mp-en.h)
	@echo "Adding untranslated messages to help_mp.h"
	@echo '// untranslated messages from the English master file:' >> help_mp.h
	@help/help_diff.sh $(HELP_FILE) < help/help_mp-en.h >> help_mp.h
endif

.PHONY: all install* uninstall strip doxygen

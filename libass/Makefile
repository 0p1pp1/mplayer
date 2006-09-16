
include ../config.mak

LIBNAME=libass.a

LIBS=$(LIBNAME)

SRCS=ass.c ass_cache.c ass_fontconfig.c ass_render.c ass_utils.c ass_mp.c ass_bitmap.c

OBJS=$(SRCS:.c=.o)

CFLAGS  = -I. -I.. \
          -I../libmpcodecs \
          $(OPTFLAGS) \
          -D_GNU_SOURCE \

.SUFFIXES: .c .o

# .PHONY: all clean

.c.o:
	$(CC) -c $(CFLAGS) -o $@ $<

all:    $(LIBS)

$(LIBNAME):     $(OBJS)
	$(AR) r $(LIBNAME) $(OBJS)
	$(RANLIB) $(LIBNAME)

clean:
	rm -f *.o *.a *~

distclean: clean
	rm -f .depend

dep:    depend

depend:
	$(CC) -MM $(CFLAGS) $(SRCS) 1>.depend

#
# include dependency files if they exist
#
ifneq ($(wildcard .depend),)
include .depend
endif


/*
    libgha.c - Library for direct hardware access
    Copyrights:
    1996/10/27	- Robin Cutshaw (robin@xfree86.org)
		  XFree86 3.3.3 implementation
    1999	- �yvind Aabling.
    		  Modified for GATOS/win/gfxdump.
		  
    2002	- library implementation by Nick Kurshev
		- some changes by Alex Beregszaszi
    
    supported O/S's:	SVR4, UnixWare, SCO, Solaris,
			FreeBSD, NetBSD, 386BSD, BSDI BSD/386,
			Linux, Mach/386, ISC
			DOS (WATCOM 9.5 compiler), Win9x (with mapdev.vxd)
    Licence: GPL
    Original location: www.linuxvideo.org/gatos
*/

#include "libdha.h"
#include "AsmMacros.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

/* instead exit() use libdha_exit, and do the 'mother-application' deinit
   only in this code */
void libdha_exit(const char *message, int level)
{
    printf("libdha: FATAL: %s\n", message);
    exit(level); /* FIXME */
}

#if defined(_WIN32)
#include "sysdep/libdha_win32.c"
#elif defined (__EMX__)
#include "sysdep/libdha_os2.c"
#else

#if defined(SVR4) || defined(SCO325)
#  if !(defined(sun) && defined (i386) && defined (SVR4))
#    define DEV_MEM "/dev/pmem"
#  elif defined(PowerMAX_OS)
#    define DEV_MEM "/dev/iomem"
#  endif
#  ifdef SCO325
#   undef DEV_MEM
#   define DEV_MEM "/dev/mem"
#  endif
# endif /* SVR4 */

/* Generic version */
#include <sys/mman.h>

#ifndef DEV_MEM
#define DEV_MEM "/dev/mem"
#endif

static int mem=-1;
void *map_phys_mem(unsigned base, unsigned size)
{
  if ( (mem = open(DEV_MEM,O_RDWR)) == -1) {
    perror("libdha: open(/dev/mem) failed") ; exit(1) ;
  }
  return mmap(0,size,PROT_READ|PROT_WRITE,MAP_SHARED,mem,base) ;
}

void unmap_phys_mem(void *ptr, unsigned size)
{
  int res=munmap(ptr,size) ;
  if (res == -1) { perror("libdha: munmap() failed") ; exit(1) ; }
  close(mem);
}
#endif

unsigned char  INPORT8(unsigned idx)
{
  return inb(idx);
}

unsigned short INPORT16(unsigned idx)
{
  return inw(idx);
}

unsigned       INPORT32(unsigned idx)
{
  return inl(idx);
}

void          OUTPORT8(unsigned idx,unsigned char val)
{
  outb(idx,val);
}

void          OUTPORT16(unsigned idx,unsigned short val)
{
  outw(idx,val);
}

void          OUTPORT32(unsigned idx,unsigned val)
{
  outl(idx,val);
}


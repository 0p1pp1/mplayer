/*
    libgha.h - Library for direct hardware access
    Copyrights:
    1996/10/27	- Robin Cutshaw (robin@xfree86.org)
		  XFree86 3.3.3 implementation
    1999	- �yvind Aabling.
    		  Modified for GATOS/win/gfxdump.
    2002	- library implementation by Nick Kurshev
    
    supported O/S's:	SVR4, UnixWare, SCO, Solaris,
			FreeBSD, NetBSD, 386BSD, BSDI BSD/386,
			Linux, Mach/386, ISC
			DOS (WATCOM 9.5 compiler), Win9x (with mapdev.vxd)
    Licence: GPL
*/
#ifndef LIBDHA_H
#define LIBDHA_H

#ifdef __cplusplus
extern "C" {
#endif

#define MAX_DEV_PER_VENDOR_CFG1 64
#define MAX_PCI_DEVICES_PER_BUS 32
#define MAX_PCI_DEVICES         64
#define PCI_MULTIFUNC_DEV	0x80

typedef struct pciinfo_s
{
  int	bus,card,func ;			/* PCI/AGP bus:card:func */
  unsigned short vendor,device ;			/* Card vendor+device ID */
  unsigned	base0,base1,base2,baserom ;	/* Memory and I/O base addresses */
}pciinfo_t;

/* needed for mga_vid */
extern int pci_config_read(unsigned char bus, unsigned char dev, unsigned char offset,
			int len, unsigned long *val);
			/* Fill array pci_list which must have size MAX_PCI_DEVICES
			   and return 0 if sucessful */
extern int  pci_scan(pciinfo_t *pci_list,unsigned *num_card);



extern unsigned char  INPORT8(unsigned idx);
extern unsigned short INPORT16(unsigned idx);
extern unsigned       INPORT32(unsigned idx);
#define INPORT(idx) INPORT32(idx)
extern void          OUTPORT8(unsigned idx,unsigned char val);
extern void          OUTPORT16(unsigned idx,unsigned short val);
extern void          OUTPORT32(unsigned idx,unsigned val);
#define OUTPORT(idx,val) OUTPORT32(idx,val)

extern void *  map_phys_mem(unsigned base, unsigned size);
extern void    unmap_phys_mem(void *ptr, unsigned size);

#ifdef __cplusplus
}
#endif

#endif
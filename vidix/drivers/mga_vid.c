/*
    Matrox MGA driver
    
    ported to VIDIX by Alex Beregszaszi

    YUY2 support (see config.format) added by A'rpi/ESP-team
    double buffering added by A'rpi/ESP-team


    DONT USE THIS! IT'S UNDER DEVELOPEMENT! USE MGA_VID KERNELDRIVER!
*/

/*
 * Original comment:
 *
 * mga_vid.c
 *
 * Copyright (C) 1999 Aaron Holtzman
 * 
 * Module skeleton based on gutted agpgart module by Jeff Hartmann 
 * <slicer@ionet.net>
 *
 * Matrox MGA G200/G400 YUV Video Interface module Version 0.1.0
 * 
 * BES == Back End Scaler
 * 
 * This software has been released under the terms of the GNU Public
 * license. See http://www.gnu.org/copyleft/gpl.html for details.
 */


//#define CRTC2

// Set this value, if autodetection fails! (video ram size in megabytes)
// #define MGA_MEMORY_SIZE 16

//#define MGA_VIDMEM_SIZE mga_ram_size

//#define MGA_ALLOW_IRQ
#undef MGA_ALLOW_IRQ

#define MGA_VSYNC_POS 2

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <inttypes.h>

#include "../../libdha/pci_ids.h"
#include "../../libdha/pci_names.h"

#include "../vidix.h"
#include "../fourcc.h"
#include "../../libdha/libdha.h"
#include "mga_vid.h"

#if    !defined(ENOTSUP) && defined(EOPNOTSUPP)
#define ENOTSUP EOPNOTSUPP
#endif

#define TRUE 1
#define FALSE 0

#ifndef PCI_VENDOR_ID_MATROX
#define PCI_VENDOR_ID_MATROX 0x102b
#endif

#ifndef PCI_DEVICE_ID_MATROX_G200_PCI 
#define PCI_DEVICE_ID_MATROX_G200_PCI 0x0520
#endif

#ifndef PCI_DEVICE_ID_MATROX_G200_AGP 
#define PCI_DEVICE_ID_MATROX_G200_AGP 0x0521
#endif

#ifndef PCI_DEVICE_ID_MATROX_G400 
#define PCI_DEVICE_ID_MATROX_G400 0x0525
#endif

#ifndef PCI_DEVICE_ID_MATROX_G550 
#define PCI_DEVICE_ID_MATROX_G550 0x2527
#endif

/* from radeon_vid */
#define GETREG(TYPE,PTR,OFFZ)		(*((volatile TYPE*)((PTR)+(OFFZ))))
#define SETREG(TYPE,PTR,OFFZ,VAL)	(*((volatile TYPE*)((PTR)+(OFFZ))))=VAL

#define readb(addr)		GETREG(uint8_t,(uint32_t)(addr),0)
#define writeb(val,addr)	SETREG(uint8_t,(uint32_t)(addr),0,val)
#define readl(addr)		GETREG(uint32_t,(uint32_t)(addr),0)
#define writel(val,addr)	SETREG(uint32_t,(uint32_t)(addr),0,val)

static pciinfo_t pci_info;
static int probed = 0;

static vidix_ckey_t colorkey;

typedef struct bes_registers_s
{
	//BES Control
	uint32_t besctl;
	//BES Global control
	uint32_t besglobctl;
	//Luma control (brightness and contrast)
	uint32_t beslumactl;
	//Line pitch
	uint32_t bespitch;

	//Buffer A-1 Chroma 3 plane org
	uint32_t besa1c3org;
	//Buffer A-1 Chroma org
	uint32_t besa1corg;
	//Buffer A-1 Luma org
	uint32_t besa1org;

	//Buffer A-2 Chroma 3 plane org
	uint32_t besa2c3org;
	//Buffer A-2 Chroma org
	uint32_t besa2corg;
	//Buffer A-2 Luma org
	uint32_t besa2org;

	//Buffer B-1 Chroma 3 plane org
	uint32_t besb1c3org;
	//Buffer B-1 Chroma org
	uint32_t besb1corg;
	//Buffer B-1 Luma org
	uint32_t besb1org;

	//Buffer B-2 Chroma 3 plane org
	uint32_t besb2c3org;
	//Buffer B-2 Chroma org
	uint32_t besb2corg;
	//Buffer B-2 Luma org
	uint32_t besb2org;

	//BES Horizontal coord
	uint32_t beshcoord;
	//BES Horizontal inverse scaling [5.14]
	uint32_t beshiscal;
	//BES Horizontal source start [10.14] (for scaling)
	uint32_t beshsrcst;
	//BES Horizontal source ending [10.14] (for scaling) 
	uint32_t beshsrcend;
	//BES Horizontal source last 
	uint32_t beshsrclst;

	
	//BES Vertical coord
	uint32_t besvcoord;
	//BES Vertical inverse scaling [5.14]
	uint32_t besviscal;
	//BES Field 1 vertical source last position
	uint32_t besv1srclst;
	//BES Field 1 weight start
	uint32_t besv1wght;
	//BES Field 2 vertical source last position
	uint32_t besv2srclst;
	//BES Field 2 weight start
	uint32_t besv2wght;

} bes_registers_t;

static bes_registers_t regs;
#ifdef CRTC2
typedef struct crtc2_registers_s
{
	uint32_t c2ctl;
	uint32_t c2datactl;
	uint32_t c2misc;
	uint32_t c2hparam;
	uint32_t c2hsync;
	uint32_t c2offset;
	uint32_t c2pl2startadd0;
	uint32_t c2pl2startadd1;
	uint32_t c2pl3startadd0;
	uint32_t c2pl3startadd1;
	uint32_t c2preload;
	uint32_t c2spicstartadd0;
	uint32_t c2spicstartadd1;
	uint32_t c2startadd0;
	uint32_t c2startadd1;
	uint32_t c2subpiclut;
	uint32_t c2vcount;
	uint32_t c2vparam;
	uint32_t c2vsync;
} crtc2_registers_t;
static crtc2_registers_t cregs;
#endif

static uint32_t mga_vid_in_use = 0;
static uint32_t is_g400 = 0;
static uint32_t vid_src_ready = 0;
static uint32_t vid_overlay_on = 0;

static uint8_t *mga_mmio_base = 0;
static uint32_t mga_mem_base = 0; 

static int mga_src_base = 0;	// YUV buffer position in video memory

static uint32_t mga_ram_size = 0;	// how much megabytes videoram we have

//static int mga_force_memsize = 0;

int mga_verbose = 0;

static mga_vid_config_t mga_config; 

static int colkey_saved=0;
static int colkey_on=0;
static unsigned char colkey_color[4];
static unsigned char colkey_mask[4];

static int mga_irq = -1;

static vidix_capability_t mga_cap =
{
    "Matrox MGA G200/G400 YUV Video",
    TYPE_OUTPUT,
    0,
    1,
    0,
    0,
    1024,
    768,
    4,
    4,
    -1,
    FLAG_UPSCALER | FLAG_DOWNSCALER,
    VENDOR_MATROX,
    0,
    { 0, 0, 0, 0}
};

//All register offsets are converted to word aligned offsets (32 bit)
//because we want all our register accesses to be 32 bits
#define VCOUNT      0x1e20

#define PALWTADD      0x3c00 // Index register for X_DATAREG port
#define X_DATAREG     0x3c0a

#define XMULCTRL      0x19
#define BPP_8         0x00
#define BPP_15        0x01
#define BPP_16        0x02
#define BPP_24        0x03
#define BPP_32_DIR    0x04
#define BPP_32_PAL    0x07

#define XCOLMSK       0x40
#define X_COLKEY      0x42
#define XKEYOPMODE    0x51
#define XCOLMSK0RED   0x52
#define XCOLMSK0GREEN 0x53
#define XCOLMSK0BLUE  0x54
#define XCOLKEY0RED   0x55
#define XCOLKEY0GREEN 0x56
#define XCOLKEY0BLUE  0x57

#ifdef CRTC2

/*CRTC2 registers*/
#define XMISCCTRL  0x1e
#define C2CTL       0x3c10 
#define C2DATACTL   0x3c4c
#define C2MISC      0x3c44
#define C2HPARAM    0x3c14
#define C2HSYNC     0x3c18
#define C2OFFSET    0x3c40
#define C2PL2STARTADD0 0x3c30  // like BESA1CORG
#define C2PL2STARTADD1 0x3c34  // like BESA2CORG
#define C2PL3STARTADD0 0x3c38  // like BESA1C3ORG
#define C2PL3STARTADD1 0x3c3c  // like BESA2C3ORG
#define C2PRELOAD   0x3c24
#define C2SPICSTARTADD0 0x3c54
#define C2SPICSTARTADD1 0x3c58
#define C2STARTADD0 0x3c28  // like BESA1ORG
#define C2STARTADD1 0x3c2c  // like BESA2ORG
#define C2SUBPICLUT 0x3c50
#define C2VCOUNT    0x3c48
#define C2VPARAM    0x3c1c
#define C2VSYNC     0x3c20

#endif

// Backend Scaler registers
#define BESCTL      0x3d20
#define BESGLOBCTL  0x3dc0
#define BESLUMACTL  0x3d40
#define BESPITCH    0x3d24

#define BESA1C3ORG  0x3d60
#define BESA1CORG   0x3d10
#define BESA1ORG    0x3d00

#define BESA2C3ORG  0x3d64 
#define BESA2CORG   0x3d14
#define BESA2ORG    0x3d04

#define BESB1C3ORG  0x3d68
#define BESB1CORG   0x3d18
#define BESB1ORG    0x3d08

#define BESB2C3ORG  0x3d6C
#define BESB2CORG   0x3d1C
#define BESB2ORG    0x3d0C

#define BESHCOORD   0x3d28
#define BESHISCAL   0x3d30
#define BESHSRCEND  0x3d3C
#define BESHSRCLST  0x3d50
#define BESHSRCST   0x3d38
#define BESV1WGHT   0x3d48
#define BESV2WGHT   0x3d4c
#define BESV1SRCLST 0x3d54
#define BESV2SRCLST 0x3d58
#define BESVISCAL   0x3d34
#define BESVCOORD   0x3d2c
#define BESSTATUS   0x3dc4

#define CRTCX	    0x1fd4
#define CRTCD	    0x1fd5
#define	IEN	    0x1e1c
#define ICLEAR	    0x1e18
#define STATUS      0x1e14

static int mga_next_frame=0;

#ifdef CRTC2
static void crtc2_frame_sel(int frame)
{
switch(frame) {
case 0:	
	cregs.c2pl2startadd0=regs.besa1corg;
	cregs.c2pl3startadd0=regs.besa1c3org;
	cregs.c2startadd0=regs.besa1org;
	break;
case 1:
	cregs.c2pl2startadd0=regs.besa2corg;
	cregs.c2pl3startadd0=regs.besa2c3org;
	cregs.c2startadd0=regs.besa2org;
	break;
case 2:
	cregs.c2pl2startadd0=regs.besb1corg;
	cregs.c2pl3startadd0=regs.besb1c3org;
	cregs.c2startadd0=regs.besb1org;
	break;
case 3:
	cregs.c2pl2startadd0=regs.besb2corg;
	cregs.c2pl3startadd0=regs.besb2c3org;
	cregs.c2startadd0=regs.besb2org;
	break;
}
	writel(cregs.c2startadd0, mga_mmio_base + C2STARTADD0);
	writel(cregs.c2pl2startadd0, mga_mmio_base + C2PL2STARTADD0);
	writel(cregs.c2pl3startadd0, mga_mmio_base + C2PL3STARTADD0);
}
#endif

int vixPlaybackFrameSelect(unsigned int frame)
{
    if ( mga_irq != -1 ) {
	mga_next_frame=frame;
    } else {

	//we don't need the vcount protection as we're only hitting
	//one register (and it doesn't seem to be double buffered)
	regs.besctl = (regs.besctl & ~0x07000000) + (frame << 25);
	writel( regs.besctl, mga_mmio_base + BESCTL ); 

//	writel( regs.besglobctl + ((readl(mga_mmio_base + VCOUNT)+2)<<16),
	writel( regs.besglobctl + (MGA_VSYNC_POS<<16),
			mga_mmio_base + BESGLOBCTL);
#ifdef CRTC2
	crtc2_frame_sel(frame);
#endif

    }

    return(0);
}


static void mga_vid_write_regs(int restore)
{
	//Make sure internal registers don't get updated until we're done
	writel( (readl(mga_mmio_base + VCOUNT)-1)<<16,
			mga_mmio_base + BESGLOBCTL);

	// color or coordinate keying
	
	if(restore && colkey_saved){
	    // restore it
	    colkey_saved=0;

		printf("[mga] Restoring colorkey (ON: %d  %02X:%02X:%02X)\n",
			colkey_on,colkey_color[0],colkey_color[1],colkey_color[2]);

		// Set color key registers:
		writeb( XKEYOPMODE, mga_mmio_base + PALWTADD);
		writeb( colkey_on, mga_mmio_base + X_DATAREG);
		
		writeb( XCOLKEY0RED, mga_mmio_base + PALWTADD);
		writeb( colkey_color[0], mga_mmio_base + X_DATAREG);
		writeb( XCOLKEY0GREEN, mga_mmio_base + PALWTADD);
		writeb( colkey_color[1], mga_mmio_base + X_DATAREG);
		writeb( XCOLKEY0BLUE, mga_mmio_base + PALWTADD);
		writeb( colkey_color[2], mga_mmio_base + X_DATAREG);
		writeb( X_COLKEY, mga_mmio_base + PALWTADD);
		writeb( colkey_color[3], mga_mmio_base + X_DATAREG);

		writeb( XCOLMSK0RED, mga_mmio_base + PALWTADD);
		writeb( colkey_mask[0], mga_mmio_base + X_DATAREG);
		writeb( XCOLMSK0GREEN, mga_mmio_base + PALWTADD);
		writeb( colkey_mask[1], mga_mmio_base + X_DATAREG);
		writeb( XCOLMSK0BLUE, mga_mmio_base + PALWTADD);
		writeb( colkey_mask[2], mga_mmio_base + X_DATAREG);
		writeb( XCOLMSK, mga_mmio_base + PALWTADD);
		writeb( colkey_mask[3], mga_mmio_base + X_DATAREG);

	} else if(!colkey_saved){
	    // save it
	    colkey_saved=1;
		// Get color key registers:
		writeb( XKEYOPMODE, mga_mmio_base + PALWTADD);
		colkey_on=(unsigned char)readb(mga_mmio_base + X_DATAREG) & 1;
		
		writeb( XCOLKEY0RED, mga_mmio_base + PALWTADD);
		colkey_color[0]=(unsigned char)readb(mga_mmio_base + X_DATAREG);
		writeb( XCOLKEY0GREEN, mga_mmio_base + PALWTADD);
		colkey_color[1]=(unsigned char)readb(mga_mmio_base + X_DATAREG);
		writeb( XCOLKEY0BLUE, mga_mmio_base + PALWTADD);
		colkey_color[2]=(unsigned char)readb(mga_mmio_base + X_DATAREG);
		writeb( X_COLKEY, mga_mmio_base + PALWTADD);
		colkey_color[3]=(unsigned char)readb(mga_mmio_base + X_DATAREG);

		writeb( XCOLMSK0RED, mga_mmio_base + PALWTADD);
		colkey_mask[0]=(unsigned char)readb(mga_mmio_base + X_DATAREG);
		writeb( XCOLMSK0GREEN, mga_mmio_base + PALWTADD);
		colkey_mask[1]=(unsigned char)readb(mga_mmio_base + X_DATAREG);
		writeb( XCOLMSK0BLUE, mga_mmio_base + PALWTADD);
		colkey_mask[2]=(unsigned char)readb(mga_mmio_base + X_DATAREG);
		writeb( XCOLMSK, mga_mmio_base + PALWTADD);
		colkey_mask[3]=(unsigned char)readb(mga_mmio_base + X_DATAREG);

		printf("[mga] Saved colorkey (ON: %d  %02X:%02X:%02X)\n",
			colkey_on,colkey_color[0],colkey_color[1],colkey_color[2]);

	}
	
if(!restore){
	writeb( XKEYOPMODE, mga_mmio_base + PALWTADD);
	writeb( mga_config.colkey_on, mga_mmio_base + X_DATAREG);
	if ( mga_config.colkey_on ) 
	{
		uint32_t r=0, g=0, b=0;

		writeb( XMULCTRL, mga_mmio_base + PALWTADD);
		switch (readb (mga_mmio_base + X_DATAREG)) 
		{
			case BPP_8:
				/* Need to look up the color index, just using
														 color 0 for now. */
			break;

			case BPP_15:
				r = mga_config.colkey_red   >> 3;
				g = mga_config.colkey_green >> 3;
				b = mga_config.colkey_blue  >> 3;
			break;

			case BPP_16:
				r = mga_config.colkey_red   >> 3;
				g = mga_config.colkey_green >> 2;
				b = mga_config.colkey_blue  >> 3;
			break;

			case BPP_24:
			case BPP_32_DIR:
			case BPP_32_PAL:
				r = mga_config.colkey_red;
				g = mga_config.colkey_green;
				b = mga_config.colkey_blue;
			break;
		}

		// Disable color keying on alpha channel 
		writeb( XCOLMSK, mga_mmio_base + PALWTADD);
		writeb( 0x00, mga_mmio_base + X_DATAREG);
		writeb( X_COLKEY, mga_mmio_base + PALWTADD);
		writeb( 0x00, mga_mmio_base + X_DATAREG);


		// Set up color key registers
		writeb( XCOLKEY0RED, mga_mmio_base + PALWTADD);
		writeb( r, mga_mmio_base + X_DATAREG);
		writeb( XCOLKEY0GREEN, mga_mmio_base + PALWTADD);
		writeb( g, mga_mmio_base + X_DATAREG);
		writeb( XCOLKEY0BLUE, mga_mmio_base + PALWTADD);
		writeb( b, mga_mmio_base + X_DATAREG);

		// Set up color key mask registers
		writeb( XCOLMSK0RED, mga_mmio_base + PALWTADD);
		writeb( 0xff, mga_mmio_base + X_DATAREG);
		writeb( XCOLMSK0GREEN, mga_mmio_base + PALWTADD);
		writeb( 0xff, mga_mmio_base + X_DATAREG);
		writeb( XCOLMSK0BLUE, mga_mmio_base + PALWTADD);
		writeb( 0xff, mga_mmio_base + X_DATAREG);
	}

}

	// Backend Scaler
	writel( regs.besctl,      mga_mmio_base + BESCTL); 
	if(is_g400)
		writel( regs.beslumactl,  mga_mmio_base + BESLUMACTL); 
	writel( regs.bespitch,    mga_mmio_base + BESPITCH); 

	writel( regs.besa1org,    mga_mmio_base + BESA1ORG);
	writel( regs.besa1corg,   mga_mmio_base + BESA1CORG);
	writel( regs.besa2org,    mga_mmio_base + BESA2ORG);
	writel( regs.besa2corg,   mga_mmio_base + BESA2CORG);
	writel( regs.besb1org,    mga_mmio_base + BESB1ORG);
	writel( regs.besb1corg,   mga_mmio_base + BESB1CORG);
	writel( regs.besb2org,    mga_mmio_base + BESB2ORG);
	writel( regs.besb2corg,   mga_mmio_base + BESB2CORG);
	if(is_g400) 
	{
		writel( regs.besa1c3org,  mga_mmio_base + BESA1C3ORG);
		writel( regs.besa2c3org,  mga_mmio_base + BESA2C3ORG);
		writel( regs.besb1c3org,  mga_mmio_base + BESB1C3ORG);
		writel( regs.besb2c3org,  mga_mmio_base + BESB2C3ORG);
	}

	writel( regs.beshcoord,   mga_mmio_base + BESHCOORD);
	writel( regs.beshiscal,   mga_mmio_base + BESHISCAL);
	writel( regs.beshsrcst,   mga_mmio_base + BESHSRCST);
	writel( regs.beshsrcend,  mga_mmio_base + BESHSRCEND);
	writel( regs.beshsrclst,  mga_mmio_base + BESHSRCLST);
	
	writel( regs.besvcoord,   mga_mmio_base + BESVCOORD);
	writel( regs.besviscal,   mga_mmio_base + BESVISCAL);

	writel( regs.besv1srclst, mga_mmio_base + BESV1SRCLST);
	writel( regs.besv1wght,   mga_mmio_base + BESV1WGHT);
	writel( regs.besv2srclst, mga_mmio_base + BESV2SRCLST);
	writel( regs.besv2wght,   mga_mmio_base + BESV2WGHT);
	
	//update the registers somewhere between 1 and 2 frames from now.
	writel( regs.besglobctl + ((readl(mga_mmio_base + VCOUNT)+2)<<16),
			mga_mmio_base + BESGLOBCTL);

#if 0
	printf("[mga] wrote BES registers\n");
	printf("[mga] BESCTL = 0x%08x\n",
			readl(mga_mmio_base + BESCTL));
	printf("[mga] BESGLOBCTL = 0x%08x\n",
			readl(mga_mmio_base + BESGLOBCTL));
	printf("[mga] BESSTATUS= 0x%08x\n",
			readl(mga_mmio_base + BESSTATUS));
#endif
#ifdef CRTC2
//	printf("c2ctl:0x%08x c2datactl:0x%08x\n",readl(mga_mmio_base + C2CTL),readl(mga_mmio_base + C2DATACTL));
//	printf("c2misc:0x%08x\n",readl(mga_mmio_base + C2MISC));
//	printf("c2ctl:0x%08x c2datactl:0x%08x\n",cregs.c2ctl,cregs.c2datactl);

//	writel(cregs.c2ctl,	mga_mmio_base + C2CTL);

	writel(((readl(mga_mmio_base + C2CTL) & ~0x03e00000) + (cregs.c2ctl & 0x03e00000)),	mga_mmio_base + C2CTL);
	writel(((readl(mga_mmio_base + C2DATACTL) & ~0x000000ff) + (cregs.c2datactl & 0x000000ff)), mga_mmio_base + C2DATACTL);
	// ctrc2
	// disable CRTC2 acording to specs
//	writel(cregs.c2ctl & 0xfffffff0,	mga_mmio_base + C2CTL);
 // je to treba ???
//	writeb((readb(mga_mmio_base + XMISCCTRL) & 0x19) | 0xa2, mga_mmio_base + XMISCCTRL); // MAFC - mfcsel & vdoutsel
//	writeb((readb(mga_mmio_base + XMISCCTRL) & 0x19) | 0x92, mga_mmio_base + XMISCCTRL);
//	writeb((readb(mga_mmio_base + XMISCCTRL) & ~0xe9) + 0xa2, mga_mmio_base + XMISCCTRL);
//	writel(cregs.c2datactl, mga_mmio_base + C2DATACTL);
//	writel(cregs.c2hparam, mga_mmio_base + C2HPARAM);
//	writel(cregs.c2hsync, mga_mmio_base + C2HSYNC);
//	writel(cregs.c2vparam, mga_mmio_base + C2VPARAM);
//	writel(cregs.c2vsync, mga_mmio_base + C2VSYNC);
	writel(cregs.c2misc, mga_mmio_base + C2MISC);

	printf("c2offset = %d\n",cregs.c2offset);

	writel(cregs.c2offset, mga_mmio_base + C2OFFSET);
	writel(cregs.c2startadd0, mga_mmio_base + C2STARTADD0);
//	writel(cregs.c2startadd1, mga_mmio_base + C2STARTADD1);
	writel(cregs.c2pl2startadd0, mga_mmio_base + C2PL2STARTADD0);
//	writel(cregs.c2pl2startadd1, mga_mmio_base + C2PL2STARTADD1);
	writel(cregs.c2pl3startadd0, mga_mmio_base + C2PL3STARTADD0);
//	writel(cregs.c2pl3startadd1, mga_mmio_base + C2PL3STARTADD1);
	writel(cregs.c2spicstartadd0, mga_mmio_base + C2SPICSTARTADD0);
//	writel(cregs.c2spicstartadd1, mga_mmio_base + C2SPICSTARTADD1);
//	writel(cregs.c2subpiclut, mga_mmio_base + C2SUBPICLUT);
//	writel(cregs.c2preload, mga_mmio_base + C2PRELOAD);
	// finaly enable everything
//	writel(cregs.c2ctl,	mga_mmio_base + C2CTL);
//	printf("c2ctl:0x%08x c2datactl:0x%08x\n",readl(mga_mmio_base + C2CTL),readl(mga_mmio_base + C2DATACTL));
//	printf("c2misc:0x%08x\n", readl(mga_mmio_base + C2MISC));
#endif	
}

#if 0
static int mga_vid_set_config(mga_vid_config_t *config)
{
	int x, y, sw, sh, dw, dh;
	int besleft, bestop, ifactor, ofsleft, ofstop, baseadrofs, weight, weights;
	int frame_size=config->frame_size;
#ifdef CRTC2
#define right_margin 0
#define left_margin 18
#define hsync_len 46
#define lower_margin 10
#define vsync_len 4
#define upper_margin 39

	unsigned int hdispend = (config->src_width + 31) & ~31;
	unsigned int hsyncstart = hdispend + (right_margin & ~7);
	unsigned int hsyncend = hsyncstart + (hsync_len & ~7);
	unsigned int htotal = hsyncend + (left_margin & ~7);
	unsigned int vdispend = config->src_height;
	unsigned int vsyncstart = vdispend + lower_margin;
	unsigned int vsyncend = vsyncstart + vsync_len;
	unsigned int vtotal = vsyncend + upper_margin;
#endif 
    x = config->dest.x;
    y = config->dest.y;
    sw = config->src.width;
    sh = config->src.height;
    dw = config->dest.width;
    dh = config->dest.height;

    printf("[mga] Setting up a %dx%d+%d+%d video window (src %dx%d) format %X\n",
       dw, dh, x, y, sw, sh, config->fourcc);

    if((sw < 4) || (sh < 4) || (dw < 4) || (dh < 4))
    {
        printf("[mga] Invalid src/dest dimenstions\n");
        return(EINVAL);
    }

    //FIXME check that window is valid and inside desktop
	
    //FIXME figure out a better way to allocate memory on card
    //allocate 2 megs
    //mga_src_base = mga_mem_base + (MGA_VIDMEM_SIZE-2) * 0x100000;
    //mga_src_base = (MGA_VIDMEM_SIZE-3) * 0x100000;

    //Setup the BES registers for a three plane 4:2:0 video source 

    regs.besglobctl = 0;

switch(config->fourcc){
    case IMGFMT_YV12:	
    case IMGFMT_I420:	
    case IMGFMT_IYUV:	
	regs.besctl = 1         // BES enabled
                    + (0<<6)    // even start polarity
                    + (1<<10)   // x filtering enabled
                    + (1<<11)   // y filtering enabled
                    + (1<<16)   // chroma upsampling
                    + (1<<17)   // 4:2:0 mode
                    + (1<<18);  // dither enabled
#if 0
	if(is_g400)
	{
		//zoom disabled, zoom filter disabled, 420 3 plane format, proc amp
		//disabled, rgb mode disabled 
		regs.besglobctl = (1<<5);
	}
	else
	{
		//zoom disabled, zoom filter disabled, Cb samples in 0246, Cr
		//in 1357, BES register update on besvcnt
	        regs.besglobctl = 0;
	}
#endif
        break;

    case IMGFMT_YUY2:	
	regs.besctl = 1         // BES enabled
                    + (0<<6)    // even start polarity
                    + (1<<10)   // x filtering enabled
                    + (1<<11)   // y filtering enabled
                    + (1<<16)   // chroma upsampling
                    + (0<<17)   // 4:2:2 mode
                    + (1<<18);  // dither enabled

	regs.besglobctl = 0;        // YUY2 format selected
        break;

    case IMGFMT_UYVY:	
	regs.besctl = 1         // BES enabled
                    + (0<<6)    // even start polarity
                    + (1<<10)   // x filtering enabled
                    + (1<<11)   // y filtering enabled
                    + (1<<16)   // chroma upsampling
                    + (0<<17)   // 4:2:2 mode
                    + (1<<18);  // dither enabled

	regs.besglobctl = 1<<6;        // UYVY format selected
        break;

    default:
	printf("[mga] Unsupported pixel format: %x\n",config->fourcc);
	return(ENOTSUP);
}


	//Disable contrast and brightness control
	regs.besglobctl |= (1<<5) + (1<<7);
	regs.beslumactl = (0x7f << 16) + (0x80<<0);
	regs.beslumactl = 0x80<<0;

	//Setup destination window boundaries
	besleft = x > 0 ? x : 0;
	bestop = y > 0 ? y : 0;
	regs.beshcoord = (besleft<<16) + (x + dw-1);
	regs.besvcoord = (bestop<<16) + (y + dh-1);
	
	//Setup source dimensions
	regs.beshsrclst  = (sw - 1) << 16;
	regs.bespitch = (sw + 31) & ~31 ; 
	
	//Setup horizontal scaling
	ifactor = ((sw-1)<<14)/(dw-1);
	ofsleft = besleft - x;
		
	regs.beshiscal = ifactor<<2;
	regs.beshsrcst = (ofsleft*ifactor)<<2;
	regs.beshsrcend = regs.beshsrcst + (((dw - ofsleft - 1) * ifactor) << 2);
	
	//Setup vertical scaling
	ifactor = ((sh-1)<<14)/(dh-1);
	ofstop = bestop - y;

	regs.besviscal = ifactor<<2;

	baseadrofs = ((ofstop*regs.besviscal)>>16)*regs.bespitch;
	//frame_size = ((sw + 31) & ~31) * sh + (((sw + 31) & ~31) * sh) / 2;
	regs.besa1org = (uint32_t) mga_src_base + baseadrofs;
	regs.besa2org = (uint32_t) mga_src_base + baseadrofs + 1*frame_size;
	regs.besb1org = (uint32_t) mga_src_base + baseadrofs + 2*frame_size;
	regs.besb2org = (uint32_t) mga_src_base + baseadrofs + 3*frame_size;

if(config->format==MGA_VID_FORMAT_YV12
 ||config->format==MGA_VID_FORMAT_IYUV
 ||config->format==MGA_VID_FORMAT_I420
 ){
        // planar YUV frames:
	if (is_g400) 
		baseadrofs = (((ofstop*regs.besviscal)/4)>>16)*regs.bespitch;
	else 
		baseadrofs = (((ofstop*regs.besviscal)/2)>>16)*regs.bespitch;

    if(config->format==MGA_VID_FORMAT_YV12){
	regs.besa1corg = (uint32_t) mga_src_base + baseadrofs + regs.bespitch * sh ;
	regs.besa2corg = (uint32_t) mga_src_base + baseadrofs + 1*frame_size + regs.bespitch * sh;
	regs.besb1corg = (uint32_t) mga_src_base + baseadrofs + 2*frame_size + regs.bespitch * sh;
	regs.besb2corg = (uint32_t) mga_src_base + baseadrofs + 3*frame_size + regs.bespitch * sh;
	regs.besa1c3org = regs.besa1corg + ((regs.bespitch * sh) / 4);
	regs.besa2c3org = regs.besa2corg + ((regs.bespitch * sh) / 4);
	regs.besb1c3org = regs.besb1corg + ((regs.bespitch * sh) / 4);
	regs.besb2c3org = regs.besb2corg + ((regs.bespitch * sh) / 4);
    } else {
	regs.besa1c3org = (uint32_t) mga_src_base + baseadrofs + regs.bespitch * sh ;
	regs.besa2c3org = (uint32_t) mga_src_base + baseadrofs + 1*frame_size + regs.bespitch * sh;
	regs.besb1c3org = (uint32_t) mga_src_base + baseadrofs + 2*frame_size + regs.bespitch * sh;
	regs.besb2c3org = (uint32_t) mga_src_base + baseadrofs + 3*frame_size + regs.bespitch * sh;
	regs.besa1corg = regs.besa1c3org + ((regs.bespitch * sh) / 4);
	regs.besa2corg = regs.besa2c3org + ((regs.bespitch * sh) / 4);
	regs.besb1corg = regs.besb1c3org + ((regs.bespitch * sh) / 4);
	regs.besb2corg = regs.besb2c3org + ((regs.bespitch * sh) / 4);
    }

}

	weight = ofstop * (regs.besviscal >> 2);
	weights = weight < 0 ? 1 : 0;
	regs.besv2wght = regs.besv1wght = (weights << 16) + ((weight & 0x3FFF) << 2);
	regs.besv2srclst = regs.besv1srclst = sh - 1 - (((ofstop * regs.besviscal) >> 16) & 0x03FF);

#ifdef CRTC2
	// pridat hlavni registry - tj. casovani ...


switch(config->format){
    case MGA_VID_FORMAT_YV12:	
    case MGA_VID_FORMAT_I420:	
    case MGA_VID_FORMAT_IYUV:	
	cregs.c2ctl = 1         // CRTC2 enabled
		    + (1<<1)	// external clock
		    + (0<<2)	// external clock
		    + (1<<3)	// pixel clock enable - not needed ???
		    + (0<<4)	// high prioryty req
		    + (1<<5)	// high prioryty req
		    + (0<<6)	// high prioryty req
		    + (1<<8)	// high prioryty req max
		    + (0<<9)	// high prioryty req max
		    + (0<<10)	// high prioryty req max
                    + (0<<20)   // CRTC1 to DAC
                    + (1<<21)   // 420 mode
                    + (1<<22)   // 420 mode
                    + (1<<23)   // 420 mode
                    + (0<<24)   // single chroma line for 420 mode - need to be corrected
                    + (0<<25)   /*/ interlace mode - need to be corrected*/
                    + (0<<26)   // field legth polariry
                    + (0<<27)   // field identification polariry
                    + (1<<28)   // VIDRST detection mode
                    + (0<<29)   // VIDRST detection mode
                    + (1<<30)   // Horizontal counter preload
                    + (1<<31)   // Vertical counter preload
		    ;
	cregs.c2datactl = 1         // disable dither - propably not needed, we are already in YUV mode
		    + (1<<1)	// Y filter enable
		    + (1<<2)	// CbCr filter enable
		    + (0<<3)	// subpicture enable (disabled)
		    + (0<<4)	// NTSC enable (disabled - PAL)
		    + (0<<5)	// C2 static subpicture enable (disabled)
		    + (0<<6)	// C2 subpicture offset division (disabled)
		    + (0<<7)	// 422 subformat selection !
/*		    + (0<<8)	// 15 bpp high alpha
		    + (0<<9)	// 15 bpp high alpha
		    + (0<<10)	// 15 bpp high alpha
		    + (0<<11)	// 15 bpp high alpha
		    + (0<<12)	// 15 bpp high alpha
		    + (0<<13)	// 15 bpp high alpha
		    + (0<<14)	// 15 bpp high alpha
		    + (0<<15)	// 15 bpp high alpha
		    + (0<<16)	// 15 bpp low alpha
		    + (0<<17)	// 15 bpp low alpha
		    + (0<<18)	// 15 bpp low alpha
		    + (0<<19)	// 15 bpp low alpha
		    + (0<<20)	// 15 bpp low alpha
		    + (0<<21)	// 15 bpp low alpha
		    + (0<<22)	// 15 bpp low alpha
		    + (0<<23)	// 15 bpp low alpha
		    + (0<<24)	// static subpicture key
		    + (0<<25)	// static subpicture key
		    + (0<<26)	// static subpicture key
		    + (0<<27)	// static subpicture key
		    + (0<<28)	// static subpicture key
*/		    ;
        break;

    case MGA_VID_FORMAT_YUY2:	
	cregs.c2ctl = 1         // CRTC2 enabled
		    + (1<<1)	// external clock
		    + (0<<2)	// external clock
		    + (1<<3)	// pixel clock enable - not needed ???
		    + (0<<4)	// high prioryty req - acc to spec
		    + (1<<5)	// high prioryty req
		    + (0<<6)	// high prioryty req
				// 7 reserved
		    + (1<<8)	// high prioryty req max
		    + (0<<9)	// high prioryty req max
		    + (0<<10)	// high prioryty req max
				// 11-19 reserved
                    + (0<<20)   // CRTC1 to DAC
                    + (1<<21)   // 422 mode
                    + (0<<22)   // 422 mode
                    + (1<<23)   // 422 mode
                    + (0<<24)   // single chroma line for 420 mode - need to be corrected
                    + (0<<25)   /*/ interlace mode - need to be corrected*/
                    + (0<<26)   // field legth polariry
                    + (0<<27)   // field identification polariry
                    + (1<<28)   // VIDRST detection mode
                    + (0<<29)   // VIDRST detection mode
                    + (1<<30)   // Horizontal counter preload
                    + (1<<31)   // Vertical counter preload
		    ;
	cregs.c2datactl = 1         // disable dither - propably not needed, we are already in YUV mode
		    + (1<<1)	// Y filter enable
		    + (1<<2)	// CbCr filter enable
		    + (0<<3)	// subpicture enable (disabled)
		    + (0<<4)	// NTSC enable (disabled - PAL)
		    + (0<<5)	// C2 static subpicture enable (disabled)
		    + (0<<6)	// C2 subpicture offset division (disabled)
		    + (0<<7)	// 422 subformat selection !
/*		    + (0<<8)	// 15 bpp high alpha
		    + (0<<9)	// 15 bpp high alpha
		    + (0<<10)	// 15 bpp high alpha
		    + (0<<11)	// 15 bpp high alpha
		    + (0<<12)	// 15 bpp high alpha
		    + (0<<13)	// 15 bpp high alpha
		    + (0<<14)	// 15 bpp high alpha
		    + (0<<15)	// 15 bpp high alpha
		    + (0<<16)	// 15 bpp low alpha
		    + (0<<17)	// 15 bpp low alpha
		    + (0<<18)	// 15 bpp low alpha
		    + (0<<19)	// 15 bpp low alpha
		    + (0<<20)	// 15 bpp low alpha
		    + (0<<21)	// 15 bpp low alpha
		    + (0<<22)	// 15 bpp low alpha
		    + (0<<23)	// 15 bpp low alpha
		    + (0<<24)	// static subpicture key
		    + (0<<25)	// static subpicture key
		    + (0<<26)	// static subpicture key
		    + (0<<27)	// static subpicture key
		    + (0<<28)	// static subpicture key
*/			;
          break;

    case MGA_VID_FORMAT_UYVY:	
	cregs.c2ctl = 1         // CRTC2 enabled
		    + (1<<1)	// external clock
		    + (0<<2)	// external clock
		    + (1<<3)	// pixel clock enable - not needed ???
		    + (0<<4)	// high prioryty req
		    + (1<<5)	// high prioryty req
		    + (0<<6)	// high prioryty req
		    + (1<<8)	// high prioryty req max
		    + (0<<9)	// high prioryty req max
		    + (0<<10)	// high prioryty req max
                    + (0<<20)   // CRTC1 to DAC
                    + (1<<21)   // 422 mode
                    + (0<<22)   // 422 mode
                    + (1<<23)   // 422 mode
                    + (1<<24)   // single chroma line for 420 mode - need to be corrected
                    + (1<<25)   /*/ interlace mode - need to be corrected*/
                    + (0<<26)   // field legth polariry
                    + (0<<27)   // field identification polariry
                    + (1<<28)   // VIDRST detection mode
                    + (0<<29)   // VIDRST detection mode
                    + (1<<30)   // Horizontal counter preload
                    + (1<<31)   // Vertical counter preload
		    ;
	cregs.c2datactl = 0         // enable dither - propably not needed, we are already in YUV mode
		    + (1<<1)	// Y filter enable
		    + (1<<2)	// CbCr filter enable
		    + (0<<3)	// subpicture enable (disabled)
		    + (0<<4)	// NTSC enable (disabled - PAL)
		    + (0<<5)	// C2 static subpicture enable (disabled)
		    + (0<<6)	// C2 subpicture offset division (disabled)
		    + (1<<7)	// 422 subformat selection !
/*		    + (0<<8)	// 15 bpp high alpha
		    + (0<<9)	// 15 bpp high alpha
		    + (0<<10)	// 15 bpp high alpha
		    + (0<<11)	// 15 bpp high alpha
		    + (0<<12)	// 15 bpp high alpha
		    + (0<<13)	// 15 bpp high alpha
		    + (0<<14)	// 15 bpp high alpha
		    + (0<<15)	// 15 bpp high alpha
		    + (0<<16)	// 15 bpp low alpha
		    + (0<<17)	// 15 bpp low alpha
		    + (0<<18)	// 15 bpp low alpha
		    + (0<<19)	// 15 bpp low alpha
		    + (0<<20)	// 15 bpp low alpha
		    + (0<<21)	// 15 bpp low alpha
		    + (0<<22)	// 15 bpp low alpha
		    + (0<<23)	// 15 bpp low alpha
		    + (0<<24)	// static subpicture key
		    + (0<<25)	// static subpicture key
		    + (0<<26)	// static subpicture key
		    + (0<<27)	// static subpicture key
		    + (0<<28)	// static subpicture key
*/		    ;
        break;

    default:
	printf("[mga] Unsupported pixel format: 0x%X\n",config->format);
	return -1;
    }

	cregs.c2hparam=((hdispend - 8) << 16) | (htotal - 8);
	cregs.c2hsync=((hsyncend - 8) << 16) | (hsyncstart - 8);
	
	cregs.c2misc=0	// CRTCV2 656 togg f0
		    +(0<<1) // CRTCV2 656 togg f0
		    +(0<<2) // CRTCV2 656 togg f0
		    +(0<<4) // CRTCV2 656 togg f1
		    +(0<<5) // CRTCV2 656 togg f1
		    +(0<<6) // CRTCV2 656 togg f1
		    +(0<<8) // Hsync active high
		    +(0<<9) // Vsync active high
		    // 16-27 c2vlinecomp - nevim co tam dat
		    ;
	cregs.c2offset=(regs.bespitch << 1);

	cregs.c2pl2startadd0=regs.besa1corg;
//	cregs.c2pl2startadd1=regs.besa2corg;
	cregs.c2pl3startadd0=regs.besa1c3org;
//	cregs.c2pl3startadd1=regs.besa2c3org;
		    
	cregs.c2preload=(vsyncstart << 16) | (hsyncstart); // from 
	
	cregs.c2spicstartadd0=0; // not used
//	cregs.c2spicstartadd1=0; // not used
	
	cregs.c2startadd0=regs.besa1org;
//	cregs.c2startadd1=regs.besa2org;
	
	cregs.c2subpiclut=0; //not used
	
	cregs.c2vparam=((vdispend - 1) << 16) | (vtotal - 1);
	cregs.c2vsync=((vsyncend - 1) << 16) | (vsyncstart - 1);

	
#endif

	mga_vid_write_regs(0);
	return 0;
}
#endif

#ifdef MGA_ALLOW_IRQ

static void enable_irq(){
	long int cc;

	cc = readl(mga_mmio_base + IEN);
//	printf("*** !!! IRQREG = %d\n", (int)(cc&0xff));

	writeb( 0x11, mga_mmio_base + CRTCX);
	
	writeb(0x20, mga_mmio_base + CRTCD );  /* clear 0, enable off */
	writeb(0x00, mga_mmio_base + CRTCD );  /* enable on */
	writeb(0x10, mga_mmio_base + CRTCD );  /* clear = 1 */
	
	writel( regs.besglobctl , mga_mmio_base + BESGLOBCTL);

}

static void disable_irq(){

	writeb( 0x11, mga_mmio_base + CRTCX);
	writeb(0x20, mga_mmio_base + CRTCD );  /* clear 0, enable off */

}

void mga_handle_irq(int irq, void *dev_id/*, struct pt_regs *pregs*/) {
//	static int frame=0;
//	static int counter=0;
	long int cc;
//	if ( ! mga_enabled_flag ) return;

//	printf("vcount = %d\n",readl(mga_mmio_base + VCOUNT));

	//printf("mga_interrupt #%d\n", irq);

	if ( irq != -1 ) {

		cc = readl(mga_mmio_base + STATUS);
		if ( ! (cc & 0x10) ) return;  /* vsyncpen */
// 		debug_irqcnt++;
	} 

//    if ( debug_irqignore ) {
//	debug_irqignore = 0;


/*
	if ( mga_conf_deinterlace ) {
		if ( mga_first_field ) {
			// printf("mga_interrupt first field\n");
			if ( syncfb_interrupt() )
				mga_first_field = 0;
		} else {
			// printf("mga_interrupt second field\n");
			mga_select_buffer( mga_current_field | 2 );
			mga_first_field = 1;
		}
	} else {
		syncfb_interrupt();
	}
*/

//	frame=(frame+1)&1;
	regs.besctl = (regs.besctl & ~0x07000000) + (mga_next_frame << 25);
	writel( regs.besctl, mga_mmio_base + BESCTL ); 

#ifdef CRTC2
// sem pridat vyber obrazku !!!!	
	crtc2_frame_sel(mga_next_frame);
#endif
	
#if 0
	++counter;
	if(!(counter&63)){
	    printf("mga irq counter = %d\n",counter);
	}
#endif

//    } else {
//	debug_irqignore = 1;
//    }

	if ( irq != -1 ) {
		writeb( 0x11, mga_mmio_base + CRTCX);
		writeb( 0, mga_mmio_base + CRTCD );
		writeb( 0x10, mga_mmio_base + CRTCD );
	}

//	writel( regs.besglobctl, mga_mmio_base + BESGLOBCTL);


	return;

}

#endif

int vixConfigPlayback(vidix_playback_t *config)
{
	int x, y, sw, sh, dw, dh;
	int besleft, bestop, ifactor, ofsleft, ofstop, baseadrofs, weight, weights;
	int frame_size=config->frame_size;
#ifdef CRTC2
#define right_margin 0
#define left_margin 18
#define hsync_len 46
#define lower_margin 10
#define vsync_len 4
#define upper_margin 39

	unsigned int hdispend = (config->src.w + 31) & ~31;
	unsigned int hsyncstart = hdispend + (right_margin & ~7);
	unsigned int hsyncend = hsyncstart + (hsync_len & ~7);
	unsigned int htotal = hsyncend + (left_margin & ~7);
	unsigned int vdispend = config->src.h;
	unsigned int vsyncstart = vdispend + lower_margin;
	unsigned int vsyncend = vsyncstart + vsync_len;
	unsigned int vtotal = vsyncend + upper_margin;
#endif 
    int frame;

#if 0	
    switch(config->fourcc)
    {
        case IMGFMT_YV12:
	    mga_config.format = MGA_VID_FORMAT_YV12;
    	    break;
        case IMGFMT_IYUV:
	    mga_config.format = MGA_VID_FORMAT_IYUV;
	    break;
	case IMGFMT_I420:
	    mga_config.format = MGA_VID_FORMAT_I420;
	    break;
	case IMGFMT_YUY2:
	    mga_config.format = MGA_VID_FORMAT_YUY2;
	    break;
	case IMGFMT_UYVY:
	    mga_config.format = MGA_VID_FORMAT_UYVY;
	    break;
	default:
	    printf("[mga] unsupported video format: %x\n",
	        config->fourcc);
	    return(ENOTSUP);
    }
#endif
    if ((config->num_frames < 1) || (config->num_frames > 4))
    {
	printf("[mga] illegal num_frames: %d, setting to 2\n", config->num_frames);
	config->num_frames = 2;
//        return(EINVAL);
    }

    x = config->dest.x;
    y = config->dest.y;
    sw = config->src.w;
    sh = config->src.h;
    dw = config->dest.w;
    dh = config->dest.h;
    
    config->dest.pitch.y=32;
    config->dest.pitch.u=
    config->dest.pitch.v=16;

    printf("[mga] Setting up a %dx%d+%d+%d video window (src %dx%d) format %X\n",
           dw, dh, x, y, sw, sh, config->fourcc);

    if ((sw < 4) || (sh < 4) || (dw < 4) || (dh < 4))
    {
        printf("[mga] Invalid src/dest dimenstions\n");
        return(EINVAL);
    }

    //FIXME check that window is valid and inside desktop

//    printf("[mga] vcount = %d\n", readl(mga_mmio_base + VCOUNT));
    printf("[mga] mga_mmio_base = %p\n",mga_mmio_base);
    printf("[mga] mga_mem_base = %08x\n",mga_mem_base);

    switch(config->fourcc)
    {
	case IMGFMT_I420:
	case IMGFMT_IYUV:
	case IMGFMT_YV12:
	    config->frame_size = ((sw + 31) & ~31) * sh + (((sw + 31) & ~31) * sh) / 2;
	    break;
	case IMGFMT_YUY2:
	case IMGFMT_UYVY:
	    config->frame_size = ((sw + 31) & ~31) * sh * 2;
	    break;
    }

//    config->frame_size = config->src.h*config->src.w+(config->src.w*config->src.h)/2;

    frame_size = config->frame_size;
    
    config->offset.y=0;
    config->offset.v=((sw + 31) & ~31) * sh;
    config->offset.u=config->offset.v+((sw + 31) & ~31) * sh /4;

    mga_src_base = (mga_ram_size*0x100000-config->num_frames*config->frame_size);
    if (mga_src_base < 0)
    {
    	printf("[mga] not enough memory for frames!\n");
    	return(EFAULT);
    }
    mga_src_base &= (~0xFFFF); /* 64k boundary */
    printf("[mga] YUV buffer base: %p\n", mga_src_base);

    config->dga_addr = mga_mem_base + mga_src_base;
	
    if (is_g400) 
      mga_config.card_type = MGA_G400;
    else
      mga_config.card_type = MGA_G200;
		       
    mga_config.ram_size = mga_ram_size;
	
    //FIXME figure out a better way to allocate memory on card
    //allocate 2 megs
    //mga_src_base = mga_mem_base + (MGA_VIDMEM_SIZE-2) * 0x100000;
    //mga_src_base = (MGA_VIDMEM_SIZE-3) * 0x100000;

	
    //Setup the BES registers for a three plane 4:2:0 video source 

    regs.besglobctl = 0;

    switch(config->fourcc)
    {
	case IMGFMT_YV12:	
	case IMGFMT_I420:	
	case IMGFMT_IYUV:	
	regs.besctl = 1         // BES enabled
                    + (0<<6)    // even start polarity
                    + (1<<10)   // x filtering enabled
                    + (1<<11)   // y filtering enabled
                    + (1<<16)   // chroma upsampling
                    + (1<<17)   // 4:2:0 mode
                    + (1<<18);  // dither enabled
#if 0
	if(is_g400)
	{
		//zoom disabled, zoom filter disabled, 420 3 plane format, proc amp
		//disabled, rgb mode disabled 
		regs.besglobctl = (1<<5);
	}
	else
	{
		//zoom disabled, zoom filter disabled, Cb samples in 0246, Cr
		//in 1357, BES register update on besvcnt
	        regs.besglobctl = 0;
	}
#endif
        break;

    case IMGFMT_YUY2:	
	regs.besctl = 1         // BES enabled
                    + (0<<6)    // even start polarity
                    + (1<<10)   // x filtering enabled
                    + (1<<11)   // y filtering enabled
                    + (1<<16)   // chroma upsampling
                    + (0<<17)   // 4:2:2 mode
                    + (1<<18);  // dither enabled

	regs.besglobctl = 0;        // YUY2 format selected
        break;

    case IMGFMT_UYVY:	
	regs.besctl = 1         // BES enabled
                    + (0<<6)    // even start polarity
                    + (1<<10)   // x filtering enabled
                    + (1<<11)   // y filtering enabled
                    + (1<<16)   // chroma upsampling
                    + (0<<17)   // 4:2:2 mode
                    + (1<<18);  // dither enabled

	regs.besglobctl = 1<<6;        // UYVY format selected
        break;

	default:
	    printf("[mga] Unsupported pixel format: %x\n", config->fourcc);
	    return(ENOTSUP);
    }


	//Disable contrast and brightness control
	regs.besglobctl |= (1<<5) + (1<<7);
	regs.beslumactl = (0x7f << 16) + (0x80<<0);
	regs.beslumactl = 0x80<<0;

	//Setup destination window boundaries
	besleft = x > 0 ? x : 0;
	bestop = y > 0 ? y : 0;
	regs.beshcoord = (besleft<<16) + (x + dw-1);
	regs.besvcoord = (bestop<<16) + (y + dh-1);
	
	//Setup source dimensions
	regs.beshsrclst  = (sw - 1) << 16;
	regs.bespitch = (sw + 31) & ~31 ; 
	
	//Setup horizontal scaling
	ifactor = ((sw-1)<<14)/(dw-1);
	ofsleft = besleft - x;
		
	regs.beshiscal = ifactor<<2;
	regs.beshsrcst = (ofsleft*ifactor)<<2;
	regs.beshsrcend = regs.beshsrcst + (((dw - ofsleft - 1) * ifactor) << 2);
	
	//Setup vertical scaling
	ifactor = ((sh-1)<<14)/(dh-1);
	ofstop = bestop - y;

	regs.besviscal = ifactor<<2;

	baseadrofs = ((ofstop*regs.besviscal)>>16)*regs.bespitch;
	//frame_size = ((sw + 31) & ~31) * sh + (((sw + 31) & ~31) * sh) / 2;
	regs.besa1org = (uint32_t) mga_src_base + baseadrofs;
	regs.besa2org = (uint32_t) mga_src_base + baseadrofs + 1*frame_size;
	regs.besb1org = (uint32_t) mga_src_base + baseadrofs + 2*frame_size;
	regs.besb2org = (uint32_t) mga_src_base + baseadrofs + 3*frame_size;

if(config->fourcc==IMGFMT_YV12
 ||config->fourcc==IMGFMT_IYUV
 ||config->fourcc==IMGFMT_I420
 ){
        // planar YUV frames:
	if (is_g400) 
		baseadrofs = (((ofstop*regs.besviscal)/4)>>16)*regs.bespitch;
	else 
		baseadrofs = (((ofstop*regs.besviscal)/2)>>16)*regs.bespitch;

    if(config->fourcc==IMGFMT_YV12){
	regs.besa1corg = (uint32_t) mga_src_base + baseadrofs + regs.bespitch * sh ;
	regs.besa2corg = (uint32_t) mga_src_base + baseadrofs + 1*frame_size + regs.bespitch * sh;
	regs.besb1corg = (uint32_t) mga_src_base + baseadrofs + 2*frame_size + regs.bespitch * sh;
	regs.besb2corg = (uint32_t) mga_src_base + baseadrofs + 3*frame_size + regs.bespitch * sh;
	regs.besa1c3org = regs.besa1corg + ((regs.bespitch * sh) / 4);
	regs.besa2c3org = regs.besa2corg + ((regs.bespitch * sh) / 4);
	regs.besb1c3org = regs.besb1corg + ((regs.bespitch * sh) / 4);
	regs.besb2c3org = regs.besb2corg + ((regs.bespitch * sh) / 4);
    } else {
	regs.besa1c3org = (uint32_t) mga_src_base + baseadrofs + regs.bespitch * sh ;
	regs.besa2c3org = (uint32_t) mga_src_base + baseadrofs + 1*frame_size + regs.bespitch * sh;
	regs.besb1c3org = (uint32_t) mga_src_base + baseadrofs + 2*frame_size + regs.bespitch * sh;
	regs.besb2c3org = (uint32_t) mga_src_base + baseadrofs + 3*frame_size + regs.bespitch * sh;
	regs.besa1corg = regs.besa1c3org + ((regs.bespitch * sh) / 4);
	regs.besa2corg = regs.besa2c3org + ((regs.bespitch * sh) / 4);
	regs.besb1corg = regs.besb1c3org + ((regs.bespitch * sh) / 4);
	regs.besb2corg = regs.besb2c3org + ((regs.bespitch * sh) / 4);
    }

}

    weight = ofstop * (regs.besviscal >> 2);
    weights = weight < 0 ? 1 : 0;
    regs.besv2wght = regs.besv1wght = (weights << 16) + ((weight & 0x3FFF) << 2);
    regs.besv2srclst = regs.besv1srclst = sh - 1 - (((ofstop * regs.besviscal) >> 16) & 0x03FF);

#ifdef CRTC2
	// pridat hlavni registry - tj. casovani ...


switch(config->fourcc){
    case IMGFMT_YV12:	
    case IMGFMT_I420:	
    case IMGFMT_IYUV:	
	cregs.c2ctl = 1         // CRTC2 enabled
		    + (1<<1)	// external clock
		    + (0<<2)	// external clock
		    + (1<<3)	// pixel clock enable - not needed ???
		    + (0<<4)	// high prioryty req
		    + (1<<5)	// high prioryty req
		    + (0<<6)	// high prioryty req
		    + (1<<8)	// high prioryty req max
		    + (0<<9)	// high prioryty req max
		    + (0<<10)	// high prioryty req max
                    + (0<<20)   // CRTC1 to DAC
                    + (1<<21)   // 420 mode
                    + (1<<22)   // 420 mode
                    + (1<<23)   // 420 mode
                    + (0<<24)   // single chroma line for 420 mode - need to be corrected
                    + (0<<25)   /*/ interlace mode - need to be corrected*/
                    + (0<<26)   // field legth polariry
                    + (0<<27)   // field identification polariry
                    + (1<<28)   // VIDRST detection mode
                    + (0<<29)   // VIDRST detection mode
                    + (1<<30)   // Horizontal counter preload
                    + (1<<31)   // Vertical counter preload
		    ;
	cregs.c2datactl = 1         // disable dither - propably not needed, we are already in YUV mode
		    + (1<<1)	// Y filter enable
		    + (1<<2)	// CbCr filter enable
		    + (0<<3)	// subpicture enable (disabled)
		    + (0<<4)	// NTSC enable (disabled - PAL)
		    + (0<<5)	// C2 static subpicture enable (disabled)
		    + (0<<6)	// C2 subpicture offset division (disabled)
		    + (0<<7)	// 422 subformat selection !
/*		    + (0<<8)	// 15 bpp high alpha
		    + (0<<9)	// 15 bpp high alpha
		    + (0<<10)	// 15 bpp high alpha
		    + (0<<11)	// 15 bpp high alpha
		    + (0<<12)	// 15 bpp high alpha
		    + (0<<13)	// 15 bpp high alpha
		    + (0<<14)	// 15 bpp high alpha
		    + (0<<15)	// 15 bpp high alpha
		    + (0<<16)	// 15 bpp low alpha
		    + (0<<17)	// 15 bpp low alpha
		    + (0<<18)	// 15 bpp low alpha
		    + (0<<19)	// 15 bpp low alpha
		    + (0<<20)	// 15 bpp low alpha
		    + (0<<21)	// 15 bpp low alpha
		    + (0<<22)	// 15 bpp low alpha
		    + (0<<23)	// 15 bpp low alpha
		    + (0<<24)	// static subpicture key
		    + (0<<25)	// static subpicture key
		    + (0<<26)	// static subpicture key
		    + (0<<27)	// static subpicture key
		    + (0<<28)	// static subpicture key
*/		    ;
        break;

    case IMGFMT_YUY2:	
	cregs.c2ctl = 1         // CRTC2 enabled
		    + (1<<1)	// external clock
		    + (0<<2)	// external clock
		    + (1<<3)	// pixel clock enable - not needed ???
		    + (0<<4)	// high prioryty req - acc to spec
		    + (1<<5)	// high prioryty req
		    + (0<<6)	// high prioryty req
				// 7 reserved
		    + (1<<8)	// high prioryty req max
		    + (0<<9)	// high prioryty req max
		    + (0<<10)	// high prioryty req max
				// 11-19 reserved
                    + (0<<20)   // CRTC1 to DAC
                    + (1<<21)   // 422 mode
                    + (0<<22)   // 422 mode
                    + (1<<23)   // 422 mode
                    + (0<<24)   // single chroma line for 420 mode - need to be corrected
                    + (0<<25)   /*/ interlace mode - need to be corrected*/
                    + (0<<26)   // field legth polariry
                    + (0<<27)   // field identification polariry
                    + (1<<28)   // VIDRST detection mode
                    + (0<<29)   // VIDRST detection mode
                    + (1<<30)   // Horizontal counter preload
                    + (1<<31)   // Vertical counter preload
		    ;
	cregs.c2datactl = 1         // disable dither - propably not needed, we are already in YUV mode
		    + (1<<1)	// Y filter enable
		    + (1<<2)	// CbCr filter enable
		    + (0<<3)	// subpicture enable (disabled)
		    + (0<<4)	// NTSC enable (disabled - PAL)
		    + (0<<5)	// C2 static subpicture enable (disabled)
		    + (0<<6)	// C2 subpicture offset division (disabled)
		    + (0<<7)	// 422 subformat selection !
/*		    + (0<<8)	// 15 bpp high alpha
		    + (0<<9)	// 15 bpp high alpha
		    + (0<<10)	// 15 bpp high alpha
		    + (0<<11)	// 15 bpp high alpha
		    + (0<<12)	// 15 bpp high alpha
		    + (0<<13)	// 15 bpp high alpha
		    + (0<<14)	// 15 bpp high alpha
		    + (0<<15)	// 15 bpp high alpha
		    + (0<<16)	// 15 bpp low alpha
		    + (0<<17)	// 15 bpp low alpha
		    + (0<<18)	// 15 bpp low alpha
		    + (0<<19)	// 15 bpp low alpha
		    + (0<<20)	// 15 bpp low alpha
		    + (0<<21)	// 15 bpp low alpha
		    + (0<<22)	// 15 bpp low alpha
		    + (0<<23)	// 15 bpp low alpha
		    + (0<<24)	// static subpicture key
		    + (0<<25)	// static subpicture key
		    + (0<<26)	// static subpicture key
		    + (0<<27)	// static subpicture key
		    + (0<<28)	// static subpicture key
*/			;
          break;

    case IMGFMT_UYVY:	
	cregs.c2ctl = 1         // CRTC2 enabled
		    + (1<<1)	// external clock
		    + (0<<2)	// external clock
		    + (1<<3)	// pixel clock enable - not needed ???
		    + (0<<4)	// high prioryty req
		    + (1<<5)	// high prioryty req
		    + (0<<6)	// high prioryty req
		    + (1<<8)	// high prioryty req max
		    + (0<<9)	// high prioryty req max
		    + (0<<10)	// high prioryty req max
                    + (0<<20)   // CRTC1 to DAC
                    + (1<<21)   // 422 mode
                    + (0<<22)   // 422 mode
                    + (1<<23)   // 422 mode
                    + (1<<24)   // single chroma line for 420 mode - need to be corrected
                    + (1<<25)   /*/ interlace mode - need to be corrected*/
                    + (0<<26)   // field legth polariry
                    + (0<<27)   // field identification polariry
                    + (1<<28)   // VIDRST detection mode
                    + (0<<29)   // VIDRST detection mode
                    + (1<<30)   // Horizontal counter preload
                    + (1<<31)   // Vertical counter preload
		    ;
	cregs.c2datactl = 0         // enable dither - propably not needed, we are already in YUV mode
		    + (1<<1)	// Y filter enable
		    + (1<<2)	// CbCr filter enable
		    + (0<<3)	// subpicture enable (disabled)
		    + (0<<4)	// NTSC enable (disabled - PAL)
		    + (0<<5)	// C2 static subpicture enable (disabled)
		    + (0<<6)	// C2 subpicture offset division (disabled)
		    + (1<<7)	// 422 subformat selection !
/*		    + (0<<8)	// 15 bpp high alpha
		    + (0<<9)	// 15 bpp high alpha
		    + (0<<10)	// 15 bpp high alpha
		    + (0<<11)	// 15 bpp high alpha
		    + (0<<12)	// 15 bpp high alpha
		    + (0<<13)	// 15 bpp high alpha
		    + (0<<14)	// 15 bpp high alpha
		    + (0<<15)	// 15 bpp high alpha
		    + (0<<16)	// 15 bpp low alpha
		    + (0<<17)	// 15 bpp low alpha
		    + (0<<18)	// 15 bpp low alpha
		    + (0<<19)	// 15 bpp low alpha
		    + (0<<20)	// 15 bpp low alpha
		    + (0<<21)	// 15 bpp low alpha
		    + (0<<22)	// 15 bpp low alpha
		    + (0<<23)	// 15 bpp low alpha
		    + (0<<24)	// static subpicture key
		    + (0<<25)	// static subpicture key
		    + (0<<26)	// static subpicture key
		    + (0<<27)	// static subpicture key
		    + (0<<28)	// static subpicture key
*/		    ;
        break;

    default:
	printf("[mga] Unsupported pixel format: %x\n",config->fourcc);
	return(ENOTSUP);
    }

	cregs.c2hparam=((hdispend - 8) << 16) | (htotal - 8);
	cregs.c2hsync=((hsyncend - 8) << 16) | (hsyncstart - 8);
	
	cregs.c2misc=0	// CRTCV2 656 togg f0
		    +(0<<1) // CRTCV2 656 togg f0
		    +(0<<2) // CRTCV2 656 togg f0
		    +(0<<4) // CRTCV2 656 togg f1
		    +(0<<5) // CRTCV2 656 togg f1
		    +(0<<6) // CRTCV2 656 togg f1
		    +(0<<8) // Hsync active high
		    +(0<<9) // Vsync active high
		    // 16-27 c2vlinecomp - nevim co tam dat
		    ;
	cregs.c2offset=(regs.bespitch << 1);

	cregs.c2pl2startadd0=regs.besa1corg;
//	cregs.c2pl2startadd1=regs.besa2corg;
	cregs.c2pl3startadd0=regs.besa1c3org;
//	cregs.c2pl3startadd1=regs.besa2c3org;
		    
	cregs.c2preload=(vsyncstart << 16) | (hsyncstart); // from 
	
	cregs.c2spicstartadd0=0; // not used
//	cregs.c2spicstartadd1=0; // not used
	
    cregs.c2startadd0=regs.besa1org;
//    cregs.c2startadd1=regs.besa2org;
	
    cregs.c2subpiclut=0; //not used
	
    cregs.c2vparam=((vdispend - 1) << 16) | (vtotal - 1);
    cregs.c2vsync=((vsyncend - 1) << 16) | (vsyncstart - 1);
#endif

    mga_vid_write_regs(0);
    return(0);
}

int vixPlaybackOn(void)
{
    printf("[mga] playback on\n");

    vid_src_ready = 1;
    if(vid_overlay_on)
    {
	regs.besctl |= 1;
    	mga_vid_write_regs(0);
    }
#ifdef MGA_ALLOW_IRQ
    if (mga_irq != -1)
	enable_irq();
#endif
    mga_next_frame=0;

    return(0);
}

int vixPlaybackOff(void)
{
    printf("[mga] playback off\n");

    vid_src_ready = 0;   
#ifdef MGA_ALLOW_IRQ
    if (mga_irq != -1)
	disable_irq();
#endif
    regs.besctl &= ~1;
    regs.besglobctl &= ~(1<<6); /* UYVY format selected */
    mga_vid_write_regs(0);

    return(0);
}

static int mga_vid_release(/*struct inode *inode, struct file *file*/)
{
	//Close the window just in case
	printf("[mga] Video OFF (release)\n");

	vid_src_ready = 0;   
	regs.besctl &= ~1;
        regs.besglobctl &= ~(1<<6);  // UYVY format selected
//	mga_config.colkey_on=0; //!!!
	mga_vid_write_regs(1);
	mga_vid_in_use = 0;

//	MOD_DEC_USE_COUNT;
	return 0;
}

int vixProbe(int verbose)
{
	pciinfo_t lst[MAX_PCI_DEVICES];
	unsigned int i, num_pci;
	int err;

	printf("[mga] probe\n");

	mga_verbose = verbose;

	is_g400 = -1;

	err = pci_scan(&lst, &num_pci);
	if (err)
	{
	    printf("[mga] Error occured during pci scan: %s\n", strerror(err));
	    return(err);
	}

	if (mga_verbose > 1)
	    printf("[mga] found %d pci devices\n", num_pci);
	
	for (i = 0; i < num_pci; i++)
	{
	    if (mga_verbose > 2)
		printf("pci[%d] vendor: %d device: %d\n",
		    i, lst[i].vendor, lst[i].device);
	    if (lst[i].vendor == VENDOR_MATROX)
	    {
		switch(lst[i].device)
		{
		    case DEVICE_MATROX_MGA_G550_AGP:
			printf("[mga] Found MGA G550\n");
			is_g400 = 1;
			goto card_found;
		    case DEVICE_MATROX_MGA_G400_AGP:
			printf("[mga] Found MGA G400/G450\n");
			is_g400 = 1;
			goto card_found;
		    case DEVICE_MATROX_MGA_G200_AGP:
			printf("[mga] Found MGA G200 AGP\n");
			is_g400 = 0;
			goto card_found;
		    case DEVICE_MATROX_MGA_G200:
			printf("[mga] Found MGA G200 PCI\n");
			is_g400 = 0;
			goto card_found;
		}
	    }
	}

	if (is_g400 == -1)
	{
		printf("[mga] No supported cards found\n");
		return(ENXIO);
	}

card_found:
	probed = 1;
	memcpy(&pci_info, &lst[i], sizeof(pciinfo_t));

	mga_cap.device_id = pci_info.device; /* set device id in capabilites */

	return(0);
}

int vixInit(void)
{
    unsigned int card_option;

    printf("[mga] init\n");

    mga_vid_in_use = 0;

    printf("Matrox MGA G200/G400/G450 YUV Video interface v2.01 (c) Aaron Holtzman & A'rpi\n");

    if (!probed)
    {
	printf("[mga] driver was not probed but is being initializing\n");
	return(EINTR);
    }

#if 0	
#if LINUX_VERSION_CODE >= 0x020300
    mga_mmio_base = ioremap_nocache(dev->resource[1].start,0x4000);
    mga_mem_base =  dev->resource[0].start;
#else
    mga_mmio_base = ioremap_nocache(dev->base_address[1] & PCI_BASE_ADDRESS_MEM_MASK,0x4000);
    mga_mem_base =  dev->base_address[0] & PCI_BASE_ADDRESS_MEM_MASK;
#endif
#endif

#warning "FIXME: implement pciconfig_read! (or enable syscall)\n"
#if 0
/* from linux/pci.h */
#define PCI_DEVFN(slot, func) ((((slot) & 0x1f) << 3) | ((func) & 0x07))

    pciconfig_read(pci_info.bus, PCI_DEVFN(pci_info.card, pci_info.func),
        0x40, 4, &card_option);
//    pci_read_config_dword(dev,  0x40, &card_option);
    printf("[mga] OPTION word: 0x%08X  mem: 0x%02X  %s\n", card_option,
    	(card_option>>10)&0x17, ((card_option>>14)&1)?"SGRAM":"SDRAM");
#endif

//    temp = (card_option >> 10) & 0x17;

    if (mga_ram_size)
    {
    	printf("[mga] RAMSIZE forced to %d MB\n", mga_ram_size);
    }
    else
    {
#ifdef MGA_MEMORY_SIZE
        mga_ram_size = MGA_MEMORY_SIZE;
        printf("[mga] hard-coded RAMSIZE is %d MB\n", (unsigned int) mga_ram_size);
#else
        if (is_g400)
	{
	    switch((card_option>>10)&0x17)
	    {
	        // SDRAM:
	        case 0x00:
	        case 0x04:  mga_ram_size = 16; break;
	        case 0x03:  mga_ram_size = 32; break;
	        // SGRAM:
	        case 0x10:
	        case 0x14:  mga_ram_size = 32; break;
	        case 0x11:
	        case 0x12:  mga_ram_size = 16; break;
	        default:
	    	    mga_ram_size = 16;
		    printf("[mga] Couldn't detect RAMSIZE, assuming 16MB!\n");
	    }
	}
	else
	{
	    switch((card_option>>10)&0x17)
	    {
//    		case 0x10:
//		case 0x13:  mga_ram_size = 8; break;
		default: mga_ram_size = 8;
	    }
	} 

#if 0
//	    printf("List resources -----------\n");
	    for(temp=0;temp<DEVICE_COUNT_RESOURCE;temp++){
	        struct resource *res=&pci_dev->resource[temp];
	        if(res->flags){
	          int size=(1+res->end-res->start)>>20;
	          printf("res %d:  start: 0x%X   end: 0x%X  (%d MB) flags=0x%X\n",temp,res->start,res->end,size,res->flags);
	          if(res->flags&(IORESOURCE_MEM|IORESOURCE_PREFETCH)){
	              if(size>mga_ram_size && size<=64) mga_ram_size=size;
	          }
	        }
	    }
#endif

        printf("[mga] detected RAMSIZE is %d MB\n", (unsigned int) mga_ram_size);
#endif
    }

    if (mga_ram_size)
    {
	if ((mga_ram_size < 4) || (mga_ram_size > 64))
	{
	    printf("[mga] invalid RAMSIZE: %d MB\n", mga_ram_size);
	    return(EINVAL);
	}
    }

    printf("[mga] hardware addresses: mmio: %p, framebuffer: %p\n",
        pci_info.base1, pci_info.base0);

    mga_mmio_base = map_phys_mem(pci_info.base1,0x4000);
    mga_mem_base = map_phys_mem(pci_info.base0,mga_ram_size*1024*1024);

    printf("[mga] MMIO at %p, IRQ: %d, framebuffer: %p\n",
        mga_mmio_base, mga_irq, mga_mem_base);

#ifdef MGA_ALLOW_IRQ
    if (mga_irq != -1)
    {
    	int tmp = request_irq(mga_irq, mga_handle_irq, SA_INTERRUPT | SA_SHIRQ, "Syncfb Time Base", &mga_irq);
    	if (tmp)
	{
    	    printf("syncfb (mga): cannot register irq %d (Err: %d)\n", mga_irq, tmp);
    	    mga_irq=-1;
	}
	else
	{
	    printf("syncfb (mga): registered irq %d\n", mga_irq);
	}
    }
    else
    {
	printf("syncfb (mga): No valid irq was found\n");
	mga_irq=-1;
    }
#else
	printf("syncfb (mga): IRQ disabled in mga_vid.c\n");
	mga_irq=-1;
#endif

    return(0);
}

void vixDestroy(void)
{
    printf("[mga] destroy\n");

#ifdef MGA_ALLOW_IRQ
    if (mga_irq != -1)
    	free_irq(mga_irq, &mga_irq);
#endif

    if (mga_mmio_base)
        unmap_phys_mem(mga_mmio_base, 0x4000);
    if (mga_mem_base)
        unmap_phys_mem(mga_mem_base, mga_ram_size);

    /* FIXME turn off BES */

    return;
}

int vixQueryFourcc(vidix_fourcc_t *to)
{
    printf("[mga] query fourcc (%x)\n", to->fourcc);

    switch(to->fourcc)
    {
	case IMGFMT_YV12:
	case IMGFMT_IYUV:
	case IMGFMT_I420:
	case IMGFMT_YUY2:
	case IMGFMT_UYVY:
	    break;
	default:
	    to->depth = to->flags = 0;
	    return(ENOTSUP);
    }
    
    to->depth = VID_DEPTH_12BPP | VID_DEPTH_16BPP | VID_DEPTH_32BPP;
    to->flags = VID_CAP_EXPAND | VID_CAP_SHRINK;
    return(0);
}

unsigned int vixGetVersion(void)
{
    return(VIDIX_VERSION);
}

int vixGetCapability(vidix_capability_t *to)
{
    memcpy(to, &mga_cap, sizeof(vidix_capability_t));
    return(0);
}


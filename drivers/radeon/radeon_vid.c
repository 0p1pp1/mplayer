/*
 *
 * radeon_vid.c
 *
 * Copyright (C) 2001 Nick Kurshev
 * 
 * BES YUV Framebuffer driver for Radeon cards
 * 
 * This software has been released under the terms of the GNU Public
 * license. See http://www.gnu.org/copyleft/gpl.html for details.
 *
 * This file is partly based on mga_vid and sis_vid stuff from
 * mplayer's package.
 */

/*
  It's entirely possible this major conflicts with something else
  mknod /dev/radeon_vid c 178 0
 */

/*
  TODO:
  OV0_COLOUR_CNTL         brightness saturation 
   SCALER_GAMMA_SEL_BRIGHT  gamma correction ??? 
  OV0_GRAPHICS_KEY_CLR    color key 
  OV0_AUTO_FLIP_CNTL
  OV0_DEINTERLACE_PATTERN
  OV0_FILTER_CNTL
  OV0_VIDEO_KEY_CLR
  OV0_KEY_CNTL

  BPP should be known
*/

#include <linux/config.h>
#include <linux/version.h>
#include <linux/module.h>
#include <linux/types.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/mm.h>
#include <linux/string.h>
#include <linux/errno.h>
#include <linux/slab.h>
#include <linux/pci.h>
#include <linux/ioport.h>
#include <linux/init.h>

#include "radeon_vid.h"
#include "radeon.h"

#ifdef CONFIG_MTRR 
#include <asm/mtrr.h>
#endif

#include <asm/uaccess.h>
#include <asm/system.h>
#include <asm/io.h>

#define TRUE 1
#define FALSE 0

#define RADEON_VID_MAJOR 178


MODULE_AUTHOR("Nick Kurshev <nickols_k@mail.ru>");
MODULE_DESCRIPTION("Accelerated YUV BES driver for Radeons");
MODULE_LICENSE("GPL");


typedef struct bes_registers_s
{
  /* base address of yuv framebuffer */
  uint32_t yuv_base;
  uint32_t fourcc;
  /* YUV BES registers */
  uint32_t reg_load_cntl;
  uint32_t h_inc;
  uint32_t step_by;
  uint32_t y_x_start;
  uint32_t y_x_end;
  uint32_t v_inc;
  uint32_t p1_blank_lines_at_top;
  uint32_t vid_buf_pitch0_value;
  uint32_t p1_x_start_end;
  uint32_t p2_x_start_end;
  uint32_t p3_x_start_end;
  uint32_t vid_buf0_base_adrs;
  /* These ones are for auto flip: maybe in the future */
  uint32_t vid_buf1_base_adrs;
  uint32_t vid_buf2_base_adrs;
  uint32_t vid_buf3_base_adrs;
  uint32_t vid_buf4_base_adrs;
  uint32_t vid_buf5_base_adrs;

  uint32_t p1_v_accum_init;
  uint32_t p1_h_accum_init;
  uint32_t p23_h_accum_init;
  uint32_t scale_cntl;
  uint32_t exclusive_horz;
  uint32_t auto_flip_cntl;
  uint32_t filter_cntl;
  uint32_t colour_cntl;
  uint32_t graphics_key_msk;
  uint32_t graphics_key_clr;
  uint32_t key_cntl;
  uint32_t test;
} bes_registers_t;

typedef struct video_registers_s
{
  uint32_t name;
  uint32_t value;
}video_registers_t;

static bes_registers_t besr;
static video_registers_t vregs[] = 
{
  { OV0_REG_LOAD_CNTL, 0 },
  { OV0_H_INC, 0 },
  { OV0_STEP_BY, 0 },
  { OV0_Y_X_START, 0 },
  { OV0_Y_X_END, 0 },
  { OV0_V_INC, 0 },
  { OV0_P1_BLANK_LINES_AT_TOP, 0 },
  { OV0_VID_BUF_PITCH0_VALUE, 0 },
  { OV0_P1_X_START_END, 0 },
  { OV0_P2_X_START_END, 0 },
  { OV0_P3_X_START_END, 0 },
  { OV0_VID_BUF0_BASE_ADRS, 0 },
  { OV0_VID_BUF1_BASE_ADRS, 0 },
  { OV0_VID_BUF2_BASE_ADRS, 0 },
  { OV0_VID_BUF3_BASE_ADRS, 0 },
  { OV0_VID_BUF4_BASE_ADRS, 0 },
  { OV0_VID_BUF5_BASE_ADRS, 0 },
  { OV0_P1_V_ACCUM_INIT, 0 },
  { OV0_P1_H_ACCUM_INIT, 0 },
  { OV0_P23_H_ACCUM_INIT, 0 },
  { OV0_SCALE_CNTL, 0 },
  { OV0_EXCLUSIVE_HORZ, 0 },
  { OV0_AUTO_FLIP_CNTL, 0 },
  { OV0_FILTER_CNTL, 0 },
  { OV0_COLOUR_CNTL, 0 },
  { OV0_GRAPHICS_KEY_MSK, 0 },
  { OV0_GRAPHICS_KEY_CLR, 0 },
  { OV0_KEY_CNTL, 0 },
  { OV0_TEST, 0 }
};

static uint32_t radeon_vid_in_use = 0;
static uint32_t vid_src_ready = 0;
static uint32_t vid_overlay_on = 0;

static uint8_t *radeon_mmio_base = 0;
static uint32_t radeon_mem_base = 0; 
#define RADEON_SRC_BASE 0ULL /* this driver uses all video memory */

static uint32_t radeon_ram_size = 0;

//static struct video_window radeon_win;
static mga_vid_config_t radeon_config; 

#define DEBUG 1
#if DEBUG
#define RTRACE		printk
#else
#define RTRACE(...)	((void)0)
#endif


/*
 * IO macros
 */

#define INREG8(addr)		readb((radeon_mmio_base)+addr)
#define OUTREG8(addr,val)	writeb(val, (radeon_mmio_base)+addr)
#define INREG(addr)		readl((radeon_mmio_base)+addr)
#define OUTREG(addr,val)	writel(val, (radeon_mmio_base)+addr)

static void radeon_vid_save_state( void )
{
  size_t i;
  for(i=0;i<sizeof(vregs)/sizeof(video_registers_t);i++)
	vregs[i].value = INREG(vregs[i].name);
}

static void radeon_vid_restore_state( void )
{
  size_t i;
  for(i=0;i<sizeof(vregs)/sizeof(video_registers_t);i++)
	OUTREG(vregs[i].name,vregs[i].value);
}

static void radeon_vid_stop_video( void )
{
  if(vid_src_ready == 1)
  {
    OUTREG(OV0_SCALE_CNTL, SCALER_SOFT_RESET);
    OUTREG(OV0_EXCLUSIVE_HORZ, 0);
    OUTREG(OV0_AUTO_FLIP_CNTL, 0);   /* maybe */
    OUTREG(OV0_FILTER_CNTL, 0x0000000f);
/*
    OUTREG(OV0_COLOUR_CNTL, (brightness & 0x7f) |
			    (saturation << 8) |
			    (saturation << 16));
    OUTREG(OV0_GRAPHICS_KEY_MSK, (1 << depth) - 1);
    OUTREG(OV0_GRAPHICS_KEY_CLR, colorKey);
*/
    OUTREG(OV0_KEY_CNTL, GRAPHIC_KEY_FN_NE);
    OUTREG(OV0_TEST, 0);
  } 
  vid_src_ready = 0;
}

static void radeon_vid_display_video( void )
{
    int bes_flags;
    OUTREG(OV0_REG_LOAD_CNTL,		REG_LD_CTL_LOCK);
    while(!(INREG(OV0_REG_LOAD_CNTL)&REG_LD_CTL_LOCK_READBACK));

    OUTREG(OV0_AUTO_FLIP_CNTL,OV0_AUTO_FLIP_CNTL_SOFT_BUF_ODD);

    OUTREG(OV0_DEINTERLACE_PATTERN,0xAAAAA);
   
    OUTREG(OV0_AUTO_FLIP_CNTL,(INREG(OV0_AUTO_FLIP_CNTL)^OV0_AUTO_FLIP_CNTL_SOFT_EOF_TOGGLE));
    OUTREG(OV0_AUTO_FLIP_CNTL,(INREG(OV0_AUTO_FLIP_CNTL)^OV0_AUTO_FLIP_CNTL_SOFT_EOF_TOGGLE));

    OUTREG(OV0_H_INC,			besr.h_inc);
    OUTREG(OV0_STEP_BY,			besr.step_by);
    OUTREG(OV0_Y_X_START,		besr.y_x_start);
    OUTREG(OV0_Y_X_END,			besr.y_x_end);
    OUTREG(OV0_V_INC,			besr.v_inc);
    OUTREG(OV0_P1_BLANK_LINES_AT_TOP,	besr.p1_blank_lines_at_top);
    OUTREG(OV0_VID_BUF_PITCH0_VALUE,	besr.vid_buf_pitch0_value);
    OUTREG(OV0_P1_X_START_END,		besr.p1_x_start_end);
    OUTREG(OV0_P2_X_START_END,		besr.p2_x_start_end);
    OUTREG(OV0_P3_X_START_END,		besr.p3_x_start_end);
    OUTREG(OV0_VID_BUF0_BASE_ADRS,	besr.vid_buf0_base_adrs);
    OUTREG(OV0_VID_BUF1_BASE_ADRS,	besr.vid_buf1_base_adrs);
    OUTREG(OV0_VID_BUF2_BASE_ADRS,	besr.vid_buf2_base_adrs);
    OUTREG(OV0_VID_BUF3_BASE_ADRS,	besr.vid_buf3_base_adrs);
    OUTREG(OV0_VID_BUF4_BASE_ADRS,	besr.vid_buf4_base_adrs);
    OUTREG(OV0_VID_BUF5_BASE_ADRS,	besr.vid_buf5_base_adrs);
    OUTREG(OV0_P1_V_ACCUM_INIT,		besr.p1_v_accum_init);
    OUTREG(OV0_P1_H_ACCUM_INIT,		besr.p1_h_accum_init);
    OUTREG(OV0_P23_H_ACCUM_INIT,	besr.p23_h_accum_init);

    bes_flags = SCALER_ENABLE |
                SCALER_DOUBLE_BUFFER |
                SCALER_ADAPTIVE_DEINT |
                SCALER_SMART_SWITCH |
                SCALER_HORZ_PICK_NEAREST;
    switch(besr.fourcc)
    {
        case IMGFMT_RGB15:
        case IMGFMT_BGR15: bes_flags |= SCALER_SOURCE_15BPP; break;
        case IMGFMT_RGB16:
	case IMGFMT_BGR16: bes_flags |= SCALER_SOURCE_16BPP; break;
        case IMGFMT_RGB24:
        case IMGFMT_BGR24: bes_flags |= SCALER_SOURCE_24BPP; break;
        case IMGFMT_RGB32:
	case IMGFMT_BGR32: bes_flags |= SCALER_SOURCE_32BPP; break;

        case IMGFMT_YVU9:  bes_flags |= SCALER_SOURCE_YUV9; break;
	case IMGFMT_IYUV:  bes_flags |= SCALER_SOURCE_YUV12; break;
	case IMGFMT_UYVY:  bes_flags |= SCALER_SOURCE_YVYU422; break;

	case IMGFMT_YV12:
	case IMGFMT_I420:
	case IMGFMT_YUY2:
	default:           bes_flags |= SCALER_SOURCE_VYUY422; break;
    }
    OUTREG(OV0_SCALE_CNTL,		bes_flags);
/*
 TODO:
 brightness: -64 : +63
 saturation: 0 : 31
 	OUTREG(OV0_COLOUR_CNTL, (brightness & 0x7f) |
				(saturation << 8) |
				(saturation << 16));
	OUTREG(OV0_GRAPHICS_KEY_CLR, colkey_red | colkey_green << 8 | colkey_blue << 16);

*/
    OUTREG(OV0_REG_LOAD_CNTL,		0);
}

static void radeon_vid_start_video( void )
{
  if(vid_src_ready == 0) radeon_vid_display_video();
  vid_src_ready = 1;
}

#define XXX_SRC_X 0
#define XXX_SRC_Y 0

#define XXX_WIDTH   config->dest_width
#define XXX_HEIGHT  config->dest_height

static int radeon_vid_init_video( mga_vid_config_t *config )
{
    uint32_t tmp,left,src_w,pitch;

RTRACE("radeon_vid: usr_config: version = %x card=%x ram=%x src(%xx%x) dest(%x:%xx%x:%x) frame_size=%x num_frames=%x\n"
	,(uint32_t)config->version
	,(uint32_t)config->card_type
	,(uint32_t)config->ram_size
	,(uint32_t)config->src_width
	,(uint32_t)config->src_height
	,(uint32_t)config->x_org
	,(uint32_t)config->y_org
	,(uint32_t)config->dest_width
	,(uint32_t)config->dest_height
	,(uint32_t)config->frame_size
	,(uint32_t)config->num_frames);

    config->num_frames = 1; /* FIXME: should be 6 later */
    switch(config->format)
    {
        case IMGFMT_RGB15:
        case IMGFMT_BGR15:
        case IMGFMT_RGB16:
	case IMGFMT_BGR16:
        case IMGFMT_RGB24:
        case IMGFMT_BGR24:
        case IMGFMT_RGB32:
	case IMGFMT_BGR32:

        case IMGFMT_YVU9:
	case IMGFMT_IYUV:
	case IMGFMT_UYVY:

	case IMGFMT_YV12:
	case IMGFMT_I420:
	case IMGFMT_YUY2:
				break;
	default:
		printk( "radeon_vid: Unsupported pixel format: 0x%X\n",config->format);
		return -1;
    }

    pitch = ((XXX_WIDTH << 1) + 15) & ~15;
    besr.fourcc = config->format;

    besr.v_inc = (config->src_height << 20) / (config->dest_height<<4);
    besr.h_inc = (config->src_width  << 12) / (config->dest_width<<4);
    besr.step_by = 1;

    while(besr.h_inc >= (2 << 12)) {
	besr.step_by++;
	besr.h_inc >>= 1;
    }

RTRACE("radeon_vid: BES: v_inc=%x h_inc=%x step_by=%x\n",besr.v_inc,besr.h_inc,besr.step_by);
    /* keep everything in 16.16 */

    besr.vid_buf0_base_adrs = RADEON_SRC_BASE; /* I guess that offset 0 is o'k */
/*    besr.vid_buf0_base_adrs += (((XXX_SRC_X >> 16) & ~7) << 1)&0xfffffff0;*/
RTRACE("radeon_vid: BES: vid_buf0_basey=%x\n",besr.vid_buf0_base_adrs);
    besr.vid_buf1_base_adrs = besr.vid_buf0_base_adrs + config->frame_size;
    besr.vid_buf2_base_adrs = besr.vid_buf0_base_adrs;
    besr.vid_buf3_base_adrs = besr.vid_buf0_base_adrs + config->frame_size;
    besr.vid_buf4_base_adrs = besr.vid_buf0_base_adrs;
    besr.vid_buf5_base_adrs = besr.vid_buf0_base_adrs + config->frame_size;

    tmp = (XXX_SRC_X & 0x0003ffff) + 0x00028000 + (besr.h_inc << 3);
    besr.p1_h_accum_init = ((tmp <<  4) & 0x000f8000) |
			    ((tmp << 12) & 0xf0000000);

    tmp = ((XXX_SRC_X >> 1) & 0x0001ffff) + 0x00028000 + (besr.h_inc << 2);
    besr.p23_h_accum_init = ((tmp <<  4) & 0x000f8000) |
			     ((tmp << 12) & 0x70000000);

    tmp = (XXX_SRC_Y & 0x0000ffff) + 0x00018000;
    besr.p1_v_accum_init = ((tmp << 4) & 0x03ff8000) | 0x00000001;

    left = 0; /*(XXX_SRC_X >> 16) & 7;*/
    besr.h_inc |= ((besr.h_inc >> 1) << 16);
    besr.step_by |= (besr.step_by << 8);
    besr.y_x_start = (config->x_org + 8) | (config->y_org << 16);
    besr.y_x_end = ((config->x_org + config->dest_width) + 8) | ((config->y_org + config->dest_height) << 16);
    besr.p1_blank_lines_at_top = 0x00000fff | ((config->src_height - 1) << 16);
    besr.vid_buf_pitch0_value = pitch;
RTRACE("radeon_vid: BES: y_x_start=%x y_x_end=%x blank_at_top=%x pitch0_value=%x\n"
    ,besr.y_x_start,besr.y_x_end,besr.p1_blank_lines_at_top,besr.vid_buf_pitch0_value);
    besr.p1_x_start_end = (config->src_width + left - 1) | (left << 16);
    left >>= 1; src_w=config->src_width >> 1;
    besr.p2_x_start_end = (src_w + left - 1) | (left << 16);
    besr.p3_x_start_end = besr.p2_x_start_end;
    return 0;
}

static void radeon_vid_frame_sel(int frame)
{
  /* TODO ASAP */
}

static int radeon_vid_ioctl(struct inode *inode, struct file *file, unsigned int cmd, unsigned long arg)
{
	int frame;

	switch(cmd)
	{
		case MGA_VID_CONFIG:
			RTRACE( "radeon_mmio_base = %p\n",radeon_mmio_base);
			RTRACE( "radeon_mem_base = %08x\n",radeon_mem_base);
			RTRACE( "radeon_vid: Received configuration\n");

 			if(copy_from_user(&radeon_config,(mga_vid_config_t*) arg,sizeof(mga_vid_config_t)))
			{
				printk( "radeon_vid: failed copy from userspace\n");
				return(-EFAULT);
			}
			if(radeon_config.version != MGA_VID_VERSION){
				printk( "radeon_vid: incompatible version! driver: %X  requested: %X\n",MGA_VID_VERSION,radeon_config.version);
				return(-EFAULT);
			}

			if(radeon_config.frame_size==0 || radeon_config.frame_size>1024*768*2){
				printk( "radeon_vid: illegal frame_size: %d\n",radeon_config.frame_size);
				return(-EFAULT);
			}

			if(radeon_config.num_frames<1 || radeon_config.num_frames>4){
				printk( "radeon_vid: illegal num_frames: %d\n",radeon_config.num_frames);
				return(-EFAULT);
			}

                        /* FIXME: Fake of G400 ;) or would be better G200 ??? */
			radeon_config.card_type = 0;
			radeon_config.ram_size = radeon_ram_size;

			if (copy_to_user((mga_vid_config_t *) arg, &radeon_config, sizeof(mga_vid_config_t)))
			{
				printk( "radeon_vid: failed copy to userspace\n");
				return(-EFAULT);
			}
			return radeon_vid_init_video(&radeon_config);
		break;

		case MGA_VID_ON:
			RTRACE( "radeon_vid: Video ON (ioctl)\n");
			vid_src_ready = 1;
			if(vid_overlay_on) radeon_vid_start_video();
		break;

		case MGA_VID_OFF:
			RTRACE( "radeon_vid: Video OFF (ioctl)\n");
			radeon_vid_stop_video();
		break;

		case MGA_VID_FSEL:
			if(copy_from_user(&frame,(int *) arg,sizeof(int)))
			{
				printk("radeon_vid: FSEL failed copy from userspace\n");
				return(-EFAULT);
			}
			radeon_vid_frame_sel(frame);
		break;

	        default:
			printk( "radeon_vid: Invalid ioctl\n");
			return (-EINVAL);
	}

	return 0;
}

struct ati_card_id_s
{
  int id;
  char name[17];
}ati_card_ids[]=
{
 { PCI_DEVICE_ID_RADEON_QD, "Radeon QD " },
 { PCI_DEVICE_ID_RADEON_QE, "Radeon QE " },
 { PCI_DEVICE_ID_RADEON_QF, "Radeon QF " },
 { PCI_DEVICE_ID_RADEON_QG, "Radeon QG " },
 { PCI_DEVICE_ID_RADEON_QY, "Radeon VE QY " },
 { PCI_DEVICE_ID_RADEON_QZ, "Radeon VE QZ " },
 { PCI_DEVICE_ID_RADEON_LY, "Radeon M6 LY " },
 { PCI_DEVICE_ID_RADEON_LZ, "Radeon M6 LZ " },
 { PCI_DEVICE_ID_RADEON_LW, "Radeon M7 LW " },
 { PCI_DEVICE_ID_R200_QL,   "Radeon2 8500 QL " },
 { PCI_DEVICE_ID_RV200_QW,  "Radeon2 7500 QW " }
};

static int radeon_vid_config_card(void)
{
	struct pci_dev *dev = NULL;
	size_t i;

	for(i=0;i<sizeof(ati_card_ids)/sizeof(struct ati_card_id_s);i++)
	  if((dev=pci_find_device(PCI_VENDOR_ID_ATI, ati_card_ids[i].id, NULL)))
		break;
	if(dev)
		printk("radeon_vid: Found %s\n",ati_card_ids[i].name);
	else
	{
		printk("radeon_vid: No supported cards found\n");
		return FALSE;
	}

	radeon_mmio_base = ioremap_nocache(pci_resource_start (dev, 2),RADEON_REGSIZE);
	radeon_mem_base =  dev->resource[0].start;

	RTRACE( "radeon_vid: MMIO at 0x%p\n", radeon_mmio_base);
	RTRACE( "radeon_vid: Frame Buffer at 0x%08x\n", radeon_mem_base);

	radeon_ram_size = pci_resource_len(dev, 0)/0x100000;

	return TRUE;
}


static ssize_t radeon_vid_read(struct file *file, char *buf, size_t count, loff_t *ppos)
{
	return -EINVAL;
}

static ssize_t radeon_vid_write(struct file *file, const char *buf, size_t count, loff_t *ppos)
{
	return -EINVAL;
}

static int radeon_vid_mmap(struct file *file, struct vm_area_struct *vma)
{

	RTRACE( "radeon_vid: mapping video memory into userspace\n");
	if(remap_page_range(vma->vm_start, radeon_mem_base + RADEON_SRC_BASE,
		 vma->vm_end - vma->vm_start, vma->vm_page_prot)) 
	{
		printk( "radeon_vid: error mapping video memory\n");
		return(-EAGAIN);
	}

	return(0);
}

static int radeon_vid_release(struct inode *inode, struct file *file)
{
	//Close the window just in case
	vid_src_ready = 0;   
	radeon_vid_in_use = 0;
	radeon_vid_stop_video();

	MOD_DEC_USE_COUNT;
	return 0;
}

static long long radeon_vid_lseek(struct file *file, long long offset, int origin)
{
	return -ESPIPE;
}					 

static int radeon_vid_open(struct inode *inode, struct file *file)
{
	int minor = MINOR(inode->i_rdev);

	if(minor != 0)
	 return(-ENXIO);

	if(radeon_vid_in_use == 1) 
		return(-EBUSY);

	radeon_vid_in_use = 1;
	MOD_INC_USE_COUNT;
	return(0);
}

#if LINUX_VERSION_CODE >= 0x020400
static struct file_operations radeon_vid_fops =
{
	llseek:		radeon_vid_lseek,
	read:			radeon_vid_read,
	write:		radeon_vid_write,
	ioctl:		radeon_vid_ioctl,
	mmap:			radeon_vid_mmap,
	open:			radeon_vid_open,
	release: 	radeon_vid_release
};
#else
static struct file_operations radeon_vid_fops =
{
	radeon_vid_lseek,
	radeon_vid_read,
	radeon_vid_write,
	NULL,
	NULL,
	radeon_vid_ioctl,
	radeon_vid_mmap,
	radeon_vid_open,
	NULL,
	radeon_vid_release
};
#endif

/* 
 * Main Initialization Function 
 */


static int radeon_vid_initialize(void)
{
	radeon_vid_in_use = 0;

	RTRACE( "Radeon BES YUV Video interface v0.01 (c) Nick Kurshev\n");
	if(register_chrdev(RADEON_VID_MAJOR, "radeon_vid", &radeon_vid_fops))
	{
		printk( "radeon_vid: unable to get major: %d\n", RADEON_VID_MAJOR);
		return -EIO;
	}

	if (!radeon_vid_config_card())
	{
		printk("radeon_vid: can't configure this card\n");
		unregister_chrdev(RADEON_VID_MAJOR, "radeon_vid");
		return -EINVAL;
	}
	radeon_vid_save_state();
	return(0);
}

int init_module(void)
{
	return radeon_vid_initialize();
}

void cleanup_module(void)
{
	radeon_vid_restore_state();
	if(radeon_mmio_base)
		iounmap(radeon_mmio_base);

	RTRACE( "radeon_vid: Cleaning up module\n");
	unregister_chrdev(RADEON_VID_MAJOR, "radeon_vid");
}


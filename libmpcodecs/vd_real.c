#include <stdio.h>
#include <stdlib.h>

#include "config.h"
#ifdef USE_REALCODECS

#include <dlfcn.h>

#include "mp_msg.h"
#include "help_mp.h"

#include "vd_internal.h"

static vd_info_t info = {
	"RealPlayer 8 video codecs",
	"real",
	VFM_REAL,
	"Florian Schneider",
	"using original closed source codecs for Linux",
	"binary real video codecs"
};

LIBVD_EXTERN(real)


typedef unsigned long ulong;

ulong (*rvyuv_custom_message)(ulong,ulong);
ulong (*rvyuv_free)(ulong);
ulong (*rvyuv_hive_message)(ulong,ulong);
ulong (*rvyuv_init)(ulong,ulong);
ulong (*rvyuv_transform)(ulong,ulong,ulong,ulong,ulong);

void *rv_handle=NULL;

void *__builtin_vec_new(unsigned long size) {
	return malloc(size);
}

void __builtin_vec_delete(void *mem) {
	free(mem);
}

void __pure_virtual(void) {
	fprintf(stderr, "I'm outa here!\n");
	exit(1);
}


// to set/get/query special features/parameters
static int control(sh_video_t *sh,int cmd,void* arg,...){
//    switch(cmd){
//    case VDCTRL_QUERY_MAX_PP_LEVEL:
//	return 9;
//    case VDCTRL_SET_PP_LEVEL:
//	vfw_set_postproc(sh,10*(*((int*)arg)));
//	return CONTROL_OK;
//    }
    return CONTROL_UNKNOWN;
}

/* exits program when failure */
int load_syms(char *path) {
		void *handle;
		char *error;

		fprintf(stderr, "opening dll %s\n", path);
		rv_handle = dlopen (path, RTLD_LAZY);
		handle=rv_handle;
		if (!handle) {
			fputs (dlerror(), stderr);
			return 0;
		}

		rvyuv_custom_message = dlsym(handle, "RV20toYUV420CustomMessage");
		if ((error = dlerror()) != NULL)  {
			fprintf (stderr, "dlsym(rvyuvCustomMessage): %s\n", error);
			return 0;
		}
		rvyuv_free = dlsym(handle, "RV20toYUV420Free");
		if ((error = dlerror()) != NULL)  {
			fprintf (stderr, "dlsym(rvyuvFree): %s\n", error);
			return 0;
		}
		rvyuv_hive_message = dlsym(handle, "RV20toYUV420HiveMessage");
		if ((error = dlerror()) != NULL)  {
			fprintf (stderr, "dlsym(rvyuvHiveMessage): %s\n", error);
			return 0;
		}
		rvyuv_init = dlsym(handle, "RV20toYUV420Init");
		if ((error = dlerror()) != NULL)  {
			fprintf (stderr, "dlsym(rvyuvInit): %s\n", error);
			return 0;
		}
		rvyuv_transform = dlsym(handle, "RV20toYUV420Transform");
		if ((error = dlerror()) != NULL)  {
			fprintf (stderr, "dlsym(rvyuvTransform): %s\n", error);
			return 0;
		}
	return 1;
}

/* we need exact positions */
struct rv_init_t {
	short unk1;
	short w;
	short h;
	short unk3;
	int unk2;
	int subformat;
	int unk5;
	int format;
} rv_init_t;

// init driver
static int init(sh_video_t *sh){
	//unsigned int out_fmt;
	char path[4096];
	int result;
	// we export codec id and sub-id from demuxer in bitmapinfohdr:
	unsigned int* extrahdr=(unsigned int*)(sh->bih+1);
	struct rv_init_t init_data={
		11, sh->disp_w, sh->disp_h,0,0,extrahdr[0],
		1,extrahdr[1]}; // rv30

	mp_msg(MSGT_DECVIDEO,MSGL_V,"realvideo codec id: 0x%08X  sub-id: 0x%08X\n",extrahdr[1],extrahdr[0]);

	sprintf(path, LIBDIR "/real/%s", sh->codec->dll);
	if(!load_syms(path)){
		mp_msg(MSGT_DECVIDEO,MSGL_ERR,MSGTR_MissingDLLcodec,sh->codec->dll);
		mp_msg(MSGT_DECVIDEO,MSGL_HINT,"You need to copy the contents of the codecs directory from RealPlayer8\n");
		mp_msg(MSGT_DECVIDEO,MSGL_HINT,"into " LIBDIR "/real/ !\n");
		return 0;
	}
	// only I420 supported
	if(!mpcodecs_config_vo(sh,sh->disp_w,sh->disp_h,IMGFMT_I420)) return 0;
	// init codec:
	sh->context=NULL;
	result=(*rvyuv_init)(&init_data, &sh->context);
	if (result){
	    mp_msg(MSGT_DECVIDEO,MSGL_ERR,"Couldn't open RealVideo codec, error code: 0x%X  \n",result);
	    return 0;
	}
	// setup rv30 codec (codec sub-type and image dimensions):
	if(extrahdr[1]>=0x30000000){
	    ulong cmsg24[4]={sh->disp_w,sh->disp_h,sh->disp_w,sh->disp_h};
	    ulong cmsg_data[3]={0x24,1+((extrahdr[0]>>16)&7),&cmsg24};
	    (*rvyuv_custom_message)(cmsg_data,sh->context);
	}
	mp_msg(MSGT_DECVIDEO,MSGL_V,"INFO: RealVideo codec init OK!\n");
	return 1;
}

// uninit driver
static void uninit(sh_video_t *sh){
	if(rv_handle) dlclose(rv_handle);
	rv_handle=NULL;
}

// decode a frame
static mp_image_t* decode(sh_video_t *sh,void* data,int len,int flags){
	mp_image_t* mpi;
	ulong result;
	int *buff=(unsigned int *)((char*)data+len);
	ulong transform_out[5];
	ulong transform_in[6]={
		len,		// length of the packet (sub-packets appended)
		0,		// unknown, seems to be unused
		buff[0],	// number of sub-packets - 1
		&buff[2],	// table of sub-packet offsets
		0,		// unknown, seems to be unused
		buff[1],	// timestamp (the integer value from the stream)
	};

	if(len<=0 || flags&2) return NULL; // skipped frame || hardframedrop

	mpi=mpcodecs_get_image(sh, MP_IMGTYPE_TEMP, 0 /*MP_IMGFLAG_ACCEPT_STRIDE*/,
		sh->disp_w, sh->disp_h);
	if(!mpi) return NULL;
	
	result=(*rvyuv_transform)(data, mpi->planes[0], transform_in,
		transform_out, sh->context);

	return (result?NULL:mpi);
}

#endif

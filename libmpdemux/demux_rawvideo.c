
#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include "../cfgparser.h"

#include "stream.h"
#include "demuxer.h"
#include "stheader.h"

#include "../libmpcodecs/img_format.h"

int use_rawvideo = 0;
static int format = IMGFMT_I420;
static int size_id = 0;
static int width = 0;
static int height = 0;
static float fps = 25;
static int imgsize=0;

config_t demux_rawvideo_opts[] = {
  { "on", &use_rawvideo, CONF_TYPE_FLAG, 0,0, 1, NULL },
  // size:
  { "w", &width, CONF_TYPE_INT,CONF_RANGE,1,8192, NULL },
  { "h", &height, CONF_TYPE_INT,CONF_RANGE,1,8192, NULL },
  { "sqcif", &size_id, CONF_TYPE_FLAG,0,0,1, NULL },
  { "qcif", &size_id, CONF_TYPE_FLAG,0,0,2, NULL },
  { "cif", &size_id, CONF_TYPE_FLAG,0,0,3, NULL },
  { "4cif", &size_id, CONF_TYPE_FLAG,0,0,4, NULL },
  { "pal", &size_id, CONF_TYPE_FLAG,0,0,5, NULL },
  { "ntsc", &size_id, CONF_TYPE_FLAG,0,0,6, NULL },
  // format:
  { "format", &format, CONF_TYPE_INT, 0, 0 , 0, NULL },
  { "y420", &format, CONF_TYPE_FLAG, 0, 0 , IMGFMT_I420, NULL },
  { "yv12", &format, CONF_TYPE_FLAG, 0, 0 , IMGFMT_YV12, NULL },
  { "yuy2", &format, CONF_TYPE_FLAG, 0, 0 , IMGFMT_YUY2, NULL },
  { "y8", &format, CONF_TYPE_FLAG, 0, 0 , IMGFMT_Y8, NULL },
  // misc:
  { "fps", &fps, CONF_TYPE_FLOAT,CONF_RANGE,0.001,1000, NULL },
  { "size", &imgsize, CONF_TYPE_INT, CONF_RANGE, 1 , 8192*8192*4, NULL },

  {NULL, NULL, 0, 0, 0, 0, NULL}
};


int demux_rawvideo_open(demuxer_t* demuxer) {
  sh_video_t* sh_video;

  switch(size_id){
  case 1: width=128; height=96; break;
  case 2: width=176; height=144; break;
  case 3: width=352; height=288; break;
  case 4: width=704; height=576; break;
  case 5: width=720; height=576; break;
  case 6: width=720; height=480; break;
  }
  if(!width || !height){
      mp_msg(MSGT_DEMUX,MSGL_ERR,"rawvideo: width or height not specified!\n");
      return 0;
  }
  
  if(!imgsize)
  switch(format){
  case IMGFMT_I420:
  case IMGFMT_IYUV:
  case IMGFMT_YV12: imgsize=width*height+2*(width>>1)*(height>>1);break;
  case IMGFMT_YUY2: imgsize=width*height*2;break;
  case IMGFMT_Y8: imgsize=width*height;break;
  default:
      mp_msg(MSGT_DEMUX,MSGL_ERR,"rawvideo: img size not specified and unknown format!\n");
      return 0;
  }

  sh_video = new_sh_video(demuxer,0);
  sh_video->format=format;
  sh_video->fps=fps;
  sh_video->frametime=1.0/fps;
  sh_video->disp_w=width;
  sh_video->disp_h=height;
  sh_video->i_bps=fps*imgsize;

  demuxer->movi_start = demuxer->stream->start_pos;
  demuxer->movi_end = demuxer->stream->end_pos;

  demuxer->video->sh = sh_video;
  sh_video->ds = demuxer->video;

  return 1;
}

int demux_rawvideo_fill_buffer(demuxer_t* demuxer, demux_stream_t *ds) {

  if(demuxer->stream->eof) return 0;
  if(ds!=demuxer->video) return 0;

  ds_read_packet(ds,demuxer->stream,imgsize,0,stream_tell(demuxer->stream),0x10);

  return 1;
}

void demux_rawvideo_seek(demuxer_t *demuxer,float rel_seek_secs,int flags){
  stream_t* s = demuxer->stream;
  sh_video_t* sh_video = demuxer->video->sh;
  off_t pos;

  pos = (flags & 1) ? demuxer->movi_start : stream_tell(s);
  if(flags & 2)
    pos += ((demuxer->movi_end - demuxer->movi_start)*rel_seek_secs);
  else
    pos += (rel_seek_secs*sh_video->i_bps);

  pos/=imgsize;
  stream_seek(s,pos*imgsize);
  sh_video->timer=pos * sh_video->frametime;
//  printf("demux_rawvideo: streamtell=%d\n",(int)stream_tell(demuxer->stream));
}

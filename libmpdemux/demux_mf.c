
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "config.h"
#include "mp_msg.h"
#include "help_mp.h"

#include "stream.h"
#include "demuxer.h"
#include "stheader.h"
#include "mf.h"

typedef struct
{
 int nr_of_frames;
 int curr_frame;
} demuxer_mf_t;

void demux_seek_mf(demuxer_t *demuxer,float rel_seek_secs,int flags){
}

// return value:
//     0 = EOF or no stream found
//     1 = successfully read a packet
int demux_mf_fill_buffer(demuxer_t *demuxer){
  mf_t         * mf;
  demuxer_mf_t * dmf;
  struct stat    fs;
  FILE         * f;

  dmf=(demuxer_mf_t*)demuxer->priv;
  if ( dmf->curr_frame >= dmf->nr_of_frames ) return 0;
  mf=(mf_t*)demuxer->stream->priv;

  stat( mf->names[dmf->curr_frame],&fs );
//  printf( "[demux_mf] frame: %d (%s,%d)\n",dmf->curr_frame,mf->names[dmf->curr_frame],fs.st_size );

  if ( !( f=fopen( mf->names[dmf->curr_frame],"r" ) ) ) return 0;
  {
   sh_video_t     * sh_video = demuxer->video->sh;
   demux_packet_t * dp = new_demux_packet( fs.st_size );
//    stream_read(stream,dp->buffer,len);
   if ( !fread( dp->buffer,1,fs.st_size,f ) ) return 0;
   dp->pts=dmf->curr_frame / sh_video->fps;
   dp->pos=dmf->curr_frame;
   dp->flags=0;
   // append packet to DS stream:
   ds_add_packet( demuxer->video,dp );
  }
  fclose( f );

  dmf->curr_frame++;
  return 1;
}

demuxer_t* demux_open_mf(demuxer_t* demuxer){
  sh_video_t   *sh_video = NULL;
  mf_t         *mf = NULL;
  demuxer_mf_t *dmf = NULL;

  mp_msg( MSGT_DEMUX,MSGL_V,"[demux_mf] mf demuxer opened.\n" );
  mf=(mf_t*)demuxer->stream->priv;
  dmf=calloc( 1,sizeof( demuxer_mf_t ) );

  // go back to the beginning
  stream_reset(demuxer->stream);
  stream_seek(demuxer->stream, 0);
  demuxer->movi_start = 0;
  demuxer->movi_end = mf->nr_of_files - 1;
  dmf->nr_of_frames= mf->nr_of_files;
  dmf->curr_frame=0;

  // create a new video stream header
  sh_video = new_sh_video(demuxer, 0);
  // make sure the demuxer knows about the new video stream header
  // (even though new_sh_video() ought to take care of it)
  demuxer->video->sh = sh_video;

  // make sure that the video demuxer stream header knows about its
  // parent video demuxer stream (this is getting wacky), or else
  // video_read_properties() will choke
  sh_video->ds = demuxer->video;

  if ( !strcmp( mf_type,"jpg" ) ) sh_video->format = mmioFOURCC('M', 'J', 'P', 'G');
   else { mp_msg(MSGT_DEMUX, MSGL_INFO, "[demux_mf] unknow input file type.\n" ); free( dmf ); return NULL; }

  sh_video->disp_w = mf_w;
  sh_video->disp_h = mf_h;
  sh_video->fps = mf_fps;
  sh_video->frametime = 1 / sh_video->fps;

  /* disable seeking */
  demuxer->seekable = 0;

  demuxer->priv=(void*)dmf;

  return demuxer;
}

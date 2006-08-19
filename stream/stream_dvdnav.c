#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include "mp_msg.h"
#include "osdep/timer.h"
#include "input/input.h"
#include "stream.h"
#include "libmpdemux/demuxer.h"
#include "stream_dvdnav.h"
#include "libvo/video_out.h"
#include "spudec.h"
#include "m_option.h"
#include "m_struct.h"
#include "help_mp.h"

extern char *dvd_device;
extern char *audio_lang, *dvdsub_lang;

static struct stream_priv_s {
  int track;
  char* device;
} stream_priv_dflts = {
  1,
  NULL
};

#define ST_OFF(f) M_ST_OFF(struct stream_priv_s,f)
/// URL definition
static m_option_t stream_opts_fields[] = {
  {"filename", 	ST_OFF(device), CONF_TYPE_STRING, 0, 0, 0, NULL },
  {"hostname", 	ST_OFF(track), CONF_TYPE_INT, 0, 0, 0, NULL},
  { NULL, NULL, 0, 0, 0, 0,  NULL }
};
static struct m_struct_st stream_opts = {
  "dvd",
  sizeof(struct stream_priv_s),
  &stream_priv_dflts,
  stream_opts_fields
};

int dvd_nav_skip_opening=0;     /* skip opening stalls? */
int osd_show_dvd_nav_delay=0;   /* count down for dvd nav text on OSD */
char dvd_nav_text[50];          /* for reporting stuff to OSD */
int osd_show_dvd_nav_highlight; /* show highlight area */
int osd_show_dvd_nav_sx;        /* start x .... */
int osd_show_dvd_nav_ex;
int osd_show_dvd_nav_sy;
int osd_show_dvd_nav_ey;
int dvd_nav_still=0;            /* are we on a still picture? */

dvdnav_priv_t * new_dvdnav_stream(char * filename) {
  char * title_str;
  dvdnav_priv_t *dvdnav_priv;

  if (!filename)
    return NULL;

  if (!(dvdnav_priv=calloc(1,sizeof(*dvdnav_priv))))
    return NULL;

  if (!(dvdnav_priv->filename=strdup(filename))) {
    free(dvdnav_priv);
    return NULL;
  }

  if(dvdnav_open(&(dvdnav_priv->dvdnav),dvdnav_priv->filename)!=DVDNAV_STATUS_OK)
  {
    free(dvdnav_priv->filename);
    free(dvdnav_priv);
    return NULL;
  }

  if (!dvdnav_priv->dvdnav) {
    free(dvdnav_priv);
    return NULL;
  }

  if(1)	//from vlc: if not used dvdnav from cvs will fail
  {
    int len, event;
    char buf[2048];
    
    dvdnav_get_next_block(dvdnav_priv->dvdnav,buf,&event,&len);
  }
  
  /* turn on dvdnav caching */
  dvdnav_set_readahead_flag(dvdnav_priv->dvdnav, 0);
  if(dvdnav_set_PGC_positioning_flag(dvdnav_priv->dvdnav, 1) != DVDNAV_STATUS_OK)
    mp_msg(MSGT_OPEN,MSGL_ERR,"stream_dvdnav, failed to set PGC positioning\n");
#if 1
  /* report the title?! */
  if (dvdnav_get_title_string(dvdnav_priv->dvdnav,&title_str)==DVDNAV_STATUS_OK) {
    mp_msg(MSGT_IDENTIFY, MSGL_INFO,"Title: '%s'\n",title_str);
  }
#endif

  //dvdnav_event_clear(dvdnav_priv);

  return dvdnav_priv;
}

int dvdnav_stream_reset(dvdnav_priv_t * dvdnav_priv) {
  if (!dvdnav_priv) return 0;

//  if (dvdnav_reset(dvdnav_priv->dvdnav)!=DVDNAV_STATUS_OK)
    return 0;

  dvdnav_priv->started=0;

  return 1;
}

int dvdnav_stream_sleeping(dvdnav_priv_t * dvdnav_priv) {
    unsigned int now;

    if (!dvdnav_priv) return 0;

    if(dvdnav_priv->sleeping)
    {
      now=GetTimer();
      while(dvdnav_priv->sleeping>1 || now<dvdnav_priv->sleep_until) {
//        printf("%s %u<%u\n",__FUNCTION__,now,dvdnav_priv->sleep_until);
//        usec_sleep(1000); /* 1ms granularity */
        return 1; 
      }
      dvdnav_still_skip(dvdnav_priv->dvdnav); // continue past...
      dvdnav_priv->sleeping=0;
      mp_msg(MSGT_OPEN,MSGL_V, "%s: woke up!\n",__FUNCTION__);
    }
    dvd_nav_still=0;
    mp_msg(MSGT_OPEN,MSGL_V, "%s: active\n",__FUNCTION__);
    return 0;
}

void dvdnav_stream_sleep(dvdnav_priv_t * dvdnav_priv, int seconds) {
    if (!dvdnav_priv) return;

    if (!dvdnav_priv->started) return;

    dvdnav_priv->sleeping=0;
    switch (seconds) {
    case 0:
            return;
    case 0xff:
            mp_msg(MSGT_OPEN,MSGL_V, "Sleeping indefinately\n" );
            dvdnav_priv->sleeping=2;
            break;
    default:
            mp_msg(MSGT_OPEN,MSGL_V, "Sleeping %d sec(s)\n", seconds );
            dvdnav_priv->sleep_until = GetTimer();// + seconds*1000000;
            dvdnav_priv->sleeping=1;
            break;
    }
    //if (dvdnav_priv->started) dvd_nav_still=1;
}

void dvdnav_stream_add_event(dvdnav_priv_t* dvdnav_priv, int event, unsigned char *buf, int len) {
  //printf("%s: %d\n",__FUNCTION__,event);

  dvdnav_event_t * dvdnav_event;
  mp_cmd_t * cmd;

  if (!dvdnav_priv->started) return;

  if (!(dvdnav_event=calloc(1,sizeof(*dvdnav_event)))) {
    mp_msg(MSGT_OPEN,MSGL_V, "%s: dvdnav_event: out of memory!\n",__FUNCTION__);
    return;
  }
  dvdnav_event->event=event;
  dvdnav_event->details=calloc(1,len);
  memcpy(dvdnav_event->details,buf,len);
  dvdnav_event->len=len;

  if (!(cmd = calloc(1,sizeof(*cmd)))) {
    mp_msg(MSGT_OPEN,MSGL_V, "%s: mp_cmd_t: out of memory!\n",__FUNCTION__);
    free(dvdnav_event->details);
    free(dvdnav_event);
    return;
  }
  cmd->id=MP_CMD_DVDNAV_EVENT; // S+event;
  cmd->name=strdup("dvdnav_event"); // FIXME: do I really need a 'name'?
  cmd->nargs=1;
  cmd->args[0].v.v=dvdnav_event;
}

int dvdnav_stream_read(dvdnav_priv_t * dvdnav_priv, unsigned char *buf, int *len) {
  int event = DVDNAV_NOP;

  if (!len) return -1;
  *len=-1;
  if (!dvdnav_priv) return -1;
  if (!buf) return -1;

  if (dvd_nav_still) {
    mp_msg(MSGT_OPEN,MSGL_V, "%s: got a stream_read while I should be asleep!\n",__FUNCTION__);
    *len=0;
    return -1;
  }

  if (dvdnav_get_next_block(dvdnav_priv->dvdnav,buf,&event,len)!=DVDNAV_STATUS_OK) {
    mp_msg(MSGT_OPEN,MSGL_V, "Error getting next block from DVD %d (%s)\n",event, dvdnav_err_to_string(dvdnav_priv->dvdnav) );
    *len=-1;
  }
  else if (event!=DVDNAV_BLOCK_OK) {

    // need to handle certain events internally (like skipping stills)
    switch (event) {
    case DVDNAV_STILL_FRAME: {
      dvdnav_still_event_t *still_event = (dvdnav_still_event_t*)(buf);
      //if (dvdnav_priv->started) dvd_nav_still=1;
      //else
        dvdnav_still_skip(dvdnav_priv->dvdnav); // don't let dvdnav stall on this image

      break;
    }
    case DVDNAV_WAIT:
        dvdnav_wait_skip(dvdnav_priv->dvdnav);
        break;
    }

    // got an event, repeat the read
    dvdnav_stream_add_event(dvdnav_priv,event,buf,*len);
    *len=0;
  }
//  printf("%s: got %d\n",__FUNCTION__,*len);
  return event;
}

void dvdnav_stream_fullstart(dvdnav_priv_t * dvdnav_priv) {
  if (dvdnav_priv && !dvdnav_priv->started) {
    dvdnav_stream_reset(dvdnav_priv);
    dvdnav_priv->started=1;
  }
}

unsigned int * dvdnav_stream_get_palette(dvdnav_priv_t * dvdnav_priv) {
  if (!dvdnav_priv) {
    mp_msg(MSGT_OPEN,MSGL_V, "%s: NULL dvdnav_priv\n",__FUNCTION__);
    return NULL;
  }
  if (!dvdnav_priv->dvdnav) {
    mp_msg(MSGT_OPEN,MSGL_V, "%s: NULL dvdnav_priv->dvdnav\n",__FUNCTION__);
    return NULL;
  }
}

static void update_title_len(stream_t *stream) {
  dvdnav_priv_t *priv = stream->priv;
  dvdnav_status_t status;
  uint32_t pos = 0, len = 0;

  status = dvdnav_get_position(priv->dvdnav, &pos, &len);
  if(status == DVDNAV_STATUS_OK && len)
    stream->end_pos = (off_t) len * 2048;
}
  

static int seek(stream_t *s, off_t newpos) {
uint32_t pos = 0, len = 0, sector = 0;
dvdnav_priv_t *priv = s->priv;

  if(newpos==0) {
    if(dvdnav_stream_reset(priv->dvdnav))
      s->pos=0;
  }
  else {
    if(s->end_pos && newpos > s->end_pos) 
       newpos = s->end_pos;
    sector = newpos / 2048ULL;
    if(dvdnav_sector_search(priv->dvdnav, (uint64_t) sector, SEEK_SET) != DVDNAV_STATUS_OK)
      goto fail;

    s->pos = newpos;
  }
  
  return 1;
  
fail:
    mp_msg(MSGT_STREAM,MSGL_INFO,"dvdnav_stream, seeking to %"PRIu64" failed: %s\n", newpos, dvdnav_err_to_string(priv->dvdnav));

  return 1;
}

static void stream_dvdnav_close(stream_t *s) {
  dvdnav_priv_t *priv = s->priv;
  dvdnav_close(priv->dvdnav);
  priv->dvdnav = NULL;
  free(priv);
}


static int fill_buffer(stream_t *s, char *but, int len)
{
    int event;
    dvdnav_priv_t* dvdnav_priv=s->priv;
    len=0;
    if(!s->end_pos)
      update_title_len(s);
    while(!len) /* grab all event until DVDNAV_BLOCK_OK (len=2048), DVDNAV_STOP or DVDNAV_STILL_FRAME */
    {
        if(-1==(event=dvdnav_stream_read(dvdnav_priv, s->buffer, &len)) || len==-1)
        {
            mp_msg(MSGT_CPLAYER,MSGL_ERR, "DVDNAV stream read error!\n");
            return 0;
        }
        switch (event) {
            case DVDNAV_STOP: return len;
	    case DVDNAV_BLOCK_OK: return len;
#if 0
            case DVDNAV_STILL_FRAME: {
                if(!dvdnav_priv->stillok) dvdnav_priv->stillcounter++;
                dvdnav_priv->lockstillcounter++;
                return len;
                break; 
            }
	    
            case DVDNAV_WAIT: {
                if(dvdnav_priv->waitcounter>=DVDNAV_MAX_WAIT_FRAME) return len;
                break;
            }
#endif
        }
#if 0
        if(dvdnav_priv->event.cell_really_change &&
            dvdnav_priv->started &&
	    !dvdnav_priv->vts_domain) 
	        return len;
#endif
  }
  mp_msg(MSGT_STREAM,MSGL_DBG2,"DVDNAV fill_buffer len: %d\n",len);
  return len;
}

static int control(stream_t *stream, int cmd, void* arg) {
  dvdnav_priv_t* dvdnav_priv=stream->priv;
  int tit, part;

  switch(cmd) 
  {
    case STREAM_CTRL_SEEK_TO_CHAPTER:
    {
      int chap = *((unsigned int *)arg)+1;

      if(chap < 1 || dvdnav_current_title_info(dvdnav_priv->dvdnav, &tit, &part) != DVDNAV_STATUS_OK)
        break;
      if(dvdnav_part_play(dvdnav_priv->dvdnav, tit, chap) != DVDNAV_STATUS_OK)
        break;
      return 1;
    }
    case STREAM_CTRL_GET_CURRENT_CHAPTER:
    {
      if(dvdnav_current_title_info(dvdnav_priv->dvdnav, &tit, &part) != DVDNAV_STATUS_OK)
        break;
      *((unsigned int *)arg) = part - 1;
      return 1;
    }
  }

  return STREAM_UNSUPORTED;
}

static int open_s(stream_t *stream,int mode, void* opts, int* file_format) {
  struct stream_priv_s* p = (struct stream_priv_s*)opts;
  char *filename;
  int event,len,tmplen=0;
  uint32_t pos, l2;
  dvdnav_priv_t *dvdnav_priv;
  dvdnav_status_t status;

  //mp_msg(MSGT_OPEN,MSGL_INFO,"URL: %s\n", filename);
  
  if(p->device) filename = p->device; 
  else if(dvd_device) filename= dvd_device; 
  else filename = DEFAULT_DVD_DEVICE;
  if(!(dvdnav_priv=new_dvdnav_stream(filename))) {
    mp_msg(MSGT_OPEN,MSGL_ERR,MSGTR_CantOpenDVD,filename);
    return STREAM_UNSUPORTED;
  }

  if(dvdnav_title_play(dvdnav_priv->dvdnav, p->track) != DVDNAV_STATUS_OK) {
    mp_msg(MSGT_OPEN,MSGL_FATAL,"dvdnav_stream, couldn't select title %d, error '%s'\n", p->track, dvdnav_err_to_string(dvdnav_priv->dvdnav));
    return STREAM_UNSUPORTED;
  }

  stream->sector_size = 2048;
  stream->flags = STREAM_READ | STREAM_SEEK;
  stream->fill_buffer = fill_buffer;
  stream->seek = seek;
  stream->control = control;
  stream->close = stream_dvdnav_close;
  stream->type = STREAMTYPE_DVDNAV;
  stream->priv=(void*)dvdnav_priv;
  *file_format = DEMUXER_TYPE_MPEG_PS;

  update_title_len(stream);
  if(!stream->pos)
    mp_msg(MSGT_OPEN,MSGL_ERR, "INIT ERROR: %d, couldn't get init pos %s\r\n", status, dvdnav_err_to_string(dvdnav_priv->dvdnav));

  return STREAM_OK;
}

stream_info_t stream_info_dvdnav = {
  "DVDNAV stream",
  "null",
  "",
  "",
  open_s,
  { "dvdnav", NULL },
  &stream_opts,
  1 // Urls are an option string
};


#include "config.h"

#define MAX_PACKS 4096
#define MAX_PACK_BYTES 0x800000

#define DEMUXER_TYPE_UNKNOWN 0
#define DEMUXER_TYPE_MPEG_ES 1
#define DEMUXER_TYPE_MPEG_PS 2
#define DEMUXER_TYPE_AVI 3
#define DEMUXER_TYPE_AVI_NI 4
#define DEMUXER_TYPE_AVI_NINI 5
#define DEMUXER_TYPE_ASF 6

#define DEMUXER_TIME_NONE 0
#define DEMUXER_TIME_PTS 1
#define DEMUXER_TIME_FILE 2
#define DEMUXER_TIME_BPS 3


typedef struct demux_packet_st {
  int len;
  float pts;
  int pos;  // pozicio indexben (AVI) ill. fileban (MPG)
  unsigned char* buffer;
  int flags; // keyframe, etc
  struct demux_packet_st* next;
} demux_packet_t;

typedef struct {
  int buffer_pos;          // current buffer position
  int buffer_size;         // current buffer size
  unsigned char* buffer;   // current buffer
  float pts;               // current buffer's pts
  int pts_bytes;           // number of bytes read after last pts stamp
  int eof;                 // end of demuxed stream? (true if all buffer empty)
  int pos;                 // position in the input stream (file)
  int dpos;                // position in the demuxed stream
  int pack_no;		   // serial number of packet
  int flags;               // flags of current packet (keyframe etc)
//---------------
  int packs;              // number of packets in buffer
  int bytes;              // total bytes of packets in buffer
  demux_packet_t *first;  // read to current buffer from here
  demux_packet_t *last;   // append new packets from input stream to here
  int id;                 // stream ID  (for multiple audio/video streams)
  struct demuxer_st *demuxer; // parent demuxer structure (stream handler)
// ---- asf -----
  demux_packet_t *asf_packet;  // read asf fragments here
  int asf_seq;
// ---- stream header ----
  void* sh;
} demux_stream_t;

typedef struct demuxer_st {
  stream_t *stream;
  int synced;  // stream synced (used by mpeg)
  int filepos; // input stream current pos.
  int type;    // demuxer type: mpeg PS, mpeg ES, avi, avi-ni, avi-nini, asf
  int file_format;  // file format: mpeg/avi/asf
//  int time_src;// time source (pts/file/bps)
  unsigned int movi_start;
  unsigned int movi_end;
  //
  demux_stream_t *audio;
  demux_stream_t *video;
  demux_stream_t *sub;

  // index:
//  AVIINDEXENTRY* idx;
  void* idx;
  int idx_size;
  int idx_pos;
  int idx_pos_a;
  int idx_pos_v;
  int idx_offset;  // ennyit kell hozzaadni az index offset ertekekhez

  // stream headers:
//  sh_audio_t* a_streams[256];
//  sh_video_t* v_streams[256];
  void* a_streams[256];
  void* v_streams[256];
  char s_streams[32]; // dvd subtitles
} demuxer_t;

inline static demux_packet_t* new_demux_packet(int len){
  demux_packet_t* dp=malloc(sizeof(demux_packet_t));
  dp->len=len;
  dp->buffer=malloc(len);
  dp->next=NULL;
  dp->pts=0;
  dp->pos=0;
  dp->flags=0;
  return dp;
}

inline static void free_demux_packet(demux_packet_t* dp){
  free(dp->buffer);
  free(dp);
}

demux_stream_t* new_demuxer_stream(struct demuxer_st *demuxer,int id);
demuxer_t* new_demuxer(stream_t *stream,int type,int a_id,int v_id,int s_id);

void ds_add_packet(demux_stream_t *ds,demux_packet_t* dp);
void ds_read_packet(demux_stream_t *ds,stream_t *stream,int len,float pts,int pos,int flags);

int demux_fill_buffer(demuxer_t *demux,demux_stream_t *ds);
int ds_fill_buffer(demux_stream_t *ds);

inline static int ds_tell(demux_stream_t *ds){
  return (ds->dpos-ds->buffer_size)+ds->buffer_pos;
}

inline static int ds_tell_pts(demux_stream_t *ds){
  return (ds->pts_bytes-ds->buffer_size)+ds->buffer_pos;
}

int demux_read_data(demux_stream_t *ds,unsigned char* mem,int len);
int demux_read_data_pack(demux_stream_t *ds,unsigned char* mem,int len);

#if 1
#define demux_getc(ds) (\
     (ds->buffer_pos<ds->buffer_size) ? ds->buffer[ds->buffer_pos++] \
     :((!ds_fill_buffer(ds))? (-1) : ds->buffer[ds->buffer_pos++] ) )
#else
inline static int demux_getc(demux_stream_t *ds){
  if(ds->buffer_pos>=ds->buffer_size){
    if(!ds_fill_buffer(ds)){
//      printf("DEMUX_GETC: EOF reached!\n");
      return -1; // EOF
    }
  }
//  printf("[%02X]",ds->buffer[ds->buffer_pos]);
  return ds->buffer[ds->buffer_pos++];
}
#endif

void ds_free_packs(demux_stream_t *ds);
int ds_get_packet(demux_stream_t *ds,unsigned char **start);
int ds_get_packet_sub(demux_stream_t *ds,unsigned char **start);


static inline int avi_stream_id(unsigned int id){
  unsigned char *p=(unsigned char *)&id;
  unsigned char a,b;
#if WORDS_BIGENDIAN
  a=p[3]-'0'; b=p[2]-'0';
#else
  a=p[0]-'0'; b=p[1]-'0';
#endif
  if(a>9 || b>9) return 100; // invalid ID
  return a*10+b;
}




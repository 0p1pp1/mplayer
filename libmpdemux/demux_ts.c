/*
 * Demultiplexer for MPEG2 Transport Streams.
 *
 * Written by Nico <nsabbi@libero.it>
 * Modified by 0p1pp1
 * Kind feedback is appreciated; 'sucks' and alike is not.
 * Originally based on demux_pva.c written by Matteo Giani and FFmpeg (libavformat) sources
 *
 * This file is part of MPlayer.
 *
 * MPlayer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * MPlayer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with MPlayer; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"
#include "mp_msg.h"
#include "mpcommon.h"
#include "help_mp.h"

#include "libmpcodecs/dec_audio.h"
#include "stream/stream.h"
#include "demuxer.h"
#include "parse_es.h"
#include "stheader.h"
#include "ms_hdr.h"
#include "mpeg_hdr.h"
#include "demux_ts.h"

#if CONFIG_DEMULTI2
#include <demulti2.h>
#endif

#define TS_PH_PACKET_SIZE 192
#define TS_FEC_PACKET_SIZE 204
#define TS_PACKET_SIZE 188
#define NB_PID_MAX 8192

#define MAX_HEADER_SIZE 6			/* enough for PES header + length */
#define MAX_CHECK_SIZE	65535
#define NUM_CONSECUTIVE_TS_PACKETS 32
#define NUM_CONSECUTIVE_AUDIO_PACKETS 348
#define MAX_A52_FRAME_SIZE 3840

#ifndef SIZE_MAX
#define SIZE_MAX ((size_t)-1)
#endif

#define TYPE_AUDIO 1
#define TYPE_VIDEO 2
#define TYPE_SUB   3

// for indicating a PSI section as not received yet
#define VERSION_NONE 0x20

int ts_prog;
int ts_keep_broken=0;
off_t ts_probe = 0;
int audio_substream_id = -1;
int low_cn = 0;

typedef enum
{
	UNKNOWN		= -1,
	VIDEO_MPEG1 	= 0x10000001,
	VIDEO_MPEG2 	= 0x10000002,
	VIDEO_MPEG4 	= 0x10000004,
	VIDEO_H264 	= 0x10000005,
	VIDEO_AVC	= mmioFOURCC('a', 'v', 'c', '1'),
	VIDEO_DIRAC	= mmioFOURCC('d', 'r', 'a', 'c'),
	VIDEO_HEVC	= mmioFOURCC('H', 'E', 'V', 'C'),
	VIDEO_VC1	= mmioFOURCC('W', 'V', 'C', '1'),
	AUDIO_MP2   	= 0x50,
	AUDIO_A52   	= 0x2000,
	AUDIO_DTS	= 0x2001,
	AUDIO_LPCM_BE  	= 0x10001,
	AUDIO_AAC	= mmioFOURCC('M', 'P', '4', 'A'),
	AUDIO_AAC_LATM	= mmioFOURCC('M', 'P', '4', 'L'),
	AUDIO_TRUEHD	= mmioFOURCC('T', 'R', 'H', 'D'),
	AUDIO_S302M     = mmioFOURCC('B', 'S', 'S', 'D'),
	AUDIO_PCM_BR    = mmioFOURCC('B', 'P', 'C', 'M'),
	SPU_DVD		= 0x3000000,
	SPU_DVB		= 0x3000001,
	SPU_TELETEXT	= 0x3000002,
	SPU_PGS		= 0x3000003,
	PES_PRIVATE1	= 0xBD00000,
	SL_PES_STREAM	= 0xD000000,
	SL_SECTION	= 0xD100000,
	MP4_OD		= 0xD200000,
} es_stream_type_t;

typedef struct {
	uint8_t *buffer;
	uint16_t buffer_len;
} ts_section_t;

typedef struct {
	int size;
	unsigned char *start;
	uint16_t payload_size;
	es_stream_type_t type, subtype;
	double pts, last_pts;
	int pid;
	int ecm_pid;
	char lang[4];
	// copy from PMT
	char lang2[4];
	int is_dualmono;
	int component_tag;
	// pointer to PMT
	int prog_idx;
	int es_idx;
	int last_cc;				// last cc code (-1 if first packet)
	int is_synced;
	ts_section_t section;
	uint8_t *extradata;
	int extradata_alloc, extradata_len;
	struct {
		uint8_t au_start, au_end, last_au_end;
	} sl;
} ES_stream_t;

typedef struct {
	void *sh;
	int id;
	int type;
} sh_av_t;

typedef struct {
  uint16_t cas_id;
  uint16_t pid;
  uint8_t emm_type;
} cas_t;

typedef struct {
	cas_t cas;
	uint8_t version_number;

	ts_section_t section;
} ecm_t;

typedef struct MpegTSContext {
	int packet_size; 		// raw packet size, including FEC if present e.g. 188 bytes
	ES_stream_t *pids[NB_PID_MAX];
	sh_av_t streams[NB_PID_MAX];
	ecm_t *ecms[NB_PID_MAX];
} MpegTSContext;


typedef struct {
	demux_stream_t *ds;
	demux_packet_t *pack;
	int offset, buffer_size;
} av_fifo_t;

#define MAX_EXTRADATA_SIZE 64*1024
typedef struct {
	int32_t object_type;	//aka codec used
	int32_t stream_type;	//video, audio etc.
	uint8_t buf[MAX_EXTRADATA_SIZE];
	uint16_t buf_size;
	uint8_t szm1;
} mp4_decoder_config_t;

typedef struct {
	//flags
	uint8_t flags;
	uint8_t au_start;
	uint8_t au_end;
	uint8_t random_accesspoint;
	uint8_t random_accesspoint_only;
	uint8_t padding;
	uint8_t use_ts;
	uint8_t idle;
	uint8_t duration;

	uint32_t ts_resolution, ocr_resolution;
	uint8_t ts_len, ocr_len, au_len, instant_bitrate_len, degr_len, au_seqnum_len, packet_seqnum_len;
	uint32_t timescale;
	uint16_t au_duration, cts_duration;
	uint64_t ocr, dts, cts;
} mp4_sl_config_t;

typedef struct {
	uint16_t id;
	uint8_t flags;
	mp4_decoder_config_t decoder;
	mp4_sl_config_t sl;
} mp4_es_descr_t;

typedef struct {
	uint16_t id;
	uint8_t flags;
	mp4_es_descr_t *es;
	uint16_t es_cnt;
} mp4_od_t;

typedef struct {
	uint8_t skip;
	uint8_t table_id;
	uint8_t ssi;
	uint16_t section_length;
	uint16_t ts_id;
	uint8_t version_number;
	uint8_t curr_next;
	uint8_t section_number;
	uint8_t last_section_number;
	struct pat_progs_t {
		uint16_t id;
		uint16_t pmt_pid;
	} *progs;
	uint16_t progs_cnt;
	ts_section_t section;
} pat_t;

typedef struct {
	uint16_t progid;
	uint8_t skip;
	uint8_t table_id;
	uint8_t ssi;
	uint16_t section_length;
	uint8_t version_number;
	uint8_t curr_next;
	uint8_t section_number;
	uint8_t last_section_number;
	uint16_t PCR_PID;
	uint16_t prog_descr_length;
	cas_t prog_ecm;
	ts_section_t section;
	uint16_t es_cnt;
	struct pmt_es_t {
		uint16_t pid;
		uint32_t type;	//it's 8 bit long, but cast to the right type as FOURCC
		uint16_t descr_length;
		uint8_t format_descriptor[5];
		uint8_t lang[4];
		uint8_t lang2[4];
		uint8_t is_dualmono;
		uint16_t mp4_es_id;
		cas_t es_ecm;
		int component_tag;
		uint16_t highCN_pid; // alternative PES for high C/N condition(ISDB-S)
	} *es;
	mp4_od_t iod, *od;
	mp4_es_descr_t *mp4es;
	int od_cnt, mp4es_cnt;
	uint8_t eit_version;
} pmt_t;

typedef struct {
	uint64_t size;
	float duration;
	double first_pts;
	double last_pts;
} TS_stream_info;

typedef struct {
	MpegTSContext ts;
	int last_pid;
	av_fifo_t fifo[3];	//0 for audio, 1 for video, 2 for subs
	pat_t pat;
	pmt_t *pmt;
	uint16_t pmt_cnt;
	void *dm2_handle;
	cas_t emm;
	double pcr_delta;
	uint32_t prog;
	uint32_t vbitrate;
	int keep_broken;
	int last_aid;
	int last_vid;
	int last_sid;
	char packet[TS_FEC_PACKET_SIZE];
	TS_stream_info vstr, astr;
	ts_section_t eit_section;
} ts_priv_t;


typedef struct {
	es_stream_type_t type;
	ts_section_t section;
} TS_pids_t;


static int IS_AUDIO(es_stream_type_t type)
{
	switch (type) {
	case AUDIO_MP2:
	case AUDIO_A52:
	case AUDIO_LPCM_BE:
	case AUDIO_PCM_BR:
	case AUDIO_AAC:
	case AUDIO_AAC_LATM:
	case AUDIO_DTS:
	case AUDIO_TRUEHD:
	case AUDIO_S302M:
		return 1;
	}
	return 0;
}

static int IS_VIDEO(es_stream_type_t type)
{
	switch (type) {
	case VIDEO_MPEG1:
	case VIDEO_MPEG2:
	case VIDEO_MPEG4:
	case VIDEO_H264:
	case VIDEO_AVC:
	case VIDEO_DIRAC:
	case VIDEO_HEVC:
	case VIDEO_VC1:
		return 1;
	}
	return 0;
}

static int IS_SUB(es_stream_type_t type)
{
	switch (type) {
	case SPU_DVD:
	case SPU_DVB:
	case SPU_PGS:
	case SPU_TELETEXT:
		return 1;
	}
	return 0;
}

static int ts_parse(demuxer_t *demuxer, ES_stream_t *es, unsigned char *packet, int probe);

static uint8_t get_packet_size(const unsigned char *buf, int size)
{
	int i;

	if (size < (TS_FEC_PACKET_SIZE * NUM_CONSECUTIVE_TS_PACKETS))
		return 0;

	for(i=0; i<NUM_CONSECUTIVE_TS_PACKETS; i++)
	{
		if (buf[i * TS_PACKET_SIZE] != 0x47)
		{
			mp_msg(MSGT_DEMUX, MSGL_DBG2, "GET_PACKET_SIZE, pos %d, char: %2x\n", i, buf[i * TS_PACKET_SIZE]);
			goto try_fec;
		}
	}
	return TS_PACKET_SIZE;

try_fec:
	for(i=0; i<NUM_CONSECUTIVE_TS_PACKETS; i++)
	{
		if (buf[i * TS_FEC_PACKET_SIZE] != 0x47){
			mp_msg(MSGT_DEMUX, MSGL_DBG2, "GET_PACKET_SIZE, pos %d, char: %2x\n", i, buf[i * TS_PACKET_SIZE]);
			goto try_philips;
		}
	}
	return TS_FEC_PACKET_SIZE;

 try_philips:
	for(i=0; i<NUM_CONSECUTIVE_TS_PACKETS; i++)
	{
		if (buf[i * TS_PH_PACKET_SIZE] != 0x47)
		return 0;
	}
	return TS_PH_PACKET_SIZE;
}

static int parse_avc_sps(uint8_t *buf, int len, int *w, int *h);
static uint8_t *pid_lang_from_pmt(ts_priv_t *priv, int pid);

static void ts_add_stream(demuxer_t * demuxer, ES_stream_t *es)
{
	int i;
	ts_priv_t *priv = (ts_priv_t*) demuxer->priv;

	if(priv->ts.streams[es->pid].sh)
	{
		sh_common_t *sh;
		int ctag;

		ctag = es->component_tag;
		sh = priv->ts.streams[es->pid].sh;
		sh->default_track = ctag == 0x00 || ctag == 0x10 || ctag == 0x30;

		if (es->lang[0]) {
			free(sh->lang);
			sh->lang = strdup(es->lang);
		}

		if (es->lang2[0] &&
		    (IS_AUDIO(es->type) || IS_AUDIO(es->subtype))) {
			sh_audio_t *sh_audio = (sh_audio_t *) sh;

			free(sh_audio->lang2);
			sh_audio->lang2 = strdup(es->lang2);
		}
		return;
	}

	if((IS_AUDIO(es->type) || IS_AUDIO(es->subtype)) && priv->last_aid+1 < MAX_A_STREAMS)
	{
		sh_audio_t *sh = new_sh_audio_aid(demuxer, priv->last_aid, es->pid, pid_lang_from_pmt(priv, es->pid));
		if(sh)
		{
			sh->needs_parsing = 1;
			sh->format = IS_AUDIO(es->type) ? es->type : es->subtype;
			sh->ds = demuxer->audio;
			sh->default_track = (es->component_tag == 0x10);
			if (es->lang[0])
				sh->lang = strdup(es->lang);
			if (es->lang2[0])
				sh->lang2 = strdup(es->lang2);

			priv->ts.streams[es->pid].id = priv->last_aid;
			priv->ts.streams[es->pid].sh = sh;
			priv->ts.streams[es->pid].type = TYPE_AUDIO;
			mp_msg(MSGT_DEMUX, MSGL_V, "\r\nADDED AUDIO PID %d, type: %x stream n. %d\r\n", es->pid, sh->format, priv->last_aid);
			priv->last_aid++;

			if(es->extradata && es->extradata_len)
			{
				sh->wf = malloc(sizeof(*sh->wf) + es->extradata_len);
				sh->wf->cbSize = es->extradata_len;
				memcpy(sh->wf + 1, es->extradata, es->extradata_len);
			}
		}
	}

	if((IS_VIDEO(es->type) || IS_VIDEO(es->subtype)) && priv->last_vid+1 < MAX_V_STREAMS)
	{
		sh_video_t *sh = new_sh_video_vid(demuxer, priv->last_vid, es->pid);
		if(sh)
		{
			sh->format = IS_VIDEO(es->type) ? es->type : es->subtype;
			sh->ds = demuxer->video;
			sh->default_track = (es->component_tag == 0x00);

			priv->ts.streams[es->pid].id = priv->last_vid;
			priv->ts.streams[es->pid].sh = sh;
			priv->ts.streams[es->pid].type = TYPE_VIDEO;
			mp_msg(MSGT_DEMUX, MSGL_V, "\r\nADDED VIDEO PID %d, type: %x stream n. %d\r\n", es->pid, sh->format, priv->last_vid);
			priv->last_vid++;


			if(sh->format == VIDEO_AVC && es->extradata && es->extradata_len)
			{
				int w = 0, h = 0;
				sh->bih = calloc(1, sizeof(*sh->bih) + es->extradata_len);
				sh->bih->biSize= sizeof(*sh->bih) + es->extradata_len;
				sh->bih->biCompression = sh->format;
				memcpy(sh->bih + 1, es->extradata, es->extradata_len);
				mp_msg(MSGT_DEMUXER,MSGL_DBG2, "EXTRADATA(%d BYTES): \n", es->extradata_len);
				for(i = 0;i < es->extradata_len; i++)
					mp_msg(MSGT_DEMUXER,MSGL_DBG2, "%02x ", (int) es->extradata[i]);
				mp_msg(MSGT_DEMUXER,MSGL_DBG2,"\n");
				if(parse_avc_sps(es->extradata, es->extradata_len, &w, &h))
				{
					sh->bih->biWidth = w;
					sh->bih->biHeight = h;
				}
			}
		}
	}

	if(IS_SUB(es->type) && priv->last_sid+1 < MAX_S_STREAMS)
	{
		sh_sub_t *sh = new_sh_sub_sid(demuxer, priv->last_sid, es->pid, pid_lang_from_pmt(priv, es->pid));
 		if (sh) {
			switch (es->type) {
			case SPU_DVB:
				sh->type = 'b'; break;
			case SPU_DVD:
				sh->type = 'v'; break;
			case SPU_PGS:
				sh->type = 'p'; break;
			case SPU_TELETEXT:
				sh->type = 'd'; break;
        		}
			sh->default_track = (es->component_tag == 0x30);
			if (es->lang[0])
				sh->lang = strdup(es->lang);
			priv->ts.streams[es->pid].id = priv->last_sid;
			priv->ts.streams[es->pid].sh = sh;
			priv->ts.streams[es->pid].type = TYPE_SUB;
			priv->last_sid++;
		}
	}
}

static int ts_check_file(demuxer_t * demuxer)
{
	const int buf_size = (TS_FEC_PACKET_SIZE * NUM_CONSECUTIVE_TS_PACKETS);
	unsigned char buf[TS_FEC_PACKET_SIZE * NUM_CONSECUTIVE_TS_PACKETS], done = 0, *ptr;
	uint32_t _read, i, count = 0, is_ts;
	int cc[NB_PID_MAX], last_cc[NB_PID_MAX], pid, cc_ok, c, good, bad;
	uint8_t size = 0;
	off_t pos = 0;
	off_t init_pos;

	mp_msg(MSGT_DEMUX, MSGL_V, "Checking for MPEG-TS...\n");

	init_pos = stream_tell(demuxer->stream);
	is_ts = 0;
	while(! done)
	{
		i = 1;
		c = 0;

		while(((c=stream_read_char(demuxer->stream)) != 0x47)
			&& (c >= 0)
			&& (i < MAX_CHECK_SIZE)
			&& ! demuxer->stream->eof
		) i++;


		if(c != 0x47)
		{
			mp_msg(MSGT_DEMUX, MSGL_V, "THIS DOESN'T LOOK LIKE AN MPEG-TS FILE!\n");
			is_ts = 0;
			done = 1;
			continue;
		}

		pos = stream_tell(demuxer->stream) - 1;
		buf[0] = c;
		_read = stream_read(demuxer->stream, &buf[1], buf_size-1);

		if(_read < buf_size-1)
		{
			mp_msg(MSGT_DEMUX, MSGL_V, "COULDN'T READ ENOUGH DATA, EXITING TS_CHECK\n");
			stream_reset(demuxer->stream);
			return 0;
		}

		size = get_packet_size(buf, buf_size);
		if(size)
		{
			done = 1;
			is_ts = 1;
		}

		if(pos - init_pos >= MAX_CHECK_SIZE)
		{
			done = 1;
			is_ts = 0;
		}
	}

	mp_msg(MSGT_DEMUX, MSGL_V, "TRIED UP TO POSITION %"PRIu64", FOUND %x, packet_size= %d, SEEMS A TS? %d\n", (uint64_t) pos, c, size, is_ts);
	stream_seek(demuxer->stream, pos);

	if(! is_ts)
	  return 0;

	//LET'S CHECK continuity counters
	good = bad = 0;
	for(count = 0; count < NB_PID_MAX; count++)
	{
		cc[count] = last_cc[count] = -1;
	}

	for(count = 0; count < NUM_CONSECUTIVE_TS_PACKETS; count++)
	{
		ptr = &(buf[size * count]);
		pid = ((ptr[1] & 0x1f) << 8) | ptr[2];
		mp_msg(MSGT_DEMUX, MSGL_DBG2, "BUF: %02x %02x %02x %02x, PID %d, SIZE: %d \n",
		ptr[0], ptr[1], ptr[2], ptr[3], pid, size);

		if((pid == 8191) || (pid < 16) || (ptr[1] & 0x80))
			continue;

		cc[pid] = (ptr[3] & 0xf);
		cc_ok = (last_cc[pid] < 0) || ((((last_cc[pid] + !!(ptr[3] & 0x10)) & 0x0f) == cc[pid]));
		mp_msg(MSGT_DEMUX, MSGL_DBG2, "PID %d, COMPARE CC %d AND LAST_CC %d\n", pid, cc[pid], last_cc[pid]);
		if(! cc_ok)
			//return 0;
			bad++;
		else
			good++;

		last_cc[pid] = cc[pid];
	}

	mp_msg(MSGT_DEMUX, MSGL_V, "GOOD CC: %d, BAD CC: %d\n", good, bad);

	if(good >= bad)
			return size;
	else
			return 0;
}

static inline int32_t pid_from_id(ts_priv_t *priv, int id, int type)
{
	int i;

	if (id < 0)
		return -1;

	for (i = 0; i < NB_PID_MAX; i++)
		if (priv->ts.streams[i].id == id && priv->ts.streams[i].type == type)
			break;

	if (i == NB_PID_MAX)
		return -1;

	return i;
}

static int32_t progid_idx_in_pmt(ts_priv_t *priv, uint16_t progid)
{
	int x;

	if(priv->pmt == NULL)
		return -1;

	for(x = 0; x < priv->pmt_cnt; x++)
	{
		if(priv->pmt[x].progid == progid)
			return x;
	}

	return -1;
}


static int32_t progid_for_pid(ts_priv_t *priv, int pid, int32_t req)		//finds the first program listing a pid
{
	int i, j;
	pmt_t *pmt;


	if(priv->pmt == NULL || pid >= 8192)
		return -1;

	i = priv->ts.pids[pid]->prog_idx;
	if (i >= 0) {
		pmt = &priv->pmt[i];
		if (pmt && (req <= 0 || req == pmt->progid))
			return pmt->progid;
	}

	for(i=0; i < priv->pmt_cnt; i++)
	{
		pmt = &(priv->pmt[i]);

		if(pmt->es == NULL)
			continue;

		if (req > 0 && req != pmt->progid)
			continue;

		for(j = 0; j < pmt->es_cnt; j++)
			if(pmt->es[j].pid == pid)
				return pmt->progid;
	}
	return -1;
}

static int32_t prog_pcr_pid(ts_priv_t *priv, int progid)
{
	int i;

	if(priv->pmt == NULL)
		return -1;
	for(i=0; i < priv->pmt_cnt; i++)
	{
		if(priv->pmt[i].progid == progid)
			return priv->pmt[i].PCR_PID;
	}
	return -1;
}


static int pes_match_lang(struct pmt_es_t *pes, char *lang)
{
	int len;

	if (!pes)
		return 0;

	mp_msg(MSGT_DEMUXER, MSGL_V, "CMP LANG %s AND %s, pid: %#04x\n",
		pes->lang, lang, pes->pid);

	lang += strspn(lang, ",");
	while ((len = strcspn(lang, ",")) > 0) {
		if (! strncmp(pes->lang, lang, len))
			return 1;
		if (pes->is_dualmono && ! strncmp(pes->lang2, lang, len))
			return 2;
		lang += len;
		lang += strspn(lang, ",");
	}

	return 0;
}

typedef struct {
	int32_t atype, vtype, stype;	//types
	int32_t apid, vpid, spid;	//stream ids
	char alang[32];	//languages
	char slang[32];
	uint16_t prog;
	off_t probe;
	int scrambled;
} tsdemux_init_t;

//second stage: returns the count of A52 syncwords found
static int a52_check(char *buf, int len)
{
	int cnt, frame_length = 0, ok, srate;

	cnt = ok = 0;
	if(len < 8)
		return 0;

	while(cnt < len - 7)
	{
		if(buf[cnt] == 0x0B && buf[cnt+1] == 0x77)
		{
			frame_length = mp_a52_framesize(&buf[cnt], &srate);
			if(frame_length>=7 && frame_length<=3840)
			{
				cnt += frame_length;
				ok++;
			}
			else
			    cnt++;
		}
		else
			cnt++;
	}

	mp_msg(MSGT_DEMUXER, MSGL_V, "A52_CHECK(%d input bytes), found %d frame syncwords of %d bytes length\n", len, ok, frame_length);
	return ok;
}


static off_t ts_detect_streams(demuxer_t *demuxer, tsdemux_init_t *param)
{
	int video_found = 0, audio_found = 0, i, num_packets = 0, req_apid, req_vpid, req_spid;
	int prog_candidate;
	int is_audio, is_video, is_sub, has_tables;
	int32_t p, chosen_pid = 0;
	off_t pos=0, ret = 0, init_pos, end_pos;
	ES_stream_t es;
	unsigned char tmp[TS_FEC_PACKET_SIZE];
	ts_priv_t *priv = (ts_priv_t*) demuxer->priv;
	struct {
		char *buf;
		int pos;
	} pes_priv1[8192], *pptr;
	char *tmpbuf;
	struct pmt_es_t *pes;
	struct pmt_es_t *vpes, *apes, *spes; // current/last selected PES
	int dmode;

	priv->last_pid = 8192;		//invalid pid
	vpes = apes = spes = NULL;
	dmode = dualmono_mode + 1; // Main/L: 2^0,  Sub/R: 2^1, Both: 2^1 | 2^0

	req_apid = param->apid;
	req_vpid = param->vpid;
	req_spid = param->spid;
	prog_candidate = param->prog;

	has_tables = 0;
	memset(pes_priv1, 0, sizeof(pes_priv1));
	init_pos = stream_tell(demuxer->stream);
	mp_msg(MSGT_DEMUXER, MSGL_V, "PROBING UP TO %"PRIu64", PROG: %d\n", (uint64_t) param->probe, param->prog);
	end_pos = init_pos + (param->probe ? param->probe : TS_MAX_PROBE_SIZE);
	while(1)
	{
		pos = stream_tell(demuxer->stream);
		if(pos > end_pos || demuxer->stream->eof)
			break;

		if(ts_parse(demuxer, &es, tmp, 1))
		{
			param->scrambled |= (tmp[3] & 0xC0);
			//Non PES-aligned A52 audio may escape detection if PMT is not present;
			//in this case we try to find at least 3 A52 syncwords
			if((es.type == PES_PRIVATE1) && (! audio_found) && req_apid > -2)
			{
				pptr = &pes_priv1[es.pid];
				if(pptr->pos < 64*1024)
				{
				tmpbuf = realloc(pptr->buf, pptr->pos + es.size);
				if(tmpbuf != NULL)
				{
					pptr->buf = tmpbuf;
					memcpy(&(pptr->buf[ pptr->pos ]), es.start, es.size);
					pptr->pos += es.size;
					if(a52_check(pptr->buf, pptr->pos) > 2)
					{
						param->atype = AUDIO_A52;
						param->apid = es.pid;
						es.type = AUDIO_A52;
					}
				}
				}
			}

			is_audio = IS_AUDIO(es.type) || ((es.type==SL_PES_STREAM) && IS_AUDIO(es.subtype));
			is_video = IS_VIDEO(es.type) || ((es.type==SL_PES_STREAM) && IS_VIDEO(es.subtype));
			is_sub   = IS_SUB(es.type);


			if((! is_audio) && (! is_video) && (! is_sub))
				continue;
			if(is_audio && req_apid==-2)
				continue;
			if(is_video && req_vpid==-2)
				continue;
			if(is_sub && req_spid==-2)
				continue;

			chosen_pid = 0;
			pes = (es.prog_idx >= 0 && priv->pmt && priv->pmt[es.prog_idx].es_cnt > 0) ?
				&priv->pmt[es.prog_idx].es[es.es_idx] : NULL;

			if(is_video)
			{
				if(req_vpid > 0)
				{
					chosen_pid = (req_vpid == es.pid);
					if(! chosen_pid)
						continue;
				}
			}
			else if(is_audio)
			{
				if(req_apid > 0)
				{
					chosen_pid = (req_apid == es.pid);
					if(! chosen_pid)
						continue;
				}
			}
			else if(is_sub)
			{
				if(req_spid > 0)
				{
					chosen_pid = (req_spid == es.pid);
					if(! chosen_pid)
						continue;
				}
			}

			if(param->apid < 0 &&
			   param->vpid < 0 &&
			   param->spid < 0 &&
			   prog_candidate <= 0)
			{
				mp_msg(MSGT_IDENTIFY, MSGL_V,
					"chose the first found pid:0x%04hx.\n", es.pid);
				chosen_pid = 1;
			}

			if((ret == 0) && chosen_pid)
			{
				ret = stream_tell(demuxer->stream);
			}

			p = progid_for_pid(priv, es.pid, prog_candidate);
			if(p != -1)
			{
				has_tables++;
				if(!param->prog && chosen_pid)
					prog_candidate = p;
			}

			if((param->prog > 0) && (param->prog != p))
			{
				if(audio_found)
				{
					if(is_video && (req_vpid == es.pid))
					{
						param->vtype = IS_VIDEO(es.type) ? es.type : es.subtype;
						param->vpid = es.pid;
						video_found = 1;
						break;
					}
				}

				if(video_found)
				{
					if(is_audio && (req_apid == es.pid))
					{
						param->atype = IS_AUDIO(es.type) ? es.type : es.subtype;
						param->apid = es.pid;
						audio_found = 1;
						break;
					}
				}


				continue;
			}


			mp_msg(MSGT_DEMUXER, MSGL_DBG2, "TYPE: %x, PID: %d, PROG FOUND: %d\n", es.type, es.pid, prog_candidate);


			if(is_video)
			{
				int selected = 0;

				// assert(chosen_pid || req_vpid <= 0)
				if(chosen_pid) // (req_vpid == es.pid)
					selected = 1;
				else { // (req_vpid != es.pid) && (req_vpid <= 0)
					// vid not specified by user
					if (!low_cn && pes && pes->highCN_pid != 0)
						selected = 0;
					else if (low_cn && vpes && vpes->highCN_pid &&
					         (!pes || !pes->highCN_pid))
						selected = 0;
					else if(param->vpid == -1) {
						if(prog_candidate == p)
							selected = 1;
					} else if(param->vpid == es.pid)
						selected = 1;
					else if (p <= 0 || prog_candidate != p || !pes)
						selected = 0;
					else if (!vpes)
						selected = 1;
					else if (low_cn &&
					         !vpes->highCN_pid &&
					         pes->highCN_pid)
						selected = 1;
					else if(pes->component_tag < vpes->component_tag)
						selected = 1;
				}

				if(selected)
				{
					if (param->vpid != es.pid || (pes && !vpes))
						mp_msg(MSGT_IDENTIFY, MSGL_V,
							"%svideo found! 0x%04hx tag:0x%02x"
							" in [0x%04x]\n",
							(!video_found) ? "New " : "Better ",
							es.pid, pes ? pes->component_tag : 0xff,
							prog_candidate);

					param->vtype = IS_VIDEO(es.type) ? es.type : es.subtype;
					param->vpid = es.pid;
					vpes = pes;
					video_found = 1;
					if(p != -1 && prog_candidate <= 0)
						prog_candidate = p;
				}
			}


			if(is_sub)
			{
				int selected = 0;

				// assert(chosen_pid || req_spid <= 0)
				if(chosen_pid) // (req_spid == es.pid)
					selected = 1;
				else { // (req_spid != es.pid) && (req_spid <= 0)
					// sid not specified by user
					if (!low_cn && pes && pes->highCN_pid != 0)
						selected = 0;
					else if (low_cn && spes && spes->highCN_pid &&
					         (!pes || !pes->highCN_pid))
						selected = 0;
					else if (param->slang[0] &&
					         pes_match_lang(spes, param->slang) &&
					         !pes_match_lang(pes, param->slang))
						selected = 0;
					else if(param->spid == -1) {
						if(prog_candidate == p)
							selected = 1;
					} else if(param->spid == es.pid)
						selected = 1;
					else if (p <= 0 || prog_candidate != p || !pes)
						selected = 0;
					else if (!spes)
						selected = 1;
					else if (low_cn &&
					         !spes->highCN_pid &&
					         pes->highCN_pid)
						selected = 1;
					else if (param->slang[0] &&
					         !pes_match_lang(spes, param->slang) &&
					         pes_match_lang(pes, param->slang))
						selected = 1;
					else if(pes->component_tag < spes->component_tag)
						selected = 1;
				}

				if(selected)
				{
					if (param->spid != es.pid || (pes && !spes))
						mp_msg(MSGT_IDENTIFY, MSGL_V,
							"sub found! 0x%04hx tag:0x%02x"
							" in [0x%04x].\n",
							es.pid, pes ? pes->component_tag : 0xff,
							prog_candidate);

					param->stype = es.type;
					param->spid = es.pid;
					spes = pes;
					if(p != -1 && prog_candidate <= 0)
						prog_candidate = p;
				}
			}

			if(is_audio)
			{
				int selected = 0;

				// assert(chosen_pid || req_apid <= 0)
				if(chosen_pid) // (req_apid == es.pid)
					selected = 1;
				else { // (req_apid != es.pid) && (req_apid <= 0)
					// aid not specified by user
					if (!low_cn && pes && pes->highCN_pid != 0)
						selected = 0;
					else if (low_cn && apes && apes->highCN_pid &&
					         (!pes || !pes->highCN_pid))
						selected = 0;
					else if (param->alang[0] &&
					         pes_match_lang(apes, param->alang) &&
					         !pes_match_lang(pes, param->alang))
						selected = 0;
					else if (param->alang[0] && pes && pes->is_dualmono &&
					         pes_match_lang(apes, param->alang) & dmode &&
					         !(pes_match_lang(pes, param->alang) & dmode))
						selected = 0;
					else if(param->apid == -1) {
						if(prog_candidate == p)
							selected = 1;
					} else if(param->apid == es.pid)
						selected = 1;
					else if (p <= 0 || prog_candidate != p || !pes)
						selected = 0;
					else if (!apes)
						selected = 1;
					else if (low_cn &&
					         !apes->highCN_pid &&
					         pes->highCN_pid)
						selected = 1;
					else if (param->alang[0] &&
					         !pes_match_lang(apes, param->alang) &&
					         pes_match_lang(pes, param->alang))
						selected = 1;
					else if(pes->component_tag < apes->component_tag)
						selected = 1;
					else if(param->alang[0] && apes->is_dualmono &&
					        !(pes_match_lang(apes, param->alang) & dmode) &&
					        (pes_match_lang(pes, param->alang) & dmode))
						selected = 1;
				}

				if(selected)
				{
					if (param->apid != es.pid || (pes && !apes)) {
						int t;
						char *l1, *l2, *sep;

						if (pes) {
							t = pes->component_tag;
							l1 = pes->lang;
							l2 = pes->lang2;
							sep = *l2 ? "/" : "";
						} else {
							t = 0xff;
							l1 = l2 = sep = "";
						}

						mp_msg(MSGT_IDENTIFY, MSGL_V,
							"%saudio found! 0x%04hx tag:0x%02x"
							" lang:%.3s%s%.3s in [0x%04x].\n",
							(!audio_found) ? "New " : "Better ",
							es.pid, t, l1, sep, l2,
							prog_candidate);
					}

					param->atype = IS_AUDIO(es.type) ? es.type : es.subtype;
					param->apid = es.pid;
					apes = pes;
					audio_found = 1;
					if(p != -1 && prog_candidate <= 0)
						prog_candidate = p;
				}
			}

			if(audio_found && (param->apid == es.pid) && (! video_found)) {
				num_packets++;
				if(num_packets >= NUM_CONSECUTIVE_AUDIO_PACKETS &&
				   !param->probe) {
					//novideo or we have at least 348 audio packets
					//(64 KB) without video (TS with audio only)
					param->vtype = 0;
					break;
				}
			}

			if((has_tables==0) && (video_found && audio_found) && (pos >= 1000000))
				break;
		}
	}

	for(i=0; i<8192; i++)
	{
		if(pes_priv1[i].buf != NULL)
		{
			free(pes_priv1[i].buf);
			pes_priv1[i].buf = NULL;
			pes_priv1[i].pos = 0;
		}
	}

	if(video_found)
	{
		if(param->vtype == VIDEO_MPEG1)
			mp_msg(MSGT_DEMUXER, MSGL_INFO, "VIDEO MPEG1(pid=%d) ", param->vpid);
		else if(param->vtype == VIDEO_MPEG2)
			mp_msg(MSGT_DEMUXER, MSGL_INFO, "VIDEO MPEG2(pid=%d) ", param->vpid);
		else if(param->vtype == VIDEO_MPEG4)
			mp_msg(MSGT_DEMUXER, MSGL_INFO, "VIDEO MPEG4(pid=%d) ", param->vpid);
		else if(param->vtype == VIDEO_H264)
			mp_msg(MSGT_DEMUXER, MSGL_INFO, "VIDEO H264(pid=%d) ", param->vpid);
		else if(param->vtype == VIDEO_VC1)
			mp_msg(MSGT_DEMUXER, MSGL_INFO, "VIDEO VC1(pid=%d) ", param->vpid);
		else if(param->vtype == VIDEO_AVC)
			mp_msg(MSGT_DEMUXER, MSGL_INFO, "VIDEO AVC(NAL-H264, pid=%d) ", param->vpid);
		else if(param->vtype == VIDEO_HEVC)
			mp_msg(MSGT_DEMUXER, MSGL_INFO, "VIDEO HEVC(pid=%d) ", param->vpid);
	}
	else
	{
		param->vtype = UNKNOWN;
		//WE DIDN'T MATCH ANY VIDEO STREAM
		mp_msg(MSGT_DEMUXER, MSGL_INFO, "NO VIDEO! ");
	}

	if(param->atype == AUDIO_MP2)
		mp_msg(MSGT_DEMUXER, MSGL_INFO, "AUDIO MPA(pid=%d)", param->apid);
	else if(param->atype == AUDIO_A52)
		mp_msg(MSGT_DEMUXER, MSGL_INFO, "AUDIO A52(pid=%d)", param->apid);
	else if(param->atype == AUDIO_DTS)
		mp_msg(MSGT_DEMUXER, MSGL_INFO, "AUDIO DTS(pid=%d)", param->apid);
	else if(param->atype == AUDIO_LPCM_BE)
		mp_msg(MSGT_DEMUXER, MSGL_INFO, "AUDIO LPCM(pid=%d)", param->apid);
	else if(param->atype == AUDIO_PCM_BR)
		mp_msg(MSGT_DEMUXER, MSGL_INFO, "AUDIO PCMBR(pid=%d)", param->apid);
	else if(param->atype == AUDIO_AAC)
		mp_msg(MSGT_DEMUXER, MSGL_INFO, "AUDIO AAC(pid=%d)", param->apid);
	else if(param->atype == AUDIO_AAC_LATM)
		mp_msg(MSGT_DEMUXER, MSGL_INFO, "AUDIO AAC LATM(pid=%d)", param->apid);
	else if(param->atype == AUDIO_TRUEHD)
		mp_msg(MSGT_DEMUXER, MSGL_INFO, "AUDIO TRUEHD(pid=%d)", param->apid);
	else if(param->atype == AUDIO_S302M)
		mp_msg(MSGT_DEMUXER, MSGL_INFO, "AUDIO S302M(pid=%d)", param->apid);
	else
	{
		audio_found = 0;
		param->atype = UNKNOWN;
		//WE DIDN'T MATCH ANY AUDIO STREAM, SO WE FORCE THE DEMUXER TO IGNORE AUDIO
		mp_msg(MSGT_DEMUXER, MSGL_INFO, "NO AUDIO! (try increasing -tsprobe)");
	}

	if(IS_SUB(param->stype))
		mp_msg(MSGT_DEMUXER, MSGL_INFO, " SUB %s(pid=%d) ", (param->stype==SPU_DVD ? "DVD" : param->stype==SPU_DVB ? "DVB" : "Teletext"), param->spid);
	else
	{
		param->stype = UNKNOWN;
		mp_msg(MSGT_DEMUXER, MSGL_INFO, " NO SUBS (yet)! ");
	}

	if(video_found || audio_found)
	{
		if(!param->prog)
		{
			p = progid_for_pid(priv, video_found ? param->vpid : param->apid, 0);
			if(p != -1)
				param->prog = p;
		}

		if(demuxer->stream->eof && (ret == 0))
			ret = init_pos;
		mp_msg(MSGT_DEMUXER, MSGL_INFO, " PROGRAM N. %d\n", param->prog);
	}
	else
		mp_msg(MSGT_DEMUXER, MSGL_INFO, "\n");


	for(i=0; i<NB_PID_MAX; i++)
	{
		if(priv->ts.pids[i] != NULL)
		{
			priv->ts.pids[i]->payload_size = 0;
			priv->ts.pids[i]->pts = priv->ts.pids[i]->last_pts = 0;
			priv->ts.pids[i]->last_cc = -1;
			priv->ts.pids[i]->is_synced = 0;
		}
	}

	return ret;
}

static int parse_avc_sps(uint8_t *buf, int len, int *w, int *h)
{
	int sps, sps_len;
	unsigned char *ptr;
	mp_mpeg_header_t picture;
	if(len < 6)
		return 0;
	sps = buf[5] & 0x1f;
	if(!sps)
		return 0;
	sps_len = (buf[6] << 8) | buf[7];
	if(!sps_len || (sps_len > len - 8))
		return 0;
	ptr = &(buf[8]);
	picture.display_picture_width = picture.display_picture_height = 0;
	h264_parse_sps(&picture, ptr, len - 8);
	if(!picture.display_picture_width || !picture.display_picture_height)
		return 0;
	*w = picture.display_picture_width;
	*h = picture.display_picture_height;
	return 1;
}

static demuxer_t *demux_open_ts(demuxer_t * demuxer)
{
	int i;
	uint8_t packet_size;
	sh_video_t *sh_video;
	sh_audio_t *sh_audio;
	off_t start_pos;
	tsdemux_init_t params;
	ts_priv_t * priv = demuxer->priv;

	mp_msg(MSGT_DEMUX, MSGL_V, "DEMUX OPEN, AUDIO_ID: %d, VIDEO_ID: %d, SUBTITLE_ID: %d,\n",
		demuxer->audio->id, demuxer->video->id, demuxer->sub->id);


	demuxer->type= DEMUXER_TYPE_MPEG_TS;


	stream_reset(demuxer->stream);

	packet_size = ts_check_file(demuxer);
	if(!packet_size)
	    return NULL;

	priv = calloc(1, sizeof(ts_priv_t));
	if(priv == NULL)
	{
		mp_msg(MSGT_DEMUX, MSGL_FATAL, "DEMUX_OPEN_TS, couldn't allocate enough memory for ts->priv, exit\n");
		return NULL;
	}

	for(i=0; i < NB_PID_MAX; i++)
	{
	    priv->ts.pids[i] = NULL;
	    priv->ts.streams[i].id = -3;
	    // priv->ts.ecms[i] = NULL; /* priv is calloced. */
	}
	priv->pat.progs = NULL;
	priv->pat.progs_cnt = 0;
	priv->pat.section.buffer = NULL;
	priv->pat.section.buffer_len = 0;
	priv->pat.version_number = VERSION_NONE;

	priv->pmt = NULL;
	priv->pmt_cnt = 0;
	priv->pcr_delta = 0.0;

	priv->keep_broken = ts_keep_broken;
	priv->ts.packet_size = packet_size;


	demuxer->priv = priv;
	if(demuxer->stream->type != STREAMTYPE_FILE)
		demuxer->seekable = 1;
	else
		demuxer->seekable = 1;


	// As initial ds->id values were set as PID,
	//  (by demux_open(), from -vid/-aid ...),
	// convert them into the index of demuxer->{a-,v-,s-}streams[].
	params.atype = params.vtype = params.stype = UNKNOWN;
	params.apid = demuxer->audio->id;
	params.vpid = demuxer->video->id;
	params.spid = demuxer->sub->id;
	params.prog = ts_prog;
	params.probe = ts_probe;
	params.scrambled = 0;

	if(audio_lang != NULL)
	{
		strncpy(params.alang, audio_lang, sizeof(params.alang) - 1);
		params.alang[sizeof(params.alang) - 1] = 0;
	}
	else
		memset(params.alang, 0, sizeof(params.alang));

	if(dvdsub_lang != NULL)
	{
		strncpy(params.slang, dvdsub_lang, sizeof(params.slang) - 1);
		params.slang[sizeof(params.slang) - 1] = 0;
	}
	else
		memset(params.slang, 0, sizeof(params.slang));

#ifdef CONFIG_DEMULTI2
	priv->dm2_handle = demulti2_open();
#endif

	start_pos = ts_detect_streams(demuxer, &params);

	demuxer->sub->id = params.spid;
	priv->prog = params.prog;

	if(params.vtype != UNKNOWN)
	{
		ts_add_stream(demuxer, priv->ts.pids[params.vpid]);
		sh_video = priv->ts.streams[params.vpid].sh;
		demuxer->video->id = priv->ts.streams[params.vpid].id;
		sh_video->ds = demuxer->video;
		sh_video->format = params.vtype;
		demuxer->video->sh = sh_video;
	}
	else {
		demuxer->video->id = -2;
		demuxer->video->sh = NULL;
	}

	if(params.atype != UNKNOWN)
	{
		ES_stream_t *es = priv->ts.pids[params.apid];

		if(!IS_AUDIO(es->type) && !IS_AUDIO(es->subtype) && IS_AUDIO(params.atype)) es->subtype = params.atype;
		ts_add_stream(demuxer, priv->ts.pids[params.apid]);
		sh_audio = priv->ts.streams[params.apid].sh;
		demuxer->audio->id = priv->ts.streams[params.apid].id;
		sh_audio->ds = demuxer->audio;
		sh_audio->format = params.atype;
		demuxer->audio->sh = sh_audio;
		//change dmono mode if necessary
		if (params.alang[0] &&
		    es->prog_idx >= 0 && priv->pmt &&
		    priv->pmt[es->prog_idx].es[es->es_idx].is_dualmono) {
			if (dualmono_mode == 0)
				dualmono_mode = strncmp(params.alang, sh_audio->lang, 3) &&
						!strncmp(params.alang, sh_audio->lang2, 3);
			else if (dualmono_mode == 1)
				dualmono_mode = strncmp(params.alang, sh_audio->lang, 3) ||
						!strncmp(params.alang, sh_audio->lang2, 3);
			sh_audio->dualmono_mode = dualmono_mode;
		}
	}
	else {
		demuxer->audio->id = -2;
		demuxer->audio->sh = NULL;
	}

	if (params.stype == UNKNOWN) {
		demuxer->sub->id = -2;
		demuxer->sub->sh = NULL;
	}

	mp_msg(MSGT_DEMUXER,MSGL_V, "Opened TS demuxer, audio: %x(pid %d), video: %x(pid %d)...POS=%"PRIu64", PROBE=%"PRIu64"\n", params.atype, demuxer->audio->id, params.vtype, demuxer->video->id, (uint64_t) start_pos, ts_probe);


	start_pos = start_pos <= priv->ts.packet_size ?
                    demuxer->stream->start_pos :
                    start_pos - priv->ts.packet_size;
	demuxer->movi_start = start_pos;
	demuxer->reference_clock = MP_NOPTS_VALUE;
	stream_reset(demuxer->stream);
	stream_seek(demuxer->stream, start_pos);	//IF IT'S FROM A PIPE IT WILL FAIL, BUT WHO CARES?


	priv->last_pid = 8192;		//invalid pid

	for(i = 0; i < 3; i++)
	{
		priv->fifo[i].pack  = NULL;
		priv->fifo[i].offset = 0;
	}
	priv->fifo[0].ds = demuxer->audio;
	priv->fifo[1].ds = demuxer->video;
	priv->fifo[2].ds = demuxer->sub;

	priv->fifo[0].buffer_size = 1536;
	priv->fifo[1].buffer_size = 32767 * 4;
	priv->fifo[2].buffer_size = 32767;

	priv->pat.section.buffer_len = 0;
	for(i = 0; i < priv->pmt_cnt; i++)
		priv->pmt[i].section.buffer_len = 0;

	demuxer->filepos = stream_tell(demuxer->stream);
	return demuxer;
}

static void demux_close_ts(demuxer_t * demuxer)
{
	uint16_t i;
	ts_priv_t *priv = (ts_priv_t*) demuxer->priv;

	if(priv)
	{
#if CONFIG_DEMULTI2
		if (priv->dm2_handle)
			demulti2_close(priv->dm2_handle);
#endif

		free(priv->pat.section.buffer);
		free(priv->pat.progs);
		free(priv->eit_section.buffer);

		if(priv->pmt)
		{
			for(i = 0; i < priv->pmt_cnt; i++)
			{
				free(priv->pmt[i].section.buffer);
				free(priv->pmt[i].es);
			}
			free(priv->pmt);
		}
		for (i = 0; i < NB_PID_MAX; i++)
		{
			if (priv->ts.pids[i])
				free(priv->ts.pids[i]->section.buffer);
			free(priv->ts.pids[i]);
			priv->ts.pids[i] = NULL;

			if (priv->ts.ecms[i])
				free(priv->ts.ecms[i]->section.buffer);
			free(priv->ts.ecms[i]);
			priv->ts.ecms[i] = NULL;
		}
		for (i = 0; i < 3; i++)
		{
			if (priv->fifo[i].pack)
				free_demux_packet(priv->fifo[i].pack);
			priv->fifo[i].pack = NULL;
		}
		free(priv);
	}
	demuxer->priv=NULL;
}


#define getbits mp_getbits

static int mp4_parse_sl_packet(pmt_t *pmt, uint8_t *buf, uint16_t packet_len, int pid, ES_stream_t *pes_es)
{
	int i, n, m, mp4_es_id = -1;
	uint64_t v = 0;
	uint32_t pl_size = 0;
	int deg_flag = 0;
	mp4_es_descr_t *es = NULL;
	mp4_sl_config_t *sl = NULL;
	uint8_t au_start = 0, au_end = 0, rap_flag = 0, ocr_flag = 0, padding = 0,  padding_bits = 0, idle = 0;

	pes_es->is_synced = 0;
	mp_msg(MSGT_DEMUXER,MSGL_V, "mp4_parse_sl_packet, pid: %d, pmt: %pm, packet_len: %d\n", pid, pmt, packet_len);
	if(! pmt || !packet_len)
		return 0;

	for(i = 0; i < pmt->es_cnt; i++)
	{
		if(pmt->es[i].pid == pid)
			mp4_es_id = pmt->es[i].mp4_es_id;
	}
	if(mp4_es_id < 0)
		return -1;

	for(i = 0; i < pmt->mp4es_cnt; i++)
	{
		if(pmt->mp4es[i].id == mp4_es_id)
			es = &(pmt->mp4es[i]);
	}
	if(! es)
		return -1;

	pes_es->subtype = es->decoder.object_type;

	sl = &(es->sl);
	if(!sl)
		return -1;

	//now es is the complete es_descriptor of out mp4 ES stream
	mp_msg(MSGT_DEMUXER,MSGL_DBG2, "ID: %d, FLAGS: 0x%x, subtype: %x\n", es->id, sl->flags, pes_es->subtype);

	n = 0;
	if(sl->au_start)
		pes_es->sl.au_start = au_start = getbits(buf, n++, 1);
	else
		pes_es->sl.au_start = (pes_es->sl.last_au_end ? 1 : 0);
	if(sl->au_end)
		pes_es->sl.au_end = au_end = getbits(buf, n++, 1);

	if(!sl->au_start && !sl->au_end)
	{
		pes_es->sl.au_start = pes_es->sl.au_end = au_start = au_end = 1;
	}
	pes_es->sl.last_au_end = pes_es->sl.au_end;


	if(sl->ocr_len > 0)
		ocr_flag = getbits(buf, n++, 1);
	if(sl->idle)
		idle = getbits(buf, n++, 1);
	if(sl->padding)
		padding = getbits(buf, n++, 1);
	if(padding)
	{
		padding_bits = getbits(buf, n, 3);
		n += 3;
	}

	if(idle || (padding && !padding_bits))
	{
		pes_es->payload_size = 0;
		return -1;
	}

	//(! idle && (!padding || padding_bits != 0)) is true
	n += sl->packet_seqnum_len;
	if(sl->degr_len)
		deg_flag = getbits(buf, n++, 1);
	if(deg_flag)
		n += sl->degr_len;

	if(ocr_flag)
	{
		n += sl->ocr_len;
		mp_msg(MSGT_DEMUXER,MSGL_DBG2, "OCR: %d bits\n", sl->ocr_len);
	}

	if(packet_len * 8 <= n)
		return -1;

	mp_msg(MSGT_DEMUXER,MSGL_DBG2, "\nAU_START: %d, AU_END: %d\n", au_start, au_end);
	if(au_start)
	{
		int dts_flag = 0, cts_flag = 0, ib_flag = 0;

		if(sl->random_accesspoint)
			rap_flag = getbits(buf, n++, 1);

		//check commented because it seems it's rarely used, and we need this flag set in case of au_start
		//the decoder will eventually discard the payload if it can't decode it
		//if(rap_flag || sl->random_accesspoint_only)
			pes_es->is_synced = 1;

		n += sl->au_seqnum_len;
		if(packet_len * 8 <= n+8)
			return -1;
		if(sl->use_ts)
		{
			dts_flag = getbits(buf, n++, 1);
			cts_flag = getbits(buf, n++, 1);
		}
		if(sl->instant_bitrate_len)
			ib_flag = getbits(buf, n++, 1);
		if(packet_len * 8 <= n+8)
			return -1;
		if(dts_flag && (sl->ts_len > 0))
		{
			n += sl->ts_len;
			mp_msg(MSGT_DEMUXER,MSGL_DBG2, "DTS: %d bits\n", sl->ts_len);
		}
		if(packet_len * 8 <= n+8)
			return -1;
		if(cts_flag && (sl->ts_len > 0))
		{
			int i = 0, m;

			while(i < sl->ts_len)
			{
				m = FFMIN(8, sl->ts_len - i);
				v |= getbits(buf, n, m);
				if(sl->ts_len - i > 8)
					v <<= 8;
				i += m;
				n += m;
				if(packet_len * 8 <= n+8)
					return -1;
			}

			pes_es->pts = (double) v / (double) sl->ts_resolution;
			mp_msg(MSGT_DEMUXER,MSGL_DBG2, "CTS: %d bits, value: %"PRIu64"/%d = %.3f\n", sl->ts_len, v, sl->ts_resolution, pes_es->pts);
		}


		i = 0;
		pl_size = 0;
		while(i < sl->au_len)
		{
			m = FFMIN(8, sl->au_len - i);
			pl_size |= getbits(buf, n, m);
			if(sl->au_len - i > 8)
				pl_size <<= 8;
			i += m;
			n += m;
			if(packet_len * 8 <= n+8)
				return -1;
		}
		mp_msg(MSGT_DEMUXER,MSGL_DBG2, "AU_LEN: %u (%d bits)\n", pl_size, sl->au_len);
		if(ib_flag)
			n += sl->instant_bitrate_len;
	}

	m = (n+7)/8;
	if(0 < pl_size && pl_size < pes_es->payload_size)
		pes_es->payload_size = pl_size;

	mp_msg(MSGT_DEMUXER,MSGL_V, "mp4_parse_sl_packet, n=%d, m=%d, size from pes hdr: %u, sl hdr size: %u, RAP FLAGS: %d/%d\n",
		n, m, pes_es->payload_size, pl_size, (int) rap_flag, (int) sl->random_accesspoint_only);

	return m;
}

//this function parses the extension fields in the PES header and returns the substream_id, or -1 in case of errors
static int parse_pes_extension_fields(unsigned char *p, int pkt_len)
{
	int skip;
	unsigned char flags;

	if(!(p[7] & 0x1))	//no extension_field
		return -1;
	skip = 9;
	if(p[7] & 0x80)
	{
		skip += 5;
		if(p[7] & 0x40)
			skip += 5;
	}
	if(p[7] & 0x20)	//escr_flag
		skip += 6;
	if(p[7] & 0x10)	//es_rate_flag
		skip += 3;
	if(p[7] & 0x08)//dsm_trick_mode is unsupported, skip
	{
		skip = 0;//don't let's parse the extension fields
	}
	if(p[7] & 0x04)	//additional_copy_info
		skip += 1;
	if(p[7] & 0x02)	//pes_crc_flag
		skip += 2;
	if(skip >= pkt_len)	//too few bytes
		return -1;
	flags = p[skip];
	skip++;
	if(flags & 0x80)	//pes_private_data_flag
		skip += 16;
	if(skip >= pkt_len)
		return -1;
	if(flags & 0x40)	//pack_header_field_flag
	{
		unsigned char l = p[skip];
		skip += l;
	}
	if(flags & 0x20)	//program_packet_sequence_counter
		skip += 2;
	if(flags & 0x10)	//p_std
		skip += 2;
	if(skip >= pkt_len)
		return -1;
	if(flags & 0x01)	//finally the long desired pes_extension2
	{
		unsigned char l = p[skip];	//ext2 flag+len
		skip++;
		if((l == 0x81) && (skip < pkt_len))
		{
			int ssid = p[skip];
			mp_msg(MSGT_IDENTIFY, MSGL_V, "SUBSTREAM_ID=%d (0x%02X)\n", ssid, ssid);
			return ssid;
		}
	}

	return -1;
}

static int pes_parse2(unsigned char *buf, uint16_t packet_len, ES_stream_t *es, int32_t type_from_pmt, pmt_t *pmt, int pid)
{
	unsigned char  *p;
	uint32_t       header_len;
	int64_t        pts;
	uint32_t       stream_id;
	uint32_t       pkt_len, pes_is_aligned;

	//Here we are always at the start of a PES packet
	mp_msg(MSGT_DEMUX, MSGL_DBG2, "pes_parse2(%p, %d): \n", buf, (uint32_t) packet_len);

	if(packet_len == 0 || packet_len > 184)
	{
		mp_msg(MSGT_DEMUX, MSGL_DBG2, "pes_parse2, BUFFER LEN IS TOO SMALL OR TOO BIG: %d EXIT\n", packet_len);
		return 0;
	}

	p = buf;
	pkt_len = packet_len;


	mp_msg(MSGT_DEMUX, MSGL_DBG2, "pes_parse2: HEADER %02x %02x %02x %02x\n", p[0], p[1], p[2], p[3]);
	if (p[0] || p[1] || (p[2] != 1))
	{
		mp_msg(MSGT_DEMUX, MSGL_DBG2, "pes_parse2: error HEADER %02x %02x %02x (should be 0x000001) \n", p[0], p[1], p[2]);
		return 0 ;
	}

	packet_len -= 6;
	if(packet_len==0)
	{
		mp_msg(MSGT_DEMUX, MSGL_DBG2, "pes_parse2: packet too short: %d, exit\n", packet_len);
		return 0;
	}

	es->payload_size = (p[4] << 8 | p[5]);
	pes_is_aligned = (p[6] & 4);

	stream_id  = p[3];


	if (p[7] & 0x80)
	{ 	/* pts available */
		pts  = (int64_t)(p[9] & 0x0E) << 29 ;
		pts |=  p[10]         << 22 ;
		pts |= (p[11] & 0xFE) << 14 ;
		pts |=  p[12]         <<  7 ;
		pts |= (p[13] & 0xFE) >>  1 ;

		es->pts = pts / 90000.0;
	}
	else
		es->pts = 0.0;


	header_len = p[8];


	if (header_len + 9 > pkt_len) //9 are the bytes read up to the header_length field
	{
		mp_msg(MSGT_DEMUX, MSGL_DBG2, "demux_ts: illegal value for PES_header_data_length (0x%02x)\n", header_len);
		return 0;
	}

	if(stream_id==0xfd)
	{
		int ssid = parse_pes_extension_fields(p, pkt_len);
		if((audio_substream_id!=-1) && (ssid != audio_substream_id))
			return 0;
		if(ssid == 0x72 && type_from_pmt != AUDIO_DTS && type_from_pmt != SPU_PGS)
			es->type  = type_from_pmt = AUDIO_TRUEHD;
	}

	p += header_len + 9;
	packet_len -= header_len + 3;

	if(es->payload_size)
		es->payload_size -= header_len + 3;


	es->is_synced = 1;	//only for SL streams we have to make sure it's really true, see below
	if (stream_id == 0xbd)
	{
		mp_msg(MSGT_DEMUX, MSGL_DBG3, "pes_parse2: audio buf = %02X %02X %02X %02X %02X %02X %02X %02X, 80: %d\n",
			p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7], p[0] & 0x80);


		/*
		* we check the descriptor tag first because some stations
		* do not include any of the A52 header info in their audio tracks
		* these "raw" streams may begin with a byte that looks like a stream type.
		*/


		if(type_from_pmt == SPU_PGS)
		{
			es->start = p;
			es->size  = packet_len;
			es->type  = SPU_PGS;
			es->payload_size -= packet_len;
			return 1;
		}
		if(
			(type_from_pmt == AUDIO_A52) ||		 /* A52 - raw */
			(packet_len >= 2 && p[0] == 0x0B && p[1] == 0x77)		/* A52 - syncword */
		)
		{
			mp_msg(MSGT_DEMUX, MSGL_DBG2, "A52 RAW OR SYNCWORD\n");
			es->start = p;
			es->size  = packet_len;
			es->type  = AUDIO_A52;
			es->payload_size -= packet_len;

			return 1;
		}
		/* SPU SUBS */
		else if(type_from_pmt == SPU_DVB ||
		(packet_len >= 2 && (p[0] == 0x20) && pes_is_aligned)) // && p[1] == 0x00))
		{
			// offset/length fiddling to make decoding with lavc possible
			es->start = p + 2;
			es->size  = packet_len - 2;
			es->type  = SPU_DVB;
			es->payload_size -= packet_len;

			return 1;
		}
		else if (pes_is_aligned && packet_len >= 1 && ((p[0] & 0xE0) == 0x20))	//SPU_DVD
		{
			//DVD SUBS
			es->start   = p+1;
			es->size    = packet_len-1;
			es->type    = SPU_DVD;
			es->payload_size -= packet_len;

			return 1;
		}
		else if (pes_is_aligned && packet_len >= 4 && (p[0] & 0xF8) == 0x80)
		{
			mp_msg(MSGT_DEMUX, MSGL_DBG2, "A52 WITH HEADER\n");
			es->start   = p+4;
			es->size    = packet_len - 4;
			es->type    = AUDIO_A52;
			es->payload_size -= packet_len;

			return 1;
		}
		else if (pes_is_aligned && packet_len >= 1 && ((p[0]&0xf0) == 0xa0))
		{
			int pcm_offset;

			for (pcm_offset=0; ++pcm_offset < packet_len-1 ; )
			{
				if (p[pcm_offset] == 0x01 && p[pcm_offset+1] == 0x80)
				{ 	/* START */
					pcm_offset += 2;
					break;
				}
			}

			es->start   = p + pcm_offset;
			es->size    = packet_len - pcm_offset;
			es->type    = AUDIO_LPCM_BE;
			es->payload_size -= packet_len;

			return 1;
		}
		else
		{
			mp_msg(MSGT_DEMUX, MSGL_DBG2, "PES_PRIVATE1\n");
			es->start   = p;
			es->size    = packet_len;
			es->type    = (type_from_pmt == UNKNOWN ? PES_PRIVATE1 : type_from_pmt);
			es->payload_size -= packet_len;

			return 1;
		}
	}
	else if(((stream_id >= 0xe0) && (stream_id <= 0xef)) || (stream_id == 0xfd && type_from_pmt != UNKNOWN))
	{
		es->start   = p;
		es->size    = packet_len;
		if(type_from_pmt != UNKNOWN)
		    es->type    = type_from_pmt;
		else
		    es->type    = VIDEO_MPEG2;
		if(es->payload_size)
			es->payload_size -= packet_len;

		mp_msg(MSGT_DEMUX, MSGL_DBG2, "pes_parse2: M2V size %d\n", es->size);
		return 1;
	}
	else if ((stream_id == 0xfa))
	{
		int l;

		es->is_synced = 0;
		if(type_from_pmt != UNKNOWN)	//MP4 A/V or SL
		{
			es->start   = p;
			es->size    = packet_len;
			es->type    = type_from_pmt;

			if(type_from_pmt == SL_PES_STREAM)
			{
				//if(pes_is_aligned)
				//{
					l = mp4_parse_sl_packet(pmt, p, packet_len, pid, es);
					mp_msg(MSGT_DEMUX, MSGL_DBG2, "L=%d, TYPE=%x\n", l, type_from_pmt);
					if(l < 0)
					{
						mp_msg(MSGT_DEMUX, MSGL_DBG2, "pes_parse2: couldn't parse SL header, passing along full PES payload\n");
						l = 0;
					}
				//}

				es->start   += l;
				es->size    -= l;
			}

			if(es->payload_size)
				es->payload_size -= packet_len;
			return 1;
		}
	}
	else if ((stream_id & 0xe0) == 0xc0)
	{
		es->start   = p;
		es->size    = packet_len;

		if(type_from_pmt != UNKNOWN)
			es->type = type_from_pmt;
		else
			es->type    = AUDIO_MP2;

		es->payload_size -= packet_len;

		return 1;
	}
	else if (type_from_pmt != -1)	//as a last resort here we trust the PMT, if present
	{
		es->start   = p;
		es->size    = packet_len;
		es->type    = type_from_pmt;
		es->payload_size -= packet_len;

		return 1;
	}
	else
	{
		mp_msg(MSGT_DEMUX, MSGL_DBG2, "pes_parse2: unknown packet, id: %x\n", stream_id);
	}

	es->is_synced = 0;
	return 0;
}




static int ts_sync(stream_t *stream)
{
	mp_msg(MSGT_DEMUX, MSGL_DBG3, "TS_SYNC \n");

	while (!stream->eof)
		if (stream_read_char(stream) == 0x47)
			return 1;

	return 0;
}


static void ts_dump_streams(ts_priv_t *priv)
{
	int i;

	for(i = 0; i < 3; i++)
	{
		if((priv->fifo[i].pack != NULL) && (priv->fifo[i].offset != 0))
		{
			resize_demux_packet(priv->fifo[i].pack, priv->fifo[i].offset);
			ds_add_packet(priv->fifo[i].ds, priv->fifo[i].pack);
			priv->fifo[i].offset = 0;
			priv->fifo[i].pack = NULL;
		}
	}
}


static int32_t prog_idx_in_pat(ts_priv_t *priv, uint16_t progid)
{
	int x;

	if(priv->pat.progs == NULL)
			return -1;

	for(x = 0; x < priv->pat.progs_cnt; x++)
	{
		if(priv->pat.progs[x].id == progid)
			return x;
	}

	return -1;
}


static int32_t prog_id_in_pat(ts_priv_t *priv, uint16_t pid)
{
	int x;

	if(priv->pat.progs == NULL)
		return -1;

	for(x = 0; x < priv->pat.progs_cnt; x++)
	{
		if(priv->pat.progs[x].pmt_pid == pid)
			return priv->pat.progs[x].id;
	}

	return -1;
}

// support multi sections in a TS packet (as in EIT)
static int next_section(ts_section_t *section)
{
	int skip, skip2;
	int tlen;
	uint8_t *buf;
	int buflen;

	buf = section->buffer;
	buflen = section->buffer_len;

	if (buflen < 4)
		return 0;

	skip = buf[0];
	if (buflen < skip + 4)
		return 0;

	tlen = (buf[skip + 2] & 0x0f) << 8 | buf[skip + 3];
	skip2 = 1 + skip + 3 + tlen;

	if (buflen < skip2 + 1)
		return 0;

	if (buf[skip2] == 0xff) {
		section->buffer_len = 0;
		return 0;
	}

	memmove(buf + 1, buf + skip2, buflen - skip2);
	buf[0] = 0;
	section->buffer_len = 1 + buflen - skip2;
	return section->buffer_len >= 4 &&
		section->buffer_len >= ((buf[2] << 0x0f) << 8 | buf[3]) + 4;
}

static int collect_section(ts_section_t *section, int is_start, unsigned char *buff, int size)
{
	uint8_t *ptr;
	uint16_t tlen;
	int skip, tid;

	mp_msg(MSGT_DEMUX, MSGL_V, "COLLECT_SECTION, start: %d, size: %d, collected: %d\n", is_start, size, section->buffer_len);
	if(! is_start && !section->buffer_len)
		return 0;

	// support section tail concatenation in non-0 pointer case
	if (is_start && buff[0] > 0 && size > buff[0] && section->buffer_len > 0) {
		skip = section->buffer[0];
		tlen = (section->buffer[skip + 2] & 0x0f) << 8 |
			section->buffer[skip + 3];

		if (section->buffer_len + buff[0] == 1 + skip + 3 + tlen) {
			is_start = 0;
			buff++;
			size--;
		}
	}

	if(is_start)
	{
		if(! section->buffer)
		{
			section->buffer = malloc(4096 + 256);
			if(section->buffer == NULL)
				return 0;
		}
		section->buffer_len = 0;
	}

	if(size + section->buffer_len > 4096+256)
	{
		mp_msg(MSGT_DEMUX, MSGL_V, "COLLECT_SECTION, excessive len: %d + %d\n", section->buffer_len, size);
		return 0;
	}

	memcpy(&(section->buffer[section->buffer_len]), buff, size);
	section->buffer_len += size;

	if(section->buffer_len < 3)
		return 0;

	skip = section->buffer[0];
	if(skip + 2 > section->buffer_len)
		return 0;

	ptr = &(section->buffer[skip + 1]);
	tid = ptr[0];
	if (tid == 0xff) {
		section->buffer_len = 0;
		return 0;
	} else if (skip + 4 > section->buffer_len)
		return 0;

	tlen = ((ptr[1] & 0x0f) << 8) | ptr[2];
	mp_msg(MSGT_DEMUX, MSGL_V, "SKIP: %d+1, TID: %d, TLEN: %d, COLLECTED: %d\n", skip, tid, tlen, section->buffer_len);
	if(section->buffer_len < (skip+1+3+tlen))
	{
		mp_msg(MSGT_DEMUX, MSGL_DBG2, "DATA IS NOT ENOUGH, NEXT TIME\n");
		return 0;
	}

	return skip+1;
}

static const uint32_t crc_tab[256] = {
  0x00000000, 0x04c11db7, 0x09823b6e, 0x0d4326d9, 0x130476dc, 0x17c56b6b,
  0x1a864db2, 0x1e475005, 0x2608edb8, 0x22c9f00f, 0x2f8ad6d6, 0x2b4bcb61,
  0x350c9b64, 0x31cd86d3, 0x3c8ea00a, 0x384fbdbd, 0x4c11db70, 0x48d0c6c7,
  0x4593e01e, 0x4152fda9, 0x5f15adac, 0x5bd4b01b, 0x569796c2, 0x52568b75,
  0x6a1936c8, 0x6ed82b7f, 0x639b0da6, 0x675a1011, 0x791d4014, 0x7ddc5da3,
  0x709f7b7a, 0x745e66cd, 0x9823b6e0, 0x9ce2ab57, 0x91a18d8e, 0x95609039,
  0x8b27c03c, 0x8fe6dd8b, 0x82a5fb52, 0x8664e6e5, 0xbe2b5b58, 0xbaea46ef,
  0xb7a96036, 0xb3687d81, 0xad2f2d84, 0xa9ee3033, 0xa4ad16ea, 0xa06c0b5d,
  0xd4326d90, 0xd0f37027, 0xddb056fe, 0xd9714b49, 0xc7361b4c, 0xc3f706fb,
  0xceb42022, 0xca753d95, 0xf23a8028, 0xf6fb9d9f, 0xfbb8bb46, 0xff79a6f1,
  0xe13ef6f4, 0xe5ffeb43, 0xe8bccd9a, 0xec7dd02d, 0x34867077, 0x30476dc0,
  0x3d044b19, 0x39c556ae, 0x278206ab, 0x23431b1c, 0x2e003dc5, 0x2ac12072,
  0x128e9dcf, 0x164f8078, 0x1b0ca6a1, 0x1fcdbb16, 0x018aeb13, 0x054bf6a4,
  0x0808d07d, 0x0cc9cdca, 0x7897ab07, 0x7c56b6b0, 0x71159069, 0x75d48dde,
  0x6b93dddb, 0x6f52c06c, 0x6211e6b5, 0x66d0fb02, 0x5e9f46bf, 0x5a5e5b08,
  0x571d7dd1, 0x53dc6066, 0x4d9b3063, 0x495a2dd4, 0x44190b0d, 0x40d816ba,
  0xaca5c697, 0xa864db20, 0xa527fdf9, 0xa1e6e04e, 0xbfa1b04b, 0xbb60adfc,
  0xb6238b25, 0xb2e29692, 0x8aad2b2f, 0x8e6c3698, 0x832f1041, 0x87ee0df6,
  0x99a95df3, 0x9d684044, 0x902b669d, 0x94ea7b2a, 0xe0b41de7, 0xe4750050,
  0xe9362689, 0xedf73b3e, 0xf3b06b3b, 0xf771768c, 0xfa325055, 0xfef34de2,
  0xc6bcf05f, 0xc27dede8, 0xcf3ecb31, 0xcbffd686, 0xd5b88683, 0xd1799b34,
  0xdc3abded, 0xd8fba05a, 0x690ce0ee, 0x6dcdfd59, 0x608edb80, 0x644fc637,
  0x7a089632, 0x7ec98b85, 0x738aad5c, 0x774bb0eb, 0x4f040d56, 0x4bc510e1,
  0x46863638, 0x42472b8f, 0x5c007b8a, 0x58c1663d, 0x558240e4, 0x51435d53,
  0x251d3b9e, 0x21dc2629, 0x2c9f00f0, 0x285e1d47, 0x36194d42, 0x32d850f5,
  0x3f9b762c, 0x3b5a6b9b, 0x0315d626, 0x07d4cb91, 0x0a97ed48, 0x0e56f0ff,
  0x1011a0fa, 0x14d0bd4d, 0x19939b94, 0x1d528623, 0xf12f560e, 0xf5ee4bb9,
  0xf8ad6d60, 0xfc6c70d7, 0xe22b20d2, 0xe6ea3d65, 0xeba91bbc, 0xef68060b,
  0xd727bbb6, 0xd3e6a601, 0xdea580d8, 0xda649d6f, 0xc423cd6a, 0xc0e2d0dd,
  0xcda1f604, 0xc960ebb3, 0xbd3e8d7e, 0xb9ff90c9, 0xb4bcb610, 0xb07daba7,
  0xae3afba2, 0xaafbe615, 0xa7b8c0cc, 0xa379dd7b, 0x9b3660c6, 0x9ff77d71,
  0x92b45ba8, 0x9675461f, 0x8832161a, 0x8cf30bad, 0x81b02d74, 0x857130c3,
  0x5d8a9099, 0x594b8d2e, 0x5408abf7, 0x50c9b640, 0x4e8ee645, 0x4a4ffbf2,
  0x470cdd2b, 0x43cdc09c, 0x7b827d21, 0x7f436096, 0x7200464f, 0x76c15bf8,
  0x68860bfd, 0x6c47164a, 0x61043093, 0x65c52d24, 0x119b4be9, 0x155a565e,
  0x18197087, 0x1cd86d30, 0x029f3d35, 0x065e2082, 0x0b1d065b, 0x0fdc1bec,
  0x3793a651, 0x3352bbe6, 0x3e119d3f, 0x3ad08088, 0x2497d08d, 0x2056cd3a,
  0x2d15ebe3, 0x29d4f654, 0xc5a92679, 0xc1683bce, 0xcc2b1d17, 0xc8ea00a0,
  0xd6ad50a5, 0xd26c4d12, 0xdf2f6bcb, 0xdbee767c, 0xe3a1cbc1, 0xe760d676,
  0xea23f0af, 0xeee2ed18, 0xf0a5bd1d, 0xf464a0aa, 0xf9278673, 0xfde69bc4,
  0x89b8fd09, 0x8d79e0be, 0x803ac667, 0x84fbdbd0, 0x9abc8bd5, 0x9e7d9662,
  0x933eb0bb, 0x97ffad0c, 0xafb010b1, 0xab710d06, 0xa6322bdf, 0xa2f33668,
  0xbcb4666d, 0xb8757bda, 0xb5365d03, 0xb1f740b4
};

/* relicenced to LGPL from fluendo ts demuxer */
static uint32_t
calc_crc32 (uint8_t *data, unsigned int datalen)
{
  unsigned int i;
  uint32_t crc = 0xffffffff;

  for (i = 0; i < datalen; i++) {
    crc = (crc << 8) ^ crc_tab[((crc >> 24) ^ *data++) & 0xff];
  }
  return crc;
}

static int parse_pat(ts_priv_t * priv, int is_start, unsigned char *buff, int size)
{
	int skip;
	unsigned char *ptr;
	unsigned char *base;
	int entries, i;
	uint16_t progid;
	ts_section_t *section;
	uint8_t ver;

	section = &(priv->pat.section);
	skip = collect_section(section, is_start, buff, size);
	if(! skip)
		return 0;

	ptr = &(section->buffer[skip]);
	//PARSING
	priv->pat.table_id = ptr[0];
	if(priv->pat.table_id != 0)
		return 0;
	priv->pat.ssi = (ptr[1] >> 7) & 0x1;
	if(!(ptr[5] & 0x01)) {
		mp_msg(MSGT_DEMUX, MSGL_V, "ignorening a non-current PAT.\n");
		return 0;
	}
	priv->pat.curr_next = ptr[5] & 0x01;
	priv->pat.ts_id = (ptr[3]  << 8 ) | ptr[4];
	ver = (ptr[5] >> 1) & 0x1F;
	if (priv->pat.progs && priv->pat.version_number == ver) {
		mp_msg(MSGT_DEMUX, MSGL_V, "ignoreing an unchanged PAT\n");
		return 0;
	}
	priv->pat.version_number = ver;
	priv->pat.section_length = ((ptr[1] & 0x03) << 8 ) | ptr[2];
	priv->pat.section_number = ptr[6];
	priv->pat.last_section_number = ptr[7];

	if (calc_crc32(ptr, priv->pat.section_length + 3)) {
		mp_msg(MSGT_DEMUX, MSGL_INFO, "broken PAT, CRC check failed.\n");
		return 0;
	}
	mp_msg(MSGT_DEMUX, MSGL_V, "PARSE_PAT: section_len: %d, section %d/%d\n", priv->pat.section_length, priv->pat.section_number, priv->pat.last_section_number);

	// reset old PAT & PMT */
	if (priv->pat.progs) {
		mp_msg(MSGT_DEMUX, MSGL_DBG2, "releasing the previous PAT & PMT's\n");
		free(priv->pat.progs);
		priv->pat.progs = NULL;
		priv->pat.progs_cnt = 0;

		for(i = 0; priv->pmt && i < priv->pmt_cnt; i++) {
			int j;
			ES_stream_t *tss;

			free(priv->pmt[i].section.buffer);
			// PES's can be shared amoung programs,
			// but all programs are removed here anyway.
			for(j = 0; j < priv->pmt[i].es_cnt; j++) {
				tss = priv->ts.pids[priv->pmt[i].es[j].pid];
				if (tss)
					free(tss);
				priv->ts.pids[priv->pmt[i].es[j].pid] = NULL;
			}
			free(priv->pmt[i].es);
		}
		free(priv->pmt);
		priv->pmt = NULL;
		priv->pmt_cnt = 0;
	}

	entries = (int) (priv->pat.section_length - 9) / 4;	//entries per section
	priv->pat.progs = calloc(entries, sizeof(struct pat_progs_t));
	if (entries > 0 && !priv->pat.progs) {
		mp_msg(MSGT_DEMUX, MSGL_ERR, "PARSE_PAT: COULDN'T ALLOC PAT progs table.\n");
		return 0;
	}

	for(i=0; i < entries; i++)
	{
		int32_t idx;
		base = &ptr[8 + i*4];
		progid = (base[0] << 8) | base[1];

		if (progid == 0)	// skip NIT description
			continue;

		if((idx = prog_idx_in_pat(priv, progid)) == -1)
		{
			priv->pat.progs = realloc_struct(priv->pat.progs, priv->pat.progs_cnt+1, sizeof(struct pat_progs_t));
			if(!priv->pat.progs)
			{
				int sz = sizeof(struct pat_progs_t) * (priv->pat.progs_cnt+1);
				priv->pat.progs_cnt = 0;
				mp_msg(MSGT_DEMUX, MSGL_ERR, "PARSE_PAT: COULDN'T REALLOC %d bytes, NEXT\n", sz);
				break;
			}
			idx = priv->pat.progs_cnt;
			priv->pat.progs_cnt++;
		}

		priv->pat.progs[idx].id = progid;
		priv->pat.progs[idx].pmt_pid = ((base[2]  & 0x1F) << 8) | base[3];
		mp_msg(MSGT_DEMUX, MSGL_V, "PROG: %d (%d-th of %d), PMT: %d\n", priv->pat.progs[idx].id, i+1, entries, priv->pat.progs[idx].pmt_pid);
		mp_msg(MSGT_IDENTIFY, MSGL_V, "PROGRAM_ID=%d (0x%02X), PMT_PID: %d(0x%02X)\n",
			progid, progid, priv->pat.progs[idx].pmt_pid, priv->pat.progs[idx].pmt_pid);
	}

	return 1;
}


static int32_t es_pid_in_pmt(pmt_t * pmt, uint16_t pid)
{
	uint16_t i;

	if(pmt == NULL)
		return -1;

	if(pmt->es == NULL)
		return -1;

	for(i = 0; i < pmt->es_cnt; i++)
	{
		if(pmt->es[i].pid == pid)
			return (int32_t) i;
	}

	return -1;
}


static uint16_t get_mp4_desc_len(uint8_t *buf, int *len)
{
	//uint16_t i = 0, size = 0;
	int i = 0, j, size = 0;

	mp_msg(MSGT_DEMUX, MSGL_DBG2, "PARSE_MP4_DESC_LEN(%d), bytes: ", *len);
	j = FFMIN(*len, 4);
	while(i < j)
	{
		mp_msg(MSGT_DEMUX, MSGL_DBG2, " %x ", buf[i]);
		size |= (buf[i] & 0x7f);
		if(!(buf[i] & 0x80))
			break;
		size <<= 7;
		i++;
	}
	mp_msg(MSGT_DEMUX, MSGL_DBG2, ", SIZE=%d\n", size);

	*len = i+1;
	return size;
}


static uint16_t parse_mp4_slconfig_descriptor(uint8_t *buf, int len, void *elem)
{
	int i = 0;
	mp4_es_descr_t *es;
	mp4_sl_config_t *sl;

	mp_msg(MSGT_DEMUX, MSGL_V, "PARSE_MP4_SLCONFIG_DESCRIPTOR(%d)\n", len);
	es = (mp4_es_descr_t *) elem;
	if(!es)
	{
		mp_msg(MSGT_DEMUX, MSGL_V, "argh! NULL elem passed, skip\n");
		return len;
	}
	sl = &(es->sl);

	sl->ts_len = sl->ocr_len = sl->au_len = sl->instant_bitrate_len = sl->degr_len = sl->au_seqnum_len = sl->packet_seqnum_len = 0;
	sl->ocr = sl->dts = sl->cts = 0;

	if(buf[0] == 0)
	{
		i++;
		sl->flags = buf[i];
		i++;
		sl->ts_resolution = (buf[i] << 24) | (buf[i+1] << 16) | (buf[i+2] << 8) | buf[i+3];
		i += 4;
		sl->ocr_resolution = (buf[i] << 24) | (buf[i+1] << 16) | (buf[i+2] << 8) | buf[i+3];
		i += 4;
		sl->ts_len = buf[i];
		i++;
		sl->ocr_len = buf[i];
		i++;
		sl->au_len = buf[i];
		i++;
		sl->instant_bitrate_len = buf[i];
		i++;
		sl->degr_len = (buf[i] >> 4) & 0x0f;
		sl->au_seqnum_len = ((buf[i] & 0x0f) << 1) | ((buf[i+1] >> 7) & 0x01);
		i++;
		sl->packet_seqnum_len = ((buf[i] >> 2) & 0x1f);
		i++;

	}
	else if(buf[0] == 1)
	{
		sl->flags = 0;
		sl->ts_resolution = 1000;
		sl->ts_len = 32;
		i++;
	}
	else if(buf[0] == 2)
	{
		sl->flags = 4;
		i++;
	}
	else
	{
		sl->flags = 0;
		i++;
	}

	sl->au_start = (sl->flags >> 7) & 0x1;
	sl->au_end = (sl->flags >> 6) & 0x1;
	sl->random_accesspoint = (sl->flags >> 5) & 0x1;
	sl->random_accesspoint_only = (sl->flags >> 4) & 0x1;
	sl->padding = (sl->flags >> 3) & 0x1;
	sl->use_ts = (sl->flags >> 2) & 0x1;
	sl->idle = (sl->flags >> 1) & 0x1;
	sl->duration = sl->flags & 0x1;

	if(sl->duration)
	{
		sl->timescale = (buf[i] << 24) | (buf[i+1] << 16) | (buf[i+2] << 8) | buf[i+3];
		i += 4;
		sl->au_duration = (buf[i] << 8) | buf[i+1];
		i += 2;
		sl->cts_duration = (buf[i] << 8) | buf[i+1];
		i += 2;
	}
	else	//no support for fixed durations atm
		sl->timescale = sl->au_duration = sl->cts_duration = 0;

	mp_msg(MSGT_DEMUX, MSGL_V, "MP4SLCONFIG(len=0x%x), predef: %d, flags: %x, use_ts: %d, tslen: %d, timescale: %d, dts: %"PRIu64", cts: %"PRIu64"\n",
		len, buf[0], sl->flags, sl->use_ts, sl->ts_len, sl->timescale, (uint64_t) sl->dts, (uint64_t) sl->cts);

	return len;
}

static int parse_mp4_descriptors(pmt_t *pmt, uint8_t *buf, int len, void *elem);

static uint16_t parse_mp4_decoder_config_descriptor(pmt_t *pmt, uint8_t *buf, int len, void *elem)
{
	int i = 0, j;
	mp4_es_descr_t *es;
	mp4_decoder_config_t *dec;

	mp_msg(MSGT_DEMUX, MSGL_V, "PARSE_MP4_DECODER_CONFIG_DESCRIPTOR(%d)\n", len);
	es = (mp4_es_descr_t *) elem;
	if(!es)
	{
		mp_msg(MSGT_DEMUX, MSGL_V, "argh! NULL elem passed, skip\n");
		return len;
	}
	dec = (mp4_decoder_config_t*) &(es->decoder);

	dec->object_type = buf[i];
	dec->stream_type =  (buf[i+1]>>2) & 0x3f;

	if(dec->object_type == 1 && dec->stream_type == 1)
	{
		 dec->object_type = MP4_OD;
		 dec->stream_type = MP4_OD;
	}
	else if(dec->stream_type == 4)
	{
		if(dec->object_type == 0x6a)
			dec->object_type = VIDEO_MPEG1;
		if(dec->object_type >= 0x60 && dec->object_type <= 0x65)
			dec->object_type = VIDEO_MPEG2;
		else if(dec->object_type == 0x20)
			dec->object_type = VIDEO_MPEG4;
		else if(dec->object_type == 0x21)
			dec->object_type = VIDEO_AVC;
		/*else if(dec->object_type == 0x22)
			fprintf(stderr, "TYPE 0x22\n");*/
		else dec->object_type = UNKNOWN;
	}
	else if(dec->stream_type == 5)
	{
		if(dec->object_type == 0x40)
			dec->object_type = AUDIO_AAC;
		else if(dec->object_type == 0x6b)
			dec->object_type = AUDIO_MP2;
		else if(dec->object_type >= 0x66 && dec->object_type <= 0x69)
			dec->object_type = AUDIO_MP2;
		else
			dec->object_type = UNKNOWN;
	}
	else
		dec->object_type = dec->stream_type = UNKNOWN;

	if(dec->object_type != UNKNOWN)
	{
		//update the type of the current stream
		for(j = 0; j < pmt->es_cnt; j++)
		{
			if(pmt->es[j].mp4_es_id == es->id)
			{
				pmt->es[j].type = SL_PES_STREAM;
			}
		}
	}

	if(len > 13)
		parse_mp4_descriptors(pmt, &buf[13], len-13, dec);

	mp_msg(MSGT_DEMUX, MSGL_V, "MP4DECODER(0x%x), object_type: 0x%x, stream_type: 0x%x\n", len, dec->object_type, dec->stream_type);

	return len;
}

static uint16_t parse_mp4_decoder_specific_descriptor(uint8_t *buf, int len, void *elem)
{
	int i;
	mp4_decoder_config_t *dec;

	mp_msg(MSGT_DEMUX, MSGL_V, "PARSE_MP4_DECODER_SPECIFIC_DESCRIPTOR(%d)\n", len);
	dec = (mp4_decoder_config_t *) elem;
	if(!dec)
	{
		mp_msg(MSGT_DEMUX, MSGL_V, "argh! NULL elem passed, skip\n");
		return len;
	}

	mp_msg(MSGT_DEMUX, MSGL_DBG2, "MP4 SPECIFIC INFO BYTES: \n");
	for(i=0; i<len; i++)
		mp_msg(MSGT_DEMUX, MSGL_DBG2, "%02x ", buf[i]);
	mp_msg(MSGT_DEMUX, MSGL_DBG2, "\n");

	if(len > MAX_EXTRADATA_SIZE)
	{
		mp_msg(MSGT_DEMUX, MSGL_ERR, "DEMUX_TS, EXTRADATA SUSPICIOUSLY BIG: %d, REFUSED\r\n", len);
		return len;
	}
	memcpy(dec->buf, buf, len);
	dec->buf_size = len;

	return len;
}

static uint16_t parse_mp4_es_descriptor(pmt_t *pmt, uint8_t *buf, int len)
{
	int i = 0, j = 0, k, found;
	uint8_t flag;
	mp4_es_descr_t es, *target_es = NULL;

	mp_msg(MSGT_DEMUX, MSGL_V, "PARSE_MP4ES: len=%d\n", len);
	memset(&es, 0, sizeof(mp4_es_descr_t));
	while(i < len)
	{
		es.id = (buf[i] << 8) | buf[i+1];
		mp_msg(MSGT_DEMUX, MSGL_V, "MP4ES_ID: %d\n", es.id);
		i += 2;
		flag = buf[i];
		i++;
		if(flag & 0x80)
			i += 2;
		if(flag & 0x40)
			i += buf[i]+1;
		if(flag & 0x20)		//OCR, maybe we need it
			i += 2;

		j = parse_mp4_descriptors(pmt, &buf[i], len-i, &es);
		mp_msg(MSGT_DEMUX, MSGL_V, "PARSE_MP4ES, types after parse_mp4_descriptors: 0x%x, 0x%x\n", es.decoder.object_type, es.decoder.stream_type);
		if(es.decoder.object_type != UNKNOWN && es.decoder.stream_type != UNKNOWN)
		{
			found = 0;
			//search this ES_ID if we already have it
			for(k=0; k < pmt->mp4es_cnt; k++)
			{
				if(pmt->mp4es[k].id == es.id)
				{
					target_es = &(pmt->mp4es[k]);
					found = 1;
				}
			}

			if(! found)
			{
				pmt->mp4es = realloc_struct(pmt->mp4es, pmt->mp4es_cnt+1, sizeof(mp4_es_descr_t));
				if(!pmt->mp4es)
				{
					pmt->mp4es_cnt = 0;
					fprintf(stderr, "CAN'T REALLOC MP4_ES_DESCR\n");
					continue;
				}
				target_es = &(pmt->mp4es[pmt->mp4es_cnt]);
				pmt->mp4es_cnt++;
			}
			memcpy(target_es, &es, sizeof(mp4_es_descr_t));
			mp_msg(MSGT_DEMUX, MSGL_V, "MP4ES_CNT: %d, ID=%d\n", pmt->mp4es_cnt, target_es->id);
		}

		i += j;
	}

	return len;
}

static void parse_mp4_object_descriptor(pmt_t *pmt, uint8_t *buf, int len, void *elem)
{
	int i, j = 0, id;

	i=0;
	id = (buf[0] << 2) | ((buf[1] & 0xc0) >> 6);
	mp_msg(MSGT_DEMUX, MSGL_V, "PARSE_MP4_OBJECT_DESCRIPTOR: len=%d, OD_ID=%d\n", len, id);
	if(buf[1] & 0x20)
	{
		i += buf[2] + 1;	//url
		mp_msg(MSGT_DEMUX, MSGL_V, "URL\n");
	}
	else
	{
		i = 2;

		while(i < len)
		{
			j = parse_mp4_descriptors(pmt, &(buf[i]), len-i, elem);
			mp_msg(MSGT_DEMUX, MSGL_V, "OBJD, NOW i = %d, j=%d, LEN=%d\n", i, j, len);
			i += j;
		}
	}
}


static void parse_mp4_iod(pmt_t *pmt, uint8_t *buf, int len, void *elem)
{
	int i, j = 0;
	mp4_od_t *iod = &(pmt->iod);

	iod->id = (buf[0] << 2) | ((buf[1] & 0xc0) >> 6);
	mp_msg(MSGT_DEMUX, MSGL_V, "PARSE_MP4_IOD: len=%d, IOD_ID=%d\n", len, iod->id);
	i = 2;
	if(buf[1] & 0x20)
	{
		i += buf[2] + 1;	//url
		mp_msg(MSGT_DEMUX, MSGL_V, "URL\n");
	}
	else
	{
		i = 7;
		while(i < len)
		{
			j = parse_mp4_descriptors(pmt, &(buf[i]), len-i, elem);
			mp_msg(MSGT_DEMUX, MSGL_V, "IOD, NOW i = %d, j=%d, LEN=%d\n", i, j, len);
			i += j;
		}
	}
}

static int parse_mp4_descriptors(pmt_t *pmt, uint8_t *buf, int len, void *elem)
{
	int tag, descr_len, i = 0, j = 0;

	mp_msg(MSGT_DEMUX, MSGL_V, "PARSE_MP4_DESCRIPTORS, len=%d\n", len);
	if(! len)
		return len;

	while(i < len)
	{
		tag = buf[i];
		j = len - i -1;
		descr_len = get_mp4_desc_len(&(buf[i+1]), &j);
		mp_msg(MSGT_DEMUX, MSGL_V, "TAG=%d (0x%x), DESCR_len=%d, len=%d, j=%d\n", tag, tag, descr_len, len, j);
		if(descr_len > len - j+1)
		{
			mp_msg(MSGT_DEMUX, MSGL_V, "descriptor is too long, exit\n");
			return len;
		}
		i += j+1;

		switch(tag)
		{
			case 0x1:
				parse_mp4_object_descriptor(pmt, &(buf[i]), descr_len, elem);
				break;
			case 0x2:
				parse_mp4_iod(pmt, &(buf[i]), descr_len, elem);
				break;
			case 0x3:
				parse_mp4_es_descriptor(pmt, &(buf[i]), descr_len);
				break;
			case 0x4:
				parse_mp4_decoder_config_descriptor(pmt, &buf[i], descr_len, elem);
				break;
			case 0x05:
				parse_mp4_decoder_specific_descriptor(&buf[i], descr_len, elem);
				break;
			case 0x6:
				parse_mp4_slconfig_descriptor(&buf[i], descr_len, elem);
				break;
			default:
				mp_msg(MSGT_DEMUX, MSGL_V, "Unsupported mp4 descriptor 0x%x\n", tag);
		}
		i += descr_len;
	}

	return len;
}

static ES_stream_t *new_pid(ts_priv_t *priv, int pid)
{
	ES_stream_t *tss;

	tss = calloc(sizeof(*tss), 1);
	if(! tss)
		return NULL;
	tss->pid = pid;
	tss->ecm_pid = 8192;
	tss->last_cc = -1;
	tss->type = UNKNOWN;
	tss->subtype = UNKNOWN;
	tss->is_synced = 0;
	tss->extradata = NULL;
	tss->extradata_alloc = tss->extradata_len = 0;
	tss->component_tag = -1;
	tss->prog_idx = -1;
	tss->es_idx = -1;
	priv->ts.pids[pid] = tss;

	return tss;
}


static int parse_program_descriptors(pmt_t *pmt, uint8_t *buf, uint16_t len)
{
	uint16_t i = 0, k, olen = len;

	while(len >= 2)
	{
		mp_msg(MSGT_DEMUX, MSGL_V, "PROG DESCR, TAG=%x, LEN=%d(%x)\n", buf[i], buf[i+1], buf[i+1]);
		if(buf[i+1] > len-2)
		{
			mp_msg(MSGT_DEMUX, MSGL_V, "ERROR, descriptor len is too long, skipping\n");
			return olen;
		}

		if(buf[i] == 0x1d)
		{
			if(buf[i+3] == 2)	//buggy versions of vlc muxer make this non-standard mess (missing iod_scope)
				k = 3;
			else
				k = 4;		//this is standard compliant
			parse_mp4_descriptors(pmt, &buf[i+k], (int) buf[i+1]-(k-2), NULL);
		}
		else if (buf[i] == 0x09)  // CA descriptor
		{
			// if there are multiple CA desc specified, overrides the old one.
			// but this desc. should be specified exactly once.
			pmt->prog_ecm.cas_id = buf[i + 2] << 8 | buf[i + 3];
			pmt->prog_ecm.pid = (buf[i + 4] << 8 | buf[i + 5]) & 0x1fff;
			mp_msg(MSGT_DEMUX, MSGL_DBG2,
				"Program CA found.(casid:%d pid:0x%04hx)\n",
				pmt->prog_ecm.cas_id, pmt->prog_ecm.pid);
		}

		len -= 2 + buf[i+1];
		i += 2 + buf[i+1];
	}

	return olen;
}

static int parse_descriptors(struct pmt_es_t *es, uint8_t *ptr)
{
	int j, descr_len, len;

	j = 0;
	len = es->descr_length;
	while(len > 2)
	{
		descr_len = ptr[j+1];
		mp_msg(MSGT_DEMUX, MSGL_V, "...descr id: 0x%x, len=%d\n", ptr[j], descr_len);
		if(descr_len > len)
		{
			mp_msg(MSGT_DEMUX, MSGL_ERR, "INVALID DESCR LEN for tag %02x: %d vs %d max, EXIT LOOP\n", ptr[j], descr_len, len);
			return -1;
		}


		if(ptr[j] == 0x6a || ptr[j] == 0x7a)	//A52 Descriptor
		{
			if(es->type == 0x6)
			{
				es->type = AUDIO_A52;
				mp_msg(MSGT_DEMUX, MSGL_DBG2, "DVB A52 Descriptor\n");
			}
		}
		else if(ptr[j] == 0x7b)	//DVB DTS Descriptor
		{
			if(es->type == 0x6)
			{
				es->type = AUDIO_DTS;
				mp_msg(MSGT_DEMUX, MSGL_DBG2, "DVB DTS Descriptor\n");
			}
		}
		else if(ptr[j] == 0x56) // Teletext
		{
			if(descr_len >= 5) {
				memcpy(es->lang, ptr+j+2, 3);
				es->lang[3] = 0;
			}
			es->type = SPU_TELETEXT;
		}
		else if(ptr[j] == 0x59)	//Subtitling Descriptor
		{
			uint8_t subtype;

			mp_msg(MSGT_DEMUX, MSGL_DBG2, "Subtitling Descriptor\n");
			if(descr_len < 8)
			{
				mp_msg(MSGT_DEMUX, MSGL_DBG2, "Descriptor length too short for DVB Subtitle Descriptor: %d, SKIPPING\n", descr_len);
			}
			else
			{
				memcpy(es->lang, &ptr[j+2], 3);
				es->lang[3] = 0;
				subtype = ptr[j+5];
				if(
					(subtype >= 0x10 && subtype <= 0x13) ||
					(subtype >= 0x20 && subtype <= 0x23)
				)
				{
					es->type = SPU_DVB;
					//page parameters: compo page 2 bytes, ancillary page 2 bytes
				}
				else
					es->type = UNKNOWN;
			}
		}
		else if(ptr[j] == 0x50)	//Component Descriptor
		{
			mp_msg(MSGT_DEMUX, MSGL_DBG2, "Component Descriptor\n");
			memcpy(es->lang, &ptr[j+5], 3);
			es->lang[3] = 0;
		}
		else if(ptr[j] == 0xa)	//Language Descriptor
		{
			memcpy(es->lang, &ptr[j+2], 3);
			es->lang[3] = 0;
			mp_msg(MSGT_DEMUX, MSGL_V, "Language Descriptor: %s\n", es->lang);
		}
		else if(ptr[j] == 0x5)	//Registration Descriptor (looks like e fourCC :) )
		{
			mp_msg(MSGT_DEMUX, MSGL_DBG2, "Registration Descriptor\n");
			if(descr_len < 4)
			{
				mp_msg(MSGT_DEMUX, MSGL_DBG2, "Registration Descriptor length too short: %d, SKIPPING\n", descr_len);
			}
			else
			{
				char *d;
				memcpy(es->format_descriptor, &ptr[j+2], 4);
				es->format_descriptor[4] = 0;

				d = &ptr[j+2];
				if(d[0] == 'A' && d[1] == 'C' && d[2] == '-' && d[3] == '3')
				{
					es->type = AUDIO_A52;
				}
				else if(d[0] == 'D' && d[1] == 'T' && d[2] == 'S' && d[3] == '1')
				{
					es->type = AUDIO_DTS;
				}
				else if(d[0] == 'D' && d[1] == 'T' && d[2] == 'S' && d[3] == '2')
				{
					es->type = AUDIO_DTS;
				}
				else if(d[0] == 'V' && d[1] == 'C' && d[2] == '-' && d[3] == '1')
				{
					es->type = VIDEO_VC1;
				}
				else if(d[0] == 'd' && d[1] == 'r' && d[2] == 'a' && d[3] == 'c')
				{
					es->type = VIDEO_DIRAC;
				}
				else if(d[0] == 'B' && d[1] == 'S' && d[2] == 'S' && d[3] == 'D')
				{
					es->type = AUDIO_S302M;
				}
				else if(d[0] == 'H' && d[1] == 'E' && d[2] == 'V' && d[3] == 'C')
				{
					es->type = VIDEO_HEVC;
				}
				else
					es->type = UNKNOWN;
				mp_msg(MSGT_DEMUX, MSGL_DBG2, "FORMAT %s\n", es->format_descriptor);
			}
		}
		else if(ptr[j] == 0x1e || ptr[j] == 0x1f)
		{
			// 0x1f is FMC, but currently it is easiest to handle them the same way
			es->mp4_es_id = (ptr[j+2] << 8) | ptr[j+3];
			mp_msg(MSGT_DEMUX, MSGL_V, "SL Descriptor: ES_ID: %d(%x), pid: %d\n", es->mp4_es_id, es->mp4_es_id, es->pid);
		}
		else if (ptr[j] == 0x09) // CA descriptor
		{
			es->es_ecm.cas_id = (ptr[j+2] << 8) | ptr[j+3];
			es->es_ecm.pid = ((ptr[j+4] << 8) | ptr[j+5]) & 0x1fff;
			mp_msg(MSGT_DEMUX, MSGL_DBG2, "ES CA found.(casid:%d pid:0x%04hx)\n",
				es->es_ecm.cas_id, es->es_ecm.pid);
		}
		else if(ptr[j] == 0x52)	// Stream Identifier
		{
			es->component_tag = ptr[j+2];
			mp_msg(MSGT_DEMUX, MSGL_DBG2, "ES tag found.(tag:0x%02x)\n",
				es->component_tag);
		}
		else if(ptr[j] == 0xC0) // hierarchical transmission descriptor
		{
			uint16_t refpid;

			refpid = (ptr[j + 3] & 0x1f) << 8 | ptr[j + 4];
			if (refpid != 0x1fff && ! (ptr[j+2] & 0x01))
				es->highCN_pid = refpid;
		}
		else
			mp_msg(MSGT_DEMUX, MSGL_DBG2, "Unknown descriptor 0x%x, SKIPPING\n", ptr[j]);

		len -= 2 + descr_len;
		j += 2 + descr_len;
	}

	return 1;
}

static int parse_sl_section(pmt_t *pmt, ts_section_t *section, int is_start, unsigned char *buff, int size)
{
	int tid, len, skip;
	uint8_t *ptr;
	skip = collect_section(section, is_start, buff, size);
	if(! skip)
		return 0;

	ptr = &(section->buffer[skip]);
	tid = ptr[0];
	len = ((ptr[1] & 0x0f) << 8) | ptr[2];
	mp_msg(MSGT_DEMUX, MSGL_V, "TABLEID: %d (av. %d), skip=%d, LEN: %d\n", tid, section->buffer_len, skip, len);
	if(len > 4093 || section->buffer_len < len || tid != 5)
	{
		mp_msg(MSGT_DEMUX, MSGL_V, "SECTION TOO LARGE or wrong section type, EXIT\n");
		return 0;
	}

	if(! (ptr[5] & 1))
		return 0;

	//8 is the current position, len - 9 is the amount of data available
	parse_mp4_descriptors(pmt, &ptr[8], len - 9, NULL);

	return 1;
}

static void parse_acomp(ts_priv_t * priv, pmt_t * pmt, unsigned char *buf)
{
	int len;
	uint8_t component_tag;
	int multi_lingual;
	int is_dualmono;
	char *lang1, *lang2;
	int i;
	sh_audio_t *sh_audio;
	ES_stream_t *tss;

	len = buf[1] + 2;
	is_dualmono = (buf[3] == 0x02);
	multi_lingual = !!(buf[7] & 0x80);
	if (len < 11 || (multi_lingual && len < 14)) {
		mp_msg(MSGT_DEMUX, MSGL_WARN,
			"PARSE_EIT: broken Audio Component descriptor.\n");
		return;
	}

	component_tag = buf[4];
	lang1 = (char *) &buf[8];
	lang2 = (char *) &buf[11];
	if (component_tag < 0x10 || lang1[0] == 0)
		return;
	mp_msg(MSGT_DEMUX, MSGL_V, "PARSE_EIT: AudioComponet Desc. "
		"tag:0x%02hhx(%.3s%s%.3s)...", component_tag, lang1,
		(is_dualmono ? "/" : ""),  (multi_lingual ? lang2 : ""));

	for (i = 0; i < pmt->es_cnt; i++) {
		if (pmt->es[i].component_tag != component_tag)
			continue;

		mp_msg(MSGT_DEMUX, MSGL_V, "pid:0x%04x", pmt->es[i].pid);

		pmt->es[i].is_dualmono = is_dualmono;
		memcpy(pmt->es[i].lang, lang1, 3);
		pmt->es[i].lang[3] = 0;
		if (multi_lingual) {
			memcpy(pmt->es[i].lang2, lang2, 3);
			pmt->es[i].lang2[3] = 0;
		} else if (is_dualmono) {
			memcpy(pmt->es[i].lang2, lang1, 3);
			pmt->es[i].lang2[3] = 0;
		} else
			pmt->es[i].lang2[0] = 0;

		// copy the info to priv->ts.pids[] & sh_audio
		tss = priv->ts.pids[pmt->es[i].pid];
		if (tss) {
			tss->component_tag = component_tag;
			tss->is_dualmono = is_dualmono;
			memcpy(tss->lang, pmt->es[i].lang, 4);
			memcpy(tss->lang2, pmt->es[i].lang2, 4);
		}
		sh_audio = priv->ts.streams[pmt->es[i].pid].sh;
		if (sh_audio) {
			free(sh_audio->lang);
			sh_audio->lang = NULL;
			free(sh_audio->lang2);
			sh_audio->lang2 = NULL;

			if (tss->lang[0])
				sh_audio->lang = strdup(tss->lang);
			if (tss->lang2[0])
				sh_audio->lang2 = strdup(tss->lang2);
		}
		break;
	}
	mp_msg(MSGT_DEMUX, MSGL_V, "\n");
	return;
}


// parse EIT to set audio language
static int parse_eit(ts_priv_t * priv, int is_start, unsigned char *buff, int size)
{
	int skip;
	unsigned char *base;
	int len, dlen;
	int idx;
	uint8_t ver;
	pmt_t *pmt;
	uint16_t progid;

	// TODO: store the EIT if its corresponding PMT is not received yet,
	//       and update  sh_audio->lang in parse_pmt() / ts_add_stream()

	skip = collect_section(&priv->eit_section, is_start, buff, size);
	if(! skip)
		return 0;

	if (!priv->pmt || !priv->pmt_cnt)
		return 0;
	base = &(priv->eit_section.buffer[skip]);
	if (base[0] != 0x4e) // only EIT_p/f for the TS is required
		return 0;
	len = ((base[1] & 0x0f) << 8) | base[2];

	if (len < 15)  {
		mp_msg(MSGT_DEMUX, MSGL_WARN,
			"PARSE_EIT: too short section(%d bytes)\n", len + 3);
		return -1;
	}

	// just process current, sec:0 (present) with at least 1 event
	if (!(base[5] & 0x01) || base[6])
		return 0;
	progid = (base[3] << 8) | base[4];
	idx = progid_idx_in_pmt(priv, progid);
	if (idx < 0)	// PMT not ready yet.
		return 0;

	ver = (base[5] >> 1) & 0x1f;
	mp_msg(MSGT_DEMUX, MSGL_V,
		"EIT for prog:%hu(%#.4hx) ver:%#.2hhx.\n", progid, progid, ver);

	pmt = &(priv->pmt[idx]);
	if (ver == pmt->eit_version) // already processed
		return 0;

	if (calc_crc32(base, len + 3)) {
		mp_msg(MSGT_DEMUX, MSGL_INFO, "broken EIT, CRC check failed.\n");
		return 0;
	}

	if (len < 27)  {
		mp_msg(MSGT_DEMUX, MSGL_WARN,
			"PARSE_EIT: too short section(%d bytes)\n", len + 3);
		return -1;
	}

	base += 14;	// base: beggining of the event loop
	dlen = (base[10] & 0x0f) << 8 | base[11];
	if (14 + 12 + dlen + 4 > len + 3) {
		mp_msg(MSGT_DEMUX, MSGL_WARN,
			"PARSE_EIT: broken section(total:%dB, desc:%dB)\n",
			len + 3, dlen);
		return -1;
	}
	base += 12;	// base: 1st byte of the descriptors
	pmt->eit_version = ver;

	while (dlen >= 2) {
		if (base[1] + 2 > dlen)
			break;
		if (base[0] == 0xC4)
			parse_acomp(priv, pmt, base);
		dlen -= base[1] + 2;
		base += base[1] + 2;
	}
	return 0;
}


static void ts_add_ecm(ts_priv_t *priv, cas_t *cas)
{
	ecm_t *ecm;

	if (cas->pid >= 8191)
		return;
	if (priv->ts.ecms[cas->pid])
		return;

	ecm = (ecm_t *) calloc(1, sizeof(ecm_t));
	if (ecm == NULL)
	{
		mp_msg(MSGT_DEMUX, MSGL_ERR, "ts_add_ecm, couldn't alloc ecm_t.\n");
		return;
	}

	ecm->cas.cas_id = cas->cas_id;
	ecm->cas.pid = cas->pid;
	ecm->version_number = 0x20;
	priv->ts.ecms[cas->pid] = ecm;
	return;
}

static int parse_pmt(ts_priv_t * priv, uint16_t progid, uint16_t pid, int is_start, unsigned char *buff, int size)
{
	unsigned char *base, *es_base;
	pmt_t *pmt;
	int32_t idx, es_count, section_bytes;
	uint8_t m=0;
	int skip;
	ts_section_t *section;
	ES_stream_t *tss;
	int i;
	uint8_t ver;
	int pidx;

	idx = progid_idx_in_pmt(priv, progid);

	if(idx == -1)
	{
		priv->pmt = realloc_struct(priv->pmt, priv->pmt_cnt + 1, sizeof(pmt_t));
		if(!priv->pmt)
		{
			int sz = (priv->pmt_cnt + 1) * sizeof(pmt_t);
			priv->pmt_cnt = 0;
			mp_msg(MSGT_DEMUX, MSGL_ERR, "PARSE_PMT: COULDN'T REALLOC %d bytes, NEXT\n", sz);
			return 0;
		}
		idx = priv->pmt_cnt;
		memset(&(priv->pmt[idx]), 0, sizeof(pmt_t));
		priv->pmt_cnt++;
		priv->pmt[idx].progid = progid;
		priv->pmt[idx].version_number = VERSION_NONE;
		priv->pmt[idx].eit_version = VERSION_NONE;
		priv->pmt[idx].prog_ecm.pid = 8192;  // set INVALID PID
	}

	pmt = &(priv->pmt[idx]);

	section = &(pmt->section);
	skip = collect_section(section, is_start, buff, size);
	if(! skip)
		return 0;

	base = &(section->buffer[skip]);

	mp_msg(MSGT_DEMUX, MSGL_V, "FILL_PMT(prog=%d), PMT_len: %d, IS_START: %d, TS_PID: %d, SIZE=%d, M=%d, ES_CNT=%d, IDX=%d, PMT_PTR=%p\n",
		progid, pmt->section.buffer_len, is_start, pid, size, m, pmt->es_cnt, idx, pmt);

	pmt->table_id = base[0];
	if(pmt->table_id != 2)
		return -1;
	if (!(base[5] & 1))
	{
		mp_msg(MSGT_DEMUX, MSGL_DBG2, "Ignoreing a non-current PMT.\n");
		return 0;
	}
	if (pmt->version_number == ((base[5] >> 1) & 0x1f))
	{
		mp_msg(MSGT_DEMUX, MSGL_DBG2, "Ignoreing an unchanged PMT.\n");
		return 0;
	}
	if (calc_crc32(base, (((base[1] & 0x0f) << 8) | base[2]) + 3)) {
		mp_msg(MSGT_DEMUX, MSGL_INFO, "broken PMT. CRC check failed.\n");
		return 0;
	}
	if (pmt->es) {
		mp_msg(MSGT_DEMUX, MSGL_V, "releasing the previous PMT.\n");
		for (i = 0; i < pmt->es_cnt; i++) {
			tss = priv->ts.pids[pmt->es[i].pid];
			if (!tss || tss->prog_idx != idx)
				continue;
			free(tss);
			priv->ts.pids[pmt->es[i].pid] = NULL;
		}
		free(pmt->es);
		pmt->es = NULL;
		pmt->es_cnt = 0;
		free(pmt->od);
		pmt->od_cnt = 0;
		free(pmt->mp4es);
		pmt->mp4es_cnt = 0;
		pmt->prog_ecm.pid = 8192;
	}

	pmt->ssi = base[1] & 0x80;
	pmt->section_length = (((base[1] & 0xf) << 8 ) | base[2]);
	ver = (base[5] >> 1) & 0x1f;
	if (ver == pmt->version_number)
		return 0;
	pmt->version_number = ver;
	pmt->curr_next = (base[5] & 1);
	pmt->section_number = base[6];
	pmt->last_section_number = base[7];
	pmt->PCR_PID = ((base[8] & 0x1f) << 8 ) | base[9];
	pmt->prog_descr_length = ((base[10] & 0xf) << 8 ) | base[11];
	if(pmt->prog_descr_length > pmt->section_length - 9)
	{
		mp_msg(MSGT_DEMUX, MSGL_V, "PARSE_PMT, INVALID PROG_DESCR LENGTH (%d vs %d)\n", pmt->prog_descr_length, pmt->section_length - 9);
		return -1;
	}

	if(pmt->prog_descr_length)
		parse_program_descriptors(pmt, &base[12], pmt->prog_descr_length);

	if (pmt->prog_ecm.pid < 8191)
		ts_add_ecm(priv, &pmt->prog_ecm);

	es_base = &base[12 + pmt->prog_descr_length];	//the beginning of th ES loop

	section_bytes= pmt->section_length - 13 - pmt->prog_descr_length;
	es_count  = 0;

	pidx = idx;	// because idx is reused by the following while() block
	while(section_bytes >= 5)
	{
		int es_pid, es_type;
		cas_t es_ecm;

		es_type = es_base[0];
		es_pid = ((es_base[1] & 0x1f) << 8) | es_base[2];

		idx = es_pid_in_pmt(pmt, es_pid);
		if(idx == -1)
		{
			pmt->es = realloc_struct(pmt->es, pmt->es_cnt + 1, sizeof(struct pmt_es_t));
			if(!pmt->es)
			{
				int sz = sizeof(struct pmt_es_t) * (pmt->es_cnt + 1);
				pmt->es_cnt = 0;
				mp_msg(MSGT_DEMUX, MSGL_ERR, "PARSE_PMT, COULDN'T ALLOCATE %d bytes for PMT_ES\n", sz);
				continue;
			}
			idx = pmt->es_cnt;
			memset(&(pmt->es[idx]), 0, sizeof(struct pmt_es_t));
			pmt->es[idx].es_ecm.pid = 8192;
			pmt->es[idx].component_tag = -1;
			pmt->es_cnt++;
		}

		pmt->es[idx].descr_length = ((es_base[3] & 0xf) << 8) | es_base[4];


		if(pmt->es[idx].descr_length > section_bytes - 5)
		{
			mp_msg(MSGT_DEMUX, MSGL_V, "PARSE_PMT, ES_DESCR_LENGTH TOO LARGE %d > %d, EXIT\n",
				pmt->es[idx].descr_length, section_bytes - 5);
			return -1;
		}


		pmt->es[idx].pid = es_pid;
		if(es_type != 0x6)
			pmt->es[idx].type = UNKNOWN;
		else
			pmt->es[idx].type = es_type;

		pmt->es[idx].component_tag = -1;
		pmt->es[idx].lang[0] = 0;
		pmt->es[idx].highCN_pid = 0;
		parse_descriptors(&pmt->es[idx], &es_base[5]);

		if (pmt->es[idx].es_ecm.pid < 8192) // per-stream ECM specified.
			es_ecm = pmt->es[idx].es_ecm;
		else
			es_ecm = pmt->prog_ecm;

		if (es_ecm.pid < 8191)
			ts_add_ecm(priv, &es_ecm);

		switch(es_type)
		{
			case 1:
				pmt->es[idx].type = VIDEO_MPEG1;
				break;
			case 2:
				pmt->es[idx].type = VIDEO_MPEG2;
				break;
			case 3:
			case 4:
				pmt->es[idx].type = AUDIO_MP2;
				break;
			case 6:
				if(pmt->es[idx].type == 0x6)	//this could have been ovrwritten by parse_descriptors
					pmt->es[idx].type = UNKNOWN;
				break;
			case 0x10:
				pmt->es[idx].type = VIDEO_MPEG4;
				break;
			case 0x0f:
				pmt->es[idx].type = AUDIO_AAC;
				break;
			case 0x11:
				pmt->es[idx].type = AUDIO_AAC_LATM;
				for (i = 0; i < pmt->mp4es_cnt; i++)
					if (pmt->mp4es[i].id == pmt->es[idx].mp4_es_id &&
					    pmt->mp4es[i].decoder.object_type == AUDIO_AAC)
						pmt->es[idx].type = AUDIO_AAC;
				break;
			case 0x1b:
				pmt->es[idx].type = VIDEO_H264;
				break;
			case 0x12:
				pmt->es[idx].type = SL_PES_STREAM;
				break;
			case 0x13:
				pmt->es[idx].type = SL_SECTION;
				break;
			case 0x80:
				pmt->es[idx].type = AUDIO_PCM_BR;
				break;
			case 0x81:
				pmt->es[idx].type = AUDIO_A52;
				break;
			case 0x8A:
			case 0x82:
			case 0x85:
			case 0x86:
				pmt->es[idx].type = AUDIO_DTS;
				break;
			case 0x90:
				pmt->es[idx].type = SPU_PGS;
				break;
			case 0xD1:
				pmt->es[idx].type = VIDEO_DIRAC;
				break;
			case 0xEA:
				pmt->es[idx].type = VIDEO_VC1;
				break;
			default:
				mp_msg(MSGT_DEMUX, MSGL_DBG2, "UNKNOWN ES TYPE=0x%x\n", es_type);
				pmt->es[idx].type = UNKNOWN;
		}

		tss = priv->ts.pids[es_pid];			//an ES stream
		if(tss == NULL)
			tss = new_pid(priv, es_pid);
		if (tss)
		{
			sh_common_t *sh = priv->ts.streams[es_pid].sh;
			int ctag = pmt->es[idx].component_tag;

			// copy PES info to priv->ts.pids[] and sh
			tss->type = pmt->es[idx].type;
			tss->ecm_pid = es_ecm.pid;
			tss->component_tag = ctag;
			mp_msg(MSGT_DEMUX, MSGL_DBG2,
				"set CA(pid:0x%04hx) for ES(pid:0x%04hx)\n", es_ecm.pid, es_pid);
			// PES's can be shared amoung programs
			if (tss->prog_idx < 0 || progid == priv->prog) {
				tss->prog_idx = pidx;
				tss->es_idx = idx;
			}
			memcpy(tss->lang, pmt->es[idx].lang, 4);

			// and copy some of them to sh
			if (!IS_VIDEO(tss->type) && tss->type != UNKNOWN &&
			    sh != NULL && pmt->es[idx].lang[0])
				sh->lang = strdup(pmt->es[idx].lang);

			if (sh != NULL)
				sh->default_track =
					(ctag == 0 || ctag == 0x10 || ctag == 0x30);
		}

		section_bytes -= 5 + pmt->es[idx].descr_length;
		mp_msg(MSGT_DEMUX, MSGL_V, "PARSE_PMT(%d INDEX %d), STREAM: %d, FOUND pid=0x%x (%d), type=0x%x, ES_DESCR_LENGTH: %d, bytes left: %d\n",
			progid, idx, es_count, pmt->es[idx].pid, pmt->es[idx].pid, pmt->es[idx].type, pmt->es[idx].descr_length, section_bytes);


		es_base += 5 + pmt->es[idx].descr_length;

		es_count++;
	}

	mp_msg(MSGT_DEMUX, MSGL_V, "----------------------------\n");
	return 1;
}

static void parse_ecm(ts_priv_t *priv, uint16_t pid, int is_start,
			unsigned char *buf, int size)
{
#ifdef CONFIG_DEMULTI2
	ecm_t *ecm;
	int skip;
	unsigned char *ptr;
	int len;
	uint8_t version;
	int ret;

	if (priv->dm2_handle == NULL) {
		mp_msg(MSGT_DEMUX, MSGL_V, "ECM decoding feature not ready.\n");
		return;
	}

	mp_msg(MSGT_DEMUX, MSGL_DBG2, "parsing an ECM section.\n");

	if (priv->ts.ecms[pid] == NULL)
		return;

	if (priv->ts.ecms[pid]->cas.cas_id != 0x0005)
		return;

	ecm = priv->ts.ecms[pid];
	skip = collect_section(&ecm->section, is_start, buf, size);
	if (!skip)
		return;
	ptr = &ecm->section.buffer[skip];

	if (ptr[0] != 0x82)
	{
		mp_msg(MSGT_DEMUX, MSGL_INFO, "parse_ecm: invalid tid:0x%02hhx\n",
			ptr[0]);
		return;
	}

	// processing the next ECM beforehand is OK,
	//  because Ks[{odd,even}] is updated in turn, replacing the unused one.
/*
	if (!(ptr[5] & 0x01))
	{
		mp_msg(MSGT_DEMUX, MSGL_DBG2, "ECM not yet applicable.\n");
		return;
	}
 */
	if (calc_crc32(ptr, (((ptr[1] & 0x0f) << 8) | ptr[2]) + 3)) {
		mp_msg(MSGT_DEMUX, MSGL_INFO, "broken ECM, CRC check failed.\n");
		return;
	}
	version = (ptr[5] & 0x3e) >> 1;
	if (version == ecm->version_number)
	{
		mp_msg(MSGT_DEMUX, MSGL_DBG2, "Ignoring an unchanged ECM.\n");
		return;
	}
	ecm->version_number = version;

	// remove the section header & section CRC trailer
	len = (((ptr[1] & 0x0f) << 8) | ptr[2]) - 5 -4;
	ptr = &ecm->section.buffer[skip + 8];

	if (len < 30 || len >= 256)
	{
		mp_msg(MSGT_DEMUX, MSGL_WARN, "Too long ECM message(%d)\n", len);
		return;
	}

	ret = demulti2_feed_ecm(priv->dm2_handle, ptr, len, pid);
	if (ret != DEMULTI2_RET_OK)
		mp_msg(MSGT_DEMUX, MSGL_V, "failed to feed ECM (%d)\n", ret);
#endif /* CONFIG_DEMULTI2 */
	return;
}

static pmt_t* pmt_of_pid(ts_priv_t *priv, int pid, mp4_decoder_config_t **mp4_dec)
{
	int32_t i, j, k;

	if (pid >= 8192 || !priv->pmt || !priv->ts.pids[pid])
		return NULL;

	i = priv->ts.pids[pid]->prog_idx;
	j = priv->ts.pids[pid]->es_idx;
	if (i < 0 || i >= priv->pmt_cnt || j < 0)
		return NULL;

	//search mp4_es_id
	if(priv->pmt[i].es && j < priv->pmt[i].es_cnt &&
	   priv->pmt[i].es[j].mp4_es_id)
	{
		for(k = 0; k < priv->pmt[i].mp4es_cnt; k++)
		{
			if(priv->pmt[i].mp4es[k].id == priv->pmt[i].es[j].mp4_es_id)
			{
				*mp4_dec = &(priv->pmt[i].mp4es[k].decoder);
				break;
			}
		}
	}
	return &(priv->pmt[i]);
}


static int32_t pid_type_from_pmt(ts_priv_t *priv, int pid)
{
	int32_t pmt_idx, pid_idx, i, j;

	pmt_idx = progid_idx_in_pmt(priv, priv->prog);

	if(pmt_idx != -1)
	{
		pid_idx = es_pid_in_pmt(&(priv->pmt[pmt_idx]), pid);
		if(pid_idx != -1)
			return priv->pmt[pmt_idx].es[pid_idx].type;
	}
	//else
	//{
		for(i = 0; i < priv->pmt_cnt; i++)
		{
			pmt_t *pmt = &(priv->pmt[i]);
			for(j = 0; j < pmt->es_cnt; j++)
				if(pmt->es[j].pid == pid)
					return pmt->es[j].type;
		}
	//}

	return UNKNOWN;
}


static uint8_t *pid_lang_from_pmt(ts_priv_t *priv, int pid)
{
	int32_t i, j;

	if (! priv->pmt)
		return NULL;

	i = priv->ts.pids[pid]->prog_idx;

	if(i >= 0)
	{
		j = priv->ts.pids[pid]->es_idx;
		if(priv->pmt[i].es)
			return priv->pmt[i].es[j].lang;
	}
	else
	{
		for(i = 0; i < priv->pmt_cnt; i++)
		{
			pmt_t *pmt = &(priv->pmt[i]);
			for(j = 0; j < pmt->es_cnt; j++)
				if(pmt->es[j].pid == pid)
					return pmt->es[j].lang;
		}
	}

	return NULL;
}


static int fill_packet(demuxer_t *demuxer, demux_stream_t *ds, demux_packet_t **dp, int *dp_offset, TS_stream_info *si)
{
	int ret = 0;

	if(*dp && *dp_offset <= 0)
	{
		free_demux_packet(*dp);
		*dp = NULL;
	}
	if(*dp)
	{
		ret = *dp_offset;
		resize_demux_packet(*dp, ret);	//shrinked to the right size
		ds_add_packet(ds, *dp);
		mp_msg(MSGT_DEMUX, MSGL_DBG2, "ADDED %d  bytes to %s fifo, PTS=%.3f\n", ret, (ds == demuxer->audio ? "audio" : (ds == demuxer->video ? "video" : "sub")), (*dp)->pts);
		if(si)
		{
			float diff = (*dp)->pts - si->last_pts;
			float dur;

			if(abs(diff) > 1) //1 second, there's a discontinuity
			{
				si->duration += si->last_pts - si->first_pts;
				si->first_pts = si->last_pts = (*dp)->pts;
			}
			else
			{
				si->last_pts = (*dp)->pts;
			}
			si->size += ret;
			dur = si->duration + (si->last_pts - si->first_pts);

			if(dur > 0 && ds == demuxer->video)
			{
				ts_priv_t * priv = (ts_priv_t*) demuxer->priv;
				if(dur > 1)	//otherwise it may be unreliable
					priv->vbitrate = (uint32_t) ((float) si->size / dur);
			}
		}
	}

	*dp = NULL;
	*dp_offset = 0;

	return ret;
}

static int fill_extradata(mp4_decoder_config_t * mp4_dec, ES_stream_t *tss)
{
	uint8_t *tmp;

	mp_msg(MSGT_DEMUX, MSGL_DBG2, "MP4_dec: %p, pid: %d\n", mp4_dec, tss->pid);

	if(mp4_dec->buf_size > tss->extradata_alloc)
	{
		tmp = realloc(tss->extradata, mp4_dec->buf_size);
		if(!tmp)
			return 0;
		tss->extradata = tmp;
		tss->extradata_alloc = mp4_dec->buf_size;
	}
	memcpy(tss->extradata, mp4_dec->buf, mp4_dec->buf_size);
	tss->extradata_len = mp4_dec->buf_size;
	mp_msg(MSGT_DEMUX, MSGL_V, "EXTRADATA: %p, alloc=%d, len=%d\n", tss->extradata, tss->extradata_alloc, tss->extradata_len);

	return tss->extradata_len;
}

static void reset_fifos(demuxer_t *demuxer, int v, int a, int s);

static void
select_pes_in_pmt(demuxer_t *demuxer, pmt_t *pmt, int lowcn, demux_program_t *prog);

static void reselect_streams(demuxer_t *demuxer)
{
	ts_priv_t *priv = (ts_priv_t *)demuxer->priv;
	int pidx;
	demux_program_t prog;

	int prev_apid, prev_vpid, prev_spid;
	int pid;

	mp_msg(MSGT_DEMUX, MSGL_INFO, "PMT info changed. re-selecting streams\n");
	if (priv->prog == 0) {
		mp_msg(MSGT_DEMUX, MSGL_ERR, "no PMT selected.\n");
		return;
	}
	pidx = progid_idx_in_pmt(priv, priv->prog);
	if (pidx < 0) {
		mp_msg(MSGT_DEMUX, MSGL_ERR,
			"PMT info for id:%d not found.\n", priv->prog);
		return;
	}
	select_pes_in_pmt(demuxer, &priv->pmt[pidx], low_cn, &prog);

	if (demuxer->audio->id >= 0) {
		prev_apid = pid_from_id(priv, demuxer->audio->id, TYPE_AUDIO);
		if (prev_apid < 0)
			mp_msg(MSGT_DEMUX, MSGL_ERR, "broken internal table...\n");
	} else
		prev_apid = -1;

	if (demuxer->video->id >= 0) {
		prev_vpid = pid_from_id(priv, demuxer->video->id, TYPE_VIDEO);
		if (prev_vpid < 0)
			mp_msg(MSGT_DEMUX, MSGL_ERR, "broken internal table...\n");
	} else
		prev_vpid = -1;

	if (demuxer->sub->id >= 0) {
		prev_spid = pid_from_id(priv, demuxer->sub->id, TYPE_SUB);
		if (prev_spid < 0)
			mp_msg(MSGT_DEMUX, MSGL_ERR, "broken internal table...\n");
	} else
		prev_spid = -1;


	if (prev_apid >= 0 && prev_apid != prog.aid) {
		reset_fifos(demuxer, 1, 0, 0);
		pid = prog.aid;
		if (pid < 0) {
			mp_msg(MSGT_DEMUX, MSGL_INFO, "no audio found in prog:%d\n", priv->prog);
			demuxer->audio->id = -1;
		} else {
			ts_add_stream(demuxer, priv->ts.pids[pid]);
			demuxer->audio->id = priv->ts.streams[pid].id;
			demuxer->audio->sh = priv->ts.streams[pid].sh;
			ds_free_packs(demuxer->audio);

			mp_msg(MSGT_DEMUX, MSGL_INFO, "audio stream pid: %#04hx -> %#04hx\n",
				prev_apid, pid);
		}
	}

	if (prev_vpid >= 0 && prev_vpid != prog.vid) {
		reset_fifos(demuxer, 0, 1, 0);
		pid = prog.vid;
		if (pid < 0) {
			mp_msg(MSGT_DEMUX, MSGL_INFO, "no video found in prog:%d\n", priv->prog);
			demuxer->video->id = -1;
		} else {
			ts_add_stream(demuxer, priv->ts.pids[pid]);
			demuxer->video->id = priv->ts.streams[pid].id;
			demuxer->video->sh = priv->ts.streams[pid].sh;
			ds_free_packs(demuxer->video);

			mp_msg(MSGT_DEMUX, MSGL_INFO, "video stream pid: %#04hx -> %#04hx(%d)\n",
				prev_vpid, pid, priv->ts.streams[pid].id);
		}
	}

	if (prev_spid >= 0 && prev_spid != prog.sid) {
		reset_fifos(demuxer, 0, 0, 1);
		pid = prog.sid;
		if (pid < 0) {
			mp_msg(MSGT_DEMUX, MSGL_INFO, "no sub found in prog:%d\n", priv->prog);
			demuxer->sub->id = -1;
		} else {
			ts_add_stream(demuxer, priv->ts.pids[pid]);
			demuxer->sub->id = priv->ts.streams[pid].id;
			demuxer->sub->sh = priv->ts.streams[pid].sh;
			ds_free_packs(demuxer->sub);

			mp_msg(MSGT_DEMUX, MSGL_INFO, "subpic stream pid: %#04hx -> %#04hx\n",
				prev_spid, pid);
		}
	}
}

static void reset_es(ts_priv_t *priv, int pid)
{
	ES_stream_t *es;
	ecm_t *ecm;

	if (!priv || pid < 16 || pid >= 8191)
		return;

	es = priv->ts.pids[pid];
	if (!es)
		return;

	es->last_cc = -1;
	es->is_synced = 0;

	if (es->ecm_pid < 0 || es->ecm_pid >= 8191)
		return;

	ecm = priv->ts.ecms[es->ecm_pid];
	if (!ecm)
		return;

	// reset ECM status
	// It may be reset multiple times as the ECM stream can be shared
	// amoung PES's, but that's ok and no halm.
	ecm->version_number = 0x20; // reset the previous version number
	return;
}

static void reset_on_discon(demuxer_t *demuxer)
{
	int i;
	ts_priv_t *priv = demuxer->priv;

	for (i = 0; i < priv->last_aid && demuxer->a_streams[i]; i++) {
		sh_audio_t *sh = demuxer->a_streams[i];
		reset_es(priv, sh->aid);
	}

	for (i = 0; i < priv->last_vid && demuxer->v_streams[i]; i++) {
		sh_video_t *sh = demuxer->v_streams[i];
		reset_es(priv, sh->vid);
	}

	for (i = 0; i < priv->last_sid && demuxer->s_streams[i]; i++) {
		sh_sub_t *sh = demuxer->s_streams[i];
		reset_es(priv, sh->sid);
	}

	priv->pcr_delta = 0;
	videobuf_code_len = 0;
	return;
}

static int check_discon(demuxer_t *demuxer, double pcr)
{
	double old, d;
	ts_priv_t * priv = (ts_priv_t*) demuxer->priv;

	old = demuxer->reference_clock;
	d = pcr - old;
	if (d < - MP_PTS_WRAP_THRESHOLD)
		d += MP_PTS_WRAP_VALUE;
	else if (d > MP_PTS_WRAP_THRESHOLD)
		d -= MP_PTS_WRAP_VALUE;
	demuxer->reference_clock = pcr;

	if (d <= 0.0)
		return 1;

	if (priv->pcr_delta == 0.0) {
		priv->pcr_delta = d;
		return 0;
	}

	if (d < priv->pcr_delta * 3 || priv->pcr_delta < 0.5) {
		priv->pcr_delta = 0.8 * priv->pcr_delta  + 0.2 * d;
		return 0;
	}

	mp_msg(MSGT_DEMUX, MSGL_WARN, "PCR jump from %g to %g\n", old, pcr);
	return 1;
}

// 0 = EOF or no stream found
// else = [-] number of bytes written to the packet
static int ts_parse(demuxer_t *demuxer , ES_stream_t *es, unsigned char *packet, int probe)
{
	ES_stream_t *tss;
	int buf_size, is_start, pid, base;
	int len, cc, cc_ok, afc, retv = 0, is_video, is_audio, is_sub;
	ts_priv_t * priv = (ts_priv_t*) demuxer->priv;
	stream_t *stream = demuxer->stream;
	char *p;
	demux_stream_t *ds = NULL;
	demux_packet_t **dp = NULL;
	int *dp_offset = 0, *buffer_size = 0;
	int32_t progid, pid_type, bad, ts_error;
	int junk = 0, rap_flag = 0;
	pmt_t *pmt;
	mp4_decoder_config_t *mp4_dec;
	TS_stream_info *si;
	int scrambled;


	memset(es, 0, sizeof(*es));
	while(1)
	{
		bad = ts_error = 0;
		ds = NULL;
		dp = NULL;
		dp_offset = buffer_size = NULL;
		rap_flag = 0;
		mp4_dec = NULL;
		es->is_synced = 0;
		es->prog_idx = -1;
		si = NULL;

		junk = priv->ts.packet_size - TS_PACKET_SIZE;
		buf_size = priv->ts.packet_size - junk;

		if(stream_eof(stream))
		{
			if(! probe)
			{
				ts_dump_streams(priv);
				demuxer->filepos = stream_tell(demuxer->stream);
			}

			return 0;
		}


		if(! ts_sync(stream))
		{
			mp_msg(MSGT_DEMUX, MSGL_INFO, "TS_PARSE: COULDN'T SYNC\n");
			return 0;
		}

		len = stream_read(stream, &packet[1], 3);
		if (len != 3)
			return 0;
		buf_size -= 4;

		if((packet[1]  >> 7) & 0x01)	//transport error
			ts_error = 1;


		is_start = packet[1] & 0x40;
		pid = ((packet[1] & 0x1f) << 8) | packet[2];

		tss = priv->ts.pids[pid];			//an ES stream
		if(tss == NULL)
		{
			tss = new_pid(priv, pid);
			if(tss == NULL)
				continue;
		}

		cc = (packet[3] & 0xf);
		// CC increments only when a payload is present
		cc_ok = (tss->last_cc < 0) || (((tss->last_cc + !!(packet[3] & 0x10)) & 0x0f) == cc);
		if (!ts_error)
			tss->last_cc = cc;

		bad = ts_error || (! cc_ok && pid != 0x1fff);
		if(bad)
		{
			mp_msg(MSGT_DEMUX, MSGL_STATUS, "TS_PARSE: packet lost in 0x%04x E:%d\n", pid, ts_error);
			if(priv->keep_broken == 0)
			{
				stream_skip(stream, buf_size-1+junk);
				continue;
			}

			is_start = 0;	//queued to the packet data
		}


		afc = (packet[3] >> 4) & 3;

		if(afc > 1)
		{
			int c;
			c = stream_read_char(stream);
			buf_size--;
			if(c < 0 || c > 183)	//broken from the stream layer or invalid
			{
				stream_skip(stream, buf_size-1+junk);
				continue;
			}

			//c==0 is allowed!
			if(c > 0)
			{
				uint8_t pcrbuf[188];
				int flags = stream_read_char(stream);
				int has_pcr;
				rap_flag = (flags & 0x40) >> 6;
				has_pcr = flags & 0x10;

				buf_size--;
				c--;
				stream_read(stream, pcrbuf, c);

				if(has_pcr && !probe)
				{
					int pcr_pid = prog_pcr_pid(priv, priv->prog);
					if(pcr_pid == pid)
					{
						uint64_t pcr, pcr_ext;
						double new;

						pcr  = (int64_t)(pcrbuf[0]) << 25;
						pcr |=  pcrbuf[1]         << 17 ;
						pcr |= (pcrbuf[2]) << 9;
						pcr |=  pcrbuf[3]  <<  1 ;
						pcr |= (pcrbuf[4] & 0x80) >>  7;

						pcr_ext = (pcrbuf[4] & 0x01) << 8;
						pcr_ext |= pcrbuf[5];

						pcr = pcr * 300 + pcr_ext;

						new = (double)pcr/(double)27000000.0;
						if (demuxer->reference_clock == MP_NOPTS_VALUE)
							demuxer->reference_clock = new;
						else if (check_discon(demuxer, new))
							reset_on_discon(demuxer);
					}
				}

				buf_size -= c;
				if(buf_size == 0)
					continue;
			}
		}

		// moved down here to allow PCR streams without any payloads
		if(! (afc % 2))	//no payload in this TS packet
		{
			stream_skip(stream, buf_size-1+junk);
			continue;
		}

		if(is_start)
			tss->is_synced = 1;

		if((!is_start && !tss->is_synced) || ((pid > 1) && (pid < 16)) || (pid == 8191))		//invalid pid
		{
			stream_skip(stream, buf_size-1+junk);
			continue;
		}

		//find the program that the pid belongs to; if (it's the right one or -1) && pid_type==SL_SECTION
		//call parse_sl_section()
		pmt = pmt_of_pid(priv, pid, &mp4_dec);
		if(mp4_dec)
		{
			fill_extradata(mp4_dec, tss);
			if(IS_VIDEO(mp4_dec->object_type) || IS_AUDIO(mp4_dec->object_type))
			{
				tss->type = SL_PES_STREAM;
				tss->subtype = mp4_dec->object_type;
			}
		}


		//TABLE PARSING

		base = priv->ts.packet_size - buf_size;

		priv->last_pid = pid;

		is_video = IS_VIDEO(tss->type) || (tss->type==SL_PES_STREAM && IS_VIDEO(tss->subtype));
		is_audio = IS_AUDIO(tss->type) || (tss->type==SL_PES_STREAM && IS_AUDIO(tss->subtype)) || (tss->type == PES_PRIVATE1);
		is_sub	= IS_SUB(tss->type);
		pid_type = pid_type_from_pmt(priv, pid);

			// PES CONTENT STARTS HERE
		if(! probe)
		{
			if((is_video || is_audio || is_sub) && is_start)
				ts_add_stream(demuxer, tss);

			if(is_video && (demuxer->video->id == priv->ts.streams[pid].id))
			{
				ds = demuxer->video;

				dp = &priv->fifo[1].pack;
				dp_offset = &priv->fifo[1].offset;
				buffer_size = &priv->fifo[1].buffer_size;
				si = &priv->vstr;
			}
			else if(is_audio && (demuxer->audio->id == priv->ts.streams[pid].id))
			{
				ds = demuxer->audio;

				dp = &priv->fifo[0].pack;
				dp_offset = &priv->fifo[0].offset;
				buffer_size = &priv->fifo[0].buffer_size;
				si = &priv->astr;
			}
			else if(is_sub)
			{
				sh_sub_t *sh_sub = demuxer->sub->sh;

				if(sh_sub && sh_sub->sid == tss->pid)
				{
					ds = demuxer->sub;

					dp = &priv->fifo[2].pack;
					dp_offset = &priv->fifo[2].offset;
					buffer_size = &priv->fifo[2].buffer_size;
				}
				else
				{
					stream_skip(stream, buf_size+junk);
					continue;
				}
			}

			//IS IT TIME TO QUEUE DATA to the dp_packet?
			if(is_start && (dp != NULL))
			{
				retv = fill_packet(demuxer, ds, dp, dp_offset, si);
			}


			if(dp && *dp == NULL)
			{
				if(*buffer_size > MAX_PACK_BYTES)
					*buffer_size = MAX_PACK_BYTES;
				*dp = new_demux_packet(*buffer_size);	//es->size
				*dp_offset = 0;
				if(! *dp)
				{
					fprintf(stderr, "fill_buffer, NEW_ADD_PACKET(%d)FAILED\n", *buffer_size);
					continue;
				}
				mp_msg(MSGT_DEMUX, MSGL_DBG2, "CREATED DP(%d)\n", *buffer_size);
			}
		}


		if(probe || !dp)	//dp is NULL for tables and sections
		{
			p = &packet[base];
		}
		else	//feeding
		{
			if(*dp_offset + buf_size > *buffer_size)
			{
				*buffer_size = *dp_offset + buf_size + TS_FEC_PACKET_SIZE;
				resize_demux_packet(*dp, *buffer_size);
			}
			p = &((*dp)->buffer[*dp_offset]);
		}

		len = stream_read(stream, p, buf_size);
		if(len < buf_size)
		{
			mp_msg(MSGT_DEMUX, MSGL_DBG2,  "\r\nts_parse() couldn't read enough data: %d < %d\r\n", len, buf_size);
			continue;
		}
		stream_skip(stream, junk);

		// currently SI streams (dp==NULL) are not scrambled in ISDB-T/S.
		scrambled = (packet[3] & 0xC0);
#if CONFIG_DEMULTI2
		if (dp && scrambled && priv->dm2_handle && tss->ecm_pid < 8191)
		{
			int ret;
			ret = demulti2_descramble(priv->dm2_handle, p, buf_size,
					scrambled, tss->ecm_pid, NULL);
			if (ret == DEMULTI2_RET_OK) {
				packet[3] &= 0x3F;
				scrambled = 0;
			}
			mp_msg(MSGT_DEMUX, MSGL_DBG2, "descrambled. ret:%d\n", ret);
		}
#endif

		if(pid  == 0)
		{
			parse_pat(priv, is_start, p, buf_size);
			if (!probe && priv->prog > 0 && prog_idx_in_pat(priv, priv->prog) < 0)
			{
				mp_msg(MSGT_DEMUX, MSGL_INFO, "PAT changed and PROG:%" PRIu16
				       " disappeared.\n", priv->prog);
				if (priv->pat.progs) {
					priv->prog = priv->pat.progs[0].id;
					priv->pcr_delta = 0.0;
					mp_msg(MSGT_DEMUX, MSGL_INFO, "switching to the PROG:%" PRIu16 "\n", priv->prog);
				}
			}
			continue;
		}
/*
		else if (pid == 1)
		{
			parse_cat(priv, is_start, p, buf_size);
			continue;
		}
		else if (pid == priv->emm.pid)
		{
			parse_emm(priv, is_start, p, buf_size);
			continue;
		}
 */
		else if (priv->ts.ecms[pid] != NULL)
		{
			parse_ecm(priv, pid, is_start, p, buf_size);
			continue;
		}
		else if (pid == 0x12)
		{
			parse_eit(priv, is_start, p, buf_size);
			while (next_section(&priv->eit_section) > 0)
				parse_eit(priv, 0, p, 0);
			continue;
		}
		else if((tss->type == SL_SECTION) && pmt)
		{
			int k, mp4_es_id = -1;
			ts_section_t *section;
			for(k = 0; k < pmt->mp4es_cnt; k++)
			{
				if(pmt->mp4es[k].decoder.object_type == MP4_OD && pmt->mp4es[k].decoder.stream_type == MP4_OD)
					mp4_es_id = pmt->mp4es[k].id;
			}
			mp_msg(MSGT_DEMUX, MSGL_DBG2, "MP4ESID: %d\n", mp4_es_id);
			for(k = 0; k < pmt->es_cnt; k++)
			{
				if(pmt->es[k].mp4_es_id == mp4_es_id)
				{
					section = &(tss->section);
					parse_sl_section(pmt, section, is_start, &packet[base], buf_size);
				}
			}
			continue;
		}
		else
		{
			progid = prog_id_in_pat(priv, pid);
			if(progid != -1)
			{
				if(pid != demuxer->video->id && pid != demuxer->audio->id && pid != demuxer->sub->id)
				{
					int ret;

					ret = parse_pmt(priv, progid, pid, is_start, &packet[base], buf_size);
					if(!probe && progid == (signed) priv->prog && ret > 0)
						reselect_streams(demuxer);
					continue;
				}
				else
					mp_msg(MSGT_DEMUX, MSGL_ERR, "Argh! Data pid %d used in the PMT, Skipping PMT parsing!\n", pid);
			}
		}

		if(!probe && !dp)
			continue;

		if (scrambled)
		{
			if (probe)
			{
				es->pid = tss->pid;
				es->type = tss->type;
				es->subtype = tss->subtype;
				es->component_tag = tss->component_tag;
				es->is_dualmono = tss->is_dualmono;
				memcpy(es->lang, tss->lang, 4);
				memcpy(es->lang2, tss->lang2, 4);
				es->prog_idx = tss->prog_idx;
				es->es_idx = tss->es_idx;
				return -1;
			}
			else
			{
				continue;
			}
		}

		if(is_start)
		{
			mp_msg(MSGT_DEMUX, MSGL_DBG2, "IS_START\n");

			len = pes_parse2(p, buf_size, es, pid_type, pmt, pid);
			if(! len)
			{
				tss->is_synced = 0;
				continue;
			}
			es->pid = tss->pid;
			es->component_tag = tss->component_tag;
			es->is_dualmono = tss->is_dualmono;
			memcpy(es->lang, tss->lang, 4);
			memcpy(es->lang2, tss->lang2, 4);
			es->prog_idx = tss->prog_idx;
			es->es_idx = tss->es_idx;
			tss->is_synced |= es->is_synced || rap_flag;
			tss->payload_size = es->payload_size;

			if(probe)
			{
				if(es->type == UNKNOWN)
					return 0;

				tss->type = es->type;
				tss->subtype = es->subtype;

				return 1;
			}
			else
			{
				if(es->pts == 0.0)
					es->pts = tss->pts = tss->last_pts;
				else
					tss->pts = tss->last_pts = es->pts;

				mp_msg(MSGT_DEMUX, MSGL_DBG2, "ts_parse, NEW pid=%d, PSIZE: %u, type=%X, start=%p, len=%d\n",
					es->pid, es->payload_size, es->type, es->start, es->size);

				demuxer->filepos = stream_tell(demuxer->stream) - es->size;

				if(es->size < 0 || es->size > buf_size) {
					mp_msg(MSGT_DEMUX, MSGL_ERR, "Broken ES packet size\n");
					es->size = 0;
				}
				memmove(p, es->start, es->size);
				*dp_offset += es->size;
				(*dp)->flags = 0;
				(*dp)->pos = stream_tell(demuxer->stream);
				(*dp)->pts = es->pts;
				// subtitle packets must be returned immediately if possible
				if (is_sub && !tss->payload_size)
					retv = fill_packet(demuxer, ds, dp, dp_offset, si);

				if(retv > 0)
					return retv;
				else
					continue;
			}
		}
		else
		{
			uint16_t sz;

			es->pid = tss->pid;
			es->type = tss->type;
			es->subtype = tss->subtype;
			es->pts = tss->pts = tss->last_pts;
			es->start = &packet[base];
			es->component_tag = tss->component_tag;
			es->is_dualmono = tss->is_dualmono;
			memcpy(es->lang, tss->lang, 4);
			memcpy(es->lang2, tss->lang2, 4);
			es->prog_idx = tss->prog_idx;
			es->es_idx = tss->es_idx;

			if(tss->payload_size > 0)
			{
				sz = FFMIN(tss->payload_size, buf_size);
				tss->payload_size -= sz;
				es->size = sz;
			}
			else
			{
				if(is_video)
				{
					sz = es->size = buf_size;
				}
				else
				{
					continue;
				}
			}


			if(! probe)
			{
				*dp_offset += sz;

				// subtitle packets must be returned immediately if possible
				if(*dp_offset >= MAX_PACK_BYTES || (is_sub && !tss->payload_size))
				{
					(*dp)->pts = tss->last_pts;
					retv = fill_packet(demuxer, ds, dp, dp_offset, si);
					return 1;
				}

				continue;
			}
			else
			{
				memmove(es->start, p, sz);

				if(es->size)
					return es->size;
				else
					continue;
			}
		}
	}

	return 0;
}


static void reset_fifos(demuxer_t *demuxer, int a, int v, int s)
{
	ts_priv_t* priv = demuxer->priv;
	if(a)
	{
		if(priv->fifo[0].pack != NULL)
		{
			free_demux_packet(priv->fifo[0].pack);
			priv->fifo[0].pack = NULL;
		}
		priv->fifo[0].offset = 0;
	}

	if(v)
	{
		if(priv->fifo[1].pack != NULL)
		{
			free_demux_packet(priv->fifo[1].pack);
			priv->fifo[1].pack = NULL;
		}
		priv->fifo[1].offset = 0;
	}

	if(s)
	{
		if(priv->fifo[2].pack != NULL)
		{
			free_demux_packet(priv->fifo[2].pack);
			priv->fifo[2].pack = NULL;
		}
		priv->fifo[2].offset = 0;
	}
	demuxer->reference_clock = MP_NOPTS_VALUE;
	priv->pcr_delta = 0.0;
}


static void demux_seek_ts(demuxer_t *demuxer, float rel_seek_secs, float audio_delay, int flags)
{
	demux_stream_t *d_audio=demuxer->audio;
	demux_stream_t *d_video=demuxer->video;
	sh_audio_t *sh_audio=d_audio->sh;
	sh_video_t *sh_video=d_video->sh;
	ts_priv_t * priv = (ts_priv_t*) demuxer->priv;
	int i, video_stats;
	off_t newpos;

	//================= seek in MPEG-TS ==========================

	ts_dump_streams(demuxer->priv);
	reset_fifos(demuxer, sh_audio != NULL, sh_video != NULL, demuxer->sub->id > 0);

	demux_flush(demuxer);



	video_stats = (sh_video != NULL);
	if(video_stats)
	{
		mp_msg(MSGT_DEMUX, MSGL_V, "IBPS: %d, vb: %d\r\n", sh_video->i_bps, priv->vbitrate);
		if(priv->vbitrate)
			video_stats = priv->vbitrate;
		else
			video_stats = sh_video->i_bps;
	}

	newpos = (flags & SEEK_ABSOLUTE) ? demuxer->movi_start : demuxer->filepos;
	if(flags & SEEK_FACTOR) // float seek 0..1
		newpos+=(demuxer->movi_end-demuxer->movi_start)*rel_seek_secs;
	else
	{
		// time seek (secs)
		if(! video_stats) // unspecified or VBR
			newpos += 2324*75*rel_seek_secs; // 174.3 kbyte/sec
		else
			newpos += video_stats*rel_seek_secs;
	}


	if(newpos < demuxer->movi_start)
  		newpos = demuxer->movi_start;	//begininng of stream

	stream_seek(demuxer->stream, newpos);
	for(i = 0; i < NB_PID_MAX; i++)
		if(priv->ts.pids[i] != NULL)
			priv->ts.pids[i]->is_synced = 0;

	videobuf_code_len = 0;

	if(sh_video != NULL)
		ds_fill_buffer(d_video);

	if(sh_audio != NULL)
	{
		ds_fill_buffer(d_audio);
	}

	while(sh_video != NULL)
	{
		if(sh_audio && !d_audio->eof && d_video->pts && d_audio->pts)
		{
			double a_pts=d_audio->pts;
			a_pts+=(ds_tell_pts(d_audio)-sh_audio->a_in_buffer_len)/(double)sh_audio->i_bps;
			if(d_video->pts > a_pts)
			{
				skip_audio_frame(sh_audio);  // sync audio
				continue;
			}
		}


		i = sync_video_packet(d_video);
		if((sh_video->format == VIDEO_MPEG1) || (sh_video->format == VIDEO_MPEG2))
		{
			if(i==0x1B3 || i==0x1B8) break; // found it!
		}
		else if((sh_video->format == VIDEO_MPEG4) && (i==0x1B6))
			break;
		else if(sh_video->format == VIDEO_VC1 && (i==0x10E || i==0x10F))
			break;
		else if(sh_video->format == VIDEO_HEVC)
			break;
		else	//H264
		{
			if((i & ~0x60) == 0x105 || (i & ~0x60) == 0x107) break;
		}

		if(!i || !skip_video_packet(d_video)) break; // EOF?
	}
}


static int demux_ts_fill_buffer(demuxer_t * demuxer, demux_stream_t *ds)
{
	ES_stream_t es;
	ts_priv_t *priv = (ts_priv_t *)demuxer->priv;

	return -ts_parse(demuxer, &es, priv->packet, 0);
}


static int ts_check_file_dmx(demuxer_t *demuxer)
{
    return ts_check_file(demuxer) ? DEMUXER_TYPE_MPEG_TS : 0;
}

static int is_usable_program(ts_priv_t *priv, pmt_t *pmt)
{
	int j;

	for(j = 0; j < pmt->es_cnt; j++)
	{
		if(priv->ts.pids[pmt->es[j].pid] == NULL || priv->ts.streams[pmt->es[j].pid].sh == NULL)
			continue;
		if(
			priv->ts.streams[pmt->es[j].pid].type == TYPE_VIDEO ||
			priv->ts.streams[pmt->es[j].pid].type == TYPE_AUDIO
		)
			return 1;
	}

	return 0;
}

static void
select_pes_in_pmt(demuxer_t *demuxer, pmt_t *pmt, int lowcn, demux_program_t *prog)
{
	int j;
	int orig_vid, orig_sid, orig_aid;
	int aidx, vidx, sidx;

	orig_vid = (demuxer->video && demuxer->video->sh) ?
			((sh_video_t *) demuxer->video->sh)->vid : 0;
	orig_sid = (demuxer->sub && demuxer->sub->sh) ?
			((sh_sub_t *) demuxer->sub->sh)->sid : 0;
	orig_aid = (demuxer->audio && demuxer->audio->sh) ?
			((sh_audio_t *) demuxer->audio->sh)->aid : 0;

	prog->aid = prog->vid = prog->sid = -2;	//no choice by default
	aidx = vidx = sidx = -1;
	for(j = 0; j < pmt->es_cnt; j++)
	{
		if (!lowcn && pmt->es[j].highCN_pid != 0)
			continue;

		if (IS_VIDEO(pmt->es[j].type) && video_id != -2) {
			if (vidx != -1 && lowcn &&
			    pmt->es[vidx].highCN_pid != 0 && pmt->es[j].highCN_pid == 0)
				continue;
			if (vidx != -1 &&
			    (!lowcn || pmt->es[vidx].highCN_pid != 0) &&
			    pmt->es[vidx].pid == orig_vid)
				continue;

			if (vidx == -1 ||
			    (lowcn &&
			     !pmt->es[vidx].highCN_pid && !!pmt->es[j].highCN_pid) ||
			    pmt->es[j].component_tag < pmt->es[vidx].component_tag) {
				vidx = j;
				prog->vid = pmt->es[j].pid;
			}
		} else if (IS_SUB(pmt->es[j].type) && dvdsub_id != -2) {
			if (sidx != -1 && lowcn &&
			    pmt->es[sidx].highCN_pid != 0 && pmt->es[j].highCN_pid == 0)
				continue;
			if (sidx != -1 && dvdsub_lang &&
			    pes_match_lang(&pmt->es[sidx], dvdsub_lang) &&
			    !pes_match_lang(&pmt->es[j], dvdsub_lang))
				continue;
			if (sidx != -1 &&
			    (!lowcn || pmt->es[sidx].highCN_pid != 0) &&
			    (!dvdsub_lang || !dvdsub_lang[0] ||
			         pes_match_lang(&pmt->es[sidx], dvdsub_lang)) &&
			    pmt->es[sidx].pid == orig_sid)
				continue;

			if (sidx == -1 ||
			    (lowcn && !pmt->es[sidx].highCN_pid && !!pmt->es[j].highCN_pid) ||
			    (dvdsub_lang &&
			        !pes_match_lang(&pmt->es[sidx], dvdsub_lang) &&
			         pes_match_lang(&pmt->es[j], dvdsub_lang)) ||
			    pmt->es[j].component_tag < pmt->es[sidx].component_tag) {
				sidx = j;
				prog->sid = pmt->es[j].pid;
			}
		} else if (IS_AUDIO(pmt->es[j].type) && audio_id != -2) {
			if (aidx != -1 && lowcn &&
			    pmt->es[aidx].highCN_pid != 0 && pmt->es[j].highCN_pid == 0)
				continue;
			if (aidx != -1 && audio_lang &&
			    pes_match_lang(&pmt->es[aidx], audio_lang) &&
			    !pes_match_lang(&pmt->es[j], audio_lang))
				continue;
			if (aidx != -1 &&
			    (!lowcn || pmt->es[aidx].highCN_pid != 0) &&
			    (!audio_lang || !audio_lang[0] ||
			         pes_match_lang(&pmt->es[aidx], audio_lang)) &&
			    pmt->es[aidx].pid == orig_aid)
				continue;

			if (aidx == -1 ||
			    (lowcn && !pmt->es[aidx].highCN_pid && !!pmt->es[j].highCN_pid) ||
			    (audio_lang &&
			        !pes_match_lang(&pmt->es[aidx], audio_lang) &&
			         pes_match_lang(&pmt->es[j], audio_lang)) ||
			    pmt->es[j].component_tag < pmt->es[aidx].component_tag) {
				aidx = j;
				prog->aid = pmt->es[j].pid;
			}
		}
	}
}

static int demux_ts_control(demuxer_t *demuxer, int cmd, void *arg)
{
	ts_priv_t* priv = (ts_priv_t *)demuxer->priv;
        pmt_t* pmt;
        mp4_decoder_config_t *dummy;

	switch(cmd)
	{
		case DEMUXER_CTRL_SWITCH_AUDIO:
		case DEMUXER_CTRL_SWITCH_VIDEO:
		{
			void *sh = NULL;
			int i = 8192, n;
			int reftype, areset = 0, vreset = 0;
			demux_stream_t *ds;
			int id_max;
			sh_common_t **streams;
			int dmode = 0;

			if(cmd == DEMUXER_CTRL_SWITCH_VIDEO)
			{
				reftype = TYPE_VIDEO;
				ds = demuxer->video;
				vreset  = 1;
				id_max = priv->last_vid;
				streams = (sh_common_t **)demuxer->v_streams;
				if (ds && ds->sh)
					i = ((sh_video_t *) ds->sh)->vid;
			}
			else
			{
				reftype = TYPE_AUDIO;
				ds = demuxer->audio;
				areset = 1;
				id_max = priv->last_aid;
				streams = (sh_common_t **)demuxer->a_streams;
				if (ds && ds->sh)
					i = ((sh_audio_t *) ds->sh)->aid;
			}
			n = *((int*)arg);
			if(n == -2)
			{
				reset_fifos(demuxer, areset, vreset, 0);
				ds->id = -2;
				ds->sh = NULL;
				ds_free_packs(ds);
				*((int*)arg) = ds->id;
				return DEMUXER_CTRL_OK;
			}

			if(n < 0)
			{
				// firstly, search from PMT
				pmt = pmt_of_pid (priv, i, &dummy);
				if (i < 8192 && pmt && pmt->es_cnt > 0)
				{
					int k, es_idx;

					es_idx = priv->ts.pids[i]->es_idx;
					// dmono, main->sub switch
					if (reftype == TYPE_AUDIO &&
					    priv->ts.pids[i] &&
					    priv->ts.pids[i]->is_dualmono &&
					    ((sh_audio_t *) ds->sh)->dualmono_mode == 0) {
						sh = priv->ts.streams[i].sh;
						dmode = 1;
						goto finish;
					}

					k = (es_idx + 1) % pmt->es_cnt;
					while(k != es_idx)
					{
						if(priv->ts.streams[pmt->es[k].pid].type == reftype &&
						   (pmt->es[k].highCN_pid == 0) ^ low_cn)
						{
							i = pmt->es[k].pid;
							break;
						}
						k = (k + 1) % pmt->es_cnt;
					}
					sh = priv->ts.streams[i].sh;
				} else {
					if (i >= 8192 || ds == NULL || ds->id < 0)
						return DEMUXER_CTRL_NOTIMPL;
					sh = streams[(ds->id + 1) % id_max];
				}
			}
			else	//audio track <n>
			{
				if (n >= 8192 || priv->ts.streams[n].type != reftype) return DEMUXER_CTRL_NOTIMPL;
				i = n;
				sh = priv->ts.streams[i].sh;
				if (reftype == TYPE_AUDIO && sh)
					dmode = ((sh_audio_t *) sh)->dualmono_mode;
			}

finish:
			if(sh)
			{
				if(ds->id != priv->ts.streams[i].id)
					reset_fifos(demuxer, areset, vreset, 0);
				ds->id = priv->ts.streams[i].id;
				ds->sh = sh;
				ds_free_packs(ds);
				if (reftype == TYPE_AUDIO)
					((sh_audio_t *)sh)->dualmono_mode = dmode;
				mp_msg(MSGT_DEMUX, MSGL_V, "\r\ndemux_ts, switched to audio pid %d, id: %d, sh: %p\r\n", i, ds->id, sh);
			}

			*((int*)arg) = ds->id;
			return DEMUXER_CTRL_OK;
		}

		case DEMUXER_CTRL_IDENTIFY_PROGRAM:		//returns in prog->{aid,vid} the new ids that comprise a program
		{
			int i, cnt=0;
			pmt_t *pmt = NULL;
			demux_program_t *prog = arg;

			if(priv->pmt_cnt < 2)
				return DEMUXER_CTRL_NOTIMPL;

			if(prog->progid == -1)
			{
				int cur_pmt_idx = 0;

				for(i = 0; i < priv->pmt_cnt; i++)
					if(priv->pmt[i].progid == priv->prog)
					{
						cur_pmt_idx = i;
						break;
					}

				i = (cur_pmt_idx + 1) % priv->pmt_cnt;
				while(i != cur_pmt_idx)
				{
					pmt = &priv->pmt[i];
					cnt = is_usable_program(priv, pmt);
					if(cnt)
						break;
					i = (i + 1) % priv->pmt_cnt;
				}
			}
			else
			{
				for(i = 0; i < priv->pmt_cnt; i++)
					if(priv->pmt[i].progid == prog->progid)
					{
						pmt = &priv->pmt[i]; //required program
						cnt = is_usable_program(priv, pmt);
					}
			}

			if(!cnt)
				return DEMUXER_CTRL_NOTIMPL;

			//finally some food
			select_pes_in_pmt(demuxer, pmt, low_cn, prog);
			priv->prog = prog->progid = pmt->progid;
			priv->pcr_delta = 0.0;
			return DEMUXER_CTRL_OK;
		}

		case DEMUXER_CTRL_SET_LOWCN:
		{
			int mode;
			int progidx;
			pmt_t *pmt;
			demux_program_t prog;

			mode = *((int *)arg);
			if (mode == low_cn)
				return DEMUXER_CTRL_OK;

			progidx = progid_idx_in_pmt(priv, priv->prog);
			if (progidx < 0)
				return DEMUXER_CTRL_DONTKNOW;

			pmt = &priv->pmt[progidx];
			select_pes_in_pmt(demuxer, pmt, mode, &prog);
			if (prog.vid == -2 && prog.aid == -2)
				return DEMUXER_CTRL_NOTIMPL;

			if (prog.vid !=-2 &&
			    !(demuxer->video && demuxer->video->sh &&
			      ((sh_video_t *) demuxer->video->sh)->vid == prog.vid))
				demuxer_switch_video(demuxer, prog.vid);
			if (prog.aid != -2 &&
			    !(demuxer->audio && demuxer->audio->sh &&
			      ((sh_audio_t *) demuxer->audio->sh)->aid == prog.aid))
				demuxer_switch_audio(demuxer, prog.aid);
			low_cn = mode;
			return DEMUXER_CTRL_OK;
		}

		default:
			return DEMUXER_CTRL_NOTIMPL;
	}
}


const demuxer_desc_t demuxer_desc_mpeg_ts = {
  "MPEG-TS demuxer",
  "mpegts",
  "TS",
  "Nico Sabbi",
  "",
  DEMUXER_TYPE_MPEG_TS,
  0, // unsafe autodetect
  ts_check_file_dmx,
  demux_ts_fill_buffer,
  demux_open_ts,
  demux_close_ts,
  demux_seek_ts,
  demux_ts_control
};

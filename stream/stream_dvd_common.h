#ifndef MPLAYER_STREAM_DVD_COMMON_H
#define MPLAYER_STREAM_DVD_COMMON_H

#include "config.h"
#include <inttypes.h>
#ifdef CONFIG_DVDREAD_INTERNAL
#include "libdvdread/ifo_types.h"
#else
#include <dvdread/ifo_types.h>
#endif

int mp_dvdtimetomsec(dvd_time_t *dt);

#endif /* MPLAYER_STREAM_DVD_COMMON_H */

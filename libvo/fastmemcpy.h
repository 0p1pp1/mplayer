#ifndef __MPLAYER_MEMCPY
#define __MPLAYER_MEMCPY 1

#include "../config.h"

#ifdef USE_FASTMEMCPY
#if defined(HAVE_MMX) || defined(HAVE_MMX2) || defined(HAVE_3DNOW) \
/*    || defined(HAVE_SSE) || defined(HAVE_SSE2) */
#include <stddef.h>

extern void * fast_memcpy(void * to, const void * from, size_t len);
#define memcpy(a,b,c) fast_memcpy(a,b,c)

#endif /* HAVE_MMX/MMX2/3DNOW/SSE/SSE2 */
#endif /* USE_FASTMEMCPY */
#endif

#ifndef __MPLAYER_MEMCPY
#define __MPLAYER_MEMCPY 1

#include "../config.h"

#ifdef USE_FASTMEMCPY
#if defined(HAVE_MMX) || defined(HAVE_MMX2) || defined(HAVE_3DNOW) \
/*    || defined(HAVE_SSE) || defined(HAVE_SSE2) */
#include <stddef.h>

extern void * fast_memcpy(void * to, const void * from, size_t len);
extern void * mem2agpcpy(void * to, const void * from, size_t len);
#define memcpy(a,b,c) fast_memcpy(a,b,c)

#else /* HAVE_MMX/MMX2/3DNOW/SSE/SSE2 */
#define mem2agpcpy(a,b,c) memcpy(a,b,c)
#endif

#else /* USE_FASTMEMCPY */
#define mem2agpcpy(a,b,c) memcpy(a,b,c)
#endif

static inline void * mem2agpcpy_pic(void * dst, void * src, int bytesPerLine, int height, int dstStride, int srcStride)
{
	int i;
	void *retval=dst;

	if(dstStride == srcStride) mem2agpcpy(dst, src, srcStride*height);
	else
	{
		for(i=0; i<height; i++)
		{
			mem2agpcpy(dst, src, bytesPerLine);
			src+= srcStride;
			dst+= dstStride;
		}
	}

	return retval;
}

static inline void * memcpy_pic(void * dst, void * src, int bytesPerLine, int height, int dstStride, int srcStride)
{
	int i;
	void *retval=dst;

	if(dstStride == srcStride) memcpy(dst, src, srcStride*height);
	else
	{
		for(i=0; i<height; i++)
		{
			memcpy(dst, src, bytesPerLine);
			src+= srcStride;
			dst+= dstStride;
		}
	}

	return retval;
}

#endif

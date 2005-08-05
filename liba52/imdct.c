/*
 * imdct.c
 * Copyright (C) 2000-2001 Michel Lespinasse <walken@zoy.org>
 * Copyright (C) 1999-2000 Aaron Holtzman <aholtzma@ess.engr.uvic.ca>
 *
 * This file is part of a52dec, a free ATSC A-52 stream decoder.
 * See http://liba52.sourceforge.net/ for updates.
 *
 * Modified for use with MPlayer, changes contained in liba52_changes.diff.
 * detailed CVS changelog at http://www.mplayerhq.hu/cgi-bin/cvsweb.cgi/main/
 * $Id$
 *
 * a52dec is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * a52dec is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * SSE optimizations from Michael Niedermayer (michaelni@gmx.at)
 * 3DNOW optimizations from Nick Kurshev <nickols_k@mail.ru>
 *   michael did port them from libac3 (untested, perhaps totally broken)
 * AltiVec optimizations from Romain Dolbeau (romain@dolbeau.org)
 */

#include "config.h"

#include <math.h>
#include <stdio.h>
#ifndef M_PI
#define M_PI 3.1415926535897932384626433832795029
#endif
#include <inttypes.h>

#include "a52.h"
#include "a52_internal.h"
#include "mm_accel.h"
#include "mangle.h"

#ifdef RUNTIME_CPUDETECT
#undef HAVE_3DNOWEX
#endif

#define USE_AC3_C

void (* imdct_256) (sample_t data[], sample_t delay[], sample_t bias);
void (* imdct_512) (sample_t data[], sample_t delay[], sample_t bias);

typedef struct complex_s {
    sample_t real;
    sample_t imag;
} complex_t;

static void fft_128p(complex_t *a);

static const int pm128[128] attribute_used __attribute__((aligned(16))) =
{
	0, 16, 32, 48, 64, 80,  96, 112,  8, 40, 72, 104, 24, 56,  88, 120,
	4, 20, 36, 52, 68, 84, 100, 116, 12, 28, 44,  60, 76, 92, 108, 124,
	2, 18, 34, 50, 66, 82,  98, 114, 10, 42, 74, 106, 26, 58,  90, 122,
	6, 22, 38, 54, 70, 86, 102, 118, 14, 46, 78, 110, 30, 62,  94, 126,
	1, 17, 33, 49, 65, 81,  97, 113,  9, 41, 73, 105, 25, 57,  89, 121,
	5, 21, 37, 53, 69, 85, 101, 117, 13, 29, 45,  61, 77, 93, 109, 125,
	3, 19, 35, 51, 67, 83,  99, 115, 11, 43, 75, 107, 27, 59,  91, 123,
	7, 23, 39, 55, 71, 87, 103, 119, 15, 31, 47,  63, 79, 95, 111, 127
}; 

/* 128 point bit-reverse LUT */
static uint8_t attribute_used bit_reverse_512[] = {
	0x00, 0x40, 0x20, 0x60, 0x10, 0x50, 0x30, 0x70, 
	0x08, 0x48, 0x28, 0x68, 0x18, 0x58, 0x38, 0x78, 
	0x04, 0x44, 0x24, 0x64, 0x14, 0x54, 0x34, 0x74, 
	0x0c, 0x4c, 0x2c, 0x6c, 0x1c, 0x5c, 0x3c, 0x7c, 
	0x02, 0x42, 0x22, 0x62, 0x12, 0x52, 0x32, 0x72, 
	0x0a, 0x4a, 0x2a, 0x6a, 0x1a, 0x5a, 0x3a, 0x7a, 
	0x06, 0x46, 0x26, 0x66, 0x16, 0x56, 0x36, 0x76, 
	0x0e, 0x4e, 0x2e, 0x6e, 0x1e, 0x5e, 0x3e, 0x7e, 
	0x01, 0x41, 0x21, 0x61, 0x11, 0x51, 0x31, 0x71, 
	0x09, 0x49, 0x29, 0x69, 0x19, 0x59, 0x39, 0x79, 
	0x05, 0x45, 0x25, 0x65, 0x15, 0x55, 0x35, 0x75, 
	0x0d, 0x4d, 0x2d, 0x6d, 0x1d, 0x5d, 0x3d, 0x7d, 
	0x03, 0x43, 0x23, 0x63, 0x13, 0x53, 0x33, 0x73, 
	0x0b, 0x4b, 0x2b, 0x6b, 0x1b, 0x5b, 0x3b, 0x7b, 
	0x07, 0x47, 0x27, 0x67, 0x17, 0x57, 0x37, 0x77, 
	0x0f, 0x4f, 0x2f, 0x6f, 0x1f, 0x5f, 0x3f, 0x7f};

static uint8_t bit_reverse_256[] = {
	0x00, 0x20, 0x10, 0x30, 0x08, 0x28, 0x18, 0x38, 
	0x04, 0x24, 0x14, 0x34, 0x0c, 0x2c, 0x1c, 0x3c, 
	0x02, 0x22, 0x12, 0x32, 0x0a, 0x2a, 0x1a, 0x3a, 
	0x06, 0x26, 0x16, 0x36, 0x0e, 0x2e, 0x1e, 0x3e, 
	0x01, 0x21, 0x11, 0x31, 0x09, 0x29, 0x19, 0x39, 
	0x05, 0x25, 0x15, 0x35, 0x0d, 0x2d, 0x1d, 0x3d, 
	0x03, 0x23, 0x13, 0x33, 0x0b, 0x2b, 0x1b, 0x3b, 
	0x07, 0x27, 0x17, 0x37, 0x0f, 0x2f, 0x1f, 0x3f};

#if defined(ARCH_X86) || defined(ARCH_X86_64)
// NOTE: SSE needs 16byte alignment or it will segfault 
// 
static complex_t __attribute__((aligned(16))) buf[128];
static float __attribute__((aligned(16))) sseSinCos1c[256];
static float __attribute__((aligned(16))) sseSinCos1d[256];
static float attribute_used __attribute__((aligned(16))) ps111_1[4]={1,1,1,-1};
//static float __attribute__((aligned(16))) sseW0[4];
static float __attribute__((aligned(16))) sseW1[8];
static float __attribute__((aligned(16))) sseW2[16];
static float __attribute__((aligned(16))) sseW3[32];
static float __attribute__((aligned(16))) sseW4[64];
static float __attribute__((aligned(16))) sseW5[128];
static float __attribute__((aligned(16))) sseW6[256];
static float __attribute__((aligned(16))) *sseW[7]=
	{NULL /*sseW0*/,sseW1,sseW2,sseW3,sseW4,sseW5,sseW6};
static float __attribute__((aligned(16))) sseWindow[512];
#else
static complex_t  __attribute__((aligned(16))) buf[128];
#endif

/* Twiddle factor LUT */
static complex_t __attribute__((aligned(16))) w_1[1];
static complex_t __attribute__((aligned(16))) w_2[2];
static complex_t __attribute__((aligned(16))) w_4[4];
static complex_t __attribute__((aligned(16))) w_8[8];
static complex_t __attribute__((aligned(16))) w_16[16];
static complex_t __attribute__((aligned(16))) w_32[32];
static complex_t __attribute__((aligned(16))) w_64[64];
static complex_t __attribute__((aligned(16))) * w[7] = {w_1, w_2, w_4, w_8, w_16, w_32, w_64};

/* Twiddle factors for IMDCT */
static sample_t __attribute__((aligned(16))) xcos1[128];
static sample_t __attribute__((aligned(16))) xsin1[128];
static sample_t __attribute__((aligned(16))) xcos2[64];
static sample_t __attribute__((aligned(16))) xsin2[64];

/* Windowing function for Modified DCT - Thank you acroread */
sample_t imdct_window[] = {
	0.00014, 0.00024, 0.00037, 0.00051, 0.00067, 0.00086, 0.00107, 0.00130,
	0.00157, 0.00187, 0.00220, 0.00256, 0.00297, 0.00341, 0.00390, 0.00443,
	0.00501, 0.00564, 0.00632, 0.00706, 0.00785, 0.00871, 0.00962, 0.01061,
	0.01166, 0.01279, 0.01399, 0.01526, 0.01662, 0.01806, 0.01959, 0.02121,
	0.02292, 0.02472, 0.02662, 0.02863, 0.03073, 0.03294, 0.03527, 0.03770,
	0.04025, 0.04292, 0.04571, 0.04862, 0.05165, 0.05481, 0.05810, 0.06153,
	0.06508, 0.06878, 0.07261, 0.07658, 0.08069, 0.08495, 0.08935, 0.09389,
	0.09859, 0.10343, 0.10842, 0.11356, 0.11885, 0.12429, 0.12988, 0.13563,
	0.14152, 0.14757, 0.15376, 0.16011, 0.16661, 0.17325, 0.18005, 0.18699,
	0.19407, 0.20130, 0.20867, 0.21618, 0.22382, 0.23161, 0.23952, 0.24757,
	0.25574, 0.26404, 0.27246, 0.28100, 0.28965, 0.29841, 0.30729, 0.31626,
	0.32533, 0.33450, 0.34376, 0.35311, 0.36253, 0.37204, 0.38161, 0.39126,
	0.40096, 0.41072, 0.42054, 0.43040, 0.44030, 0.45023, 0.46020, 0.47019,
	0.48020, 0.49022, 0.50025, 0.51028, 0.52031, 0.53033, 0.54033, 0.55031,
	0.56026, 0.57019, 0.58007, 0.58991, 0.59970, 0.60944, 0.61912, 0.62873,
	0.63827, 0.64774, 0.65713, 0.66643, 0.67564, 0.68476, 0.69377, 0.70269,
	0.71150, 0.72019, 0.72877, 0.73723, 0.74557, 0.75378, 0.76186, 0.76981,
	0.77762, 0.78530, 0.79283, 0.80022, 0.80747, 0.81457, 0.82151, 0.82831,
	0.83496, 0.84145, 0.84779, 0.85398, 0.86001, 0.86588, 0.87160, 0.87716,
	0.88257, 0.88782, 0.89291, 0.89785, 0.90264, 0.90728, 0.91176, 0.91610,
	0.92028, 0.92432, 0.92822, 0.93197, 0.93558, 0.93906, 0.94240, 0.94560,
	0.94867, 0.95162, 0.95444, 0.95713, 0.95971, 0.96217, 0.96451, 0.96674,
	0.96887, 0.97089, 0.97281, 0.97463, 0.97635, 0.97799, 0.97953, 0.98099,
	0.98236, 0.98366, 0.98488, 0.98602, 0.98710, 0.98811, 0.98905, 0.98994,
	0.99076, 0.99153, 0.99225, 0.99291, 0.99353, 0.99411, 0.99464, 0.99513,
	0.99558, 0.99600, 0.99639, 0.99674, 0.99706, 0.99736, 0.99763, 0.99788,
	0.99811, 0.99831, 0.99850, 0.99867, 0.99882, 0.99895, 0.99908, 0.99919,
	0.99929, 0.99938, 0.99946, 0.99953, 0.99959, 0.99965, 0.99969, 0.99974,
	0.99978, 0.99981, 0.99984, 0.99986, 0.99988, 0.99990, 0.99992, 0.99993,
	0.99994, 0.99995, 0.99996, 0.99997, 0.99998, 0.99998, 0.99998, 0.99999,
	0.99999, 0.99999, 0.99999, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000,
	1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000 };


static inline void swap_cmplx(complex_t *a, complex_t *b)
{
    complex_t tmp;

    tmp = *a;
    *a = *b;
    *b = tmp;
}



static inline complex_t cmplx_mult(complex_t a, complex_t b)
{
    complex_t ret;

    ret.real = a.real * b.real - a.imag * b.imag;
    ret.imag = a.real * b.imag + a.imag * b.real;

    return ret;
}

void
imdct_do_512(sample_t data[],sample_t delay[], sample_t bias)
{
    int i;
#ifndef USE_AC3_C
	int k;
    int p,q;
    int m;
    int two_m;
    int two_m_plus_one;

    sample_t tmp_b_i;
    sample_t tmp_b_r;
#endif
    sample_t tmp_a_i;
    sample_t tmp_a_r;

    sample_t *data_ptr;
    sample_t *delay_ptr;
    sample_t *window_ptr;
	
    /* 512 IMDCT with source and dest data in 'data' */
	
    /* Pre IFFT complex multiply plus IFFT cmplx conjugate & reordering*/
    for( i=0; i < 128; i++) {
	/* z[i] = (X[256-2*i-1] + j * X[2*i]) * (xcos1[i] + j * xsin1[i]) ; */ 
#ifdef USE_AC3_C
	int j= pm128[i];
#else
	int j= bit_reverse_512[i];
#endif	
	buf[i].real =         (data[256-2*j-1] * xcos1[j])  -  (data[2*j]       * xsin1[j]);
	buf[i].imag = -1.0 * ((data[2*j]       * xcos1[j])  +  (data[256-2*j-1] * xsin1[j]));
    }

    /* FFT Merge */
/* unoptimized variant
    for (m=1; m < 7; m++) {
	if(m)
	    two_m = (1 << m);
	else
	    two_m = 1;

	two_m_plus_one = (1 << (m+1));

	for(i = 0; i < 128; i += two_m_plus_one) {
	    for(k = 0; k < two_m; k++) {
		p = k + i;
		q = p + two_m;
		tmp_a_r = buf[p].real;
		tmp_a_i = buf[p].imag;
		tmp_b_r = buf[q].real * w[m][k].real - buf[q].imag * w[m][k].imag;
		tmp_b_i = buf[q].imag * w[m][k].real + buf[q].real * w[m][k].imag;
		buf[p].real = tmp_a_r + tmp_b_r;
		buf[p].imag =  tmp_a_i + tmp_b_i;
		buf[q].real = tmp_a_r - tmp_b_r;
		buf[q].imag =  tmp_a_i - tmp_b_i;
	    }
	}
    }
*/
#ifdef USE_AC3_C
	fft_128p (&buf[0]);
#else

    /* 1. iteration */
    for(i = 0; i < 128; i += 2) {
	tmp_a_r = buf[i].real;
	tmp_a_i = buf[i].imag;
	tmp_b_r = buf[i+1].real;
	tmp_b_i = buf[i+1].imag;
	buf[i].real = tmp_a_r + tmp_b_r;
	buf[i].imag =  tmp_a_i + tmp_b_i;
	buf[i+1].real = tmp_a_r - tmp_b_r;
	buf[i+1].imag =  tmp_a_i - tmp_b_i;
    }
        
    /* 2. iteration */
	// Note w[1]={{1,0}, {0,-1}}
    for(i = 0; i < 128; i += 4) {
	tmp_a_r = buf[i].real;
	tmp_a_i = buf[i].imag;
	tmp_b_r = buf[i+2].real;
	tmp_b_i = buf[i+2].imag;
	buf[i].real = tmp_a_r + tmp_b_r;
	buf[i].imag =  tmp_a_i + tmp_b_i;
	buf[i+2].real = tmp_a_r - tmp_b_r;
	buf[i+2].imag =  tmp_a_i - tmp_b_i;
	tmp_a_r = buf[i+1].real;
	tmp_a_i = buf[i+1].imag;
	tmp_b_r = buf[i+3].imag;
	tmp_b_i = buf[i+3].real;
	buf[i+1].real = tmp_a_r + tmp_b_r;
	buf[i+1].imag =  tmp_a_i - tmp_b_i;
	buf[i+3].real = tmp_a_r - tmp_b_r;
	buf[i+3].imag =  tmp_a_i + tmp_b_i;
    }

    /* 3. iteration */
    for(i = 0; i < 128; i += 8) {
		tmp_a_r = buf[i].real;
		tmp_a_i = buf[i].imag;
		tmp_b_r = buf[i+4].real;
		tmp_b_i = buf[i+4].imag;
		buf[i].real = tmp_a_r + tmp_b_r;
		buf[i].imag =  tmp_a_i + tmp_b_i;
		buf[i+4].real = tmp_a_r - tmp_b_r;
		buf[i+4].imag =  tmp_a_i - tmp_b_i;
		tmp_a_r = buf[1+i].real;
		tmp_a_i = buf[1+i].imag;
		tmp_b_r = (buf[i+5].real + buf[i+5].imag) * w[2][1].real;
		tmp_b_i = (buf[i+5].imag - buf[i+5].real) * w[2][1].real;
		buf[1+i].real = tmp_a_r + tmp_b_r;
		buf[1+i].imag =  tmp_a_i + tmp_b_i;
		buf[i+5].real = tmp_a_r - tmp_b_r;
		buf[i+5].imag =  tmp_a_i - tmp_b_i;
		tmp_a_r = buf[i+2].real;
		tmp_a_i = buf[i+2].imag;
		tmp_b_r = buf[i+6].imag;
		tmp_b_i = - buf[i+6].real;
		buf[i+2].real = tmp_a_r + tmp_b_r;
		buf[i+2].imag =  tmp_a_i + tmp_b_i;
		buf[i+6].real = tmp_a_r - tmp_b_r;
		buf[i+6].imag =  tmp_a_i - tmp_b_i;
		tmp_a_r = buf[i+3].real;
		tmp_a_i = buf[i+3].imag;
		tmp_b_r = (buf[i+7].real - buf[i+7].imag) * w[2][3].imag;
		tmp_b_i = (buf[i+7].imag + buf[i+7].real) * w[2][3].imag;
		buf[i+3].real = tmp_a_r + tmp_b_r;
		buf[i+3].imag =  tmp_a_i + tmp_b_i;
		buf[i+7].real = tmp_a_r - tmp_b_r;
		buf[i+7].imag =  tmp_a_i - tmp_b_i;
     }
    
    /* 4-7. iterations */
    for (m=3; m < 7; m++) {
        two_m = (1 << m);

	two_m_plus_one = two_m<<1;

	for(i = 0; i < 128; i += two_m_plus_one) {
	    for(k = 0; k < two_m; k++) {
		int p = k + i;
		int q = p + two_m;
		tmp_a_r = buf[p].real;
		tmp_a_i = buf[p].imag;
		tmp_b_r = buf[q].real * w[m][k].real - buf[q].imag * w[m][k].imag;
		tmp_b_i = buf[q].imag * w[m][k].real + buf[q].real * w[m][k].imag;
		buf[p].real = tmp_a_r + tmp_b_r;
		buf[p].imag =  tmp_a_i + tmp_b_i;
		buf[q].real = tmp_a_r - tmp_b_r;
		buf[q].imag =  tmp_a_i - tmp_b_i;
	    }
	}
    }
#endif    
    /* Post IFFT complex multiply  plus IFFT complex conjugate*/
    for( i=0; i < 128; i++) {
	/* y[n] = z[n] * (xcos1[n] + j * xsin1[n]) ; */
	tmp_a_r =        buf[i].real;
	tmp_a_i = -1.0 * buf[i].imag;
	buf[i].real =(tmp_a_r * xcos1[i])  -  (tmp_a_i  * xsin1[i]);
	buf[i].imag =(tmp_a_r * xsin1[i])  +  (tmp_a_i  * xcos1[i]);
    }
	
    data_ptr = data;
    delay_ptr = delay;
    window_ptr = imdct_window;

    /* Window and convert to real valued signal */
    for(i=0; i< 64; i++) { 
	*data_ptr++   = -buf[64+i].imag   * *window_ptr++ + *delay_ptr++ + bias; 
	*data_ptr++   =  buf[64-i-1].real * *window_ptr++ + *delay_ptr++ + bias; 
    }
    
    for(i=0; i< 64; i++) { 
	*data_ptr++  = -buf[i].real       * *window_ptr++ + *delay_ptr++ + bias; 
	*data_ptr++  =  buf[128-i-1].imag * *window_ptr++ + *delay_ptr++ + bias; 
    }
    
    /* The trailing edge of the window goes into the delay line */
    delay_ptr = delay;

    for(i=0; i< 64; i++) { 
	*delay_ptr++  = -buf[64+i].real   * *--window_ptr; 
	*delay_ptr++  =  buf[64-i-1].imag * *--window_ptr; 
    }
    
    for(i=0; i<64; i++) {
	*delay_ptr++  =  buf[i].imag       * *--window_ptr; 
	*delay_ptr++  = -buf[128-i-1].real * *--window_ptr; 
    }
}

#ifdef HAVE_ALTIVEC

#ifndef SYS_DARWIN
#include <altivec.h>
#endif

// used to build registers permutation vectors (vcprm)
// the 's' are for words in the _s_econd vector
#define WORD_0 0x00,0x01,0x02,0x03
#define WORD_1 0x04,0x05,0x06,0x07
#define WORD_2 0x08,0x09,0x0a,0x0b
#define WORD_3 0x0c,0x0d,0x0e,0x0f
#define WORD_s0 0x10,0x11,0x12,0x13
#define WORD_s1 0x14,0x15,0x16,0x17
#define WORD_s2 0x18,0x19,0x1a,0x1b
#define WORD_s3 0x1c,0x1d,0x1e,0x1f

#ifdef SYS_DARWIN
#define vcprm(a,b,c,d) (const vector unsigned char)(WORD_ ## a, WORD_ ## b, WORD_ ## c, WORD_ ## d)
#else
#define vcprm(a,b,c,d) (const vector unsigned char){WORD_ ## a, WORD_ ## b, WORD_ ## c, WORD_ ## d}
#endif

// vcprmle is used to keep the same index as in the SSE version.
// it's the same as vcprm, with the index inversed
// ('le' is Little Endian)
#define vcprmle(a,b,c,d) vcprm(d,c,b,a)

// used to build inverse/identity vectors (vcii)
// n is _n_egative, p is _p_ositive
#define FLOAT_n -1.
#define FLOAT_p 1.

#ifdef SYS_DARWIN
#define vcii(a,b,c,d) (const vector float)(FLOAT_ ## a, FLOAT_ ## b, FLOAT_ ## c, FLOAT_ ## d)
#else
#define vcii(a,b,c,d) (const vector float){FLOAT_ ## a, FLOAT_ ## b, FLOAT_ ## c, FLOAT_ ## d}
#endif

#ifdef SYS_DARWIN
#define FOUROF(a) (a)
#else
#define FOUROF(a) {a,a,a,a}
#endif


void
imdct_do_512_altivec(sample_t data[],sample_t delay[], sample_t bias)
{
  int i;
  int k;
  int p,q;
  int m;
  long two_m;
  long two_m_plus_one;

  sample_t tmp_b_i;
  sample_t tmp_b_r;
  sample_t tmp_a_i;
  sample_t tmp_a_r;

  sample_t *data_ptr;
  sample_t *delay_ptr;
  sample_t *window_ptr;
	
  /* 512 IMDCT with source and dest data in 'data' */
	
  /* Pre IFFT complex multiply plus IFFT cmplx conjugate & reordering*/
  for( i=0; i < 128; i++) {
    /* z[i] = (X[256-2*i-1] + j * X[2*i]) * (xcos1[i] + j * xsin1[i]) ; */ 
    int j= bit_reverse_512[i];
    buf[i].real =         (data[256-2*j-1] * xcos1[j])  -  (data[2*j]       * xsin1[j]);
    buf[i].imag = -1.0 * ((data[2*j]       * xcos1[j])  +  (data[256-2*j-1] * xsin1[j]));
  }
  
  /* 1. iteration */
  for(i = 0; i < 128; i += 2) {
#if 0
    tmp_a_r = buf[i].real;
    tmp_a_i = buf[i].imag;
    tmp_b_r = buf[i+1].real;
    tmp_b_i = buf[i+1].imag;
    buf[i].real = tmp_a_r + tmp_b_r;
    buf[i].imag =  tmp_a_i + tmp_b_i;
    buf[i+1].real = tmp_a_r - tmp_b_r;
    buf[i+1].imag =  tmp_a_i - tmp_b_i;
#else
    vector float temp, bufv; 

    bufv = vec_ld(i << 3, (float*)buf);
    temp = vec_perm(bufv, bufv, vcprm(2,3,0,1));
    bufv = vec_madd(bufv, vcii(p,p,n,n), temp);
    vec_st(bufv, i << 3, (float*)buf);
#endif
  }
        
  /* 2. iteration */
  // Note w[1]={{1,0}, {0,-1}}
  for(i = 0; i < 128; i += 4) {
#if 0
    tmp_a_r = buf[i].real;
    tmp_a_i = buf[i].imag;
    tmp_b_r = buf[i+2].real;
    tmp_b_i = buf[i+2].imag;
    buf[i].real = tmp_a_r + tmp_b_r;
    buf[i].imag =  tmp_a_i + tmp_b_i;
    buf[i+2].real = tmp_a_r - tmp_b_r;
    buf[i+2].imag =  tmp_a_i - tmp_b_i;
    tmp_a_r = buf[i+1].real;
    tmp_a_i = buf[i+1].imag;
    /* WARNING: im <-> re here ! */
    tmp_b_r = buf[i+3].imag;
    tmp_b_i = buf[i+3].real;
    buf[i+1].real = tmp_a_r + tmp_b_r;
    buf[i+1].imag =  tmp_a_i - tmp_b_i;
    buf[i+3].real = tmp_a_r - tmp_b_r;
    buf[i+3].imag =  tmp_a_i + tmp_b_i;
#else
    vector float buf01, buf23, temp1, temp2;
	
    buf01 = vec_ld((i + 0) << 3, (float*)buf);
    buf23 = vec_ld((i + 2) << 3, (float*)buf);
    buf23 = vec_perm(buf23,buf23,vcprm(0,1,3,2));

    temp1 = vec_madd(buf23, vcii(p,p,p,n), buf01);
    temp2 = vec_madd(buf23, vcii(n,n,n,p), buf01);

    vec_st(temp1, (i + 0) << 3, (float*)buf);
    vec_st(temp2, (i + 2) << 3, (float*)buf);
#endif
  }

  /* 3. iteration */
  for(i = 0; i < 128; i += 8) {
#if 0
    tmp_a_r = buf[i].real;
    tmp_a_i = buf[i].imag;
    tmp_b_r = buf[i+4].real;
    tmp_b_i = buf[i+4].imag;
    buf[i].real = tmp_a_r + tmp_b_r;
    buf[i].imag =  tmp_a_i + tmp_b_i;
    buf[i+4].real = tmp_a_r - tmp_b_r;
    buf[i+4].imag =  tmp_a_i - tmp_b_i;
    tmp_a_r = buf[1+i].real;
    tmp_a_i = buf[1+i].imag;
    tmp_b_r = (buf[i+5].real + buf[i+5].imag) * w[2][1].real;
    tmp_b_i = (buf[i+5].imag - buf[i+5].real) * w[2][1].real;
    buf[1+i].real = tmp_a_r + tmp_b_r;
    buf[1+i].imag =  tmp_a_i + tmp_b_i;
    buf[i+5].real = tmp_a_r - tmp_b_r;
    buf[i+5].imag =  tmp_a_i - tmp_b_i;
    tmp_a_r = buf[i+2].real;
    tmp_a_i = buf[i+2].imag;
    /* WARNING re <-> im & sign */
    tmp_b_r = buf[i+6].imag;
    tmp_b_i = - buf[i+6].real;
    buf[i+2].real = tmp_a_r + tmp_b_r;
    buf[i+2].imag =  tmp_a_i + tmp_b_i;
    buf[i+6].real = tmp_a_r - tmp_b_r;
    buf[i+6].imag =  tmp_a_i - tmp_b_i;
    tmp_a_r = buf[i+3].real;
    tmp_a_i = buf[i+3].imag;
    tmp_b_r = (buf[i+7].real - buf[i+7].imag) * w[2][3].imag;
    tmp_b_i = (buf[i+7].imag + buf[i+7].real) * w[2][3].imag;
    buf[i+3].real = tmp_a_r + tmp_b_r;
    buf[i+3].imag =  tmp_a_i + tmp_b_i;
    buf[i+7].real = tmp_a_r - tmp_b_r;
    buf[i+7].imag =  tmp_a_i - tmp_b_i;
#else
    vector float buf01, buf23, buf45, buf67;

    buf01 = vec_ld((i + 0) << 3, (float*)buf);
    buf23 = vec_ld((i + 2) << 3, (float*)buf);

    tmp_b_r = (buf[i+5].real + buf[i+5].imag) * w[2][1].real;
    tmp_b_i = (buf[i+5].imag - buf[i+5].real) * w[2][1].real;
    buf[i+5].real = tmp_b_r;
    buf[i+5].imag = tmp_b_i;
    tmp_b_r = (buf[i+7].real - buf[i+7].imag) * w[2][3].imag;
    tmp_b_i = (buf[i+7].imag + buf[i+7].real) * w[2][3].imag;
    buf[i+7].real = tmp_b_r;
    buf[i+7].imag = tmp_b_i;

    buf23 = vec_ld((i + 2) << 3, (float*)buf);
    buf45 = vec_ld((i + 4) << 3, (float*)buf);
    buf67 = vec_ld((i + 6) << 3, (float*)buf);
    buf67 = vec_perm(buf67, buf67, vcprm(1,0,2,3));
	
    vec_st(vec_add(buf01, buf45), (i + 0) << 3, (float*)buf);
    vec_st(vec_madd(buf67, vcii(p,n,p,p), buf23), (i + 2) << 3, (float*)buf);
    vec_st(vec_sub(buf01, buf45), (i + 4) << 3, (float*)buf);
    vec_st(vec_nmsub(buf67, vcii(p,n,p,p), buf23), (i + 6) << 3, (float*)buf);
#endif
  }
    
  /* 4-7. iterations */
  for (m=3; m < 7; m++) {
    two_m = (1 << m);

    two_m_plus_one = two_m<<1;

    for(i = 0; i < 128; i += two_m_plus_one) {
      for(k = 0; k < two_m; k+=2) {
#if 0
        int p = k + i;
        int q = p + two_m;
        tmp_a_r = buf[p].real;
        tmp_a_i = buf[p].imag;
        tmp_b_r =
          buf[q].real * w[m][k].real -
          buf[q].imag * w[m][k].imag;
        tmp_b_i =
          buf[q].imag * w[m][k].real +
          buf[q].real * w[m][k].imag;
        buf[p].real = tmp_a_r + tmp_b_r;
        buf[p].imag =  tmp_a_i + tmp_b_i;
        buf[q].real = tmp_a_r - tmp_b_r;
        buf[q].imag =  tmp_a_i - tmp_b_i;

        tmp_a_r = buf[(p + 1)].real;
        tmp_a_i = buf[(p + 1)].imag;
        tmp_b_r =
          buf[(q + 1)].real * w[m][(k + 1)].real -
          buf[(q + 1)].imag * w[m][(k + 1)].imag;
        tmp_b_i =
          buf[(q + 1)].imag * w[m][(k + 1)].real +
          buf[(q + 1)].real * w[m][(k + 1)].imag;
        buf[(p + 1)].real = tmp_a_r + tmp_b_r;
        buf[(p + 1)].imag =  tmp_a_i + tmp_b_i;
        buf[(q + 1)].real = tmp_a_r - tmp_b_r;
        buf[(q + 1)].imag =  tmp_a_i - tmp_b_i;
#else
        int p = k + i;
        int q = p + two_m;
        vector float vecp, vecq, vecw, temp1, temp2, temp3, temp4;
        const vector float vczero = (const vector float)FOUROF(0.);
        // first compute buf[q] and buf[q+1]
        vecq = vec_ld(q << 3, (float*)buf);
        vecw = vec_ld(0, (float*)&(w[m][k]));
        temp1 = vec_madd(vecq, vecw, vczero);
        temp2 = vec_perm(vecq, vecq, vcprm(1,0,3,2));
        temp2 = vec_madd(temp2, vecw, vczero);
        temp3 = vec_perm(temp1, temp2, vcprm(0,s0,2,s2));
        temp4 = vec_perm(temp1, temp2, vcprm(1,s1,3,s3));
        vecq = vec_madd(temp4, vcii(n,p,n,p), temp3);
        // then butterfly with buf[p] and buf[p+1]
        vecp = vec_ld(p << 3, (float*)buf);
        
        temp1 = vec_add(vecp, vecq);
        temp2 = vec_sub(vecp, vecq);
                
        vec_st(temp1, p << 3, (float*)buf);
        vec_st(temp2, q << 3, (float*)buf);
#endif
      }
    }
  }

  /* Post IFFT complex multiply  plus IFFT complex conjugate*/
  for( i=0; i < 128; i+=4) {
    /* y[n] = z[n] * (xcos1[n] + j * xsin1[n]) ; */
#if 0
    tmp_a_r =        buf[(i + 0)].real;
    tmp_a_i = -1.0 * buf[(i + 0)].imag;
    buf[(i + 0)].real =
      (tmp_a_r * xcos1[(i + 0)])  -  (tmp_a_i  * xsin1[(i + 0)]);
    buf[(i + 0)].imag =
      (tmp_a_r * xsin1[(i + 0)])  +  (tmp_a_i  * xcos1[(i + 0)]);

    tmp_a_r =        buf[(i + 1)].real;
    tmp_a_i = -1.0 * buf[(i + 1)].imag;
    buf[(i + 1)].real =
      (tmp_a_r * xcos1[(i + 1)])  -  (tmp_a_i  * xsin1[(i + 1)]);
    buf[(i + 1)].imag =
      (tmp_a_r * xsin1[(i + 1)])  +  (tmp_a_i  * xcos1[(i + 1)]);

    tmp_a_r =        buf[(i + 2)].real;
    tmp_a_i = -1.0 * buf[(i + 2)].imag;
    buf[(i + 2)].real =
      (tmp_a_r * xcos1[(i + 2)])  -  (tmp_a_i  * xsin1[(i + 2)]);
    buf[(i + 2)].imag =
      (tmp_a_r * xsin1[(i + 2)])  +  (tmp_a_i  * xcos1[(i + 2)]);

    tmp_a_r =        buf[(i + 3)].real;
    tmp_a_i = -1.0 * buf[(i + 3)].imag;
    buf[(i + 3)].real =
      (tmp_a_r * xcos1[(i + 3)])  -  (tmp_a_i  * xsin1[(i + 3)]);
    buf[(i + 3)].imag =
      (tmp_a_r * xsin1[(i + 3)])  +  (tmp_a_i  * xcos1[(i + 3)]);
#else
    vector float bufv_0, bufv_2, cosv, sinv, temp1, temp2;
    vector float temp0022, temp1133, tempCS01;
    const vector float vczero = (const vector float)FOUROF(0.);

    bufv_0 = vec_ld((i + 0) << 3, (float*)buf);
    bufv_2 = vec_ld((i + 2) << 3, (float*)buf);

    cosv = vec_ld(i << 2, xcos1);
    sinv = vec_ld(i << 2, xsin1);

    temp0022 = vec_perm(bufv_0, bufv_0, vcprm(0,0,2,2));
    temp1133 = vec_perm(bufv_0, bufv_0, vcprm(1,1,3,3));
    tempCS01 = vec_perm(cosv, sinv, vcprm(0,s0,1,s1));
    temp1 = vec_madd(temp0022, tempCS01, vczero);
    tempCS01 = vec_perm(cosv, sinv, vcprm(s0,0,s1,1));
    temp2 = vec_madd(temp1133, tempCS01, vczero);
    bufv_0 = vec_madd(temp2, vcii(p,n,p,n), temp1);
    
    vec_st(bufv_0, (i + 0) << 3, (float*)buf);

    /* idem with bufv_2 and high-order cosv/sinv */

    temp0022 = vec_perm(bufv_2, bufv_2, vcprm(0,0,2,2));
    temp1133 = vec_perm(bufv_2, bufv_2, vcprm(1,1,3,3));
    tempCS01 = vec_perm(cosv, sinv, vcprm(2,s2,3,s3));
    temp1 = vec_madd(temp0022, tempCS01, vczero);
    tempCS01 = vec_perm(cosv, sinv, vcprm(s2,2,s3,3));
    temp2 = vec_madd(temp1133, tempCS01, vczero);
    bufv_2 = vec_madd(temp2, vcii(p,n,p,n), temp1);

    vec_st(bufv_2, (i + 2) << 3, (float*)buf);
    
#endif
  }
  
  data_ptr = data;
  delay_ptr = delay;
  window_ptr = imdct_window;

  /* Window and convert to real valued signal */
  for(i=0; i< 64; i++) { 
    *data_ptr++   = -buf[64+i].imag   * *window_ptr++ + *delay_ptr++ + bias; 
    *data_ptr++   =  buf[64-i-1].real * *window_ptr++ + *delay_ptr++ + bias; 
  }
    
  for(i=0; i< 64; i++) { 
    *data_ptr++  = -buf[i].real       * *window_ptr++ + *delay_ptr++ + bias; 
    *data_ptr++  =  buf[128-i-1].imag * *window_ptr++ + *delay_ptr++ + bias; 
  }
    
  /* The trailing edge of the window goes into the delay line */
  delay_ptr = delay;

  for(i=0; i< 64; i++) { 
    *delay_ptr++  = -buf[64+i].real   * *--window_ptr; 
    *delay_ptr++  =  buf[64-i-1].imag * *--window_ptr; 
  }
    
  for(i=0; i<64; i++) {
    *delay_ptr++  =  buf[i].imag       * *--window_ptr; 
    *delay_ptr++  = -buf[128-i-1].real * *--window_ptr; 
  }
}
#endif


// Stuff below this line is borrowed from libac3
#include "srfftp.h"
#if defined(ARCH_X86) || defined(ARCH_X86_64)
#ifndef HAVE_3DNOW
#define HAVE_3DNOW 1
#endif
#include "srfftp_3dnow.h"

const i_cmplx_t x_plus_minus_3dnow __attribute__ ((aligned (8))) = {{ 0x00000000UL, 0x80000000UL }}; 
const i_cmplx_t x_minus_plus_3dnow __attribute__ ((aligned (8))) = {{ 0x80000000UL, 0x00000000UL }}; 
const complex_t HSQRT2_3DNOW __attribute__ ((aligned (8))) = { 0.707106781188, 0.707106781188 };

#undef HAVE_3DNOWEX
#include "imdct_3dnow.h"
#define HAVE_3DNOWEX
#include "imdct_3dnow.h"

void
imdct_do_512_sse(sample_t data[],sample_t delay[], sample_t bias)
{
/*	int i,k;
    int p,q;*/
    int m;
    long two_m;
    long two_m_plus_one;
    long two_m_plus_one_shl3;
    complex_t *buf_offset;

/*  sample_t tmp_a_i;
    sample_t tmp_a_r;
    sample_t tmp_b_i;
    sample_t tmp_b_r;*/

    sample_t *data_ptr;
    sample_t *delay_ptr;
    sample_t *window_ptr;
	
    /* 512 IMDCT with source and dest data in 'data' */
    /* see the c version (dct_do_512()), its allmost identical, just in C */ 

    /* Pre IFFT complex multiply plus IFFT cmplx conjugate */
    /* Bit reversed shuffling */
	asm volatile(
		"xor %%"REG_S", %%"REG_S"		\n\t"
		"lea "MANGLE(bit_reverse_512)", %%"REG_a"\n\t"
		"mov $1008, %%"REG_D"			\n\t"
		"push %%"REG_BP"			\n\t" //use ebp without telling gcc
		".balign 16				\n\t"
		"1:					\n\t"
		"movlps (%0, %%"REG_S"), %%xmm0	\n\t" // XXXI
		"movhps 8(%0, %%"REG_D"), %%xmm0	\n\t" // RXXI
		"movlps 8(%0, %%"REG_S"), %%xmm1	\n\t" // XXXi
		"movhps (%0, %%"REG_D"), %%xmm1	\n\t" // rXXi
		"shufps $0x33, %%xmm1, %%xmm0		\n\t" // irIR
		"movaps "MANGLE(sseSinCos1c)"(%%"REG_S"), %%xmm2\n\t"
		"mulps %%xmm0, %%xmm2			\n\t"
		"shufps $0xB1, %%xmm0, %%xmm0		\n\t" // riRI
		"mulps "MANGLE(sseSinCos1d)"(%%"REG_S"), %%xmm0\n\t"
		"subps %%xmm0, %%xmm2			\n\t"
		"movzb (%%"REG_a"), %%"REG_d"		\n\t"
		"movzb 1(%%"REG_a"), %%"REG_BP"		\n\t"
		"movlps %%xmm2, (%1, %%"REG_d", 8)	\n\t"
		"movhps %%xmm2, (%1, %%"REG_BP", 8)	\n\t"
		"add $16, %%"REG_S"			\n\t"
		"add $2, %%"REG_a"			\n\t" // avoid complex addressing for P4 crap
		"sub $16, %%"REG_D"			\n\t"
		"jnc 1b				 	\n\t"
		"pop %%"REG_BP"				\n\t"//no we didnt touch ebp *g*
		:: "r" (data), "r" (buf)
		: "%"REG_S, "%"REG_D, "%"REG_a, "%"REG_d
	);


    /* FFT Merge */
/* unoptimized variant
    for (m=1; m < 7; m++) {
	if(m)
	    two_m = (1 << m);
	else
	    two_m = 1;

	two_m_plus_one = (1 << (m+1));

	for(i = 0; i < 128; i += two_m_plus_one) {
	    for(k = 0; k < two_m; k++) {
		p = k + i;
		q = p + two_m;
		tmp_a_r = buf[p].real;
		tmp_a_i = buf[p].imag;
		tmp_b_r = buf[q].real * w[m][k].real - buf[q].imag * w[m][k].imag;
		tmp_b_i = buf[q].imag * w[m][k].real + buf[q].real * w[m][k].imag;
		buf[p].real = tmp_a_r + tmp_b_r;
		buf[p].imag =  tmp_a_i + tmp_b_i;
		buf[q].real = tmp_a_r - tmp_b_r;
		buf[q].imag =  tmp_a_i - tmp_b_i;
	    }
	}
    }
*/
    
    /* 1. iteration */
	// Note w[0][0]={1,0}
	asm volatile(
		"xorps %%xmm1, %%xmm1	\n\t"
		"xorps %%xmm2, %%xmm2	\n\t"
		"mov %0, %%"REG_S"	\n\t"
		".balign 16				\n\t"
		"1:			\n\t"
		"movlps (%%"REG_S"), %%xmm0\n\t" //buf[p]
		"movlps 8(%%"REG_S"), %%xmm1\n\t" //buf[q]
		"movhps (%%"REG_S"), %%xmm0\n\t" //buf[p]
		"movhps 8(%%"REG_S"), %%xmm2\n\t" //buf[q]
		"addps %%xmm1, %%xmm0	\n\t"
		"subps %%xmm2, %%xmm0	\n\t"
		"movaps %%xmm0, (%%"REG_S")\n\t"
		"add $16, %%"REG_S"	\n\t"
		"cmp %1, %%"REG_S"	\n\t"
		" jb 1b			\n\t"
		:: "g" (buf), "r" (buf + 128)
		: "%"REG_S
	);
        
    /* 2. iteration */
	// Note w[1]={{1,0}, {0,-1}}
	asm volatile(
		"movaps "MANGLE(ps111_1)", %%xmm7\n\t" // 1,1,1,-1
		"mov %0, %%"REG_S"		\n\t"
		".balign 16				\n\t"
		"1:				\n\t"
		"movaps 16(%%"REG_S"), %%xmm2	\n\t" //r2,i2,r3,i3
		"shufps $0xB4, %%xmm2, %%xmm2	\n\t" //r2,i2,i3,r3
		"mulps %%xmm7, %%xmm2		\n\t" //r2,i2,i3,-r3
		"movaps (%%"REG_S"), %%xmm0	\n\t" //r0,i0,r1,i1
		"movaps (%%"REG_S"), %%xmm1	\n\t" //r0,i0,r1,i1
		"addps %%xmm2, %%xmm0		\n\t"
		"subps %%xmm2, %%xmm1		\n\t"
		"movaps %%xmm0, (%%"REG_S")	\n\t"
		"movaps %%xmm1, 16(%%"REG_S")	\n\t"
		"add $32, %%"REG_S"	\n\t"
		"cmp %1, %%"REG_S"	\n\t"
		" jb 1b			\n\t"
		:: "g" (buf), "r" (buf + 128)
		: "%"REG_S
	);

    /* 3. iteration */
/*
 Note sseW2+0={1,1,sqrt(2),sqrt(2))
 Note sseW2+16={0,0,sqrt(2),-sqrt(2))
 Note sseW2+32={0,0,-sqrt(2),-sqrt(2))
 Note sseW2+48={1,-1,sqrt(2),-sqrt(2))
*/
	asm volatile(
		"movaps 48+"MANGLE(sseW2)", %%xmm6\n\t" 
		"movaps 16+"MANGLE(sseW2)", %%xmm7\n\t" 
		"xorps %%xmm5, %%xmm5		\n\t"
		"xorps %%xmm2, %%xmm2		\n\t"
		"mov %0, %%"REG_S"		\n\t"
		".balign 16			\n\t"
		"1:				\n\t"
		"movaps 32(%%"REG_S"), %%xmm2	\n\t" //r4,i4,r5,i5
		"movaps 48(%%"REG_S"), %%xmm3	\n\t" //r6,i6,r7,i7
		"movaps "MANGLE(sseW2)", %%xmm4	\n\t" //r4,i4,r5,i5
		"movaps 32+"MANGLE(sseW2)", %%xmm5\n\t" //r6,i6,r7,i7
		"mulps %%xmm2, %%xmm4		\n\t"
		"mulps %%xmm3, %%xmm5		\n\t"
		"shufps $0xB1, %%xmm2, %%xmm2	\n\t" //i4,r4,i5,r5
		"shufps $0xB1, %%xmm3, %%xmm3	\n\t" //i6,r6,i7,r7
		"mulps %%xmm6, %%xmm3		\n\t"
		"mulps %%xmm7, %%xmm2		\n\t"
		"movaps (%%"REG_S"), %%xmm0	\n\t" //r0,i0,r1,i1
		"movaps 16(%%"REG_S"), %%xmm1	\n\t" //r2,i2,r3,i3
		"addps %%xmm4, %%xmm2		\n\t"
		"addps %%xmm5, %%xmm3		\n\t"
		"movaps %%xmm2, %%xmm4		\n\t"
		"movaps %%xmm3, %%xmm5		\n\t"
		"addps %%xmm0, %%xmm2		\n\t"
		"addps %%xmm1, %%xmm3		\n\t"
		"subps %%xmm4, %%xmm0		\n\t"
		"subps %%xmm5, %%xmm1		\n\t"
		"movaps %%xmm2, (%%"REG_S")	\n\t" 
		"movaps %%xmm3, 16(%%"REG_S")	\n\t" 
		"movaps %%xmm0, 32(%%"REG_S")	\n\t" 
		"movaps %%xmm1, 48(%%"REG_S")	\n\t" 
		"add $64, %%"REG_S"	\n\t"
		"cmp %1, %%"REG_S"	\n\t"
		" jb 1b			\n\t"
		:: "g" (buf), "r" (buf + 128)
		: "%"REG_S
	);

    /* 4-7. iterations */
    for (m=3; m < 7; m++) {
	two_m = (1 << m);
	two_m_plus_one = two_m<<1;
	two_m_plus_one_shl3 = (two_m_plus_one<<3);
	buf_offset = buf+128;
	asm volatile(
		"mov %0, %%"REG_S"			\n\t"
		".balign 16				\n\t"
		"1:					\n\t"
		"xor %%"REG_D", %%"REG_D"		\n\t" // k
		"lea (%%"REG_S", %3), %%"REG_d"		\n\t"
		"2:					\n\t"
		"movaps (%%"REG_d", %%"REG_D"), %%xmm1	\n\t"
		"movaps (%4, %%"REG_D", 2), %%xmm2	\n\t"
		"mulps %%xmm1, %%xmm2			\n\t"
		"shufps $0xB1, %%xmm1, %%xmm1		\n\t"
		"mulps 16(%4, %%"REG_D", 2), %%xmm1	\n\t"
		"movaps (%%"REG_S", %%"REG_D"), %%xmm0	\n\t"
		"addps %%xmm2, %%xmm1			\n\t"
		"movaps %%xmm1, %%xmm2			\n\t"
		"addps %%xmm0, %%xmm1			\n\t"
		"subps %%xmm2, %%xmm0			\n\t"
		"movaps %%xmm1, (%%"REG_S", %%"REG_D")	\n\t"
		"movaps %%xmm0, (%%"REG_d", %%"REG_D")	\n\t"
		"add $16, %%"REG_D"			\n\t"
		"cmp %3, %%"REG_D"			\n\t" //FIXME (opt) count against 0 
		"jb 2b					\n\t"
		"add %2, %%"REG_S"			\n\t"
		"cmp %1, %%"REG_S"			\n\t"
		" jb 1b					\n\t"
		:: "g" (buf), "m" (buf_offset), "m" (two_m_plus_one_shl3), "r" (two_m<<3),
		   "r" (sseW[m])
		: "%"REG_S, "%"REG_D, "%"REG_d
	);
    }

    /* Post IFFT complex multiply  plus IFFT complex conjugate*/
	asm volatile(
		"mov $-1024, %%"REG_S"			\n\t"
		".balign 16				\n\t"
		"1:					\n\t"
		"movaps (%0, %%"REG_S"), %%xmm0		\n\t"
		"movaps (%0, %%"REG_S"), %%xmm1		\n\t"
		"shufps $0xB1, %%xmm0, %%xmm0		\n\t"
		"mulps 1024+"MANGLE(sseSinCos1c)"(%%"REG_S"), %%xmm1\n\t"
		"mulps 1024+"MANGLE(sseSinCos1d)"(%%"REG_S"), %%xmm0\n\t"
		"addps %%xmm1, %%xmm0			\n\t"
		"movaps %%xmm0, (%0, %%"REG_S")		\n\t"
		"add $16, %%"REG_S"			\n\t"
		" jnz 1b				\n\t"
		:: "r" (buf+128)
		: "%"REG_S
	);   

	
    data_ptr = data;
    delay_ptr = delay;
    window_ptr = imdct_window;

    /* Window and convert to real valued signal */
	asm volatile(
		"xor %%"REG_D", %%"REG_D"		\n\t"  // 0
		"xor %%"REG_S", %%"REG_S"		\n\t"  // 0
		"movss %3, %%xmm2			\n\t"  // bias
		"shufps $0x00, %%xmm2, %%xmm2		\n\t"  // bias, bias, ...
		".balign 16				\n\t"
		"1:					\n\t"
		"movlps (%0, %%"REG_S"), %%xmm0		\n\t" // ? ? A ?
		"movlps 8(%0, %%"REG_S"), %%xmm1	\n\t" // ? ? C ?
		"movhps -16(%0, %%"REG_D"), %%xmm1	\n\t" // ? D C ?
		"movhps -8(%0, %%"REG_D"), %%xmm0	\n\t" // ? B A ?
		"shufps $0x99, %%xmm1, %%xmm0		\n\t" // D C B A
		"mulps "MANGLE(sseWindow)"(%%"REG_S"), %%xmm0\n\t"
		"addps (%2, %%"REG_S"), %%xmm0		\n\t"
		"addps %%xmm2, %%xmm0			\n\t"
		"movaps %%xmm0, (%1, %%"REG_S")		\n\t"
		"add  $16, %%"REG_S"			\n\t"
		"sub  $16, %%"REG_D"			\n\t"
		"cmp  $512, %%"REG_S"			\n\t" 
		" jb 1b					\n\t"
		:: "r" (buf+64), "r" (data_ptr), "r" (delay_ptr), "m" (bias)
		: "%"REG_S, "%"REG_D
	);
	data_ptr+=128;
	delay_ptr+=128;
//	window_ptr+=128;
	
	asm volatile(
		"mov $1024, %%"REG_D"			\n\t"  // 512
		"xor %%"REG_S", %%"REG_S"		\n\t"  // 0
		"movss %3, %%xmm2			\n\t"  // bias
		"shufps $0x00, %%xmm2, %%xmm2		\n\t"  // bias, bias, ...
		".balign 16				\n\t"
		"1:					\n\t"
		"movlps (%0, %%"REG_S"), %%xmm0		\n\t" // ? ? ? A
		"movlps 8(%0, %%"REG_S"), %%xmm1	\n\t" // ? ? ? C
		"movhps -16(%0, %%"REG_D"), %%xmm1	\n\t" // D ? ? C
		"movhps -8(%0, %%"REG_D"), %%xmm0	\n\t" // B ? ? A
		"shufps $0xCC, %%xmm1, %%xmm0		\n\t" // D C B A
		"mulps 512+"MANGLE(sseWindow)"(%%"REG_S"), %%xmm0\n\t"
		"addps (%2, %%"REG_S"), %%xmm0		\n\t"
		"addps %%xmm2, %%xmm0			\n\t"
		"movaps %%xmm0, (%1, %%"REG_S")		\n\t"
		"add $16, %%"REG_S"			\n\t"
		"sub $16, %%"REG_D"			\n\t"
		"cmp $512, %%"REG_S"			\n\t" 
		" jb 1b					\n\t"
		:: "r" (buf), "r" (data_ptr), "r" (delay_ptr), "m" (bias)
		: "%"REG_S, "%"REG_D
	);
	data_ptr+=128;
//	window_ptr+=128;

    /* The trailing edge of the window goes into the delay line */
    delay_ptr = delay;

	asm volatile(
		"xor %%"REG_D", %%"REG_D"		\n\t"  // 0
		"xor %%"REG_S", %%"REG_S"		\n\t"  // 0
		".balign 16				\n\t"
		"1:					\n\t"
		"movlps (%0, %%"REG_S"), %%xmm0		\n\t" // ? ? ? A
		"movlps 8(%0, %%"REG_S"), %%xmm1	\n\t" // ? ? ? C
		"movhps -16(%0, %%"REG_D"), %%xmm1	\n\t" // D ? ? C 
		"movhps -8(%0, %%"REG_D"), %%xmm0	\n\t" // B ? ? A 
		"shufps $0xCC, %%xmm1, %%xmm0		\n\t" // D C B A
		"mulps 1024+"MANGLE(sseWindow)"(%%"REG_S"), %%xmm0\n\t"
		"movaps %%xmm0, (%1, %%"REG_S")		\n\t"
		"add $16, %%"REG_S"			\n\t"
		"sub $16, %%"REG_D"			\n\t"
		"cmp $512, %%"REG_S"			\n\t" 
		" jb 1b					\n\t"
		:: "r" (buf+64), "r" (delay_ptr)
		: "%"REG_S, "%"REG_D
	);
	delay_ptr+=128;
//	window_ptr-=128;
	
	asm volatile(
		"mov $1024, %%"REG_D"			\n\t"  // 1024
		"xor %%"REG_S", %%"REG_S"		\n\t"  // 0
		".balign 16				\n\t"
		"1:					\n\t"
		"movlps (%0, %%"REG_S"), %%xmm0	\n\t" // ? ? A ?
		"movlps 8(%0, %%"REG_S"), %%xmm1	\n\t" // ? ? C ?
		"movhps -16(%0, %%"REG_D"), %%xmm1	\n\t" // ? D C ? 
		"movhps -8(%0, %%"REG_D"), %%xmm0	\n\t" // ? B A ? 
		"shufps $0x99, %%xmm1, %%xmm0		\n\t" // D C B A
		"mulps 1536+"MANGLE(sseWindow)"(%%"REG_S"), %%xmm0\n\t"
		"movaps %%xmm0, (%1, %%"REG_S")		\n\t"
		"add $16, %%"REG_S"			\n\t"
		"sub $16, %%"REG_D"			\n\t"
		"cmp $512, %%"REG_S"			\n\t" 
		" jb 1b					\n\t"
		:: "r" (buf), "r" (delay_ptr)
		: "%"REG_S, "%"REG_D
	);
}
#endif // ARCH_X86 || ARCH_X86_64

void
imdct_do_256(sample_t data[],sample_t delay[],sample_t bias)
{
    int i,k;
    int p,q;
    int m;
    int two_m;
    int two_m_plus_one;

    sample_t tmp_a_i;
    sample_t tmp_a_r;
    sample_t tmp_b_i;
    sample_t tmp_b_r;

    sample_t *data_ptr;
    sample_t *delay_ptr;
    sample_t *window_ptr;

    complex_t *buf_1, *buf_2;

    buf_1 = &buf[0];
    buf_2 = &buf[64];

    /* Pre IFFT complex multiply plus IFFT cmplx conjugate */
    for(k=0; k<64; k++) { 
	/* X1[k] = X[2*k]  */
	/* X2[k] = X[2*k+1]     */

	p = 2 * (128-2*k-1);
	q = 2 * (2 * k);

	/* Z1[k] = (X1[128-2*k-1] + j * X1[2*k]) * (xcos2[k] + j * xsin2[k]); */ 
	buf_1[k].real =         data[p] * xcos2[k] - data[q] * xsin2[k];
	buf_1[k].imag = -1.0f * (data[q] * xcos2[k] + data[p] * xsin2[k]); 
	/* Z2[k] = (X2[128-2*k-1] + j * X2[2*k]) * (xcos2[k] + j * xsin2[k]); */ 
	buf_2[k].real =          data[p + 1] * xcos2[k] - data[q + 1] * xsin2[k];
	buf_2[k].imag = -1.0f * ( data[q + 1] * xcos2[k] + data[p + 1] * xsin2[k]); 
    }

    /* IFFT Bit reversed shuffling */
    for(i=0; i<64; i++) { 
	k = bit_reverse_256[i];
	if (k < i) {
	    swap_cmplx(&buf_1[i],&buf_1[k]);
	    swap_cmplx(&buf_2[i],&buf_2[k]);
	}
    }

    /* FFT Merge */
    for (m=0; m < 6; m++) {
	two_m = (1 << m);
	two_m_plus_one = (1 << (m+1));

	/* FIXME */
	if(m)
	    two_m = (1 << m);
	else
	    two_m = 1;

	for(k = 0; k < two_m; k++) {
	    for(i = 0; i < 64; i += two_m_plus_one) {
		p = k + i;
		q = p + two_m;
		/* Do block 1 */
		tmp_a_r = buf_1[p].real;
		tmp_a_i = buf_1[p].imag;
		tmp_b_r = buf_1[q].real * w[m][k].real - buf_1[q].imag * w[m][k].imag;
		tmp_b_i = buf_1[q].imag * w[m][k].real + buf_1[q].real * w[m][k].imag;
		buf_1[p].real = tmp_a_r + tmp_b_r;
		buf_1[p].imag =  tmp_a_i + tmp_b_i;
		buf_1[q].real = tmp_a_r - tmp_b_r;
		buf_1[q].imag =  tmp_a_i - tmp_b_i;

		/* Do block 2 */
		tmp_a_r = buf_2[p].real;
		tmp_a_i = buf_2[p].imag;
		tmp_b_r = buf_2[q].real * w[m][k].real - buf_2[q].imag * w[m][k].imag;
		tmp_b_i = buf_2[q].imag * w[m][k].real + buf_2[q].real * w[m][k].imag;
		buf_2[p].real = tmp_a_r + tmp_b_r;
		buf_2[p].imag =  tmp_a_i + tmp_b_i;
		buf_2[q].real = tmp_a_r - tmp_b_r;
		buf_2[q].imag =  tmp_a_i - tmp_b_i;
	    }
	}
    }

    /* Post IFFT complex multiply */
    for( i=0; i < 64; i++) {
	/* y1[n] = z1[n] * (xcos2[n] + j * xs in2[n]) ; */ 
	tmp_a_r =  buf_1[i].real;
	tmp_a_i = -buf_1[i].imag;
	buf_1[i].real =(tmp_a_r * xcos2[i])  -  (tmp_a_i  * xsin2[i]);
	buf_1[i].imag =(tmp_a_r * xsin2[i])  +  (tmp_a_i  * xcos2[i]);
	/* y2[n] = z2[n] * (xcos2[n] + j * xsin2[n]) ; */ 
	tmp_a_r =  buf_2[i].real;
	tmp_a_i = -buf_2[i].imag;
	buf_2[i].real =(tmp_a_r * xcos2[i])  -  (tmp_a_i  * xsin2[i]);
	buf_2[i].imag =(tmp_a_r * xsin2[i])  +  (tmp_a_i  * xcos2[i]);
    }
	
    data_ptr = data;
    delay_ptr = delay;
    window_ptr = imdct_window;

    /* Window and convert to real valued signal */
    for(i=0; i< 64; i++) { 
	*data_ptr++  = -buf_1[i].imag      * *window_ptr++ + *delay_ptr++ + bias;
	*data_ptr++  =  buf_1[64-i-1].real * *window_ptr++ + *delay_ptr++ + bias;
    }

    for(i=0; i< 64; i++) {
	*data_ptr++  = -buf_1[i].real      * *window_ptr++ + *delay_ptr++ + bias;
	*data_ptr++  =  buf_1[64-i-1].imag * *window_ptr++ + *delay_ptr++ + bias;
    }
	
    delay_ptr = delay;

    for(i=0; i< 64; i++) {
	*delay_ptr++ = -buf_2[i].real      * *--window_ptr;
	*delay_ptr++ =  buf_2[64-i-1].imag * *--window_ptr;
    }

    for(i=0; i< 64; i++) {
	*delay_ptr++ =  buf_2[i].imag      * *--window_ptr;
	*delay_ptr++ = -buf_2[64-i-1].real * *--window_ptr;
    }
}

void imdct_init (uint32_t mm_accel)
{
#ifdef LIBA52_MLIB
    if (mm_accel & MM_ACCEL_MLIB) {
        fprintf (stderr, "Using mlib for IMDCT transform\n");
	imdct_512 = imdct_do_512_mlib;
	imdct_256 = imdct_do_256_mlib;
    } else
#endif
    {
	int i, j, k;

	/* Twiddle factors to turn IFFT into IMDCT */
	for (i = 0; i < 128; i++) {
	    xcos1[i] = -cos ((M_PI / 2048) * (8 * i + 1));
	    xsin1[i] = -sin ((M_PI / 2048) * (8 * i + 1));
	}
#if defined(ARCH_X86) || defined(ARCH_X86_64)
	for (i = 0; i < 128; i++) {
	    sseSinCos1c[2*i+0]= xcos1[i];
	    sseSinCos1c[2*i+1]= -xcos1[i];
	    sseSinCos1d[2*i+0]= xsin1[i];
	    sseSinCos1d[2*i+1]= xsin1[i];	
	}
#endif

	/* More twiddle factors to turn IFFT into IMDCT */
	for (i = 0; i < 64; i++) {
	    xcos2[i] = -cos ((M_PI / 1024) * (8 * i + 1));
	    xsin2[i] = -sin ((M_PI / 1024) * (8 * i + 1));
	}

	for (i = 0; i < 7; i++) {
	    j = 1 << i;
	    for (k = 0; k < j; k++) {
		w[i][k].real = cos (-M_PI * k / j);
		w[i][k].imag = sin (-M_PI * k / j);
	    }
	}
#if defined(ARCH_X86) || defined(ARCH_X86_64)
	for (i = 1; i < 7; i++) {
	    j = 1 << i;
	    for (k = 0; k < j; k+=2) {
	    
	    	sseW[i][4*k + 0] = w[i][k+0].real;
	    	sseW[i][4*k + 1] = w[i][k+0].real;
	    	sseW[i][4*k + 2] = w[i][k+1].real;
	    	sseW[i][4*k + 3] = w[i][k+1].real;

	    	sseW[i][4*k + 4] = -w[i][k+0].imag;
	    	sseW[i][4*k + 5] = w[i][k+0].imag;
	    	sseW[i][4*k + 6] = -w[i][k+1].imag;
	    	sseW[i][4*k + 7] = w[i][k+1].imag;	    
	    	
	//we multiply more or less uninitalized numbers so we need to use exactly 0.0
		if(k==0)
		{
//			sseW[i][4*k + 0]= sseW[i][4*k + 1]= 1.0;
			sseW[i][4*k + 4]= sseW[i][4*k + 5]= 0.0;
		}
		
		if(2*k == j)
		{
			sseW[i][4*k + 0]= sseW[i][4*k + 1]= 0.0;
//			sseW[i][4*k + 4]= -(sseW[i][4*k + 5]= -1.0);
		}
	    }
	}

	for(i=0; i<128; i++)
	{
		sseWindow[2*i+0]= -imdct_window[2*i+0];
		sseWindow[2*i+1]=  imdct_window[2*i+1];	
	}
	
	for(i=0; i<64; i++)
	{
		sseWindow[256 + 2*i+0]= -imdct_window[254 - 2*i+1];
		sseWindow[256 + 2*i+1]=  imdct_window[254 - 2*i+0];
		sseWindow[384 + 2*i+0]=  imdct_window[126 - 2*i+1];
		sseWindow[384 + 2*i+1]= -imdct_window[126 - 2*i+0];
	}
#endif // ARCH_X86 || ARCH_X86_64

	imdct_512 = imdct_do_512;
#if defined(ARCH_X86) || defined(ARCH_X86_64)
	if(mm_accel & MM_ACCEL_X86_SSE)
	{
	  fprintf (stderr, "Using SSE optimized IMDCT transform\n");
	  imdct_512 = imdct_do_512_sse;
	}  
	else
	if(mm_accel & MM_ACCEL_X86_3DNOWEXT)
	{
	  fprintf (stderr, "Using 3DNowEx optimized IMDCT transform\n");
	  imdct_512 = imdct_do_512_3dnowex;
	}
	else
	if(mm_accel & MM_ACCEL_X86_3DNOW)
	{
	  fprintf (stderr, "Using 3DNow optimized IMDCT transform\n");
	  imdct_512 = imdct_do_512_3dnow;
	}
	else
#endif // ARCH_X86 || ARCH_X86_64
#ifdef HAVE_ALTIVEC
        if (mm_accel & MM_ACCEL_PPC_ALTIVEC)
	{
	  fprintf(stderr, "Using AltiVec optimized IMDCT transform\n");
          imdct_512 = imdct_do_512_altivec;
	}
        else
#endif
	fprintf (stderr, "No accelerated IMDCT transform found\n");
	imdct_256 = imdct_do_256;
    }
}

static void fft_asmb(int k, complex_t *x, complex_t *wTB,
	     const complex_t *d, const complex_t *d_3)
{
  register complex_t  *x2k, *x3k, *x4k, *wB;
  register float a_r, a_i, a1_r, a1_i, u_r, u_i, v_r, v_i;

  x2k = x + 2 * k;
  x3k = x2k + 2 * k;
  x4k = x3k + 2 * k;
  wB = wTB + 2 * k;
  
  TRANSZERO(x[0],x2k[0],x3k[0],x4k[0]);
  TRANS(x[1],x2k[1],x3k[1],x4k[1],wTB[1],wB[1],d[1],d_3[1]);
  
  --k;
  for(;;) {
     TRANS(x[2],x2k[2],x3k[2],x4k[2],wTB[2],wB[2],d[2],d_3[2]);
     TRANS(x[3],x2k[3],x3k[3],x4k[3],wTB[3],wB[3],d[3],d_3[3]);
     if (!--k) break;
     x += 2;
     x2k += 2;
     x3k += 2;
     x4k += 2;
     d += 2;
     d_3 += 2;
     wTB += 2;
     wB += 2;
  }
 
}

static void fft_asmb16(complex_t *x, complex_t *wTB)
{
  register float a_r, a_i, a1_r, a1_i, u_r, u_i, v_r, v_i;
  int k = 2;

  /* transform x[0], x[8], x[4], x[12] */
  TRANSZERO(x[0],x[4],x[8],x[12]);

  /* transform x[1], x[9], x[5], x[13] */
  TRANS(x[1],x[5],x[9],x[13],wTB[1],wTB[5],delta16[1],delta16_3[1]);

  /* transform x[2], x[10], x[6], x[14] */
  TRANSHALF_16(x[2],x[6],x[10],x[14]);

  /* transform x[3], x[11], x[7], x[15] */
  TRANS(x[3],x[7],x[11],x[15],wTB[3],wTB[7],delta16[3],delta16_3[3]);

} 

static void fft_4(complex_t *x)
{
  /* delta_p = 1 here */
  /* x[k] = sum_{i=0..3} x[i] * w^{i*k}, w=e^{-2*pi/4} 
   */

  register float yt_r, yt_i, yb_r, yb_i, u_r, u_i, vi_r, vi_i;
  
  yt_r = x[0].real;
  yb_r = yt_r - x[2].real;
  yt_r += x[2].real;

  u_r = x[1].real;
  vi_i = x[3].real - u_r;
  u_r += x[3].real;
  
  u_i = x[1].imag;
  vi_r = u_i - x[3].imag;
  u_i += x[3].imag;

  yt_i = yt_r;
  yt_i += u_r;
  x[0].real = yt_i;
  yt_r -= u_r;
  x[2].real = yt_r;
  yt_i = yb_r;
  yt_i += vi_r;
  x[1].real = yt_i;
  yb_r -= vi_r;
  x[3].real = yb_r;

  yt_i = x[0].imag;
  yb_i = yt_i - x[2].imag;
  yt_i += x[2].imag;

  yt_r = yt_i;
  yt_r += u_i;
  x[0].imag = yt_r;
  yt_i -= u_i;
  x[2].imag = yt_i;
  yt_r = yb_i;
  yt_r += vi_i;
  x[1].imag = yt_r;
  yb_i -= vi_i;
  x[3].imag = yb_i;
}


static void fft_8(complex_t *x)
{
  /* delta_p = diag{1, sqrt(i)} here */
  /* x[k] = sum_{i=0..7} x[i] * w^{i*k}, w=e^{-2*pi/8} 
   */
  register float wT1_r, wT1_i, wB1_r, wB1_i, wT2_r, wT2_i, wB2_r, wB2_i;
  
  wT1_r = x[1].real;
  wT1_i = x[1].imag;
  wB1_r = x[3].real;
  wB1_i = x[3].imag;

  x[1] = x[2];
  x[2] = x[4];
  x[3] = x[6];
  fft_4(&x[0]);

  
  /* x[0] x[4] */
  wT2_r = x[5].real;
  wT2_r += x[7].real;
  wT2_r += wT1_r;
  wT2_r += wB1_r;
  wT2_i = wT2_r;
  wT2_r += x[0].real;
  wT2_i = x[0].real - wT2_i;
  x[0].real = wT2_r;
  x[4].real = wT2_i;

  wT2_i = x[5].imag;
  wT2_i += x[7].imag;
  wT2_i += wT1_i;
  wT2_i += wB1_i;
  wT2_r = wT2_i;
  wT2_r += x[0].imag;
  wT2_i = x[0].imag - wT2_i;
  x[0].imag = wT2_r;
  x[4].imag = wT2_i;
  
  /* x[2] x[6] */
  wT2_r = x[5].imag;
  wT2_r -= x[7].imag;
  wT2_r += wT1_i;
  wT2_r -= wB1_i;
  wT2_i = wT2_r;
  wT2_r += x[2].real;
  wT2_i = x[2].real - wT2_i;
  x[2].real = wT2_r;
  x[6].real = wT2_i;

  wT2_i = x[5].real;
  wT2_i -= x[7].real;
  wT2_i += wT1_r;
  wT2_i -= wB1_r;
  wT2_r = wT2_i;
  wT2_r += x[2].imag;
  wT2_i = x[2].imag - wT2_i;
  x[2].imag = wT2_i;
  x[6].imag = wT2_r;
  

  /* x[1] x[5] */
  wT2_r = wT1_r;
  wT2_r += wB1_i;
  wT2_r -= x[5].real;
  wT2_r -= x[7].imag;
  wT2_i = wT1_i;
  wT2_i -= wB1_r;
  wT2_i -= x[5].imag;
  wT2_i += x[7].real;

  wB2_r = wT2_r;
  wB2_r += wT2_i;
  wT2_i -= wT2_r;
  wB2_r *= HSQRT2;
  wT2_i *= HSQRT2;
  wT2_r = wB2_r;
  wB2_r += x[1].real;
  wT2_r =  x[1].real - wT2_r;

  wB2_i = x[5].real;
  x[1].real = wB2_r;
  x[5].real = wT2_r;

  wT2_r = wT2_i;
  wT2_r += x[1].imag;
  wT2_i = x[1].imag - wT2_i;
  wB2_r = x[5].imag;
  x[1].imag = wT2_r;
  x[5].imag = wT2_i;

  /* x[3] x[7] */
  wT1_r -= wB1_i;
  wT1_i += wB1_r;
  wB1_r = wB2_i - x[7].imag;
  wB1_i = wB2_r + x[7].real;
  wT1_r -= wB1_r;
  wT1_i -= wB1_i;
  wB1_r = wT1_r + wT1_i;
  wB1_r *= HSQRT2;
  wT1_i -= wT1_r;
  wT1_i *= HSQRT2;
  wB2_r = x[3].real;
  wB2_i = wB2_r + wT1_i;
  wB2_r -= wT1_i;
  x[3].real = wB2_i;
  x[7].real = wB2_r;
  wB2_i = x[3].imag;
  wB2_r = wB2_i + wB1_r;
  wB2_i -= wB1_r;
  x[3].imag = wB2_i;
  x[7].imag = wB2_r;
}


static void fft_128p(complex_t *a)
{
  fft_8(&a[0]); fft_4(&a[8]); fft_4(&a[12]);
  fft_asmb16(&a[0], &a[8]);
  
  fft_8(&a[16]), fft_8(&a[24]);
  fft_asmb(4, &a[0], &a[16],&delta32[0], &delta32_3[0]);

  fft_8(&a[32]); fft_4(&a[40]); fft_4(&a[44]);
  fft_asmb16(&a[32], &a[40]);

  fft_8(&a[48]); fft_4(&a[56]); fft_4(&a[60]);
  fft_asmb16(&a[48], &a[56]);

  fft_asmb(8, &a[0], &a[32],&delta64[0], &delta64_3[0]);

  fft_8(&a[64]); fft_4(&a[72]); fft_4(&a[76]);
  /* fft_16(&a[64]); */
  fft_asmb16(&a[64], &a[72]);

  fft_8(&a[80]); fft_8(&a[88]);
  
  /* fft_32(&a[64]); */
  fft_asmb(4, &a[64], &a[80],&delta32[0], &delta32_3[0]);

  fft_8(&a[96]); fft_4(&a[104]), fft_4(&a[108]);
  /* fft_16(&a[96]); */
  fft_asmb16(&a[96], &a[104]);

  fft_8(&a[112]), fft_8(&a[120]);
  /* fft_32(&a[96]); */
  fft_asmb(4, &a[96], &a[112], &delta32[0], &delta32_3[0]);
  
  /* fft_128(&a[0]); */
  fft_asmb(16, &a[0], &a[64], &delta128[0], &delta128_3[0]);
}




/*
 *  my_profile.h
 *
 *	Copyright (C) Nick Kurshev <nickols_k@mail.ru> - Oct 2001
 *
 *  You can redistribute this file under terms and conditions
 *  of GNU General Public licence v2.
 *  This file is partly based on vbetest.c from lrmi distributive.
 */
#ifndef MY_PROFILE_INC
#define MY_PROFILE_INC

extern unsigned long long int my_profile_start,my_profile_end,my_profile_total;

#if defined ( ENABLE_PROFILE ) && defined ( ARCH_X86 )
static inline unsigned long long int read_tsc( void )
{
  unsigned long long int retval;
  __asm __volatile ("rdtsc":"=A"(retval)::"memory");
  return retval;
}

#define PROFILE_START()			(my_profile_start=read_tsc())
#define PROFILE_END(your_message)	{ my_profile_end=read_tsc(); my_profile_total+=my_profile_end - my_profile_start; printf(your_message" current=%llu total=%llu\n\t",my_profile_end-my_profile_start,my_profile_total); }
#else
#define PROFILE_INIT()
#define PROFILE_START()
#define PROFILE_END(your_message)
#endif



#endif
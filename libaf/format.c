#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <inttypes.h>
#include <limits.h>

#include "af.h"

// Convert from string to format
int af_str2fmt(char* str)
{
  int format=0;
  // Scan for endianess
  if(strstr(str,"be") || strstr(str,"BE"))
    format |= AF_FORMAT_BE;
  else if(strstr(str,"le") || strstr(str,"LE"))
    format |= AF_FORMAT_LE;
  else
    format |= AF_FORMAT_NE;    

  // Scan for special formats
  if(strstr(str,"mulaw") || strstr(str,"MULAW")){
    format |= AF_FORMAT_MU_LAW; return format;
  }
  if(strstr(str,"alaw") || strstr(str,"ALAW")){
    format |= AF_FORMAT_A_LAW; return format;
  }
  if(strstr(str,"ac3") || strstr(str,"AC3")){
    format |= AF_FORMAT_AC3; return format;
  }
  if(strstr(str,"mpeg2") || strstr(str,"MPEG2")){
    format |= AF_FORMAT_MPEG2; return format;
  }
  if(strstr(str,"imaadpcm") || strstr(str,"IMAADPCM")){
    format |= AF_FORMAT_IMA_ADPCM; return format;
  }
  
  // Scan for int/float
  if(strstr(str,"float") || strstr(str,"FLOAT")){
    format |= AF_FORMAT_F; return format;
  }
  else
    format |= AF_FORMAT_I;

  // Scan for signed/unsigned
  if(strstr(str,"unsigned") || strstr(str,"UNSIGNED"))
    format |= AF_FORMAT_US; 
  else
    format |= AF_FORMAT_SI;
  
  return format;
}

inline int af_fmt2bits(int format)
{
    return (format & AF_FORMAT_BITS_MASK)+8;
//    return (((format & AF_FORMAT_BITS_MASK)>>3)+1) * 8;
#if 0
    switch(format & AF_FORMAT_BITS_MASK)
    {
	case AF_FORMAT_8BIT: return 8;
	case AF_FORMAT_16BIT: return 16;
	case AF_FORMAT_24BIT: return 24;
	case AF_FORMAT_32BIT: return 32;
	case AF_FORMAT_48BIT: return 48;
    }
#endif
    return -1;
}

inline int af_bits2fmt(int bits)
{
    return (bits/8 - 1) << 3;
}

/* Convert format to str input str is a buffer for the 
   converted string, size is the size of the buffer */
char* af_fmt2str(int format, char* str, int size)
{
  int i=0;

  if (size < 1)
    return NULL;
  size--; // reserve one for terminating 0

  // Endianess
  if(AF_FORMAT_LE == (format & AF_FORMAT_END_MASK))
    i+=snprintf(str,size-i,"little-endian ");
  else
    i+=snprintf(str,size-i,"big-endian ");
  
  if(format & AF_FORMAT_SPECIAL_MASK){
    switch(format & AF_FORMAT_SPECIAL_MASK){
    case(AF_FORMAT_MU_LAW): 
      i+=snprintf(&str[i],size-i,"mu-law "); break;
    case(AF_FORMAT_A_LAW): 
      i+=snprintf(&str[i],size-i,"A-law "); break;
    case(AF_FORMAT_MPEG2): 
      i+=snprintf(&str[i],size-i,"MPEG-2 "); break;
    case(AF_FORMAT_AC3): 
      i+=snprintf(&str[i],size-i,"AC3 "); break;
    case(AF_FORMAT_IMA_ADPCM): 
      i+=snprintf(&str[i],size-i,"IMA-ADPCM "); break;
    default:
      printf("Unknown special\n");
    }
  }
  else{
    // Bits
    i+=snprintf(&str[i],size-i,"%d-bit ", af_fmt2bits(format));

    // Point
    if(AF_FORMAT_F == (format & AF_FORMAT_POINT_MASK))
      i+=snprintf(&str[i],size-i,"float ");
    else{
      // Sign
      if(AF_FORMAT_US == (format & AF_FORMAT_SIGN_MASK))
	i+=snprintf(&str[i],size-i,"unsigned ");
      else
	i+=snprintf(&str[i],size-i,"signed ");

      i+=snprintf(&str[i],size-i,"int ");
    }
  }
  // remove trailing space
  if (i > 0 && str[i - 1] == ' ')
    i--;
  str[i] = 0; // make sure it is 0 terminated.
  return str;
}

char *af_fmt2str_short(int format)
{
    switch(format)
    {
	// special
	case AF_FORMAT_MU_LAW: return "mulaw";
	case AF_FORMAT_A_LAW: return "alaw";
	case AF_FORMAT_MPEG2: return "mpeg2";
	case AF_FORMAT_AC3: return "ac3";
	case AF_FORMAT_IMA_ADPCM: return "imaadpcm";
	// ordinary
	case AF_FORMAT_U8: return "u8";
	case AF_FORMAT_S8: return "s8";
	case AF_FORMAT_U16_LE: return "u16le";
	case AF_FORMAT_U16_BE: return "u16be";
	case AF_FORMAT_S16_LE: return "s16le";
	case AF_FORMAT_S16_BE: return "s16be";
	case AF_FORMAT_U24_LE: return "u24le";
	case AF_FORMAT_U24_BE: return "u24be";
	case AF_FORMAT_S24_LE: return "s24le";
	case AF_FORMAT_S24_BE: return "s24be";
	case AF_FORMAT_U32_LE: return "u32le";
	case AF_FORMAT_U32_BE: return "u32be";
	case AF_FORMAT_S32_LE: return "s32le";
	case AF_FORMAT_S32_BE: return "s32be";
	case AF_FORMAT_FLOAT_LE: return "floatle";
	case AF_FORMAT_FLOAT_BE: return "floatbe";
    }
    return "??";
}

int af_str2fmt_short(char* str)
{
    int i;
    static struct {
	const char *name;
	const int format;
    } table[] = {
	{ "mulaw", AF_FORMAT_MU_LAW },
	{ "alaw", AF_FORMAT_A_LAW },
	{ "mpeg2", AF_FORMAT_MPEG2 },
	{ "ac3", AF_FORMAT_AC3 },
	{ "imaadpcm", AF_FORMAT_IMA_ADPCM },

	{ "u8", AF_FORMAT_U8 },
	{ "s8", AF_FORMAT_S8 },
	{ "u16le", AF_FORMAT_U16_LE },
	{ "u16be", AF_FORMAT_U16_BE },
	{ "u16ne", AF_FORMAT_U16_NE },
	{ "s16le", AF_FORMAT_S16_LE },
	{ "s16be", AF_FORMAT_S16_BE },
	{ "s16ne", AF_FORMAT_S16_NE },
	{ "u24le", AF_FORMAT_U24_LE },
	{ "u24be", AF_FORMAT_U24_BE },
	{ "u24ne", AF_FORMAT_U24_NE },
	{ "s24le", AF_FORMAT_S24_LE },
	{ "s24be", AF_FORMAT_S24_BE },
	{ "s24ne", AF_FORMAT_S24_NE },
	{ "u32le", AF_FORMAT_U32_LE },
	{ "u32be", AF_FORMAT_U32_BE },
	{ "u32ne", AF_FORMAT_U32_NE },
	{ "s32le", AF_FORMAT_S32_LE },
	{ "s32be", AF_FORMAT_S32_BE },
	{ "s32ne", AF_FORMAT_S32_NE },
	{ "floatle", AF_FORMAT_FLOAT_LE },
	{ "floatbe", AF_FORMAT_FLOAT_BE },
	{ "floatne", AF_FORMAT_FLOAT_NE },
	
	{ NULL, 0 }
    };
    
    for (i = 0; table[i].name; i++)
	if (!strcasecmp(str, table[i].name))
	    return table[i].format;

    return -1;
}

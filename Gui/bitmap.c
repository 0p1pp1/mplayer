
#include <stdlib.h>
#include <stdio.h>

#include "bitmap.h"
#include "../error.h"

#define BMP 1
#define TGA 2
#define PNG 3
#define TGAPACKED 4

extern char * strcat( char * dest,const char * src );

int conv24to32( txSample * bf )
{
 unsigned char * tmpImage;
 int             i,c;

 if ( bf->BPP == 24 )
  {
   tmpImage=bf->Image;
   bf->ImageSize=bf->Width * bf->Height * 4;
   bf->BPP=32;
   if ( ( bf->Image=malloc( bf->ImageSize ) ) == NULL )
    {
     #ifdef DEBUG
      dbprintf( 4,"[bitmap] Not enough memory for image.\n" );
     #endif
     return 1;
    }
   memset( bf->Image,0,bf->ImageSize );
   for ( c=0,i=0;i < bf->Width * bf->Height * 3; )
    {
     bf->Image[c++]=tmpImage[i++];
     bf->Image[c++]=tmpImage[i++];
     bf->Image[c++]=tmpImage[i++]; c++;
    }
   free( tmpImage );
  }
 return 0;
}

void bgr2rgb( txSample * bf )
{
 unsigned char c;
 int           i;

 for ( i=0;i < bf->ImageSize;i+=4 )
  {
   c=bf->Image[i];
   bf->Image[i]=bf->Image[i+2];
   bf->Image[i+2]=c;
  }
}

void Normalize( txSample * bf )
{
 int           i;

 for ( i=0;i < bf->ImageSize;i+=4 ) bf->Image[i+3]=0;
}

unsigned char tmp[512];

unsigned char * fExist( unsigned char * fname )
{
 FILE          * fl;
 unsigned char   ext[][6] = { ".tga\0",".TGA\0",".png\0",".PNG\0",".bmp\0",".BMP\0"  };
 int             i;

 fl=fopen( fname,"rb" );
 if ( fl != NULL )
  {
   fclose( fl );
   return fname;
  }
 for ( i=0;i<10;i++ )
  {
   strcpy( tmp,fname );
   strcat( tmp,ext[i] );
   fl=fopen( tmp,"rb" );
   if ( fl != NULL )
    {
     fclose( fl );
     return tmp;
    }
  }
 return NULL;
}

int aComp( unsigned char * b1,unsigned char * b2,int size )
{
 int i;
 for( i=0;i<size;i++ ) if ( b1[i] != b2[i] ) return 0;
 return 1;
}

int GetFileType( char * fname )
{
 FILE * fl;
 unsigned char buffer[10];
 unsigned char  bmp[2] = { 0x42,0x4d };
 unsigned char  tga[7] = { 0x00,0x02,0x00,0x00,0x00,0x00,0x00 };
 unsigned char ptga[7] = { 0x00,0x0a,0x00,0x00,0x00,0x00,0x00 };
 unsigned char  png[8] = { 0x89,0x50,0x4e,0x47,0x0d,0x0a,0x1a,0x0a };

 if ( ( fl=fopen( fname,"rb" ) ) == NULL ) return -1;
 fread( buffer,1,10,fl );
 fclose( fl );

 if ( aComp( buffer,bmp,2 ) ) return BMP;        // --- bmp
 if ( aComp( &buffer[1],tga,8 ) ) return TGA;        // --- tga
 if ( aComp( &buffer[1],ptga,7 ) ) return TGAPACKED; // --- tga
 if ( aComp( buffer,png,8 ) ) return PNG;        // --- png
 return 0;                                       // --- others
}

int bpRead( char * fname, txSample * bf )
{
 int bgr = 0;
 int i;

 fname=fExist( fname );
 if ( fname == NULL ) return -2;
 switch ( GetFileType( fname ) )
  {
   case BMP:
        i=bmpRead( fname,bf );
        switch ( i )
         {
          case 0:  break;
          case 3:  return -1;
          default: return -3;
         }
        break;
   case TGA:
        i=tgaRead( fname,bf );
        switch ( i )
         {
          case 0:  break;
          case 3:  return -1;
          default: return -4;
         }
        break;
   case PNG:
        if ( pngRead( fname,bf ) ) return -5;
        bgr=1;
        break;
   case TGAPACKED:
        #ifdef DEBUG
         dbprintf( 4,"[bitmap] sorry, packed TGA not supported.\n" );
        #endif
        return -6;
   default:
     {
      #ifdef DEBUG
       dbprintf( 4,"[bitmap] Unknown file type ( %s ).\n",fname );
      #endif
      return -7;
     }
  }
 if ( bf->BPP < 24 )
  {
   #ifdef DEBUG
    dbprintf( 4,"[bitmap] sorry, 16 or less bitmaps not supported.\n" );
   #endif
   return -1;
  }
 if ( conv24to32( bf ) ) return -8;
 if ( bgr ) bgr2rgb( bf );
 Normalize( bf );
 return 0;
}

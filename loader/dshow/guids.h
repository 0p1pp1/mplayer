#ifndef GUIDS_H
#define GUIDS_H
//#include <loader.h>
//#include <wine/winbase.h>
#include <com.h>
#include <formats.h>
extern int DSHOW_DEBUG;
#define Debug if(DSHOW_DEBUG)

struct IUnknown;
typedef struct  _MediaType
{
    GUID      majortype;		//0x0
    GUID      subtype;			//0x10
    int      bFixedSizeSamples;		//0x20
    int      bTemporalCompression;	//0x24
    unsigned long     lSampleSize;	//0x28
    GUID      formattype;		//0x2c
    IUnknown  *pUnk;			//0x3c
    unsigned long     cbFormat;		//0x40
    char *pbFormat;			//0x44
} AM_MEDIA_TYPE;
typedef enum
{
    PINDIR_INPUT	= 0,
    PINDIR_OUTPUT	= PINDIR_INPUT + 1
} PIN_DIRECTION;
typedef long long REFERENCE_TIME;
//typedef long long LONGLONG;
struct RECT32
{
    int left, top, right, bottom;
};
typedef struct tagVIDEOINFOHEADER {

    RECT32            rcSource;          // The bit we really want to use
    RECT32            rcTarget;          // Where the video should go
    unsigned long     dwBitRate;         // Approximate bit data rate
    unsigned long     dwBitErrorRate;    // Bit error rate for this stream
    REFERENCE_TIME  AvgTimePerFrame;   // Average time per frame (100ns units)

    BITMAPINFOHEADER bmiHeader;

} VIDEOINFOHEADER;
typedef struct _AllocatorProperties
{
    long cBuffers;
    long cbBuffer;
    long cbAlign;
    long cbPrefix;
}   ALLOCATOR_PROPERTIES;
struct IBaseFilter;
typedef struct _PinInfo
{
    IBaseFilter *pFilter;
    PIN_DIRECTION dir;
    unsigned short achName[ 128 ];
}   PIN_INFO;


extern GUID IID_IBaseFilter;
extern GUID IID_IEnumPins;
extern GUID IID_IEnumMediaTypes;
extern GUID IID_IMemInputPin;
extern GUID IID_IMemAllocator;
extern GUID IID_IMediaSample;
extern GUID IID_DivxHidden;
extern GUID IID_Iv50Hidden;
extern GUID CLSID_DivxDecompressorCF;
extern GUID CLSID_IV50_Decoder;
extern GUID CLSID_MemoryAllocator;
extern GUID MEDIATYPE_Video;
extern GUID GUID_NULL;
extern GUID FORMAT_VideoInfo;
extern GUID MEDIASUBTYPE_RGB565;
extern GUID MEDIASUBTYPE_RGB555;
extern GUID MEDIASUBTYPE_RGB24;
extern GUID MEDIASUBTYPE_RGB32;
extern GUID MEDIASUBTYPE_YUYV;
extern GUID MEDIASUBTYPE_IYUV;
extern GUID MEDIASUBTYPE_YVU9;
extern GUID MEDIASUBTYPE_Y411;
extern GUID MEDIASUBTYPE_Y41P;
extern GUID MEDIASUBTYPE_YUY2;
extern GUID MEDIASUBTYPE_YVYU;
extern GUID MEDIASUBTYPE_UYVY;
extern GUID MEDIASUBTYPE_Y211;
extern GUID MEDIASUBTYPE_YV12;

#endif

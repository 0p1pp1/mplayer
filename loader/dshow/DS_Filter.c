#include "config.h"
#include "DS_Filter.h"
#include "driver.h"
#include "com.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "win32.h" // printf macro

typedef long STDCALL (*GETCLASS) (const GUID*, const GUID*, void**);

#ifndef WIN32_LOADER
const GUID IID_IUnknown =
{
    0x00000000, 0x0000, 0x0000,
    {0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x46}
};
const GUID IID_IClassFactory =
{
    0x00000001, 0x0000, 0x0000,
    {0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x46}
};

HRESULT STDCALL CoInitialize(LPVOID pvReserved); 
void STDCALL CoUninitialize(void); 
#endif

//void trapbug();

static void DS_Filter_Start(DS_Filter* This)
{
    HRESULT hr;

    if (This->m_pAll)
	return;

    //Debug printf("DS_Filter_Start(%p)\n", This);
    hr = This->m_pFilter->vt->Run(This->m_pFilter, (REFERENCE_TIME)0);
    if (hr != 0)
    {
	Debug printf("WARNING: m_Filter->Run() failed, error code %x\n", (int)hr);
    }
    hr = This->m_pImp->vt->GetAllocator(This->m_pImp, &This->m_pAll);

    if (hr || !This->m_pAll)
    {
	Debug printf("WARNING: error getting IMemAllocator interface %x\n", (int)hr);
	This->m_pImp->vt->Release((IUnknown*)This->m_pImp);
        return;
    }
    This->m_pImp->vt->NotifyAllocator(This->m_pImp, This->m_pAll, 0);
}

static void DS_Filter_Stop(DS_Filter* This)
{
    if (This->m_pAll)
    {
	//Debug	printf("DS_Filter_Stop(%p)\n", This);
	This->m_pFilter->vt->Stop(This->m_pFilter); // causes weird crash ??? FIXME
	This->m_pAll->vt->Release((IUnknown*)This->m_pAll);
	This->m_pAll = 0;
    }
}

void DS_Filter_Destroy(DS_Filter* This)
{
    This->Stop(This);

    if (This->m_pOurInput)
	This->m_pOurInput->vt->Release((IUnknown*)This->m_pOurInput);
    if (This->m_pInputPin)
	This->m_pInputPin->vt->Disconnect(This->m_pInputPin);
    if (This->m_pOutputPin)
	This->m_pOutputPin->vt->Disconnect(This->m_pOutputPin);
    if (This->m_pFilter)
	This->m_pFilter->vt->Release((IUnknown*)This->m_pFilter);
    if (This->m_pOutputPin)
	This->m_pOutputPin->vt->Release((IUnknown*)This->m_pOutputPin);
    if (This->m_pInputPin)
	This->m_pInputPin->vt->Release((IUnknown*)This->m_pInputPin);
    if (This->m_pImp)
	This->m_pImp->vt->Release((IUnknown*)This->m_pImp);

    if (This->m_pOurOutput)
	This->m_pOurOutput->vt->Release((IUnknown*)This->m_pOurOutput);
    if (This->m_pParentFilter)
	This->m_pParentFilter->vt->Release((IUnknown*)This->m_pParentFilter);
    if (This->m_pSrcFilter)
	This->m_pSrcFilter->vt->Release((IUnknown*)This->m_pSrcFilter);

    // FIXME - we are still leaving few things allocated!
    if (This->m_iHandle)
	FreeLibrary((unsigned)This->m_iHandle);

    free(This);

#ifdef WIN32_LOADER
    CodecRelease();
#else
    CoUninitialize();
#endif
}

DS_Filter* DS_FilterCreate(const char* dllname, const GUID* id,
			   AM_MEDIA_TYPE* in_fmt,
			   AM_MEDIA_TYPE* out_fmt)
{
    int init = 0;
//    char eb[250];
    const char* em = NULL;
    HRESULT result;
    DS_Filter* This = (DS_Filter*) malloc(sizeof(DS_Filter));
    if (!This)
	return NULL;

#ifdef WIN32_LOADER
    CodecAlloc();
#else
    CoInitialize(0L);
#endif

    This->m_pFilter = NULL;
    This->m_pInputPin = NULL;
    This->m_pOutputPin = NULL;
    This->m_pSrcFilter = NULL;
    This->m_pParentFilter = NULL;
    This->m_pOurInput = NULL;
    This->m_pOurOutput = NULL;
    This->m_pAll = NULL;
    This->m_pImp = NULL;

    This->Start = DS_Filter_Start;
    This->Stop = DS_Filter_Stop;

    for (;;)
    {
	GETCLASS func;
	struct IClassFactory* factory = NULL;
	struct IUnknown* object = NULL;
	IEnumPins* enum_pins = 0;
	IPin* array[256];
	ULONG fetched;
        unsigned int i;

	This->m_iHandle = LoadLibraryA(dllname);
	if (!This->m_iHandle)
	{
	    em = "could not open DirectShow DLL";
	    break;
	}
	func = (GETCLASS)GetProcAddress((unsigned)This->m_iHandle, "DllGetClassObject");
	if (!func)
	{
	    em = "illegal or corrupt DirectShow DLL";
	    break;
	}
	result = func(id, &IID_IClassFactory, (void**)&factory);
	if (result || !factory)
	{
	    em = "no such class object";
	    break;
	}
	result = factory->vt->CreateInstance(factory, 0, &IID_IUnknown, (void**)&object);
	factory->vt->Release((IUnknown*)factory);
	if (result || !object)
	{
	    em = "class factory failure";
	    break;
	}
	result = object->vt->QueryInterface(object, &IID_IBaseFilter, (void**)&This->m_pFilter);
	object->vt->Release((IUnknown*)object);
	if (result || !This->m_pFilter)
	{
	    em = "object does not provide IBaseFilter interface";
            break;
	}
	// enumerate pins
	result = This->m_pFilter->vt->EnumPins(This->m_pFilter, &enum_pins);
	if (result || !enum_pins)
	{
	    em = "could not enumerate pins";
            break;
	}

	enum_pins->vt->Reset(enum_pins);
	result = enum_pins->vt->Next(enum_pins, (ULONG)256, (IPin**)array, &fetched);
	Debug printf("Pins enumeration returned %ld pins, error is %x\n", fetched, (int)result);

	for (i = 0; i < fetched; i++)
	{
	    int direction = -1;
	    array[i]->vt->QueryDirection(array[i], (PIN_DIRECTION*)&direction);
	    if (!This->m_pInputPin && direction == 0)
	    {
		This->m_pInputPin = array[i];
		This->m_pInputPin->vt->AddRef((IUnknown*)This->m_pInputPin);
	    }
	    if (!This->m_pOutputPin && direction == 1)
	    {
		This->m_pOutputPin = array[i];
		This->m_pOutputPin->vt->AddRef((IUnknown*)This->m_pOutputPin);
	    }
	    array[i]->vt->Release((IUnknown*)(array[i]));
	}
	if (!This->m_pInputPin)
	{
	    em = "could not find input pin";
            break;
	}
	if (!This->m_pOutputPin)
	{
	    em = "could not find output pin";
            break;
	}
	result = This->m_pInputPin->vt->QueryInterface((IUnknown*)This->m_pInputPin,
						       &IID_IMemInputPin,
						       (void**)&This->m_pImp);
	if (result)
	{
	    em = "could not get IMemInputPin interface";
	    break;
	}

	This->m_pOurType = in_fmt;
	This->m_pDestType = out_fmt;
        result = This->m_pInputPin->vt->QueryAccept(This->m_pInputPin, This->m_pOurType);
	if (result)
	{
	    em = "source format is not accepted";
            break;
	}
	This->m_pParentFilter = CBaseFilter2Create();
	This->m_pSrcFilter = CBaseFilterCreate(This->m_pOurType, This->m_pParentFilter);
	This->m_pOurInput = This->m_pSrcFilter->GetPin(This->m_pSrcFilter);
	This->m_pOurInput->vt->AddRef((IUnknown*)This->m_pOurInput);

	result = This->m_pInputPin->vt->ReceiveConnection(This->m_pInputPin,
							  This->m_pOurInput,
							  This->m_pOurType);
	if (result)
	{
	    em = "could not connect to input pin";
            break;
	}

	This->m_pOurOutput = COutputPinCreate(This->m_pDestType);

	result = This->m_pOutputPin->vt->ReceiveConnection(This->m_pOutputPin,
							   (IPin*) This->m_pOurOutput,
							   This->m_pDestType);
	if (result)
	{
	    em = "could not connect to output pin";
            break;
	}

	init++;
        break;
    }

    if (!init)
    {
	DS_Filter_Destroy(This);
	printf("Warning: DS_Filter() %s.  (DLL=%.200s, r=0x%x)\n", em, dllname, result);
        This = 0;
    }
    return This;
}

#include "control.h"

#ifndef __aop_h__
#define __aop_h__

struct af_instance_s;

// Audio data chunk
typedef struct af_data_s
{
  void* audio;  // data buffer
  int len;      // buffer length
  int rate;	// sample rate
  int nch;	// number of channels
  int format;	// format
  int bps; 	// bytes per sample
} af_data_t;

// Fraction, used to calculate buffer lengths
typedef struct frac_s
{
  int n; // Numerator
  int d; // Denominator
} frac_t;

// Flags used for defining the behavour of an audio filter
#define AF_FLAGS_REENTRANT 	0x00000000
#define AF_FLAGS_NOT_REENTRANT 	0x00000001

/* Audio filter information not specific for current instance, but for
   a specific filter */ 
typedef struct af_info_s 
{
  const char *info;
  const char *name;
  const char *author;
  const char *comment;
  const int flags;
  int (*open)(struct af_instance_s* vf);
} af_info_t;

// Linked list of audio filters
typedef struct af_instance_s
{
  af_info_t* info;
  int (*control)(struct af_instance_s* af, int cmd, void* arg);
  void (*uninit)(struct af_instance_s* af);
  af_data_t* (*play)(struct af_instance_s* af, af_data_t* data);
  void* setup;	  // setup data for this specific instance and filter
  af_data_t* data; // configuration for outgoing data stream
  struct af_instance_s* next;
  struct af_instance_s* prev;  
  double delay; // Delay caused by the filter [ms]
  frac_t mul; /* length multiplier: how much does this instance change
		 the length of the buffer. */
}af_instance_t;

// Initialization flags
#define AF_INIT_AUTO		0x00000000
#define AF_INIT_SLOW		0x00000001
#define AF_INIT_FAST		0x00000002
#define AF_INIT_FORCE	  	0x00000003
#define AF_INIT_TYPE_MASK 	0x00000003

// Configuration switches
typedef struct af_cfg_s{
  int force;	// Initialization type
  char** list;	/* list of names of plugins that are added to filter
		   list during first initialization of stream */
}af_cfg_t;

// Current audio stream
typedef struct af_stream_s
{
  // The first and last filter in the list
  af_instance_t* first;
  af_instance_t* last;
  // Storage for input and output data formats
  af_data_t input;
  af_data_t output;
  // Cofiguration for this stream
  af_cfg_t cfg;
}af_stream_t;

/*********************************************
// Return values
*/

#define AF_DETACH   2
#define AF_OK       1
#define AF_TRUE     1
#define AF_FALSE    0
#define AF_UNKNOWN -1
#define AF_ERROR   -2
#define AF_NA      -3



/*********************************************
// Export functions
*/

/* Initialize the stream "s". This function creates a new fileterlist
   if nessesary according to the values set in input and output. Input
   and output should contain the format of the current movie and the
   formate of the preferred output respectively. The function is
   reentreant i.e. if called wit an already initialized stream the
   stream will be reinitialized. The return value is 0 if sucess and
   -1 if failure */
int af_init(af_stream_t* s);

// Uninit and remove all filters
void af_uninit(af_stream_t* s);

/* Add filter during execution. This function adds the filter "name"
   to the stream s. The filter will be inserted somewhere nice in the
   list of filters. The return value is a pointer to the new filter,
   If the filter couldn't be added the return value is NULL. */
af_instance_t* af_add(af_stream_t* s, char* name);

// Uninit and remove the filter "af"
void af_remove(af_stream_t* s, af_instance_t* af);

/* Find filter in the dynamic filter list using it's name This
   function is used for finding already initialized filters */
af_instance_t* af_get(af_stream_t* s, char* name);

// Filter data chunk through the filters in the list
af_data_t* af_play(af_stream_t* s, af_data_t* data);

/* Calculate how long the output from the filters will be given the
   input length "len". The calculated length is >= the actual
   length */
int af_outputlen(af_stream_t* s, int len);

/* Calculate how long the input to the filters should be to produce a
   certain output length, i.e. the return value of this function is
   the input length required to produce the output length "len". The
   calculated length is <= the actual length */
int af_inputlen(af_stream_t* s, int len);

/* Calculate how long the input IN to the filters should be to produce
   a certain output length OUT but with the following three constraints:
   1. IN <= max_insize, where max_insize is the maximum possible input
      block length
   2. OUT <= max_outsize, where max_outsize is the maximum possible
      output block length
   3. If possible OUT >= len. 
   Return -1 in case of error */ 
int af_calc_insize_constrained(af_stream_t* s, int len,
			       int max_outsize,int max_insize);

/* Calculate the total delay caused by the filters */
double af_calc_delay(af_stream_t* s);

// Helper functions and macros used inside the audio filters

/* Helper function called by the macro with the same name only to be
   called from inside filters */
int af_resize_local_buffer(af_instance_t* af, af_data_t* data);

/* Helper function used to calculate the exact buffer length needed
   when buffers are resized. The returned length is >= than what is
   needed */
int af_lencalc(frac_t mul, af_data_t* data);

/* Memory reallocation macro: if a local buffer is used (i.e. if the
   filter doesn't operate on the incoming buffer this macro must be
   called to ensure the buffer is big enough. */
#define RESIZE_LOCAL_BUFFER(a,d)\
((a->data->len < af_lencalc(a->mul,d))?af_resize_local_buffer(a,d):AF_OK)

/* Some other useful macro definitions*/
#ifndef min
#define min(a,b)(((a)>(b))?(b):(a))
#endif

#ifndef max
#define max(a,b)(((a)>(b))?(a):(b))
#endif

#ifndef clamp
#define clamp(a,min,max) (((a)>(max))?(max):(((a)<(min))?(min):(a)))
#endif

#ifndef sign
#define sign(a) (((a)>0)?(1):(-1)) 
#endif

#ifndef lround
#define lround(a,b) ((b)((a)>=0.0?(a)+0.5:(a)-0.5))
#endif

#endif

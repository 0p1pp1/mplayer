#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include "../config.h"
#include "../mp_msg.h"

#include "af.h"

// Static list of filters
extern af_info_t af_info_dummy;
extern af_info_t af_info_delay;
extern af_info_t af_info_channels;
extern af_info_t af_info_format;
extern af_info_t af_info_resample;
extern af_info_t af_info_volume;
extern af_info_t af_info_equalizer;

static af_info_t* filter_list[]={ \
   &af_info_dummy,\
   &af_info_delay,\
   &af_info_channels,\
   &af_info_format,\
   &af_info_resample,\
   &af_info_volume,\
   &af_info_equalizer,\
   NULL \
};

/* Find a filter in the static list of filters using it's name. This
   function is used internally */
af_info_t* af_find(char*name)
{
  int i=0;
  while(filter_list[i]){
    if(!strcmp(filter_list[i]->name,name))
      return filter_list[i];
    i++;
  }
  mp_msg(MSGT_AFILTER,MSGL_ERR,"Couldn't find audio filter '%s'\n",name);
  return NULL;
} 

/* Find filter in the dynamic filter list using it's name This
   function is used for finding already initialized filters */
af_instance_t* af_get(af_stream_t* s, char* name)
{
  af_instance_t* af=s->first; 
  // Find the filter
  while(af != NULL){
    if(!strcmp(af->info->name,name))
      return af;
    af=af->next;
  }
  return NULL;
}

/*/ Function for creating a new filter of type name. The name may
  contain the commandline parameters for the filter */
af_instance_t* af_create(af_stream_t* s, char* name)
{
  char* cmdline = name;
  char* delim   = "=";

  // Allocate space for the new filter and reset all pointers
  af_instance_t* new=malloc(sizeof(af_instance_t));
  if(!new){
    mp_msg(MSGT_AFILTER,MSGL_ERR,"Could not allocate memory\n");
    return NULL;
  }  
  memset(new,0,sizeof(af_instance_t));

  // Check for commandline parameters
  strsep(&cmdline, delim);

  // Find filter from name
  if(NULL == (new->info=af_find(name)))
    return NULL;

  /* Make sure that the filter is not already in the list if it is
     non-reentrant */
  if(new->info->flags & AF_FLAGS_NOT_REENTRANT){
    if(af_get(s,name)){
      mp_msg(MSGT_AFILTER,MSGL_ERR,"There can only be one instance of the filter '%s' in each stream\n",name);  
      free(new);
      return NULL;
    }
  }
  
  mp_msg(MSGT_AFILTER,MSGL_V,"Adding filter %s \n",name);
  
  // Initialize the new filter
  if(AF_OK == new->info->open(new) && 
     AF_ERROR < new->control(new,AF_CONTROL_POST_CREATE,&s->cfg)){
    if(cmdline){
      if(AF_ERROR<new->control(new,AF_CONTROL_COMMAND_LINE,cmdline))
	return new;
    }
    else
      return new; 
  }
  
  free(new);
  mp_msg(MSGT_AFILTER,MSGL_ERR,"Couldn't create or open audio filter '%s'\n",name);  
  return NULL;
}

/* Create and insert a new filter of type name before the filter in the
   argument. This function can be called during runtime, the return
   value is the new filter */
af_instance_t* af_prepend(af_stream_t* s, af_instance_t* af, char* name)
{
  // Create the new filter and make sure it is OK
  af_instance_t* new=af_create(s,name);
  if(!new)
    return NULL;
  // Update pointers
  new->next=af;
  if(af){
    new->prev=af->prev;
    af->prev=new;
  }
  else
    s->last=new;
  if(new->prev)
    new->prev->next=new;
  else
    s->first=new;
  return new;
}

/* Create and insert a new filter of type name after the filter in the
   argument. This function can be called during runtime, the return
   value is the new filter */
af_instance_t* af_append(af_stream_t* s, af_instance_t* af, char* name)
{
  // Create the new filter and make sure it is OK
  af_instance_t* new=af_create(s,name);
  if(!new)
    return NULL;
  // Update pointers
  new->prev=af;
  if(af){
    new->next=af->next;
    af->next=new;
  }
  else
    s->first=new;
  if(new->next)
    new->next->prev=new;
  else
    s->last=new;
  return new;
}

// Uninit and remove the filter "af"
void af_remove(af_stream_t* s, af_instance_t* af)
{
  if(!af) return;

  // Notify filter before changing anything
  af->control(af,AF_CONTROL_PRE_DESTROY,0);

  // Detach pointers
  if(af->prev)
    af->prev->next=af->next;
  else
    s->first=af->next;
  if(af->next)
    af->next->prev=af->prev;
  else
    s->last=af->prev;

  // Uninitialize af and free memory   
  af->uninit(af);
  free(af);
}

/* Reinitializes all filters downstream from the filter given in the
   argument the return value is AF_OK if success and AF_ERROR if
   failure */
int af_reinit(af_stream_t* s, af_instance_t* af)
{
  if(!af)
    return AF_ERROR;

  do{
    af_data_t in; // Format of the input to current filter
    int rv=0; // Return value

    // Check if this is the first filter 
    if(!af->prev) 
      memcpy(&in,&(s->input),sizeof(af_data_t));
    else
      memcpy(&in,af->prev->data,sizeof(af_data_t));
    // Reset just in case...
    in.audio=NULL;
    in.len=0;
    
    rv = af->control(af,AF_CONTROL_REINIT,&in);
    switch(rv){
    case AF_OK:
      break;
    case AF_FALSE:{ // Configuration filter is needed
      af_instance_t* new = NULL;
      // Insert channels filter
      if((af->prev?af->prev->data->nch:s->input.nch) != in.nch){
	// Create channels filter
	if(NULL == (new = af_prepend(s,af,"channels")))
	  return AF_ERROR;
	// Set number of output channels
	if(AF_OK != (rv = new->control(new,AF_CONTROL_CHANNELS,&in.nch)))
	  return rv;
	// Initialize channels filter
	if(!new->prev) 
	  memcpy(&in,&(s->input),sizeof(af_data_t));
	else
	  memcpy(&in,new->prev->data,sizeof(af_data_t));
	if(AF_OK != (rv = new->control(new,AF_CONTROL_REINIT,&in)))
	  return rv;
      }
      // Insert format filter
      if(((af->prev?af->prev->data->format:s->input.format) != in.format) || 
	 ((af->prev?af->prev->data->bps:s->input.bps) != in.bps)){
	// Create format filter
	if(NULL == (new = af_prepend(s,af,"format")))
	  return AF_ERROR;
	// Set output format
	if(AF_OK != (rv = new->control(new,AF_CONTROL_FORMAT,&in)))
	  return rv;
	// Initialize format filter
	if(!new->prev) 
	  memcpy(&in,&(s->input),sizeof(af_data_t));
	else
	  memcpy(&in,new->prev->data,sizeof(af_data_t));
	if(AF_OK != (rv = new->control(new,AF_CONTROL_REINIT,&in)))
	  return rv;
      }
      if(!new) // Should _never_ happen
	return AF_ERROR;
      af=new;
      break;
    }
    case AF_DETACH:{ // Filter is redundant and wants to be unloaded
      af_instance_t* aft=af->prev;
      af_remove(s,af);
      if(aft)
	af=aft;
      else
	af=s->first; // Restart configuration
      break;
    }
    default:
      mp_msg(MSGT_AFILTER,MSGL_ERR,"Reinitialization did not work, audio filter '%s' returned error code %i\n",af->info->name,rv);
      return AF_ERROR;
    }
    af=af->next;
  }while(af);
  return AF_OK;
}

// Uninit and remove all filters
void af_uninit(af_stream_t* s)
{
  while(s->first)
    af_remove(s,s->first);
}

/* Initialize the stream "s". This function creates a new filter list
   if necessary according to the values set in input and output. Input
   and output should contain the format of the current movie and the
   formate of the preferred output respectively. The function is
   reentrant i.e. if called with an already initialized stream the
   stream will be reinitialized. The return value is 0 if success and
   -1 if failure */
int af_init(af_stream_t* s)
{
  int i=0;

  // Sanity check
  if(!s) return -1;

  // Precaution in case caller is misbehaving
  s->input.audio  = s->output.audio  = NULL;
  s->input.len    = s->output.len    = 0;

  // Figure out how fast the machine is
  if(AF_INIT_AUTO == (AF_INIT_TYPE_MASK & s->cfg.force)){
#    if defined(HAVE_SSE) || defined(HAVE_3DNOWEX)
    s->cfg.force = (s->cfg.force & ~AF_INIT_TYPE_MASK) | AF_INIT_FAST;
#    else
    s->cfg.force = (s->cfg.force & ~AF_INIT_TYPE_MASK) | AF_INIT_SLOW;
#    endif
  }

  // Check if this is the first call
  if(!s->first){
    // Add all filters in the list (if there are any)
    if(!s->cfg.list){      // To make automatic format conversion work
      if(!af_append(s,s->first,"dummy")) 
	return -1; 
    }
    else{
      while(s->cfg.list[i]){
	if(!af_append(s,s->last,s->cfg.list[i++]))
	  return -1;
      }
    }
  }
  
  // Init filters 
  if(AF_OK != af_reinit(s,s->first))
    return -1;

  // Check output format
  if((AF_INIT_TYPE_MASK & s->cfg.force) != AF_INIT_FORCE){
    af_instance_t* af = NULL; // New filter
    // Check output frequency if not OK fix with resample
    if(s->last->data->rate!=s->output.rate){
      if(NULL==(af=af_get(s,"resample"))){
	if((AF_INIT_TYPE_MASK & s->cfg.force) == AF_INIT_SLOW){
	  if(!strcmp(s->first->info->name,"format"))
	    af = af_append(s,s->first,"resample");
	  else
	    af = af_prepend(s,s->first,"resample");
	}		
	else{
	  if(!strcmp(s->last->info->name,"format"))
	    af = af_prepend(s,s->last,"resample");
	  else
	    af = af_append(s,s->last,"resample");
	}
      }
      // Init the new filter
      if(!af || (AF_OK != af->control(af,AF_CONTROL_RESAMPLE,&(s->output.rate))))
	return -1;
      if(AF_OK != af_reinit(s,af))
      	return -1;
    }	
      
    // Check number of output channels fix if not OK
    // If needed always inserted last -> easy to screw up other filters
    if(s->last->data->nch!=s->output.nch){
      if(!strcmp(s->last->info->name,"format"))
	af = af_prepend(s,s->last,"channels");
      else
	af = af_append(s,s->last,"channels");
      // Init the new filter
      if(!af || (AF_OK != af->control(af,AF_CONTROL_CHANNELS,&(s->output.nch))))
	return -1;
      if(AF_OK != af_reinit(s,af))
	return -1;
    }
    
    // Check output format fix if not OK
    if((s->last->data->format != s->output.format) || 
       (s->last->data->bps != s->output.bps)){
      if(strcmp(s->last->info->name,"format"))
	af = af_append(s,s->last,"format");
      else
	af = s->last;
      // Init the new filter
      if(!af ||(AF_OK != af->control(af,AF_CONTROL_FORMAT,&(s->output))))
	return -1;
      if(AF_OK != af_reinit(s,af))
	return -1;
    }

    // Re init again just in case
    if(AF_OK != af_reinit(s,s->first))
      return -1;

    if((s->last->data->format != s->output.format) || 
       (s->last->data->bps    != s->output.bps)    ||
       (s->last->data->nch    != s->output.nch)    || 
       (s->last->data->rate   != s->output.rate))  {
      // Something is stuffed audio out will not work 
      mp_msg(MSGT_AFILTER,MSGL_ERR,"Unable to setup filter system can not meet sound-card demands, please report this error on MPlayer development mailing list. \n");
      af_uninit(s);
      return -1;
    }
  }
  return 0;
}

/* Add filter during execution. This function adds the filter "name"
   to the stream s. The filter will be inserted somewhere nice in the
   list of filters. The return value is a pointer to the new filter,
   If the filter couldn't be added the return value is NULL. */
af_instance_t* af_add(af_stream_t* s, char* name){
  af_instance_t* new;
  // Sanity check
  if(!s || !s->first || !name)
    return NULL;
  // Insert the filter somwhere nice
  if(!strcmp(s->first->info->name,"format"))
    new = af_append(s, s->first, name);
  else
    new = af_prepend(s, s->first, name);
  if(!new)
    return NULL;

  // Reinitalize the filter list
  if(AF_OK != af_reinit(s, s->first)){
    free(new);
    return NULL;
  }
  return new;
}

// Filter data chunk through the filters in the list
af_data_t* af_play(af_stream_t* s, af_data_t* data)
{
  af_instance_t* af=s->first; 
  // Iterate through all filters 
  do{
    data=af->play(af,data);
    af=af->next;
  }while(af);
  return data;
}

/* Helper function used to calculate the exact buffer length needed
   when buffers are resized. The returned length is >= than what is
   needed */
inline int af_lencalc(frac_t mul, af_data_t* d){
  register int t = d->bps*d->nch;
  return t*(((d->len/t)*mul.n)/mul.d + 1);
}

/* Calculate how long the output from the filters will be given the
   input length "len". The calculated length is >= the actual
   length. */
int af_outputlen(af_stream_t* s, int len)
{
  int t = s->input.bps*s->input.nch;
  af_instance_t* af=s->first; 
  register frac_t mul = {1,1};
  // Iterate through all filters 
  do{
    mul.n *= af->mul.n;
    mul.d *= af->mul.d;
    af=af->next;
  }while(af);
  return t * (((len/t)*mul.n + 1)/mul.d);
}

/* Calculate how long the input to the filters should be to produce a
   certain output length, i.e. the return value of this function is
   the input length required to produce the output length "len". The
   calculated length is <= the actual length */
int af_inputlen(af_stream_t* s, int len)
{
  int t = s->input.bps*s->input.nch;
  af_instance_t* af=s->first; 
  register frac_t mul = {1,1};
  // Iterate through all filters 
  do{
    mul.n *= af->mul.n;
    mul.d *= af->mul.d;
    af=af->next;
  }while(af);
  return t * (((len/t) * mul.d - 1)/mul.n);
}

/* Calculate how long the input IN to the filters should be to produce
   a certain output length OUT but with the following three constraints:
   1. IN <= max_insize, where max_insize is the maximum possible input
      block length
   2. OUT <= max_outsize, where max_outsize is the maximum possible
      output block length
   3. If possible OUT >= len. 
   Return -1 in case of error */ 
int af_calc_insize_constrained(af_stream_t* s, int len,
			       int max_outsize,int max_insize)
{
  int t   = s->input.bps*s->input.nch;
  int in  = 0;
  int out = 0;
  af_instance_t* af=s->first; 
  register frac_t mul = {1,1};
  // Iterate through all filters and calculate total multiplication factor
  do{
    mul.n *= af->mul.n;
    mul.d *= af->mul.d;
    af=af->next;
  }while(af);
  in = t * (((len/t) * mul.d - 1)/mul.n);
  
  if(in>max_insize) in=t*(max_insize/t);

  // Try to meet constraint nr 3. 
  while((out=t * (((in/t+1)*mul.n - 1)/mul.d)) <= max_outsize && in<=max_insize){
    if( (t * (((in/t)*mul.n))/mul.d) >= len) return in;
    in+=t;
  }
  
  // Could no meet constraint nr 3.
  while(out > max_outsize || in > max_insize){
    in-=t;
    if(in<t) return -1; // Input parameters are probably incorrect
    out = t * (((in/t)*mul.n + 1)/mul.d);
  }
  return in;
}

/* Calculate the total delay [ms] caused by the filters */
double af_calc_delay(af_stream_t* s)
{
  af_instance_t* af=s->first; 
  register double delay = 0.0;
  // Iterate through all filters 
  while(af){
    delay += af->delay;
    af=af->next;
  }
  return delay;
}

/* Helper function called by the macro with the same name this
   function should not be called directly */
inline int af_resize_local_buffer(af_instance_t* af, af_data_t* data)
{
  // Calculate new length
  register int len = af_lencalc(af->mul,data);
  mp_msg(MSGT_AFILTER,MSGL_V,"Reallocating memory in module %s, old len = %i, new len = %i\n",af->info->name,af->data->len,len);
  // If there is a buffer free it
  if(af->data->audio) 
    free(af->data->audio);
  // Create new buffer and check that it is OK
  af->data->audio = malloc(len);
  if(!af->data->audio){
    mp_msg(MSGT_AFILTER,MSGL_ERR,"Could not allocate memory \n");
    return AF_ERROR;
  }
  af->data->len=len;
  return AF_OK;
}

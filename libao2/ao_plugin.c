#include <stdio.h>
#include <stdlib.h>

#include "../config.h"

#include "audio_out.h"
#include "audio_out_internal.h"

#include "audio_plugin.h"

static ao_info_t info = 
{
	"Plugin audio output",
	"plugin",
	"Anders",
	""
};

LIBAO_EXTERN(plugin)

#define plugin(i) (ao_plugin_local_data.plugins[i])
#define driver() (ao_plugin_local_data.driver)

#define NPL 2 //Number of PLugins

extern ao_plugin_functions_t audio_plugin_delay;

// local data 
typedef struct ao_plugin_local_data_s
{
  char* cfg_plugins;           // List of plugins read from cfg-file
  ao_functions_t* driver;      // Output driver set in mplayer.c
  ao_plugin_functions_t** plugins;               // List of used plugins
  ao_plugin_functions_t* available_plugins[NPL]; // List of abailabel plugins
} ao_plugin_local_data_t;

ao_plugin_local_data_t ao_plugin_local_data={
  NULL,
  NULL,
  NULL,
  {
    &audio_plugin_delay,
    NULL
  }
};

// gloabal data 
ao_plugin_data_t ao_plugin_data;
  

// to set/get/query special features/parameters
static int control(int cmd,int arg){
  switch(cmd){
  case AOCONTROL_SET_PLUGIN_DRIVER:
    ao_plugin_local_data.driver=(ao_functions_t*)arg;
    return CONTROL_OK;
  case AOCONTROL_SET_PLUGIN_LIST:
    ao_plugin_local_data.cfg_plugins=(char*)arg;
    return CONTROL_OK;
  default:
    return driver()->control(cmd,arg);
  }
  return CONTROL_UNKNOWN;
}

// Recursive function for adding plugins
// return 1 for success and 0 for error
int add_plugin(int i,char* cfg){
  int cnt=0;
  // Find end of plugin name
  while((cfg[cnt]!=',')&&(cfg[cnt]!='\0')&&(cnt<100)) cnt++;
  if(cnt >= 100)
    return 0;

  // Is this the last itteration or just another plugin
  if(cfg[cnt]=='\0'){
    ao_plugin_local_data.plugins=malloc((i+1)*sizeof(ao_plugin_functions_t*));
    if(ao_plugin_local_data.plugins){
      ao_plugin_local_data.plugins[i+1]=NULL;
      // Find the plugin matching the cfg string name
      cnt=0;
      while(ao_plugin_local_data.available_plugins[cnt] && cnt<20){
	if(0==strcmp(ao_plugin_local_data.available_plugins[cnt]->info->short_name,cfg)){
	  ao_plugin_local_data.plugins[i]=ao_plugin_local_data.available_plugins[cnt];
	  return 1;
	}
	cnt++;
      }
      printf("[plugin]: Invalid plugin: %s \n",cfg);
      return 0;
    }
    else 
      return 0;
  } else {
    cfg[cnt]='\0';
    if(add_plugin(i+1,&cfg[cnt+1])){
      cnt=0;
      // Find the plugin matching the cfg string name
      while(ao_plugin_local_data.available_plugins[cnt] && cnt < 20){
	if(0==strcmp(ao_plugin_local_data.available_plugins[cnt]->info->short_name,cfg)){
	  ao_plugin_local_data.plugins[i]=ao_plugin_local_data.available_plugins[cnt];
	  return 1;
	}
	cnt++;
      }
      printf("[plugin]: Invalid plugin: %s \n",cfg);
      return 0;
    }
    else 
      return 0;
  }	
  return 0; // Will never happen...
}

// open & setup audio device and plugins
// return: 1=success 0=fail
static int init(int rate,int channels,int format,int flags){
  int ok=1;

  /* Create list of plugins from cfg option */
  int i=0; 
  if(ao_plugin_local_data.cfg_plugins){
    if(!add_plugin(i,ao_plugin_local_data.cfg_plugins))
      return 0;
  }

  /* Set input parameters and itterate through plugins each plugin
     changes the parameters according to its output */
  ao_plugin_data.rate=rate;
  ao_plugin_data.channels=channels;
  ao_plugin_data.format=format;
  ao_plugin_data.sz_mult=1;
  ao_plugin_data.sz_fix=0;
  ao_plugin_data.delay_mult=1;
  ao_plugin_data.delay_fix=0;
  i=0;
  while(plugin(i)&&ok)
    ok=plugin(i++)->init();
  
  if(!ok) return 0;

  // This should never happen but check anyway 
  if(NULL==ao_plugin_local_data.driver)
    return 0;
  
  ok = driver()->init(ao_plugin_data.rate,
		      ao_plugin_data.channels,
		      ao_plugin_data.format,
		      flags);
  if(!ok) return 0;

  /* Now that the driver is initialized we can calculate and set the
     input and output buffers for each plugin */
  ao_plugin_data.len=driver()->get_space();
  while((i>0) && ok)
    ok=plugin(--i)->control(AOCONTROL_PLUGIN_SET_LEN,ao_plugin_data.len);

  if(!ok) return 0;

  return 1;
}

// close audio device
static void uninit(){
  int i=0;
  driver()->uninit();
  while(plugin(i))
    plugin(i++)->uninit();
  if(ao_plugin_local_data.plugins)
    free(ao_plugin_local_data.plugins);
}

// stop playing and empty buffers (for seeking/pause)
static void reset(){
  int i=0;
  driver()->reset();
  while(plugin(i))
    plugin(i++)->reset();
}

// stop playing, keep buffers (for pause)
static void audio_pause(){
  driver()->pause();
}

// resume playing, after audio_pause()
static void audio_resume(){
  driver()->resume();
}

// return: how many bytes can be played without blocking
static int get_space(){
  double sz=(double)(driver()->get_space());
  sz*=ao_plugin_data.sz_mult;
  sz+=ao_plugin_data.sz_fix;
  return (int)(sz);
}

// plays 'len' bytes of 'data'
// return: number of bytes played
static int play(void* data,int len,int flags){
  int i=0;
  /* Due to constant buffer sizes in plugins limit length */
  int tmp = get_space();
  int ret_len =(tmp<len)?tmp:len;
  /* Filter data */
  ao_plugin_data.len=ret_len;  
  ao_plugin_data.data=data;
  while(plugin(i))
    plugin(i++)->play();
  /* Send data to output */
  len=driver()->play(ao_plugin_data.data,ao_plugin_data.len,flags);

  if(len!=ao_plugin_data.len)
    printf("Buffer over flow in sound plugin ");
  
  return ret_len;
}

// return: delay in seconds between first and last sample in buffer
static float get_delay(){
  float delay=driver()->get_delay();
  delay*=ao_plugin_data.delay_mult;
  delay+=ao_plugin_data.delay_fix;
  return delay;
}




#include "sub.h"

//static int vo_font_loaded=-1;
font_desc_t* vo_font=NULL;

unsigned char* vo_osd_text="00:00:00";
int sub_unicode=0;
int sub_utf8=0;

inline static void vo_draw_text_osd(int dxs,int dys,void (*draw_alpha)(int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride)){
	unsigned char *cp=vo_osd_text;
	int c;
   	int font;
        int y=10;
        int x=20;

        while (*cp){
          c=*cp++;
          if ((font=vo_font->font[c])>=0)
            draw_alpha(x,y,
              vo_font->width[c],
              vo_font->pic_a[font]->h,
              vo_font->pic_b[font]->bmp+vo_font->start[c],
              vo_font->pic_a[font]->bmp+vo_font->start[c],
              vo_font->pic_a[font]->w);
          x+=vo_font->width[c]+vo_font->charspace;
        }

}

int vo_osd_progbar_type=-1;
int vo_osd_progbar_value=100;   // 0..256

// if we have n=256 bars then OSD progbar looks like below
// 
// 0  1    2    3 ...  256  <= vo_osd_progbar_value
// |  |    |    |       |
// [ ===  ===  === ... === ]
// 
//  the above schema is rescalled to n=elems bars

inline static void vo_draw_text_progbar(int dxs,int dys,void (*draw_alpha)(int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride)){
   	unsigned char *s;
   	unsigned char *sa;
        int i,w,h,st,mark;
        int y=(dys-vo_font->height)/2;
        int c,font;
        int delimw=vo_font->width[OSD_PB_START]
     		  +vo_font->width[OSD_PB_END]
     		  +vo_font->charspace;
        int width=(2*dxs-3*delimw)/3;
   	int charw=vo_font->width[OSD_PB_0]+vo_font->charspace;
        int elems=width/charw;
   	int x=(dxs-elems*charw-delimw)/2;

	if (vo_osd_progbar_value<=0)
     	   mark=0;
	else if ((mark=(vo_osd_progbar_value*elems)>>8)>elems)
	   mark=elems;

//        printf("osd.progbar  width=%d  xpos=%d\n",width,x);

        c=vo_osd_progbar_type;
        if(vo_osd_progbar_type>0 && (font=vo_font->font[c])>=0) {
	    int xp=x-vo_font->width[c]-vo_font->spacewidth;
	   draw_alpha((xp<0?0:xp),y,
              vo_font->width[c],
              vo_font->pic_a[font]->h,
              vo_font->pic_b[font]->bmp+vo_font->start[c],
              vo_font->pic_a[font]->bmp+vo_font->start[c],
              vo_font->pic_a[font]->w);
	}
   
        c=OSD_PB_START;
        if ((font=vo_font->font[c])>=0)
            draw_alpha(x,y,
              vo_font->width[c],
              vo_font->pic_a[font]->h,
              vo_font->pic_b[font]->bmp+vo_font->start[c],
              vo_font->pic_a[font]->bmp+vo_font->start[c],
              vo_font->pic_a[font]->w);
        x+=vo_font->width[c]+vo_font->charspace;

   	c=OSD_PB_0;
   	if ((font=vo_font->font[c])>=0){
	   w=vo_font->width[c];
	   h=vo_font->pic_a[font]->h;
	   s=vo_font->pic_b[font]->bmp+vo_font->start[c];
	   sa=vo_font->pic_a[font]->bmp+vo_font->start[c];
	   st=vo_font->pic_a[font]->w;
     	   for (i=mark;i--;){
	       draw_alpha(x,y,w,h,s,sa,st);
	       x+=charw;
	   }
	}

   	c=OSD_PB_1;
	if ((font=vo_font->font[c])>=0){
	   w=vo_font->width[c];
	   h=vo_font->pic_a[font]->h;
	   s =vo_font->pic_b[font]->bmp+vo_font->start[c];
	   sa=vo_font->pic_a[font]->bmp+vo_font->start[c];
	   st=vo_font->pic_a[font]->w;
     	   for (i=elems-mark;i--;){
	       draw_alpha(x,y,w,h,s,sa,st);
	       x+=charw;
	   }
	}

        c=OSD_PB_END;
        if ((font=vo_font->font[c])>=0)
            draw_alpha(x,y,
              vo_font->width[c],
              vo_font->pic_a[font]->h,
              vo_font->pic_b[font]->bmp+vo_font->start[c],
              vo_font->pic_a[font]->bmp+vo_font->start[c],
              vo_font->pic_a[font]->w);
//        x+=vo_font->width[c]+vo_font->charspace;


//        vo_osd_progbar_value=(vo_osd_progbar_value+1)&0xFF;

}

subtitle* vo_sub=NULL;

#define MAX_UCS 1600
#define MAX_UCSLINES 16


inline static void vo_draw_text_sub(int dxs,int dys,void (*draw_alpha)(int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride)){
   static int utbl[MAX_UCS+1];
   static int xtbl[MAX_UCSLINES];
   static int lines;
   static subtitle *memsub=NULL;
   static int memy;
   static int memdxs;
   static int memdys;
   
   unsigned char *t;
   int c,i,j,l,x,y,font;
   int len;
   int k,lastk;
   int lastStripPosition;
   int xsize,lastxsize;
   int h,lasth;
   
   if ((memsub!=vo_sub)||(memdxs!=dxs)||(memdys!=dys)){
      
      memsub=vo_sub;
      memdxs=dxs;
      memdys=memy=dys;
      
      // too long lines divide into a smaller ones
      i=k=lines=lasth=0;
      h=vo_font->height;
      xsize=-vo_font->charspace;
      lastStripPosition=-1;
      l=vo_sub->lines;

      while (l--){
	  t=vo_sub->text[i++];	  
	  len=strlen(t)-1;
	  
	  printf("sub(%d) '%s'\n",len,t);
//	  if(len<0) memy -=h; // according to max of vo_font->pic_a[font]->h 
//	  else
	  for (j=0;j<=len;j++){
	      if ((c=t[j])>=0x80){
		 if (sub_unicode) 
		    c = (c<<8) + t[++j]; 
		 else
		    if (sub_utf8){
		       if ((c & 0xe0) == 0xc0)    /* 2 bytes U+00080..U+0007FF*/
			  c = (c & 0x1f)<<6 | (t[++j] & 0x3f);
		       else if((c & 0xf0) == 0xe0)/* 3 bytes U+00800..U+00FFFF*/
			  c = ((c & 0x0f)<<6 |
			       (t[++j] & 0x3f))<<6 | (t[++j] & 0x3f);
		    }
	      }
	      if (k==MAX_UCS){
		 utbl[k]=l=0; break;
	      }
	      utbl[k++]=c;
	      if (c==' '){
		 lastk=k;
		 lastStripPosition=j;
		 lastxsize=xsize;
	      }
#if 1
	      else if ((font=vo_font->font[c])>=0){
		  if (vo_font->pic_a[font]->h > h){
		     h=vo_font->pic_a[font]->h;
		  }
	      }
#endif
#if 0
	      else if ((font=vo_font->font[c])>=0){
		  if ((memy-h)+vo_font->pic_a[font]->h > dys){
		     h=vo_font->pic_a[font]->h;
		  }
	      }
#endif
	      xsize+=vo_font->width[c]+vo_font->charspace;
	      if (dxs<xsize){
		 if (lastStripPosition>0){
		    j=lastStripPosition;
		    xsize=lastxsize;
		    k=lastk;
		 } else {
		    xsize -=vo_font->width[c]+vo_font->charspace; // go back
		    k--; // cut line here
		    while (t[j] && t[j]!=' ') j++; // jump to the nearest space
		 }
	      } else if (j<len)
		   continue;
	      if (h>memy){ // out of the screen so end parsing
		 memy +=vo_font->height-lasth; // correct the y position
		 l=0; break;
	      }
	      utbl[k++]=0;
	      xtbl[lines++]=(dxs-xsize)/2;
	      if (lines==MAX_UCSLINES||k>MAX_UCS){
		 l=0; break;
	      } else if(l || j<len){ // not the last line or not the last char
		 lastStripPosition=-1;
		 xsize=-vo_font->charspace;
		 lasth=h;
		 h=vo_font->height;
	      }
	      printf("h: %d -> %d  \n",vo_font->height,h);
	      memy -=h; // according to max of vo_font->pic_a[font]->h 
	  }
      }
   }
   
   y = memy;
   
//   printf("lines=%d  y=%d\n",lines,y);

   i=j=0; l=lines;
   while (i<lines){
	 x= xtbl[i++]; 
	 while ((c=utbl[j++])){
	       if ((font=vo_font->font[c])>=0 && y<dys)
		  draw_alpha(x,y,
			     vo_font->width[c],
			     vo_font->pic_a[font]->h+y<dys ? vo_font->pic_a[font]->h : dys-y,
			     vo_font->pic_b[font]->bmp+vo_font->start[c],
			     vo_font->pic_a[font]->bmp+vo_font->start[c],
			     vo_font->pic_a[font]->w);
	       x+=vo_font->width[c]+vo_font->charspace;
	 }
         y+=vo_font->height;
   }
}

static int draw_alpha_init_flag=0;

extern void vo_draw_alpha_init();

void vo_draw_text(int dxs,int dys,void (*draw_alpha)(int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride)){

    if(!vo_font) return; // no font

    if(!draw_alpha_init_flag){
	draw_alpha_init_flag=1;
	vo_draw_alpha_init();
    }

    if(vo_osd_text){
        vo_draw_text_osd(dxs,dys,draw_alpha);
    }

    if(vo_sub){
        vo_draw_text_sub(dxs,dys,draw_alpha);
    }
    
    if(vo_osd_progbar_type>=0 && vo_font->font[OSD_PB_0]>=0){
        vo_draw_text_progbar(dxs,dys,draw_alpha);
    }

}
         

/*
 * Subtitle reader with format autodetection
 *
 * Written by laaz
 * Some code cleanup & realloc() by A'rpi/ESP-team
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "subreader.h"

#define ERR (void *)-1


int sub_uses_time=0;
int sub_errs=0;
int sub_num=0;         // number of subtitle structs
int sub_format=-1;     // 0 for microdvd, 1 for SubRip, 2 for the third format


char *sub_readtext(char *source, char **dest) {
    int len=0;
    char *p;
    
    for (p=source;*p!='\r' && *p!='\n' && *p!='|'; p++,len++);
    
    *dest= (char *)malloc (len+1);
    if (!dest) {return ERR;}
    
    strncpy(*dest, source, len);
    (*dest)[len]=0;
    
    while (*p=='\r' || *p=='\n' || *p=='|') p++;
    
    if (*p) return p;  // not-last text field
    else return NULL;  // last text field
}



subtitle *sub_read_line_microdvd(FILE *fd,subtitle *current) {
    char line[1001];
    char line2[1001];
    char *p, *next;
    int i;

    bzero (current, sizeof(current));

    do {
	if (!fgets (line, 1000, fd)) return NULL;
    } while (*line=='\n' || *line == '\r' || !*line);
    
    if (sscanf (line, "{%d}{%d}%s", &(current->start), &(current->end),line2) <2) {return ERR;}

    p=line;
    while (*p++!='}');
    while (*p++!='}');

    next=p, i=0;
    while ((next =sub_readtext (next, &(current->text[i])))) {
        if (current->text[i]==ERR) {return ERR;}
	i++;
	if (i>SUB_MAX_TEXT) { printf ("Too many lines in a subtitle\n");current->lines=i;return;}
    }
    current->lines=i+1;

    return current;
}

subtitle *sub_read_line_subrip(FILE *fd, subtitle *current) {
    char line[1001];
    int a1,a2,a3,a4,b1,b2,b3,b4;
    char *p=NULL, *q=NULL;
    int len;
    
    bzero (current, sizeof(current));
    
    while (!current->text[0]) {
	if (!fgets (line, 1000, fd)) return NULL;
	if (sscanf (line, "%d:%d:%d.%d,%d:%d:%d.%d",&a1,&a2,&a3,&a4,&b1,&b2,&b3,&b4) < 8) continue;
	current->start = a1*360000+a2*6000+a3*100+a4;
	current->end   = b1*360000+b2*6000+b3*100+b4;

	if (!fgets (line, 1000, fd)) return NULL;

	p=q=line;
	for (current->lines=1; current->lines < SUB_MAX_TEXT; current->lines++) {
	    for (q=p,len=0; *p && *p!='\r' && *p!='\n' && strncmp(p,"[br]",4); p++,len++);
	    current->text[current->lines-1]=(char *)malloc (len+1);
	    if (!current->text[current->lines-1]) return ERR;
	    strncpy (current->text[current->lines-1], q, len);
	    current->text[current->lines-1][len]='\0';
	    if (!*p || *p=='\r' || *p=='\n') break;
	    while (*p++!=']');
	}
    }
    return current;
}

subtitle *sub_read_line_third(FILE *fd,subtitle *current) {
    char line[1001];
    int a1,a2,a3,a4,b1,b2,b3,b4;
    char *p=NULL;
    int i,len;
    
    bzero (current, sizeof(current));
    
    while (!current->text[0]) {
	if (!fgets (line, 1000, fd)) return NULL;
	if ((len=sscanf (line, "%d:%d:%d,%d --> %d:%d:%d,%d",&a1,&a2,&a3,&a4,&b1,&b2,&b3,&b4)) < 8)
	    continue;
	current->start = a1*360000+a2*6000+a3*100+a4/10;
	current->end   = b1*360000+b2*6000+b3*100+b4/10;
	for (i=0; i<SUB_MAX_TEXT;) {
	    if (!fgets (line, 1000, fd)) break;
	    len=0;
	    for (p=line; *p!='\n' && *p!='\r' && *p; p++,len++);
	    if (len) {
		current->text[i]=(char *)malloc (len+1);
		if (!current->text[i]) return ERR;
		strncpy (current->text[i], line, len); current->text[i][len]='\0';
		i++;
	    } else {
		break;
	    }
	}
	current->lines=i;
    }
    return current;
}


int sub_autodetect (FILE *fd) {
    char line[1001];
    int i,j=0;
//    char *p;
    
    while (1) {
	j++;
	if (!fgets (line, 1000, fd))
	    return -1;

//	if (sscanf (line, "{%i}{%i}", &i, &i, p)==2) // ha valaki tudja miert 2, mondja mar el nekem ;)
	if (sscanf (line, "{%d}{%d}", &i, &i)==2) // ha valaki tudja miert 2, mondja mar el nekem ;)
		{sub_uses_time=0;return 0;}
	if (sscanf (line, "%d:%d:%d.%d,%d:%d:%d.%d",     &i, &i, &i, &i, &i, &i, &i, &i)==8)
		{sub_uses_time=1;return 1;}
	if (sscanf (line, "%d:%d:%d,%d --> %d:%d:%d,%d", &i, &i, &i, &i, &i, &i, &i, &i)==8)
		{sub_uses_time=1;return 2;}
	if (j>100) return -1;  // too many bad lines or bad coder
    }
}


subtitle* sub_read_file (char *filename) {
    FILE *fd;
    int n_max;
    subtitle *first;
    subtitle * (*func[3])(FILE *fd,subtitle *dest)=
    {
	    sub_read_line_microdvd,
	    sub_read_line_subrip,
	    sub_read_line_third
    };

    fd=fopen (filename, "r"); if (!fd) return NULL;

    sub_format=sub_autodetect (fd);
    if (sub_format==-1) {printf ("SUB: Could not determine file format\n");return NULL;}
    printf ("SUB: Detected subtitle file format: %i\n",sub_format);
    
    rewind (fd);

    sub_num=0;n_max=32;
    first=(subtitle *)malloc(n_max*sizeof(subtitle));
    if(!first) return NULL;
    
    while(1){
        subtitle *sub;
        if(sub_num>=n_max){
            n_max+=16;
            first=realloc(first,n_max*sizeof(subtitle));
        }
        sub=func[sub_format](fd,&first[sub_num]);
        if(!sub) break;   // EOF
        if(sub==ERR) ++sub_errs; else ++sub_num; // Error vs. Valid
    }
    
    fclose(fd);

//    printf ("SUB: Subtitle format %s time.\n", sub_uses_time?"uses":"doesn't use");
    printf ("SUB: Read %i subtitles", sub_num);
    if (sub_errs) printf (", %i error(s).\n", sub_errs);
    else 	  printf (".\n");

    return first;
}

#if 0
int main(int argc, char **argv) {  // for testing

    int i,j;
    subtitle *subs;
    subtitle *egysub;
    
    if(argc<2){
        printf("\nUsage: subreader filename.sub\n\n");
        exit(1);
    }
    
    subs=sub_get_subtitles(argv[1]);
    if(!subs){
        printf("Couldn't load file... let's write a bugreport :)\n");
        exit(1);
    }

    for(j=0;j<sub_num;j++){
	egysub=&subs[j];
        printf ("%i line%c (%i-%i) ",
		    egysub->lines,
		    (1==egysub->lines)?' ':'s',
		    egysub->start,
		    egysub->end);
	for (i=0; i<egysub->lines; i++) {
	    printf ("%s%s",egysub->text[i], i==egysub->lines-1?"":" <BREAK> ");
	}
	printf ("\n");
    }

    printf ("Subtitle format %s time.\n", sub_uses_time?"uses":"doesn't use");
    printf ("Read %i subtitles, %i errors.\n", sub_num, sub_errs);
    return 0;
}
#endif

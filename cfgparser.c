/*
 * command line and config file parser
 * by Szabolcs Berecz <szabi@inf.elte.hu>
 * (C) 2001
 */

//#define DEBUG

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>


#define COMMAND_LINE		0
#define CONFIG_FILE		1

#define MAX_RECURSION_DEPTH	8

#ifdef DEBUG
#include <assert.h>
#endif

#include "cfgparser.h"

static struct config *config;
static int nr_options;		/* number of options in 'conf' */
static int parser_mode;		/* COMMAND_LINE or CONFIG_FILE */
static int recursion_depth = 0;

static int init_conf(struct config *conf, int mode)
{
#ifdef DEBUG
	assert(conf != NULL);
#endif

	/* calculate the number of options in 'conf' */
	for (nr_options = 0; conf[nr_options].name != NULL; nr_options++)
		/* NOTHING */;

	config = conf;
#ifdef DEBUG
	if (mode != COMMAND_LINE && mode != CONFIG_FILE) {
		printf("init_conf: wrong mode!\n");
		return -1;
	}
#endif
	parser_mode = mode;
	return 1;
}

static int read_option(char *opt, char *param)
{
	int i;
	long tmp_int;
	double tmp_float;
	int ret = -1;
	char *endptr;

	for (i = 0; i < nr_options; i++) {
		int namelength;
		/* allow 'aa*' in config.name */
		namelength=strlen(config[i].name);
		if ( (config[i].name[namelength-1]=='*') && 
			    !memcmp(opt, config[i].name, namelength-1))
		        break;
	    
	    
		if (!strcasecmp(opt, config[i].name))
			break;
	}
	if (i == nr_options) {
		printf("invalid option:\n");
		ret = ERR_NOT_AN_OPTION;
		goto out;
	}
	if (config[i].flags & CONF_NOCFG && parser_mode == CONFIG_FILE) {
		printf("this option can only be used on command line:\n");
		ret = ERR_NOT_AN_OPTION;
		goto out;
	}
	if (config[i].flags & CONF_NOCMD && parser_mode == COMMAND_LINE) {
		printf("this option can only be used in config file:\n");
		ret = ERR_NOT_AN_OPTION;
		goto out;
	}

	switch (config[i].type) {
		case CONF_TYPE_FLAG:
			/* flags need a parameter in config file */
			if (parser_mode == CONFIG_FILE) {
				if (!strcasecmp(param, "yes") ||	/* any other language? */
				    !strcasecmp(param, "ja") ||
				    !strcasecmp(param, "si") ||
				    !strcasecmp(param, "igen") ||
				    !strcasecmp(param, "y") ||
				    !strcasecmp(param, "j") ||
				    !strcasecmp(param, "i") ||
				    !strcmp(param, "1"))
					*((int *) config[i].p) = config[i].max;
				else if (!strcasecmp(param, "no") ||
				    !strcasecmp(param, "nein") ||
				    !strcasecmp(param, "nicht") ||
				    !strcasecmp(param, "nem") ||
				    !strcasecmp(param, "n") ||
				    !strcmp(param, "0"))
					*((int *) config[i].p) = config[i].min;
				else {
					printf("invalid parameter for flag:\n");
					ret = ERR_OUT_OF_RANGE;
					goto out;
				}
				ret = 1;
			} else {	/* parser_mode == COMMAND_LINE */
				*((int *) config[i].p) = config[i].max;
				ret = 0;
			}
			break;
		case CONF_TYPE_INT:
			if (param == NULL)
				goto err_missing_param;

			tmp_int = strtol(param, &endptr, 0);
			if (*endptr) {
				printf("parameter must be an integer:\n");
				ret = ERR_OUT_OF_RANGE;
				goto out;
			}

			if (config[i].flags & CONF_MIN)
				if (tmp_int < config[i].min) {
					printf("parameter must be >= %d:\n", (int) config[i].min);
					ret = ERR_OUT_OF_RANGE;
					goto out;
				}

			if (config[i].flags & CONF_MAX)
				if (tmp_int > config[i].max) {
					printf("parameter must be <= %d:\n", (int) config[i].max);
					ret = ERR_OUT_OF_RANGE;
					goto out;
				}

			*((int *) config[i].p) = tmp_int;
			ret = 1;
			break;
		case CONF_TYPE_FLOAT:
			if (param == NULL)
				goto err_missing_param;

			tmp_float = strtod(param, &endptr);

			if ((*endptr == ':') || (*endptr == '/'))
				tmp_float /= strtod(endptr+1, &endptr);

			if (*endptr) {
				printf("parameter must be a floating point number"
				       " or a ratio (numerator[:/]denominator):\n");

				ret = ERR_MISSING_PARAM;
				goto out;
			}

			if (config[i].flags & CONF_MIN)
				if (tmp_float < config[i].min) {
					printf("parameter must be >= %f:\n", config[i].min);
					ret = ERR_OUT_OF_RANGE;
					goto out;
				}

			if (config[i].flags & CONF_MAX)
				if (tmp_float > config[i].max) {
					printf("parameter must be <= %f:\n", config[i].max);
					ret = ERR_OUT_OF_RANGE;
					goto out;
				}

			*((float *) config[i].p) = tmp_float;
			ret = 1;
			break;
		case CONF_TYPE_STRING:
			if (param == NULL)
				goto err_missing_param;

			if (config[i].flags & CONF_MIN)
				if (strlen(param) < config[i].min) {
					printf("parameter must be >= %d chars:\n",
							(int) config[i].min);
					ret = ERR_OUT_OF_RANGE;
					goto out;
				}

			if (config[i].flags & CONF_MAX)
				if (strlen(param) > config[i].max) {
					printf("parameter must be <= %d chars:\n",
							(int) config[i].max);
					ret = ERR_OUT_OF_RANGE;
					goto out;
				}

			*((char **) config[i].p) = strdup(param);
			ret = 1;
			break;
		case CONF_TYPE_FUNC_PARAM:
			if (param == NULL)
				goto err_missing_param;
			if ((((cfg_func_param_t) config[i].p)(config + i, param)) < 0) {
				ret = ERR_FUNC_ERR;
				goto out;
			}
			ret = 1;
			break;
		case CONF_TYPE_FUNC_FULL:
			if (param!=NULL && param[0]=='-'){
			    ret=((cfg_func_arg_param_t) config[i].p)(config + i, opt, NULL);
			    if (ret>=0) ret=0;
			    /* if we return >=0: param is processed again (if there is any) */
			}else{
			    ret=((cfg_func_arg_param_t) config[i].p)(config + i, opt, param);
			    /* if we return 0: need no param, precess it again */
			    /* if we return 1: accepted param */
			}
			break;
		case CONF_TYPE_FUNC:
			if ((((cfg_func_t) config[i].p)(config + i)) < 0) {
				ret = ERR_FUNC_ERR;
				goto out;
			}
			ret = 0;
			break;
		case CONF_TYPE_PRINT:
			printf("%s", (char *) config[i].p);
			exit(1);
		default:
			printf("picsaba\n");
			break;
	}
out:
	return ret;
err_missing_param:
	printf("missing parameter:\n");
	ret = ERR_MISSING_PARAM;
	goto out;
}

int parse_config_file(struct config *conf, char *conffile)
{
#define PRINT_LINENUM	printf("%s(%d): ", conffile, line_num)
#define MAX_LINE_LEN	1000
#define MAX_OPT_LEN	100
#define MAX_PARAM_LEN	100
	FILE *fp;
	char *line;
	char opt[MAX_OPT_LEN + 1];
	char param[MAX_PARAM_LEN + 1];
	char c;		/* for the "" and '' check */
	int tmp;
	int line_num = 0;
	int line_pos;	/* line pos */
	int opt_pos;	/* opt pos */
	int param_pos;	/* param pos */
	int ret = 1;
	int errors = 0;

#ifdef DEBUG
	assert(conffile != NULL);
#endif
	if (++recursion_depth > 1)
		printf("Reading config file: %s", conffile);

	if (recursion_depth > MAX_RECURSION_DEPTH) {
		printf(": too deep 'include'. check your configfiles\n");
		ret = -1;
		goto out;
	}

	if (init_conf(conf, CONFIG_FILE) == -1) {
		ret = -1;
		goto out;
	}

	if ((line = (char *) malloc(MAX_LINE_LEN + 1)) == NULL) {
		perror("\ncan't get memory for 'line'");
		ret = -1;
		goto out;
	}

	if ((fp = fopen(conffile, "r")) == NULL) {
		if (recursion_depth > 1)
			printf(": %s\n", strerror(errno));
		free(line);
		ret = 0;
		goto out;
	}
	if (recursion_depth > 1)
		printf("\n");

	while (fgets(line, MAX_LINE_LEN, fp)) {
		if (errors >= 16) {
			printf("too many errors\n");
			goto out;
		}

		line_num++;
		line_pos = 0;

		/* skip whitespaces */
		while (isspace(line[line_pos]))
			++line_pos;

		/* EOL / comment */
		if (line[line_pos] == '\0' || line[line_pos] == '#')
			continue;

		/* read option. */
		for (opt_pos = 0; isprint(line[line_pos]) &&
				line[line_pos] != ' ' &&
				line[line_pos] != '#' &&
				line[line_pos] != '='; /* NOTHING */) {
			opt[opt_pos++] = line[line_pos++];
			if (opt_pos >= MAX_OPT_LEN) {
				PRINT_LINENUM;
				printf("too long option\n");
				errors++;
				ret = -1;
				goto nextline;
			}
		}
		if (opt_pos == 0) {
			PRINT_LINENUM;
			printf("parse error\n");
			ret = -1;
			errors++;
			continue;
		}
		opt[opt_pos] = '\0';
#ifdef DEBUG
		PRINT_LINENUM;
		printf("option: %s\n", opt);
#endif

		/* skip whitespaces */
		while (isspace(line[line_pos]))
			++line_pos;

		/* check '=' */
		if (line[line_pos++] != '=') {
			PRINT_LINENUM;
			printf("option without parameter\n");
			ret = -1;
			errors++;
			continue;
		}

		/* whitespaces... */
		while (isspace(line[line_pos]))
			++line_pos;

		/* read the parameter */
		if (line[line_pos] == '"' || line[line_pos] == '\'') {
			c = line[line_pos];
			++line_pos;
			for (param_pos = 0; line[line_pos] != c; /* NOTHING */) {
				param[param_pos++] = line[line_pos++];
				if (param_pos >= MAX_PARAM_LEN) {
					PRINT_LINENUM;
					printf("too long parameter\n");
					ret = -1;
					errors++;
					goto nextline;
				}
			}
			line_pos++;	/* skip the closing " or ' */
		} else {
			for (param_pos = 0; isprint(line[line_pos]) && !isspace(line[line_pos])
					&& line[line_pos] != '#'; /* NOTHING */) {
				param[param_pos++] = line[line_pos++];
				if (param_pos >= MAX_PARAM_LEN) {
					PRINT_LINENUM;
					printf("too long parameter\n");
					ret = -1;
					errors++;
					goto nextline;
				}
			}
		}
		param[param_pos] = '\0';

		/* did we read a parameter? */
		if (param_pos == 0) {
			PRINT_LINENUM;
			printf("option without parameter\n");
			ret = -1;
			errors++;
			continue;
		}
#ifdef DEBUG
		PRINT_LINENUM;
		printf("parameter: %s\n", param);
#endif
		/* now, check if we have some more chars on the line */
		/* whitespace... */
		while (isspace(line[line_pos]))
			++line_pos;

		/* EOL / comment */
		if (line[line_pos] != '\0' && line[line_pos] != '#') {
			PRINT_LINENUM;
			printf("extra characters on line: %s\n", line+line_pos);
			ret = -1;
		}

		tmp = read_option(opt, param);
		switch (tmp) {
		case ERR_NOT_AN_OPTION:
		case ERR_MISSING_PARAM:
		case ERR_OUT_OF_RANGE:
		case ERR_FUNC_ERR:
			PRINT_LINENUM;
			printf("%s\n", opt);
			ret = -1;
			errors++;
			continue;
			/* break */
		}	
nextline:
		;
	}

	free(line);
	fclose(fp);
out:
	--recursion_depth;
	return ret;
}

int parse_command_line(struct config *conf, int argc, char **argv, char **envp, char ***filenames)
{
	int i;
	char **f = NULL;
	int f_nr = 0;
	int tmp;
	char *opt;

#ifdef DEBUG
	assert(argv != NULL);
	assert(envp != NULL);
	assert(argc >= 1);
#endif

	if (init_conf(conf, COMMAND_LINE) == -1)
		return -1;

	/* in order to work recursion detection properly in parse_config_file */
	++recursion_depth;

	for (i = 1; i < argc; i++) {
		opt = argv[i];
		if (*opt != '-')
			goto filename;

		/* remove trailing '-' */
		opt++;

		tmp = read_option(opt, argv[i + 1]);

		switch (tmp) {
		case ERR_NOT_AN_OPTION:
filename:
			/* opt is not an option -> treat it as a filename */
			if (!(f = (char **) realloc(f, sizeof(*f) * (f_nr + 2))))
				goto err_out_mem;

			f[f_nr++] = argv[i];
			break;
		case ERR_MISSING_PARAM:
		case ERR_OUT_OF_RANGE:
		case ERR_FUNC_ERR:
			goto err_out;
			/* break; */
		default:
			i += tmp;
		}
	}
	if (f)
		f[f_nr] = NULL;
	if (filenames)
		*filenames = f;
	--recursion_depth;
	return f_nr; //filenames_nr;
err_out_mem:
	printf("can't allocate memory for filenames\n");
err_out:
	--recursion_depth;
	printf("command line: %s\n", argv[i]);
	return -1;
}

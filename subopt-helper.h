#ifndef SUBOPT_HELPER_H
#define SUBOPT_HELPER_H

/**
 * \file subopt-helper.h
 *
 * \brief Datatype and functions declarations for usage 
 *        of the suboption parser.
 *
 */

#define OPT_ARG_BOOL 0
#define OPT_ARG_INT  1
#define OPT_ARG_STR  2

/** simple structure for defining the option name, type and storage location */
typedef struct opt_s
{
  char * name; ///< string that identifies the option
  int type;    ///< option type as defined in subopt-helper.h
  void * valp; ///< pointer to the mem where the value should be stored
  int (* test)(void *); ///< argument test func ( optional )
  int set;     ///< Is set internally by the parser if the option was found.
               ///< Don't use it at initialization of your opts, it will be
               ///< overriden anyway!
} opt_t;

/** parses the string for the options specified in opt */
int subopt_parse( char const * const str, opt_t * opts );


/*------------------ arg specific types and declaration -------------------*/
typedef struct strarg_s
{
  unsigned char len; ///< length of the string determined by the parser
  char const * str;  ///< pointer to position inside the parse string
} strarg_t;

#endif

   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*            DEFAULT ATTRIBUTE HEADER FILE            */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides functions for parsing the default       */
/*   attribute and determining default values based on       */
/*   slot constraints.                                       */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_default
#define _H_default

#ifndef _H_constrnt
#include "constrnt.h"
#endif
#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _DEFAULT_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           DeriveDefaultFromConstraints(CONSTRAINT_RECORD *,DATA_OBJECT *,int);
   LOCALE struct expr                   *ParseDefault(char *,int,int,int,int *,int *,int *);

#endif





   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.20  01/31/02            */
   /*                                                     */
   /*          FACT CONSTRUCT COMPILER HEADER FILE        */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_factcmp

#define _H_factcmp

#ifndef _H_pattern
#include "pattern.h"
#endif
#ifndef _H_network
#include "network.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _FACTCMP_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

# include "evaluatn.h"

   LOCALE void                           FactPatternsCompilerSetup(void *,EXEC_STATUS);
   LOCALE void                           FactPatternNodeReference(void *,EXEC_STATUS,void *,FILE *,int,int);

#endif

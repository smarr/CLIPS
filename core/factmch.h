   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.20  01/31/02            */
   /*                                                     */
   /*               FACT MATCH HEADER FILE                */
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

#ifndef _H_factmch

#define _H_factmch

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_factmngr
#include "fact/fact_manager.h"
#endif
#ifndef _H_factbld
#include "factbld.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _FACTMCH_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

# include "execution_status.h"

   LOCALE void                           FactPatternMatch(void *,EXEC_STATUS,struct fact *,
                                               struct factPatternNode *,int,
                                               struct multifieldMarker *,
                                               struct multifieldMarker *);
   LOCALE void                           MarkFactPatternForIncrementalReset(void *,EXEC_STATUS,struct patternNodeHeader *,int);
   LOCALE void                           FactsIncrementalReset(void *,EXEC_STATUS);

#endif







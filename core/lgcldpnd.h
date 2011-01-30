   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*          LOGICAL DEPENDENCIES HEADER FILE           */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provide support routines for managing truth      */
/*   maintenance using the logical conditional element.      */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_lgcldpnd

#define _H_lgcldpnd

#ifndef _H_match
#include "match.h"
#endif
#ifndef _H_pattern
#include "pattern.h"
#endif

struct dependency
  {
   void *dPtr;
   struct dependency *next;
  };

#ifdef LOCALE
#undef LOCALE
#endif
#ifdef _LGCLDPND_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE BOOLEAN                        AddLogicalDependencies(struct patternEntity *,int);
   LOCALE void                           RemoveEntityDependencies(struct patternEntity *);
   LOCALE void                           RemovePMDependencies(struct partialMatch *);
   LOCALE void                           RemoveLogicalSupport(struct partialMatch *);
   LOCALE void                           ForceLogicalRetractions(void);
   LOCALE void                           Dependencies(struct patternEntity *);
   LOCALE void                           Dependents(struct patternEntity *);
   LOCALE void                           DependenciesCommand(void);
   LOCALE void                           DependentsCommand(void);

#endif






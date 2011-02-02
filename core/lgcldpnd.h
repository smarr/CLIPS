   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
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
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#ifndef _H_lgcldpnd

#define _H_lgcldpnd

struct dependency
  {
   void *dPtr;
   struct dependency *next;
  };

#ifndef _H_match
#include "match.h"
#endif
#ifndef _H_pattern
#include "pattern.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif
#ifdef _LGCLDPND_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE intBool                        AddLogicalDependencies(void *,EXEC_STATUS,struct patternEntity *,int);
   LOCALE void                           RemoveEntityDependencies(void *,EXEC_STATUS,struct patternEntity *);
   LOCALE void                           RemovePMDependencies(void *,EXEC_STATUS,struct partialMatch *);
   LOCALE void                           DestroyPMDependencies(void *,EXEC_STATUS,struct partialMatch *);
   LOCALE void                           RemoveLogicalSupport(void *,EXEC_STATUS,struct partialMatch *);
   LOCALE void                           ForceLogicalRetractions(void *,EXEC_STATUS);
   LOCALE void                           Dependencies(void *,EXEC_STATUS,struct patternEntity *);
   LOCALE void                           Dependents(void *,EXEC_STATUS,struct patternEntity *);
   LOCALE void                           DependenciesCommand(void *,EXEC_STATUS);
   LOCALE void                           DependentsCommand(void *,EXEC_STATUS);
   LOCALE void                           ReturnEntityDependencies(void *,EXEC_STATUS,struct patternEntity *);
   LOCALE struct partialMatch           *FindLogicalBind(struct joinNode *,struct partialMatch *);

#endif






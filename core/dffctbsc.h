   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
  /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*         DEFFACTS BASIC COMMANDS HEADER FILE         */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements core commands for the deffacts        */
/*   construct such as clear, reset, save, undeffacts,       */
/*   ppdeffacts, list-deffacts, and get-deffacts-list.       */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_dffctbsc
#define _H_dffctbsc

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _DFFCTBSC_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           DeffactsBasicCommands(void);
   LOCALE void                           UndeffactsCommand(void);
   LOCALE BOOLEAN                        Undeffacts(void *);
   LOCALE void                           GetDeffactsListFunction(DATA_OBJECT_PTR);
   LOCALE void                           GetDeffactsList(DATA_OBJECT_PTR,void *);
   LOCALE SYMBOL_HN                     *DeffactsModuleFunction(void);
   LOCALE void                           PPDeffactsCommand(void);
   LOCALE int                            PPDeffacts(char *,char *);
   LOCALE void                           ListDeffactsCommand(void);
   LOCALE void                           ListDeffacts(char *,void *);

#endif



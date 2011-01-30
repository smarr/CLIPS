   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*       DEFTEMPLATE BASIC COMMANDS HEADER FILE        */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements core commands for the deftemplate     */
/*   construct such as clear, reset, save, undeftemplate,    */
/*   ppdeftemplate, list-deftemplates, and                   */
/*   get-deftemplate-list.                                   */
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

#ifndef _H_tmpltbsc
#define _H_tmpltbsc

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _TMPLTBSC_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           DeftemplateBasicCommands(void);
   LOCALE void                           UndeftemplateCommand(void);
   LOCALE BOOLEAN                        Undeftemplate(void *);
   LOCALE void                           GetDeftemplateListFunction(DATA_OBJECT_PTR);
   LOCALE void                           GetDeftemplateList(DATA_OBJECT_PTR,void *);
   LOCALE SYMBOL_HN                     *DeftemplateModuleFunction(void);
#if DEBUGGING_FUNCTIONS
   LOCALE void                           PPDeftemplateCommand(void);
   LOCALE int                            PPDeftemplate(char *,char *);
   LOCALE void                           ListDeftemplatesCommand(void);
   LOCALE void                           ListDeftemplates(char *,void *);
   LOCALE BOOLEAN                        GetDeftemplateWatch(void *);
   LOCALE void                           SetDeftemplateWatch(int,void *);
   LOCALE BOOLEAN                        DeftemplateWatchAccess(int,int,struct expr *);
   LOCALE BOOLEAN                        DeftemplateWatchPrint(char *,int,struct expr *);
#endif

#ifndef _TMPLTBSC_SOURCE_
#if (! RUN_TIME) && (! BLOAD_ONLY) && DEBUGGING_FUNCTIONS
   extern Thread int                            DeletedTemplateDebugFlags;
#endif
#endif

#endif



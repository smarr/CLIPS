   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                  WATCH HEADER FILE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Support functions for the watch and unwatch      */
/*   commands.                                               */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_watch
#define _H_watch

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _WATCH_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#ifndef _H_expressn
#include "expressn.h"
#endif

   LOCALE BOOLEAN                        SetWatchItem(char *,int,struct expr *);
   LOCALE int                            GetWatchItem(char *);
   LOCALE BOOLEAN                        AddWatchItem(char *,int,int *,int,
                                                      BOOLEAN (*)(int,int,struct expr *),
                                                      BOOLEAN (*)(char *,int,struct expr *));
   LOCALE char                          *GetNthWatchName(int);
   LOCALE int                            GetNthWatchValue(int);
   LOCALE void                           WatchCommand(void);
   LOCALE void                           UnwatchCommand(void);
   LOCALE void                           ListWatchItemsCommand(void);
   LOCALE void                           WatchFunctionDefinitions(void);
   LOCALE DllExport BOOLEAN              Watch(char *);
   LOCALE DllExport BOOLEAN              Unwatch(char *);
   LOCALE int                            GetWatchItemCommand(void);

#endif



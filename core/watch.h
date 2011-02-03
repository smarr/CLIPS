   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
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
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added EnvSetWatchItem function.                */
/*                                                           */
/*************************************************************/

#ifndef _H_watch
#define _H_watch

#ifndef _H_expressn
#include "expressn.h"
#endif

#define WATCH_DATA 54

struct watchItem
  {
   char *name;
   unsigned *flag;
   int code,priority;
   unsigned (*accessFunc)(void *,EXEC_STATUS,int,unsigned,struct expr *);
   unsigned (*printFunc)(void *,EXEC_STATUS,char *,int,struct expr *);
   struct watchItem *next;
  };

struct watchData
  { 
   struct watchItem *ListOfWatchItems;
  };

#define WatchData(theEnv,execStatus) ((struct watchData *) GetEnvironmentData(theEnv,execStatus,WATCH_DATA))

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _WATCH_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define GetWatchItem(a) EnvGetWatchItem(GetCurrentEnvironment(),GetCurrentExecutionState(),a)
#define SetWatchItem(a,b) EnvSetWatchItem(GetCurrentEnvironment(),GetCurrentExecutionState(),a,b)

#if ALLOW_ENVIRONMENT_GLOBALS
   LOCALE intBool                        Watch(char *);
   LOCALE intBool                        Unwatch(char *);
#endif

   LOCALE intBool                        EnvWatch(void *,EXEC_STATUS,char *);
   LOCALE intBool                        EnvUnwatch(void *,EXEC_STATUS,char *);
   LOCALE void                           InitializeWatchData(void *,EXEC_STATUS);   
   LOCALE int                            EnvSetWatchItem(void *,EXEC_STATUS,char *,unsigned,struct expr *);
   LOCALE int                            EnvGetWatchItem(void *,EXEC_STATUS,char *);
   LOCALE intBool                        AddWatchItem(void *,EXEC_STATUS,char *,int,unsigned *,int,
                                                      unsigned (*)(void *,EXEC_STATUS,int,unsigned,struct expr *),
                                                      unsigned (*)(void *,EXEC_STATUS,char *,int,struct expr *));
   LOCALE char                          *GetNthWatchName(void *,EXEC_STATUS,int);
   LOCALE int                            GetNthWatchValue(void *,EXEC_STATUS,int);
   LOCALE void                           WatchCommand         (void *, EXEC_STATUS);
   LOCALE void                           UnwatchCommand       (void *, EXEC_STATUS);
   LOCALE void                           ListWatchItemsCommand(void *, EXEC_STATUS);
   LOCALE void                           WatchFunctionDefinitions(void *,EXEC_STATUS);
   LOCALE int                            GetWatchItemCommand(void *,EXEC_STATUS);

#endif




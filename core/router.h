   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*                 ROUTER HEADER FILE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides a centralized mechanism for handling    */
/*   input and output requests.                              */
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
/*            Added support for passing context information  */ 
/*            to the router functions.                       */
/*                                                           */
/*************************************************************/

#ifndef _H_router
#define _H_router

#ifndef _H_prntutil
#include "prntutil.h"
#endif

#ifndef _STDIO_INCLUDED_
#define _STDIO_INCLUDED_
#include <stdio.h>
#endif

#define WWARNING "wwarning"
#define WERROR "werror"
#define WTRACE "wtrace"
#define WDIALOG "wdialog"
#define WPROMPT  WPROMPT_STRING
#define WDISPLAY "wdisplay"

#define ROUTER_DATA 46

# include "execution_status.h"

struct router
  {
   char *name;
   int active;
   int priority;
   short int environmentAware;
   void *context;
   int (*query)(void *,EXEC_STATUS,char *);
   int (*printer)(void *,EXEC_STATUS,char *,char *);
   int (*exiter)(void *,EXEC_STATUS,int);
   int (*charget)(void *,EXEC_STATUS,char *);
   int (*charunget)(void *,EXEC_STATUS,int,char *);
   struct router *next;
  };

struct routerData
  { 
   size_t CommandBufferInputCount;
   int AwaitingInput;
   char *LineCountRouter;
   char *FastCharGetRouter;
   char *FastCharGetString;
   long FastCharGetIndex;
   struct router *ListOfRouters;
   FILE *FastLoadFilePtr;
   FILE *FastSaveFilePtr;
   int Abort;
  };

#define RouterData(theEnv) ((struct routerData *) GetEnvironmentData(theEnv,ROUTER_DATA))

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _ROUTER_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define ExitRouter(a) EnvExitRouter(GetCurrentEnvironment(),GetCurrentExecutionState(),a)
#define GetcRouter(a) EnvGetcRouter(GetCurrentEnvironment(),GetCurrentExecutionState(),a)
#define PrintRouter(a,b) EnvPrintRouter(GetCurrentEnvironment(),GetCurrentExecutionState(),a,b)
#define UngetcRouter(a,b) EnvUngetcRouter(GetCurrentEnvironment(),GetCurrentExecutionState(),a,b)
#define ActivateRouter(a) EnvActivateRouter(GetCurrentEnvironment(),GetCurrentExecutionState(),a)
#define DeactivateRouter(a) EnvDeactivateRouter(GetCurrentEnvironment(),GetCurrentExecutionState(),a)
#define DeleteRouter(a) EnvDeleteRouter(GetCurrentEnvironment(),GetCurrentExecutionState(),a)

   LOCALE void                           InitializeDefaultRouters(void *,EXEC_STATUS);
   LOCALE int                            EnvPrintRouter(void *,EXEC_STATUS,char *,char *);
   LOCALE int                            EnvGetcRouter(void *,EXEC_STATUS,char *);
   LOCALE int                            EnvUngetcRouter(void *,EXEC_STATUS,int,char *);
   LOCALE void                           EnvExitRouter(void *,EXEC_STATUS,int);
   LOCALE void                           AbortExit(void *,EXEC_STATUS);
   LOCALE intBool                        EnvAddRouterWithContext(void *,EXEC_STATUS,
                                                   char *,int,
                                                   int (*)(void *,EXEC_STATUS,char *),
                                                   int (*)(void *,EXEC_STATUS,char *,char *),
                                                   int (*)(void *,EXEC_STATUS,char *),
                                                   int (*)(void *,EXEC_STATUS,int,char *),
                                                   int (*)(void *,EXEC_STATUS,int),
                                                   void *);
   LOCALE intBool                        EnvAddRouter(void *,EXEC_STATUS,
                                                   char *,int,
                                                   int (*)(void *,EXEC_STATUS,char *),
                                                   int (*)(void *,EXEC_STATUS,char *,char *),
                                                   int (*)(void *,EXEC_STATUS,char *),
                                                   int (*)(void *,EXEC_STATUS,int,char *),
                                                   int (*)(void *,EXEC_STATUS,int));
   LOCALE intBool                        AddRouter(char *,int,
                                                   int (*)(char *),
                                                   int (*)(char *,char *),
                                                   int (*)(char *),
                                                   int (*)(int,char *),
                                                   int (*)(int));
   LOCALE int                            EnvDeleteRouter(void *,EXEC_STATUS,char *);
   LOCALE int                            QueryRouters(void *,EXEC_STATUS,char *);
   LOCALE int                            EnvDeactivateRouter(void *,EXEC_STATUS,char *);
   LOCALE int                            EnvActivateRouter(void *,EXEC_STATUS,char *);
   LOCALE void                           SetFastLoad(void *,EXEC_STATUS,FILE *);
   LOCALE void                           SetFastSave(void *,EXEC_STATUS,FILE *);
   LOCALE FILE                          *GetFastLoad(void *,EXEC_STATUS);
   LOCALE FILE                          *GetFastSave(void *,EXEC_STATUS);
   LOCALE void                           UnrecognizedRouterMessage(void *,EXEC_STATUS,char *);
   LOCALE void                           ExitCommand(void *,EXEC_STATUS);
   LOCALE int                            PrintNRouter(void *,EXEC_STATUS,char *,char *,unsigned long);

#endif



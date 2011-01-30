   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
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

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _ROUTER_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define PrintCLIPS(x,y) PrintRouter(x,y)
#define GetcCLIPS(x,y) GetcRouter(x,y)
#define UngetcCLIPS(x,y) UngetcRouter(x,y)
#define ExitCLIPS(x) ExitRouter(x)

#define WCLIPS WPROMPT

   LOCALE void                           InitializeDefaultRouters(void);
   LOCALE int                            PrintRouter(char *,char *);
   LOCALE int                            GetcRouter(char *);
   LOCALE int                            UngetcRouter(int,char *);
   LOCALE void                           ExitRouter(int);
   LOCALE void                           AbortExit(void);
   LOCALE DllExport BOOLEAN              AddRouter(char *,int,int (*)(char *),
                                                              int (*)(char *,char *),
                                                              int (*)(char *),
                                                              int (*)(int,char *),
                                                              int (*)(int));
   LOCALE DllExport int                  DeleteRouter(char *);
   LOCALE int                            QueryRouters(char *);
   LOCALE int                            DeactivateRouter(char *);
   LOCALE int                            ActivateRouter(char *);
   LOCALE void                           SetFastLoad(FILE *);
   LOCALE void                           SetFastSave(FILE *);
   LOCALE FILE                          *GetFastLoad(void);
   LOCALE FILE                          *GetFastSave(void);
   LOCALE void                           UnrecognizedRouterMessage(char *);
   LOCALE void                           ExitCommand(void);
   LOCALE int                            PrintNRouter(char *,char *,long);

#ifndef _ROUTER_SOURCE_
   extern Thread char                       *WWARNING;
   extern Thread char                       *WERROR;
   extern Thread char                       *WTRACE;
   extern Thread char                       *WDIALOG;
   extern Thread char                       *WPROMPT;
   extern Thread char                       *WDISPLAY;
   extern Thread int                         CommandBufferInputCount;
   extern Thread char                       *LineCountRouter;
   extern Thread char                       *FastCharGetRouter;
   extern Thread char                       *FastCharGetString;
   extern Thread long                        FastCharGetIndex;
#endif

#endif







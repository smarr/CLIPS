   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.20  01/31/02            */
   /*                                                     */
   /*         DEFMODULE BASIC COMMANDS HEADER FILE        */
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
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_modulbsc
#define _H_modulbsc

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _MODULBSC_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define GetDefmoduleList(a) EnvGetDefmoduleList(GetCurrentEnvironment(),GetCurrentExecutionState(),a)
#define ListDefmodules(a) EnvListDefmodules(GetCurrentEnvironment(),GetCurrentExecutionState(),a)

   LOCALE void                           DefmoduleBasicCommands(void *,EXEC_STATUS);
   LOCALE void                           EnvGetDefmoduleList(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                           PPDefmoduleCommand(void *,EXEC_STATUS);
   LOCALE int                            PPDefmodule(void *,EXEC_STATUS,char *,char *);
   LOCALE void                           ListDefmodulesCommand(void *,EXEC_STATUS);
   LOCALE void                           EnvListDefmodules(void *,EXEC_STATUS,char *);

#endif


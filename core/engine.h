   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                 ENGINE HEADER FILE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides functionality primarily associated with */
/*   the run and focus commands.                             */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_engine

#define _H_engine

#ifndef _H_ruledef
#include "ruledef.h"
#endif
#ifndef _H_network
#include "network.h"
#endif
#ifndef _H_moduldef
#include "moduldef.h"
#endif

struct focus
  {
   struct defmodule *theModule;
   struct defruleModule *theDefruleModule;
   struct focus *next;
  };

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _ENGINE_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

/**************************************************************/
/* The GetFocus function is remapped under certain conditions */
/* because it conflicts with a Windows 3.1 function.          */
/**************************************************************/
#if ! ((GENERIC || IBM) && WINDOW_INTERFACE)
#define WRGetFocus GetFocus
#endif

#define MAX_PATTERNS_CHECKED 64

   LOCALE DllExport long          Run(long);
   LOCALE BOOLEAN                 AddRunFunction(char *,void (*)(void),int);
   LOCALE BOOLEAN                 RemoveRunFunction(char *);
   LOCALE void                    InitializeEngine(void);
   LOCALE void                    SetBreak(void *);
   LOCALE BOOLEAN                 RemoveBreak(void *);
   LOCALE void                    RemoveAllBreakpoints(void);
   LOCALE void                    ShowBreaks(char *,void *);
   LOCALE BOOLEAN                 DefruleHasBreakpoint(void *);
   LOCALE void                    RunCommand(void);
   LOCALE void                    SetBreakCommand(void);
   LOCALE void                    RemoveBreakCommand(void);
   LOCALE void                    ShowBreaksCommand(void);
   LOCALE void                    HaltCommand(void);
   LOCALE int                     FocusCommand(void);
   LOCALE void                    ClearFocusStackCommand(void);
   LOCALE void                    ClearFocusStack(void);
   LOCALE void                   *GetNextFocus(void *);
   LOCALE void                    Focus(void *);
   LOCALE int                     GetFocusChanged(void);
   LOCALE void                    SetFocusChanged(int);
   LOCALE void                    ListFocusStackCommand(void);
   LOCALE void                    ListFocusStack(char *);
   LOCALE void                    GetFocusStackFunction(DATA_OBJECT_PTR);
   LOCALE void                    GetFocusStack(DATA_OBJECT_PTR);
   LOCALE SYMBOL_HN              *PopFocusFunction(void);
   LOCALE SYMBOL_HN              *GetFocusFunction(void);
   LOCALE void                   *PopFocus(void);
   LOCALE DllExport void         *WRGetFocus(void);

#ifndef _ENGINE_SOURCE_
   extern Thread struct defrule      *ExecutingRule;
   extern Thread BOOLEAN              HaltRules;
   extern Thread BOOLEAN              DeletedFiringRule;
   extern Thread struct joinNode     *TheLogicalJoin;
#endif

#endif







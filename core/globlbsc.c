   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*         DEFGLOBAL BASIC COMMANDS HEADER FILE        */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements core commands for the defglobal       */
/*   construct such as clear, reset, save, undefglobal,      */
/*   ppdefglobal, list-defglobals, and get-defglobals-list.  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _GLOBLBSC_SOURCE_

#include "setup.h"

#if DEFGLOBAL_CONSTRUCT

#include "constrct.h"
#include "extnfunc.h"
#include "watch.h"

#include "globlcom.h"
#include "globldef.h"

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
#include "globlbin.h"
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
#include "globlcmp.h"
#endif

#include "globlbsc.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    SaveDefglobals(char *);
   static void                    ResetDefglobalAction(struct constructHeader *,void *);
#if DEBUGGING_FUNCTIONS && (! RUN_TIME)
   static BOOLEAN                 DefglobalWatchAccess(int,int,struct expr *);
   static BOOLEAN                 DefglobalWatchPrint(char *,int,struct expr *);
#endif

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

#if DEBUGGING_FUNCTIONS
   Thread globle BOOLEAN               WatchGlobals = OFF;
#endif

/*****************************************************************/
/* DefglobalBasicCommands: Initializes basic defglobal commands. */
/*****************************************************************/
globle void DefglobalBasicCommands()
  {
   AddSaveFunction("defglobal",SaveDefglobals,40);
   AddResetFunction("defglobal",ResetDefglobals,50);

#if ! RUN_TIME
   DefineFunction2("get-defglobal-list",'m',PTIF GetDefglobalListFunction,"GetDefglobalListFunction","01w");
   DefineFunction2("undefglobal",'v',PTIF UndefglobalCommand,"UndefglobalCommand","11w");
   DefineFunction2("defglobal-module",'w',PTIF DefglobalModuleFunction,"DefglobalModuleFunction","11w");

#if DEBUGGING_FUNCTIONS
   DefineFunction2("list-defglobals",'v', PTIF ListDefglobalsCommand,"ListDefglobalsCommand","01w");
   DefineFunction2("ppdefglobal",'v',PTIF PPDefglobalCommand,"PPDefglobalCommand","11w");
   AddWatchItem("globals",0,&WatchGlobals,0,DefglobalWatchAccess,DefglobalWatchPrint);
#endif

#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)
   DefglobalBinarySetup();
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
   DefglobalCompilerSetup();
#endif

#endif
  }

/*************************************************************/
/* ResetDefglobals: Defglobal reset routine for use with the */
/*   reset command. Restores the values of the defglobals.   */
/*************************************************************/
globle void ResetDefglobals()
  {
   if (! GetResetGlobals()) return;
   DoForAllConstructs(ResetDefglobalAction,DefglobalModuleIndex,TRUE,NULL);
  }

/******************************************************/
/* ResetDefglobalAction: Action to be applied to each */
/*   defglobal construct during a reset command.      */
/******************************************************/
#if IBM_TBC
#pragma argsused
#endif
static void ResetDefglobalAction(
  struct constructHeader *theConstruct,
  void *buffer)
  {
#if MAC_MPW || MAC_MCW || IBM_MCW
#pragma unused(buffer)
#endif
   struct defglobal *theDefglobal = (struct defglobal *) theConstruct;
   DATA_OBJECT assignValue;

   if (EvaluateExpression(theDefglobal->initial,&assignValue))
     {
      assignValue.type = SYMBOL;
      assignValue.value = FalseSymbol;
     }

   QSetDefglobalValue(theDefglobal,&assignValue,FALSE);
  }

/******************************************/
/* SaveDefglobals: Defglobal save routine */
/*   for use with the save command.       */
/******************************************/
static void SaveDefglobals(
  char *logicalName)
  { SaveConstruct(logicalName,DefglobalConstruct); }

/********************************************/
/* UndefglobalCommand: H/L access routine   */
/*   for the undefglobal command.           */
/********************************************/
globle void UndefglobalCommand()
  { UndefconstructCommand("undefglobal",DefglobalConstruct); }

/**********************************/
/* Undefglobal: C access routine  */
/*   for the undefglobal command. */
/**********************************/
globle BOOLEAN Undefglobal(
  void *theDefglobal)
  { return(Undefconstruct(theDefglobal,DefglobalConstruct)); }

/**************************************************/
/* GetDefglobalListFunction: H/L access routine   */
/*   for the get-defglobal-list function.         */
/**************************************************/
globle void GetDefglobalListFunction(
  DATA_OBJECT_PTR returnValue)
  { GetConstructListFunction("get-defglobal-list",returnValue,DefglobalConstruct); }

/******************************************/
/* GetDefglobalList: C access routine for */
/*   the get-defglobal-list function.     */
/******************************************/
globle void GetDefglobalList(
  DATA_OBJECT_PTR returnValue,
  void *theModule)
  { GetConstructList(returnValue,DefglobalConstruct,(struct defmodule *) theModule); }

/*************************************************/
/* DefglobalModuleFunction: H/L access routine   */
/*   for the defglobal-module function.          */
/*************************************************/
globle SYMBOL_HN *DefglobalModuleFunction()
  { return(GetConstructModuleCommand("defglobal-module",DefglobalConstruct)); }

#if DEBUGGING_FUNCTIONS

/********************************************/
/* PPDefglobalCommand: H/L access routine   */
/*   for the ppdefglobal command.           */
/********************************************/
globle void PPDefglobalCommand()
  { PPConstructCommand("ppdefglobal",DefglobalConstruct); }

/*************************************/
/* PPDefglobal: C access routine for */
/*   the ppdefglobal command.        */
/*************************************/
globle int PPDefglobal(
  char *defglobalName,
  char *logicalName)
  { return(PPConstruct(defglobalName,logicalName,DefglobalConstruct)); }

/***********************************************/
/* ListDefglobalsCommand: H/L access routine   */
/*   for the list-defglobals command.          */
/***********************************************/
globle void ListDefglobalsCommand()
  { ListConstructCommand("list-defglobals",DefglobalConstruct); }

/****************************************/
/* ListDefglobals: C access routine for */
/*   the list-defglobals command.       */
/****************************************/
globle void ListDefglobals(
  char *logicalName,
  void *vTheModule)
  {
   struct defmodule *theModule = (struct defmodule *) vTheModule;

   ListConstruct(DefglobalConstruct,logicalName,theModule);
  }

/******************************************************/
/* GetDefglobalWatch: C access routine for retrieving */
/*   the current watch value of a defglobal.          */
/******************************************************/
globle BOOLEAN GetDefglobalWatch(
  void *theGlobal)
  { return(((struct defglobal *) theGlobal)->watch); }

/*****************************************************/
/* SetDeftemplateWatch: C access routine for setting */
/*   the current watch value of a deftemplate.       */
/*****************************************************/
globle void SetDefglobalWatch(
  int newState,
  void *theGlobal)
  { ((struct defglobal *) theGlobal)->watch = newState; }

#if ! RUN_TIME

/********************************************************/
/* DefglobalWatchAccess: Access routine for setting the */
/*   watch flag of a defglobal via the watch command.   */
/********************************************************/
#if IBM_TBC
#pragma argsused
#endif
static BOOLEAN DefglobalWatchAccess(
  int code,
  int newState,
  EXPRESSION *argExprs)
  {
#if MAC_MPW || MAC_MCW || IBM_MCW
#pragma unused(code)
#endif
   return(ConstructSetWatchAccess(DefglobalConstruct,newState,argExprs,
                                  GetDefglobalWatch,SetDefglobalWatch));
  }

/*********************************************************************/
/* DefglobalWatchPrint: Access routine for printing which defglobals */
/*   have their watch flag set via the list-watch-items command.     */
/*********************************************************************/
#if IBM_TBC
#pragma argsused
#endif
static BOOLEAN DefglobalWatchPrint(
  char *log,
  int code,
  EXPRESSION *argExprs)
  {
#if MAC_MPW || MAC_MCW || IBM_MCW
#pragma unused(code)
#endif
   return(ConstructPrintWatchAccess(DefglobalConstruct,log,argExprs,
                                    GetDefglobalWatch,SetDefglobalWatch));
  }

#endif

#endif /* DEBUGGING_FUNCTIONS */

#endif /* DEFGLOBAL_CONSTRUCT */



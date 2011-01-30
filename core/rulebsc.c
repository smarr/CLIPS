   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*          DEFRULE BASIC COMMANDS HEADER FILE         */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements core commands for the defrule         */
/*   construct such as clear, reset, save, undefrule,        */
/*   ppdefrule, list-defrules, and                           */
/*   get-defrule-list.                                       */
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

#define _RULEBSC_SOURCE_

#include "setup.h"

#if DEFRULE_CONSTRUCT

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "argacces.h"
#include "constrct.h"
#include "router.h"
#include "watch.h"
#include "extnfunc.h"
#include "ruledef.h"
#include "engine.h"
#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
#include "rulebin.h"
#endif
#if CONSTRUCT_COMPILER && (! RUN_TIME)
#include "rulecmp.h"
#endif

#include "rulebsc.h"

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

#if DEBUGGING_FUNCTIONS
   Thread globle BOOLEAN                 WatchRules = OFF;
#endif

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    ResetDefrules(void);
   static void                    SaveDefrules(char *);
#if (! RUN_TIME)
   static int                     ClearDefrulesReady(void);
   static void                    ClearDefrules(void);
#endif

/*************************************************************/
/* DefruleBasicCommands: Initializes basic defrule commands. */
/*************************************************************/
globle void DefruleBasicCommands()
  {
   AddResetFunction("defrule",ResetDefrules,70);
   AddSaveFunction("defrule",SaveDefrules,0);
#if (! RUN_TIME)
   AddClearReadyFunction("defrule",ClearDefrulesReady,0);
   AddClearFunction("defrule",ClearDefrules,0);
#endif

#if DEBUGGING_FUNCTIONS
   AddWatchItem("rules",0,&WatchRules,70,DefruleWatchAccess,DefruleWatchPrint);
#endif

#if ! RUN_TIME
   DefineFunction2("get-defrule-list",'m',PTIF GetDefruleListFunction,"GetDefruleListFunction","01w");
   DefineFunction2("undefrule",'v',PTIF UndefruleCommand,"UndefruleCommand","11w");
   DefineFunction2("defrule-module",'w',PTIF DefruleModuleFunction,"DefruleModuleFunction","11w");

#if DEBUGGING_FUNCTIONS
   DefineFunction2("rules",'v', PTIF ListDefrulesCommand,"ListDefrulesCommand","01w");
   DefineFunction2("list-defrules",'v', PTIF ListDefrulesCommand,"ListDefrulesCommand","01w");
   DefineFunction2("ppdefrule",'v',PTIF PPDefruleCommand,"PPDefruleCommand","11w");
#endif

#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)
   DefruleBinarySetup();
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
   DefruleCompilerSetup();
#endif

#endif
  }

/*****************************************************/
/* ResetDefrules: Defrule reset routine for use with */
/*   the reset command. Sets the current entity time */
/*   tag (used by the conflict resolution strategies */
/*   for recency) to zero. The focus stack is also   */
/*   cleared.                                        */
/*****************************************************/
static void ResetDefrules()
  {
   struct defmodule *theModule;

   CurrentEntityTimeTag = 0L;
   ClearFocusStack();
   theModule = (struct defmodule *) FindDefmodule("MAIN");
   Focus((void *) theModule);
  }

#if (! RUN_TIME)

/******************************************************************/
/* ClearDefrulesReady: Indicates whether defrules can be cleared. */
/******************************************************************/
static int ClearDefrulesReady()
  {
   if (ExecutingRule != NULL) return(FALSE);

   ClearFocusStack();
   if (GetCurrentModule() == NULL) return(FALSE);

   CurrentEntityTimeTag = 0L;

   return(TRUE);
  }

/***************************************************************/
/* ClearDefrules: Pushes the MAIN module as the current focus. */
/***************************************************************/
static void ClearDefrules()
  {
   struct defmodule *theModule;

   theModule = (struct defmodule *) FindDefmodule("MAIN");
   Focus((void *) theModule);
  }
#endif

/**************************************/
/* SaveDefrules: Defrule save routine */
/*   for use with the save command.   */
/**************************************/
static void SaveDefrules(
  char *logicalName)
  { SaveConstruct(logicalName,DefruleConstruct); }

/******************************************/
/* UndefruleCommand: H/L access routine   */
/*   for the undefrule command.           */
/******************************************/
globle void UndefruleCommand()
  { UndefconstructCommand("undefrule",DefruleConstruct); }

/********************************/
/* Undefrule: C access routine  */
/*   for the undefrule command. */
/********************************/
globle BOOLEAN Undefrule(
  void *theDefrule)
  { return(Undefconstruct(theDefrule,DefruleConstruct)); }

/************************************************/
/* GetDefruleListFunction: H/L access routine   */
/*   for the get-defrule-list function.         */
/************************************************/
globle void GetDefruleListFunction(
  DATA_OBJECT_PTR returnValue)
  { GetConstructListFunction("get-defrule-list",returnValue,DefruleConstruct); }

/****************************************/
/* GetDefruleList: C access routine for */
/*   the get-defrule-list function.     */
/****************************************/
globle void GetDefruleList(
  DATA_OBJECT_PTR returnValue,
  void *theModule)
  { GetConstructList(returnValue,DefruleConstruct,(struct defmodule *) theModule); }

/*******************************************/
/* DefruleModuleFunction: C access routine */
/*   for the defrule-module function.      */
/*******************************************/
globle SYMBOL_HN *DefruleModuleFunction()
  { return(GetConstructModuleCommand("defrule-module",DefruleConstruct)); }

#if DEBUGGING_FUNCTIONS

/******************************************/
/* PPDefruleCommand: H/L access routine   */
/*   for the ppdefrule command.           */
/******************************************/
globle void PPDefruleCommand()
  { PPConstructCommand("ppdefrule",DefruleConstruct); }

/***********************************/
/* PPDefrule: C access routine for */
/*   the ppdefrule command.        */
/***********************************/
globle int PPDefrule(
  char *defruleName,
  char *logicalName)
  { return(PPConstruct(defruleName,logicalName,DefruleConstruct)); }

/*********************************************/
/* ListDefrulesCommand: H/L access routine   */
/*   for the list-defrules command.          */
/*********************************************/
globle void ListDefrulesCommand()
  { ListConstructCommand("list-defrules",DefruleConstruct); }

/**************************************/
/* ListDefrules: C access routine for */
/*   the list-defrules command.       */
/**************************************/
globle void ListDefrules(
  char *logicalName,
  void *theModule)
  { ListConstruct(DefruleConstruct,logicalName,(struct defmodule *) theModule); }

/***************************************************************/
/* GetDefruleWatchActivations: C access routine for retrieving */
/*   the current watch value of a defrule's activations.       */
/***************************************************************/
globle BOOLEAN GetDefruleWatchActivations(
  void *rulePtr)
  {
   struct defrule *thePtr;

   for (thePtr = (struct defrule *) rulePtr;
        thePtr != NULL;
        thePtr = thePtr->disjunct)
     { if (thePtr->watchActivation) return(TRUE); }

   return(FALSE);
  }

/***********************************************************/
/* GetDefruleWatchFirings: C access routine for retrieving */
/*   the current watch value of a defrule's firings.       */
/***********************************************************/
globle BOOLEAN GetDefruleWatchFirings(
  void *rulePtr)
  {
   struct defrule *thePtr;

   for (thePtr = (struct defrule *) rulePtr;
        thePtr != NULL;
        thePtr = thePtr->disjunct)
     { if (thePtr->watchFiring) return(TRUE); }

   return(FALSE);
  }

/************************************************************/
/* SetDefruleWatchActivations: C access routine for setting */
/*   the current watch value of a defrule's activations.    */
/************************************************************/
globle void SetDefruleWatchActivations(
  int newState,
  void *rulePtr)
  {
   struct defrule *thePtr;

   for (thePtr = (struct defrule *) rulePtr;
        thePtr != NULL;
        thePtr = thePtr->disjunct)
     { thePtr->watchActivation = newState; }
  }

/********************************************************/
/* SetDefruleWatchFirings: C access routine for setting */
/*   the current watch value of a defrule's firings.    */
/********************************************************/
globle void SetDefruleWatchFirings(
  int newState,
  void *rulePtr)
  {
   struct defrule *thePtr;

   for (thePtr = (struct defrule *) rulePtr;
        thePtr != NULL;
        thePtr = thePtr->disjunct)
     { thePtr->watchFiring = newState; }
  }

/*******************************************************************/
/* DefruleWatchAccess: Access function for setting the watch flags */
/*   associated with rules (activations and rule firings).         */
/*******************************************************************/
globle BOOLEAN DefruleWatchAccess(
  int code,
  int newState,
  struct expr *argExprs)
  {
   if (code)
     return(ConstructSetWatchAccess(DefruleConstruct,newState,argExprs,
                                    GetDefruleWatchActivations,SetDefruleWatchActivations));
   else
     return(ConstructSetWatchAccess(DefruleConstruct,newState,argExprs,
                                    GetDefruleWatchFirings,SetDefruleWatchFirings));
  }

/*****************************************************************/
/* DefruleWatchPrint: Access routine for printing which defrules */
/*   have their watch flag set via the list-watch-items command. */
/*****************************************************************/
globle BOOLEAN DefruleWatchPrint(
  char *log,
  int code,
  struct expr *argExprs)
  {
   if (code)
     return(ConstructPrintWatchAccess(DefruleConstruct,log,argExprs,
                                      GetDefruleWatchActivations,SetDefruleWatchActivations));
   else
     return(ConstructPrintWatchAccess(DefruleConstruct,log,argExprs,
                                      GetDefruleWatchActivations,SetDefruleWatchActivations));
  }

#endif /* DEBUGGING_FUNCTIONS */

#endif /* DEFTEMPLATE_CONSTRUCT */



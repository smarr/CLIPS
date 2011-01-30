   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*          DEFTEMPLATE BASIC COMMANDS MODULE          */
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
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _TMPLTBSC_SOURCE_

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include "argacces.h"
#include "memalloc.h"
#include "scanner.h"
#include "router.h"
#include "extnfunc.h"
#include "constrct.h"
#include "cstrccom.h"
#include "factrhs.h"
#include "cstrcpsr.h"
#include "tmpltpsr.h"
#include "tmpltdef.h"
#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
#include "tmpltbin.h"
#endif
#if CONSTRUCT_COMPILER && (! RUN_TIME)
#include "tmpltcmp.h"
#endif
#include "tmpltutl.h"

#include "tmpltbsc.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    ResetDeftemplates(void);
   static void                    ClearDeftemplates(void);
   static void                    SaveDeftemplates(char *);

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

#if DEBUGGING_FUNCTIONS
   Thread globle int                     DeletedTemplateDebugFlags = FALSE;
#endif

/*********************************************************************/
/* DeftemplateBasicCommands: Initializes basic deftemplate commands. */
/*********************************************************************/
globle void DeftemplateBasicCommands()
  {
   AddResetFunction("deftemplate",ResetDeftemplates,0);
   AddClearFunction("deftemplate",ClearDeftemplates,0);
   AddSaveFunction("deftemplate",SaveDeftemplates,10);

#if ! RUN_TIME
   DefineFunction2("get-deftemplate-list",'m',PTIF GetDeftemplateListFunction,"GetDeftemplateListFunction","01w");
   DefineFunction2("undeftemplate",'v',PTIF UndeftemplateCommand,"UndeftemplateCommand","11w");
   DefineFunction2("deftemplate-module",'w',PTIF DeftemplateModuleFunction,"DeftemplateModuleFunction","11w");

#if DEBUGGING_FUNCTIONS
   DefineFunction2("list-deftemplates",'v', PTIF ListDeftemplatesCommand,"ListDeftemplatesCommand","01w");
   DefineFunction2("ppdeftemplate",'v',PTIF PPDeftemplateCommand,"PPDeftemplateCommand","11w");
#endif

#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)
   DeftemplateBinarySetup();
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
   DeftemplateCompilerSetup();
#endif

#endif
  }

/*************************************************************/
/* ResetDeftemplates: Deftemplate reset routine for use with */
/*   the reset command. Asserts the initial-fact fact when   */
/*   the deffacts construct has been disabled.               */
/*************************************************************/
static void ResetDeftemplates()
  {
#if ! DEFFACTS_CONSTRUCT
   struct fact *factPtr;

   factPtr = StringToFact("(initial-fact)");

   if (factPtr == NULL) return;

   Assert((void *) factPtr);
#endif
 }

/*****************************************************************/
/* ClearDeftemplates: Deftemplate clear routine for use with the */
/*   clear command. Creates the initial-facts deftemplate.       */
/*****************************************************************/
static void ClearDeftemplates()
  {
#if (! RUN_TIME) && (! BLOAD_ONLY)
   CreateImpliedDeftemplate((SYMBOL_HN *) AddSymbol("initial-fact"),FALSE);
#endif
  }

/**********************************************/
/* SaveDeftemplates: Deftemplate save routine */
/*   for use with the save command.           */
/**********************************************/
static void SaveDeftemplates(
  char *logicalName)
  { SaveConstruct(logicalName,DeftemplateConstruct); }

/**********************************************/
/* UndeftemplateCommand: H/L access routine   */
/*   for the undeftemplate command.           */
/**********************************************/
globle void UndeftemplateCommand()
  { UndefconstructCommand("undeftemplate",DeftemplateConstruct); }

/************************************/
/* Undeftemplate: C access routine  */
/*   for the undeftemplate command. */
/************************************/
globle BOOLEAN Undeftemplate(
  void *theDeftemplate)
  { return(Undefconstruct(theDeftemplate,DeftemplateConstruct)); }

/****************************************************/
/* GetDeftemplateListFunction: H/L access routine   */
/*   for the get-deftemplate-list function.         */
/****************************************************/
globle void GetDeftemplateListFunction(
  DATA_OBJECT_PTR returnValue)
  { GetConstructListFunction("get-deftemplate-list",returnValue,DeftemplateConstruct); }

/************************************************/
/* GetDeftemplateListFunction: C access routine */
/*   for the get-deftemplate-list function.     */
/************************************************/
globle void GetDeftemplateList(
  DATA_OBJECT_PTR returnValue,
  void *theModule)
  { GetConstructList(returnValue,DeftemplateConstruct,(struct defmodule *) theModule); }

/***************************************************/
/* DeftemplateModuleFunction: H/L access routine   */
/*   for the deftemplate-module function.          */
/***************************************************/
globle SYMBOL_HN *DeftemplateModuleFunction()
  { return(GetConstructModuleCommand("deftemplate-module",DeftemplateConstruct)); }

#if DEBUGGING_FUNCTIONS

/**********************************************/
/* PPDeftemplateCommand: H/L access routine   */
/*   for the ppdeftemplate command.           */
/**********************************************/
globle void PPDeftemplateCommand()
  { PPConstructCommand("ppdeftemplate",DeftemplateConstruct); }

/***************************************/
/* PPDeftemplate: C access routine for */
/*   the ppdeftemplate command.        */
/***************************************/
globle int PPDeftemplate(
  char *deftemplateName,
  char *logicalName)
  { return(PPConstruct(deftemplateName,logicalName,DeftemplateConstruct)); }

/*************************************************/
/* ListDeftemplatesCommand: H/L access routine   */
/*   for the list-deftemplates command.          */
/*************************************************/
globle void ListDeftemplatesCommand()
  { ListConstructCommand("list-deftemplates",DeftemplateConstruct); }

/******************************************/
/* ListDeftemplates: C access routine for */
/*   the list-deftemplates command.       */
/******************************************/
globle void ListDeftemplates(
  char *logicalName,
  void *theModule)
  { ListConstruct(DeftemplateConstruct,logicalName,(struct defmodule *) theModule); }

/********************************************************/
/* GetDeftemplateWatch: C access routine for retrieving */
/*   the current watch value of a deftemplate.          */
/********************************************************/
globle BOOLEAN GetDeftemplateWatch(
  void *theTemplate)
  { return(((struct deftemplate *) theTemplate)->watch); }

/******************************************************/
/* SetDeftemplateWatch:  C access routine for setting */
/*   the current watch value of a deftemplate.        */
/******************************************************/
globle void SetDeftemplateWatch(
  int newState,
  void *theTemplate)
  { ((struct deftemplate *) theTemplate)->watch = newState; }

/**********************************************************/
/* DeftemplateWatchAccess: Access routine for setting the */
/*   watch flag of a deftemplate via the watch command.   */
/**********************************************************/
#if IBM_TBC
#pragma argsused
#endif
globle BOOLEAN DeftemplateWatchAccess(
  int code,
  int newState,
  EXPRESSION *argExprs)
  {
#if MAC_MPW || MAC_MCW
#pragma unused(code)
#endif
   return(ConstructSetWatchAccess(DeftemplateConstruct,newState,argExprs,
                                  GetDeftemplateWatch,SetDeftemplateWatch));
  }

/*************************************************************************/
/* DeftemplateWatchPrint: Access routine for printing which deftemplates */
/*   have their watch flag set via the list-watch-items command.         */
/*************************************************************************/
#if IBM_TBC
#pragma argsused
#endif
globle BOOLEAN DeftemplateWatchPrint(
  char *log,
  int code,
  EXPRESSION *argExprs)
  {
#if MAC_MPW || MAC_MCW || IBM_MCW
#pragma unused(code)
#endif
   return(ConstructPrintWatchAccess(DeftemplateConstruct,log,argExprs,
                                    GetDeftemplateWatch,SetDeftemplateWatch));
  }

#endif /* DEBUGGING_FUNCTIONS */

#endif /* DEFTEMPLATE_CONSTRUCT */


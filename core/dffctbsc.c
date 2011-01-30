   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*         DEFFACTS BASIC COMMANDS HEADER FILE         */
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
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _DFFCTBSC_SOURCE_

#include "setup.h"

#if DEFFACTS_CONSTRUCT

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
#include "tmpltdef.h"
#include "cstrcpsr.h"
#include "dffctpsr.h"
#include "dffctdef.h"
#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
#include "dffctbin.h"
#endif
#if CONSTRUCT_COMPILER && (! RUN_TIME)
#include "dffctcmp.h"
#endif

#include "dffctbsc.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    ResetDeffacts(void);
   static void                    ClearDeffacts(void);
   static void                    SaveDeffacts(char *);
   static void                    ResetDeffactsAction(struct constructHeader *,void *);

/***************************************************************/
/* DeffactsBasicCommands: Initializes basic deffacts commands. */
/***************************************************************/
globle void DeffactsBasicCommands()
  {
   AddResetFunction("deffacts",ResetDeffacts,0);
   AddClearFunction("deffacts",ClearDeffacts,0);
   AddSaveFunction("deffacts",SaveDeffacts,10);

#if ! RUN_TIME
   DefineFunction2("get-deffacts-list",'m',PTIF GetDeffactsListFunction,"GetDeffactsListFunction","01w");
   DefineFunction2("undeffacts",'v',PTIF UndeffactsCommand,"UndeffactsCommand","11w");
   DefineFunction2("deffacts-module",'w',PTIF DeffactsModuleFunction,"DeffactsModuleFunction","11w");

#if DEBUGGING_FUNCTIONS
   DefineFunction2("list-deffacts",'v', PTIF ListDeffactsCommand,"ListDeffactsCommand","01w");
   DefineFunction2("ppdeffacts",'v',PTIF PPDeffactsCommand,"PPDeffactsCommand","11w");
#endif

#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)
   DeffactsBinarySetup();
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
   DeffactsCompilerSetup();
#endif

#endif
  }

/**********************************************************/
/* ResetDeffacts: Deffacts reset routine for use with the */
/*   reset command. Asserts all of the facts contained in */
/*   deffacts constructs.                                 */
/**********************************************************/
static void ResetDeffacts()
  { DoForAllConstructs(ResetDeffactsAction,DeffactsModuleIndex,TRUE,NULL); }

/*****************************************************/
/* ResetDeffactsAction: Action to be applied to each */
/*   deffacts construct during a reset command.      */
/*****************************************************/
#if IBM_TBC
#pragma argsused
#endif
static void ResetDeffactsAction(
  struct constructHeader *theConstruct,
  void *buffer)
  {
#if MAC_MPW || MAC_MCW || IBM_MCW
#pragma unused(buffer)
#endif
   DATA_OBJECT result;
   struct deffacts *theDeffacts = (struct deffacts *) theConstruct;

   if (theDeffacts->assertList == NULL) return;

   SetEvaluationError(FALSE);

   EvaluateExpression(theDeffacts->assertList,&result);
  }

/**********************************************************/
/* ClearDeffacts: Deffacts clear routine for use with the */
/*   clear command. Creates the initial-facts deffacts.   */
/**********************************************************/
static void ClearDeffacts()
  {
#if (! RUN_TIME) && (! BLOAD_ONLY)
   struct expr *stub;
   struct deffacts *newDeffacts;

   /*=====================================*/
   /* Create the data structures for the  */
   /* expression (assert (initial-fact)). */
   /*=====================================*/

   stub = GenConstant(FCALL,FindFunction("assert"));
   stub->argList = GenConstant(DEFTEMPLATE_PTR,FindDeftemplate("initial-fact"));
   ExpressionInstall(stub);

   /*=============================================*/
   /* Create a deffacts data structure to contain */
   /* the expression and initialize it.           */
   /*=============================================*/

   newDeffacts = get_struct(deffacts);
   newDeffacts->header.whichModule =
      (struct defmoduleItemHeader *) GetDeffactsModuleItem(NULL);
   newDeffacts->header.name = (SYMBOL_HN *) AddSymbol("initial-fact");
   IncrementSymbolCount(newDeffacts->header.name);
   newDeffacts->assertList = PackExpression(stub);
   newDeffacts->header.next = NULL;
   newDeffacts->header.ppForm = NULL;
   newDeffacts->header.usrData = NULL;
   ReturnExpression(stub);

   /*===========================================*/
   /* Store the deffacts in the current module. */
   /*===========================================*/

   AddConstructToModule(&newDeffacts->header);
#endif
  }

/***************************************/
/* SaveDeffacts: Deffacts save routine */
/*   for use with the save command.    */
/***************************************/
static void SaveDeffacts(
  char *logicalName)
  { SaveConstruct(logicalName,DeffactsConstruct); }

/*******************************************/
/* UndeffactsCommand: H/L access routine   */
/*   for the undeffacts command.           */
/*******************************************/
globle void UndeffactsCommand()
  { UndefconstructCommand("undeffacts",DeffactsConstruct); }

/*********************************/
/* Undeffacts: C access routine  */
/*   for the undeffacts command. */
/*********************************/
globle BOOLEAN Undeffacts(
  void *theDeffacts)
  { return(Undefconstruct(theDeffacts,DeffactsConstruct)); }

/*************************************************/
/* GetDeffactsListFunction: H/L access routine   */
/*   for the get-deffacts-list function.         */
/*************************************************/
globle void GetDeffactsListFunction(
  DATA_OBJECT_PTR returnValue)
  { GetConstructListFunction("get-deffacts-list",returnValue,DeffactsConstruct); }

/*****************************************/
/* GetDeffactsList: C access routine for */
/*   the get-deffacts-list function.     */
/*****************************************/
globle void GetDeffactsList(
  DATA_OBJECT_PTR returnValue,
  void *theModule)
  { GetConstructList(returnValue,DeffactsConstruct,(struct defmodule *) theModule); }

/************************************************/
/* DeffactsModuleFunction: H/L access routine   */
/*   for the deffacts-module function.          */
/************************************************/
globle SYMBOL_HN *DeffactsModuleFunction()
  { return(GetConstructModuleCommand("deffacts-module",DeffactsConstruct)); }

#if DEBUGGING_FUNCTIONS

/*******************************************/
/* PPDeffactsCommand: H/L access routine   */
/*   for the ppdeffacts command.           */
/*******************************************/
globle void PPDeffactsCommand()
  { PPConstructCommand("ppdeffacts",DeffactsConstruct); }

/************************************/
/* PPDeffacts: C access routine for */
/*   the ppdeffacts command.        */
/************************************/
globle int PPDeffacts(
  char *deffactsName,
  char *logicalName)
  { return(PPConstruct(deffactsName,logicalName,DeffactsConstruct)); }

/*********************************************/
/* ListDeffactsCommand: H/L access routine   */
/*   for the list-deffacts command.          */
/*********************************************/
globle void ListDeffactsCommand()
  { ListConstructCommand("list-deffacts",DeffactsConstruct); }

/**************************************/
/* ListDeffacts: C access routine for */
/*   the list-deffacts command.       */
/**************************************/
globle void ListDeffacts(
  char *logicalName,
  void *theModule)
  { ListConstruct(DeffactsConstruct,logicalName,(struct defmodule *) theModule); }

#endif /* DEBUGGING_FUNCTIONS */

#endif /* DEFFACTS_CONSTRUCT */



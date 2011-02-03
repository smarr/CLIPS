   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.23  01/31/05            */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Deffunction Execution Routines                   */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*************************************************************/

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "setup.h"

#if DEFFUNCTION_CONSTRUCT

#ifndef _STDIO_INCLUDED_
#define _STDIO_INCLUDED_
#include <stdio.h>
#endif

#include "constrct.h"
#include "envrnmnt.h"
#include "prcdrfun.h"
#include "prccode.h"
#include "proflfun.h"
#include "router.h"
#include "utility.h"
#include "watch.h"

#define _DFFNXEXE_SOURCE_
#include "dffnxexe.h"

/* =========================================
   *****************************************
                   CONSTANTS
   =========================================
   ***************************************** */
#define BEGIN_TRACE ">> "
#define END_TRACE   "<< "

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

static void UnboundDeffunctionErr(void *,EXEC_STATUS);

#if DEBUGGING_FUNCTIONS
static void WatchDeffunction(void *,EXEC_STATUS,char *);
#endif

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/****************************************************
  NAME         : CallDeffunction
  DESCRIPTION  : Executes the body of a deffunction
  INPUTS       : 1) The deffunction
                 2) Argument expressions
                 3) Data object buffer to hold result
  RETURNS      : Nothing useful
  SIDE EFFECTS : Deffunction executed and result
                 stored in data object buffer
  NOTES        : Used in EvaluateExpression(theEnv,execStatus,)
 ****************************************************/
globle void CallDeffunction(
  void *theEnv,
  EXEC_STATUS,
  DEFFUNCTION *dptr,
  EXPRESSION *args,
  DATA_OBJECT *result)
  {
   int oldce;
   DEFFUNCTION *previouslyExecutingDeffunction;
#if PROFILING_FUNCTIONS
   struct profileFrameInfo profileFrame;
#endif

   result->type = SYMBOL;
   result->value = EnvFalseSymbol(theEnv,execStatus);
   execStatus->EvaluationError = FALSE;
   if (execStatus->HaltExecution)
     return;
   oldce = ExecutingConstruct(theEnv,execStatus);
   SetExecutingConstruct(theEnv,execStatus,TRUE);
   previouslyExecutingDeffunction = DeffunctionData(theEnv,execStatus)->ExecutingDeffunction;
   DeffunctionData(theEnv,execStatus)->ExecutingDeffunction = dptr;
   execStatus->CurrentEvaluationDepth++;
   dptr->executing++;
   PushProcParameters(theEnv,execStatus,args,CountArguments(args),EnvGetDeffunctionName(theEnv,execStatus,(void *) dptr),
                      "deffunction",UnboundDeffunctionErr);
   if (execStatus->EvaluationError)
     {
      dptr->executing--;
      DeffunctionData(theEnv,execStatus)->ExecutingDeffunction = previouslyExecutingDeffunction;
      execStatus->CurrentEvaluationDepth--;
      PeriodicCleanup(theEnv,execStatus,FALSE,TRUE);
      SetExecutingConstruct(theEnv,execStatus,oldce);
      return;
     }

#if DEBUGGING_FUNCTIONS
   if (dptr->trace)
     WatchDeffunction(theEnv,execStatus,BEGIN_TRACE);
#endif

#if PROFILING_FUNCTIONS
   StartProfile(theEnv,execStatus,&profileFrame,
                &dptr->header.usrData,
                ProfileFunctionData(theEnv,execStatus)->ProfileConstructs);
#endif

   EvaluateProcActions(theEnv,execStatus,dptr->header.whichModule->theModule,
                       dptr->code,dptr->numberOfLocalVars,
                       result,UnboundDeffunctionErr);

#if PROFILING_FUNCTIONS
    EndProfile(theEnv,execStatus,&profileFrame);
#endif

#if DEBUGGING_FUNCTIONS
   if (dptr->trace)
     WatchDeffunction(theEnv,execStatus,END_TRACE);
#endif
   ProcedureFunctionData(theEnv,execStatus)->ReturnFlag = FALSE;

   dptr->executing--;
   PopProcParameters(theEnv,execStatus);
   DeffunctionData(theEnv,execStatus)->ExecutingDeffunction = previouslyExecutingDeffunction;
   execStatus->CurrentEvaluationDepth--;
   PropagateReturnValue(theEnv,execStatus,result);
   PeriodicCleanup(theEnv,execStatus,FALSE,TRUE);
   SetExecutingConstruct(theEnv,execStatus,oldce);
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/*******************************************************
  NAME         : UnboundDeffunctionErr
  DESCRIPTION  : Print out a synopis of the currently
                   executing deffunction for unbound
                   variable errors
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Error synopsis printed to WERROR
  NOTES        : None
 *******************************************************/
static void UnboundDeffunctionErr(
  void *theEnv,
  EXEC_STATUS)
  {
   EnvPrintRouter(theEnv,WERROR,"deffunction ");
   EnvPrintRouter(theEnv,WERROR,EnvGetDeffunctionName(theEnv,execStatus,(void *) DeffunctionData(theEnv,execStatus)->ExecutingDeffunction));
   EnvPrintRouter(theEnv,WERROR,".\n");
  }

#if DEBUGGING_FUNCTIONS

/***************************************************
  NAME         : WatchDeffunction
  DESCRIPTION  : Displays a message indicating when
                 a deffunction began and ended
                 execution
  INPUTS       : The beginning or end trace string
                 to print when deffunction starts
                 or finishes respectively
  RETURNS      : Nothing useful
  SIDE EFFECTS : Watch message printed
  NOTES        : None
 ***************************************************/
static void WatchDeffunction(
  void *theEnv,
  EXEC_STATUS,
  char *tstring)
  {
   EnvPrintRouter(theEnv,WTRACE,"DFN ");
   EnvPrintRouter(theEnv,WTRACE,tstring);
   if (DeffunctionData(theEnv,execStatus)->ExecutingDeffunction->header.whichModule->theModule != ((struct defmodule *) EnvGetCurrentModule(theEnv,execStatus)))
     {
      EnvPrintRouter(theEnv,WTRACE,EnvGetDefmoduleName(theEnv,execStatus,(void *)
                        DeffunctionData(theEnv,execStatus)->ExecutingDeffunction->header.whichModule->theModule));
      EnvPrintRouter(theEnv,WTRACE,"::");
     }
   EnvPrintRouter(theEnv,WTRACE,ValueToString(DeffunctionData(theEnv,execStatus)->ExecutingDeffunction->header.name));
   EnvPrintRouter(theEnv,WTRACE," ED:");
   PrintLongInteger(theEnv,execStatus,WTRACE,(long long) execStatus->CurrentEvaluationDepth);
   PrintProcParamArray(theEnv,execStatus,WTRACE);
  }

#endif
#endif

   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/09/97          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Deffunction Execution Routines                   */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
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
               MACROS AND TYPES
   =========================================
   ***************************************** */

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

static void UnboundDeffunctionErr(void);

#if DEBUGGING_FUNCTIONS
static void WatchDeffunction(char *);
#endif

/* =========================================
   *****************************************
      EXTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */
Thread globle DEFFUNCTION *ExecutingDeffunction = NULL;

/* =========================================
   *****************************************
      INTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */

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
  NOTES        : Used in EvaluateExpression()
 ****************************************************/
globle void CallDeffunction(
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
   result->value = FalseSymbol;
   EvaluationError = FALSE;
   if (HaltExecution)
     return;
   oldce = ExecutingConstruct();
   SetExecutingConstruct(TRUE);
   previouslyExecutingDeffunction = ExecutingDeffunction;
   ExecutingDeffunction = dptr;
   CurrentEvaluationDepth++;
   dptr->executing++;
   PushProcParameters(args,CountArguments(args),GetDeffunctionName((void *) dptr),
                      "deffunction",UnboundDeffunctionErr);
   if (EvaluationError)
     {
      dptr->executing--;
      ExecutingDeffunction = previouslyExecutingDeffunction;
      CurrentEvaluationDepth--;
      PeriodicCleanup(FALSE,TRUE);
      SetExecutingConstruct(oldce);
      return;
     }

#if DEBUGGING_FUNCTIONS
   if (dptr->trace)
     WatchDeffunction(BEGIN_TRACE);
#endif

#if PROFILING_FUNCTIONS
   StartProfile(&profileFrame,
                &dptr->header.usrData,
                ProfileConstructs);
#endif

   EvaluateProcActions(dptr->header.whichModule->theModule,
                       dptr->code,dptr->numberOfLocalVars,
                       result,UnboundDeffunctionErr);

#if PROFILING_FUNCTIONS
    EndProfile(&profileFrame);
#endif

#if DEBUGGING_FUNCTIONS
   if (dptr->trace)
     WatchDeffunction(END_TRACE);
#endif
   ReturnFlag = FALSE;

   dptr->executing--;
   PopProcParameters();
   ExecutingDeffunction = previouslyExecutingDeffunction;
   CurrentEvaluationDepth--;
   PropagateReturnValue(result);
   PeriodicCleanup(FALSE,TRUE);
   SetExecutingConstruct(oldce);
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
static void UnboundDeffunctionErr()
  {
   PrintRouter(WERROR,"deffunction ");
   PrintRouter(WERROR,GetDeffunctionName((void *) ExecutingDeffunction));
   PrintRouter(WERROR,".\n");
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
  char *tstring)
  {
   PrintRouter(WTRACE,"DFN ");
   PrintRouter(WTRACE,tstring);
   if (ExecutingDeffunction->header.whichModule->theModule != ((struct defmodule *) GetCurrentModule()))
     {
      PrintRouter(WTRACE,GetDefmoduleName((void *)
                        ExecutingDeffunction->header.whichModule->theModule));
      PrintRouter(WTRACE,"::");
     }
   PrintRouter(WTRACE,ValueToString(ExecutingDeffunction->header.name));
   PrintRouter(WTRACE," ED:");
   PrintLongInteger(WTRACE,(long) CurrentEvaluationDepth);
   PrintProcParamArray(WTRACE);
  }

#endif
#endif

/***************************************************
  NAME         :
  DESCRIPTION  :
  INPUTS       :
  RETURNS      :
  SIDE EFFECTS :
  NOTES        :
 ***************************************************/

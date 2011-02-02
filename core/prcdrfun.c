   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*             PROCEDURAL FUNCTIONS MODULE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for several procedural         */
/*   functions including if, while, loop-for-count, bind,    */
/*   progn, return, break, and switch                        */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*            Changed name of variable exp to theExp         */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#define _PRCDRFUN_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "setup.h"

#include "argacces.h"
#include "constrnt.h"
#include "cstrnchk.h"
#include "cstrnops.h"
#include "envrnmnt.h"
#include "exprnpsr.h"
#include "memalloc.h"
#include "multifld.h"
#include "prcdrpsr.h"
#include "router.h"
#include "scanner.h"
#include "utility.h"

#include "prcdrfun.h"

#if DEFGLOBAL_CONSTRUCT
#include "globldef.h"
#endif

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    DeallocateProceduralFunctionData(void *,EXEC_STATUS);

/**********************************************/
/* ProceduralFunctionDefinitions: Initializes */
/*   the procedural functions.                */
/**********************************************/
globle void ProceduralFunctionDefinitions(
  void *theEnv,
  EXEC_STATUS)
  {
   AllocateEnvironmentData(theEnv,execStatus,PRCDRFUN_DATA,sizeof(struct procedureFunctionData),DeallocateProceduralFunctionData);

#if ! RUN_TIME
   EnvDefineFunction2(theEnv,execStatus,"if", 'u', PTIEF IfFunction, "IfFunction", NULL);
   EnvDefineFunction2(theEnv,execStatus,"while", 'u', PTIEF WhileFunction, "WhileFunction", NULL);
   EnvDefineFunction2(theEnv,execStatus,"loop-for-count",'u', PTIEF LoopForCountFunction, "LoopForCountFunction", NULL);
   EnvDefineFunction2(theEnv,execStatus,"(get-loop-count)",'g', PTIEF GetLoopCount, "GetLoopCount", NULL);
   EnvDefineFunction2(theEnv,execStatus,"bind", 'u', PTIEF BindFunction, "BindFunction", NULL);
   EnvDefineFunction2(theEnv,execStatus,"progn", 'u', PTIEF PrognFunction, "PrognFunction", NULL);
   EnvDefineFunction2(theEnv,execStatus,"return", 'u', PTIEF ReturnFunction, "ReturnFunction",NULL);
   EnvDefineFunction2(theEnv,execStatus,"break", 'v', PTIEF BreakFunction, "BreakFunction",NULL);
   EnvDefineFunction2(theEnv,execStatus,"switch", 'u', PTIEF SwitchFunction, "SwitchFunction",NULL);

   ProceduralFunctionParsers(theEnv,execStatus);

   FuncSeqOvlFlags(theEnv,execStatus,"progn",FALSE,FALSE);
   FuncSeqOvlFlags(theEnv,execStatus,"if",FALSE,FALSE);
   FuncSeqOvlFlags(theEnv,execStatus,"while",FALSE,FALSE);
   FuncSeqOvlFlags(theEnv,execStatus,"loop-for-count",FALSE,FALSE);
   FuncSeqOvlFlags(theEnv,execStatus,"return",FALSE,FALSE);
   FuncSeqOvlFlags(theEnv,execStatus,"switch",FALSE,FALSE);
#endif

   EnvAddResetFunction(theEnv,execStatus,"bind",FlushBindList,0);
   EnvAddClearFunction(theEnv,execStatus,"bind",FlushBindList,0);
  }

/*************************************************************/
/* DeallocateProceduralFunctionData: Deallocates environment */
/*    data for procedural functions.                         */
/*************************************************************/
static void DeallocateProceduralFunctionData(
  void *theEnv,
  EXEC_STATUS)
  {
   DATA_OBJECT_PTR nextPtr, garbagePtr;

   garbagePtr = ProcedureFunctionData(theEnv,execStatus)->BindList;

   while (garbagePtr != NULL)
     {
      nextPtr = garbagePtr->next;
      rtn_struct(theEnv,execStatus,dataObject,garbagePtr);
      garbagePtr = nextPtr;
     }
  }

/***************************************/
/* WhileFunction: H/L access routine   */
/*   for the while function.           */
/***************************************/
globle void WhileFunction(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT_PTR returnValue)
  {
   DATA_OBJECT theResult;
   
   /*====================================================*/
   /* Evaluate the body of the while loop as long as the */
   /* while condition evaluates to a non-FALSE value.    */
   /*====================================================*/

   execStatus->CurrentEvaluationDepth++;
   EnvRtnUnknown(theEnv,execStatus,1,&theResult);
   while (((theResult.value != EnvFalseSymbol(theEnv,execStatus)) ||
           (theResult.type != SYMBOL)) &&
           (execStatus->HaltExecution != TRUE))
     {
      if ((ProcedureFunctionData(theEnv,execStatus)->BreakFlag == TRUE) || (ProcedureFunctionData(theEnv,execStatus)->ReturnFlag == TRUE))
        break;
      EnvRtnUnknown(theEnv,execStatus,2,&theResult);
      execStatus->CurrentEvaluationDepth--;
      if (ProcedureFunctionData(theEnv,execStatus)->ReturnFlag == TRUE)
        { PropagateReturnValue(theEnv,execStatus,&theResult); }
      PeriodicCleanup(theEnv,execStatus,FALSE,TRUE);
      execStatus->CurrentEvaluationDepth++;
      if ((ProcedureFunctionData(theEnv,execStatus)->BreakFlag == TRUE) || (ProcedureFunctionData(theEnv,execStatus)->ReturnFlag == TRUE))
        break;
      EnvRtnUnknown(theEnv,execStatus,1,&theResult);
     }
   execStatus->CurrentEvaluationDepth--;

   /*=====================================================*/
   /* Reset the break flag. The return flag is not reset  */
   /* because the while loop is probably contained within */
   /* a deffunction or RHS of a rule which needs to be    */
   /* returned from as well.                              */
   /*=====================================================*/

   ProcedureFunctionData(theEnv,execStatus)->BreakFlag = FALSE;

   /*====================================================*/
   /* If the return command was issued, then return that */
   /* value, otherwise return the symbol FALSE.          */
   /*====================================================*/

   if (ProcedureFunctionData(theEnv,execStatus)->ReturnFlag == TRUE)
     {
      returnValue->type = theResult.type;
      returnValue->value = theResult.value;
      returnValue->begin = theResult.begin;
      returnValue->end = theResult.end;
     }
   else
     {
      returnValue->type = SYMBOL;
      returnValue->value = EnvFalseSymbol(theEnv,execStatus);
     }
  }

/**********************************************/
/* LoopForCountFunction: H/L access routine   */
/*   for the loop-for-count function.         */
/**********************************************/
globle void LoopForCountFunction(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT_PTR loopResult)
  {
   DATA_OBJECT arg_ptr;
   long long iterationEnd;
   LOOP_COUNTER_STACK *tmpCounter;

   tmpCounter = get_struct(theEnv,execStatus,loopCounterStack);
   tmpCounter->loopCounter = 0L;
   tmpCounter->nxt = ProcedureFunctionData(theEnv,execStatus)->LoopCounterStack;
   ProcedureFunctionData(theEnv,execStatus)->LoopCounterStack = tmpCounter;
   if (EnvArgTypeCheck(theEnv,execStatus,"loop-for-count",1,INTEGER,&arg_ptr) == FALSE)
     {
      loopResult->type = SYMBOL;
      loopResult->value = EnvFalseSymbol(theEnv,execStatus);
      ProcedureFunctionData(theEnv,execStatus)->LoopCounterStack = tmpCounter->nxt;
      rtn_struct(theEnv,execStatus,loopCounterStack,tmpCounter);
      return;
     }
   tmpCounter->loopCounter = DOToLong(arg_ptr);
   if (EnvArgTypeCheck(theEnv,execStatus,"loop-for-count",2,INTEGER,&arg_ptr) == FALSE)
     {
      loopResult->type = SYMBOL;
      loopResult->value = EnvFalseSymbol(theEnv,execStatus);
      ProcedureFunctionData(theEnv,execStatus)->LoopCounterStack = tmpCounter->nxt;
      rtn_struct(theEnv,execStatus,loopCounterStack,tmpCounter);
      return;
     }
   iterationEnd = DOToLong(arg_ptr);
   while ((tmpCounter->loopCounter <= iterationEnd) &&
          (execStatus->HaltExecution != TRUE))
     {
      if ((ProcedureFunctionData(theEnv,execStatus)->BreakFlag == TRUE) || (ProcedureFunctionData(theEnv,execStatus)->ReturnFlag == TRUE))
        break;
      execStatus->CurrentEvaluationDepth++;
      EnvRtnUnknown(theEnv,execStatus,3,&arg_ptr);
      execStatus->CurrentEvaluationDepth--;
      if (ProcedureFunctionData(theEnv,execStatus)->ReturnFlag == TRUE)
        { PropagateReturnValue(theEnv,execStatus,&arg_ptr); }
      PeriodicCleanup(theEnv,execStatus,FALSE,TRUE);
      if ((ProcedureFunctionData(theEnv,execStatus)->BreakFlag == TRUE) || (ProcedureFunctionData(theEnv,execStatus)->ReturnFlag == TRUE))
        break;
      tmpCounter->loopCounter++;
     }

   ProcedureFunctionData(theEnv,execStatus)->BreakFlag = FALSE;
   if (ProcedureFunctionData(theEnv,execStatus)->ReturnFlag == TRUE)
     {
      loopResult->type = arg_ptr.type;
      loopResult->value = arg_ptr.value;
      loopResult->begin = arg_ptr.begin;
      loopResult->end = arg_ptr.end;
     }
   else
     {
      loopResult->type = SYMBOL;
      loopResult->value = EnvFalseSymbol(theEnv,execStatus);
     }
   ProcedureFunctionData(theEnv,execStatus)->LoopCounterStack = tmpCounter->nxt;
   rtn_struct(theEnv,execStatus,loopCounterStack,tmpCounter);
  }

/************************************************/
/* GetLoopCount                                 */
/************************************************/
globle long long GetLoopCount(
  void *theEnv,
  EXEC_STATUS)
  {
   int depth;
   LOOP_COUNTER_STACK *tmpCounter;

   depth = ValueToInteger(GetFirstArgument()->value);
   tmpCounter = ProcedureFunctionData(theEnv,execStatus)->LoopCounterStack;
   while (depth > 0)
     {
      tmpCounter = tmpCounter->nxt;
      depth--;
     }
   return(tmpCounter->loopCounter);
  }

/************************************/
/* IfFunction: H/L access routine   */
/*   for the if function.           */
/************************************/
globle void IfFunction(
  void *theEnv,
  EXEC_STATUS,
  EXEC_STATUS,
  DATA_OBJECT_PTR returnValue)
  {
   int numArgs;
   struct expr *theExpr;

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/

   if ((execStatus->CurrentExpression->argList == NULL) ||
       (execStatus->CurrentExpression->argList->nextArg == NULL))
     {
      EnvArgRangeCheck(theEnv,execStatus,"if",2,3);
      returnValue->type = SYMBOL;
      returnValue->value = EnvFalseSymbol(theEnv,execStatus);
      return;
     }

   if (execStatus->CurrentExpression->>argList->nextArg->nextArg == NULL)
     { numArgs = 2; }
   else if (execStatus->CurrentExpression->>argList->nextArg->nextArg->nextArg == NULL)
     { numArgs = 3; }
   else
     {
      EnvArgRangeCheck(theEnv,execStatus,"if",2,3);
      returnValue->type = SYMBOL;
      returnValue->value = EnvFalseSymbol(theEnv,execStatus);
      return;
     }

   /*=========================*/
   /* Evaluate the condition. */
   /*=========================*/

   EvaluateExpression(theEnv,execStatus,execStatus->CurrentExpression->>argList,returnValue);

   if ((ProcedureFunctionData(theEnv,execStatus)->BreakFlag == TRUE) || (ProcedureFunctionData(theEnv,execStatus)->ReturnFlag == TRUE))
     {
      returnValue->type = SYMBOL;
      returnValue->value = EnvFalseSymbol(theEnv,execStatus);
      return;
     }

   /*=========================================*/
   /* If the condition evaluated to FALSE and */
   /* an "else" portion exists, evaluate it   */
   /* and return the value.                   */
   /*=========================================*/

   if ((returnValue->value == EnvFalseSymbol(theEnv,execStatus)) &&
       (returnValue->type == SYMBOL) &&
       (numArgs == 3))
     {
      theExpr = execStatus->CurrentExpression->>argList->nextArg->nextArg;
      switch (theExpr->type)
        {
         case INTEGER:
         case FLOAT:
         case SYMBOL:
         case STRING:
#if OBJECT_SYSTEM
         case INSTANCE_NAME:
         case INSTANCE_ADDRESS:
#endif
         case EXTERNAL_ADDRESS:
           returnValue->type = theExpr->type;
           returnValue->value = theExpr->value;
           break;

         default:
           EvaluateExpression(theEnv,execStatus,theExpr,returnValue);
           break;
        }
      return;
     }

   /*===================================================*/
   /* Otherwise if the symbol evaluated to a non-FALSE  */
   /* value, evaluate the "then" portion and return it. */
   /*===================================================*/

   else if ((returnValue->value != EnvFalseSymbol(theEnv,execStatus)) ||
            (returnValue->type != SYMBOL))
     {
      theExpr = execStatus->CurrentExpression->>argList->nextArg;
      switch (theExpr->type)
        {
         case INTEGER:
         case FLOAT:
         case SYMBOL:
         case STRING:
#if OBJECT_SYSTEM
         case INSTANCE_NAME:
         case INSTANCE_ADDRESS:
#endif
         case EXTERNAL_ADDRESS:
           returnValue->type = theExpr->type;
           returnValue->value = theExpr->value;
           break;
           
         default:
           EvaluateExpression(theEnv,execStatus,theExpr,returnValue);
           break;
        }
      return;
     }

   /*=========================================*/
   /* Return FALSE if the condition evaluated */
   /* to FALSE and there is no "else" portion */
   /* of the if statement.                    */
   /*=========================================*/

   returnValue->type = SYMBOL;
   returnValue->value = EnvFalseSymbol(theEnv,execStatus);
   return;
  }

/**************************************/
/* BindFunction: H/L access routine   */
/*   for the bind function.           */
/**************************************/
globle void BindFunction(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT_PTR returnValue)
  {
   DATA_OBJECT *theBind, *lastBind;
   int found = FALSE,
       unbindVar = FALSE;
   SYMBOL_HN *variableName = NULL;
#if DEFGLOBAL_CONSTRUCT
   struct defglobal *theGlobal = NULL;
#endif

   /*===============================================*/
   /* Determine the name of the variable to be set. */
   /*===============================================*/

#if DEFGLOBAL_CONSTRUCT
   if (GetFirstArgument()->type == DEFGLOBAL_PTR)
     { theGlobal = (struct defglobal *) GetFirstArgument()->value; }
   else
#endif
     {
      EvaluateExpression(theEnv,execStatus,GetFirstArgument(),returnValue);
      variableName = (SYMBOL_HN *) DOPToPointer(returnValue);
     }

   /*===========================================*/
   /* Determine the new value for the variable. */
   /*===========================================*/

   if (GetFirstArgument()->nextArg == NULL)
     { unbindVar = TRUE; }
   else if (GetFirstArgument()->nextArg->nextArg == NULL)
     { EvaluateExpression(theEnv,execStatus,GetFirstArgument()->nextArg,returnValue); }
   else
     { StoreInMultifield(theEnv,execStatus,returnValue,GetFirstArgument()->nextArg,TRUE); }

   /*==================================*/
   /* Bind a defglobal if appropriate. */
   /*==================================*/

#if DEFGLOBAL_CONSTRUCT
   if (theGlobal != NULL)
     {
      QSetDefglobalValue(theEnv,execStatus,theGlobal,returnValue,unbindVar);
      return;
     }
#endif

   /*===============================================*/
   /* Search for the variable in the list of binds. */
   /*===============================================*/

   theBind = ProcedureFunctionData(theEnv,execStatus)->BindList;
   lastBind = NULL;

   while ((theBind != NULL) && (found == FALSE))
     {
      if (theBind->supplementalInfo == (void *) variableName)
        { found = TRUE; }
      else
        {
         lastBind = theBind;
         theBind = theBind->next;
        }
     }

   /*========================================================*/
   /* If variable was not in the list of binds, then add it. */
   /* Make sure that this operation preserves the bind list  */
   /* as a stack.                                            */
   /*========================================================*/

   if (found == FALSE)
     {
      if (unbindVar == FALSE)
        {
         theBind = get_struct(theEnv,execStatus,dataObject);
         theBind->supplementalInfo = (void *) variableName;
         IncrementSymbolCount(variableName);
         theBind->next = NULL;
         if (lastBind == NULL)
           { ProcedureFunctionData(theEnv,execStatus)->BindList = theBind; }
         else
           { lastBind->next = theBind; }
        }
      else
        {
         returnValue->type = SYMBOL;
         returnValue->value = EnvFalseSymbol(theEnv,execStatus);
         return;
        }
     }
   else
     { ValueDeinstall(theEnv,execStatus,theBind); }

   /*================================*/
   /* Set the value of the variable. */
   /*================================*/

   if (unbindVar == FALSE)
     {
      theBind->type = returnValue->type;
      theBind->value = returnValue->value;
      theBind->begin = returnValue->begin;
      theBind->end = returnValue->end;
      ValueInstall(theEnv,execStatus,returnValue);
     }
   else
     {
      if (lastBind == NULL) ProcedureFunctionData(theEnv,execStatus)->BindList = theBind->next;
      else lastBind->next = theBind->next;
      DecrementSymbolCount(theEnv,execStatus,(struct symbolHashNode *) theBind->supplementalInfo);
      rtn_struct(theEnv,execStatus,dataObject,theBind);
      returnValue->type = SYMBOL;
      returnValue->value = EnvFalseSymbol(theEnv,execStatus);
     }
  }

/*******************************************/
/* GetBoundVariable: Searches the BindList */
/*   for a specified variable.             */
/*******************************************/
globle intBool GetBoundVariable(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT_PTR vPtr,
  SYMBOL_HN *varName)
  {
   DATA_OBJECT_PTR bindPtr;
   
   for (bindPtr = ProcedureFunctionData(theEnv,execStatus)->BindList; bindPtr != NULL; bindPtr = bindPtr->next)
     {
      if (bindPtr->supplementalInfo == (void *) varName)
        {
         vPtr->type = bindPtr->type;
         vPtr->value = bindPtr->value;
         vPtr->begin = bindPtr->begin;
         vPtr->end = bindPtr->end;
         return(TRUE);
        }
     }

   return(FALSE);
  }

/*************************************************/
/* FlushBindList: Removes all variables from the */
/*   list of currently bound local variables.    */
/*************************************************/
globle void FlushBindList(
  void *theEnv,
  EXEC_STATUS)
  {
   ReturnValues(theEnv,execStatus,ProcedureFunctionData(theEnv,execStatus)->BindList,TRUE);
   ProcedureFunctionData(theEnv,execStatus)->BindList = NULL;
  }

/***************************************/
/* PrognFunction: H/L access routine   */
/*   for the progn function.           */
/***************************************/
globle void PrognFunction(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT_PTR returnValue)
  {
   struct expr *argPtr;

   argPtr = execStatus->CurrentExpression->>argList;

   if (argPtr == NULL)
     {
      returnValue->type = SYMBOL;
      returnValue->value = EnvFalseSymbol(theEnv,execStatus);
      return;
     }

   while ((argPtr != NULL) && (GetHaltExecution(theEnv,execStatus) != TRUE))
     {
      EvaluateExpression(theEnv,execStatus,argPtr,returnValue);

      if ((ProcedureFunctionData(theEnv,execStatus)->BreakFlag == TRUE) || (ProcedureFunctionData(theEnv,execStatus)->ReturnFlag == TRUE))
        break;
      argPtr = argPtr->nextArg;
     }

   if (GetHaltExecution(theEnv,execStatus) == TRUE)
     {
      returnValue->type = SYMBOL;
      returnValue->value = EnvFalseSymbol(theEnv,execStatus);
      return;
     }

   return;
  }

/*****************************************************************/
/* ReturnFunction: H/L access routine for the return function.   */
/*****************************************************************/
globle void ReturnFunction(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT_PTR result)
  {
   if (EnvRtnArgCount(theEnv,execStatus) == 0)
     {
      result->type = RVOID;
      result->value = EnvFalseSymbol(theEnv,execStatus);
     }
   else
     EnvRtnUnknown(theEnv,execStatus,1,result);
   ProcedureFunctionData(theEnv,execStatus)->ReturnFlag = TRUE;
  }

/***************************************************************/
/* BreakFunction: H/L access routine for the break function.   */
/***************************************************************/
globle void BreakFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   ProcedureFunctionData(theEnv,execStatus)->BreakFlag = TRUE;
  }

/*****************************************************************/
/* SwitchFunction: H/L access routine for the switch function.   */
/*****************************************************************/
globle void SwitchFunction(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT_PTR result)
  {
   DATA_OBJECT switch_val,case_val;
   EXPRESSION *theExp;

   result->type = SYMBOL;
   result->value = EnvFalseSymbol(theEnv,execStatus);

   /* ==========================
      Get the value to switch on
      ========================== */
   EvaluateExpression(theEnv,execStatus,GetFirstArgument(),&switch_val);
   if (execStatus->EvaluationError)
     return;
   for (theExp = GetFirstArgument()->nextArg ; theExp != NULL ; theExp = theExp->nextArg->nextArg)
     {
      /* =================================================
         RVOID is the default case (if any) for the switch
         ================================================= */
      if (theExp->type == RVOID)
        {
         EvaluateExpression(theEnv,execStatus,theExp->nextArg,result);
         return;
        }

      /* ====================================================
         If the case matches, evaluate the actions and return
         ==================================================== */
      EvaluateExpression(theEnv,execStatus,theExp,&case_val);
      if (execStatus->EvaluationError)
        return;
      if (switch_val.type == case_val.type)
        {
         if ((case_val.type == MULTIFIELD) ? MultifieldDOsEqual(&switch_val,&case_val) :
             (switch_val.value == case_val.value))
           {
            EvaluateExpression(theEnv,execStatus,theExp->nextArg,result);
            return;
           }
        }
     }
  }






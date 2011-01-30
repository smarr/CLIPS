   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
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
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description               */
/* ------------------+-------------+------------------------   */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS         */
/*************************************************************/

#define _PRCDRFUN_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "setup.h"

#include "memalloc.h"
#include "router.h"
#include "argacces.h"
#include "constrnt.h"
#include "cstrnchk.h"
#include "cstrnops.h"
#include "multifld.h"
#include "exprnpsr.h"
#include "scanner.h"
#include "utility.h"
#include "prcdrpsr.h"
#include "prcdrfun.h"

#if DEFGLOBAL_CONSTRUCT
#include "globldef.h"
#endif

typedef struct loopCounterStack
  {
   long loopCounter;
   struct loopCounterStack *nxt;
  } LOOP_COUNTER_STACK;

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle int                 ReturnFlag = FALSE;
   Thread globle int                 BreakFlag = FALSE;

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static LOOP_COUNTER_STACK *LoopCounterStack = NULL;
   Thread static struct dataObject  *BindList = NULL;

#if ! RUN_TIME
/**********************************************/
/* ProceduralFunctionDefinitions: Initializes */
/*   the procedural functions.                */
/**********************************************/
globle void ProceduralFunctionDefinitions()
  {
   DefineFunction2("if", 'u', PTIF IfFunction, "IfFunction", NULL);
   DefineFunction2("while", 'u', PTIF WhileFunction, "WhileFunction", NULL);
   DefineFunction2("loop-for-count",'u', PTIF LoopForCountFunction, "LoopForCountFunction", NULL);
   DefineFunction2("(get-loop-count)",'l', PTIF GetLoopCount, "GetLoopCount", NULL);
   DefineFunction2("bind", 'u', PTIF BindFunction, "BindFunction", NULL);
   DefineFunction2("progn", 'u', PTIF PrognFunction, "PrognFunction", NULL);
   DefineFunction2("return", 'u', PTIF ReturnFunction, "ReturnFunction",NULL);
   DefineFunction2("break", 'v', PTIF BreakFunction, "BreakFunction",NULL);
   DefineFunction2("switch", 'u', PTIF SwitchFunction, "SwitchFunction",NULL);

   ProceduralFunctionParsers();

   FuncSeqOvlFlags("progn",FALSE,FALSE);
   FuncSeqOvlFlags("if",FALSE,FALSE);
   FuncSeqOvlFlags("while",FALSE,FALSE);
   FuncSeqOvlFlags("loop-for-count",FALSE,FALSE);
   FuncSeqOvlFlags("return",FALSE,FALSE);
   FuncSeqOvlFlags("switch",FALSE,FALSE);
  }
#endif

/***************************************/
/* WhileFunction: H/L access routine   */
/*   for the while function.           */
/***************************************/
globle void WhileFunction(
  DATA_OBJECT_PTR returnValue)
  {
   DATA_OBJECT theResult;

   /*====================================================*/
   /* Evaluate the body of the while loop as long as the */
   /* while condition evaluates to a non-FALSE value.    */
   /*====================================================*/

   CurrentEvaluationDepth++;
   RtnUnknown(1,&theResult);
   while (((theResult.value != FalseSymbol) ||
           (theResult.type != SYMBOL)) &&
           (HaltExecution != TRUE))
     {
      if ((BreakFlag == TRUE) || (ReturnFlag == TRUE))
        break;
      RtnUnknown(2,&theResult);
      CurrentEvaluationDepth--;
      if (ReturnFlag == TRUE)
        { PropagateReturnValue(&theResult); }
      PeriodicCleanup(FALSE,TRUE);
      CurrentEvaluationDepth++;
      if ((BreakFlag == TRUE) || (ReturnFlag == TRUE))
        break;
      RtnUnknown(1,&theResult);
     }
   CurrentEvaluationDepth--;

   /*=====================================================*/
   /* Reset the break flag. The return flag is not reset  */
   /* because the while loop is probably contained within */
   /* a deffunction or RHS of a rule which needs to be    */
   /* returned from as well.                              */
   /*=====================================================*/

   BreakFlag = FALSE;

   /*====================================================*/
   /* If the return command was issued, then return that */
   /* value, otherwise return the symbol FALSE.          */
   /*====================================================*/

   if (ReturnFlag == TRUE)
     {
      returnValue->type = theResult.type;
      returnValue->value = theResult.value;
      returnValue->begin = theResult.begin;
      returnValue->end = theResult.end;
     }
   else
     {
      returnValue->type = SYMBOL;
      returnValue->value = FalseSymbol;
     }
  }

/**********************************************/
/* LoopForCountFunction: H/L access routine   */
/*   for the loop-for-count function.         */
/**********************************************/
globle void LoopForCountFunction(
  DATA_OBJECT_PTR loopResult)
  {
   DATA_OBJECT arg_ptr;
   long iterationEnd;
   LOOP_COUNTER_STACK *tmpCounter;

   tmpCounter = get_struct(loopCounterStack);
   tmpCounter->loopCounter = 0L;
   tmpCounter->nxt = LoopCounterStack;
   LoopCounterStack = tmpCounter;
   if (ArgTypeCheck("loop-for-count",1,INTEGER,&arg_ptr) == FALSE)
     {
      loopResult->type = SYMBOL;
      loopResult->value = FalseSymbol;
      LoopCounterStack = tmpCounter->nxt;
      rtn_struct(loopCounterStack,tmpCounter);
      return;
     }
   tmpCounter->loopCounter = DOToLong(arg_ptr);
   if (ArgTypeCheck("loop-for-count",2,INTEGER,&arg_ptr) == FALSE)
     {
      loopResult->type = SYMBOL;
      loopResult->value = FalseSymbol;
      LoopCounterStack = tmpCounter->nxt;
      rtn_struct(loopCounterStack,tmpCounter);
      return;
     }
   iterationEnd = DOToLong(arg_ptr);
   while ((tmpCounter->loopCounter <= iterationEnd) &&
          (HaltExecution != TRUE))
     {
      if ((BreakFlag == TRUE) || (ReturnFlag == TRUE))
        break;
      CurrentEvaluationDepth++;
      RtnUnknown(3,&arg_ptr);
      CurrentEvaluationDepth--;
      if (ReturnFlag == TRUE)
        { PropagateReturnValue(&arg_ptr); }
      PeriodicCleanup(FALSE,TRUE);
      if ((BreakFlag == TRUE) || (ReturnFlag == TRUE))
        break;
      tmpCounter->loopCounter++;
     }

   BreakFlag = FALSE;
   if (ReturnFlag == TRUE)
     {
      loopResult->type = arg_ptr.type;
      loopResult->value = arg_ptr.value;
      loopResult->begin = arg_ptr.begin;
      loopResult->end = arg_ptr.end;
     }
   else
     {
      loopResult->type = SYMBOL;
      loopResult->value = FalseSymbol;
     }
   LoopCounterStack = tmpCounter->nxt;
   rtn_struct(loopCounterStack,tmpCounter);
  }

/************************************************/
/* GetLoopCount                                 */
/************************************************/
globle long GetLoopCount()
  {
   int depth;
   LOOP_COUNTER_STACK *tmpCounter;

   depth = ValueToInteger(GetFirstArgument()->value);
   tmpCounter = LoopCounterStack;
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
  DATA_OBJECT_PTR returnValue)
  {
   int numArgs;

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/

   if ((numArgs = ArgRangeCheck("if",2,3)) == -1)
     {
      returnValue->type = SYMBOL;
      returnValue->value = FalseSymbol;
      return;
     }

   /*=========================*/
   /* Evaluate the condition. */
   /*=========================*/

   RtnUnknown(1,returnValue);
   if ((BreakFlag == TRUE) || (ReturnFlag == TRUE))
     { return; }

   /*=========================================*/
   /* If the condition evaluated to FALSE and */
   /* an "else" portion exists, evaluate it   */
   /* and return the value.                   */
   /*=========================================*/

   if ((returnValue->value == FalseSymbol) &&
       (returnValue->type == SYMBOL) &&
       (numArgs == 3))
     {
      RtnUnknown(3,returnValue);
      return;
     }

   /*===================================================*/
   /* Otherwise if the symbol evaluated to a non-FALSE  */
   /* value, evaluate the "then" portion and return it. */
   /*===================================================*/

   else if ((returnValue->value != FalseSymbol) ||
            (returnValue->type != SYMBOL))
     {
      RtnUnknown(2,returnValue);
      return;
     }

   /*=========================================*/
   /* Return FALSE if the condition evaluated */
   /* to FALSE and there is no "else" portion */
   /* of the if statement.                    */
   /*=========================================*/

   returnValue->type = SYMBOL;
   returnValue->value = FalseSymbol;
   return;
  }

/**************************************/
/* BindFunction: H/L access routine   */
/*   for the bind function.           */
/**************************************/
globle void BindFunction(
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
      EvaluateExpression(GetFirstArgument(),returnValue);
      variableName = (SYMBOL_HN *) DOPToPointer(returnValue);
     }

   /*===========================================*/
   /* Determine the new value for the variable. */
   /*===========================================*/

   if (GetFirstArgument()->nextArg == NULL)
     { unbindVar = TRUE; }
   else if (GetFirstArgument()->nextArg->nextArg == NULL)
     { EvaluateExpression(GetFirstArgument()->nextArg,returnValue); }
   else
     { StoreInMultifield(returnValue,GetFirstArgument()->nextArg,TRUE); }

   /*==================================*/
   /* Bind a defglobal if appropriate. */
   /*==================================*/

#if DEFGLOBAL_CONSTRUCT
   if (theGlobal != NULL)
     {
      QSetDefglobalValue(theGlobal,returnValue,unbindVar);
      return;
     }
#endif

   /*===============================================*/
   /* Search for the variable in the list of binds. */
   /*===============================================*/

   theBind = BindList;
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
         theBind = get_struct(dataObject);
         theBind->supplementalInfo = (void *) variableName;
         theBind->next = NULL;
         if (lastBind == NULL)
           { BindList = theBind; }
         else
           { lastBind->next = theBind; }
        }
      else
        {
         returnValue->type = SYMBOL;
         returnValue->value = FalseSymbol;
         return;
        }
     }
   else
     { ValueDeinstall(theBind); }

   /*================================*/
   /* Set the value of the variable. */
   /*================================*/

   if (unbindVar == FALSE)
     {
      theBind->type = returnValue->type;
      theBind->value = returnValue->value;
      theBind->begin = returnValue->begin;
      theBind->end = returnValue->end;
      ValueInstall(returnValue);
     }
   else
     {
      if (lastBind == NULL) BindList = theBind->next;
      else lastBind->next = theBind->next;
      rtn_struct(dataObject,theBind);
      returnValue->type = SYMBOL;
      returnValue->value = FalseSymbol;
     }
  }

/*******************************************/
/* GetBoundVariable: Searches the BindList */
/*   for a specified variable.             */
/*******************************************/
globle BOOLEAN GetBoundVariable(
  DATA_OBJECT_PTR vPtr,
  SYMBOL_HN *varName)
  {
   DATA_OBJECT_PTR bindPtr;

   for (bindPtr = BindList; bindPtr != NULL; bindPtr = bindPtr->next)
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
globle void FlushBindList()
  {
   ReturnValues(BindList);
   BindList = NULL;
  }

/***************************************/
/* PrognFunction: H/L access routine   */
/*   for the progn function.           */
/***************************************/
globle void PrognFunction(
  DATA_OBJECT_PTR returnValue)
  {
   int numa, i;

   numa = RtnArgCount();

   if (numa == 0)
     {
      returnValue->type = SYMBOL;
      returnValue->value = FalseSymbol;
      return;
     }

   i = 1;
   while ((i <= numa) && (GetHaltExecution() != TRUE))
     {
      RtnUnknown(i,returnValue);
      if ((BreakFlag == TRUE) || (ReturnFlag == TRUE))
        break;
      i++;
     }

   if (GetHaltExecution() == TRUE)
     {
      returnValue->type = SYMBOL;
      returnValue->value = FalseSymbol;
      return;
     }

   return;
  }

/*****************************************************************/
/* ReturnFunction: H/L access routine for the return function.   */
/*****************************************************************/
globle void ReturnFunction(
  DATA_OBJECT_PTR result)
  {
   if (RtnArgCount() == 0)
     {
      result->type = RVOID;
      result->value = FalseSymbol;
     }
   else
     RtnUnknown(1,result);
   ReturnFlag = TRUE;
  }

/***************************************************************/
/* BreakFunction: H/L access routine for the break function.   */
/***************************************************************/
globle void BreakFunction()
  {
   BreakFlag = TRUE;
  }

/*****************************************************************/
/* SwitchFunction: H/L access routine for the switch function.   */
/*****************************************************************/
globle void SwitchFunction(
  DATA_OBJECT_PTR result)
  {
   DATA_OBJECT switch_val,case_val;
   EXPRESSION *exp;

   result->type = SYMBOL;
   result->value = FalseSymbol;

   /* ==========================
      Get the value to switch on
      ========================== */
   EvaluateExpression(GetFirstArgument(),&switch_val);
   if (EvaluationError)
     return;
   for (exp = GetFirstArgument()->nextArg ; exp != NULL ; exp = exp->nextArg->nextArg)
     {
      /* =================================================
         RVOID is the default case (if any) for the switch
         ================================================= */
      if (exp->type == RVOID)
        {
         EvaluateExpression(exp->nextArg,result);
         return;
        }

      /* ====================================================
         If the case matches, evaluate the actions and return
         ==================================================== */
      EvaluateExpression(exp,&case_val);
      if (EvaluationError)
        return;
      if (switch_val.type == case_val.type)
        {
         if ((case_val.type == MULTIFIELD) ? MultifieldDOsEqual(&switch_val,&case_val) :
             (switch_val.value == case_val.value))
           {
            EvaluateExpression(exp->nextArg,result);
            return;
           }
        }
     }
  }






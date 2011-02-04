   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*             EXPRESSION OPERATIONS MODULE            */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides utility routines for manipulating and   */
/*   examining expressions.                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#define _EXPRNOPS_SOURCE_

#include "setup.h"

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "memalloc.h"
#include "envrnmnt.h"
#include "router.h"
#include "extnfunc.h"
#include "cstrnchk.h"
#include "prntutil.h"
#include "cstrnutl.h"
#include "cstrnops.h"

#include "exprnops.h"

#if (! RUN_TIME)

/**************************************************************/
/* CheckArgumentAgainstRestriction: Compares an argument to a */
/*   function to the set of restrictions for that function to */
/*   determine if any incompatibilities exist. If so, the     */
/*   value TRUE is returned, otherwise FALSE is returned.     */
/*   Restrictions checked are:                                */
/*     a - external address                                   */
/*     d - float                                              */
/*     e - instance address, instance name, or symbol         */
/*     f - float                                              */
/*     g - integer, float, or symbol                          */
/*     h - instance address, instance name, fact address,     */
/*         integer, or symbol                                 */
/*     i - integer                                            */
/*     j - symbol, string, or instance name                   */
/*     k - symbol or string                                   */
/*     l - integer                                            */
/*     m - multifield                                         */
/*     n - float or integer                                   */
/*     o - instance name                                      */
/*     p - instance name or symbol                            */
/*     q - string, symbol, or multifield                      */
/*     s - string                                             */
/*     u - unknown (any type allowed)                         */
/*     w - symbol                                             */
/*     x - instance address                                   */
/*     y - fact address                                       */
/*     z - fact address, integer, or symbol (*)               */
/**************************************************************/
globle int CheckArgumentAgainstRestriction(
  void *theEnv,
  EXEC_STATUS,
  struct expr *theExpression,
  int theRestriction)
  {
   CONSTRAINT_RECORD *cr1, *cr2, *cr3;

   /*=============================================*/
   /* Generate a constraint record for the actual */
   /* argument passed to the function.            */
   /*=============================================*/

   cr1 = ExpressionToConstraintRecord(theEnv,execStatus,theExpression);

   /*================================================*/
   /* Generate a constraint record based on the type */
   /* of argument expected by the function.          */
   /*================================================*/

   cr2 = ArgumentTypeToConstraintRecord(theEnv,execStatus,theRestriction);

   /*===============================================*/
   /* Intersect the two constraint records and then */
   /* discard them.                                 */
   /*===============================================*/

   cr3 = IntersectConstraints(theEnv,execStatus,cr1,cr2);

   RemoveConstraint(theEnv,execStatus,cr1);
   RemoveConstraint(theEnv,execStatus,cr2);

   /*====================================================*/
   /* If the intersection of the two constraint records  */
   /* is empty, then the argument passed to the function */
   /* doesn't satisfy the restrictions for the argument. */
   /*====================================================*/

   if (UnmatchableConstraint(cr3))
     {
      RemoveConstraint(theEnv,execStatus,cr3);
      return(TRUE);
     }

   /*===================================================*/
   /* The argument satisfies the function restrictions. */
   /*===================================================*/

   RemoveConstraint(theEnv,execStatus,cr3);
   return(FALSE);
  }

#endif /* (! RUN_TIME) */

/************************************************************/
/* ConstantExpression: Returns TRUE if the expression */
/*   is a constant, otherwise FALSE.                  */
/************************************************************/
globle intBool ConstantExpression(
  struct expr *testPtr)
  {
   while (testPtr != NULL)
     {
      if ((testPtr->type != SYMBOL) && (testPtr->type != STRING) &&
#if OBJECT_SYSTEM
          (testPtr->type != INSTANCE_NAME) && (testPtr->type != INSTANCE_ADDRESS) &&
#endif
          (testPtr->type != INTEGER) && (testPtr->type != FLOAT))
        { return(FALSE); }
      testPtr = testPtr->nextArg;
     }

   return(TRUE);
  }

/************************************************/
/* ConstantType: Returns TRUE if the type */
/*   is a constant, otherwise FALSE.      */
/************************************************/
globle intBool ConstantType(
  int theType)
  {
   switch (theType)
     {
      case SYMBOL:
      case STRING:
      case INTEGER:
      case FLOAT:
#if OBJECT_SYSTEM
      case INSTANCE_NAME:
      case INSTANCE_ADDRESS:
#endif
        return(TRUE);
     }

   return(FALSE);
  }

/*****************************************************************************/
/* IdenticalExpression: Determines if two expressions are identical. Returns */
/*   TRUE if the expressions are identical, otherwise FALSE is returned.     */
/*****************************************************************************/
globle intBool IdenticalExpression(
  struct expr *firstList,
  struct expr *secondList)
  {
   /*==============================================*/
   /* Compare each argument in both expressions by */
   /* following the nextArg list.                  */
   /*==============================================*/

   for (;
        (firstList != NULL) && (secondList != NULL);
        firstList = firstList->nextArg, secondList = secondList->nextArg)
     {
      /*=========================*/
      /* Compare type and value. */
      /*=========================*/

      if (firstList->type != secondList->type)
        { return(FALSE); }

      if (firstList->value != secondList->value)
        { return (FALSE); }

      /*==============================*/
      /* Compare the arguments lists. */
      /*==============================*/

      if (IdenticalExpression(firstList->argList,secondList->argList) == FALSE)
        { return(FALSE); }
     }

   /*=====================================================*/
   /* If firstList and secondList aren't both NULL, then  */
   /* one of the lists contains more expressions than the */
   /* other.                                              */
   /*=====================================================*/

   if (firstList != secondList) return(FALSE);

   /*============================*/
   /* Expressions are identical. */
   /*============================*/

   return(TRUE);
  }

/****************************************************/
/* CountArguments: Returns the number of structures */
/*   stored in an expression as traversed through   */
/*   the nextArg pointer but not the argList        */
/*   pointer.                                       */
/****************************************************/
globle int CountArguments(
  struct expr *testPtr)
  {
   int size = 0;

   while (testPtr != NULL)
     {
      size++;
      testPtr = testPtr->nextArg;
     }

   return(size);
  }

/******************************************/
/* CopyExpresssion: Copies an expression. */
/******************************************/
globle struct expr *CopyExpression(
  void *theEnv,
  EXEC_STATUS,
  struct expr *original)
  {
   struct expr *topLevel, *next, *last;

   if (original == NULL) return(NULL);

   topLevel = GenConstant(theEnv,execStatus,original->type,original->value);
   topLevel->argList = CopyExpression(theEnv,execStatus,original->argList);

   last = topLevel;
   original = original->nextArg;
   while (original != NULL)
     {
      next = GenConstant(theEnv,execStatus,original->type,original->value);
      next->argList = CopyExpression(theEnv,execStatus,original->argList);

      last->nextArg = next;
      last = next;
      original = original->nextArg;
     }

   return(topLevel);
  }

/************************************************************/
/* ExpressionContainsVariables: Determines if an expression */
/*   contains any variables. Returns TRUE if the expression */
/*   contains any variables, otherwise FALSE is returned.   */
/************************************************************/
globle intBool ExpressionContainsVariables(
  struct expr *theExpression,
  intBool globalsAreVariables)
  {
   while (theExpression != NULL)
     {
      if (theExpression->argList != NULL)
        {
         if (ExpressionContainsVariables(theExpression->argList,globalsAreVariables))
           { return(TRUE); }
        }

      if ((theExpression->type == MF_VARIABLE) ||
          (theExpression->type == SF_VARIABLE) ||
          (theExpression->type == FACT_ADDRESS) ||
          (((theExpression->type == GBL_VARIABLE) ||
            (theExpression->type == MF_GBL_VARIABLE)) &&
           (globalsAreVariables == TRUE)))
        { return(TRUE); }

      theExpression = theExpression->nextArg;
     }

   return(FALSE);
  }

/*****************************************/
/* ExpressionSize: Returns the number of */
/*   structures stored in an expression. */
/*****************************************/
globle long ExpressionSize(
  struct expr *testPtr)
  {
   long size = 0;

   while (testPtr != NULL)
     {
      size++;
      if (testPtr->argList != NULL)
        { size += ExpressionSize(testPtr->argList); }
      testPtr = testPtr->nextArg;
     }
   return(size);
  }

/************************************************/
/* GenConstant: Generates a constant expression */
/*   value of type string, symbol, or number.   */
/************************************************/
globle struct expr *GenConstant(
  void *theEnv,
  EXEC_STATUS,
  unsigned short type,
  void *value)
  {
   struct expr *top;

   top = get_struct(theEnv,execStatus,expr);
   top->nextArg = NULL;
   top->argList = NULL;
   top->type = type;
   top->value = value;
   top->refCount = 0;

   return(top);
  }

/*************************************************/
/* PrintExpression: Pretty prints an expression. */
/*************************************************/
globle void PrintExpression(
  void *theEnv,
  EXEC_STATUS,
  char *fileid,
  struct expr *theExpression)
  {
   struct expr *oldExpression;

   if (theExpression == NULL)
     { return; }

   while (theExpression != NULL)
     {
      switch (theExpression->type)
        {
         case SF_VARIABLE:
         case GBL_VARIABLE:
            EnvPrintRouter(theEnv,execStatus,fileid,"?");
            EnvPrintRouter(theEnv,execStatus,fileid,ValueToString(theExpression->value));
            break;

         case MF_VARIABLE:
         case MF_GBL_VARIABLE:
            EnvPrintRouter(theEnv,execStatus,fileid,"$?");
            EnvPrintRouter(theEnv,execStatus,fileid,ValueToString(theExpression->value));
            break;

         case FCALL:
           EnvPrintRouter(theEnv,execStatus,fileid,"(");
           EnvPrintRouter(theEnv,execStatus,fileid,ValueToString(ExpressionFunctionCallName(theExpression)));
           if (theExpression->argList != NULL) { EnvPrintRouter(theEnv,execStatus,fileid," "); }
           PrintExpression(theEnv,execStatus,fileid,theExpression->argList);
           EnvPrintRouter(theEnv,execStatus,fileid,")");
           break;

         default:
           oldExpression = execStatus->CurrentExpression;
           execStatus->CurrentExpression = theExpression;
           PrintAtom(theEnv,execStatus,fileid,theExpression->type,theExpression->value);
           execStatus->CurrentExpression = oldExpression;
           break;
        }

      theExpression = theExpression->nextArg;
      if (theExpression != NULL) EnvPrintRouter(theEnv,execStatus,fileid," ");
     }

   return;
  }

/*************************************************************************/
/* CombineExpressions: Combines two expressions into a single equivalent */
/*   expression. Mainly serves to merge expressions containing "and"     */
/*   and "or" expressions without unnecessary duplication of the "and"   */
/*   and "or" expressions (i.e., two "and" expressions can be merged by  */
/*   placing them as arguments within another "and" expression, but it   */
/*   is more efficient to add the arguments of one of the "and"          */
/*   expressions to the list of arguments for the other and expression). */
/*************************************************************************/
globle struct expr *CombineExpressions(
  void *theEnv,
  EXEC_STATUS,
  struct expr *expr1,
  struct expr *expr2)
  {
   struct expr *tempPtr;

   /*===========================================================*/
   /* If the 1st expression is NULL, return the 2nd expression. */
   /*===========================================================*/

   if (expr1 == NULL) return(expr2);

   /*===========================================================*/
   /* If the 2nd expression is NULL, return the 1st expression. */
   /*===========================================================*/

   if (expr2 == NULL) return(expr1);

   /*============================================================*/
   /* If the 1st expression is an "and" expression, and the 2nd  */
   /* expression is not an "and" expression, then include the    */
   /* 2nd expression in the argument list of the 1st expression. */
   /*============================================================*/

   if ((expr1->value == ExpressionData(theEnv,execStatus)->PTR_AND) &&
       (expr2->value != ExpressionData(theEnv,execStatus)->PTR_AND))
     {
      tempPtr = expr1->argList;
      if (tempPtr == NULL)
        {
         rtn_struct(theEnv,execStatus,expr,expr1);
         return(expr2);
        }

      while (tempPtr->nextArg != NULL)
        { tempPtr = tempPtr->nextArg; }

      tempPtr->nextArg = expr2;
      return(expr1);
     }

   /*============================================================*/
   /* If the 2nd expression is an "and" expression, and the 1st  */
   /* expression is not an "and" expression, then include the    */
   /* 1st expression in the argument list of the 2nd expression. */
   /*============================================================*/

   if ((expr1->value != ExpressionData(theEnv,execStatus)->PTR_AND) &&
       (expr2->value == ExpressionData(theEnv,execStatus)->PTR_AND))
     {
      tempPtr = expr2->argList;
      if (tempPtr == NULL)
        {
         rtn_struct(theEnv,execStatus,expr,expr2);
         return(expr1);
        }

      expr2->argList = expr1;
      expr1->nextArg = tempPtr;

      return(expr2);
     }

   /*===========================================================*/
   /* If both expressions are "and" expressions, then add the   */
   /* 2nd expression to the argument list of the 1st expression */
   /* and throw away the extraneous "and" expression.           */
   /*===========================================================*/

   if ((expr1->value == ExpressionData(theEnv,execStatus)->PTR_AND) &&
       (expr2->value == ExpressionData(theEnv,execStatus)->PTR_AND))
     {
      tempPtr = expr1->argList;
      if (tempPtr == NULL)
        {
         rtn_struct(theEnv,execStatus,expr,expr1);
         return(expr2);
        }

      while (tempPtr->nextArg != NULL)
        { tempPtr = tempPtr->nextArg; }

      tempPtr->nextArg = expr2->argList;
      rtn_struct(theEnv,execStatus,expr,expr2);

      return(expr1);
     }

   /*=====================================================*/
   /* If neither expression is an "and" expression, then  */
   /* create an "and" expression and add both expressions */
   /* to the argument list of that "and" expression.      */
   /*=====================================================*/

   tempPtr = GenConstant(theEnv,execStatus,FCALL,ExpressionData(theEnv,execStatus)->PTR_AND);
   tempPtr->argList = expr1;
   expr1->nextArg = expr2;
   return(tempPtr);
  }

/********************************************************/
/* AppendExpressions: Attaches an expression to the end */
/*   of another expression's nextArg list.              */
/********************************************************/
globle struct expr *AppendExpressions(
  struct expr *expr1,
  struct expr *expr2)
  {
   struct expr *tempPtr;

   /*===========================================================*/
   /* If the 1st expression is NULL, return the 2nd expression. */
   /*===========================================================*/

   if (expr1 == NULL) return(expr2);

   /*===========================================================*/
   /* If the 2nd expression is NULL, return the 1st expression. */
   /*===========================================================*/

   if (expr2 == NULL) return(expr1);

   /*====================================*/
   /* Find the end of the 1st expression */
   /* and attach the 2nd expression.     */
   /*====================================*/

   tempPtr = expr1;
   while (tempPtr->nextArg != NULL) tempPtr = tempPtr->nextArg;
   tempPtr->nextArg = expr2;

   /*===============================*/
   /* Return the merged expression. */
   /*===============================*/

   return(expr1);
  }


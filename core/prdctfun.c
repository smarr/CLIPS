   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*              PREDICATE FUNCTIONS MODULE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for several predicate          */
/*   functions including not, and, or, eq, neq, <=, >=, <,   */
/*   >, =, <>, symbolp, stringp, lexemep, numberp, integerp, */
/*   floatp, oddp, evenp, multifieldp, sequencep, and        */
/*   pointerp.                                               */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _PRDCTFUN_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "setup.h"

#include "exprnpsr.h"
#include "argacces.h"
#include "multifld.h"
#include "router.h"

#include "prdctfun.h"

#if ! RUN_TIME
/**************************************************/
/* PredicateFunctionDefinitions: Defines standard */
/*   math and predicate functions.                */
/**************************************************/
globle void PredicateFunctionDefinitions()
  {
   DefineFunction2("not", 'b', NotFunction, "NotFunction", "11");
   DefineFunction2("and", 'b', AndFunction, "AndFunction", "2*");
   DefineFunction2("or", 'b', OrFunction, "OrFunction", "2*");

   DefineFunction2("eq", 'b', EqFunction, "EqFunction", "2*");
   DefineFunction2("neq", 'b', NeqFunction, "NeqFunction", "2*");

   DefineFunction2("<=", 'b', LessThanOrEqualFunction, "LessThanOrEqualFunction", "2*n");
   DefineFunction2(">=", 'b', GreaterThanOrEqualFunction, "GreaterThanOrEqualFunction", "2*n");
   DefineFunction2("<", 'b', LessThanFunction, "LessThanFunction", "2*n");
   DefineFunction2(">", 'b', GreaterThanFunction, "GreaterThanFunction", "2*n");
   DefineFunction2("=", 'b', NumericEqualFunction, "NumericEqualFunction", "2*n");
   DefineFunction2("<>", 'b', NumericNotEqualFunction, "NumericNotEqualFunction", "2*n");
   DefineFunction2("!=", 'b', NumericNotEqualFunction, "NumericNotEqualFunction", "2*n");

   DefineFunction2("symbolp", 'b', SymbolpFunction, "SymbolpFunction", "11");
   DefineFunction2("wordp", 'b', SymbolpFunction, "SymbolpFunction", "11");
   DefineFunction2("stringp", 'b', StringpFunction, "StringpFunction", "11");
   DefineFunction2("lexemep", 'b', LexemepFunction, "LexemepFunction", "11");
   DefineFunction2("numberp", 'b', NumberpFunction, "NumberpFunction", "11");
   DefineFunction2("integerp", 'b', IntegerpFunction, "IntegerpFunction", "11");
   DefineFunction2("floatp", 'b', FloatpFunction, "FloatpFunction", "11");
   DefineFunction2("oddp", 'b', OddpFunction, "OddpFunction", "11i");
   DefineFunction2("evenp", 'b', EvenpFunction, "EvenpFunction", "11i");
   DefineFunction2("multifieldp",'b', MultifieldpFunction, "MultifieldpFunction", "11");
   DefineFunction2("sequencep",'b', MultifieldpFunction, "MultifieldpFunction", "11");
   DefineFunction2("pointerp", 'b', PointerpFunction, "PointerpFunction", "11");
  }
#endif

/************************************/
/* EqFunction: H/L access routine   */
/*   for the eq function.           */
/************************************/
globle BOOLEAN EqFunction()
  {
   DATA_OBJECT item, nextItem;
   int numArgs, i;
   struct expr *theExpression;

   /*====================================*/
   /* Determine the number of arguments. */
   /*====================================*/

   numArgs = RtnArgCount();
   if (numArgs == 0) return(FALSE);

   /*==============================================*/
   /* Get the value of the first argument against  */
   /* which subsequent arguments will be compared. */
   /*==============================================*/

   theExpression = GetFirstArgument();
   EvaluateExpression(theExpression,&item);

   /*=====================================*/
   /* Compare all arguments to the first. */
   /* If any are the same, return FALSE.  */
   /*=====================================*/

   theExpression = GetNextArgument(theExpression);
   for (i = 2 ; i <= numArgs ; i++)
     {
      EvaluateExpression(theExpression,&nextItem);

      if (GetType(nextItem) != GetType(item))
        { return(FALSE); }

      if (GetType(nextItem) == MULTIFIELD)
        {
         if (MultifieldDOsEqual(&nextItem,&item) == FALSE)
           { return(FALSE); }
        }
      else if (nextItem.value != item.value)
        { return(FALSE); }

      theExpression = GetNextArgument(theExpression);
     }

   /*=====================================*/
   /* All of the arguments were different */
   /* from the first. Return TRUE.        */
   /*=====================================*/

   return(TRUE);
  }

/*************************************/
/* NeqFunction: H/L access routine   */
/*   for the neq function.           */
/*************************************/
globle BOOLEAN NeqFunction()
  {
   DATA_OBJECT item, nextItem;
   int numArgs, i;
   struct expr *theExpression;

   /*====================================*/
   /* Determine the number of arguments. */
   /*====================================*/

   numArgs = RtnArgCount();
   if (numArgs == 0) return(FALSE);

   /*==============================================*/
   /* Get the value of the first argument against  */
   /* which subsequent arguments will be compared. */
   /*==============================================*/

   theExpression = GetFirstArgument();
   EvaluateExpression(theExpression,&item);

   /*=====================================*/
   /* Compare all arguments to the first. */
   /* If any are different, return FALSE. */
   /*=====================================*/

   for (i = 2, theExpression = GetNextArgument(theExpression);
        i <= numArgs;
        i++, theExpression = GetNextArgument(theExpression))
     {
      EvaluateExpression(theExpression,&nextItem);
      if (GetType(nextItem) != GetType(item))
        { continue; }
      else if (nextItem.type == MULTIFIELD)
        {
         if (MultifieldDOsEqual(&nextItem,&item) == TRUE)
           { return(FALSE); }
        }
      else if (nextItem.value == item.value)
        { return(FALSE); }
     }

   /*=====================================*/
   /* All of the arguments were identical */
   /* to the first. Return TRUE.          */
   /*=====================================*/

   return(TRUE);
  }

/*****************************************/
/* StringpFunction: H/L access routine   */
/*   for the stringp function.           */
/*****************************************/
globle BOOLEAN StringpFunction()
  {
   DATA_OBJECT item;

   if (ArgCountCheck("stringp",EXACTLY,1) == -1) return(FALSE);

   RtnUnknown(1,&item);

   if (GetType(item) == STRING)
     { return(TRUE); }
   else
     { return(FALSE); }
  }

/*****************************************/
/* SymbolpFunction: H/L access routine   */
/*   for the symbolp function.           */
/*****************************************/
globle BOOLEAN SymbolpFunction()
  {
   DATA_OBJECT item;

   if (ArgCountCheck("symbolp",EXACTLY,1) == -1) return(FALSE);

   RtnUnknown(1,&item);

   if (GetType(item) == SYMBOL)
     { return(TRUE); }
   else
     { return(FALSE); }
  }

/*****************************************/
/* LexemepFunction: H/L access routine   */
/*   for the lexemep function.           */
/*****************************************/
globle BOOLEAN LexemepFunction()
  {
   DATA_OBJECT item;

   if (ArgCountCheck("lexemep",EXACTLY,1) == -1) return(FALSE);

   RtnUnknown(1,&item);

   if ((GetType(item) == SYMBOL) || (GetType(item) == STRING))
     { return(TRUE); }
   else
     { return(FALSE); }
  }

/*****************************************/
/* NumberpFunction: H/L access routine   */
/*   for the numberp function.           */
/*****************************************/
globle BOOLEAN NumberpFunction()
  {
   DATA_OBJECT item;

   if (ArgCountCheck("numberp",EXACTLY,1) == -1) return(FALSE);

   RtnUnknown(1,&item);

   if ((GetType(item) == FLOAT) || (GetType(item) == INTEGER))
     { return(TRUE); }
   else
     { return(FALSE); }
  }

/****************************************/
/* FloatpFunction: H/L access routine   */
/*   for the floatp function.           */
/****************************************/
globle BOOLEAN FloatpFunction()
  {
   DATA_OBJECT item;

   if (ArgCountCheck("floatp",EXACTLY,1) == -1) return(FALSE);

   RtnUnknown(1,&item);

   if (GetType(item) == FLOAT)
     { return(TRUE); }
   else
     { return(FALSE); }
  }

/******************************************/
/* IntegerpFunction: H/L access routine   */
/*   for the integerp function.           */
/******************************************/
globle BOOLEAN IntegerpFunction()
  {
   DATA_OBJECT item;

   if (ArgCountCheck("integerp",EXACTLY,1) == -1) return(FALSE);

   RtnUnknown(1,&item);

   if (GetType(item) != INTEGER) return(FALSE);

   return(TRUE);
  }

/*********************************************/
/* MultifieldpFunction: H/L access routine   */
/*   for the multifieldp function.           */
/*********************************************/
globle BOOLEAN MultifieldpFunction()
  {
   DATA_OBJECT item;

   if (ArgCountCheck("multifieldp",EXACTLY,1) == -1) return(FALSE);

   RtnUnknown(1,&item);

   if (GetType(item) != MULTIFIELD) return(FALSE);

   return(TRUE);
  }

/******************************************/
/* PointerpFunction: H/L access routine   */
/*   for the pointerp function.           */
/******************************************/
globle BOOLEAN PointerpFunction()
  {
   DATA_OBJECT item;

   if (ArgCountCheck("pointerp",EXACTLY,1) == -1) return(FALSE);

   RtnUnknown(1,&item);

   if (GetType(item) != EXTERNAL_ADDRESS) return(FALSE);

   return(TRUE);
  }

/*************************************/
/* NotFunction: H/L access routine   */
/*   for the not function.           */
/*************************************/
globle BOOLEAN NotFunction()
  {
   EXPRESSION *theArgument;
   DATA_OBJECT result;

   theArgument = GetFirstArgument();
   if (theArgument == NULL) { return(FALSE); }

   if (EvaluateExpression(theArgument,&result)) return(FALSE);

   if ((result.value == FalseSymbol) && (result.type == SYMBOL))
     { return(TRUE); }

   return(FALSE);
  }

/*************************************/
/* AndFunction: H/L access routine   */
/*   for the and function.           */
/*************************************/
globle BOOLEAN AndFunction()
  {
   EXPRESSION *theArgument;
   DATA_OBJECT result;

   for (theArgument = GetFirstArgument();
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument))
     {
      if (EvaluateExpression(theArgument,&result)) return(FALSE);
      if ((result.value == FalseSymbol) && (result.type == SYMBOL))
        { return(FALSE); }
     }

   return(TRUE);
  }

/************************************/
/* OrFunction: H/L access routine   */
/*   for the or function.           */
/************************************/
globle BOOLEAN OrFunction()
  {
   EXPRESSION *theArgument;
   DATA_OBJECT result;

   for (theArgument = GetFirstArgument();
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument))
     {
      if (EvaluateExpression(theArgument,&result)) return(FALSE);

      if ((result.value != FalseSymbol) || (result.type != SYMBOL))
        { return(TRUE); }
     }

   return(FALSE);
  }

/*****************************************/
/* LessThanOrEqualFunction: H/L access   */
/*   routine for the <= function.        */
/*****************************************/
globle BOOLEAN LessThanOrEqualFunction()
  {
   EXPRESSION *theArgument;
   DATA_OBJECT rv1, rv2;
   int pos = 1;

   /*=========================*/
   /* Get the first argument. */
   /*=========================*/

   theArgument = GetFirstArgument();
   if (theArgument == NULL) { return(TRUE); }
   if (! GetNumericArgument(theArgument,"<=",&rv1,FALSE,pos)) return(FALSE);
   pos++;

   /*====================================================*/
   /* Compare each of the subsequent arguments to its    */
   /* predecessor. If any is greater, then return FALSE. */
   /*====================================================*/

   for (theArgument = GetNextArgument(theArgument);
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument), pos++)
     {
      if (! GetNumericArgument(theArgument,"<=",&rv2,FALSE,pos)) return(FALSE);
      if (rv1.type == INTEGER)
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToLong(rv1.value) > ValueToLong(rv2.value))
              { return(FALSE); }
           }
         else
           {
            if ((double) ValueToLong(rv1.value) > ValueToDouble(rv2.value))
              { return(FALSE); }
           }
        }
      else
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToDouble(rv1.value) > (double) ValueToLong(rv2.value))
              { return(FALSE); }
           }
         else
           {
            if (ValueToDouble(rv1.value) > ValueToDouble(rv2.value))
              { return(FALSE); }
           }
        }

      rv1.type = rv2.type;
      rv1.value = rv2.value;
     }

   /*======================================*/
   /* Each argument was less than or equal */
   /* to it predecessor. Return TRUE.      */
   /*======================================*/

   return(TRUE);
  }

/********************************************/
/* GreaterThanOrEqualFunction: H/L access   */
/*   routine for the >= function.           */
/********************************************/
globle BOOLEAN GreaterThanOrEqualFunction()
  {
   EXPRESSION *theArgument;
   DATA_OBJECT rv1, rv2;
   int pos = 1;

   /*=========================*/
   /* Get the first argument. */
   /*=========================*/

   theArgument = GetFirstArgument();
   if (theArgument == NULL) { return(TRUE); }
   if (! GetNumericArgument(theArgument,">=",&rv1,FALSE,pos)) return(FALSE);
   pos++;

   /*===================================================*/
   /* Compare each of the subsequent arguments to its   */
   /* predecessor. If any is lesser, then return FALSE. */
   /*===================================================*/

   for (theArgument = GetNextArgument(theArgument);
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument), pos++)
     {
      if (! GetNumericArgument(theArgument,">=",&rv2,FALSE,pos)) return(FALSE);
      if (rv1.type == INTEGER)
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToLong(rv1.value) < ValueToLong(rv2.value))
              { return(FALSE); }
           }
         else
           {
            if ((double) ValueToLong(rv1.value) < ValueToDouble(rv2.value))
              { return(FALSE); }
           }
        }
      else
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToDouble(rv1.value) < (double) ValueToLong(rv2.value))
              { return(FALSE); }
           }
         else
           {
            if (ValueToDouble(rv1.value) < ValueToDouble(rv2.value))
              { return(FALSE); }
           }
        }

      rv1.type = rv2.type;
      rv1.value = rv2.value;
     }

   /*=========================================*/
   /* Each argument was greater than or equal */
   /* to its predecessor. Return TRUE.        */
   /*=========================================*/

   return(TRUE);
  }

/**********************************/
/* LessThanFunction: H/L access   */
/*   routine for the < function.  */
/**********************************/
globle BOOLEAN LessThanFunction()
  {
   EXPRESSION *theArgument;
   DATA_OBJECT rv1, rv2;
   int pos = 1;

   /*=========================*/
   /* Get the first argument. */
   /*=========================*/

   theArgument = GetFirstArgument();
   if (theArgument == NULL) { return(TRUE); }
   if (! GetNumericArgument(theArgument,"<",&rv1,FALSE,pos)) return(FALSE);
   pos++;

   /*==========================================*/
   /* Compare each of the subsequent arguments */
   /* to its predecessor. If any is greater or */
   /* equal, then return FALSE.                */
   /*==========================================*/

   for (theArgument = GetNextArgument(theArgument);
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument), pos++)
     {
      if (! GetNumericArgument(theArgument,"<",&rv2,FALSE,pos)) return(FALSE);
      if (rv1.type == INTEGER)
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToLong(rv1.value) >= ValueToLong(rv2.value))
              { return(FALSE); }
           }
         else
           {
            if ((double) ValueToLong(rv1.value) >= ValueToDouble(rv2.value))
              { return(FALSE); }
           }
        }
      else
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToDouble(rv1.value) >= (double) ValueToLong(rv2.value))
              { return(FALSE); }
           }
         else
           {
            if (ValueToDouble(rv1.value) >= ValueToDouble(rv2.value))
              { return(FALSE); }
           }
        }

      rv1.type = rv2.type;
      rv1.value = rv2.value;
     }

   /*=================================*/
   /* Each argument was less than its */
   /* predecessor. Return TRUE.       */
   /*=================================*/

   return(TRUE);
  }

/*************************************/
/* GreaterThanFunction: H/L access   */
/*   routine for the > function.     */
/*************************************/
globle BOOLEAN GreaterThanFunction()
  {
   EXPRESSION *theArgument;
   DATA_OBJECT rv1, rv2;
   int pos = 1;

   /*=========================*/
   /* Get the first argument. */
   /*=========================*/

   theArgument = GetFirstArgument();
   if (theArgument == NULL) { return(TRUE); }
   if (! GetNumericArgument(theArgument,">",&rv1,FALSE,pos)) return(FALSE);
   pos++;

   /*==========================================*/
   /* Compare each of the subsequent arguments */
   /* to its predecessor. If any is lesser or  */
   /* equal, then return FALSE.                */
   /*==========================================*/

   for (theArgument = GetNextArgument(theArgument);
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument), pos++)
     {
      if (! GetNumericArgument(theArgument,">",&rv2,FALSE,pos)) return(FALSE);
      if (rv1.type == INTEGER)
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToLong(rv1.value) <= ValueToLong(rv2.value))
              { return(FALSE); }
           }
         else
           {
            if ((double) ValueToLong(rv1.value) <= ValueToDouble(rv2.value))
              { return(FALSE); }
           }
        }
      else
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToDouble(rv1.value) <= (double) ValueToLong(rv2.value))
              { return(FALSE); }
           }
         else
           {
            if (ValueToDouble(rv1.value) <= ValueToDouble(rv2.value))
              { return(FALSE); }
           }
        }

      rv1.type = rv2.type;
      rv1.value = rv2.value;
     }

   /*================================*/
   /* Each argument was greater than */
   /* its predecessor. Return TRUE.  */
   /*================================*/

   return(TRUE);
  }

/**************************************/
/* NumericEqualFunction: H/L access   */
/*   routine for the = function.      */
/**************************************/
globle BOOLEAN NumericEqualFunction()
  {
   EXPRESSION *theArgument;
   DATA_OBJECT rv1, rv2;
   int pos = 1;

   /*=========================*/
   /* Get the first argument. */
   /*=========================*/

   theArgument = GetFirstArgument();

   if (theArgument == NULL) { return(TRUE); }
   if (! GetNumericArgument(theArgument,"=",&rv1,FALSE,pos)) return(FALSE);
   pos++;

   /*=================================================*/
   /* Compare each of the subsequent arguments to the */
   /* first. If any is unequal, then return FALSE.    */
   /*=================================================*/

   for (theArgument = GetNextArgument(theArgument);
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument), pos++)
     {
      if (! GetNumericArgument(theArgument,"=",&rv2,FALSE,pos)) return(FALSE);
      if (rv1.type == INTEGER)
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToLong(rv1.value) != ValueToLong(rv2.value))
              { return(FALSE); }
           }
         else
           {
            if ((double) ValueToLong(rv1.value) != ValueToDouble(rv2.value))
              { return(FALSE); }
           }
        }
      else
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToDouble(rv1.value) != (double) ValueToLong(rv2.value))
              { return(FALSE); }
           }
         else
           {
            if (ValueToDouble(rv1.value) != ValueToDouble(rv2.value))
              { return(FALSE); }
           }
        }
     }

   /*=================================*/
   /* All arguments were equal to the */
   /* first argument. Return TRUE.    */
   /*=================================*/

   return(TRUE);
  }

/*****************************************/
/* NumericNotEqualFunction: H/L access   */
/*   routine for the <> function.        */
/*****************************************/
globle BOOLEAN NumericNotEqualFunction()
  {
   EXPRESSION *theArgument;
   DATA_OBJECT rv1, rv2;
   int pos = 1;

   /*=========================*/
   /* Get the first argument. */
   /*=========================*/

   theArgument = GetFirstArgument();
   if (theArgument == NULL) { return(TRUE); }
   if (! GetNumericArgument(theArgument,"<>",&rv1,FALSE,pos)) return(FALSE);
   pos++;

   /*=================================================*/
   /* Compare each of the subsequent arguments to the */
   /* first. If any is equal, then return FALSE.      */
   /*=================================================*/

   for (theArgument = GetNextArgument(theArgument);
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument), pos++)
     {
      if (! GetNumericArgument(theArgument,"<>",&rv2,FALSE,pos)) return(FALSE);
      if (rv1.type == INTEGER)
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToLong(rv1.value) == ValueToLong(rv2.value))
              { return(FALSE); }
           }
         else
           {
            if ((double) ValueToLong(rv1.value) == ValueToDouble(rv2.value))
              { return(FALSE); }
           }
        }
      else
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToDouble(rv1.value) == (double) ValueToLong(rv2.value))
              { return(FALSE); }
           }
         else
           {
            if (ValueToDouble(rv1.value) == ValueToDouble(rv2.value))
              { return(FALSE); }
           }
        }
     }

   /*===================================*/
   /* All arguments were unequal to the */
   /* first argument. Return TRUE.      */
   /*===================================*/

   return(TRUE);
  }

/**************************************/
/* OddpFunction: H/L access routine   */
/*   for the oddp function.           */
/**************************************/
globle BOOLEAN OddpFunction()
  {
   DATA_OBJECT item;
   long num, halfnum;

   if (ArgCountCheck("oddp",EXACTLY,1) == -1) return(FALSE);
   if (ArgTypeCheck("oddp",1,INTEGER,&item) == FALSE) return(FALSE);

   num = DOToLong(item);

   halfnum = (num / 2) * 2;
   if (num == halfnum) return(FALSE);

   return(TRUE);
  }

/***************************************/
/* EvenpFunction: H/L access routine   */
/*   for the evenp function.           */
/***************************************/
globle BOOLEAN EvenpFunction()
  {
   DATA_OBJECT item;
   long num, halfnum;

   if (ArgCountCheck("evenp",EXACTLY,1) == -1) return(FALSE);
   if (ArgTypeCheck("evenp",1,INTEGER,&item) == FALSE) return(FALSE);

   num = DOToLong(item);

   halfnum = (num / 2) * 2;
   if (num != halfnum) return(FALSE);

   return(TRUE);
  }




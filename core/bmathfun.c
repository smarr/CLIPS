   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*             BASIC MATH FUNCTIONS MODULE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for numerous basic math        */
/*   functions including +, *, -, /, integer, float, div,    */
/*   abs,set-auto-float-dividend, get-auto-float-dividend,   */
/*   min, and max.                                           */
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

#define _BMATHFUN_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "setup.h"

#include "exprnpsr.h"
#include "argacces.h"
#include "router.h"

#include "bmathfun.h"

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static BOOLEAN          AutoFloatDividend = TRUE;

/***************************************************************/
/* BasicMathFunctionDefinitions: Defines basic math functions. */
/***************************************************************/
globle void BasicMathFunctionDefinitions()
  {
#if ! RUN_TIME
   DefineFunction2("+", 'n',PTIF AdditionFunction, "AdditionFunction", "2*n");
   DefineFunction2("*", 'n', PTIF MultiplicationFunction, "MultiplicationFunction", "2*n");
   DefineFunction2("-", 'n', PTIF SubtractionFunction, "SubtractionFunction", "2*n");
   DefineFunction2("/", 'n', PTIF DivisionFunction, "DivisionFunction", "2*n");
   DefineFunction2("div", 'l', PTIF DivFunction, "DivFunction", "2*n");

   DefineFunction2("set-auto-float-dividend", 'b',
                   SetAutoFloatDividendCommand, "SetAutoFloatDividendCommand", "11");
   DefineFunction2("get-auto-float-dividend", 'b',
                  GetAutoFloatDividendCommand, "GetAutoFloatDividendCommand", "00");

   DefineFunction2("integer", 'l', PTIF IntegerFunction, "IntegerFunction", "11n");
   DefineFunction2("float", 'd', PTIF FloatFunction, "FloatFunction", "11n");
   DefineFunction2("abs", 'n', PTIF AbsFunction, "AbsFunction", "11n");
   DefineFunction2("min", 'n', PTIF MinFunction, "MinFunction", "2*n");
   DefineFunction2("max", 'n', PTIF MaxFunction, "MaxFunction", "2*n");
#endif
  }

/**********************************/
/* AdditionFunction: H/L access   */
/*   routine for the + function.  */
/**********************************/
globle void AdditionFunction(
  DATA_OBJECT_PTR returnValue)
  {
   double ftotal = 0.0;
   long ltotal = 0L;
   BOOLEAN useFloatTotal = FALSE;
   EXPRESSION *theExpression;
   DATA_OBJECT theArgument;
   int pos = 1;

   /*=================================================*/
   /* Loop through each of the arguments adding it to */
   /* a running total. If a floating point number is  */
   /* encountered, then do all subsequent operations  */
   /* using floating point values.                    */
   /*=================================================*/

   theExpression = GetFirstArgument();

   while (theExpression != NULL)
     {
      if (! GetNumericArgument(theExpression,"+",&theArgument,useFloatTotal,pos)) theExpression = NULL;
      else theExpression = GetNextArgument(theExpression);

      if (useFloatTotal)
        { ftotal += ValueToDouble(theArgument.value); }
      else
        {
         if (theArgument.type == INTEGER)
           { ltotal += ValueToLong(theArgument.value); }
         else
           {
            ftotal = (double) ltotal + ValueToDouble(theArgument.value);
            useFloatTotal = TRUE;
           }
        }

      pos++;
     }

   /*======================================================*/
   /* If a floating point number was in the argument list, */
   /* then return a float, otherwise return an integer.    */
   /*======================================================*/

   if (useFloatTotal)
     {
      returnValue->type = FLOAT;
      returnValue->value = (void *) AddDouble(ftotal);
     }
   else
     {
      returnValue->type = INTEGER;
      returnValue->value = (void *) AddLong(ltotal);
     }
  }

/****************************************/
/* MultiplicationFunction: CLIPS access */
/*   routine for the * function.        */
/****************************************/
globle void MultiplicationFunction(
  DATA_OBJECT_PTR returnValue)
  {
   double ftotal = 1.0;
   long ltotal = 1L;
   BOOLEAN useFloatTotal = FALSE;
   EXPRESSION *theExpression;
   DATA_OBJECT theArgument;
   int pos = 1;

   /*===================================================*/
   /* Loop through each of the arguments multiplying it */
   /* by a running product. If a floating point number  */
   /* is encountered, then do all subsequent operations */
   /* using floating point values.                      */
   /*===================================================*/

   theExpression = GetFirstArgument();

   while (theExpression != NULL)
     {
      if (! GetNumericArgument(theExpression,"*",&theArgument,useFloatTotal,pos)) theExpression = NULL;
      else theExpression = GetNextArgument(theExpression);

      if (useFloatTotal)
        { ftotal *= ValueToDouble(theArgument.value); }
      else
        {
         if (theArgument.type == INTEGER)
           { ltotal *= ValueToLong(theArgument.value); }
         else
           {
            ftotal = (double) ltotal * ValueToDouble(theArgument.value);
            useFloatTotal = TRUE;
           }
        }
      pos++;
     }

   /*======================================================*/
   /* If a floating point number was in the argument list, */
   /* then return a float, otherwise return an integer.    */
   /*======================================================*/

   if (useFloatTotal)
     {
      returnValue->type = FLOAT;
      returnValue->value = (void *) AddDouble(ftotal);
     }
   else
     {
      returnValue->type = INTEGER;
      returnValue->value = (void *) AddLong(ltotal);
     }
  }

/*************************************/
/* SubtractionFunction: CLIPS access */
/*   routine for the - function.     */
/*************************************/
globle void SubtractionFunction(
  DATA_OBJECT_PTR returnValue)
  {
   double ftotal = 0.0;
   long ltotal = 0L;
   BOOLEAN useFloatTotal = FALSE;
   EXPRESSION *theExpression;
   DATA_OBJECT theArgument;
   int pos = 1;

   /*=================================================*/
   /* Get the first argument. This number which will  */
   /* be the starting total from which all subsequent */
   /* arguments will subtracted.                      */
   /*=================================================*/

   theExpression = GetFirstArgument();
   if (theExpression != NULL)
     {
      if (! GetNumericArgument(theExpression,"-",&theArgument,useFloatTotal,pos)) theExpression = NULL;
      else theExpression = GetNextArgument(theExpression);

      if (theArgument.type == INTEGER)
        { ltotal = ValueToLong(theArgument.value); }
      else
        {
         ftotal = ValueToDouble(theArgument.value);
         useFloatTotal = TRUE;
        }
      pos++;
     }

   /*===================================================*/
   /* Loop through each of the arguments subtracting it */
   /* from a running total. If a floating point number  */
   /* is encountered, then do all subsequent operations */
   /* using floating point values.                      */
   /*===================================================*/

   while (theExpression != NULL)
     {
      if (! GetNumericArgument(theExpression,"-",&theArgument,useFloatTotal,pos)) theExpression = NULL;
      else theExpression = GetNextArgument(theExpression);

      if (useFloatTotal)
        { ftotal -= ValueToDouble(theArgument.value); }
      else
        {
         if (theArgument.type == INTEGER)
           { ltotal -= ValueToLong(theArgument.value); }
         else
           {
            ftotal = (double) ltotal - ValueToDouble(theArgument.value);
            useFloatTotal = TRUE;
           }
        }
      pos++;
     }

   /*======================================================*/
   /* If a floating point number was in the argument list, */
   /* then return a float, otherwise return an integer.    */
   /*======================================================*/

   if (useFloatTotal)
     {
      returnValue->type = FLOAT;
      returnValue->value = (void *) AddDouble(ftotal);
     }
   else
     {
      returnValue->type = INTEGER;
      returnValue->value = (void *) AddLong(ltotal);
     }
  }

/***********************************/
/* DivisionFunction:  CLIPS access */
/*   routine for the / function.   */
/***********************************/
globle void DivisionFunction(
  DATA_OBJECT_PTR returnValue)
  {
   double ftotal = 1.0;
   long ltotal = 1L;
   BOOLEAN useFloatTotal = AutoFloatDividend;
   EXPRESSION *theExpression;
   DATA_OBJECT theArgument;
   int pos = 1;

   /*===================================================*/
   /* Get the first argument. This number which will be */
   /* the starting product from which all subsequent    */
   /* arguments will divide. If the auto float dividend */
   /* feature is enable, then this number is converted  */
   /* to a float if it is an integer.                   */
   /*===================================================*/

   theExpression = GetFirstArgument();
   if (theExpression != NULL)
     {
      if (! GetNumericArgument(theExpression,"/",&theArgument,useFloatTotal,pos)) theExpression = NULL;
      else theExpression = GetNextArgument(theExpression);

      if (theArgument.type == INTEGER)
        { ltotal = ValueToLong(theArgument.value); }
      else
        {
         ftotal = ValueToDouble(theArgument.value);
         useFloatTotal = TRUE;
        }
      pos++;
     }

   /*====================================================*/
   /* Loop through each of the arguments dividing it     */
   /* into a running product. If a floating point number */
   /* is encountered, then do all subsequent operations  */
   /* using floating point values. Each argument is      */
   /* checked to prevent a divide by zero error.         */
   /*====================================================*/

   while (theExpression != NULL)
     {
      if (! GetNumericArgument(theExpression,"/",&theArgument,useFloatTotal,pos)) theExpression = NULL;
      else theExpression = GetNextArgument(theExpression);

      if ((theArgument.type == INTEGER) ? (ValueToLong(theArgument.value) == 0L) :
                                 ((theArgument.type == FLOAT) ? ValueToDouble(theArgument.value) == 0.0 : FALSE))
        {
         DivideByZeroErrorMessage("/");
         SetHaltExecution(TRUE);
         SetEvaluationError(TRUE);
         returnValue->type = FLOAT;
         returnValue->value = (void *) AddDouble(1.0);
         return;
        }

      if (useFloatTotal)
        { ftotal /= ValueToDouble(theArgument.value); }
      else
        {
         if (theArgument.type == INTEGER)
           { ltotal /= ValueToLong(theArgument.value); }
         else
           {
            ftotal = (double) ltotal / ValueToDouble(theArgument.value);
            useFloatTotal = TRUE;
           }
        }
      pos++;
     }

   /*======================================================*/
   /* If a floating point number was in the argument list, */
   /* then return a float, otherwise return an integer.    */
   /*======================================================*/

   if (useFloatTotal)
     {
      returnValue->type = FLOAT;
      returnValue->value = (void *) AddDouble(ftotal);
     }
   else
     {
      returnValue->type = INTEGER;
      returnValue->value = (void *) AddLong(ltotal);
     }
  }

/*************************************/
/* DivFunction: H/L access routine   */
/*   for the div function.           */
/*************************************/
globle long DivFunction()
  {
   long total = 1L;
   EXPRESSION *theExpression;
   DATA_OBJECT theArgument;
   int pos = 1;
   long theNumber;

   /*===================================================*/
   /* Get the first argument. This number which will be */
   /* the starting product from which all subsequent    */
   /* arguments will divide.                            */
   /*===================================================*/

   theExpression = GetFirstArgument();
   if (theExpression != NULL)
     {
      if (! GetNumericArgument(theExpression,"div",&theArgument,FALSE,pos)) theExpression = NULL;
      else theExpression = GetNextArgument(theExpression);

      if (theArgument.type == INTEGER)
        { total = ValueToLong(theArgument.value); }
      else
        { total = (long) ValueToDouble(theArgument.value); }
      pos++;
     }

   /*=====================================================*/
   /* Loop through each of the arguments dividing it into */
   /* a running product. Floats are converted to integers */
   /* and each argument is checked to prevent a divide by */
   /* zero error.                                         */
   /*=====================================================*/

   while (theExpression != NULL)
     {
      if (! GetNumericArgument(theExpression,"div",&theArgument,FALSE,pos)) theExpression = NULL;
      else theExpression = GetNextArgument(theExpression);

      if (theArgument.type == INTEGER) theNumber = ValueToLong(theArgument.value);
      else if (theArgument.type == FLOAT) theNumber = (long) ValueToDouble(theArgument.value);
      else theNumber = 1;

      if (theNumber == 0L)
        {
         DivideByZeroErrorMessage("div");
         SetHaltExecution(TRUE);
         SetEvaluationError(TRUE);
         return(1L);
        }

      if (theArgument.type == INTEGER)
        { total /= ValueToLong(theArgument.value); }
      else
        { total = total / (long) ValueToDouble(theArgument.value); }

      pos++;
     }

   /*======================================================*/
   /* The result of the div function is always an integer. */
   /*======================================================*/

   return(total);
  }

/*****************************************************/
/* SetAutoFloatDividendCommand: H/L access routine   */
/*   for the set-auto-float-dividend command.        */
/*****************************************************/
globle int SetAutoFloatDividendCommand()
  {
   int oldValue;
   DATA_OBJECT theArgument;

   /*===============================*/
   /* Remember the present setting. */
   /*===============================*/

   oldValue = AutoFloatDividend;

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/

   if (ArgCountCheck("set-auto-float-dividend",EXACTLY,1) == -1)
     { return(oldValue); }

   RtnUnknown(1,&theArgument);

   /*============================================================*/
   /* The symbol FALSE disables the auto float dividend feature. */
   /*============================================================*/

   if ((theArgument.value == FalseSymbol) && (theArgument.type == SYMBOL))
     { AutoFloatDividend = FALSE; }
   else
     { AutoFloatDividend = TRUE; }

   /*======================================*/
   /* Return the old value of the feature. */
   /*======================================*/

   return(oldValue);
  }

/*****************************************************/
/* GetAutoFloatDividendCommand: H/L access routine   */
/*   for the get-auto-float-dividend command.        */
/*****************************************************/
globle int GetAutoFloatDividendCommand()
  {
   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/

   ArgCountCheck("get-auto-float-dividend",EXACTLY,0);

   /*=============================*/
   /* Return the current setting. */
   /*=============================*/

   return(AutoFloatDividend);
  }

/**********************************************/
/* GetAutoFloatDividend: C access routine for */
/*   the get-auto-float-dividend command.     */
/**********************************************/
globle BOOLEAN GetAutoFloatDividend()
  {
   return(AutoFloatDividend);
  }

/**********************************************/
/* SetAutoFloatDividend: C access routine for */
/*   the set-auto-float-dividend command.     */
/**********************************************/
globle BOOLEAN SetAutoFloatDividend(
  int value)
  {
   int ov;

   ov = AutoFloatDividend;
   AutoFloatDividend = value;
   return(ov);
  }

/*****************************************/
/* IntegerFunction: H/L access routine   */
/*   for the integer function.           */
/*****************************************/
globle long int IntegerFunction()
  {
   DATA_OBJECT valstruct;

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/

   if (ArgCountCheck("integer",EXACTLY,1) == -1) return(0L);

   /*================================================================*/
   /* Check for the correct type of argument. Note that ArgTypeCheck */
   /* will convert floats to integers when an integer is requested   */
   /* (which is the purpose of the integer function).                */
   /*================================================================*/

   if (ArgTypeCheck("integer",1,INTEGER,&valstruct) == FALSE) return(0L);

   /*===================================================*/
   /* Return the numeric value converted to an integer. */
   /*===================================================*/

   return(ValueToLong(valstruct.value));
  }

/***************************************/
/* FloatFunction: H/L access routine   */
/*   for the float function.           */
/***************************************/
globle double FloatFunction()
  {
   DATA_OBJECT valstruct;

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/

   if (ArgCountCheck("float",EXACTLY,1) == -1) return(0.0);

   /*================================================================*/
   /* Check for the correct type of argument. Note that ArgTypeCheck */
   /* will convert integers to floats when a float is requested      */
   /* (which is the purpose of the float function).                  */
   /*================================================================*/

   if (ArgTypeCheck("float",1,FLOAT,&valstruct) == FALSE) return(0.0);

   /*================================================*/
   /* Return the numeric value converted to a float. */
   /*================================================*/

   return(ValueToDouble(valstruct.value));
  }

/*************************************/
/* AbsFunction: H/L access routine   */
/*   for the abs function.           */
/*************************************/
globle void AbsFunction(
  DATA_OBJECT_PTR returnValue)
  {
   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/

   if (ArgCountCheck("abs",EXACTLY,1) == -1)
     {
      returnValue->type = INTEGER;
      returnValue->value = (void *) AddLong(0L);
      return;
     }

   /*======================================*/
   /* Check that the argument is a number. */
   /*======================================*/

   if (ArgTypeCheck("abs",1,INTEGER_OR_FLOAT,returnValue) == FALSE)
     {
      returnValue->type = INTEGER;
      returnValue->value = (void *) AddLong(0L);
      return;
     }

   /*==========================================*/
   /* Return the absolute value of the number. */
   /*==========================================*/

   if (returnValue->type == INTEGER)
     {
      if (ValueToLong(returnValue->value) < 0L)
        { returnValue->value = (void *) AddLong(- ValueToLong(returnValue->value)); }
     }
   else if (ValueToDouble(returnValue->value) < 0.0)
     { returnValue->value = (void *) AddDouble(- ValueToDouble(returnValue->value)); }
  }

/*************************************/
/* MinFunction: H/L access routine   */
/*   for the min function.           */
/*************************************/
globle void MinFunction(
  DATA_OBJECT_PTR returnValue)
  {
   DATA_OBJECT argValue;
   int numberOfArguments, i;

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/

   if ((numberOfArguments = ArgCountCheck("min",AT_LEAST,1)) == -1)
     {
      returnValue->type = INTEGER;
      returnValue->value = (void *) AddLong(0L);
      return;
     }

   /*============================================*/
   /* Check that the first argument is a number. */
   /*============================================*/

   if (ArgTypeCheck("min",1,INTEGER_OR_FLOAT,returnValue) == FALSE)
     {
      returnValue->type = INTEGER;
      returnValue->value = (void *) AddLong(0L);
      return;
     }

   /*===========================================================*/
   /* Loop through the remaining arguments, first checking each */
   /* argument to see that it is a number, and then determining */
   /* if the argument is less than the previous arguments and   */
   /* is thus the minimum value.                                */
   /*===========================================================*/

   for (i = 2 ; i <= numberOfArguments ; i++)
     {
      if (ArgTypeCheck("min",i,INTEGER_OR_FLOAT,&argValue) == FALSE) return;

      if (returnValue->type == INTEGER)
        {
         if (argValue.type == INTEGER)
           {
            if (ValueToLong(returnValue->value) > ValueToLong(argValue.value))
              {
               returnValue->type = argValue.type;
               returnValue->value = argValue.value;
              }
           }
         else
           {
            if ((double) ValueToLong(returnValue->value) >
                         ValueToDouble(argValue.value))
              {
               returnValue->type = argValue.type;
               returnValue->value = argValue.value;
              }
           }
        }
      else
        {
         if (argValue.type == INTEGER)
           {
            if (ValueToDouble(returnValue->value) >
                (double) ValueToLong(argValue.value))
              {
               returnValue->type = argValue.type;
               returnValue->value = argValue.value;
              }
           }
         else
           {
            if (ValueToDouble(returnValue->value) > ValueToDouble(argValue.value))
              {
               returnValue->type = argValue.type;
               returnValue->value = argValue.value;
              }
           }
        }
     }

   return;
  }

/*************************************/
/* MaxFunction: H/L access routine   */
/*   for the max function.           */
/*************************************/
globle void MaxFunction(
  DATA_OBJECT_PTR returnValue)
  {
   DATA_OBJECT argValue;
   int numberOfArguments, i;

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/

   if ((numberOfArguments = ArgCountCheck("max",AT_LEAST,1)) == -1)
     {
      returnValue->type = INTEGER;
      returnValue->value = (void *) AddLong(0L);
      return;
     }

   /*============================================*/
   /* Check that the first argument is a number. */
   /*============================================*/

   if (ArgTypeCheck("max",1,INTEGER_OR_FLOAT,returnValue) == FALSE)
     {
      returnValue->type = INTEGER;
      returnValue->value = (void *) AddLong(0L);
      return;
     }

   /*===========================================================*/
   /* Loop through the remaining arguments, first checking each */
   /* argument to see that it is a number, and then determining */
   /* if the argument is greater than the previous arguments    */
   /* and is thus the maximum value.                            */
   /*===========================================================*/

   for (i = 2 ; i <= numberOfArguments ; i++)
     {
      if (ArgTypeCheck("max",i,INTEGER_OR_FLOAT,&argValue) == FALSE) return;

      if (returnValue->type == INTEGER)
        {
         if (argValue.type == INTEGER)
           {
            if (ValueToLong(returnValue->value) < ValueToLong(argValue.value))
              {
               returnValue->type = argValue.type;
               returnValue->value = argValue.value;
              }
           }
         else
           {
            if ((double) ValueToLong(returnValue->value) <
                         ValueToDouble(argValue.value))
              {
               returnValue->type = argValue.type;
               returnValue->value = argValue.value;
              }
           }
        }
      else
        {
         if (argValue.type == INTEGER)
           {
            if (ValueToDouble(returnValue->value) <
                (double) ValueToLong(argValue.value))
              {
               returnValue->type = argValue.type;
               returnValue->value = argValue.value;
              }
           }
         else
           {
            if (ValueToDouble(returnValue->value) < ValueToDouble(argValue.value))
              {
               returnValue->type = argValue.type;
               returnValue->value = argValue.value;
              }
           }
        }
     }

   return;
  }

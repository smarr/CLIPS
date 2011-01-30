   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  02/10/99            */
   /*                                                     */
   /*               STRING FUNCTIONS MODULE               */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for several string functions   */
/*   including str-cat, sym-cat, str-length, str-compare,    */
/*   upcase, lowcase, sub-string, str-index, eval, and       */
/*   build.                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Barry Cameron                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/*      MDT          | 10-Feb-1999 | DR832                   */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _STRNGFUN_SOURCE_

#include "setup.h"

#if STRING_FUNCTIONS

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <ctype.h>
#include <string.h>

#include "memalloc.h"
#include "extnfunc.h"
#include "argacces.h"
#include "router.h"
#include "strngrtr.h"
#include "scanner.h"
#include "prcdrpsr.h"
#include "cstrcpsr.h"
#include "exprnpsr.h"
#include "constrct.h"

#if DEFRULE_CONSTRUCT
#include "drive.h"
#endif

#include "strngfun.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    StrOrSymCatFunction(DATA_OBJECT_PTR,int);

#if ! RUN_TIME
/******************************************/
/* StringFunctionDefinitions: Initializes */
/*   the string manipulation functions.   */
/******************************************/
globle void StringFunctionDefinitions()
  {
   DefineFunction2("str-cat", 'k', PTIF StrCatFunction, "StrCatFunction", "1*");
   DefineFunction2("sym-cat", 'k', PTIF SymCatFunction, "SymCatFunction", "1*");
   DefineFunction2("str-length", 'l', PTIF StrLengthFunction, "StrLengthFunction", "11j");
   DefineFunction2("str-compare", 'l', PTIF StrCompareFunction, "StrCompareFunction", "23*jji");
   DefineFunction2("upcase", 'j', PTIF UpcaseFunction, "UpcaseFunction", "11j");
   DefineFunction2("lowcase", 'j', PTIF LowcaseFunction, "LowcaseFunction", "11j");
   DefineFunction2("sub-string", 's', PTIF SubStringFunction, "SubStringFunction", "33*iij");
   DefineFunction2("str-index", 'u', PTIF StrIndexFunction, "StrIndexFunction", "22j");
   DefineFunction2("eval", 'u', PTIF EvalFunction, "EvalFunction", "11k");
   DefineFunction2("build", 'b', PTIF BuildFunction, "BuildFunction", "11k");
  }
#endif

/****************************************/
/* StrCatFunction: H/L access routine   */
/*   for the str-cat function.          */
/****************************************/
globle void StrCatFunction(
  DATA_OBJECT_PTR returnValue)
  { StrOrSymCatFunction(returnValue,STRING); }

/****************************************/
/* SymCatFunction: H/L access routine   */
/*   for the sym-cat function.          */
/****************************************/
globle void SymCatFunction(
  DATA_OBJECT_PTR returnValue)
  {
   StrOrSymCatFunction(returnValue,SYMBOL);
  }

/********************************************************/
/* StrOrSymCatFunction: Driver routine for implementing */
/*   the str-cat and sym-cat functions.                 */
/********************************************************/
static void StrOrSymCatFunction(
  DATA_OBJECT_PTR returnValue,
  int returnType)
  {
   DATA_OBJECT theArg;
   int numArgs, i, total, j;
   char *theString, **arrayOfStrings;
   void *hashPtr;
   char *functionName;

   /*============================================*/
   /* Determine the calling function name.       */
   /* Store the null string or the symbol nil as */
   /* the return value in the event of an error. */
   /*============================================*/

   SetpType(returnValue,returnType);
   if (returnType == STRING)
     {
      functionName = "str-cat";
      SetpValue(returnValue,(void *) AddSymbol(""));
     }
   else
     {
      functionName = "sym-cat";
      SetpValue(returnValue,(void *) AddSymbol("nil"));
     }

   /*===============================================*/
   /* Determine the number of arguments as create a */
   /* string array which is large enough to store   */
   /* the string representation of each argument.   */
   /*===============================================*/

   numArgs = RtnArgCount();
   arrayOfStrings = (char **) gm1((int) sizeof(char *) * numArgs);

   total = 1;
   for (i = 1 ; i <= numArgs ; i++)
     {
      RtnUnknown(i,&theArg);
      IncrementSymbolCount(theArg.value);
     }
     
   /*=============================================*/
   /* Evaluate each argument and store its string */
   /* representation in the string array.         */
   /*=============================================*/

   for (i = 1 ; i <= numArgs ; i++)
     {
      RtnUnknown(i,&theArg);

      switch(GetType(theArg))
        {
         case STRING:
#if OBJECT_SYSTEM
         case INSTANCE_NAME:
#endif
         case SYMBOL:
           arrayOfStrings[i-1] = ValueToString(GetValue(theArg));
           break;

         case FLOAT:
           hashPtr = AddSymbol(FloatToString(ValueToDouble(GetValue(theArg))));
           arrayOfStrings[i-1] = ValueToString(hashPtr);
           break;

         case INTEGER:
           hashPtr = AddSymbol(LongIntegerToString(ValueToLong(GetValue(theArg))));
           arrayOfStrings[i-1] = ValueToString(hashPtr);
           break;

         default:
           ExpectedTypeError1(functionName,i,"string, instance name, symbol, float, or integer");
           SetEvaluationError(TRUE);
           break;
        }

      if (EvaluationError)
        {
         rm(arrayOfStrings,(int) sizeof(char *) * numArgs);
         return;
        }

      total += strlen(arrayOfStrings[i - 1]);
     }

   /*=========================================================*/
   /* Allocate the memory to store the concatenated string or */
   /* symbol, then copy the values in the string array to the */
   /* memory just allocated.                                  */
   /*=========================================================*/

   theString = (char *) gm2 (((int) sizeof(char) * total));

   j = 0;
   for (i = 0 ; i < numArgs ; i++)
     {
      sprintf(&theString[j],"%s",arrayOfStrings[i]);
      j += strlen(arrayOfStrings[i]);
     }

   /*=========================================*/
   /* Return the concatenated value and clean */
   /* up the temporary memory used.           */
   /*=========================================*/

   SetpValue(returnValue,(void *) AddSymbol(theString));
   rm(theString,(int) sizeof(char) * total);
   rm(arrayOfStrings,(int) sizeof(char *) * numArgs);
   
  }

/*******************************************/
/* StrLengthFunction: H/L access routine   */
/*   for the str-length function.          */
/*******************************************/
globle long int StrLengthFunction()
  {
   DATA_OBJECT theArg;

   /*===================================================*/
   /* Function str-length expects exactly one argument. */
   /*===================================================*/

   if (ArgCountCheck("str-length",EXACTLY,1) == -1)
     { return(-1L); }

   /*==================================================*/
   /* The argument should be of type symbol or string. */
   /*==================================================*/

   if (ArgTypeCheck("str-length",1,SYMBOL_OR_STRING,&theArg) == FALSE)
     { return(-1L); }

   /*============================================*/
   /* Return the length of the string or symbol. */
   /*============================================*/

   return( (long) strlen(DOToString(theArg)));
  }

/****************************************/
/* UpcaseFunction: H/L access routine   */
/*   for the upcase function.           */
/****************************************/
globle void UpcaseFunction(
  DATA_OBJECT_PTR returnValue)
  {
   DATA_OBJECT theArg;
   int i, slen;
   char *osptr, *nsptr;

   /*===============================================*/
   /* Function upcase expects exactly one argument. */
   /*===============================================*/

   if (ArgCountCheck("upcase",EXACTLY,1) == -1)
     {
      SetpType(returnValue,STRING);
      SetpValue(returnValue,(void *) AddSymbol(""));
      return;
     }

   /*==================================================*/
   /* The argument should be of type symbol or string. */
   /*==================================================*/

   if (ArgTypeCheck("upcase",1,SYMBOL_OR_STRING,&theArg) == FALSE)
     {
      SetpType(returnValue,STRING);
      SetpValue(returnValue,(void *) AddSymbol(""));
      return;
     }

   /*======================================================*/
   /* Allocate temporary memory and then copy the original */
   /* string or symbol to that memory, while uppercasing   */
   /* lower case alphabetic characters.                    */
   /*======================================================*/

   osptr = DOToString(theArg);
   slen = strlen(osptr) + 1;
   nsptr = (char *) gm2(slen);

   for (i = 0  ; i < slen ; i++)
     {
      if (islower(osptr[i]))
        { nsptr[i] = (char) toupper(osptr[i]); }
      else
        { nsptr[i] = osptr[i]; }
     }

   /*========================================*/
   /* Return the uppercased string and clean */
   /* up the temporary memory used.          */
   /*========================================*/

   SetpType(returnValue,GetType(theArg));
   SetpValue(returnValue,(void *) AddSymbol(nsptr));
   rm(nsptr,slen);
  }

/*****************************************/
/* LowcaseFunction: H/L access routine   */
/*   for the lowcase function.           */
/*****************************************/
globle void LowcaseFunction(
  DATA_OBJECT_PTR returnValue)
  {
   DATA_OBJECT theArg;
   int i, slen;
   char *osptr, *nsptr;

   /*================================================*/
   /* Function lowcase expects exactly one argument. */
   /*================================================*/

   if (ArgCountCheck("lowcase",EXACTLY,1) == -1)
     {
      SetpType(returnValue,STRING);
      SetpValue(returnValue,(void *) AddSymbol(""));
      return;
     }

   /*==================================================*/
   /* The argument should be of type symbol or string. */
   /*==================================================*/

   if (ArgTypeCheck("lowcase",1,SYMBOL_OR_STRING,&theArg) == FALSE)
     {
      SetpType(returnValue,STRING);
      SetpValue(returnValue,(void *) AddSymbol(""));
      return;
     }

   /*======================================================*/
   /* Allocate temporary memory and then copy the original */
   /* string or symbol to that memory, while lowercasing   */
   /* upper case alphabetic characters.                    */
   /*======================================================*/

   osptr = DOToString(theArg);
   slen = strlen(osptr) + 1;
   nsptr = (char *) gm2(slen);

   for (i = 0  ; i < slen ; i++)
     {
      if (isupper(osptr[i]))
        { nsptr[i] = (char) tolower(osptr[i]); }
      else
        { nsptr[i] = osptr[i]; }
     }

   /*========================================*/
   /* Return the lowercased string and clean */
   /* up the temporary memory used.          */
   /*========================================*/

   SetpType(returnValue,GetType(theArg));
   SetpValue(returnValue,(void *) AddSymbol(nsptr));
   rm(nsptr,slen);
  }

/********************************************/
/* StrCompareFunction: H/L access routine   */
/*   for the str-compare function.          */
/********************************************/
globle long int StrCompareFunction()
  {
   int numArgs, length;
   DATA_OBJECT arg1, arg2, arg3;
   long returnValue;

   /*=======================================================*/
   /* Function str-compare expects either 2 or 3 arguments. */
   /*=======================================================*/

   if ((numArgs = ArgRangeCheck("str-compare",2,3)) == -1) return(0L);

   /*=============================================================*/
   /* The first two arguments should be of type symbol or string. */
   /*=============================================================*/

   if (ArgTypeCheck("str-compare",1,SYMBOL_OR_STRING,&arg1) == FALSE)
     { return(0L); }

   if (ArgTypeCheck("str-compare",2,SYMBOL_OR_STRING,&arg2) == FALSE)
     { return(0L); }

   /*===================================================*/
   /* Compare the strings. Use the 3rd argument for the */
   /* maximum length of comparison, if it is provided.  */
   /*===================================================*/

   if (numArgs == 3)
     {
      if (ArgTypeCheck("str-compare",3,INTEGER,&arg3) == FALSE)
        { return(0L); }

      length = CoerceToInteger(GetType(arg3),GetValue(arg3));
      returnValue = strncmp(DOToString(arg1),DOToString(arg2),
                            (CLIPS_STD_SIZE) length);
     }
   else
     { returnValue = strcmp(DOToString(arg1),DOToString(arg2)); }

   /*========================================================*/
   /* Return Values are as follows:                          */
   /* -1 is returned if <string-1> is less than <string-2>.  */
   /*  1 is return if <string-1> is greater than <string-2>. */
   /*  0 is returned if <string-1> is equal to <string-2>.   */
   /*========================================================*/

   if (returnValue < 0) returnValue = -1;
   else if (returnValue > 0) returnValue = 1;
   return(returnValue);
  }

/*******************************************/
/* SubStringFunction: H/L access routine   */
/*   for the sub-string function.          */
/*******************************************/
globle void *SubStringFunction()
  {
   DATA_OBJECT theArgument;
   char *tempString, *returnString;
   int start, end, i, j;
   void *returnValue;

   /*===================================*/
   /* Check and retrieve the arguments. */
   /*===================================*/

   if (ArgCountCheck("sub-string",EXACTLY,3) == -1)
     { return((void *) AddSymbol("")); }

   if (ArgTypeCheck("sub-string",1,INTEGER,&theArgument) == FALSE)
     { return((void *) AddSymbol("")); }

   start = CoerceToInteger(theArgument.type,theArgument.value) - 1;

   if (ArgTypeCheck("sub-string",2,INTEGER,&theArgument) == FALSE)
     {  return((void *) AddSymbol("")); }

   end = CoerceToInteger(theArgument.type,theArgument.value) - 1;

   if (ArgTypeCheck("sub-string",3,SYMBOL_OR_STRING,&theArgument) == FALSE)
     { return((void *) AddSymbol("")); }

   /*================================================*/
   /* If parameters are out of range return an error */
   /*================================================*/

   if (start < 0) start = 0;
   if (end > (int) strlen(DOToString(theArgument)))
     { end = strlen(DOToString(theArgument)); }

   /*==================================*/
   /* If the start is greater than the */
   /* end, return a null string.       */
   /*==================================*/

   if (start > end)
     { return((void *) AddSymbol("")); }

   /*=============================================*/
   /* Otherwise, allocate the string and copy the */
   /* designated portion of the old string to the */
   /* new string.                                 */
   /*=============================================*/

   else
     {
      returnString = (char *) gm2(end - start +2);  /* (end - start) inclusive + EOS */
      tempString = DOToString(theArgument);
      for(j=0, i=start;i <= end; i++, j++)
        { *(returnString+j) = *(tempString+i); }
      *(returnString+j) = '\0';
     }

   /*========================*/
   /* Return the new string. */
   /*========================*/

   returnValue = (void *) AddSymbol(returnString);
   rm(returnString,end - start + 2);
   return(returnValue);
  }

/******************************************/
/* StrIndexFunction: H/L access routine   */
/*   for the sub-index function.          */
/******************************************/
globle void StrIndexFunction(
  DATA_OBJECT_PTR result)
  {
   DATA_OBJECT theArgument1, theArgument2;
   char *strg1, *strg2;
   int i, j;

   result->type = SYMBOL;
   result->value = FalseSymbol;

   /*===================================*/
   /* Check and retrieve the arguments. */
   /*===================================*/

   if (ArgCountCheck("str-index",EXACTLY,2) == -1) return;

   if (ArgTypeCheck("str-index",1,SYMBOL_OR_STRING,&theArgument1) == FALSE) return;

   if (ArgTypeCheck("str-index",2,SYMBOL_OR_STRING,&theArgument2) == FALSE) return;

   strg1 = DOToString(theArgument1);
   strg2 = DOToString(theArgument2);

   /*=================================*/
   /* Find the position in string2 of */
   /* string1 (counting from 1).      */
   /*=================================*/

   if (strlen(strg1) == 0)
     {
      result->type = INTEGER;
      result->value = (void *) AddLong((long) strlen(strg2) + 1L);
      return;
     }

   for (i=1; *strg2; i++, strg2++)
     {
      for (j=0; *(strg1+j) && *(strg1+j) == *(strg2+j); j++)
        { /* Do Nothing */ }

      if (*(strg1+j) == '\0')
        {
         result->type = INTEGER;
         result->value = (void *) AddLong((long) i);
         return;
        }
     }

   return;
  }

#if (! RUN_TIME) && (! BLOAD_ONLY)

/**************************************/
/* EvalFunction: H/L access routine   */
/*   for the eval function.           */
/**************************************/
globle void EvalFunction(
  DATA_OBJECT_PTR returnValue)
  {
   DATA_OBJECT theArg;

   /*=============================================*/
   /* Function eval expects exactly one argument. */
   /*=============================================*/

   if (ArgCountCheck("eval",EXACTLY,1) == -1)
     {
      SetpType(returnValue,SYMBOL);
      SetpValue(returnValue,FalseSymbol);
      return;
     }

   /*==================================================*/
   /* The argument should be of type SYMBOL or STRING. */
   /*==================================================*/

   if (ArgTypeCheck("eval",1,SYMBOL_OR_STRING,&theArg) == FALSE)
     {
      SetpType(returnValue,SYMBOL);
      SetpValue(returnValue,FalseSymbol);
      return;
     }

   /*======================*/
   /* Evaluate the string. */
   /*======================*/

   Eval(DOToString(theArg),returnValue);
  }

/****************************/
/* Eval: C access routine   */
/*   for the eval function. */
/****************************/
globle int Eval(
  char *theString,
  DATA_OBJECT_PTR returnValue)
  {
   struct expr *top;
   int ov;
   Thread static int depth = 0;
   char logicalNameBuffer[20];
   struct BindInfo *oldBinds;

   /*======================================================*/
   /* Evaluate the string. Create a different logical name */
   /* for use each time the eval function is called.       */
   /*======================================================*/

   depth++;
   sprintf(logicalNameBuffer,"Eval-%d",depth);
   if (OpenStringSource(logicalNameBuffer,theString,0) == 0)
     {
      SetpType(returnValue,SYMBOL);
      SetpValue(returnValue,FalseSymbol);
      depth--;
      return(FALSE);
     }

   /*================================================*/
   /* Save the current parsing state before routines */
   /* are called to parse the eval string.           */
   /*================================================*/

   ov = GetPPBufferStatus();
   SetPPBufferStatus(FALSE);
   oldBinds = GetParsedBindNames();
   SetParsedBindNames(NULL);

   /*========================================================*/
   /* Parse the string argument passed to the eval function. */
   /*========================================================*/

   top = ParseAtomOrExpression(logicalNameBuffer,NULL);

   /*============================*/
   /* Restore the parsing state. */
   /*============================*/

   SetPPBufferStatus(ov);
   ClearParsedBindNames();
   SetParsedBindNames(oldBinds);

   /*===========================================*/
   /* Return if an error occured while parsing. */
   /*===========================================*/

   if (top == NULL)
     {
      SetEvaluationError(TRUE);
      CloseStringSource(logicalNameBuffer);
      SetpType(returnValue,SYMBOL);
      SetpValue(returnValue,FalseSymbol);
      depth--;
      return(FALSE);
     }

   /*==============================================*/
   /* The sequence expansion operator must be used */
   /* within the argument list of a function call. */
   /*==============================================*/

   if ((top->type == MF_GBL_VARIABLE) || (top->type == MF_VARIABLE))
     {
      PrintErrorID("MISCFUN",1,FALSE);
      PrintRouter(WERROR,"expand$ must be used in the argument list of a function call.\n");
      SetEvaluationError(TRUE);
      CloseStringSource(logicalNameBuffer);
      SetpType(returnValue,SYMBOL);
      SetpValue(returnValue,FalseSymbol);
      ReturnExpression(top);
      depth--;
      return(FALSE);
     }

   /*=======================================*/
   /* The expression to be evaluated cannot */
   /* contain any local variables.          */
   /*=======================================*/

   if (ExpressionContainsVariables(top,FALSE))
     {
      PrintErrorID("STRNGFUN",2,FALSE);
      PrintRouter(WERROR,"Some variables could not be accessed by the eval function.\n");
      SetEvaluationError(TRUE);
      CloseStringSource(logicalNameBuffer);
      SetpType(returnValue,SYMBOL);
      SetpValue(returnValue,FalseSymbol);
      ReturnExpression(top);
      depth--;
      return(FALSE);
     }

   /*====================================*/
   /* Evaluate the expression and return */
   /* the memory used to parse it.       */
   /*====================================*/

   ExpressionInstall(top);
   EvaluateExpression(top,returnValue);
   ExpressionDeinstall(top);

   depth--;
   ReturnExpression(top);
   CloseStringSource(logicalNameBuffer);

   if (GetEvaluationError()) return(FALSE);
   return(TRUE);
  }

#else

/*************************************************/
/* EvalFunction: This is the non-functional stub */
/*   provided for use with a run-time version.   */
/*************************************************/
globle void EvalFunction(
  DATA_OBJECT_PTR returnValue)
  {
   PrintErrorID("STRNGFUN",1,FALSE);
   PrintRouter(WERROR,"Function eval does not work in run time modules.\n");
   SetpType(returnValue,SYMBOL);
   SetpValue(returnValue,FalseSymbol);
  }

/**************************************************/
/* Eval: This is the non-functional stub provided */
/*   for use with a run-time version.             */
/**************************************************/
globle int Eval(
  char *theString,
  DATA_OBJECT_PTR returnValue)
  {
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(theString)
#endif

   PrintErrorID("STRNGFUN",1,FALSE);
   PrintRouter(WERROR,"Function eval does not work in run time modules.\n");
   SetpType(returnValue,SYMBOL);
   SetpValue(returnValue,FalseSymbol);
   return(FALSE);
  }

#endif

#if (! RUN_TIME) && (! BLOAD_ONLY)
/***************************************/
/* BuildFunction: H/L access routine   */
/*   for the build function.           */
/***************************************/
globle int BuildFunction()
  {
   DATA_OBJECT theArg;

   /*==============================================*/
   /* Function build expects exactly one argument. */
   /*==============================================*/

   if (ArgCountCheck("build",EXACTLY,1) == -1) return(FALSE);

   /*==================================================*/
   /* The argument should be of type SYMBOL or STRING. */
   /*==================================================*/

   if (ArgTypeCheck("build",1,SYMBOL_OR_STRING,&theArg) == FALSE)
     { return(FALSE); }

   /*======================*/
   /* Build the construct. */
   /*======================*/

   return(Build(DOToString(theArg)));
  }

/*****************************/
/* Build: C access routine   */
/*   for the build function. */
/*****************************/
globle int Build(
  char *theString)
  {
   char *constructType;
   struct token theToken;
   int errorFlag;

   /*====================================================*/
   /* No additions during defrule join network activity. */
   /*====================================================*/

#if DEFRULE_CONSTRUCT
   if (JoinOperationInProgress) return(FALSE);
#endif

   /*===========================================*/
   /* Create a string source router so that the */
   /* string can be used as an input source.    */
   /*===========================================*/

   if (OpenStringSource("build",theString,0) == 0)
     { return(FALSE); }

   /*================================*/
   /* The first token of a construct */
   /* must be a left parenthesis.    */
   /*================================*/

   GetToken("build",&theToken);

   if (theToken.type != LPAREN)
     {
      CloseStringSource("build");
      return(FALSE);
     }

   /*==============================================*/
   /* The next token should be the construct type. */
   /*==============================================*/

   GetToken("build",&theToken);
   if (theToken.type != SYMBOL)
     {
      CloseStringSource("build");
      return(FALSE);
     }

   constructType = ValueToString(theToken.value);

   /*======================*/
   /* Parse the construct. */
   /*======================*/

   errorFlag = ParseConstruct(constructType,"build");

   /*=================================*/
   /* Close the string source router. */
   /*=================================*/

   CloseStringSource("build");

   /*=========================================*/
   /* If an error occured while parsing the   */
   /* construct, then print an error message. */
   /*=========================================*/

   if (errorFlag == 1)
     {
      PrintRouter(WERROR,"\nERROR:\n");
      PrintInChunks(WERROR,GetPPBuffer());
      PrintRouter(WERROR,"\n");
     }

   DestroyPPBuffer();

   /*===============================================*/
   /* Return TRUE if the construct was successfully */
   /* parsed, otherwise return FALSE.               */
   /*===============================================*/

   if (errorFlag == 0) return(TRUE);

   return(FALSE);
  }
#else
/**************************************************/
/* BuildFunction: This is the non-functional stub */
/*   provided for use with a run-time version.    */
/**************************************************/
globle int BuildFunction()
  {
   PrintErrorID("STRNGFUN",1,FALSE);
   PrintRouter(WERROR,"Function build does not work in run time modules.\n");
   return(FALSE);
  }

/***************************************************/
/* Build: This is the non-functional stub provided */
/*   for use with a run-time version.              */
/***************************************************/
globle int Build(
  char *theString)
  {
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(theString)
#endif

   PrintErrorID("STRNGFUN",1,FALSE);
   PrintRouter(WERROR,"Function build does not work in run time modules.\n");
   return(FALSE);
  }
#endif /* (! RUN_TIME) && (! BLOAD_ONLY) */

#endif /* STRING_FUNCTIONS */

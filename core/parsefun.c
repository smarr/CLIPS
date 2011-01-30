   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  03/24/96            */
   /*                                                     */
   /*               PARSING FUNCTIONS MODULE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for several parsing related    */
/*   functions including...                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description              */
/* ------------------+-------------+------------------------  */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS        */
/*************************************************************/

#define _PARSEFUN_SOURCE_

#include "setup.h"

/*
#include <stdio.h>
#define _STDIO_INCLUDED_
#include <ctype.h>
*/

#include <string.h>

#include "argacces.h"
#include "cstrcpsr.h"
#include "exprnpsr.h"
#include "extnfunc.h"
#include "memalloc.h"
#include "multifld.h"
#include "prcdrpsr.h"
#include "router.h"
#include "strngrtr.h"
#include "utility.h"

#include "parsefun.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static int                     FindErrorCapture(char *);
   static int                     PrintErrorCapture(char *,char *);
   static void                    DeactivateErrorCapture(void);
   static void                    SetErrorCaptureValues(DATA_OBJECT_PTR);

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static char               *ErrorString;
   Thread static int                 ErrorCurrentPosition = 0;
   Thread static int                 ErrorMaximumPosition = 0;
   Thread static char               *WarningString;
   Thread static int                 WarningCurrentPosition = 0;
   Thread static int                 WarningMaximumPosition = 0;

#if ! RUN_TIME
/*****************************************/
/* ParseFunctionDefinitions: Initializes */
/*   the parsing related functions.      */
/*****************************************/
globle void ParseFunctionDefinitions()
  {
   DefineFunction2("check-syntax",'u',PTIF CheckSyntaxFunction,"CheckSyntaxFunction","11s");
  }
#endif

#if (! RUN_TIME) && (! BLOAD_ONLY)
/*******************************************/
/* CheckSyntaxFunction: H/L access routine */
/*   for the check-syntax function.        */
/*******************************************/
globle void CheckSyntaxFunction(
  DATA_OBJECT *returnValue)
  {
   DATA_OBJECT theArg;

   /*===============================*/
   /* Set up a default return value */
   /* (TRUE for problems found).    */
   /*===============================*/

   SetpType(returnValue,SYMBOL);
   SetpValue(returnValue,TrueSymbol);

   /*=====================================================*/
   /* Function check-syntax expects exactly one argument. */
   /*=====================================================*/

   if (ArgCountCheck("check-syntax",EXACTLY,1) == -1) return;

   /*========================================*/
   /* The argument should be of type STRING. */
   /*========================================*/

   if (ArgTypeCheck("check-syntax",1,STRING,&theArg) == FALSE)
     { return; }

   /*===================*/
   /* Check the syntax. */
   /*===================*/

   CheckSyntax(DOToString(theArg),returnValue);
  }

/*********************************/
/* CheckSyntax: C access routine */
/*   for the build function.     */
/*********************************/
globle int CheckSyntax(
  char *theString,
  DATA_OBJECT_PTR returnValue)
  {
   char *name;
   struct token theToken;
   struct expr *top;
   short rv;

   /*==============================*/
   /* Set the default return value */
   /* (TRUE for problems found).   */
   /*==============================*/

   SetpType(returnValue,SYMBOL);
   SetpValue(returnValue,TrueSymbol);

   /*===========================================*/
   /* Create a string source router so that the */
   /* string can be used as an input source.    */
   /*===========================================*/

   if (OpenStringSource("check-syntax",theString,0) == 0)
     { return(TRUE); }

   /*=================================*/
   /* Only expressions and constructs */
   /* can have their syntax checked.  */
   /*=================================*/

   GetToken("check-syntax",&theToken);

   if (theToken.type != LPAREN)
     {
      CloseStringSource("check-syntax");
      SetpValue(returnValue,AddSymbol("MISSING-LEFT-PARENTHESIS"));
      return(TRUE);
     }

   /*========================================*/
   /* The next token should be the construct */
   /* type or function name.                 */
   /*========================================*/

   GetToken("check-syntax",&theToken);
   if (theToken.type != SYMBOL)
     {
      CloseStringSource("check-syntax");
      SetpValue(returnValue,AddSymbol("EXPECTED-SYMBOL-AFTER-LEFT-PARENTHESIS"));
      return(TRUE);
     }

   name = ValueToString(theToken.value);

   /*==============================================*/
   /* Set up a router to capture the error output. */
   /*==============================================*/

   AddRouter("error-capture",40,
              FindErrorCapture, PrintErrorCapture,
              NULL, NULL, NULL);

   /*================================*/
   /* Determine if it's a construct. */
   /*================================*/

   if (FindConstruct(name))
     {
      CheckSyntaxMode = TRUE;
      rv = (short) ParseConstruct(name,"check-syntax");
      GetToken("check-syntax",&theToken);
      CheckSyntaxMode = FALSE;

      if (rv)
        {
         PrintRouter(WERROR,"\nERROR:\n");
         PrintInChunks(WERROR,GetPPBuffer());
         PrintRouter(WERROR,"\n");
        }

      DestroyPPBuffer();

      CloseStringSource("check-syntax");

      if ((rv != FALSE) || (WarningString != NULL))
        {
         SetErrorCaptureValues(returnValue);
         DeactivateErrorCapture();
         return(TRUE);
        }

      if (theToken.type != STOP)
        {
         SetpValue(returnValue,AddSymbol("EXTRANEOUS-INPUT-AFTER-LAST-PARENTHESIS"));
         DeactivateErrorCapture();
         return(TRUE);
        }

      SetpType(returnValue,SYMBOL);
      SetpValue(returnValue,FalseSymbol);
      DeactivateErrorCapture();
      return(FALSE);
     }

   /*=======================*/
   /* Parse the expression. */
   /*=======================*/

   top = Function2Parse("check-syntax",name);
   GetToken("check-syntax",&theToken);
   ClearParsedBindNames();
   CloseStringSource("check-syntax");

   if (top == NULL)
     {
      SetErrorCaptureValues(returnValue);
      DeactivateErrorCapture();
      return(TRUE);
     }

   if (theToken.type != STOP)
     {
      SetpValue(returnValue,AddSymbol("EXTRANEOUS-INPUT-AFTER-LAST-PARENTHESIS"));
      DeactivateErrorCapture();
      ReturnExpression(top);
      return(TRUE);
     }

   DeactivateErrorCapture();

   ReturnExpression(top);
   SetpType(returnValue,SYMBOL);
   SetpValue(returnValue,FalseSymbol);
   return(FALSE);
  }

/**************************************************/
/* DeactivateErrorCapture: Deactivates the error  */
/*   capture router and the strings used to store */
/*   the captured information.                    */
/**************************************************/
static void DeactivateErrorCapture()
  {
   if (ErrorString != NULL)
     {
      rm(ErrorString,ErrorMaximumPosition);
      ErrorString = NULL;
     }

   if (WarningString != NULL)
     {
      rm(WarningString,WarningMaximumPosition);
      WarningString = NULL;
     }

   ErrorCurrentPosition = 0;
   ErrorMaximumPosition = 0;
   WarningCurrentPosition = 0;
   WarningMaximumPosition = 0;

   DeleteRouter("error-capture");
  }

/******************************************************************/
/* SetErrorCaptureValues: Stores the error/warnings captured when */
/*   parsing an expression or construct into a multifield value.  */
/*   The first field contains the output sent to the WERROR       */
/*   logical name and the second field contains the output sent   */
/*   to the WWARNING logical name. FALSE is stored in either      */
/*   position if no output was sent to those logical names.       */
/******************************************************************/
static void SetErrorCaptureValues(
  DATA_OBJECT_PTR returnValue)
  {
   struct multifield *theMultifield;

   theMultifield = (struct multifield *) CreateMultifield(2L);

   if (ErrorString != NULL)
     {
      SetMFType(theMultifield,1,STRING);
      SetMFValue(theMultifield,1,AddSymbol(ErrorString));
     }
   else
     {
      SetMFType(theMultifield,1,SYMBOL);
      SetMFValue(theMultifield,1,FalseSymbol);
     }

   if (WarningString != NULL)
     {
      SetMFType(theMultifield,2,STRING);
      SetMFValue(theMultifield,2,AddSymbol(WarningString));
     }
   else
     {
      SetMFType(theMultifield,2,SYMBOL);
      SetMFValue(theMultifield,2,FalseSymbol);
     }

   SetpType(returnValue,MULTIFIELD);
   SetpDOBegin(returnValue,1);
   SetpDOEnd(returnValue,2);
   SetpValue(returnValue,(void *) theMultifield);
  }

/**********************************/
/* FindErrorCapture: Find routine */
/*   for the check-syntax router. */
/**********************************/
static int FindErrorCapture(
  char *logicalName)
  {
   if ((strcmp(logicalName,WERROR) == 0) ||
       (strcmp(logicalName,WWARNING) == 0))
     { return(TRUE); }

   return(FALSE);
  }

/************************************/
/* PrintErrorCapture: Print routine */
/*   for the check-syntax router.   */
/************************************/
static int PrintErrorCapture(
  char *logicalName,
  char *str)
  {
   if (strcmp(logicalName,WERROR) == 0)
     {
      ErrorString = AppendToString(str,ErrorString,
                                   &ErrorCurrentPosition,
                                   &ErrorMaximumPosition);
     }
   else if (strcmp(logicalName,WWARNING) == 0)
     {
      WarningString = AppendToString(str,WarningString,
                                     &WarningCurrentPosition,
                                     &WarningMaximumPosition);
     }

   return(1);
  }

#else
/****************************************************/
/* CheckSyntaxFunction: This is the non-functional  */
/*   stub provided for use with a run-time version. */
/****************************************************/
globle void CheckSyntaxFunction(
  DATA_OBJECT *returnValue)
  {
   PrintErrorID("PARSEFUN",1,FALSE);
   PrintRouter(WERROR,"Function check-syntax does not work in run time modules.\n");
   SetpType(returnValue,SYMBOL);
   SetpValue(returnValue,TrueSymbol);
  }

/************************************************/
/* CheckSyntax: This is the non-functional stub */
/*   provided for use with a run-time version.  */
/************************************************/
globle int CheckSyntax(
  char *theString,
  DATA_OBJECT_PTR returnValue)
  {
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(theString)
#pragma unused(returnValue)
#endif

   PrintErrorID("PARSEFUN",1,FALSE);
   PrintRouter(WERROR,"Function check-syntax does not work in run time modules.\n");
   SetpType(returnValue,SYMBOL);
   SetpValue(returnValue,TrueSymbol);
   return(TRUE);
  }

#endif /* (! RUN_TIME) && (! BLOAD_ONLY) */



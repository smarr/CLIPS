   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.21  06/15/03            */
   /*                                                     */
   /*            FACT RHS PATTERN PARSER MODULE           */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides a number of routines for parsing fact   */
/*   patterns typically found on the RHS of a rule (such as  */
/*   the assert command). Also contains some functions for   */
/*   parsing RHS slot values (used by functions such as      */
/*   assert, modify, and duplicate).                         */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Chris Culbert                                        */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _FACTRHS_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT

#include "constant.h"
#include "envrnmnt.h"
#include "extnfunc.h"
#include "modulutl.h"
#include "modulpsr.h"
#include "pattern.h"
#include "prntutil.h"
#include "cstrcpsr.h"

#if BLOAD_AND_BSAVE || BLOAD || BLOAD_ONLY
#include "bload.h"
#endif

#include "tmpltpsr.h"
#include "tmpltrhs.h"
#include "tmpltutl.h"
#include "exprnpsr.h"
#include "strngrtr.h"
#include "router.h"

#include "factrhs.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if RUN_TIME || BLOAD_ONLY || BLOAD || BLOAD_AND_BSAVE
   static void                       NoSuchTemplateError(void *,EXEC_STATUS,char *);
#endif

#if (! RUN_TIME)

/**********************************************************************/
/* BuildRHSAssert: Parses zero or more RHS fact patterns (the format  */
/*   which is used by the assert command and the deffacts construct). */
/*   Each of the RHS patterns is attached to an assert command and if */
/*   there is more than one assert command, then a progn command is   */
/*   wrapped around all of the assert commands.                       */
/**********************************************************************/
globle struct expr *BuildRHSAssert(
  void *theEnv,
  EXEC_STATUS,
  char *logicalName,
  struct token *theToken,
  int *error,
  int atLeastOne,
  int readFirstParen,
  char *whereParsed,
  const char* assertLikeCommand)
  {
   struct expr *lastOne, *nextOne, *assertList, *stub;

   *error = FALSE;

   /*===============================================================*/
   /* If the first parenthesis of the RHS fact pattern has not been */
   /* read yet, then get the next token. If a right parenthesis is  */
   /* encountered then exit (however, set the error return value if */
   /* at least one fact was expected).                              */
   /*===============================================================*/

   if (readFirstParen == FALSE)
     {
      if (theToken->type == RPAREN)
        {
         if (atLeastOne)
           {
            *error = TRUE;
            SyntaxErrorMessage(theEnv,execStatus,whereParsed);
           }
         return(NULL);
        }
     }

   /*================================================*/
   /* Parse the facts until no more are encountered. */
   /*================================================*/

   lastOne = assertList = NULL;
   while ((nextOne = GetRHSPattern(theEnv,execStatus,logicalName,theToken,
                                   error,FALSE,readFirstParen,
                                   TRUE,RPAREN)) != NULL)
     {
      PPCRAndIndent(theEnv,execStatus);

      stub = GenConstant(theEnv,execStatus,FCALL,(void *) FindFunction(theEnv,execStatus, assertLikeCommand));
      stub->argList = nextOne;
      nextOne = stub;

      if (lastOne == NULL)
        { assertList = nextOne; }
      else
        { lastOne->nextArg = nextOne; }
      lastOne = nextOne;

      readFirstParen = TRUE;
     }

   /*======================================================*/
   /* If an error was detected while parsing, then return. */
   /*======================================================*/

   if (*error)
     {
      ReturnExpression(theEnv,execStatus,assertList);
      return(NULL);
     }

   /*======================================*/
   /* Fix the pretty print representation. */
   /*======================================*/

   if (theToken->type == RPAREN)
     {
      PPBackup(theEnv,execStatus);
      PPBackup(theEnv,execStatus);
      SavePPBuffer(theEnv,execStatus,")");
     }

   /*==============================================================*/
   /* If no facts are being asserted then return NULL. In addition */
   /* if at least one fact was required, then signal an error.     */
   /*==============================================================*/

   if (assertList == NULL)
     {
      if (atLeastOne)
        {
         *error = TRUE;
         SyntaxErrorMessage(theEnv,execStatus,whereParsed);
        }

      return(NULL);
     }

   /*===============================================*/
   /* If more than one fact is being asserted, then */
   /* wrap the assert commands within a progn call. */
   /*===============================================*/

   if (assertList->nextArg != NULL)
     {
      stub = GenConstant(theEnv,execStatus,FCALL,(void *) FindFunction(theEnv,execStatus,"progn"));
      stub->argList = assertList;
      assertList = stub;
     }

   /*==========================================================*/
   /* Return the expression for asserting the specified facts. */
   /*==========================================================*/

   return(assertList);
  }

#endif

/***************************************************************/
/* GetRHSPattern: Parses a single RHS fact pattern. The return */
/*   value is the fact just parsed (or NULL if the delimiter   */
/*   for no more facts is the first token parsed). If an error */
/*   occurs, then the error flag passed as an argument is set. */
/***************************************************************/
globle struct expr *GetRHSPattern(
  void *theEnv,
  EXEC_STATUS,
  char *readSource,
  struct token *tempToken,
  int *error,
  int constantsOnly,
  int readFirstParen,
  int checkFirstParen,
  int endType)
  {
   struct expr *lastOne = NULL;
   struct expr *nextOne, *firstOne, *argHead = NULL;
   int printError, count;
   struct deftemplate *theDeftemplate;
   struct symbolHashNode *templateName;
   char *nullBitMap = "\0";

   /*=================================================*/
   /* Get the opening parenthesis of the RHS pattern. */
   /*=================================================*/

   *error = FALSE;

   if (readFirstParen) GetToken(theEnv,execStatus,readSource,tempToken);

   if (checkFirstParen)
     {
      if (tempToken->type == endType) return(NULL);

      if (tempToken->type != LPAREN)
        {
         SyntaxErrorMessage(theEnv,execStatus,"RHS patterns");
         *error = TRUE;
         return(NULL);
        }
     }

   /*======================================================*/
   /* The first field of an asserted fact must be a symbol */
   /* (but not = or : which have special significance).    */
   /*======================================================*/

   GetToken(theEnv,execStatus,readSource,tempToken);
   if (tempToken->type != SYMBOL)
     {
      SyntaxErrorMessage(theEnv,execStatus,"first field of a RHS pattern");
      *error = TRUE;
      return(NULL);
     }
   else if ((strcmp(ValueToString(tempToken->value),"=") == 0) ||
            (strcmp(ValueToString(tempToken->value),":") == 0))
     {
      SyntaxErrorMessage(theEnv,execStatus,"first field of a RHS pattern");
      *error = TRUE;
      return(NULL);
     }

   /*=========================================================*/
   /* Check to see if the relation name is a reserved symbol. */
   /*=========================================================*/

   templateName = (struct symbolHashNode *) tempToken->value;

   if (ReservedPatternSymbol(theEnv,execStatus,ValueToString(templateName),NULL))
     {
      ReservedPatternSymbolErrorMsg(theEnv,execStatus,ValueToString(templateName),"a relation name");
      *error = TRUE;
      return(NULL);
     }

   /*============================================================*/
   /* A module separator in the name is illegal in this context. */
   /*============================================================*/

   if (FindModuleSeparator(ValueToString(templateName)))
     {
      IllegalModuleSpecifierMessage(theEnv,execStatus);

      *error = TRUE;
      return(NULL);
     }

   /*=============================================================*/
   /* Determine if there is an associated deftemplate. If so, let */
   /* the deftemplate parsing functions parse the RHS pattern and */
   /* then return the fact pattern that was parsed.               */
   /*=============================================================*/

   theDeftemplate = (struct deftemplate *)
                    FindImportedConstruct(theEnv,execStatus,"deftemplate",NULL,ValueToString(templateName),
                                          &count,TRUE,NULL);

   if (count > 1)
     {
      AmbiguousReferenceErrorMessage(theEnv,execStatus,"deftemplate",ValueToString(templateName));
      *error = TRUE;
      return(NULL);
     }

   /*======================================================*/
   /* If no deftemplate exists with the specified relation */
   /* name, then create an implied deftemplate.            */
   /*======================================================*/

   if (theDeftemplate == NULL)
#if (! BLOAD_ONLY) && (! RUN_TIME)
     {
#if BLOAD || BLOAD_AND_BSAVE
      if ((Bloaded(theEnv,execStatus)) && (! ConstructData(theEnv,execStatus)->CheckSyntaxMode))
        {
         NoSuchTemplateError(theEnv,execStatus,ValueToString(templateName));
         *error = TRUE;
         return(NULL);
        }
#endif
#if DEFMODULE_CONSTRUCT
      if (FindImportExportConflict(theEnv,execStatus,"deftemplate",((struct defmodule *) EnvGetCurrentModule(theEnv,execStatus)),ValueToString(templateName)))
        {
         ImportExportConflictMessage(theEnv,execStatus,"implied deftemplate",ValueToString(templateName),NULL,NULL);
         *error = TRUE;
         return(NULL);
        }
#endif
      if (! ConstructData(theEnv,execStatus)->CheckSyntaxMode)
        { theDeftemplate = CreateImpliedDeftemplate(theEnv,execStatus,(SYMBOL_HN *) templateName,TRUE); }
     }
#else
    {
     NoSuchTemplateError(theEnv,execStatus,ValueToString(templateName));
     *error = TRUE;
     return(NULL);
    }
#endif

   /*=========================================*/
   /* If an explicit deftemplate exists, then */
   /* parse the fact as a deftemplate fact.   */
   /*=========================================*/

   if ((theDeftemplate != NULL) && (theDeftemplate->implied == FALSE))
     {
      firstOne = GenConstant(theEnv,execStatus,DEFTEMPLATE_PTR,theDeftemplate);
      firstOne->nextArg = ParseAssertTemplate(theEnv,execStatus,readSource,tempToken,
                                              error,endType,
                                              constantsOnly,theDeftemplate);
      if (*error)
        {
         ReturnExpression(theEnv,execStatus,firstOne);
         firstOne = NULL;
        }

      return(firstOne);
     }

   /*========================================*/
   /* Parse the fact as an ordered RHS fact. */
   /*========================================*/

   firstOne = GenConstant(theEnv,execStatus,DEFTEMPLATE_PTR,theDeftemplate);

#if (! RUN_TIME) && (! BLOAD_ONLY)
   SavePPBuffer(theEnv,execStatus," ");
#endif

   while ((nextOne = GetAssertArgument(theEnv,execStatus,readSource,tempToken,
                                        error,endType,constantsOnly,&printError)) != NULL)
     {
      if (argHead == NULL) argHead = nextOne;
      else lastOne->nextArg = nextOne;
      lastOne = nextOne;
#if (! RUN_TIME) && (! BLOAD_ONLY)
      SavePPBuffer(theEnv,execStatus," ");
#endif
     }

   /*===========================================================*/
   /* If an error occurred, set the error flag and return NULL. */
   /*===========================================================*/

   if (*error)
     {
      if (printError) SyntaxErrorMessage(theEnv,execStatus,"RHS patterns");
      ReturnExpression(theEnv,execStatus,firstOne);
      ReturnExpression(theEnv,execStatus,argHead);
      return(NULL);
     }

   /*=====================================*/
   /* Fix the pretty print representation */
   /* of the RHS ordered fact.            */
   /*=====================================*/

#if (! RUN_TIME) && (! BLOAD_ONLY)
   PPBackup(theEnv,execStatus);
   PPBackup(theEnv,execStatus);
   SavePPBuffer(theEnv,execStatus,tempToken->printForm);
#endif

   /*==========================================================*/
   /* Ordered fact assertions are processed by stuffing all of */
   /* the fact's proposition (except the relation name) into a */
   /* single multifield slot.                                  */
   /*==========================================================*/

   firstOne->nextArg = GenConstant(theEnv,execStatus,FACT_STORE_MULTIFIELD,EnvAddBitMap(theEnv,execStatus,(void *) nullBitMap,1));
   firstOne->nextArg->argList = argHead;

   /*==============================*/
   /* Return the RHS ordered fact. */
   /*==============================*/

   return(firstOne);
  }

/********************************************************************/
/* GetAssertArgument: Parses a single RHS slot value and returns an */
/*   expression representing the value. When parsing a deftemplate  */
/*   slot, the slot name has already been parsed when this function */
/*   is called. NULL is returned if a slot or fact delimiter is     */
/*   encountered. In the event of a parse error, the error flag     */
/*   passed as an argument is set.                                  */
/********************************************************************/
globle struct expr *GetAssertArgument(
  void *theEnv,
  EXEC_STATUS,
  char *logicalName,
  struct token *theToken,
  int *error,
  int endType,
  int constantsOnly,
  int *printError)
  {
#if ! RUN_TIME
   struct expr *nextField;
#else
   struct expr *nextField = NULL;
#endif

   /*=================================================*/
   /* Read in the first token of the slot's value. If */
   /* the end delimiter is encountered, then return.  */
   /*=================================================*/

   *printError = TRUE;
   GetToken(theEnv,execStatus,logicalName,theToken);
   if (theToken->type == endType) return(NULL);

   /*=============================================================*/
   /* If an equal sign of left parenthesis was parsed, then parse */
   /* a function which is to be evaluated to determine the slot's */
   /* value. The equal sign corresponds to the return value       */
   /* constraint which can be used in LHS fact patterns. The      */
   /* equal sign is no longer necessary on either the LHS or RHS  */
   /* of a rule to indicate that a function is being evaluated to */
   /* determine its value either for assignment or pattern        */
   /* matching.                                                   */
   /*=============================================================*/

   if ((theToken->type == SYMBOL) ?
       (strcmp(ValueToString(theToken->value),"=") == 0) :
       (theToken->type == LPAREN))
     {
      if (constantsOnly)
        {
         *error = TRUE;
         return(NULL);
        }

#if ! RUN_TIME
      if (theToken->type == LPAREN) nextField = Function1Parse(theEnv,execStatus,logicalName);
      else nextField = Function0Parse(theEnv,execStatus,logicalName);
      if (nextField == NULL)
#endif
        {
         *printError = FALSE;
         *error = TRUE;
        }
#if ! RUN_TIME
      else
        {
         theToken->type= RPAREN;
         theToken->value = (void *) EnvAddSymbol(theEnv,execStatus,")");
         theToken->printForm = ")";
        }
#endif

      return(nextField);
     }

   /*==================================================*/
   /* Constants are always allowed as RHS slot values. */
   /*==================================================*/

   if ((theToken->type == SYMBOL) || (theToken->type == STRING) ||
#if OBJECT_SYSTEM
           (theToken->type == INSTANCE_NAME) ||
#endif
           (theToken->type == FLOAT) || (theToken->type == INTEGER))
     { return(GenConstant(theEnv,execStatus,theToken->type,theToken->value)); }

   /*========================================*/
   /* Variables are also allowed as RHS slot */
   /* values under some circumstances.       */
   /*========================================*/

   if ((theToken->type == SF_VARIABLE) ||
#if DEFGLOBAL_CONSTRUCT
            (theToken->type == GBL_VARIABLE) ||
            (theToken->type == MF_GBL_VARIABLE) ||
#endif
            (theToken->type == MF_VARIABLE))
     {
      if (constantsOnly)
        {
         *error = TRUE;
         return(NULL);
        }

      return(GenConstant(theEnv,execStatus,theToken->type,theToken->value));
     }

   /*==========================================================*/
   /* If none of the other cases have been satisfied, then the */
   /* token parsed is not appropriate for a RHS slot value.    */
   /*==========================================================*/

   *error = TRUE;
   return(NULL);
  }

/****************************************************/
/* StringToFact: Converts the string representation */
/*   of a fact to a fact data structure.            */
/****************************************************/
globle struct fact *StringToFact(
  void *theEnv,
  EXEC_STATUS,
  char *str)
  {
   struct token theToken;
   struct fact *factPtr;
   unsigned numberOfFields = 0, whichField;
   struct expr *assertArgs, *tempPtr;
   int error = FALSE;
   DATA_OBJECT theResult;

   /*=========================================*/
   /* Open a string router and parse the fact */
   /* using the router as an input source.    */
   /*=========================================*/
   
   SetEvaluationError(theEnv,execStatus,FALSE);

   OpenStringSource(theEnv,execStatus,"assert_str",str,0);

   assertArgs = GetRHSPattern(theEnv,execStatus,"assert_str",&theToken,
                              &error,FALSE,TRUE,
                              TRUE,RPAREN);

   CloseStringSource(theEnv,execStatus,"assert_str");

   /*===========================================*/
   /* Check for errors or the use of variables. */
   /*===========================================*/
   
   if ((assertArgs == NULL) && (! error))
     {
      SyntaxErrorMessage(theEnv,execStatus,"RHS patterns");
      ReturnExpression(theEnv,execStatus,assertArgs);
      return(NULL);
     }

   if (error)
     {
      ReturnExpression(theEnv,execStatus,assertArgs);
      return(NULL);
     }

   if (ExpressionContainsVariables(assertArgs,FALSE))
     {
      LocalVariableErrorMessage(theEnv,execStatus,"the assert-string function");
      SetEvaluationError(theEnv,execStatus,TRUE);
      ReturnExpression(theEnv,execStatus,assertArgs);
      return(NULL);
     }

   /*=======================================================*/
   /* Count the number of fields needed for the fact and    */
   /* create a fact data structure of the appropriate size. */
   /*=======================================================*/

   for (tempPtr = assertArgs->nextArg; tempPtr != NULL; tempPtr = tempPtr->nextArg)
     { numberOfFields++; }

   factPtr = (struct fact *) CreateFactBySize(theEnv,execStatus,numberOfFields);
   factPtr->whichDeftemplate = (struct deftemplate *) assertArgs->value;

   /*=============================================*/
   /* Copy the fields to the fact data structure. */
   /*=============================================*/

   ExpressionInstall(theEnv,execStatus,assertArgs); /* DR0836 */
   whichField = 0;
   for (tempPtr = assertArgs->nextArg; tempPtr != NULL; tempPtr = tempPtr->nextArg)
     {
      EvaluateExpression(theEnv,execStatus,tempPtr,&theResult);
      factPtr->theProposition.theFields[whichField].type = theResult.type;
      factPtr->theProposition.theFields[whichField].value = theResult.value;
      whichField++;
     }
   ExpressionDeinstall(theEnv,execStatus,assertArgs); /* DR0836 */
   ReturnExpression(theEnv,execStatus,assertArgs);

   /*==================*/
   /* Return the fact. */
   /*==================*/

   return(factPtr);
  }

#if RUN_TIME || BLOAD_ONLY || BLOAD || BLOAD_AND_BSAVE

/*********************************************************/
/* NoSuchTemplateError: Prints out an error message      */
/* in a BLOAD_ONLY, RUN_TIME or bload active environment */
/* when an implied deftemplate cannot be created for     */
/* an assert                                             */
/*********************************************************/
static void NoSuchTemplateError(
  void *theEnv,
  EXEC_STATUS,
  char *templateName)
  {
   PrintErrorID(theEnv,execStatus,"FACTRHS",1,FALSE);
   EnvPrintRouter(theEnv,execStatus,WERROR,"Template ");
   EnvPrintRouter(theEnv,execStatus,WERROR,templateName);
   EnvPrintRouter(theEnv,execStatus,WERROR," does not exist for assert.\n");
  }

#endif /* RUN_TIME || BLOAD_ONLY || BLOAD || BLOAD_AND_BSAVE */

#endif /* DEFTEMPLATE_CONSTRUCT */



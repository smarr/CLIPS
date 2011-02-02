   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*              CONSTRUCT PARSER MODULE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Parsing routines and utilities for parsing       */
/*   constructs.                                             */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Added environment parameter to GenClose.       */
/*            Added environment parameter to GenOpen.        */
/*                                                           */
/*            Made the construct redefinition message more   */
/*            prominent.                                     */
/*                                                           */
/*            Added pragmas to remove compilation warnings.  */
/*                                                           */
/*************************************************************/

#define _CSTRCPSR_SOURCE_

#include "setup.h"

#if (! RUN_TIME) && (! BLOAD_ONLY)

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <stdlib.h>

#include "envrnmnt.h"
#include "router.h"
#include "watch.h"
#include "constrct.h"
#include "prcdrpsr.h"
#include "exprnpsr.h"
#include "modulutl.h"
#include "modulpsr.h"
#include "sysdep.h"
#include "utility.h"

#include "cstrcpsr.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static int                     FindConstructBeginning(void *,EXEC_STATUS,char *,struct token *,int,int *);

/************************************************/
/* Load: C access routine for the load command. */
/************************************************/
#if ALLOW_ENVIRONMENT_GLOBALS
globle int Load(
  char *fileName,EXEC_STATUS)
  {
   return EnvLoad(GetCurrentEnvironment(),execStatus,fileName);
  }
#endif
  
/************************************************************/
/* EnvLoad: C access routine for the load command. Returns  */
/*   0 if the file couldn't be opened, -1 if the file was   */
/*   opened but an error occurred while loading constructs, */
/*   and 1 if the file was opened and no errors occured     */
/*   while loading.                                         */
/************************************************************/
globle int EnvLoad(
  void *theEnv,
  EXEC_STATUS,
  char *fileName)
  {
   FILE *theFile;
   int noErrorsDetected;

   /*=======================================*/
   /* Open the file specified by file name. */
   /*=======================================*/

   if ((theFile = GenOpen(theEnv,execStatus,fileName,"r")) == NULL) return(0);

   /*===================================================*/
   /* Read in the constructs. Enabling fast load allows */
   /* the router system to be bypassed for quicker load */
   /* times.                                            */
   /*===================================================*/

   SetFastLoad(theEnv,execStatus,theFile);
   noErrorsDetected = LoadConstructsFromLogicalName(theEnv,execStatus,(char *) theFile);
   SetFastLoad(theEnv,execStatus,NULL);

   /*=================*/
   /* Close the file. */
   /*=================*/

   GenClose(theEnv,execStatus,theFile);

   /*========================================*/
   /* If no errors occurred during the load, */
   /* return 1, otherwise return -1.         */
   /*========================================*/

   if (noErrorsDetected) return(1);

   return(-1);
  }

/*****************************************************************/
/* LoadConstructsFromLogicalName: Loads a set of constructs into */
/*   the current environment from a specified logical name.      */
/*****************************************************************/
globle int LoadConstructsFromLogicalName(
  void *theEnv,
  EXEC_STATUS,
  char *readSource)
  {
   int constructFlag;
   struct token theToken;
   int noErrors = TRUE;
   int foundConstruct;

   /*=========================================*/
   /* Reset the halt execution and evaluation */
   /* error flags in preparation for parsing. */
   /*=========================================*/

   if (execStatus->CurrentEvaluationDepth == 0) SetHaltExecution(theEnv,execStatus,FALSE);
   SetEvaluationError(theEnv,execStatus,FALSE);

   /*========================================================*/
   /* Find the beginning of the first construct in the file. */
   /*========================================================*/

   execStatus->CurrentEvaluationDepth++;
   GetToken(theEnv,execStatus,readSource,&theToken);
   foundConstruct = FindConstructBeginning(theEnv,execStatus,readSource,&theToken,FALSE,&noErrors);

   /*==================================================*/
   /* Parse the file until the end of file is reached. */
   /*==================================================*/

   while ((foundConstruct == TRUE) && (GetHaltExecution(theEnv,execStatus) == FALSE))
     {
      /*===========================================================*/
      /* Clear the pretty print buffer in preparation for parsing. */
      /*===========================================================*/

      FlushPPBuffer(theEnv,execStatus);

      /*======================*/
      /* Parse the construct. */
      /*======================*/

      constructFlag = ParseConstruct(theEnv,execStatus,ValueToString(theToken.value),readSource);

      /*==============================================================*/
      /* If an error occurred while parsing, then find the beginning  */
      /* of the next construct (but don't generate any more error     */
      /* messages--in effect, skip everything until another construct */
      /* is found).                                                   */
      /*==============================================================*/

      if (constructFlag == 1)
        {
         EnvPrintRouter(theEnv,execStatus,WERROR,"\nERROR:\n");
         PrintInChunks(theEnv,execStatus,WERROR,GetPPBuffer(theEnv,execStatus));
         EnvPrintRouter(theEnv,execStatus,WERROR,"\n");
         noErrors = FALSE;
         GetToken(theEnv,execStatus,readSource,&theToken);
         foundConstruct = FindConstructBeginning(theEnv,execStatus,readSource,&theToken,TRUE,&noErrors);
        }

      /*======================================================*/
      /* Otherwise, find the beginning of the next construct. */
      /*======================================================*/

      else
        {
         GetToken(theEnv,execStatus,readSource,&theToken);
         foundConstruct = FindConstructBeginning(theEnv,execStatus,readSource,&theToken,FALSE,&noErrors);
        }

      /*=====================================================*/
      /* Yield time if necessary to foreground applications. */
      /*=====================================================*/

       if (foundConstruct)
         { IncrementSymbolCount(theToken.value); }
       execStatus->CurrentEvaluationDepth--;
       PeriodicCleanup(theEnv,execStatus,FALSE,TRUE);
       YieldTime(theEnv,execStatus);
       execStatus->CurrentEvaluationDepth++;
       if (foundConstruct)
         { DecrementSymbolCount(theEnv,execStatus,(SYMBOL_HN *) theToken.value); }
     }

   execStatus->CurrentEvaluationDepth--;

   /*========================================================*/
   /* Print a carriage return if a single character is being */
   /* printed to indicate constructs are being processed.    */
   /*========================================================*/

#if DEBUGGING_FUNCTIONS
   if ((EnvGetWatchItem(theEnv,execStatus,"compilations") != TRUE) && GetPrintWhileLoading(theEnv,execStatus))
#else
   if (GetPrintWhileLoading(theEnv,execStatus))
#endif
     { EnvPrintRouter(theEnv,execStatus,WDIALOG,"\n"); }

   /*=============================================================*/
   /* Once the load is complete, destroy the pretty print buffer. */
   /* This frees up any memory that was used to create the pretty */
   /* print forms for constructs during parsing. Thus calls to    */
   /* the mem-used function will accurately reflect the amount of */
   /* memory being used after a load command.                     */
   /*=============================================================*/

   DestroyPPBuffer(theEnv,execStatus);

   /*==========================================================*/
   /* Return a boolean flag which indicates whether any errors */
   /* were encountered while loading the constructs.           */
   /*==========================================================*/

   return(noErrors);
  }

/********************************************************************/
/* FindConstructBeginning: Searches for a left parenthesis followed */
/*   by the name of a valid construct. Used by the load command to  */
/*   find the next construct to be parsed. Returns TRUE is the      */
/*   beginning of a construct was found, otherwise FALSE.           */
/********************************************************************/
static int FindConstructBeginning(
  void *theEnv,
  EXEC_STATUS,
  char *readSource,
  struct token *theToken,
  int errorCorrection,
  int *noErrors)
  {
   int leftParenthesisFound = FALSE;
   int firstAttempt = TRUE;

   /*===================================================*/
   /* Process tokens until the beginning of a construct */
   /* is found or there are no more tokens.             */
   /*===================================================*/

   while (theToken->type != STOP)
     {
      /*=====================================================*/
      /* Constructs begin with a left parenthesis. Make note */
      /* that the opening parenthesis has been found.        */
      /*=====================================================*/

      if (theToken->type == LPAREN)
        { leftParenthesisFound = TRUE; }

      /*=================================================================*/
      /* The name of the construct follows the opening left parenthesis. */
      /* If it is the name of a valid construct, then return TRUE.       */
      /* Otherwise, reset the flags to look for the beginning of a       */
      /* construct. If error correction is being performed (i.e. the     */
      /* last construct parsed had an error in it), then don't bother to */
      /* print an error message, otherwise, print an error message.      */
      /*=================================================================*/

      else if ((theToken->type == SYMBOL) && (leftParenthesisFound == TRUE))
        {
         /*===========================================================*/
         /* Is this a valid construct name (e.g., defrule, deffacts). */
         /*===========================================================*/

         if (FindConstruct(theEnv,execStatus,ValueToString(theToken->value)) != NULL) return(TRUE);

         /*===============================================*/
         /* The construct name is invalid. Print an error */
         /* message if one hasn't already been printed.   */
         /*===============================================*/

         if (firstAttempt && (! errorCorrection))
           {
            errorCorrection = TRUE;
            *noErrors = FALSE;
            PrintErrorID(theEnv,execStatus,"CSTRCPSR",1,TRUE);
            EnvPrintRouter(theEnv,execStatus,WERROR,"Expected the beginning of a construct.\n");
           }

         /*======================================================*/
         /* Indicate that an error has been found and that we're */
         /* looking for a left parenthesis again.                */
         /*======================================================*/

         firstAttempt = FALSE;
         leftParenthesisFound = FALSE;
        }

      /*====================================================================*/
      /* Any token encountered other than a left parenthesis or a construct */
      /* name following a left parenthesis is illegal. Again, if error      */
      /* correction is in progress, no error message is printed, otherwise, */
      /*  an error message is printed.                                      */
      /*====================================================================*/

      else
        {
         if (firstAttempt && (! errorCorrection))
           {
            errorCorrection = TRUE;
            *noErrors = FALSE;
            PrintErrorID(theEnv,execStatus,"CSTRCPSR",1,TRUE);
            EnvPrintRouter(theEnv,execStatus,WERROR,"Expected the beginning of a construct.\n");
           }

         firstAttempt = FALSE;
         leftParenthesisFound = FALSE;
        }

      /*============================================*/
      /* Move on to the next token to be processed. */
      /*============================================*/

      GetToken(theEnv,execStatus,readSource,theToken);
     }

   /*===================================================================*/
   /* Couldn't find the beginning of a construct, so FALSE is returned. */
   /*===================================================================*/

   return(FALSE);
  }

/***********************************************************/
/* ParseConstruct: Parses a construct. Returns an integer. */
/*   -1 if the construct name has no parsing function, 0   */
/*   if the construct was parsed successfully, and 1 if    */
/*   the construct was parsed unsuccessfully.              */
/***********************************************************/
globle int ParseConstruct(
  void *theEnv,
  EXEC_STATUS,
  char *name,
  char *logicalName)
  {
   struct construct *currentPtr;
   int rv, ov;

   /*=================================*/
   /* Look for a valid construct name */
   /* (e.g. defrule, deffacts).       */
   /*=================================*/

   currentPtr = FindConstruct(theEnv,execStatus,name);
   if (currentPtr == NULL) return(-1);

   /*==================================*/
   /* Prepare the parsing environment. */
   /*==================================*/

   ov = GetHaltExecution(theEnv,execStatus);
   SetEvaluationError(theEnv,execStatus,FALSE);
   SetHaltExecution(theEnv,execStatus,FALSE);
   ClearParsedBindNames(theEnv,execStatus);
   PushRtnBrkContexts(theEnv,execStatus);
   ExpressionData(theEnv,execStatus)->ReturnContext = FALSE;
   ExpressionData(theEnv,execStatus)->BreakContext = FALSE;
   execStatus->CurrentEvaluationDepth++;

   /*=======================================*/
   /* Call the construct's parsing routine. */
   /*=======================================*/

   ConstructData(theEnv,execStatus)->ParsingConstruct = TRUE;
   rv = (*currentPtr->parseFunction)(theEnv,execStatus,logicalName);
   ConstructData(theEnv,execStatus)->ParsingConstruct = FALSE;

   /*===============================*/
   /* Restore environment settings. */
   /*===============================*/

   execStatus->CurrentEvaluationDepth--;
   PopRtnBrkContexts(theEnv,execStatus);

   ClearParsedBindNames(theEnv,execStatus);
   SetPPBufferStatus(theEnv,execStatus,OFF);
   SetHaltExecution(theEnv,execStatus,ov);

   /*==============================*/
   /* Return the status of parsing */
   /* the construct.               */
   /*==============================*/

   return(rv);
  }

/*********************************************************/
/* GetConstructNameAndComment: Get the name and comment  */
/*   field of a construct. Returns name of the construct */
/*   if no errors are detected, otherwise returns NULL.  */
/*********************************************************/
#if WIN_BTC && (! DEBUGGING_FUNCTIONS)
#pragma argsused
#endif
globle SYMBOL_HN *GetConstructNameAndComment(
  void *theEnv,
  EXEC_STATUS,
  char *readSource,
  struct token *inputToken,
  char *constructName,
  void *(*findFunction)(void *,EXEC_STATUS,char *),
  int (*deleteFunction)(void *,EXEC_STATUS,void *),
  char *constructSymbol,
  int fullMessageCR,
  int getComment,
  int moduleNameAllowed)
  {
#if (MAC_MCW || WIN_MCW || MAC_XCD) && (! DEBUGGING_FUNCTIONS)
#pragma unused(fullMessageCR)
#endif
   SYMBOL_HN *name, *moduleName;
   int redefining = FALSE;
   void *theConstruct;
   unsigned separatorPosition;
   struct defmodule *theModule;

   /*==========================*/
   /* Next token should be the */
   /* name of the construct.   */
   /*==========================*/

   GetToken(theEnv,execStatus,readSource,inputToken);
   if (inputToken->type != SYMBOL)
     {
      PrintErrorID(theEnv,execStatus,"CSTRCPSR",2,TRUE);
      EnvPrintRouter(theEnv,execStatus,WERROR,"Missing name for ");
      EnvPrintRouter(theEnv,execStatus,WERROR,constructName);
      EnvPrintRouter(theEnv,execStatus,WERROR," construct\n");
      return(NULL);
     }

   name = (SYMBOL_HN *) inputToken->value;

   /*===============================*/
   /* Determine the current module. */
   /*===============================*/

   separatorPosition = FindModuleSeparator(ValueToString(name));
   if (separatorPosition)
     {
      if (moduleNameAllowed == FALSE)
        {
         SyntaxErrorMessage(theEnv,execStatus,"module specifier");
         return(NULL);
        }

      moduleName = ExtractModuleName(theEnv,execStatus,separatorPosition,ValueToString(name));
      if (moduleName == NULL)
        {
         SyntaxErrorMessage(theEnv,execStatus,"construct name");
         return(NULL);
        }

      theModule = (struct defmodule *) EnvFindDefmodule(theEnv,execStatus,ValueToString(moduleName));
      if (theModule == NULL)
        {
         CantFindItemErrorMessage(theEnv,execStatus,"defmodule",ValueToString(moduleName));
         return(NULL);
        }

      EnvSetCurrentModule(theEnv,execStatus,(void *) theModule);
      name = ExtractConstructName(theEnv,execStatus,separatorPosition,ValueToString(name));
      if (name == NULL)
        {
         SyntaxErrorMessage(theEnv,execStatus,"construct name");
         return(NULL);
        }
     }

   /*=====================================================*/
   /* If the module was not specified, record the current */
   /* module name as part of the pretty-print form.       */
   /*=====================================================*/

   else
     {
      theModule = ((struct defmodule *) EnvGetCurrentModule(theEnv,execStatus));
      if (moduleNameAllowed)
        {
         PPBackup(theEnv,execStatus);
         SavePPBuffer(theEnv,execStatus,EnvGetDefmoduleName(theEnv,execStatus,theModule));
         SavePPBuffer(theEnv,execStatus,"::");
         SavePPBuffer(theEnv,execStatus,ValueToString(name));
        }
     }

   /*==================================================================*/
   /* Check for import/export conflicts from the construct definition. */
   /*==================================================================*/

#if DEFMODULE_CONSTRUCT
   if (FindImportExportConflict(theEnv,execStatus,constructName,theModule,ValueToString(name)))
     {
      ImportExportConflictMessage(theEnv,execStatus,constructName,ValueToString(name),NULL,NULL);
      return(NULL);
     }
#endif

   /*========================================================*/
   /* Remove the construct if it is already in the knowledge */
   /* base and we're not just checking syntax.               */
   /*========================================================*/

   if ((findFunction != NULL) && (! ConstructData(theEnv,execStatus)->CheckSyntaxMode))
     {
      theConstruct = (*findFunction)(theEnv,execStatus,ValueToString(name));
      if (theConstruct != NULL)
        {
         redefining = TRUE;
         if (deleteFunction != NULL)
           {
            if ((*deleteFunction)(theEnv,execStatus,theConstruct) == FALSE)
              {
               PrintErrorID(theEnv,execStatus,"CSTRCPSR",4,TRUE);
               EnvPrintRouter(theEnv,execStatus,WERROR,"Cannot redefine ");
               EnvPrintRouter(theEnv,execStatus,WERROR,constructName);
               EnvPrintRouter(theEnv,execStatus,WERROR," ");
               EnvPrintRouter(theEnv,execStatus,WERROR,ValueToString(name));
               EnvPrintRouter(theEnv,execStatus,WERROR," while it is in use.\n");
               return(NULL);
              }
           }
        }
     }

   /*=============================================*/
   /* If compilations are being watched, indicate */
   /* that a construct is being compiled.         */
   /*=============================================*/

#if DEBUGGING_FUNCTIONS
   if ((EnvGetWatchItem(theEnv,execStatus,"compilations") == TRUE) &&
       GetPrintWhileLoading(theEnv,execStatus) && (! ConstructData(theEnv,execStatus)->CheckSyntaxMode))
     {
      if (redefining) 
        {
         PrintWarningID(theEnv,execStatus,"CSTRCPSR",1,TRUE);
         EnvPrintRouter(theEnv,execStatus,WDIALOG,"Redefining ");
        }
      else EnvPrintRouter(theEnv,execStatus,WDIALOG,"Defining ");

      EnvPrintRouter(theEnv,execStatus,WDIALOG,constructName);
      EnvPrintRouter(theEnv,execStatus,WDIALOG,": ");
      EnvPrintRouter(theEnv,execStatus,WDIALOG,ValueToString(name));

      if (fullMessageCR) EnvPrintRouter(theEnv,execStatus,WDIALOG,"\n");
      else EnvPrintRouter(theEnv,execStatus,WDIALOG," ");
     }
   else
#endif
     {
      if (GetPrintWhileLoading(theEnv,execStatus) && (! ConstructData(theEnv,execStatus)->CheckSyntaxMode))
        { EnvPrintRouter(theEnv,execStatus,WDIALOG,constructSymbol); }
     }

   /*===============================*/
   /* Get the comment if it exists. */
   /*===============================*/

   GetToken(theEnv,execStatus,readSource,inputToken);
   if ((inputToken->type == STRING) && getComment)
     {
      PPBackup(theEnv,execStatus);
      SavePPBuffer(theEnv,execStatus," ");
      SavePPBuffer(theEnv,execStatus,inputToken->printForm);
      GetToken(theEnv,execStatus,readSource,inputToken);
      if (inputToken->type != RPAREN)
        {
         PPBackup(theEnv,execStatus);
         SavePPBuffer(theEnv,execStatus,"\n   ");
         SavePPBuffer(theEnv,execStatus,inputToken->printForm);
        }
     }
   else if (inputToken->type != RPAREN)
     {
      PPBackup(theEnv,execStatus);
      SavePPBuffer(theEnv,execStatus,"\n   ");
      SavePPBuffer(theEnv,execStatus,inputToken->printForm);
     }

   /*===================================*/
   /* Return the name of the construct. */
   /*===================================*/

   return(name);
  }

/****************************************/
/* RemoveConstructFromModule: Removes a */
/*   construct from its module's list   */
/****************************************/
globle void RemoveConstructFromModule(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theConstruct)
  {
   struct constructHeader *lastConstruct,*currentConstruct;

   /*==============================*/
   /* Find the specified construct */
   /* in the module's list.        */
   /*==============================*/

   lastConstruct = NULL;
   currentConstruct = theConstruct->whichModule->firstItem;
   while (currentConstruct != theConstruct)
     {
      lastConstruct = currentConstruct;
      currentConstruct = currentConstruct->next;
     }

   /*========================================*/
   /* If it wasn't there, something's wrong. */
   /*========================================*/

   if (currentConstruct == NULL)
     {
      SystemError(theEnv,execStatus,"CSTRCPSR",1);
      EnvExitRouter(theEnv,execStatus,EXIT_FAILURE);
     }

   /*==========================*/
   /* Remove it from the list. */
   /*==========================*/

   if (lastConstruct == NULL)
     { theConstruct->whichModule->firstItem = theConstruct->next; }
   else
     { lastConstruct->next = theConstruct->next; }

   /*=================================================*/
   /* Update the pointer to the last item in the list */
   /* if the construct just deleted was at the end.   */
   /*=================================================*/

   if (theConstruct == theConstruct->whichModule->lastItem)
     { theConstruct->whichModule->lastItem = lastConstruct; }
  }

/******************************************************/
/* ImportExportConflictMessage: Generic error message */
/*   for an import/export module conflict detected    */
/*   when a construct is being defined.               */
/******************************************************/
globle void ImportExportConflictMessage(
  void *theEnv,
  EXEC_STATUS,
  char *constructName,
  char *itemName,
  char *causedByConstruct,
  char *causedByName)
  {
   PrintErrorID(theEnv,execStatus,"CSTRCPSR",3,TRUE);
   EnvPrintRouter(theEnv,execStatus,WERROR,"Cannot define ");
   EnvPrintRouter(theEnv,execStatus,WERROR,constructName);
   EnvPrintRouter(theEnv,execStatus,WERROR," ");
   EnvPrintRouter(theEnv,execStatus,WERROR,itemName);
   EnvPrintRouter(theEnv,execStatus,WERROR," because of an import/export conflict");

   if (causedByConstruct == NULL) EnvPrintRouter(theEnv,execStatus,WERROR,".\n");
   else
     {
      EnvPrintRouter(theEnv,execStatus,WERROR," caused by the ");
      EnvPrintRouter(theEnv,execStatus,WERROR,causedByConstruct);
      EnvPrintRouter(theEnv,execStatus,WERROR," ");
      EnvPrintRouter(theEnv,execStatus,WERROR,causedByName);
      EnvPrintRouter(theEnv,execStatus,WERROR,".\n");
     }
  }

#endif /* (! RUN_TIME) && (! BLOAD_ONLY) */



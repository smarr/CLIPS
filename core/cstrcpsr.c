   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
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
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _CSTRCPSR_SOURCE_

#include "setup.h"

#if (! RUN_TIME) && (! BLOAD_ONLY)

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <stdlib.h>

#include "router.h"
#include "watch.h"
#include "constrct.h"
#include "prcdrpsr.h"
#include "exprnpsr.h"
#include "modulutl.h"
#include "modulpsr.h"
#include "utility.h"

#include "cstrcpsr.h"

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle int                   CheckSyntaxMode = FALSE;

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static int                     FindConstructBeginning(char *,struct token *,int,int *);

/********************************************************/
/* Load: C access routine for the load command. Returns */
/*   0 if the file couldn't be opened, -1 if the file   */
/*   was opened but an error occurred while loading     */
/*   constructs, and 1 if the file was opened and no    */
/*   errors occured while loading.                      */
/********************************************************/
globle int Load(
  char *fileName)
  {
   FILE *theFile;
   int noErrorsDetected;

   /*=======================================*/
   /* Open the file specified by file name. */
   /*=======================================*/

   if ((theFile = fopen(fileName,"r")) == NULL) return(0);

   /*===================================================*/
   /* Read in the constructs. Enabling fast load allows */
   /* the router system to be bypassed for quicker load */
   /* times.                                            */
   /*===================================================*/

   SetFastLoad(theFile);
   noErrorsDetected = LoadConstructsFromLogicalName((char *) theFile);
   SetFastLoad(NULL);

   /*=================*/
   /* Close the file. */
   /*=================*/

   fclose(theFile);

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

   if (CurrentEvaluationDepth == 0) SetHaltExecution(FALSE);
   SetEvaluationError(FALSE);

   /*========================================================*/
   /* Find the beginning of the first construct in the file. */
   /*========================================================*/

   GetToken(readSource,&theToken);
   foundConstruct = FindConstructBeginning(readSource,&theToken,FALSE,&noErrors);

   /*==================================================*/
   /* Parse the file until the end of file is reached. */
   /*==================================================*/

   while ((foundConstruct == TRUE) && (GetHaltExecution() == FALSE))
     {
      /*===========================================================*/
      /* Clear the pretty print buffer in preparation for parsing. */
      /*===========================================================*/

      FlushPPBuffer();

      /*======================*/
      /* Parse the construct. */
      /*======================*/

      constructFlag = ParseConstruct(ValueToString(theToken.value),readSource);

      /*==============================================================*/
      /* If an error occurred while parsing, then find the beginning  */
      /* of the next construct (but don't generate any more error     */
      /* messages--in effect, skip everything until another construct */
      /* is found).                                                   */
      /*==============================================================*/

      if (constructFlag == 1)
        {
         PrintRouter(WERROR,"\nERROR:\n");
         PrintInChunks(WERROR,GetPPBuffer());
         PrintRouter(WERROR,"\n");
         noErrors = FALSE;
         GetToken(readSource,&theToken);
         foundConstruct = FindConstructBeginning(readSource,&theToken,TRUE,&noErrors);
        }

      /*======================================================*/
      /* Otherwise, find the beginning of the next construct. */
      /*======================================================*/

      else
        {
         GetToken(readSource,&theToken);
         foundConstruct = FindConstructBeginning(readSource,&theToken,FALSE,&noErrors);
        }

      /*=====================================================*/
      /* Yield time if necessary to foreground applications. */
      /*=====================================================*/

       if (foundConstruct)
         { IncrementSymbolCount(theToken.value); }
       CurrentEvaluationDepth--;
       PeriodicCleanup(FALSE,TRUE);
       YieldTime();
       CurrentEvaluationDepth++;
       if (foundConstruct)
         { DecrementSymbolCount((SYMBOL_HN *) theToken.value); }
     }

   /*========================================================*/
   /* Print a carriage return if a single character is being */
   /* printed to indicate constructs are being processed.    */
   /*========================================================*/

#if DEBUGGING_FUNCTIONS
   if ((GetWatchItem("compilations") != TRUE) && GetPrintWhileLoading())
#else
   if (GetPrintWhileLoading())
#endif
     { PrintRouter(WDIALOG,"\n"); }

   /*=============================================================*/
   /* Once the load is complete, destroy the pretty print buffer. */
   /* This frees up any memory that was used to create the pretty */
   /* print forms for constructs during parsing. Thus calls to    */
   /* the mem-used function will accurately reflect the amount of */
   /* memory being used after a load command.                     */
   /*=============================================================*/

   DestroyPPBuffer();

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

         if (FindConstruct(ValueToString(theToken->value)) != NULL) return(TRUE);

         /*===============================================*/
         /* The construct name is invalid. Print an error */
         /* message if one hasn't already been printed.   */
         /*===============================================*/

         if (firstAttempt && (! errorCorrection))
           {
            errorCorrection = TRUE;
            *noErrors = FALSE;
            PrintErrorID("CSTRCPSR",1,TRUE);
            PrintRouter(WERROR,"Expected the beginning of a construct.\n");
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
            PrintErrorID("CSTRCPSR",1,TRUE);
            PrintRouter(WERROR,"Expected the beginning of a construct.\n");
           }

         firstAttempt = FALSE;
         leftParenthesisFound = FALSE;
        }

      /*============================================*/
      /* Move on to the next token to be processed. */
      /*============================================*/

      GetToken(readSource,theToken);
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
  char *name,
  char *logicalName)
  {
   struct construct *currentPtr;
   int rv, ov;

   /*=================================*/
   /* Look for a valid construct name */
   /* (e.g. defrule, deffacts).       */
   /*=================================*/

   currentPtr = FindConstruct(name);
   if (currentPtr == NULL) return(-1);

   /*==================================*/
   /* Prepare the parsing environment. */
   /*==================================*/

   ov = GetHaltExecution();
   SetEvaluationError(FALSE);
   SetHaltExecution(FALSE);
   ClearParsedBindNames();
   PushRtnBrkContexts();
   ReturnContext = FALSE;
   BreakContext = FALSE;
   CurrentEvaluationDepth++;

   /*=======================================*/
   /* Call the construct's parsing routine. */
   /*=======================================*/

   rv = (*currentPtr->parseFunction)(logicalName);

   /*===============================*/
   /* Restore environment settings. */
   /*===============================*/

   CurrentEvaluationDepth--;
   PopRtnBrkContexts();

   ClearParsedBindNames();
   SetPPBufferStatus(OFF);
   SetHaltExecution(ov);

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
globle SYMBOL_HN *GetConstructNameAndComment(
  char *readSource,
  struct token *inputToken,
  char *constructName,
  void *(*findFunction)(char *),
  int (*deleteFunction)(void *),
  char *constructSymbol,
  int fullMessageCR,
  int getComment,
  int moduleNameAllowed)
  {
   SYMBOL_HN *name, *moduleName;
   int redefining = FALSE;
   void *theConstruct;
   int separatorPosition;
   struct defmodule *theModule;

   /*==========================*/
   /* Next token should be the */
   /* name of the construct.   */
   /*==========================*/

   GetToken(readSource,inputToken);
   if (inputToken->type != SYMBOL)
     {
      PrintErrorID("CSTRCPSR",2,TRUE);
      PrintRouter(WERROR,"Missing name for ");
      PrintRouter(WERROR,constructName);
      PrintRouter(WERROR," construct\n");
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
         SyntaxErrorMessage("module specifier");
         return(NULL);
        }

      moduleName = ExtractModuleName(separatorPosition,ValueToString(name));
      if (moduleName == NULL)
        {
         SyntaxErrorMessage("construct name");
         return(NULL);
        }

      theModule = (struct defmodule *) FindDefmodule(ValueToString(moduleName));
      if (theModule == NULL)
        {
         CantFindItemErrorMessage("defmodule",ValueToString(moduleName));
         return(NULL);
        }

      SetCurrentModule((void *) theModule);
      name = ExtractConstructName(separatorPosition,ValueToString(name));
      if (name == NULL)
        {
         SyntaxErrorMessage("construct name");
         return(NULL);
        }
     }

   /*=====================================================*/
   /* If the module was not specified, record the current */
   /* module name as part of the pretty-print form.       */
   /*=====================================================*/

   else
     {
      theModule = ((struct defmodule *) GetCurrentModule());
      if (moduleNameAllowed)
        {
         PPBackup();
         SavePPBuffer(GetDefmoduleName(theModule));
         SavePPBuffer("::");
         SavePPBuffer(ValueToString(name));
        }
     }

   /*==================================================================*/
   /* Check for import/export conflicts from the construct definition. */
   /*==================================================================*/

#if DEFMODULE_CONSTRUCT
   if (FindImportExportConflict(constructName,theModule,ValueToString(name)))
     {
      ImportExportConflictMessage(constructName,ValueToString(name),NULL,NULL);
      return(NULL);
     }
#endif

   /*========================================================*/
   /* Remove the construct if it is already in the knowledge */
   /* base and we're not just checking syntax.               */
   /*========================================================*/

   if ((findFunction != NULL) && (! CheckSyntaxMode))
     {
      theConstruct = (*findFunction)(ValueToString(name));
      if (theConstruct != NULL)
        {
         redefining = TRUE;
         if (deleteFunction != NULL)
           {
            if ((*deleteFunction)(theConstruct) == FALSE)
              {
               PrintErrorID("CSTRCPSR",4,TRUE);
               PrintRouter(WERROR,"Cannot redefine ");
               PrintRouter(WERROR,constructName);
               PrintRouter(WERROR," ");
               PrintRouter(WERROR,ValueToString(name));
               PrintRouter(WERROR," while it is in use.\n");
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
   if ((GetWatchItem("compilations") == TRUE) &&
       GetPrintWhileLoading() && (! CheckSyntaxMode))
     {
      if (redefining) PrintRouter(WDIALOG,"Redefining ");
      else PrintRouter(WDIALOG,"Defining ");

      PrintRouter(WDIALOG,constructName);
      PrintRouter(WDIALOG,": ");
      PrintRouter(WDIALOG,ValueToString(name));

      if (fullMessageCR) PrintRouter(WDIALOG,"\n");
      else PrintRouter(WDIALOG," ");
     }
   else
#endif
     {
      if (GetPrintWhileLoading() && (! CheckSyntaxMode))
        { PrintRouter(WDIALOG,constructSymbol); }
     }

   /*===============================*/
   /* Get the comment if it exists. */
   /*===============================*/

   GetToken(readSource,inputToken);
   if ((inputToken->type == STRING) && getComment)
     {
      PPBackup();
      SavePPBuffer(" ");
      SavePPBuffer(inputToken->printForm);
      GetToken(readSource,inputToken);
      if (inputToken->type != RPAREN)
        {
         PPBackup();
         SavePPBuffer("\n   ");
         SavePPBuffer(inputToken->printForm);
        }
     }
   else if (inputToken->type != RPAREN)
     {
      PPBackup();
      SavePPBuffer("\n   ");
      SavePPBuffer(inputToken->printForm);
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
      SystemError("CSTRCPSR",1);
      ExitRouter(EXIT_FAILURE);
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
  char *constructName,
  char *itemName,
  char *causedByConstruct,
  char *causedByName)
  {
   PrintErrorID("CSTRCPSR",3,TRUE);
   PrintRouter(WERROR,"Cannot define ");
   PrintRouter(WERROR,constructName);
   PrintRouter(WERROR," ");
   PrintRouter(WERROR,itemName);
   PrintRouter(WERROR," because of an import/export conflict");

   if (causedByConstruct == NULL) PrintRouter(WERROR,".\n");
   else
     {
      PrintRouter(WERROR," caused by the ");
      PrintRouter(WERROR,causedByConstruct);
      PrintRouter(WERROR," ");
      PrintRouter(WERROR,causedByName);
      PrintRouter(WERROR,".\n");
     }
  }

#endif /* (! RUN_TIME) && (! BLOAD_ONLY) */


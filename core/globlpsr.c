   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*              DEFGLOBAL PARSER MODULE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Parses the defglobal construct.                  */
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
/*            Made the construct redefinition message more   */
/*            prominent.                                     */
/*                                                           */
/*************************************************************/

#define _GLOBLPSR_SOURCE_

#include "setup.h"

#if DEFGLOBAL_CONSTRUCT

#include <string.h>

#include "pprint.h"
#include "router.h"
#include "memalloc.h"
#include "scanner.h"
#include "evaluatn.h"
#include "exprnpsr.h"
#include "constrct.h"
#include "multifld.h"
#include "watch.h"
#include "modulutl.h"
#include "modulpsr.h"
#include "cstrcpsr.h"
#include "globldef.h"
#include "globlbsc.h"
#include "envrnmnt.h"

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
#include "bload.h"
#endif

#include "globlpsr.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if (! RUN_TIME) && (! BLOAD_ONLY)
   static intBool                 GetVariableDefinition(void *,EXEC_STATUS,char *,int *,int,struct token *);
   static void                    AddDefglobal(void *,EXEC_STATUS,SYMBOL_HN *,DATA_OBJECT_PTR,struct expr *);
#endif

/*********************************************************************/
/* ParseDefglobal: Coordinates all actions necessary for the parsing */
/*   and creation of a defglobal into the current environment.       */
/*********************************************************************/
globle intBool ParseDefglobal(
  void *theEnv,
  EXEC_STATUS,
  char *readSource)
  {
   int defglobalError = FALSE;
#if (MAC_MCW || WIN_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(theEnv,execStatus,readSource)
#endif

#if (! RUN_TIME) && (! BLOAD_ONLY)

   struct token theToken;
   int tokenRead = TRUE;
   struct defmodule *theModule;

   /*=====================================*/
   /* Pretty print buffer initialization. */
   /*=====================================*/

   SetPPBufferStatus(theEnv,execStatus,ON);
   FlushPPBuffer(theEnv,execStatus);
   SetIndentDepth(theEnv,execStatus,3);
   SavePPBuffer(theEnv,execStatus,"(defglobal ");

   /*=================================================*/
   /* Individual defglobal constructs can't be parsed */
   /* while a binary load is in effect.               */
   /*=================================================*/

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
   if ((Bloaded(theEnv,execStatus) == TRUE) && (! ConstructData(theEnv,execStatus)->CheckSyntaxMode))
     {
      CannotLoadWithBloadMessage(theEnv,execStatus,"defglobal");
      return(TRUE);
     }
#endif

   /*===========================*/
   /* Look for the module name. */
   /*===========================*/

   GetToken(theEnv,execStatus,readSource,&theToken);
   if (theToken.type == SYMBOL)
     {
      /*=================================================*/
      /* The optional module name can't contain a module */
      /* separator like other constructs. For example,   */
      /* (defrule X::foo is OK for rules, but the right  */
      /* syntax for defglobals is (defglobal X ?*foo*.   */
      /*=================================================*/

      tokenRead = FALSE;
      if (FindModuleSeparator(ValueToString(theToken.value)))
        {
         SyntaxErrorMessage(theEnv,execStatus,"defglobal");
         return(TRUE);
        }

      /*=================================*/
      /* Determine if the module exists. */
      /*=================================*/

      theModule = (struct defmodule *) EnvFindDefmodule(theEnv,execStatus,ValueToString(theToken.value));
      if (theModule == NULL)
        {
         CantFindItemErrorMessage(theEnv,execStatus,"defmodule",ValueToString(theToken.value));
         return(TRUE);
        }

      /*=========================================*/
      /* If the module name was OK, then set the */
      /* current module to the specified module. */
      /*=========================================*/

      SavePPBuffer(theEnv,execStatus," ");
      EnvSetCurrentModule(theEnv,execStatus,(void *) theModule);
     }

   /*===========================================*/
   /* If the module name wasn't specified, then */
   /* use the current module's name in the      */
   /* defglobal's pretty print representation.  */
   /*===========================================*/

   else
     {
      PPBackup(theEnv,execStatus);
      SavePPBuffer(theEnv,execStatus,EnvGetDefmoduleName(theEnv,execStatus,((struct defmodule *) EnvGetCurrentModule(theEnv,execStatus))));
      SavePPBuffer(theEnv,execStatus," ");
      SavePPBuffer(theEnv,execStatus,theToken.printForm);
     }

   /*======================*/
   /* Parse the variables. */
   /*======================*/

   while (GetVariableDefinition(theEnv,execStatus,readSource,&defglobalError,tokenRead,&theToken))
     {
      tokenRead = FALSE;

      FlushPPBuffer(theEnv,execStatus);
      SavePPBuffer(theEnv,execStatus,"(defglobal ");
      SavePPBuffer(theEnv,execStatus,EnvGetDefmoduleName(theEnv,execStatus,((struct defmodule *) EnvGetCurrentModule(theEnv,execStatus))));
      SavePPBuffer(theEnv,execStatus," ");
     }

#endif

   /*==================================*/
   /* Return the parsing error status. */
   /*==================================*/

   return(defglobalError);
  }

#if (! RUN_TIME) && (! BLOAD_ONLY)

/***************************************************************/
/* GetVariableDefinition: Parses and evaluates a single global */
/*   variable in a defglobal construct. Returns TRUE if the    */
/*   variable was successfully parsed and FALSE if a right     */
/*   parenthesis is encountered (signifying the end of the     */
/*   defglobal construct) or an error occurs. The error status */
/*   flag is also set if an error occurs.                      */
/***************************************************************/
static intBool GetVariableDefinition(
  void *theEnv,
  EXEC_STATUS,
  char *readSource,
  int *defglobalError,
  int tokenRead,
  struct token *theToken)
  {
   SYMBOL_HN *variableName;
   struct expr *assignPtr;
   DATA_OBJECT assignValue;

   /*========================================*/
   /* Get next token, which should either be */
   /* a closing parenthesis or a variable.   */
   /*========================================*/

   if (! tokenRead) GetToken(theEnv,execStatus,readSource,theToken);
   if (theToken->type == RPAREN) return(FALSE);

   if (theToken->type == SF_VARIABLE)
     {
      SyntaxErrorMessage(theEnv,execStatus,"defglobal");
      *defglobalError = TRUE;
      return(FALSE);
     }
   else if (theToken->type != GBL_VARIABLE)
     {
      SyntaxErrorMessage(theEnv,execStatus,"defglobal");
      *defglobalError = TRUE;
      return(FALSE);
     }

   variableName = (SYMBOL_HN *) theToken->value;

   SavePPBuffer(theEnv,execStatus," ");

   /*================================*/
   /* Print out compilation message. */
   /*================================*/

#if DEBUGGING_FUNCTIONS
   if ((EnvGetWatchItem(theEnv,execStatus,"compilations") == ON) && GetPrintWhileLoading(theEnv,execStatus))
     {
      if (QFindDefglobal(theEnv,execStatus,variableName) != NULL) 
        {
         PrintWarningID(theEnv,execStatus,"CSTRCPSR",1,TRUE);
         EnvPrintRouter(theEnv,WDIALOG,"Redefining defglobal: ");
        }
      else EnvPrintRouter(theEnv,WDIALOG,"Defining defglobal: ");
      EnvPrintRouter(theEnv,WDIALOG,ValueToString(variableName));
      EnvPrintRouter(theEnv,WDIALOG,"\n");
     }
   else
#endif
     { if (GetPrintWhileLoading(theEnv,execStatus)) EnvPrintRouter(theEnv,WDIALOG,":"); }

   /*==================================================================*/
   /* Check for import/export conflicts from the construct definition. */
   /*==================================================================*/

#if DEFMODULE_CONSTRUCT
   if (FindImportExportConflict(theEnv,execStatus,"defglobal",((struct defmodule *) EnvGetCurrentModule(theEnv,execStatus)),ValueToString(variableName)))
     {
      ImportExportConflictMessage(theEnv,execStatus,"defglobal",ValueToString(variableName),NULL,NULL);
      *defglobalError = TRUE;
      return(FALSE);
     }
#endif

   /*==============================*/
   /* The next token must be an =. */
   /*==============================*/

   GetToken(theEnv,execStatus,readSource,theToken);
   if (strcmp(theToken->printForm,"=") != 0)
     {
      SyntaxErrorMessage(theEnv,execStatus,"defglobal");
      *defglobalError = TRUE;
      return(FALSE);
     }

   SavePPBuffer(theEnv,execStatus," ");

   /*======================================================*/
   /* Parse the expression to be assigned to the variable. */
   /*======================================================*/

   assignPtr = ParseAtomOrExpression(theEnv,execStatus,readSource,NULL);
   if (assignPtr == NULL)
     {
      *defglobalError = TRUE;
      return(FALSE);
     }

   /*==========================*/
   /* Evaluate the expression. */
   /*==========================*/

   if (! ConstructData(theEnv,execStatus)->CheckSyntaxMode)
     {
      SetEvaluationError(theEnv,execStatus,FALSE);
      if (EvaluateExpression(theEnv,execStatus,assignPtr,&assignValue))
        {
         ReturnExpression(theEnv,execStatus,assignPtr);
         *defglobalError = TRUE;
         return(FALSE);
        }
     }
   else
     { ReturnExpression(theEnv,execStatus,assignPtr); }

   SavePPBuffer(theEnv,execStatus,")");

   /*======================================*/
   /* Add the variable to the global list. */
   /*======================================*/

   if (! ConstructData(theEnv,execStatus)->CheckSyntaxMode)
     { AddDefglobal(theEnv,execStatus,variableName,&assignValue,assignPtr); }

   /*==================================================*/
   /* Return TRUE to indicate that the global variable */
   /* definition was successfully parsed.              */
   /*==================================================*/

   return(TRUE);
  }

/*********************************************************/
/* AddDefglobal: Adds a defglobal to the current module. */
/*********************************************************/
static void AddDefglobal(
  void *theEnv,
  EXEC_STATUS,
  SYMBOL_HN *name,
  DATA_OBJECT_PTR vPtr,
  struct expr *ePtr)
  {
   struct defglobal *defglobalPtr;
   intBool newGlobal = FALSE;
#if DEBUGGING_FUNCTIONS
   int GlobalHadWatch = FALSE;
#endif

   /*========================================================*/
   /* If the defglobal is already defined, then use the old  */
   /* data structure and substitute new values. If it hasn't */
   /* been defined, then create a new data structure.        */
   /*========================================================*/

   defglobalPtr = QFindDefglobal(theEnv,execStatus,name);
   if (defglobalPtr == NULL)
     {
      newGlobal = TRUE;
      defglobalPtr = get_struct(theEnv,defglobal);
     }
   else
     {
      DeinstallConstructHeader(theEnv,execStatus,&defglobalPtr->header);
#if DEBUGGING_FUNCTIONS
      GlobalHadWatch = defglobalPtr->watch;
#endif
     }

   /*===========================================*/
   /* Remove the old values from the defglobal. */
   /*===========================================*/

   if (newGlobal == FALSE)
     {
      ValueDeinstall(theEnv,execStatus,&defglobalPtr->current);
      if (defglobalPtr->current.type == MULTIFIELD)
        { ReturnMultifield(theEnv,execStatus,(struct multifield *) defglobalPtr->current.value); }

      RemoveHashedExpression(theEnv,execStatus,defglobalPtr->initial);
     }

   /*=======================================*/
   /* Copy the new values to the defglobal. */
   /*=======================================*/

   defglobalPtr->current.type = vPtr->type;
   if (vPtr->type != MULTIFIELD) defglobalPtr->current.value = vPtr->value;
   else DuplicateMultifield(theEnv,execStatus,&defglobalPtr->current,vPtr);
   ValueInstall(theEnv,execStatus,&defglobalPtr->current);

   defglobalPtr->initial = AddHashedExpression(theEnv,execStatus,ePtr);
   ReturnExpression(theEnv,execStatus,ePtr);
   DefglobalData(theEnv,execStatus)->ChangeToGlobals = TRUE;

   /*=================================*/
   /* Restore the old watch value to  */
   /* the defglobal if redefined.     */
   /*=================================*/

#if DEBUGGING_FUNCTIONS
   defglobalPtr->watch = GlobalHadWatch ? TRUE : WatchGlobals;
#endif

   /*======================================*/
   /* Save the name and pretty print form. */
   /*======================================*/

   defglobalPtr->header.name = name;
   defglobalPtr->header.usrData = NULL;
   IncrementSymbolCount(name);

   SavePPBuffer(theEnv,execStatus,"\n");
   if (EnvGetConserveMemory(theEnv,execStatus) == TRUE)
     { defglobalPtr->header.ppForm = NULL; }
   else
     { defglobalPtr->header.ppForm = CopyPPBuffer(theEnv,execStatus); }

   defglobalPtr->inScope = TRUE;

   /*=============================================*/
   /* If the defglobal was redefined, we're done. */
   /*=============================================*/

   if (newGlobal == FALSE) return;

   /*===================================*/
   /* Copy the defglobal variable name. */
   /*===================================*/

   defglobalPtr->busyCount = 0;
   defglobalPtr->header.whichModule = (struct defmoduleItemHeader *)
                               GetModuleItem(theEnv,execStatus,NULL,FindModuleItem(theEnv,execStatus,"defglobal")->moduleIndex);

   /*=============================================*/
   /* Add the defglobal to the list of defglobals */
   /* for the current module.                     */
   /*=============================================*/

   AddConstructToModule(&defglobalPtr->header);
  }

/*****************************************************************/
/* ReplaceGlobalVariable: Replaces a global variable found in an */
/*   expression with the appropriate primitive data type which   */
/*   can later be used to retrieve the global variable's value.  */
/*****************************************************************/
globle intBool ReplaceGlobalVariable(
  void *theEnv,
  EXEC_STATUS,
  struct expr *ePtr)
  {
   struct defglobal *theGlobal;
   int count;

   /*=================================*/
   /* Search for the global variable. */
   /*=================================*/

   theGlobal = (struct defglobal *)
               FindImportedConstruct(theEnv,execStatus,"defglobal",NULL,ValueToString(ePtr->value),
                                     &count,TRUE,NULL);

   /*=============================================*/
   /* If it wasn't found, print an error message. */
   /*=============================================*/

   if (theGlobal == NULL)
     {
      GlobalReferenceErrorMessage(theEnv,execStatus,ValueToString(ePtr->value));
      return(FALSE);
     }

   /*========================================================*/
   /* The current implementation of the defmodules shouldn't */
   /* allow a construct to be defined which would cause an   */
   /* ambiguous reference, but we'll check for it anyway.    */
   /*========================================================*/

   if (count > 1)
     {
      AmbiguousReferenceErrorMessage(theEnv,execStatus,"defglobal",ValueToString(ePtr->value));
      return(FALSE);
     }

   /*==============================================*/
   /* Replace the symbolic reference of the global */
   /* variable with a direct pointer reference.    */
   /*==============================================*/

   ePtr->type = DEFGLOBAL_PTR;
   ePtr->value = (void *) theGlobal;

   return(TRUE);
  }

/*****************************************************************/
/* GlobalReferenceErrorMessage: Prints an error message when a   */
/*   symbolic reference to a global variable cannot be resolved. */
/*****************************************************************/
globle void GlobalReferenceErrorMessage(
  void *theEnv,
  EXEC_STATUS,
  char *variableName)
  {
   PrintErrorID(theEnv,execStatus,"GLOBLPSR",1,TRUE);
   EnvPrintRouter(theEnv,WERROR,"\nGlobal variable ?*");
   EnvPrintRouter(theEnv,WERROR,variableName);
   EnvPrintRouter(theEnv,WERROR,"* was referenced, but is not defined.\n");
  }

#endif /* (! RUN_TIME) && (! BLOAD_ONLY) */

#endif /* DEFGLOBAL_CONSTRUCT */




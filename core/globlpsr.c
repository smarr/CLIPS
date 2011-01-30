   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
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
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
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

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
#include "bload.h"
#endif

#include "globlpsr.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if (! RUN_TIME) && (! BLOAD_ONLY)
   static BOOLEAN                 GetVariableDefinition(char *,int *,int,struct token *);
   static void                    AddDefglobal(SYMBOL_HN *,DATA_OBJECT_PTR,struct expr *);
#endif

/*********************************************************************/
/* ParseDefglobal: Coordinates all actions necessary for the parsing */
/*   and creation of a defglobal into the current environment.       */
/*********************************************************************/
globle BOOLEAN ParseDefglobal(
  char *readSource)
  {
   int defglobalError = FALSE;
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(readSource)
#endif

#if (! RUN_TIME) && (! BLOAD_ONLY)

   struct token theToken;
   int tokenRead = TRUE;
   struct defmodule *theModule;

   /*=====================================*/
   /* Pretty print buffer initialization. */
   /*=====================================*/

   SetPPBufferStatus(ON);
   FlushPPBuffer();
   SetIndentDepth(3);
   SavePPBuffer("(defglobal ");

   /*=================================================*/
   /* Individual defglobal constructs can't be parsed */
   /* while a binary load is in effect.               */
   /*=================================================*/

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
   if ((Bloaded() == TRUE) && (! CheckSyntaxMode))
     {
      CannotLoadWithBloadMessage("defglobal");
      return(TRUE);
     }
#endif

   /*===========================*/
   /* Look for the module name. */
   /*===========================*/

   GetToken(readSource,&theToken);
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
         SyntaxErrorMessage("defglobal");
         return(TRUE);
        }

      /*=================================*/
      /* Determine if the module exists. */
      /*=================================*/

      theModule = (struct defmodule *) FindDefmodule(ValueToString(theToken.value));
      if (theModule == NULL)
        {
         CantFindItemErrorMessage("defmodule",ValueToString(theToken.value));
         return(TRUE);
        }

      /*=========================================*/
      /* If the module name was OK, then set the */
      /* current module to the specified module. */
      /*=========================================*/

      SavePPBuffer(" ");
      SetCurrentModule((void *) theModule);
     }

   /*===========================================*/
   /* If the module name wasn't specified, then */
   /* use the current module's name in the      */
   /* defglobal's pretty print representation.  */
   /*===========================================*/

   else
     {
      PPBackup();
      SavePPBuffer(GetDefmoduleName(((struct defmodule *) GetCurrentModule())));
      SavePPBuffer(" ");
      SavePPBuffer(theToken.printForm);
     }

   /*======================*/
   /* Parse the variables. */
   /*======================*/

   while (GetVariableDefinition(readSource,&defglobalError,tokenRead,&theToken))
     {
      tokenRead = FALSE;

      FlushPPBuffer();
      SavePPBuffer("(defglobal ");
      SavePPBuffer(GetDefmoduleName(((struct defmodule *) GetCurrentModule())));
      SavePPBuffer(" ");
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
static BOOLEAN GetVariableDefinition(
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

   if (! tokenRead) GetToken(readSource,theToken);
   if (theToken->type == RPAREN) return(FALSE);

   if (theToken->type == SF_VARIABLE)
     {
      SyntaxErrorMessage("defglobal");
      *defglobalError = TRUE;
      return(FALSE);
     }
   else if (theToken->type != GBL_VARIABLE)
     {
      SyntaxErrorMessage("defglobal");
      *defglobalError = TRUE;
      return(FALSE);
     }

   variableName = (SYMBOL_HN *) theToken->value;

   SavePPBuffer(" ");

   /*================================*/
   /* Print out compilation message. */
   /*================================*/

#if DEBUGGING_FUNCTIONS
   if ((GetWatchItem("compilations") == ON) && GetPrintWhileLoading())
     {
      if (QFindDefglobal(variableName) != NULL) PrintRouter(WDIALOG,"Redefining defglobal: ?");
      else PrintRouter(WDIALOG,"Defining defglobal: ");
      PrintRouter(WDIALOG,ValueToString(variableName));
      PrintRouter(WDIALOG,"\n");
     }
   else
#endif
     { if (GetPrintWhileLoading()) PrintRouter(WDIALOG,":"); }

   /*==================================================================*/
   /* Check for import/export conflicts from the construct definition. */
   /*==================================================================*/

#if DEFMODULE_CONSTRUCT
   if (FindImportExportConflict("defglobal",((struct defmodule *) GetCurrentModule()),ValueToString(variableName)))
     {
      ImportExportConflictMessage("defglobal",ValueToString(variableName),NULL,NULL);
      *defglobalError = TRUE;
      return(FALSE);
     }
#endif

   /*==============================*/
   /* The next token must be an =. */
   /*==============================*/

   GetToken(readSource,theToken);
   if (strcmp(theToken->printForm,"=") != 0)
     {
      SyntaxErrorMessage("defglobal");
      *defglobalError = TRUE;
      return(FALSE);
     }

   SavePPBuffer(" ");

   /*======================================================*/
   /* Parse the expression to be assigned to the variable. */
   /*======================================================*/

   assignPtr = ParseAtomOrExpression(readSource,NULL);
   if (assignPtr == NULL)
     {
      *defglobalError = TRUE;
      return(FALSE);
     }

   /*==========================*/
   /* Evaluate the expression. */
   /*==========================*/

   if (! CheckSyntaxMode)
     {
      SetEvaluationError(FALSE);
      if (EvaluateExpression(assignPtr,&assignValue))
        {
         ReturnExpression(assignPtr);
         *defglobalError = TRUE;
         return(FALSE);
        }
     }
   else
     { ReturnExpression(assignPtr); }

   SavePPBuffer(")");

   /*======================================*/
   /* Add the variable to the global list. */
   /*======================================*/

   if (! CheckSyntaxMode)
     { AddDefglobal(variableName,&assignValue,assignPtr); }

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
  SYMBOL_HN *name,
  DATA_OBJECT_PTR vPtr,
  struct expr *ePtr)
  {
   struct defglobal *defglobalPtr;
   BOOLEAN newGlobal = FALSE;
#if DEBUGGING_FUNCTIONS
   int GlobalHadWatch = FALSE;
#endif

   /*========================================================*/
   /* If the defglobal is already defined, then use the old  */
   /* data structure and substitute new values. If it hasn't */
   /* been defined, then create a new data structure.        */
   /*========================================================*/

   defglobalPtr = QFindDefglobal(name);
   if (defglobalPtr == NULL)
     {
      newGlobal = TRUE;
      defglobalPtr = get_struct(defglobal);
     }
   else
     {
      DeinstallConstructHeader(&defglobalPtr->header);
#if DEBUGGING_FUNCTIONS
      GlobalHadWatch = defglobalPtr->watch;
#endif
     }

   /*===========================================*/
   /* Remove the old values from the defglobal. */
   /*===========================================*/

   if (newGlobal == FALSE)
     {
      ValueDeinstall(&defglobalPtr->current);
      if (defglobalPtr->current.type == MULTIFIELD)
        { ReturnMultifield((struct multifield *) defglobalPtr->current.value); }

      RemoveHashedExpression(defglobalPtr->initial);
     }

   /*=======================================*/
   /* Copy the new values to the defglobal. */
   /*=======================================*/

   defglobalPtr->current.type = vPtr->type;
   if (vPtr->type != MULTIFIELD) defglobalPtr->current.value = vPtr->value;
   else DuplicateMultifield(&defglobalPtr->current,vPtr);
   ValueInstall(&defglobalPtr->current);

   defglobalPtr->initial = AddHashedExpression(ePtr);
   ReturnExpression(ePtr);
   ChangeToGlobals = TRUE;

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

   SavePPBuffer("\n");
   if (GetConserveMemory() == TRUE)
     { defglobalPtr->header.ppForm = NULL; }
   else
     { defglobalPtr->header.ppForm = CopyPPBuffer(); }

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
                               GetModuleItem(NULL,FindModuleItem("defglobal")->moduleIndex);

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
globle BOOLEAN ReplaceGlobalVariable(
  struct expr *ePtr)
  {
   struct defglobal *theGlobal;
   int count;

   /*=================================*/
   /* Search for the global variable. */
   /*=================================*/

   theGlobal = (struct defglobal *)
               FindImportedConstruct("defglobal",NULL,ValueToString(ePtr->value),
                                     &count,TRUE,NULL);

   /*=============================================*/
   /* If it wasn't found, print an error message. */
   /*=============================================*/

   if (theGlobal == NULL)
     {
      GlobalReferenceErrorMessage(ValueToString(ePtr->value));
      return(FALSE);
     }

   /*========================================================*/
   /* The current implementation of the defmodules shouldn't */
   /* allow a construct to be defined which would cause an   */
   /* ambiguous reference, but we'll check for it anyway.    */
   /*========================================================*/

   if (count > 1)
     {
      AmbiguousReferenceErrorMessage("defglobal",ValueToString(ePtr->value));
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
  char *variableName)
  {
   PrintErrorID("GLOBLPSR",1,TRUE);
   PrintRouter(WERROR,"\nGlobal variable ?*");
   PrintRouter(WERROR,variableName);
   PrintRouter(WERROR,"* was referenced, but is not defined.\n");
  }

#endif /* (! RUN_TIME) && (! BLOAD_ONLY) */

#endif /* DEFGLOBAL_CONSTRUCT */




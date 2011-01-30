   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*              DEFMODULE PARSER MODULE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Parses a defmodule construct.                    */
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

#define _MODULPSR_SOURCE_

#include "setup.h"

#if DEFMODULE_CONSTRUCT && (! RUN_TIME) && (! BLOAD_ONLY)

#include <stdio.h>
#include <string.h>
#define _STDIO_INCLUDED_

#include "memalloc.h"
#include "constant.h"
#include "router.h"
#include "extnfunc.h"
#include "argacces.h"
#include "cstrcpsr.h"
#include "constrct.h"
#include "modulutl.h"
#include "utility.h"

#if BLOAD || BLOAD_AND_BSAVE
#include "bload.h"
#endif

#include "modulpsr.h"

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static struct portConstructItem    *ListOfPortConstructItems = NULL;
   Thread static long                         NumberOfDefmodules;
   Thread static struct callFunctionItem     *AfterModuleDefinedFunctions = NULL;

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static int                        ParsePortSpecifications(char *,struct token *,
                                                             struct defmodule *);
   static int                        ParseImportSpec(char *,struct token *,
                                                     struct defmodule *);
   static int                        ParseExportSpec(char *,struct token *,
                                                     struct defmodule *,
                                                     struct defmodule *);
   static BOOLEAN                    DeleteDefmodule(void *);
   static int                        FindMultiImportConflict(struct defmodule *);
   static void                       NotExportedErrorMessage(char *,char *,char *);

/*********************************************/
/* GetNumberOfDefmodules: Returns the number */
/*   of defmodules currently defined.        */
/*********************************************/
globle long GetNumberOfDefmodules()
  {
   return(NumberOfDefmodules);
  }

/******************************************/
/* SetNumberOfDefmodules: Sets the number */
/*   of defmodules currently defined.     */
/******************************************/
globle void SetNumberOfDefmodules(
  long value)
  {
   NumberOfDefmodules = value;
  }

/****************************************************/
/* AddAfterModuleChangeFunction: Adds a function to */
/*   the list of functions that are to be called    */
/*   after a module change occurs.                  */
/****************************************************/
globle void AddAfterModuleDefinedFunction(
  char *name,
  void (*func)(void),
  int priority)
  {
   AfterModuleDefinedFunctions =
     AddFunctionToCallList(name,priority,func,AfterModuleDefinedFunctions);
  }

/******************************************************/
/* AddPortConstructItem: Adds an item to the list of  */
/*   items that can be imported/exported by a module. */
/******************************************************/
globle void AddPortConstructItem(
  char *theName,
  int theType)
  {
   struct portConstructItem *newItem;

   newItem = get_struct(portConstructItem);
   newItem->constructName = theName;
   newItem->typeExpected = theType;
   newItem->next = ListOfPortConstructItems;
   ListOfPortConstructItems = newItem;
  }

/******************************************************/
/* ParseDefmodule: Coordinates all actions necessary  */
/*   for the parsing and creation of a defmodule into */
/*   the current environment.                         */
/******************************************************/
globle int ParseDefmodule(
  char *readSource)
  {
   SYMBOL_HN *defmoduleName;
   struct defmodule *newDefmodule;
   struct token inputToken;
   int i;
   struct moduleItem *theItem;
   struct portItem *portSpecs, *nextSpec;
   struct defmoduleItemHeader *theHeader;
   struct callFunctionItem *defineFunctions;
   struct defmodule *redefiningMainModule = NULL;
   int parseError;
   struct portItem *oldImportList, *oldExportList;
   short overwrite = FALSE;

   /*================================================*/
   /* Flush the buffer which stores the pretty print */
   /* representation for a module.  Add the already  */
   /* parsed keyword defmodule to this buffer.       */
   /*================================================*/

   SetPPBufferStatus(ON);
   FlushPPBuffer();
   SetIndentDepth(3);
   SavePPBuffer("(defmodule ");

   /*===============================*/
   /* Modules cannot be loaded when */
   /* a binary load is in effect.   */
   /*===============================*/

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
   if ((Bloaded() == TRUE) && (! CheckSyntaxMode))
     {
      CannotLoadWithBloadMessage("defmodule");
      return(TRUE);
     }
#endif

   /*=====================================================*/
   /* Parse the name and comment fields of the defmodule. */
   /* Remove the defmodule if it already exists.          */
   /*=====================================================*/

   defmoduleName = GetConstructNameAndComment(readSource,&inputToken,"defmodule",
                                              FindDefmodule,DeleteDefmodule,"+",
                                              TRUE,TRUE,FALSE);
   if (defmoduleName == NULL) { return(TRUE); }

   if (strcmp(ValueToString(defmoduleName),"MAIN") == 0)
     { redefiningMainModule = (struct defmodule *) FindDefmodule("MAIN"); }

   /*==============================================*/
   /* Create the defmodule structure if necessary. */
   /*==============================================*/

   if (redefiningMainModule == NULL)
     {
      newDefmodule = (struct defmodule *) FindDefmodule(ValueToString(defmoduleName));
      if (newDefmodule)
        { overwrite = TRUE; }
      else
        {
         newDefmodule = get_struct(defmodule);
         newDefmodule->name = defmoduleName;
         newDefmodule->usrData = NULL;
         newDefmodule->next = NULL;
        }
     }
   else
     {
      overwrite = TRUE;
      newDefmodule = redefiningMainModule;
     }

   if (overwrite)
     {
      oldImportList = newDefmodule->importList;
      oldExportList = newDefmodule->exportList;
     }

   newDefmodule->importList = NULL;
   newDefmodule->exportList = NULL;

   /*===================================*/
   /* Finish parsing the defmodule (its */
   /* import/export specifications).    */
   /*===================================*/

   parseError = ParsePortSpecifications(readSource,&inputToken,newDefmodule);

   /*====================================*/
   /* Check for import/export conflicts. */
   /*====================================*/

   if (! parseError) parseError = FindMultiImportConflict(newDefmodule);

   /*======================================================*/
   /* If an error occured in parsing or an import conflict */
   /* was detected, abort the definition of the defmodule. */
   /* If we're only checking syntax, then we want to exit  */
   /* at this point as well.                               */
   /*======================================================*/

   if (parseError || CheckSyntaxMode)
     {
      while (newDefmodule->importList != NULL)
        {
         nextSpec = newDefmodule->importList->next;
         rtn_struct(portItem,newDefmodule->importList);
         newDefmodule->importList = nextSpec;
        }

      while (newDefmodule->exportList != NULL)
        {
         nextSpec = newDefmodule->exportList->next;
         rtn_struct(portItem,newDefmodule->exportList);
         newDefmodule->exportList = nextSpec;
        }

      if ((redefiningMainModule == NULL) && (! overwrite))
        { rtn_struct(defmodule,newDefmodule); }

      if (overwrite)
        {
         newDefmodule->importList = oldImportList;
         newDefmodule->exportList = oldExportList;
        }

      if (parseError) return(TRUE);
      return(FALSE);
     }

   /*===============================================*/
   /* Increment the symbol table counts for symbols */
   /* used in the defmodule data structures.        */
   /*===============================================*/

   if (redefiningMainModule == NULL)
     { IncrementSymbolCount(newDefmodule->name); }
   else
     {
      if ((newDefmodule->importList != NULL) ||
          (newDefmodule->exportList != NULL))
        { MainModuleRedefinable = FALSE; }
     }

   for (portSpecs = newDefmodule->importList; portSpecs != NULL; portSpecs = portSpecs->next)
     {
      if (portSpecs->moduleName != NULL) IncrementSymbolCount(portSpecs->moduleName);
      if (portSpecs->constructType != NULL) IncrementSymbolCount(portSpecs->constructType);
      if (portSpecs->constructName != NULL) IncrementSymbolCount(portSpecs->constructName);
     }

   for (portSpecs = newDefmodule->exportList; portSpecs != NULL; portSpecs = portSpecs->next)
     {
      if (portSpecs->moduleName != NULL) IncrementSymbolCount(portSpecs->moduleName);
      if (portSpecs->constructType != NULL) IncrementSymbolCount(portSpecs->constructType);
      if (portSpecs->constructName != NULL) IncrementSymbolCount(portSpecs->constructName);
     }

   /*====================================================*/
   /* Allocate storage for the module's construct lists. */
   /*====================================================*/

   if (redefiningMainModule != NULL) { /* Do nothing */ }
   else if (NumberOfModuleItems == 0) newDefmodule->itemsArray = NULL;
   else
     {
      newDefmodule->itemsArray = (struct defmoduleItemHeader **) gm2((int) sizeof(void *) * NumberOfModuleItems);
      for (i = 0, theItem = ListOfModuleItems;
           (i < NumberOfModuleItems) && (theItem != NULL);
           i++, theItem = theItem->next)
        {
         if (theItem->allocateFunction == NULL)
           { newDefmodule->itemsArray[i] = NULL; }
         else
           {
            newDefmodule->itemsArray[i] = (struct defmoduleItemHeader *)
                                          (*theItem->allocateFunction)();
            theHeader = (struct defmoduleItemHeader *) newDefmodule->itemsArray[i];
            theHeader->theModule = newDefmodule;
            theHeader->firstItem = NULL;
            theHeader->lastItem = NULL;
           }
        }
     }

   /*=======================================*/
   /* Save the pretty print representation. */
   /*=======================================*/

   SavePPBuffer("\n");

   if (GetConserveMemory() == TRUE)
     { newDefmodule->ppForm = NULL; }
   else
     { newDefmodule->ppForm = CopyPPBuffer(); }

   /*==============================================*/
   /* Add the defmodule to the list of defmodules. */
   /*==============================================*/

   if (redefiningMainModule == NULL)
     {
      if (LastDefmodule == NULL) ListOfDefmodules = newDefmodule;
      else LastDefmodule->next = newDefmodule;
      LastDefmodule = newDefmodule;
      newDefmodule->bsaveID = NumberOfDefmodules++;
     }

   SetCurrentModule((void *) newDefmodule);

   /*=========================================*/
   /* Call any functions required by other    */
   /* constructs when a new module is defined */
   /*=========================================*/

   for (defineFunctions = AfterModuleDefinedFunctions;
        defineFunctions != NULL;
        defineFunctions = defineFunctions->next)
     { (* (void (*)(void)) defineFunctions->func)(); }

   /*===============================================*/
   /* Defmodule successfully parsed with no errors. */
   /*===============================================*/

   return(FALSE);
  }

/*************************************************************/
/* DeleteDefmodule: Used by the parsing routine to determine */
/*   if a module can be redefined. Only the MAIN module can  */
/*   be redefined (and it can only be redefined once).       */
/*************************************************************/
static BOOLEAN DeleteDefmodule(
  void *theConstruct)
  {
   if (strcmp(GetDefmoduleName(theConstruct),"MAIN") == 0)
     { return(MainModuleRedefinable); }

   return(FALSE);
  }

/*********************************************************/
/* ParsePortSpecifications: Parses the import and export */
/*   specifications found in a defmodule construct.      */
/*********************************************************/
static int ParsePortSpecifications(
  char *readSource,
  struct token *theToken,
  struct defmodule *theDefmodule)
  {
   int error;

   /*=============================*/
   /* The import and export lists */
   /* are initially empty.        */
   /*=============================*/

   theDefmodule->importList = NULL;
   theDefmodule->exportList = NULL;

   /*==========================================*/
   /* Parse import/export specifications until */
   /* a right parenthesis is encountered.      */
   /*==========================================*/

   while (theToken->type != RPAREN)
     {
      /*========================================*/
      /* Look for the opening left parenthesis. */
      /*========================================*/

      if (theToken->type != LPAREN)
        {
         SyntaxErrorMessage("defmodule");
         return(TRUE);
        }

      /*====================================*/
      /* Look for the import/export keyword */
      /* and call the appropriate functions */
      /* for parsing the specification.     */
      /*====================================*/

      GetToken(readSource,theToken);

      if (theToken->type != SYMBOL)
        {
         SyntaxErrorMessage("defmodule");
         return(TRUE);
        }

      if (strcmp(ValueToString(theToken->value),"import") == 0)
        {
         error = ParseImportSpec(readSource,theToken,theDefmodule);
        }
      else if (strcmp(ValueToString(theToken->value),"export") == 0)
        {
         error = ParseExportSpec(readSource,theToken,theDefmodule,NULL);
        }
      else
        {
         SyntaxErrorMessage("defmodule");
         return(TRUE);
        }

      if (error) return(TRUE);

      /*============================================*/
      /* Begin parsing the next port specification. */
      /*============================================*/

      PPCRAndIndent();
      GetToken(readSource,theToken);

      if (theToken->type == RPAREN)
        {
         PPBackup();
         PPBackup();
         SavePPBuffer(")");
        }
     }

   /*===================================*/
   /* Return FALSE to indicate no error */
   /* occurred while parsing the        */
   /* import/export specifications.     */
   /*===================================*/

   return(FALSE);
  }

/**********************************************************/
/* ParseImportSpec: Parses import specifications found in */
/*   a defmodule construct.                               */
/*                                                        */
/* <import-spec> ::= (import <module-name> <port-item>)   */
/*                                                        */
/* <port-item>   ::= ?ALL |                               */
/*                   ?NONE |                              */
/*                   <construct-name> ?ALL |              */
/*                   <construct-name> ?NONE |             */
/*                   <construct-name> <names>*            */
/**********************************************************/
static int ParseImportSpec(
  char *readSource,
  struct token *theToken,
  struct defmodule *newModule)
  {
   struct defmodule *theModule;
   struct portItem *thePort, *oldImportSpec;
   int found, count;

   /*===========================*/
   /* Look for the module name. */
   /*===========================*/

   SavePPBuffer(" ");

   GetToken(readSource,theToken);

   if (theToken->type != SYMBOL)
     {
      SyntaxErrorMessage("defmodule import specification");
      return(TRUE);
     }

   /*=====================================*/
   /* Verify the existence of the module. */
   /*=====================================*/

   if ((theModule = (struct defmodule *)
                    FindDefmodule(ValueToString(theToken->value))) == NULL)
     {
      CantFindItemErrorMessage("defmodule",ValueToString(theToken->value));
      return(TRUE);
     }

   /*========================================*/
   /* If the specified module doesn't export */
   /* any constructs, then the import        */
   /* specification is meaningless.          */
   /*========================================*/

   if (theModule->exportList == NULL)
     {
      NotExportedErrorMessage(GetDefmoduleName(theModule),NULL,NULL);
      return(TRUE);
     }

   /*==============================================*/
   /* Parse the remaining portion of the import    */
   /* specification and return if an error occurs. */
   /*==============================================*/

   oldImportSpec = newModule->importList;
   if (ParseExportSpec(readSource,theToken,newModule,theModule)) return(TRUE);

   /*========================================================*/
   /* If the ?NONE keyword was used with the import spec,    */
   /* then no constructs were actually imported and the      */
   /* import spec does not need to be checked for conflicts. */
   /*========================================================*/

   if (newModule->importList == oldImportSpec) return(FALSE);

   /*======================================================*/
   /* Check to see if the construct being imported can be  */
   /* by the specified module. This check exported doesn't */
   /* guarantee that a specific named construct actually   */
   /* exists. It just checks that it could be exported if  */
   /* it does exists.                                      */
   /*======================================================*/

   if (newModule->importList->constructType != NULL)
     {
      /*=============================*/
      /* Look for the construct in   */
      /* the module that exports it. */
      /*=============================*/

      found = FALSE;
      for (thePort = theModule->exportList;
           (thePort != NULL) && (! found);
           thePort = thePort->next)
        {
         if (thePort->constructType == NULL) found = TRUE;
         else if (thePort->constructType == newModule->importList->constructType)
           {
            if (newModule->importList->constructName == NULL) found = TRUE;
            else if (thePort->constructName == NULL) found = TRUE;
            else if (thePort->constructName == newModule->importList->constructName)
              { found = TRUE; }
           }
        }

      /*=======================================*/
      /* If it's not exported by the specified */
      /* module, print an error message.       */
      /*=======================================*/

      if (! found)
        {
         if (newModule->importList->constructName == NULL)
           {
            NotExportedErrorMessage(GetDefmoduleName(theModule),
                                    ValueToString(newModule->importList->constructType),
                                    NULL);
           }
         else
           {
            NotExportedErrorMessage(GetDefmoduleName(theModule),
                                    ValueToString(newModule->importList->constructType),
                                    ValueToString(newModule->importList->constructName));
           }
         return(TRUE);
        }
     }

   /*======================================================*/
   /* Verify that specific named constructs actually exist */
   /* and can be seen from the module importing them.      */
   /*======================================================*/

   SaveCurrentModule();
   SetCurrentModule((void *) newModule);

   for (thePort = newModule->importList;
        thePort != NULL;
        thePort = thePort->next)
     {
      if ((thePort->constructType == NULL) || (thePort->constructName == NULL))
        { continue; }

      theModule = (struct defmodule *)
                  FindDefmodule(ValueToString(thePort->moduleName));
      SetCurrentModule(theModule);
      if (FindImportedConstruct(ValueToString(thePort->constructType),NULL,
                                ValueToString(thePort->constructName),&count,
                                TRUE,FALSE) == NULL)
        {
         NotExportedErrorMessage(GetDefmoduleName(theModule),
                                 ValueToString(thePort->constructType),
                                 ValueToString(thePort->constructName));
         RestoreCurrentModule();
         return(TRUE);
        }
     }

   RestoreCurrentModule();

   /*===============================================*/
   /* The import list has been successfully parsed. */
   /*===============================================*/

   return(FALSE);
  }

/**********************************************************/
/* ParseExportSpec: Parses export specifications found in */
/*   a defmodule construct. This includes parsing the     */
/*   remaining specification found in an import           */
/*   specification after the module name.                 */
/**********************************************************/
static int ParseExportSpec(
  char *readSource,
  struct token *theToken,
  struct defmodule *newModule,
  struct defmodule *importModule)
  {
   struct portItem *newPort;
   SYMBOL_HN *theConstruct, *moduleName;
   struct portConstructItem *thePortConstruct;
   char *errorMessage;

   /*===========================================*/
   /* Set up some variables for error messages. */
   /*===========================================*/

   if (importModule != NULL)
     {
      errorMessage = "defmodule import specification";
      moduleName = importModule->name;
     }
   else
     {
      errorMessage = "defmodule export specification";
      moduleName = NULL;
     }

   /*=============================================*/
   /* Handle the special variables ?ALL and ?NONE */
   /* in the import/export specification.         */
   /*=============================================*/

   SavePPBuffer(" ");
   GetToken(readSource,theToken);

   if (theToken->type == SF_VARIABLE)
     {
      /*==============================*/
      /* Check to see if the variable */
      /* is either ?ALL or ?NONE.     */
      /*==============================*/

      if (strcmp(ValueToString(theToken->value),"ALL") == 0)
        {
         newPort = (struct portItem *) get_struct(portItem);
         newPort->moduleName = moduleName;
         newPort->constructType = NULL;
         newPort->constructName = NULL;
         newPort->next = NULL;
        }
      else if (strcmp(ValueToString(theToken->value),"NONE") == 0)
        { newPort = NULL; }
      else
        {
         SyntaxErrorMessage(errorMessage);
         return(TRUE);
        }

      /*=======================================================*/
      /* The export/import specification must end with a right */
      /* parenthesis after ?ALL or ?NONE at this point.        */
      /*=======================================================*/

      GetToken(readSource,theToken);

      if (theToken->type != RPAREN)
        {
         if (newPort != NULL) rtn_struct(portItem,newPort);
         PPBackup();
         SavePPBuffer(" ");
         SavePPBuffer(theToken->printForm);
         SyntaxErrorMessage(errorMessage);
         return(TRUE);
        }

      /*=====================================*/
      /* Add the new specification to either */
      /* the import or export list.          */
      /*=====================================*/

      if (newPort != NULL)
        {
         if (importModule != NULL)
           {
            newPort->next = newModule->importList;
            newModule->importList = newPort;
           }
         else
           {
            newPort->next = newModule->exportList;
            newModule->exportList = newPort;
           }
        }

      /*============================================*/
      /* Return FALSE to indicate the import/export */
      /* specification was successfully parsed.     */
      /*============================================*/

      return(FALSE);
     }

   /*========================================================*/
   /* If the ?ALL and ?NONE keywords were not used, then the */
   /* token must be the name of an importable construct.     */
   /*========================================================*/

   if (theToken->type != SYMBOL)
     {
      SyntaxErrorMessage(errorMessage);
      return(TRUE);
     }

   theConstruct = (SYMBOL_HN *) theToken->value;

   if ((thePortConstruct = ValidPortConstructItem(ValueToString(theConstruct))) == NULL)
     {
      SyntaxErrorMessage(errorMessage);
      return(TRUE);
     }

   /*=============================================================*/
   /* If the next token is the special variable ?ALL, then all    */
   /* constructs of the specified type are imported/exported. If  */
   /* the next token is the special variable ?NONE, then no       */
   /* constructs of the specified type will be imported/exported. */
   /*=============================================================*/

   SavePPBuffer(" ");
   GetToken(readSource,theToken);

   if (theToken->type == SF_VARIABLE)
     {
      /*==============================*/
      /* Check to see if the variable */
      /* is either ?ALL or ?NONE.     */
      /*==============================*/

      if (strcmp(ValueToString(theToken->value),"ALL") == 0)
        {
         newPort = (struct portItem *) get_struct(portItem);
         newPort->moduleName = moduleName;
         newPort->constructType = theConstruct;
         newPort->constructName = NULL;
         newPort->next = NULL;
        }
      else if (strcmp(ValueToString(theToken->value),"NONE") == 0)
        { newPort = NULL; }
      else
        {
         SyntaxErrorMessage(errorMessage);
         return(TRUE);
        }

      /*=======================================================*/
      /* The export/import specification must end with a right */
      /* parenthesis after ?ALL or ?NONE at this point.        */
      /*=======================================================*/

      GetToken(readSource,theToken);

      if (theToken->type != RPAREN)
        {
         if (newPort != NULL) rtn_struct(portItem,newPort);
         PPBackup();
         SavePPBuffer(" ");
         SavePPBuffer(theToken->printForm);
         SyntaxErrorMessage(errorMessage);
         return(TRUE);
        }

      /*=====================================*/
      /* Add the new specification to either */
      /* the import or export list.          */
      /*=====================================*/

      if (newPort != NULL)
        {
         if (importModule != NULL)
           {
            newPort->next = newModule->importList;
            newModule->importList = newPort;
           }
         else
           {
            newPort->next = newModule->exportList;
            newModule->exportList = newPort;
           }
        }

      /*============================================*/
      /* Return FALSE to indicate the import/export */
      /* specification was successfully parsed.     */
      /*============================================*/

      return(FALSE);
     }

   /*============================================*/
   /* There must be at least one named construct */
   /* in the import/export list at this point.   */
   /*============================================*/

   if (theToken->type == RPAREN)
     {
      SyntaxErrorMessage(errorMessage);
      return(TRUE);
     }

   /*=====================================*/
   /* Read in the list of imported items. */
   /*=====================================*/

   while (theToken->type != RPAREN)
     {
      if (theToken->type != thePortConstruct->typeExpected)
        {
         SyntaxErrorMessage(errorMessage);
         return(TRUE);
        }

      /*========================================*/
      /* Create the data structure to represent */
      /* the import/export specification for    */
      /* the named construct.                   */
      /*========================================*/

      newPort = (struct portItem *) get_struct(portItem);
      newPort->moduleName = moduleName;
      newPort->constructType = theConstruct;
      newPort->constructName = (SYMBOL_HN *) theToken->value;

      /*=====================================*/
      /* Add the new specification to either */
      /* the import or export list.          */
      /*=====================================*/

      if (importModule != NULL)
        {
         newPort->next = newModule->importList;
         newModule->importList = newPort;
        }
      else
        {
         newPort->next = newModule->exportList;
         newModule->exportList = newPort;
        }

      /*===================================*/
      /* Move on to the next import/export */
      /* specification.                    */
      /*===================================*/

      SavePPBuffer(" ");
      GetToken(readSource,theToken);
     }

   /*=============================*/
   /* Fix up pretty print buffer. */
   /*=============================*/

   PPBackup();
   PPBackup();
   SavePPBuffer(")");

   /*============================================*/
   /* Return FALSE to indicate the import/export */
   /* specification was successfully parsed.     */
   /*============================================*/

   return(FALSE);
  }

/*************************************************************/
/* ValidPortConstructItem: Returns TRUE if a given construct */
/*   name is in the list of constructs which can be exported */
/*   and imported, otherwise FALSE is returned.              */
/*************************************************************/
globle struct portConstructItem *ValidPortConstructItem(
  char *theName)
  {
   struct portConstructItem *theItem;

   for (theItem = ListOfPortConstructItems;
        theItem != NULL;
        theItem = theItem->next)
     { if (strcmp(theName,theItem->constructName) == 0) return(theItem); }

   return(NULL);
  }

/***********************************************************/
/* FindMultiImportConflict: Determines if a module imports */
/*   the same named construct from more than one module    */
/*   (i.e. an ambiguous reference which is not allowed).   */
/***********************************************************/
static int FindMultiImportConflict(
  struct defmodule *theModule)
  {
   struct defmodule *testModule;
   int count;
   struct portConstructItem *thePCItem;
   struct construct *theConstruct;
   void *theCItem;

   /*==========================*/
   /* Save the current module. */
   /*==========================*/

   SaveCurrentModule();

   /*============================*/
   /* Loop through every module. */
   /*============================*/

   for (testModule = (struct defmodule *) GetNextDefmodule(NULL);
        testModule != NULL;
        testModule = (struct defmodule *) GetNextDefmodule(testModule))
     {
      /*========================================*/
      /* Loop through every construct type that */
      /* can be imported/exported by a module.  */
      /*========================================*/

      for (thePCItem = ListOfPortConstructItems;
           thePCItem != NULL;
           thePCItem = thePCItem->next)
        {
         SetCurrentModule((void *) testModule);

         /*=====================================================*/
         /* Loop through every construct of the specified type. */
         /*=====================================================*/

         theConstruct = FindConstruct(thePCItem->constructName);

         for (theCItem = (*theConstruct->getNextItemFunction)(NULL);
              theCItem != NULL;
              theCItem = (*theConstruct->getNextItemFunction)(theCItem))
            {
             /*===============================================*/
             /* Check to see if the specific construct in the */
             /* module can be imported with more than one     */
             /* reference into the module we're examining for */
             /* ambiguous import  specifications.             */
             /*===============================================*/

             SetCurrentModule((void *) theModule);
             FindImportedConstruct(thePCItem->constructName,NULL,
                                   ValueToString((*theConstruct->getConstructNameFunction)
                                                 ((struct constructHeader *) theCItem)),
                                   &count,FALSE,NULL);
             if (count > 1)
               {
                ImportExportConflictMessage("defmodule",GetDefmoduleName(theModule),
                                            thePCItem->constructName,
                                            ValueToString((*theConstruct->getConstructNameFunction)
                                                          ((struct constructHeader *) theCItem)));
                RestoreCurrentModule();
                return(TRUE);
               }

             SetCurrentModule((void *) testModule);
            }
        }
     }

   /*=============================*/
   /* Restore the current module. */
   /*=============================*/

   RestoreCurrentModule();

   /*=======================================*/
   /* Return FALSE to indicate no ambiguous */
   /* references were found.                */
   /*=======================================*/

   return(FALSE);
  }

/******************************************************/
/* NotExportedErrorMessage: Generalized error message */
/*  for indicating that a construct type or specific  */
/*  named construct is not exported.                  */
/******************************************************/
static void NotExportedErrorMessage(
  char *theModule,
  char *theConstruct,
  char *theName)
  {
   PrintErrorID("MODULPSR",1,TRUE);
   PrintRouter(WERROR,"Module ");
   PrintRouter(WERROR,theModule);
   PrintRouter(WERROR," does not export ");

   if (theConstruct == NULL) PrintRouter(WERROR,"any constructs");
   else if (theName == NULL)
     {
      PrintRouter(WERROR,"any ");
      PrintRouter(WERROR,theConstruct);
      PrintRouter(WERROR," constructs");
     }
   else
     {
      PrintRouter(WERROR,"the ");
      PrintRouter(WERROR,theConstruct);
      PrintRouter(WERROR," ");
      PrintRouter(WERROR,theName);
     }

   PrintRouter(WERROR,".\n");
  }

/*************************************************************/
/* FindImportExportConflict: Determines if the definition of */
/*   a construct would cause an import/export conflict. The  */
/*   construct is not yet defined when this function is      */
/*   called. TRUE is returned if an import/export conflicts  */
/*   is found, otherwise FALSE is returned.                  */
/*************************************************************/
globle int FindImportExportConflict(
  char *constructName,
  struct defmodule *matchModule,
  char *findName)
  {
   struct defmodule *theModule;
   struct moduleItem *theModuleItem;
   int count;

   /*===========================================================*/
   /* If the construct type can't be imported or exported, then */
   /* it's not possible to have an import/export conflict.      */
   /*===========================================================*/

   if (ValidPortConstructItem(constructName) == NULL) return(FALSE);

   /*============================================*/
   /* There module name should already have been */
   /* separated fromthe construct's name.        */
   /*============================================*/

   if (FindModuleSeparator(findName)) return(FALSE);

   /*===============================================================*/
   /* The construct must be capable of being stored within a module */
   /* (this test should never fail). The construct must also have   */
   /* a find function associated with it so we can actually look    */
   /* for import/export conflicts.                                  */
   /*===============================================================*/

   if ((theModuleItem = FindModuleItem(constructName)) == NULL) return(FALSE);

   if (theModuleItem->findFunction == NULL) return(FALSE);

   /*==========================*/
   /* Save the current module. */
   /*==========================*/

   SaveCurrentModule();

   /*================================================================*/
   /* Look at each module and count each definition of the specified */
   /* construct which is visible to the module. If more than one     */
   /* definition is visible, then an import/export conflict exists   */
   /* and TRUE is returned.                                          */
   /*================================================================*/

   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      SetCurrentModule((void *) theModule);

      FindImportedConstruct(constructName,NULL,findName,&count,TRUE,matchModule);
      if (count > 1)
        {
         RestoreCurrentModule();
         return(TRUE);
        }
     }

   /*==========================================*/
   /* Restore the current module. No conflicts */
   /* were detected so FALSE is returned.      */
   /*==========================================*/

   RestoreCurrentModule();
   return(FALSE);
  }

#endif /* DEFMODULE_CONSTRUCT && (! RUN_TIME) && (! BLOAD_ONLY) */



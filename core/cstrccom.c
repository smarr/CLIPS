   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98             */
   /*                                                     */
   /*              CONSTRUCT COMMANDS MODULE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains generic routines for deleting, pretty   */
/*   printing, finding, obtaining module information,        */
/*   obtaining lists of constructs, listing constructs, and  */
/*   manipulation routines.                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _CSTRCCOM_SOURCE_

#include <string.h>

#include "setup.h"

#include "constant.h"
#include "memalloc.h"
#include "moduldef.h"
#include "argacces.h"
#include "multifld.h"
#include "modulutl.h"
#include "router.h"
#include "utility.h"
#include "commline.h"

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
#include "bload.h"
#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)
#include "cstrcpsr.h"
#endif

#include "cstrccom.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if DEBUGGING_FUNCTIONS
   static void                    ConstructPrintWatch(char *,struct construct *,void *,
                                                      BOOLEAN (*)(void *));
   static BOOLEAN                 ConstructWatchSupport(struct construct *,char *,
                                                        char *,EXPRESSION *,BOOLEAN,
                                                        BOOLEAN,BOOLEAN (*)(void *),
                                                        void (*)(BOOLEAN,void *));
#endif

#if (! RUN_TIME)

/************************************/
/* AddConstructToModule: Adds a     */
/* construct to the current module. */
/************************************/
globle void AddConstructToModule(
  struct constructHeader *theConstruct)
  {
   if (theConstruct->whichModule->lastItem == NULL)
     { theConstruct->whichModule->firstItem = theConstruct; }
   else
     { theConstruct->whichModule->lastItem->next = theConstruct; }

   theConstruct->whichModule->lastItem = theConstruct;
   theConstruct->next = NULL;
  }

#endif /* (! RUN_TIME) */

/****************************************************/
/* DeleteNamedConstruct: Generic driver routine for */
/*   deleting a specific construct from a module.   */
/****************************************************/
globle BOOLEAN DeleteNamedConstruct(
  char *constructName,
  struct construct *constructClass)
  {
#if (! BLOAD_ONLY)
   void *constructPtr;

   /*=============================*/
   /* Constructs can't be deleted */
   /* while a bload is in effect. */
   /*=============================*/

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
   if (Bloaded() == TRUE) return(FALSE);
#endif

   /*===============================*/
   /* Look for the named construct. */
   /*===============================*/

   constructPtr = (*constructClass->findFunction)(constructName);

   /*========================================*/
   /* If the construct was found, delete it. */
   /*========================================*/

   if (constructPtr != NULL)
     { return((*constructClass->deleteFunction)(constructPtr)); }

   /*========================================*/
   /* If the construct wasn't found, but the */
   /* special symbol * was used, then delete */
   /* all constructs of the specified type.  */
   /*========================================*/

   if (strcmp("*",constructName) == 0)
     {
      (*constructClass->deleteFunction)(NULL);
      return(TRUE);
     }

   /*===============================*/
   /* Otherwise, return FALSE to    */
   /* indicate no deletion occured. */
   /*===============================*/

   return(FALSE);
#else
   return(FALSE);
#endif
  }

/*******************************************/
/* FindNamedConstruct: Generic routine for */
/*   searching for a specified construct.  */
/*******************************************/
globle void *FindNamedConstruct(
  char *constructName,
  struct construct *constructClass)
  {
   void *theConstruct;
   SYMBOL_HN *findValue;

   /*==========================*/
   /* Save the current module. */
   /*==========================*/

   SaveCurrentModule();

   /*=========================================================*/
   /* Extract the construct name. If a module was specified,  */
   /* then ExtractModuleAndConstructName will set the current */
   /* module to the module specified in the name.             */
   /*=========================================================*/

   constructName = ExtractModuleAndConstructName(constructName);

   /*=================================================*/
   /* If a valid construct name couldn't be extracted */
   /* or the construct name isn't in the symbol table */
   /* (which means the construct doesn't exist), then */
   /* return NULL to indicate the specified construct */
   /* couldn't be found.                              */
   /*=================================================*/

   if ((constructName == NULL) ?
       TRUE :
       ((findValue = (SYMBOL_HN *) FindSymbol(constructName)) == NULL))
     {
      RestoreCurrentModule();
      return(NULL);
     }

   /*===============================================*/
   /* Loop through every construct of the specified */
   /* class in the current module checking to see   */
   /* if the construct's name matches the construct */
   /* being sought. If found, restore the current   */
   /* module and return a pointer to the construct. */
   /*===============================================*/

   for (theConstruct = (*constructClass->getNextItemFunction)(NULL);
        theConstruct != NULL;
        theConstruct = (*constructClass->getNextItemFunction)(theConstruct))
     {
      if (findValue == (*constructClass->getConstructNameFunction)((struct constructHeader *) theConstruct))
        {
         RestoreCurrentModule();
         return (theConstruct);
        }
     }

   /*=============================*/
   /* Restore the current module. */
   /*=============================*/
   RestoreCurrentModule();

   /*====================================*/
   /* Return NULL to indicated the named */
   /* construct was not found.           */
   /*====================================*/

   return(NULL);
  }

/*****************************************/
/* UndefconstructCommand: Driver routine */
/*   for the undef<construct> commands.  */
/*****************************************/
globle void UndefconstructCommand(
  char *command,
  struct construct *constructClass)
  {
   char *constructName;
   char buffer[80];

   /*==============================================*/
   /* Get the name of the construct to be deleted. */
   /*==============================================*/

   sprintf(buffer,"%s name",constructClass->constructName);

   constructName = GetConstructName(command,buffer);
   if (constructName == NULL) return;

#if (! RUN_TIME) && (! BLOAD_ONLY)

   /*=============================================*/
   /* Check to see if the named construct exists. */
   /*=============================================*/

   if (((*constructClass->findFunction)(constructName) == FALSE) &&
       (strcmp("*",constructName) != 0))
     {
      CantFindItemErrorMessage(constructClass->constructName,constructName);
      return;
     }

   /*===============================================*/
   /* If the construct does exist, try deleting it. */
   /*===============================================*/

   else if (DeleteNamedConstruct(constructName,constructClass) == FALSE)
     {
      CantDeleteItemErrorMessage(constructClass->constructName,constructName);
      return;
     }

   return;
#else
   /*=====================================*/
   /* Constructs can't be deleted in a    */
   /* run-time or bload only environment. */
   /*=====================================*/

   CantDeleteItemErrorMessage(constructClass->constructName,constructName);
   return;
#endif
  }

/******************************************/
/* PPConstructCommand: Driver routine for */
/*   the ppdef<construct> commands.       */
/******************************************/
globle void PPConstructCommand(
  char *command,
  struct construct *constructClass)
  {
   char *constructName;
   char buffer[80];

   /*===============================*/
   /* Get the name of the construct */
   /* to be "pretty printed."       */
   /*===============================*/

   sprintf(buffer,"%s name",constructClass->constructName);

   constructName = GetConstructName(command,buffer);
   if (constructName == NULL) return;

   /*================================*/
   /* Call the driver routine for    */
   /* pretty printing the construct. */
   /*================================*/

   if (PPConstruct(constructName,WDISPLAY,constructClass) == FALSE)
     { CantFindItemErrorMessage(constructClass->constructName,constructName); }
  }

/***********************************/
/* PPConstruct: Driver routine for */
/*   pretty printing a construct.  */
/***********************************/
globle int PPConstruct(
  char *constructName,
  char *logicalName,
  struct construct *constructClass)
  {
   void *constructPtr;

   /*==================================*/
   /* Use the construct's name to find */
   /* a pointer to actual construct.   */
   /*==================================*/

   constructPtr = (*constructClass->findFunction)(constructName);
   if (constructPtr == NULL) return(FALSE);

   /*==============================================*/
   /* If the pretty print form is NULL (because of */
   /* conserve-mem), return TRUE (which indicates  */
   /* the construct was found).                    */
   /*==============================================*/

   if ((*constructClass->getPPFormFunction)((struct constructHeader *) constructPtr) == NULL)
     { return(TRUE); }

   /*============================================*/
   /* Print the pretty print string in smaller   */
   /* chunks. (VMS had a bug that didn't allow   */
   /* printing a string greater than 512 bytes.) */
   /*============================================*/

   PrintInChunks(logicalName,(*constructClass->getPPFormFunction)((struct constructHeader *) constructPtr));

   /*=======================================*/
   /* Return TRUE to indicate the construct */
   /* was found and pretty printed.         */
   /*=======================================*/

   return(TRUE);
  }

/*********************************************/
/* GetConstructModuleCommand: Driver routine */
/*   for def<construct>-module routines      */
/*********************************************/
globle SYMBOL_HN *GetConstructModuleCommand(
  char *command,
  struct construct *constructClass)
  {
   char *constructName;
   char buffer[80];
   struct defmodule *constructModule;

   /*=========================================*/
   /* Get the name of the construct for which */
   /* we want to determine its module.        */
   /*=========================================*/

   sprintf(buffer,"%s name",constructClass->constructName);

   constructName = GetConstructName(command,buffer);
   if (constructName == NULL) return((SYMBOL_HN *) FalseSymbol);

   /*==========================================*/
   /* Get a pointer to the construct's module. */
   /*==========================================*/

   constructModule = GetConstructModule(constructName,constructClass);
   if (constructModule == NULL)
     {
      CantFindItemErrorMessage(constructClass->constructName,constructName);
      return((SYMBOL_HN *) FalseSymbol);
     }

   /*============================================*/
   /* Return the name of the construct's module. */
   /*============================================*/

   return(constructModule->name);
  }

/******************************************/
/* GetConstructModule: Driver routine for */
/*   getting the module for a construct   */
/******************************************/
globle struct defmodule *GetConstructModule(
  char *constructName,
  struct construct *constructClass)
  {
   struct constructHeader *constructPtr;
   int count, position;
   SYMBOL_HN *theName;

   /*====================================================*/
   /* If the construct name contains a module specifier, */
   /* then get a pointer to the defmodule associated     */
   /* with the specified name.                           */
   /*====================================================*/

   if ((position = FindModuleSeparator(constructName)) != FALSE)
     {
      theName = ExtractModuleName(position,constructName);
      if (theName != NULL)
        { return((struct defmodule *) FindDefmodule(ValueToString(theName))); }
     }

   /*============================================*/
   /* No module was specified, so search for the */
   /* named construct in the current module and  */
   /* modules from which it imports.             */
   /*============================================*/

   constructPtr = (struct constructHeader *)
                  FindImportedConstruct(constructClass->constructName,NULL,constructName,
                                        &count,TRUE,NULL);
   if (constructPtr == NULL) return(NULL);

   return(constructPtr->whichModule->theModule);
  }

/*************************************/
/* Undefconstruct: Generic C routine */
/*   for deleting a construct.       */
/*************************************/
globle BOOLEAN Undefconstruct(
  void *theConstruct,
  struct construct *constructClass)
  {
#if BLOAD_ONLY || RUN_TIME
#if MAC_MPW || MAC_MCW
#pragma unused(theConstruct)
#pragma unused(constructClass)
#endif
   return(FALSE);
#else
   void *currentConstruct,*nextConstruct;
   BOOLEAN success;

   /*================================================*/
   /* Delete all constructs of the specified type if */
   /* the construct pointer is the NULL pointer.     */
   /*================================================*/

   if (theConstruct == NULL)
     {
      success = TRUE;

      /*===================================================*/
      /* Loop through all of the constructs in the module. */
      /*===================================================*/

      currentConstruct = (*constructClass->getNextItemFunction)(NULL);
      while (currentConstruct != NULL)
        {
         /*==============================*/
         /* Remember the next construct. */
         /*==============================*/

         nextConstruct = (*constructClass->getNextItemFunction)(currentConstruct);

         /*=============================*/
         /* Try deleting the construct. */
         /*=============================*/

         if ((*constructClass->isConstructDeletableFunction)(currentConstruct))
           {
            RemoveConstructFromModule((struct constructHeader *) currentConstruct);
            (*constructClass->freeFunction)(currentConstruct);
           }
         else
           {
            CantDeleteItemErrorMessage(constructClass->constructName,
                        ValueToString((*constructClass->getConstructNameFunction)((struct constructHeader *) currentConstruct)));
            success = FALSE;
           }

         /*================================*/
         /* Move on to the next construct. */
         /*================================*/

         currentConstruct = nextConstruct;
        }

      /*=======================================*/
      /* Perform periodic cleanup if embedded. */
      /*=======================================*/

      if ((CurrentEvaluationDepth == 0) && (! EvaluatingTopLevelCommand) &&
          (CurrentExpression == NULL))
        { PeriodicCleanup(TRUE,FALSE); }

      /*============================================*/
      /* Return TRUE if all constructs successfully */
      /* deleted, otherwise FALSE.                  */
      /*============================================*/

      return(success);
     }

   /*==================================================*/
   /* Return FALSE if the construct cannot be deleted. */
   /*==================================================*/

   if ((*constructClass->isConstructDeletableFunction)(theConstruct) == FALSE)
     { return(FALSE); }

   /*===========================*/
   /* Remove the construct from */
   /* the list in its module.   */
   /*===========================*/

   RemoveConstructFromModule((struct constructHeader *) theConstruct);

   /*=======================*/
   /* Delete the construct. */
   /*=======================*/

   (*constructClass->freeFunction)(theConstruct);

   /*=======================================*/
   /* Perform periodic cleanup if embedded. */
   /*=======================================*/

   if ((CurrentEvaluationDepth == 0) && (! EvaluatingTopLevelCommand) &&
       (CurrentExpression == NULL))
     { PeriodicCleanup(TRUE,FALSE); }

   /*=============================*/
   /* Return TRUE to indicate the */
   /* construct was deleted.      */
   /*=============================*/

   return(TRUE);
#endif
  }

/***********************************/
/* SaveConstruct: Generic routine  */
/*   for saving a construct class. */
/***********************************/
globle void SaveConstruct(
  char *logicalName,
  struct construct *constructClass)
  {
   struct defmodule *theModule;
   char *ppform;
   struct constructHeader *theConstruct;

   /*==========================*/
   /* Save the current module. */
   /*==========================*/

   SaveCurrentModule();

   /*===========================*/
   /* Loop through each module. */
   /*===========================*/

   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      /*===========================*/
      /* Set the current module to */
      /* the one we're examining.  */
      /*===========================*/

      SetCurrentModule((void *) theModule);

      /*==============================================*/
      /* Loop through each construct of the specified */
      /* construct class in the module.               */
      /*==============================================*/

      for (theConstruct = (struct constructHeader *)
                          (*constructClass->getNextItemFunction)(NULL);
           theConstruct != NULL;
           theConstruct = (struct constructHeader *)
                          (*constructClass->getNextItemFunction)(theConstruct))
        {
         /*==========================================*/
         /* Print the construct's pretty print form. */
         /*==========================================*/

         ppform = (*constructClass->getPPFormFunction)(theConstruct);
         if (ppform != NULL)
           {
            PrintInChunks(logicalName,ppform);
            PrintRouter(logicalName,"\n");
           }
        }
     }

   /*=============================*/
   /* Restore the current module. */
   /*=============================*/

   RestoreCurrentModule();
  }

/*********************************************************/
/* GetConstructModuleName: Generic routine for returning */
/*   the name of the module to which a construct belongs */
/*********************************************************/
globle char *GetConstructModuleName(
  struct constructHeader *theConstruct)
  { return(GetDefmoduleName((void *) theConstruct->whichModule->theModule)); }

/*********************************************************/
/* GetConstructNameString: Generic routine for returning */
/*   the name string of a construct.                     */
/*********************************************************/
globle char *GetConstructNameString(
  struct constructHeader *theConstruct)
  { return(ValueToString(theConstruct->name)); }

/**********************************************************/
/* GetConstructNamePointer: Generic routine for returning */
/*   the name pointer of a construct.                     */
/**********************************************************/
globle SYMBOL_HN *GetConstructNamePointer(
  struct constructHeader *theConstruct)
  { return(theConstruct->name); }

/************************************************/
/* GetConstructListFunction: Generic Routine    */
/*   for retrieving the constructs in a module. */
/************************************************/
globle void GetConstructListFunction(
  char *functionName,
  DATA_OBJECT_PTR returnValue,
  struct construct *constructClass)
  {
   struct defmodule *theModule;
   DATA_OBJECT result;
   int numArgs;

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/

   if ((numArgs = ArgCountCheck(functionName,NO_MORE_THAN,1)) == -1)
     {
      SetMultifieldErrorValue(returnValue);
      return;
     }

   /*====================================*/
   /* If an argument was given, check to */
   /* see that it's a valid module name. */
   /*====================================*/

   if (numArgs == 1)
     {
      /*======================================*/
      /* Only symbols are valid module names. */
      /*======================================*/

      RtnUnknown(1,&result);
      if (GetType(result) != SYMBOL)
        {
         SetMultifieldErrorValue(returnValue);
         ExpectedTypeError1(functionName,1,"defmodule name");
         return;
        }

      /*===========================================*/
      /* Verify that the named module exists or is */
      /* the symbol * (for obtaining the construct */
      /* list for all modules).                    */
      /*===========================================*/

      if ((theModule = (struct defmodule *) FindDefmodule(DOToString(result))) == NULL)
        {
         if (strcmp("*",DOToString(result)) != 0)
           {
            SetMultifieldErrorValue(returnValue);
            ExpectedTypeError1(functionName,1,"defmodule name");
            return;
           }

         theModule = NULL;
        }
     }

   /*=====================================*/
   /* Otherwise use the current module to */
   /* generate the construct list.        */
   /*=====================================*/

   else
     { theModule = ((struct defmodule *) GetCurrentModule()); }

   /*=============================*/
   /* Call the driver routine to  */
   /* get the list of constructs. */
   /*=============================*/

   GetConstructList(returnValue,constructClass,theModule);
  }

/********************************************/
/* GetConstructList: Generic C Routine for  */
/*   retrieving the constructs in a module. */
/********************************************/
globle void GetConstructList(
  DATA_OBJECT_PTR returnValue,
  struct construct *constructClass,
  struct defmodule *theModule)
  {
   void *theConstruct;
   long count = 0;
   struct multifield *theList;
   SYMBOL_HN *theName;
   struct defmodule *loopModule;
   int allModules = FALSE;

   /*==========================*/
   /* Save the current module. */
   /*==========================*/

   SaveCurrentModule();

   /*=======================================*/
   /* If the module specified is NULL, then */
   /* get all constructs in all modules.    */
   /*=======================================*/

   if (theModule == NULL)
     {
      theModule = (struct defmodule *) GetNextDefmodule(NULL);
      allModules = TRUE;
     }

   /*================================*/
   /* Count the number of constructs */
   /* to be retrieved.               */
   /*================================*/

   loopModule = theModule;
   while (loopModule != NULL)
     {
      SetCurrentModule((void *) loopModule);
      theConstruct = NULL;
      while ((theConstruct = (*constructClass->getNextItemFunction)(theConstruct)) != NULL)
        { count++; }

      if (allModules) loopModule = (struct defmodule *) GetNextDefmodule(loopModule);
      else loopModule = NULL;
     }

   /*================================*/
   /* Create the multifield value to */
   /* store the construct names.     */
   /*================================*/

   SetpType(returnValue,MULTIFIELD);
   SetpDOBegin(returnValue,1);
   SetpDOEnd(returnValue,count);
   theList = (struct multifield *) CreateMultifield((int) count);
   SetpValue(returnValue,(void *) theList);

   /*===========================*/
   /* Store the construct names */
   /* in the multifield value.  */
   /*===========================*/

   loopModule = theModule;
   count = 1;
   while (loopModule != NULL)
     {
      /*============================*/
      /* Set the current module to  */
      /* the module being examined. */
      /*============================*/

      SetCurrentModule((void *) loopModule);

      /*===============================*/
      /* Add each construct name found */
      /* in the module to the list.    */
      /*===============================*/

      theConstruct = NULL;
      while ((theConstruct = (*constructClass->getNextItemFunction)(theConstruct)) != NULL)
        {
         theName = (*constructClass->getConstructNameFunction)((struct constructHeader *) theConstruct);
         SetMFType(theList,count,SYMBOL);
         if (allModules)
           {
            char buffer[512];

            strcpy(buffer,GetDefmoduleName(loopModule));
            strcat(buffer,"::");
            strcat(buffer,ValueToString(theName));
            SetMFValue(theList,count,AddSymbol(buffer));
           }
         else
           { SetMFValue(theList,count,AddSymbol(ValueToString(theName))); }
         count++;
        }

      /*==================================*/
      /* Move on to the next module (if   */
      /* the list is to contain the names */
      /* of constructs from all modules). */
      /*==================================*/

      if (allModules) loopModule = (struct defmodule *) GetNextDefmodule(loopModule);
      else loopModule = NULL;
     }

   /*=============================*/
   /* Restore the current module. */
   /*=============================*/

   RestoreCurrentModule();
  }

/*********************************************/
/* ListConstructCommand: Generic Routine for */
/*   listing the constructs in a module.     */
/*********************************************/
globle void ListConstructCommand(
  char *functionName,
  struct construct *constructClass)
  {
   struct defmodule *theModule;
   DATA_OBJECT result;
   int numArgs;

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/

   if ((numArgs = ArgCountCheck(functionName,NO_MORE_THAN,1)) == -1) return;

   /*====================================*/
   /* If an argument was given, check to */
   /* see that it's a valid module name. */
   /*====================================*/

   if (numArgs == 1)
     {
      /*======================================*/
      /* Only symbols are valid module names. */
      /*======================================*/

      RtnUnknown(1,&result);
      if (GetType(result) != SYMBOL)
        {
         ExpectedTypeError1(functionName,1,"defmodule name");
         return;
        }

      /*===========================================*/
      /* Verify that the named module exists or is */
      /* the symbol * (for obtaining the construct */
      /* list for all modules).                    */
      /*===========================================*/

      if ((theModule = (struct defmodule *) FindDefmodule(DOToString(result))) == NULL)
        {
         if (strcmp("*",DOToString(result)) != 0)
           {
            ExpectedTypeError1(functionName,1,"defmodule name");
            return;
           }

         theModule = NULL;
        }
     }

   /*=====================================*/
   /* Otherwise use the current module to */
   /* generate the construct list.        */
   /*=====================================*/

   else
     { theModule = ((struct defmodule *) GetCurrentModule()); }

   /*=========================*/
   /* Call the driver routine */
   /* to list the constructs. */
   /*=========================*/

   ListConstruct(constructClass,WDISPLAY,theModule);
  }

/*****************************************/
/* ListConstruct: Generic C Routine for  */
/*   listing the constructs in a module. */
/*****************************************/
globle void ListConstruct(
  struct construct *constructClass,
  char *logicalName,
  struct defmodule *theModule)
  {
   void *constructPtr;
   SYMBOL_HN *constructName;
   long count = 0;
   int allModules = FALSE;

   /*==========================*/
   /* Save the current module. */
   /*==========================*/

   SaveCurrentModule();

   /*=======================================*/
   /* If the module specified is NULL, then */
   /* list all constructs in all modules.   */
   /*=======================================*/

   if (theModule == NULL)
     {
      theModule = (struct defmodule *) GetNextDefmodule(NULL);
      allModules = TRUE;
     }

   /*==================================*/
   /* Loop through all of the modules. */
   /*==================================*/

   while (theModule != NULL)
     {
      /*========================================*/
      /* If we're printing the construct in all */
      /* modules, then preface each module      */
      /* listing with the name of the module.   */
      /*========================================*/

      if (allModules)
        {
         PrintRouter(logicalName,GetDefmoduleName(theModule));
         PrintRouter(logicalName,":\n");
        }

      /*===============================*/
      /* Set the current module to the */
      /* module we're examining.       */
      /*===============================*/

      SetCurrentModule((void *) theModule);

      /*===========================================*/
      /* List all of the constructs in the module. */
      /*===========================================*/

      for (constructPtr = (*constructClass->getNextItemFunction)(NULL);
           constructPtr != NULL;
           constructPtr = (*constructClass->getNextItemFunction)(constructPtr))
        {
         if (HaltExecution == TRUE) return;

         constructName = (*constructClass->getConstructNameFunction)((struct constructHeader *) constructPtr);

         if (constructName != NULL)
           {
            if (allModules) PrintRouter(WDISPLAY,"   ");
            PrintRouter(logicalName,ValueToString(constructName));
            PrintRouter(logicalName,"\n");
           }

         count++;
        }

      /*====================================*/
      /* Move on to the next module (if the */
      /* listing is to contain the names of */
      /* constructs from all modules).      */
      /*====================================*/

      if (allModules) theModule = (struct defmodule *) GetNextDefmodule(theModule);
      else theModule = NULL;
     }

   /*=================================================*/
   /* Print the tally and restore the current module. */
   /*=================================================*/

   PrintTally(WDISPLAY,count,constructClass->constructName,
                             constructClass->pluralName);

   RestoreCurrentModule();
  }

/**********************************************************/
/* SetNextConstruct: Sets the next field of one construct */
/*   to point to another construct of the same type.      */
/**********************************************************/
globle void SetNextConstruct(
  struct constructHeader *theConstruct,
  struct constructHeader *targetConstruct)
  { theConstruct->next = targetConstruct; }

/********************************************************************/
/* GetConstructModuleItem: Returns the construct module for a given */
/*   construct (note that this is a pointer to a data structure     */
/*   like the deffactsModule, not a pointer to an environment       */
/*   module which contains a number of types of constructs.         */
/********************************************************************/
globle struct defmoduleItemHeader *GetConstructModuleItem(
  struct constructHeader *theConstruct)
  { return(theConstruct->whichModule); }

/*************************************************/
/* GetConstructPPForm: Returns the pretty print  */
/*   representation for the specified construct. */
/*************************************************/
globle char *GetConstructPPForm(
  struct constructHeader *theConstruct)
  { return(theConstruct->ppForm); }

/****************************************************/
/* GetNextConstructItem: Returns the next construct */
/*   items from a list of constructs.               */
/****************************************************/
globle struct constructHeader *GetNextConstructItem(
  struct constructHeader *theConstruct,
  int moduleIndex)
  {
   struct defmoduleItemHeader *theModuleItem;

   if (theConstruct == NULL)
     {
      theModuleItem = (struct defmoduleItemHeader *)
                      GetModuleItem(NULL,moduleIndex);
      if (theModuleItem == NULL) return(NULL);
      return(theModuleItem->firstItem);
     }

   return(theConstruct->next);
  }

/*******************************************************/
/* GetConstructModuleItemByIndex: Returns a pointer to */
/*  the defmodule item for the specified construct. If */
/*  theModule is NULL, then the construct module item  */
/*  for the current module is returned, otherwise the  */
/*  construct module item for the specified construct  */
/*  is returned.                                       */
/*******************************************************/
globle struct defmoduleItemHeader *GetConstructModuleItemByIndex(
  struct defmodule *theModule,
  int moduleIndex)
  {
   if (theModule != NULL)
     {
      return((struct defmoduleItemHeader *)
             GetModuleItem(theModule,moduleIndex));
     }

   return((struct defmoduleItemHeader *)
          GetModuleItem(((struct defmodule *) GetCurrentModule()),moduleIndex));
  }

/******************************************/
/* FreeConstructHeaderModule: Deallocates */
/*   the data structures associated with  */
/*   the construct module item header.    */
/******************************************/
globle void FreeConstructHeaderModule(
  struct defmoduleItemHeader *theModuleItem,
  struct construct *constructClass)
  {
   struct constructHeader *thisOne, *nextOne;

   thisOne = theModuleItem->firstItem;

   while (thisOne != NULL)
     {
      nextOne = thisOne->next;
      (*constructClass->freeFunction)(thisOne);
      thisOne = nextOne;
     }
  }

/**********************************************/
/* DoForAllConstructs: Executes an action for */
/*   all constructs of a specified class.     */
/**********************************************/
globle long DoForAllConstructs(
  void (*actionFunction)(struct constructHeader *,void *),
  int moduleItemIndex,
  int interruptable,
  void *userBuffer)
  {
   struct constructHeader *theConstruct;
   struct defmoduleItemHeader *theModuleItem;
   void *theModule;
   long moduleCount = 0L;

   /*==========================*/
   /* Save the current module. */
   /*==========================*/

   SaveCurrentModule();

   /*==================================*/
   /* Loop through all of the modules. */
   /*==================================*/

   for (theModule = GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = GetNextDefmodule(theModule), moduleCount++)
     {
      /*=============================*/
      /* Set the current module to   */
      /* the module we're examining. */
      /*=============================*/

      SetCurrentModule((void *) theModule);

      /*================================================*/
      /* Perform the action for each of the constructs. */
      /*================================================*/

      theModuleItem = (struct defmoduleItemHeader *)
                      GetModuleItem((struct defmodule *) theModule,moduleItemIndex);

      for (theConstruct = theModuleItem->firstItem;
           theConstruct != NULL;
           theConstruct = theConstruct->next)
        {
         if (interruptable)
           {
            if (GetHaltExecution() == TRUE)
              {
               RestoreCurrentModule();
               return(-1L);
              }
           }

         (*actionFunction)(theConstruct,userBuffer);
        }
     }

   /*=============================*/
   /* Restore the current module. */
   /*=============================*/

   RestoreCurrentModule();

   /*=========================================*/
   /* Return the number of modules traversed. */
   /*=========================================*/

   return(moduleCount);
  }

/*****************************************************/
/* InitializeConstructHeader: Initializes construct  */
/*   header info, including to which module item the */
/*   new construct belongs                           */
/*****************************************************/
globle void InitializeConstructHeader(
  char *constructType,
  struct constructHeader *theConstruct,
  SYMBOL_HN *theConstructName)
  {
   struct moduleItem *theModuleItem;
   struct defmoduleItemHeader *theItemHeader;

   theModuleItem = FindModuleItem(constructType);
   theItemHeader = (struct defmoduleItemHeader *)
                   GetModuleItem(NULL,theModuleItem->moduleIndex);

   theConstruct->whichModule = theItemHeader;
   theConstruct->name = theConstructName;
   theConstruct->ppForm = NULL;
   theConstruct->bsaveID = 0L;
   theConstruct->next = NULL;
   theConstruct->usrData = NULL;
  }

/*************************************************/
/* SetConstructPPForm: Sets a construct's pretty */
/*   print form and deletes the old one.         */
/*************************************************/
globle void SetConstructPPForm(
  struct constructHeader *theConstruct,
  char *ppForm)
  {
   if (theConstruct->ppForm != NULL)
     {
      rm((void *) theConstruct->ppForm,
         (int) ((strlen(theConstruct->ppForm) + 1) * sizeof(char)));
     }
   theConstruct->ppForm = ppForm;
  }

#if DEBUGGING_FUNCTIONS

/******************************************************/
/* ConstructPrintWatchAccess: Provides an interface   */
/*   to the list-watch-items function for a construct */
/******************************************************/
globle BOOLEAN ConstructPrintWatchAccess(
  struct construct *constructClass,
  char *log,
  EXPRESSION *argExprs,
  BOOLEAN (*getWatchFunc)(void *),
  void (*setWatchFunc)(BOOLEAN,void *))
  {
   return(ConstructWatchSupport(constructClass,"list-watch-items",log,argExprs,
                                FALSE,FALSE,getWatchFunc,setWatchFunc));
  }

/**************************************************/
/* ConstructSetWatchAccess: Provides an interface */
/*   to the watch function for a construct        */
/**************************************************/
globle BOOLEAN ConstructSetWatchAccess(
  struct construct *constructClass,
  BOOLEAN newState,
  EXPRESSION *argExprs,
  BOOLEAN (*getWatchFunc)(void *),
  void (*setWatchFunc)(BOOLEAN,void *))
  {
   return(ConstructWatchSupport(constructClass,"watch",WERROR,argExprs,
                                TRUE,newState,getWatchFunc,setWatchFunc));
  }

/******************************************************/
/* ConstructWatchSupport: Generic construct interface */
/*   into watch and list-watch-items.                 */
/******************************************************/
static BOOLEAN ConstructWatchSupport(
  struct construct *constructClass,
  char *funcName,
  char *log,
  EXPRESSION *argExprs,
  BOOLEAN setFlag,
  BOOLEAN newState,
  BOOLEAN (*getWatchFunc)(void *),
  void (*setWatchFunc)(BOOLEAN,void *))
  {
   struct defmodule *theModule;
   void *theConstruct;
   DATA_OBJECT constructName;
   int argIndex = 2;

   /*========================================*/
   /* If no constructs are specified, then   */
   /* show/set the trace for all constructs. */
   /*========================================*/

   if (argExprs == NULL)
     {
      /*==========================*/
      /* Save the current module. */
      /*==========================*/

      SaveCurrentModule();

      /*===========================*/
      /* Loop through each module. */
      /*===========================*/

      for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
           theModule != NULL;
           theModule = (struct defmodule *) GetNextDefmodule((void *) theModule))
        {
         /*============================*/
         /* Set the current module to  */
         /* the module being examined. */
         /*============================*/

         SetCurrentModule((void *) theModule);

         /*====================================================*/
         /* If we're displaying the names of constructs with   */
         /* watch flags enabled, then preface each module      */
         /* listing of constructs with the name of the module. */
         /*====================================================*/

         if (setFlag == FALSE)
           {
            PrintRouter(log,GetDefmoduleName((void *) theModule));
            PrintRouter(log,":\n");
           }

         /*============================================*/
         /* Loop through each construct in the module. */
         /*============================================*/

         for (theConstruct = (*constructClass->getNextItemFunction)(NULL);
              theConstruct != NULL;
              theConstruct = (*constructClass->getNextItemFunction)(theConstruct))
           {
            /*=============================================*/
            /* Either set the watch flag for the construct */
            /* or display its current state.               */
            /*=============================================*/

            if (setFlag)
              { (*setWatchFunc)(newState,theConstruct); }
            else
              {
               PrintRouter(log,"   ");
               ConstructPrintWatch(log,constructClass,theConstruct,getWatchFunc);
              }
           }
        }

      /*=============================*/
      /* Restore the current module. */
      /*=============================*/

      RestoreCurrentModule();

      /*====================================*/
      /* Return TRUE to indicate successful */
      /* completion of the command.         */
      /*====================================*/

      return(TRUE);
     }

   /*==================================================*/
   /* Show/set the trace for each specified construct. */
   /*==================================================*/

   while (argExprs != NULL)
     {
      /*==========================================*/
      /* Evaluate the argument that should be a   */
      /* construct name. Return FALSE is an error */
      /* occurs when evaluating the argument.     */
      /*==========================================*/

      if (EvaluateExpression(argExprs,&constructName))
        { return(FALSE); }

      /*================================================*/
      /* Check to see that it's a valid construct name. */
      /*================================================*/

      if ((constructName.type != SYMBOL) ? TRUE :
          ((theConstruct = LookupConstruct(constructClass,
                                           DOToString(constructName),TRUE)) == NULL))
        {
         ExpectedTypeError1(funcName,argIndex,constructClass->constructName);
         return(FALSE);
        }

      /*=============================================*/
      /* Either set the watch flag for the construct */
      /* or display its current state.               */
      /*=============================================*/

      if (setFlag)
        { (*setWatchFunc)(newState,theConstruct); }
      else
        { ConstructPrintWatch(log,constructClass,theConstruct,getWatchFunc); }

      /*===============================*/
      /* Move on to the next argument. */
      /*===============================*/

      argIndex++;
      argExprs = GetNextArgument(argExprs);
     }

   /*====================================*/
   /* Return TRUE to indicate successful */
   /* completion of the command.         */
   /*====================================*/

   return(TRUE);
  }

/*************************************************/
/* ConstructPrintWatch: Displays the trace value */
/*   of a construct for list-watch-items         */
/*************************************************/
static void ConstructPrintWatch(
  char *log,
  struct construct *constructClass,
  void *theConstruct,
  BOOLEAN (*getWatchFunc)(void *))
  {
   PrintRouter(log,ValueToString((*constructClass->getConstructNameFunction)((struct constructHeader *) theConstruct)));
   PrintRouter(log,(*getWatchFunc)(theConstruct) ? " = on\n" : " = off\n");
  }

#endif /* DEBUGGING_FUNCTIONS */

/*****************************************************/
/* LookupConstruct: Finds a construct in the current */
/*   or imported modules. If specified, will also    */
/*   look for construct in a non-imported module.    */
/*****************************************************/
globle void *LookupConstruct(
  struct construct *constructClass,
  char *constructName,
  BOOLEAN moduleNameAllowed)
  {
   void *theConstruct;
   char *constructType;
   int moduleCount;

   /*============================================*/
   /* Look for the specified construct in the    */
   /* current module or in any imported modules. */
   /*============================================*/

   constructType = constructClass->constructName;
   theConstruct = FindImportedConstruct(constructType,NULL,constructName,
                                        &moduleCount,TRUE,NULL);

   /*===========================================*/
   /* Return NULL if the reference is ambiguous */
   /* (it was found in more than one module).   */
   /*===========================================*/

   if (theConstruct != NULL)
     {
      if (moduleCount > 1)
        {
         AmbiguousReferenceErrorMessage(constructType,constructName);
         return(NULL);
        }
      return(theConstruct);
     }

   /*=============================================*/
   /* If specified, check to see if the construct */
   /* is in a non-imported module.                */
   /*=============================================*/

   if (moduleNameAllowed && FindModuleSeparator(constructName))
     { theConstruct = (*constructClass->findFunction)(constructName); }

   /*====================================*/
   /* Return a pointer to the construct. */
   /*====================================*/

   return(theConstruct);
  }


   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                  DEFMODULE MODULE                   */
   /*******************************************************/

/*************************************************************/
/* Purpose: Defines basic defmodule primitive functions such */
/*   as allocating and deallocating, traversing, and finding */
/*   defmodule data structures.                              */
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

#define _MODULDEF_SOURCE_

#include "setup.h"

#include <stdio.h>
#include <string.h>
#define _STDIO_INCLUDED_

#include "memalloc.h"
#include "constant.h"
#include "router.h"
#include "extnfunc.h"
#include "argacces.h"
#include "constrct.h"
#include "modulpsr.h"
#include "modulcmp.h"
#include "modulbsc.h"
#include "utility.h"

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
#include "bload.h"
#include "modulbin.h"
#endif

#include "moduldef.h"

/*******************/
/* DATA STRUCTURES */
/*******************/

typedef struct moduleStackItem
  {
   BOOLEAN changeFlag;
   struct defmodule *theModule;
   struct moduleStackItem *next;
  } MODULE_STACK_ITEM;

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static struct moduleItem           *LastModuleItem = NULL;
   Thread static struct callFunctionItem     *AfterModuleChangeFunctions = NULL;
   Thread static MODULE_STACK_ITEM           *ModuleStack = NULL;
   Thread static BOOLEAN                      CallModuleChangeFunctions = TRUE;

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle struct defmodule            *ListOfDefmodules = NULL;
   Thread globle struct defmodule            *CurrentModule = NULL;
   Thread globle struct defmodule            *LastDefmodule = NULL;
   Thread globle int                          NumberOfModuleItems = 0;
   Thread globle struct moduleItem           *ListOfModuleItems = NULL;
   Thread globle long                         ModuleChangeIndex = 0;
   Thread globle int                          MainModuleRedefinable = TRUE;

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if (! RUN_TIME)
   static void                       ReturnDefmodule(struct defmodule *);
#endif

/**************************************************************/
/* InitializeDefmodules: Initializes the defmodule construct. */
/**************************************************************/
globle void InitializeDefmodules()
  {
   DefmoduleBasicCommands();

#if (! RUN_TIME)
   CreateMainModule();
#endif

#if DEFMODULE_CONSTRUCT && (! RUN_TIME) && (! BLOAD_ONLY)
   AddConstruct("defmodule","defmodules",ParseDefmodule,NULL,NULL,NULL,NULL,
                                                        NULL,NULL,NULL,NULL,NULL);
#endif

#if (! RUN_TIME) && DEFMODULE_CONSTRUCT
   DefineFunction2("get-current-module", 'w',
                   PTIF GetCurrentModuleCommand,
                   "GetCurrentModuleCommand", "00");

   DefineFunction2("set-current-module", 'w',
                   PTIF SetCurrentModuleCommand,
                   "SetCurrentModuleCommand", "11w");
#endif
  }

/******************************************************/
/* RegisterModuleItem: Called to register a construct */
/*   which can be placed within a module.             */
/******************************************************/
globle int RegisterModuleItem(
   char *theItem,
   void *(*allocateFunction)(void),
   void (*freeFunction)(void *),
   void *(*bloadModuleReference)(int),
   void  (*constructsToCModuleReference)(FILE *,int,int,int),
   void *(*findFunction)(char *))
  {
   struct moduleItem *newModuleItem;

   newModuleItem = get_struct(moduleItem);
   newModuleItem->name = theItem;
   newModuleItem->allocateFunction = allocateFunction;
   newModuleItem->freeFunction = freeFunction;
   newModuleItem->bloadModuleReference = bloadModuleReference;
   newModuleItem->constructsToCModuleReference = constructsToCModuleReference;
   newModuleItem->findFunction = findFunction;
   newModuleItem->moduleIndex = NumberOfModuleItems++;
   newModuleItem->next = NULL;

   if (LastModuleItem == NULL)
     {
      ListOfModuleItems = newModuleItem;
      LastModuleItem = newModuleItem;
     }
   else
     {
      LastModuleItem->next = newModuleItem;
      LastModuleItem = newModuleItem;
     }

   return(newModuleItem->moduleIndex);
  }

/***********************************************************/
/* GetListOfModuleItems: Returns the list of module items. */
/***********************************************************/
globle struct moduleItem *GetListOfModuleItems()
  {
   return (ListOfModuleItems);
  }

/***************************************************************/
/* GetNumberOfModuleItems: Returns the number of module items. */
/***************************************************************/
globle int GetNumberOfModuleItems()
  {
   return (NumberOfModuleItems);
  }

/********************************************************/
/* FindModuleItem: Finds the module item data structure */
/*   corresponding to the specified name.               */
/********************************************************/
globle struct moduleItem *FindModuleItem(
  char *theName)
  {
   struct moduleItem *theModuleItem;

   for (theModuleItem = ListOfModuleItems;
        theModuleItem != NULL;
        theModuleItem = theModuleItem->next)
     { if (strcmp(theModuleItem->name,theName) == 0) return(theModuleItem); }

   return(NULL);
  }

/**************************************************************/
/* GetCurrentModule: Returns a pointer to the current module. */
/**************************************************************/
globle void *GetCurrentModule()
  {
   return ((void *) CurrentModule);
  }

/***********************************************************/
/* SetCurrentModule: Sets the value of the current module. */
/***********************************************************/
globle void *SetCurrentModule(
  void *xNewValue)
  {
   struct defmodule *newValue = (struct defmodule *) xNewValue;
   struct callFunctionItem *changeFunctions;
   void *rv;

   /*=============================================*/
   /* Change the current module to the specified  */
   /* module and save the previous current module */
   /* for the return value.                       */
   /*=============================================*/

   rv = (void *) CurrentModule;
   CurrentModule = newValue;

   /*==========================================================*/
   /* Call the list of registered functions that need to know  */
   /* when the module has changed. The module change functions */
   /* should only be called if this is a "real" module change. */
   /* Many routines temporarily change the module to look for  */
   /* constructs, etc. The SaveCurrentModule function will     */
   /* disable the change functions from being called.          */
   /*==========================================================*/

   if (CallModuleChangeFunctions)
     {
      ModuleChangeIndex++;
      changeFunctions = AfterModuleChangeFunctions;
      while (changeFunctions != NULL)
        {
         (* (void (*)(void)) changeFunctions->func)();
         changeFunctions = changeFunctions->next;
        }
     }

   /*=====================================*/
   /* Return the previous current module. */
   /*=====================================*/

   return(rv);
  }

/********************************************************/
/* SaveCurrentModule: Saves current module on stack and */
/*   prevents SetCurrentModule() from calling change    */
/*   functions                                          */
/********************************************************/
globle void SaveCurrentModule()
  {
   MODULE_STACK_ITEM *tmp;

   tmp = get_struct(moduleStackItem);
   tmp->changeFlag = CallModuleChangeFunctions;
   CallModuleChangeFunctions = FALSE;
   tmp->theModule = CurrentModule;
   tmp->next = ModuleStack;
   ModuleStack = tmp;
  }

/**********************************************************/
/* RestoreCurrentModule: Restores saved module and resets */
/*   ability of SetCurrentModule() to call changed        */
/*   functions to previous state                          */
/**********************************************************/
globle void RestoreCurrentModule()
  {
   MODULE_STACK_ITEM *tmp;

   tmp = ModuleStack;
   ModuleStack = tmp->next;
   CallModuleChangeFunctions = tmp->changeFlag;
   CurrentModule = tmp->theModule;
   rtn_struct(moduleStackItem,tmp);
  }

/*************************************************************/
/* GetModuleItem: Returns the data pointer for the specified */
/*   module item in the specified module. If no module is    */
/*   indicated, then the module item for the current module  */
/*   is returned.                                            */
/*************************************************************/
globle void *GetModuleItem(
  struct defmodule *theModule,
  int moduleItemIndex)
  {
   if (theModule == NULL)
     {
      if (CurrentModule == NULL) return(NULL);
      theModule = CurrentModule;
     }

   if (theModule->itemsArray == NULL) return (NULL);
   return ((void *) theModule->itemsArray[moduleItemIndex]);
  }

/************************************************************/
/* SetModuleItem: Sets the data pointer for the specified   */
/*   module item in the specified module. If no module is   */
/*   indicated, then the module item for the current module */
/*   is returned.                                           */
/************************************************************/
globle void SetModuleItem(
  struct defmodule *theModule,
  int moduleItemIndex,
  void *newValue)
  {
   if (theModule == NULL)
     {
      if (CurrentModule == NULL) return;
      theModule = CurrentModule;
     }

   if (theModule->itemsArray == NULL) return;
   theModule->itemsArray[moduleItemIndex] = (struct defmoduleItemHeader *) newValue;
  }

/******************************************************/
/* CreateMainModule: Creates the default MAIN module. */
/******************************************************/
globle void CreateMainModule()
  {
   struct defmodule *newDefmodule;
   struct moduleItem *theItem;
   int i;
   struct defmoduleItemHeader *theHeader;

   /*=======================================*/
   /* Allocate the defmodule data structure */
   /* and name it the MAIN module.          */
   /*=======================================*/

   newDefmodule = get_struct(defmodule);
   newDefmodule->name = (SYMBOL_HN *) AddSymbol("MAIN");
   IncrementSymbolCount(newDefmodule->name);
   newDefmodule->next = NULL;
   newDefmodule->ppForm = NULL;
   newDefmodule->importList = NULL;
   newDefmodule->exportList = NULL;
   newDefmodule->bsaveID = 0L;
   newDefmodule->usrData = NULL;

   /*==================================*/
   /* Initialize the array for storing */
   /* the module's construct lists.    */
   /*==================================*/

   if (NumberOfModuleItems == 0) newDefmodule->itemsArray = NULL;
   else
     {
      newDefmodule->itemsArray = (struct defmoduleItemHeader **)
                                 gm2((int) sizeof(void *) * NumberOfModuleItems);
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
   /* Add the module to the list of modules */
   /* and make it the current module.       */
   /*=======================================*/

#if (! BLOAD_ONLY) && (! RUN_TIME) && DEFMODULE_CONSTRUCT
   SetNumberOfDefmodules(1L);
#endif

   LastDefmodule = newDefmodule;
   ListOfDefmodules = newDefmodule;
   SetCurrentModule((void *) newDefmodule);
  }

/*********************************************************************/
/* SetListOfDefmodules: Sets the list of defmodules to the specified */
/*   value. Normally used when initializing a run-time module or     */
/*   when bloading a binary file to install the list of defmodules.  */
/*********************************************************************/
globle void SetListOfDefmodules(
  void *defmodulePtr)
  {
   ListOfDefmodules = (struct defmodule *) defmodulePtr;

   LastDefmodule = ListOfDefmodules;
   if (LastDefmodule == NULL) return;
   while (LastDefmodule->next != NULL) LastDefmodule = LastDefmodule->next;
  }

/*******************************************************************/
/* GetNextDefmodule: If passed a NULL pointer, returns the first   */
/*   defmodule in the ListOfDefmodules. Otherwise returns the next */
/*   defmodule following the defmodule passed as an argument.      */
/*******************************************************************/
globle void *GetNextDefmodule(
  void *defmodulePtr)
  {
   if (defmodulePtr == NULL)
     { return((void *) ListOfDefmodules); }
   else
     { return((void *) (((struct defmodule *) defmodulePtr)->next)); }
  }

/**************************************/
/* GetDefmoduleName: Returns the name */
/*   of the specified defmodule.      */
/**************************************/
globle char *GetDefmoduleName(
  void *defmodulePtr)
  { return(ValueToString(((struct defmodule *) defmodulePtr)->name)); }

/************************************************/
/* GetDefmodulePPForm: Returns the pretty print */
/*   representation of the specified defmodule. */
/************************************************/
globle char *GetDefmodulePPForm(
  void *defmodulePtr)
  { return(((struct defmodule *) defmodulePtr)->ppForm); }

#if (! RUN_TIME)

/***********************************************/
/* RemoveAllDefmodules: Removes all defmodules */
/*   from the current environment.             */
/***********************************************/
globle void RemoveAllDefmodules()
  {
   struct defmodule *nextDefmodule;

   while (ListOfDefmodules != NULL)
     {
      nextDefmodule = ListOfDefmodules->next;
      ReturnDefmodule(ListOfDefmodules);
      ListOfDefmodules = nextDefmodule;
     }

   CurrentModule = NULL;
   LastDefmodule = NULL;
  }

/************************************************************/
/* ReturnDefmodule: Returns the data structures associated  */
/*   with a defmodule construct to the pool of free memory. */
/************************************************************/
static void ReturnDefmodule(
  struct defmodule *theDefmodule)
  {
   int i;
   struct moduleItem *theItem;
   struct portItem *theSpec, *nextSpec;

   /*=====================================================*/
   /* Set the current module to the module being deleted. */
   /*=====================================================*/

   if (theDefmodule == NULL) return;
   SetCurrentModule((void *) theDefmodule);

   /*============================================*/
   /* Call the free functions for the constructs */
   /* belonging to this module.                  */
   /*============================================*/

   if (theDefmodule->itemsArray != NULL)
     {
      for (i = 0, theItem = ListOfModuleItems;
           (i < NumberOfModuleItems) && (theItem != NULL);
           i++, theItem = theItem->next)
        {
         if (theItem->freeFunction != NULL)
           { (*theItem->freeFunction)(theDefmodule->itemsArray[i]); }
        }

      rm(theDefmodule->itemsArray,(int) sizeof(void *) * NumberOfModuleItems);
    }

   /*======================================================*/
   /* Decrement the symbol count for the defmodule's name. */
   /*======================================================*/

   DecrementSymbolCount(theDefmodule->name);

   /*====================================*/
   /* Free the items in the import list. */
   /*====================================*/

   theSpec = theDefmodule->importList;
   while (theSpec != NULL)
     {
      nextSpec = theSpec->next;
      if (theSpec->moduleName != NULL) DecrementSymbolCount(theSpec->moduleName);
      if (theSpec->constructType != NULL) DecrementSymbolCount(theSpec->constructType);
      if (theSpec->constructName != NULL) DecrementSymbolCount(theSpec->constructName);
      rtn_struct(portItem,theSpec);
      theSpec = nextSpec;
     }

   /*====================================*/
   /* Free the items in the export list. */
   /*====================================*/

   theSpec = theDefmodule->exportList;
   while (theSpec != NULL)
     {
      nextSpec = theSpec->next;
      if (theSpec->moduleName != NULL) DecrementSymbolCount(theSpec->moduleName);
      if (theSpec->constructType != NULL) DecrementSymbolCount(theSpec->constructType);
      if (theSpec->constructName != NULL) DecrementSymbolCount(theSpec->constructName);
      rtn_struct(portItem,theSpec);
      theSpec = nextSpec;
     }

   /*=========================================*/
   /* Free the defmodule pretty print string. */
   /*=========================================*/

   if (theDefmodule->ppForm != NULL)
     {
      rm(theDefmodule->ppForm,
         (int) sizeof(char) * ((int) strlen(theDefmodule->ppForm) + 1));
     }
     
   /*=======================*/
   /* Return the user data. */
   /*=======================*/

   ClearUserDataList(theDefmodule->usrData);
   
   /*======================================*/
   /* Return the defmodule data structure. */
   /*======================================*/

   rtn_struct(defmodule,theDefmodule);
  }

#endif /* (! RUN_TIME) */

/**********************************************************************/
/* FindDefmodule: Searches for a defmodule in the list of defmodules. */
/*   Returns a pointer to the defmodule if found, otherwise NULL.     */
/**********************************************************************/
globle void *FindDefmodule(
  char *defmoduleName)
  {
   struct defmodule *defmodulePtr;
   SYMBOL_HN *findValue;

   if ((findValue = (SYMBOL_HN *) FindSymbol(defmoduleName)) == NULL) return(NULL);

   defmodulePtr = ListOfDefmodules;
   while (defmodulePtr != NULL)
     {
      if (defmodulePtr->name == findValue)
        { return((void *) defmodulePtr); }

      defmodulePtr = defmodulePtr->next;
     }

   return(NULL);
  }

/*************************************************/
/* GetCurrentModuleCommand: H/L access routine   */
/*   for the get-current-module command.         */
/*************************************************/
globle SYMBOL_HN *GetCurrentModuleCommand()
  {
   struct defmodule *theModule;

   ArgCountCheck("get-current-module",EXACTLY,0);

   theModule = (struct defmodule *) GetCurrentModule();

   if (theModule == NULL) return((SYMBOL_HN *) FalseSymbol);

   return((SYMBOL_HN *) AddSymbol(ValueToString(theModule->name)));
  }

/*************************************************/
/* SetCurrentModuleCommand: H/L access routine   */
/*   for the set-current-module command.         */
/*************************************************/
globle SYMBOL_HN *SetCurrentModuleCommand()
  {
   DATA_OBJECT argPtr;
   char *argument;
   struct defmodule *theModule;
   SYMBOL_HN *defaultReturn;

   /*=====================================================*/
   /* Check for the correct number and type of arguments. */
   /*=====================================================*/

   theModule = ((struct defmodule *) GetCurrentModule());
   if (theModule == NULL) return((SYMBOL_HN *) FalseSymbol);

   defaultReturn = (SYMBOL_HN *) AddSymbol(ValueToString(((struct defmodule *) GetCurrentModule())->name));

   if (ArgCountCheck("set-current-module",EXACTLY,1) == -1)
     { return(defaultReturn); }

   if (ArgTypeCheck("set-current-module",1,SYMBOL,&argPtr) == FALSE)
     { return(defaultReturn); }

   argument = DOToString(argPtr);

   /*================================================*/
   /* Set the current module to the specified value. */
   /*================================================*/

   theModule = (struct defmodule *) FindDefmodule(argument);

   if (theModule == NULL)
     {
      CantFindItemErrorMessage("defmodule",argument);
      return(defaultReturn);
     }

   SetCurrentModule((void *) theModule);

   /*================================*/
   /* Return the new current module. */
   /*================================*/

   return((SYMBOL_HN *) defaultReturn);
  }

/*************************************************/
/* AddAfterModuleChangeFunction: Adds a function */
/*   to the list of functions to be called after */
/*   a module change occurs.                     */
/*************************************************/
globle void AddAfterModuleChangeFunction(
  char *name,
  void (*func)(void),
  int priority)
  {
   AfterModuleChangeFunctions =
     AddFunctionToCallList(name,priority,func,AfterModuleChangeFunctions);
  }

/************************************************/
/* IllegalModuleSpecifierMessage: Error message */
/*   for the illegal use of a module specifier. */
/************************************************/
globle void IllegalModuleSpecifierMessage()
  {
   PrintErrorID("MODULDEF",1,TRUE);
   PrintRouter(WERROR,"Illegal use of the module specifier.\n");
  }



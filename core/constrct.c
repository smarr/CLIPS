   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*                  CONSTRUCT MODULE                   */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides basic functionality for creating new    */
/*   types of constructs, saving constructs to a file, and   */
/*   adding new functionality to the clear and reset         */
/*   commands.                                               */
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

#define _CONSTRCT_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include "setup.h"

#include "constant.h"
#include "memalloc.h"
#include "router.h"
#include "scanner.h"
#include "watch.h"
#include "prcdrfun.h"
#include "prcdrpsr.h"
#include "argacces.h"
#include "exprnpsr.h"
#include "multifld.h"
#include "moduldef.h"
#include "utility.h"
#include "commline.h"

#include "constrct.h"

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle int                   ClearReadyInProgress = FALSE;
   Thread globle int                   ClearInProgress = FALSE;
   Thread globle int                   ResetReadyInProgress = FALSE;
   Thread globle int                   ResetInProgress = FALSE;

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

#if (! RUN_TIME) && (! BLOAD_ONLY)
   Thread static struct callFunctionItem   *ListOfSaveFunctions = NULL;
   Thread static BOOLEAN                    PrintWhileLoading = FALSE;
   Thread static BOOLEAN                    WatchCompilations = ON;
#endif

   Thread static struct construct          *ListOfConstructs = NULL;
   Thread static struct callFunctionItem   *ListOfResetFunctions = NULL;
   Thread static struct callFunctionItem   *ListOfClearFunctions = NULL;
   Thread static struct callFunctionItem   *ListOfClearReadyFunctions = NULL;
   Thread static int                        Executing = FALSE;
   Thread static int                      (*BeforeResetFunction)(void) = NULL;

#if (! RUN_TIME) && (! BLOAD_ONLY)

/*************************************************/
/* FindConstruct: Determines whether a construct */
/*   type is in the ListOfConstructs.            */
/*************************************************/
globle struct construct *FindConstruct(
  char *name)
  {
   struct construct *currentPtr;

   for (currentPtr = ListOfConstructs;
        currentPtr != NULL;
        currentPtr = currentPtr->next)
     {
      if (strcmp(name,currentPtr->constructName) == 0)
        { return(currentPtr); }
     }

   return(NULL);
  }

/***********************************************************/
/* RemoveConstruct: Removes a construct and its associated */
/*   parsing function from the ListOfConstructs. Returns   */
/*   TRUE if the construct type was removed, otherwise     */
/*   FALSE.                                                */
/***********************************************************/
globle int RemoveConstruct(
  char *name)
  {
   struct construct *currentPtr, *lastPtr = NULL;

   for (currentPtr = ListOfConstructs;
        currentPtr != NULL;
        currentPtr = currentPtr->next)
     {
      if (strcmp(name,currentPtr->constructName) == 0)
        {
         if (lastPtr == NULL)
           { ListOfConstructs = currentPtr->next; }
         else
           { lastPtr->next = currentPtr->next; }
         rtn_struct(construct,currentPtr);
         return(TRUE);
        }

      lastPtr = currentPtr;
     }

   return(FALSE);
  }

/************************************************/
/* Save: C access routine for the save command. */
/************************************************/
globle int Save(
  char *fileName)
  {
   struct callFunctionItem *saveFunction;
   FILE *filePtr;

   /*=====================*/
   /* Open the save file. */
   /*=====================*/

   if ((filePtr = fopen(fileName,"w")) == NULL)
     { return(FALSE); }

   /*===========================*/
   /* Bypass the router system. */
   /*===========================*/

   SetFastSave(filePtr);

   /*======================*/
   /* Save the constructs. */
   /*======================*/

   for (saveFunction = ListOfSaveFunctions;
        saveFunction != NULL;
        saveFunction = saveFunction->next)
     {
      ((* (void (*)(char *)) saveFunction->func))((char *) filePtr);
     }

   /*======================*/
   /* Close the save file. */
   /*======================*/

   fclose(filePtr);

   /*===========================*/
   /* Remove the router bypass. */
   /*===========================*/

   SetFastSave(NULL);

   /*=========================*/
   /* Return TRUE to indicate */
   /* successful completion.  */
   /*=========================*/

   return(TRUE);
  }

/*******************************************************/
/* RemoveSaveFunction: Removes a function from the     */
/*   ListOfSaveFunctions. Returns TRUE if the function */
/*   was successfully removed, otherwise FALSE.        */
/*******************************************************/
globle BOOLEAN RemoveSaveFunction(
  char *name)
  {
   int found;

   ListOfSaveFunctions =
     RemoveFunctionFromCallList(name,ListOfSaveFunctions,&found);

   if (found) return(TRUE);

   return(FALSE);
  }

/**********************************/
/* SetCompilationsWatch: Sets the */
/*   value of WatchCompilations.  */
/**********************************/
globle void SetCompilationsWatch(
  int value)
  {
   WatchCompilations = value;
  }

/*************************************/
/* GetCompilationsWatch: Returns the */
/*   value of WatchCompilations.     */
/*************************************/
globle BOOLEAN GetCompilationsWatch()
  { return(WatchCompilations); }

/**********************************/
/* SetPrintWhileLoading: Sets the */
/*   value of PrintWhileLoading.  */
/**********************************/
globle void SetPrintWhileLoading(
  BOOLEAN value)
  {
   PrintWhileLoading = value;
  }

/*************************************/
/* GetPrintWhileLoading: Returns the */
/*   value of PrintWhileLoading.     */
/*************************************/
globle BOOLEAN GetPrintWhileLoading()
  {
   return(PrintWhileLoading);
  }
#endif

/*************************************/
/* InitializeConstructs: Initializes */
/*   the Construct Manager.          */
/*************************************/
globle void InitializeConstructs()
  {
#if (! RUN_TIME)
   DefineFunction2("clear",   'v', PTIF ClearCommand,   "ClearCommand", "00");
   DefineFunction2("reset",   'v', PTIF ResetCommand,   "ResetCommand", "00");
#endif

#if DEBUGGING_FUNCTIONS && (! RUN_TIME) && (! BLOAD_ONLY)
   AddWatchItem("compilations",0,&WatchCompilations,30,NULL,NULL);
#endif
  }

/**************************************/
/* ClearCommand: H/L access routine   */
/*   for the clear command.           */
/**************************************/
globle void ClearCommand()
  {
   if (ArgCountCheck("clear",EXACTLY,0) == -1) return;
   Clear();
   return;
  }

/**************************************/
/* ResetCommand: H/L access routine   */
/*   for the reset command.           */
/**************************************/
globle void ResetCommand()
  {
   if (ArgCountCheck("reset",EXACTLY,0) == -1) return;
   Reset();
   return;
  }

/****************************/
/* Reset: C access routine  */
/*   for the reset command. */
/****************************/
globle void Reset()
  {
   struct callFunctionItem *resetPtr;

   /*=====================================*/
   /* The reset command can't be executed */
   /* while a reset is in progress.       */
   /*=====================================*/

   if (ResetInProgress) return;

   ResetInProgress = TRUE;
   ResetReadyInProgress = TRUE;

   /*================================================*/
   /* If the reset is performed from the top level   */
   /* command prompt, reset the halt execution flag. */
   /*================================================*/

   if (CurrentEvaluationDepth == 0) SetHaltExecution(FALSE);

   /*=======================================================*/
   /* Call the before reset function to determine if the    */
   /* reset should continue. [Used by the some of the       */
   /* windowed interfaces to query the user whether a       */
   /* reset should proceed with activations on the agenda.] */
   /*=======================================================*/

   if ((BeforeResetFunction != NULL) ? ((*BeforeResetFunction)() == FALSE) :
                                       FALSE)
     {
      ResetReadyInProgress = FALSE;
      ResetInProgress = FALSE;
      return;
     }
   ResetReadyInProgress = FALSE;

   /*===========================*/
   /* Call each reset function. */
   /*===========================*/

   for (resetPtr = ListOfResetFunctions;
        (resetPtr != NULL) && (GetHaltExecution() == FALSE);
        resetPtr = resetPtr->next)
     { (*resetPtr->func)(); }

   /*============================================*/
   /* Set the current module to the MAIN module. */
   /*============================================*/

   SetCurrentModule((void *) FindDefmodule("MAIN"));

   /*===========================================*/
   /* Perform periodic cleanup if the reset was */
   /* issued from an embedded controller.       */
   /*===========================================*/

   if ((CurrentEvaluationDepth == 0) && (! EvaluatingTopLevelCommand) &&
       (CurrentExpression == NULL))
     { PeriodicCleanup(TRUE,FALSE); }

   /*===================================*/
   /* A reset is no longer in progress. */
   /*===================================*/

   ResetInProgress = FALSE;
  }

/************************************/
/* SetBeforeResetFunction: Sets the */
/*  value of BeforeResetFunction.   */
/************************************/
globle int (*SetBeforeResetFunction(int (*theFunction)(void)))(void)
  {
   int (*tempFunction)(void);

   tempFunction = BeforeResetFunction;
   BeforeResetFunction = theFunction;
   return(tempFunction);
  }

/*************************************/
/* AddResetFunction: Adds a function */
/*   to ListOfResetFunctions.        */
/*************************************/
globle BOOLEAN AddResetFunction(
  char *name,
  void (*functionPtr)(void),
  int priority)
  {
   ListOfResetFunctions = AddFunctionToCallList(name,priority,
                                                functionPtr,
                                                ListOfResetFunctions);
   return(TRUE);
  }

/*******************************************/
/* RemoveResetFunction: Removes a function */
/*   from the ListOfResetFunctions.        */
/*******************************************/
globle BOOLEAN RemoveResetFunction(
  char *name)
  {
   int found;

   ListOfResetFunctions =
      RemoveFunctionFromCallList(name,ListOfResetFunctions,&found);

   if (found) return(TRUE);

   return(FALSE);
  }

/**************************************************/
/* Clear: C access routine for the clear command. */
/**************************************************/
globle void Clear()
  {
   struct callFunctionItem *theFunction;

   /*==========================================*/
   /* Activate the watch router which captures */
   /* trace output so that it is not displayed */
   /* during a clear.                          */
   /*==========================================*/

#if DEBUGGING_FUNCTIONS
   ActivateRouter(WTRACE);
#endif

   /*===================================*/
   /* Determine if a clear is possible. */
   /*===================================*/

   ClearReadyInProgress = TRUE;
   if (ClearReady() == FALSE)
     {
      PrintErrorID("CONSTRCT",1,FALSE);
      PrintRouter(WERROR,"Some constructs are still in use. Clear cannot continue.\n");
#if DEBUGGING_FUNCTIONS
      DeactivateRouter(WTRACE);
#endif
      ClearReadyInProgress = FALSE;
      return;
     }
   ClearReadyInProgress = FALSE;

   /*===========================*/
   /* Call all clear functions. */
   /*===========================*/

   ClearInProgress = TRUE;

   for (theFunction = ListOfClearFunctions;
        theFunction != NULL;
        theFunction = theFunction->next)
     { (*theFunction->func)(); }

   /*=============================*/
   /* Deactivate the watch router */
   /* for capturing output.       */
   /*=============================*/

#if DEBUGGING_FUNCTIONS
   DeactivateRouter(WTRACE);
#endif

   /*===========================================*/
   /* Perform periodic cleanup if the clear was */
   /* issued from an embedded controller.       */
   /*===========================================*/

   if ((CurrentEvaluationDepth == 0) && (! EvaluatingTopLevelCommand) &&
       (CurrentExpression == NULL))
     { PeriodicCleanup(TRUE,FALSE); }

   /*===========================*/
   /* Clear has been completed. */
   /*===========================*/

   ClearInProgress = FALSE;
  }

/*********************************************************/
/* ClearReady: Returns TRUE if a clear can be performed, */
/*   otherwise FALSE. Note that this is destructively    */
/*   determined (e.g. facts will be deleted as part of   */
/*   the determination).                                 */
/*********************************************************/
globle BOOLEAN ClearReady()
  {
   struct callFunctionItem *theFunction;
   int (*tempFunction)(void);

   for (theFunction = ListOfClearReadyFunctions;
        theFunction != NULL;
        theFunction = theFunction->next)
     {
      tempFunction = (int (*)(void)) theFunction->func;
      if ((*tempFunction)() == FALSE)
        { return(FALSE); }
     }

   return(TRUE);
  }

/******************************************/
/* AddClearReadyFunction: Adds a function */
/*   to ListOfClearReadyFunctions.        */
/******************************************/
globle BOOLEAN AddClearReadyFunction(
  char *name,
  int (*functionPtr)(void),
  int priority)
  {
   ListOfClearReadyFunctions =
     AddFunctionToCallList(name,priority,
                           (void (*)(void)) functionPtr,
                           ListOfClearReadyFunctions);
   return(1);
  }

/************************************************/
/* RemoveClearReadyFunction: Removes a function */
/*   from the ListOfClearReadyFunctions.        */
/************************************************/
globle BOOLEAN RemoveClearReadyFunction(
  char *name)
  {
   int found;

   ListOfClearReadyFunctions =
      RemoveFunctionFromCallList(name,ListOfClearReadyFunctions,&found);

   if (found) return(TRUE);

   return(FALSE);
  }

/*************************************/
/* AddClearFunction: Adds a function */
/*   to ListOfClearFunctions.        */
/*************************************/
globle BOOLEAN AddClearFunction(
  char *name,
  void (*functionPtr)(void),
  int priority)
  {
   ListOfClearFunctions =
      AddFunctionToCallList(name,priority,
                            (void (*)(void)) functionPtr,
                            ListOfClearFunctions);
   return(1);
  }

/*******************************************/
/* RemoveClearFunction: Removes a function */
/*    from the ListOfClearFunctions.       */
/*******************************************/
globle BOOLEAN RemoveClearFunction(
  char *name)
  {
   int found;

   ListOfClearFunctions =
     RemoveFunctionFromCallList(name,ListOfClearFunctions,&found);

   if (found) return(TRUE);

   return(FALSE);
  }

/***********************************************/
/* ExecutingConstruct: Returns TRUE if a */
/*   construct is currently being executed,    */
/*   otherwise FALSE.                    */
/***********************************************/
globle int ExecutingConstruct()
  { return(Executing); }

/********************************************/
/* SetExecutingConstruct: Sets the value of */
/*   the executing variable indicating that */
/*   actions such as reset, clear, etc      */
/*   should not be performed.               */
/********************************************/
globle void SetExecutingConstruct(
  int value)
  {
   Executing = value;
  }

/************************************************************/
/* OldGetConstructList: Returns a list of all the construct */
/*   names in a multifield value. It doesn't check the      */
/*   number of arguments. It assumes that the restriction   */
/*   string in DefineFunction2 call was "00".               */
/************************************************************/
globle void OldGetConstructList(
  DATA_OBJECT_PTR returnValue,
  void *(*nextFunction)(void *),
  char *(*nameFunction)(void *))
  {
   void *theConstruct;
   long count = 0;
   struct multifield *theList;

   /*====================================*/
   /* Determine the number of constructs */
   /* of the specified type.             */
   /*====================================*/

   for (theConstruct = (*nextFunction)(NULL);
        theConstruct != NULL;
        theConstruct = (*nextFunction)(theConstruct))
     { count++; }

   /*===========================*/
   /* Create a multifield large */
   /* enough to store the list. */
   /*===========================*/

   SetpType(returnValue,MULTIFIELD);
   SetpDOBegin(returnValue,1);
   SetpDOEnd(returnValue,count);
   theList = (struct multifield *) CreateMultifield((int) count);
   SetpValue(returnValue,(void *) theList);

   /*====================================*/
   /* Store the names in the multifield. */
   /*====================================*/

   for (theConstruct = (*nextFunction)(NULL), count = 1;
        theConstruct != NULL;
        theConstruct = (*nextFunction)(theConstruct), count++)
     {
      if (HaltExecution == TRUE)
        {
         SetMultifieldErrorValue(returnValue);
         return;
        }
      SetMFType(theList,count,SYMBOL);
      SetMFValue(theList,count,AddSymbol((*nameFunction)(theConstruct)));
     }
  }

/*******************************************************/
/* DeinstallConstructHeader: Decrements the busy count */
/*   of a construct name and frees its pretty print    */
/*   representation string (both of which are stored   */
/*   in the generic construct header).                 */
/*******************************************************/
globle void DeinstallConstructHeader(
  struct constructHeader *theHeader)
  {
   DecrementSymbolCount(theHeader->name);
   if (theHeader->ppForm != NULL)
     {
      rm(theHeader->ppForm,
         (int) sizeof(char) * ((int) strlen(theHeader->ppForm) + 1));
      theHeader->ppForm = NULL;
     }

   if (theHeader->usrData != NULL)
     {
      ClearUserDataList(theHeader->usrData);
      theHeader->usrData = NULL;
     }
  }

/*****************************************************/
/* AddConstruct: Adds a construct and its associated */
/*   parsing function to the ListOfConstructs.       */
/*****************************************************/
globle struct construct *AddConstruct(
  char *name,
  char *pluralName,
  int (*parseFunction)(char *),
  void *(*findFunction)(char *),
  SYMBOL_HN *(*getConstructNameFunction)(struct constructHeader *),
  char *(*getPPFormFunction)(struct constructHeader *),
  struct defmoduleItemHeader *(*getModuleItemFunction)(struct constructHeader *),
  void *(*getNextItemFunction)(void *),
  void (*setNextItemFunction)(struct constructHeader *,struct constructHeader *),
  BOOLEAN (*isConstructDeletableFunction)(void *),
  int (*deleteFunction)(void *),
  void (*freeFunction)(void *))
  {
   struct construct *newPtr;

   /*=============================*/
   /* Allocate and initialize the */
   /* construct data structure.   */
   /*=============================*/

   newPtr = get_struct(construct);

   newPtr->constructName = name;
   newPtr->pluralName = pluralName;
   newPtr->parseFunction = parseFunction;
   newPtr->findFunction = findFunction;
   newPtr->getConstructNameFunction = getConstructNameFunction;
   newPtr->getPPFormFunction = getPPFormFunction;
   newPtr->getModuleItemFunction = getModuleItemFunction;
   newPtr->getNextItemFunction = getNextItemFunction;
   newPtr->setNextItemFunction = setNextItemFunction;
   newPtr->isConstructDeletableFunction = isConstructDeletableFunction;
   newPtr->deleteFunction = deleteFunction;
   newPtr->freeFunction = freeFunction;

   /*===============================*/
   /* Add the construct to the list */
   /* of constructs and return it.  */
   /*===============================*/

   newPtr->next = ListOfConstructs;
   ListOfConstructs = newPtr;
   return(newPtr);
  }

/************************************/
/* AddSaveFunction: Adds a function */
/*   to the ListOfSaveFunctions.    */
/************************************/
globle BOOLEAN AddSaveFunction(
  char *name,
  void (*functionPtr)(char *),
  int priority)
  {
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(name)
#pragma unused(functionPtr)
#pragma unused(priority)
#endif
#if (! RUN_TIME) && (! BLOAD_ONLY)
   ListOfSaveFunctions =
     AddFunctionToCallList(name,priority,
                           (void (*)(void)) functionPtr,
                           ListOfSaveFunctions);
#endif

   return(1);
  }

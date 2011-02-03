   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
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
/*      6.24: Added environment parameter to GenClose.       */
/*            Added environment parameter to GenOpen.        */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#define _CONSTRCT_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include "setup.h"

#include "constant.h"
#include "envrnmnt.h"
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
#include "sysdep.h"
#include "utility.h"
#include "commline.h"

#include "ruledef.h" /* TBD Remove */
#include "constrct.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                        DeallocateConstructData(void *,EXEC_STATUS);

/**************************************************/
/* InitializeConstructData: Allocates environment */
/*    data for constructs.                        */
/**************************************************/
globle void InitializeConstructData(
  void *theEnv,
  EXEC_STATUS)
  {
   AllocateEnvironmentData(theEnv,execStatus,CONSTRUCT_DATA,sizeof(struct constructData),DeallocateConstructData);

#if (! RUN_TIME) && (! BLOAD_ONLY)   
   ConstructData(theEnv,execStatus)->WatchCompilations = ON;
#endif
  }
  
/****************************************************/
/* DeallocateConstructData: Deallocates environment */
/*    data for constructs.                          */
/****************************************************/
static void DeallocateConstructData(
  void *theEnv,
  EXEC_STATUS)
  {
   struct construct *tmpPtr, *nextPtr;

#if (! RUN_TIME) && (! BLOAD_ONLY)
   DeallocateCallList(theEnv,execStatus,ConstructData(theEnv,execStatus)->ListOfSaveFunctions);
#endif
   DeallocateCallList(theEnv,execStatus,ConstructData(theEnv,execStatus)->ListOfResetFunctions);
   DeallocateCallList(theEnv,execStatus,ConstructData(theEnv,execStatus)->ListOfClearFunctions);
   DeallocateCallList(theEnv,execStatus,ConstructData(theEnv,execStatus)->ListOfClearReadyFunctions);
   
   tmpPtr = ConstructData(theEnv,execStatus)->ListOfConstructs;
   while (tmpPtr != NULL)
     {
      nextPtr = tmpPtr->next;
      rtn_struct(theEnv,execStatus,construct,tmpPtr);
      tmpPtr = nextPtr;
     }
  }

#if (! RUN_TIME) && (! BLOAD_ONLY)

/*************************************************/
/* FindConstruct: Determines whether a construct */
/*   type is in the ListOfConstructs.            */
/*************************************************/
globle struct construct *FindConstruct(
  void *theEnv,
  EXEC_STATUS,
  char *name)
  {
   struct construct *currentPtr;

   for (currentPtr = ConstructData(theEnv,execStatus)->ListOfConstructs;
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
  void *theEnv,
  EXEC_STATUS,
  char *name)
  {
   struct construct *currentPtr, *lastPtr = NULL;

   for (currentPtr = ConstructData(theEnv,execStatus)->ListOfConstructs;
        currentPtr != NULL;
        currentPtr = currentPtr->next)
     {
      if (strcmp(name,currentPtr->constructName) == 0)
        {
         if (lastPtr == NULL)
           { ConstructData(theEnv,execStatus)->ListOfConstructs = currentPtr->next; }
         else
           { lastPtr->next = currentPtr->next; }
         rtn_struct(theEnv,execStatus,construct,currentPtr);
         return(TRUE);
        }

      lastPtr = currentPtr;
     }

   return(FALSE);
  }

/***************************/
/* Save: C access routine  */
/*   for the save command. */
/***************************/
#if ALLOW_ENVIRONMENT_GLOBALS
globle int Save(
  char *fileName)  
  {
   return EnvSave(GetCurrentEnvironment(),getCurrentExecutionState(),fileName);
  }  
#endif

/************************************************/
/* Save: C access routine for the save command. */
/************************************************/
globle int EnvSave(
  void *theEnv,
  EXEC_STATUS,
  char *fileName)
  {
   struct callFunctionItem *saveFunction;
   FILE *filePtr;
   void *defmodulePtr;

   /*=====================*/
   /* Open the save file. */
   /*=====================*/

   if ((filePtr = GenOpen(theEnv,execStatus,fileName,"w")) == NULL)
     { return(FALSE); }

   /*===========================*/
   /* Bypass the router system. */
   /*===========================*/

   SetFastSave(theEnv,execStatus,filePtr);

   /*======================*/
   /* Save the constructs. */
   /*======================*/
   
   for (defmodulePtr = EnvGetNextDefmodule(theEnv,execStatus,NULL);
        defmodulePtr != NULL;
        defmodulePtr = EnvGetNextDefmodule(theEnv,execStatus,defmodulePtr))
     {
      for (saveFunction = ConstructData(theEnv,execStatus)->ListOfSaveFunctions;
           saveFunction != NULL;
           saveFunction = saveFunction->next)
        {
         ((* (void (*)(void *,EXEC_STATUS,void *,char *)) saveFunction->func))(theEnv,execStatus,defmodulePtr,(char *) filePtr);
        }
     }

   /*======================*/
   /* Close the save file. */
   /*======================*/

   GenClose(theEnv,execStatus,filePtr);

   /*===========================*/
   /* Remove the router bypass. */
   /*===========================*/

   SetFastSave(theEnv,execStatus,NULL);

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
globle intBool RemoveSaveFunction(
  void *theEnv,
  EXEC_STATUS,
  char *name)
  {
   int found;

   ConstructData(theEnv,execStatus)->ListOfSaveFunctions =
     RemoveFunctionFromCallList(theEnv,execStatus,name,ConstructData(theEnv,execStatus)->ListOfSaveFunctions,&found);

   if (found) return(TRUE);

   return(FALSE);
  }

/**********************************/
/* SetCompilationsWatch: Sets the */
/*   value of WatchCompilations.  */
/**********************************/
globle void SetCompilationsWatch(
  void *theEnv,
  EXEC_STATUS,
  unsigned value)
  {
   ConstructData(theEnv,execStatus)->WatchCompilations = value;
  }

/*************************************/
/* GetCompilationsWatch: Returns the */
/*   value of WatchCompilations.     */
/*************************************/
globle unsigned GetCompilationsWatch(
  void *theEnv,
  EXEC_STATUS)
  {   
   return(ConstructData(theEnv,execStatus)->WatchCompilations); 
  }

/**********************************/
/* SetPrintWhileLoading: Sets the */
/*   value of PrintWhileLoading.  */
/**********************************/
globle void SetPrintWhileLoading(
  void *theEnv,
  EXEC_STATUS,
  intBool value)
  {
   ConstructData(theEnv,execStatus)->PrintWhileLoading = value;
  }

/*************************************/
/* GetPrintWhileLoading: Returns the */
/*   value of PrintWhileLoading.     */
/*************************************/
globle intBool GetPrintWhileLoading(
  void *theEnv,
  EXEC_STATUS)
  {
   return(ConstructData(theEnv,execStatus)->PrintWhileLoading);
  }
#endif

/*************************************/
/* InitializeConstructs: Initializes */
/*   the Construct Manager.          */
/*************************************/
globle void InitializeConstructs(
  void *theEnv,
  EXEC_STATUS)
  {
#if (! RUN_TIME)
   EnvDefineFunction2(theEnv,execStatus,"clear",   'v', PTIEF ClearCommand,   "ClearCommand", "00");
   EnvDefineFunction2(theEnv,execStatus,"reset",   'v', PTIEF ResetCommand,   "ResetCommand", "00");

#if DEBUGGING_FUNCTIONS && (! BLOAD_ONLY)
   AddWatchItem(theEnv,execStatus,"compilations",0,&ConstructData(theEnv,execStatus)->WatchCompilations,30,NULL,NULL);
#endif
#else
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv,execStatus)
#endif
#endif
  }

/**************************************/
/* ClearCommand: H/L access routine   */
/*   for the clear command.           */
/**************************************/
globle void ClearCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   if (EnvArgCountCheck(theEnv,execStatus,"clear",EXACTLY,0) == -1) return;
   EnvClear(theEnv,execStatus);
   return;
  }

/**************************************/
/* ResetCommand: H/L access routine   */
/*   for the reset command.           */
/**************************************/
globle void ResetCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   if (EnvArgCountCheck(theEnv,execStatus,"reset",EXACTLY,0) == -1) return;
   EnvReset(theEnv,execStatus);
   return;
  }

/****************************/
/* Reset: C access routine  */
/*   for the reset command. */
/****************************/
#if ALLOW_ENVIRONMENT_GLOBALS
globle void Reset(EXEC_STATUS)
  {
   EnvReset(GetCurrentEnvironment(),execStatus);
  }  
#endif

/******************************/
/* EnvReset: C access routine */
/*   for the reset command.   */
/******************************/
globle void EnvReset(
  void *theEnv,
  EXEC_STATUS)
  {
   struct callFunctionItem *resetPtr;

   /*=====================================*/
   /* The reset command can't be executed */
   /* while a reset is in progress.       */
   /*=====================================*/

   if (ConstructData(theEnv,execStatus)->ResetInProgress) return;

   ConstructData(theEnv,execStatus)->ResetInProgress = TRUE;
   ConstructData(theEnv,execStatus)->ResetReadyInProgress = TRUE;

   /*================================================*/
   /* If the reset is performed from the top level   */
   /* command prompt, reset the halt execution flag. */
   /*================================================*/

   if (execStatus->CurrentEvaluationDepth == 0) SetHaltExecution(theEnv,execStatus,FALSE);

   /*=======================================================*/
   /* Call the before reset function to determine if the    */
   /* reset should continue. [Used by the some of the       */
   /* windowed interfaces to query the user whether a       */
   /* reset should proceed with activations on the agenda.] */
   /*=======================================================*/

   if ((ConstructData(theEnv,execStatus)->BeforeResetFunction != NULL) ? 
       ((*ConstructData(theEnv,execStatus)->BeforeResetFunction)(theEnv,execStatus) == FALSE) :
                                       FALSE)
     {
      ConstructData(theEnv,execStatus)->ResetReadyInProgress = FALSE;
      ConstructData(theEnv,execStatus)->ResetInProgress = FALSE;
      return;
     }
   ConstructData(theEnv,execStatus)->ResetReadyInProgress = FALSE;

   /*===========================*/
   /* Call each reset function. */
   /*===========================*/

   for (resetPtr = ConstructData(theEnv,execStatus)->ListOfResetFunctions;
        (resetPtr != NULL) && (GetHaltExecution(theEnv,execStatus) == FALSE);
        resetPtr = resetPtr->next)
     { 
      if (resetPtr->environmentAware)
        { (*resetPtr->func)(theEnv,execStatus); }
      else            
        { (* (void (*)(void)) resetPtr->func)(); }
     }

   /*============================================*/
   /* Set the current module to the MAIN module. */
   /*============================================*/

   EnvSetCurrentModule(theEnv,execStatus,(void *) EnvFindDefmodule(theEnv,execStatus,"MAIN"));

   /*===========================================*/
   /* Perform periodic cleanup if the reset was */
   /* issued from an embedded controller.       */
   /*===========================================*/

   if ((execStatus->CurrentEvaluationDepth == 0) && (! CommandLineData(theEnv,execStatus)->EvaluatingTopLevelCommand) &&
       (execStatus->CurrentExpression == NULL))
     { PeriodicCleanup(theEnv,execStatus,TRUE,FALSE); }

   /*===================================*/
   /* A reset is no longer in progress. */
   /*===================================*/

   ConstructData(theEnv,execStatus)->ResetInProgress = FALSE;
  }

/************************************/
/* SetBeforeResetFunction: Sets the */
/*  value of BeforeResetFunction.   */
/************************************/
globle int (*SetBeforeResetFunction(
	void *theEnv,
  EXEC_STATUS,
  int (*theFunction)(void *)))(void *)
  {
   int (*tempFunction)(void *);

   tempFunction = ConstructData(theEnv,execStatus)->BeforeResetFunction;
   ConstructData(theEnv,execStatus)->BeforeResetFunction = theFunction;
   return(tempFunction);
  }

#if ALLOW_ENVIRONMENT_GLOBALS
/*************************************/
/* AddResetFunction: Adds a function */
/*   to ListOfResetFunctions.        */
/*************************************/
globle intBool AddResetFunction(
  char *name,
  void (*functionPtr)(void),
  int priority)
  {
   void *theEnv;
	  
   theEnv = GetCurrentEnvironment();
   EXEC_STATUS = GetCurrentExectionStatus();
   
   ConstructData(theEnv,execStatus)->ListOfResetFunctions = 
      AddFunctionToCallList(theEnv,execStatus,name,priority,(void (*)(void *)) functionPtr,
                            ConstructData(theEnv,execStatus)->ListOfResetFunctions,FALSE);
   return(TRUE);
  }
#endif

/****************************************/
/* EnvAddResetFunction: Adds a function */
/*   to ListOfResetFunctions.           */
/****************************************/
globle intBool EnvAddResetFunction(
  void *theEnv,
  EXEC_STATUS,
  char *name,
  void (*functionPtr)(void *),
  int priority)
  {
   ConstructData(theEnv,execStatus)->ListOfResetFunctions = AddFunctionToCallList(theEnv,execStatus,name,priority,
                                                functionPtr,
                                                ConstructData(theEnv,execStatus)->ListOfResetFunctions,TRUE);
   return(TRUE);
  }

/**********************************************/
/* EnvRemoveResetFunction: Removes a function */
/*   from the ListOfResetFunctions.           */
/**********************************************/
globle intBool EnvRemoveResetFunction(
  void *theEnv,
  EXEC_STATUS,
  char *name)
  {
   int found;

   ConstructData(theEnv,execStatus)->ListOfResetFunctions =
      RemoveFunctionFromCallList(theEnv,execStatus,name,ConstructData(theEnv,execStatus)->ListOfResetFunctions,&found);

   if (found) return(TRUE);

   return(FALSE);
  }

/****************************/
/* Clear: C access routine  */
/*   for the clear command. */
/****************************/
#if ALLOW_ENVIRONMENT_GLOBALS
globle void Clear(EXEC_STATUS)
  {
   EnvClear(GetCurrentEnvironment(),execStatus);
  }  
#endif
    
/*****************************************************/
/* EnvClear: C access routine for the clear command. */
/*****************************************************/
globle void EnvClear(
  void *theEnv,
  EXEC_STATUS)
  {
   struct callFunctionItem *theFunction;
   
   /*==========================================*/
   /* Activate the watch router which captures */
   /* trace output so that it is not displayed */
   /* during a clear.                          */
   /*==========================================*/

#if DEBUGGING_FUNCTIONS
   EnvActivateRouter(theEnv,execStatus,WTRACE);
#endif

   /*===================================*/
   /* Determine if a clear is possible. */
   /*===================================*/

   ConstructData(theEnv,execStatus)->ClearReadyInProgress = TRUE;
   if (ClearReady(theEnv,execStatus) == FALSE)
     {
      PrintErrorID(theEnv,execStatus,"CONSTRCT",1,FALSE);
      EnvPrintRouter(theEnv,execStatus,WERROR,"Some constructs are still in use. Clear cannot continue.\n");
#if DEBUGGING_FUNCTIONS
      EnvDeactivateRouter(theEnv,execStatus,WTRACE);
#endif
      ConstructData(theEnv,execStatus)->ClearReadyInProgress = FALSE;
      return;
     }
   ConstructData(theEnv,execStatus)->ClearReadyInProgress = FALSE;

   /*===========================*/
   /* Call all clear functions. */
   /*===========================*/

   ConstructData(theEnv,execStatus)->ClearInProgress = TRUE;

   for (theFunction = ConstructData(theEnv,execStatus)->ListOfClearFunctions;
        theFunction != NULL;
        theFunction = theFunction->next)
     { 
      if (theFunction->environmentAware)
        { (*theFunction->func)(theEnv,execStatus); }
      else            
        { (* (void (*)(void)) theFunction->func)(); }
     }

   /*=============================*/
   /* Deactivate the watch router */
   /* for capturing output.       */
   /*=============================*/

#if DEBUGGING_FUNCTIONS
   EnvDeactivateRouter(theEnv,execStatus,WTRACE);
#endif

   /*===========================================*/
   /* Perform periodic cleanup if the clear was */
   /* issued from an embedded controller.       */
   /*===========================================*/

   if ((execStatus->CurrentEvaluationDepth == 0) && (! CommandLineData(theEnv,execStatus)->EvaluatingTopLevelCommand) &&
       (execStatus->CurrentExpression == NULL))
     { PeriodicCleanup(theEnv,execStatus,TRUE,FALSE); }

   /*===========================*/
   /* Clear has been completed. */
   /*===========================*/

   ConstructData(theEnv,execStatus)->ClearInProgress = FALSE;
   
   if ((DefruleData(theEnv,execStatus)->RightPrimeJoins != NULL) ||
       (DefruleData(theEnv,execStatus)->LeftPrimeJoins != NULL))
     { SystemError(theEnv,execStatus,"CONSTRCT",1); }
       
   /*============================*/
   /* Perform reset after clear. */
   /*============================*/
   
   EnvReset(theEnv,execStatus);
  }

/*********************************************************/
/* ClearReady: Returns TRUE if a clear can be performed, */
/*   otherwise FALSE. Note that this is destructively    */
/*   determined (e.g. facts will be deleted as part of   */
/*   the determination).                                 */
/*********************************************************/
globle intBool ClearReady(
  void *theEnv,
  EXEC_STATUS)
  {
   struct callFunctionItem *theFunction;
   int (*tempFunction)(void *,EXEC_STATUS);

   for (theFunction = ConstructData(theEnv,execStatus)->ListOfClearReadyFunctions;
        theFunction != NULL;
        theFunction = theFunction->next)
     {
      tempFunction = (int (*)(void *)) theFunction->func;
      if ((*tempFunction)(theEnv,execStatus) == FALSE)
        { return(FALSE); }
     }

   return(TRUE);
  }

/******************************************/
/* AddClearReadyFunction: Adds a function */
/*   to ListOfClearReadyFunctions.        */
/******************************************/
globle intBool AddClearReadyFunction(
  void *theEnv,
  EXEC_STATUS,
  char *name,
  int (*functionPtr)(void *),
  int priority)
  {
   ConstructData(theEnv,execStatus)->ListOfClearReadyFunctions =
     AddFunctionToCallList(theEnv,execStatus,name,priority,
                           (void (*)(void *)) functionPtr,
                           ConstructData(theEnv,execStatus)->ListOfClearReadyFunctions,TRUE);
   return(1);
  }

/************************************************/
/* RemoveClearReadyFunction: Removes a function */
/*   from the ListOfClearReadyFunctions.        */
/************************************************/
globle intBool RemoveClearReadyFunction(
  void *theEnv,
  EXEC_STATUS,
  char *name)
  {
   int found;

   ConstructData(theEnv,execStatus)->ListOfClearReadyFunctions =
      RemoveFunctionFromCallList(theEnv,execStatus,name,ConstructData(theEnv,execStatus)->ListOfClearReadyFunctions,&found);

   if (found) return(TRUE);

   return(FALSE);
  }

#if ALLOW_ENVIRONMENT_GLOBALS
/*************************************/
/* AddClearFunction: Adds a function */
/*   to ListOfClearFunctions.        */
/*************************************/
globle intBool AddClearFunction(
  char *name,
  void (*functionPtr)(void),
  int priority)
  {
   void *theEnv;

   theEnv = GetCurrentEnvironment();
   EXEC_STATUS = GetCurrentExectionStatus();
   
   ConstructData(theEnv,execStatus)->ListOfClearFunctions =
      AddFunctionToCallList(theEnv,execStatus,name,priority,
                            (void (*)(void *)) functionPtr,
                            ConstructData(theEnv,execStatus)->ListOfClearFunctions,FALSE);
   return(1);
  }
#endif
    
/****************************************/
/* EnvAddClearFunction: Adds a function */
/*   to ListOfClearFunctions.           */
/****************************************/
globle intBool EnvAddClearFunction(
  void *theEnv,
  EXEC_STATUS,
  char *name,
  void (*functionPtr)(void *),
  int priority)
  {
   ConstructData(theEnv,execStatus)->ListOfClearFunctions =
      AddFunctionToCallList(theEnv,execStatus,name,priority,
                            (void (*)(void *)) functionPtr,
                            ConstructData(theEnv,execStatus)->ListOfClearFunctions,TRUE);
   return(1);
  }

/**********************************************/
/* EnvRemoveClearFunction: Removes a function */
/*    from the ListOfClearFunctions.          */
/**********************************************/
globle intBool EnvRemoveClearFunction(
  void *theEnv,
  EXEC_STATUS,
  char *name)
  {
   int found;

   ConstructData(theEnv,execStatus)->ListOfClearFunctions =
     RemoveFunctionFromCallList(theEnv,execStatus,name,ConstructData(theEnv,execStatus)->ListOfClearFunctions,&found);

   if (found) return(TRUE);

   return(FALSE);
  }

/***********************************************/
/* ExecutingConstruct: Returns TRUE if a */
/*   construct is currently being executed,    */
/*   otherwise FALSE.                    */
/***********************************************/
globle int ExecutingConstruct(
  void *theEnv,
  EXEC_STATUS)
  {
   return(ConstructData(theEnv,execStatus)->Executing); 
  }

/********************************************/
/* SetExecutingConstruct: Sets the value of */
/*   the executing variable indicating that */
/*   actions such as reset, clear, etc      */
/*   should not be performed.               */
/********************************************/
globle void SetExecutingConstruct(
  void *theEnv,
  EXEC_STATUS,
  int value)
  {
   ConstructData(theEnv,execStatus)->Executing = value;
  }

/************************************************************/
/* OldGetConstructList: Returns a list of all the construct */
/*   names in a multifield value. It doesn't check the      */
/*   number of arguments. It assumes that the restriction   */
/*   string in DefineFunction2 call was "00".               */
/************************************************************/
globle void OldGetConstructList(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT_PTR returnValue,
  void *(*nextFunction)(void *,EXEC_STATUS,void *),
  char *(*nameFunction)(void *,EXEC_STATUS,void *))
  {
   void *theConstruct;
   unsigned long count = 0;
   struct multifield *theList;

   /*====================================*/
   /* Determine the number of constructs */
   /* of the specified type.             */
   /*====================================*/

   for (theConstruct = (*nextFunction)(theEnv,execStatus,NULL);
        theConstruct != NULL;
        theConstruct = (*nextFunction)(theEnv,execStatus,theConstruct))
     { count++; }

   /*===========================*/
   /* Create a multifield large */
   /* enough to store the list. */
   /*===========================*/

   SetpType(returnValue,MULTIFIELD);
   SetpDOBegin(returnValue,1);
   SetpDOEnd(returnValue,(long) count);
   theList = (struct multifield *) EnvCreateMultifield(theEnv,execStatus,count);
   SetpValue(returnValue,(void *) theList);

   /*====================================*/
   /* Store the names in the multifield. */
   /*====================================*/

   for (theConstruct = (*nextFunction)(theEnv,execStatus,NULL), count = 1;
        theConstruct != NULL;
        theConstruct = (*nextFunction)(theEnv,execStatus,theConstruct), count++)
     {
      if (execStatus->HaltExecution == TRUE)
        {
         EnvSetMultifieldErrorValue(theEnv,execStatus,returnValue);
         return;
        }
      SetMFType(theList,count,SYMBOL);
      SetMFValue(theList,count,EnvAddSymbol(theEnv,execStatus,(*nameFunction)(theEnv,execStatus,theConstruct)));
     }
  }

/*******************************************************/
/* DeinstallConstructHeader: Decrements the busy count */
/*   of a construct name and frees its pretty print    */
/*   representation string (both of which are stored   */
/*   in the generic construct header).                 */
/*******************************************************/
globle void DeinstallConstructHeader(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theHeader)
  {
   DecrementSymbolCount(theEnv,execStatus,theHeader->name);
   if (theHeader->ppForm != NULL)
     {
      rm(theEnv,execStatus,theHeader->ppForm,
         sizeof(char) * (strlen(theHeader->ppForm) + 1));
      theHeader->ppForm = NULL;
     }

   if (theHeader->usrData != NULL)
     {
      ClearUserDataList(theEnv,execStatus,theHeader->usrData);
      theHeader->usrData = NULL;
     }
  }

/**************************************************/
/* DestroyConstructHeader: Frees the pretty print */
/*   representation string and user data (both of */
/*   which are stored in the generic construct    */
/*   header).                                     */
/**************************************************/
globle void DestroyConstructHeader(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theHeader)
  {
   if (theHeader->ppForm != NULL)
     {
      rm(theEnv,execStatus,theHeader->ppForm,
         sizeof(char) * (strlen(theHeader->ppForm) + 1));
      theHeader->ppForm = NULL;
     }

   if (theHeader->usrData != NULL)
     {
      ClearUserDataList(theEnv,execStatus,theHeader->usrData);
      theHeader->usrData = NULL;
     }
  }

/*****************************************************/
/* AddConstruct: Adds a construct and its associated */
/*   parsing function to the ListOfConstructs.       */
/*****************************************************/
globle struct construct *AddConstruct(
  void *theEnv,
  EXEC_STATUS,
  char *name,
  char *pluralName,
  int (*parseFunction)(void *,EXEC_STATUS,char *),
  void *(*findFunction)(void *,EXEC_STATUS,char *),
  SYMBOL_HN *(*getConstructNameFunction)(struct constructHeader *),
  char *(*getPPFormFunction)(void *,EXEC_STATUS,struct constructHeader *),
  struct defmoduleItemHeader *(*getModuleItemFunction)(struct constructHeader *),
  void *(*getNextItemFunction)(void *,EXEC_STATUS,void *),
  void (*setNextItemFunction)(struct constructHeader *,struct constructHeader *),
  intBool (*isConstructDeletableFunction)(void *,EXEC_STATUS,void *),
  int (*deleteFunction)(void *,EXEC_STATUS,void *),
  void (*freeFunction)(void *,EXEC_STATUS,void *))
  {
   struct construct *newPtr;

   /*=============================*/
   /* Allocate and initialize the */
   /* construct data structure.   */
   /*=============================*/

   newPtr = get_struct(theEnv,execStatus,construct);

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

   newPtr->next = ConstructData(theEnv,execStatus)->ListOfConstructs;
   ConstructData(theEnv,execStatus)->ListOfConstructs = newPtr;
   return(newPtr);
  }

/************************************/
/* AddSaveFunction: Adds a function */
/*   to the ListOfSaveFunctions.    */
/************************************/
globle intBool AddSaveFunction(
  void *theEnv,
  EXEC_STATUS,
  char *name,
  void (*functionPtr)(void *,EXEC_STATUS,void *,char *),
  int priority)
  {
#if (MAC_MCW || WIN_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(name)
#pragma unused(functionPtr)
#pragma unused(priority)
#endif

#if (! RUN_TIME) && (! BLOAD_ONLY)
   ConstructData(theEnv,execStatus)->ListOfSaveFunctions =
     AddFunctionToCallList(theEnv,execStatus,name,priority,
                           (void (*)(void *)) functionPtr,
                           ConstructData(theEnv,execStatus)->ListOfSaveFunctions,TRUE);
#else
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv,execStatus)
#endif
#endif

   return(1);
  }

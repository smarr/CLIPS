   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*                    WATCH MODULE                     */
   /*******************************************************/


/*************************************************************/
/* Purpose: Support functions for the watch and unwatch      */
/*   commands.                                               */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian Dantes                                         */
/*                                                           */
/* Revision History:                                         */
/*      6.23: Changed name of variable log to logName        */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added EnvSetWatchItem function.                */
/*                                                           */
/*************************************************************/

#define _WATCH_SOURCE_

#include "setup.h"

#if DEBUGGING_FUNCTIONS

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include "constant.h"
#include "envrnmnt.h"
#include "memalloc.h"
#include "router.h"
#include "argacces.h"
#include "extnfunc.h"
#include "watch.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static struct watchItem       *ValidWatchItem(void *,EXEC_STATUS,char *,int *);
   static intBool                 RecognizeWatchRouters(void *,EXEC_STATUS,char *);
   static int                     CaptureWatchPrints(void *,EXEC_STATUS,char *,char *);
   static void                    DeallocateWatchData(void *,EXEC_STATUS);

/**********************************************/
/* InitializeWatchData: Allocates environment */
/*    data for watch items.                   */
/**********************************************/
globle void InitializeWatchData(
  void *theEnv,
  EXEC_STATUS)
  {
   AllocateEnvironmentData(theEnv,execStatus,WATCH_DATA,sizeof(struct watchData),DeallocateWatchData);
  }
  
/************************************************/
/* DeallocateWatchData: Deallocates environment */
/*    data for watch items.                     */
/************************************************/
static void DeallocateWatchData(
  void *theEnv,
  EXEC_STATUS)
  {
   struct watchItem *tmpPtr, *nextPtr;

   tmpPtr = WatchData(theEnv,execStatus)->ListOfWatchItems;
   while (tmpPtr != NULL)
     {
      nextPtr = tmpPtr->next;
      rtn_struct(theEnv,execStatus,watchItem,tmpPtr);
      tmpPtr = nextPtr;
     }
  }

/*************************************************************/
/* AddWatchItem: Adds an item to the list of watchable items */
/*   that can be set using the watch and unwatch commands.   */
/*   Returns FALSE if the item is already in the list,       */
/*   otherwise returns TRUE.                                 */
/*************************************************************/
globle intBool AddWatchItem(
  void *theEnv,
  EXEC_STATUS,
  char *name,
  int code,
  unsigned *flag,
  int priority,
  unsigned (*accessFunc)(void *,EXEC_STATUS,int,unsigned,struct expr *),
  unsigned (*printFunc)(void *,EXEC_STATUS,char *,int,struct expr *))
  {
   struct watchItem *newPtr, *currentPtr, *lastPtr;

   /*================================================================*/
   /* Find the insertion point in the watchable items list to place  */
   /* the new item. If the item is already in the list return FALSE. */
   /*================================================================*/

   for (currentPtr = WatchData(theEnv,execStatus)->ListOfWatchItems, lastPtr = NULL;
        currentPtr != NULL;
        currentPtr = currentPtr->next)
     {
      if (strcmp(currentPtr->name,name) == 0) return(FALSE);
      if (priority < currentPtr->priority) lastPtr = currentPtr;
     }

   /*============================*/
   /* Create the new watch item. */
   /*============================*/

   newPtr = get_struct(theEnv,execStatus,watchItem);
   newPtr->name = name;
   newPtr->flag = flag;
   newPtr->code = code;
   newPtr->priority = priority;
   newPtr->accessFunc = accessFunc;
   newPtr->printFunc = printFunc;

   /*=================================================*/
   /* Insert the new item in the list of watch items. */
   /*=================================================*/

   if (lastPtr == NULL)
     {
      newPtr->next = WatchData(theEnv,execStatus)->ListOfWatchItems;
      WatchData(theEnv,execStatus)->ListOfWatchItems = newPtr;
     }
   else
     {
      newPtr->next = lastPtr->next;
      lastPtr->next = newPtr;
     }

   /*==================================================*/
   /* Return TRUE to indicate the item has been added. */
   /*==================================================*/

   return(TRUE);
  }

/*****************************************************/
/* EnvWatch: C access routine for the watch command. */
/*****************************************************/
globle intBool EnvWatch(
  void *theEnv,
  EXEC_STATUS,
  char *itemName)
  {
   return(EnvSetWatchItem(theEnv,execStatus,itemName,ON,NULL));
  }

/**************************************************/
/* Watch: C access routine for the watch command. */
/**************************************************/
#if ALLOW_ENVIRONMENT_GLOBALS
globle intBool Watch(
  char *itemName)
  {
   return(EnvWatch(GetCurrentEnvironment(),itemName));
  }
#endif

/*********************************************************/
/* EnvUnwatch: C access routine for the unwatch command. */
/*********************************************************/
globle intBool EnvUnwatch(
  void *theEnv,
  EXEC_STATUS,
  char *itemName)
  {
   return(EnvSetWatchItem(theEnv,execStatus,itemName,OFF,NULL));
  }

/******************************************************/
/* Unwatch: C access routine for the unwatch command. */
/******************************************************/
#if ALLOW_ENVIRONMENT_GLOBALS
globle intBool Unwatch(
  char *itemName)
  {
   return(EnvUnwatch(GetCurrentEnvironment(),itemName));
  }
#endif

/***********************************************************************/
/* EnvSetWatchItem: Sets the state of a specified watch item to either */
/*   on or off. Returns TRUE if the item was set, otherwise FALSE.     */
/***********************************************************************/
globle int EnvSetWatchItem(
  void *theEnv,
  EXEC_STATUS,
  char *itemName,
  unsigned newState,
  struct expr *argExprs)
  {
   struct watchItem *wPtr;

   /*======================================================*/
   /* If the new state isn't on or off, then return FALSE. */
   /*======================================================*/

   if ((newState != ON) && (newState != OFF)) return(FALSE);

   /*===================================================*/
   /* If the name of the watch item to set is all, then */
   /* all watch items are set to the new state and TRUE */
   /* is returned.                                      */
   /*===================================================*/

   if (strcmp(itemName,"all") == 0)
     {
      for (wPtr = WatchData(theEnv,execStatus)->ListOfWatchItems; wPtr != NULL; wPtr = wPtr->next)
        {
         /*==============================================*/
         /* If no specific arguments are specified, then */
         /* set the global flag for the watch item.      */
         /*==============================================*/

         if (argExprs == NULL) *(wPtr->flag) = newState;

         /*=======================================*/
         /* Set flags for individual watch items. */
         /*=======================================*/

         if ((wPtr->accessFunc == NULL) ? FALSE :
             ((*wPtr->accessFunc)(theEnv,execStatus,wPtr->code,newState,argExprs) == FALSE))
           {
            SetEvaluationError(theEnv,execStatus,TRUE);
            return(FALSE);
           }
        }
      return(TRUE);
     }

   /*=================================================*/
   /* Search for the watch item to be set in the list */
   /* of watch items. If found, set the watch item to */
   /* its new state and return TRUE.                  */
   /*=================================================*/

   for (wPtr = WatchData(theEnv,execStatus)->ListOfWatchItems; wPtr != NULL; wPtr = wPtr->next)
     {
      if (strcmp(itemName,wPtr->name) == 0)
        {
         /*==============================================*/
         /* If no specific arguments are specified, then */
         /* set the global flag for the watch item.      */
         /*==============================================*/

         if (argExprs == NULL) *(wPtr->flag) = newState;

         /*=======================================*/
         /* Set flags for individual watch items. */
         /*=======================================*/

         if ((wPtr->accessFunc == NULL) ? FALSE :
             ((*wPtr->accessFunc)(theEnv,execStatus,wPtr->code,newState,argExprs) == FALSE))
           {
            SetEvaluationError(theEnv,execStatus,TRUE);
            return(FALSE);
           }

         return(TRUE);
        }
     }

   /*=================================================*/
   /* If the specified item was not found in the list */
   /* of watchable items then return FALSE.           */
   /*=================================================*/

   return(FALSE);
  }

/******************************************************************/
/* EnvGetWatchItem: Gets the current state of the specified watch */
/*   item. Returns the state of the watch item (0 for off and 1   */
/*   for on) if the watch item is found in the list of watch      */
/*   items, otherwise -1 is returned.                             */
/******************************************************************/
globle int EnvGetWatchItem(
  void *theEnv,
  EXEC_STATUS,
  char *itemName)
  {
   struct watchItem *wPtr;

   for (wPtr = WatchData(theEnv,execStatus)->ListOfWatchItems; wPtr != NULL; wPtr = wPtr->next)
     { 
      if (strcmp(itemName,wPtr->name) == 0) 
        { return((int) *(wPtr->flag)); }
     }

   return(-1);
  }

/****************************************************************/
/* ValidWatchItem: Returns TRUE if the specified name is found  */
/*   in the list of watch items, otherwise returns FALSE.       */
/****************************************************************/
static struct watchItem *ValidWatchItem(
  void *theEnv,
  EXEC_STATUS,
  char *itemName,
  int *recognized)
  {
   struct watchItem *wPtr;

   *recognized = TRUE;
   if (strcmp(itemName,"all") == 0)
     return(NULL);

   for (wPtr = WatchData(theEnv,execStatus)->ListOfWatchItems; wPtr != NULL; wPtr = wPtr->next)
     { if (strcmp(itemName,wPtr->name) == 0) return(wPtr); }

   *recognized = FALSE;
   return(NULL);
  }

/*************************************************************/
/* GetNthWatchName: Returns the name associated with the nth */
/*   item in the list of watchable items. If the nth item    */
/*   does not exist, then NULL is returned.                  */
/*************************************************************/
globle char *GetNthWatchName(
  void *theEnv,
  EXEC_STATUS,
  int whichItem)
  {
   int i;
   struct watchItem *wPtr;

   for (wPtr = WatchData(theEnv,execStatus)->ListOfWatchItems, i = 1;
        wPtr != NULL;
        wPtr = wPtr->next, i++)
     { if (i == whichItem) return(wPtr->name); }

   return(NULL);
  }

/***************************************************************/
/* GetNthWatchValue: Returns the current state associated with */
/*   the nth item in the list of watchable items. If the nth   */
/*   item does not exist, then -1 is returned.                 */
/***************************************************************/
globle int GetNthWatchValue(
  void *theEnv,
  EXEC_STATUS,
  int whichItem)
  {
   int i;
   struct watchItem *wPtr;

   for (wPtr = WatchData(theEnv,execStatus)->ListOfWatchItems, i = 1;
        wPtr != NULL;
        wPtr = wPtr->next, i++)
     { if (i == whichItem) return((int) *(wPtr->flag)); }

   return(-1);
  }

/**************************************/
/* WatchCommand: H/L access routine   */
/*   for the watch command.           */
/**************************************/
globle void WatchCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   DATA_OBJECT theValue;
   char *argument;
   int recognized;
   struct watchItem *wPtr;

   /*========================================*/
   /* Determine which item is to be watched. */
   /*========================================*/

   if (EnvArgTypeCheck(theEnv,execStatus,"watch",1,SYMBOL,&theValue) == FALSE) return;
   argument = DOToString(theValue);
   wPtr = ValidWatchItem(theEnv,execStatus,argument,&recognized);
   if (recognized == FALSE)
     {
      SetEvaluationError(theEnv,execStatus,TRUE);
      ExpectedTypeError1(theEnv,execStatus,"watch",1,"watchable symbol");
      return;
     }

   /*=================================================*/
   /* Check to make sure extra arguments are allowed. */
   /*=================================================*/

   if (GetNextArgument(GetFirstArgument()) != NULL)
     {
      if ((wPtr == NULL) ? TRUE : (wPtr->accessFunc == NULL))
        {
         SetEvaluationError(theEnv,execStatus,TRUE);
         ExpectedCountError(theEnv,execStatus,"watch",EXACTLY,1);
         return;
        }
     }

   /*=====================*/
   /* Set the watch item. */
   /*=====================*/

   EnvSetWatchItem(theEnv,execStatus,argument,ON,GetNextArgument(GetFirstArgument()));
  }

/****************************************/
/* UnwatchCommand: H/L access routine   */
/*   for the unwatch command.           */
/****************************************/
globle void UnwatchCommand(
  void *theEnv,
	EXEC_STATUS)
  {
   DATA_OBJECT theValue;
   char *argument;
   int recognized;
   struct watchItem *wPtr;

   /*==========================================*/
   /* Determine which item is to be unwatched. */
   /*==========================================*/

   if (EnvArgTypeCheck(theEnv,execStatus,"unwatch",1,SYMBOL,&theValue) == FALSE) return;
   argument = DOToString(theValue);
   wPtr = ValidWatchItem(theEnv,execStatus,argument,&recognized);
   if (recognized == FALSE)
     {
      SetEvaluationError(theEnv,execStatus,TRUE);
      ExpectedTypeError1(theEnv,execStatus,"unwatch",1,"watchable symbol");
      return;
     }

   /*=================================================*/
   /* Check to make sure extra arguments are allowed. */
   /*=================================================*/

   if (GetNextArgument(GetFirstArgument()) != NULL)
     {
      if ((wPtr == NULL) ? TRUE : (wPtr->accessFunc == NULL))
        {
         SetEvaluationError(theEnv,execStatus,TRUE);
         ExpectedCountError(theEnv,execStatus,"unwatch",EXACTLY,1);
         return;
        }
     }

   /*=====================*/
   /* Set the watch item. */
   /*=====================*/

   EnvSetWatchItem(theEnv,execStatus,argument,OFF,GetNextArgument(GetFirstArgument()));
  }

/************************************************/
/* ListWatchItemsCommand: H/L access routines   */
/*   for the list-watch-items command.          */
/************************************************/
globle void ListWatchItemsCommand(
  void *theEnv,
	EXEC_STATUS)
  {
   struct watchItem *wPtr;
   DATA_OBJECT theValue;
   int recognized;

   /*=======================*/
   /* List the watch items. */
   /*=======================*/

   if (GetFirstArgument() == NULL)
     {
      for (wPtr = WatchData(theEnv,execStatus)->ListOfWatchItems; wPtr != NULL; wPtr = wPtr->next)
        {
         EnvPrintRouter(theEnv,execStatus,WDISPLAY,wPtr->name);
         if (*(wPtr->flag)) EnvPrintRouter(theEnv,execStatus,WDISPLAY," = on\n");
         else EnvPrintRouter(theEnv,execStatus,WDISPLAY," = off\n");
        }
      return;
     }

   /*=======================================*/
   /* Determine which item is to be listed. */
   /*=======================================*/

   if (EnvArgTypeCheck(theEnv,execStatus,"list-watch-items",1,SYMBOL,&theValue) == FALSE) return;
   wPtr = ValidWatchItem(theEnv,execStatus,DOToString(theValue),&recognized);
   if ((recognized == FALSE) || (wPtr == NULL))
     {
      SetEvaluationError(theEnv,execStatus,TRUE);
      ExpectedTypeError1(theEnv,execStatus,"list-watch-items",1,"watchable symbol");
      return;
     }

   /*=================================================*/
   /* Check to make sure extra arguments are allowed. */
   /*=================================================*/

   if ((wPtr->printFunc == NULL) &&
       (GetNextArgument(GetFirstArgument()) != NULL))
     {
      SetEvaluationError(theEnv,execStatus,TRUE);
      ExpectedCountError(theEnv,execStatus,"list-watch-items",EXACTLY,1);
      return;
     }

   /*====================================*/
   /* List the status of the watch item. */
   /*====================================*/

   EnvPrintRouter(theEnv,execStatus,WDISPLAY,wPtr->name);
   if (*(wPtr->flag)) EnvPrintRouter(theEnv,execStatus,WDISPLAY," = on\n");
   else EnvPrintRouter(theEnv,execStatus,WDISPLAY," = off\n");

   /*============================================*/
   /* List the status of individual watch items. */
   /*============================================*/

   if (wPtr->printFunc != NULL)
     {
      if ((*wPtr->printFunc)(theEnv,execStatus,WDISPLAY,wPtr->code,
                             GetNextArgument(GetFirstArgument())) == FALSE)
        { SetEvaluationError(theEnv,execStatus,TRUE); }
     }
  }

/*******************************************/
/* GetWatchItemCommand: H/L access routine */
/*   for the get-watch-item command.       */
/*******************************************/
globle int GetWatchItemCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   DATA_OBJECT theValue;
   char *argument;
   int recognized;

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/

   if (EnvArgCountCheck(theEnv,execStatus,"get-watch-item",EXACTLY,1) == -1)
     { return(FALSE); }

   /*========================================*/
   /* Determine which item is to be watched. */
   /*========================================*/

   if (EnvArgTypeCheck(theEnv,execStatus,"get-watch-item",1,SYMBOL,&theValue) == FALSE)
     { return(FALSE); }

   argument = DOToString(theValue);
   ValidWatchItem(theEnv,execStatus,argument,&recognized);
   if (recognized == FALSE)
     {
      SetEvaluationError(theEnv,execStatus,TRUE);
      ExpectedTypeError1(theEnv,execStatus,"get-watch-item",1,"watchable symbol");
      return(FALSE);
     }

   /*===========================*/
   /* Get the watch item value. */
   /*===========================*/

   if (EnvGetWatchItem(theEnv,execStatus,argument) == 1)
     { return(TRUE); }

   return(FALSE);
  }

/*************************************************************/
/* WatchFunctionDefinitions: Initializes the watch commands. */
/*************************************************************/
globle void WatchFunctionDefinitions(
  void *theEnv,
  EXEC_STATUS)
  {
#if ! RUN_TIME
   EnvDefineFunction2(theEnv,execStatus,"watch",   'v', PTIEF WatchCommand,   "WatchCommand", "1**w");
   EnvDefineFunction2(theEnv,execStatus,"unwatch", 'v', PTIEF UnwatchCommand, "UnwatchCommand", "1**w");
   EnvDefineFunction2(theEnv,execStatus,"get-watch-item", 'b', PTIEF GetWatchItemCommand,   "GetWatchItemCommand", "11w");
   EnvDefineFunction2(theEnv,execStatus,"list-watch-items", 'v', PTIEF ListWatchItemsCommand,
                   "ListWatchItemsCommand", "0**w");
#endif

   EnvAddRouter(theEnv,execStatus,WTRACE,1000,RecognizeWatchRouters,CaptureWatchPrints,NULL,NULL,NULL);
   EnvDeactivateRouter(theEnv,execStatus,WTRACE);
  }

/**************************************************/
/* RecognizeWatchRouters: Looks for WTRACE prints */
/**************************************************/
#if WIN_BTC
#pragma argsused
#endif
static intBool RecognizeWatchRouters(
  void *theEnv,
  EXEC_STATUS,
  char *logName)
  {
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv,execStatus)
#endif

   if (strcmp(logName,WTRACE) == 0) return(TRUE);

   return(FALSE);
  }

/**************************************************/
/* CaptureWatchPrints: Suppresses WTRACE messages */
/**************************************************/
#if WIN_BTC
#pragma argsused
#endif
static int CaptureWatchPrints(
  void *theEnv,
  EXEC_STATUS,
  char *logName,
  char *str)
  {
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(logName)
#pragma unused(str)
#pragma unused(theEnv,execStatus)
#endif
   return(1);
  }

#endif /* DEBUGGING_FUNCTIONS */


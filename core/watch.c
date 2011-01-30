   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
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
/*      Brian Donnell                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _WATCH_SOURCE_

#include "setup.h"

#if DEBUGGING_FUNCTIONS

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include "constant.h"
#include "memalloc.h"
#include "router.h"
#include "argacces.h"
#include "extnfunc.h"
#include "watch.h"

/*************************/
/* STRUCTURE DEFINITIONS */
/*************************/

struct watchItem
  {
   char *name;
   int *flag;
   int code,priority;
   BOOLEAN (*accessFunc)(int,int,struct expr *);
   BOOLEAN (*printFunc)(char *,int,struct expr *);
   struct watchItem *next;
  };

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static struct watchItem       *ValidWatchItem(char *,int *);
   static BOOLEAN                 RecognizeWatchRouters(char *);
   static int                     CaptureWatchPrints(char *,char *);

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static struct watchItem      *ListOfWatchItems = NULL;

/*************************************************************/
/* AddWatchItem: Adds an item to the list of watchable items */
/*   that can be set using the watch and unwatch commands.   */
/*   Returns FALSE if the item is already in the list,       */
/*   otherwise returns TRUE.                                 */
/*************************************************************/
globle BOOLEAN AddWatchItem(
  char *name,
  int code,
  int *flag,
  int priority,
  BOOLEAN (*accessFunc)(int,int,struct expr *),
  BOOLEAN (*printFunc)(char *,int,struct expr *))
  {
   struct watchItem *newPtr, *currentPtr, *lastPtr;

   /*================================================================*/
   /* Find the insertion point in the watchable items list to place  */
   /* the new item. If the item is already in the list return FALSE. */
   /*================================================================*/

   for (currentPtr = ListOfWatchItems, lastPtr = NULL;
        currentPtr != NULL;
        currentPtr = currentPtr->next)
     {
      if (strcmp(currentPtr->name,name) == 0) return(FALSE);
      if (priority < currentPtr->priority) lastPtr = currentPtr;
     }

   /*============================*/
   /* Create the new watch item. */
   /*============================*/

   newPtr = get_struct(watchItem);
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
      newPtr->next = ListOfWatchItems;
      ListOfWatchItems = newPtr;
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

/**************************************************/
/* Watch: C access routine for the watch command. */
/**************************************************/
globle BOOLEAN Watch(
  char *itemName)
  {
   return(SetWatchItem(itemName,ON,NULL));
  }

/******************************************************/
/* Unwatch: C access routine for the unwatch command. */
/******************************************************/
globle BOOLEAN Unwatch(
  char *itemName)
  {
   return(SetWatchItem(itemName,OFF,NULL));
  }

/********************************************************************/
/* SetWatchItem: Sets the state of a specified watch item to either */
/*   on or off. Returns TRUE if the item was set, otherwise FALSE.  */
/********************************************************************/
globle BOOLEAN SetWatchItem(
  char *itemName,
  int newState,
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
      for (wPtr = ListOfWatchItems; wPtr != NULL; wPtr = wPtr->next)
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
             ((*wPtr->accessFunc)(wPtr->code,newState,argExprs) == FALSE))
           {
            SetEvaluationError(TRUE);
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

   for (wPtr = ListOfWatchItems; wPtr != NULL; wPtr = wPtr->next)
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
             ((*wPtr->accessFunc)(wPtr->code,newState,argExprs) == FALSE))
           {
            SetEvaluationError(TRUE);
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

/*********************************************************************/
/* GetWatchItem: Gets the current state of the specified watch item. */
/*   Returns the state of the watch item (0 for off and 1 for on) if */
/*   the watch item is found in the list of watch items, otherwise   */
/*   -1 is returned.                                                 */
/*********************************************************************/
globle int GetWatchItem(
  char *itemName)
  {
   struct watchItem *wPtr;

   for (wPtr = ListOfWatchItems; wPtr != NULL; wPtr = wPtr->next)
     { if (strcmp(itemName,wPtr->name) == 0) return(*(wPtr->flag)); }

   return(-1);
  }

/****************************************************************/
/* ValidWatchItem: Returns TRUE if the specified name is found  */
/*   in the list of watch items, otherwise returns FALSE.       */
/****************************************************************/
static struct watchItem *ValidWatchItem(
  char *itemName,
  int *recognized)
  {
   struct watchItem *wPtr;

   *recognized = TRUE;
   if (strcmp(itemName,"all") == 0)
     return(NULL);

   for (wPtr = ListOfWatchItems; wPtr != NULL; wPtr = wPtr->next)
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
  int whichItem)
  {
   int i;
   struct watchItem *wPtr;

   for (wPtr = ListOfWatchItems, i = 1;
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
  int whichItem)
  {
   int i;
   struct watchItem *wPtr;

   for (wPtr = ListOfWatchItems, i = 1;
        wPtr != NULL;
        wPtr = wPtr->next, i++)
     { if (i == whichItem) return(*(wPtr->flag)); }

   return(-1);
  }

/**************************************/
/* WatchCommand: H/L access routine   */
/*   for the watch command.           */
/**************************************/
globle void WatchCommand()
  {
   DATA_OBJECT theValue;
   char *argument;
   int recognized;
   struct watchItem *wPtr;

   /*========================================*/
   /* Determine which item is to be watched. */
   /*========================================*/

   if (ArgTypeCheck("watch",1,SYMBOL,&theValue) == FALSE) return;
   argument = DOToString(theValue);
   wPtr = ValidWatchItem(argument,&recognized);
   if (recognized == FALSE)
     {
      SetEvaluationError(TRUE);
      ExpectedTypeError1("watch",1,"watchable symbol");
      return;
     }

   /*=================================================*/
   /* Check to make sure extra arguments are allowed. */
   /*=================================================*/

   if (GetNextArgument(GetFirstArgument()) != NULL)
     {
      if ((wPtr == NULL) ? TRUE : (wPtr->accessFunc == NULL))
        {
         SetEvaluationError(TRUE);
         ExpectedCountError("watch",EXACTLY,1);
         return;
        }
     }

   /*=====================*/
   /* Set the watch item. */
   /*=====================*/

   SetWatchItem(argument,ON,GetNextArgument(GetFirstArgument()));
  }

/****************************************/
/* UnwatchCommand: H/L access routine   */
/*   for the unwatch command.           */
/****************************************/
globle void UnwatchCommand()
  {
   DATA_OBJECT theValue;
   char *argument;
   int recognized;
   struct watchItem *wPtr;

   /*==========================================*/
   /* Determine which item is to be unwatched. */
   /*==========================================*/

   if (ArgTypeCheck("unwatch",1,SYMBOL,&theValue) == FALSE) return;
   argument = DOToString(theValue);
   wPtr = ValidWatchItem(argument,&recognized);
   if (recognized == FALSE)
     {
      SetEvaluationError(TRUE);
      ExpectedTypeError1("unwatch",1,"watchable symbol");
      return;
     }

   /*=================================================*/
   /* Check to make sure extra arguments are allowed. */
   /*=================================================*/

   if (GetNextArgument(GetFirstArgument()) != NULL)
     {
      if ((wPtr == NULL) ? TRUE : (wPtr->accessFunc == NULL))
        {
         SetEvaluationError(TRUE);
         ExpectedCountError("unwatch",EXACTLY,1);
         return;
        }
     }

   /*=====================*/
   /* Set the watch item. */
   /*=====================*/

   SetWatchItem(argument,OFF,GetNextArgument(GetFirstArgument()));
  }

/************************************************/
/* ListWatchItemsCommand: H/L access routines   */
/*   for the list-watch-items command.          */
/************************************************/
globle void ListWatchItemsCommand()
  {
   struct watchItem *wPtr;
   DATA_OBJECT theValue;
   int recognized;

   /*=======================*/
   /* List the watch items. */
   /*=======================*/

   if (GetFirstArgument() == NULL)
     {
      for (wPtr = ListOfWatchItems; wPtr != NULL; wPtr = wPtr->next)
        {
         PrintRouter(WDISPLAY,wPtr->name);
         if (*(wPtr->flag)) PrintRouter(WDISPLAY," = on\n");
         else PrintRouter(WDISPLAY," = off\n");
        }
      return;
     }

   /*=======================================*/
   /* Determine which item is to be listed. */
   /*=======================================*/

   if (ArgTypeCheck("list-watch-items",1,SYMBOL,&theValue) == FALSE) return;
   wPtr = ValidWatchItem(DOToString(theValue),&recognized);
   if ((recognized == FALSE) || (wPtr == NULL))
     {
      SetEvaluationError(TRUE);
      ExpectedTypeError1("list-watch-items",1,"watchable symbol");
      return;
     }

   /*=================================================*/
   /* Check to make sure extra arguments are allowed. */
   /*=================================================*/

   if ((wPtr->printFunc == NULL) &&
       (GetNextArgument(GetFirstArgument()) != NULL))
     {
      SetEvaluationError(TRUE);
      ExpectedCountError("list-watch-items",EXACTLY,1);
      return;
     }

   /*====================================*/
   /* List the status of the watch item. */
   /*====================================*/

   PrintRouter(WDISPLAY,wPtr->name);
   if (*(wPtr->flag)) PrintRouter(WDISPLAY," = on\n");
   else PrintRouter(WDISPLAY," = off\n");

   /*============================================*/
   /* List the status of individual watch items. */
   /*============================================*/

   if (wPtr->printFunc != NULL)
     {
      if ((*wPtr->printFunc)(WDISPLAY,wPtr->code,
                             GetNextArgument(GetFirstArgument())) == FALSE)
        { SetEvaluationError(TRUE); }
     }
  }

/*******************************************/
/* GetWatchItemCommand: H/L access routine */
/*   for the get-watch-item command.       */
/*******************************************/
globle int GetWatchItemCommand()
  {
   DATA_OBJECT theValue;
   char *argument;
   int recognized;

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/

   if (ArgCountCheck("get-watch-item",EXACTLY,1) == -1)
     { return(FALSE); }

   /*========================================*/
   /* Determine which item is to be watched. */
   /*========================================*/

   if (ArgTypeCheck("get-watch-item",1,SYMBOL,&theValue) == FALSE)
     { return(FALSE); }

   argument = DOToString(theValue);
   ValidWatchItem(argument,&recognized);
   if (recognized == FALSE)
     {
      SetEvaluationError(TRUE);
      ExpectedTypeError1("get-watch-item",1,"watchable symbol");
      return(FALSE);
     }

   /*===========================*/
   /* Get the watch item value. */
   /*===========================*/

   if (GetWatchItem(argument) == 1)
     { return(TRUE); }

   return(FALSE);
  }

/*************************************************************/
/* WatchFunctionDefinitions: Initializes the watch commands. */
/*************************************************************/
globle void WatchFunctionDefinitions()
  {
#if ! RUN_TIME
   DefineFunction2("watch",   'v', PTIF WatchCommand,   "WatchCommand", "1**w");
   DefineFunction2("unwatch", 'v', PTIF UnwatchCommand, "UnwatchCommand", "1**w");
   DefineFunction2("get-watch-item", 'b', PTIF GetWatchItemCommand,   "GetWatchItemCommand", "11w");
   DefineFunction2("list-watch-items", 'v', PTIF ListWatchItemsCommand,
                   "ListWatchItemsCommand", "0**w");
#endif

   AddRouter(WTRACE,1000,RecognizeWatchRouters,CaptureWatchPrints,NULL,NULL,NULL);
   DeactivateRouter(WTRACE);
  }

/**************************************************/
/* RecognizeWatchRouters: Looks for WTRACE prints */
/**************************************************/
static BOOLEAN RecognizeWatchRouters(
  char *log)
  {
   if (strcmp(log,WTRACE) == 0) return(TRUE);

   return(FALSE);
  }

/**************************************************/
/* CaptureWatchPrints: Suppresses WTRACE messages */
/**************************************************/
#if IBM_TBC
#pragma argsused
#endif
static int CaptureWatchPrints(
  char *log,
  char *str)
  {
#if MAC_MPW || MAC_MCW || IBM_MCW
#pragma unused(log)
#pragma unused(str)
#endif
   return(1);
  }

#endif /* DEBUGGING_FUNCTIONS */


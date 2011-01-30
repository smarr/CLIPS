   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                   UTILITY MODULE                    */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides a set of utility functions useful to    */
/*   other modules. Primarily these are the functions for    */
/*   handling periodic garbage collection and appending      */
/*   string data.                                            */
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

#define _UTILITY_SOURCE_

#include "setup.h"

#include <ctype.h>
#include <stdlib.h>

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>
#include <math.h>

#include "evaluatn.h"
#include "facthsh.h"
#include "memalloc.h"
#include "multifld.h"
#include "prntutil.h"

#include "utility.h"

#define MAX_EPHEMERAL_COUNT 1000L
#define MAX_EPHEMERAL_SIZE 10240L
#define COUNT_INCREMENT 1000L
#define SIZE_INCREMENT 10240L

struct cleanupFunction
  {
   char *name;
   void (*ip)(void);
   int priority;
   struct cleanupFunction *next;
  };

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static BOOLEAN                 AddCPFunction(char *,void (*)(void),int,struct cleanupFunction **);
   static BOOLEAN                 RemoveCPFunction(char *,struct cleanupFunction **);

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static struct cleanupFunction   *ListOfCleanupFunctions = NULL;
   Thread static struct cleanupFunction   *ListOfPeriodicFunctions = NULL;

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle unsigned long          EphemeralItemCount = 0;
   Thread globle unsigned long          EphemeralItemSize = 0;
   Thread globle unsigned long          CurrentEphemeralCountMax = MAX_EPHEMERAL_COUNT;
   Thread globle unsigned long          CurrentEphemeralSizeMax = MAX_EPHEMERAL_SIZE;

   Thread globle void                 (*YieldTimeFunction)(void) = NULL;

/*************************************************************/
/* PeriodicCleanup: Returns garbage created during execution */
/*   that has not been returned to the memory pool yet. The  */
/*   cleanup is normally deferred so that an executing rule  */
/*   can still access these data structures. Always calls a  */
/*   series of functions that should be called periodically. */
/*   Usually used by interfaces to update displays.          */
/*************************************************************/
globle void PeriodicCleanup(
  BOOLEAN cleanupAllDepths,
  BOOLEAN useHeuristics)
  {
   int oldDepth = -1;
   struct cleanupFunction *cleanupPtr,*periodPtr;
   Thread static int lastEvaluationDepth = -1;

   /*=============================================*/
   /* Call functions for handling periodic tasks. */
   /*=============================================*/

   for (periodPtr = ListOfPeriodicFunctions;
        periodPtr != NULL;
        periodPtr = periodPtr->next)
     { (*periodPtr->ip)(); }

   /*===================================================*/
   /* If the last level we performed cleanup was deeper */
   /* than the current level, reset the values used by  */
   /* the heuristics to determine if garbage collection */
   /* should be performed. If the heuristic values had  */
   /* to be incremented because there was no garbage    */
   /* that could be cleaned up, we don't want to keep   */
   /* those same high values permanently so we reset    */
   /* them when we go back to a lower evaluation depth. */
   /*===================================================*/

   if (lastEvaluationDepth > CurrentEvaluationDepth)
     {
      lastEvaluationDepth = CurrentEvaluationDepth;
      CurrentEphemeralCountMax = MAX_EPHEMERAL_COUNT;
      CurrentEphemeralSizeMax = MAX_EPHEMERAL_SIZE;
     }

   /*======================================================*/
   /* If we're using heuristics to determine if garbage    */
   /* collection to occur, then check to see if enough     */
   /* garbage has been created to make cleanup worthwhile. */
   /*======================================================*/

   if (useHeuristics &&
       (EphemeralItemCount < CurrentEphemeralCountMax) &&
       (EphemeralItemSize < CurrentEphemeralSizeMax))
     { return; }

   /*==========================================================*/
   /* If cleanup is being performed at all depths, rather than */
   /* just the current evaluation depth, then temporarily set  */
   /* the evaluation depth to a level that will force cleanup  */
   /* at all depths.                                           */
   /*==========================================================*/

   if (cleanupAllDepths)
     {
      oldDepth = CurrentEvaluationDepth;
      CurrentEvaluationDepth = -1;
     }

   /*=============================================*/
   /* Free up multifield values no longer in use. */
   /*=============================================*/

   FlushMultifields();

   /*=====================================*/
   /* Call the list of cleanup functions. */
   /*=====================================*/

   for (cleanupPtr = ListOfCleanupFunctions;
        cleanupPtr != NULL;
        cleanupPtr = cleanupPtr->next)
     { (*cleanupPtr->ip)(); }

   /*================================================*/
   /* Free up atomic values that are no longer used. */
   /*================================================*/

   RemoveEphemeralAtoms();

   /*=========================================*/
   /* Restore the evaluation depth if cleanup */
   /* was performed on all depths.            */
   /*=========================================*/

   if (cleanupAllDepths) CurrentEvaluationDepth = oldDepth;

   /*============================================================*/
   /* If very little memory was freed up, then increment the     */
   /* values used by the heuristics so that we don't continually */
   /* try to free up memory that isn't being released.           */
   /*============================================================*/

   if ((EphemeralItemCount + COUNT_INCREMENT) > CurrentEphemeralCountMax)
     { CurrentEphemeralCountMax = EphemeralItemCount + COUNT_INCREMENT; }

   if ((EphemeralItemSize + SIZE_INCREMENT) > CurrentEphemeralSizeMax)
     { CurrentEphemeralSizeMax = EphemeralItemSize + SIZE_INCREMENT; }

   /*===============================================================*/
   /* Remember the evaluation depth at which garbage collection was */
   /* last performed. This information is used for resetting the    */
   /* ephemeral count and size numbers used by the heuristics.      */
   /*===============================================================*/

   lastEvaluationDepth = CurrentEvaluationDepth;
  }

/***************************************************/
/* AddCleanupFunction: Adds a function to the list */
/*   of functions called to perform cleanup such   */
/*   as returning free memory to the memory pool.  */
/***************************************************/
globle BOOLEAN AddCleanupFunction(
  char *name,
  void (*theFunction)(void),
  int priority)
  {
   return(AddCPFunction(name,theFunction,priority,&ListOfCleanupFunctions));
  }

/****************************************************/
/* AddPeriodicFunction: Adds a function to the list */
/*   of functions called to handle periodic tasks.  */
/****************************************************/
globle BOOLEAN AddPeriodicFunction(
  char *name,
  void (*theFunction)(void),
  int priority)
  {
   return(AddCPFunction(name,theFunction,priority,&ListOfPeriodicFunctions));
  }

/**********************************/
/* AddCPFunction: Adds a function */
/*   to a list of functions.      */
/**********************************/
static BOOLEAN AddCPFunction(
  char *name,
  void (*theFunction)(void),
  int priority,
  struct cleanupFunction **head)
  {
   struct cleanupFunction *newPtr, *currentPtr, *lastPtr = NULL;

   newPtr = get_struct(cleanupFunction);

   newPtr->name = name;
   newPtr->ip = theFunction;
   newPtr->priority = priority;

   if (*head == NULL)
     {
      newPtr->next = NULL;
      *head = newPtr;
      return(1);
     }

   currentPtr = *head;
   while ((currentPtr != NULL) ? (priority < currentPtr->priority) : FALSE)
     {
      lastPtr = currentPtr;
      currentPtr = currentPtr->next;
     }

   if (lastPtr == NULL)
     {
      newPtr->next = *head;
      *head = newPtr;
     }
   else
     {
      newPtr->next = currentPtr;
      lastPtr->next = newPtr;
     }

   return(1);
  }

/*******************************************************/
/* RemoveCleanupFunction: Removes a function from the  */
/*   list of functions called to perform cleanup such  */
/*   as returning free memory to the memory pool.      */
/*******************************************************/
globle BOOLEAN RemoveCleanupFunction(
  char *name)
  {
   return(RemoveCPFunction(name,&ListOfCleanupFunctions));
  }

/********************************************************/
/* RemovePeriodicFunction: Removes a function from the  */
/*   list of functions called to handle periodic tasks. */
/********************************************************/
globle BOOLEAN RemovePeriodicFunction(
  char *name)
  {
   return(RemoveCPFunction(name,&ListOfPeriodicFunctions));
  }

/****************************************/
/* RemoveCPFunction: Removes a function */
/*   from a list of functions.          */
/****************************************/
static BOOLEAN RemoveCPFunction(
  char *name,
  struct cleanupFunction **head)
  {
   struct cleanupFunction *currentPtr, *lastPtr;

   lastPtr = NULL;
   currentPtr = *head;

   while (currentPtr != NULL)
     {
      if (strcmp(name,currentPtr->name) == 0)
        {
         if (lastPtr == NULL)
           { *head = currentPtr->next; }
         else
           { lastPtr->next = currentPtr->next; }
         rtn_struct(cleanupFunction,currentPtr);
         return(TRUE);
        }
      lastPtr = currentPtr;
      currentPtr = currentPtr->next;
     }

   return(FALSE);
  }

/*****************************************************/
/* StringPrintForm: Generates printed representation */
/*   of a string. Replaces / with // and " with /".  */
/*****************************************************/
globle char *StringPrintForm(
  char *str)
  {
   int i = 0, pos = 0, max = 0;
   char *theString = NULL;
   void *thePtr;

   theString = ExpandStringWithChar('"',theString,&pos,&max,max+80);
   while (str[i] != EOS)
     {
      if ((str[i] == '"') || (str[i] == '\\'))
        {
         theString = ExpandStringWithChar('\\',theString,&pos,&max,max+80);
         theString = ExpandStringWithChar(str[i],theString,&pos,&max,max+80);
        }
      else
        { theString = ExpandStringWithChar(str[i],theString,&pos,&max,max+80); }
      i++;
     }

   theString = ExpandStringWithChar('"',theString,&pos,&max,max+80);

   thePtr = AddSymbol(theString);
   rm(theString,max);
   return(ValueToString(thePtr));
  }

/***********************************************************/
/* AppendStrings: Appends two strings together. The string */
/*   created is added to the SymbolTable, so it is not     */
/*   necessary to deallocate the string returned.          */
/***********************************************************/
globle char *AppendStrings(
  char *str1,
  char *str2)
  {
   int pos = 0, max = 0;
   char *theString = NULL;
   void *thePtr;

   theString = AppendToString(str1,theString,&pos,&max);
   theString = AppendToString(str2,theString,&pos,&max);

   thePtr = AddSymbol(theString);
   rm(theString,max);
   return(ValueToString(thePtr));
  }

/******************************************************/
/* AppendToString: Appends a string to another string */
/*   (expanding the other string if necessary).       */
/******************************************************/
globle char *AppendToString(
  char *appendStr,
  char *oldStr,
  int *oldPos,
  int *oldMax)
  {
   int length;

   /*=========================================*/
   /* Expand the old string so it can contain */
   /* the new string (if necessary).          */
   /*=========================================*/

   length = strlen(appendStr);
   if (length + *oldPos + 1 > *oldMax)
     {
      oldStr = (char *) genrealloc(oldStr,(unsigned) *oldMax,(unsigned) length + *oldPos + 1);
      *oldMax = length + *oldPos + 1;
     }

   /*==============================================================*/
   /* Return NULL if the old string was not successfully expanded. */
   /*==============================================================*/

   if (oldStr == NULL) { return(NULL); }

   /*===============================================*/
   /* Append the new string to the expanded string. */
   /*===============================================*/

   strcpy(&oldStr[*oldPos],appendStr);
   *oldPos += length;

   /*============================================================*/
   /* Return the expanded string containing the appended string. */
   /*============================================================*/

   return(oldStr);
  }

/*******************************************************/
/* AppendNToString: Appends a string to another string */
/*   (expanding the other string if necessary). Only a */
/*   specified number of characters are appended from  */
/*   the string.                                       */
/*******************************************************/
globle char *AppendNToString(
  char *appendStr,
  char *oldStr,
  int length,
  int *oldPos,
  int *oldMax)
  {
   int lengthWithEOS;

   /*====================================*/
   /* Determine the number of characters */
   /* to be appended from the string.    */
   /*====================================*/

   if (appendStr[length-1] != '\0') lengthWithEOS = length + 1;
   else lengthWithEOS = length;

   /*=========================================*/
   /* Expand the old string so it can contain */
   /* the new string (if necessary).          */
   /*=========================================*/

   if (lengthWithEOS + *oldPos > *oldMax)
     {
      oldStr = (char *) genrealloc(oldStr,(unsigned) *oldMax,(unsigned) *oldPos + lengthWithEOS);
      *oldMax = *oldPos + lengthWithEOS;
     }

   /*==============================================================*/
   /* Return NULL if the old string was not successfully expanded. */
   /*==============================================================*/

   if (oldStr == NULL) { return(NULL); }

   /*==================================*/
   /* Append N characters from the new */
   /* string to the expanded string.   */
   /*==================================*/

   strncpy(&oldStr[*oldPos],appendStr,(CLIPS_STD_SIZE) length);
   *oldPos += (lengthWithEOS - 1);
   oldStr[*oldPos] = '\0';

   /*============================================================*/
   /* Return the expanded string containing the appended string. */
   /*============================================================*/

   return(oldStr);
  }

/*******************************************************/
/* ExpandStringWithChar: Adds a character to a string, */
/*   reallocating space for the string if it needs to  */
/*   be enlarged. The backspace character causes the   */
/*   size of the string to reduced if it is "added" to */
/*   the string.                                       */
/*******************************************************/
globle char *ExpandStringWithChar(
  int inchar,
  char *str,
  int *pos,
  int *max,
  int newSize)
  {
   if (*pos >= (*max - 1))
     {
      str = (char *) genrealloc(str,(unsigned) *max,(unsigned) newSize);
      *max = newSize;
     }

  if (inchar != '\b')
    {
     str[*pos] = (char) inchar;
     (*pos)++;
     str[*pos] = '\0';
    }
  else
    {
     if (*pos > 0) (*pos)--;
     str[*pos] = '\0';
    }

   return(str);
  }

/*****************************************************************/
/* AddFunctionToCallList: Adds a function to a list of functions */
/*   which are called to perform certain operations (e.g. clear, */
/*   reset, and bload functions).                                */
/*****************************************************************/
globle struct callFunctionItem *AddFunctionToCallList(
  char *name,
  int priority,
  void (*func)(void),
  struct callFunctionItem *head)
  {
   struct callFunctionItem *newPtr, *currentPtr, *lastPtr = NULL;

   newPtr = get_struct(callFunctionItem);

   newPtr->name = name;
   newPtr->func = func;
   newPtr->priority = priority;

   if (head == NULL)
     {
      newPtr->next = NULL;
      return(newPtr);
     }

   currentPtr = head;
   while ((currentPtr != NULL) ? (priority < currentPtr->priority) : FALSE)
     {
      lastPtr = currentPtr;
      currentPtr = currentPtr->next;
     }

   if (lastPtr == NULL)
     {
      newPtr->next = head;
      head = newPtr;
     }
   else
     {
      newPtr->next = currentPtr;
      lastPtr->next = newPtr;
     }

   return(head);
  }

/*****************************************************************/
/* RemoveFunctionFromCallList: Removes a function from a list of */
/*   functions which are called to perform certain operations    */
/*   (e.g. clear, reset, and bload functions).                   */
/*****************************************************************/
globle struct callFunctionItem *RemoveFunctionFromCallList(
  char *name,
  struct callFunctionItem *head,
  int *found)
  {
   struct callFunctionItem *currentPtr, *lastPtr;

   *found = FALSE;
   lastPtr = NULL;
   currentPtr = head;

   while (currentPtr != NULL)
     {
      if (strcmp(name,currentPtr->name) == 0)
        {
         *found = TRUE;
         if (lastPtr == NULL)
           { head = currentPtr->next; }
         else
           { lastPtr->next = currentPtr->next; }

         rtn_struct(callFunctionItem,currentPtr);
         return(head);
        }

      lastPtr = currentPtr;
      currentPtr = currentPtr->next;
     }

   return(head);
  }

/*****************************************/
/* ItemHashValue: Returns the hash value */
/*   for the specified value.            */
/*****************************************/
globle int ItemHashValue(
  int theType,
  void *theValue,
  int theRange)
  {
   switch(theType)
     {
      case FLOAT:
        return(HashFloat(ValueToDouble(theValue),theRange));

      case INTEGER:
        return(HashInteger(ValueToLong(theValue),theRange));

      case SYMBOL:
      case STRING:
#if OBJECT_SYSTEM
      case INSTANCE_NAME:
#endif
        return(HashSymbol(ValueToString(theValue),theRange));

      case MULTIFIELD:
        return(HashMultifield((struct multifield *) theValue,theRange));

#if DEFTEMPLATE_CONSTRUCT
      case FACT_ADDRESS:
        return(HashFact((struct fact *) theValue) % theRange);
#endif

      case EXTERNAL_ADDRESS:
#if OBJECT_SYSTEM
      case INSTANCE_ADDRESS:
#endif
        return(((int) theValue) % theRange);

      default:
        SystemError("UTILITY",1);
        return(-1L);

     }

   return(-1L);
  }

/********************************************/
/* YieldTime: Yields time to a user-defined */
/*   function. Intended to allow foreground */
/*   application responsiveness when CLIPS  */
/*   is running in the background.          */
/********************************************/
void YieldTime()
  {
   if (YieldTimeFunction != NULL)
     { (*YieldTimeFunction)(); }
  }

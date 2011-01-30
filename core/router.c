   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                  I/O ROUTER MODULE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides a centralized mechanism for handling    */
/*   input and output requests.                              */
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

#define _ROUTER_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <stdlib.h>
#include <string.h>

#include "setup.h"

#include "constant.h"
#include "memalloc.h"
#include "filertr.h"
#include "strngrtr.h"
#include "extnfunc.h"
#include "argacces.h"
#include "sysdep.h"

#include "router.h"

struct router
  {
   char *name;
   int active;
   int priority;
   int (*query)(char *);
   int (*printer)(char *,char *);
   int (*exiter)(int);
   int (*charget)(char *);
   int (*charunget)(int,char *);
   struct router *next;
  };

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static int                     QueryRouter(char *,struct router *);

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static struct router       *ListOfRouters = NULL;
   Thread static FILE                *FastLoadFilePtr = NULL;
   Thread static FILE                *FastSaveFilePtr = NULL;
   Thread static int                  Abort;

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle char                *WWARNING = "wwarning";
   Thread globle char                *WERROR = "werror";
   Thread globle char                *WTRACE = "wtrace";
   Thread globle char                *WDIALOG = "wdialog";
   Thread globle char                *WPROMPT  = WPROMPT_STRING;
   Thread globle char                *WDISPLAY = "wdisplay";
   Thread globle int                  CommandBufferInputCount = -1;
   Thread globle char                *LineCountRouter = NULL;
   Thread globle char                *FastCharGetRouter = NULL;
   Thread globle char                *FastCharGetString = NULL;
   Thread globle long                 FastCharGetIndex = 0;

/*********************************************************/
/* InitializeDefaultRouters: Initializes output streams. */
/*********************************************************/
globle void InitializeDefaultRouters()
  {
#if (! RUN_TIME)
   DefineFunction2("exit",    'v', PTIF ExitCommand,    "ExitCommand", "*1i");
#endif

   InitializeFileRouter();
   InitializeStringRouter();
  }

/****************************************/
/* PrintRouter: Generic print function. */
/****************************************/
globle int PrintRouter(
  char *logicalName,
  char *str)
  {
   struct router *currentPtr;

   /*===================================================*/
   /* If the "fast save" option is being used, then the */
   /* logical name is actually a pointer to a file and  */
   /* fprintf can be called directly to bypass querying */
   /* all of the routers.                               */
   /*===================================================*/

   if (((char *) FastSaveFilePtr) == logicalName)
     {
      fprintf(FastSaveFilePtr,"%s",str);
      return(2);
     }

   /*==============================================*/
   /* Search through the list of routers until one */
   /* is found that will handle the print request. */
   /*==============================================*/

   currentPtr = ListOfRouters;
   while (currentPtr != NULL)
     {
      if ((currentPtr->printer != NULL) ? QueryRouter(logicalName,currentPtr) : FALSE)
        {
         (*currentPtr->printer) (logicalName,str);
         return(1);
        }
      currentPtr = currentPtr->next;
     }

   /*=====================================================*/
   /* The logical name was not recognized by any routers. */
   /*=====================================================*/

   if (strcmp(WERROR,logicalName) != 0) UnrecognizedRouterMessage(logicalName);
   return(0);
  }

/***********************************************/
/* GetcRouter: Generic get character function. */
/***********************************************/
globle int GetcRouter(
  char *logicalName)
  {
   struct router *currentPtr;
   int inchar;

   /*===================================================*/
   /* If the "fast load" option is being used, then the */
   /* logical name is actually a pointer to a file and  */
   /* getc can be called directly to bypass querying    */
   /* all of the routers.                               */
   /*===================================================*/

   if (((char *) FastLoadFilePtr) == logicalName)
     {
      inchar = getc(FastLoadFilePtr);

      if ((inchar == '\r') || (inchar == '\n'))
        {
         if (((char *) FastLoadFilePtr) == LineCountRouter)
           { IncrementLineCount(); }
        }

      if (inchar == '\r') return('\n');
      /*
      if (inchar != '\b')
        { return(inchar); }
      */
      return(inchar);
     }

   /*===============================================*/
   /* If the "fast string get" option is being used */
   /* for the specified logical name, then bypass   */
   /* the router system and extract the character   */
   /* directly from the fast get string.            */
   /*===============================================*/

   if (FastCharGetRouter == logicalName)
     {
      inchar = FastCharGetString[FastCharGetIndex];

      FastCharGetIndex++;

      if (inchar == '\0') return(EOF);

      if ((inchar == '\r') || (inchar == '\n'))
        {
         if (FastCharGetRouter == LineCountRouter)
           { IncrementLineCount(); }
        }

      if (inchar == '\r') return('\n');

      return(inchar);
     }

   /*==============================================*/
   /* Search through the list of routers until one */
   /* is found that will handle the getc request.  */
   /*==============================================*/

   currentPtr = ListOfRouters;
   while (currentPtr != NULL)
     {
      if ((currentPtr->charget != NULL) ? QueryRouter(logicalName,currentPtr) : FALSE)
        {
         inchar = (*currentPtr->charget) (logicalName);

         if ((inchar == '\r') || (inchar == '\n'))
           {
            if ((LineCountRouter != NULL) &&
                (strcmp(logicalName,LineCountRouter) == 0))
              { IncrementLineCount(); }
           }

         if (inchar == '\r') return('\n');
         /*
         if (inchar != '\b')
           { return(inchar); }
         */
         return(inchar);
        }
      currentPtr = currentPtr->next;
     }

   /*=====================================================*/
   /* The logical name was not recognized by any routers. */
   /*=====================================================*/

   UnrecognizedRouterMessage(logicalName);
   return(-1);
  }

/***************************************************/
/* UngetcRouter: Generic unget character function. */
/***************************************************/
globle int UngetcRouter(
  int ch,
  char *logicalName)
  {
   struct router *currentPtr;

   /*===================================================*/
   /* If the "fast load" option is being used, then the */
   /* logical name is actually a pointer to a file and  */
   /* ungetc can be called directly to bypass querying  */
   /* all of the routers.                               */
   /*===================================================*/

   if (((char *) FastLoadFilePtr) == logicalName)
     {
      if ((ch == '\r') || (ch == '\n'))
        {
         if (((char *) FastLoadFilePtr) == LineCountRouter)
           { DecrementLineCount(); }
        }

      return(ungetc(ch,FastLoadFilePtr));
     }

   /*===============================================*/
   /* If the "fast string get" option is being used */
   /* for the specified logical name, then bypass   */
   /* the router system and unget the character     */
   /* directly from the fast get string.            */
   /*===============================================*/

   if (FastCharGetRouter == logicalName)
     {
      if ((ch == '\r') || (ch == '\n'))
        {
         if (FastCharGetRouter == LineCountRouter)
           { DecrementLineCount(); }
        }

      if (FastCharGetIndex > 0) FastCharGetIndex--;
      return(ch);
     }

   /*===============================================*/
   /* Search through the list of routers until one  */
   /* is found that will handle the ungetc request. */
   /*===============================================*/

   currentPtr = ListOfRouters;
   while (currentPtr != NULL)
     {
      if ((currentPtr->charunget != NULL) ? QueryRouter(logicalName,currentPtr) : FALSE)
        {
         if ((ch == '\r') || (ch == '\n'))
           {
            if ((LineCountRouter != NULL) &&
                (strcmp(logicalName,LineCountRouter) == 0))
              { DecrementLineCount(); }
           }

         return((*currentPtr->charunget) (ch,logicalName));
        }

      currentPtr = currentPtr->next;
     }

   /*=====================================================*/
   /* The logical name was not recognized by any routers. */
   /*=====================================================*/

   UnrecognizedRouterMessage(logicalName);
   return(-1);
  }

/*****************************************************/
/* ExitCommand: H/L command for exiting the program. */
/*****************************************************/
globle void ExitCommand()
  {
   int argCnt;
   int status;

   if ((argCnt = ArgCountCheck("exit",NO_MORE_THAN,1)) == -1) return;
   if (argCnt == 0)
     { ExitRouter(EXIT_SUCCESS); }
   else
    {
     status = (int) RtnLong(1);
     if (GetEvaluationError()) return;
     ExitRouter(status);
    }

   return;
  }

/*******************************************/
/* ExitRouter: Generic exit function. Calls */
/*   all of the router exit functions.     */
/*******************************************/
globle void ExitRouter(
  int num)
  {
   struct router *currentPtr, *nextPtr;

   Abort = FALSE;
   currentPtr = ListOfRouters;
   while (currentPtr != NULL)
     {
      nextPtr = currentPtr->next;
      if (currentPtr->active == TRUE)
        { if (currentPtr->exiter != NULL) (*currentPtr->exiter) (num); }
      currentPtr = nextPtr;
     }

   if (Abort) return;
//   genexit(num);
  }

/********************************************/
/* AbortExit: Forces ExitRouter to terminate */
/*   after calling all closing routers.     */
/********************************************/
globle void AbortExit()
  {
   Abort = TRUE;
  }

/*********************************************************/
/* AddRouter: Adds an I/O router to the list of routers. */
/*********************************************************/
globle BOOLEAN AddRouter(
  char *routerName,
  int priority,
  int (*queryFunction)(char *),
  int (*printFunction)(char *,char *),
  int (*getcFunction)(char *),
  int (*ungetcFunction)(int,char *),
  int (*exitFunction)(int))
  {
   struct router *newPtr, *lastPtr, *currentPtr;

   newPtr = get_struct(router);

   newPtr->name = routerName;
   newPtr->active = TRUE;
   newPtr->priority = priority;
   newPtr->query = queryFunction;
   newPtr->printer = printFunction;
   newPtr->exiter = exitFunction;
   newPtr->charget = getcFunction;
   newPtr->charunget = ungetcFunction;
   newPtr->next = NULL;

   if (ListOfRouters == NULL)
     {
      ListOfRouters = newPtr;
      return(1);
     }

   lastPtr = NULL;
   currentPtr = ListOfRouters;
   while ((currentPtr != NULL) ? (priority < currentPtr->priority) : FALSE)
     {
      lastPtr = currentPtr;
      currentPtr = currentPtr->next;
     }

   if (lastPtr == NULL)
     {
      newPtr->next = ListOfRouters;
      ListOfRouters = newPtr;
     }
   else
     {
      newPtr->next = currentPtr;
      lastPtr->next = newPtr;
     }

   return(1);
  }

/*****************************************************************/
/* DeleteRouter: Removes an I/O router from the list of routers. */
/*****************************************************************/
globle int DeleteRouter(
  char *routerName)
  {
   struct router *currentPtr, *lastPtr;

   currentPtr = ListOfRouters;
   lastPtr = NULL;

   while (currentPtr != NULL)
     {
      if (strcmp(currentPtr->name,routerName) == 0)
        {
         if (lastPtr == NULL)
           {
            ListOfRouters = currentPtr->next;
            rm(currentPtr,(int) sizeof(struct router));
            return(1);
           }
         lastPtr->next = currentPtr->next;
         rm(currentPtr,(int) sizeof(struct router));
         return(1);
        }
      lastPtr = currentPtr;
      currentPtr = currentPtr->next;
     }

   return(0);
  }

/*********************************************************************/
/* QueryRouters: Determines if any router recognizes a logical name. */
/*********************************************************************/
globle int QueryRouters(
  char *logicalName)
  {
   struct router *currentPtr;

   currentPtr = ListOfRouters;
   while (currentPtr != NULL)
     {
      if (QueryRouter(logicalName,currentPtr) == TRUE) return(TRUE);
      currentPtr = currentPtr->next;
     }

   return(FALSE);
  }

/************************************************/
/* QueryRouter: Determines if a specific router */
/*    recognizes a logical name.                */
/************************************************/
static int QueryRouter(
  char *logicalName,
  struct router *currentPtr)
  {
   /*===================================================*/
   /* If the router is inactive, then it can't respond. */
   /*===================================================*/

   if (currentPtr->active == FALSE)
     { return(FALSE); }

   /*=============================================================*/
   /* If the router has no query function, then it can't respond. */
   /*=============================================================*/

   if (currentPtr->query == NULL) return(FALSE);

   /*=========================================*/
   /* Call the router's query function to see */
   /* if it recognizes the logical name.      */
   /*=========================================*/

   if ( (*currentPtr->query) (logicalName) == TRUE )
     { return(TRUE); }

   return(FALSE);
  }

/****************************************************/
/* DeactivateRouter: Deactivates a specific router. */
/****************************************************/
globle int DeactivateRouter(
  char *routerName)
  {
   struct router *currentPtr;

   currentPtr = ListOfRouters;

   while (currentPtr != NULL)
     {
      if (strcmp(currentPtr->name,routerName) == 0)
        {
         currentPtr->active = FALSE;
         return(TRUE);
        }
      currentPtr = currentPtr->next;
     }

   return(FALSE);
  }

/************************************************/
/* ActivateRouter: Activates a specific router. */
/************************************************/
globle int ActivateRouter(
  char *routerName)
  {
   struct router *currentPtr;

   currentPtr = ListOfRouters;

   while (currentPtr != NULL)
     {
      if (strcmp(currentPtr->name,routerName) == 0)
        {
         currentPtr->active = TRUE;
         return(TRUE);
        }
      currentPtr = currentPtr->next;
     }

   return(FALSE);
  }

/********************************************************/
/* SetFastLoad: Used to bypass router system for loads. */
/********************************************************/
globle void SetFastLoad(
  FILE *filePtr)
  { FastLoadFilePtr = filePtr; }

/********************************************************/
/* SetFastSave: Used to bypass router system for saves. */
/********************************************************/
globle void SetFastSave(
  FILE *filePtr)
  { FastSaveFilePtr = filePtr; }

/******************************************************/
/* GetFastLoad: Returns the "fast load" file pointer. */
/******************************************************/
globle FILE *GetFastLoad()
  { return(FastLoadFilePtr); }

/******************************************************/
/* GetFastSave: Returns the "fast save" file pointer. */
/******************************************************/
globle FILE *GetFastSave()
  { return(FastSaveFilePtr); }

/*****************************************************/
/* UnrecognizedRouterMessage: Standard error message */
/*   for an unrecognized router name.                */
/*****************************************************/
globle void UnrecognizedRouterMessage(
  char *logicalName)
  {
   PrintErrorID("ROUTER",1,FALSE);
   PrintRouter(WERROR,"Logical name ");
   PrintRouter(WERROR,logicalName);
   PrintRouter(WERROR," was not recognized by any routers\n");
  }

/*****************************************/
/* PrintNRouter: Generic print function. */
/*****************************************/
globle int PrintNRouter(
  char *logicalName,
  char *str,
  long length)
  {
   char *tempStr;
   int rv;

   tempStr = (char *) genlongalloc(length+1);
   strncpy(tempStr,str,length);
   tempStr[length] = 0;
   rv = PrintRouter(logicalName,tempStr);
   genlongfree(tempStr,length+1);
   return(rv);
  }

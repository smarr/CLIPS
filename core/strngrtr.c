   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.22  06/15/04            */
   /*                                                     */
   /*              STRING I/O ROUTER MODULE               */
   /*******************************************************/

/*************************************************************/
/* Purpose: I/O Router routines which allow strings to be    */
/*   used as input and output sources.                       */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _STRNGRTR_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <stdlib.h>
#include <string.h>

#include "setup.h"

#include "constant.h"
#include "envrnmnt.h"
#include "memalloc.h"
#include "router.h"
#include "sysdep.h"

#include "strngrtr.h"

#define READ_STRING 0
#define WRITE_STRING 1

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static int                     FindString(void *,char *);
   static int                     PrintString(void *,char *,char *);
   static int                     GetcString(void *,char *);
   static int                     UngetcString(void *,int,char *);
   static struct stringRouter    *FindStringRouter(void *,char *);
   static int                     CreateReadStringSource(void *,char *,char *,size_t,size_t);
   static void                    DeallocateStringRouterData(void *);

/**********************************************************/
/* InitializeStringRouter: Initializes string I/O router. */
/**********************************************************/
globle void InitializeStringRouter(
  void *theEnv,
  EXEC_STATUS)
  {
   AllocateEnvironmentData(theEnv,execStatus,STRING_ROUTER_DATA,sizeof(struct stringRouterData),DeallocateStringRouterData);

   EnvAddRouter(theEnv,execStatus,"string",0,FindString,PrintString,GetcString,UngetcString,NULL);
  }
  
/*******************************************/
/* DeallocateStringRouterData: Deallocates */
/*    environment data for string routers. */
/*******************************************/
static void DeallocateStringRouterData(
  void *theEnv,
  EXEC_STATUS)
  {
   struct stringRouter *tmpPtr, *nextPtr;
   
   tmpPtr = StringRouterData(theEnv)->ListOfStringRouters;
   while (tmpPtr != NULL)
     {
      nextPtr = tmpPtr->next;
      rm(theEnv,execStatus,tmpPtr->name,strlen(tmpPtr->name) + 1);
      rtn_struct(theEnv,execStatus,stringRouter,tmpPtr);
      tmpPtr = nextPtr;
     }
  }

/*************************************************************/
/* FindString: Find routine for string router logical names. */
/*************************************************************/
static int FindString(
  void *theEnv,
  EXEC_STATUS,
  char *fileid)
  {
   struct stringRouter *head;

   head = StringRouterData(theEnv)->ListOfStringRouters;
   while (head != NULL)
     {
      if (strcmp(head->name,fileid) == 0)
        { return(TRUE); }
      head = head->next;
     }

   return(FALSE);
  }

/**************************************************/
/* PrintString: Print routine for string routers. */
/**************************************************/
static int PrintString(
  void *theEnv,
  EXEC_STATUS,
  char *logicalName,
  char *str)
  {
   struct stringRouter *head;

   head = FindStringRouter(theEnv,execStatus,logicalName);
   if (head == NULL)
     {
      SystemError(theEnv,execStatus,"ROUTER",3);
      EnvExitRouter(theEnv,execStatus,EXIT_FAILURE);
     }

   if (head->readWriteType != WRITE_STRING) return(1);
   
   if (head->maximumPosition == 0) return(1);
   
   if ((head->currentPosition + 1) >= head->maximumPosition) return(1);

   genstrncpy(&head->str[head->currentPosition],
              str,(STD_SIZE) (head->maximumPosition - head->currentPosition) - 1);

   head->currentPosition += strlen(str);
   
   return(1);
  }

/************************************************/
/* GetcString: Getc routine for string routers. */
/************************************************/
static int GetcString(
  void *theEnv,
  EXEC_STATUS,
  char *logicalName)
  {
   struct stringRouter *head;
   int rc;

   head = FindStringRouter(theEnv,execStatus,logicalName);
   if (head == NULL)
     {
      SystemError(theEnv,execStatus,"ROUTER",1);
      EnvExitRouter(theEnv,execStatus,EXIT_FAILURE);
     }

   if (head->readWriteType != READ_STRING) return(EOF);
   if (head->currentPosition >= head->maximumPosition)
     {
      head->currentPosition++;
      return(EOF);
     }

   rc = (unsigned char) head->str[head->currentPosition];
   head->currentPosition++;

   return(rc);
  }

/****************************************************/
/* UngetcString: Ungetc routine for string routers. */
/****************************************************/
#if WIN_BTC
#pragma argsused
#endif
static int UngetcString(
  void *theEnv,
  EXEC_STATUS,
  int ch,
  char *logicalName)
  {
   struct stringRouter *head;
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(ch)
#endif

   head = FindStringRouter(theEnv,execStatus,logicalName);

   if (head == NULL)
     {
      SystemError(theEnv,execStatus,"ROUTER",2);
      EnvExitRouter(theEnv,execStatus,EXIT_FAILURE);
     }

   if (head->readWriteType != READ_STRING) return(0);
   if (head->currentPosition > 0)
     { head->currentPosition--; }

   return(1);
  }

/************************************************/
/* OpenStringSource: Opens a new string router. */
/************************************************/
globle int OpenStringSource(
  void *theEnv,
  EXEC_STATUS,
  char *name,
  char *str,
  size_t currentPosition)
  {
   size_t maximumPosition;

   if (str == NULL)
     {
      currentPosition = 0;
      maximumPosition = 0;
     }
   else
     { maximumPosition = strlen(str); }

   return(CreateReadStringSource(theEnv,execStatus,name,str,currentPosition,maximumPosition));
  }

/******************************************************/
/* OpenTextSource: Opens a new string router for text */
/*   (which is not NULL terminated).                  */
/******************************************************/
globle int OpenTextSource(
  void *theEnv,
  EXEC_STATUS,
  char *name,
  char *str,
  size_t currentPosition,
  size_t maximumPosition)
  {
   if (str == NULL)
     {
      currentPosition = 0;
      maximumPosition = 0;
     }

   return(CreateReadStringSource(theEnv,execStatus,name,str,currentPosition,maximumPosition));
  }

/******************************************************************/
/* CreateReadStringSource: Creates a new string router for input. */
/******************************************************************/
static int CreateReadStringSource(
  void *theEnv,
  EXEC_STATUS,
  char *name,
  char *str,
  size_t currentPosition,
  size_t maximumPosition)
  {
   struct stringRouter *newStringRouter;

   if (FindStringRouter(theEnv,execStatus,name) != NULL) return(0);

   newStringRouter = get_struct(theEnv,execStatus,stringRouter);
   newStringRouter->name = (char *) gm1(theEnv,execStatus,strlen(name) + 1);
   genstrcpy(newStringRouter->name,name);
   newStringRouter->str = str;
   newStringRouter->currentPosition = currentPosition;
   newStringRouter->readWriteType = READ_STRING;
   newStringRouter->maximumPosition = maximumPosition;
   newStringRouter->next = StringRouterData(theEnv)->ListOfStringRouters;
   StringRouterData(theEnv)->ListOfStringRouters = newStringRouter;

   return(1);
  }

/**********************************************/
/* CloseStringSource: Closes a string router. */
/**********************************************/
globle int CloseStringSource(
  void *theEnv,
  EXEC_STATUS,
  char *name)
  {
   struct stringRouter *head, *last;

   last = NULL;
   head = StringRouterData(theEnv)->ListOfStringRouters;
   while (head != NULL)
     {
      if (strcmp(head->name,name) == 0)
        {
         if (last == NULL)
           {
            StringRouterData(theEnv)->ListOfStringRouters = head->next;
            rm(theEnv,execStatus,head->name,strlen(head->name) + 1);
            rtn_struct(theEnv,execStatus,stringRouter,head);
            return(1);
           }
         else
           {
            last->next = head->next;
            rm(theEnv,execStatus,head->name,strlen(head->name) + 1);
            rtn_struct(theEnv,execStatus,stringRouter,head);
            return(1);
           }
        }
      last = head;
      head = head->next;
     }

   return(0);
  }

/******************************************************************/
/* OpenStringDestination: Opens a new string router for printing. */
/******************************************************************/
globle int OpenStringDestination(
  void *theEnv,
  EXEC_STATUS,
  char *name,
  char *str,
  size_t maximumPosition)
  {
   struct stringRouter *newStringRouter;

   if (FindStringRouter(theEnv,execStatus,name) != NULL) return(0);

   newStringRouter = get_struct(theEnv,execStatus,stringRouter);
   newStringRouter->name = (char *) gm1(theEnv,execStatus,(int) strlen(name) + 1);
   genstrcpy(newStringRouter->name,name);
   newStringRouter->str = str;
   newStringRouter->currentPosition = 0;
   newStringRouter->readWriteType = WRITE_STRING;
   newStringRouter->maximumPosition = maximumPosition;
   newStringRouter->next = StringRouterData(theEnv)->ListOfStringRouters;
   StringRouterData(theEnv)->ListOfStringRouters = newStringRouter;

   return(1);
  }

/***************************************************/
/* CloseStringDestination: Closes a string router. */
/***************************************************/
globle int CloseStringDestination(
  void *theEnv,
  EXEC_STATUS,
  char *name)
  {
   return(CloseStringSource(theEnv,execStatus,name));
  }

/*******************************************************************/
/* FindStringRouter: Returns a pointer to the named string router. */
/*******************************************************************/
static struct stringRouter *FindStringRouter(
  void *theEnv,
  EXEC_STATUS,
  char *name)
  {
   struct stringRouter *head;

   head = StringRouterData(theEnv)->ListOfStringRouters;
   while (head != NULL)
     {
      if (strcmp(head->name,name) == 0)
        { return(head); }
      head = head->next;
     }

   return(NULL);
  }





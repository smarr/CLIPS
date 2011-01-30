   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
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
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _STRNGRTR_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <stdlib.h>
#include <string.h>

#include "setup.h"

#include "constant.h"
#include "memalloc.h"
#include "router.h"

#include "strngrtr.h"

#define READ_STRING 0
#define WRITE_STRING 1

struct stringRouter
  {
   char *name;
   char *str;
   int currentPosition;
   int maximumPosition;
   int readWriteType;
   struct stringRouter *next;
  };

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static int                     FindString(char *);
   static int                     PrintString(char *,char *);
   static int                     GetcString(char *);
   static int                     UngetcString(int,char *);
   static struct stringRouter    *FindStringRouter(char *);
   static int                     CreateReadStringSource(char *,char *,int,int);

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static struct stringRouter *ListOfStringRouters = NULL;

/**********************************************************/
/* InitializeStringRouter: Initializes string I/O router. */
/**********************************************************/
globle void InitializeStringRouter()
  {
   AddRouter("string",0,FindString,PrintString,GetcString,UngetcString,NULL);
  }

/*************************************************************/
/* FindString: Find routine for string router logical names. */
/*************************************************************/
static int FindString(
  char *fileid)
  {
   struct stringRouter *head;

   head = ListOfStringRouters;
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
  char *logicalName,
  char *str)
  {
   struct stringRouter *head;

   head = FindStringRouter(logicalName);
   if (head == NULL)
     {
      SystemError("ROUTER",3);
      ExitRouter(EXIT_FAILURE);
     }

   if (head->readWriteType != WRITE_STRING) return(1);
   if (head->currentPosition >= (head->maximumPosition - 1)) return(1);

   strncpy(&head->str[head->currentPosition],
           str,(CLIPS_STD_SIZE) (head->maximumPosition - head->currentPosition) - 1);

   head->currentPosition += strlen(str);
   return(1);
  }

/************************************************/
/* GetcString: Getc routine for string routers. */
/************************************************/
static int GetcString(
  char *logicalName)
  {
   struct stringRouter *head;
   int rc;

   head = FindStringRouter(logicalName);
   if (head == NULL)
     {
      SystemError("ROUTER",1);
      ExitRouter(EXIT_FAILURE);
     }

   if (head->readWriteType != READ_STRING) return(EOF);
   if (head->currentPosition >= head->maximumPosition)
     {
      head->currentPosition++;
      return(EOF);
     }

   rc = head->str[head->currentPosition];
   head->currentPosition++;

   return(rc);
  }

/****************************************************/
/* UngetcString: Ungetc routine for string routers. */
/****************************************************/
#if IBM_TBC
#pragma argsused
#endif
static int UngetcString(
  int ch,
  char *logicalName)
  {
   struct stringRouter *head;
#if MAC_MPW || MAC_MCW || IBM_MCW
#pragma unused(ch)
#endif

   head = FindStringRouter(logicalName);

   if (head == NULL)
     {
      SystemError("ROUTER",2);
      ExitRouter(EXIT_FAILURE);
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
  char *name,
  char *str,
  int currentPosition)
  {
   int maximumPosition;

   if (str == NULL)
     {
      currentPosition = 0;
      maximumPosition = 0;
     }
   else
     { maximumPosition = strlen(str); }

   return(CreateReadStringSource(name,str,currentPosition,maximumPosition));
  }

/******************************************************/
/* OpenTextSource: Opens a new string router for text */
/*   (which is not NULL terminated).                  */
/******************************************************/
globle int OpenTextSource(
  char *name,
  char *str,
  int currentPosition,
  int maximumPosition)
  {
   if (str == NULL)
     {
      currentPosition = 0;
      maximumPosition = 0;
     }

   return(CreateReadStringSource(name,str,currentPosition,maximumPosition));
  }

/******************************************************************/
/* CreateReadStringSource: Creates a new string router for input. */
/******************************************************************/
static int CreateReadStringSource(
  char *name,
  char *str,
  int currentPosition,
  int maximumPosition)
  {
   struct stringRouter *newStringRouter;

   if (FindStringRouter(name) != NULL) return(0);

   newStringRouter = get_struct(stringRouter);
   newStringRouter->name = (char *) gm1((int) strlen(name) + 1);
   strcpy(newStringRouter->name,name);
   newStringRouter->str = str;
   newStringRouter->currentPosition = currentPosition;
   newStringRouter->readWriteType = READ_STRING;
   newStringRouter->maximumPosition = maximumPosition;
   newStringRouter->next = ListOfStringRouters;
   ListOfStringRouters = newStringRouter;

   return(1);
  }

/**********************************************/
/* CloseStringSource: Closes a string router. */
/**********************************************/
globle int CloseStringSource(
  char *name)
  {
   struct stringRouter *head, *last;

   last = NULL;
   head = ListOfStringRouters;
   while (head != NULL)
     {
      if (strcmp(head->name,name) == 0)
        {
         if (last == NULL)
           {
            ListOfStringRouters = head->next;
            rm(head->name,(int) strlen(head->name) + 1);
            rtn_struct(stringRouter,head);
            return(1);
           }
         else
           {
            last->next = head->next;
            rm(head->name,(int) strlen(head->name) + 1);
            rtn_struct(stringRouter,head);
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
  char *name,
  char *str,
  int maximumPosition)
  {
   struct stringRouter *newStringRouter;

   if (FindStringRouter(name) != NULL) return(0);

   newStringRouter = get_struct(stringRouter);
   newStringRouter->name = (char *) gm1((int) strlen(name) + 1);
   strcpy(newStringRouter->name,name);
   newStringRouter->str = str;
   newStringRouter->currentPosition = 0;
   newStringRouter->readWriteType = WRITE_STRING;
   newStringRouter->maximumPosition = maximumPosition;
   newStringRouter->next = ListOfStringRouters;
   ListOfStringRouters = newStringRouter;

   return(1);
  }

/***************************************************/
/* CloseStringDestination: Closes a string router. */
/***************************************************/
globle int CloseStringDestination(
  char *name)
  {
   return(CloseStringSource(name));
  }

/*******************************************************************/
/* FindStringRouter: Returns a pointer to the named string router. */
/*******************************************************************/
static struct stringRouter *FindStringRouter(
  char *name)
  {
   struct stringRouter *head;

   head = ListOfStringRouters;
   while (head != NULL)
     {
      if (strcmp(head->name,name) == 0)
        { return(head); }
      head = head->next;
     }

   return(NULL);
  }

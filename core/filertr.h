   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.20  01/31/02            */
   /*                                                     */
   /*             FILE I/O ROUTER HEADER FILE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: I/O Router routines which allow files to be used */
/*   as input and output sources.                            */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_filertr
#define _H_filertr

#ifndef _STDIO_INCLUDED_
#define _STDIO_INCLUDED_
#include <stdio.h>
#endif

#define FILE_ROUTER_DATA 47
   
struct fileRouter
  {
   char *logicalName;
   FILE *stream;
   struct fileRouter *next;
  };

struct fileRouterData
  { 
   struct fileRouter *ListOfFileRouters;
  };

#define FileRouterData(theEnv) ((struct fileRouterData *) GetEnvironmentData(theEnv,execStatus,FILE_ROUTER_DATA))

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _FILERTR_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

# include "execution_status.h"

   LOCALE void                           InitializeFileRouter(void *,EXEC_STATUS);
   LOCALE FILE                          *FindFptr(void *,EXEC_STATUS,char *);
   LOCALE int                            OpenAFile(void *,EXEC_STATUS,char *,char *,char *);
   LOCALE int                            CloseAllFiles(void *,EXEC_STATUS);
   LOCALE int                            CloseFile(void *,EXEC_STATUS,char *);
   LOCALE int                            FindFile(void *,EXEC_STATUS,char *);

#endif







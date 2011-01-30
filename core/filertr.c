   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*               FILE I/O ROUTER MODULE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: I/O Router routines which allow files to be used */
/*   as input and output sources.                            */
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

#define _FILERTR_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include "setup.h"

#include "constant.h"
#include "memalloc.h"
#include "router.h"

#include "filertr.h"

struct fileRouter
  {
   char *logicalName;
   FILE *stream;
   struct fileRouter *next;
  };

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static int                     ExitFile(int);
   static int                     PrintFile(char *,char *);
   static int                     GetcFile(char *);
   static int                     UngetcFile(int,char *);

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static struct fileRouter   *ListOfFileRouters = NULL;

/***************************************************************/
/* InitializeFileRouter: Initializes file input/output router. */
/***************************************************************/
globle void InitializeFileRouter()
  {
   AddRouter("fileio",0,FindFile,
             PrintFile,GetcFile,
             UngetcFile,ExitFile);
  }

/*****************************************/
/* FindFptr: Returns a pointer to a file */
/*   stream for a given logical name.    */
/*****************************************/
globle FILE *FindFptr(
  char *logicalName)
  {
   struct fileRouter *fptr;

   /*========================================================*/
   /* Check to see if standard input or output is requested. */
   /*========================================================*/

   if (strcmp(logicalName,"stdout") == 0)
     { return(stdout); }
   else if (strcmp(logicalName,"stdin") == 0)
     { return(stdin); }
   else if (strcmp(logicalName,WTRACE) == 0)
     { return(stdout); }
   else if (strcmp(logicalName,WDIALOG) == 0)
     { return(stdout); }
   else if (strcmp(logicalName,WPROMPT) == 0)
     { return(stdout); }
   else if (strcmp(logicalName,WDISPLAY) == 0)
     { return(stdout); }
   else if (strcmp(logicalName,WERROR) == 0)
     { return(stdout); }
   else if (strcmp(logicalName,WWARNING) == 0)
     { return(stdout); }

   /*==============================================================*/
   /* Otherwise, look up the logical name on the global file list. */
   /*==============================================================*/

   fptr = ListOfFileRouters;
   while ((fptr != NULL) ? (strcmp(logicalName,fptr->logicalName) != 0) : FALSE)
     { fptr = fptr->next; }

   if (fptr != NULL) return(fptr->stream);

   return(NULL);
  }

/*****************************************************/
/* FindFile: Find routine for file router logical    */
/*   names. Returns TRUE if the specified logical    */
/*   name has an associated file stream (which means */
/*   that the logical name can be handled by the     */
/*   file router). Otherwise, FALSE is returned.     */
/*****************************************************/
globle int FindFile(
  char *logicalName)
  {
   if (FindFptr(logicalName) != NULL) return(TRUE);

   return(FALSE);
  }

/********************************************/
/* ExitFile:  Exit routine for file router. */
/********************************************/
#if IBM_TBC
#pragma argsused
#endif
static int ExitFile(
  int num)
  {
#if MAC_MPW || MAC_MCW || IBM_MCW
#pragma unused(num)
#endif
#if BASIC_IO
   CloseAllFiles();
#endif
   return(1);
  }

/*********************************************/
/* PrintFile: Print routine for file router. */
/*********************************************/
static int PrintFile(
  char *logicalName,
  char *str)
  {
   FILE *fptr;

   fptr = FindFptr(logicalName);
   fprintf(fptr,"%s",str);
   fflush(fptr);
   return(1);
  }

/*******************************************/
/* GetcFile: Getc routine for file router. */
/*******************************************/
static int GetcFile(
  char *logicalName)
  {
   FILE *fptr;
   int theChar;

   fptr = FindFptr(logicalName);

   theChar = getc(fptr);

   /*=================================================*/
   /* The following code prevents Control-D on UNIX   */
   /* machines from terminating all input from stdin. */
   /*=================================================*/

   if ((fptr == stdin) && (theChar == EOF)) clearerr(stdin);

   return(theChar);
  }

/***********************************************/
/* UngetcFile: Ungetc routine for file router. */
/***********************************************/
static int UngetcFile(
  int ch,
  char *logicalName)
  {
   FILE *fptr;

   fptr = FindFptr(logicalName);
   return(ungetc(ch,fptr));
  }

/*********************************************************/
/* OpenFile: Opens a file with the specified access mode */
/*   and stores the opened stream on the list of files   */
/*   associated with logical names Returns TRUE if the   */
/*   file was succesfully opened, otherwise FALSE.       */
/*********************************************************/
globle int OpenAFile(
  char *fileName,
  char *accessMode,
  char *logicalName)
  {
   FILE *newstream;
   struct fileRouter *newRouter;

   /*==================================*/
   /* Make sure the file can be opened */
   /* with the specified access mode.  */
   /*==================================*/

   if ((newstream = fopen(fileName,accessMode)) == NULL)
     { return(FALSE); }

   /*===========================*/
   /* Create a new file router. */
   /*===========================*/

   newRouter = get_struct(fileRouter);
   newRouter->logicalName = (char *) gm2 ((int) strlen(logicalName) + 1);
   strcpy(newRouter->logicalName,logicalName);
   newRouter->stream = newstream;

   /*==========================================*/
   /* Add the newly opened file to the list of */
   /* files associated with logical names.     */
   /*==========================================*/

   newRouter->next = ListOfFileRouters;
   ListOfFileRouters = newRouter;

   /*==================================*/
   /* Return TRUE to indicate the file */
   /* was opened successfully.         */
   /*==================================*/

   return(TRUE);
  }

/*************************************************************/
/* CloseFile: Closes the file associated with the specified  */
/*   logical name. Returns TRUE if the file was successfully */
/*   closed, otherwise FALSE.                                */
/*************************************************************/
globle int CloseFile(
  char *fid)
  {
   struct fileRouter *fptr, *prev;

   for (fptr = ListOfFileRouters, prev = NULL;
        fptr != NULL;
        fptr = fptr->next)
     {
      if (strcmp(fptr->logicalName,fid) == 0)
        {
         fclose(fptr->stream);
         rm(fptr->logicalName,(int) strlen(fptr->logicalName) + 1);
         if (prev == NULL)
           { ListOfFileRouters = fptr->next; }
         else
           { prev->next = fptr->next; }
         rm(fptr,(int) sizeof(struct fileRouter));

         return(TRUE);
        }

      prev = fptr;
     }

   return(FALSE);
  }

/**********************************************/
/* CloseAllFiles: Closes all files associated */
/*   with a file I/O router. Returns TRUE if  */
/*   any file was closed, otherwise FALSE.    */
/**********************************************/
globle int CloseAllFiles()
  {
   struct fileRouter *fptr, *prev;

   if (ListOfFileRouters == NULL) return(FALSE);

   fptr = ListOfFileRouters;

   while (fptr != NULL)
     {
      fclose(fptr->stream);
      prev = fptr;
      rm(fptr->logicalName,(int) strlen(fptr->logicalName) + 1);
      fptr = fptr->next;
      rm(prev,(int) sizeof(struct fileRouter));
     }

   ListOfFileRouters = NULL;

   return(TRUE);
  }




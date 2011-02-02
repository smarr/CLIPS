   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*                 PRETTY PRINT MODULE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Routines for processing the pretty print         */
/*   representation of constructs.                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Chris Culbert                                        */
/*      Brian Dantes                                         */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Corrected code generating compilation          */
/*            warnings.                                      */
/*                                                           */
/*************************************************************/

#define _PPRINT_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>
#include <ctype.h>

#include "setup.h"

#include "constant.h"
#include "envrnmnt.h"
#include "memalloc.h"
#include "sysdep.h"
#include "utility.h"

#include "pprint.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    DeallocatePrettyPrintData(void *);

/****************************************************/
/* InitializePrettyPrintData: Allocates environment */
/*    data for pretty print routines.               */
/****************************************************/
globle void InitializePrettyPrintData(
  void *theEnv,
  EXEC_STATUS)
  {
   AllocateEnvironmentData(theEnv,execStatus,PRETTY_PRINT_DATA,sizeof(struct prettyPrintData),DeallocatePrettyPrintData);
   
   PrettyPrintData(theEnv)->PPBufferEnabled = TRUE;
  }

/******************************************************/
/* DeallocatePrettyPrintData: Deallocates environment */
/*    data for the pretty print routines.             */
/******************************************************/
static void DeallocatePrettyPrintData(
  void *theEnv,
  EXEC_STATUS)
  {
   if (PrettyPrintData(theEnv)->PrettyPrintBuffer != NULL) 
     { rm(theEnv,execStatus,PrettyPrintData(theEnv)->PrettyPrintBuffer,PrettyPrintData(theEnv)->PPBufferMax); }
  }

/*******************************************************/
/* FlushPPBuffer: Resets the pretty print save buffer. */
/*******************************************************/
globle void FlushPPBuffer(
  void *theEnv,
  EXEC_STATUS)
  {
   if (PrettyPrintData(theEnv)->PrettyPrintBuffer == NULL) return;
   PrettyPrintData(theEnv)->PPBackupOnce = 0;
   PrettyPrintData(theEnv)->PPBackupTwice = 0;
   PrettyPrintData(theEnv)->PPBufferPos = 0;
   PrettyPrintData(theEnv)->PrettyPrintBuffer[0] = EOS;
   return;
  }

/*********************************************************************/
/* DestroyPPBuffer: Resets and removes the pretty print save buffer. */
/*********************************************************************/
globle void DestroyPPBuffer(
	void *theEnv,
	EXEC_STATUS)
  {
   PrettyPrintData(theEnv)->PPBackupOnce = 0;
   PrettyPrintData(theEnv)->PPBackupTwice = 0;
   PrettyPrintData(theEnv)->PPBufferPos = 0;
   if (PrettyPrintData(theEnv)->PrettyPrintBuffer != NULL) rm(theEnv,execStatus,PrettyPrintData(theEnv)->PrettyPrintBuffer,PrettyPrintData(theEnv)->PPBufferMax);
   PrettyPrintData(theEnv)->PrettyPrintBuffer = NULL;
   PrettyPrintData(theEnv)->PPBufferMax = 0;
  }

/*********************************************/
/* SavePPBuffer: Appends a string to the end */
/*   of the pretty print save buffer.        */
/*********************************************/
globle void SavePPBuffer(
  void *theEnv,
  EXEC_STATUS,
  char *str)
  {
   size_t increment;

   /*==========================================*/
   /* If the pretty print buffer isn't needed, */
   /* then don't bother writing to it.         */
   /*==========================================*/

   if ((PrettyPrintData(theEnv)->PPBufferStatus == OFF) || (! PrettyPrintData(theEnv)->PPBufferEnabled)) 
     { return; }

   /*===============================*/
   /* Determine the increment size. */
   /*===============================*/

   increment = 512;
   if (PrettyPrintData(theEnv)->PPBufferPos > increment)
     { increment = PrettyPrintData(theEnv)->PPBufferPos * 3; }

   /*================================================*/
   /* If the pretty print buffer isn't big enough to */
   /* contain the string, then increase its size.    */
   /*================================================*/

   if (strlen(str) + PrettyPrintData(theEnv)->PPBufferPos + 1 >= PrettyPrintData(theEnv)->PPBufferMax)
     {
      PrettyPrintData(theEnv)->PrettyPrintBuffer = 
         (char *) genrealloc(theEnv,execStatus,PrettyPrintData(theEnv)->PrettyPrintBuffer,
                                    PrettyPrintData(theEnv)->PPBufferMax,
                                    PrettyPrintData(theEnv)->PPBufferMax + increment);
      PrettyPrintData(theEnv)->PPBufferMax += increment;
     }

   /*==================================================*/
   /* Remember the previous tokens saved to the pretty */
   /* print buffer in case it is necessary to back up. */
   /*==================================================*/

   PrettyPrintData(theEnv)->PPBackupTwice = PrettyPrintData(theEnv)->PPBackupOnce;
   PrettyPrintData(theEnv)->PPBackupOnce = PrettyPrintData(theEnv)->PPBufferPos;

   /*=============================================*/
   /* Save the string to the pretty print buffer. */
   /*=============================================*/

   PrettyPrintData(theEnv)->PrettyPrintBuffer = AppendToString(theEnv,execStatus,str,PrettyPrintData(theEnv)->PrettyPrintBuffer,&PrettyPrintData(theEnv)->PPBufferPos,&PrettyPrintData(theEnv)->PPBufferMax);
  }

/***************************************************/
/* PPBackup:  Removes the last string added to the */
/*   pretty print save buffer. Only capable of     */
/*   backing up for the two most recent additions. */
/***************************************************/
globle void PPBackup(
  void *theEnv,
  EXEC_STATUS)
  {
   if ((PrettyPrintData(theEnv)->PPBufferStatus == OFF) || 
       (PrettyPrintData(theEnv)->PrettyPrintBuffer == NULL) ||
       (! PrettyPrintData(theEnv)->PPBufferEnabled))
     { return; }

   PrettyPrintData(theEnv)->PPBufferPos = PrettyPrintData(theEnv)->PPBackupOnce;
   PrettyPrintData(theEnv)->PPBackupOnce = PrettyPrintData(theEnv)->PPBackupTwice;
   PrettyPrintData(theEnv)->PrettyPrintBuffer[PrettyPrintData(theEnv)->PPBufferPos] = EOS;
  }

/**************************************************/
/* CopyPPBuffer: Makes a copy of the pretty print */
/*   save buffer.                                 */
/**************************************************/
globle char *CopyPPBuffer(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t length;
   char *newString;

   length = (1 + strlen(PrettyPrintData(theEnv)->PrettyPrintBuffer)) * (int) sizeof (char);
   newString = (char *) gm2(theEnv,execStatus,length);

   genstrcpy(newString,PrettyPrintData(theEnv)->PrettyPrintBuffer);
   return(newString);
  }

/************************************************************/
/* GetPPBuffer: Returns a pointer to the PrettyPrintBuffer. */
/************************************************************/
globle char *GetPPBuffer(
  void *theEnv,
  EXEC_STATUS)
  {
   return(PrettyPrintData(theEnv)->PrettyPrintBuffer);
  }

/*******************************************/
/* PPCRAndIndent: Prints white spaces into */
/*   the pretty print buffer.              */
/*******************************************/
globle void PPCRAndIndent(
  void *theEnv,
  EXEC_STATUS)
  {
   int i;
   char buffer[120];

   if ((PrettyPrintData(theEnv)->PPBufferStatus == OFF) || 
       (! PrettyPrintData(theEnv)->PPBufferEnabled))
     { return; }

   buffer[0] = '\n';

   for (i = 1 ; i <= PrettyPrintData(theEnv)->IndentationDepth ; i++)
     { buffer[i] = ' '; }
   buffer[i] = EOS;

   SavePPBuffer(theEnv,execStatus,buffer);
  }

/************************************************/
/* IncrementIndentDepth: Increments indentation */
/*   depth for pretty printing.                 */
/************************************************/
globle void IncrementIndentDepth(
  void *theEnv,
  EXEC_STATUS,
  int value)
  {
   PrettyPrintData(theEnv)->IndentationDepth += value;
  }

/************************************************/
/* DecrementIndentDepth: Decrements indentation */
/*   depth for pretty printing.                 */
/************************************************/
globle void DecrementIndentDepth(
  void *theEnv,
  EXEC_STATUS,
  int value)
  {
   PrettyPrintData(theEnv)->IndentationDepth -= value;
  }

/************************************/
/* SetIndentDepth: Sets indentation */
/*   depth for pretty printing.     */
/************************************/
globle void SetIndentDepth(
  void *theEnv,
  EXEC_STATUS,
  int value)
  {
   PrettyPrintData(theEnv)->IndentationDepth = value;
  }

/******************************************/
/* SetPPBufferStatus: Sets PPBufferStatus */
/*   flag to boolean value of ON or OFF.  */
/******************************************/
globle void SetPPBufferStatus(
  void *theEnv,
  EXEC_STATUS,
  int value)
  {
   PrettyPrintData(theEnv)->PPBufferStatus = value;
  }

/************************************/
/* GetPPBufferStatus: Returns value */
/*   of the PPBufferStatus flag.    */
/************************************/
globle int GetPPBufferStatus(
  void *theEnv,
  EXEC_STATUS)
  {
   return(PrettyPrintData(theEnv)->PPBufferStatus);
  }

/******************************************/
/* SetPPBufferEnabled: */
/******************************************/
globle int SetPPBufferEnabled(
  void *theEnv,
  EXEC_STATUS,
  int value)
  {
   int oldValue;
   
   oldValue = PrettyPrintData(theEnv)->PPBufferEnabled;
   PrettyPrintData(theEnv)->PPBufferEnabled = value;
   return(oldValue);
  }

/************************************/
/* GetPPBufferEnabled: */
/************************************/
globle int GetPPBufferEnabled(
  void *theEnv,
  EXEC_STATUS)
  {
   return(PrettyPrintData(theEnv)->PPBufferEnabled);
  }


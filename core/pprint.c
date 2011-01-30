   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
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
/*      Brian Donnell                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description              */
/* ------------------+-------------+------------------------  */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS        */
/*************************************************************/

#define _PPRINT_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>
#include <ctype.h>

#include "setup.h"
#include "constant.h"
#include "memalloc.h"
#include "utility.h"

#include "pprint.h"

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static int              PPBufferStatus = OFF;
   Thread static int              IndentationDepth = 0;
   Thread static int              PPBufferPos = 0;
   Thread static int              PPBufferMax = 0;
   Thread static int              PPBackupOnce = 0;
   Thread static int              PPBackupTwice = 0;
   Thread static char            *PrettyPrintBuffer = NULL;

/*******************************************************/
/* FlushPPBuffer: Resets the pretty print save buffer. */
/*******************************************************/
globle void FlushPPBuffer()
  {
   if (PrettyPrintBuffer == NULL) return;
   PPBackupOnce = 0;
   PPBackupTwice = 0;
   PPBufferPos = 0;
   PrettyPrintBuffer[0] = EOS;
   return;
  }

/*********************************************************************/
/* DestroyPPBuffer: Resets and removes the pretty print save buffer. */
/*********************************************************************/
globle void DestroyPPBuffer()
  {
   PPBackupOnce = 0;
   PPBackupTwice = 0;
   PPBufferPos = 0;
   if (PrettyPrintBuffer != NULL) rm(PrettyPrintBuffer,PPBufferMax);
   PrettyPrintBuffer = NULL;
   PPBufferMax = 0;
  }

/*********************************************/
/* SavePPBuffer: Appends a string to the end */
/*   of the pretty print save buffer.        */
/*********************************************/
globle void SavePPBuffer(
  char *str)
  {
   long int longSize;
   int normalSize;
   int increment;

   /*==========================================*/
   /* If the pretty print buffer isn't needed, */
   /* then don't bother writing to it.         */
   /*==========================================*/

   if (PPBufferStatus == OFF) return;

   /*===============================*/
   /* Determine the increment size. */
   /*===============================*/

   increment = 512;
   if (PPBufferPos > increment)
     {
      increment = PPBufferPos * 3;
      if (increment < 0)
        { increment = 512; }
     }

   /*==================================================*/
   /* The pretty print buffer is limited in size to    */
   /* the maximum size of a signed int. Any characters */
   /* beyond that number are discarded.                */
   /*==================================================*/

   normalSize = strlen(str);
   longSize = (long) normalSize;
   longSize += (long) PPBufferPos + ((long) increment) + 1L;
   normalSize += PPBufferPos + increment + 1;
   if (normalSize != longSize) return;

   /*================================================*/
   /* If the pretty print buffer isn't big enough to */
   /* contain the string, then increase its size.    */
   /*================================================*/

   if (((int) strlen(str)) + PPBufferPos + 1 >= PPBufferMax)
     {
      PrettyPrintBuffer = (char *) genrealloc(PrettyPrintBuffer,(unsigned) PPBufferMax,
                                     (unsigned) PPBufferMax + increment);
      PPBufferMax += increment;
     }

   /*==================================================*/
   /* Remember the previous tokens saved to the pretty */
   /* print buffer in case it is necessary to back up. */
   /*==================================================*/

   PPBackupTwice = PPBackupOnce;
   PPBackupOnce = PPBufferPos;

   /*=============================================*/
   /* Save the string to the pretty print buffer. */
   /*=============================================*/

   PrettyPrintBuffer = AppendToString(str,PrettyPrintBuffer,&PPBufferPos,&PPBufferMax);
  }

/***************************************************/
/* PPBackup:  Removes the last string added to the */
/*   pretty print save buffer. Only capable of     */
/*   backing up for the two most recent additions. */
/***************************************************/
globle void PPBackup()
  {
   if ((PPBufferStatus == OFF) || (PrettyPrintBuffer == NULL)) return;

   PPBufferPos = PPBackupOnce;
   PPBackupOnce = PPBackupTwice;
   PrettyPrintBuffer[PPBufferPos] = EOS;
  }

/**************************************************/
/* CopyPPBuffer: Makes a copy of the pretty print */
/*   save buffer.                                 */
/**************************************************/
globle char *CopyPPBuffer()
  {
   int length;
   char *newString;

   length = (1 + strlen(PrettyPrintBuffer)) * (int) sizeof (char);
   newString = (char *) gm2(length);

   strcpy(newString,PrettyPrintBuffer);
   return(newString);
  }

/************************************************************/
/* GetPPBuffer: Returns a pointer to the PrettyPrintBuffer. */
/************************************************************/
globle char *GetPPBuffer()
  {
   return(PrettyPrintBuffer);
  }

/*******************************************/
/* PPCRAndIndent: Prints white spaces into */
/*   the pretty print buffer.              */
/*******************************************/
globle void PPCRAndIndent()
  {
   int i;
   char buffer[120];

   buffer[0] = '\n';

   for (i = 1 ; i <= IndentationDepth ; i++)
     { buffer[i] = ' '; }
   buffer[i] = EOS;

   SavePPBuffer(buffer);
  }

/************************************************/
/* IncrementIndentDepth: Increments indentation */
/*   depth for pretty printing.                 */
/************************************************/
globle void IncrementIndentDepth(
  int value)
  {
   IndentationDepth += value;
  }

/************************************************/
/* DecrementIndentDepth: Decrements indentation */
/*   depth for pretty printing.                 */
/************************************************/
globle void DecrementIndentDepth(
  int value)
  {
   IndentationDepth -= value;
  }

/************************************/
/* SetIndentDepth: Sets indentation */
/*   depth for pretty printing.     */
/************************************/
globle void SetIndentDepth(
  int value)
  {
   IndentationDepth = value;
  }

/******************************************/
/* SetPPBufferStatus: Sets PPBufferStatus */
/*   flag to boolean value of ON or OFF.  */
/******************************************/
globle void SetPPBufferStatus(
  int value)
  {
   PPBufferStatus = value;
  }

/************************************/
/* GetPPBufferStatus: Returns value */
/*   of the PPBufferStatus flag.    */
/************************************/
globle int GetPPBufferStatus()
  {
   return(PPBufferStatus);
  }



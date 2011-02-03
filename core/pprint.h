   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.20  01/31/02            */
   /*                                                     */
   /*               PRETTY PRINT HEADER FILE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Routines for processing the pretty print         */
/*   representation of constructs.                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_pprint
#define _H_pprint

# ifndef _H_execstatus
# include "execstatus.h"
# endif

#define PRETTY_PRINT_DATA 52

struct prettyPrintData
  { 
   int PPBufferStatus;
   int PPBufferEnabled;
   int IndentationDepth;
   size_t PPBufferPos;
   size_t PPBufferMax;
   size_t PPBackupOnce;
   size_t PPBackupTwice;
   char *PrettyPrintBuffer;
  };

#define PrettyPrintData(theEnv,execStatus) ((struct prettyPrintData *) GetEnvironmentData(theEnv,execStatus,PRETTY_PRINT_DATA))

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _PPRINT_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif


   LOCALE void                           InitializePrettyPrintData(void *,EXEC_STATUS);
   LOCALE void                           FlushPPBuffer(void *,EXEC_STATUS);
   LOCALE void                           DestroyPPBuffer(void *,EXEC_STATUS);
   LOCALE void                           SavePPBuffer(void *,EXEC_STATUS,char *);
   LOCALE void                           PPBackup(void *,EXEC_STATUS);
   LOCALE char                          *CopyPPBuffer(void *,EXEC_STATUS);
   LOCALE char                          *GetPPBuffer(void *,EXEC_STATUS);
   LOCALE void                           PPCRAndIndent(void *,EXEC_STATUS);
   LOCALE void                           IncrementIndentDepth(void *,EXEC_STATUS,int);
   LOCALE void                           DecrementIndentDepth(void *,EXEC_STATUS,int);
   LOCALE void                           SetIndentDepth(void *,EXEC_STATUS,int);
   LOCALE void                           SetPPBufferStatus(void *,EXEC_STATUS,int);
   LOCALE int                            GetPPBufferStatus(void *,EXEC_STATUS);
   LOCALE int                            SetPPBufferEnabled(void *,EXEC_STATUS,int);
   LOCALE int                            GetPPBufferEnabled(void *,EXEC_STATUS);

#endif




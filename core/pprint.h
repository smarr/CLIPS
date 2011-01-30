   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
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

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _PPRINT_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           FlushPPBuffer(void);
   LOCALE void                           DestroyPPBuffer(void);
   LOCALE void                           SavePPBuffer(char *);
   LOCALE void                           PPBackup(void);
   LOCALE char                          *CopyPPBuffer(void);
   LOCALE char                          *GetPPBuffer(void);
   LOCALE void                           PPCRAndIndent(void);
   LOCALE void                           IncrementIndentDepth(int);
   LOCALE void                           DecrementIndentDepth(int);
   LOCALE void                           SetIndentDepth(int);
   LOCALE void                           SetPPBufferStatus(int);
   LOCALE int                            GetPPBufferStatus(void);

#endif





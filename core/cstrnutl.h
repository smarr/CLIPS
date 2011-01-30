   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*            CONSTRAINT UTILITY HEADER FILE           */
   /*******************************************************/

/*************************************************************/
/* Purpose: Utility routines for manipulating, initializing, */
/*   creating, copying, and comparing constraint records.    */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian Donnell                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_cstrnutl
#define _H_cstrnutl

#ifndef _H_constrnt
#include "constrnt.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _CSTRNUTL_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#ifndef _STDIO_INCLUDED_
#define _STDIO_INCLUDED_
#include <stdio.h>
#endif

   LOCALE struct constraintRecord       *GetConstraintRecord(void);
   LOCALE int                            CompareNumbers(int,void *,int,void *);
   LOCALE struct constraintRecord       *CopyConstraintRecord(CONSTRAINT_RECORD *);
   LOCALE int                            SetConstraintType(int,CONSTRAINT_RECORD *);
   LOCALE void                           SetAnyAllowedFlags(CONSTRAINT_RECORD *,int);
   LOCALE void                           SetAnyRestrictionFlags(CONSTRAINT_RECORD *,int);
   LOCALE CONSTRAINT_RECORD             *ArgumentTypeToConstraintRecord(int);
   LOCALE CONSTRAINT_RECORD             *FunctionCallToConstraintRecord(void *);
   LOCALE CONSTRAINT_RECORD             *ExpressionToConstraintRecord(struct expr *);

#endif





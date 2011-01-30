   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*    CONSTRAINT BLOAD/BSAVE/CONSTRUCTS-TO-C HEADER    */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for      */
/*    constraint records.                                    */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_cstrnbin
#define _H_cstrnbin

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_constrnt
#include "constrnt.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _CSTRNBIN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#ifndef _STDIO_INCLUDED_
#define _STDIO_INCLUDED_
#include <stdio.h>
#endif

#define ConstraintIndex(theConstraint) (((! GetDynamicConstraintChecking()) || (theConstraint == NULL)) ? -1L : ((long) theConstraint->bsaveIndex))
#define ConstraintPointer(i) (((i) == -1L) ? NULL : (CONSTRAINT_RECORD *) &ConstraintArray[i])

#if BLOAD_AND_BSAVE
   LOCALE void                           WriteNeededConstraints(FILE *);
#endif
   LOCALE void                           ReadNeededConstraints(void);
   LOCALE void                           ClearBloadedConstraints(void);

#ifndef _CSTRNBIN_SOURCE_
   extern Thread CONSTRAINT_RECORD *   ConstraintArray;
#endif

#endif





   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*           CONSTRAINT OPERATIONS HEADER FILE         */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides functions for performing operations on  */
/*   constraint records including computing the intersection */
/*   and union of constraint records.                        */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_cstrnops
#define _H_cstrnops

#if (! RUN_TIME)

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_constrnt
#include "constrnt.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _CSTRNOPS_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE struct constraintRecord       *IntersectConstraints(struct constraintRecord *,struct constraintRecord *);
#if (! BLOAD_ONLY)
   LOCALE struct constraintRecord       *UnionConstraints(struct constraintRecord *,struct constraintRecord *);
   LOCALE void                           RemoveConstantFromConstraint(int,void *,CONSTRAINT_RECORD *);
#endif

#endif

#endif

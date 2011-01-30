   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*            CONSTRAINT CHECKING HEADER FILE          */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides functions for constraint checking of    */
/*   data types.                                             */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_cstrnchk
#define _H_cstrnchk

#ifndef _H_constrnt
#include "constrnt.h"
#endif
#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _CSTRNCHK_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define NO_VIOLATION                    0
#define TYPE_VIOLATION                  1
#define RANGE_VIOLATION                 2
#define ALLOWED_VALUES_VIOLATION        3
#define FUNCTION_RETURN_TYPE_VIOLATION  4
#define CARDINALITY_VIOLATION           5

   LOCALE BOOLEAN                        CheckCardinalityConstraint(long,CONSTRAINT_RECORD *);
   LOCALE BOOLEAN                        CheckAllowedValuesConstraint(int,void *,CONSTRAINT_RECORD *);
   LOCALE int                            ConstraintCheckExpressionChain(struct expr *,
                                                                     CONSTRAINT_RECORD *);
   LOCALE void                           ConstraintViolationErrorMessage(char *,char *,int,int,
                                                                      struct symbolHashNode *,
                                                                      int,int,CONSTRAINT_RECORD *,
                                                                      int);
   LOCALE int                            ConstraintCheckValue(int,void *,CONSTRAINT_RECORD *);
   LOCALE int                            ConstraintCheckDataObject(DATA_OBJECT *,CONSTRAINT_RECORD *);
#if (! BLOAD_ONLY) && (! RUN_TIME)
   LOCALE int                            ConstraintCheckExpression(struct expr *,
                                                                CONSTRAINT_RECORD *);
#endif
#if (! RUN_TIME)
   LOCALE BOOLEAN                        UnmatchableConstraint(struct constraintRecord *);
#endif

#endif





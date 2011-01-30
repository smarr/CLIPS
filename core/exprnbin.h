   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*           EXPRESSION BLOAD/BSAVE HEADER FILE        */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for the  */
/*    expression data structure.                             */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_exprnbin
#define _H_exprnbin

#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _STDIO_INCLUDED_
#define _STDIO_INCLUDED_
#include <stdio.h>
#endif

#ifdef LOCALE
#undef LOCALE
#endif
#ifdef _EXPRNBIN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define ExpressionPointer(i) ((struct expr *) (((i) == -1L) ? NULL : &ExpressionArray[i]))
#define HashedExpressionPointer(i) ExpressionPointer(i)

   LOCALE void                        AllocateExpressions(void);
   LOCALE void                        RefreshExpressions(void);
   LOCALE void                        ClearBloadedExpressions(void);
   LOCALE void                        FindHashedExpressions(void);
   LOCALE void                        BsaveHashedExpressions(FILE *);
   LOCALE void                        BsaveConstructExpressions(FILE *);
   LOCALE void                        BsaveExpression(struct expr *,FILE *);

#ifndef _EXPRNBIN_SOURCE_
   extern Thread struct expr                           *ExpressionArray;
   extern Thread long int                               ExpressionCount;
#endif

#endif








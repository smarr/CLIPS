   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*          EXPRESSION OPERATIONS HEADER FILE          */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides utility routines for manipulating and   */
/*   examining expressions.                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_exprnops

#define _H_exprnops

#ifndef _H_expressn
#include "expressn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _EXPRNOPS_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE BOOLEAN                        ConstantExpression(struct expr *);
   LOCALE void                           PrintExpression(char *,struct expr *);
   LOCALE long                           ExpressionSize(struct expr *);
   LOCALE int                            CountArguments(struct expr *);
   LOCALE BOOLEAN                        ExpressionContainsVariables(struct expr *,int);
   LOCALE BOOLEAN                        IdenticalExpression(struct expr *,struct expr *);
   LOCALE struct expr                   *GenConstant(int,void *);
#if ! RUN_TIME
   LOCALE int                            CheckArgumentAgainstRestriction(struct expr *,int);
#endif
   LOCALE BOOLEAN                        ConstantType(int);
   LOCALE struct expr                   *CombineExpressions(struct expr *,struct expr *);
   LOCALE struct expr                   *AppendExpressions(struct expr *,struct expr *);

#endif





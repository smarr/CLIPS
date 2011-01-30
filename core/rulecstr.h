   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*             RULE CONSTRAINTS HEADER FILE            */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides routines for detecting constraint       */
/*   conflicts in the LHS and RHS of rules.                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_rulecstr

#define _H_rulecstr

#ifdef LOCALE
#undef LOCALE
#endif
#ifdef _RULECSTR_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE struct lhsParseNode           *GetExpressionVarConstraints(struct lhsParseNode *);
   LOCALE struct lhsParseNode           *DeriveVariableConstraints(struct lhsParseNode *);
   LOCALE BOOLEAN                        ProcessConnectedConstraints(struct lhsParseNode *,struct lhsParseNode *,struct lhsParseNode *);
   LOCALE void                           ConstraintReferenceErrorMessage(struct symbolHashNode *,
                                                                struct lhsParseNode *,
                                                                int,int,
                                                                struct symbolHashNode *,
                                                                int);
   LOCALE BOOLEAN                        CheckRHSForConstraintErrors(struct expr *,struct lhsParseNode *);

#endif


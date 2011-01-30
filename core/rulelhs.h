   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*           DEFRULE LHS PARSING HEADER FILE           */
   /*******************************************************/

/*************************************************************/
/* Purpose: Coordinates parsing of the LHS conditional       */
/*   elements of a rule.                                     */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_rulelhs
#define _H_rulelhs

#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_reorder
#include "reorder.h"
#endif
#ifndef _H_scanner
#include "scanner.h"
#endif
#ifndef _H_pattern
#include "pattern.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _RULELHS_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

LOCALE struct lhsParseNode           *ParseRuleLHS(char *,struct token *,char *);
LOCALE void                           PropagatePatternType(struct lhsParseNode *,struct patternParser *);

#ifndef _RULELHS_SOURCE_
   extern Thread int                     GlobalSalience;
   extern Thread int                     GlobalAutoFocus;
   extern Thread struct expr            *SalienceExpression;
#endif

#endif





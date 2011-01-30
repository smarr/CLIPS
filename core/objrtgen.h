   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/09/97          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_objrtgen
#define _H_objrtgen

#if INSTANCE_PATTERN_MATCHING && (! RUN_TIME) && (! BLOAD_ONLY)

#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_reorder
#include "reorder.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _OBJRTGEN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

LOCALE void ReplaceGetJNObjectValue(EXPRESSION *,struct lhsParseNode *);
LOCALE EXPRESSION *GenGetJNObjectValue(struct lhsParseNode *);
LOCALE EXPRESSION *ObjectJNVariableComparison(struct lhsParseNode *,struct lhsParseNode *);
LOCALE EXPRESSION *GenObjectPNConstantCompare(struct lhsParseNode *);
LOCALE void ReplaceGetPNObjectValue(EXPRESSION *,struct lhsParseNode *);
LOCALE EXPRESSION *GenGetPNObjectValue(struct lhsParseNode *);
LOCALE EXPRESSION *ObjectPNVariableComparison(struct lhsParseNode *,struct lhsParseNode *);
LOCALE void GenObjectLengthTest(struct lhsParseNode *);
LOCALE void GenObjectZeroLengthTest(struct lhsParseNode *);

#endif

#endif







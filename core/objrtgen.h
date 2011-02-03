   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.24  05/17/06          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Converted INSTANCE_PATTERN_MATCHING to         */
/*            DEFRULE_CONSTRUCT.                             */
/*                                                           */
/*************************************************************/

#ifndef _H_objrtgen
#define _H_objrtgen

#if DEFRULE_CONSTRUCT && OBJECT_SYSTEM && (! RUN_TIME) && (! BLOAD_ONLY)

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

   LOCALE void             ReplaceGetJNObjectValue(void *,EXEC_STATUS,EXPRESSION *,struct lhsParseNode *,int);
   LOCALE EXPRESSION      *GenGetJNObjectValue(void *,EXEC_STATUS,struct lhsParseNode *,int);
   LOCALE EXPRESSION      *ObjectJNVariableComparison(void *,EXEC_STATUS,struct lhsParseNode *,struct lhsParseNode *,int);
   LOCALE EXPRESSION      *GenObjectPNConstantCompare(void *,EXEC_STATUS,struct lhsParseNode *);
   LOCALE void             ReplaceGetPNObjectValue(void *,EXEC_STATUS,EXPRESSION *,struct lhsParseNode *);
   LOCALE EXPRESSION      *GenGetPNObjectValue(void *,EXEC_STATUS,struct lhsParseNode *); 
   LOCALE EXPRESSION      *ObjectPNVariableComparison(void *,EXEC_STATUS,struct lhsParseNode *,struct lhsParseNode *);
   LOCALE void             GenObjectLengthTest(void *,EXEC_STATUS,struct lhsParseNode *);
   LOCALE void             GenObjectZeroLengthTest(void *,EXEC_STATUS,struct lhsParseNode *);

#endif

#endif





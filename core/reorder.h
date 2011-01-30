   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                 REORDER HEADER FILE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides routines necessary for converting the   */
/*   the LHS of a rule into an appropriate form suitable for */
/*   the KB Rete topology. This includes transforming the    */
/*   LHS so there is at most one "or" CE (and this is the    */
/*   first CE of the LHS if it is used), adding initial      */
/*   patterns to the LHS (if no LHS is specified or a "test" */
/*   or "not" CE is the first pattern within an "and" CE),   */
/*   removing redundant CEs, and determining appropriate     */
/*   information on nesting for implementing joins from the  */
/*   right.                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_reorder
#define _H_reorder

struct lhsParseNode;

#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_ruledef
#include "ruledef.h"
#endif
#ifndef _H_pattern
#include "pattern.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _REORDER_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

/***********************************************************************/
/* lhsParseNode structure: Stores information about the intermediate   */
/*   parsed representation of the lhs of a rule.                       */
/***********************************************************************/
struct lhsParseNode
  {
   int type;
   void *value;
   unsigned int negated : 1;
   unsigned int logical : 1;
   unsigned int multifieldSlot : 1;
   unsigned int bindingVariable : 1;
   unsigned int derivedConstraints : 1;
   unsigned int userCE : 1;
   unsigned int whichCE : 7;
   unsigned int marked : 1;
   unsigned int withinMultifieldSlot : 1;
   unsigned int multiFieldsBefore : 7;
   unsigned int multiFieldsAfter : 7;
   unsigned int singleFieldsBefore : 7;
   unsigned int singleFieldsAfter : 7;
   struct constraintRecord *constraints;
   struct lhsParseNode *referringNode;
   struct patternParser *patternType;
   int pattern;
   int index;
   struct symbolHashNode *slot;
   int slotNumber;
   int beginNandDepth;
   int endNandDepth;
   struct expr *networkTest;
   struct lhsParseNode *expression;
   void *userData;
   struct lhsParseNode *right;
   struct lhsParseNode *bottom;
  };

LOCALE struct lhsParseNode           *ReorderPatterns(struct lhsParseNode *,int *);
LOCALE struct lhsParseNode           *CopyLHSParseNodes(struct lhsParseNode *);
LOCALE void                           CopyLHSParseNode(struct lhsParseNode *,struct lhsParseNode *,int);
LOCALE struct lhsParseNode           *GetLHSParseNode(void);
LOCALE void                           ReturnLHSParseNodes(struct lhsParseNode *);
LOCALE struct lhsParseNode           *ExpressionToLHSParseNodes(struct expr *);
LOCALE struct expr                   *LHSParseNodesToExpression(struct lhsParseNode *);
LOCALE void                           AddInitialPatterns(struct lhsParseNode *);

#endif










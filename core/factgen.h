   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*        FACT RETE FUNCTION GENERATION HEADER FILE    */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_factgen

#define _H_factgen

#ifndef _H_reorder
#include "reorder.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _FACTGEN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

/**********************************************************/
/* factGetVarPN1Call: This structure is used to store the */
/*   arguments to the most general extraction routine for */
/*   retrieving a variable from the fact pattern network. */
/**********************************************************/
struct factGetVarPN1Call
  {
   unsigned int factAddress : 1;
   unsigned int allFields : 1;
   unsigned int whichField : 8;
   unsigned int whichSlot : 8;
  };

/***********************************************************/
/* factGetVarPN2Call: This structure is used to store the  */
/*   arguments to the most specific extraction routine for */
/*   retrieving a variable from the fact pattern network.  */
/*   It is used for retrieving the single value stored in  */
/*   a single field slot (the slot index can be used to    */
/*   directly to retrieve the value from the fact array).  */
/***********************************************************/
struct factGetVarPN2Call
  {
   unsigned int whichSlot : 8;
  };

/**********************************************************/
/* factGetVarPN3Call:  */
/**********************************************************/
struct factGetVarPN3Call
  {
   unsigned int fromBeginning : 1;
   unsigned int fromEnd : 1;
   unsigned int beginOffset : 7;
   unsigned int endOffset : 7;
   unsigned int whichSlot : 8;
  };

/**************************************************************/
/* factConstantPN1Call: Used for testing for a constant value */
/*   in the fact pattern network. Compare the value of a      */
/*   single field slot to a constant.                         */
/**************************************************************/
struct factConstantPN1Call
  {
   unsigned int testForEquality : 1;
   unsigned int whichSlot : 8;
  };

/******************************************************************/
/* factConstantPN2Call: Used for testing for a constant value in  */
/*   the fact pattern network. Compare the value of a multifield  */
/*   slot to a constant (where the value retrieved for comparison */
/*   from the slot contains no multifields before or only one     */
/*   multifield before and none after).                           */
/******************************************************************/
struct factConstantPN2Call
  {
   unsigned int testForEquality : 1;
   unsigned int fromBeginning : 1;
   unsigned int offset : 8;
   unsigned int whichSlot : 8;
  };

/**********************************************************/
/* factGetVarJN1Call: This structure is used to store the */
/*   arguments to the most general extraction routine for */
/*   retrieving a fact variable from the join network.    */
/**********************************************************/
struct factGetVarJN1Call
  {
   unsigned int factAddress : 1;
   unsigned int allFields : 1;
   unsigned int whichPattern : 8;
   unsigned int whichSlot : 8;
   unsigned int whichField : 8;
  };

/**********************************************************/
/* factGetVarJN2Call:  */
/**********************************************************/
struct factGetVarJN2Call
  {
   unsigned int whichPattern : 8;
   unsigned int whichSlot : 8;
  };

/**********************************************************/
/* factGetVarJN3Call:  */
/**********************************************************/
struct factGetVarJN3Call
  {
   unsigned int fromBeginning : 1;
   unsigned int fromEnd : 1;
   unsigned int beginOffset : 7;
   unsigned int endOffset : 7;
   unsigned int whichPattern : 8;
   unsigned int whichSlot : 8;
  };

/**********************************************************/
/* factCompVarsPN1Call:  */
/**********************************************************/
struct factCompVarsPN1Call
  {
   unsigned int pass : 1;
   unsigned int fail : 1;
   unsigned int field1 : 7;
   unsigned int field2 : 7;
  };

/**********************************************************/
/* factCompVarsJN1Call:  */
/**********************************************************/
struct factCompVarsJN1Call
  {
   unsigned int pass : 1;
   unsigned int fail : 1;
   unsigned int slot1 : 7;
   unsigned int pattern2 : 8;
   unsigned int slot2 : 7;
  };

/**********************************************************/
/* factCompVarsJN2Call:  */
/**********************************************************/
struct factCompVarsJN2Call
  {
   unsigned int pass : 1;
   unsigned int fail : 1;
   unsigned int slot1 : 7;
   unsigned int fromBeginning1 : 1;
   unsigned int offset1 : 7;
   unsigned int pattern2 : 8;
   unsigned int slot2 : 7;
   unsigned int fromBeginning2 : 1;
   unsigned int offset2 : 7;
  };

/**********************************************************/
/* factCheckLengthPNCall: This structure is used to store */
/*   the  arguments to the routine for determining if the */
/*   length of a multifield slot is equal or greater than */
/*   a specified value.                                   */
/**********************************************************/

struct factCheckLengthPNCall
  {
    unsigned int minLength : 8;
    unsigned int exactly : 1;
    unsigned int whichSlot : 8;
  };

/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   LOCALE void                       InitializeFactReteFunctions(void);
   LOCALE struct expr               *FactPNVariableComparison(struct lhsParseNode *,
                                                              struct lhsParseNode *);
   LOCALE struct expr               *FactJNVariableComparison(struct lhsParseNode *,
                                                              struct lhsParseNode *);
   LOCALE void                       FactReplaceGetvar(struct expr *,struct lhsParseNode *);
   LOCALE void                       FactReplaceGetfield(struct expr *,struct lhsParseNode *);
   LOCALE struct expr               *FactGenPNConstant(struct lhsParseNode *);
   LOCALE struct expr               *FactGenGetfield(struct lhsParseNode *);
   LOCALE struct expr               *FactGenGetvar(struct lhsParseNode *);
   LOCALE struct expr               *FactGenCheckLength(struct lhsParseNode *);
   LOCALE struct expr               *FactGenCheckZeroLength(int);

#endif

   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/09/97          */
   /*                                                     */
   /*    INFERENCE ENGINE OBJECT PARSING ROUTINES MODULE  */
   /*******************************************************/

/**************************************************************/
/* Purpose: RETE Network Parsing Interface for Objects        */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Brian L. Donnell                                      */
/*                                                            */
/* Contributing Programmer(s):                                */
/*                                                            */
/* Revision History:                                          */
/*                                                            */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/**************************************************************/
/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "setup.h"

#if INSTANCE_PATTERN_MATCHING && (! RUN_TIME) && (! BLOAD_ONLY)

#ifndef _STDIO_INCLUDED_
#include <stdio.h>
#define _STDIO_INCLUDED_
#endif

#include "classfun.h"
#include "objrtfnx.h"

#define _OBJRTGEN_SOURCE_
#include "objrtgen.h"

/* =========================================
   *****************************************
                   CONSTANTS
   =========================================
   ***************************************** */

/* =========================================
   *****************************************
                 MACROS AND TYPES
   =========================================
   ***************************************** */

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

static void GenObjectGetVar(int,EXPRESSION *,struct lhsParseNode *);
static BOOLEAN IsSimpleSlotVariable(struct lhsParseNode *);
static EXPRESSION *GenerateSlotComparisonTest(int,struct lhsParseNode *,struct lhsParseNode *);

/* =========================================
   *****************************************
      EXTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */

/* =========================================
   *****************************************
      INTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/**********************************************
  Build functions used by AddPatternParser() to
  provide object access to the join nertwork
 **********************************************/
globle void ReplaceGetJNObjectValue(
  EXPRESSION *theItem,
  struct lhsParseNode *theNode)
  {
   GenObjectGetVar(TRUE,theItem,theNode);
  }

globle EXPRESSION *GenGetJNObjectValue(
  struct lhsParseNode *theNode)
  {
   EXPRESSION *theItem;

   theItem = GenConstant(0,NULL);
   GenObjectGetVar(TRUE,theItem,theNode);
   return(theItem);
  }

globle EXPRESSION *ObjectJNVariableComparison(
  struct lhsParseNode *selfNode,
  struct lhsParseNode *referringNode)
  {
   return(GenerateSlotComparisonTest(TRUE,selfNode,referringNode));
  }

/**********************************************
  Build functions used by AddPatternParser() to
  provide object access to the pattern network
 **********************************************/
globle EXPRESSION *GenObjectPNConstantCompare(
  struct lhsParseNode *theNode)
  {
   struct ObjectCmpPNConstant hack;
   EXPRESSION *exp;
   int tmpType;

   /* ===============================================================
      If the value of a single field slot (or relation name) is being
      compared against a constant, then use specialized routines for
      doing the comparison.

      If a constant comparison is being done within a multifield slot
      and the constant's position has no multifields to the left or
      no multifields to the right, then use the same routine used for
      the single field slot case, but include the offset from either
      the beginning or end of the slot.

      Otherwise, use a general eq/neq test.
      =============================================================== */
   ClearBitString((void *) &hack,(int) sizeof(struct ObjectCmpPNConstant));
   if (theNode->negated)
     hack.fail = 1;
   else
     hack.pass = 1;
   if (((theNode->withinMultifieldSlot == FALSE) ||
        (theNode->multiFieldsAfter == 0) ||
        (theNode->multiFieldsBefore == 0)) &&
       (theNode->slotNumber != ISA_ID) && (theNode->slotNumber != NAME_ID))
     {
      if (theNode->withinMultifieldSlot == FALSE)
        hack.fromBeginning = TRUE;
      else if (theNode->multiFieldsBefore == 0)
        {
         hack.fromBeginning = TRUE;
         hack.offset = theNode->singleFieldsBefore;
        }
      else
        hack.offset = theNode->singleFieldsAfter;
      exp = GenConstant(OBJ_PN_CONSTANT,AddBitMap((void *) &hack,
                                        (int) sizeof(struct ObjectCmpPNConstant)));
      exp->argList = GenConstant(theNode->type,theNode->value);
     }
   else
     {
      hack.general = 1;
      exp = GenConstant(OBJ_PN_CONSTANT,AddBitMap((void *) &hack,
                                        (int) sizeof(struct ObjectCmpPNConstant)));
      exp->argList = GenConstant(0,NULL);
      tmpType = theNode->type;
      theNode->type = SF_VARIABLE;
      GenObjectGetVar(FALSE,exp->argList,theNode);
      theNode->type = tmpType;
      exp->argList->nextArg = GenConstant(theNode->type,theNode->value);
     }
   return(exp);
  }

globle void ReplaceGetPNObjectValue(
  EXPRESSION *theItem,
  struct lhsParseNode *theNode)
  {
   GenObjectGetVar(FALSE,theItem,theNode);
  }

globle EXPRESSION *GenGetPNObjectValue(
  struct lhsParseNode *theNode)
  {
   EXPRESSION *theItem;

   theItem = GenConstant(0,NULL);
   GenObjectGetVar(FALSE,theItem,theNode);
   return(theItem);
  }

globle EXPRESSION *ObjectPNVariableComparison(
  struct lhsParseNode *selfNode,
  struct lhsParseNode *referringNode)
  {
   return(GenerateSlotComparisonTest(FALSE,selfNode,referringNode));
  }

/****************************************************
  NAME         : GenObjectLengthTest
  DESCRIPTION  : Generates a test on the cardinality
                 of a slot matching an object pattern
  INPUTS       : The first lhsParseNode for a slot
                 in an object pattern
  RETURNS      : Nothing useful
  SIDE EFFECTS : The lhsParseNode network test is
                 modified to include the length test
  NOTES        : None
 ****************************************************/
globle void GenObjectLengthTest(
  struct lhsParseNode *theNode)
  {
   struct ObjectMatchLength hack;
   EXPRESSION *theTest;

   if ((theNode->singleFieldsAfter == 0) &&
       (theNode->type != SF_VARIABLE) &&
       (theNode->type != SF_WILDCARD))
     return;

   ClearBitString((void *) &hack,(int) sizeof(struct ObjectMatchLength));

   if ((theNode->type != MF_VARIABLE) &&
       (theNode->type != MF_WILDCARD) &&
       (theNode->multiFieldsAfter == 0))
     hack.exactly = 1;
   else
     hack.exactly = 0;

   if ((theNode->type == SF_VARIABLE) || (theNode->type == SF_WILDCARD))
     hack.minLength = 1 + theNode->singleFieldsAfter;
   else
     hack.minLength = theNode->singleFieldsAfter;

   theTest = GenConstant(OBJ_SLOT_LENGTH,AddBitMap((void *) &hack,
                                         (int) sizeof(struct ObjectMatchLength)));
   theNode->networkTest = CombineExpressions(theTest,theNode->networkTest);
  }

/****************************************************
  NAME         : GenObjectZeroLengthTest
  DESCRIPTION  : Generates a test on the cardinality
                 of a slot matching an object pattern
  INPUTS       : The first lhsParseNode for a slot
                 in an object pattern
  RETURNS      : Nothing useful
  SIDE EFFECTS : The lhsParseNode network test is
                 modified to include the length test
  NOTES        : None
 ****************************************************/
globle void GenObjectZeroLengthTest(
  struct lhsParseNode *theNode)
  {
   struct ObjectMatchLength hack;
   EXPRESSION *theTest;

   ClearBitString((void *) &hack,(int) sizeof(struct ObjectMatchLength));
   hack.exactly = 1;
   hack.minLength = 0;
   theTest = GenConstant(OBJ_SLOT_LENGTH,AddBitMap((void *) &hack,
                                         (int) sizeof(struct ObjectMatchLength)));
   theNode->networkTest = CombineExpressions(theTest,theNode->networkTest);
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***************************************************
  NAME         : GenObjectGetVar
  DESCRIPTION  : Generates the expressions necessary
                 to access object pattern variables
  INPUTS       : 1) An integer code indicating if
                    this is a join network reference
                    or a pattern network reference
                 2) The expression for which to set
                    the type and value
                 3) The lhsParseNode for the
                    variable reference
  RETURNS      : Nothing useful
  SIDE EFFECTS : The value is a packed long holding
                 pattern index, slot number,
                 field index, etc.
  NOTES        : None
 ***************************************************/
static void GenObjectGetVar(
  int joinReference,
  EXPRESSION *theItem,
  struct lhsParseNode *theNode)
  {
   struct ObjectMatchVar1 hack1;
   struct ObjectMatchVar2 hack2;

   ClearBitString((void *) &hack1,(int) sizeof(struct ObjectMatchVar1));
   ClearBitString((void *) &hack2,(int) sizeof(struct ObjectMatchVar2));

   if (joinReference)
     {
      hack1.whichPattern = (unsigned) theNode->pattern;
      hack2.whichPattern = (unsigned) theNode->pattern;
     }

   /* ========================
      Access an object address
      ======================== */
   if (theNode->slotNumber < 0)
     {
      hack1.objectAddress = 1;
      theItem->type = (short) (joinReference ? OBJ_GET_SLOT_JNVAR1 : OBJ_GET_SLOT_PNVAR1);
      theItem->value = AddBitMap((void *) &hack1,(int) sizeof(struct ObjectMatchVar1));
      return;
     }

   /* ======================================
      Access the entire contents of the slot
      ====================================== */
   if ((theNode->singleFieldsBefore == 0) &&
       (theNode->singleFieldsAfter == 0) &&
       (theNode->multiFieldsBefore == 0) &&
       (theNode->multiFieldsAfter == 0) &&
       ((theNode->withinMultifieldSlot == FALSE) ||
        (theNode->type == MF_VARIABLE) ||
        (theNode->type == MF_WILDCARD)))
     {
      hack1.allFields = 1;
      hack1.whichSlot = (unsigned) theNode->slotNumber;
      theItem->type = (short) (joinReference ? OBJ_GET_SLOT_JNVAR1 : OBJ_GET_SLOT_PNVAR1);
      theItem->value = AddBitMap((void *) &hack1,(int) sizeof(struct ObjectMatchVar1));
      return;
     }

   /* =============================================================
      Access a particular field(s) in a multifield slot pattern
      containing at most one multifield variable and at least
      one (or two if no multifield variables) single-field variable
      ============================================================= */
   if (((theNode->type == SF_WILDCARD) || (theNode->type == SF_VARIABLE)) &&
       ((theNode->multiFieldsBefore == 0) || (theNode->multiFieldsAfter == 0)))
     {
      hack2.whichSlot = (unsigned) theNode->slotNumber;
      if (theNode->multiFieldsBefore == 0)
        {
         hack2.fromBeginning = 1;
         hack2.beginningOffset = theNode->singleFieldsBefore;
        }
      else
        {
         hack2.fromEnd = 1;
         hack2.endOffset = theNode->singleFieldsAfter;
        }
      theItem->type = (short) (joinReference ? OBJ_GET_SLOT_JNVAR2 : OBJ_GET_SLOT_PNVAR2);
      theItem->value = AddBitMap((void *) &hack2,sizeof(struct ObjectMatchVar2));
      return;
     }

   if (((theNode->type == MF_WILDCARD) || (theNode->type == MF_VARIABLE)) &&
       (theNode->multiFieldsBefore == 0) &&
       (theNode->multiFieldsAfter == 0))
     {
      hack2.whichSlot = (unsigned) theNode->slotNumber;
      hack2.fromBeginning = 1;
      hack2.fromEnd = 1;
      hack2.beginningOffset = theNode->singleFieldsBefore;
      hack2.endOffset = theNode->singleFieldsAfter;
      theItem->type = (short) (joinReference ? OBJ_GET_SLOT_JNVAR2 : OBJ_GET_SLOT_PNVAR2);
      theItem->value = AddBitMap((void *) &hack2,sizeof(struct ObjectMatchVar2));
      return;
     }

   /* ==================================================
      General slot field access using multifield markers
      ================================================== */
   hack1.whichSlot = (unsigned) theNode->slotNumber;
   hack1.whichField = (unsigned) theNode->index;
   theItem->type = (short) (joinReference ? OBJ_GET_SLOT_JNVAR1 : OBJ_GET_SLOT_PNVAR1);
   theItem->value = AddBitMap((void *) &hack1,sizeof(struct ObjectMatchVar1));
  }

/****************************************************************
  NAME         : IsSimpleSlotVariable
  DESCRIPTION  : Determines if a slot pattern variable
                 references a single-field slot or a single-field
                 in a multifield slot which does not require
                 use of multifield markers
                 (Object addresses are not simple variables)
  INPUTS       : The intermediate parse node
  RETURNS      : TRUE if the variable is simple,
                 FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ****************************************************************/
static BOOLEAN IsSimpleSlotVariable(
  struct lhsParseNode *node)
  {
   if ((node->type == MF_WILDCARD) || (node->type == MF_VARIABLE))
     return(FALSE);
   if ((node->slotNumber < 0) ||
       (node->slotNumber == ISA_ID) ||
       (node->slotNumber == NAME_ID))
     return(FALSE);
   if (node->withinMultifieldSlot == FALSE)
     return(TRUE);
   if (node->multifieldSlot == TRUE)
     return(FALSE);
   if ((node->multiFieldsBefore == 0) || (node->multiFieldsAfter == 0))
     return(TRUE);
   return(FALSE);
  }

/***************************************************************
  NAME         : GenerateSlotComparisonTest
  DESCRIPTION  : Generates pattern and join network
                 expressions for comparing object
                 pattern variables
  INPUTS       : 1) A flag indicating if this is a
                    pattern or join network test
                 2) The intermediate parse node
                    for the first variable
                 3) The intermediate parse node
                    for the second variable
  RETURNS      : An expression for comparing the
                 variables
  SIDE EFFECTS : Expression and bitmaps generated
  NOTES        : The following tests are generated
                 for the following scenarios:

                 SF slot w/ SF slot: PN_1 or JN_1
                 Example: (foo ?x) with (bar ?xy)

                 SF slot w/ SF reference in MF slot: PN_2 or JN_2
                 Example: (foo ?x) (bar ? ?x ? ?)

                 SF reference w/ SF reference: PN_3 or JN_3
                 Example: (foo ? ?x ?) and (bar ? ? ? ?x)

                 All other cases: EQ/NEQ general test
                 Example: (foo $? ?x $?) and (bar ?x)
 ***************************************************************/
static EXPRESSION *GenerateSlotComparisonTest(
  int joinTest,
  struct lhsParseNode *selfNode,
  struct lhsParseNode *referringNode)
  {
   EXPRESSION *exp;
   struct ObjectCmpPNSingleSlotVars1 phack1;
   struct ObjectCmpPNSingleSlotVars2 phack2;
   struct ObjectCmpPNSingleSlotVars3 phack3;
   struct ObjectCmpJoinSingleSlotVars1 jhack1;
   struct ObjectCmpJoinSingleSlotVars2 jhack2;
   struct ObjectCmpJoinSingleSlotVars3 jhack3;

   /* =========================================================
      If we are comparing two single-field slot variables that
      don't require multifield markers for lookup, use
      a quick comparison.  Otherwise, use a general eq/neq with
      the pattern variable access routines
      ========================================================= */
   if (IsSimpleSlotVariable(selfNode) && IsSimpleSlotVariable(referringNode))
     {
      /* ==============================
         Compare two single-field slots
         ============================== */
      if ((selfNode->withinMultifieldSlot == FALSE) &&
          (referringNode->withinMultifieldSlot == FALSE))
        {
         ClearBitString((void *) &phack1,(int) sizeof(struct ObjectCmpPNSingleSlotVars1));
         ClearBitString((void *) &jhack1,(int) sizeof(struct ObjectCmpJoinSingleSlotVars1));
         if (selfNode->negated)
           phack1.fail = jhack1.fail = 1;
         else
           phack1.pass = jhack1.pass = 1;
         phack1.firstSlot = jhack1.firstSlot = (unsigned) selfNode->slotNumber;
         phack1.secondSlot = jhack1.secondSlot = (unsigned) referringNode->slotNumber;
         if (joinTest)
           {
            jhack1.firstPattern = (unsigned) selfNode->pattern;
            jhack1.secondPattern = (unsigned) referringNode->pattern;
            exp = GenConstant(OBJ_JN_CMP1,AddBitMap((void *) &jhack1,
                                           (int) sizeof(struct ObjectCmpJoinSingleSlotVars1)));
           }
         else
           exp = GenConstant(OBJ_PN_CMP1,AddBitMap((void *) &phack1,
                                           (int) sizeof(struct ObjectCmpPNSingleSlotVars1)));
        }
      /* ============================================
         Compare a single-field slot with a
         single-field in a multifield slot (make sure
         the multifield slot reference is first
         ============================================ */
      else if ((selfNode->withinMultifieldSlot == FALSE) ||
               (referringNode->withinMultifieldSlot == FALSE))
        {
         ClearBitString((void *) &phack2,(int) sizeof(struct ObjectCmpPNSingleSlotVars2));
         ClearBitString((void *) &jhack2,(int) sizeof(struct ObjectCmpJoinSingleSlotVars2));
         if (selfNode->negated)
           phack2.fail = jhack2.fail = 1;
         else
           phack2.pass = jhack2.pass = 1;
         if (selfNode->withinMultifieldSlot == TRUE)
           {
            phack2.firstSlot = jhack2.firstSlot = (unsigned) selfNode->slotNumber;
            phack2.secondSlot = jhack2.secondSlot = (unsigned) referringNode->slotNumber;
            if (joinTest)
              {
               jhack2.firstPattern = (unsigned) selfNode->pattern;
               jhack2.secondPattern = (unsigned) referringNode->pattern;
              }
            if (selfNode->multiFieldsBefore == 0)
              {
               phack2.fromBeginning = jhack2.fromBeginning = 1;
               phack2.offset = jhack2.offset = selfNode->singleFieldsBefore;
              }
            else
              phack2.offset = jhack2.offset = selfNode->singleFieldsAfter;
           }
         else
           {
            phack2.firstSlot = jhack2.firstSlot = (unsigned) referringNode->slotNumber;
            phack2.secondSlot = jhack2.secondSlot = (unsigned) selfNode->slotNumber;
            if (joinTest)
              {
               jhack2.firstPattern = (unsigned) referringNode->pattern;
               jhack2.secondPattern = (unsigned) selfNode->pattern;
              }
            if (referringNode->multiFieldsBefore == 0)
              {
               phack2.fromBeginning = jhack2.fromBeginning = 1;
               phack2.offset = jhack2.offset = referringNode->singleFieldsBefore;
              }
            else
              phack2.offset = jhack2.offset = referringNode->singleFieldsAfter;
           }
         if (joinTest)
           exp = GenConstant(OBJ_JN_CMP2,AddBitMap((void *) &jhack2,
                                           (int) sizeof(struct ObjectCmpJoinSingleSlotVars2)));
         else
           exp = GenConstant(OBJ_PN_CMP2,AddBitMap((void *) &phack2,
                                           (int) sizeof(struct ObjectCmpPNSingleSlotVars2)));
        }

      /* ===================================
         Compare two single-field references
         within multifield slots
         =================================== */
      else
        {
         ClearBitString((void *) &phack3,(int) sizeof(struct ObjectCmpPNSingleSlotVars3));
         ClearBitString((void *) &jhack3,(int) sizeof(struct ObjectCmpJoinSingleSlotVars3));
         if (selfNode->negated)
           phack3.fail = jhack3.fail = 1;
         else
           phack3.pass = jhack3.pass = 1;
         phack3.firstSlot = jhack3.firstSlot = (unsigned) selfNode->slotNumber;
         phack3.secondSlot = jhack3.secondSlot = (unsigned) referringNode->slotNumber;
         if (selfNode->multiFieldsBefore == 0)
           {
            phack3.firstFromBeginning = jhack3.firstFromBeginning = 1;
            phack3.firstOffset = jhack3.firstOffset = selfNode->singleFieldsBefore;
           }
         else
           phack3.firstOffset = jhack3.firstOffset = selfNode->singleFieldsAfter;
         if (referringNode->multiFieldsBefore == 0)
           {
            phack3.secondFromBeginning = jhack3.secondFromBeginning = 1;
            phack3.secondOffset = jhack3.secondOffset = referringNode->singleFieldsBefore;
           }
         else
           phack3.secondOffset = jhack3.secondOffset = referringNode->singleFieldsAfter;
         if (joinTest)
           {
            jhack3.firstPattern = (unsigned) selfNode->pattern;
            jhack3.secondPattern = (unsigned) referringNode->pattern;
            exp = GenConstant(OBJ_JN_CMP3,AddBitMap((void *) &jhack3,
                                         (int) sizeof(struct ObjectCmpJoinSingleSlotVars3)));
           }
         else
           exp = GenConstant(OBJ_PN_CMP3,AddBitMap((void *) &phack3,
                                           (int) sizeof(struct ObjectCmpPNSingleSlotVars3)));
        }
     }

   /* ==================================================
      General comparison for multifield slot references,
      references which require multifield markers, and
      object addresses
      ================================================== */
   else
     {
      exp = GenConstant(FCALL,selfNode->negated ? PTR_NEQ : PTR_EQ);
      exp->argList = GenConstant(0,NULL);
      GenObjectGetVar(joinTest,exp->argList,selfNode);
      exp->argList->nextArg = GenConstant(0,NULL);
      GenObjectGetVar(joinTest,exp->argList->nextArg,referringNode);
     }
   return(exp);
  }

#endif

/***************************************************
  NAME         :
  DESCRIPTION  :
  INPUTS       :
  RETURNS      :
  SIDE EFFECTS :
  NOTES        :
 ***************************************************/

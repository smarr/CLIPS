   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                 FACT MATCH MODULE                   */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the algorithm for pattern matching in */
/*   the fact pattern network.                               */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _FACTMCH_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT && DEFRULE_CONSTRUCT

#include "memalloc.h"
#include "extnfunc.h"
#include "router.h"
#if INCREMENTAL_RESET
#include "incrrset.h"
#endif
#include "reteutil.h"
#include "drive.h"
#include "factgen.h"
#include "factrete.h"
#include "tmpltdef.h"

#include "factmch.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static BOOLEAN                  EvaluatePatternExpression(struct factPatternNode *,struct expr *,int);
   static void                     TraceErrorToJoin(struct factPatternNode *,int);
   static void                     ProcessFactAlphaMatch(struct fact *,struct multifieldMarker *,struct factPatternNode *);
   static struct factPatternNode  *GetNextFactPatternNode(int,struct factPatternNode *);
   static int                      SkipFactPatternNode(struct factPatternNode *);
   static void                     ProcessMultifieldNode(struct factPatternNode *,
                                                         struct multifieldMarker *,
                                                         struct multifieldMarker *,int);
   static void                     PatternNetErrorMessage(struct factPatternNode *);

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle struct fact             *CurrentPatternFact;
   Thread globle struct multifieldMarker *CurrentPatternMarks;

/*************************************************************************/
/* FactPatternMatch: Implements the core loop for fact pattern matching. */
/*************************************************************************/
globle void FactPatternMatch(
  struct fact *theFact,
  struct factPatternNode *patternPtr,
  int offset,
  struct multifieldMarker *markers,
  struct multifieldMarker *endMark)
  {
   int theSlotField;
   int offsetSlot;

   /*=========================================================*/
   /* If there's nothing left in the pattern network to match */
   /* against, then the current traversal of the pattern      */
   /* network needs to back up.                               */
   /*=========================================================*/

   if (patternPtr == NULL) return;

   /*=======================================================*/
   /* The offsetSlot variable indicates the current offset  */
   /* within the multifield slot being pattern matched.     */
   /* (Recall that a multifield wildcard or variable        */
   /* recursively iterates through all possible  bindings.) */
   /* Once a new slot starts being pattern matched, the     */
   /* offset is reset to zero.                              */
   /*=======================================================*/

   offsetSlot = patternPtr->whichSlot;

   /*================================================*/
   /* Set up some global parameters for use by the   */
   /* Rete access functions and general convenience. */
   /*================================================*/

   CurrentPatternFact = theFact;
   CurrentPatternMarks = markers;

   /*============================================*/
   /* Loop through each node in pattern network. */
   /*============================================*/

   while (patternPtr != NULL)
     {
      /*=============================================================*/
      /* Determine the position of the field we're going to pattern  */
      /* match. If this routine has been entered recursively because */
      /* of multifield wildcards or variables, then add in the       */
      /* additional offset caused by the values which match these    */
      /* multifields. This offset may be negative (if for example a  */
      /* a multifield matched a zero length value).                  */
      /*=============================================================*/

      theSlotField = patternPtr->whichField;
      if (offsetSlot == patternPtr->whichSlot)
        { theSlotField += offset; }

      /*===================================*/
      /* Determine if we want to skip this */
      /* node during an incremental reset. */
      /*===================================*/

      if (SkipFactPatternNode(patternPtr))
        { patternPtr = GetNextFactPatternNode(TRUE,patternPtr); }

      /*=========================================================*/
      /* If this is a single field pattern node, then determine  */
      /* if the constraints for the node have been satisfied for */
      /* the current field in the slot being examined.           */
      /*=========================================================*/

      else if (patternPtr->header.singlefieldNode)
        {
         /*==================================================*/
         /* If we're at the last slot in the pattern, make   */
         /* sure the number of fields in the fact correspond */
         /* to the number of fields required by the pattern  */
         /* based on the binding of multifield variables.    */
         /*==================================================*/

         int skipit = FALSE;
         if (patternPtr->header.endSlot &&
             ((CurrentPatternMarks == NULL) ?
              FALSE :
              (CurrentPatternMarks->where.whichSlotNumber == patternPtr->whichSlot)) &&
             (CurrentPatternFact->theProposition.theFields
                  [patternPtr->whichSlot].type == MULTIFIELD))
           {
            if ((patternPtr->leaveFields + theSlotField) !=
               ((struct multifield *) CurrentPatternFact->theProposition.theFields
                                      [patternPtr->whichSlot].value)->multifieldLength)
              { skipit = TRUE; }
           }

         if (skipit)
           { patternPtr = GetNextFactPatternNode(TRUE,patternPtr); }
         else

         /*=============================================*/
         /* If the constraints are satisified, then ... */
         /*=============================================*/

         if (EvaluatePatternExpression(patternPtr,
                                       patternPtr->networkTest,
                                       theSlotField))
           {
            /*=======================================================*/
            /* If a leaf pattern node has been successfully reached, */
            /* then the pattern has been satisified. Generate an     */
            /* alpha match to store in the pattern node.             */
            /*=======================================================*/

            if (patternPtr->header.stopNode)
              { ProcessFactAlphaMatch(theFact,markers,patternPtr); }

            /*===================================*/
            /* Move on to the next pattern node. */
            /*===================================*/

            patternPtr = GetNextFactPatternNode(FALSE,patternPtr);
           }

         /*==============================================*/
         /* Otherwise, move on to the next pattern node. */
         /*==============================================*/

         else
           { patternPtr = GetNextFactPatternNode(TRUE,patternPtr); }
        }

      /*======================================================*/
      /* If this is a multifield pattern node, then determine */
      /* if the constraints for the node have been satisfied  */
      /* for the current field in the slot being examined.    */
      /*======================================================*/

      else if (patternPtr->header.multifieldNode)
        {
         /*========================================================*/
         /* Determine if the multifield pattern node's constraints */
         /* are satisfied. If we've traversed to a different slot  */
         /* than the one we started this routine with, then the    */
         /* offset into the slot is reset to zero.                 */
         /*========================================================*/

         if (offsetSlot == patternPtr->whichSlot)
           { ProcessMultifieldNode(patternPtr,markers,endMark,offset); }
         else
           { ProcessMultifieldNode(patternPtr,markers,endMark,0); }

         /*===================================================*/
         /* Move on to the next pattern node. Since the lower */
         /* branches of the pattern network have already been */
         /* recursively processed by ProcessMultifieldNode,   */
         /* we get the next pattern node by treating this     */
         /* multifield pattern node as if it were a single    */
         /* field pattern node that failed its constraint.    */
         /*===================================================*/

         patternPtr = GetNextFactPatternNode(TRUE,patternPtr);
        }
     }
  }

/**************************************************************/
/* ProcessMultifieldNode: Handles recursive pattern matching  */
/*  when a multifield wildcard or variable is encountered as  */
/*  a slot constraint. The pattern matching routine is called */
/*  iteratively for each possible binding of the multifield   */
/*  wildcard or variable.                                     */
/**************************************************************/
static void ProcessMultifieldNode(
  struct factPatternNode *thePattern,
  struct multifieldMarker *markers,
  struct multifieldMarker *endMark,
  int offset)
  {
   struct multifieldMarker *newMark, *oldMark;
   int repeatCount;
   struct multifield *theSlotValue;

   /*========================================*/
   /* Get a pointer to the slot value of the */
   /* multifield slot being pattern matched. */
   /*========================================*/

   theSlotValue = (struct multifield *)
     CurrentPatternFact->theProposition.theFields[thePattern->whichSlot].value;

   /*===============================================*/
   /* Save the value of the markers already stored. */
   /*===============================================*/

   oldMark = markers;

   /*===========================================*/
   /* Create a new multifield marker and append */
   /* it to the end of the current list.        */
   /*===========================================*/

   newMark = get_struct(multifieldMarker);
   newMark->whichField = thePattern->whichField - 1;
   newMark->where.whichSlotNumber = (short) thePattern->whichSlot;
   newMark->startPosition = (thePattern->whichField - 1) + offset;
   newMark->next = NULL;

   if (endMark == NULL)
     {
      markers = newMark;
      CurrentPatternMarks = markers;
     }
   else
     { endMark->next = newMark; }

   /*============================================*/
   /* Handle a multifield constraint as the last */
   /* constraint of a slot as a special case.    */
   /*============================================*/

   if (thePattern->header.endSlot)
     {
      newMark->endPosition = theSlotValue->multifieldLength -
                             (thePattern->leaveFields + 1);

      /*=======================================================*/
      /* Make sure the endPosition is never more than less one */
      /* less of the startPosition (a multifield containing no */
      /* values.                                               */
      /*=======================================================*/

      if (newMark->endPosition < newMark->startPosition)
        { newMark->endPosition = newMark->startPosition - 1; }

      /*===========================================*/
      /* Determine if the constraint is satisfied. */
      /*===========================================*/

      if ((thePattern->networkTest == NULL) ?
          TRUE :
          (EvaluatePatternExpression(thePattern,thePattern->networkTest,
                                     (int) thePattern->whichField + offset)))
        {
         /*=======================================================*/
         /* If a leaf pattern node has been successfully reached, */
         /* then the pattern has been satisified. Generate an     */
         /* alpha match to store in the pattern node.             */
         /*=======================================================*/

         if (thePattern->header.stopNode)
           { ProcessFactAlphaMatch(CurrentPatternFact,CurrentPatternMarks,thePattern); }

         /*=============================================*/
         /* Recursively continue pattern matching based */
         /* on the multifield binding just generated.   */
         /*=============================================*/

         FactPatternMatch(CurrentPatternFact,
                          thePattern->nextLevel,0,CurrentPatternMarks,newMark);
        }

      /*================================================*/
      /* Discard the multifield marker since we've done */
      /* all the pattern matching for this binding of   */
      /* the multifield slot constraint.                */
      /*================================================*/

      rtn_struct(multifieldMarker,newMark);
      if (endMark != NULL) endMark->next = NULL;
      CurrentPatternMarks = oldMark;
      return;
     }

   /*==============================================*/
   /* Perform matching for nodes beneath this one. */
   /*==============================================*/

   for (repeatCount = theSlotValue->multifieldLength -
                      (newMark->startPosition + thePattern->leaveFields);
        repeatCount >= 0;
        repeatCount--)
     {
      newMark->endPosition = newMark->startPosition + (repeatCount - 1);

      if ((thePattern->networkTest == NULL) ?
          TRUE :
          (EvaluatePatternExpression(thePattern,thePattern->networkTest,
                                     (int) thePattern->whichField + offset)))
        {
         FactPatternMatch(CurrentPatternFact,
                          thePattern->nextLevel,offset + repeatCount - 1,
                          CurrentPatternMarks,newMark);
        }
     }

    /*======================================================*/
    /* Get rid of the marker created for a multifield node. */
    /*======================================================*/

    rtn_struct(multifieldMarker,newMark);
    if (endMark != NULL) endMark->next = NULL;
    CurrentPatternMarks = oldMark;
   }

/******************************************************/
/* GetNextFactPatternNode: Returns the next node in a */
/*   pattern network tree to be traversed. The next   */
/*   node is computed using a depth first traversal.  */
/******************************************************/
static struct factPatternNode *GetNextFactPatternNode(
  int finishedMatching,
  struct factPatternNode *thePattern)
  {
   EvaluationError = FALSE;

   /*===================================================*/
   /* If pattern matching was successful at the current */
   /* node in the tree and it's possible to go deeper   */
   /* into the tree, then move down to the next level.  */
   /*===================================================*/

   if (finishedMatching == FALSE)
     { if (thePattern->nextLevel != NULL) return(thePattern->nextLevel); }

   /*================================================*/
   /* Keep backing up toward the root of the pattern */
   /* network until a side branch can be taken.      */
   /*================================================*/

   while (thePattern->rightNode == NULL)
     {
      /*========================================*/
      /* Back up to check the next side branch. */
      /*========================================*/

      thePattern = thePattern->lastLevel;

      /*======================================*/
      /* If we branched up from the root, the */
      /* entire tree has been traversed.      */
      /*======================================*/

      if (thePattern == NULL) return(NULL);

      /*===================================================*/
      /* If we branched up to a multifield node, then stop */
      /* since these nodes are handled recursively. The    */
      /* previous call to the pattern matching algorithm   */
      /* on the stack will handle backing up to the nodes  */
      /* above the multifield node in the pattern network. */
      /*===================================================*/

      if (thePattern->header.multifieldNode) return(NULL);
     }

   /*==================================*/
   /* Move on to the next side branch. */
   /*==================================*/

   return(thePattern->rightNode);
  }

/*******************************************************/
/* ProcessFactAlphaMatch: When a fact pattern has been */
/*   satisfied, this routine creates an alpha match to */
/*   store in the pattern network and then sends the   */
/*   new alpha match through the join network.         */
/*******************************************************/
static void ProcessFactAlphaMatch(
  struct fact *theFact,
  struct multifieldMarker *theMarks,
  struct factPatternNode *thePattern)
  {
   struct partialMatch *theMatch;
   struct patternMatch *listOfMatches;
   struct joinNode *listOfJoins;

  /*===========================================*/
  /* Create the partial match for the pattern. */
  /*===========================================*/

  theMatch = CreateAlphaMatch(theFact,theMarks,(struct patternNodeHeader *) &thePattern->header);

  /*=======================================================*/
  /* Add the pattern to the list of matches for this fact. */
  /*=======================================================*/

  listOfMatches = (struct patternMatch *) theFact->list;
  theFact->list = (void *) get_struct(patternMatch);
  ((struct patternMatch *) theFact->list)->next = listOfMatches;
  ((struct patternMatch *) theFact->list)->matchingPattern = (struct patternNodeHeader *) thePattern;
  ((struct patternMatch *) theFact->list)->theMatch = theMatch;

  /*================================================================*/
  /* Send the partial match to the joins connected to this pattern. */
  /*================================================================*/

  for (listOfJoins = thePattern->header.entryJoin;
       listOfJoins != NULL;
       listOfJoins = listOfJoins->rightMatchNode)
     { NetworkAssert(theMatch,listOfJoins,RHS); }
  }

/*****************************************************************/
/* EvaluatePatternExpression: Performs a faster evaluation for   */
/*   fact pattern network expressions than if EvaluateExpression */
/*   were used directly.                                         */
/*****************************************************************/
static int EvaluatePatternExpression(
  struct factPatternNode *patternPtr,
  struct expr *theTest,
  int thePosition)
  {
   DATA_OBJECT theResult;
   struct expr *oldArgument;
   int rv;

   /*=====================================*/
   /* A pattern node without a constraint */
   /* is always satisfied.                */
   /*=====================================*/

   if (theTest == NULL) return(TRUE);

   /*======================================*/
   /* Evaluate pattern network primitives. */
   /*======================================*/

   switch(theTest->type)
     {
      /*==============================================*/
      /* This primitive compares the value stored in  */
      /* a single field slot to a constant for either */
      /* equality or inequality.                      */
      /*==============================================*/

      case FACT_PN_CONSTANT1:
        oldArgument = CurrentExpression;
        CurrentExpression = theTest;
        rv = FactPNConstant1(theTest->value,&theResult);
        CurrentExpression = oldArgument;
        return(rv);

      /*=============================================*/
      /* This primitive compares the value stored in */
      /* a multifield slot to a constant for either  */
      /* equality or inequality.                     */
      /*=============================================*/

      case FACT_PN_CONSTANT2:
        oldArgument = CurrentExpression;
        CurrentExpression = theTest;
        rv = FactPNConstant2(theTest->value,&theResult);
        CurrentExpression = oldArgument;
        return(rv);

      /*================================================*/
      /* This primitive determines if a multifield slot */
      /* contains at least a certain number of fields.  */
      /*================================================*/

      case FACT_SLOT_LENGTH:
        oldArgument = CurrentExpression;
        CurrentExpression = theTest;
        rv = FactSlotLength(theTest->value,&theResult);
        CurrentExpression = oldArgument;
        return(rv);
     }

   /*==============================================*/
   /* Evaluate "or" expressions by evaluating each */
   /* argument and return TRUE if any of them      */
   /* evaluated to TRUE, otherwise return FALSE.   */
   /*==============================================*/

   if (theTest->value == PTR_OR)
     {
      for (theTest = theTest->argList;
           theTest != NULL;
           theTest = theTest->nextArg)
        {
         if (EvaluatePatternExpression(patternPtr,theTest,thePosition) == TRUE)
           {
            if (EvaluationError) return(FALSE);
            return(TRUE);
           }
         if (EvaluationError) return(FALSE);
        }

      return(FALSE);
     }

   /*===============================================*/
   /* Evaluate "and" expressions by evaluating each */
   /* argument and return FALSE if any of them      */
   /* evaluated to FALSE, otherwise return TRUE.    */
   /*===============================================*/

   else if (theTest->value == PTR_AND)
     {
      for (theTest = theTest->argList;
           theTest != NULL;
           theTest = theTest->nextArg)
        {
         if (EvaluatePatternExpression(patternPtr,theTest,thePosition) == FALSE)
           { return(FALSE); }
         if (EvaluationError) return(FALSE);
        }

      return(TRUE);
     }

   /*==========================================================*/
   /* Evaluate all other expressions using EvaluateExpression. */
   /*==========================================================*/

   if (EvaluateExpression(theTest,&theResult))
     {
      PatternNetErrorMessage(patternPtr);
      return(FALSE);
     }

   if ((theResult.value == FalseSymbol) && (theResult.type == SYMBOL))
     { return(FALSE); }

   return(TRUE);
  }

/************************************************************************/
/* PatternNetErrorMessage: Prints the informational header to the error */
/*   message that occurs when a error occurs as the  result of          */
/*   evaluating an expression in the fact pattern network. Prints the   */
/*   fact currently being pattern matched and the field number or slot  */
/*   name in the pattern from which the error originated. The error is  */
/*   then trace to the point where the pattern enters the join network  */
/*   so that the names of the rule which utilize the pattern can also   */
/*   be printed.                                                        */
/************************************************************************/
static void PatternNetErrorMessage(
  struct factPatternNode *patternPtr)
  {
   char buffer[60];
   struct templateSlot *theSlots;
   int i;

   /*=======================================*/
   /* Print the fact being pattern matched. */
   /*=======================================*/

   PrintErrorID("FACTMCH",1,TRUE);
   PrintRouter(WERROR,"This error occurred in the fact pattern network\n");
   PrintRouter(WERROR,"   Currently active fact: ");
   PrintFact(WERROR,CurrentPatternFact);
   PrintRouter(WERROR,"\n");

   /*==============================================*/
   /* Print the field position or slot name of the */
   /* pattern from which the error originated.     */
   /*==============================================*/

   if (CurrentPatternFact->whichDeftemplate->implied)
     { sprintf(buffer,"   Problem resides in field #%d\n",patternPtr->whichField); }
   else
     {
      theSlots = CurrentPatternFact->whichDeftemplate->slotList;
      for (i = 0; i < (int) patternPtr->whichSlot; i++) theSlots = theSlots->next;
      sprintf(buffer,"   Problem resides in slot %s\n",ValueToString(theSlots->slotName));
     }

   PrintRouter(WERROR,buffer);

   /*==========================================================*/
   /* Trace the pattern to its entry point to the join network */
   /* (which then traces to the defrule data structure so that */
   /* the name(s) of the rule(s) utilizing the patterns can be */
   /* printed).                                                */
   /*==========================================================*/

   TraceErrorToJoin(patternPtr,FALSE);
   PrintRouter(WERROR,"\n");
  }

/***************************************************************************/
/* TraceErrorToJoin: Traces the cause of an evaluation error which occured */
/*   in the fact pattern network to the entry join in the join network for */
/*   the pattern from which the error originated. Once the entry join is   */
/*   reached, the error is then traced to the defrule data structures so   */
/*   that the name of the rule(s) containing the pattern can be printed.   */
/***************************************************************************/
static void TraceErrorToJoin(
  struct factPatternNode *patternPtr,
  int traceRight)
  {
   struct joinNode *joinPtr;
   char buffer[60];

   while (patternPtr != NULL)
     {
      if (patternPtr->header.stopNode)
        {
         for (joinPtr = patternPtr->header.entryJoin;
              joinPtr != NULL;
              joinPtr = joinPtr->rightMatchNode)
           {
            sprintf(buffer,"      Of pattern #%d in rule(s):\n",GetPatternNumberFromJoin(joinPtr));
            PrintRouter(WERROR,buffer);
            TraceErrorToRule(joinPtr,"         ");
           }
        }
      else
        { TraceErrorToJoin(patternPtr->nextLevel,TRUE); }

      if (traceRight) patternPtr = patternPtr->rightNode;
      else patternPtr = NULL;
     }
  }

/***********************************************************************/
/* SkipFactPatternNode: During an incremental reset, only fact pattern */
/*   nodes associated with new patterns are traversed. Given a pattern */
/*   node, this routine will return TRUE if the pattern node should be */
/*   traversed during incremental reset pattern matching or FALSE if   */
/*   the node should be skipped.                                       */
/***********************************************************************/
static int SkipFactPatternNode(
  struct factPatternNode *thePattern)
  {
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY || (! INCREMENTAL_RESET))
#pragma unused(thePattern)
#endif

#if INCREMENTAL_RESET && (! RUN_TIME) && (! BLOAD_ONLY)
   if (IncrementalResetInProgress &&
       (thePattern->header.initialize == FALSE))
     { return(TRUE); }
#endif

   return(FALSE);
  }

#if INCREMENTAL_RESET

/***************************************************************/
/* MarkFactPatternForIncrementalReset: Sets the initialization */
/*  field of a fact pattern for use with incremental reset.    */
/*  This is called before an incremental reset for newly added */
/*  patterns to indicate that the pattern nodes should be      */
/*  traversed and then after an incremental reset to indicate  */
/*  that the nodes were traversed ("initialized") by the       */
/*  incremental reset.                                         */
/***************************************************************/
globle void MarkFactPatternForIncrementalReset(
  struct patternNodeHeader *thePattern,
  int value)
  {
   struct factPatternNode *patternPtr = (struct factPatternNode *) thePattern;

   /*=====================================*/
   /* We should be passed a valid pointer */
   /* to a fact pattern network node.     */
   /*=====================================*/

   Bogus(patternPtr == NULL);

   /*============================================*/
   /* If the pattern was previously initialized, */
   /* then don't bother with it.                 */
   /*============================================*/

   if (patternPtr->header.initialize == FALSE) return;

   /*======================================================*/
   /* Set the initialization field of this pattern network */
   /* node and all pattern network nodes which preceed it. */
   /*======================================================*/

   while (patternPtr != NULL)
     {
      patternPtr->header.initialize = value;
      patternPtr = patternPtr->lastLevel;
     }
  }

/**************************************************************/
/* FactsIncrementalReset: Incremental reset function for the  */
/*   fact pattern network. Asserts all facts in the fact-list */
/*   so that they repeat the pattern matching process. During */
/*   an incremental reset, newly added patterns should be the */
/*   only active patterns in the fact pattern network.        */
/**************************************************************/
globle void FactsIncrementalReset()
  {
   struct fact *factPtr;

   for (factPtr = (struct fact *) GetNextFact(NULL);
        factPtr != NULL;
        factPtr = (struct fact *) GetNextFact(factPtr))
     {
      FactPatternMatch(factPtr,
                       factPtr->whichDeftemplate->patternNetwork,
                       0,NULL,NULL);
     }
  }

#endif /* INCREMENTAL_RESET */

#endif /* DEFTEMPLATE_CONSTRUCT && DEFRULE_CONSTRUCT */


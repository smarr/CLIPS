   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*              INCREMENTAL RESET MODULE               */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides functionality for the incremental       */
/*   reset of the pattern and join networks when a new       */
/*   rule is added.                                          */
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

#define _INCRRSET_SOURCE_

#include "setup.h"

#include <stdio.h>
#define _STDIO_INCLUDED_

#if DEFRULE_CONSTRUCT && INCREMENTAL_RESET

#include "constant.h"
#include "agenda.h"
#include "router.h"
#include "drive.h"
#include "pattern.h"
#include "evaluatn.h"
#include "argacces.h"
#include "incrrset.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if (! RUN_TIME) && (! BLOAD_ONLY)
   static void                    MarkNetworkForIncrementalReset(struct defrule *,int);
   static void                    CheckForPrimableJoins(struct defrule *);
   static void                    PrimeJoin(struct joinNode *);
   static void                    MarkPatternForIncrementalReset(int,struct patternNodeHeader *,int);
#endif

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle BOOLEAN              IncrementalResetInProgress = FALSE;

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static BOOLEAN              IncrementalResetFlag = TRUE;

/**************************************************************/
/* IncrementalReset: Incrementally resets the specified rule. */
/**************************************************************/
globle void IncrementalReset(
  struct defrule *tempRule)
  {
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(tempRule)
#endif

#if (! RUN_TIME) && (! BLOAD_ONLY)
   struct defrule *tempPtr;
   struct patternParser *theParser;

   /*================================================*/
   /* If incremental reset is disabled, then return. */
   /*================================================*/

   if (! GetIncrementalReset()) return;

   /*=====================================================*/
   /* Mark the pattern and join network data structures   */
   /* associated with the rule being incrementally reset. */
   /*=====================================================*/

   MarkNetworkForIncrementalReset(tempRule,TRUE);

   /*==========================*/
   /* Begin incremental reset. */
   /*==========================*/

   IncrementalResetInProgress = TRUE;

   /*============================================================*/
   /* If the new rule shares patterns or joins with other rules, */
   /* then it is necessary to update its join network based on   */
   /* existing partial matches it shares with other rules.       */
   /*============================================================*/

   for (tempPtr = tempRule;
        tempPtr != NULL;
        tempPtr = tempPtr->disjunct)
     { CheckForPrimableJoins(tempPtr); }

   /*===============================================*/
   /* Filter existing data entities through the new */
   /* portions of the pattern and join networks.    */
   /*===============================================*/

   for (theParser = ListOfPatternParsers;
        theParser != NULL;
        theParser = theParser->next)
     {
      if (theParser->incrementalResetFunction != NULL)
        { (*theParser->incrementalResetFunction)(); }
     }

   /*========================*/
   /* End incremental reset. */
   /*========================*/

   IncrementalResetInProgress = FALSE;

   /*====================================================*/
   /* Remove the marks in the pattern and join networks. */
   /*====================================================*/

   MarkNetworkForIncrementalReset(tempRule,FALSE);
#endif
  }

#if (! RUN_TIME) && (! BLOAD_ONLY)

/**********************************************************************/
/* MarkNetworkForIncrementalReset: Coordinates marking the initialize */
/*   flags in the pattern and join networks both before and after an  */
/*   incremental reset.                                               */
/**********************************************************************/
static void MarkNetworkForIncrementalReset(
  struct defrule *tempRule,
  int value)
  {
   struct joinNode *joinPtr;
   struct patternNodeHeader *patternPtr;

   /*============================================*/
   /* Loop through each of the rule's disjuncts. */
   /*============================================*/

   for (;
        tempRule != NULL;
        tempRule = tempRule->disjunct)
     {
      /*============================================*/
      /* Loop through each of the disjunct's joins. */
      /*============================================*/

      for (joinPtr = tempRule->lastJoin;
           joinPtr != NULL;
           joinPtr = GetPreviousJoin(joinPtr))
        {
         /*================*/
         /* Mark the join. */
         /*================*/

         joinPtr->marked = FALSE; /* GDR 6.05 */
         if ((joinPtr->initialize) && (joinPtr->joinFromTheRight == FALSE))
           {
            joinPtr->initialize = value;
            patternPtr = (struct patternNodeHeader *) GetPatternForJoin(joinPtr);
            MarkPatternForIncrementalReset((int) joinPtr->rhsType,patternPtr,value);
           }
        }
     }
  }

/*******************************************************************************/
/* CheckForPrimableJoins: Updates the joins of a rule for an incremental reset */
/*   if portions of that rule are shared with other rules that have already    */
/*   been incrementally reset. A join for a new rule will be updated if it is  */
/*   marked for initialization and either its parent join or its associated    */
/*   entry pattern node has not been marked for initialization. The function   */
/*   PrimeJoin is used to update joins which meet these criteria.              */
/*******************************************************************************/
static void CheckForPrimableJoins(
  struct defrule *tempRule)
  {
   struct joinNode *joinPtr;
   struct partialMatch *theList;

   /*========================================*/
   /* Loop through each of the rule's joins. */
   /*========================================*/

   for (joinPtr = tempRule->lastJoin;
        joinPtr != NULL;
        joinPtr = GetPreviousJoin(joinPtr))
     {
      /*===============================*/
      /* Update the join if necessary. */
      /*===============================*/

      if ((joinPtr->initialize) && (! joinPtr->marked)) /* GDR 6.05 */
        {
         if (joinPtr->firstJoin == TRUE)
           {
            if (((struct patternNodeHeader *) GetPatternForJoin(joinPtr))->initialize == FALSE)
              {
               PrimeJoin(joinPtr);
               joinPtr->marked = TRUE; /* GDR 6.05 */
              }
           }
         else if (joinPtr->lastLevel->initialize == FALSE)
           {
            PrimeJoin(joinPtr);
            joinPtr->marked = TRUE; /* GDR 6.05 */
           }
        }

      /*================================================================*/
      /* If the join is associated with a rule activation (i.e. partial */
      /* matches that reach this join cause an activation to be placed  */
      /* on the agenda), then add activations to the agenda for the     */
      /* rule being  incrementally reset.                               */
      /*================================================================*/

      else if (joinPtr->ruleToActivate == tempRule)
        {
         for (theList = joinPtr->beta;
              theList != NULL;
              theList = theList->next)
           { AddActivation(tempRule,theList); }
        }
     }
  }

/****************************************************************************/
/* PrimeJoin: Updates a join in a rule for an incremental reset. Joins are  */
/*   updated by "priming" them only if the join (or its associated pattern) */
/*   is shared with other rules that have already been incrementally reset. */
/*   A join for a new rule will be updated if it is marked for              */
/*   initialization and either its parent join or its associated entry      */
/*   pattern node has not been marked for initialization.                   */
/****************************************************************************/
static void PrimeJoin(
  struct joinNode *joinPtr)
  {
   struct partialMatch *theList;

   /*===========================================================*/
   /* If the join is the first join of a rule, then send all of */
   /* the partial matches from the alpha memory of the pattern  */
   /* associated with this join to the join for processing and  */
   /* the priming process is then complete.                     */
   /*===========================================================*/

   if (joinPtr->firstJoin == TRUE)
     {
      for (theList = ((struct patternNodeHeader *) joinPtr->rightSideEntryStructure)->alphaMemory;
           theList != NULL;
           theList = theList->next)
        { NetworkAssert(theList,joinPtr,RHS); }
      return;
     }

   /*======================================================*/
   /* If the join already has partial matches in its beta  */
   /* memory, then don't bother priming it. I don't recall */
   /* if this situation is possible.                       */
   /*======================================================*/

   if (joinPtr->beta != NULL) return;

   /*================================================================*/
   /* Send all partial matches from the preceding join to this join. */
   /*================================================================*/

   for (theList = joinPtr->lastLevel->beta;
        theList != NULL;
        theList = theList->next)
     {
      if (! theList->counterf) /* 6.05 incremental reset bug fix */
        { NetworkAssert(theList,joinPtr,LHS); }
     }
  }

/*********************************************************************/
/* MarkPatternForIncrementalReset: Given a pattern node and its type */
/*   (fact, instance, etc.), calls the appropriate function to mark  */
/*   the pattern for an incremental reset. Used to mark the pattern  */
/*   nodes both before and after an incremental reset.               */
/*********************************************************************/
static void MarkPatternForIncrementalReset(
  int rhsType,
  struct patternNodeHeader *theHeader,
  int value)
  {
   struct patternParser *tempParser;

   tempParser = GetPatternParser(rhsType);

   if (tempParser != NULL)
     {
      if (tempParser->markIRPatternFunction != NULL)
        { (*tempParser->markIRPatternFunction)(theHeader,value); }
     }
  }

#endif

/*********************************************/
/* GetIncrementalReset: C access routine for */
/*   the get-incremental-reset command.      */
/*********************************************/
globle BOOLEAN GetIncrementalReset()
  { return(IncrementalResetFlag); }

/*********************************************/
/* SetIncrementalReset: C access routine for */
/*   the set-incremental-reset command.      */
/*********************************************/
globle BOOLEAN SetIncrementalReset(
  int value)
  {
   int ov;

   ov = IncrementalResetFlag;
   if (GetNextDefrule(NULL) != NULL) return(-1);
   IncrementalResetFlag = value;
   return(ov);
  }

/****************************************************/
/* SetIncrementalResetCommand: H/L access routine   */
/*   for the set-incremental-reset command.         */
/****************************************************/
globle int SetIncrementalResetCommand()
  {
   int oldValue;
   DATA_OBJECT argPtr;

   oldValue = GetIncrementalReset();

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/

   if (ArgCountCheck("set-incremental-reset",EXACTLY,1) == -1)
     { return(oldValue); }

   /*=========================================*/
   /* The incremental reset behavior can't be */
   /* changed when rules are loaded.          */
   /*=========================================*/

   if (GetNextDefrule(NULL) != NULL)
     {
      PrintErrorID("INCRRSET",1,FALSE);
      PrintRouter(WERROR,"The incremental reset behavior cannot be changed with rules loaded.\n");
      SetEvaluationError(TRUE);
      return(oldValue);
     }

   /*==================================================*/
   /* The symbol FALSE disables incremental reset. Any */
   /* other value enables incremental reset.           */
   /*==================================================*/

   RtnUnknown(1,&argPtr);

   if ((argPtr.value == FalseSymbol) && (argPtr.type == SYMBOL))
     { SetIncrementalReset(FALSE); }
   else
     { SetIncrementalReset(TRUE); }

   /*=======================*/
   /* Return the old value. */
   /*=======================*/

   return(oldValue);
  }

/****************************************************/
/* GetIncrementalResetCommand: H/L access routine   */
/*   for the get-incremental-reset command.         */
/****************************************************/
globle int GetIncrementalResetCommand()
  {
   int oldValue;

   oldValue = GetIncrementalReset();

   if (ArgCountCheck("get-incremental-reset",EXACTLY,0) == -1)
     { return(oldValue); }

   return(oldValue);
  }

#endif /* DEFRULE_CONSTRUCT && INCREMENTAL_RESET */

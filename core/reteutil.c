   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                 RETE UTILITY MODULE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides a set of utility functions useful to    */
/*   other modules.                                          */
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

#define _RETEUTIL_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "setup.h"

#if DEFRULE_CONSTRUCT

#include "memalloc.h"
#include "router.h"
#include "retract.h"
#include "drive.h"
#include "incrrset.h"
#include "pattern.h"
#include "match.h"
#include "moduldef.h"
#include "reteutil.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    TraceErrorToRuleDriver(struct joinNode *,char *);

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle struct partialMatch   *GlobalLHSBinds = NULL;
   Thread globle struct partialMatch   *GlobalRHSBinds = NULL;
   Thread globle struct joinNode       *GlobalJoin = NULL;

/***********************************************************/
/* PrintPartialMatch: Prints out the list of fact indices  */
/*   and/or instance names associated with a partial match */
/*   or rule instantiation.                                */
/***********************************************************/
globle void PrintPartialMatch(
  char *logicalName,
  struct partialMatch *list)
  {
   struct patternEntity *matchingItem;
   short int i;

   for (i = 0; i < (int) list->bcount;)
     {
      if (get_nth_pm_match(list,i)->matchingItem != NULL)
        {
         matchingItem = get_nth_pm_match(list,i)->matchingItem;
         if (matchingItem != NULL) (*matchingItem->theInfo->base.shortPrintFunction)(logicalName,matchingItem);
        }
      i++;
      if (i < (int) list->bcount) PrintRouter(logicalName,",");
     }
  }

/**********************************************/
/* CopyPartialMatch:  Copies a partial match. */
/**********************************************/
globle struct partialMatch *CopyPartialMatch(
  struct partialMatch *list,
  int addActivationSlot,
  int addDependencySlot)
  {
   struct partialMatch *linker;
   short int i;

   linker = get_var_struct(partialMatch,sizeof(struct genericMatch) *
                                        (list->bcount + addActivationSlot + addDependencySlot - 1));

   linker->next = NULL;
   linker->betaMemory = TRUE;
   linker->busy = FALSE;
   linker->activationf = addActivationSlot;
   linker->dependentsf = addDependencySlot;
   linker->notOriginf = FALSE;
   linker->counterf = FALSE;
   linker->bcount = list->bcount;

   for (i = 0; i < (int) linker->bcount; i++) linker->binds[i] = list->binds[i];

   if (addActivationSlot) linker->binds[i++].gm.theValue = NULL;
   if (addDependencySlot) linker->binds[i].gm.theValue = NULL;

   return(linker);
  }

/****************************************************/
/* MergePartialMatches: Merges two partial matches. */
/****************************************************/
globle struct partialMatch *MergePartialMatches(
  struct partialMatch *list1,
  struct partialMatch *list2,
  int addActivationSlot,
  int addDependencySlot)
  {
   struct partialMatch *linker;
   short int i, j;

   linker = get_var_struct(partialMatch,
                           sizeof(struct genericMatch) *
                            (list1->bcount + list2->bcount + addActivationSlot + addDependencySlot - 1));

   linker->next = NULL;
   linker->betaMemory = TRUE;
   linker->busy = FALSE;
   linker->activationf = addActivationSlot;
   linker->dependentsf = addDependencySlot;
   linker->notOriginf = FALSE;
   linker->counterf = FALSE;
   linker->bcount = list1->bcount + list2->bcount;

   for (i = 0; i < (int) list1->bcount; i++)
     { linker->binds[i] = list1->binds[i]; }

   for (i = (short) list1->bcount, j = 0; i < (short) linker->bcount; i++, j++)
     { linker->binds[i] = list2->binds[j]; }

   if (addActivationSlot) linker->binds[i++].gm.theValue = NULL;
   if (addDependencySlot) linker->binds[i].gm.theValue = NULL;

   return(linker);
  }

/*******************************************************************/
/* InitializePatternHeader: Initializes a pattern header structure */
/*   (used by the fact and instance pattern matchers).             */
/*******************************************************************/
globle void InitializePatternHeader(
  struct patternNodeHeader *theHeader)
  {
   theHeader->entryJoin = NULL;
   theHeader->alphaMemory = NULL;
   theHeader->endOfQueue = NULL;
   theHeader->singlefieldNode = FALSE;
   theHeader->multifieldNode = FALSE;
   theHeader->stopNode = FALSE;
#if INCREMENTAL_RESET && (! RUN_TIME)
   theHeader->initialize = GetIncrementalReset();
#else
   theHeader->initialize = FALSE;
#endif
   theHeader->marked = FALSE;
   theHeader->beginSlot = FALSE;
   theHeader->endSlot = FALSE;
  }

/******************************************************************/
/* CreateAlphaMatch: Given a pointer to an entity (such as a fact */
/*   or instance) which matched a pattern, this function creates  */
/*   a partial match suitable for storing in the alpha memory of  */
/*   the pattern network. Note that the multifield markers which  */
/*   are passed as a calling argument are copied (thus the caller */
/*   is still responsible for freeing these data structures).     */
/******************************************************************/
globle struct partialMatch *CreateAlphaMatch(
  void *theEntity,
  struct multifieldMarker *markers,
  struct patternNodeHeader *theHeader)
  {
   struct partialMatch *theMatch;
   struct alphaMatch *afbtemp;

   /*==================================================*/
   /* Create the alpha match and intialize its values. */
   /*==================================================*/

   theMatch = get_struct(partialMatch);
   theMatch->next = NULL;
   theMatch->betaMemory = FALSE;
   theMatch->busy = FALSE;
   theMatch->activationf = FALSE;
   theMatch->dependentsf = FALSE;
   theMatch->notOriginf = FALSE;
   theMatch->counterf = FALSE;
   theMatch->bcount = 1;

   afbtemp = get_struct(alphaMatch);
   afbtemp->next = NULL;
   afbtemp->matchingItem = (struct patternEntity *) theEntity;

   if (markers != NULL)
     { afbtemp->markers = CopyMultifieldMarkers(markers); }
   else
     { afbtemp->markers = NULL; }

   theMatch->binds[0].gm.theMatch = afbtemp;

   /*====================================*/
   /* Store the alpha match in the alpha */
   /* memory of the pattern node.        */
   /*====================================*/

   if (theHeader->endOfQueue == NULL)
     {
      theHeader->alphaMemory = theMatch;
      theHeader->endOfQueue = theMatch;
     }
   else
     {
      theHeader->endOfQueue->next = theMatch;
      theHeader->endOfQueue = theMatch;
     }

   /*===================================================*/
   /* Return a pointer to the newly create alpha match. */
   /*===================================================*/

   return(theMatch);
  }

/*********************************************************/
/* AddSingleMatch: Combines an alpha match and a partial */
/*   match into a new partial match.                     */
/*********************************************************/
globle struct partialMatch *AddSingleMatch(
  struct partialMatch *list,
  struct alphaMatch *afb,
  int addActivationSlot,
  int addDependencySlot)
  {
   struct partialMatch *linker;
   short int i;

   linker = get_var_struct(partialMatch,sizeof(struct genericMatch) *
                                        (list->bcount + addActivationSlot +
                                        addDependencySlot));

   linker->next = NULL;
   linker->betaMemory = TRUE;
   linker->busy = FALSE;
   linker->activationf = addActivationSlot;
   linker->dependentsf = addDependencySlot;
   linker->notOriginf = FALSE;
   linker->counterf = FALSE;
   linker->bcount = list->bcount + 1;

   for (i = 0; i < (int) list->bcount; i++)
     { linker->binds[i] = list->binds[i]; }

   set_nth_pm_match(linker,i++,afb);

   if (addActivationSlot) linker->binds[i++].gm.theValue = NULL;
   if (addDependencySlot) linker->binds[i].gm.theValue = NULL;

   return(linker);
  }

/*******************************************/
/* CopyMultifieldMarkers: Copies a list of */
/*   multifieldMarker data structures.     */
/*******************************************/
struct multifieldMarker *CopyMultifieldMarkers(
  struct multifieldMarker *theMarkers)
  {
   struct multifieldMarker *head = NULL, *lastMark = NULL, *newMark;

   while (theMarkers != NULL)
     {
      newMark = get_struct(multifieldMarker);
      newMark->next = NULL;
      newMark->whichField = theMarkers->whichField;
      newMark->where = theMarkers->where;
      newMark->startPosition = theMarkers->startPosition;
      newMark->endPosition = theMarkers->endPosition;

      if (lastMark == NULL)
        { head = newMark; }
      else
        { lastMark->next = newMark; }
      lastMark = newMark;

      theMarkers = theMarkers->next;
     }

   return(head);
  }

/***************************************************************/
/* NewPseudoFactPartialMatch: Creates a partial structure that */
/*   indicates the "pseudo" fact to which a not pattern CE has */
/*   been bound. Since a non-existant fact has no fact index,  */
/*   the partial match structure is given a pseudo fact index  */
/*   (a unique negative integer). Note that a "pseudo" fact    */
/*   can also be used as a "pseudo" instance.                  */
/***************************************************************/
globle struct partialMatch *NewPseudoFactPartialMatch()
  {
   struct partialMatch *linker;
   struct alphaMatch *tempAlpha;

   linker = get_struct(partialMatch);
   linker->next = NULL;
   linker->betaMemory = TRUE;
   linker->busy = FALSE;
   linker->activationf = FALSE;
   linker->dependentsf = FALSE;
   linker->notOriginf = TRUE;
   linker->counterf = FALSE;
   linker->bcount = 0;
   tempAlpha = get_struct(alphaMatch);
   tempAlpha->next = NULL;
   tempAlpha->matchingItem = NULL;
   tempAlpha->markers = NULL;

   linker->binds[0].gm.theMatch = tempAlpha;
   return(linker);
  }

/******************************************************************/
/* FlushAlphaBetaMemory: Returns all partial matches in a list of */
/*   partial matches either directly to the pool of free memory   */
/*   or to the list of GarbagePartialMatches. Partial matches     */
/*   stored in alpha memories and partial matches which store the */
/*   information for pseudo facts (for not CEs) may be referred   */
/*   to by other data structures and thus must be placed on the   */
/*   list of GarbagePartialMatches.                               */
/******************************************************************/
globle void FlushAlphaBetaMemory(
  struct partialMatch *pfl)
  {
   struct partialMatch *pfltemp;

   while (pfl != NULL)
     {
      pfltemp = pfl->next;

      if (((pfl->notOriginf) && (pfl->counterf == FALSE)) ||
          (pfl->betaMemory == FALSE))
        {
         pfl->next = GarbagePartialMatches;
         GarbagePartialMatches = pfl;
        }
      else
        { ReturnPartialMatch(pfl); }

      pfl = pfltemp;
     }
  }

/***********************************************************************/
/* GetPatternNumberFromJoin: Given a pointer to a join associated with */
/*   a pattern CE, returns an integer representing the position of the */
/*   pattern CE in the rule (e.g. first, second, third).               */
/***********************************************************************/
globle int GetPatternNumberFromJoin(
  struct joinNode *joinPtr)
  {
   int whichOne = 0;

   while (joinPtr != NULL)
     {
      if (joinPtr->joinFromTheRight)
        { joinPtr = (struct joinNode *) joinPtr->rightSideEntryStructure; }
      else
        {
         whichOne++;
         joinPtr = joinPtr->lastLevel;
        }
     }

   return(whichOne);
  }

/************************************************************************/
/* TraceErrorToRule: Prints an error message when a error occurs as the */
/*   result of evaluating an expression in the pattern network. Used to */
/*   indicate which rule caused the problem.                            */
/************************************************************************/
globle void TraceErrorToRule(
  struct joinNode *joinPtr,
  char *indentSpaces)
  {
   MarkRuleNetwork(0);
   TraceErrorToRuleDriver(joinPtr,indentSpaces);
  }

/**************************************************************/
/* TraceErrorToRuleDriver: Driver code for printing out which */
/*   rule caused a pattern or join network error.             */
/**************************************************************/
static void TraceErrorToRuleDriver(
  struct joinNode *joinPtr,
  char *indentSpaces)
  {
   char *name;

   while (joinPtr != NULL)
     {
      if (joinPtr->marked)
        { /* Do Nothing */ }
      else if (joinPtr->ruleToActivate != NULL)
        {
         joinPtr->marked = 1;
         name = GetDefruleName(joinPtr->ruleToActivate);
         PrintRouter(WERROR,indentSpaces);
         PrintRouter(WERROR,name);
         PrintRouter(WERROR,"\n");
        }
      else
        {
         joinPtr->marked = 1;
         TraceErrorToRuleDriver(joinPtr->nextLevel,indentSpaces);
        }

      joinPtr = joinPtr->rightDriveNode;
     }
  }

/********************************************************/
/* MarkRuleNetwork: Sets the marked flag in each of the */
/*   joins in the join network to the specified value.  */
/********************************************************/
globle void MarkRuleNetwork(
  int value)
  {
   struct defrule *rulePtr;
   struct joinNode *joinPtr;
   struct defmodule *modulePtr;

   /*===========================*/
   /* Loop through each module. */
   /*===========================*/

   SaveCurrentModule();
   for (modulePtr = (struct defmodule *) GetNextDefmodule(NULL);
        modulePtr != NULL;
        modulePtr = (struct defmodule *) GetNextDefmodule(modulePtr))
     {
      SetCurrentModule((void *) modulePtr);

      /*=========================*/
      /* Loop through each rule. */
      /*=========================*/

      rulePtr = (struct defrule *) GetNextDefrule(NULL);
      while (rulePtr != NULL)
        {
         /*=============================*/
         /* Mark each join for the rule */
         /* with the specified value.   */
         /*=============================*/

         joinPtr = rulePtr->lastJoin;
         while (joinPtr != NULL)
           {
            joinPtr->marked = value;
            joinPtr = GetPreviousJoin(joinPtr);
           }

         /*=================================*/
         /* Move on to the next rule or the */
         /* next disjunct for this rule.    */
         /*=================================*/

         if (rulePtr->disjunct != NULL) rulePtr = rulePtr->disjunct;
         else rulePtr = (struct defrule *) GetNextDefrule(rulePtr);
        }

     }

   RestoreCurrentModule();
  }

#if (CONSTRUCT_COMPILER || BLOAD_AND_BSAVE) && (! RUN_TIME)

/*************************************************************/
/* TagRuleNetwork: Assigns each join in the join network and */
/*   each defrule data structure with a unique integer ID.   */
/*   Also counts the number of defrule and joinNode data     */
/*   structures currently in use.                            */
/*************************************************************/
globle void TagRuleNetwork(
  long int *moduleCount,
  long int *ruleCount,
  long int *joinCount)
  {
   struct defmodule *modulePtr;
   struct defrule *rulePtr;
   struct joinNode *joinPtr;

   *moduleCount = 0;
   *ruleCount = 0;
   *joinCount = 0;

   MarkRuleNetwork(0);

   /*===========================*/
   /* Loop through each module. */
   /*===========================*/

   for (modulePtr = (struct defmodule *) GetNextDefmodule(NULL);
        modulePtr != NULL;
        modulePtr = (struct defmodule *) GetNextDefmodule(modulePtr))
     {
      (*moduleCount)++;
      SetCurrentModule((void *) modulePtr);

      /*=========================*/
      /* Loop through each rule. */
      /*=========================*/

      rulePtr = (struct defrule *) GetNextDefrule(NULL);

      while (rulePtr != NULL)
        {
         rulePtr->header.bsaveID = *ruleCount;
         (*ruleCount)++;

         /*=========================*/
         /* Loop through each join. */
         /*=========================*/

         for (joinPtr = rulePtr->lastJoin;
              joinPtr != NULL;
              joinPtr = GetPreviousJoin(joinPtr))
           {
            if (joinPtr->marked == 0)
              {
               joinPtr->marked = 1;
               joinPtr->bsaveID = *joinCount;
               (*joinCount)++;
              }
           }

         if (rulePtr->disjunct != NULL) rulePtr = rulePtr->disjunct;
         else rulePtr = (struct defrule *) GetNextDefrule(rulePtr);
        }
     }
  }

#endif /* (CONSTRUCT_COMPILER || BLOAD_AND_BSAVE) && (! RUN_TIME) */

#endif /* DEFRULE_CONSTRUCT */






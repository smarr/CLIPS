   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                RULE COMMANDS MODULE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides the matches command. Also provides the  */
/*   the developer commands show-joins and rule-complexity.  */
/*   Also provides the initialization routine which          */
/*   registers rule commands found in other modules.         */
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

#define _RULECOM_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include "setup.h"

#if DEFRULE_CONSTRUCT

#include "constant.h"
#include "memalloc.h"
#include "evaluatn.h"
#include "extnfunc.h"
#include "engine.h"
#include "watch.h"
#include "crstrtgy.h"
#include "constrct.h"
#include "argacces.h"
#include "router.h"
#include "reteutil.h"
#include "ruledlt.h"
#include "pattern.h"

#if BLOAD || BLOAD_AND_BSAVE || BLOAD_ONLY
#include "rulebin.h"
#endif

#if LOGICAL_DEPENDENCIES
#include "lgcldpnd.h"
#endif

#if INCREMENTAL_RESET
#include "incrrset.h"
#endif

#include "rulecom.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if DEVELOPER
   static void                    ShowJoins(void *);
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

#if DEVELOPER && (! RUN_TIME) && (! BLOAD_ONLY)
   Thread static int                  WatchRuleAnalysis = OFF;
#endif

/****************************************************************/
/* DefruleCommands: Initializes defrule commands and functions. */
/****************************************************************/
globle void DefruleCommands()
  {
#if ! RUN_TIME
   DefineFunction2("run",'v', PTIF RunCommand,"RunCommand", "*1i");
   DefineFunction2("halt",'v', PTIF HaltCommand,"HaltCommand","00");
   DefineFunction2("focus",'b', PTIF FocusCommand,"FocusCommand", "1*w");
   DefineFunction2("clear-focus-stack",'v',PTIF ClearFocusStackCommand,
                                       "ClearFocusStackCommand","00");
   DefineFunction2("get-focus-stack",'m',PTIF GetFocusStackFunction,
                                     "GetFocusStackFunction","00");
   DefineFunction2("pop-focus",'w',PTIF PopFocusFunction,
                               "PopFocusFunction","00");
   DefineFunction2("get-focus",'w',PTIF GetFocusFunction,
                               "GetFocusFunction","00");
#if DEBUGGING_FUNCTIONS
   DefineFunction2("set-break",'v', PTIF SetBreakCommand,
                               "SetBreakCommand","11w");
   DefineFunction2("remove-break",'v', PTIF RemoveBreakCommand,
                                  "RemoveBreakCommand", "*1w");
   DefineFunction2("show-breaks",'v', PTIF ShowBreaksCommand,
                                 "ShowBreaksCommand", "01w");
   DefineFunction2("matches",'v',PTIF MatchesCommand,"MatchesCommand","11w");
   DefineFunction2("list-focus-stack",'v', PTIF ListFocusStackCommand,
                                      "ListFocusStackCommand", "00");
#if LOGICAL_DEPENDENCIES
   DefineFunction2("dependencies", 'v', PTIF DependenciesCommand,
                                   "DependenciesCommand", "11h");
   DefineFunction2("dependents",   'v', PTIF DependentsCommand,
                                   "DependentsCommand", "11h");
#endif /* LOGICAL_DEPENDENCIES */

#endif /* DEBUGGING_FUNCTIONS */

#if INCREMENTAL_RESET
   DefineFunction2("get-incremental-reset",'b',
                   GetIncrementalResetCommand,"GetIncrementalResetCommand","00");
   DefineFunction2("set-incremental-reset",'b',
                   SetIncrementalResetCommand,"SetIncrementalResetCommand","11");
#endif /* INCREMENTAL_RESET */

#if CONFLICT_RESOLUTION_STRATEGIES
   DefineFunction2("get-strategy", 'w', PTIF GetStrategyCommand,  "GetStrategyCommand", "00");
   DefineFunction2("set-strategy", 'w', PTIF SetStrategyCommand,  "SetStrategyCommand", "11w");
#endif /* CONFLICT_RESOLUTION_STRATEGIES */

#if DEVELOPER && (! BLOAD_ONLY)
   DefineFunction2("rule-complexity",'l', PTIF RuleComplexityCommand,"RuleComplexityCommand", "11w");
   DefineFunction2("show-joins",   'v', PTIF ShowJoinsCommand,    "ShowJoinsCommand", "11w");
   AddWatchItem("rule-analysis",0,&WatchRuleAnalysis,0,NULL,NULL);
#endif /* DEVELOPER && (! BLOAD_ONLY) */

#endif /* ! RUN_TIME */
  }

#if DEBUGGING_FUNCTIONS

/****************************************/
/* MatchesCommand: H/L access routine   */
/*   for the matches command.           */
/****************************************/
globle void MatchesCommand()
  {
   char *ruleName;
   void *rulePtr;

   ruleName = GetConstructName("matches","rule name");
   if (ruleName == NULL) return;

   rulePtr = FindDefrule(ruleName);
   if (rulePtr == NULL)
     {
      CantFindItemErrorMessage("defrule",ruleName);
      return;
     }

   Matches(rulePtr);
  }

/*********************************/
/* Matches: C access routine for */
/*   the matches command.        */
/*********************************/
globle BOOLEAN Matches(
  void *theRule)
  {
   struct defrule *rulePtr, *tmpPtr;
   struct partialMatch *listOfMatches, **theStorage;
   struct joinNode *theJoin, *lastJoin;
   int i, depth;
   ACTIVATION *agendaPtr;
   int flag;
   int matchesDisplayed;

   /*=================================================*/
   /* Loop through each of the disjuncts for the rule */
   /*=================================================*/

   for (rulePtr = (struct defrule *) theRule, tmpPtr = rulePtr;
        rulePtr != NULL;
        rulePtr = rulePtr->disjunct)
     {
      /*======================================*/
      /* Determine the last join in the rule. */
      /*======================================*/

      lastJoin = rulePtr->lastJoin;

      /*===================================*/
      /* Determine the number of patterns. */
      /*===================================*/

      depth = GetPatternNumberFromJoin(lastJoin);

      /*=========================================*/
      /* Store the alpha memory partial matches. */
      /*=========================================*/

      theStorage = (struct partialMatch **)
                   genalloc((unsigned) (depth * sizeof(struct partialMatch)));

      theJoin = lastJoin;
      i = depth - 1;
      while (theJoin != NULL)
        {
         if (theJoin->joinFromTheRight)
           { theJoin = (struct joinNode *) theJoin->rightSideEntryStructure; }
         else
           {
            theStorage[i] = ((struct patternNodeHeader *) theJoin->rightSideEntryStructure)->alphaMemory;
            i--;
            theJoin = theJoin->lastLevel;
           }
        }

      /*========================================*/
      /* List the alpha memory partial matches. */
      /*========================================*/

      for (i = 0; i < depth; i++)
        {
         if (GetHaltExecution() == TRUE)
           {
            genfree(theStorage,(unsigned) (depth * sizeof(struct partialMatch)));
            return(TRUE);
           }

         PrintRouter(WDISPLAY,"Matches for Pattern ");
         PrintLongInteger(WDISPLAY,(long int) i + 1);
         PrintRouter(WDISPLAY,"\n");

         listOfMatches = theStorage[i];
         if (listOfMatches == NULL) PrintRouter(WDISPLAY," None\n");

         while (listOfMatches != NULL)
           {
            if (GetHaltExecution() == TRUE)
              {
               genfree(theStorage,(unsigned) (depth * sizeof(struct partialMatch)));
               return(TRUE);
              }
            PrintPartialMatch(WDISPLAY,listOfMatches);
            PrintRouter(WDISPLAY,"\n");
            listOfMatches = listOfMatches->next;
           }
        }

      genfree(theStorage,(unsigned) (depth * sizeof(struct partialMatch)));

      /*========================================*/
      /* Store the beta memory partial matches. */
      /*========================================*/

      depth = lastJoin->depth;
      theStorage = (struct partialMatch **) genalloc((unsigned) (depth * sizeof(struct partialMatch)));

      theJoin = lastJoin;
      for (i = depth - 1; i >= 0; i--)
        {
         theStorage[i] = theJoin->beta;
         theJoin = theJoin->lastLevel;
        }

      /*=======================================*/
      /* List the beta memory partial matches. */
      /*=======================================*/

      for (i = 1; i < depth; i++)
        {
         if (GetHaltExecution() == TRUE)
           {
            genfree(theStorage,(unsigned) (depth * sizeof(struct partialMatch)));
            return(TRUE);
           }

         matchesDisplayed = 0;
         PrintRouter(WDISPLAY,"Partial matches for CEs 1 - ");
         PrintLongInteger(WDISPLAY,(long int) i + 1);
         PrintRouter(WDISPLAY,"\n");
         listOfMatches = theStorage[i];

         while (listOfMatches != NULL)
           {
            if (GetHaltExecution() == TRUE)
              {
               genfree(theStorage,(unsigned) (depth * sizeof(struct partialMatch)));
               return(TRUE);
              }

            if (listOfMatches->counterf == FALSE)
              {
               matchesDisplayed++;
               PrintPartialMatch(WDISPLAY,listOfMatches);
               PrintRouter(WDISPLAY,"\n");
              }
            listOfMatches = listOfMatches->next;
           }

         if (matchesDisplayed == 0) { PrintRouter(WDISPLAY," None\n"); }
        }

      genfree(theStorage,(unsigned) (depth * sizeof(struct partialMatch)));
     }

   /*===================*/
   /* List activations. */
   /*===================*/

   rulePtr = tmpPtr;
   PrintRouter(WDISPLAY,"Activations\n");
   flag = 1;
   for (agendaPtr = (struct activation *) GetNextActivation(NULL);
        agendaPtr != NULL;
        agendaPtr = (struct activation *) GetNextActivation(agendaPtr))
     {
      if (GetHaltExecution() == TRUE) return(TRUE);

      if (((struct activation *) agendaPtr)->theRule->header.name == rulePtr->header.name)
        {
         flag = 0;
         PrintPartialMatch(WDISPLAY,GetActivationBasis(agendaPtr));
         PrintRouter(WDISPLAY,"\n");
        }
     }

   if (flag) PrintRouter(WDISPLAY," None\n");

   return(TRUE);
  }

#endif /* DEBUGGING_FUNCTIONS */

#if DEVELOPER
/***********************************************/
/* RuleComplexityCommand: H/L access routine   */
/*   for the rule-complexity function.         */
/***********************************************/
globle long RuleComplexityCommand()
  {
   char *ruleName;
   struct defrule *rulePtr;

   ruleName = GetConstructName("rule-complexity","rule name");
   if (ruleName == NULL) return(-1);

   rulePtr = (struct defrule *) FindDefrule(ruleName);
   if (rulePtr == NULL)
     {
      CantFindItemErrorMessage("defrule",ruleName);
      return(-1);
     }

   return(rulePtr->complexity);
  }

/******************************************/
/* ShowJoinsCommand: H/L access routine   */
/*   for the show-joins command.          */
/******************************************/
globle void ShowJoinsCommand()
  {
   char *ruleName;
   void *rulePtr;

   ruleName = GetConstructName("show-joins","rule name");
   if (ruleName == NULL) return;

   rulePtr = FindDefrule(ruleName);
   if (rulePtr == NULL)
     {
      CantFindItemErrorMessage("defrule",ruleName);
      return;
     }

   ShowJoins(rulePtr);

   return;
  }

/*********************************/
/* ShowJoins: C access routine   */
/*   for the show-joins command. */
/*********************************/
static void ShowJoins(
  void *theRule)
  {
   struct defrule *rulePtr;
   struct joinNode *theJoin;
   struct joinNode *joinList[MAXIMUM_NUMBER_OF_PATTERNS];
   int numberOfJoins;

   rulePtr = (struct defrule *) theRule;

   /*=================================================*/
   /* Loop through each of the disjuncts for the rule */
   /*=================================================*/

   while (rulePtr != NULL)
     {
      /*=====================================*/
      /* Determine the number of join nodes. */
      /*=====================================*/

      numberOfJoins = -1;
      theJoin = rulePtr->lastJoin;
      while (theJoin != NULL)
        {
         if (theJoin->joinFromTheRight)
           { theJoin = (struct joinNode *) theJoin->rightSideEntryStructure; }
         else
           {
            numberOfJoins++;
            joinList[numberOfJoins] = theJoin;
            theJoin = theJoin->lastLevel;
           }
        }

      /*====================*/
      /* Display the joins. */
      /*====================*/

      while (numberOfJoins >= 0)
        {
         char buffer[20];
         sprintf(buffer,"%2d%c%c: ",(int) joinList[numberOfJoins]->depth,
                                     (joinList[numberOfJoins]->patternIsNegated) ? 'n' : ' ',
                                     (joinList[numberOfJoins]->logicalJoin) ? 'l' : ' ');
         PrintRouter(WDISPLAY,buffer);
         PrintExpression(WDISPLAY,joinList[numberOfJoins]->networkTest);
         PrintRouter(WDISPLAY,"\n");
         numberOfJoins--;
        };

      /*===============================*/
      /* Proceed to the next disjunct. */
      /*===============================*/

      rulePtr = rulePtr->disjunct;
      if (rulePtr != NULL) PrintRouter(WDISPLAY,"\n");
     }
  }

#endif /* DEVELOPER */

#endif /* DEFRULE_CONSTRUCT */


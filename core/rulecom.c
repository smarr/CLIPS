   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  10/19/06            */
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
/*      6.24: Removed CONFLICT_RESOLUTION_STRATEGIES         */
/*            INCREMENTAL_RESET, and LOGICAL_DEPENDENCIES    */
/*            compilation flags.                             */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Added support for hashed alpha memories.       */
/*                                                           */
/*            Added matches-count function.                  */
/*                                                           */
/*            Added get-join-hashing and set-join-hashing    */
/*            functions.                                     */
/*                                                           */
/*************************************************************/

#define _RULECOM_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include "setup.h"

#if DEFRULE_CONSTRUCT

#include "argacces.h"
#include "constant.h"
#include "constrct.h"
#include "crstrtgy.h"
#include "engine.h"
#include "envrnmnt.h"
#include "evaluatn.h"
#include "extnfunc.h"
#include "incrrset.h"
#include "lgcldpnd.h"
#include "memalloc.h"
#include "pattern.h"
#include "reteutil.h"
#include "router.h"
#include "ruledlt.h"
#include "sysdep.h"
#include "watch.h"

#if BLOAD || BLOAD_AND_BSAVE || BLOAD_ONLY
#include "rulebin.h"
#endif

#include "rulecom.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if DEVELOPER
   static void                    ShowJoins(void *,void *);
#endif
   static int                     ListAlphaMatches(void *,struct joinNode *,int);
   static int                     ListBetaMatches(void *,struct joinNode *,int);
   static int                     ListBetaJoinActivity(void *,struct joinNode *,int,long long *,int);
   static void                    PrintMatchesMemory(void *,struct joinNode *,struct betaMemory *,int,int);
   
/****************************************************************/
/* DefruleCommands: Initializes defrule commands and functions. */
/****************************************************************/
globle void DefruleCommands(
  void *theEnv,
  EXEC_STATUS)
  {
#if ! RUN_TIME
   EnvDefineFunction2(theEnv,execStatus,"run",'v', PTIEF RunCommand,"RunCommand", "*1i");
   EnvDefineFunction2(theEnv,execStatus,"halt",'v', PTIEF HaltCommand,"HaltCommand","00");
   EnvDefineFunction2(theEnv,execStatus,"focus",'b', PTIEF FocusCommand,"FocusCommand", "1*w");
   EnvDefineFunction2(theEnv,execStatus,"clear-focus-stack",'v',PTIEF ClearFocusStackCommand,
                                       "ClearFocusStackCommand","00");
   EnvDefineFunction2(theEnv,execStatus,"get-focus-stack",'m',PTIEF GetFocusStackFunction,
                                     "GetFocusStackFunction","00");
   EnvDefineFunction2(theEnv,execStatus,"pop-focus",'w',PTIEF PopFocusFunction,
                               "PopFocusFunction","00");
   EnvDefineFunction2(theEnv,execStatus,"get-focus",'w',PTIEF GetFocusFunction,
                               "GetFocusFunction","00");
#if DEBUGGING_FUNCTIONS
   EnvDefineFunction2(theEnv,execStatus,"set-break",'v', PTIEF SetBreakCommand,
                               "SetBreakCommand","11w");
   EnvDefineFunction2(theEnv,execStatus,"remove-break",'v', PTIEF RemoveBreakCommand,
                                  "RemoveBreakCommand", "*1w");
   EnvDefineFunction2(theEnv,execStatus,"show-breaks",'v', PTIEF ShowBreaksCommand,
                                 "ShowBreaksCommand", "01w");
   EnvDefineFunction2(theEnv,execStatus,"matches",'v',PTIEF MatchesCommand,"MatchesCommand","11w");
   EnvDefineFunction2(theEnv,execStatus,"join-activity",'g',PTIEF JoinActivityCommand,"JoinActivityCommand","11w");
   EnvDefineFunction2(theEnv,execStatus,"matches-count",'v',PTIEF MatchesCountCommand,"MatchesCountCommand","11w");
   EnvDefineFunction2(theEnv,execStatus,"list-focus-stack",'v', PTIEF ListFocusStackCommand,
                                      "ListFocusStackCommand", "00");
   EnvDefineFunction2(theEnv,execStatus,"dependencies", 'v', PTIEF DependenciesCommand,
                                   "DependenciesCommand", "11h");
   EnvDefineFunction2(theEnv,execStatus,"dependents",   'v', PTIEF DependentsCommand,
                                   "DependentsCommand", "11h");
   EnvDefineFunction2(theEnv,execStatus,"timetag",   'g', PTIEF TimetagFunction,
                                   "TimetagFunction", "11h");
#endif /* DEBUGGING_FUNCTIONS */

   EnvDefineFunction2(theEnv,execStatus,"get-incremental-reset",'b',
                   GetIncrementalResetCommand,"GetIncrementalResetCommand","00");
   EnvDefineFunction2(theEnv,execStatus,"set-incremental-reset",'b',
                   SetIncrementalResetCommand,"SetIncrementalResetCommand","11");

   EnvDefineFunction2(theEnv,execStatus,"get-beta-memory-resizing",'b',
                   GetBetaMemoryResizingCommand,"GetBetaMemoryResizingCommand","00");
   EnvDefineFunction2(theEnv,execStatus,"set-beta-memory-resizing",'b',
                   SetBetaMemoryResizingCommand,"SetBetaMemoryResizingCommand","11");

   EnvDefineFunction2(theEnv,execStatus,"get-strategy", 'w', PTIEF GetStrategyCommand,  "GetStrategyCommand", "00");
   EnvDefineFunction2(theEnv,execStatus,"set-strategy", 'w', PTIEF SetStrategyCommand,  "SetStrategyCommand", "11w");

#if DEVELOPER && (! BLOAD_ONLY)
   EnvDefineFunction2(theEnv,execStatus,"rule-complexity",'l', PTIEF RuleComplexityCommand,"RuleComplexityCommand", "11w");
   EnvDefineFunction2(theEnv,execStatus,"show-joins",   'v', PTIEF ShowJoinsCommand,    "ShowJoinsCommand", "11w");
   EnvDefineFunction2(theEnv,execStatus,"show-aht",   'v', PTIEF ShowAlphaHashTable,    "ShowAlphaHashTable", "00");
#if DEBUGGING_FUNCTIONS
   AddWatchItem(theEnv,execStatus,"rule-analysis",0,&DefruleData(theEnv)->WatchRuleAnalysis,0,NULL,NULL);
#endif
#endif /* DEVELOPER && (! BLOAD_ONLY) */

#else
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv)
#endif
#endif /* ! RUN_TIME */
  }

/***********************************************/
/* EnvGetBetaMemoryResizing: C access routine  */
/*   for the get-beta-memory-resizing command. */
/***********************************************/
globle intBool EnvGetBetaMemoryResizing(
  void *theEnv,
  EXEC_STATUS)
  {   
   return(DefruleData(theEnv)->BetaMemoryResizingFlag);
  }

/***********************************************/
/* EnvSetBetaMemoryResizing: C access routine  */
/*   for the set-beta-memory-resizing command. */
/***********************************************/
globle intBool EnvSetBetaMemoryResizing(
  void *theEnv,
  EXEC_STATUS,
  int value)
  {
   int ov;

   ov = DefruleData(theEnv)->BetaMemoryResizingFlag;

   DefruleData(theEnv)->BetaMemoryResizingFlag = value;

   return(ov);
  }

/****************************************************/
/* SetBetaMemoryResizingCommand: H/L access routine */
/*   for the set-beta-memory-resizing command.      */
/****************************************************/
globle int SetBetaMemoryResizingCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   int oldValue;
   DATA_OBJECT argPtr;

   oldValue = EnvGetBetaMemoryResizing(theEnv);

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/

   if (EnvArgCountCheck(theEnv,execStatus,"set-beta-memory-resizing",EXACTLY,1) == -1)
     { return(oldValue); }

   /*=================================================*/
   /* The symbol FALSE disables beta memory resizing. */
   /* Any other value enables beta memory resizing.   */
   /*=================================================*/

   EnvRtnUnknown(theEnv,execStatus,1,&argPtr);

   if ((argPtr.value == EnvFalseSymbol(theEnv)) && (argPtr.type == SYMBOL))
     { EnvSetBetaMemoryResizing(theEnv,execStatus,FALSE); }
   else
     { EnvSetBetaMemoryResizing(theEnv,execStatus,TRUE); }

   /*=======================*/
   /* Return the old value. */
   /*=======================*/

   return(oldValue);
  }

/****************************************************/
/* GetBetaMemoryResizingCommand: H/L access routine */
/*   for the get-beta-memory-resizing command.      */
/****************************************************/
globle int GetBetaMemoryResizingCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   int oldValue;

   oldValue = EnvGetBetaMemoryResizing(theEnv);

   if (EnvArgCountCheck(theEnv,execStatus,"get-beta-memory-resizing",EXACTLY,0) == -1)
     { return(oldValue); }

   return(oldValue);
  }

#if DEBUGGING_FUNCTIONS

/****************************************/
/* MatchesCommand: H/L access routine   */
/*   for the matches command.           */
/****************************************/
globle void MatchesCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   char *ruleName;
   void *rulePtr;

   ruleName = GetConstructName(theEnv,execStatus,"matches","rule name");
   if (ruleName == NULL) return;

   rulePtr = EnvFindDefrule(theEnv,execStatus,ruleName);
   if (rulePtr == NULL)
     {
      CantFindItemErrorMessage(theEnv,execStatus,"defrule",ruleName);
      return;
     }

   EnvMatches(theEnv,execStatus,rulePtr);
  }

/********************************/
/* EnvMatches: C access routine */
/*   for the matches command.   */
/********************************/
globle intBool EnvMatches(
  void *theEnv,
  EXEC_STATUS,
  void *theRule)
  {
   struct defrule *rulePtr, *tmpPtr;
   struct joinNode *lastJoin;
   ACTIVATION *agendaPtr;
   int flag;

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

      /*========================================*/
      /* List the alpha memory partial matches. */
      /*========================================*/

      ListAlphaMatches(theEnv,execStatus,lastJoin->lastLevel,0);

      /*=======================================*/
      /* List the beta memory partial matches. */
      /*=======================================*/

      ListBetaMatches(theEnv,execStatus,lastJoin,1);
     }

   /*===================*/
   /* List activations. */
   /*===================*/

   rulePtr = tmpPtr;
   EnvPrintRouter(theEnv,execStatus,WDISPLAY,"Activations\n");
   flag = 1;
   for (agendaPtr = (struct activation *) EnvGetNextActivation(theEnv,execStatus,NULL);
        agendaPtr != NULL;
        agendaPtr = (struct activation *) EnvGetNextActivation(theEnv,execStatus,agendaPtr))
     {
      if (GetHaltExecution(theEnv) == TRUE) return(TRUE);

      if (((struct activation *) agendaPtr)->theRule->header.name == rulePtr->header.name)
        {
         flag = 0;
         PrintPartialMatch(theEnv,execStatus,WDISPLAY,GetActivationBasis(agendaPtr));
         EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");
        }
     }

   if (flag) EnvPrintRouter(theEnv,execStatus,WDISPLAY," None\n");

   return(TRUE);
  }

/*********************/
/* ListAlphaMatches: */
/*********************/
static int ListAlphaMatches(
  void *theEnv,
  EXEC_STATUS,
  struct joinNode *theJoin,
  int priorPatterns)
  {
   struct alphaMemoryHash *listOfHashNodes;
   struct partialMatch *listOfMatches;
   int flag;

   if (theJoin == NULL) 
     { return(priorPatterns); }
     
   if (theJoin->rightSideEntryStructure == NULL)
     { 
      priorPatterns++;

      EnvPrintRouter(theEnv,execStatus,WDISPLAY,"Matches for Pattern ");
      PrintLongInteger(theEnv,execStatus,WDISPLAY,(long int) priorPatterns);
      EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");
     
      if (theJoin->rightMemory->beta[0]->children != NULL)
        { EnvPrintRouter(theEnv,execStatus,WDISPLAY,"*\n"); }
      else
        { EnvPrintRouter(theEnv,execStatus,WDISPLAY," None\n"); }
     
      return(priorPatterns); 
     }
   
   if (theJoin->lastLevel != NULL)
     { priorPatterns = ListAlphaMatches(theEnv,execStatus,theJoin->lastLevel,priorPatterns); }
     
   if (theJoin->joinFromTheRight)
     { return ListAlphaMatches(theEnv,execStatus,(struct joinNode *) theJoin->rightSideEntryStructure,priorPatterns); }
     
   listOfHashNodes =  ((struct patternNodeHeader *) theJoin->rightSideEntryStructure)->firstHash;

   priorPatterns++;
   
   if (GetHaltExecution(theEnv) == TRUE)
     { return(priorPatterns); }
   
   EnvPrintRouter(theEnv,execStatus,WDISPLAY,"Matches for Pattern ");
   PrintLongInteger(theEnv,execStatus,WDISPLAY,(long int) priorPatterns);
   EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");

   for (flag = 1;
        listOfHashNodes != NULL;
        listOfHashNodes = listOfHashNodes->nextHash)
     {
      listOfMatches = listOfHashNodes->alphaMemory;

      while (listOfMatches != NULL)
        {
         if (GetHaltExecution(theEnv) == TRUE)
           { return(priorPatterns); }
                 
         flag = 0;
         PrintPartialMatch(theEnv,execStatus,WDISPLAY,listOfMatches);
         EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");
         listOfMatches = listOfMatches->nextInMemory;
        }
     }
           
   if (flag) EnvPrintRouter(theEnv,execStatus,WDISPLAY," None\n");
   
   return(priorPatterns);
  }
  
/********************/
/* ListBetaMatches: */
/********************/
static int ListBetaMatches(
  void *theEnv,
  EXEC_STATUS,
  struct joinNode *theJoin,
  int blockStart)
  {
   int patternsFound = 0, startPatterns;

   if (GetHaltExecution(theEnv) == TRUE)
     { return(0); }   

   if (theJoin == NULL) 
     { return(patternsFound); }
     
   if (theJoin->lastLevel != NULL)
     { patternsFound += ListBetaMatches(theEnv,execStatus,theJoin->lastLevel,blockStart); }
     
   if (theJoin->depth > 2)
     {
      PrintMatchesMemory(theEnv,execStatus,theJoin,
                                theJoin->leftMemory,
                                blockStart,
                                blockStart + patternsFound - 1); 
     }

   startPatterns = patternsFound;
   
   if (theJoin->joinFromTheRight)
     { patternsFound += ListBetaMatches(theEnv,execStatus,(struct joinNode *) theJoin->rightSideEntryStructure,blockStart+patternsFound); }

   if ((theJoin->joinFromTheRight) &&
       (((struct joinNode *) (theJoin->rightSideEntryStructure))->depth > 1))
     { 
      PrintMatchesMemory(theEnv,execStatus,theJoin,
                                theJoin->rightMemory,
                                blockStart + startPatterns,
                                blockStart + patternsFound - 1); 
     }
         
   
   if (theJoin->joinFromTheRight)
     { return(patternsFound); } 
   else
     { return(patternsFound + 1); } 
  }
 
/****************************/
/* PrintMatchesMemory: */
/****************************/
#if WIN_BTC
#pragma argsused
#endif
static void PrintMatchesMemory(
  void *theEnv,
  EXEC_STATUS,
  struct joinNode *theJoin,
  struct betaMemory *theMemory,
  int startCE, 
  int endCE)  
  {
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theJoin)
#endif
   struct partialMatch *listOfMatches;
   unsigned long b;
   int matchesDisplayed;

   if (GetHaltExecution(theEnv) == TRUE)
     { return; }
     
   matchesDisplayed = 0;
   EnvPrintRouter(theEnv,execStatus,WDISPLAY,"Partial matches for CEs ");
   PrintLongInteger(theEnv,execStatus,WDISPLAY,(long int) startCE);
   EnvPrintRouter(theEnv,execStatus,WDISPLAY," - ");
   PrintLongInteger(theEnv,execStatus,WDISPLAY,(long int) endCE);
   
   EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");

   for (b = 0; b < theMemory->size; b++)
     {
      listOfMatches = theMemory->beta[b];

      while (listOfMatches != NULL)
        {
         if (GetHaltExecution(theEnv) == TRUE)
           { return; }

         matchesDisplayed++;
         PrintPartialMatch(theEnv,execStatus,WDISPLAY,listOfMatches);
         EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");
    
         listOfMatches = listOfMatches->nextInMemory;
        }
     }

   if (matchesDisplayed == 0) { EnvPrintRouter(theEnv,execStatus,WDISPLAY," None\n"); }
  }

 
/*******************************************/
/* JoinActivityCommand: H/L access routine */
/*   for the join-activity command.        */
/*******************************************/
globle long long JoinActivityCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   char *ruleName;
   void *rulePtr;

   ruleName = GetConstructName(theEnv,execStatus,"join-activity","rule name");
   if (ruleName == NULL) return(0);

   rulePtr = EnvFindDefrule(theEnv,execStatus,ruleName);
   if (rulePtr == NULL)
     {
      CantFindItemErrorMessage(theEnv,execStatus,"defrule",ruleName);
      return(0);
     }

   return EnvJoinActivity(theEnv,execStatus,rulePtr,0);
  }

/*************************************/
/* EnvJoinActivity: C access routine */
/*   for the join-activity command.  */
/*************************************/
globle long long EnvJoinActivity(
  void *theEnv,
  EXEC_STATUS,
  void *theRule,
  int verbosity)
  {
   struct defrule *rulePtr;
   struct joinNode *lastJoin;
   long long totalActivity = 0;

   /*=================================================*/
   /* Loop through each of the disjuncts for the rule */
   /*=================================================*/

   for (rulePtr = (struct defrule *) theRule;
		rulePtr != NULL;
		rulePtr = rulePtr->disjunct)
     {
      /*======================================*/
      /* Determine the last join in the rule. */
      /*======================================*/

      lastJoin = rulePtr->lastJoin;

      /*=======================================*/
      /* List the beta memory partial matches. */
      /*=======================================*/

      ListBetaJoinActivity(theEnv,execStatus,lastJoin,1,&totalActivity,verbosity);
     }
     
   return(totalActivity);
  }

/*************************/
/* ListBetaJoinActivity: */
/*************************/
static int ListBetaJoinActivity(
  void *theEnv,
  EXEC_STATUS,
  struct joinNode *theJoin,
  int blockStart,
  long long *activity,
  int verbosity)
  {
   int priorLeftPatterns = 0, priorRightPatterns = 0;

   if (theJoin == NULL) 
     { return(priorLeftPatterns + priorRightPatterns); }
     
   if (theJoin->lastLevel != NULL)
     { priorLeftPatterns = ListBetaJoinActivity(theEnv,execStatus,theJoin->lastLevel,blockStart,activity,verbosity); }
     
   if (theJoin->joinFromTheRight)
     { priorRightPatterns = ListBetaJoinActivity(theEnv,execStatus,(struct joinNode *) theJoin->rightSideEntryStructure,blockStart+priorLeftPatterns,activity,verbosity); }
   
   if (theJoin->depth == 1)
     { 
      if (theJoin->joinFromTheRight)
        { return(priorLeftPatterns + priorRightPatterns); } 
      else
        { return(priorLeftPatterns + priorRightPatterns + 1); } 
     }
      
   if ((priorLeftPatterns > 1) || (theJoin->joinFromTheRight))
     {
      /* EnvPrintRouter(theEnv,execStatus,WDISPLAY,"Join Activity for CEs "); */
      
      if (priorLeftPatterns > 1)
        {
        /*
         PrintLongInteger(theEnv,execStatus,WDISPLAY,(long int) blockStart);
         EnvPrintRouter(theEnv,execStatus,WDISPLAY," - ");
         PrintLongInteger(theEnv,execStatus,WDISPLAY,(long int) priorLeftPatterns);
         */
        }
      else if (theJoin->joinFromTheRight)
        {
        /*
         PrintLongInteger(theEnv,execStatus,WDISPLAY,(long int) blockStart + priorLeftPatterns);
         EnvPrintRouter(theEnv,execStatus,WDISPLAY," - ");
         PrintLongInteger(theEnv,execStatus,WDISPLAY,(long int) blockStart + priorLeftPatterns + (priorRightPatterns - 1));
         */
        }
  
      *activity += theJoin->memoryCompares + theJoin->memoryAdds + theJoin->memoryDeletes;
      /*
      EnvPrintRouter(theEnv,execStatus,WDISPLAY,": C ");
      PrintLongInteger(theEnv,execStatus,WDISPLAY,theJoin->memoryCompares);
      EnvPrintRouter(theEnv,execStatus,WDISPLAY,"/ A ");
      PrintLongInteger(theEnv,execStatus,WDISPLAY,theJoin->memoryAdds);
      EnvPrintRouter(theEnv,execStatus,WDISPLAY,"/ D ");
      PrintLongInteger(theEnv,execStatus,WDISPLAY,theJoin->memoryDeletes);
      EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");
      */
     }
   
   if (theJoin->joinFromTheRight)
     { return(priorLeftPatterns + priorRightPatterns); } 
   else
     { return(priorLeftPatterns + priorRightPatterns + 1); } 
  }

/*******************************************/
/* MatchesCountCommand: H/L access routine */
/*   for the matches-count command.        */
/*******************************************/
globle void MatchesCountCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   char *ruleName;
   void *rulePtr;

   ruleName = GetConstructName(theEnv,execStatus,"matches-count","rule name");
   if (ruleName == NULL) return;

   rulePtr = EnvFindDefrule(theEnv,execStatus,ruleName);
   if (rulePtr == NULL)
     {
      CantFindItemErrorMessage(theEnv,execStatus,"defrule",ruleName);
      return;
     }

   EnvMatchesCount(theEnv,execStatus,rulePtr);
  }

/*************************************/
/* EnvMatchesCount: C access routine */
/*   for the matches-count command.  */
/*************************************/
globle intBool EnvMatchesCount(
  void *theEnv,
  EXEC_STATUS,
  void *theRule)
  {
   struct defrule *rulePtr, *tmpPtr;
   struct betaMemory *theMemory, **theStorage;
   struct partialMatch *listOfMatches;
   struct alphaMemoryHash *listOfHashNodes, **theAlphaStorage;
   struct joinNode *theJoin, *lastJoin;
   int i, depth;
   ACTIVATION *agendaPtr;
   long count;

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

      theAlphaStorage = (struct alphaMemoryHash **)
                        genalloc(theEnv,execStatus,(unsigned) (depth * sizeof(struct alphaMemoryHash *)));

      theJoin = lastJoin;
      i = depth - 1;
      while (theJoin != NULL)
        {
         if (theJoin->joinFromTheRight)
           { theJoin = (struct joinNode *) theJoin->rightSideEntryStructure; }
         else
           {
            theAlphaStorage[i] = ((struct patternNodeHeader *) theJoin->rightSideEntryStructure)->firstHash;
            i--;
            theJoin = theJoin->lastLevel;
           }
        }

      /*========================================*/
      /* List the alpha memory partial matches. */
      /*========================================*/

      for (i = 0; i < depth; i++)
        {
         if (GetHaltExecution(theEnv) == TRUE)
           {
            genfree(theEnv,execStatus,theAlphaStorage,(unsigned) (depth * sizeof(struct alphaMemoryHash *)));
            return(TRUE);
           }

         EnvPrintRouter(theEnv,execStatus,WDISPLAY,"Matches for Pattern ");
         PrintLongInteger(theEnv,execStatus,WDISPLAY,(long int) i + 1);
         EnvPrintRouter(theEnv,execStatus,WDISPLAY,": ");

         count = 0;
         for (listOfHashNodes = theAlphaStorage[i];
              listOfHashNodes != NULL;
              listOfHashNodes = listOfHashNodes->nextHash)
           {
            listOfMatches = listOfHashNodes->alphaMemory;

            while (listOfMatches != NULL)
              {
               if (GetHaltExecution(theEnv) == TRUE)
                 {
                  genfree(theEnv,execStatus,theAlphaStorage,(unsigned) (depth * sizeof(struct alphaMemoryHash *)));
                  return(TRUE);
                 }
                 
               count++;
               listOfMatches = listOfMatches->nextInMemory;
              }
           }
           
         PrintLongInteger(theEnv,execStatus,WDISPLAY,count);
         EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");
        }

      genfree(theEnv,execStatus,theAlphaStorage,(unsigned) (depth * sizeof(struct alphaMemoryHash *)));

      /*========================================*/
      /* Store the beta memory partial matches. */
      /*========================================*/

      depth = lastJoin->depth;
      theStorage = (struct betaMemory **) genalloc(theEnv,execStatus,(unsigned) (depth * sizeof(struct betaMemory *)));

      theJoin = lastJoin;
      for (i = depth - 1; i >= 0; i--)
        {
         /* theStorage[i] = GetBetaMemory(theEnv,execStatus,theJoin); */
         theStorage[i] = theJoin->leftMemory;
         theJoin = theJoin->lastLevel;
        }

      /*=======================================*/
      /* List the beta memory partial matches. */
      /*=======================================*/

      for (i = 1; i < depth; i++)
        {
         if (GetHaltExecution(theEnv) == TRUE)
           {
            genfree(theEnv,execStatus,theStorage,(unsigned) (depth * sizeof(struct betaMemory *)));
            return(TRUE);
           }

         /* count = 0; */

         EnvPrintRouter(theEnv,execStatus,WDISPLAY,"Partial matches for CEs 1 - ");
         PrintLongInteger(theEnv,execStatus,WDISPLAY,(long int) i + 1);
         EnvPrintRouter(theEnv,execStatus,WDISPLAY,": ");
         theMemory = theStorage[i];
		 /*
		 for (b = 0; b < theMemory->size; b++)
		   {
			listOfMatches = theMemory->beta[b];

			while (listOfMatches != NULL)
			  {
			   if (GetHaltExecution(theEnv) == TRUE)
				 {
				  genfree(theEnv,execStatus,theStorage,(unsigned) (depth * sizeof(struct betaMemory *)));
				  return(TRUE);
				 }

			   count++;
			   listOfMatches = listOfMatches->nextInMemory;
			  }
		   }
         */
         count = theMemory->count;
         PrintLongInteger(theEnv,execStatus,WDISPLAY,count);

         EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n"); 
        }

      genfree(theEnv,execStatus,theStorage,(unsigned) (depth * sizeof(struct betaMemory *)));
     }

   /*===================*/
   /* List activations. */
   /*===================*/

   rulePtr = tmpPtr;
   EnvPrintRouter(theEnv,execStatus,WDISPLAY,"Activations: ");
   count = 0;
   for (agendaPtr = (struct activation *) EnvGetNextActivation(theEnv,execStatus,NULL);
        agendaPtr != NULL;
        agendaPtr = (struct activation *) EnvGetNextActivation(theEnv,execStatus,agendaPtr))
     {
      if (GetHaltExecution(theEnv) == TRUE) return(TRUE);

      if (((struct activation *) agendaPtr)->theRule->header.name == rulePtr->header.name)
        { count++; }
     }

   PrintLongInteger(theEnv,execStatus,WDISPLAY,count);
   EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");

   return(TRUE);
  }

/***************************************/
/* TimetagFunction: H/L access routine */
/*   for the timetag function.         */
/***************************************/
globle long long TimetagFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   DATA_OBJECT item;
   void *ptr;

   if (EnvArgCountCheck(theEnv,execStatus,"timetag",EXACTLY,1) == -1) return(-1LL);

   ptr = GetFactOrInstanceArgument(theEnv,execStatus,1,&item,"timetag");

   if (ptr == NULL) return(-1);

   return ((struct patternEntity *) ptr)->timeTag;
  }

#endif /* DEBUGGING_FUNCTIONS */

#if DEVELOPER
/***********************************************/
/* RuleComplexityCommand: H/L access routine   */
/*   for the rule-complexity function.         */
/***********************************************/
globle long RuleComplexityCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   char *ruleName;
   struct defrule *rulePtr;

   ruleName = GetConstructName(theEnv,execStatus,"rule-complexity","rule name");
   if (ruleName == NULL) return(-1);

   rulePtr = (struct defrule *) EnvFindDefrule(theEnv,execStatus,ruleName);
   if (rulePtr == NULL)
     {
      CantFindItemErrorMessage(theEnv,execStatus,"defrule",ruleName);
      return(-1);
     }

   return(rulePtr->complexity);
  }

/******************************************/
/* ShowJoinsCommand: H/L access routine   */
/*   for the show-joins command.          */
/******************************************/
globle void ShowJoinsCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   char *ruleName;
   void *rulePtr;

   ruleName = GetConstructName(theEnv,execStatus,"show-joins","rule name");
   if (ruleName == NULL) return;

   rulePtr = EnvFindDefrule(theEnv,execStatus,ruleName);
   if (rulePtr == NULL)
     {
      CantFindItemErrorMessage(theEnv,execStatus,"defrule",ruleName);
      return;
     }

   ShowJoins(theEnv,execStatus,rulePtr);

   return;
  }

/*********************************/
/* ShowJoins: C access routine   */
/*   for the show-joins command. */
/*********************************/
static void ShowJoins(
  void *theEnv,
  EXEC_STATUS,
  void *theRule)
  {
   struct defrule *rulePtr;
   struct joinNode *theJoin;
   struct joinNode *joinList[MAXIMUM_NUMBER_OF_PATTERNS];
   int numberOfJoins;
   char rhsType;

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
         
         if (joinList[numberOfJoins]->patternIsNegated)
           { rhsType = 'n'; }
         else if (joinList[numberOfJoins]->patternIsExists)
           { rhsType = 'x'; }
         else
           { rhsType = ' '; }
           
         gensprintf(buffer,"%2d%c%c: ",(int) joinList[numberOfJoins]->depth,
                                     rhsType,
                                     (joinList[numberOfJoins]->logicalJoin) ? 'l' : ' ');
         EnvPrintRouter(theEnv,execStatus,WDISPLAY,buffer);
         PrintExpression(theEnv,execStatus,WDISPLAY,joinList[numberOfJoins]->networkTest);
         EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");
         
         if (joinList[numberOfJoins]->leftHash != NULL)
           {
            EnvPrintRouter(theEnv,execStatus,WDISPLAY,"    LH : ");
            PrintExpression(theEnv,execStatus,WDISPLAY,joinList[numberOfJoins]->leftHash);
            EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");
           }
         
         numberOfJoins--;
        };

      /*===============================*/
      /* Proceed to the next disjunct. */
      /*===============================*/

      rulePtr = rulePtr->disjunct;
      if (rulePtr != NULL) EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");
     }
  }

/******************************************************/
/* ShowAlphaHashTable: Displays the number of entries */
/*   in each slot of the alpha hash table.            */
/******************************************************/
globle void ShowAlphaHashTable(
   void *theEnv,
  EXEC_STATUS)
   {
    int i, count;
    long totalCount = 0;
    struct alphaMemoryHash *theEntry;
    struct partialMatch *theMatch;
    char buffer[40];

    for (i = 0; i < ALPHA_MEMORY_HASH_SIZE; i++)
      {
       for (theEntry =  DefruleData(theEnv)->AlphaMemoryTable[i], count = 0;
            theEntry != NULL;
            theEntry = theEntry->next)
         { count++; }

       if (count != 0)
         {
          totalCount += count;
          gensprintf(buffer,"%4d: %4d ->",i,count);
          EnvPrintRouter(theEnv,execStatus,WDISPLAY,buffer);
          
          for (theEntry =  DefruleData(theEnv)->AlphaMemoryTable[i], count = 0;
               theEntry != NULL;
               theEntry = theEntry->next)
            {
             for (theMatch = theEntry->alphaMemory;
                  theMatch != NULL;
                  theMatch = theMatch->nextInMemory)
               { count++; }
               
             gensprintf(buffer," %4d",count);
             EnvPrintRouter(theEnv,execStatus,WDISPLAY,buffer);
             if (theEntry->owner->rightHash == NULL)
               { EnvPrintRouter(theEnv,execStatus,WDISPLAY,"*"); }
            }
          
          EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");
         }
      }
    gensprintf(buffer,"Total Count: %ld\n",totalCount);
    EnvPrintRouter(theEnv,execStatus,WDISPLAY,buffer);
   }

#endif /* DEVELOPER */

#endif /* DEFRULE_CONSTRUCT */


   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  10/19/06            */
   /*                                                     */
   /*                 RULE PARSING MODULE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose:  Parses a defrule construct.                     */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed DYNAMIC_SALIENCE, INCREMENTAL_RESET,   */
/*            and LOGICAL_DEPENDENCIES compilation flags.    */
/*                                                           */
/*      6.30: Added support for hashed alpha memories.       */
/*                                                           */
/*************************************************************/

#define _RULEPSR_SOURCE_

#include "setup.h"

#if DEFRULE_CONSTRUCT

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include "analysis.h"
#include "constant.h"
#include "constrct.h"
#include "cstrcpsr.h"
#include "cstrnchk.h"
#include "cstrnops.h"
#include "engine.h"
#include "envrnmnt.h"
#include "exprnpsr.h"
#include "incrrset.h"
#include "memalloc.h"
#include "pattern.h"
#include "prccode.h"
#include "prcdrpsr.h"
#include "router.h"
#include "rulebld.h"
#include "rulebsc.h"
#include "rulecstr.h"
#include "ruledef.h"
#include "ruledlt.h"
#include "rulelhs.h"
#include "scanner.h"
#include "symbol.h"
#include "watch.h"

#include "lgcldpnd.h"

#if DEFTEMPLATE_CONSTRUCT
#include "tmpltfun.h"
#endif

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
#include "bload.h"
#endif

#include "rulepsr.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if (! RUN_TIME) && (! BLOAD_ONLY)
   static struct expr            *ParseRuleRHS(void *,EXEC_STATUS,char *);
   static int                     ReplaceRHSVariable(void *,EXEC_STATUS,struct expr *,void *);
   static struct defrule         *ProcessRuleLHS(void *,EXEC_STATUS,struct lhsParseNode *,struct expr *,SYMBOL_HN *,int *);
   static struct defrule         *CreateNewDisjunct(void *,EXEC_STATUS,SYMBOL_HN *,int,struct expr *,
                                                    int,unsigned,struct joinNode *);
   static int                     RuleComplexity(void *,EXEC_STATUS,struct lhsParseNode *);
   static int                     ExpressionComplexity(void *,EXEC_STATUS,struct expr *);
   static int                     LogicalAnalysis(void *,EXEC_STATUS,struct lhsParseNode *);
   static void                    AddToDefruleList(struct defrule *);
#if DEVELOPER && DEBUGGING_FUNCTIONS
   static void                    DumpRuleAnalysis(void *,EXEC_STATUS,struct lhsParseNode *);
#endif
#endif

/****************************************************/
/* ParseDefrule: Coordinates all actions necessary  */
/*   for the parsing and creation of a defrule into */
/*   the current environment.                       */
/****************************************************/
globle int ParseDefrule(
  void *theEnv,
  EXEC_STATUS,
  char *readSource)
  {
#if (MAC_MCW || WIN_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(theEnv,execStatus,readSource)
#endif

#if (! RUN_TIME) && (! BLOAD_ONLY)
   SYMBOL_HN *ruleName;
   struct lhsParseNode *theLHS;
   struct expr *actions;
   struct token theToken;
   struct defrule *topDisjunct, *tempPtr;
   struct defruleModule *theModuleItem;
   int error;

   /*================================================*/
   /* Flush the buffer which stores the pretty print */
   /* representation for a rule.  Add the already    */
   /* parsed keyword defrule to this buffer.         */
   /*================================================*/

   SetPPBufferStatus(theEnv,execStatus,ON);
   FlushPPBuffer(theEnv,execStatus);
   SavePPBuffer(theEnv,execStatus,"(defrule ");

   /*=========================================================*/
   /* Rules cannot be loaded when a binary load is in effect. */
   /*=========================================================*/

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
   if ((Bloaded(theEnv,execStatus) == TRUE) && (! ConstructData(theEnv,execStatus)->CheckSyntaxMode))
     {
      CannotLoadWithBloadMessage(theEnv,execStatus,"defrule");
      return(TRUE);
     }
#endif

   /*================================================*/
   /* Parse the name and comment fields of the rule, */
   /* deleting the rule if it already exists.        */
   /*================================================*/

#if DEBUGGING_FUNCTIONS
   DefruleData(theEnv,execStatus)->DeletedRuleDebugFlags = 0;
#endif

   ruleName = GetConstructNameAndComment(theEnv,execStatus,readSource,&theToken,"defrule",
                                         EnvFindDefrule,EnvUndefrule,"*",FALSE,
                                         TRUE,TRUE);

   if (ruleName == NULL) return(TRUE);

   /*============================*/
   /* Parse the LHS of the rule. */
   /*============================*/

   theLHS = ParseRuleLHS(theEnv,execStatus,readSource,&theToken,ValueToString(ruleName),&error);
   if (error)
     {
      ReturnPackedExpression(theEnv,execStatus,PatternData(theEnv,execStatus)->SalienceExpression);
      PatternData(theEnv,execStatus)->SalienceExpression = NULL;
      return(TRUE);
     }

   /*============================*/
   /* Parse the RHS of the rule. */
   /*============================*/

   ClearParsedBindNames(theEnv,execStatus);
   ExpressionData(theEnv,execStatus)->ReturnContext = TRUE;
   actions = ParseRuleRHS(theEnv,execStatus,readSource);

   if (actions == NULL)
     {
      ReturnPackedExpression(theEnv,execStatus,PatternData(theEnv,execStatus)->SalienceExpression);
      PatternData(theEnv,execStatus)->SalienceExpression = NULL;
      ReturnLHSParseNodes(theEnv,execStatus,theLHS);
      return(TRUE);
     }

   /*=======================*/
   /* Process the rule LHS. */
   /*=======================*/

   topDisjunct = ProcessRuleLHS(theEnv,execStatus,theLHS,actions,ruleName,&error);

   ReturnExpression(theEnv,execStatus,actions);
   ClearParsedBindNames(theEnv,execStatus);
   ReturnLHSParseNodes(theEnv,execStatus,theLHS);

   if (error)
     {
      ReturnPackedExpression(theEnv,execStatus,PatternData(theEnv,execStatus)->SalienceExpression);
      PatternData(theEnv,execStatus)->SalienceExpression = NULL;
      return(TRUE);
     }

   /*==============================================*/
   /* If we're only checking syntax, don't add the */
   /* successfully parsed defrule to the KB.       */
   /*==============================================*/

   if (ConstructData(theEnv,execStatus)->CheckSyntaxMode)
     {
      ReturnPackedExpression(theEnv,execStatus,PatternData(theEnv,execStatus)->SalienceExpression);
      PatternData(theEnv,execStatus)->SalienceExpression = NULL;
      return(FALSE);
     }

   PatternData(theEnv,execStatus)->SalienceExpression = NULL;

   /*======================================*/
   /* Save the nice printout of the rules. */
   /*======================================*/

   SavePPBuffer(theEnv,execStatus,"\n");
   if (EnvGetConserveMemory(theEnv,execStatus) == TRUE)
     { topDisjunct->header.ppForm = NULL; }
   else
     { topDisjunct->header.ppForm = CopyPPBuffer(theEnv,execStatus); }

   /*=======================================*/
   /* Store a pointer to the rule's module. */
   /*=======================================*/

   theModuleItem = (struct defruleModule *)
                   GetModuleItem(theEnv,execStatus,NULL,FindModuleItem(theEnv,execStatus,"defrule")->moduleIndex);

   for (tempPtr = topDisjunct; tempPtr != NULL; tempPtr = tempPtr->disjunct)
     { 
      tempPtr->header.whichModule = (struct defmoduleItemHeader *) theModuleItem; 
      tempPtr->header.ppForm = topDisjunct->header.ppForm;
     }

   /*===============================================*/
   /* Rule completely parsed. Add to list of rules. */
   /*===============================================*/

   AddToDefruleList(topDisjunct);

   /*========================================================================*/
   /* If a rule is redefined, then we want to restore its breakpoint status. */
   /*========================================================================*/

#if DEBUGGING_FUNCTIONS
   if (BitwiseTest(DefruleData(theEnv,execStatus)->DeletedRuleDebugFlags,0))
     { EnvSetBreak(theEnv,execStatus,topDisjunct); }
   if (BitwiseTest(DefruleData(theEnv,execStatus)->DeletedRuleDebugFlags,1) || EnvGetWatchItem(theEnv,execStatus,"activations"))
     { EnvSetDefruleWatchActivations(theEnv,execStatus,ON,(void *) topDisjunct); }
   if (BitwiseTest(DefruleData(theEnv,execStatus)->DeletedRuleDebugFlags,2) || EnvGetWatchItem(theEnv,execStatus,"rules"))
     { EnvSetDefruleWatchFirings(theEnv,execStatus,ON,(void *) topDisjunct); }
#endif

   /*================================*/
   /* Perform the incremental reset. */
   /*================================*/

   IncrementalReset(theEnv,execStatus,topDisjunct);

   /*=============================================*/
   /* Return FALSE to indicate no errors occured. */
   /*=============================================*/

#endif
   return(FALSE);
  }

#if (! RUN_TIME) && (! BLOAD_ONLY)

/**************************************************************/
/* ProcessRuleLHS: Processes each of the disjuncts of a rule. */
/**************************************************************/
static struct defrule *ProcessRuleLHS(
  void *theEnv,
  EXEC_STATUS,
  struct lhsParseNode *theLHS,
  struct expr *actions,
  SYMBOL_HN *ruleName,
  int *error)
  {
   struct lhsParseNode *tempNode = NULL;
   struct defrule *topDisjunct = NULL, *currentDisjunct, *lastDisjunct = NULL;
   struct expr *newActions, *packPtr;
   int logicalJoin;
   int localVarCnt;
   int complexity;
   struct joinNode *lastJoin;
   intBool emptyLHS;

   /*================================================*/
   /* Initially set the parsing error flag to FALSE. */
   /*================================================*/

   *error = FALSE;

   /*===========================================================*/
   /* The top level of the construct representing the LHS of a  */
   /* rule is assumed to be an OR.  If the implied OR is at the */
   /* top level of the pattern construct, then remove it.       */
   /*===========================================================*/

   if (theLHS == NULL)
     { emptyLHS = TRUE; }
   else
     {
      emptyLHS = FALSE;
      if (theLHS->type == OR_CE) theLHS = theLHS->right;
     }
 
   /*=========================================*/
   /* Loop through each disjunct of the rule. */
   /*=========================================*/

   localVarCnt = CountParsedBindNames(theEnv,execStatus);
   
   while ((theLHS != NULL) || (emptyLHS == TRUE))
     {
      /*===================================*/
      /* Analyze the LHS of this disjunct. */
      /*===================================*/

      if (emptyLHS)
        { tempNode = NULL; }
      else
        {
         if (theLHS->type == AND_CE) tempNode = theLHS->right;
         else if (theLHS->type == PATTERN_CE) tempNode = theLHS;
        }

      if (VariableAnalysis(theEnv,execStatus,tempNode))
        {
         *error = TRUE;
         ReturnDefrule(theEnv,execStatus,topDisjunct);
         return(NULL);
        }

      /*=========================================*/
      /* Perform entity dependent post analysis. */
      /*=========================================*/

      if (PostPatternAnalysis(theEnv,execStatus,tempNode))
        {
         *error = TRUE;
         ReturnDefrule(theEnv,execStatus,topDisjunct);
         return(NULL);
        }

      /*========================================================*/
      /* Print out developer information if it's being watched. */
      /*========================================================*/

#if DEVELOPER && DEBUGGING_FUNCTIONS
      if (EnvGetWatchItem(theEnv,execStatus,"rule-analysis"))
        { DumpRuleAnalysis(theEnv,execStatus,tempNode); }
#endif

      /*========================================*/
      /* Check to see that logical CEs are used */
      /* appropriately in the LHS of the rule.  */
      /*========================================*/

      if ((logicalJoin = LogicalAnalysis(theEnv,execStatus,tempNode)) < 0)
        {
         *error = TRUE;
         ReturnDefrule(theEnv,execStatus,topDisjunct);
         return(NULL);
        }

      /*======================================================*/
      /* Check to see if there are any RHS constraint errors. */
      /*======================================================*/

      if (CheckRHSForConstraintErrors(theEnv,execStatus,actions,tempNode))
        {
         *error = TRUE;
         ReturnDefrule(theEnv,execStatus,topDisjunct);
         return(NULL);
        }

      /*=================================================*/
      /* Replace variable references in the RHS with the */
      /* appropriate variable retrieval functions.       */
      /*=================================================*/

      newActions = CopyExpression(theEnv,execStatus,actions);
      if (ReplaceProcVars(theEnv,execStatus,"RHS of defrule",newActions,NULL,NULL,
                          ReplaceRHSVariable,(void *) tempNode))
        {
         *error = TRUE;
         ReturnDefrule(theEnv,execStatus,topDisjunct);
         ReturnExpression(theEnv,execStatus,newActions);
         return(NULL);
        }

      /*==================================*/
      /* We're finished for this disjunct */
      /* if we're only checking syntax.   */
      /*==================================*/

      if (ConstructData(theEnv,execStatus)->CheckSyntaxMode)
        {
         ReturnExpression(theEnv,execStatus,newActions);
         if (emptyLHS)
           { emptyLHS = FALSE; }
         else
           { theLHS = theLHS->bottom; }
         continue;
        }

      /*=================================*/
      /* Install the disjunct's actions. */
      /*=================================*/

      ExpressionInstall(theEnv,execStatus,newActions);
      packPtr = PackExpression(theEnv,execStatus,newActions);
      ReturnExpression(theEnv,execStatus,newActions);

      /*===============================================================*/
      /* Create the pattern and join data structures for the new rule. */
      /*===============================================================*/

      lastJoin = ConstructJoins(theEnv,execStatus,logicalJoin,tempNode,1);

      /*===================================================================*/
      /* Determine the rule's complexity for use with conflict resolution. */
      /*===================================================================*/

      complexity = RuleComplexity(theEnv,execStatus,tempNode);

      /*=====================================================*/
      /* Create the defrule data structure for this disjunct */
      /* and put it in the list of disjuncts for this rule.  */
      /*=====================================================*/

      currentDisjunct = CreateNewDisjunct(theEnv,execStatus,ruleName,localVarCnt,packPtr,complexity,
                                          (unsigned) logicalJoin,lastJoin);

      /*============================================================*/
      /* Place the disjunct in the list of disjuncts for this rule. */
      /* If the disjunct is the first disjunct, then increment the  */
      /* reference counts for the dynamic salience (the expression  */
      /* for the dynamic salience is only stored with the first     */
      /* disjuncts and the other disjuncts refer back to the first  */
      /* disjunct for their dynamic salience value.                 */
      /*============================================================*/

      if (topDisjunct == NULL)
        {
         topDisjunct = currentDisjunct;
         ExpressionInstall(theEnv,execStatus,topDisjunct->dynamicSalience);
        }
      else lastDisjunct->disjunct = currentDisjunct;

      /*===========================================*/
      /* Move on to the next disjunct of the rule. */
      /*===========================================*/

      lastDisjunct = currentDisjunct;
      
      if (emptyLHS)
        { emptyLHS = FALSE; }
      else
        { theLHS = theLHS->bottom; }
     }

   return(topDisjunct);
  }

/************************************************************************/
/* CreateNewDisjunct: Creates and initializes a defrule data structure. */
/************************************************************************/
static struct defrule *CreateNewDisjunct(
  void *theEnv,
  EXEC_STATUS,
  SYMBOL_HN *ruleName,
  int localVarCnt,
  struct expr *theActions,
  int complexity,
  unsigned logicalJoin,
  struct joinNode *lastJoin)
  {
   struct joinNode *tempJoin;
   struct defrule *newDisjunct;

   /*===================================================*/
   /* Create and initialize the defrule data structure. */
   /*===================================================*/

   newDisjunct = get_struct(theEnv,defrule);
   newDisjunct->header.ppForm = NULL;
   newDisjunct->header.next = NULL;
   newDisjunct->header.usrData = NULL;
   newDisjunct->logicalJoin = NULL;
   newDisjunct->disjunct = NULL;
   newDisjunct->header.name = ruleName;
   IncrementSymbolCount(newDisjunct->header.name);
   newDisjunct->actions = theActions;
   newDisjunct->salience = PatternData(theEnv,execStatus)->GlobalSalience;
   newDisjunct->afterBreakpoint = 0;
   newDisjunct->watchActivation = 0;
   newDisjunct->watchFiring = 0;
   newDisjunct->executing = 0;
   newDisjunct->complexity = complexity;
   newDisjunct->autoFocus = PatternData(theEnv,execStatus)->GlobalAutoFocus;
   newDisjunct->dynamicSalience = PatternData(theEnv,execStatus)->SalienceExpression;
   newDisjunct->localVarCnt = localVarCnt;

   /*=====================================*/
   /* Add a pointer to the rule's module. */
   /*=====================================*/

   newDisjunct->header.whichModule =
      (struct defmoduleItemHeader *)
      GetModuleItem(theEnv,execStatus,NULL,FindModuleItem(theEnv,execStatus,"defrule")->moduleIndex);

   /*============================================================*/
   /* Attach the rule's last join to the defrule data structure. */
   /*============================================================*/

   lastJoin->ruleToActivate = newDisjunct;
   newDisjunct->lastJoin = lastJoin;

   /*=================================================*/
   /* Determine the rule's logical join if it exists. */
   /*=================================================*/

   tempJoin = lastJoin;
   while (tempJoin != NULL)
     {
      if (tempJoin->depth == logicalJoin)
        {
         newDisjunct->logicalJoin = tempJoin;
         tempJoin->logicalJoin = TRUE;
        }
      tempJoin = tempJoin->lastLevel;
     }

   /*==================================================*/
   /* Return the newly created defrule data structure. */
   /*==================================================*/

   return(newDisjunct);
  }

/****************************************************************/
/* ReplaceExpressionVariables: Replaces all symbolic references */
/*   to variables (local and global) found in an expression on  */
/*   the RHS of a rule with expressions containing function     */
/*   calls to retrieve the variable's value. Makes the final    */
/*   modifications necessary for handling the modify and        */
/*   duplicate commands.                                        */
/****************************************************************/
static int ReplaceRHSVariable(
  void *theEnv,
  EXEC_STATUS,
  struct expr *list,
  void *VtheLHS)
  {
   struct lhsParseNode *theVariable;

   /*=======================================*/
   /* Handle modify and duplicate commands. */
   /*=======================================*/

#if DEFTEMPLATE_CONSTRUCT
   if (list->type == FCALL)
     {
      if (list->value == (void *) FindFunction(theEnv,execStatus,"modify"))
        {
         if (UpdateModifyDuplicate(theEnv,execStatus,list,"modify",VtheLHS) == FALSE)
           return(-1);
        }
      else if (list->value == (void *) FindFunction(theEnv,execStatus,"duplicate"))
        {
         if (UpdateModifyDuplicate(theEnv,execStatus,list,"duplicate",VtheLHS) == FALSE)
           return(-1);
        }

      return(0);
     }

#endif

   if ((list->type != SF_VARIABLE) && (list->type != MF_VARIABLE))
     { return(FALSE); }

   /*===============================================================*/
   /* Check to see if the variable is bound on the LHS of the rule. */
   /*===============================================================*/

   theVariable = FindVariable((SYMBOL_HN *) list->value,(struct lhsParseNode *) VtheLHS);
   if (theVariable == NULL) return(FALSE);

   /*================================================*/
   /* Replace the variable reference with a function */
   /* call to retrieve the variable.                 */
   /*================================================*/

   if (theVariable->patternType != NULL)
     { (*theVariable->patternType->replaceGetJNValueFunction)(theEnv,execStatus,list,theVariable,LHS); }
   else
     { return(FALSE); }

   /*=================================================================*/
   /* Return TRUE to indicate the variable was successfully replaced. */
   /*=================================================================*/

   return(TRUE);
  }

/*******************************************************/
/* ParseRuleRHS: Coordinates all the actions necessary */
/*   for parsing the RHS of a rule.                    */
/*******************************************************/
static struct expr *ParseRuleRHS(
  void *theEnv,
  EXEC_STATUS,
  char *readSource)
  {
   struct expr *actions;
   struct token theToken;

   /*=========================================================*/
   /* Process the actions on the right hand side of the rule. */
   /*=========================================================*/

   SavePPBuffer(theEnv,execStatus,"\n   ");
   SetIndentDepth(theEnv,execStatus,3);

   actions = GroupActions(theEnv,execStatus,readSource,&theToken,TRUE,NULL,FALSE);

   if (actions == NULL) return(NULL);

   /*=============================*/
   /* Reformat the closing token. */
   /*=============================*/

   PPBackup(theEnv,execStatus);
   PPBackup(theEnv,execStatus);
   SavePPBuffer(theEnv,execStatus,theToken.printForm);

   /*======================================================*/
   /* Check for the closing right parenthesis of the rule. */
   /*======================================================*/

   if (theToken.type != RPAREN)
     {
      SyntaxErrorMessage(theEnv,execStatus,"defrule");
      ReturnExpression(theEnv,execStatus,actions);
      return(NULL);
     }

   /*========================*/
   /* Return the rule's RHS. */
   /*========================*/

   return(actions);
  }

/************************************************************/
/* RuleComplexity: Returns the complexity of a rule for use */
/*   by the LEX and MEA conflict resolution strategies.     */
/************************************************************/
static int RuleComplexity(
  void *theEnv,
  EXEC_STATUS,
  struct lhsParseNode *theLHS)
  {
   struct lhsParseNode *thePattern, *tempPattern;
   int complexity = 0;

   while (theLHS != NULL)
     {
      complexity += 1; /* Add 1 for each pattern. */
      complexity += ExpressionComplexity(theEnv,execStatus,theLHS->networkTest);
      thePattern = theLHS->right;
      while (thePattern != NULL)
        {
         if (thePattern->multifieldSlot)
           {
            tempPattern = thePattern->bottom;
            while (tempPattern != NULL)
              {
               complexity += ExpressionComplexity(theEnv,execStatus,tempPattern->networkTest);
               tempPattern = tempPattern->right;
              }
           }
         else
           { complexity += ExpressionComplexity(theEnv,execStatus,thePattern->networkTest); }
         thePattern = thePattern->right;
        }
      theLHS = theLHS->bottom;
     }

   return(complexity);
  }

/********************************************************************/
/* ExpressionComplexity: Determines the complexity of a expression. */
/********************************************************************/
static int ExpressionComplexity(
   void *theEnv,
  EXEC_STATUS,
  struct expr *exprPtr)
  {
   int complexity = 0;

   while (exprPtr != NULL)
     {
      if (exprPtr->type == FCALL)
        {
         /*=========================================*/
         /* Logical combinations do not add to the  */
         /* complexity, but their arguments do.     */
         /*=========================================*/

         if ((exprPtr->value == ExpressionData(theEnv,execStatus)->PTR_AND) ||
                  (exprPtr->value == ExpressionData(theEnv,execStatus)->PTR_NOT) ||
                  (exprPtr->value == ExpressionData(theEnv,execStatus)->PTR_OR))
           { complexity += ExpressionComplexity(theEnv,execStatus,exprPtr->argList); }

         /*=========================================*/
         /* else other function calls increase the  */
         /* complexity, but their arguments do not. */
         /*=========================================*/

         else
           { complexity++; }
        }
      else if ((EvaluationData(theEnv,execStatus)->PrimitivesArray[exprPtr->type] != NULL) ?
               EvaluationData(theEnv,execStatus)->PrimitivesArray[exprPtr->type]->addsToRuleComplexity : FALSE)
        { complexity++; }

      exprPtr = exprPtr->nextArg;
     }

   return(complexity);
  }

/********************************************/
/* LogicalAnalysis: Analyzes the use of the */
/*   logical CE within the LHS of a rule.   */
/********************************************/
static int LogicalAnalysis(
  void *theEnv,
  EXEC_STATUS,
  struct lhsParseNode *patternList)
  {
   int firstLogical, logicalsFound = FALSE, logicalJoin = 1;
   int gap = FALSE;

   if (patternList == NULL) return(0);

   firstLogical = patternList->logical;

   /*===================================================*/
   /* Loop through each pattern in the LHS of the rule. */
   /*===================================================*/

   for (;
        patternList != NULL;
        patternList = patternList->bottom)
     {
      /*=======================================*/
      /* Skip anything that isn't a pattern CE */
      /* or is embedded within a not/and CE.   */
      /*=======================================*/

      if ((patternList->type != PATTERN_CE) || (patternList->endNandDepth != 1))
        { continue; }

      /*=====================================================*/
      /* If the pattern CE is not contained within a logical */
      /* CE, then set the gap flag to TRUE indicating that   */
      /* any subsequent pattern CE found within a logical CE */
      /* represents a gap between logical CEs which is an    */
      /* error.                                              */
      /*=====================================================*/

      if (patternList->logical == FALSE)
        {
         gap = TRUE;
         continue;
        }

      /*=================================================*/
      /* If a logical CE is encountered and the first CE */
      /* of the rule isn't a logical CE, then indicate   */
      /* that the first CE must be a logical CE.         */
      /*=================================================*/

      if (! firstLogical)
        {
         PrintErrorID(theEnv,execStatus,"RULEPSR",1,TRUE);
         EnvPrintRouter(theEnv,WERROR,"Logical CEs must be placed first in a rule\n");
         return(-1);
        }

      /*===================================================*/
      /* If a break within the logical CEs was found and a */
      /* new logical CE is encountered, then indicate that */
      /* there can't be any gaps between logical CEs.      */
      /*===================================================*/

      if (gap)
        {
         PrintErrorID(theEnv,execStatus,"RULEPSR",2,TRUE);
         EnvPrintRouter(theEnv,WERROR,"Gaps may not exist between logical CEs\n");
         return(-1);
        }

      /*===========================================*/
      /* Increment the count of logical CEs found. */
      /*===========================================*/

      logicalJoin++;
      logicalsFound = TRUE;
     }

   /*============================================*/
   /* If logical CEs were found, then return the */
   /* join number where the logical information  */
   /* will be stored in the join network.        */
   /*============================================*/

   if (logicalsFound) return(logicalJoin);

   /*=============================*/
   /* Return zero indicating that */
   /* no logical CE was found.    */
   /*=============================*/

   return(0);
  }

/*****************************************************************/
/* FindVariable: Searches for the last occurence of a variable   */
/*  in the LHS of a rule that is visible from the RHS of a rule. */
/*  The last occurence of the variable on the LHS side of the    */
/*  rule will have the strictest constraints (because it will    */
/*  have been intersected with all of the other constraints for  */
/*  the variable on the LHS of the rule). The strictest          */
/*  constraints are useful for performing type checking on the   */
/*  RHS of the rule.                                             */
/*****************************************************************/
globle struct lhsParseNode *FindVariable(
  SYMBOL_HN *name,
  struct lhsParseNode *theLHS)
  {
   struct lhsParseNode *theFields, *tmpFields = NULL;
   struct lhsParseNode *theReturnValue = NULL;

   /*==============================================*/
   /* Loop through each CE in the LHS of the rule. */
   /*==============================================*/

   for (;
        theLHS != NULL;
        theLHS = theLHS->bottom)
     {
      /*==========================================*/
      /* Don't bother searching for the variable  */
      /* in anything other than a pattern CE that */
      /* is not contained within a not CE.        */
      /*==========================================*/

      if ((theLHS->type != PATTERN_CE) ||
          (theLHS->negated == TRUE) ||
          (theLHS->exists == TRUE) ||
          (theLHS->beginNandDepth > 1))
        { continue; }

      /*=====================================*/
      /* Check the pattern address variable. */
      /*=====================================*/

      if (theLHS->value == (void *) name)
        { theReturnValue = theLHS; }

      /*============================================*/
      /* Check for the variable inside the pattern. */
      /*============================================*/

      theFields = theLHS->right;
      while (theFields != NULL)
        {
         /*=================================================*/
         /* Go one level deeper to check a multifield slot. */
         /*=================================================*/

         if (theFields->multifieldSlot)
           {
            tmpFields = theFields;
            theFields = theFields->bottom;
           }

         /*=================================*/
         /* See if the field being examined */
         /* is the variable being sought.   */
         /*=================================*/

         if (theFields == NULL)
           { /* Do Nothing */ }
         else if (((theFields->type == SF_VARIABLE) ||
                   (theFields->type == MF_VARIABLE)) &&
             (theFields->value == (void *) name))
           { theReturnValue = theFields; }

         /*============================*/
         /* Move on to the next field. */
         /*============================*/

         if (theFields == NULL)
           {
            theFields = tmpFields;
            tmpFields = NULL;
           }
         else if ((theFields->right == NULL) && (tmpFields != NULL))
           {
            theFields = tmpFields;
            tmpFields = NULL;
           }
         theFields = theFields->right;
        }
     }

   /*=========================================================*/
   /* Return a pointer to the LHS location where the variable */
   /* was found (or a NULL pointer if it wasn't).             */
   /*=========================================================*/

   return(theReturnValue);
  }

/**********************************************************/
/* AddToDefruleList: Adds a defrule to the list of rules. */
/**********************************************************/
static void AddToDefruleList(
  struct defrule *rulePtr)
  {
   struct defrule *tempRule;
   struct defruleModule *theModuleItem;

   theModuleItem = (struct defruleModule *) rulePtr->header.whichModule;

   if (theModuleItem->header.lastItem == NULL)
     { theModuleItem->header.firstItem = (struct constructHeader *) rulePtr; }
   else
     {
      tempRule = (struct defrule *) theModuleItem->header.lastItem;
      while (tempRule != NULL)
        {
         tempRule->header.next = (struct constructHeader *) rulePtr;
         tempRule = tempRule->disjunct;
        }
     }

   theModuleItem->header.lastItem = (struct constructHeader *) rulePtr;
  }

#if DEVELOPER && DEBUGGING_FUNCTIONS

/**********************************************************/
/* DumpRuleAnalysis:  */
/**********************************************************/
static void DumpRuleAnalysis(
  void *theEnv,
  EXEC_STATUS,
  struct lhsParseNode *tempNode)
  {
   struct lhsParseNode *traceNode;
   char buffer[20];

   EnvPrintRouter(theEnv,WDISPLAY,"\n");
   for (traceNode = tempNode; traceNode != NULL; traceNode = traceNode->bottom)
     {
      if (! traceNode->userCE)
        { continue; }
        
      gensprintf(buffer,"CE %2d: ",traceNode->whichCE);
      EnvPrintRouter(theEnv,WDISPLAY,buffer);
      PrintExpression(theEnv,execStatus,WDISPLAY,traceNode->networkTest);
      EnvPrintRouter(theEnv,WDISPLAY,"\n");

      if (traceNode->externalNetworkTest != NULL)
        { 
         EnvPrintRouter(theEnv,WDISPLAY,"       ET: ");
         PrintExpression(theEnv,execStatus,WDISPLAY,traceNode->externalNetworkTest);
         EnvPrintRouter(theEnv,WDISPLAY,"\n");
        }

      if (traceNode->secondaryNetworkTest != NULL)
        { 
         EnvPrintRouter(theEnv,WDISPLAY,"       ST: ");
         PrintExpression(theEnv,execStatus,WDISPLAY,traceNode->secondaryNetworkTest);
         EnvPrintRouter(theEnv,WDISPLAY,"\n");
        }
                 
      if (traceNode->externalRightHash != NULL)
        { 
         EnvPrintRouter(theEnv,WDISPLAY,"       EB: ");
         PrintExpression(theEnv,execStatus,WDISPLAY,traceNode->externalRightHash);
         EnvPrintRouter(theEnv,WDISPLAY,"\n");
        }
                 
      if (traceNode->externalLeftHash != NULL)
        { 
         EnvPrintRouter(theEnv,WDISPLAY,"       EH: ");
         PrintExpression(theEnv,execStatus,WDISPLAY,traceNode->externalLeftHash);
         EnvPrintRouter(theEnv,WDISPLAY,"\n");
        }
               
      if (traceNode->leftHash != NULL)
        { 
         EnvPrintRouter(theEnv,WDISPLAY,"       LH: ");
         PrintExpression(theEnv,execStatus,WDISPLAY,traceNode->leftHash);
         EnvPrintRouter(theEnv,WDISPLAY,"\n");
        }
                 
      if (traceNode->rightHash != NULL)
        { 
         EnvPrintRouter(theEnv,WDISPLAY,"       RH: ");
         PrintExpression(theEnv,execStatus,WDISPLAY,traceNode->rightHash);
         EnvPrintRouter(theEnv,WDISPLAY,"\n");
        }
                 
      if (traceNode->betaHash != NULL)
        { 
         EnvPrintRouter(theEnv,WDISPLAY,"       BH: ");
         PrintExpression(theEnv,execStatus,WDISPLAY,traceNode->betaHash);
         EnvPrintRouter(theEnv,WDISPLAY,"\n");
        }
     }
  }
#endif

#endif /* (! RUN_TIME) && (! BLOAD_ONLY) */

#endif /* DEFRULE_CONSTRUCT */


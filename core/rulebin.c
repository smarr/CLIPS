
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  10/19/06            */
   /*                                                     */
   /*              DEFRULE BSAVE/BLOAD MODULE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for the  */
/*    defrule construct.                                     */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*      Barry Cameron                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed CONFLICT_RESOLUTION_STRATEGIES,        */
/*            DYNAMIC_SALIENCE, and LOGICAL_DEPENDENCIES     */
/*            compilation flags.                             */
/*                                                           */
/*      6.30: Added support for hashed alpha memories.       */
/*                                                           */
/*            Added salience groups to improve performance   */
/*            with large numbers of activations of different */
/*            saliences.                                     */
/*                                                           */
/*************************************************************/

#define _RULEBIN_SOURCE_

#include "setup.h"

#if DEFRULE_CONSTRUCT && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME)

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include "memalloc.h"
#include "bload.h"
#include "bsave.h"
#include "envrnmnt.h"
#include "reteutil.h"
#include "agenda.h"
#include "engine.h"
#include "retract.h"
#include "rulebsc.h"
#include "pattern.h"
#include "moduldef.h"

#include "rulebin.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if BLOAD_AND_BSAVE
   static void                    BsaveFind(void *,EXEC_STATUS);
   static void                    BsaveExpressions(void *,EXEC_STATUS,FILE *);
   static void                    BsaveStorage(void *,EXEC_STATUS,FILE *);
   static void                    BsaveBinaryItem(void *,EXEC_STATUS,FILE *);
   static void                    BsaveJoins(void *,EXEC_STATUS,FILE *);
   static void                    BsaveJoin(void *,EXEC_STATUS,FILE *,struct joinNode *);
   static void                    BsaveDisjuncts(void *,EXEC_STATUS,FILE *,struct defrule *);
   static void                    BsaveTraverseJoins(void *,EXEC_STATUS,FILE *,struct joinNode *);
   static void                    BsaveLinks(void *,EXEC_STATUS,FILE *);
   static void                    BsaveTraverseLinks(void *,EXEC_STATUS,FILE *,struct joinNode *);
   static void                    BsaveLink(FILE *,struct joinLink *);
#endif
   static void                    BloadStorage(void *,EXEC_STATUS);
   static void                    BloadBinaryItem(void *,EXEC_STATUS);
   static void                    UpdateDefruleModule(void *,EXEC_STATUS,void *,long);
   static void                    UpdateDefrule(void *,EXEC_STATUS,void *,long);
   static void                    UpdateJoin(void *,EXEC_STATUS,void *,long);
   static void                    UpdateLink(void *,EXEC_STATUS,void *,long);
   static void                    ClearBload(void *,EXEC_STATUS);
   static void                    DeallocateDefruleBloadData(void *,EXEC_STATUS);

/*****************************************************/
/* DefruleBinarySetup: Installs the binary save/load */
/*   feature for the defrule construct.              */
/*****************************************************/
globle void DefruleBinarySetup(
  void *theEnv,
  EXEC_STATUS)
  {
   AllocateEnvironmentData(theEnv,execStatus,RULEBIN_DATA,sizeof(struct defruleBinaryData),DeallocateDefruleBloadData);

#if BLOAD_AND_BSAVE
   AddBinaryItem(theEnv,execStatus,"defrule",20,BsaveFind,BsaveExpressions,
                             BsaveStorage,BsaveBinaryItem,
                             BloadStorage,BloadBinaryItem,
                             ClearBload);
#endif
#if BLOAD || BLOAD_ONLY
   AddBinaryItem(theEnv,execStatus,"defrule",20,NULL,NULL,NULL,NULL,
                             BloadStorage,BloadBinaryItem,
                             ClearBload);
#endif
  }

/*******************************************************/
/* DeallocateDefruleBloadData: Deallocates environment */
/*    data for the defrule bsave functionality.        */
/*******************************************************/
static void DeallocateDefruleBloadData(
  void *theEnv,
  EXEC_STATUS)
  {
#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME)
   size_t space;
   long i;
   struct defruleModule *theModuleItem;
   struct activation *theActivation, *tmpActivation;
   struct salienceGroup *theGroup, *tmpGroup;

   for (i = 0; i < DefruleBinaryData(theEnv,execStatus)->NumberOfJoins; i++)
     { 
      DestroyBetaMemory(theEnv,execStatus,&DefruleBinaryData(theEnv,execStatus)->JoinArray[i],LHS); 
      DestroyBetaMemory(theEnv,execStatus,&DefruleBinaryData(theEnv,execStatus)->JoinArray[i],RHS); 
      ReturnLeftMemory(theEnv,execStatus,&DefruleBinaryData(theEnv,execStatus)->JoinArray[i]);
      ReturnRightMemory(theEnv,execStatus,&DefruleBinaryData(theEnv,execStatus)->JoinArray[i]);
     }

   for (i = 0; i < DefruleBinaryData(theEnv,execStatus)->NumberOfDefruleModules; i++)
     {
      theModuleItem = &DefruleBinaryData(theEnv,execStatus)->ModuleArray[i];
      
      theActivation = theModuleItem->agenda;
      while (theActivation != NULL)
        {
         tmpActivation = theActivation->next;
         
         rtn_struct(theEnv,execStatus,activation,theActivation);
         
         theActivation = tmpActivation;
        }

      theGroup = theModuleItem->groupings;
      while (theGroup != NULL)
        {
         tmpGroup = theGroup->next;
         
         rtn_struct(theEnv,execStatus,salienceGroup,theGroup);
         
         theGroup = tmpGroup;
        }
     }
     
   space = DefruleBinaryData(theEnv,execStatus)->NumberOfDefruleModules * sizeof(struct defruleModule);
   if (space != 0) genfree(theEnv,execStatus,(void *) DefruleBinaryData(theEnv,execStatus)->ModuleArray,space);
   
   space = DefruleBinaryData(theEnv,execStatus)->NumberOfDefrules * sizeof(struct defrule);
   if (space != 0) genfree(theEnv,execStatus,(void *) DefruleBinaryData(theEnv,execStatus)->DefruleArray,space);
   
   space = DefruleBinaryData(theEnv,execStatus)->NumberOfJoins * sizeof(struct joinNode);
   if (space != 0) genfree(theEnv,execStatus,(void *) DefruleBinaryData(theEnv,execStatus)->JoinArray,space);

   space = DefruleBinaryData(theEnv,execStatus)->NumberOfLinks * sizeof(struct joinLink);
   if (space != 0) genfree(theEnv,execStatus,(void *) DefruleBinaryData(theEnv,execStatus)->LinkArray,space);
   
   if (Bloaded(theEnv,execStatus))
     { rm3(theEnv,execStatus,DefruleData(theEnv,execStatus)->AlphaMemoryTable,sizeof(ALPHA_MEMORY_HASH *) * ALPHA_MEMORY_HASH_SIZE); }
#endif
  }

#if BLOAD_AND_BSAVE

/*************************************************************/
/* BsaveFind: Determines the amount of memory needed to save */
/*   the defrule and joinNode data structures in addition to */
/*   the memory needed for their associated expressions.     */
/*************************************************************/
static void BsaveFind(
  void *theEnv,
  EXEC_STATUS)
  {
   struct defrule *theDefrule, *theDisjunct;
   struct defmodule *theModule;

   /*=======================================================*/
   /* If a binary image is already loaded, then temporarily */
   /* save the count values since these will be overwritten */
   /* in the process of saving the binary image.            */
   /*=======================================================*/

   SaveBloadCount(theEnv,execStatus,DefruleBinaryData(theEnv,execStatus)->NumberOfDefruleModules);
   SaveBloadCount(theEnv,execStatus,DefruleBinaryData(theEnv,execStatus)->NumberOfDefrules);
   SaveBloadCount(theEnv,execStatus,DefruleBinaryData(theEnv,execStatus)->NumberOfJoins);
   SaveBloadCount(theEnv,execStatus,DefruleBinaryData(theEnv,execStatus)->NumberOfLinks);

   /*====================================================*/
   /* Set the binary save ID for defrule data structures */
   /* and count the number of each type.                 */
   /*====================================================*/

   TagRuleNetwork(theEnv,execStatus,&DefruleBinaryData(theEnv,execStatus)->NumberOfDefruleModules,
                         &DefruleBinaryData(theEnv,execStatus)->NumberOfDefrules,
                         &DefruleBinaryData(theEnv,execStatus)->NumberOfJoins,
                         &DefruleBinaryData(theEnv,execStatus)->NumberOfLinks);

   /*===========================*/
   /* Loop through each module. */
   /*===========================*/

   for (theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,NULL);
        theModule != NULL;
        theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,theModule))
     {
      /*============================*/
      /* Set the current module to  */
      /* the module being examined. */
      /*============================*/

      EnvSetCurrentModule(theEnv,execStatus,(void *) theModule);

      /*==================================================*/
      /* Loop through each defrule in the current module. */
      /*==================================================*/

      for (theDefrule = (struct defrule *) EnvGetNextDefrule(theEnv,execStatus,NULL);
           theDefrule != NULL;
           theDefrule = (struct defrule *) EnvGetNextDefrule(theEnv,execStatus,theDefrule))
        {
         /*================================================*/
         /* Initialize the construct header for the binary */
         /* save. The binary save ID has already been set. */
         /*================================================*/

         MarkConstructHeaderNeededItems(&theDefrule->header,theDefrule->header.bsaveID);

         /*===========================================*/
         /* Count and mark data structures associated */
         /* with dynamic salience.                    */
         /*===========================================*/

         ExpressionData(theEnv,execStatus)->ExpressionCount += ExpressionSize(theDefrule->dynamicSalience);
         MarkNeededItems(theEnv,execStatus,theDefrule->dynamicSalience);

         /*==========================================*/
         /* Loop through each disjunct of the rule   */
         /* counting and marking the data structures */
         /* associated with RHS actions.             */
         /*==========================================*/

         for (theDisjunct = theDefrule;
              theDisjunct != NULL;
              theDisjunct = theDisjunct->disjunct)
           {
            ExpressionData(theEnv,execStatus)->ExpressionCount += ExpressionSize(theDisjunct->actions);
            MarkNeededItems(theEnv,execStatus,theDisjunct->actions);
           }
        }
     }

   /*===============================*/
   /* Reset the bsave tags assigned */
   /* to defrule data structures.   */
   /*===============================*/

   MarkRuleNetwork(theEnv,execStatus,1);
  }

/************************************************/
/* BsaveExpressions: Saves the expressions used */
/*   by defrules to the binary save file.       */
/************************************************/
static void BsaveExpressions(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   struct defrule *theDefrule, *theDisjunct;
   struct defmodule *theModule;

   /*===========================*/
   /* Loop through each module. */
   /*===========================*/

   for (theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,NULL);
        theModule != NULL;
        theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,theModule))
     {
      /*======================================================*/
      /* Set the current module to the module being examined. */
      /*======================================================*/

      EnvSetCurrentModule(theEnv,execStatus,(void *) theModule);

      /*==================================================*/
      /* Loop through each defrule in the current module. */
      /*==================================================*/

      for (theDefrule = (struct defrule *) EnvGetNextDefrule(theEnv,execStatus,NULL);
           theDefrule != NULL;
           theDefrule = (struct defrule *) EnvGetNextDefrule(theEnv,execStatus,theDefrule))
        {
         /*===========================================*/
         /* Save the dynamic salience of the defrule. */
         /*===========================================*/

         BsaveExpression(theEnv,execStatus,theDefrule->dynamicSalience,fp);

         /*===================================*/
         /* Loop through each disjunct of the */
         /* defrule and save its RHS actions. */
         /*===================================*/

         for (theDisjunct = theDefrule;
              theDisjunct != NULL;
              theDisjunct = theDisjunct->disjunct)
           { BsaveExpression(theEnv,execStatus,theDisjunct->actions,fp); }
        }
     }

   /*==============================*/
   /* Set the marked flag for each */
   /* join in the join network.    */
   /*==============================*/

   MarkRuleNetwork(theEnv,execStatus,1);
  }

/*****************************************************/
/* BsaveStorage: Writes out storage requirements for */
/*   all defrule structures to the binary file       */
/*****************************************************/
static void BsaveStorage(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   size_t space;
   long int value;

   space = sizeof(long) * 5;
   GenWrite(&space,sizeof(size_t),fp);
   GenWrite(&DefruleBinaryData(theEnv,execStatus)->NumberOfDefruleModules,sizeof(long int),fp);
   GenWrite(&DefruleBinaryData(theEnv,execStatus)->NumberOfDefrules,sizeof(long int),fp);
   GenWrite(&DefruleBinaryData(theEnv,execStatus)->NumberOfJoins,sizeof(long int),fp);
   GenWrite(&DefruleBinaryData(theEnv,execStatus)->NumberOfLinks,sizeof(long int),fp);

   if (DefruleData(theEnv,execStatus)->RightPrimeJoins == NULL)
     { value = -1; }
   else
     { value = DefruleData(theEnv,execStatus)->RightPrimeJoins->bsaveID; }
   
   GenWrite(&value,sizeof(long int),fp);

   if (DefruleData(theEnv,execStatus)->LeftPrimeJoins == NULL)
     { value = -1; }
   else
     { value = DefruleData(theEnv,execStatus)->LeftPrimeJoins->bsaveID; }
   
   GenWrite(&value,sizeof(long int),fp);
  }

/*******************************************/
/* BsaveBinaryItem: Writes out all defrule */
/*   structures to the binary file.        */
/*******************************************/
static void BsaveBinaryItem(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   size_t space;
   struct defrule *theDefrule;
   struct defmodule *theModule;
   struct defruleModule *theModuleItem;
   struct bsaveDefruleModule tempDefruleModule;

   /*===============================================*/
   /* Write out the space required by the defrules. */
   /*===============================================*/

   space = (DefruleBinaryData(theEnv,execStatus)->NumberOfDefrules * sizeof(struct bsaveDefrule)) +
           (DefruleBinaryData(theEnv,execStatus)->NumberOfJoins * sizeof(struct bsaveJoinNode)) +
           (DefruleBinaryData(theEnv,execStatus)->NumberOfLinks * sizeof(struct bsaveJoinLink)) +
           (DefruleBinaryData(theEnv,execStatus)->NumberOfDefruleModules * sizeof(struct bsaveDefruleModule));
   GenWrite(&space,sizeof(size_t),fp);

   /*===============================================*/
   /* Write out each defrule module data structure. */
   /*===============================================*/

   DefruleBinaryData(theEnv,execStatus)->NumberOfDefrules = 0;
   for (theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,NULL);
        theModule != NULL;
        theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,theModule))
     {
      EnvSetCurrentModule(theEnv,execStatus,(void *) theModule);

      theModuleItem = (struct defruleModule *)
                      GetModuleItem(theEnv,execStatus,NULL,FindModuleItem(theEnv,execStatus,"defrule")->moduleIndex);
      AssignBsaveDefmdlItemHdrVals(&tempDefruleModule.header,
                                           &theModuleItem->header);
      GenWrite(&tempDefruleModule,sizeof(struct bsaveDefruleModule),fp);
     }

   /*========================================*/
   /* Write out each defrule data structure. */
   /*========================================*/

   for (theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,NULL);
        theModule != NULL;
        theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,theModule))
     {
      EnvSetCurrentModule(theEnv,execStatus,(void *) theModule);

      for (theDefrule = (struct defrule *) EnvGetNextDefrule(theEnv,execStatus,NULL);
           theDefrule != NULL;
           theDefrule = (struct defrule *) EnvGetNextDefrule(theEnv,execStatus,theDefrule))
        { BsaveDisjuncts(theEnv,execStatus,fp,theDefrule); }
     }

   /*=============================*/
   /* Write out the Rete Network. */
   /*=============================*/

   MarkRuleNetwork(theEnv,execStatus,1);
   BsaveJoins(theEnv,execStatus,fp);

   /*===========================*/
   /* Write out the join links. */
   /*===========================*/

   MarkRuleNetwork(theEnv,execStatus,1);
   BsaveLinks(theEnv,execStatus,fp);
      
   /*=============================================================*/
   /* If a binary image was already loaded when the bsave command */
   /* was issued, then restore the counts indicating the number   */
   /* of defrules, defrule modules, and joins in the binary image */
   /* (these were overwritten by the binary save).                */
   /*=============================================================*/

   RestoreBloadCount(theEnv,execStatus,&DefruleBinaryData(theEnv,execStatus)->NumberOfDefruleModules);
   RestoreBloadCount(theEnv,execStatus,&DefruleBinaryData(theEnv,execStatus)->NumberOfDefrules);
   RestoreBloadCount(theEnv,execStatus,&DefruleBinaryData(theEnv,execStatus)->NumberOfJoins);
   RestoreBloadCount(theEnv,execStatus,&DefruleBinaryData(theEnv,execStatus)->NumberOfLinks);
  }

/************************************************************/
/* BsaveDisjuncts: Writes out all the disjunct defrule data */
/*   structures for a specific rule to the binary file.     */
/************************************************************/
static void BsaveDisjuncts(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp,
  struct defrule *theDefrule)
  {
   struct defrule *theDisjunct;
   struct bsaveDefrule tempDefrule;
   long int disjunctExpressionCount = 0L;
   int first;

   /*=========================================*/
   /* Loop through each disjunct of the rule. */
   /*=========================================*/

   for (theDisjunct = theDefrule, first = TRUE;
        theDisjunct != NULL;
        theDisjunct = theDisjunct->disjunct, first = FALSE)
     {
      DefruleBinaryData(theEnv,execStatus)->NumberOfDefrules++;

      /*======================================*/
      /* Set header and miscellaneous values. */
      /*======================================*/

      AssignBsaveConstructHeaderVals(&tempDefrule.header,
                                     &theDisjunct->header);
      tempDefrule.salience = theDisjunct->salience;
      tempDefrule.localVarCnt = theDisjunct->localVarCnt;
      tempDefrule.complexity = theDisjunct->complexity;
      tempDefrule.autoFocus = theDisjunct->autoFocus;

      /*=======================================*/
      /* Set dynamic salience data structures. */
      /*=======================================*/

      if (theDisjunct->dynamicSalience != NULL)
        {
         if (first)
           {
            tempDefrule.dynamicSalience = ExpressionData(theEnv,execStatus)->ExpressionCount;
            disjunctExpressionCount = ExpressionData(theEnv,execStatus)->ExpressionCount;
            ExpressionData(theEnv,execStatus)->ExpressionCount += ExpressionSize(theDisjunct->dynamicSalience);
           }
         else
           { tempDefrule.dynamicSalience = disjunctExpressionCount; }
        }
      else
        { tempDefrule.dynamicSalience = -1L; }

      /*==============================================*/
      /* Set the index to the disjunct's RHS actions. */
      /*==============================================*/

      if (theDisjunct->actions != NULL)
        {
         tempDefrule.actions = ExpressionData(theEnv,execStatus)->ExpressionCount;
         ExpressionData(theEnv,execStatus)->ExpressionCount += ExpressionSize(theDisjunct->actions);
        }
      else
        { tempDefrule.actions = -1L; }

      /*=================================*/
      /* Set the index to the disjunct's */
      /* logical join and last join.     */
      /*=================================*/

      tempDefrule.logicalJoin = BsaveJoinIndex(theDisjunct->logicalJoin);
      tempDefrule.lastJoin = BsaveJoinIndex(theDisjunct->lastJoin);

      /*=====================================*/
      /* Set the index to the next disjunct. */
      /*=====================================*/

      if (theDisjunct->disjunct != NULL)
        { tempDefrule.disjunct = DefruleBinaryData(theEnv,execStatus)->NumberOfDefrules; }
      else
        { tempDefrule.disjunct = -1L; }

      /*=================================*/
      /* Write the disjunct to the file. */
      /*=================================*/

      GenWrite(&tempDefrule,sizeof(struct bsaveDefrule),fp);
     }
  }

/********************************************/
/* BsaveJoins: Writes out all the join node */
/*   data structures to the binary file.    */
/********************************************/
static void BsaveJoins(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   struct defrule *rulePtr;
   struct defmodule *theModule;

   /*===========================*/
   /* Loop through each module. */
   /*===========================*/

   for (theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,NULL);
        theModule != NULL;
        theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,theModule))
     {
      EnvSetCurrentModule(theEnv,execStatus,(void *) theModule);

      /*===========================================*/
      /* Loop through each rule and its disjuncts. */
      /*===========================================*/

      rulePtr = (struct defrule *) EnvGetNextDefrule(theEnv,execStatus,NULL);
      while (rulePtr != NULL)
        {
         /*=========================================*/
         /* Loop through each join of the disjunct. */
         /*=========================================*/

         BsaveTraverseJoins(theEnv,execStatus,fp,rulePtr->lastJoin);
         
         /*=======================================*/
         /* Move on to the next rule or disjunct. */
         /*=======================================*/

         if (rulePtr->disjunct != NULL) rulePtr = rulePtr->disjunct;
         else rulePtr = (struct defrule *) EnvGetNextDefrule(theEnv,execStatus,rulePtr);
        }
     }
  }

/**************************************************************/
/* BsaveTraverseJoins: Traverses the join network for a rule. */
/**************************************************************/
static void BsaveTraverseJoins(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp,
  struct joinNode *joinPtr)
  {
   for (;
        joinPtr != NULL;
        joinPtr = joinPtr->lastLevel)
     { 
      if (joinPtr->marked) BsaveJoin(theEnv,execStatus,fp,joinPtr); 
      
      if (joinPtr->joinFromTheRight)
        { BsaveTraverseJoins(theEnv,execStatus,fp,(struct joinNode *) joinPtr->rightSideEntryStructure); }
     }
  }

/********************************************/
/* BsaveJoin: Writes out a single join node */
/*   data structure to the binary file.     */
/********************************************/
static void BsaveJoin(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp,
  struct joinNode *joinPtr)
  {
   struct bsaveJoinNode tempJoin;

   joinPtr->marked = 0;
   tempJoin.depth = joinPtr->depth;
   tempJoin.rhsType = joinPtr->rhsType;
   tempJoin.firstJoin = joinPtr->firstJoin;
   tempJoin.logicalJoin = joinPtr->logicalJoin;
   tempJoin.joinFromTheRight = joinPtr->joinFromTheRight;
   tempJoin.patternIsNegated = joinPtr->patternIsNegated;
   tempJoin.patternIsExists = joinPtr->patternIsExists;

   if (joinPtr->joinFromTheRight)
     { tempJoin.rightSideEntryStructure =  BsaveJoinIndex(joinPtr->rightSideEntryStructure); }
   else
     { tempJoin.rightSideEntryStructure =  -1L; }
     
   tempJoin.lastLevel =  BsaveJoinIndex(joinPtr->lastLevel);
   tempJoin.nextLinks =  BsaveJoinLinkIndex(joinPtr->nextLinks);
   tempJoin.rightMatchNode =  BsaveJoinIndex(joinPtr->rightMatchNode);
   tempJoin.networkTest = HashedExpressionIndex(theEnv,execStatus,joinPtr->networkTest);
   tempJoin.secondaryNetworkTest = HashedExpressionIndex(theEnv,execStatus,joinPtr->secondaryNetworkTest);
   tempJoin.leftHash = HashedExpressionIndex(theEnv,execStatus,joinPtr->leftHash);
   tempJoin.rightHash = HashedExpressionIndex(theEnv,execStatus,joinPtr->rightHash);

   if (joinPtr->ruleToActivate != NULL)
     {
      tempJoin.ruleToActivate =
         GetDisjunctIndex(joinPtr->ruleToActivate);
     }
   else
     { tempJoin.ruleToActivate = -1L; }

   GenWrite(&tempJoin,(unsigned long) sizeof(struct bsaveJoinNode),fp);
  }
  
/********************************************/
/* BsaveLinks: Writes out all the join link */
/*   data structures to the binary file.    */
/********************************************/
static void BsaveLinks(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   struct defrule *rulePtr;
   struct defmodule *theModule;
   struct joinLink *theLink;

   for (theLink = DefruleData(theEnv,execStatus)->LeftPrimeJoins;
        theLink != NULL;
        theLink = theLink->next)
     { BsaveLink(fp,theLink);  }

   for (theLink = DefruleData(theEnv,execStatus)->RightPrimeJoins;
        theLink != NULL;
        theLink = theLink->next)
     { BsaveLink(fp,theLink);  }

   /*===========================*/
   /* Loop through each module. */
   /*===========================*/

   for (theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,NULL);
        theModule != NULL;
        theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,theModule))
     {
      EnvSetCurrentModule(theEnv,execStatus,(void *) theModule);

      /*===========================================*/
      /* Loop through each rule and its disjuncts. */
      /*===========================================*/

      rulePtr = (struct defrule *) EnvGetNextDefrule(theEnv,execStatus,NULL);
      while (rulePtr != NULL)
        {
         /*=========================================*/
         /* Loop through each join of the disjunct. */
         /*=========================================*/

         BsaveTraverseLinks(theEnv,execStatus,fp,rulePtr->lastJoin);
         
         /*=======================================*/
         /* Move on to the next rule or disjunct. */
         /*=======================================*/

         if (rulePtr->disjunct != NULL) rulePtr = rulePtr->disjunct;
         else rulePtr = (struct defrule *) EnvGetNextDefrule(theEnv,execStatus,rulePtr);
        }
     }
  }

/***************************************************/
/* BsaveTraverseLinks: Traverses the join network */
/*   for a rule saving the join links.            */
/**************************************************/
static void BsaveTraverseLinks(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp,
  struct joinNode *joinPtr)
  {
   struct joinLink *theLink;
   
   for (;
        joinPtr != NULL;
        joinPtr = joinPtr->lastLevel)
     { 
      if (joinPtr->marked) 
        {
         for (theLink = joinPtr->nextLinks;
              theLink != NULL;
              theLink = theLink->next)
           { BsaveLink(fp,theLink); }
         
         joinPtr->marked = 0;
        }
      
      if (joinPtr->joinFromTheRight)
        { BsaveTraverseLinks(theEnv,execStatus,fp,(struct joinNode *) joinPtr->rightSideEntryStructure); }
     }
  }

/********************************************/
/* BsaveLink: Writes out a single join link */
/*   data structure to the binary file.     */
/********************************************/
static void BsaveLink(
  FILE *fp,
  struct joinLink *linkPtr)
  {
   struct bsaveJoinLink tempLink;

   tempLink.enterDirection = linkPtr->enterDirection;
   tempLink.join =  BsaveJoinIndex(linkPtr->join);
   tempLink.next =  BsaveJoinLinkIndex(linkPtr->next);

   GenWrite(&tempLink,(unsigned long) sizeof(struct bsaveJoinLink),fp);
  }

/***********************************************************/
/* AssignBsavePatternHeaderValues: Assigns the appropriate */
/*   values to a bsave pattern header record.              */
/***********************************************************/
globle void AssignBsavePatternHeaderValues(
  void *theEnv,
  EXEC_STATUS,
  struct bsavePatternNodeHeader *theBsaveHeader,
  struct patternNodeHeader *theHeader)
  {
   theBsaveHeader->multifieldNode = theHeader->multifieldNode;
   theBsaveHeader->entryJoin = BsaveJoinIndex(theHeader->entryJoin);
   theBsaveHeader->rightHash = HashedExpressionIndex(theEnv,execStatus,theHeader->rightHash);
   theBsaveHeader->singlefieldNode = theHeader->singlefieldNode;
   theBsaveHeader->stopNode = theHeader->stopNode;
   theBsaveHeader->beginSlot = theHeader->beginSlot;
   theBsaveHeader->endSlot = theHeader->endSlot;
   theBsaveHeader->selector = theHeader->selector;
  }

#endif /* BLOAD_AND_BSAVE */

/************************************************/
/* BloadStorage: Loads storage requirements for */
/*   the defrules used by this binary image.    */
/************************************************/
static void BloadStorage(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;

   /*=================================================*/
   /* Determine the number of defrule, defruleModule, */
   /* and joinNode data structures to be read.        */
   /*=================================================*/

   GenReadBinary(theEnv,execStatus,&space,sizeof(size_t));
   GenReadBinary(theEnv,execStatus,&DefruleBinaryData(theEnv,execStatus)->NumberOfDefruleModules,sizeof(long int));
   GenReadBinary(theEnv,execStatus,&DefruleBinaryData(theEnv,execStatus)->NumberOfDefrules,sizeof(long int));
   GenReadBinary(theEnv,execStatus,&DefruleBinaryData(theEnv,execStatus)->NumberOfJoins,sizeof(long int));
   GenReadBinary(theEnv,execStatus,&DefruleBinaryData(theEnv,execStatus)->NumberOfLinks,sizeof(long int));
   GenReadBinary(theEnv,execStatus,&DefruleBinaryData(theEnv,execStatus)->RightPrimeIndex,sizeof(long int));
   GenReadBinary(theEnv,execStatus,&DefruleBinaryData(theEnv,execStatus)->LeftPrimeIndex,sizeof(long int));

   /*===================================*/
   /* Allocate the space needed for the */
   /* defruleModule data structures.    */
   /*===================================*/

   if (DefruleBinaryData(theEnv,execStatus)->NumberOfDefruleModules == 0)
     {
      DefruleBinaryData(theEnv,execStatus)->ModuleArray = NULL;
      DefruleBinaryData(theEnv,execStatus)->DefruleArray = NULL;
      DefruleBinaryData(theEnv,execStatus)->JoinArray = NULL;
     }

   space = DefruleBinaryData(theEnv,execStatus)->NumberOfDefruleModules * sizeof(struct defruleModule);
   DefruleBinaryData(theEnv,execStatus)->ModuleArray = (struct defruleModule *) genalloc(theEnv,execStatus,space);

   /*===============================*/
   /* Allocate the space needed for */
   /* the defrule data structures.  */
   /*===============================*/

   if (DefruleBinaryData(theEnv,execStatus)->NumberOfDefrules == 0)
     {
      DefruleBinaryData(theEnv,execStatus)->DefruleArray = NULL;
      DefruleBinaryData(theEnv,execStatus)->JoinArray = NULL;
      return;
     }

   space = DefruleBinaryData(theEnv,execStatus)->NumberOfDefrules * sizeof(struct defrule);
   DefruleBinaryData(theEnv,execStatus)->DefruleArray = (struct defrule *) genalloc(theEnv,execStatus,space);

   /*===============================*/
   /* Allocate the space needed for */
   /* the joinNode data structures. */
   /*===============================*/

   space = DefruleBinaryData(theEnv,execStatus)->NumberOfJoins * sizeof(struct joinNode);
   DefruleBinaryData(theEnv,execStatus)->JoinArray = (struct joinNode *) genalloc(theEnv,execStatus,space);

   /*===============================*/
   /* Allocate the space needed for */
   /* the joinNode data structures. */
   /*===============================*/

   space = DefruleBinaryData(theEnv,execStatus)->NumberOfLinks * sizeof(struct joinLink);
   DefruleBinaryData(theEnv,execStatus)->LinkArray = (struct joinLink *) genalloc(theEnv,execStatus,space);
  }

/****************************************************/
/* BloadBinaryItem: Loads and refreshes the defrule */
/*   constructs used by this binary image.          */
/****************************************************/
static void BloadBinaryItem(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;

   /*======================================================*/
   /* Read in the amount of space used by the binary image */
   /* (this is used to skip the construct in the event it  */
   /* is not available in the version being run).          */
   /*======================================================*/

   GenReadBinary(theEnv,execStatus,&space,sizeof(size_t));

   /*===========================================*/
   /* Read in the defruleModule data structures */
   /* and refresh the pointers.                 */
   /*===========================================*/

   BloadandRefresh(theEnv,execStatus,DefruleBinaryData(theEnv,execStatus)->NumberOfDefruleModules,
                   sizeof(struct bsaveDefruleModule),UpdateDefruleModule);

   /*=====================================*/
   /* Read in the defrule data structures */
   /* and refresh the pointers.           */
   /*=====================================*/

   BloadandRefresh(theEnv,execStatus,DefruleBinaryData(theEnv,execStatus)->NumberOfDefrules,
                   sizeof(struct bsaveDefrule),UpdateDefrule);

   /*======================================*/
   /* Read in the joinNode data structures */
   /* and refresh the pointers.            */
   /*======================================*/

   BloadandRefresh(theEnv,execStatus,DefruleBinaryData(theEnv,execStatus)->NumberOfJoins,
                   sizeof(struct bsaveJoinNode),UpdateJoin);

   /*======================================*/
   /* Read in the joinLink data structures */
   /* and refresh the pointers.            */
   /*======================================*/

   BloadandRefresh(theEnv,execStatus,DefruleBinaryData(theEnv,execStatus)->NumberOfLinks,
                   sizeof(struct bsaveJoinLink),UpdateLink);
             
   DefruleData(theEnv,execStatus)->RightPrimeJoins = BloadJoinLinkPointer(DefruleBinaryData(theEnv,execStatus)->RightPrimeIndex);
   DefruleData(theEnv,execStatus)->LeftPrimeJoins = BloadJoinLinkPointer(DefruleBinaryData(theEnv,execStatus)->LeftPrimeIndex);
  }

/**********************************************/
/* UpdateDefruleModule: Bload refresh routine */
/*   for defrule module data structures.      */
/**********************************************/
static void UpdateDefruleModule(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   struct bsaveDefruleModule *bdmPtr;

   bdmPtr = (struct bsaveDefruleModule *) buf;
   UpdateDefmoduleItemHeader(theEnv,execStatus,&bdmPtr->header,&DefruleBinaryData(theEnv,execStatus)->ModuleArray[obji].header,
                             (int) sizeof(struct defrule),
                             (void *) DefruleBinaryData(theEnv,execStatus)->DefruleArray);
   DefruleBinaryData(theEnv,execStatus)->ModuleArray[obji].agenda = NULL;
   DefruleBinaryData(theEnv,execStatus)->ModuleArray[obji].groupings = NULL;

  }

/****************************************/
/* UpdateDefrule: Bload refresh routine */
/*   for defrule data structures.       */
/****************************************/
static void UpdateDefrule(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   struct bsaveDefrule *br;

   br = (struct bsaveDefrule *) buf;
   UpdateConstructHeader(theEnv,execStatus,&br->header,&DefruleBinaryData(theEnv,execStatus)->DefruleArray[obji].header,
                         (int) sizeof(struct defruleModule),(void *) DefruleBinaryData(theEnv,execStatus)->ModuleArray,
                         (int) sizeof(struct defrule),(void *) DefruleBinaryData(theEnv,execStatus)->DefruleArray);

   DefruleBinaryData(theEnv,execStatus)->DefruleArray[obji].dynamicSalience = ExpressionPointer(br->dynamicSalience);

   DefruleBinaryData(theEnv,execStatus)->DefruleArray[obji].actions = ExpressionPointer(br->actions);
   DefruleBinaryData(theEnv,execStatus)->DefruleArray[obji].logicalJoin = BloadJoinPointer(br->logicalJoin);
   DefruleBinaryData(theEnv,execStatus)->DefruleArray[obji].lastJoin = BloadJoinPointer(br->lastJoin);
   DefruleBinaryData(theEnv,execStatus)->DefruleArray[obji].disjunct = BloadDefrulePointer(DefruleBinaryData(theEnv,execStatus)->DefruleArray,br->disjunct);
   DefruleBinaryData(theEnv,execStatus)->DefruleArray[obji].salience = br->salience;
   DefruleBinaryData(theEnv,execStatus)->DefruleArray[obji].localVarCnt = br->localVarCnt;
   DefruleBinaryData(theEnv,execStatus)->DefruleArray[obji].complexity = br->complexity;
   DefruleBinaryData(theEnv,execStatus)->DefruleArray[obji].autoFocus = br->autoFocus;
   DefruleBinaryData(theEnv,execStatus)->DefruleArray[obji].executing = 0;
   DefruleBinaryData(theEnv,execStatus)->DefruleArray[obji].afterBreakpoint = 0;
#if DEBUGGING_FUNCTIONS
   DefruleBinaryData(theEnv,execStatus)->DefruleArray[obji].watchActivation = AgendaData(theEnv)->WatchActivations;
   DefruleBinaryData(theEnv,execStatus)->DefruleArray[obji].watchFiring = DefruleData(theEnv,execStatus)->WatchRules;
#endif
  }

/*************************************/
/* UpdateJoin: Bload refresh routine */
/*   for joinNode data structures.   */
/*************************************/
static void UpdateJoin(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   struct bsaveJoinNode *bj;

   bj = (struct bsaveJoinNode *) buf;
   DefruleBinaryData(theEnv,execStatus)->JoinArray[obji].firstJoin = bj->firstJoin;
   DefruleBinaryData(theEnv,execStatus)->JoinArray[obji].logicalJoin = bj->logicalJoin;
   DefruleBinaryData(theEnv,execStatus)->JoinArray[obji].joinFromTheRight = bj->joinFromTheRight;
   DefruleBinaryData(theEnv,execStatus)->JoinArray[obji].patternIsNegated = bj->patternIsNegated;
   DefruleBinaryData(theEnv,execStatus)->JoinArray[obji].patternIsExists = bj->patternIsExists;
   DefruleBinaryData(theEnv,execStatus)->JoinArray[obji].depth = bj->depth;
   DefruleBinaryData(theEnv,execStatus)->JoinArray[obji].rhsType = bj->rhsType;
   DefruleBinaryData(theEnv,execStatus)->JoinArray[obji].networkTest = HashedExpressionPointer(bj->networkTest);
   DefruleBinaryData(theEnv,execStatus)->JoinArray[obji].secondaryNetworkTest = HashedExpressionPointer(bj->secondaryNetworkTest);
   DefruleBinaryData(theEnv,execStatus)->JoinArray[obji].leftHash = HashedExpressionPointer(bj->leftHash);
   DefruleBinaryData(theEnv,execStatus)->JoinArray[obji].rightHash = HashedExpressionPointer(bj->rightHash);
   DefruleBinaryData(theEnv,execStatus)->JoinArray[obji].nextLinks = BloadJoinLinkPointer(bj->nextLinks);
   DefruleBinaryData(theEnv,execStatus)->JoinArray[obji].lastLevel = BloadJoinPointer(bj->lastLevel);

   if (bj->joinFromTheRight == TRUE)
     { DefruleBinaryData(theEnv,execStatus)->JoinArray[obji].rightSideEntryStructure =  (void *) BloadJoinPointer(bj->rightSideEntryStructure); }
   else
     { DefruleBinaryData(theEnv,execStatus)->JoinArray[obji].rightSideEntryStructure = NULL; }

   DefruleBinaryData(theEnv,execStatus)->JoinArray[obji].rightMatchNode = BloadJoinPointer(bj->rightMatchNode);
   DefruleBinaryData(theEnv,execStatus)->JoinArray[obji].ruleToActivate = BloadDefrulePointer(DefruleBinaryData(theEnv,execStatus)->DefruleArray,bj->ruleToActivate);
   DefruleBinaryData(theEnv,execStatus)->JoinArray[obji].initialize = 0;
   DefruleBinaryData(theEnv,execStatus)->JoinArray[obji].marked = 0;
   DefruleBinaryData(theEnv,execStatus)->JoinArray[obji].bsaveID = 0L;
   DefruleBinaryData(theEnv,execStatus)->JoinArray[obji].leftMemory = NULL;
   DefruleBinaryData(theEnv,execStatus)->JoinArray[obji].rightMemory = NULL;

   AddBetaMemoriesToJoin(theEnv,execStatus,&DefruleBinaryData(theEnv,execStatus)->JoinArray[obji]);
  }

/*************************************/
/* UpdateLink: Bload refresh routine */
/*   for joinLink data structures.   */
/*************************************/
static void UpdateLink(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   struct bsaveJoinLink *bj;

   bj = (struct bsaveJoinLink *) buf;
   DefruleBinaryData(theEnv,execStatus)->LinkArray[obji].enterDirection = bj->enterDirection;
   DefruleBinaryData(theEnv,execStatus)->LinkArray[obji].next = BloadJoinLinkPointer(bj->next);
   DefruleBinaryData(theEnv,execStatus)->LinkArray[obji].join = BloadJoinPointer(bj->join);
  }
  
/************************************************************/
/* UpdatePatternNodeHeader: Refreshes the values in pattern */
/*   node headers from the loaded binary image.             */
/************************************************************/
globle void UpdatePatternNodeHeader(
  void *theEnv,
  EXEC_STATUS,
  struct patternNodeHeader *theHeader,
  struct bsavePatternNodeHeader *theBsaveHeader)
  {
   struct joinNode *theJoin;

   theHeader->singlefieldNode = theBsaveHeader->singlefieldNode;
   theHeader->multifieldNode = theBsaveHeader->multifieldNode;
   theHeader->stopNode = theBsaveHeader->stopNode;
   theHeader->beginSlot = theBsaveHeader->beginSlot;
   theHeader->endSlot = theBsaveHeader->endSlot;
   theHeader->selector = theBsaveHeader->selector;
   theHeader->initialize = 0;
   theHeader->marked = 0;
   theHeader->firstHash = NULL;
   theHeader->lastHash = NULL;
   theHeader->rightHash = HashedExpressionPointer(theBsaveHeader->rightHash);

   theJoin = BloadJoinPointer(theBsaveHeader->entryJoin);
   theHeader->entryJoin = theJoin;

   while (theJoin != NULL)
     {
      theJoin->rightSideEntryStructure = (void *) theHeader;
      theJoin = theJoin->rightMatchNode;
     }
  }

/**************************************/
/* ClearBload: Defrule clear routine  */
/*   when a binary load is in effect. */
/**************************************/
static void ClearBload(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;
   long i;
   struct patternParser *theParser = NULL;
   struct patternEntity *theEntity = NULL;
   void *theModule;

   /*===========================================*/
   /* Delete all known entities before removing */
   /* the defrule data structures.              */
   /*===========================================*/

   GetNextPatternEntity(theEnv,execStatus,&theParser,&theEntity);
   while (theEntity != NULL)
     {
      (*theEntity->theInfo->base.deleteFunction)(theEnv,execStatus,theEntity);
      theEntity = NULL;
      GetNextPatternEntity(theEnv,execStatus,&theParser,&theEntity);
     }

   /*=========================================*/
   /* Remove all activations from the agenda. */
   /*=========================================*/

   SaveCurrentModule(theEnv,execStatus);
   for (theModule = EnvGetNextDefmodule(theEnv,execStatus,NULL);
        theModule != NULL;
        theModule = EnvGetNextDefmodule(theEnv,execStatus,theModule))
     {
      EnvSetCurrentModule(theEnv,execStatus,theModule);
      RemoveAllActivations(theEnv,execStatus);
     }
   RestoreCurrentModule(theEnv,execStatus);
   EnvClearFocusStack(theEnv,execStatus);

   /*==========================================================*/
   /* Remove all partial matches from the beta memories in the */
   /* join network. Alpha memories do not need to be examined  */
   /* since all pattern entities have been deleted by now.     */
   /*==========================================================*/

   for (i = 0; i < DefruleBinaryData(theEnv,execStatus)->NumberOfJoins; i++)
     { 
      FlushBetaMemory(theEnv,execStatus,&DefruleBinaryData(theEnv,execStatus)->JoinArray[i],LHS); 
      ReturnLeftMemory(theEnv,execStatus,&DefruleBinaryData(theEnv,execStatus)->JoinArray[i]);
      FlushBetaMemory(theEnv,execStatus,&DefruleBinaryData(theEnv,execStatus)->JoinArray[i],RHS); 
      ReturnRightMemory(theEnv,execStatus,&DefruleBinaryData(theEnv,execStatus)->JoinArray[i]);
     }

   /*================================================*/
   /* Decrement the symbol count for each rule name. */
   /*================================================*/

   for (i = 0; i < DefruleBinaryData(theEnv,execStatus)->NumberOfDefrules; i++)
     { UnmarkConstructHeader(theEnv,execStatus,&DefruleBinaryData(theEnv,execStatus)->DefruleArray[i].header); }

   /*==================================================*/
   /* Return the space allocated for the bload arrays. */
   /*==================================================*/

   space = DefruleBinaryData(theEnv,execStatus)->NumberOfDefruleModules * sizeof(struct defruleModule);
   if (space != 0) genfree(theEnv,execStatus,(void *) DefruleBinaryData(theEnv,execStatus)->ModuleArray,space);
   DefruleBinaryData(theEnv,execStatus)->NumberOfDefruleModules = 0;
   
   space = DefruleBinaryData(theEnv,execStatus)->NumberOfDefrules * sizeof(struct defrule);
   if (space != 0) genfree(theEnv,execStatus,(void *) DefruleBinaryData(theEnv,execStatus)->DefruleArray,space);
   DefruleBinaryData(theEnv,execStatus)->NumberOfDefrules = 0;
   
   space = DefruleBinaryData(theEnv,execStatus)->NumberOfJoins * sizeof(struct joinNode);
   if (space != 0) genfree(theEnv,execStatus,(void *) DefruleBinaryData(theEnv,execStatus)->JoinArray,space);
   DefruleBinaryData(theEnv,execStatus)->NumberOfJoins = 0;

   space = DefruleBinaryData(theEnv,execStatus)->NumberOfLinks * sizeof(struct joinLink);
   if (space != 0) genfree(theEnv,execStatus,(void *) DefruleBinaryData(theEnv,execStatus)->LinkArray,space);
   DefruleBinaryData(theEnv,execStatus)->NumberOfLinks = 0;

   DefruleData(theEnv,execStatus)->RightPrimeJoins = NULL;
   DefruleData(theEnv,execStatus)->LeftPrimeJoins = NULL;
  }

/*******************************************************/
/* BloadDefruleModuleReference: Returns the defrule    */
/*   module pointer for using with the bload function. */
/*******************************************************/
globle void *BloadDefruleModuleReference(
  void *theEnv,
  EXEC_STATUS,
  int theIndex)
  {
   return ((void *) &DefruleBinaryData(theEnv,execStatus)->ModuleArray[theIndex]);
  }

#endif /* DEFRULE_CONSTRUCT && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME) */



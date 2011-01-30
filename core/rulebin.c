
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
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
/*      Brian L. Donnell                                     */
/*      Barry Cameron                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
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
#include "reteutil.h"
#include "agenda.h"
#include "engine.h"
#include "rulebsc.h"
#include "pattern.h"
#include "moduldef.h"

#include "rulebin.h"

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static long                              NumberOfDefruleModules;
   Thread static long                              NumberOfDefrules;
   Thread static long                              NumberOfJoins;
   Thread static struct defruleModule             *ModuleArray;
   Thread static struct defrule                   *DefruleArray;
   Thread static struct joinNode                  *JoinArray;

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if BLOAD_AND_BSAVE
   static void                    BsaveFind(void);
   static void                    BsaveExpressions(FILE *);
   static void                    BsaveStorage(FILE *);
   static void                    BsaveBinaryItem(FILE *);
   static void                    BsaveJoins(FILE *);
   static void                    BsaveJoin(FILE *,struct joinNode *);
   static void                    BsaveDisjuncts(FILE *,struct defrule *);
#endif
   static void                    BloadStorage(void);
   static void                    BloadBinaryItem(void);
   static void                    UpdateDefruleModule(void *,long);
   static void                    UpdateDefrule(void *,long);
   static void                    UpdateJoin(void *,long);
   static void                    ClearBload(void);

/*****************************************************/
/* DefruleBinarySetup: Installs the binary save/load */
/*   feature for the defrule construct.              */
/*****************************************************/
globle void DefruleBinarySetup()
  {
#if BLOAD_AND_BSAVE
   AddBinaryItem("defrule",20,BsaveFind,BsaveExpressions,
                             BsaveStorage,BsaveBinaryItem,
                             BloadStorage,BloadBinaryItem,
                             ClearBload);
#endif
#if BLOAD || BLOAD_ONLY
   AddBinaryItem("defrule",20,NULL,NULL,NULL,NULL,
                             BloadStorage,BloadBinaryItem,
                             ClearBload);
#endif
  }

#if BLOAD_AND_BSAVE

/*************************************************************/
/* BsaveFind: Determines the amount of memory needed to save */
/*   the defrule and joinNode data structures in addition to */
/*   the memory needed for their associated expressions.     */
/*************************************************************/
static void BsaveFind()
  {
   struct defrule *theDefrule, *theDisjunct;
   struct defmodule *theModule;

   /*=======================================================*/
   /* If a binary image is already loaded, then temporarily */
   /* save the count values since these will be overwritten */
   /* in the process of saving the binary image.            */
   /*=======================================================*/

   if (Bloaded())
     {
      SaveBloadCount(NumberOfDefruleModules);
      SaveBloadCount(NumberOfDefrules);
      SaveBloadCount(NumberOfJoins);
     }

   /*====================================================*/
   /* Set the binary save ID for defrule data structures */
   /* and count the number of each type.                 */
   /*====================================================*/

   TagRuleNetwork(&NumberOfDefruleModules,&NumberOfDefrules,&NumberOfJoins);

   /*===========================*/
   /* Loop through each module. */
   /*===========================*/

   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      /*============================*/
      /* Set the current module to  */
      /* the module being examined. */
      /*============================*/

      SetCurrentModule((void *) theModule);

      /*==================================================*/
      /* Loop through each defrule in the current module. */
      /*==================================================*/

      for (theDefrule = (struct defrule *) GetNextDefrule(NULL);
           theDefrule != NULL;
           theDefrule = (struct defrule *) GetNextDefrule(theDefrule))
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

#if DYNAMIC_SALIENCE
         ExpressionCount += ExpressionSize(theDefrule->dynamicSalience);
         MarkNeededItems(theDefrule->dynamicSalience);
#endif

         /*==========================================*/
         /* Loop through each disjunct of the rule   */
         /* counting and marking the data structures */
         /* associated with RHS actions.             */
         /*==========================================*/

         for (theDisjunct = theDefrule;
              theDisjunct != NULL;
              theDisjunct = theDisjunct->disjunct)
           {
            ExpressionCount += ExpressionSize(theDisjunct->actions);
            MarkNeededItems(theDisjunct->actions);
           }
        }
     }

   /*===============================*/
   /* Reset the bsave tags assigned */
   /* to defrule data structures.   */
   /*===============================*/

   MarkRuleNetwork(1);
  }

/************************************************/
/* BsaveExpressions: Saves the expressions used */
/*   by defrules to the binary save file.       */
/************************************************/
static void BsaveExpressions(
  FILE *fp)
  {
   struct defrule *theDefrule, *theDisjunct;
   struct defmodule *theModule;

   /*===========================*/
   /* Loop through each module. */
   /*===========================*/

   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      /*======================================================*/
      /* Set the current module to the module being examined. */
      /*======================================================*/

      SetCurrentModule((void *) theModule);

      /*==================================================*/
      /* Loop through each defrule in the current module. */
      /*==================================================*/

      for (theDefrule = (struct defrule *) GetNextDefrule(NULL);
           theDefrule != NULL;
           theDefrule = (struct defrule *) GetNextDefrule(theDefrule))
        {
         /*===========================================*/
         /* Save the dynamic salience of the defrule. */
         /*===========================================*/

#if DYNAMIC_SALIENCE
         BsaveExpression(theDefrule->dynamicSalience,fp);
#endif

         /*===================================*/
         /* Loop through each disjunct of the */
         /* defrule and save its RHS actions. */
         /*===================================*/

         for (theDisjunct = theDefrule;
              theDisjunct != NULL;
              theDisjunct = theDisjunct->disjunct)
           { BsaveExpression(theDisjunct->actions,fp); }
        }
     }

   /*==============================*/
   /* Set the marked flag for each */
   /* join in the join network.    */
   /*==============================*/

   MarkRuleNetwork(1);
  }

/*****************************************************/
/* BsaveStorage: Writes out storage requirements for */
/*   all defrule structures to the binary file       */
/*****************************************************/
static void BsaveStorage(
  FILE *fp)
  {
   unsigned long space;

   space = sizeof(long) * 3;
   GenWrite(&space,(unsigned long) sizeof(unsigned long int),fp);
   GenWrite(&NumberOfDefruleModules,(unsigned long) sizeof(long int),fp);
   GenWrite(&NumberOfDefrules,(unsigned long) sizeof(long int),fp);
   GenWrite(&NumberOfJoins,(unsigned long) sizeof(long int),fp);
  }

/*******************************************/
/* BsaveBinaryItem: Writes out all defrule */
/*   structures to the binary file.        */
/*******************************************/
static void BsaveBinaryItem(
  FILE *fp)
  {
   unsigned long int space;
   struct defrule *theDefrule;
   struct defmodule *theModule;
   struct defruleModule *theModuleItem;
   struct bsaveDefruleModule tempDefruleModule;

   /*===============================================*/
   /* Write out the space required by the defrules. */
   /*===============================================*/

   space = (NumberOfDefrules * sizeof(struct bsaveDefrule)) +
           (NumberOfJoins * sizeof(struct bsaveJoinNode)) +
           (NumberOfDefruleModules * sizeof(struct bsaveDefruleModule));
   GenWrite(&space,(unsigned long) sizeof(unsigned long int),fp);

   /*===============================================*/
   /* Write out each defrule module data structure. */
   /*===============================================*/

   NumberOfDefrules = 0;
   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      SetCurrentModule((void *) theModule);

      theModuleItem = (struct defruleModule *)
                      GetModuleItem(NULL,FindModuleItem("defrule")->moduleIndex);
      AssignBsaveDefmdlItemHdrVals(&tempDefruleModule.header,
                                           &theModuleItem->header);
      GenWrite(&tempDefruleModule,(unsigned long) sizeof(struct bsaveDefruleModule),fp);
     }

   /*========================================*/
   /* Write out each defrule data structure. */
   /*========================================*/

   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      SetCurrentModule((void *) theModule);

      for (theDefrule = (struct defrule *) GetNextDefrule(NULL);
           theDefrule != NULL;
           theDefrule = (struct defrule *) GetNextDefrule(theDefrule))
        { BsaveDisjuncts(fp,theDefrule); }
     }

   /*=============================*/
   /* Write out the Rete Network. */
   /*=============================*/

   MarkRuleNetwork(1);
   BsaveJoins(fp);

   /*=============================================================*/
   /* If a binary image was already loaded when the bsave command */
   /* was issued, then restore the counts indicating the number   */
   /* of defrules, defrule modules, and joins in the binary image */
   /* (these were overwritten by the binary save).                */
   /*=============================================================*/

   if (Bloaded())
     {
      RestoreBloadCount(&NumberOfDefruleModules);
      RestoreBloadCount(&NumberOfDefrules);
      RestoreBloadCount(&NumberOfJoins);
     }
  }

/************************************************************/
/* BsaveDisjuncts: Writes out all the disjunct defrule data */
/*   structures for a specific rule to the binary file.     */
/************************************************************/
static void BsaveDisjuncts(
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
      NumberOfDefrules++;

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

#if DYNAMIC_SALIENCE
      if (theDisjunct->dynamicSalience != NULL)
        {
         if (first)
           {
            tempDefrule.dynamicSalience = ExpressionCount;
            disjunctExpressionCount = ExpressionCount;
            ExpressionCount += ExpressionSize(theDisjunct->dynamicSalience);
           }
         else
           { tempDefrule.dynamicSalience = disjunctExpressionCount; }
        }
      else
#endif
        { tempDefrule.dynamicSalience = -1L; }

      /*==============================================*/
      /* Set the index to the disjunct's RHS actions. */
      /*==============================================*/

      if (theDisjunct->actions != NULL)
        {
         tempDefrule.actions = ExpressionCount;
         ExpressionCount += ExpressionSize(theDisjunct->actions);
        }
      else
        { tempDefrule.actions = -1L; }

      /*=================================*/
      /* Set the index to the disjunct's */
      /* logical join and last join.     */
      /*=================================*/

#if LOGICAL_DEPENDENCIES
      tempDefrule.logicalJoin = BsaveJoinIndex(theDisjunct->logicalJoin);
#else
      tempDefrule.logicalJoin = -1L;
#endif
      tempDefrule.lastJoin = BsaveJoinIndex(theDisjunct->lastJoin);

      /*=====================================*/
      /* Set the index to the next disjunct. */
      /*=====================================*/

      if (theDisjunct->disjunct != NULL)
        { tempDefrule.disjunct = NumberOfDefrules; }
      else
        { tempDefrule.disjunct = -1L; }

      /*=================================*/
      /* Write the disjunct to the file. */
      /*=================================*/

      GenWrite(&tempDefrule,(unsigned long) sizeof(struct bsaveDefrule),fp);
     }
  }

/********************************************/
/* BsaveJoins: Writes out all the join node */
/*   data structures to the binary file.    */
/********************************************/
static void BsaveJoins(
  FILE *fp)
  {
   struct defrule *rulePtr;
   struct joinNode *joinPtr;
   struct defmodule *theModule;

   /*===========================*/
   /* Loop through each module. */
   /*===========================*/

   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      SetCurrentModule((void *) theModule);

      /*===========================================*/
      /* Loop through each rule and its disjuncts. */
      /*===========================================*/

      rulePtr = (struct defrule *) GetNextDefrule(NULL);
      while (rulePtr != NULL)
        {
         /*=========================================*/
         /* Loop through each join of the disjunct. */
         /*=========================================*/

         for (joinPtr = rulePtr->lastJoin;
              joinPtr != NULL;
              joinPtr = GetPreviousJoin(joinPtr))
           { if (joinPtr->marked) BsaveJoin(fp,joinPtr); }

         /*=======================================*/
         /* Move on to the next rule or disjunct. */
         /*=======================================*/

         if (rulePtr->disjunct != NULL) rulePtr = rulePtr->disjunct;
         else rulePtr = (struct defrule *) GetNextDefrule(rulePtr);
        }
     }
  }

/********************************************/
/* BsaveJoin: Writes out a single join node */
/*   data structure to the binary file.     */
/********************************************/
static void BsaveJoin(
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

   if (joinPtr->joinFromTheRight)
     { tempJoin.rightSideEntryStructure =  BsaveJoinIndex(joinPtr->rightSideEntryStructure); }
   else
     { tempJoin.rightSideEntryStructure =  -1L; }

   tempJoin.lastLevel =  BsaveJoinIndex(joinPtr->lastLevel);
   tempJoin.nextLevel =  BsaveJoinIndex(joinPtr->nextLevel);
   tempJoin.rightMatchNode =  BsaveJoinIndex(joinPtr->rightMatchNode);
   tempJoin.rightDriveNode =  BsaveJoinIndex(joinPtr->rightDriveNode);
   tempJoin.networkTest = HashedExpressionIndex(joinPtr->networkTest);

   if (joinPtr->ruleToActivate != NULL)
     {
      tempJoin.ruleToActivate =
         GetDisjunctIndex(joinPtr->ruleToActivate);
     }
   else
     { tempJoin.ruleToActivate = -1L; }

   GenWrite(&tempJoin,(unsigned long) sizeof(struct bsaveJoinNode),fp);
  }

/***********************************************************/
/* AssignBsavePatternHeaderValues: Assigns the appropriate */
/*   values to a bsave pattern header record.              */
/***********************************************************/
globle void AssignBsavePatternHeaderValues(
  struct bsavePatternNodeHeader *theBsaveHeader,
  struct patternNodeHeader *theHeader)
  {
   theBsaveHeader->multifieldNode = theHeader->multifieldNode;
   theBsaveHeader->entryJoin = BsaveJoinIndex(theHeader->entryJoin);
   theBsaveHeader->singlefieldNode = theHeader->singlefieldNode;
   theBsaveHeader->stopNode = theHeader->stopNode;
   theBsaveHeader->beginSlot = theHeader->beginSlot;
   theBsaveHeader->endSlot = theHeader->endSlot;
  }

#endif /* BLOAD_AND_BSAVE */

/************************************************/
/* BloadStorage: Loads storage requirements for */
/*   the defrules used by this binary image.    */
/************************************************/
static void BloadStorage()
  {
   unsigned long space;

   /*=================================================*/
   /* Determine the number of defrule, defruleModule, */
   /* and joinNode data structures to be read.        */
   /*=================================================*/

   GenRead(&space,(unsigned long) sizeof(unsigned long int));
   GenRead(&NumberOfDefruleModules,(unsigned long) sizeof(long int));
   GenRead(&NumberOfDefrules,(unsigned long) sizeof(long int));
   GenRead(&NumberOfJoins,(unsigned long) sizeof(long int));

   /*===================================*/
   /* Allocate the space needed for the */
   /* defruleModule data structures.    */
   /*===================================*/

   if (NumberOfDefruleModules == 0)
     {
      ModuleArray = NULL;
      DefruleArray = NULL;
      JoinArray = NULL;
     }

   space = NumberOfDefruleModules * sizeof(struct defruleModule);
   ModuleArray = (struct defruleModule *) genlongalloc(space);

   /*===============================*/
   /* Allocate the space needed for */
   /* the defrule data structures.  */
   /*===============================*/

   if (NumberOfDefrules == 0)
     {
      DefruleArray = NULL;
      JoinArray = NULL;
      return;
     }

   space = NumberOfDefrules * sizeof(struct defrule);
   DefruleArray = (struct defrule *) genlongalloc(space);

   /*===============================*/
   /* Allocate the space needed for */
   /* the joinNode data structures. */
   /*===============================*/

   space = NumberOfJoins * sizeof(struct joinNode);
   JoinArray = (struct joinNode *) genlongalloc(space);
  }

/****************************************************/
/* BloadBinaryItem: Loads and refreshes the defrule */
/*   constructs used by this binary image.          */
/****************************************************/
static void BloadBinaryItem()
  {
   unsigned long space;

   /*======================================================*/
   /* Read in the amount of space used by the binary image */
   /* (this is used to skip the construct in the event it  */
   /* is not available in the version being run).          */
   /*======================================================*/

   GenRead(&space,(unsigned long) sizeof(unsigned long int));

   /*===========================================*/
   /* Read in the defruleModule data structures */
   /* and refresh the pointers.                 */
   /*===========================================*/

   BloadandRefresh(NumberOfDefruleModules,(unsigned) sizeof(struct bsaveDefruleModule),
                   UpdateDefruleModule);

   /*=====================================*/
   /* Read in the defrule data structures */
   /* and refresh the pointers.           */
   /*=====================================*/

   BloadandRefresh(NumberOfDefrules,(unsigned) sizeof(struct bsaveDefrule),
                   UpdateDefrule);

   /*======================================*/
   /* Read in the joinNode data structures */
   /* and refresh the pointers.            */
   /*======================================*/

   BloadandRefresh(NumberOfJoins,(unsigned) sizeof(struct bsaveJoinNode),
                   UpdateJoin);
  }

/**********************************************/
/* UpdateDefruleModule: Bload refresh routine */
/*   for defrule module data structures.      */
/**********************************************/
static void UpdateDefruleModule(
  void *buf,
  long obji)
  {
   struct bsaveDefruleModule *bdmPtr;

   bdmPtr = (struct bsaveDefruleModule *) buf;
   UpdateDefmoduleItemHeader(&bdmPtr->header,&ModuleArray[obji].header,
                             (int) sizeof(struct defrule),
                             (void *) DefruleArray);
   ModuleArray[obji].agenda = NULL;
  }

/****************************************/
/* UpdateDefrule: Bload refresh routine */
/*   for defrule data structures.       */
/****************************************/
static void UpdateDefrule(
  void *buf,
  long obji)
  {
   struct bsaveDefrule *br;

   br = (struct bsaveDefrule *) buf;
   UpdateConstructHeader(&br->header,&DefruleArray[obji].header,
                         (int) sizeof(struct defruleModule),(void *) ModuleArray,
                         (int) sizeof(struct defrule),(void *) DefruleArray);
#if DYNAMIC_SALIENCE
   DefruleArray[obji].dynamicSalience = ExpressionPointer(br->dynamicSalience);
#endif
   DefruleArray[obji].actions = ExpressionPointer(br->actions);
#if LOGICAL_DEPENDENCIES
   DefruleArray[obji].logicalJoin = BloadJoinPointer(br->logicalJoin);
#endif
   DefruleArray[obji].lastJoin = BloadJoinPointer(br->lastJoin);
   DefruleArray[obji].disjunct = BloadDefrulePointer(DefruleArray,br->disjunct);
   DefruleArray[obji].salience = br->salience;
   DefruleArray[obji].localVarCnt = br->localVarCnt;
   DefruleArray[obji].complexity = br->complexity;
   DefruleArray[obji].autoFocus = br->autoFocus;
   DefruleArray[obji].executing = 0;
   DefruleArray[obji].afterBreakpoint = 0;
#if DEBUGGING_FUNCTIONS
   DefruleArray[obji].watchActivation = WatchActivations;
   DefruleArray[obji].watchFiring = WatchRules;
#endif
  }

/*************************************/
/* UpdateJoin: Bload refresh routine */
/*   for joinNode data structures.   */
/*************************************/
static void UpdateJoin(
  void *buf,
  long obji)
  {
   struct bsaveJoinNode *bj;

   bj = (struct bsaveJoinNode *) buf;
   JoinArray[obji].firstJoin = bj->firstJoin;
   JoinArray[obji].logicalJoin = bj->logicalJoin;
   JoinArray[obji].joinFromTheRight = bj->joinFromTheRight;
   JoinArray[obji].patternIsNegated = bj->patternIsNegated;
   JoinArray[obji].depth = bj->depth;
   JoinArray[obji].rhsType = bj->rhsType;
   JoinArray[obji].networkTest = HashedExpressionPointer(bj->networkTest);
   JoinArray[obji].nextLevel = BloadJoinPointer(bj->nextLevel);
   JoinArray[obji].lastLevel = BloadJoinPointer(bj->lastLevel);

   if (bj->joinFromTheRight == TRUE)
     { JoinArray[obji].rightSideEntryStructure =  (void *) BloadJoinPointer(bj->rightSideEntryStructure); }

   JoinArray[obji].rightMatchNode = BloadJoinPointer(bj->rightMatchNode);
   JoinArray[obji].rightDriveNode = BloadJoinPointer(bj->rightDriveNode);
   JoinArray[obji].ruleToActivate = BloadDefrulePointer(DefruleArray,bj->ruleToActivate);
   JoinArray[obji].initialize = 0;
   JoinArray[obji].marked = 0;
   JoinArray[obji].bsaveID = 0L;
   JoinArray[obji].beta = NULL;
  }

/************************************************************/
/* UpdatePatternNodeHeader: Refreshes the values in pattern */
/*   node headers from the loaded binary image.             */
/************************************************************/
globle void UpdatePatternNodeHeader(
  struct patternNodeHeader *theHeader,
  struct bsavePatternNodeHeader *theBsaveHeader)
  {
   struct joinNode *theJoin;

   theHeader->singlefieldNode = theBsaveHeader->singlefieldNode;
   theHeader->multifieldNode = theBsaveHeader->multifieldNode;
   theHeader->stopNode = theBsaveHeader->stopNode;
   theHeader->beginSlot = theBsaveHeader->beginSlot;
   theHeader->endSlot = theBsaveHeader->endSlot;
   theHeader->initialize = 0;
   theHeader->marked = 0;
   theHeader->alphaMemory = NULL;
   theHeader->endOfQueue = NULL;

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
static void ClearBload()
  {
   unsigned long int space;
   long i;
   struct patternParser *theParser = NULL;
   struct patternEntity *theEntity = NULL;
   void *theModule;

   /*===========================================*/
   /* Delete all known entities before removing */
   /* the defrule data structures.              */
   /*===========================================*/

   GetNextPatternEntity(&theParser,&theEntity);
   while (theEntity != NULL)
     {
      (*theEntity->theInfo->base.deleteFunction)(theEntity);
      theEntity = NULL;
      GetNextPatternEntity(&theParser,&theEntity);
     }

   /*=========================================*/
   /* Remove all activations from the agenda. */
   /*=========================================*/

   SaveCurrentModule();
   for (theModule = GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = GetNextDefmodule(theModule))
     {
      SetCurrentModule(theModule);
      RemoveAllActivations();
     }
   RestoreCurrentModule();
   ClearFocusStack();

   /*==========================================================*/
   /* Remove all partial matches from the beta memories in the */
   /* join network. Alpha memories do not need to be examined  */
   /* since all pattern entities have been deleted by now.     */
   /*==========================================================*/

   for (i = 0; i < NumberOfJoins; i++)
     { FlushAlphaBetaMemory(JoinArray[i].beta); }

   /*================================================*/
   /* Decrement the symbol count for each rule name. */
   /*================================================*/

   for (i = 0; i < NumberOfDefrules; i++)
     { UnmarkConstructHeader(&DefruleArray[i].header); }

   /*==================================================*/
   /* Return the space allocated for the bload arrays. */
   /*==================================================*/

   space = NumberOfDefruleModules * sizeof(struct defruleModule);
   if (space != 0) genlongfree((void *) ModuleArray,space);

   space = NumberOfDefrules * sizeof(struct defrule);
   if (space != 0) genlongfree((void *) DefruleArray,space);

   space = NumberOfJoins * sizeof(struct joinNode);
   if (space != 0) genlongfree((void *) JoinArray,space);
  }

/*******************************************************/
/* BloadDefruleModuleReference: Returns the defrule    */
/*   module pointer for using with the bload function. */
/*******************************************************/
globle void *BloadDefruleModuleReference(
  int index)
  {
   return ((void *) &ModuleArray[index]);
  }

#endif /* DEFRULE_CONSTRUCT && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME) */



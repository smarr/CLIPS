   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.30  10/19/06          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Binary Load/Save Functions Defrule               */
/*          Object Pattern Network                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Converted INSTANCE_PATTERN_MATCHING to         */
/*            DEFRULE_CONSTRUCT.                             */
/*                                                           */
/*            ResetObjectMatchTimeTags did not pass in the   */
/*            environment argument when BLOAD_ONLY was set.  */
/*                                                           */
/*      6.30: Added support for hashed alpha memories.       */
/*                                                           */
/*************************************************************/

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "setup.h"

#if DEFRULE_CONSTRUCT && OBJECT_SYSTEM && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)

#include "bload.h"
#include "bsave.h"
#include "envrnmnt.h"
#include "memalloc.h"
#include "insfun.h"
#include "objrtmch.h"
#include "reteutil.h"
#include "rulebin.h"

#define _OBJRTBIN_SOURCE_
#include "objrtbin.h"

/* =========================================
   *****************************************
               MACROS AND TYPES
   =========================================
   ***************************************** */
typedef unsigned long UNLN;

typedef struct bsaveObjectPatternNode
  {
   unsigned multifieldNode : 1;
   unsigned endSlot        : 1;
   unsigned selector       : 1;
   unsigned whichField     : 8;
   unsigned short leaveFields;
   unsigned slotNameID;
   long networkTest,
        nextLevel,
        lastLevel,
        leftNode,
        rightNode,
        alphaNode;
  } BSAVE_OBJECT_PATTERN_NODE;

typedef struct bsaveObjectAlphaNode
  {
   struct bsavePatternNodeHeader header;
   long classbmp,
        slotbmp,
        patternNode,
        nxtInGroup,
        nxtTerminal;
  } BSAVE_OBJECT_ALPHA_NODE;

#define BsaveObjectPatternIndex(op) ((op != NULL) ? op->bsaveID : -1L)
#define BsaveObjectAlphaIndex(ap)   ((ap != NULL) ? ap->bsaveID : -1L)

#define ObjectPatternPointer(i) ((i == -1L) ? NULL : (OBJECT_PATTERN_NODE *) &ObjectReteBinaryData(theEnv,execStatus)->PatternArray[i])
#define ObjectAlphaPointer(i)   ((i == -1L) ? NULL : (OBJECT_ALPHA_NODE *) &ObjectReteBinaryData(theEnv,execStatus)->AlphaArray[i])

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

#if BLOAD_AND_BSAVE
static void BsaveObjectPatternsFind(void *,EXEC_STATUS);
static void BsaveStorageObjectPatterns(void *,EXEC_STATUS,FILE *);
static void BsaveObjectPatterns(void *,EXEC_STATUS,FILE *);
#endif
static void BloadStorageObjectPatterns(void *,EXEC_STATUS);
static void BloadObjectPatterns(void *,EXEC_STATUS);
static void UpdateAlpha(void *,EXEC_STATUS,void *,long);
static void UpdatePattern(void *,EXEC_STATUS,void *,long);
static void ClearBloadObjectPatterns(void *,EXEC_STATUS);
static void DeallocateObjectReteBinaryData(void *,EXEC_STATUS);

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***********************************************************
  NAME         : SetupObjectPatternsBload
  DESCRIPTION  : Initializes data structures and
                   routines for binary loads of
                   generic function constructs
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Routines defined and structures initialized
  NOTES        : None
 ***********************************************************/
globle void SetupObjectPatternsBload(
  void *theEnv,
  EXEC_STATUS)
  {
   AllocateEnvironmentData(theEnv,execStatus,OBJECTRETEBIN_DATA,sizeof(struct objectReteBinaryData),DeallocateObjectReteBinaryData);

#if BLOAD_AND_BSAVE
   AddBinaryItem(theEnv,execStatus,"object patterns",0,BsaveObjectPatternsFind,NULL,
                             BsaveStorageObjectPatterns,BsaveObjectPatterns,
                             BloadStorageObjectPatterns,BloadObjectPatterns,
                             ClearBloadObjectPatterns);
#endif
#if BLOAD || BLOAD_ONLY
   AddBinaryItem(theEnv,execStatus,"object patterns",0,NULL,NULL,NULL,NULL,
                             BloadStorageObjectPatterns,BloadObjectPatterns,
                             ClearBloadObjectPatterns);
#endif
  }
  
/***********************************************************/
/* DeallocateObjectReteBinaryData: Deallocates environment */
/*    data for object rete binary functionality.           */
/***********************************************************/
static void DeallocateObjectReteBinaryData(
  void *theEnv,
  EXEC_STATUS)
  {
#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME)
   size_t space;
   long i;

   for (i = 0; i < ObjectReteBinaryData(theEnv,execStatus)->AlphaNodeCount; i++)
     { DestroyAlphaMemory(theEnv,execStatus,&ObjectReteBinaryData(theEnv,execStatus)->AlphaArray[i].header,FALSE); }

   space = ObjectReteBinaryData(theEnv,execStatus)->AlphaNodeCount * sizeof(struct objectAlphaNode);
   if (space != 0) genfree(theEnv,execStatus,(void *) ObjectReteBinaryData(theEnv,execStatus)->AlphaArray,space);

   space = ObjectReteBinaryData(theEnv,execStatus)->PatternNodeCount * sizeof(struct objectPatternNode);
   if (space != 0) genfree(theEnv,execStatus,(void *) ObjectReteBinaryData(theEnv,execStatus)->PatternArray,space);

#endif
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

#if BLOAD_AND_BSAVE

/***************************************************
  NAME         : BsaveObjectPatternsFind
  DESCRIPTION  : Sets the Bsave IDs for the object
                 pattern data structures and
                 determines how much space
                 (including padding) is necessary
                 for the alpha node bitmPS
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Counts written
  NOTES        : None
 ***************************************************/
static void BsaveObjectPatternsFind(
  void *theEnv,
  EXEC_STATUS)
  {
   OBJECT_ALPHA_NODE *alphaPtr;
   OBJECT_PATTERN_NODE *patternPtr;

   SaveBloadCount(theEnv,execStatus,ObjectReteBinaryData(theEnv,execStatus)->AlphaNodeCount);
   SaveBloadCount(theEnv,execStatus,ObjectReteBinaryData(theEnv,execStatus)->PatternNodeCount);

   ObjectReteBinaryData(theEnv,execStatus)->AlphaNodeCount = 0L;
   alphaPtr = ObjectNetworkTerminalPointer(theEnv,execStatus);
   while (alphaPtr != NULL)
     {
      alphaPtr->classbmp->neededBitMap = TRUE;
      if (alphaPtr->slotbmp != NULL)
        alphaPtr->slotbmp->neededBitMap = TRUE;
      alphaPtr->bsaveID = ObjectReteBinaryData(theEnv,execStatus)->AlphaNodeCount++;
      alphaPtr = alphaPtr->nxtTerminal;
     }

   ObjectReteBinaryData(theEnv,execStatus)->PatternNodeCount = 0L;
   patternPtr = ObjectNetworkPointer(theEnv,execStatus);
   while (patternPtr != NULL)
     {
      patternPtr->bsaveID = ObjectReteBinaryData(theEnv,execStatus)->PatternNodeCount++;
      if (patternPtr->nextLevel == NULL)
        {
         while (patternPtr->rightNode == NULL)
           {
            patternPtr = patternPtr->lastLevel;
            if (patternPtr == NULL)
              return;
           }
         patternPtr = patternPtr->rightNode;
        }
      else
        patternPtr = patternPtr->nextLevel;
     }
  }

/****************************************************
  NAME         : BsaveStorageObjectPatterns
  DESCRIPTION  : Writes out the number of bytes
                 required for object pattern bitmaps,
                 and the number of object pattern
                 alpha an intermediate nodes
  INPUTS       : Bsave file stream pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Counts written
  NOTES        : None
 ****************************************************/
static void BsaveStorageObjectPatterns(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   size_t space;

   space = sizeof(long) * 2;
   GenWrite(&space,sizeof(size_t),fp);
   GenWrite(&ObjectReteBinaryData(theEnv,execStatus)->AlphaNodeCount,sizeof(long),fp);
   GenWrite(&ObjectReteBinaryData(theEnv,execStatus)->PatternNodeCount,sizeof(long),fp);
  }

/***************************************************
  NAME         : BsaveObjectPatterns
  DESCRIPTION  : Writes ouyt object pattern data
                 structures to binary save file
  INPUTS       : Bsave file stream pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Data structures written
  NOTES        : Extra padding written with alpha
                 node bitmaps to ensure correct
                 alignment of structues on bload
 ***************************************************/
static void BsaveObjectPatterns(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   size_t space;
   OBJECT_ALPHA_NODE *alphaPtr;
   OBJECT_PATTERN_NODE *patternPtr;
   BSAVE_OBJECT_ALPHA_NODE dummyAlpha;
   BSAVE_OBJECT_PATTERN_NODE dummyPattern;

   space = (sizeof(BSAVE_OBJECT_ALPHA_NODE) * ObjectReteBinaryData(theEnv,execStatus)->AlphaNodeCount) +
           (sizeof(BSAVE_OBJECT_PATTERN_NODE) * ObjectReteBinaryData(theEnv,execStatus)->PatternNodeCount);
   GenWrite(&space,sizeof(size_t),fp);

   /* ==========================================
      Write out the alpha terminal pattern nodes
      ========================================== */
   alphaPtr = ObjectNetworkTerminalPointer(theEnv,execStatus);
   while (alphaPtr != NULL)
     {
      AssignBsavePatternHeaderValues(theEnv,execStatus,&dummyAlpha.header,&alphaPtr->header);
      dummyAlpha.classbmp = (long) alphaPtr->classbmp->bucket;
      if (alphaPtr->slotbmp != NULL)
        dummyAlpha.slotbmp = (long) alphaPtr->slotbmp->bucket;
      else
        dummyAlpha.slotbmp = -1L;
      dummyAlpha.patternNode = BsaveObjectPatternIndex(alphaPtr->patternNode);
      dummyAlpha.nxtInGroup = BsaveObjectAlphaIndex(alphaPtr->nxtInGroup);
      dummyAlpha.nxtTerminal = BsaveObjectAlphaIndex(alphaPtr->nxtTerminal);
      GenWrite(&dummyAlpha,sizeof(BSAVE_OBJECT_ALPHA_NODE),fp);
      alphaPtr = alphaPtr->nxtTerminal;
     }

   /* ========================================
      Write out the intermediate pattern nodes
      ======================================== */
   patternPtr = ObjectNetworkPointer(theEnv,execStatus);
   while (patternPtr != NULL)
     {
      dummyPattern.multifieldNode = patternPtr->multifieldNode;
      dummyPattern.whichField = patternPtr->whichField;
      dummyPattern.leaveFields = patternPtr->leaveFields;
      dummyPattern.endSlot = patternPtr->endSlot;
      dummyPattern.selector = patternPtr->selector;
      dummyPattern.slotNameID = patternPtr->slotNameID;
      dummyPattern.networkTest = HashedExpressionIndex(theEnv,execStatus,patternPtr->networkTest);
      dummyPattern.nextLevel = BsaveObjectPatternIndex(patternPtr->nextLevel);
      dummyPattern.lastLevel = BsaveObjectPatternIndex(patternPtr->lastLevel);
      dummyPattern.leftNode = BsaveObjectPatternIndex(patternPtr->leftNode);
      dummyPattern.rightNode = BsaveObjectPatternIndex(patternPtr->rightNode);
      dummyPattern.alphaNode = BsaveObjectAlphaIndex(patternPtr->alphaNode);
      GenWrite(&dummyPattern,sizeof(BSAVE_OBJECT_PATTERN_NODE),fp);

      if (patternPtr->nextLevel == NULL)
        {
         while (patternPtr->rightNode == NULL)
           {
            patternPtr = patternPtr->lastLevel;
            if (patternPtr == NULL)
              {
               RestoreBloadCount(theEnv,execStatus,&ObjectReteBinaryData(theEnv,execStatus)->AlphaNodeCount);
               RestoreBloadCount(theEnv,execStatus,&ObjectReteBinaryData(theEnv,execStatus)->PatternNodeCount);
               return;
              }
           }
         patternPtr = patternPtr->rightNode;
        }
      else
        patternPtr = patternPtr->nextLevel;
     }

   RestoreBloadCount(theEnv,execStatus,&ObjectReteBinaryData(theEnv,execStatus)->AlphaNodeCount);
   RestoreBloadCount(theEnv,execStatus,&ObjectReteBinaryData(theEnv,execStatus)->PatternNodeCount);
  }

#endif

/***************************************************
  NAME         : BloadStorageObjectPatterns
  DESCRIPTION  : Reads in the storage requirements
                 for the object patterns in this
                 bload image
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Counts read and arrays allocated
  NOTES        : None
 ***************************************************/
static void BloadStorageObjectPatterns(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;
   long counts[2];

   GenReadBinary(theEnv,execStatus,(void *) &space,sizeof(size_t));
   GenReadBinary(theEnv,execStatus,(void *) counts,space);
   ObjectReteBinaryData(theEnv,execStatus)->AlphaNodeCount = counts[0];
   ObjectReteBinaryData(theEnv,execStatus)->PatternNodeCount = counts[1];

   if (ObjectReteBinaryData(theEnv,execStatus)->AlphaNodeCount == 0L)
     ObjectReteBinaryData(theEnv,execStatus)->AlphaArray = NULL;
   else
     {
      space = (ObjectReteBinaryData(theEnv,execStatus)->AlphaNodeCount * sizeof(OBJECT_ALPHA_NODE));
      ObjectReteBinaryData(theEnv,execStatus)->AlphaArray = (OBJECT_ALPHA_NODE *) genalloc(theEnv,execStatus,space);
     }
   if (ObjectReteBinaryData(theEnv,execStatus)->PatternNodeCount == 0L)
     ObjectReteBinaryData(theEnv,execStatus)->PatternArray = NULL;
   else
     {
      space = (ObjectReteBinaryData(theEnv,execStatus)->PatternNodeCount * sizeof(OBJECT_PATTERN_NODE));
      ObjectReteBinaryData(theEnv,execStatus)->PatternArray = (OBJECT_PATTERN_NODE *) genalloc(theEnv,execStatus,space);
     }
  }

/****************************************************
  NAME         : BloadObjectPatterns
  DESCRIPTION  : Reads in all object pattern
                 data structures from binary
                 image and updates pointers
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Binary data structures updated
  NOTES        : Assumes storage allocated previously
 ****************************************************/
static void BloadObjectPatterns(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;
   long i;

   GenReadBinary(theEnv,execStatus,(void *) &space,sizeof(size_t));
   if (space == 0L)
     return;

   /* ================================================
      Read in the alpha and intermediate pattern nodes
      ================================================ */
   BloadandRefresh(theEnv,execStatus,ObjectReteBinaryData(theEnv,execStatus)->AlphaNodeCount,sizeof(BSAVE_OBJECT_ALPHA_NODE),UpdateAlpha);
   BloadandRefresh(theEnv,execStatus,ObjectReteBinaryData(theEnv,execStatus)->PatternNodeCount,sizeof(BSAVE_OBJECT_PATTERN_NODE),UpdatePattern);

   for (i = 0; i < ObjectReteBinaryData(theEnv,execStatus)->PatternNodeCount; i++)
     {
      if ((ObjectReteBinaryData(theEnv,execStatus)->PatternArray[i].lastLevel != NULL) &&
          (ObjectReteBinaryData(theEnv,execStatus)->PatternArray[i].lastLevel->selector))
        { 
         AddHashedPatternNode(theEnv,execStatus,ObjectReteBinaryData(theEnv,execStatus)->PatternArray[i].lastLevel,
                                     &ObjectReteBinaryData(theEnv,execStatus)->PatternArray[i],
                                     ObjectReteBinaryData(theEnv,execStatus)->PatternArray[i].networkTest->type,
                                     ObjectReteBinaryData(theEnv,execStatus)->PatternArray[i].networkTest->value); 
        }
     }

   /* =======================
      Set the global pointers
      ======================= */
   SetObjectNetworkTerminalPointer(theEnv,execStatus,(OBJECT_ALPHA_NODE *) &ObjectReteBinaryData(theEnv,execStatus)->AlphaArray[0]);
   SetObjectNetworkPointer(theEnv,execStatus,(OBJECT_PATTERN_NODE *) &ObjectReteBinaryData(theEnv,execStatus)->PatternArray[0]);
  }

/***************************************************
  NAME         : UpdateAlpha
  DESCRIPTION  : Updates all the pointers for an
                 alpha node based on the binary
                 image indices
  INPUTS       : 1) A pointer to the binary
                    image alpha node buffer
                 2) The index of the actual
                    alpha node in the array
  RETURNS      : Nothing useful
  SIDE EFFECTS : Alpha node updated
  NOTES        : None
 ***************************************************/
static void UpdateAlpha(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   BSAVE_OBJECT_ALPHA_NODE *bap;
   OBJECT_ALPHA_NODE *ap;

   bap = (BSAVE_OBJECT_ALPHA_NODE *) buf;
   ap = (OBJECT_ALPHA_NODE *) &ObjectReteBinaryData(theEnv,execStatus)->AlphaArray[obji];

   UpdatePatternNodeHeader(theEnv,execStatus,&ap->header,&bap->header);
   ap->matchTimeTag = 0L;
   ap->classbmp = BitMapPointer(bap->classbmp);
   if (bap->slotbmp != -1L)
     {
      ap->slotbmp = BitMapPointer(bap->slotbmp);
      IncrementBitMapCount(ap->slotbmp);
     }
   else
     ap->slotbmp = NULL;
   IncrementBitMapCount(ap->classbmp);
   ap->patternNode = ObjectPatternPointer(bap->patternNode);
   ap->nxtInGroup = ObjectAlphaPointer(bap->nxtInGroup);
   ap->nxtTerminal = ObjectAlphaPointer(bap->nxtTerminal);
   ap->bsaveID = 0L;
  }

/***************************************************
  NAME         : UpdatePattern
  DESCRIPTION  : Updates all the pointers for a
                 pattern node based on the binary
                 image indices
  INPUTS       : 1) A pointer to the binary
                    image pattern node buffer
                 2) The index of the actual
                    pattern node in the array
  RETURNS      : Nothing useful
  SIDE EFFECTS : Pattern node updated
  NOTES        : None
 ***************************************************/
static void UpdatePattern(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   BSAVE_OBJECT_PATTERN_NODE *bop;
   OBJECT_PATTERN_NODE *op;

   bop = (BSAVE_OBJECT_PATTERN_NODE *) buf;
   op = (OBJECT_PATTERN_NODE *) &ObjectReteBinaryData(theEnv,execStatus)->PatternArray[obji];

   op->blocked = FALSE;
   op->multifieldNode = bop->multifieldNode;
   op->whichField = bop->whichField;
   op->leaveFields = bop->leaveFields;
   op->endSlot = bop->endSlot;
   op->selector = bop->selector;
   op->matchTimeTag = 0L;
   op->slotNameID = bop->slotNameID;
   op->networkTest = HashedExpressionPointer(bop->networkTest);
   op->nextLevel = ObjectPatternPointer(bop->nextLevel);
   op->lastLevel = ObjectPatternPointer(bop->lastLevel);
   op->leftNode = ObjectPatternPointer(bop->leftNode);
   op->rightNode = ObjectPatternPointer(bop->rightNode);
   op->alphaNode = ObjectAlphaPointer(bop->alphaNode);
   op->bsaveID = 0L;
  }

/***************************************************
  NAME         : ClearBloadObjectPatterns
  DESCRIPTION  : Releases all emmory associated
                 with binary image object patterns
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Memory released and global
                 network pointers set to NULL
  NOTES        : None
 ***************************************************/
static void ClearBloadObjectPatterns(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;
   register long i;

   for (i = 0; i < ObjectReteBinaryData(theEnv,execStatus)->PatternNodeCount; i++)
     {
      if ((ObjectReteBinaryData(theEnv,execStatus)->PatternArray[i].lastLevel != NULL) &&
          (ObjectReteBinaryData(theEnv,execStatus)->PatternArray[i].lastLevel->selector))
        { 
         RemoveHashedPatternNode(theEnv,execStatus,ObjectReteBinaryData(theEnv,execStatus)->PatternArray[i].lastLevel,
                                        &ObjectReteBinaryData(theEnv,execStatus)->PatternArray[i],
                                        ObjectReteBinaryData(theEnv,execStatus)->PatternArray[i].networkTest->type,
                                        ObjectReteBinaryData(theEnv,execStatus)->PatternArray[i].networkTest->value); 
        }
     }

   /* ================================================
      All instances have been deleted by this point
      so we don't need to worry about clearing partial
      matches
      ================================================ */
   for (i = 0L ; i < ObjectReteBinaryData(theEnv,execStatus)->AlphaNodeCount ; i++)
     {
      DecrementBitMapCount(theEnv,execStatus,ObjectReteBinaryData(theEnv,execStatus)->AlphaArray[i].classbmp);
      if (ObjectReteBinaryData(theEnv,execStatus)->AlphaArray[i].slotbmp != NULL)
        DecrementBitMapCount(theEnv,execStatus,ObjectReteBinaryData(theEnv,execStatus)->AlphaArray[i].slotbmp);
     }

   if (ObjectReteBinaryData(theEnv,execStatus)->AlphaNodeCount != 0L)
     {
      space = (ObjectReteBinaryData(theEnv,execStatus)->AlphaNodeCount * sizeof(OBJECT_ALPHA_NODE));
      genfree(theEnv,execStatus,(void *) ObjectReteBinaryData(theEnv,execStatus)->AlphaArray,space);
      ObjectReteBinaryData(theEnv,execStatus)->AlphaArray = NULL;
      ObjectReteBinaryData(theEnv,execStatus)->AlphaNodeCount = 0;
      space = (ObjectReteBinaryData(theEnv,execStatus)->PatternNodeCount * sizeof(OBJECT_PATTERN_NODE));
      genfree(theEnv,execStatus,(void *) ObjectReteBinaryData(theEnv,execStatus)->PatternArray,space);
      ObjectReteBinaryData(theEnv,execStatus)->PatternArray = NULL;
      ObjectReteBinaryData(theEnv,execStatus)->PatternNodeCount = 0;
     }

   SetObjectNetworkTerminalPointer(theEnv,execStatus,NULL);
   SetObjectNetworkPointer(theEnv,execStatus,NULL);
#if BLOAD_ONLY
   ResetObjectMatchTimeTags(theEnv,execStatus);
#endif
  }

#endif


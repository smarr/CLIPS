   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/09/97          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Binary Load/Save Functions Defrule               */
/*          Object Pattern Network                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "setup.h"

#if INSTANCE_PATTERN_MATCHING && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)

#include "bload.h"
#include "bsave.h"
#include "memalloc.h"
#include "insfun.h"
#include "objrtmch.h"
#include "rulebin.h"

#define _OBJRTBIN_SOURCE_
#include "objrtbin.h"

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
typedef unsigned long UNLN;

typedef struct bsaveObjectPatternNode
  {
   unsigned multifieldNode : 1;
   unsigned endSlot        : 1;
   unsigned whichField     : 8;
   unsigned leaveFields    : 8;
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

#define ObjectPatternPointer(i) ((i == -1L) ? NULL : (OBJECT_PATTERN_NODE *) &PatternArray[i])
#define ObjectAlphaPointer(i)   ((i == -1L) ? NULL : (OBJECT_ALPHA_NODE *) &AlphaArray[i])

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

#if BLOAD_AND_BSAVE
static void BsaveObjectPatternsFind(void);
static void BsaveStorageObjectPatterns(FILE *);
static void BsaveObjectPatterns(FILE *);
#endif
static void BloadStorageObjectPatterns(void);
static void BloadObjectPatterns(void);
static void UpdateAlpha(void *,long);
static void UpdatePattern(void *,long);
static void ClearBloadObjectPatterns(void);

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
Thread static long AlphaNodeCount = 0L,
            PatternNodeCount = 0L;

Thread static OBJECT_ALPHA_NODE *AlphaArray = NULL;
Thread static OBJECT_PATTERN_NODE *PatternArray = NULL;

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***********************************************************
  NAME         : SetupObjectsBload
  DESCRIPTION  : Initializes data structures and
                   routines for binary loads of
                   generic function constructs
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Routines defined and structures initialized
  NOTES        : None
 ***********************************************************/
globle void SetupObjectPatternsBload()
  {
#if BLOAD_AND_BSAVE
   AddBinaryItem("object patterns",0,BsaveObjectPatternsFind,NULL,
                             BsaveStorageObjectPatterns,BsaveObjectPatterns,
                             BloadStorageObjectPatterns,BloadObjectPatterns,
                             ClearBloadObjectPatterns);
#endif
#if BLOAD || BLOAD_ONLY
   AddBinaryItem("object patterns",0,NULL,NULL,NULL,NULL,
                             BloadStorageObjectPatterns,BloadObjectPatterns,
                             ClearBloadObjectPatterns);
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
static void BsaveObjectPatternsFind()
  {
   OBJECT_ALPHA_NODE *alphaPtr;
   OBJECT_PATTERN_NODE *patternPtr;

   if (Bloaded())
     {
      SaveBloadCount(AlphaNodeCount);
      SaveBloadCount(PatternNodeCount);
     }
   AlphaNodeCount = 0L;
   alphaPtr = ObjectNetworkTerminalPointer();
   while (alphaPtr != NULL)
     {
      alphaPtr->classbmp->neededBitMap = TRUE;
      if (alphaPtr->slotbmp != NULL)
        alphaPtr->slotbmp->neededBitMap = TRUE;
      alphaPtr->bsaveID = AlphaNodeCount++;
      alphaPtr = alphaPtr->nxtTerminal;
     }

   PatternNodeCount = 0L;
   patternPtr = ObjectNetworkPointer();
   while (patternPtr != NULL)
     {
      patternPtr->bsaveID = PatternNodeCount++;
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
  FILE *fp)
  {
   UNLN space;

   space = sizeof(long) * 2;
   GenWrite(&space,(UNLN) sizeof(UNLN),fp);
   GenWrite(&AlphaNodeCount,(UNLN) sizeof(long),fp);
   GenWrite(&PatternNodeCount,(UNLN) sizeof(long),fp);
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
  FILE *fp)
  {
   UNLN space;
   OBJECT_ALPHA_NODE *alphaPtr;
   OBJECT_PATTERN_NODE *patternPtr;
   BSAVE_OBJECT_ALPHA_NODE dummyAlpha;
   BSAVE_OBJECT_PATTERN_NODE dummyPattern;

   space = (sizeof(BSAVE_OBJECT_ALPHA_NODE) * AlphaNodeCount) +
           (sizeof(BSAVE_OBJECT_PATTERN_NODE) * PatternNodeCount);
   GenWrite(&space,(UNLN) sizeof(UNLN),fp);

   /* ==========================================
      Write out the alpha terminal pattern nodes
      ========================================== */
   alphaPtr = ObjectNetworkTerminalPointer();
   while (alphaPtr != NULL)
     {
      AssignBsavePatternHeaderValues(&dummyAlpha.header,&alphaPtr->header);
      dummyAlpha.classbmp = alphaPtr->classbmp->bucket;
      if (alphaPtr->slotbmp != NULL)
        dummyAlpha.slotbmp = alphaPtr->slotbmp->bucket;
      else
        dummyAlpha.slotbmp = -1L;
      dummyAlpha.patternNode = BsaveObjectPatternIndex(alphaPtr->patternNode);
      dummyAlpha.nxtInGroup = BsaveObjectAlphaIndex(alphaPtr->nxtInGroup);
      dummyAlpha.nxtTerminal = BsaveObjectAlphaIndex(alphaPtr->nxtTerminal);
      GenWrite(&dummyAlpha,(UNLN) sizeof(BSAVE_OBJECT_ALPHA_NODE),fp);
      alphaPtr = alphaPtr->nxtTerminal;
     }

   /* ========================================
      Write out the intermediate pattern nodes
      ======================================== */
   patternPtr = ObjectNetworkPointer();
   while (patternPtr != NULL)
     {
      dummyPattern.multifieldNode = patternPtr->multifieldNode;
      dummyPattern.whichField = patternPtr->whichField;
      dummyPattern.leaveFields = patternPtr->leaveFields;
      dummyPattern.endSlot = patternPtr->endSlot;
      dummyPattern.slotNameID = patternPtr->slotNameID;
      dummyPattern.networkTest = HashedExpressionIndex(patternPtr->networkTest);
      dummyPattern.nextLevel = BsaveObjectPatternIndex(patternPtr->nextLevel);
      dummyPattern.lastLevel = BsaveObjectPatternIndex(patternPtr->lastLevel);
      dummyPattern.leftNode = BsaveObjectPatternIndex(patternPtr->leftNode);
      dummyPattern.rightNode = BsaveObjectPatternIndex(patternPtr->rightNode);
      dummyPattern.alphaNode = BsaveObjectAlphaIndex(patternPtr->alphaNode);
      GenWrite(&dummyPattern,(UNLN) sizeof(BSAVE_OBJECT_PATTERN_NODE),fp);

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

   if (Bloaded())
     {
      RestoreBloadCount(&AlphaNodeCount);
      RestoreBloadCount(&PatternNodeCount);
     }
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
static void BloadStorageObjectPatterns()
  {
   UNLN space;
   long counts[2];

   GenRead((void *) &space,(UNLN) sizeof(UNLN));
   GenRead((void *) counts,space);
   AlphaNodeCount = counts[0];
   PatternNodeCount = counts[1];

   if (AlphaNodeCount == 0L)
     AlphaArray = NULL;
   else
     {
      space = (UNLN) (AlphaNodeCount * sizeof(OBJECT_ALPHA_NODE));
      AlphaArray = (OBJECT_ALPHA_NODE *) genlongalloc(space);
     }
   if (PatternNodeCount == 0L)
     PatternArray = NULL;
   else
     {
      space = (UNLN) (PatternNodeCount * sizeof(OBJECT_PATTERN_NODE));
      PatternArray = (OBJECT_PATTERN_NODE *) genlongalloc(space);
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
static void BloadObjectPatterns()
  {
   UNLN space;

   GenRead((void *) &space,(UNLN) sizeof(UNLN));
   if (space == 0L)
     return;

   /* ================================================
      Read in the alpha and intermediate pattern nodes
      ================================================ */
   BloadandRefresh(AlphaNodeCount,(unsigned) sizeof(BSAVE_OBJECT_ALPHA_NODE),UpdateAlpha);
   BloadandRefresh(PatternNodeCount,(unsigned) sizeof(BSAVE_OBJECT_PATTERN_NODE),UpdatePattern);

   /* =======================
      Set the global pointers
      ======================= */
   SetObjectNetworkTerminalPointer((OBJECT_ALPHA_NODE *) &AlphaArray[0]);
   SetObjectNetworkPointer((OBJECT_PATTERN_NODE *) &PatternArray[0]);
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
  void *buf,
  long obji)
  {
   BSAVE_OBJECT_ALPHA_NODE *bap;
   OBJECT_ALPHA_NODE *ap;

   bap = (BSAVE_OBJECT_ALPHA_NODE *) buf;
   ap = (OBJECT_ALPHA_NODE *) &AlphaArray[obji];

   UpdatePatternNodeHeader(&ap->header,&bap->header);
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
  void *buf,
  long obji)
  {
   BSAVE_OBJECT_PATTERN_NODE *bop;
   OBJECT_PATTERN_NODE *op;

   bop = (BSAVE_OBJECT_PATTERN_NODE *) buf;
   op = (OBJECT_PATTERN_NODE *) &PatternArray[obji];

   op->blocked = FALSE;
   op->multifieldNode = bop->multifieldNode;
   op->whichField = bop->whichField;
   op->leaveFields = bop->leaveFields;
   op->endSlot = bop->endSlot;
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
static void ClearBloadObjectPatterns()
  {
   UNLN space;
   register long i;

   /* ================================================
      All instances have been deleted by this point
      so we don't need to worry about clearing partial
      matches
      ================================================ */
   for (i = 0L ; i < AlphaNodeCount ; i++)
     {
      DecrementBitMapCount(AlphaArray[i].classbmp);
      if (AlphaArray[i].slotbmp != NULL)
        DecrementBitMapCount(AlphaArray[i].slotbmp);
     }

   if (AlphaNodeCount != 0L)
     {
      space = (UNLN) (AlphaNodeCount * sizeof(OBJECT_ALPHA_NODE));
      genlongfree((void *) AlphaArray,space);
      AlphaArray = NULL;
      space = (UNLN) (PatternNodeCount * sizeof(OBJECT_PATTERN_NODE));
      genlongfree((void *) PatternArray,space);
      PatternArray = NULL;
     }

   SetObjectNetworkTerminalPointer(NULL);
   SetObjectNetworkPointer(NULL);
#if BLOAD_ONLY
   ResetObjectMatchTimeTags();
#endif
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


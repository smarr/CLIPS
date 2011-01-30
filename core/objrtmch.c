   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/09/97          */
   /*                                                     */
   /*          OBJECT PATTERN MATCHER MODULE              */
   /*******************************************************/

/**************************************************************/
/* Purpose: RETE Network Interface for Objects                */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Brian L. Donnell                                      */
/*                                                            */
/* Contributing Programmer(s):                                */
/*                                                            */
/* Revision History:                                          */
/*                                                            */
/* Who               |     Date    | Description              */
/* ------------------+-------------+------------------------  */
/*      MDT          | 23-Nov-1998 |        DR829             */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS        */
/**************************************************************/
/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "setup.h"

#if INSTANCE_PATTERN_MATCHING

#include "classfun.h"
#include "memalloc.h"
#include "drive.h"
#include "engine.h"
#include "multifld.h"

#if LOGICAL_DEPENDENCIES
#include "lgcldpnd.h"
#endif

#if INCREMENTAL_RESET && (! RUN_TIME) && (! BLOAD_ONLY)
#include "incrrset.h"
#endif

#include "reteutil.h"
#include "ruledlt.h"
#include "reorder.h"
#include "retract.h"
#include "router.h"

#include "objrtfnx.h"

#define _OBJRTMCH_SOURCE_
#include "objrtmch.h"

#include "insmngr.h"

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
typedef struct objectMatchAction
  {
   int type;
   INSTANCE_TYPE *ins;
   SLOT_BITMAP *slotNameIDs;
   struct objectMatchAction *nxt;
  } OBJECT_MATCH_ACTION;

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

static void ResetObjectMatchTimeTags(void);
static void QueueObjectMatchAction(int,INSTANCE_TYPE *,int);
static SLOT_BITMAP *QueueModifySlotMap(SLOT_BITMAP *,int);
static void ReturnObjectMatchAction(OBJECT_MATCH_ACTION *);
static void ProcessObjectMatchQueue(void);
static void MarkObjectPatternNetwork(SLOT_BITMAP *);
static BOOLEAN CompareSlotBitMaps(SLOT_BITMAP *,SLOT_BITMAP *);
static void ObjectPatternMatch(int,OBJECT_PATTERN_NODE *,struct multifieldMarker *);
static void ProcessPatternNode(int,OBJECT_PATTERN_NODE *,struct multifieldMarker *);
static void CreateObjectAlphaMatch(OBJECT_ALPHA_NODE *);
static BOOLEAN EvaluateObjectPatternTest(int,struct multifieldMarker *,EXPRESSION *,
                                         OBJECT_PATTERN_NODE *);
static void ObjectAssertAction(INSTANCE_TYPE *);
static void ObjectModifyAction(INSTANCE_TYPE *,SLOT_BITMAP *);
static void ObjectRetractAction(INSTANCE_TYPE *,SLOT_BITMAP *);
static void ObjectPatternNetErrorMessage(OBJECT_PATTERN_NODE *);
static void TraceErrorToObjectPattern(int,OBJECT_PATTERN_NODE *);

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
Thread static OBJECT_MATCH_ACTION *ObjectMatchActionQueue = NULL;
Thread static OBJECT_PATTERN_NODE *ObjectPatternNetworkPointer = NULL;
Thread static OBJECT_ALPHA_NODE *ObjectPatternNetworkTerminalPointer = NULL;
Thread static BOOLEAN DelayObjectPatternMatching = FALSE;
Thread static unsigned long CurrentObjectMatchTimeTag = 0L;
Thread static long UseEntityTimeTag = 0L;

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***************************************************************************
  NAME         : ObjectMatchDelay
  DESCRIPTION  : H/L interface for SetDelayObjectPatternMatching
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : DelayObjectPatternMatching set and Rete network updates
                 delayed until pattern-matching is completed
  NOTES        : H/L Syntax: (object-pattern-match-delay <action>*)
 ***************************************************************************/
globle void ObjectMatchDelay(
  DATA_OBJECT *result)
  {
   register int ov;

   ov = SetDelayObjectPatternMatching(TRUE);
   EvaluateExpression(GetFirstArgument(),result);
   if (EvaluationError)
     {
      SetHaltExecution(FALSE);
      SetEvaluationError(FALSE);
      SetDelayObjectPatternMatching(ov);
      SetEvaluationError(TRUE);
     }
   else
     SetDelayObjectPatternMatching(ov);
  }

/***************************************************
  NAME         : SetDelayObjectPatternMatching
  DESCRIPTION  : Sets the flag determining if Rete
                 network activity is to be delayed
                 for objects or not
  INPUTS       : The value of the flag
  RETURNS      : The old value of the flag
  SIDE EFFECTS : DelayObjectPatternMatching set
  NOTES        : When the delay is set to FALSE,
                 all pending Rete network updates
                 are performed
 ***************************************************/
globle BOOLEAN SetDelayObjectPatternMatching(
  int value)
  {
   BOOLEAN oldval;

   oldval = DelayObjectPatternMatching;
   if (value)
     DelayObjectPatternMatching = TRUE;
   else
     {
      DelayObjectPatternMatching = FALSE;
      ObjectNetworkAction(0,NULL,-1);
     }
   return(oldval);
  }

/***************************************************
  NAME         : GetDelayObjectPatternMatching
  DESCRIPTION  : Gets the flag determining if Rete
                 network activity is to be delayed
                 for objects or not
  INPUTS       : None
  RETURNS      : The flag
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle BOOLEAN GetDelayObjectPatternMatching()
  {
   return(DelayObjectPatternMatching);
  }

/********************************************************
  NAME         : ObjectNetworkPointer
  DESCRIPTION  : Returns the first object network
                 pattern node
  INPUTS       : None
  RETURNS      : The top of the object pattern network
  SIDE EFFECTS : None
  NOTES        : None
 ********************************************************/
globle OBJECT_PATTERN_NODE *ObjectNetworkPointer()
  {
   return(ObjectPatternNetworkPointer);
  }

/********************************************************
  NAME         : ObjectNetworkTerminalPointer
  DESCRIPTION  : Returns the first terminal pattern node
  INPUTS       : None
  RETURNS      : The last node of a pattern
  SIDE EFFECTS : None
  NOTES        : None
 ********************************************************/
globle OBJECT_ALPHA_NODE *ObjectNetworkTerminalPointer()
  {
   return(ObjectPatternNetworkTerminalPointer);
  }

/***************************************************
  NAME         : SetObjectNetworkPointer
  DESCRIPTION  : Sets the object pattern network to
                  the given network
  INPUTS       : Top of the new pattern network
  RETURNS      : Nothing useful
  SIDE EFFECTS : ObjectPatternNetworkPointer set
  NOTES        : None
 ***************************************************/
globle void SetObjectNetworkPointer(
  OBJECT_PATTERN_NODE *value)
  {
   ObjectPatternNetworkPointer = value;
  }

/*******************************************************
  NAME         : SetObjectNetworkTerminalPointer
  DESCRIPTION  : Sets the global list of terminal
                 pattern nodes (the ones containing
                 the bitmaps) to the given node
  INPUTS       : The last node of a pattern
  RETURNS      : Nothing useful
  SIDE EFFECTS : ObjectPatternNetworkTerminalPointer set
  NOTES        : None
 *******************************************************/
globle void SetObjectNetworkTerminalPointer(
  OBJECT_ALPHA_NODE *value)
  {
   ObjectPatternNetworkTerminalPointer = value;
  }

/************************************************************************
  NAME         : ObjectNetworkAction
  DESCRIPTION  : Main driver for pattern-matching on objects
                 If the pattern-matching is current delayed or another
                 object is currently being pattern-matched, the requested
                 match action is queued for later processing.
                 Otherwise, the match action is performed and the
                 Rete network is updated.
  INPUTS       : 1) The match action type
                    OBJECT_ASSERT  (1)
                    OBJECT_RETRACT (2)
                    OBJECT_MODIFY  (3)
                 2) The instance to be matched (can be NULL if only
                    want pending actions to be performed)
                 3) The name id of the slot being updated (can be -1)
                    If this argument is -1, it is assumed that any
                    pattern which could match this instance must be
                    checked.  Otherwise, only the patterns which
                    explicitly match on the named slot will be checked.
  RETURNS      : Nothing useful
  SIDE EFFECTS : Action queued or Rete network updated
  NOTES        : None
 ************************************************************************/
globle void ObjectNetworkAction(
  int type,
  INSTANCE_TYPE *ins,
  int slotNameID)
  {
   SLOT_BITMAP *tmpMap;

   if (JoinOperationInProgress)
     return;

   JoinOperationInProgress = TRUE;


   /* ================================================
      For purposes of conflict resolution, all objects
      which have had pattern-matching delayed will
      have the same relative timestamp, i.e., the
      inference engine thinks they all just appeared
      simultaneously

      When delay is off, however, each object gets the
      new and current timestamp as expected.
      ================================================ */
   UseEntityTimeTag = CurrentEntityTimeTag++;

   /* ==================================================
      If pattern-matching is delayed (by use of the
      set-object-pattern-match-delay function), then
      the instance should be marked for later processing
      (when the delay is turned off).
      ================================================== */
   if (ins != NULL)
     {
      /* 6.05 Bug Fix */
      ins->reteSynchronized = FALSE;

      if (DelayObjectPatternMatching == FALSE)
        switch (type)
        {
         case OBJECT_ASSERT  :
           ObjectAssertAction(ins);
           break;
         case OBJECT_RETRACT :
           ObjectRetractAction(ins,NULL);
           break;
         default             :
           tmpMap = QueueModifySlotMap(NULL,slotNameID);
           ObjectModifyAction(ins,tmpMap);
           rm((void *) tmpMap,SlotBitMapSize(tmpMap));
        }
      else
        QueueObjectMatchAction(type,ins,slotNameID);
     }

   /* ========================================
      Process all pending actions in the queue
      All updates will use the same timestamp
      ======================================== */
   ProcessObjectMatchQueue();

   JoinOperationInProgress = FALSE;

#if LOGICAL_DEPENDENCIES
   ForceLogicalRetractions();
#endif
   /*=========================================*/
   /* Free partial matches that were released */
   /* by the assertion of the fact.           */
   /*=========================================*/

   if (ExecutingRule == NULL) FlushGarbagePartialMatches();
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***************************************************
  NAME         : ResetObjectMatchTimeTags
  DESCRIPTION  : If CurrentObjectMatchTimeTag + 1
                 would cause an overflow,
                 CurrentObjectMatchTimeTag
                 is reset to 0L and all time tags
                 in object pattern nodes are reset.
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : CurrentObjectMatchTimeTag reset to
                 0, and all match time tags reset
                 These tags are used to recognize
                 valid pattern nodes on a match
  NOTES        : None
 ***************************************************/
static void ResetObjectMatchTimeTags()
  {
   OBJECT_ALPHA_NODE *alphaPtr;
   OBJECT_PATTERN_NODE *lastLevel;

   /* ============================================
      If the current tag incremented by one would
      not cause an overflow, then we can leave
      things alone.
      ============================================ */
   if ((CurrentObjectMatchTimeTag + 1L) > CurrentObjectMatchTimeTag)
     return;
   CurrentObjectMatchTimeTag = 0L;
   alphaPtr = ObjectNetworkTerminalPointer();
   while (alphaPtr != NULL)
     {
      alphaPtr->matchTimeTag = 0L;
      lastLevel = alphaPtr->patternNode;
      while (lastLevel != NULL)
        {
         if (lastLevel->matchTimeTag == 0L)
           break;
         lastLevel->matchTimeTag = 0L;
         lastLevel = lastLevel->lastLevel;
        }
      alphaPtr = alphaPtr->nxtTerminal;
     }
  }

/***************************************************
  NAME         : QueueObjectMatchAction
  DESCRIPTION  : Posts a Rete network match event
                 for later processing
  INPUTS       : 1) The match action type
                    OBJECT_ASSERT  (1)
                    OBJECT_RETRACT (2)
                    OBJECT_MODIFY  (3)
                 2) The instance to be matched
                 3) The name id of the slot being
                    updated (can be -1)
  RETURNS      : Nothing useful
  SIDE EFFECTS : Queue updated
  NOTES        : None
 ***************************************************/
static void QueueObjectMatchAction(
  int type,
  INSTANCE_TYPE *ins,
  int slotNameID)
  {
   OBJECT_MATCH_ACTION *prv,*cur,*newMatch;

   prv = NULL;
   cur = ObjectMatchActionQueue;
   while (cur != NULL)
     {
      /* ===========================================================
         Here are the possibilities for the first Rete event already
         on the queue as compared with the new event for an object:

         Assert/Retract  -->  Delete assert event
                              Ignore retract event
         Assert/Modify   -->  Ignore modify event
         Modify/Modify   -->  Merge new modify event
         Modify/Retract  -->  Delete modify event
                              Queue the retract event
         =========================================================== */
      if (cur->ins == ins)
        {
         /* ===================================================
            An action for initially asserting the newly created
            object to all applicable patterns
            =================================================== */
         if (cur->type == OBJECT_ASSERT)
           {
            if (type == OBJECT_RETRACT)
              {
               /* ===================================================
                  If we are retracting the entire object, then we can
                  remove the assert action (and all modifies as well)
                  and ignore the retract action
                  (basically the object came and went before the Rete
                  network had a chance to see it)
                  =================================================== */
               if (prv == NULL)
                 ObjectMatchActionQueue = cur->nxt;
               else
                 prv->nxt = cur->nxt;
               cur->ins->busy--;
               ReturnObjectMatchAction(cur);
              }

            /* =================================================
               If this is a modify action, then we can ignore it
               since the assert action will encompass it
               ================================================= */
           }

         /* ===================================================
            If the object is being deleted after a slot modify,
            drop the modify event and replace with the retract
            =================================================== */
         else if (type == OBJECT_RETRACT)
           {
            cur->type = OBJECT_RETRACT;
            if (cur->slotNameIDs != NULL)
              {
               rm((void *) cur->slotNameIDs,SlotBitMapSize(cur->slotNameIDs));
               cur->slotNameIDs = NULL;
              }
           }

         /* ====================================================
            If a modify event for this slot is already on the
            queue, ignore this one. Otherwise, merge the slot id
            ==================================================== */
         else
            cur->slotNameIDs = QueueModifySlotMap(cur->slotNameIDs,slotNameID);

         return;
        }
      prv = cur;
      cur = cur->nxt;
     }

   /* ================================================
      If there are no actions for the instance already
      on the queue, the new action is simply appended.
      ================================================ */
   newMatch = get_struct(objectMatchAction);
   newMatch->type = type;
   newMatch->nxt = cur;
   newMatch->slotNameIDs = (type != OBJECT_MODIFY) ? NULL :
                       QueueModifySlotMap(NULL,slotNameID);
   newMatch->ins = ins;
   newMatch->ins->busy++;
   if (prv == NULL)
     ObjectMatchActionQueue = newMatch;
   else
     prv->nxt = newMatch;
  }

/****************************************************
  NAME         : QueueModifySlotMap
  DESCRIPTION  : Sets the bitmap for a queued
                 object modify Rete network action
  INPUTS       : 1) The old bitmap (can be NULL)
                 2) The canonical slot id to set
  RETURNS      : The (new) bitmap
  SIDE EFFECTS : Bitmap allocated/reallocated if
                 necessary, and slot id bit set
  NOTES        : If the bitmap must be (re)allocated,
                 this routine allocates twice the
                 room necessary for the current id
                 to allow for growth.
 ****************************************************/
static SLOT_BITMAP *QueueModifySlotMap(
  SLOT_BITMAP *oldMap,
  int slotNameID)
  {
   SLOT_BITMAP *newMap;
   unsigned short newmaxid;
   int oldsz,newsz;

   if ((oldMap == NULL) ? TRUE : (slotNameID > oldMap->maxid))
     {
      newmaxid = (unsigned short) (slotNameID * 2);
      newsz = (int) (sizeof(SLOT_BITMAP) +
                     (sizeof(char) * (newmaxid / BITS_PER_BYTE)));
      newMap = (SLOT_BITMAP *) gm2(newsz);
      ClearBitString((void *) newMap,newsz);
      if (oldMap != NULL)
        {
         oldsz = SlotBitMapSize(oldMap);
         GenCopyMemory(char,oldsz,newMap,oldMap);
         rm((void *) oldMap,oldsz);
        }
      newMap->maxid = newmaxid;
     }
   else
     newMap = oldMap;
   SetBitMap(newMap->map,slotNameID);
   return(newMap);
  }

/***************************************************
  NAME         : ReturnObjectMatchAction
  DESCRIPTION  : Deallocates and object match action
                 structure and associated slot
                 bitmap (if any)
  INPUTS       : The queued match action item
  RETURNS      : Nothing useful
  SIDE EFFECTS : Object match action item deleted
  NOTES        : None
 ***************************************************/
static void ReturnObjectMatchAction(
  OBJECT_MATCH_ACTION *omaPtr)
  {
   if (omaPtr->slotNameIDs != NULL)
     rm((void *) omaPtr->slotNameIDs,SlotBitMapSize(omaPtr->slotNameIDs));
   rtn_struct(objectMatchAction,omaPtr);
  }

/***************************************************
  NAME         : ProcessObjectMatchQueue
  DESCRIPTION  : Processes all outstanding object
                 Rete network update events
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Pattern-matching on objects
  NOTES        : None
 ***************************************************/
static void ProcessObjectMatchQueue()
  {
   OBJECT_MATCH_ACTION *cur;

   while ((ObjectMatchActionQueue != NULL) &&
          (DelayObjectPatternMatching == FALSE))
     {
      cur = ObjectMatchActionQueue;
      ObjectMatchActionQueue = cur->nxt;

      switch(cur->type)
        {
         case OBJECT_ASSERT  :
           ObjectAssertAction(cur->ins);
           break;
         case OBJECT_RETRACT :
           ObjectRetractAction(cur->ins,cur->slotNameIDs);
           break;
         default             :
           ObjectModifyAction(cur->ins,cur->slotNameIDs);
        }
      cur->ins->busy--;
      ReturnObjectMatchAction(cur);
     }
  }

/******************************************************
  NAME         : MarkObjectPatternNetwork
  DESCRIPTION  : Iterates through all terminal
                 pattern nodes checking class and
                 slot bitmaps.  If a pattern is
                 applicable to the object/slot change,
                 then all the nodes belonging to
                 the pattern are marked as needing
                 to be examined by the pattern matcher.
  INPUTS       : The bitmap of ids of the slots being
                 changed (NULL if this is an assert for the
                  for the entire object)
  RETURNS      : Nothing useful
  SIDE EFFECTS : Applicable pattern nodes marked
  NOTES        : Incremental reset status is also
                 checked here
 ******************************************************/
static void MarkObjectPatternNetwork(
  SLOT_BITMAP *slotNameIDs)
  {
   OBJECT_ALPHA_NODE *alphaPtr;
   OBJECT_PATTERN_NODE *upper;
   CLASS_BITMAP *clsset;
   unsigned id;

   ResetObjectMatchTimeTags();
   CurrentObjectMatchTimeTag++;
   alphaPtr = ObjectNetworkTerminalPointer();
   id = CurrentPatternObject->cls->id;
   while (alphaPtr != NULL)
     {
      /* =============================================================
         If an incremental reset is in progress, make sure that the
         pattern has been marked for initialization before proceeding.
         ============================================================= */
#if INCREMENTAL_RESET && (! RUN_TIME) && (! BLOAD_ONLY)
      if (IncrementalResetInProgress &&
          (alphaPtr->header.initialize == FALSE))
        {
         alphaPtr = alphaPtr->nxtTerminal;
         continue;
        }
#endif

      /* ============================================
         Check the class bitmap to see if the pattern
         pattern is applicable to the object at all
         ============================================ */
      clsset = (CLASS_BITMAP *) ValueToBitMap(alphaPtr->classbmp);

      if ((id > (unsigned) clsset->maxid) ? FALSE : TestBitMap(clsset->map,id))
        {
         /* ===================================================
            If we are doing an assert, then we need to
            check all patterns which satsify the class bitmap
            (The retraction has already been done in this case)
            =================================================== */
         if (slotNameIDs == NULL)
           {
            alphaPtr->matchTimeTag = CurrentObjectMatchTimeTag;
            for (upper = alphaPtr->patternNode ; upper != NULL ; upper = upper->lastLevel)
              {
               if (upper->matchTimeTag == CurrentObjectMatchTimeTag)
                 break;
               else
                 upper->matchTimeTag = CurrentObjectMatchTimeTag;
              }
           }

         /* ===================================================
            If we are doing a slot modify, then we need to
            check only the subset of patterns which satisfy the
            class bitmap AND actually match on the slot in
            question.
            =================================================== */
         else if (alphaPtr->slotbmp != NULL)
           {
           if (CompareSlotBitMaps(slotNameIDs,
                  (SLOT_BITMAP *) ValueToBitMap(alphaPtr->slotbmp)))
              {
               alphaPtr->matchTimeTag = CurrentObjectMatchTimeTag;
               for (upper = alphaPtr->patternNode ; upper != NULL ; upper = upper->lastLevel)
                 {
                  if (upper->matchTimeTag == CurrentObjectMatchTimeTag)
                    break;
                  else
                    upper->matchTimeTag = CurrentObjectMatchTimeTag;
                 }
              }
           }
        }
      alphaPtr = alphaPtr->nxtTerminal;
     }
  }

/***************************************************
  NAME         : CompareSlotBitMaps
  DESCRIPTION  : Compares two slot bitmaps by
                 bitwising and'ing byte per byte up
                 to the length of the smaller map.
  INPUTS       : The two slot bitmaps
  RETURNS      : TRUE if any common bits
                 are set in both maps, FALSE
                 otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
static BOOLEAN CompareSlotBitMaps(
  SLOT_BITMAP *smap1,
  SLOT_BITMAP *smap2)
  {
   unsigned short i,maxByte;

   maxByte = (unsigned short)
             (((smap1->maxid < smap2->maxid) ?
              smap1->maxid : smap2->maxid) / BITS_PER_BYTE);
   for (i = 0 ; i <= maxByte ; i++)
     if (smap1->map[i] & smap2->map[i])
       return(TRUE);
   return(FALSE);
  }

/**********************************************************************************
  NAME         : ObjectPatternMatch
  DESCRIPTION  : Iterates through all the pattern nodes on one level
                 in the pattern network.  A node is only processed
                 if it can lead to a terminating class bitmap node
                 which applies to the object being matched.  This
                 allows for a significant reduction in the number of
                 patterns considered.
  INPUTS       : 1) The offset of the slot position from the pattern index
                 2) The pattern node being examined
                 3) The end of the list of multifield markers for the pattern
  RETURNS      : Nothing useful
  SIDE EFFECTS : The pattern tests are evaluated and the child nodes may
                 be processed (which may cause a whole series of Rete network
                 updates).
  NOTES        : Several globals are used to keep track of the current
                 slot being examined:
                 CurrentPatternMarks - the series of multifield markers
                 CurrentPatternObject - the object being pattern-matched
                 CurrentPatternObjectSlot - the current slot being examined
                 CurrentObjectSlotLength - the cardinality of the slot value

                 An optimization is performed when evaluating
                 constant tests on a slot value field.  All
                 pattern nodes on a level which restrict the same slot
                 are grouped together.  Those which are constant
                 tests are placed at the far right.  Thus, as soon
                 as one of these constant tests succeeds, the remaining
                 nodes for that slot on this level can be skipped
 **********************************************************************************/
static void ObjectPatternMatch(
  int offset,
  OBJECT_PATTERN_NODE *patternTop,
  struct multifieldMarker *endMark)
  {
   register int saveSlotLength;
   register INSTANCE_SLOT *saveSlot;
   OBJECT_PATTERN_NODE *blockedNode;

   while (patternTop != NULL)
     {
      /* ===========================================================
         MarkObjectPatternNetwork() has already marked pattern nodes
         which need processing according to the class bitmaps,
         slot updates and incremental reset status
         =========================================================== */
      if (patternTop->matchTimeTag == CurrentObjectMatchTimeTag)
        {
         /* ========================================
            Make sure we are examining the correct
            slot of the object for this pattern node
            ======================================== */
         if ((patternTop->slotNameID == ISA_ID) ||
             (patternTop->slotNameID == NAME_ID))
           {
            CurrentPatternObjectSlot = NULL;
            CurrentObjectSlotLength = 1;
            offset = 0;
           }
         else if ((CurrentPatternObjectSlot == NULL) ? TRUE :
                  (CurrentPatternObjectSlot->desc->slotName->id != patternTop->slotNameID))
           {
            /* ====================================================
               Need to reset the indices for the multifield
               markers now that we have moved onto a different slot
                  ==================================================== */
            CurrentPatternObjectSlot =
             CurrentPatternObject->slotAddresses[CurrentPatternObject->cls->slotNameMap
                                             [patternTop->slotNameID] - 1];
            offset = 0;
            if (CurrentPatternObjectSlot->desc->multiple)
              CurrentObjectSlotLength =
                GetInstanceSlotLength(CurrentPatternObjectSlot);
            else
              CurrentObjectSlotLength = 1;
           }

         /* ========================================================
            Process the pattern node.  If it is satisfied by the
            the instance, ProcessPatternNode() will recursively pass
            all of its children nodes through ObjectPatternMatch()
            ======================================================== */
         saveSlotLength = CurrentObjectSlotLength;
         saveSlot = CurrentPatternObjectSlot;
         ProcessPatternNode(offset,patternTop,endMark);
         CurrentObjectSlotLength = saveSlotLength;
         CurrentPatternObjectSlot = saveSlot;
        }

      /* ==============================================================
         Move on to the siblings of this node - if the current node was
         a constant test that succeeded, skip further sibling nodes
         (which test on the same field in the pattern)
         which match on the same slot since they are all constant tests
         as well and will, of course fail.
         ============================================================== */
      if (patternTop->blocked == TRUE)
        {
         patternTop->blocked = FALSE;
         blockedNode = patternTop;
         patternTop = patternTop->rightNode;
         while (patternTop != NULL)
           {
            if ((patternTop->slotNameID != blockedNode->slotNameID) ||
                (patternTop->whichField != blockedNode->whichField))
              break;
            patternTop = patternTop->rightNode;
           }
        }
      else
        patternTop = patternTop->rightNode;
     }
  }

/**********************************************************************************
  NAME         : ProcessPatternNode
  DESCRIPTION  : Determines if a pattern node satsifies the corresponding
                 slot value field(s) in an object.  If it does,
                 ObjectPatternMatch() is recursively called to process
                 the child nodes of this node.  In this mutual recursion
                 between ObjectPatternMatch() and ProcessPatternNode(),
                 the nodes of all applicable patterns are processed
                 to completion.  ObjectPatternMatch() enters an object
                 into a pattern's aplha memory when the traversal reaches
                 a terminal class bitmap node.
  INPUTS       : 1) The offset of the slot index from the pattern index
                 2) The pattern node being examined
                 3) The end of the list of multifield markers for the pattern
  RETURNS      : Nothing useful
  SIDE EFFECTS : The pattern tests are evaluated and the child nodes may
                 be processed (which may cause a whole series of Rete network
                 updates).
  NOTES        : Several globals are used to keep track of the current
                 slot being examined:
                 CurrentPatternMarks - the series of multifield markers
                 CurrentPatternObject - the object being pattern-matched
                 CurrentPatternObjectSlot - the current slot being examined
                 CurrentObjectSlotLength - the cardinality of the slot value
 **********************************************************************************/
static void ProcessPatternNode(
  int offset,
  OBJECT_PATTERN_NODE *patternNode,
  struct multifieldMarker *endMark)
  {
   int patternSlotField,objectSlotField,
       objectSlotLength,
       repeatCount;
   INSTANCE_SLOT *objectSlot;
   struct multifieldMarker *newMark;

   patternSlotField = patternNode->whichField;
   objectSlotField = patternSlotField + offset;

   /* ==========================================
      If this is a test on the class or the name
      of the object, process it separately.
      ========================================== */
   if (CurrentPatternObjectSlot == NULL)
     {
      if ((patternNode->networkTest == NULL) ? TRUE :
          (EvaluateObjectPatternTest(objectSlotField,NULL,
                                     (EXPRESSION *) patternNode->networkTest,patternNode)))
        {
         if (patternNode->alphaNode != NULL)
           CreateObjectAlphaMatch(patternNode->alphaNode);
         ObjectPatternMatch(offset,patternNode->nextLevel,endMark);
        }
      return;
     }

   /* ================================
      Check a single-field restriction
      ================================ */
   if (patternNode->multifieldNode == 0)
     {
      if ((patternNode->networkTest == NULL) ? TRUE :
          EvaluateObjectPatternTest(objectSlotField,NULL,
                                    (EXPRESSION *) patternNode->networkTest,patternNode))
        {
         if (patternNode->alphaNode != NULL)
           CreateObjectAlphaMatch(patternNode->alphaNode);
         ObjectPatternMatch(offset,patternNode->nextLevel,endMark);
        }
      return;
     }

   /* ==================================================================
      Check a multifield restriction.  Add a marker for this field which
      has indices indicating to which values in the object slot the
      multifield pattern node is bound
      ================================================================== */
   newMark = get_struct(multifieldMarker);
   newMark->whichField = patternSlotField;
   newMark->where.whichSlot = (void *) CurrentPatternObjectSlot->desc->slotName->name;
   newMark->startPosition = objectSlotField;
   newMark->next = NULL;
   if (CurrentPatternObjectMarks == NULL)
     CurrentPatternObjectMarks = newMark;
   else
     endMark->next = newMark;

   /* ==========================================================
      If there are further pattern restrictions on this slot,
      try pattern-matching for all possible bound values of the
      multifield pattern node: from no values to all values from
      the starting position of the multifield to the end of the
      object slot.  Otherwise, bind the multifield to all the
      remaining fields in the slot value and continue with
      pattern-matching
      ========================================================== */
   if (patternNode->endSlot == FALSE)
     {
      objectSlotLength = CurrentObjectSlotLength;
      objectSlot = CurrentPatternObjectSlot;
      newMark->endPosition = newMark->startPosition - 1;
      repeatCount = objectSlotLength - newMark->startPosition
                    - patternNode->leaveFields + 2;
      while (repeatCount > 0)
        {
         if ((patternNode->networkTest == NULL) ? TRUE :
              EvaluateObjectPatternTest(objectSlotField,newMark,
                        (EXPRESSION *) patternNode->networkTest,patternNode))
           {
            if (patternNode->alphaNode != NULL)
              CreateObjectAlphaMatch(patternNode->alphaNode);
            ObjectPatternMatch((int) (offset + (newMark->endPosition - objectSlotField)),
                               patternNode->nextLevel,newMark);
            CurrentObjectSlotLength = objectSlotLength;
            CurrentPatternObjectSlot = objectSlot;
           }
         newMark->endPosition++;
         repeatCount--;
        }
     }
   else
     {
      newMark->endPosition = CurrentObjectSlotLength;
      if ((patternNode->networkTest == NULL) ? TRUE :
          EvaluateObjectPatternTest(objectSlotField,newMark,
                                    (EXPRESSION *) patternNode->networkTest,patternNode))
        {
         if (patternNode->alphaNode != NULL)
           CreateObjectAlphaMatch(patternNode->alphaNode);
         ObjectPatternMatch(0,patternNode->nextLevel,newMark);
        }
     }

   /* ======================================
      Delete the temporary multifield marker
      ====================================== */

   if (CurrentPatternObjectMarks == newMark)
     CurrentPatternObjectMarks = NULL;
   else
     endMark->next = NULL;
   rtn_struct(multifieldMarker,newMark);
  }

/***************************************************
  NAME         : CreateObjectAlphaMatch
  DESCRIPTION  : Places an instance in the alpha
                 memory of a pattern and drives the
                 partial match through the join
                 network
  INPUTS       : The alpha memory node
  RETURNS      : Nothing useful
  SIDE EFFECTS : Join network updated
  NOTES        : None
 ***************************************************/
static void CreateObjectAlphaMatch(
  OBJECT_ALPHA_NODE *alphaPtr)
  {
   struct joinNode *listOfJoins;
   struct partialMatch *theMatch;
   struct patternMatch *newMatch;

   while (alphaPtr != NULL)
     {
      if (alphaPtr->matchTimeTag == CurrentObjectMatchTimeTag)
        {
         /* ===================================================
            If we have reached the class bitmap of the pattern,
            place the object in the alpha memory of each of
            the terminal nodes underneath and drive
            the partial matches through the join network.

            Insert the instance into the alpha memory
            of this pattern and mark it as busy
            =================================================== */
         CurrentPatternObject->busy++;
         theMatch = CreateAlphaMatch((void *) CurrentPatternObject,
                                     CurrentPatternObjectMarks,
                                     (struct patternNodeHeader *) alphaPtr);

         /* ======================================
            Attach the partial match to the object
            to ease later retraction
            ====================================== */
         newMatch = get_struct(patternMatch);
         newMatch->next = (struct patternMatch *) CurrentPatternObject->partialMatchList;
         newMatch->matchingPattern = (struct patternNodeHeader *) alphaPtr;
         newMatch->theMatch = theMatch;
         CurrentPatternObject->partialMatchList = (void *) newMatch;

         /* ================================================
            Drive the partial match through the join network
            ================================================ */
         listOfJoins = alphaPtr->header.entryJoin;
         while (listOfJoins != NULL)
           {
            NetworkAssert(theMatch,listOfJoins,RHS);
            listOfJoins = listOfJoins->rightMatchNode;
           }
        }
      alphaPtr = alphaPtr->nxtInGroup;
     }
  }

/******************************************************
  NAME         : EvaluateObjectPatternTest
  DESCRIPTION  : Evaluates the pattern network test
                 expression for a node
  INPUTS       : 1) The actual index of the slot value
                    field currently being examined
                 2) The multifield marker (if any)
                    for the pattern node being exmained
                 3) The pattern network test expression
                 4) The pattern node being examined
  RETURNS      : TRUE if the node passes the
                 test, FALSE otherwise
  SIDE EFFECTS : Evaluation of the test
                 EvaluationError and HaltExecution
                 are always set to FALSE
  NOTES        : Assumes networkTest != NULL
 ******************************************************/
static BOOLEAN EvaluateObjectPatternTest(
  int objectSlotField,
  struct multifieldMarker *selfSlotMarker,
  EXPRESSION *networkTest,
  OBJECT_PATTERN_NODE *patternNode)
  {
   DATA_OBJECT vresult;
   int rv;

   if (networkTest->type == OBJ_PN_CONSTANT)
     {
      struct expr *oldArgument;

      oldArgument = CurrentExpression;
      CurrentExpression = networkTest;
      rv = ObjectCmpConstantFunction(networkTest->value,&vresult);
      CurrentExpression = oldArgument;
      if (rv)
        {
         if (((struct ObjectCmpPNConstant *)
                 ValueToBitMap(networkTest->value))->pass)
           patternNode->blocked = TRUE;
         return(TRUE);
        }
      return(FALSE);
     }

   /* =========================================================
      Evaluate or expressions expressed in the format:
         (or <expression 1> <expression 2> ... <expression n>)
       Returns TRUE (1.0) if any of the expression are TRUE,
       otherwise returns false (0.0).
      ========================================================= */
   if (networkTest->value == PTR_OR)
     {
      networkTest = networkTest->argList;
      while (networkTest != NULL)
        {
         if (EvaluateObjectPatternTest(objectSlotField,selfSlotMarker,networkTest,patternNode))
           {
            /* ============================================
               A node can be blocked ONLY if there were one
               positive constant test on that node
               ============================================ */
            patternNode->blocked = FALSE;
            return(TRUE);
           }
         patternNode->blocked = FALSE;
         networkTest = networkTest->nextArg;
        }
      return(FALSE);
     }

   /* ==========================================================
      Evaluate and expressions expressed in the format:
       (and <expression 1> <expression 2> ... <expression n>)
      Returns false (0.0) if any of the expression are false,
      otherwise returns TRUE (1.0).
      ========================================================== */
   else if (networkTest->value == PTR_AND)
     {
      networkTest = networkTest->argList;
      while (networkTest != NULL)
        {
         if (EvaluateObjectPatternTest(objectSlotField,selfSlotMarker,networkTest,patternNode)
              == FALSE)
           {
            patternNode->blocked = FALSE;
            return(FALSE);
           }
         patternNode->blocked = FALSE;
         networkTest = networkTest->nextArg;
        }
      return(TRUE);
     }

   /* =======================================================
      Evaluate all other expressions using EvaluateExpression
      ======================================================= */
   else
     {
      HaltExecution = FALSE;
      if (EvaluateExpression(networkTest,&vresult))
        {
         ObjectPatternNetErrorMessage(patternNode);
         EvaluationError = FALSE;
         HaltExecution = FALSE;
         return(FALSE);
        }
      if ((vresult.value != FalseSymbol) || (vresult.type != SYMBOL))
        return(TRUE);
     }
   return(FALSE);
  }

/***************************************************
  NAME         : ObjectAssertAction
  DESCRIPTION  : Filters an instance through the
                 object pattern network
  INPUTS       : The instance
  RETURNS      : Nothing useful
  SIDE EFFECTS : Instance matched
  NOTES        : None
 ***************************************************/
static void ObjectAssertAction(
  INSTANCE_TYPE *ins)
  {
   ins->header.timeTag = UseEntityTimeTag;
   CurrentPatternObject = ins;
   CurrentPatternObjectSlot = NULL;
   MarkObjectPatternNetwork(NULL);
   ObjectPatternMatch(0,ObjectNetworkPointer(),NULL);
   ins->reteSynchronized = TRUE;
  }

/**********************************************************************
  NAME         : ObjectModifyAction
  DESCRIPTION  : Removes an instance from patterns (and attached joins)
                 applicable to specified slot(s), and then filters
                 same instance through object pattern network
                 (only against patterns which explicitly match on
                 named slot(s))
  INPUTS       : 1) The instance
                 2) The bitmap of slot ids
  RETURNS      : Nothing useful
  SIDE EFFECTS : Instance retracted/asserted
  NOTES        : None
 **********************************************************************/
static void ObjectModifyAction(
  INSTANCE_TYPE *ins,
  SLOT_BITMAP *slotNameIDs)
  {
   ins->header.timeTag = UseEntityTimeTag;
   ObjectRetractAction(ins,slotNameIDs);
   CurrentPatternObject = ins;
   CurrentPatternObjectSlot = NULL;
   MarkObjectPatternNetwork(slotNameIDs);
   ObjectPatternMatch(0,ObjectNetworkPointer(),NULL);
   ins->reteSynchronized = TRUE;
  }

/****************************************************
  NAME         : ObjectRetractAction
  DESCRIPTION  : Retracts the instance from the
                 applicable patterns for the object
                 (if the slotNameID != -1, then the
                  instance is only retracted from
                  the alpha memories of the patterns
                  which actually match on that slot)
  INPUTS       : 1) The instance
                 2) The slot bitmap for a modify
                    (NULL if the instance is actually
                     being removed)
  RETURNS      : Nothing useful
  SIDE EFFECTS : Retractions performed
  NOTES        : None
 ****************************************************/
static void ObjectRetractAction(
  INSTANCE_TYPE *ins,
  SLOT_BITMAP *slotNameIDs)
  {
   struct patternMatch *prvMatch,*tmpMatch,
                       *deleteMatch,*lastDeleteMatch;
   OBJECT_ALPHA_NODE *alphaPtr;
#if LOGICAL_DEPENDENCIES
   void *saveDependents;
#endif

   if (slotNameIDs == NULL)
     {
      if (ins->partialMatchList != NULL)
        {
         tmpMatch = (struct patternMatch *) ins->partialMatchList;
         while (tmpMatch != NULL)
           {
            ins->busy--;
            tmpMatch = tmpMatch->next;
           }
         NetworkRetract((struct patternMatch *) ins->partialMatchList);
         ins->partialMatchList = NULL;
        }
     }
   else
     {
      deleteMatch = NULL;
      lastDeleteMatch = NULL;
      prvMatch = NULL;
      tmpMatch = (struct patternMatch *) ins->partialMatchList;
      while (tmpMatch != NULL)
        {
         alphaPtr = (OBJECT_ALPHA_NODE *) tmpMatch->matchingPattern;
         if (alphaPtr->slotbmp != NULL)
           {
           if (CompareSlotBitMaps(slotNameIDs,
                  (SLOT_BITMAP *) ValueToBitMap(alphaPtr->slotbmp)))
              {
               ins->busy--;
               if (prvMatch == NULL)
                 ins->partialMatchList = (void *) tmpMatch->next;
               else
                 prvMatch->next = tmpMatch->next;
               if (!deleteMatch)
                 deleteMatch = tmpMatch;
               else
                 lastDeleteMatch->next = tmpMatch;
               lastDeleteMatch = tmpMatch;
               tmpMatch = tmpMatch->next;
               lastDeleteMatch->next = NULL;
              }
            else
              {
               prvMatch = tmpMatch;
               tmpMatch = tmpMatch->next;
              }
           }
         else
           {
            prvMatch = tmpMatch;
            tmpMatch = tmpMatch->next;
           }
        }

      /* =============================================
         We need to preserve any logical dependencies
         of this object and reattach them after doing
         the retract.  Otherwise, the Rete network
         will believe the object is gone and remove
         the links from the partial matches upon which
         this object is logically dependent.
         ============================================= */
      if (deleteMatch != NULL)
        {
#if LOGICAL_DEPENDENCIES
         saveDependents = ins->header.dependents;
         ins->header.dependents = NULL;
         NetworkRetract(deleteMatch);
         ins->header.dependents = saveDependents;
#else
         NetworkRetract(deleteMatch);
#endif
        }
     }
   ins->reteSynchronized = TRUE;
  }

/*****************************************************
  NAME         : ObjectPatternNetErrorMessage
  DESCRIPTION  : Prints out a locational error message
                 when an evaluation error occurs
                 during object pattern-matching
  INPUTS       : The pattern node
  RETURNS      : Nothing useful
  SIDE EFFECTS : Error message displayed
  NOTES        : None
 *****************************************************/
static void ObjectPatternNetErrorMessage(
  OBJECT_PATTERN_NODE *patternPtr)
  {
   PrintErrorID("OBJRTMCH",1,TRUE);
   PrintRouter(WERROR,"This error occurred in the object pattern network\n");
   PrintRouter(WERROR,"   Currently active instance: [");
   PrintRouter(WERROR,ValueToString(CurrentPatternObject->name));
   PrintRouter(WERROR,"]\n");
   PrintRouter(WERROR,"   Problem resides in slot ");
   PrintRouter(WERROR,ValueToString(FindIDSlotName(patternPtr->slotNameID)));
   PrintRouter(WERROR," field #");
   PrintLongInteger(WERROR,(long) patternPtr->whichField);
   PrintRouter(WERROR,"\n");
   TraceErrorToObjectPattern(TRUE,patternPtr);
   PrintRouter(WERROR,"\n");
  }

/*********************************************************
  NAME         : TraceErrorToObjectPattern
  DESCRIPTION  : Used by ObjectPatternNetErrorMessage() to
                 print the rule(s) which contain an object
                 pattern.
  INPUTS       : 1) A flag indicating if this is the
                    node in which the error actually
                    occurred or not
                 2) The pattern node
  RETURNS      : Nothing useful
  SIDE EFFECTS : Error message displayed
  NOTES        : None
 *********************************************************/
static void TraceErrorToObjectPattern(
  int errorNode,
  OBJECT_PATTERN_NODE *patternPtr)
  {
   struct joinNode *joinPtr;

   while (patternPtr != NULL)
     {
      if (patternPtr->alphaNode != NULL)
        {
         joinPtr = patternPtr->alphaNode->header.entryJoin;
         while (joinPtr != NULL)
           {
            PrintRouter(WERROR,"      Of pattern #");
            PrintLongInteger(WERROR,(long) joinPtr->depth);
            PrintRouter(WERROR," in rule(s):\n");
            TraceErrorToRule(joinPtr,"         ");
            joinPtr = joinPtr->rightMatchNode;
           }
        }
      TraceErrorToObjectPattern(FALSE,patternPtr->nextLevel);
      if (errorNode)
        break;
      patternPtr = patternPtr->rightNode;
     }

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
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/09/97          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_objrtmch
#define _H_objrtmch

#if INSTANCE_PATTERN_MATCHING

#define OBJECT_ASSERT  1
#define OBJECT_RETRACT 2
#define OBJECT_MODIFY  3

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_match
#include "match.h"
#endif
#ifndef _H_network
#include "network.h"
#endif
#ifndef _H_object
#include "object.h"
#endif
#ifndef _H_symbol
#include "symbol.h"
#endif

typedef struct classBitMap
  {
   unsigned short maxid;
   char map[1];
  } CLASS_BITMAP;

#define ClassBitMapSize(bmp) ((int) (sizeof(CLASS_BITMAP) + \
                                     (sizeof(char) * (bmp->maxid / BITS_PER_BYTE))))

typedef struct slotBitMap
  {
   unsigned short maxid;
   char map[1];
  } SLOT_BITMAP;

#define SlotBitMapSize(bmp) ((int) (sizeof(SLOT_BITMAP) + \
                                     (sizeof(char) * (bmp->maxid / BITS_PER_BYTE))))

typedef struct objectAlphaNode OBJECT_ALPHA_NODE;

typedef struct objectPatternNode
  {
   unsigned blocked        : 1;
   unsigned multifieldNode : 1;
   unsigned endSlot        : 1;
   unsigned whichField     : 8;
   unsigned leaveFields    : 8;
   unsigned long matchTimeTag;
   unsigned slotNameID;
   EXPRESSION *networkTest;
   struct objectPatternNode *nextLevel;
   struct objectPatternNode *lastLevel;
   struct objectPatternNode *leftNode;
   struct objectPatternNode *rightNode;
   OBJECT_ALPHA_NODE *alphaNode;
   long bsaveID;
  } OBJECT_PATTERN_NODE;

struct objectAlphaNode
  {
   struct patternNodeHeader header;
   unsigned long matchTimeTag;
   BITMAP_HN *classbmp,*slotbmp;
   OBJECT_PATTERN_NODE *patternNode;
   struct objectAlphaNode *nxtInGroup,
                          *nxtTerminal;
   long bsaveID;
  };

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _OBJRTMCH_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

LOCALE void ObjectMatchDelay(DATA_OBJECT *);
LOCALE BOOLEAN SetDelayObjectPatternMatching(int);
LOCALE BOOLEAN GetDelayObjectPatternMatching(void);
LOCALE OBJECT_PATTERN_NODE *ObjectNetworkPointer(void);
LOCALE OBJECT_ALPHA_NODE *ObjectNetworkTerminalPointer(void);
LOCALE void SetObjectNetworkPointer(OBJECT_PATTERN_NODE *);
LOCALE void SetObjectNetworkTerminalPointer(OBJECT_ALPHA_NODE *);
LOCALE void ObjectNetworkAction(int,INSTANCE_TYPE *,int);

#endif

#endif







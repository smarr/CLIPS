   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.24  05/17/06          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
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
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#ifndef _H_classfun
#define _H_classfun

#ifndef _H_object
#include "object.h"
#endif

#define TestTraversalID(traversalRecord,id) TestBitMap(traversalRecord,id)
#define SetTraversalID(traversalRecord,id) SetBitMap(traversalRecord,id)
#define ClearTraversalID(traversalRecord,id) ClearBitMap(traversalRecord,id)

#define CLASS_TABLE_HASH_SIZE     167
#define SLOT_NAME_TABLE_HASH_SIZE 167

#define INITIAL_OBJECT_CLASS_NAME "INITIAL-OBJECT"

#define ISA_ID  0
#define NAME_ID 1

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _CLASSFUN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

LOCALE void IncrementDefclassBusyCount(void *,EXEC_STATUS,void *);
LOCALE void DecrementDefclassBusyCount(void *,EXEC_STATUS,void *);
LOCALE intBool InstancesPurge(void *,EXEC_STATUS);

#if ! RUN_TIME
LOCALE void InitializeClasses(void *,EXEC_STATUS);
#endif
LOCALE SLOT_DESC *FindClassSlot(DEFCLASS *,SYMBOL_HN *);
LOCALE void ClassExistError(void *,EXEC_STATUS,char *,char *);
LOCALE void DeleteClassLinks(void *,EXEC_STATUS,CLASS_LINK *);
LOCALE void PrintClassName(void *,EXEC_STATUS,char *,DEFCLASS *,intBool);

#if DEBUGGING_FUNCTIONS || ((! BLOAD_ONLY) && (! RUN_TIME))
LOCALE void PrintPackedClassLinks(void *,EXEC_STATUS,char *,char *,PACKED_CLASS_LINKS *);
#endif

#if ! RUN_TIME
LOCALE void PutClassInTable(void *,EXEC_STATUS,DEFCLASS *);
LOCALE void RemoveClassFromTable(void *,EXEC_STATUS,DEFCLASS *);
LOCALE void AddClassLink(void *,EXEC_STATUS,PACKED_CLASS_LINKS *,DEFCLASS *,int);
LOCALE void DeleteSubclassLink(void *,EXEC_STATUS,DEFCLASS *,DEFCLASS *);
LOCALE DEFCLASS *NewClass(void *,EXEC_STATUS,SYMBOL_HN *);
LOCALE void DeletePackedClassLinks(void *,EXEC_STATUS,PACKED_CLASS_LINKS *,int);
LOCALE void AssignClassID(void *,EXEC_STATUS,DEFCLASS *);
LOCALE SLOT_NAME *AddSlotName(void *,EXEC_STATUS,SYMBOL_HN *,int,int);
LOCALE void DeleteSlotName(void *,EXEC_STATUS,SLOT_NAME *);
LOCALE void RemoveDefclass(void *,EXEC_STATUS,void *);
LOCALE void InstallClass(void *,EXEC_STATUS,DEFCLASS *,int);
#endif
LOCALE void DestroyDefclass(void *,EXEC_STATUS,void *);

#if (! BLOAD_ONLY) && (! RUN_TIME)
LOCALE int IsClassBeingUsed(DEFCLASS *);
LOCALE int RemoveAllUserClasses(void *,EXEC_STATUS);
LOCALE int DeleteClassUAG(void *,EXEC_STATUS,DEFCLASS *);
LOCALE void MarkBitMapSubclasses(char *,DEFCLASS *,int);
#endif

LOCALE short FindSlotNameID(void *,EXEC_STATUS,SYMBOL_HN *);
LOCALE SYMBOL_HN *FindIDSlotName(void *,EXEC_STATUS,int);
LOCALE SLOT_NAME *FindIDSlotNameHash(void *,EXEC_STATUS,int);
LOCALE int GetTraversalID(void *,EXEC_STATUS);
LOCALE void ReleaseTraversalID(void *,EXEC_STATUS);
LOCALE unsigned HashClass(SYMBOL_HN *);

#ifndef _CLASSFUN_SOURCE_

#if DEFRULE_CONSTRUCT
extern SYMBOL_HN *INITIAL_OBJECT_SYMBOL;
#endif
#if DEBUGGING_FUNCTIONS
extern unsigned WatchInstances,WatchSlots;
#endif
#endif

#define DEFCLASS_DATA 21

#define PRIMITIVE_CLASSES 9

struct defclassData
  { 
   struct construct *DefclassConstruct;
   int DefclassModuleIndex;
   ENTITY_RECORD DefclassEntityRecord;
   DEFCLASS *PrimitiveClassMap[PRIMITIVE_CLASSES];
   DEFCLASS **ClassIDMap;
   DEFCLASS **ClassTable;
   unsigned short MaxClassID;
   unsigned short AvailClassID;
   SLOT_NAME **SlotNameTable;
   SYMBOL_HN *ISA_SYMBOL;
   SYMBOL_HN *NAME_SYMBOL;
#if DEFRULE_CONSTRUCT
   SYMBOL_HN *INITIAL_OBJECT_SYMBOL;
#endif
#if DEBUGGING_FUNCTIONS
   unsigned WatchInstances;
   unsigned WatchSlots;
#endif
   unsigned short CTID;
   struct token ObjectParseToken;
   unsigned short ClassDefaultsMode;
  };

#define DefclassData(theEnv,execStatus) ((struct defclassData *) GetEnvironmentData(theEnv,execStatus,DEFCLASS_DATA))

#endif










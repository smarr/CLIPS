   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/13/98          */
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

LOCALE void IncrementDefclassBusyCount(void *);
LOCALE void DecrementDefclassBusyCount(void *);
LOCALE BOOLEAN InstancesPurge(void);

#if ! RUN_TIME
LOCALE void InitializeClasses(void);
#endif
LOCALE SLOT_DESC *FindClassSlot(DEFCLASS *,SYMBOL_HN *);
LOCALE void ClassExistError(char *,char *);
LOCALE void DeleteClassLinks(CLASS_LINK *);
LOCALE void PrintClassName(char *,DEFCLASS *,BOOLEAN);

#if DEBUGGING_FUNCTIONS || ((! BLOAD_ONLY) && (! RUN_TIME))
LOCALE void PrintPackedClassLinks(char *,char *,PACKED_CLASS_LINKS *);
#endif

#if ! RUN_TIME
LOCALE void PutClassInTable(DEFCLASS *);
LOCALE void RemoveClassFromTable(DEFCLASS *);
LOCALE void AddClassLink(PACKED_CLASS_LINKS *,DEFCLASS *,int);
LOCALE void DeleteSubclassLink(DEFCLASS *,DEFCLASS *);
LOCALE DEFCLASS *NewClass(SYMBOL_HN *);
LOCALE void DeletePackedClassLinks(PACKED_CLASS_LINKS *,int);
LOCALE void AssignClassID(DEFCLASS *);
LOCALE SLOT_NAME *AddSlotName(SYMBOL_HN *,unsigned,int);
LOCALE void DeleteSlotName(SLOT_NAME *);
LOCALE void RemoveDefclass(void *);
LOCALE void InstallClass(DEFCLASS *,int);
#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)
LOCALE int IsClassBeingUsed(DEFCLASS *);
LOCALE int RemoveAllUserClasses(void);
LOCALE int DeleteClassUAG(DEFCLASS *);
LOCALE void MarkBitMapSubclasses(char *,DEFCLASS *,int);
#endif

LOCALE int FindSlotNameID(SYMBOL_HN *);
LOCALE SYMBOL_HN *FindIDSlotName(unsigned);
LOCALE SLOT_NAME *FindIDSlotNameHash(unsigned);
LOCALE int GetTraversalID(void);
LOCALE void ReleaseTraversalID(void);
LOCALE unsigned HashClass(SYMBOL_HN *);

#ifndef _CLASSFUN_SOURCE_
extern Thread DEFCLASS **ClassIDMap;
extern Thread DEFCLASS **ClassTable;
extern Thread SLOT_NAME **SlotNameTable;
extern Thread DEFCLASS *PrimitiveClassMap[];
extern Thread unsigned short MaxClassID;
extern Thread SYMBOL_HN *ISA_SYMBOL,*NAME_SYMBOL;
#if DEFRULE_CONSTRUCT
extern Thread SYMBOL_HN *INITIAL_OBJECT_SYMBOL;
#endif
#if DEBUGGING_FUNCTIONS
extern Thread int WatchInstances,WatchSlots;
#endif
#endif

#endif







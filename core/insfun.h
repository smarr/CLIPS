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

#ifndef _H_insfun
#define _H_insfun

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_moduldef
#include "moduldef.h"
#endif
#ifndef _H_object
#include "object.h"
#endif

#ifndef _H_pattern
#include "pattern.h"
#endif

typedef struct igarbage
  {
   INSTANCE_TYPE *ins;
   struct igarbage *nxt;
  } IGARBAGE;

#define INSTANCE_TABLE_HASH_SIZE 683
#define InstanceSizeHeuristic(ins)      sizeof(INSTANCE_TYPE)

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _INSFUN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

LOCALE void IncrementInstanceCount(void *);
LOCALE void DecrementInstanceCount(void *);
LOCALE void InitializeInstanceTable(void);
LOCALE void CleanupInstances(void);
LOCALE unsigned HashInstance(SYMBOL_HN *);
LOCALE void DestroyAllInstances(void);
LOCALE void RemoveInstanceData(INSTANCE_TYPE *);
LOCALE INSTANCE_TYPE *FindInstanceBySymbol(SYMBOL_HN *);
globle INSTANCE_TYPE *FindInstanceInModule(SYMBOL_HN *,struct defmodule *,
                                           struct defmodule *,BOOLEAN);
LOCALE INSTANCE_SLOT *FindInstanceSlot(INSTANCE_TYPE *,SYMBOL_HN *);
LOCALE int FindInstanceTemplateSlot(DEFCLASS *,SYMBOL_HN *);
LOCALE int EvaluateAndStoreInDataObject(int,EXPRESSION *,DATA_OBJECT *);
LOCALE int PutSlotValue(INSTANCE_TYPE *,INSTANCE_SLOT *,DATA_OBJECT *,DATA_OBJECT *,char *);
LOCALE int DirectPutSlotValue(INSTANCE_TYPE *,INSTANCE_SLOT *,DATA_OBJECT *,DATA_OBJECT *);
LOCALE BOOLEAN ValidSlotValue(DATA_OBJECT *,SLOT_DESC *,INSTANCE_TYPE *,char *);
LOCALE INSTANCE_TYPE *CheckInstance(char *);
LOCALE void NoInstanceError(char *,char *);
LOCALE void SlotExistError(char *,char *);
LOCALE void StaleInstanceAddress(char *,int);
LOCALE int GetInstancesChanged(void);
LOCALE void SetInstancesChanged(int);
LOCALE void PrintSlot(char *,SLOT_DESC *,INSTANCE_TYPE *,char *);
LOCALE void PrintInstanceNameAndClass(char *,INSTANCE_TYPE *,BOOLEAN);

#ifndef _INSFUN_SOURCE_
extern Thread INSTANCE_TYPE **InstanceTable;
extern Thread int MaintainGarbageInstances;
extern Thread int MkInsMsgPass;
extern Thread int ChangesToInstances;
extern Thread IGARBAGE *InstanceGarbageList;
extern Thread struct patternEntityRecord InstanceInfo;
#endif

#endif








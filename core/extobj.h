   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/09/97          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose      External Function Definitions for COOL       */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_extobj
#define _H_extobj

#ifndef _H_moduldef
#include "moduldef.h"
#endif
#ifndef _H_constrct
#include "constrct.h"
#endif
#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_expressn
#include "expressn.h"
#endif

#define GetDefclassName(x) GetConstructNameString((struct constructHeader *) x)
#define GetDefclassPPForm(x) GetConstructPPForm((struct constructHeader *) x)
#define GetDefinstancesName(x) GetConstructNameString((struct constructHeader *) x)
#define GetDefinstancesPPForm(x) GetConstructPPForm((struct constructHeader *) x)

#define DefclassModule(x) GetConstructModuleName((struct constructHeader *) x)
#define DefinstancesModule(x) GetConstructModuleName((struct constructHeader *) x)

extern void SetupObjectSystem(void);

extern void IncrementInstanceCount(void *);
extern void DecrementInstanceCount(void *);
extern int GetInstancesChanged(void);
extern void SetInstancesChanged(int);

extern void *GetNextDefclass(void *);
extern void *FindDefclass(char *);
extern BOOLEAN IsDefclassDeletable(void *);
extern BOOLEAN Undefclass(void *);

#if DEFINSTANCES_CONSTRUCT
extern void *GetNextDefinstances(void *);
extern void *FindDefinstances(char *);
extern int IsDefinstancesDeletable(void *);
extern BOOLEAN Undefinstances(void *);
extern void GetDefinstancesList(DATA_OBJECT *,struct defmodule *);
#endif

extern long SaveInstances(char *,int,EXPRESSION *,BOOLEAN);

#if BSAVE_INSTANCES
extern long BinarySaveInstances(char *,int,EXPRESSION *,BOOLEAN);
#endif

#if BLOAD_INSTANCES
extern long BinaryLoadInstances(char *);
#endif

extern long LoadInstances(char *);
extern long LoadInstancesFromString(char *,int);
extern long RestoreInstances(char *);
extern long RestoreInstancesFromString(char *,int);
extern void *MakeInstance(char *);
extern BOOLEAN DeleteInstance(void *);
extern BOOLEAN UnmakeInstance(void *);
extern void *CreateRawInstance(void *,char *);
extern void *FindInstance(void *,char *,BOOLEAN);
extern int ValidInstanceAddress(void *);
extern void DirectGetSlot(void *,char *,DATA_OBJECT *);
extern int DirectPutSlot(void *,char *,DATA_OBJECT *);
extern char *GetInstanceName(void *);
extern void *GetInstanceClass(void *);
extern void *GetNextInstance(void *);
extern unsigned long GetGlobalNumberOfInstances(void);
extern void *GetNextInstanceInScope(void *);
extern void *GetNextInstanceInClass(void *,void *);
extern void GetInstancePPForm(char *,int,void *);

extern char *GetDefmessageHandlerName(void *,unsigned);
extern char *GetDefmessageHandlerType(void *,unsigned);
extern unsigned GetNextDefmessageHandler(void *,unsigned);
extern unsigned FindDefmessageHandler(void *,char *,char *);
extern int IsDefmessageHandlerDeletable(void *,unsigned);
extern int UndefmessageHandler(void *,unsigned);
extern int WildDeleteHandler(void *,char *,char *);
extern void Send(DATA_OBJECT *,char *,char *,DATA_OBJECT *);

#if DEBUGGING_FUNCTIONS

extern void DescribeClass(char *,void *);
extern void BrowseClasses(char *,void *);

extern BOOLEAN GetDefclassWatchInstances(void *);
extern void SetDefclassWatchInstances(int,void *);
extern BOOLEAN GetDefclassWatchSlots(void *);
extern void SetDefclassWatchSlots(int,void *);

extern char *GetDefmessageHandlerPPForm(void *,unsigned);

extern void ListDefclasses(char *,struct defmodule *);
extern void ListDefinstances(char *,struct defmodule *);
extern void Instances(char *,void *,char *,int);
extern void ListDefmessageHandlers(char *,void *,int);

extern void PreviewSend(char *,void *,char *);

extern BOOLEAN GetDefmessageHandlerWatch(void *,unsigned);
extern void SetDefmessageHandlerWatch(int,void *,unsigned);

#endif

extern BOOLEAN SuperclassP(void *,void *);
extern BOOLEAN SubclassP(void *,void *);
extern BOOLEAN ClassAbstractP(void *);
#if DEFRULE_CONSTRUCT
extern BOOLEAN ClassReactiveP(void *);
#endif
extern BOOLEAN SlotExistP(void *,char *,BOOLEAN);
extern BOOLEAN SlotWritableP(void *,char *);
extern BOOLEAN SlotInitableP(void *,char *);
extern BOOLEAN SlotPublicP(void *,char *);
extern BOOLEAN SlotDirectAccessP(void *,char *);
extern BOOLEAN SlotDefaultValue(void *,char *,DATA_OBJECT_PTR);
extern void ClassSlots(void *,DATA_OBJECT *,int);
extern void GetDefmessageHandlerList(void *,DATA_OBJECT *,int);
extern void ClassSuperclasses(void *,DATA_OBJECT *,int);
extern void ClassSubclasses(void *,DATA_OBJECT *,int);
extern void SlotFacets(void *,char *,DATA_OBJECT *);
extern void SlotSources(void *,char *,DATA_OBJECT *);
extern void SlotTypes(void *,char *,DATA_OBJECT *);
extern void SlotAllowedValues(void *,char *,DATA_OBJECT *);
extern void SlotRange(void *,char *,DATA_OBJECT *);
extern void SlotCardinality(void *,char *,DATA_OBJECT *);
extern void GetDefclassList(DATA_OBJECT *,struct defmodule *);

#if INSTANCE_PATTERN_MATCHING
extern BOOLEAN SetDelayObjectPatternMatching(int);
extern BOOLEAN GetDelayObjectPatternMatching(void);
#endif

extern Thread int ChangesToInstances;
extern Thread INSTANCE_TYPE DummyInstance;

#endif




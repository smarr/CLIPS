   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.24  07/01/05          */
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
/*      6.24: Added allowed-classes slot facet.              */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#ifndef _H_classinf
#define _H_classinf

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _CLASSINF_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define ClassAbstractP(a) EnvClassAbstractP(GetCurrentEnvironment(),GetCurrentExecutionStatus(),a)
#define ClassReactiveP(a) EnvClassReactiveP(GetCurrentEnvironment(),GetCurrentExecutionStatus(),a)
#define ClassSlots(a,b,c) EnvClassSlots(GetCurrentEnvironment(),GetCurrentExecutionStatus(),a,b,c)
#define ClassSubclasses(a,b,c) EnvClassSubclasses(GetCurrentEnvironment(),GetCurrentExecutionStatus(),a,b,c)
#define ClassSuperclasses(a,b,c) EnvClassSuperclasses(GetCurrentEnvironment(),GetCurrentExecutionStatus(),a,b,c)
#define SlotAllowedValues(a,b,c) EnvSlotAllowedValues(GetCurrentEnvironment(),GetCurrentExecutionStatus(),a,b,c)
#define SlotAllowedClasses(a,b,c) EnvSlotAllowedClasses(GetCurrentEnvironment(),GetCurrentExecutionStatus(),a,b,c)
#define SlotCardinality(a,b,c) EnvSlotCardinality(GetCurrentEnvironment(),GetCurrentExecutionStatus(),a,b,c)
#define SlotFacets(a,b,c) EnvSlotFacets(GetCurrentEnvironment(),GetCurrentExecutionStatus(),a,b,c)
#define SlotRange(a,b,c) EnvSlotRange(GetCurrentEnvironment(),GetCurrentExecutionStatus(),a,b,c)
#define SlotSources(a,b,c) EnvSlotSources(GetCurrentEnvironment(),GetCurrentExecutionStatus(),a,b,c)
#define SlotTypes(a,b,c) EnvSlotTypes(GetCurrentEnvironment(),GetCurrentExecutionStatus(),a,b,c)
#define GetDefmessageHandlerList(a,b,c) EnvGetDefmessageHandlerList(GetCurrentEnvironment(),GetCurrentExecutionStatus(),a,b,c)

LOCALE intBool ClassAbstractPCommand(void *,EXEC_STATUS);
#if DEFRULE_CONSTRUCT
LOCALE intBool ClassReactivePCommand(void *,EXEC_STATUS);
#endif
LOCALE void *ClassInfoFnxArgs(void *,EXEC_STATUS,char *,int *);
LOCALE void ClassSlotsCommand(void *,EXEC_STATUS,DATA_OBJECT *);
LOCALE void ClassSuperclassesCommand(void *,EXEC_STATUS,DATA_OBJECT *);
LOCALE void ClassSubclassesCommand(void *,EXEC_STATUS,DATA_OBJECT *);
LOCALE void GetDefmessageHandlersListCmd(void *,EXEC_STATUS,DATA_OBJECT *);
LOCALE void SlotFacetsCommand(void *,EXEC_STATUS,DATA_OBJECT *);
LOCALE void SlotSourcesCommand(void *,EXEC_STATUS,DATA_OBJECT *);
LOCALE void SlotTypesCommand(void *,EXEC_STATUS,DATA_OBJECT *);
LOCALE void SlotAllowedValuesCommand(void *,EXEC_STATUS,DATA_OBJECT *);
LOCALE void SlotAllowedClassesCommand(void *,EXEC_STATUS,DATA_OBJECT *);
LOCALE void SlotRangeCommand(void *,EXEC_STATUS,DATA_OBJECT *);
LOCALE void SlotCardinalityCommand(void *,EXEC_STATUS,DATA_OBJECT *);
LOCALE intBool EnvClassAbstractP(void *,EXEC_STATUS,void *);
#if DEFRULE_CONSTRUCT
LOCALE intBool EnvClassReactiveP(void *,EXEC_STATUS,void *);
#endif
LOCALE void EnvClassSlots(void *,EXEC_STATUS,void *,DATA_OBJECT *,int);
LOCALE void EnvGetDefmessageHandlerList(void *,EXEC_STATUS,void *,DATA_OBJECT *,int);
LOCALE void EnvClassSuperclasses(void *,EXEC_STATUS,void *,DATA_OBJECT *,int);
LOCALE void EnvClassSubclasses(void *,EXEC_STATUS,void *,DATA_OBJECT *,int);
LOCALE void ClassSubclassAddresses(void *,EXEC_STATUS,void *,DATA_OBJECT *,int);
LOCALE void EnvSlotFacets(void *,EXEC_STATUS,void *,char *,DATA_OBJECT *);
LOCALE void EnvSlotSources(void *,EXEC_STATUS,void *,char *,DATA_OBJECT *);
LOCALE void EnvSlotTypes(void *,EXEC_STATUS,void *,char *,DATA_OBJECT *);
LOCALE void EnvSlotAllowedValues(void *,EXEC_STATUS,void *,char *,DATA_OBJECT *);
LOCALE void EnvSlotAllowedClasses(void *,EXEC_STATUS,void *,char *,DATA_OBJECT *);
LOCALE void EnvSlotRange(void *,EXEC_STATUS,void *,char *,DATA_OBJECT *);
LOCALE void EnvSlotCardinality(void *,EXEC_STATUS,void *,char *,DATA_OBJECT *);

#endif






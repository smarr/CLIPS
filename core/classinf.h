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

LOCALE BOOLEAN ClassAbstractPCommand(void);
#if DEFRULE_CONSTRUCT
LOCALE BOOLEAN ClassReactivePCommand(void);
#endif
LOCALE void *ClassInfoFnxArgs(char *,int *);
LOCALE void ClassSlotsCommand(DATA_OBJECT *);
LOCALE void ClassSuperclassesCommand(DATA_OBJECT *);
LOCALE void ClassSubclassesCommand(DATA_OBJECT *);
LOCALE void GetDefmessageHandlersListCmd(DATA_OBJECT *);
LOCALE void SlotFacetsCommand(DATA_OBJECT *);
LOCALE void SlotSourcesCommand(DATA_OBJECT *);
LOCALE void SlotTypesCommand(DATA_OBJECT *);
LOCALE void SlotAllowedValuesCommand(DATA_OBJECT *);
LOCALE void SlotRangeCommand(DATA_OBJECT *);
LOCALE void SlotCardinalityCommand(DATA_OBJECT *);
LOCALE BOOLEAN ClassAbstractP(void *);
#if DEFRULE_CONSTRUCT
LOCALE BOOLEAN ClassReactiveP(void *);
#endif
LOCALE void ClassSlots(void *,DATA_OBJECT *,int);
LOCALE void GetDefmessageHandlerList(void *,DATA_OBJECT *,int);
LOCALE void ClassSuperclasses(void *,DATA_OBJECT *,int);
LOCALE void ClassSubclasses(void *,DATA_OBJECT *,int);
LOCALE void ClassSubclassAddresses(void *,DATA_OBJECT *,int);
LOCALE void SlotFacets(void *,char *,DATA_OBJECT *);
LOCALE void SlotSources(void *,char *,DATA_OBJECT *);
LOCALE void SlotTypes(void *,char *,DATA_OBJECT *);
LOCALE void SlotAllowedValues(void *,char *,DATA_OBJECT *);
LOCALE void SlotRange(void *,char *,DATA_OBJECT *);
LOCALE void SlotCardinality(void *,char *,DATA_OBJECT *);

#endif




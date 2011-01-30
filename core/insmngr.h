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

#ifndef _H_insmngr
#define _H_insmngr

#ifndef _H_object
#include "object.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _INSMNGR_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

LOCALE void InitializeInstanceCommand(DATA_OBJECT *);
LOCALE void MakeInstanceCommand(DATA_OBJECT *);
LOCALE SYMBOL_HN *GetFullInstanceName(INSTANCE_TYPE *);
LOCALE INSTANCE_TYPE *BuildInstance(SYMBOL_HN *,DEFCLASS *,BOOLEAN);
LOCALE void InitSlotsCommand(DATA_OBJECT *);
LOCALE BOOLEAN QuashInstance(INSTANCE_TYPE *);

#if INSTANCE_PATTERN_MATCHING
LOCALE void InactiveInitializeInstance(DATA_OBJECT *);
LOCALE void InactiveMakeInstance(DATA_OBJECT *);
#endif

#ifndef _INSMNGR_SOURCE_
extern Thread INSTANCE_TYPE *InstanceList;
extern Thread unsigned long GlobalNumberOfInstances;
#endif

#endif








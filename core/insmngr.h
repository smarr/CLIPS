   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.24  05/17/06          */
   /*                                                     */
   /*            INSTANCE PRIMITIVE SUPPORT MODULE        */
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

LOCALE void InitializeInstanceCommand(void *,EXEC_STATUS,DATA_OBJECT *);
LOCALE void MakeInstanceCommand(void *,EXEC_STATUS,DATA_OBJECT *);
LOCALE SYMBOL_HN *GetFullInstanceName(void *,EXEC_STATUS,INSTANCE_TYPE *);
LOCALE INSTANCE_TYPE *BuildInstance(void *,EXEC_STATUS,SYMBOL_HN *,DEFCLASS *,intBool);
LOCALE void InitSlotsCommand(void *,EXEC_STATUS,DATA_OBJECT *);
LOCALE intBool QuashInstance(void *,EXEC_STATUS,INSTANCE_TYPE *);

#if DEFRULE_CONSTRUCT && OBJECT_SYSTEM
LOCALE void InactiveInitializeInstance(void *,EXEC_STATUS,DATA_OBJECT *);
LOCALE void InactiveMakeInstance(void *,EXEC_STATUS,DATA_OBJECT *);
#endif

#endif








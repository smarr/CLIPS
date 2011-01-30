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

#ifndef _H_classini
#define _H_classini

#ifndef _H_constrct
#include "constrct.h"
#endif
#ifndef _H_object
#include "object.h"
#endif

#if OBJECT_SYSTEM

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _CLASSINI_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

LOCALE void SetupObjectSystem(void);
#if RUN_TIME
LOCALE void ObjectsRunTimeInitialize(DEFCLASS *[],SLOT_NAME *[],DEFCLASS *[],unsigned);
#else
LOCALE void CreateSystemClasses(void);
#endif

#ifndef _CLASSINI_SOURCE_
extern Thread int DefclassModuleIndex;
extern Thread struct construct *DefclassConstruct;
#endif

#endif

#endif



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

#ifndef _H_genrcfun
#define _H_genrcfun

#define SaveBusyCount(gfunc)    (OldGenericBusySave = gfunc->busy)
#define RestoreBusyCount(gfunc) (gfunc->busy = OldGenericBusySave)

typedef struct defgenericModule DEFGENERIC_MODULE;
typedef struct restriction RESTRICTION;
typedef struct method DEFMETHOD;
typedef struct defgeneric DEFGENERIC;

#ifndef _STDIO_INCLUDED_
#define _STDIO_INCLUDED_
#include <stdio.h>
#endif

#ifndef _H_constrct
#include "constrct.h"
#endif
#ifndef _H_moduldef
#include "moduldef.h"
#endif
#ifndef _H_symbol
#include "symbol.h"
#endif
#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

#if OBJECT_SYSTEM
#ifndef _H_object
#include "object.h"
#endif
#endif

struct defgenericModule
  {
   struct defmoduleItemHeader header;
  };

struct restriction
  {
   void **types;
   EXPRESSION *query;
   unsigned tcnt;
  };

struct method
  {
   unsigned index,busy;
   int restrictionCount,
       minRestrictions,maxRestrictions,
       localVarCount;
   unsigned system : 1;
   unsigned trace : 1;
   RESTRICTION *restrictions;
   EXPRESSION *actions;
   char *ppForm;
   struct userData *usrData;
  };

struct defgeneric
  {
   struct constructHeader header;
   unsigned busy,trace;
   DEFMETHOD *methods;
   unsigned mcnt,new_index;
  };

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _GENRCFUN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ! RUN_TIME
LOCALE BOOLEAN ClearDefgenericsReady(void);
LOCALE void *AllocateDefgenericModule(void);
LOCALE void FreeDefgenericModule(void *);
#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)

LOCALE int ClearDefmethods(void);
LOCALE int RemoveAllExplicitMethods(DEFGENERIC *);
LOCALE void RemoveDefgeneric(void *);
LOCALE int ClearDefgenerics(void);
LOCALE void MethodAlterError(DEFGENERIC *);
LOCALE void DeleteMethodInfo(DEFGENERIC *,DEFMETHOD *);
LOCALE int MethodsExecuting(DEFGENERIC *);
#if ! OBJECT_SYSTEM
LOCALE BOOLEAN SubsumeType(int,int);
#endif
#endif

LOCALE int FindMethodByIndex(DEFGENERIC *,unsigned);
#if DEBUGGING_FUNCTIONS
LOCALE void PreviewGeneric(void);
LOCALE void PrintMethod(char *,int,DEFMETHOD *);
#endif
LOCALE DEFGENERIC *CheckGenericExists(char *,char *);
LOCALE int CheckMethodExists(char *,DEFGENERIC *,int);

#if ! OBJECT_SYSTEM
LOCALE char *TypeName(int);
#endif

LOCALE void PrintGenericName(char *,DEFGENERIC *);

#ifndef _GENRCFUN_SOURCE_
extern Thread DEFGENERIC *CurrentGeneric;
extern Thread DEFMETHOD *CurrentMethod;
extern Thread DATA_OBJECT *GenericCurrentArgument;

#if DEBUGGING_FUNCTIONS
extern Thread int WatchGenerics,WatchMethods;
#endif

#if (! RUN_TIME) && (! BLOAD_ONLY)
extern Thread int OldGenericBusySave;
#endif

#endif

#endif






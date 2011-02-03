   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.24  06/05/06          */
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
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#ifndef _H_genrcfun
#define _H_genrcfun

typedef struct defgenericModule DEFGENERIC_MODULE;
typedef struct restriction RESTRICTION;
typedef struct method DEFMETHOD;
typedef struct defgeneric DEFGENERIC;

#ifndef _STDIO_INCLUDED_
#define _STDIO_INCLUDED_
#include <stdio.h>
#endif

#ifndef _H_conscomp
#include "conscomp.h"
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
   short tcnt;
  };

struct method
  {
   short index;
   unsigned busy;
   short restrictionCount;
   short minRestrictions;
   short maxRestrictions;
   short localVarCount;
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
   short mcnt;
   short new_index;
  };

#define DEFGENERIC_DATA 27

struct defgenericData
  { 
   struct construct *DefgenericConstruct;
   int DefgenericModuleIndex;
   ENTITY_RECORD GenericEntityRecord;
#if DEBUGGING_FUNCTIONS
   unsigned WatchGenerics;
   unsigned WatchMethods;
#endif
   DEFGENERIC *CurrentGeneric;
   DEFMETHOD *CurrentMethod;
   DATA_OBJECT *GenericCurrentArgument;
#if (! RUN_TIME) && (! BLOAD_ONLY)
   unsigned OldGenericBusySave;
#endif
#if CONSTRUCT_COMPILER && (! RUN_TIME)
   struct CodeGeneratorItem *DefgenericCodeItem;
#endif
#if (! BLOAD_ONLY) && (! RUN_TIME)
   struct token GenericInputToken;
#endif
  };

#define DefgenericData(theEnv,execStatus) ((struct defgenericData *) GetEnvironmentData(theEnv,execStatus,DEFGENERIC_DATA))
#define SaveBusyCount(gfunc)    (DefgenericData(theEnv,execStatus)->OldGenericBusySave = gfunc->busy)
#define RestoreBusyCount(gfunc) (gfunc->busy = DefgenericData(theEnv,execStatus)->OldGenericBusySave)

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _GENRCFUN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ! RUN_TIME
LOCALE intBool ClearDefgenericsReady(void *,EXEC_STATUS);
LOCALE void *AllocateDefgenericModule(void *,EXEC_STATUS);
LOCALE void FreeDefgenericModule(void *,EXEC_STATUS,void *);
#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)

LOCALE int ClearDefmethods(void *,EXEC_STATUS);
LOCALE int RemoveAllExplicitMethods(void *,EXEC_STATUS,DEFGENERIC *);
LOCALE void RemoveDefgeneric(void *,EXEC_STATUS,void *);
LOCALE int ClearDefgenerics(void *,EXEC_STATUS);
LOCALE void MethodAlterError(void *,EXEC_STATUS,DEFGENERIC *);
LOCALE void DeleteMethodInfo(void *,EXEC_STATUS,DEFGENERIC *,DEFMETHOD *);
LOCALE void DestroyMethodInfo(void *,EXEC_STATUS,DEFGENERIC *,DEFMETHOD *);
LOCALE int MethodsExecuting(DEFGENERIC *);
#endif
#if ! OBJECT_SYSTEM
LOCALE intBool SubsumeType(int,int);
#endif

LOCALE long FindMethodByIndex(DEFGENERIC *,long);
#if DEBUGGING_FUNCTIONS
LOCALE void PreviewGeneric(void *,EXEC_STATUS);
LOCALE void PrintMethod(void *,EXEC_STATUS,char *,int,DEFMETHOD *);
#endif
LOCALE DEFGENERIC *CheckGenericExists(void *,EXEC_STATUS,char *,char *);
LOCALE long CheckMethodExists(void *,EXEC_STATUS,char *,DEFGENERIC *,long);

#if ! OBJECT_SYSTEM
LOCALE char *TypeName(void *,EXEC_STATUS,int);
#endif

LOCALE void PrintGenericName(void *,EXEC_STATUS,char *,DEFGENERIC *);

#endif






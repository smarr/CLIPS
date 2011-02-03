   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.26  06/05/06            */
   /*                                                     */
   /*               DEFTEMPLATE HEADER FILE               */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*      6.23: Added support for templates maintaining their  */
/*            own list of facts.                             */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#ifndef _H_tmpltdef
#define _H_tmpltdef

struct deftemplate;
struct templateSlot;
struct deftemplateModule;

#ifndef _H_conscomp
#include "conscomp.h"
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
#ifndef _H_constrct
#include "constrct.h"
#endif
#ifndef _H_moduldef
#include "moduldef.h"
#endif
#ifndef _H_constrnt
#include "constrnt.h"
#endif
#include "factbld.h"
#ifndef _H_factmngr
#include "fact/fact_manager.h"
#endif
#ifndef _H_cstrccom
#include "cstrccom.h"
#endif

struct deftemplate
  {
   struct constructHeader header;
   struct templateSlot *slotList;
   unsigned int implied       : 1;
   unsigned int watch         : 1;
   unsigned int inScope       : 1;
   unsigned short numberOfSlots;
   long busyCount;
   struct factPatternNode *patternNetwork;
   struct fact *factList;
   struct fact *lastFact;
  };

struct templateSlot
  {
   struct symbolHashNode *slotName;
   unsigned int multislot : 1;
   unsigned int noDefault : 1;
   unsigned int defaultPresent : 1;
   unsigned int defaultDynamic : 1;
   CONSTRAINT_RECORD *constraints;
   struct expr *defaultList;
   struct expr *facetList;
   struct templateSlot *next;
  };

struct deftemplateModule
  {
   struct defmoduleItemHeader header;
  };

#define DEFTEMPLATE_DATA 5

struct deftemplateData
  { 
   struct construct *DeftemplateConstruct;
   int DeftemplateModuleIndex;
   struct entityRecord DeftemplatePtrRecord;
#if DEBUGGING_FUNCTIONS
   int DeletedTemplateDebugFlags;
#endif
#if CONSTRUCT_COMPILER && (! RUN_TIME)
   struct CodeGeneratorItem *DeftemplateCodeItem;
#endif
#if (! RUN_TIME) && (! BLOAD_ONLY)
   int DeftemplateError;
#endif
  };

#define EnvGetDeftemplateName(theEnv,execStatus,x) GetConstructNameString((struct constructHeader *) x)
#define EnvGetDeftemplatePPForm(theEnv,execStatus,x) GetConstructPPForm(theEnv,execStatus,(struct constructHeader *) x)
#define EnvDeftemplateModule(theEnv,execStatus,x) GetConstructModuleName((struct constructHeader *) x)
#define DeftemplateData(theEnv,execStatus) ((struct deftemplateData *) GetEnvironmentData(theEnv,execStatus,DEFTEMPLATE_DATA))

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _TMPLTDEF_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define FindDeftemplate(a) EnvFindDeftemplate(GetCurrentEnvironment(),getCurrentExecutionState(),a)
#define GetNextDeftemplate(a) EnvGetNextDeftemplate(GetCurrentEnvironment(),getCurrentExecutionState(),a)
#define IsDeftemplateDeletable(a) EnvIsDeftemplateDeletable(GetCurrentEnvironment(),getCurrentExecutionState(),a)
#define GetDeftemplateName(x) GetConstructNameString((struct constructHeader *) x)
#define GetDeftemplatePPForm(x) GetConstructPPForm(GetCurrentEnvironment(),getCurrentExecutionState(),(struct constructHeader *) x)
#define GetNextFactInTemplate(a,b) EnvGetNextFactInTemplate(GetCurrentEnvironment(),getCurrentExecutionState(),a,b)
#define DeftemplateModule(x) GetConstructModuleName((struct constructHeader *) x)

   LOCALE void                           InitializeDeftemplates(void *,EXEC_STATUS);
   LOCALE void                          *EnvFindDeftemplate(void *,EXEC_STATUS,char *);
   LOCALE void                          *EnvGetNextDeftemplate(void *,EXEC_STATUS,void *);
   LOCALE intBool                        EnvIsDeftemplateDeletable(void *,EXEC_STATUS,void *);
   LOCALE void                          *EnvGetNextFactInTemplate(void *,EXEC_STATUS,void *,void *);
   LOCALE struct deftemplateModule      *GetDeftemplateModuleItem(void *,EXEC_STATUS,struct defmodule *);
   LOCALE void                           ReturnSlots(void *,EXEC_STATUS,struct templateSlot *);
   LOCALE void                           IncrementDeftemplateBusyCount(void *,EXEC_STATUS,void *);
   LOCALE void                           DecrementDeftemplateBusyCount(void *,EXEC_STATUS,void *);
   LOCALE void                          *CreateDeftemplateScopeMap(void *,EXEC_STATUS,struct deftemplate *);

#if RUN_TIME
   LOCALE void                           DeftemplateRunTimeInitialize(void *,EXEC_STATUS);
#endif
   
#endif



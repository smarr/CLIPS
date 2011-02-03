   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*                DEFGLOBAL HEADER FILE                */
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
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#ifndef _H_globldef
#define _H_globldef

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
#ifndef _H_cstrccom
#include "cstrccom.h"
#endif

#define DEFGLOBAL_DATA 1

struct defglobalData
  { 
   struct construct *DefglobalConstruct;
   int DefglobalModuleIndex;  
   int ChangeToGlobals;   
   intBool ResetGlobals;
   struct entityRecord GlobalInfo;
   struct entityRecord DefglobalPtrRecord;
   long LastModuleIndex;
   struct defmodule *TheDefmodule;
#if CONSTRUCT_COMPILER && (! RUN_TIME)
   struct CodeGeneratorItem *DefglobalCodeItem;
#endif
  };

struct defglobal
  {
   struct constructHeader header;
   unsigned int watch   : 1;
   unsigned int inScope : 1;
   long busyCount;
   DATA_OBJECT current;
   struct expr *initial;
  };

struct defglobalModule
  {
   struct defmoduleItemHeader header;
  };

#define EnvGetDefglobalName(theEnv,execStatus,x) GetConstructNameString((struct constructHeader *) x)
#define EnvGetDefglobalPPForm(theEnv,execStatus,x) GetConstructPPForm(theEnv,execStatus,(struct constructHeader *) x)
#define EnvDefglobalModule(theEnv,execStatus,x) GetConstructModuleName((struct constructHeader *) x)

#define DefglobalData(theEnv,execStatus) ((struct defglobalData *) GetEnvironmentData(theEnv,execStatus,DEFGLOBAL_DATA))

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _GLOBLDEF_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define DefglobalModule(x) GetConstructModuleName((struct constructHeader *) x)
#define FindDefglobal(a) EnvFindDefglobal(GetCurrentEnvironment(),getCurrentExecutionState(),a)
#define GetDefglobalName(x) GetConstructNameString((struct constructHeader *) x)
#define GetDefglobalPPForm(x) GetConstructPPForm(GetCurrentEnvironment(),getCurrentExecutionState(),(struct constructHeader *) x)
#define GetDefglobalValue(a,b) EnvGetDefglobalValue(GetCurrentEnvironment(),getCurrentExecutionState(),a,b)
#define GetDefglobalValueForm(a,b,c) EnvGetDefglobalValueForm(GetCurrentEnvironment(),getCurrentExecutionState(),a,b,c)
#define GetGlobalsChanged() EnvGetGlobalsChanged(GetCurrentEnvironment(),getCurrentExecutionState())
#define GetNextDefglobal(a) EnvGetNextDefglobal(GetCurrentEnvironment(),getCurrentExecutionState(),a)
#define IsDefglobalDeletable(a) EnvIsDefglobalDeletable(GetCurrentEnvironment(),getCurrentExecutionState(),a)
#define SetDefglobalValue(a,b) EnvSetDefglobalValue(GetCurrentEnvironment(),getCurrentExecutionState(),a,b)
#define SetGlobalsChanged(a) EnvSetGlobalsChanged(GetCurrentEnvironment(),getCurrentExecutionState(),a)

   LOCALE void                           InitializeDefglobals(void *,EXEC_STATUS);
   LOCALE void                          *EnvFindDefglobal(void *,EXEC_STATUS,char *);
   LOCALE void                          *EnvGetNextDefglobal(void *,EXEC_STATUS,void *);
   LOCALE void                           CreateInitialFactDefglobal(void);
   LOCALE intBool                        EnvIsDefglobalDeletable(void *,EXEC_STATUS,void *);
   LOCALE struct defglobalModule        *GetDefglobalModuleItem(void *,EXEC_STATUS,struct defmodule *);
   LOCALE void                           QSetDefglobalValue(void *,EXEC_STATUS,struct defglobal *,DATA_OBJECT_PTR,int);
   LOCALE struct defglobal              *QFindDefglobal(void *,EXEC_STATUS,struct symbolHashNode *);
   LOCALE void                           EnvGetDefglobalValueForm(void *,EXEC_STATUS,char *,unsigned,void *);
   LOCALE int                            EnvGetGlobalsChanged(void *,EXEC_STATUS);
   LOCALE void                           EnvSetGlobalsChanged(void *,EXEC_STATUS,int);
   LOCALE intBool                        EnvGetDefglobalValue(void *,EXEC_STATUS,char *,DATA_OBJECT_PTR);
   LOCALE intBool                        EnvSetDefglobalValue(void *,EXEC_STATUS,char *,DATA_OBJECT_PTR);
   LOCALE void                           UpdateDefglobalScope(void *,EXEC_STATUS);
   LOCALE void                          *GetNextDefglobalInScope(void *,EXEC_STATUS,void *);
   LOCALE int                            QGetDefglobalValue(void *,EXEC_STATUS,void *,DATA_OBJECT_PTR);

#ifndef _GLOBLDEF_SOURCE_
#endif

#endif



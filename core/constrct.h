   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*                  CONSTRUCT MODULE                   */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#ifndef _H_constrct

#define _H_constrct

struct constructHeader;
struct construct;

#ifndef _H_moduldef
#include "moduldef.h"
#endif
#ifndef _H_symbol
#include "symbol.h"
#endif

#include "userdata.h"

struct constructHeader
  {
   struct symbolHashNode *name;
   char *ppForm;
   struct defmoduleItemHeader *whichModule;
   long bsaveID;
   struct constructHeader *next;
   struct userData *usrData;
  };

#define CHS (struct constructHeader *)

struct construct
  {
   char *constructName;
   char *pluralName;
   int (*parseFunction)(void *,EXEC_STATUS,char *);
   void *(*findFunction)(void *,EXEC_STATUS,char *);
   struct symbolHashNode *(*getConstructNameFunction)(struct constructHeader *);
   char *(*getPPFormFunction)(void *,EXEC_STATUS,struct constructHeader *);
   struct defmoduleItemHeader *(*getModuleItemFunction)(struct constructHeader *);
   void *(*getNextItemFunction)(void *,EXEC_STATUS,void *);
   void (*setNextItemFunction)(struct constructHeader *,struct constructHeader *);
   intBool (*isConstructDeletableFunction)(void *,EXEC_STATUS,void *);
   int (*deleteFunction)(void *,EXEC_STATUS,void *);
   void (*freeFunction)(void *,EXEC_STATUS,void *);
   struct construct *next;
  };

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_scanner
#include "scanner.h"
#endif

#define CONSTRUCT_DATA 42

struct constructData
  { 
   int ClearReadyInProgress;
   int ClearInProgress;
   int ResetReadyInProgress;
   int ResetInProgress;
#if (! RUN_TIME) && (! BLOAD_ONLY)
   struct callFunctionItem   *ListOfSaveFunctions;
   intBool PrintWhileLoading;
   unsigned WatchCompilations;
#endif
   struct construct *ListOfConstructs;
   struct callFunctionItem *ListOfResetFunctions;
   struct callFunctionItem *ListOfClearFunctions;
   struct callFunctionItem *ListOfClearReadyFunctions;
   int Executing;
   int (*BeforeResetFunction)(void *);
   int CheckSyntaxMode;
   int ParsingConstruct;
  };

#define ConstructData(theEnv,execStatus) ((struct constructData *) GetEnvironmentData(theEnv,execStatus,CONSTRUCT_DATA))

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _CONSTRCT_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define RemoveClearFunction(a) EnvRemoveClearFunction(GetCurrentEnvironment(),a)
#define RemoveResetFunction(a) EnvRemoveResetFunction(GetCurrentEnvironment(),a)

#if ALLOW_ENVIRONMENT_GLOBALS
   LOCALE void                           Clear(EXEC_STATUS);
   LOCALE void                           Reset(EXEC_STATUS);
   LOCALE int                            Save(char *);
#endif

   LOCALE void                           EnvClear(void *,EXEC_STATUS);
   LOCALE void                           EnvReset(void *,EXEC_STATUS);
   LOCALE int                            EnvSave(void *,EXEC_STATUS,char *);

   LOCALE void                           InitializeConstructData(void *,EXEC_STATUS);
   LOCALE intBool                        AddSaveFunction(void *,EXEC_STATUS,char *,void (*)(void *,EXEC_STATUS,void *,char *),int);
   LOCALE intBool                        RemoveSaveFunction(void *,EXEC_STATUS,char *);
   LOCALE intBool                        EnvAddResetFunction(void *,EXEC_STATUS,char *,void (*)(void *),int);
   LOCALE intBool                        AddResetFunction(char *,void (*)(void),int);
   LOCALE intBool                        EnvRemoveResetFunction(void *,EXEC_STATUS,char *);
   LOCALE intBool                        AddClearReadyFunction(void *,EXEC_STATUS,char *,int (*)(void *),int);
   LOCALE intBool                        RemoveClearReadyFunction(void *,EXEC_STATUS,char *);
   LOCALE intBool                        EnvAddClearFunction(void *,EXEC_STATUS,char *,void (*)(void *),int);
   LOCALE intBool                        AddClearFunction(char *,void (*)(void),int);
   LOCALE intBool                        EnvRemoveClearFunction(void *,EXEC_STATUS,char *);
   LOCALE struct construct              *AddConstruct(void *,EXEC_STATUS,char *,char *,
                                                      int (*)(void *,EXEC_STATUS,char *),
                                                      void *(*)(void *,EXEC_STATUS,char *),
                                                      SYMBOL_HN *(*)(struct constructHeader *),
                                                      char *(*)(void *,EXEC_STATUS,struct constructHeader *),
                                                      struct defmoduleItemHeader *(*)(struct constructHeader *),
                                                      void *(*)(void *,EXEC_STATUS,void *),
                                                      void (*)(struct constructHeader *,struct constructHeader *),
                                                      intBool (*)(void *,EXEC_STATUS,void *),
                                                      int (*)(void *,EXEC_STATUS,void *),
                                                      void (*)(void *,EXEC_STATUS,void *));
   LOCALE int                            RemoveConstruct(void *,EXEC_STATUS,char *);
   LOCALE void                           SetCompilationsWatch(void *,EXEC_STATUS,unsigned);
   LOCALE unsigned                       GetCompilationsWatch(void *,EXEC_STATUS);
   LOCALE void                           SetPrintWhileLoading(void *,EXEC_STATUS,intBool);
   LOCALE intBool                        GetPrintWhileLoading(void *,EXEC_STATUS);
   LOCALE int                            ExecutingConstruct(void *,EXEC_STATUS);
   LOCALE void                           SetExecutingConstruct(void *,EXEC_STATUS,int);
   LOCALE void                           InitializeConstructs(void *,EXEC_STATUS);
   LOCALE int                          (*SetBeforeResetFunction(void *,EXEC_STATUS,int (*)(void *)))(void *);
   LOCALE void                           OldGetConstructList(void *,EXEC_STATUS,DATA_OBJECT *,
                                                          void *(*)(void *,EXEC_STATUS,void *),
                                                          char *(*)(void *,EXEC_STATUS,void *));
   LOCALE void                           ResetCommand(void *,EXEC_STATUS);
   LOCALE void                           ClearCommand(void *,EXEC_STATUS);
   LOCALE intBool                        ClearReady(void *,EXEC_STATUS);
   LOCALE struct construct              *FindConstruct(void *,EXEC_STATUS,char *);
   LOCALE void                           DeinstallConstructHeader(void *,EXEC_STATUS,struct constructHeader *);
   LOCALE void                           DestroyConstructHeader(void *,EXEC_STATUS,struct constructHeader *);

#endif








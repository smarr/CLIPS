   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
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
   int (*parseFunction)(char *);
   void *(*findFunction)(char *);
   struct symbolHashNode *(*getConstructNameFunction)(struct constructHeader *);
   char *(*getPPFormFunction)(struct constructHeader *);
   struct defmoduleItemHeader *(*getModuleItemFunction)(struct constructHeader *);
   void *(*getNextItemFunction)(void *);
   void (*setNextItemFunction)(struct constructHeader *,struct constructHeader *);
   BOOLEAN (*isConstructDeletableFunction)(void *);
   int (*deleteFunction)(void *);
   void (*freeFunction)(void *);
   struct construct *next;
  };

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_scanner
#include "scanner.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _CONSTRCT_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE int                            Save(char *);
   LOCALE BOOLEAN                        AddSaveFunction(char *,void (*)(char *),int);
   LOCALE BOOLEAN                        RemoveSaveFunction(char *);
   LOCALE DllExport void                 Reset(void);
   LOCALE DllExport BOOLEAN              AddResetFunction(char *,void (*)(void),int);
   LOCALE BOOLEAN                        RemoveResetFunction(char *);
   LOCALE DllExport void                 Clear(void);
   LOCALE BOOLEAN                        AddClearReadyFunction(char *,int (*)(void),int);
   LOCALE BOOLEAN                        RemoveClearReadyFunction(char *);
   LOCALE BOOLEAN                        AddClearFunction(char *,void (*)(void),int);
   LOCALE BOOLEAN                        RemoveClearFunction(char *);
   LOCALE struct construct              *AddConstruct(char *,char *,int (*)(char *),
                                                      void *(*)(char *),
                                                      SYMBOL_HN *(*)(struct constructHeader *),
                                                      char *(*)(struct constructHeader *),
                                                      struct defmoduleItemHeader *(*)(struct constructHeader *),
                                                      void *(*)(void *),
                                                      void (*)(struct constructHeader *,struct constructHeader *),
                                                      BOOLEAN (*)(void *),
                                                      int (*)(void *),
                                                      void (*)(void *));
   LOCALE int                            RemoveConstruct(char *);
   LOCALE void                           SetCompilationsWatch(int);
   LOCALE BOOLEAN                        GetCompilationsWatch(void);
   LOCALE DllExport void                 SetPrintWhileLoading(BOOLEAN);
   LOCALE BOOLEAN                        GetPrintWhileLoading(void);
   LOCALE int                            ExecutingConstruct(void);
   LOCALE void                           SetExecutingConstruct(int);
   LOCALE void                           InitializeConstructs(void);
   LOCALE int                          (*SetBeforeResetFunction(int (*)(void)))(void);
   LOCALE void                           OldGetConstructList(DATA_OBJECT *,
                                                          void *(*)(void *),
                                                          char *(*)(void *));
   LOCALE void                           ResetCommand(void);
   LOCALE void                           ClearCommand(void);
   LOCALE BOOLEAN                        ClearReady(void);
   LOCALE struct construct              *FindConstruct(char *);
   LOCALE void                           DeinstallConstructHeader(struct constructHeader *);

#ifndef _CONSTRCT_SOURCE_
   extern Thread int                            ClearInProgress;
   extern Thread int                            ClearReadyInProgress;
   extern Thread int                            ResetInProgress;
   extern Thread int                            ResetReadyInProgress;
#endif

#endif


   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*              DEFFUNCTION HEADER FILE                */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Donnell                                     */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_dffnxfun
#define _H_dffnxfun

#define GetDeffunctionName(x) GetConstructNameString((struct constructHeader *) x)
#define GetDeffunctionPPForm(x) GetConstructPPForm((struct constructHeader *) x)

#define GetDeffunctionNamePointer(x) GetConstructNamePointer((struct constructHeader *) x)
#define SetDeffunctionPPForm(d,ppf) SetConstructPPForm((struct constructHeader *) d,ppf)

#define DeffunctionModule(x) GetConstructModuleName((struct constructHeader *) x)

typedef struct deffunctionStruct DEFFUNCTION;
typedef struct deffunctionModule DEFFUNCTION_MODULE;

#ifndef _H_cstrccom
#include "cstrccom.h"
#endif
#ifndef _H_moduldef
#include "moduldef.h"
#endif
#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_symbol
#include "symbol.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif
#ifdef _DFFNXFUN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

struct deffunctionModule
  {
   struct defmoduleItemHeader header;
  };

struct deffunctionStruct
  {
   struct constructHeader header;
   unsigned busy,
            executing;
   unsigned short trace;
   EXPRESSION *code;
   int minNumberOfParameters,
       maxNumberOfParameters,
       numberOfLocalVars;
  };

LOCALE void SetupDeffunctions(void);
LOCALE void *FindDeffunction(char *);
LOCALE DEFFUNCTION *LookupDeffunctionByMdlOrScope(char *);
LOCALE DEFFUNCTION *LookupDeffunctionInScope(char *);
LOCALE BOOLEAN Undeffunction(void *);
LOCALE void *GetNextDeffunction(void *);
LOCALE int IsDeffunctionDeletable(void *);
LOCALE void UndeffunctionCommand(void);
LOCALE SYMBOL_HN *GetDeffunctionModuleCommand(void);
LOCALE void DeffunctionGetBind(DATA_OBJECT *);
LOCALE void DFRtnUnknown(DATA_OBJECT *);
LOCALE void DFWildargs(DATA_OBJECT *);
LOCALE int CheckDeffunctionCall(void *,int);
#if DEBUGGING_FUNCTIONS
LOCALE void PPDeffunctionCommand(void);
LOCALE void ListDeffunctionsCommand(void);
LOCALE void ListDeffunctions(char *,struct defmodule *);
LOCALE void SetDeffunctionWatch(int,void *);
LOCALE int GetDeffunctionWatch(void *);
#endif
#if (! BLOAD_ONLY) && (! RUN_TIME)
LOCALE void RemoveDeffunction(void *);
#endif

LOCALE void GetDeffunctionListFunction(DATA_OBJECT *);
globle void GetDeffunctionList(DATA_OBJECT *,struct defmodule *);

#ifndef _DFFNXFUN_SOURCE_
extern Thread struct construct *DeffunctionConstruct;
extern Thread int DeffunctionModuleIndex;

#if DEBUGGING_FUNCTIONS
extern Thread BOOLEAN WatchDeffunctions;
#endif

#endif

#endif







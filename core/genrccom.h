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

#ifndef _H_genrccom
#define _H_genrccom

#define GetDefgenericName(x) GetConstructNameString((struct constructHeader *) x)
#define GetDefgenericPPForm(x) GetConstructPPForm((struct constructHeader *) x)

#define SetNextDefgeneric(g,t) SetNextConstruct((struct constructHeader *) g, \
                                                (struct constructHeader *) t)
#define GetDefgenericNamePointer(x) GetConstructNamePointer((struct constructHeader *) x)
#define SetDefgenericPPForm(g,ppf) SetConstructPPForm((struct constructHeader *) g,ppf)

#define DefgenericModule(x) GetConstructModuleName((struct constructHeader *) x)

#ifndef _H_constrct
#include "constrct.h"
#endif
#ifndef _H_cstrccom
#include "cstrccom.h"
#endif
#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_moduldef
#include "moduldef.h"
#endif
#ifndef _H_genrcfun
#include "genrcfun.h"
#endif
#ifndef _H_symbol
#include "symbol.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _GENRCCOM_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

LOCALE void SetupGenericFunctions(void);
LOCALE void *FindDefgeneric(char *);
LOCALE DEFGENERIC *LookupDefgenericByMdlOrScope(char *);
LOCALE DEFGENERIC *LookupDefgenericInScope(char *);
LOCALE void *GetNextDefgeneric(void *);
LOCALE unsigned GetNextDefmethod(void *,unsigned);
LOCALE int IsDefgenericDeletable(void *);
LOCALE int IsDefmethodDeletable(void *,unsigned);
LOCALE void UndefgenericCommand(void);
LOCALE SYMBOL_HN *GetDefgenericModuleCommand(void);
LOCALE void UndefmethodCommand(void);
LOCALE DEFMETHOD *GetDefmethodPointer(void *,unsigned);

LOCALE BOOLEAN Undefgeneric(void *);
LOCALE BOOLEAN Undefmethod(void *,unsigned);

#if ! OBJECT_SYSTEM
LOCALE void TypeCommand(DATA_OBJECT *);
#endif

#if DEBUGGING_FUNCTIONS
LOCALE void GetDefmethodDescription(char *,int,void *,unsigned);
LOCALE BOOLEAN GetDefgenericWatch(void *);
LOCALE void SetDefgenericWatch(int,void *);
LOCALE BOOLEAN GetDefmethodWatch(void *,unsigned);
LOCALE void SetDefmethodWatch(int,void *,unsigned);
LOCALE void PPDefgenericCommand(void);
LOCALE void PPDefmethodCommand(void);
LOCALE void ListDefmethodsCommand(void);
LOCALE char *GetDefmethodPPForm(void *,unsigned);
LOCALE void ListDefgenericsCommand(void);
LOCALE void ListDefgenerics(char *,struct defmodule *);
LOCALE void ListDefmethods(char *,void *);
#endif

LOCALE void GetDefgenericListFunction(DATA_OBJECT *);
globle void GetDefgenericList(DATA_OBJECT *,struct defmodule *);
LOCALE void GetDefmethodListCommand(DATA_OBJECT *);
LOCALE void GetDefmethodList(void *,DATA_OBJECT *);
LOCALE void GetMethodRestrictionsCommand(DATA_OBJECT *);
LOCALE void GetMethodRestrictions(void *,unsigned,DATA_OBJECT *);

#ifndef _GENRCCOM_SOURCE_
extern Thread struct construct *DefgenericConstruct;
extern Thread int DefgenericModuleIndex;
#endif

#endif






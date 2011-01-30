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

#ifndef _H_classcom
#define _H_classcom

#define GetDefclassName(x) GetConstructNameString((struct constructHeader *) x)
#define GetDefclassPPForm(x) GetConstructPPForm((struct constructHeader *) x)

#define GetDefclassNamePointer(x) GetConstructNamePointer((struct constructHeader *) x)
#define GetDefclassModule(x) GetConstructModuleItem((struct constructHeader *) x)

#define SetNextDefclass(c,t) SetNextConstruct((struct constructHeader *) c, \
                                              (struct constructHeader *) t)

#define SetDefclassPPForm(c,ppf) SetConstructPPForm((struct constructHeader *) c,ppf)

#define DefclassModule(x) GetConstructModuleName((struct constructHeader *) x)

#ifndef _H_cstrccom
#include "cstrccom.h"
#endif
#ifndef _H_moduldef
#include "moduldef.h"
#endif
#ifndef _H_object
#include "object.h"
#endif
#ifndef _H_symbol
#include "symbol.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _CLASSCOM_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

LOCALE void *FindDefclass(char *);
LOCALE DEFCLASS *LookupDefclassByMdlOrScope(char *);
LOCALE DEFCLASS *LookupDefclassInScope(char *);
LOCALE DEFCLASS *LookupDefclassAnywhere(struct defmodule *,char *);
LOCALE BOOLEAN DefclassInScope(DEFCLASS *,struct defmodule *);
LOCALE DllExport void *GetNextDefclass(void *);
LOCALE BOOLEAN IsDefclassDeletable(void *);

LOCALE void UndefclassCommand(void);

#if DEBUGGING_FUNCTIONS
LOCALE void PPDefclassCommand(void);
LOCALE void ListDefclassesCommand(void);
LOCALE void ListDefclasses(char *,struct defmodule *);
LOCALE BOOLEAN GetDefclassWatchInstances(void *);
LOCALE void SetDefclassWatchInstances(int,void *);
LOCALE BOOLEAN GetDefclassWatchSlots(void *);
LOCALE void SetDefclassWatchSlots(int,void *);
LOCALE BOOLEAN DefclassWatchAccess(int,int,EXPRESSION *);
LOCALE BOOLEAN DefclassWatchPrint(char *,int,EXPRESSION *);
#endif

LOCALE void GetDefclassListFunction(DATA_OBJECT *);
LOCALE void GetDefclassList(DATA_OBJECT *,struct defmodule *);
LOCALE BOOLEAN Undefclass(void *);
LOCALE BOOLEAN HasSuperclass(DEFCLASS *,DEFCLASS *);

LOCALE SYMBOL_HN *CheckClassAndSlot(char *,DEFCLASS **);

#if (! BLOAD_ONLY) && (! RUN_TIME)
LOCALE void SaveDefclasses(char *);
#endif

#endif


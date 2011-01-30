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

#ifndef _H_defins
#define _H_defins

#if DEFINSTANCES_CONSTRUCT

#define GetDefinstancesName(x) GetConstructNameString((struct constructHeader *) x)
#define GetDefinstancesPPForm(x) GetConstructPPForm((struct constructHeader *) x)

#define GetDefinstancesNamePointer(x) GetConstructNamePointer((struct constructHeader *) x)
#define SetDefinstancesPPForm(d,ppf) SetConstructPPForm((struct constructHeader *) d,ppf)

#define GetDefinstancesModuleName(x) GetConstructModuleName((struct constructHeader *) x)

struct definstances;

#ifndef _H_constrct
#include "constrct.h"
#endif
#ifndef _H_cstrccom
#include "cstrccom.h"
#endif
#ifndef _H_moduldef
#include "moduldef.h"
#endif
#ifndef _H_object
#include "object.h"
#endif

typedef struct definstancesModule
  {
   struct defmoduleItemHeader header;
  } DEFINSTANCES_MODULE;

typedef struct definstances
  {
   struct constructHeader header;
   unsigned busy;
   EXPRESSION *mkinstance;
  } DEFINSTANCES;

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _DEFINS_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

LOCALE void SetupDefinstances(void);
LOCALE void *GetNextDefinstances(void *);
LOCALE void *FindDefinstances(char *);
LOCALE int IsDefinstancesDeletable(void *);
LOCALE void UndefinstancesCommand(void);
LOCALE SYMBOL_HN *GetDefinstancesModuleCommand(void);
LOCALE BOOLEAN Undefinstances(void *);

#if DEBUGGING_FUNCTIONS
LOCALE void PPDefinstancesCommand(void);
LOCALE void ListDefinstancesCommand(void);
LOCALE void ListDefinstances(char *,struct defmodule *);
#endif

LOCALE void GetDefinstancesListFunction(DATA_OBJECT *);
LOCALE void GetDefinstancesList(DATA_OBJECT *,struct defmodule *);

#ifndef _DEFINS_SOURCE_
extern Thread int DefinstancesModuleIndex;
#endif

#endif

#endif






   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
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
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_tmpltdef
#define _H_tmpltdef

struct deftemplate;
struct templateSlot;
struct deftemplateModule;

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
#include "factmngr.h"
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
   unsigned int numberOfSlots : 13;
   long busyCount;
   struct factPatternNode *patternNetwork;
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
   struct templateSlot *next;
  };

struct deftemplateModule
  {
   struct defmoduleItemHeader header;
  };

#define GetDeftemplateName(x) GetConstructNameString((struct constructHeader *) x)
#define GetDeftemplatePPForm(x) GetConstructPPForm((struct constructHeader *) x)
#define DeftemplateModule(x) GetConstructModuleName((struct constructHeader *) x)

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _TMPLTDEF_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           InitializeDeftemplates(void);
   LOCALE DllExport void                *FindDeftemplate(char *);
   LOCALE void                          *GetNextDeftemplate(void *);
   LOCALE BOOLEAN                        IsDeftemplateDeletable(void *);
   LOCALE struct deftemplateModule      *GetDeftemplateModuleItem(struct defmodule *);
   LOCALE void                           ReturnSlots(struct templateSlot *);

#ifndef _TMPLTDEF_SOURCE_
   extern Thread struct construct              *DeftemplateConstruct;
   extern Thread int                            DeftemplateModuleIndex;
#endif


#endif



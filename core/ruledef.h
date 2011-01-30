   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                 DEFRULE HEADER FILE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Defines basic defrule primitive functions such   */
/*   as allocating and deallocating, traversing, and finding */
/*   defrule data structures.                                */
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

#ifndef _H_ruledef
#define _H_ruledef

#define GetDisjunctIndex(r) ((struct constructHeader *) r)->bsaveID

struct defrule;
struct defruleModule;

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
#ifndef _H_cstrccom
#include "cstrccom.h"
#endif
#ifndef _H_agenda
#include "agenda.h"
#endif
#ifndef _H_network
#include "network.h"
#endif


struct defrule
  {
   struct constructHeader header;
   int salience;
   int localVarCnt;
   unsigned int complexity      : 11;
   unsigned int afterBreakpoint :  1;
   unsigned int watchActivation :  1;
   unsigned int watchFiring     :  1;
   unsigned int autoFocus       :  1;
   unsigned int executing       :  1;
#if DYNAMIC_SALIENCE
   struct expr *dynamicSalience;
#endif
   struct expr *actions;
#if LOGICAL_DEPENDENCIES
   struct joinNode *logicalJoin;
#endif
   struct joinNode *lastJoin;
   struct defrule *disjunct;
  };

struct defruleModule
  {
   struct defmoduleItemHeader header;
   struct activation *agenda;
  };

#define GetDefruleName(x) GetConstructNameString((struct constructHeader *) x)
#define GetDefrulePPForm(x) GetConstructPPForm((struct constructHeader *) x)
#define DefruleModule(x) GetConstructModuleName((struct constructHeader *) x)

#define GetPreviousJoin(theJoin) \
   (((theJoin)->joinFromTheRight) ? \
    ((struct joinNode *) (theJoin)->rightSideEntryStructure) : \
    ((theJoin)->lastLevel))
#define GetPatternForJoin(theJoin) \
   (((theJoin)->joinFromTheRight) ? \
    NULL : \
    ((theJoin)->rightSideEntryStructure))

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _RULEDEF_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           InitializeDefrules(void);
   LOCALE void                          *FindDefrule(char *);
   LOCALE void                          *GetNextDefrule(void *);
   LOCALE struct defruleModule          *GetDefruleModuleItem(struct defmodule *);
   LOCALE BOOLEAN                        IsDefruleDeletable(void *);

#ifndef _RULEDEF_SOURCE_
   extern Thread struct construct              *DefruleConstruct;
   extern Thread int                            DefruleModuleIndex;
   extern Thread long                           CurrentEntityTimeTag;
#endif

#endif



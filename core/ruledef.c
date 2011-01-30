   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                   DEFRULE MODULE                    */
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
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _RULEDEF_SOURCE_

#include "setup.h"

#if DEFRULE_CONSTRUCT

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "memalloc.h"
#include "engine.h"
#include "pattern.h"
#include "rulebsc.h"
#include "rulecom.h"
#include "drive.h"
#include "rulepsr.h"
#include "ruledlt.h"
#include "agenda.h"

#if BLOAD || BLOAD_AND_BSAVE || BLOAD_ONLY
#include "bload.h"
#include "rulebin.h"
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
#include "rulecmp.h"
#endif

#include "ruledef.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                   *AllocateModule(void);
   static void                    ReturnModule(void *);
   static void                    InitializeDefruleModules(void);

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle struct construct       *DefruleConstruct;
   Thread globle int                     DefruleModuleIndex;
   Thread globle long                    CurrentEntityTimeTag = 0L;

/**********************************************************/
/* InitializeDefrules: Initializes the defrule construct. */
/**********************************************************/
globle void InitializeDefrules()
  {
   InitializeEngine();
   InitializeAgenda();

   InitializeDefruleModules();

   AddReservedPatternSymbol("and",NULL);
   AddReservedPatternSymbol("not",NULL);
   AddReservedPatternSymbol("or",NULL);
   AddReservedPatternSymbol("test",NULL);
   AddReservedPatternSymbol("logical",NULL);
   AddReservedPatternSymbol("exists",NULL);
   AddReservedPatternSymbol("forall",NULL);

   DefruleBasicCommands();

   DefruleCommands();

   DefruleConstruct =
      AddConstruct("defrule","defrules",
                   ParseDefrule,FindDefrule,
                   GetConstructNamePointer,GetConstructPPForm,
                   GetConstructModuleItem,GetNextDefrule,SetNextConstruct,
                   IsDefruleDeletable,Undefrule,ReturnDefrule);
  }

/*****************************************************/
/* InitializeDefruleModules: Initializes the defrule */
/*   construct for use with the defmodule construct. */
/*****************************************************/
static void InitializeDefruleModules()
  {
   DefruleModuleIndex = RegisterModuleItem("defrule",
                                    AllocateModule,
                                    ReturnModule,
#if BLOAD_AND_BSAVE || BLOAD || BLOAD_ONLY
                                    BloadDefruleModuleReference,
#else
                                    NULL,
#endif
#if CONSTRUCT_COMPILER && (! RUN_TIME)
                                    DefruleCModuleReference,
#else
                                    NULL,
#endif
                                    FindDefrule);
  }

/***********************************************/
/* AllocateModule: Allocates a defrule module. */
/***********************************************/
static void *AllocateModule()
  {
   struct defruleModule *theItem;

   theItem = get_struct(defruleModule);
   theItem->agenda = NULL;
   return((void *) theItem);
  }

/*********************************************/
/* ReturnModule: Deallocates a defrule module. */
/*********************************************/
static void ReturnModule(
  void *theItem)
  {
   FreeConstructHeaderModule((struct defmoduleItemHeader *) theItem,DefruleConstruct);
   rtn_struct(defruleModule,theItem);
  }

/************************************************************/
/* GetDefruleModuleItem: Returns a pointer to the defmodule */
/*  item for the specified defrule or defmodule.            */
/************************************************************/
globle struct defruleModule *GetDefruleModuleItem(
  struct defmodule *theModule)
  { return((struct defruleModule *) GetConstructModuleItemByIndex(theModule,DefruleModuleIndex)); }

/****************************************************************/
/* FindDefrule: Searches for a defrule in the list of defrules. */
/*   Returns a pointer to the defrule if found, otherwise NULL. */
/****************************************************************/
globle void *FindDefrule(
  char *defruleName)
  { return(FindNamedConstruct(defruleName,DefruleConstruct)); }

/***************************************************************/
/* GetNextDefrule: If passed a NULL pointer, returns the first */
/*   defrule in the ListOfDefrules. Otherwise returns the next */
/*   defrule following the defrule passed as an argument.      */
/***************************************************************/
globle void *GetNextDefrule(
  void *defrulePtr)
  { return((void *) GetNextConstructItem((struct constructHeader *) defrulePtr,DefruleModuleIndex)); }

/******************************************************/
/* IsDefruleDeletable: Returns TRUE if a particular   */
/*   defrule can be deleted, otherwise returns FALSE. */
/******************************************************/
globle BOOLEAN IsDefruleDeletable(
  void *vTheDefrule)
  {
#if BLOAD_ONLY || RUN_TIME
#if MAC_MPW || MAC_MCW
#pragma unused(vTheDefrule)
#endif
   return(FALSE);
#else
   struct defrule *theDefrule;
#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded()) return(FALSE);
#endif

   for (theDefrule = (struct defrule *) vTheDefrule;
        theDefrule != NULL;
        theDefrule = theDefrule->disjunct)
     { if (theDefrule->executing) return(FALSE); }

   if (JoinOperationInProgress) return(FALSE);

   return(TRUE);
#endif
  }

#endif /* DEFRULE_CONSTRUCT */



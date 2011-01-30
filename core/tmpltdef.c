   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                 DEFTEMPLATE MODULE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Defines basic deftemplate primitive functions    */
/*   such as allocating and deallocating, traversing, and    */
/*   finding deftemplate data structures.                    */
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

#define _TMPLTDEF_SOURCE_

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "memalloc.h"
#include "exprnops.h"
#include "cstrccom.h"
#include "network.h"
#include "tmpltpsr.h"
#include "tmpltbsc.h"
#include "tmpltutl.h"
#include "tmpltfun.h"
#include "router.h"
#include "modulpsr.h"
#include "modulutl.h"
#include "cstrnchk.h"

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
#include "bload.h"
#include "tmpltbin.h"
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
#include "tmpltcmp.h"
#endif

#include "tmpltdef.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                   *AllocateModule(void);
   static void                    ReturnModule(void *);
   static void                    ReturnDeftemplate(void *);
   static void                    InitializeDeftemplateModules(void);
   static void                    IncrementDeftemplateBusyCount(void *);
   static void                    DecrementDeftemplateBusyCount(void *);

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle struct construct       *DeftemplateConstruct;
   Thread globle int                     DeftemplateModuleIndex;
   Thread globle struct entityRecord     DeftemplatePtrRecord = { "DEFTEMPLATE_PTR",
                                                           DEFTEMPLATE_PTR,1,0,0,
                                                           NULL,
                                                           NULL,NULL,
                                                           NULL,
                                                           NULL,
                                                           DecrementDeftemplateBusyCount,
                                                           IncrementDeftemplateBusyCount,
                                                           NULL,NULL,NULL,NULL };

/******************************************************************/
/* InitializeDeftemplates: Initializes the deftemplate construct. */
/******************************************************************/
globle void InitializeDeftemplates()
  {
   InitializeFacts();

   InitializeDeftemplateModules();

   DeftemplateBasicCommands();

   DeftemplateFunctions();

   DeftemplateConstruct =
      AddConstruct("deftemplate","deftemplates",ParseDeftemplate,FindDeftemplate,
                   GetConstructNamePointer,GetConstructPPForm,
                   GetConstructModuleItem,GetNextDeftemplate,SetNextConstruct,
                   IsDeftemplateDeletable,Undeftemplate,ReturnDeftemplate);

   InstallPrimitive((ENTITY_RECORD_PTR) &DeftemplatePtrRecord,DEFTEMPLATE_PTR);
  }

/*************************************************************/
/* InitializeDeftemplateModules: Initializes the deftemplate */
/*   construct for use with the defmodule construct.         */
/*************************************************************/
static void InitializeDeftemplateModules()
  {
   DeftemplateModuleIndex = RegisterModuleItem("deftemplate",
                                    AllocateModule,
                                    ReturnModule,
#if BLOAD_AND_BSAVE || BLOAD || BLOAD_ONLY
                                    BloadDeftemplateModuleReference,
#else
                                    NULL,
#endif
#if CONSTRUCT_COMPILER && (! RUN_TIME)
                                    DeftemplateCModuleReference,
#else
                                    NULL,
#endif
                                    FindDeftemplate);

#if (! BLOAD_ONLY) && (! RUN_TIME) && DEFMODULE_CONSTRUCT
   AddPortConstructItem("deftemplate",SYMBOL);
#endif
  }

/***************************************************/
/* AllocateModule: Allocates a deftemplate module. */
/***************************************************/
static void *AllocateModule()
  { return((void *) get_struct(deftemplateModule)); }

/*************************************************/
/* ReturnModule: Deallocates a deftemplate module. */
/*************************************************/
static void ReturnModule(
  void *theItem)
  {
   FreeConstructHeaderModule((struct defmoduleItemHeader *) theItem,DeftemplateConstruct);
   rtn_struct(deftemplateModule,theItem);
  }

/****************************************************************/
/* GetDeftemplateModuleItem: Returns a pointer to the defmodule */
/*  item for the specified deftemplate or defmodule.            */
/****************************************************************/
globle struct deftemplateModule *GetDeftemplateModuleItem(
  struct defmodule *theModule)
  { return((struct deftemplateModule *) GetConstructModuleItemByIndex(theModule,DeftemplateModuleIndex)); }

/***********************************************************/
/* FindDeftemplate: Searches for a deftemplate in the list */
/*   of deftemplates. Returns a pointer to the deftemplate */
/*   if found, otherwise NULL.                             */
/***********************************************************/
globle void *FindDeftemplate(
  char *deftemplateName)
  { return(FindNamedConstruct(deftemplateName,DeftemplateConstruct)); }

/***********************************************************************/
/* GetNextDeftemplate: If passed a NULL pointer, returns the first     */
/*   deftemplate in the ListOfDeftemplates. Otherwise returns the next */
/*   deftemplate following the deftemplate passed as an argument.      */
/***********************************************************************/
globle void *GetNextDeftemplate(
  void *deftemplatePtr)
  { return((void *) GetNextConstructItem((struct constructHeader *) deftemplatePtr,DeftemplateModuleIndex)); }

/**********************************************************/
/* IsDeftemplateDeletable: Returns TRUE if a particular   */
/*   deftemplate can be deleted, otherwise returns FALSE. */
/**********************************************************/
globle BOOLEAN IsDeftemplateDeletable(
  void *vTheDeftemplate)
  {
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(vTheDeftemplate)
#endif

#if BLOAD_ONLY || RUN_TIME
   return(FALSE);
#else
   struct deftemplate *theDeftemplate = (struct deftemplate *) vTheDeftemplate;
#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded()) return(FALSE);
#endif

   if (theDeftemplate->busyCount > 0) return(FALSE);
   if (theDeftemplate->patternNetwork != NULL) return(FALSE);

   return(TRUE);
#endif
  }

/**************************************************************/
/* ReturnDeftemplate: Returns the data structures associated  */
/*   with a deftemplate construct to the pool of free memory. */
/**************************************************************/
static void ReturnDeftemplate(
  void *vTheConstruct)
  {
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(vTheConstruct)
#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)
   struct deftemplate *theConstruct = (struct deftemplate *) vTheConstruct;
   struct templateSlot *slotPtr;

   if (theConstruct == NULL) return;

   /*====================================================================*/
   /* If a template is redefined, then we want to save its debug status. */
   /*====================================================================*/

#if DEBUGGING_FUNCTIONS
   DeletedTemplateDebugFlags = 0;
   if (theConstruct->watch) BitwiseSet(DeletedTemplateDebugFlags,0);
#endif

   /*===========================================*/
   /* Free storage used by the templates slots. */
   /*===========================================*/

   slotPtr = theConstruct->slotList;
   while (slotPtr != NULL)
     {
      DecrementSymbolCount(slotPtr->slotName);
      RemoveHashedExpression(slotPtr->defaultList);
      slotPtr->defaultList = NULL;
      RemoveConstraint(slotPtr->constraints);
      slotPtr->constraints = NULL;
      slotPtr = slotPtr->next;
     }

   ReturnSlots(theConstruct->slotList);

   /*==================================*/
   /* Free storage used by the header. */
   /*==================================*/

   DeinstallConstructHeader(&theConstruct->header);

   rtn_struct(deftemplate,theConstruct);
#endif
  }

/***********************************************/
/* ReturnSlots: Returns the slot structures of */
/*   a deftemplate to free memory.             */
/***********************************************/
globle void ReturnSlots(
  struct templateSlot *slotPtr)
  {
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(slotPtr)
#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)
   struct templateSlot *nextSlot;

   while (slotPtr != NULL)
     {
      nextSlot = slotPtr->next;
      ReturnExpression(slotPtr->defaultList);
      RemoveConstraint(slotPtr->constraints);
      rtn_struct(templateSlot,slotPtr);
      slotPtr = nextSlot;
     }
#endif
  }

/*************************************************/
/* DecrementDeftemplateBusyCount: Decrements the */
/*   busy count of a deftemplate data structure. */
/*************************************************/
static void DecrementDeftemplateBusyCount(
  void *vTheTemplate)
  {
   struct deftemplate *theTemplate = (struct deftemplate *) vTheTemplate;

   if (! ClearInProgress) theTemplate->busyCount--;
  }

/*************************************************/
/* IncrementDeftemplateBusyCount: Increments the */
/*   busy count of a deftemplate data structure. */
/*************************************************/
static void IncrementDeftemplateBusyCount(
  void *vTheTemplate)
  {
   struct deftemplate *theTemplate = (struct deftemplate *) vTheTemplate;

   theTemplate->busyCount++;
  }

#endif /* DEFTEMPLATE_CONSTRUCT */



   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*              DEFFACTS DEFINITION MODULE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: Defines basic deffacts primitive functions such  */
/*   as allocating and deallocating, traversing, and finding */
/*   deffacts data structures.                               */
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

#define _DFFCTDEF_SOURCE_

#include "setup.h"

#if DEFFACTS_CONSTRUCT

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "memalloc.h"
#include "dffctpsr.h"
#include "dffctbsc.h"

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
#include "bload.h"
#include "dffctbin.h"
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
#include "dffctcmp.h"
#endif

#include "dffctdef.h"

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle struct construct       *DeffactsConstruct;
   Thread globle int                     DeffactsModuleIndex;

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                   *AllocateModule(void);
   static void                    ReturnModule(void *);
   static void                    ReturnDeffacts(void *);
   static void                    InitializeDeffactsModules(void);

/***********************************************************/
/* InitializeDeffacts: Initializes the deffacts construct. */
/***********************************************************/
globle void InitializeDeffacts()
  {
   InitializeDeffactsModules();

   DeffactsBasicCommands();

   DeffactsConstruct =
      AddConstruct("deffacts","deffacts",ParseDeffacts,FindDeffacts,
                   GetConstructNamePointer,GetConstructPPForm,
                   GetConstructModuleItem,GetNextDeffacts,SetNextConstruct,
                   IsDeffactsDeletable,Undeffacts,ReturnDeffacts);
  }

/*******************************************************/
/* InitializeDeffactsModules: Initializes the deffacts */
/*   construct for use with the defmodule construct.   */
/*******************************************************/
static void InitializeDeffactsModules()
  {
   DeffactsModuleIndex = RegisterModuleItem("deffacts",
                                    AllocateModule,
                                    ReturnModule,
#if BLOAD_AND_BSAVE || BLOAD || BLOAD_ONLY
                                    BloadDeffactsModuleReference,
#else
                                    NULL,
#endif
#if CONSTRUCT_COMPILER && (! RUN_TIME)
                                    DeffactsCModuleReference,
#else
                                    NULL,
#endif
                                    FindDeffacts);
  }

/************************************************/
/* AllocateModule: Allocates a deffacts module. */
/************************************************/
static void *AllocateModule()
  { return((void *) get_struct(deffactsModule)); }

/**********************************************/
/* ReturnModule: Deallocates a deffacts module. */
/**********************************************/
static void ReturnModule(
  void *theItem)
  {
   FreeConstructHeaderModule((struct defmoduleItemHeader *) theItem,DeffactsConstruct);
   rtn_struct(deffactsModule,theItem);
  }

/*************************************************************/
/* GetDeffactsModuleItem: Returns a pointer to the defmodule */
/*  item for the specified deffacts or defmodule.            */
/*************************************************************/
globle struct deffactsModule *GetDeffactsModuleItem(
  struct defmodule *theModule)
  { return((struct deffactsModule *) GetConstructModuleItemByIndex(theModule,DeffactsModuleIndex)); }

/*****************************************************************/
/* FindDeffacts: Searches for a deffact in the list of deffacts. */
/*   Returns a pointer to the deffact if found, otherwise NULL.  */
/*****************************************************************/
globle void *FindDeffacts(
  char *deffactsName)
  { return(FindNamedConstruct(deffactsName,DeffactsConstruct)); }

/****************************************************************/
/* GetNextDeffacts: If passed a NULL pointer, returns the first */
/*   deffacts in the ListOfDeffacts. Otherwise returns the next */
/*   deffacts following the deffacts passed as an argument.     */
/****************************************************************/
globle void *GetNextDeffacts(
  void *deffactsPtr)
  { return((void *) GetNextConstructItem((struct constructHeader *) deffactsPtr,DeffactsModuleIndex)); }

/*******************************************************/
/* IsDeffactsDeletable: Returns TRUE if a particular   */
/*   deffacts can be deleted, otherwise returns FALSE. */
/*******************************************************/
#if IBM_TBC
#pragma argsused
#endif
globle BOOLEAN IsDeffactsDeletable(
  void *ptr)
  {
#if MAC_MPW || MAC_MCW || IBM_MCW
#pragma unused(ptr)
#endif
#if BLOAD_ONLY || RUN_TIME
   return(FALSE);
#else
#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded()) return(FALSE);
#endif
   if (ResetInProgress) return(FALSE);
   return(TRUE);
#endif
  }

/***********************************************************/
/* ReturnDeffacts: Returns the data structures associated  */
/*   with a deffacts construct to the pool of free memory. */
/***********************************************************/
static void ReturnDeffacts(
  void *vTheDeffacts)
  {
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(vTheDeffacts)
#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)
   struct deffacts *theDeffacts = (struct deffacts *) vTheDeffacts;

   if (theDeffacts == NULL) return;

   ExpressionDeinstall(theDeffacts->assertList);
   ReturnPackedExpression(theDeffacts->assertList);

   DeinstallConstructHeader(&theDeffacts->header);

   rtn_struct(deffacts,theDeffacts);
#endif
  }

#endif /* DEFFACTS_CONSTRUCT */



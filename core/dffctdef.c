   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/02/06            */
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
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Corrected code to remove run-time program      */
/*            compiler warning.                              */
/*                                                           */
/*************************************************************/

#define _DFFCTDEF_SOURCE_

#include "setup.h"

#if DEFFACTS_CONSTRUCT

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "memalloc.h"
#include "dffctpsr.h"
#include "dffctbsc.h"
#include "envrnmnt.h"

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
#include "bload.h"
#include "dffctbin.h"
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
#include "dffctcmp.h"
#endif

#include "dffctdef.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                   *AllocateModule(void *);
   static void                    ReturnModule(void *,void *);
   static void                    ReturnDeffacts(void *,void *);
   static void                    InitializeDeffactsModules(void *);
   static void                    DeallocateDeffactsData(void *);
#if ! RUN_TIME
   static void                    DestroyDeffactsAction(void *,struct constructHeader *,void *);
#endif

/***********************************************************/
/* InitializeDeffacts: Initializes the deffacts construct. */
/***********************************************************/
globle void InitializeDeffacts(  
  void *theEnv,
  EXEC_STATUS)
  {
   AllocateEnvironmentData(theEnv,execStatus,DEFFACTS_DATA,sizeof(struct deffactsData),DeallocateDeffactsData);
  
   InitializeDeffactsModules(theEnv);

   DeffactsBasicCommands(theEnv);

   DeffactsData(theEnv)->DeffactsConstruct =
      AddConstruct(theEnv,execStatus,"deffacts","deffacts",ParseDeffacts,EnvFindDeffacts,
                   GetConstructNamePointer,GetConstructPPForm,
                   GetConstructModuleItem,EnvGetNextDeffacts,SetNextConstruct,
                   EnvIsDeffactsDeletable,EnvUndeffacts,ReturnDeffacts);
  }
  
/***************************************************/
/* DeallocateDeffactsData: Deallocates environment */
/*    data for the deffacts construct.             */
/***************************************************/
static void DeallocateDeffactsData(
  void *theEnv,
  EXEC_STATUS)
  {
#if ! RUN_TIME
   struct deffactsModule *theModuleItem;
   void *theModule;

#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded(theEnv)) return;
#endif

   DoForAllConstructs(theEnv,execStatus,DestroyDeffactsAction,DeffactsData(theEnv)->DeffactsModuleIndex,FALSE,NULL); 

   for (theModule = EnvGetNextDefmodule(theEnv,execStatus,NULL);
        theModule != NULL;
        theModule = EnvGetNextDefmodule(theEnv,execStatus,theModule))
     {
      theModuleItem = (struct deffactsModule *)
                      GetModuleItem(theEnv,execStatus,(struct defmodule *) theModule,
                                    DeffactsData(theEnv)->DeffactsModuleIndex);
      rtn_struct(theEnv,execStatus,deffactsModule,theModuleItem);
     }
#else
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv)
#endif
#endif
  }
  
#if ! RUN_TIME
/*********************************************************/
/* DestroyDeffactsAction: Action used to remove deffacts */
/*   as a result of DestroyEnvironment.                  */
/*********************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void DestroyDeffactsAction(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theConstruct,
  void *buffer)
  {
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(buffer)
#endif
#if (! BLOAD_ONLY) && (! RUN_TIME)
   struct deffacts *theDeffacts = (struct deffacts *) theConstruct;
   
   if (theDeffacts == NULL) return;

   ReturnPackedExpression(theEnv,execStatus,theDeffacts->assertList);
   
   DestroyConstructHeader(theEnv,execStatus,&theDeffacts->header);

   rtn_struct(theEnv,execStatus,deffacts,theDeffacts);
#else
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv,execStatus,theConstruct)
#endif
#endif
  }
#endif

/*******************************************************/
/* InitializeDeffactsModules: Initializes the deffacts */
/*   construct for use with the defmodule construct.   */
/*******************************************************/
static void InitializeDeffactsModules(  
  void *theEnv,
  EXEC_STATUS)
  {
   DeffactsData(theEnv)->DeffactsModuleIndex = 
      RegisterModuleItem(theEnv,execStatus,"deffacts",
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
                         EnvFindDeffacts);
  }

/************************************************/
/* AllocateModule: Allocates a deffacts module. */
/************************************************/
static void *AllocateModule(
  void *theEnv,
  EXEC_STATUS)
  {
   return((void *) get_struct(theEnv,execStatus,deffactsModule)); 
  }

/************************************************/
/* ReturnModule: Deallocates a deffacts module. */
/************************************************/
static void ReturnModule(
  void *theEnv,
  EXEC_STATUS,
  void *theItem)
  {
   FreeConstructHeaderModule(theEnv,execStatus,(struct defmoduleItemHeader *) theItem,DeffactsData(theEnv)->DeffactsConstruct);
   rtn_struct(theEnv,execStatus,deffactsModule,theItem);
  }

/*************************************************************/
/* GetDeffactsModuleItem: Returns a pointer to the defmodule */
/*  item for the specified deffacts or defmodule.            */
/*************************************************************/
globle struct deffactsModule *GetDeffactsModuleItem(
  void *theEnv,
  EXEC_STATUS,
  struct defmodule *theModule)
  { 
   return((struct deffactsModule *) GetConstructModuleItemByIndex(theEnv,execStatus,theModule,DeffactsData(theEnv)->DeffactsModuleIndex)); 
  }

/**************************************************/
/* EnvFindDeffacts: Searches for a deffact in the */
/*   list of deffacts. Returns a pointer to the   */
/*   deffact if found, otherwise NULL.            */
/**************************************************/
globle void *EnvFindDeffacts(
  void *theEnv,
  EXEC_STATUS,
  char *deffactsName)
  { 
   return(FindNamedConstruct(theEnv,execStatus,deffactsName,DeffactsData(theEnv)->DeffactsConstruct)); 
  }

/*********************************************************/
/* EnvGetNextDeffacts: If passed a NULL pointer, returns */
/*   the first deffacts in the ListOfDeffacts. Otherwise */
/*   returns the next deffacts following the deffacts    */
/*   passed as an argument.                              */
/*********************************************************/
globle void *EnvGetNextDeffacts(
  void *theEnv,
  EXEC_STATUS,
  void *deffactsPtr)
  {
   return((void *) GetNextConstructItem(theEnv,execStatus,(struct constructHeader *) deffactsPtr,DeffactsData(theEnv)->DeffactsModuleIndex)); 
  }

/********************************************************/
/* EnvIsDeffactsDeletable: Returns TRUE if a particular */
/*   deffacts can be deleted, otherwise returns FALSE.  */
/********************************************************/
#if WIN_BTC
#pragma argsused
#endif
globle intBool EnvIsDeffactsDeletable(
  void *theEnv,
  EXEC_STATUS,
  void *ptr)
  {
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(ptr)
#endif
   if (! ConstructsDeletable(theEnv))
     { return FALSE; }

   if (ConstructData(theEnv)->ResetInProgress) return(FALSE);

   return(TRUE);
  }

/***********************************************************/
/* ReturnDeffacts: Returns the data structures associated  */
/*   with a deffacts construct to the pool of free memory. */
/***********************************************************/
static void ReturnDeffacts(
  void *theEnv,
  EXEC_STATUS,
  void *vTheDeffacts)
  {
#if (MAC_MCW || WIN_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(theEnv,execStatus,vTheDeffacts)
#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)
   struct deffacts *theDeffacts = (struct deffacts *) vTheDeffacts;

   if (theDeffacts == NULL) return;

   ExpressionDeinstall(theEnv,execStatus,theDeffacts->assertList);
   ReturnPackedExpression(theEnv,execStatus,theDeffacts->assertList);

   DeinstallConstructHeader(theEnv,execStatus,&theDeffacts->header);

   rtn_struct(theEnv,execStatus,deffacts,theDeffacts);
#endif
  }
  
#endif /* DEFFACTS_CONSTRUCT */



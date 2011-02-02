   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.21  06/15/03            */
   /*                                                     */
   /*             DEFMODULE BSAVE/BLOAD MODULE            */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for the  */
/*    defmodule construct.                                   */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _MODULBIN_SOURCE_

#include "setup.h"

#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME)

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "memalloc.h"
#include "constrct.h"
#include "moduldef.h"
#include "bload.h"
#include "bsave.h"
#include "envrnmnt.h"

#include "modulbin.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if BLOAD_AND_BSAVE
   static void                    BsaveFind(void *,EXEC_STATUS);
   static void                    BsaveStorage(void *,EXEC_STATUS,FILE *);
   static void                    BsaveBinaryItem(void *,EXEC_STATUS,FILE *);
#endif
   static void                    BloadStorage(void *,EXEC_STATUS);
   static void                    BloadBinaryItem(void *,EXEC_STATUS);
   static void                    UpdateDefmodule(void *,EXEC_STATUS,void *,long);
   static void                    UpdatePortItem(void *,EXEC_STATUS,void *,long);
   static void                    ClearBload(void *,EXEC_STATUS);

/*********************************************/
/* DefmoduleBinarySetup: Installs the binary */
/*   save/load feature for defmodules.       */
/*********************************************/
globle void DefmoduleBinarySetup(
  void *theEnv,
  EXEC_STATUS)
  {
   AddBeforeBloadFunction(theEnv,execStatus,"defmodule",RemoveAllDefmodules,2000);

#if BLOAD_AND_BSAVE
   AddBinaryItem(theEnv,execStatus,"defmodule",0,BsaveFind,NULL,
                             BsaveStorage,BsaveBinaryItem,
                             BloadStorage,BloadBinaryItem,
                             ClearBload);
#endif

   AddAbortBloadFunction(theEnv,execStatus,"defmodule",CreateMainModule,0);

#if (BLOAD || BLOAD_ONLY)
   AddBinaryItem(theEnv,execStatus,"defmodule",0,NULL,NULL,NULL,NULL,
                             BloadStorage,BloadBinaryItem,
                             ClearBload);
#endif
  }

/**************************************************************/
/* UpdateDefmoduleItemHeader: Updates the values in defmodule */
/*   item headers for the loaded binary image.                */
/**************************************************************/
globle void UpdateDefmoduleItemHeader(
  void *theEnv,
  EXEC_STATUS,
  struct bsaveDefmoduleItemHeader *theBsaveHeader,
  struct defmoduleItemHeader *theHeader,
  int itemSize,
  void *itemArray)
  {
   long firstOffset,lastOffset;

   theHeader->theModule = ModulePointer(theBsaveHeader->theModule);
   if (theBsaveHeader->firstItem == -1L)
     {
      theHeader->firstItem = NULL;
      theHeader->lastItem = NULL;
     }
   else
     {
      firstOffset = itemSize * theBsaveHeader->firstItem;
      lastOffset = itemSize * theBsaveHeader->lastItem;
      theHeader->firstItem =
        (struct constructHeader *) &((char *) itemArray)[firstOffset];
      theHeader->lastItem =
        (struct constructHeader *) &((char *) itemArray)[lastOffset];
     }
  }

#if BLOAD_AND_BSAVE

/*********************************************************/
/* AssignBsaveDefmdlItemHdrVals: Assigns the appropriate */
/*   values to a bsave defmodule item header record.     */
/*********************************************************/
globle void AssignBsaveDefmdlItemHdrVals(
  struct bsaveDefmoduleItemHeader *theBsaveHeader,
  struct defmoduleItemHeader *theHeader)
  {
   theBsaveHeader->theModule = theHeader->theModule->bsaveID;
   if (theHeader->firstItem == NULL)
     {
      theBsaveHeader->firstItem = -1L;
      theBsaveHeader->lastItem = -1L;
     }
   else
     {
      theBsaveHeader->firstItem = theHeader->firstItem->bsaveID;
      theBsaveHeader->lastItem = theHeader->lastItem->bsaveID;
     }
  }

/**********************************************************/
/* BsaveFind: Counts the number of data structures which  */
/*   must be saved in the binary image for the defmodules */
/*   in the current environment.                          */
/**********************************************************/
static void BsaveFind(
  void *theEnv,
  EXEC_STATUS)
  {
   struct defmodule *defmodulePtr;
   struct portItem *theList;

   /*=======================================================*/
   /* If a binary image is already loaded, then temporarily */
   /* save the count values since these will be overwritten */
   /* in the process of saving the binary image.            */
   /*=======================================================*/

   SaveBloadCount(theEnv,execStatus,DefmoduleData(theEnv,execStatus)->BNumberOfDefmodules);
   SaveBloadCount(theEnv,execStatus,DefmoduleData(theEnv,execStatus)->NumberOfPortItems);

   /*==========================================*/
   /* Set the count of defmodule and defmodule */
   /* port items data structures to zero.      */
   /*==========================================*/

   DefmoduleData(theEnv,execStatus)->BNumberOfDefmodules = 0;
   DefmoduleData(theEnv,execStatus)->NumberOfPortItems = 0;

   /*===========================*/
   /* Loop through each module. */
   /*===========================*/

   for (defmodulePtr = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,NULL);
        defmodulePtr != NULL;
        defmodulePtr = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,defmodulePtr))
     {
      /*==============================================*/
      /* Increment the number of modules encountered. */
      /*==============================================*/

      DefmoduleData(theEnv,execStatus)->BNumberOfDefmodules++;

      /*===========================*/
      /* Mark the defmodule's name */
      /* as being a needed symbol. */
      /*===========================*/

      defmodulePtr->name->neededSymbol = TRUE;

      /*==============================================*/
      /* Loop through each of the port items in the   */
      /* defmodule's import list incrementing the     */
      /* number of port items encountered and marking */
      /* needed symbols.                              */
      /*==============================================*/

      for (theList = defmodulePtr->importList;
           theList != NULL;
           theList = theList->next)
        {
         DefmoduleData(theEnv,execStatus)->NumberOfPortItems++;
         if (theList->moduleName != NULL)
           { theList->moduleName->neededSymbol = TRUE; }
         if (theList->constructType != NULL)
           { theList->constructType->neededSymbol = TRUE; }
         if (theList->constructName != NULL)
           { theList->constructName->neededSymbol = TRUE; }
        }

      /*==============================================*/
      /* Loop through each of the port items in the   */
      /* defmodule's export list incrementing the     */
      /* number of port items encountered and marking */
      /* needed symbols.                              */
      /*==============================================*/

      for (theList = defmodulePtr->exportList;
           theList != NULL;
           theList = theList->next)
        {
         DefmoduleData(theEnv,execStatus)->NumberOfPortItems++;
         if (theList->moduleName != NULL)
           { theList->moduleName->neededSymbol = TRUE; }
         if (theList->constructType != NULL)
           { theList->constructType->neededSymbol = TRUE; }
         if (theList->constructName != NULL)
           { theList->constructName->neededSymbol = TRUE; }
        }
     }
  }

/*********************************************************/
/* BsaveStorage: Writes out the storage requirements for */
/*    all defmodule structures to the binary file.       */
/*********************************************************/
static void BsaveStorage(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   size_t space;

   space = sizeof(long) * 2;
   GenWrite(&space,sizeof(size_t),fp);
   GenWrite(&DefmoduleData(theEnv,execStatus)->BNumberOfDefmodules,sizeof(long int),fp);
   GenWrite(&DefmoduleData(theEnv,execStatus)->NumberOfPortItems,sizeof(long int),fp);
  }

/*********************************************/
/* BsaveBinaryItem: Writes out all defmodule */
/*   structures to the binary file.          */
/*********************************************/
static void BsaveBinaryItem(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   size_t space;
   struct defmodule *defmodulePtr;
   struct bsaveDefmodule newDefmodule;
   struct bsavePortItem newPortItem;
   struct portItem *theList;

   /*=========================================================*/
   /* Write out the amount of space taken up by the defmodule */
   /* and port item data structures in the binary image.      */
   /*=========================================================*/

   space = DefmoduleData(theEnv,execStatus)->BNumberOfDefmodules * sizeof(struct bsaveDefmodule);
   space += DefmoduleData(theEnv,execStatus)->NumberOfPortItems * sizeof(struct bsavePortItem);
   GenWrite(&space,sizeof(size_t),fp);

   /*==========================================*/
   /* Write out each defmodule data structure. */
   /*==========================================*/

   DefmoduleData(theEnv,execStatus)->BNumberOfDefmodules = 0;
   DefmoduleData(theEnv,execStatus)->NumberOfPortItems = 0;
   for (defmodulePtr = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,NULL);
        defmodulePtr != NULL;
        defmodulePtr = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,defmodulePtr))
     {
      newDefmodule.name = defmodulePtr->name->bucket;

      DefmoduleData(theEnv,execStatus)->BNumberOfDefmodules++;
      if (defmodulePtr->next != NULL)
        { newDefmodule.next = DefmoduleData(theEnv,execStatus)->BNumberOfDefmodules; }
      else
        { newDefmodule.next = -1L; }

      if (defmodulePtr->importList == NULL)
        { newDefmodule.importList = -1L; }
      else
        {
         newDefmodule.importList = DefmoduleData(theEnv,execStatus)->NumberOfPortItems;
         for (theList = defmodulePtr->importList;
              theList != NULL;
              theList = theList->next)
           { DefmoduleData(theEnv,execStatus)->NumberOfPortItems++; }
        }

      if (defmodulePtr->exportList == NULL)
        { newDefmodule.exportList = -1L; }
      else
        {
         newDefmodule.exportList = DefmoduleData(theEnv,execStatus)->NumberOfPortItems;
         for (theList = defmodulePtr->exportList;
              theList != NULL;
              theList = theList->next)
           { DefmoduleData(theEnv,execStatus)->NumberOfPortItems++; }
        }

      newDefmodule.bsaveID = defmodulePtr->bsaveID;
      GenWrite(&newDefmodule,sizeof(struct bsaveDefmodule),fp);
     }

   /*==========================================*/
   /* Write out each port item data structure. */
   /*==========================================*/

   DefmoduleData(theEnv,execStatus)->NumberOfPortItems = 0;
   defmodulePtr = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,NULL);
   while (defmodulePtr != NULL)
     {
      for (theList = defmodulePtr->importList;
           theList != NULL;
           theList = theList->next)
        {
         DefmoduleData(theEnv,execStatus)->NumberOfPortItems++;
         if (theList->moduleName == NULL) newPortItem.moduleName = -1L;
         else newPortItem.moduleName = (long) theList->moduleName->bucket;

         if (theList->constructType == NULL) newPortItem.constructType = -1L;
         else newPortItem.constructType = (long) theList->constructType->bucket;

         if (theList->constructName == NULL) newPortItem.constructName = -1L;
         else newPortItem.constructName = (long) theList->constructName->bucket;

         if (theList->next == NULL) newPortItem.next = -1L;
         else newPortItem.next = DefmoduleData(theEnv,execStatus)->NumberOfPortItems;

         GenWrite(&newPortItem,sizeof(struct bsavePortItem),fp);
        }

      for (theList = defmodulePtr->exportList;
           theList != NULL;
           theList = theList->next)
        {
         DefmoduleData(theEnv,execStatus)->NumberOfPortItems++;
         if (theList->moduleName == NULL) newPortItem.moduleName = -1L;
         else newPortItem.moduleName = (long) theList->moduleName->bucket;

         if (theList->constructType == NULL) newPortItem.constructType = -1L;
         else newPortItem.constructType = (long) theList->constructType->bucket;

         if (theList->constructName == NULL) newPortItem.constructName = -1L;
         else newPortItem.constructName = (long) theList->constructName->bucket;

         if (theList->next == NULL) newPortItem.next = -1L;
         else newPortItem.next = DefmoduleData(theEnv,execStatus)->NumberOfPortItems;

         GenWrite(&newPortItem,sizeof(struct bsavePortItem),fp);
        }

      defmodulePtr = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,defmodulePtr);
     }

   /*=============================================================*/
   /* If a binary image was already loaded when the bsave command */
   /* was issued, then restore the counts indicating the number   */
   /* of defmodule and port items in the binary image (these were */
   /* overwritten by the binary save).                            */
   /*=============================================================*/

   RestoreBloadCount(theEnv,execStatus,&DefmoduleData(theEnv,execStatus)->BNumberOfDefmodules);
   RestoreBloadCount(theEnv,execStatus,&DefmoduleData(theEnv,execStatus)->NumberOfPortItems);
  }

#endif /* BLOAD_AND_BSAVE */

/**********************************************************/
/* BloadStorage: Allocates storage requirements for the   */
/*   defmodules and port items used by this binary image. */
/**********************************************************/
static void BloadStorage(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;

   /*=======================================*/
   /* Determine the number of defmodule and */
   /* port item data structures to be read. */
   /*=======================================*/

   GenReadBinary(theEnv,execStatus,&space,sizeof(size_t));
   GenReadBinary(theEnv,execStatus,&DefmoduleData(theEnv,execStatus)->BNumberOfDefmodules,sizeof(long int));
   GenReadBinary(theEnv,execStatus,&DefmoduleData(theEnv,execStatus)->NumberOfPortItems,sizeof(long int));

   /*================================*/
   /* Allocate the space needed for  */
   /* the defmodule data structures. */
   /*================================*/

   if (DefmoduleData(theEnv,execStatus)->BNumberOfDefmodules == 0)
     {
      DefmoduleData(theEnv,execStatus)->DefmoduleArray = NULL;
      return;
     }

   space = (DefmoduleData(theEnv,execStatus)->BNumberOfDefmodules * sizeof(struct defmodule));
   DefmoduleData(theEnv,execStatus)->DefmoduleArray = (struct defmodule *) genalloc(theEnv,execStatus,space);

   /*================================*/
   /* Allocate the space needed for  */
   /* the port item data structures. */
   /*================================*/

   if (DefmoduleData(theEnv,execStatus)->NumberOfPortItems == 0)
     {
      DefmoduleData(theEnv,execStatus)->PortItemArray = NULL;
      return;
     }

   space = (DefmoduleData(theEnv,execStatus)->NumberOfPortItems * sizeof(struct portItem));
   DefmoduleData(theEnv,execStatus)->PortItemArray = (struct portItem *) genalloc(theEnv,execStatus,space);
  }

/********************************************/
/* BloadBinaryItem: Loads and refreshes the */
/*   defmodules used by this binary image.  */
/********************************************/
static void BloadBinaryItem(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;

   GenReadBinary(theEnv,execStatus,&space,sizeof(size_t));
   if (DefmoduleData(theEnv,execStatus)->BNumberOfDefmodules == 0) return;

   BloadandRefresh(theEnv,execStatus,DefmoduleData(theEnv,execStatus)->BNumberOfDefmodules,sizeof(struct bsaveDefmodule),UpdateDefmodule);
   BloadandRefresh(theEnv,execStatus,DefmoduleData(theEnv,execStatus)->NumberOfPortItems,sizeof(struct bsavePortItem),UpdatePortItem);

   SetListOfDefmodules(theEnv,execStatus,(void *) DefmoduleData(theEnv,execStatus)->DefmoduleArray);
   EnvSetCurrentModule(theEnv,execStatus,(void *) EnvGetNextDefmodule(theEnv,execStatus,NULL));
  }

/******************************************/
/* UpdateDefmodule: Bload refresh routine */
/*   for defmodule data structure.        */
/******************************************/
static void UpdateDefmodule(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   struct bsaveDefmodule *bdp;
   struct moduleItem *theItem;
   int i;

   bdp = (struct bsaveDefmodule *) buf;
   DefmoduleData(theEnv,execStatus)->DefmoduleArray[obji].name = SymbolPointer(bdp->name);
   IncrementSymbolCount(DefmoduleData(theEnv,execStatus)->DefmoduleArray[obji].name);
   if (bdp->next != -1L)
     { DefmoduleData(theEnv,execStatus)->DefmoduleArray[obji].next = (struct defmodule *) &DefmoduleData(theEnv,execStatus)->DefmoduleArray[bdp->next]; }
   else
     { DefmoduleData(theEnv,execStatus)->DefmoduleArray[obji].next = NULL; }

   if (GetNumberOfModuleItems(theEnv,execStatus) == 0)
     { DefmoduleData(theEnv,execStatus)->DefmoduleArray[obji].itemsArray = NULL; }
   else
     {
      DefmoduleData(theEnv,execStatus)->DefmoduleArray[obji].itemsArray = 
         (struct defmoduleItemHeader **) gm2(theEnv,execStatus,sizeof(void *) * GetNumberOfModuleItems(theEnv,execStatus));
     }

   for (i = 0, theItem = GetListOfModuleItems(theEnv,execStatus);
        (i < GetNumberOfModuleItems(theEnv,execStatus)) && (theItem != NULL) ;
        i++, theItem = theItem->next)
     {
      if (theItem->bloadModuleReference == NULL)
        { DefmoduleData(theEnv,execStatus)->DefmoduleArray[obji].itemsArray[i] = NULL; }
      else
        {
         DefmoduleData(theEnv,execStatus)->DefmoduleArray[obji].itemsArray[i] =
             (struct defmoduleItemHeader *)
             (*theItem->bloadModuleReference)(theEnv,execStatus,obji);
        }
     }

   DefmoduleData(theEnv,execStatus)->DefmoduleArray[obji].ppForm = NULL;

   if (bdp->importList != -1L)
     { DefmoduleData(theEnv,execStatus)->DefmoduleArray[obji].importList = (struct portItem *) &DefmoduleData(theEnv,execStatus)->PortItemArray[bdp->importList]; }
   else
     { DefmoduleData(theEnv,execStatus)->DefmoduleArray[obji].importList = NULL; }

   if (bdp->exportList != -1L)
     { DefmoduleData(theEnv,execStatus)->DefmoduleArray[obji].exportList = (struct portItem *) &DefmoduleData(theEnv,execStatus)->PortItemArray[bdp->exportList]; }
   else
     { DefmoduleData(theEnv,execStatus)->DefmoduleArray[obji].exportList = NULL; }
   DefmoduleData(theEnv,execStatus)->DefmoduleArray[obji].bsaveID = bdp->bsaveID;
  }

/*****************************************/
/* UpdatePortItem: Bload refresh routine */
/*   for port item data structure.       */
/*****************************************/
static void UpdatePortItem(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   struct bsavePortItem *bdp;

   bdp = (struct bsavePortItem *) buf;

   if (bdp->moduleName != -1L)
     {
      DefmoduleData(theEnv,execStatus)->PortItemArray[obji].moduleName = SymbolPointer(bdp->moduleName);
      IncrementSymbolCount(DefmoduleData(theEnv,execStatus)->PortItemArray[obji].moduleName);
     }
   else
     { DefmoduleData(theEnv,execStatus)->PortItemArray[obji].moduleName = NULL; }

   if (bdp->constructType != -1L)
     {
      DefmoduleData(theEnv,execStatus)->PortItemArray[obji].constructType = SymbolPointer(bdp->constructType);
      IncrementSymbolCount(DefmoduleData(theEnv,execStatus)->PortItemArray[obji].constructType);
     }
   else
     { DefmoduleData(theEnv,execStatus)->PortItemArray[obji].constructType = NULL; }

   if (bdp->constructName != -1L)
     {
      DefmoduleData(theEnv,execStatus)->PortItemArray[obji].constructName = SymbolPointer(bdp->constructName);
      IncrementSymbolCount(DefmoduleData(theEnv,execStatus)->PortItemArray[obji].constructName);
     }
   else
     { DefmoduleData(theEnv,execStatus)->PortItemArray[obji].constructName = NULL; }

   if (bdp->next != -1L)
     { DefmoduleData(theEnv,execStatus)->PortItemArray[obji].next = (struct portItem *) &DefmoduleData(theEnv,execStatus)->PortItemArray[bdp->next]; }
   else
     { DefmoduleData(theEnv,execStatus)->PortItemArray[obji].next = NULL; }
  }

/***************************************/
/* ClearBload: Defmodule clear routine */
/*   when a binary load is in effect.  */
/***************************************/
static void ClearBload(
  void *theEnv,
  EXEC_STATUS)
  {
   long i;
   size_t space;
   struct portItem *theList;

   /*===========================*/
   /* Decrement in use counters */
   /* used by the binary image. */
   /*===========================*/

   for (i = 0; i < DefmoduleData(theEnv,execStatus)->BNumberOfDefmodules; i++)
     {
      DecrementSymbolCount(theEnv,execStatus,DefmoduleData(theEnv,execStatus)->DefmoduleArray[i].name);
      for (theList = DefmoduleData(theEnv,execStatus)->DefmoduleArray[i].importList;
           theList != NULL;
           theList = theList->next)
        {
         if (theList->moduleName != NULL) DecrementSymbolCount(theEnv,execStatus,theList->moduleName);
         if (theList->constructType != NULL) DecrementSymbolCount(theEnv,execStatus,theList->constructType);
         if (theList->constructName != NULL) DecrementSymbolCount(theEnv,execStatus,theList->constructName);
        }

      for (theList = DefmoduleData(theEnv,execStatus)->DefmoduleArray[i].exportList;
           theList != NULL;
           theList = theList->next)
        {
         if (theList->moduleName != NULL) DecrementSymbolCount(theEnv,execStatus,theList->moduleName);
         if (theList->constructType != NULL) DecrementSymbolCount(theEnv,execStatus,theList->constructType);
         if (theList->constructName != NULL) DecrementSymbolCount(theEnv,execStatus,theList->constructName);
        }

      rm(theEnv,execStatus,DefmoduleData(theEnv,execStatus)->DefmoduleArray[i].itemsArray,sizeof(void *) * GetNumberOfModuleItems(theEnv,execStatus));
     }

   /*================================*/
   /* Deallocate the space used for  */
   /* the defmodule data structures. */
   /*================================*/

   space = DefmoduleData(theEnv,execStatus)->BNumberOfDefmodules * sizeof(struct defmodule);
   if (space != 0) genfree(theEnv,execStatus,(void *) DefmoduleData(theEnv,execStatus)->DefmoduleArray,space);
   DefmoduleData(theEnv,execStatus)->BNumberOfDefmodules = 0;
   
   /*================================*/
   /* Deallocate the space used for  */
   /* the port item data structures. */
   /*================================*/

   space = DefmoduleData(theEnv,execStatus)->NumberOfPortItems * sizeof(struct portItem);
   if (space != 0) genfree(theEnv,execStatus,(void *) DefmoduleData(theEnv,execStatus)->PortItemArray,space);
   DefmoduleData(theEnv,execStatus)->NumberOfPortItems = 0;
   
   /*===========================*/
   /* Reset module information. */
   /*===========================*/

   SetListOfDefmodules(theEnv,execStatus,NULL);
   CreateMainModule(theEnv,execStatus);
   DefmoduleData(theEnv,execStatus)->MainModuleRedefinable = TRUE;
  }

#endif /*  (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME) */



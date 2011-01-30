   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
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
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
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

#include "modulbin.h"


/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static long                             NumberOfDefmodules = 0;
   Thread static long                             NumberOfPortItems = 0;
   Thread static struct portItem                 *PortItemArray = NULL;

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle struct defmodule                *DefmoduleArray = NULL;

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if BLOAD_AND_BSAVE
   static void                    BsaveFind(void);
   static void                    BsaveStorage(FILE *);
   static void                    BsaveBinaryItem(FILE *);
#endif
   static void                    BloadStorage(void);
   static void                    BloadBinaryItem(void);
   static void                    UpdateDefmodule(void *,long);
   static void                    UpdatePortItem(void *,long);
   static void                    ClearBload(void);

/*********************************************/
/* DefmoduleBinarySetup: Installs the binary */
/*   save/load feature for defmodules.       */
/*********************************************/
globle void DefmoduleBinarySetup()
  {
   AddBeforeBloadFunction("defmodule",RemoveAllDefmodules,2000);

#if BLOAD_AND_BSAVE
   AddBinaryItem("defmodule",0,BsaveFind,NULL,
                             BsaveStorage,BsaveBinaryItem,
                             BloadStorage,BloadBinaryItem,
                             ClearBload);
#endif

   AddAbortBloadFunction("defmodule",CreateMainModule,0);

#if (BLOAD || BLOAD_ONLY)
   AddBinaryItem("defmodule",0,NULL,NULL,NULL,NULL,
                             BloadStorage,BloadBinaryItem,
                             ClearBload);
#endif
  }

/**************************************************************/
/* UpdateDefmoduleItemHeader: Updates the values in defmodule */
/*   item headers for the loaded binary image.                */
/**************************************************************/
globle void UpdateDefmoduleItemHeader(
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
static void BsaveFind()
  {
   struct defmodule *defmodulePtr;
   struct portItem *theList;

   /*=======================================================*/
   /* If a binary image is already loaded, then temporarily */
   /* save the count values since these will be overwritten */
   /* in the process of saving the binary image.            */
   /*=======================================================*/

   if (Bloaded())
     {
      SaveBloadCount(NumberOfDefmodules);
      SaveBloadCount(NumberOfPortItems);
     }

   /*==========================================*/
   /* Set the count of defmodule and defmodule */
   /* port items data structures to zero.      */
   /*==========================================*/

   NumberOfDefmodules = 0;
   NumberOfPortItems = 0;

   /*===========================*/
   /* Loop through each module. */
   /*===========================*/

   for (defmodulePtr = (struct defmodule *) GetNextDefmodule(NULL);
        defmodulePtr != NULL;
        defmodulePtr = (struct defmodule *) GetNextDefmodule(defmodulePtr))
     {
      /*==============================================*/
      /* Increment the number of modules encountered. */
      /*==============================================*/

      NumberOfDefmodules++;

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
         NumberOfPortItems++;
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
         NumberOfPortItems++;
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
  FILE *fp)
  {
   unsigned long space;

   space = sizeof(long) * 2;
   GenWrite(&space,(unsigned long) sizeof(unsigned long int),fp);
   GenWrite(&NumberOfDefmodules,(unsigned long) sizeof(long int),fp);
   GenWrite(&NumberOfPortItems,(unsigned long) sizeof(long int),fp);
  }

/*********************************************/
/* BsaveBinaryItem: Writes out all defmodule */
/*   structures to the binary file.          */
/*********************************************/
static void BsaveBinaryItem(
  FILE *fp)
  {
   unsigned long int space;
   struct defmodule *defmodulePtr;
   struct bsaveDefmodule newDefmodule;
   struct bsavePortItem newPortItem;
   struct portItem *theList;

   /*=========================================================*/
   /* Write out the amount of space taken up by the defmodule */
   /* and port item data structures in the binary image.      */
   /*=========================================================*/

   space = NumberOfDefmodules * sizeof(struct bsaveDefmodule);
   space += NumberOfPortItems * sizeof(struct bsavePortItem);
   GenWrite(&space,(unsigned long) sizeof(unsigned long int),fp);

   /*==========================================*/
   /* Write out each defmodule data structure. */
   /*==========================================*/

   NumberOfDefmodules = 0;
   NumberOfPortItems = 0;
   for (defmodulePtr = (struct defmodule *) GetNextDefmodule(NULL);
        defmodulePtr != NULL;
        defmodulePtr = (struct defmodule *) GetNextDefmodule(defmodulePtr))
     {
      newDefmodule.name = (unsigned long) defmodulePtr->name->bucket;

      NumberOfDefmodules++;
      if (defmodulePtr->next != NULL)
        { newDefmodule.next = NumberOfDefmodules; }
      else
        { newDefmodule.next = -1L; }

      if (defmodulePtr->importList == NULL)
        { newDefmodule.importList = -1L; }
      else
        {
         newDefmodule.importList = NumberOfPortItems;
         for (theList = defmodulePtr->importList;
              theList != NULL;
              theList = theList->next)
           { NumberOfPortItems++; }
        }

      if (defmodulePtr->exportList == NULL)
        { newDefmodule.exportList = -1L; }
      else
        {
         newDefmodule.exportList = NumberOfPortItems;
         for (theList = defmodulePtr->exportList;
              theList != NULL;
              theList = theList->next)
           { NumberOfPortItems++; }
        }

      newDefmodule.bsaveID = defmodulePtr->bsaveID;
      GenWrite(&newDefmodule,(unsigned long) sizeof(struct bsaveDefmodule),fp);
     }

   /*==========================================*/
   /* Write out each port item data structure. */
   /*==========================================*/

   NumberOfPortItems = 0;
   defmodulePtr = (struct defmodule *) GetNextDefmodule(NULL);
   while (defmodulePtr != NULL)
     {
      for (theList = defmodulePtr->importList;
           theList != NULL;
           theList = theList->next)
        {
         NumberOfPortItems++;
         if (theList->moduleName == NULL) newPortItem.moduleName = -1L;
         else newPortItem.moduleName = (unsigned long) theList->moduleName->bucket;

         if (theList->constructType == NULL) newPortItem.constructType = -1L;
         else newPortItem.constructType = (unsigned long) theList->constructType->bucket;

         if (theList->constructName == NULL) newPortItem.constructName = -1L;
         else newPortItem.constructName = (unsigned long) theList->constructName->bucket;

         if (theList->next == NULL) newPortItem.next = -1L;
         else newPortItem.next = NumberOfPortItems;

         GenWrite(&newPortItem,(unsigned long) sizeof(struct bsavePortItem),fp);
        }

      for (theList = defmodulePtr->exportList;
           theList != NULL;
           theList = theList->next)
        {
         NumberOfPortItems++;
         if (theList->moduleName == NULL) newPortItem.moduleName = -1L;
         else newPortItem.moduleName = (unsigned long) theList->moduleName->bucket;

         if (theList->constructType == NULL) newPortItem.constructType = -1L;
         else newPortItem.constructType = (unsigned long) theList->constructType->bucket;

         if (theList->constructName == NULL) newPortItem.constructName = -1L;
         else newPortItem.constructName = (unsigned long) theList->constructName->bucket;

         if (theList->next == NULL) newPortItem.next = -1L;
         else newPortItem.next = NumberOfPortItems;

         GenWrite(&newPortItem,(unsigned long) sizeof(struct bsavePortItem),fp);
        }

      defmodulePtr = (struct defmodule *) GetNextDefmodule(defmodulePtr);
     }

   /*=============================================================*/
   /* If a binary image was already loaded when the bsave command */
   /* was issued, then restore the counts indicating the number   */
   /* of defmodule and port items in the binary image (these were */
   /* overwritten by the binary save).                            */
   /*=============================================================*/

   if (Bloaded())
     {
      RestoreBloadCount(&NumberOfDefmodules);
      RestoreBloadCount(&NumberOfPortItems);
     }
  }

#endif /* BLOAD_AND_BSAVE */

/**********************************************************/
/* BloadStorage: Allocates storage requirements for the   */
/*   defmodules and port items used by this binary image. */
/**********************************************************/
static void BloadStorage()
  {
   unsigned long int space;

   /*=======================================*/
   /* Determine the number of defmodule and */
   /* port item data structures to be read. */
   /*=======================================*/

   GenRead(&space,(unsigned long) sizeof(unsigned long int));
   GenRead(&NumberOfDefmodules,(unsigned long) sizeof(long int));
   GenRead(&NumberOfPortItems,(unsigned long) sizeof(long int));

   /*================================*/
   /* Allocate the space needed for  */
   /* the defmodule data structures. */
   /*================================*/

   if (NumberOfDefmodules == 0)
     {
      DefmoduleArray = NULL;
      return;
     }

   space = (unsigned long) (NumberOfDefmodules * sizeof(struct defmodule));
   DefmoduleArray = (struct defmodule *) genlongalloc(space);

   /*================================*/
   /* Allocate the space needed for  */
   /* the port item data structures. */
   /*================================*/

   if (NumberOfPortItems == 0)
     {
      PortItemArray = NULL;
      return;
     }

   space = (unsigned long) (NumberOfPortItems * sizeof(struct portItem));
   PortItemArray = (struct portItem *) genlongalloc(space);
  }

/********************************************/
/* BloadBinaryItem: Loads and refreshes the */
/*   defmodules used by this binary image.  */
/********************************************/
static void BloadBinaryItem()
  {
   unsigned long int space;

   GenRead(&space,(unsigned long) sizeof(unsigned long int));
   if (NumberOfDefmodules == 0) return;

   BloadandRefresh(NumberOfDefmodules,(unsigned) sizeof(struct bsaveDefmodule),UpdateDefmodule);
   BloadandRefresh(NumberOfPortItems,(unsigned) sizeof(struct bsavePortItem),UpdatePortItem);

   SetListOfDefmodules((void *) DefmoduleArray);
   SetCurrentModule((void *) GetNextDefmodule(NULL));
  }

/******************************************/
/* UpdateDefmodule: Bload refresh routine */
/*   for defmodule data structure.        */
/******************************************/
static void UpdateDefmodule(
  void *buf,
  long obji)
  {
   struct bsaveDefmodule *bdp;
   struct moduleItem *theItem;
   int i;

   bdp = (struct bsaveDefmodule *) buf;
   DefmoduleArray[obji].name = SymbolPointer(bdp->name);
   IncrementSymbolCount(DefmoduleArray[obji].name);
   if (bdp->next != -1L)
     { DefmoduleArray[obji].next = (struct defmodule *) &DefmoduleArray[bdp->next]; }
   else
     { DefmoduleArray[obji].next = NULL; }

   DefmoduleArray[obji].itemsArray = (struct defmoduleItemHeader **) gm2((int) sizeof(void *) * GetNumberOfModuleItems());

   for (i = 0, theItem = GetListOfModuleItems();
        (i < GetNumberOfModuleItems()) && (theItem != NULL) ;
        i++, theItem = theItem->next)
     {
      if (theItem->bloadModuleReference == NULL)
        { DefmoduleArray[obji].itemsArray[i] = NULL; }
      else
        {
         DefmoduleArray[obji].itemsArray[i] =
             (struct defmoduleItemHeader *)
             (*theItem->bloadModuleReference)(obji);
        }
     }

   DefmoduleArray[obji].ppForm = NULL;

   if (bdp->importList != -1L)
     { DefmoduleArray[obji].importList = (struct portItem *) &PortItemArray[bdp->importList]; }
   else
     { DefmoduleArray[obji].importList = NULL; }

   if (bdp->exportList != -1L)
     { DefmoduleArray[obji].exportList = (struct portItem *) &PortItemArray[bdp->exportList]; }
   else
     { DefmoduleArray[obji].exportList = NULL; }
   DefmoduleArray[obji].bsaveID = bdp->bsaveID;
  }

/*****************************************/
/* UpdatePortItem: Bload refresh routine */
/*   for port item data structure.       */
/*****************************************/
static void UpdatePortItem(
  void *buf,
  long obji)
  {
   struct bsavePortItem *bdp;

   bdp = (struct bsavePortItem *) buf;

   if (bdp->moduleName != -1L)
     {
      PortItemArray[obji].moduleName = SymbolPointer(bdp->moduleName);
      IncrementSymbolCount(PortItemArray[obji].moduleName);
     }
   else
     { PortItemArray[obji].moduleName = NULL; }

   if (bdp->constructType != -1L)
     {
      PortItemArray[obji].constructType = SymbolPointer(bdp->constructType);
      IncrementSymbolCount(PortItemArray[obji].constructType);
     }
   else
     { PortItemArray[obji].constructType = NULL; }

   if (bdp->constructName != -1L)
     {
      PortItemArray[obji].constructName = SymbolPointer(bdp->constructName);
      IncrementSymbolCount(PortItemArray[obji].constructName);
     }
   else
     { PortItemArray[obji].constructName = NULL; }

   if (bdp->next != -1L)
     { PortItemArray[obji].next = (struct portItem *) &PortItemArray[bdp->next]; }
   else
     { PortItemArray[obji].next = NULL; }
  }

/***************************************/
/* ClearBload: Defmodule clear routine */
/*   when a binary load is in effect.  */
/***************************************/
static void ClearBload()
  {
   long i;
   unsigned long space;
   struct portItem *theList;

   /*===========================*/
   /* Decrement in use counters */
   /* used by the binary image. */
   /*===========================*/

   for (i = 0; i < NumberOfDefmodules; i++)
     {
      DecrementSymbolCount(DefmoduleArray[i].name);
      for (theList = DefmoduleArray[i].importList;
           theList != NULL;
           theList = theList->next)
        {
         if (theList->moduleName != NULL) DecrementSymbolCount(theList->moduleName);
         if (theList->constructType != NULL) DecrementSymbolCount(theList->constructType);
         if (theList->constructName != NULL) DecrementSymbolCount(theList->constructName);
        }

      for (theList = DefmoduleArray[i].exportList;
           theList != NULL;
           theList = theList->next)
        {
         if (theList->moduleName != NULL) DecrementSymbolCount(theList->moduleName);
         if (theList->constructType != NULL) DecrementSymbolCount(theList->constructType);
         if (theList->constructName != NULL) DecrementSymbolCount(theList->constructName);
        }

      rm(DefmoduleArray[i].itemsArray,(int) sizeof(void *) * GetNumberOfModuleItems());
     }

   /*================================*/
   /* Deallocate the space used for  */
   /* the defmodule data structures. */
   /*================================*/

   space = NumberOfDefmodules * sizeof(struct defmodule);
   if (space != 0) genlongfree((void *) DefmoduleArray,space);

   /*================================*/
   /* Deallocate the space used for  */
   /* the port item data structures. */
   /*================================*/

   space = NumberOfPortItems * sizeof(struct portItem);
   if (space != 0) genlongfree((void *) PortItemArray,space);

   /*===========================*/
   /* Reset module information. */
   /*===========================*/

   SetListOfDefmodules(NULL);
   CreateMainModule();
   MainModuleRedefinable = TRUE;
  }

#endif /*  (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME) */



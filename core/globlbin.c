   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*            DEFGLOBAL BSAVE/BLOAD MODULE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for the  */
/*    defglobal construct.                                   */
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

#define _GLOBLBIN_SOURCE_

#include "setup.h"

#if DEFGLOBAL_CONSTRUCT && (BLOAD || BLOAD_AND_BSAVE || BLOAD_ONLY) && (! RUN_TIME)

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "memalloc.h"
#include "multifld.h"
#include "globldef.h"
#include "bload.h"
#include "bsave.h"
#include "moduldef.h"
#include "globlbsc.h"

#include "globlbin.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if BLOAD_AND_BSAVE
   static void                    BsaveFind(void);
   static void                    BsaveStorage(FILE *);
   static void                    BsaveBinaryItem(FILE *);
#endif
   static void                    BloadStorageDefglobals(void);
   static void                    BloadBinaryItem(void);
   static void                    UpdateDefglobalModule(void *,long);
   static void                    UpdateDefglobal(void *,long);
   static void                    ClearBload(void);

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle struct defglobal                 *DefglobalArray = NULL;

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static long                              NumberOfDefglobals = 0;
   Thread static struct defglobalModule           *ModuleArray;
   Thread static long                              NumberOfDefglobalModules;

/*********************************************/
/* DefglobalBinarySetup: Installs the binary */
/*   save/load feature for the defglobals.   */
/*********************************************/
globle void DefglobalBinarySetup()
  {
#if (BLOAD_AND_BSAVE || BLOAD)
   AddAfterBloadFunction("defglobal",ResetDefglobals,50);
#endif

#if BLOAD_AND_BSAVE
   AddBinaryItem("defglobal",0,BsaveFind,NULL,
                             BsaveStorage,BsaveBinaryItem,
                             BloadStorageDefglobals,BloadBinaryItem,
                             ClearBload);
#endif

#if (BLOAD || BLOAD_ONLY)
   AddBinaryItem("defglobal",0,NULL,NULL,NULL,NULL,
                             BloadStorageDefglobals,BloadBinaryItem,
                             ClearBload);
#endif
  }

#if BLOAD_AND_BSAVE

/****************************************************/
/* BsaveFind:  Counts the number of data structures */
/*   which must be saved in the binary image for    */
/*   the defglobals in the current environment.     */
/****************************************************/
static void BsaveFind()
  {
   struct defglobal *defglobalPtr;
   struct defmodule *theModule;

   /*=======================================================*/
   /* If a binary image is already loaded, then temporarily */
   /* save the count values since these will be overwritten */
   /* in the process of saving the binary image.            */
   /*=======================================================*/

   if (Bloaded())
     {
      SaveBloadCount(NumberOfDefglobalModules);
      SaveBloadCount(NumberOfDefglobals);
     }

   /*============================================*/
   /* Set the count of defglobals and defglobals */
   /* module data structures to zero.            */
   /*============================================*/

   NumberOfDefglobals = 0;
   NumberOfDefglobalModules = 0;

   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      /*================================================*/
      /* Set the current module to the module being     */
      /* examined and increment the number of defglobal */
      /* modules encountered.                           */
      /*================================================*/

      SetCurrentModule((void *) theModule);
      NumberOfDefglobalModules++;

      /*====================================================*/
      /* Loop through each defglobal in the current module. */
      /*====================================================*/

      for (defglobalPtr = (struct defglobal *) GetNextDefglobal(NULL);
           defglobalPtr != NULL;
           defglobalPtr = (struct defglobal *) GetNextDefglobal(defglobalPtr))
        {
         /*======================================================*/
         /* Initialize the construct header for the binary save. */
         /*======================================================*/

         MarkConstructHeaderNeededItems(&defglobalPtr->header,NumberOfDefglobals++);
        }
     }
  }

/*****************************************************/
/* BsaveStorage: Writes out storage requirements for */
/*   all defglobal structures to the binary file     */
/*****************************************************/
static void BsaveStorage(
  FILE *fp)
  {
   unsigned long space;

   /*===========================================================*/
   /* Only two data structures are saved as part of a defglobal */
   /* binary image: the defglobal data structure and the        */
   /* defglobalModule data structure.                           */
   /*===========================================================*/

   space = sizeof(long) * 2;
   GenWrite(&space,(unsigned long) sizeof(unsigned long int),fp);
   GenWrite(&NumberOfDefglobals,(unsigned long) sizeof(long int),fp);
   GenWrite(&NumberOfDefglobalModules,(unsigned long) sizeof(long int),fp);
  }

/*********************************************/
/* BsaveBinaryItem: Writes out all defglobal */
/*   structures to the binary file           */
/*********************************************/
static void BsaveBinaryItem(
  FILE *fp)
  {
   unsigned long int space;
   struct defglobal *theDefglobal;
   struct bsaveDefglobal newDefglobal;
   struct defmodule *theModule;
   struct bsaveDefglobalModule tempDefglobalModule;
   struct defglobalModule *theModuleItem;

   /*==========================================================*/
   /* Write out the amount of space taken up by the defglobal  */
   /* and defglobalModule data structures in the binary image. */
   /*==========================================================*/

   space = NumberOfDefglobals * sizeof(struct bsaveDefglobal) +
           (NumberOfDefglobalModules * sizeof(struct bsaveDefglobalModule));
   GenWrite(&space,(unsigned long) sizeof(unsigned long int),fp);

   /*=================================================*/
   /* Write out each defglobal module data structure. */
   /*=================================================*/

   NumberOfDefglobals = 0;
   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      SetCurrentModule((void *) theModule);

      theModuleItem = (struct defglobalModule *)
                      GetModuleItem(NULL,FindModuleItem("defglobal")->moduleIndex);
      AssignBsaveDefmdlItemHdrVals(&tempDefglobalModule.header,
                                           &theModuleItem->header);
      GenWrite(&tempDefglobalModule,(unsigned long) sizeof(struct bsaveDefglobalModule),fp);
     }

   /*===========================*/
   /* Write out each defglobal. */
   /*===========================*/

   NumberOfDefglobals = 0;
   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      SetCurrentModule((void *) theModule);

      for (theDefglobal = (struct defglobal *) GetNextDefglobal(NULL);
           theDefglobal != NULL;
           theDefglobal = (struct defglobal *) GetNextDefglobal(theDefglobal))
        {
         AssignBsaveConstructHeaderVals(&newDefglobal.header,
                                          &theDefglobal->header);
         newDefglobal.initial = HashedExpressionIndex(theDefglobal->initial);

         GenWrite(&newDefglobal,(unsigned long) sizeof(struct bsaveDefglobal),fp);
        }
     }

   /*=============================================================*/
   /* If a binary image was already loaded when the bsave command */
   /* was issued, then restore the counts indicating the number   */
   /* of defglobals and defglobal modules in the binary image     */
   /* (these were overwritten by the binary save).                */
   /*=============================================================*/

   if (Bloaded())
     {
      RestoreBloadCount(&NumberOfDefglobalModules);
      RestoreBloadCount(&NumberOfDefglobals);
     }
  }

#endif /* BLOAD_AND_BSAVE */

/***********************************************/
/* BloadStorageDefglobals: Allocates space for */
/*   the defglobals used by this binary image. */
/***********************************************/
static void BloadStorageDefglobals()
  {
   unsigned long int space;

   /*=======================================================*/
   /* Determine the number of defglobal and defglobalModule */
   /* data structures to be read.                           */
   /*=======================================================*/

   GenRead(&space,(unsigned long) sizeof(unsigned long int));
   GenRead(&NumberOfDefglobals,(unsigned long) sizeof(long int));
   GenRead(&NumberOfDefglobalModules,(unsigned long) sizeof(long int));

   /*===================================*/
   /* Allocate the space needed for the */
   /* defglobalModule data structures.  */
   /*===================================*/

   if (NumberOfDefglobalModules == 0)
     {
      DefglobalArray = NULL;
      ModuleArray = NULL;
     }

   space = NumberOfDefglobalModules * sizeof(struct defglobalModule);
   ModuleArray = (struct defglobalModule *) genlongalloc(space);

   /*===================================*/
   /* Allocate the space needed for the */
   /* defglobal data structures.        */
   /*===================================*/

   if (NumberOfDefglobals == 0)
     {
      DefglobalArray = NULL;
      return;
     }

   space = (unsigned long) (NumberOfDefglobals * sizeof(struct defglobal));
   DefglobalArray = (struct defglobal *) genlongalloc(space);
  }

/******************************************************/
/* BloadBinaryItem: Loads and refreshes the defglobal */
/*   constructs used by this binary image.            */
/******************************************************/
static void BloadBinaryItem()
  {
   unsigned long int space;

   /*======================================================*/
   /* Read in the amount of space used by the binary image */
   /* (this is used to skip the construct in the event it  */
   /* is not available in the version being run).          */
   /*======================================================*/

   GenRead(&space,(unsigned long) sizeof(unsigned long int));

   /*=============================================*/
   /* Read in the defglobalModule data structures */
   /* and refresh the pointers.                   */
   /*=============================================*/

   BloadandRefresh(NumberOfDefglobalModules,
                   (unsigned) sizeof(struct bsaveDefglobalModule),
                   UpdateDefglobalModule);

   /*=======================================*/
   /* Read in the defglobal data structures */
   /* and refresh the pointers.             */
   /*=======================================*/

   BloadandRefresh(NumberOfDefglobals,
                   (unsigned) sizeof(struct bsaveDefglobal),
                   UpdateDefglobal);
  }

/************************************************/
/* UpdateDefglobalModule: Bload refresh routine */
/*   for defglobal module data structures.      */
/************************************************/
static void UpdateDefglobalModule(
  void *buf,
  long obji)
  {
   struct bsaveDefglobalModule *bdmPtr;

   bdmPtr = (struct bsaveDefglobalModule *) buf;

   UpdateDefmoduleItemHeader(&bdmPtr->header,&ModuleArray[obji].header,
                             (int) sizeof(struct defglobal),
                             (void *) DefglobalArray);
  }

/******************************************/
/* UpdateDefglobal: Bload refresh routine */
/*   for defglobal data structures.       */
/******************************************/
static void UpdateDefglobal(
  void *buf,
  long obji)
  {
   struct bsaveDefglobal *bdp;

   bdp = (struct bsaveDefglobal *) buf;
   UpdateConstructHeader(&bdp->header,&DefglobalArray[obji].header,
                         (int) sizeof(struct defglobalModule),(void *) ModuleArray,
                         (int) sizeof(struct defglobal),(void *) DefglobalArray);

#if DEBUGGING_FUNCTIONS
   DefglobalArray[obji].watch = WatchGlobals;
#endif
   DefglobalArray[obji].initial = HashedExpressionPointer(bdp->initial);
   DefglobalArray[obji].current.type = RVOID;

  }

/***************************************/
/* ClearBload: Defglobal clear routine */
/*   when a binary load is in effect.  */
/***************************************/
static void ClearBload()
  {
   long i;
   unsigned long space;

   /*=======================================================*/
   /* Decrement in use counters for atomic values contained */
   /* in the construct headers. Also decrement data         */
   /* structures used to store the defglobal's value.       */
   /*=======================================================*/

   for (i = 0; i < NumberOfDefglobals; i++)
     {
      UnmarkConstructHeader(&DefglobalArray[i].header);

      ValueDeinstall(&(DefglobalArray[i].current));
      if (DefglobalArray[i].current.type == MULTIFIELD)
        { ReturnMultifield((struct multifield *) DefglobalArray[i].current.value); }
     }

   /*==============================================================*/
   /* Deallocate the space used for the defglobal data structures. */
   /*==============================================================*/

   space = NumberOfDefglobals * sizeof(struct defglobal);
   if (space != 0) genlongfree((void *) DefglobalArray,space);

   /*=====================================================================*/
   /* Deallocate the space used for the defglobal module data structures. */
   /*=====================================================================*/

   space =  NumberOfDefglobalModules * sizeof(struct defglobalModule);
   if (space != 0) genlongfree((void *) ModuleArray,space);
  }

/********************************************************/
/* BloadDefglobalModuleReference: Returns the defglobal */
/*   module pointer for using with the bload function.  */
/********************************************************/
globle void *BloadDefglobalModuleReference(
  int index)
  {
   return ((void *) &ModuleArray[index]);
  }

#endif /* DEFGLOBAL_CONSTRUCT && (BLOAD || BLOAD_AND_BSAVE || BLOAD_ONLY) && (! RUN_TIME) */




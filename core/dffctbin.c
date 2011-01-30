   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*             DEFFACTS BSAVE/BLOAD MODULE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for the  */
/*    deffacts construct.                                    */
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

#define _DFFCTBIN_SOURCE_

#include "setup.h"

#if DEFFACTS_CONSTRUCT && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME)

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "memalloc.h"
#include "dffctdef.h"
#include "moduldef.h"
#include "bload.h"
#include "bsave.h"

#include "dffctbin.h"

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static struct deffacts                  *DeffactsArray = NULL;
   Thread static long                              NumberOfDeffacts = 0;
   Thread static struct deffactsModule            *ModuleArray;
   Thread static long                              NumberOfDeffactsModules;

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if BLOAD_AND_BSAVE
   static void                    BsaveFind(void);
   static void                    BsaveExpressions(FILE *);
   static void                    BsaveStorage(FILE *);
   static void                    BsaveBinaryItem(FILE *);
#endif
   static void                    BloadStorage(void);
   static void                    BloadBinaryItem(void);
   static void                    UpdateDeffactsModule(void *,long);
   static void                    UpdateDeffacts(void *,long);
   static void                    ClearBload(void);

/********************************************/
/* DeffactsBinarySetup: Installs the binary */
/*   save/load feature for deffacts.        */
/********************************************/
globle void DeffactsBinarySetup()
  {
#if BLOAD_AND_BSAVE
   AddBinaryItem("deffacts",0,BsaveFind,BsaveExpressions,
                             BsaveStorage,BsaveBinaryItem,
                             BloadStorage,BloadBinaryItem,
                             ClearBload);
#endif

#if (BLOAD || BLOAD_ONLY)
   AddBinaryItem("deffacts",0,NULL,NULL,NULL,NULL,
                             BloadStorage,BloadBinaryItem,
                             ClearBload);
#endif
  }

#if BLOAD_AND_BSAVE

/*********************************************************/
/* BsaveFind: Counts the number of data structures which */
/*   must be saved in the binary image for the deffacts  */
/*   in the current environment.                         */
/*********************************************************/
static void BsaveFind()
  {
   struct deffacts *theDeffacts;
   struct defmodule *theModule;

   /*=======================================================*/
   /* If a binary image is already loaded, then temporarily */
   /* save the count values since these will be overwritten */
   /* in the process of saving the binary image.            */
   /*=======================================================*/

   if (Bloaded())
     {
      SaveBloadCount(NumberOfDeffactsModules);
      SaveBloadCount(NumberOfDeffacts);
     }

   /*========================================*/
   /* Set the count of deffacts and deffacts */
   /* module data structures to zero.        */
   /*========================================*/

   NumberOfDeffacts = 0;
   NumberOfDeffactsModules = 0;

   /*===========================*/
   /* Loop through each module. */
   /*===========================*/

   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      /*===============================================*/
      /* Set the current module to the module being    */
      /* examined and increment the number of deffacts */
      /* modules encountered.                          */
      /*===============================================*/

      SetCurrentModule((void *) theModule);
      NumberOfDeffactsModules++;

      /*===================================================*/
      /* Loop through each deffacts in the current module. */
      /*===================================================*/

      for (theDeffacts = (struct deffacts *) GetNextDeffacts(NULL);
           theDeffacts != NULL;
           theDeffacts = (struct deffacts *) GetNextDeffacts(theDeffacts))
        {
         /*======================================================*/
         /* Initialize the construct header for the binary save. */
         /*======================================================*/

         MarkConstructHeaderNeededItems(&theDeffacts->header,NumberOfDeffacts++);

         /*============================================================*/
         /* Count the number of expressions contained in the deffacts' */
         /* assertion list and mark any atomic values contained there  */
         /* as in use.                                                 */
         /*============================================================*/

         ExpressionCount += ExpressionSize(theDeffacts->assertList);
         MarkNeededItems(theDeffacts->assertList);
        }
     }
  }

/************************************************/
/* BsaveExpressions: Saves the expressions used */
/*   by deffacts to the binary save file.       */
/************************************************/
static void BsaveExpressions(
  FILE *fp)
  {
   struct deffacts *theDeffacts;
   struct defmodule *theModule;

   /*===========================*/
   /* Loop through each module. */
   /*===========================*/

   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      /*======================================================*/
      /* Set the current module to the module being examined. */
      /*======================================================*/

      SetCurrentModule((void *) theModule);

      /*==================================================*/
      /* Loop through each deffacts in the current module */
      /* and save the assertion list expression.          */
      /*==================================================*/

      for (theDeffacts = (struct deffacts *) GetNextDeffacts(NULL);
           theDeffacts != NULL;
           theDeffacts = (struct deffacts *) GetNextDeffacts(theDeffacts))
        { BsaveExpression(theDeffacts->assertList,fp); }
     }
  }

/******************************************************/
/* BsaveStorage: Writes out the storage requirements  */
/*    for all deffacts structures to the binary file. */
/******************************************************/
static void BsaveStorage(
  FILE *fp)
  {
   unsigned long space;

   /*=================================================================*/
   /* Only two data structures are saved as part of a deffacts binary */
   /* image: the deffacts data structure and the deffactsModule data  */
   /* structure. The assertion list expressions are not save with the */
   /* deffacts portion of the binary image.                           */
   /*=================================================================*/

   space = sizeof(long) * 2;
   GenWrite(&space,(unsigned long) sizeof(unsigned long int),fp);
   GenWrite(&NumberOfDeffacts,(unsigned long) sizeof(long int),fp);
   GenWrite(&NumberOfDeffactsModules,(unsigned long) sizeof(long int),fp);
  }

/********************************************/
/* BsaveBinaryItem: Writes out all deffacts */
/*   structures to the binary file.         */
/********************************************/
static void BsaveBinaryItem(
  FILE *fp)
  {
   unsigned long int space;
   struct deffacts *theDeffacts;
   struct bsaveDeffacts newDeffacts;
   struct defmodule *theModule;
   struct bsaveDeffactsModule tempDeffactsModule;
   struct deffactsModule *theModuleItem;

   /*=========================================================*/
   /* Write out the amount of space taken up by the deffacts  */
   /* and deffactsModule data structures in the binary image. */
   /*=========================================================*/

   space = NumberOfDeffacts * sizeof(struct bsaveDeffacts) +
           (NumberOfDeffactsModules * sizeof(struct bsaveDeffactsModule));
   GenWrite(&space,(unsigned long) sizeof(unsigned long int),fp);

   /*================================================*/
   /* Write out each deffacts module data structure. */
   /*================================================*/

   NumberOfDeffacts = 0;
   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      SetCurrentModule((void *) theModule);

      theModuleItem = (struct deffactsModule *) GetModuleItem(NULL,DeffactsModuleIndex);
      AssignBsaveDefmdlItemHdrVals(&tempDeffactsModule.header,&theModuleItem->header);
      GenWrite(&tempDeffactsModule,(unsigned long) sizeof(struct bsaveDeffactsModule),fp);
     }

   /*==========================*/
   /* Write out each deffacts. */
   /*==========================*/

   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      SetCurrentModule((void *) theModule);

      for (theDeffacts = (struct deffacts *) GetNextDeffacts(NULL);
           theDeffacts != NULL;
           theDeffacts = (struct deffacts *) GetNextDeffacts(theDeffacts))
        {
         AssignBsaveConstructHeaderVals(&newDeffacts.header,&theDeffacts->header);
         if (theDeffacts->assertList != NULL)
           {
            newDeffacts.assertList = ExpressionCount;
            ExpressionCount += ExpressionSize(theDeffacts->assertList);
           }
         else
           { newDeffacts.assertList = -1L; }

         GenWrite(&newDeffacts,(unsigned long) sizeof(struct bsaveDeffacts),fp);
        }
     }

   /*=============================================================*/
   /* If a binary image was already loaded when the bsave command */
   /* was issued, then restore the counts indicating the number   */
   /* of deffacts and deffacts modules in the binary image (these */
   /* were overwritten by the binary save).                       */
   /*=============================================================*/

   if (Bloaded())
     {
      RestoreBloadCount(&NumberOfDeffactsModules);
      RestoreBloadCount(&NumberOfDeffacts);
     }
  }

#endif /* BLOAD_AND_BSAVE */

/****************************************************/
/* BloadStorage: Allocates storage requirements for */
/*   the deffacts used by this binary image.        */
/****************************************************/
static void BloadStorage()
  {
   unsigned long int space;

   /*=====================================================*/
   /* Determine the number of deffacts and deffactsModule */
   /* data structures to be read.                         */
   /*=====================================================*/

   GenRead(&space,(unsigned long) sizeof(unsigned long int));
   GenRead(&NumberOfDeffacts,(unsigned long) sizeof(long int));
   GenRead(&NumberOfDeffactsModules,(unsigned long) sizeof(long int));

   /*===================================*/
   /* Allocate the space needed for the */
   /* deffactsModule data structures.   */
   /*===================================*/

   if (NumberOfDeffactsModules == 0)
     {
      DeffactsArray = NULL;
      ModuleArray = NULL;
      return;
     }

   space = NumberOfDeffactsModules * sizeof(struct deffactsModule);
   ModuleArray = (struct deffactsModule *) genlongalloc(space);

   /*===================================*/
   /* Allocate the space needed for the */
   /* deffacts data structures.         */
   /*===================================*/

   if (NumberOfDeffacts == 0)
     {
      DeffactsArray = NULL;
      return;
     }

   space = (unsigned long) (NumberOfDeffacts * sizeof(struct deffacts));
   DeffactsArray = (struct deffacts *) genlongalloc(space);
  }

/*****************************************************/
/* BloadBinaryItem: Loads and refreshes the deffacts */
/*   constructs used by this binary image.           */
/*****************************************************/
static void BloadBinaryItem()
  {
   unsigned long int space;

   /*======================================================*/
   /* Read in the amount of space used by the binary image */
   /* (this is used to skip the construct in the event it  */
   /* is not available in the version being run).          */
   /*======================================================*/

   GenRead(&space,(unsigned long) sizeof(unsigned long int));

   /*============================================*/
   /* Read in the deffactsModule data structures */
   /* and refresh the pointers.                  */
   /*============================================*/

   BloadandRefresh(NumberOfDeffactsModules,
                   (unsigned) sizeof(struct bsaveDeffactsModule),
                   UpdateDeffactsModule);

   /*======================================*/
   /* Read in the deffacts data structures */
   /* and refresh the pointers.            */
   /*======================================*/

   BloadandRefresh(NumberOfDeffacts,
                   (unsigned) sizeof(struct bsaveDeffacts),
                   UpdateDeffacts);
  }

/***************************************************/
/* UpdateDeffactsModule: Bload refresh routine for */
/*   deffacts module data structures.              */
/***************************************************/
static void UpdateDeffactsModule(
  void *buf,
  long obji)
  {
   struct bsaveDeffactsModule *bdmPtr;

   bdmPtr = (struct bsaveDeffactsModule *) buf;
   UpdateDefmoduleItemHeader(&bdmPtr->header,&ModuleArray[obji].header,
                             (int) sizeof(struct deffacts),(void *) DeffactsArray);
  }

/*********************************************/
/* UpdateDeffacts: Bload refresh routine for */
/*   deffacts data structures.               */
/*********************************************/
static void UpdateDeffacts(
  void *buf,
  long obji)
  {
   struct bsaveDeffacts *bdp;

   bdp = (struct bsaveDeffacts *) buf;
   UpdateConstructHeader(&bdp->header,&DeffactsArray[obji].header,
                         (int) sizeof(struct deffactsModule),(void *) ModuleArray,
                         (int) sizeof(struct deffacts),(void *) DeffactsArray);
   DeffactsArray[obji].assertList = ExpressionPointer(bdp->assertList);
  }

/**************************************/
/* ClearBload: Deffacts clear routine */
/*   when a binary load is in effect. */
/**************************************/
static void ClearBload()
  {
   long i;
   unsigned long space;

   /*=============================================*/
   /* Decrement in use counters for atomic values */
   /* contained in the construct headers.         */
   /*=============================================*/

   for (i = 0; i < NumberOfDeffacts; i++)
     { UnmarkConstructHeader(&DeffactsArray[i].header); }

   /*=============================================================*/
   /* Deallocate the space used for the deffacts data structures. */
   /*=============================================================*/

   space = NumberOfDeffacts * sizeof(struct deffacts);
   if (space != 0) genlongfree((void *) DeffactsArray,space);

   /*====================================================================*/
   /* Deallocate the space used for the deffacts module data structures. */
   /*====================================================================*/

   space =  NumberOfDeffactsModules * sizeof(struct deffactsModule);
   if (space != 0) genlongfree((void *) ModuleArray,space);
  }

/******************************************************/
/* BloadDeffactsModuleReference: Returns the deffacts */
/*   module pointer for use with the bload function.  */
/******************************************************/
globle void *BloadDeffactsModuleReference(
  int index)
  {
   return ((void *) &ModuleArray[index]);
  }

#endif /* DEFFACTS_CONSTRUCT && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME) */



   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.21  06/15/03            */
   /*                                                     */
   /*            DEFTEMPLATE BSAVE/BLOAD MODULE           */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for the  */
/*    deftemplate construct.                                 */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*      6.23: Added support for templates maintaining their  */
/*            own list of facts.                             */
/*                                                           */
/*************************************************************/

#define  _TMPLTBIN_SOURCE_

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME)

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "memalloc.h"
#include "bload.h"
#include "bsave.h"
#include "factbin.h"
#include "cstrnbin.h"
#include "fact/fact_manager.h"
#include "tmpltpsr.h"
#include "tmpltdef.h"
#include "tmpltutl.h"
#include "envrnmnt.h"

#include "tmpltbin.h"

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
   static void                    UpdateDeftemplateModule(void *,EXEC_STATUS,void *,long);
   static void                    UpdateDeftemplate(void *,EXEC_STATUS,void *,long);
   static void                    UpdateDeftemplateSlot(void *,EXEC_STATUS,void *,long);
   static void                    ClearBload(void *,EXEC_STATUS);
   static void                    DeallocateDeftemplateBloadData(void *,EXEC_STATUS);

/***********************************************/
/* DeftemplateBinarySetup: Installs the binary */
/*   save/load feature for deftemplates.       */
/***********************************************/
globle void DeftemplateBinarySetup(
  void *theEnv,
  EXEC_STATUS)
  {
   AllocateEnvironmentData(theEnv,execStatus,TMPLTBIN_DATA,sizeof(struct deftemplateBinaryData),DeallocateDeftemplateBloadData);
#if BLOAD_AND_BSAVE
   AddBinaryItem(theEnv,execStatus,"deftemplate",0,BsaveFind,NULL,
                             BsaveStorage,BsaveBinaryItem,
                             BloadStorage,BloadBinaryItem,
                             ClearBload);
#endif
#if (BLOAD || BLOAD_ONLY)
   AddBinaryItem(theEnv,execStatus,"deftemplate",0,NULL,NULL,NULL,NULL,
                             BloadStorage,BloadBinaryItem,
                             ClearBload);
#endif
  }
  
/***********************************************************/
/* DeallocateDeftemplateBloadData: Deallocates environment */
/*    data for the deftemplate bsave functionality.        */
/***********************************************************/
static void DeallocateDeftemplateBloadData(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;

   space =  DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateModules * sizeof(struct deftemplateModule);
   if (space != 0) genfree(theEnv,execStatus,(void *) DeftemplateBinaryData(theEnv,execStatus)->ModuleArray,space);
   
   space = DeftemplateBinaryData(theEnv,execStatus)->NumberOfDeftemplates * sizeof(struct deftemplate);
   if (space != 0) genfree(theEnv,execStatus,(void *) DeftemplateBinaryData(theEnv,execStatus)->DeftemplateArray,space);

   space =  DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateSlots * sizeof(struct templateSlot);
   if (space != 0) genfree(theEnv,execStatus,(void *) DeftemplateBinaryData(theEnv,execStatus)->SlotArray,space);
  }

#if BLOAD_AND_BSAVE

/**************************************************************/
/* BsaveFind: Counts the number of data structures which must */
/*   be saved in the binary image for the deftemplates in the */
/*   current environment.                                     */
/**************************************************************/
static void BsaveFind(
  void *theEnv,
  EXEC_STATUS)
  {
   struct deftemplate *theDeftemplate;
   struct templateSlot *theSlot;
   struct defmodule *theModule;

   /*=======================================================*/
   /* If a binary image is already loaded, then temporarily */
   /* save the count values since these will be overwritten */
   /* in the process of saving the binary image.            */
   /*=======================================================*/

   SaveBloadCount(theEnv,execStatus,DeftemplateBinaryData(theEnv,execStatus)->NumberOfDeftemplates);
   SaveBloadCount(theEnv,execStatus,DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateSlots);
   SaveBloadCount(theEnv,execStatus,DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateModules);

   /*==================================================*/
   /* Set the count of deftemplates, deftemplate slots */
   /* and deftemplate module data structures to zero.  */
   /*==================================================*/

   DeftemplateBinaryData(theEnv,execStatus)->NumberOfDeftemplates = 0;
   DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateSlots = 0;
   DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateModules = 0;

   /*===========================*/
   /* Loop through each module. */
   /*===========================*/

   for (theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,NULL);
        theModule != NULL;
        theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,theModule))
     {
      /*============================================*/
      /* Set the current module to the module being */
      /* examined and increment the number of       */
      /* deftemplate modules encountered.           */
      /*============================================*/

      EnvSetCurrentModule(theEnv,execStatus,(void *) theModule);
      DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateModules++;

      /*======================================================*/
      /* Loop through each deftemplate in the current module. */
      /*======================================================*/

      for (theDeftemplate = (struct deftemplate *) EnvGetNextDeftemplate(theEnv,execStatus,NULL);
           theDeftemplate != NULL;
           theDeftemplate = (struct deftemplate *) EnvGetNextDeftemplate(theEnv,execStatus,theDeftemplate))
        {
         /*======================================================*/
         /* Initialize the construct header for the binary save. */
         /*======================================================*/

         MarkConstructHeaderNeededItems(&theDeftemplate->header,
                                        DeftemplateBinaryData(theEnv,execStatus)->NumberOfDeftemplates++);

         /*=============================================================*/
         /* Loop through each slot in the deftemplate, incrementing the */
         /* slot count and marking the slot names as needed symbols.    */
         /*=============================================================*/

         for (theSlot = theDeftemplate->slotList;
              theSlot != NULL;
              theSlot = theSlot->next)
           {
            DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateSlots++;
            theSlot->slotName->neededSymbol = TRUE;
           }
        }

     }
  }

/*********************************************************/
/* BsaveStorage: Writes out the storage requirements for */
/*    all deftemplate structures to the binary file.     */
/*********************************************************/
static void BsaveStorage(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   size_t space;

   /*========================================================================*/
   /* Three data structures are saved as part of a deftemplate binary image: */
   /* the deftemplate data structure, the deftemplateModule data structure,  */
   /* and the templateSlot data structure. The data structures associated    */
   /* with default values and constraints are not save with the deftemplate  */
   /* portion of the binary image.                                           */
   /*========================================================================*/

   space = sizeof(long) * 3;
   GenWrite(&space,sizeof(size_t),fp);
   GenWrite(&DeftemplateBinaryData(theEnv,execStatus)->NumberOfDeftemplates,sizeof(long int),fp);
   GenWrite(&DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateSlots,sizeof(long int),fp);
   GenWrite(&DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateModules,sizeof(long int),fp);
  }

/***********************************************/
/* BsaveBinaryItem: Writes out all deftemplate */
/*   structures to the binary file.            */
/***********************************************/
static void BsaveBinaryItem(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   size_t space;
   struct deftemplate *theDeftemplate;
   struct bsaveDeftemplate tempDeftemplate;
   struct templateSlot *theSlot;
   struct bsaveTemplateSlot tempTemplateSlot;
   struct bsaveDeftemplateModule tempTemplateModule;
   struct defmodule *theModule;
   struct deftemplateModule *theModuleItem;

   /*============================================================*/
   /* Write out the amount of space taken up by the deftemplate, */
   /* deftemplateModule, and templateSlot data structures in the */
   /* binary image.                                              */
   /*============================================================*/

   space = (DeftemplateBinaryData(theEnv,execStatus)->NumberOfDeftemplates * sizeof(struct bsaveDeftemplate)) +
           (DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateSlots * sizeof(struct bsaveTemplateSlot)) +
           (DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateModules * sizeof(struct bsaveDeftemplateModule));
   GenWrite(&space,sizeof(size_t),fp);

   /*===================================================*/
   /* Write out each deftemplate module data structure. */
   /*===================================================*/

   DeftemplateBinaryData(theEnv,execStatus)->NumberOfDeftemplates = 0;
   for (theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,NULL);
        theModule != NULL;
        theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,theModule))
     {
      EnvSetCurrentModule(theEnv,execStatus,(void *) theModule);

      theModuleItem = (struct deftemplateModule *)
                      GetModuleItem(theEnv,execStatus,NULL,FindModuleItem(theEnv,execStatus,"deftemplate")->moduleIndex);
      AssignBsaveDefmdlItemHdrVals(&tempTemplateModule.header,
                                           &theModuleItem->header);
      GenWrite(&tempTemplateModule,sizeof(struct bsaveDeftemplateModule),fp);
     }

   /*============================================*/
   /* Write out each deftemplate data structure. */
   /*============================================*/

   DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateSlots = 0;
   for (theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,NULL);
        theModule != NULL;
        theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,theModule))
     {
      EnvSetCurrentModule(theEnv,execStatus,(void *) theModule);

      for (theDeftemplate = (struct deftemplate *) EnvGetNextDeftemplate(theEnv,execStatus,NULL);
           theDeftemplate != NULL;
           theDeftemplate = (struct deftemplate *) EnvGetNextDeftemplate(theEnv,execStatus,theDeftemplate))
        {
         AssignBsaveConstructHeaderVals(&tempDeftemplate.header,
                                          &theDeftemplate->header);
         tempDeftemplate.implied = theDeftemplate->implied;
         tempDeftemplate.numberOfSlots = theDeftemplate->numberOfSlots;
         tempDeftemplate.patternNetwork = BsaveFactPatternIndex(theDeftemplate->patternNetwork);

         if (theDeftemplate->slotList != NULL)
           { tempDeftemplate.slotList = DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateSlots; }
         else tempDeftemplate.slotList = -1L;

         GenWrite(&tempDeftemplate,sizeof(struct bsaveDeftemplate),fp);

         DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateSlots += theDeftemplate->numberOfSlots;
        }
     }

   /*=============================================*/
   /* Write out each templateSlot data structure. */
   /*=============================================*/

   for (theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,NULL);
        theModule != NULL;
        theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,theModule))
     {
      EnvSetCurrentModule(theEnv,execStatus,(void *) theModule);

      for (theDeftemplate = (struct deftemplate *) EnvGetNextDeftemplate(theEnv,execStatus,NULL);
           theDeftemplate != NULL;
           theDeftemplate = (struct deftemplate *) EnvGetNextDeftemplate(theEnv,execStatus,theDeftemplate))
        {
         for (theSlot = theDeftemplate->slotList;
              theSlot != NULL;
              theSlot = theSlot->next)
           {
            tempTemplateSlot.constraints = ConstraintIndex(theSlot->constraints);
            tempTemplateSlot.slotName = theSlot->slotName->bucket;
            tempTemplateSlot.multislot = theSlot->multislot;
            tempTemplateSlot.noDefault = theSlot->noDefault;
            tempTemplateSlot.defaultPresent = theSlot->defaultPresent;
            tempTemplateSlot.defaultDynamic = theSlot->defaultDynamic;
            tempTemplateSlot.defaultList = HashedExpressionIndex(theEnv,execStatus,theSlot->defaultList);
            tempTemplateSlot.facetList = HashedExpressionIndex(theEnv,execStatus,theSlot->facetList);

            if (theSlot->next != NULL) tempTemplateSlot.next = 0L;
            else tempTemplateSlot.next = -1L;

            GenWrite(&tempTemplateSlot,sizeof(struct bsaveTemplateSlot),fp);
           }
        }
     }

   /*=============================================================*/
   /* If a binary image was already loaded when the bsave command */
   /* was issued, then restore the counts indicating the number   */
   /* of deftemplates, deftemplate modules, and deftemplate slots */
   /* in the binary image (these were overwritten by the binary   */
   /* save).                                                      */
   /*=============================================================*/

   RestoreBloadCount(theEnv,execStatus,&DeftemplateBinaryData(theEnv,execStatus)->NumberOfDeftemplates);
   RestoreBloadCount(theEnv,execStatus,&DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateSlots);
   RestoreBloadCount(theEnv,execStatus,&DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateModules);
  }

#endif /* BLOAD_AND_BSAVE */

/****************************************************/
/* BloadStorage: Allocates storage requirements for */
/*   the deftemplates used by this binary image.    */
/****************************************************/
static void BloadStorage(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;

   /*=========================================================*/
   /* Determine the number of deftemplate, deftemplateModule, */
   /* and templateSlot data structures to be read.            */
   /*=========================================================*/

   GenReadBinary(theEnv,execStatus,&space,sizeof(size_t));
   GenReadBinary(theEnv,execStatus,&DeftemplateBinaryData(theEnv,execStatus)->NumberOfDeftemplates,sizeof(long int));
   GenReadBinary(theEnv,execStatus,&DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateSlots,sizeof(long int));
   GenReadBinary(theEnv,execStatus,&DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateModules,sizeof(long int));

   /*====================================*/
   /* Allocate the space needed for the  */
   /* deftemplateModule data structures. */
   /*====================================*/

   if (DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateModules == 0)
     {
      DeftemplateBinaryData(theEnv,execStatus)->DeftemplateArray = NULL;
      DeftemplateBinaryData(theEnv,execStatus)->SlotArray = NULL;
      DeftemplateBinaryData(theEnv,execStatus)->ModuleArray = NULL;
      return;
     }

   space = DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateModules * sizeof(struct deftemplateModule);
   DeftemplateBinaryData(theEnv,execStatus)->ModuleArray = (struct deftemplateModule *) genalloc(theEnv,execStatus,space);

   /*===================================*/
   /* Allocate the space needed for the */
   /* deftemplate data structures.      */
   /*===================================*/

   if (DeftemplateBinaryData(theEnv,execStatus)->NumberOfDeftemplates == 0)
     {
      DeftemplateBinaryData(theEnv,execStatus)->DeftemplateArray = NULL;
      DeftemplateBinaryData(theEnv,execStatus)->SlotArray = NULL;
      return;
     }

   space = DeftemplateBinaryData(theEnv,execStatus)->NumberOfDeftemplates * sizeof(struct deftemplate);
   DeftemplateBinaryData(theEnv,execStatus)->DeftemplateArray = (struct deftemplate *) genalloc(theEnv,execStatus,space);

   /*===================================*/
   /* Allocate the space needed for the */
   /* templateSlot data structures.     */
   /*===================================*/

   if (DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateSlots == 0)
     {
      DeftemplateBinaryData(theEnv,execStatus)->SlotArray = NULL;
      return;
     }

   space =  DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateSlots * sizeof(struct templateSlot);
   DeftemplateBinaryData(theEnv,execStatus)->SlotArray = (struct templateSlot *) genalloc(theEnv,execStatus,space);
  }

/********************************************************/
/* BloadBinaryItem: Loads and refreshes the deftemplate */
/*   constructs used by this binary image.              */
/********************************************************/
static void BloadBinaryItem(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;

   /*======================================================*/
   /* Read in the amount of space used by the binary image */
   /* (this is used to skip the construct in the event it  */
   /* is not available in the version being run).          */
   /*======================================================*/

   GenReadBinary(theEnv,execStatus,&space,sizeof(size_t));

   /*===============================================*/
   /* Read in the deftemplateModule data structures */
   /* and refresh the pointers.                     */
   /*===============================================*/

   BloadandRefresh(theEnv,execStatus,DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateModules,sizeof(struct bsaveDeftemplateModule),
                   UpdateDeftemplateModule);

   /*===============================================*/
   /* Read in the deftemplateModule data structures */
   /* and refresh the pointers.                     */
   /*===============================================*/

   BloadandRefresh(theEnv,execStatus,DeftemplateBinaryData(theEnv,execStatus)->NumberOfDeftemplates,sizeof(struct bsaveDeftemplate),
                   UpdateDeftemplate);

   /*==========================================*/
   /* Read in the templateSlot data structures */
   /* and refresh the pointers.                */
   /*==========================================*/

   BloadandRefresh(theEnv,execStatus,DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateSlots,sizeof(struct bsaveTemplateSlot),
                   UpdateDeftemplateSlot);
  }

/**************************************************/
/* UpdateDeftemplateModule: Bload refresh routine */
/*   for deftemplateModule data structures.       */
/**************************************************/
static void UpdateDeftemplateModule(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   struct bsaveDeftemplateModule *bdmPtr;

   bdmPtr = (struct bsaveDeftemplateModule *) buf;
   UpdateDefmoduleItemHeader(theEnv,execStatus,&bdmPtr->header,&DeftemplateBinaryData(theEnv,execStatus)->ModuleArray[obji].header,
                             (int) sizeof(struct deftemplate),
                             (void *) DeftemplateBinaryData(theEnv,execStatus)->DeftemplateArray);
  }

/********************************************/
/* UpdateDeftemplate: Bload refresh routine */
/*   for deftemplate data structures.       */
/********************************************/
static void UpdateDeftemplate(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   struct deftemplate *theDeftemplate;
   struct bsaveDeftemplate *bdtPtr;

   bdtPtr = (struct bsaveDeftemplate *) buf;
   theDeftemplate = (struct deftemplate *) &DeftemplateBinaryData(theEnv,execStatus)->DeftemplateArray[obji];

   UpdateConstructHeader(theEnv,execStatus,&bdtPtr->header,&theDeftemplate->header,
                         (int) sizeof(struct deftemplateModule),(void *) DeftemplateBinaryData(theEnv,execStatus)->ModuleArray,
                         (int) sizeof(struct deftemplate),(void *) DeftemplateBinaryData(theEnv,execStatus)->DeftemplateArray);

   if (bdtPtr->slotList != -1L)
     { theDeftemplate->slotList = (struct templateSlot *) &DeftemplateBinaryData(theEnv,execStatus)->SlotArray[bdtPtr->slotList]; }
   else
     { theDeftemplate->slotList = NULL; }

   if (bdtPtr->patternNetwork != -1L)
     { theDeftemplate->patternNetwork = (struct factPatternNode *) BloadFactPatternPointer(bdtPtr->patternNetwork); }
   else
     { theDeftemplate->patternNetwork = NULL; }

   theDeftemplate->implied = bdtPtr->implied;
#if DEBUGGING_FUNCTIONS
   theDeftemplate->watch = FactData(theEnv,execStatus)->WatchFacts;
#endif
   theDeftemplate->inScope = FALSE;
   theDeftemplate->numberOfSlots = (unsigned short) bdtPtr->numberOfSlots;
   theDeftemplate->factList = NULL;
   theDeftemplate->lastFact = NULL;
  }

/************************************************/
/* UpdateDeftemplateSlot: Bload refresh routine */
/*   for templateSlot data structures.          */
/************************************************/
static void UpdateDeftemplateSlot(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   struct templateSlot *theSlot;
   struct bsaveTemplateSlot *btsPtr;

   btsPtr = (struct bsaveTemplateSlot *) buf;
   theSlot = (struct templateSlot *) &DeftemplateBinaryData(theEnv,execStatus)->SlotArray[obji];

   theSlot->slotName = SymbolPointer(btsPtr->slotName);
   IncrementSymbolCount(theSlot->slotName);
   theSlot->defaultList = HashedExpressionPointer(btsPtr->defaultList);
   theSlot->facetList = HashedExpressionPointer(btsPtr->facetList);
   theSlot->constraints = ConstraintPointer(btsPtr->constraints);

   theSlot->multislot = btsPtr->multislot;
   theSlot->noDefault = btsPtr->noDefault;
   theSlot->defaultPresent = btsPtr->defaultPresent;
   theSlot->defaultDynamic = btsPtr->defaultDynamic;

   if (btsPtr->next != -1L)
     { theSlot->next = (struct templateSlot *) &DeftemplateBinaryData(theEnv,execStatus)->SlotArray[obji + 1]; }
   else
     { theSlot->next = NULL; }
  }

/*****************************************/
/* ClearBload: Deftemplate clear routine */
/*   when a binary load is in effect.    */
/*****************************************/
static void ClearBload(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;
   int i;

   /*=============================================*/
   /* Decrement in use counters for atomic values */
   /* contained in the construct headers.         */
   /*=============================================*/

   for (i = 0; i < DeftemplateBinaryData(theEnv,execStatus)->NumberOfDeftemplates; i++)
     { UnmarkConstructHeader(theEnv,execStatus,&DeftemplateBinaryData(theEnv,execStatus)->DeftemplateArray[i].header); }

   /*=======================================*/
   /* Decrement in use counters for symbols */
   /* used as slot names.                   */
   /*=======================================*/

   for (i = 0; i < DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateSlots; i++)
     { DecrementSymbolCount(theEnv,execStatus,DeftemplateBinaryData(theEnv,execStatus)->SlotArray[i].slotName); }

   /*======================================================================*/
   /* Deallocate the space used for the deftemplateModule data structures. */
   /*======================================================================*/

   space =  DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateModules * sizeof(struct deftemplateModule);
   if (space != 0) genfree(theEnv,execStatus,(void *) DeftemplateBinaryData(theEnv,execStatus)->ModuleArray,space);
   DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateModules = 0;
   
   /*================================================================*/
   /* Deallocate the space used for the deftemplate data structures. */
   /*================================================================*/

   space = DeftemplateBinaryData(theEnv,execStatus)->NumberOfDeftemplates * sizeof(struct deftemplate);
   if (space != 0) genfree(theEnv,execStatus,(void *) DeftemplateBinaryData(theEnv,execStatus)->DeftemplateArray,space);
   DeftemplateBinaryData(theEnv,execStatus)->NumberOfDeftemplates = 0;
   
   /*=================================================================*/
   /* Deallocate the space used for the templateSlot data structures. */
   /*=================================================================*/

   space =  DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateSlots * sizeof(struct templateSlot);
   if (space != 0) genfree(theEnv,execStatus,(void *) DeftemplateBinaryData(theEnv,execStatus)->SlotArray,space);
   DeftemplateBinaryData(theEnv,execStatus)->NumberOfTemplateSlots = 0;
   
   /*======================================*/
   /* Create the initial-fact deftemplate. */
   /*======================================*/

#if (! BLOAD_ONLY)
   CreateImpliedDeftemplate(theEnv,execStatus,(SYMBOL_HN *) EnvAddSymbol(theEnv,execStatus,"initial-fact"),FALSE);
#endif
  }

/************************************************************/
/* BloadDeftemplateModuleReference: Returns the deftemplate */
/*   module pointer for use with the bload function.        */
/************************************************************/
globle void *BloadDeftemplateModuleReference(
  void *theEnv,
  EXEC_STATUS,
  int theIndex)
  {
   return ((void *) &DeftemplateBinaryData(theEnv,execStatus)->ModuleArray[theIndex]);
  }

#endif /* DEFTEMPLATE_CONSTRUCT && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME) */




   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  10/19/06            */
   /*                                                     */
   /*                FACT BSAVE/BLOAD MODULE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for the  */
/*    fact pattern network.                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Added support for hashed alpha memories.       */
/*                                                           */
/*************************************************************/

#define _FACTBIN_SOURCE_

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME)

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "memalloc.h"
#include "tmpltdef.h"
#include "bload.h"
#include "bsave.h"
#include "reteutil.h"
#include "rulebin.h"
#include "moduldef.h"
#include "envrnmnt.h"

#include "factbin.h"

/********************************************/
/* INTERNAL DATA STRUCTURES AND DEFINITIONS */
/********************************************/

struct bsaveFactPatternNode
  {
   struct bsavePatternNodeHeader header;
   unsigned short whichSlot;
   unsigned short whichField;
   unsigned short leaveFields;
   long networkTest;
   long nextLevel;
   long lastLevel;
   long leftNode;
   long rightNode;
  };

#define BSAVE_FIND         0
#define BSAVE_PATTERNS     1

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if BLOAD_AND_BSAVE
   static void                    BsaveDriver(void *,EXEC_STATUS,int,FILE *,struct factPatternNode *);
   static void                    BsaveFind(void *,EXEC_STATUS);
   static void                    BsaveStorage(void *,EXEC_STATUS,FILE *);
   static void                    BsaveFactPatterns(void *,EXEC_STATUS,FILE *);
   static void                    BsavePatternNode(void *,EXEC_STATUS,struct factPatternNode *,FILE *);
#endif
   static void                    BloadStorage(void *,EXEC_STATUS);
   static void                    BloadBinaryItem(void *,EXEC_STATUS);
   static void                    UpdateFactPatterns(void *,EXEC_STATUS,void *,long);
   static void                    ClearBload(void *,EXEC_STATUS);
   static void                    DeallocateFactBloadData(void *,EXEC_STATUS);

/*****************************************************/
/* FactBinarySetup: Initializes the binary load/save */
/*   feature for the fact pattern network.           */
/*****************************************************/
globle void FactBinarySetup(
  void *theEnv,
  EXEC_STATUS)
  {
   AllocateEnvironmentData(theEnv,execStatus,FACTBIN_DATA,sizeof(struct factBinaryData),DeallocateFactBloadData);
   
#if BLOAD_AND_BSAVE
   AddBinaryItem(theEnv,execStatus,"facts",0,BsaveFind,NULL,
                            BsaveStorage,BsaveFactPatterns,
                            BloadStorage,BloadBinaryItem,
                            ClearBload);
#endif
#if BLOAD || BLOAD_ONLY
   AddBinaryItem(theEnv,execStatus,"facts",0,NULL,NULL,NULL,NULL,
                            BloadStorage,BloadBinaryItem,
                            ClearBload);
#endif
  }
  
/****************************************************/
/* DeallocateFactBloadData: Deallocates environment */
/*    data for the fact bsave functionality.        */
/****************************************************/
static void DeallocateFactBloadData(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;
   int i;
   
   for (i = 0; i < FactBinaryData(theEnv,execStatus)->NumberOfPatterns; i++)
     { DestroyAlphaMemory(theEnv,execStatus,&FactBinaryData(theEnv,execStatus)->FactPatternArray[i].header,FALSE); }

   space = FactBinaryData(theEnv,execStatus)->NumberOfPatterns * sizeof(struct factPatternNode);
   if (space != 0) genfree(theEnv,execStatus,(void *) FactBinaryData(theEnv,execStatus)->FactPatternArray,space);
  }

#if BLOAD_AND_BSAVE

/*********************************************************/
/* BsaveFind: Counts the number of data structures which */
/*   must be saved in the binary image for the fact      */
/*   pattern network in the current environment.         */
/*********************************************************/
static void BsaveFind(
  void *theEnv,
  EXEC_STATUS)
  {
   struct deftemplate *theDeftemplate;
   struct defmodule *theModule;

   /*=======================================================*/
   /* If a binary image is already loaded, then temporarily */
   /* save the count values since these will be overwritten */
   /* in the process of saving the binary image.            */
   /*=======================================================*/

   SaveBloadCount(theEnv,execStatus,FactBinaryData(theEnv,execStatus)->NumberOfPatterns);

   /*=======================================*/
   /* Set the count of fact pattern network */
   /* data structures to zero.              */
   /*=======================================*/

   FactBinaryData(theEnv,execStatus)->NumberOfPatterns = 0L;

   /*===========================*/
   /* Loop through each module. */
   /*===========================*/

   for (theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,NULL);
        theModule != NULL;
        theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,theModule))
     {
      /*===============================*/
      /* Set the current module to the */
      /* module being examined.        */
      /*===============================*/

      EnvSetCurrentModule(theEnv,execStatus,(void *) theModule);

      /*=====================================================*/
      /* Loop through each deftemplate in the current module */
      /* and count the number of data structures which must  */
      /* be saved for its pattern network.                   */
      /*=====================================================*/

      for (theDeftemplate = (struct deftemplate *) EnvGetNextDeftemplate(theEnv,execStatus,NULL);
           theDeftemplate != NULL;
           theDeftemplate = (struct deftemplate *) EnvGetNextDeftemplate(theEnv,execStatus,theDeftemplate))
        { BsaveDriver(theEnv,execStatus,BSAVE_FIND,NULL,theDeftemplate->patternNetwork); }
     }
  }

/**********************************************************/
/* BsaveDriver: Binary save driver routine which handles  */
/*   both finding/marking the data structures to be saved */
/*   and saving the data structures to a file.            */
/**********************************************************/
static void BsaveDriver(
  void *theEnv,
  EXEC_STATUS,
  int action,
  FILE *fp,
  struct factPatternNode *thePattern)
  {
   while (thePattern != NULL)
     {
      switch(action)
        {
         case BSAVE_FIND:
           thePattern->bsaveID = FactBinaryData(theEnv,execStatus)->NumberOfPatterns++;
           break;

         case BSAVE_PATTERNS:
           BsavePatternNode(theEnv,execStatus,thePattern,fp);
           break;

         default:
           break;
        }

      if (thePattern->nextLevel == NULL)
        {
         while (thePattern->rightNode == NULL)
           {
            thePattern = thePattern->lastLevel;
            if (thePattern == NULL) return;
           }
         thePattern = thePattern->rightNode;
        }
      else
        { thePattern = thePattern->nextLevel; }
     }
  }

/*********************************************************/
/* BsaveStorage: Writes out storage requirements for all */
/*   factPatternNode data structures to the binary file  */
/*********************************************************/
static void BsaveStorage(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   size_t space;

   space = sizeof(long);
   GenWrite(&space,sizeof(size_t),fp);
   GenWrite(&FactBinaryData(theEnv,execStatus)->NumberOfPatterns,sizeof(long int),fp);
  }

/*****************************************************/
/* BsaveFactPatterns: Writes out all factPatternNode */
/*    data structures to the binary file.            */
/*****************************************************/
static void BsaveFactPatterns(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   size_t space;
   struct deftemplate *theDeftemplate;
   struct defmodule *theModule;

   /*========================================*/
   /* Write out the amount of space taken up */
   /* by the factPatternNode data structures */
   /* in the binary image.                   */
   /*========================================*/

   space = FactBinaryData(theEnv,execStatus)->NumberOfPatterns * sizeof(struct bsaveFactPatternNode);
   GenWrite(&space,sizeof(size_t),fp);

   /*===========================*/
   /* Loop through each module. */
   /*===========================*/

   for (theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,NULL);
        theModule != NULL;
        theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,theModule))
     {
      /*=====================================================*/
      /* Loop through each deftemplate in the current module */
      /* and save its fact pattern network to the file.      */
      /*=====================================================*/

      EnvSetCurrentModule(theEnv,execStatus,(void *) theModule);
      for (theDeftemplate = (struct deftemplate *) EnvGetNextDeftemplate(theEnv,execStatus,NULL);
           theDeftemplate != NULL;
           theDeftemplate = (struct deftemplate *) EnvGetNextDeftemplate(theEnv,execStatus,theDeftemplate))
        { BsaveDriver(theEnv,execStatus,BSAVE_PATTERNS,fp,theDeftemplate->patternNetwork); }
    }

   /*=============================================================*/
   /* If a binary image was already loaded when the bsave command */
   /* was issued, then restore the counts indicating the number   */
   /* of factPatternNode data structures in the binary image      */
   /* (these were overwritten by the binary save).                */
   /*=============================================================*/

   RestoreBloadCount(theEnv,execStatus,&FactBinaryData(theEnv,execStatus)->NumberOfPatterns);
  }

/******************************************************/
/* BsavePatternNode: Writes out a single fact pattern */
/*   node to the binary image save file.              */
/******************************************************/
static void BsavePatternNode(
  void *theEnv,
  EXEC_STATUS,
  struct factPatternNode *thePattern,
  FILE *fp)
  {
   struct bsaveFactPatternNode tempNode;

   AssignBsavePatternHeaderValues(theEnv,execStatus,&tempNode.header,&thePattern->header);

   tempNode.whichField = thePattern->whichField;
   tempNode.leaveFields = thePattern->leaveFields;
   tempNode.whichSlot = thePattern->whichSlot;
   tempNode.networkTest = HashedExpressionIndex(theEnv,execStatus,thePattern->networkTest);
   tempNode.nextLevel =  BsaveFactPatternIndex(thePattern->nextLevel);
   tempNode.lastLevel =  BsaveFactPatternIndex(thePattern->lastLevel);
   tempNode.leftNode =  BsaveFactPatternIndex(thePattern->leftNode);
   tempNode.rightNode =  BsaveFactPatternIndex(thePattern->rightNode);

   GenWrite(&tempNode,(unsigned long) sizeof(struct bsaveFactPatternNode),fp);
  }

#endif /* BLOAD_AND_BSAVE */

/*****************************************************/
/* BloadStorage: Allocates storage requirements for  */
/*   the factPatternNodes used by this binary image. */
/*****************************************************/
static void BloadStorage(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;

   /*=========================================*/
   /* Determine the number of factPatternNode */
   /* data structures to be read.             */
   /*=========================================*/

   GenReadBinary(theEnv,execStatus,&space,sizeof(size_t));
   GenReadBinary(theEnv,execStatus,&FactBinaryData(theEnv,execStatus)->NumberOfPatterns,sizeof(long int));

   /*===================================*/
   /* Allocate the space needed for the */
   /* factPatternNode data structures.  */
   /*===================================*/

   if (FactBinaryData(theEnv,execStatus)->NumberOfPatterns == 0)
     {
      FactBinaryData(theEnv,execStatus)->FactPatternArray = NULL;
      return;
     }

   space = FactBinaryData(theEnv,execStatus)->NumberOfPatterns * sizeof(struct factPatternNode);
   FactBinaryData(theEnv,execStatus)->FactPatternArray = (struct factPatternNode *) genalloc(theEnv,execStatus,space);
  }

/************************************************************/
/* BloadBinaryItem: Loads and refreshes the factPatternNode */
/*   data structures used by this binary image.             */
/************************************************************/
static void BloadBinaryItem(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;
   long i;

   /*======================================================*/
   /* Read in the amount of space used by the binary image */
   /* (this is used to skip the construct in the event it  */
   /* is not available in the version being run).          */
   /*======================================================*/

   GenReadBinary(theEnv,execStatus,&space,sizeof(size_t));

   /*=============================================*/
   /* Read in the factPatternNode data structures */
   /* and refresh the pointers.                   */
   /*=============================================*/

   BloadandRefresh(theEnv,execStatus,FactBinaryData(theEnv,execStatus)->NumberOfPatterns,(unsigned) sizeof(struct bsaveFactPatternNode),
                   UpdateFactPatterns);
                   
   for (i = 0; i < FactBinaryData(theEnv,execStatus)->NumberOfPatterns; i++)
     {
      if ((FactBinaryData(theEnv,execStatus)->FactPatternArray[i].lastLevel != NULL) &&
          (FactBinaryData(theEnv,execStatus)->FactPatternArray[i].lastLevel->header.selector))
        { 
         AddHashedPatternNode(theEnv,execStatus,FactBinaryData(theEnv,execStatus)->FactPatternArray[i].lastLevel,
                                     &FactBinaryData(theEnv,execStatus)->FactPatternArray[i],
                                     FactBinaryData(theEnv,execStatus)->FactPatternArray[i].networkTest->type,
                                     FactBinaryData(theEnv,execStatus)->FactPatternArray[i].networkTest->value); 
        }
     }
  }

/*************************************************/
/* UpdateFactPatterns: Bload refresh routine for */
/*   the factPatternNode structure.              */
/*************************************************/
static void UpdateFactPatterns(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   struct bsaveFactPatternNode *bp;

   bp = (struct bsaveFactPatternNode *) buf;

   UpdatePatternNodeHeader(theEnv,execStatus,&FactBinaryData(theEnv,execStatus)->FactPatternArray[obji].header,&bp->header);

   FactBinaryData(theEnv,execStatus)->FactPatternArray[obji].bsaveID = 0L;
   FactBinaryData(theEnv,execStatus)->FactPatternArray[obji].whichField = bp->whichField;
   FactBinaryData(theEnv,execStatus)->FactPatternArray[obji].leaveFields = bp->leaveFields;
   FactBinaryData(theEnv,execStatus)->FactPatternArray[obji].whichSlot = bp->whichSlot;

   FactBinaryData(theEnv,execStatus)->FactPatternArray[obji].networkTest = HashedExpressionPointer(bp->networkTest);
   FactBinaryData(theEnv,execStatus)->FactPatternArray[obji].rightNode = BloadFactPatternPointer(bp->rightNode);
   FactBinaryData(theEnv,execStatus)->FactPatternArray[obji].nextLevel = BloadFactPatternPointer(bp->nextLevel);
   FactBinaryData(theEnv,execStatus)->FactPatternArray[obji].lastLevel = BloadFactPatternPointer(bp->lastLevel);
   FactBinaryData(theEnv,execStatus)->FactPatternArray[obji].leftNode  = BloadFactPatternPointer(bp->leftNode);
  }

/***************************************************/
/* ClearBload:  Fact pattern network clear routine */
/*   when a binary load is in effect.              */
/***************************************************/
static void ClearBload(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;
   long i;
   
   for (i = 0; i < FactBinaryData(theEnv,execStatus)->NumberOfPatterns; i++)
     {
      if ((FactBinaryData(theEnv,execStatus)->FactPatternArray[i].lastLevel != NULL) &&
          (FactBinaryData(theEnv,execStatus)->FactPatternArray[i].lastLevel->header.selector))
        { 
         RemoveHashedPatternNode(theEnv,execStatus,FactBinaryData(theEnv,execStatus)->FactPatternArray[i].lastLevel,
                                        &FactBinaryData(theEnv,execStatus)->FactPatternArray[i],
                                        FactBinaryData(theEnv,execStatus)->FactPatternArray[i].networkTest->type,
                                        FactBinaryData(theEnv,execStatus)->FactPatternArray[i].networkTest->value); 
        }
     }


   space = FactBinaryData(theEnv,execStatus)->NumberOfPatterns * sizeof(struct factPatternNode);
   if (space != 0) genfree(theEnv,execStatus,(void *) FactBinaryData(theEnv,execStatus)->FactPatternArray,space);
   FactBinaryData(theEnv,execStatus)->NumberOfPatterns = 0;
  }

#endif /* DEFTEMPLATE_CONSTRUCT && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME) */



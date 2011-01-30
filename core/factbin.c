
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
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
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
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
#include "rulebin.h"
#include "moduldef.h"

#include "factbin.h"

/********************************************/
/* INTERNAL DATA STRUCTURES AND DEFINITIONS */
/********************************************/

struct bsaveFactPatternNode
  {
   struct bsavePatternNodeHeader header;
   unsigned int whichSlot : 8;
   unsigned int whichField : 8;
   unsigned int leaveFields : 8;
   long networkTest;
   long nextLevel;
   long lastLevel;
   long leftNode;
   long rightNode;
  };

#define BSAVE_FIND         0
#define BSAVE_PATTERNS     1

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle struct factPatternNode            *FactPatternArray;

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static long                               NumberOfPatterns;

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if BLOAD_AND_BSAVE
   static void                    BsaveDriver(int,FILE *,struct factPatternNode *);
   static void                    BsaveFind(void);
   static void                    BsaveStorage(FILE *);
   static void                    BsaveFactPatterns(FILE *);
   static void                    BsavePatternNode(struct factPatternNode *,FILE *);
#endif
   static void                    BloadStorage(void);
   static void                    BloadBinaryItem(void);
   static void                    UpdateFactPatterns(void *,long);
   static void                    ClearBload(void);

/*****************************************************/
/* FactBinarySetup: Initializes the binary load/save */
/*   feature for the fact pattern network.           */
/*****************************************************/
globle void FactBinarySetup()
  {
#if BLOAD_AND_BSAVE
   AddBinaryItem("facts",0,BsaveFind,NULL,
                            BsaveStorage,BsaveFactPatterns,
                            BloadStorage,BloadBinaryItem,
                            ClearBload);
#endif
#if BLOAD || BLOAD_ONLY
   AddBinaryItem("facts",0,NULL,NULL,NULL,NULL,
                            BloadStorage,BloadBinaryItem,
                            ClearBload);
#endif
  }

#if BLOAD_AND_BSAVE

/*********************************************************/
/* BsaveFind: Counts the number of data structures which */
/*   must be saved in the binary image for the fact      */
/*   pattern network in the current environment.         */
/*********************************************************/
static void BsaveFind()
  {
   struct deftemplate *theDeftemplate;
   struct defmodule *theModule;

   /*=======================================================*/
   /* If a binary image is already loaded, then temporarily */
   /* save the count values since these will be overwritten */
   /* in the process of saving the binary image.            */
   /*=======================================================*/

   if (Bloaded()) SaveBloadCount(NumberOfPatterns);

   /*=======================================*/
   /* Set the count of fact pattern network */
   /* data structures to zero.              */
   /*=======================================*/

   NumberOfPatterns = 0L;

   /*===========================*/
   /* Loop through each module. */
   /*===========================*/

   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      /*===============================*/
      /* Set the current module to the */
      /* module being examined.        */
      /*===============================*/

      SetCurrentModule((void *) theModule);

      /*=====================================================*/
      /* Loop through each deftemplate in the current module */
      /* and count the number of data structures which must  */
      /* be saved for its pattern network.                   */
      /*=====================================================*/

      for (theDeftemplate = (struct deftemplate *) GetNextDeftemplate(NULL);
           theDeftemplate != NULL;
           theDeftemplate = (struct deftemplate *) GetNextDeftemplate(theDeftemplate))
        { BsaveDriver(BSAVE_FIND,NULL,theDeftemplate->patternNetwork); }
     }
  }

/**********************************************************/
/* BsaveDriver: Binary save driver routine which handles  */
/*   both finding/marking the data structures to be saved */
/*   and saving the data structures to a file.            */
/**********************************************************/
static void BsaveDriver(
  int action,
  FILE *fp,
  struct factPatternNode *thePattern)
  {
   while (thePattern != NULL)
     {
      switch(action)
        {
         case BSAVE_FIND:
           thePattern->bsaveID = NumberOfPatterns++;
           break;

         case BSAVE_PATTERNS:
           BsavePatternNode(thePattern,fp);
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
  FILE *fp)
  {
   unsigned long space;

   space = sizeof(long);
   GenWrite(&space,(unsigned long) sizeof(unsigned long int),fp);
   GenWrite(&NumberOfPatterns,(unsigned long) sizeof(long int),fp);
  }

/*****************************************************/
/* BsaveFactPatterns: Writes out all factPatternNode */
/*    data structures to the binary file.            */
/*****************************************************/
static void BsaveFactPatterns(
  FILE *fp)
  {
   unsigned long int space;
   struct deftemplate *theDeftemplate;
   struct defmodule *theModule;

   /*========================================*/
   /* Write out the amount of space taken up */
   /* by the factPatternNode data structures */
   /* in the binary image.                   */
   /*========================================*/

   space = NumberOfPatterns * sizeof(struct bsaveFactPatternNode);
   GenWrite(&space,(unsigned long) sizeof(unsigned long int),fp);

   /*===========================*/
   /* Loop through each module. */
   /*===========================*/

   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      /*=====================================================*/
      /* Loop through each deftemplate in the current module */
      /* and save its fact pattern network to the file.      */
      /*=====================================================*/

      SetCurrentModule((void *) theModule);
      for (theDeftemplate = (struct deftemplate *) GetNextDeftemplate(NULL);
           theDeftemplate != NULL;
           theDeftemplate = (struct deftemplate *) GetNextDeftemplate(theDeftemplate))
        { BsaveDriver(BSAVE_PATTERNS,fp,theDeftemplate->patternNetwork); }
    }

   /*=============================================================*/
   /* If a binary image was already loaded when the bsave command */
   /* was issued, then restore the counts indicating the number   */
   /* of factPatternNode data structures in the binary image      */
   /* (these were overwritten by the binary save).                */
   /*=============================================================*/

   if (Bloaded()) RestoreBloadCount(&NumberOfPatterns);
  }

/******************************************************/
/* BsavePatternNode: Writes out a single fact pattern */
/*   node to the binary image save file.              */
/******************************************************/
static void BsavePatternNode(
  struct factPatternNode *thePattern,
  FILE *fp)
  {
   struct bsaveFactPatternNode tempNode;

   AssignBsavePatternHeaderValues(&tempNode.header,&thePattern->header);

   tempNode.whichField = thePattern->whichField;
   tempNode.leaveFields = thePattern->leaveFields;
   tempNode.whichSlot = thePattern->whichSlot;
   tempNode.networkTest = HashedExpressionIndex(thePattern->networkTest);
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
static void BloadStorage()
  {
   unsigned long space;

   /*=========================================*/
   /* Determine the number of factPatternNode */
   /* data structures to be read.             */
   /*=========================================*/

   GenRead(&space,(unsigned long) sizeof(unsigned long int));
   GenRead(&NumberOfPatterns,(unsigned long) sizeof(long int));

   /*===================================*/
   /* Allocate the space needed for the */
   /* factPatternNode data structures.  */
   /*===================================*/

   if (NumberOfPatterns == 0)
     {
      FactPatternArray = NULL;
      return;
     }

   space = NumberOfPatterns * sizeof(struct factPatternNode);
   FactPatternArray = (struct factPatternNode *) genlongalloc(space);
  }

/************************************************************/
/* BloadBinaryItem: Loads and refreshes the factPatternNode */
/*   data structures used by this binary image.             */
/************************************************************/
static void BloadBinaryItem()
  {
   unsigned long space;

   /*======================================================*/
   /* Read in the amount of space used by the binary image */
   /* (this is used to skip the construct in the event it  */
   /* is not available in the version being run).          */
   /*======================================================*/

   GenRead(&space,(unsigned long) sizeof(unsigned long int));

   /*=============================================*/
   /* Read in the factPatternNode data structures */
   /* and refresh the pointers.                   */
   /*=============================================*/

   BloadandRefresh(NumberOfPatterns,(unsigned) sizeof(struct bsaveFactPatternNode),
                   UpdateFactPatterns);
  }

/*************************************************/
/* UpdateFactPatterns: Bload refresh routine for */
/*   the factPatternNode structure.              */
/*************************************************/
static void UpdateFactPatterns(
  void *buf,
  long obji)
  {
   struct bsaveFactPatternNode *bp;

   bp = (struct bsaveFactPatternNode *) buf;

   UpdatePatternNodeHeader(&FactPatternArray[obji].header,&bp->header);

   FactPatternArray[obji].bsaveID = 0L;
   FactPatternArray[obji].whichField = bp->whichField;
   FactPatternArray[obji].leaveFields = bp->leaveFields;
   FactPatternArray[obji].whichSlot = bp->whichSlot;

   FactPatternArray[obji].networkTest = HashedExpressionPointer(bp->networkTest);
   FactPatternArray[obji].rightNode = BloadFactPatternPointer(bp->rightNode);
   FactPatternArray[obji].nextLevel = BloadFactPatternPointer(bp->nextLevel);
   FactPatternArray[obji].lastLevel = BloadFactPatternPointer(bp->lastLevel);
   FactPatternArray[obji].leftNode  = BloadFactPatternPointer(bp->leftNode);
  }

/***************************************************/
/* ClearBload:  Fact pattern network clear routine */
/*   when a binary load is in effect.              */
/***************************************************/
static void ClearBload()
  {
   unsigned long int space;

   space = NumberOfPatterns * sizeof(struct factPatternNode);
   if (space != 0) genlongfree((void *) FactPatternArray,space);
  }

#endif /* DEFTEMPLATE_CONSTRUCT && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME) */



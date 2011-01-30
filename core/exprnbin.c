   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*             EXPRESSION BSAVE/BLOAD MODULE           */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for the  */
/*    expression data structure.                             */
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

#define _EXPRNBIN_SOURCE_

#include "setup.h"

#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "memalloc.h"
#include "dffctdef.h"
#include "moduldef.h"
#include "constrct.h"
#include "extnfunc.h"
#include "bload.h"
#include "bsave.h"

#if DEFRULE_CONSTRUCT
#include "network.h"
#endif

#if DEFGENERIC_CONSTRUCT
#include "genrcbin.h"
#endif

#if DEFFUNCTION_CONSTRUCT
#include "dffnxbin.h"
#endif

#if DEFTEMPLATE_CONSTRUCT
#include "tmpltbin.h"
#endif

#if DEFGLOBAL_CONSTRUCT
#include "globlbin.h"
#endif

#if OBJECT_SYSTEM
#include "objbin.h"
#include "insfun.h"
#include "inscom.h"
#endif

#include "exprnbin.h"

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static  long                                  NumberOfExpressions;

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle struct expr                           *ExpressionArray;
   Thread globle long int                               ExpressionCount;

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                        UpdateExpression(void *,long);

/***********************************************************/
/* AllocateExpressions: Determines the amount of space     */
/*   required for loading the binary image of expressions  */
/*   and allocates that amount of space.                   */
/***********************************************************/
globle void AllocateExpressions()
  {
   unsigned long space;

   GenRead((void *) &NumberOfExpressions,(unsigned long) sizeof(long));
   if (NumberOfExpressions == 0L)
     ExpressionArray = NULL;
   else
     {
      space = NumberOfExpressions * sizeof(struct expr);
      ExpressionArray = (struct expr *) genlongalloc(space);
     }
  }

/**********************************************/
/* RefreshExpressions: Refreshes the pointers */
/*   used by the expression binary image.     */
/**********************************************/
globle void RefreshExpressions()
  {
   if (ExpressionArray == NULL) return;

   BloadandRefresh(NumberOfExpressions,
                   (unsigned) sizeof(BSAVE_EXPRESSION),UpdateExpression);
  }

/*********************************************************
  NAME         : UpdateExpression
  DESCRIPTION  : Given a bloaded expression buffer,
                   this routine refreshes the pointers
                   in the expression array
  INPUTS       : 1) a bloaded expression buffer
                 2) the index of the expression to refresh
  RETURNS      : Nothing useful
  SIDE EFFECTS : Expression updated
  NOTES        : None
 *********************************************************/
static void UpdateExpression(
  void *buf,
  long obji)
  {
   BSAVE_EXPRESSION *bexp;
   long index;

   bexp = (BSAVE_EXPRESSION *) buf;
   ExpressionArray[obji].type = bexp->type;
   switch(bexp->type)
     {
      case FCALL:
        ExpressionArray[obji].value = (void *) FunctionArray[bexp->value];
        break;

      case GCALL:
#if DEFGENERIC_CONSTRUCT
        ExpressionArray[obji].value = (void *) GenericPointer(bexp->value);
#else
        ExpressionArray[obji].value = NULL;
#endif
        break;

      case PCALL:
#if DEFFUNCTION_CONSTRUCT
        ExpressionArray[obji].value = (void *) DeffunctionPointer(bexp->value);
#else
        ExpressionArray[obji].value = NULL;
#endif
        break;

      case DEFTEMPLATE_PTR:
#if DEFTEMPLATE_CONSTRUCT
        ExpressionArray[obji].value = (void *) DeftemplatePointer(bexp->value);
#else
        ExpressionArray[obji].value = NULL;
#endif
        break;

     case DEFCLASS_PTR:
#if OBJECT_SYSTEM
        ExpressionArray[obji].value = (void *) DefclassPointer(bexp->value);
#else
        ExpressionArray[obji].value = NULL;
#endif
        break;

      case DEFGLOBAL_PTR:

#if DEFGLOBAL_CONSTRUCT
        ExpressionArray[obji].value = (void *) DefglobalPointer(bexp->value);
#else
        ExpressionArray[obji].value = NULL;
#endif
        break;


      case INTEGER:
        ExpressionArray[obji].value = (void *) IntegerArray[bexp->value];
        IncrementIntegerCount((INTEGER_HN *) ExpressionArray[obji].value);
        break;

      case FLOAT:
        ExpressionArray[obji].value = (void *) FloatArray[bexp->value];
        IncrementFloatCount((FLOAT_HN *) ExpressionArray[obji].value);
        break;

      case INSTANCE_NAME:
#if ! OBJECT_SYSTEM
        ExpressionArray[obji].type = SYMBOL;
#endif
      case GBL_VARIABLE:
      case SYMBOL:
      case STRING:
        ExpressionArray[obji].value = (void *) SymbolArray[bexp->value];
        IncrementSymbolCount((SYMBOL_HN *) ExpressionArray[obji].value);
        break;

#if DEFTEMPLATE_CONSTRUCT
      case FACT_ADDRESS:
        ExpressionArray[obji].value = (void *) &DummyFact;
        IncrementFactCount(ExpressionArray[obji].value);
        break;
#endif

#if OBJECT_SYSTEM
      case INSTANCE_ADDRESS:
        ExpressionArray[obji].value = (void *) &DummyInstance;
        IncrementInstanceCount(ExpressionArray[obji].value);
        break;
#endif

      case EXTERNAL_ADDRESS:
        ExpressionArray[obji].value = NULL;
        break;

      case RVOID:
        break;

      default:
        if (PrimitivesArray[bexp->type] == NULL) break;
        if (PrimitivesArray[bexp->type]->bitMap)
          {
           ExpressionArray[obji].value = (void *) BitMapArray[bexp->value];
           IncrementBitMapCount((BITMAP_HN *) ExpressionArray[obji].value);
          }
        break;
     }

   index = (long int) bexp->nextArg;
   if (index == -1L)
     { ExpressionArray[obji].nextArg = NULL; }
   else
     { ExpressionArray[obji].nextArg = (struct expr *) &ExpressionArray[index]; }

   index = (long int) bexp->argList;
   if (index == -1L)
     { ExpressionArray[obji].argList = NULL; }
   else
     { ExpressionArray[obji].argList = (struct expr *) &ExpressionArray[index]; }
  }

/*********************************************/
/* ClearBloadedExpressions: Clears the space */
/*   utilized by an expression binary image. */
/*********************************************/
globle void ClearBloadedExpressions()
  {
   unsigned long int i, space;

   /*===============================================*/
   /* Update the busy counts of atomic data values. */
   /*===============================================*/

   for (i = 0; i < NumberOfExpressions; i++)
     {
      switch (ExpressionArray[i].type)
        {
         case SYMBOL          :
         case STRING          :
         case INSTANCE_NAME   :
         case GBL_VARIABLE    :
           DecrementSymbolCount((SYMBOL_HN *) ExpressionArray[i].value);
           break;
         case FLOAT           :
           DecrementFloatCount((FLOAT_HN *) ExpressionArray[i].value);
           break;
         case INTEGER         :
           DecrementIntegerCount((INTEGER_HN *) ExpressionArray[i].value);
           break;

#if DEFTEMPLATE_CONSTRUCT
         case FACT_ADDRESS    :
           DecrementFactCount(ExpressionArray[i].value);
           break;
#endif

#if OBJECT_SYSTEM
         case INSTANCE_ADDRESS :
           DecrementInstanceCount(ExpressionArray[i].value);
           break;
#endif

         case RVOID:
           break;

         default:
           if (PrimitivesArray[ExpressionArray[i].type] == NULL) break;
           if (PrimitivesArray[ExpressionArray[i].type]->bitMap)
             { DecrementBitMapCount((BITMAP_HN *) ExpressionArray[i].value); }
           break;
        }
     }

   /*===================================*/
   /* Free the binary expression array. */
   /*===================================*/

   space = NumberOfExpressions * sizeof(struct expr);
   if (space != 0) genlongfree((void *) ExpressionArray,space);
  }


#if BLOAD_AND_BSAVE

/***************************************************
  NAME         : FindHashedExpressions
  DESCRIPTION  : Sets the bsave expression array
                 indices for hashed expression nodes
                 and marks the items needed by
                 these expressions
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Atoms marked and ids set
  NOTES        : None
 ***************************************************/
globle void FindHashedExpressions()
  {
   register unsigned i;
   EXPRESSION_HN *exphash;

   for (i = 0 ; i < EXPRESSION_HASH_SIZE ; i++)
     for (exphash = ExpressionHashTable[i] ; exphash != NULL ; exphash = exphash->nxt)
       {
        MarkNeededItems(exphash->exp);
        exphash->bsaveID = ExpressionCount;
        ExpressionCount += ExpressionSize(exphash->exp);
       }
  }

/***************************************************
  NAME         : BsaveHashedExpressions
  DESCRIPTION  : Writes out hashed expressions
  INPUTS       : Bsave file stream pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Expressions written
  NOTES        : None
 ***************************************************/
globle void BsaveHashedExpressions(
  FILE *fp)
  {
   register unsigned i;
   EXPRESSION_HN *exphash;

   for (i = 0 ; i < EXPRESSION_HASH_SIZE ; i++)
     for (exphash = ExpressionHashTable[i] ; exphash != NULL ; exphash = exphash->nxt)
       BsaveExpression(exphash->exp,fp);
  }

/***************************************************************/
/* BsaveConstructExpressions: Writes all expression needed by  */
/*   constructs for this binary image to the binary save file. */
/***************************************************************/
globle void BsaveConstructExpressions(
  FILE *fp)
  {
   struct BinaryItem *biPtr;

   for (biPtr = ListOfBinaryItems;
        biPtr != NULL;
        biPtr = biPtr->next)
     {
      if (biPtr->expressionFunction != NULL)
        { (*biPtr->expressionFunction)(fp); }
     }
  }

/***************************************/
/* BsaveExpression: Recursively saves  */
/*   an expression to the binary file. */
/***************************************/
globle void BsaveExpression(
  struct expr *testPtr,
  FILE *fp)
  {
   BSAVE_EXPRESSION newTest;
   long int newIndex;

   while (testPtr != NULL)
     {
      ExpressionCount++;

      /*================*/
      /* Copy the type. */
      /*================*/

      newTest.type = testPtr->type;

      /*=======================================*/
      /* Convert the argList slot to an index. */
      /*=======================================*/

      if (testPtr->argList == NULL)
        { newTest.argList = -1L; }
      else
        { newTest.argList = ExpressionCount; }

      /*========================================*/
      /* Convert the nextArg slot to an index. */
      /*========================================*/

      if (testPtr->nextArg == NULL)
        { newTest.nextArg = -1L; }
      else
        {
         newIndex = ExpressionCount + ExpressionSize(testPtr->argList);
         newTest.nextArg = newIndex;
        }

      /*=========================*/
      /* Convert the value slot. */
      /*=========================*/

      switch(testPtr->type)
        {
         case FLOAT:
           newTest.value = (unsigned long) ((FLOAT_HN *) testPtr->value)->bucket;
           break;

         case INTEGER:
           newTest.value = (unsigned long) ((INTEGER_HN *) testPtr->value)->bucket;
           break;

         case FCALL:
           newTest.value = (unsigned long) ((struct FunctionDefinition *)
                                  testPtr->value)->bsaveIndex;
           break;

         case GCALL:
#if DEFGENERIC_CONSTRUCT
           if (testPtr->value != NULL)
             newTest.value = ((struct constructHeader *) testPtr->value)->bsaveID;
           else
#endif
             newTest.value = -1L;
           break;

         case PCALL:
#if DEFFUNCTION_CONSTRUCT
           if (testPtr->value != NULL)
             newTest.value = ((struct constructHeader *) testPtr->value)->bsaveID;
           else
#endif
             newTest.value = -1L;
           break;

         case DEFTEMPLATE_PTR:
#if DEFTEMPLATE_CONSTRUCT
           if (testPtr->value != NULL)
             newTest.value = ((struct constructHeader *) testPtr->value)->bsaveID;
           else
#endif
             newTest.value = -1L;
           break;

         case DEFCLASS_PTR:
#if OBJECT_SYSTEM
           if (testPtr->value != NULL)
             newTest.value = ((struct constructHeader *) testPtr->value)->bsaveID;
           else
#endif
             newTest.value = -1L;
           break;

         case DEFGLOBAL_PTR:
#if DEFGLOBAL_CONSTRUCT
           if (testPtr->value != NULL)
             newTest.value = ((struct defglobal *) testPtr->value)->header.bsaveID;
           else
#endif
             newTest.value = -1L;
           break;

#if OBJECT_SYSTEM
         case INSTANCE_NAME:
#endif
         case SYMBOL:
         case GBL_VARIABLE:
         case STRING:
           newTest.value = (unsigned long) ((SYMBOL_HN *) testPtr->value)->bucket;
           break;

         case FACT_ADDRESS:
         case INSTANCE_ADDRESS:
         case EXTERNAL_ADDRESS:
           newTest.value = -1L;
           break;

         case RVOID:
           break;

         default:
           if (PrimitivesArray[testPtr->type] == NULL) break;
           if (PrimitivesArray[testPtr->type]->bitMap)
             { newTest.value = (unsigned long) ((BITMAP_HN *) testPtr->value)->bucket; }
           break;
        }

     /*===========================*/
     /* Write out the expression. */
     /*===========================*/

     GenWrite(&newTest,(unsigned long) sizeof(BSAVE_EXPRESSION),fp);

     /*==========================*/
     /* Write out argument list. */
     /*==========================*/

     if (testPtr->argList != NULL)
       {
        BsaveExpression(testPtr->argList,fp);
       }

     testPtr = testPtr->nextArg;
    }
  }

#endif /* BLOAD_AND_BSAVE */

#endif /* (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) */

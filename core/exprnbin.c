   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.23  01/31/05            */
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
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
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
#include "envrnmnt.h"

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
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                        UpdateExpression(void *,EXEC_STATUS,void *,long);

/***********************************************************/
/* AllocateExpressions: Determines the amount of space     */
/*   required for loading the binary image of expressions  */
/*   and allocates that amount of space.                   */
/***********************************************************/
globle void AllocateExpressions(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;

   GenReadBinary(theEnv,execStatus,(void *) &ExpressionData(theEnv,execStatus)->NumberOfExpressions,sizeof(long));
   if (ExpressionData(theEnv,execStatus)->NumberOfExpressions == 0L)
     ExpressionData(theEnv,execStatus)->ExpressionArray = NULL;
   else
     {
      space = ExpressionData(theEnv,execStatus)->NumberOfExpressions * sizeof(struct expr);
      ExpressionData(theEnv,execStatus)->ExpressionArray = (struct expr *) genalloc(theEnv,execStatus,space);
     }
  }

/**********************************************/
/* RefreshExpressions: Refreshes the pointers */
/*   used by the expression binary image.     */
/**********************************************/
globle void RefreshExpressions(
  void *theEnv,
  EXEC_STATUS)
  {
   if (ExpressionData(theEnv,execStatus)->ExpressionArray == NULL) return;

   BloadandRefresh(theEnv,execStatus,ExpressionData(theEnv,execStatus)->NumberOfExpressions,
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
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   BSAVE_EXPRESSION *bexp;
   long theIndex;

   bexp = (BSAVE_EXPRESSION *) buf;
   ExpressionData(theEnv,execStatus)->ExpressionArray[obji].type = bexp->type;
   switch(bexp->type)
     {
      case FCALL:
        ExpressionData(theEnv,execStatus)->ExpressionArray[obji].value = (void *) BloadData(theEnv,execStatus)->FunctionArray[bexp->value];
        break;

      case GCALL:
#if DEFGENERIC_CONSTRUCT
        ExpressionData(theEnv,execStatus)->ExpressionArray[obji].value = (void *) GenericPointer(bexp->value);
#else
        ExpressionData(theEnv,execStatus)->ExpressionArray[obji].value = NULL;
#endif
        break;

      case PCALL:
#if DEFFUNCTION_CONSTRUCT
        ExpressionData(theEnv,execStatus)->ExpressionArray[obji].value = (void *) DeffunctionPointer(bexp->value);
#else
        ExpressionData(theEnv,execStatus)->ExpressionArray[obji].value = NULL;
#endif
        break;

      case DEFTEMPLATE_PTR:
#if DEFTEMPLATE_CONSTRUCT
        ExpressionData(theEnv,execStatus)->ExpressionArray[obji].value = (void *) DeftemplatePointer(bexp->value);
#else
        ExpressionData(theEnv,execStatus)->ExpressionArray[obji].value = NULL;
#endif
        break;

     case DEFCLASS_PTR:
#if OBJECT_SYSTEM
        ExpressionData(theEnv,execStatus)->ExpressionArray[obji].value = (void *) DefclassPointer(bexp->value);
#else
        ExpressionData(theEnv,execStatus)->ExpressionArray[obji].value = NULL;
#endif
        break;

      case DEFGLOBAL_PTR:

#if DEFGLOBAL_CONSTRUCT
        ExpressionData(theEnv,execStatus)->ExpressionArray[obji].value = (void *) DefglobalPointer(bexp->value);
#else
        ExpressionData(theEnv,execStatus)->ExpressionArray[obji].value = NULL;
#endif
        break;


      case INTEGER:
        ExpressionData(theEnv,execStatus)->ExpressionArray[obji].value = (void *) SymbolData(theEnv,execStatus)->IntegerArray[bexp->value];
        IncrementIntegerCount((INTEGER_HN *) ExpressionData(theEnv,execStatus)->ExpressionArray[obji].value);
        break;

      case FLOAT:
        ExpressionData(theEnv,execStatus)->ExpressionArray[obji].value = (void *) SymbolData(theEnv,execStatus)->FloatArray[bexp->value];
        IncrementFloatCount((FLOAT_HN *) ExpressionData(theEnv,execStatus)->ExpressionArray[obji].value);
        break;

      case INSTANCE_NAME:
#if ! OBJECT_SYSTEM
        ExpressionData(theEnv,execStatus)->ExpressionArray[obji].type = SYMBOL;
#endif
      case GBL_VARIABLE:
      case SYMBOL:
      case STRING:
        ExpressionData(theEnv,execStatus)->ExpressionArray[obji].value = (void *) SymbolData(theEnv,execStatus)->SymbolArray[bexp->value];
        IncrementSymbolCount((SYMBOL_HN *) ExpressionData(theEnv,execStatus)->ExpressionArray[obji].value);
        break;

#if DEFTEMPLATE_CONSTRUCT
      case FACT_ADDRESS:
        ExpressionData(theEnv,execStatus)->ExpressionArray[obji].value = (void *) &FactData(theEnv,execStatus)->DummyFact;
        EnvIncrementFactCount(theEnv,execStatus,ExpressionData(theEnv,execStatus)->ExpressionArray[obji].value);
        break;
#endif

#if OBJECT_SYSTEM
      case INSTANCE_ADDRESS:
        ExpressionData(theEnv,execStatus)->ExpressionArray[obji].value = (void *) &InstanceData(theEnv,execStatus)->DummyInstance;
        EnvIncrementInstanceCount(theEnv,execStatus,ExpressionData(theEnv,execStatus)->ExpressionArray[obji].value);
        break;
#endif

      case EXTERNAL_ADDRESS:
        ExpressionData(theEnv,execStatus)->ExpressionArray[obji].value = NULL;
        break;

      case RVOID:
        break;

      default:
        if (EvaluationData(theEnv,execStatus)->PrimitivesArray[bexp->type] == NULL) break;
        if (EvaluationData(theEnv,execStatus)->PrimitivesArray[bexp->type]->bitMap)
          {
           ExpressionData(theEnv,execStatus)->ExpressionArray[obji].value = (void *) SymbolData(theEnv,execStatus)->BitMapArray[bexp->value];
           IncrementBitMapCount((BITMAP_HN *) ExpressionData(theEnv,execStatus)->ExpressionArray[obji].value);
          }
        break;
     }

   theIndex = (long int) bexp->nextArg;
   if (theIndex == -1L)
     { ExpressionData(theEnv,execStatus)->ExpressionArray[obji].nextArg = NULL; }
   else
     { ExpressionData(theEnv,execStatus)->ExpressionArray[obji].nextArg = (struct expr *) &ExpressionData(theEnv,execStatus)->ExpressionArray[theIndex]; }

   theIndex = (long int) bexp->argList;
   if (theIndex == -1L)
     { ExpressionData(theEnv,execStatus)->ExpressionArray[obji].argList = NULL; }
   else
     { ExpressionData(theEnv,execStatus)->ExpressionArray[obji].argList = (struct expr *) &ExpressionData(theEnv,execStatus)->ExpressionArray[theIndex]; }
  }

/*********************************************/
/* ClearBloadedExpressions: Clears the space */
/*   utilized by an expression binary image. */
/*********************************************/
globle void ClearBloadedExpressions(
  void *theEnv,
  EXEC_STATUS)
  {
   unsigned long int i;
   size_t space;

   /*===============================================*/
   /* Update the busy counts of atomic data values. */
   /*===============================================*/

   for (i = 0; i < (unsigned long) ExpressionData(theEnv,execStatus)->NumberOfExpressions; i++)
     {
      switch (ExpressionData(theEnv,execStatus)->ExpressionArray[i].type)
        {
         case SYMBOL          :
         case STRING          :
         case INSTANCE_NAME   :
         case GBL_VARIABLE    :
           DecrementSymbolCount(theEnv,execStatus,(SYMBOL_HN *) ExpressionData(theEnv,execStatus)->ExpressionArray[i].value);
           break;
         case FLOAT           :
           DecrementFloatCount(theEnv,execStatus,(FLOAT_HN *) ExpressionData(theEnv,execStatus)->ExpressionArray[i].value);
           break;
         case INTEGER         :
           DecrementIntegerCount(theEnv,execStatus,(INTEGER_HN *) ExpressionData(theEnv,execStatus)->ExpressionArray[i].value);
           break;

#if DEFTEMPLATE_CONSTRUCT
         case FACT_ADDRESS    :
           EnvDecrementFactCount(theEnv,execStatus,ExpressionData(theEnv,execStatus)->ExpressionArray[i].value);
           break;
#endif

#if OBJECT_SYSTEM
         case INSTANCE_ADDRESS :
           EnvDecrementInstanceCount(theEnv,execStatus,ExpressionData(theEnv,execStatus)->ExpressionArray[i].value);
           break;
#endif

         case RVOID:
           break;

         default:
           if (EvaluationData(theEnv,execStatus)->PrimitivesArray[ExpressionData(theEnv,execStatus)->ExpressionArray[i].type] == NULL) break;
           if (EvaluationData(theEnv,execStatus)->PrimitivesArray[ExpressionData(theEnv,execStatus)->ExpressionArray[i].type]->bitMap)
             { DecrementBitMapCount(theEnv,execStatus,(BITMAP_HN *) ExpressionData(theEnv,execStatus)->ExpressionArray[i].value); }
           break;
        }
     }

   /*===================================*/
   /* Free the binary expression array. */
   /*===================================*/

   space = ExpressionData(theEnv,execStatus)->NumberOfExpressions * sizeof(struct expr);
   if (space != 0) genfree(theEnv,execStatus,(void *) ExpressionData(theEnv,execStatus)->ExpressionArray,space);
   ExpressionData(theEnv,execStatus)->ExpressionArray = 0;
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
globle void FindHashedExpressions(
  void *theEnv,
  EXEC_STATUS)
  {
   register unsigned i;
   EXPRESSION_HN *exphash;

   for (i = 0 ; i < EXPRESSION_HASH_SIZE ; i++)
     for (exphash = ExpressionData(theEnv,execStatus)->ExpressionHashTable[i] ; exphash != NULL ; exphash = exphash->next)
       {
        MarkNeededItems(theEnv,execStatus,exphash->exp);
        exphash->bsaveID = ExpressionData(theEnv,execStatus)->ExpressionCount;
        ExpressionData(theEnv,execStatus)->ExpressionCount += ExpressionSize(exphash->exp);
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
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   register unsigned i;
   EXPRESSION_HN *exphash;

   for (i = 0 ; i < EXPRESSION_HASH_SIZE ; i++)
     for (exphash = ExpressionData(theEnv,execStatus)->ExpressionHashTable[i] ; exphash != NULL ; exphash = exphash->next)
       BsaveExpression(theEnv,execStatus,exphash->exp,fp);
  }

/***************************************************************/
/* BsaveConstructExpressions: Writes all expression needed by  */
/*   constructs for this binary image to the binary save file. */
/***************************************************************/
globle void BsaveConstructExpressions(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   struct BinaryItem *biPtr;

   for (biPtr = BsaveData(theEnv,execStatus)->ListOfBinaryItems;
        biPtr != NULL;
        biPtr = biPtr->next)
     {
      if (biPtr->expressionFunction != NULL)
        { (*biPtr->expressionFunction)(theEnv,execStatus,fp); }
     }
  }

/***************************************/
/* BsaveExpression: Recursively saves  */
/*   an expression to the binary file. */
/***************************************/
globle void BsaveExpression(
  void *theEnv,
  EXEC_STATUS,
  struct expr *testPtr,
  FILE *fp)
  {
   BSAVE_EXPRESSION newTest;
   long int newIndex;

   while (testPtr != NULL)
     {
      ExpressionData(theEnv,execStatus)->ExpressionCount++;

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
        { newTest.argList = ExpressionData(theEnv,execStatus)->ExpressionCount; }

      /*========================================*/
      /* Convert the nextArg slot to an index. */
      /*========================================*/

      if (testPtr->nextArg == NULL)
        { newTest.nextArg = -1L; }
      else
        {
         newIndex = ExpressionData(theEnv,execStatus)->ExpressionCount + ExpressionSize(testPtr->argList);
         newTest.nextArg = newIndex;
        }

      /*=========================*/
      /* Convert the value slot. */
      /*=========================*/

      switch(testPtr->type)
        {
         case FLOAT:
           newTest.value = (long) ((FLOAT_HN *) testPtr->value)->bucket;
           break;

         case INTEGER:
           newTest.value = (long) ((INTEGER_HN *) testPtr->value)->bucket;
           break;

         case FCALL:
           newTest.value = (long) ((struct FunctionDefinition *)
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
           newTest.value = (long) ((SYMBOL_HN *) testPtr->value)->bucket;
           break;

         case FACT_ADDRESS:
         case INSTANCE_ADDRESS:
         case EXTERNAL_ADDRESS:
           newTest.value = -1L;
           break;

         case RVOID:
           break;

         default:
           if (EvaluationData(theEnv,execStatus)->PrimitivesArray[testPtr->type] == NULL) break;
           if (EvaluationData(theEnv,execStatus)->PrimitivesArray[testPtr->type]->bitMap)
             { newTest.value = (long) ((BITMAP_HN *) testPtr->value)->bucket; }
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
        BsaveExpression(theEnv,execStatus,testPtr->argList,fp);
       }

     testPtr = testPtr->nextArg;
    }
  }

#endif /* BLOAD_AND_BSAVE */

#endif /* (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) */


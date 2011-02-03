   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.24  06/05/06          */
   /*                                                     */
   /*                     BSAVE MODULE                    */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides core routines for saving constructs to  */
/*   a binary file.                                          */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added environment parameter to GenClose.       */
/*            Added environment parameter to GenOpen.        */
/*                                                           */
/*************************************************************/

#define _BSAVE_SOURCE_

#include "setup.h"

#include "argacces.h"
#include "bload.h"
#include "cstrnbin.h"
#include "envrnmnt.h"
#include "exprnpsr.h"
#include "memalloc.h"
#include "moduldef.h"
#include "router.h"
#include "symblbin.h"

#include "bsave.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if BLOAD_AND_BSAVE
   static void                        FindNeededItems(void *,EXEC_STATUS);
   static void                        InitializeFunctionNeededFlags(void *,EXEC_STATUS);
   static void                        WriteNeededFunctions(void *,EXEC_STATUS,FILE *);
   static size_t                      FunctionBinarySize(void *,EXEC_STATUS);
   static void                        WriteBinaryHeader(void *,EXEC_STATUS,FILE *);
   static void                        WriteBinaryFooter(void *,EXEC_STATUS,FILE *);
#endif
   static void                        DeallocateBsaveData(void *,EXEC_STATUS);

/**********************************************/
/* InitializeBsaveData: Allocates environment */
/*    data for the bsave command.             */
/**********************************************/
globle void InitializeBsaveData(
  void *theEnv,
  EXEC_STATUS)
  {
   AllocateEnvironmentData(theEnv,execStatus,BSAVE_DATA,sizeof(struct bsaveData),DeallocateBsaveData);
  }
  
/************************************************/
/* DeallocateBsaveData: Deallocates environment */
/*    data for the bsave command.               */
/************************************************/
static void DeallocateBsaveData(
  void *theEnv,
  EXEC_STATUS)
  {
   struct BinaryItem *tmpPtr, *nextPtr;
   
   tmpPtr = BsaveData(theEnv,execStatus)->ListOfBinaryItems;
   while (tmpPtr != NULL)
     {
      nextPtr = tmpPtr->next;
      rtn_struct(theEnv,execStatus,BinaryItem,tmpPtr);
      tmpPtr = nextPtr;
     }
  }

/**************************************/
/* BsaveCommand: H/L access routine   */
/*   for the bsave command.           */
/**************************************/
globle int BsaveCommand(
  void *theEnv,
	EXEC_STATUS)
  {
#if (! RUN_TIME) && BLOAD_AND_BSAVE
   char *fileName;

   if (EnvArgCountCheck(theEnv,execStatus,"bsave",EXACTLY,1) == -1) return(FALSE);
   fileName = GetFileName(theEnv,execStatus,"bsave",1);
   if (fileName != NULL)
     { if (EnvBsave(theEnv,execStatus,fileName)) return(TRUE); }
#else
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv,execStatus)
#endif
#endif
   return(FALSE);
  }

#if BLOAD_AND_BSAVE

/******************************/
/* EnvBsave: C access routine */
/*   for the bsave command.   */
/******************************/
globle intBool EnvBsave(
  void *theEnv,
  EXEC_STATUS,
  char *fileName)
  {
   FILE *fp;
   struct BinaryItem *biPtr;
   char constructBuffer[CONSTRUCT_HEADER_SIZE];
   long saveExpressionCount;

   /*===================================*/
   /* A bsave can't occur when a binary */
   /* image is already loaded.          */
   /*===================================*/

   if (Bloaded(theEnv,execStatus))
     {
      PrintErrorID(theEnv,execStatus,"BSAVE",1,FALSE);
      EnvPrintRouter(theEnv,WERROR,
          "Cannot perform a binary save while a binary load is in effect.\n");
      return(0);
     }

   /*================*/
   /* Open the file. */
   /*================*/

   if ((fp = GenOpen(theEnv,execStatus,fileName,"wb")) == NULL)
     {
      OpenErrorMessage(theEnv,execStatus,"bsave",fileName);
      return(0);
     }

   /*==============================*/
   /* Remember the current module. */
   /*==============================*/

   SaveCurrentModule(theEnv,execStatus);

   /*==================================*/
   /* Write binary header to the file. */
   /*==================================*/

   WriteBinaryHeader(theEnv,execStatus,fp);

   /*===========================================*/
   /* Initialize count variables, index values, */
   /* and determine some of the data structures */
   /* which need to be saved.                   */
   /*===========================================*/

   ExpressionData(theEnv,execStatus)->ExpressionCount = 0;
   InitializeFunctionNeededFlags(theEnv,execStatus);
   InitAtomicValueNeededFlags(theEnv,execStatus);
   FindHashedExpressions(theEnv,execStatus);
   FindNeededItems(theEnv,execStatus);
   SetAtomicValueIndices(theEnv,execStatus,FALSE);

   /*===============================*/
   /* Save the functions and atoms. */
   /*===============================*/

   WriteNeededFunctions(theEnv,execStatus,fp);
   WriteNeededAtomicValues(theEnv,execStatus,fp);

   /*=========================================*/
   /* Write out the number of expression data */
   /* structures in the binary image.         */
   /*=========================================*/

   GenWrite((void *) &ExpressionData(theEnv,execStatus)->ExpressionCount,(unsigned long) sizeof(unsigned long),fp);

   /*===========================================*/
   /* Save the numbers indicating the amount of */
   /* memory needed to bload the constructs.    */
   /*===========================================*/

   for (biPtr = BsaveData(theEnv,execStatus)->ListOfBinaryItems;
        biPtr != NULL;
        biPtr = biPtr->next)
     {
      if (biPtr->bsaveStorageFunction != NULL)
        {
         genstrncpy(constructBuffer,biPtr->name,CONSTRUCT_HEADER_SIZE);
         GenWrite(constructBuffer,(unsigned long) CONSTRUCT_HEADER_SIZE,fp);
         (*biPtr->bsaveStorageFunction)(theEnv,execStatus,fp);
        }
     }

   /*====================================*/
   /* Write a binary footer to the file. */
   /*====================================*/

   WriteBinaryFooter(theEnv,execStatus,fp);

   /*===================*/
   /* Save expressions. */
   /*===================*/

   ExpressionData(theEnv,execStatus)->ExpressionCount = 0;
   BsaveHashedExpressions(theEnv,execStatus,fp);
   saveExpressionCount = ExpressionData(theEnv,execStatus)->ExpressionCount;
   BsaveConstructExpressions(theEnv,execStatus,fp);
   ExpressionData(theEnv,execStatus)->ExpressionCount = saveExpressionCount;

   /*===================*/
   /* Save constraints. */
   /*===================*/

   WriteNeededConstraints(theEnv,execStatus,fp);

   /*==================*/
   /* Save constructs. */
   /*==================*/

   for (biPtr = BsaveData(theEnv,execStatus)->ListOfBinaryItems;
        biPtr != NULL;
        biPtr = biPtr->next)
     {
      if (biPtr->bsaveFunction != NULL)
        {
         genstrncpy(constructBuffer,biPtr->name,CONSTRUCT_HEADER_SIZE);
         GenWrite(constructBuffer,(unsigned long) CONSTRUCT_HEADER_SIZE,fp);
         (*biPtr->bsaveFunction)(theEnv,execStatus,fp);
        }
     }

   /*===================================*/
   /* Save a binary footer to the file. */
   /*===================================*/

   WriteBinaryFooter(theEnv,execStatus,fp);

   /*===========*/
   /* Clean up. */
   /*===========*/

   RestoreAtomicValueBuckets(theEnv,execStatus);

   /*=================*/
   /* Close the file. */
   /*=================*/

   GenClose(theEnv,execStatus,fp);

   /*=============================*/
   /* Restore the current module. */
   /*=============================*/

   RestoreCurrentModule(theEnv,execStatus);

   /*========================================*/
   /* Return TRUE to indicate success. */
   /*========================================*/

   return(TRUE);
  }

/*********************************************/
/* InitializeFunctionNeededFlags: Marks each */
/*   function in the list of functions as    */
/*   being unneeded by this binary image.    */
/*********************************************/
static void InitializeFunctionNeededFlags(
  void *theEnv,
  EXEC_STATUS)
  {
   struct FunctionDefinition *functionList;

   for (functionList = GetFunctionList(theEnv,execStatus);
        functionList != NULL;
        functionList = functionList->next)
     { functionList->bsaveIndex = 0; }
  }

/**********************************************************/
/* FindNeededItems: Searches through the constructs for   */
/*   the functions, constraints, or atoms that are needed */
/*   by that construct. This routine also counts the      */
/*   number of expressions in use (through a global).     */
/**********************************************************/
static void FindNeededItems(
  void *theEnv,
  EXEC_STATUS)
  {
   struct BinaryItem *biPtr;

   for (biPtr = BsaveData(theEnv,execStatus)->ListOfBinaryItems;
        biPtr != NULL;
        biPtr = biPtr->next)
     { if (biPtr->findFunction != NULL) (*biPtr->findFunction)(theEnv,execStatus); }
  }

/****************************************************/
/* WriteNeededFunctions: Writes the names of needed */
/*   functions to the binary save file.             */
/****************************************************/
static void WriteNeededFunctions(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   unsigned long int count = 0;
   size_t space, length;
   struct FunctionDefinition *functionList;

   /*================================================*/
   /* Assign each function an index if it is needed. */
   /*================================================*/

   for (functionList = GetFunctionList(theEnv,execStatus);
        functionList != NULL;
        functionList = functionList->next)
     {
      if (functionList->bsaveIndex)
        { functionList->bsaveIndex = (short int) count++; }
      else
        { functionList->bsaveIndex = -1; }
     }

   /*===================================================*/
   /* Write the number of function names to be written. */
   /*===================================================*/

   GenWrite(&count,(unsigned long) sizeof(unsigned long int),fp);
   if (count == 0)
     {
      GenWrite(&count,(unsigned long) sizeof(unsigned long int),fp);
      return;
     }

   /*================================*/
   /* Determine the amount of space  */
   /* needed for the function names. */
   /*================================*/

   space = FunctionBinarySize(theEnv,execStatus);
   GenWrite(&space,(unsigned long) sizeof(unsigned long int),fp);

   /*===============================*/
   /* Write out the function names. */
   /*===============================*/

   for (functionList = GetFunctionList(theEnv,execStatus);
        functionList != NULL;
        functionList = functionList->next)
     {
      if (functionList->bsaveIndex >= 0)
        {
         length = strlen(ValueToString(functionList->callFunctionName)) + 1;
         GenWrite(ValueToString(functionList->callFunctionName),(unsigned long) length,fp);
        }
     }
  }

/*********************************************/
/* FunctionBinarySize: Determines the number */
/*   of bytes needed to save all of the      */
/*   function names in the binary save file. */
/*********************************************/
static size_t FunctionBinarySize(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t size = 0;
   struct FunctionDefinition *functionList;

   for (functionList = GetFunctionList(theEnv,execStatus);
        functionList != NULL;
        functionList = functionList->next)
     {
      if (functionList->bsaveIndex >= 0)
        { size += strlen(ValueToString(functionList->callFunctionName)) + 1; }
     }

   return(size);
  }

/***************************************************/
/* SaveBloadCount: Used to save the data structure */
/*   count values when a binary save command is    */
/*   issued when a binary image is loaded.         */
/***************************************************/
globle void SaveBloadCount(
  void *theEnv,
  EXEC_STATUS,
  long cnt)
  {
   BLOADCNTSV *tmp, *prv;

   tmp = get_struct(theEnv,bloadcntsv);
   tmp->val = cnt;
   tmp->nxt = NULL;

   if (BsaveData(theEnv,execStatus)->BloadCountSaveTop == NULL)
     { BsaveData(theEnv,execStatus)->BloadCountSaveTop = tmp; }
   else
     {
      prv = BsaveData(theEnv,execStatus)->BloadCountSaveTop;
      while (prv->nxt != NULL)
        { prv = prv->nxt; }
      prv->nxt = tmp;
     }
  }

/**************************************************/
/* RestoreBloadCount: Restores the data structure */
/*   count values after a binary save command is  */
/*   completed when a binary image is loaded.     */
/**************************************************/
globle void RestoreBloadCount(
  void *theEnv,
  EXEC_STATUS,
  long *cnt)
  {
   BLOADCNTSV *tmp;

   *cnt = BsaveData(theEnv,execStatus)->BloadCountSaveTop->val;
   tmp = BsaveData(theEnv,execStatus)->BloadCountSaveTop;
   BsaveData(theEnv,execStatus)->BloadCountSaveTop = BsaveData(theEnv,execStatus)->BloadCountSaveTop->nxt;
   rtn_struct(theEnv,execStatus,bloadcntsv,tmp);
  }

/**********************************************/
/* MarkNeededItems: Examines an expression to */
/*   determine which items are needed to save */
/*   an expression as part of a binary image. */
/**********************************************/
globle void MarkNeededItems(
  void *theEnv,
  EXEC_STATUS,
  struct expr *testPtr)
  {
   while (testPtr != NULL)
     {
      switch (testPtr->type)
        {
         case SYMBOL:
         case STRING:
         case GBL_VARIABLE:
         case INSTANCE_NAME:
            ((SYMBOL_HN *) testPtr->value)->neededSymbol = TRUE;
            break;

         case FLOAT:
            ((FLOAT_HN *) testPtr->value)->neededFloat = TRUE;
            break;

         case INTEGER:
            ((INTEGER_HN *) testPtr->value)->neededInteger = TRUE;
            break;

         case FCALL:
            ((struct FunctionDefinition *) testPtr->value)->bsaveIndex = TRUE;
            break;

         case RVOID:
           break;

         default:
           if (EvaluationData(theEnv,execStatus)->PrimitivesArray[testPtr->type] == NULL) break;
           if (EvaluationData(theEnv,execStatus)->PrimitivesArray[testPtr->type]->bitMap)
             { ((BITMAP_HN *) testPtr->value)->neededBitMap = TRUE; }
           break;

        }

      if (testPtr->argList != NULL)
        { MarkNeededItems(theEnv,execStatus,testPtr->argList); }

      testPtr = testPtr->nextArg;
     }
  }

/******************************************************/
/* WriteBinaryHeader: Writes a binary header used for */
/*   verification when a binary image is loaded.      */
/******************************************************/
static void WriteBinaryHeader(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   GenWrite(BloadData(theEnv,execStatus)->BinaryPrefixID,(unsigned long) strlen(BloadData(theEnv,execStatus)->BinaryPrefixID) + 1,fp);
   GenWrite(BloadData(theEnv,execStatus)->BinaryVersionID,(unsigned long) strlen(BloadData(theEnv,execStatus)->BinaryVersionID) + 1,fp);
  }

/******************************************************/
/* WriteBinaryFooter: Writes a binary footer used for */
/*   verification when a binary image is loaded.      */
/******************************************************/
static void WriteBinaryFooter(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   char footerBuffer[CONSTRUCT_HEADER_SIZE];

   genstrncpy(footerBuffer,BloadData(theEnv,execStatus)->BinaryPrefixID,CONSTRUCT_HEADER_SIZE);
   GenWrite(footerBuffer,(unsigned long) CONSTRUCT_HEADER_SIZE,fp);
  }

#endif /* BLOAD_AND_BSAVE */

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE

/**********************************************************/
/* AddBinaryItem: Informs the bload/bsave commands of the */
/*   appropriate access functions needed to save/load the */
/*   data structures of a construct or other "item" to a  */
/*   binary file.                                         */
/**********************************************************/
globle intBool AddBinaryItem(
  void *theEnv,
  EXEC_STATUS,
  char *name,
  int priority,
  void (*findFunction)(void *,EXEC_STATUS),
  void (*expressionFunction)(void *,EXEC_STATUS,FILE *),
  void (*bsaveStorageFunction)(void *,EXEC_STATUS,FILE *),
  void (*bsaveFunction)(void *,EXEC_STATUS,FILE *),
  void (*bloadStorageFunction)(void *,EXEC_STATUS),
  void (*bloadFunction)(void *,EXEC_STATUS),
  void (*clearFunction)(void *,EXEC_STATUS))
  {
   struct BinaryItem *newPtr, *currentPtr, *lastPtr = NULL;

   /*========================================*/
   /* Create the binary item data structure. */
   /*========================================*/

   newPtr = get_struct(theEnv,BinaryItem);

   newPtr->name = name;
   newPtr->findFunction = findFunction;
   newPtr->expressionFunction = expressionFunction;
   newPtr->bsaveStorageFunction = bsaveStorageFunction;
   newPtr->bsaveFunction = bsaveFunction;
   newPtr->bloadStorageFunction = bloadStorageFunction;
   newPtr->bloadFunction = bloadFunction;
   newPtr->clearFunction = clearFunction;
   newPtr->priority = priority;

   /*=================================*/
   /* If no binary items are defined, */
   /* just put the item on the list.  */
   /*=================================*/

   if (BsaveData(theEnv,execStatus)->ListOfBinaryItems == NULL)
     {
      newPtr->next = NULL;
      BsaveData(theEnv,execStatus)->ListOfBinaryItems = newPtr;
      return(TRUE);
     }

   /*=========================================*/
   /* Otherwise, place the binary item at the */
   /* appropriate place in the list of binary */
   /* items based on its priority.            */
   /*=========================================*/

   currentPtr = BsaveData(theEnv,execStatus)->ListOfBinaryItems;
   while ((currentPtr != NULL) ? (priority < currentPtr->priority) : FALSE)
     {
      lastPtr = currentPtr;
      currentPtr = currentPtr->next;
     }

   if (lastPtr == NULL)
     {
      newPtr->next = BsaveData(theEnv,execStatus)->ListOfBinaryItems;
      BsaveData(theEnv,execStatus)->ListOfBinaryItems = newPtr;
     }
   else
     {
      newPtr->next = currentPtr;
      lastPtr->next = newPtr;
     }

   /*==================================*/
   /* Return TRUE to indicate the item */
   /* was successfully added.          */
   /*==================================*/

   return(TRUE);
  }

#endif /* BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE */






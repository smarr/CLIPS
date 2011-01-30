   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/13/98          */
   /*                                                     */
   /*                     BSAVE MODULE                    */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides core routines for saving constructs to  */
/*   a binary file.                                          */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _BSAVE_SOURCE_

#include "setup.h"

#include "memalloc.h"
#include "exprnpsr.h"
#include "argacces.h"
#include "router.h"
#include "cstrnbin.h"
#include "moduldef.h"
#include "symblbin.h"
#include "crc.h"
#include "bload.h"

#include "bsave.h"

/*******************/
/* DATA STRUCTURES */
/*******************/

#if BLOAD_AND_BSAVE

typedef struct bloadcntsv
  {
   long val;
   struct bloadcntsv *nxt;
} BLOADCNTSV;

#endif

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if BLOAD_AND_BSAVE
   static void                        FindNeededItems(void);
   static void                        InitializeFunctionNeededFlags(void);
   static void                        WriteNeededFunctions(FILE *);
   static unsigned long int           FunctionBinarySize(void);
   static void                        WriteBinaryHeader(FILE *);
   static void                        WriteBinaryFooter(FILE *);
#endif

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle struct BinaryItem         *ListOfBinaryItems = NULL;

/****************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS  */
/****************************************/

#if BLOAD_AND_BSAVE
   Thread static BLOADCNTSV                *BloadCountSaveTop = NULL;
#endif

/**************************************/
/* BsaveCommand: H/L access routine   */
/*   for the bsave command.           */
/**************************************/
globle int BsaveCommand()
  {
#if (! RUN_TIME) && BLOAD_AND_BSAVE
   char *fileName;

   if (ArgCountCheck("bsave",EXACTLY,1) == -1) return(FALSE);
   fileName = GetFileName("bsave",1);
   if (fileName != NULL)
     { if (Bsave(fileName)) return(TRUE); }
#endif
   return(FALSE);
  }

#if BLOAD_AND_BSAVE

/****************************/
/* Bsave: C access routine  */
/*   for the bsave command. */
/****************************/
globle BOOLEAN Bsave(
  char *fileName)
  {
   FILE *fp;
   struct BinaryItem *biPtr;
   char constructBuffer[CONSTRUCT_HEADER_SIZE];
   long saveExpressionCount;
   char   CRCstr[9];
   size_t CRCpos;
   size_t pos;
   unsigned long myCRC = 0xFFFFFFFF;

   /*===================================*/
   /* A bsave can't occur when a binary */
   /* image is already loaded.          */
   /*===================================*/

   if (Bloaded())
     {
      PrintErrorID("BSAVE",1,FALSE);
      PrintRouter(WERROR,
          "Cannot perform a binary save while a binary load is in effect.\n");
      return(0);
     }

   /*================*/
   /* Open the file. */
   /*================*/

   if ((fp = fopen(fileName,"wb")) == NULL)
     {
      OpenErrorMessage("bsave",fileName);
      return(0);
     }

   /*==============================*/
   /* Remember the current module. */
   /*==============================*/

   SaveCurrentModule();

   /*==================================*/
   /* Write binary header to the file. */
   /*==================================*/

   WriteBinaryHeader(fp);

   CRCpos = ftell(fp);	
   memset(CRCstr,0,9);
   sprintf(CRCstr,"%X",myCRC);
   GenWrite(CRCstr,8,fp);
   pos = ftell(fp);

   /*===========================================*/
   /* Initialize count variables, index values, */
   /* and determine some of the data structures */
   /* which need to be saved.                   */
   /*===========================================*/

   ExpressionCount = 0;
   InitializeFunctionNeededFlags();
   InitAtomicValueNeededFlags();
   FindHashedExpressions();
   FindNeededItems();
   SetAtomicValueIndices(FALSE);

   /*===============================*/
   /* Save the functions and atoms. */
   /*===============================*/

   WriteNeededFunctions(fp);
   WriteNeededAtomicValues(fp);

   /*=========================================*/
   /* Write out the number of expression data */
   /* structures in the binary image.         */
   /*=========================================*/

   GenWrite((void *) &ExpressionCount,(unsigned long) sizeof(unsigned long),fp);

   /*===========================================*/
   /* Save the numbers indicating the amount of */
   /* memory needed to bload the constructs.    */
   /*===========================================*/

   for (biPtr = ListOfBinaryItems;
        biPtr != NULL;
        biPtr = biPtr->next)
     {
      if (biPtr->bsaveStorageFunction != NULL)
        {
         strncpy(constructBuffer,biPtr->name,CONSTRUCT_HEADER_SIZE);
         GenWrite(constructBuffer,(unsigned long) CONSTRUCT_HEADER_SIZE,fp);
         (*biPtr->bsaveStorageFunction)(fp);
        }
     }

   /*====================================*/
   /* Write a binary footer to the file. */
   /*====================================*/

   WriteBinaryFooter(fp);

   /*===================*/
   /* Save expressions. */
   /*===================*/

   ExpressionCount = 0;
   BsaveHashedExpressions(fp);
   saveExpressionCount = ExpressionCount;
   BsaveConstructExpressions(fp);
   ExpressionCount = saveExpressionCount;

   /*===================*/
   /* Save constraints. */
   /*===================*/

   WriteNeededConstraints(fp);

   /*==================*/
   /* Save constructs. */
   /*==================*/

   for (biPtr = ListOfBinaryItems;
        biPtr != NULL;
        biPtr = biPtr->next)
     {
      if (biPtr->bsaveFunction != NULL)
        {
         strncpy(constructBuffer,biPtr->name,CONSTRUCT_HEADER_SIZE);
         GenWrite(constructBuffer,(unsigned long) CONSTRUCT_HEADER_SIZE,fp);
         (*biPtr->bsaveFunction)(fp);
        }
     }

   /*===================================*/
   /* Save a binary footer to the file. */
   /*===================================*/

   WriteBinaryFooter(fp);

   /*===========*/
   /* Clean up. */
   /*===========*/

   RestoreAtomicValueBuckets();

   fseek(fp, pos, SEEK_SET);

   myCRC = CalcCRC(fp);

   if (myCRC != 0L)	{
	   fflush(fp);
	   fseek(fp, CRCpos, SEEK_SET);
	   memset(CRCstr,0,9);
	   sprintf(CRCstr,"%X",myCRC);
	   GenWrite(CRCstr,8,fp);
	   fflush(fp);
   }
   else	{
	   fclose(fp);
	   return(FALSE);
   }

   /*=================*/
   /* Close the file. */
   /*=================*/

   fclose(fp);

   /*=============================*/
   /* Restore the current module. */
   /*=============================*/

   RestoreCurrentModule();

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
static void InitializeFunctionNeededFlags()
  {
   struct FunctionDefinition *functionList;

   for (functionList = GetFunctionList();
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
static void FindNeededItems()
  {
   struct BinaryItem *biPtr;

   for (biPtr = ListOfBinaryItems;
        biPtr != NULL;
        biPtr = biPtr->next)
     { if (biPtr->findFunction != NULL) (*biPtr->findFunction)(); }
  }

/****************************************************/
/* WriteNeededFunctions: Writes the names of needed */
/*   functions to the binary save file.             */
/****************************************************/
static void WriteNeededFunctions(
  FILE *fp)
  {
   unsigned long int space, count = 0, length;
   struct FunctionDefinition *functionList;

   /*================================================*/
   /* Assign each function an index if it is needed. */
   /*================================================*/

   for (functionList = GetFunctionList();
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

   space = FunctionBinarySize();
   GenWrite(&space,(unsigned long) sizeof(unsigned long int),fp);

   /*===============================*/
   /* Write out the function names. */
   /*===============================*/

   for (functionList = GetFunctionList();
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
static unsigned long int FunctionBinarySize()
  {
   unsigned long int size = 0;
   struct FunctionDefinition *functionList;

   for (functionList = GetFunctionList();
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
  long cnt)
  {
   BLOADCNTSV *tmp, *prv;

   tmp = get_struct(bloadcntsv);
   tmp->val = cnt;
   tmp->nxt = NULL;

   if (BloadCountSaveTop == NULL)
     { BloadCountSaveTop = tmp; }
   else
     {
      prv = BloadCountSaveTop;
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
  long *cnt)
  {
   BLOADCNTSV *tmp;

   *cnt = BloadCountSaveTop->val;
   tmp = BloadCountSaveTop;
   BloadCountSaveTop = BloadCountSaveTop->nxt;
   rtn_struct(bloadcntsv,tmp);
  }

/**********************************************/
/* MarkNeededItems: Examines an expression to */
/*   determine which items are needed to save */
/*   an expression as part of a binary image. */
/**********************************************/
globle void MarkNeededItems(
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
           if (PrimitivesArray[testPtr->type] == NULL) break;
           if (PrimitivesArray[testPtr->type]->bitMap)
             { ((BITMAP_HN *) testPtr->value)->neededBitMap = TRUE; }
           break;

        }

      if (testPtr->argList != NULL)
        { MarkNeededItems(testPtr->argList); }

      testPtr = testPtr->nextArg;
     }
  }

/******************************************************/
/* WriteBinaryHeader: Writes a binary header used for */
/*   verification when a binary image is loaded.      */
/******************************************************/
static void WriteBinaryHeader(
  FILE *fp)
  {
   GenWrite(BinaryPrefixID,(unsigned long) strlen(BinaryPrefixID) + 1,fp);
   GenWrite(BinaryVersionID,(unsigned long) strlen(BinaryVersionID) + 1,fp);
  }

/******************************************************/
/* WriteBinaryFooter: Writes a binary footer used for */
/*   verification when a binary image is loaded.      */
/******************************************************/
static void WriteBinaryFooter(
  FILE *fp)
  {
   char footerBuffer[CONSTRUCT_HEADER_SIZE];

   strncpy(footerBuffer,BinaryPrefixID,CONSTRUCT_HEADER_SIZE);
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
globle BOOLEAN AddBinaryItem(
  char *name,
  int priority,
  void (*findFunction)(void),
  void (*expressionFunction)(FILE *),
  void (*bsaveStorageFunction)(FILE *),
  void (*bsaveFunction)(FILE *),
  void (*bloadStorageFunction)(void),
  void (*bloadFunction)(void),
  void (*clearFunction)(void))
  {
   struct BinaryItem *newPtr, *currentPtr, *lastPtr = NULL;

   /*========================================*/
   /* Create the binary item data structure. */
   /*========================================*/

   newPtr = get_struct(BinaryItem);

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

   if (ListOfBinaryItems == NULL)
     {
      newPtr->next = NULL;
      ListOfBinaryItems = newPtr;
      return(TRUE);
     }

   /*=========================================*/
   /* Otherwise, place the binary item at the */
   /* appropriate place in the list of binary */
   /* items based on its priority.            */
   /*=========================================*/

   currentPtr = ListOfBinaryItems;
   while ((currentPtr != NULL) ? (priority < currentPtr->priority) : FALSE)
     {
      lastPtr = currentPtr;
      currentPtr = currentPtr->next;
     }

   if (lastPtr == NULL)
     {
      newPtr->next = ListOfBinaryItems;
      ListOfBinaryItems = newPtr;
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

#if BLOAD_AND_BSAVE || BSAVE_INSTANCES

/***********************************************/
/* GenWrite: Generic routine for writing to a  */
/*   file. No machine specific code as of yet. */
/***********************************************/
globle void GenWrite(
  void *dataPtr,
  unsigned long size,
  FILE *fp)
  {
   if (size == 0) return;
   fwrite(dataPtr,(CLIPS_STD_SIZE) size,1,fp);
  }

#endif /* BLOAD_AND_BSAVE || BSAVE_INSTANCES */

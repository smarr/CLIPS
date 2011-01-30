   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  05/23/96            */
   /*                                                     */
   /*                SORT FUNCTIONS MODULE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for sorting functions.         */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _SORTFUN_SOURCE_

#include "setup.h"

#include "argacces.h"
#include "evaluatn.h"
#include "extnfunc.h"
#include "memalloc.h"
#include "multifld.h"
#include "sysdep.h"

#include "sortfun.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    DoMergeSort(DATA_OBJECT *,DATA_OBJECT *,long,long,long,long,
                                              int (*)(DATA_OBJECT *,DATA_OBJECT *));
   static int                     DefaultCompareSwapFunction(DATA_OBJECT *,DATA_OBJECT *);

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static struct expr         *SortComparisonFunction;

#if ! RUN_TIME
/****************************************/
/* SortFunctionDefinitions: Initializes */
/*   the sorting functions.             */
/****************************************/
globle void SortFunctionDefinitions()
  {
   DefineFunction2("sort",'u', PTIF SortFunction,"SortFunction","1**w");
  }
#endif

/**************************************/
/* DefaultCompareSwapFunction:  */
/**************************************/
static int DefaultCompareSwapFunction(
  DATA_OBJECT *item1,
  DATA_OBJECT *item2)
  {
   DATA_OBJECT returnValue;

   SortComparisonFunction->argList = GenConstant(item1->type,item1->value);
   SortComparisonFunction->argList->nextArg = GenConstant(item2->type,item2->value);

   EvaluateExpression(SortComparisonFunction,&returnValue);

   ReturnExpression(SortComparisonFunction->argList);
   SortComparisonFunction->argList = NULL;

   if ((GetType(returnValue) == SYMBOL) &&
       (GetValue(returnValue) == FalseSymbol))
     { return(FALSE); }

   return(TRUE);
  }

/**************************************/
/* SortFunction: H/L access routine   */
/*   for the rest$ function.          */
/**************************************/
globle void SortFunction(
  DATA_OBJECT_PTR returnValue)
  {
   long argumentCount, i;
   DATA_OBJECT *theArguments;
   DATA_OBJECT theArg;
   struct multifield *theMultifield;
   char *functionName;
   struct expr *functionReference;

   /*==================================*/
   /* Set up the default return value. */
   /*==================================*/

   SetpType(returnValue,SYMBOL);
   SetpValue(returnValue,FalseSymbol);

   /*=============================================*/
   /* The function expects at least one argument. */
   /*=============================================*/

   if ((argumentCount = ArgCountCheck("sort",AT_LEAST,1)) == -1)
     { return; }

   /*=============================================*/
   /* Verify that the comparison function exists. */
   /*=============================================*/

   if (ArgTypeCheck("sort",1,SYMBOL,&theArg) == FALSE)
     { return; }

   functionName = DOToString(theArg);
   functionReference = FunctionReferenceExpression(functionName);
   if (functionReference == NULL)
     {
      ExpectedTypeError1("sort",1,"function name, deffunction name, or defgeneric name");
      return;
     }

   /*=====================================*/
   /* If there are no items to be sorted, */
   /* then return an empty multifield.    */
   /*=====================================*/

   if (argumentCount == 1)
     {
      SetMultifieldErrorValue(returnValue);
      ReturnExpression(functionReference);
      return;
     }

   theArguments = (DATA_OBJECT *) genalloc((argumentCount - 1) * sizeof(DATA_OBJECT));

   for (i = 2; i <= argumentCount; i++)
     {
      RtnUnknown(i,&theArguments[i-2]);
      if (GetType(theArguments[i-2]) == MULTIFIELD)
        {
         ExpectedTypeError1("sort",i,"single-field value");
         genfree(theArguments,(argumentCount - 1) * sizeof(DATA_OBJECT));
         ReturnExpression(functionReference);
         return;
        }
     }

   functionReference->nextArg = SortComparisonFunction;
   SortComparisonFunction = functionReference;

   MergeSort(argumentCount-1,theArguments,DefaultCompareSwapFunction);

   SortComparisonFunction = SortComparisonFunction->nextArg;
   functionReference->nextArg = NULL;
   ReturnExpression(functionReference);

   theMultifield = (struct multifield *) CreateMultifield(argumentCount-1);

   for (i = 2; i <= argumentCount; i++)
     {
      SetMFType(theMultifield,i-1,GetType(theArguments[i-2]));
      SetMFValue(theMultifield,i-1,GetValue(theArguments[i-2]));
     }

   genfree(theArguments,(argumentCount - 1) * sizeof(DATA_OBJECT));

   SetpType(returnValue,MULTIFIELD);
   SetpDOBegin(returnValue,1);
   SetpDOEnd(returnValue,argumentCount-1);
   SetpValue(returnValue,(void *) theMultifield);
  }


/*******************************************/
/* MergeSort: Sorts a list of DATA_OBJECTs */
/*   according to user specified criteria. */
/*******************************************/
void MergeSort(
  long listSize,
  DATA_OBJECT *theList,
  int (*swapFunction)(DATA_OBJECT *,DATA_OBJECT *))
  {
   DATA_OBJECT *tempList;
   long middle;

   if (listSize <= 1) return;

   /*==============================*/
   /* Create the temporary storage */
   /* needed for the merge sort.   */
   /*==============================*/

   tempList = (DATA_OBJECT *) genalloc(listSize * sizeof(DATA_OBJECT));

   /*=====================================*/
   /* Call the merge sort driver routine. */
   /*=====================================*/

   middle = (listSize + 1) / 2;
   DoMergeSort(theList,tempList,0,middle-1,middle,listSize - 1,swapFunction);

   /*==================================*/
   /* Deallocate the temporary storage */
   /* needed by the merge sort.        */
   /*==================================*/

   genfree(tempList,listSize * sizeof(DATA_OBJECT));
  }


/******************************************************/
/* DoMergeSort: Driver routine for performing a merge */
/*   sort on an array of DATA_OBJECT structures.      */
/******************************************************/
static void DoMergeSort(
  DATA_OBJECT *theList,
  DATA_OBJECT *tempList,
  long s1,
  long e1,
  long s2,
  long e2,
  int (*swapFunction)(DATA_OBJECT *,DATA_OBJECT *))
  {
   DATA_OBJECT temp;
   long middle, size;
   long c1, c2, mergePoint;

   /* Sort the two subareas before merging them. */

   if (s1 == e1)
     { /* List doesn't need to be merged. */ }
   else if ((s1 + 1) == e1)
     {
      if ((*swapFunction)(&theList[s1],&theList[e1]))
        {
         TransferDataObjectValues(&temp,&theList[s1]);
         TransferDataObjectValues(&theList[s1],&theList[e1]);
         TransferDataObjectValues(&theList[e1],&temp);
        }
     }
   else
     {
      size = ((e1 - s1) + 1);
      middle = s1 + ((size + 1) / 2);
      DoMergeSort(theList,tempList,s1,middle-1,middle,e1,swapFunction);
     }

   if (s2 == e2)
     { /* List doesn't need to be merged. */ }
   else if ((s2 + 1) == e2)
     {
      if ((*swapFunction)(&theList[s2],&theList[e2]))
        {
         TransferDataObjectValues(&temp,&theList[s2]);
         TransferDataObjectValues(&theList[s2],&theList[e2]);
         TransferDataObjectValues(&theList[e2],&temp);
        }
     }
   else
     {
      size = ((e2 - s2) + 1);
      middle = s2 + ((size + 1) / 2);
      DoMergeSort(theList,tempList,s2,middle-1,middle,e2,swapFunction);
     }

   /*======================*/
   /* Merge the two areas. */
   /*======================*/

   mergePoint = s1;
   c1 = s1;
   c2 = s2;

   while (mergePoint <= e2)
     {
      if (c1 > e1)
        {
         TransferDataObjectValues(&tempList[mergePoint],&theList[c2]);
         c2++;
         mergePoint++;
        }
      else if (c2 > e2)
        {
         TransferDataObjectValues(&tempList[mergePoint],&theList[c1]);
         c1++;
         mergePoint++;
        }
      else if ((*swapFunction)(&theList[c1],&theList[c2]))
        {
         TransferDataObjectValues(&tempList[mergePoint],&theList[c2]);
         c2++;
         mergePoint++;
        }
      else
        {
         TransferDataObjectValues(&tempList[mergePoint],&theList[c1]);
         c1++;
         mergePoint++;
        }
     }

   /*=======================================*/
   /* Copy them back to the original array. */
   /*=======================================*/

   for (c1 = s1; c1 <= e2; c1++)
     { TransferDataObjectValues(&theList[c1],&tempList[c1]); }
  }


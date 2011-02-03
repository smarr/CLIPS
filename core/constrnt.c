   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  07/01/05            */
   /*                                                     */
   /*                 CONSTRAINT MODULE                   */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides functions for creating and removing     */
/*   constraint records, adding them to the contraint hash   */
/*   table, and enabling and disabling static and dynamic    */
/*   constraint checking.                                    */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian Dantes                                         */
/*                                                           */
/* Revision History:                                         */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: Added allowed-classes slot facet.              */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#define _CONSTRNT_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <stdlib.h>

#include "setup.h"

#include "argacces.h"
#include "constant.h"
#include "envrnmnt.h"
#include "extnfunc.h"
#include "memalloc.h"
#include "multifld.h"
#include "router.h"
#include "scanner.h"

#include "constrnt.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if (! RUN_TIME) && (! BLOAD_ONLY)
   static void                     InstallConstraintRecord(void *,EXEC_STATUS,CONSTRAINT_RECORD *);
   static int                      ConstraintCompare(struct constraintRecord *,struct constraintRecord *);
#endif
#if (! RUN_TIME)
   static void                     ReturnConstraintRecord(void *,EXEC_STATUS,CONSTRAINT_RECORD *);
   static void                     DeinstallConstraintRecord(void *,EXEC_STATUS,CONSTRAINT_RECORD *);
#endif
   static void                     DeallocateConstraintData(void *,EXEC_STATUS);

/*****************************************************/
/* InitializeConstraints: Initializes the constraint */
/*   hash table to NULL and defines the static and   */
/*   dynamic constraint access functions.            */
/*****************************************************/
globle void InitializeConstraints(
  void *theEnv,
  EXEC_STATUS)
  {
#if (! RUN_TIME) && (! BLOAD_ONLY)
   int i;
#endif

   AllocateEnvironmentData(theEnv,execStatus,CONSTRAINT_DATA,sizeof(struct constraintData),DeallocateConstraintData);
   
   ConstraintData(theEnv,execStatus)->StaticConstraintChecking = TRUE;
   
#if (! RUN_TIME) && (! BLOAD_ONLY)

    ConstraintData(theEnv,execStatus)->ConstraintHashtable = (struct constraintRecord **)
                          gm2(theEnv,execStatus,(int) sizeof (struct constraintRecord *) *
                                    SIZE_CONSTRAINT_HASH);

    if (ConstraintData(theEnv,execStatus)->ConstraintHashtable == NULL) EnvExitRouter(theEnv,execStatus,EXIT_FAILURE);

    for (i = 0; i < SIZE_CONSTRAINT_HASH; i++) ConstraintData(theEnv,execStatus)->ConstraintHashtable[i] = NULL;
#endif

#if (! RUN_TIME)
   EnvDefineFunction2(theEnv,execStatus,"get-dynamic-constraint-checking",'b',GDCCommand,"GDCCommand", "00");
   EnvDefineFunction2(theEnv,execStatus,"set-dynamic-constraint-checking",'b',SDCCommand,"SDCCommand", "11");

   EnvDefineFunction2(theEnv,execStatus,"get-static-constraint-checking",'b',GSCCommand,"GSCCommand", "00");
   EnvDefineFunction2(theEnv,execStatus,"set-static-constraint-checking",'b',SSCCommand,"SSCCommand", "11");
#endif
  }
  
/*****************************************************/
/* DeallocateConstraintData: Deallocates environment */
/*    data for constraints.                          */
/*****************************************************/
static void DeallocateConstraintData(
  void *theEnv,
  EXEC_STATUS)
  {
#if ! RUN_TIME   
   struct constraintRecord *tmpPtr, *nextPtr;
   int i;

   for (i = 0; i < SIZE_CONSTRAINT_HASH; i++)
     {
      tmpPtr = ConstraintData(theEnv,execStatus)->ConstraintHashtable[i];
      while (tmpPtr != NULL)
        {
         nextPtr = tmpPtr->next;
         ReturnConstraintRecord(theEnv,execStatus,tmpPtr);
         tmpPtr = nextPtr;
        }
     }

   rm(theEnv,execStatus,ConstraintData(theEnv,execStatus)->ConstraintHashtable,
      (int) sizeof (struct constraintRecord *) * SIZE_CONSTRAINT_HASH);
#else
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv,execStatus)
#endif
#endif
      
#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME)
   if (ConstraintData(theEnv,execStatus)->NumberOfConstraints != 0)
     {
      genfree(theEnv,execStatus,(void *) ConstraintData(theEnv,execStatus)->ConstraintArray,
              (sizeof(CONSTRAINT_RECORD) * ConstraintData(theEnv,execStatus)->NumberOfConstraints));
     }
#endif
  }

#if (! RUN_TIME)

/*************************************************************/
/* ReturnConstraintRecord: Frees the data structures used by */
/*   a constraint record. If the returnOnlyFields argument   */
/*   is FALSE, then the constraint record is also freed.     */
/*************************************************************/
static void ReturnConstraintRecord(
  void *theEnv,
  EXEC_STATUS,
  CONSTRAINT_RECORD *constraints)
  {
   if (constraints == NULL) return;

   if (constraints->bucket < 0)
     {
      ReturnExpression(theEnv,execStatus,constraints->classList);
      ReturnExpression(theEnv,execStatus,constraints->restrictionList);
      ReturnExpression(theEnv,execStatus,constraints->maxValue);
      ReturnExpression(theEnv,execStatus,constraints->minValue);
      ReturnExpression(theEnv,execStatus,constraints->minFields);
      ReturnExpression(theEnv,execStatus,constraints->maxFields);
     }

   ReturnConstraintRecord(theEnv,execStatus,constraints->multifield);

   rtn_struct(theEnv,execStatus,constraintRecord,constraints);
  }

/***************************************************/
/* DeinstallConstraintRecord: Decrements the count */
/*   values of all occurrences of primitive data   */
/*   types found in a constraint record.           */
/***************************************************/
static void DeinstallConstraintRecord(
  void *theEnv,
  EXEC_STATUS,
  CONSTRAINT_RECORD *constraints)
  {
   if (constraints->bucket >= 0)
     {
      RemoveHashedExpression(theEnv,execStatus,constraints->classList);
      RemoveHashedExpression(theEnv,execStatus,constraints->restrictionList);
      RemoveHashedExpression(theEnv,execStatus,constraints->maxValue);
      RemoveHashedExpression(theEnv,execStatus,constraints->minValue);
      RemoveHashedExpression(theEnv,execStatus,constraints->minFields);
      RemoveHashedExpression(theEnv,execStatus,constraints->maxFields);
     }
   else
     {
      ExpressionDeinstall(theEnv,execStatus,constraints->classList);
      ExpressionDeinstall(theEnv,execStatus,constraints->restrictionList);
      ExpressionDeinstall(theEnv,execStatus,constraints->maxValue);
      ExpressionDeinstall(theEnv,execStatus,constraints->minValue);
      ExpressionDeinstall(theEnv,execStatus,constraints->minFields);
      ExpressionDeinstall(theEnv,execStatus,constraints->maxFields);
     }

   if (constraints->multifield != NULL)
     { DeinstallConstraintRecord(theEnv,execStatus,constraints->multifield); }
  }

/******************************************/
/* RemoveConstraint: Removes a constraint */
/*   from the constraint hash table.      */
/******************************************/
globle void RemoveConstraint(
  void *theEnv,
  EXEC_STATUS,
  struct constraintRecord *theConstraint)
  {
   struct constraintRecord *tmpPtr, *prevPtr = NULL;

   if (theConstraint == NULL) return;

   /*========================================*/
   /* If the bucket value is less than zero, */
   /* then the constraint wasn't stored in   */
   /* the hash table.                        */
   /*========================================*/

   if (theConstraint->bucket < 0)
     {
      ReturnConstraintRecord(theEnv,execStatus,theConstraint);
      return;
     }

   /*================================*/
   /* Find and remove the constraint */
   /* from the contraint hash table. */
   /*================================*/

   tmpPtr = ConstraintData(theEnv,execStatus)->ConstraintHashtable[theConstraint->bucket];
   while (tmpPtr != NULL)
     {
      if (tmpPtr == theConstraint)
        {
         theConstraint->count--;
         if (theConstraint->count == 0)
           {
            if (prevPtr == NULL)
              { ConstraintData(theEnv,execStatus)->ConstraintHashtable[theConstraint->bucket] = theConstraint->next; }
            else
              { prevPtr->next = theConstraint->next; }
            DeinstallConstraintRecord(theEnv,execStatus,theConstraint);
            ReturnConstraintRecord(theEnv,execStatus,theConstraint);
           }
         return;
        }

      prevPtr = tmpPtr;
      tmpPtr = tmpPtr->next;
     }

   return;
  }

#endif /* (! RUN_TIME) */

#if (! RUN_TIME) && (! BLOAD_ONLY)

/***********************************/
/* HashConstraint: Returns a hash  */
/*   value for a given constraint. */
/***********************************/
globle unsigned long HashConstraint(
  struct constraintRecord *theConstraint)
  {
   int i = 0;
   unsigned long count = 0;
   unsigned long hashValue;
   struct expr *tmpPtr;

   count += (unsigned long)
      (theConstraint->anyAllowed * 17) +
      (theConstraint->symbolsAllowed * 5) +
      (theConstraint->stringsAllowed * 23) +
      (theConstraint->floatsAllowed * 19) +
      (theConstraint->integersAllowed * 29) +
      (theConstraint->instanceNamesAllowed * 31) +
      (theConstraint->instanceAddressesAllowed * 17);

   count += (unsigned long)
      (theConstraint->externalAddressesAllowed * 29) +
      (theConstraint->voidAllowed * 29) +
      (theConstraint->multifieldsAllowed * 29) +
      (theConstraint->factAddressesAllowed * 79) +
      (theConstraint->anyRestriction * 59) +
      (theConstraint->symbolRestriction * 61);
      
   count += (unsigned long)
      (theConstraint->stringRestriction * 3) +
      (theConstraint->floatRestriction * 37) +
      (theConstraint->integerRestriction * 9) +
      (theConstraint->classRestriction * 11) +
      (theConstraint->instanceNameRestriction * 7);

   for (tmpPtr = theConstraint->classList; tmpPtr != NULL; tmpPtr = tmpPtr->nextArg)
     { count += GetAtomicHashValue(tmpPtr->type,tmpPtr->value,i++); }

   for (tmpPtr = theConstraint->restrictionList; tmpPtr != NULL; tmpPtr = tmpPtr->nextArg)
     { count += GetAtomicHashValue(tmpPtr->type,tmpPtr->value,i++); }

   for (tmpPtr = theConstraint->minValue; tmpPtr != NULL; tmpPtr = tmpPtr->nextArg)
     { count += GetAtomicHashValue(tmpPtr->type,tmpPtr->value,i++); }

   for (tmpPtr = theConstraint->maxValue; tmpPtr != NULL; tmpPtr = tmpPtr->nextArg)
     { count += GetAtomicHashValue(tmpPtr->type,tmpPtr->value,i++); }

   for (tmpPtr = theConstraint->minFields; tmpPtr != NULL; tmpPtr = tmpPtr->nextArg)
     { count += GetAtomicHashValue(tmpPtr->type,tmpPtr->value,i++); }

   for (tmpPtr = theConstraint->maxFields; tmpPtr != NULL; tmpPtr = tmpPtr->nextArg)
     { count += GetAtomicHashValue(tmpPtr->type,tmpPtr->value,i++); }

   if (theConstraint->multifield != NULL)
     { count += HashConstraint(theConstraint->multifield); }

   hashValue = (unsigned long) (count % SIZE_CONSTRAINT_HASH);

   return(hashValue);
  }

/**********************************************/
/* ConstraintCompare: Compares two constraint */
/*   records and returns TRUE if they are     */
/*   identical, otherwise FALSE.              */
/**********************************************/
static int ConstraintCompare(
  struct constraintRecord *constraint1,
  struct constraintRecord *constraint2)
  {
   struct expr *tmpPtr1, *tmpPtr2;

   if ((constraint1->anyAllowed != constraint2->anyAllowed) ||
       (constraint1->symbolsAllowed != constraint2->symbolsAllowed) ||
       (constraint1->stringsAllowed != constraint2->stringsAllowed) ||
       (constraint1->floatsAllowed != constraint2->floatsAllowed) ||
       (constraint1->integersAllowed != constraint2->integersAllowed) ||
       (constraint1->instanceNamesAllowed != constraint2->instanceNamesAllowed) ||
       (constraint1->instanceAddressesAllowed != constraint2->instanceAddressesAllowed) ||
       (constraint1->externalAddressesAllowed != constraint2->externalAddressesAllowed) ||
       (constraint1->voidAllowed != constraint2->voidAllowed) ||
       (constraint1->multifieldsAllowed != constraint2->multifieldsAllowed) ||
       (constraint1->singlefieldsAllowed != constraint2->singlefieldsAllowed) ||
       (constraint1->factAddressesAllowed != constraint2->factAddressesAllowed) ||
       (constraint1->anyRestriction != constraint2->anyRestriction) ||
       (constraint1->symbolRestriction != constraint2->symbolRestriction) ||
       (constraint1->stringRestriction != constraint2->stringRestriction) ||
       (constraint1->floatRestriction != constraint2->floatRestriction) ||
       (constraint1->integerRestriction != constraint2->integerRestriction) ||
       (constraint1->classRestriction != constraint2->classRestriction) ||
       (constraint1->instanceNameRestriction != constraint2->instanceNameRestriction))
     { return(FALSE); }

   for (tmpPtr1 = constraint1->classList, tmpPtr2 = constraint2->classList;
        (tmpPtr1 != NULL) && (tmpPtr2 != NULL);
        tmpPtr1 = tmpPtr1->nextArg, tmpPtr2 = tmpPtr2->nextArg)
     {
      if ((tmpPtr1->type != tmpPtr2->type) || (tmpPtr1->value != tmpPtr2->value))
        { return(FALSE); }
     }
   if (tmpPtr1 != tmpPtr2) return(FALSE);

   for (tmpPtr1 = constraint1->restrictionList, tmpPtr2 = constraint2->restrictionList;
        (tmpPtr1 != NULL) && (tmpPtr2 != NULL);
        tmpPtr1 = tmpPtr1->nextArg, tmpPtr2 = tmpPtr2->nextArg)
     {
      if ((tmpPtr1->type != tmpPtr2->type) || (tmpPtr1->value != tmpPtr2->value))
        { return(FALSE); }
     }
   if (tmpPtr1 != tmpPtr2) return(FALSE);

   for (tmpPtr1 = constraint1->minValue, tmpPtr2 = constraint2->minValue;
        (tmpPtr1 != NULL) && (tmpPtr2 != NULL);
        tmpPtr1 = tmpPtr1->nextArg, tmpPtr2 = tmpPtr2->nextArg)
     {
      if ((tmpPtr1->type != tmpPtr2->type) || (tmpPtr1->value != tmpPtr2->value))
        { return(FALSE); }
     }
   if (tmpPtr1 != tmpPtr2) return(FALSE);

   for (tmpPtr1 = constraint1->maxValue, tmpPtr2 = constraint2->maxValue;
        (tmpPtr1 != NULL) && (tmpPtr2 != NULL);
        tmpPtr1 = tmpPtr1->nextArg, tmpPtr2 = tmpPtr2->nextArg)
     {
      if ((tmpPtr1->type != tmpPtr2->type) || (tmpPtr1->value != tmpPtr2->value))
        { return(FALSE); }
     }
   if (tmpPtr1 != tmpPtr2) return(FALSE);

   for (tmpPtr1 = constraint1->minFields, tmpPtr2 = constraint2->minFields;
        (tmpPtr1 != NULL) && (tmpPtr2 != NULL);
        tmpPtr1 = tmpPtr1->nextArg, tmpPtr2 = tmpPtr2->nextArg)
     {
      if ((tmpPtr1->type != tmpPtr2->type) || (tmpPtr1->value != tmpPtr2->value))
        { return(FALSE); }
     }
   if (tmpPtr1 != tmpPtr2) return(FALSE);

   for (tmpPtr1 = constraint1->maxFields, tmpPtr2 = constraint2->maxFields;
        (tmpPtr1 != NULL) && (tmpPtr2 != NULL);
        tmpPtr1 = tmpPtr1->nextArg, tmpPtr2 = tmpPtr2->nextArg)
     {
      if ((tmpPtr1->type != tmpPtr2->type) || (tmpPtr1->value != tmpPtr2->value))
        { return(FALSE); }
     }
   if (tmpPtr1 != tmpPtr2) return(FALSE);

   if (((constraint1->multifield == NULL) && (constraint2->multifield != NULL)) ||
       ((constraint1->multifield != NULL) && (constraint2->multifield == NULL)))
     { return(FALSE); }
   else if (constraint1->multifield == constraint2->multifield)
     { return(TRUE); }

   return(ConstraintCompare(constraint1->multifield,constraint2->multifield));
  }

/************************************/
/* AddConstraint: Adds a constraint */
/*   to the constraint hash table.  */
/************************************/
globle struct constraintRecord *AddConstraint(
  void *theEnv,
  EXEC_STATUS,
  struct constraintRecord *theConstraint)
  {
   struct constraintRecord *tmpPtr;
   unsigned long hashValue;

   if (theConstraint == NULL) return(NULL);

   hashValue = HashConstraint(theConstraint);

   for (tmpPtr = ConstraintData(theEnv,execStatus)->ConstraintHashtable[hashValue];
        tmpPtr != NULL;
        tmpPtr = tmpPtr->next)
     {
      if (ConstraintCompare(theConstraint,tmpPtr))
        {
         tmpPtr->count++;
         ReturnConstraintRecord(theEnv,execStatus,theConstraint);
         return(tmpPtr);
        }
     }

   InstallConstraintRecord(theEnv,execStatus,theConstraint);
   theConstraint->count = 1;
   theConstraint->bucket = hashValue;
   theConstraint->next = ConstraintData(theEnv,execStatus)->ConstraintHashtable[hashValue];
   ConstraintData(theEnv,execStatus)->ConstraintHashtable[hashValue] = theConstraint;
   return(theConstraint);
  }

/*************************************************/
/* InstallConstraintRecord: Increments the count */
/*   values of all occurrences of primitive data */
/*   types found in a constraint record.         */
/*************************************************/
static void InstallConstraintRecord(
  void *theEnv,
  EXEC_STATUS,
  CONSTRAINT_RECORD *constraints)
  {
   struct expr *tempExpr;

   tempExpr = AddHashedExpression(theEnv,execStatus,constraints->classList);
   ReturnExpression(theEnv,execStatus,constraints->classList);
   constraints->classList = tempExpr;

   tempExpr = AddHashedExpression(theEnv,execStatus,constraints->restrictionList);
   ReturnExpression(theEnv,execStatus,constraints->restrictionList);
   constraints->restrictionList = tempExpr;

   tempExpr = AddHashedExpression(theEnv,execStatus,constraints->maxValue);
   ReturnExpression(theEnv,execStatus,constraints->maxValue);
   constraints->maxValue = tempExpr;

   tempExpr = AddHashedExpression(theEnv,execStatus,constraints->minValue);
   ReturnExpression(theEnv,execStatus,constraints->minValue);
   constraints->minValue = tempExpr;

   tempExpr = AddHashedExpression(theEnv,execStatus,constraints->minFields);
   ReturnExpression(theEnv,execStatus,constraints->minFields);
   constraints->minFields = tempExpr;

   tempExpr = AddHashedExpression(theEnv,execStatus,constraints->maxFields);
   ReturnExpression(theEnv,execStatus,constraints->maxFields);
   constraints->maxFields = tempExpr;

   if (constraints->multifield != NULL)
     { InstallConstraintRecord(theEnv,execStatus,constraints->multifield); }
  }

#endif /* (! RUN_TIME) && (! BLOAD_ONLY) */

/**********************************************/
/* SDCCommand: H/L access routine for the     */
/*   set-dynamic-constraint-checking command. */
/**********************************************/
globle int SDCCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   int oldValue;
   DATA_OBJECT arg_ptr;

   oldValue = EnvGetDynamicConstraintChecking(theEnv,execStatus);

   if (EnvArgCountCheck(theEnv,execStatus,"set-dynamic-constraint-checking",EXACTLY,1) == -1)
     { return(oldValue); }

   EnvRtnUnknown(theEnv,execStatus,1,&arg_ptr);

   if ((arg_ptr.value == EnvFalseSymbol(theEnv,execStatus)) && (arg_ptr.type == SYMBOL))
     { EnvSetDynamicConstraintChecking(theEnv,execStatus,FALSE); }
   else
     { EnvSetDynamicConstraintChecking(theEnv,execStatus,TRUE); }

   return(oldValue);
  }

/**********************************************/
/* GDCCommand: H/L access routine for the     */
/*   get-dynamic-constraint-checking command. */
/**********************************************/
globle int GDCCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   int oldValue;

   oldValue = EnvGetDynamicConstraintChecking(theEnv,execStatus);

   if (EnvArgCountCheck(theEnv,execStatus,"get-dynamic-constraint-checking",EXACTLY,0) == -1)
     { return(oldValue); }

   return(oldValue);
  }

/*********************************************/
/* SSCCommand: H/L access routine for the    */
/*   set-static-constraint-checking command. */
/*********************************************/
globle int SSCCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   int oldValue;
   DATA_OBJECT arg_ptr;

   oldValue = EnvGetStaticConstraintChecking(theEnv,execStatus);

   if (EnvArgCountCheck(theEnv,execStatus,"set-static-constraint-checking",EXACTLY,1) == -1)
     { return(oldValue); }

   EnvRtnUnknown(theEnv,execStatus,1,&arg_ptr);

   if ((arg_ptr.value == EnvFalseSymbol(theEnv,execStatus)) && (arg_ptr.type == SYMBOL))
     { EnvSetStaticConstraintChecking(theEnv,execStatus,FALSE); }
   else
     { EnvSetStaticConstraintChecking(theEnv,execStatus,TRUE); }

   return(oldValue);
  }

/*********************************************/
/* GSCCommand: H/L access routine for the    */
/*   get-static-constraint-checking command. */
/*********************************************/
globle int GSCCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   int oldValue;

   oldValue = EnvGetStaticConstraintChecking(theEnv,execStatus);

   if (EnvArgCountCheck(theEnv,execStatus,"get-static-constraint-checking",EXACTLY,0) == -1)
     { return(oldValue); }

   return(oldValue);
  }

/******************************************************/
/* EnvSetDynamicConstraintChecking: C access routine  */
/*   for the set-dynamic-constraint-checking command. */
/******************************************************/
globle intBool EnvSetDynamicConstraintChecking(
  void *theEnv,
  EXEC_STATUS,
  int value)
  {
   int ov;
   ov = ConstraintData(theEnv,execStatus)->DynamicConstraintChecking;
   ConstraintData(theEnv,execStatus)->DynamicConstraintChecking = value;
   return(ov);
  }

/******************************************************/
/* EnvGetDynamicConstraintChecking: C access routine  */
/*   for the get-dynamic-constraint-checking command. */
/******************************************************/
globle intBool EnvGetDynamicConstraintChecking(
  void *theEnv,
  EXEC_STATUS)
  { 
   return(ConstraintData(theEnv,execStatus)->DynamicConstraintChecking); 
  }

/*****************************************************/
/* EnvSetStaticConstraintChecking: C access routine  */
/*   for the set-static-constraint-checking command. */
/*****************************************************/
globle intBool EnvSetStaticConstraintChecking(
  void *theEnv,
  EXEC_STATUS,
  int value)
  {
   int ov;

   ov = ConstraintData(theEnv,execStatus)->StaticConstraintChecking;
   ConstraintData(theEnv,execStatus)->StaticConstraintChecking = value;
   return(ov);
  }

/*****************************************************/
/* EnvGetStaticConstraintChecking: C access routine  */
/*   for the get-static-constraint-checking command. */
/*****************************************************/
globle intBool EnvGetStaticConstraintChecking(
  void *theEnv,
  EXEC_STATUS)
  {    
   return(ConstraintData(theEnv,execStatus)->StaticConstraintChecking); 
  }


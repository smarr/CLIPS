   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
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
/*      Brian Donnell                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _CONSTRNT_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <stdlib.h>

#include "setup.h"

#include "constant.h"
#include "memalloc.h"
#include "router.h"
#include "extnfunc.h"
#include "scanner.h"
#include "multifld.h"
#include "constrnt.h"
#include "argacces.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if (! RUN_TIME) && (! BLOAD_ONLY)
   static void                     InstallConstraintRecord(CONSTRAINT_RECORD *);
   static int                      ConstraintCompare(struct constraintRecord *,struct constraintRecord *);
#endif
#if (! RUN_TIME)
   static void                     ReturnConstraintRecord(CONSTRAINT_RECORD *);
   static void                     DeinstallConstraintRecord(CONSTRAINT_RECORD *);
#endif

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle struct constraintRecord   **ConstraintHashtable;

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static BOOLEAN                     StaticConstraintChecking = TRUE;
   Thread static BOOLEAN                     DynamicConstraintChecking = FALSE;

/*****************************************************/
/* InitializeConstraints: Initializes the constraint */
/*   hash table to NULL and defines the static and   */
/*   dynamic constraint access functions.            */
/*****************************************************/
globle void InitializeConstraints()
   {
#if (! RUN_TIME) && (! BLOAD_ONLY)
    int i;

    ConstraintHashtable = (struct constraintRecord **)
                          gm2((int) sizeof (struct constraintRecord *) *
                                    SIZE_CONSTRAINT_HASH);

    if (ConstraintHashtable == NULL) ExitRouter(EXIT_FAILURE);

    for (i = 0; i < SIZE_CONSTRAINT_HASH; i++) ConstraintHashtable[i] = NULL;
#endif

#if (! RUN_TIME)
   DefineFunction2("get-dynamic-constraint-checking",'b',GDCCommand,"GDCCommand", "00");
   DefineFunction2("set-dynamic-constraint-checking",'b',SDCCommand,"SDCCommand", "11");

   DefineFunction2("get-static-constraint-checking",'b',GSCCommand,"GSCCommand", "00");
   DefineFunction2("set-static-constraint-checking",'b',SSCCommand,"SSCCommand", "11");
#endif
  }

#if (! RUN_TIME)

/*************************************************************/
/* ReturnConstraintRecord: Frees the data structures used by */
/*   a constraint record. If the returnOnlyFields argument   */
/*   is FALSE, then the constraint record is also freed.     */
/*************************************************************/
static void ReturnConstraintRecord(
  CONSTRAINT_RECORD *constraints)
  {
   if (constraints == NULL) return;

   if (constraints->bucket < 0)
     {
      ReturnExpression(constraints->restrictionList);
      ReturnExpression(constraints->maxValue);
      ReturnExpression(constraints->minValue);
      ReturnExpression(constraints->minFields);
      ReturnExpression(constraints->maxFields);
     }

   ReturnConstraintRecord(constraints->multifield);

   rtn_struct(constraintRecord,constraints);
  }

/***************************************************/
/* DeinstallConstraintRecord: Decrements the count */
/*   values of all occurrences of primitive data   */
/*   types found in a constraint record.           */
/***************************************************/
static void DeinstallConstraintRecord(
  CONSTRAINT_RECORD *constraints)
  {
   if (constraints->bucket >= 0)
     {
      RemoveHashedExpression(constraints->restrictionList);
      RemoveHashedExpression(constraints->maxValue);
      RemoveHashedExpression(constraints->minValue);
      RemoveHashedExpression(constraints->minFields);
      RemoveHashedExpression(constraints->maxFields);
     }
   else
     {
      ExpressionDeinstall(constraints->restrictionList);
      ExpressionDeinstall(constraints->maxValue);
      ExpressionDeinstall(constraints->minValue);
      ExpressionDeinstall(constraints->minFields);
      ExpressionDeinstall(constraints->maxFields);
     }

   if (constraints->multifield != NULL)
     { DeinstallConstraintRecord(constraints->multifield); }
  }

/******************************************/
/* RemoveConstraint: Removes a constraint */
/*   from the constraint hash table.      */
/******************************************/
globle void RemoveConstraint(
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
      ReturnConstraintRecord(theConstraint);
      return;
     }

   /*================================*/
   /* Find and remove the constraint */
   /* from the contraint hash table. */
   /*================================*/

   tmpPtr = ConstraintHashtable[theConstraint->bucket];
   while (tmpPtr != NULL)
     {
      if (tmpPtr == theConstraint)
        {
         theConstraint->count--;
         if (theConstraint->count == 0)
           {
            if (prevPtr == NULL)
              { ConstraintHashtable[theConstraint->bucket] = theConstraint->next; }
            else
              { prevPtr->next = theConstraint->next; }
            DeinstallConstraintRecord(theConstraint);
            ReturnConstraintRecord(theConstraint);
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
globle int HashConstraint(
  struct constraintRecord *theConstraint)
  {
   int i = 0;
   unsigned int count = 0;
   int hashValue;
   struct expr *tmpPtr;

   count +=
      (theConstraint->anyAllowed * 17) +
      (theConstraint->symbolsAllowed * 5) +
      (theConstraint->stringsAllowed * 23) +
      (theConstraint->floatsAllowed * 19) +
      (theConstraint->integersAllowed * 29) +
      (theConstraint->instanceNamesAllowed * 31) +
      (theConstraint->instanceAddressesAllowed * 17);

   count +=
      (theConstraint->externalAddressesAllowed * 29) +
      (theConstraint->voidAllowed * 29) +
      (theConstraint->multifieldsAllowed * 29) +
      (theConstraint->factAddressesAllowed * 79) +
      (theConstraint->anyRestriction * 59) +
      (theConstraint->symbolRestriction * 61);
   count +=
      (theConstraint->stringRestriction * 3) +
      (theConstraint->floatRestriction * 37) +
      (theConstraint->integerRestriction * 9) +
      (theConstraint->instanceNameRestriction * 7);

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

   hashValue = (int) (count % SIZE_CONSTRAINT_HASH);
   if (hashValue < 0) hashValue = - hashValue;
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
       (constraint1->instanceNameRestriction != constraint2->instanceNameRestriction))
     { return(FALSE); }

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
  struct constraintRecord *theConstraint)
  {
   struct constraintRecord *tmpPtr;
   int hashValue;

   if (theConstraint == NULL) return(NULL);

   hashValue = HashConstraint(theConstraint);

   for (tmpPtr = ConstraintHashtable[hashValue];
        tmpPtr != NULL;
        tmpPtr = tmpPtr->next)
     {
      if (ConstraintCompare(theConstraint,tmpPtr))
        {
         tmpPtr->count++;
         ReturnConstraintRecord(theConstraint);
         return(tmpPtr);
        }
     }

   InstallConstraintRecord(theConstraint);
   theConstraint->count = 1;
   theConstraint->bucket = hashValue;
   theConstraint->next = ConstraintHashtable[hashValue];
   ConstraintHashtable[hashValue] = theConstraint;
   return(theConstraint);
  }

/*************************************************/
/* InstallConstraintRecord: Increments the count */
/*   values of all occurrences of primitive data */
/*   types found in a constraint record.         */
/*************************************************/
static void InstallConstraintRecord(
  CONSTRAINT_RECORD *constraints)
  {
   struct expr *tempExpr;

   tempExpr = AddHashedExpression(constraints->restrictionList);
   ReturnExpression(constraints->restrictionList);
   constraints->restrictionList = tempExpr;

   tempExpr = AddHashedExpression(constraints->maxValue);
   ReturnExpression(constraints->maxValue);
   constraints->maxValue = tempExpr;

   tempExpr = AddHashedExpression(constraints->minValue);
   ReturnExpression(constraints->minValue);
   constraints->minValue = tempExpr;

   tempExpr = AddHashedExpression(constraints->minFields);
   ReturnExpression(constraints->minFields);
   constraints->minFields = tempExpr;

   tempExpr = AddHashedExpression(constraints->maxFields);
   ReturnExpression(constraints->maxFields);
   constraints->maxFields = tempExpr;

   if (constraints->multifield != NULL)
     { InstallConstraintRecord(constraints->multifield); }
  }

#endif /* (! RUN_TIME) && (! BLOAD_ONLY) */

/**********************************************/
/* SDCCommand: H/L access routine for the     */
/*   set-dynamic-constraint-checking command. */
/**********************************************/
globle int SDCCommand()
  {
   int oldValue;
   DATA_OBJECT arg_ptr;

   oldValue = GetDynamicConstraintChecking();

   if (ArgCountCheck("set-dynamic-constraint-checking",EXACTLY,1) == -1)
     { return(oldValue); }

   RtnUnknown(1,&arg_ptr);

   if ((arg_ptr.value == FalseSymbol) && (arg_ptr.type == SYMBOL))
     { SetDynamicConstraintChecking(FALSE); }
   else
     { SetDynamicConstraintChecking(TRUE); }

   return(oldValue);
  }

/**********************************************/
/* GDCCommand: H/L access routine for the     */
/*   get-dynamic-constraint-checking command. */
/**********************************************/
globle int GDCCommand()
  {
   int oldValue;

   oldValue = GetDynamicConstraintChecking();

   if (ArgCountCheck("get-dynamic-constraint-checking",EXACTLY,0) == -1)
     { return(oldValue); }

   return(oldValue);
  }

/*********************************************/
/* SSCCommand: H/L access routine for the    */
/*   set-static-constraint-checking command. */
/*********************************************/
globle int SSCCommand()
  {
   int oldValue;
   DATA_OBJECT arg_ptr;

   oldValue = GetStaticConstraintChecking();

   if (ArgCountCheck("set-static-constraint-checking",EXACTLY,1) == -1)
     { return(oldValue); }

   RtnUnknown(1,&arg_ptr);

   if ((arg_ptr.value == FalseSymbol) && (arg_ptr.type == SYMBOL))
     { SetStaticConstraintChecking(FALSE); }
   else
     { SetStaticConstraintChecking(TRUE); }

   return(oldValue);
  }

/*********************************************/
/* GSCCommand: H/L access routine for the    */
/*   get-static-constraint-checking command. */
/*********************************************/
globle int GSCCommand()
  {
   int oldValue;

   oldValue = GetStaticConstraintChecking();

   if (ArgCountCheck("get-static-constraint-checking",EXACTLY,0) == -1)
     { return(oldValue); }

   return(oldValue);
  }

/******************************************************/
/* SetDynamicConstraintChecking: C access routine for */
/*   the set-dynamic-constraint-checking command.     */
/******************************************************/
globle BOOLEAN SetDynamicConstraintChecking(
  int value)
  {
   int ov;

   ov = DynamicConstraintChecking;
   DynamicConstraintChecking = value;
   return(ov);
  }

/******************************************************/
/* GetDynamicConstraintChecking: C access routine for */
/*   the get-dynamic-constraint-checking command.     */
/******************************************************/
globle BOOLEAN GetDynamicConstraintChecking()
  { return(DynamicConstraintChecking); }

/*****************************************************/
/* SetStaticConstraintChecking: C access routine for */
/*   the set-static-constraint-checking command.     */
/*****************************************************/
globle BOOLEAN SetStaticConstraintChecking(
  int value)
  {
   int ov;

   ov = StaticConstraintChecking;
   StaticConstraintChecking = value;
   return(ov);
  }

/*****************************************************/
/* GetStaticConstraintChecking: C access routine for */
/*   the get-static-constraint-checking command.     */
/*****************************************************/
globle BOOLEAN GetStaticConstraintChecking()
  { return(StaticConstraintChecking); }


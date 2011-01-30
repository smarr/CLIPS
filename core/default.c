   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*               DEFAULT ATTRIBUTE MODULE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides functions for parsing the default       */
/*   attribute and determining default values based on       */
/*   slot constraints.                                       */
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

#define _DEFAULT_SOURCE_

#include "setup.h"

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <stdlib.h>
#include <string.h>

#include "constant.h"
#include "constrnt.h"
#include "cstrnchk.h"
#include "multifld.h"
#include "inscom.h"
#include "exprnpsr.h"
#include "scanner.h"
#include "router.h"
#include "factmngr.h"
#include "cstrnutl.h"

#include "default.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    *FindDefaultValue(int,CONSTRAINT_RECORD *,void *);

/********************************************************/
/* DeriveDefaultFromConstraints: Returns an appropriate */
/*   default value for the supplied constraints.        */
/********************************************************/
globle void DeriveDefaultFromConstraints(
  CONSTRAINT_RECORD *constraints,
  DATA_OBJECT *theDefault,
  int multifield)
  {
   short theType;
   long minFields; /* 6.04 Bug Fix */
   void *theValue;

   /*=============================================================*/
   /* If no constraints are specified, then use the symbol nil as */
   /* a default for single field slots and a multifield of length */
   /* 0 as a default for multifield slots.                        */
   /*=============================================================*/

   if (constraints == NULL)
     {
      if (multifield)
        {
         SetpType(theDefault,MULTIFIELD);
         SetpDOBegin(theDefault,1);
         SetpDOEnd(theDefault,0);
         SetpValue(theDefault,(void *) CreateMultifield(0L));
        }
      else
        {
         theDefault->type = SYMBOL;
         theDefault->value = AddSymbol("nil");
        }

      return;
     }

   /*=========================================*/
   /* Determine the default's type and value. */
   /*=========================================*/

   if (constraints->anyAllowed || constraints->symbolsAllowed)
     {
      theType = SYMBOL;
      theValue = FindDefaultValue(SYMBOL,constraints,AddSymbol("nil"));
     }

   else if (constraints->stringsAllowed)
     {
      theType = STRING;
      theValue = FindDefaultValue(STRING,constraints,AddSymbol(""));
     }

   else if (constraints->integersAllowed)
     {
      theType = INTEGER;
      theValue = FindDefaultValue(INTEGER,constraints,AddLong(0L));
     }

   else if (constraints->floatsAllowed)
     {
      theType = FLOAT;
      theValue = FindDefaultValue(FLOAT,constraints,AddDouble(0.0));
     }
#if OBJECT_SYSTEM
   else if (constraints->instanceNamesAllowed)
     {
      theType = INSTANCE_NAME;
      theValue = FindDefaultValue(INSTANCE_NAME,constraints,AddSymbol("nil"));
     }

   else if (constraints->instanceAddressesAllowed)
     {
      theType = INSTANCE_ADDRESS;
      theValue = (void *) &DummyInstance;
     }
#endif
#if DEFTEMPLATE_CONSTRUCT
   else if (constraints->factAddressesAllowed)
     {
      theType = FACT_ADDRESS;
      theValue = (void *) &DummyFact;
     }
#endif
   else if (constraints->externalAddressesAllowed)
     {
      theType = EXTERNAL_ADDRESS;
      theValue = NULL;
     }

   else
     {
      theType = SYMBOL;
      theValue = AddSymbol("nil");
     }

   /*=========================================================*/
   /* If the default is for a multifield slot, then create a  */
   /* multifield default value that satisfies the cardinality */
   /* constraints for the slot. The default value for a       */
   /* multifield slot is a multifield of length 0.            */
   /*=========================================================*/

   if (multifield)
     {
      if (constraints->minFields == NULL) minFields = 0;
      else if (constraints->minFields->value == NegativeInfinity) minFields = 0;
      else minFields = ValueToLong(constraints->minFields->value);

      SetpType(theDefault,MULTIFIELD);
      SetpDOBegin(theDefault,1);
      SetpDOEnd(theDefault,minFields);
      SetpValue(theDefault,(void *) CreateMultifield(minFields));

      for (; minFields > 0; minFields--)
        {
         SetMFType(GetpValue(theDefault),minFields,theType);
         SetMFValue(GetpValue(theDefault),minFields,theValue);
        }
     }
   else
     {
      theDefault->type = theType;
      theDefault->value = theValue;
     }
  }

/***********************************************************************/
/* FindDefaultValue: Searches the list of restriction values for a     */
/*   constraint to find a default value of the specified type. For     */
/*   example, if the attribute (allowed-symbols on off) was specified, */
/*   then the symbol "on" would be used as a default value rather than */
/*   the symbol "nil". For integers and floats, the range attribute is */
/*   also used to select a suitable default value. If a minimum value  */
/*   was specified, then this value is used first followed by the      */
/*   maximum value.                                                    */
/************************************************************************/
static void *FindDefaultValue(
  int theType,
  CONSTRAINT_RECORD *theConstraints,
  void *standardDefault)
  {
   struct expr *theList;

   /*=====================================================*/
   /* Look on the the allowed values list to see if there */
   /* is a value of the requested type. Return the first  */
   /* value found of the requested type.                  */
   /*=====================================================*/

   theList = theConstraints->restrictionList;
   while (theList != NULL)
     {
      if (theList->type == theType) return(theList->value);
      theList = theList->nextArg;
     }

   /*=============================================================*/
   /* If no specific values were available for the default value, */
   /* and the type requested is a float or integer, then use the  */
   /* range attribute to select a default value.                  */
   /*=============================================================*/

   if (theType == INTEGER)
     {
      if (theConstraints->minValue->type == INTEGER)
        { return(theConstraints->minValue->value); }
      else if (theConstraints->minValue->type == FLOAT)
        { return(AddLong((long) ValueToDouble(theConstraints->minValue->value))); }
      else if (theConstraints->maxValue->type == INTEGER)
        { return(theConstraints->maxValue->value); }
      else if (theConstraints->maxValue->type == FLOAT)
        { return(AddLong((long) ValueToDouble(theConstraints->maxValue->value))); }
     }
   else if (theType == FLOAT)
     {
      if (theConstraints->minValue->type == FLOAT)
        { return(theConstraints->minValue->value); }
      else if (theConstraints->minValue->type == INTEGER)
        { return(AddDouble((double) ValueToLong(theConstraints->minValue->value))); }
      else if (theConstraints->maxValue->type == FLOAT)
        { return(theConstraints->maxValue->value); }
      else if (theConstraints->maxValue->type == INTEGER)
        { return(AddDouble((double) ValueToLong(theConstraints->maxValue->value))); }
     }

   /*======================================*/
   /* Use the standard default value (such */
   /* as nil if symbols are allowed).      */
   /*======================================*/

   return(standardDefault);
  }

#if (! RUN_TIME) && (! BLOAD_ONLY)

/**********************************************/
/* ParseDefault: Parses a default value list. */
/**********************************************/
globle struct expr *ParseDefault(
  char *readSource,
  int multifield,
  int dynamic,
  int evalStatic,
  int *noneSpecified,
  int *deriveSpecified,
  int *error)
  {
   struct expr *defaultList = NULL, *lastDefault = NULL;
   struct expr *newItem, *tmpItem;
   struct token theToken;
   DATA_OBJECT theValue;
   CONSTRAINT_RECORD *rv;
   int specialVarCode;

   *noneSpecified = FALSE;
   *deriveSpecified = FALSE;

   SavePPBuffer(" ");
   GetToken(readSource,&theToken);

   /*===================================================*/
   /* Read the items contained in the default attribute */
   /* until a closing right parenthesis is encountered. */
   /*===================================================*/

   while (theToken.type != RPAREN)
     {
      /*========================================*/
      /* Get the next item in the default list. */
      /*========================================*/

      newItem = ParseAtomOrExpression(readSource,&theToken);
      if (newItem == NULL)
        {
         ReturnExpression(defaultList);
         *error = TRUE;
         return(NULL);
        }

      /*===========================================================*/
      /* Check for invalid variable usage. With the expection of   */
      /* ?NONE for the default attribute, local variables may not  */
      /* be used within the default or default-dynamic attributes. */
      /*===========================================================*/

      if ((newItem->type == SF_VARIABLE) || (newItem->type == MF_VARIABLE))
        {
         if (strcmp(ValueToString(newItem->value),"NONE") == 0)
           { specialVarCode = 0; }
         else if (strcmp(ValueToString(newItem->value),"DERIVE") == 0)
           { specialVarCode = 1; }
         else
           { specialVarCode = -1; }

         if ((dynamic) ||
             (newItem->type == MF_VARIABLE) ||
             (specialVarCode == -1) ||
             ((specialVarCode != -1) && (defaultList != NULL)))
           {
            if (dynamic) SyntaxErrorMessage("default-dynamic attribute");
            else SyntaxErrorMessage("default attribute");
            ReturnExpression(newItem);
            ReturnExpression(defaultList);
            *error = TRUE;
            return(NULL);
           }

         ReturnExpression(newItem);

         /*============================================*/
         /* Check for the closing right parenthesis of */
         /* the default or default dynamic attribute.  */
         /*============================================*/

         GetToken(readSource,&theToken);

         if (theToken.type != RPAREN)
           {
            if (dynamic) SyntaxErrorMessage("default-dynamic attribute");
            else SyntaxErrorMessage("default attribute");
            PPBackup();
            SavePPBuffer(" ");
            SavePPBuffer(theToken.printForm);
            *error = TRUE;
           }

         if (specialVarCode == 0)
           *noneSpecified = TRUE;
         else
           *deriveSpecified = TRUE;
         return(NULL);
        }

      /*====================================================*/
      /* Look to see if any variables have been used within */
      /* expressions contained within the default list.     */
      /*====================================================*/

      if (ExpressionContainsVariables(newItem,FALSE) == TRUE)
        {
         ReturnExpression(defaultList);
         ReturnExpression(newItem);
         *error = TRUE;
         if (dynamic) SyntaxErrorMessage("default-dynamic attribute");
         else SyntaxErrorMessage("default attribute");
         return(NULL);
        }

      /*============================================*/
      /* Add the default value to the default list. */
      /*============================================*/

      if (lastDefault == NULL)
        { defaultList = newItem; }
      else
        { lastDefault->nextArg = newItem; }
      lastDefault = newItem;

      /*=======================================*/
      /* Begin parsing the next default value. */
      /*=======================================*/

      SavePPBuffer(" ");
      GetToken(readSource,&theToken);
     }

   /*=====================================*/
   /* Fix up pretty print representation. */
   /*=====================================*/

   PPBackup();
   PPBackup();
   SavePPBuffer(")");

   /*=========================================*/
   /* A single field slot's default attribute */
   /* must contain a single value.            */
   /*=========================================*/

   if (multifield == FALSE)
     {
      if (defaultList == NULL)
        { *error = TRUE; }
      else if (defaultList->nextArg != NULL)
        { *error = TRUE; }
      else
        {
         rv = ExpressionToConstraintRecord(defaultList);
         rv->multifieldsAllowed = FALSE;
         if (UnmatchableConstraint(rv)) *error = TRUE;
         RemoveConstraint(rv);
        }

      if (*error)
        {
         PrintErrorID("DEFAULT",1,TRUE);
         PrintRouter(WERROR,"The default value for a single field slot must be a single field value\n");
         ReturnExpression(defaultList);
         return(NULL);
        }
     }

   /*=======================================================*/
   /* If the dynamic-default attribute is not being parsed, */
   /* evaluate the expressions to make the default value.   */
   /*=======================================================*/

   if (dynamic || (! evalStatic) || (defaultList == NULL)) return(defaultList);

   tmpItem = defaultList;
   newItem = defaultList;

   defaultList = NULL;

   while (newItem != NULL)
     {
      SetEvaluationError(FALSE);
      if (EvaluateExpression(newItem,&theValue)) *error = TRUE;

      if ((theValue.type == MULTIFIELD) &&
          (multifield == FALSE) &&
          (*error == FALSE))
        {
         PrintErrorID("DEFAULT",1,TRUE);
         PrintRouter(WERROR,"The default value for a single field slot must be a single field value\n");
         *error = TRUE;
        }

      if (*error)
        {
         ReturnExpression(tmpItem);
         ReturnExpression(defaultList);
         *error = TRUE;
         return(NULL);
        }

      lastDefault = ConvertValueToExpression(&theValue);

      defaultList = AppendExpressions(defaultList,lastDefault);

      newItem = newItem->nextArg;
     }

   ReturnExpression(tmpItem);

   /*==========================*/
   /* Return the default list. */
   /*==========================*/

   return(defaultList);
  }

#endif /* (! RUN_TIME) && (! BLOAD_ONLY) */

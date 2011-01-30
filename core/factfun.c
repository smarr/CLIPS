   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*               FACT FUNCTIONS MODULE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/*                                                           */
/* (fact-existp <fact-address-or-index>)                     */
/*    Returns TRUE if the fact exists, otherwise FALSE is    */
/*    returned.                                              */
/*                                                           */
/* (fact-relation <fact-address-or-index>)                   */
/*    Returns the deftemplate name of the fact. Returns      */
/*    False if the specified fact doesn't exist.             */
/*                                                           */
/* (fact-slot-value <fact-address-or-index> <slot-name>)     */
/*    Returns the contents of a slot (use the slot name      */
/*    implied for the implied multifield slot of an ordered  */
/*    fact). Returns the value FALSE if the slot name is     */
/*    invalid or the fact doesn't exist.                     */
/*                                                           */
/* (fact-slot-names <fact-address-or-index>)                 */
/*    Returns the slot names associated with a fact in a     */
/*    multifield value. Returns FALSE if the fact doesn't    */
/*    exist.                                                 */
/*                                                           */
/* (get-fact-list [<module-name>])                           */
/*    Returns the list of facts visible to the specified     */
/*    module or to the current module if none is specified.  */
/*    If * is specified then all facts are returned.         */
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

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT

#define _FACTFUN_SOURCE_

#include "extnfunc.h"
#include "argacces.h"
#include "prntutil.h"
#include "tmpltutl.h"

#include "factfun.h"

/****************************************************/
/* FactFunctionDefinitions: Defines fact functions. */
/****************************************************/
globle void FactFunctionDefinitions()
  {
#if ! RUN_TIME
   DefineFunction2("fact-existp",  'b', PTIF FactExistpFunction,  "FactExistpFunction", "11z");
   DefineFunction2("fact-relation",'w', PTIF FactRelationFunction,"FactRelationFunction", "11z");
   DefineFunction2("fact-slot-value",'u', PTIF FactSlotValueFunction,"FactSlotValueFunction", "22*zw");
   DefineFunction2("fact-slot-names",'u', PTIF FactSlotNamesFunction,"FactSlotNamesFunction", "11z");
   DefineFunction2("get-fact-list",'m',PTIF GetFactListFunction,"GetFactListFunction","01w");
#endif
  }

/**********************************************/
/* FactRelationFunction: H/L access routine   */
/*   for the fact-relation function.          */
/**********************************************/
globle void *FactRelationFunction()
  {
   struct fact *theFact;

   if (ArgCountCheck("fact-relation",EXACTLY,1) == -1) return(FalseSymbol);

   theFact = GetFactAddressOrIndexArgument("fact-relation",1,FALSE);

   if (theFact == NULL) return(FalseSymbol);

   return(FactRelation(theFact));
  }

/**************************************/
/* FactRelation: C access routine for */
/*   the fact-relation function.      */
/**************************************/
globle void *FactRelation(
  void *vTheFact)
  {
   struct fact *theFact = (struct fact *) vTheFact;

   return((void *) theFact->whichDeftemplate->header.name);
  }

/********************************************/
/* FactExistpFunction: H/L access routine   */
/*   for the fact-existp function.          */
/********************************************/
globle long int FactExistpFunction()
  {
   struct fact *theFact;

   if (ArgCountCheck("fact-existp",EXACTLY,1) == -1) return(-1L);

   theFact = GetFactAddressOrIndexArgument("fact-existp",1,FALSE);

   return(FactExistp(theFact));
  }

/************************************/
/* FactExistp: C access routine for */
/*   the fact-existp function.      */
/************************************/
globle long int FactExistp(
  void *vTheFact)
  {
   struct fact *theFact = (struct fact *) vTheFact;

   if (theFact == NULL) return(FALSE);

   if (theFact->garbage) return(FALSE);

   return(TRUE);
  }

/***********************************************/
/* FactSlotValueFunction: H/L access routine   */
/*   for the fact-slot-value function.         */
/***********************************************/
globle void FactSlotValueFunction(
  DATA_OBJECT *returnValue)
  {
   struct fact *theFact;
   DATA_OBJECT theValue;

   /*=============================================*/
   /* Set up the default return value for errors. */
   /*=============================================*/

   returnValue->type = SYMBOL;
   returnValue->value = FalseSymbol;

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/

   if (ArgCountCheck("fact-slot-value",EXACTLY,2) == -1) return;

   /*================================*/
   /* Get the reference to the fact. */
   /*================================*/

   theFact = GetFactAddressOrIndexArgument("fact-slot-value",1,TRUE);
   if (theFact == NULL) return;

   /*===========================*/
   /* Get the name of the slot. */
   /*===========================*/

   if (ArgTypeCheck("fact-slot-value",2,SYMBOL,&theValue) == FALSE)
     { return; }

   /*=======================*/
   /* Get the slot's value. */
   /*=======================*/

   FactSlotValue(theFact,DOToString(theValue),returnValue);
  }

/***************************************/
/* FactSlotValue: C access routine for */
/*   the fact-slot-value function.     */
/***************************************/
globle void FactSlotValue(
  void *vTheFact,
  char *theSlotName,
  DATA_OBJECT *returnValue)
  {
   struct fact *theFact = (struct fact *) vTheFact;
   int position;

   /*==================================================*/
   /* Make sure the slot exists (the symbol implied is */
   /* used for the implied slot of an ordered fact).   */
   /*==================================================*/

   if (theFact->whichDeftemplate->implied)
     {
      if (strcmp(theSlotName,"implied") != 0)
        {
         SetEvaluationError(TRUE);
         InvalidDeftemplateSlotMessage(theSlotName,
                                       ValueToString(theFact->whichDeftemplate->header.name));
         return;
        }
     }

   else if (FindSlot(theFact->whichDeftemplate,(SYMBOL_HN *) AddSymbol(theSlotName),&position) == NULL)
     {
      SetEvaluationError(TRUE);
      InvalidDeftemplateSlotMessage(theSlotName,
                                    ValueToString(theFact->whichDeftemplate->header.name));
      return;
     }

   /*==========================*/
   /* Return the slot's value. */
   /*==========================*/

   if (theFact->whichDeftemplate->implied)
     { GetFactSlot(theFact,NULL,returnValue); }
   else
     { GetFactSlot(theFact,theSlotName,returnValue); }
  }

/***********************************************/
/* FactSlotNamesFunction: H/L access routine   */
/*   for the fact-slot-names function.         */
/***********************************************/
globle void FactSlotNamesFunction(
  DATA_OBJECT *returnValue)
  {
   struct fact *theFact;

   /*=============================================*/
   /* Set up the default return value for errors. */
   /*=============================================*/

   returnValue->type = SYMBOL;
   returnValue->value = FalseSymbol;

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/

   if (ArgCountCheck("fact-slot-names",EXACTLY,1) == -1) return;

   /*================================*/
   /* Get the reference to the fact. */
   /*================================*/

   theFact = GetFactAddressOrIndexArgument("fact-slot-names",1,TRUE);
   if (theFact == NULL) return;

   /*=====================*/
   /* Get the slot names. */
   /*=====================*/

   FactSlotNames(theFact,returnValue);
  }

/***************************************/
/* FactSlotNames: C access routine for */
/*   the fact-slot-names function.     */
/***************************************/
globle void FactSlotNames(
  void *vTheFact,
  DATA_OBJECT *returnValue)
  {
   struct fact *theFact = (struct fact *) vTheFact;
   struct multifield *theList;
   struct templateSlot *theSlot;
   int count;

   /*===============================================*/
   /* If we're dealing with an implied deftemplate, */
   /* then the only slot names is "implied."        */
   /*===============================================*/

   if (theFact->whichDeftemplate->implied)
     {
      SetpType(returnValue,MULTIFIELD);
      SetpDOBegin(returnValue,1);
      SetpDOEnd(returnValue,1);
      theList = (struct multifield *) CreateMultifield((int) 1);
      SetMFType(theList,1,SYMBOL);
      SetMFValue(theList,1,AddSymbol("implied"));
      SetpValue(returnValue,(void *) theList);
      return;
     }

   /*=================================*/
   /* Count the number of slot names. */
   /*=================================*/

   for (count = 0, theSlot = theFact->whichDeftemplate->slotList;
        theSlot != NULL;
        count++, theSlot = theSlot->next)
     { /* Do Nothing */ }

   /*=============================================================*/
   /* Create a multifield value in which to store the slot names. */
   /*=============================================================*/

   SetpType(returnValue,MULTIFIELD);
   SetpDOBegin(returnValue,1);
   SetpDOEnd(returnValue,count);
   theList = (struct multifield *) CreateMultifield(count);
   SetpValue(returnValue,(void *) theList);

   /*===============================================*/
   /* Store the slot names in the multifield value. */
   /*===============================================*/

   for (count = 1, theSlot = theFact->whichDeftemplate->slotList;
        theSlot != NULL;
        count++, theSlot = theSlot->next)
     {
      SetMFType(theList,count,SYMBOL);
      SetMFValue(theList,count,theSlot->slotName);
     }
  }

/*********************************************/
/* GetFactListFunction: H/L access routine   */
/*   for the get-fact-list function.         */
/*********************************************/
globle void GetFactListFunction(
  DATA_OBJECT_PTR returnValue)
  {
   struct defmodule *theModule;
   DATA_OBJECT result;
   int numArgs;

   /*===========================================*/
   /* Determine if a module name was specified. */
   /*===========================================*/

   if ((numArgs = ArgCountCheck("get-fact-list",NO_MORE_THAN,1)) == -1)
     {
      SetMultifieldErrorValue(returnValue);
      return;
     }

   if (numArgs == 1)
     {
      RtnUnknown(1,&result);

      if (GetType(result) != SYMBOL)
        {
         SetMultifieldErrorValue(returnValue);
         ExpectedTypeError1("get-fact-list",1,"defmodule name");
         return;
        }

      if ((theModule = (struct defmodule *) FindDefmodule(DOToString(result))) == NULL)
        {
         if (strcmp("*",DOToString(result)) != 0)
           {
            SetMultifieldErrorValue(returnValue);
            ExpectedTypeError1("get-fact-list",1,"defmodule name");
            return;
           }

         theModule = NULL;
        }
     }
   else
     { theModule = ((struct defmodule *) GetCurrentModule()); }

   /*=====================*/
   /* Get the constructs. */
   /*=====================*/

   GetFactList(returnValue,theModule);
  }

/*************************************/
/* GetFactList: C access routine for */
/*   the get-fact-list function.     */
/*************************************/
globle void GetFactList(
  DATA_OBJECT_PTR returnValue,
  void *vTheModule)
  {
   struct fact *theFact;
   long count;
   struct multifield *theList;
   struct defmodule *theModule = (struct defmodule *) vTheModule;

   /*==========================*/
   /* Save the current module. */
   /*==========================*/

   SaveCurrentModule();

   /*============================================*/
   /* Count the number of facts to be retrieved. */
   /*============================================*/

   if (theModule == NULL)
     {
      for (theFact = (struct fact *) GetNextFact(NULL), count = 0;
           theFact != NULL;
           theFact = (struct fact *) GetNextFact(theFact), count++)
        { /* Do Nothing */ }
     }
   else
     {
      SetCurrentModule((void *) theModule);
      UpdateDeftemplateScope();
      for (theFact = (struct fact *) GetNextFactInScope(NULL), count = 0;
           theFact != NULL;
           theFact = (struct fact *) GetNextFactInScope(theFact), count++)
        { /* Do Nothing */ }
     }

   /*===========================================================*/
   /* Create the multifield value to store the construct names. */
   /*===========================================================*/

   SetpType(returnValue,MULTIFIELD);
   SetpDOBegin(returnValue,1);
   SetpDOEnd(returnValue,count);
   theList = (struct multifield *) CreateMultifield(count);
   SetpValue(returnValue,(void *) theList);

   /*==================================================*/
   /* Store the fact pointers in the multifield value. */
   /*==================================================*/

   if (theModule == NULL)
     {
      for (theFact = (struct fact *) GetNextFact(NULL), count = 1;
           theFact != NULL;
           theFact = (struct fact *) GetNextFact(theFact), count++)
        {
         SetMFType(theList,count,FACT_ADDRESS);
         SetMFValue(theList,count,(void *) theFact);
        }
     }
   else
     {
      for (theFact = (struct fact *) GetNextFactInScope(NULL), count = 1;
           theFact != NULL;
           theFact = (struct fact *) GetNextFactInScope(theFact), count++)
        {
         SetMFType(theList,count,FACT_ADDRESS);
         SetMFValue(theList,count,(void *) theFact);
        }
     }

   /*=============================*/
   /* Restore the current module. */
   /*=============================*/

   RestoreCurrentModule();
   UpdateDeftemplateScope();
  }

/**************************************************************/
/* GetFactAddressOrIndexArgument: Retrieves an argument for a */
/*   function which should be a reference to a valid fact.    */
/**************************************************************/
globle struct fact *GetFactAddressOrIndexArgument(
  char *theFunction,
  int position,
  int noFactError)
  {
   DATA_OBJECT item;
   long factIndex;
   struct fact *theFact;
   char tempBuffer[20];

   RtnUnknown(position,&item);

   if (GetType(item) == FACT_ADDRESS)
     {
      if (((struct fact *) GetValue(item))->garbage) return(NULL);
      else return (((struct fact *) GetValue(item)));
     }
   else if (GetType(item) == INTEGER)
     {
      factIndex = ValueToLong(item.value);
      if (factIndex < 0)
        {
         ExpectedTypeError1(theFunction,position,"fact-address or fact-index");
         return(NULL);
        }

      theFact = FindIndexedFact(factIndex);
      if ((theFact == NULL) && noFactError)
        {
         sprintf(tempBuffer,"f-%ld",factIndex);
         CantFindItemErrorMessage("fact",tempBuffer);
         return(NULL);
        }

      return(theFact);
     }

   ExpectedTypeError1(theFunction,position,"fact-address or fact-index");
   return(NULL);
  }

#endif /* DEFTEMPLATE_CONSTRUCT */



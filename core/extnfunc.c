   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*               EXTERNAL FUNCTION MODULE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Routines for adding new user or system defined   */
/*   functions.                                              */
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

#define _EXTNFUNC_SOURCE_

#include "setup.h"

#include <ctype.h>

#include "constant.h"
#include "router.h"
#include "memalloc.h"
#include "evaluatn.h"

#include "extnfunc.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    AddHashFunction(struct FunctionDefinition *);
   static void                    InitializeFunctionHashTable(void);
   static int                     RemoveHashFunction(struct FunctionDefinition *);

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static struct FunctionDefinition     *ListOfFunctions = NULL;
   Thread static struct FunctionHash          **FunctionHashtable;

#if (! RUN_TIME)

/************************************************************/
/* DefineFunction: Used to define a system or user external */
/*   function so that the KB can access it.                 */
/************************************************************/
globle int DefineFunction(
  char *name,
  int returnType,
  int (*pointer)(void),
  char *actualName)
  {
   return(DefineFunction2(name,returnType,pointer,actualName,NULL));
  }

/*************************************************************/
/* DefineFunction2: Used to define a system or user external */
/*   function so that the KB can access it. Allows argument  */
/*   restrictions to be attached to the function.            */
/*   Return types are:                                       */
/*     a - external address                                  */
/*     b - boolean integer (converted to symbol)             */
/*     c - character (converted to symbol)                   */
/*     d - double precision float                            */
/*     f - single precision float (converted to double)      */
/*     i - integer (converted to long integer)               */
/*     j - unknown (symbol, string,                          */
/*                  or instance name by convention)          */
/*     k - unknown (symbol or string by convention)          */
/*     l - long integer                                      */
/*     m - unknown (multifield by convention)                */
/*     n - unknown (integer or float by convention)          */
/*     o - instance name                                     */
/*     s - string                                            */
/*     u - unknown                                           */
/*     v - void                                              */
/*     w - symbol                                            */
/*     x - instance address                                  */
/*************************************************************/
globle int DefineFunction2(
  char *name,
  int returnType,
  int (*pointer)(void),
  char *actualName,
  char *restrictions)
  {
   struct FunctionDefinition *newFunction;

   if ( (returnType != 'a') &&
        (returnType != 'b') &&
        (returnType != 'c') &&
        (returnType != 'd') &&
        (returnType != 'f') &&
        (returnType != 'i') &&
        (returnType != 'j') &&
        (returnType != 'k') &&
        (returnType != 'l') &&
        (returnType != 'm') &&
        (returnType != 'n') &&
#if OBJECT_SYSTEM
        (returnType != 'o') &&
#endif
        (returnType != 's') &&
        (returnType != 'u') &&
        (returnType != 'v') &&
#if OBJECT_SYSTEM
        (returnType != 'x') &&
#endif
        (returnType != 'w') )
     { return(0); }

   newFunction = get_struct(FunctionDefinition);
   newFunction->callFunctionName = (SYMBOL_HN *) AddSymbol(name);
   newFunction->returnValueType = (char) returnType;
   newFunction->functionPointer = pointer;
   newFunction->next = GetFunctionList();
   newFunction->actualFunctionName = actualName;
   if (restrictions != NULL)
     {
      if (((int) (strlen(restrictions)) < 2) ? TRUE :
          ((! isdigit(restrictions[0]) && (restrictions[0] != '*')) ||
           (! isdigit(restrictions[1]) && (restrictions[1] != '*'))))
        restrictions = NULL;
     }
   newFunction->restrictions = restrictions;
   newFunction->parser = NULL;
   newFunction->overloadable = TRUE;
   newFunction->sequenceuseok = TRUE;
   newFunction->usrData = NULL;

   IncrementSymbolCount(newFunction->callFunctionName);
   ListOfFunctions = newFunction;
   AddHashFunction(newFunction);

   return(1);
  }

/***********************************************/
/* UndefineFunction: Used to remove a function */
/*   definition from the list of functions.    */
/***********************************************/
globle int UndefineFunction(
  char *functionName)
  {
   SYMBOL_HN *findValue;
   struct FunctionDefinition *fPtr, *lastPtr = NULL;

   findValue = (SYMBOL_HN *) FindSymbol(functionName);

   for (fPtr = ListOfFunctions;
        fPtr != NULL;
        fPtr = fPtr->next)
     {
      if (fPtr->callFunctionName == findValue)
        {
         DecrementSymbolCount(fPtr->callFunctionName);
         RemoveHashFunction(fPtr);

         if (lastPtr == NULL)
           { ListOfFunctions = fPtr->next; }
         else
           { lastPtr->next = fPtr->next; }

         rtn_struct(FunctionDefinition,fPtr);
         return(TRUE);
        }

      lastPtr = fPtr;
     }

   return(FALSE);
  }

/***************************************************************************/
/* AddFunctionParser: Associates a specialized expression parsing function */
/*   with the function entry for a function which was defined using        */
/*   DefineFunction. When this function is parsed, the specialized parsing */
/*   function will be called to parse the arguments of the function. Only  */
/*   user and system defined functions can have specialized parsing        */
/*   routines. Generic functions and deffunctions can not have specialized */
/*   parsing routines.                                                     */
/***************************************************************************/
globle int AddFunctionParser(
  char *functionName,
  struct expr *(*fpPtr)(struct expr *,char *))
  {
   struct FunctionDefinition *fdPtr;

   fdPtr = FindFunction(functionName);
   if (fdPtr == NULL)
     {
      PrintRouter(WERROR,"Function parsers can only be added for existing functions.\n");
      return(0);
     }
   fdPtr->restrictions = NULL;
   fdPtr->parser = fpPtr;
   fdPtr->overloadable = FALSE;

   return(1);
  }

/*********************************************************************/
/* RemoveFunctionParser: Removes a specialized expression parsing    */
/*   function (if it exists) from the function entry for a function. */
/*********************************************************************/
globle int RemoveFunctionParser(
  char *functionName)
  {
   struct FunctionDefinition *fdPtr;

   fdPtr = FindFunction(functionName);
   if (fdPtr == NULL)
     {
      PrintRouter(WERROR,"Function parsers can only be removed from existing functions.\n");
      return(0);
     }

   fdPtr->parser = NULL;

   return(1);
  }

/*****************************************************************/
/* FuncSeqOvlFlags: Makes a system function overloadable or not, */
/* i.e. can the function be a method for a generic function.     */
/*****************************************************************/
globle int FuncSeqOvlFlags(
  char *functionName,
  int seqp,
  int ovlp)
  {
   struct FunctionDefinition *fdPtr;

   fdPtr = FindFunction(functionName);
   if (fdPtr == NULL)
     {
      PrintRouter(WERROR,"Only existing functions can be marked as using sequence expansion arguments/overloadable or not.\n");
      return(FALSE);
     }
   fdPtr->sequenceuseok = (short) (seqp ? TRUE : FALSE);
   fdPtr->overloadable = (short) (ovlp ? TRUE : FALSE);
   return(TRUE);
  }

#endif

/*********************************************************/
/* GetArgumentTypeName: Returns a descriptive string for */
/*   a function argument type (used by DefineFunction2). */
/*********************************************************/
globle char *GetArgumentTypeName(
  int theRestriction)
  {
   switch ((char) theRestriction)
     {
      case 'a':
        return("external address");

      case 'e':
        return("instance address, instance name, or symbol");

      case 'd':
      case 'f':
        return("float");

      case 'g':
        return("integer, float, or symbol");

      case 'h':
        return("instance address, instance name, fact address, integer, or symbol");

      case 'j':
        return("symbol, string, or instance name");

      case 'k':
        return("symbol or string");

      case 'i':
      case 'l':
        return("integer");

      case 'm':
        return("multifield");

      case 'n':
        return("integer or float");

      case 'o':
        return("instance name");

      case 'p':
        return("instance name or symbol");

      case 'q':
        return("multifield, symbol, or string");

      case 's':
        return("string");

      case 'w':
        return("symbol");

      case 'x':
        return("instance address");

      case 'y':
        return("fact-address");

      case 'z':
        return("fact-address, integer, or symbol");

      case 'u':
        return("non-void return value");
     }

   return("unknown argument type");
  }

/***************************************************/
/* GetNthRestriction: Returns the restriction type */
/*   for the nth parameter of a function.          */
/***************************************************/
globle int GetNthRestriction(
  struct FunctionDefinition *theFunction,
  int position)
  {
   int defaultRestriction = (int) 'u', theLength;
   int i = 2;

   /*===========================================================*/
   /* If no restrictions at all are specified for the function, */
   /* then return 'u' to indicate that any value is suitable as */
   /* an argument to the function.                              */
   /*===========================================================*/

   if (theFunction == NULL) return(defaultRestriction);

   if (theFunction->restrictions == NULL) return(defaultRestriction);

   /*===========================================================*/
   /* If no type restrictions are specified for the function,   */
   /* then return 'u' to indicate that any value is suitable as */
   /* an argument to the function.                              */
   /*===========================================================*/

   theLength = strlen(theFunction->restrictions);

   if (theLength < 3) return(defaultRestriction);

   /*==============================================*/
   /* Determine the functions default restriction. */
   /*==============================================*/

   defaultRestriction = (int) theFunction->restrictions[i];

   if (defaultRestriction == '*') defaultRestriction = (int) 'u';

   /*=======================================================*/
   /* If the requested position does not have a restriction */
   /* specified, then return the default restriction.       */
   /*=======================================================*/

   if (theLength < (position + 3)) return(defaultRestriction);

   /*=========================================================*/
   /* Return the restriction specified for the nth parameter. */
   /*=========================================================*/

   return((int) theFunction->restrictions[position + 2]);
  }

/*************************************************/
/* GetFunctionList: Returns the ListOfFunctions. */
/*************************************************/
globle struct FunctionDefinition *GetFunctionList()
  {
   return(ListOfFunctions);
  }

/**************************************************************/
/* InstallFunctionList: Sets the ListOfFunctions and adds all */
/*   the function entries to the FunctionHashTable.           */
/**************************************************************/
globle void InstallFunctionList(
  struct FunctionDefinition *value)
  {
   int i;
   struct FunctionHash *fhPtr, *nextPtr;

   if (FunctionHashtable != NULL)
     {
      for (i = 0; i < SIZE_FUNCTION_HASH; i++)
        {
         fhPtr = FunctionHashtable[i];
         while (fhPtr != NULL)
           {
            nextPtr = fhPtr->next;
            rtn_struct(FunctionHash,fhPtr);
            fhPtr = nextPtr;
           }
         FunctionHashtable[i] = NULL;
        }
     }

   ListOfFunctions = value;

   while (value != NULL)
     {
      AddHashFunction(value);
      value = value->next;
     }
  }

/********************************************************/
/* FindFunction: Returns a pointer to the corresponding */
/*   FunctionDefinition structure if a function name is */
/*   in the function list, otherwise returns NULL.      */
/********************************************************/
globle struct FunctionDefinition *FindFunction(
  char *functionName)
  {
   struct FunctionHash *fhPtr;
   int hashValue;
   SYMBOL_HN *findValue;

   hashValue = HashSymbol(functionName,SIZE_FUNCTION_HASH);

   findValue = (SYMBOL_HN *) FindSymbol(functionName);

   for (fhPtr = FunctionHashtable[hashValue];
        fhPtr != NULL;
        fhPtr = fhPtr->next)
     {
      if (fhPtr->fdPtr->callFunctionName == findValue)
        { return(fhPtr->fdPtr); }
     }

   return(NULL);
  }

/*********************************************************/
/* InitializeFunctionHashTable: Purpose is to initialize */
/*   the function hash table to NULL.                    */
/*********************************************************/
static void InitializeFunctionHashTable()
  {
   int i;

   FunctionHashtable = (struct FunctionHash **)
                       gm2((int) sizeof (struct FunctionHash *) *
                           SIZE_FUNCTION_HASH);

   for (i = 0; i < SIZE_FUNCTION_HASH; i++) FunctionHashtable[i] = NULL;
  }

/****************************************************************/
/* AddHashFunction: Adds a function to the function hash table. */
/****************************************************************/
static void AddHashFunction(
  struct FunctionDefinition *fdPtr)
  {
   struct FunctionHash *newhash, *temp;
   int hashValue;

   if (FunctionHashtable == NULL) InitializeFunctionHashTable();

   newhash = get_struct(FunctionHash);
   newhash->fdPtr = fdPtr;

   hashValue = HashSymbol(fdPtr->callFunctionName->contents,SIZE_FUNCTION_HASH);

   temp = FunctionHashtable[hashValue];
   FunctionHashtable[hashValue] = newhash;
   newhash->next = temp;
  }

/******************************************/
/* RemoveHashFunction: Removes a function */
/*   from the function hash table.        */
/******************************************/
static int RemoveHashFunction(
  struct FunctionDefinition *fdPtr)
  {
   struct FunctionHash *fhPtr, *lastPtr = NULL;
   int hashValue;

   hashValue = HashSymbol(ValueToString(fdPtr->callFunctionName),SIZE_FUNCTION_HASH);

   for (fhPtr = FunctionHashtable[hashValue];
        fhPtr != NULL;
        fhPtr = fhPtr->next)
     {
      if (fhPtr->fdPtr == fdPtr)
        {
         if (lastPtr == NULL)
           { FunctionHashtable[hashValue] = fhPtr->next; }
         else
           { lastPtr->next = fhPtr->next; }

         rtn_struct(FunctionHash,fhPtr);
         return(TRUE);
        }

      lastPtr = fhPtr;
     }

   return(FALSE);
  }

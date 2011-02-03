   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  10/19/06            */
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
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Corrected code to remove run-time program      */
/*            compiler warning.                              */
/*                                                           */
/*      6.30: Added support for passing context information  */ 
/*            to user defined functions.                     */
/*                                                           */
/*************************************************************/

#define _EXTNFUNC_SOURCE_

#include "setup.h"

#include <ctype.h>
#include <stdlib.h>

#include "constant.h"
#include "envrnmnt.h"
#include "router.h"
#include "memalloc.h"
#include "evaluatn.h"

#include "extnfunc.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    AddHashFunction(void *,EXEC_STATUS,struct FunctionDefinition *);
   static void                    InitializeFunctionHashTable(void *,EXEC_STATUS);
   static void                    DeallocateExternalFunctionData(void *,EXEC_STATUS);
#if (! RUN_TIME)
   static int                     RemoveHashFunction(void *,EXEC_STATUS,struct FunctionDefinition *);
#endif

/*********************************************************/
/* InitializeExternalFunctionData: Allocates environment */
/*    data for external functions.                       */
/*********************************************************/
globle void InitializeExternalFunctionData(
  void *theEnv,
  EXEC_STATUS)
  {
   AllocateEnvironmentData(theEnv,execStatus,EXTERNAL_FUNCTION_DATA,sizeof(struct externalFunctionData),DeallocateExternalFunctionData);
  }

/***********************************************************/
/* DeallocateExternalFunctionData: Deallocates environment */
/*    data for external functions.                         */
/***********************************************************/
static void DeallocateExternalFunctionData(
  void *theEnv,
  EXEC_STATUS)
  {
   struct FunctionHash *fhPtr, *nextFHPtr;
   int i;

#if ! RUN_TIME
   struct FunctionDefinition *tmpPtr, *nextPtr;

   tmpPtr = ExternalFunctionData(theEnv,execStatus)->ListOfFunctions;
   while (tmpPtr != NULL)
     {
      nextPtr = tmpPtr->next;
      rtn_struct(theEnv,execStatus,FunctionDefinition,tmpPtr);
      tmpPtr = nextPtr;
     }
#endif

   if (ExternalFunctionData(theEnv,execStatus)->FunctionHashtable == NULL)
     { return; }
     
   for (i = 0; i < SIZE_FUNCTION_HASH; i++)
     {
      fhPtr = ExternalFunctionData(theEnv,execStatus)->FunctionHashtable[i];
      while (fhPtr != NULL)
        {
         nextFHPtr = fhPtr->next;
         rtn_struct(theEnv,execStatus,FunctionHash,fhPtr);
         fhPtr = nextFHPtr;
        }
     }
   
   genfree(theEnv,execStatus,ExternalFunctionData(theEnv,execStatus)->FunctionHashtable,
           (int) sizeof (struct FunctionHash *) * SIZE_FUNCTION_HASH);
  }

#if (! RUN_TIME)

/************************************************************/
/* DefineFunction: Used to define a system or user external */
/*   function so that the KB can access it.                 */
/************************************************************/
#if ALLOW_ENVIRONMENT_GLOBALS
globle int DefineFunction(
  char *name,
  int returnType,
  int (*pointer)(void),
  char *actualName)
  {
   void *theEnv;
   
   // Lode: TODO: add exec_status??
   theEnv = GetCurrentEnvironment();

   return(DefineFunction3(theEnv,execStatus,name,returnType,
                          (int (*)(void *)) pointer,
                          actualName,NULL,FALSE,NULL));
  }
#endif

/***************************************************************/
/* EnvDefineFunction: Used to define a system or user external */
/*   function so that the KB can access it.                    */
/***************************************************************/
globle int EnvDefineFunction(
  void *theEnv,
  EXEC_STATUS,
  char *name,
  int returnType,
  int (*pointer)(void *),
  char *actualName)
  {
   return(DefineFunction3(theEnv,execStatus,name,returnType,pointer,actualName,NULL,TRUE,NULL));
  }
  
/************************************************************/
/* EnvDefineFunctionWithContext: Used to define a system or */
/*   user external function so that the KB can access it.   */
/************************************************************/
globle int EnvDefineFunctionWithContext(
  void *theEnv,
  EXEC_STATUS,
  char *name,
  int returnType,
  int (*pointer)(void *),
  char *actualName,
  void *context)
  {
   return(DefineFunction3(theEnv,execStatus,name,returnType,pointer,actualName,NULL,TRUE,context));
  }
  
/*************************************************************/
/* DefineFunction2: Used to define a system or user external */
/*   function so that the KB can access it.                  */
/*************************************************************/
#if ALLOW_ENVIRONMENT_GLOBALS
globle int DefineFunction2(
  char *name,
  int returnType,
  int (*pointer)(void),
  char *actualName,
  char *restrictions)
  {
   void *theEnv;
   
	 // Lode: TODO add EXEC_STATUS??
   theEnv = GetCurrentEnvironment();

   return(DefineFunction3(theEnv,execStatus,name,returnType,
                          (int (*)(void *)) pointer,
                          actualName,restrictions,FALSE,NULL));
  }
#endif
  
/*************************************************************/
/* EnvDefineFunction2: Used to define a system or user external */
/*   function so that the KB can access it.                  */
/*************************************************************/
globle int EnvDefineFunction2(
  void *theEnv,
  EXEC_STATUS,
  char *name,
  int returnType,
  int (*pointer)(void *),
  char *actualName,
  char *restrictions)
  {
   return(DefineFunction3(theEnv,execStatus,name,returnType,pointer,actualName,restrictions,TRUE,NULL));
  }

/*************************************************************/
/* EnvDefineFunction2WithContext: Used to define a system or */
/*   user external function so that the KB can access it.    */
/*************************************************************/
globle int EnvDefineFunction2WithContext(
  void *theEnv,
  EXEC_STATUS,
  char *name,
  int returnType,
  int (*pointer)(void *),
  char *actualName,
  char *restrictions,
  void *context)
  {
   return(DefineFunction3(theEnv,execStatus,name,returnType,pointer,actualName,restrictions,TRUE,context));
  }

/*************************************************************/
/* DefineFunction3: Used to define a system or user external */
/*   function so that the KB can access it. Allows argument  */
/*   restrictions to be attached to the function.            */
/*   Return types are:                                       */
/*     a - external address                                  */
/*     b - boolean integer (converted to symbol)             */
/*     c - character (converted to symbol)                   */
/*     d - double precision float                            */
/*     f - single precision float (converted to double)      */
/*     g - long long integer                                 */
/*     i - integer (converted to long long integer)          */
/*     j - unknown (symbol, string,                          */
/*                  or instance name by convention)          */
/*     k - unknown (symbol or string by convention)          */
/*     l - long integer (converted to long long integer)     */
/*     m - unknown (multifield by convention)                */
/*     n - unknown (integer or float by convention)          */
/*     o - instance name                                     */
/*     s - string                                            */
/*     u - unknown                                           */
/*     v - void                                              */
/*     w - symbol                                            */
/*     x - instance address                                  */
/*************************************************************/
globle int DefineFunction3(
  void *theEnv,
  EXEC_STATUS,
  char *name,
  int returnType,
  int (*pointer)(void *),
  char *actualName,
  char *restrictions,
  intBool environmentAware,
  void *context)
  {
   struct FunctionDefinition *newFunction;

   if ( (returnType != 'a') &&
        (returnType != 'b') &&
        (returnType != 'c') &&
        (returnType != 'd') &&
        (returnType != 'f') &&
        (returnType != 'g') &&
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

   newFunction = FindFunction(theEnv,execStatus,name);
   if (newFunction == NULL)
     {
      newFunction = get_struct(theEnv,execStatus,FunctionDefinition);
      newFunction->callFunctionName = (SYMBOL_HN *) EnvAddSymbol(theEnv,execStatus,name);
      IncrementSymbolCount(newFunction->callFunctionName);
      newFunction->next = GetFunctionList(theEnv,execStatus);
      ExternalFunctionData(theEnv,execStatus)->ListOfFunctions = newFunction;
      AddHashFunction(theEnv,execStatus,newFunction);
     }
     
   newFunction->returnValueType = (char) returnType;
   newFunction->functionPointer = (int (*)(void)) pointer;
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
   newFunction->environmentAware = (short) environmentAware;
   newFunction->usrData = NULL;
   newFunction->context = context;

   return(1);
  }
  
/***********************************************/
/* UndefineFunction: Used to remove a function */
/*   definition from the list of functions.    */
/***********************************************/
globle int UndefineFunction(
  void *theEnv,
  EXEC_STATUS,
  char *functionName)
  {
   SYMBOL_HN *findValue;
   struct FunctionDefinition *fPtr, *lastPtr = NULL;

   findValue = (SYMBOL_HN *) FindSymbolHN(theEnv,execStatus,functionName);

   for (fPtr = ExternalFunctionData(theEnv,execStatus)->ListOfFunctions;
        fPtr != NULL;
        fPtr = fPtr->next)
     {
      if (fPtr->callFunctionName == findValue)
        {
         DecrementSymbolCount(theEnv,execStatus,fPtr->callFunctionName);
         RemoveHashFunction(theEnv,execStatus,fPtr);

         if (lastPtr == NULL)
           { ExternalFunctionData(theEnv,execStatus)->ListOfFunctions = fPtr->next; }
         else
           { lastPtr->next = fPtr->next; }
           
         ClearUserDataList(theEnv,execStatus,fPtr->usrData);
         rtn_struct(theEnv,execStatus,FunctionDefinition,fPtr);
         return(TRUE);
        }

      lastPtr = fPtr;
     }

   return(FALSE);
  }

/******************************************/
/* RemoveHashFunction: Removes a function */
/*   from the function hash table.        */
/******************************************/
static int RemoveHashFunction(
  void *theEnv,
  EXEC_STATUS,
  struct FunctionDefinition *fdPtr)
  {
   struct FunctionHash *fhPtr, *lastPtr = NULL;
   unsigned hashValue;

   hashValue = HashSymbol(ValueToString(fdPtr->callFunctionName),SIZE_FUNCTION_HASH);

   for (fhPtr = ExternalFunctionData(theEnv,execStatus)->FunctionHashtable[hashValue];
        fhPtr != NULL;
        fhPtr = fhPtr->next)
     {
      if (fhPtr->fdPtr == fdPtr)
        {
         if (lastPtr == NULL)
           { ExternalFunctionData(theEnv,execStatus)->FunctionHashtable[hashValue] = fhPtr->next; }
         else
           { lastPtr->next = fhPtr->next; }

         rtn_struct(theEnv,execStatus,FunctionHash,fhPtr);
         return(TRUE);
        }

      lastPtr = fhPtr;
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
  void *theEnv,
  EXEC_STATUS,
  char *functionName,
  struct expr *(*fpPtr)(void *,EXEC_STATUS,struct expr *,char *))
  {
   struct FunctionDefinition *fdPtr;

   fdPtr = FindFunction(theEnv,execStatus,functionName);
   if (fdPtr == NULL)
     {
      EnvPrintRouter(theEnv,execStatus,WERROR,"Function parsers can only be added for existing functions.\n");
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
  void *theEnv,
  EXEC_STATUS,
  char *functionName)
  {
   struct FunctionDefinition *fdPtr;

   fdPtr = FindFunction(theEnv,execStatus,functionName);
   if (fdPtr == NULL)
     {
      EnvPrintRouter(theEnv,execStatus,WERROR,"Function parsers can only be removed from existing functions.\n");
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
  void *theEnv,
  EXEC_STATUS,
  char *functionName,
  int seqp,
  int ovlp)
  {
   struct FunctionDefinition *fdPtr;

   fdPtr = FindFunction(theEnv,execStatus,functionName);
   if (fdPtr == NULL)
     {
      EnvPrintRouter(theEnv,execStatus,WERROR,"Only existing functions can be marked as using sequence expansion arguments/overloadable or not.\n");
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
   int defaultRestriction = (int) 'u';
   size_t theLength;
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

   if (theLength < (size_t) (position + 3)) return(defaultRestriction);

   /*=========================================================*/
   /* Return the restriction specified for the nth parameter. */
   /*=========================================================*/

   return((int) theFunction->restrictions[position + 2]);
  }

/*************************************************/
/* GetFunctionList: Returns the ListOfFunctions. */
/*************************************************/
globle struct FunctionDefinition *GetFunctionList(
  void *theEnv,
  EXEC_STATUS)
  {
   return(ExternalFunctionData(theEnv,execStatus)->ListOfFunctions);
  }

/**************************************************************/
/* InstallFunctionList: Sets the ListOfFunctions and adds all */
/*   the function entries to the FunctionHashTable.           */
/**************************************************************/
globle void InstallFunctionList(
  void *theEnv,
  EXEC_STATUS,
  struct FunctionDefinition *value)
  {
   int i;
   struct FunctionHash *fhPtr, *nextPtr;

   if (ExternalFunctionData(theEnv,execStatus)->FunctionHashtable != NULL)
     {
      for (i = 0; i < SIZE_FUNCTION_HASH; i++)
        {
         fhPtr = ExternalFunctionData(theEnv,execStatus)->FunctionHashtable[i];
         while (fhPtr != NULL)
           {
            nextPtr = fhPtr->next;
            rtn_struct(theEnv,execStatus,FunctionHash,fhPtr);
            fhPtr = nextPtr;
           }
         ExternalFunctionData(theEnv,execStatus)->FunctionHashtable[i] = NULL;
        }
     }

   ExternalFunctionData(theEnv,execStatus)->ListOfFunctions = value;

   while (value != NULL)
     {
      AddHashFunction(theEnv,execStatus,value);
      value = value->next;
     }
  }

/********************************************************/
/* FindFunction: Returns a pointer to the corresponding */
/*   FunctionDefinition structure if a function name is */
/*   in the function list, otherwise returns NULL.      */
/********************************************************/
globle struct FunctionDefinition *FindFunction(
  void *theEnv,
  EXEC_STATUS,
  char *functionName)
  {
   struct FunctionHash *fhPtr;
   unsigned hashValue;
   SYMBOL_HN *findValue;

   if (ExternalFunctionData(theEnv,execStatus)->FunctionHashtable == NULL) return(NULL);
   
   hashValue = HashSymbol(functionName,SIZE_FUNCTION_HASH);

   findValue = (SYMBOL_HN *) FindSymbolHN(theEnv,execStatus,functionName);

   for (fhPtr = ExternalFunctionData(theEnv,execStatus)->FunctionHashtable[hashValue];
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
static void InitializeFunctionHashTable(
  void *theEnv,
  EXEC_STATUS)
  {
   int i;

   ExternalFunctionData(theEnv,execStatus)->FunctionHashtable = (struct FunctionHash **)
                       gm2(theEnv,execStatus,(int) sizeof (struct FunctionHash *) *
                           SIZE_FUNCTION_HASH);

   for (i = 0; i < SIZE_FUNCTION_HASH; i++) ExternalFunctionData(theEnv,execStatus)->FunctionHashtable[i] = NULL;
  }

/****************************************************************/
/* AddHashFunction: Adds a function to the function hash table. */
/****************************************************************/
static void AddHashFunction(
  void *theEnv,
  EXEC_STATUS,
  struct FunctionDefinition *fdPtr)
  {
   struct FunctionHash *newhash, *temp;
   unsigned hashValue;

   if (ExternalFunctionData(theEnv,execStatus)->FunctionHashtable == NULL) InitializeFunctionHashTable(theEnv,execStatus);

   newhash = get_struct(theEnv,execStatus,FunctionHash);
   newhash->fdPtr = fdPtr;

   hashValue = HashSymbol(fdPtr->callFunctionName->contents,SIZE_FUNCTION_HASH);

   temp = ExternalFunctionData(theEnv,execStatus)->FunctionHashtable[hashValue];
   ExternalFunctionData(theEnv,execStatus)->FunctionHashtable[hashValue] = newhash;
   newhash->next = temp;
  }

/*************************************************/
/* GetMinimumArgs: Returns the minimum number of */
/*   arguments expected by an external function. */
/*************************************************/
globle int GetMinimumArgs(
  struct FunctionDefinition *theFunction)
  {
   char theChar[2], *restrictions;

   restrictions = theFunction->restrictions;
   if (restrictions == NULL) return(-1);

   theChar[0] = restrictions[0];
   theChar[1] = '\0';

   if (isdigit(theChar[0]))
     { return atoi(theChar); }
   else if (theChar[0] == '*')
     { return(-1); }
   
   return(-1); 
  }
  
/*************************************************/
/* GetMaximumArgs: Returns the maximum number of */
/*   arguments expected by an external function. */
/*************************************************/
globle int GetMaximumArgs(
  struct FunctionDefinition *theFunction)
  {
   char theChar[2], *restrictions;

   restrictions = theFunction->restrictions;
   if (restrictions == NULL) return(-1);
   if (restrictions[0] == '\0') return(-1);

   theChar[0] = restrictions[1];
   theChar[1] = '\0';

   if (isdigit(theChar[0]))
     { return atoi(theChar); }
   else if (theChar[0] == '*')
     { return(-1); }
   
   return(-1); 
  }

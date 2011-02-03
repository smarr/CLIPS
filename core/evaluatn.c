   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  10/19/06            */
   /*                                                     */
   /*                  EVALUATION MODULE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides routines for evaluating expressions.    */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added EvaluateAndStoreInDataObject function.   */
/*                                                           */
/*      6.30: Added support for passing context information  */ 
/*            to user defined functions.                     */
/*                                                           */
/*************************************************************/

#define _EVALUATN_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "setup.h"

#include "argacces.h"
#include "commline.h"
#include "constant.h"
#include "envrnmnt.h"
#include "memalloc.h"
#include "router.h"
#include "extnfunc.h"
#include "prcdrfun.h"
#include "multifld.h"
#include "fact/fact_manager.h"
#include "prntutil.h"
#include "exprnpsr.h"
#include "utility.h"
#include "proflfun.h"
#include "sysdep.h"

#if DEFFUNCTION_CONSTRUCT
#include "dffnxfun.h"
#endif

#if DEFGENERIC_CONSTRUCT
#include "genrccom.h"
#endif

#if OBJECT_SYSTEM
#include "object.h"
#endif

#include "evaluatn.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    PropagateReturnAtom(void *,EXEC_STATUS,int,void *);
   static void                    DeallocateEvaluationData(void *,EXEC_STATUS);
   static void                    PrintCAddress(void *,EXEC_STATUS,char *,void *);
   static void                    NewCAddress(void *,EXEC_STATUS,DATA_OBJECT *);
   /*
   static intBool                 DiscardCAddress(void *,EXEC_STATUS,void *);
   */
   
/**************************************************/
/* InitializeEvaluationData: Allocates environment */
/*    data for expression evaluation.             */
/**************************************************/
globle void InitializeEvaluationData(
  void *theEnv,
  EXEC_STATUS)
  {
   struct externalAddressType cPointer = { "C", PrintCAddress, PrintCAddress, NULL, NewCAddress, NULL };
   
   AllocateEnvironmentData(theEnv,execStatus,EVALUATION_DATA,sizeof(struct evaluationData),DeallocateEvaluationData);

   InstallExternalAddressType(theEnv,execStatus,&cPointer);
  }

/*****************************************************/
/* DeallocateEvaluationData: Deallocates environment */
/*    data for evaluation data.                      */
/*****************************************************/
static void DeallocateEvaluationData(
  void *theEnv,
  EXEC_STATUS)
  {
   int i;
   
   for (i = 0; i < EvaluationData(theEnv,execStatus)->numberOfAddressTypes; i++)
     { rtn_struct(theEnv,execStatus,externalAddressType,EvaluationData(theEnv,execStatus)->ExternalAddressTypes[i]); }
  }

/**************************************************************/
/* EvaluateExpression: Evaluates an expression. Returns FALSE */
/*   if no errors occurred during evaluation, otherwise TRUE. */
/**************************************************************/
globle int EvaluateExpression(
  void *theEnv,
  EXEC_STATUS,
  struct expr *problem,
  DATA_OBJECT_PTR returnValue)
  {
   struct expr *oldArgument;
   void *oldContext;
   struct FunctionDefinition *fptr;
#if PROFILING_FUNCTIONS
   struct profileFrameInfo profileFrame;
#endif

   if (problem == NULL)
     {
      returnValue->type = SYMBOL;
      returnValue->value = EnvFalseSymbol(theEnv,execStatus);
      return(execStatus->EvaluationError);
     }

   switch (problem->type)
     {
      case STRING:
      case SYMBOL:
      case FLOAT:
      case INTEGER:
#if OBJECT_SYSTEM
      case INSTANCE_NAME:
      case INSTANCE_ADDRESS:
#endif
      case EXTERNAL_ADDRESS:
        returnValue->type = problem->type;
        returnValue->value = problem->value;
        break;

      case DATA_OBJECT_ARRAY: /* TBD Remove with AddPrimitive */
        returnValue->type = problem->type;
        returnValue->value = problem->value;
        break;

      case FCALL:
        {
         fptr = (struct FunctionDefinition *) problem->value;
         oldContext = SetEnvironmentFunctionContext(theEnv,execStatus,fptr->context);

#if PROFILING_FUNCTIONS   
         StartProfile(theEnv,execStatus,&profileFrame,
                      &fptr->usrData,
                      ProfileFunctionData(theEnv,execStatus)->ProfileUserFunctions);
#endif

         oldArgument = execStatus->CurrentExpression;
         execStatus->CurrentExpression = problem;

         switch(fptr->returnValueType)
           {
            case 'v' :
              if (fptr->environmentAware)
                { (* (void (*)(void *,EXEC_STATUS)) fptr->functionPointer)(theEnv,execStatus); }
              else
                { (* (void (*)(void)) fptr->functionPointer)(); }
              returnValue->type = RVOID;
              returnValue->value = EnvFalseSymbol(theEnv,execStatus);
              break;
            case 'b' :
              returnValue->type = SYMBOL;
              if (fptr->environmentAware)
                {
                 if ((* (int (*)(void *,EXEC_STATUS)) fptr->functionPointer)(theEnv,execStatus))
                   returnValue->value = EnvTrueSymbol(theEnv,execStatus);
                 else
                   returnValue->value = EnvFalseSymbol(theEnv,execStatus);
                }
              else
                {
                 if ((* (int (*)(void)) fptr->functionPointer)())
                   returnValue->value = EnvTrueSymbol(theEnv,execStatus);
                 else
                   returnValue->value = EnvFalseSymbol(theEnv,execStatus);
                }
              break;
            case 'a' :
              returnValue->type = EXTERNAL_ADDRESS;
              if (fptr->environmentAware)
                {
                 returnValue->value =
                                (* (void *(*)(void *,EXEC_STATUS)) fptr->functionPointer)(theEnv,execStatus);
                }
              else
                {
                 returnValue->value =
                                (* (void *(*)(void)) fptr->functionPointer)();
                }
              break;
            case 'g' :
              returnValue->type = INTEGER;
              if (fptr->environmentAware)
                {
                 returnValue->value = (void *)
                   EnvAddLong(theEnv,execStatus,(* (long long (*)(void *,EXEC_STATUS)) fptr->functionPointer)(theEnv,execStatus));
                }
              else
                {
                 returnValue->value = (void *)
                   EnvAddLong(theEnv,execStatus,(* (long long (*)(void)) fptr->functionPointer)());
                }
              break;
            case 'i' :
              returnValue->type = INTEGER;
              if (fptr->environmentAware)
                {
                 returnValue->value = (void *)
                   EnvAddLong(theEnv,execStatus,(long long) (* (int (*)(void *,EXEC_STATUS)) fptr->functionPointer)(theEnv,execStatus));
                }
              else
                {
                 returnValue->value = (void *)
                   EnvAddLong(theEnv,execStatus,(long long) (* (int (*)(void)) fptr->functionPointer)());
                }
              break;
            case 'l' :
              returnValue->type = INTEGER;
              if (fptr->environmentAware)
                {
                 returnValue->value = (void *)
                    EnvAddLong(theEnv,execStatus,(long long) (* (long int (*)(void *,EXEC_STATUS)) fptr->functionPointer)(theEnv,execStatus));
                }
              else
                {
                 returnValue->value = (void *)
                    EnvAddLong(theEnv,execStatus,(long long) (* (long int (*)(void)) fptr->functionPointer)());
                }
              break;
            case 'f' :
              returnValue->type = FLOAT;
              if (fptr->environmentAware)
                {
                 returnValue->value = (void *)
                    EnvAddDouble(theEnv,execStatus,(double) (* (float (*)(void *,EXEC_STATUS)) fptr->functionPointer)(theEnv,execStatus));
                }
              else
                {
                 returnValue->value = (void *)
                    EnvAddDouble(theEnv,execStatus,(double) (* (float (*)(void)) fptr->functionPointer)());
                }
              break;
            case 'd' :
              returnValue->type = FLOAT;
              if (fptr->environmentAware)
                {
                 returnValue->value = (void *)
                    EnvAddDouble(theEnv,execStatus,(* (double (*)(void *,EXEC_STATUS)) fptr->functionPointer)(theEnv,execStatus));
                }
              else
                {
                 returnValue->value = (void *)
                    EnvAddDouble(theEnv,execStatus,(* (double (*)(void)) fptr->functionPointer)());
                }
              break;
            case 's' :
              returnValue->type = STRING;
              if (fptr->environmentAware)
                {
                 returnValue->value = (void *)
                   (* (SYMBOL_HN *(*)(void *,EXEC_STATUS)) fptr->functionPointer)(theEnv,execStatus);
                }
              else
                {
                 returnValue->value = (void *)
                   (* (SYMBOL_HN *(*)(void)) fptr->functionPointer)();
                }
              break;
            case 'w' :
              returnValue->type = SYMBOL;
              if (fptr->environmentAware)
                {
                 returnValue->value = (void *)
                   (* (SYMBOL_HN *(*)(void *,EXEC_STATUS)) fptr->functionPointer)(theEnv,execStatus);
                }
              else
                {
                 returnValue->value = (void *)
                   (* (SYMBOL_HN *(*)(void)) fptr->functionPointer)();
                }
              break;
#if OBJECT_SYSTEM
            case 'x' :
              returnValue->type = INSTANCE_ADDRESS;
              if (fptr->environmentAware)
                {
                 returnValue->value =
                                (* (void *(*)(void *,EXEC_STATUS)) fptr->functionPointer)(theEnv,execStatus);
                }
              else
                {
                 returnValue->value =
                                (* (void *(*)(void)) fptr->functionPointer)();
                }
              break;
            case 'o' :
              returnValue->type = INSTANCE_NAME;
              if (fptr->environmentAware)
                {
                 returnValue->value = (void *)
                   (* (SYMBOL_HN *(*)(void *,EXEC_STATUS)) fptr->functionPointer)(theEnv,execStatus);
                }
              else
                {
                 returnValue->value = (void *)
                   (* (SYMBOL_HN *(*)(void)) fptr->functionPointer)();
                }
              break;
#endif
            case 'c' :
              {
               char cbuff[2];
               if (fptr->environmentAware)
                 {
                  cbuff[0] = (* (char (*)(void *,EXEC_STATUS)) fptr->functionPointer)(theEnv,execStatus);
                 }
               else
                 {
                  cbuff[0] = (* (char (*)(void)) fptr->functionPointer)();
                 }
               cbuff[1] = EOS;
               returnValue->type = SYMBOL;
               returnValue->value = (void *) EnvAddSymbol(theEnv,execStatus,cbuff);
               break;
              }

            case 'j' :
            case 'k' :
            case 'm' :
            case 'n' :
            case 'u' :
               if (fptr->environmentAware)
                 {
                  (* (void (*)(void *,EXEC_STATUS,DATA_OBJECT_PTR)) fptr->functionPointer)(theEnv,execStatus,returnValue);
                 }
               else
                 {
                  (* (void (*)(DATA_OBJECT_PTR)) fptr->functionPointer)(returnValue);
                 }
              break;

            default :
               SystemError(theEnv,execStatus,"EVALUATN",2);
               EnvExitRouter(theEnv,execStatus,EXIT_FAILURE);
               break;
            }

#if PROFILING_FUNCTIONS 
        EndProfile(theEnv,execStatus,&profileFrame);
#endif

        SetEnvironmentFunctionContext(theEnv,execStatus,oldContext);
        execStatus->CurrentExpression = oldArgument;
        break;
        }

     case MULTIFIELD:
        returnValue->type = MULTIFIELD;
        returnValue->value = ((DATA_OBJECT_PTR) (problem->value))->value;
        returnValue->begin = ((DATA_OBJECT_PTR) (problem->value))->begin;
        returnValue->end = ((DATA_OBJECT_PTR) (problem->value))->end;
        break;

     case MF_VARIABLE:
     case SF_VARIABLE:
        if (GetBoundVariable(theEnv,execStatus,returnValue,(SYMBOL_HN *) problem->value) == FALSE)
          {
           PrintErrorID(theEnv,execStatus,"EVALUATN",1,FALSE);
           EnvPrintRouter(theEnv,execStatus,WERROR,"Variable ");
           EnvPrintRouter(theEnv,execStatus,WERROR,ValueToString(problem->value));
           EnvPrintRouter(theEnv,execStatus,WERROR," is unbound\n");
           returnValue->type = SYMBOL;
           returnValue->value = EnvFalseSymbol(theEnv,execStatus);
           SetEvaluationError(theEnv,execStatus,TRUE);
          }
        break;

      default:
        if (EvaluationData(theEnv,execStatus)->PrimitivesArray[problem->type] == NULL)
          {
           SystemError(theEnv,execStatus,"EVALUATN",3);
           EnvExitRouter(theEnv,execStatus,EXIT_FAILURE);
          }

        if (EvaluationData(theEnv,execStatus)->PrimitivesArray[problem->type]->copyToEvaluate)
          {
           returnValue->type = problem->type;
           returnValue->value = problem->value;
           break;
          }

        if (EvaluationData(theEnv,execStatus)->PrimitivesArray[problem->type]->evaluateFunction == NULL)
          {
           SystemError(theEnv,execStatus,"EVALUATN",4);
           EnvExitRouter(theEnv,execStatus,EXIT_FAILURE);
          }

        oldArgument = execStatus->CurrentExpression;
        execStatus->CurrentExpression = problem;

#if PROFILING_FUNCTIONS 
        StartProfile(theEnv,execStatus,&profileFrame,
                     &EvaluationData(theEnv,execStatus)->PrimitivesArray[problem->type]->usrData,
                     ProfileFunctionData(theEnv,execStatus)->ProfileUserFunctions);
#endif

        (*EvaluationData(theEnv,execStatus)->PrimitivesArray[problem->type]->evaluateFunction)(theEnv,execStatus,problem->value,returnValue);

#if PROFILING_FUNCTIONS
        EndProfile(theEnv,execStatus,&profileFrame);
#endif

        execStatus->CurrentExpression = oldArgument;
        break;
     }

   PropagateReturnValue(theEnv,execStatus,returnValue);
   return(execStatus->EvaluationError);
  }

/******************************************/
/* InstallPrimitive: Installs a primitive */
/*   data type in the primitives array.   */
/******************************************/
globle void InstallPrimitive(
  void *theEnv,
  EXEC_STATUS,
  struct entityRecord *thePrimitive,
  int whichPosition)
  {
   if (EvaluationData(theEnv,execStatus)->PrimitivesArray[whichPosition] != NULL)
     {
      SystemError(theEnv,execStatus,"EVALUATN",5);
      EnvExitRouter(theEnv,execStatus,EXIT_FAILURE);
     }

   EvaluationData(theEnv,execStatus)->PrimitivesArray[whichPosition] = thePrimitive;
  }

/******************************************************/
/* InstallExternalAddressType: Installs an external   */
/*   address type in the external address type array. */
/******************************************************/
globle int InstallExternalAddressType(
  void *theEnv,
  EXEC_STATUS,
  struct externalAddressType *theAddressType)
  {
   struct externalAddressType *copyEAT;
   
   int rv = EvaluationData(theEnv,execStatus)->numberOfAddressTypes;
   
   if (EvaluationData(theEnv,execStatus)->numberOfAddressTypes == MAXIMUM_EXTERNAL_ADDRESS_TYPES)
     {
      SystemError(theEnv,execStatus,"EVALUATN",6);
      EnvExitRouter(theEnv,execStatus,EXIT_FAILURE);
     }

   copyEAT = (struct externalAddressType *) genalloc(theEnv,execStatus,sizeof(struct externalAddressType));
   memcpy(copyEAT,theAddressType,sizeof(struct externalAddressType));   
   EvaluationData(theEnv,execStatus)->ExternalAddressTypes[EvaluationData(theEnv,execStatus)->numberOfAddressTypes++] = copyEAT;
   
   return rv;
  }

/******************************************************/
/* SetEvaluationError: Sets the EvaluationError flag. */
/******************************************************/
globle void SetEvaluationError(
  void *theEnv,
  EXEC_STATUS,
  int value)
  {
   execStatus->EvaluationError = value;
   if (value == TRUE) 
     { execStatus->HaltExecution = TRUE; }
  }

/*********************************************************/
/* GetEvaluationError: Returns the EvaluationError flag. */
/*********************************************************/
globle int GetEvaluationError(
  void *theEnv,
  EXEC_STATUS)
  {
   return(execStatus->EvaluationError);
  }

/**************************************************/
/* SetHaltExecution: Sets the HaltExecution flag. */
/**************************************************/
globle void SetHaltExecution(
  void *theEnv,
  EXEC_STATUS,
  int value)
  { 
   execStatus->HaltExecution = value; 
  }

/*****************************************************/
/* GetHaltExecution: Returns the HaltExecution flag. */
/*****************************************************/
globle int GetHaltExecution(
  void *theEnv,
  EXEC_STATUS)
  {
   return(execStatus->HaltExecution);
  }

/******************************************************/
/* ReturnValues: Returns a linked list of DATA_OBJECT */
/*   structures to the pool of free memory.           */
/******************************************************/
globle void ReturnValues(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT_PTR garbagePtr,
  intBool decrementSupplementalInfo)
  {
   DATA_OBJECT_PTR nextPtr;

   while (garbagePtr != NULL)
     {
      nextPtr = garbagePtr->next;
      ValueDeinstall(theEnv,execStatus,garbagePtr);
      if ((garbagePtr->supplementalInfo != NULL) && decrementSupplementalInfo)
        { DecrementSymbolCount(theEnv,execStatus,(struct symbolHashNode *) garbagePtr->supplementalInfo); }
      rtn_struct(theEnv,execStatus,dataObject,garbagePtr);
      garbagePtr = nextPtr;
     }
  }

/***************************************************/
/* PrintDataObject: Prints a DATA_OBJECT structure */
/*   to the specified logical name.                */
/***************************************************/
globle void PrintDataObject(
  void *theEnv,
  EXEC_STATUS,
  char *fileid,
  DATA_OBJECT_PTR argPtr)
  {
   switch(argPtr->type)
     {
      case RVOID:
      case SYMBOL:
      case STRING:
      case INTEGER:
      case FLOAT:
      case EXTERNAL_ADDRESS:
      case FACT_ADDRESS:
#if OBJECT_SYSTEM
      case INSTANCE_NAME:
      case INSTANCE_ADDRESS:
#endif
        PrintAtom(theEnv,execStatus,fileid,argPtr->type,argPtr->value);
        break;

      case MULTIFIELD:
        PrintMultifield(theEnv,execStatus,fileid,(struct multifield *) argPtr->value,
                        argPtr->begin,argPtr->end,TRUE);
        break;

      default:
        if (EvaluationData(theEnv,execStatus)->PrimitivesArray[argPtr->type] != NULL)
          {
           if (EvaluationData(theEnv,execStatus)->PrimitivesArray[argPtr->type]->longPrintFunction)
             {
              (*EvaluationData(theEnv,execStatus)->PrimitivesArray[argPtr->type]->longPrintFunction)(theEnv,execStatus,fileid,argPtr->value);
              break;
             }
           else if (EvaluationData(theEnv,execStatus)->PrimitivesArray[argPtr->type]->shortPrintFunction)
             {
              (*EvaluationData(theEnv,execStatus)->PrimitivesArray[argPtr->type]->shortPrintFunction)(theEnv,execStatus,fileid,argPtr->value);
              break;
             }
          }

        EnvPrintRouter(theEnv,execStatus,fileid,"<UnknownPrintType");
        PrintLongInteger(theEnv,execStatus,fileid,(long int) argPtr->type);
        EnvPrintRouter(theEnv,execStatus,fileid,">");
        SetHaltExecution(theEnv,execStatus,TRUE);
        SetEvaluationError(theEnv,execStatus,TRUE);
        break;
     }
  }

/****************************************************/
/* EnvSetMultifieldErrorValue: Creates a multifield */
/*   value of length zero for error returns.        */
/****************************************************/
globle void EnvSetMultifieldErrorValue(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT_PTR returnValue)
  {
   returnValue->type = MULTIFIELD;
   returnValue->value = EnvCreateMultifield(theEnv,execStatus,0L);
   returnValue->begin = 1;
   returnValue->end = 0;
  }

/**************************************************/
/* ValueInstall: Increments the appropriate count */
/*   (in use) values for a DATA_OBJECT structure. */
/**************************************************/
globle void ValueInstall(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *vPtr)
  {
   if (vPtr->type == MULTIFIELD) MultifieldInstall(theEnv,execStatus,(struct multifield *) vPtr->value);
   else AtomInstall(theEnv,execStatus,vPtr->type,vPtr->value);
  }

/****************************************************/
/* ValueDeinstall: Decrements the appropriate count */
/*   (in use) values for a DATA_OBJECT structure.   */
/****************************************************/
globle void ValueDeinstall(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *vPtr)
  {
   if (vPtr->type == MULTIFIELD) MultifieldDeinstall(theEnv,execStatus,(struct multifield *) vPtr->value);
   else AtomDeinstall(theEnv,execStatus,vPtr->type,vPtr->value);
  }

/*****************************************/
/* AtomInstall: Increments the reference */
/*   count of an atomic data type.       */
/*****************************************/
globle void AtomInstall(
  void *theEnv,
  EXEC_STATUS,
  int type,
  void *vPtr)
  {
   switch (type)
     {
      case SYMBOL:
      case STRING:
#if DEFGLOBAL_CONSTRUCT
      case GBL_VARIABLE:
#endif
#if OBJECT_SYSTEM
      case INSTANCE_NAME:
#endif
        IncrementSymbolCount(vPtr);
        break;

      case FLOAT:
        IncrementFloatCount(vPtr);
        break;

      case INTEGER:
        IncrementIntegerCount(vPtr);
        break;

      case EXTERNAL_ADDRESS:
        IncrementExternalAddressCount(vPtr);
        break;

      case MULTIFIELD:
        MultifieldInstall(theEnv,execStatus,(struct multifield *) vPtr);
        break;

      case RVOID:
        break;

      default:
        if (EvaluationData(theEnv,execStatus)->PrimitivesArray[type] == NULL) break;
        if (EvaluationData(theEnv,execStatus)->PrimitivesArray[type]->bitMap) IncrementBitMapCount(vPtr);
        else if (EvaluationData(theEnv,execStatus)->PrimitivesArray[type]->incrementBusyCount)
          { (*EvaluationData(theEnv,execStatus)->PrimitivesArray[type]->incrementBusyCount)(theEnv,execStatus,vPtr); }
        break;
     }
  }

/*******************************************/
/* AtomDeinstall: Decrements the reference */
/*   count of an atomic data type.         */
/*******************************************/
globle void AtomDeinstall(
  void *theEnv,
  EXEC_STATUS,
  int type,
  void *vPtr)
  {
   switch (type)
     {
      case SYMBOL:
      case STRING:
#if DEFGLOBAL_CONSTRUCT
      case GBL_VARIABLE:
#endif
#if OBJECT_SYSTEM
      case INSTANCE_NAME:
#endif
        DecrementSymbolCount(theEnv,execStatus,(SYMBOL_HN *) vPtr);
        break;

      case FLOAT:
        DecrementFloatCount(theEnv,execStatus,(FLOAT_HN *) vPtr);
        break;

      case INTEGER:
        DecrementIntegerCount(theEnv,execStatus,(INTEGER_HN *) vPtr);
        break;

      case EXTERNAL_ADDRESS:
        DecrementExternalAddressCount(theEnv,execStatus,(EXTERNAL_ADDRESS_HN *) vPtr);
        break;

      case MULTIFIELD:
        MultifieldDeinstall(theEnv,execStatus,(struct multifield *) vPtr);
        break;

      case RVOID:
        break;

      default:
        if (EvaluationData(theEnv,execStatus)->PrimitivesArray[type] == NULL) break;
        if (EvaluationData(theEnv,execStatus)->PrimitivesArray[type]->bitMap) DecrementBitMapCount(theEnv,execStatus,(BITMAP_HN *) vPtr);
        else if (EvaluationData(theEnv,execStatus)->PrimitivesArray[type]->decrementBusyCount)
          { (*EvaluationData(theEnv,execStatus)->PrimitivesArray[type]->decrementBusyCount)(theEnv,execStatus,vPtr); }
     }
  }

/*********************************************************************/
/* PropagateReturnValue: Decrements the associated depth for a value */
/*   stored in a DATA_OBJECT structure. In effect, the values        */
/*   returned by certain evaluations (such as a deffunction call)    */
/*   are passed up to the previous depth of evaluation. The return   */
/*   value's depth is decremented so that it will not be garbage     */
/*   collected along with other items that are no longer needed from */
/*   the evaluation that generated the return value.                 */
/*********************************************************************/
globle void PropagateReturnValue(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *vPtr)
  {
   long i;
   struct multifield *theSegment;
   struct field *theMultifield;

   if (vPtr->type != MULTIFIELD)
     { PropagateReturnAtom(theEnv,execStatus,vPtr->type,vPtr->value); }
   else
     {
      theSegment = (struct multifield *) vPtr->value;

      if (theSegment->depth > execStatus->CurrentEvaluationDepth)
        theSegment->depth = (short) execStatus->CurrentEvaluationDepth;

      theMultifield = theSegment->theFields;

      for (i = 0; i < theSegment->multifieldLength; i++)
        { PropagateReturnAtom(theEnv,execStatus,theMultifield[i].type,theMultifield[i].value); }
     }
  }

/*****************************************/
/* PropagateReturnAtom: Support function */
/*   for PropagateReturnValue.           */
/*****************************************/
static void PropagateReturnAtom(
  void *theEnv,
  EXEC_STATUS,
  int type,
  void *value)
  {
   switch (type)
     {
      case INTEGER         :
      case FLOAT           :
      case SYMBOL          :
      case STRING          :
      case EXTERNAL_ADDRESS:
#if OBJECT_SYSTEM
      case INSTANCE_NAME   :
#endif
        if (((SYMBOL_HN *) value)->depth > execStatus->CurrentEvaluationDepth)
          { ((SYMBOL_HN *) value)->depth = execStatus->CurrentEvaluationDepth; }
        break;

#if OBJECT_SYSTEM
      case INSTANCE_ADDRESS :
        if (((INSTANCE_TYPE *) value)->depth > execStatus->CurrentEvaluationDepth)
          { ((INSTANCE_TYPE *) value)->depth = execStatus->CurrentEvaluationDepth; }
        break;
#endif
      case FACT_ADDRESS :
        if (((int) ((struct fact *) value)->depth) > execStatus->CurrentEvaluationDepth)
          { ((struct fact *) value)->depth = (unsigned) execStatus->CurrentEvaluationDepth; }
        break;
     }
  }

#if DEFFUNCTION_CONSTRUCT || DEFGENERIC_CONSTRUCT

/********************************************/
/* EnvFunctionCall: Allows Deffunctions and */
/*   Generic Functions to be called from C. */
/*   Allows only constants as arguments.    */
/********************************************/
globle int EnvFunctionCall(
  void *theEnv,
  EXEC_STATUS,
  char *name,
  char *args,
  DATA_OBJECT *result)
  {
   FUNCTION_REFERENCE theReference;

   /*=======================================*/
   /* Call the function if it can be found. */
   /*=======================================*/

   if (GetFunctionReference(theEnv,execStatus,name,&theReference))
     { return(FunctionCall2(theEnv,execStatus,&theReference,args,result)); }

   /*=========================================================*/
   /* Otherwise signal an error if a deffunction, defgeneric, */
   /* or user defined function doesn't exist that matches     */
   /* the specified function name.                            */
   /*=========================================================*/

   PrintErrorID(theEnv,execStatus,"EVALUATN",2,FALSE);
   EnvPrintRouter(theEnv,execStatus,WERROR,"No function, generic function or deffunction of name ");
   EnvPrintRouter(theEnv,execStatus,WERROR,name);
   EnvPrintRouter(theEnv,execStatus,WERROR," exists for external call.\n");
   return(TRUE);
  }

/********************************************/
/* FunctionCall2: Allows Deffunctions and    */
/*   Generic Functions to be called from C. */
/*   Allows only constants as arguments.    */
/********************************************/
globle int FunctionCall2(
  void *theEnv,
  EXEC_STATUS,
  FUNCTION_REFERENCE *theReference,
  char *args,
  DATA_OBJECT *result)
  {
   EXPRESSION *argexps;
   int error = FALSE;

   /*=============================================*/
   /* Force periodic cleanup if the function call */
   /* was executed from an embedded application.  */
   /*=============================================*/

   if ((execStatus->CurrentEvaluationDepth == 0) && (! CommandLineData(theEnv,execStatus)->EvaluatingTopLevelCommand) &&
       (execStatus->CurrentExpression == NULL))
     { PeriodicCleanup(theEnv,execStatus,TRUE,FALSE); }

   /*========================*/
   /* Reset the error state. */
   /*========================*/

   if (execStatus->CurrentEvaluationDepth == 0) SetHaltExecution(theEnv,execStatus,FALSE);
   execStatus->EvaluationError = FALSE;

   /*======================================*/
   /* Initialize the default return value. */
   /*======================================*/

   result->type = SYMBOL;
   result->value = EnvFalseSymbol(theEnv,execStatus);

   /*============================*/
   /* Parse the argument string. */
   /*============================*/

   argexps = ParseConstantArguments(theEnv,execStatus,args,&error);
   if (error == TRUE) return(TRUE);

   /*====================*/
   /* Call the function. */
   /*====================*/

   theReference->argList = argexps;
   error = EvaluateExpression(theEnv,execStatus,theReference,result);

   /*========================*/
   /* Return the expression. */
   /*========================*/

   ReturnExpression(theEnv,execStatus,argexps);
   theReference->argList = NULL;

   /*==========================*/
   /* Return the error status. */
   /*==========================*/

   return(error);
  }

#endif

/***************************************************/
/* CopyDataObject: Copies the values from a source */
/*   DATA_OBJECT to a destination DATA_OBJECT.     */
/***************************************************/
globle void CopyDataObject(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *dst,
  DATA_OBJECT *src,
  int garbageMultifield)
  {
   if (src->type != MULTIFIELD)
     {
      dst->type = src->type;
      dst->value = src->value;
     }
   else
     {
      DuplicateMultifield(theEnv,execStatus,dst,src);
      if (garbageMultifield)
        { AddToMultifieldList(theEnv,execStatus,(struct multifield *) dst->value); }
     }
  }

/***********************************************/
/* TransferDataObjectValues: Copies the values */
/*   directly from a source DATA_OBJECT to a   */
/*   destination DATA_OBJECT.                  */
/***********************************************/
globle void TransferDataObjectValues(
  DATA_OBJECT *dst,
  DATA_OBJECT *src)
  {
   dst->type = src->type;
   dst->value = src->value;
   dst->begin = src->begin;
   dst->end = src->end;
   dst->supplementalInfo = src->supplementalInfo;
   dst->next = src->next;
  }

/************************************************************************/
/* ConvertValueToExpression: Converts the value stored in a data object */
/*   into an expression. For multifield values, a chain of expressions  */
/*   is generated and the chain is linked by the nextArg field. For a   */
/*   single field value, a single expression is created.                */
/************************************************************************/
globle struct expr *ConvertValueToExpression(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *theValue)
  {
   long i;
   struct expr *head = NULL, *last = NULL, *newItem;

   if (GetpType(theValue) != MULTIFIELD)
     { return(GenConstant(theEnv,execStatus,GetpType(theValue),GetpValue(theValue))); }

   for (i = GetpDOBegin(theValue); i <= GetpDOEnd(theValue); i++)
     {
      newItem = GenConstant(theEnv,execStatus,GetMFType(GetpValue(theValue),i),
                        GetMFValue(GetpValue(theValue),i));
      if (last == NULL) head = newItem;
      else last->nextArg = newItem;
      last = newItem;
     }

   if (head == NULL)
     return(GenConstant(theEnv,execStatus,FCALL,(void *) FindFunction(theEnv,execStatus,"create$")));

   return(head);
  }

/****************************************/
/* GetAtomicHashValue: Returns the hash */
/*   value for an atomic data type.     */
/****************************************/
unsigned long GetAtomicHashValue(
  unsigned short type,
  void *value,
  int position)
  {
   unsigned long tvalue;
   union
     {
      double fv;
      void *vv;
      unsigned long liv;
     } fis;

   switch (type)
     {
      case FLOAT:
        fis.liv = 0;
        fis.fv = ValueToDouble(value);
        tvalue = fis.liv;
        break;

      case INTEGER:
        tvalue = (unsigned long) ValueToLong(value);
        break;

      case EXTERNAL_ADDRESS:
         fis.liv = 0;
         fis.vv = ValueToExternalAddress(value);
         tvalue = (unsigned long) fis.liv;
         break;

      case FACT_ADDRESS:
#if OBJECT_SYSTEM
      case INSTANCE_ADDRESS:
#endif
         fis.liv = 0;
         fis.vv = value;
         tvalue = (unsigned long) fis.liv;
         break;
         
      case STRING:
#if OBJECT_SYSTEM
      case INSTANCE_NAME:
#endif
      case SYMBOL:
        tvalue = ((SYMBOL_HN *) value)->bucket;
        break;

      default:
        tvalue = type;
     }

   if (position < 0) return(tvalue);

   return((unsigned long) (tvalue * (((unsigned long) position) + 29)));
  }

/***********************************************************/
/* FunctionReferenceExpression: Returns an expression with */
/*   an appropriate expression reference to the specified  */
/*   name if it is the name of a deffunction, defgeneric,  */
/*   or user/system defined function.                      */
/***********************************************************/
globle struct expr *FunctionReferenceExpression(
  void *theEnv,
  EXEC_STATUS,
  char *name)
  {
#if DEFGENERIC_CONSTRUCT
   void *gfunc;
#endif
#if DEFFUNCTION_CONSTRUCT
   void *dptr;
#endif
   struct FunctionDefinition *fptr;

   /*=====================================================*/
   /* Check to see if the function call is a deffunction. */
   /*=====================================================*/

#if DEFFUNCTION_CONSTRUCT
   if ((dptr = (void *) LookupDeffunctionInScope(theEnv,execStatus,name)) != NULL)
     { return(GenConstant(theEnv,execStatus,PCALL,dptr)); }
#endif

   /*====================================================*/
   /* Check to see if the function call is a defgeneric. */
   /*====================================================*/

#if DEFGENERIC_CONSTRUCT
   if ((gfunc = (void *) LookupDefgenericInScope(theEnv,execStatus,name)) != NULL)
     { return(GenConstant(theEnv,execStatus,GCALL,gfunc)); }
#endif

   /*======================================*/
   /* Check to see if the function call is */
   /* a system or user defined function.   */
   /*======================================*/

   if ((fptr = FindFunction(theEnv,execStatus,name)) != NULL)
     { return(GenConstant(theEnv,execStatus,FCALL,fptr)); }

   /*===================================================*/
   /* The specified function name is not a deffunction, */
   /* defgeneric, or user/system defined function.      */
   /*===================================================*/

   return(NULL);
  }

/******************************************************************/
/* GetFunctionReference: Fills an expression with an appropriate  */
/*   expression reference to the specified name if it is the      */
/*   name of a deffunction, defgeneric, or user/system defined    */
/*   function.                                                    */
/******************************************************************/
globle intBool GetFunctionReference(
  void *theEnv,
  EXEC_STATUS,
  char *name,
  FUNCTION_REFERENCE *theReference)
  {
#if DEFGENERIC_CONSTRUCT
   void *gfunc;
#endif
#if DEFFUNCTION_CONSTRUCT
   void *dptr;
#endif
   struct FunctionDefinition *fptr;

   theReference->nextArg = NULL;
   theReference->argList = NULL;
   theReference->type = RVOID;
   theReference->value = NULL;

   /*=====================================================*/
   /* Check to see if the function call is a deffunction. */
   /*=====================================================*/

#if DEFFUNCTION_CONSTRUCT
   if ((dptr = (void *) LookupDeffunctionInScope(theEnv,execStatus,name)) != NULL)
     {
      theReference->type = PCALL;
      theReference->value = dptr;
      return(TRUE);
     }
#endif

   /*====================================================*/
   /* Check to see if the function call is a defgeneric. */
   /*====================================================*/

#if DEFGENERIC_CONSTRUCT
   if ((gfunc = (void *) LookupDefgenericInScope(theEnv,execStatus,name)) != NULL)
     {
      theReference->type = GCALL;
      theReference->value = gfunc;
      return(TRUE);
     }
#endif

   /*======================================*/
   /* Check to see if the function call is */
   /* a system or user defined function.   */
   /*======================================*/

   if ((fptr = FindFunction(theEnv,execStatus,name)) != NULL)
     {
      theReference->type = FCALL;
      theReference->value = fptr;
      return(TRUE);
     }

   /*===================================================*/
   /* The specified function name is not a deffunction, */
   /* defgeneric, or user/system defined function.      */
   /*===================================================*/

   return(FALSE);
  }

/*******************************************************/
/* DOsEqual: Determines if two DATA_OBJECTS are equal. */
/*******************************************************/
globle intBool DOsEqual(
  DATA_OBJECT_PTR dobj1,
  DATA_OBJECT_PTR dobj2)
  {
   if (GetpType(dobj1) != GetpType(dobj2))
     { return(FALSE); }

   if (GetpType(dobj1) == MULTIFIELD)
     {
      if (MultifieldDOsEqual(dobj1,dobj2) == FALSE)
        { return(FALSE); }
     }
   else if (GetpValue(dobj1) != GetpValue(dobj2))
     { return(FALSE); }

   return(TRUE);
  }

/***********************************************************
  NAME         : EvaluateAndStoreInDataObject
  DESCRIPTION  : Evaluates slot-value expressions
                   and stores the result in a
                   Kernel data object
  INPUTS       : 1) Flag indicating if multifields are OK
                 2) The value-expression
                 3) The data object structure
                 4) Flag indicating if a multifield value
                    should be placed on the garbage list.
  RETURNS      : FALSE on errors, TRUE otherwise
  SIDE EFFECTS : Segment allocated for storing
                 multifield values
  NOTES        : None
 ***********************************************************/
globle int EvaluateAndStoreInDataObject(
  void *theEnv,
  EXEC_STATUS,
  int mfp,
  EXPRESSION *theExp,
  DATA_OBJECT *val,
  int garbageSegment)
  {
   val->type = MULTIFIELD;
   val->begin = 0;
   val->end = -1;
   
   if (theExp == NULL)
     {
      if (garbageSegment) val->value = EnvCreateMultifield(theEnv,execStatus,0L);
      else val->value = CreateMultifield2(theEnv,execStatus,0L);

      return(TRUE);
     }

   if ((mfp == 0) && (theExp->nextArg == NULL))
     EvaluateExpression(theEnv,execStatus,theExp,val);
   else
     StoreInMultifield(theEnv,execStatus,val,theExp,garbageSegment);
   
   return(execStatus->EvaluationError ? FALSE : TRUE);
  }

/*******************************************************/
/* PrintCAddress:  */
/*******************************************************/
static void PrintCAddress(
  void *theEnv,
  EXEC_STATUS,
  char *logicalName,
  void *theValue)
  {
   char buffer[20];

   EnvPrintRouter(theEnv,execStatus,logicalName,"<Pointer-C-");
        
   gensprintf(buffer,"%p",ValueToExternalAddress(theValue));
   EnvPrintRouter(theEnv,execStatus,logicalName,buffer);
   EnvPrintRouter(theEnv,execStatus,logicalName,">");
  }

/*******************************************************/
/* NewCAddress:  */
/*******************************************************/
static void NewCAddress(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *rv)
  {
   int numberOfArguments;

   numberOfArguments = EnvRtnArgCount(theEnv,execStatus);
      
   if (numberOfArguments != 1)
     {
      PrintErrorID(theEnv,execStatus,"NEW",1,FALSE);
      EnvPrintRouter(theEnv,execStatus,WERROR,"Function new expected no additional arguments for the C external language type.\n");
      SetEvaluationError(theEnv,execStatus,TRUE);
      return;
     }

   SetpType(rv,EXTERNAL_ADDRESS);
   SetpValue(rv,EnvAddExternalAddress(theEnv,execStatus,NULL,0));
  }

/*******************************************************/
/* DiscardCAddress: TBD Remove */
/*******************************************************/
/*
static intBool DiscardCAddress(
  void *theEnv,
  EXEC_STATUS,
  void *theValue)
  {
   EnvPrintRouter(theEnv,execStatus,WDISPLAY,"Discarding C Address\n");
   
   return TRUE;
  }
*/


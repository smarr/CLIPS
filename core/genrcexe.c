   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  05/17/06            */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Generic Function Execution Routines              */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: Removed IMPERATIVE_METHODS compilation flag.   */
/*                                                           */
/*************************************************************/

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "setup.h"

#if DEFGENERIC_CONSTRUCT

#if OBJECT_SYSTEM
#include "classcom.h"
#include "classfun.h"
#include "insfun.h"
#endif

#include "argacces.h"
#include "constrct.h"
#include "envrnmnt.h"
#include "genrccom.h"
#include "prcdrfun.h"
#include "prccode.h"
#include "proflfun.h"
#include "router.h"
#include "utility.h"

#define _GENRCEXE_SOURCE_
#include "genrcexe.h"

/* =========================================
   *****************************************
                   CONSTANTS
   =========================================
   ***************************************** */

#define BEGIN_TRACE     ">>"
#define END_TRACE       "<<"

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

static DEFMETHOD *FindApplicableMethod(void *,EXEC_STATUS,DEFGENERIC *,DEFMETHOD *);

#if DEBUGGING_FUNCTIONS
static void WatchGeneric(void *,EXEC_STATUS,char *);
static void WatchMethod(void *,EXEC_STATUS,char *);
#endif

#if OBJECT_SYSTEM
static DEFCLASS *DetermineRestrictionClass(void *,EXEC_STATUS,DATA_OBJECT *);
#endif

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***********************************************************************************
  NAME         : GenericDispatch
  DESCRIPTION  : Executes the most specific applicable method
  INPUTS       : 1) The generic function
                 2) The method to start after in the search for an applicable
                    method (ignored if arg #3 is not NULL).
                 3) A specific method to call (NULL if want highest precedence
                    method to be called)
                 4) The generic function argument expressions
                 5) The caller's result value buffer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Any side-effects of evaluating the generic function arguments
                 Any side-effects of evaluating query functions on method parameter
                   restrictions when determining the core (see warning #1)
                 Any side-effects of actual execution of methods (see warning #2)
                 Caller's buffer set to the result of the generic function call

                 In case of errors, the result is FALSE, otherwise it is the
                   result returned by the most specific method (which can choose
                   to ignore or return the values of more general methods)
  NOTES        : WARNING #1: Query functions on method parameter restrictions
                    should not have side-effects, for they might be evaluated even
                    for methods that aren't applicable to the generic function call.
                 WARNING #2: Side-effects of method execution should not always rely
                    on only being executed once per generic function call.  Every
                    time a method calls (shadow-call) the same next-most-specific
                    method is executed.  Thus, it is possible for a method to be
                    executed multiple times per generic function call.
 ***********************************************************************************/
globle void GenericDispatch(
  void *theEnv,
  EXEC_STATUS,
  DEFGENERIC *gfunc,
  DEFMETHOD *prevmeth,
  DEFMETHOD *meth,
  EXPRESSION *params,
  DATA_OBJECT *result)
  {
   DEFGENERIC *previousGeneric;
   DEFMETHOD *previousMethod;
   int oldce;
#if PROFILING_FUNCTIONS
   struct profileFrameInfo profileFrame;
#endif

   result->type = SYMBOL;
   result->value = EnvFalseSymbol(theEnv,execStatus);
   execStatus->EvaluationError = FALSE;
   if (execStatus->HaltExecution)
     return;
   oldce = ExecutingConstruct(theEnv,execStatus);
   SetExecutingConstruct(theEnv,execStatus,TRUE);
   previousGeneric = DefgenericData(theEnv,execStatus)->CurrentGeneric;
   previousMethod = DefgenericData(theEnv,execStatus)->CurrentMethod;
   DefgenericData(theEnv,execStatus)->CurrentGeneric = gfunc;
   execStatus->CurrentEvaluationDepth++;
   gfunc->busy++;
   PushProcParameters(theEnv,execStatus,params,CountArguments(params),
                      EnvGetDefgenericName(theEnv,execStatus,(void *) gfunc),
                      "generic function",UnboundMethodErr);
   if (execStatus->EvaluationError)
     {
      gfunc->busy--;
      DefgenericData(theEnv,execStatus)->CurrentGeneric = previousGeneric;
      DefgenericData(theEnv,execStatus)->CurrentMethod = previousMethod;
      execStatus->CurrentEvaluationDepth--;
      PeriodicCleanup(theEnv,execStatus,FALSE,TRUE);
      SetExecutingConstruct(theEnv,execStatus,oldce);
      return;
     }
   if (meth != NULL)
     {
      if (IsMethodApplicable(theEnv,execStatus,meth))
        {
         meth->busy++;
         DefgenericData(theEnv,execStatus)->CurrentMethod = meth;
        }
      else
        {
         PrintErrorID(theEnv,execStatus,"GENRCEXE",4,FALSE);
         SetEvaluationError(theEnv,execStatus,TRUE);
         DefgenericData(theEnv,execStatus)->CurrentMethod = NULL;
         EnvPrintRouter(theEnv,execStatus,WERROR,"Generic function ");
         EnvPrintRouter(theEnv,execStatus,WERROR,EnvGetDefgenericName(theEnv,execStatus,(void *) gfunc));
         EnvPrintRouter(theEnv,execStatus,WERROR," method #");
         PrintLongInteger(theEnv,execStatus,WERROR,(long long) meth->index);
         EnvPrintRouter(theEnv,execStatus,WERROR," is not applicable to the given arguments.\n");
        }
     }
   else
     DefgenericData(theEnv,execStatus)->CurrentMethod = FindApplicableMethod(theEnv,execStatus,gfunc,prevmeth);
   if (DefgenericData(theEnv,execStatus)->CurrentMethod != NULL)
     {
#if DEBUGGING_FUNCTIONS
      if (DefgenericData(theEnv,execStatus)->CurrentGeneric->trace)
        WatchGeneric(theEnv,execStatus,BEGIN_TRACE);
      if (DefgenericData(theEnv,execStatus)->CurrentMethod->trace)
        WatchMethod(theEnv,execStatus,BEGIN_TRACE);
#endif
      if (DefgenericData(theEnv,execStatus)->CurrentMethod->system)
        {
         EXPRESSION fcall;

         fcall.type = FCALL;
         fcall.value = DefgenericData(theEnv,execStatus)->CurrentMethod->actions->value;
         fcall.nextArg = NULL;
         fcall.argList = GetProcParamExpressions(theEnv,execStatus);
         EvaluateExpression(theEnv,execStatus,&fcall,result);
        }
      else
        {
#if PROFILING_FUNCTIONS
         StartProfile(theEnv,execStatus,&profileFrame,
                      &DefgenericData(theEnv,execStatus)->CurrentMethod->usrData,
                      ProfileFunctionData(theEnv,execStatus)->ProfileConstructs);
#endif

         EvaluateProcActions(theEnv,execStatus,DefgenericData(theEnv,execStatus)->CurrentGeneric->header.whichModule->theModule,
                             DefgenericData(theEnv,execStatus)->CurrentMethod->actions,DefgenericData(theEnv,execStatus)->CurrentMethod->localVarCount,
                             result,UnboundMethodErr);

#if PROFILING_FUNCTIONS
         EndProfile(theEnv,execStatus,&profileFrame);
#endif
        }
      DefgenericData(theEnv,execStatus)->CurrentMethod->busy--;
#if DEBUGGING_FUNCTIONS
      if (DefgenericData(theEnv,execStatus)->CurrentMethod->trace)
        WatchMethod(theEnv,execStatus,END_TRACE);
      if (DefgenericData(theEnv,execStatus)->CurrentGeneric->trace)
        WatchGeneric(theEnv,execStatus,END_TRACE);
#endif
     }
   else if (! execStatus->EvaluationError)
     {
      PrintErrorID(theEnv,execStatus,"GENRCEXE",1,FALSE);
      EnvPrintRouter(theEnv,execStatus,WERROR,"No applicable methods for ");
      EnvPrintRouter(theEnv,execStatus,WERROR,EnvGetDefgenericName(theEnv,execStatus,(void *) gfunc));
      EnvPrintRouter(theEnv,execStatus,WERROR,".\n");
      SetEvaluationError(theEnv,execStatus,TRUE);
     }
   gfunc->busy--;
   ProcedureFunctionData(theEnv,execStatus)->ReturnFlag = FALSE;
   PopProcParameters(theEnv,execStatus);
   DefgenericData(theEnv,execStatus)->CurrentGeneric = previousGeneric;
   DefgenericData(theEnv,execStatus)->CurrentMethod = previousMethod;
   execStatus->CurrentEvaluationDepth--;
   PropagateReturnValue(theEnv,execStatus,result);
   PeriodicCleanup(theEnv,execStatus,FALSE,TRUE);
   SetExecutingConstruct(theEnv,execStatus,oldce);
  }

/*******************************************************
  NAME         : UnboundMethodErr
  DESCRIPTION  : Print out a synopis of the currently
                   executing method for unbound variable
                   errors
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Error synopsis printed to WERROR
  NOTES        : None
 *******************************************************/
globle void UnboundMethodErr(
  void *theEnv,
  EXEC_STATUS)
  {
   EnvPrintRouter(theEnv,execStatus,WERROR,"generic function ");
   EnvPrintRouter(theEnv,execStatus,WERROR,EnvGetDefgenericName(theEnv,execStatus,(void *) DefgenericData(theEnv,execStatus)->CurrentGeneric));
   EnvPrintRouter(theEnv,execStatus,WERROR," method #");
   PrintLongInteger(theEnv,execStatus,WERROR,(long long) DefgenericData(theEnv,execStatus)->CurrentMethod->index);
   EnvPrintRouter(theEnv,execStatus,WERROR,".\n");
  }

/***********************************************************************
  NAME         : IsMethodApplicable
  DESCRIPTION  : Tests to see if a method satsifies the arguments of a
                   generic function
                 A method is applicable if all its restrictions are
                   satisfied by the corresponding arguments
  INPUTS       : The method address
  RETURNS      : TRUE if method is applicable, FALSE otherwise
  SIDE EFFECTS : Any query functions are evaluated
  NOTES        : Uses globals ProcParamArraySize and ProcParamArray
 ***********************************************************************/
globle intBool IsMethodApplicable(
  void *theEnv,
  EXEC_STATUS,
  DEFMETHOD *meth)
  {
   DATA_OBJECT temp;
   short i,j,k;
   register RESTRICTION *rp;
#if OBJECT_SYSTEM
   void *type;
#else
   int type;
#endif

   if ((ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArraySize < meth->minRestrictions) ||
       ((ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArraySize > meth->minRestrictions) && (meth->maxRestrictions != -1)))
     return(FALSE);
   for (i = 0 , k = 0 ; i < ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArraySize ; i++)
     {
      rp = &meth->restrictions[k];
      if (rp->tcnt != 0)
        {
#if OBJECT_SYSTEM
         type = (void *) DetermineRestrictionClass(theEnv,execStatus,&ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray[i]);
         if (type == NULL)
           return(FALSE);
         for (j = 0 ; j < rp->tcnt ; j++)
           {
            if (type == rp->types[j])
              break;
            if (HasSuperclass((DEFCLASS *) type,(DEFCLASS *) rp->types[j]))
              break;
            if (rp->types[j] == (void *) DefclassData(theEnv,execStatus)->PrimitiveClassMap[INSTANCE_ADDRESS])
              {
               if (ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray[i].type == INSTANCE_ADDRESS)
                 break;
              }
            else if (rp->types[j] == (void *) DefclassData(theEnv,execStatus)->PrimitiveClassMap[INSTANCE_NAME])
              {
               if (ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray[i].type == INSTANCE_NAME)
                 break;
              }
            else if (rp->types[j] ==
                (void *) DefclassData(theEnv,execStatus)->PrimitiveClassMap[INSTANCE_NAME]->directSuperclasses.classArray[0])
              {
               if ((ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray[i].type == INSTANCE_NAME) ||
                   (ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray[i].type == INSTANCE_ADDRESS))
                 break;
              }
           }
#else
         type = ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray[i].type;
         for (j = 0 ; j < rp->tcnt ; j++)
           {
            if (type == ValueToInteger(rp->types[j]))
              break;
            if (SubsumeType(type,ValueToInteger(rp->types[j])))
              break;
           }
#endif
         if (j == rp->tcnt)
           return(FALSE);
        }
      if (rp->query != NULL)
        {
         DefgenericData(theEnv,execStatus)->GenericCurrentArgument = &ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray[i];
         EvaluateExpression(theEnv,execStatus,rp->query,&temp);
         if ((temp.type != SYMBOL) ? FALSE :
             (temp.value == EnvFalseSymbol(theEnv,execStatus)))
           return(FALSE);
        }
      if (((int) k) != meth->restrictionCount-1)
        k++;
     }
   return(TRUE);
  }

/***************************************************
  NAME         : NextMethodP
  DESCRIPTION  : Determines if a shadowed generic
                   function method is available for
                   execution
  INPUTS       : None
  RETURNS      : TRUE if there is a method available,
                   FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : H/L Syntax: (next-methodp)
 ***************************************************/
globle int NextMethodP(
  void *theEnv,
  EXEC_STATUS)
  {
   register DEFMETHOD *meth;

   if (DefgenericData(theEnv,execStatus)->CurrentMethod == NULL)
     return(FALSE);
   meth = FindApplicableMethod(theEnv,execStatus,DefgenericData(theEnv,execStatus)->CurrentGeneric,DefgenericData(theEnv,execStatus)->CurrentMethod);
   if (meth != NULL)
     {
      meth->busy--;
      return(TRUE);
     }
   return(FALSE);
  }

/****************************************************
  NAME         : CallNextMethod
  DESCRIPTION  : Executes the next available method
                   in the core for a generic function
  INPUTS       : Caller's buffer for the result
  RETURNS      : Nothing useful
  SIDE EFFECTS : Side effects of execution of shadow
                 EvaluationError set if no method
                   is available to execute.
  NOTES        : H/L Syntax: (call-next-method)
 ****************************************************/
globle void CallNextMethod(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *result)
  {
   DEFMETHOD *oldMethod;
#if PROFILING_FUNCTIONS
   struct profileFrameInfo profileFrame;
#endif

   result->type = SYMBOL;
   result->value = EnvFalseSymbol(theEnv,execStatus);
   if (execStatus->HaltExecution)
     return;
   oldMethod = DefgenericData(theEnv,execStatus)->CurrentMethod;
   if (DefgenericData(theEnv,execStatus)->CurrentMethod != NULL)
     DefgenericData(theEnv,execStatus)->CurrentMethod = FindApplicableMethod(theEnv,execStatus,DefgenericData(theEnv,execStatus)->CurrentGeneric,DefgenericData(theEnv,execStatus)->CurrentMethod);
   if (DefgenericData(theEnv,execStatus)->CurrentMethod == NULL)
     {
      DefgenericData(theEnv,execStatus)->CurrentMethod = oldMethod;
      PrintErrorID(theEnv,execStatus,"GENRCEXE",2,FALSE);
      EnvPrintRouter(theEnv,execStatus,WERROR,"Shadowed methods not applicable in current context.\n");
      SetEvaluationError(theEnv,execStatus,TRUE);
      return;
     }

#if DEBUGGING_FUNCTIONS
   if (DefgenericData(theEnv,execStatus)->CurrentMethod->trace)
     WatchMethod(theEnv,execStatus,BEGIN_TRACE);
#endif
   if (DefgenericData(theEnv,execStatus)->CurrentMethod->system)
     {
      EXPRESSION fcall;

      fcall.type = FCALL;
      fcall.value = DefgenericData(theEnv,execStatus)->CurrentMethod->actions->value;
      fcall.nextArg = NULL;
      fcall.argList = GetProcParamExpressions(theEnv,execStatus);
      EvaluateExpression(theEnv,execStatus,&fcall,result);
     }
   else
     {
#if PROFILING_FUNCTIONS
      StartProfile(theEnv,execStatus,&profileFrame,
                   &DefgenericData(theEnv,execStatus)->CurrentGeneric->header.usrData,
                   ProfileFunctionData(theEnv,execStatus)->ProfileConstructs);
#endif

      EvaluateProcActions(theEnv,execStatus,DefgenericData(theEnv,execStatus)->CurrentGeneric->header.whichModule->theModule,
                         DefgenericData(theEnv,execStatus)->CurrentMethod->actions,DefgenericData(theEnv,execStatus)->CurrentMethod->localVarCount,
                         result,UnboundMethodErr);

#if PROFILING_FUNCTIONS
      EndProfile(theEnv,execStatus,&profileFrame);
#endif
     }

   DefgenericData(theEnv,execStatus)->CurrentMethod->busy--;
#if DEBUGGING_FUNCTIONS
   if (DefgenericData(theEnv,execStatus)->CurrentMethod->trace)
     WatchMethod(theEnv,execStatus,END_TRACE);
#endif
   DefgenericData(theEnv,execStatus)->CurrentMethod = oldMethod;
   ProcedureFunctionData(theEnv,execStatus)->ReturnFlag = FALSE;
  }

/**************************************************************************
  NAME         : CallSpecificMethod
  DESCRIPTION  : Allows a specific method to be called without regards to
                   higher precedence methods which might also be applicable
                   However, shadowed methods can still be called.
  INPUTS       : A data object buffer to hold the method evaluation result
  RETURNS      : Nothing useful
  SIDE EFFECTS : Side-effects of method applicability tests and the
                 evaluation of methods
  NOTES        : H/L Syntax: (call-specific-method
                                <generic-function> <method-index> <args>)
 **************************************************************************/
globle void CallSpecificMethod(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *result)
  {
   DATA_OBJECT temp;
   DEFGENERIC *gfunc;
   int mi;

   result->type = SYMBOL;
   result->value = EnvFalseSymbol(theEnv,execStatus);
   if (EnvArgTypeCheck(theEnv,execStatus,"call-specific-method",1,SYMBOL,&temp) == FALSE)
     return;
   gfunc = CheckGenericExists(theEnv,execStatus,"call-specific-method",DOToString(temp));
   if (gfunc == NULL)
     return;
   if (EnvArgTypeCheck(theEnv,execStatus,"call-specific-method",2,INTEGER,&temp) == FALSE)
     return;
   mi = CheckMethodExists(theEnv,execStatus,"call-specific-method",gfunc,(long) DOToLong(temp));
   if (mi == -1)
     return;
   gfunc->methods[mi].busy++;
   GenericDispatch(theEnv,execStatus,gfunc,NULL,&gfunc->methods[mi],
                   GetFirstArgument()->nextArg->nextArg,result);
   gfunc->methods[mi].busy--;
  }

/***********************************************************************
  NAME         : OverrideNextMethod
  DESCRIPTION  : Changes the arguments to shadowed methods, thus the set
                 of applicable methods to this call may change
  INPUTS       : A buffer to hold the result of the call
  RETURNS      : Nothing useful
  SIDE EFFECTS : Any of evaluating method restrictions and bodies
  NOTES        : H/L Syntax: (override-next-method <args>)
 ***********************************************************************/
globle void OverrideNextMethod(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *result)
  {
   result->type = SYMBOL;
   result->value = EnvFalseSymbol(theEnv,execStatus);
   if (execStatus->HaltExecution)
     return;
   if (DefgenericData(theEnv,execStatus)->CurrentMethod == NULL)
     {
      PrintErrorID(theEnv,execStatus,"GENRCEXE",2,FALSE);
      EnvPrintRouter(theEnv,execStatus,WERROR,"Shadowed methods not applicable in current context.\n");
      SetEvaluationError(theEnv,execStatus,TRUE);
      return;
     }
   GenericDispatch(theEnv,execStatus,DefgenericData(theEnv,execStatus)->CurrentGeneric,DefgenericData(theEnv,execStatus)->CurrentMethod,NULL,
                   GetFirstArgument(),result);
  }

/***********************************************************
  NAME         : GetGenericCurrentArgument
  DESCRIPTION  : Returns the value of the generic function
                   argument being tested in the method
                   applicability determination process
  INPUTS       : A data-object buffer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Data-object set
  NOTES        : Useful for queries in wildcard restrictions
 ***********************************************************/
globle void GetGenericCurrentArgument(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *result)
  {
   result->type = DefgenericData(theEnv,execStatus)->GenericCurrentArgument->type;
   result->value = DefgenericData(theEnv,execStatus)->GenericCurrentArgument->value;
   result->begin = DefgenericData(theEnv,execStatus)->GenericCurrentArgument->begin;
   result->end = DefgenericData(theEnv,execStatus)->GenericCurrentArgument->end;
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/************************************************************
  NAME         : FindApplicableMethod
  DESCRIPTION  : Finds the first/next applicable
                   method for a generic function call
  INPUTS       : 1) The generic function pointer
                 2) The address of the current method
                    (NULL to find the first)
  RETURNS      : The address of the first/next
                   applicable method (NULL on errors)
  SIDE EFFECTS : Any from evaluating query restrictions
                 Methoid busy count incremented if applicable
  NOTES        : None
 ************************************************************/
static DEFMETHOD *FindApplicableMethod(
  void *theEnv,
  EXEC_STATUS,
  DEFGENERIC *gfunc,
  DEFMETHOD *meth)
  {
   if (meth != NULL)
     meth++;
   else
     meth = gfunc->methods;
   for ( ; meth < &gfunc->methods[gfunc->mcnt] ; meth++)
     {
      meth->busy++;
      if (IsMethodApplicable(theEnv,execStatus,meth))
        return(meth);
      meth->busy--;
     }
   return(NULL);
  }

#if DEBUGGING_FUNCTIONS

/**********************************************************************
  NAME         : WatchGeneric
  DESCRIPTION  : Prints out a trace of the beginning or end
                   of the execution of a generic function
  INPUTS       : A string to indicate beginning or end of execution
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : Uses the globals CurrentGeneric, ProcParamArraySize and
                   ProcParamArray for other trace info
 **********************************************************************/
static void WatchGeneric(
  void *theEnv,
  EXEC_STATUS,
  char *tstring)
  {
   EnvPrintRouter(theEnv,execStatus,WTRACE,"GNC ");
   EnvPrintRouter(theEnv,execStatus,WTRACE,tstring);
   EnvPrintRouter(theEnv,execStatus,WTRACE," ");
   if (DefgenericData(theEnv,execStatus)->CurrentGeneric->header.whichModule->theModule != ((struct defmodule *) EnvGetCurrentModule(theEnv,execStatus)))
     {
      EnvPrintRouter(theEnv,execStatus,WTRACE,EnvGetDefmoduleName(theEnv,execStatus,(void *)
                        DefgenericData(theEnv,execStatus)->CurrentGeneric->header.whichModule->theModule));
      EnvPrintRouter(theEnv,execStatus,WTRACE,"::");
     }
   EnvPrintRouter(theEnv,execStatus,WTRACE,ValueToString((void *) DefgenericData(theEnv,execStatus)->CurrentGeneric->header.name));
   EnvPrintRouter(theEnv,execStatus,WTRACE," ");
   EnvPrintRouter(theEnv,execStatus,WTRACE," ED:");
   PrintLongInteger(theEnv,execStatus,WTRACE,(long long) execStatus->CurrentEvaluationDepth);
   PrintProcParamArray(theEnv,execStatus,WTRACE);
  }

/**********************************************************************
  NAME         : WatchMethod
  DESCRIPTION  : Prints out a trace of the beginning or end
                   of the execution of a generic function
                   method
  INPUTS       : A string to indicate beginning or end of execution
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : Uses the globals CurrentGeneric, CurrentMethod,
                   ProcParamArraySize and ProcParamArray for
                   other trace info
 **********************************************************************/
static void WatchMethod(
  void *theEnv,
  EXEC_STATUS,
  char *tstring)
  {
   EnvPrintRouter(theEnv,execStatus,WTRACE,"MTH ");
   EnvPrintRouter(theEnv,execStatus,WTRACE,tstring);
   EnvPrintRouter(theEnv,execStatus,WTRACE," ");
   if (DefgenericData(theEnv,execStatus)->CurrentGeneric->header.whichModule->theModule != ((struct defmodule *) EnvGetCurrentModule(theEnv,execStatus)))
     {
      EnvPrintRouter(theEnv,execStatus,WTRACE,EnvGetDefmoduleName(theEnv,execStatus,(void *)
                        DefgenericData(theEnv,execStatus)->CurrentGeneric->header.whichModule->theModule));
      EnvPrintRouter(theEnv,execStatus,WTRACE,"::");
     }
   EnvPrintRouter(theEnv,execStatus,WTRACE,ValueToString((void *) DefgenericData(theEnv,execStatus)->CurrentGeneric->header.name));
   EnvPrintRouter(theEnv,execStatus,WTRACE,":#");
   if (DefgenericData(theEnv,execStatus)->CurrentMethod->system)
     EnvPrintRouter(theEnv,execStatus,WTRACE,"SYS");
   PrintLongInteger(theEnv,execStatus,WTRACE,(long long) DefgenericData(theEnv,execStatus)->CurrentMethod->index);
   EnvPrintRouter(theEnv,execStatus,WTRACE," ");
   EnvPrintRouter(theEnv,execStatus,WTRACE," ED:");
   PrintLongInteger(theEnv,execStatus,WTRACE,(long long) execStatus->CurrentEvaluationDepth);
   PrintProcParamArray(theEnv,execStatus,WTRACE);
  }

#endif

#if OBJECT_SYSTEM

/***************************************************
  NAME         : DetermineRestrictionClass
  DESCRIPTION  : Finds the class of an argument in
                   the ProcParamArray
  INPUTS       : The argument data object
  RETURNS      : The class address, NULL if error
  SIDE EFFECTS : EvaluationError set on errors
  NOTES        : None
 ***************************************************/
static DEFCLASS *DetermineRestrictionClass(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *dobj)
  {
   INSTANCE_TYPE *ins;
   DEFCLASS *cls;

   if (dobj->type == INSTANCE_NAME)
     {
      ins = FindInstanceBySymbol(theEnv,execStatus,(SYMBOL_HN *) dobj->value);
      cls = (ins != NULL) ? ins->cls : NULL;
     }
   else if (dobj->type == INSTANCE_ADDRESS)
     {
      ins = (INSTANCE_TYPE *) dobj->value;
      cls = (ins->garbage == 0) ? ins->cls : NULL;
     }
   else
     return(DefclassData(theEnv,execStatus)->PrimitiveClassMap[dobj->type]);
   if (cls == NULL)
     {
      SetEvaluationError(theEnv,execStatus,TRUE);
      PrintErrorID(theEnv,execStatus,"GENRCEXE",3,FALSE);
      EnvPrintRouter(theEnv,execStatus,WERROR,"Unable to determine class of ");
      PrintDataObject(theEnv,execStatus,WERROR,dobj);
      EnvPrintRouter(theEnv,execStatus,WERROR," in generic function ");
      EnvPrintRouter(theEnv,execStatus,WERROR,EnvGetDefgenericName(theEnv,execStatus,(void *) DefgenericData(theEnv,execStatus)->CurrentGeneric));
      EnvPrintRouter(theEnv,execStatus,WERROR,".\n");
     }
   return(cls);
  }

#endif

#endif


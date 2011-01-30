   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/09/97          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Generic Function Execution Routines              */
/*                                                           */
/* Principal Programmer(s):                                  */
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
               MACROS AND TYPES
   =========================================
   ***************************************** */

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

static DEFMETHOD *FindApplicableMethod(DEFGENERIC *,DEFMETHOD *);

#if DEBUGGING_FUNCTIONS
static void WatchGeneric(char *);
static void WatchMethod(char *);
#endif

#if OBJECT_SYSTEM
static DEFCLASS *DetermineRestrictionClass(DATA_OBJECT *);
#endif

/* =========================================
   *****************************************
      EXTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */

/* =========================================
   *****************************************
      INTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */

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
   result->value = FalseSymbol;
   EvaluationError = FALSE;
   if (HaltExecution)
     return;
   oldce = ExecutingConstruct();
   SetExecutingConstruct(TRUE);
   previousGeneric = CurrentGeneric;
   previousMethod = CurrentMethod;
   CurrentGeneric = gfunc;
   CurrentEvaluationDepth++;
   gfunc->busy++;
   PushProcParameters(params,CountArguments(params),
                       GetDefgenericName((void *) gfunc),
                      "generic function",UnboundMethodErr);
   if (EvaluationError)
     {
      gfunc->busy--;
      CurrentGeneric = previousGeneric;
      CurrentMethod = previousMethod;
      CurrentEvaluationDepth--;
      PeriodicCleanup(FALSE,TRUE);
      SetExecutingConstruct(oldce);
      return;
     }
   if (meth != NULL)
     {
      if (IsMethodApplicable(meth))
        {
         meth->busy++;
         CurrentMethod = meth;
        }
      else
        {
         PrintErrorID("GENRCEXE",4,FALSE);
         SetEvaluationError(TRUE);
         CurrentMethod = NULL;
         PrintRouter(WERROR,"Generic function ");
         PrintRouter(WERROR,GetDefgenericName((void *) gfunc));
         PrintRouter(WERROR," method #");
         PrintLongInteger(WERROR,(long) meth->index);
         PrintRouter(WERROR," is not applicable to the given arguments.\n");
        }
     }
   else
     CurrentMethod = FindApplicableMethod(gfunc,prevmeth);
   if (CurrentMethod != NULL)
     {
#if DEBUGGING_FUNCTIONS
      if (CurrentGeneric->trace)
        WatchGeneric(BEGIN_TRACE);
      if (CurrentMethod->trace)
        WatchMethod(BEGIN_TRACE);
#endif
      if (CurrentMethod->system)
        {
         EXPRESSION fcall;

         fcall.type = FCALL;
         fcall.value = CurrentMethod->actions->value;
         fcall.nextArg = NULL;
         fcall.argList = GetProcParamExpressions();
         EvaluateExpression(&fcall,result);
        }
      else
        {
#if PROFILING_FUNCTIONS
         StartProfile(&profileFrame,
                      &CurrentMethod->usrData,
                      ProfileConstructs);
#endif

         EvaluateProcActions(CurrentGeneric->header.whichModule->theModule,
                             CurrentMethod->actions,CurrentMethod->localVarCount,
                             result,UnboundMethodErr);

#if PROFILING_FUNCTIONS
         EndProfile(&profileFrame);
#endif
        }
      CurrentMethod->busy--;
#if DEBUGGING_FUNCTIONS
      if (CurrentMethod->trace)
        WatchMethod(END_TRACE);
      if (CurrentGeneric->trace)
        WatchGeneric(END_TRACE);
#endif
     }
   else if (! EvaluationError)
     {
      PrintErrorID("GENRCEXE",1,FALSE);
      PrintRouter(WERROR,"No applicable methods for ");
      PrintRouter(WERROR,GetDefgenericName((void *) gfunc));
      PrintRouter(WERROR,".\n");
      SetEvaluationError(TRUE);
     }
   gfunc->busy--;
   ReturnFlag = FALSE;
   PopProcParameters();
   CurrentGeneric = previousGeneric;
   CurrentMethod = previousMethod;
   CurrentEvaluationDepth--;
   PropagateReturnValue(result);
   PeriodicCleanup(FALSE,TRUE);
   SetExecutingConstruct(oldce);
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
globle void UnboundMethodErr()
  {
   PrintRouter(WERROR,"generic function ");
   PrintRouter(WERROR,GetDefgenericName((void *) CurrentGeneric));
   PrintRouter(WERROR," method #");
   PrintLongInteger(WERROR,(long) CurrentMethod->index);
   PrintRouter(WERROR,".\n");
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
globle BOOLEAN IsMethodApplicable(
  DEFMETHOD *meth)
  {
   DATA_OBJECT temp;
   register int i,j,k;
   register RESTRICTION *rp;
#if OBJECT_SYSTEM
   void *type;
#else
   int type;
#endif

   if ((ProcParamArraySize < meth->minRestrictions) ||
       ((ProcParamArraySize > meth->minRestrictions) && (meth->maxRestrictions != -1)))
     return(FALSE);
   for (i = 0 , k = 0 ; i < ProcParamArraySize ; i++)
     {
      rp = &meth->restrictions[k];
      if (rp->tcnt != 0)
        {
#if OBJECT_SYSTEM
         type = (void *) DetermineRestrictionClass(&ProcParamArray[i]);
         if (type == NULL)
           return(FALSE);
         for (j = 0 ; j < rp->tcnt ; j++)
           {
            if (type == rp->types[j])
              break;
            if (HasSuperclass((DEFCLASS *) type,(DEFCLASS *) rp->types[j]))
              break;
            if (rp->types[j] == (void *) PrimitiveClassMap[INSTANCE_ADDRESS])
              {
               if (ProcParamArray[i].type == INSTANCE_ADDRESS)
                 break;
              }
            else if (rp->types[j] == (void *) PrimitiveClassMap[INSTANCE_NAME])
              {
               if (ProcParamArray[i].type == INSTANCE_NAME)
                 break;
              }
            else if (rp->types[j] ==
                (void *) PrimitiveClassMap[INSTANCE_NAME]->directSuperclasses.classArray[0])
              {
               if ((ProcParamArray[i].type == INSTANCE_NAME) ||
                   (ProcParamArray[i].type == INSTANCE_ADDRESS))
                 break;
              }
           }
#else
         type = ProcParamArray[i].type;
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
         GenericCurrentArgument = &ProcParamArray[i];
         EvaluateExpression(rp->query,&temp);
         if ((temp.type != SYMBOL) ? FALSE :
             (temp.value == FalseSymbol))
           return(FALSE);
        }
      if (k != meth->restrictionCount-1)
        k++;
     }
   return(TRUE);
  }

#if IMPERATIVE_METHODS

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
globle int NextMethodP()
  {
   register DEFMETHOD *meth;

   if (CurrentMethod == NULL)
     return(FALSE);
   meth = FindApplicableMethod(CurrentGeneric,CurrentMethod);
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
  DATA_OBJECT *result)
  {
   DEFMETHOD *oldMethod;
#if PROFILING_FUNCTIONS
   struct profileFrameInfo profileFrame;
#endif

   result->type = SYMBOL;
   result->value = FalseSymbol;
   if (HaltExecution)
     return;
   oldMethod = CurrentMethod;
   if (CurrentMethod != NULL)
     CurrentMethod = FindApplicableMethod(CurrentGeneric,CurrentMethod);
   if (CurrentMethod == NULL)
     {
      CurrentMethod = oldMethod;
      PrintErrorID("GENRCEXE",2,FALSE);
      PrintRouter(WERROR,"Shadowed methods not applicable in current context.\n");
      SetEvaluationError(TRUE);
      return;
     }

#if DEBUGGING_FUNCTIONS
   if (CurrentMethod->trace)
     WatchMethod(BEGIN_TRACE);
#endif
   if (CurrentMethod->system)
     {
      EXPRESSION fcall;

      fcall.type = FCALL;
      fcall.value = CurrentMethod->actions->value;
      fcall.nextArg = NULL;
      fcall.argList = GetProcParamExpressions();
      EvaluateExpression(&fcall,result);
     }
   else
     {
#if PROFILING_FUNCTIONS
      StartProfile(&profileFrame,
                   &CurrentGeneric->header.usrData,
                   ProfileConstructs);
#endif

      EvaluateProcActions(CurrentGeneric->header.whichModule->theModule,
                         CurrentMethod->actions,CurrentMethod->localVarCount,
                         result,UnboundMethodErr);

#if PROFILING_FUNCTIONS
      EndProfile(&profileFrame);
#endif
     }

   CurrentMethod->busy--;
#if DEBUGGING_FUNCTIONS
   if (CurrentMethod->trace)
     WatchMethod(END_TRACE);
#endif
   CurrentMethod = oldMethod;
   ReturnFlag = FALSE;
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
  DATA_OBJECT *result)
  {
   DATA_OBJECT temp;
   DEFGENERIC *gfunc;
   int mi;

   result->type = SYMBOL;
   result->value = FalseSymbol;
   if (ArgTypeCheck("call-specific-method",1,SYMBOL,&temp) == FALSE)
     return;
   gfunc = CheckGenericExists("call-specific-method",DOToString(temp));
   if (gfunc == NULL)
     return;
   if (ArgTypeCheck("call-specific-method",2,INTEGER,&temp) == FALSE)
     return;
   mi = CheckMethodExists("call-specific-method",gfunc,DOToInteger(temp));
   if (mi == -1)
     return;
   gfunc->methods[mi].busy++;
   GenericDispatch(gfunc,NULL,&gfunc->methods[mi],
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
  DATA_OBJECT *result)
  {
   result->type = SYMBOL;
   result->value = FalseSymbol;
   if (HaltExecution)
     return;
   if (CurrentMethod == NULL)
     {
      PrintErrorID("GENRCEXE",2,FALSE);
      PrintRouter(WERROR,"Shadowed methods not applicable in current context.\n");
      SetEvaluationError(TRUE);
      return;
     }
   GenericDispatch(CurrentGeneric,CurrentMethod,NULL,
                   GetFirstArgument(),result);
  }

#endif /* IMPERATIVE_METHODS */

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
  DATA_OBJECT *result)
  {
   result->type = GenericCurrentArgument->type;
   result->value = GenericCurrentArgument->value;
   result->begin = GenericCurrentArgument->begin;
   result->end = GenericCurrentArgument->end;
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
      if (IsMethodApplicable(meth))
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
  char *tstring)
  {
   PrintRouter(WTRACE,"GNC ");
   PrintRouter(WTRACE,tstring);
   PrintRouter(WTRACE," ");
   if (CurrentGeneric->header.whichModule->theModule != ((struct defmodule *) GetCurrentModule()))
     {
      PrintRouter(WTRACE,GetDefmoduleName((void *)
                        CurrentGeneric->header.whichModule->theModule));
      PrintRouter(WTRACE,"::");
     }
   PrintRouter(WTRACE,ValueToString((void *) CurrentGeneric->header.name));
   PrintRouter(WTRACE," ");
   PrintRouter(WTRACE," ED:");
   PrintLongInteger(WTRACE,(long) CurrentEvaluationDepth);
   PrintProcParamArray(WTRACE);
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
  char *tstring)
  {
   PrintRouter(WTRACE,"MTH ");
   PrintRouter(WTRACE,tstring);
   PrintRouter(WTRACE," ");
   if (CurrentGeneric->header.whichModule->theModule != ((struct defmodule *) GetCurrentModule()))
     {
      PrintRouter(WTRACE,GetDefmoduleName((void *)
                        CurrentGeneric->header.whichModule->theModule));
      PrintRouter(WTRACE,"::");
     }
   PrintRouter(WTRACE,ValueToString((void *) CurrentGeneric->header.name));
   PrintRouter(WTRACE,":#");
   if (CurrentMethod->system)
     PrintRouter(WTRACE,"SYS");
   PrintLongInteger(WTRACE,(long) CurrentMethod->index);
   PrintRouter(WTRACE," ");
   PrintRouter(WTRACE," ED:");
   PrintLongInteger(WTRACE,(long) CurrentEvaluationDepth);
   PrintProcParamArray(WTRACE);
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
  DATA_OBJECT *dobj)
  {
   INSTANCE_TYPE *ins;
   DEFCLASS *cls;

   if (dobj->type == INSTANCE_NAME)
     {
      ins = FindInstanceBySymbol((SYMBOL_HN *) dobj->value);
      cls = (ins != NULL) ? ins->cls : NULL;
     }
   else if (dobj->type == INSTANCE_ADDRESS)
     {
      ins = (INSTANCE_TYPE *) dobj->value;
      cls = (ins->garbage == 0) ? ins->cls : NULL;
     }
   else
     return(PrimitiveClassMap[dobj->type]);
   if (cls == NULL)
     {
      SetEvaluationError(TRUE);
      PrintErrorID("GENRCEXE",3,FALSE);
      PrintRouter(WERROR,"Unable to determine class of ");
      PrintDataObject(WERROR,dobj);
      PrintRouter(WERROR," in generic function ");
      PrintRouter(WERROR,GetDefgenericName((void *) CurrentGeneric));
      PrintRouter(WERROR,".\n");
     }
   return(cls);
  }

#endif

#endif

/***************************************************
  NAME         :
  DESCRIPTION  :
  INPUTS       :
  RETURNS      :
  SIDE EFFECTS :
  NOTES        :
 ***************************************************/

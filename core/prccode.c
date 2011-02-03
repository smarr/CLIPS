   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.24  06/05/06          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/***************************************************************/
/* Purpose: Procedural Code Support Routines for Deffunctions, */
/*          Generic Function Methods,Message-Handlers          */
/*          and Rules                                          */
/*                                                             */
/* Principal Programmer(s):                                    */
/*      Brian L. Dantes                                        */
/*                                                             */
/* Contributing Programmer(s):                                 */
/*                                                             */
/* Revision History:                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859    */
/*                                                             */
/*            Changed name of variable log to logName          */
/*            because of Unix compiler warnings of shadowed    */
/*            definitions.                                     */
/*                                                             */
/*      6.24: Renamed BOOLEAN macro type to intBool.           */
/*                                                             */
/*            Added pragmas to remove compilation warnings.    */
/*                                                             */
/***************************************************************/

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "setup.h"

#ifndef _STDIO_INCLUDED_
#include <stdio.h>
#define _STDIO_INCLUDED_
#endif

#include <stdlib.h>
#include <ctype.h>

#include "memalloc.h"
#include "constant.h"
#include "envrnmnt.h"
#if DEFGLOBAL_CONSTRUCT
#include "globlpsr.h"
#endif
#include "exprnpsr.h"
#include "multifld.h"
#if OBJECT_SYSTEM
#include "object.h"
#endif
#include "prcdrpsr.h"
#include "router.h"
#include "utility.h"

#define _PRCCODE_SOURCE_
#include "prccode.h"

/* =========================================
   *****************************************
               MACROS AND TYPES
   =========================================
   ***************************************** */
typedef struct
  {
   unsigned firstFlag  : 1;
   unsigned first      : 15;
   unsigned secondFlag : 1;
   unsigned second     : 15;
  } PACKED_PROC_VAR;

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

static void EvaluateProcParameters(void *,EXEC_STATUS,EXPRESSION *,int,char *,char *);
static intBool RtnProcParam(void *,EXEC_STATUS,void *,DATA_OBJECT *);
static intBool GetProcBind(void *,EXEC_STATUS,void *,DATA_OBJECT *);
static intBool PutProcBind(void *,EXEC_STATUS,void *,DATA_OBJECT *);
static intBool RtnProcWild(void *,EXEC_STATUS,void *,DATA_OBJECT *);
static void DeallocateProceduralPrimitiveData(void *,EXEC_STATUS);
static void ReleaseProcParameters(void *,EXEC_STATUS);

#if (! BLOAD_ONLY) && (! RUN_TIME)
static int FindProcParameter(SYMBOL_HN *,EXPRESSION *,SYMBOL_HN *);
static int ReplaceProcBinds(void *,EXEC_STATUS,EXPRESSION *,
                             int (*)(void *,EXEC_STATUS,EXPRESSION *,void *),void *);
static EXPRESSION *CompactActions(void *,EXEC_STATUS,EXPRESSION *);
#endif

#if (! DEFFUNCTION_CONSTRUCT) || (! DEFGENERIC_CONSTRUCT)
static intBool EvaluateBadCall(void *,EXEC_STATUS,void *,DATA_OBJECT *);
#endif

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/****************************************************
  NAME         : InstallProcedurePrimitives
  DESCRIPTION  : Installs primitive function handlers
                 for accessing parameters and local
                 variables within the bodies of
                 message-handlers, methods, rules and
                 deffunctions.
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Primitive entities installed
  NOTES        : None
 ****************************************************/
globle void InstallProcedurePrimitives(
  void *theEnv,
  EXEC_STATUS)
  {
   ENTITY_RECORD procParameterInfo = { "PROC_PARAM", PROC_PARAM,0,1,0,NULL,NULL,NULL,
                                           RtnProcParam,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL },
                     procWildInfo =      { "PROC_WILD_PARAM", PROC_WILD_PARAM,0,1,0,NULL,NULL,NULL,
                                           RtnProcWild,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL },
                     procGetInfo =       { "PROC_GET_BIND", PROC_GET_BIND,0,1,0,NULL,NULL,NULL,
                                           GetProcBind,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL },
                     procBindInfo =      { "PROC_BIND", PROC_BIND,0,1,0,NULL,NULL,NULL,
                                           PutProcBind,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL };
#if ! DEFFUNCTION_CONSTRUCT
   ENTITY_RECORD deffunctionEntityRecord =
                     { "PCALL", PCALL,0,0,1,
                       NULL,NULL,NULL,
                       EvaluateBadCall,
                       NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL };
#endif
#if ! DEFGENERIC_CONSTRUCT
   ENTITY_RECORD genericEntityRecord =
                     { "GCALL", GCALL,0,0,1,
                       NULL,NULL,NULL,
                       EvaluateBadCall,
                       NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL };
#endif

   AllocateEnvironmentData(theEnv,execStatus,PROCEDURAL_PRIMITIVE_DATA,sizeof(struct proceduralPrimitiveData),DeallocateProceduralPrimitiveData);

   memcpy(&ProceduralPrimitiveData(theEnv,execStatus)->ProcParameterInfo,&procParameterInfo,sizeof(struct entityRecord));  
   memcpy(&ProceduralPrimitiveData(theEnv,execStatus)->ProcWildInfo,&procWildInfo,sizeof(struct entityRecord));  
   memcpy(&ProceduralPrimitiveData(theEnv,execStatus)->ProcGetInfo,&procGetInfo,sizeof(struct entityRecord));  
   memcpy(&ProceduralPrimitiveData(theEnv,execStatus)->ProcBindInfo,&procBindInfo,sizeof(struct entityRecord));  

   InstallPrimitive(theEnv,execStatus,&ProceduralPrimitiveData(theEnv,execStatus)->ProcParameterInfo,PROC_PARAM);
   InstallPrimitive(theEnv,execStatus,&ProceduralPrimitiveData(theEnv,execStatus)->ProcWildInfo,PROC_WILD_PARAM);
   InstallPrimitive(theEnv,execStatus,&ProceduralPrimitiveData(theEnv,execStatus)->ProcGetInfo,PROC_GET_BIND);
   InstallPrimitive(theEnv,execStatus,&ProceduralPrimitiveData(theEnv,execStatus)->ProcBindInfo,PROC_BIND);

   ProceduralPrimitiveData(theEnv,execStatus)->Oldindex = -1;
   
   /* ===============================================
      Make sure a default evaluation function is
      in place for deffunctions and generic functions
      in the event that a binary image containing
      these items is loaded into a configuration
      that does not support them.
      =============================================== */

#if ! DEFFUNCTION_CONSTRUCT
   memcpy(&ProceduralPrimitiveData(theEnv,execStatus)->DeffunctionEntityRecord,&deffunctionEntityRecord,sizeof(struct entityRecord));  
   InstallPrimitive(theEnv,execStatus,&ProceduralPrimitiveData(theEnv,execStatus)->DeffunctionEntityRecord,PCALL);
#endif

#if ! DEFGENERIC_CONSTRUCT
   memcpy(&ProceduralPrimitiveData(theEnv,execStatus)->GenericEntityRecord,&genericEntityRecord,sizeof(struct entityRecord));  
   InstallPrimitive(theEnv,execStatus,&ProceduralPrimitiveData(theEnv,execStatus)->GenericEntityRecord,GCALL);
#endif

   /* =============================================
      Install the special empty multifield to
      let callers distinguish between no parameters
      and zero-length multifield parameters
      ============================================= */
   ProceduralPrimitiveData(theEnv,execStatus)->NoParamValue = CreateMultifield2(theEnv,execStatus,0L);
   MultifieldInstall(theEnv,execStatus,(MULTIFIELD_PTR) ProceduralPrimitiveData(theEnv,execStatus)->NoParamValue);
  }

/**************************************************************/
/* DeallocateProceduralPrimitiveData: Deallocates environment */
/*    data for the procedural primitives functionality.       */
/**************************************************************/
static void DeallocateProceduralPrimitiveData(
  void *theEnv,
  EXEC_STATUS)
  {
   ReturnMultifield(theEnv,execStatus,(struct multifield *) ProceduralPrimitiveData(theEnv,execStatus)->NoParamValue);
   ReleaseProcParameters(theEnv,execStatus);
  }

#if (! BLOAD_ONLY) && (! RUN_TIME)

#if DEFFUNCTION_CONSTRUCT || OBJECT_SYSTEM

/************************************************************
  NAME         : ParseProcParameters
  DESCRIPTION  : Parses a parameter list for a
                  procedural routine, such as a
                  deffunction or message-handler
  INPUTS       : 1) The logical name of the input
                 2) A buffer for scanned tokens
                 3) The partial list of parameters so far
                    (can be NULL)
                 3) A buffer for a wildcard symbol (if any)
                 4) A buffer for a minimum of parameters
                 5) A buffer for a maximum of parameters
                    (will be set to -1 if there is a wilcard)
                 6) A buffer for an error flag
                 7) The address of a function to do specialized
                    checking on a parameter (can be NULL)
                    The function should accept a string and
                    return FALSE if the parameter is OK, TRUE
                    otherwise.
  RETURNS      : A list of expressions containing the
                   parameter names
  SIDE EFFECTS : Parameters parsed and expressions formed
  NOTES        : None
 ************************************************************/
globle EXPRESSION *ParseProcParameters(
  void *theEnv,
  EXEC_STATUS,
  char *readSource,
  struct token *tkn,
  EXPRESSION *parameterList,
  SYMBOL_HN **wildcard,
  int *min,
  int *max,
  int *error,
  int (*checkfunc)(void *,EXEC_STATUS,char *))
  {
   EXPRESSION *nextOne,*lastOne,*check;
   int paramprintp = 0;

   *wildcard = NULL;
   *min = 0;
   *error = TRUE;
   lastOne = nextOne = parameterList;
   while (nextOne != NULL)
     {
      (*min)++;
      lastOne = nextOne;
      nextOne = nextOne->nextArg;
     }
   if (tkn->type != LPAREN)
     {
      SyntaxErrorMessage(theEnv,execStatus,"parameter list");
      ReturnExpression(theEnv,execStatus,parameterList);
      return(NULL);
     }
   GetToken(theEnv,execStatus,readSource,tkn);
   while ((tkn->type == SF_VARIABLE) || (tkn->type == MF_VARIABLE))
     {
      for (check = parameterList ; check != NULL ; check = check->nextArg)
        if (check->value == tkn->value)
         {
          PrintErrorID(theEnv,execStatus,"PRCCODE",7,FALSE);
          EnvPrintRouter(theEnv,execStatus,WERROR,"Duplicate parameter names not allowed.\n");
          ReturnExpression(theEnv,execStatus,parameterList);
          return(NULL);
         }
      if (*wildcard != NULL)
        {
         PrintErrorID(theEnv,execStatus,"PRCCODE",8,FALSE);
         EnvPrintRouter(theEnv,execStatus,WERROR,"No parameters allowed after wildcard parameter.\n");
         ReturnExpression(theEnv,execStatus,parameterList);
         return(NULL);
        }
      if ((checkfunc != NULL) ? (*checkfunc)(theEnv,execStatus,ValueToString(tkn->value)) : FALSE)
        {
         ReturnExpression(theEnv,execStatus,parameterList);
         return(NULL);
        }
      nextOne = GenConstant(theEnv,execStatus,tkn->type,tkn->value);
      if (tkn->type == MF_VARIABLE)
        *wildcard = (SYMBOL_HN *) tkn->value;
      else
        (*min)++;
      if (lastOne == NULL)
        { parameterList = nextOne; }
      else
        { lastOne->nextArg = nextOne; }
      lastOne = nextOne;
      SavePPBuffer(theEnv,execStatus," ");
      paramprintp = 1;
      GetToken(theEnv,execStatus,readSource,tkn);
     }
   if (tkn->type != RPAREN)
     {
      SyntaxErrorMessage(theEnv,execStatus,"parameter list");
      ReturnExpression(theEnv,execStatus,parameterList);
      return(NULL);
     }
   if (paramprintp)
     {
      PPBackup(theEnv,execStatus);
      PPBackup(theEnv,execStatus);
      SavePPBuffer(theEnv,execStatus,")");
     }
   *error = FALSE;
   *max = (*wildcard != NULL) ? -1 : *min;
   return(parameterList);
  }

#endif

/*************************************************************************
  NAME         : ParseProcActions
  DESCRIPTION  : Parses the bodies of deffunctions, generic function
                 methods and message-handlers.  Replaces parameter
                 and local variable references with appropriate
                 runtime access functions
  INPUTS       : 1) The type of procedure body being parsed
                 2) The logical name of the input
                 3) A buffer for scanned tokens
                 4) A list of expressions containing the names
                    of the parameters
                 5) The wilcard parameter symbol (NULL if none)
                 6) A pointer to a function to parse variables not
                    recognized by the standard parser
                    The function should accept the variable
                    expression and a generic pointer for special
                    data (can be NULL) as arguments.  If the variable
                    is recognized, the function should modify the
                    expression to access this variable.  Return 1
                    if recognized, 0 if not, -1 on errors
                    This argument can be NULL.
                 7) A pointer to a function to handle binds in a
                    special way. The function should accept the
                    bind function call expression as an argument.
                    If the variable is recognized and treated specially,
                    the function should modify the expression
                    appropriately (including attaching/removing
                    any necessary argument expressions).  Return 1
                    if recognized, 0 if not, -1 on errors.
                    This argument can be NULL.
                 8) A buffer for holding the number of local vars
                    used by this procedure body.
                 9) Special user data buffer to pass to variable
                    reference and bind replacement functions
RETURNS      : A packed expression containing the body, NULL on
                   errors.
SIDE EFFECTS : Variable references replaced with runtime calls
                  to access the paramter and local variable array
NOTES        : None
*************************************************************************/
globle EXPRESSION *ParseProcActions(
  void *theEnv,
  EXEC_STATUS,
  char *bodytype,
  char *readSource,
  struct token *tkn,
  EXPRESSION *params,
  SYMBOL_HN *wildcard,
  int (*altvarfunc)(void *,EXEC_STATUS,EXPRESSION *,void *),
  int (*altbindfunc)(void *,EXEC_STATUS,EXPRESSION *,void *),
  int *lvarcnt,
  void *userBuffer)
  {
   EXPRESSION *actions,*pactions;

   /* ====================================================================
      Clear parsed bind list - so that only local vars from this body will
      be on it.  The position of vars on thsi list are used to generate
      indices into the LocalVarArray at runtime.  The parsing of the
      "bind" function adds vars to this list.
      ==================================================================== */
   ClearParsedBindNames(theEnv,execStatus);
   actions = GroupActions(theEnv,execStatus,readSource,tkn,TRUE,NULL,FALSE);
   if (actions == NULL)
     return(NULL);

   /* ====================================================================
      Replace any bind functions with special functions before replacing
      any variable references.  This allows those bind names to be removed
      before they can be seen by variable replacement and thus generate
      incorrect indices.
      ==================================================================== */
   if (altbindfunc != NULL)
     {
      if (ReplaceProcBinds(theEnv,execStatus,actions,altbindfunc,userBuffer))
        {
         ClearParsedBindNames(theEnv,execStatus);
         ReturnExpression(theEnv,execStatus,actions);
         return(NULL);
        }
     }

   /* ======================================================================
      The number of names left on the bind list is the number of local
      vars for this procedure body.  Replace all variable reference with
      runtime access functions for ProcParamArray, LocalVarArray or
      other special items, such as direct slot references, global variables,
      or fact field references.
      ====================================================================== */
   *lvarcnt = CountParsedBindNames(theEnv,execStatus);
   if (ReplaceProcVars(theEnv,execStatus,bodytype,actions,params,wildcard,altvarfunc,userBuffer))
     {
      ClearParsedBindNames(theEnv,execStatus);
      ReturnExpression(theEnv,execStatus,actions);
      return(NULL);
     }

   /* =======================================================================
      Normally, actions are grouped in a progn.  If there is only one action,
      the progn is unnecessary and can be removed.  Also, the actions are
      packed into a contiguous array to save on memory overhead.  The
      intermediate parsed bind names are freed to avoid tying up memory.
      ======================================================================= */
   actions = CompactActions(theEnv,execStatus,actions);
   pactions = PackExpression(theEnv,execStatus,actions);
   ReturnExpression(theEnv,execStatus,actions);
   ClearParsedBindNames(theEnv,execStatus);
   return(pactions);
  }

/*************************************************************************
  NAME         : ReplaceProcVars
  DESCRIPTION  : Examines an expression for variables
                   and replaces any that correspond to
                   procedure parameters or globals
                   with function calls that get these
                   variables' values at run-time.
                   For example, procedure arguments
                   are stored an array at run-time, so at
                   parse-time, parameter-references are replaced
                   with function calls referencing this array at
                   the appropriate position.
  INPUTS       : 1) The type of procedure being parsed
                 2) The expression-actions to be examined
                 3) The parameter list
                 4) The wildcard parameter symbol (NULL if none)
                 5) A pointer to a function to parse variables not
                    recognized by the standard parser
                    The function should accept the variable
                    expression and a generic pointer for special
                    data (can be NULL) as arguments.  If the variable
                    is recognized, the function should modify the
                    expression to access this variable.  Return 1
                    if recognized, 0 if not, -1 on errors
                    This argument can be NULL.
                 6) Data buffer to be passed to alternate parsing
                    function
  RETURNS      : FALSE if OK, TRUE on errors
  SIDE EFFECTS : Variable references replaced with function calls
  NOTES        : This function works from the ParsedBindNames list in
                    SPCLFORM.C to access local binds.  Make sure that
                    the list accurately reflects the binds by calling
                    ClearParsedBindNames(theEnv,execStatus) before the parse of the body
                    in which variables are being replaced.
 *************************************************************************/
globle int ReplaceProcVars(
  void *theEnv,
  EXEC_STATUS,
  char *bodytype,
  EXPRESSION *actions,
  EXPRESSION *parameterList,
  SYMBOL_HN *wildcard,
  int (*altvarfunc)(void *,EXEC_STATUS,EXPRESSION *,void *),
  void *specdata)
  {
   int position,altcode;
   intBool boundPosn;
   EXPRESSION *arg_lvl,*altvarexp;
   SYMBOL_HN *bindName;
   PACKED_PROC_VAR pvar;

   while (actions != NULL)
     {
      if (actions->type == SF_VARIABLE)
        {
         /*===============================================*/
         /* See if the variable is in the parameter list. */
         /*===============================================*/
         bindName = (SYMBOL_HN *) actions->value;
         position = FindProcParameter(bindName,parameterList,wildcard);

         /*=============================================================*/
         /* Check to see if the variable is bound within the procedure. */
         /*=============================================================*/
         boundPosn = SearchParsedBindNames(theEnv,execStatus,bindName);

         /*=============================================*/
         /* If variable is not defined in the parameter */
         /* list or as part of a bind action then...    */
         /*=============================================*/

         if ((position == 0) && (boundPosn == 0))
           {
            /*================================================================*/
            /* Check to see if the variable has a special access function,    */
            /* such as direct slot reference or a rule RHS pattern reference. */
            /*================================================================*/
            if ((altvarfunc != NULL) ? ((*altvarfunc)(theEnv,execStatus,actions,specdata) != 1) : TRUE)
              {
               PrintErrorID(theEnv,execStatus,"PRCCODE",3,TRUE);
               EnvPrintRouter(theEnv,execStatus,WERROR,"Undefined variable ");
               EnvPrintRouter(theEnv,execStatus,WERROR,ValueToString(bindName));
               EnvPrintRouter(theEnv,execStatus,WERROR," referenced in ");
               EnvPrintRouter(theEnv,execStatus,WERROR,bodytype);
               EnvPrintRouter(theEnv,execStatus,WERROR,".\n");
               return(TRUE);
              }
           }

         /*===================================================*/
         /* Else if variable is defined in the parameter list */
         /* and not rebound within the procedure then...      */
         /*===================================================*/

         else if ((position > 0) && (boundPosn == 0))
           {
            actions->type = (unsigned short) ((bindName != wildcard) ? PROC_PARAM : PROC_WILD_PARAM);
            actions->value = EnvAddBitMap(theEnv,execStatus,(void *) &position,(int) sizeof(int));
           }

         /*=========================================================*/
         /* Else the variable is rebound within the procedure so... */
         /*=========================================================*/

         else
           {
            if (altvarfunc != NULL)
              {
               altvarexp = GenConstant(theEnv,execStatus,actions->type,actions->value);
               altcode = (*altvarfunc)(theEnv,execStatus,altvarexp,specdata);
               if (altcode == 0)
                 {
                  rtn_struct(theEnv,execStatus,expr,altvarexp);
                  altvarexp = NULL;
                 }
               else if (altcode == -1)
                 {
                  rtn_struct(theEnv,execStatus,expr,altvarexp);
                  return(TRUE);
                 }
              }
            else
              altvarexp = NULL;
            actions->type = PROC_GET_BIND;
            ClearBitString((void *) &pvar,(int) sizeof(PACKED_PROC_VAR));
            pvar.first = boundPosn;
            pvar.second = position;
            pvar.secondFlag = (bindName != wildcard) ? 0 : 1;
            actions->value = EnvAddBitMap(theEnv,execStatus,(void *) &pvar,(int) sizeof(PACKED_PROC_VAR));
            actions->argList = GenConstant(theEnv,execStatus,SYMBOL,(void *) bindName);
            actions->argList->nextArg = altvarexp;
           }
        }
#if DEFGLOBAL_CONSTRUCT
      else if (actions->type == GBL_VARIABLE)
        {
         if (ReplaceGlobalVariable(theEnv,execStatus,actions) == FALSE)
           return(-1);
        }
#endif
      if ((altvarfunc != NULL) ? ((*altvarfunc)(theEnv,execStatus,actions,specdata) == -1) : FALSE)
        return(TRUE);
      if (actions->argList != NULL)
        {
         if (ReplaceProcVars(theEnv,execStatus,bodytype,actions->argList,parameterList,
                                        wildcard,altvarfunc,specdata))
           return(TRUE);

         /* ====================================================================
            Check to see if this is a call to the bind function.  If so (and the
            second argument is a symbol) then it is a locally bound variable
            (as opposed to a global).

            Replace the call to "bind" with a call to PROC_BIND - the
            special internal function for procedure local variables.
            ==================================================================== */
         if ((actions->value == (void *) FindFunction(theEnv,execStatus,"bind")) &&
             (actions->argList->type == SYMBOL))
           {
            actions->type = PROC_BIND;
            boundPosn = SearchParsedBindNames(theEnv,execStatus,(SYMBOL_HN *) actions->argList->value);
            actions->value = EnvAddBitMap(theEnv,execStatus,(void *) &boundPosn,(int) sizeof(intBool));
            arg_lvl = actions->argList->nextArg;
            rtn_struct(theEnv,execStatus,expr,actions->argList);
            actions->argList = arg_lvl;
           }
        }
      actions = actions->nextArg;
     }
   return(FALSE);
  }

#if DEFGENERIC_CONSTRUCT

/*****************************************************
  NAME         : GenProcWildcardReference
  DESCRIPTION  : Returns an expression to access the
                    wildcard parameter for a method
  INPUTS       : The starting index of the wildcard
  RETURNS      : An expression containing the wildcard
                 reference
  SIDE EFFECTS : Expression allocated
  NOTES        : None
 *****************************************************/
globle EXPRESSION *GenProcWildcardReference(
  void *theEnv,
  EXEC_STATUS,
  int theIndex)
  {
   return(GenConstant(theEnv,execStatus,PROC_WILD_PARAM,EnvAddBitMap(theEnv,execStatus,(void *) &theIndex,(int) sizeof(int))));
  }

#endif

#endif

/*******************************************************************
  NAME         : PushProcParameters
  DESCRIPTION  : Given a list of parameter expressions,
                   this function evaluates each expression
                   and stores the results in a contiguous
                   array of DATA_OBJECTS.  Used in creating a new
                   ProcParamArray for the execution of a
                   procedure
                 The current arrays are saved on a stack.
  INPUTS       : 1) The paramter expression list
                 2) The number of parameters in the list
                 3) The name of the procedure for which
                    these parameters are being evaluated
                 4) The type of procedure
                 5) A pointer to a function to print out a trace
                    message about the currently executing
                    procedure when unbound variables are detected
                    at runtime (The function should take no
                    arguments and have no return value.  The
                    function should print its synopsis to WERROR
                    and include the final carriage-return.)
  RETURNS      : Nothing useful
  SIDE EFFECTS : Any side-effects of the evaluation of the
                   parameter expressions
                 DATA_OBJECT array allocated (deallocated on errors)
                 ProcParamArray set
  NOTES        : EvaluationError set on errors
 *******************************************************************/
globle void PushProcParameters(
  void *theEnv,
  EXEC_STATUS,
  EXPRESSION *parameterList,
  int numberOfParameters,
  char *pname,
  char *bodytype,
  void (*UnboundErrFunc)(void *,EXEC_STATUS))
  {
   register PROC_PARAM_STACK *ptmp;

   ptmp = get_struct(theEnv,execStatus,ProcParamStack);
   ptmp->ParamArray = ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray;
   ptmp->ParamArraySize = ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArraySize;
   ptmp->UnboundErrFunc = ProceduralPrimitiveData(theEnv,execStatus)->ProcUnboundErrFunc;
   ptmp->nxt = ProceduralPrimitiveData(theEnv,execStatus)->pstack;
   ProceduralPrimitiveData(theEnv,execStatus)->pstack = ptmp;
   EvaluateProcParameters(theEnv,execStatus,parameterList,numberOfParameters,pname,bodytype);
   if (execStatus->EvaluationError)
     {
      ptmp = ProceduralPrimitiveData(theEnv,execStatus)->pstack;
      ProceduralPrimitiveData(theEnv,execStatus)->pstack = ProceduralPrimitiveData(theEnv,execStatus)->pstack->nxt;
      rtn_struct(theEnv,execStatus,ProcParamStack,ptmp);
      return;
     }

   /* ================================================================
      Record ProcParamExpressions and WildcardValue for previous frame
      AFTER evaluating arguments for the new frame, because they could
      have gone from NULL to non-NULL (if they were already non-NULL,
      they would remain unchanged.)
      ================================================================ */
#if DEFGENERIC_CONSTRUCT
   ptmp->ParamExpressions = ProceduralPrimitiveData(theEnv,execStatus)->ProcParamExpressions;
   ProceduralPrimitiveData(theEnv,execStatus)->ProcParamExpressions = NULL;
#endif
   ptmp->WildcardValue = ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue;
   ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue = NULL;
   ProceduralPrimitiveData(theEnv,execStatus)->ProcUnboundErrFunc = UnboundErrFunc;
  }

/******************************************************************
  NAME         : PopProcParameters
  DESCRIPTION  : Restores old procedure arrays
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Stack popped and globals restored
  NOTES        : Assumes pstack != NULL
 ******************************************************************/
globle void PopProcParameters(
  void *theEnv,
  EXEC_STATUS)
  {
   register PROC_PARAM_STACK *ptmp;

   if (ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray != NULL)
     rm(theEnv,execStatus,(void *) ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray,(sizeof(DATA_OBJECT) * ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArraySize));

#if DEFGENERIC_CONSTRUCT
   if (ProceduralPrimitiveData(theEnv,execStatus)->ProcParamExpressions != NULL)
     rm(theEnv,execStatus,(void *) ProceduralPrimitiveData(theEnv,execStatus)->ProcParamExpressions,(sizeof(EXPRESSION) * ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArraySize));
#endif

   ptmp = ProceduralPrimitiveData(theEnv,execStatus)->pstack;
   ProceduralPrimitiveData(theEnv,execStatus)->pstack = ProceduralPrimitiveData(theEnv,execStatus)->pstack->nxt;
   ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray = ptmp->ParamArray;
   ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArraySize = ptmp->ParamArraySize;

#if DEFGENERIC_CONSTRUCT
   ProceduralPrimitiveData(theEnv,execStatus)->ProcParamExpressions = ptmp->ParamExpressions;
#endif

   if (ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue != NULL)
     {
      MultifieldDeinstall(theEnv,execStatus,(MULTIFIELD_PTR) ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue->value);
      if (ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue->value != ProceduralPrimitiveData(theEnv,execStatus)->NoParamValue)
        AddToMultifieldList(theEnv,execStatus,(MULTIFIELD_PTR) ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue->value);
      rtn_struct(theEnv,execStatus,dataObject,ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue);
     }
   ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue = ptmp->WildcardValue;
   ProceduralPrimitiveData(theEnv,execStatus)->ProcUnboundErrFunc = ptmp->UnboundErrFunc;
   rtn_struct(theEnv,execStatus,ProcParamStack,ptmp);
  }

/******************************************************************
  NAME         : ReleaseProcParameters
  DESCRIPTION  : Restores old procedure arrays
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Stack popped and globals restored
  NOTES        : Assumes pstack != NULL
 ******************************************************************/
static void ReleaseProcParameters(
  void *theEnv,
  EXEC_STATUS)
  {
   register PROC_PARAM_STACK *ptmp, *next;

   if (ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray != NULL)
     rm(theEnv,execStatus,(void *) ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray,(sizeof(DATA_OBJECT) * ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArraySize));


   if (ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue != NULL)
     {
      if (ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue->value != ProceduralPrimitiveData(theEnv,execStatus)->NoParamValue)
        { ReturnMultifield(theEnv,execStatus,(struct multifield *) ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue->value); }
     
      rtn_struct(theEnv,execStatus,dataObject,ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue); 
     }
     
#if DEFGENERIC_CONSTRUCT
   if (ProceduralPrimitiveData(theEnv,execStatus)->ProcParamExpressions != NULL)
     rm(theEnv,execStatus,(void *) ProceduralPrimitiveData(theEnv,execStatus)->ProcParamExpressions,(sizeof(EXPRESSION) * ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArraySize));
#endif

   ptmp = ProceduralPrimitiveData(theEnv,execStatus)->pstack;
   
   while (ptmp != NULL)
     {
      next = ptmp->nxt;

      if (ptmp->ParamArray != NULL)
        { rm(theEnv,execStatus,(void *) ptmp->ParamArray,(sizeof(DATA_OBJECT) * ptmp->ParamArraySize)); }

#if DEFGENERIC_CONSTRUCT
      if (ptmp->ParamExpressions != NULL)
        { rm(theEnv,execStatus,(void *) ptmp->ParamExpressions,(sizeof(EXPRESSION) * ptmp->ParamArraySize)); }
#endif

      if (ptmp->WildcardValue != NULL)
        { 
         if (ptmp->WildcardValue->value != ProceduralPrimitiveData(theEnv,execStatus)->NoParamValue)
           { ReturnMultifield(theEnv,execStatus,(struct multifield *) ptmp->WildcardValue->value); }

         rtn_struct(theEnv,execStatus,dataObject,ptmp->WildcardValue); 
        }
     
      rtn_struct(theEnv,execStatus,ProcParamStack,ptmp);
      ptmp = next;
     }
  }
  
#if DEFGENERIC_CONSTRUCT

/***********************************************************
  NAME         : GetProcParamExpressions
  DESCRIPTION  : Forms an array of expressions equivalent to
                 the current procedure paramter array.  Used
                 to conveniently attach these parameters as
                 arguments to a H/L system function call
                 (used by the generic dispatch).
  INPUTS       : None
  RETURNS      : A pointer to an array of expressions
  SIDE EFFECTS : Expression array created
  NOTES        : None
 ***********************************************************/
globle EXPRESSION *GetProcParamExpressions(
  void *theEnv,
  EXEC_STATUS)
  {
   register int i;

   if ((ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray == NULL) || (ProceduralPrimitiveData(theEnv,execStatus)->ProcParamExpressions != NULL))
     return(ProceduralPrimitiveData(theEnv,execStatus)->ProcParamExpressions);
   ProceduralPrimitiveData(theEnv,execStatus)->ProcParamExpressions = (EXPRESSION *)
               gm2(theEnv,execStatus,(sizeof(EXPRESSION) * ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArraySize));
   for (i = 0 ; i < ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArraySize ; i++)
     {
      ProceduralPrimitiveData(theEnv,execStatus)->ProcParamExpressions[i].type = ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray[i].type;
      if (ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray[i].type != MULTIFIELD)
        ProceduralPrimitiveData(theEnv,execStatus)->ProcParamExpressions[i].value = ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray[i].value;
      else
        ProceduralPrimitiveData(theEnv,execStatus)->ProcParamExpressions[i].value = (void *) &ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray[i];
      ProceduralPrimitiveData(theEnv,execStatus)->ProcParamExpressions[i].argList = NULL;
      ProceduralPrimitiveData(theEnv,execStatus)->ProcParamExpressions[i].nextArg =
        ((i + 1) != ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArraySize) ? &ProceduralPrimitiveData(theEnv,execStatus)->ProcParamExpressions[i+1] : NULL;
     }
   return(ProceduralPrimitiveData(theEnv,execStatus)->ProcParamExpressions);
  }

#endif

/***********************************************************
  NAME         : EvaluateProcActions
  DESCRIPTION  : Evaluates the actions of a deffunction,
                 generic function method or message-handler.
  INPUTS       : 1) The module where the actions should be
                    executed
                 2) The actions (linked by nextArg fields)
                 3) The number of local variables to reserve
                    space for.
                 4) A buffer to hold the result of evaluating
                    the actions.
                 5) A function which prints out the name of
                    the currently executing body for error
                    messages (can be NULL).
  RETURNS      : Nothing useful
  SIDE EFFECTS : Allocates and deallocates space for
                 local variable array.
  NOTES        : None
 ***********************************************************/
globle void EvaluateProcActions(
  void *theEnv,
  EXEC_STATUS,
  struct defmodule *theModule,
  EXPRESSION *actions,
  int lvarcnt,
  DATA_OBJECT *result,
  void (*crtproc)(void *))
  {
   DATA_OBJECT *oldLocalVarArray;
   register int i;
   struct defmodule *oldModule;
   EXPRESSION *oldActions;
   struct trackedMemory *theTM;

   oldLocalVarArray = ProceduralPrimitiveData(theEnv,execStatus)->LocalVarArray;
   ProceduralPrimitiveData(theEnv,execStatus)->LocalVarArray = (lvarcnt == 0) ? NULL :
                   (DATA_OBJECT *) gm2(theEnv,execStatus,(sizeof(DATA_OBJECT) * lvarcnt));

   if (lvarcnt != 0)
     { theTM = AddTrackedMemory(theEnv,execStatus,ProceduralPrimitiveData(theEnv,execStatus)->LocalVarArray,sizeof(DATA_OBJECT) * lvarcnt); }
   else
     { theTM = NULL; }
     
   for (i = 0 ; i < lvarcnt ; i++)
     ProceduralPrimitiveData(theEnv,execStatus)->LocalVarArray[i].supplementalInfo = EnvFalseSymbol(theEnv,execStatus);

   oldModule = ((struct defmodule *) EnvGetCurrentModule(theEnv,execStatus));
   if (oldModule != theModule)
     EnvSetCurrentModule(theEnv,execStatus,(void *) theModule);
   oldActions = ProceduralPrimitiveData(theEnv,execStatus)->CurrentProcActions;
   ProceduralPrimitiveData(theEnv,execStatus)->CurrentProcActions = actions;

   if (EvaluateExpression(theEnv,execStatus,actions,result))
     {
      result->type = SYMBOL;
      result->value = EnvFalseSymbol(theEnv,execStatus);
     }

   ProceduralPrimitiveData(theEnv,execStatus)->CurrentProcActions = oldActions;
   if (oldModule != ((struct defmodule *) EnvGetCurrentModule(theEnv,execStatus)))
     EnvSetCurrentModule(theEnv,execStatus,(void *) oldModule);
   if ((crtproc != NULL) ? execStatus->HaltExecution : FALSE)
     {
      PrintErrorID(theEnv,execStatus,"PRCCODE",4,FALSE);
      EnvPrintRouter(theEnv,execStatus,WERROR,"Execution halted during the actions of ");
      (*crtproc)(theEnv,execStatus);
     }
   if ((ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue != NULL) ? (result->value == ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue->value) : FALSE)
     {
      MultifieldDeinstall(theEnv,execStatus,(MULTIFIELD_PTR) ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue->value);
      if (ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue->value != ProceduralPrimitiveData(theEnv,execStatus)->NoParamValue)
        AddToMultifieldList(theEnv,execStatus,(MULTIFIELD_PTR) ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue->value);
      rtn_struct(theEnv,execStatus,dataObject,ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue);
      ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue = NULL;
     }

   if (lvarcnt != 0)
     {
      RemoveTrackedMemory(theEnv,execStatus,theTM);
      for (i = 0 ; i < lvarcnt ; i++)
        if (ProceduralPrimitiveData(theEnv,execStatus)->LocalVarArray[i].supplementalInfo == EnvTrueSymbol(theEnv,execStatus))
          ValueDeinstall(theEnv,execStatus,&ProceduralPrimitiveData(theEnv,execStatus)->LocalVarArray[i]);
      rm(theEnv,execStatus,(void *) ProceduralPrimitiveData(theEnv,execStatus)->LocalVarArray,(sizeof(DATA_OBJECT) * lvarcnt));
     }

   ProceduralPrimitiveData(theEnv,execStatus)->LocalVarArray = oldLocalVarArray;
  }

/****************************************************
  NAME         : PrintProcParamArray
  DESCRIPTION  : Displays the contents of the
                 current procedure parameter array
  INPUTS       : The logical name of the output
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ****************************************************/
globle void PrintProcParamArray(
  void *theEnv,
  EXEC_STATUS,
  char *logName)
  {
   register int i;

   EnvPrintRouter(theEnv,execStatus,logName," (");
   for (i = 0 ; i < ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArraySize ; i++)
     {
      PrintDataObject(theEnv,execStatus,logName,&ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray[i]);
      if (i != ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArraySize-1)
        EnvPrintRouter(theEnv,execStatus,logName," ");
     }
   EnvPrintRouter(theEnv,execStatus,logName,")\n");
  }

/****************************************************************
  NAME         : GrabProcWildargs
  DESCRIPTION  : Groups a portion of the ProcParamArray
                   into a multi-field variable
  INPUTS       : 1) Starting index in ProcParamArray
                      for grouping of arguments into
                      multi-field variable
                 2) Caller's result value buffer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Multi-field variable allocated and set
                   with corresponding values of ProcParamArray
  NOTES        : Multi-field is NOT on list of ephemeral segments
 ****************************************************************/
globle void GrabProcWildargs(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *result,
  int theIndex)
  {
   register int i,j;
   long k; /* 6.04 Bug Fix */
   long size;
   DATA_OBJECT *val;

   result->type = MULTIFIELD;
   result->begin = 0;
   if (ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue == NULL)
     {
      ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue = get_struct(theEnv,execStatus,dataObject);
      ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue->begin = 0;
     }
   else if (theIndex == ProceduralPrimitiveData(theEnv,execStatus)->Oldindex)
     {
      result->end = ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue->end;
      result->value = ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue->value;
      return;
     }
   else
     {
      MultifieldDeinstall(theEnv,execStatus,(MULTIFIELD_PTR) ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue->value);
      if (ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue->value != ProceduralPrimitiveData(theEnv,execStatus)->NoParamValue)
        AddToMultifieldList(theEnv,execStatus,(MULTIFIELD_PTR) ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue->value);
     }
   ProceduralPrimitiveData(theEnv,execStatus)->Oldindex = theIndex;
   size = ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArraySize - theIndex + 1;
   if (size <= 0)
     {
      result->end = ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue->end = -1;
      result->value = ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue->value = ProceduralPrimitiveData(theEnv,execStatus)->NoParamValue;
      MultifieldInstall(theEnv,execStatus,(MULTIFIELD_PTR) ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue->value);
      return;
     }
   for (i = theIndex-1 ; i < ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArraySize ; i++)
     {
      if (ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray[i].type == MULTIFIELD)
        size += ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray[i].end - ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray[i].begin;
     }
   result->end = ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue->end = size-1;
   result->value = ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue->value = (void *) CreateMultifield2(theEnv,execStatus,(unsigned long) size);
   for (i = theIndex-1 , j = 1 ; i < ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArraySize ; i++)
     {
      if (ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray[i].type != MULTIFIELD)
        {
         SetMFType(result->value,j,(short) ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray[i].type);
         SetMFValue(result->value,j,ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray[i].value);
         j++;
        }
      else
        {
         val = &ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray[i];
         for (k = val->begin + 1 ; k <= val->end + 1 ; k++ , j++)
           {
            SetMFType(result->value,j,GetMFType(val->value,k));
            SetMFValue(result->value,j,GetMFValue(val->value,k));
           }
        }
     }
   MultifieldInstall(theEnv,execStatus,(MULTIFIELD_PTR) ProceduralPrimitiveData(theEnv,execStatus)->WildcardValue->value);
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/*******************************************************************
  NAME         : EvaluateProcParameters
  DESCRIPTION  : Given a list of parameter expressions,
                   this function evaluates each expression
                   and stores the results in a contiguous
                   array of DATA_OBJECTS.  Used in creating a new
                   ProcParamArray for the execution of a
                   procedure
  INPUTS       : 1) The paramter expression list
                 2) The number of parameters in the list
                 3) The name of the procedure for which
                    these parameters are being evaluated
                 4) The type of procedure
  RETURNS      : Nothing useful
  SIDE EFFECTS : Any side-effects of the evaluation of the
                   parameter expressions
                 DATA_OBJECT array allocated (deallocated on errors)
                 ProcParamArray set
  NOTES        : EvaluationError set on errors
 *******************************************************************/
static void EvaluateProcParameters(
  void *theEnv,
  EXEC_STATUS,
  EXPRESSION *parameterList,
  int numberOfParameters,
  char *pname,
  char *bodytype)
  {
   DATA_OBJECT *rva,temp;
   int i = 0;

   if (numberOfParameters == 0)
     {
      ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray = NULL;
      ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArraySize = 0;
      return;
     }

   rva = (DATA_OBJECT *) gm2(theEnv,execStatus,(sizeof(DATA_OBJECT) * numberOfParameters));
   while (parameterList != NULL)
     {
      if ((EvaluateExpression(theEnv,execStatus,parameterList,&temp) == TRUE) ? TRUE :
          (temp.type == RVOID))
        {
         if (temp.type == RVOID)
           {
            PrintErrorID(theEnv,execStatus,"PRCCODE",2,FALSE);
            EnvPrintRouter(theEnv,execStatus,WERROR,"Functions without a return value are illegal as ");
            EnvPrintRouter(theEnv,execStatus,WERROR,bodytype);
            EnvPrintRouter(theEnv,execStatus,WERROR," arguments.\n");
            SetEvaluationError(theEnv,execStatus,TRUE);
           }
         PrintErrorID(theEnv,execStatus,"PRCCODE",6,FALSE);
         EnvPrintRouter(theEnv,execStatus,WERROR,"This error occurred while evaluating arguments ");
         EnvPrintRouter(theEnv,execStatus,WERROR,"for the ");
         EnvPrintRouter(theEnv,execStatus,WERROR,bodytype);
         EnvPrintRouter(theEnv,execStatus,WERROR," ");
         EnvPrintRouter(theEnv,execStatus,WERROR,pname);
         EnvPrintRouter(theEnv,execStatus,WERROR,".\n");
         rm(theEnv,execStatus,(void *) rva,(sizeof(DATA_OBJECT) * numberOfParameters));
         return;
        }
      rva[i].type = temp.type;
      rva[i].value = temp.value;
      rva[i].begin = temp.begin;
      rva[i].end = temp.end;
      parameterList = parameterList->nextArg;
      i++;
     }
   ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArraySize = numberOfParameters;
   ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray = rva;
  }

/***************************************************
  NAME         : RtnProcParam
  DESCRIPTION  : Internal function for getting the
                   value of an argument passed to
                   a procedure
  INPUTS       : 1) Expression to evaluate
                    (PROC_PARAM index)
                 2) Caller's result value buffer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Caller's buffer set to specified
                   node of ProcParamArray
  NOTES        : None
 ***************************************************/
static intBool RtnProcParam(
  void *theEnv,
  EXEC_STATUS,
  void *value,
  DATA_OBJECT *result)
  {
   register DATA_OBJECT *src;
   
   src = &ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray[*((int *) ValueToBitMap(value)) - 1];
   result->type = src->type;
   result->value = src->value;
   result->begin = src->begin;
   result->end = src->end;
   return(TRUE);
  }

/**************************************************************
  NAME         : GetProcBind
  DESCRIPTION  : Internal function for looking up the
                    values of parameters or bound variables
                    within procedures
  INPUTS       : 1) Expression to evaluate
                    (PROC_GET_BIND index)
                 2) Caller's result value buffer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Caller's buffer set to parameter value in
                   ProcParamArray or the value in LocalVarArray
  NOTES        : None
 **************************************************************/
static intBool GetProcBind(
  void *theEnv,
  EXEC_STATUS,
  void *value,
  DATA_OBJECT *result)
  {
   register DATA_OBJECT *src;
   PACKED_PROC_VAR *pvar;

   pvar = (PACKED_PROC_VAR *) ValueToBitMap(value);
   src = &ProceduralPrimitiveData(theEnv,execStatus)->LocalVarArray[pvar->first - 1];
   if (src->supplementalInfo == EnvTrueSymbol(theEnv,execStatus))
     {
      result->type = src->type;
      result->value = src->value;
      result->begin = src->begin;
      result->end = src->end;
      return(TRUE);
     }
   if (GetFirstArgument()->nextArg != NULL)
     {
      EvaluateExpression(theEnv,execStatus,GetFirstArgument()->nextArg,result);
      return(TRUE);
     }
   if (pvar->second == 0)
     {
      PrintErrorID(theEnv,execStatus,"PRCCODE",5,FALSE);
      SetEvaluationError(theEnv,execStatus,TRUE);
      EnvPrintRouter(theEnv,execStatus,WERROR,"Variable ");
      EnvPrintRouter(theEnv,execStatus,WERROR,ValueToString(GetFirstArgument()->value));
      if (ProceduralPrimitiveData(theEnv,execStatus)->ProcUnboundErrFunc != NULL)
        {
         EnvPrintRouter(theEnv,execStatus,WERROR," unbound in ");
         (*ProceduralPrimitiveData(theEnv,execStatus)->ProcUnboundErrFunc)(theEnv,execStatus);
        }
      else
        EnvPrintRouter(theEnv,execStatus,WERROR," unbound.\n");
      result->type = SYMBOL;
      result->value = EnvFalseSymbol(theEnv,execStatus);
      return(TRUE);
     }
   if (pvar->secondFlag == 0)
     {
      src = &ProceduralPrimitiveData(theEnv,execStatus)->ProcParamArray[pvar->second - 1];
      result->type = src->type;
      result->value = src->value;
      result->begin = src->begin;
      result->end = src->end;
     }
   else
     GrabProcWildargs(theEnv,execStatus,result,(int) pvar->second);
   return(TRUE);
  }

/**************************************************************
  NAME         : PutProcBind
  DESCRIPTION  : Internal function for setting the values of
                 of locally bound variables within procedures
  INPUTS       : 1) Expression to evaluate
                    (PROC_PARAM index)
                 2) Caller's result value buffer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Bound variable in LocalVarArray set to
                   value in caller's buffer.
  NOTES        : None
 **************************************************************/
static intBool PutProcBind(
  void *theEnv,
  EXEC_STATUS,
  void *value,
  DATA_OBJECT *result)
  {
   register DATA_OBJECT *dst;

   dst = &ProceduralPrimitiveData(theEnv,execStatus)->LocalVarArray[*((int *) ValueToBitMap(value)) - 1];
   if (GetFirstArgument() == NULL)
     {
      if (dst->supplementalInfo == EnvTrueSymbol(theEnv,execStatus))
        ValueDeinstall(theEnv,execStatus,dst);
      dst->supplementalInfo = EnvFalseSymbol(theEnv,execStatus);
      result->type = SYMBOL;
      result->value = EnvFalseSymbol(theEnv,execStatus);
     }
   else
     {
      if (GetFirstArgument()->nextArg != NULL)
        StoreInMultifield(theEnv,execStatus,result,GetFirstArgument(),TRUE);
      else
        EvaluateExpression(theEnv,execStatus,GetFirstArgument(),result);
      if (dst->supplementalInfo == EnvTrueSymbol(theEnv,execStatus))
        ValueDeinstall(theEnv,execStatus,dst);
      dst->supplementalInfo = EnvTrueSymbol(theEnv,execStatus);
      dst->type = result->type;
      dst->value = result->value;
      dst->begin = result->begin;
      dst->end = result->end;
      ValueInstall(theEnv,execStatus,dst);
     }
   return(TRUE);
  }

/****************************************************************
  NAME         : RtnProcWild
  DESCRIPTION  : Groups a portion of the ProcParamArray
                   into a multi-field variable
  INPUTS       : 1) Starting index in ProcParamArray
                      for grouping of arguments into
                      multi-field variable (expression value)
                 2) Caller's result value buffer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Multi-field variable allocated and set
                   with corresponding values of ProcParamArray
  NOTES        : Multi-field is NOT on list of ephemeral segments
 ****************************************************************/
static intBool RtnProcWild(
  void *theEnv,
  EXEC_STATUS,
  void *value,
  DATA_OBJECT *result)
  {
   GrabProcWildargs(theEnv,execStatus,result,*(int *) ValueToBitMap(value));
   return(TRUE);
  }

#if (! BLOAD_ONLY) && (! RUN_TIME)

/***************************************************
  NAME         : FindProcParameter
  DESCRIPTION  : Determines the relative position in
                   an n-element list of a certain
                   parameter.  The index is 1..n.
  INPUTS       : 1) Parameter name
                 2) Parameter list
                 3) Wildcard symbol (NULL if none)
  RETURNS      : Index of parameter in list, 0 if
                   not found
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
static int FindProcParameter(
  SYMBOL_HN *name,
  EXPRESSION *parameterList,
  SYMBOL_HN *wildcard)
  {
   int i = 1;

   while (parameterList != NULL)
     {
      if (parameterList->value == (void *) name)
        return(i);
      i++;
      parameterList = parameterList->nextArg;
     }

   /* ===================================================================
      Wildcard may not be stored in actual list but know is always at end
      =================================================================== */
   if (name == wildcard)
     return(i);
   return(0);
  }

/*************************************************************************
  NAME         : ReplaceProcBinds
  DESCRIPTION  : Examines an expression and replaces calls to the
                 "bind" function which are specially recognized

                 For example, in a message-handler,

                   (bind ?self <value>) would be illegal

                   and

                   (bind ?self:<slot-name> <value>) would be
                   replaced with
                   (put <slot-name> <value>)

  INPUTS       : 1) The actions in which to replace special binds
                 2) A pointer to a function to handle binds in a
                    special way. The function should accept the
                    bind function call expression and a specialized
                    data buffer (can be NULL) as arguments.
                    If the variable is recognized and treated specially,
                    the function should modify the expression
                    appropriately (including attaching/removing
                    any necessary argument expressions).  Return 1
                    if recognized, 0 if not, -1 on errors.
                    This argument CANNOT be NULL.
                 3) Specialized user data buffer
  RETURNS      : FALSE if OK, TRUE on errors
  SIDE EFFECTS : Some binds replaced with specialized calls
  NOTES        : Local variable binds are replaced in ReplaceProcVars
                 (after this routine has had a chance to replace all
                  special binds and remove the names from the parsed
                  bind list)
 *************************************************************************/
static int ReplaceProcBinds(
  void *theEnv,
  EXEC_STATUS,
  EXPRESSION *actions,
  int (*altbindfunc)(void *,EXEC_STATUS,EXPRESSION *,void *),
  void *userBuffer)
  {
   int bcode;
   SYMBOL_HN *bname;

   while (actions != NULL)
     {
      if (actions->argList != NULL)
        {
         if (ReplaceProcBinds(theEnv,execStatus,actions->argList,altbindfunc,userBuffer))
           return(TRUE);
         if ((actions->value == (void *) FindFunction(theEnv,execStatus,"bind")) &&
             (actions->argList->type == SYMBOL))
           {
            bname = (SYMBOL_HN *) actions->argList->value;
            bcode = (*altbindfunc)(theEnv,execStatus,actions,userBuffer);
            if (bcode == -1)
              return(TRUE);
            if (bcode == 1)
              RemoveParsedBindName(theEnv,execStatus,bname);
           }
        }
      actions = actions->nextArg;
     }
   return(FALSE);
  }

/*****************************************************
  NAME         : CompactActions
  DESCRIPTION  : Examines a progn expression chain,
                 and if there is only one action,
                 the progn header is deallocated and
                 the action is returned.  If there are
                 no actions, the progn expression is
                 modified to be the FALSE symbol
                 and returned.  Otherwise, the progn
                 is simply returned.
  INPUTS       : The action expression
  RETURNS      : The compacted expression
  SIDE EFFECTS : Some expressions possibly deallocated
  NOTES        : Assumes actions is a progn expression
                 and actions->nextArg == NULL
 *****************************************************/
static EXPRESSION *CompactActions(
  void *theEnv,
  EXEC_STATUS,
  EXPRESSION *actions)
  {
   register struct expr *tmp;

   if (actions->argList == NULL)
     {
      actions->type = SYMBOL;
      actions->value = EnvFalseSymbol(theEnv,execStatus);
     }
   else if (actions->argList->nextArg == NULL)
     {
      tmp = actions;
      actions = actions->argList;
      rtn_struct(theEnv,execStatus,expr,tmp);
     }
   return(actions);
  }

#endif

#if (! DEFFUNCTION_CONSTRUCT) || (! DEFGENERIC_CONSTRUCT)

/******************************************************
  NAME         : EvaluateBadCall
  DESCRIPTION  : Default evaluation function for
                 deffunctions and gneric functions
                 in configurations where either
                 capability is not present.
  INPUTS       : 1) The function (ignored)
                 2) A data object buffer for the result
  RETURNS      : FALSE
  SIDE EFFECTS : Data object buffer set to the
                 symbol FALSE and evaluation error set
  NOTES        : Used for binary images which
                 contain deffunctions and generic
                 functions which cannot be used
 ******************************************************/
#if WIN_BTC
#pragma argsused
#endif
static intBool EvaluateBadCall(
  void *theEnv,
  EXEC_STATUS,
  void *value,
  DATA_OBJECT *result)
  {
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(value)
#endif
   PrintErrorID(theEnv,execStatus,"PRCCODE",1,FALSE);
   EnvPrintRouter(theEnv,execStatus,WERROR,"Attempted to call a deffunction/generic function ");
   EnvPrintRouter(theEnv,execStatus,WERROR,"which does not exist.\n");
   SetEvaluationError(theEnv,execStatus,TRUE);
   SetpType(result,SYMBOL);
   SetpValue(result,EnvFalseSymbol(theEnv,execStatus));
   return(FALSE);
  }

#endif


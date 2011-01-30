   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/09/97          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/***************************************************************/
/* Purpose: Procedural Code Support Routines for Deffunctions, */
/*          Generic Function Methods,Message-Handlers          */
/*          and Rules                                          */
/*                                                             */
/* Principal Programmer(s):                                    */
/*      Brian L. Donnell                                       */
/*                                                             */
/* Contributing Programmer(s):                                 */
/*                                                             */
/* Revision History:                                           */
/*                                                             */
/* Who               |     Date    | Description               */
/* ------------------+-------------+------------------------   */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS         */
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
                   CONSTANTS
   =========================================
   ***************************************** */

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

typedef struct ProcParamStack
  {
   DATA_OBJECT *ParamArray;

#if DEFGENERIC_CONSTRUCT
   EXPRESSION *ParamExpressions;
#endif

   int ParamArraySize;
   DATA_OBJECT *WildcardValue;
   void (*UnboundErrFunc)(void);
   struct ProcParamStack *nxt;
  } PROC_PARAM_STACK;

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

static void EvaluateProcParameters(EXPRESSION *,int,char *,char *);
static BOOLEAN RtnProcParam(void *,DATA_OBJECT *);
static BOOLEAN GetProcBind(void *,DATA_OBJECT *);
static BOOLEAN PutProcBind(void *,DATA_OBJECT *);
static BOOLEAN RtnProcWild(void *,DATA_OBJECT *);

#if (! BLOAD_ONLY) && (! RUN_TIME)
static int FindProcParameter(SYMBOL_HN *,EXPRESSION *,SYMBOL_HN *);
static int ReplaceProcBinds(EXPRESSION *,int (*)(EXPRESSION *,void *),void *);
static EXPRESSION *CompactActions(EXPRESSION *);
#endif

#if (! DEFFUNCTION_CONSTRUCT) || (! DEFGENERIC_CONSTRUCT)
static BOOLEAN EvaluateBadCall(void *,DATA_OBJECT *);
#endif

/* =========================================
   *****************************************
      INTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */
Thread globle void * NoParamValue = NULL;
Thread globle DATA_OBJECT *ProcParamArray = NULL;
Thread globle int ProcParamArraySize = 0;
Thread globle EXPRESSION * CurrentProcActions = NULL;

/* =========================================
   *****************************************
      INTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */
#if DEFGENERIC_CONSTRUCT
Thread static EXPRESSION *ProcParamExpressions = NULL;
#endif

Thread static PROC_PARAM_STACK *pstack = NULL;
Thread static DATA_OBJECT *WildcardValue = NULL,
                          *LocalVarArray = NULL;
Thread static void (*ProcUnboundErrFunc)(void) = NULL;

Thread static ENTITY_RECORD ProcParameterInfo = { "PROC_PARAM", PROC_PARAM,0,1,0,NULL,NULL,NULL,
                                           RtnProcParam,NULL,NULL,NULL,NULL,NULL,NULL },
                     ProcWildInfo =      { "PROC_WILD_PARAM", PROC_WILD_PARAM,0,1,0,NULL,NULL,NULL,
                                           RtnProcWild,NULL,NULL,NULL,NULL,NULL,NULL },
                     ProcGetInfo =       { "PROC_GET_BIND", PROC_GET_BIND,0,1,0,NULL,NULL,NULL,
                                           GetProcBind,NULL,NULL,NULL,NULL,NULL,NULL },
                     ProcBindInfo =      { "PROC_BIND", PROC_BIND,0,1,0,NULL,NULL,NULL,
                                           PutProcBind,NULL,NULL,NULL,NULL,NULL,NULL };

#if ! DEFFUNCTION_CONSTRUCT

Thread static ENTITY_RECORD DeffunctionEntityRecord =
                     { "PCALL", PCALL,0,0,1,
                       NULL,NULL,NULL,
                       EvaluateBadCall,
                       NULL,NULL,NULL,NULL,NULL,NULL,NULL };

#endif

#if ! DEFGENERIC_CONSTRUCT

Thread static ENTITY_RECORD GenericEntityRecord =
                     { "GCALL", GCALL,0,0,1,
                       NULL,NULL,NULL,
                       EvaluateBadCall,
                       NULL,NULL,NULL,NULL,NULL,NULL,NULL };

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
globle void InstallProcedurePrimitives()
  {
   InstallPrimitive(&ProcParameterInfo,PROC_PARAM);
   InstallPrimitive(&ProcWildInfo,PROC_WILD_PARAM);
   InstallPrimitive(&ProcGetInfo,PROC_GET_BIND);
   InstallPrimitive(&ProcBindInfo,PROC_BIND);

   /* ===============================================
      Make sure a default evaluation function is
      in place for deffunctions and generic functions
      in the event that a binary image containing
      these items is loaded into a configuration
      that does not support them.
      =============================================== */

#if ! DEFFUNCTION_CONSTRUCT
   InstallPrimitive(&DeffunctionEntityRecord,PCALL);
#endif

#if ! DEFGENERIC_CONSTRUCT
   InstallPrimitive(&GenericEntityRecord,GCALL);
#endif

   /* =============================================
      Install the special empty multifield to
      let callers distinguish between no parameters
      and zero-length multifield parameters
      ============================================= */
   NoParamValue = CreateMultifield2(0L);
   MultifieldInstall((MULTIFIELD_PTR) NoParamValue);
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
  char *readSource,
  struct token *tkn,
  EXPRESSION *parameterList,
  SYMBOL_HN **wildcard,
  int *min,
  int *max,
  int *error,
  int (*checkfunc)(char *))
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
      SyntaxErrorMessage("parameter list");
      ReturnExpression(parameterList);
      return(NULL);
     }
   GetToken(readSource,tkn);
   while ((tkn->type == SF_VARIABLE) || (tkn->type == MF_VARIABLE))
     {
      for (check = parameterList ; check != NULL ; check = check->nextArg)
        if (check->value == tkn->value)
         {
          PrintErrorID("PRCCODE",7,FALSE);
          PrintRouter(WERROR,"Duplicate parameter names not allowed.\n");
          ReturnExpression(parameterList);
          return(NULL);
         }
      if (*wildcard != NULL)
        {
         PrintErrorID("PRCCODE",8,FALSE);
         PrintRouter(WERROR,"No parameters allowed after wildcard parameter.\n");
         ReturnExpression(parameterList);
         return(NULL);
        }
      if ((checkfunc != NULL) ? (*checkfunc)(ValueToString(tkn->value)) : FALSE)
        {
         ReturnExpression(parameterList);
         return(NULL);
        }
      nextOne = GenConstant(tkn->type,tkn->value);
      if (tkn->type == MF_VARIABLE)
        *wildcard = (SYMBOL_HN *) tkn->value;
      else
        (*min)++;
      if (lastOne == NULL)
        { parameterList = nextOne; }
      else
        { lastOne->nextArg = nextOne; }
      lastOne = nextOne;
      SavePPBuffer(" ");
      paramprintp = 1;
      GetToken(readSource,tkn);
     }
   if (tkn->type != RPAREN)
     {
      SyntaxErrorMessage("parameter list");
      ReturnExpression(parameterList);
      return(NULL);
     }
   if (paramprintp)
     {
      PPBackup();
      PPBackup();
      SavePPBuffer(")");
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
  char *bodytype,
  char *readSource,
  struct token *tkn,
  EXPRESSION *params,
  SYMBOL_HN *wildcard,
  int (*altvarfunc)(EXPRESSION *,void *),
  int (*altbindfunc)(EXPRESSION *,void *),
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
   ClearParsedBindNames();
   actions = GroupActions(readSource,tkn,TRUE,NULL,FALSE);
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
      if (ReplaceProcBinds(actions,altbindfunc,userBuffer))
        {
         ClearParsedBindNames();
         ReturnExpression(actions);
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
   *lvarcnt = CountParsedBindNames();
   if (ReplaceProcVars(bodytype,actions,params,wildcard,altvarfunc,userBuffer))
     {
      ClearParsedBindNames();
      ReturnExpression(actions);
      return(NULL);
     }

   /* =======================================================================
      Normally, actions are grouped in a progn.  If there is only one action,
      the progn is unnecessary and can be removed.  Also, the actions are
      packed into a contiguous array to save on memory overhead.  The
      intermediate parsed bind names are freed to avoid tying up memory.
      ======================================================================= */
   actions = CompactActions(actions);
   pactions = PackExpression(actions);
   ReturnExpression(actions);
   ClearParsedBindNames();
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
                    ClearParsedBindNames() before the parse of the body
                    in which variables are being replaced.
 *************************************************************************/
globle int ReplaceProcVars(
  char *bodytype,
  EXPRESSION *actions,
  EXPRESSION *parameterList,
  SYMBOL_HN *wildcard,
  int (*altvarfunc)(EXPRESSION *,void *),
  void *specdata)
  {
   int position,altcode;
   BOOLEAN boundPosn;
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
         boundPosn = SearchParsedBindNames(bindName);

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
            if ((altvarfunc != NULL) ? ((*altvarfunc)(actions,specdata) != 1) : TRUE)
              {
               PrintErrorID("PRCCODE",3,TRUE);
               PrintRouter(WERROR,"Undefined variable ");
               PrintRouter(WERROR,ValueToString(bindName));
               PrintRouter(WERROR," referenced in ");
               PrintRouter(WERROR,bodytype);
               PrintRouter(WERROR,".\n");
               return(TRUE);
              }
           }

         /*===================================================*/
         /* Else if variable is defined in the parameter list */
         /* and not rebound within the procedure then...      */
         /*===================================================*/

         else if ((position > 0) && (boundPosn == 0))
           {
            actions->type = (short) ((bindName != wildcard) ? PROC_PARAM : PROC_WILD_PARAM);
            actions->value = AddBitMap((void *) &position,(int) sizeof(int));
           }

         /*=========================================================*/
         /* Else the variable is rebound within the procedure so... */
         /*=========================================================*/

         else
           {
            if (altvarfunc != NULL)
              {
               altvarexp = GenConstant(actions->type,actions->value);
               altcode = (*altvarfunc)(altvarexp,specdata);
               if (altcode == 0)
                 {
                  rtn_struct(expr,altvarexp);
                  altvarexp = NULL;
                 }
               else if (altcode == -1)
                 {
                  rtn_struct(expr,altvarexp);
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
            actions->value = AddBitMap((void *) &pvar,(int) sizeof(PACKED_PROC_VAR));
            actions->argList = GenConstant(SYMBOL,(void *) bindName);
            actions->argList->nextArg = altvarexp;
           }
        }
#if DEFGLOBAL_CONSTRUCT
      else if (actions->type == GBL_VARIABLE)
        {
         if (ReplaceGlobalVariable(actions) == FALSE)
           return(-1);
        }
#endif
      if ((altvarfunc != NULL) ? ((*altvarfunc)(actions,specdata) == -1) : FALSE)
        return(TRUE);
      if (actions->argList != NULL)
        {
         if (ReplaceProcVars(bodytype,actions->argList,parameterList,
                                        wildcard,altvarfunc,specdata))
           return(TRUE);

         /* ====================================================================
            Check to see if this is a call to the bind function.  If so (and the
            second argument is a symbol) then it is a locally bound variable
            (as opposed to a global).

            Replace the call to "bind" with a call to PROC_BIND - the
            special internal function for procedure local variables.
            ==================================================================== */
         if ((actions->value == (void *) FindFunction("bind")) &&
             (actions->argList->type == SYMBOL))
           {
            actions->type = PROC_BIND;
            boundPosn = SearchParsedBindNames((SYMBOL_HN *) actions->argList->value);
            actions->value = AddBitMap((void *) &boundPosn,(int) sizeof(BOOLEAN));
            arg_lvl = actions->argList->nextArg;
            rtn_struct(expr,actions->argList);
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
  int index)
  {
   return(GenConstant(PROC_WILD_PARAM,AddBitMap((void *) &index,(int) sizeof(int))));
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
  EXPRESSION *parameterList,
  int numberOfParameters,
  char *pname,
  char *bodytype,
  void (*UnboundErrFunc)(void))
  {
   register PROC_PARAM_STACK *ptmp;

   ptmp = get_struct(ProcParamStack);
   ptmp->ParamArray = ProcParamArray;
   ptmp->ParamArraySize = ProcParamArraySize;
   ptmp->UnboundErrFunc = ProcUnboundErrFunc;
   ptmp->nxt = pstack;
   pstack = ptmp;
   EvaluateProcParameters(parameterList,numberOfParameters,pname,bodytype);
   if (EvaluationError)
     {
      ptmp = pstack;
      pstack = pstack->nxt;
      rtn_struct(ProcParamStack,ptmp);
      return;
     }

   /* ================================================================
      Record ProcParamExpressions and WildcardValue for previous frame
      AFTER evaluating arguments for the new frame, because they could
      have gone from NULL to non-NULL (if they were already non-NULL,
      they would remain unchanged.)
      ================================================================ */
#if DEFGENERIC_CONSTRUCT
   ptmp->ParamExpressions = ProcParamExpressions;
   ProcParamExpressions = NULL;
#endif
   ptmp->WildcardValue = WildcardValue;
   WildcardValue = NULL;
   ProcUnboundErrFunc = UnboundErrFunc;
  }

/******************************************************************
  NAME         : PopProcParameters
  DESCRIPTION  : Restores old procedure arrays
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Stack popped and globals restored
  NOTES        : Assumes pstack != NULL
 ******************************************************************/
globle void PopProcParameters()
  {
   register PROC_PARAM_STACK *ptmp;

   if (ProcParamArray != NULL)
     rm((void *) ProcParamArray,(int) (sizeof(DATA_OBJECT) * ProcParamArraySize));

#if DEFGENERIC_CONSTRUCT
   if (ProcParamExpressions != NULL)
     rm((void *) ProcParamExpressions,(int) (sizeof(EXPRESSION) * ProcParamArraySize));
#endif

   ptmp = pstack;
   pstack = pstack->nxt;
   ProcParamArray = ptmp->ParamArray;
   ProcParamArraySize = ptmp->ParamArraySize;

#if DEFGENERIC_CONSTRUCT
   ProcParamExpressions = ptmp->ParamExpressions;
#endif

   if (WildcardValue != NULL)
     {
      MultifieldDeinstall((MULTIFIELD_PTR) WildcardValue->value);
      if (WildcardValue->value != NoParamValue)
        AddToMultifieldList((MULTIFIELD_PTR) WildcardValue->value);
      rtn_struct(dataObject,WildcardValue);
     }
   WildcardValue = ptmp->WildcardValue;
   ProcUnboundErrFunc = ptmp->UnboundErrFunc;
   rtn_struct(ProcParamStack,ptmp);
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
globle EXPRESSION *GetProcParamExpressions()
  {
   register int i;

   if ((ProcParamArray == NULL) || (ProcParamExpressions != NULL))
     return(ProcParamExpressions);
   ProcParamExpressions = (EXPRESSION *)
               gm2((int) (sizeof(EXPRESSION) * ProcParamArraySize));
   for (i = 0 ; i < ProcParamArraySize ; i++)
     {
      ProcParamExpressions[i].type = (short) ProcParamArray[i].type;
      if (ProcParamArray[i].type != MULTIFIELD)
        ProcParamExpressions[i].value = ProcParamArray[i].value;
      else
        ProcParamExpressions[i].value = (void *) &ProcParamArray[i];
      ProcParamExpressions[i].argList = NULL;
      ProcParamExpressions[i].nextArg =
        ((i + 1) != ProcParamArraySize) ? &ProcParamExpressions[i+1] : NULL;
     }
   return(ProcParamExpressions);
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
  struct defmodule *theModule,
  EXPRESSION *actions,
  int lvarcnt,
  DATA_OBJECT *result,
  void (*crtproc)(void))
  {
   DATA_OBJECT *oldLocalVarArray;
   register int i;
   struct defmodule *oldModule;
   EXPRESSION *oldActions;

   oldLocalVarArray = LocalVarArray;
   LocalVarArray = (lvarcnt == 0) ? NULL :
                   (DATA_OBJECT *) gm2((int) (sizeof(DATA_OBJECT) * lvarcnt));
   for (i = 0 ; i < lvarcnt ; i++)
     LocalVarArray[i].supplementalInfo = FalseSymbol;

   oldModule = ((struct defmodule *) GetCurrentModule());
   if (oldModule != theModule)
     SetCurrentModule((void *) theModule);
   oldActions = CurrentProcActions;
   CurrentProcActions = actions;

   if (EvaluateExpression(actions,result))
     {
      result->type = SYMBOL;
      result->value = FalseSymbol;
     }

   CurrentProcActions = oldActions;
   if (oldModule != ((struct defmodule *) GetCurrentModule()))
     SetCurrentModule((void *) oldModule);
   if ((crtproc != NULL) ? HaltExecution : FALSE)
     {
      PrintErrorID("PRCCODE",4,FALSE);
      PrintRouter(WERROR,"Execution halted during the actions of ");
      (*crtproc)();
     }
   if ((WildcardValue != NULL) ? (result->value == WildcardValue->value) : FALSE)
     {
      MultifieldDeinstall((MULTIFIELD_PTR) WildcardValue->value);
      if (WildcardValue->value != NoParamValue)
        AddToMultifieldList((MULTIFIELD_PTR) WildcardValue->value);
      rtn_struct(dataObject,WildcardValue);
      WildcardValue = NULL;
     }

   if (lvarcnt != 0)
     {
      for (i = 0 ; i < lvarcnt ; i++)
        if (LocalVarArray[i].supplementalInfo == TrueSymbol)
          ValueDeinstall(&LocalVarArray[i]);
      rm((void *) LocalVarArray,(int) (sizeof(DATA_OBJECT) * lvarcnt));
     }

   LocalVarArray = oldLocalVarArray;
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
  char *log)
  {
   register int i;

   PrintRouter(log," (");
   for (i = 0 ; i < ProcParamArraySize ; i++)
     {
      PrintDataObject(log,&ProcParamArray[i]);
      if (i != ProcParamArraySize-1)
        PrintRouter(log," ");
     }
   PrintRouter(log,")\n");
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
  DATA_OBJECT *result,
  int index)
  {
   register int i,j;
   long k; /* 6.04 Bug Fix */
   long size;
   DATA_OBJECT *val;
   Thread static int oldindex = -1;

   result->type = MULTIFIELD;
   result->begin = 0;
   if (WildcardValue == NULL)
     {
      WildcardValue = get_struct(dataObject);
      WildcardValue->begin = 0;
     }
   else if (index == oldindex)
     {
      result->end = WildcardValue->end;
      result->value = WildcardValue->value;
      return;
     }
   else
     {
      MultifieldDeinstall((MULTIFIELD_PTR) WildcardValue->value);
      if (WildcardValue->value != NoParamValue)
        AddToMultifieldList((MULTIFIELD_PTR) WildcardValue->value);
     }
   oldindex = index;
   size = ProcParamArraySize - index + 1;
   if (size <= 0)
     {
      result->end = WildcardValue->end = -1;
      result->value = WildcardValue->value = NoParamValue;
      MultifieldInstall((MULTIFIELD_PTR) WildcardValue->value);
      return;
     }
   for (i = index-1 ; i < ProcParamArraySize ; i++)
     {
      if (ProcParamArray[i].type == MULTIFIELD)
        size += ProcParamArray[i].end - ProcParamArray[i].begin;
     }
   result->end = WildcardValue->end = size-1;
   result->value = WildcardValue->value = (void *) CreateMultifield2(size);
   for (i = index-1 , j = 1 ; i < ProcParamArraySize ; i++)
     {
      if (ProcParamArray[i].type != MULTIFIELD)
        {
         SetMFType(result->value,j,(short) ProcParamArray[i].type);
         SetMFValue(result->value,j,ProcParamArray[i].value);
         j++;
        }
      else
        {
         val = &ProcParamArray[i];
         for (k = val->begin + 1 ; k <= val->end + 1 ; k++ , j++)
           {
            SetMFType(result->value,j,GetMFType(val->value,k));
            SetMFValue(result->value,j,GetMFValue(val->value,k));
           }
        }
     }
   MultifieldInstall((MULTIFIELD_PTR) WildcardValue->value);
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
  EXPRESSION *parameterList,
  int numberOfParameters,
  char *pname,
  char *bodytype)
  {
   DATA_OBJECT *rva,temp;
   int i = 0;

   if (numberOfParameters == 0)
     {
      ProcParamArray = NULL;
      ProcParamArraySize = 0;
      return;
     }

   rva = (DATA_OBJECT *) gm2((int) (sizeof(DATA_OBJECT) * numberOfParameters));
   while (parameterList != NULL)
     {
      if ((EvaluateExpression(parameterList,&temp) == TRUE) ? TRUE :
          (temp.type == RVOID))
        {
         if (temp.type == RVOID)
           {
            PrintErrorID("PRCCODE",2,FALSE);
            PrintRouter(WERROR,"Functions without a return value are illegal as ");
            PrintRouter(WERROR,bodytype);
            PrintRouter(WERROR," arguments.\n");
            SetEvaluationError(TRUE);
           }
         PrintErrorID("PRCCODE",6,FALSE);
         PrintRouter(WERROR,"This error occurred while evaluating arguments ");
         PrintRouter(WERROR,"for the ");
         PrintRouter(WERROR,bodytype);
         PrintRouter(WERROR," ");
         PrintRouter(WERROR,pname);
         PrintRouter(WERROR,".\n");
         rm((void *) rva,(int) (sizeof(DATA_OBJECT) * numberOfParameters));
         return;
        }
      rva[i].type = temp.type;
      rva[i].value = temp.value;
      rva[i].begin = temp.begin;
      rva[i].end = temp.end;
      parameterList = parameterList->nextArg;
      i++;
     }
   ProcParamArraySize = numberOfParameters;
   ProcParamArray = rva;
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
static BOOLEAN RtnProcParam(
  void *value,
  DATA_OBJECT *result)
  {
   register DATA_OBJECT *src;

   src = &ProcParamArray[*((int *) ValueToBitMap(value)) - 1];
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
static BOOLEAN GetProcBind(
  void *value,
  DATA_OBJECT *result)
  {
   register DATA_OBJECT *src;
   PACKED_PROC_VAR *pvar;

   pvar = (PACKED_PROC_VAR *) ValueToBitMap(value);
   src = &LocalVarArray[pvar->first - 1];
   if (src->supplementalInfo == TrueSymbol)
     {
      result->type = src->type;
      result->value = src->value;
      result->begin = src->begin;
      result->end = src->end;
      return(TRUE);
     }
   if (GetFirstArgument()->nextArg != NULL)
     {
      EvaluateExpression(GetFirstArgument()->nextArg,result);
      return(TRUE);
     }
   if (pvar->second == 0)
     {
      PrintErrorID("PRCCODE",5,FALSE);
      SetEvaluationError(TRUE);
      PrintRouter(WERROR,"Variable ");
      PrintRouter(WERROR,ValueToString(GetFirstArgument()->value));
      if (ProcUnboundErrFunc != NULL)
        {
         PrintRouter(WERROR," unbound in ");
         (*ProcUnboundErrFunc)();
        }
      else
        PrintRouter(WERROR," unbound.\n");
      result->type = SYMBOL;
      result->value = FalseSymbol;
      return(TRUE);
     }
   if (pvar->secondFlag == 0)
     {
      src = &ProcParamArray[pvar->second - 1];
      result->type = src->type;
      result->value = src->value;
      result->begin = src->begin;
      result->end = src->end;
     }
   else
     GrabProcWildargs(result,(int) pvar->second);
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
static BOOLEAN PutProcBind(
  void *value,
  DATA_OBJECT *result)
  {
   register DATA_OBJECT *dst;

   dst = &LocalVarArray[*((int *) ValueToBitMap(value)) - 1];
   if (GetFirstArgument() == NULL)
     {
      if (dst->supplementalInfo == TrueSymbol)
        ValueDeinstall(dst);
      dst->supplementalInfo = FalseSymbol;
      result->type = SYMBOL;
      result->value = FalseSymbol;
     }
   else
     {
      if (GetFirstArgument()->nextArg != NULL)
        StoreInMultifield(result,GetFirstArgument(),TRUE);
      else
        EvaluateExpression(GetFirstArgument(),result);
      if (dst->supplementalInfo == TrueSymbol)
        ValueDeinstall(dst);
      dst->supplementalInfo = TrueSymbol;
      dst->type = result->type;
      dst->value = result->value;
      dst->begin = result->begin;
      dst->end = result->end;
      ValueInstall(dst);
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
static BOOLEAN RtnProcWild(
  void *value,
  DATA_OBJECT *result)
  {
   GrabProcWildargs(result,*(int *) ValueToBitMap(value));
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
  EXPRESSION *actions,
  int (*altbindfunc)(EXPRESSION *,void *),
  void *userBuffer)
  {
   int bcode;
   SYMBOL_HN *bname;

   while (actions != NULL)
     {
      if (actions->argList != NULL)
        {
         if (ReplaceProcBinds(actions->argList,altbindfunc,userBuffer))
           return(TRUE);
         if ((actions->value == (void *) FindFunction("bind")) &&
             (actions->argList->type == SYMBOL))
           {
            bname = (SYMBOL_HN *) actions->argList->value;
            bcode = (*altbindfunc)(actions,userBuffer);
            if (bcode == -1)
              return(TRUE);
            if (bcode == 1)
              RemoveParsedBindName(bname);
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
  EXPRESSION *actions)
  {
   register struct expr *tmp;

   if (actions->argList == NULL)
     {
      actions->type = SYMBOL;
      actions->value = FalseSymbol;
     }
   else if (actions->argList->nextArg == NULL)
     {
      tmp = actions;
      actions = actions->argList;
      rtn_struct(expr,tmp);
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
static BOOLEAN EvaluateBadCall(
  void *value,
  DATA_OBJECT *result)
  {
   PrintErrorID("PRCCODE",1,FALSE);
   PrintRouter(WERROR,"Attempted to call a deffunction/generic function ");
   PrintRouter(WERROR,"which does not exist.\n");
   SetEvaluationError(TRUE);
   SetpType(result,SYMBOL);
   SetpValue(result,FalseSymbol);
   return(FALSE);
  }

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
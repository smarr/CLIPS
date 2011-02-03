   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*              CLIPS Version 6.24  06/05/06           */
   /*                                                     */
   /*                INSTANCE PARSER MODULE               */
   /*******************************************************/

/*************************************************************/
/* Purpose:  Instance Function Parsing Routines              */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*            Changed name of variable exp to theExp         */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "setup.h"

#if OBJECT_SYSTEM

#ifndef _STDIO_INCLUDED_
#define _STDIO_INCLUDED_
#include <stdio.h>
#endif

#include <string.h>

#include "classcom.h"
#include "classfun.h"
#include "classinf.h"
#include "constant.h"
#include "envrnmnt.h"
#include "evaluatn.h"
#include "exprnpsr.h"
#include "extnfunc.h"
#include "moduldef.h"
#include "prntutil.h"
#include "router.h"

#define _INSPSR_SOURCE_
#include "inspsr.h"

/* =========================================
   *****************************************
                   CONSTANTS
   =========================================
   ***************************************** */
#define MAKE_TYPE       0
#define INITIALIZE_TYPE 1
#define MODIFY_TYPE     2
#define DUPLICATE_TYPE  3

#define CLASS_RLN          "of"
#define DUPLICATE_NAME_REF "to"

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

static intBool ReplaceClassNameWithReference(void *,EXEC_STATUS,EXPRESSION *);

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

#if ! RUN_TIME

/*************************************************************************************
  NAME         : ParseInitializeInstance
  DESCRIPTION  : Parses initialize-instance and make-instance function
                   calls into an EXPRESSION form that
                   can later be evaluated with EvaluateExpression(theEnv,execStatus,)
  INPUTS       : 1) The address of the top node of the expression
                    containing the initialize-instance function call
                 2) The logical name of the input source
  RETURNS      : The address of the modified expression, or NULL
                    if there is an error
  SIDE EFFECTS : The expression is enhanced to include all
                    aspects of the initialize-instance call
                    (slot-overrides etc.)
                 The "top" expression is deleted on errors.
  NOTES        : This function parses a initialize-instance call into
                 an expression of the following form :

                 (initialize-instance <instance-name> <slot-override>*)
                  where <slot-override> ::= (<slot-name> <expression>+)

                  goes to -->

                  initialize-instance
                      |
                      V
                  <instance or name>-><slot-name>-><dummy-node>...
                                                      |
                                                      V
                                               <value-expression>...

                  (make-instance <instance> of <class> <slot-override>*)
                  goes to -->

                  make-instance
                      |
                      V
                  <instance-name>-><class-name>-><slot-name>-><dummy-node>...
                                                                 |
                                                                 V
                                                          <value-expression>...

                  (make-instance of <class> <slot-override>*)
                  goes to -->

                  make-instance
                      |
                      V
                  (gensym*)-><class-name>-><slot-name>-><dummy-node>...
                                                                 |
                                                                 V
                                                          <value-expression>...

                  (modify-instance <instance> <slot-override>*)
                  goes to -->

                  modify-instance
                      |
                      V
                  <instance or name>-><slot-name>-><dummy-node>...
                                                      |
                                                      V
                                               <value-expression>...

                  (duplicate-instance <instance> [to <new-name>] <slot-override>*)
                  goes to -->

                  duplicate-instance
                      |
                      V
                  <instance or name>-><new-name>-><slot-name>-><dummy-node>...
                                          OR                         |
                                      (gensym*)                      V
                                                           <value-expression>...

 *************************************************************************************/
globle EXPRESSION *ParseInitializeInstance(
  void *theEnv,
  EXEC_STATUS,
  EXPRESSION *top,
  char *readSource)
  {
   int error,fcalltype,readclass;

   if ((top->value == (void *) FindFunction(theEnv,execStatus,"make-instance")) ||
       (top->value == (void *) FindFunction(theEnv,execStatus,"active-make-instance")))
     fcalltype = MAKE_TYPE;
   else if ((top->value == (void *) FindFunction(theEnv,execStatus,"initialize-instance")) ||
            (top->value == (void *) FindFunction(theEnv,execStatus,"active-initialize-instance")))
     fcalltype = INITIALIZE_TYPE;
   else if ((top->value == (void *) FindFunction(theEnv,execStatus,"modify-instance")) ||
            (top->value == (void *) FindFunction(theEnv,execStatus,"active-modify-instance")) ||
            (top->value == (void *) FindFunction(theEnv,execStatus,"message-modify-instance")) ||
            (top->value == (void *) FindFunction(theEnv,execStatus,"active-message-modify-instance")))
     fcalltype = MODIFY_TYPE;
   else
     fcalltype = DUPLICATE_TYPE;
   IncrementIndentDepth(theEnv,execStatus,3);
   error = FALSE;
   if (top->type == UNKNOWN_VALUE)
     top->type = FCALL;
   else
     SavePPBuffer(theEnv,execStatus," ");
   top->argList = ArgumentParse(theEnv,execStatus,readSource,&error);
   if (error)
     goto ParseInitializeInstanceError;
   else if (top->argList == NULL)
     {
      SyntaxErrorMessage(theEnv,execStatus,"instance");
      goto ParseInitializeInstanceError;
     }
   SavePPBuffer(theEnv,execStatus," ");

   if (fcalltype == MAKE_TYPE)
     {
      /* ======================================
         Handle the case of anonymous instances
         where the name was not specified
         ====================================== */
      if ((top->argList->type != SYMBOL) ? FALSE :
          (strcmp(ValueToString(top->argList->value),CLASS_RLN) == 0))
        {
         top->argList->nextArg = ArgumentParse(theEnv,execStatus,readSource,&error);
         if (error == TRUE)
           goto ParseInitializeInstanceError;
         if (top->argList->nextArg == NULL)
           {
            SyntaxErrorMessage(theEnv,execStatus,"instance class");
            goto ParseInitializeInstanceError;
           }
         if ((top->argList->nextArg->type != SYMBOL) ? TRUE :
             (strcmp(ValueToString(top->argList->nextArg->value),CLASS_RLN) != 0))
           {
            top->argList->type = FCALL;
            top->argList->value = (void *) FindFunction(theEnv,execStatus,"gensym*");
            readclass = FALSE;
           }
         else
           readclass = TRUE;
        }
      else
        {
         GetToken(theEnv,execStatus,readSource,&DefclassData(theEnv,execStatus)->ObjectParseToken);
         if ((GetType(DefclassData(theEnv,execStatus)->ObjectParseToken) != SYMBOL) ? TRUE :
             (strcmp(CLASS_RLN,DOToString(DefclassData(theEnv,execStatus)->ObjectParseToken)) != 0))
           {
            SyntaxErrorMessage(theEnv,execStatus,"make-instance");
            goto ParseInitializeInstanceError;
           }
         SavePPBuffer(theEnv,execStatus," ");
         readclass = TRUE;
        }
      if (readclass)
        {
         top->argList->nextArg = ArgumentParse(theEnv,execStatus,readSource,&error);
         if (error)
           goto ParseInitializeInstanceError;
         if (top->argList->nextArg == NULL)
           {
            SyntaxErrorMessage(theEnv,execStatus,"instance class");
            goto ParseInitializeInstanceError;
           }
        }

      /* ==============================================
         If the class name is a constant, go ahead and
         look it up now and replace it with the pointer
         ============================================== */
      if (ReplaceClassNameWithReference(theEnv,execStatus,top->argList->nextArg) == FALSE)
        goto ParseInitializeInstanceError;

      PPCRAndIndent(theEnv,execStatus);
      GetToken(theEnv,execStatus,readSource,&DefclassData(theEnv,execStatus)->ObjectParseToken);
      top->argList->nextArg->nextArg =
                  ParseSlotOverrides(theEnv,execStatus,readSource,&error);
     }
   else
     {
      PPCRAndIndent(theEnv,execStatus);
      GetToken(theEnv,execStatus,readSource,&DefclassData(theEnv,execStatus)->ObjectParseToken);
      if (fcalltype == DUPLICATE_TYPE)
        {
         if ((DefclassData(theEnv,execStatus)->ObjectParseToken.type != SYMBOL) ? FALSE :
             (strcmp(DOToString(DefclassData(theEnv,execStatus)->ObjectParseToken),DUPLICATE_NAME_REF) == 0))
           {
            PPBackup(theEnv,execStatus);
            PPBackup(theEnv,execStatus);
            SavePPBuffer(theEnv,execStatus,DefclassData(theEnv,execStatus)->ObjectParseToken.printForm);
            SavePPBuffer(theEnv,execStatus," ");
            top->argList->nextArg = ArgumentParse(theEnv,execStatus,readSource,&error);
            if (error)
              goto ParseInitializeInstanceError;
            if (top->argList->nextArg == NULL)
              {
               SyntaxErrorMessage(theEnv,execStatus,"instance name");
               goto ParseInitializeInstanceError;
              }
            PPCRAndIndent(theEnv,execStatus);
            GetToken(theEnv,execStatus,readSource,&DefclassData(theEnv,execStatus)->ObjectParseToken);
           }
         else
           top->argList->nextArg = GenConstant(theEnv,execStatus,FCALL,(void *) FindFunction(theEnv,execStatus,"gensym*"));
         top->argList->nextArg->nextArg = ParseSlotOverrides(theEnv,execStatus,readSource,&error);
        }
      else
        top->argList->nextArg = ParseSlotOverrides(theEnv,execStatus,readSource,&error);
     }
   if (error)
      goto ParseInitializeInstanceError;
   if (GetType(DefclassData(theEnv,execStatus)->ObjectParseToken) != RPAREN)
     {
      SyntaxErrorMessage(theEnv,execStatus,"slot-override");
      goto ParseInitializeInstanceError;
     }
   DecrementIndentDepth(theEnv,execStatus,3);
   return(top);

ParseInitializeInstanceError:
   SetEvaluationError(theEnv,execStatus,TRUE);
   ReturnExpression(theEnv,execStatus,top);
   DecrementIndentDepth(theEnv,execStatus,3);
   return(NULL);
  }

/********************************************************************************
  NAME         : ParseSlotOverrides
  DESCRIPTION  : Forms expressions for slot-overrides
  INPUTS       : 1) The logical name of the input
                 2) Caller's buffer for error flkag
  RETURNS      : Address override expressions, NULL
                   if none or error.
  SIDE EFFECTS : Slot-expression built
                 Caller's error flag set
  NOTES        : <slot-override> ::= (<slot-name> <value>*)*

                 goes to

                 <slot-name> --> <dummy-node> --> <slot-name> --> <dummy-node>...
                                       |
                                       V
                               <value-expression> --> <value-expression> --> ...

                 Assumes first token has already been scanned
 ********************************************************************************/
globle EXPRESSION *ParseSlotOverrides(
  void *theEnv,
  EXEC_STATUS,
  char *readSource,
  int *error)
  {
   EXPRESSION *top = NULL,*bot = NULL,*theExp;

   while (GetType(DefclassData(theEnv,execStatus)->ObjectParseToken) == LPAREN)
     {
      *error = FALSE;
      theExp = ArgumentParse(theEnv,execStatus,readSource,error);
      if (*error == TRUE)
        {
         ReturnExpression(theEnv,execStatus,top);
         return(NULL);
        }
      else if (theExp == NULL)
        {
         SyntaxErrorMessage(theEnv,execStatus,"slot-override");
         *error = TRUE;
         ReturnExpression(theEnv,execStatus,top);
         SetEvaluationError(theEnv,execStatus,TRUE);
         return(NULL);
        }
      theExp->nextArg = GenConstant(theEnv,execStatus,SYMBOL,EnvTrueSymbol(theEnv,execStatus));
      if (CollectArguments(theEnv,execStatus,theExp->nextArg,readSource) == NULL)
        {
         *error = TRUE;
         ReturnExpression(theEnv,execStatus,top);
         return(NULL);
        }
      if (top == NULL)
        top = theExp;
      else
        bot->nextArg = theExp;
      bot = theExp->nextArg;
      PPCRAndIndent(theEnv,execStatus);
      GetToken(theEnv,execStatus,readSource,&DefclassData(theEnv,execStatus)->ObjectParseToken);
     }
   PPBackup(theEnv,execStatus);
   PPBackup(theEnv,execStatus);
   SavePPBuffer(theEnv,execStatus,DefclassData(theEnv,execStatus)->ObjectParseToken.printForm);
   return(top);
  }

#endif

/****************************************************************************
  NAME         : ParseSimpleInstance
  DESCRIPTION  : Parses instances from file for load-instances
                   into an EXPRESSION forms that
                   can later be evaluated with EvaluateExpression(theEnv,execStatus,)
  INPUTS       : 1) The address of the top node of the expression
                    containing the make-instance function call
                 2) The logical name of the input source
  RETURNS      : The address of the modified expression, or NULL
                    if there is an error
  SIDE EFFECTS : The expression is enhanced to include all
                    aspects of the make-instance call
                    (slot-overrides etc.)
                 The "top" expression is deleted on errors.
  NOTES        : The name, class, values etc. must be constants.

                 This function parses a make-instance call into
                 an expression of the following form :

                  (make-instance <instance> of <class> <slot-override>*)
                  where <slot-override> ::= (<slot-name> <expression>+)

                  goes to -->

                  make-instance
                      |
                      V
                  <instance-name>-><class-name>-><slot-name>-><dummy-node>...
                                                                 |
                                                                 V
                                                          <value-expression>...

 ****************************************************************************/
globle EXPRESSION *ParseSimpleInstance(
  void *theEnv,
  EXEC_STATUS,
  EXPRESSION *top,
  char *readSource)
  {
   EXPRESSION *theExp,*vals = NULL,*vbot,*tval;
   unsigned short type;

   GetToken(theEnv,execStatus,readSource,&DefclassData(theEnv,execStatus)->ObjectParseToken);
   if ((GetType(DefclassData(theEnv,execStatus)->ObjectParseToken) != INSTANCE_NAME) &&
       (GetType(DefclassData(theEnv,execStatus)->ObjectParseToken) != SYMBOL))
     goto MakeInstanceError;

   if ((GetType(DefclassData(theEnv,execStatus)->ObjectParseToken) == SYMBOL) &&
       (strcmp(CLASS_RLN,DOToString(DefclassData(theEnv,execStatus)->ObjectParseToken)) == 0))
     {
      top->argList = GenConstant(theEnv,execStatus,FCALL,
                                 (void *) FindFunction(theEnv,execStatus,"gensym*"));
     }
   else
     {
      top->argList = GenConstant(theEnv,execStatus,INSTANCE_NAME,
                                 (void *) GetValue(DefclassData(theEnv,execStatus)->ObjectParseToken));
      GetToken(theEnv,execStatus,readSource,&DefclassData(theEnv,execStatus)->ObjectParseToken);
      if ((GetType(DefclassData(theEnv,execStatus)->ObjectParseToken) != SYMBOL) ? TRUE :
          (strcmp(CLASS_RLN,DOToString(DefclassData(theEnv,execStatus)->ObjectParseToken)) != 0))
        goto MakeInstanceError;
     }

   GetToken(theEnv,execStatus,readSource,&DefclassData(theEnv,execStatus)->ObjectParseToken);
   if (GetType(DefclassData(theEnv,execStatus)->ObjectParseToken) != SYMBOL)
     goto MakeInstanceError;
   top->argList->nextArg =
        GenConstant(theEnv,execStatus,SYMBOL,(void *) GetValue(DefclassData(theEnv,execStatus)->ObjectParseToken));
   theExp = top->argList->nextArg;
   if (ReplaceClassNameWithReference(theEnv,execStatus,theExp) == FALSE)
     goto MakeInstanceError;
   GetToken(theEnv,execStatus,readSource,&DefclassData(theEnv,execStatus)->ObjectParseToken);
   while (GetType(DefclassData(theEnv,execStatus)->ObjectParseToken) == LPAREN)
     {
      GetToken(theEnv,execStatus,readSource,&DefclassData(theEnv,execStatus)->ObjectParseToken);
      if (GetType(DefclassData(theEnv,execStatus)->ObjectParseToken) != SYMBOL)
        goto SlotOverrideError;
      theExp->nextArg = GenConstant(theEnv,execStatus,SYMBOL,(void *) GetValue(DefclassData(theEnv,execStatus)->ObjectParseToken));
      theExp->nextArg->nextArg = GenConstant(theEnv,execStatus,SYMBOL,EnvTrueSymbol(theEnv,execStatus));
      theExp = theExp->nextArg->nextArg;
      GetToken(theEnv,execStatus,readSource,&DefclassData(theEnv,execStatus)->ObjectParseToken);
      vbot = NULL;
      while (GetType(DefclassData(theEnv,execStatus)->ObjectParseToken) != RPAREN)
        {
         type = GetType(DefclassData(theEnv,execStatus)->ObjectParseToken);
         if (type == LPAREN)
           {
            GetToken(theEnv,execStatus,readSource,&DefclassData(theEnv,execStatus)->ObjectParseToken);
            if ((GetType(DefclassData(theEnv,execStatus)->ObjectParseToken) != SYMBOL) ? TRUE :
                (strcmp(ValueToString(DefclassData(theEnv,execStatus)->ObjectParseToken.value),"create$") != 0))
              goto SlotOverrideError;
            GetToken(theEnv,execStatus,readSource,&DefclassData(theEnv,execStatus)->ObjectParseToken);
            if (GetType(DefclassData(theEnv,execStatus)->ObjectParseToken) != RPAREN)
              goto SlotOverrideError;
            tval = GenConstant(theEnv,execStatus,FCALL,(void *) FindFunction(theEnv,execStatus,"create$"));
           }
         else
           {
            if ((type != SYMBOL) && (type != STRING) &&
                (type != FLOAT) && (type != INTEGER) && (type != INSTANCE_NAME))
              goto SlotOverrideError;
            tval = GenConstant(theEnv,execStatus,type,(void *) GetValue(DefclassData(theEnv,execStatus)->ObjectParseToken));
           }
         if (vals == NULL)
           vals = tval;
         else
           vbot->nextArg = tval;
         vbot = tval;
         GetToken(theEnv,execStatus,readSource,&DefclassData(theEnv,execStatus)->ObjectParseToken);
        }
      theExp->argList = vals;
      GetToken(theEnv,execStatus,readSource,&DefclassData(theEnv,execStatus)->ObjectParseToken);
      vals = NULL;
     }
   if (GetType(DefclassData(theEnv,execStatus)->ObjectParseToken) != RPAREN)
     goto SlotOverrideError;
   return(top);

MakeInstanceError:
   SyntaxErrorMessage(theEnv,execStatus,"make-instance");
   SetEvaluationError(theEnv,execStatus,TRUE);
   ReturnExpression(theEnv,execStatus,top);
   return(NULL);

SlotOverrideError:
   SyntaxErrorMessage(theEnv,execStatus,"slot-override");
   SetEvaluationError(theEnv,execStatus,TRUE);
   ReturnExpression(theEnv,execStatus,top);
   ReturnExpression(theEnv,execStatus,vals);
   return(NULL);
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***************************************************
  NAME         : ReplaceClassNameWithReference
  DESCRIPTION  : In parsing a make instance call,
                 this function replaces a constant
                 class name with an actual pointer
                 to the class
  INPUTS       : The expression
  RETURNS      : TRUE if all OK, FALSE
                 if class cannot be found
  SIDE EFFECTS : The expression type and value are
                 modified if class is found
  NOTES        : Searches current nd imported
                 modules for reference
 ***************************************************/
static intBool ReplaceClassNameWithReference(
  void *theEnv,
  EXEC_STATUS,
  EXPRESSION *theExp)
  {
   char *theClassName;
   void *theDefclass;

   if (theExp->type == SYMBOL)
     {
      theClassName = ValueToString(theExp->value);
      theDefclass = (void *) LookupDefclassInScope(theEnv,execStatus,theClassName);
      if (theDefclass == NULL)
        {
         CantFindItemErrorMessage(theEnv,execStatus,"class",theClassName);
         return(FALSE);
        }
      if (EnvClassAbstractP(theEnv,execStatus,theDefclass))
        {
         PrintErrorID(theEnv,execStatus,"INSMNGR",3,FALSE);
         EnvPrintRouter(theEnv,execStatus,WERROR,"Cannot create instances of abstract class ");
         EnvPrintRouter(theEnv,execStatus,WERROR,theClassName);
         EnvPrintRouter(theEnv,execStatus,WERROR,".\n");
         return(FALSE);
        }
      theExp->type = DEFCLASS_PTR;
      theExp->value = theDefclass;
     }
   return(TRUE);
  }

#endif




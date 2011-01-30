   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*          PROCEDURAL FUNCTIONS PARSER MODULE         */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description               */
/* ------------------+-------------+------------------------   */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS         */
/*************************************************************/

#define _PRCDRPSR_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "setup.h"

#include "memalloc.h"
#include "router.h"
#include "argacces.h"
#include "constrnt.h"
#include "cstrnchk.h"
#include "cstrnops.h"
#include "multifld.h"
#include "exprnpsr.h"
#include "scanner.h"
#include "utility.h"
#include "modulutl.h"
#include "cstrnutl.h"

#include "prcdrpsr.h"

#if DEFGLOBAL_CONSTRUCT
#include "globldef.h"
#include "globlpsr.h"
#endif

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if (! RUN_TIME) && (! BLOAD_ONLY)
   static struct expr            *WhileParse(struct expr *,char *);
   static struct expr            *LoopForCountParse(struct expr *,char *);
   static void                    ReplaceLoopCountVars(SYMBOL_HN *,EXPRESSION *,int);
   static struct expr            *IfParse(struct expr *,char *);
   static struct expr            *PrognParse(struct expr *,char *);
   static struct expr            *BindParse(struct expr *,char *);
   static int                     AddBindName(struct symbolHashNode *,CONSTRAINT_RECORD *);
   static struct expr            *ReturnParse(struct expr *,char *);
   static struct expr            *BreakParse(struct expr *,char *);
   static struct expr            *SwitchParse(struct expr *,char *);
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

#if (! RUN_TIME)
   Thread static struct BindInfo   *ListOfParsedBindNames;
#endif

#if ! RUN_TIME
/*******************************************/
/* ProceduralFunctionParsers        */
/*******************************************/
globle void ProceduralFunctionParsers()
  {
#if (! BLOAD_ONLY)
   AddFunctionParser("bind",BindParse);
   AddFunctionParser("progn",PrognParse);
   AddFunctionParser("if",IfParse);
   AddFunctionParser("while",WhileParse);
   AddFunctionParser("loop-for-count",LoopForCountParse);
   AddFunctionParser("return",ReturnParse);
   AddFunctionParser("break",BreakParse);
   AddFunctionParser("switch",SwitchParse);
#endif
  }

/********************************************************/
/* GetParsedBindNames:                                      */
/********************************************************/
globle struct BindInfo *GetParsedBindNames()
  {
   return(ListOfParsedBindNames);
  }

/********************************************************/
/* SetParsedBindNames:                                      */
/********************************************************/
globle void SetParsedBindNames(
  struct BindInfo *newValue)
  {
   ListOfParsedBindNames = newValue;
  }

/********************************************************/
/* ClearParsedBindNames:                                     */
/********************************************************/
globle void ClearParsedBindNames()
  {
   struct BindInfo *temp_bind;

   while (ListOfParsedBindNames != NULL)
     {
      temp_bind = ListOfParsedBindNames->next;
      RemoveConstraint(ListOfParsedBindNames->constraints);
      rtn_struct(BindInfo,ListOfParsedBindNames);
      ListOfParsedBindNames = temp_bind;
     }
  }

/********************************************************/
/* ParsedBindNamesEmpty:                                     */
/********************************************************/
globle BOOLEAN ParsedBindNamesEmpty()
  {
   if (ListOfParsedBindNames != NULL) return(FALSE);

   return(TRUE);
  }

#if (! BLOAD_ONLY)

/*********************************************************/
/* WhileParse: purpose is to parse the while statement.  */
/*   The parse of the statement is the return value.     */
/*   Syntax: (while <expression> do <action>+)           */
/*********************************************************/
static struct expr *WhileParse(
  struct expr *parse,
  char *infile)
  {
   struct token theToken;
   int read_first_paren;

   /*===============================*/
   /* Process the while expression. */
   /*===============================*/

   SavePPBuffer(" ");

   parse->argList = ParseAtomOrExpression(infile,NULL);
   if (parse->argList == NULL)
     {
      ReturnExpression(parse);
      return(NULL);
     }

   /*====================================*/
   /* Process the do keyword if present. */
   /*====================================*/

   GetToken(infile,&theToken);
   if ((theToken.type == SYMBOL) && (strcmp(ValueToString(theToken.value),"do") == 0))
     {
      read_first_paren = TRUE;
      PPBackup();
      SavePPBuffer(" ");
      SavePPBuffer(theToken.printForm);
      IncrementIndentDepth(3);
      PPCRAndIndent();
     }
   else if (theToken.type == LPAREN)
     {
      read_first_paren = FALSE;
      PPBackup();
      IncrementIndentDepth(3);
      PPCRAndIndent();
      SavePPBuffer(theToken.printForm);
     }
   else
     {
      SyntaxErrorMessage("while function");
      ReturnExpression(parse);
      return(NULL);
     }

   /*============================*/
   /* Process the while actions. */
   /*============================*/
   if (svContexts->rtn == TRUE)
     ReturnContext = TRUE;
   BreakContext = TRUE;
   parse->argList->nextArg = GroupActions(infile,&theToken,read_first_paren,NULL,FALSE);

   if (parse->argList->nextArg == NULL)
     {
      ReturnExpression(parse);
      return(NULL);
     }
   PPBackup();
   PPBackup();
   SavePPBuffer(theToken.printForm);

   /*=======================================================*/
   /* Check for the closing right parenthesis of the while. */
   /*=======================================================*/

   if (theToken.type != RPAREN)
     {
      SyntaxErrorMessage("while function");
      ReturnExpression(parse);
      return(NULL);
     }

   DecrementIndentDepth(3);

   return(parse);
  }

/******************************************************************************************/
/* LoopForCountParse: purpose is to parse the loop-for-count statement.                   */
/*   The parse of the statement is the return value.                                      */
/*   Syntax: (loop-for-count <range> [do] <action>+)                                      */
/*           <range> ::= (<sf-var> [<start-integer-expression>] <end-integer-expression>) */
/******************************************************************************************/
static struct expr *LoopForCountParse(
  struct expr *parse,
  char *infile)
  {
   struct token theToken;
   SYMBOL_HN *loopVar = NULL;
   EXPRESSION *tmpexp;
   int read_first_paren;
   struct BindInfo *oldBindList,*newBindList,*prev;

   /*======================================*/
   /* Process the loop counter expression. */
   /*======================================*/

   SavePPBuffer(" ");
   GetToken(infile,&theToken);

   /* ==========================================
      Simple form: loop-for-count <end> [do] ...
      ========================================== */
   if (theToken.type != LPAREN)
     {
      parse->argList = GenConstant(INTEGER,AddLong(1L));
      parse->argList->nextArg = ParseAtomOrExpression(infile,&theToken);
      if (parse->argList->nextArg == NULL)
        {
         ReturnExpression(parse);
         return(NULL);
        }
     }
   else
     {
      GetToken(infile,&theToken);
      if (theToken.type != SF_VARIABLE)
        {
         if (theToken.type != SYMBOL)
           goto LoopForCountParseError;
         parse->argList = GenConstant(INTEGER,AddLong(1L));
         parse->argList->nextArg = Function2Parse(infile,ValueToString(theToken.value));
         if (parse->argList->nextArg == NULL)
           {
            ReturnExpression(parse);
            return(NULL);
           }
        }

      /* =============================================================
         Complex form: loop-for-count (<var> [<start>] <end>) [do] ...
         ============================================================= */
      else
        {
         loopVar = (SYMBOL_HN *) theToken.value;
         SavePPBuffer(" ");
         parse->argList = ParseAtomOrExpression(infile,NULL);
         if (parse->argList == NULL)
           {
            ReturnExpression(parse);
            return(NULL);
           }
         if (CheckArgumentAgainstRestriction(parse->argList,(int) 'i'))
           goto LoopForCountParseError;
         SavePPBuffer(" ");
         GetToken(infile,&theToken);
         if (theToken.type == RPAREN)
           {
            PPBackup();
            PPBackup();
            SavePPBuffer(theToken.printForm);
            tmpexp = GenConstant(INTEGER,AddLong(1L));
            tmpexp->nextArg = parse->argList;
            parse->argList = tmpexp;
           }
         else
          {
            parse->argList->nextArg = ParseAtomOrExpression(infile,&theToken);
            if (parse->argList->nextArg == NULL)
              {
               ReturnExpression(parse);
               return(NULL);
              }
            GetToken(infile,&theToken);
            if (theToken.type != RPAREN)
              goto LoopForCountParseError;
           }
         SavePPBuffer(" ");
        }
     }

   if (CheckArgumentAgainstRestriction(parse->argList->nextArg,(int) 'i'))
     goto LoopForCountParseError;

   /*====================================*/
   /* Process the do keyword if present. */
   /*====================================*/

   GetToken(infile,&theToken);
   if ((theToken.type == SYMBOL) && (strcmp(ValueToString(theToken.value),"do") == 0))
     {
      read_first_paren = TRUE;
      PPBackup();
      SavePPBuffer(" ");
      SavePPBuffer(theToken.printForm);
      IncrementIndentDepth(3);
      PPCRAndIndent();
     }
   else if (theToken.type == LPAREN)
     {
      read_first_paren = FALSE;
      PPBackup();
      IncrementIndentDepth(3);
      PPCRAndIndent();
      SavePPBuffer(theToken.printForm);
     }
   else
     goto LoopForCountParseError;

   /*=====================================*/
   /* Process the loop-for-count actions. */
   /*=====================================*/
   if (svContexts->rtn == TRUE)
     ReturnContext = TRUE;
   BreakContext = TRUE;
   oldBindList = GetParsedBindNames();
   SetParsedBindNames(NULL);
   parse->argList->nextArg->nextArg =
      GroupActions(infile,&theToken,read_first_paren,NULL,FALSE);

   if (parse->argList->nextArg->nextArg == NULL)
     {
      SetParsedBindNames(oldBindList);
      ReturnExpression(parse);
      return(NULL);
     }
   newBindList = GetParsedBindNames();
   prev = NULL;
   while (newBindList != NULL)
     {
      if ((loopVar == NULL) ? FALSE :
          (strcmp(ValueToString(newBindList->name),ValueToString(loopVar)) == 0))
        {
         ClearParsedBindNames();
         SetParsedBindNames(oldBindList);
         PrintErrorID("PRCDRPSR",1,TRUE);
         PrintRouter(WERROR,"Cannot rebind loop variable in function loop-for-count.\n");
         ReturnExpression(parse);
         return(NULL);
        }
      prev = newBindList;
      newBindList = newBindList->next;
     }
   if (prev == NULL)
     SetParsedBindNames(oldBindList);
   else
     prev->next = oldBindList;
   if (loopVar != NULL)
     ReplaceLoopCountVars(loopVar,parse->argList->nextArg->nextArg,0);
   PPBackup();
   PPBackup();
   SavePPBuffer(theToken.printForm);

   /*================================================================*/
   /* Check for the closing right parenthesis of the loop-for-count. */
   /*================================================================*/

   if (theToken.type != RPAREN)
     {
      SyntaxErrorMessage("loop-for-count function");
      ReturnExpression(parse);
      return(NULL);
     }

   DecrementIndentDepth(3);

   return(parse);

LoopForCountParseError:
   SyntaxErrorMessage("loop-for-count function");
   ReturnExpression(parse);
   return(NULL);
  }

/***************************************************/
/* ReplaceLoopCountVars                            */
/***************************************************/
static void ReplaceLoopCountVars(
  SYMBOL_HN *loopVar,
  EXPRESSION *exp,
  int depth)
  {
   while (exp != NULL)
     {
      if ((exp->type != SF_VARIABLE) ? FALSE :
          (strcmp(ValueToString(exp->value),ValueToString(loopVar)) == 0))
        {
         exp->type = FCALL;
         exp->value = (void *) FindFunction("(get-loop-count)");
         exp->argList = GenConstant(INTEGER,AddLong((long) depth));
        }
      else if (exp->argList != NULL)
        {
         if ((exp->type != FCALL) ? FALSE :
             (exp->value == (void *) FindFunction("loop-for-count")))
           ReplaceLoopCountVars(loopVar,exp->argList,depth+1);
         else
           ReplaceLoopCountVars(loopVar,exp->argList,depth);
        }
      exp = exp->nextArg;
     }
  }

/*********************************************************/
/* IfParse: purpose is to parse the if statement.  The  */
/*   parse of the statement is the return value.         */
/*   Syntax: (if <expression> then <action>+             */
/*               [ else <action>+ ] )                    */
/*********************************************************/
static struct expr *IfParse(
  struct expr *top,
  char *infile)
  {
   struct token theToken;

   /*============================*/
   /* Process the if expression. */
   /*============================*/

   SavePPBuffer(" ");

   top->argList = ParseAtomOrExpression(infile,NULL);

   if (top->argList == NULL)
     {
      ReturnExpression(top);
      return(NULL);
     }

   /*========================================*/
   /* Keyword 'then' must follow expression. */
   /*========================================*/

   IncrementIndentDepth(3);
   PPCRAndIndent();

   GetToken(infile,&theToken);
   if ((theToken.type != SYMBOL) || (strcmp(ValueToString(theToken.value),"then") != 0))
     {
      SyntaxErrorMessage("if function");
      ReturnExpression(top);
      return(NULL);
     }

   /*==============================*/
   /* Process the if then actions. */
   /*==============================*/

   PPCRAndIndent();
   if (svContexts->rtn == TRUE)
     ReturnContext = TRUE;
   if (svContexts->brk == TRUE)
     BreakContext = TRUE;
   top->argList->nextArg = GroupActions(infile,&theToken,TRUE,"else",FALSE);

   if (top->argList->nextArg == NULL)
     {
      ReturnExpression(top);
      return(NULL);
     }

   /*===========================================*/
   /* A ')' signals an if then without an else. */
   /*===========================================*/

   if (theToken.type == RPAREN)
     {
      DecrementIndentDepth(3);
      PPBackup();
      PPBackup();
      SavePPBuffer(theToken.printForm);
      return(top);
     }

   /*=============================================*/
   /* Keyword 'else' must follow if then actions. */
   /*=============================================*/

   if ((theToken.type != SYMBOL) || (strcmp(ValueToString(theToken.value),"else") != 0))
     {
      SyntaxErrorMessage("if function");
      ReturnExpression(top);
      return(NULL);
     }

   /*==============================*/
   /* Process the if else actions. */
   /*==============================*/

   PPCRAndIndent();
   top->argList->nextArg->nextArg = GroupActions(infile,&theToken,TRUE,NULL,FALSE);

   if (top->argList->nextArg->nextArg == NULL)
     {
      ReturnExpression(top);
      return(NULL);
     }

   /*======================================================*/
   /* Check for the closing right parenthesis of the if. */
   /*======================================================*/

   if (theToken.type != RPAREN)
     {
      SyntaxErrorMessage("if function");
      ReturnExpression(top);
      return(NULL);
     }

   /*===========================================*/
   /* A ')' signals an if then without an else. */
   /*===========================================*/

   PPBackup();
   PPBackup();
   SavePPBuffer(")");
   DecrementIndentDepth(3);
   return(top);
  }

/********************************************************/
/* PrognParse: purpose is to parse the progn statement. */
/*   The parse of the statement is the return value.    */
/*   Syntax:  (progn <expression>*)                     */
/********************************************************/
static struct expr *PrognParse(
  struct expr *top,
  char *infile)
  {
   struct token tkn;
   struct expr *tmp;

   ReturnExpression(top);
   BreakContext = svContexts->brk;
   ReturnContext = svContexts->rtn;
   IncrementIndentDepth(3);
   PPCRAndIndent();
   tmp = GroupActions(infile,&tkn,TRUE,NULL,FALSE);
   DecrementIndentDepth(3);
   PPBackup();
   PPBackup();
   SavePPBuffer(tkn.printForm);
   return(tmp);
  }

/***********************************************************/
/* BindParse: purpose is to parse the bind statement. The */
/*   parse of the statement is the return value.           */
/*   Syntax:  (bind ?var <expression>)                     */
/***********************************************************/
static struct expr *BindParse(
  struct expr *top,
  char *infile)
  {
   struct token theToken;
   SYMBOL_HN *variableName;
   struct expr *texp;
   CONSTRAINT_RECORD *theConstraint = NULL;
#if DEFGLOBAL_CONSTRUCT
   struct defglobal *theGlobal;
   int count;
#endif

   SavePPBuffer(" ");

   /*=============================================*/
   /* Next token must be the name of the variable */
   /* to be bound.                                */
   /*=============================================*/

   GetToken(infile,&theToken);
   if ((theToken.type != SF_VARIABLE) && (theToken.type != GBL_VARIABLE))
     {
      if ((theToken.type != MF_VARIABLE) || SequenceOpMode)
        {
         SyntaxErrorMessage("bind function");
         ReturnExpression(top);
         return(NULL);
        }
     }

   /*==============================*/
   /* Process the bind expression. */
   /*==============================*/

   top->argList = GenConstant(SYMBOL,theToken.value);
   variableName = (SYMBOL_HN *) theToken.value;

#if DEFGLOBAL_CONSTRUCT
   if ((theToken.type == GBL_VARIABLE) ?
       ((theGlobal = (struct defglobal *)
                     FindImportedConstruct("defglobal",NULL,ValueToString(variableName),
                                           &count,TRUE,FALSE)) != NULL) :
       FALSE)
     {
      top->argList->type = DEFGLOBAL_PTR;
      top->argList->value = (void *) theGlobal;
     }
   else if (theToken.type == GBL_VARIABLE)
     {
      GlobalReferenceErrorMessage(ValueToString(variableName));
      ReturnExpression(top);
      return(NULL);
     }
#endif

   texp = get_struct(expr);
   texp->argList = texp->nextArg = NULL;
   if (CollectArguments(texp,infile) == NULL)
     {
      ReturnExpression(top);
      return(NULL);
     }

   top->argList->nextArg = texp->argList;
   rtn_struct(expr,texp);

#if DEFGLOBAL_CONSTRUCT
   if (top->argList->type == DEFGLOBAL_PTR) return(top);
#endif

   if (top->argList->nextArg != NULL)
     { theConstraint = ExpressionToConstraintRecord(top->argList->nextArg); }

   AddBindName(variableName,theConstraint);

   return(top);
  }

/********************************************/
/* ReturnParse: Parses the return function. */
/********************************************/
static struct expr *ReturnParse(
  struct expr *top,
  char *infile)
  {
   int error_flag = FALSE;
   struct token theToken;

   if (svContexts->rtn == TRUE)
     ReturnContext = TRUE;
   if (ReturnContext == FALSE)
     {
      PrintErrorID("PRCDRPSR",2,TRUE);
      PrintRouter(WERROR,"The return function is not valid in this context.\n");
      ReturnExpression(top);
      return(NULL);
     }
   ReturnContext = FALSE;

   SavePPBuffer(" ");

   top->argList = ArgumentParse(infile,&error_flag);
   if (error_flag)
     {
      ReturnExpression(top);
      return(NULL);
     }
   else if (top->argList == NULL)
     {
      PPBackup();
      PPBackup();
      SavePPBuffer(")");
     }
   else
     {
      SavePPBuffer(" ");
      GetToken(infile,&theToken);
      if (theToken.type != RPAREN)
        {
         SyntaxErrorMessage("return function");
         ReturnExpression(top);
         return(NULL);
        }
      PPBackup();
      PPBackup();
      SavePPBuffer(")");
     }
   return(top);
  }

/**********************************************/
/* BreakParse:                                */
/**********************************************/
static struct expr *BreakParse(
  struct expr *top,
  char *infile)
  {
   struct token theToken;

   if (svContexts->brk == FALSE)
     {
      PrintErrorID("PRCDRPSR",2,TRUE);
      PrintRouter(WERROR,"The break function not valid in this context.\n");
      ReturnExpression(top);
      return(NULL);
     }

   SavePPBuffer(" ");
   GetToken(infile,&theToken);
   if (theToken.type != RPAREN)
     {
      SyntaxErrorMessage("break function");
      ReturnExpression(top);
      return(NULL);
     }
   PPBackup();
   PPBackup();
   SavePPBuffer(")");
   return(top);
  }

/**********************************************/
/* SwitchParse:                               */
/**********************************************/
static struct expr *SwitchParse(
  struct expr *top,
  char *infile)
  {
   struct token theToken;
   EXPRESSION *exp,*chk;
   int case_count = 0,default_count = 0;

   /*============================*/
   /* Process the switch value   */
   /*============================*/
   IncrementIndentDepth(3);
   SavePPBuffer(" ");
   top->argList = exp = ParseAtomOrExpression(infile,NULL);
   if (exp == NULL)
     goto SwitchParseError;

   /*========================*/
   /* Parse case statements. */
   /*========================*/
   GetToken(infile,&theToken);
   while (theToken.type != RPAREN)
     {
      PPBackup();
      PPCRAndIndent();
      SavePPBuffer(theToken.printForm);
      if (theToken.type != LPAREN)
        goto SwitchParseErrorAndMessage;
      GetToken(infile,&theToken);
      SavePPBuffer(" ");
      if ((theToken.type == SYMBOL) &&
          (strcmp(ValueToString(theToken.value),"case") == 0))
        {
         if (default_count != 0)
           goto SwitchParseErrorAndMessage;
         exp->nextArg = ParseAtomOrExpression(infile,NULL);
         SavePPBuffer(" ");
         if (exp->nextArg == NULL)
           goto SwitchParseError;
         for (chk = top->argList->nextArg ; chk != exp->nextArg ; chk = chk->nextArg)
           {
            if ((chk->type == exp->nextArg->type) &&
                (chk->value == exp->nextArg->value) &&
                IdenticalExpression(chk->argList,exp->nextArg->argList))
              {
               PrintErrorID("PRCDRPSR",3,TRUE);
               PrintRouter(WERROR,"Duplicate case found in switch function.\n");
               goto SwitchParseError;
              }
           }
         GetToken(infile,&theToken);
         if ((theToken.type != SYMBOL) ? TRUE :
             (strcmp(ValueToString(theToken.value),"then") != 0))
           goto SwitchParseErrorAndMessage;
         case_count++;
        }
      else if ((theToken.type == SYMBOL) &&
               (strcmp(ValueToString(theToken.value),"default") == 0))
        {
         if ((case_count < 2) || default_count)
           goto SwitchParseErrorAndMessage;
         exp->nextArg = GenConstant(RVOID,NULL);
         default_count = 1;
        }
      else
        goto SwitchParseErrorAndMessage;
      exp = exp->nextArg;
      if (svContexts->rtn == TRUE)
        ReturnContext = TRUE;
      if (svContexts->brk == TRUE)
        BreakContext = TRUE;
      IncrementIndentDepth(3);
      PPCRAndIndent();
      exp->nextArg = GroupActions(infile,&theToken,TRUE,NULL,FALSE);
      DecrementIndentDepth(3);
      ReturnContext = FALSE;
      BreakContext = FALSE;
      if (exp->nextArg == NULL)
        goto SwitchParseError;
      exp = exp->nextArg;
      PPBackup();
      PPBackup();
      SavePPBuffer(theToken.printForm);
      GetToken(infile,&theToken);
     }
   if (case_count >= 2)
     {
      DecrementIndentDepth(3);
      return(top);
     }

SwitchParseErrorAndMessage:
   SyntaxErrorMessage("switch function");
SwitchParseError:
   ReturnExpression(top);
   DecrementIndentDepth(3);
   return(NULL);
  }

/********************************************************/
/* SearchParsedBindNames:                               */
/********************************************************/
globle int SearchParsedBindNames(
  SYMBOL_HN *name_sought)
  {
   struct BindInfo *var_ptr;
   int index = 1;

   var_ptr = ListOfParsedBindNames;
   while (var_ptr != NULL)
     {
      if (var_ptr->name == name_sought)
        { return(index); }
      var_ptr = var_ptr->next;
      index++;
     }

   return(0);
  }

/********************************************************/
/* FindBindConstraints:                               */
/********************************************************/
globle struct constraintRecord *FindBindConstraints(
  SYMBOL_HN *nameSought)
  {
   struct BindInfo *theVariable;

   theVariable = ListOfParsedBindNames;
   while (theVariable != NULL)
     {
      if (theVariable->name == nameSought)
        { return(theVariable->constraints); }
      theVariable = theVariable->next;
     }

   return(NULL);
  }

/********************************************************/
/* CountParsedBindNames: Counts the number of variables */
/*   names that have been bound using the bind function */
/*   in the current context (e.g. the RHS of a rule).   */
/********************************************************/
globle int CountParsedBindNames()
  {
   struct BindInfo *theVariable;
   int index = 0;

   theVariable = ListOfParsedBindNames;
   while (theVariable != NULL)
     {
      theVariable = theVariable->next;
      index++;
     }

   return(index);
  }

/****************************************************************/
/* AddBindName: Adds a variable name used as the first argument */
/*   of the bind function to the list of variable names parsed  */
/*   within the current semantic context (e.g. RHS of a rule).  */
/****************************************************************/
static int AddBindName(
  SYMBOL_HN *variableName,
  CONSTRAINT_RECORD *theConstraint)
  {
   CONSTRAINT_RECORD *tmpConstraint;
   struct BindInfo *currentBind, *lastBind;
   int index = 1;

   /*=========================================================*/
   /* Look for the variable name in the list of bind variable */
   /* names already parsed. If it is found, then return the   */
   /* index to the variable and union the new constraint      */
   /* information with the old constraint information.        */
   /*=========================================================*/

   lastBind = NULL;
   currentBind = ListOfParsedBindNames;
   while (currentBind != NULL)
     {
      if (currentBind->name == variableName)
        {
         if (theConstraint != NULL)
           {
            tmpConstraint = currentBind->constraints;
            currentBind->constraints = UnionConstraints(theConstraint,currentBind->constraints);
            RemoveConstraint(tmpConstraint);
            RemoveConstraint(theConstraint);
           }

         return(index);
        }
      lastBind = currentBind;
      currentBind = currentBind->next;
      index++;
     }

   /*===============================================================*/
   /* If the variable name wasn't found, then add it to the list of */
   /* variable names and store the constraint information with it.  */
   /*===============================================================*/

   currentBind = get_struct(BindInfo);
   currentBind->name = variableName;
   currentBind->constraints = theConstraint;
   currentBind->next = NULL;

   if (lastBind == NULL) ListOfParsedBindNames = currentBind;
   else lastBind->next = currentBind;

   return(index);
  }

/********************************************************/
/* RemoveParsedBindName:                                     */
/********************************************************/
globle void RemoveParsedBindName(
  struct symbolHashNode *bname)
  {
   struct BindInfo *prv,*tmp;

   prv = NULL;
   tmp = ListOfParsedBindNames;
   while ((tmp != NULL) ? (tmp->name != bname) : FALSE)
     {
      prv = tmp;
      tmp = tmp->next;
     }
   if (tmp != NULL)
     {
      if (prv == NULL)
        ListOfParsedBindNames = tmp->next;
      else
        prv->next = tmp->next;

      RemoveConstraint(tmp->constraints);
      rtn_struct(BindInfo,tmp);
     }
  }

#endif

#endif


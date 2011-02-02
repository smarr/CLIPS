   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*          PROCEDURAL FUNCTIONS PARSER MODULE         */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*      6.23: Changed name of variable exp to theExp         */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#define _PRCDRPSR_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "setup.h"

#include "argacces.h"
#include "constrnt.h"
#include "cstrnchk.h"
#include "cstrnops.h"
#include "cstrnutl.h"
#include "envrnmnt.h"
#include "exprnpsr.h"
#include "memalloc.h"
#include "modulutl.h"
#include "multifld.h"
#include "router.h"
#include "scanner.h"
#include "utility.h"

#include "prcdrpsr.h"

#if DEFGLOBAL_CONSTRUCT
#include "globldef.h"
#include "globlpsr.h"
#endif

#if ! RUN_TIME
#define PRCDRPSR_DATA 12

struct procedureParserData
  { 
   struct BindInfo *ListOfParsedBindNames;
  };

#define ProcedureParserData(theEnv,execStatus) ((struct procedureParserData *) GetEnvironmentData(theEnv,execStatus,PRCDRPSR_DATA))
#endif

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if (! RUN_TIME) && (! BLOAD_ONLY)
   static struct expr            *WhileParse(void *,EXEC_STATUS,struct expr *,char *);
   static struct expr            *LoopForCountParse(void *,EXEC_STATUS,struct expr *,char *);
   static void                    ReplaceLoopCountVars(void *,EXEC_STATUS,SYMBOL_HN *,EXPRESSION *,int);
   static struct expr            *IfParse(void *,EXEC_STATUS,struct expr *,char *);
   static struct expr            *PrognParse(void *,EXEC_STATUS,struct expr *,char *);
   static struct expr            *BindParse(void *,EXEC_STATUS,struct expr *,char *);
   static int                     AddBindName(void *,EXEC_STATUS,struct symbolHashNode *,CONSTRAINT_RECORD *);
   static struct expr            *ReturnParse(void *,EXEC_STATUS,struct expr *,char *);
   static struct expr            *BreakParse(void *,EXEC_STATUS,struct expr *,char *);
   static struct expr            *SwitchParse(void *,EXEC_STATUS,struct expr *,char *);
   static void                    DeallocateProceduralFunctionData(void *,EXEC_STATUS);
#endif

#if ! RUN_TIME
/*******************************************/
/* ProceduralFunctionParsers        */
/*******************************************/
globle void ProceduralFunctionParsers(
  void *theEnv,
  EXEC_STATUS)
  {
   AllocateEnvironmentData(theEnv,execStatus,PRCDRPSR_DATA,sizeof(struct procedureParserData),DeallocateProceduralFunctionData);

#if (! BLOAD_ONLY)
   AddFunctionParser(theEnv,execStatus,"bind",BindParse);
   AddFunctionParser(theEnv,execStatus,"progn",PrognParse);
   AddFunctionParser(theEnv,execStatus,"if",IfParse);
   AddFunctionParser(theEnv,execStatus,"while",WhileParse);
   AddFunctionParser(theEnv,execStatus,"loop-for-count",LoopForCountParse);
   AddFunctionParser(theEnv,execStatus,"return",ReturnParse);
   AddFunctionParser(theEnv,execStatus,"break",BreakParse);
   AddFunctionParser(theEnv,execStatus,"switch",SwitchParse);
#endif
  }

/*************************************************************/
/* DeallocateProceduralFunctionData: Deallocates environment */
/*    data for procedural functions.                         */
/*************************************************************/
static void DeallocateProceduralFunctionData(
  void *theEnv,
  EXEC_STATUS)
  { 
   struct BindInfo *temp_bind;

   while (ProcedureParserData(theEnv,execStatus)->ListOfParsedBindNames != NULL)
     {
      temp_bind = ProcedureParserData(theEnv,execStatus)->ListOfParsedBindNames->next;
      rtn_struct(theEnv,execStatus,BindInfo,ProcedureParserData(theEnv,execStatus)->ListOfParsedBindNames);
      ProcedureParserData(theEnv,execStatus)->ListOfParsedBindNames = temp_bind;
     }
  }

/********************************************************/
/* GetParsedBindNames:                                      */
/********************************************************/
globle struct BindInfo *GetParsedBindNames(
  void *theEnv,
  EXEC_STATUS)
  {
   return(ProcedureParserData(theEnv,execStatus)->ListOfParsedBindNames);
  }

/********************************************************/
/* SetParsedBindNames:                                      */
/********************************************************/
globle void SetParsedBindNames(
  void *theEnv,
  EXEC_STATUS,
  struct BindInfo *newValue)
  {
   ProcedureParserData(theEnv,execStatus)->ListOfParsedBindNames = newValue;
  }

/********************************************************/
/* ClearParsedBindNames:                                     */
/********************************************************/
globle void ClearParsedBindNames(
  void *theEnv,
  EXEC_STATUS)
  {
   struct BindInfo *temp_bind;

   while (ProcedureParserData(theEnv,execStatus)->ListOfParsedBindNames != NULL)
     {
      temp_bind = ProcedureParserData(theEnv,execStatus)->ListOfParsedBindNames->next;
      RemoveConstraint(theEnv,execStatus,ProcedureParserData(theEnv,execStatus)->ListOfParsedBindNames->constraints);
      rtn_struct(theEnv,execStatus,BindInfo,ProcedureParserData(theEnv,execStatus)->ListOfParsedBindNames);
      ProcedureParserData(theEnv,execStatus)->ListOfParsedBindNames = temp_bind;
     }
  }

/********************************************************/
/* ParsedBindNamesEmpty:                                     */
/********************************************************/
globle intBool ParsedBindNamesEmpty(
  void *theEnv,
  EXEC_STATUS)
  {
   if (ProcedureParserData(theEnv,execStatus)->ListOfParsedBindNames != NULL) return(FALSE);

   return(TRUE);
  }

#if (! BLOAD_ONLY)

/*********************************************************/
/* WhileParse: purpose is to parse the while statement.  */
/*   The parse of the statement is the return value.     */
/*   Syntax: (while <expression> do <action>+)           */
/*********************************************************/
static struct expr *WhileParse(
  void *theEnv,
  EXEC_STATUS,
  struct expr *parse,
  char *infile)
  {
   struct token theToken;
   int read_first_paren;

   /*===============================*/
   /* Process the while expression. */
   /*===============================*/

   SavePPBuffer(theEnv,execStatus," ");

   parse->argList = ParseAtomOrExpression(theEnv,execStatus,infile,NULL);
   if (parse->argList == NULL)
     {
      ReturnExpression(theEnv,execStatus,parse);
      return(NULL);
     }

   /*====================================*/
   /* Process the do keyword if present. */
   /*====================================*/

   GetToken(theEnv,execStatus,infile,&theToken);
   if ((theToken.type == SYMBOL) && (strcmp(ValueToString(theToken.value),"do") == 0))
     {
      read_first_paren = TRUE;
      PPBackup(theEnv,execStatus);
      SavePPBuffer(theEnv,execStatus," ");
      SavePPBuffer(theEnv,execStatus,theToken.printForm);
      IncrementIndentDepth(theEnv,execStatus,3);
      PPCRAndIndent(theEnv,execStatus);
     }
   else if (theToken.type == LPAREN)
     {
      read_first_paren = FALSE;
      PPBackup(theEnv,execStatus);
      IncrementIndentDepth(theEnv,execStatus,3);
      PPCRAndIndent(theEnv,execStatus);
      SavePPBuffer(theEnv,execStatus,theToken.printForm);
     }
   else
     {
      SyntaxErrorMessage(theEnv,execStatus,"while function");
      ReturnExpression(theEnv,execStatus,parse);
      return(NULL);
     }

   /*============================*/
   /* Process the while actions. */
   /*============================*/
   if (ExpressionData(theEnv,execStatus)->svContexts->rtn == TRUE)
     ExpressionData(theEnv,execStatus)->ReturnContext = TRUE;
   ExpressionData(theEnv,execStatus)->BreakContext = TRUE;
   parse->argList->nextArg = GroupActions(theEnv,execStatus,infile,&theToken,read_first_paren,NULL,FALSE);

   if (parse->argList->nextArg == NULL)
     {
      ReturnExpression(theEnv,execStatus,parse);
      return(NULL);
     }
   PPBackup(theEnv,execStatus);
   PPBackup(theEnv,execStatus);
   SavePPBuffer(theEnv,execStatus,theToken.printForm);

   /*=======================================================*/
   /* Check for the closing right parenthesis of the while. */
   /*=======================================================*/

   if (theToken.type != RPAREN)
     {
      SyntaxErrorMessage(theEnv,execStatus,"while function");
      ReturnExpression(theEnv,execStatus,parse);
      return(NULL);
     }

   DecrementIndentDepth(theEnv,execStatus,3);

   return(parse);
  }

/******************************************************************************************/
/* LoopForCountParse: purpose is to parse the loop-for-count statement.                   */
/*   The parse of the statement is the return value.                                      */
/*   Syntax: (loop-for-count <range> [do] <action>+)                                      */
/*           <range> ::= (<sf-var> [<start-integer-expression>] <end-integer-expression>) */
/******************************************************************************************/
static struct expr *LoopForCountParse(
  void *theEnv,
  EXEC_STATUS,
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

   SavePPBuffer(theEnv,execStatus," ");
   GetToken(theEnv,execStatus,infile,&theToken);

   /* ==========================================
      Simple form: loop-for-count <end> [do] ...
      ========================================== */
   if (theToken.type != LPAREN)
     {
      parse->argList = GenConstant(theEnv,execStatus,INTEGER,EnvAddLong(theEnv,execStatus,1LL));
      parse->argList->nextArg = ParseAtomOrExpression(theEnv,execStatus,infile,&theToken);
      if (parse->argList->nextArg == NULL)
        {
         ReturnExpression(theEnv,execStatus,parse);
         return(NULL);
        }
     }
   else
     {
      GetToken(theEnv,execStatus,infile,&theToken);
      if (theToken.type != SF_VARIABLE)
        {
         if (theToken.type != SYMBOL)
           goto LoopForCountParseError;
         parse->argList = GenConstant(theEnv,execStatus,INTEGER,EnvAddLong(theEnv,execStatus,1LL));
         parse->argList->nextArg = Function2Parse(theEnv,execStatus,infile,ValueToString(theToken.value));
         if (parse->argList->nextArg == NULL)
           {
            ReturnExpression(theEnv,execStatus,parse);
            return(NULL);
           }
        }

      /* =============================================================
         Complex form: loop-for-count (<var> [<start>] <end>) [do] ...
         ============================================================= */
      else
        {
         loopVar = (SYMBOL_HN *) theToken.value;
         SavePPBuffer(theEnv,execStatus," ");
         parse->argList = ParseAtomOrExpression(theEnv,execStatus,infile,NULL);
         if (parse->argList == NULL)
           {
            ReturnExpression(theEnv,execStatus,parse);
            return(NULL);
           }
         if (CheckArgumentAgainstRestriction(theEnv,execStatus,parse->argList,(int) 'i'))
           goto LoopForCountParseError;
         SavePPBuffer(theEnv,execStatus," ");
         GetToken(theEnv,execStatus,infile,&theToken);
         if (theToken.type == RPAREN)
           {
            PPBackup(theEnv,execStatus);
            PPBackup(theEnv,execStatus);
            SavePPBuffer(theEnv,execStatus,theToken.printForm);
            tmpexp = GenConstant(theEnv,execStatus,INTEGER,EnvAddLong(theEnv,execStatus,1LL));
            tmpexp->nextArg = parse->argList;
            parse->argList = tmpexp;
           }
         else
          {
            parse->argList->nextArg = ParseAtomOrExpression(theEnv,execStatus,infile,&theToken);
            if (parse->argList->nextArg == NULL)
              {
               ReturnExpression(theEnv,execStatus,parse);
               return(NULL);
              }
            GetToken(theEnv,execStatus,infile,&theToken);
            if (theToken.type != RPAREN)
              goto LoopForCountParseError;
           }
         SavePPBuffer(theEnv,execStatus," ");
        }
     }

   if (CheckArgumentAgainstRestriction(theEnv,execStatus,parse->argList->nextArg,(int) 'i'))
     goto LoopForCountParseError;

   /*====================================*/
   /* Process the do keyword if present. */
   /*====================================*/

   GetToken(theEnv,execStatus,infile,&theToken);
   if ((theToken.type == SYMBOL) && (strcmp(ValueToString(theToken.value),"do") == 0))
     {
      read_first_paren = TRUE;
      PPBackup(theEnv,execStatus);
      SavePPBuffer(theEnv,execStatus," ");
      SavePPBuffer(theEnv,execStatus,theToken.printForm);
      IncrementIndentDepth(theEnv,execStatus,3);
      PPCRAndIndent(theEnv,execStatus);
     }
   else if (theToken.type == LPAREN)
     {
      read_first_paren = FALSE;
      PPBackup(theEnv,execStatus);
      IncrementIndentDepth(theEnv,execStatus,3);
      PPCRAndIndent(theEnv,execStatus);
      SavePPBuffer(theEnv,execStatus,theToken.printForm);
     }
   else
     goto LoopForCountParseError;

   /*=====================================*/
   /* Process the loop-for-count actions. */
   /*=====================================*/
   if (ExpressionData(theEnv,execStatus)->svContexts->rtn == TRUE)
     ExpressionData(theEnv,execStatus)->ReturnContext = TRUE;
   ExpressionData(theEnv,execStatus)->BreakContext = TRUE;
   oldBindList = GetParsedBindNames(theEnv,execStatus);
   SetParsedBindNames(theEnv,execStatus,NULL);
   parse->argList->nextArg->nextArg =
      GroupActions(theEnv,execStatus,infile,&theToken,read_first_paren,NULL,FALSE);

   if (parse->argList->nextArg->nextArg == NULL)
     {
      SetParsedBindNames(theEnv,execStatus,oldBindList);
      ReturnExpression(theEnv,execStatus,parse);
      return(NULL);
     }
   newBindList = GetParsedBindNames(theEnv,execStatus);
   prev = NULL;
   while (newBindList != NULL)
     {
      if ((loopVar == NULL) ? FALSE :
          (strcmp(ValueToString(newBindList->name),ValueToString(loopVar)) == 0))
        {
         ClearParsedBindNames(theEnv,execStatus);
         SetParsedBindNames(theEnv,execStatus,oldBindList);
         PrintErrorID(theEnv,execStatus,"PRCDRPSR",1,TRUE);
         EnvPrintRouter(theEnv,execStatus,WERROR,"Cannot rebind loop variable in function loop-for-count.\n");
         ReturnExpression(theEnv,execStatus,parse);
         return(NULL);
        }
      prev = newBindList;
      newBindList = newBindList->next;
     }
   if (prev == NULL)
     SetParsedBindNames(theEnv,execStatus,oldBindList);
   else
     prev->next = oldBindList;
   if (loopVar != NULL)
     ReplaceLoopCountVars(theEnv,execStatus,loopVar,parse->argList->nextArg->nextArg,0);
   PPBackup(theEnv,execStatus);
   PPBackup(theEnv,execStatus);
   SavePPBuffer(theEnv,execStatus,theToken.printForm);

   /*================================================================*/
   /* Check for the closing right parenthesis of the loop-for-count. */
   /*================================================================*/

   if (theToken.type != RPAREN)
     {
      SyntaxErrorMessage(theEnv,execStatus,"loop-for-count function");
      ReturnExpression(theEnv,execStatus,parse);
      return(NULL);
     }

   DecrementIndentDepth(theEnv,execStatus,3);

   return(parse);

LoopForCountParseError:
   SyntaxErrorMessage(theEnv,execStatus,"loop-for-count function");
   ReturnExpression(theEnv,execStatus,parse);
   return(NULL);
  }

/***************************************************/
/* ReplaceLoopCountVars                            */
/***************************************************/
static void ReplaceLoopCountVars(
  void *theEnv,
  EXEC_STATUS,
  SYMBOL_HN *loopVar,
  EXPRESSION *theExp,
  int depth)
  {
   while (theExp != NULL)
     {
      if ((theExp->type != SF_VARIABLE) ? FALSE :
          (strcmp(ValueToString(theExp->value),ValueToString(loopVar)) == 0))
        {
         theExp->type = FCALL;
         theExp->value = (void *) FindFunction(theEnv,execStatus,"(get-loop-count)");
         theExp->argList = GenConstant(theEnv,execStatus,INTEGER,EnvAddLong(theEnv,execStatus,(long long) depth));
        }
      else if (theExp->argList != NULL)
        {
         if ((theExp->type != FCALL) ? FALSE :
             (theExp->value == (void *) FindFunction(theEnv,execStatus,"loop-for-count")))
           ReplaceLoopCountVars(theEnv,execStatus,loopVar,theExp->argList,depth+1);
         else
           ReplaceLoopCountVars(theEnv,execStatus,loopVar,theExp->argList,depth);
        }
      theExp = theExp->nextArg;
     }
  }

/*********************************************************/
/* IfParse: purpose is to parse the if statement.  The  */
/*   parse of the statement is the return value.         */
/*   Syntax: (if <expression> then <action>+             */
/*               [ else <action>+ ] )                    */
/*********************************************************/
static struct expr *IfParse(
  void *theEnv,
  EXEC_STATUS,
  struct expr *top,
  char *infile)
  {
   struct token theToken;

   /*============================*/
   /* Process the if expression. */
   /*============================*/

   SavePPBuffer(theEnv,execStatus," ");

   top->argList = ParseAtomOrExpression(theEnv,execStatus,infile,NULL);

   if (top->argList == NULL)
     {
      ReturnExpression(theEnv,execStatus,top);
      return(NULL);
     }

   /*========================================*/
   /* Keyword 'then' must follow expression. */
   /*========================================*/

   IncrementIndentDepth(theEnv,execStatus,3);
   PPCRAndIndent(theEnv,execStatus);

   GetToken(theEnv,execStatus,infile,&theToken);
   if ((theToken.type != SYMBOL) || (strcmp(ValueToString(theToken.value),"then") != 0))
     {
      SyntaxErrorMessage(theEnv,execStatus,"if function");
      ReturnExpression(theEnv,execStatus,top);
      return(NULL);
     }

   /*==============================*/
   /* Process the if then actions. */
   /*==============================*/

   PPCRAndIndent(theEnv,execStatus);
   if (ExpressionData(theEnv,execStatus)->svContexts->rtn == TRUE)
     ExpressionData(theEnv,execStatus)->ReturnContext = TRUE;
   if (ExpressionData(theEnv,execStatus)->svContexts->brk == TRUE)
     ExpressionData(theEnv,execStatus)->BreakContext = TRUE;
   top->argList->nextArg = GroupActions(theEnv,execStatus,infile,&theToken,TRUE,"else",FALSE);

   if (top->argList->nextArg == NULL)
     {
      ReturnExpression(theEnv,execStatus,top);
      return(NULL);
     }

   top->argList->nextArg = RemoveUnneededProgn(theEnv,execStatus,top->argList->nextArg);

   /*===========================================*/
   /* A ')' signals an if then without an else. */
   /*===========================================*/

   if (theToken.type == RPAREN)
     {
      DecrementIndentDepth(theEnv,execStatus,3);
      PPBackup(theEnv,execStatus);
      PPBackup(theEnv,execStatus);
      SavePPBuffer(theEnv,execStatus,theToken.printForm);
      return(top);
     }

   /*=============================================*/
   /* Keyword 'else' must follow if then actions. */
   /*=============================================*/

   if ((theToken.type != SYMBOL) || (strcmp(ValueToString(theToken.value),"else") != 0))
     {
      SyntaxErrorMessage(theEnv,execStatus,"if function");
      ReturnExpression(theEnv,execStatus,top);
      return(NULL);
     }

   /*==============================*/
   /* Process the if else actions. */
   /*==============================*/

   PPCRAndIndent(theEnv,execStatus);
   top->argList->nextArg->nextArg = GroupActions(theEnv,execStatus,infile,&theToken,TRUE,NULL,FALSE);

   if (top->argList->nextArg->nextArg == NULL)
     {
      ReturnExpression(theEnv,execStatus,top);
      return(NULL);
     }

   top->argList->nextArg->nextArg = RemoveUnneededProgn(theEnv,execStatus,top->argList->nextArg->nextArg);

   /*======================================================*/
   /* Check for the closing right parenthesis of the if. */
   /*======================================================*/

   if (theToken.type != RPAREN)
     {
      SyntaxErrorMessage(theEnv,execStatus,"if function");
      ReturnExpression(theEnv,execStatus,top);
      return(NULL);
     }

   /*===========================================*/
   /* A ')' signals an if then without an else. */
   /*===========================================*/

   PPBackup(theEnv,execStatus);
   PPBackup(theEnv,execStatus);
   SavePPBuffer(theEnv,execStatus,")");
   DecrementIndentDepth(theEnv,execStatus,3);
   return(top);
  }

/********************************************************/
/* PrognParse: purpose is to parse the progn statement. */
/*   The parse of the statement is the return value.    */
/*   Syntax:  (progn <expression>*)                     */
/********************************************************/
static struct expr *PrognParse(
  void *theEnv,
  EXEC_STATUS,
  struct expr *top,
  char *infile)
  {
   struct token tkn;
   struct expr *tmp;

   ReturnExpression(theEnv,execStatus,top);
   ExpressionData(theEnv,execStatus)->BreakContext = ExpressionData(theEnv,execStatus)->svContexts->brk;
   ExpressionData(theEnv,execStatus)->ReturnContext = ExpressionData(theEnv,execStatus)->svContexts->rtn;
   IncrementIndentDepth(theEnv,execStatus,3);
   PPCRAndIndent(theEnv,execStatus);
   tmp = GroupActions(theEnv,execStatus,infile,&tkn,TRUE,NULL,FALSE);
   DecrementIndentDepth(theEnv,execStatus,3);
   PPBackup(theEnv,execStatus);
   PPBackup(theEnv,execStatus);
   SavePPBuffer(theEnv,execStatus,tkn.printForm);
   return(tmp);
  }

/***********************************************************/
/* BindParse: purpose is to parse the bind statement. The */
/*   parse of the statement is the return value.           */
/*   Syntax:  (bind ?var <expression>)                     */
/***********************************************************/
static struct expr *BindParse(
  void *theEnv,
  EXEC_STATUS,
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

   SavePPBuffer(theEnv,execStatus," ");

   /*=============================================*/
   /* Next token must be the name of the variable */
   /* to be bound.                                */
   /*=============================================*/

   GetToken(theEnv,execStatus,infile,&theToken);
   if ((theToken.type != SF_VARIABLE) && (theToken.type != GBL_VARIABLE))
     {
      if ((theToken.type != MF_VARIABLE) || ExpressionData(theEnv,execStatus)->SequenceOpMode)
        {
         SyntaxErrorMessage(theEnv,execStatus,"bind function");
         ReturnExpression(theEnv,execStatus,top);
         return(NULL);
        }
     }

   /*==============================*/
   /* Process the bind expression. */
   /*==============================*/

   top->argList = GenConstant(theEnv,execStatus,SYMBOL,theToken.value);
   variableName = (SYMBOL_HN *) theToken.value;

#if DEFGLOBAL_CONSTRUCT
   if ((theToken.type == GBL_VARIABLE) ?
       ((theGlobal = (struct defglobal *)
                     FindImportedConstruct(theEnv,execStatus,"defglobal",NULL,ValueToString(variableName),
                                           &count,TRUE,FALSE)) != NULL) :
       FALSE)
     {
      top->argList->type = DEFGLOBAL_PTR;
      top->argList->value = (void *) theGlobal;
     }
   else if (theToken.type == GBL_VARIABLE)
     {
      GlobalReferenceErrorMessage(theEnv,execStatus,ValueToString(variableName));
      ReturnExpression(theEnv,execStatus,top);
      return(NULL);
     }
#endif

   texp = get_struct(theEnv,execStatus,expr);
   texp->argList = texp->nextArg = NULL;
   if (CollectArguments(theEnv,execStatus,texp,infile) == NULL)
     {
      ReturnExpression(theEnv,execStatus,top);
      return(NULL);
     }

   top->argList->nextArg = texp->argList;
   rtn_struct(theEnv,execStatus,expr,texp);

#if DEFGLOBAL_CONSTRUCT
   if (top->argList->type == DEFGLOBAL_PTR) return(top);
#endif

   if (top->argList->nextArg != NULL)
     { theConstraint = ExpressionToConstraintRecord(theEnv,execStatus,top->argList->nextArg); }

   AddBindName(theEnv,execStatus,variableName,theConstraint);

   return(top);
  }

/********************************************/
/* ReturnParse: Parses the return function. */
/********************************************/
static struct expr *ReturnParse(
  void *theEnv,
  EXEC_STATUS,
  struct expr *top,
  char *infile)
  {
   int error_flag = FALSE;
   struct token theToken;

   if (ExpressionData(theEnv,execStatus)->svContexts->rtn == TRUE)
     ExpressionData(theEnv,execStatus)->ReturnContext = TRUE;
   if (ExpressionData(theEnv,execStatus)->ReturnContext == FALSE)
     {
      PrintErrorID(theEnv,execStatus,"PRCDRPSR",2,TRUE);
      EnvPrintRouter(theEnv,execStatus,WERROR,"The return function is not valid in this context.\n");
      ReturnExpression(theEnv,execStatus,top);
      return(NULL);
     }
   ExpressionData(theEnv,execStatus)->ReturnContext = FALSE;

   SavePPBuffer(theEnv,execStatus," ");

   top->argList = ArgumentParse(theEnv,execStatus,infile,&error_flag);
   if (error_flag)
     {
      ReturnExpression(theEnv,execStatus,top);
      return(NULL);
     }
   else if (top->argList == NULL)
     {
      PPBackup(theEnv,execStatus);
      PPBackup(theEnv,execStatus);
      SavePPBuffer(theEnv,execStatus,")");
     }
   else
     {
      SavePPBuffer(theEnv,execStatus," ");
      GetToken(theEnv,execStatus,infile,&theToken);
      if (theToken.type != RPAREN)
        {
         SyntaxErrorMessage(theEnv,execStatus,"return function");
         ReturnExpression(theEnv,execStatus,top);
         return(NULL);
        }
      PPBackup(theEnv,execStatus);
      PPBackup(theEnv,execStatus);
      SavePPBuffer(theEnv,execStatus,")");
     }
   return(top);
  }

/**********************************************/
/* BreakParse:                                */
/**********************************************/
static struct expr *BreakParse(
  void *theEnv,
  EXEC_STATUS,
  struct expr *top,
  char *infile)
  {
   struct token theToken;

   if (ExpressionData(theEnv,execStatus)->svContexts->brk == FALSE)
     {
      PrintErrorID(theEnv,execStatus,"PRCDRPSR",2,TRUE);
      EnvPrintRouter(theEnv,execStatus,WERROR,"The break function not valid in this context.\n");
      ReturnExpression(theEnv,execStatus,top);
      return(NULL);
     }

   SavePPBuffer(theEnv,execStatus," ");
   GetToken(theEnv,execStatus,infile,&theToken);
   if (theToken.type != RPAREN)
     {
      SyntaxErrorMessage(theEnv,execStatus,"break function");
      ReturnExpression(theEnv,execStatus,top);
      return(NULL);
     }
   PPBackup(theEnv,execStatus);
   PPBackup(theEnv,execStatus);
   SavePPBuffer(theEnv,execStatus,")");
   return(top);
  }

/**********************************************/
/* SwitchParse:                               */
/**********************************************/
static struct expr *SwitchParse(
  void *theEnv,
  EXEC_STATUS,
  struct expr *top,
  char *infile)
  {
   struct token theToken;
   EXPRESSION *theExp,*chk;
   int default_count = 0;

   /*============================*/
   /* Process the switch value   */
   /*============================*/
   IncrementIndentDepth(theEnv,execStatus,3);
   SavePPBuffer(theEnv,execStatus," ");
   top->argList = theExp = ParseAtomOrExpression(theEnv,execStatus,infile,NULL);
   if (theExp == NULL)
     goto SwitchParseError;

   /*========================*/
   /* Parse case statements. */
   /*========================*/
   GetToken(theEnv,execStatus,infile,&theToken);
   while (theToken.type != RPAREN)
     {
      PPBackup(theEnv,execStatus);
      PPCRAndIndent(theEnv,execStatus);
      SavePPBuffer(theEnv,execStatus,theToken.printForm);
      if (theToken.type != LPAREN)
        goto SwitchParseErrorAndMessage;
      GetToken(theEnv,execStatus,infile,&theToken);
      SavePPBuffer(theEnv,execStatus," ");
      if ((theToken.type == SYMBOL) &&
          (strcmp(ValueToString(theToken.value),"case") == 0))
        {
         if (default_count != 0)
           goto SwitchParseErrorAndMessage;
         theExp->nextArg = ParseAtomOrExpression(theEnv,execStatus,infile,NULL);
         SavePPBuffer(theEnv,execStatus," ");
         if (theExp->nextArg == NULL)
           goto SwitchParseError;
         for (chk = top->argList->nextArg ; chk != theExp->nextArg ; chk = chk->nextArg)
           {
            if ((chk->type == theExp->nextArg->type) &&
                (chk->value == theExp->nextArg->value) &&
                IdenticalExpression(chk->argList,theExp->nextArg->argList))
              {
               PrintErrorID(theEnv,execStatus,"PRCDRPSR",3,TRUE);
               EnvPrintRouter(theEnv,execStatus,WERROR,"Duplicate case found in switch function.\n");
               goto SwitchParseError;
              }
           }
         GetToken(theEnv,execStatus,infile,&theToken);
         if ((theToken.type != SYMBOL) ? TRUE :
             (strcmp(ValueToString(theToken.value),"then") != 0))
           goto SwitchParseErrorAndMessage;
        }
      else if ((theToken.type == SYMBOL) &&
               (strcmp(ValueToString(theToken.value),"default") == 0))
        {
         if (default_count)
           goto SwitchParseErrorAndMessage;
         theExp->nextArg = GenConstant(theEnv,execStatus,RVOID,NULL);
         default_count = 1;
        }
      else
        goto SwitchParseErrorAndMessage;
      theExp = theExp->nextArg;
      if (ExpressionData(theEnv,execStatus)->svContexts->rtn == TRUE)
        ExpressionData(theEnv,execStatus)->ReturnContext = TRUE;
      if (ExpressionData(theEnv,execStatus)->svContexts->brk == TRUE)
        ExpressionData(theEnv,execStatus)->BreakContext = TRUE;
      IncrementIndentDepth(theEnv,execStatus,3);
      PPCRAndIndent(theEnv,execStatus);
      theExp->nextArg = GroupActions(theEnv,execStatus,infile,&theToken,TRUE,NULL,FALSE);
      DecrementIndentDepth(theEnv,execStatus,3);
      ExpressionData(theEnv,execStatus)->ReturnContext = FALSE;
      ExpressionData(theEnv,execStatus)->BreakContext = FALSE;
      if (theExp->nextArg == NULL)
        goto SwitchParseError;
      theExp = theExp->nextArg;
      PPBackup(theEnv,execStatus);
      PPBackup(theEnv,execStatus);
      SavePPBuffer(theEnv,execStatus,theToken.printForm);
      GetToken(theEnv,execStatus,infile,&theToken);
     }
   DecrementIndentDepth(theEnv,execStatus,3);
   return(top);

SwitchParseErrorAndMessage:
   SyntaxErrorMessage(theEnv,execStatus,"switch function");
SwitchParseError:
   ReturnExpression(theEnv,execStatus,top);
   DecrementIndentDepth(theEnv,execStatus,3);
   return(NULL);
  }

/********************************************************/
/* SearchParsedBindNames:                               */
/********************************************************/
globle int SearchParsedBindNames(
  void *theEnv,
  EXEC_STATUS,
  SYMBOL_HN *name_sought)
  {
   struct BindInfo *var_ptr;
   int theIndex = 1;

   var_ptr = ProcedureParserData(theEnv,execStatus)->ListOfParsedBindNames;
   while (var_ptr != NULL)
     {
      if (var_ptr->name == name_sought)
        { return(theIndex); }
      var_ptr = var_ptr->next;
      theIndex++;
     }

   return(0);
  }

/********************************************************/
/* FindBindConstraints:                               */
/********************************************************/
globle struct constraintRecord *FindBindConstraints(
  void *theEnv,
  EXEC_STATUS,
  SYMBOL_HN *nameSought)
  {
   struct BindInfo *theVariable;

   theVariable = ProcedureParserData(theEnv,execStatus)->ListOfParsedBindNames;
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
globle int CountParsedBindNames(
  void *theEnv,
  EXEC_STATUS)
  {
   struct BindInfo *theVariable;
   int theIndex = 0;

   theVariable = ProcedureParserData(theEnv,execStatus)->ListOfParsedBindNames;
   while (theVariable != NULL)
     {
      theVariable = theVariable->next;
      theIndex++;
     }

   return(theIndex);
  }

/****************************************************************/
/* AddBindName: Adds a variable name used as the first argument */
/*   of the bind function to the list of variable names parsed  */
/*   within the current semantic context (e.g. RHS of a rule).  */
/****************************************************************/
static int AddBindName(
  void *theEnv,
  EXEC_STATUS,
  SYMBOL_HN *variableName,
  CONSTRAINT_RECORD *theConstraint)
  {
   CONSTRAINT_RECORD *tmpConstraint;
   struct BindInfo *currentBind, *lastBind;
   int theIndex = 1;

   /*=========================================================*/
   /* Look for the variable name in the list of bind variable */
   /* names already parsed. If it is found, then return the   */
   /* index to the variable and union the new constraint      */
   /* information with the old constraint information.        */
   /*=========================================================*/

   lastBind = NULL;
   currentBind = ProcedureParserData(theEnv,execStatus)->ListOfParsedBindNames;
   while (currentBind != NULL)
     {
      if (currentBind->name == variableName)
        {
         if (theConstraint != NULL)
           {
            tmpConstraint = currentBind->constraints;
            currentBind->constraints = UnionConstraints(theEnv,execStatus,theConstraint,currentBind->constraints);
            RemoveConstraint(theEnv,execStatus,tmpConstraint);
            RemoveConstraint(theEnv,execStatus,theConstraint);
           }

         return(theIndex);
        }
      lastBind = currentBind;
      currentBind = currentBind->next;
      theIndex++;
     }

   /*===============================================================*/
   /* If the variable name wasn't found, then add it to the list of */
   /* variable names and store the constraint information with it.  */
   /*===============================================================*/

   currentBind = get_struct(theEnv,execStatus,BindInfo);
   currentBind->name = variableName;
   currentBind->constraints = theConstraint;
   currentBind->next = NULL;

   if (lastBind == NULL) ProcedureParserData(theEnv,execStatus)->ListOfParsedBindNames = currentBind;
   else lastBind->next = currentBind;

   return(theIndex);
  }

/********************************************************/
/* RemoveParsedBindName:                                     */
/********************************************************/
globle void RemoveParsedBindName(
  void *theEnv,
  EXEC_STATUS,
  struct symbolHashNode *bname)
  {
   struct BindInfo *prv,*tmp;

   prv = NULL;
   tmp = ProcedureParserData(theEnv,execStatus)->ListOfParsedBindNames;
   while ((tmp != NULL) ? (tmp->name != bname) : FALSE)
     {
      prv = tmp;
      tmp = tmp->next;
     }
   if (tmp != NULL)
     {
      if (prv == NULL)
        ProcedureParserData(theEnv,execStatus)->ListOfParsedBindNames = tmp->next;
      else
        prv->next = tmp->next;

      RemoveConstraint(theEnv,execStatus,tmp->constraints);
      rtn_struct(theEnv,execStatus,BindInfo,tmp);
     }
  }

#endif

#endif


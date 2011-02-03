   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.22  06/15/04            */
   /*                                                     */
   /*             DEFRULE LHS PARSING MODULE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Coordinates parsing of the LHS conditional       */
/*   elements of a rule.                                     */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _RULELHS_SOURCE_

#include "setup.h"

#if (! RUN_TIME) && (! BLOAD_ONLY) &&  DEFRULE_CONSTRUCT

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include "agenda.h"
#include "argacces.h"
#include "constant.h"
#include "constrct.h"
#include "constrnt.h"
#include "cstrnchk.h"
#include "envrnmnt.h"
#include "exprnpsr.h"
#include "memalloc.h"
#include "pattern.h"
#include "reorder.h"
#include "router.h"
#include "ruledef.h"
#include "scanner.h"
#include "symbol.h"

#include "rulelhs.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static struct lhsParseNode    *RuleBodyParse(void *,EXEC_STATUS,char *,struct token *,char *,int *);
   static void                    DeclarationParse(void *,EXEC_STATUS,char *,char *,int *);
   static struct lhsParseNode    *LHSPattern(void *,EXEC_STATUS,char *,int,char *,int *,int,
                                             struct token *,char *);
   static struct lhsParseNode    *ConnectedPatternParse(void *,EXEC_STATUS,char *,struct token *,int *);
   static struct lhsParseNode    *GroupPatterns(void *,EXEC_STATUS,char *,int,char *,int *);
   static struct lhsParseNode    *TestPattern(void *,EXEC_STATUS,char *,int *);
   static struct lhsParseNode    *AssignmentParse(void *,EXEC_STATUS,char *,SYMBOL_HN *,int *);
   static void                    TagLHSLogicalNodes(struct lhsParseNode *);
   static struct lhsParseNode    *SimplePatternParse(void *,EXEC_STATUS,char *,struct token *,int *);
   static void                    ParseSalience(void *,EXEC_STATUS,char *,char *,int *);
   static void                    ParseAutoFocus(void *,EXEC_STATUS,char *,int *);

/*******************************************************************/
/* ParseRuleLHS: Coordinates all the actions necessary for parsing */
/*   the LHS of a rule including the reordering of pattern         */
/*   conditional elements to conform with the KB Rete topology.    */
/*******************************************************************/
globle struct lhsParseNode *ParseRuleLHS(
  void *theEnv,
  EXEC_STATUS,
  char *readSource,
  struct token *theToken,
  char *ruleName,
  int *error)
  {
   struct lhsParseNode *theLHS;
   int result;
   
   *error = FALSE;

   /*========================================*/
   /* Initialize salience parsing variables. */
   /*========================================*/

   PatternData(theEnv,execStatus)->GlobalSalience = 0;
   PatternData(theEnv,execStatus)->GlobalAutoFocus = FALSE;
   PatternData(theEnv,execStatus)->SalienceExpression = NULL;

   /*============================*/
   /* Set the indentation depth. */
   /*============================*/

   SetIndentDepth(theEnv,execStatus,3);

   /*=====================================================*/
   /* Get the raw representation for the LHS of the rule. */
   /*=====================================================*/

   theLHS = RuleBodyParse(theEnv,execStatus,readSource,theToken,ruleName,error);

   if (*error) return(NULL);

   /*====================================================*/
   /* Reorder the raw representation so that it consists */
   /* of at most a single top level OR CE containing one */
   /* or more AND CEs.                                   */
   /*====================================================*/

   theLHS = ReorderPatterns(theEnv,execStatus,theLHS,&result);

   /*================================*/
   /* Return the LHS representation. */
   /*================================*/

   return(theLHS);
  }

/*********************************************************/
/* RuleBodyParse: Parses the LHS of a rule, but does not */
/*   reorder any of the LHS patterns to conform with the */
/*   KB Rete Topology.                                   */
/*                                                       */
/* <rule-body> ::= [<declaration>]                       */
/*                 <conditional-element>*                */
/*                 =>                                    */
/*********************************************************/
static struct lhsParseNode *RuleBodyParse(
  void *theEnv,
  EXEC_STATUS,
  char *readSource,
  struct token *theToken,
  char *ruleName,
  int *error)
  {
   struct lhsParseNode *theNode, *otherNodes;

   /*=============================*/
   /* Set the error return value. */
   /*=============================*/

   *error = FALSE;

   /*==================================================*/
   /* If we're already at the separator, "=>", between */
   /* the LHS and RHS, then the LHS is empty.          */
   /*==================================================*/

   if ((theToken->type == SYMBOL) ?
       (strcmp(ValueToString(theToken->value),"=>") == 0) : FALSE)
     { return(NULL); }

   /*===========================================*/
   /* Parse the first pattern as a special case */
   /* (the declare statement is allowed).       */
   /*===========================================*/

   theNode = LHSPattern(theEnv,execStatus,readSource,SYMBOL,"=>",error,TRUE,theToken,ruleName);

   if (*error == TRUE)
     {
      ReturnLHSParseNodes(theEnv,execStatus,theNode);
      return(NULL);
     }

   PPCRAndIndent(theEnv,execStatus);

   /*======================================*/
   /* Parse the other patterns in the LHS. */
   /*======================================*/

   otherNodes = GroupPatterns(theEnv,execStatus,readSource,SYMBOL,"=>",error);

   if (*error == TRUE)
     {
      ReturnLHSParseNodes(theEnv,execStatus,theNode);
      return(NULL);
     }

   /*================================================*/
   /* Construct the final LHS by combining the first */
   /* pattern with the remaining patterns.           */
   /*================================================*/

   if (theNode == NULL)
     { theNode = otherNodes; }
   else
     { theNode->bottom = otherNodes; }

   /*=======================*/
   /* Return the final LHS. */
   /*=======================*/

   return(theNode);
  }

/********************************************************/
/* DeclarationParse: Parses a defrule declaration.      */
/*                                                      */
/* <declaration> ::= (declare <rule-property>+)         */
/*                                                      */
/* <rule-property> ::= (salience <integer-expression>)  */
/* <rule-property> ::= (auto-focus TRUE | FALSE)        */
/********************************************************/
static void DeclarationParse(
  void *theEnv,
  EXEC_STATUS,
  char *readSource,
  char *ruleName,
  int *error)
  {
   struct token theToken;
   struct expr *packPtr;
   int notDone = TRUE;
   int salienceParsed = FALSE, autoFocusParsed = FALSE;

   /*===========================*/
   /* Next token must be a '('. */
   /*===========================*/

   SavePPBuffer(theEnv,execStatus," ");

   GetToken(theEnv,execStatus,readSource,&theToken);
   if (theToken.type != LPAREN)
     {
      SyntaxErrorMessage(theEnv,execStatus,"declare statement");
      *error = TRUE;
      return;
     }

   /*==========================================*/
   /* Continue parsing until there are no more */
   /* valid rule property declarations.        */
   /*==========================================*/

   while (notDone)
     {
      /*=============================================*/
      /* The name of a rule property must be symbol. */
      /*=============================================*/

      GetToken(theEnv,execStatus,readSource,&theToken);
      if (theToken.type != SYMBOL)
        {
         SyntaxErrorMessage(theEnv,execStatus,"declare statement");
         *error = TRUE;
        }

      /*==============================================*/
      /* Parse a salience declaration if encountered. */
      /*==============================================*/

      else if (strcmp(ValueToString(theToken.value),"salience") == 0)
        {
         if (salienceParsed)
           {
            AlreadyParsedErrorMessage(theEnv,execStatus,"salience declaration",NULL);
            *error = TRUE;
           }
         else
           {
            ParseSalience(theEnv,execStatus,readSource,ruleName,error);
            salienceParsed = TRUE;
           }
        }

      /*=================================================*/
      /* Parse an auto-focus declaration if encountered. */
      /* A global flag is used to indicate if the        */
      /* auto-focus feature for a rule was parsed.       */
      /*=================================================*/

      else if (strcmp(ValueToString(theToken.value),"auto-focus") == 0)
        {
         if (autoFocusParsed)
           {
            AlreadyParsedErrorMessage(theEnv,execStatus,"auto-focus declaration",NULL);
            *error = TRUE;
           }
         else
           {
            ParseAutoFocus(theEnv,execStatus,readSource,error);
            autoFocusParsed = TRUE;
           }
         }

       /*==========================================*/
       /* Otherwise the symbol does not correspond */
       /* to a valid rule property.                */
       /*==========================================*/

       else
        {
         SyntaxErrorMessage(theEnv,execStatus,"declare statement");
         *error = TRUE;
        }

      /*=====================================*/
      /* Return if an error was encountered. */
      /*=====================================*/

      if (*error)
        {
         ReturnExpression(theEnv,execStatus,PatternData(theEnv,execStatus)->SalienceExpression);
         PatternData(theEnv,execStatus)->SalienceExpression = NULL;
         return;
        }

      /*=======================================*/
      /* Both the salience and auto-focus rule */
      /* properties are closed with a ')'.     */
      /*=======================================*/

      GetToken(theEnv,execStatus,readSource,&theToken);
      if (theToken.type != RPAREN)
        {
         PPBackup(theEnv,execStatus);
         SavePPBuffer(theEnv,execStatus," ");
         SavePPBuffer(theEnv,execStatus,theToken.printForm);
         ReturnExpression(theEnv,execStatus,PatternData(theEnv,execStatus)->SalienceExpression);
         PatternData(theEnv,execStatus)->SalienceExpression = NULL;
         SyntaxErrorMessage(theEnv,execStatus,"declare statement");
         *error = TRUE;
         return;
        }

      /*=============================================*/
      /* The declare statement is closed with a ')'. */
      /*=============================================*/

      GetToken(theEnv,execStatus,readSource,&theToken);
      if (theToken.type == RPAREN) notDone = FALSE;
      else if (theToken.type != LPAREN)
        {
         ReturnExpression(theEnv,execStatus,PatternData(theEnv,execStatus)->SalienceExpression);
         PatternData(theEnv,execStatus)->SalienceExpression = NULL;
         SyntaxErrorMessage(theEnv,execStatus,"declare statement");
         *error = TRUE;
         return;
        }
      else
        {
         PPBackup(theEnv,execStatus);
         SavePPBuffer(theEnv,execStatus," (");
        }
     }

   /*==========================================*/
   /* Return the value of the salience through */
   /* the global variable SalienceExpression.  */
   /*==========================================*/

   packPtr = PackExpression(theEnv,execStatus,PatternData(theEnv,execStatus)->SalienceExpression);
   ReturnExpression(theEnv,execStatus,PatternData(theEnv,execStatus)->SalienceExpression);
   PatternData(theEnv,execStatus)->SalienceExpression = packPtr;
   return;
  }

/************************************************************/
/* ParseSalience: Parses the rest of a defrule salience     */
/*   declaration once the salience keyword has been parsed. */
/************************************************************/
static void ParseSalience(
  void *theEnv,
  EXEC_STATUS,
  char *readSource,
  char *ruleName,
  int *error)
  {
   int salience;
   DATA_OBJECT salienceValue;

   /*==============================*/
   /* Get the salience expression. */
   /*==============================*/

   SavePPBuffer(theEnv,execStatus," ");

   PatternData(theEnv,execStatus)->SalienceExpression = ParseAtomOrExpression(theEnv,execStatus,readSource,NULL);
   if (PatternData(theEnv,execStatus)->SalienceExpression == NULL)
     {
      *error = TRUE;
      return;
     }

   /*============================================================*/
   /* Evaluate the expression and determine if it is an integer. */
   /*============================================================*/

   SetEvaluationError(theEnv,execStatus,FALSE);
   if (EvaluateExpression(theEnv,execStatus,PatternData(theEnv,execStatus)->SalienceExpression,&salienceValue))
     {
      SalienceInformationError(theEnv,execStatus,"defrule",ruleName);
      *error = TRUE;
      return;
     }

   if (salienceValue.type != INTEGER)
     {
      SalienceNonIntegerError(theEnv,execStatus);
      *error = TRUE;
      return;
     }

   /*=======================================================*/
   /* Salience number must be in the range -10000 to 10000. */
   /*=======================================================*/

   salience = (int) ValueToLong(salienceValue.value);

   if ((salience > MAX_DEFRULE_SALIENCE) || (salience < MIN_DEFRULE_SALIENCE))
     {
      SalienceRangeError(theEnv,execStatus,MIN_DEFRULE_SALIENCE,MAX_DEFRULE_SALIENCE);
      *error = TRUE;
      return;
     }

   /*==========================================*/
   /* If the expression is a constant integer, */
   /* don't bother storing the expression.     */
   /*==========================================*/

   if (PatternData(theEnv,execStatus)->SalienceExpression->type == INTEGER)
     {
      ReturnExpression(theEnv,execStatus,PatternData(theEnv,execStatus)->SalienceExpression);
      PatternData(theEnv,execStatus)->SalienceExpression = NULL;
     }

   PatternData(theEnv,execStatus)->GlobalSalience = salience;
  }

/**************************************************************/
/* ParseAutoFocus: Parses the rest of a defrule auto-focus    */
/*   declaration once the auto-focus keyword has been parsed. */
/**************************************************************/
static void ParseAutoFocus(
  void *theEnv,
  EXEC_STATUS,
  char *readSource,
  int *error)
  {
   struct token theToken;

   /*========================================*/
   /* The auto-focus value must be a symbol. */
   /*========================================*/

   SavePPBuffer(theEnv,execStatus," ");

   GetToken(theEnv,execStatus,readSource,&theToken);
   if (theToken.type != SYMBOL)
     {
      SyntaxErrorMessage(theEnv,execStatus,"auto-focus statement");
      *error = TRUE;
      return;
     }

   /*====================================================*/
   /* The auto-focus value must be either TRUE or FALSE. */
   /* If a valid value is parsed, then set the value of  */
   /* the global variable GlobalAutoFocus.               */
   /*====================================================*/

   if (strcmp(ValueToString(theToken.value),"TRUE") == 0)
     { PatternData(theEnv,execStatus)->GlobalAutoFocus = TRUE; }
   else if (strcmp(ValueToString(theToken.value),"FALSE") == 0)
     { PatternData(theEnv,execStatus)->GlobalAutoFocus = FALSE; }
   else
     {
      SyntaxErrorMessage(theEnv,execStatus,"auto-focus statement");
      *error = TRUE;
     }
  }

/*****************************************************************/
/* LHSPattern: Parses a single conditional element found on the  */
/*   LHS of a rule. Conditonal element types include pattern CEs */
/*   (which may be assigned to a variable), test CEs, not CEs,   */
/*   logical CEs, and CEs, and or CEs.                           */
/*                                                               */
/* <conditional-element> ::= <pattern-CE> |                      */
/*                           <assigned-pattern-CE> |             */
/*                           <not-CE> | <and-CE> | <or-CE> |     */
/*                           <logical-CE> | <test-CE> |          */
/*                           <forall-CE> | <exists-CE>           */
/*****************************************************************/
static struct lhsParseNode *LHSPattern(
  void *theEnv,
  EXEC_STATUS,
  char *readSource,
  int terminator,
  char *terminatorString,
  int *error,
  int allowDeclaration,
  struct token *firstToken,
  char *ruleName)
  {
   struct token theToken;
   struct lhsParseNode *theNode;

   /*=========================================================*/
   /* Check to see if the first token has already been read.  */
   /* This should only occur for the first pattern in a rule. */
   /*=========================================================*/

   if (firstToken == NULL) GetToken(theEnv,execStatus,readSource,&theToken);
   else CopyToken(&theToken,firstToken);

   /*=====================================================*/
   /* A left parenthesis begins all CEs and declarations. */
   /*=====================================================*/

   if (theToken.type == LPAREN)
     {
      /*================================================*/
      /* The first field of a pattern must be a symbol. */
      /*================================================*/

      GetToken(theEnv,execStatus,readSource,&theToken);
      if (theToken.type != SYMBOL)
        {
         SyntaxErrorMessage(theEnv,execStatus,"the first field of a pattern");
         *error = TRUE;
         return(NULL);
        }

      /*====================================*/
      /* If this is the first CE of a rule, */
      /* then a declare statement is valid. */
      /*====================================*/

      if (allowDeclaration &&
          (strcmp(ValueToString(theToken.value),"declare") == 0))
        {
         if (ruleName == NULL) SystemError(theEnv,execStatus,"RULELHS",1);
         DeclarationParse(theEnv,execStatus,readSource,ruleName,error);
         theNode = NULL;
        }

      /*==================================*/
      /* Otherwise check for a *test* CE. */
      /*==================================*/

      else if (strcmp(ValueToString(theToken.value),"test") == 0)
        { theNode = TestPattern(theEnv,execStatus,readSource,error); }

      /*============================================*/
      /* Otherwise check for an *and*, *or*, *not*, */
      /* *logical*, *exists*, or *forall* CE.       */
      /*============================================*/

      else if ((strcmp(ValueToString(theToken.value),"and") == 0) ||
               (strcmp(ValueToString(theToken.value),"logical") == 0) ||
               (strcmp(ValueToString(theToken.value),"not") == 0) ||
               (strcmp(ValueToString(theToken.value),"exists") == 0) ||
               (strcmp(ValueToString(theToken.value),"forall") == 0) ||
               (strcmp(ValueToString(theToken.value),"or") == 0))
        { theNode = ConnectedPatternParse(theEnv,execStatus,readSource,&theToken,error); }

      /*=================================*/
      /* Otherwise parse a *pattern* CE. */
      /*=================================*/

      else
        { theNode = SimplePatternParse(theEnv,execStatus,readSource,&theToken,error); }
     }

   /*=======================================*/
   /* Check for a pattern address variable. */
   /*=======================================*/

   else if (theToken.type == SF_VARIABLE)
     { theNode = AssignmentParse(theEnv,execStatus,readSource,(SYMBOL_HN *) theToken.value,error); }

   /*=================================================*/
   /* Check for the group terminator (either a "=>"   */
   /* separating the LHS from the RHS or a ")" ending */
   /* a CE containing other CEs such as an *and* CE). */
   /*=================================================*/

   else if ((theToken.type == terminator) ?
            (strcmp(theToken.printForm,terminatorString) == 0) : FALSE)
     { return(NULL);  }

   /*====================================*/
   /* Otherwise invalid syntax was used. */
   /*====================================*/

   else
     {
      SyntaxErrorMessage(theEnv,execStatus,"defrule");
      *error = TRUE;
      return(NULL);
     }

   /*================================*/
   /* If an error occurred, free any */
   /* allocated data structures.     */
   /*================================*/

   if (*error == TRUE)
     {
      ReturnLHSParseNodes(theEnv,execStatus,theNode);
      return(NULL);
     }

   /*=========================*/
   /* Return the LHS pattern. */
   /*=========================*/

   return(theNode);
  }

/*********************************************************************/
/* ConnectedPatternParse:  Handles parsing of the connected          */
/*   conditional elements (i.e. those conditional elements that may  */
/*   contain one or more other conditional elements).  The connected */
/*   conditional elements include the *and*, *or*, *not*, *logical*, */
/*   *exists*, and *forall* CEs. This routine is entered with the    */
/*   parsing pointing to the name of the connected CE. It is exited  */
/*   with the parser pointing to the closing right parenthesis of    */
/*   the connected CE.                                               */
/*                                                                   */
/* <and-CE>      ::= (and <conditional-element>+)                    */
/*                                                                   */
/* <or-CE>       ::= (or <conditional-element>+)                     */
/*                                                                   */
/* <logical-CE>  ::= (logical <conditional-element>+)                */
/*                                                                   */
/* <not-CE>      ::= (not <conditional-element>)                     */
/*                                                                   */
/* <exists-CE>   ::= (exists <conditional-element>+)                 */
/*                                                                   */
/* <forall-CE>   ::= (forall <conditional-element>                   */
/*                           <conditional-element>+)                 */
/*********************************************************************/
static struct lhsParseNode *ConnectedPatternParse(
  void *theEnv,
  EXEC_STATUS,
  char *readSource,
  struct token *theToken,
  int *error)
  {
   unsigned short connectorValue = 0;
   struct lhsParseNode *theNode, *tempNode, *theGroup;
   char *errorCE = NULL;
   int logical = FALSE;
   int tempValue;

   /*==========================================================*/
   /* Use appropriate spacing for pretty printing of the rule. */
   /*==========================================================*/

   IncrementIndentDepth(theEnv,execStatus,5);
   if (strcmp(ValueToString(theToken->value),"or") == 0)
     {
      connectorValue = OR_CE;
      errorCE = "the or conditional element";
      SavePPBuffer(theEnv,execStatus,"  ");
     }
   else if (strcmp(ValueToString(theToken->value),"and") == 0)
     {
      connectorValue = AND_CE;
      errorCE = "the and conditional element";
      SavePPBuffer(theEnv,execStatus," ");
     }
   else if (strcmp(ValueToString(theToken->value),"not") == 0)
     {
      connectorValue = NOT_CE;
      errorCE = "the not conditional element";
      SavePPBuffer(theEnv,execStatus," ");
     }
   else if (strcmp(ValueToString(theToken->value),"exists") == 0)
     {
      connectorValue = EXISTS_CE;
      errorCE = "the exists conditional element";
      PPCRAndIndent(theEnv,execStatus);
     }
   else if (strcmp(ValueToString(theToken->value),"forall") == 0)
     {
      connectorValue = FORALL_CE;
      errorCE = "the forall conditional element";
      PPCRAndIndent(theEnv,execStatus);
     }
   else if (strcmp(ValueToString(theToken->value),"logical") == 0)
     {
      connectorValue = AND_CE;
      errorCE = "the logical conditional element";
      logical = TRUE;
      PPCRAndIndent(theEnv,execStatus);
     }

   /*=====================================================*/
   /* The logical CE cannot be contained within a not CE. */
   /*=====================================================*/

   if (PatternData(theEnv,execStatus)->WithinNotCE && logical)
     {
      PrintErrorID(theEnv,execStatus,"RULELHS",1,TRUE);
      EnvPrintRouter(theEnv,WERROR,"The logical CE cannot be used within a not/exists/forall CE.\n");
      *error = TRUE;
      return(NULL);
     }

   /*=====================================================*/
   /* Remember if we're currently within a *not* CE and   */
   /* then check to see if we're entering a new *not* CE. */
   /*=====================================================*/

      tempValue = PatternData(theEnv,execStatus)->WithinNotCE;
   if ((connectorValue == NOT_CE) ||
       (connectorValue == EXISTS_CE) ||
       (connectorValue == FORALL_CE))
     { PatternData(theEnv,execStatus)->WithinNotCE = TRUE; }

   /*===========================================*/
   /* Parse all of the CEs contained with the   */
   /* CE. A ) will terminate the end of the CE. */
   /*===========================================*/

   theGroup = GroupPatterns(theEnv,execStatus,readSource,RPAREN,")",error);

   /*====================================*/
   /* Restore the "with a *not* CE" flag */
   /* and reset the indentation depth.   */
   /*====================================*/

   PatternData(theEnv,execStatus)->WithinNotCE = tempValue;
   DecrementIndentDepth(theEnv,execStatus,5);

   /*============================================*/
   /* If an error occured while parsing, return. */
   /*============================================*/

   if (*error == TRUE)
     {
      ReturnLHSParseNodes(theEnv,execStatus,theGroup);
      return(NULL);
     }

   /*=========================================================*/
   /* If we parsed a *logical* CE, then mark the logical flag */
   /* for all of the CEs contained within the logical CE.     */
   /*=========================================================*/

   if (logical) TagLHSLogicalNodes(theGroup);

   /*=====================================================*/
   /* All the connected CEs must contain at least one CE. */
   /*=====================================================*/

   if (theGroup == NULL)
     {
      SyntaxErrorMessage(theEnv,execStatus,errorCE);
      *error = TRUE;
      return(NULL);
     }

   /*============================================*/
   /* A not CE may not contain more than one CE. */
   /*============================================*/

   if ((connectorValue == NOT_CE) && (theGroup->bottom != NULL))
     {
      SyntaxErrorMessage(theEnv,execStatus,errorCE);
      ReturnLHSParseNodes(theEnv,execStatus,theGroup);
      *error = TRUE;
      return(NULL);
     }

   /*============================================*/
   /* A forall CE must contain at least two CEs. */
   /*============================================*/

   if ((connectorValue == FORALL_CE) && (theGroup->bottom == NULL))
     {
      SyntaxErrorMessage(theEnv,execStatus,errorCE);
      ReturnLHSParseNodes(theEnv,execStatus,theGroup);
      *error = TRUE;
      return(NULL);
     }

   /*========================================================*/
   /* Remove an "and" and "or" CE that only contains one CE. */
   /*========================================================*/

   if (((connectorValue == AND_CE) || (connectorValue == OR_CE)) &&
       (theGroup->bottom == NULL))
     {
      theGroup->logical = logical;
      return(theGroup);
     }

   /*===========================================================*/
   /* Create the top most node which connects the CEs together. */
   /*===========================================================*/

   theNode = GetLHSParseNode(theEnv,execStatus);
   theNode->logical = logical;

   /*======================================================*/
   /* Attach and/or/not CEs directly to the top most node. */
   /*======================================================*/

   if ((connectorValue == AND_CE) ||
       (connectorValue == OR_CE) ||
       (connectorValue == NOT_CE))
     {
      theNode->type = connectorValue;
      theNode->right = theGroup;
     }

   /*=================================================================*/
   /* Wrap two not CEs around the patterns contained in an exists CE. */
   /*=================================================================*/

   else if (connectorValue == EXISTS_CE)
     {
      theNode->type = NOT_CE;

      theNode->right = GetLHSParseNode(theEnv,execStatus);
      theNode->right->type = NOT_CE;
      theNode->right->logical = logical;

      if (theGroup->bottom != NULL)
        {
         theNode->right->right = GetLHSParseNode(theEnv,execStatus);
         theNode->right->right->type = AND_CE;
         theNode->right->right->logical = logical;
         theNode->right->right->right = theGroup;
        }
      else
        { theNode->right->right = theGroup; }
     }

   /*==================================================*/
   /* For a forall CE, wrap a not CE around all of the */
   /* CEs and a not CE around the 2nd through nth CEs. */
   /*==================================================*/

   else if (connectorValue == FORALL_CE)
     {
      theNode->type = NOT_CE;

      tempNode = theGroup->bottom;
      theGroup->bottom = NULL;

      theNode->right = GetLHSParseNode(theEnv,execStatus);
      theNode->right->type = AND_CE;
      theNode->right->logical = logical;
      theNode->right->right = theGroup;

      theGroup = tempNode;

      theNode->right->right->bottom = GetLHSParseNode(theEnv,execStatus);
      theNode->right->right->bottom->type = NOT_CE;
      theNode->right->right->bottom->logical = logical;

      tempNode = theNode->right->right->bottom;

      if (theGroup->bottom == NULL)
        { tempNode->right = theGroup; }
      else
        {
         tempNode->right = GetLHSParseNode(theEnv,execStatus);
         tempNode->right->type = AND_CE;
         tempNode->right->logical = logical;
         tempNode->right->right = theGroup;
        }
     }

   /*================*/
   /* Return the CE. */
   /*================*/

   return(theNode);
  }

/***********************************************/
/* GroupPatterns: Groups a series of connected */
/*   conditional elements together.            */
/***********************************************/
static struct lhsParseNode *GroupPatterns(
  void *theEnv,
  EXEC_STATUS,
  char *readSource,
  int terminator,
  char *terminatorString,
  int *error)
  {
   struct lhsParseNode *lastNode, *newNode, *theNode;

   lastNode = theNode = NULL;

   while (TRUE)
     {
      /*==================*/
      /* Get the next CE. */
      /*==================*/

      newNode = LHSPattern(theEnv,execStatus,readSource,terminator,terminatorString,
                           error,FALSE,NULL,NULL);

      /*=======================================================*/
      /* If an error occurred, release any LHS data structures */
      /* previously allocated by this routine.                 */
      /*=======================================================*/

      if (*error)
        {
         ReturnLHSParseNodes(theEnv,execStatus,theNode);
         return(NULL);
        }

      /*===============================================*/
      /* A NULL value for the CE just parsed indicates */
      /* that the terminator for the group of patterns */
      /* was encountered (either a "=>" or a ")".      */
      /*===============================================*/

      if (newNode == NULL)
        {
         PPBackup(theEnv,execStatus);
         PPBackup(theEnv,execStatus);
         if (terminator == RPAREN)
           { SavePPBuffer(theEnv,execStatus,terminatorString); }
         else
           {
            PPCRAndIndent(theEnv,execStatus);
            SavePPBuffer(theEnv,execStatus,terminatorString);
           }

         return(theNode);
        }

      /*============================*/
      /* Add the new CE to the list */
      /* of CEs being grouped.      */
      /*============================*/

      if (lastNode == NULL)
        { theNode = newNode; }
      else
        { lastNode->bottom = newNode; }

      lastNode = newNode;

      /*======================================*/
      /* Fix the pretty print representation. */
      /*======================================*/

      PPCRAndIndent(theEnv,execStatus);
     }
  }

/**************************************************************/
/* TestPattern: Handles parsing of test conditional elements. */
/*                                                            */
/* <test-CE> ::= (test <function-call>)                       */
/**************************************************************/
static struct lhsParseNode *TestPattern(
  void *theEnv,
  EXEC_STATUS,
  char *readSource,
  int *error)
  {
   struct lhsParseNode *theNode;
   struct token theToken;
   struct expr *theExpression;

   /*================================================*/
   /* Create the data specification for the test CE. */
   /*================================================*/

   SavePPBuffer(theEnv,execStatus," ");
   theNode = GetLHSParseNode(theEnv,execStatus);
   theNode->type = TEST_CE;
   theExpression = Function0Parse(theEnv,execStatus,readSource);
   theNode->expression = ExpressionToLHSParseNodes(theEnv,execStatus,theExpression);
   ReturnExpression(theEnv,execStatus,theExpression);

   if (theNode->expression == NULL)
     {
      *error = TRUE;
      ReturnLHSParseNodes(theEnv,execStatus,theNode);
      return(NULL);
     }

   /*=========================================================*/
   /* Check for the closing right parenthesis of the test CE. */
   /*=========================================================*/

   GetToken(theEnv,execStatus,readSource,&theToken);
   if (theToken.type != RPAREN)
     {
      SyntaxErrorMessage(theEnv,execStatus,"test conditional element");
      *error = TRUE;
      ReturnLHSParseNodes(theEnv,execStatus,theNode);
      return(NULL);
     }

   /*=====================*/
   /* Return the test CE. */
   /*=====================*/

   return(theNode);
  }

/****************************************************************/
/* AssignmentParse: Finishes the parsing of pattern conditional */
/*   elements that have been bound to a  variable.              */
/*                                                              */
/* <assigned-pattern-CE> ::= ?<variable-symbol> <- <pattern-CE> */
/****************************************************************/
static struct lhsParseNode *AssignmentParse(
  void *theEnv,
  EXEC_STATUS,
  char *readSource,
  SYMBOL_HN *factAddress,
  int *error)
  {
   struct lhsParseNode *theNode;
   struct token theToken;

   /*=====================================================*/
   /* Patterns cannot be bound if they are with a not CE. */
   /*=====================================================*/

   if (PatternData(theEnv,execStatus)->WithinNotCE)
     {
      PrintErrorID(theEnv,execStatus,"RULELHS",2,TRUE);
      EnvPrintRouter(theEnv,WERROR,"A pattern CE cannot be bound to a pattern-address within a not CE\n");
      *error = TRUE;
      return(NULL);
     }

   /*===============================================*/
   /* Check for binder token, "<-", after variable. */
   /*===============================================*/

   SavePPBuffer(theEnv,execStatus," ");

   GetToken(theEnv,execStatus,readSource,&theToken);

   if ((theToken.type == SYMBOL) ? (strcmp(ValueToString(theToken.value),"<-") != 0) :
                                   TRUE)
     {
      SyntaxErrorMessage(theEnv,execStatus,"binding patterns");
      *error = TRUE;
      return(NULL);
     }

   SavePPBuffer(theEnv,execStatus," ");

   /*================================================*/
   /* Check for opening left parenthesis of pattern. */
   /*================================================*/

   GetToken(theEnv,execStatus,readSource,&theToken);
   if (theToken.type != LPAREN)
     {
      SyntaxErrorMessage(theEnv,execStatus,"binding patterns");
      *error = TRUE;
      return(NULL);
     }

   /*======================================================*/
   /* Parse the pattern and return the data specification. */
   /*======================================================*/

   GetToken(theEnv,execStatus,readSource,&theToken);

   theNode = SimplePatternParse(theEnv,execStatus,readSource,&theToken,error);

   if (*error == TRUE)
     {
      ReturnLHSParseNodes(theEnv,execStatus,theNode);
      return(NULL);
     }

   /*=============================================*/
   /* Store the name of the variable to which the */
   /* pattern is bound and return the pattern.    */
   /*=============================================*/

   theNode->value = (void *) factAddress;
   return(theNode);
  }

/************************************************************/
/* TagLHSLogicalNodes: Marks all *and*, *or*, and *not* CEs */
/*   contained within a logical CE as having the properties */
/*   associated with a logical CE.                          */
/************************************************************/
static void TagLHSLogicalNodes(
  struct lhsParseNode *nodePtr)
  {
   while (nodePtr != NULL)
     {
      nodePtr->logical = TRUE;
      if ((nodePtr->type == AND_CE) ||
          (nodePtr->type == OR_CE) ||
          (nodePtr->type == NOT_CE))
        { TagLHSLogicalNodes(nodePtr->right); }
      nodePtr = nodePtr->bottom;
     }
  }

/***********************************************************/
/* SimplePatternParse: Parses a simple pattern (an opening */
/*   parenthesis followed by one or more fields followed   */
/*   by a closing parenthesis).                            */
/*                                                         */
/* <pattern-CE> ::= <ordered-pattern-CE> |                 */
/*                  <template-pattern-CE>                  */
/***********************************************************/
static struct lhsParseNode *SimplePatternParse(
  void *theEnv,
  EXEC_STATUS,
  char *readSource,
  struct token *theToken,
  int *error)
  {
   struct lhsParseNode *theNode;
   struct patternParser *tempParser;

   /*=================================================*/
   /* The first field of a pattern must be a symbol.  */
   /* In addition, the symbols ":" and "=" can not    */
   /* be used because they have special significance. */
   /*=================================================*/

   if (theToken->type != SYMBOL)
     {
      SyntaxErrorMessage(theEnv,execStatus,"the first field of a pattern");
      *error = TRUE;
      return(NULL);
     }
   else if ((strcmp(ValueToString(theToken->value),"=") == 0) ||
            (strcmp(ValueToString(theToken->value),":") == 0))
     {
      SyntaxErrorMessage(theEnv,execStatus,"the field field of a pattern");
      *error = TRUE;
      return(NULL);
     }

   /*===============================================*/
   /* Construct the topmost node of the pattern CE. */
   /*===============================================*/

   theNode = GetLHSParseNode(theEnv,execStatus);
   theNode->type = PATTERN_CE;
   theNode->negated = FALSE;
   theNode->exists = FALSE;

   /*======================================================*/
   /* Search for a pattern parser that claims the pattern. */
   /*======================================================*/

   for (tempParser = PatternData(theEnv,execStatus)->ListOfPatternParsers;
        tempParser != NULL;
        tempParser = tempParser->next)
     {
      if ((*tempParser->recognizeFunction)((SYMBOL_HN *) theToken->value))
        {
         theNode->patternType = tempParser;
         theNode->right = (*tempParser->parseFunction)(theEnv,execStatus,readSource,theToken);
         if (theNode->right == NULL)
           {
            *error = TRUE;
            ReturnLHSParseNodes(theEnv,execStatus,theNode);
            return(NULL);
           }

         PropagatePatternType(theNode,tempParser);
         return(theNode);
        }
     }

   /*=================================*/
   /* If a pattern parser couldn't be */
   /* found, then signal an error.    */
   /*=================================*/

   *error = TRUE;
   SyntaxErrorMessage(theEnv,execStatus,"the field field of a pattern");
   ReturnLHSParseNodes(theEnv,execStatus,theNode);
   return(NULL);
  }

/**************************************************************/
/* PropagatePatternType: Sets the selfPattern field for all   */
/*   lhsParseNodes in a linked list of those data structures. */
/**************************************************************/
globle void PropagatePatternType(
  struct lhsParseNode *theLHS,
  struct patternParser *theParser)
  {
   while (theLHS != NULL)
     {
      theLHS->patternType = theParser;
      if (theLHS->right != NULL) PropagatePatternType(theLHS->right,theParser);
      if (theLHS->expression != NULL) PropagatePatternType(theLHS->expression,theParser);
      theLHS = theLHS->bottom;
     }
  }

#endif /* (! RUN_TIME) && (! BLOAD_ONLY) &&  DEFRULE_CONSTRUCT */



   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*            EXPRESSION PARSER HEADER FILE            */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides routines for parsing expressions.       */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#ifndef _H_exprnpsr

#define _H_exprnpsr

#if (! RUN_TIME)

typedef struct saved_contexts
  {
   int rtn;
   int brk;
   struct saved_contexts *nxt;
  } SAVED_CONTEXTS;

#endif

#ifndef _H_extnfunc
#include "extnfunc.h"
#endif
#ifndef _H_scanner
#include "scanner.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _EXPRNPSR_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define GetSequenceOperatorRecognition() EnvGetSequenceOperatorRecognition(GetCurrentEnvironment(),GetCurrentExecutionState())
#define SetSequenceOperatorRecognition(a) EnvSetSequenceOperatorRecognition(GetCurrentEnvironment(),GetCurrentExecutionState(),a)

   LOCALE struct expr                   *Function0Parse(void *,EXEC_STATUS,char *);
   LOCALE struct expr                   *Function1Parse(void *,EXEC_STATUS,char *);
   LOCALE struct expr                   *Function2Parse(void *,EXEC_STATUS,char *,char *);
   LOCALE void                           PushRtnBrkContexts(void *,EXEC_STATUS);
   LOCALE void                           PopRtnBrkContexts(void *,EXEC_STATUS);
   LOCALE intBool                        ReplaceSequenceExpansionOps(void *,EXEC_STATUS,struct expr *,struct expr *,
                                                                     void *,void *);
   LOCALE struct expr                   *CollectArguments(void *,EXEC_STATUS,struct expr *,char *);
   LOCALE struct expr                   *ArgumentParse(void *,EXEC_STATUS,char *,int *);
   LOCALE struct expr                   *ParseAtomOrExpression(void *,EXEC_STATUS,char *,struct token *);
   LOCALE EXPRESSION                    *ParseConstantArguments(void *,EXEC_STATUS,char *,int *);
   LOCALE intBool                        EnvSetSequenceOperatorRecognition(void *,EXEC_STATUS,int);
   LOCALE intBool                        EnvGetSequenceOperatorRecognition(void *,EXEC_STATUS);
   LOCALE struct expr                   *GroupActions(void *,EXEC_STATUS,char *,struct token *,int,char *,int);
   LOCALE struct expr                   *RemoveUnneededProgn(void *,EXEC_STATUS,struct expr *);
#if (! RUN_TIME)
   LOCALE int                     CheckExpressionAgainstRestrictions(void *,EXEC_STATUS,struct expr *,char *,char *);
#endif

#endif





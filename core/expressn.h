   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*               EXPRESSION HEADER FILE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains routines for creating, deleting,        */
/*   compacting, installing, and hashing expressions.        */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_expressn

#define _H_expressn

struct expr;
struct exprHashNode;

#ifndef _H_exprnops
#include "exprnops.h"
#endif

/******************************/
/* Expression Data Structures */
/******************************/

struct expr
   {
    short type;
    void *value;
    struct expr *argList;
    struct expr *nextArg;
   };

#define arg_list argList
#define next_arg nextArg

typedef struct expr EXPRESSION;

typedef struct exprHashNode
  {
   unsigned hashval;
   unsigned count;
   struct expr *exp;
   struct exprHashNode *nxt;
   long bsaveID;
  } EXPRESSION_HN;

#define EXPRESSION_HASH_SIZE 503

/*************************/
/* Type and Value Macros */
/*************************/

#define GetType(target)         ((target).type)
#define GetpType(target)        ((target)->type)
#define SetType(target,val)     ((target).type = (val))
#define SetpType(target,val)    ((target)->type = (val))
#define GetValue(target)        ((target).value)
#define GetpValue(target)       ((target)->value)
#define SetValue(target,val)    ((target).value = (void *) (val))
#define SetpValue(target,val)   ((target)->value = (void *) (val))

/********************/
/* Global Functions */
/********************/

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _EXPRESSN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           ReturnExpression(struct expr *);
   LOCALE void                           ExpressionInstall(struct expr *);
   LOCALE void                           ExpressionDeinstall(struct expr *);
   LOCALE struct expr                   *CopyExpression(struct expr *);
   LOCALE struct expr                   *PackExpression(struct expr *);
   LOCALE void                           ReturnPackedExpression(struct expr *);
   LOCALE void                           InitExpressionData(void);
   LOCALE void                           InitExpressionPointers(void);
#if (! BLOAD_ONLY) && (! RUN_TIME)
   LOCALE EXPRESSION                    *AddHashedExpression(EXPRESSION *);
#endif
#if (! RUN_TIME)
   LOCALE void                           RemoveHashedExpression(EXPRESSION *);
#endif
#if BLOAD_AND_BSAVE || BLOAD_ONLY || BLOAD || CONSTRUCT_COMPILER
   LOCALE long                           HashedExpressionIndex(EXPRESSION *);
#endif

/********************/
/* Global Variables */
/********************/

#ifndef _EXPRESSN_SOURCE_
   extern Thread void                          *PTR_AND;
   extern Thread void                          *PTR_OR;
   extern Thread void                          *PTR_EQ;
   extern Thread void                          *PTR_NEQ;
   extern Thread void                          *PTR_NOT;
   extern Thread EXPRESSION_HN                **ExpressionHashTable;
#endif

#endif





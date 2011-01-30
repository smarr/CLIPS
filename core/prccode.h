   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/09/97          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_prccode
#define _H_prccode

#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_moduldef
#include "moduldef.h"
#endif
#ifndef _H_scanner
#include "scanner.h"
#endif
#ifndef _H_symbol
#include "symbol.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _PRCCODE_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

LOCALE void InstallProcedurePrimitives(void);

#if (! BLOAD_ONLY) && (! RUN_TIME)

#if DEFFUNCTION_CONSTRUCT || OBJECT_SYSTEM
LOCALE EXPRESSION *ParseProcParameters(char *,struct token *,EXPRESSION *,
                                       SYMBOL_HN **,int *,int *,int *,int (*)(char *));
#endif
LOCALE EXPRESSION *ParseProcActions(char *,char *,struct token *,EXPRESSION *,SYMBOL_HN *,
                                    int (*)(EXPRESSION *,void *),int (*)(EXPRESSION *,void *),
                                    int *,void *);
LOCALE BOOLEAN ReplaceProcVars(char *,EXPRESSION *,EXPRESSION *,SYMBOL_HN *,
                                     int (*)(EXPRESSION *,void *),void *);
#if DEFGENERIC_CONSTRUCT
LOCALE EXPRESSION *GenProcWildcardReference(int);
#endif
#endif

LOCALE void PushProcParameters(EXPRESSION *,int,char *,char *,void (*)(void));
LOCALE void PopProcParameters(void);

#if DEFGENERIC_CONSTRUCT
LOCALE EXPRESSION *GetProcParamExpressions(void);
#endif

LOCALE void EvaluateProcActions(struct defmodule *,EXPRESSION *,int,
                                DATA_OBJECT *,void (*)(void));
LOCALE void PrintProcParamArray(char *);
LOCALE void GrabProcWildargs(DATA_OBJECT *,int);

#ifndef _GENRCFUN_SOURCE_
extern Thread void * NoParamValue;
extern Thread DATA_OBJECT *ProcParamArray;
extern Thread int ProcParamArraySize;
extern Thread EXPRESSION *CurrentProcActions;
#endif

#endif






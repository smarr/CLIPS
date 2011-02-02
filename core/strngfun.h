   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.21  06/15/03            */
   /*                                                     */
   /*             STRING FUNCTIONS HEADER FILE            */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_strngfun

#define _H_strngfun

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _STRNGFUN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ALLOW_ENVIRONMENT_GLOBALS
   LOCALE int                            Build(char *);
   LOCALE int                            Eval(char *,DATA_OBJECT_PTR);
#endif

   LOCALE int                            EnvBuild(void *,EXEC_STATUS,char *);
   LOCALE int                            EnvEval(void *,EXEC_STATUS,char *,DATA_OBJECT_PTR);
   LOCALE void                           StringFunctionDefinitions(void *,EXEC_STATUS);
   LOCALE void                           StrCatFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                           SymCatFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE long long                      StrLengthFunction(void *,EXEC_STATUS);
   LOCALE void                           UpcaseFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                           LowcaseFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE long long                      StrCompareFunction(void *,EXEC_STATUS);
   LOCALE void                          *SubStringFunction(void *,EXEC_STATUS);
   LOCALE void                           StrIndexFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                           EvalFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE int                            BuildFunction(void *,EXEC_STATUS);
   LOCALE void                           StringToFieldFunction(void *,EXEC_STATUS,DATA_OBJECT *);
   LOCALE void                           StringToField(void *,EXEC_STATUS,char *,DATA_OBJECT *);

#endif







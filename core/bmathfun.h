   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*             BASIC MATH FUNCTIONS MODULE             */
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
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#ifndef _H_bmathfun

#define _H_bmathfun

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _BMATHFUN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define GetAutoFloatDividend() EnvGetAutoFloatDividend(GetCurrentEnvironment())
#define SetAutoFloatDividend(a) EnvSetAutoFloatDividend(GetCurrentEnvironment(),a)

   LOCALE void                    BasicMathFunctionDefinitions(void *,EXEC_STATUS);
   LOCALE void                    AdditionFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                    MultiplicationFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                    SubtractionFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                    DivisionFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE long long               DivFunction(void *,EXEC_STATUS);
   LOCALE intBool                 SetAutoFloatDividendCommand(void *,EXEC_STATUS);
   LOCALE intBool                 GetAutoFloatDividendCommand(void *,EXEC_STATUS);
   LOCALE intBool                 EnvGetAutoFloatDividend(void *,EXEC_STATUS);
   LOCALE intBool                 EnvSetAutoFloatDividend(void *,EXEC_STATUS,int);
   LOCALE long long               IntegerFunction(void *,EXEC_STATUS);
   LOCALE double                  FloatFunction(void *,EXEC_STATUS);
   LOCALE void                    AbsFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                    MinFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                    MaxFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);

#endif





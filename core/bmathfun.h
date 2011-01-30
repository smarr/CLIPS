   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/02/96            */
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

   LOCALE void                    BasicMathFunctionDefinitions(void);
   LOCALE void                    AdditionFunction(DATA_OBJECT_PTR);
   LOCALE void                    MultiplicationFunction(DATA_OBJECT_PTR);
   LOCALE void                    SubtractionFunction(DATA_OBJECT_PTR);
   LOCALE void                    DivisionFunction(DATA_OBJECT_PTR);
   LOCALE long                    DivFunction(void);
   LOCALE BOOLEAN                 SetAutoFloatDividendCommand(void);
   LOCALE BOOLEAN                 GetAutoFloatDividendCommand(void);
   LOCALE BOOLEAN                 GetAutoFloatDividend(void);
   LOCALE BOOLEAN                 SetAutoFloatDividend(int);
   LOCALE long int                IntegerFunction(void);
   LOCALE double                  FloatFunction(void);
   LOCALE void                    AbsFunction(DATA_OBJECT_PTR);
   LOCALE void                    MinFunction(DATA_OBJECT_PTR);
   LOCALE void                    MaxFunction(DATA_OBJECT_PTR);

#endif










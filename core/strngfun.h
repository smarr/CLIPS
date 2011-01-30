   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
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

   LOCALE void                           StringFunctionDefinitions(void);
   LOCALE void                           StrCatFunction(DATA_OBJECT_PTR);
   LOCALE void                           SymCatFunction(DATA_OBJECT_PTR);
   LOCALE long int                       StrLengthFunction(void);
   LOCALE void                           UpcaseFunction(DATA_OBJECT_PTR);
   LOCALE void                           LowcaseFunction(DATA_OBJECT_PTR);
   LOCALE long int                       StrCompareFunction(void);
   LOCALE void                          *SubStringFunction(void);
   LOCALE void                           StrIndexFunction(DATA_OBJECT_PTR);
   LOCALE void                           EvalFunction(DATA_OBJECT_PTR);
   LOCALE int                            Eval(char *,DATA_OBJECT_PTR);
   LOCALE int                            BuildFunction(void);
   LOCALE DllExport int                  Build(char *);

#endif







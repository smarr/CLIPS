   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.20  01/31/02            */
   /*                                                     */
   /*          EXTENDED MATH FUNCTIONS HEADER FILE        */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for numerous extended math     */
/*   functions including cos, sin, tan, sec, csc, cot, acos, */
/*   asin, atan, asec, acsc, acot, cosh, sinh, tanh, sech,   */
/*   csch, coth, acosh, asinh, atanh, asech, acsch, acoth,   */
/*   mod, exp, log, log10, sqrt, pi, deg-rad, rad-deg,       */
/*   deg-grad, grad-deg, **, and round.                      */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_emathfun

#define _H_emathfun

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _EMATHFUN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           ExtendedMathFunctionDefinitions(void *theEnv,EXEC_STATUS);
#if EXTENDED_MATH_FUNCTIONS
   LOCALE double                         CosFunction(void *,EXEC_STATUS);
   LOCALE double                         SinFunction(void *,EXEC_STATUS);
   LOCALE double                         TanFunction(void *,EXEC_STATUS);
   LOCALE double                         SecFunction(void *,EXEC_STATUS);
   LOCALE double                         CscFunction(void *,EXEC_STATUS);
   LOCALE double                         CotFunction(void *,EXEC_STATUS);
   LOCALE double                         AcosFunction(void *,EXEC_STATUS);
   LOCALE double                         AsinFunction(void *,EXEC_STATUS);
   LOCALE double                         AtanFunction(void *,EXEC_STATUS);
   LOCALE double                         AsecFunction(void *,EXEC_STATUS);
   LOCALE double                         AcscFunction(void *,EXEC_STATUS);
   LOCALE double                         AcotFunction(void *,EXEC_STATUS);
   LOCALE double                         CoshFunction(void *,EXEC_STATUS);
   LOCALE double                         SinhFunction(void *,EXEC_STATUS);
   LOCALE double                         TanhFunction(void *,EXEC_STATUS);
   LOCALE double                         SechFunction(void *,EXEC_STATUS);
   LOCALE double                         CschFunction(void *,EXEC_STATUS);
   LOCALE double                         CothFunction(void *,EXEC_STATUS);
   LOCALE double                         AcoshFunction(void *,EXEC_STATUS);
   LOCALE double                         AsinhFunction(void *,EXEC_STATUS);
   LOCALE double                         AtanhFunction(void *,EXEC_STATUS);
   LOCALE double                         AsechFunction(void *,EXEC_STATUS);
   LOCALE double                         AcschFunction(void *,EXEC_STATUS);
   LOCALE double                         AcothFunction(void *,EXEC_STATUS);
   LOCALE long long                      RoundFunction(void *,EXEC_STATUS);
   LOCALE void                           ModFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE double                         ExpFunction(void *,EXEC_STATUS);
   LOCALE double                         LogFunction(void *,EXEC_STATUS);
   LOCALE double                         Log10Function(void *,EXEC_STATUS);
   LOCALE double                         SqrtFunction(void *,EXEC_STATUS);
   LOCALE double                         PiFunction(void *,EXEC_STATUS);
   LOCALE double                         DegRadFunction(void *,EXEC_STATUS);
   LOCALE double                         RadDegFunction(void *,EXEC_STATUS);
   LOCALE double                         DegGradFunction(void *,EXEC_STATUS);
   LOCALE double                         GradDegFunction(void *,EXEC_STATUS);
   LOCALE double                         PowFunction(void *,EXEC_STATUS);
#endif

#endif




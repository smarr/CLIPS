   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*            PREDICATE FUNCTIONS HEADER FILE          */
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

#ifndef _H_prdctfun

#define _H_prdctfun

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _PRDCTFUN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           PredicateFunctionDefinitions(void *,EXEC_STATUS);
   LOCALE intBool                        EqFunction(void *,EXEC_STATUS);
   LOCALE intBool                        NeqFunction(void *,EXEC_STATUS);
   LOCALE intBool                        StringpFunction(void *,EXEC_STATUS);
   LOCALE intBool                        SymbolpFunction(void *,EXEC_STATUS);
   LOCALE intBool                        LexemepFunction(void *,EXEC_STATUS);
   LOCALE intBool                        NumberpFunction(void *,EXEC_STATUS);
   LOCALE intBool                        FloatpFunction(void *,EXEC_STATUS);
   LOCALE intBool                        IntegerpFunction(void *,EXEC_STATUS);
   LOCALE intBool                        MultifieldpFunction(void *,EXEC_STATUS);
   LOCALE intBool                        PointerpFunction(void *,EXEC_STATUS);
   LOCALE intBool                        NotFunction(void *,EXEC_STATUS);
   LOCALE intBool                        AndFunction(void *,EXEC_STATUS);
   LOCALE intBool                        OrFunction(void *,EXEC_STATUS);
   LOCALE intBool                        LessThanOrEqualFunction(void *,EXEC_STATUS);
   LOCALE intBool                        GreaterThanOrEqualFunction(void *,EXEC_STATUS);
   LOCALE intBool                        LessThanFunction(void *,EXEC_STATUS);
   LOCALE intBool                        GreaterThanFunction(void *,EXEC_STATUS);
   LOCALE intBool                        NumericEqualFunction(void *,EXEC_STATUS);
   LOCALE intBool                        NumericNotEqualFunction(void *,EXEC_STATUS);
   LOCALE intBool                        OddpFunction(void *,EXEC_STATUS);
   LOCALE intBool                        EvenpFunction(void *,EXEC_STATUS);

#endif




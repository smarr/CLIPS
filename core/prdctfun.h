   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
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

   LOCALE void                           PredicateFunctionDefinitions(void);
   LOCALE BOOLEAN                        EqFunction(void);
   LOCALE BOOLEAN                        NeqFunction(void);
   LOCALE BOOLEAN                        StringpFunction(void);
   LOCALE BOOLEAN                        SymbolpFunction(void);
   LOCALE BOOLEAN                        LexemepFunction(void);
   LOCALE BOOLEAN                        NumberpFunction(void);
   LOCALE BOOLEAN                        FloatpFunction(void);
   LOCALE BOOLEAN                        IntegerpFunction(void);
   LOCALE BOOLEAN                        MultifieldpFunction(void);
   LOCALE BOOLEAN                        PointerpFunction(void);
   LOCALE BOOLEAN                        NotFunction(void);
   LOCALE BOOLEAN                        AndFunction(void);
   LOCALE BOOLEAN                        OrFunction(void);
   LOCALE BOOLEAN                        LessThanOrEqualFunction(void);
   LOCALE BOOLEAN                        GreaterThanOrEqualFunction(void);
   LOCALE BOOLEAN                        LessThanFunction(void);
   LOCALE BOOLEAN                        GreaterThanFunction(void);
   LOCALE BOOLEAN                        NumericEqualFunction(void);
   LOCALE BOOLEAN                        NumericNotEqualFunction(void);
   LOCALE BOOLEAN                        OddpFunction(void);
   LOCALE BOOLEAN                        EvenpFunction(void);

#endif




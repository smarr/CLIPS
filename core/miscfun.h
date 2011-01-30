   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*          MISCELLANEOUS FUNCTIONS HEADER FILE        */
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

#ifndef _H_miscfun

#define _H_miscfun

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _MISCFUN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           MiscFunctionDefinitions(void);
   LOCALE void                           CreateFunction(DATA_OBJECT_PTR);
   LOCALE long int                       SetgenFunction(void);
   LOCALE void                          *GensymFunction(void);
   LOCALE void                          *GensymStarFunction(void);
   LOCALE long                           RandomFunction(void);
   LOCALE void                           SeedFunction(void);
   LOCALE long int                       LengthFunction(void);
   LOCALE void                           ConserveMemCommand(void);
   LOCALE long int                       ReleaseMemCommand(void);
   LOCALE DllExport long int             MemUsedCommand(void);
   LOCALE long int                       MemRequestsCommand(void);
   LOCALE void                           OptionsCommand(void);
   LOCALE void                           ExpandFuncCall(DATA_OBJECT *);
   LOCALE void                           DummyExpandFuncMultifield(DATA_OBJECT *);
   LOCALE SYMBOL_HN                     *CauseEvaluationError(void);
   LOCALE BOOLEAN                        SetSORCommand(void);
   LOCALE SYMBOL_HN                     *GetFunctionRestrictions(void);
   LOCALE void                           AproposCommand(void);
   LOCALE void                          *GensymStar(void);
   LOCALE void                           GetFunctionListFunction(DATA_OBJECT *);

#endif







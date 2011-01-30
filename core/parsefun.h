   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  03/24/96            */
   /*                                                     */
   /*            PARSING FUNCTIONS HEADER FILE            */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for several parsing related    */
/*   functions including...                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_parsefun

#define _H_parsefun

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _PARSEFUN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           ParseFunctionDefinitions(void);
   LOCALE void                           CheckSyntaxFunction(DATA_OBJECT *);
   LOCALE int                            CheckSyntax(char *,DATA_OBJECT_PTR);

#endif







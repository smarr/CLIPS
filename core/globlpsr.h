   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*             DEFGLOBAL PARSER HEADER FILE            */
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

#ifndef _H_globlpsr

#define _H_globlpsr

#ifdef _DEFGLOBL_SOURCE_
struct defglobal;
#endif

#ifndef _H_expressn
#include "expressn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _GLOBLPSR_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE DllExport BOOLEAN       ParseDefglobal(char *);
   LOCALE BOOLEAN                 ReplaceGlobalVariable(struct expr *);
   LOCALE void                    GlobalReferenceErrorMessage(char *);

#endif








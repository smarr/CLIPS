   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/09/97          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_insqypsr
#define _H_insqypsr

#if INSTANCE_SET_QUERIES && (! RUN_TIME)

#ifndef _H_expressn
#include "expressn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _INSQYPSR_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

LOCALE EXPRESSION *ParseQueryNoAction(EXPRESSION *,char *);
LOCALE EXPRESSION *ParseQueryAction(EXPRESSION *,char *);

#ifndef _INSQYPSR_SOURCE_
#endif

#endif

#endif






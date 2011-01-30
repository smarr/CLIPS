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

#ifndef _H_inspsr
#define _H_inspsr

#ifndef _H_expressn
#include "expressn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _INSPSR_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ! RUN_TIME
LOCALE EXPRESSION *ParseInitializeInstance(EXPRESSION *,char *);
LOCALE EXPRESSION *ParseSlotOverrides(char *,int *);
#endif

LOCALE EXPRESSION *ParseSimpleInstance(EXPRESSION *,char *);

#ifndef _INSCOM_SOURCE_
#endif

#endif






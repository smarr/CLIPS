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

#ifndef _H_dffnxpsr
#define _H_dffnxpsr

#if DEFFUNCTION_CONSTRUCT && (! BLOAD_ONLY) && (! RUN_TIME)

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _DFFNXPSR_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

LOCALE BOOLEAN ParseDeffunction(char *);

#ifndef _DFFNXPSR_SOURCE_
#endif

#endif

#endif





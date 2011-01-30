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

#ifndef _H_objrtbld
#define _H_objrtbld

#if INSTANCE_PATTERN_MATCHING

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _OBJRTBLD_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

LOCALE void SetupObjectPatternStuff(void);

#endif

#endif







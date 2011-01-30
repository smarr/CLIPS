   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*      DEFTEMPLATE CONSTRUCT COMPILER HEADER FILE     */
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

#ifndef _H_tmpltcmp

#define _H_tmpltcmp

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _TMPLTCMP_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           DeftemplateCompilerSetup(void);
   LOCALE void                           DeftemplateCModuleReference(FILE *,int,int,int);
   LOCALE void                           DeftemplateCConstructReference(FILE *,void *,int,int);

#endif

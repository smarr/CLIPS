   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*            STRING I/O ROUTER HEADER FILE            */
   /*******************************************************/

/*************************************************************/
/* Purpose: I/O Router routines which allow strings to be    */
/*   used as input and output sources.                       */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_strngrtr
#define _H_strngrtr

#ifndef _STDIO_INCLUDED_
#define _STDIO_INCLUDED_
#include <stdio.h>
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _STRNGRTR_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

/**************************/
/* I/O ROUTER DEFINITIONS */
/**************************/

   LOCALE void                           InitializeStringRouter(void);
   LOCALE DllExport int                  OpenStringSource(char *,char *,int);
   LOCALE int                            OpenTextSource(char *,char *,int,int);
   LOCALE DllExport int                  CloseStringSource(char *);
   LOCALE int                            OpenStringDestination(char *,char *,int);
   LOCALE int                            CloseStringDestination(char *);

#endif


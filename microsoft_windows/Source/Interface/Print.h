   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.20  10/19/01            */
   /*                                                     */
   /*                PRINT HEADER FILE                    */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides basic routines for printing.            */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_print

#define _H_print

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _PRINT_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           InitializePrintDialog(HWND);
   LOCALE BOOL                           PrintWindowDriver(HINSTANCE,HWND,LPSTR);

#ifndef _PRINT_SOURCE_
   extern PRINTDLG                PrintDialog;
#endif

#endif


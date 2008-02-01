   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.20  10/02/01            */
   /*                                                     */
   /*                STATUS HEADER FILE                   */
   /*******************************************************/

/*************************************************************/
/* Purpose: Initialization routines for Windows interface.   */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_initialization

#define _H_initialization

#define ARROW_CURSOR              1
#define QUESTION_CURSOR           2
#define WAIT_CURSOR               3

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _INITIALIZATION_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE BOOL                           initInstance(HINSTANCE,UINT,int,LPSTR);
   LOCALE WPARAM                         exitInstance(MSG *);
   LOCALE ATOM                           registerMDIChild(HINSTANCE,DWORD,UINT,WNDPROC,int);

#ifndef _INITIALIZATION_SOURCE_
   extern HCURSOR                 ARROW;
   extern HCURSOR                 QUERY;
   extern HCURSOR                 WAIT[];
   extern OPENFILENAME            ofn;
   extern char                    szFileName[];
   extern char                    szFileTitle[];
   extern HICON                   hHourGlass;
#endif

#endif

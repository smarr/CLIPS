   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.20  10/02/01            */
   /*                                                     */
   /*                  TEXT HEADER FILE                   */
   /*******************************************************/

/*************************************************************/
/* Purpose: Code for text editing window.                    */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_text

#define _H_text

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _TEXT_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE HWND                           text_New(HWND hwnd,char *);
   LOCALE BOOL                           text_Open(HWND hwnd);
   LOCALE BOOL                           text_Revert(HWND hwnd,char *,HWND ewnd);
   LOCALE BOOL                           text_Save(HWND hwnd);
   LOCALE BOOL                           text_SaveAs(HWND hwnd);
   LOCALE BOOL                           text_InitInstance(HINSTANCE hinst);
   LOCALE void                           text_ExitInstance(void);
   LOCALE int                            text_QueryClose(HWND);
 
#ifndef _TEXT_SOURCE_
   extern ATOM                    EditAtomClass;
#endif

#endif





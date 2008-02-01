   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.20  10/02/01            */
   /*                                                     */
   /*              EDIT CONTROL HEADER FILE               */
   /*******************************************************/

/*************************************************************/
/* Purpose: Routines for the edit control in the editing     */
/*   windows.                                                */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_edit

#define _H_edit

#define ID_EDIT_CONTROL 1

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _EDIT_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE HWND                           edit_New(HWND,DWORD,BOOL);
   LOCALE void                           edit_ChooseFont(HWND);
   LOCALE void                           edit_UpdateMenu(HWND,HMENU);

#ifndef _EDIT_SOURCE_
#endif

#endif








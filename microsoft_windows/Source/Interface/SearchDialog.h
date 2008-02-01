	/******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  11/29/07            */
   /*                                                     */
   /*              SEARCH DIALOG HEADER FILE              */
   /*******************************************************/

/**************************************************************/
/* Purpose:                                                   */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Gary D. Riley                                         */
/*                                                            */
/* Contributing Programmer(s):                                */
/*                                                            */
/* Revision History:                                          */
/*      6.30: Renamed Search.c and Search.h to SearchDialog.c */
/*            and SearchDialog.h because of conflicts with    */
/*            the Borland search.h header file.               */
/*                                                            */
/**************************************************************/

#ifndef _H_SearchDialog

#define _H_SearchDialog

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _SEARCHDIALOG_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           SetUpSearch(HWND,int);
   LOCALE int                            StartSearch(HWND,WPARAM,LONG);
   LOCALE void                           InitFindReplace(HWND);

#ifndef _SEARCHDIALOG_SOURCE_
   extern UINT                    uFindReplaceMsg;
   extern BOOL                    SearchActive;
   extern HWND                    SearchDlg;
#endif

#endif



   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.20  10/02/01            */
   /*                                                     */
   /*                DIALOG1 HEADER FILE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides dialogs for Windows interface.          */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Christopher J. Ortiz                                 */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Gary D. Riley                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_dialog1

#define _H_dialog1

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _DIALOG1_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE BOOL FAR PASCAL                WatchDlgProc(HWND,UINT,WPARAM,LPARAM);
   LOCALE int                            SetCheckBox(HWND,WORD,int);
   LOCALE BOOL FAR PASCAL                ExecDlg(HWND,UINT,WPARAM,LPARAM);
   LOCALE BOOL FAR PASCAL                OptionDlgProc(HWND,UINT,WPARAM,LPARAM);


#ifndef _DIALOG1_SOURCE_
   extern int                     RuleStep;
   extern int                     Warnings;
   extern int                     Complete;
#endif


#endif


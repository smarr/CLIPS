   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.20  10/02/01            */
   /*                                                     */
   /*                DIALOG2 HEADER FILE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides dialogs for Windows interface.          */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Christopher J. Ortiz                                  */
/*                                                            */
/* Contributing Programmer(s):                                */
/*      Gary D. Riley                                         */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_dialog2

#define _H_dialog2

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _DIALOG2_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE BOOL FAR PASCAL                DeffactsManager(HWND,UINT,WPARAM,LPARAM);
   LOCALE BOOL FAR PASCAL                DeftemplateManager(HWND,UINT,WPARAM,LPARAM);
   LOCALE BOOL FAR PASCAL                DeffunctionManager(HWND,UINT,WPARAM,LPARAM);
   LOCALE BOOL FAR PASCAL                DefglobalManager(HWND,UINT,WPARAM,LPARAM);
   LOCALE BOOL FAR PASCAL                DefglobalManager(HWND,UINT,WPARAM,LPARAM);
   LOCALE BOOL FAR PASCAL                DefruleManager(HWND,UINT,WPARAM,LPARAM);
   LOCALE BOOL FAR PASCAL                AgendaManager(HWND,UINT,WPARAM,LPARAM);
   LOCALE BOOL FAR PASCAL                DefinstancesManager(HWND,UINT,WPARAM,LPARAM);
   LOCALE BOOL FAR PASCAL                DefgenericManager(HWND,UINT,WPARAM,LPARAM); 
   LOCALE BOOL FAR PASCAL                DefmethodsManager(HWND,UINT,WPARAM,LPARAM);
   LOCALE BOOL FAR PASCAL                DefclassManager(HWND,UINT,WPARAM,LPARAM);
   LOCALE BOOL FAR PASCAL                DefmessageHandlerManager(HWND,UINT,WPARAM,LPARAM);
   LOCALE BOOL FAR PASCAL                CommandComplete(HWND,UINT,WPARAM,LPARAM);

#ifndef _DIALOG2_SOURCE_
   extern char                    CompleteString[];
   extern struct symbolMatch     *GlobalMatches;
#endif

#endif


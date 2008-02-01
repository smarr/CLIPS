   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.20  10/02/01            */
   /*                                                     */
   /*                 MENU HEADER FILE                    */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides menus for Windows interface.          */
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

#ifndef _H_menu

#define _H_menu

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _MENU_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           DoBrowseChoice(HWND,WORD);
   LOCALE void                           DoExecutionChoice(HWND,WORD);
   LOCALE void                           UpdateMenu(HWND);
   LOCALE void                           UpdateModuleMenu(HMENU);
   LOCALE void                           DoModuleChoice(HMENU,WORD);
   LOCALE void                           DoHelpChoice(HWND,WORD);
   LOCALE void                           DoFileChoice(HWND,int);
   LOCALE void                           DoWindowChoice(HWND,WORD);

#endif


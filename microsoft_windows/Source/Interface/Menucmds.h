   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.20  10/02/01            */
   /*                                                     */
   /*             MENU COMMANDS HEADER FILE               */
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

#ifndef _H_menucmds

#define _H_menucmds

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _MENUCMDS_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           DoLoad(HWND,WORD);
   LOCALE void                           SaveBinaryFile(HWND);
   LOCALE void                           OpenDribbleFile(HWND,WORD);
   LOCALE BOOL                           DoCommandCompletion(HWND,char *,int);

#endif



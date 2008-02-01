   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.20  10/02/01            */
   /*                                                     */
   /*                  FRAME HEADER FILE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_frame

#define _H_frame

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _FRAME_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE int                            AvailableFrameHeight(void);
   LOCALE int                            AvailableFrameWidth(void);
   LOCALE LRESULT CALLBACK               mainFrameWndProc(HWND,UINT,WPARAM,LPARAM);

#ifndef _FRAME_SOURCE_
   extern HWND                    hMainFrame;
   extern HMENU                   hMainMenu;
   extern HACCEL                  hMainAccel;
   extern HACCEL                  haccel;
   extern HWND                    MDIClientWnd;
   //extern int                     ShiftKeyDown;
#endif

#endif

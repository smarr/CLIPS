   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.20  10/02/01            */
   /*                                                     */
   /*                STATUS HEADER FILE                   */
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

#ifndef _H_status

#define _H_status

struct statusWindowData
  {
   char *baseName;
   int noLines;
   int lastLine;
   int lineSize;
   void (*getPPForm)(void *,char *,unsigned,void *);
   void *(*getNextValue)(void *,void *);
   int (*getChanged)(void *);
   void (*setChanged)(void *,int);
   int (*getCount)(void *);
  };

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _STATUS_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE  LRESULT CALLBACK              StatusWndProc(HWND,UINT,WPARAM,LPARAM);
   LOCALE int                            UpdateFactWnd(void);
   LOCALE int                            UpdateAgendaWnd(void);
   LOCALE int                            UpdateInstWnd(void);
   LOCALE int                            UpdateGlobalWnd(void);
   LOCALE void                           UpdateStatus(void);
   LOCALE BOOL                           status_InitInstance(HINSTANCE);
   LOCALE BOOL                           factsWindow_New(HWND);
   LOCALE BOOL                           agendaWindow_New(HWND);
   LOCALE BOOL                           instancesWindow_New(HWND);
   LOCALE BOOL                           globalsWindow_New(HWND);
   LOCALE BOOL                           focusWindow_New(HWND);
   LOCALE void                           TileStatusWindows(void);

#ifndef _STATUS_SOURCE_
   extern HWND                    FactsWindow;
   extern HWND                    AgendaWindow;
   extern HWND                    InstancesWindow;
   extern HWND                    GlobalsWindow;
   extern HWND                    FocusWindow;
   extern ATOM                    StatusAtomClass;
#endif

#endif


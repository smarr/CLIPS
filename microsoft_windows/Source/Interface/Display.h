   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.20  10/02/01            */
   /*                                                     */
   /*                DISPLAY HEADER FILE                  */
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

#ifndef _H_display

#define _H_display

#define ID_DISPLAY_CONTROL 2

#define DIALOG_SIZE 800

struct priorCommand
  {
   struct priorCommand *next;
   struct priorCommand *prev;
   char *command;
  };
  
struct displayWindowData
  {
   int linesInDisplayWindow;
   int lastLine;
   int lineSize;
   int maxCharWidth;
   int oldLine;
   int horizScroll;
   int restoreCaretX;
   int restoreCaretY;
   int caretOffset;
   int caretLinesBack;
   int caretCharsBack;
   int selectionLineStart;
   int selectionLineEnd;
   int selectionCharStart;
   int selectionCharEnd;
   struct priorCommand *topCommand;
   struct priorCommand *bottomCommand;
   struct priorCommand *currentCommand;
   int maxCommandCount;
   int currentCommandCount;
   char **terminal;
  };

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _DISPLAY_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE BOOL                           displayWindow_New(HWND);
   LOCALE BOOL                           display_InitInstance(HINSTANCE);
   LOCALE void                           display_OnSetFocus(HWND,HWND);
   LOCALE void                           display_OnPaint(HWND);
   LOCALE int                            DisplayPrint(HWND,char *);
   LOCALE LRESULT CALLBACK               displayWndProc(HWND,UINT,WPARAM,LPARAM);
   LOCALE void                           ClearDialogWnd(void);
   LOCALE void                           ExitToShell(void);
   LOCALE void                           GetUserCmd(HWND,WORD,BOOL,unsigned);
   LOCALE void                           TileDisplayWindow(void);
   LOCALE void                           DisplayLineCountAndStart(HWND,int *,int *);
   LOCALE void                           UpdateCursor(int); 
   LOCALE void                           StartWaitCursor(void);
   LOCALE void                           StopWaitCursor(void);
   LOCALE int                            DisplayBeforeCommandExecution(void *);
   LOCALE void                           ClearWindowCommand(void *);
   LOCALE void                           ClearCommandFromDisplay(HWND,void *);
   
#ifndef _DISPLAY_SOURCE_
   extern HWND                    DialogWindow;
   extern ATOM                    DisplayAtomClass;
#endif

#endif




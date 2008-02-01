   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*       Microsoft Windows Version 3.0  01/31/02       */
   /*                                                     */
   /*                   DISPLAY MODULE                    */
   /*******************************************************/

/**************************************************************/
/* Purpose:                                                   */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Christopher J. Ortiz                                  */
/*                                                            */
/* Contributing Programmer(s):                                */
/*      Gary D. Riley                                         */
/*      Ernst Bokkelkamp                                      */
/*                                                            */
/* Revision History:                                          */
/*      6.24: Ernst's fix for the caret artifact from text    */
/*            scrolling.                                      */
/*                                                            */
/**************************************************************/

#define _DISPLAY_SOURCE_

#include "StdSDK.h"     // Standard application includes
#include <tchar.h>
#include <windowsx.h>

#include "mdi.h"
#include "display.h"
#include "Initialization.h"

#include "setup.h"

#include "argacces.h"
#include "commline.h" 
#include "filecom.h"

#include "router.h"
#include "resource.h"
#include "menucmds.h"
#include "dialog2.h"
#include "print.h"
#include "frame.h"
#include "findwnd.h"

#define setProc(hwnd, proc) SetWindowLong(hwnd, GWL_USERDATA, (LPARAM)proc)
#define getProc(hwnd)       (WNDPROC)GetWindowLong(hwnd, GWL_USERDATA)

#define DEFAULT_COMMAND_MAX 20
#define DISPLAY_FONT_NAME "Courier"
#define DISPLAY_FONT_HEIGHT -13
#define LEFT_MARGIN_SPACE 2
#define SCROLL_TIMER 1

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    display_OnClose(HWND);
   static void                    FreeTerminalText(HWND);
   static void                    DeleteTerminal(HWND);
   static void                    RedrawTerminal(HWND,HDC); 
   static void                    SendToScreen(HWND);
   static void                    display_OnSize(HWND,UINT,int,int);
   static void                    display_OnVScroll(HWND,HWND,UINT,int);
   static void                    display_OnHScroll(HWND,HWND,UINT,int);
   static void                    display_OnKillFocus(HWND,HWND);
   static void                    display_OnChar(HWND,TCHAR,int);
   static void                    display_OnMDIActivate(HWND,BOOL,HWND,HWND);
   static void                    display_OnCommand(HWND,int,HWND,UINT);
   static BOOL                    display_OnContextMenu(HWND,HWND,int,int);
   static void                    display_OnTimer(HWND,UINT);
   static void                    display_OnEnterSizeMove(HWND);
   static void                    display_OnKey(HWND,UINT,BOOL,int,UINT);
   static void                    display_OnLButtonDown(HWND,BOOL,int,int,UINT);
   static void                    display_OnLButtonUp(HWND,int,int,UINT);
   static void                    display_OnMouseMove(HWND,int,int,UINT);
   static void                    display_OnUpdateMenu(HWND,HMENU);
   static void                    CreateTerminal(struct displayWindowData *);
   static void                    SaveDisplayWindow(HWND);
   static int                     ComputeCaretX(HWND,struct displayWindowData *);
   static void                    HandleBackSpace(HWND,struct displayWindowData *,BOOL,unsigned);
   static void                    HandleStandardCharacter(HWND,struct displayWindowData *,BOOL,char);
   static int                     QuickBackSpace(HWND,struct displayWindowData *);
   static void                    DisplayPaste(HWND,struct displayWindowData *,char *);
   static void                    SwitchCommand(HWND,void *,struct priorCommand *,struct priorCommand *);
   static void                    FlashBalancedParenthesis(void *,struct displayWindowData *,int);
   static void                    FindInsertionPoint(HWND,int,int,int *,int *);
   static HRGN                    DisplaySelectionRegion(HWND,struct displayWindowData *);
   static void                    InvertSelection(HWND,struct displayWindowData *);
   static void                    RemoveSelection(HWND,struct displayWindowData *);
   static char                   *DisplaySelectionText(HWND,struct displayWindowData *,size_t *);
   static VOID CALLBACK           ScrollTimerProc(HWND,UINT,UINT_PTR,DWORD);
   static void                    display_OnScrollTimer(HWND,int,int);
   
/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   globle HWND                 DialogWindow = NULL;
   globle ATOM                 DisplayAtomClass;

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static HMENU                DisplayMenu = NULL;
   static HMENU                DisplayWindowMenu = NULL;
   static HMENU                DisplayContextMenu = NULL;
   static int                  OriginalPosition = 0;
   static int                  OriginalWidth = 0;
   static int                  OriginalHeight = 0;
   static int                  HoverX;
   static int                  HoverY;
   static UINT_PTR             ScrollTimer;
  
/*********************/
/* displayWndProc:   */
/*********************/
LRESULT CALLBACK displayWndProc(
  HWND hwnd, 
  UINT message, 
  WPARAM wParam, 
  LPARAM lParam)
  {
   switch(message)
     { 
      HANDLE_MSG(hwnd,WM_COMMAND,display_OnCommand);
      HANDLE_MSG(hwnd,WM_CLOSE,display_OnClose);
      HANDLE_MSG(hwnd,WM_SIZE,display_OnSize);
      HANDLE_MSG(hwnd,WM_ENTERSIZEMOVE,display_OnEnterSizeMove);
      HANDLE_MSG(hwnd,WM_PAINT,display_OnPaint);
      HANDLE_MSG(hwnd,WM_VSCROLL,display_OnVScroll);
      HANDLE_MSG(hwnd,WM_HSCROLL,display_OnHScroll);
      HANDLE_MSG(hwnd,WM_SETFOCUS,display_OnSetFocus);
      HANDLE_MSG(hwnd,WM_KILLFOCUS,display_OnKillFocus);
      HANDLE_MSG(hwnd,WM_CHAR,display_OnChar);
      HANDLE_MSG(hwnd,WM_MDIACTIVATE,display_OnMDIActivate);
	  HANDLE_MSG(hwnd,WM_CONTEXTMENU,display_OnContextMenu);
      HANDLE_MSG(hwnd,WM_DESTROY,DeleteTerminal);
      HANDLE_MSG(hwnd,WM_TIMER,display_OnTimer);
      HANDLE_MSG(hwnd,WM_KEYDOWN,display_OnKey);
      HANDLE_MSG(hwnd,WM_LBUTTONDOWN,display_OnLButtonDown);
      HANDLE_MSG(hwnd,WM_LBUTTONUP,display_OnLButtonUp);
      HANDLE_MSG(hwnd,WM_MOUSEMOVE,display_OnMouseMove);
         
      case UWM_SCROLL:
        display_OnScrollTimer(hwnd,HoverX,HoverY);
        return 0;

      case UWM_UPDATE_MENU:
        display_OnUpdateMenu(hwnd,GetMenu(hMainFrame));
        display_OnUpdateMenu(hwnd,DisplayContextMenu);
        return 0;
	 } 
 	  	  
   return DefMDIChildProc(hwnd,message,wParam,lParam);
  }

/************************************************/
/* display_OnChar:     */
/************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void display_OnChar(
  HWND hwnd, 
  TCHAR ch, 
  int cRepeat)
  {
#if WIN_MCW
#pragma unused(cRepeat)
#endif
   GetUserCmd(hwnd,ch,FALSE,0);
  }
  
/*************************************************************/
/* display_InitInstance: Registers the display window class, */
/*   and loads its menu to a local static variable. Returns  */
/*   TRUE if successful, otherwise FALSE.                    */
/*************************************************************/
BOOL display_InitInstance(
  HINSTANCE hinst)
  {
   if (! (DisplayAtomClass = registerMDIChild(hinst,CS_NOCLOSE,IDR_DISPLAY,displayWndProc,sizeof(WORD))))
     { return FALSE; }

   DisplayMenu = LoadMenu(hinst,MAKEINTRESOURCE(IDR_CLIPS));

   if (DisplayMenu == NULL)
     { return FALSE; }
   
   DisplayContextMenu = LoadMenu(hinst,MAKEINTRESOURCE(IDR_DISPLAY_CONTEXT));

   DisplayWindowMenu = findWindowMenu(DisplayMenu);

   return TRUE;
  }

/*********************************************************/
/* TileDisplayWindow:  */
/*********************************************************/
void TileDisplayWindow()
  {
   int width, height;
   int xpos, ypos, wwidth, wheight;
   
   width = AvailableFrameWidth();
   height = AvailableFrameHeight();
   
   xpos = 2;
   ypos = 2;
   wwidth = (int) (width * 0.66) - 5;
   wheight = (int)(height * 0.66) - 5;

   MoveWindow(DialogWindow,xpos,ypos,wwidth,wheight,TRUE);
  }

/*********************************************************/
/* displayWindow_New:  */
/*********************************************************/
BOOL displayWindow_New(
  HWND hwnd)
  {
   HDC hDC;
   RECT rect; /*, placement; */
   int width, height;
   struct displayWindowData *theData;
   TEXTMETRIC tm;
   LOGFONT lf;
   HFONT hfont;
   
   theData = (struct displayWindowData *) malloc(sizeof(struct displayWindowData));
   
   if (theData == NULL)
     { return(FALSE); }

   GetClientRect(hwnd,&rect);

   /* GetWindowPlacement(hwnd,&placement); */

   width = AvailableFrameWidth();
   height = AvailableFrameHeight();
   
   /*===========================*/
   /* Create the dialog window. */
   /*===========================*/
   
   DialogWindow = mdi_Create(MDIClientWnd,0,IDR_DISPLAY,"Dialog Window",
                             /* CW_USEDEFAULT,0,CW_USEDEFAULT,0 */
                             2,2,(int) (width * 0.66) - 5,(int) (height * 0.66) - 5);
   
   if (DialogWindow == NULL) 
     {       
      free(theData);
      return(FALSE); 
     }
     
   hDC = GetDC(DialogWindow);
   
   memset(&lf,0,sizeof(LOGFONT));
   lf.lfHeight = DISPLAY_FONT_HEIGHT;
   strcpy(lf.lfFaceName,DISPLAY_FONT_NAME);
   hfont = CreateFontIndirect(&lf);
   if (hfont != NULL)
     { SelectObject(hDC,hfont); }   

   GetTextMetrics(hDC,&tm);
   ReleaseDC(DialogWindow,hDC);
   if (hfont != NULL)
     { DeleteFont(hfont); }  
     
   GetClientRect(DialogWindow,(LPRECT) &rect);

   theData->lastLine = 0;
   theData->oldLine = 0;
   theData->horizScroll = 0;
      
   theData->selectionLineStart = 0;
   theData->selectionLineEnd = 0;
   theData->selectionCharStart = 0;
   theData->selectionCharEnd = 0;

   theData->restoreCaretX = 0;
   theData->restoreCaretY = 0;
   theData->caretOffset = 0;
   theData->caretLinesBack = 0;
   theData->caretCharsBack = 0;
   
   theData->lineSize = tm.tmHeight+ tm.tmExternalLeading;
   theData->maxCharWidth = tm.tmMaxCharWidth;
   theData->linesInDisplayWindow  = (rect.bottom / theData->lineSize) - 1;
   
   /*===================================*/
   /* Set up the prior command history. */
   /*===================================*/
   
   theData->maxCommandCount = DEFAULT_COMMAND_MAX;
   theData->currentCommandCount = 1;
   theData->topCommand = (struct priorCommand *) malloc(sizeof(struct priorCommand));
   theData->currentCommand = theData->topCommand;
   theData->bottomCommand = theData->topCommand;
   theData->topCommand->next = NULL;
   theData->topCommand->prev = NULL;
   theData->topCommand->command = (char *) malloc(1);
   theData->topCommand->command[0] = '\0';
   
   CreateTerminal(theData);

   SetWindowLong(DialogWindow,GWL_USERDATA,(long) theData);

   SetScrollRange(DialogWindow,SB_HORZ,0,255,TRUE);

   /* MoveWindow(DialogWindow,2,2,(int) (width * 0.66) - 5,(int) (height * 0.66) - 5,TRUE); */
   
   return(TRUE);
  }
    
/******************************************/
/* display_OnUpdateMenu: Updates the menu */
/*   based on the display window state.   */
/******************************************/
#if WIN_BTC
#pragma argsused
#endif
static void display_OnUpdateMenu(
  HWND hdisplay,
  HMENU hmenu)
  {   
#if WIN_MCW
#pragma unused(hdisplay)
#endif
   struct displayWindowData *theData;
  
   theData = (struct displayWindowData *) GetWindowLong(hdisplay,GWL_USERDATA);
   
   if (theData == NULL)
     { return; }

   EnableMenuItem(hmenu,ID_EDIT_PASTE, (unsigned)
                  MF_BYCOMMAND | 
                  (IsClipboardFormatAvailable(CF_TEXT) ? MF_ENABLED : MF_GRAYED));

   if ((theData->selectionLineStart != theData->selectionLineEnd) ||
       (theData->selectionCharStart != theData->selectionCharEnd))
     { EnableMenuItem(hmenu,ID_EDIT_COPY,(unsigned) MF_BYCOMMAND | MF_ENABLED); }
   else
     { EnableMenuItem(hmenu,ID_EDIT_COPY,(unsigned) MF_BYCOMMAND | MF_GRAYED); }
       

   EnableMenuItem(hmenu,ID_FILE_CLOSE,MF_GRAYED);
   EnableMenuItem(hmenu,ID_FILE_SAVE,MF_ENABLED);
   EnableMenuItem(hmenu,ID_FILE_SAVE_AS,MF_ENABLED);

   EnableMenuItem(hmenu,ID_EDIT_UNDO,MF_GRAYED);
   EnableMenuItem(hmenu,ID_EDIT_CUT,MF_GRAYED);
   /* EnableMenuItem(hmenu,ID_EDIT_COPY,MF_GRAYED); */
   EnableMenuItem(hmenu,ID_EDIT_CLEAR,MF_GRAYED);
   EnableMenuItem(hmenu,ID_EDIT_SELECT_ALL,MF_ENABLED);
   EnableMenuItem(hmenu,ID_EDIT_BALANCE,MF_GRAYED);
   EnableMenuItem(hmenu,ID_EDIT_COMMENT,MF_GRAYED);
   EnableMenuItem(hmenu,ID_EDIT_UNCOMMENT,MF_GRAYED);
   EnableMenuItem(hmenu,ID_EDIT_SET_FONT,MF_GRAYED);

   EnableMenuItem(hmenu,ID_BUFFER_FIND,MF_GRAYED);
   EnableMenuItem(hmenu,ID_BUFFER_REPLACE,MF_GRAYED);
   EnableMenuItem(hmenu,ID_BUFFER_LOAD,MF_GRAYED);
   EnableMenuItem(hmenu,ID_BUFFER_BATCH,MF_GRAYED);
   EnableMenuItem(hmenu,ID_BUFFER_LOAD_BUFFER,MF_GRAYED);
   
   PostMessage(hMainFrame,UWM_UPDATE_TOOLBAR,0,0);
  }

/************************************************/
/* display_OnClose: Overrides the close handler */
/*   so the display window can't be closed.     */
/************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void display_OnClose(
  HWND hwnd)
  {
#if WIN_MCW
#pragma unused(hwnd)
#endif
   return;
  }
 
/************************************************/
/* display_OnEnterSizeMove:    */
/************************************************/
static void display_OnEnterSizeMove(
  HWND hwnd)
  {
   RECT theRect;
   
   GetClientRect(hwnd,&theRect);

   OriginalPosition = GetScrollPos(hwnd,SB_VERT);
   
   OriginalWidth = theRect.right;
   OriginalHeight = theRect.bottom;
  }

/************************************/
/* display_OnSize: Handles resizing */
/*   of the dialog window.          */
/************************************/
#if WIN_BTC
#pragma argsused
#endif
static void display_OnSize(
  HWND hwnd, 
  UINT state, 
  int cx, 
  int cy)
  {
#if WIN_MCW
#pragma unused(cx)
#pragma unused(state)
#endif
   int min, max, old, pos;
   int startBegin, startEnd;
   struct displayWindowData *theData;
   RECT rect;

   theData = (struct displayWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) 
     { return; }

   GetScrollRange(hwnd,SB_VERT,&min,&max);
   pos = GetScrollPos(hwnd,SB_VERT);

   if (pos < theData->linesInDisplayWindow)
     { startBegin = 0; }
   else
     { startBegin = max + (pos - max) - theData->linesInDisplayWindow; }
    
   old = theData->linesInDisplayWindow;  
   theData->linesInDisplayWindow = (cy / theData->lineSize) - 1;
        
   if (max < DIALOG_SIZE) 
     { max = theData->lastLine; }
   else 
     { max = DIALOG_SIZE; }
   
   if (max > theData->linesInDisplayWindow)
     { 
      pos += (theData->linesInDisplayWindow - old);
      if (pos < theData->linesInDisplayWindow)
        { pos = theData->linesInDisplayWindow; }
      else if (pos > max)
        { pos = max; }
      
      SetScrollRange(hwnd,SB_VERT,theData->linesInDisplayWindow,max,FALSE); 
      SetScrollPos(hwnd,SB_VERT,pos,TRUE);
     }
   else
     { 
      SetScrollRange(hwnd,SB_VERT,0,0,FALSE); 
      SetScrollPos(hwnd,SB_VERT,0,TRUE);
     }

   if (pos < theData->linesInDisplayWindow)
     { startEnd = 0; }
   else
     { startEnd = max + (pos - max) - theData->linesInDisplayWindow; }


   /* InvalidateRect(hwnd,NULL,TRUE); */
   
   /*===========================================*/
   /* If the window resizes to the right, then  */
   /* invalidate the new visible portion on the */
   /* right side of the window.                 */
   /*===========================================*/

   if (cx > OriginalWidth)
     {
      rect.left = OriginalWidth;
      rect.right = cx;
      rect.top = 0;
      rect.bottom = (theData->linesInDisplayWindow + 1) * theData->lineSize;
      InvalidateRect(hwnd,&rect,TRUE);
     }

   if (startEnd == startBegin)
     {
      if (old < theData->linesInDisplayWindow)
        {
         rect.left = 0;
         rect.right = cx;
         rect.top = (old + 1) * theData->lineSize;
         rect.bottom = (theData->linesInDisplayWindow + 1) * theData->lineSize;
         InvalidateRect(hwnd,&rect,TRUE);     
        } 
      else
        {
         rect.left = 0;
         rect.right = cx;
         rect.top = (theData->linesInDisplayWindow + 1) * theData->lineSize;
         rect.bottom = (old + 1) * theData->lineSize;
         InvalidateRect(hwnd,&rect,TRUE);     
        }
     }  
   else if (startEnd < startBegin)
     {       
      rect.left = 0;
      rect.right = cx;
      rect.top = 0;
      rect.bottom = theData->lineSize * (theData->linesInDisplayWindow + 1);
      ScrollWindowEx(hwnd,0,(startBegin - startEnd) * theData->lineSize,&rect,&rect,NULL,NULL,SW_ERASE | SW_INVALIDATE);
     } 
   else
     { /* MessageBeep(0); */ }

   OriginalWidth = cx;
   OriginalHeight = cy;

   FORWARD_WM_SIZE(hwnd,state,cx,cy,DefMDIChildProc);
  }

/************************************************/
/* display_OnPaint:    */
/************************************************/
void display_OnPaint(
  HWND hwnd)
  {
   HDC hdc;
   PAINTSTRUCT ps;

   hdc = BeginPaint(hwnd,&ps);
   SetMapMode(hdc,MM_TEXT);;
   RedrawTerminal(hwnd,hdc);
   ValidateRect(hwnd,NULL);
   EndPaint(hwnd,&ps) ;
  }

/************************************************/
/* display_OnVScroll: */
/************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void display_OnVScroll(
  HWND hwnd, 
  HWND hwndCtl, 
  UINT code, 
  int pos)
  {
#if WIN_MCW
#pragma unused(hwndCtl)
#endif
   int min, max, cp, org;
   static int CaretON = TRUE;
   struct displayWindowData *theData;
   RECT rect;
   
   theData = (struct displayWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) 
     { return; }

   GetScrollRange(hwnd,SB_VERT,&min,&max);
   cp = GetScrollPos(hwnd,SB_VERT);
   org = cp;
   
   switch (code)
	 {   
      case SB_LINEDOWN:
        cp++;
        break;

      case SB_LINEUP:
        cp--;
        break;

      case SB_PAGEDOWN:
        cp += theData->linesInDisplayWindow;
        break;
        
      case SB_PAGEUP:
        cp -= theData->linesInDisplayWindow;
        break;
      
      case SB_THUMBTRACK:
        cp = pos;
        break;

      default:
        return;
	 }

   if (cp >= max)
     { cp = max; }

   if (cp >= max - (theData->caretLinesBack))
     {
      if (FORWARD_WM_MDIGETACTIVE(MDIClientWnd,SendMessage) == hwnd)
        {
         ShowCaret(hwnd);
         CaretON = TRUE;
        }
     }
   else if (CaretON)
     {
      if (FORWARD_WM_MDIGETACTIVE(MDIClientWnd,SendMessage) == hwnd)
        {
         HideCaret(hwnd); 
         CaretON = FALSE;
        }
	 }

   if (cp < theData->linesInDisplayWindow)
     { cp = theData->linesInDisplayWindow; } 
  
   SetScrollPos(hwnd,SB_VERT,cp,TRUE);
  
   GetClientRect(hwnd,&rect);
   rect.bottom = rect.top + (theData->lineSize * (theData->linesInDisplayWindow + 1));
   ScrollWindowEx(hwnd,0,(org - cp) * theData->lineSize,&rect,&rect,NULL,NULL,SW_ERASE | SW_INVALIDATE);
  }

/************************************************/
/* display_OnHScroll: */
/************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void display_OnHScroll(
  HWND hwnd, 
  HWND hwndCtl, 
  UINT code, 
  int pos)
  {
#if WIN_MCW
#pragma unused(hwndCtl)
#endif
   int min, max, cp, org;
   struct displayWindowData *theData;
   RECT rect;
   
   theData = (struct displayWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) 
     { return; }

   GetScrollRange(hwnd,SB_HORZ,&min,&max);
   cp = GetScrollPos(hwnd,SB_HORZ);
   org = cp;
   
   switch (code)
	 {   
      case SB_LINEDOWN:
        cp++;
        break;

      case SB_LINEUP:
        cp--;
        break;

      case SB_PAGEDOWN:
        cp += theData->linesInDisplayWindow; // Should be window width?
        break;
        
      case SB_PAGEUP:
        cp -= theData->linesInDisplayWindow; // Should be window width?
        break;
      
      case SB_THUMBTRACK:
        cp = pos;
        break;

      default:
        return;
	 }

   if (cp > max) cp = max;
   if (cp < min) cp = min;

   SetScrollPos(hwnd,SB_HORZ,cp,TRUE);

   GetClientRect(hwnd,&rect);
   rect.left = LEFT_MARGIN_SPACE;
   rect.bottom = rect.top + (theData->lineSize * (theData->linesInDisplayWindow + 1));
   ScrollWindowEx(hwnd,(org - cp) * theData->maxCharWidth,0,&rect,&rect,NULL,NULL,SW_ERASE | SW_INVALIDATE);
  }

/************************************************/
/* display_OnSetFocus: */
/************************************************/
#if WIN_BTC
#pragma argsused
#endif
void display_OnSetFocus(
  HWND hwnd, 
  HWND oldfocus)
  {
#if WIN_MCW
#pragma unused(oldfocus)
#endif
   struct displayWindowData *theData;
   
   theData = (struct displayWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) 
     { return; }
     
   SetFocus(hwnd);
   CreateCaret(hwnd,NULL,theData->maxCharWidth,theData->lineSize);
   
   SetCaretPos(theData->restoreCaretX,theData->restoreCaretY);
   ShowCaret(hwnd);
  }
   
/************************************************/
/* display_OnKillFocus: */
/************************************************/
#if WIN_BTC
#pragma argsused
#endif
void display_OnKillFocus(
  HWND hwnd,
  HWND newFocus)
  {
#if WIN_MCW
#pragma unused(newFocus)
#endif
   POINT thePoint;
   struct displayWindowData *theData;
   
   theData = (struct displayWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) 
     { return; }

   GetCaretPos(&thePoint);
   
   theData->restoreCaretX = thePoint.x;
   theData->restoreCaretY = thePoint.y;
   
   HideCaret(hwnd); 
   DestroyCaret();  
  }

/************************************************/
/* display_OnMDIActivate: */
/************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void display_OnMDIActivate(
  HWND hwnd, 
  BOOL active, 
  HWND hActivate, 
  HWND hDeactivate)
  {
#if WIN_MCW
#pragma unused(hDeactivate)
#pragma unused(hActivate)
#endif
   struct displayWindowData *theData;

   if (active)
     {       
      display_OnUpdateMenu(hwnd,DisplayMenu);

      if (FORWARD_WM_MDISETMENU(MDIClientWnd,TRUE,DisplayMenu,
                                DisplayWindowMenu,SendMessage) != 0)
        { DrawMenuBar(hMainFrame); }
     }
       
   theData = (struct displayWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   if (theData == NULL) return;        
       
   InvertSelection(hwnd,theData);
  }
  
/************************************************/
/* display_OnCommand: */
/************************************************/
static void display_OnCommand(
  HWND hwnd, 
  int id, 
  HWND hctl, 
  UINT codeNotify)
  {	 
   HANDLE hData;
   LPSTR  pData;
   char *buffer, *tempText;
   size_t length, x, p;
   struct displayWindowData *theData;
   HGLOBAL h;
   int lines, start;

   switch (id) 
	 {
      case ID_EDIT_PASTE:
        OpenClipboard(DialogWindow);
        hData = GetClipboardData(CF_TEXT);
        pData = (char *) GlobalLock(hData);
        
        length = strlen(pData);
        tempText = (char *) malloc(length+1);

        for (x = 0, p = 0; x < length; x++)
          {
           if (pData[x] != '\r')
             { tempText[p++] = pData[x]; }
          }

        tempText[p] = '\0';
          
        theData = (struct displayWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
        DisplayPaste(hwnd,theData,tempText); 

        free(tempText);
        GlobalUnlock(hData);
        CloseClipboard();     
        return;  

      case ID_EDIT_COPY:
        theData = (struct displayWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
        tempText = DisplaySelectionText(hwnd,theData,&length);
        if (tempText == NULL) return;
        
        if (! OpenClipboard(DialogWindow))
          { return; }
          
        h = GlobalAlloc(GMEM_MOVEABLE | GMEM_DDESHARE,(length + 1) * sizeof(char));
        
        if (h == NULL)
          {
           CloseClipboard();
           return;
          } 
          
        pData = (char *) GlobalLock(h);
        if (pData == NULL)
          {
           GlobalFree(h);
           CloseClipboard();
           return;
          }

        strcpy(pData,tempText);
        
        GlobalUnlock(h);
        
        EmptyClipboard();
        
        SetClipboardData(CF_TEXT,h);
        
        CloseClipboard();
        
        free(tempText);

        return;

      case ID_EDIT_SELECT_ALL:
        theData = (struct displayWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
        RemoveSelection(hwnd,theData);
        
        theData->selectionLineStart = 0;
        theData->selectionCharStart = 0;
        
        DisplayLineCountAndStart(hwnd,&lines,&start);
  
        theData->selectionLineEnd = lines - 1;
        theData->selectionCharEnd = strlen(theData->terminal[theData->lastLine]);
        
        InvalidateRect(hwnd,NULL,TRUE);
        
        PostMessage(hwnd,UWM_UPDATE_MENU,0,0);
        break;
               
      case ID_FILE_PRINT:
        PrintWindowDriver(GetWindowInstance(hMainFrame),hwnd,"Dialog Window");
	    return;
	    
      case ID_FILE_SAVE:
      case ID_FILE_SAVE_AS:
        SaveDisplayWindow(hwnd);
        return;
	    
      case ID_HELP_COMPLETE:
        buffer = GetCommandString(GetCurrentEnvironment());         

        if (buffer == NULL)
          {  
           MessageBeep(0);
           break;
          }

        length = strlen(buffer);
        buffer = GetCommandCompletionString(GetCurrentEnvironment(),buffer,length);

        if (buffer == NULL)
          {
           MessageBeep(0);
           break;
          }

        length = strlen(buffer);

        if (DoCommandCompletion(hwnd,buffer,1))
          {  
           AppendCommandString(GetCurrentEnvironment(),&(CompleteString[length]));
           EnvPrintRouter(GetCurrentEnvironment(),WPROMPT,(&(CompleteString[length])));
          }
       break;
	 } 
   
   FORWARD_WM_COMMAND(hwnd, id, hctl, codeNotify, DefMDIChildProc);
  }
  
/***********************************************************/
/* display_OnContextMenu: Pops up a context-specific menu. */
/***********************************************************/
static BOOL display_OnContextMenu(
  HWND hwnd,
  HWND hwndCtl,
  int xPos,
  int yPos)
  {
   return mdi_OnContextMenu(hwnd,hwndCtl,xPos,yPos,DisplayContextMenu);
  }

/**********************************************************/
/* display_OnTimer:  */
/**********************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void display_OnTimer(
  HWND hwnd,
  UINT id)
  {
#if WIN_MCW
#pragma unused(hwnd)
#pragma unused(id)
#endif
   static int value = 0;
   POINT thePoint;

   GetCursorPos(&thePoint);
   ScreenToClient(MDIClientWnd,&thePoint);

   value++;
   
   SetClassLong(DialogWindow,GCL_HCURSOR,(LONG) WAIT[value]);

   if (ChildWindowFromPointEx(MDIClientWnd,thePoint,CWP_SKIPINVISIBLE | CWP_SKIPDISABLED) == DialogWindow)
     { SetCursor(WAIT[value]); }
  
   if (value > 7)
     { value = 0; }
  }
  
/**********************************************************/
/* display_OnKey:  */
/**********************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void display_OnKey(
  HWND hwnd,
  UINT vk,
  BOOL down,
  int repeatCount,
  UINT flags)
  {
#if WIN_MCW
#pragma unused(hwnd)
#pragma unused(repeatCount)
#pragma unused(flags)
#endif
   struct displayWindowData *theData;
   POINT thePoint;
   char *theCommand, theChar;
   int commandSize;
   void *theEnv;
   int shift;
   int caretLine;
            
   if (! down) return;
   
   if ((vk != VK_LEFT) && (vk != VK_RIGHT) &&
       (vk != VK_UP) && (vk != VK_DOWN)) 
     { return; }

   theEnv = GetCurrentEnvironment();  
   if (CommandLineData(theEnv)->EvaluatingTopLevelCommand || BatchActive(theEnv))
     { return; }          
          
   theData = (struct displayWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) 
     { return; }
   
   commandSize = RouterData(GetCurrentEnvironment())->CommandBufferInputCount;
   theCommand = GetCommandString(GetCurrentEnvironment());

   RemoveSelection(hwnd,theData);
      
   if (GetKeyState(VK_SHIFT) & 0x8000)
     { shift = TRUE; }
   else
     { shift = FALSE; }
             
   switch (vk)
     {
      /*==================*/
      /* Handle Up Arrow. */
      /*==================*/
      
      case VK_UP:
        if (theData->currentCommand->next != NULL)
          {
           if (shift)
             { SwitchCommand(hwnd,theEnv,theData->currentCommand,theData->bottomCommand); }
           else
             { SwitchCommand(hwnd,theEnv,theData->currentCommand,theData->currentCommand->next); }
          }
        break;
        
      /*====================*/
      /* Handle Down Arrow. */
      /*====================*/
      
      case VK_DOWN:
        if (theData->currentCommand->prev != NULL)
          {
           if (shift)
             { SwitchCommand(hwnd,theEnv,theData->currentCommand,theData->topCommand); }
           else
             { SwitchCommand(hwnd,theEnv,theData->currentCommand,theData->currentCommand->prev); }
          }
        break;
        
      /*====================*/
      /* Handle Left Arrow. */
      /*====================*/
      
      case VK_LEFT:
        if (theData->caretOffset < commandSize)
          {         
           GetCaretPos(&thePoint);
           theData->caretOffset++; 
           theChar = theCommand[commandSize - theData->caretOffset];
           if (theChar == '\n')
             { 
              theData->caretLinesBack++;
              theData->caretCharsBack = 0;
              thePoint.y -= theData->lineSize; 
             }
           else
             { theData->caretCharsBack++; }
           
           if (shift)
             {
              while ((theData->caretOffset < commandSize) &&
                     (theCommand[commandSize - (theData->caretOffset + 1)] != '\n'))
                { 
                 theData->caretCharsBack++;
                 theData->caretOffset++;
                }
             }
             
           thePoint.x = ComputeCaretX(hwnd,theData);
           SetCaretPos(thePoint.x,thePoint.y);
           FlashBalancedParenthesis(theEnv,theData,FALSE);
          }
        break;

      /*=====================*/
      /* Handle Right Arrow. */
      /*=====================*/
      
      case VK_RIGHT:
        if (theData->caretOffset)
          {
           GetCaretPos(&thePoint);
           theChar = theCommand[commandSize - theData->caretOffset];
           theData->caretOffset--;

           if (theChar == '\n')
             { 
              theData->caretLinesBack--;
              theData->caretCharsBack = -1;
              
              caretLine = theData->lastLine - theData->caretLinesBack;
              if (caretLine < 0)
                { caretLine = DIALOG_SIZE + caretLine; }
              theData->caretCharsBack = strlen(theData->terminal[caretLine]);
              
              thePoint.y += theData->lineSize; 
             }
           else
             { theData->caretCharsBack--; }

           if (shift)
             {
              while ((theData->caretOffset > 0) &&
                     (theCommand[commandSize - theData->caretOffset] != '\n'))
                { 
                 theData->caretCharsBack--;
                 theData->caretOffset--;
                }
             }

           thePoint.x = ComputeCaretX(hwnd,theData);
           SetCaretPos(thePoint.x,thePoint.y);
           FlashBalancedParenthesis(theEnv,theData,FALSE);
          }
        break;
     }
  }

/**********************************************************/
/* display_OnLButtonDown:  */
/**********************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void display_OnLButtonDown(
  HWND hwnd,
  BOOL dblclk,
  int x,
  int y,
  UINT keyflags)
  {
#if WIN_MCW
#pragma unused(dblclk)
#pragma unused(keyflags)
#endif
   int theLine, theCharacter;
   struct displayWindowData *theData;
 
   theData = (struct displayWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) 
     { return; }

   SetCapture(hwnd);
   
   /*======================================================*/
   /* If the shift key is down, then extend the selection. */
   /*======================================================*/

   if ((GetKeyState(VK_SHIFT) & 0x8000) &&
       ((theData->selectionLineStart != theData->selectionLineEnd) ||
        (theData->selectionCharStart != theData->selectionCharEnd)))
     {
      display_OnScrollTimer(hwnd,x,y);
     }
   else
     {
      /*===========================*/
      /* Remove the old selection. */
      /*===========================*/
      
      InvertSelection(hwnd,theData);
      
      /*=============================================================*/
      /* Determine the starting line and character of the selection. */
      /*=============================================================*/
  
      FindInsertionPoint(hwnd,x,y,&theLine,&theCharacter);
       
      theData->selectionLineStart = theLine;
      theData->selectionCharStart = theCharacter;
      theData->selectionLineEnd = theLine;
      theData->selectionCharEnd = theCharacter;
     }
     
   HoverX = x;
   HoverY = y;
   
   ScrollTimer = SetTimer(hwnd,SCROLL_TIMER,100,(TIMERPROC) ScrollTimerProc);
  }

/**********************************************************/
/* display_OnLButtonUp:  */
/**********************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void display_OnLButtonUp(
  HWND hwnd,
  int x,
  int y,
  UINT keyflags)
  {
#if WIN_MCW
#pragma unused(keyflags)
#endif
   struct displayWindowData *theData;
   
   if (GetCapture() != hwnd) return;
 
   theData = (struct displayWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) 
     {
      ReleaseCapture();
      return; 
     }
   
   display_OnScrollTimer(hwnd,x,y);

   KillTimer(hwnd,ScrollTimer);
   
   ReleaseCapture();
   
   PostMessage(hwnd,UWM_UPDATE_MENU,0,0);
  }

/**********************************************************/
/* InvertSelection:  */
/**********************************************************/
static void InvertSelection(
   HWND hwnd,
   struct displayWindowData *theData)
  {
   HDC hDC;
   HRGN theRegion;
   HBRUSH hbrush; /* , hbrushOld; */
   int oldROP;

   hDC = GetDC(hwnd);
 
   oldROP = SetROP2(hDC,R2_MERGEPENNOT);
 
   /* hbrush = CreateSolidBrush(GetSysColor(COLOR_HIGHLIGHT)); */

   hbrush = CreateSolidBrush(RGB(0,0,0));

   /* hbrushOld = SelectObject(hDC,hbrush); */
   
   theRegion = DisplaySelectionRegion(hwnd,theData);

   FillRgn(hDC,theRegion,hbrush);
         
   DeleteObject(hbrush);
   
   /* SelectObject(hDC,hbrushOld); */
    
   DeleteObject(theRegion);
    
   SetROP2(hDC,oldROP);
    
   ReleaseDC(hwnd,hDC);  
  }

/**********************************************************/
/* display_OnMouseMove:  */
/**********************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void display_OnMouseMove(
  HWND hwnd,
  int x,
  int y,
  UINT keyflags)
  {
#if WIN_MCW
#pragma unused(x)
#pragma unused(y)
#pragma unused(keyflags)
#endif
   
   if (GetCapture() != hwnd) return;   

   HoverX = x;
   HoverY = y;      
  }
    
/**********************************************************/
/* display_OnScrollTimer:  */
/**********************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void display_OnScrollTimer(
  HWND hwnd,
  int x,
  int y)
  {
#if WIN_MCW
#pragma unused(x)
#pragma unused(y)
#endif
   int theLine, theCharacter;
   struct displayWindowData *theData;
   int endBeforeStart;
   int osc, osl;
   int min, max, pos, invalid = FALSE;
   RECT rect;
   
   if (GetCapture() != hwnd) return;   

   theData = (struct displayWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) 
     { return; }

   FindInsertionPoint(hwnd,x,y,&theLine,&theCharacter);
       
   GetScrollRange(hwnd,SB_VERT,&min,&max);
   pos = GetScrollPos(hwnd,SB_VERT);
   if (min != max)
     {
      if (theLine > pos)
        { 
         SetScrollPos(hwnd,SB_VERT,theLine,TRUE);
         /*
         GetClientRect(hwnd,&rect);
         rect.bottom = rect.top + (theData->lineSize * (theData->linesInDisplayWindow + 1));         
         ScrollWindowEx(hwnd,0,(pos - theLine) * theData->lineSize,&rect,&rect,NULL,NULL,SW_ERASE | SW_INVALIDATE);
         */
         invalid = TRUE;
        }
      else if (theLine < (pos - theData->linesInDisplayWindow))
        {
         SetScrollPos(hwnd,SB_VERT,theLine + theData->linesInDisplayWindow,TRUE);
         invalid = TRUE;
        }
     } 

   GetScrollRange(hwnd,SB_HORZ,&min,&max);
   pos = GetScrollPos(hwnd,SB_HORZ);
   GetClientRect(hwnd,&rect);
   
   if (x < 0)
     {
      pos += (x / theData->maxCharWidth);
      if (pos < 0) pos = 0;
      SetScrollPos(hwnd,SB_HORZ,pos,TRUE);
      invalid = TRUE;
     } 
   else if (x > (rect.right - rect.left))
     {
      pos += (x - (rect.right - rect.left)) / theData->maxCharWidth;
      if (pos > max) pos = max;
      SetScrollPos(hwnd,SB_HORZ,pos,TRUE);
      invalid = TRUE;
     }     
     
   if (invalid)
     { 
      theData->selectionLineEnd = theLine;
      theData->selectionCharEnd = theCharacter;
      InvalidateRect(hwnd,NULL,TRUE);
      return; 
     }
 
    if ((theLine == theData->selectionLineEnd) &&
        (theCharacter == theData->selectionCharEnd))
     { return; }
           
   if (theData->selectionLineEnd < theData->selectionLineStart)
     { endBeforeStart = TRUE; }
   else if (theData->selectionLineEnd < theData->selectionLineStart)
     { endBeforeStart = FALSE; }
   else if (theData->selectionCharEnd < theData->selectionCharStart)
     { endBeforeStart = TRUE; }
   else
     { endBeforeStart = FALSE; }
   
   if (endBeforeStart)
     {
      if ((theLine < theData->selectionLineEnd) ||
          ((theLine == theData->selectionLineEnd) &&
           (theCharacter < theData->selectionCharEnd)))
        {
         osc = theData->selectionCharStart;
         osl = theData->selectionLineStart;
                  
         theData->selectionLineStart = theData->selectionLineEnd;
         theData->selectionCharStart = theData->selectionCharEnd;
         theData->selectionLineEnd = theLine;
         theData->selectionCharEnd = theCharacter;
         
         InvertSelection(hwnd,theData);
         
         theData->selectionCharStart = osc;
         theData->selectionLineStart = osl;
        }     
      else if ((theLine > theData->selectionLineStart) ||
               ((theLine == theData->selectionLineStart) &&
                (theCharacter > theData->selectionCharStart)))
        {
         InvertSelection(hwnd,theData);
         theData->selectionLineEnd = theLine;
         theData->selectionCharEnd = theCharacter;
         InvertSelection(hwnd,theData);
        }
      else
        {
         osc = theData->selectionCharStart;
         osl = theData->selectionLineStart;
         
         theData->selectionCharStart = theCharacter;
         theData->selectionLineStart = theLine;        

         InvertSelection(hwnd,theData);

         theData->selectionCharStart = osc;
         theData->selectionLineStart = osl;  
         theData->selectionCharEnd = theCharacter;
         theData->selectionLineEnd = theLine;
        }
     }
   else
     {
      if ((theLine < theData->selectionLineStart) ||
          ((theLine == theData->selectionLineStart) &&
           (theCharacter < theData->selectionCharStart)))
        {
         InvertSelection(hwnd,theData);
         theData->selectionLineEnd = theLine;
         theData->selectionCharEnd = theCharacter;
         InvertSelection(hwnd,theData);
        }
      else if ((theLine > theData->selectionLineEnd) ||
               ((theLine == theData->selectionLineEnd) &&
                (theCharacter > theData->selectionCharEnd)))
        {
         osc = theData->selectionCharStart;
         osl = theData->selectionLineStart;
         
         theData->selectionCharStart = theData->selectionCharEnd;
         theData->selectionLineStart = theData->selectionLineEnd;
         theData->selectionCharEnd = theCharacter; 
         theData->selectionLineEnd = theLine; 
         
         InvertSelection(hwnd,theData);
         
         theData->selectionCharStart = osc;
         theData->selectionLineStart = osl;
        }
      else
        {
         osc = theData->selectionCharStart;
         osl = theData->selectionLineStart;
         
         theData->selectionCharStart = theCharacter;
         theData->selectionLineStart = theLine;        

         InvertSelection(hwnd,theData);

         theData->selectionCharStart = osc;
         theData->selectionLineStart = osl;  
         theData->selectionCharEnd = theCharacter;
         theData->selectionLineEnd = theLine;
        }
     }     
  }

/*************************************************/
/* ComputeCaretX: Computes the x position of the */
/*   carte based on the current caret settings.  */
/*************************************************/
static int ComputeCaretX(
  HWND hwnd,
  struct displayWindowData *theData)
  {
   char *buffer; 
   int caretLine;
   int count, pos;
   size_t bufferLength;

   /*================================================*/
   /* Determine the line on which the caret resides. */
   /*================================================*/
   
   caretLine = theData->lastLine - theData->caretLinesBack;
   if (caretLine < 0)
     { caretLine = DIALOG_SIZE + caretLine; }
                            
   buffer = theData->terminal[caretLine];
   
   if (buffer == NULL)
     { return(LEFT_MARGIN_SPACE); }
          
   bufferLength = strlen(buffer);
      
   if (theData->caretCharsBack == -1)
     { theData->caretCharsBack = bufferLength; }
      
   count = bufferLength - theData->caretCharsBack;    
     
   pos = GetScrollPos(hwnd,SB_HORZ);
   
   /*=======================================*/
   /* If the caret is scrolled out of view, */
   /* return a value that draw it off the   */
   /* left side of the window.              */
   /*=======================================*/
   
   if (pos > count)
     { return(-20); }
       
   else if (pos == count)     
     { return(LEFT_MARGIN_SPACE); }
   else  if (pos != 0)   
     { return(LEFT_MARGIN_SPACE + (count - pos) * theData->maxCharWidth); }
   else
     { return(LEFT_MARGIN_SPACE + count * theData->maxCharWidth); }
  }         
           
/**************************************************/
/* CreateTerminal: Initialize the structure which */
/*   will hold all the text for the terminal.     */
/**************************************************/
static void CreateTerminal(
  struct displayWindowData *theData)
  {
   int i;
     
   theData->terminal = (char **) malloc (sizeof(char *)*(DIALOG_SIZE+1));
   
   if (theData->terminal == NULL)
     { ExitToShell(); }
   
   for (i = 0; i <= (DIALOG_SIZE); i++)
     { theData->terminal[i] = NULL; }
  }

/*****************************************/
/* FreeTerminalText: Frees all line text */
/*   associated with the Terminal.       */
/*****************************************/
static void FreeTerminalText(
  HWND hwnd)
  { 
   int i;
   struct displayWindowData *theData;
   
   theData = (struct displayWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) 
     { return; }

   for (i = 0; i <= DIALOG_SIZE; i++)
     {  
      if (theData->terminal[i] != NULL)
	    { 
	     free(theData->terminal[i]);
         theData->terminal[i] = NULL;
        }
	}
}

/************************************/
/* DeleteTerminal: Frees all memory */
/*   associated with the Terminal.  */
/************************************/
static void DeleteTerminal(
  HWND hwnd)
  {  
   struct displayWindowData *theData;
   
   theData = (struct displayWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) 
     { return; }
     
   FreeTerminalText(hwnd);
   free(theData->terminal);
  }

/********************************************************/
/* RedrawTerminal: Draw dialog window based on the min, */
/*   max, and current position of the thumbnail in the  */
/*   horizontal and vertical scroll bars.               */
/********************************************************/
static void RedrawTerminal( 
  HWND hwnd,
  HDC hDC)
  {
   int x, max, min, pos, vpos, start;
   RECT Rect;
   char *buffer = NULL;
   HFONT hfont;
   struct displayWindowData *theData;
   LOGFONT lf;
   HRGN theRegion;

   theData = (struct displayWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) 
     { return; }
   
   memset(&lf,0,sizeof(LOGFONT));
   lf.lfHeight = DISPLAY_FONT_HEIGHT;
   strcpy(lf.lfFaceName,DISPLAY_FONT_NAME);
   hfont = CreateFontIndirect(&lf);

   if (hfont != NULL)
     { SelectObject(hDC,hfont); }   
          
   /*===============================================*/
   /* Get current value of the vertical scroll bar. */
   /*===============================================*/
   
   GetScrollRange(hwnd,SB_VERT,&min,&max);
   vpos = GetScrollPos(hwnd,SB_VERT);

   /*===========================================*/
   /* Locate the first visible line to display. */
   /*===========================================*/
   
   start = theData->lastLine - (max - vpos) - theData->linesInDisplayWindow;

   if (start == theData->lastLine)
     { start++; }

   /*================================================*/
   /* If the start value is negative, then check if  */
   /* at a 'hard' top or if a wrap around is needed. */
   /*================================================*/
   
   if (start < 0)
     {  
      if (max == DIALOG_SIZE)
	    { start = max + start + 1; }
      else
        { start = 0; }
     }

   /*============================================*/
   /* Get Position of the horizontal scroll bar. */
   /*============================================*/
   
   pos = GetScrollPos(hwnd,SB_HORZ);

   /*============================================================*/
   /* Loop to display text in the visible portion of the screen. */
   /*============================================================*/
   
   for (x = 0; x <= theData->linesInDisplayWindow; x++)
     {
      size_t bufsize;

      GetClientRect(hwnd,&Rect);
      Rect.left = 2;
      Rect.top = x * theData->lineSize;

      /*=================================*/
      /* Calculate horizontal scroll bar */ 
      /* max based on what is displayed. */
      /*=================================*/
      
      if (theData->terminal[start] != NULL)
        { bufsize = strlen(theData->terminal[start]); }
      else
        { bufsize = 0; }

      /*====================================*/
      /* Display each line of text adjusted */
      /* for the horizontal scroll bar.     */
      /*====================================*/
      
      buffer = NULL;
      if ((pos <= (int) bufsize ) && (theData->terminal[start] != NULL))
        {  
         buffer = theData->terminal[start] + pos;
	     DrawText(hDC,buffer,-1,&Rect,DT_LEFT | DT_NOCLIP | DT_NOPREFIX );
        }

      /*=================================*/
      /* Check if wrap around is needed. */
      /*=================================*/
      
      if (start == theData->lastLine) 
        { break; }
        
      start++;
      if (start > DIALOG_SIZE)
        { start = 0; }
     }

   /*==================================*/
   /* Highlight the current selection. */
   /*==================================*/

   theRegion = DisplaySelectionRegion(hwnd,theData);

   if (FORWARD_WM_MDIGETACTIVE(MDIClientWnd,SendMessage) == hwnd)
     { 
      InvertRgn(hDC,theRegion); 
     }
  
   DeleteObject(theRegion);
         
   /*======================================*/
   /* Calculate and display caret adjusted */ 
   /* for horizontal scroll bar.           */
   /*======================================*/

   if (buffer == NULL)
     { buffer = ""; }
   
   DrawText(hDC,buffer,-1,&Rect,DT_LEFT | DT_NOCLIP | DT_CALCRECT);

   if (FORWARD_WM_MDIGETACTIVE(MDIClientWnd,SendMessage) == hwnd)
     {
      Rect.top -= (theData->caretLinesBack * theData->lineSize);             
      Rect.top += ((max - vpos) * theData->lineSize);

      Rect.right = ComputeCaretX(hwnd,theData); 

      SetCaretPos(Rect.right,Rect.top);
     }
  
   if (hfont != NULL)
     { DeleteFont(hfont); }   
  }
  
/************************************************************/
/* DisplayLineCountAndStart: Returns the number of lines in */
/*   the display window and the index of the first line.    */
/************************************************************/
void DisplayLineCountAndStart( 
  HWND hwnd,
  int *lines,
  int *start)
  {
   struct displayWindowData *theData;
   
   theData = (struct displayWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) 
     { return; }
        
   if (theData->lastLine == DIALOG_SIZE)
     {
      *start = 0;
      *lines = DIALOG_SIZE + 1;
     }
   else if (theData->terminal[theData->lastLine + 1] == NULL)
     {
      *start = 0;
      *lines = theData->lastLine + 1;
     }
   else 
     {
      *start = theData->lastLine + 1;
      *lines = DIALOG_SIZE + 1;
     }
  }

/*****************************************************/
/* DisplayPrint: Function will allocate memory used */
/*   to store strings sent to the Dialog window.     */
/*****************************************************/
#if WIN_BTC
#pragma argsused
#endif
int DisplayPrint( 
  HWND hwnd,
  char *buffer)
  {       
   unsigned Loc = 0; 
   char *str; 
   size_t oldsize, size;
   struct displayWindowData *theData;
   
   theData = (struct displayWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) 
     { return(FALSE); }

   if (buffer[0] == '\b') 
     { return(TRUE); }
   
   /*============================================*/
   /* Allocate room for the buffer to be parsed. */
   /*============================================*/

   str = (char *) malloc(strlen(buffer)+1);
   if (str == NULL)
     { ExitToShell(); }

   /*============================================*/
   /* Reset the caret positional flags whenever  */
   /* anything is printed to the display window. */
   /*============================================*/
   
   theData->caretOffset = 0;
   theData->caretLinesBack = 0;
   theData->caretCharsBack = 0;
   
   /*================================================*/
   /* Loop through each 'part' of the buffer string  */
   /* Note: 'part' is text between carriage returns. */
   /*================================================*/
   
   while (Loc < strlen(buffer))
     {
      str[0] = '\0';
     
      /*====================================*/
      /* Capture text up to the line break. */
      /*====================================*/
      
      sscanf(&(buffer[Loc]),"%[^\n]",str);

      /*==========================================*/
      /* Allocate new memory if new line of text  */
      /* or reallocate if appending line of text. */
      /*==========================================*/

      if (theData->terminal[theData->lastLine] != NULL)
	    { oldsize = strlen(theData->terminal[theData->lastLine]); }
      else
        { oldsize = 0; }

      size = oldsize + strlen(str)+1;
      
      if (theData->terminal[theData->lastLine] != NULL)
        { theData->terminal[theData->lastLine] = (char *) realloc(theData->terminal[theData->lastLine],size); }
      else
        { theData->terminal[theData->lastLine] = (char *) malloc(size); }

      if (theData->terminal[theData->lastLine] == NULL)
        { ExitToShell(); }

      /*===================================*/
      /* Copy string to the dialog window. */
      /*===================================*/
      
      theData->terminal[theData->lastLine][oldsize] = '\0';
      strcat(theData->terminal[theData->lastLine],str);
      Loc += strlen(str);

      if (buffer[Loc] == '\n')
        {  
         int min, max;
         
         /*================================*/
         /* Display line before advancing. */
         /*================================*/
         
         SendToScreen(hwnd);
         theData->lastLine++;
         if (theData->lastLine > DIALOG_SIZE) 
           { theData->lastLine = 0; }

         /*=========================*/
         /* Free next line of text. */
         /*=========================*/
         
         if (theData->terminal[theData->lastLine] != NULL)
           {
            free(theData->terminal[theData->lastLine]);
            theData->terminal[theData->lastLine] = NULL;
           }

         /*=====================*/
         /* Update scroll bars. */
         /*=====================*/
         
         GetScrollRange(hwnd,SB_VERT,&min,&max);
         if (max < DIALOG_SIZE) 
           { 
            max = theData->lastLine;
           }
         else 
           { max = DIALOG_SIZE; }
           
         if (max > theData->linesInDisplayWindow)
           { 
            SetScrollRange(hwnd,SB_VERT,theData->linesInDisplayWindow,max,FALSE); 
            SetScrollPos(hwnd,SB_VERT,max,TRUE);
           }
         else
           { 
            SetScrollRange(hwnd,SB_VERT,0,0,FALSE); 
            SetScrollPos(hwnd,SB_VERT,0,TRUE);
           }

         GetScrollRange(hwnd,SB_HORZ,&min,&max);
         if (max < (int) size) max = (int) size;
         //SetScrollRange(hwnd,SB_HORZ,0,max,FALSE);
         SetScrollPos(hwnd,SB_HORZ,0,TRUE);
		}

      SendToScreen(hwnd);
      Loc++;
	 }
	 
   free(str);

   return (TRUE);
  }

/**********************************************************************/
/* SendToScreen: Displays the current text line in the Dialog Window. */
/**********************************************************************/
static void SendToScreen(
  HWND hwnd)
  {         
   RECT DRect;                     /* Client Area of Dialog Window */
   RECT Rect;                      /* Adjusted Area for scrolling */
   HDC hDC = GetDC(DialogWindow);  /* Handle to the device context */
   int min, max, pos;              /* Scroll Bar Values */
   int Scroll = 0;                 /* Scrolling Needed? */
   HANDLE OldObject;
   HFONT hfont;
   struct displayWindowData *theData;
   LOGFONT lf;
  
   theData = (struct displayWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) 
     { return; }
   
   GetClientRect(hwnd,&Rect);
   GetClientRect(hwnd,&DRect);
   
   if (hwnd == FORWARD_WM_MDIGETACTIVE(MDIClientWnd,SendMessage))
     { HideCaret(hwnd); }

   memset(&lf,0,sizeof(LOGFONT));
   lf.lfHeight = DISPLAY_FONT_HEIGHT;
   strcpy(lf.lfFaceName,DISPLAY_FONT_NAME);
   hfont = CreateFontIndirect(&lf);
   if (hfont != NULL)
     { SelectFont(hDC,hfont); }
   
   /*========================================================*/
   /* Move to the bottom of the screen if not already there. */
   /*========================================================*/

   GetScrollRange(hwnd,SB_VERT,&min,&max);
   pos = GetScrollPos(hwnd,SB_VERT);
   if (pos != max)
     {  
      InvalidateRect(hwnd,NULL,TRUE);
      SetScrollPos(hwnd,SB_VERT,max,FALSE);
      SendMessage(hwnd,WM_PAINT,0,0);
     }

   /*==========================================================*/
   /* Determine if the screen is full and scrolling is needed. */
   /*==========================================================*/

   if (max > theData->linesInDisplayWindow) Scroll = 1;

   /*===============================================================*/
   /* Scroll Window if newline and text will not fit on the screen. */
   /*===============================================================*/

   if (Scroll && theData->lastLine != theData->oldLine)
     {  
      theData->oldLine = theData->lastLine;
      ScrollDC (hDC,0,-theData->lineSize,&DRect,&DRect,NULL,&Rect);
     }

   /*==========================================*/
   /* Calculate where text is to be displayed. */
   /*==========================================*/

   Rect.left = 2;
   if (! Scroll)
     { Rect.top = (theData->lastLine) * theData->lineSize; }
   else
     { Rect.top = (theData->linesInDisplayWindow) * theData->lineSize; }

   /*=============================*/
   /* Clear line to be displayed. */
   /*=============================*/
   
   OldObject = SelectObject(hDC,GetStockObject(WHITE_PEN));
   Rectangle(hDC,Rect.left,Rect.top,Rect.right,Rect.bottom);
   SelectObject(hDC,OldObject);

   /*============================*/
   /* Display Text Adjusting     */
   /* for the Horizontal scroll. */
   /*============================*/
   
   pos = GetScrollPos(hwnd,SB_HORZ);
     
   if (theData->terminal[theData->lastLine] == NULL)
     { Rect.right = 2; /* Do Nothing */ }
   else if (pos < (int) strlen(theData->terminal[theData->lastLine]))
	 {
	  DrawText(hDC,theData->terminal[theData->lastLine]+pos,-1,&Rect,
               DT_LEFT | DT_NOCLIP | DT_SINGLELINE | DT_NOPREFIX );
      DrawText(hDC,theData->terminal[theData->lastLine]+pos,-1,&Rect,
               DT_LEFT | DT_NOCLIP | DT_SINGLELINE | DT_CALCRECT | DT_NOPREFIX);
     }
   else
     { Rect.right = 2; }

   /*=================================*/
   /* Automatic horizontal scrolling. */
   /*=================================*/

   if (theData->terminal[theData->lastLine] != NULL)
     {
      DrawText(hDC,theData->terminal[theData->lastLine]+pos,-1,&Rect,
               DT_LEFT | DT_NOCLIP | DT_SINGLELINE | DT_NOPREFIX );
     }
     
   if (Rect.right > DRect.right && theData->horizScroll)
     {
      GetScrollRange(hwnd,SB_HORZ,&min,&max);
      pos++;
      if (max < pos) max = pos;
      SetScrollPos(hwnd,SB_HORZ,pos,TRUE);
      InvalidateRect(hwnd,NULL,TRUE);
     }

   theData->oldLine = theData->lastLine;
   ReleaseDC(hwnd,hDC);  

   /*EB*/ /* Show caret after scroll */ 

   /*=============*/
   /* Show caret. */
   /*=============*/

   if (FORWARD_WM_MDIGETACTIVE(MDIClientWnd,SendMessage) == hwnd)
     {
      SetCaretPos(Rect.right,Rect.top);
      ShowCaret(hwnd);
     }
   /*EB*/
    
   if (hfont != NULL)
     { DeleteFont(hfont); }
  }

/******************************************/
/* ClearWindowCommand: H/L access routine */
/*   for the clear-window command.        */
/******************************************/
void ClearWindowCommand(
  void *theEnv)
  {
   if (EnvArgCountCheck(theEnv,"clear-window",EXACTLY,0) == -1) return;
   ClearDialogWnd();
  }
  
/*****************************************************************/
/* ClearDialogWnd: Procedure will clear all text from the dialog */
/*    window and free storage in the terminal data structure.    */
/*****************************************************************/
void ClearDialogWnd(void)
  {
   struct displayWindowData *theData;
   
   theData = (struct displayWindowData *) GetWindowLong(DialogWindow,GWL_USERDATA);
   
   if (theData == NULL) 
     { return; }
     
   /*===========================================*/
   /* Free all data associated with the screen. */
   /*===========================================*/
   
   FreeTerminalText(DialogWindow);

   /*=====================================*/
   /* Reset information about the screen. */
   /*=====================================*/
   
   theData->lastLine = 0;
   theData->oldLine = 0;
   
   theData->selectionLineStart = 0;
   theData->selectionLineEnd = 0;
   theData->selectionCharStart = 0;
   theData->selectionCharEnd = 0;

   /*=====================*/
   /* Update scroll bars. */
   /*=====================*/
  
   SetScrollRange(DialogWindow,SB_VERT,0,0,FALSE); 
   SetScrollPos(DialogWindow,SB_VERT,0,TRUE);
   SetScrollPos(DialogWindow,SB_HORZ,0,TRUE);
   InvalidateRect(DialogWindow,NULL,TRUE);
   FlushCommandString(GetCurrentEnvironment());
  }

/******************************************/
/* ExitToShell: Quit and exit to Windows. */
/******************************************/
void ExitToShell(void)
  {   
   MessageBeep(0);
   MessageBox(DialogWindow, "CLIPS is out of memory!!", "-ERROR-", MB_ICONHAND | MB_OK );
   PostMessage(DialogWindow, WM_COMMAND, ID_APP_EXIT, 0);
  }

/***********************************************/
/* GetUserCmd: Function used to filter/display */ 
/*   characters typed in from the keyboard.    */
/***********************************************/
void GetUserCmd( 
  HWND hwnd,
  WORD wParam,      /* Key Code */
  BOOL screenOnly,  /* Send to Screen and or Command Buffer */
  unsigned inputSize)   /* Number of characters send to screen only */
  {
   struct displayWindowData *theData;
   POINT thePoint;
   void *theEnv;
   
   theData = (struct displayWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) 
     { return; }
     
   RemoveSelection(hwnd,theData);
   
   switch (wParam)
     {
      case VK_ESCAPE:
        theEnv = GetCurrentEnvironment();  
        if (CommandLineData(theEnv)->EvaluatingTopLevelCommand || BatchActive(theEnv))
          { break; }
              
        GetCaretPos(&thePoint);
        thePoint.y += (theData->caretLinesBack * theData->lineSize);
        theData->caretOffset = 0;
        theData->caretLinesBack = 0;
        theData->caretCharsBack = 0;
        thePoint.x = ComputeCaretX(hwnd,theData);
        SetCaretPos(thePoint.x,thePoint.y);
        break;
        
      /*===================*/
      /* Handle backspace. */
      /*===================*/

      case VK_BACK:
        HandleBackSpace(hwnd,theData,screenOnly,inputSize);
        break;

      /*============================*/
      /* Remove special keys (ALT). */
      /*============================*/
      
      case '\f':
      case VK_MENU:
        break;

      /*==================*/
      /* Handle tab keys. */
      /*==================*/
      
      case '\t':  
        if (! screenOnly)
	      {  
	       if (GetCommandString(GetCurrentEnvironment()) == NULL)
	         { SetCommandString(GetCurrentEnvironment(),"   "); }
           else
             { AppendCommandString(GetCurrentEnvironment(),"   "); }
          }
        EnvPrintRouter (GetCurrentEnvironment(),"stdout", "   " );
        break;
      
      /*=====================*/
      /* Return/newline key. */
      /*=====================*/
      
      case '\r':
      case '\n':
        wParam = (int)'\n';
	    if (GetScrollPos(hwnd,SB_HORZ) != 0)
	      {  
	       SetScrollPos (hwnd,SB_HORZ,0,TRUE);
           InvalidateRect(hwnd, NULL,FALSE);
           SendMessage(hwnd,WM_PAINT,0,0);
          }

      /*=======================*/
      /* All other characters. */
      /*=======================*/
      
      default:
        HandleStandardCharacter(hwnd,theData,screenOnly,(char) wParam);
        break;
     }
     
   theData->horizScroll = 0;
  }

/**********************************************/
/* HandleStandardCharacter: Handles typing of */
/*    characters that do not have any special */
/*    processing requirements in the display  */
/*    window.                                 */
/**********************************************/
static void HandleStandardCharacter( 
  HWND hwnd,
  struct displayWindowData *theData,
  BOOL screenOnly,      /* Send to Screen and or Command Buffer */
  char theChar)
  {
   char text[2];
   char *theCommand, *preCommand, *postCommand;
   void *theEnv;
   size_t length;
   int tempOffset, tempChars, tempLines;
     
   /*============================================*/
   /* Only process characters that are printable */
   /* or represent white space.                  */
   /*============================================*/
      
   if ((! isprint (theChar)) && (! isspace(theChar)))
     { return; }
      
   text[0] = theChar;
   text[1] = '\0';
   
   theEnv = GetCurrentEnvironment();
  
   /*==============================*/
   /* Add to CLIPS command buffer. */
   /*==============================*/
           
   if (! screenOnly)
     { 
      theCommand = GetCommandString(theEnv);
      if (theCommand == NULL)
		{ 
		 SetCommandString(theEnv,text);
		 EnvPrintRouter(theEnv,"stdout",text); 
         FlashBalancedParenthesis(theEnv,theData,TRUE);
		}
	  else if (theData->caretOffset == 0)
       	{  
       	 AppendCommandString(theEnv,text); 
       	 EnvPrintRouter(theEnv,"stdout",text);
         FlashBalancedParenthesis(theEnv,theData,TRUE);
       	}
      else
        {
         /*=================================================*/
         /* The command string needs to be split in two so  */
         /* the typed character can be inserted in the      */
         /* middle. Create the storage for the two strings. */
         /*=================================================*/
                                         
         length = strlen(theCommand);
         if (length == 0) return;
         
         postCommand = (char *) malloc(theData->caretOffset + 1);
         if (postCommand == NULL) return;
         
         preCommand = (char *) malloc((length - theData->caretOffset) + 1);
         if (preCommand == NULL)
           {
            free(postCommand); 
            return; 
           }
            
         /*================================================*/
         /* Store the current command string in two parts. */
         /*================================================*/
                  
         strncpy(postCommand,&theCommand[length - theData->caretOffset],theData->caretOffset+1);
         strncpy(preCommand,theCommand,length - theData->caretOffset);
         preCommand[length - theData->caretOffset] = '\0';
         
         /*========================================*/
         /* Create the new command string with the */
         /* character inserted at the caret.       */
         /*========================================*/
         
         SetCommandString(theEnv,preCommand);
         AppendCommandString(theEnv,text);
         AppendCommandString(theEnv,postCommand);

         /*========================================*/
         /* Quickly delete the current user input. */
         /*========================================*/
         
         for (; length != 0; length--)
           { QuickBackSpace(hwnd,theData); }
           
         /*=============================*/
         /* Preserve the caret offsets. */
         /*=============================*/
         
         tempOffset = theData->caretOffset;
         tempChars = theData->caretCharsBack;
         tempLines = theData->caretLinesBack;
         
       	 EnvPrintRouter(theEnv,"stdout",preCommand);
       	 EnvPrintRouter(theEnv,"stdout",text);
       	 EnvPrintRouter(theEnv,"stdout",postCommand);
         
         /*============================*/
         /* Restore the caret offsets. */
         /*============================*/
         
         theData->caretOffset = tempOffset;
         theData->caretCharsBack = tempChars;
         theData->caretLinesBack = tempLines;
            
         if (theChar == ')')   
           { FlashBalancedParenthesis(theEnv,theData,TRUE); }
         
         InvalidateRect (hwnd,NULL,TRUE);	    
     
         /*============================================*/
         /* Release the memory used for the two parts. */
         /*============================================*/
         
         free(postCommand);
         free(preCommand);         
        }
        
      return;
     }
          
   EnvPrintRouter(theEnv,"stdout",text);
  }
  
/***************************************************/
/* QuickBackSpace: Handles deletion of a character */
/*   from the terminal without updating the CLIPS  */
/*   command string. Returns TRUE if a carriage    */
/*   return was deleted, otherwise FALSE.          */
/***************************************************/
static int QuickBackSpace( 
  HWND hwnd,
  struct displayWindowData *theData)
  {
   size_t size;
   int rv;
   char text[2];
   
   /*================================*/
   /* Initialize Values when sending */
   /* to the command buffer.         */
   /*================================*/
     
   if (theData->terminal[theData->lastLine] != NULL)
     { size = strlen(theData->terminal[theData->lastLine]); }
   else
     { size = 0; }

   if (size > 0)
     { 
      theData->terminal[theData->lastLine][size - 1] = '\0'; 
      rv = FALSE;
     }
   else
     {  
      int min, max;

      if (theData->terminal[theData->lastLine] != NULL)
        {
	     free(theData->terminal[theData->lastLine]);
         theData->terminal[theData->lastLine] = NULL;
	    }

      theData->lastLine--;
      theData->oldLine--;

      if (theData->lastLine < 0)
        { theData->lastLine = DIALOG_SIZE; }

      GetScrollRange(hwnd,SB_VERT,&min,&max);
      if ((theData->linesInDisplayWindow < max) && (max < DIALOG_SIZE))
        {  
         SetScrollRange(hwnd,SB_VERT,theData->linesInDisplayWindow,max-1,FALSE);
	     SetScrollPos(hwnd,SB_VERT,max-1,FALSE);
        }
      else
        {
         SetScrollRange(hwnd,SB_VERT,0,0,FALSE); 
         SetScrollPos(hwnd,SB_VERT,0,TRUE);
        }
        
      rv = TRUE;
	 }
	 
   text[0] = (char) VK_BACK;
   text[1] = '\0';
   EnvPrintRouter(GetCurrentEnvironment(),"stdout",text);
   
   return rv;
  }
  
/**************************************************/
/* HandleBackSpace: Handles typing of a backspace */
/*    character in the display window.            */
/**************************************************/
static void HandleBackSpace( 
  HWND hwnd,
  struct displayWindowData *theData,
  BOOL screenOnly,      /* Send to Screen and or Command Buffer */
  unsigned inputSize)   /* Number of characters send to screen only */
  {
   size_t length;
   void *theEnv;
   char *theCommand, *preCommand, *postCommand;
   int tempOffset, tempChars, tempLines;

   /*================================*/
   /* Initialize Values when sending */
   /* to the command buffer.         */
   /*================================*/

   if (! screenOnly)
     {      
      theEnv = GetCurrentEnvironment();
      theCommand = GetCommandString(theEnv);
      
      theData->horizScroll = 1;
	  if (theCommand == NULL)
	    { return; }
      inputSize = strlen(theCommand);
     }
                   
   if (inputSize == 0)
     { return; }
     
   if (! screenOnly)
     { 
      if (theData->caretOffset == 0)
        { ExpandCommandString (theEnv,'\b'); }
      else
        {
         /*=================================================*/
         /* The command string needs to be split in two so  */
         /* the deleted character can be removed from the   */
         /* middle. Create the storage for the two strings. */
         /*=================================================*/
                                         
         length = strlen(theCommand);
         if (length == 0) return;
         
         postCommand = (char *) malloc(theData->caretOffset + 1);
         if (postCommand == NULL) return;
         
         preCommand = (char *) malloc(length - theData->caretOffset);
         if (preCommand == NULL)
           {
            free(postCommand); 
            return; 
           }
            
         /*================================================*/
         /* Store the current command string in two parts. */
         /*================================================*/
                  
         strncpy(postCommand,&theCommand[length - theData->caretOffset],theData->caretOffset+1);
         strncpy(preCommand,theCommand,length - (theData->caretOffset + 1));
         preCommand[length - (theData->caretOffset + 1)] = '\0';
         
         /*========================================*/
         /* Create the new command string with the */
         /* character removed at the caret.        */
         /*========================================*/
         
         SetCommandString(theEnv,preCommand);
         AppendCommandString(theEnv,postCommand);

         /*=============================*/
         /* Preserve the caret offsets. */
         /*=============================*/
         
         tempOffset = theData->caretOffset;
         tempChars = theData->caretCharsBack;
         tempLines = theData->caretLinesBack;
         
         /*========================================*/
         /* Quickly delete the current user input. */
         /*========================================*/
         
         for (; length != 0; length--)
           { QuickBackSpace(hwnd,theData); }
           
         /*=================================================*/
         /* Reprint the command with the character deleted. */
         /*=================================================*/
           
       	 EnvPrintRouter(theEnv,"stdout",preCommand);
       	 EnvPrintRouter(theEnv,"stdout",postCommand);
         
         /*============================*/
         /* Restore the caret offsets. */
         /*============================*/
         
         theData->caretOffset = tempOffset;
         theData->caretCharsBack = tempChars;
         theData->caretLinesBack = tempLines;

         FlashBalancedParenthesis(theEnv,theData,FALSE);            
         InvalidateRect(hwnd,NULL,TRUE);	    
     
         /*============================================*/
         /* Release the memory used for the two parts. */
         /*============================================*/
         
         free(postCommand);
         free(preCommand); 
         
         return;        
        }
     }
        
   if (QuickBackSpace(hwnd,theData))
     { InvalidateRect(hwnd,NULL,TRUE); }

   if (! screenOnly)
     { FlashBalancedParenthesis(theEnv,theData,FALSE); }           
	    
   SendToScreen(hwnd);
  }
  
/***************************************/
/* SaveDisplayWindow: Saves the output */
/*   of a display window to a file.    */
/***************************************/
static void SaveDisplayWindow( 
  HWND hwnd)
  {  
   OPENFILENAME ofn;
   char File[256], FileTitle[256], Filter[256];
   UINT i;
   int cbString;
   char Replace;
   int x;
   size_t size;
   struct displayWindowData *theData;
   FILE *fp;
   int lines, displayStart, iLineNum;
   
   theData = (struct displayWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) return; 

   sprintf(File,"DialogWindow.txt");
   memset(&ofn,0,sizeof(OPENFILENAME));
   ofn.lpstrTitle = "Select Dialog Window Save File";

   if ((cbString = LoadString(GetWindowInstance(hwnd),IDS_TEXT_FILES,Filter,sizeof(Filter))) == 0)
     { return; }

   Replace = Filter[cbString-1];
   for (i=0; Filter[i] != '\0'; i++)
     {
      if (Filter[i] == Replace)
        { Filter[i] = '\0'; }
     }

   ofn.lStructSize = sizeof(OPENFILENAME);
   ofn.hwndOwner = hwnd;
   ofn.lpstrFilter = Filter;
   ofn.nFilterIndex = 1;
   ofn.lpstrFile = File;
   ofn.nMaxFile = sizeof(File);
   ofn.lpstrFileTitle = FileTitle;
   ofn.nMaxFileTitle = sizeof (FileTitle);
   ofn.lpstrInitialDir = NULL;
   ofn.Flags = OFN_OVERWRITEPROMPT | OFN_HIDEREADONLY;
      
   /*==========================*/
   /* Perform the file dialog. */
   /*==========================*/
   
   if (! GetSaveFileName(&ofn))
     { return; }
        
   /*==========================*/
   /* Adjust Path Information. */
   /*==========================*/
   
   size = strlen(ofn.lpstrFile);
   for (x = 0; x < (int) size; x++)
     {
      if (ofn.lpstrFile[x] == '\\')
        { ofn.lpstrFile[x] = '/'; }
     }
     
   /*=================================*/
   /* Save the contents of the status */
   /* window to the file.             */
   /*=================================*/

   DisplayLineCountAndStart(hwnd,&lines,&displayStart);
  
   if ((fp = fopen(ofn.lpstrFile,"w")) == NULL)
     { return; }
     
   iLineNum = displayStart;
   for (i = 0; i < (unsigned) lines; i++)
     {
      if (theData->terminal[iLineNum] != NULL)
        { fprintf(fp,"%s",theData->terminal[iLineNum]); }
     
      fprintf(fp,"\n");
      
      iLineNum++;
      if (iLineNum > DIALOG_SIZE)
        { iLineNum = 0; }
     }

   fclose(fp);
  }
    
/*************************************/
/* UpdateCursor: Changes the cursor. */
/*************************************/
void UpdateCursor( 
  int theCursor)
  {     
   HCURSOR myCursor;
   POINT thePoint;

   GetCursorPos(&thePoint);
   ScreenToClient(MDIClientWnd,&thePoint);
 
   switch (theCursor)
     {  
      case ARROW_CURSOR:    
        myCursor = ARROW;
        break;
        
      case QUESTION_CURSOR:
        myCursor = QUERY;
        break;
        
      case WAIT_CURSOR:
        myCursor = WAIT[0];
        break;
     }
   
   SetClassLong(DialogWindow,GCL_HCURSOR,(LONG) myCursor);

   if (ChildWindowFromPointEx(MDIClientWnd,thePoint,CWP_SKIPINVISIBLE | CWP_SKIPDISABLED) == DialogWindow)
     { SetCursor(myCursor); }
  }

/*******************************************************/
/* StartWaitCursor: Function is called to set an event */ 
/*   timer used to update the animated wait cursor.    */
/*******************************************************/
void StartWaitCursor()
  {
   SetTimer(DialogWindow,IDT_UPDATEWAIT,500,(TIMERPROC) NULL);
  }

/********************************************************/
/* StopWaitCursor: Function is called to stop the event */ 
/*   timer used to update the animated wait cursor.     */
/********************************************************/
void StopWaitCursor()
  {
   KillTimer(DialogWindow,IDT_UPDATEWAIT);
   UpdateCursor(ARROW_CURSOR);
  }

/***********************************************************/
/* DisplayBeforeCommandExecution: This callback routine is */
/*   invoked by CLIPS from the command loop just before a  */
/*   command is executed. WinCLIPS uses this to prevent a  */
/*   command the user is currently editing from being      */
/*   executed as well as for maintaining a command         */
/*   history that can be invoked using the up/down arrow   */
/*   keys.                                                 */
/***********************************************************/
globle int DisplayBeforeCommandExecution(
  void *theEnv)
  {
   struct displayWindowData *theData;
   char *theCommand;
   size_t i, length, lastCR;
   struct priorCommand *nextCommand;

   /*======================================================*/
   /* Don't prevent any batch commands from being executed */
   /* (as the user shouldn't be able to move the editing   */
   /* caret inside a command during batch execution).      */
   /* Immediately returning also prevents batch commands   */
   /* from being added to the command history (which I     */
   /* think for most cases is the preferable behavior).    */
   /*======================================================*/
         
   if (BatchActive(theEnv))
     { return TRUE; }

   /*====================================================*/
   /* If the user has moved the caret into the center of */
   /* the command, don't execute the command as the user */
   /* may be temporarily adding/removing parentheses     */
   /* that cause a completed command to be formed.       */
   /*====================================================*/
   
   theData = (struct displayWindowData *) GetWindowLong(DialogWindow,GWL_USERDATA);
   
   if (theData->caretOffset != 0)
     { return FALSE; }
          
   /*=================================================*/
   /* Replace the first command with the contents of  */
   /* the command string, up to but not including the */ 
   /* last carriage return which initiated execution  */
   /* of the command. Removing the last carriage      */
   /* will prevent the command from being immediately */
   /* executed when the command is recalled by the    */
   /* up/down arrow keys (i.e. the user must hit the  */
   /* final carriage return again to execute the      */
   /* recalled command).                              */
   /*=================================================*/
      
   free(theData->topCommand->command);

   theCommand = GetCommandString(theEnv);
   length = strlen(theCommand);
   
   for (i = 0, lastCR = length; i < length; i++)
     {
      if (theCommand[i] == '\n')
        { lastCR = i; }
     }   

   theData->topCommand->command = (char *) malloc(lastCR + 1);
   strncpy(theData->topCommand->command,theCommand,lastCR);
   theData->topCommand->command[lastCR] = '\0';   

   /*====================================================*/
   /* If this command is identical to the prior command, */
   /* don't add it to the command history.               */
   /*====================================================*/
    
   if ((theData->topCommand->next != NULL) &&
       (strcmp(theData->topCommand->command,theData->topCommand->next->command) == 0))
     {
      free(theData->topCommand->command);
      theData->topCommand->command = (char *) malloc(1);
      theData->topCommand->command[0] = '\0';
      return TRUE;
     }

   /*=================================================*/
   /* Add a new empty command to the top of the stack */
   /* in preparation for the next user command.       */
   /*=================================================*/

   nextCommand = (struct priorCommand *) malloc(sizeof(struct priorCommand));
   nextCommand->next = theData->topCommand;
   nextCommand->prev = NULL;
   nextCommand->command = (char *) malloc(1);
   nextCommand->command[0] = '\0';
   
   theData->topCommand->prev = nextCommand;
   theData->topCommand = nextCommand;
   theData->currentCommand = nextCommand;
   
   theData->currentCommandCount++;   
   
   /*=============================================*/
   /* Remove commands at the end of the command   */
   /* history if the maximum number of remembered */
   /* commands is exceeded.                       */
   /*=============================================*/
   
   while (theData->currentCommandCount > theData->maxCommandCount)
     {
      if (theData->bottomCommand->prev == NULL)
        { break; }
        
      theData->bottomCommand->prev->next = NULL;
      nextCommand = theData->bottomCommand;
      theData->bottomCommand = theData->bottomCommand->prev;
      free(nextCommand->command);
      free(nextCommand);
      
      theData->currentCommandCount--;
     }
     
   /*==================================*/
   /* Return TRUE to indicate that the */
   /* command should be executed.      */
   /*==================================*/
     
   return TRUE;
  }
  
/***********************************************************/
/* SwitchCommand:                                        */
/***********************************************************/
static void SwitchCommand(
  HWND hwnd,
  void *theEnv,
  struct priorCommand *oldCommand,
  struct priorCommand *newCommand)
  {
   struct displayWindowData *theData;
   char *theCommand;
   size_t i, length;
   
   /*==================================*/
   /* Reset the caret information each */
   /* time a command is changed.       */
   /*==================================*/
   
   theData = (struct displayWindowData *) GetWindowLong(DialogWindow,GWL_USERDATA);
   
   theData->caretOffset = 0;
   theData->caretLinesBack = 0;
   theData->caretCharsBack = 0;
     
   /*=============================================*/
   /* Remove the current command from the window. */
   /*=============================================*/
        
   theCommand = GetCommandString(theEnv);
   if (theCommand == NULL)
     { theCommand = ""; }
     
   length = strlen(theCommand);
     
   for (i = 0; i < length; i++)
     { QuickBackSpace(hwnd,theData); }
           
   /*==============================================*/
   /* Replace the old command with the contents of */
   /* the command string, which will now include   */
   /* any edits the user made.                     */
   /*==============================================*/
   
   free(oldCommand->command);
   oldCommand->command = (char *) malloc(length + 1);
   strncpy(oldCommand->command,theCommand,length + 1);

   /*======================*/
   /* Use the new command. */
   /*======================*/
   
   SetCommandString(theEnv,newCommand->command);
   EnvPrintRouter(theEnv,"stdout",newCommand->command);
   theData->currentCommand = newCommand;
   
   /*=============================*/
   /* Force the window to update. */
   /*=============================*/
   
   InvalidateRect(hwnd,NULL,TRUE);	    
  }
  
/***********************************************************/
/* ClearCommandFromDisplay:                                */
/***********************************************************/
void ClearCommandFromDisplay(
  HWND hwnd,
  void *theEnv)
  {
   struct displayWindowData *theData;
   char *theCommand;
   size_t i, length;
   
   /*==================================*/
   /* Reset the caret information each */
   /* time a command is removed.       */
   /*==================================*/
   
   theData = (struct displayWindowData *) GetWindowLong(DialogWindow,GWL_USERDATA);
   
   theData->caretOffset = 0;
   theData->caretLinesBack = 0;
   theData->caretCharsBack = 0;
  
   /*=======================================*/
   /* Remove the selection from the window. */
   /*=======================================*/
   
   RemoveSelection(hwnd,theData);
      
   /*=============================================*/
   /* Remove the current command from the window. */
   /*=============================================*/
       
   theCommand = GetCommandString(theEnv);
   if (theCommand == NULL)
     { theCommand = ""; }
     
   length = strlen(theCommand);
     
   for (i = 0; i < length; i++)
     { QuickBackSpace(hwnd,theData); }
              
   /*=============================*/
   /* Force the window to update. */
   /*=============================*/
   
   InvalidateRect(hwnd,NULL,TRUE);	    
  }
  
/****************************************************/
/* DisplayPaste: Handles a paste operation in the   */
/*   command currently being entered at the prompt. */
/****************************************************/
static void DisplayPaste( 
  HWND hwnd,
  struct displayWindowData *theData,
  char *theString)
  {
   char *theCommand, *preCommand, *postCommand;
   void *theEnv;
   size_t length;
   int tempOffset, tempChars, tempLines;
   
   RemoveSelection(hwnd,theData);
   
   if (theData->caretOffset == 0)
     {  
      AppendCommandString(GetCurrentEnvironment(),theString);
      EnvPrintRouter(GetCurrentEnvironment(),WPROMPT,theString);
      return;
     }

   theEnv = GetCurrentEnvironment();
   theCommand = GetCommandString(theEnv);
   if (theCommand == NULL)
     { 
      AppendCommandString(GetCurrentEnvironment(),theString);
      EnvPrintRouter(GetCurrentEnvironment(),WPROMPT,theString);
      return; 
	 }

   /*================================================*/
   /* The command string needs to be split in two so */
   /* the clipboard can be inserted in the middle.   */
   /* Create the storage for the two strings.        */
   /*================================================*/
                                         
   length = strlen(theCommand);
   if (length == 0) return;
         
   postCommand = (char *) malloc(theData->caretOffset + 1);
   if (postCommand == NULL) return;
         
   preCommand = (char *) malloc((length - theData->caretOffset) + 1);
   if (preCommand == NULL)
     {
      free(postCommand); 
      return; 
     }
            
   /*================================================*/
   /* Store the current command string in two parts. */
   /*================================================*/
                  
   strncpy(postCommand,&theCommand[length - theData->caretOffset],theData->caretOffset+1);
   strncpy(preCommand,theCommand,length - theData->caretOffset);
   preCommand[length - theData->caretOffset] = '\0';
         
   /*========================================*/
   /* Create the new command string with the */
   /* character inserted at the caret.       */
   /*========================================*/
         
   SetCommandString(theEnv,preCommand);
   AppendCommandString(theEnv,theString);
   AppendCommandString(theEnv,postCommand);

   /*========================================*/
   /* Quickly delete the current user input. */
   /*========================================*/
         
   for (; length != 0; length--)
     { QuickBackSpace(hwnd,theData); }
           
   /*=============================*/
   /* Preserve the caret offsets. */
   /*=============================*/
         
   tempOffset = theData->caretOffset;
   tempChars = theData->caretCharsBack;
   tempLines = theData->caretLinesBack;
         
   EnvPrintRouter(GetCurrentEnvironment(),"stdout",preCommand);
   EnvPrintRouter(GetCurrentEnvironment(),"stdout",theString);
   EnvPrintRouter(GetCurrentEnvironment(),"stdout",postCommand);
         
   /*============================*/
   /* Restore the caret offsets. */
   /*============================*/
         
   theData->caretOffset = tempOffset;
   theData->caretCharsBack = tempChars;
   theData->caretLinesBack = tempLines;
                     
   InvalidateRect (hwnd,NULL,TRUE);	    
     
   /*============================================*/
   /* Release the memory used for the two parts. */
   /*============================================*/
         
   free(postCommand);
   free(preCommand);         
  }
  
/*******************************************************/
/* FlashBalancedParenthesis: Temporarily flashes the   */
/*    balancing left parenthesis for the right         */  
/*    parenthesis to the immediate left of the cursor. */
/*******************************************************/
static void FlashBalancedParenthesis(
  void *theEnv,
  struct displayWindowData *theData,
  int beep) /* Add hwnd argument */
  {
   POINT thePoint;
   char characterToCheck;
   unsigned short nestingDepth;
   unsigned int caretLocation, position;
   size_t commandLength;
   char *theCommand;
   DWORD sTime;
   int tempOffset, tempChars, tempLines, tempX, tempY;
   
   /*=======================================================*/
   /* Don't balance the parentheses if there is no command. */
   /*=======================================================*/
   
   theCommand = GetCommandString(theEnv);
   if (theCommand == NULL) return;
   
   commandLength = strlen(theCommand);
   if (commandLength <= 0) 
     { return; }

   /*=====================*/
   /* Where is the caret? */
   /*=====================*/
    
   caretLocation = theData->caretOffset;

   if (caretLocation == commandLength)
     { return; }
     
   position = commandLength - (caretLocation + 1);
   
   /*======================================================*/
   /* What is the character on the left side of the caret? */
   /*======================================================*/
    
   characterToCheck = theCommand[position];

   /*======================================*/
   /* We only balance a right parenthesis. */
   /*======================================*/
   
   if (characterToCheck != ')') return;

   /*======================================================================*/
   /* The nesting depth will start at zero. Each time a ')' is encountered */
   /* the nesting depth is incremented by one and each time a '(' is       */
   /* encountered the nesting depth is decremented by one. If a '(' is     */
   /* encountered when the nesting depth is zero (the starting value), the */
   /* matching parenthesis has been found.                                 */
   /*======================================================================*/
   
   nestingDepth = 0;

   /*=============================*/
   /* Preserve the caret offsets. */
   /*=============================*/
         
   tempOffset = theData->caretOffset;
   tempChars = theData->caretCharsBack;
   tempLines = theData->caretLinesBack; 
            
   /*================================================*/
   /* We've already considered the last parenthesis. */
   /*================================================*/
   
   theData->caretCharsBack++;
  
   /*==================================================*/
   /* Start looking for the matching left parenthesis. */
   /*==================================================*/
      
   while (position--) 
     {
      characterToCheck = theCommand[position];
      
      if (characterToCheck =='\n')
        {
         theData->caretLinesBack++;
         theData->caretCharsBack = 0;
        }
      else
        { theData->caretCharsBack++; }
        
      if (characterToCheck == '(') 
        {
         if (nestingDepth == 0) 
           { 
            GetCaretPos(&thePoint);
            tempX = ComputeCaretX(DialogWindow,theData);
            tempY = thePoint.y - (theData->lineSize * (theData->caretLinesBack - tempLines));
            SetCaretPos(tempX,tempY);
  
            /*=================================*/
            /* Pause briefly with the matching */
            /* left parenthesis highlighted.   */
            /*=================================*/
            
            sTime = GetTickCount();
            
            while ((GetTickCount() - sTime) < 120) /* .12 second delay */
              { /* Wait */ }
                    
            /*============================*/
            /* Restore the caret offsets. */
            /*============================*/
         
            theData->caretOffset = tempOffset;
            theData->caretCharsBack = tempChars;
            theData->caretLinesBack = tempLines;
            
            /*===============================================*/
            /* Place the caret back at its current position. */
            /*===============================================*/
            
            SetCaretPos(thePoint.x,thePoint.y);
		    
		    return;
		   }
         else
		   { nestingDepth--; }
	    }
      else if (characterToCheck == ')') 
        { nestingDepth++; }
     }

   /*================================================*/
   /* Beep to indicate a matching ')' was not found. */
   /*================================================*/
   
   if (beep)
     { MessageBeep(0); }
   
   /*============================*/
   /* Restore the caret offsets. */
   /*============================*/
         
   theData->caretOffset = tempOffset;
   theData->caretCharsBack = tempChars;
   theData->caretLinesBack = tempLines;
  }
  
/************************************************************/
/* FindInsertionPoint: Given the mouse X and Y locations,   */
/*   this routine finds the closest insertion point         */
/*   to the point (given as a line and character position). */
/*   The point passed to this routine should be in local    */
/*   coordinates.                                           */
/************************************************************/
static void FindInsertionPoint(
  HWND hwnd,
  int mouseX,
  int mouseY,
  int *theLine,
  int *theCharacter)
  {
   int max, min, topDisplayNumber, linesInUse, leftMarginNumber;
   struct displayWindowData *theData;
 
   /*===========================*/
   /* Retrieve the window data. */
   /*===========================*/
   
   theData = (struct displayWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) 
     { return; }
     
   /*===============================================*/
   /* Get current value of the vertical scroll bar. */
   /* This corresponds to the first line that will  */
   /* be displayed in the window.                   */
   /*===============================================*/
   
   GetScrollRange(hwnd,SB_VERT,&min,&max);
   if (min != max)
     {
      linesInUse = (max - min) + theData->linesInDisplayWindow + 1;
      topDisplayNumber = GetScrollPos(hwnd,SB_VERT) - theData->linesInDisplayWindow;
     }
   else
     {
      linesInUse = theData->lastLine + 1;
      topDisplayNumber = 0;
     }

   /*=================================================================*/
   /* The line the cursor is on can be found by dividing the vertical */
   /* distance from the top of the buffer by the height of each line. */
   /* Then the number of lines above the top line in the window is    */
   /* added to this total.                                            */
   /*=================================================================*/

   *theLine = (mouseY / theData->lineSize) + topDisplayNumber;

   /*===============================================================*/
   /* The character insertion point can be found by subtracting the */
   /* left window margin and the number of pixels that have been    */
   /* scrolled off to the left from the horizontal distance to      */
   /* the left side of the buffer. This result is then divided by   */
   /* the width of a character to determine which character the     */
   /* insertion point falls before. Note this calculation assumes   */
   /* that all characters have the same width so it will only work  */
   /* for a font such as monaco or courier.                         */
   /*===============================================================*/

   leftMarginNumber = GetScrollPos(hwnd,SB_HORZ);
   
   *theCharacter = (mouseX -
                    LEFT_MARGIN_SPACE +
                    (leftMarginNumber * theData->maxCharWidth)) /
                   theData->maxCharWidth;

   /*================================================================*/
   /* If the cursor falls more than halfway across a character, then */
   /* the insertion point is after the character, not before it.     */
   /*================================================================*/

   if (mouseX >= ((*theCharacter * theData->maxCharWidth) +
                       LEFT_MARGIN_SPACE -
                       (leftMarginNumber * theData->maxCharWidth) +
                       (theData->maxCharWidth / 2)))
     { (*theCharacter)++; }

   if (*theCharacter < 0) *theCharacter = 0;

   if (*theLine < 0)
     { 
      *theLine = 0;
      *theCharacter = 0;
     } 
   else if (*theLine > (linesInUse - 1))
     {
      *theLine = linesInUse - 1;
      *theCharacter = strlen(theData->terminal[theData->lastLine]);
     }
   else
     {
      int lineOfInterest, lineLength;
      char *theStr;

      lineOfInterest = theData->lastLine - (linesInUse - ((*theLine) + 1));
      if (lineOfInterest < 0)
        { lineOfInterest += (DIALOG_SIZE + 1); }
      
      theStr = theData->terminal[lineOfInterest];
      lineLength = strlen(theStr);
      
      if (*theCharacter > lineLength)
        { *theCharacter = lineLength; }
     }
  }
  
/*****************************************************************/
/* DisplaySelectionRegion                                        */
/*****************************************************************/
static HRGN DisplaySelectionRegion(
  HWND hwnd,
  struct displayWindowData *theData)
  {
   RECT theRect, tempRect;
   int numberOfLines, topDisplayNumber, leftMarginNumber;
   HRGN theRegion = CreateRectRgn(0,0,0,0), tempRegion;
   int swap, tc, tl;
   int min, max;

   /*=========================================*/
   /* If the selection is not being extended, */
   /* then nothing needs to be done.          */
   /*=========================================*/

   if ((theData->selectionLineStart == theData->selectionLineEnd) &&
       (theData->selectionCharStart == theData->selectionCharEnd))
     { return(theRegion); }

   /*====================================================*/
   /* Swap the start and end positions if the end occurs */
   /* before the start. The start is the position at     */
   /* which the mouse was initially clicked and the end  */
   /* is the current position of the mouse.              */
   /*====================================================*/

   if ((theData->selectionLineStart > theData->selectionLineEnd) ||
       ((theData->selectionLineStart == theData->selectionLineEnd) &&
        (theData->selectionCharStart > theData->selectionCharEnd)))
     {
      tl = theData->selectionLineStart;
      tc = theData->selectionCharStart;
      theData->selectionLineStart = theData->selectionLineEnd;
      theData->selectionCharStart = theData->selectionCharEnd;
      theData->selectionLineEnd = tl;
      theData->selectionCharEnd = tc;
      swap = TRUE;
     }
   else
     { swap = FALSE; }

   /*=================================================*/
   /* Handle the first line of the current selection. */
   /*=================================================*/
   
   /* GetWindowPortBounds(whichWindow,&tempRect); */
   GetClientRect(hwnd,&tempRect); 
   
   GetScrollRange(hwnd,SB_VERT,&min,&max);
   if (min != max)
     { topDisplayNumber = GetScrollPos(hwnd,SB_VERT) - theData->linesInDisplayWindow; }
   else
     { topDisplayNumber = 0; }
   
   leftMarginNumber = GetScrollPos(hwnd,SB_HORZ);
  
   theRect.left = (short) (tempRect.left + LEFT_MARGIN_SPACE +
                  (theData->selectionCharStart * theData->maxCharWidth) -
                  (leftMarginNumber * theData->maxCharWidth));
   if (theData->selectionCharStart == 0) theRect.left -= LEFT_MARGIN_SPACE;
   if (theData->selectionLineStart == theData->selectionLineEnd)
     {
      theRect.right = (short) (tempRect.left + LEFT_MARGIN_SPACE +
                      (theData->selectionCharEnd * theData->maxCharWidth) -
                      (leftMarginNumber * theData->maxCharWidth));
     }
   else
     { 
      /* theRect.right = (short) (tempRect.right - (SCROLL_BAR_WIDTH - 1)); */
      theRect.right = (short) tempRect.right; 
     }
   
   theRect.top = (short) ((theData->selectionLineStart - topDisplayNumber) * theData->lineSize);
   theRect.bottom = (short) (theRect.top + theData->lineSize);
   /* CropScrollBarSpillage(&theRect,whichWindow); */
   
   tempRegion = CreateRectRgn(theRect.left,theRect.top,theRect.right,theRect.bottom);
   CombineRgn(theRegion,tempRegion,theRegion,RGN_OR);

   /*==================================*/
   /* Handle the lines between the top */
   /* and bottom lines selected.       */
   /*==================================*/

   numberOfLines = (theData->selectionLineEnd - theData->selectionLineStart) + 1;
   if (numberOfLines > 2)
     {
      theRect.left = tempRect.left;
      /* theRect.right = (short) (tempRect.right - (SCROLL_BAR_WIDTH - 1)); */
      theRect.right = (short) tempRect.right;
      theRect.top = (short) (((theData->selectionLineStart + 1) - topDisplayNumber) * theData->lineSize);
      theRect.bottom = (short) (theRect.top + (theData->lineSize * (numberOfLines - 2)));
      /* CropScrollBarSpillage(&theRect,whichWindow); */
      DeleteObject(tempRegion);
      tempRegion = CreateRectRgn(theRect.left,theRect.top,theRect.right,theRect.bottom);
      CombineRgn(theRegion,tempRegion,theRegion,RGN_OR);
     }

   /*=======================*/
   /* Handle the last line. */
   /*=======================*/

   if ((theData->selectionCharEnd != 0) && (numberOfLines > 1))
     {
      theRect.left = tempRect.left;
      theRect.right = (short) (tempRect.left + LEFT_MARGIN_SPACE +
                      (theData->selectionCharEnd * theData->maxCharWidth) -
                      (leftMarginNumber * theData->maxCharWidth));
      theRect.top = (short) ((theData->selectionLineEnd - topDisplayNumber) * theData->lineSize);
      theRect.bottom = (short) (theRect.top + theData->lineSize);
      /* CropScrollBarSpillage(&theRect,whichWindow); */
      DeleteObject(tempRegion);
      tempRegion = CreateRectRgn(theRect.left,theRect.top,theRect.right,theRect.bottom);
      CombineRgn(theRegion,tempRegion,theRegion,RGN_OR);
     }

   /*=========================================================*/
   /* Subtract out the whitespace at the bottom of the window */
   /* that doesn't include any of the displayed lines.        */
   /*=========================================================*/
   
   GetClientRect(hwnd,&theRect);
   theRect.top = (theData->linesInDisplayWindow + 1) * theData->lineSize;
   DeleteObject(tempRegion);
   tempRegion = CreateRectRgn(theRect.left,theRect.top,theRect.right,theRect.bottom);
   CombineRgn(theRegion,theRegion,tempRegion,RGN_DIFF);

   /*===================================*/
   /* Swap positions back if necessary. */
   /*===================================*/

   if (swap)
     {
      tl = theData->selectionLineStart;
      tc = theData->selectionCharStart;
      theData->selectionLineStart = theData->selectionLineEnd;
      theData->selectionCharStart = theData->selectionCharEnd;
      theData->selectionLineEnd = tl;
      theData->selectionCharEnd = tc;
     }

   /*=============================================*/
   /* Return the region of the current selection. */
   /*=============================================*/

   DeleteObject(tempRegion);
   return(theRegion);
  }
  
/*****************************************************************/
/* RemoveSelection                                        */
/*****************************************************************/
static void RemoveSelection(
  HWND hwnd,
  struct displayWindowData *theData)
  {
   if ((theData->selectionLineStart == theData->selectionLineEnd) &&
       (theData->selectionCharStart == theData->selectionCharEnd))
     { return; }
     
   InvertSelection(hwnd,theData);
   
   theData->selectionLineStart = 0;
   theData->selectionCharStart = 0;
   theData->selectionLineEnd = 0;
   theData->selectionCharEnd = 0;
   
   PostMessage(hwnd,UWM_UPDATE_MENU,0,0);
  }

/***********************************************/
/* DisplaySelectionText: Returns a copy of the */
/*   selected text in the display window.      */
/***********************************************/
char *DisplaySelectionText(
  HWND hwnd,
  struct displayWindowData *theData,
  size_t *length)
  {
   /* struct lineRecord *linePtr; */
   char *linePtr;
   char *textPtr, *tempPtr;
   long tempLength, addLength;
   int i, numberOfLines, firstLine;
   int lineStart, charStart, lineEnd, charEnd;
   
   /*=====================================================*/
   /* If nothing is selected, just return a NULL pointer. */
   /*=====================================================*/
   
   if ((theData->selectionLineStart == theData->selectionLineEnd) &&
       (theData->selectionCharStart == theData->selectionCharEnd))
     {
      *length = 0;
      return(NULL);
     }

   /*===================================================*/
   /* Swap the selection start point with the end point */
   /* if the end point is before the start point.       */
   /*===================================================*/
   
   if ((theData->selectionLineStart < theData->selectionLineEnd) ||
       ((theData->selectionLineStart == theData->selectionLineEnd) &&
        (theData->selectionCharStart < theData->selectionCharEnd)))
     {
      lineStart = theData->selectionLineStart;
      charStart = theData->selectionCharStart;
      lineEnd = theData->selectionLineEnd;
      charEnd = theData->selectionCharEnd;
     }
   else
     {
      lineStart = theData->selectionLineEnd;
      charStart = theData->selectionCharEnd;
      lineEnd = theData->selectionLineStart;
      charEnd = theData->selectionCharStart;
     }

   /*============================================*/
   /* Determine which line in the terminal array */
   /* is the first line of the displayed output. */
   /*============================================*/
   
   DisplayLineCountAndStart(hwnd,&numberOfLines,&firstLine); 

   /*====================================*/
   /* Handle the case where the selected */
   /* text is all on the same line.      */
   /*====================================*/
   
   if (lineStart == lineEnd)
     {
      *length = (size_t) (charEnd - charStart);
      textPtr = (char *) malloc((size_t) *length + 1);


      tempPtr = theData->terminal[(firstLine + lineStart) % (DIALOG_SIZE + 1)];
      tempPtr = &tempPtr[charStart];

      strncpy(textPtr,tempPtr,*length);
      textPtr[*length] = '\0';
      return(textPtr);
     }

   /*=========================================*/
   /* Count the number of characters selected */
   /* in the first line of the selection.     */
   /*=========================================*/
   
   linePtr = theData->terminal[(firstLine + lineStart) % (DIALOG_SIZE + 1)];
   tempLength = (strlen(linePtr) - charStart) + 2;

   /*============================================*/
   /* Count the number of characters selected in */
   /* the lines between the first selected line  */
   /* and the last selected line.                */
   /*============================================*/

   for (i = lineStart + 1; i < lineEnd; i++)
     {
      linePtr = theData->terminal[(firstLine + i) % (DIALOG_SIZE + 1)];
      tempLength += strlen(linePtr) + 2;
     }

   /*==================================*/
   /* Count characters selected in the */
   /* last line of the selection.      */
   /*==================================*/

   tempLength += charEnd;

   /*================================================*/
   /* Allocate space for storing the selection text. */
   /*================================================*/

   textPtr = (char *) malloc(tempLength + 1);
   if (textPtr == NULL)
     {
      *length = 0;
      return(NULL);
     }

   /*=======================================*/
   /* Copy the first line of selected text. */
   /*=======================================*/

   tempLength = 0;

   linePtr = theData->terminal[(firstLine + lineStart) % (DIALOG_SIZE + 1)];
   addLength = strlen(linePtr) - charStart;

   strncpy(&textPtr[tempLength],&(linePtr[charStart]),(unsigned long) addLength);

   tempLength += addLength;
   textPtr[tempLength++] = '\r';
   textPtr[tempLength++] = '\n';

   /*====================================*/
   /* Copy the selected text between the */
   /* first and last selected lines.     */
   /*====================================*/

   for (i = lineStart + 1; i < lineEnd; i++)
     {
      linePtr = theData->terminal[(firstLine + i) % (DIALOG_SIZE + 1)];
      addLength = strlen(linePtr);
      strncpy(&textPtr[tempLength],linePtr,(unsigned long) addLength);
      tempLength += addLength;
      textPtr[tempLength++] = '\r';
      textPtr[tempLength++] = '\n';
     }

   /*======================================*/
   /* Copy the last line of selected text. */
   /*======================================*/

   linePtr = theData->terminal[(firstLine + lineEnd) % (DIALOG_SIZE + 1)];
   
   strncpy(&textPtr[tempLength],linePtr,(unsigned long) charEnd);
   tempLength += charEnd;

   textPtr[tempLength++] = '\0';
   *length = (unsigned long) tempLength;
  
   return(textPtr);
  }
  
/***********************************************/
/* ScrollTimerProc:      */
/***********************************************/
#if WIN_BTC
#pragma argsused
#endif
static VOID CALLBACK ScrollTimerProc(
  HWND hwnd,
  UINT msg,
  UINT_PTR id,
  DWORD ticks)
  {
#if WIN_MCW
#pragma unused(hwnd)
#pragma unused(msg)
#pragma unused(id)
#pragma unused(ticks)
#endif
   PostMessage(hwnd,UWM_SCROLL,0,0);
  }
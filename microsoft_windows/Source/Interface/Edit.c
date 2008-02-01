   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*       Microsoft Windows Version 3.0  01/31/02       */
   /*                                                     */
   /*                     EDIT MODULE                     */
   /*******************************************************/

/**************************************************************/
/* Purpose: Contains the callback functions for most of the   */
/*       simple dialogs from the main menu.                   */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Christopher J. Ortiz                                  */
/*                                                            */
/* Contributing Programmer(s):                                */
/*      Gary D. Riley                                         */
/*                                                            */
/* Revision History:                                          */
/*                                                            */
/**************************************************************/

#define _EDIT_SOURCE_

#include "StdSDK.h"     // Standard application includes
#include <tchar.h>
#include <windowsx.h>

#include "setup.h"

#include "mdi.h"
#include "edit.h"
#include "resource.h"
#include "EditUtil.h"
#include "SearchDialog.h"

#define setProc(hwnd, proc) SetWindowLong(hwnd, GWL_USERDATA, (LPARAM)proc)
#if WIN_BTC
#define getProc(hwnd)       (FARPROC)GetWindowLong(hwnd, GWL_USERDATA)
#else
#define getProc(hwnd)       (WNDPROC)GetWindowLong(hwnd, GWL_USERDATA)
#endif

#define getRGB(hwnd)        (COLORREF)GetProp(hwnd, MAKEINTATOM(ATOM_TEXTCOLOR))
#define setRGB(hwnd, rgb)   SetProp(hwnd, MAKEINTATOM(ATOM_TEXTCOLOR), (HANDLE)rgb)

//=============================================================================
// These atoms are used to attach properties to the edit window
#define ATOM_LASTSEL   0x100 // good random number
#define ATOM_BKG       0x101 // different random number
#define ATOM_TEXTCOLOR 0x102

/************************************************/
/* editsubclass_OnCtlColor: Sets the background */
/*   brush, text color, and background color.   */
/************************************************/
#if WIN_BTC
#pragma argsused
#endif
static HBRUSH editsubclass_OnCtlColor(
  HWND hwnd, 
  HDC hdc, 
  HWND hchild, 
  int type)
  {
#if WIN_MCW
#pragma unused(hchild)
#endif
   HBRUSH EditBackgroundBrush = (HBRUSH) GetProp(hwnd, MAKEINTATOM(ATOM_BKG));
   LOGBRUSH lbr;

   if (type != CTLCOLOR_EDIT)
     { return NULL; }

   if (EditBackgroundBrush != NULL)
     { GetObject(EditBackgroundBrush, sizeof(lbr), &lbr); }

   SetTextColor(hdc, getRGB(hwnd));
   SetBkColor(hdc, lbr.lbColor);
   return EditBackgroundBrush;
  }

/*******************************************/
/* editsubclass_OnSetSel: If the selection */
/*   changed, update the menus.            */
/*******************************************/
static LRESULT editsubclass_OnSetSel(
  HWND hwnd, 
  UINT message, 
  WPARAM wParam, 
  LPARAM lParam)
  {
   LRESULT result;

   PostMessage(GetParent(hwnd), UWM_UPDATE_MENU, 0, 0);
   result = CallWindowProc(getProc(hwnd), hwnd, message, wParam, lParam);
   SetProp(hwnd, MAKEINTATOM(ATOM_LASTSEL), 
                             (HANDLE)Edit_GetSel(hwnd));
   return result;
  }

/***************************************/
/* editsubclass_CheckSelChange: Checks */
/*   to see if the selection changed.  */
/***************************************/
static void editsubclass_CheckSelChange(
  HWND hwnd)
  {
   DWORD lastsel;
   DWORD newsel;

   // check to see if we changed the selection
   newsel   = Edit_GetSel(hwnd);
   lastsel  = (DWORD)GetProp(hwnd, MAKEINTATOM(ATOM_LASTSEL));
   
   if (lastsel != newsel)
     {
      SetProp(hwnd, MAKEINTATOM(ATOM_LASTSEL), (HANDLE)newsel);
      PostMessage(GetParent(hwnd), UWM_UPDATE_MENU, 0, 0);
     }
  }

/****************************************************************************
*                         editsubclass_OnRButtonUp
* Inputs:
*       HWND hwnd: Window handle
*    BOOL dblclk: FALSE, always
*    int x: Mouse position
*    int y: Mouse position
*    UINT keyflags: Key status flags
* Result: void
*       
* Effect: 
*       Forces the button-down message to the parent
****************************************************************************/
static void editsubclass_OnRButtonUp(
  HWND hwnd, 
  int x, 
  int y, 
  UINT keyflags)
  {
   FORWARD_WM_RBUTTONUP(GetParent(hwnd), x, y, keyflags, SendMessage);
  }

/****************************************************************************
*                           editsubclass_OnDestroy
* Inputs:
*       HWND hwnd: Window handle
* Result: void
*       
* Effect: 
*       Deletes the brush attached to the edit control and the brush property
*    from the window.  Deletes the selection property from the window.
****************************************************************************/
static void editsubclass_OnDestroy(
  HWND hwnd)
  {
   HBRUSH br;

   br = (HBRUSH) GetProp(hwnd, MAKEINTATOM(ATOM_BKG));
   if (br != NULL)
     { DeleteBrush(br); }

   RemoveProp(hwnd, MAKEINTATOM(ATOM_BKG));
   RemoveProp(hwnd, MAKEINTATOM(ATOM_LASTSEL));
  }

/****************************************************************************
*                              editSubclassProc
* Inputs:
*       HWND hwnd: Edit control
*       int message: Message ID
*       WPARAM wParam:
*       LPARAM lParam:
* Result: LRESULT
*       Per-message defined
* Effect: 
*       Intercepts EM_SETSEL messages and forces a recomputation of the
*       menu state
****************************************************************************/
static LRESULT CALLBACK editSubclassProc(
  HWND hwnd, 
  UINT message, 
  WPARAM wParam, 
  LPARAM lParam)
  {
   if (message == uFindReplaceMsg)
     { return StartSearch(hwnd,wParam,lParam); }

   switch(message)
     { /* message */
      HANDLE_MSG(hwnd, WM_RBUTTONUP, editsubclass_OnRButtonUp);
      HANDLE_MSG(hwnd, WM_DESTROY, editsubclass_OnDestroy);
      HANDLE_MSG(hwnd, WM_CTLCOLOREDIT, editsubclass_OnCtlColor);

      case EM_SETSEL:
        return editsubclass_OnSetSel(hwnd, message, wParam, lParam);

      // We intercept these messages, any one of which could have caused
      // a change in the selection status.
      case WM_KEYUP:
      case WM_SYSKEYUP:
      case WM_LBUTTONUP:
        editsubclass_CheckSelChange(hwnd);
        break;
     } /* message */

   return CallWindowProc(getProc(hwnd), hwnd, message, wParam, lParam);
  }

/****************************************************/
/* edit_New: Creates and subclasses an edit window. */
/****************************************************/
HWND edit_New(
  HWND parent, 
  DWORD styles, 
  BOOL subclassing)
  {
   RECT r;
   HWND hedit;
   LOGFONT lf;
   HFONT hfont;

   GetClientRect(parent, &r);
     
   hedit = CreateWindow(_T("EDIT"), NULL,
                   styles,
              r.left, r.top,
              r.right - r.left,
              r.bottom - r.top,
              parent,
              (HMENU) ID_EDIT_CONTROL,
              GetWindowInstance(parent),
              (LPVOID)NULL);

   if (hedit != NULL)
     {
      SetFocus(hedit);

      if (subclassing)
        { setProc(hedit, SubclassWindow(hedit, editSubclassProc)); }

      SetProp(hedit, MAKEINTATOM(ATOM_BKG), 
               CreateSolidBrush(GetSysColor(COLOR_WINDOW)));
               
      if (styles & ES_READONLY)
        { 
         TCHAR rodata[256];
         LoadString(GetWindowInstance(parent), IDS_RO_TEXT,
                    rodata, DIM(rodata));
         SetWindowText(hedit, rodata);
        } 
        
      /*===================================*/
      /* Set the font to courier 10 point. */
      /*===================================*/

      memset(&lf,0,sizeof(LOGFONT));
      lf.lfHeight = -13;
      strcpy(lf.lfFaceName,"Courier New");
      hfont = CreateFontIndirect(&lf);
      SetWindowFont(hedit,hfont,TRUE);
     } 

   return hedit;
  }

/*************************************/
/* edit_ChooseFont: Changes the font */
/*   and font color of the window.   */
/*************************************/
void edit_ChooseFont(
  HWND hwnd)
  {
   LOGFONT lf;
   HFONT hf;
   CHOOSEFONT cf;
     
   hf = GetWindowFont(hwnd);
     
   cf.lStructSize = sizeof(cf);
   cf.hwndOwner = hwnd;
   cf.lpLogFont = &lf;
   cf.Flags = CF_NOVECTORFONTS | CF_SCREENFONTS | CF_EFFECTS;
   cf.rgbColors = getRGB(hwnd);

   if (hf != NULL)
     {
      GetObject(hf, sizeof(lf), &lf);
      cf.Flags |= CF_INITTOLOGFONTSTRUCT;
     } 

   if (ChooseFont(&cf) != 0)
     { 
      HFONT nf = CreateFontIndirect(&lf);
         
      if (nf != NULL)
        {
         DeleteFont(hf);
         SetWindowFont(hwnd,nf,TRUE);         
         setRGB(hwnd,cf.rgbColors);
        }
     } 
  }

/*************************************/
/* edit_UpdateMenu: Updates the menu */
/*   based on the edit state.        */
/*************************************/
void edit_UpdateMenu(
  HWND hedit,
  HMENU hmenu)
  {
   int first;
   int last;
   BOOL hasSelection;

   SendMessage(hedit,EM_GETSEL,(WPARAM) &first,(LPARAM) &last);
   hasSelection = (first != last);

   EnableMenuItem(hmenu,ID_EDIT_CUT,(unsigned)
                  MF_BYCOMMAND | (!(GetWindowStyle(hedit) & ES_READONLY) && hasSelection 
                    ? MF_ENABLED : MF_GRAYED));
   EnableMenuItem(hmenu, ID_EDIT_COPY,(unsigned)
                    MF_BYCOMMAND | (hasSelection ? MF_ENABLED : MF_GRAYED));
   EnableMenuItem(hmenu, ID_EDIT_PASTE,(unsigned)
                  MF_BYCOMMAND | 
                  (IsClipboardFormatAvailable(CF_TEXT) ? MF_ENABLED : MF_GRAYED));
   EnableMenuItem(hmenu, ID_EDIT_UNDO,(unsigned)
                  MF_BYCOMMAND | (Edit_CanUndo(hedit) ? MF_ENABLED : MF_GRAYED));
   EnableMenuItem(hmenu,ID_EDIT_SELECT_ALL,(unsigned)
                  (Edit_GetTextLength(hedit) > 0 ? MF_ENABLED : MF_GRAYED));
   EnableMenuItem(hmenu,ID_EDIT_CLEAR,(unsigned)
                  MF_BYCOMMAND | (hasSelection ? MF_ENABLED : MF_GRAYED));
                    
   EnableMenuItem(hmenu,ID_EDIT_SET_FONT,MF_ENABLED );
   EnableMenuItem(hmenu,ID_EDIT_BALANCE,MF_ENABLED );
   EnableMenuItem(hmenu,ID_EDIT_COMMENT,MF_ENABLED );
   EnableMenuItem(hmenu,ID_EDIT_UNCOMMENT,MF_ENABLED );
   
   if (! SearchActive)
     {
      EnableMenuItem(hmenu,ID_BUFFER_FIND,MF_ENABLED);
      EnableMenuItem(hmenu,ID_BUFFER_REPLACE,MF_ENABLED);
     }
     
   EnableMenuItem(hmenu,ID_BUFFER_LOAD_BUFFER,MF_ENABLED);
   if (hasSelection)
     {
      EnableMenuItem(hmenu,ID_BUFFER_LOAD,MF_ENABLED);
      EnableMenuItem(hmenu,ID_BUFFER_BATCH,MF_ENABLED);
     }
  }

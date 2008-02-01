   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*       Microsoft Windows Version 3.0  01/31/02       */
   /*                                                     */
   /*                    FRAME MODULE                     */
   /*******************************************************/

/**************************************************************/
/* Purpose: Callback and support for all status windows.      */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Gary D. Riley                                         */
/*                                                            */
/* Contributing Programmer(s):                                */
/*      Ernst Bokkelkamp                                      */
/*                                                            */
/* Revision History:                                          */
/*      6.24: Ernst's changes to remember window positions.   */
/*                                                            */
/**************************************************************/


#define _FRAME_SOURCE_

#include "StdSDK.h"             // Standard application includes
#include <tchar.h>
#include "MainFrameToolbar.h"   // For non-static function prototypes
#include "resource.h"           // For resource identifiers
#include "mdi.h"
#include "text.h"
#include "edit.h" 
#include "clipsdde.h"
#include "EditUtil.h"
#include "SearchDialog.h"
#include "Text.h"

#include "Initialization.h"
#include "menucmds.h"
#include "menu.h"
#include "display.h"
#include "dialog1.h"
#include "dialog2.h"
#include "status.h"
#include "setup.h"

#include "agenda.h"
#include "commline.h"
#include "router.h"

#include "Frame.h"

#include "Registry.h" /* EB */

HWND MDIClientWnd;
HWND hMainFrame;
HMENU hMainMenu; // main menu for child-free MDI app
HACCEL hMainAccel; // accelerators for child-free MDI app

HACCEL haccel;   // Current accelerator table in use
BOOL view_status_line = TRUE;

//int                     ShiftKeyDown;

//
// Function prototypes for static functions
//

static BOOL mainframe_DisplayContextMenu (HWND hwnd, POINT pt) ;

int AvailableFrameHeight(void);

//
// Function prototypes for callback functions
//

BOOL    CALLBACK mainframe_SysColorChangeEnumeration (HWND hwnd, 
                              LPARAM lParam) ;
LRESULT CALLBACK mainFrameWndProc (HWND hwnd, UINT message, 
                   WPARAM wParam, LPARAM lParam) ;


//
// Function prototypes for message handlers
//

static void mainframe_OnActivateApp(HWND hwnd, BOOL activate, DWORD threadid);
static void mainframe_OnCommand (HWND hwnd, int id, HWND hwndCtl, 
                 UINT codeNotify) ;
static BOOL mainframe_OnContextMenu (HWND hwnd, HWND hwndCtl, 
                     int xPos, int yPos) ;
static BOOL mainframe_OnCreate (HWND hwnd, LPCREATESTRUCT lpCreateStruct) ;
static void mainframe_OnDestroy (HWND hwnd) ;
static void mainframe_OnDisplayChange (HWND hwnd, UINT cBitsPerPixel, 
                       UINT cxScreen, UINT cyScreen) ;
static void mainframe_OnFileClose(HWND hwnd);
static void mainframe_OnInitMenuPopup (HWND hwnd, HMENU hmenu, UINT item, BOOL sysmenu);
static void mainframe_OnMenuSelect(HWND hwnd,HMENU hmenu,int item,HMENU hmenuPopup,UINT flags);
static void mainframe_OnNCRButtonUp (HWND hwnd, int x, int y, UINT codeHitTest) ;
static BOOL mainframe_OnNotify (HWND hwnd, int idFrom, NMHDR FAR* pnmhdr) ;
static void mainframe_OnPaint (HWND hwnd) ;
static BOOL mainframe_OnQueryEndSession(HWND hwnd);
static void mainframe_OnRButtonDown (HWND hwnd, BOOL fDoubleClick, 
                     int x, int y, UINT keyFlags) ;
static HACCEL mainframe_OnSetAccel(HWND hwnd, HACCEL haccel);
static void mainframe_OnSettingChange (HWND hwnd, UINT uiFlag, 
                       LPCTSTR pszMetrics) ;
static void mainframe_OnSize (HWND hwnd, UINT state, int cx, int cy) ;
static void mainframe_OnSysColorChange(HWND hwnd) ;
static void mainframe_OnUserChanged (HWND hwnd) ;
static LRESULT mainframe_OnMDIDestroy(HWND hwnd);
static void mainframe_OnUpdateToolbar(HWND hwnd);
static void mainframe_OnViewStatus(HWND hwnd);
static void mainframe_OnClose(HWND);
static BOOL mainframe_OnSetCursor(HWND hwnd, WPARAM wParam, LPARAM lParam);
static void mainframe_OnMove(HWND,int,int);

  
static LRESULT sendToActiveMDI(HWND mdiclient, UINT message, 
                   WPARAM wParam, LPARAM lParam);
            
LRESULT fwdFrameProc(
  HWND hwnd, 
  UINT message, 
  WPARAM wParam, 
  LPARAM lParam);
//
// Typedefs
//


//
// Function prototypes for callback functions
//

BOOL    CALLBACK mainframe_SysColorChangeNotification (HWND hwnd, 
                               LPARAM lParam) ;
LRESULT CALLBACK mainFrameWndProc (HWND hwnd, UINT message, 
                   WPARAM wParam, LPARAM lParam) ;
static BOOL CALLBACK QuitEnumProc(HWND,LPARAM);

/***************/
/* DEFINITIONS */
/***************/

#define IDC_CANCEL                2
#define IDC_OK                    1

/***************************************************************/
/* fwdFrameProc: Forwards to DefFrameProc. This is a necessary */
/*   level of indirection so that the FORWARD_ macros (which   */
/*   expect a canonical parameter set) can call this function  */
/*   and have it pass the message on to the DefFrameProc       */
/*   function which takes one additional parameter.            */
/***************************************************************/
LRESULT fwdFrameProc(
  HWND hwnd, 
  UINT message, 
  WPARAM wParam, 
  LPARAM lParam)
  {
   return DefFrameProc(hwnd,MDIClientWnd,message,wParam,lParam);
  }



//
//  LRESULT CALLBACK
//  mainFrameWndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
//
//  hwnd            Handle of window to which this message applies
//  message         Message number
//  wParam          Message parameter
//  lParam          Message parameter
//
//  PURPOSE:  Processes messages for the main window.
//
//  MESSAGES:
//
//      WM_COMMAND          - notification from the menu or controls
//  WM_CONTEXTMENU      - request to display a context menu
//  WM_CREATE           - notification that a window is being created
//      WM_DESTROY          - window is being destroyed
//  WM_DISPLAYCHANGE    - display resolution change notification
//  WM_NCRBUTTONUP      - right button release in non-client area
//  WM_NOTIFY           - notifcation from a common control
//      WM_PAINT            - redraw all or part of the client area
//  WM_PRINTCLIENT      - request to draw all of client area into provided DC
//  WM_RBUTTONDOWN      - right button click in client area
//  WM_SETTINGCHANGE    - system parameter change notification
//  WM_SIZE             - window size has changed
//  WM_SYSCOLORCHANGE   - system color setting change notification
//  WM_USERCHANGED      - user log in/out notification
//
LRESULT CALLBACK mainFrameWndProc(
  HWND hwnd, 
  UINT message, 
  WPARAM wParam, 
  LPARAM lParam)
  {      
   COPYDATASTRUCT *theCDS;
   HWND hEditWnd;
      
   switch (message) 
     { 
      HANDLE_MSG(hwnd, WM_ACTIVATEAPP,   mainframe_OnActivateApp);
      HANDLE_MSG(hwnd, WM_COMMAND,       mainframe_OnCommand);
      HANDLE_MSG(hwnd, WM_CREATE,        mainframe_OnCreate);
      HANDLE_MSG(hwnd, WM_DESTROY,       mainframe_OnDestroy);
      HANDLE_MSG(hwnd, WM_NCRBUTTONUP,   mainframe_OnNCRButtonUp);
      HANDLE_MSG(hwnd, WM_PAINT,         mainframe_OnPaint);
      HANDLE_MSG(hwnd, WM_RBUTTONDOWN,   mainframe_OnRButtonDown);
      HANDLE_MSG(hwnd, WM_SIZE,          mainframe_OnSize);
      HANDLE_MSG(hwnd, WM_MOVE,          mainframe_OnMove);
      HANDLE_MSG(hwnd, WM_INITMENUPOPUP, mainframe_OnInitMenuPopup);
      HANDLE_MSG(hwnd, WM_MENUSELECT,    mainframe_OnMenuSelect);
      HANDLE_MSG(hwnd, WM_CLOSE,         mainframe_OnClose);

      case WM_COPYDATA:
        theCDS = (PCOPYDATASTRUCT) lParam;
        switch(theCDS->dwData )
          {
           case CD_FILENAME:
             if ((hEditWnd = text_New(hMainFrame,(char *) theCDS->lpData)) != NULL)
               { text_Revert(hMainFrame,(char *) theCDS->lpData,hEditWnd); }
          }
        
        return 0;
      
      case WM_CONTEXTMENU:      // Request to display a context menu
        return HANDLE_WM_CONTEXTMENU (hwnd, wParam, lParam, 
                        mainframe_OnContextMenu) ;


      case WM_DISPLAYCHANGE:   // Only comes through on plug'n'play systems
        return HANDLE_WM_DISPLAYCHANGE (hwnd, wParam, lParam, 
                        mainframe_OnDisplayChange) ;

      case WM_NOTIFY:          // Notification from a Common Control
        return HANDLE_WM_NOTIFY (hwnd, wParam, lParam, 
                        mainframe_OnNotify) ;

      case WM_SETTINGCHANGE:  // An application changed a systemwide setting
//    case WM_WININICHANGE:   // A WIN.INI setting has changed
        return HANDLE_WM_SETTINGCHANGE (hwnd, wParam, lParam, 
                        mainframe_OnSettingChange) ;

      case WM_SYSCOLORCHANGE: // A change has been made to a system 
                // color setting
        return HANDLE_WM_SYSCOLORCHANGE (hwnd, wParam, lParam, 
                        mainframe_OnSysColorChange) ;

      case WM_CTLCOLOREDIT:
      case WM_CTLCOLORMSGBOX:
      case WM_CTLCOLORLISTBOX:
      case WM_CTLCOLORBTN:
      case WM_CTLCOLORDLG:
      case WM_CTLCOLORSCROLLBAR:
      case WM_CTLCOLORSTATIC:
        return sendToActiveMDI(MDIClientWnd, message, wParam, lParam);

      case WM_USERCHANGED:                // User logged in or out
        return HANDLE_WM_USERCHANGED (hwnd, wParam, lParam, 
                        mainframe_OnUserChanged) ;

      case UWM_MDI_DESTROY:
        return mainframe_OnMDIDestroy(hwnd);

      case UWM_UPDATE_TOOLBAR:
        mainframe_OnUpdateToolbar(hwnd);
        return 0;

      case UWM_SET_ACCELERATOR:
        HANDLE_UWM_SET_ACCELERATOR(hwnd, wParam, lParam, 
                        mainframe_OnSetAccel);
        return 0;
                
      case WM_QUERYENDSESSION:
        return HANDLE_WM_QUERYENDSESSION(hwnd, wParam, lParam, 
                         mainframe_OnQueryEndSession) ;
                         
      case WM_SETCURSOR:
        //SetCursor(ARROW);
		return mainframe_OnSetCursor(hwnd, wParam, lParam);

      case WM_ERASEBKGND:
        return 1;
/*	    
	  case WM_KEYDOWN:
		if (LOWORD(wParam) == VK_SHIFT)
	      { ShiftKeyDown = TRUE; }
	    break;

      case WM_KEYUP:
        if (LOWORD(wParam) == VK_SHIFT)
	      { ShiftKeyDown = FALSE; }
	    break;
*/
                        
       default:
         return DefFrameProc (hwnd, MDIClientWnd, message, 
                        wParam, lParam) ;
     } 
#if WIN_BTC
   /* avoids warning */
#else
   return 0;
#endif
  }
  
/****************************************************/
/* mainframe_OnClose: Sends an exit message to the  */
/*   application when the close control is clicked. */
/****************************************************/
static void mainframe_OnClose(
  HWND hwnd)
  {
   if (EnumChildWindows(MDIClientWnd,(WNDENUMPROC) QuitEnumProc,(LPARAM) MDIClientWnd))
     { PostMessage(hwnd,WM_COMMAND,ID_APP_EXIT,0); }
  }

/***************************************************************/
/* mainframe_OnQueryEndSession: Attempts to close all windows. */
/***************************************************************/
static BOOL mainframe_OnQueryEndSession(
  HWND hwnd)
  {
   /*===============================================*/
   /* First, check this out with all child windows. */
   /* Try to close all child windows.               */
   /*===============================================*/
    
   FORWARD_WM_COMMAND(hwnd,ID_CLOSE_ALL,0,0,SendMessage);

   /*================================================*/
   /* If all child windows agreed to close, they are */
   /* destroyed by now. If GetFirstChild returns a   */
   /* window handle, it means not all windows were   */
   /* destroyed.                                     */
   /*================================================*/

   if (GetFirstChild (MDIClientWnd))
     { 
      /*=========================================*/
      /* At least one child window still exists. */
      /* Don't permit the session to end or the  */
      /* application to close.                   */
      /*=========================================*/
       
      return FALSE ; 
     } 
   
   return TRUE;
  }


/****************************************************************************
*                              mainframe_OnExit
* Inputs:
*       HWND hwnd: Window handle of main frame
* Result: void
*       
* Effect: 
*       Performs the actions to terminate the window
****************************************************************************/
static void mainframe_OnExit(
  HWND hwnd)
  {
   DestroyWindow(hwnd);
  }

/****************************************************************************
*                                closeEnumProc
* Inputs:
*       HWND hwnd: A child window of the MDICLIENT window
*    LPARAM lParam: 32-bit value passed to EnumChildWindows; 
*            (LPARAM) cast of the frame window handle
* Result: BOOL
*       TRUE, always
* Effect: 
*       Sends a WM_CLOSE message to the child window selected
* Notes:
*    We only want this processed by the MDI child, not by any of its
*    children.  Therefore, we ignore any window whose parent is not
*    the frame window, whose handle comes in as the lParam.
****************************************************************************/
static BOOL CALLBACK closeEnumProc(
  HWND hwnd, 
  LPARAM lParam)
  {
   if ((hwnd == FactsWindow) ||
       (hwnd == AgendaWindow) ||
       (hwnd == InstancesWindow) ||
       (hwnd == GlobalsWindow) ||
       (hwnd == FocusWindow))
     {
      FORWARD_WM_CLOSE(hwnd,SendMessage);
      return TRUE;
     }

   if (hwnd == DialogWindow)
     {
      return TRUE;
     }
     
   // Ignore the caption windows for iconized child windows
   if (GetWindowOwner(hwnd) == NULL)
     { 
      // Ignore any window which is not a direct descendant of the
      // frame window
      if (GetParent(hwnd) != (HWND)lParam)
        return TRUE;  // ignore, but continue iteration
      
      FORWARD_WM_MDIRESTORE(MDIClientWnd, hwnd, SendMessage);
      
      if (FORWARD_WM_QUERYENDSESSION(hwnd, SendMessage))
        FORWARD_WM_MDIDESTROY(MDIClientWnd, hwnd, SendMessage);
     }
   
   return TRUE;
  }
  
/*****************************************/
/* QuitEnumProc: Closes all edit windows */
/*   in preparation of quitting.         */
/*****************************************/
#if WIN_BTC
#pragma argsused
#endif
static BOOL CALLBACK QuitEnumProc(
  HWND hwnd, 
  LPARAM lParam)
  {
#if WIN_MCW
#pragma unused(lParam)
#endif
   int result;

   /*=================================================*/
   /* Ignore any windows that aren't editing windows. */
   /*=================================================*/
   
   if ((hwnd == FactsWindow) ||
       (hwnd == AgendaWindow) ||
       (hwnd == InstancesWindow) ||
       (hwnd == GlobalsWindow) ||
       (hwnd == FocusWindow) ||
       (hwnd == DialogWindow))
     { return TRUE; }
  
   if (GetClassWord(hwnd,GCW_ATOM) != EditAtomClass)
     { return TRUE; }
   
   /*======================================*/
   /* Determine whether the editing window */
   /* is to be save if modified.           */
   /*======================================*/
   
   result = text_QueryClose(hwnd);
   
   /*======================================*/
   /* If the user cancels, abort the quit. */
   /*======================================*/
   
   if (result == IDCANCEL)
     { return FALSE; }
     
   /*======================================*/
   /* Otherwise save the file if requested */
   /* and close the window.                */
   /*======================================*/
   
   if (result == IDYES)
     { text_Save(hwnd); }
     
   FORWARD_WM_CLOSE(hwnd,DefMDIChildProc);

   return TRUE;
  }
  
/****************************************************************************
*                            mainframe_OnCloseAll
* Inputs:
*       HWND hwnd: Window handle
* Result: void
*       
* Effect: 
*       Attempts to close all the MDI child windows, one at a time.
****************************************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void mainframe_OnCloseAll(
  HWND hwnd)
  {
#if WIN_MCW
#pragma unused(hwnd)
#endif
   EnumChildWindows(MDIClientWnd,(WNDENUMPROC) closeEnumProc,(LPARAM) MDIClientWnd);
  }

//
//  void mainframe_OnCommand (HWND hwnd, int id, HWND hwndCtl, UINT codeNotify)
//
//  hwnd            Handle of window to which this message applies
//  id              Specifies the identifier of the menu item, 
//            control, or accelerator.
//  hwndCtl         Handle of the control sending the message if the message
//                  is from a control, otherwise, this parameter is NULL. 
//  codeNotify      Specifies the notification code if the message is from 
//            a control.
//                  This parameter is 1 when the message is from an 
//            accelerator.
//                  This parameter is 0 when the message is from a menu.
//
//  PURPOSE:        
//
//  COMMENTS:
//
static void mainframe_OnCommand(
  HWND hwnd, 
  int id, 
  HWND hwndCtl, 
  UINT codeNotify)
  {        
   switch (id) 
     {
      case ID_APP_EXIT:
        mainframe_OnExit(hwnd);
        EnvExitRouter(GetCurrentEnvironment(),EXIT_SUCCESS);
        QuitDDE();
        return ;

      case ID_FILE_NEW:
        text_New(hwnd,NULL);
        return;

      case ID_FILE_OPEN:
        text_Open(hwnd);
        return;

      case  ID_FILE_REVERT:
        text_Revert(hwnd,szFileName,GetFirstChild(MDIClientWnd));
        return;
        
      //case  ID_WINDOW_ARRANGE:
      //  FORWARD_WM_MDIICONARRANGE(MDIClientWnd, SendMessage);
      //  return;

      case  ID_WINDOW_ARRANGE_STATUS:
        TileDisplayWindow();
        TileStatusWindows();
        return;
       
      case  ID_WINDOW_TILE_HORZ:
        FORWARD_WM_MDITILE(MDIClientWnd, MDITILE_HORIZONTAL, 
                        SendMessage);
        return ;
      
      case  ID_WINDOW_TILE_VERT:
        FORWARD_WM_MDITILE(MDIClientWnd, MDITILE_VERTICAL, 
                        SendMessage);
        return ;
        
      case  ID_WINDOW_CASCADE:
        FORWARD_WM_MDICASCADE(MDIClientWnd, 0, SendMessage);
        return;
                
      case ID_CLOSE_ALL:
        mainframe_OnCloseAll(hwnd);
        
      case  ID_FILE_CLOSE:
        mainframe_OnFileClose(hwnd);
        break;
      
      case  ID_EDIT_UNDO:
      case  ID_EDIT_CUT:
      case  ID_EDIT_COPY:
      case  ID_EDIT_PASTE:
      case  ID_EDIT_CLEAR:
      case  ID_EDIT_SELECT_ALL:
        break;
      
      case ID_VIEW_STATUS_BAR:
        mainframe_OnViewStatus(hwnd);
        break;

      case ID_FILE_LOAD:
      case ID_FILE_LOAD_BATCH:
      case ID_FILE_LOAD_BINARY:
      case ID_FILE_DRIBBLE:
      case ID_FILE_SAVE_BINARY:
      case ID_FILE_PAGE_SETUP:
        DoFileChoice(hwnd,id);
        return;
        
      case ID_EXECUTION_RESET:
      case ID_EXECUTION_CLEAR:
      case ID_EXECUTION_RUN:
      case ID_EXECUTION_STEP:
      case ID_EXECUTION_WATCH:
      case ID_EXECUTION_OPTIONS:
      case ID_EXECUTION_PREFERENCES:
      case ID_EXECUTION_HALT:
      case ID_EXECUTION_HALT_NOW:
        DoExecutionChoice(hwnd,(WORD) id);
        break;

      case ID_BROWSE_RULE:
      case ID_BROWSE_AGENDA:
      case ID_BROWSE_FACTS:
      case ID_BROWSE_TEMPLATE:
      case ID_BROWSE_FUNCTION:
      case ID_BROWSE_GLOBAL:
      case ID_BROWSE_GENERIC:
      case ID_BROWSE_INSTANCES:
      case ID_BROWSE_CLASS:
        DoBrowseChoice(hwnd,(WORD) id);
        break;

      case ID_HELP_ABOUT:
      case ID_HELP_CLIPS:
      case ID_HELP_COMPLETE:
        DoHelpChoice(hwnd,(WORD) id);
        break;
      
      case ID_WIN_SHOW_ALL:
      case ID_WIN_HIDE_ALL:
      case ID_WIN_CLEAR:
           DoWindowChoice(hwnd,(WORD) id);
           break;
 
      default:
        if (IDM_MODULE_ONE <= id && id <= IDM_MODULE_LAST)
          {
	       HMENU hMenu = GetMenu(hwnd);
		   DoModuleChoice(hMenu,(WORD) id);
		   return;
          }

        if (IDM_FIRSTDOCUMENT <= id && id <= IDM_LASTDOCUMENT)
          { 
           HWND ChildWnd;
           
           ChildWnd = GetDlgItem(MDIClientWnd, id);
           
           if (IsWindow(ChildWnd))
             { 
              FORWARD_WM_MDIACTIVATE(MDIClientWnd, FALSE, NULL, ChildWnd, 
                                        SendMessage);
             }
           return;
          } /* activate document */
            
     } /* id */

   // Warning: Failure to pass the command on to the frame procedure means
   // the child windows will never see WM_COMMAND messages, and key 
   // functionality such as the system menu on a maximized child window will
   // be missing.
 
   FORWARD_WM_COMMAND(hwnd, id, hwndCtl, codeNotify, fwdFrameProc);
   FORWARD_WM_COMMAND(MDIClientWnd, id, hwndCtl, codeNotify, sendToActiveMDI);
  }

/****************************************************************************
*                            mainframe_OnFileClose
* Inputs:
*       HWND hwnd: Parent window
* Result: void
*       
* Effect: 
*       Closes the active MDI child
****************************************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void mainframe_OnFileClose(
  HWND hwnd)
  {
#if WIN_MCW
#pragma unused(hwnd)
#endif
   sendToActiveMDI(MDIClientWnd,WM_CLOSE,0,0);
  }

/****************************************************************************
*                           mainframe_OnActivateApp
* Inputs:
*       HWND hwnd: Window handle
*       BOOL activate: TRUE if activating, FALSE if deactivating
*       DWORD threadid: Thread ID
* Result: void
*       
* Effect: 
*       Forces menus to recompute.  This is because global context (such as
*       the contents of the clipboard) might have changed.
****************************************************************************/
static void mainframe_OnActivateApp(
  HWND hwnd, 
  BOOL activate, 
  DWORD threadid)
  {
   if (activate)
     { sendToActiveMDI(MDIClientWnd, UWM_UPDATE_MENU, 0, 0); } 
     
   FORWARD_WM_ACTIVATEAPP(hwnd, activate, threadid, DefWindowProc);
  }

//
//  BOOL mainframe_OnContextMenu (HWND hwnd, HWND hwndCtl, int xPos, int yPos)
//
//  hwnd            Handle of window to which this message applies
//  hwndCtl         Handle of the window in which the user right clicked the 
//            mouse
//                  This may be the frame window itself or a control.
//  xPos            Horizontal position of the cursor, in screen coordinates
//  yPos            Vertical position of the cursor, in screen coordinates
//
//  PURPOSE:        Notification that the user clicked the right
//                  mouse button in the window.
//
//  COMMENTS:       Normally a window processes this message by
//                  displaying a context menu using the TrackPopupMenu
//                  or TrackPopupMenuEx functions. If a window does not
//                  display a context menu it should pass this message
//                  to the DefWindowProc function. 
//

static BOOL mainframe_OnContextMenu(
  HWND hwnd, 
  HWND hwndCtl, 
  int xPos, 
  int yPos)
  {
   POINT               pt ; //= { xPos, yPos } ;   // location of mouse click 
   RECT                rc ;                    // client area of window 

   pt.x = xPos;
   pt.y = yPos;
    
   if (FORWARD_UWM_CONTEXTMENU(MDIClientWnd,hwndCtl, 
                       xPos, yPos, sendToActiveMDI))
     { return TRUE; }

   /*================================================*/
   /* Get the bounding rectangle of the client area. */
   /*================================================*/
    
   GetClientRect (hwnd, &rc) ;
 
   /*===================================================*/
   /* Convert the mouse position to client coordinates. */
   /*===================================================*/
   
   ScreenToClient (hwnd, &pt) ;
 
   // If the mouse click was in the client area,
   // display the appropriate floating popup menu.
   if (PtInRect (&rc, pt))
     {
      if (mainframe_DisplayContextMenu (hwnd, pt))
        { return TRUE; }
     }
 
   // Otherwise forward the message for default processing
   return FORWARD_WM_CONTEXTMENU (hwnd, hwndCtl, xPos, yPos, DefWindowProc) ;
  }

//
//  BOOL mainframe_OnCreate (HWND hwnd, LPCREATESTRUCT lpCreateStruct)
//
//  hwnd            Handle of window to which this message applies
//  lpCreateStruct  Points to a CREATESTRUCT structure that contains
//                  information about the window being created
//
//  PURPOSE:        Perform any per-window initialization in response
//                  to this message, such as creating any desired
//                  child windows such as toolbars and status bars.
//
//  COMMENTS:       Windows sends this message after the window is
//                  created, but before the window becomes visible.
//                  Return TRUE to continue creation of the window; 
//                  otherwise return FALSE to fail the window creation.
//

#if WIN_BTC
#pragma argsused
#endif
static BOOL mainframe_OnCreate(
  HWND hwnd, 
  LPCREATESTRUCT lpCreateStruct)
  {
#if WIN_MCW
#pragma unused(lpCreateStruct)
#endif
   CLIENTCREATESTRUCT ccs;

   ccs.hWindowMenu = NULL;
   ccs.idFirstChild = IDM_FIRSTDOCUMENT;

   // Create the toolbar...
   if (! mainframe_CreateToolbar (hwnd))
     { return FALSE; }

   if (! mainframe_CreateStatusLine(hwnd))
     { return FALSE; }


   MDIClientWnd = CreateWindow(_T("MDICLIENT"), NULL,
                    WS_BORDER | WS_CHILD | WS_CLIPCHILDREN | WS_VISIBLE, 
                0, 0, 0, 0,
                hwnd,
                (HMENU) 1, // child ID
                GetWindowInstance(hwnd),
                (LPSTR)&ccs);
   
   if (MDIClientWnd == NULL)
     { return FALSE; }

   ShowWindow(MDIClientWnd, SW_SHOW);
   
   StartUpDDE();
   
   return TRUE ;
  }

//
//  void mainframe_OnDestroy (HWND hwnd)
//
//  hwnd            Handle of window to which this message applies
//
//  PURPOSE:        Notification that the specified window is being destroyed.
//                  The window is no longer visible to the user.
//
//  COMMENTS:
//

static void mainframe_OnDestroy(
  HWND hwnd)
  {
   // Tell WinHelp we don't need it any more...
   WinHelp (hwnd, getHelpFileName ("CLIPS6"), HELP_QUIT, (DWORD) 0) ;
   PostQuitMessage(0);
   ShutDownDDE();
  }

//
//  void mainframe_OnDisplayChange (HWND hwnd, UINT cBitsPerPixel,
//                                  UINT cxScreen, UINT cyScreen)
//
//  hwnd            Handle of window to which this message applies
//  cBitsPerPixel   Specifies the new image depth of the display
//                  in bits per pixel.
//  cxScreen        Specifies the new horizontal resolution of the screen.
//  cyScreen        Specifies the new vertical resolution of the screen.
//
//  PURPOSE:        Windows calls this handler when the display
//                  resolution has changed.
//
//  COMMENTS:       Beta versions of Windows 95 sent this message twice,
//                  once in anticipation of a change to the display
//                  resolution and once after the display resolution
//                  changed. The cBitsPerPixel parameter was a Boolean
//                  value which indicated which notification applied.
//
//                  Windows 95 now only sends this message after the
//                  display resolution has changed and the cBitsPerPixel
//                  parameter has the semantics described above.
//
//  VERSION NOTES:  The WM_DISPLAYCHANGE message is implemented on Windows 95
//                  but not currently on the Windows NT.
//

static void mainframe_OnDisplayChange(
  HWND hwnd, 
  UINT cBitsPerPixel, 
  UINT cxScreen, 
  UINT cyScreen)
  {
   FORWARD_WM_DISPLAYCHANGE (hwnd, cBitsPerPixel, 
                        cxScreen, cyScreen, fwdFrameProc) ;
  }


//
//  void mainframe_OnNCRButtonUp (HWND hwnd, int x, int y, UINT codeHitTest)
//
//  hwnd            Handle of window to which this message applies
//  x               Specifies the horizontal client coordinate of the mouse 
//            click.
//  y               Specifies the vertical client coordinate of the mouse 
//            click.
//  codeHitTest     Specifies the hit-test value returned by the DefFrameProc
//                  function as a result of processing the WM_NCHITTEST 
//            message. 
//
//  PURPOSE:        Windows calls this handler when the user releases the right
//                  mouse button while the cursor is in the non-client area of
//                  a window.
//
//  COMMENTS:       Windows 95 user interface guildines state that you should
//                  display a window popup menu in certain circumstances in
//                  response to this message. The window pop-up menu is
//                  the popup menu associated with a window, in this case,
//                  the main frame window. Do not mistake the window popup
//                  menu for the "Window" drop-down menu found in MDI
//                  applications. The window popup menu replaces the
//                  Windows 3.x Control menu, also referred to as the System
//                  menu.
//
//                  Note: DefWindowProc on Windows 95 processes this message
//                  by sending a WM_CONTEXTMENU message. Therefore all context
//                  menu processing under Windows 95 is normally performed
//                  in response to the WM_CONTEXTMENU message.

//                  The user displays a window’s popup menu by clicking the
//                  right mouse button anywhere in the title bar area, 
//                  excluding the title bar icon. Clicking on the title bar 
//                  icon with the right button displays the pop-up menu for 
//                  the object represented by the icon.
//

static void mainframe_OnNCRButtonUp(
  HWND hwnd, 
  int x, 
  int y, 
  UINT codeHitTest)
  {
   switch (codeHitTest) 
     {
      case HTSYSMENU:
        // Windows NT note:
        // The user just clicked the right mouse button on the 
        // application's title bar icon. Normally you would now alter
        // the default system menu however your application requires. 
        // See the Explorer for an example.
        return ;

      case HTCAPTION:
      case HTREDUCE:
      case HTZOOM:
        // Windows NT note:

        // The user just clicked the right mouse button on the 
        // application's title bar , excluding the title bar icon. 
        // Normally you would now display the window popup menu.
        return;
     }

   // Allow default message processing regardless
   FORWARD_WM_NCRBUTTONUP (hwnd, x, y, codeHitTest, fwdFrameProc) ;
  }

//
//  BOOL mainframe_OnNotify (HWND hwnd, int idCtrl, NMHDR FAR* pnmhdr)
//
//  hwnd            Handle of window to which this message applies
//  idCtrl          Identifier of the control sending the message
//  pnmhdr          Pointer to structure that contains the notification
//                  code and additional information
//
//  PURPOSE:        An event has occurred in one of thie window's child
//                  controls or a control requires some kind of information
//
//  COMMENTS:
//

static BOOL mainframe_OnNotify(
  HWND hwnd, 
  int idFrom, 
  LPNMHDR pnmhdr)
  {
   if (Toolbar_OnNotify (hwnd, idFrom, pnmhdr))
     { return TRUE; }

   return FALSE;
  }

//
//  void mainframe_OnPaint (HWND hwnd)
//
//  hwnd            Handle of window to which this message applies
//
//  PURPOSE:        Windows calls this handler when the window needs 
//            repainting.
//
//  COMMENTS:
//

static void mainframe_OnPaint(
  HWND hwnd)
  {
   PAINTSTRUCT ps ;

   BeginPaint (hwnd, &ps) ;

   // Your drawing code goes here...

   EndPaint (hwnd, &ps) ;
  }


//
//  void mainframe_OnRButtonDown (HWND hwnd, BOOL fDoubleClick, 
//                    int x, int y, UINT keyFlags)
//
//  hwnd            Handle of window to which this message applies
//  fDoubleClick    TRUE when this is a double-click notification,
//                  FALSE when this is a single-click notification.
//  x               Specifies the horizontal client coordinate of the mouse 
//            click
//  y               Specifies the vertical client coordinate of the mouse click
//  keyFlags        Flags indicating the state of some virtual keys
//
//  PURPOSE:        Windows calls this handler when the user presses the right
//                  mouse button while the cursor is in the client area of a 
//            window.
//
//  COMMENTS:       Windows 95 user interface guidelines recommend you display
//                  the context menu for the item in the client area upon which
//                  the user just clicked.
//                  
//

#if WIN_BTC
#pragma argsused
#endif
static void mainframe_OnRButtonDown(
  HWND hwnd, 
  BOOL fDoubleClick, 
  int x, 
  int y, 
  UINT keyFlags)
  {
#if WIN_MCW
#pragma unused(fDoubleClick)
#pragma unused(keyFlags)
#endif
   POINT pt; // = { x, y } ;
    
   pt.x = x;
   pt.y = y;

   mainframe_DisplayContextMenu (hwnd, pt) ;
  }

/****************************************************************************
*                            mainframe_OnSetAccel
* Inputs:
*       HWND hwnd: Main frame window
*    HACCEL newaccel
* Result: HACCEL
*       Previous accelerator
* Effect: 
*       Sets the 
****************************************************************************/
#if WIN_BTC
#pragma argsused
#endif
static HACCEL mainframe_OnSetAccel(
  HWND hwnd, 
  HACCEL newaccel)
  {
#if WIN_MCW
#pragma unused(hwnd)
#endif
   HACCEL oldaccel = haccel ;
   
   haccel = newaccel;
   return oldaccel;
  }

//
//  void mainframe_OnSettingChange (HWND hwnd, UINT uiFlag, LPCTSTR pszMetrics)
//  void mainframe_OnWinIniChange  (HWND hwnd, UINT unused, LPCTSTR pszSection)
//
//                  WM_SETTINGCHANGE:
//  hwnd            Handle of window to which this message applies
//  uiFlag          Specifies the systemwide parameter that has changed.
//  pszMetrics      Points to the string "WindowMetrics" when certain
//                  systemwide parameters have changed
//
//                  WM_WININICHANGE:
//  hwnd            Handle of window to which this message applies
//  unused          Unused, must be zero.
//  pszSection      Points to a string containing the name
//                  of the section that has changed
//
//  PURPOSE:        This is the appropriate place to reload any system
//                  metrics that the application has cached.
//
//  COMMENTS:       Windows 95 sends the WM_SETTINGCHANGE message to all
//                  windows after an application changes a system-wide
//                  setting by calling the SystemParametersInfo function.
//
//                  Windows NT sends the WM_WININICHANGE message to all
//                  windows after an application changes a system-wide
//                  setting by calling the SystemParametersInfo function.
//
//                  On all operating systems, an application sends the
//                  WM_WININICHANGE message to all top-level windows
//                  after making a change to the WIN.INI file
//
//  VERSION NOTES:  Microsoft assigned the same message code (26) to both
//                  the WM_SETTINGCHANGE and the WM_WININICHANGE messages.
//                  The only way to distinguish the two messages is to
//                  test the uiFlag parameter. A zero value means this is
//                  a WM_WININICHANGE message and a non-zero value means
//                  this is a WM_SETTINGCHANGE message.
//

/****************************************************************************
*                                resize_Frame
* Inputs:
*       HWND hwnd: Parent window
* Result: void
*       
* Effect: 
*       Forces the client window to resize
****************************************************************************/

static void resize_Frame(
  HWND hwnd)
  {
   RECT toolrect;
   RECT client;
   HWND hwndToolbar ;

   hwndToolbar = GetDlgItem (hwnd, IDC_TOOLBAR) ;
   ASSERT (NULL != hwndToolbar) ;

   // Get client rectangle
   GetClientRect(hwnd, &client);

   // Compute size of toolbar
   GetWindowRect(hwndToolbar, &toolrect);
   ScreenToClient(hwnd, (LPPOINT)&toolrect.right);

   // Compute the size of the status line (which is drawn on the
   // main frame itself).

   if (view_status_line)
     {
      RECT sr;
      GetWindowRect(GetDlgItem(hwnd, IDC_STATUS), &sr);
      client.bottom -= (sr.bottom - sr.top);
     }

   client.top = toolrect.bottom;

   if (MDIClientWnd != NULL)
     {
      MoveWindow(MDIClientWnd,
             client.left,
             client.top,
             client.right - client.left,
             client.bottom - client.top,
             TRUE);
     }       
  }
  
/********************************************************/
/* AvailableFrameHeight: Returns the amount of vertical */
/*   space available in which to place windows.         */
/********************************************************/
int AvailableFrameHeight()
  {
   RECT toolrect;
   RECT client = {0,0,0,0};
   HWND hwndToolbar ;
   int height;
   RECT sr;
   WINDOWPLACEMENT placement;

   if (hMainFrame == NULL) return(0);
      
   /*=======================*/
   /* Get client rectangle. */
   /*=======================*/
   
   /* GetClientRect(hMainFrame, &client); */

   placement.length = sizeof(WINDOWPLACEMENT);
   
   GetWindowPlacement(hMainFrame,&placement);

   height = placement.rcNormalPosition.bottom - placement.rcNormalPosition.top;
   
   /*=====================================================*/
   /* Determine the amount of space the window would take */
   /* up if the client area is empty. This is going to be */
   /* the size of the window title bar and borders        */
   /*=====================================================*/

   if (hMainMenu == NULL)
     { AdjustWindowRect(&client,WS_OVERLAPPEDWINDOW,FALSE); }
   else
     { AdjustWindowRect(&client,WS_OVERLAPPEDWINDOW,TRUE); }
        
   height -= (client.bottom - client.top);
  
   hwndToolbar = GetDlgItem (hMainFrame, IDC_TOOLBAR) ;
   ASSERT (NULL != hwndToolbar) ;

   /*==========================*/
   /* Compute size of toolbar. */
   /*==========================*/
   
   GetWindowRect(hwndToolbar, &toolrect);
   height -= (toolrect.bottom - toolrect.top);
   
   /*============================================*/
   /* Compute the size of the status line (which */ 
   /* is drawn on the main frame itself).        */
   /*============================================*/

   GetWindowRect(GetDlgItem(hMainFrame,IDC_STATUS),&sr);
   if (view_status_line)
     { height -= (sr.bottom - sr.top); }
     
   /*====================================*/
   /* Subtract the status line again to  */
   /* leave space for minimized windows. */
   /*====================================*/
   
   height -= (sr.bottom - sr.top) + 15;

   /*========================================*/
   /* Return the amount of available height. */
   /*========================================*/
   
   return(height);
  }
  
/********************************************************/
/* AvailableFrameWidth: Returns the amount of horizonal */
/*   space available in which to place windows.         */
/********************************************************/
int AvailableFrameWidth()
  {
   /* RECT toolrect; */
   RECT client = {0,0,0,0};
   /* HWND hwndToolbar ; */
   int width;
   /* RECT sr; */
   WINDOWPLACEMENT placement;
   
   if (hMainFrame == NULL) return(0);
      
   /*=======================*/
   /* Get client rectangle. */
   /*=======================*/
   
   placement.length = sizeof(WINDOWPLACEMENT);
   
   GetWindowPlacement(hMainFrame,&placement);
     
   width = placement.rcNormalPosition.right - placement.rcNormalPosition.left;

   if (hMainMenu == NULL)
     { AdjustWindowRect(&client,WS_OVERLAPPEDWINDOW,FALSE); }
   else
     { AdjustWindowRect(&client,WS_OVERLAPPEDWINDOW,TRUE); }
   
   width -= (client.right - client.left);
  
   /*=======================================*/
   /* Return the amount of available width. */
   /*=======================================*/
   
   return(width);
  }
    
#if WIN_BTC
#pragma argsused
#endif
static void mainframe_OnSettingChange(
  HWND hwnd, 
  UINT uiFlag, 
  LPCTSTR pszMetrics)
  {
#if WIN_MCW
#pragma unused(pszMetrics)
#pragma unused(hwnd)
#pragma unused(uiFlag)
#endif
  }

/******************************************/
/* mainframe_OnSize: Notify child windows */
/*   that parent changed size.            */
/******************************************/
static void mainframe_OnSize(
  HWND hwnd, 
  UINT state, 
  int cx, 
  int cy)
  {
   HWND hwndToolbar ;
   HWND status;
   RECT r, p;
   int height;

   r.left = 0;
   r.top = 0;
   r.right = cx;
   r.bottom = cy;
 
   GetWindowRect(hwnd,&p);
   
   if (hMainMenu == NULL)
     { AdjustWindowRect(&r,WS_OVERLAPPEDWINDOW,FALSE); }
   else
     { AdjustWindowRect(&r,WS_OVERLAPPEDWINDOW,TRUE); }
   
   SaveWindowInformation(r.bottom-r.top,r.right-r.left,p.left,p.top);
  
   hwndToolbar = GetDlgItem (hwnd, IDC_TOOLBAR) ;
   ASSERT (NULL != hwndToolbar) ;

   FORWARD_WM_SIZE (hwndToolbar, state, cx, cy, SendMessage) ;

   status = GetDlgItem(hwnd, IDC_STATUS);
   ASSERT( NULL != status);
   GetWindowRect(status, &r);
   height = (r.bottom - r.top); 
   SetWindowPos(status, NULL, 0, cy - height, cx, height, SWP_NOZORDER);

   if (state != SIZE_MINIMIZED)
     { resize_Frame(hwnd); } 
  }

/******************************************/
/* mainframe_OnMove:           */
/******************************************/
static void mainframe_OnMove(
  HWND hwnd, 
  int x,
  int y)
  {
   RECT r, p;

   r.left = x;
   r.top = y;
   r.right = x;
   r.bottom = y;
    
   GetWindowRect(hwnd,&p);
   
   if (hMainMenu == NULL)
     { AdjustWindowRect(&r,WS_OVERLAPPEDWINDOW,FALSE); }
   else
     { AdjustWindowRect(&r,WS_OVERLAPPEDWINDOW,TRUE); }
      
   SaveWindowInformation(p.bottom-p.top,p.right-p.left,r.left,r.top);
  }

//
//  void mainframe_OnSysColorChange (HWND hwnd)
//
//  hwnd            Handle of window to which this message applies
//
//  PURPOSE:        Forward the WM_SYSCOLORCHANGE message to all child
//                  windows of this top-level window, as well as to all
//                  children of the child windows, if any.
//
//  COMMENTS:       Windows sends the WM_SYSCOLORCHANGE message to all
//                  top-level windows after a change has been made to
//                  the system color settings. Your top level window
//                  must forward the WM_SYSCOLORCHANGE message to its
//                  common controls; otherwise the controls will not be
//                  notified of the color change.
//

static void mainframe_OnSysColorChange(
  HWND hwnd)
  {
   EnumChildWindows (hwnd, mainframe_SysColorChangeNotification, 
                    (LPARAM) NULL) ;
  }

//
//  void mainframe_SysColorChangeNotification (HWND hwnd, LPARAM lParam)
//
//  hwnd            Handle of child window
//  lParam          Application-defined value specified as the last
//                  parameter on the EnumChildWindows function call.
//
//  PURPOSE:        Forward a WM_SYSCOLORCHANGE message to the
//                  specified child window.
//
//  COMMENTS:
//

#if WIN_BTC
#pragma argsused
#endif
BOOL CALLBACK mainframe_SysColorChangeNotification(
  HWND hwnd, 
  LPARAM lParam)
  {
#if WIN_MCW
#pragma unused(lParam)
#endif
   // Forward the message to a child window
   FORWARD_WM_SYSCOLORCHANGE (hwnd, SendMessage) ;

   // Keep on enumerating...
   return TRUE ;
  }

//
//  void mainframe_OnUserChanged (HWND hwnd)
//
//  hwnd            Handle of window to which this message applies
//
//  PURPOSE:        This is the appropriate place to reload any cached
//                  user-specific information.
//                  
//  COMMENTS:       Windows sends the WM_USERCHANGED message to all windows
//                  after a user has logged on or off. When a user logs on
//                  or off, the system updates the user-specific settings. 
//                  Windows sends the WM_USERCHANGED message immediately 
//                  after updating the user-specific settings. 
//
//  VERSION NOTES:  The WM_USERCHANGED message is implemented on Windows 95
//                  but not currently on the Windows NT.
//

#if WIN_BTC
#pragma argsused
#endif
static void mainframe_OnUserChanged(
  HWND hwnd)
  {
#if WIN_MCW
#pragma unused(hwnd)
#endif
  }

/*********************************************************/
/* mainframe_DisplayContextMenu: Display the appropriate */
/*   context menu for the object located at 'pt' in the  */
/*   main frame window.                                  */
/*********************************************************/
BOOL mainframe_DisplayContextMenu(
  HWND hwnd, 
  POINT pt)
  {
   HMENU hmenuBar, hmenuPopup;
   int nItems;

   /*=======================*/
   /* Get main menu handle. */
   /*=======================*/
   
   hmenuBar = GetMenu (hwnd) ;
   ASSERT (NULL != hmenuBar) ;

   /*==========================================*/
   /* Get the count of items on the main menu. */
   /*==========================================*/
   
   nItems = GetMenuItemCount (hmenuBar) ;
   ASSERT (-1 != nItems) ;

   /*=========================================================*/
   /* ASSUMPTION: Help menu is the rightmost on the menu bar. */
   /*=========================================================*/
   
   hmenuPopup = GetSubMenu (hmenuBar, nItems - 1) ;
   ASSERT (NULL != hmenuPopup) ;

   /*===============================================*/
   /* Convert click location to screen coordinates. */
   /*===============================================*/
   
   ClientToScreen (hwnd, &pt) ;

   /*=====================================================*/
   /* Display the floating popup menu at the mouse click  */
   /* location. Track the right mouse as this function is */
   /*    called during WM_CONTEXTMENU message processing. */
   /*=====================================================*/
   
   return TrackPopupMenu (hmenuPopup,
                           TPM_LEFTALIGN | TPM_RIGHTBUTTON,
                           pt.x, pt.y, 0, hwnd, NULL) ;
  }

/********************************************************/
/* mainframe_OnMDIDestroy: If the last MDI child window */
/*   is destroyed, reverts to the minimal menu.         */
/********************************************************/
#if WIN_BTC
#pragma argsused
#endif
static LRESULT mainframe_OnMDIDestroy(
  HWND hwnd)
  {

#if WIN_MCW
#pragma unused(hwnd)
#endif   /*
   HWND child = FORWARD_WM_MDIGETACTIVE(MDIClientWnd, SendMessage);
   
   if (child == NULL)
     {
      FORWARD_WM_MDISETMENU(MDIClientWnd, TRUE, hMainMenu, NULL, SendMessage);
      SendMessage(hMainFrame, UWM_SET_ACCELERATOR, 0, (LPARAM)hMainAccel);
      DrawMenuBar(hMainFrame);
      PostMessage(hMainFrame, UWM_UPDATE_TOOLBAR, 0, 0);
     }
   */
   return 0;
  }

/************************************************************/
/* mainframe_OnUpdateToolbar: Causes the toolbar to update. */
/************************************************************/
static void mainframe_OnUpdateToolbar(
  HWND hwnd)
  {
   HWND hwndToolbar ;
     
   hwndToolbar = GetDlgItem(hwnd,IDC_TOOLBAR);
     
   if (hwndToolbar != NULL)
     { Toolbar_UpdateUI(hwndToolbar); }
  }

/**********************************************************/
/* mainframe_OnInitMenuPopup: Handles the initialization  */
/*   of menu items.  First handles any local changes then */
/*   forwards the message to the active child window.     */
/**********************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void mainframe_OnInitMenuPopup(
  HWND hwnd, 
  HMENU hmenu, 
  UINT item, 
  BOOL sysmenu)
  {
#if WIN_MCW
#pragma unused(hwnd)
#endif
   BOOL modified;
   unsigned menuID;
   
   /*============================================*/
   /* Do any frame-specific initialization here. */
   /*============================================*/

   HWND child = FORWARD_WM_MDIGETACTIVE(MDIClientWnd,SendMessage);
   
   //EnableMenuItem(hmenu, ID_FILE_CLOSE, (child != NULL) ? MF_ENABLED 
   //                                : MF_GRAYED);

   if (child == NULL)
     {
      EnableMenuItem(hmenu,ID_FILE_SAVE_AS,MF_GRAYED);
      EnableMenuItem(hmenu,ID_FILE_SAVE,MF_GRAYED); 
     }
   else if (GetClassWord(child,GCW_ATOM) == EditAtomClass)
     {
      EnableMenuItem(hmenu,ID_FILE_SAVE_AS,MF_ENABLED);                                                         
      modified = Edit_GetModify(GetDlgItem(child,ID_EDIT_CONTROL));
      if (GetWindowLong(child,GWL_USERDATA) == 0L)
        { modified = TRUE; }
      EnableMenuItem(hmenu,ID_FILE_SAVE,(UINT) (modified ? MF_ENABLED : MF_GRAYED));
      
      EnableMenuItem(hmenu,ID_FILE_REVERT,
                     (UINT) (modified ?
                             ((GetWindowLong(child,GWL_USERDATA) != 0L) ? 
                               MF_ENABLED : MF_GRAYED) : MF_GRAYED));
     }
   else
     {                                   
      CheckMenuItem(hmenu,ID_VIEW_STATUS_BAR,
                    (UINT) (view_status_line ? MF_CHECKED : MF_UNCHECKED));
     }
   
   /*=========================*/
   /* Update the module menu. */
   /*=========================*/
   
   menuID = GetMenuItemID(hmenu,0);
   if (menuID == IDM_MODULE_ONE)
     { UpdateModuleMenu (hmenu); }
     
   /*=======================================================*/
   /* Now forward it to the child window, which will do its */
   /* own handling of the popup menu initialization.        */
   /*=======================================================*/

   FORWARD_WM_INITMENUPOPUP(MDIClientWnd,hmenu,item,sysmenu,sendToActiveMDI);
  }

/**********************************************************/
/* mainframe_OnViewStatus: Toggles the view status state. */
/**********************************************************/
static void mainframe_OnViewStatus(
  HWND hwnd)
  {
   RECT r;
   HWND status = GetDlgItem(hwnd, IDC_STATUS);

   view_status_line = !view_status_line;     
     
   ShowWindow(status, view_status_line ? SW_SHOW : SW_HIDE);
     
   GetWindowRect(hwnd, &r);
   resize_Frame(hwnd);
  }

/***************************************************************/
/* sendToActiveMDI: Sends the message to the active MDI child. */
/***************************************************************/
static LRESULT sendToActiveMDI(
   HWND mdiclient, 
   UINT message, 
   WPARAM wParam, 
   LPARAM lParam)
   {
    HWND child = FORWARD_WM_MDIGETACTIVE(mdiclient, SendMessage);
   
    if (child == NULL)
      { return 0; }
    return SendMessage(child,message, wParam, lParam);
   }

/****************************************************/
/* mainframe_OnMenuSelect: Updates the status line. */
/****************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void mainframe_OnMenuSelect(
  HWND hwnd, 
  HMENU hmenu, 
  int item, 
  HMENU hmenuPopup, 
  UINT flags)
  {
#if WIN_MCW
#pragma unused(hmenu)
#pragma unused(hmenuPopup)
#endif
   TCHAR prompt[256];
   HWND status;

   status = GetDlgItem(hwnd, IDC_STATUS);

   if (flags == (UINT)-1)
     {
      SetWindowText(status, _T(""));
      return;
     }
     
   if (flags & MF_POPUP)
     { return; }  // top-level menu item

   LoadString(GetWindowInstance(hwnd),(unsigned) item,prompt,DIM(prompt));
   SetWindowText(status, prompt);
  }

/****************************************************/
/* mainframe_OnSetCursor: Updates the cursor.       */
/****************************************************/
#if WIN_BTC
#pragma argsused
#endif
static BOOL mainframe_OnSetCursor(
	HWND hwnd,
	WPARAM wParam,
	LPARAM lParam)
	{
#if WIN_MCW
#pragma unused(hwnd)
#pragma unused(wParam)
#endif
		switch LOWORD(lParam)		{
//		case HTCAPTION: 
//		case HTCLIENT:
//		case HTCLOSE:
//		case HTERROR:
//		case HTGROWBOX:
//		case HTHELP:
//		case HTHSCROLL:
//		case HTMENU:
//		case HTMAXBUTTON:
//		case HTMINBUTTON:
//		case HTNOWHERE:
//		case HTREDUCE:
//		case HTSYSMENU:
//		case HTTRANSPARENT:
//		case HTVSCROLL:
//		case HTZOOM:
//		case HTBORDER:
		case HTBOTTOM:
				SetCursor(LoadCursor(NULL,IDC_SIZENS));
				return TRUE;
		case HTBOTTOMLEFT:
				SetCursor(LoadCursor(NULL,IDC_SIZENESW));
				return TRUE;
		case HTBOTTOMRIGHT:
				SetCursor(LoadCursor(NULL,IDC_SIZENWSE));
				return TRUE;
		case HTLEFT:				;
				SetCursor(LoadCursor(NULL,IDC_SIZEWE));
				return TRUE;
		case HTRIGHT:
				SetCursor(LoadCursor(NULL,IDC_SIZEWE));
				return TRUE;
		case HTTOP:
				SetCursor(LoadCursor(NULL,IDC_SIZENS));
				return TRUE;
		case HTTOPLEFT:				;
				SetCursor(LoadCursor(NULL,IDC_SIZENWSE));
				return TRUE;
		case HTTOPRIGHT:
				SetCursor(LoadCursor(NULL,IDC_SIZENESW));
				return TRUE;
		default:
			SetCursor(LoadCursor(NULL,IDC_ARROW));
			break;
		}
		return FALSE;
	}
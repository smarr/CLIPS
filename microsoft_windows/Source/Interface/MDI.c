   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*       Microsoft Windows Version 3.0  01/31/02       */
   /*                                                     */
   /*         MULTIPLE DOCUMENT INTERFACE MODULE          */
   /*******************************************************/

/**************************************************************/
/* Purpose:                                                   */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Christopher J. Ortiz                                  */
/*      Gary Riley                                            */
/*                                                            */
/* Contributing Programmer(s):                                */
/*                                                            */
/* Revision History:                                          */
/*                                                            */
/**************************************************************/

#include "stdSDK.h"

#include "setup.h"

#include "display.h"
#include "resource.h"
#include "Frame.h"
#include "mdi.h"
#include "EditUtil.h"

// If you are going to run under Win32s, you must enable the line below.
// CreateMDIWindow does not exist in Win32s.
//#define USE_WM_MDICREATE
BOOL mdi_DisplayContextMenu(HWND,POINT,HMENU);
/********************************************************/
/* mdi_DisplayContextMenu: Pops up a text context menu. */
/********************************************************/
BOOL mdi_DisplayContextMenu(
  HWND hwnd, 
  POINT pt, 
  HMENU context)
  {
   HMENU popup;

   if (context == NULL)
     { return FALSE; }
     
   popup = GetSubMenu(context, 0);

   ClientToScreen(hwnd, &pt);
   mdi_ConformMenus(GetMenu(hMainFrame), popup);

   TrackPopupMenu(popup, TPM_LEFTALIGN | TPM_RIGHTBUTTON,
                                pt.x, pt.y, 0, 
                                hMainFrame, NULL);
   return TRUE;
  }

/*******************************************************/
/* mdi_OnContextMenu: Pops up a context-specific menu. */
/*******************************************************/
#if WIN_BTC
#pragma argsused
#endif
BOOL mdi_OnContextMenu(
  HWND hwnd, 
  HWND hwndCtl, 
  int xPos, 
  int yPos, 
  HMENU context)
  {
#if WIN_MCW
#pragma unused(hwndCtl)
#endif
   POINT pt;
   RECT  rc;

   pt.x = xPos;
   pt.y = yPos;
     
   /*================================*/
   /* Bring the window to the front. */
   /*================================*/
   
   if (IsWindow(hwnd))
     { FORWARD_WM_MDIACTIVATE(MDIClientWnd,FALSE,NULL,hwnd,SendMessage); }
  
   /*===============================*/
   /* Get the bounding rectangle of */
   /* the client area of the child. */
   /*===============================*/
   
   GetClientRect(hwnd,&rc);
   
   /*============================*/
   /* Convert the mouse position */
   /* to client coordinates.     */
   /*============================*/
          
   ScreenToClient(hwnd,&pt);
   
   /*======================================*/
   /* If the mouse click was in the client */
   /* area of this child window, display   */
   /* the appropriate popup menu.          */
   /*======================================*/
                            
   if (PtInRect(&rc,pt))
     {
      if (mdi_DisplayContextMenu(hwnd,pt,context))
        { return TRUE; }
     }

   /*============================*/
   /* Otherwise, tell our caller */
   /* that we don't handle it.   */
   /*============================*/
   
   return FALSE;
  }

/**************************************************************/
/* mdi_ConformMenus4: For each menu item in hPopup, its state */
/*   is set to be the same state as found in hMaster, unless  */
/*   there is no such state in which case it is left alone.   */
/*   This can only work with API level 4 and above.           */
/**************************************************************/
static void mdi_ConformMenus4(
  HMENU hMaster, 
  HMENU hPopup)
  {
   unsigned count = (unsigned) GetMenuItemCount(hPopup);
   unsigned i;
   MENUITEMINFO getpopupinfo;
   MENUITEMINFO setpopupinfo;
   MENUITEMINFO masterinfo;
     
   getpopupinfo.cbSize = sizeof(MENUITEMINFO);
   getpopupinfo.fMask  = MIIM_SUBMENU | MIIM_ID;

   setpopupinfo.cbSize = sizeof(MENUITEMINFO);
   setpopupinfo.fMask  = MIIM_STATE;

   masterinfo.cbSize   = sizeof(MENUITEMINFO);
   masterinfo.fMask    = MIIM_STATE;

   for (i = 0; i < count; i++)
     { /* process each */
      VERIFY(GetMenuItemInfo(hPopup, i, TRUE, &getpopupinfo));
      if (getpopupinfo.hSubMenu != NULL)
        { mdi_ConformMenus4(hMaster, getpopupinfo.hSubMenu); }
      else
        { 
         GetMenuItemInfo(hMaster, getpopupinfo.wID, FALSE, &masterinfo);
         setpopupinfo.fState = masterinfo.fState;
         SetMenuItemInfo(hPopup, i, TRUE, &setpopupinfo);
        }
     }
  }

/***************************************************************/
/* mdi_ConformMenus3: For each menu item in hPopup, its state  */
/*   is set to be the same state as found in hMaster, unless   */
/*   there is no such state in which case it is left alone.    */
/*   This somewhat clunky version is required for API level 3. */
/***************************************************************/
static void mdi_ConformMenus3(
  HMENU hMaster, 
  HMENU hPopup)
  {
   unsigned count = (unsigned) GetMenuItemCount(hPopup);
   unsigned i;
   UINT popupstate;
   HMENU submenu;

   for (i = 0; i < count; i++)
     { 
      popupstate = GetMenuState(hPopup, i, MF_BYPOSITION);
      if ((popupstate & ~0xFF) != 0)
        { 
         submenu = GetSubMenu(hPopup,(int) i);
         mdi_ConformMenus3(hMaster, submenu);
        } 
      else
        { 
         UINT id = GetMenuItemID(hPopup,(int) i);
         UINT masterstate = GetMenuState(hMaster, id, MF_BYCOMMAND);

         EnableMenuItem(hPopup, i, MF_BYPOSITION |
                        (masterstate & (MF_ENABLED | MF_DISABLED | MF_GRAYED)));
         CheckMenuItem(hPopup, i, MF_BYPOSITION |
                        (masterstate & (MF_CHECKED | MF_UNCHECKED)));
        } 
     } 
  }

/**************************************************************/
/* mdi_ConformMenus:  For each menu item in hPopup, its state */
/*   is set to be the same state as found in hMaster, unless  */
/*   there is no such state in which case it is left alone.   */
/**************************************************************/
void mdi_ConformMenus(
  HMENU hMaster, 
  HMENU hPopup)
  {
   OSVERSIONINFO info;
     
   info.dwOSVersionInfoSize = sizeof(info);
   GetVersionEx(&info);
   if (info.dwMajorVersion < 4)
     { mdi_ConformMenus3(hMaster, hPopup); }
   else
     { mdi_ConformMenus4(hMaster, hPopup); }
  }

/************************************************************/
/* mdi_Create: Creates an MDI child window. CreateMDIWindow */
/*   only exists in Windows/NT and Windows/96. To use in    */
/*   Win32s, you must define USE_WM_MDICREATE.              */
/************************************************************/                                
#if WIN_BTC
#pragma argsused
#endif
HWND mdi_Create(
  HWND hwnd, 
  DWORD styles, 
  unsigned classid, 
  char *title,
  int x,
  int y,
  int width,
  int height)
  {
#if WIN_MCW
#pragma unused(styles)
#endif
   HINSTANCE hinst = GetWindowInstance(hwnd);
   HWND hwndChild;
   TCHAR Class[80];
     
   LoadString(hinst,classid,Class,DIM(Class));

   hwndChild = CreateMDIWindow(Class,title,0, 
                               x,y,width,height,   
                               hwnd,hinst,0);
     
   if (hwndChild == NULL)
     { ReportError(GetLastError()); } 
   
   return hwndChild;
  }

/*****************************************************/
/* ReportError: Pops up message box reporting error. */
/*****************************************************/
void ReportError(
  DWORD err)
  {
   LPTSTR str;

   FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
                 0, err,
                 0, (LPTSTR)&str,
                 0, NULL);
   MessageBox(NULL, str, _T(""), MB_ICONSTOP | MB_OK | MB_TASKMODAL);
   LocalFree(str);
  }

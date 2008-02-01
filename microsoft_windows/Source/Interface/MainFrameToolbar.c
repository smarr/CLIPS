   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*       Microsoft Windows Version 3.0  01/31/02       */
   /*                                                     */
   /*              MAIN FRAME TOOLBAR MODULE              */
   /*******************************************************/

/**************************************************************/
/* Purpose:                                                   */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Gary Riley                                            */
/*                                                            */
/* Contributing Programmer(s):                                */
/*                                                            */
/* Revision History:                                          */
/*                                                            */
/**************************************************************/

#include <Windows.h>
#include <commctrl.h>

#include "setup.h"

#include "StdSDK.h"             // Standard application includes
#include "MainFrameToolbar.h"   // For non-static function prototypes
#include "resource.h"           // For resource identifiers
#include "EditUtil.h"

#include <tchar.h>              // For _tcschar

/****************************/
/* Toolbar Button Map Entry */
/****************************/

typedef struct _tagButtonMapEntry {
    UINT                nID ;               // Command identifier
    UINT                nIndex ;            // Bitmap index
} BUTTONMAPENTRY ;


/**********************************************/
/* Buttons Appearing Initially On The Toolbar */
/**********************************************/

static BUTTONMAPENTRY ButtonMap [] =
{
  { ID_FILE_NEW,            STD_FILENEW    },
  { ID_FILE_OPEN,           STD_FILEOPEN   },
  { ID_FILE_SAVE,           STD_FILESAVE   },
  { 0,                      0              },
  { ID_EDIT_CUT,            STD_CUT        },
  { ID_EDIT_COPY,           STD_COPY       },
  { ID_EDIT_PASTE,          STD_PASTE      },
  { 0,                      0              },
  { ID_FILE_PRINT,          STD_PRINT      },
  { ID_HELP_ABOUT,          STD_HELP       }
} ;

//
// Buttons optionally appearing on the toolbar
//

static BUTTONMAPENTRY ButtonMapExtras [] =
{
  { ID_EDIT_UNDO,           STD_UNDO       },
  { ID_EDIT_REDO,           STD_REDOW       },
  { ID_EDIT_CLEAR,          STD_DELETE     },
  { ID_FILE_PRINT_PREVIEW,  STD_PRINTPRE   },
  { ID_BUFFER_FIND,         STD_FIND       },
  { ID_BUFFER_REPLACE,      STD_REPLACE    }
} ;



/*********************************************/
/* mainframe_CreateToolbar: Create a toolbar */
/*   for the main frame window.              */
/*********************************************/
HWND mainframe_CreateToolbar(
  HWND hwnd)
  {
   HWND hwndToolbar;
   int i, nButtons, nImages ;
   TBBUTTON tbb [DIM (ButtonMap)] ;

   /*========================================*/
   /* Get the application's instance handle. */
   /*========================================*/

   // Set number of buttons to display
   nButtons = DIM (ButtonMap) ;

   // Set number of images in the bitmap
   nImages  = STD_PRINT + 1 ;

   /*======================================*/
   /* Initialize the toolbar button array. */
   /*======================================*/
   
   ZeroMemory (tbb, sizeof (tbb)) ;

   for (i = 0; i < nButtons; i++) 
     {
      // When the command identifer is zero,
      //  place a separator in this position of the toolbar
      
      if (0 == ButtonMap [i].nID) 
        {
         tbb [i].fsStyle = TBSTYLE_SEP ;
         // tbb [i].iBitmap is the separator width on
         // TBSTYLE_SEP entries. It is already zero
         // which means "use a reasonable width."
         continue ;
        }

      tbb[i].idCommand = (int) ButtonMap [i].nID ;     // Command identifier
      tbb[i].iBitmap   = (int) ButtonMap [i].nIndex ;  // Bitmap index
      tbb[i].fsState   = TBSTATE_ENABLED ;
      tbb[i].fsStyle   = TBSTYLE_BUTTON ;
     }

   /*=============================*/
   /* Create the toolbar control. */
   /*=============================*/
    
   hwndToolbar = CreateToolbarEx(
        hwnd,                  // Parent window handle
        TBSTYLE_TOOLTIPS |     // Toolbar window styles
        TBSTYLE_WRAPABLE |
        CCS_ADJUSTABLE,
        IDC_TOOLBAR,           // Control ID
        nImages,               // Number of images in the bitmap
        HINST_COMMCTRL,        // Module containing bitmap
        IDB_STD_SMALL_COLOR, //IDB_STD_LARGE_COLOR,   // Resource ID of bitmap
        tbb,                   // TBBUTTON structs about buttons
        nButtons,              // # of buttons to add
        0,                     // Width of button (default is 24)
        0,                     // Height of button (default is 22)
        0,                     // Width of bitmap (default is 16)
        0,                     // Height of bitmap (default is 15)
        sizeof (TBBUTTON));    // Size of TBBUTTON struct

   Toolbar_UpdateUI (hwndToolbar) ;    // Sync toolbar with menu state
   ShowWindow (hwndToolbar, SW_SHOW) ;

   return hwndToolbar ;
  }

/**********************************************/
/* Toolbar_OnNotify: Handle all notifications */
/*   associated with the toolbar.             */
/**********************************************/
BOOL Toolbar_OnNotify(
  HWND hwnd, 
  int idFrom, 
  LPNMHDR pnmhdr)
  {
   switch (pnmhdr->code) 
     {
      /*======================================*/
      /* Tool tip control needs the tip text. */
      /*======================================*/
      
      case TTN_NEEDTEXT:
        Toolbar_OnNeedText (hwnd, idFrom, (LPTOOLTIPTEXT) pnmhdr) ;
        return TRUE ;

      case TBN_QUERYINSERT:
      case TBN_QUERYDELETE:
        // Returns TRUE to allow the button to be inserted or deleted
        return TRUE ;

      case TBN_GETBUTTONINFO:
        // The user double-clicked on the toolbar
        // which displayed the toolbar customization dialog box.

        // Provide the dialog box with button information
        // about buttons which can be added
            
        {
         LPTBNOTIFY pTbNotify = (LPTBNOTIFY) pnmhdr ;

         // As long as we have new buttons to describe...
         if (pTbNotify->iItem < DIM (ButtonMapExtras)) 
           {
            // Describe another button...
            ZeroMemory (&pTbNotify->tbButton, sizeof (pTbNotify->tbButton));
            pTbNotify->tbButton.iBitmap   = (int) ButtonMapExtras [pTbNotify->iItem].nIndex;
            pTbNotify->tbButton.idCommand = (int) ButtonMapExtras [pTbNotify->iItem].nID;
            pTbNotify->tbButton.fsState   = TBSTATE_ENABLED ;
            pTbNotify->tbButton.fsStyle   = TBSTYLE_BUTTON ;
            pTbNotify->cchText            = 0 ;
            return TRUE;
           }
        }
       return FALSE ;
      }

   /*=============================================*/
   /* Unrecognized toolbar notification. Let some */
   /* subsequent handle process the notification. */
   /*=============================================*/
   
   return FALSE ;
  }

/**********************************/
/* Toolbar_OnNeedText: Return the */
/*   requested tool tip text.     */
/**********************************/
#if WIN_BTC
#pragma argsused
#endif
void Toolbar_OnNeedText(
  HWND hwnd, 
  int idCtrl, 
  LPTOOLTIPTEXT pttt)
  {
#if WIN_MCW
#pragma unused(idCtrl)
#endif
   HINSTANCE hinst;
   TCHAR szPrompt [256] ;

   ASSERT (0 == (pttt->uFlags & TTF_IDISHWND)) ;

   /*===========================================*/
   /* Get instance handle for string resources. */
   /*===========================================*/
   
   hinst = GetWindowInstance (hwnd) ;

   // Load the string resource with the button ID
   LoadString (hinst, pttt->hdr.idFrom, szPrompt, sizeof (szPrompt)) ;

   // The tool tip is the part after the newline character
   pttt->lpszText = _tcschr (szPrompt, (TCHAR) '\n') ;

   // If tool tip text found, skip over the newline character
   if (pttt->lpszText)
     { pttt->lpszText++; }

   pttt->hinst = NULL;
  }

/*********************************************************/
/* Toolbar_UpdateUI: For each button on the toolbar,     */
/*   enable the button if there is an enabled menu item  */
/*   with the same command identifier as the button.     */
/*   Disable the button if there is a disabled menu item */
/*   with the same command identifier as the button.     */
/*   Otherwise, leave the button state unmodified.       */
/*********************************************************/
void Toolbar_UpdateUI(
  HWND hwnd)
  {
   HWND hwndFrame;
   HMENU hmenu;
   unsigned nCount;

   /*=======================================*/
   /* Get frame window (parent of toolbar). */
   /*=======================================*/
   
   hwndFrame = GetParent(hwnd);
   ASSERT(NULL != hwndFrame);

   /*===================================*/
   /* Get the main frame window's menu. */
   /*===================================*/
   
   hmenu = GetMenu(hwndFrame);
   //ASSERT (NULL != hmenu) ;

   if (NULL == hmenu)
     { return; }

   /*======================================*/
   /* Get count of buttons on the toolbar. */
   /*======================================*/
   
   nCount = Toolbar_ButtonCount(hwnd);

   /*===================================*/
   /* For each button on the toolbar... */
   /*===================================*/
   
   while (nCount > 0) 
     {
      BOOL bMenuEnabled, bButtonEnabled;
      TBBUTTON tbb;
      UINT uiState;

      /*===================================*/
      /* Get information about the button. */
      /*===================================*/
      
      VERIFY(Toolbar_GetButton(hwnd,--nCount,&tbb));

      /*==========================================*/
      /* If the button is a separator, ignore it. */
      /*==========================================*/
      
      if (tbb.fsStyle & TBSTYLE_SEP)
        { continue; }

      /*=========================================*/
      /* Get information about the corresponding */
      /* menu item, if any.                      */
      /*=========================================*/
      
      uiState = GetMenuState(hmenu,(unsigned) tbb.idCommand,MF_BYCOMMAND);
      if (0xFFFFFFFF == uiState)
        { bMenuEnabled = FALSE; }
      else
        { bMenuEnabled = 0 == (uiState & (MF_DISABLED | MF_GRAYED)); }

      bButtonEnabled = 0 != (tbb.fsState & TBSTATE_ENABLED);

      /*===========================================*/
      /* If button and menu are in the same state, */
      /* we need do nothing for this button.       */
      /*===========================================*/
      
      if (bMenuEnabled == bButtonEnabled)
        { continue; }

      /*=====================================*/
      /* Enable/Disable this toolbar button. */
      /*=====================================*/
      
      VERIFY(Toolbar_EnableButton(hwnd,tbb.idCommand,bMenuEnabled));
     }
  }

/*************************************************************/
/* mainframe_CreateStatusLine: Creates a status line window. */
/*************************************************************/
HWND mainframe_CreateStatusLine(
  HWND hwnd)
  {
   RECT r;
   HWND hwndStatus;
   TEXTMETRIC tm;
   HDC dc;
   int height;

   dc = GetDC(hwnd);
   GetTextMetrics(dc, &tm);
   height = tm.tmHeight + tm.tmExternalLeading;
   ReleaseDC(hwnd,dc);

   GetClientRect(hwnd, &r);

   hwndStatus = CreateWindow(_T("STATIC"), NULL,
     				SS_LEFT | WS_VISIBLE | WS_CHILD,
				r.left, r.bottom - height,
				r.right - r.left,
				height,
				hwnd,
				(HMENU)IDC_STATUS,
				GetWindowInstance(hwnd),
				(LPVOID)NULL);
				
   return hwndStatus;   				    
  }

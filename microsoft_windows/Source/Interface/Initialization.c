   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*       Microsoft Windows Version 3.0  01/31/02       */
   /*                                                     */
   /*                INITIALIZATION MODULE                */
   /*******************************************************/

/**************************************************************/
/* Purpose: Handles initialization of interface.              */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Gary Riley                                            */
/*                                                            */
/* Contributing Programmer(s):                                */
/*      Ernst Bokkelkamp                                      */
/*                                                            */
/* Revision History:                                          */
/*      6.24: Ernst's changes to remember window positions.   */
/*                                                            */
/**************************************************************/

#include <windows.h>

#define _INITIALIZATION_SOURCE_

#include "setup.h"

#include "StdSDK.h"     
#include "Initialization.h"
#include "Frame.h"      
#include "resource.h"   
#include "Text.h"
#include "EditUtil.h"
#include "MDI.h"
#include "SearchDialog.h"
#include "status.h"
#include "Print.h"
#include "display.h"
#include "Registry.h"  /* EB */

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static ATOM                    internalRegisterClass(const LPWNDCLASSEX);
   static HWND                    createMainFrameWindow(HINSTANCE,int);
   static BOOL                    registerWindowClasses(HINSTANCE,UINT);

//
// Function prototypes for callback functions
//

//
// Typedefs
//

typedef ATOM (WINAPI* REGISTERCLASSEXPROC)(const LPWNDCLASSEX lpwcex) ;


#define MAXFILENAME 256

char szFileName[MAXFILENAME];
char szFileTitle[MAXFILENAME];
HICON hHourGlass;    

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   globle OPENFILENAME               ofn;
   globle HCURSOR                    ARROW;
   globle HCURSOR                    QUERY;
   globle HCURSOR                    WAIT[10];

/******************************************************************/
/* initInstance: Initialize the application. Activate a previous  */
/*   instance, if any, by searching for a window created with the */
/*   same window class name that this instance uses. If found,    */
/*   restore the previous instance's window, if necessary, and    */
/*   make that window the foreground window. Register the window  */
/*   class(es) using a WNDCLASSEX structure. The specified        */
/*   resPoolID parameter is the UINT resource identifier used to  */
/*   locate the application's main frame window class name, the   */
/*   menu used on the main frame window, the large and small      */
/*   icons for the main frame window.                             */
/******************************************************************/
BOOL initInstance(
  HINSTANCE hinst, 
  UINT resPoolID, 
  int nCmdShow,
  LPSTR lpCmdLine)
  {
   HWND  hwnd;
   TCHAR ClassName [MAX_RESOURCESTRING + 1] ;
   int n;
   COPYDATASTRUCT theCDS;
   char *fileName;
   size_t length;
   
   hMainAccel = LoadAccelerators(hinst, "ClipsAcc");
   
   n = LoadString (hinst, resPoolID, ClassName, DIM(ClassName)) ;
   
   VERIFY(n != 0);

   /*========================================*/
   /* Determine if there is a prior instance */
   /* of the main frame window.              */
   /*========================================*/
   
   hwnd = FindWindow(ClassName,NULL);
   
   /*===================================================*/
   /* Register all application-specific window classes. */
   /*===================================================*/
   
   if (! registerWindowClasses (hinst, resPoolID))
     { return FALSE; }

   /*=====================================*/
   /* Initialize the Common Controls DLL. */
   /*=====================================*/
   
   InitCommonControls();

   /*=============================================*/
   /* Create the application's main frame window. */
   /*=============================================*/
   
   hMainFrame = createMainFrameWindow (hinst, nCmdShow);

   if (hMainFrame == NULL)
     { return FALSE; }

   /*======================================================*/
   /* Constrain this application to run a single instance. */
   /*======================================================*/
   
   if (hwnd != NULL) 
     {
      /*=====================================================*/
      /* A previous instance of this application is running. */
      /* Activate the previous instance, tell it what the    */
      /* user requested this instance to do, then abort      */
      /* initialization of this instance.                    */
      /*=====================================================*/                   

      if (IsIconic (hwnd))
        { ShowWindow(hwnd,SW_RESTORE); }

      SetForegroundWindow(hwnd);

      /*=====================================================*/
      /* Send an application-defined message to the previous */
      /* instance (or use some other type of IPC mechanism)  */
      /* to tell the previous instance what to do.           */         
      /* Determining what to do generally depends on how the */
      /* user started this instance.                         */
      /*=====================================================*/

      length = strlen(lpCmdLine);
      fileName = (char *) malloc(length - 1);
      if (fileName == NULL) return FALSE;
      strncpy(fileName,&lpCmdLine[1],length-2);
      fileName[length-2] = 0;

      theCDS.dwData = CD_FILENAME;
      theCDS.cbData = length - 1;
      theCDS.lpData = fileName;

      SendMessage(hwnd,WM_COPYDATA,
                  (WPARAM)(HWND) hMainFrame,
                  (LPARAM) (LPVOID) &theCDS);
                   
      free(fileName);
      
      /*=======================================*/
      /* Abort this instance's initialization. */
      /*=======================================*/
      
      return FALSE;
     }

   /*==================================*/
   /* Go ahead and show the main frame */
   /* at this point in initialization. */
   /*==================================*/
   
   ShowWindow(hMainFrame,nCmdShow);
   UpdateWindow(hMainFrame);

   /*================================*/
   /* Initialize text child windows. */
   /*================================*/
   
   if (! text_InitInstance(hinst))
     { return FALSE; }

   /*===================================*/
   /* Initialize display child windows. */
   /*===================================*/
   
   if (! display_InitInstance(hinst))
     { return FALSE; }

   /*====================================*/
   /* Initialize display child windoows. */
   /*====================================*/
   
   if (! status_InitInstance(hinst))
     { return FALSE; }

   /*===================*/
   /* Set up the menus. */
   /*===================*/ 

   hMainAccel = LoadAccelerators(hinst,"ClipsAcc");
   haccel = hMainAccel;       

   if (FORWARD_WM_MDISETMENU(MDIClientWnd,TRUE,hMainMenu,
                             NULL,SendMessage) != 0)
     {
      SendMessage(hMainFrame, UWM_SET_ACCELERATOR, 0, (LPARAM) hMainAccel);
      DrawMenuBar(hMainFrame);
      PostMessage(hMainFrame, UWM_UPDATE_TOOLBAR, 0, 0);
     }

   /*======================================*/
   /* Initialize find/replace information. */
   /*======================================*/
   
   InitFindReplace(hwnd);

   hHourGlass = LoadCursor(NULL,IDC_WAIT);
   
   /* Initialize cursors. */
   
   WAIT[0] = LoadCursor((HINSTANCE) hinst, "CURSOR0" );
   WAIT[1] = LoadCursor((HINSTANCE) hinst, "CURSOR1" );
   WAIT[2] = LoadCursor((HINSTANCE) hinst, "CURSOR2" );
   WAIT[3] = LoadCursor((HINSTANCE) hinst, "CURSOR3" );
   WAIT[4] = LoadCursor((HINSTANCE) hinst, "CURSOR4" );
   WAIT[5] = LoadCursor((HINSTANCE) hinst, "CURSOR5" );
   WAIT[6] = LoadCursor((HINSTANCE) hinst, "CURSOR6" );
   WAIT[7] = LoadCursor((HINSTANCE) hinst, "CURSOR7" );
   WAIT[8] = LoadCursor((HINSTANCE) hinst, "CURSOR8" );
   WAIT[9] = LoadCursor((HINSTANCE) hinst, IDC_WAIT );

   QUERY = LoadCursor((HINSTANCE) hinst,"CLIPS_QUERY");
   ARROW = LoadCursor(NULL,IDC_ARROW);
   
   SetCursor(ARROW);
   
   /*====================================================*/
   /* Fill in non-variant fields of OPENFILENAME struct. */
   /*====================================================*/

   ofn.lStructSize       = sizeof(OPENFILENAME);
   ofn.hwndOwner       = hMainFrame;
   ofn.lpstrFilter     = "CLIPS Files (*.CLP)\0*.CLP\0Batch Files (*.BAT)\0*.BAT\0Text Files (*.TXT)\0*.TXT\0All Files (*.*)\0*.*\0";
   ofn.lpstrCustomFilter = NULL;
   ofn.nMaxCustFilter          = 0;
   ofn.nFilterIndex    = 1;
   ofn.lpstrFile         = szFileName;
   ofn.nMaxFile        = MAXFILENAME;
   ofn.lpstrInitialDir   = NULL;
   ofn.lpstrFileTitle    = szFileTitle;
   ofn.nMaxFileTitle     = MAXFILENAME;
   ofn.lpstrTitle        = NULL;
   ofn.lpstrDefExt       = "CLP";
   ofn.Flags             = OFN_HIDEREADONLY;

   /*==============================*/
   /* Initialize the print dialog. */
   /*==============================*/

   InitializePrintDialog(hMainFrame);

   /*============================*/
   /* Create the display window. */
   /*============================*/
   
#if DEFTEMPLATE_CONSTRUCT
   factsWindow_New(hMainFrame);
#endif
#if DEFRULE_CONSTRUCT
   agendaWindow_New(hMainFrame);
#endif
#if OBJECT_SYSTEM
   instancesWindow_New(hMainFrame);
#endif
#if DEFGLOBAL_CONSTRUCT 
   globalsWindow_New(hMainFrame);
#endif
#if DEFRULE_CONSTRUCT
   focusWindow_New(hMainFrame);
#endif
   displayWindow_New(hMainFrame);
   SendMessage(DialogWindow,WM_MDIACTIVATE,0,(LPARAM) DialogWindow);

   return TRUE;
  }

/******************************************/
/* exitInstance: Perform deinitialization */
/*   and return exit code.                */
/******************************************/
WPARAM exitInstance(
  MSG *pmsg)
  {
   text_ExitInstance();
   return pmsg->wParam;
  }
 
/******************************************/
/* registerWindowClasses: Register all    */
/*   application-specific window classes. */ 
/******************************************/
static BOOL registerWindowClasses(
  HINSTANCE hinst, 
  UINT resPoolID)
  {
   TCHAR      ClassName [MAX_RESOURCESTRING + 1] ;
   WNDCLASSEX wcex ;

   VERIFY (LoadString (hinst, resPoolID, ClassName, DIM(ClassName))) ;

   /*================================================*/
   /* Fill in window class structure with parameters */
   /* that describe the main window.                 */
   /*================================================*/
    
   wcex.cbSize        = sizeof (WNDCLASSEX) ;
   wcex.style         = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS ;
   wcex.lpfnWndProc   = mainFrameWndProc ;
   wcex.cbClsExtra    = 0 ;
   wcex.cbWndExtra    = 0 ;
   wcex.hInstance     = hinst ;
   wcex.hIcon         = LoadIcon (hinst, MAKEINTRESOURCE (resPoolID)) ;
   wcex.hCursor       = LoadCursor (NULL, IDC_ARROW) ;
   wcex.hbrBackground = (HBRUSH) (COLOR_WINDOW+1) ;

   wcex.lpszMenuName  = MAKEINTRESOURCE (resPoolID) ;
   wcex.lpszClassName = ClassName ;
   wcex.hIconSm       = (HICON) LoadImage (hinst,
                                           MAKEINTRESOURCE (resPoolID),
                                           IMAGE_ICON,
                                           GetSystemMetrics (SM_CXSMICON),
                                           GetSystemMetrics (SM_CYSMICON),
                                           LR_SHARED) ;

   /*============================================================*/
   /* Register the window class and return success/failure code. */
   /*============================================================*/
   
   return internalRegisterClass (&wcex) ;
  }

/********************************************************/
/* createMainFrameWindow: Create the application's main */
/*   frame window and show the window as requested.     */
/********************************************************/
#if WIN_BTC
#pragma argsused
#endif
static HWND createMainFrameWindow(
  HINSTANCE hinst, 
  int nCmdShow)
  {
#if WIN_MCW
#pragma unused(nCmdShow)
#endif
   HWND  hwnd ;
   TCHAR ClassName [MAX_RESOURCESTRING + 1] ;
   TCHAR Title [MAX_RESOURCESTRING + 1] ;
   int height, width, x, y;

   /*===============================*/
   /* Create the main frame window. */
   /*===============================*/
   
   VERIFY (LoadString (hinst, IDR_MAINFRAME, ClassName, DIM (ClassName))) ;
   VERIFY (LoadString (hinst, IDS_APP_TITLE, Title, DIM (Title))) ;

   LoadWindowInformation(&height,&width,&x,&y); /* EB */

   hwnd =
        CreateWindowEx (0,               // Extended window styles
                    ClassName,           // Address of registered class name
                    Title,               // Address of window name
                    WS_OVERLAPPEDWINDOW, // Window style
                    x,                   // Horizontal position of window 
                    y,                   // Vertical position of window 
                    width,               // Window width                     
                    height,              // Window height                  
                    NULL,                // Handle of parent or owner window
                    NULL,                // Handle of menu for this window
                    hinst,               // Handle of application instance
                    NULL) ;              // Address of window-creation data

   ASSERT (NULL != hwnd);

   if (hwnd == NULL)
     { return NULL; }

   return hwnd ;
  }

/*******************************************************/
/* internalRegisterClass: Registers the window class   */
/*   using RegisterClassEx if it is available. If not, */
/*   registers the class using RegisterClass.          */ 
/*   RegisterClassEx was introduced in Windows 95 and  */
/*   Windows NT 3.51. An application must register its */
/*   window classes using RegisterClassEx in order to  */
/*   specify the small icons which should be used for  */
/*   the application.                                  */
/*******************************************************/
static ATOM internalRegisterClass(
  const LPWNDCLASSEX lpwcex)
  {
   WNDCLASS wc ;

   // Get the module handle of the 32-bit USER DLL
   HANDLE hModule = GetModuleHandle (TEXT("USER32")) ;
   
   if (NULL != hModule) 
     {
      // If we're running on a Win32 version supporting RegisterClassEx
      //  get the address of the function so we can call it

      REGISTERCLASSEXPROC proc =
          (REGISTERCLASSEXPROC) GetProcAddress ((HMODULE) hModule, "RegisterClassExA") ;

      if (NULL != proc)
        {
         // RegisterClassEx exists...
         // return RegisterClassEx (&wcex) ;
         return (*proc) (lpwcex);
        }
     }

   // Convert the WNDCLASSEX structure to a WNDCLASS structure
   wc.style         = lpwcex->style ;
   wc.lpfnWndProc   = lpwcex->lpfnWndProc ;
   wc.cbClsExtra    = lpwcex->cbClsExtra ;
   wc.cbWndExtra    = lpwcex->cbWndExtra ;
   wc.hInstance     = lpwcex->hInstance ;
   wc.hIcon         = lpwcex->hIcon ;
   wc.hCursor       = lpwcex->hCursor ;
   wc.hbrBackground = lpwcex->hbrBackground ;
   wc.lpszMenuName  = lpwcex->lpszMenuName ;
   wc.lpszClassName = lpwcex->lpszClassName ;

   return RegisterClass (&wc) ;
  }

/*****************************************************************/
/* registerMDIChild: Registers the class.                        */
/*   The resPoolID identifies the resources involved             */
/*     LoadString(hinst, resPoolID,...) for the class name       */
/*     LoadCursor(hinst, resPoolID) if there is a cursor of that */
/*       ID, or IDC_ARROW if there is no cursor                  */
/*     LoadIcon(hinst, resPoolID) if there is an icon of that    */
/*       ID, or IDI_APPLICATION if there is no icon              */
/*****************************************************************/
ATOM registerMDIChild(
  HINSTANCE hinst, 
  DWORD styles,
  UINT resPoolID, 
  WNDPROC wproc, 
  int extra)
  {
   WNDCLASSEX wcex ;
   TCHAR ClassName[80];
   HCURSOR cursor;
   HICON icon;

   if (LoadString(hinst, resPoolID, ClassName, DIM(ClassName) ) == 0)
     { return (ATOM) NULL; }  // failed

   cursor = LoadCursor(hinst,MAKEINTRESOURCE(resPoolID)) ;
   icon = LoadIcon(hinst,MAKEINTRESOURCE (resPoolID)) ;

   wcex.cbSize        = sizeof (WNDCLASSEX) ;
   /* wcex.style         = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS | styles; */
   wcex.style         = CS_DBLCLKS | styles;
   wcex.lpfnWndProc   = wproc ;
   wcex.cbClsExtra    = 0 ;
   wcex.cbWndExtra    = extra;
   wcex.hInstance     = hinst ;
   wcex.hIcon         = (icon != NULL ? icon : LoadIcon(NULL, IDI_APPLICATION)) ;
   wcex.hCursor       = (cursor != NULL ? cursor : LoadCursor (NULL, IDC_ARROW)) ;
   wcex.hbrBackground = (HBRUSH) (COLOR_WINDOW+1) ;

   wcex.lpszMenuName  = NULL; // MDI children generally have no menus
   wcex.lpszClassName = ClassName ;
   wcex.hIconSm       = (HICON) LoadImage (hinst,
                                           MAKEINTRESOURCE (resPoolID),
                                           IMAGE_ICON,
                                           GetSystemMetrics (SM_CXSMICON),
                                           GetSystemMetrics (SM_CYSMICON),
                                           LR_SHARED);
  
   return internalRegisterClass (&wcex) ;
  }
  


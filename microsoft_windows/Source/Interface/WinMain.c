   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*       Microsoft Windows Version 3.0  01/31/02       */
   /*                                                     */
   /*                 WINDOWS MAIN MODULE                 */
   /*******************************************************/

/**************************************************************/
/* Purpose: Main startup functions for Windows interface.     */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Christopher J. Ortiz                                  */
/*      Gary Riley                                            */
/*                                                            */
/* Contributing Programmer(s):                                */
/*       Ernst Bokkelkamp                                     */
/*                                                            */
/* Revision History:                                          */
/*       6.24: Ernst's changes to use stand Windows cursors.  */
/*                                                            */
/**************************************************************/

/***************************************************************************/
/*                                                                         */
/* Permission is hereby granted, free of charge, to any person obtaining   */
/* a copy of this software and associated documentation files (the         */
/* "Software"), to deal in the Software without restriction, including     */
/* without limitation the rights to use, copy, modify, merge, publish,     */
/* distribute, and/or sell copies of the Software, and to permit persons   */
/* to whom the Software is furnished to do so.                             */
/*                                                                         */
/* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS */
/* OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF              */
/* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT   */
/* OF THIRD PARTY RIGHTS. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY  */
/* CLAIM, OR ANY SPECIAL INDIRECT OR CONSEQUENTIAL DAMAGES, OR ANY DAMAGES */
/* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN   */
/* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF */
/* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.          */
/*                                                                         */
/***************************************************************************/

#include <windows.h>

#include "setup.h"

#include "commline.h"
#include "engine.h"
#include "filertr.h"
#include "router.h"
#include "sysdep.h"

#include "StdSDK.h"    
#include "Initialization.h"  
#include "Frame.h"   
#include "resource.h"  
#include "mdi.h"
#include "SearchDialog.h"

#include "display.h"
#include "status.h"
#include "menu.h"
#include "Registry.h"
#include "Text.h"

#include <winuser.h>

void UserFunctions(void);

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    SetUpRouters(void *);
   static intBool                 QueryInterfaceRouter(void *,char *);
   static int                     PrintInterfaceRouter(void *,char *,char *);
   static int                     ExitInterfaceRouter(void *,int);
   static int                     GetcInterfaceRouter(void *,char *);
   static int                     InterfaceEventFunction(void *);
   static void                    WinRunEvent(void *);

/**************************************************/
/* WinMain: Entry point for the application. This */
/*   function initializes the application and     */
/*   processes the message loop.                  */
/**************************************************/
#if WIN_BTC
#pragma argsused
#endif
int WINAPI WinMain(
  HINSTANCE hInstance, 
  HINSTANCE hPrevInstance, 
  LPSTR lpCmdLine, 
  int nCmdShow)
  {   
#if WIN_MCW
#pragma unused(hPrevInstance)
#pragma unused(lpCmdLine)
#endif
   void *theEnv;
   HWND hEditWnd;
   
   /*=============================*/   
   /* Application initialization. */
   /*=============================*/
   
   theEnv = CreateEnvironment(); 

   if (! initInstance(hInstance,IDR_MAINFRAME,nCmdShow,lpCmdLine)) 
     { return FALSE; }
     
   /*==================================*/
   /* Setup routers for the interface. */
   /*==================================*/
   
   SetUpRouters(theEnv);

   /*================================================================*/
   /* Set up hook between the command loop and interface event loop. */
   /*================================================================*/

#if ! RUN_TIME
   SetEventFunction(GetCurrentEnvironment(),InterfaceEventFunction);
#endif

   /*====================================*/
   /* Add execution functions to update  */
   /* the interface between rule firings */
   /* and execution of procedural code.  */
   /*====================================*/
   
   EnvAddPeriodicFunction(theEnv,"status_wnd",WinRunEvent,0);
#if DEFRULE_CONSTRUCT
   EnvAddRunFunction(theEnv,"run_function",WinRunEvent,0);
#endif

   /*==================================================*/
   /* Add a function which prevents command execution  */
   /* if we're editing a command in the dialog window. */
   /*==================================================*/
   
   SetBeforeCommandExecutionFunction(theEnv,DisplayBeforeCommandExecution);

   /*====================================*/
   /* Register the function which allows */
   /* the display to be cleared.         */
   /*====================================*/
  
   EnvDefineFunction2(theEnv,"clear-window",'v',
                      PTIEF ClearWindowCommand,
                      "ClearWindowCommand", "00");
                      
   /*======================================*/
   /* Set the focus to the display window. */
   /*======================================*/
      
   display_OnSetFocus(DialogWindow,NULL);
      
   /*=====================================*/
   /* Read preferences from the registry. */
   /*=====================================*/
   
   ReadRegistryInformation();
   
   /*====================*/
   /* Main message loop. */
   /*====================*/
   
   if ((lpCmdLine != NULL) &&
       (lpCmdLine[0] != 0))
     { 
      char *fileName;
      size_t length;
      
      /* Strip the quotation marks from the command line argument. */
      
      length = strlen(lpCmdLine);
      fileName = (char *) malloc(length - 1);
      if (fileName == NULL) return FALSE;
      strncpy(fileName,&lpCmdLine[1],length-2);
      fileName[length-2] = 0;
      
      if ((hEditWnd = text_New(hMainFrame,fileName)) != NULL)
        { text_Revert(hMainFrame,fileName,hEditWnd); }
        
      free(fileName);
     }
 
   CommandLoop(GetCurrentEnvironment());
   
   return TRUE;
  }

/**************************************/
/* SetUpRouters: Sets up routers used */
/*   by the windowed interface.       */
/**************************************/
static void SetUpRouters(
  void *theEnv)
  {  
   EnvAddRouter(theEnv,"InterfaceExit",60,NULL,NULL,NULL,NULL,ExitInterfaceRouter);
   EnvAddRouter(theEnv,"InterfaceStdIO",10,QueryInterfaceRouter,PrintInterfaceRouter,GetcInterfaceRouter,NULL,NULL);
  }

/**************************************************/
/* ExitInterfaceRouter: Routine to  check an exit */
/*   from the dialog window to make sure that     */
/*   the user has an opportunity to save files.   */
/**************************************************/
#if WIN_BTC
#pragma argsused
#endif
static int ExitInterfaceRouter(
  void *theEnv,
  int num)
  {   
#if WIN_MCW
#pragma unused(theEnv)
#endif
  MSG msg;
   if (num >= 0) return(TRUE);
   
   //DoQuit();
   //AbortExit();
   //return(1);
   
   PostMessage(DialogWindow,WM_COMMAND,ID_APP_EXIT,0);
   exitInstance(&msg);
   return(FALSE); 
  }

/**********************************************************/
/* QueryInterfaceRouter: Router function which recognizes */
/*   I/O directed to the display window.                  */
/**********************************************************/
#if WIN_BTC
#pragma argsused
#endif
static intBool QueryInterfaceRouter(
  void *theEnv,
  char *logicalName)
  {
#if WIN_MCW
#pragma unused(theEnv)
#endif
   if ( (strcmp(logicalName,"stdout") == 0) ||
        (strcmp(logicalName,"stdin") == 0) ||
        (strcmp(logicalName,WPROMPT) == 0) ||
        (strcmp(logicalName,WTRACE) == 0) ||
        (strcmp(logicalName,WERROR) == 0) ||
        (strcmp(logicalName,WWARNING) == 0) ||
        (strcmp(logicalName,WDISPLAY) == 0) ||
        (strcmp(logicalName,WDIALOG) == 0) )
     { return(TRUE); }

    return(FALSE);
  }

/******************************************/
/* PrintInterfaceRouter: Router function  */
/*    which prints to the display window. */
/******************************************/
static int PrintInterfaceRouter(
  void *theEnv,
  char *logicalName,
  char *str)
  {
   FILE *fptr;

   fptr = FindFptr(theEnv,logicalName);
   if (fptr == stdout)
     { DisplayPrint(DialogWindow,str); }
   else
     { fprintf(fptr,"%s",str); }

   return(TRUE);
  }
  
/*******************************************/
/* GetcInterfaceRouter: Router function to */
/*   get input from the display window and */
/*   process other events.                 */
/*******************************************/
static int GetcInterfaceRouter(
  void *theEnv,
  char *logicalName)
  { 
   FILE *fptr;
   MSG msg;
   static int count = 0;

   fptr = FindFptr(theEnv,logicalName);
   if (fptr != stdin) return(getc(fptr));

   //UpdateCursor(QUESTION_CURSOR);
   SetCursor(LoadCursor(NULL,IDC_HELP));
   SetClassLong(DialogWindow,GCL_HCURSOR,(LONG) LoadCursor(NULL,IDC_HELP));

   GetMessage(&msg,NULL,0,0);
   TranslateMessage(&msg);

   while (TRUE)
     {  
      if (msg.message == WM_CHAR)
        {  
         switch(msg.wParam)
           {  
            case VK_BACK:
              GetUserCmd(DialogWindow,(WORD) msg.wParam,TRUE,(unsigned) count);
              count--;
              if (count < 0) count = 0;
              msg.wParam = '\b';
              break;

            case VK_RETURN:
              GetUserCmd(DialogWindow,(WORD) msg.wParam,TRUE,(unsigned) count);
              count = 0;
              //UpdateCursor(ARROW_CURSOR);
	  		  SetCursor(LoadCursor(NULL,IDC_ARROW));
		      SetClassLong(DialogWindow,GCL_HCURSOR,(LONG) LoadCursor(NULL,IDC_ARROW));
              msg.wParam = '\n';
              break;

            default:
              count++;
              GetUserCmd(DialogWindow,(WORD) msg.wParam,TRUE,(unsigned) count);
              break;
           }
           
         return((int) msg.wParam);
        }
      
      DispatchMessage(&msg);
      //UpdateCursor(QUESTION_CURSOR);
      SetCursor(LoadCursor(NULL,IDC_HELP));
	  SetClassLong(DialogWindow,GCL_HCURSOR,(LONG) LoadCursor(NULL,IDC_HELP));

      GetMessage(&msg,NULL,0,0);
      TranslateMessage(&msg);
     }
  }
  
/****************************************/
/* InterfaceEventFunction: Executes one */
/*   pass of the main program loop.     */
/****************************************/
#if WIN_BTC
#pragma argsused
#endif
static int InterfaceEventFunction(
  void *theEnv)
  {  
#if WIN_MCW
#pragma unused(theEnv)
#endif
   MSG msg;
   
     //UpdateCursor(ARROW_CURSOR);
	 //SetCursor(LoadCursor(NULL,IDC_ARROW));
	 //SetClassLong(DialogWindow,GCL_HCURSOR,(LONG) LoadCursor(NULL,IDC_ARROW));
   
   /*============================*/
   /* Update the status windows. */
   /*============================*/
  
   UpdateStatus();
   
   /*========================*/
   /* Update the menu items. */
   /*========================*/
   
   UpdateMenu(hMainFrame);

   /*========================*/
   /* Handle the next event. */
   /*========================*/
   
   GetMessage(&msg,NULL,0,0);
   if (! TranslateAccelerator(hMainFrame,haccel,&msg))
     {  
      TranslateMessage(&msg);
      DispatchMessage(&msg);
     }

   return(TRUE);
  }
  
/******************************************************/
/* WinRunEvent: Function which is called periodically */
/*   to update the interface while rules are firing   *
/*   or procedural code is executing.                 */
/******************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void WinRunEvent(
  void *theEnv)
  {  
#if WIN_MCW
#pragma unused(theEnv)
#endif
   MSG msg;

   UpdateStatus();
   UpdateMenu(hMainFrame);

   while (PeekMessage(&msg,NULL,0,0,PM_REMOVE))
     {  
      if (! TranslateAccelerator(hMainFrame,haccel,&msg))
        {  
         TranslateMessage(&msg);
	     DispatchMessage(&msg);
        }
     }
  }
  

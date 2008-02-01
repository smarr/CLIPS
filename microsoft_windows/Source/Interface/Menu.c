   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*       Microsoft Windows Version 3.01  06/15/03      */
   /*                                                     */
   /*                     MENU MODULE                     */
   /*******************************************************/

/**************************************************************/
/* Purpose: Contains the callback functions for items on the  */
/*      main menu.                                            */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Christopher J. Ortiz                                  */
/*                                                            */
/* Contributing Programmer(s):                                */
/*      Gary D. Riley                                         */
/*      Ernst Bokkelkamp                                      */
/*                                                            */
/* Revision History:                                          */
/*                                                            */
/*      6.24: Ernst's changes to use standard Windows         */
/*            cursors.                                        */
/**************************************************************/

#define _MENU_SOURCE_

/*-------------------------------+
| Windows & System Include Files |
+-------------------------------*/
#include <windows.h>
#include <windowsx.h>
//#include <stdlib.h>
//#include <string.h>

/*------------------------+
| CLIPS 6.0 Include Files |
+------------------------*/

#include "setup.h"

#include "agenda.h"
#include "classcom.h"
#include "commline.h"
#include "defins.h"
#include "dffctdef.h"
#include "dffnxfun.h"
#include "engine.h"
#include "filecom.h"
#include "genrccom.h"
#include "globldef.h"
#include "router.h"
#include "tmpltdef.h"

#undef GetFocus
/*
#include "evaluatn.h"
*/

/*------------------------+
| Interface Include Files |
+------------------------*/

#include "EditUtil.h"
#include "dialog1.h"

#include "display.h"
#include "Print.h"
#include "status.h"
#include "dialog2.h"
#include "menucmds.h"
#include "frame.h"
#include "Initialization.h"
#include "resource.h"

#include "menu.h"

#define SHIFTED 0x8000 

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static BOOL CALLBACK           AboutDlgProc (HWND,UINT,WPARAM,LPARAM);

/***********************************************/
/* DoExecutionChoice: Process all menu options */
/*    from the EXECUTION menu.                 */
/***********************************************/
void DoExecutionChoice( 
  HWND hwnd, 
  WORD wParam)
  { 
   char buffer[20];
   void *theEnv = GetCurrentEnvironment();

   switch (wParam)
     {   
      case ID_EXECUTION_PREFERENCES:
        DialogBox(GetWindowInstance(hwnd),"OptionDlg",hwnd,OptionDlgProc);
        break;
        
      case ID_EXECUTION_OPTIONS:
        DialogBox(GetWindowInstance(hwnd),"Exec_Option",hwnd,ExecDlg);
        break;

      case ID_EXECUTION_WATCH:
        DialogBox(GetWindowInstance(hwnd),"WatchDlg",hwnd,WatchDlgProc);
        break;

      case ID_EXECUTION_CLEAR:
        if (Warnings)
          {
           if (MessageBox(hwnd,"OK to clear CLIPS environment?",
                               "",MB_ICONQUESTION | MB_YESNO) == IDNO)
             { break; }
          }
        ClearCommandFromDisplay(DialogWindow,theEnv);
        EnvPrintRouter(theEnv,WPROMPT,"(clear)\n");
        SetCommandString(theEnv,"(clear)\n");
        break;

      case ID_EXECUTION_RESET:
        if ((EnvGetNextActivation(theEnv,NULL) != NULL) && Warnings)
          {
           if (MessageBox(hwnd,"There are activations on the agenda. OK to reset the CLIPS environment?",
                                 "",MB_ICONQUESTION | MB_YESNO) == IDNO)
             { break; }
          }
          
        ClearCommandFromDisplay(DialogWindow,theEnv);
        EnvPrintRouter(theEnv,WPROMPT,"(reset)\n");
        SetCommandString(theEnv,"(reset)\n");
        break;

       case ID_EXECUTION_RUN:   
        ClearCommandFromDisplay(DialogWindow,theEnv);
        EnvPrintRouter(theEnv,WPROMPT,"(run)\n");
        SetCommandString(theEnv,"(run)\n");
        break;

      case ID_EXECUTION_STEP:
        ClearCommandFromDisplay(DialogWindow,theEnv);
        sprintf(buffer,"(run %d)\n",RuleStep);
        SetCommandString(theEnv,buffer);
        EnvPrintRouter(theEnv,WPROMPT,buffer);
        break;

      case ID_EXECUTION_HALT:
        if (EngineData(GetCurrentEnvironment())->ExecutingRule != NULL)
          { EngineData(GetCurrentEnvironment())->HaltRules = CLIPS_TRUE; }
        else
          {
           SetHaltExecution(theEnv,CLIPS_TRUE);
           CloseAllBatchSources(theEnv);
          }
        break;

      case ID_EXECUTION_HALT_NOW:
        SetHaltExecution(theEnv,CLIPS_TRUE);
        CloseAllBatchSources(theEnv);
        break;
     }
  }

/********************************************/
/* DoWindowChoice: Process all menu options */
/*   from the WINDOW menu.                  */
/********************************************/
#if WIN_BTC
#pragma argsused
#endif
void DoWindowChoice(
  HWND hMain,
  WORD wParam)
  {
#if WIN_MCW
#pragma unused(hMain)
#endif
   switch (wParam)
     {
      case ID_WIN_SHOW_ALL: 
        SendMessage(FactsWindow,WM_MDIACTIVATE,0,(LPARAM) FactsWindow);
        SendMessage(AgendaWindow,WM_MDIACTIVATE,0,(LPARAM) AgendaWindow);
        SendMessage(InstancesWindow,WM_MDIACTIVATE,0,(LPARAM) InstancesWindow);
        SendMessage(GlobalsWindow,WM_MDIACTIVATE,0,(LPARAM) GlobalsWindow);
        SendMessage(FocusWindow,WM_MDIACTIVATE,0,(LPARAM) FocusWindow);
        break;

      case ID_WIN_HIDE_ALL:
        SendMessage(FactsWindow,WM_CLOSE,0,0);
        SendMessage(AgendaWindow,WM_CLOSE,0,0);
        SendMessage(InstancesWindow,WM_CLOSE,0,0);
        SendMessage(GlobalsWindow,WM_CLOSE,0,0);
        SendMessage(FocusWindow,WM_CLOSE,0,0);
        break;
      
      case ID_WIN_CLEAR:
        ClearDialogWnd();
        PrintPrompt(GetCurrentEnvironment());
        break;
     }  
     
   SetFocus(DialogWindow);
  }

/********************************************/
/* DoBrowseChoice: Process all menu options */
/*    from the BROWSE menu.                 */
/********************************************/
void DoBrowseChoice(
  HWND hwnd, 
  WORD wParam)
  {
   switch (wParam)
     {
#if DEFRULE_CONSTRUCT
      case ID_BROWSE_RULE:
        DialogBox(GetWindowInstance(hwnd),"List_Manager",hwnd,DefruleManager);
        break;

      case ID_BROWSE_AGENDA:
        DialogBox(GetWindowInstance(hwnd),"Agenda_Manager",hwnd,AgendaManager);
        break;
#endif

#if DEFFACTS_CONSTRUCT
      case ID_BROWSE_FACTS:
        DialogBox(GetWindowInstance(hwnd),"List_Manager",hwnd,(DLGPROC) DeffactsManager);
        break;
#endif

#if DEFTEMPLATE_CONSTRUCT
      case ID_BROWSE_TEMPLATE:
        DialogBox(GetWindowInstance(hwnd),"List_Manager",hwnd,DeftemplateManager);
        break;
#endif

#if DEFFUNCTION_CONSTRUCT
      case ID_BROWSE_FUNCTION:
        DialogBox(GetWindowInstance(hwnd),"List_Manager",hwnd,DeffunctionManager);
        break;
#endif

#if DEFGLOBAL_CONSTRUCT
      case ID_BROWSE_GLOBAL:
        DialogBox(GetWindowInstance(hwnd),"List_Manager",hwnd,DefglobalManager);
        break;
#endif

#if DEFGENERIC_CONSTRUCT
      case ID_BROWSE_GENERIC:
        DialogBox(GetWindowInstance(hwnd),"List_Manager",hwnd,DefgenericManager);
        break;
#endif


#if OBJECT_SYSTEM
      case ID_BROWSE_INSTANCES:
        DialogBox(GetWindowInstance(hwnd),"List_Manager",hwnd,DefinstancesManager);
        break;

      case ID_BROWSE_CLASS:
        DialogBox(GetWindowInstance(hwnd),"List_Manager",hwnd,DefclassManager);
        break;
#endif
     }
  }

/********************************************/
/* DoFileChoice: Process CLIPS related menu */
/*   options from the FILE menu.            */
/********************************************/
void DoFileChoice( 
  HWND hwnd,
  int id)  
  {  
   DWORD FlagSave;
   
   switch (id)
     {  
      /*------------------------+
      | Open/Close Dribble File |
      +------------------------*/
      
      case ID_FILE_DRIBBLE:
        OpenDribbleFile(hwnd,(WORD) id);
        break;

      /*-------------------------+
      | Save File as Binary File |
      +-------------------------*/
      
      case ID_FILE_SAVE_BINARY:
        SaveBinaryFile(hwnd);
        break;

      /*-----------------------------------+
      | Load a file (CLP, Binary or Batch) |
      +-----------------------------------*/
      
      case ID_FILE_LOAD:
      case ID_FILE_LOAD_BATCH:
      case ID_FILE_LOAD_BINARY:
        DoLoad(hwnd,(WORD) id);
        break;

      case ID_FILE_PAGE_SETUP:
	    FlagSave = PrintDialog.Flags;
        PrintDialog.Flags |= PD_PRINTSETUP;    
        PrintDlg((LPPRINTDLG) &PrintDialog);
        PrintDialog.Flags = FlagSave;
        break;
        
      /*---------------+
      | Start Shutdown |
      +---------------*/
      /*
      case IDM_FILE_QUIT:
      {  extern SCREEN WinFact, WinGlobal, WinInst, WinAgenda, WinDialog;
     HWND hEditor = FindWindow("ClipsEditWClass", NULL);

     if ( hEditor != NULL )
        PostMessage ( hEditor, WM_COMMAND, wParam, 0 );

     WinHelp ( WinDialog.hWnd, "CLIPS6.HLP", HELP_QUIT, 0);
     PrintCLIPS ("wclips","(exit)\n");
     DestroyWindow ( hMain );
     DestroyWindow ( WinFact.hWnd );
     DestroyWindow ( WinGlobal.hWnd);
     DestroyWindow ( WinInst.hWnd );
     DestroyWindow ( WinAgenda.hWnd );
     QuitDDE ( );
     SetCommandString("(exit)\n");

     ExitCLIPS(0);
      }
*/
   }
}

/******************************************/
/* DoHelpChoice: Process all menu options */ 
/*   from the Help menu.                  */
/******************************************/
void DoHelpChoice(
  HWND hwnd,
  WORD wParam)
  {  
   BOOL bGotHelp;
   TCHAR msg[256];
   
   switch (wParam)
     {   
      /*---------------------+
      | Show About WinDialog |
      +---------------------*/

      case ID_HELP_ABOUT:
        DialogBox(GetWindowInstance(hwnd),"AboutBox",hwnd,(DLGPROC) AboutDlgProc);
        break;

      case ID_HELP_CLIPS:        
        bGotHelp = WinHelp(hwnd,getHelpFileName("CLIPS6"),HELP_INDEX,(DWORD) 0) ;

        if (! bGotHelp)
          {
           LoadString(GetWindowInstance(hwnd),IDS_HELP_FAILED,
                      msg, DIM(msg));
           MessageBox((HWND) GetFocus(),msg,getAppName(),MB_OK|MB_ICONHAND);
          }     
        break;
     }   
  }

/*******************************************/
/* DoModuleChoice: Sets the current module */ 
/*   based on the menu item input.         */
/*******************************************/
void DoModuleChoice( 
  HMENU hMenu,
  WORD wParam)
  {
   void *theEnv = GetCurrentEnvironment();
   char moduleName[255];
   
   GetMenuString(hMenu,wParam,(LPSTR) &moduleName,255,MF_BYCOMMAND);
   EnvSetCurrentModule(theEnv,EnvFindDefmodule(theEnv,(char *) moduleName));
  }
  
/***********************************************/
/* UpdateModuleMenu: Will create the all popup */
/*   menu items for Module Menu.               */
/***********************************************/
void UpdateModuleMenu( 
  HMENU ModuleMenu)
  {  
   void *theEnv = GetCurrentEnvironment();
   char moduleTitle[255];
   unsigned position;
   struct defmodule *theModule;
   int value = GetMenuItemCount(ModuleMenu);

   /*=================================================*/
   /* Delete all Menu Items in the Module Popup Menu. */
   /*=================================================*/
   
   while (value)
     {  
      DeleteMenu(ModuleMenu,0,MF_BYPOSITION);
      value = GetMenuItemCount(ModuleMenu);
     }

   /*=================================================*/
   /* Create a new menu item for each defined module. */
   /*=================================================*/
   
   for (theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,NULL), position = IDM_MODULE_ONE;
        theModule != NULL;
        theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,theModule),position++)
     {  
      strncpy ((char *)moduleTitle,EnvGetDefmoduleName(theEnv,theModule),255);
      moduleTitle[254] = '\0';
      InsertMenu(ModuleMenu,(unsigned) GetMenuItemCount(ModuleMenu),MF_BYPOSITION|MF_STRING,position,moduleTitle);
      if (theModule == EnvGetCurrentModule(theEnv))
        { CheckMenuItem (ModuleMenu, position, MF_BYCOMMAND|MF_CHECKED); }
     }
  }
  
/************************************************************/
/* UpdateMenu: Function which will enable/disable interface */
/*   menu items based on rule execution and other factors.  */
/************************************************************/
void UpdateMenu(
  HWND hwnd)
  {      
   void *theEnv = GetCurrentEnvironment();
   HMENU hMenu = GetMenu(hwnd);
   static int value = 0;
   
   if (EnvDribbleActive(theEnv))
     { ModifyMenu(hMenu,ID_FILE_DRIBBLE,MF_BYCOMMAND,ID_FILE_DRIBBLE,"Turn &Dribble Off"); }
   else
     { ModifyMenu(hMenu,ID_FILE_DRIBBLE,MF_BYCOMMAND,ID_FILE_DRIBBLE,"Turn &Dribble On..."); }
     
   /*===================================*/
   /* Value - Flag used to execute code */
   /* once for each wait/active state.  */
   /*===================================*/

   if (CommandLineData(theEnv)->EvaluatingTopLevelCommand || BatchActive(theEnv))
     {  
      if (value)
        {  
         value = 0;

         /*==================================*/
         /* Gray File, Execute & Browse Menu */
         /* items when a rule is being fired */
         /*==================================*/
     
         EnableMenuItem(hMenu,ID_FILE_LOAD,MF_GRAYED);
         EnableMenuItem(hMenu,ID_FILE_LOAD_BINARY,MF_GRAYED);
         EnableMenuItem(hMenu,ID_FILE_SAVE_BINARY,MF_GRAYED);
         EnableMenuItem(hMenu,ID_FILE_LOAD_BATCH,MF_GRAYED);

         EnableMenuItem(hMenu,ID_HELP_COMPLETE, MF_BYCOMMAND|MF_GRAYED);

         EnableMenuItem(hMenu,ID_EXECUTION_RESET,MF_GRAYED);
         EnableMenuItem(hMenu,ID_EXECUTION_STEP,MF_GRAYED);
         EnableMenuItem(hMenu,ID_EXECUTION_CLEAR,MF_GRAYED);

         EnableMenuItem(hMenu,ID_BROWSE_RULE,MF_GRAYED);
         EnableMenuItem(hMenu,ID_BROWSE_FACTS,MF_GRAYED);
         EnableMenuItem(hMenu,ID_BROWSE_TEMPLATE,MF_GRAYED);
         EnableMenuItem(hMenu,ID_BROWSE_FUNCTION,MF_GRAYED);
         EnableMenuItem(hMenu,ID_BROWSE_GENERIC,MF_GRAYED);
         EnableMenuItem(hMenu,ID_BROWSE_GLOBAL,MF_GRAYED);
         EnableMenuItem(hMenu,ID_BROWSE_CLASS,MF_GRAYED);
         EnableMenuItem(hMenu,ID_BROWSE_INSTANCES,MF_GRAYED);
         EnableMenuItem(hMenu,ID_BROWSE_AGENDA,MF_GRAYED);
         
         if (DeleteMenu(hMenu,ID_EXECUTION_RUN,MF_BYCOMMAND))
           { 
            InsertMenu(hMenu,ID_EXECUTION_STEP,MF_BYCOMMAND,ID_EXECUTION_HALT,"&Halt\tCtrl+.");
            DrawMenuBar(hMainFrame);
           }

         //StartWaitCursor();
		 SetCursor(LoadCursor(NULL,IDC_WAIT));
		 SetClassLong(DialogWindow,GCL_HCURSOR,(LONG) LoadCursor(NULL,IDC_WAIT));
        }
      else
        {  
         /*==================================*/
         /* Update Halt and Halt Now menu    */
         /* items based on shift key status. */
         /*==================================*/
                    
         if (GetKeyState(VK_SHIFT) & SHIFTED)
           {  
            if (DeleteMenu(hMenu,ID_EXECUTION_HALT,MF_BYCOMMAND))
              { 
               InsertMenu(hMenu,ID_EXECUTION_STEP,MF_BYCOMMAND,ID_EXECUTION_HALT_NOW,"&Halt Now!!\tCtrl+Shift+.");
               DrawMenuBar(hMainFrame);
              }
           }
         else
           {
            if (DeleteMenu(hMenu,ID_EXECUTION_HALT_NOW,MF_BYCOMMAND))
              {  
               InsertMenu(hMenu,ID_EXECUTION_STEP,MF_BYCOMMAND,ID_EXECUTION_HALT,"&Halt\tCtrl+.");
               DrawMenuBar(hMainFrame);
              }
           }
        }
     }
   else
     {   
      if (! value)
        {  
         value = 1;
		 SetCursor(LoadCursor(NULL,IDC_WAIT));
		 SetClassLong(DialogWindow,GCL_HCURSOR,(LONG) LoadCursor(NULL,IDC_WAIT));

         /*=======================================*/
         /* Enable File, Execute & Browse Menu    */
         /* items when a rule is not being fired. */
         /*=======================================*/
         
         EnableMenuItem(hMenu,ID_FILE_LOAD,MF_ENABLED);
         EnableMenuItem(hMenu,ID_FILE_LOAD_BINARY,MF_ENABLED);
         EnableMenuItem(hMenu,ID_FILE_SAVE_BINARY,MF_ENABLED);
         EnableMenuItem(hMenu,ID_FILE_LOAD_BATCH,MF_ENABLED);

         EnableMenuItem(hMenu,ID_HELP_COMPLETE, MF_BYCOMMAND|MF_ENABLED);

         EnableMenuItem(hMenu,ID_EXECUTION_RESET,MF_ENABLED);
         EnableMenuItem(hMenu,ID_EXECUTION_STEP,MF_ENABLED);
         EnableMenuItem(hMenu,ID_EXECUTION_CLEAR,MF_ENABLED);
         
         if (DeleteMenu(hMenu,ID_EXECUTION_HALT,MF_BYCOMMAND))
           { 
            InsertMenu(hMenu,ID_EXECUTION_STEP,MF_BYCOMMAND,ID_EXECUTION_RUN,"&Run\tCtrl+R" ); 
            DrawMenuBar(hwnd);
           }
         else if (DeleteMenu(hMenu,ID_EXECUTION_HALT_NOW,MF_BYCOMMAND))
           {
            InsertMenu(hMenu,ID_EXECUTION_STEP,MF_BYCOMMAND,ID_EXECUTION_RUN,"&Run\tCtrl+R");
            DrawMenuBar(hwnd);
           }
         
         //StopWaitCursor();
		 SetCursor(LoadCursor(NULL,IDC_ARROW));
		 SetClassLong(DialogWindow,GCL_HCURSOR,(LONG) LoadCursor(NULL,IDC_ARROW));
        }

      /*-----------------------------------+
      | Update Browse Menu Items if needed |
      +-----------------------------------*/

      /*----------------+
      | PASTE Menu Item |
      +----------------*/
      /* 
      if ( IsClipboardFormatAvailable (CF_TEXT))
        { EnableMenuItem(hMenu,ID_EDIT_PASTE, MF_BYCOMMAND|MF_ENABLED); }
      else
        { EnableMenuItem(hMenu,ID_EDIT_PASTE, MF_BYCOMMAND|MF_GRAYED); }
      */
      /*-----------------+
      | AGENDA Menu Item |
      +-----------------*/
      
      if (EnvGetNextActivation(theEnv,NULL) != NULL )
        { EnableMenuItem(hMenu,ID_BROWSE_AGENDA,MF_BYCOMMAND|MF_ENABLED); }
      else
        { EnableMenuItem(hMenu,ID_BROWSE_AGENDA,MF_BYCOMMAND|MF_GRAYED); }

#if DEFGLOBAL_CONSTRUCT
       /*------------------+
       | DEFFACT Menu Item |
       +------------------*/
       if ( EnvGetNextDefglobal (theEnv,NULL) != NULL)
      EnableMenuItem(hMenu,ID_BROWSE_GLOBAL,MF_BYCOMMAND|MF_ENABLED);
       else
      EnableMenuItem(hMenu,ID_BROWSE_GLOBAL,MF_BYCOMMAND|MF_GRAYED);
#endif

#if DEFFACTS_CONSTRUCT
       /*------------------+
       | DEFFACT Menu Item |
       +------------------*/
       if ( EnvGetNextDeffacts (theEnv,NULL) != NULL)
      EnableMenuItem(hMenu,ID_BROWSE_FACTS,MF_BYCOMMAND|MF_ENABLED);
       else
      EnableMenuItem(hMenu,ID_BROWSE_FACTS,MF_BYCOMMAND|MF_GRAYED);
#endif

#if DEFRULE_CONSTRUCT
       /*------------------+
       | DEFRULE Menu Item |
       +------------------*/
       if (EnvGetNextDefrule(theEnv,NULL)!=NULL)
      EnableMenuItem(hMenu,ID_BROWSE_RULE,MF_BYCOMMAND|MF_ENABLED);
       else
      EnableMenuItem(hMenu,ID_BROWSE_RULE,MF_BYCOMMAND|MF_GRAYED);
#endif

#if DEFTEMPLATE_CONSTRUCT
       /*----------------------+
       | DEFTEMPLATE Menu Item |
       +----------------------*/
       if (EnvGetNextDeftemplate(theEnv,NULL) != NULL)
      EnableMenuItem(hMenu,ID_BROWSE_TEMPLATE, MF_BYCOMMAND|MF_ENABLED);
       else
      EnableMenuItem(hMenu,ID_BROWSE_TEMPLATE, MF_BYCOMMAND|MF_GRAYED);
#endif

#if DEFFUNCTION_CONSTRUCT
       /*----------------------+
       | DEFFUNCTION Menu Item |
       +----------------------*/
       if (EnvGetNextDeffunction(theEnv,NULL) != NULL)
      EnableMenuItem(hMenu,ID_BROWSE_FUNCTION, MF_BYCOMMAND|MF_ENABLED);
       else
      EnableMenuItem(hMenu,ID_BROWSE_FUNCTION, MF_BYCOMMAND|MF_GRAYED);
#endif

#if DEFGENERIC_CONSTRUCT
       /*---------------------+
       | DEFGENERIC Menu Item |
       +---------------------*/
       if ( EnvGetNextDefgeneric(theEnv,NULL)!= NULL)
      EnableMenuItem(hMenu,ID_BROWSE_GENERIC,  MF_BYCOMMAND|MF_ENABLED);
       else
      EnableMenuItem(hMenu,ID_BROWSE_GENERIC,  MF_BYCOMMAND|MF_GRAYED);
#endif

#if OBJECT_SYSTEM
#if DEFINSTANCES_CONSTRUCT
       /*----------------------+
       | DEFINSTANCE Menu Item |
       +----------------------*/
       if (EnvGetNextDefinstances (theEnv,NULL) != NULL )
      EnableMenuItem(hMenu,ID_BROWSE_INSTANCES,MF_BYCOMMAND|MF_ENABLED);
       else
      EnableMenuItem(hMenu,ID_BROWSE_INSTANCES,MF_BYCOMMAND|MF_GRAYED);
#endif

       /*-------------------+
       | DEFCLASS Menu Item |
       +-------------------*/
       if ( EnvGetNextDefclass(theEnv,NULL) != NULL )
      EnableMenuItem(hMenu,ID_BROWSE_CLASS,    MF_BYCOMMAND|MF_ENABLED);
       else
      EnableMenuItem(hMenu,ID_BROWSE_CLASS,    MF_BYCOMMAND|MF_GRAYED);
#endif
   }
}

/***************************************/
/* AboutDlgProc: Callback Function for */
/*   the About CLIPS Dialog Box.       */
/***************************************/
#if WIN_BTC
#pragma argsused
#endif
static BOOL CALLBACK AboutDlgProc(
  HWND hDlg,
  UINT message,
  WPARAM wParam,
  LPARAM lParam)
  {  
#if WIN_MCW
#pragma unused(lParam)
#endif
   switch (message)
     {  
      case WM_INITDIALOG:
        return (TRUE);

      case WM_COMMAND:
        {  
         switch (wParam)
           {
            case IDC_OK:
              EndDialog(hDlg, IDOK);
              return (TRUE);
           }
        }
     }
     
   return (FALSE);
  }

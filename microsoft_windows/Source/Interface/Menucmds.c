   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*       Microsoft Windows Version 3.0  01/31/02       */
   /*                                                     */
   /*                  MENU COMMANDS MODULE               */
   /*******************************************************/

/**************************************************************/
/* Purpose: Execute File/IO commands from the main menu as    */
/*   well as the command completion procedures.               */
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

#define _MENUCMDS_SOURCE_

/*-------------------------------+
| Windows & System Include Files |
+-------------------------------*/
#include <windows.h>
#include <windowsx.h>

/*------------------------+
| CLIPS 6.0 Include Files |
+------------------------*/

#include "setup.h"
#include "commline.h"
#include "filecom.h"
#include "router.h"

/*------------------------+
| Interface Include Files |
+------------------------*/

#include "Initialization.h"
#include "resource.h"
#include "dialog1.h"
#include "dialog2.h"
#include "display.h"
#include "menucmds.h"

/**********************************************************************/
/* DoLoad: Function will display the common file dialog and will load */
/*    a CLIPS rule file, CLIPS binary file, or a CLIPS batch file.    */
/**********************************************************************/
void DoLoad( 
  HWND hMain, 
  WORD LoadType)
  {  
   OPENFILENAME ofn;
   char File[256], FileTitle[256], Filter[256];
   UINT i;
   int cbString;
   char Replace;
   int x;
   size_t size;
   void *theEnv = GetCurrentEnvironment();

   File[0] = '\0';
   memset(&ofn,0,sizeof(OPENFILENAME));

   /*==============================================*/
   /* Initialize Common Dialog based on File Type. */
   /*==============================================*/
   
   switch(LoadType)
     {  
      case ID_FILE_LOAD:
        ofn.lpstrTitle = "Load CLIPS Constructs";
        if ((cbString = LoadString(GetWindowInstance(hMain),IDS_RULES,Filter,sizeof(Filter))) == 0)
          { return; }
	    break;

      case ID_FILE_LOAD_BATCH:
        ofn.lpstrTitle = "Load CLIPS Batch File";
        if ((cbString = LoadString(GetWindowInstance(hMain),IDS_BATCH,Filter,sizeof(Filter))) == 0)
	      { return; }
	    break;
      
      case ID_FILE_LOAD_BINARY:
        ofn.lpstrTitle = "Load CLIPS Binary File";
        if ((cbString = LoadString(GetWindowInstance(hMain),IDS_BINARY,Filter,sizeof(Filter))) == 0)
	      { return; }
        break;
     }

   Replace = Filter[cbString-1];

   for (i = 0; Filter[i] != '\0'; i++)
     {
      if (Filter[i] == Replace)
        { Filter[i] = '\0'; }
     }

   ofn.lStructSize = sizeof ( OPENFILENAME );
   ofn.hwndOwner = hMain;
   ofn.lpstrFilter = Filter;
   ofn.nFilterIndex = 1;
   ofn.lpstrFile = File;
   ofn.nMaxFile = sizeof (File );
   ofn.lpstrFileTitle = FileTitle;
   ofn.nMaxFileTitle = sizeof (FileTitle);
   ofn.lpstrInitialDir = NULL;
   ofn.Flags = OFN_HIDEREADONLY | OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST;

   /*==========================*/
   /* Perform the file dialog. */
   /*==========================*/
   
   if (! GetOpenFileName(&ofn))
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

   ClearCommandFromDisplay(DialogWindow,theEnv);
   
   /*======================*/
   /* Issue CLIPS Command. */
   /*======================*/

   switch(LoadType)
     {  
      case ID_FILE_LOAD:
        SetCommandString(theEnv,"(load \"");  
        break;
         
      case ID_FILE_LOAD_BATCH:
        SetCommandString(theEnv,"(batch \""); 
        break;
       
      case ID_FILE_LOAD_BINARY:
        SetCommandString(theEnv,"(bload \"");
        break;
     }
      
   AppendCommandString(theEnv,ofn.lpstrFile );
   AppendCommandString(theEnv,"\")\n");
   EnvPrintRouter(theEnv,WPROMPT,GetCommandString(theEnv));
  }

/*******************************************/
/* DoCommandCompletion: This function will */
/*   complete a specified string.          */
/*******************************************/
BOOL DoCommandCompletion(
  HWND hwnd,
  char *buffer,
  int fromEditor)
  {
   unsigned int numberOfMatches = 0;
   int RtnValue = TRUE;
   unsigned int commonPrefixLength = 0;
   DLGPROC theDlgProc;
     
   //HorizScroll = 1; ???
   GlobalMatches = FindSymbolMatches(GetCurrentEnvironment(),buffer,&numberOfMatches,
                                            &commonPrefixLength);

   switch (numberOfMatches)
     {  
      case 0:
        CompleteString[0] = '\0';
        MessageBeep(0);
        RtnValue = FALSE;
        break;

      case 1:
        strncpy(CompleteString,(GlobalMatches->match->contents),255);
        break;

      default:
        /* Option from Preferences */
        if (fromEditor && (! Complete))
          {  
           MessageBeep(0);
           return (FALSE);
          }

        theDlgProc = MakeProcInstance((DLGPROC) CommandComplete,GetWindowInstance(hwnd));
        if (DialogBoxParam((HINSTANCE) GetWindowInstance(hwnd),"List_Manager", hwnd, theDlgProc,(LPARAM)buffer) == IDC_CANCEL)
          { RtnValue = FALSE; }
     }  

   ReturnSymbolMatches(GetCurrentEnvironment(),GlobalMatches);
   
   return (RtnValue);
  }
 
/*****************************************************/
/* OpenDribbleFile: Function will display the common */
/*   file dialog and will open a dribble file.       */
/*****************************************************/
void OpenDribbleFile( 
  HWND hMain, 
  WORD wParam)
  {  
   OPENFILENAME ofn;
   char File[256], FileTitle[256], Filter[256];
   UINT i;
   int cbString;
   char Replace;
   int x;
   size_t size;
   void *theEnv = GetCurrentEnvironment();
   HMENU hMenu = GetMenu(hMain);

   if (! EnvDribbleActive(theEnv))
     {  
      sprintf(File,"dribble.txt");
      memset ( &ofn,0, sizeof (OPENFILENAME));
      ofn.lpstrTitle = "Select CLIPS Dribble File";
      
      if ((cbString = LoadString(GetWindowInstance(hMain),IDS_TEXT_FILES,Filter,sizeof(Filter))) == 0)
        { return; }

      Replace = Filter[cbString-1];
      for (i=0; Filter[i] != '\0'; i++)
        {
         if (Filter[i] == Replace)
           { Filter[i] = '\0'; }
        }

      ofn.lStructSize = sizeof (OPENFILENAME);
      ofn.hwndOwner = hMain;
      ofn.lpstrFilter = Filter;
      ofn.nFilterIndex = 1;
      ofn.lpstrFile = File;
      ofn.nMaxFile = sizeof (File );
      ofn.lpstrFileTitle = FileTitle;
      ofn.nMaxFileTitle = sizeof (FileTitle);
      ofn.lpstrInitialDir = NULL;
      ofn.Flags = OFN_OVERWRITEPROMPT | OFN_HIDEREADONLY;
      
      /*==========================*/
      /* Perform the file dialog. */
      /*==========================*/
   
      if (! GetOpenFileName(&ofn))
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

      /*=====================================*/
      /* Issue the CLIPS dribble-on command. */
      /*=====================================*/

      ModifyMenu(hMenu,wParam,MF_BYCOMMAND,wParam,"Turn Dribble Off");
      ClearCommandFromDisplay(DialogWindow,theEnv);
      SetCommandString(theEnv,"(dribble-on \"");  
      AppendCommandString(theEnv,ofn.lpstrFile);
      AppendCommandString(theEnv,"\")\n");
      EnvPrintRouter(theEnv,WPROMPT,GetCommandString(theEnv));
     }
   else
     {  
      /*======================================*/
      /* Issue the CLIPS dribble-off command. */
      /*======================================*/

      ModifyMenu(hMenu,wParam,MF_BYCOMMAND,wParam,"Turn Dribble On...");  
      ClearCommandFromDisplay(DialogWindow,theEnv);
      SetCommandString(theEnv,"(dribble-off)\n");  
      EnvPrintRouter(theEnv,WPROMPT,GetCommandString(theEnv));
     }
  }
   
/*********************************************************/
/* SaveBinaryFile: Function will display the common file */
/*    dialog and will save a CLIPS binary file.          */
/*********************************************************/
#if WIN_BTC
#pragma argsused
#endif
void SaveBinaryFile(
  HWND hMain)
  {  
   char File[256], FileTitle[256], Filter[256];
   UINT i;
   int cbString;
   char Replace;
   OPENFILENAME ofn;
   int x;
   size_t size;
   void *theEnv = GetCurrentEnvironment();

   File[0] = '\0';
   memset ( &ofn,0, sizeof (OPENFILENAME));
   ofn.lpstrTitle = "Save CLIPS File as Binary";
   if ((cbString = LoadString (GetWindowInstance(hMain), IDS_BINARY, Filter, sizeof (Filter))) == 0 )
      return;

   Replace = Filter[cbString-1];

   for (i=0; Filter[i] != '\0'; i++)
     if ( Filter[i] == Replace)
        Filter[i] = '\0';

   ofn.lStructSize = sizeof ( OPENFILENAME );
   ofn.hwndOwner = hMain;
   ofn.lpstrFilter = Filter;
   ofn.nFilterIndex = 1;
   ofn.lpstrFile = File;
   ofn.nMaxFile = sizeof (File );
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

   /*======================*/
   /* Issue CLIPS Command. */
   /*======================*/
   
   ClearCommandFromDisplay(DialogWindow,theEnv);
   SetCommandString(theEnv,"(bsave \"");  
   AppendCommandString(theEnv,ofn.lpstrFile);
   AppendCommandString(theEnv,"\")\n");
   EnvPrintRouter(theEnv,WPROMPT,GetCommandString(theEnv));
  }

   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*       Microsoft Windows Version 3.0  01/31/02       */
   /*                                                     */
   /*                     TEXT MODULE                     */
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

#define _TEXT_SOURCE_

#include <windows.h>
#include <stdio.h>

#include "setup.h"

#include "router.h"

#include "StdSDK.h"     // Standard application includes
#include <tchar.h>
#include "Initialization.h"       // For non-static function prototypes
#include "Frame.h"      // For non-static function prototypes
#include "resource.h"   // For resource identifiers
#include "mdi.h"
#include "findwnd.h"
#include "edit.h"
#include "EditUtil.h"
#include "Balance.h"
#include "Print.h"
#include "SearchDialog.h"
#include "Text.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    text_OnClose(HWND);
   static void                    text_OnCommand(HWND,int,HWND,UINT);
   static BOOL                    text_OnContextMenu(HWND,HWND,int,int);
   static HBRUSH                  text_OnCtlColorEdit(HWND,HDC,HWND,int);
   static void                    text_OnDestroy(HWND hwnd);
   static void                    text_OnMDIActivate(HWND,BOOL,HWND,HWND);
   static BOOL                    text_OnQueryEndSession(HWND hwnd);
   static void                    text_OnSize(HWND,UINT,int,int);
   static void                    text_OnInitMenuPopup(HWND,HMENU,UINT,BOOL);
   static void                    text_OnSetFocus(HWND,HWND);
   static void                    text_OnUpdateMenu(HWND,HMENU);
   static BOOL                    PrintFile(HWND);
   static void                    FixTextLineEndings(HWND);
   
//=============================================================================
// These values are initialized once in InitInstance and destroyed in
// ExitInstance
//=============================================================================

   
//=============================================================================
// These macros are used to access the WindowExtra components of our class

#define EXTRA_BYTES 0
#define MAXFILENAME 256 /* maximum length of file pathname */

struct textWindowData
  {
   char fileName[MAXFILENAME];
  };

char szTemp[128];      /* Temp Buffer */
// BOOL     bAbort = FALSE;
//HWND     hAbortDlgWnd = NULL;

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   globle ATOM                 EditAtomClass;

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static HMENU                TextMenu = NULL;
   static HMENU                TextWindowMenu = NULL;
   static HMENU                TextContextMenu = NULL;

/*****************************************************/
/* textWndProc: Message dispatcher for text windows. */
/*****************************************************/
static LRESULT CALLBACK textWndProc(
  HWND hwnd, 
  UINT message, 
  WPARAM wParam, 
  LPARAM lParam)
  {
   switch(message)
     { 
	  HANDLE_MSG(hwnd,WM_CLOSE, text_OnClose);
      HANDLE_MSG(hwnd,WM_COMMAND, text_OnCommand);
	  HANDLE_MSG(hwnd,WM_CONTEXTMENU, text_OnContextMenu);
      HANDLE_MSG(hwnd,WM_CTLCOLOREDIT, text_OnCtlColorEdit);
      HANDLE_MSG(hwnd,WM_DESTROY, text_OnDestroy);
      HANDLE_MSG(hwnd,WM_INITMENUPOPUP,text_OnInitMenuPopup);
      HANDLE_MSG(hwnd,WM_MDIACTIVATE,text_OnMDIActivate);
	  HANDLE_MSG(hwnd,WM_QUERYENDSESSION, text_OnQueryEndSession);
      HANDLE_MSG(hwnd,WM_SETFOCUS, text_OnSetFocus);
      HANDLE_MSG(hwnd,WM_SIZE, text_OnSize);
         
      case UWM_UPDATE_MENU:
        text_OnUpdateMenu(hwnd,GetMenu(hMainFrame));
        text_OnUpdateMenu(hwnd,TextContextMenu);
        return 0;
       
      case UWM_CONTEXTMENU:
        return HANDLE_WM_CONTEXTMENU(hwnd,wParam,lParam, 
		 					         text_OnContextMenu);
     }
   
   return DefMDIChildProc(hwnd, message, wParam, lParam);
  }

/****************************************************************************
*                             text_InitInstance
* Inputs:
*       HINSTANCE hinst: Instance handle
* Result: BOOL
*       TRUE if successful
*       FALSE if error
* Effect: 
*       Registers the text window class, and loads its menu to a local
*       static variable.
****************************************************************************/
BOOL text_InitInstance(
  HINSTANCE hinst)
  {
   EditAtomClass = registerMDIChild(hinst,0,IDR_TEXT,textWndProc,EXTRA_BYTES);
   
   if (! EditAtomClass)
     { return FALSE; }

   TextMenu = LoadMenu(hinst,MAKEINTRESOURCE(IDR_CLIPS));

   if (TextMenu == NULL)
     { return FALSE; }

   TextContextMenu = LoadMenu(hinst,MAKEINTRESOURCE(IDR_TEXT_CONTEXT));
   
   TextWindowMenu = findWindowMenu(TextMenu);

   /* Make these the frame menu. */
   
   hMainMenu = TextMenu;

   return TRUE;
  }

/****************************************************************************
*                             text_ExitInstance
* Result: void
*       
* Effect: 
*       Does whatever cleanup is necessary.  In this case, destroys the
*       menu handle.
****************************************************************************/
void text_ExitInstance()
  {
   DestroyMenu(TextMenu);
   DestroyMenu(TextContextMenu);
  }

/****************************************************************************
* text_New
* Inputs:
*       HWND hwnd: Parent window
* Result: BOOL
*       HWND if successful
*       NULL if failure
* Effect: 
*       Creates a new text window
****************************************************************************/
HWND text_New(
  HWND hwnd,
  char *fileName)
  {
   HWND hwndChild;
   static int titleIndex = 1;
   char titleBuffer[20];
   
   if (fileName == NULL)
     { 
      sprintf(titleBuffer,"Untitled%d",titleIndex++);
      hwndChild = mdi_Create(MDIClientWnd,0,IDR_TEXT,titleBuffer,
                             CW_USEDEFAULT,0,CW_USEDEFAULT,0); 
     }
   else
     { 
      hwndChild = mdi_Create(MDIClientWnd,0,IDR_TEXT,fileName,
                             CW_USEDEFAULT,0,CW_USEDEFAULT,0);
     }

   if (hwndChild != NULL)
     { /* success */
      HWND hedit;

	  hedit = edit_New(hwndChild,WS_CHILD | 
	 			                 WS_VISIBLE | 
                                 ES_MULTILINE | 
                                 ES_WANTRETURN |
                                 ES_AUTOVSCROLL |
                                 WS_VSCROLL, TRUE);
	  if (hedit == NULL)
	    { /* no edit window */
	     DestroyWindow(hwndChild);
	     return NULL;
	    } /* no edit window */
	  
	  PostMessage(hwnd,UWM_UPDATE_MENU,0,0);
     } /* success */

   return (hwndChild);
  }

/****************************************/
/* text_Open: Creates a new text window */
/*   and loads the contents of a file.  */
/****************************************/
BOOL text_Open(
  HWND hwnd)
  {
   HWND ewnd;
   
   /*===========================*/
   /* Use standard open dialog. */
   /*===========================*/

   if (! GetOpenFileName((LPOPENFILENAME) &ofn))
     { return FALSE; }
     
   /*=========================*/
   /* Create the edit buffer. */
   /*=========================*/
   
   if ((ewnd = text_New(hwnd,szFileName)) == NULL)
     { return(FALSE); }
     
   /*========================*/
   /* Read in the file data. */
   /*========================*/
   
   text_Revert(hwnd,szFileName,ewnd);
   
   return(TRUE);
  }

/***********************************************/
/* text_Revert: Reverts a text window to the   */
/*   original contents of its associated file. */
/***********************************************/
BOOL text_Revert(
  HWND hwnd,
  char *fileName,
  HWND hEditWnd)
  {
   int hFile;
   OFSTRUCT OfStruct;
   long text_length;
   char *pEditBuffer;
   HICON hSaveCursor;
   unsigned ioStatus;
   char szTemp[128];
   struct textWindowData *theData;
   
   theData = (struct textWindowData *) GetWindowLong(hEditWnd,GWL_USERDATA);
 
   
   /*===================================*/
   /* Open the file and get its handle. */
   /*===================================*/
   
   /* TBD Replace OpenFile with CreateFile. */
   
   if (theData == NULL)
     {
      hFile = OpenFile((LPSTR) fileName,
                       (LPOFSTRUCT) &OfStruct,
                       OF_READ);
     }
   else
     {
      hFile = OpenFile((LPSTR) (char *) &theData->fileName,
                       (LPOFSTRUCT) &OfStruct,
                       OF_READ);
     }
     
   if (hFile == HFILE_ERROR)
     { return (FALSE); }
 
   /*=====================*/
   /* Update window data. */
   /*=====================*/
   
   if (theData == NULL)
     { 
      theData = (struct textWindowData *) malloc(sizeof(struct textWindowData));
      if (theData == NULL) return(FALSE);
      
      strcpy((char *) &theData->fileName,(char *) fileName);
      SetWindowLong(hEditWnd,GWL_USERDATA,(long) theData);
     }
   
   /*===================================================*/
   /* Allocate edit buffer to the size of the file + 1. */
   /*===================================================*/
   
   text_length = _llseek(hFile,0,2); 
   _llseek(hFile,0,0); 

   if (text_length > 65534L) 
     {
      MessageBox(hwnd, "Can't load files larger than 65,534 bytes long !",
                 "FILE READ ERROR", MB_OK | MB_ICONEXCLAMATION);
      _lclose(hFile);
      return (FALSE);
     }

   pEditBuffer = (char *) malloc((size_t) text_length+1);

   if (pEditBuffer == NULL)
     {
      MessageBox(hwnd,"Not enough memory.",
                 NULL,MB_OK | MB_ICONEXCLAMATION);
      _lclose(hFile);
      return (FALSE);
     }

   hSaveCursor = SetCursor(hHourGlass);
        
   ioStatus = _lread(hFile,pEditBuffer,(UINT) text_length);
   pEditBuffer[text_length] = '\0';
   _lclose(hFile); 

   /*====================================*/
   /* # bytes read must equal file size. */
   /*====================================*/

   if (((long) ioStatus) != text_length)
	 {
      sprintf(szTemp,"Error reading %s tl %ld IO %ld.",
	          fileName,text_length,ioStatus);
      SetCursor(hSaveCursor);      
      MessageBox(hwnd,szTemp,NULL,MB_OK | MB_ICONEXCLAMATION);
     }
   else
     { 
      hEditWnd = GetDlgItem(hEditWnd,ID_EDIT_CONTROL);
      SendMessage(hEditWnd,WM_SETTEXT,0,(LPARAM) pEditBuffer);
      FixTextLineEndings(hEditWnd);
     }

   free(pEditBuffer);
    
   /*=======================================*/
   /* Set up a new buffer and window title. */
   /*=======================================*/	 
     
   //sprintf(szTemp, "Edit File - (%s)", szFileTitle);
   //SetNewBuffer(hWnd,(LPSTR)&szTemp);
   SetCursor(hSaveCursor);
   
   return(TRUE);
  }


/*********************************************/
/* text_SaveAs: Saves the contents a text    */
/*   window to a file specified by the user. */
/*********************************************/
BOOL text_SaveAs(
  HWND hwnd)
  {
   struct textWindowData *oldData, *theData;
   HWND hEditWnd;
   
   hEditWnd = hwnd;
      
   /*====================================*/
   /* Get rid of the current window data */
   /* so the save dialog will appear.    */
   /*====================================*/
   
   oldData = (struct textWindowData *) GetWindowLong(hEditWnd,GWL_USERDATA);
   SetWindowLong(hEditWnd,GWL_USERDATA,(long) NULL);
   
   /*================*/
   /* Save the file. */
   /*================*/
   
   text_Save(hwnd);
    
   /*=========================================*/
   /* Restore the old window data if the save */
   /* was cancelled, otherwise get rid of the */
   /* old window data.                        */
   /*=========================================*/
   
   theData = (struct textWindowData *) GetWindowLong(hEditWnd,GWL_USERDATA);
   if (theData != NULL) 
     {
      SetWindowText(hEditWnd,szFileName);
      if (oldData != NULL)
        { free(oldData); }
     }
   else
     { SetWindowLong(hEditWnd,GWL_USERDATA,(long) oldData); }
   
   return(TRUE);
  }
     
/****************************************/
/* text_Save: Saves the contents a text */
/*   window to its associated file.     */
/****************************************/
BOOL text_Save(
  HWND hwnd)
  {
   HWND hEditWnd;
   struct textWindowData *theData;
   HFILE hFile;
   OFSTRUCT OfStruct;
   char szTemp[398];
   int text_length; 
   char *pEditBuffer;
   HICON hSaveCursor;
   unsigned ioStatus;
   BOOL bSuccess;
   
   //hEditWnd = GetFirstChild(MDIClientWnd);
   hEditWnd = hwnd;
   
   /*======================*/
   /* Get the window data. */
   /*======================*/
   
   theData = (struct textWindowData *) GetWindowLong(hEditWnd,GWL_USERDATA);
   if (theData == NULL)
     {
	  if (! GetSaveFileName ((LPOPENFILENAME) &ofn))
        { return(FALSE); }
        
      theData = (struct textWindowData *) malloc(sizeof(struct textWindowData));
      if (theData == NULL) 
        { return(FALSE); }
        
      strcpy((char *) &theData->fileName,(char *) szFileName);
      SetWindowLong(hEditWnd,GWL_USERDATA,(long) theData);
      SetWindowText(hEditWnd,szFileName);
     }

   if ((hFile = OpenFile(theData->fileName, &OfStruct, OF_CANCEL | OF_CREATE)) < 0)
     {  
      sprintf(szTemp, "Cannot write to %s.",theData->fileName);
      MessageBox(hwnd,szTemp,NULL,MB_OK | MB_ICONHAND);
      return (FALSE);
     }
     
   hEditWnd = GetDlgItem(hEditWnd,ID_EDIT_CONTROL);
   text_length = SendMessage(hEditWnd,WM_GETTEXTLENGTH,0,0);
   pEditBuffer = (char *) malloc((size_t) text_length+1);

   if (pEditBuffer == NULL)
     {
      sprintf(szTemp, "Not enough memory to save %s.", theData->fileName);
      MessageBox(hwnd, szTemp, NULL, MB_OK | MB_ICONHAND);
      _lclose(hFile);
      return (FALSE);
	 }

   SendMessage(hEditWnd,WM_GETTEXT,(unsigned) text_length+1,(LPARAM) pEditBuffer);

   /*==========================================================*/
   /* Set the cursor to an hourglass during the file transfer. */
   /*==========================================================*/

   hSaveCursor = SetCursor(hHourGlass);
   
   ioStatus = _lwrite(hFile,pEditBuffer,strlen(pEditBuffer));

   _lclose(hFile);

   SetCursor(hSaveCursor);

   if (ioStatus != strlen(pEditBuffer))
     { 
      sprintf(szTemp, "Error writing to %s.", theData->fileName);
      MessageBox(hwnd, szTemp, NULL, MB_OK | MB_ICONHAND);
      bSuccess = FALSE;
     }
   else
     {  
      bSuccess = TRUE;     
      SendMessage(hEditWnd,EM_SETMODIFY,
                  (WPARAM) (UINT) FALSE, (LPARAM) 0);
     }
     
   free(pEditBuffer);
   return (bSuccess);
  }

/***************************/
/* text_OnMDIActivate:  */
/******************************/
#if WIN_BTC
#pragma argsused
#endif
static void text_OnMDIActivate(
  HWND hwnd, 
  BOOL active, 
  HWND hActivate, 
  HWND hDeactivate)
  {
#if WIN_MCW
#pragma unused(hActivate)
#pragma unused(hDeactivate)
#endif
   CheckMenuItem(TextMenu,GetWindowWord(hwnd,0), 
                 (unsigned) (active ? MF_CHECKED : MF_UNCHECKED));
                 
   if (active)
     {
      text_OnUpdateMenu(hwnd,GetMenu(hMainFrame));
      if (FORWARD_WM_MDISETMENU(MDIClientWnd,TRUE,TextMenu,
                                TextWindowMenu,SendMessage) != 0)
        {
         DrawMenuBar(hMainFrame);
         PostMessage(hMainFrame,UWM_UPDATE_TOOLBAR,0,0);
        }
     }
  }

/******************************************************/
/* text_OnClose: If the contents have changed, prompt */
/*   the user for confirmation; otherwise just close  */
/*   the window.                                      */
/******************************************************/
static void text_OnClose(
  HWND hwnd)
  {
   int result;
   
   result = text_QueryClose(hwnd);
   
   if (result == IDCANCEL)
     { return; }
     
   if (result == IDYES)
     { text_Save(hwnd); }
     
   FORWARD_WM_CLOSE(hwnd,DefMDIChildProc);
  }

/****************************************************************************
*                             text_OnDestroy
* Inputs:
*       HWND hwnd:
*       HWND destroy:
* Result: void
*       
* Effect: 
*       After forwarding the message to the default procedure, posts a
*       message to the main window suggesting that it should check for
*       the proper menu being posted.  
****************************************************************************/
static void text_OnDestroy(
  HWND hwnd)
  {
   struct textWindowData *theData;
   
   theData = (struct textWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   if (theData != NULL)
     { free(theData); }
 
   FORWARD_WM_DESTROY(hwnd, DefMDIChildProc);
   PostMessage(hMainFrame, UWM_MDI_DESTROY, 0, 0);
  }

/****************************************************************************
*                                 text_OnFont
* Inputs:
*       HWND  hwnd: Window handle
* Result: void
*       
* Effect: 
*       Changes the font of the window
****************************************************************************/
static void text_OnFont(
  HWND hwnd)
  {
   edit_ChooseFont(GetDlgItem(hwnd, ID_EDIT_CONTROL));
  }

/****************************************************************************
*                                 text_OnEdit
* Inputs:
*       HWND hwnd: Window handle
*       int id: Child window ID (ignored, we only have one edit control)
*       HWND hctl: Child window handle
*       UINT codeNotify: which event happened?
* Result: void
*       
* Effect: 
*       When changes occur that affect possible menu/toolbar settings, cause
*       the objects to update
****************************************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void text_OnEdit(
  HWND hwnd, 
  int id, 
  HWND hctl, 
  UINT codeNotify)
  {
#if WIN_MCW
#pragma unused(hctl)
#pragma unused(id)
#endif
   switch(codeNotify)
     { 
      case EN_CHANGE:
        PostMessage(hwnd, UWM_UPDATE_MENU, 0, 0);
        return;
     }
  }

/**********************************/
/* text_OnCommand: Processes menu */
/*   items for editing windows.   */
/**********************************/
static void text_OnCommand(
  HWND hwnd, 
  int id, 
  HWND hctl, 
  UINT codeNotify)
  {	 
   switch (id) 
	 {
      case ID_EDIT_SET_FONT:
        text_OnFont(hwnd);
        return;

      case ID_EDIT_BALANCE:
        Balance(GetDlgItem(hwnd,ID_EDIT_CONTROL));
        return;

      case ID_EDIT_COMMENT:
        DoComment(GetDlgItem(hwnd,ID_EDIT_CONTROL));
        return;

      case ID_EDIT_UNCOMMENT:
        DoUncomment(GetDlgItem(hwnd,ID_EDIT_CONTROL));
        return;
        
      case ID_HELP_COMPLETE:
        EditComplete(GetDlgItem(hwnd,ID_EDIT_CONTROL));
        return;
       
      case ID_EDIT_UNDO:
        FORWARD_WM_UNDO(GetDlgItem(hwnd, ID_EDIT_CONTROL), 
		 						SendMessage);
        return;
        
      case ID_EDIT_CUT:
        FORWARD_WM_CUT(GetDlgItem(hwnd, ID_EDIT_CONTROL), 
		 						SendMessage);
        return;
         
      case ID_EDIT_COPY:
        FORWARD_WM_COPY(GetDlgItem(hwnd, ID_EDIT_CONTROL), 
		 						SendMessage);
        return;
        
      case ID_EDIT_PASTE:
        FORWARD_WM_PASTE(GetDlgItem(hwnd, ID_EDIT_CONTROL), 
		 						SendMessage);
		FixTextLineEndings(GetDlgItem(hwnd, ID_EDIT_CONTROL));
        return;
         
      case ID_EDIT_CLEAR:
        FORWARD_WM_CLEAR(GetDlgItem(hwnd, ID_EDIT_CONTROL), 
		 						SendMessage);
        return;
         
      case ID_EDIT_SELECT_ALL:
        Edit_SetSel(GetDlgItem(hwnd, ID_EDIT_CONTROL), 0, -1);
        return;
                
      case ID_EDIT_CONTROL:
        text_OnEdit(hwnd,id,hctl,codeNotify);
        return;

      case ID_BUFFER_FIND:
	    SetUpSearch(GetDlgItem(hwnd,ID_EDIT_CONTROL),0);
        return;
        
      case ID_BUFFER_REPLACE:
	    SetUpSearch(GetDlgItem(hwnd,ID_EDIT_CONTROL),1);
        return;
        
      case ID_FILE_PRINT:
	    PrintFile(hwnd);
	    return;
	    
      case ID_FILE_SAVE:
        text_Save(hwnd);
        return;

      case  ID_FILE_SAVE_AS:
        text_SaveAs(hwnd);
        return;
        
      case ID_BUFFER_LOAD:
      case ID_BUFFER_BATCH:
      case ID_BUFFER_LOAD_BUFFER:
        LoadBatchBufferSelection(GetDlgItem(hwnd,ID_EDIT_CONTROL),id);
        return;
	 } 
   
   FORWARD_WM_COMMAND(hwnd, id, hctl, codeNotify, DefMDIChildProc);
  }

/*****************************************/
/* text_OnSize: Resizes the edit control */
/*   to fit in the client area.          */
/*****************************************/
static void text_OnSize(
  HWND hwnd, 
  UINT state, 
  int cx, 
  int cy)
  {
   if (state != SIZEICONIC)
     { MoveWindow(GetDlgItem(hwnd, ID_EDIT_CONTROL), 0, 0, cx, cy, TRUE); }
     
   FORWARD_WM_SIZE(hwnd, state, cx, cy, DefMDIChildProc);
  }

/**************************************/
/* text_OnInitMenuPopup: Enables cut, */
/*   copy, paste, etc.                */
/**************************************/
#if WIN_BTC
#pragma argsused
#endif
static void text_OnInitMenuPopup(
  HWND hwnd, 
  HMENU hmenu, 
  UINT item, 
  BOOL sysmenu)
  {
#if WIN_MCW
#pragma unused(sysmenu)
#pragma unused(hmenu)
#pragma unused(item)
#endif
   PostMessage(hwnd,UWM_UPDATE_MENU, 0, 0);
  }

/***********************************************************/ 
/* text_OnUpdateMenu: Updates the menu (for InitMenuPopup) */
/*   and updates the toolbar as well.                      */  
/***********************************************************/
static void text_OnUpdateMenu(
  HWND hwnd, 
  HMENU hmenu)
  {
   HWND hedit = GetDlgItem(hwnd,ID_EDIT_CONTROL);

   EnableMenuItem(hmenu,ID_FILE_CLOSE,MF_ENABLED);

   edit_UpdateMenu(hedit,hmenu);
     
   PostMessage(hMainFrame,UWM_UPDATE_TOOLBAR,0,0);
  }

/***********************************************/
/* text_OnSetFocus: Sets focus to edit control */
/*   which is contained inside the MDI child.  */
/***********************************************/
#if WIN_BTC
#pragma argsused
#endif
static void text_OnSetFocus(
  HWND hwnd,
  HWND oldfocus)
  {
#if WIN_MCW
#pragma unused(oldfocus)
#endif
   SetFocus(GetDlgItem(hwnd,ID_EDIT_CONTROL));
  }

/****************************************************************************
*			      text_OnCtlColorEdit
* Inputs:
*       HWND hwnd: Window handle of parent window
*       HDC hdc: Display context for child window
*       HWND hchild: Window handle of child window
*       int type: CTLCOLOR_EDIT is the only value we care about
* Result: HBRUSH
*       Background brush, COLOR_BACKGROUND, always
*       NULL, always -- use default brush for class
* Effect:
*       Sets the text color
****************************************************************************/
#if WIN_BTC
#pragma argsused
#endif
static HBRUSH text_OnCtlColorEdit(
  HWND hwnd,
  HDC hdc,
  HWND hchild,
  int type)
  {
#if WIN_MCW
#pragma unused(type)
#pragma unused(hwnd)
#endif
   return FORWARD_WM_CTLCOLOREDIT(hchild, hdc, hchild, SendMessage);
  }

/********************************************************/
/* text_OnContextMenu: Pops up a context-specific menu. */
/********************************************************/
static BOOL text_OnContextMenu(
  HWND hwnd, 
  HWND hwndCtl, 
  int xPos, 
  int yPos)
  {
   return mdi_OnContextMenu(hwnd,hwndCtl,xPos,yPos,TextContextMenu);
  }

/****************************************************************************
*                               text_QueryClose
* Inputs:
*       HWND hwnd: Window handle
* Result: BOOL
*       TRUE if window can close
*	FALSE if it can't
* Effect: 
*       If there is a change in the edit control, pops up a message box
*	asking for confirmation.
****************************************************************************/
int text_QueryClose(
  HWND hwnd)
  {
   BOOL modified = Edit_GetModify(GetDlgItem(hwnd,ID_EDIT_CONTROL));
   TCHAR caption[256];
   TCHAR changed[256];

   if (! modified)
	 { return IDNO; }
	 
   VERIFY(LoadString(GetWindowInstance(hwnd), IDS_SAVE_CHANGES_CAPTION,
     				caption, DIM(caption)));
   VERIFY(LoadString(GetWindowInstance(hwnd), IDS_SAVE_CHANGES,
     				changed, DIM(changed)));

   // We use a more general structure here so that in the future, we
   // could issue a box that said "Save changes [Yes][No][Cancel]" and
   // do a multiway branch based on the three possible return values.

   return (MessageBox(hwnd, changed, caption, MB_ICONSTOP | MB_YESNOCANCEL));
  }

/****************************************************************************
*                           text_OnQueryEndSession
* Inputs:
*       HWND hwnd: Window handle
* Result: BOOL
*       TRUE if session can be terminated
*	FALSE if session must not terminate
* Effect:
*       If necessary, pops up a MessageBox indicating text window has changed
****************************************************************************/
static BOOL text_OnQueryEndSession(HWND hwnd)
  {
   return text_QueryClose(hwnd);
  }

/****************************************************************/
/* PrintFile: Procedure to send editor buffer out to a printer. */
/****************************************************************/
static BOOL PrintFile(
  HWND hWnd)
  {
   char title[128];

   GetWindowText(hWnd,title,127);

   PrintWindowDriver(GetWindowInstance(hMainFrame),hWnd,title);

   return(TRUE);
  }
  
/*************************************/
/* FixTextLineEndings:  */
/*************************************/
static void FixTextLineEndings(
  HWND hEditWnd)
  {  
   long sel;
   size_t i, textLength;
   int originalStart, originalEnd;
   char *fullText;
   char lastCharIsLineFeed = FALSE;
   
   /*==============================*/
   /* Save the original selection. */
   /*==============================*/

   sel = SendMessage(hEditWnd,EM_GETSEL,0,0);
   originalStart = (int) LOWORD(sel);
   originalEnd = (int) HIWORD(sel);

   /*==========================*/
   /* Get all of the text data */
   /*==========================*/
   
   textLength = (size_t) SendMessage(hEditWnd,WM_GETTEXTLENGTH,0,0);
   fullText = (char *) malloc(textLength+1);
   if (fullText == NULL )
     { return; }

   SendMessage(hEditWnd,WM_GETTEXT,textLength+1,(LPARAM) fullText);

   /*===============================*/
   /* Insert carriage returns and   */
   /* line feeds where appropriate. */
   /*===============================*/

   for (i = textLength - 1; i > 0; i--)
     {
      if ((fullText[i] == '\r') &&
          (! lastCharIsLineFeed))
        {
         SendMessage(hEditWnd,EM_SETSEL,(WPARAM) i+1,i+1);   
         SendMessage(hEditWnd,EM_REPLACESEL,0,(LPARAM) "\n");
        }
      else if ((fullText[i] != '\r') &&
               lastCharIsLineFeed)
        {       
         SendMessage(hEditWnd,EM_SETSEL,(WPARAM) i+1,i+1);   
         SendMessage(hEditWnd,EM_REPLACESEL,0,(LPARAM) "\r");
        }
         
      if (fullText[i] == '\n')
        { lastCharIsLineFeed = TRUE; }
      else
        { lastCharIsLineFeed = FALSE; }
        
      if (i == 0) break;
     }
     
   if (lastCharIsLineFeed)
     {
      SendMessage(hEditWnd,EM_SETSEL,(WPARAM) 0,0);   
      SendMessage(hEditWnd,EM_REPLACESEL,0,(LPARAM) "\r");
     }

   /*===================*/
   /* Release the text. */
   /*===================*/
   
   free(fullText);
   
   /*=================================*/
   /* Restore the original selection. */
   /*=================================*/

   SendMessage(hEditWnd,EM_SETSEL,(WPARAM) originalStart,originalEnd);
   
   Edit_SetModify(hEditWnd,FALSE);
  }

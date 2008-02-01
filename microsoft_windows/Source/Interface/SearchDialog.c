   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  11/29/07            */
   /*                                                     */
   /*                 SEARCH DIALOG MODULE                */
   /*******************************************************/

/**************************************************************/
/* Purpose: To preform all operations required by the         */
/*          Search and Replace Menu Items                     */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Christopher J. Ortiz                                  */
/*                                                            */
/* Contributing Programmer(s):                                */
/*      Search/Replace adapted from CView Search/Replace      */
/*      Algorithm. Eastern Language Systems, Provo, UT        */
/*                                                            */
/* Revision History:                                          */
/*      6.30: Renamed Search.c and Search.h to SearchDialog.c */
/*            and SearchDialog.h because of conflicts with    */
/*            the Borland search.h header file.               */
/**************************************************************/

#define _SEARCHDIALOG_SOURCE_

/*-------------------------------+
| Windows & System Include Files |
+-------------------------------*/
#include <windows.h>
#include <string.h>
#include <commdlg.h>
#include <stdio.h>
#include <stdlib.h> 

/************************/
/* Editor Include Files */
/************************/

#include "setup.h"
#include "SearchDialog.h"
#include "Frame.h"
#include "Resource.h"

/********************/
/* Global Variables */
/********************/

FINDREPLACE fr;
UINT uFindReplaceMsg;
BOOL FirstSearch = TRUE;
char SearchFor [255];
char ReplaceWith [255];
HWND SearchDlg = NULL;
BOOL SearchActive = FALSE;

/*----------------+
| Local Functions |
+----------------*/
void DoSearch ( HWND, int, int, int);
char *stristr ( char*, char* );

/********************************************/
/* InitFindReplace: Initializes information */
/*   for find/replace dialogs.              */
/********************************************/
void InitFindReplace(
  HWND  hwnd)
  { 
   uFindReplaceMsg = RegisterWindowMessage(FINDMSGSTRING);

   memset(&SearchFor,'\0',255 );
   memset(&ReplaceWith,'\0',255);
   memset(&fr,0,sizeof(FINDREPLACE));

   fr.lStructSize = sizeof(FINDREPLACE);
   fr.hwndOwner = hwnd;
   fr.lpstrFindWhat = (LPSTR) &SearchFor;
   fr.lpstrReplaceWith = (LPSTR) &ReplaceWith;
   fr.wFindWhatLen = 255;
   fr.wReplaceWithLen = 255;
   fr.Flags = FR_HIDEUPDOWN | FR_HIDEWHOLEWORD;
  }

/************************************************/
/* SetUpSearch: Will display the common dialog  */
/*   for search & replace as well as preventing */
/*   the dialogs from being called twice.       */
/************************************************/
void SetUpSearch( 
  HWND hWnd, 
  int Replace)
  {  
   /*========================*/
   /* Call the common dialog */
   /* for Search or Replace. */
   /*========================*/
   
   fr.hwndOwner = hWnd;
   fr.Flags = FR_HIDEUPDOWN | FR_HIDEWHOLEWORD ;

   if (Replace)
     { SearchDlg = ReplaceText(&fr); }
   else
     { SearchDlg = FindText(&fr); }

   /*=====================*/
   /* Disable Menu Items. */
   /*=====================*/

   if (SearchDlg)
     {  
      HMENU hMenu = GetMenu(hMainFrame);
      
      EnableMenuItem(hMenu,ID_BUFFER_FIND,MF_GRAYED);
      EnableMenuItem(hMenu,ID_BUFFER_REPLACE,MF_GRAYED);
      
      FirstSearch = TRUE;
      SearchActive = TRUE;
     }
  }

/**********************************************************/
/* StartSearch: Call back procedure for the common Dialog */
/*   Box to do the shut down, search or replace.          */
/**********************************************************/
#if WIN_BTC
#pragma argsused
#endif
int StartSearch(
  HWND hWnd,
  WPARAM wParam,
  LONG lParam)
  {  
#if WIN_MCW
#pragma unused(wParam)
#endif
   FINDREPLACE FAR *lpfr;

   lpfr = (FINDREPLACE FAR *) lParam;

   /*==========================================*/
   /* Terminate Find/Replace Dialog if needed. */
   /*==========================================*/
   
   if (lpfr->Flags & FR_DIALOGTERM)
     {  
      HMENU hMenu = GetMenu(hMainFrame);

      EnableMenuItem(hMenu,ID_BUFFER_FIND,MF_ENABLED);
      EnableMenuItem(hMenu,ID_BUFFER_REPLACE,MF_ENABLED);
      SearchActive = FALSE;
      return 0;
	 }

   /*===========================================*/
   /* Perform the actual search and/or Replace. */
   /*===========================================*/
   
   ShowWindow (SearchDlg,SW_HIDE );
   DoSearch(hWnd,
		    ((lpfr->Flags & FR_REPLACE) || (lpfr->Flags & FR_REPLACEALL)),
            (int) (lpfr->Flags & FR_REPLACEALL),
            (int) (lpfr->Flags & FR_MATCHCASE) );
   ShowWindow(SearchDlg,SW_SHOW);
   SetFocus(hWnd);
   return 0;
  }

/***************************************************************
* DoSearch: Scans the edit buffer for mach items and can replace
*   items if needed.
****************************************************************/
#if WIN_BTC
#pragma argsused
#endif
void DoSearch(
  HWND hWnd,
  int Replace,
  int ReplaceAll,
  int MatchCase)
  {  
   static char * pBase = NULL;
   static char * pFound = NULL;
   int loc;
   size_t searchLen, replaceLen;
   size_t text_length;
   char *pEditBuffer;
   int count = 0;
   WORD Temp;
   
   HWND hEditWnd = hWnd;

   if (FirstSearch)
     { pFound = NULL; }
   FirstSearch = FALSE;

   searchLen = strlen(SearchFor);
   if (!searchLen)
	 {  
	  MessageBeep(0);
      return;
	 }

   do
     {
      text_length = (size_t) SendMessage(hEditWnd,WM_GETTEXTLENGTH,0,0);
      pEditBuffer = (char *) malloc(text_length+1);
      if (pEditBuffer == NULL)
        {
         MessageBeep(0);
         MessageBox(hEditWnd,"Can not complete operation",
					"Memory Low", MB_ICONSTOP | MB_TASKMODAL );
         return;
		}

      SendMessage(hEditWnd,WM_GETTEXT,text_length+1,
						(LPARAM) pEditBuffer);

      /*==================*/
      /* Text replacement */
      /*==================*/
		
      if (! pFound)
        { pBase = NULL; }
      else
        { 
         replaceLen = strlen(ReplaceWith);
	     if ((ReplaceAll || Replace) && replaceLen )
	       {  
	        SendMessage(hEditWnd,EM_REPLACESEL,0,(LPARAM) ReplaceWith);
            count++;
            
            free(pEditBuffer);
            text_length = (size_t) SendMessage(hEditWnd,WM_GETTEXTLENGTH,0,0);
            pEditBuffer = (char *) malloc(text_length+1);
            
            if (pEditBuffer == NULL)
              {
               MessageBeep(0);
               MessageBox(hEditWnd,"Can not complete operation",
                          "Memory Low", MB_ICONSTOP | MB_TASKMODAL );
               return;
		      }

            SendMessage(hEditWnd,WM_GETTEXT,text_length+1,
						(LPARAM) pEditBuffer);
           }
        }

      /*=======================*/
      /* Search for next match */
      /*=======================*/

      Temp = HIWORD(SendMessage(hEditWnd, EM_GETSEL, 0, 0L));
      pBase = pEditBuffer + Temp;
      
      if ( MatchCase )
	    { pFound = strstr (pBase,SearchFor); }
      else
        { pFound = stristr( pBase, SearchFor ); }

      if (pFound != NULL)
        {
         loc = pFound - pEditBuffer;

         SendMessage(hEditWnd,EM_SETSEL,(WPARAM) loc,(LPARAM) (loc + searchLen));
         SendMessage(hEditWnd,EM_SCROLLCARET,(WPARAM) 0,(LPARAM) 0L);
        }
      else
	    { MessageBeep(0); }

      free(pEditBuffer);
	 } 
   while ( pFound && ReplaceAll);

   if (ReplaceAll)
     {  
      char Buffer[20];
      sprintf((char *) &Buffer,"%d Items Replaced",count);
      MessageBox ( hEditWnd, Buffer,"",MB_ICONINFORMATION | MB_OK);
	 }
   
   SetFocus(hEditWnd);
  }

/******************************************************************
* stristr: Finds the first occurrence of one substring in another
*   without case sensitivity.
********************************************************************/
char *stristr(
  char *source,
  char *target)
  {  
   size_t targetSize = strlen(target);
   size_t sourceSize = strlen(source);
   char *pStr;
   int notfound = 1;

   for (pStr = source;
      (pStr <= (source + sourceSize - targetSize)) && notfound;
      pStr++)
#if WIN_BTC
     { notfound = strnicmp(pStr,target,targetSize); }
#endif
#if WIN_MCW || WIN_MVC
     { notfound = (int) _strnicmp(pStr,target,targetSize); }
#endif
#if (! WIN_BTC) && (! WIN_MCW) && (! WIN_MVC)
     { notfound = strnicmp(pStr,target,targetSize); }
#endif

   if (notfound)
     { return (NULL); }
   else
     { return (pStr-1); }
  }







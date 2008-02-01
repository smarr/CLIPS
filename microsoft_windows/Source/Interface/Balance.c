   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*       Microsoft Windows Version 3.02  06/15/04      */
   /*                                                     */
   /*                   BALANCE MODULE                    */
   /*******************************************************/

/**************************************************************/
/* Purpose: Handle the Balance Command within an Edit Window  */
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

#include <windows.h>
#include <windowsx.h>
#include <stdlib.h>

#include "setup.h"

#include "commline.h"
#include "cstrcpsr.h"
#include "filecom.h"
#include "router.h"
#include "strngrtr.h"

#include "Balance.h"
#include "dialog2.h"
#include "menucmds.h"
#include "frame.h"
#include "display.h"
#include "resource.h"

static void BalanceIt(int, int, int, int, char*, DWORD, HWND );

/**************************************************/
/* EditComplete: Handles Editor Complete Command. */
/**************************************************/
void EditComplete(
  HWND hWindow)
  {
   long start = 0, end = 0;
   char *buffer;
   char *completeStr;
   int lineBegin, iLength;
   unsigned theLine;
   /* Macintosh Version. */

   SendMessage(hWindow,EM_GETSEL,(WPARAM) &start,(LPARAM) &end);

   theLine = (unsigned) SendMessage(hWindow,EM_LINEFROMCHAR,(WPARAM) end,0);

   lineBegin = SendMessage(hWindow,EM_LINEINDEX,theLine,0);
   
   iLength = end - lineBegin;
   buffer = (char *) malloc((size_t) iLength + 1);
   
   if (buffer == NULL) return;
   
   *(int *) buffer = iLength;
   
   SendMessage(hWindow,EM_GETLINE,theLine,(LPARAM) buffer);
   buffer[iLength] = 0;
   
   completeStr = GetCommandCompletionString(GetCurrentEnvironment(),buffer,
                                            (unsigned) iLength);
   free(buffer);
     
   if (DoCommandCompletion(hWindow,(char *) completeStr,1))
     {
      if (strcmp((char *) CompleteString,"\0") != 0)
        { 
         start = (long) (end - strlen(completeStr));
         SetFocus(hWindow);
         SendMessage(hWindow,EM_SETSEL,(WPARAM) start,end);   
         SendMessage(hWindow,EM_REPLACESEL,0,(LPARAM) CompleteString);
         SendMessage(hWindow,EM_SCROLLCARET,(WPARAM) 0,(LPARAM) 0L);
        }
     }
  }

/*************************************/
/* Balance: Handles Balance Command. */
/*************************************/
void Balance(
  HWND hWindow)
  {  
   DWORD sel;
   int left_middle, right_middle;
   size_t text_length;
   char *text_ptr;

   /*===================================*/
   /* Get information about the current */
   /* selection to be balanced.         */
   /*===================================*/

   sel = (DWORD) SendMessage(hWindow,EM_GETSEL,0,0);
   left_middle = (int) LOWORD(sel);
   right_middle= (int) HIWORD(sel);
    
   text_length = (size_t) SendMessage(hWindow,WM_GETTEXTLENGTH,
                                      0,0);
   text_ptr = (char *) malloc(text_length+1);
   if (text_ptr == NULL )
     {
      MessageBeep ( 0 );
      MessageBox (hWindow,"Can not complete operation",
                    "Memory Low", MB_ICONSTOP | MB_TASKMODAL );
      return;
     }

   SendMessage(hWindow,WM_GETTEXT,text_length+1,
                    (LPARAM) text_ptr);

   /*===================================*/
   /* If the selection is empty then... */
   /*===================================*/

   if ( left_middle == right_middle )
     {  
      /*---------------------------------------------+
      | if the '(' is to the right of the cursor,    |
      | then all balancing should occur to the right.|
      +---------------------------------------------*/
     
      if ((text_ptr[left_middle] == '(') && (left_middle < (int) text_length))
        {  
         BalanceIt ( left_middle-1, left_middle+1, 1, 0,
                     text_ptr, text_length, hWindow );
        }
     
      /*-------------------------------------------+
      |Else if ')' is to the left of the cursor,   |
      |then all balancing should occur to the left |
      +-------------------------------------------*/
     
      else if ((left_middle > 0) ? (text_ptr[left_middle-1] == ')'): 0)
        {  
         BalanceIt ( left_middle-2, right_middle, // GDR
                             0, -1, text_ptr, text_length, hWindow );
        }
 
      /*------------------------------------------------------+
      |Else balancing occurs to the left and right of cursor |
      +------------------------------------------------------*/
   
      else
        {  
         BalanceIt (left_middle-1, right_middle, 0, 0,
                    text_ptr, text_length, hWindow );
        }
     }
   else
     { 
      /*----------------------------------------------+
      | Determine the number of right parentheses ')' |
      |  that need to be balanced from the left side. |
      +----------------------------------------------*/

      int count, i;
      int left_count, right_count;

      count = 0;
      left_count = 0;

      for ( i = left_middle; i < right_middle; i++ )
        {  
         if (text_ptr[i] == '(') count ++;
         else if ( text_ptr[i] == ')') count --;
         
         if ( count < left_count) left_count = count;
        }

      /*----------------------------------------------+ 
      | Determine the number of right parentheses ')' |
      |  that need to be balanced from the left side. |
      +----------------------------------------------*/
      
      count = 0;
      right_count = 0;

      for ( i = right_middle-1; i >= left_middle; i--)
        {  
         if ( text_ptr[i] == '(') count++;
         else if ( text_ptr[i] == ')') count--;
         
         if ( count > right_count) right_count = count;
        }

      /*--------------------------------------------+
      | Balance to the left and right of the cursor |
      +--------------------------------------------*/

      BalanceIt ( left_middle-1, right_middle, left_count, right_count,
                  text_ptr, text_length, hWindow );
     }

   free(text_ptr);
  }

/*******************************************************
* BalanceIt: Balances a selection of text by extending 
*    extending it to the left and right until the      
*    number of left and right parentheses is balanced. 
********************************************************/
static void BalanceIt( 
  int left_middle, 
  int right_middle,
  int left_count, 
  int right_count,
  char *text_ptr,
  DWORD text_length,
  HWND hWindow)
  { 
   /*-----------------------------+
   | Balance the left side of the |
   | text by moving left and up   |
   +-----------------------------*/
   
   while ( left_count <= 0 )
     {  
      if ( left_middle < 0 )
        {  
         MessageBeep(0);
         return;
        }

      if (text_ptr[left_middle] == '(') left_count++;
      else if (text_ptr[left_middle] == ')') left_count--;

      left_middle--;
     }

   /*-------------------------------+
   | Balance the right side of the  |
   | text by moving right and down. |
   +-------------------------------*/
   
   while ( right_count >= 0)
     {  
      if ( right_middle > (int) text_length )
        { 
         MessageBeep(0);
         return;
        }

      if (text_ptr[right_middle] == '(') right_count ++;
      else if ( text_ptr[right_middle] == ')') right_count --;

      right_middle++;
     }

   /*-------------------------------------------+
   | Set the current selection to balanced text |
   +-------------------------------------------*/
   
   SendMessage(hWindow,EM_SETSEL,
                (WPARAM) left_middle+1,right_middle);

   /*-----------------------------------+
   | Make sure the selection is visible |
   +-----------------------------------*/
   
   SendMessage(hWindow,EM_SCROLLCARET,(WPARAM) 0,(LPARAM) 0L);
   /*
   SendMessage (hWindow, EM_LINESCROLL, 0,
                MAKELPARAM (0, -(GetScrollPos ( hWindow, SB_HORZ))));
   */
  }

/*************************************/
/* LoadBatchBufferSelection:  */
/*************************************/
void LoadBatchBufferSelection(
  HWND hEditWnd,
  int command)
  {  
   long sel;
   size_t textLength;
   int originalStart, originalEnd, start;
   size_t length;
   char *fullText, *tempText;
   size_t x, p;
   void *theEnv = GetCurrentEnvironment();

   /*==============================*/
   /* Save the original selection. */
   /*==============================*/
   
   sel = SendMessage(hEditWnd,EM_GETSEL,0,0);
   originalStart = (int) LOWORD(sel);
   originalEnd = (int) HIWORD(sel);
    
   /*===========================*/
   /* Select the entire buffer. */
   /*===========================*/

   if (command == ID_BUFFER_LOAD_BUFFER )
     { SendMessage(hEditWnd,EM_SETSEL,0,-1); }
     
   /*=================================*/
   /* Find the area of selected text. */
   /*=================================*/

   sel = SendMessage(hEditWnd,EM_GETSEL,0,0L);
   length = (size_t) (HIWORD(sel) - LOWORD(sel));
   start = LOWORD(sel);

   /*==========================*/
   /* Get all of the text data */
   /*==========================*/
   
   textLength = (size_t) SendMessage(hEditWnd,WM_GETTEXTLENGTH,0,0);
   fullText = (char *) malloc(textLength+1);
   if (fullText == NULL )
     {
      MessageBeep(0);
      MessageBox(hEditWnd,"Can not complete operation",
                    "Memory Low", MB_ICONSTOP | MB_TASKMODAL );
      return;
     }

   SendMessage(hEditWnd,WM_GETTEXT,textLength+1,(LPARAM) fullText);

   tempText = (char *) malloc(length+1);
   if (tempText == NULL )
     {
      free(fullText);
      MessageBeep(0);
      MessageBox(hEditWnd,"Can not complete operation",
                 "Memory Low", MB_ICONSTOP | MB_TASKMODAL );
      return;
     }

   /*=================================================*/
   /* Strip out the carriage returns from the editing */
   /* buffer. The display window uses only line feeds */
   /* to determine where the line ends occur.         */
   /*=================================================*/
    
   for (x = 0, p = 0; x < length; x++)
     {
      if (fullText[start+x] != '\r')
        { tempText[p++] = fullText[start+x]; }
     }

   tempText[p] = '\0';
      
   free(fullText);
   fullText = tempText;
   
   /*=================================*/
   /* Restore the original selection. */
   /*=================================*/

   SendMessage(hEditWnd,EM_SETSEL,(WPARAM) originalStart,originalEnd);

   /*=======================================*/
   /* Bring the dialog window to the front. */
   /*=======================================*/
   
   FORWARD_WM_MDIACTIVATE(MDIClientWnd,FALSE,NULL,DialogWindow,SendMessage);
    
   if (command == ID_BUFFER_BATCH)
     {  
      OpenStringBatch(GetCurrentEnvironment(),"BatchSelection",fullText,FALSE);
     }
   else
     {  
      ClearCommandFromDisplay(DialogWindow,theEnv);
      EnvPrintRouter(theEnv,WPROMPT,"Loading Selection...\n");
      FlushCommandString(theEnv);
      OpenStringSource(theEnv,"clipread",fullText,0);
      SetPrintWhileLoading(theEnv,TRUE);
      LoadConstructsFromLogicalName(theEnv,"clipread");
      SetPrintWhileLoading(theEnv,FALSE);
      CloseStringSource(theEnv,"clipread");
      PrintPrompt(theEnv);
      free(fullText);
     }
  }
  
/*********************************************/
/* DoComment: Handles the Comment menu item. */
/*********************************************/
void DoComment(
  HWND hEditWnd)
  {
   long start = 0, end = 0;
   int startLine, endLine, theLine;
   int lineBegin;
   
   /*=============================================*/
   /* Determine the start and end lines selected  */
   /* or the line containing the insertion point. */
   /*=============================================*/
   
   SendMessage(hEditWnd,EM_GETSEL,(WPARAM) &start,(LPARAM) &end);
   startLine = SendMessage(hEditWnd,EM_LINEFROMCHAR,(WPARAM) start,0);
   endLine = SendMessage(hEditWnd,EM_LINEFROMCHAR,(WPARAM) end,0);

   /*===============================================*/
   /* Add a semicolon (the comment marker) to the   */
   /* beginning of each line in the selected range. */
   /*===============================================*/
   
   SetFocus(hEditWnd);
   for (theLine = startLine; theLine <= endLine; theLine++)
     {
      lineBegin = SendMessage(hEditWnd,EM_LINEINDEX,(WPARAM) theLine,0);

      SendMessage(hEditWnd,EM_SETSEL,(WPARAM) lineBegin,lineBegin);   
      SendMessage(hEditWnd,EM_REPLACESEL,0,(LPARAM) ";");
     }

   /*=======================================*/
   /* Select all the text on the lines that */
   /* had the comment character added.      */
   /*=======================================*/
   
   start = SendMessage(hEditWnd,EM_LINEINDEX,(WPARAM) startLine,0);
   end = SendMessage(hEditWnd,EM_LINEINDEX,(WPARAM) endLine + 1,0);
   SendMessage(hEditWnd,EM_SETSEL,(WPARAM) start,end-1);   
   SendMessage(hEditWnd,EM_SCROLLCARET,(WPARAM) 0,(LPARAM) 0L);
  }
  
/*************************************************/
/* DoUncomment: Handles the Uncomment menu item. */
/*************************************************/
void DoUncomment(
  HWND hEditWnd)
  {
   long start = 0, end = 0;
   int startLine, endLine, theLine;
   char buffer[32];

   /*=============================================*/
   /* Determine the start and end lines selected  */
   /* or the line containing the insertion point. */
   /*=============================================*/

   SendMessage(hEditWnd,EM_GETSEL,(WPARAM) &start,(LPARAM) &end);
   startLine = SendMessage(hEditWnd,EM_LINEFROMCHAR,(WPARAM) start,0);
   endLine = SendMessage(hEditWnd,EM_LINEFROMCHAR,(WPARAM) end,0);
   
   /*==================================================*/
   /* Remove a semicolon (the comment marker) from the */
   /* beginning of each line in the selected range.    */
   /*==================================================*/

   for (theLine = startLine; theLine <= endLine; theLine++)
     {
      *(int *) buffer = 1;
   
      SendMessage(hEditWnd,EM_GETLINE,(WPARAM) theLine,(LPARAM) buffer);
      if (buffer[0] == ';')
        {
         start = SendMessage(hEditWnd,EM_LINEINDEX,(WPARAM) theLine,0);
         SendMessage(hEditWnd,EM_SETSEL,(WPARAM) start,start+1);   
         SendMessage(hEditWnd,EM_REPLACESEL,0,(LPARAM) "");
        }
     }

   /*=======================================*/
   /* Select all the text on the lines were */
   /* checked for the comment character.    */
   /*=======================================*/

   start = SendMessage(hEditWnd,EM_LINEINDEX,(WPARAM) startLine,0);
   end = SendMessage(hEditWnd,EM_LINEINDEX,(WPARAM) endLine + 1,0);
   SendMessage(hEditWnd,EM_SETSEL,(WPARAM) start,end-1);   
   SendMessage(hEditWnd,EM_SCROLLCARET,(WPARAM) 0,(LPARAM) 0L);
  }


   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*       Microsoft Windows Version 3.0  01/31/02       */
   /*                                                     */
   /*                    PRINT MODULE                     */
   /*******************************************************/

/**************************************************************/
/* Purpose: Provides basic routines for printing.             */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Gary D. Riley                                         */
/*                                                            */
/* Contributing Programmer(s):                                */
/*                                                            */
/* Revision History:                                          */
/*                                                            */
/**************************************************************/

#include <windows.h>
#include <windowsx.h>

#define _PRINT_SOURCE_

#include "setup.h"

#include "Frame.h"
#include "Edit.h"
#include "Text.h"
#include "Status.h"
#include "display.h"
#include "resource.h"

#include "Print.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static BOOL CALLBACK           PrintDlgProc(HWND,UINT,WPARAM,LPARAM);
   static BOOL CALLBACK           AbortProc(HDC,int);
   static BOOL                    PrintIt(int,int,int,int,int,LPCTSTR,HWND);

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   globle PRINTDLG             PrintDialog;
   globle BOOL                 bUserAbort;
   globle HWND                 hDlgPrint;
   
   static DOCINFO  di = { sizeof (DOCINFO), "", NULL } ;

/***************************************************/
/* InitializePrintDialog: Fills in the non-variant */
/*   fields of PRINTDLG structure.                 */
/***************************************************/
void InitializePrintDialog(
  HWND hwndOwner)
  {
   PrintDialog.lStructSize         = sizeof(PRINTDLG);
   PrintDialog.hwndOwner           = hwndOwner; 
   PrintDialog.hDevMode            = NULL;
   PrintDialog.hDevNames           = NULL;
   PrintDialog.hDC                 = NULL;
   PrintDialog.Flags               = PD_RETURNDC | PD_NOSELECTION | PD_ALLPAGES | PD_COLLATE; 
   PrintDialog.nFromPage           = 1;
   PrintDialog.nToPage             = 9999;
   PrintDialog.nMinPage            = 1;
   PrintDialog.nMaxPage            = 9999;
   PrintDialog.nCopies             = 1;
   PrintDialog.hInstance           = NULL;
   PrintDialog.lCustData           = 0L;
   PrintDialog.lpfnPrintHook       = NULL;
   PrintDialog.lpfnSetupHook       = NULL;
   PrintDialog.lpPrintTemplateName = NULL;
   PrintDialog.lpSetupTemplateName = NULL;
   PrintDialog.hPrintTemplate      = NULL;
   PrintDialog.hSetupTemplate      = NULL;
  }

/*********************************************/
/* PrintDlgProc: Processes messages for the  */
/*   cancel dialog displayed while printing. */
/*********************************************/
#if WIN_BTC
#pragma argsused
#endif
static BOOL CALLBACK PrintDlgProc(
  HWND hDlg, 
  UINT msg, 
  WPARAM wParam, 
  LPARAM lParam)
  {
#if WIN_MCW
#pragma unused(lParam)
#pragma unused(wParam)
#endif
   switch (msg)
     {
      case WM_INITDIALOG:
        EnableMenuItem(GetSystemMenu(hDlg,FALSE),SC_CLOSE,MF_GRAYED);
        return TRUE;

      case WM_COMMAND :
        bUserAbort = TRUE;
        EnableWindow(GetParent(hDlg),TRUE);
        DestroyWindow(hDlg);
        hDlgPrint = 0;
        return TRUE;
     }
  
   return FALSE ;
  }

/**********************************/
/* AbortProc: Handles dispatching */
/*   of messages while printing.  */
/**********************************/
#if WIN_BTC
#pragma argsused
#endif
static BOOL CALLBACK AbortProc(
  HDC hPrinterDC,
  int iCode)
  {
#if WIN_MCW
#pragma unused(iCode)
#pragma unused(hPrinterDC)
#endif
   MSG msg;

   while ((! bUserAbort) && PeekMessage(&msg,NULL,0,0,PM_REMOVE))
     {
      if ((! hDlgPrint) || (! IsDialogMessage(hDlgPrint,&msg)))
        {
         TranslateMessage (&msg) ;
         DispatchMessage (&msg) ;
        }
     }
   return !bUserAbort ;
  }
 
/**********************************/
/* PrintWindowDriver  */
/**********************************/
BOOL PrintWindowDriver(
  HINSTANCE hInst,
  HWND hwnd, 
  LPSTR szTitleName)
  {
   BOOL bSuccess, deleteFont = FALSE;
   LPCTSTR pstrBuffer ;
   int yChar, iLinesPerPage, iTotalLines = 0;
   long iCharsPerLine;
   int displayStart = 0;
   int iTotalPages;
   TEXTMETRIC tm ;
   HFONT hfont, ofont;
   HDC dc;
   LOGFONT lf;
   HWND hwndEdit;
   struct statusWindowData *theStatusData;
   struct displayWindowData *theDisplayData;
   
   /*===========================*/
   /* Display the print dialog. */
   /*===========================*/
   
   if (! PrintDlg(&PrintDialog))
     { return TRUE ; }

   /*===========================================*/
   /* If there is nothing to print then return. */
   /*===========================================*/
   
   if (((ATOM) GetClassLong(hwnd,GCW_ATOM)) == EditAtomClass)
	 {
	  hwndEdit = GetDlgItem(hwnd,ID_EDIT_CONTROL);

	  iTotalLines = (short) SendMessage(hwndEdit,EM_GETLINECOUNT,0,0L);

	  ofont = GetWindowFont(hwndEdit);
	 }
   else if (((ATOM) GetClassLong(hwnd,GCW_ATOM)) == StatusAtomClass)
     {   
      theStatusData = (struct statusWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
      if (theStatusData == NULL) 
        { return TRUE; }

      iTotalLines = (*theStatusData->getCount)(GetCurrentEnvironment());
      
      memset(&lf,0,sizeof(LOGFONT));
      lf.lfHeight = -13;
      strcpy(lf.lfFaceName,"Courier New");
      ofont = CreateFontIndirect(&lf);
      deleteFont = TRUE;
     }
   else if (((ATOM) GetClassLong(hwnd,GCW_ATOM)) == DisplayAtomClass)
     {
      theDisplayData = (struct displayWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
      if (theDisplayData == NULL) 
        { return TRUE; }

      DisplayLineCountAndStart(hwnd,&iTotalLines,&displayStart);

      memset(&lf,0,sizeof(LOGFONT));
      lf.lfHeight = -13;
      strcpy(lf.lfFaceName,"Courier New");
      ofont = CreateFontIndirect(&lf);
      deleteFont = TRUE;
     }
   else
     { iTotalLines = 0; }

   if (iTotalLines == 0)
     { return TRUE; }
     
   /*=================================================*/
   /* Create a logical font for use with the printer. */
   /*=================================================*/
   
   memset(&lf,0,sizeof(LOGFONT));
   dc = GetDC(hwnd);
   SelectObject(dc,ofont);
   GetTextFace(dc,LF_FACESIZE,lf.lfFaceName);
   GetTextMetrics(dc,&tm);

   lf.lfHeight = tm.tmHeight * (GetDeviceCaps(PrintDialog.hDC,LOGPIXELSY) / GetDeviceCaps(dc,LOGPIXELSY));
   lf.lfPitchAndFamily = tm.tmPitchAndFamily;

   hfont = CreateFontIndirect(&lf);
   if (hfont != NULL)
     { SelectObject(PrintDialog.hDC,hfont); }
   
   ReleaseDC(hwnd,dc);
     
   if (deleteFont)
     { DeleteFont(ofont); }
     
   GetTextMetrics(PrintDialog.hDC,&tm);
   yChar = tm.tmHeight + tm.tmExternalLeading;

   iCharsPerLine = GetDeviceCaps(PrintDialog.hDC,HORZRES) / tm.tmAveCharWidth;
   iLinesPerPage = GetDeviceCaps(PrintDialog.hDC,VERTRES) / yChar;
   iTotalPages = (iTotalLines + iLinesPerPage - 1) / iLinesPerPage;

   pstrBuffer = (LPCTSTR) HeapAlloc(GetProcessHeap(), 
		                            HEAP_NO_SERIALIZE,
		                            (unsigned long) iCharsPerLine + 1);

   /*=============================================*/
   /* Disable the parent window of the dialog so  */
   /* all messages go to the cancel print dialog. */
   /*=============================================*/
   
   EnableWindow(hwnd,FALSE);

   /*=============================*/
   /* Set the success/abort flags */
   /* to their initial value.     */
   /*=============================*/
   
   bUserAbort = FALSE;

   /*=================================*/
   /* Create the cancel print dialog. */
   /*=================================*/
   
   hDlgPrint = CreateDialog(hInst,(LPCTSTR) "PrintDlgBox",hwnd,PrintDlgProc);
   SetDlgItemText(hDlgPrint,IDD_FNAME,szTitleName);

   /*=========================================*/
   /* Set the abort procedure which handles   */
   /* dispatching of messages while printing. */
   /*=========================================*/
   
   SetAbortProc(PrintDialog.hDC,AbortProc);

   GetWindowText(hwnd,(PTSTR) di.lpszDocName,sizeof (PTSTR));

   if (StartDoc (PrintDialog.hDC, &di) > 0)
     {
      bSuccess = PrintIt(iTotalPages,iLinesPerPage,iTotalLines,iCharsPerLine,
                         yChar,pstrBuffer,hwnd);
     }
   else
     { bSuccess = FALSE; }

   if (bSuccess)
     { EndDoc(PrintDialog.hDC); }

   if (! bUserAbort)
     {
      EnableWindow(hwnd,TRUE);
      DestroyWindow (hDlgPrint) ;
     }

   HeapFree(GetProcessHeap(),0,(LPVOID) pstrBuffer);
   DeleteDC(PrintDialog.hDC);

   if (hfont != NULL)
     { DeleteFont(hfont); }

   return (bSuccess && (! bUserAbort));
  }
    
/**********************************/
/* PrintIt  */
/**********************************/
static BOOL PrintIt(
  int iTotalPages,
  int iLinesPerPage,
  int iTotalLines,
  int iCharsPerLine,
  int yChar,
  LPCTSTR pstrBuffer,
  HWND hwnd)
  {
   WORD iColCopy, iNoiColCopy;
   int iPage, iLine, iLineNum;
   HWND hwndEdit;
   struct statusWindowData *theStatusData;
   struct displayWindowData *theDisplayData;
   ATOM theAtom = (unsigned short) GetClassLong(hwnd,GCW_ATOM);
   void *valuePtr;
   int count;
   int dummy, displayStart;

   if (theAtom == EditAtomClass)
     {    
      hwndEdit = GetDlgItem(hwnd,ID_EDIT_CONTROL);
     }
   else if (theAtom == StatusAtomClass)
     {   
      theStatusData = (struct statusWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
      if (theStatusData == NULL) 
        { return TRUE; }
     }
   else if (theAtom == DisplayAtomClass)
     {
      theDisplayData = (struct displayWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
      if (theDisplayData == NULL) 
        { return TRUE; }

      DisplayLineCountAndStart(hwnd,&dummy,&displayStart);
     }
   else
     { return TRUE; }

   /*========================================================*/
   /* If we're collating and there are multiple copies, then */
   /* print in the order (1,2,3,1,2,3...), otherwise print   */
   /* in the order (1,1,...2,2,...,3,3,...).                 */
   /*========================================================*/
   
   for (iColCopy = 0;
        iColCopy < ((WORD) PrintDialog.Flags & PD_COLLATE ? PrintDialog.nCopies : 1);
        iColCopy++)
     {
      for (iPage = 0 ; iPage < iTotalPages ; iPage++)
        {
         if (PrintDialog.Flags & PD_PAGENUMS)
           {
            if (((iPage + 1) < PrintDialog.nFromPage) ||
                ((iPage + 1) > PrintDialog.nToPage))
              { continue; }
           }

         for (iNoiColCopy = 0;
              iNoiColCopy < (PrintDialog.Flags & PD_COLLATE ? 1 : PrintDialog.nCopies);
              iNoiColCopy++)
           {
            if (StartPage(PrintDialog.hDC) < 0)
              { return(FALSE); }
            
            if (((ATOM) GetClassLong(hwnd,GCW_ATOM)) == EditAtomClass)
              {
               for (iLine = 0 ; iLine < iLinesPerPage ; iLine++)
                 {
                  iLineNum = iLinesPerPage * iPage + iLine ;

                  if (iLineNum > iTotalLines)
                    { break; }

                  *(int *) pstrBuffer = iCharsPerLine;

                  TextOut(PrintDialog.hDC,0,yChar * iLine,pstrBuffer,
                             (int) SendMessage(hwndEdit,EM_GETLINE,(WPARAM) iLineNum, 
                                              (LPARAM) pstrBuffer)) ;
                 }
              }
			else if (((ATOM) GetClassLong(hwnd,GCW_ATOM)) == StatusAtomClass)
              {                  
               iLineNum = iLinesPerPage * iPage;
               iLine = 0;
               for (valuePtr = (*theStatusData->getNextValue)(GetCurrentEnvironment(),NULL), count = 0;
                    valuePtr != NULL;
                    valuePtr = (*theStatusData->getNextValue)(GetCurrentEnvironment(),valuePtr), count++)
                 {
                  if (count < iLineNum) continue;
                  if (count > (iLineNum + iLinesPerPage)) break;
                  if (count > iTotalLines) break;
                  
                  (*theStatusData->getPPForm)(GetCurrentEnvironment(),(char *) pstrBuffer,(unsigned) iCharsPerLine,valuePtr);

                  TextOut(PrintDialog.hDC,0,yChar * iLine,pstrBuffer,(int) strlen(pstrBuffer));
                  
                  iLine++;
                 }  
              }
			else if (((ATOM) GetClassLong(hwnd,GCW_ATOM)) == DisplayAtomClass)
              {                  
               iLineNum = (iLinesPerPage * iPage) + displayStart;
               if (iLineNum > DIALOG_SIZE)
                 { iLineNum -= DIALOG_SIZE; }

               for (iLine = 0 ; iLine < iLinesPerPage ; iLine++)
                 {
                  if (theDisplayData->terminal[iLineNum] == NULL)
                    { break; }
                    
                  TextOut(PrintDialog.hDC,0,yChar * iLine,
                          theDisplayData->terminal[iLineNum],
                          (int) strlen(theDisplayData->terminal[iLineNum]));
                  iLineNum++;
                  if (iLineNum > DIALOG_SIZE)
                    { iLineNum = 0; }
                    
                  if (iLineNum == displayStart)
                    { break; }
                 }
              }

            if (EndPage(PrintDialog.hDC) < 0)
              { return(FALSE); }

            if (bUserAbort)
              { return(TRUE); }
           }

         if (bUserAbort)
            { return(TRUE); }
        }

      if (bUserAbort)
        { return(TRUE); }
     }
     
   return(TRUE);
  }

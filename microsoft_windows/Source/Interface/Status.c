   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*       Microsoft Windows Version 3.0  01/31/02       */
   /*                                                     */
   /*                   STATUS MODULE                     */
   /*******************************************************/

/**************************************************************/
/* Purpose: Callback and support for all status windows.      */
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

#define _STATUS_SOURCE_

/*-------------------------------+
| Windows & System Include Files |
+-------------------------------*/

#include "StdSDK.h"     // Standard application includes
#include <windows.h>
#include <windowsx.h>
#include <string.h>

/*------------------------+
| CLIPS 6.0 Include Files |
+------------------------*/
#include "setup.h"
#include "engine.h"
#include "inscom.h"
#include "globldef.h"
#include "insfun.h"
#include "factmngr.h"

/*------------------------+
| Interface Include Files |
+------------------------*/

#include "findwnd.h"
#include "MDI.h"
#include "Print.h"
#include "frame.h"
#include "Initialization.h"
#include "resource.h"
#include "status.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    status_OnSize(HWND,UINT,int,int);
   static void                    status_OnVScroll(HWND,HWND,UINT,int);
   static void                    status_OnHScroll(HWND,HWND,UINT,int);
   static void                    status_OnPaint(HWND);
   static void                    status_OnMDIActivate(HWND,BOOL,HWND,HWND);
   static void                    status_OnClose(HWND);
   static void                    status_OnCommand(HWND,int,HWND,UINT);
   static void                    status_OnUpdateMenu(HWND,HMENU);
   static BOOL                    status_OnContextMenu(HWND,HWND,int,int);
   static void                    RedrawScreen(HWND,HDC);
   static void                    UpdateWnd(HWND);
   static void                    GetFocusPPForm(void *,char *,unsigned,void *);
   static int                     CountInstances(void *);
   static int                     CountDefglobals(void *);
   static int                     CountFocus(void *);
   static int                     CountFacts(void *); 
   static int                     CountActivations(void *);
   static void                    UpdateStatusWndTitle(HWND);
   static void                    UpdateStatusContent(HWND);
   static void                    SaveStatusWindow(HWND);
   static HWND                    statusWindow_New(HWND,char *,int,int,int,int,
                                                   void (*)(void *,char *,unsigned,void *),
                                                   void *(*)(void *,void *),
												   int (*)(void *),
                                                   void (*)(void *,int),
												   int (*)(void *));
  
/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   globle HWND                       FactsWindow = NULL;
   globle HWND                       AgendaWindow = NULL;
   globle HWND                       InstancesWindow = NULL;
   globle HWND                       GlobalsWindow = NULL;
   globle HWND                       FocusWindow = NULL;
   globle ATOM                       StatusAtomClass;

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static HMENU                StatusMenu = NULL;
   static HMENU                StatusWindowMenu = NULL;
   static HMENU                StatusContextMenu = NULL;
 
/*********************/
/* StatusWndProc:   */
/*********************/
LRESULT CALLBACK StatusWndProc(
  HWND hwnd, 
  UINT message, 
  WPARAM wParam, 
  LPARAM lParam)
  {    

   switch(message)
     { 
      HANDLE_MSG(hwnd,WM_COMMAND,status_OnCommand);
      HANDLE_MSG(hwnd,WM_CLOSE,status_OnClose);
      HANDLE_MSG(hwnd,WM_SIZE,status_OnSize);
      HANDLE_MSG(hwnd,WM_PAINT,status_OnPaint);
      HANDLE_MSG(hwnd,WM_VSCROLL,status_OnVScroll);
      HANDLE_MSG(hwnd,WM_HSCROLL,status_OnHScroll);
      HANDLE_MSG(hwnd,WM_MDIACTIVATE,status_OnMDIActivate);
	  HANDLE_MSG(hwnd,WM_CONTEXTMENU,status_OnContextMenu);
     } 
         
   return DefMDIChildProc(hwnd,message,wParam,lParam);
  }

/***********************************************************/
/* status_InitInstance: Registers the status window class. */
/*   Returns TRUE if successful, otherwise FALSE.          */
/***********************************************************/
BOOL status_InitInstance(
   HINSTANCE hinst)
   {
    if (! (StatusAtomClass = registerMDIChild(hinst,0,IDR_STATUS,StatusWndProc,sizeof(WORD))))
      { return FALSE; }
      
    StatusMenu = LoadMenu(hinst,MAKEINTRESOURCE(IDR_CLIPS));

    if (StatusMenu == NULL)
      return FALSE;
      
    StatusContextMenu = LoadMenu(hinst,MAKEINTRESOURCE(IDR_STATUS_CONTEXT));

    StatusWindowMenu = findWindowMenu(StatusMenu);
   
    return TRUE;
   }
   
/*********************************************************/
/* factsWindow_New:  */
/*********************************************************/
BOOL factsWindow_New(
  HWND hwnd)
  {
   int width, height;
   int xpos, ypos, wwidth, wheight;
   
   width = AvailableFrameWidth();
   height = AvailableFrameHeight();
   
   xpos = (int) (width * 0.66);
   ypos = 2;
   wwidth = (int) (width * 0.33);
   wheight = (int) (height * 0.33) - 5;
      
   FactsWindow = statusWindow_New(hwnd,"Facts",xpos,ypos,
                                  wwidth,wheight,
                                  EnvGetFactPPForm,
								  GetNextFactInScope,
								  EnvGetFactListChanged,
                                  EnvSetFactListChanged,
								  CountFacts);
                                  
   if (FactsWindow == NULL)
     { return(FALSE); }
     
   return(TRUE);    
  }
  
/*********************************************************/
/* agendaWindow_New:  */
/*********************************************************/
BOOL agendaWindow_New(
  HWND hwnd)
  {
   int width, height;
   int xpos, ypos, wwidth, wheight;
   
   width = AvailableFrameWidth();
   height = AvailableFrameHeight();
   
   xpos = 2;
   ypos = (int) (height * 0.66);
   wwidth = (int) (width * 0.4) - 5;
   wheight = (int) (height * 0.33);
      
   AgendaWindow = statusWindow_New(hwnd,"Agenda",xpos,ypos,wwidth,wheight,
                                   EnvGetActivationPPForm,
								   EnvGetNextActivation,
								   EnvGetAgendaChanged,
                                   EnvSetAgendaChanged,
								   CountActivations);
                                  
   if (AgendaWindow == NULL)
     { return(FALSE); }
     
   return(TRUE);    
  }

/*********************************************************/
/* instancesWindow_New:  */
/*********************************************************/
BOOL instancesWindow_New(
  HWND hwnd)
  {
   int width, height;
   int xpos, ypos, wwidth, wheight;
   
   width = AvailableFrameWidth();
   height = AvailableFrameHeight();
   
   xpos = (int) (width * 0.66);
   ypos = (int) (height * 0.33);
   wwidth = (int) (width * 0.33);
   wheight = (int) (height * 0.33) - 3;
      
   InstancesWindow = statusWindow_New(hwnd,"Instances",xpos,ypos,wwidth,wheight,
                                      EnvGetInstancePPForm,
									  GetNextInstanceInScope,
									  EnvGetInstancesChanged,
                                      EnvSetInstancesChanged,
									  CountInstances);
                                  
   if (InstancesWindow == NULL)
     { return(FALSE); }
     
   return(TRUE);    
  }
  
/*********************************************************/
/* globalsWindow_New:  */
/*********************************************************/
BOOL globalsWindow_New(
  HWND hwnd)
  {
   int width, height;
   int xpos, ypos, wwidth, wheight;
   
   width = AvailableFrameWidth();
   height = AvailableFrameHeight();
   
   xpos = (int) (width * 0.66);
   ypos = (int) (height * 0.66);
   wwidth = (int) (width * 0.33);
   wheight = (int) (height * 0.33);
      
   GlobalsWindow = statusWindow_New(hwnd,"Globals",xpos,ypos,wwidth,wheight,
                                    EnvGetDefglobalValueForm,
									GetNextDefglobalInScope,
									EnvGetGlobalsChanged,
                                    EnvSetGlobalsChanged,
									CountDefglobals);
                                  
   if (GlobalsWindow == NULL)
     { return(FALSE); }
     
   return(TRUE);    
  }
  
/*********************************************************/
/* focusWindow_New:  */
/*********************************************************/
BOOL focusWindow_New(
  HWND hwnd)
  {
   int width, height;
   int xpos, ypos, wwidth, wheight;
   
   width = AvailableFrameWidth();
   height = AvailableFrameHeight();
   
   xpos = (int) (width * 0.4);
   ypos = (int) (height * 0.66);
   wwidth = (int) (width * 0.26) - 5;
   wheight = (int) (height * 0.33);

   FocusWindow = statusWindow_New(hwnd,"Focus",xpos,ypos,wwidth,wheight,
                                  GetFocusPPForm,
								  EnvGetNextFocus,
								  EnvGetFocusChanged,
                                  EnvSetFocusChanged,
								  CountFocus);
                                  
   if (FocusWindow == NULL)
     { return(FALSE); }
     
   return(TRUE);    
  }

/*********************************************************/
/* TileStatusWindows:  */
/*********************************************************/
void TileStatusWindows()
  {
   int width, height;
   int xpos, ypos, wwidth, wheight;
   
   width = AvailableFrameWidth();
   height = AvailableFrameHeight();

   /*==============================*/
   /* Reposition the facts window. */
   /*==============================*/
   
   xpos = (int) (width * 0.66);
   ypos = 2;
   wwidth = (int) (width * 0.33);
   wheight = (int) (height * 0.33) - 5;

   MoveWindow(FactsWindow,xpos,ypos,wwidth,wheight,TRUE);

   /*===============================*/
   /* Reposition the agenda window. */
   /*===============================*/
   
   xpos = 2;
   ypos = (int) (height * 0.66);
   wwidth = (int) (width * 0.4) - 5;
   wheight = (int) (height * 0.33);

   MoveWindow(AgendaWindow,xpos,ypos,wwidth,wheight,TRUE);

   /*==================================*/
   /* Reposition the instances window. */
   /*==================================*/
   
   xpos = (int) (width * 0.66);
   ypos = (int) (height * 0.33);
   wwidth = (int) (width * 0.33);
   wheight = (int) (height * 0.33) - 3;

   MoveWindow(InstancesWindow,xpos,ypos,wwidth,wheight,TRUE);

   /*================================*/
   /* Reposition the globals window. */
   /*================================*/
   
   xpos = (int) (width * 0.66);
   ypos = (int) (height * 0.66);
   wwidth = (int) (width * 0.33);
   wheight = (int) (height * 0.33);
   
   MoveWindow(GlobalsWindow,xpos,ypos,wwidth,wheight,TRUE);

   /*==============================*/
   /* Reposition the focus window. */
   /*==============================*/

   xpos = (int) (width * 0.4);
   ypos = (int) (height * 0.66);
   wwidth = (int) (width * 0.26) - 5;
   wheight = (int) (height * 0.33);
   
   MoveWindow(FocusWindow,xpos,ypos,wwidth,wheight,TRUE);
  }
  
/*********************************************************/
/* statusWindow_New:  */
/*********************************************************/
static HWND statusWindow_New(
  HWND hwnd,
  char *baseName,
  int xpos,
  int ypos,
  int width,
  int height,
  void (*getPPForm)(void *,char *,unsigned,void *),
  void *(*getNextValue)(void *,void *),
  int (*getChanged)(void *),
  void (*setChanged)(void *,int),
  int (*getCount)(void *))
  {
   HWND hwndChild;
   struct statusWindowData *theData;
   HDC hDC;
   TEXTMETRIC tm;
   char buffer[255];
   LOGFONT lf;
   HFONT hfont;

   theData = (struct statusWindowData *) malloc(sizeof(struct statusWindowData));
   
   if (theData == NULL)
     { return(NULL); }
              
   sprintf(buffer,"%s Window",baseName);

   hwndChild = mdi_Create(MDIClientWnd,0,IDR_STATUS,buffer,
                          CW_USEDEFAULT,0,CW_USEDEFAULT,0);
  
   if (hwndChild == NULL) 
     { 
      free(theData);
      return(NULL); 
     }

   theData->getNextValue = getNextValue;
   theData->getPPForm = getPPForm;
   theData->lastLine = 0;
   theData->noLines = 0;
   theData->getChanged = getChanged;
   theData->setChanged = setChanged;
   theData->getCount = getCount;
   theData->baseName = baseName;

   hDC = GetDC(hwndChild);
   memset(&lf,0,sizeof(LOGFONT));
   lf.lfHeight = -13;
   strcpy(lf.lfFaceName,"Courier New");
   hfont = CreateFontIndirect(&lf);
   SelectObject(hDC,hfont);

   GetTextMetrics(hDC,&tm);
   ReleaseDC(hwnd,hDC);
      
   theData->lineSize = tm.tmHeight+ tm.tmExternalLeading;
     
   SetWindowLong(hwndChild,GWL_USERDATA,(long) theData);
   
   MoveWindow(hwndChild,xpos,ypos,width,height,TRUE);
   
   SetScrollRange(hwndChild,SB_HORZ,0,255,TRUE);
       
   ShowWindow(hwndChild,SW_HIDE);

   return(hwndChild);
  }

/************************************************/
/* status_OnCommand:    */
/************************************************/   
static void status_OnCommand(
  HWND hwnd, 
  int id, 
  HWND hctl, 
  UINT codeNotify)
  {	 
   char buffer[255];
   struct statusWindowData *theData;
   theData = (struct statusWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) 
     { return; }

   switch (id) 
	 {
      case ID_FILE_PRINT:
        sprintf(buffer,"%s Window",theData->baseName);
        PrintWindowDriver(GetWindowInstance(hMainFrame),hwnd,buffer);
	    return;

      case ID_FILE_SAVE:
      case ID_FILE_SAVE_AS:
        SaveStatusWindow(hwnd);
        return;
	 } 
   
   FORWARD_WM_COMMAND(hwnd,id,hctl,codeNotify,DefMDIChildProc);
  }


/************************************************/
/* status_OnPaint:    */
/************************************************/
static void status_OnPaint(
  HWND hwnd)
  {
   HDC hdc;
   PAINTSTRUCT ps;
    
   hdc = BeginPaint(hwnd,&ps);
   SetMapMode(hdc,MM_TEXT);;
   RedrawScreen(hwnd,hdc);
   ValidateRect(hwnd,NULL);
   EndPaint(hwnd, &ps) ;
  }

/************************************************/
/* status_OnVScroll: */
/************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void status_OnVScroll(
  HWND hwnd, 
  HWND hwndCtl, 
  UINT code, 
  int pos)
  {
#if WIN_MCW
#pragma unused(hwndCtl)
#endif
   int min, max, cp;
   struct statusWindowData *theData;
   
   theData = (struct statusWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) 
     { return; }
     
   GetScrollRange(hwnd,SB_VERT,&min,&max);
   cp = GetScrollPos(hwnd,SB_VERT);
   
   switch (code)
	 {   
      case SB_LINEDOWN:
        cp++;
        break;

      case SB_LINEUP:
        cp--;
        break;

      case SB_PAGEDOWN:
        cp += theData->noLines;
        break;
        
      case SB_PAGEUP:
        cp -= theData->noLines;
        break;
      
      case SB_THUMBTRACK:
        cp = pos;
        break;

      default:
        return;
	 }

   if (cp > max)
     { cp = max; }

   if (cp < min)
     { cp = min; }

   SetScrollPos(hwnd,SB_VERT,cp,TRUE);
   InvalidateRect(hwnd,NULL,TRUE);
  }

/************************************************/
/* status_OnHScroll: */
/************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void status_OnHScroll(
  HWND hwnd, 
  HWND hwndCtl, 
  UINT code, 
  int pos)
  {
#if WIN_MCW
#pragma unused(hwndCtl)
#endif
   int min, max, cp;
   struct statusWindowData *theData;
   
   theData = (struct statusWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) 
     { return; }

   GetScrollRange(hwnd,SB_HORZ,&min,&max);
   cp = GetScrollPos(hwnd,SB_HORZ);
   
   switch (code)
	 {   
      case SB_LINEDOWN:
        cp++;
        break;

      case SB_LINEUP:
        cp--;
        break;

      case SB_PAGEDOWN:
        cp += theData->noLines; // Should be window width?
        break;
        
      case SB_PAGEUP:
        cp -= theData->noLines; // Should be window width?
        break;
      
      case SB_THUMBTRACK:
        cp = pos;
        break;

      default:
        return;
	 }
   
   if (cp > max) cp = max;
   if (cp < min) cp = min;
	 
   SetScrollPos(hwnd,SB_HORZ,cp,TRUE);
   InvalidateRect(hwnd,NULL,TRUE);
  }

/************************************************/
/* status_OnSize:    */
/************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void status_OnSize(
  HWND hwnd, 
  UINT state, 
  int cx, 
  int cy)
  {
#if WIN_MCW
#pragma unused(state)
#pragma unused(cx)
#endif
   struct statusWindowData *theData;
   
   theData = (struct statusWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) 
     { return; }
     
   theData->noLines = cy / theData->lineSize;
   UpdateWnd(hwnd);

   FORWARD_WM_SIZE(hwnd,state,cx,cy,DefMDIChildProc);
  }

/************************************************/
/* status_OnMDIActivate:    */
/************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void status_OnMDIActivate(
  HWND hwnd,
  BOOL active,
  HWND hActivate,
  HWND hDeactivate)
  {
#if WIN_MCW
#pragma unused(hActivate)
#pragma unused(hDeactivate)
#endif
   if (active)
     {
      ShowWindow(hwnd,SW_SHOW);
      
      UpdateStatusWndTitle(hwnd);
      
      status_OnUpdateMenu(hwnd,StatusMenu);
  
      if (FORWARD_WM_MDISETMENU(MDIClientWnd,TRUE,StatusMenu,
                            StatusWindowMenu,SendMessage) != 0)
        { DrawMenuBar(hMainFrame); }
     }
  }
  
/**************************************************/
/* status_OnClose: Overrides the close handler so */
/*   the status window is hidden, not closed.     */
/**************************************************/
static void status_OnClose(
  HWND hwnd)
  {
   char buffer[255];
   struct statusWindowData *theData;
   
   theData = (struct statusWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) 
     { return; }
     
   FORWARD_WM_MDINEXT(MDIClientWnd,hwnd,0,SendMessage);
   
   sprintf(buffer,"%s Window",theData->baseName);
   SetWindowText(hwnd,buffer);
   
   ShowWindow(hwnd,SW_HIDE);
  }
  
/*****************************************/ 
/* status_OnUpdateMenu: Updates the menu */
/*   and updates the toolbar as well.    */  
/*****************************************/
#if WIN_BTC
#pragma argsused
#endif
static void status_OnUpdateMenu(
  HWND hwnd, 
  HMENU hmenu)
  {
#if WIN_MCW
#pragma unused(hwnd)
#endif
   EnableMenuItem(hmenu,ID_FILE_CLOSE,MF_ENABLED);
   EnableMenuItem(hmenu,ID_FILE_SAVE,MF_ENABLED);
   EnableMenuItem(hmenu,ID_FILE_SAVE_AS,MF_ENABLED);

   EnableMenuItem(hmenu,ID_EDIT_UNDO,MF_GRAYED);
   EnableMenuItem(hmenu,ID_EDIT_CUT,MF_GRAYED);
   EnableMenuItem(hmenu,ID_EDIT_COPY,MF_GRAYED);
   EnableMenuItem(hmenu,ID_EDIT_CLEAR,MF_GRAYED);
   EnableMenuItem(hmenu,ID_EDIT_PASTE,MF_GRAYED);
   EnableMenuItem(hmenu,ID_EDIT_SELECT_ALL,MF_GRAYED);
   EnableMenuItem(hmenu,ID_EDIT_BALANCE,MF_GRAYED);
   EnableMenuItem(hmenu,ID_EDIT_COMMENT,MF_GRAYED);
   EnableMenuItem(hmenu,ID_EDIT_UNCOMMENT,MF_GRAYED);
   EnableMenuItem(hmenu,ID_EDIT_SET_FONT,MF_GRAYED);

   EnableMenuItem(hmenu,ID_BUFFER_FIND,MF_GRAYED);
   EnableMenuItem(hmenu,ID_BUFFER_REPLACE,MF_GRAYED);
   EnableMenuItem(hmenu,ID_BUFFER_LOAD,MF_GRAYED);
   EnableMenuItem(hmenu,ID_BUFFER_BATCH,MF_GRAYED);
   EnableMenuItem(hmenu,ID_BUFFER_LOAD_BUFFER,MF_GRAYED);

   EnableMenuItem(hmenu,ID_HELP_COMPLETE,MF_GRAYED);
   
   PostMessage(hMainFrame,UWM_UPDATE_TOOLBAR,0,0);
  }

/***********************************************************/
/* status_OnContextMenu: Pops up a context-specific menu. */
/***********************************************************/
static BOOL status_OnContextMenu(
  HWND hwnd, 
  HWND hwndCtl, 
  int xPos, 
  int yPos)
  {
   return mdi_OnContextMenu(hwnd,hwndCtl,xPos,yPos,StatusContextMenu);
  }

/*************************************/
/* RedrawScreen: Function will clear */
/*   and redraw a status screen.     */
/*************************************/
static void RedrawScreen(
  HWND hwnd,
  HDC hDC)
  {  
   void *valuePtr;
   char Buffer[300];
   RECT Rect;
   int count, pos, sbmax = 0;
   int bufsize;
   HFONT hfont;
   struct statusWindowData *theData;
   LOGFONT lf;
   
   theData = (struct statusWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) 
     { return; }
   
   //hfont = GetStockFont(SYSTEM_FIXED_FONT);
   memset(&lf,0,sizeof(LOGFONT));
   lf.lfHeight = -13;
   strcpy(lf.lfFaceName,"Courier New");
   hfont = CreateFontIndirect(&lf);
   if (hfont != NULL)
     { SelectFont(hDC,hfont); }

   pos = GetScrollPos(hwnd,SB_VERT);
   
   /*===================================*/
   /* Skip over any facts that fall out */
   /* of range above the window.        */
   /*===================================*/
   
   count = 0;
   valuePtr = (*theData->getNextValue)(GetCurrentEnvironment(),NULL);
   while ((valuePtr != NULL) && (count < pos))
     {  
      valuePtr = (*theData->getNextValue)(GetCurrentEnvironment(),valuePtr);
      count++;
     }  

   if (valuePtr != NULL)
     {  
      /*===============*/
      /* Clear window. */
      /*===============*/
      
      GetClientRect(hwnd,&Rect);
      Rectangle(hDC,Rect.left-2,Rect.top-2,Rect.right+2,Rect.bottom+2);
      Rect.left = 2;

      /*=============================*/
      /* Display the items that fall */
      /* within the status window.   */
      /*=============================*/
      
      count = 0;
      do
        {  
         pos = GetScrollPos(hwnd,SB_HORZ);
         (*theData->getPPForm)(GetCurrentEnvironment(),Buffer,299,valuePtr);
         bufsize = lstrlen (Buffer);
         if (bufsize > sbmax) sbmax = bufsize;
         if (pos < bufsize)
           {  
            char *buffer = Buffer + pos;
            DrawText(hDC,buffer,lstrlen(buffer),&Rect,DT_LEFT);
           }
         Rect.top = Rect.top + theData->lineSize;
         count++;
        }
      while (((valuePtr= (*theData->getNextValue)(GetCurrentEnvironment(),valuePtr)) != NULL) && 
             (count < theData->noLines));
        { SetScrollRange(hwnd,SB_HORZ,0,sbmax-1,TRUE); }
     }
   
   if (hfont != NULL)
     { DeleteFont(hfont); }
  }

/*************************************/
/* GetFocusPPForm: Returns the Focus */
/*   Names in printable form.        */
/*************************************/
static void GetFocusPPForm(
  void *theEnv,
  char *buffer,
  unsigned bufferLength,
  void *vTheFocus)
  {  
   struct focus *theFocus = (struct focus *) vTheFocus;
   strncpy(buffer,EnvGetDefmoduleName(theEnv,theFocus->theModule),bufferLength);
  }

/*************************************/
/* CountFocus: Returns the number of */
/*   modules in the focus stack.     */
/*************************************/
static int CountFocus(
  void *theEnv)
  { 
   int count = 0;
   void *Ptr = NULL;
  
   while ((Ptr = EnvGetNextFocus(theEnv,Ptr)) != NULL)
     { count++; }
   return count;
  }

/**********************************/
/* CountFacts: Returns the number */
/*   of facts in scope.           */
/**********************************/
static int CountFacts(
  void *theEnv)
  { 
   int count = 0;
   void *FactPtr = NULL;
   
   while ((FactPtr = GetNextFactInScope(theEnv,FactPtr)) != NULL)
     { count++; }
   return count;
  }

/**************************************/
/* CountInstances: Returns the number */
/*   of instances in scope.           */
/**************************************/
static int CountInstances(
  void *theEnv)
  { 
   int count = 0;
   void *InstPtr = NULL;
   
   while ((InstPtr = GetNextInstanceInScope(theEnv,InstPtr)) != NULL)
     { count++; }
   
   return count;
  }

/***************************************/
/* CountDefglobals: Returns the number */
/*   of defglobals in scope.           */
/***************************************/
static int CountDefglobals(
  void *theEnv)
  { 
   int count = 0;
   void *Ptr = NULL;
   
   while ((Ptr = GetNextDefglobalInScope(theEnv,Ptr)) != NULL)
     { count++; }
     
   return count;
  }

/***************************************/
/* CountActivations: Returns the number */
/*   of activations in scope.           */
/***************************************/
static int CountActivations(
  void *theEnv)
  {
   int count = 0;
   void *Ptr = NULL;

   while ((Ptr = EnvGetNextActivation(theEnv,Ptr)) != NULL)
     { count++; }

   return count;
  }

/**********************************************************/
/* UpdateStatus: Function called while CLIPS is executing */
/*    to update each visible status window.               */
/**********************************************************/
void UpdateStatus(void)
  {  
   void *theEnv = GetCurrentEnvironment();
   static long lastModuleIndex = -1;

   if (lastModuleIndex != DefmoduleData(theEnv)->ModuleChangeIndex)
     {
#if DEFRULE_CONSTRUCT
      EnvSetFocusChanged(theEnv,CLIPS_TRUE);
      EnvSetAgendaChanged(theEnv,CLIPS_TRUE);
#endif
#if DEFTEMPLATE_CONSTRUCT
      EnvSetFactListChanged(theEnv,CLIPS_TRUE);
#endif
#if OBJECT_SYSTEM
      EnvSetInstancesChanged(theEnv,CLIPS_TRUE);
#endif
#if DEFGLOBAL_CONSTRUCT
      EnvSetGlobalsChanged(theEnv,CLIPS_TRUE);
#endif
      lastModuleIndex = DefmoduleData(GetCurrentEnvironment())->ModuleChangeIndex;
     }

#if DEFRULE_CONSTRUCT
   UpdateStatusContent(AgendaWindow);
   UpdateStatusContent(FocusWindow);
#endif

#if DEFTEMPLATE_CONSTRUCT
   UpdateStatusContent(FactsWindow);
#endif

#if OBJECT_SYSTEM
   UpdateStatusContent(InstancesWindow);
#endif

#if DEFGLOBAL_CONSTRUCT
   UpdateStatusContent(GlobalsWindow);    
#endif
}

/**************************************************/
/* UpdateStatusContent: Determines if the content */
/*   of a status window needs to be updated.      */
/**************************************************/
static void UpdateStatusContent( 
  HWND hwnd)
  {
   struct statusWindowData *theData;
   
   if (hwnd == NULL) return;
   
   if (! IsWindowVisible(hwnd)) return;
    
   theData = (struct statusWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) return; 
     
   if (! (*theData->getChanged)(GetCurrentEnvironment())) return;
   
   (*theData->setChanged)(GetCurrentEnvironment(),CLIPS_FALSE);
   theData->lastLine = (*theData->getCount)(GetCurrentEnvironment());
   UpdateStatusWndTitle(hwnd);
   UpdateWnd(hwnd);
  }
    
/************************************************************/
/* UpdateWnd: Calculates the Maximum Value for the Vertical */
/*    Scrollbar, Current position and Redraws Window.       */
/************************************************************/
void UpdateWnd( 
  HWND hwnd)
  {  
   struct statusWindowData *theData;
   int max;
   
   theData = (struct statusWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) 
     { return; }
     
   /*==============================*/
   /* Calculate the maximum value  */
   /* for the vertical scroll bar. */
   /*==============================*/
   
   max = (int) theData->lastLine - theData->noLines;
   if (max < 0)
     { max = 0; }
     
   SetScrollRange(hwnd,SB_VERT,0,max,FALSE);

   /*=================================================*/
   /* If Agenda Window then start display at the top. */
   /*=================================================*/
   
   if (hwnd == AgendaWindow)
     { max = 0; }
   
   SetScrollPos(hwnd,SB_VERT,max,TRUE);
   InvalidateRect(hwnd,NULL,TRUE);
  }

/**************************************************/
/* UpdateStatusWndTitle: Function will append the */
/*   current module name to the Window Title Bar. */
/**************************************************/
static void UpdateStatusWndTitle(
  HWND hwnd)
  {  
   void *theEnv = GetCurrentEnvironment();
   struct statusWindowData *theData;
   struct defmodule *theModule = (struct defmodule *) EnvGetCurrentModule(theEnv);
   char buffer[255];
       
   theData = (struct statusWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) return; 
   
   sprintf(buffer,"%s (%s)",theData->baseName,EnvGetDefmoduleName(theEnv,theModule));
   SetWindowText(hwnd,buffer);
  }

/**************************************/
/* SaveStatusWindow: Saves the output */
/*   of a status window to a file.    */
/**************************************/
static void SaveStatusWindow( 
  HWND hwnd)
  {  
   OPENFILENAME ofn;
   char File[256], FileTitle[256], Filter[256];
   UINT i;
   int cbString;
   char Replace;
   int x;
   size_t size;
   struct statusWindowData *theData;
   FILE *fp;
   void *valuePtr;
   char buffer[300];
   
   theData = (struct statusWindowData *) GetWindowLong(hwnd,GWL_USERDATA);
   
   if (theData == NULL) return; 

   sprintf(File,"%sWindow.txt",theData->baseName);
   memset(&ofn,0,sizeof(OPENFILENAME));
   sprintf(buffer,"Select %s Window Save File",theData->baseName);
   ofn.lpstrTitle = buffer;
      
   if ((cbString = LoadString(GetWindowInstance(hwnd),IDS_TEXT_FILES,Filter,sizeof(Filter))) == 0)
     { return; }

   Replace = Filter[cbString-1];
   for (i=0; Filter[i] != '\0'; i++)
     {
      if (Filter[i] == Replace)
        { Filter[i] = '\0'; }
     }

   ofn.lStructSize = sizeof(OPENFILENAME);
   ofn.hwndOwner = hwnd;
   ofn.lpstrFilter = Filter;
   ofn.nFilterIndex = 1;
   ofn.lpstrFile = File;
   ofn.nMaxFile = sizeof(File);
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
     
   /*=================================*/
   /* Save the contents of the status */
   /* window to the file.             */
   /*=================================*/
   
   if ((fp = fopen(ofn.lpstrFile,"w")) == NULL)
     { return; }
   
   for (valuePtr = (*theData->getNextValue)(GetCurrentEnvironment(),NULL);
        (valuePtr != NULL);
        valuePtr = (*theData->getNextValue)(GetCurrentEnvironment(),valuePtr))
     {  
      (*theData->getPPForm)(GetCurrentEnvironment(),buffer,299,valuePtr);
      fprintf(fp,"%s\n",buffer);
     }  

   fclose(fp);
  }
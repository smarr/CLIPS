   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*           X Windows Version 2.01  06/15/03          */ 
   /*                                                     */
   /*                  XMENU_EXEC MODULE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Bebe Ly                                              */
/*      Daniel J. McCoy                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Gary D. Riley                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#include <stdio.h>
#include "xsetup.h"

#include "xclipstext.h"
#include "xmenu_exec.h"
#include "xmain.h"
#include "xmenu.h"

#include "setup.h"
#include "router.h"
#include "factmngr.h"
#include "commline.h"

/********** local functions not visible outside this file **********/
static void ResetClips(Widget,XtPointer,XtPointer);
static void ClearClips(Widget,XtPointer,XtPointer);

/*******************************************************************************
          Name:        ResetCallback
          Description: Called when Reset is selected form Execution menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void ResetCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
  MoveEndOfFile(dialog_text, &TheEvent);
  if(EnvGetNextFact(theEnv,NULL))
    {
    Widget confirmshell, confirm;

    confirmshell = XtCreatePopupShell("Confirmation",
                                      topLevelShellWidgetClass,
                                      toplevel,
                                      NULL, 0);

    XtSetArg(TheArgs[0], XtNlabel, "The fact list\nis not empty!");
    XtSetArg(TheArgs[1], XtNicon, clips_logo);
    confirm = XtCreateManagedWidget("confirm",
                                    dialogWidgetClass,
                                    confirmshell, TheArgs, 2);

    XawDialogAddButton(confirm, "Reset", ResetClips, (XtPointer) confirm);
    XawDialogAddButton(confirm, "Cancel", CancelPopupSelect,
                       (XtPointer) confirm);

    XtPopup(confirmshell, XtGrabNonexclusive);
    }

  else
    {
    PrintCLIPS("wclips", "(reset)\n");
    SetCommandString(GetCurrentEnvironment(),"(reset)\n");

    /* ============================================ */
    /*  Set this flag to True to break out of the   */
    /*  event loop so CLIPS could process the       */
    /*  command.                                    */
    /* ============================================ */
 
    quit_get_event = True;
    }
  }

/*******************************************************************************
          Name:        RunCallback
          Description: Called when Run is selected form Execution menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void RunCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
/*  if(CommandLineData(GetCurrentEnvironment())->EvaluatingTopLevelCommand)
   return;*/
  MoveEndOfFile(dialog_text, &TheEvent);
  PrintCLIPS("wclips", "(run)\n");
  SetCommandString(GetCurrentEnvironment(),"(run)\n");
    /* ============================================ */
    /*  Set this flag to True to break out of the   */
    /*  event loop so CLIPS could process the       */ 
    /*  command.                                    */
    /* ============================================ */

  quit_get_event = True;
 
  }

/*******************************************************************************
          Name:        StepCallback
          Description: Called when Step is selected form Execution menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void StepCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
/*  if(CommandLineData(GetCurrentEnvironment())->EvaluatingTopLevelCommand)
   return;*/
  MoveEndOfFile(dialog_text, &TheEvent);
  PrintCLIPS("wclips", "(run 1)\n");
  SetCommandString(GetCurrentEnvironment(),"(run 1)\n");
    /* ============================================ */
    /*  Set this flag to True to break out of the   */
    /*  event loop so CLIPS could process the       */ 
    /*  command.                                    */
    /* ============================================ */

   quit_get_event = True;
  }

/*******************************************************************************
          Name:        ClearCLIPSCallback
          Description: Called when Clear CLIPS is selected form Execution menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void ClearCLIPSCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
  Widget confirmshell, confirm;

/*  if(CommandLineData(GetCurrentEnvironment())->EvaluatingTopLevelCommand)
   return;*/
  confirmshell = XtCreatePopupShell("Confirmation",
                                    topLevelShellWidgetClass,
                                    toplevel,
                                    NULL, 0);

  XtSetArg(TheArgs[0], XtNlabel, "Clear CLIPS!\nAre you sure?");
  XtSetArg(TheArgs[1], XtNicon, clips_logo);
  confirm = XtCreateManagedWidget("confirm",
                                  dialogWidgetClass,
                                  confirmshell,
                                  TheArgs, 2);

  XawDialogAddButton(confirm, "Clear", ClearClips, (XtPointer) confirm);
  XawDialogAddButton(confirm, "Cancel", CancelPopupSelect, (XtPointer) confirm);

  XtPopup(confirmshell, XtGrabNonexclusive);

    /* ============================================ */
    /*  Set this flag to True to break out of the   */
    /*  event loop so CLIPS could process the       */ 
    /*  command.                                    */
    /* ============================================ */

  quit_get_event = True;
  }

/*******************************************************************************
          Name:        ResetClips
          Description: Calls the command `reset' in CLIPS
          Arguments:  w - Not Used
                       client_data - Child of widget to destroy
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
static void ResetClips(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
  XtDestroyWidget(XtParent((Widget) client_data));
  PrintCLIPS("wclips","(reset)\n");
  SetCommandString(GetCurrentEnvironment(),"(reset)\n");
    /* ============================================ */
    /*  Set this flag to True to break out of the   */
    /*  event loop so CLIPS could process the       */ 
    /*  command.                                    */
    /* ============================================ */

  quit_get_event = True;
  }

/*******************************************************************************
          Name:        ClearClips
          Description: Calls the `clear' command in CLIPS
          Arguments:  w - Not Used
                       client_data - Not Used
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
static void ClearClips(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
  XtDestroyWidget(XtParent((Widget) client_data));
  PrintCLIPS("wclips","(clear)\n");
  SetCommandString(GetCurrentEnvironment(),"(clear)\n");
    /* ============================================ */
    /*  Set this flag to True to break out of the   */
    /*  event loop so CLIPS could process the       */ 
    /*  command.                                    */
    /* ============================================ */

  quit_get_event = True;
  }

/*******************************************************************************
 *  ClearScreenCallback
 *  Description: is called when a clear screen is requested
 *  Input : unused
 *******************************************************************************/
void ClearScreenCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
    int n = 0;
    
    XtSetArg(TheArgs[n],XtNstring,"");n++;
    XtSetValues(dialog_text,TheArgs,n);
    PrintPrompt(GetCurrentEnvironment());
  }


   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*           X Windows Version 2.01  06/15/03          */ 
   /*                                                     */
   /*                    XMENU MODULE                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains all the functions that create top level */
/*   menus.                                                  */
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

#define _XMENU_SOURCE_

#include <stdio.h>
#include <errno.h>
#include <unistd.h>

#include "setup.h"

#include "constrct.h"
#include "filecom.h"  

#include "xsetup.h"
#include "xclips.h"
#include "xmenu.h"
#include "xedit.h"
#include "xclipstext.h"
#include "xmenu_wind.h"
#include "xmenu_exec.h"
#include "xmenu_file.h"
#include "xmenu_watch.h"
#include "xmenu_opt.h" 
#include "xmenu_exec.h"
#include "xmain.h" 

#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/dir.h>

/********** local functions not visible outside this function **********/
static void AboutXCLIPS(Widget,XtPointer,XtPointer);
static void CreateFileMenu(Widget);
static void CreateExecutionMenu(Widget);
static void CreateBrowseMenu(Widget);
static void CreateWindowsMenu(Widget);

/********** Variables defined in this file is available to others ***********/

Widget defrule_manager = NULL, deffact_manager = NULL, deftemplate_manager = NULL,
       deffunction_manager = NULL, defgeneric_manager = NULL, definstances_manager = NULL,
       defclass_manager = NULL, agenda_manager = NULL,defglobal_manager = NULL;

Widget button_form, button;

Widget FileItemWidgets[7];
Widget ExecItemWidgets[5];

String about_info[] =
  {
  "XCLIPS for CLIPS version 6.30",
  "",
  "Developers:",
  "",
  "     XCLIPS:  BeBe Ly & Daniel McCoy",
  "",
  "     CLIPS:   Gary Riley & Brian Dantes  ",
  "",
  NULL,
  };

/*******************************************************************************
          Name:        CreatePullDownMenus
          Description: Creates all pulldown menus
          arguements:  parent - widget all menu buttons will be in
          Returns:     None
*******************************************************************************/
void CreatePullDownMenus(
  Widget parent)
  {
   button_form = XtCreateManagedWidget("buttonForm",formWidgetClass,
                                       parent,NULL, 0);
   XtSetArg(TheArgs[0], XtNbitmap, clips_logo);
   XtSetArg(TheArgs[1], XtNinternalHeight, 0);
   XtSetArg(TheArgs[2], XtNinternalWidth, 0);
   XtSetArg(TheArgs[3], XtNshapeStyle, XmuShapeOval);
   XtSetArg(TheArgs[4], XtNborderWidth, 0);
   button = XtCreateManagedWidget("button",commandWidgetClass,
                                  button_form,TheArgs, 5);

   XtAddCallback(button, XtNcallback, AboutXCLIPS, NULL);

   CreateFileMenu(button_form);
   CreateExecutionMenu(button_form);
   CreateBrowseMenu(button_form);
   CreateWindowsMenu(button_form);
  }

/*******************************************************************************
          Name:        CreateFileMenu
          Description: Creates File menu
          arguements:  parent - form widget the menu button will be in
          Returns:     None
*******************************************************************************/
static void CreateFileMenu(
  Widget parent)
  {
   Widget line, menu;
   int i = 0;

   XtSetArg(TheArgs[0], XtNfromHoriz, button);
   XtSetArg(TheArgs[1], XtNlabel, "File");
   button = XtCreateManagedWidget("button",menuButtonWidgetClass,
                                  parent,TheArgs, 2);

   menu = XtCreatePopupShell("menu",simpleMenuWidgetClass,
                             button,NULL, 0);

   XtSetArg(TheArgs[0], XtNleftMargin, 15);

   /*====================================*/
   /* Create Edit item in the file menu. */
   /*====================================*/

   FileItemWidgets[i] = XtCreateManagedWidget("Edit...         ^V",
                                smeBSBObjectClass,menu,TheArgs,1);
   XtAddCallback(FileItemWidgets[i++], XtNcallback, EditCallback, NULL);

   /*========================================*/
   /* Create Complete item in the file menu. */
   /*========================================*/

   FileItemWidgets[i] = XtCreateManagedWidget("Complete...     ^C",smeBSBObjectClass,
                                              menu,TheArgs, 1);
   XtAddCallback(FileItemWidgets[i++], XtNcallback, CompletionDialogCallback,(XtPointer)NULL);
  
   line = XtCreateManagedWidget("line",smeLineObjectClass,menu,NULL,0);

   /*====================================*/
   /* Create Load item in the file menu. */
   /*====================================*/

   FileItemWidgets[i] = XtCreateManagedWidget("Load...         ^L",
                                              smeBSBObjectClass,menu,TheArgs,1);
   XtAddCallback(FileItemWidgets[i++], XtNcallback, LoadRulesCallback, NULL);

   /*==========================================*/
   /* Create Load batch item in the file menu. */
   /*==========================================*/

   FileItemWidgets[i] = XtCreateManagedWidget("Load Batch...",
                                smeBSBObjectClass,
                                menu,
                                TheArgs, 1);
   XtAddCallback(FileItemWidgets[i++], XtNcallback, LoadBatchCallback, NULL);

   /*===========================================*/
   /* Create Load Binary item in the file menu  */
   /*===========================================*/

   FileItemWidgets[i] = XtCreateManagedWidget("Load Binary...",
                                smeBSBObjectClass,
                                menu,
                                TheArgs, 1);
   XtAddCallback(FileItemWidgets[i++], XtNcallback, LoadBinaryCallback, NULL);

   /*===========================================*/
   /* Create Dribble item in the file menu      */
   /*===========================================*/

   file_dribble = XtCreateManagedWidget("Dribble...      ^D",
                                       smeBSBObjectClass,
                                       menu,
                                       TheArgs, 1);
   XtAddCallback(file_dribble, XtNcallback, DribbleCallback, NULL);

   line = XtCreateManagedWidget("line",
                               smeLineObjectClass,
                               menu,
                               NULL, 0);
   /*===========================================*/
   /* Create Save binary item in the file menu  */
   /*===========================================*/

   FileItemWidgets[i] = XtCreateManagedWidget("Save Binary...",
                                smeBSBObjectClass,
                                menu,
                                TheArgs, 1);
   XtAddCallback(FileItemWidgets[i++], XtNcallback, SaveBinaryCallback, NULL);

   line = XtCreateManagedWidget("line",
                               smeLineObjectClass,
                               menu,
                               NULL, 0);
   /*===========================================*/
   /* Create Quit item in the file menu         */
   /*===========================================*/

   FileItemWidgets[i] = XtCreateManagedWidget("Quit            ^Q",
                                smeBSBObjectClass,
                                menu,
                                TheArgs, 1);
   XtAddCallback(FileItemWidgets[i], XtNcallback, QuitCallback, NULL);
  }

/*******************************************************************************
          Name:        CreateExecutionMenu
          Description: Creates Execution menu
          arguements:  parent - form widget the menu button will be in
          Returns:     None
*******************************************************************************/
static void CreateExecutionMenu(
  Widget parent)
  {
   Widget line, menu, entry;
   int i = 0;

   XtSetArg(TheArgs[0], XtNfromHoriz, button);
   XtSetArg(TheArgs[1], XtNlabel, "Execution");
   button = XtCreateManagedWidget("button",
                                 menuButtonWidgetClass,
                                 parent,
                                 TheArgs, 2);

   menu = XtCreatePopupShell("menu",
                             simpleMenuWidgetClass,
                             button,
                             NULL, 0);

   ExecItemWidgets[i] = XtCreateManagedWidget("Reset        ^E",
                                smeBSBObjectClass,
                                menu,
                                NULL, 0);
   XtAddCallback(ExecItemWidgets[i], XtNcallback, ResetCallback, NULL);
   i++;
   ExecItemWidgets[i] = XtCreateManagedWidget("Run          ^R",
                                smeBSBObjectClass,
                                menu,
                                NULL, 0);
   XtAddCallback(ExecItemWidgets[i], XtNcallback, RunCallback, NULL);
   i++;
   ExecItemWidgets[i] = XtCreateManagedWidget("Step         ^T",
                                smeBSBObjectClass,
                                menu,
                                NULL, 0);
   XtAddCallback(ExecItemWidgets[i], XtNcallback, StepCallback, NULL);
   i++;
   entry = XtCreateManagedWidget("Watch...",
                                smeBSBObjectClass,
                                menu,
                                NULL, 0);
  
   XtAddCallback(entry,XtNcallback,WatchWindow,NULL);
  
   entry = XtCreateManagedWidget("Options...",
                                smeBSBObjectClass,
                                menu,
                                NULL, 0);
   XtAddCallback(entry,XtNcallback,OptionsWindow,NULL);
   line =  XtCreateManagedWidget("line",
                               smeLineObjectClass,
                               menu,
                               NULL, 0);

   ExecItemWidgets[i] = XtCreateManagedWidget("Clear CLIPS  ^K",
                                smeBSBObjectClass,
                                menu,
                                NULL, 0);
   XtAddCallback(ExecItemWidgets[i], XtNcallback, ClearCLIPSCallback, NULL);
   i++;
   ExecItemWidgets[i] = XtCreateManagedWidget("Clear Window ^N",
                                smeBSBObjectClass,
                                menu,
                                NULL, 0);
   XtAddCallback(ExecItemWidgets[i],XtNcallback,ClearScreenCallback,NULL);
  }

/*******************************************************************************
          Name:        CreateBrowseMenu
          Description: Creates Browse menu
          arguements:  parent - form widget the menu button will be in
          Returns:     None
*******************************************************************************/
static void CreateBrowseMenu(
  Widget parent)
  {
   Widget menu,entry,line;

   XtSetArg(TheArgs[0], XtNfromHoriz, button);
   XtSetArg(TheArgs[1], XtNlabel, "Browse");
   button = XtCreateManagedWidget("button",
                                 menuButtonWidgetClass,
                                 parent,
                                 TheArgs, 2);
   menu = XtCreatePopupShell("menu",
                            simpleMenuWidgetClass,
                            button,
                            NULL, 0);

   entry = XtCreateManagedWidget("Module...",
                                smeBSBObjectClass,
                                menu,
                                NULL, 0);
   XtAddCallback(entry, 
                 XtNcallback,
                 ModuleCallback,
                 NULL);

   line =  XtCreateManagedWidget("line",
                               smeLineObjectClass,
                               menu,
                               NULL, 0);

   XtSetArg(TheArgs[0], XtNleftMargin, 15);
   XtSetArg(TheArgs[1], XtNsensitive, False);

   defrule_manager = XtCreateManagedWidget("Defrule Manager...",
                                          smeBSBObjectClass,
                                          menu,
                                          TheArgs, 2);
   XtAddCallback(defrule_manager, XtNcallback, DefruleManagerCallback, NULL);

   deffact_manager = XtCreateManagedWidget("Deffacts Manager...",
                                          smeBSBObjectClass,
                                          menu,
                                          TheArgs, 2);
   XtAddCallback(deffact_manager, XtNcallback, DeffactManagerCallback, NULL);

   deftemplate_manager = XtCreateManagedWidget("Deftemplates Manager...",
                                              smeBSBObjectClass,
                                              menu,
                                              TheArgs, 2);
   XtAddCallback(deftemplate_manager, XtNcallback,
                DeftemplateManagerCallback, NULL);

   deffunction_manager = XtCreateManagedWidget("Deffunction Manager...",
                                              smeBSBObjectClass,
                                              menu,
                                              TheArgs, 2);
   XtAddCallback(deffunction_manager, XtNcallback,
                DeffunctionManagerCallback, NULL);

   defglobal_manager = XtCreateManagedWidget("Defglobal Manager...",
                                            smeBSBObjectClass,
                                              menu,
                                              TheArgs, 2);
   XtAddCallback(defglobal_manager, XtNcallback,
                DefglobalManagerCallback, NULL);

   defgeneric_manager = XtCreateManagedWidget("Defgeneric Manager...",
                                             smeBSBObjectClass,
                                             menu,
                                             TheArgs, 2);
   XtAddCallback(defgeneric_manager, XtNcallback,
                DefgenericManagerCallback, NULL);

   defclass_manager = XtCreateManagedWidget("Defclass Manager...",
                                           smeBSBObjectClass,
                                           menu,
                                           TheArgs, 2);
   XtAddCallback(defclass_manager, XtNcallback, DefclassManagerCallback, NULL);

   definstances_manager = XtCreateManagedWidget("Definstances Manager...",
                                               smeBSBObjectClass,
                                               menu,
                                               TheArgs, 2);
   XtAddCallback(definstances_manager, XtNcallback,
                DefinstancesManagerCallback, NULL);

   agenda_manager = XtCreateManagedWidget("Agenda Manager...",
                                         smeBSBObjectClass,
                                         menu,
                                         TheArgs, 1);
   XtAddCallback(agenda_manager, XtNcallback, AgendaManagerCallback, NULL);
  }

/*******************************************************************************
          Name:        CreateWindowsMenu
          Description: Creates the Windows menu
          arguements:  parent - form widget the menu button will be in
          Returns:     None
*******************************************************************************/
static void CreateWindowsMenu(
  Widget parent)
  {
   Widget line,menu, entry,all,none;

   XtSetArg(TheArgs[0], XtNfromHoriz, button);
   XtSetArg(TheArgs[1], XtNlabel, "Windows");
   button = XtCreateManagedWidget("button",
                                 menuButtonWidgetClass,
                                 parent,
                                 TheArgs, 2);

   menu = XtCreatePopupShell("menu",
                            simpleMenuWidgetClass,
                            button,
                            NULL, 0);

  /*==================================*/
  /* Create the "Fact Window" button. */
  /*==================================*/

  XtSetArg(TheArgs[0], XtNleftMargin, 15);
  facts_window = XtCreateManagedWidget("Facts Window",
                                       smeBSBObjectClass,
                                       menu,
                                       TheArgs, 1);
  XtAddCallback(facts_window, XtNcallback, FactsWindowCallback, NULL);

  /*====================================*/
  /* Create the "Agenda Window" button. */
  /*====================================*/

  agenda_window = XtCreateManagedWidget("Agenda Window",
                                        smeBSBObjectClass,
                                        menu,
                                        TheArgs, 1);
  XtAddCallback(agenda_window, XtNcallback, AgendaWindowCallback, NULL);
 
  /*=======================================*/
  /* Create the "Instances Window" button. */
  /*=======================================*/

  instances_window =  XtCreateManagedWidget("Instances Window",
                                        smeBSBObjectClass,
                                        menu,
                                        TheArgs, 1);

  XtAddCallback(instances_window, XtNcallback,InstancesWindowCallback,NULL);

  /*===========================================*/
  /* Create the "Global Window" button        */
  /*===========================================*/

  globals_window =  XtCreateManagedWidget("Global Window",
                                        smeBSBObjectClass,
                                        menu,
                                        TheArgs, 1);
  XtAddCallback(globals_window, XtNcallback,GlobalsWindowCallback,NULL);

  /*===========================================*/
  /* Create the "Focus Window" button         */
  /*===========================================*/

  focus_window = XtCreateManagedWidget("Focus Window",
                                        smeBSBObjectClass,
                                        menu,
                                        TheArgs, 1);
  XtAddCallback(focus_window, XtNcallback,FocusWindowCallback,NULL);

  line = XtCreateManagedWidget("line", smeLineObjectClass, menu, NULL, 0);

  /*===========================================*/
  /* Create the "All Windows" button          */
  /*===========================================*/

  all = XtCreateManagedWidget("All Windows",
                                        smeBSBObjectClass,
                                        menu,
                                        TheArgs, 1);
  XtAddCallback(all,XtNcallback,AllWindowsCallback,NULL);

  /*===========================*/
  /* Create the "None" button. */
  /*===========================*/

  none = XtCreateManagedWidget("None",
                                smeBSBObjectClass,
                                menu,
                                TheArgs, 1);
  XtAddCallback(none,XtNcallback,NoWindowsCallback,NULL);
  line = XtCreateManagedWidget("line", smeLineObjectClass, menu, NULL, 0);

  /*===========================================*/
  /* Create the "Command Line CLIPS" button   */
  /*===========================================*/


  entry = XtCreateManagedWidget("Command Line CLIPS ^Z",
                                smeBSBObjectClass,
                                menu,
                                TheArgs, 1);
  XtAddCallback(entry, XtNcallback, CommandLineCLIPSCallback, NULL);

  /*======================================*/
  /* Create the "Color Utilities" button. */
  /*======================================*/

  entry = XtCreateManagedWidget("Color Utility",
                                smeBSBObjectClass,
                                menu,
                                TheArgs, 1);
  XtAddCallback(entry, XtNcallback, ColorUtilityCallback, NULL);

  }

/*******************************************************************************
          Name:        AboutXCLIPS
          Description: Called when CLIPS logo is selected form menu form.
                       It displays the general information about CLIPS.

          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
static void AboutXCLIPS(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
   Widget about, about_form, about_list;

   about = XtCreatePopupShell("About XCLIPS",
                             topLevelShellWidgetClass,
                             toplevel,
                             NULL, 0);

   XtSetArg(TheArgs[0], XtNdefaultDistance, 0);
   about_form = XtCreateManagedWidget("about_form",
                                     formWidgetClass,
                                     about,
                                     TheArgs, 1);

   XtSetArg(TheArgs[0], XtNborderWidth, 0);
   XtSetArg(TheArgs[1], XtNdefaultColumns, 1);
   XtSetArg(TheArgs[2], XtNforceColumns, True);
   XtSetArg(TheArgs[3], XtNlist, about_info);
   XtSetArg(TheArgs[4], XtNallowVert, True);
   XtSetArg(TheArgs[5], XtNallowHoriz, True);
   about_list = XtCreateManagedWidget("menu",
                                     listWidgetClass,
                                     about_form,
                                     TheArgs, 6);
   XtAddCallback(about_list, XtNcallback, CancelPopupSelect,
                (XtPointer)about_form);

   XtPopup(about, XtGrabNone);
  }

/*******************************************************************************
          Name:        DialogReturn
          Description: This function will be executed when return was pressed
                       while cursor is in the dialog box of the file select
                       window.
          Arguments:  w - Widget that caused action to be called
                       event - Not used
                       params - Dialog widget
                       num_params - Not used
          Returns:     None
*******************************************************************************/
void DialogReturn(
  Widget w,
  XEvent *event,
  String *params,
  Cardinal *num_params)
  {
   MenuFunc(w, params, (XtPointer)NULL);
  }

/*******************************************************************************
          Name:        MenuFunc
          Description: Simulates callbacks for the dialog box of the file
                       select window. which callback function is executed
                       depends on which menu item from the file menu of 
                       the main CLIPS window is activated.
          Arguments:  w - Dialog widget
                       client_data - Dialog widget
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
void MenuFunc(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   String filename = XawDialogGetValueString(XtParent(w));
   char fullpath[255];
   Widget popup = XtParent(XtParent(XtParent(w)));

   if (filename[0] == 0)
     { return; }
         
   chdir(path);
   strcpy(fullpath, path);
   strcat(fullpath, filename);
   MoveEndOfFile(dialog_text, &TheEvent);

   switch(file_item)
     {
      case EDIT:
        XtDestroyWidget(popup);
        EditNewFile(w, client_data, call_data);
        break;

      case LOADBATCH:
        XtDestroyWidget(popup);
        LoadBatch(fullpath);
        break;

      case LOADBINARY:
        XtDestroyWidget(popup);
        LoadBinary(fullpath);
        break;

      case LOADFACTS:
        XtDestroyWidget(popup);
        LoadTheFacts(fullpath);
        break;

      case LOADRULES:
        XtDestroyWidget(popup);
        LoadRules(fullpath);
        break;

      case DRIBBLEON:
        XtDestroyWidget(popup);
        EnvDribbleOn(theEnv,fullpath);
        break;

      case SAVERULES:
        IntSave(w,client_data,call_data);
        break;

      case SAVEBINARY:
        IntSave(w,client_data,call_data);
        break;

      case SAVEFACTS:
        IntSave(w, client_data, call_data);
        break;
     }
  }

/*******************************************************************************
          Name:        CancelPopupSelect
          Description: Destroys a the parent of the widget sent
          Arguments:  w - Not Used
                       client_data - Child of widget to destroy
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
void CancelPopupSelect(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   XtDestroyWidget(XtParent((Widget) client_data));
  }


/*******************************************************************************
          Name:        PopdownSelect
          Description: Popdown the parent of the widget sent
          Arguments:  w - Not Used
                       client_data - Child of widget to destroy
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
void PopdownSelect(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   XtPopdown(XtParent((Widget) client_data));
  }

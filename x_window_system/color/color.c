   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*           X Windows Version 2.0  01/31/02           */
   /*                                                     */
   /*                 COLOR UTILITY MODULE                */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Bebe Ly                                              */
/*      Daniel J. McCoy                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#include <stdio.h>
#include <stdlib.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#include <X11/Xaw/Command.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Viewport.h>

#include <X11/Xaw/Cardinals.h>

#include "colors.h"
#include "xdefault.h"

#define streq(a, b) (strcmp((a), (b)) == 0)

static void MakeTopCommandButtons(void);
static void MakeColorList(void);
static void MakeDummyCLIPSInterface(void);
static void MakeBottomCommandButtons(void);
static void MakePopups(void);
static void Initialize(Widget);
static void CreateCallbacks();
static void Choice(Widget,XtPointer,XtPointer);
static void SetGround(Widget,XtPointer,XtPointer);
static void Defaults(Widget,XtPointer,XtPointer);
static void SetBox(Widget,XtPointer,XtPointer);
static int Convert(Widget,char *);
static void Quit(Widget,XtPointer,XtPointer);
static void Saveyes(Widget,XtPointer,XtPointer);
static void Saveno(Widget,XtPointer,XtPointer);
static void ChooseFile(void);
static void NextSave(Widget,XtPointer,XtPointer);
static void Cancel(Widget,XtPointer,XtPointer);
static void Quitno(Widget,XtPointer,XtPointer);

String fallback_resources[] =
{
  "*background:           gray",

  "*list.foreground:      white",
  "*list.background:      blue",

  "*Command*foreground:   white",
  "*Command*background:   blue",
  "*Command*borderWidth:  0",
  "*Command*width:        105",
  "*Command*height:       26",

  "*Toggle*foreground:    white",
  "*Toggle*background:    blue",
  "*Toggle*borderWidth:   0",
  "*Toggle*width:         105",
  "*Toggle*height:        26",

  "*Label*internalHeight: 0",
  "*Label*internalWidth:  0",
  "*Label*borderWidth:    4",

  "*Button.background:    white",
  "*Cancel.background:    white",
  NULL};

static String resourcestring[NUMRES+1] = {
  "Xclips*dialog_text*foreground:",
  "Xclips*dialog_text*background:",
  "Xclips*dialog_text*borderColor:",
  "Xclips*facts_text*foreground:",
  "Xclips*facts_text*background:",
  "Xclips*facts_text*borderColor:",
  "Xclips*instances_text*foreground:",
  "Xclips*instances_text*background:",
  "Xclips*instances_text*borderColor:",
  "Xclips*globals_text*foreground:",
  "Xclips*globals_text*background:",
  "Xclips*globals_text*borderColor:",
  "Xclips*agenda_text*foreground:",
  "Xclips*agenda_text*background:",
  "Xclips*agenda_text*borderColor:",
  "Xclips*focus_text*foreground:",
  "Xclips*focus_text*background:",
  "Xclips*focus_text*borderColor:",
  "Xclips*buttonForm.background:",
  "Xclips*MenuButton.foreground:",
  "Xclips*MenuButton.background:",
  "Xclips*MenuButton.borderColor:",
  "Xclips*SmeBSB.foreground:",
  "Xclips*line.foreground:",
  "Xclips*menu.foreground:",
  "Xclips*menu.background:",
  "Xclips*menu.borderColor:",
  "Xclips*manager_list.foreground:",
  "Xclips*manager_list.background:",
  "Xclips*manager_form.background:",
  "Xclips*manager_viewport.borderColor:",
  "Xclips*confirm*foreground:",
  "Xclips*confirm*background:",
  "Xclips*file_dialog*foreground:",
  "Xclips*file_dialog*background:",
  "Xclips*file_form.background:",
  "Xclips*managerButton.foreground:",
  "Xclips*managerButton.background:",
  "Xclips*managerButton.borderColor:",
  "Xclips*managerCancel.foreground:",
  "Xclips*managerCancel.background:",
  "Xclips*managerCancel.borderColor:",
  "Xclips*watch_form.foreground:",
  "Xclips*watch_form.background:",
  "Xclips*watch_form.borderColor:",
  "Xclips*Toggle.foreground:",
  "Xclips*Toggle.background:",
  "Xclips*Toggle.borderColor:",
  "Xclips*watchButton.foreground:",
  "Xclips*watchButton.background:",
  "Xclips*watchButton.borderColor:",
  NULL};

static String temp[1] = {"                                    "};

static String resourcecolor[NUMRES+1];

Widget root, form, viewport, list;

Widget foreground, background, border, defaults, quit;

Widget button_form, menuButton1, menuButton2, 
       menuButton3, menuButton4, menu_label, 
       dialog_label, facts_label,instances_label,
       globals_label, agenda_label, focus_label,
       manager_button_label, manager_cancel_label, 
       manager_label, confirm_label, file_label,
       watch_options_label,W_O_toggle_label,W_O_button_label;

Widget file, dialog, facts, instances,globals,focus, 
       agenda, buttonBox, buttons, menu, manager, 
       confirm, manager_buttons,watch_options,watch_option_buttons;

Widget QUIT, quitform, quitno, saveyes, 
       saveno, filesave, file_form, file_dialog;

XtAppContext app_con;
Arg args[5];
int ground_setting = FOREGROUND,
    widget_setting = DIALOG_WIN,
    i;
Position x, y;
FILE *f;
char filename[60];
/*****************************************************************
 *              MAIN
 *****************************************************************
 */
int main(
  int argc,
  char **argv)
  {
  for (i = 0; i < NUMRES; i++)
    resourcecolor[i] = XtMalloc(40);

  (void) strcpy(filename, getenv("HOME"));
  (void) strcat(filename, "/Xclips");

  XtSetArg(args[0], XtNminWidth, 709);
  XtSetArg(args[1], XtNminHeight, 506);
  XtSetArg(args[2], XtNmaxWidth, 709);
  XtSetArg(args[3], XtNmaxHeight, 506);
  root = XtAppInitialize(&app_con, "color", NULL, 0, &argc, argv, fallback_resources, args, 4);

  MakeColorList();

  MakeTopCommandButtons();

  MakeDummyCLIPSInterface();

  MakeBottomCommandButtons();

  MakePopups();

  Initialize(root);

  CreateCallbacks();

  XtRealizeWidget(root);
  XtAppMainLoop(app_con);

  return(-1);
  }

/********************************************************************************
          Name:        MakeColorList
          Description: Creates main window and places list of color selections
                       in window
          Arguements:  None
          Returns:     None
********************************************************************************/
static void MakeColorList()
  {
  form = XtCreateManagedWidget("form", formWidgetClass, root, NULL, 0);

  XtSetArg(args[0], XtNheight, 520);
  XtSetArg(args[1], XtNallowVert, True);
  XtSetArg(args[2], XtNborderWidth, 0);
  viewport = XtCreateManagedWidget("viewport", viewportWidgetClass, form, args, 3);

  XtSetArg(args[0], XtNlist, items);
  XtSetArg(args[1], XtNdefaultColumns, 1);
  XtSetArg(args[2], XtNforceColumns, True);
  list = XtCreateManagedWidget("list", listWidgetClass, viewport, args, 3);
  }

/********************************************************************************
          Name:        MakeTopCommandButtons
          Description: Creates top row of command widgets for Foreground,
                       Background, Border, Default, and Quit/Save
          Arguements:  None
          Returns:     None
********************************************************************************/
static void MakeTopCommandButtons()
  {
  XtSetArg(args[0], XtNfromHoriz, viewport);
  XtSetArg(args[1], XtNradioGroup, border);
  XtSetArg(args[2], XtNstate, True);
  foreground = XtCreateManagedWidget("Foreground", toggleWidgetClass, form, args, 3);

  XtSetArg(args[0], XtNfromHoriz, foreground);
  XtSetArg(args[1], XtNradioGroup, foreground);
  background = XtCreateManagedWidget("Background", toggleWidgetClass, form, args, 2);

  XtSetArg(args[0], XtNfromHoriz, background);
  XtSetArg(args[1], XtNradioGroup, background);
  border = XtCreateManagedWidget("Border", toggleWidgetClass, form, args, 2);

  XtSetArg(args[0], XtNfromHoriz, border);
  defaults = XtCreateManagedWidget("Default", commandWidgetClass, form, args, 1);

  XtSetArg(args[0], XtNfromHoriz, defaults);
  quit = XtCreateManagedWidget("Quit/Save", commandWidgetClass, form, args, 1);
  }

/********************************************************************************
          Name:        MakeDummyCLIPSInterface
          Description: Creates windows to look like the CLIPS X-Interface
          Arguements:  None
          Returns:     None
********************************************************************************/
static void MakeDummyCLIPSInterface()
{

  /* ========================================= */
  /*  Create the button form and button labels */
  /* ========================================= */

  XtSetArg(args[0], XtNfromHoriz, viewport);
  XtSetArg(args[1], XtNfromVert, foreground);
  XtSetArg(args[2], XtNborderWidth, 0);
  button_form = XtCreateManagedWidget("button_form", formWidgetClass, form, args, 3);

  XtSetArg(args[0], XtNwidth, 63);
  menuButton1 = XtCreateManagedWidget("button", labelWidgetClass, button_form, args, 1);

  XtSetArg(args[1], XtNfromHoriz, menuButton1);
  menuButton2 = XtCreateManagedWidget("button", labelWidgetClass, button_form, args, 2);

  XtSetArg(args[1], XtNfromHoriz, menuButton2);
  menuButton3 = XtCreateManagedWidget("button", labelWidgetClass, button_form, args, 2);

  XtSetArg(args[1], XtNfromHoriz, menuButton3);
  menuButton4 = XtCreateManagedWidget("button", labelWidgetClass, button_form, args, 2);

  /* ======================================= */
  /*  Create the label for the menu          */
  /* ======================================= */

  XtSetArg(args[0], XtNfromHoriz, viewport);
  XtSetArg(args[1], XtNfromVert, button_form);
  XtSetArg(args[2], XtNheight, 100);
  XtSetArg(args[3], XtNwidth, 75);
  menu_label = XtCreateManagedWidget("Menu", labelWidgetClass, form, args, 4);

  /* ======================================= */
  /*  Create the lable for the toggle button */
  /* ======================================= */
  XtSetArg(args[0], XtNfromHoriz,menu_label);
  XtSetArg(args[1], XtNfromVert, button_form);
  W_O_toggle_label = XtCreateManagedWidget("Toggle Button",labelWidgetClass,
                                            form, args,2);

  /* ======================================= */
  /*  Create the lable for command button    */
  /* ======================================= */

  XtSetArg(args[0], XtNfromHoriz,menu_label);
  XtSetArg(args[1], XtNfromVert,W_O_toggle_label);
  W_O_button_label = XtCreateManagedWidget("Command button",labelWidgetClass,form,args,2);

  /* ======================================= */
  /*  Create the lable for the W/O  button   */
  /* ======================================= */


  XtSetArg(args[0], XtNfromHoriz,menu_label);
  XtSetArg(args[1], XtNfromVert, button_form);
  XtSetArg(args[2], XtNheight, 100);
  XtSetArg(args[3], XtNwidth, 150);
  watch_options_label = XtCreateManagedWidget("Watch/Option", labelWidgetClass, form, args, 4);

  /* ======================================= */
  /*  Create the lable for the Clips Dialog  */
  /* ======================================= */

  XtSetArg(args[1], XtNfromVert, button_form);
  XtSetArg(args[0], XtNfromHoriz,viewport);
  XtSetArg(args[2], XtNheight, 250);
  XtSetArg(args[3], XtNwidth, 296);
  dialog_label = XtCreateManagedWidget("Dialog", labelWidgetClass, form, args, 4);
  
  /* ======================================= */
  /*  Create the lable for the fact window   */
  /* ======================================= */

  XtSetArg(args[0], XtNfromHoriz, dialog_label);
  XtSetArg(args[1], XtNfromVert, quit);
  XtSetArg(args[2], XtNheight, 126);
  XtSetArg(args[3], XtNwidth, 100);
  facts_label = XtCreateManagedWidget("Facts", labelWidgetClass, form, args, 4);

  /* ======================================= */
  /*  Create the lable for the instances win */
  /* ======================================= */

  XtSetArg(args[0], XtNfromHoriz, dialog_label);
  XtSetArg(args[1], XtNfromVert,facts_label);
  XtSetArg(args[2], XtNheight, 126);
  XtSetArg(args[3], XtNwidth, 100);
  instances_label = XtCreateManagedWidget("Instances",labelWidgetClass,form,args, 4);
  /* ======================================= */
  /*  Create the lable for the global window */
  /* ======================================= */

  XtSetArg(args[0], XtNfromHoriz, dialog_label);
  XtSetArg(args[1], XtNfromVert,instances_label);
  XtSetArg(args[2], XtNheight, 125);
  XtSetArg(args[3], XtNwidth, 100);
  globals_label = XtCreateManagedWidget("globals",labelWidgetClass,form,args,4);

  /* ======================================= */
  /*  Create the lable for the agenda window */
  /* ======================================= */

  XtSetArg(args[0], XtNfromVert, dialog_label);
  XtSetArg(args[1], XtNfromHoriz, viewport);
  XtSetArg(args[2], XtNheight, 102);
  XtSetArg(args[3], XtNwidth, 196);
  agenda_label = XtCreateManagedWidget("Agenda", labelWidgetClass, form, args, 4);

  /* ===================================== */
  /*  Create the Focus window label        */
  /* ===================================== */

  XtSetArg(args[1],XtNfromHoriz,agenda_label);
  XtSetArg(args[3],XtNwidth,90);
  focus_label = XtCreateManagedWidget("Focus", labelWidgetClass, form, args, 4);

  /* ======================================== */
  /*  Create the label for the manager button */
  /* ======================================== */

  XtSetArg(args[0], XtNfromHoriz, facts_label);
  XtSetArg(args[1], XtNfromVert, foreground);
  manager_button_label = XtCreateManagedWidget("Button", labelWidgetClass, form, args, 2);

  /* ======================================== */
  /*  Create the label for the cancel button  */
  /* ======================================== */

  XtSetArg(args[0], XtNfromHoriz, facts_label);
  XtSetArg(args[1], XtNfromVert, manager_button_label);
  manager_cancel_label = XtCreateManagedWidget("Cancel", labelWidgetClass, form, args, 2);

  XtSetArg(args[0], XtNfromHoriz, facts_label);
  XtSetArg(args[1], XtNfromVert, foreground);
  XtSetArg(args[2], XtNheight, 193);
  XtSetArg(args[3], XtNwidth, 115);
  manager_label = XtCreateManagedWidget("Manager", labelWidgetClass, form, args, 4);

  XtSetArg(args[1], XtNfromVert, manager_label);
  XtSetArg(args[2], XtNheight, 98);
  XtSetArg(args[3], XtNwidth, 123);
  XtSetArg(args[4], XtNborderWidth, 0);
  confirm_label = XtCreateManagedWidget("Confirmation", labelWidgetClass, form, args, 5);

  XtSetArg(args[1], XtNfromVert, confirm_label);
  XtSetArg(args[2], XtNheight, 98);
  XtSetArg(args[3], XtNwidth, 123);
  XtSetArg(args[4], XtNborderWidth, 0);
  file_label = XtCreateManagedWidget("File Selection", labelWidgetClass, form, args, 5);
  }

/********************************************************************************
          Name:        MakeBottomCommandButtons
          Description: Creates botton rows of command buttons for Dialog Window,
                       Facts Window, Agenda Window, Button Box, Menu Buttons,
                       Pulldown Menus, Managers, Confirmations, File Selection,
                       and Editors
          Arguements:  None
          Returns:     None
********************************************************************************/
static void MakeBottomCommandButtons()
  {
  XtSetArg(args[0], XtNfromHoriz, viewport);
  XtSetArg(args[1], XtNfromVert, agenda_label);
  XtSetArg(args[2], XtNradioGroup, file);
  XtSetArg(args[3], XtNstate, True);
  dialog = XtCreateManagedWidget("Dialog Window", toggleWidgetClass, form, args, 4);

  XtSetArg(args[0], XtNfromHoriz, dialog);
  XtSetArg(args[2], XtNradioGroup, dialog);
  facts = XtCreateManagedWidget("Facts Window", toggleWidgetClass, form, args, 3);

  XtSetArg(args[0], XtNfromHoriz, facts);
  XtSetArg(args[2], XtNradioGroup, facts);
  instances = XtCreateManagedWidget("Instances Win.", toggleWidgetClass, form, args,3);

  XtSetArg(args[0], XtNfromHoriz, instances);
  XtSetArg(args[2], XtNradioGroup, instances);
  globals = XtCreateManagedWidget("Globals Window", toggleWidgetClass, form, args,3);

  XtSetArg(args[0], XtNfromHoriz, globals);
  XtSetArg(args[2], XtNradioGroup, globals);
  agenda = XtCreateManagedWidget("Agenda Window", toggleWidgetClass, form, args, 3);

  XtSetArg(args[0], XtNfromHoriz, viewport);
  XtSetArg(args[1], XtNfromVert, dialog);
  XtSetArg(args[2], XtNradioGroup, agenda);
  focus = XtCreateManagedWidget("Focus Window",toggleWidgetClass, form, args, 3);

  XtSetArg(args[0], XtNfromHoriz, focus);
  XtSetArg(args[1], XtNfromVert, dialog);
  XtSetArg(args[2], XtNradioGroup, agenda);
  buttonBox = XtCreateManagedWidget("Button Box", toggleWidgetClass, form, args, 3);

  XtSetArg(args[0], XtNfromHoriz, buttonBox);
  XtSetArg(args[2], XtNradioGroup, buttonBox);
  buttons = XtCreateManagedWidget("Menu Buttons", toggleWidgetClass, form, args, 3);

  XtSetArg(args[0], XtNfromHoriz, buttons);
  XtSetArg(args[2], XtNradioGroup, buttons);
  menu = XtCreateManagedWidget("Pulldown Menus", toggleWidgetClass, form, args, 3);

  XtSetArg(args[0], XtNfromHoriz, menu);
  XtSetArg(args[2], XtNradioGroup, menu);
  manager = XtCreateManagedWidget("Managers", toggleWidgetClass, form, args, 3);
  
  XtSetArg(args[0], XtNfromHoriz, viewport);
  XtSetArg(args[1], XtNfromVert,focus);
  XtSetArg(args[2], XtNradioGroup, manager);
  manager_buttons = XtCreateManagedWidget("Manager Button", 
                                           toggleWidgetClass,form,args,3);

  XtSetArg(args[0], XtNfromHoriz, manager_buttons);
  XtSetArg(args[2], XtNradioGroup,manager_buttons);
  confirm = XtCreateManagedWidget("Confirmations", 
                                   toggleWidgetClass, form, args, 3);

  XtSetArg(args[0], XtNfromHoriz, confirm);
  XtSetArg(args[2], XtNradioGroup, confirm);
  file = XtCreateManagedWidget("File", toggleWidgetClass, form, args, 3);

  XtSetArg(args[0], XtNfromHoriz,file);
  XtSetArg(args[2], XtNradioGroup,file);
  watch_options = XtCreateManagedWidget("Watch/Options", toggleWidgetClass,form,args,3);

  XtSetArg(args[0], XtNfromHoriz,watch_options);
  XtSetArg(args[2], XtNradioGroup,watch_options);
  watch_option_buttons = XtCreateManagedWidget("Wtch/Opt Bttns",toggleWidgetClass, form,args,3);
  
}

/********************************************************************************
          Name:        MakePopups
          Description: Create pop-up widgets QUIT and filesave
          Arguements:  None
          Returns:     None
********************************************************************************/
static void MakePopups()
  {
  XtSetArg(args[0], XtNborderWidth, 0);
  QUIT = XtCreatePopupShell("QUIT", overrideShellWidgetClass, root, args, 1);
  quitform = XtCreateManagedWidget("quitform", formWidgetClass, QUIT, NULL, 0);
  quitno = XtCreateManagedWidget("Do NOT Quit", commandWidgetClass, quitform, args, 0);
  XtSetArg(args[0], XtNfromVert, quitno);
  saveyes = XtCreateManagedWidget("Save and Quit", commandWidgetClass, quitform, args, 1);
  XtSetArg(args[0], XtNfromVert, saveyes);
  saveno = XtCreateManagedWidget("Quit", commandWidgetClass, quitform, args, 1);

  filesave = XtCreatePopupShell("filesave", overrideShellWidgetClass, root, args, 2);
  XtSetArg(args[0], XtNborderWidth, 0);
  file_form = XtCreateManagedWidget("file_form", formWidgetClass, filesave, args, 1);
  XtSetArg(args[0], XtNvalue, "");
  XtSetArg(args[1], XtNlabel, "ERROR! Enter new [path]filename.");
  XtSetArg(args[2], XtNborderWidth, 0);
  file_dialog = XtCreateManagedWidget("file_dialog", dialogWidgetClass, file_form, args, 3);
  XawDialogAddButton(file_dialog, "OK", NextSave, (XtPointer) file_form);
  XawDialogAddButton(file_dialog, "CANCEL", Cancel, (XtPointer) file_form);
  }

/********************************************************************************
          Name:        CreateCallbacks
          Description: Contains XtAddCallbacks
          Arguements:  None
          Returns:     None
********************************************************************************/
static void CreateCallbacks()
  {
  XtAddCallback(list, XtNcallback, Choice, NULL);
  XtAddCallback(foreground, XtNcallback, SetGround, NULL);
  XtAddCallback(background, XtNcallback, SetGround, NULL);
  XtAddCallback(border, XtNcallback, SetGround, NULL);
  XtAddCallback(defaults, XtNcallback, Defaults, NULL);
  XtAddCallback(quit, XtNcallback, Quit, NULL);
  XtAddCallback(buttonBox, XtNcallback, SetBox, NULL);
  XtAddCallback(buttons, XtNcallback, SetBox, NULL);
  XtAddCallback(dialog, XtNcallback, SetBox, NULL);
  XtAddCallback(facts, XtNcallback, SetBox, NULL);
  XtAddCallback(instances, XtNcallback, SetBox, NULL);
  XtAddCallback(globals, XtNcallback, SetBox, NULL);
  XtAddCallback(agenda, XtNcallback, SetBox, NULL);
  XtAddCallback(focus,XtNcallback,SetBox,NULL);
  XtAddCallback(menu, XtNcallback, SetBox, NULL);
  XtAddCallback(manager, XtNcallback, SetBox, NULL);
  XtAddCallback(confirm, XtNcallback, SetBox, NULL);
  XtAddCallback(file, XtNcallback, SetBox, NULL);
  XtAddCallback(manager_buttons, XtNcallback, SetBox, NULL);
  XtAddCallback(watch_options,XtNcallback,SetBox,NULL);
  XtAddCallback(watch_option_buttons,XtNcallback,SetBox,NULL);
  XtAddCallback(saveyes, XtNcallback, Saveyes, NULL);
  XtAddCallback(saveno, XtNcallback, Saveno, NULL);
  XtAddCallback(quitno, XtNcallback, Quitno, (XtPointer) QUIT);
  }

/********************************************************************************
          Name:        Initialize
          Description: reads the file "Xclips" if able, loads colors
                       into resourcecolor, and sets colors on display
          Arguements:  w - Not used except as a dummy widget
          Returns:     None
********************************************************************************/
static void Initialize(
  Widget w)
  {
  char tempcolor[50]; 
  if ((f = fopen(filename, "r")) != NULL)
    {
    for (i = 0; i < NUMRES; i++)
      fscanf(f, "%s%s", temp[0],  resourcecolor[i]);
    fclose(f);

    XtSetArg(args[0], XtNforeground, Convert(w, resourcecolor[DIALOG_FG]));
    XtSetArg(args[1], XtNbackground, Convert(w, resourcecolor[DIALOG_BG]));
    XtSetArg(args[2], XtNborder, Convert(w, resourcecolor[DIALOG_BDR]));
    XtSetValues(dialog_label, args, 3);
    XtSetArg(args[0], XtNforeground, Convert(w, resourcecolor[FACTS_FG]));
    XtSetArg(args[1], XtNbackground, Convert(w, resourcecolor[FACTS_BG]));
    XtSetArg(args[2], XtNborder, Convert(w, resourcecolor[FACTS_BDR]));
    XtSetValues(facts_label, args, 3);
    XtSetArg(args[0], XtNforeground, Convert(w, resourcecolor[INSTANCES_FG]));
    XtSetArg(args[1], XtNbackground, Convert(w, resourcecolor[INSTANCES_BG]));
    XtSetArg(args[2], XtNborder, Convert(w, resourcecolor[INSTANCES_BDR]));
    XtSetValues(instances_label, args, 3);
    XtSetArg(args[0], XtNforeground, Convert(w, resourcecolor[GLOBALS_FG]));
    XtSetArg(args[1], XtNbackground, Convert(w, resourcecolor[GLOBALS_BG]));
    XtSetArg(args[2], XtNborder, Convert(w, resourcecolor[GLOBALS_BDR]));
    XtSetValues(globals_label, args, 3);
    XtSetArg(args[0], XtNforeground, Convert(w, resourcecolor[AGENDA_FG]));
    XtSetArg(args[1], XtNbackground, Convert(w, resourcecolor[AGENDA_BG]));
    XtSetArg(args[2], XtNborder, Convert(w, resourcecolor[AGENDA_BDR]));
    XtSetValues(agenda_label, args, 3);
    XtSetArg(args[0], XtNforeground, Convert(w, resourcecolor[FOCUS_FG]));
    XtSetArg(args[1], XtNbackground, Convert(w, resourcecolor[FOCUS_BG]));
    XtSetArg(args[2], XtNborder, Convert(w, resourcecolor[FOCUS_BDR]));
    XtSetValues(focus_label, args, 3);
    XtSetArg(args[0], XtNbackground, Convert(w, resourcecolor[BUTTON_FRM_BG]));
    XtSetValues(button_form, args, 1);
    XtSetArg(args[0], XtNforeground, Convert(w, resourcecolor[MENU_BUTTON_FG]));
    XtSetArg(args[1], XtNbackground, Convert(w, resourcecolor[MENU_BUTTON_BG]));
    XtSetArg(args[2], XtNborder, Convert(w, resourcecolor[MENU_BUTTON_BDR]));
    XtSetValues(menuButton1, args, 3);
    XtSetValues(menuButton2, args, 3);
    XtSetValues(menuButton3, args, 3);
    XtSetValues(menuButton4, args, 3);
    XtSetArg(args[0], XtNforeground, Convert(w, resourcecolor[SMEBSB_FG]));
    XtSetArg(args[1], XtNbackground, Convert(w, resourcecolor[MENU_BG]));
    XtSetArg(args[2], XtNborder, Convert(w, resourcecolor[MENU_BDR]));
    XtSetValues(menu_label, args, 3);
    XtSetArg(args[0], XtNforeground, Convert(w, resourcecolor[MANGR_LIST_FG]));
    XtSetArg(args[1], XtNbackground, Convert(w, resourcecolor[MANGR_LIST_BDR]));
    XtSetArg(args[2], XtNborder, Convert(w, resourcecolor[MANGR_VP_BDR]));
    XtSetValues(manager_label, args, 3);
    XtSetArg(args[0], XtNforeground, Convert(w, resourcecolor[CONFIRM_FG]));
    XtSetArg(args[1], XtNbackground, Convert(w, resourcecolor[CONFIRM_BG]));
    XtSetValues(confirm_label, args, 2);
    XtSetArg(args[0], XtNforeground, Convert(w, resourcecolor[FILE_DIALOG_FG]));
    XtSetArg(args[1], XtNbackground, Convert(w, resourcecolor[FILE_DIALOG_BG]));
    XtSetValues(file_label, args, 2);
    XtSetArg(args[0], XtNforeground, Convert(w, resourcecolor[MANGR_BUTTN_FG]));
    XtSetArg(args[2], XtNbackground, Convert(w, resourcecolor[ MANGR_BUTTN_BG]));
    XtSetArg(args[1], XtNborder, Convert(w, resourcecolor[MANGR_BUTTN_BDR]));
    XtSetValues(manager_button_label, args, 3);
    XtSetArg(args[0], XtNforeground, Convert(w, resourcecolor[MANGR_CANCL_FG]));
    XtSetArg(args[2], XtNbackground, Convert(w, resourcecolor[MANGR_CANCL_BG]));
    XtSetArg(args[1], XtNborder, Convert(w, resourcecolor[MANGR_CANCL_BDR]));
    XtSetValues(manager_cancel_label, args, 3);
    XtSetArg(args[0],XtNbackground, Convert(w, resourcecolor[WATCH_FORM_BG]));
    XtSetArg(args[1],XtNborder, Convert(w, resourcecolor[WATCH_FORM_BDR]));
    XtSetArg(args[2],XtNforeground,Convert(w, resourcecolor[WATCH_FORM_FG]));
    XtSetValues(watch_options_label,args,3);
    XtSetArg(args[0],XtNbackground, Convert(w, resourcecolor[TOGGLE_BG]));
    XtSetArg(args[1],XtNborder, Convert(w, resourcecolor[TOGGLE_BDR]));
    XtSetArg(args[2],XtNforeground,Convert(w, resourcecolor[TOGGLE_FG]));
    XtSetValues(W_O_toggle_label,args,3);
    XtSetArg(args[0],XtNbackground, Convert(w, resourcecolor[WATCH_BG]));
    XtSetArg(args[1],XtNborder, Convert(w, resourcecolor[WATCH_BDR]));
    XtSetArg(args[2],XtNforeground,Convert(w, resourcecolor[WATCH_FG]));
    XtSetValues(W_O_button_label,args,3);
    }
  else
    Defaults(w, NULL, NULL);
  }

/********************************************************************************
          Name:        Choice
          Description: Upon selecting a color this will make the proper changes
                       on the display and store the new color
          Arguements:  w - Not used except as a dummy widget
                       client_data - Not used
                       call_data - selected color
          Returns:     None
********************************************************************************/
static void Choice(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
  XawListReturnStruct *item = (XawListReturnStruct*)call_data;
  XrmValue from, to;

  from.size = strlen(item->string) + 1;
  from.addr = item->string;
  XtConvert(w, XtRString, (XrmValuePtr) &from, XtRPixel, (XrmValuePtr) &to);
  if (to.addr != NULL)
    if (ground_setting == FOREGROUND)
      {
      XtSetArg(args[0], XtNforeground, (int) *((Pixel *) to.addr));
      switch (widget_setting)
        {
        case MANAGER_BUTTON : 
                 XtSetArg(args[1], XtNborder, (int) *((Pixel *) to.addr));
                 XtSetValues(manager_button_label, args, 2);
                 resourcecolor[MANGR_BUTTN_FG] = item->string;
                 resourcecolor[MANGR_BUTTN_BDR] = item->string;
                 break;
        case WATCH_OPTION_BUTTONS :
                 XtSetArg(args[1], XtNborder, (int) *((Pixel *) to.addr));
                 XtSetValues(W_O_toggle_label,args,2);
                 resourcecolor[TOGGLE_FG] = item->string;
                 resourcecolor[TOGGLE_BDR] = item->string;
                 break;
        case MENU_BUTTON : 
                 XtSetValues(menuButton1, args, 1);
                 XtSetValues(menuButton2, args, 1);
                 XtSetValues(menuButton3, args, 1);
                 XtSetValues(menuButton4, args, 1);
                 resourcecolor[MENU_BUTTON_FG] = item->string;  
                 break;
        case DIALOG_WIN : 
                 XtSetValues(dialog_label, args, 1);
                 resourcecolor[DIALOG_FG] = item->string;
                 break;
        case FACTS_WIN : 
                 XtSetValues(facts_label, args, 1);
                 resourcecolor[FACTS_FG] = item->string;
                 break;
        case INSTANCES_WIN :
                 XtSetValues(instances_label, args, 1);
                 resourcecolor[INSTANCES_FG] = item->string;
                 break;
        case GLOBALS_WIN :
                 XtSetValues(globals_label, args, 1);
                 resourcecolor[GLOBALS_FG] = item->string;
                 break;
        case AGENDA_WIN : 
                 XtSetValues(agenda_label, args, 1);
                 resourcecolor[AGENDA_FG] = item->string;
                 break;
        case FOCUS_WIN:
                 XtSetValues(focus_label,args,1);
                 resourcecolor[FOCUS_FG] = item->string;
                 break;
        case PULLDOWN_MENUS : 
                 XtSetValues(menu_label, args, 1);
                 resourcecolor[SMEBSB_FG] = item->string;
                 resourcecolor[LINE_FG] = item->string;
                 resourcecolor[MENU_FG] = item->string;
                 break;
        case WATCH_OPTIONS:
                 XtSetValues(watch_options_label, args, 1);
                 resourcecolor[WATCH_FORM_FG] = item->string;
                 break;
        case MANAGERS : 
                 XtSetValues(manager_label, args, 1);
                 resourcecolor[MANGR_LIST_FG] = item->string;
                 break;
        case CONFIRMATION : 
                 XtSetValues(confirm_label, args, 1);
                 resourcecolor[CONFIRM_FG] = item->string;
                 break;
        case FILE_WIN : 
                 XtSetValues(file_label, args, 1);
                 resourcecolor[FILE_DIALOG_FG] = item->string;
                 break;
        }
      }
    else if (ground_setting == BACKGROUND)
      {
      XtSetArg(args[0], XtNbackground, (int) *((Pixel *) to.addr));
      switch (widget_setting)
        {
        case MANAGER_BUTTON : 
                 XtSetValues(manager_button_label, args, 1);
                 XtSetValues(manager_cancel_label, args, 1);
                 resourcecolor[MANGR_BUTTN_BG] = item->string;
                 resourcecolor[MANGR_CANCL_BG] = item->string;
                 break;
        case WATCH_OPTION_BUTTONS :
                 XtSetValues(W_O_toggle_label,args,1);
                 XtSetValues(W_O_button_label,args,1);
                 resourcecolor[TOGGLE_BG] = item->string;
                 resourcecolor[WATCH_BG] = item->string;
                 break;
        case BUTTON_BOX : 
                 XtSetValues(button_form, args, 1);
                 resourcecolor[BUTTON_FRM_BG] = item->string;
                 break;
        case MENU_BUTTON : 
                 XtSetValues(menuButton1, args, 1);
                 XtSetValues(menuButton2, args, 1);
                 XtSetValues(menuButton3, args, 1);
                 XtSetValues(menuButton4, args, 1);
                 resourcecolor[MENU_BUTTON_BG] = item->string;
                 break;
        case DIALOG_WIN : 
                 XtSetValues(dialog_label, args, 1);
                 resourcecolor[DIALOG_BG] = item->string;
                 break;
        case FACTS_WIN : 
                 XtSetValues(facts_label, args, 1);
                 resourcecolor[FACTS_BG] = item->string;
                 break;
        case INSTANCES_WIN :
                 XtSetValues(instances_label, args, 1);
                 resourcecolor[INSTANCES_BG] = item->string;
                 break;
        case GLOBALS_WIN :
                 XtSetValues(globals_label, args, 1);
                 resourcecolor[GLOBALS_BG] = item->string;
                 break;
        case AGENDA_WIN : 
                 XtSetValues(agenda_label, args, 1);
                 resourcecolor[AGENDA_BG] = item->string;
                 break;
        case FOCUS_WIN :
                 XtSetValues(focus_label, args, 1);
                 resourcecolor[FOCUS_BG] = item->string;
                 break;
        case PULLDOWN_MENUS : 
                 XtSetValues(menu_label, args, 1);
                 resourcecolor[MENU_BG] = item->string;
                 break;
        case MANAGERS : 
                 XtSetValues(manager_label, args, 1);
                 resourcecolor[MANGR_LIST_BG] = item->string;
                 resourcecolor[MANGR_LIST_BDR] = item->string;
                 break;
        case WATCH_OPTIONS:
                 XtSetValues(watch_options_label,args,1);
                 resourcecolor[WATCH_FORM_BG] = item->string;
                 break;
        case CONFIRMATION : 
                 XtSetValues(confirm_label, args, 1);
                 resourcecolor[CONFIRM_BG] = item->string;
                 break;
        case FILE_WIN : 
                 XtSetValues(file_label, args, 1);
                 resourcecolor[FILE_DIALOG_BG] = item->string;
                 resourcecolor[FILE_FORM_BG] = item->string;
                 break;
        }
      }
    else if (ground_setting == BORDER)
      {
      XtSetArg(args[0], XtNborder, (int) *((Pixel *) to.addr));
      switch (widget_setting)
        {
        case MANAGER_BUTTON : 
                 XtSetArg(args[1], XtNforeground, (int) *((Pixel *) to.addr));
                 XtSetValues(manager_cancel_label, args, 2);
                 resourcecolor[MANGR_CANCL_FG] = item->string;
                 resourcecolor[MANGR_CANCL_BDR] = item->string;
                 break;
        case WATCH_OPTION_BUTTONS :
                 XtSetArg(args[1], XtNforeground, (int) *((Pixel *) to.addr));
                 XtSetValues(W_O_button_label,args,2);
                 resourcecolor[WATCH_FG] = item->string;
                 resourcecolor[WATCH_BDR] = item->string;
                 break;
        case MENU_BUTTON : 
                 XtSetValues(menuButton1, args, 1);
                 XtSetValues(menuButton2, args, 1);
                 XtSetValues(menuButton3, args, 1);
                 XtSetValues(menuButton4, args, 1);
                 resourcecolor[MENU_BUTTON_BDR] = item->string;
                 break;
        case DIALOG_WIN : 
                 XtSetValues(dialog_label, args, 1);
                 resourcecolor[DIALOG_BDR] = item->string;
                 break;
        case FACTS_WIN : 
                 XtSetValues(facts_label, args, 1);
                 resourcecolor[GLOBALS_BDR] = item->string;
                 break;
        case INSTANCES_WIN :
                 XtSetValues(instances_label, args, 1);
                 resourcecolor[INSTANCES_BDR] = item->string;
                 break;
        case GLOBALS_WIN :
                 XtSetValues(globals_label, args, 1);
                 resourcecolor[GLOBALS_BDR] = item->string;
                 break;
        case AGENDA_WIN : 
                 XtSetValues(agenda_label, args, 1);
                 resourcecolor[AGENDA_BDR] = item->string;
                 break;
        case FOCUS_WIN:
                 XtSetValues(focus_label, args, 1);
                 resourcecolor[FOCUS_BDR] = item->string;
                 break;
        case PULLDOWN_MENUS : 
                 XtSetValues(menu_label, args, 1);
                 resourcecolor[MENU_BDR] = item->string;
                 break;
        case MANAGERS : 
                 XtSetValues(manager_label, args, 1);
                 resourcecolor[MANGR_VP_BDR] = item->string;
                 break;
        case WATCH_OPTIONS:
                 XtSetValues(watch_options_label,args,1);
                 resourcecolor[WATCH_FORM_BDR] = item->string;
                 break;
        }
      }
  }

/********************************************************************************
          Name:        SetGround
          Description: Sets current ground selection
          Arguements:  w - Not used
                       client_data - Not used
                       call_data - Not used
          Returns:     None
*******************************************************************************/
static void SetGround(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
  if (streq(XtName(w), "Foreground"))
    ground_setting = FOREGROUND;
  else if (streq(XtName(w), "Background"))
    ground_setting = BACKGROUND;
  else if (streq(XtName(w), "Border"))
    ground_setting = BORDER;
  else
    ground_setting = NOT_AVAILABLE;
  }

/********************************************************************************
          Name:        Defaults
          Description: Sets current selection and highlighted button to default
          Arguements:  w - Not used except as a dummy widget
                       client_data - Not used
                       call_data - Not used
          Returns:     None
*******************************************************************************/
static void Defaults(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
  XtSetArg(args[0], XtNforeground, Convert(w, defaultstring[DIALOG_FG]));
  XtSetArg(args[1], XtNbackground, Convert(w, defaultstring[DIALOG_BG]));
  XtSetArg(args[2], XtNborder, Convert(w, defaultstring[DIALOG_BDR]));
  XtSetValues(dialog_label, args, 3);
  XtSetArg(args[0], XtNforeground, Convert(w, defaultstring[FACTS_FG]));
  XtSetArg(args[1], XtNbackground, Convert(w, defaultstring[FACTS_BG]));
  XtSetArg(args[2], XtNborder, Convert(w, defaultstring[FACTS_BDR]));
  XtSetValues(facts_label, args, 3);
  XtSetArg(args[0], XtNforeground, Convert(w, defaultstring[INSTANCES_FG]));
  XtSetArg(args[1], XtNbackground, Convert(w, defaultstring[INSTANCES_BG]));
  XtSetArg(args[2], XtNborder, Convert(w, defaultstring[INSTANCES_BDR]));
  XtSetValues(instances_label, args, 3);
  XtSetArg(args[0], XtNforeground, Convert(w, defaultstring[GLOBALS_FG]));
  XtSetArg(args[1], XtNbackground, Convert(w, defaultstring[GLOBALS_BG]));
  XtSetArg(args[2], XtNborder, Convert(w, defaultstring[GLOBALS_BDR]));
  XtSetValues(globals_label, args, 3);
  XtSetArg(args[0], XtNforeground, Convert(w, defaultstring[AGENDA_FG]));
  XtSetArg(args[1], XtNbackground, Convert(w, defaultstring[AGENDA_BG]));
  XtSetArg(args[2], XtNborder, Convert(w, defaultstring[AGENDA_BDR]));
  XtSetValues(agenda_label, args, 3);
  XtSetArg(args[0], XtNforeground, Convert(w, defaultstring[FOCUS_FG]));
  XtSetArg(args[1], XtNbackground, Convert(w, defaultstring[FOCUS_BG]));
  XtSetArg(args[2], XtNborder, Convert(w, defaultstring[FOCUS_BDR]));
  XtSetValues(focus_label, args, 3);
  XtSetArg(args[0], XtNbackground, Convert(w, defaultstring[BUTTON_FRM_BG]));
  XtSetValues(button_form, args, 1);
  XtSetArg(args[0], XtNforeground, Convert(w, defaultstring[MENU_BUTTON_FG]));
  XtSetArg(args[1], XtNbackground, Convert(w, defaultstring[MENU_BUTTON_BG]));
  XtSetArg(args[2], XtNborder, Convert(w, defaultstring[MENU_BUTTON_BDR]));
  XtSetValues(menuButton1, args, 3);
  XtSetValues(menuButton2, args, 3);
  XtSetValues(menuButton3, args, 3);
  XtSetValues(menuButton4, args, 3);
  XtSetArg(args[0], XtNforeground, Convert(w, defaultstring[SMEBSB_FG]));
  XtSetArg(args[1], XtNbackground, Convert(w, defaultstring[MENU_BG]));
  XtSetArg(args[2], XtNborder, Convert(w, defaultstring[MENU_BDR]));
  XtSetValues(menu_label, args, 3);
  XtSetArg(args[0], XtNforeground, Convert(w, defaultstring[MANGR_LIST_FG]));
  XtSetArg(args[1], XtNbackground, Convert(w, defaultstring[MANGR_LIST_BG]));
  XtSetArg(args[2], XtNborder, Convert(w, defaultstring[MANGR_LIST_BDR]));
  XtSetValues(manager_label, args, 3);
  XtSetArg(args[0], XtNforeground, Convert(w, defaultstring[CONFIRM_FG]));
  XtSetArg(args[1], XtNbackground, Convert(w, defaultstring[CONFIRM_BG]));
  XtSetValues(confirm_label, args, 2);
  XtSetArg(args[0], XtNforeground, Convert(w, defaultstring[FILE_DIALOG_FG]));
  XtSetArg(args[1], XtNbackground, Convert(w, defaultstring[FILE_DIALOG_BG]));
  XtSetValues(file_label, args, 2);
  XtSetArg(args[0], XtNforeground, Convert(w, defaultstring[MANGR_BUTTN_FG]));
  XtSetArg(args[1], XtNbackground, Convert(w, defaultstring[MANGR_BUTTN_BG]));
  XtSetArg(args[2], XtNborder, Convert(w, defaultstring[MANGR_BUTTN_BDR]));
  XtSetValues(manager_button_label, args, 3);
  XtSetArg(args[0], XtNforeground, Convert(w, defaultstring[MANGR_CANCL_FG]));
  XtSetArg(args[1], XtNbackground, Convert(w, defaultstring[MANGR_CANCL_BG]));
  XtSetArg(args[2], XtNborder, Convert(w, defaultstring[MANGR_CANCL_BDR]));
  XtSetValues(manager_cancel_label, args, 3);
  XtSetArg(args[0], XtNbackground, Convert(w, defaultstring[WATCH_FORM_BG]));
  XtSetArg(args[1], XtNborderColor, Convert(w, defaultstring[WATCH_FORM_BDR]));
  XtSetArg(args[2], XtNforeground,Convert(w, defaultstring[WATCH_FORM_FG]));
  XtSetValues(watch_options_label, args,3);
  XtSetArg(args[0], XtNbackground, Convert(w, defaultstring[TOGGLE_BG]));
  XtSetArg(args[1], XtNborderColor, Convert(w, defaultstring[TOGGLE_BDR]));
  XtSetArg(args[2], XtNforeground,Convert(w, defaultstring[TOGGLE_FG]));
  XtSetValues(W_O_toggle_label,args,3);
  XtSetArg(args[0], XtNbackground, Convert(w, defaultstring[WATCH_BG]));
  XtSetArg(args[1], XtNborderColor, Convert(w, defaultstring[WATCH_BDR]));
  XtSetArg(args[2], XtNforeground,Convert(w, defaultstring[WATCH_FG]));
  XtSetValues(W_O_button_label,args,3);
  for (i = 0; i < NUMRES; i++)
    resourcecolor[i] = defaultstring[i];
  }

/********************************************************************************
          Name:        SetBox
          Description: Sets current box selection
          Arguements:  w - Not used
                       client_data - Not used
                       call_data - Not used
          Returns:     None
*******************************************************************************/
static void SetBox(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
  XtSetArg(args[0], XtNlabel, "Foreground");
  XtSetValues(foreground, args, 1);
  XtSetArg(args[0], XtNlabel, "Background");
  XtSetValues(background, args, 1);
  XtSetArg(args[0], XtNlabel, "Border");
  XtSetValues(border, args, 1);
  if (streq(XtName(w), "Manager Button"))
    {
    widget_setting = MANAGER_BUTTON;
    XtSetArg(args[0], XtNlabel, "Button");
    XtSetValues(foreground, args, 1);
    XtSetArg(args[0], XtNlabel, "Cancel");
    XtSetValues(border, args, 1);
    }
  else if (streq(XtName(w), "Wtch/Opt Bttns"))
    {
    widget_setting = WATCH_OPTION_BUTTONS;
    XtSetArg(args[0], XtNlabel, "ToggleButton");
    XtSetValues(foreground, args, 1);
    XtSetArg(args[0], XtNlabel, "CommandButton");
    XtSetValues(border, args, 1);  
    }
  else if (streq(XtName(w), "Button Box"))
    {
    widget_setting = BUTTON_BOX;
    XtSetArg(args[0], XtNlabel, "N/A");
    XtSetValues(foreground, args, 1);
    XtSetArg(args[0], XtNlabel, "N/A");
    XtSetValues(border, args, 1);
    }
  else if (streq(XtName(w), "Menu Buttons"))
    widget_setting = MENU_BUTTON;
  else if (streq(XtName(w), "Dialog Window"))
    widget_setting = DIALOG_WIN;
  else if (streq(XtName(w), "Facts Window"))
    widget_setting = FACTS_WIN;
  else if (streq(XtName(w), "Instances Window"))
    widget_setting = INSTANCES_WIN;
  else if (streq(XtName(w), "Globals Window"))
    widget_setting = GLOBALS_WIN;
  else if (streq(XtName(w), "Agenda Window"))
    widget_setting = AGENDA_WIN;
  else if (streq(XtName(w), "Focus Window"))
    widget_setting = FOCUS_WIN;
  else if (streq(XtName(w), "Pulldown Menus"))
    widget_setting = PULLDOWN_MENUS;
  else if (streq(XtName(w), "Managers"))
    widget_setting = MANAGERS;
  else if (streq(XtName(w), "Watch/Options"))
    widget_setting = WATCH_OPTIONS;
  else if (streq(XtName(w), "Confirmations"))
    {
    widget_setting = CONFIRMATION;
    XtSetArg(args[0], XtNlabel, "N/A");
    XtSetValues(border, args, 1);
    }
  else if (streq(XtName(w), "File"))
    {
    widget_setting = FILE_WIN;
    XtSetArg(args[0], XtNlabel, "N/A");
    XtSetValues(border, args, 1);
    }

  }

/********************************************************************************
          Name:        Convert
          Description: Converts string of a color to Pixel value
          Arguements:  w - Not used except as a dummy widget
                       color - string of a color
          Returns:     Pixel value of a valid color
                       or -1 for an invalid color
*******************************************************************************/
static int Convert(
  Widget w,
  char *color)
  {
  XrmValue from, to;

  from.size = strlen(color) + 1;
  from.addr = color;
  XtConvert(w, XtRString, (XrmValuePtr) &from, XtRPixel, (XrmValuePtr) &to);
  if (to.addr == NULL)
    return(-1);
  return((int) *((Pixel *) to.addr));
  }

/********************************************************************************
          Name:        Quit
          Description: Pops a widget with command buttons to select "Save",
                       "Do NOT Save", and "Do NOT Quit"
          Arguements:  w - Not used
                       client_data - Not used
                       call_data - Not used
          Returns:     None
********************************************************************************/
static void Quit(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
  XtTranslateCoords(quit, (Position) 0, (Position) 0, &x, &y);
  XtSetArg(args[0], XtNx, x-4);
  XtSetArg(args[1], XtNy, y-4);
  XtSetValues(QUIT, args, 2);
  XtPopup(QUIT, XtGrabNonexclusive);
  }

/********************************************************************************
          Name:        Saveyes
          Description: Writes currently selected colors to "Xclips" and exits
                       program or, if write error, calls ChooseFile for new path
                       and filename
          Arguements:  w - Not used
                       client_data - Not used
                       call_data - Not used
          Returns:     None
********************************************************************************/
static void Saveyes(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
  if ((f = fopen(filename, "w")) != NULL)
    {
    for (i = 0; i < NUMRES; i++)
      fprintf(f, "%s\t%s\n", resourcestring[i], resourcecolor[i]);
    fclose(f);
    XtDestroyApplicationContext(app_con);
    exit(1);
    }
  else
    {
    XtPopdown(QUIT);
    ChooseFile();
    }
  }

/********************************************************************************
          Name:        Saveno
          Description: Exits program without writing new colors
          Arguements:  w - Not used
                       client_data - Not used
                       call_data - Not used
          Returns:     None
********************************************************************************/
static void Saveno(
  Widget widget,
  XtPointer client_data,
  XtPointer call_data)
  {
  XtDestroyApplicationContext(app_con);
  exit(1);
  }

/********************************************************************************
          Name:        Quitno
          Description: Pops down QUIT widget
          Arguements:  w - Not used
                       client_data - Not used
                       call_data - Not used
          Returns:     None
********************************************************************************/
static void Quitno(
  Widget widget,
  XtPointer client_data,
  XtPointer call_data)
  {
  XtPopdown(QUIT);
  }

/********************************************************************************
          Name:        ChooseFile
          Description: If write error this pops a widget allowing a new
                       path and filename to be selected
          Arguements:  None
          Returns:     None
********************************************************************************/
static void ChooseFile()
  {
  XtTranslateCoords(defaults, (Position) 0, (Position) 0, &x, &y);
  XtSetArg(args[0], XtNx, x);
  XtSetArg(args[1], XtNy, y);
  XtSetValues(filesave, args, 2);
  XtPopup(filesave, XtGrabNonexclusive);
  }

/********************************************************************************
          Name:        NextSave
          Description: Saves colors to "Xclips" and exits the program
                       or, if write error calls ChooseFile
          Arguements:  w - Not used
                       client_data - Not used
                       call_data - Not used
          Returns:     None
********************************************************************************/
static void NextSave(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
  String filename = XawDialogGetValueString(file_dialog);

  if ((f = fopen(filename, "w")) != NULL)
    {
    for (i = 0; i < NUMRES; i++)
      fprintf(f, "%s\t%s\n", resourcestring[i], resourcecolor[i]);
    fclose(f);
    XtDestroyApplicationContext(app_con);
    exit(1);
    }
  else
    {
    XtPopdown(filesave);
    ChooseFile();
    }
  }

/********************************************************************************
          Name:        Cancel
          Description: Pops down a widget sent through client_data
          Arguements:  w - Not used
                       client_data - Widget to cancel
                       call_data - Not used
          Returns:     None
********************************************************************************/ 
static void Cancel(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
  XtPopdown(XtParent((Widget) client_data));
  }
 

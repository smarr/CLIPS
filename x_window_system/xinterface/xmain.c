   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*           X Windows Version 2.01  06/15/03          */ 
   /*                                                     */
   /*                    XMAIN MODULE                     */
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

/***************************************************************************/
/*                                                                         */
/* Permission is hereby granted, free of charge, to any person obtaining   */
/* a copy of this software and associated documentation files (the         */
/* "Software"), to deal in the Software without restriction, including     */
/* without limitation the rights to use, copy, modify, merge, publish,     */
/* distribute, and/or sell copies of the Software, and to permit persons   */
/* to whom the Software is furnished to do so.                             */
/*                                                                         */
/* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS */
/* OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF              */
/* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT   */
/* OF THIRD PARTY RIGHTS. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY  */
/* CLAIM, OR ANY SPECIAL INDIRECT OR CONSEQUENTIAL DAMAGES, OR ANY DAMAGES */
/* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN   */
/* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF */
/* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.          */
/*                                                                         */
/***************************************************************************/

#define _XMAIN_SOURCE_

#include <stdio.h>

#include "setup.h"
#include "sysdep.h"
#include "commline.h"
#include "symbol.h"

#include <X11/bitmaps/xlogo11>
#include "xsetup.h"
#include "xmain.h"
#include "xmenu_file.h"
#include "xclipstext.h" 
#include "xmenu.h"
#include "xclips.h"

Boolean Dribble_status;
                                     
Boolean Browse_status[WINDOW_NUM] = {False,False,False,False,False};
                          /* [0] : agenda_window    *
                           * [1] : fact_window      *
                           * [2] : instances_window *
                           * [3] : globals_window   *
                           * [4] : focus_number     */

/********** Global variables **********/
Widget toplevel = NULL, dialog = NULL;
Widget dialog_form = NULL, dialog_text = NULL;
Arg TheArgs[10];
XtAppContext app_con;
XEvent TheEvent;
KeySym TheKeysym;              /* Key code id event is a keypress */
XComposeStatus compose_status;
Pixmap checker, clips_logo;
char *clips_sel_buf;
Boolean quit_get_event = False;
unsigned long clips_count = 0;   /* This variable is for the interface */
unsigned long clips_last_pos = 0;
Boolean send_to_clips = False;

XtActionsRec actionTable[] =
  {
    {"DialogReturn",      DialogReturn},
    {"MatchDialogReturnD",MatchDialogReturnD},
    {"MatchDialogReturnE",MatchDialogReturnE},
  };

char *xclips_translation1 =
"\
Ctrl<Key>A:     no-op() \n\
Ctrl<Key>B:     no-op() \n\
Ctrl<Key>C:     complete-construct-dialog() \n\
Ctrl<Key>D:     dribble() \n\
Ctrl<Key>E:     reset() \n\
Ctrl<Key>F:     no-op() \n\
Ctrl<Key>G:     no-op() \n\
Ctrl<Key>H:     stop-execution() \n\
Ctrl<Key>I:     no-op() \n\
Ctrl<Key>J:     no-op() \n\
Ctrl<Key>K:     clear-clips() \n\
Ctrl<Key>L:     load-constructs() \n\
Ctrl<Key>M:     no-op() \n\
Ctrl<Key>N:     clear-screen() \n\
Ctrl<Key>O:     no-op() \n\
Ctrl<Key>P:     no-op() \n\
Ctrl<Key>Q:     quit() \n\
Ctrl<Key>R:     run() \n\
Ctrl<Key>S:     save-rules() \n\
Ctrl<Key>T:     step() \n\
Ctrl<Key>U:     no-op() \n\
Ctrl<Key>V:     edit() \n\
Ctrl<Key>W:     no-op() \n\
Ctrl<Key>X:     no-op() \n\
Ctrl<Key>Y:     no-op() \n\
Ctrl<Key>Z:     command-line-clips() \n\
Meta<Key>I:     no-op() \n\
Meta<Key>K:     no-op() \n\
Meta<Key>Q:     no-op() \n\
:Meta<Key>d:    no-op() \n\
:Meta<Key>D:    no-op() \n\
:Meta<Key>h:    no-op() \n\
:Meta<Key>H:    no-op() \n\
:Meta<Key>]:    no-op() \n\
:Meta<Key>[:    no-op() \n\
~Shift Meta<Key>Delete:         no-op() \n\
 Shift Meta<Key>Delete:         no-op() \n\
~Shift Meta<Key>BackSpace:      no-op() \n\
 Shift Meta<Key>BackSpace:      no-op() \n\
<Key>Return:    Clipsnewline() \n\
<Key>Linefeed:  Clipsnewline() \n\
<Key>Delete:    delete-clips-previous-character() \n\
<Key>BackSpace: delete-clips-previous-character() \n\
<Key>:          insert-clips-char() \n\
<Btn2Down>:     insert-clips-selection(PRIMARY, CUT_BUFFER0) \n\
";

String fallback_resources[] =
  {
  "*allowHoriz:                 True",
  "*allowVert:                  True",
  "*borderWidth:                4",
  "*lineWidth:                  4",
  "*defaultColumns:             1",
  "*forceColumns:               True",
  "*showGrip:                   off",

  "*MenuButton.width:           75",
  "*MenuButton3D.width:           75",
  "*watchButton.width:          75",
  "*Form.file_dialog*translations:   #override \\n <Key>Return: DialogReturn(client_data)",
  "*Form.match_dialog*translations:  #override \\n <Key>Return: MatchDialogReturnD(client_data)",
  "*Form.match_editor*translations:  #override \\n <Key>Return: MatchDialogReturnE(client_data)",
  "*manager_viewport.height:    300",
  "*manager_viewport.width:     300",
  "*manager_form.Command.width: 150",
  "*Paned*internalBorderWidth:  0",

  NULL,
  };

#define clips_logo_width 30
#define clips_logo_height 27
static char clips_logo_bits[] = {
   0x80, 0xff, 0x01, 0x00, 0xe0, 0xff, 0x07, 0x00, 0x70, 0x20, 0x1d, 0x00,
   0x38, 0xb0, 0x3c, 0x00, 0x1c, 0x48, 0x72, 0x00, 0x8c, 0x2f, 0xf9, 0x00,
   0x8e, 0xba, 0xc4, 0x00, 0x66, 0xf8, 0xc6, 0x01, 0x37, 0x29, 0x61, 0x03,
   0xb3, 0xb6, 0x31, 0x03, 0x4f, 0x88, 0x18, 0x06, 0x23, 0x6c, 0x0c, 0x0f,
   0x13, 0x22, 0x86, 0x0c, 0x1f, 0xbf, 0xc3, 0x1c, 0x86, 0x08, 0x60, 0x32,
   0x86, 0x04, 0x10, 0x31, 0x7c, 0x02, 0xd8, 0x31, 0x18, 0x05, 0xc4, 0x1d,
   0xb8, 0x8a, 0x36, 0x0e, 0x70, 0x93, 0x11, 0x06, 0xe0, 0xee, 0x11, 0x06,
   0xc0, 0xa9, 0x09, 0x06, 0x80, 0x73, 0x04, 0x06, 0x00, 0x23, 0x02, 0x06,
   0x00, 0x1b, 0xf9, 0x07, 0x00, 0x8b, 0xfc, 0x03, 0x00, 0x87, 0x0c, 0x00};

/*******************************************************************************
          Name:        main
          Description: main function - Creates the interface for CLIPS
          Arguments:   argc - number of arguments
                       argv - arguments
          Returns:     0 on exit
*******************************************************************************/
int main(
  int argc,
  char **argv)
  {
   void *theEnv;
   /*================================*/
   /* Create top level shell widget. */
   /*================================*/

   toplevel = XtAppInitialize(&app_con,"Xclips",NULL,0,&argc,argv,
                              fallback_resources,NULL,0);
                              
   /*===========================*/
   /* Add the new action table. */
   /*===========================*/

   XtAppAddActions(app_con, actionTable, XtNumber(actionTable));
   XtAppAddActions(app_con, ClipsTxtActsTable, ClipsTxtActsTableCount);
  
   /*==========================*/
   /* Create the checker icon. */
   /*==========================*/

   checker = XCreateBitmapFromData(XtDisplay(toplevel),
                                  RootWindowOfScreen(XtScreen(toplevel)),
                                  xlogo11_bits,
                                  xlogo11_width,
                                  xlogo11_height);
  
   /*=========================*/
   /* Create clips logo icon. */
   /*=========================*/

   clips_logo = XCreateBitmapFromData(XtDisplay(toplevel),
                                     RootWindowOfScreen(XtScreen(toplevel)),
                                     clips_logo_bits,
                                     clips_logo_width,
                                     clips_logo_height);

   /*==================================================*/
   /* Create the frame for the main I/O dialog window. */
   /*==================================================*/

   dialog = XtCreateManagedWidget("dialog",panedWidgetClass,
                                  toplevel,NULL,0);

   /*=============================*/
   /* Create the Pull down menus. */
   /*=============================*/

   CreatePullDownMenus(dialog);

   XtSetArg(TheArgs[0], XtNlabel, "Xclips - CLIPS Version 6.30");
   (void) XtCreateManagedWidget("menu",labelWidgetClass,dialog,TheArgs,1);

   /*====================================*/
   /* Create the main I/O dialog window. */
   /*====================================*/
  
   dialog_form = XtCreateManagedWidget("dialog_form",
                                      formWidgetClass,
                                      dialog,
                                      NULL, 0);

   dialog_text = XtVaCreateManagedWidget("dialog_text",
                                        asciiTextWidgetClass,
                                        dialog_form,
                                        XtNheight, 500,
                                        XtNwidth, 600,
                                        XtNeditType, XawtextEdit,
                                        XtNtype, XawAsciiString,
                                        XtNscrollHorizontal, XawtextScrollNever,
                                        XtNscrollVertical, XawtextScrollWhenNeeded,
                                        XtNwrap, XawtextWrapWord,
                                        XtNresize, XawtextResizeNever,
                                        NULL);

   XtOverrideTranslations(dialog_text,
                          XtParseTranslationTable(xclips_translation1));

   XtSetKeyboardFocus(dialog_form, dialog_text);

   XtRealizeWidget(toplevel); 

   theEnv = CreateEnvironment();

   InitializeInterface();
   RerouteStdin(theEnv,argc,argv);
   CommandLoop(theEnv);

   return(-1);
  }


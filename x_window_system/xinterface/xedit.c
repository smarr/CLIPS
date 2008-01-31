   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*           X Windows Version 2.01  06/15/03          */ 
   /*                                                     */
   /*                    XEDIT MODULE                     */
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
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>

#include "setup.h"
#include "clips.h"
#include "commline.h"

#include "xsetup.h"
#include "xclips.h"
#include "xedit.h"
#include "xmain.h"
#include "xmenu.h"
#include "xmenu_file.h"
#include "xclipstext.h"

static void EditorFontSelect(Widget,XtPointer,XtPointer);
static void ExitEditor(Widget,XtPointer,XtPointer);
static void EditorSaveFirst(Widget,XtPointer,XtPointer); 
static void EditorSave(Widget,XtPointer,XtPointer);

/********** Global variable in this file ********/
XawTextPosition CurrentPosition,EndPosition;
Widget CurrentSource;
char *BatchString = NULL;

char *xclips_translation3 =
"\
Ctrl<Key>B:     balance() \n\
Ctrl<Key>C:     complete-construct-editor()\n\
";

String bindings[] =
  {
  "Ctrl-a   Begining Of Line",
  "Ctrl-b   Search for matching parenthesis",
  "Crtl-d   Delete Next Character",
  "Crtl-e   End Of Line",
  "Crtl-f   Forward Character",
  "Crtl-g   Multiply Reset",
  "Crtl-h   Delete Previous Character",
  "Crtl-j   Newline And Indent",
  "Crtl-k   Kill To End Of Line",
  "Crtl-l   Redraw Display",
  "Crtl-m   Newline",
  "Crtl-n   Next Line",
  "Crtl-o   Newline And Backup",
  "Crtl-p   Previous Line",
  "Crtl-r   Search/Replace Backward",
  "Crtl-s   Search/Replace Forward",
  "Crtl-t   Transpose Characters",
  "Crtl-u   Multiply by 4",
  "Crtl-v   Next Page",
  "Crtl-w   Kill Selection",
  "Crtl-y   Unkill",
  "Crtl-z   Scroll One Line Up",
  "Meta-b   Backward Word",
  "Meta-D   Kill Word",
  "Meta-d   Delete Next Word",
  "Meta-f   Forward Word",
  "Meta-H   Backward Kill Word",
  "Meta-h   Delete Previous Word",
  "Meta-i   Insert File",
  "Meta-k   Kill To End Of Paragraph",
  "Meta-q   Form Paragraph",
  "Meta-v   Previous Page",
  "Meta-y   Insert Current Selection",
  "Meta-z   Scroll One Line Down",
  "Meta-<   Begining Of File",
  "Meta->   End Of File",
  "Meta-]   Forward Paragraph",
  "Meta-[   Backward Paragraph",
  "Meta-Delete    Delete Previous Word",
  "Meta-Shift Delete    Kill Previous Word",
  "Meta-Backspace Delete Previous Word",
  "Meta-Shift Backspace Kill Previous Word",
  NULL,
  }; 

/*******************************************************************************
          Name:        EditNewFile
          Description: Creates new edit window and places file in it
          Arguments:  w - Dialog widget
                       client_data - file to open
                       call_data - Not Used
          Returns:     NONE
*******************************************************************************/
void EditNewFile(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
  String filename = XawDialogGetValueString(XtParent(w));
  char fullpath[255];
  Widget edit = NULL, edit_form = NULL, outer_form = NULL, asciiForm = NULL,
         edit_text= NULL, button= NULL, menu= NULL, item;
  Dimension width, height;
  FILE *fp = NULL;
  char buf[512];

  strcpy(fullpath, path);
  strcat(fullpath, filename);

  /* ========================================== */
  /*  Verify if the file is editable            */
  /* ========================================== */

  if((fp = fopen(fullpath, "r+")) == NULL)
    {

    switch (errno) 
      {
      case ENOENT:

        if((fp =  fopen(fullpath, "w")) == NULL)
          {
          sprintf(buf,"Can't open file %s for edit\n", fullpath);
          EnvPrintRouter(theEnv,"wclips", buf);
          quit_get_event = TRUE;
          PrintPrompt(theEnv);
          return;
          }

      break;

     default:
        sprintf(buf, "Permission denied, can't open file %s \n", fullpath);
        EnvPrintRouter(theEnv,"wclips", buf);
        quit_get_event = TRUE;
        PrintPrompt(theEnv);
        CancelPopupSelect(outer_form, (XtPointer)XtParent(w), (XtPointer)NULL);
        return;
      }
    }
  /* ================================= */
  /*  Close the file so it could be    */
  /*  opened by the asciiTextWidget.   */
  /* ================================= */

  fclose(fp);

  /* ============================================= */
  /*  Create the editor window by using the athena */
  /*  widgets; include :                           */
  /*  topLevelShellWidgetClass                     */
  /*   panedWidgetClass                            */
  /*     formWidgetClass                           */
  /*      asciiTextWidgetClass                     */
  /*      menuButtonWidgetClass                    */
  /*        simpleMenuWidgetClass                  */
  /*          smeBSBObjectClass                    */
  /*          smeLineObjectClass                   */
  /* ============================================= */

  XtSetArg(TheArgs[0], XtNwidth, &width);
  XtSetArg(TheArgs[1], XtNheight, &height);
  XtGetValues(dialog, TheArgs, 2);
  edit  = XtCreatePopupShell(filename,
                             topLevelShellWidgetClass,
                             toplevel,
                             NULL, 0);

  XtSetArg(TheArgs[0], XtNheight, height);
  XtSetArg(TheArgs[1], XtNwidth, width);
  outer_form = XtCreateManagedWidget("outer_form",
                                     panedWidgetClass,
                                     edit,
                                     TheArgs, 2);

  edit_form = XtCreateManagedWidget("buttonForm",
                                    formWidgetClass,
                                    outer_form,
                                    NULL, 0);

  asciiForm = XtCreateManagedWidget("asciiform",
                                    formWidgetClass,
                                    outer_form,
                                    NULL, 0);

  XtSetArg(TheArgs[0], XtNheight, height-35);
  XtSetArg(TheArgs[1], XtNwidth, width);
  XtSetArg(TheArgs[2], XtNresize, "false");
  XtSetArg(TheArgs[3], XtNtype, XawAsciiFile);
  XtSetArg(TheArgs[4], XtNeditType, XawtextEdit);
  XtSetArg(TheArgs[5], XtNscrollVertical, XawtextScrollWhenNeeded);
  XtSetArg(TheArgs[6], XtNscrollHorizontal, XawtextScrollWhenNeeded);
  XtSetArg(TheArgs[7], XtNstring, fullpath);
  edit_text = XtCreateManagedWidget("dialog_text",
                                    asciiTextWidgetClass,
                                    asciiForm,
                                    TheArgs, 8);

  XtOverrideTranslations(edit_text,XtParseTranslationTable(xclips_translation3));

  /* ================================== */
  /*        CREATE FILE MENU            */
  /* ================================== */

  button = XtCreateManagedWidget("File",
                                 menuButtonWidgetClass,
                                 edit_form,
                                 NULL, 0);

  menu = XtCreatePopupShell("menu",
                            simpleMenuWidgetClass,
                            button,
                            NULL, 0);

  item = XtCreateManagedWidget("Save",
                               smeBSBObjectClass,
                               menu,
                               NULL, 0);
  XtAddCallback(item, XtNcallback, EditorSaveCallback, (XtPointer)(edit_text));
  item = XtCreateManagedWidget("Save As",
                               smeBSBObjectClass,
                               menu,
                               NULL, 0);
  XtAddCallback(item, XtNcallback, EditorSaveAsCallback,
                (XtPointer)(edit_text));
  item = XtCreateManagedWidget("Revert",
                               smeBSBObjectClass,
                               menu,
                               NULL, 0);
  XtAddCallback(item, XtNcallback, EditorRevertCallback,
                (XtPointer)(edit_text));

  (void)XtCreateManagedWidget("line",smeLineObjectClass,menu,NULL,0);
  
  item = XtCreateManagedWidget("Load Selection",
                               smeBSBObjectClass,
                               menu,
                               NULL, 0);
  XtAddCallback(item, XtNcallback,  EditorCompileSelectionCallback,
                (XtPointer)edit_text);

  item = XtCreateManagedWidget("Batch Selection",
                               smeBSBObjectClass,
                               menu,
                               NULL, 0);
  XtAddCallback(item,XtNcallback,EditorBatchSelectionCb,(XtPointer)edit_text);

  item = XtCreateManagedWidget("Load Buffer",
                               smeBSBObjectClass,
                               menu,
                               NULL, 0); 
  XtAddCallback(item, XtNcallback,  EditorCompileFileCallback,
                (XtPointer)edit_text);
 

  (void)XtCreateManagedWidget("line",smeLineObjectClass,menu,NULL,0);

  item = XtCreateManagedWidget("Exit",
                               smeBSBObjectClass,
                               menu,
                               NULL, 0);
  XtAddCallback(item, XtNcallback, EditorExitCallback, (XtPointer)edit_text);

  /* ================================= */
  /*     Create the Edit Menu          */
  /* ================================= */

  XtSetArg(TheArgs[0], XtNfromHoriz, button);
  button = XtCreateManagedWidget("Edit",
                                 menuButtonWidgetClass,
                                 edit_form,
                                 TheArgs, 1);

  menu = XtCreatePopupShell("menu",
                            simpleMenuWidgetClass,
                            button,
                            NULL, 0);

  item = XtCreateManagedWidget("Complete...   ^C",
                               smeBSBObjectClass,
                               menu,
                               NULL, 0);
  XtAddCallback(item, XtNcallback, CompletionEditCallback, (XtPointer)edit_text);
  (void)XtCreateManagedWidget("line",
                               smeLineObjectClass,
                               menu,
                               NULL, 0);
  item = XtCreateManagedWidget("Cut",
                               smeBSBObjectClass,
                               menu,
                               NULL, 0);
  XtAddCallback(item, XtNcallback, EditorCutCallback, (XtPointer)edit_text);
  item = XtCreateManagedWidget("Paste",
                               smeBSBObjectClass,
                               menu,
                               NULL, 0);
  XtAddCallback(item, XtNcallback, EditorPasteCallback, (XtPointer)edit_text);
  item = XtCreateManagedWidget("Search/Replace...",
                               smeBSBObjectClass,
                               menu,
                               NULL, 0);
  XtAddCallback(item, XtNcallback, EditorSearchReplaceCallback,
                (XtPointer)edit_text);
  item = XtCreateManagedWidget("Balance       ^B",
                               smeBSBObjectClass,
                               menu,
                               NULL, 0);
  XtAddCallback(item,XtNcallback,FindMatchingParenthesisCallback,(XtPointer)edit_text);
  (void)XtCreateManagedWidget("line",
                               smeLineObjectClass,
                               menu,
                               NULL, 0);
  item = XtCreateManagedWidget("Beginning of File",
                               smeBSBObjectClass,
                               menu,
                               NULL, 0);
  XtAddCallback(item, XtNcallback, EditorBeginingOfFileCallback,
                (XtPointer)edit_text);
  item = XtCreateManagedWidget("End of File",
                               smeBSBObjectClass,
                               menu,
                               NULL, 0);
  XtAddCallback(item, XtNcallback, EditorEndOfFileCallback,
                (XtPointer)edit_text);

  /* ================================= */
  /*    Create Font Menu               */
  /* ================================= */

  XtSetArg(TheArgs[0], XtNfromHoriz, button);
  button = XtCreateManagedWidget("Font",
                                 menuButtonWidgetClass,
                                 edit_form,
                                 TheArgs, 1);

  menu = XtCreatePopupShell("menu",
                            simpleMenuWidgetClass,
                            button,
                            NULL, 0);

  item = XtCreateManagedWidget("5x8",
                               smeBSBObjectClass,
                               menu,
                               NULL, 0);
  XtAddCallback(item, XtNcallback, EditorFontSelect, (XtPointer)edit_text);
  item = XtCreateManagedWidget("6x10",
                               smeBSBObjectClass,
                               menu,
                               NULL, 0);
  XtAddCallback(item, XtNcallback, EditorFontSelect, (XtPointer)edit_text);
  item = XtCreateManagedWidget("6x13bold",
                               smeBSBObjectClass,
                               menu,
                               NULL, 0);
  XtAddCallback(item, XtNcallback, EditorFontSelect, (XtPointer)edit_text);
  item = XtCreateManagedWidget("7x13bold",
                               smeBSBObjectClass,
                               menu,
                               NULL, 0);
  XtAddCallback(item, XtNcallback, EditorFontSelect, (XtPointer)edit_text);
  item = XtCreateManagedWidget("8x13bold",
                               smeBSBObjectClass,
                               menu,
                               NULL, 0);
  XtAddCallback(item, XtNcallback, EditorFontSelect, (XtPointer)edit_text);
  item = XtCreateManagedWidget("9x15bold",
                               smeBSBObjectClass,
                               menu,
                               NULL, 0);
  XtAddCallback(item, XtNcallback, EditorFontSelect, (XtPointer)edit_text);
  item = XtCreateManagedWidget("10x20",
                               smeBSBObjectClass,
                               menu,
                               NULL, 0);
  XtAddCallback(item, XtNcallback, EditorFontSelect, (XtPointer)edit_text);
  item = XtCreateManagedWidget("12x24",
                               smeBSBObjectClass,
                               menu,
                               NULL, 0);
  XtAddCallback(item, XtNcallback, EditorFontSelect, (XtPointer)edit_text);

  /* ================================== */
  /*        Create Help Menu            */
  /* ================================== */

  XtSetArg(TheArgs[0], XtNfromHoriz, button);
  button = XtCreateManagedWidget("Help",
                                 menuButtonWidgetClass,
                                 edit_form,
                                 TheArgs, 1);

  menu = XtCreatePopupShell("menu",
                            simpleMenuWidgetClass,
                            button,
                            NULL, 0);

  item = XtCreateManagedWidget("Key Bindings",
                               smeBSBObjectClass,
                               menu,
                               NULL, 0);
  XtAddCallback(item, XtNcallback, EditorHelpSelect, (XtPointer)edit_text);

  XtPopup(edit, XtGrabNone);
  }

/*******************************************************************************
          Name:        EditorSaveCallback
          Description: Called when Save is selected from the File menu
                       This function pops up a confirmation box before
                       the file should be saved.
          Arguments:  w - menu item that was selected
                       client_data - Widget to edit
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
void EditorSaveCallback(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
  Widget popup, confirm;

  popup = XtCreatePopupShell("popup",
                             topLevelShellWidgetClass,
                             toplevel,
                             NULL, 0);

  XtSetArg(TheArgs[0], XtNlabel, "This will overwrite\nthe old file!");
  XtSetArg(TheArgs[1], XtNicon, clips_logo);
  confirm = XtCreateManagedWidget("confirm",
                                  dialogWidgetClass,
                                  popup,
                                  TheArgs, 2);
  XawDialogAddButton(confirm, "Overwrite", EditorSave, client_data);
  XawDialogAddButton(confirm, "Cancel", CancelPopupSelect, (XtPointer)confirm);

  XtPopup(popup, XtGrabNonexclusive);
  }

/*******************************************************************************
          Name:        EditorSaveAsCallback
          Description: Called when Save As is selected from the File menu.
                       This function will save the file under a new name.
          Arguments:   w - menu item that was selected
                       client_data - Widget to edit
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
void EditorSaveAsCallback(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
  Widget popup, file_dialog;

  file_item = SAVEAS;

  popup = XtCreatePopupShell("popup",
                             topLevelShellWidgetClass,
                             toplevel,
                             NULL, 0);

  XtSetArg(TheArgs[0], XtNlabel, "Enter new file name:");
  XtSetArg(TheArgs[1], XtNvalue,  "");
  XtSetArg(TheArgs[2], XtNicon, clips_logo);
  file_dialog = XtCreateManagedWidget("file_dialog",
                                      dialogWidgetClass,
                                      popup,
                                      TheArgs, 3);
  XawDialogAddButton(file_dialog, "Save", EditorSaveAs, client_data);
  XawDialogAddButton(file_dialog, "Cancel", CancelPopupSelect,
                     (XtPointer)file_dialog);

  XtPopup(popup, XtGrabNonexclusive);
  }

/*******************************************************************************
          Name:        EditorRevertCallback
          Description: Called when Revert is selected from the File menu.
                       This function undo all the modifications since
                       the openning of the file.
          Arguments:   w - menu item that was selected
                       client_data - Widget to edit
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
void EditorRevertCallback(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
  Widget popup, confirm;
  String filename;

  file_item = REVERT;

  popup = XtCreatePopupShell("popup",
                             topLevelShellWidgetClass,
                             toplevel,
                             NULL, 0);

  XtSetArg(TheArgs[0], XtNstring, &filename);
  XtGetValues((Widget)client_data, TheArgs, 1);
  XtSetArg(TheArgs[0], XtNlabel, "You will loose\nall your changes!");
  XtSetArg(TheArgs[1], XtNicon, clips_logo);
  XtSetArg(TheArgs[2], XtNvalue, filename);
  XtSetArg(TheArgs[3], XtNeditType, XawtextRead);
  confirm = XtCreateManagedWidget("confirm", dialogWidgetClass, popup, TheArgs, 4);
  XawDialogAddButton(confirm, "Revert", EditorRevert, client_data);
  XawDialogAddButton(confirm, "Cancel", CancelPopupSelect, (XtPointer)confirm);

  XtPopup(popup, XtGrabNonexclusive);
  }

/* ================================================ */
/*    The below functions are for interacting with  */
/*    CLIPS.                                        */
/* ================================================ */                       

/*******************************************************************************
          Name:        FindSelection
          Description: Find the router
          Argument:    log_name - Router's logical name
          Returns:     True if found else fasle
*******************************************************************************/
int FindSelection(
void *theEnv,
char *log_name)
{
    if(strcmp("XeditSelection",log_name)== 0)
        return(TRUE);
    return(FALSE);
 }

/********************************************************************************
        Name:         SelectionGetc
        Description:  Get a charater from the current selection 
                      in the editor.
        Argument:     log_name - router's logical name
        Return:       character
*******************************************************************************/
int SelectionGetc(
  void *theEnv,
  char *log_name)
  {

   XawTextBlock text_return;

   if(XawTextSourceRead(CurrentSource,CurrentPosition,&text_return,1) != EndPosition)
    {
       CurrentPosition++;
       return((int)text_return.ptr[0]);
    }
   else
    return(EOF);
}
/********************************************************************************
        Name:           SelectionUngetc
        Description:    Move the cursor back one character
        Argument:       c - the character that being pushed back
        Return:         always 1
*******************************************************************************/
int  SelectionUngetc(
  void *theEnv,
int c,
char *log_name)
{
   if (c == EOF )
    return(1);
   if(CurrentPosition > 0)
     CurrentPosition--;
   return(1);
}


/********************************************************************************
        Name:         EditorCompileSelectionCallback
        Description:  Load the selection to CLIPS window
        Argument:     w - the widget that initiates the callback
                      client_data - the editor widget
                      call_data   - Not used
        Return:       none
*******************************************************************************/

void EditorCompileSelectionCallback(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   
   CurrentSource = XawTextGetSource((Widget)client_data);
   
   XawTextGetSelectionPos((Widget)client_data,&CurrentPosition,&EndPosition);
   if(CurrentPosition == EndPosition)  /* No selection was made */
      return;

   EndPosition++;                       /* Move the cursor to the end of the selection */

  /* Create a IO router for the buffer */

    EnvAddRouter(theEnv,"xclipsSelection",90,FindSelection,NULL,SelectionGetc,SelectionUngetc,NULL);

  /* Compile  */

    LoadXFile("xclipsSelection","XeditSelection");
    EnvPrintRouter(theEnv,"wclips","CLIPS> ");
    quit_get_event = True;
  }
/*******************************************************************************
          Name:        FileFind
          Description: Find the router
          Argument:    log_name - Router's logical name
          Returns:     True if found else fasle
*******************************************************************************/
int FileFind(
void *theEnv,
char *log_name)
{
    if(strcmp("XeditBuffer",log_name)== 0)
        return(TRUE);
    return(FALSE);
 }

/********************************************************************************
        Name:         FileGetc
        Description:  Get a charater from the current buffer.
        Argument:     log_name - router's logical name
        Return:       character
*******************************************************************************/
int FileGetc(
void *theEnv,
char *log_name)
{

   XawTextBlock text_return;

   if(XawTextSourceRead(CurrentSource,CurrentPosition,&text_return,1) != CurrentPosition)
    {
       CurrentPosition++;
       return((int)text_return.ptr[0]);
    }
   else
    return(EOF);
}
/********************************************************************************
        Name:           FileUngetc
        Description:    Move the cursor back one character
        Argument:       c - the character that being pushed back
        Return:         always 1
*******************************************************************************/
int  FileUngetc(
void *theEnv,
int c,
char *log_name)
{
   if (c == EOF )
    return(1);
   if(CurrentPosition > 0)
     CurrentPosition--;
   return(1);
}

/*******************************************************************************
          Name:        EditorCompileFileCallback
          Description: Called when Load File to CLIPS is selected from the File
                       menu. This function will load the entire buffer to
                       CLIPS.
          Arguments:  w - menu item that was selected
                       client_data - Widget to edit
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
void EditorCompileFileCallback(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
{
   void *theEnv = GetCurrentEnvironment();
   CurrentPosition = 0;
   CurrentSource = XawTextGetSource((Widget)client_data);

  /* Create a IO router for the file */   

    EnvAddRouter(theEnv,"xclipsFile",90,FileFind,NULL,FileGetc,FileUngetc,NULL);

  /* Compile  */
    LoadXFile("xclipsFile","XeditBuffer");
    PrintCLIPS("wclips","CLIPS> ");
    quit_get_event = True;
}

/********************************************************************************
        Name: LoadXFile
        Description:This function activates a router and tell CLIPS to
                    load the construct file using the router.
        Argument: char str1 - logical name of the router
                  char str2 - logical name of the buffer to be loaded
        Return: int - unused.
*******************************************************************************/
int  LoadXFile(
char *str1,
char *str2)
  {   
   void *theEnv = GetCurrentEnvironment();

   EnvActivateRouter(theEnv,str1);
   SetPrintWhileLoading(theEnv,TRUE);
   LoadConstructsFromLogicalName(theEnv,str2);
   EnvDeactivateRouter(theEnv,str1);
   SetPrintWhileLoading(theEnv,FALSE);
   EnvDeleteRouter(theEnv,str1);
   return 0;
}
/******************************************************************************
        Name:        EditorBatchSelectionCb
        Description: This function is called when Batch Selection is selected
                     from the menu. THis function will batch the current
                     selection and send to CLIPS.
        Argument: w - widget caused the event to happen
                  client_data - the text widget which contains the 
                                edited buffer.
                  call_data - unused
        Return:
*******************************************************************************/
void EditorBatchSelectionCb(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
{
   XawTextBlock text_return;

   if(BatchString != NULL)
    {
     free(BatchString);
     BatchString = NULL;
    }
   CurrentSource = XawTextGetSource((Widget)client_data);
   XawTextGetSelectionPos((Widget)client_data,&CurrentPosition,&EndPosition);
   XawTextSourceRead(CurrentSource,CurrentPosition,&text_return,EndPosition  - CurrentPosition );
   BatchString = (char*)malloc((EndPosition - CurrentPosition) + 2);
   strncpy(BatchString,text_return.ptr,EndPosition - CurrentPosition);
   BatchString[EndPosition - CurrentPosition] = '\n';
   BatchString[(EndPosition + 1) - CurrentPosition] = 0;
   OpenStringBatch(GetCurrentEnvironment(),"editBatch",BatchString,False);
   quit_get_event = True;
}

/*******************************************************************************
          Name:        EditorExitCallback
          Description: Called when Exit is selected from the File menu
          Arguments:  w - menu item that was selected
                       client_data - Widget to edit
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
void EditorExitCallback(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
  Widget popup, confirm;

  popup = XtCreatePopupShell("popup",
                             topLevelShellWidgetClass,
                             toplevel,
                             NULL, 0);

  XtSetArg(TheArgs[0], XtNlabel, "Exit this editor!\nAre you sure?");
  XtSetArg(TheArgs[1], XtNicon, clips_logo);
  confirm = XtCreateManagedWidget("confirm",
                                  dialogWidgetClass,
                                  popup,
                                  TheArgs, 2);
  XawDialogAddButton(confirm, "Exit", ExitEditor,
                     (XtPointer)(XtParent(XtParent((Widget)client_data))));
  XawDialogAddButton(confirm, "Cancel", CancelPopupSelect, (XtPointer)confirm);

  if (XawAsciiSourceChanged(XawTextGetSource((Widget)client_data)))
    XawDialogAddButton(confirm, "Save First", EditorSaveFirst, client_data);

  XtPopup(popup, XtGrabNonexclusive);
  }

/*******************************************************************************
          Name:        EditorCutCallback
          Description: Called when Cut is selected from the Edit menu
          Arguments:  w - menu item that was selected
                       client_data - Widget to edit
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
void EditorCutCallback(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
  DeleteCurrentSelection((Widget)client_data, &TheEvent);
  }

/*******************************************************************************
          Name:        EditorPasteCallback
          Description: Called when Paste is selected from the Edit menu
          Arguments:  w - menu item that was selected
                       client_data - Widget to edit
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
void EditorPasteCallback(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
  Stuff((TextWidget) client_data, &TheEvent);
  }

/*******************************************************************************
          Name:        EditorSearchReplaceCallback
          Description: Called when Search/Replace is selected from the Edit menu
          Arguments:  w - menu item that was selected
                       client_data - Widget to edit
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
void EditorSearchReplaceCallback(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
  static String params[] = {"forward"};
  Cardinal num_params = 1;

  _XawTextSearch((Widget)client_data, &TheEvent, params, &num_params);
  }


/*******************************************************************************
          Name:        FindMatchingParenthesisCallback
          Description: Find the matching parenthesis
          Arguments:  w - menu item that was selected
                       client_data - Widget to edit
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
void FindMatchingParenthesisCallback(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
     XawTextBlock text_return;
     XawTextPosition length;
     Boolean  Forward  = True;
     Widget source = XawTextGetSource((Widget)client_data);
     XawTextPosition Pos = XawTextGetInsertionPoint((Widget)client_data);
    
     /* ====================================== */
     /*  If there is a parenthesis at the      */
     /*  current position of the cusor then    */
     /*  try to find the matching parenthesis, */
     /*  otherwise, return.                     */
     /* ====================================== */

     length = XawTextSourceRead (source,Pos,&text_return,1);
     if(text_return.length != 0)
      {
        if(text_return.ptr[0] == ')')
          Forward = False;
        else if (text_return.ptr[0] != '(')
         {
          XBell(XtDisplay(toplevel),100);
          return;
         }
      }
     else
      {
        XBell(XtDisplay(toplevel),100);
        return;
      }
     /* =============================== */
     /*   Searching for a ")"           */
     /* =============================== */
     if(Forward)
      {
        if( SearchForward(source,&text_return,length))        
           XawTextSetSelection((Widget)client_data,Pos,text_return.firstPos + text_return.length);
        else
         {
           XBell(XtDisplay(toplevel),100);
           WarningWindow( "There is no\n matching parenthesis!");
         }
      }

     /* =============================== */
     /*   Searching for a "("           */
     /* =============================== */

     else 
      {
        if( SearchBackward(source,&text_return,length))
          XawTextSetSelection((Widget)client_data,text_return.firstPos, Pos + 1);
        else
         {
           XBell(XtDisplay(toplevel),100);
           WarningWindow("There is no\nmatching parenthesis!");
         }
      }
  }

/*******************************************************************************
          Name:        SearchForward
          Description: Search forward for the mathching parethesis [")"]
          Arguments:   source - Widget, text source
                       text_return _ Pointer to the structure that contains 
                       the information about the text block, which has been read
                       by XawTextSourceRead;
                       length - the Position of the last char in the text block
          Returns:     Return True If Found; otherwise, return False.
*******************************************************************************/
int SearchForward(
Widget source,
XawTextBlock *text_return,
XawTextPosition length)
{
   XawTextPosition newPos = length;
   int count = 1;

   while((newPos != text_return->firstPos) && (count))
    {
       newPos = XawTextSourceRead(source,newPos,text_return,1);
       if(newPos == text_return->firstPos)
         break;
       if( text_return->ptr[0] == '(')
        count++;
       else if( text_return->ptr[0] == ')')
        count--;
    }
   if(count)    /* could not find the matching parenthesis */
     return(False);
   else
     return(True); /* found the matching parenthesis */
}
/*******************************************************************************
          Name:        WarningWindow
          Description: Pop up a warning message window;
          Arguments:   Text - Warning message;
          Returns:     None
*******************************************************************************/
void WarningWindow(
char *text)
{
    Widget WarningShell,WarningDialog;

    WarningShell = XtCreatePopupShell("Confirmation",
                                    topLevelShellWidgetClass,
                                    toplevel,
                                    NULL, 0);
   XtSetArg(TheArgs[0], XtNlabel, text);
   WarningDialog =  XtCreateManagedWidget("confirm",
                                  dialogWidgetClass,
                                  WarningShell,
                                  TheArgs, 1);
   XawDialogAddButton( WarningDialog,"Okay",CancelPopupSelect,(XtPointer)WarningDialog);
   XtPopup(WarningShell, XtGrabNonexclusive);

}
/*******************************************************************************
          Name:        SearchBackward
          Description: Search backward for the matchhing parethesis ["("]
          Arguments:  text_return _ Pointer to the structure that contains
                       the information about the text block, which has been read
                       by XawTextSourceRead;
                       length - the Position of the last char in the text block
          Returns:     True if Found; otherwise, it returns False.
*******************************************************************************/
int SearchBackward(
Widget source,
XawTextBlock *text_return,
XawTextPosition length)
{
    int count = 1;
    XawTextPosition NewPos = length - 2;

    while((NewPos >= 0 ) && count)
     {
        NewPos = XawTextSourceRead(source,NewPos,text_return,1) - 2;
        if(text_return->ptr[0] == ')')
          count++;
        else if (text_return->ptr[0] == '(')
          count--;
     }
    if(count)          /* Unfound */
      return(False);
    else
      return(True);    /* Found */
}

/*******************************************************************************
          Name:        EditorBeginingOfFileCallback
          Description: Move to the beginning of the file.
                       It is Called when Begining of File 
                       is selected from the Edit menu
          Arguments:  w - menu item that was selected
                       client_data - Widget to edit
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
void EditorBeginingOfFileCallback(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
  MoveBeginningOfFile((Widget)client_data, &TheEvent);
  }

/*******************************************************************************
          Name:        EditorEndOfFileCallback
          Description: Moves the cursor to the end of the file.
                       It is called when End of File is 
                       selected from the Edit menu.
          Arguments:  w - menu item that was selected
                       client_data - Widget to edit
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
void EditorEndOfFileCallback(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
  MoveEndOfFile((Widget)client_data, &TheEvent);
  }

/*******************************************************************************
          Name:        EditorFontSelect
          Description: Changes to font selected by user
          Arguments:  w - menu item that was selected
                       client_data - edit_text asciitext widget
                       call_data - Not used
          Returns:     None
*******************************************************************************/
static void EditorFontSelect(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
  Widget edit_text = (Widget)client_data;
  XFontStruct *font;
  Arg args[1];

  if ((font = XLoadQueryFont(XtDisplay(edit_text), XtName(w))) != NULL)
    {
    XtSetArg(args[0], XtNfont, font);
    XtSetValues(edit_text, args, 1);
    }
  }

/*******************************************************************************
          Name:        EditorHelpSelect
          Description: Changes to font selected by user
          Arguments:  w - menu item that was selected
                       client_data - edit_text asciitext widget
                       call_data - Not used
          Returns:     None
*******************************************************************************/
void EditorHelpSelect(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
  Arg args[6];
  Widget help, help_form, help_list;

  help = XtCreatePopupShell("help",
                            topLevelShellWidgetClass,
                            toplevel,
                            NULL, 0);

  XtSetArg(args[0], XtNdefaultDistance, 0);
  help_form = XtCreateManagedWidget("help_form",
                                    formWidgetClass,
                                    help,
                                    args, 1);

  XtSetArg(args[0], XtNborderWidth, 0);
  XtSetArg(args[1], XtNdefaultColumns, 2);
  XtSetArg(args[2], XtNforceColumns, True);
  XtSetArg(args[3], XtNlist, bindings);
  XtSetArg(args[4], XtNallowHoriz, True);
  XtSetArg(args[5], XtNallowVert, True);
  help_list = XtCreateManagedWidget("menu",
                                    listWidgetClass,
                                    help_form,
                                    args, 6);

  XtAddCallback(help_list, XtNcallback, CancelPopupSelect,
                (XtPointer)help_form);

  XtPopup(help, XtGrabNone);
  }

/*******************************************************************************
          Name:        EditorSave
          Description: Saves editor to a new file name
          Arguments:  w - Dialog widget
                       client_data - File to save
                       call_data - Not Used
          Returns:     None
          Comments:    MUST USE `XawTextGetSource' FOR `Saves' TO WORK
*******************************************************************************/
static void EditorSave(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
  XawAsciiSave(XawTextGetSource((Widget)client_data));
  XtDestroyWidget(XtParent(XtParent(w)));
  }

/*******************************************************************************
          Name:        EditorSaveAs
          Description: Saves editor to a new file name
          Arguments:  w - Dialog widget
                       client_data - AsciiText widget
                       call_data - Not Used
          Returns:     None
          Comments:    MUST USE `XawTextGetSource' FOR `SaveAs' TO WORK
*******************************************************************************/
void EditorSaveAs(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
  Arg args[1];
  String filename = XawDialogGetValueString(XtParent(w));
  Widget edit_text = XawTextGetSource((Widget)client_data);

  if (XawAsciiSaveAsFile(edit_text, filename))
    {
    XtSetArg(args[0], XtNstring, filename);
    XtSetValues(edit_text, args, 1);
    }

  XtDestroyWidget(XtParent(XtParent(w)));
  }

/*******************************************************************************
          Name:        EditorRevert
          Description: Reverts editor file
          Arguments:  w - Dialog widget
                       client_data - AsciiText widget
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
void EditorRevert(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
  Arg args[1];
  String filename = XawDialogGetValueString(XtParent(w));

  if (access(filename, 00) == 0)
    {
    XtSetArg(args[0], XtNstring, filename);
    XtSetValues((Widget)client_data, args, 1);
    }

    XtDestroyWidget(XtParent(XtParent(w)));
  }
  
/*******************************************************************************
          Name:        ExitEditor
          Description: Exits editor without saving
          Arguments:  w - Dialog widget
                       client_data - AsciiText widget
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
static void ExitEditor(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
  if(BatchString != NULL)
   {
     free(BatchString);
     BatchString = NULL;
   }
  XtDestroyWidget(XtParent(XtParent(w)));
  XtDestroyWidget(XtParent((Widget)client_data));
  }

/*******************************************************************************
          Name:        EditorSaveFirst
          Description: Saves editor then exits
          Arguments:  w - Dialog widget
                       client_data - AsciiText widget
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
static void EditorSaveFirst(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
  XawAsciiSave(XawTextGetSource((Widget)client_data));

  XtDestroyWidget(XtParent(XtParent(w)));
  XtDestroyWidget(XtParent(XtParent(XtParent((Widget)client_data))));
  }
 

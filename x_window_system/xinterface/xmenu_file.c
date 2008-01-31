   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*           X Windows Version 2.01  06/15/03          */ 
   /*                                                     */
   /*                  XMENU_FILE MODULE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains all the callback functions for the file */
/*   menu.                                                   */
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

#define _XMENU_FILE_SOURCE_

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>

#include "setup.h"

#include "constant.h"
#include "commline.h"
#include "router.h"
#include "symbol.h"
#include "scanner.h"

#include "xsetup.h"
#include "xclips.h"
#include "xclipstext.h"
#include "xmenu.h"
#include "xmain.h"
#include "xmenu_file.h"
#include "xmenu_wind.h"

#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <sys/param.h>

#ifndef MAX
#define MAX(x, y) ((x) > (y) ? (x) : (y))
#endif


/********** local functions not visible outside this file **********/
static char * GetBufferFromTextEdit(Widget);
static void printMatch(Widget,XtPointer,XtPointer);
static void printMatchForTextEdit(Widget,XtPointer,XtPointer);
static void ClipsSave(void);
static char **GetDirectory(void);
static void FileToDialog(Widget,XtPointer,XtPointer);
static void GetFileForCLIPS(char *);

/********** local variables that available to the other files **********/
Widget file_dribble;
Widget TheFile, file_list;
int file_item = -1;
char path[255];
char **filenames = NULL;
char *completionString = NULL;
int number_entries;


/*******************************************************************************
          Name:        EditCallback
          Description: Called when Edit is selected form File menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void EditCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   file_item = EDIT;
  (void)FileSelect();
  }

/*******************************************************************************
          Name:        CompletionDialogCallback
          Description: Called when Completion is selected form File menu
          Arguments:  w - menu item that was selected
                       client_data - dialog window or edit window
                       call_data - not used
          Returns:     None
*******************************************************************************/
void CompletionDialogCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
{
  void *theEnv = GetCurrentEnvironment();
  unsigned int NumberOfMatches;
  int length;
  struct symbolMatch *matches;
  char *commandString;

  /* ================================================== */
  /* Free the memory of completionString before assign  */
  /* it to the new string.                              */
  /* ================================================== */

  if(completionString != NULL)
   {
     free(completionString);
     completionString = NULL;
   }
  /* =========================================================== */
  /* Get the the uncompleted command string; if there is none    */
  /* sound the bell and exit, else determine if the last token   */
  /* of the string can be complete                               */
  /* =========================================================== */

  commandString = GetCommandString(GetCurrentEnvironment());
  if(commandString != NULL)
   {
    length = strlen(commandString);
    commandString = GetCommandCompletionString(GetCurrentEnvironment(),commandString,length);
   }
  if(commandString == NULL)
   {
     XBell(XtDisplay(toplevel),100);
     return;
   }

  /* ============================================================ */
  /* Copy the command string to a global variable for later use.  */
  /* Global completionString has to be used here due to the       */
  /* limitation of the number of arguments could be passed in the */
  /* call back function of in X window  system.                   */
  /* ============================================================ */

  completionString = (char*)malloc(strlen(commandString) + 1);
  strcpy(completionString,commandString);

  /* ============================================================ */
  /* Find the match(es). If there is none, sound the bell and     */
  /* exit; else if there is one match complete the command; else  */
  /* if there are more than one display them                      */
  /* ============================================================ */

  matches = FindSymbolMatches(GetCurrentEnvironment(),completionString,&NumberOfMatches,NULL);
  if(NumberOfMatches == 0)
   {
     XBell(XtDisplay(toplevel),100);
     return;
   }
  else if (NumberOfMatches == 1)
   {
      length = strlen(completionString);
      AppendCommandString(GetCurrentEnvironment(),&(matches->match->contents[length]));
      EnvPrintRouter(theEnv,"stdin",&(matches->match->contents[length]));
   }
  else
   {
      DisplayMatchedList(dialog_text,matches);
   }
}

/*******************************************************************************
          Name:        CompletionEditCallback
          Description: Called when Completion is selected form File menu
                       in the editor.
          Arguments:  w - menu item that was selected
                       client_data - dialog window or edit window
                       call_data - not used
          Returns:     None
*******************************************************************************/
void CompletionEditCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
{
  unsigned int NumberOfMatches;
  int length;
  struct symbolMatch *matches;
  XawTextBlock text;
  char *matchString = NULL;
  Widget source = XawTextGetSource((Widget)client_data);
  XawTextPosition CurrentPosition,EndPosition;

  /* ================================================== */
  /* Free the memory of completionString before assign  */
  /* it to the new string.                              */
  /* ================================================== */

  if(completionString != NULL)
   {
     free(completionString);
     completionString = NULL;
   }
  
  /* =================================================== */
  /* Get the beginning and ending positions of the       */
  /* selection. If there is no selection get the last    */
  /* word from the cursor.                               */
  /* ====================================================*/
  
  XawTextGetSelectionPos((Widget)client_data,&CurrentPosition,&EndPosition);
  if(CurrentPosition == EndPosition)  /* No selection was made */
   {
     matchString = GetBufferFromTextEdit((Widget)client_data);
     length = strlen(matchString);
   }
  else
   {
      XawTextSourceRead(source,CurrentPosition,&text,EndPosition - CurrentPosition);
      XawTextUnsetSelection((Widget)client_data);
      XawTextSetInsertionPoint((Widget)client_data,EndPosition);
      matchString = text.ptr;
      length = text.length;
   }

  /* ======================================= */
  /* Determine if the word can be matched.   */
  /* ======================================= */

  matchString = GetCommandCompletionString(GetCurrentEnvironment(),matchString,length);
  if(matchString == NULL)
   {
     XBell(XtDisplay(toplevel),100);
     return;
   }
  completionString = (char*)malloc(strlen(matchString) + 1);
  strcpy(completionString,matchString);
  matches = FindSymbolMatches(GetCurrentEnvironment(),completionString,&NumberOfMatches,NULL);
  if(NumberOfMatches == 0)
   {
     XBell(XtDisplay(toplevel),100);
     return;
   }
  else if (NumberOfMatches == 1)
   {
      length = strlen(completionString);
      text.firstPos = 0;
      text.length  = strlen(&(matches->match->contents[length]));   
      text.ptr  = &(matches->match->contents[length]);
      XawTextReplace((Widget)client_data,
                        XawTextGetInsertionPoint((Widget)client_data),
                        XawTextGetInsertionPoint((Widget)client_data),&text);
      XawTextSetInsertionPoint((Widget)client_data,
                     XawTextGetInsertionPoint((Widget)client_data) + text.length);

   }
  else
   {
      DisplayMatchedList((Widget)client_data,matches);
   }
}
/*******************************************************************************
 GetBufferFromTextEdit
 Description : This function will return the last word in the editor
               from the cursor
 *******************************************************************************/
static char * GetBufferFromTextEdit(
 Widget w)
{
   XawTextBlock text_return;
   char *buffer;

   Widget source = XawTextGetSource(w);
   XawTextPosition NewPos,EndPos = XawTextGetInsertionPoint(w);

   /* ================================================ */
   /*  If Cursor is at the begining return empty       */
   /*  string,orther while move the cursor backward    */
   /*  until it hits the space then read and return    */
   /*  the last word.                                  */
   /* ================================================ */

   if(EndPos == 0)
    return("");
   NewPos = EndPos - 1;
   XawTextSourceRead(source,NewPos,&text_return,1);
   while((text_return.ptr[0] != ' ') && (NewPos != 0))
    {
      NewPos--;      
      XawTextSourceRead(source,NewPos,&text_return,1);
    }
   if(NewPos != 0)
    NewPos++;
   XawTextSourceRead(source,NewPos,&text_return,EndPos - NewPos);
   buffer = (char *)malloc(text_return.length + 1);
   strncpy(buffer,text_return.ptr,text_return.length);
   buffer[text_return.length] = 0;
   return(buffer);
}
/*******************************************************************************
     Name DisplayMatchedList
     Description : Called when there are more than one matches for completion
                   command
     
*******************************************************************************/
int DisplayMatchedList(
Widget w,
struct symbolMatch *matches)
{
   Widget matchShell,matchForm,matchViewport,
          matchDialog,matchList;
   int n;

   if(GetMatchList(matches) == 0)
     return(0);
   matchShell = XtCreatePopupShell("Matches",
                                    topLevelShellWidgetClass,
                                    toplevel,
                                    NULL, 0);
   matchForm = XtCreateManagedWidget( "manager_form", formWidgetClass,
                                        matchShell, NULL,0);
  
  XtSetArg(TheArgs[0],XtNallowHoriz, True);

  XtSetArg(TheArgs[1],XtNallowVert, True); 

  matchViewport = XtCreateManagedWidget("manager_viewport",viewportWidgetClass,
                                        matchForm,NULL,0);

  n = 0;
  XtSetArg(TheArgs[n],XtNlist,item_list);n++;
  matchList = XtCreateManagedWidget("manager_list",
                                       listWidgetClass,
                                       matchViewport,
                                       TheArgs,n);
  n = 0;
  XtSetArg(TheArgs[n], XtNresizable, True);n++;
  XtSetArg(TheArgs[n],XtNlabel,"");n++;
  XtSetArg(TheArgs[n], XtNvalue, "");n++;
  XtSetArg(TheArgs[n], XtNfromVert, matchViewport);n++;
  XtSetArg(TheArgs[n], XtNicon, clips_logo);n++;
  XtSetArg(TheArgs[n], XtNleft, XtChainLeft);n++;
  XtSetArg(TheArgs[n], XtNright, XtChainRight);n++;
  XtSetArg(TheArgs[n], XtNtop, XtChainBottom);n++;
  XtSetArg(TheArgs[n], XtNbottom, XtChainBottom);n++;

/* ============================================================= */
/*  If the current active window is clips dialog box then pass   */
/*  the appropriate function to handle the match for the clips   */
/*  dialog; else the funcTion handling the match for the text    */
/*  editor is passed as the callback function.                   */
/* ============================================================= */ 

  if(w == dialog_text)
   {
    matchDialog = XtCreateManagedWidget("match_dialog",
                                      dialogWidgetClass,
                                      matchForm,
                                      TheArgs, n);
    XawDialogAddButton(matchDialog, "SELECT",printMatch, (XtPointer)completionString);
   }
  else
   {
     matchDialog = XtCreateManagedWidget("match_editor",
                                      dialogWidgetClass,
                                      matchForm,
                                      TheArgs, n);
     XawDialogAddButton(matchDialog, "SELECT",printMatchForTextEdit,(XtPointer)w);
   }
  XawDialogAddButton(matchDialog, "CANCEL", CancelPopupSelect,
                     (XtPointer) matchForm);
  XtAddCallback(matchList, XtNcallback, FileToDialog, (XtPointer) matchDialog);
  XtPopup(matchShell,XtGrabNonexclusive);
  
  return 0;
}

/*******************************************************************************
          Name:        MatchDialogReturnD
          Description: Called when pressing return key in match dialog widget
          Arguments:  w - Widget that caused action to be called
                       event - Not used
                       params - Dialog widget
                       num_params - Not used
          Returns:     None
*******************************************************************************/
void MatchDialogReturnD(
  Widget w,
  XEvent *event,
  String *params,
  Cardinal *num_params)
  {
    printMatch(w,(XtPointer)completionString,NULL);
  }

/*******************************************************************************
          Name:        MatchDialogReturnE
          Description: Called when pressing return key in match dialog widget
          Arguments:  w - Widget that caused action to be called
                       event - Not used
                       params - editor widget
                       num_params - Not used
          Returns:     None
          Notes :      Currently, this does not work yet since I
                       do not know how to pass the text editor without
                       make it a global variable.
*******************************************************************************/
void MatchDialogReturnE(
  Widget w,
  XEvent *event,
  String *params,
  Cardinal *num_params)
  {
    /*printMatchForTextEdit(w,params,NULL);*/
  }

/*******************************************************************************
          Name:        printMatch
          Description: Simulates callbacks for dialog widget
          Arguments:  w - Dialog widget
                       client_data - Dialog widget
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
static void printMatch(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
{
  void *theEnv = GetCurrentEnvironment();
  String aString = XawDialogGetValueString(XtParent(w));
  int length = strlen((char*)client_data);

  AppendCommandString(theEnv,&(aString[length]));
  EnvPrintRouter(theEnv,"stdin",&(aString[length]));
  XtDestroyWidget(XtParent(XtParent(XtParent(w))));
}

/*******************************************************************************
          Name:        printMatchForTextEdit
          Description: Simulates callbacks for dialog widget
          Arguments:  w - Dialog widget
                       client_data - Dialog widget
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
static void printMatchForTextEdit(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
{
  XawTextBlock text;
  String aString = XawDialogGetValueString(XtParent(w));
  Widget text_widget = (Widget)client_data;
  int length;

  length = strlen(completionString);
  text.firstPos = 0;
  text.length  = strlen(&(aString[length]));
  text.ptr  = &(aString[length]);
  XawTextReplace(text_widget,
                 XawTextGetInsertionPoint(text_widget),
                 XawTextGetInsertionPoint(text_widget),&text);
  XawTextSetInsertionPoint(text_widget,
                     XawTextGetInsertionPoint(text_widget) + text.length);
  XtDestroyWidget(XtParent(XtParent(XtParent(w))));
}


/*******************************************************************************
          Name:        GetDefruleList
          Description: Gets the list of rules
          Arguments:  None
          Returns:
*******************************************************************************/
int GetMatchList(
struct symbolMatch *matches)
{
  int maxItems = 20,itemCount;
  if(matches == NULL)
    return(0);
  if(item_list != NULL)
    {
      free(item_list);
      item_list = NULL;
    }
  item_list = (String *)calloc(maxItems,sizeof(String));
  for(itemCount = 0;matches != NULL;matches = matches->next)
      {
         item_list[itemCount] = balloc(strlen(matches->match->contents) + 1,char);
         strcpy(item_list[itemCount], matches->match->contents);
         itemCount++;
         if(itemCount == (maxItems -1))
          {
            maxItems = 2*maxItems;
            item_list = (String *)realloc(item_list,maxItems * sizeof(String));
          }
      }
  item_list[itemCount] = NULL;
  sortList(item_list,itemCount);
  return(itemCount);
}

/**********************************************************************
 *  sortList
 **********************************************************************/

void sortList(
String *list,
int num)
{
   int i,j,theIndex;
   char *tempString;   

   if(num == 1)
     return;
   for(i = 0;i < num;i++)
    {
      tempString = list[i];
      theIndex = i;
      for(j = i + 1;j < num; j++)
       {
         if(strcmp(tempString,list[j]) > 0)
          {
            tempString = list[j];
            theIndex = j;
          }
       }
      if(i != theIndex)
       {
         list[theIndex] = list[i];
         list[i] = tempString;
       }
    }
}

/*******************************************************************************
          Name:        LoadBatchCallback
          Description: Called when Load Batch is selected form File menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void LoadBatchCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   file_item  = LOADBATCH;
  (void)FileSelect();
  }

/*******************************************************************************
          Name:        LoadBinaryCallback
          Description: Called when Load Binary is selected form File menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void LoadBinaryCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   file_item  = LOADBINARY;
  (void)FileSelect();
  }

/*******************************************************************************
          Name:        LoadFactsCallback
          Description: Called when Load Facts is selected form File menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void LoadFactsCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   file_item  = LOADFACTS;
  (void)FileSelect();
  }

/*******************************************************************************
          Name:        LoadRulesCallback
          Description: Called when Load Rules is selected form File menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void LoadRulesCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   file_item = LOADRULES;
  (void)FileSelect();

  }

/*******************************************************************************
          Name:        DribbleCallback
          Description: Called when Dribble is selected form File menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void DribbleCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
  void *theEnv = GetCurrentEnvironment();
  MoveEndOfFile(dialog_text, &TheEvent);
  file_item = DRIBBLEON;
  if (Dribble_status)
    {
    XtSetArg(TheArgs[0], XtNleftBitmap, None);
    XtSetValues(file_dribble, TheArgs, 1);
    SetCommandString(theEnv,"(dribble-off)\n");
    if(!CommandLineData(theEnv)->EvaluatingTopLevelCommand)
      EnvPrintRouter(theEnv,"wclips","(dribble-off)\n");
    quit_get_event = True;
    Dribble_status = !Dribble_status;
    }
  else
    FileSelect();
  }

/*******************************************************************************
          Name:        SaveBinaryCallback
          Description: Called when Save Binary is selected form File menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void SaveBinaryCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
  file_item = SAVEBINARY;
  ClipsSave();
  }

/*******************************************************************************
          Name:        SaveFactsCallback
          Description: Called when Save Facts is selected form File menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void SaveFactsCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {

  file_item = SAVEFACTS;
  ClipsSave();
  }

/*******************************************************************************
          Name:        SaveRulesCallback
          Description: Called when Save Rules is selected form File menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void SaveRulesCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {

  file_item = SAVERULES;
  ClipsSave();
  }

/*******************************************************************************
          Name:        QuitCallback
          Description: Called when Quit is selected form File menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void QuitCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
  Widget confirmshell, confirm;

  confirmshell = XtCreatePopupShell("Confirmation",
                                    topLevelShellWidgetClass,
                                    toplevel,
                                    NULL, 0);

  XtSetArg(TheArgs[0], XtNlabel, "Quit XCLIPS.\nAre you sure?");
  XtSetArg(TheArgs[1], XtNicon, clips_logo);
  confirm = XtCreateManagedWidget("confirm",
                                  dialogWidgetClass,
                                  confirmshell,
                                  TheArgs, 2);

  XawDialogAddButton(confirm, "Quit", Quit, (XtPointer) confirm);
  XawDialogAddButton(confirm, "Restart", Restart, (XtPointer) confirm);
  XawDialogAddButton(confirm, "Cancel", CancelPopupSelect, (XtPointer) confirm);

  XtPopup(confirmshell, XtGrabNonexclusive);
  }

/*******************************************************************************
          Name:        ClipsSave
          Description: Prompts for file name to execute CLIPS' bsave,
                       save-facts, or save functions
          Arguments:  None
          Returns:     None
*******************************************************************************/
static void ClipsSave()
  {
  Widget popup, file_dialog;

  popup = XtCreatePopupShell("File",
                             topLevelShellWidgetClass,
                             toplevel,
                             NULL, 0);

  XtSetArg(TheArgs[0], XtNlabel, "Enter file name:");
  XtSetArg(TheArgs[1], XtNvalue,  "");
  XtSetArg(TheArgs[2], XtNicon, clips_logo);
  file_dialog = XtCreateManagedWidget("file_dialog",
                                      dialogWidgetClass,
                                      popup,
                                      TheArgs, 3);
  XawDialogAddButton(file_dialog, "Save", IntSave, (XtPointer)NULL);
  XawDialogAddButton(file_dialog, "Cancel", CancelPopupSelect,
                     (XtPointer)file_dialog);

  XtPopup(popup, XtGrabNonexclusive);
  }

/*******************************************************************************
          Name:        IntSave
          Description: Eexecutes CLIPS' bsave, save-facts, or save functions
          Arguments:  w - Dialog Widget
                       client_data - Not Used
                       call_data - Not Used
          Returns:     None
*******************************************************************************/
void IntSave(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
  void *theEnv = GetCurrentEnvironment();
  char *filename = XawDialogGetValueString(XtParent(w));

  switch(file_item)
    {
    case SAVEBINARY:
      EnvPrintRouter(theEnv,"wclips", "(bsave ");
      SetCommandString(theEnv,"(bsave");
      AppendCommandString(theEnv,"\"");
      EnvPrintRouter(theEnv,"wclips", "\"");
      AppendCommandString(theEnv,filename);
      EnvPrintRouter(theEnv,"wclips", filename);
      AppendCommandString(theEnv,"\"");
      EnvPrintRouter(theEnv,"wclips", "\"");
      AppendCommandString(theEnv,")\n");
      EnvPrintRouter(theEnv,"wclips", ")\n");
      quit_get_event = True;
    break;

    case SAVEFACTS:
      EnvPrintRouter(theEnv,"wclips", "(save-facts ");
      SetCommandString(theEnv,"(save-facts");
      AppendCommandString(theEnv,"\"");
      EnvPrintRouter(theEnv,"wclips", "\"");
      AppendCommandString(theEnv,filename);
      EnvPrintRouter(theEnv,"wclips", filename);
      AppendCommandString(theEnv,"\"");
      EnvPrintRouter(theEnv,"wclips", "\"");
      AppendCommandString(theEnv,")\n");
      EnvPrintRouter(theEnv,"wclips", ")\n");
      quit_get_event = True;
    break;

    case SAVERULES:
      EnvPrintRouter(theEnv,"wclips", "(save ");
      SetCommandString(theEnv,"(save");
      AppendCommandString(theEnv,"\"");
      EnvPrintRouter(theEnv,"wclips", "\"");
      AppendCommandString(theEnv,filename);
      EnvPrintRouter(theEnv,"wclips", filename);
      AppendCommandString(theEnv,"\"");
      EnvPrintRouter(theEnv,"wclips", "\"");
      AppendCommandString(theEnv,")\n");
      EnvPrintRouter(theEnv,"wclips", ")\n");
      quit_get_event = True;
    break;
    }

  XtDestroyWidget(XtParent(XtParent(w)));
  }

/*******************************************************************************
          Name:        FileSelect
          Description: Pops up window in center of the Dialog Window for file
                       selection by user
          Arguments:  None
          Returns:     None
          Contrubuting Programmers:  Albert Leigh - MacDonnell Douglas
                                     Stan Smith - Barrios
                                     some guy upstairs
******************************************************************************/
void FileSelect()
  {
  Widget file_form, file_dialog, view;

  /*XDefineCursor(XtDisplay(toplevel),toplevel,XC_watch);*/
  TheFile = XtCreatePopupShell("File",
                            topLevelShellWidgetClass,
                            toplevel,
                            NULL, 0);

  file_form = XtCreateManagedWidget("file_form",
                                    formWidgetClass,
                                    TheFile,
                                    NULL, 0);

  XtSetArg(TheArgs[0], XtNforceBars, True);
  XtSetArg(TheArgs[1], XtNbottom, XtChainBottom);
  XtSetArg(TheArgs[2], XtNheight,150);
  XtSetArg(TheArgs[3], XtNallowHoriz,True);
  XtSetArg(TheArgs[4],XtNallowVert,True);
  view = XtCreateManagedWidget("view",
                               viewportWidgetClass,
                               file_form,
                               TheArgs, 5);

  /* =============================================================== *
   *  Create the Select/Cancel dialog box in the file selection      *
   *  dialog box.                                                    *
   * =============================================================== */

  XtSetArg(TheArgs[0], XtNresizable, True);
  XtSetArg(TheArgs[1], XtNlabel, "Enter File Name");
  XtSetArg(TheArgs[2], XtNvalue, "");
  XtSetArg(TheArgs[3], XtNfromVert, view);
  XtSetArg(TheArgs[4], XtNicon, clips_logo);
  XtSetArg(TheArgs[5], XtNleft, XtChainLeft);
  XtSetArg(TheArgs[6], XtNright, XtChainRight);
  XtSetArg(TheArgs[7], XtNtop, XtChainBottom);
  XtSetArg(TheArgs[8], XtNbottom, XtChainBottom);
  file_dialog = XtCreateManagedWidget("file_dialog",
                                      dialogWidgetClass,
                                      file_form,
                                      TheArgs, 9);
  XawDialogAddButton(file_dialog, "SELECT", MenuFunc, (XtPointer) file_dialog);
  XawDialogAddButton(file_dialog, "CANCEL", CancelPopupSelect,
                     (XtPointer) file_form);

  XtSetArg(TheArgs[0], XtNfromHoriz, file_dialog);
  XtSetArg(TheArgs[1], XtNfromVert, view);

  /* =============================================================== *
   *  Get the path of the current dirrectory                         *
   * =============================================================== */

  if(getwd(path) == NULL)
    printf("Error getting current working directory '%s'\n", path);

  if(path[strlen(path) - 1] != '/')
    strcat(path, "/");

  /* =============================================================== *
   *  Create the file dialog list box                                *
   * =============================================================== */

  XtSetArg(TheArgs[0], XtNdefaultColumns, 4);
  XtSetArg(TheArgs[1], XtNlist, GetDirectory());
  XtSetArg(TheArgs[2], XtNforceColumns, False);
  XtSetArg(TheArgs[3], XtNverticalList, True);
  XtSetArg(TheArgs[4], XtNinternalWidth, 10);
  file_list = XtCreateManagedWidget("file_dialog",
                                    listWidgetClass,
                                    view,
                                    TheArgs, 5);
  XtAddCallback(file_list, XtNcallback, FileToDialog, (XtPointer) file_dialog);

  XtPopup(TheFile, XtGrabNonexclusive);
  /*XDefineCursor(XtDisplay(toplevel),toplevel,None);*/
  }

/*******************************************************************************
          Name:        GetDirectory
          Description: used with FileSelect to create list of filenames in a
                       specific directory
          Arguments:  None
          Returns:     None
          Contrubuting Programmers:  Albert Leigh - MacDonnell Douglas
                                     Lac Nguyen - Computer Science Corp.
                                     Stan Smith - Barrios
                                     some guy upstairs
*******************************************************************************/
static char **GetDirectory()
  {
   int fcount;
   char *fullpath;
   DIR *dirp;
   struct direct *entry;
   int namelength = 0;

   if ((dirp = opendir(path)) == NULL)
     {
      number_entries = 1;
      filenames = (char **) calloc(1,sizeof(char **));
      filenames[0] = (char *) malloc(sizeof(char) * 14);
      strcpy(filenames[0], "..");
      return(filenames);
     }
     
   /*======================================================*/
   /* Determine the number of file names in the directory. */
   /*======================================================*/

   fcount = 0;

   while ((entry = readdir(dirp)) != NULL)
     {
      namelength = MAX(namelength,strlen(entry->d_name));
      fcount++;
     }
     
   /*==================================================================*/
   /* Make sure the memory allocated for the filename will contain it. */
   /*==================================================================*/

   namelength = MAX((namelength + 2), 14);
   if (strcmp(path,"/") == 0)
     { fcount--; }
   number_entries = fcount - 1;

   rewinddir(dirp);

   filenames = (char **)calloc(fcount, sizeof(char *));
   filenames[0] = (char *)malloc(sizeof(char *) * fcount * namelength);
   fullpath  = (char *) malloc(sizeof(char) * (strlen(path) + namelength));
   fcount = 0;

   /*=========================================================*/
   /* Get the list of file or directory names in a directory. */
   /*=========================================================*/

   while ((entry = readdir(dirp)) != NULL)
     {
      if (strcmp(entry->d_name, "."))
        {
         if ((strcmp(path,"/") != 0)|| (strcmp (entry->d_name,"..") != 0))
           {
            filenames[fcount] = *filenames + (fcount * namelength);
            strcpy(filenames[fcount], entry->d_name);

            sprintf(fullpath, "%s%s", path, entry->d_name);

            if (IsDirectory(fullpath))
              { strcat(filenames[fcount], "/"); }
            fcount++;
           }	
        }
     }

   closedir(dirp);

   qsort(*filenames, fcount, (sizeof(char) * namelength), 
        (int (*)(const void *, const void *)) strcmp);

   return(filenames);
  }

/*******************************************************************************
          Name:        IsDirectory
          Description: used with FileSelect to test for directory
          Arguments:  temppath - directory temppath to check for
          Returns:     0 - for directory
                       1 - for not directory
          Contrubuting Programmers:  Albert Leigh - MacDonnell Douglas
                                     Stan Smith - Barrios
                                     some guy upstairs
*******************************************************************************/
int IsDirectory(
  char *temppath)
  {
  struct stat sbuf;

  if(!stat(temppath, &sbuf) && ((sbuf.st_mode & S_IFMT) == S_IFDIR))
    return(1);

  else
    return(0);
  }

/*******************************************************************************
          Name:        FileToDialog
          Description: copies selected list string to dialog's asciitext window
          Arguments:  w - list widget
                       client_data - dialog widget
                       call_data - list string selected
          Returns:     None
          Contrubuting Programmers:  Albert Leigh - MacDonnell Douglas
                                     Stan Smith - Barrios
                                     some guy upstairs
*******************************************************************************/
static void FileToDialog(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
  XawListReturnStruct *item = (XawListReturnStruct *)call_data;
  char *ptr;

  if (!strcmp(item->string, "../"))
    {
    path[strlen(path) - 1] = '\0';
    ptr = strrchr(path, '/');
    ptr++;
    *(ptr) = '\0';

    /*
    for(i = 0; i < number_entries; i++)
      free(filenames[i]);
    */
    if (filenames != NULL)
      {
       free(filenames[0]);
       free(filenames);
      }

    XawListChange(w, GetDirectory(), 0, 0, True);
    }
  else if (strrchr(item->string, '/'))
    {
    strcat(path, item->string);
    XawListChange(w, GetDirectory(), 0, 0, True);
    }

  else
    {
    XtSetArg(TheArgs[0], XtNvalue, item->string);
    XtSetValues((Widget)client_data, TheArgs, 1);
    }
  }

/*******************************************************************************
          Name:        LoadBatch
          Description: Loads batch file into CLIPS
          Arguments:  str - file to load
          Returns:     None
*******************************************************************************/
void LoadBatch(
  char *str)
  {
  void *theEnv = GetCurrentEnvironment();
  EnvPrintRouter(theEnv,"wclips", "(batch ");
  SetCommandString(theEnv,"(batch");
  GetFileForCLIPS(str);
  EnvPrintRouter(theEnv,"wclips", ")\n");
  AppendCommandString(theEnv,")\n");
  quit_get_event = True;
  }

/*******************************************************************************
          Name:        LoadBinary
          Description: Loads binary file into CLIPS
          Arguments:  str - file to load
          Returns:     None
*******************************************************************************/
void LoadBinary(
  char *str)
  {
  void *theEnv = GetCurrentEnvironment();
  EnvPrintRouter(theEnv,"wclips", "(bload ");
  SetCommandString(theEnv,"(bload");
  GetFileForCLIPS(str);
  EnvPrintRouter(theEnv,"wclips", ")\n");
  AppendCommandString(theEnv,")\n");
  quit_get_event = True;
  }

/*******************************************************************************
          Name:        LoadTheFacts
          Description: Loads facts file into CLIPS
          Arguments:  str - file to load
          Returns:     None
*******************************************************************************/
void LoadTheFacts(
  char *str)
  {
  void *theEnv = GetCurrentEnvironment();
  EnvPrintRouter(theEnv,"wclips", "(load-facts ");
  SetCommandString(theEnv,"(load-facts");
  GetFileForCLIPS(str);
  EnvPrintRouter(theEnv,"wclips", ")\n");
  AppendCommandString(theEnv,")\n");
  quit_get_event = True;
  }

/*******************************************************************************
          Name:        LoadRules
          Description: Loads rules into CLIPS
          Arguments:  str - file to load
          Returns:     None
*******************************************************************************/
void LoadRules(
  char *str)
  {
  void *theEnv = GetCurrentEnvironment();
  EnvPrintRouter(theEnv,"wclips", "(load ");
  SetCommandString(theEnv,"(load");
  GetFileForCLIPS(str);
  EnvPrintRouter(theEnv,"wclips", ")\n");
  AppendCommandString(theEnv,")\n");
  quit_get_event = True;
  }

/*******************************************************************************
          Name:        IntDribbleOn
          Description: Turns on dribble
          Arguments:  str - file to dribble to
          Returns:     None
*******************************************************************************/
void IntDribbleOn(
  String str)
  {
  void *theEnv = GetCurrentEnvironment();
  if(!CommandLineData(theEnv)->EvaluatingTopLevelCommand)
    EnvPrintRouter(theEnv,"wclips", "(dribble-on ");
  SetCommandString(theEnv,"(dribble-on");
  GetFileForCLIPS(str);
  if(!CommandLineData(theEnv)->EvaluatingTopLevelCommand)
    EnvPrintRouter(theEnv,"wclips", ")\n");
  AppendCommandString(theEnv,")\n");
  quit_get_event = True;
  if (((access(str, 02) == 0) || (access(str, 00))) && (strcmp(str, "\0") != 0))
    {
    Dribble_status = True;
    XtSetArg(TheArgs[0], XtNleftBitmap, checker);
    XtSetValues(file_dribble, TheArgs, 1);
    }
  }

/*******************************************************************************
          Name:        GetFileForCLIPS
          Description: Gets file for CLIPS to load
          Arguments:  file - File to get
          Returns:     None
*******************************************************************************/
static void GetFileForCLIPS(
  char *file)
  {
  void *theEnv = GetCurrentEnvironment();
  AppendCommandString(theEnv,"\"");
  AppendCommandString(theEnv,file);
  AppendCommandString(theEnv,"\"");
  if(!CommandLineData(theEnv)->EvaluatingTopLevelCommand)
   {
    EnvPrintRouter(theEnv,"wclips", "\"");
    EnvPrintRouter(theEnv,"wclips", file);
    EnvPrintRouter(theEnv,"wclips", "\"");
   }
  }

/******************************************************************************
          Name:        Restart
          Description: Restarts CLIPS
          Arguments:  w - not used
                       client_data - popup widget
                       call_data - not used
          Returns:     None
*******************************************************************************/
void Restart(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {  
   void *theEnv = GetCurrentEnvironment();
   system("xclips&");
   XclipsExit(theEnv,0);
  }

/******************************************************************************
          Name:        Quit
          Description: Quits CLIPS
          Arguments:  None
          Returns:     None
******************************************************************************/
void Quit(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)  
  {
   void *theEnv = GetCurrentEnvironment();
   XclipsExit(theEnv,0);
  }


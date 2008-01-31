   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*           X Windows Version 2.01  06/15/03          */ 
   /*                                                     */
   /*              XMENU_FILE HEADER MODULE               */
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

#ifndef _H_xmenu_file
#define _H_xmenu_file

void EditCallback(Widget,XtPointer,XtPointer);
void CompletionDialogCallback(Widget,XtPointer,XtPointer);
void CompletionEditCallback(Widget,XtPointer,XtPointer);
int DisplayMatchedList(Widget,struct symbolMatch *);
void MatchDialogReturnD(Widget,XEvent *,String *,Cardinal *);
void MatchDialogReturnE(Widget,XEvent *,String *,Cardinal *);
int GetMatchList(struct symbolMatch *);
void sortList(String *,int);
void LoadBatchCallback(Widget,XtPointer,XtPointer);
void LoadBinaryCallback(Widget,XtPointer,XtPointer);
void LoadFactsCallback(Widget,XtPointer,XtPointer);
void LoadRulesCallback(Widget,XtPointer,XtPointer);
void DribbleCallback(Widget,XtPointer,XtPointer);
void SaveBinaryCallback(Widget,XtPointer,XtPointer);
void SaveFactsCallback(Widget,XtPointer,XtPointer);
void SaveRulesCallback(Widget,XtPointer,XtPointer);
void QuitCallback(Widget,XtPointer,XtPointer);
void IntSave(Widget,XtPointer,XtPointer);
void FileSelect(void);
int IsDirectory(char *);
void LoadBatch(char *);
void LoadBinary(char *);
void LoadTheFacts(char *);
void LoadRules(char *);
void IntDribbleOn(String);
void Restart(Widget,XtPointer,XtPointer);
void Quit(Widget,XtPointer,XtPointer);

#ifndef _XMENU_FILE_SOURCE_
   extern char                     path[];
   extern Widget                   file_dribble;
   extern Widget                   TheFile;
   extern Widget                   file_list;
   extern int                      file_item;
#endif

#endif

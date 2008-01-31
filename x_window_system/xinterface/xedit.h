   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*           X Windows Version 2.0  01/31/02           */ 
   /*                                                     */
   /*                XEDIT HEADER MODULE                  */
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

void EditNewFile(Widget,XtPointer,XtPointer);
void EditorSaveCallback(Widget,XtPointer,XtPointer);
void EditorSaveAsCallback(Widget,XtPointer,XtPointer);
void EditorRevertCallback(Widget,XtPointer,XtPointer);
int FindSelection(void *,char *);
int SelectionGetc(void *,char *);
int  SelectionUngetc(void *,int,char *);
void EditorCompileSelectionCallback(Widget,XtPointer,XtPointer);
int FileFind(void *,char *);
int FileGetc(void *,char *);
int  FileUngetc(void *,int,char *);
void EditorCompileFileCallback(Widget,XtPointer,XtPointer);
int  LoadXFile(char *,char *);
void EditorBatchSelectionCb(Widget,XtPointer,XtPointer);
void EditorExitCallback(Widget,XtPointer,XtPointer);
void EditorCutCallback(Widget,XtPointer,XtPointer);
void EditorPasteCallback(Widget,XtPointer,XtPointer);
void EditorSearchReplaceCallback(Widget,XtPointer,XtPointer);
void FindMatchingParenthesisCallback(Widget,XtPointer,XtPointer);
int SearchForward(Widget,XawTextBlock *,XawTextPosition);
void WarningWindow(char *);
int SearchBackward(Widget,XawTextBlock *,XawTextPosition);
void EditorBeginingOfFileCallback(Widget,XtPointer,XtPointer);
void EditorEndOfFileCallback(Widget,XtPointer,XtPointer);
void EditorHelpSelect(Widget,XtPointer,XtPointer);
void EditorSaveAs(Widget,XtPointer,XtPointer);
void EditorRevert(Widget,XtPointer,XtPointer);




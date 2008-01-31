   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*           X Windows Version 2.01  06/15/03          */ 
   /*                                                     */
   /*              XCLIPSTEXT HEADER MODULE               */
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

#ifndef _H_xclipstext
#define _H_xclipstext

void UnKill(TextWidget,XEvent *);
void Stuff(TextWidget,XEvent *);
void MoveBeginningOfFile(Widget,XEvent *);
void MoveEndOfFile(Widget,XEvent *);
void DeleteCurrentSelection(Widget,XEvent *);
int LocalClipsInsertNewLine(TextWidget,XEvent *);
void InsertClipsString(Widget,XEvent *,String *,Cardinal *);

#ifndef _XCLIPSTEXT_SOURCE_
#endif

#endif

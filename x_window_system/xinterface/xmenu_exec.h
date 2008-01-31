   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*           X Windows Version 2.01  06/15/03          */ 
   /*                                                     */
   /*              XMENU_EXEC HEADER MODULE               */
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

#ifndef _H_xmenu_exec
#define _H_xmenu_exec

void ResetCallback(Widget,XtPointer,XtPointer);
void RunCallback(Widget,XtPointer,XtPointer);
void StepCallback(Widget,XtPointer,XtPointer);
void ClearCLIPSCallback(Widget,XtPointer,XtPointer);
void ClearScreenCallback(Widget,XtPointer,XtPointer);

#endif
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*           X Windows Version 2.01  06/15/03          */ 
   /*                                                     */
   /*                XMENU HEADER MODULE                  */
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

#ifndef _H_xmenu
#define _H_xmenu

void DialogReturn(Widget,XEvent *,String *,Cardinal *);
void CreatePullDownMenus(Widget);
void CancelPopupSelect(Widget,XtPointer,XtPointer);
void PopdownSelect(Widget,XtPointer,XtPointer);
void MenuFunc(Widget,XtPointer,XtPointer);

#ifndef _XMENU_SOURCE_
   extern Widget                   defrule_manager;
   extern Widget                   deffact_manager;
   extern Widget                   deftemplate_manager;
   extern Widget                   deffunction_manager;
   extern Widget                   defgeneric_manager;
   extern Widget                   definstances_manager;
   extern Widget                   defclass_manager;
   extern Widget                   agenda_manager;
   extern Widget                   defglobal_manager;
   extern Widget                   FileItemWidgets[];
   extern Widget                   ExecItemWidgets[];
#endif

#endif

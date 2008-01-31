   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*           X Windows Version 2.01  06/15/03          */ 
   /*                                                     */
   /*              XMENU_OPT HEADER MODULE                */
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

#ifndef _H_xmenu_opt
#define _H_xmenu_opt

void OptionsWindow(Widget,XtPointer,XtPointer);
void SetStrategyCallback(Widget,XtPointer,XtPointer);
void SetSalienceCallback(Widget,XtPointer,XtPointer);
void OkayOptionsCallback(Widget,XtPointer,XtPointer);

#ifndef _XMENU_OPT_SOURCE_
   extern Widget                   option_widgets[];
   extern Widget                   strategy_widgets[];
   extern Widget                   sal_opt_widgets[];
#endif

#endif

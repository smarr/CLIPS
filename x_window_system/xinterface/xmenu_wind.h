   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*           X Windows Version 2.01  06/15/03          */ 
   /*                                                     */
   /*              XMENU_WIND HEADER MODULE               */
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

#ifndef _H_xmenu_wind
#define _H_xmenu_wind

void ModuleCallback(Widget,XtPointer,XtPointer);
void DefruleManagerCallback(Widget,XtPointer,XtPointer);
void DeffactManagerCallback(Widget,XtPointer,XtPointer);
void DeftemplateManagerCallback(Widget,XtPointer,XtPointer);
void DeffunctionManagerCallback(Widget,XtPointer,XtPointer);
void DefglobalManagerCallback(Widget,XtPointer,XtPointer);
int IntGetDefglobalList(void);
void DefgenericManagerCallback(Widget,XtPointer,XtPointer);
void DefinstancesManagerCallback(Widget,XtPointer,XtPointer);
void DefclassManagerCallback(Widget,XtPointer,XtPointer);
void AgendaManagerCallback(Widget,XtPointer,XtPointer);
int IntGetAgendaList(void);
void FactsWindowCallback(Widget,XtPointer,XtPointer);
void CreateFactWindow(void);
void AgendaWindowCallback(Widget,XtPointer,XtPointer);
void CreateAgendaWindow(void);
void FocusWindowCallback(Widget,XtPointer,XtPointer);
void CreateFocusWindow(void);
void InstancesWindowCallback(Widget,XtPointer,XtPointer);
void CreateInstanceWindow(void);
void GlobalsWindowCallback(Widget,XtPointer,XtPointer);
void CreateGlobalWindow(void);
void AllWindowsCallback(Widget,XtPointer,XtPointer);
void NoWindowsCallback(Widget,XtPointer,XtPointer);
void CommandLineCLIPSCallback(Widget,XtPointer,XtPointer);
void ColorUtilityCallback(Widget,XtPointer,XtPointer);
int IntGetDefruleLis(void);
int IntGetFactList(void);
int IntGetDeftemplateList(void);
int IntGetDeffunctionList(void);
int IntGetDefgenericList(void);
int IntGetDefmethodList(char *);
int IntGetDefinstancesList(void);
int IntGetDefclassList(void);
int IntGetDefmessgHndlerList(char *);
void InitializeList(String list[]);
void SetManagerList(Widget);
Widget GetManagerList(void);
int RefreshMngrList(void);
void ClearParameters(void);
void CancelSelectPrimary(Widget,XtPointer,XtPointer);

#ifndef _XMENU_WIND_SOURCE_
   extern Widget                   facts_window; 
   extern Widget                   agenda_window;
   extern Widget                   instances_window; 
   extern Widget                   globals_window;
   extern Widget                   focus_window;
   extern String                  *item_list;
   extern Widget                   agenda_text;
   extern Widget                   facts_text;
   extern Widget                   instances_text;
   extern Widget                   globals_text;
   extern Widget                   focus_text;
   extern Widget                   agenda;
   extern Widget                   facts;
   extern Widget                   instances;
   extern Widget                   globals;
   extern Widget                   focus;
   extern Boolean                  list_change;
   extern Boolean                  list1_change;
#endif

#endif

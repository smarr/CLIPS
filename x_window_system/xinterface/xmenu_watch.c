   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*           X Windows Version 2.01  06/15/03          */ 
   /*                                                     */
   /*                 XMENU_WATCH MODULE                  */
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
#include "xsetup.h"
#include "xclips.h"
#include "setup.h"
#include "constant.h"
#include "watch.h"
#include "xmain.h"
#include "xmenu.h"

#include "xmenu_watch.h"

/******* Global variables ***********/
char *WatchName[MAX_WATCH] = {"compilations","facts","rules","statistics","activations",
                              "focus","globals","deffunctions","generic-functions","methods",
                              "instances","slots","message-handlers","messages"};
Widget watchShell = NULL,watchForm = NULL;
Widget watch_widgets[MAX_WATCH];

/*******************************************************************************
          Name:        WatchWindow
          Description: Creates Watch menu
          arguments: 
          Returns:     None
*******************************************************************************/
void WatchWindow(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)

  {
   void *theEnv = GetCurrentEnvironment();
  int n = 0,i;

  Widget Okay,cancel,All,none;
  static char *WidgetName[MAX_WATCH] = {"Compilations","Facts","Rules","Statistics","Activations",
                                        "Focus","Globals","Deffunctions","Generic Functions","Methods",
                                        "Instances","Slots","Messages-handlers","Messages"};

  /* ====================================================== */
  /*  If the watch shell have already existed pop it up,    */
  /*  else create it.                                       */
  /* ====================================================== */

  if(watchShell != NULL)
    {
       for(i = 0; i < MAX_WATCH; i++)
         {
           n = 0;
           if (EnvGetWatchItem(theEnv,WatchName[i]))
             XtSetArg( TheArgs[n],XtNstate,True);
           else
             XtSetArg( TheArgs[n],XtNstate,False);
           n++;
           XtSetValues(watch_widgets[i],TheArgs,n);
         }
       XtPopup(watchShell,XtGrabExclusive);
       return;
    }
  /* ========================================= */
  /*   Create the watch toggle menu            */
  /* ========================================= */

  XtSetArg( TheArgs[n], XtNwidth,250);n++;
  XtSetArg( TheArgs[n], XtNheight,400);n++;
  watchShell = XtCreatePopupShell("Watch Menu",topLevelShellWidgetClass,XtParent(w),NULL,0);
  n = 0;
  watchForm = XtCreateManagedWidget( "watch_form", formWidgetClass,
                                        watchShell, TheArgs,n);
  n = 0;
  XtSetArg(TheArgs[n],XtNwidth,150);n++;
  XtSetArg(TheArgs[n],XtNhorizDistance,15);n++;
  for(i = 0; i < 7;i++)
   {
     if (EnvGetWatchItem(theEnv,WatchName[i]))
        XtSetArg( TheArgs[n],XtNstate,True);
     else
        XtSetArg( TheArgs[n],XtNstate,False);
     n++;
     watch_widgets[i] = XtCreateManagedWidget(WidgetName[i],
                                      toggleWidgetClass,
                                      watchForm,
                                      TheArgs, n);
     n =  2;
     XtSetArg(TheArgs[n],XtNfromVert,watch_widgets[i]);n++;
  }
 n = 1;
 XtSetArg(TheArgs[n],XtNfromHoriz,watch_widgets[0]);n++;
 for(; i < MAX_WATCH ; i++)
  {
      if (EnvGetWatchItem(theEnv,WatchName[i]))
        XtSetArg( TheArgs[n],XtNstate,True);
      else
        XtSetArg( TheArgs[n],XtNstate,False);
      n++;
      watch_widgets[i] = XtCreateManagedWidget(WidgetName[i],
                                      toggleWidgetClass,
                                      watchForm,
                                      TheArgs, n);
      n = 1;
      XtSetArg(TheArgs[n],XtNfromVert,watch_widgets[i]);n++;
      XtSetArg(TheArgs[n],XtNfromHoriz,watch_widgets[i - 7]);n++;
  }
  /* ======================= */
  /* Create the "All" button */
  /* ======================= */

  n = 0;
  XtSetArg(TheArgs[n],XtNcornerRoundPercent,40);n++;
  XtSetArg(TheArgs[n],XtNshapeStyle,XmuShapeRoundedRectangle);n++;
  XtSetArg(TheArgs[n],XtNwidth,150);n++;
  XtSetArg(TheArgs[n],XtNfromVert,watch_widgets[6]);n++;
  XtSetArg(TheArgs[n],XtNvertDistance,31);n++;
  XtSetArg(TheArgs[n],XtNlabel,"All");n++;
  All = XtCreateManagedWidget("watchButton",
                                       commandWidgetClass,
                                        watchForm,
                                        TheArgs, n);
  XtAddCallback(All, XtNcallback, WatchAllCallback, NULL);

  /* ============================= */
  /* Create the "None" button      */
  /* ============================= */

  n = 3;
  XtSetArg(TheArgs[n],XtNfromHoriz,All);n++;
  XtSetArg(TheArgs[n],XtNfromVert,watch_widgets[6]);n++;
  XtSetArg(TheArgs[n],XtNvertDistance,31);n++;
  XtSetArg(TheArgs[n],XtNhorizDistance,30);n++;
  XtSetArg(TheArgs[n],XtNlabel,"None");n++;
  none = XtCreateManagedWidget("watchButton",
                                       commandWidgetClass,
                                        watchForm,TheArgs,n);
  XtAddCallback(none, XtNcallback, WatchNoneCallback, NULL);

  /* ================================ */
  /* Create the "Okay" button         */
  /* ================================ */

  n = 3;
  XtSetArg(TheArgs[n],XtNfromVert,All);n++;
  XtSetArg(TheArgs[n],XtNvertDistance,15);n++;
  XtSetArg(TheArgs[n],XtNlabel,"Okay");n++;
  Okay = XtCreateManagedWidget("watchButton",
                                       commandWidgetClass,
                                        watchForm,
                                        TheArgs, n);
  XtAddCallback(Okay,XtNcallback,OkWatchCallback,(XtPointer)watchForm);

  /* ================================ */
  /*   Create the "Cancel" button     */
  /* ================================ */

  n = 3;
  XtSetArg(TheArgs[n],XtNfromVert,none);n++;
  XtSetArg(TheArgs[n],XtNvertDistance,15);n++;
  XtSetArg(TheArgs[n],XtNfromHoriz,Okay);n++;
  XtSetArg(TheArgs[n],XtNlabel,"Cancel");n++;
  XtSetArg(TheArgs[n],XtNhorizDistance,30);n++;
  cancel = XtCreateManagedWidget("watchButton",
                                       commandWidgetClass,
                                        watchForm,
                                        TheArgs, n);
  XtAddCallback(cancel,XtNcallback,PopdownSelect,(XtPointer)watchForm);
  XtPopup(watchShell,XtGrabExclusive);
  }


/**************************************************************************
   OkWatchCallback
   Description:  This function will reset the watch flags to the new
                 values and remove the watch window from the screen.
   Arguments:    w - widget that event was activated
                 client_data - NULL
                 call_data - Unused
   Return:       None
 **************************************************************************/
void OkWatchCallback(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
{
   void *theEnv = GetCurrentEnvironment();
   int i,n;
   Boolean OnOff = False;
   
   for(i = 0; i< MAX_WATCH; i++)
    {
       n = 0;
       XtSetArg(TheArgs[n],XtNstate,&OnOff);n++;
       XtGetValues(watch_widgets[i],TheArgs,n);
     /*----------------------------------------------------------------*/
     /* I have to do this because I am not sure if True and False in X */
     /* are defined the same as CLIPS_TRUE and CLIPS_FALSE             */
     /*----------------------------------------------------------------*/
       if((OnOff == True)&&(EnvGetWatchItem(theEnv,WatchName[i])!= CLIPS_TRUE))
        {
          EnvSetWatchItem(theEnv,WatchName[i], ON,NULL);
        }
       else if((OnOff == False) && (EnvGetWatchItem(theEnv,WatchName[i]) == CLIPS_TRUE))
        {
          EnvSetWatchItem(theEnv,WatchName[i], OFF,NULL);
        }
    }
   XtPopdown(XtParent(XtParent(w)));
   quit_get_event = True;
}

/*******************************************************************************
          Name:        WatchAllCallback
          Description: Called when Watch All button is activated.
                       It turns all the watch toggle buttons to ON, and
                       the watch flags' values will be reset only when 
                       the Okay button is pressed.
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void WatchAllCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
     int i, n = 0;

     XtSetArg(TheArgs[n],XtNstate,True);n++;
     for(i = 0;i < MAX_WATCH; i++)
      {
         XtSetValues(watch_widgets[i],TheArgs,n);
      }
   quit_get_event = True;
  }

/*******************************************************************************
          Name:        WatchNoneCallback
          Description: Called when Watch None is selected from Watch menu.
                       It turns all the watch toggle buttons to OFF, and
                       the values of the watch flags are resset only when 
                       the Okay button is pressed.
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void WatchNoneCallback(
  Widget w,
  XtPointer client_data,
  XtPointer call_data)
  {
     int i, n = 0;

     XtSetArg(TheArgs[n],XtNstate,False);n++;
     for(i = 0;i < MAX_WATCH ; i++)
      {
         XtSetValues(watch_widgets[i],TheArgs,n);
      }
    quit_get_event = True;
  }


   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*           X Windows Version 2.01  06/15/03          */ 
   /*                                                     */
   /*                  XMENU_WIND MODULE                  */
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

#define _XMENU_WIND_SOURCE_

#include <stdio.h>
#include <stdlib.h>

#include "setup.h"

#include "agenda.h"
#include "globldef.h"
#include "genrcfun.h"
#include "defins.h"
#include "classcom.h"
#include "commline.h"
#include "dffctdef.h"
#include "dffnxfun.h"
#include "engine.h"
#include "genrccom.h"
#include "insfun.h"
#include "moduldef.h"
#include "msgcom.h"
#include "object.h"
#include "router.h"
#include "rulebsc.h"
#include "ruledef.h"
#include "tmpltbsc.h"
#include "tmpltdef.h"
  
#include "xsetup.h"
#include "xmenu_wind.h"
#include "xclips.h"
#include "xmain.h"
#include "xmenu_file.h"
#include "xclipstext.h"

/********** local functions not visible outside this file **********/
static char **IntGetModuleList(void);
static void DoneSelectDefmoduleCallback(Widget,XtPointer,XtPointer);
static void DefmoduleSelectCallback(Widget,XtPointer,XtPointer);
static void DefglobalRemoveCallback(Widget,XtPointer,XtPointer);
static  void DefglobalPprintCallback(Widget,XtPointer,XtPointer);
static void AgendaRemove(Widget,XtPointer,XtPointer);
static void AgendaFire(Widget,XtPointer,XtPointer);
static void DefruleRemoveCallback(Widget,XtPointer,XtPointer);
static void DefruleMatchesCallback(Widget,XtPointer,XtPointer);
static void DefrulePprintCallback(Widget,XtPointer,XtPointer);
static void DefruleRefreshCallback(Widget,XtPointer,XtPointer);
static void DeffactsRemove(Widget,XtPointer,XtPointer);
static void DeffactsPprint(Widget,XtPointer,XtPointer);
static void DeftemplateRemove(Widget,XtPointer,XtPointer);
static void DeftemplatePprint(Widget,XtPointer,XtPointer);
static void DeffunctionRemoveCallback(Widget,XtPointer,XtPointer);
static void DeffunctionPprintCallback(Widget,XtPointer,XtPointer);
static void DefgenericRemoveCallback(Widget,XtPointer,XtPointer);
static void DefgenericPprintCallback(Widget,XtPointer,XtPointer);
static void DefgenericWatchCallback(Widget,XtPointer,XtPointer);
static void DefgenericMngrCheckBoxCallback(Widget,XtPointer,XtPointer);
static void DefgenericMethodCallback(Widget,XtPointer,XtPointer);
static void RemoveDefmethodCallback(Widget,XtPointer,XtPointer);
static void DefmethodPprintCallback(Widget,XtPointer,XtPointer);
static void DefmethodWatchCallback(Widget,XtPointer,XtPointer);
static void DefmethodMngrCheckBoxCallback(Widget,XtPointer,XtPointer);
static void DefinstancesRemoveCallback(Widget,XtPointer,XtPointer);
static void DefinstancesPprintCallback(Widget,XtPointer,XtPointer);
static void DefclassRemoveCallback(Widget,XtPointer,XtPointer);
static void DefclassDescribeCallback(Widget,XtPointer,XtPointer);
static void DefclassBrowseCallback(Widget,XtPointer,XtPointer);
static void DefclassPprintCallback(Widget,XtPointer,XtPointer);
static void DefclassMessageHandlersCallback(Widget,XtPointer,XtPointer);
static void RemoveMessageHandlerCallback(Widget,XtPointer,XtPointer);
static void MessageHandlerPprintCallback(Widget,XtPointer,XtPointer);
static void CancelSelectSecondary(Widget,XtPointer,XtPointer);
static void DefruleBreakPointCallback(Widget,XtPointer,XtPointer);
static void DefruleActivationCallback(Widget,XtPointer,XtPointer);
static void DefruleFiringsCallback(Widget,XtPointer,XtPointer);
static void DefruleMngrCheckboxesCallback(Widget,XtPointer,XtPointer);
static void WatchInstancesCallback(Widget,XtPointer,XtPointer);
static void WatchSlotCallback(Widget,XtPointer,XtPointer);
static void DefclssMngrChckbxCallback(Widget,XtPointer,XtPointer);
static void DeftemplateWatchCallback(Widget,XtPointer,XtPointer);
static void DeftemplateMngrCheckboxCallback(Widget,XtPointer,XtPointer);
static void DeffunctionWatchCallback(Widget,XtPointer,XtPointer);
static void DeffunctionMngrCheckboxCallback(Widget,XtPointer,XtPointer);
static void DefmessHdlrMngrWatchCallback(Widget,XtPointer,XtPointer);
static void DefmessHdlrMngrCheckBoxCallback(Widget,XtPointer,XtPointer);

/********** local variables available to other files ***********/
Widget agenda = NULL,agenda_form = NULL, agenda_text = NULL, facts = NULL, facts_form = NULL, 
       facts_text = NULL,instances = NULL,instances_form = NULL,
       instances_text = NULL,globals = NULL,globals_form = NULL,
       globals_text = NULL,focus = NULL, focus_form  = NULL,focus_text = NULL;

char *xclips_translation2 =
"\
Ctrl<Key>D:     no-op() \n\
Ctrl<Key>G:     no-op() \n\
Ctrl<Key>J:     no-op() \n\
Ctrl<Key>K:     no-op() \n\
Ctrl<Key>M:     no-op() \n\
Ctrl<Key>O:     no-op() \n\
Ctrl<Key>R:     search(forward) \n\
Meta<Key>Q:     no-op() \n\
:Meta<Key>d:    no-op() \n\
:Meta<Key>D:    no-op() \n\
:Meta<Key>h:    no-op() \n\
:Meta<Key>H:    no-op() \n\
:Meta<Key>]:    no-op() \n\
:Meta<Key>[:    no-op() \n\
~Shift Meta<Key>Delete:         no-op() \n\
 Shift Meta<Key>Delete:         no-op() \n\
~Shift Meta<Key>BackSpace:      no-op() \n\
 Shift Meta<Key>BackSpace:      no-op() \n\
<Key>Return:    no-op() \n\
<Key>Delete:    no-op() \n\
<Key>BackSpace: no-op() \n\
<Key>:          no-op() \n\
<Btn2Down>:     no-op() \n\
";

/********** local variables **********/
Boolean list_change = False;
Boolean list1_change = False;
static Widget manager_list, manager_list1;
static Boolean defrulemanager_flag = False;
static Boolean deffactsmanager_flag = False;
static Boolean deftemplatemanager_flag = False;
static Boolean deffunctionmanager_flag = False;
static Boolean defglobalmanager_flag = False;
static Boolean defgenericmanager_flag = False;
static Boolean defmethodmanager_flag = False;
static Boolean definstancesmanager_flag = False;
static Boolean defclassmanager_flag = False;
static Boolean agendamanager_flag = False;
static Boolean defmessagehandler_flag = False;
Widget facts_window, agenda_window,instances_window, globals_window,focus_window;
static String curr_def_name;
String *item_list;
String *item_list1;


/*******************************************************************************
          Name:        ModuleCallback
          Description: Called when Module is selected form Execution menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void ModuleCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   static char **moduleList = NULL;
   char *ModuleName;
   int i = 0;
   Widget defmoduleShell = NULL, defmoduleForm = NULL, defmoduleViewport = NULL,
          defmoduleList = NULL, done = NULL;

   /*======================*/
   /* Get the module list. */
   /*======================*/

   moduleList = IntGetModuleList();
   
   /*==============================*/
   /* Create the defmodule window. */
   /*==============================*/

   defmoduleShell = XtCreatePopupShell("Defmodule",
                                      topLevelShellWidgetClass,
                                      toplevel,
                                      NULL, 0);
   defmoduleForm = XtCreateManagedWidget("manager_form",
                                              formWidgetClass,
                                              defmoduleShell,
                                              NULL, 0);
   XtSetArg(TheArgs[0],XtNallowHoriz,True);
   XtSetArg(TheArgs[1],XtNallowVert,True);
   defmoduleViewport = XtCreateManagedWidget("manager_viewport",
                                                  viewportWidgetClass,
                                                  defmoduleForm,
                                                  TheArgs,2);
   if (moduleList == NULL)
     {
      moduleList = (char**)balloc(2,String);
      moduleList[0] = "";
      moduleList[1] = NULL;
     }
   else  /* Find the index of the current module from the list */
     {
      ModuleName = EnvGetDefmoduleName(theEnv,(struct defmodule*) EnvGetCurrentModule(theEnv));
      for (i = 0; moduleList[i] != NULL;)
        {
         if (strcmp(ModuleName,moduleList[i]) == 0)
            { break; }
         i++;
        }
     }
    
   XtSetArg(TheArgs[0],XtNlist,moduleList);
   defmoduleList = XtCreateManagedWidget("manager_list",
                                       listWidgetClass,
                                       defmoduleViewport,
                                       TheArgs, 1);
   
   /*===============================*/
   /* Highlight the current module. */
   /*===============================*/

   XawListHighlight(defmoduleList,i);

   XtAddCallback(defmoduleList,XtNcallback,
                 DefmoduleSelectCallback,defmoduleList);

   /*=========================*/
   /* Create the Done Button. */
   /*=========================*/
       
   XtSetArg(TheArgs[0], XtNlabel,"Done");
   XtSetArg(TheArgs[1], XtNfromHoriz, defmoduleViewport);
   done = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 defmoduleForm,
                                 TheArgs,2);
   XtAddCallback(done,XtNcallback,DoneSelectDefmoduleCallback,moduleList);
   XtPopup(defmoduleShell, XtGrabNonexclusive);
  }

/****************************************************************************
 *  IntGetModuleList
 *  Description:
 *  Input :
 *  Returns: A list of modules
 ***************************************************************************/
static char **IntGetModuleList()
  {
   void *theEnv = GetCurrentEnvironment();
   int maxItems = 20, itemCount = 0;
   char* name;
   static char **itemList = NULL;
   struct defmodule *theDefmodule = NULL;

   if ((theDefmodule = (struct defmodule *) EnvGetNextDefmodule(theEnv,NULL)) == NULL)
     { return(NULL); }

   itemList = (String *)calloc(maxItems,sizeof(String));
   while( theDefmodule != NULL)
     {
      name = EnvGetDefmoduleName(theEnv,theDefmodule);
      itemList[itemCount]  = balloc(strlen(name) + 1,char);
      strcpy(itemList[itemCount],name);
      itemCount++;
      if (itemCount == (maxItems - 1))
        {
         maxItems = 2*maxItems;
        itemList = (String *)realloc(itemList,maxItems * sizeof(String));
        }
      theDefmodule = (struct defmodule *)EnvGetNextDefmodule(theEnv,theDefmodule);
     }
   itemList[itemCount] = NULL;
   /*sortList(itemList,itemCount);*/
   return(itemList);
  }

/*******************************************************************************
          Name:        DoneSelectDefmoduleCallback
          Description: Called when Done is selected form module window
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
static void DoneSelectDefmoduleCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
{
    char** list = (char**)client_data;
    int i = 0;

    XtDestroyWidget(XtParent(XtParent(w)));
    if(list == NULL)
      return;
    while(list[i] != NULL)
     {
       free(list[i]);
       i++;
     }
    free(list);
}
/****************************************************************************
**
        Name:           DefmoduleSelectCallback
        Description:
        Arguments:
        Return:
*****************************************************************************
**/
static void DefmoduleSelectCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   
   XawListReturnStruct *current = XawListShowCurrent((Widget)client_data);

   if (current->list_index == XAW_LIST_NONE)
     { return; }
     
   EnvSetCurrentModule(theEnv,EnvFindDefmodule(theEnv,current->string));
 
   /*=====================================================*/
   /* Set this flag to True to get out of the event loop. */
   /*=====================================================*/

   quit_get_event = True; 
  }


/*******************************************************************************
          Name:        DefruleManagerCallback
          Description: Pop up the Rule Manager
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void DefruleManagerCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   static Widget defrulemanager = NULL;
   Widget defrulemanager_form,defrulemanager_viewport,
          removeb = NULL, refresh = NULL, matches = NULL, pprint = NULL, 
          break_point = NULL,break_point_label = NULL,watch_activation_label = NULL,
          watch_firing_label = NULL,
          watch_activation = NULL,watch_firing = NULL, cancel = NULL;
   static Widget CheckBoxes[3];
   int itemCount = 0;
   char buffer[MAX_CHAR_IN_BUF];
   
  /* ======================= */
  /*  Get the rule list      */
  /* ======================= */
 
  itemCount = IntGetDefruleLis();

  if(item_list == NULL)
    {
        ClearParameters();
        defrulemanager_flag = False;
        return;
     }

  defrulemanager_flag = True;
  
 /* ======================================== */
  /* Get the title for defrule manager window */
  /* ======================================== */
  sprintf(buffer,"Defrule Manager - %d Items",itemCount);


  /* ====================================================== */
  /*  Create the defrule manager window                     */
  /* ====================================================== */
 
  defrulemanager = XtCreatePopupShell(buffer,
                                      topLevelShellWidgetClass,
                                      toplevel,
                                      NULL, 0);

  defrulemanager_form = XtCreateManagedWidget("manager_form",
                                              formWidgetClass,
                                              defrulemanager,
                                              NULL, 0);

  XtSetArg(TheArgs[0],XtNallowHoriz,True);
  XtSetArg(TheArgs[1],XtNallowVert,True);
  defrulemanager_viewport = XtCreateManagedWidget("manager_viewport",
                                                  viewportWidgetClass,
                                                  defrulemanager_form,
                                                  TheArgs, 2);
  XtSetArg(TheArgs[0], XtNlist, item_list);
  manager_list = XtCreateManagedWidget("manager_list",
                                       listWidgetClass,
                                       defrulemanager_viewport,
                                       TheArgs, 1);

  XtSetArg(TheArgs[0], XtNfromHoriz, defrulemanager_viewport);

  /* ==================================================== */
  /*  Create the Remove button                            */
  /* ==================================================== */

  XtSetArg(TheArgs[1], XtNlabel, "Remove");
  removeb = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 defrulemanager_form,
                                 TheArgs, 2);
  XtAddCallback(removeb, XtNcallback, DefruleRemoveCallback, manager_list);

  /* ==================================================== */
  /*  Create the Refresh button                           */
  /* ==================================================== */

  XtSetArg(TheArgs[1], XtNfromVert, removeb);
  XtSetArg(TheArgs[2], XtNlabel, "Refresh");
  refresh = XtCreateManagedWidget("managerButton",
                                  commandWidgetClass,
                                  defrulemanager_form,
                                  TheArgs, 3);
  XtAddCallback(refresh, XtNcallback, DefruleRefreshCallback, manager_list);

  /* ==================================================== */
  /*  Create the Matches button                           */
  /* ==================================================== */

  XtSetArg(TheArgs[1], XtNfromVert, refresh);
  XtSetArg(TheArgs[2], XtNlabel, "Matches");
  matches = XtCreateManagedWidget("managerButton",
                                  commandWidgetClass,
                                  defrulemanager_form,
                                  TheArgs, 3);
  XtAddCallback(matches, XtNcallback, DefruleMatchesCallback, manager_list);

  /* ==================================================== */
  /*  Create the Pprint button                            */
  /* ==================================================== */

  XtSetArg(TheArgs[1], XtNfromVert, matches);
  XtSetArg(TheArgs[2], XtNlabel, "Pprint");
  pprint = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 defrulemanager_form,
                                 TheArgs, 3);
  XtAddCallback(pprint, XtNcallback, DefrulePprintCallback, manager_list);

  /* ==================================================== */
  /*  Create the BreakPoint button                        */
  /* ==================================================== */
 
  XtSetArg(TheArgs[1], XtNfromVert, pprint);
  XtSetArg(TheArgs[2], XtNlabel," ");
  CheckBoxes[0] = break_point = XtCreateManagedWidget("managerButton",
                                    toggleWidgetClass,
                                    defrulemanager_form,
                                    TheArgs, 3);
  XtAddCallback(break_point,XtNcallback,DefruleBreakPointCallback,manager_list);
  XtSetArg(TheArgs[0], XtNfromHoriz,break_point);
  XtSetArg(TheArgs[1], XtNfromVert, pprint);
  XtSetArg(TheArgs[2], XtNlabel,"Breakpoint");
  XtSetArg(TheArgs[3], XtNborderWidth,0);
  break_point_label = XtCreateManagedWidget("checkBoxLabel",
                                    labelWidgetClass,
                                    defrulemanager_form,
                                    TheArgs, 4);
  /* ==================================================== */
  /*  Create the Watch Activation button                  */
  /* ==================================================== */

  XtSetArg(TheArgs[0], XtNfromHoriz,defrulemanager_viewport);
  XtSetArg(TheArgs[1], XtNfromVert,break_point);
  XtSetArg(TheArgs[2], XtNlabel," ");
  CheckBoxes[1] = watch_activation = XtCreateManagedWidget("managerButton",
                                    toggleWidgetClass,
                                    defrulemanager_form,
                                    TheArgs, 3);
  XtAddCallback(watch_activation,XtNcallback,DefruleActivationCallback,manager_list);
  XtSetArg(TheArgs[0], XtNfromHoriz,watch_activation);
  XtSetArg(TheArgs[1], XtNfromVert, break_point);
  XtSetArg(TheArgs[2], XtNlabel,"Watch Activation");
  XtSetArg(TheArgs[3], XtNborderWidth,0);
  watch_activation_label = XtCreateManagedWidget("checkBoxLabel",
                                    labelWidgetClass,
                                    defrulemanager_form,
                                    TheArgs, 4);
  /* ==================================================== */
  /*  Create the Watch Firing button                            */
  /* ==================================================== */

  XtSetArg(TheArgs[0], XtNfromHoriz,defrulemanager_viewport);
  XtSetArg(TheArgs[1], XtNfromVert,watch_activation);
  XtSetArg(TheArgs[2], XtNlabel," ");

  CheckBoxes[2] = watch_firing = XtCreateManagedWidget("managerButton", 
                                    toggleWidgetClass,
                                    defrulemanager_form,
                                    TheArgs, 3); 
  XtAddCallback(watch_firing,XtNcallback,DefruleFiringsCallback,manager_list);
  XtSetArg(TheArgs[0], XtNfromHoriz,watch_firing);
  XtSetArg(TheArgs[1], XtNfromVert, watch_activation);
  XtSetArg(TheArgs[2], XtNlabel,"Watch Firing");
  XtSetArg(TheArgs[3], XtNborderWidth,0);
  watch_firing_label = XtCreateManagedWidget("checkBoxLabel",
                                    labelWidgetClass,
                                    defrulemanager_form,
                                    TheArgs, 4);
  XtSetArg(TheArgs[0], XtNfromHoriz,defrulemanager_viewport);
  XtSetArg(TheArgs[1], XtNfromVert, watch_firing);
  XtSetArg(TheArgs[2], XtNlabel, "Done");
  cancel = XtCreateManagedWidget("managerCancel",
                                 commandWidgetClass,
                                 defrulemanager_form,
                                 TheArgs, 3);
  XtAddCallback(cancel, XtNcallback, CancelSelectPrimary, defrulemanager);

  /* ====================================================== */
  /*  Add the callback function to the manager_list widget */
  /* ====================================================== */

  XtAddCallback(manager_list,XtNcallback,DefruleMngrCheckboxesCallback,CheckBoxes);
  XtPopup(defrulemanager, XtGrabNonexclusive);

  }

/*******************************************************************************
          Name:        DeffactManagerCallback
          Description: Pop up the Deffacts Manager
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void DeffactManagerCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
  Widget deffactsmanager,deffactsmanager_form, deffactsmanager_viewport,
         removeb, pprint, cancel;
  char buffer[MAX_CHAR_IN_BUF];
  int itemCount;

  itemCount = IntGetFactList();

  if(item_list == NULL)
    {
     ClearParameters();
     deffactsmanager_flag = False;
     return;
    }

  deffactsmanager_flag = True;

  /* ======================================== */
  /* Get the title for deffact manager window */
  /* ======================================== */
  sprintf(buffer,"Deffacts Manager - %d Items",itemCount);

  /* ==================================================== */
  /*  Create the Deffacts Manager window                  */ 
  /* ==================================================== */

  deffactsmanager = XtCreatePopupShell(buffer,
                                       topLevelShellWidgetClass,
                                       toplevel,
                                       NULL, 0);

  deffactsmanager_form = XtCreateManagedWidget("manager_form",
                                               formWidgetClass,
                                               deffactsmanager,
                                               NULL, 0);
  XtSetArg(TheArgs[0],XtNallowHoriz,True);
  XtSetArg(TheArgs[1],XtNallowVert,True);
  deffactsmanager_viewport = XtCreateManagedWidget("manager_viewport",
                                                         viewportWidgetClass,
                                                         deffactsmanager_form,
                                                         TheArgs, 2);

  XtSetArg(TheArgs[0], XtNlist, item_list);
  manager_list = XtCreateManagedWidget("manager_list",
                                        listWidgetClass,
                                        deffactsmanager_viewport,
                                        TheArgs, 1);
  /* ==================================================== */
  /*  Create the Remove button                            */
  /* ==================================================== */

  XtSetArg(TheArgs[0], XtNfromHoriz, deffactsmanager_viewport);
  XtSetArg(TheArgs[1], XtNlabel, "Remove");
  removeb = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 deffactsmanager_form,
                                 TheArgs, 2);
  XtAddCallback(removeb, XtNcallback, DeffactsRemove, manager_list);

  /* ==================================================== */
  /*  Create the Pprint button                            */
  /* ==================================================== */

  XtSetArg(TheArgs[1], XtNfromVert, removeb);
  XtSetArg(TheArgs[2], XtNlabel, "Pprint");
  pprint = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 deffactsmanager_form,
                                 TheArgs, 3);
  XtAddCallback(pprint, XtNcallback, DeffactsPprint, manager_list);

  /* ==================================================== */
  /*  Create the Done button                              */
  /* ==================================================== */

  XtSetArg(TheArgs[1], XtNfromVert, pprint);
  XtSetArg(TheArgs[2], XtNlabel, "Done");
  cancel = XtCreateManagedWidget("managerCancel",
                                 commandWidgetClass,
                                 deffactsmanager_form,
                                 TheArgs, 3);
  XtAddCallback(cancel, XtNcallback, CancelSelectPrimary, deffactsmanager);

  XtPopup(deffactsmanager, XtGrabNonexclusive);
 
  }

/*******************************************************************************
          Name:        DeftemplateManagerCallback
          Description: Pop up the Deftemplate Manager
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void DeftemplateManagerCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
  Widget deftemplatemanager, deftemplatemanager_form,
         deftemplatemanager_viewport, removeb, pprint,watch_label, cancel;
  static Widget watch = NULL;
  int itemCount = 0;
  char buffer[MAX_CHAR_IN_BUF];

  itemCount = IntGetDeftemplateList();

  if(item_list == NULL)
    {
    deftemplatemanager_flag = False;
    return;
    }

  deftemplatemanager_flag = True;

  /* ======================================== */
  /* Get the title for deffact manager window */
  /* ======================================== */
  sprintf(buffer,"Deftemplate Manager - %d Items",itemCount);

  /* ==================================================== */
  /*  Create the Deftemplate Mananaager window            */
  /* ==================================================== */

  deftemplatemanager = XtCreatePopupShell(buffer,
                                          topLevelShellWidgetClass,
                                          toplevel,
                                          NULL, 0);

  deftemplatemanager_form = XtCreateManagedWidget("manager_form",
                                                  formWidgetClass,
                                                  deftemplatemanager,
                                                  NULL, 0);

  XtSetArg(TheArgs[0],XtNallowHoriz,True);
  XtSetArg(TheArgs[1],XtNallowVert,True);
  deftemplatemanager_viewport = XtCreateManagedWidget("manager_viewport",
                                                      viewportWidgetClass,
                                                      deftemplatemanager_form,
                                                      TheArgs, 2);

  XtSetArg(TheArgs[0], XtNlist, item_list);
  manager_list = XtCreateManagedWidget("manager_list",
                                       listWidgetClass,
                                       deftemplatemanager_viewport,
                                       TheArgs, 1);
  /* ==================================================== */
  /*  Create the Remove button                            */
  /* ==================================================== */

  XtSetArg(TheArgs[0], XtNfromHoriz, deftemplatemanager_viewport);
  XtSetArg(TheArgs[1], XtNlabel, "Remove");
  removeb = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 deftemplatemanager_form,
                                 TheArgs, 2);
  XtAddCallback(removeb, XtNcallback, DeftemplateRemove,
                manager_list);

  /* ==================================================== */
  /*  Create the Pprint button                            */
  /* ==================================================== */

  XtSetArg(TheArgs[1], XtNfromVert, removeb);
  XtSetArg(TheArgs[2], XtNlabel, "Pprint");
  pprint = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 deftemplatemanager_form,
                                 TheArgs, 3);
  XtAddCallback(pprint, XtNcallback, DeftemplatePprint,
                manager_list);

  /* ==================================================== */
  /*  Create the Watch button                             */
  /*===================================================g */

  XtSetArg(TheArgs[1], XtNfromVert, pprint);
  XtSetArg(TheArgs[2], XtNlabel," ");
  watch = XtCreateManagedWidget("managerButton",
                                 toggleWidgetClass,
                                 deftemplatemanager_form,
                                 TheArgs, 3); 
  XtAddCallback(watch,XtNcallback,DeftemplateWatchCallback,manager_list);
  XtAddCallback(manager_list,XtNcallback,DeftemplateMngrCheckboxCallback,watch);
  XtSetArg(TheArgs[0],XtNfromHoriz,watch);
  XtSetArg(TheArgs[1],XtNfromVert,pprint);
  XtSetArg(TheArgs[2],XtNlabel,"Watch");
  XtSetArg(TheArgs[3],XtNborderWidth,0);
  watch_label = XtCreateManagedWidget("checkBoxLabel",
                                 labelWidgetClass,
                                 deftemplatemanager_form,
                                 TheArgs,4);
  /* ==================================================== */
  /*  Create the Done button                              */
  /* ==================================================== */

  XtSetArg(TheArgs[0],XtNfromHoriz,deftemplatemanager_viewport);
  XtSetArg(TheArgs[1], XtNfromVert, watch);
  XtSetArg(TheArgs[2], XtNlabel, "Done");
  cancel = XtCreateManagedWidget("managerCancel",
                                 commandWidgetClass,
                                 deftemplatemanager_form,
                                 TheArgs, 3);
  XtAddCallback(cancel,XtNcallback,CancelSelectPrimary, deftemplatemanager);

  XtPopup(deftemplatemanager, XtGrabNonexclusive);
  }

/*******************************************************************************
          Name:        DeffunctionManagerCallback
          Description: Pops up the Deffunction Manager
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void DeffunctionManagerCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
  Widget deffunctionmanager, deffunctionmanager_form,
         deffunctionmanager_viewport,watch_label, removeb, pprint, cancel;

  static Widget watch;
  int itemCount = 0;
  char buffer[MAX_CHAR_IN_BUF];

  itemCount = IntGetDeffunctionList();

  if(item_list == NULL)
    {
      ClearParameters();
      deffunctionmanager_flag = False;
      return;
    }

  deffunctionmanager_flag = True;
 
 /* ============================================= */
  /* Get the title for deffunction manager window */
  /* ============================================ */
  sprintf(buffer,"Deffunction Manager -  %d Items",itemCount);

  /* ==================================================== */
  /*  Create the Deffunction Manager window               */
  /* ==================================================== */

  deffunctionmanager = XtCreatePopupShell(buffer,
                                          topLevelShellWidgetClass,
                                          toplevel,
                                          NULL, 0);

  deffunctionmanager_form = XtCreateManagedWidget("manager_form",
                                                  formWidgetClass,
                                                  deffunctionmanager,
                                                  NULL, 0);
  XtSetArg(TheArgs[0],XtNallowHoriz,True);
  XtSetArg(TheArgs[1],XtNallowVert,True);
  deffunctionmanager_viewport = XtCreateManagedWidget("manager_viewport",
                                                      viewportWidgetClass,
                                                      deffunctionmanager_form,
                                                      TheArgs, 2);

  XtSetArg(TheArgs[0], XtNlist, item_list);
  manager_list = XtCreateManagedWidget("manager_list",
                                       listWidgetClass,
                                       deffunctionmanager_viewport,
                                       TheArgs, 1);
  /* ==================================================== */
  /*  Create the Remove button                            */
  /* ==================================================== */

  XtSetArg(TheArgs[0], XtNfromHoriz, deffunctionmanager_viewport);
  XtSetArg(TheArgs[1], XtNlabel, "Remove");
  removeb = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 deffunctionmanager_form,
                                 TheArgs, 2);
  XtAddCallback(removeb, XtNcallback, DeffunctionRemoveCallback,
                manager_list);
  /* ==================================================== */
  /*  Create the Pprint button                            */
  /* ==================================================== */

  XtSetArg(TheArgs[1], XtNfromVert, removeb);
  XtSetArg(TheArgs[2], XtNlabel, "Pprint");
  pprint = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 deffunctionmanager_form,
                                 TheArgs, 3);
  XtAddCallback(pprint, XtNcallback, DeffunctionPprintCallback,
                manager_list);

  /* ==================================================== */
  /*  Create the Watch button                            */
  /* ==================================================== */
  
  XtSetArg(TheArgs[1], XtNlabel, " ");
  XtSetArg(TheArgs[2],XtNfromVert,pprint);
  watch = XtCreateManagedWidget("managerButton",
                                toggleWidgetClass,
                                deffunctionmanager_form,
                                TheArgs,3);
  XtAddCallback(watch,XtNcallback,DeffunctionWatchCallback,(XtPointer)manager_list);
  XtAddCallback(manager_list,XtNcallback,DeffunctionMngrCheckboxCallback,(XtPointer)watch);
  XtSetArg(TheArgs[0], XtNfromHoriz,watch);
  XtSetArg(TheArgs[1], XtNlabel, "Watch");
  watch_label = XtCreateManagedWidget("checkBoxLabel",
                                labelWidgetClass,
                                deffunctionmanager_form,
                                TheArgs,3);

  /* ==================================================== */
  /*  Create the Cancel button                            */
  /* ==================================================== */

  XtSetArg(TheArgs[0], XtNfromHoriz,deffunctionmanager_viewport);
  XtSetArg(TheArgs[1], XtNfromVert, watch);
  XtSetArg(TheArgs[2], XtNlabel, "Done");
  cancel = XtCreateManagedWidget("managerCancel",
                                 commandWidgetClass,
                                 deffunctionmanager_form,
                                 TheArgs, 3);
  XtAddCallback(cancel, XtNcallback, CancelSelectPrimary,
                (XtPointer)deffunctionmanager);

  XtPopup(deffunctionmanager, XtGrabNonexclusive);
  }

/*******************************************************************************
          Name:        DefglobalManagerCallback
          Description: Pops up the Defglobal Manager
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void DefglobalManagerCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
{
   Widget defglobalmanager, defglobalmanager_form,
         defglobalmanager_viewport, removeb, pprint, cancel;
   char buffer[MAX_CHAR_IN_BUF];
   int itemCount = 0;

  itemCount = IntGetDefglobalList();

  if(item_list == NULL)
    {
      ClearParameters();
      defglobalmanager_flag = False;
      return;
    }

  defglobalmanager_flag = True;
  /* ========================================== */
  /* Get the title for defglobal manager window */
  /* ========================================== */

  sprintf(buffer,"Defglobal Manager - %d Items",itemCount);

  /* ==================================================== */
  /*  Create the Defglobal Manager window                 */
  /* ==================================================== */

  defglobalmanager = XtCreatePopupShell(buffer,
                                          topLevelShellWidgetClass,
                                          toplevel,
                                          NULL, 0);

  defglobalmanager_form = XtCreateManagedWidget("manager_form",
                                                  formWidgetClass,
                                                  defglobalmanager,
                                                  NULL, 0);
  XtSetArg(TheArgs[0],XtNallowHoriz,True);
  XtSetArg(TheArgs[1],XtNallowVert,True);
  defglobalmanager_viewport = XtCreateManagedWidget("manager_viewport",
                                                      viewportWidgetClass,
                                                      defglobalmanager_form,
                                                      TheArgs, 2);

  XtSetArg(TheArgs[0], XtNlist, item_list);
  manager_list = XtCreateManagedWidget("manager_list",
                                       listWidgetClass,
                                       defglobalmanager_viewport,
                                       TheArgs, 1);
  /* ==================================================== */
  /*  Create the Remove button                            */
  /* ==================================================== */

  XtSetArg(TheArgs[0], XtNfromHoriz, defglobalmanager_viewport);
  XtSetArg(TheArgs[1], XtNlabel, "Remove");
  removeb = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 defglobalmanager_form,
                                 TheArgs, 2);
  XtAddCallback(removeb, XtNcallback, DefglobalRemoveCallback,
                (XtPointer)manager_list);
  /* ==================================================== */
  /*  Create the Pprint button                            */
  /* ==================================================== */

  XtSetArg(TheArgs[1], XtNfromVert, removeb);
  XtSetArg(TheArgs[2], XtNlabel, "Pprint");
  pprint = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 defglobalmanager_form,
                                 TheArgs, 3);
  XtAddCallback(pprint, XtNcallback,DefglobalPprintCallback,
                (XtPointer)manager_list);
  /* ==================================================== */
  /*  Create the Cancel button                            */
  /* ==================================================== */

  XtSetArg(TheArgs[1], XtNfromVert, pprint);
  XtSetArg(TheArgs[2], XtNlabel, "Done");
  cancel = XtCreateManagedWidget("managerCancel",
                                 commandWidgetClass,
                                 defglobalmanager_form,
                                 TheArgs, 3);
  XtAddCallback(cancel, XtNcallback, CancelSelectPrimary,
                (XtPointer)defglobalmanager);

  XtPopup(defglobalmanager, XtGrabNonexclusive);


  }

/*******************************************************************************
          Name:        IntGetDefglobal
          Description: 
          Arguments:  
          Returns:     
*******************************************************************************/

int IntGetDefglobalList()
  {
   void *theEnv = GetCurrentEnvironment();
   struct defglobal* defglPtr = NULL;
   int itemCount = 0,maxItems = 20;
   char *name;

   if ((defglPtr = (struct defglobal*) EnvGetNextDefglobal(theEnv,NULL)) == NULL)
     {
      item_list = NULL;
      return(0);
     }
   
   item_list = (String*)calloc(maxItems,sizeof(String));
   while(defglPtr != NULL)
     {
      name = (char*) EnvGetDefglobalName(theEnv,(struct constructHeader *) defglPtr);
      item_list[itemCount] = balloc(strlen(name) + 1,char);
      strcpy(item_list[itemCount++],name);
      if (itemCount == (maxItems - 1))
        {
         maxItems = maxItems * 2;
         item_list = (String *)realloc(item_list,maxItems * sizeof(String));
        }
      defglPtr = (struct defglobal*) EnvGetNextDefglobal(theEnv,defglPtr);
     }
     
   item_list[itemCount] = NULL;
   sortList(item_list,itemCount);
   return(itemCount);
  }

/*******************************************************************************
          Name:        DefglobalRemoveCallback
          Description: 
          Arguments:  
          Returns:     
*******************************************************************************/
static void DefglobalRemoveCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
     Widget list_widget = (Widget)client_data;
     XawListReturnStruct *current = XawListShowCurrent(list_widget);


     if(current->list_index == XAW_LIST_NONE)
      return;
     MoveEndOfFile(dialog_text, &TheEvent);
     SetCommandString(theEnv,"(undefglobal ");
     EnvPrintRouter(theEnv,"wclips","(undefglobal ");
     AppendCommandString(theEnv,current->string);
     EnvPrintRouter(theEnv,"wclips",current->string);
     AppendCommandString(theEnv,")\n");
     EnvPrintRouter(theEnv,"wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
     quit_get_event = True;
     list_change = True;

  }


/*******************************************************************************
          Name:       DefglobalPprintCallback 
          Description: 
          Arguments:  
          Returns:     
*******************************************************************************/
static  void DefglobalPprintCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);

  if(current->list_index == XAW_LIST_NONE ||
     EnvGetDefglobalPPForm(theEnv,(struct constructHeader *) EnvFindDefglobal(theEnv,current->string)) == NULL)
      return;
  MoveEndOfFile(dialog_text, &TheEvent);
  SetCommandString(theEnv,"(ppdefglobal ");
  EnvPrintRouter(theEnv,"wclips","(ppdefglobal ");
  AppendCommandString(theEnv,current->string);
  EnvPrintRouter(theEnv,"wclips",current->string);
  AppendCommandString(theEnv,")\n");
  EnvPrintRouter(theEnv,"wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;

  }

/*******************************************************************************
          Name:        DefgenericManagerCallback
          Description: Pops up the Defgeneric Manager
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void DefgenericManagerCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
  Widget defgenericmanager, defgenericmanager_form, defgenericmanager_viewport,
          watch, watch_label,removeb, pprint, methods, cancel;
  char buffer[MAX_CHAR_IN_BUF];
  int itemCount = 0;

  itemCount = IntGetDefgenericList();

  if(item_list == NULL)
    {
    ClearParameters();
    defgenericmanager_flag = False;
    release(curr_def_name);
    return;
    }

  defgenericmanager_flag = True;

  /* =========================================== */
  /* Get the title for defgeneric manager window */
  /* =========================================== */
  sprintf(buffer,"Defgeneric Manager - %d Items",itemCount);

  /* ==================================================== */
  /*  Create the Defgeneric Manager                       */
  /* ==================================================== */

  defgenericmanager = XtCreatePopupShell(buffer,
                                         topLevelShellWidgetClass,
                                         toplevel,
                                         NULL, 0);

  defgenericmanager_form = XtCreateManagedWidget("manager_form",
                                                 formWidgetClass,
                                                 defgenericmanager,
                                                 NULL, 0);
  XtSetArg(TheArgs[0],XtNallowHoriz,True);
  XtSetArg(TheArgs[1],XtNallowVert,True);
  defgenericmanager_viewport = XtCreateManagedWidget("manager_viewport",
                                                     viewportWidgetClass,
                                                     defgenericmanager_form,
                                                     TheArgs, 2);

  XtSetArg(TheArgs[0], XtNlist, item_list);
  manager_list = XtCreateManagedWidget("manager_list",
                                       listWidgetClass,
                                       defgenericmanager_viewport, 
                                       TheArgs, 1);
  /* ==================================================== */
  /*  Create the Remove button                            */
  /* ==================================================== */

  XtSetArg(TheArgs[0], XtNfromHoriz, defgenericmanager_viewport);
  XtSetArg(TheArgs[1], XtNlabel, "Remove");
  removeb = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 defgenericmanager_form,
                                 TheArgs, 2);
  XtAddCallback(removeb, XtNcallback, DefgenericRemoveCallback, (XtPointer)manager_list);

  /* ==================================================== */
  /*  Create the Pprint button                            */
  /* ==================================================== */

  XtSetArg(TheArgs[1], XtNfromVert, removeb);
  XtSetArg(TheArgs[2], XtNlabel, "Pprint");
  pprint = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 defgenericmanager_form,
                                 TheArgs, 3);
  XtAddCallback(pprint, XtNcallback, DefgenericPprintCallback, (XtPointer)manager_list);

  /* ==================================================== */
  /*  Create the Methods button                           */
  /* ==================================================== */

  XtSetArg(TheArgs[1], XtNfromVert, pprint);
  XtSetArg(TheArgs[2], XtNlabel, "Methods...");
  methods = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 defgenericmanager_form,
                                 TheArgs, 3);
  XtAddCallback(methods,XtNcallback,DefgenericMethodCallback, (XtPointer)manager_list);

  /* ==================================================== */
  /*  Create the Watch button                            */
  /* ==================================================== */
  XtSetArg(TheArgs[1], XtNlabel, " ");
  XtSetArg(TheArgs[2],XtNfromVert,methods);
  watch = XtCreateManagedWidget("managerButton",
                                toggleWidgetClass,
                                defgenericmanager_form,
                                TheArgs,3);
  XtAddCallback(watch,XtNcallback,DefgenericWatchCallback,(XtPointer)manager_list);
  XtAddCallback(manager_list,XtNcallback,DefgenericMngrCheckBoxCallback,(XtPointer)watch);
  XtSetArg(TheArgs[0], XtNfromHoriz,watch);
  XtSetArg(TheArgs[1], XtNlabel, "Watch");
  watch_label = XtCreateManagedWidget("checkBoxLabel",
                                labelWidgetClass,
                                defgenericmanager_form,
                                TheArgs,3);

  /* ==================================================== */
  /*  Create the Done button                              */
  /* ==================================================== */
  XtSetArg(TheArgs[0], XtNfromHoriz,defgenericmanager_viewport);
  XtSetArg(TheArgs[1], XtNfromVert, watch);
  XtSetArg(TheArgs[2], XtNlabel, "Done");
  cancel = XtCreateManagedWidget("managerCancel",
                                 commandWidgetClass,
                                 defgenericmanager_form,
                                 TheArgs, 3);
  XtAddCallback(cancel, XtNcallback, CancelSelectPrimary,(XtPointer)defgenericmanager);

  XtPopup(defgenericmanager, XtGrabNonexclusive);
  }


/*******************************************************************************
          Name:        DefinstancesManagerCallback
          Description: Pops up the Definstances Manager
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void DefinstancesManagerCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
  Widget definstancesmanager, definstancesmanager_form,
         definstancesmanager_viewport, removeb, pprint, cancel;
  char buffer[MAX_CHAR_IN_BUF];
  int itemCount = 0;

  itemCount = IntGetDefinstancesList();

  if(item_list == NULL)
    {
    ClearParameters();
    definstancesmanager_flag = False;
    return;
    }

  definstancesmanager_flag = True;
  /* =========================================== */
  /* Get the title for definstance manager window */
  /* =========================================== */
  sprintf(buffer,"Definstance Manager - %d Items",itemCount);

  /* ==================================================== */
  /*  Create the Definstances Manager window              */
  /* ==================================================== */

  definstancesmanager = XtCreatePopupShell(buffer,
                                           topLevelShellWidgetClass,
                                           toplevel,
                                           NULL, 0);

  definstancesmanager_form = XtCreateManagedWidget("manager_form",
                                                   formWidgetClass,
                                                   definstancesmanager,
                                                   NULL, 0);

  XtSetArg(TheArgs[0],XtNallowHoriz,True);
  XtSetArg(TheArgs[1],XtNallowVert,True);
  definstancesmanager_viewport = XtCreateManagedWidget("manager_viewport",
                                                       viewportWidgetClass,
                                                       definstancesmanager_form,
                                                       TheArgs, 2);

  XtSetArg(TheArgs[0], XtNlist, item_list);
  manager_list = XtCreateManagedWidget("manager_list",
                                       listWidgetClass,
                                       definstancesmanager_viewport,
                                       TheArgs, 1);
  XtSetArg(TheArgs[0], XtNfromHoriz, definstancesmanager_viewport);
  /* ==================================================== */
  /*  Create the Remove button                            */
  /* ==================================================== */

  XtSetArg(TheArgs[1], XtNlabel, "Remove");
  removeb = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 definstancesmanager_form,
                                 TheArgs, 2);
  XtAddCallback(removeb, XtNcallback, DefinstancesRemoveCallback,
                (XtPointer)manager_list);

  /* ==================================================== */
  /*  Create the Pprint button                            */
  /* ==================================================== */

  XtSetArg(TheArgs[1], XtNfromVert, removeb);
  XtSetArg(TheArgs[2], XtNlabel, "Pprint");
  pprint = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 definstancesmanager_form,
                                 TheArgs, 3);
  XtAddCallback(pprint, XtNcallback, DefinstancesPprintCallback,
                (XtPointer)manager_list);

  /* ==================================================== */
  /*  Create the Done button                              */
  /* ==================================================== */

  XtSetArg(TheArgs[1], XtNfromVert, pprint);
  XtSetArg(TheArgs[2], XtNlabel, "Done");
  cancel = XtCreateManagedWidget("managerCancel",
                                 commandWidgetClass,
                                 definstancesmanager_form,
                                 TheArgs, 3);
  XtAddCallback(cancel, XtNcallback, CancelSelectPrimary,
                (XtPointer)definstancesmanager);

  XtPopup(definstancesmanager, XtGrabNonexclusive);
  }

/*******************************************************************************
          Name:        DefclassManagerCallback
          Description: Pops up the Defclass Manager
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void DefclassManagerCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
  Widget defclassmanager, defclassmanager_form, defclassmanager_viewport,
         removeb,  describe, browse, pprint, message_handlers, watch_instances,
         watch_slot, watch_instances_label,watch_slot_label,cancel;
  static Widget CheckBoxes[3];
  char buffer[MAX_CHAR_IN_BUF];
  int itemCount = 0;
 
  itemCount = IntGetDefclassList();

  if(item_list == NULL)
    {
    ClearParameters();
    release(curr_def_name);
    defclassmanager_flag = False;
    return;
    }
  
  defclassmanager_flag = True;
  /* =========================================== */
  /* Get the title for defclass manager window   */
  /* =========================================== */
  sprintf(buffer,"Defclass Manager - %d Items",itemCount);

  /* =========================================== */
  /* Create the parent window for the defclass   */
  /* window manager                              */
  /* =========================================== */

  defclassmanager = XtCreatePopupShell(buffer,
                                       topLevelShellWidgetClass,
                                       toplevel,
                                       NULL, 0);

  defclassmanager_form = XtCreateManagedWidget("manager_form",
                                               formWidgetClass,
                                               defclassmanager,
                                               NULL, 0);

  /* =========================================== */
  /* Create the list widget for the defclass     */
  /* list                                        */
  /* =========================================== */

  XtSetArg(TheArgs[0],XtNallowHoriz,True);
  XtSetArg(TheArgs[1],XtNallowVert,True);
  defclassmanager_viewport = XtCreateManagedWidget("manager_viewport",
                                                   viewportWidgetClass,
                                                   defclassmanager_form,
                                                   TheArgs, 2);

  XtSetArg(TheArgs[0], XtNlist, item_list);
  manager_list = XtCreateManagedWidget("manager_list",
                                       listWidgetClass,
                                       defclassmanager_viewport,
                                       TheArgs, 1);

  /* =========================================== */
  /* Create the Remove button                    */
  /* =========================================== */

  XtSetArg(TheArgs[0], XtNfromHoriz, defclassmanager_viewport);
  XtSetArg(TheArgs[1], XtNlabel, "Remove");
  removeb = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 defclassmanager_form,
                                 TheArgs, 2);
  XtAddCallback(removeb, XtNcallback, DefclassRemoveCallback, (XtPointer)manager_list);

  /* =========================================== */
  /* Create the Describe button                  */
  /* =========================================== */

  XtSetArg(TheArgs[1], XtNfromVert, removeb);
  XtSetArg(TheArgs[2], XtNlabel, "Describe");
  describe = XtCreateManagedWidget("managerButton",
                                   commandWidgetClass,
                                   defclassmanager_form,
                                   TheArgs, 3);
  XtAddCallback(describe, XtNcallback, DefclassDescribeCallback, (XtPointer)manager_list);

  /* =========================================== */
  /* Create the Browse button                    */
  /* =========================================== */

  XtSetArg(TheArgs[1], XtNfromVert, describe);
  XtSetArg(TheArgs[2], XtNlabel, "Browse");
  browse  = XtCreateManagedWidget("managerButton",
                                   commandWidgetClass,
                                   defclassmanager_form,
                                   TheArgs, 3);
  XtAddCallback(browse, XtNcallback, DefclassBrowseCallback, (XtPointer)manager_list);

  /* =========================================== */
  /* Create the Pretty Print button              */
  /* =========================================== */

  XtSetArg(TheArgs[1], XtNfromVert, browse);
  XtSetArg(TheArgs[2], XtNlabel, "Pprint");
  pprint = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 defclassmanager_form,
                                 TheArgs, 3);
  XtAddCallback(pprint, XtNcallback, DefclassPprintCallback, (XtPointer)manager_list);

  /* =========================================== */
  /* Create the Message Handler button           */
  /* =========================================== */

  XtSetArg(TheArgs[1], XtNfromVert, pprint);
  XtSetArg(TheArgs[2], XtNlabel, "Message Handlers...");
  message_handlers = XtCreateManagedWidget("managerButton",
                                           commandWidgetClass,
                                           defclassmanager_form,
                                           TheArgs, 3);
  XtAddCallback(message_handlers,XtNcallback, DefclassMessageHandlersCallback,(XtPointer)manager_list);
  /* ======================================================= */
  /* Create the toggle button for the Watch Instance button  */
  /* ======================================================= */

  XtSetArg(TheArgs[1], XtNfromVert, message_handlers);
  XtSetArg(TheArgs[2], XtNlabel," ");
  CheckBoxes[0] = watch_instances = XtCreateManagedWidget("managerButton",
                                           toggleWidgetClass,
                                           defclassmanager_form,
                                           TheArgs, 3);
  XtAddCallback(watch_instances,XtNcallback,WatchInstancesCallback,(XtPointer)manager_list);
  /* ======================================================= */
  /* Create the label button for the Watch Instance button  */
  /* ======================================================= */

  XtSetArg(TheArgs[1], XtNfromVert, message_handlers);
  XtSetArg(TheArgs[2], XtNlabel,"Watch Instances");
  XtSetArg(TheArgs[0],XtNfromHoriz,watch_instances);
  XtSetArg(TheArgs[3],XtNborderWidth,0);
  watch_instances_label = XtCreateManagedWidget("checkBoxLabel",
                                           labelWidgetClass,
                                           defclassmanager_form,
                                           TheArgs, 4);
  /* ======================================================= */
  /* Create the toggle button for the Watch Slot button      */
  /* ======================================================= */

  XtSetArg(TheArgs[0],XtNfromHoriz,defclassmanager_viewport);
  XtSetArg(TheArgs[1], XtNfromVert,watch_instances);
  XtSetArg(TheArgs[2], XtNlabel," ");
  CheckBoxes[1] = watch_slot =  XtCreateManagedWidget("managerButton",
                                           toggleWidgetClass,
                                           defclassmanager_form,
                                           TheArgs, 3);
  XtAddCallback(watch_slot,XtNcallback,WatchSlotCallback,(XtPointer)manager_list);
  /* ======================================================= */
  /* Create the label button for the Watch Slot              */
  /* ======================================================= */

  XtSetArg(TheArgs[0],XtNfromHoriz,watch_slot);
  XtSetArg(TheArgs[1], XtNfromVert,watch_instances);
  XtSetArg(TheArgs[2], XtNlabel,"Watch Slots");
  XtSetArg(TheArgs[3], XtNborderWidth,0);
  watch_slot_label =  XtCreateManagedWidget("checkBoxLabel",
                                           labelWidgetClass,
                                           defclassmanager_form,
                                           TheArgs,4);
  /* ======================================================= */
  /* Create the command button for the Done                  */
  /* ======================================================= */

  XtSetArg(TheArgs[0],XtNfromHoriz,defclassmanager_viewport);
  XtSetArg(TheArgs[1], XtNfromVert, watch_slot);
  XtSetArg(TheArgs[2], XtNlabel, "Done");
  cancel = XtCreateManagedWidget("managerCancel",
                                 commandWidgetClass,
                                 defclassmanager_form,
                                 TheArgs, 3);
  XtAddCallback(cancel, XtNcallback, CancelSelectPrimary, (XtPointer)defclassmanager);

  /* ======================================================= */
  /* Add the callback function to the manager list           */
  /* ======================================================= */

  XtAddCallback(manager_list,XtNcallback,DefclssMngrChckbxCallback,(XtPointer)CheckBoxes);

  XtPopup(defclassmanager, XtGrabNonexclusive);
  }


/*******************************************************************************
          Name:        AgendaManagerCallback
          Description: Pop up the Agenda Manager
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void AgendaManagerCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
  Widget agendamanager, agendamanager_form, agendamanager_viewport, removeb,
         fire, cancel;
  char buffer[MAX_CHAR_IN_BUF];

  int itemCount = IntGetAgendaList();

  if(item_list == NULL)
    {
    ClearParameters();
    agendamanager_flag = False;
    return;
    }

  agendamanager_flag = True;
  /* =========================================== */
  /* Get the title for agenda manager window */
  /* =========================================== */
  sprintf(buffer,"Agenda Manager - %d Items",itemCount);

  /* ==================================================== */
  /*  Create the Agenda Manager window                    */
  /* ==================================================== */

  agendamanager = XtCreatePopupShell(buffer,
                                     topLevelShellWidgetClass,
                                     toplevel,
                                     NULL, 0);

  agendamanager_form = XtCreateManagedWidget("manager_form",
                                             formWidgetClass,
                                             agendamanager,
                                             NULL, 0);

  XtSetArg(TheArgs[0],XtNallowHoriz,True);
  XtSetArg(TheArgs[1],XtNallowVert,True);
  agendamanager_viewport = XtCreateManagedWidget("manager_viewport",
                                                 viewportWidgetClass,
                                                 agendamanager_form,
                                                 TheArgs, 2);

  XtSetArg(TheArgs[0], XtNlist, item_list);
  manager_list = XtCreateManagedWidget("manager_list",
                                       listWidgetClass,
                                       agendamanager_viewport,
                                       TheArgs, 1);

  /* ==================================================== */
  /*  Create the Remove button                            */
  /* ==================================================== */

  XtSetArg(TheArgs[0], XtNfromHoriz, agendamanager_viewport);
  XtSetArg(TheArgs[1], XtNlabel, "Remove");
  removeb = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 agendamanager_form,
                                 TheArgs, 2);
  XtAddCallback(removeb, XtNcallback, AgendaRemove, (XtPointer)manager_list);

  /* ==================================================== */
  /*  Create the Fire button                              */
  /* ==================================================== */

  XtSetArg(TheArgs[1], XtNfromVert, removeb);
  XtSetArg(TheArgs[2], XtNlabel, "Fire");
  fire = XtCreateManagedWidget("managerButton",
                               commandWidgetClass,
                               agendamanager_form,
                               TheArgs, 3);
  XtAddCallback(fire, XtNcallback, AgendaFire,(XtPointer)manager_list);

  /* ==================================================== */
  /*  Create the Done button                              */
  /* ==================================================== */

  XtSetArg(TheArgs[1], XtNfromVert, fire);
  XtSetArg(TheArgs[2], XtNlabel, "Done");
  cancel = XtCreateManagedWidget("managerCancel",
                                 commandWidgetClass,
                                 agendamanager_form,
                                 TheArgs, 3);
  XtAddCallback(cancel, XtNcallback, CancelSelectPrimary, (XtPointer) agendamanager);

  XtPopup(agendamanager,  XtGrabNonexclusive);
  }

/*******************************************************************************
          Name:        IntGetAgendaList
          Description: Gets agenda list
          Arguments:  None
          Returns:
*******************************************************************************/
int IntGetAgendaList()
  {
   void *theEnv = GetCurrentEnvironment();
  int maxItems = 20,itemCount = 0;
  struct activation *act_ptr;
  char buffer[MAX_CHAR_IN_BUF];

  if((act_ptr = (struct activation *) EnvGetNextActivation(theEnv,NULL)) == NULL)
    {
      item_list = NULL;
      return(0);
    }
  item_list = (String *)calloc(maxItems,sizeof(String));
  while(act_ptr != NULL)
      {
      /*name = GetActivationName((void*)act_ptr);*/
      EnvGetActivationPPForm(theEnv,buffer,MAX_CHAR_IN_BUF - 1,act_ptr);
      item_list[itemCount] = balloc(strlen(buffer) + 1, char);
      strcpy(item_list[itemCount++],buffer);
      if(itemCount == (maxItems - 1))
        {
          maxItems = maxItems * 2;
          item_list = (String *)realloc(item_list,maxItems*sizeof(String));
        }
      act_ptr = (struct activation *) EnvGetNextActivation(theEnv,act_ptr);
      }
  item_list[itemCount] = NULL;
  /*sortList(item_list,itemCount);*/
  return(itemCount);
  }

/******************************************************************************
          Name:        AgendaRemove
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void AgendaRemove(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   register int i;
   struct activation *act_ptr;
   Widget list_widget = (Widget)client_data;
   XawListReturnStruct *current = XawListShowCurrent(list_widget);

   if (current->list_index == XAW_LIST_NONE)
     { return; }
     
   act_ptr = (struct activation *) EnvGetNextActivation(theEnv,NULL);
   for (i = 0; i < current->list_index ; i++)
     { act_ptr = (struct activation *) EnvGetNextActivation(theEnv,act_ptr); }
     
   EnvDeleteActivation(theEnv, (void *) act_ptr);

   /*=====================================================*/
   /* Set this flag to True to get out of the event loop. */
   /*=====================================================*/

   quit_get_event = True; 
   list_change = True;
  }

/******************************************************************************
          Name:        AgendaFire
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void AgendaFire(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
  register int i;
  struct activation *act_ptr;
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  act_ptr = (struct activation *) EnvGetNextActivation(theEnv,NULL);
  for (i = 0; i < current->list_index ; i++)
    act_ptr = (struct activation *) EnvGetNextActivation(theEnv,act_ptr);
  MoveActivationToTop(theEnv,(void*)act_ptr);
  MoveEndOfFile(dialog_text, &TheEvent);
  SetCommandString(theEnv,"(run 1)\n");
  EnvPrintRouter(theEnv,"wclips","(run 1)\n");

  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */

  quit_get_event = True;
  list_change = True;
  }


/*******************************************************************************
          Name:        FactsWindowCallback
          Description: Called when Facts Window is selected from the Window menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void FactsWindowCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   
   if (Browse_status[FACT_WIN])
     {
      XtSetArg(TheArgs[0], XtNleftBitmap, None);
      XtPopdown(facts);
      }
    else if (facts_text != NULL)
      {
       XtPopup(facts,XtGrabNone);
       EnvSetFactListChanged(theEnv,FALSE);
       PrintChangedFacts();
       XtSetArg(TheArgs[0], XtNleftBitmap, checker);
      }
    else
      {
       CreateFactWindow();
       EnvSetFactListChanged(theEnv,FALSE);
       PrintChangedFacts();
       XtSetArg(TheArgs[0], XtNleftBitmap, checker);
      }

   XtSetValues(facts_window, TheArgs, 1);
   Browse_status[FACT_WIN] = !Browse_status[FACT_WIN];
  }

/**********************************************************************
 *    CreateFactWindow
 *
 **********************************************************************/

void CreateFactWindow()
{
   void *theEnv = GetCurrentEnvironment();
    Dimension height;
    int n = 0;
    char *name,labelBuffer[256];
    struct defmodule* theModule = (struct defmodule *) EnvGetCurrentModule(theEnv);


 /* Change the name of the window to the current module */

    if(theModule  != NULL)
     {
       name = EnvGetDefmoduleName(theEnv,theModule);
       strcpy(labelBuffer,"Facts Window(");
       strcat(labelBuffer,name);
       strcat(labelBuffer,")");
     }
     else
     {
       strcpy(labelBuffer,"Facts Window");
     }

    XtSetArg(TheArgs[n], XtNheight, &height);n++;
    XtGetValues(dialog, TheArgs, n);
    height = (height + 150) / 3;
    n = 0;
    XtSetArg(TheArgs[n], XtNheight, height);n++;
    facts = XtCreatePopupShell(labelBuffer,
                               topLevelShellWidgetClass,
                               toplevel,
                               TheArgs, n);
    n = 0;
    XtSetArg(TheArgs[n], XtNdefaultDistance, 0);n++;
    facts_form = XtCreateManagedWidget("facts_form",
                                       formWidgetClass,
                                       facts,
                                       TheArgs, n);
    n = 0;
    XtSetArg(TheArgs[n], XtNwidth, 250);n++;
    XtSetArg(TheArgs[n], XtNeditType, XawtextAppend);n++;
    XtSetArg(TheArgs[n], XtNscrollHorizontal, XawtextScrollAlways);n++;
    XtSetArg(TheArgs[n], XtNscrollVertical, XawtextScrollAlways);n++;
    facts_text = XtCreateManagedWidget("facts_text",
                                       asciiTextWidgetClass,
                                       facts_form,
                                       TheArgs, n);

    XtOverrideTranslations(facts_text,
                           XtParseTranslationTable(xclips_translation2));

    XtPopup(facts, XtGrabNone);

    if(! EnvAddRouter(theEnv,"xfacts", 10, XclipsQuery, XclipsPrint, NULL, NULL, XclipsExit))
      {
      EnvPrintRouter(theEnv,"werror", "Could not allocate xfacts router!\n");
      XclipsExit(theEnv,0);
       EnvExitRouter(theEnv,0);
      }

}
/*******************************************************************************
          Name:        AgendaWindowCallback
          Description: Called when Agenda Window is selected from the Window
                       menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void AgendaWindowCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   
   if (Browse_status[AGENDA_WIN])
     {
      XtSetArg(TheArgs[0], XtNleftBitmap, None);
      XtPopdown(agenda);
     }
   else if(agenda != NULL)
     {
      XtPopup(agenda,XtGrabNone);
      EnvSetAgendaChanged(theEnv,FALSE);
      PrintChangedAgenda();
      XtSetArg(TheArgs[0], XtNleftBitmap,checker);
     }
   else
     {
      CreateAgendaWindow();
      EnvSetAgendaChanged(theEnv,FALSE);
      PrintChangedAgenda();
      XtSetArg(TheArgs[0], XtNleftBitmap, checker);
     }

   XtSetValues(agenda_window, TheArgs, 1);
   Browse_status[AGENDA_WIN] = !Browse_status[AGENDA_WIN];
  }
  
/*******************************************************************************
          Name:
          Description:
          Arguments:  None
          Returns:     None
*******************************************************************************/
void CreateAgendaWindow()
{
   void *theEnv = GetCurrentEnvironment();
    Dimension width;
    char *name,labelBuffer[256];
    struct defmodule* theModule = (struct defmodule *) EnvGetCurrentModule(theEnv);

    XtSetArg(TheArgs[0], XtNwidth, &width);
    XtGetValues(dialog, TheArgs, 1);
    XtSetArg(TheArgs[0], XtNwidth,2 * width/3);
    if(theModule  != NULL)
     {
       name = EnvGetDefmoduleName(theEnv,theModule);
       strcpy(labelBuffer,"Agenda Window(");
       strcat(labelBuffer,name);
       strcat(labelBuffer,")");
     }
    else
     {
       strcpy(labelBuffer,"Agenda Window");
     }
    agenda = XtCreatePopupShell(labelBuffer,
                                topLevelShellWidgetClass,
                                toplevel,
                                TheArgs, 1);

    XtSetArg(TheArgs[0], XtNdefaultDistance, 0);
    agenda_form = XtCreateManagedWidget("agenda_form",
                                        formWidgetClass,
                                        agenda,
                                        TheArgs, 1);

    XtSetArg(TheArgs[0], XtNheight, 150);
    XtSetArg(TheArgs[1], XtNeditType, XawtextAppend);
    XtSetArg(TheArgs[2], XtNscrollHorizontal, XawtextScrollAlways);
    XtSetArg(TheArgs[3], XtNscrollVertical, XawtextScrollAlways);
    agenda_text = XtCreateManagedWidget("agenda_text",
                                        asciiTextWidgetClass,
                                        agenda_form,
                                        TheArgs, 4);
    XtOverrideTranslations(agenda_text,
                           XtParseTranslationTable(xclips_translation2));

    XtPopup(agenda, XtGrabNone);

    if(!EnvAddRouter(theEnv, "xagenda", 10, XclipsQuery, XclipsPrint,NULL,NULL,XclipsExit))
      {
      EnvPrintRouter(theEnv,WERROR, "Could not allocate xagenda router!\n");
      XclipsExit(theEnv,0);
      EnvExitRouter(theEnv,0);
      }
}
/*******************************************************************************
          Name:
          Description:
          Arguments: 
          Returns:     None
*******************************************************************************/
void FocusWindowCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   
   if (Browse_status[FOCUS_WIN])
     {
      XtSetArg(TheArgs[0], XtNleftBitmap, None);
      XtPopdown(focus);
     }
   else if (focus != NULL)
     {
      XtPopup(focus,XtGrabNone);
      EnvSetFocusChanged(theEnv,FALSE);
      PrintChangedFocus();
      XtSetArg(TheArgs[0], XtNleftBitmap,checker);
     }
   else
     {
      CreateFocusWindow();
      EnvSetFocusChanged(theEnv,FALSE);
      PrintChangedFocus();
      XtSetArg(TheArgs[0], XtNleftBitmap, checker);
     }
   
   XtSetValues(focus_window, TheArgs, 1);
   Browse_status[FOCUS_WIN] = !Browse_status[FOCUS_WIN];
  }

/*******************************************************************************
          Name:
          Description:
          Arguments:  
          Returns:     None
*******************************************************************************/
void CreateFocusWindow()
  {
   void *theEnv = GetCurrentEnvironment();
    Dimension width;

    XtSetArg(TheArgs[0], XtNwidth, &width);
    XtGetValues(dialog, TheArgs, 1);
    XtSetArg(TheArgs[0], XtNwidth,width/3);
    focus = XtCreatePopupShell("Focus Window",
                                topLevelShellWidgetClass,
                                toplevel,
                                TheArgs, 1);

    XtSetArg(TheArgs[0], XtNdefaultDistance, 0);
    focus_form = XtCreateManagedWidget("agenda_form",
                                        formWidgetClass,
                                        focus,
                                        TheArgs, 1);

    XtSetArg(TheArgs[0], XtNheight, 150);
    XtSetArg(TheArgs[1], XtNeditType, XawtextAppend);
    XtSetArg(TheArgs[2], XtNscrollHorizontal, XawtextScrollAlways);
    XtSetArg(TheArgs[3], XtNscrollVertical, XawtextScrollAlways);
    focus_text = XtCreateManagedWidget("focus_text",
                                        asciiTextWidgetClass,
                                        focus_form,
                                        TheArgs, 4);
    XtOverrideTranslations(focus_text,
                           XtParseTranslationTable(xclips_translation2));

    XtPopup(focus, XtGrabNone);

    if(!EnvAddRouter(theEnv,"xfocus", 10, XclipsQuery, XclipsPrint,NULL,NULL,XclipsExit))
      {
      EnvPrintRouter(theEnv,"werror", "Could not allocate xfocus router!\n");
      XclipsExit(theEnv,0);
      EnvExitRouter(theEnv,0);
      }
}


/*******************************************************************************
          Name:        
          Description: 
          Arguments: 
          Returns:     None
*******************************************************************************/
void InstancesWindowCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   
   if (Browse_status[INSTANCE_WIN])
     {
      XtSetArg(TheArgs[0], XtNleftBitmap, None);
      XtPopdown(instances);
     }
   else if (instances != NULL)
     {
      XtPopup(instances,XtGrabNone);
      EnvSetInstancesChanged(theEnv,FALSE);
      PrintChangedInstances();
      XtSetArg(TheArgs[0], XtNleftBitmap,checker);
     }
   else
     {
      CreateInstanceWindow();
      EnvSetInstancesChanged(theEnv,FALSE);
      PrintChangedInstances();
      XtSetArg(TheArgs[0], XtNleftBitmap, checker);
     }
     
   XtSetValues(instances_window, TheArgs, 1);
   Browse_status[INSTANCE_WIN] = !Browse_status[INSTANCE_WIN];
  }

/**********************************************************************************
 *    CreateInstanceWindow
 **********************************************************************************/
void CreateInstanceWindow()
  {
   void *theEnv = GetCurrentEnvironment();
   Dimension height;
   char *name,labelBuffer[256];
   struct defmodule* theModule = (struct defmodule *) EnvGetCurrentModule(theEnv);


 /* Change the name of the window to the current module */

    if(theModule  != NULL)
     {
       name = EnvGetDefmoduleName(theEnv,theModule);
       strcpy(labelBuffer,"Instances Window(");
       strcat(labelBuffer,name);
       strcat(labelBuffer,")");
     }
     else
     {
       strcpy(labelBuffer,"Instances Window");
     }

    XtSetArg(TheArgs[0], XtNheight, &height);
    XtGetValues(dialog, TheArgs, 1);
    height = (height + 150)/3;
    XtSetArg(TheArgs[0], XtNheight, height);
    instances = XtCreatePopupShell(labelBuffer,
                               topLevelShellWidgetClass,
                               toplevel,
                               TheArgs, 1);

    XtSetArg(TheArgs[0], XtNdefaultDistance, 0);
    instances_form = XtCreateManagedWidget("instances_form",
                                       formWidgetClass,
                                       instances,
                                       TheArgs, 1);

    XtSetArg(TheArgs[0], XtNwidth, 250);
    XtSetArg(TheArgs[1], XtNeditType, XawtextAppend);
    XtSetArg(TheArgs[2], XtNscrollHorizontal,XawtextScrollAlways);
    XtSetArg(TheArgs[3], XtNscrollVertical, XawtextScrollAlways);
    instances_text = XtCreateManagedWidget("instances_text",
                                       asciiTextWidgetClass,
                                       instances_form,
                                       TheArgs, 4);

    XtOverrideTranslations(instances_text,
                           XtParseTranslationTable(xclips_translation2));

    XtPopup(instances, XtGrabNone);
  
   if (! EnvAddRouter(theEnv,"xinstances", 10, XclipsQuery, XclipsPrint, NULL, NULL, XclipsExit))
     {
      EnvPrintRouter(theEnv,"werror", "Could not allocate xinstances router!\n");
      XclipsExit(theEnv,0);
      EnvExitRouter(theEnv,0);
     }
  }

/*******************************************************************************
          Name:
          Description:
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void GlobalsWindowCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   
  if (Browse_status[GLOBAL_WIN])
    {
    XtPopdown(globals);
    XtSetArg(TheArgs[0], XtNleftBitmap, None);
    }
  else if(globals != NULL)
   {
     XtPopup(globals,XtGrabNone);
     EnvSetGlobalsChanged(theEnv,FALSE);
     PrintChangedGlobals();
     XtSetArg(TheArgs[0], XtNleftBitmap,checker);
   }
  else
    {
     CreateGlobalWindow();
     EnvSetGlobalsChanged(theEnv,FALSE);
     PrintChangedGlobals();
     XtSetArg(TheArgs[0], XtNleftBitmap, checker);
    }

  XtSetValues(globals_window, TheArgs, 1);

  Browse_status[GLOBAL_WIN] = !Browse_status[GLOBAL_WIN];

}
/**********************************************************************************
 *    CreateGlobalWindow
 **********************************************************************************/

void CreateGlobalWindow()
  {
   void *theEnv = GetCurrentEnvironment();
   Dimension height;
   char *name,labelBuffer[256];
   struct defmodule* theModule = (struct defmodule *) EnvGetCurrentModule(theEnv);


   /* Change the name of the window to the current module */

   if (theModule  != NULL)
     {
      name = EnvGetDefmoduleName(theEnv,theModule);
      strcpy(labelBuffer,"Globals Window(");
      strcat(labelBuffer,name);
      strcat(labelBuffer,")");
     }
   else
     { strcpy(labelBuffer,"Globals Window"); }

    XtSetArg(TheArgs[0], XtNheight, &height);
    XtGetValues(dialog, TheArgs, 1);
    height = (height + 150)/3;
    XtSetArg(TheArgs[0], XtNheight, height);
    globals = XtCreatePopupShell(labelBuffer,
                               topLevelShellWidgetClass,
                               toplevel,
                               TheArgs, 1);

    XtSetArg(TheArgs[0], XtNdefaultDistance, 0);
    globals_form = XtCreateManagedWidget("globals_form",
                                       formWidgetClass,
                                       globals,
                                       TheArgs, 1);

    XtSetArg(TheArgs[0], XtNwidth, 250);
    XtSetArg(TheArgs[1], XtNeditType, XawtextAppend);
    XtSetArg(TheArgs[2], XtNscrollHorizontal,XawtextScrollAlways);
    XtSetArg(TheArgs[3], XtNscrollVertical, XawtextScrollAlways);
    globals_text = XtCreateManagedWidget("globals_text",
                                       asciiTextWidgetClass,
                                       globals_form,
                                       TheArgs, 4);

    XtOverrideTranslations(globals_text,
                           XtParseTranslationTable(xclips_translation2));

    XtPopup(globals, XtGrabNone);

    if (! EnvAddRouter(theEnv, "xglobals", 10, XclipsQuery, XclipsPrint, NULL, NULL, XclipsExit))
      {
       EnvPrintRouter(theEnv,"werror", "Could not allocate xglobals router!\n");
       XclipsExit(theEnv,0);
       EnvExitRouter(theEnv,0);
      }
}

/*******************************************************************************
          Name:		AllWindowsCallback
          Description:	This function turn all the browse flags to True,
                        create all the windows and put the check mark
                        in front of the items of the window menu
          Arguments:
          Returns:
*******************************************************************************/
void AllWindowsCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
    int n = 0;

    /* ============================================================ *
     *  If the fact window has not been created then create it      *
     * ============================================================ */

    if(!Browse_status[FACT_WIN])
     {
       if(facts != NULL)
         XtPopup(facts,XtGrabNone);
       else
         CreateFactWindow();
       XtSetArg(TheArgs[n],XtNleftBitmap,checker);n++;
       XtSetValues(facts_window,TheArgs,n);       
       Browse_status[FACT_WIN] = !Browse_status[FACT_WIN];
       PrintChangedFacts();
     }
    /* ============================================================ *
     *  If the agenda window has not been created then create it    *
     * ============================================================ */

    if(!Browse_status[AGENDA_WIN])
     {
       if(agenda != NULL)
        XtPopup(agenda,XtGrabNone);
       else
        CreateAgendaWindow();
       XtSetArg(TheArgs[n],XtNleftBitmap,checker);n++;
       XtSetValues(agenda_window,TheArgs,n);
       Browse_status[AGENDA_WIN] = !Browse_status[AGENDA_WIN];
       PrintChangedAgenda();
     }
    /* ============================================================ *
     *  If the instance window has not been created then create it  *
     * ============================================================ */

    if(!Browse_status[INSTANCE_WIN])
     {
       
       if(instances != NULL)
        XtPopup(instances,XtGrabNone);
       else
        CreateInstanceWindow();
       XtSetArg(TheArgs[n],XtNleftBitmap,checker);n++;
       XtSetValues(instances_window,TheArgs,n);
       Browse_status[INSTANCE_WIN] = !Browse_status[INSTANCE_WIN];
       PrintChangedInstances();
     }
    /* ============================================================ *
     *  If the global window has not been created then create it    *
     * ============================================================ */

    if(!Browse_status[GLOBAL_WIN])
     {
       if(globals != NULL)
        XtPopup(globals,XtGrabNone);
       else
        CreateGlobalWindow();
       XtSetArg(TheArgs[n],XtNleftBitmap,checker);n++;
       XtSetValues(globals_window,TheArgs,n);
       Browse_status[GLOBAL_WIN]  = !Browse_status[GLOBAL_WIN];
       PrintChangedGlobals();
    }
    /* ============================================================ *
     *  If the focus window has not been created then create it     *
     * ============================================================ */

    if(!Browse_status[FOCUS_WIN])
    {
       if(focus != NULL)
        XtPopup(focus,XtGrabNone);
       else
        CreateFocusWindow();
       XtSetArg(TheArgs[n],XtNleftBitmap,checker);n++;
       XtSetValues(focus_window,TheArgs,n);
       Browse_status[FOCUS_WIN] = !Browse_status[FOCUS_WIN];
       PrintChangedFocus();
    }
  }

/*******************************************************************************
          Name:		NoWindowsCallback
          Description:  This fucntion will pop down all of the windows
          Arguments:    Widget w - Unused
                        XtPointer client_data, call_data - Unsused
          Returns:	none
*******************************************************************************/
void NoWindowsCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
    int n = 0;

    XtSetArg(TheArgs[n], XtNleftBitmap, None);    n++;

    if (Browse_status[GLOBAL_WIN])
     {
       XtPopdown(globals);
       XtSetValues(globals_window,TheArgs,n);
       Browse_status[GLOBAL_WIN] = !Browse_status[GLOBAL_WIN];
     }
    if (Browse_status[INSTANCE_WIN])
     {
       XtPopdown(instances);
       XtSetValues(instances_window,TheArgs,n);
       Browse_status[INSTANCE_WIN] = !Browse_status[INSTANCE_WIN];
     }     
    if(Browse_status[FACT_WIN])
     {
       XtPopdown(facts);
       XtSetValues(facts_window,TheArgs,n);
       Browse_status[FACT_WIN] = !Browse_status[FACT_WIN]; 
     }
    if(Browse_status[AGENDA_WIN])
     {
       XtPopdown(agenda);
       XtSetValues(agenda_window,TheArgs,n);
       Browse_status[AGENDA_WIN] = !Browse_status[AGENDA_WIN];
     }
    if(Browse_status[FOCUS_WIN])
     {
       XtPopdown(focus);
       XtSetValues(focus_window,TheArgs,n);
       Browse_status[FOCUS_WIN] = !Browse_status[FOCUS_WIN];
     }

  }


/*******************************************************************************
          Name:        CommandLineCLIPSCallback
          Description: Called when Command Line CLIPS is selected from the
                       Window menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void CommandLineCLIPSCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
  system("xterm -e clips &");
  }

/*******************************************************************************
          Name:        ColorUtilityCallback
          Description: Called when Color is selected from the Window menu
          Arguments:  w - menu item that was selected
                       client_data - entry number on menu
                       call_data - not used
          Returns:     None
*******************************************************************************/
void ColorUtilityCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
  system("color&");
  }

/*******************************************************************************
          Name:        IntGetDefruleLis
          Description: Gets the list of rules
          Arguments:  None
          Returns:
*******************************************************************************/
int IntGetDefruleLis()
  {
   void *theEnv = GetCurrentEnvironment();
   struct defrule *rule_ptr;
   int maxItems = 20,itemCount = 0;
   char *name;

   if ((rule_ptr = (struct defrule *) EnvGetNextDefrule(theEnv,NULL)) == NULL)
     {
      item_list = NULL;
      return(0);
     }
   item_list = (String *)calloc(maxItems,sizeof(String));
   
   while(rule_ptr != NULL)
     {
      name = EnvGetDefruleName(theEnv,(void*) rule_ptr);
      item_list[itemCount] = balloc(strlen(name) + 1,char);
      strcpy(item_list[itemCount], name);
      itemCount++;
      if (itemCount == (maxItems -1))
        {
         maxItems = 2*maxItems;
         item_list = (String *)realloc(item_list,maxItems * sizeof(String));
        } 
      rule_ptr = (struct defrule *) EnvGetNextDefrule(theEnv, (void*)rule_ptr);
     }
     
   item_list[itemCount] = NULL;
   sortList(item_list,itemCount);
   return(itemCount);
  }

/******************************************************************************
          Name:        DefruleRemoveCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data - not used
          Returns:     None
*******************************************************************************/
static void DefruleRemoveCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   Widget list_widget = (Widget)client_data;
   XawListReturnStruct *current = XawListShowCurrent(list_widget);

   if (current->list_index == XAW_LIST_NONE)
     { return; }
     
   MoveEndOfFile(dialog_text, &TheEvent);
   SetCommandString(theEnv,"(undefrule ");
   EnvPrintRouter(theEnv,"wclips","(undefrule ");
   AppendCommandString(theEnv,current->string);
   EnvPrintRouter(theEnv,"wclips",current->string);
   AppendCommandString(theEnv,")\n");
   EnvPrintRouter(theEnv,"wclips",")\n");

   /*=====================================================*/
   /* Set this flag to True to get out of the event loop. */
   /*=====================================================*/

   quit_get_event = True;
   list_change = True;
  }

/******************************************************************************
          Name:        DefruleMatchesCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefruleMatchesCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   Widget list_widget = (Widget)client_data;
   XawListReturnStruct *current = XawListShowCurrent(list_widget);

   if (current->list_index == XAW_LIST_NONE)
     { return; }

   MoveEndOfFile(dialog_text, &TheEvent);
   SetCommandString(theEnv,"(matches ");
   EnvPrintRouter(theEnv,"wclips","(matches ");
   AppendCommandString(theEnv,current->string);
   EnvPrintRouter(theEnv,"wclips",current->string);
   AppendCommandString(theEnv,")\n");
   EnvPrintRouter(theEnv,"wclips",")\n");

   /*=====================================================*/
   /* Set this flag to True to get out of the event loop. */
   /*=====================================================*/

   quit_get_event = True;
  }

/******************************************************************************
          Name:        DefrulePprintCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefrulePprintCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE ||
     EnvGetDefrulePPForm(theEnv,(struct constructHeader *) EnvFindDefrule(theEnv,current->string)) == NULL)
      return;
  MoveEndOfFile(dialog_text, &TheEvent);
  SetCommandString(theEnv,"(ppdefrule ");
  EnvPrintRouter(theEnv,"wclips","(ppdefrule ");
  AppendCommandString(theEnv,current->string);
  EnvPrintRouter(theEnv,"wclips",current->string);
  AppendCommandString(theEnv,")\n");
  EnvPrintRouter(theEnv,"wclips",")\n");

  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */

  quit_get_event = True;
  }

/******************************************************************************
          Name:        DefruleRefreshCallback
          Description: Calls CLIPS refresh command
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefruleRefreshCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &TheEvent);
  SetCommandString(theEnv,"(refresh ");
  EnvPrintRouter(theEnv,"wclips","(refresh ");
  AppendCommandString(theEnv,current->string);
  EnvPrintRouter(theEnv,"wclips",current->string);
  AppendCommandString(theEnv,")\n");
  EnvPrintRouter(theEnv,"wclips",")\n");
  quit_get_event = True;
  }

/*******************************************************************************
          Name:        IntGetFactList
          Description: Gets list of facts
          Arguments:  None
          Returns:
*******************************************************************************/
int IntGetFactList()
{
   void *theEnv = GetCurrentEnvironment();
  struct deffacts *fact_ptr;
  int maxItems = 20,itemCount = 0;
  char *name;

  if((fact_ptr = (struct deffacts *) EnvGetNextDeffacts(theEnv,NULL)) == NULL)
    {
      item_list = NULL;
      return(0);
    }
  item_list = (String*)calloc(maxItems,sizeof(String));
  while(fact_ptr != NULL)
     {
      name = (char*) EnvGetDeffactsName(theEnv,fact_ptr);
      item_list[itemCount] = balloc(strlen(name) + 1,char);
      strcpy(item_list[itemCount],name);
      itemCount += 1;
      if(itemCount == (maxItems - 1))
       {
         maxItems = 2 * (maxItems);
         item_list = (String*)realloc(item_list,maxItems * sizeof(String));
       }
      fact_ptr = (struct deffacts *) EnvGetNextDeffacts(theEnv,(void*)fact_ptr);
      }
    item_list[itemCount] = NULL;
    sortList(item_list,itemCount);
    return(itemCount);
  }

/******************************************************************************
          Name:        DeffactsRemove
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DeffactsRemove(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &TheEvent);
  SetCommandString(theEnv,"(undeffacts ");
  EnvPrintRouter(theEnv,"wclips","(undeffacts ");
  AppendCommandString(theEnv,current->string);
  EnvPrintRouter(theEnv,"wclips",current->string);
  AppendCommandString(theEnv,")\n");
  EnvPrintRouter(theEnv,"wclips",")\n");

  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */

  quit_get_event = True;
  list_change = True;
  }

/******************************************************************************
          Name:        DeffactsPprint
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DeffactsPprint(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &TheEvent);
  SetCommandString(theEnv,"(ppdeffacts ");
  EnvPrintRouter(theEnv,"wclips","(ppdeffacts ");
  AppendCommandString(theEnv,current->string);
  EnvPrintRouter(theEnv,"wclips",current->string);
  AppendCommandString(theEnv,")\n");
  EnvPrintRouter(theEnv,"wclips",")\n");

  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;
  }

/*******************************************************************************
          Name:        IntGetDeftemplateList
          Description: Gets list of deftemplates
          Arguments:  None
          Returns:
*******************************************************************************/
int IntGetDeftemplateList()
  {
   void *theEnv = GetCurrentEnvironment();
  struct deftemplate *dtmpl_ptr;
  int itemCount = 0,maxItems = 20;
  char *name;

  if((dtmpl_ptr = (struct deftemplate *) EnvGetNextDeftemplate(theEnv,NULL)) == NULL)
   {
     item_list = NULL;
     return(0);
   }
  item_list = (String *)calloc(maxItems,sizeof(String));
  while(dtmpl_ptr != NULL)
    {
      name = EnvGetDeftemplateName(theEnv,(struct constructHeader *) dtmpl_ptr);
      item_list[itemCount] = balloc(strlen(name) + 1, char);
      strcpy(item_list[itemCount++],name);
      if(itemCount == (maxItems - 1))
       {
         maxItems = 2 * maxItems;
         item_list = (String*)realloc(item_list,maxItems * sizeof(String));
       }
      dtmpl_ptr = (struct deftemplate *) EnvGetNextDeftemplate(theEnv,(void*)dtmpl_ptr);
    }
  item_list[itemCount] = NULL;
  sortList(item_list,itemCount);
  return(itemCount); 
  }

/******************************************************************************
          Name:        DeftemplateRemove
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DeftemplateRemove(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &TheEvent);
  SetCommandString(theEnv,"(undeftemplate ");
  EnvPrintRouter(theEnv,"wclips","(undeftemplate ");
  AppendCommandString(theEnv,current->string);
  EnvPrintRouter(theEnv,"wclips",current->string);
  AppendCommandString(theEnv,")\n");
  EnvPrintRouter(theEnv,"wclips",")\n");

  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */

  quit_get_event = True;
  list_change = True;
  }


/******************************************************************************
          Name:        DeftemplatePprint
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DeftemplatePprint(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &TheEvent);
  SetCommandString(theEnv,"(ppdeftemplate ");
  EnvPrintRouter(theEnv,"wclips","(ppdeftemplate ");
  AppendCommandString(theEnv,current->string);
  EnvPrintRouter(theEnv,"wclips",current->string);
  AppendCommandString(theEnv,")\n");
  EnvPrintRouter(theEnv,"wclips",")\n");

  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;
  }

/*******************************************************************************
          Name:        IntGetDeffunctionList
          Description: Gets list of deffunctions
          Arguments:  None
          Returns:
*******************************************************************************/
int IntGetDeffunctionList()
  {
   void *theEnv = GetCurrentEnvironment();
  DEFFUNCTION * dfunc_ptr;
  int itemCount = 0,maxItems = 20;
  char *name;

  if((dfunc_ptr = (DEFFUNCTION *) EnvGetNextDeffunction(theEnv,NULL)) == NULL)
    {
      item_list = NULL;
      return(0);
    }
  item_list = (String *)calloc(maxItems,sizeof(String));
  while(dfunc_ptr != NULL)
   {
     name = EnvGetDeffunctionName(theEnv,(void *) dfunc_ptr);
     item_list[itemCount] = balloc(strlen(name) + 1, char); 
     strcpy(item_list[itemCount++],name);
     if(itemCount == (maxItems - 1))
        {
          maxItems = maxItems * 2;
          item_list = (String *)realloc(item_list,maxItems * sizeof(String));
        }
     dfunc_ptr = (DEFFUNCTION *) EnvGetNextDeffunction(theEnv,(void*)dfunc_ptr);  
   }
  item_list[itemCount] = NULL;
  sortList(item_list,itemCount);
  return(itemCount);
  }

/******************************************************************************
          Name:        DeffunctionRemoveCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DeffunctionRemoveCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &TheEvent);
  SetCommandString(theEnv,"(undeffunction ");
  EnvPrintRouter(theEnv,"wclips","(undeffunction ");
  AppendCommandString(theEnv,current->string);
  EnvPrintRouter(theEnv,"wclips",current->string);
  AppendCommandString(theEnv,")\n");
  EnvPrintRouter(theEnv,"wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;
  list_change = True;
  }


/******************************************************************************
          Name:        DeffunctionPprintCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DeffunctionPprintCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   Widget list_widget = (Widget)client_data;
   XawListReturnStruct *current = XawListShowCurrent(list_widget);

   if (current->list_index == XAW_LIST_NONE ||
       EnvGetDeffunctionPPForm(theEnv,EnvFindDeffunction(theEnv,current->string)) == NULL)
     { return; }
     
   MoveEndOfFile(dialog_text, &TheEvent);
   SetCommandString(theEnv,"(ppdeffunction ");
   EnvPrintRouter(theEnv,"wclips","(ppdeffunction ");
   AppendCommandString(theEnv,current->string);
   EnvPrintRouter(theEnv,"wclips",current->string);
   AppendCommandString(theEnv,")\n");
   EnvPrintRouter(theEnv,"wclips",")\n");
   
   /*=====================================================*/
   /* Set this flag to True to get out of the event loop. */
   /*=====================================================*/
   quit_get_event = True;
  }

/*******************************************************************************
          Name:        IntGetDefgenericList
          Description: Gets list of defgenerics
          Arguments:  None
          Returns:
*******************************************************************************/
int IntGetDefgenericList()
  {
   void *theEnv = GetCurrentEnvironment();
   int maxItems = 20,itemCount = 0;
   struct generic_func *generic_func_ptr;
   char *name;

   if ((generic_func_ptr = (struct generic_func *) EnvGetNextDefgeneric(theEnv,NULL)) == NULL)
     {
      item_list  =  NULL;
      return(0);
     }
     
   item_list = (String *)calloc(maxItems,sizeof(String));
   while( generic_func_ptr != NULL)
     {
      name = (char*) EnvGetDefgenericName(theEnv,generic_func_ptr);
      item_list[itemCount] = balloc(strlen(name) + 1, char);
      strcpy(item_list[itemCount++],name);
      if (itemCount == (maxItems - 1))
        {
         maxItems = maxItems * 2;
         item_list = (String *)realloc(item_list,maxItems*sizeof(String));
        }
      generic_func_ptr = (struct generic_func *) EnvGetNextDefgeneric(theEnv,(void*)generic_func_ptr);
     }
   item_list[itemCount] = NULL;
   sortList(item_list,itemCount);
   return(itemCount);
  }

/******************************************************************************
          Name:        DefgenericRemoveCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefgenericRemoveCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &TheEvent);
  SetCommandString(theEnv,"(undefgeneric ");
  EnvPrintRouter(theEnv,"wclips","(undefgeneric ");
  AppendCommandString(theEnv,current->string);
  EnvPrintRouter(theEnv,"wclips",current->string);
  AppendCommandString(theEnv,")\n");
  EnvPrintRouter(theEnv,"wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;
  list_change = True;
  }


/******************************************************************************
          Name:        DefgenericPprintCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefgenericPprintCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   Widget list_widget = (Widget)client_data;
   XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE ||
     EnvGetDefgenericPPForm(theEnv,EnvFindDefgeneric(theEnv,current->string)) == NULL)
      return;
  MoveEndOfFile(dialog_text, &TheEvent);
  SetCommandString(theEnv,"(ppdefgeneric ");
  EnvPrintRouter(theEnv,"wclips","(ppdefgeneric ");
  AppendCommandString(theEnv,current->string);
  EnvPrintRouter(theEnv,"wclips",current->string);
  AppendCommandString(theEnv,")\n");
  EnvPrintRouter(theEnv,"wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;
  }

/******************************************************************************
        Name:           DefgenericWatchCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void DefgenericWatchCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
  void* defgenericPtr = NULL;
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);

  if(current->list_index == XAW_LIST_NONE)
      return;
  defgenericPtr = EnvFindDefgeneric(theEnv,current->string);
  EnvSetDefgenericWatch(theEnv,!EnvGetDefgenericWatch(theEnv,defgenericPtr),defgenericPtr);
}

/******************************************************************************
        Name:           DefgenericMngrCheckBoxCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void DefgenericMngrCheckBoxCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   void *defgenericPtr = NULL;
   Widget checkbox = (Widget)client_data;
   XawListReturnStruct *current = XawListShowCurrent(w);

   if (current->list_index == XAW_LIST_NONE)
     { return; }

   defgenericPtr = EnvFindDefgeneric(theEnv,current->string);
   XtSetArg(TheArgs[0],XtNstate,EnvGetDefgenericWatch(theEnv,defgenericPtr));
   XtSetValues(checkbox,TheArgs,1);
  }


/******************************************************************************
          Name:        DefgenericMethodCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefgenericMethodCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
  
  
  Widget defmethodmanager,  defmethodmanager_form,  defmethodmanager_viewport,
         removeb, pprint, cancel,watch_label;
  static Widget watch;
  char title[MAX_CHAR_IN_BUF];
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);
  int itemCount = 0;
   
 
  if(current->list_index == XAW_LIST_NONE)
      return;

  /* ========================================== */
  /*   Get the defmethod list                   */
  /* ========================================== */

  itemCount = IntGetDefmethodList(current->string);
  if(item_list1 == NULL)
    {
      defmethodmanager_flag = False;
      return;
    }
   curr_def_name = balloc(strlen(current->string) +  1,char);
   strcpy(curr_def_name,current->string);
   defmethodmanager_flag = True;
   title[0] = 0;
   sprintf(title,"%s Defmethod Manager - %d Items",curr_def_name,itemCount);
   
   /* =========================================== */
   /*   Create the defmethod manager window       */
   /* =========================================== */

   defmethodmanager = XtCreatePopupShell(title,
                                         topLevelShellWidgetClass,
                                         toplevel,
                                         NULL, 0);

   defmethodmanager_form = XtCreateManagedWidget("manager_form",
                                                 formWidgetClass,
                                                  defmethodmanager,
                                                 NULL, 0);

   XtSetArg(TheArgs[0],XtNallowHoriz,True);
   XtSetArg(TheArgs[1],XtNallowVert,True);
   defmethodmanager_viewport = XtCreateManagedWidget("manager_viewport",
                                                     viewportWidgetClass,
                                                      defmethodmanager_form,
                                                     TheArgs, 2);

   XtSetArg(TheArgs[0], XtNlist, item_list1);
   manager_list1 = XtCreateManagedWidget("manager_list",
                                       listWidgetClass,
                                        defmethodmanager_viewport,
                                       TheArgs, 1);
  /* ============================================= */
  /*    Create the Pprint button                   */
  /* ============================================= */

  XtSetArg(TheArgs[0], XtNfromHoriz,  defmethodmanager_viewport);
  XtSetArg(TheArgs[1], XtNlabel, "Remove");
  removeb = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                  defmethodmanager_form,
                                 TheArgs, 2);
  XtAddCallback(removeb,XtNcallback,RemoveDefmethodCallback,(XtPointer)manager_list1);  

  XtSetArg(TheArgs[1], XtNfromVert, removeb);
  XtSetArg(TheArgs[2], XtNlabel, "Pprint");
  pprint = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                  defmethodmanager_form,
                                 TheArgs, 3);
  XtAddCallback(pprint,XtNcallback,DefmethodPprintCallback,(XtPointer)manager_list1);

  /* ============================================= */
  /*  Create the Watch button                      */
  /* ============================================= */

  XtSetArg(TheArgs[1], XtNfromVert,pprint );
  XtSetArg(TheArgs[2], XtNlabel, " ");
  watch = XtCreateManagedWidget("managerButton",
                                 toggleWidgetClass,
                                  defmethodmanager_form,
                                 TheArgs, 3);
  XtAddCallback(watch,XtNcallback,DefmethodWatchCallback,(XtPointer)manager_list1);
  XtAddCallback(manager_list1,XtNcallback,DefmethodMngrCheckBoxCallback,(XtPointer)watch);
  XtSetArg(TheArgs[2], XtNlabel,"Watch");
  XtSetArg(TheArgs[0],XtNfromHoriz,watch);
  watch_label = XtCreateManagedWidget("checkBoxLabel",
                                      labelWidgetClass,
                                      defmethodmanager_form,
                                      TheArgs,3);

  /* ============================================= */
  /*   Create the DOne button                      */
  /* ============================================= */

  XtSetArg(TheArgs[0], XtNfromHoriz,  defmethodmanager_viewport);
  XtSetArg(TheArgs[1], XtNfromVert, watch);
  XtSetArg(TheArgs[2], XtNlabel, "Done");
  cancel = XtCreateManagedWidget("managerCancel",
                                 commandWidgetClass,
                                  defmethodmanager_form,
                                 TheArgs, 3);
  XtAddCallback(cancel, XtNcallback, CancelSelectSecondary,
                (XtPointer) defmethodmanager);

  XtPopup(defmethodmanager, XtGrabNonexclusive);

  }

/******************************************************************************
          Name:        RemoveDefmethodCallback
          Description: removes aa method from the method list
          Arguments:  w - not used
                       client_data - not used
                       call_data - not used
          Returns:     None
*******************************************************************************/
static void RemoveDefmethodCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
    char theIndex[5];
    int i;
    Widget aList = (Widget)client_data;
    XawListReturnStruct *current = XawListShowCurrent(aList);

    if(current->list_index == XAW_LIST_NONE)
      return;
    for (i = 0; ('0' <= current->string[i]) && (current->string[i] <= '9');i++)
      theIndex[i] = current->string[i];

    theIndex[i] = 0;
    MoveEndOfFile(dialog_text, &TheEvent);
    SetCommandString(theEnv,"(undefmethod  ");
    EnvPrintRouter(theEnv,"wclips","(undefmethod ");
    AppendCommandString(theEnv,curr_def_name);
    EnvPrintRouter(theEnv,"wclips",curr_def_name);
    AppendCommandString(theEnv," ");
    EnvPrintRouter(theEnv,"wclips"," ");
    AppendCommandString(theEnv,theIndex);
    EnvPrintRouter(theEnv,"wclips",theIndex);
    AppendCommandString(theEnv,")\n");
    EnvPrintRouter(theEnv,"wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
    quit_get_event = True;
    list1_change = True;

  }
/******************************************************************************
          Name:        DefmethodPprintCallback
          Description: Print the method
          Arguments:  w - not used
                       client_data - the list widget
                       call_data - not used
          Returns:     None
*******************************************************************************/
static void DefmethodPprintCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)

{
   void *theEnv = GetCurrentEnvironment();
    char theIndex[5];
    int i;
    unsigned methodIndex;
    Widget aList = (Widget)client_data;
    XawListReturnStruct *current = XawListShowCurrent(aList);

    if(current->list_index == XAW_LIST_NONE)
      return;
    for (i = 0; ('0' <= current->string[i]) && (current->string[i] <= '9');i++)
      theIndex[i] = current->string[i];

    theIndex[i] = 0;
    methodIndex = (unsigned)atoi(theIndex);
    if(EnvGetDefmethodPPForm(theEnv,EnvFindDefgeneric(theEnv,curr_def_name),methodIndex) == NULL)
      return;
    
    MoveEndOfFile(dialog_text, &TheEvent);
    AppendCommandString(theEnv,"(ppdefmethod ");
    EnvPrintRouter(theEnv,"wclips","(ppdefmethod ");
    AppendCommandString(theEnv,curr_def_name);
    EnvPrintRouter(theEnv,"wclips",curr_def_name);
    AppendCommandString(theEnv," ");
    EnvPrintRouter(theEnv,"wclips"," ");
    AppendCommandString(theEnv,theIndex);
    EnvPrintRouter(theEnv,"wclips",theIndex);
    AppendCommandString(theEnv,")\n");
    EnvPrintRouter(theEnv,"wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
    quit_get_event = True;

}

/*******************************************************************************
          Name:        
          Description:
          Arguments: 
          Returns:
*******************************************************************************/
static void DefmethodWatchCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   char theIndex[5];
   int i;
   unsigned MethodIndex;
   void *defgenericPtr = NULL;
   Widget aList = (Widget)client_data;
   XawListReturnStruct *current = XawListShowCurrent(aList);

   if (current->list_index == XAW_LIST_NONE)
     { return; }
     
   for (i = 0; ('0' <= current->string[i]) && (current->string[i] <= '9');i++)
      theIndex[i] = current->string[i];
   
   theIndex[i] = 0;
   MethodIndex = (unsigned) atoi(theIndex);
   defgenericPtr = EnvFindDefgeneric(theEnv,curr_def_name);
   EnvSetDefmethodWatch(theEnv,! EnvGetDefmethodWatch(theEnv,defgenericPtr,MethodIndex),
                       defgenericPtr,MethodIndex);

}

/*******************************************************************************
          Name:		DefmethodMngrCheckBoxCallback
          Description:
          Arguments:
          Returns:
*******************************************************************************/

static void DefmethodMngrCheckBoxCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   char theIndex[5];
   int i;
   unsigned MethodIndex;
   XawListReturnStruct *current = XawListShowCurrent(w);

   if (current->list_index == XAW_LIST_NONE)
     { return; }
     
    for (i = 0; ('0' <= current->string[i]) && (current->string[i] <= '9');i++)
      theIndex[i] = current->string[i];
    theIndex[i] = 0;
    MethodIndex = (unsigned)atoi(theIndex);
    XtSetArg(TheArgs[0],XtNstate,EnvGetDefmethodWatch(theEnv,EnvFindDefgeneric(theEnv,curr_def_name),MethodIndex));
    XtSetValues((Widget)client_data,TheArgs,1);

}
/*******************************************************************************
          Name:        IntGetDefmethodList
          Description: Gets list of defmethods
          Arguments:  None
          Returns:
*******************************************************************************/

int IntGetDefmethodList(
  char *Aname)
  {
   void *theEnv = GetCurrentEnvironment();
   void *genrc_ptr;
   unsigned theIndex;
   char buf[61];
   register int itemCount = 0;
   int maxItems = 20;

   genrc_ptr = EnvFindDefgeneric(theEnv,Aname);
   if (item_list1 != NULL)
     { free(item_list1); }
   if ((theIndex = EnvGetNextDefmethod(theEnv,genrc_ptr,0)) == 0)
     {
      item_list1 = NULL;
      return(0);
     }
   
   item_list1 = (String*) calloc(maxItems,sizeof(String));
   while (theIndex != 0)
     {
      EnvGetDefmethodDescription(theEnv,buf,60,genrc_ptr,theIndex);
      item_list1[itemCount] = balloc(strlen(buf) + 1,char);
      strcpy(item_list1[itemCount++],buf);
      if (itemCount == (maxItems - 1))
        {
         maxItems = 2 * maxItems;
         item_list1 = (String*)realloc(item_list1,maxItems*sizeof(String));
        }
      theIndex = EnvGetNextDefmethod(theEnv,genrc_ptr,theIndex);
     }
   item_list1[itemCount] = NULL;
   sortList(item_list1,itemCount);
   return(itemCount);
  }

/*******************************************************************************
          Name:        IntGetDefinstancesList
          Description: Gets list of definstances
          Arguments:  None
          Returns:
*******************************************************************************/
int IntGetDefinstancesList()
  {
   void *theEnv = GetCurrentEnvironment();
  int maxItems = 20, itemCount = 0;
  struct definstance *definstance_ptr;
  char *name;


  if((definstance_ptr = (struct definstance *)  EnvGetNextDefinstances(theEnv,NULL)) == NULL)
    {
      item_list = NULL;
      return(0);
    }
  item_list = (String*)calloc(maxItems,sizeof(String));
  while( definstance_ptr != NULL)
   {
      name = EnvGetDefinstancesName(theEnv, (void*)definstance_ptr);
      item_list[itemCount] = balloc(strlen(name) + 1, char);
      strcpy(item_list[itemCount++],name);
      if(itemCount == (maxItems - 1))
       {
         maxItems = maxItems * 2;
         item_list = (String*)realloc(item_list,maxItems * sizeof(String));
       }
      definstance_ptr = (struct definstance *)  EnvGetNextDefinstances(theEnv,(void*)definstance_ptr);
   }
  item_list[itemCount] = NULL;
  sortList(item_list,itemCount);
  return(itemCount);
}

/******************************************************************************
          Name:        DefinstancesRemoveCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefinstancesRemoveCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &TheEvent);
  SetCommandString(theEnv,"(undefinstances ");
  EnvPrintRouter(theEnv,"wclips","(undefinstances ");
  AppendCommandString(theEnv,current->string);
  EnvPrintRouter(theEnv,"wclips",current->string);
  AppendCommandString(theEnv,")\n");
  EnvPrintRouter(theEnv,"wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;
  list_change = True;
  }


/******************************************************************************
          Name:        DefinstancesPprintCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefinstancesPprintCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE ||
     EnvGetDefinstancesPPForm(theEnv,EnvFindDefinstances(theEnv,current->string)) == NULL)
      return;
  MoveEndOfFile(dialog_text, &TheEvent);
  SetCommandString(theEnv,"(ppdefinstances ");
  EnvPrintRouter(theEnv,"wclips","(ppdefinstances ");
  AppendCommandString(theEnv,current->string);
  EnvPrintRouter(theEnv,"wclips",current->string);
  AppendCommandString(theEnv,")\n");
  EnvPrintRouter(theEnv,"wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;
  }

/*******************************************************************************
          Name:        IntGetDefclassList
          Description: Gets list of defclasses
          Arguments:  None
          Returns:
*******************************************************************************/
int IntGetDefclassList()
  {
   void *theEnv = GetCurrentEnvironment();
   int maxItems = 20, itemCount = 0;
   struct cls *cls_ptr;
   char *name;

   if ((cls_ptr = (struct cls *) EnvGetNextDefclass(theEnv,NULL)) == NULL)
     {
      item_list = NULL;
      return(0);
     }
   item_list = (String*)calloc(maxItems,sizeof(String));
   while( cls_ptr != NULL)
     {
      name = (char*)EnvGetDefclassName(theEnv,cls_ptr);
      item_list[itemCount] = balloc(strlen(name) + 1, char);
      strcpy(item_list[itemCount++], name);
      if(itemCount == (maxItems - 1))
       {
         maxItems = maxItems * 2;
         item_list = (String*)realloc(item_list,maxItems * sizeof(String));
       }
      cls_ptr = (struct cls *) EnvGetNextDefclass(theEnv,(void*)cls_ptr);
    }
   item_list[itemCount] = NULL;
   sortList(item_list,itemCount);  
   return(itemCount);
  }

/******************************************************************************
          Name:        DefclassRemoveCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefclassRemoveCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &TheEvent);
  SetCommandString(theEnv,"(undefclass ");
  EnvPrintRouter(theEnv,"wclips","(undefclass ");
  AppendCommandString(theEnv,current->string);
  EnvPrintRouter(theEnv,"wclips",current->string);
  AppendCommandString(theEnv,")\n");
  EnvPrintRouter(theEnv,"wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;
  list_change = True;

  }

/******************************************************************************
          Name:        DefclassDescribeCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefclassDescribeCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &TheEvent);
  SetCommandString(theEnv,"(describe-class ");
  EnvPrintRouter(theEnv,"wclips","(describe-class ");
  AppendCommandString(theEnv,current->string);
  EnvPrintRouter(theEnv,"wclips",current->string);
  AppendCommandString(theEnv,")\n");
  EnvPrintRouter(theEnv,"wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;
  }

/******************************************************************************
          Name:        DefclassBrowseCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefclassBrowseCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE)
      return;
  MoveEndOfFile(dialog_text, &TheEvent);
  SetCommandString(theEnv,"(browse-classes ");
  EnvPrintRouter(theEnv,"wclips","(browse-classes ");
  AppendCommandString(theEnv,current->string);
  EnvPrintRouter(theEnv,"wclips",current->string);
  AppendCommandString(theEnv,")\n");
  EnvPrintRouter(theEnv,"wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;
  }


/******************************************************************************
          Name:        DefclassPprintCallback
          Description: Calls CLIPS
          Arguments:  w - not used
                       client_data - not used
                       call_data
          Returns:     None
*******************************************************************************/
static void DefclassPprintCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);


  if(current->list_index == XAW_LIST_NONE ||
     EnvGetDefclassPPForm(theEnv,EnvFindDefclass(theEnv,current->string)) == NULL)
      return;
  MoveEndOfFile(dialog_text, &TheEvent);
  SetCommandString(theEnv,"(ppdefclass ");
  EnvPrintRouter(theEnv,"wclips","(ppdefclass ");
  AppendCommandString(theEnv,current->string);	
  EnvPrintRouter(theEnv,"wclips",current->string);
  AppendCommandString(theEnv,")\n");
  EnvPrintRouter(theEnv,"wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
  quit_get_event = True;
  }

/******************************************************************************
          Name:        DefclassMessageHandlersCallback
          Description: Displays a dialog box allowing the message handlers for 
                       the currently selected class to  be browsed.
          Arguments:  w - not used
                       client_data - not used
                       call_data - the list widget containing the list of defclass
          Returns:     None
*******************************************************************************/
static void DefclassMessageHandlersCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
  char title[MAX_CHAR_IN_BUF];
  Widget defmessHdlrManager, defmessHdlrManager_form, defmessHdlrManager_viewport,
         removeb, pprint, cancel,watch,watch_label;
  int itemCount = 0;
 
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);
 

  if(current->list_index == XAW_LIST_NONE)
      return;
  itemCount = IntGetDefmessgHndlerList(current->string);
  if(item_list1 == NULL)
   {
     defmessagehandler_flag = False;
     return;
   }
  curr_def_name = balloc(strlen(current->string) + 1,char);
  strcpy(curr_def_name,current->string);
  title[0] = 0;
  sprintf(title,"%s Defmessage-Handler Manager - %d Items",curr_def_name,itemCount);
  defmessagehandler_flag = True;

  /* ========================================== */
  /*  Create Defmessage Manager window          */
  /* ========================================== */

  defmessHdlrManager =  XtCreatePopupShell(title,
                                         topLevelShellWidgetClass,
                                         toplevel,
                                         NULL, 0);
  defmessHdlrManager_form =  XtCreateManagedWidget("manager_form",
                                                 formWidgetClass,
                                                 defmessHdlrManager,
                                                 NULL, 0);
  XtSetArg(TheArgs[0],XtNallowHoriz,True);
  XtSetArg(TheArgs[1],XtNallowVert,True);
  defmessHdlrManager_viewport = XtCreateManagedWidget("manager_viewport",
                                                     viewportWidgetClass,
                                                     defmessHdlrManager_form,
                                                     TheArgs,2);
   XtSetArg(TheArgs[0], XtNlist, item_list1);
   manager_list1 = XtCreateManagedWidget("manager_list",
                                         listWidgetClass,
                                         defmessHdlrManager_viewport,
                                         TheArgs, 1);

  /* ========================================== */
  /*  Create the Remove button                  */
  /* ========================================== */

  XtSetArg(TheArgs[0], XtNfromHoriz,  defmessHdlrManager_viewport);
  XtSetArg(TheArgs[1], XtNlabel, "Remove");
  removeb = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 defmessHdlrManager_form,
                                 TheArgs, 2);
  XtAddCallback(removeb,XtNcallback,RemoveMessageHandlerCallback,(XtPointer)manager_list1); 

  /* ========================================== */
  /*  Create the Pprrint button                 */
  /* ========================================== */

  XtSetArg(TheArgs[1], XtNfromVert, removeb);
  XtSetArg(TheArgs[2], XtNlabel, "Pprint");
  pprint = XtCreateManagedWidget("managerButton",
                                 commandWidgetClass,
                                 defmessHdlrManager_form,
                                 TheArgs, 3);
  XtAddCallback(pprint,XtNcallback,MessageHandlerPprintCallback,manager_list1); 

  /* ========================================== */
  /*  Create the Watch button                   */
  /* ========================================== */

  XtSetArg(TheArgs[1],XtNfromVert,pprint);
  XtSetArg(TheArgs[2],XtNlabel," ");
  watch = XtCreateManagedWidget("managerButton",
                                 toggleWidgetClass,
                                 defmessHdlrManager_form,
                                 TheArgs, 3);
  XtAddCallback(watch,XtNcallback,DefmessHdlrMngrWatchCallback,manager_list1);
  XtAddCallback(manager_list1,XtNcallback,DefmessHdlrMngrCheckBoxCallback,watch);
  XtSetArg(TheArgs[0],XtNfromHoriz,watch);
  XtSetArg(TheArgs[1],XtNfromVert,pprint);
  XtSetArg(TheArgs[2],XtNlabel,"Watch");
  watch_label = XtCreateManagedWidget("checkBoxLabel",
                                 labelWidgetClass,
                                 defmessHdlrManager_form,
                                 TheArgs, 3);

  /* ========================================== */
  /*  Create the Cancel button                  */
  /* ========================================== */

  XtSetArg(TheArgs[0], XtNfromHoriz,defmessHdlrManager_viewport);
  XtSetArg(TheArgs[1], XtNfromVert, watch);
  XtSetArg(TheArgs[2], XtNlabel, "Done");
  cancel = XtCreateManagedWidget("managerCancel",
                                 commandWidgetClass,
                                 defmessHdlrManager_form,
                                 TheArgs, 3);
  XtAddCallback(cancel,XtNcallback,CancelSelectSecondary,defmessHdlrManager);
  XtPopup(defmessHdlrManager, XtGrabNonexclusive);
  }

/*******************************************************************************
          Name:        IntGetDefmessgHndlerList
          Description: Gets the defmessage-handlers list
          Arguments:  name of the defclass
          Returns:
*******************************************************************************/
int IntGetDefmessgHndlerList(
char *name)
{
   void *theEnv = GetCurrentEnvironment();
   void *defclass_ptr;
   unsigned theIndex;
   char *buf1,*buf2;
   unsigned int maxItems = 20;
   int itemCount = 0;
 
   defclass_ptr = EnvFindDefclass(theEnv,name);
   if(item_list1 != NULL)
     free(item_list1);
   if((theIndex = EnvGetNextDefmessageHandler(theEnv,defclass_ptr,0)) == 0)
    {
      item_list1 = NULL;
      return(0);
    }
   item_list1 = (String*)calloc(maxItems,sizeof(String));
   while(theIndex != 0)
    {
      buf1 = EnvGetDefmessageHandlerName(theEnv,defclass_ptr,theIndex);
      buf2 = EnvGetDefmessageHandlerType(theEnv,defclass_ptr,theIndex);
      item_list1[itemCount] = balloc(strlen(buf1) + strlen(buf2) + 2,char);
      strcpy(item_list1[itemCount],buf1);
      strcat(item_list1[itemCount]," ");
      strcat(item_list1[itemCount++],buf2);
      if(itemCount == (maxItems - 1))
       {
          maxItems = 2 * maxItems;
          item_list1 = (String*)realloc(item_list1,maxItems * sizeof(String));
       }
      theIndex = EnvGetNextDefmessageHandler(theEnv,defclass_ptr,theIndex);
    }
   item_list1[itemCount] = NULL;
   sortList(item_list1,itemCount);
   return(itemCount);
}
/*******************************************************************************
          Name:        RemoveMessageHandlerCallback
          Description: Take the message-handler out of the list
          Arguments:   w - widget that initiate this call back
                       call_data - not used
                       client_data - the list widget which contains the list
                                     of the defmessage-handlers
          Returns:
*******************************************************************************/
static void RemoveMessageHandlerCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
{
   void *theEnv = GetCurrentEnvironment();
    char buf[256];
    int i;
    Widget aList = (Widget)client_data;
    XawListReturnStruct *current = XawListShowCurrent(aList);

    if(current->list_index == XAW_LIST_NONE)
      return;
    for (i = 0; current->string[i] != ' '; i++)
      buf[i] = current->string[i];

    buf[i] = 0;
    MoveEndOfFile(dialog_text, &TheEvent);
    AppendCommandString(theEnv,"(undefmessage-handler  ");
    EnvPrintRouter(theEnv,"wclips","(undefmessage-handler  ");
    AppendCommandString(theEnv,curr_def_name);
    EnvPrintRouter(theEnv,"wclips",curr_def_name);
    AppendCommandString(theEnv," ");
    EnvPrintRouter(theEnv,"wclips"," ");
    AppendCommandString(theEnv,buf);
    EnvPrintRouter(theEnv,"wclips",buf);
    strcpy(buf,&(current->string[i]));
    AppendCommandString(theEnv," ");
    EnvPrintRouter(theEnv,"wclips"," ");
    AppendCommandString(theEnv,buf);
    EnvPrintRouter(theEnv,"wclips",buf);
    AppendCommandString(theEnv,")\n");
    EnvPrintRouter(theEnv,"wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
    quit_get_event = True;
    list1_change = True;    

}
/*******************************************************************************
          Name:        MessageHandlerPprintCallback
          Description: Take the message-handler out of the list
          Arguments:   w - widget that initiate this call back
                       call_data - not used
                       client_data - the list widget which contains the list
                                     of the defmessage-handlers
          Returns:
*******************************************************************************/
static void MessageHandlerPprintCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
{
   void *theEnv = GetCurrentEnvironment();
    char buf[256];
    int i;
    unsigned messageIndex;
    Widget aList = (Widget)client_data;
    XawListReturnStruct *current = XawListShowCurrent(aList);

    if(current->list_index == XAW_LIST_NONE)
      return;
    for (i = 0; current->string[i] != ' '; i++)
      buf[i] = current->string[i];
    buf[i] = 0;
    i++;
    messageIndex  = EnvFindDefmessageHandler(theEnv,EnvFindDefclass(theEnv,curr_def_name),
                                           buf,&(current->string[i]));
    if(EnvGetDefmessageHandlerPPForm(theEnv,EnvFindDefclass(theEnv,curr_def_name),messageIndex) == NULL)
      return;
    MoveEndOfFile(dialog_text, &TheEvent);
    AppendCommandString(theEnv,"(ppdefmessage-handler  ");
    EnvPrintRouter(theEnv,"wclips","(ppdefmessage-handler  ");
    AppendCommandString(theEnv,curr_def_name);
    EnvPrintRouter(theEnv,"wclips",curr_def_name);
    AppendCommandString(theEnv," ");
    EnvPrintRouter(theEnv,"wclips"," ");
    AppendCommandString(theEnv,buf);
    EnvPrintRouter(theEnv,"wclips",buf);
    strcpy(buf,&(current->string[i]));
    AppendCommandString(theEnv," ");
    EnvPrintRouter(theEnv,"wclips"," ");
    AppendCommandString(theEnv,buf);
    EnvPrintRouter(theEnv,"wclips",buf);
    AppendCommandString(theEnv,")\n");
    EnvPrintRouter(theEnv,"wclips",")\n");
  /* ================================================== */
  /* Set this flag to True to get out of the event loop */
  /* ================================================== */
    quit_get_event = True;

}
/*******************************************************************************
          Name:        Initialize
          Description:
          Arguents:  list -
          Returns:
*******************************************************************************/
void InitializeList(
  String list[1000])
  {
  register int i  = 0;

  while(list[i] != NULL)
    {
    release(list[i]);
    list[i++] = NULL;
    }
  }

/*******************************************************************************
          Name:        SetManagerList
          Description:
          Arguments:  widget
          Returns:
*******************************************************************************/
void SetManagerList(
Widget widget)
  {
  manager_list  = widget;
  }

/*******************************************************************************
          Name:        GetManagerList
          Description:
          Arguments:  None
          Returns:     manager_list
*******************************************************************************/
Widget GetManagerList()
  {
  return(manager_list);
  }

/*******************************************************************************
          Name:        RefreshMngrList
          Description: Update the manager lists if neccessary
          Arguments:  None
          Returns:
          Notes:       manager_list and manager_list1 are the global variables
                       which store the list widget(s) on the current manager window(s)

*******************************************************************************/
int RefreshMngrList()
  {
  int itemCount = 0;
  char buffer[MAX_CHAR_IN_BUF];
  Window theWindow;
  Display *theDisplay;
  
  if(list_change)
   {
    list_change = False;
 
  /* =========================================================== *
   *  Update the rule manager list                               *
   * =========================================================== */
  
    if(defrulemanager_flag)
    {
       itemCount = IntGetDefruleLis();
       if(item_list == NULL)
         {
           XtDestroyWidget(XtParent(XtParent(XtParent(manager_list))));
           ClearParameters();
           defrulemanager_flag = False;
         }
       else
        {
           defrulemanager_flag = True;
           theWindow =  XtWindow(XtParent(XtParent(XtParent(manager_list))));
           theDisplay = XtDisplay(XtParent(XtParent(XtParent(manager_list))));
           sprintf(buffer,"Defrule Manager - %d Items",itemCount);
           XStoreName(theDisplay,theWindow,buffer);
           XawListChange(manager_list,item_list,0,0,False);
        }
    }
  /* =========================================================== *
   *  Update the deffact manager list                            *
   * =========================================================== */

    else if (deffactsmanager_flag)
    {
       itemCount = IntGetFactList();
       if(item_list == NULL)
         {
           XtDestroyWidget(XtParent(XtParent(XtParent(manager_list))));
           ClearParameters();
           deffactsmanager_flag = False;
         }
       else
        {
           deffactsmanager_flag = True;
           theWindow =  XtWindow(XtParent(XtParent(XtParent(manager_list))));
           theDisplay = XtDisplay(XtParent(XtParent(XtParent(manager_list))));
           sprintf(buffer,"Deffacts Manager - %d Items",itemCount);
           XStoreName(theDisplay,theWindow,buffer);
           XawListChange(manager_list,item_list,0,0,False);
        }

    }
  /* =========================================================== *
   *  Update the deftemplate manager list                        *
   * =========================================================== */

    else if (deftemplatemanager_flag)
    {
       itemCount = IntGetDeftemplateList();
       if(item_list == NULL)
         {
           XtDestroyWidget(XtParent(XtParent(XtParent(manager_list))));
           ClearParameters();
           deftemplatemanager_flag = False;
         }
       else
        {
           deftemplatemanager_flag = True;
           theWindow =  XtWindow(XtParent(XtParent(XtParent(manager_list))));
           theDisplay = XtDisplay(XtParent(XtParent(XtParent(manager_list))));
           sprintf(buffer,"Deftemplate Manager - %d Items",itemCount);
           XStoreName(theDisplay,theWindow,buffer);
           XawListChange(manager_list,item_list,0,0,False);
        }
    }
  /* =========================================================== *
   *  Update the deffunction manager list                        *
   * =========================================================== */

   else if (deffunctionmanager_flag)
   {
       itemCount = IntGetDeffunctionList();
       if(item_list == NULL)
         {
           XtDestroyWidget(XtParent(XtParent(XtParent(manager_list))));
           ClearParameters();
           deffunctionmanager_flag = False;
         }
       else
        {
           deffunctionmanager_flag = True;
           theWindow =  XtWindow(XtParent(XtParent(XtParent(manager_list))));
           theDisplay = XtDisplay(XtParent(XtParent(XtParent(manager_list))));
           sprintf(buffer,"Deffunction Manager - %d Items",itemCount);
           XStoreName(theDisplay,theWindow,buffer);
           XawListChange(manager_list,item_list,0,0,False);
        }
   }

  /* =========================================================== *
   *  Update the defglobal manager list                         *
   * =========================================================== */
   else if (defglobalmanager_flag)
   {
       itemCount = IntGetDefglobalList();
       if(item_list == NULL)
         {
           XtDestroyWidget(XtParent(XtParent(XtParent(manager_list))));
           ClearParameters();
           defglobalmanager_flag = False;
         }
       else
        {
           defglobalmanager_flag = True;
           theWindow =  XtWindow(XtParent(XtParent(XtParent(manager_list))));
           theDisplay = XtDisplay(XtParent(XtParent(XtParent(manager_list))));
           sprintf(buffer,"Defglobal Manager - %d Items",itemCount);
           XStoreName(theDisplay,theWindow,buffer);
           XawListChange(manager_list,item_list,0,0,False);
        }
   }

  /* =========================================================== *
   *  Update the defgeneric manager list                         *
   * =========================================================== */

   else if (defgenericmanager_flag)
   {
       itemCount = IntGetDefgenericList();
       if(item_list == NULL)
         {
           XtDestroyWidget(XtParent(XtParent(XtParent(manager_list))));
           ClearParameters();
           defgenericmanager_flag = False;
         }
       else
        {
           /*defgenericmanager_flag = True;*/
           theWindow =  XtWindow(XtParent(XtParent(XtParent(manager_list))));
           theDisplay = XtDisplay(XtParent(XtParent(XtParent(manager_list))));
           sprintf(buffer,"Defgeneric Manager - %d Items",itemCount);
           XStoreName(theDisplay,theWindow,buffer);
           XawListChange(manager_list,item_list,0,0,False);
        }
   }

  /* =========================================================== *
   *  Update the definstances manager list                       *
   * =========================================================== */
 
  else if (definstancesmanager_flag)
  {
       itemCount = IntGetDefinstancesList();
       if(item_list == NULL)
         {
           XtDestroyWidget(XtParent(XtParent(XtParent(manager_list))));
           ClearParameters();
           definstancesmanager_flag = False;
         }
       else
        {
           definstancesmanager_flag = True;
           theWindow =  XtWindow(XtParent(XtParent(XtParent(manager_list))));
           theDisplay = XtDisplay(XtParent(XtParent(XtParent(manager_list))));
           sprintf(buffer,"Definstances Manager - %d Items",itemCount);
           XStoreName(theDisplay,theWindow,buffer);
           XawListChange(manager_list,item_list,0,0,False);
        }
   }
  /* =========================================================== *
   *  Update the defclass manager list                               *
   * =========================================================== */

    else if (defclassmanager_flag)
    {
       itemCount = IntGetDefclassList();
       if(item_list == NULL)
         {
           XtDestroyWidget(XtParent(XtParent(XtParent(manager_list))));
           ClearParameters();
           defclassmanager_flag = False;
         }
       else
        {
           defclassmanager_flag = True;
           theWindow =  XtWindow(XtParent(XtParent(XtParent(manager_list))));
           theDisplay = XtDisplay(XtParent(XtParent(XtParent(manager_list))));
           sprintf(buffer,"Defclass Manager - %d Items",itemCount);
           XStoreName(theDisplay,theWindow,buffer);
           XawListChange(manager_list,item_list,0,0,False);
        }
    }
  /* =========================================================== *
   *  Update the agenda manager list                             *
   * =========================================================== */

    else if (agendamanager_flag)
    {
       itemCount = IntGetAgendaList();
       if(item_list == NULL)
         {
           XtDestroyWidget(XtParent(XtParent(XtParent(manager_list))));
           ClearParameters();
           agendamanager_flag = False;
         }
       else
        {
           agendamanager_flag = True;
           theWindow =  XtWindow(XtParent(XtParent(XtParent(manager_list))));
           theDisplay = XtDisplay(XtParent(XtParent(XtParent(manager_list))));
           sprintf(buffer,"Agenda Manager - %d Items",itemCount);
           XStoreName(theDisplay,theWindow,buffer);
           XawListChange(manager_list,item_list,0,0,False);
        }
     }
   } /* End of list change = True */
  /* =========================================================== *
   *  Update the defmethod manager list                          *
   * =========================================================== */
 
  else if (list1_change)
   {
    if ((defmethodmanager_flag)&&(list1_change))
    {
          
       list1_change = False;
       itemCount = IntGetDefmethodList(curr_def_name);
       if(item_list1 == NULL)
        {
           XtDestroyWidget(XtParent(XtParent(XtParent(manager_list1))));
           defmethodmanager_flag = False;
           release(curr_def_name);
        }
       else
        {
           theWindow =  XtWindow(XtParent(XtParent(XtParent(manager_list1))));
           theDisplay = XtDisplay(XtParent(XtParent(XtParent(manager_list1))));
           sprintf(buffer,"%s Defmethod Manager - %d Items",curr_def_name,itemCount);
           XStoreName(theDisplay,theWindow,buffer); 
           XawListChange(manager_list1,item_list1,0,0,False);
        }
    
     }
  /* =========================================================== *
   *  Update the defmessage handler list                         *
   * =========================================================== */

     else if((defmessagehandler_flag)&&(list1_change))
     {

       list1_change = False;
       itemCount = IntGetDefmessgHndlerList(curr_def_name);
       if(item_list1 == NULL)
        {
           XtDestroyWidget(XtParent(XtParent(XtParent(manager_list1))));
           defmessagehandler_flag = False;
           release(curr_def_name);
        }
       else
        {
          theWindow =  XtWindow(XtParent(XtParent(XtParent(manager_list1))));
           theDisplay = XtDisplay(XtParent(XtParent(XtParent(manager_list1))));
           sprintf(buffer,"%s Defmessage-Handler Manager - %d Items",curr_def_name,itemCount);
           XStoreName(theDisplay,theWindow,buffer);
          XawListChange(manager_list1,item_list1,0,0,False);
        }
     }
   } /* End of list1_change = True */ 
  return(0);
  }

/*******************************************************************************
          Name:        ClearParameters
          Description: Clear the list_change flag t False and reset the manager
		       widget to NULL
          Arguments:  None
          Returns:
*******************************************************************************/
void ClearParameters()
  {
  list_change = False;
  SetManagerList((Widget)NULL);
  }

/******************************************************************************
          Name:        CancelSelectPrimary
          Description: Destroys top level popup window for managers
          Arguments:  w - not used
                       client_data - widget to destroy
                       call_data - not used
          Returns:     None
*******************************************************************************/
void CancelSelectPrimary(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
  Widget widget = (Widget) client_data;

  XtDestroyWidget(widget);
  ClearParameters();
  free(item_list);
  item_list = NULL;
  defrulemanager_flag = False;
  deffactsmanager_flag = False;
  deftemplatemanager_flag = False;
  deffunctionmanager_flag = False;
  defglobalmanager_flag = False;
  defgenericmanager_flag = False;
  definstancesmanager_flag = False;
  defclassmanager_flag = False;
  agendamanager_flag = False;
  }

/******************************************************************************
          Name:        CancelSelectSecondary
          Description: Destroys second level popup window for managers
          Arguments:  w - not used
                       client_data - widget to destroy
                       call_data - not used
          Returns:     None
*******************************************************************************/
static void CancelSelectSecondary(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
  Widget widget = (Widget) client_data;

  XtDestroyWidget(widget);
  list1_change = False;
  manager_list1 = NULL;
  release(curr_def_name);
  free(item_list1);
  item_list1 = NULL;
  defmethodmanager_flag = False;
  defmessagehandler_flag = False;
  }


/******************************************************************************
	Name:		DefruleBreakPointCallback
	Description:	
	Arguments:
	Return:
*******************************************************************************/
static void  DefruleBreakPointCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   Boolean OnOff = False;
   void *defrulePtr;
   Widget list_widget = (Widget)client_data;
   XawListReturnStruct *current = XawListShowCurrent(list_widget);

   if (current->list_index == XAW_LIST_NONE)
     { return; }
     
   defrulePtr = EnvFindDefrule(theEnv,current->string);
   XtSetArg(TheArgs[0],XtNstate,&OnOff);
   XtGetValues(w,TheArgs,1);
   
   if (OnOff == True)
     { EnvSetBreak(theEnv,defrulePtr); }
   else
     {
      if (EnvRemoveBreak(theEnv,defrulePtr) == CLIPS_FALSE)
        {
         EnvPrintRouter(theEnv,"werror","Rule ");
         EnvPrintRouter(theEnv,"werror",current->string);
         EnvPrintRouter(theEnv,"werror"," does not have a breakpoint set.\n");
        }
     }
  }
   
/******************************************************************************
        Name:		DefruleActivationCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void   DefruleActivationCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
  void *defrulePtr;
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);

  if(current->list_index == XAW_LIST_NONE)
      return;
  defrulePtr = EnvFindDefrule(theEnv,current->string);
  EnvSetDefruleWatchActivations(theEnv,(! EnvGetDefruleWatchActivations(theEnv,defrulePtr)),defrulePtr);
  
  }

/******************************************************************************
        Name:		DefruleFiringsCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void   DefruleFiringsCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   void *defrulePtr;
   Widget list_widget = (Widget)client_data;
   XawListReturnStruct *current = XawListShowCurrent(list_widget);

   if (current->list_index == XAW_LIST_NONE)
     { return; }
   defrulePtr = EnvFindDefrule(theEnv,current->string);
   EnvSetDefruleWatchFirings(theEnv,(! EnvGetDefruleWatchFirings(theEnv,defrulePtr)),defrulePtr);
  }

/******************************************************************************
        Name:           DefruleMngrCheckboxesCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void DefruleMngrCheckboxesCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   void *defrulePtr;
   Widget *CheckBoxes = (Widget*)client_data;
   XawListReturnStruct *current = XawListShowCurrent(w);

   if (current->list_index == XAW_LIST_NONE)
     { return; }
     
   defrulePtr = EnvFindDefrule(theEnv,current->string);
   XtSetArg(TheArgs[0],XtNstate,EnvDefruleHasBreakpoint(theEnv,defrulePtr));
   XtSetValues(CheckBoxes[0],TheArgs,1);
   XtSetArg(TheArgs[0],XtNstate,EnvGetDefruleWatchActivations(theEnv,defrulePtr));
   XtSetValues(CheckBoxes[1],TheArgs,1);
   XtSetArg(TheArgs[0],XtNstate,EnvGetDefruleWatchFirings(theEnv,defrulePtr));
   XtSetValues(CheckBoxes[2],TheArgs,1);
  }

/******************************************************************************
        Name:		WatchInstancesCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void   WatchInstancesCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   void *defclassPtr;
   Widget list_widget = (Widget)client_data;
   XawListReturnStruct *current = XawListShowCurrent(list_widget);

   if(current->list_index == XAW_LIST_NONE)
       return;
   defclassPtr = EnvFindDefclass(theEnv,current->string);
   EnvSetDefclassWatchInstances(theEnv,! EnvGetDefclassWatchInstances(theEnv,defclassPtr),defclassPtr); 
  }
  
/******************************************************************************
        Name:		WatchSlotCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void   WatchSlotCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   void *defclassPtr;
   Widget list_widget = (Widget)client_data;
   XawListReturnStruct *current = XawListShowCurrent(list_widget);

   if(current->list_index == XAW_LIST_NONE)
       return;
   defclassPtr = EnvFindDefclass(theEnv,current->string);
   EnvSetDefclassWatchSlots(theEnv, ! EnvGetDefclassWatchSlots(theEnv,defclassPtr),defclassPtr); 
  }


/******************************************************************************
        Name:		DefclssMngrChckbxCallback           
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void DefclssMngrChckbxCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   void *defclassPtr;
   Widget *CheckBoxes = (Widget*)client_data;
   XawListReturnStruct *current = XawListShowCurrent(w);

   if (current->list_index == XAW_LIST_NONE)
            return;
     defclassPtr = EnvFindDefclass(theEnv,current->string);
     XtSetArg(TheArgs[0],XtNstate,EnvGetDefclassWatchInstances(theEnv,defclassPtr));
     XtSetValues(CheckBoxes[0],TheArgs,1);
     XtSetArg(TheArgs[0],XtNstate,EnvGetDefclassWatchSlots(theEnv,defclassPtr));
     XtSetValues(CheckBoxes[1],TheArgs,1);
 
  }

/******************************************************************************
        Name:		DeftemplateWatchCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void DeftemplateWatchCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   void *deftemplatePtr = NULL;
   Widget list_widget = (Widget)client_data;
   XawListReturnStruct *current = XawListShowCurrent(list_widget);

   if (current->list_index == XAW_LIST_NONE)
     { return; }
     
   deftemplatePtr = EnvFindDeftemplate(theEnv,current->string);
   EnvSetDeftemplateWatch(theEnv,! EnvGetDeftemplateWatch(theEnv,deftemplatePtr),deftemplatePtr);
  }

/******************************************************************************
        Name:		DeftemplateMngrCheckboxCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void DeftemplateMngrCheckboxCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   void* deftemplatePtr = NULL;
   Widget checkbox = (Widget)client_data;
   XawListReturnStruct *current = XawListShowCurrent(w);

  if(current->list_index == XAW_LIST_NONE)
      return;
   
   deftemplatePtr = EnvFindDeftemplate(theEnv,current->string);
   XtSetArg(TheArgs[0],XtNstate,EnvGetDeftemplateWatch(theEnv,deftemplatePtr));
   XtSetValues(checkbox,TheArgs,1); 
  }


/******************************************************************************
        Name:		DeffunctionWatchCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void DeffunctionWatchCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
  void* deffunctionPtr = NULL;
  Widget list_widget = (Widget)client_data;
  XawListReturnStruct *current = XawListShowCurrent(list_widget);

  if(current->list_index == XAW_LIST_NONE)
      return;
  deffunctionPtr = EnvFindDeffunction(theEnv,current->string);
  EnvSetDeffunctionWatch(theEnv,! EnvGetDeffunctionWatch(theEnv,deffunctionPtr),deffunctionPtr);
}



/******************************************************************************
        Name:           DeffunctionMngrCheckBoxCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void DeffunctionMngrCheckboxCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   void *deffunctionPtr = NULL;
   Widget checkbox = (Widget)client_data;
   XawListReturnStruct *current = XawListShowCurrent(w);

   if (current->list_index == XAW_LIST_NONE)
      return;

  deffunctionPtr = EnvFindDeffunction(theEnv,current->string);
  XtSetArg(TheArgs[0],XtNstate,EnvGetDeffunctionWatch(theEnv,deffunctionPtr));
  XtSetValues(checkbox,TheArgs,1);

}

/******************************************************************************
        Name:           DefmessHdlrMngrWatchCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void DefmessHdlrMngrWatchCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   void *defclassPtr = NULL;
   char *buf1,buf2[20];
   unsigned i,j = 0,theIndex;
   Widget list_widget = (Widget)client_data;
   XawListReturnStruct *current = XawListShowCurrent(list_widget);

  if(current->list_index == XAW_LIST_NONE)
      return;
 /* ======================================= */
  /*  get the name of the defmessage handler */
  /* ======================================  */

  buf1 = (char*)balloc(strlen(current->string),char);
  for (i = 0; current->string[i] != ' '; i++)
    buf1[i] = current->string[i];
  buf1[i] = 0;

  while(current->string[i] == ' ')
    i++;

 /* ======================================= */
 /* Get the handler-type                    */ 
 /* ======================================= */

  while(current->string[i] != 0)
    buf2[j++] = current->string[i++];

  buf2[j] = 0;

  defclassPtr = EnvFindDefclass(theEnv,curr_def_name);
  theIndex = EnvFindDefmessageHandler(theEnv,defclassPtr,buf1,buf2);
  EnvSetDefmessageHandlerWatch(theEnv,
           (! EnvGetDefmessageHandlerWatch(theEnv,defclassPtr,theIndex)),
           defclassPtr,theIndex);
  free(buf1);
}

/******************************************************************************
        Name:           DefmessHdlrMngrCheckBoxCallback
        Description:
        Arguments:
        Return:
*******************************************************************************/
static void DefmessHdlrMngrCheckBoxCallback(
  Widget w,
  XtPointer client_data, 
  XtPointer call_data)
  {
   void *theEnv = GetCurrentEnvironment();
   void* defclassPtr = NULL;
   char *buf1;
   char buf2[20];
   unsigned i,j =  0, theIndex;
   Widget checkbox = (Widget)client_data;
   XawListReturnStruct *current = XawListShowCurrent(w);

   if (current->list_index == XAW_LIST_NONE)
     { return; }

  /* ======================================= */
  /*  get the name of the defmessage handler */
  /* ======================================  */

  buf1 = (char*)balloc(strlen(current->string),char);
  for (i = 0; current->string[i] != ' '; i++)
    buf1[i] = current->string[i];
  buf1[i] = 0;

  while(current->string[i] == ' ')
     i++;

 /* ======================================= */
 /* Get the handler-type                    */
 /* ======================================= */
 
  while(current->string[i] != 0)
    buf2[j++] = current->string[i++];

  buf2[j] = 0;
 
  defclassPtr = EnvFindDefclass(theEnv,curr_def_name);
  theIndex = EnvFindDefmessageHandler(theEnv,defclassPtr,buf1,buf2);
  XtSetArg(TheArgs[0],XtNstate,EnvGetDefmessageHandlerWatch(theEnv,defclassPtr,theIndex));
  XtSetValues(checkbox,TheArgs,1);
  free(buf1);
}

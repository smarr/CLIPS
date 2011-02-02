   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.24  06/02/06          */
   /*                                                     */
   /*                OBJECT MESSAGE COMMANDS              */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*      6.23: Changed name of variable log to logName        */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Removed IMPERATIVE_MESSAGE_HANDLERS            */
/*                    compilation flag.                      */
/*                                                           */
/*            Corrected code to remove run-time program      */
/*            compiler warnings.                             */
/*                                                           */
/*************************************************************/

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "setup.h"

#if OBJECT_SYSTEM

#include <string.h>

#include "argacces.h"
#include "classcom.h"
#include "classfun.h"
#include "classinf.h"
#include "envrnmnt.h"
#include "insfun.h"
#include "insmoddp.h"
#include "msgfun.h"
#include "msgpass.h"
#include "memalloc.h"
#include "prccode.h"
#include "router.h"

#if BLOAD || BLOAD_AND_BSAVE
#include "bload.h"
#endif

#if ! RUN_TIME
#include "extnfunc.h"
#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)
#include "constrct.h"
#include "msgpsr.h"
#endif

#if DEBUGGING_FUNCTIONS
#include "watch.h"
#endif

#define _MSGCOM_SOURCE_
#include "msgcom.h"

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

#if ! RUN_TIME
static void CreateSystemHandlers(void *);
#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)
static int WildDeleteHandler(void *,DEFCLASS *,SYMBOL_HN *,char *);
#endif

#if DEBUGGING_FUNCTIONS
static unsigned DefmessageHandlerWatchAccess(void *,int,unsigned,EXPRESSION *);
static unsigned DefmessageHandlerWatchPrint(void *,char *,int,EXPRESSION *);
static unsigned DefmessageHandlerWatchSupport(void *,char *,char *,int,
                                              void (*)(void *,char *,void *,int),
                                              void (*)(void *,int,void *,int),
                                              EXPRESSION *);
static unsigned WatchClassHandlers(void *,void *,char *,int,char *,int,int,
                                  void (*)(void *,char *,void *,int),
                                  void (*)(void *,int,void *,int));
static void PrintHandlerWatchFlag(void *,char *,void *,int);
#endif

static void DeallocateMessageHandlerData(void *);
  
/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***************************************************
  NAME         : SetupMessageHandlers
  DESCRIPTION  : Sets up internal symbols and
                 fucntion definitions pertaining to
                 message-handlers.  Also creates
                 system handlers
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Functions and data structures
                 initialized
  NOTES        : Should be called before
                 SetupInstanceModDupCommands() in
                 INSMODDP.C
 ***************************************************/
globle void SetupMessageHandlers(
  void *theEnv,
  EXEC_STATUS)
  {
   ENTITY_RECORD handlerGetInfo = { "HANDLER_GET", HANDLER_GET,0,1,1,
                                        PrintHandlerSlotGetFunction,
                                        PrintHandlerSlotGetFunction,NULL,
                                        HandlerSlotGetFunction,
                                        NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL },

                 handlerPutInfo = { "HANDLER_PUT", HANDLER_PUT,0,1,1,
                                        PrintHandlerSlotPutFunction,
                                        PrintHandlerSlotPutFunction,NULL,
                                        HandlerSlotPutFunction,
                                        NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL };

   AllocateEnvironmentData(theEnv,execStatus,MESSAGE_HANDLER_DATA,sizeof(struct messageHandlerData),DeallocateMessageHandlerData);
   memcpy(&MessageHandlerData(theEnv)->HandlerGetInfo,&handlerGetInfo,sizeof(struct entityRecord));   
   memcpy(&MessageHandlerData(theEnv)->HandlerPutInfo,&handlerPutInfo,sizeof(struct entityRecord));   

   MessageHandlerData(theEnv)->hndquals[0] = "around";
   MessageHandlerData(theEnv)->hndquals[1] = "before";
   MessageHandlerData(theEnv)->hndquals[2] = "primary";
   MessageHandlerData(theEnv)->hndquals[3] = "after";

   InstallPrimitive(theEnv,execStatus,&MessageHandlerData(theEnv)->HandlerGetInfo,HANDLER_GET);
   InstallPrimitive(theEnv,execStatus,&MessageHandlerData(theEnv)->HandlerPutInfo,HANDLER_PUT);

#if ! RUN_TIME
   MessageHandlerData(theEnv)->INIT_SYMBOL = (SYMBOL_HN *) EnvAddSymbol(theEnv,execStatus,INIT_STRING);
   IncrementSymbolCount(MessageHandlerData(theEnv)->INIT_SYMBOL);

   MessageHandlerData(theEnv)->DELETE_SYMBOL = (SYMBOL_HN *) EnvAddSymbol(theEnv,execStatus,DELETE_STRING);
   IncrementSymbolCount(MessageHandlerData(theEnv)->DELETE_SYMBOL);
   
   MessageHandlerData(theEnv)->CREATE_SYMBOL = (SYMBOL_HN *) EnvAddSymbol(theEnv,execStatus,CREATE_STRING);
   IncrementSymbolCount(MessageHandlerData(theEnv)->CREATE_SYMBOL);
   
   EnvAddClearFunction(theEnv,execStatus,"defclass",CreateSystemHandlers,-100);

#if ! BLOAD_ONLY
   MessageHandlerData(theEnv)->SELF_SYMBOL = (SYMBOL_HN *) EnvAddSymbol(theEnv,execStatus,SELF_STRING);
   IncrementSymbolCount(MessageHandlerData(theEnv)->SELF_SYMBOL);

   AddConstruct(theEnv,execStatus,"defmessage-handler","defmessage-handlers",
                ParseDefmessageHandler,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL);
   EnvDefineFunction2(theEnv,execStatus,"undefmessage-handler",'v',PTIEF UndefmessageHandlerCommand,
                  "UndefmessageHandlerCommand","23w");

#endif

   EnvDefineFunction2(theEnv,execStatus,"send",'u',PTIEF SendCommand,"SendCommand","2*uuw");

#if DEBUGGING_FUNCTIONS
   EnvDefineFunction2(theEnv,execStatus,"preview-send",'v',PTIEF PreviewSendCommand,"PreviewSendCommand","22w");

   EnvDefineFunction2(theEnv,execStatus,"ppdefmessage-handler",'v',PTIEF PPDefmessageHandlerCommand,
                  "PPDefmessageHandlerCommand","23w");
   EnvDefineFunction2(theEnv,execStatus,"list-defmessage-handlers",'v',PTIEF ListDefmessageHandlersCommand,
                  "ListDefmessageHandlersCommand","02w");
#endif

   EnvDefineFunction2(theEnv,execStatus,"next-handlerp",'b',PTIEF NextHandlerAvailable,"NextHandlerAvailable","00");
   FuncSeqOvlFlags(theEnv,execStatus,"next-handlerp",TRUE,FALSE);
   EnvDefineFunction2(theEnv,execStatus,"call-next-handler",'u',
                  PTIEF CallNextHandler,"CallNextHandler","00");
   FuncSeqOvlFlags(theEnv,execStatus,"call-next-handler",TRUE,FALSE);
   EnvDefineFunction2(theEnv,execStatus,"override-next-handler",'u',
                  PTIEF CallNextHandler,"CallNextHandler",NULL);
   FuncSeqOvlFlags(theEnv,execStatus,"override-next-handler",TRUE,FALSE);

   EnvDefineFunction2(theEnv,execStatus,"dynamic-get",'u',PTIEF DynamicHandlerGetSlot,"DynamicHandlerGetSlot","11w");
   EnvDefineFunction2(theEnv,execStatus,"dynamic-put",'u',PTIEF DynamicHandlerPutSlot,"DynamicHandlerPutSlot","1**w");
   EnvDefineFunction2(theEnv,execStatus,"get",'u',PTIEF DynamicHandlerGetSlot,"DynamicHandlerGetSlot","11w");
   EnvDefineFunction2(theEnv,execStatus,"put",'u',PTIEF DynamicHandlerPutSlot,"DynamicHandlerPutSlot","1**w");
#endif

#if DEBUGGING_FUNCTIONS
   AddWatchItem(theEnv,execStatus,"messages",0,&MessageHandlerData(theEnv)->WatchMessages,36,NULL,NULL);
   AddWatchItem(theEnv,execStatus,"message-handlers",0,&MessageHandlerData(theEnv)->WatchHandlers,35,
                DefmessageHandlerWatchAccess,DefmessageHandlerWatchPrint);
#endif
  }

/*******************************************************/
/* DeallocateMessageHandlerData: Deallocates environment */
/*    data for the message handler functionality.        */
/******************************************************/
static void DeallocateMessageHandlerData(
  void *theEnv,
  EXEC_STATUS)
  {
   HANDLER_LINK *tmp, *mhead, *chead;
    
   mhead = MessageHandlerData(theEnv)->TopOfCore;
   while (mhead != NULL)
     { 
      tmp = mhead;
      mhead = mhead->nxt;
      rtn_struct(theEnv,execStatus,messageHandlerLink,tmp);
     }
     
   chead = MessageHandlerData(theEnv)->OldCore;
   while (chead != NULL)
     { 
      mhead = chead;
      chead = chead->nxtInStack;
      
      while (mhead != NULL)
        {
         tmp = mhead;
         mhead = mhead->nxt;
         rtn_struct(theEnv,execStatus,messageHandlerLink,tmp);
        }
     }
  }

/*****************************************************
  NAME         : EnvGetDefmessageHandlerName
  DESCRIPTION  : Gets the name of a message-handler
  INPUTS       : 1) Pointer to a class
                 2) Array index of handler in class's
                    message-handler array (+1)
  RETURNS      : Name-string of message-handler
  SIDE EFFECTS : None
  NOTES        : None
 *****************************************************/
#if WIN_BTC
#pragma argsused
#endif
char *EnvGetDefmessageHandlerName(
  void *theEnv,
  EXEC_STATUS,
  void *ptr,
  int theIndex)
  {
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv)
#endif

   return(ValueToString(((DEFCLASS *) ptr)->handlers[theIndex-1].name));
  }

/*****************************************************
  NAME         : EnvGetDefmessageHandlerType
  DESCRIPTION  : Gets the type of a message-handler
  INPUTS       : 1) Pointer to a class
                 2) Array index of handler in class's
                    message-handler array (+1)
  RETURNS      : Type-string of message-handler
  SIDE EFFECTS : None
  NOTES        : None
 *****************************************************/
globle char *EnvGetDefmessageHandlerType(
  void *theEnv,
  EXEC_STATUS,
  void *ptr,
  int theIndex)
  {
   return(MessageHandlerData(theEnv)->hndquals[((DEFCLASS *) ptr)->handlers[theIndex-1].type]);
  }

/**************************************************************
  NAME         : EnvGetNextDefmessageHandler
  DESCRIPTION  : Finds first or next handler for a class
  INPUTS       : 1) The address of the handler's class
                 2) The array index of the current handler (+1)
  RETURNS      : The array index (+1) of the next handler, or 0
                   if there is none
  SIDE EFFECTS : None
  NOTES        : If index == 0, the first handler array index
                 (i.e. 1) returned
 **************************************************************/
#if WIN_BTC
#pragma argsused
#endif
globle int EnvGetNextDefmessageHandler(
  void *theEnv,
  EXEC_STATUS,
  void *ptr,
  int theIndex)
  {
   DEFCLASS *cls;
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv)
#endif

   cls = (DEFCLASS *) ptr;
   if (theIndex == 0)
     return((cls->handlers != NULL) ? 1 : 0);
   if (theIndex == cls->handlerCount)
     return(0);
   return(theIndex+1);
  }

/*****************************************************
  NAME         : GetDefmessageHandlerPointer
  DESCRIPTION  : Returns a pointer to a handler
  INPUTS       : 1) Pointer to a class
                 2) Array index of handler in class's
                    message-handler array (+1)
  RETURNS      : Pointer to the handler.
  SIDE EFFECTS : None
  NOTES        : None
 *****************************************************/
globle HANDLER *GetDefmessageHandlerPointer(
  void *ptr,
  int theIndex)
  {
   return(&((DEFCLASS *) ptr)->handlers[theIndex-1]);
  }

#if DEBUGGING_FUNCTIONS

/*********************************************************
  NAME         : EnvGetDefmessageHandlerWatch
  DESCRIPTION  : Determines if trace messages for calls
                 to this handler will be generated or not
  INPUTS       : 1) A pointer to the class
                 2) The index of the handler
  RETURNS      : TRUE if a trace is active,
                 FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 *********************************************************/
#if WIN_BTC
#pragma argsused
#endif
globle unsigned EnvGetDefmessageHandlerWatch(
  void *theEnv,
  EXEC_STATUS,
  void *theClass,
  int theIndex)
  {
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv)
#endif

   return(((DEFCLASS *) theClass)->handlers[theIndex-1].trace);
  }

/*********************************************************
  NAME         : EnvSetDefmessageHandlerWatch
  DESCRIPTION  : Sets the trace to ON/OFF for the
                 calling of the handler
  INPUTS       : 1) TRUE to set the trace on,
                    FALSE to set it off
                 2) A pointer to the class
                 3) The index of the handler
  RETURNS      : Nothing useful
  SIDE EFFECTS : Watch flag for the handler set
  NOTES        : None
 *********************************************************/
#if WIN_BTC
#pragma argsused
#endif
globle void EnvSetDefmessageHandlerWatch(
  void *theEnv,
  EXEC_STATUS,
  int newState,
  void *theClass,
  int theIndex)
  {
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv)
#endif

   ((DEFCLASS *) theClass)->handlers[theIndex-1].trace = newState;
  }

#endif

/***************************************************
  NAME         : EnvFindDefmessageHandler
  DESCRIPTION  : Determines the index of a specfied
                  message-handler
  INPUTS       : 1) A pointer to the class
                 2) Name-string of the handler
                 3) Handler-type: "around","before",
                    "primary", or "after"
  RETURNS      : The index of the handler
                   (0 if not found)
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle unsigned EnvFindDefmessageHandler(
  void *theEnv,
  EXEC_STATUS,
  void *ptr,
  char *hname,
  char *htypestr)
  {
   unsigned htype;
   SYMBOL_HN *hsym;
   DEFCLASS *cls;
   int theIndex;

   htype = HandlerType(theEnv,execStatus,"handler-lookup",htypestr);
   if (htype == MERROR)
     return(0);
   hsym = FindSymbolHN(theEnv,execStatus,hname);
   if (hsym == NULL)
     return(0);
   cls = (DEFCLASS *) ptr;
   theIndex = FindHandlerByIndex(cls,hsym,(unsigned) htype);
   return((unsigned) (theIndex+1));
  }

/***************************************************
  NAME         : EnvIsDefmessageHandlerDeletable
  DESCRIPTION  : Determines if a message-handler
                   can be deleted
  INPUTS       : 1) Address of the handler's class
                 2) Index of the handler
  RETURNS      : TRUE if deletable, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle int EnvIsDefmessageHandlerDeletable(
  void *theEnv,
  EXEC_STATUS,
  void *ptr,
  int theIndex)
  {
   DEFCLASS *cls;

   if (! ConstructsDeletable(theEnv))
     { return FALSE; }

   cls = (DEFCLASS *) ptr;
   if (cls->handlers[theIndex-1].system == 1)
     return(FALSE);

#if (! BLOAD_ONLY) && (! RUN_TIME)
   return((HandlersExecuting(cls) == FALSE) ? TRUE : FALSE);
#else
   return FALSE;
#endif
  }

/******************************************************************************
  NAME         : UndefmessageHandlerCommand
  DESCRIPTION  : Deletes a handler from a class
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Handler deleted if possible
  NOTES        : H/L Syntax: (undefmessage-handler <class> <handler> [<type>])
 ******************************************************************************/
globle void UndefmessageHandlerCommand(
  void *theEnv,
  EXEC_STATUS)
  {
#if RUN_TIME || BLOAD_ONLY
   PrintErrorID(theEnv,execStatus,"MSGCOM",3,FALSE);
   EnvPrintRouter(theEnv,execStatus,WERROR,"Unable to delete message-handlers.\n");
#else
   SYMBOL_HN *mname;
   char *tname;
   DATA_OBJECT tmp;
   DEFCLASS *cls;

#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded(theEnv))
     {
      PrintErrorID(theEnv,execStatus,"MSGCOM",3,FALSE);
      EnvPrintRouter(theEnv,execStatus,WERROR,"Unable to delete message-handlers.\n");
      return;
     }
#endif
   if (EnvArgTypeCheck(theEnv,execStatus,"undefmessage-handler",1,SYMBOL,&tmp) == FALSE)
     return;
   cls = LookupDefclassByMdlOrScope(theEnv,execStatus,DOToString(tmp));
   if ((cls == NULL) ? (strcmp(DOToString(tmp),"*") != 0) : FALSE)
     {
      ClassExistError(theEnv,execStatus,"undefmessage-handler",DOToString(tmp));
      return;
     }
   if (EnvArgTypeCheck(theEnv,execStatus,"undefmessage-handler",2,SYMBOL,&tmp) == FALSE)
     return;
   mname = (SYMBOL_HN *) tmp.value;
   if (EnvRtnArgCount(theEnv) == 3)
     {
      if (EnvArgTypeCheck(theEnv,execStatus,"undefmessage-handler",3,SYMBOL,&tmp) == FALSE)
        return;
      tname = DOToString(tmp);
      if (strcmp(tname,"*") == 0)
        tname = NULL;
     }
   else
     tname = MessageHandlerData(theEnv)->hndquals[MPRIMARY];
   WildDeleteHandler(theEnv,execStatus,cls,mname,tname);
#endif
  }

/***********************************************************
  NAME         : EnvUndefmessageHandler
  DESCRIPTION  : Deletes a handler from a class
  INPUTS       : 1) Class address    (Can be NULL)
                 2) Handler index (can be 0)
  RETURNS      : 1 if successful, 0 otherwise
  SIDE EFFECTS : Handler deleted if possible
  NOTES        : None
 ***********************************************************/
globle int EnvUndefmessageHandler(
  void *theEnv,
  EXEC_STATUS,
  void *vptr,
  int mhi)
  {
#if (MAC_MCW || WIN_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(vptr)
#pragma unused(mhi)
#endif

#if RUN_TIME || BLOAD_ONLY
   PrintErrorID(theEnv,execStatus,"MSGCOM",3,FALSE);
   EnvPrintRouter(theEnv,execStatus,WERROR,"Unable to delete message-handlers.\n");
   return(0);
#else
   DEFCLASS *cls;

#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded(theEnv))
     {
      PrintErrorID(theEnv,execStatus,"MSGCOM",3,FALSE);
      EnvPrintRouter(theEnv,execStatus,WERROR,"Unable to delete message-handlers.\n");
      return(0);
     }
#endif
   if (vptr == NULL)
     {
      if (mhi != 0)
        {
         PrintErrorID(theEnv,execStatus,"MSGCOM",1,FALSE);
         EnvPrintRouter(theEnv,execStatus,WERROR,"Incomplete message-handler specification for deletion.\n");
         return(0);
        }
      return(WildDeleteHandler(theEnv,execStatus,NULL,NULL,NULL));
     }
   if (mhi == 0)
     return(WildDeleteHandler(theEnv,execStatus,(DEFCLASS *) vptr,NULL,NULL));
   cls = (DEFCLASS *) vptr;
   if (HandlersExecuting(cls))
     {
      HandlerDeleteError(theEnv,execStatus,EnvGetDefclassName(theEnv,execStatus,(void *) cls));
      return(0);
     }
   cls->handlers[mhi-1].mark = 1;
   DeallocateMarkedHandlers(theEnv,execStatus,cls);
   return(1);
#endif
  }

#if DEBUGGING_FUNCTIONS

/*******************************************************************************
  NAME         : PPDefmessageHandlerCommand
  DESCRIPTION  : Displays the pretty-print form (if any) for a handler
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : H/L Syntax: (ppdefmessage-handler <class> <message> [<type>])
 *******************************************************************************/
globle void PPDefmessageHandlerCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   DATA_OBJECT temp;
   SYMBOL_HN *csym,*msym;
   char *tname;
   DEFCLASS *cls = NULL;
   unsigned mtype;
   HANDLER *hnd;

   if (EnvArgTypeCheck(theEnv,execStatus,"ppdefmessage-handler",1,SYMBOL,&temp) == FALSE)
     return;
   csym = FindSymbolHN(theEnv,execStatus,DOToString(temp));
   if (EnvArgTypeCheck(theEnv,execStatus,"ppdefmessage-handler",2,SYMBOL,&temp) == FALSE)
     return;
   msym = FindSymbolHN(theEnv,execStatus,DOToString(temp));
   if (EnvRtnArgCount(theEnv) == 3)
     {
      if (EnvArgTypeCheck(theEnv,execStatus,"ppdefmessage-handler",3,SYMBOL,&temp) == FALSE)
        return;
      tname = DOToString(temp);
     }
   else
     tname = MessageHandlerData(theEnv)->hndquals[MPRIMARY];
   mtype = HandlerType(theEnv,execStatus,"ppdefmessage-handler",tname);
   if (mtype == MERROR)
     {
      SetEvaluationError(theEnv,execStatus,TRUE);
      return;
     }
   if (csym != NULL)
     cls = LookupDefclassByMdlOrScope(theEnv,execStatus,ValueToString(csym));
   if (((cls == NULL) || (msym == NULL)) ? TRUE :
       ((hnd = FindHandlerByAddress(cls,msym,(unsigned) mtype)) == NULL))
     {
      PrintErrorID(theEnv,execStatus,"MSGCOM",2,FALSE);
      EnvPrintRouter(theEnv,execStatus,WERROR,"Unable to find message-handler ");
      EnvPrintRouter(theEnv,execStatus,WERROR,ValueToString(msym));
      EnvPrintRouter(theEnv,execStatus,WERROR," ");
      EnvPrintRouter(theEnv,execStatus,WERROR,tname);
      EnvPrintRouter(theEnv,execStatus,WERROR," for class ");
      EnvPrintRouter(theEnv,execStatus,WERROR,ValueToString(csym));
      EnvPrintRouter(theEnv,execStatus,WERROR," in function ppdefmessage-handler.\n");
      SetEvaluationError(theEnv,execStatus,TRUE);
      return;
     }
   if (hnd->ppForm != NULL)
     PrintInChunks(theEnv,execStatus,WDISPLAY,hnd->ppForm);
  }

/*****************************************************************************
  NAME         : ListDefmessageHandlersCommand
  DESCRIPTION  : Depending on arguments, does lists handlers which
                   match restrictions
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : H/L Syntax: (list-defmessage-handlers [<class> [inherit]]))
 *****************************************************************************/
globle void ListDefmessageHandlersCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   int inhp;
   void *clsptr;

   if (EnvRtnArgCount(theEnv) == 0)
     EnvListDefmessageHandlers(theEnv,execStatus,WDISPLAY,NULL,0);
   else
     {
      clsptr = ClassInfoFnxArgs(theEnv,execStatus,"list-defmessage-handlers",&inhp);
      if (clsptr == NULL)
        return;
      EnvListDefmessageHandlers(theEnv,execStatus,WDISPLAY,clsptr,inhp);
     }
  }

/********************************************************************
  NAME         : PreviewSendCommand
  DESCRIPTION  : Displays a list of the core for a message describing
                   shadows,etc.
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Temporary core created and destroyed
  NOTES        : H/L Syntax: (preview-send <class> <msg>)
 ********************************************************************/
globle void PreviewSendCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   DEFCLASS *cls;
   DATA_OBJECT temp;

   /* =============================
      Get the class for the message
      ============================= */
   if (EnvArgTypeCheck(theEnv,execStatus,"preview-send",1,SYMBOL,&temp) == FALSE)
     return;
   cls = LookupDefclassByMdlOrScope(theEnv,execStatus,DOToString(temp));
   if (cls == NULL)
     {
      ClassExistError(theEnv,execStatus,"preview-send",ValueToString(temp.value));
      return;
     }

   if (EnvArgTypeCheck(theEnv,execStatus,"preview-send",2,SYMBOL,&temp) == FALSE)
     return;
   EnvPreviewSend(theEnv,execStatus,WDISPLAY,(void *) cls,DOToString(temp));
  }

/********************************************************
  NAME         : EnvGetDefmessageHandlerPPForm
  DESCRIPTION  : Gets a message-handler pretty print form
  INPUTS       : 1) Address of the handler's class
                 2) Index of the handler
  RETURNS      : TRUE if printable, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ********************************************************/
#if WIN_BTC
#pragma argsused
#endif
globle char *EnvGetDefmessageHandlerPPForm(
  void *theEnv,
  EXEC_STATUS,
  void *ptr,
  int theIndex)
  {
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv)
#endif

   return(((DEFCLASS *) ptr)->handlers[theIndex-1].ppForm);
  }

/*******************************************************************
  NAME         : EnvListDefmessageHandlers
  DESCRIPTION  : Lists message-handlers for a class
  INPUTS       : 1) The logical name of the output
                 2) Class name (NULL to display all handlers)
                 3) A flag indicating whether to list inherited
                    handlers or not
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 *******************************************************************/
globle void EnvListDefmessageHandlers(
  void *theEnv,
  EXEC_STATUS,
  char *logName,
  void *vptr,
  int inhp)
  {
   DEFCLASS *cls;
   long cnt;
   PACKED_CLASS_LINKS plinks;

   if (vptr != NULL)
     {
      cls = (DEFCLASS *) vptr;
      if (inhp)
        cnt = DisplayHandlersInLinks(theEnv,execStatus,logName,&cls->allSuperclasses,0);
      else
        {
         plinks.classCount = 1;
         plinks.classArray = &cls;
         cnt = DisplayHandlersInLinks(theEnv,execStatus,logName,&plinks,0);
        }
     }
   else
     {
      plinks.classCount = 1;
      cnt = 0L;
      for (cls = (DEFCLASS *) EnvGetNextDefclass(theEnv,execStatus,NULL) ;
           cls != NULL ;
           cls = (DEFCLASS *) EnvGetNextDefclass(theEnv,execStatus,(void *) cls))
        {
         plinks.classArray = &cls;
         cnt += DisplayHandlersInLinks(theEnv,execStatus,logName,&plinks,0);
        }
     }
   PrintTally(theEnv,execStatus,logName,cnt,"message-handler","message-handlers");
  }

/********************************************************************
  NAME         : EnvPreviewSend
  DESCRIPTION  : Displays a list of the core for a message describing
                   shadows,etc.
  INPUTS       : 1) Logical name of output
                 2) Class pointer
                 3) Message name-string
  RETURNS      : Nothing useful
  SIDE EFFECTS : Temporary core created and destroyed
  NOTES        : None
 ********************************************************************/
globle void EnvPreviewSend(
  void *theEnv,
  EXEC_STATUS,
  char *logicalName,
  void *clsptr,
  char *msgname)
  {
   HANDLER_LINK *core;
   SYMBOL_HN *msym;

   msym = FindSymbolHN(theEnv,execStatus,msgname);
   if (msym == NULL)
     return;
   core = FindPreviewApplicableHandlers(theEnv,execStatus,(DEFCLASS *) clsptr,msym);
   if (core != NULL)
     {
      DisplayCore(theEnv,execStatus,logicalName,core,0);
      DestroyHandlerLinks(theEnv,execStatus,core);
     }
  }

/****************************************************
  NAME         : DisplayHandlersInLinks
  DESCRIPTION  : Recursively displays all handlers
                  for an array of classes
  INPUTS       : 1) The logical name of the output
                 2) The packed class links
                 3) The index to print from the links
  RETURNS      : The number of handlers printed
  SIDE EFFECTS : None
  NOTES        : Used by DescribeClass()
 ****************************************************/
globle long DisplayHandlersInLinks(
  void *theEnv,
  EXEC_STATUS,
  char *logName,
  PACKED_CLASS_LINKS *plinks,
  int theIndex)
  {
   long i;
   long cnt;

   cnt = (long) plinks->classArray[theIndex]->handlerCount;
   if (((int) theIndex) < (plinks->classCount - 1))
     cnt += DisplayHandlersInLinks(theEnv,execStatus,logName,plinks,theIndex + 1);
   for (i = 0 ; i < plinks->classArray[theIndex]->handlerCount ; i++)
     PrintHandler(theEnv,execStatus,logName,&plinks->classArray[theIndex]->handlers[i],TRUE);
   return(cnt);
  }

#endif

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

#if ! RUN_TIME

/**********************************************************
  NAME         : CreateSystemHandlers
  DESCRIPTION  : Attachess the system message-handlers
                 after a (clear)
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : System handlers created
  NOTES        : Must be called after CreateSystemClasses()
 **********************************************************/
static void CreateSystemHandlers(
  void *theEnv,
  EXEC_STATUS)
  {
   NewSystemHandler(theEnv,execStatus,USER_TYPE_NAME,INIT_STRING,"init-slots",0);
   NewSystemHandler(theEnv,execStatus,USER_TYPE_NAME,DELETE_STRING,"delete-instance",0);
   NewSystemHandler(theEnv,execStatus,USER_TYPE_NAME,CREATE_STRING,"(create-instance)",0);

#if DEBUGGING_FUNCTIONS
   NewSystemHandler(theEnv,execStatus,USER_TYPE_NAME,PRINT_STRING,"ppinstance",0);
#endif

   NewSystemHandler(theEnv,execStatus,USER_TYPE_NAME,DIRECT_MODIFY_STRING,"(direct-modify)",1);
   NewSystemHandler(theEnv,execStatus,USER_TYPE_NAME,MSG_MODIFY_STRING,"(message-modify)",1);
   NewSystemHandler(theEnv,execStatus,USER_TYPE_NAME,DIRECT_DUPLICATE_STRING,"(direct-duplicate)",2);
   NewSystemHandler(theEnv,execStatus,USER_TYPE_NAME,MSG_DUPLICATE_STRING,"(message-duplicate)",2);
  }

#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)

/************************************************************
  NAME         : WildDeleteHandler
  DESCRIPTION  : Deletes a handler from a class
  INPUTS       : 1) Class address (Can be NULL)
                 2) Message Handler Name (Can be NULL)
                 3) Type name ("primary", etc.)
  RETURNS      : 1 if successful, 0 otherwise
  SIDE EFFECTS : Handler deleted if possible
  NOTES        : None
 ************************************************************/
static int WildDeleteHandler(
  void *theEnv,
  EXEC_STATUS,
  DEFCLASS *cls,
  SYMBOL_HN *msym,
  char *tname)
  {
   int mtype;

   if (msym == NULL)
     msym = (SYMBOL_HN *) EnvAddSymbol(theEnv,execStatus,"*");
   if (tname != NULL)
     {
      mtype = (int) HandlerType(theEnv,execStatus,"undefmessage-handler",tname);
      if (mtype == MERROR)
        return(0);
     }
   else
     mtype = -1;
   if (cls == NULL)
     {
      int success = 1;

      for (cls = (DEFCLASS *) EnvGetNextDefclass(theEnv,execStatus,NULL) ;
           cls != NULL ;
           cls = (DEFCLASS *) EnvGetNextDefclass(theEnv,execStatus,(void *) cls))
        if (DeleteHandler(theEnv,execStatus,cls,msym,mtype,FALSE) == 0)
          success = 0;
      return(success);
     }
   return(DeleteHandler(theEnv,execStatus,cls,msym,mtype,TRUE));
  }

#endif

#if DEBUGGING_FUNCTIONS

/******************************************************************
  NAME         : DefmessageHandlerWatchAccess
  DESCRIPTION  : Parses a list of class names passed by
                 AddWatchItem() and sets the traces accordingly
  INPUTS       : 1) A code indicating which trace flag is to be set
                    0 - Watch instance creation/deletion
                    1 - Watch slot changes to instances
                 2) The value to which to set the trace flags
                 3) A list of expressions containing the names
                    of the classes for which to set traces
  RETURNS      : TRUE if all OK, FALSE otherwise
  SIDE EFFECTS : Watch flags set in specified classes
  NOTES        : Accessory function for AddWatchItem()
 ******************************************************************/
#if WIN_BTC
#pragma argsused
#endif
static unsigned DefmessageHandlerWatchAccess(
  void *theEnv,
  EXEC_STATUS,
  int code,
  unsigned newState,
  EXPRESSION *argExprs)
  {
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(code)
#endif
   if (newState)
     return(DefmessageHandlerWatchSupport(theEnv,execStatus,"watch",NULL,newState,
                                        NULL,EnvSetDefmessageHandlerWatch,argExprs));
   else
     return(DefmessageHandlerWatchSupport(theEnv,execStatus,"unwatch",NULL,newState,
                                        NULL,EnvSetDefmessageHandlerWatch,argExprs));
  }

/***********************************************************************
  NAME         : DefmessageHandlerWatchPrint
  DESCRIPTION  : Parses a list of class names passed by
                 AddWatchItem() and displays the traces accordingly
  INPUTS       : 1) The logical name of the output
                 2) A code indicating which trace flag is to be examined
                    0 - Watch instance creation/deletion
                    1 - Watch slot changes to instances
                 3) A list of expressions containing the names
                    of the classes for which to examine traces
  RETURNS      : TRUE if all OK, FALSE otherwise
  SIDE EFFECTS : Watch flags displayed for specified classes
  NOTES        : Accessory function for AddWatchItem()
 ***********************************************************************/
#if WIN_BTC
#pragma argsused
#endif
static unsigned DefmessageHandlerWatchPrint(
  void *theEnv,
  EXEC_STATUS,
  char *logName,
  int code,
  EXPRESSION *argExprs)
  {
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(code)
#endif
   return(DefmessageHandlerWatchSupport(theEnv,execStatus,"list-watch-items",logName,-1,
                                        PrintHandlerWatchFlag,NULL,argExprs));
  }

/*******************************************************
  NAME         : DefmessageHandlerWatchSupport
  DESCRIPTION  : Sets or displays handlers specified
  INPUTS       : 1) The calling function name
                 2) The logical output name for displays
                    (can be NULL)
                 4) The new set state (can be -1)
                 5) The print function (can be NULL)
                 6) The trace function (can be NULL)
                 7) The handlers expression list
  RETURNS      : TRUE if all OK,
                 FALSE otherwise
  SIDE EFFECTS : Handler trace flags set or displayed
  NOTES        : None
 *******************************************************/
static unsigned DefmessageHandlerWatchSupport(
  void *theEnv,
  EXEC_STATUS,
  char *funcName,
  char *logName,
  int newState,
  void (*printFunc)(void *,char *,void *,int),
  void (*traceFunc)(void *,int,void *,int),
  EXPRESSION *argExprs)
  {
   struct defmodule *theModule;
   void *theClass;
   char *theHandlerStr;
   int theType;
   int argIndex = 2;
   DATA_OBJECT tmpData;

   /* ===============================
      If no handlers are specified,
      show the trace for all handlers
      in all handlers
      =============================== */
   if (argExprs == NULL)
     {
      SaveCurrentModule(theEnv);
      theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,NULL);
      while (theModule != NULL)
        {
         EnvSetCurrentModule(theEnv,execStatus,(void *) theModule);
         if (traceFunc == NULL)
           {
            EnvPrintRouter(theEnv,execStatus,logName,EnvGetDefmoduleName(theEnv,execStatus,(void *) theModule));
            EnvPrintRouter(theEnv,execStatus,logName,":\n");
           }
         theClass = EnvGetNextDefclass(theEnv,execStatus,NULL);
         while (theClass != NULL)
            {
             if (WatchClassHandlers(theEnv,execStatus,theClass,NULL,-1,logName,newState,
                                    TRUE,printFunc,traceFunc) == FALSE)
                 return(FALSE);
             theClass = EnvGetNextDefclass(theEnv,execStatus,theClass);
            }
          theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,(void *) theModule);
         }
      RestoreCurrentModule(theEnv);
      return(TRUE);
     }

   /* ================================================
      Set or show the traces for the specified handler
      ================================================ */
   while (argExprs != NULL)
     {
      if (EvaluateExpression(theEnv,execStatus,argExprs,&tmpData))
        return(FALSE);
      if (tmpData.type != SYMBOL)
        {
         ExpectedTypeError1(theEnv,execStatus,funcName,argIndex,"class name");
         return(FALSE);
        }
      theClass = (void *) LookupDefclassByMdlOrScope(theEnv,execStatus,DOToString(tmpData));
      if (theClass == NULL)
        {
         ExpectedTypeError1(theEnv,execStatus,funcName,argIndex,"class name");
         return(FALSE);
        }
      if (GetNextArgument(argExprs) != NULL)
        {
         argExprs = GetNextArgument(argExprs);
         argIndex++;
         if (EvaluateExpression(theEnv,execStatus,argExprs,&tmpData))
           return(FALSE);
         if (tmpData.type != SYMBOL)
           {
            ExpectedTypeError1(theEnv,execStatus,funcName,argIndex,"handler name");
            return(FALSE);
           }
         theHandlerStr = DOToString(tmpData);
         if (GetNextArgument(argExprs) != NULL)
           {
            argExprs = GetNextArgument(argExprs);
            argIndex++;
            if (EvaluateExpression(theEnv,execStatus,argExprs,&tmpData))
              return(FALSE);
            if (tmpData.type != SYMBOL)
              {
               ExpectedTypeError1(theEnv,execStatus,funcName,argIndex,"handler type");
               return(FALSE);
              }
            if ((theType = (int) HandlerType(theEnv,execStatus,funcName,DOToString(tmpData))) == MERROR)
              return(FALSE);
           }
         else
           theType = -1;
        }
      else
        {
         theHandlerStr = NULL;
         theType = -1;
        }
      if (WatchClassHandlers(theEnv,execStatus,theClass,theHandlerStr,theType,logName,
                             newState,FALSE,printFunc,traceFunc) == FALSE)
        {
         ExpectedTypeError1(theEnv,execStatus,funcName,argIndex,"handler");
         return(FALSE);
        }
      argIndex++;
      argExprs = GetNextArgument(argExprs);
     }
   return(TRUE);
  }

/*******************************************************
  NAME         : WatchClassHandlers
  DESCRIPTION  : Sets or displays handlers specified
  INPUTS       : 1) The class
                 2) The handler name (or NULL wildcard)
                 3) The handler type (or -1 wildcard)
                 4) The logical output name for displays
                    (can be NULL)
                 5) The new set state (can be -1)
                 6) The print function (can be NULL)
                 7) The trace function (can be NULL)
  RETURNS      : TRUE if all OK,
                 FALSE otherwise
  SIDE EFFECTS : Handler trace flags set or displayed
  NOTES        : None
 *******************************************************/
static unsigned WatchClassHandlers(
  void *theEnv,
  EXEC_STATUS,
  void *theClass,
  char *theHandlerStr,
  int theType,
  char *logName,
  int newState,
  int indentp,
  void (*printFunc)(void *,char *,void *,int),
  void (*traceFunc)(void *,int,void *,int))
  {
   unsigned theHandler;
   int found = FALSE;

   theHandler = EnvGetNextDefmessageHandler(theEnv,execStatus,theClass,0);
   while (theHandler != 0)
     {
      if ((theType == -1) ? TRUE :
          (theType == (int) ((DEFCLASS *) theClass)->handlers[theHandler-1].type))
        {
         if ((theHandlerStr == NULL) ? TRUE :
             (strcmp(theHandlerStr,EnvGetDefmessageHandlerName(theEnv,execStatus,theClass,theHandler)) == 0))
            {
             if (traceFunc != NULL)
               (*traceFunc)(theEnv,execStatus,newState,theClass,theHandler);
             else
               {
                if (indentp)
                  EnvPrintRouter(theEnv,execStatus,logName,"   ");
                (*printFunc)(theEnv,execStatus,logName,theClass,theHandler);
               }
             found = TRUE;
            }
        }
      theHandler = EnvGetNextDefmessageHandler(theEnv,execStatus,theClass,theHandler);
     }
   if ((theHandlerStr != NULL) && (theType != -1) && (found == FALSE))
     return(FALSE);
   return(TRUE);
  }

/***************************************************
  NAME         : PrintHandlerWatchFlag
  DESCRIPTION  : Displays trace value for handler
  INPUTS       : 1) The logical name of the output
                 2) The class
                 3) The handler index
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
static void PrintHandlerWatchFlag(
  void *theEnv,
  EXEC_STATUS,
  char *logName,
  void *theClass,
  int theHandler)
  {
   EnvPrintRouter(theEnv,execStatus,logName,EnvGetDefclassName(theEnv,execStatus,theClass));
   EnvPrintRouter(theEnv,execStatus,logName," ");
   EnvPrintRouter(theEnv,execStatus,logName,EnvGetDefmessageHandlerName(theEnv,execStatus,theClass,theHandler));
   EnvPrintRouter(theEnv,execStatus,logName," ");
   EnvPrintRouter(theEnv,execStatus,logName,EnvGetDefmessageHandlerType(theEnv,execStatus,theClass,theHandler));
   
   if (EnvGetDefmessageHandlerWatch(theEnv,execStatus,theClass,theHandler))
     EnvPrintRouter(theEnv,execStatus,logName," = on\n");
   else
     EnvPrintRouter(theEnv,execStatus,logName," = off\n");
  }

#endif /* DEBUGGING_FUNCTIONS */
#endif


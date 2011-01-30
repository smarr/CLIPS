   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/09/97          */
   /*                                                     */
   /*                OBJECT MESSAGE COMMANDS              */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
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
#include "insfun.h"
#include "insmoddp.h"
#include "msgfun.h"
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
                   CONSTANTS
   =========================================
   ***************************************** */

/* =========================================
   *****************************************
               MACROS AND TYPES
   =========================================
   ***************************************** */

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

#if ! RUN_TIME
static void CreateSystemHandlers(void);
#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)
static int WildDeleteHandler(DEFCLASS *,SYMBOL_HN *,char *);
#endif

#if DEBUGGING_FUNCTIONS
static BOOLEAN DefmessageHandlerWatchAccess(int,int,EXPRESSION *);
static BOOLEAN DefmessageHandlerWatchPrint(char *,int,EXPRESSION *);
static BOOLEAN DefmessageHandlerWatchSupport(char *,char *,int,
                                             void (*)(char *,void *,unsigned),
                                             void (*)(int,void *,unsigned),EXPRESSION *);
static BOOLEAN WatchClassHandlers(void *,char *,int,char *,int,int,
                                  void (*)(char *,void *,unsigned),
                                  void (*)(int,void *,unsigned));
static void PrintHandlerWatchFlag(char *,void *,unsigned);
#endif

/* =========================================
   *****************************************
      EXTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */

/* =========================================
   *****************************************
      INTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */

Thread static ENTITY_RECORD HandlerGetInfo = { "HANDLER_GET", HANDLER_GET,0,1,1,
                                        PrintHandlerSlotGetFunction,
                                        PrintHandlerSlotGetFunction,NULL,
                                        HandlerSlotGetFunction,
                                        NULL,NULL,NULL,NULL,NULL,NULL },

                     HandlerPutInfo = { "HANDLER_PUT", HANDLER_PUT,0,1,1,
                                        PrintHandlerSlotPutFunction,
                                        PrintHandlerSlotPutFunction,NULL,
                                        HandlerSlotPutFunction,
                                        NULL,NULL,NULL,NULL,NULL,NULL };

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
globle void SetupMessageHandlers()
  {
   InstallPrimitive(&HandlerGetInfo,HANDLER_GET);
   InstallPrimitive(&HandlerPutInfo,HANDLER_PUT);

#if ! RUN_TIME
   INIT_SYMBOL = (SYMBOL_HN *) AddSymbol(INIT_STRING);
   IncrementSymbolCount(INIT_SYMBOL);

   DELETE_SYMBOL = (SYMBOL_HN *) AddSymbol(DELETE_STRING);
   IncrementSymbolCount(DELETE_SYMBOL);
   AddClearFunction("defclass",CreateSystemHandlers,-100);

#if ! BLOAD_ONLY
   SELF_SYMBOL = (SYMBOL_HN *) AddSymbol(SELF_STRING);
   IncrementSymbolCount(SELF_SYMBOL);

   AddConstruct("defmessage-handler","defmessage-handlers",
                ParseDefmessageHandler,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL);
   DefineFunction2("undefmessage-handler",'v',PTIF UndefmessageHandlerCommand,
                  "UndefmessageHandlerCommand","23w");

#endif

   DefineFunction2("send",'u',PTIF SendCommand,"SendCommand","2*uuw");

#if DEBUGGING_FUNCTIONS
   DefineFunction2("preview-send",'v',PTIF PreviewSendCommand,"PreviewSendCommand","22w");

   DefineFunction2("ppdefmessage-handler",'v',PTIF PPDefmessageHandlerCommand,
                  "PPDefmessageHandlerCommand","23w");
   DefineFunction2("list-defmessage-handlers",'v',PTIF ListDefmessageHandlersCommand,
                  "ListDefmessageHandlersCommand","02w");
#endif

#if IMPERATIVE_MESSAGE_HANDLERS
   DefineFunction2("next-handlerp",'b',PTIF NextHandlerAvailable,"NextHandlerAvailable","00");
   FuncSeqOvlFlags("next-handlerp",TRUE,FALSE);
   DefineFunction2("call-next-handler",'u',
                  PTIF CallNextHandler,"CallNextHandler","00");
   FuncSeqOvlFlags("call-next-handler",TRUE,FALSE);
   DefineFunction2("override-next-handler",'u',
                  PTIF CallNextHandler,"CallNextHandler",NULL);
   FuncSeqOvlFlags("override-next-handler",TRUE,FALSE);
#endif

   DefineFunction2("dynamic-get",'u',PTIF DynamicHandlerGetSlot,"DynamicHandlerGetSlot","11w");
   DefineFunction2("dynamic-put",'u',PTIF DynamicHandlerPutSlot,"DynamicHandlerPutSlot","1**w");
   DefineFunction2("get",'u',PTIF DynamicHandlerGetSlot,"DynamicHandlerGetSlot","11w");
   DefineFunction2("put",'u',PTIF DynamicHandlerPutSlot,"DynamicHandlerPutSlot","1**w");
#endif

#if DEBUGGING_FUNCTIONS
   AddWatchItem("messages",0,&WatchMessages,36,NULL,NULL);
   AddWatchItem("message-handlers",0,&WatchHandlers,35,
                DefmessageHandlerWatchAccess,DefmessageHandlerWatchPrint);
#endif
  }

/*****************************************************
  NAME         : GetDefmessageHandlerName
  DESCRIPTION  : Gets the name of a message-handler
  INPUTS       : 1) Pointer to a class
                 2) Array index of handler in class's
                    message-handler array (+1)
  RETURNS      : Name-string of message-handler
  SIDE EFFECTS : None
  NOTES        : None
 *****************************************************/
char *GetDefmessageHandlerName(
  void *ptr,
  unsigned index)
  {
   return(ValueToString(((DEFCLASS *) ptr)->handlers[index-1].name));
  }

/*****************************************************
  NAME         : GetDefmessageHandlerType
  DESCRIPTION  : Gets the type of a message-handler
  INPUTS       : 1) Pointer to a class
                 2) Array index of handler in class's
                    message-handler array (+1)
  RETURNS      : Type-string of message-handler
  SIDE EFFECTS : None
  NOTES        : None
 *****************************************************/
globle char *GetDefmessageHandlerType(
  void *ptr,
  unsigned index)
  {
   return(hndquals[((DEFCLASS *) ptr)->handlers[index-1].type]);
  }

/**************************************************************
  NAME         : GetNextDefmessageHandler
  DESCRIPTION  : Finds first or next handler for a class
  INPUTS       : 1) The address of the handler's class
                 2) The array index of the current handler (+1)
  RETURNS      : The array index (+1) of the next handler, or 0
                   if there is none
  SIDE EFFECTS : None
  NOTES        : If index == 0, the first handler array index
                 (i.e. 1) returned
 **************************************************************/
globle unsigned GetNextDefmessageHandler(
  void *ptr,
  unsigned index)
  {
   DEFCLASS *cls;

   cls = (DEFCLASS *) ptr;
   if (index == 0)
     return((cls->handlers != NULL) ? 1 : 0);
   if (index == cls->handlerCount)
     return(0);
   return(index+1);
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
  unsigned index)
  {
   return(&((DEFCLASS *) ptr)->handlers[index-1]);
  }

#if DEBUGGING_FUNCTIONS

/*********************************************************
  NAME         : GetDefmessageHandlerWatch
  DESCRIPTION  : Determines if trace messages for calls
                 to this handler will be generated or not
  INPUTS       : 1) A pointer to the class
                 2) The index of the handler
  RETURNS      : TRUE if a trace is active,
                 FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 *********************************************************/
globle BOOLEAN GetDefmessageHandlerWatch(
  void *theClass,
  unsigned theIndex)
  {
   return(((DEFCLASS *) theClass)->handlers[theIndex-1].trace);
  }

/*********************************************************
  NAME         : SetDefmessageHandlerWatch
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
globle void SetDefmessageHandlerWatch(
  int newState,
  void *theClass,
  unsigned theIndex)
  {
   ((DEFCLASS *) theClass)->handlers[theIndex-1].trace = newState;
  }

#endif

/***************************************************
  NAME         : FindDefmessageHandler
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
globle unsigned FindDefmessageHandler(
  void *ptr,
  char *hname,
  char *htypestr)
  {
   int htype;
   SYMBOL_HN *hsym;
   DEFCLASS *cls;
   int index;

   htype = HandlerType("handler-lookup",htypestr);
   if (htype == MERROR)
     return(0);
   hsym = FindSymbol(hname);
   if (hsym == NULL)
     return(0);
   cls = (DEFCLASS *) ptr;
   index = FindHandlerByIndex(cls,hsym,(unsigned) htype);
   return((unsigned) (index+1));
  }

/***************************************************
  NAME         : IsDefmessageHandlerDeletable
  DESCRIPTION  : Determines if a message-handler
                   can be deleted
  INPUTS       : 1) Address of the handler's class
                 2) Index of the handler
  RETURNS      : TRUE if deletable, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle int IsDefmessageHandlerDeletable(
  void *ptr,
  unsigned index)
  {
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(ptr)
#pragma unused(index)
#endif

#if BLOAD_ONLY || RUN_TIME
   return(FALSE);
#else
   DEFCLASS *cls;

#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded())
     return(FALSE);
#endif
   cls = (DEFCLASS *) ptr;
   if (cls->handlers[index-1].system == 1)
     return(FALSE);
   return((HandlersExecuting(cls) == FALSE) ? TRUE : FALSE);
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
globle void UndefmessageHandlerCommand()
  {
#if RUN_TIME || BLOAD_ONLY
   PrintErrorID("MSGCOM",3,FALSE);
   PrintRouter(WERROR,"Unable to delete message-handlers.\n");
#else
   SYMBOL_HN *mname;
   char *tname;
   DATA_OBJECT tmp;
   DEFCLASS *cls;

#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded())
     {
      PrintErrorID("MSGCOM",3,FALSE);
      PrintRouter(WERROR,"Unable to delete message-handlers.\n");
      return;
     }
#endif
   if (ArgTypeCheck("undefmessage-handler",1,SYMBOL,&tmp) == FALSE)
     return;
   cls = LookupDefclassByMdlOrScope(DOToString(tmp));
   if ((cls == NULL) ? (strcmp(DOToString(tmp),"*") != 0) : FALSE)
     {
      ClassExistError("undefmessage-handler",DOToString(tmp));
      return;
     }
   if (ArgTypeCheck("undefmessage-handler",2,SYMBOL,&tmp) == FALSE)
     return;
   mname = (SYMBOL_HN *) tmp.value;
   if (RtnArgCount() == 3)
     {
      if (ArgTypeCheck("undefmessage-handler",3,SYMBOL,&tmp) == FALSE)
        return;
      tname = DOToString(tmp);
      if (strcmp(tname,"*") == 0)
        tname = NULL;
     }
   else
     tname = hndquals[MPRIMARY];
   WildDeleteHandler(cls,mname,tname);
#endif
  }

/***********************************************************
  NAME         : UndefmessageHandler
  DESCRIPTION  : Deletes a handler from a class
  INPUTS       : 1) Class address    (Can be NULL)
                 2) Handler index (can be 0)
  RETURNS      : 1 if successful, 0 otherwise
  SIDE EFFECTS : Handler deleted if possible
  NOTES        : None
 ***********************************************************/
globle int UndefmessageHandler(
  void *vptr,
  unsigned mhi)
  {
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(vptr)
#pragma unused(mhi)
#endif

#if RUN_TIME || BLOAD_ONLY
   PrintErrorID("MSGCOM",3,FALSE);
   PrintRouter(WERROR,"Unable to delete message-handlers.\n");
   return(0);
#else
   DEFCLASS *cls;

#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded())
     {
      PrintErrorID("MSGCOM",3,FALSE);
      PrintRouter(WERROR,"Unable to delete message-handlers.\n");
      return(0);
     }
#endif
   if (vptr == NULL)
     {
      if (mhi != 0)
        {
         PrintErrorID("MSGCOM",1,FALSE);
         PrintRouter(WERROR,"Incomplete message-handler specification for deletion.\n");
         return(0);
        }
      return(WildDeleteHandler(NULL,NULL,NULL));
     }
   if (mhi == 0)
     return(WildDeleteHandler((DEFCLASS *) vptr,NULL,NULL));
   cls = (DEFCLASS *) vptr;
   if (HandlersExecuting(cls))
     {
      HandlerDeleteError(GetDefclassName((void *) cls));
      return(0);
     }
   cls->handlers[mhi-1].mark = 1;
   DeallocateMarkedHandlers(cls);
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
globle void PPDefmessageHandlerCommand()
  {
   DATA_OBJECT temp;
   SYMBOL_HN *csym,*msym;
   char *tname;
   DEFCLASS *cls = NULL;
   int mtype;
   HANDLER *hnd;

   if (ArgTypeCheck("ppdefmessage-handler",1,SYMBOL,&temp) == FALSE)
     return;
   csym = FindSymbol(DOToString(temp));
   if (ArgTypeCheck("ppdefmessage-handler",2,SYMBOL,&temp) == FALSE)
     return;
   msym = FindSymbol(DOToString(temp));
   if (RtnArgCount() == 3)
     {
      if (ArgTypeCheck("ppdefmessage-handler",3,SYMBOL,&temp) == FALSE)
        return;
      tname = DOToString(temp);
     }
   else
     tname = hndquals[MPRIMARY];
   mtype = HandlerType("ppdefmessage-handler",tname);
   if (mtype == MERROR)
     {
      SetEvaluationError(TRUE);
      return;
     }
   if (csym != NULL)
     cls = LookupDefclassByMdlOrScope(ValueToString(csym));
   if (((cls == NULL) || (msym == NULL)) ? TRUE :
       ((hnd = FindHandlerByAddress(cls,msym,(unsigned) mtype)) == NULL))
     {
      PrintErrorID("MSGCOM",2,FALSE);
      PrintRouter(WERROR,"Unable to find message-handler ");
      PrintRouter(WERROR,ValueToString(msym));
      PrintRouter(WERROR," ");
      PrintRouter(WERROR,tname);
      PrintRouter(WERROR," for class ");
      PrintRouter(WERROR,ValueToString(csym));
      PrintRouter(WERROR," in function ppdefmessage-handler.\n");
      SetEvaluationError(TRUE);
      return;
     }
   if (hnd->ppForm != NULL)
     PrintInChunks(WDISPLAY,hnd->ppForm);
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
globle void ListDefmessageHandlersCommand()
  {
   int inhp;
   void *clsptr;

   if (RtnArgCount() == 0)
     ListDefmessageHandlers(WDISPLAY,NULL,0);
   else
     {
      clsptr = ClassInfoFnxArgs("list-defmessage-handlers",&inhp);
      if (clsptr == NULL)
        return;
      ListDefmessageHandlers(WDISPLAY,clsptr,inhp);
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
globle void PreviewSendCommand()
  {
   DEFCLASS *cls;
   DATA_OBJECT temp;

   /* =============================
      Get the class for the message
      ============================= */
   if (ArgTypeCheck("preview-send",1,SYMBOL,&temp) == FALSE)
     return;
   cls = LookupDefclassByMdlOrScope(DOToString(temp));
   if (cls == NULL)
     {
      ClassExistError("preview-send",ValueToString(temp.value));
      return;
     }

   if (ArgTypeCheck("preview-send",2,SYMBOL,&temp) == FALSE)
     return;
   PreviewSend(WDISPLAY,(void *) cls,DOToString(temp));
  }

/********************************************************
  NAME         : GetDefmessageHandlerPPForm
  DESCRIPTION  : Gets a message-handler pretty print form
  INPUTS       : 1) Address of the handler's class
                 2) Index of the handler
  RETURNS      : TRUE if printable, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ********************************************************/
globle char *GetDefmessageHandlerPPForm(
  void *ptr,
  unsigned index)
  {
   return(((DEFCLASS *) ptr)->handlers[index-1].ppForm);
  }

/*******************************************************************
  NAME         : ListDefmessageHandlers
  DESCRIPTION  : Lists message-handlers for a class
  INPUTS       : 1) The logical name of the output
                 2) Class name (NULL to display all handlers)
                 3) A flag indicating whether to list inherited
                    handlers or not
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 *******************************************************************/
globle void ListDefmessageHandlers(
  char *log,
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
        cnt = DisplayHandlersInLinks(log,&cls->allSuperclasses,0);
      else
        {
         plinks.classCount = 1;
         plinks.classArray = &cls;
         cnt = DisplayHandlersInLinks(log,&plinks,0);
        }
     }
   else
     {
      plinks.classCount = 1;
      cnt = 0L;
      for (cls = (DEFCLASS *) GetNextDefclass(NULL) ;
           cls != NULL ;
           cls = (DEFCLASS *) GetNextDefclass((void *) cls))
        {
         plinks.classArray = &cls;
         cnt += DisplayHandlersInLinks(log,&plinks,0);
        }
     }
   PrintTally(log,cnt,"message-handler","message-handlers");
  }

/********************************************************************
  NAME         : PreviewSend
  DESCRIPTION  : Displays a list of the core for a message describing
                   shadows,etc.
  INPUTS       : 1) Logical name of output
                 2) Class pointer
                 3) Message name-string
  RETURNS      : Nothing useful
  SIDE EFFECTS : Temporary core created and destroyed
  NOTES        : None
 ********************************************************************/
globle void PreviewSend(
  char *logicalName,
  void *clsptr,
  char *msgname)
  {
   HANDLER_LINK *core;
   SYMBOL_HN *msym;

   msym = FindSymbol(msgname);
   if (msym == NULL)
     return;
   core = FindPreviewApplicableHandlers((DEFCLASS *) clsptr,msym);
   if (core != NULL)
     {
      DisplayCore(logicalName,core,0);
      DestroyHandlerLinks(core);
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
  char *log,
  PACKED_CLASS_LINKS *plinks,
  unsigned index)
  {
   register unsigned i;
   long cnt;

   cnt = (long) plinks->classArray[index]->handlerCount;
   if (index < (plinks->classCount - 1))
     cnt += DisplayHandlersInLinks(log,plinks,index + 1);
   for (i = 0 ; i < plinks->classArray[index]->handlerCount ; i++)
     PrintHandler(log,&plinks->classArray[index]->handlers[i],TRUE);
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
static void CreateSystemHandlers()
  {
   NewSystemHandler(USER_TYPE_NAME,INIT_STRING,"init-slots",0);
   NewSystemHandler(USER_TYPE_NAME,DELETE_STRING,"delete-instance",0);

#if DEBUGGING_FUNCTIONS
   NewSystemHandler(USER_TYPE_NAME,PRINT_STRING,"ppinstance",0);
#endif

   NewSystemHandler(USER_TYPE_NAME,DIRECT_MODIFY_STRING,"(direct-modify)",1);
   NewSystemHandler(USER_TYPE_NAME,MSG_MODIFY_STRING,"(message-modify)",1);
   NewSystemHandler(USER_TYPE_NAME,DIRECT_DUPLICATE_STRING,"(direct-duplicate)",2);
   NewSystemHandler(USER_TYPE_NAME,MSG_DUPLICATE_STRING,"(message-duplicate)",2);
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
  DEFCLASS *cls,
  SYMBOL_HN *msym,
  char *tname)
  {
   int mtype;

   if (msym == NULL)
     msym = (SYMBOL_HN *) AddSymbol("*");
   if (tname != NULL)
     {
      mtype = HandlerType("undefmessage-handler",tname);
      if (mtype == MERROR)
        return(0);
     }
   else
     mtype = -1;
   if (cls == NULL)
     {
      int success = 1;

      for (cls = (DEFCLASS *) GetNextDefclass(NULL) ;
           cls != NULL ;
           cls = (DEFCLASS *) GetNextDefclass((void *) cls))
        if (DeleteHandler(cls,msym,mtype,FALSE) == 0)
          success = 0;
      return(success);
     }
   return(DeleteHandler(cls,msym,mtype,TRUE));
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
#if IBM_TBC
#pragma argsused
#endif
static BOOLEAN DefmessageHandlerWatchAccess(
  int code,
  int newState,
  EXPRESSION *argExprs)
  {
#if MAC_MPW || MAC_MCW || IBM_MCW
#pragma unused(code)
#endif
   return(DefmessageHandlerWatchSupport(newState ? "watch" : "unwatch",NULL,newState,
                                        NULL,SetDefmessageHandlerWatch,argExprs));
  }

/***********************************************************************
  NAME         : DefclassWatchPrint
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
#if IBM_TBC
#pragma argsused
#endif
static BOOLEAN DefmessageHandlerWatchPrint(
  char *log,
  int code,
  EXPRESSION *argExprs)
  {
#if MAC_MPW || MAC_MCW || IBM_MCW
#pragma unused(code)
#endif
   return(DefmessageHandlerWatchSupport("list-watch-items",log,-1,
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
static BOOLEAN DefmessageHandlerWatchSupport(
  char *funcName,
  char *log,
  int newState,
  void (*printFunc)(char *,void *,unsigned),
  void (*traceFunc)(int,void *,unsigned),
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
      SaveCurrentModule();
      theModule = (struct defmodule *) GetNextDefmodule(NULL);
      while (theModule != NULL)
        {
         SetCurrentModule((void *) theModule);
         if (traceFunc == NULL)
           {
            PrintRouter(log,GetDefmoduleName((void *) theModule));
            PrintRouter(log,":\n");
           }
         theClass = GetNextDefclass(NULL);
         while (theClass != NULL)
            {
             if (WatchClassHandlers(theClass,NULL,-1,log,newState,
                                    TRUE,printFunc,traceFunc) == FALSE)
                 return(FALSE);
             theClass = GetNextDefclass(theClass);
            }
          theModule = (struct defmodule *) GetNextDefmodule((void *) theModule);
         }
      RestoreCurrentModule();
      return(TRUE);
     }

   /* ================================================
      Set or show the traces for the specified handler
      ================================================ */
   while (argExprs != NULL)
     {
      if (EvaluateExpression(argExprs,&tmpData))
        return(FALSE);
      if (tmpData.type != SYMBOL)
        {
         ExpectedTypeError1(funcName,argIndex,"class name");
         return(FALSE);
        }
      theClass = (void *) LookupDefclassByMdlOrScope(DOToString(tmpData));
      if (theClass == NULL)
        {
         ExpectedTypeError1(funcName,argIndex,"class name");
         return(FALSE);
        }
      if (GetNextArgument(argExprs) != NULL)
        {
         argExprs = GetNextArgument(argExprs);
         argIndex++;
         if (EvaluateExpression(argExprs,&tmpData))
           return(FALSE);
         if (tmpData.type != SYMBOL)
           {
            ExpectedTypeError1(funcName,argIndex,"handler name");
            return(FALSE);
           }
         theHandlerStr = DOToString(tmpData);
         if (GetNextArgument(argExprs) != NULL)
           {
            argExprs = GetNextArgument(argExprs);
            argIndex++;
            if (EvaluateExpression(argExprs,&tmpData))
              return(FALSE);
            if (tmpData.type != SYMBOL)
              {
               ExpectedTypeError1(funcName,argIndex,"handler type");
               return(FALSE);
              }
            if ((theType = HandlerType(funcName,DOToString(tmpData))) == MERROR)
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
      if (WatchClassHandlers(theClass,theHandlerStr,theType,log,
                             newState,FALSE,printFunc,traceFunc) == FALSE)
        {
         ExpectedTypeError1(funcName,argIndex,"handler");
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
static BOOLEAN WatchClassHandlers(
  void *theClass,
  char *theHandlerStr,
  int theType,
  char *log,
  int newState,
  int indentp,
  void (*printFunc)(char *,void *,unsigned),
  void (*traceFunc)(int,void *,unsigned))
  {
   unsigned theHandler;
   int found = FALSE;

   theHandler = GetNextDefmessageHandler(theClass,0);
   while (theHandler != 0)
     {
      if ((theType == -1) ? TRUE :
          (theType == ((DEFCLASS *) theClass)->handlers[theHandler-1].type))
        {
         if ((theHandlerStr == NULL) ? TRUE :
             (strcmp(theHandlerStr,GetDefmessageHandlerName(theClass,theHandler)) == 0))
            {
             if (traceFunc != NULL)
               (*traceFunc)(newState,theClass,theHandler);
             else
               {
                if (indentp)
                  PrintRouter(log,"   ");
                (*printFunc)(log,theClass,theHandler);
               }
             found = TRUE;
            }
        }
      theHandler = GetNextDefmessageHandler(theClass,theHandler);
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
  char *log,
  void *theClass,
  unsigned theHandler)
  {
   PrintRouter(log,GetDefclassName(theClass));
   PrintRouter(log," ");
   PrintRouter(log,GetDefmessageHandlerName(theClass,theHandler));
   PrintRouter(log," ");
   PrintRouter(log,GetDefmessageHandlerType(theClass,theHandler));
   PrintRouter(log,GetDefmessageHandlerWatch(theClass,theHandler) ? " = on\n" : " = off\n");
  }

#endif /* DEBUGGING_FUNCTIONS */
#endif

/***************************************************
  NAME         :
  DESCRIPTION  :
  INPUTS       :
  RETURNS      :
  SIDE EFFECTS :
  NOTES        :
 ***************************************************/


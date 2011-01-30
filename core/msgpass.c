   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/09/97          */
   /*                                                     */
   /*              OBJECT MESSAGE DISPATCH CODE           */
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

#ifndef _STDIO_INCLUDED_
#include <stdio.h>
#define _STDIO_INCLUDED_
#endif
#include <stdlib.h>

#include "argacces.h"
#include "classcom.h"
#include "classfun.h"
#include "memalloc.h"
#include "constrct.h"
#include "exprnpsr.h"
#include "insfun.h"
#include "msgfun.h"
#include "multifld.h"
#include "prcdrfun.h"
#include "prccode.h"
#include "proflfun.h"
#include "router.h"
#include "utility.h"
#include "commline.h"

#define _MSGPASS_SOURCE_
#include "msgpass.h"

#include "inscom.h"

/* =========================================
   *****************************************
                   CONSTANTS
   =========================================
   ***************************************** */

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

static void PerformMessage(DATA_OBJECT *,EXPRESSION *,SYMBOL_HN *);
static HANDLER_LINK *FindApplicableHandlers(DEFCLASS *,SYMBOL_HN *);
static void CallHandlers(DATA_OBJECT *);
static void EarlySlotBindError(INSTANCE_TYPE *,DEFCLASS *,unsigned);

/* =========================================
   *****************************************
      EXTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */
Thread globle SYMBOL_HN *CurrentMessageName = NULL;
Thread globle HANDLER_LINK *CurrentCore = NULL;

/* =========================================
   *****************************************
      INTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */
Thread static HANDLER_LINK *TopOfCore = NULL, *NextInCore = NULL;

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/*****************************************************
  NAME         : DirectMessage
  DESCRIPTION  : Plugs in given instance and
                  performs specified message
  INPUTS       : 1) Message symbolic name
                 2) The instance address
                 3) Address of DATA_OBJECT buffer
                    (NULL if don't care)
                 4) Message argument expressions
  RETURNS      : Nothing useful
  SIDE EFFECTS : Side effects of message execution
  NOTES        : None
 *****************************************************/
globle void DirectMessage(
  SYMBOL_HN *msg,
  INSTANCE_TYPE *ins,
  DATA_OBJECT *resultbuf,
  EXPRESSION *remargs)
  {
   EXPRESSION args;
   DATA_OBJECT temp;

   if (resultbuf == NULL)
     resultbuf = &temp;
   args.nextArg = remargs;
   args.argList = NULL;
   args.type = INSTANCE_ADDRESS;
   args.value = (void *) ins;
   PerformMessage(resultbuf,&args,msg);
  }

/***************************************************
  NAME         : Send
  DESCRIPTION  : C Interface for sending messages to
                  instances
  INPUTS       : 1) The data object of the instance
                 2) The message name-string
                 3) The message arguments string
                    (Constants only)
                 4) Caller's buffer for result
  RETURNS      : Nothing useful
  SIDE EFFECTS : Executes message and stores result
                   caller's buffer
  NOTES        : None
 ***************************************************/
globle void Send(
  DATA_OBJECT *idata,
  char *msg,
  char *args,
  DATA_OBJECT *result)
  {
   int error;
   EXPRESSION *iexp;
   SYMBOL_HN *msym;

   SetEvaluationError(FALSE);
   result->type = SYMBOL;
   result->value = FalseSymbol;
   msym = FindSymbol(msg);
   if (msym == NULL)
     {
      PrintNoHandlerError(msg);
      SetEvaluationError(TRUE);
      return;
     }
   iexp = GenConstant(idata->type,idata->value);
   iexp->nextArg = ParseConstantArguments(args,&error);
   if (error == TRUE)
     {
      ReturnExpression(iexp);
      SetEvaluationError(TRUE);
      return;
     }
   PerformMessage(result,iexp,msym);
   ReturnExpression(iexp);

   if ((CurrentEvaluationDepth == 0) && (! EvaluatingTopLevelCommand) &&
       (CurrentExpression == NULL))
     { PeriodicCleanup(TRUE,FALSE); }
  }

/*****************************************************
  NAME         : DestroyHandlerLinks
  DESCRIPTION  : Iteratively deallocates handler-links
  INPUTS       : The handler-link list
  RETURNS      : Nothing useful
  SIDE EFFECTS : Deallocation of links
  NOTES        : None
 *****************************************************/
globle void DestroyHandlerLinks(
  HANDLER_LINK *mhead)
  {
   HANDLER_LINK *tmp;

   while (mhead != NULL)
     {
      tmp = mhead;
      mhead = mhead->nxt;
      tmp->hnd->busy--;
      DecrementDefclassBusyCount((void *) tmp->hnd->cls);
      rtn_struct(messageHandlerLink,tmp);
     }
  }

/***********************************************************************
  NAME         : SendCommand
  DESCRIPTION  : Determines the applicable handler(s) and sets up the
                   core calling frame.  Then calls the core frame.
  INPUTS       : Caller's space for storing the result of the handler(s)
  RETURNS      : Nothing useful
  SIDE EFFECTS : Any side-effects caused by the execution of handlers in
                   the core framework
  NOTES        : H/L Syntax : (send <instance> <hnd> <args>*)
 ***********************************************************************/
globle void SendCommand(
  DATA_OBJECT *result)
  {
   EXPRESSION args;
   SYMBOL_HN *msg;
   DATA_OBJECT temp;

   result->type = SYMBOL;
   result->value = FalseSymbol;
   if (ArgTypeCheck("send",2,SYMBOL,&temp) == FALSE)
     return;
   msg = (SYMBOL_HN *) temp.value;

   /* =============================================
      Get the instance or primitive for the message
      ============================================= */
   args.type = GetFirstArgument()->type;
   args.value = GetFirstArgument()->value;
   args.argList = GetFirstArgument()->argList;
   args.nextArg = GetFirstArgument()->nextArg->nextArg;

   PerformMessage(result,&args,msg);
  }

/***************************************************
  NAME         : GetNthMessageArgument
  DESCRIPTION  : Returns the address of the nth
                 (starting at 1) which is an
                 argument of the current message
                 dispatch
  INPUTS       : None
  RETURNS      : The message argument
  SIDE EFFECTS : None
  NOTES        : The active instance is always
                 stored as the first argument (0) in
                 the call frame of the message
 ***************************************************/
globle DATA_OBJECT *GetNthMessageArgument(
  int n)
  {
   return(&ProcParamArray[n]);
  }

#if IMPERATIVE_MESSAGE_HANDLERS

/*****************************************************
  NAME         : NextHandlerAvailable
  DESCRIPTION  : Determines if there the currently
                   executing handler can call a
                   shadowed handler
                 Used before calling call-next-handler
  INPUTS       : None
  RETURNS      : TRUE if shadow ready, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : H/L Syntax: (next-handlerp)
 *****************************************************/
globle int NextHandlerAvailable()
  {
   if (CurrentCore == NULL)
     return(FALSE);
   if (CurrentCore->hnd->type == MAROUND)
     return((NextInCore != NULL) ? TRUE : FALSE);
   if ((CurrentCore->hnd->type == MPRIMARY) && (NextInCore != NULL))
     return((NextInCore->hnd->type == MPRIMARY) ? TRUE : FALSE);
   return(FALSE);
  }

/********************************************************
  NAME         : CallNextHandler
  DESCRIPTION  : This function allows around-handlers
                   to execute the rest of the core frame.
                 It also allows primary handlers
                   to execute shadowed primaries.

                 The original handler arguments are
                   left intact.
  INPUTS       : The caller's result-value buffer
  RETURNS      : Nothing useful
  SIDE EFFECTS : The core frame is called and any
                   appropriate changes are made when
                   used in an around handler
                   See CallHandlers()
                 But when call-next-handler is called
                   from a primary, the same shadowed
                   primary is called over and over
                   again for repeated calls to
                   call-next-handler.
  NOTES        : H/L Syntax: (call-next-handler) OR
                    (override-next-handler <arg> ...)
 ********************************************************/
globle void CallNextHandler(
  DATA_OBJECT *result)
  {
   EXPRESSION args;
   int overridep;
   HANDLER_LINK *oldNext,*oldCurrent;
#if PROFILING_FUNCTIONS
   struct profileFrameInfo profileFrame;
#endif


   SetpType(result,SYMBOL);
   SetpValue(result,FalseSymbol);
   EvaluationError = FALSE;
   if (HaltExecution)
     return;
   if (NextHandlerAvailable() == FALSE)
     {
      PrintErrorID("MSGPASS",1,FALSE);
      PrintRouter(WERROR,"Shadowed message-handlers not applicable in current context.\n");
      SetEvaluationError(TRUE);
      return;
     }
   if (CurrentExpression->value == (void *) FindFunction("override-next-handler"))
     {
      overridep = 1;
      args.type = (short) ProcParamArray[0].type;
      if (args.type != MULTIFIELD)
        args.value = (void *) ProcParamArray[0].value;
      else
        args.value = (void *) &ProcParamArray[0];
      args.nextArg = GetFirstArgument();
      args.argList = NULL;
      PushProcParameters(&args,CountArguments(&args),
                          ValueToString(CurrentMessageName),"message",
                          UnboundHandlerErr);
      if (EvaluationError)
        {
         ReturnFlag = FALSE;
         return;
        }
     }
   else
     overridep = 0;
   oldNext = NextInCore;
   oldCurrent = CurrentCore;
   if (CurrentCore->hnd->type == MAROUND)
     {
      if (NextInCore->hnd->type == MAROUND)
        {
         CurrentCore = NextInCore;
         NextInCore = NextInCore->nxt;
#if DEBUGGING_FUNCTIONS
         if (CurrentCore->hnd->trace)
           WatchHandler(WTRACE,CurrentCore,BEGIN_TRACE);
#endif
         if (CheckHandlerArgCount())
           {
#if PROFILING_FUNCTIONS
            StartProfile(&profileFrame,
                         &CurrentCore->hnd->usrData,
                         ProfileConstructs);
#endif

            EvaluateProcActions(CurrentCore->hnd->cls->header.whichModule->theModule,
                               CurrentCore->hnd->actions,
                               CurrentCore->hnd->localVarCount,
                               result,UnboundHandlerErr);
#if PROFILING_FUNCTIONS
            EndProfile(&profileFrame);
#endif
           }
#if DEBUGGING_FUNCTIONS
         if (CurrentCore->hnd->trace)
           WatchHandler(WTRACE,CurrentCore,END_TRACE);
#endif
        }
      else
        CallHandlers(result);
     }
   else
     {
      CurrentCore = NextInCore;
      NextInCore = NextInCore->nxt;
#if DEBUGGING_FUNCTIONS
      if (CurrentCore->hnd->trace)
        WatchHandler(WTRACE,CurrentCore,BEGIN_TRACE);
#endif
      if (CheckHandlerArgCount())
        {
#if PROFILING_FUNCTIONS
        StartProfile(&profileFrame,
                     &CurrentCore->hnd->usrData,
                     ProfileConstructs);
#endif

        EvaluateProcActions(CurrentCore->hnd->cls->header.whichModule->theModule,
                            CurrentCore->hnd->actions,
                            CurrentCore->hnd->localVarCount,
                            result,UnboundHandlerErr);
#if PROFILING_FUNCTIONS
         EndProfile(&profileFrame);
#endif
        }

#if DEBUGGING_FUNCTIONS
      if (CurrentCore->hnd->trace)
        WatchHandler(WTRACE,CurrentCore,END_TRACE);
#endif
     }
   NextInCore = oldNext;
   CurrentCore = oldCurrent;
   if (overridep)
     PopProcParameters();
   ReturnFlag = FALSE;
  }

#endif /* IMPERATIVE_MESSAGE_HANDLERS */

/*************************************************************************
  NAME         : FindApplicableOfName
  DESCRIPTION  : Groups all handlers of all types of the specified
                   class of the specified name into the applicable handler
                   list
  INPUTS       : 1) The class address
                 2-3) The tops and bottoms of the four handler type lists:
                      around, before, primary and after
                 4) The message name symbol
  RETURNS      : Nothing useful
  SIDE EFFECTS : Modifies the handler lists to include applicable handlers
  NOTES        : None
 *************************************************************************/
globle void FindApplicableOfName(
  DEFCLASS *cls,
  HANDLER_LINK *tops[4],
  HANDLER_LINK *bots[4],
  SYMBOL_HN *mname)
  {
   register int i,e;
   HANDLER *hnd;
   unsigned *arr;
   HANDLER_LINK *tmp;

   i = FindHandlerNameGroup(cls,mname);
   if (i == -1)
     return;
   e = cls->handlerCount-1;
   hnd = cls->handlers;
   arr = cls->handlerOrderMap;
   for ( ; i <= e ; i++)
     {
      if (hnd[arr[i]].name != mname)
        break;
#if ! IMPERATIVE_MESSAGE_HANDLERS
      if ((hnd[arr[i]].type == MPRIMARY) && (tops[MPRIMARY] != NULL))
        continue;
#endif
      tmp = get_struct(messageHandlerLink);
      hnd[arr[i]].busy++;
      IncrementDefclassBusyCount((void *) hnd[arr[i]].cls);
      tmp->hnd = &hnd[arr[i]];
      if (tops[tmp->hnd->type] == NULL)
        {
         tmp->nxt = NULL;
         tops[tmp->hnd->type] = bots[tmp->hnd->type] = tmp;
        }
#if AUXILIARY_MESSAGE_HANDLERS
      else if (tmp->hnd->type == MAFTER)
        {
         tmp->nxt = tops[tmp->hnd->type];
         tops[tmp->hnd->type] = tmp;
        }
#endif
      else
        {
         bots[tmp->hnd->type]->nxt = tmp;
         bots[tmp->hnd->type] = tmp;
         tmp->nxt = NULL;
        }
     }
  }

/*************************************************************************
  NAME         : JoinHandlerLinks
  DESCRIPTION  : Joins the queues of different handlers together
  INPUTS       : 1-2) The tops and bottoms of the four handler type lists:
                      around, before, primary and after
                 3) The message name symbol
  RETURNS      : The top of the joined lists, NULL on errors
  SIDE EFFECTS : Links all the handler type lists together, or all the
                   lists are destroyed if there are no primary handlers
  NOTES        : None
 *************************************************************************/
globle HANDLER_LINK *JoinHandlerLinks(
  HANDLER_LINK *tops[4],
  HANDLER_LINK *bots[4],
  SYMBOL_HN *mname)
  {
   register int i;
   HANDLER_LINK *mlink;

   if (tops[MPRIMARY] == NULL)
    {
     PrintNoHandlerError(ValueToString(mname));
     for (i = MAROUND ; i <= MAFTER ; i++)
       DestroyHandlerLinks(tops[i]);
     SetEvaluationError(TRUE);
     return(NULL);
    }

   mlink = tops[MPRIMARY];

#if AUXILIARY_MESSAGE_HANDLERS
   if (tops[MBEFORE] != NULL)
     {
      bots[MBEFORE]->nxt = mlink;
      mlink = tops[MBEFORE];
     }
#endif

#if IMPERATIVE_MESSAGE_HANDLERS
   if (tops[MAROUND] != NULL)
     {
      bots[MAROUND]->nxt = mlink;
      mlink = tops[MAROUND];
     }
#endif

#if AUXILIARY_MESSAGE_HANDLERS
   bots[MPRIMARY]->nxt = tops[MAFTER];
#endif

   return(mlink);
  }

/***************************************************
  NAME         : PrintHandlerSlotGetFunction
  DESCRIPTION  : Developer access function for
                 printing direct slot references
                 in message-handlers
  INPUTS       : 1) The logical name of the output
                 2) The bitmap expression
  RETURNS      : Nothing useful
  SIDE EFFECTS : Expression printed
  NOTES        : None
 ***************************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle void PrintHandlerSlotGetFunction(
  char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   HANDLER_SLOT_REFERENCE *theReference;
   DEFCLASS *theDefclass;
   SLOT_DESC *sd;

   theReference = (HANDLER_SLOT_REFERENCE *) ValueToBitMap(theValue);
   PrintRouter(logicalName,"?self:[");
   theDefclass = ClassIDMap[theReference->classID];
   PrintRouter(logicalName,ValueToString(theDefclass->header.name));
   PrintRouter(logicalName,"]");
   sd = theDefclass->instanceTemplate[theDefclass->slotNameMap[theReference->slotID]];
   PrintRouter(logicalName,ValueToString(sd->slotName->name));
#else
#if MAC_MPW || MAC_MCW
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/***************************************************
  NAME         : HandlerSlotGetFunction
  DESCRIPTION  : Access function for handling the
                 statically-bound direct slot
                 references in message-handlers
  INPUTS       : 1) The bitmap expression
                 2) A data object buffer
  RETURNS      : TRUE if OK, FALSE
                 on errors
  SIDE EFFECTS : Data object buffer gets value of
                 slot. On errors, buffer gets
                 symbol FALSE, EvaluationError
                 is set and error messages are
                 printed
  NOTES        : It is possible for a handler
                 (attached to a superclass of
                  the currently active instance)
                 containing these static references
                 to be called for an instance
                 which does not contain the slots
                 (e.g., an instance of a subclass
                  where the original slot was
                  no-inherit or the subclass
                  overrode the original slot)
 ***************************************************/
globle BOOLEAN HandlerSlotGetFunction(
  void *theValue,
  DATA_OBJECT *theResult)
  {
   HANDLER_SLOT_REFERENCE *theReference;
   DEFCLASS *theDefclass;
   INSTANCE_TYPE *theInstance;
   INSTANCE_SLOT *sp;
   unsigned instanceSlotIndex;

   theReference = (HANDLER_SLOT_REFERENCE *) ValueToBitMap(theValue);
   theInstance = (INSTANCE_TYPE *) ProcParamArray[0].value;
   theDefclass = ClassIDMap[theReference->classID];

   if (theInstance->garbage)
     {
      StaleInstanceAddress("for slot get",0);
      theResult->type = SYMBOL;
      theResult->value = FalseSymbol;
      SetEvaluationError(TRUE);
      return(FALSE);
     }

   if (theInstance->cls == theDefclass)
     {
      instanceSlotIndex = theInstance->cls->slotNameMap[theReference->slotID];
      sp = theInstance->slotAddresses[instanceSlotIndex - 1];
     }
   else
     {
      if (theReference->slotID > theInstance->cls->maxSlotNameID)
        goto HandlerGetError;
      instanceSlotIndex = theInstance->cls->slotNameMap[theReference->slotID];
      if (instanceSlotIndex == 0)
        goto HandlerGetError;
      instanceSlotIndex--;
      sp = theInstance->slotAddresses[instanceSlotIndex];
      if (sp->desc->cls != theDefclass)
        goto HandlerGetError;
     }
   theResult->type = sp->type;
   theResult->value = sp->value;
   if (sp->type == MULTIFIELD)
     {
      theResult->begin = 0;
      theResult->end = GetInstanceSlotLength(sp) - 1;
     }
   return(TRUE);

HandlerGetError:
   EarlySlotBindError(theInstance,theDefclass,theReference->slotID);
   theResult->type = SYMBOL;
   theResult->value = FalseSymbol;
   SetEvaluationError(TRUE);
   return(FALSE);
  }

/***************************************************
  NAME         : PrintHandlerSlotPutFunction
  DESCRIPTION  : Developer access function for
                 printing direct slot bindings
                 in message-handlers
  INPUTS       : 1) The logical name of the output
                 2) The bitmap expression
  RETURNS      : Nothing useful
  SIDE EFFECTS : Expression printed
  NOTES        : None
 ***************************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle void PrintHandlerSlotPutFunction(
  char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   HANDLER_SLOT_REFERENCE *theReference;
   DEFCLASS *theDefclass;
   SLOT_DESC *sd;

   theReference = (HANDLER_SLOT_REFERENCE *) ValueToBitMap(theValue);
   PrintRouter(logicalName,"(bind ?self:[");
   theDefclass = ClassIDMap[theReference->classID];
   PrintRouter(logicalName,ValueToString(theDefclass->header.name));
   PrintRouter(logicalName,"]");
   sd = theDefclass->instanceTemplate[theDefclass->slotNameMap[theReference->slotID]];
   PrintRouter(logicalName,ValueToString(sd->slotName->name));
   if (GetFirstArgument() != NULL)
     {
      PrintRouter(logicalName," ");
      PrintExpression(logicalName,GetFirstArgument());
     }
   PrintRouter(logicalName,")");
#else
#if MAC_MPW || MAC_MCW
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/***************************************************
  NAME         : HandlerSlotPutFunction
  DESCRIPTION  : Access function for handling the
                 statically-bound direct slot
                 bindings in message-handlers
  INPUTS       : 1) The bitmap expression
                 2) A data object buffer
  RETURNS      : TRUE if OK, FALSE
                 on errors
  SIDE EFFECTS : Data object buffer gets symbol
                 TRUE and slot is set. On errors,
                 buffer gets symbol FALSE,
                 EvaluationError is set and error
                 messages are printed
  NOTES        : It is possible for a handler
                 (attached to a superclass of
                  the currently active instance)
                 containing these static references
                 to be called for an instance
                 which does not contain the slots
                 (e.g., an instance of a subclass
                  where the original slot was
                  no-inherit or the subclass
                  overrode the original slot)
 ***************************************************/
globle BOOLEAN HandlerSlotPutFunction(
  void *theValue,
  DATA_OBJECT *theResult)
  {
   HANDLER_SLOT_REFERENCE *theReference;
   DEFCLASS *theDefclass;
   INSTANCE_TYPE *theInstance;
   INSTANCE_SLOT *sp;
   unsigned instanceSlotIndex;
   DATA_OBJECT theSetVal;

   theReference = (HANDLER_SLOT_REFERENCE *) ValueToBitMap(theValue);
   theInstance = (INSTANCE_TYPE *) ProcParamArray[0].value;
   theDefclass = ClassIDMap[theReference->classID];

   if (theInstance->garbage)
     {
      StaleInstanceAddress("for slot put",0);
      theResult->type = SYMBOL;
      theResult->value = FalseSymbol;
      SetEvaluationError(TRUE);
      return(FALSE);
     }

   if (theInstance->cls == theDefclass)
     {
      instanceSlotIndex = theInstance->cls->slotNameMap[theReference->slotID];
      sp = theInstance->slotAddresses[instanceSlotIndex - 1];
     }
   else
     {
      if (theReference->slotID > theInstance->cls->maxSlotNameID)
        goto HandlerPutError;
      instanceSlotIndex = theInstance->cls->slotNameMap[theReference->slotID];
      if (instanceSlotIndex == 0)
        goto HandlerPutError;
      instanceSlotIndex--;
      sp = theInstance->slotAddresses[instanceSlotIndex];
      if (sp->desc->cls != theDefclass)
        goto HandlerPutError;
     }

   /* =======================================================
      The slot has already been verified not to be read-only.
      However, if it is initialize-only, we need to make sure
      that we are initializing the instance (something we
      could not verify at parse-time)
      ======================================================= */
   if (sp->desc->initializeOnly && (!theInstance->initializeInProgress))
     {
      SlotAccessViolationError(ValueToString(sp->desc->slotName->name),
                               TRUE,(void *) theInstance);
      goto HandlerPutError2;
     }

   /* ======================================
      No arguments means to use the
      special NoParamValue to reset the slot
      to its default value
      ====================================== */
   if (GetFirstArgument())
     {
      if (EvaluateAndStoreInDataObject((int) sp->desc->multiple,
                                       GetFirstArgument(),&theSetVal) == FALSE)
         goto HandlerPutError2;
     }
   else
     {
      SetDOBegin(theSetVal,1);
      SetDOEnd(theSetVal,0);
      SetType(theSetVal,MULTIFIELD);
      SetValue(theSetVal,NoParamValue);
     }
   if (PutSlotValue(theInstance,sp,&theSetVal,theResult,NULL) == FALSE)
      goto HandlerPutError2;
   return(TRUE);

HandlerPutError:
   EarlySlotBindError(theInstance,theDefclass,theReference->slotID);

HandlerPutError2:
   theResult->type = SYMBOL;
   theResult->value = FalseSymbol;
   SetEvaluationError(TRUE);

   return(FALSE);
  }

/*****************************************************
  NAME         : DynamicHandlerGetSlot
  DESCRIPTION  : Directly references a slot's value
                 (uses dynamic binding to lookup slot)
  INPUTS       : The caller's result buffer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Caller's result buffer set
  NOTES        : H/L Syntax: (get <slot>)
 *****************************************************/
globle void DynamicHandlerGetSlot(
  DATA_OBJECT *result)
  {
   INSTANCE_SLOT *sp;
   INSTANCE_TYPE *ins;
   DATA_OBJECT temp;

   result->type = SYMBOL;
   result->value = FalseSymbol;
   if (CheckCurrentMessage("dynamic-get",TRUE) == FALSE)
     return;
   EvaluateExpression(GetFirstArgument(),&temp);
   if (temp.type != SYMBOL)
     {
      ExpectedTypeError1("dynamic-get",1,"symbol");
      SetEvaluationError(TRUE);
      return;
     }
   ins = GetActiveInstance();
   sp = FindInstanceSlot(ins,(SYMBOL_HN *) temp.value);
   if (sp == NULL)
     {
      SlotExistError(ValueToString(temp.value),"dynamic-get");
      return;
     }
   if ((sp->desc->publicVisibility == 0) &&
       (CurrentCore->hnd->cls != sp->desc->cls))
     {
      SlotVisibilityViolationError(sp->desc,CurrentCore->hnd->cls);
      SetEvaluationError(TRUE);
      return;
     }
   result->type = sp->type;
   result->value = sp->value;
   if (sp->type == MULTIFIELD)
     {
      result->begin = 0;
      result->end = GetInstanceSlotLength(sp) - 1;
     }
  }

/***********************************************************
  NAME         : DynamicHandlerPutSlot
  DESCRIPTION  : Directly puts a slot's value
                 (uses dynamic binding to lookup slot)
  INPUTS       : Data obejct buffer for holding slot value
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot modified - and caller's buffer set
                 to value (or symbol FALSE on errors)
  NOTES        : H/L Syntax: (put <slot> <value>*)
 ***********************************************************/
globle void DynamicHandlerPutSlot(
  DATA_OBJECT *theResult)
  {
   INSTANCE_SLOT *sp;
   INSTANCE_TYPE *ins;
   DATA_OBJECT temp;

   theResult->type = SYMBOL;
   theResult->value = FalseSymbol;
   if (CheckCurrentMessage("dynamic-put",TRUE) == FALSE)
     return;
   EvaluateExpression(GetFirstArgument(),&temp);
   if (temp.type != SYMBOL)
     {
      ExpectedTypeError1("dynamic-put",1,"symbol");
      SetEvaluationError(TRUE);
      return;
     }
   ins = GetActiveInstance();
   sp = FindInstanceSlot(ins,(SYMBOL_HN *) temp.value);
   if (sp == NULL)
     {
      SlotExistError(ValueToString(temp.value),"dynamic-put");
      return;
     }
   if ((sp->desc->noWrite == 0) ? FALSE :
       ((sp->desc->initializeOnly == 0) || (!ins->initializeInProgress)))
     {
      SlotAccessViolationError(ValueToString(sp->desc->slotName->name),
                               TRUE,(void *) ins);
      SetEvaluationError(TRUE);
      return;
     }
   if ((sp->desc->publicVisibility == 0) &&
       (CurrentCore->hnd->cls != sp->desc->cls))
     {
      SlotVisibilityViolationError(sp->desc,CurrentCore->hnd->cls);
      SetEvaluationError(TRUE);
      return;
     }
   if (GetFirstArgument()->nextArg)
     {
      if (EvaluateAndStoreInDataObject((int) sp->desc->multiple,
                        GetFirstArgument()->nextArg,&temp) == FALSE)
        return;
     }
   else
     {
      SetpDOBegin(&temp,1);
      SetpDOEnd(&temp,0);
      SetpType(&temp,MULTIFIELD);
      SetpValue(&temp,NoParamValue);
     }
   PutSlotValue(ins,sp,&temp,theResult,NULL);
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/*****************************************************
  NAME         : PerformMessage
  DESCRIPTION  : Calls core framework for a message
  INPUTS       : 1) Caller's result buffer
                 2) Message argument expressions
                    (including implicit object)
                 3) Message name
  RETURNS      : Nothing useful
  SIDE EFFECTS : Any side-effects of message execution
                    and caller's result buffer set
  NOTES        : None
 *****************************************************/
static void PerformMessage(
  DATA_OBJECT *result,
  EXPRESSION *args,
  SYMBOL_HN *mname)
  {
   int oldce;
   HANDLER_LINK *oldCore;
   DEFCLASS *cls = NULL;
   INSTANCE_TYPE *ins = NULL;
   SYMBOL_HN *oldName;
#if PROFILING_FUNCTIONS
   struct profileFrameInfo profileFrame;
#endif

   result->type = SYMBOL;
   result->value = FalseSymbol;
   EvaluationError = FALSE;
   if (HaltExecution)
     return;
   oldce = ExecutingConstruct();
   SetExecutingConstruct(TRUE);
   oldName = CurrentMessageName;
   CurrentMessageName = mname;
   CurrentEvaluationDepth++;
   PushProcParameters(args,CountArguments(args),
                        ValueToString(CurrentMessageName),"message",
                        UnboundHandlerErr);

   if (EvaluationError)
     {
      CurrentEvaluationDepth--;
      CurrentMessageName = oldName;
      PeriodicCleanup(FALSE,TRUE);
      SetExecutingConstruct(oldce);
      return;
     }

   if (ProcParamArray->type == INSTANCE_ADDRESS)
     {
      ins = (INSTANCE_TYPE *) ProcParamArray->value;
      if (ins->garbage == 1)
        {
         StaleInstanceAddress("send",0);
         SetEvaluationError(TRUE);
        }
      else if (DefclassInScope(ins->cls,(struct defmodule *) GetCurrentModule()) == FALSE)
        NoInstanceError(ValueToString(ins->name),"send");
      else
        {
         cls = ins->cls;
         ins->busy++;
        }
     }
   else if (ProcParamArray->type == INSTANCE_NAME)
     {
      ins = FindInstanceBySymbol((SYMBOL_HN *) ProcParamArray->value);
      if (ins == NULL)
        {
         PrintErrorID("MSGPASS",2,FALSE);
         PrintRouter(WERROR,"No such instance ");
         PrintRouter(WERROR,ValueToString((SYMBOL_HN *) ProcParamArray->value));
         PrintRouter(WERROR," in function send.\n");
         SetEvaluationError(TRUE);
        }
      else
        {
         ProcParamArray->value = (void *) ins;
         ProcParamArray->type = INSTANCE_ADDRESS;
         cls = ins->cls;
         ins->busy++;
        }
     }
   else if ((cls = PrimitiveClassMap[ProcParamArray->type]) == NULL)
     {
      SystemError("MSGPASS",1);
      ExitRouter(EXIT_FAILURE);
     }
   if (EvaluationError)
     {
      PopProcParameters();
      CurrentEvaluationDepth--;
      CurrentMessageName = oldName;
      PeriodicCleanup(FALSE,TRUE);
      SetExecutingConstruct(oldce);
      return;
     }

   oldCore = TopOfCore;
   TopOfCore = FindApplicableHandlers(cls,mname);

   if (TopOfCore != NULL)
     {
      HANDLER_LINK *oldCurrent,*oldNext;

      oldCurrent = CurrentCore;
      oldNext = NextInCore;

#if IMPERATIVE_MESSAGE_HANDLERS

      if (TopOfCore->hnd->type == MAROUND)
        {
         CurrentCore = TopOfCore;
         NextInCore = TopOfCore->nxt;
#if DEBUGGING_FUNCTIONS
         if (WatchMessages)
           WatchMessage(WTRACE,BEGIN_TRACE);
         if (CurrentCore->hnd->trace)
           WatchHandler(WTRACE,CurrentCore,BEGIN_TRACE);
#endif
         if (CheckHandlerArgCount())
           {
#if PROFILING_FUNCTIONS
            StartProfile(&profileFrame,
                         &CurrentCore->hnd->usrData,
                         ProfileConstructs);
#endif


           EvaluateProcActions(CurrentCore->hnd->cls->header.whichModule->theModule,
                               CurrentCore->hnd->actions,
                               CurrentCore->hnd->localVarCount,
                               result,UnboundHandlerErr);


#if PROFILING_FUNCTIONS
            EndProfile(&profileFrame);
#endif
           }

#if DEBUGGING_FUNCTIONS
         if (CurrentCore->hnd->trace)
           WatchHandler(WTRACE,CurrentCore,END_TRACE);
         if (WatchMessages)
           WatchMessage(WTRACE,END_TRACE);
#endif
        }
      else

#endif  /* IMPERATIVE_MESSAGE_HANDLERS */

        {
         CurrentCore = NULL;
         NextInCore = TopOfCore;
#if DEBUGGING_FUNCTIONS
         if (WatchMessages)
           WatchMessage(WTRACE,BEGIN_TRACE);
#endif
         CallHandlers(result);
#if DEBUGGING_FUNCTIONS
         if (WatchMessages)
           WatchMessage(WTRACE,END_TRACE);
#endif
        }

      DestroyHandlerLinks(TopOfCore);
      CurrentCore = oldCurrent;
      NextInCore = oldNext;
     }

   TopOfCore = oldCore;
   ReturnFlag = FALSE;

   if (ins != NULL)
     ins->busy--;

   /* ==================================
      Restore the original calling frame
      ================================== */
   PopProcParameters();
   CurrentEvaluationDepth--;
   CurrentMessageName = oldName;
   PropagateReturnValue(result);
   PeriodicCleanup(FALSE,TRUE);
   SetExecutingConstruct(oldce);
   if (EvaluationError)
     {
      result->type = SYMBOL;
      result->value = FalseSymbol;
     }
  }

/*****************************************************************************
  NAME         : FindApplicableHandlers
  DESCRIPTION  : Given a message name, this routine forms the "core frame"
                   for the message : a list of all applicable class handlers.
                   An applicable class handler is one whose name matches
                     the message and whose class matches the instance.

                   The list is in the following order :

                   All around handlers (from most specific to most general)
                   All before handlers (from most specific to most general)
                   All primary handlers (from most specific to most general)
                   All after handlers (from most general to most specific)

  INPUTS       : 1) The class of the instance (or primitive) for the message
                 2) The message name
  RETURNS      : NULL if no applicable handlers or errors,
                   the list of handlers otherwise
  SIDE EFFECTS : Links are allocated for the list
  NOTES        : The instance is the first thing on the ProcParamArray
                 The number of arguments is in ProcParamArraySize
 *****************************************************************************/
static HANDLER_LINK *FindApplicableHandlers(
  DEFCLASS *cls,
  SYMBOL_HN *mname)
  {
   register int i;
   HANDLER_LINK *tops[4],*bots[4];

   for (i = MAROUND ; i <= MAFTER ; i++)
     tops[i] = bots[i] = NULL;

   for (i = 0 ; i < cls->allSuperclasses.classCount ; i++)
     FindApplicableOfName(cls->allSuperclasses.classArray[i],tops,bots,mname);
   return(JoinHandlerLinks(tops,bots,mname));
  }

/***************************************************************
  NAME         : CallHandlers
  DESCRIPTION  : Moves though the current message frame
                   for a send-message as follows :

                 Call all before handlers and ignore their
                   return values.
                 Call the first primary handler and
                   ignore the rest.  The return value
                   of the handler frame is this message's value.
                 Call all after handlers and ignore their
                   return values.
  INPUTS       : Caller's buffer for the return value of
                   the message
  RETURNS      : Nothing useful
  SIDE EFFECTS : The handlers are evaluated.
  NOTES        : IMPORTANT : The global NextInCore should be
                 pointing to the first handler to be executed.
 ***************************************************************/
static void CallHandlers(
  DATA_OBJECT *result)
  {
   HANDLER_LINK *oldCurrent,*oldNext;
#if AUXILIARY_MESSAGE_HANDLERS
   DATA_OBJECT temp;
#endif
#if PROFILING_FUNCTIONS
   struct profileFrameInfo profileFrame;
#endif

   if (HaltExecution)
     return;

#if AUXILIARY_MESSAGE_HANDLERS
   oldCurrent = CurrentCore;
   oldNext = NextInCore;

   while (NextInCore->hnd->type == MBEFORE)
     {
      CurrentCore = NextInCore;
      NextInCore = NextInCore->nxt;
#if DEBUGGING_FUNCTIONS
      if (CurrentCore->hnd->trace)
        WatchHandler(WTRACE,CurrentCore,BEGIN_TRACE);
#endif
      if (CheckHandlerArgCount())
        {
#if PROFILING_FUNCTIONS
         StartProfile(&profileFrame,
                      &CurrentCore->hnd->usrData,
                      ProfileConstructs);
#endif

         EvaluateProcActions(CurrentCore->hnd->cls->header.whichModule->theModule,
                            CurrentCore->hnd->actions,
                            CurrentCore->hnd->localVarCount,
                            &temp,UnboundHandlerErr);


#if PROFILING_FUNCTIONS
         EndProfile(&profileFrame);
#endif
        }

#if DEBUGGING_FUNCTIONS
      if (CurrentCore->hnd->trace)
        WatchHandler(WTRACE,CurrentCore,END_TRACE);
#endif
      ReturnFlag = FALSE;
      if ((NextInCore == NULL) || HaltExecution)
        {
         NextInCore = oldNext;
         CurrentCore = oldCurrent;
         return;
        }
     }
   if (NextInCore->hnd->type == MPRIMARY)
     {
#endif /* AUXILIARY_MESSAGE_HANDLERS */

      CurrentCore = NextInCore;
      NextInCore = NextInCore->nxt;
#if DEBUGGING_FUNCTIONS
      if (CurrentCore->hnd->trace)
        WatchHandler(WTRACE,CurrentCore,BEGIN_TRACE);
#endif
      if (CheckHandlerArgCount())
        {
#if PROFILING_FUNCTIONS
         StartProfile(&profileFrame,
                      &CurrentCore->hnd->usrData,
                      ProfileConstructs);
#endif


        EvaluateProcActions(CurrentCore->hnd->cls->header.whichModule->theModule,
                            CurrentCore->hnd->actions,
                            CurrentCore->hnd->localVarCount,
                            result,UnboundHandlerErr);

#if PROFILING_FUNCTIONS
         EndProfile(&profileFrame);
#endif
        }


#if DEBUGGING_FUNCTIONS
      if (CurrentCore->hnd->trace)
        WatchHandler(WTRACE,CurrentCore,END_TRACE);
#endif
      ReturnFlag = FALSE;

#if AUXILIARY_MESSAGE_HANDLERS
      if ((NextInCore == NULL) || HaltExecution)
        {
         NextInCore = oldNext;
         CurrentCore = oldCurrent;
         return;
        }
      while (NextInCore->hnd->type == MPRIMARY)
        {
         NextInCore = NextInCore->nxt;
         if (NextInCore == NULL)
           {
            NextInCore = oldNext;
            CurrentCore = oldCurrent;
            return;
           }
        }
     }
   while (NextInCore->hnd->type == MAFTER)
     {
      CurrentCore = NextInCore;
      NextInCore = NextInCore->nxt;
#if DEBUGGING_FUNCTIONS
      if (CurrentCore->hnd->trace)
        WatchHandler(WTRACE,CurrentCore,BEGIN_TRACE);
#endif
      if (CheckHandlerArgCount())
        {
#if PROFILING_FUNCTIONS
         StartProfile(&profileFrame,
                      &CurrentCore->hnd->usrData,
                      ProfileConstructs);
#endif


         EvaluateProcActions(CurrentCore->hnd->cls->header.whichModule->theModule,
                            CurrentCore->hnd->actions,
                            CurrentCore->hnd->localVarCount,
                            &temp,UnboundHandlerErr);

#if PROFILING_FUNCTIONS
         EndProfile(&profileFrame);
#endif
        }


#if DEBUGGING_FUNCTIONS
      if (CurrentCore->hnd->trace)
        WatchHandler(WTRACE,CurrentCore,END_TRACE);
#endif
      ReturnFlag = FALSE;
      if ((NextInCore == NULL) || HaltExecution)
        {
         NextInCore = oldNext;
         CurrentCore = oldCurrent;
         return;
        }
     }
#endif /*  AUXILIARY_MESSAGE_HANDLERS */

   NextInCore = oldNext;
   CurrentCore = oldCurrent;
  }


/********************************************************
  NAME         : EarlySlotBindError
  DESCRIPTION  : Prints out an error message when
                 a message-handler from a superclass
                 which contains a static-bind
                 slot access is not valid for the
                 currently active instance (i.e.
                 the instance is not using the
                 superclass's slot)
  INPUTS       : 1) The currently active instance
                 2) The defclass holding the invalid slot
                 3) The canonical id of the slot
  RETURNS      : Nothing useful
  SIDE EFFECTS : Error message printed
  NOTES        : None
 ********************************************************/
static void EarlySlotBindError(
  INSTANCE_TYPE *theInstance,
  DEFCLASS *theDefclass,
  unsigned slotID)
  {
   SLOT_DESC *sd;

   sd = theDefclass->instanceTemplate[theDefclass->slotNameMap[slotID] - 1];
   PrintErrorID("MSGPASS",3,FALSE);
   PrintRouter(WERROR,"Static reference to slot ");
   PrintRouter(WERROR,ValueToString(sd->slotName->name));
   PrintRouter(WERROR," of class ");
   PrintClassName(WERROR,theDefclass,FALSE);
   PrintRouter(WERROR," does not apply to ");
   PrintInstanceNameAndClass(WERROR,theInstance,TRUE);
  }

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
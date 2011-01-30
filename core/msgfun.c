   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/09/97          */
   /*                                                     */
   /*                  OBJECT MESSAGE FUNCTIONS           */
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

#include "classcom.h"
#include "classfun.h"
#include "memalloc.h"
#include "extnfunc.h"
#include "insfun.h"
#include "prccode.h"
#include "router.h"

#define _MSGFUN_SOURCE_
#include "msgfun.h"

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

#if DEBUGGING_FUNCTIONS
static HANDLER_LINK *DisplayPrimaryCore(char *,HANDLER_LINK *,int);
static void PrintPreviewHandler(char *,HANDLER_LINK *,int,char *);
#endif

/* =========================================
   *****************************************
      EXTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */
Thread globle SYMBOL_HN *INIT_SYMBOL = NULL,
       globle           *DELETE_SYMBOL = NULL;

#if IBM_TBC
#pragma warn -ias
#endif
Thread globle char *hndquals[] = {"around","before","primary","after"};
#if IBM_TBC
#pragma warn +ias
#endif

#if DEBUGGING_FUNCTIONS
Thread globle int WatchHandlers = OFF;
Thread globle int WatchMessages = OFF;
#endif

/* =========================================
   *****************************************
      INTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/********************************************************
  NAME         : UnboundHandlerErr
  DESCRIPTION  : Print out a synopis of the currently
                   executing handler for unbound variable
                   errors
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Error synopsis printed to WERROR
  NOTES        : None
 ********************************************************/
globle void UnboundHandlerErr()
  {
   PrintRouter(WERROR,"message-handler ");
   PrintHandler(WERROR,CurrentCore->hnd,TRUE);
  }

/*****************************************************************
  NAME         : PrintNoHandlerError
  DESCRIPTION  : Print "No primaries found" error message for send
  INPUTS       : The name of the message
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 *****************************************************************/
globle void PrintNoHandlerError(
  char *msg)
  {
   PrintErrorID("MSGFUN",1,FALSE);
   PrintRouter(WERROR,"No applicable primary message-handlers found for ");
   PrintRouter(WERROR,msg);
   PrintRouter(WERROR,".\n");
  }

/***************************************************************
  NAME         : CheckHandlerArgCount
  DESCRIPTION  : Verifies that the current argument
                   list satisfies the current
                   handler's parameter count restriction
  INPUTS       : None
  RETURNS      : TRUE if all OK, FALSE otherwise
  SIDE EFFECTS : EvaluationError set on errors
  NOTES        : Uses ProcParamArraySize and CurrentCore globals
 ***************************************************************/
globle int CheckHandlerArgCount()
  {
   HANDLER *hnd;

   hnd = CurrentCore->hnd;
   if ((hnd->maxParams == -1) ? (ProcParamArraySize < hnd->minParams) :
       (ProcParamArraySize != hnd->minParams))
     {
      SetEvaluationError(TRUE);
      PrintErrorID("MSGFUN",2,FALSE);
      PrintRouter(WERROR,"Message-handler ");
      PrintRouter(WERROR,ValueToString(hnd->name));
      PrintRouter(WERROR," ");
      PrintRouter(WERROR,hndquals[hnd->type]);
      PrintRouter(WERROR," in class ");
      PrintRouter(WERROR,GetDefclassName((void *) hnd->cls));
      PrintRouter(WERROR," expected ");
      PrintRouter(WERROR,(hnd->maxParams == -1) ? "at least " : "exactly ");
      PrintLongInteger(WERROR,(long) (hnd->minParams-1));
      PrintRouter(WERROR," argument(s).\n");
      return(FALSE);
     }
   return(TRUE);
  }

/***************************************************
  NAME         : SlotAccessViolationError
  DESCRIPTION  : Prints out an error message when
                 attempt is made to set a read-only
                 or initialize-only slot improperly
  INPUTS       : 1) The slot name
                 2) A flag indicating if the source
                    is a class or an instance
                 3) A pointer to the source
                    instance/class
  RETURNS      : Nothing useful
  SIDE EFFECTS : Error message printed
  NOTES        : None
 ***************************************************/
globle void SlotAccessViolationError(
  char *slotName,
  BOOLEAN instanceFlag,
  void *theInstanceOrClass)
  {
   PrintErrorID("MSGFUN",3,FALSE);
   PrintRouter(WERROR,slotName);
   PrintRouter(WERROR," slot in ");
   if (instanceFlag)
     PrintInstanceNameAndClass(WERROR,(INSTANCE_TYPE *) theInstanceOrClass,FALSE);
   else
     {
      PrintRouter(WERROR,"class ");
      PrintClassName(WERROR,(DEFCLASS *) theInstanceOrClass,FALSE);
     }
   PrintRouter(WERROR,": write access denied.\n");
  }

/***************************************************
  NAME         : SlotVisibilityViolationError
  DESCRIPTION  : Prints out an error message when
                 attempt is made to access a
                 private slot improperly
  INPUTS       : 1) The slot descriptor
                 2) A pointer to the source class
  RETURNS      : Nothing useful
  SIDE EFFECTS : Error message printed
  NOTES        : None
 ***************************************************/
globle void SlotVisibilityViolationError(
  SLOT_DESC *sd,
  DEFCLASS *theDefclass)
  {
   PrintErrorID("MSGFUN",6,FALSE);
   PrintRouter(WERROR,"Private slot ");
   PrintRouter(WERROR,ValueToString(sd->slotName->name));
   PrintRouter(WERROR," of class ");
   PrintClassName(WERROR,sd->cls,FALSE);
   PrintRouter(WERROR," cannot be accessed directly\n   by handlers attached to class ");
   PrintClassName(WERROR,theDefclass,TRUE);
  }

#if ! RUN_TIME

/******************************************************************************
  NAME         : NewSystemHandler
  DESCRIPTION  : Adds a new system handler for a system class

                 The handler is assumed to be primary and of
                 the form:

                 (defmessage-handler <class> <handler> () (<func>))

  INPUTS       : 1) Name-string of the system class
                 2) Name-string of the system handler
                 3) Name-string of the internal H/L function to implement
                      this handler
                 4) The number of extra arguments (past the instance itself)
                    that the handler willl accept
  RETURNS      : Nothing useful
  SIDE EFFECTS : Creates the new handler and inserts it in the system class's
                   handler array
                 On errors, generate a system error and exits.
  NOTES        : Does not check to see if handler already exists
 *******************************************************************************/
globle void NewSystemHandler(
  char *cname,
  char *mname,
  char *fname,
  int extraargs)
  {
   DEFCLASS *cls;
   HANDLER *hnd;

   cls = LookupDefclassInScope(cname);
   hnd = InsertHandlerHeader(cls,(SYMBOL_HN *) AddSymbol(mname),MPRIMARY);
   IncrementSymbolCount(hnd->name);
   hnd->system = 1;
   hnd->minParams = hnd->maxParams = extraargs + 1;
   hnd->localVarCount = 0;
   hnd->actions = get_struct(expr);
   hnd->actions->argList = NULL;
   hnd->actions->type = FCALL;
   hnd->actions->value = (void *) FindFunction(fname);
   hnd->actions->nextArg = NULL;
  }

/***************************************************
  NAME         : InsertHandlerHeader
  DESCRIPTION  : Allocates a new handler header and
                   inserts it in the proper (sorted)
                   position in the class hnd array
  INPUTS       : 1) The class
                 2) The handler name
                 3) The handler type
  RETURNS      : The address of the new handler
                   header, NULL on errors
  SIDE EFFECTS : Class handler array reallocated
                   and resorted
  NOTES        : Assumes handler does not exist
 ***************************************************/
globle HANDLER *InsertHandlerHeader(
  DEFCLASS *cls,
  SYMBOL_HN *mname,
  int mtype)
  {
   HANDLER *nhnd,*hnd;
   unsigned *narr,*arr;
   register int i,j,ni = -1;

   hnd = cls->handlers;
   arr = cls->handlerOrderMap;
   nhnd = (HANDLER *) gm2((int) (sizeof(HANDLER) * (cls->handlerCount+1)));
   narr = (unsigned *) gm2((int) (sizeof(unsigned) * (cls->handlerCount+1)));
   GenCopyMemory(HANDLER,cls->handlerCount,nhnd,hnd);
   for (i = 0 , j = 0 ; i < cls->handlerCount ; i++ , j++)
     {
      if (ni == -1)
        {
         if ((hnd[arr[i]].name->bucket > mname->bucket) ? TRUE :
             (hnd[arr[i]].name == mname))
           {
            ni = i;
            j++;
           }
        }
      narr[j] = arr[i];
     }
   if (ni == -1)
     ni = cls->handlerCount;
   narr[ni] = cls->handlerCount;
   nhnd[cls->handlerCount].system = 0;
   nhnd[cls->handlerCount].type = mtype;
   nhnd[cls->handlerCount].busy = 0;
   nhnd[cls->handlerCount].mark = 0;
#if DEBUGGING_FUNCTIONS
   nhnd[cls->handlerCount].trace = WatchHandlers;
#endif
   nhnd[cls->handlerCount].name = mname;
   nhnd[cls->handlerCount].cls = cls;
   nhnd[cls->handlerCount].minParams = 0;
   nhnd[cls->handlerCount].maxParams = 0;
   nhnd[cls->handlerCount].localVarCount = 0;
   nhnd[cls->handlerCount].actions = NULL;
   nhnd[cls->handlerCount].ppForm = NULL;
   nhnd[cls->handlerCount].usrData = NULL;
   if (cls->handlerCount != 0)
     {
      rm((void *) hnd,(int) (sizeof(HANDLER) * cls->handlerCount));
      rm((void *) arr,(int) (sizeof(unsigned) * cls->handlerCount));
     }
   cls->handlers = nhnd;
   cls->handlerOrderMap = narr;
   cls->handlerCount++;
   return(&nhnd[cls->handlerCount-1]);
  }

#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)

/*****************************************************
  NAME         : HandlersExecuting
  DESCRIPTION  : Determines if any message-handlers
                   for a class are currently executing
  INPUTS       : The class address
  RETURNS      : TRUE if any handlers are executing,
                   FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 *****************************************************/
globle int HandlersExecuting(
  DEFCLASS *cls)
  {
   register unsigned i;

   for (i = 0 ; i < cls->handlerCount ; i++)
     if (cls->handlers[i].busy > 0)
       return(TRUE);
   return(FALSE);
  }

/*********************************************************************
  NAME         : DeleteHandler
  DESCRIPTION  : Deletes one or more message-handlers
                   from a class definition
  INPUTS       : 1) The class address
                 2) The message-handler name
                    (if this is * and there is no handler
                     called *, then the delete operations
                     will be applied to all handlers matching the type
                 3) The message-handler type
                    (if this is -1, then the delete operations will be
                     applied to all handlers matching the name
                 4) A flag saying whether to print error messages when
                     handlers are not found meeting specs
  RETURNS      : 1 if successful, 0 otherwise
  SIDE EFFECTS : Handlers deleted
  NOTES        : If any handlers for the class are
                   currently executing, this routine
                   will fail
 **********************************************************************/
globle int DeleteHandler(
   DEFCLASS *cls,
   SYMBOL_HN *mname,
   int mtype,
   int indicate_missing)
  {
   register int i;
   HANDLER *hnd;
   int found,success = 1;

   if (cls->handlerCount == 0)
     {
      if (indicate_missing)
        {
         HandlerDeleteError(GetDefclassName((void *) cls));
         return(0);
        }
      return(1);
     }
   if (HandlersExecuting(cls))
     {
      HandlerDeleteError(GetDefclassName((void *) cls));
      return(0);
     }
   if (mtype == -1)
     {
      found = FALSE;
      for (i = MAROUND ; i <= MAFTER ; i++)
        {
         hnd = FindHandlerByAddress(cls,mname,(unsigned) i);
         if (hnd != NULL)
           {
            found = TRUE;
            if (hnd->system == 0)
              hnd->mark = 1;
            else
              {
               PrintErrorID("MSGPSR",3,FALSE);
               PrintRouter(WERROR,"System message-handlers may not be modified.\n");
               success = 0;
              }
           }
        }
      if ((found == FALSE) ? (strcmp(ValueToString(mname),"*") == 0) : FALSE)
        {
         for (i = 0 ; i < cls->handlerCount ; i++)
           if (cls->handlers[i].system == 0)
             cls->handlers[i].mark = 1;
        }
     }
   else
     {
      hnd = FindHandlerByAddress(cls,mname,(unsigned) mtype);
      if (hnd == NULL)
        {
         if (strcmp(ValueToString(mname),"*") == 0)
           {
            for (i = 0 ; i < cls->handlerCount ; i++)
              if ((cls->handlers[i].type == mtype) &&
                  (cls->handlers[i].system == 0))
                cls->handlers[i].mark = 1;
           }
         else
           {
            if (indicate_missing)
              HandlerDeleteError(GetDefclassName((void *) cls));
            success = 0;
           }
        }
      else if (hnd->system == 0)
        hnd->mark = 1;
      else
        {
         if (indicate_missing)
           {
            PrintErrorID("MSGPSR",3,FALSE);
            PrintRouter(WERROR,"System message-handlers may not be modified.\n");
           }
         success = 0;
        }
     }
   DeallocateMarkedHandlers(cls);
   return(success);
  }

/***************************************************
  NAME         : DeallocateMarkedHandlers
  DESCRIPTION  : Removes any handlers from a class
                   that have been previously marked
                   for deletion.
  INPUTS       : The class
  RETURNS      : Nothing useful
  SIDE EFFECTS : Marked handlers are deleted
  NOTES        : Assumes none of the handlers are
                   currently executing or have a
                   busy count != 0 for any reason
 ***************************************************/
globle void DeallocateMarkedHandlers(
  DEFCLASS *cls)
  {
   unsigned count;
   HANDLER *hnd,*nhnd;
   unsigned *arr,*narr;
   register int i,j;

   for (i = 0 , count = 0 ; i < cls->handlerCount ; i++)
     {
      hnd = &cls->handlers[i];
      if (hnd->mark == 1)
        {
         count++;
         DecrementSymbolCount(hnd->name);
         ExpressionDeinstall(hnd->actions);
         ReturnPackedExpression(hnd->actions);
         ClearUserDataList(hnd->usrData);
         if (hnd->ppForm != NULL)
           rm((void *) hnd->ppForm,
              (int) (sizeof(char) * (strlen(hnd->ppForm)+1)));
        }
      else
         /* ============================================
            Use the busy field to count how many
            message-handlers are removed before this one
            ============================================ */
        hnd->busy = count;
     }
   if (count == 0)
     return;
   if (count == cls->handlerCount)
     {
      rm((void *) cls->handlers,(int) (sizeof(HANDLER) * cls->handlerCount));
      rm((void *) cls->handlerOrderMap,(int) (sizeof(unsigned) * cls->handlerCount));
      cls->handlers = NULL;
      cls->handlerOrderMap = NULL;
      cls->handlerCount = 0;
     }
   else
     {
      count = cls->handlerCount - count;
      hnd = cls->handlers;
      arr = cls->handlerOrderMap;
      nhnd = (HANDLER *) gm2((int) (sizeof(HANDLER) * count));
      narr = (unsigned *) gm2((int) (sizeof(unsigned) * count));
      for (i = 0 , j = 0 ; j < count ; i++)
        {
         if (hnd[arr[i]].mark == 0)
           {
            /* ==============================================================
               The offsets in the map need to be decremented by the number of
               preceding nodes which were deleted.  Use the value of the busy
               field set in the first loop.
               ============================================================== */
            narr[j] = arr[i] - hnd[arr[i]].busy;
            j++;
           }
        }
      for (i = 0 , j = 0 ; j < count ; i++)
        {
         if (hnd[i].mark == 0)
           {
            hnd[i].busy = 0;
            GenCopyMemory(HANDLER,1,&nhnd[j],&hnd[i]);
            j++;
           }
        }
      rm((void *) hnd,(int) (sizeof(HANDLER) * cls->handlerCount));
      rm((void *) arr,(int) (sizeof(unsigned) * cls->handlerCount));
      cls->handlers = nhnd;
      cls->handlerOrderMap = narr;
      cls->handlerCount = count;
     }
  }

#endif

/*****************************************************
  NAME         : HandlerType
  DESCRIPTION  : Determines type of message-handler
  INPUTS       : 1) Calling function string
                 2) String representing type
  RETURNS      : MAROUND  (0) for "around"
                 MBEFORE  (1) for "before"
                 MPRIMARY (2) for "primary"
                 MAFTER   (3) for "after"
                 MERROR   (4) on errors
  SIDE EFFECTS : None
  NOTES        : None
 *****************************************************/
globle int HandlerType(
  char *func,
  char *str)
  {
   register int i;

   for (i = MAROUND ; i <= MAFTER ; i++)
     if (strcmp(str,hndquals[i]) == 0)
       {
#if ! IMPERATIVE_MESSAGE_HANDLERS
        if (i == MAROUND)
          break;
#endif
#if ! AUXILIARY_MESSAGE_HANDLERS
        if ((i == MBEFORE) || (i == MAFTER))
          break;
#endif
        return(i);
       }

   PrintErrorID("MSGFUN",7,FALSE);
   PrintRouter("werror","Unrecognized message-handler type in ");
   PrintRouter("werror",func);
   PrintRouter("werror",".\n");
   return(MERROR);
  }

/*****************************************************************
  NAME         : CheckCurrentMessage
  DESCRIPTION  : Makes sure that a message is available
                   and active for an internal message function
  INPUTS       : 1) The name of the function checking the message
                 2) A flag indicating whether the object must be
                      a class instance or not (it could be a
                      primitive type)
  RETURNS      : TRUE if all OK, FALSE otherwise
  SIDE EFFECTS : EvaluationError set on errors
  NOTES        : None
 *****************************************************************/
globle int CheckCurrentMessage(
  char *func,
  int ins_reqd)
  {
   register DATA_OBJECT *activeMsgArg;

   if (!CurrentCore || (CurrentCore->hnd->actions != CurrentProcActions))
     {
      PrintErrorID("MSGFUN",4,FALSE);
      PrintRouter(WERROR,func);
      PrintRouter(WERROR," may only be called from within message-handlers.\n");
      SetEvaluationError(TRUE);
      return(FALSE);
     }
   activeMsgArg = GetNthMessageArgument(0);
   if ((ins_reqd == TRUE) ? (activeMsgArg->type != INSTANCE_ADDRESS) : FALSE)
     {
      PrintErrorID("MSGFUN",5,FALSE);
      PrintRouter(WERROR,func);
      PrintRouter(WERROR," operates only on instances.\n");
      SetEvaluationError(TRUE);
      return(FALSE);
     }
   if ((activeMsgArg->type == INSTANCE_ADDRESS) ?
       (((INSTANCE_TYPE *) activeMsgArg->value)->garbage == 1) : FALSE)
     {
      StaleInstanceAddress(func,0);
      SetEvaluationError(TRUE);
      return(FALSE);
     }
   return(TRUE);
  }

/***************************************************
  NAME         : PrintHandler
  DESCRIPTION  : Displays a handler synopsis
  INPUTS       : 1) Logical name of output
                 2) The handler
                 5) Flag indicating whether to
                    printout a terminating newline
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle void PrintHandler(
  char *log,
  HANDLER *theHandler,
  int crtn)
  {
   PrintRouter(log,ValueToString(theHandler->name));
   PrintRouter(log," ");
   PrintRouter(log,hndquals[theHandler->type]);
   PrintRouter(log," in class ");
   PrintClassName(log,theHandler->cls,crtn);
  }

/***********************************************************
  NAME         : FindHandlerByAddress
  DESCRIPTION  : Uses a binary search on a class's
                   handler header array
  INPUTS       : 1) The class address
                 2) The handler symbolic name
                 3) The handler type (MPRIMARY,etc.)
  RETURNS      : The address of the found handler,
                   NULL if not found
  SIDE EFFECTS : None
  NOTES        : Assumes array is in ascending order
                   1st key: symbolic name of handler
                   2nd key: type of handler
 ***********************************************************/
globle HANDLER *FindHandlerByAddress(
  DEFCLASS *cls,
  SYMBOL_HN *name,
  unsigned type)
  {
   register int b,i;
   HANDLER *hnd;
   unsigned *arr;

   if ((b = FindHandlerNameGroup(cls,name)) == -1)
     return(NULL);
   arr = cls->handlerOrderMap;
   hnd = cls->handlers;
   for (i = b ; i < cls->handlerCount ; i++)
     {
      if (hnd[arr[i]].name != name)
        return(NULL);
      if (hnd[arr[i]].type == type)
        return(&hnd[arr[i]]);
     }
   return(NULL);
  }

/***********************************************************
  NAME         : FindHandlerByAddress
  DESCRIPTION  : Uses a binary search on a class's
                   handler header array
  INPUTS       : 1) The class address
                 2) The handler symbolic name
                 3) The handler type (MPRIMARY,etc.)
  RETURNS      : The index of the found handler,
                   -1 if not found
  SIDE EFFECTS : None
  NOTES        : Assumes array is in ascending order
                   1st key: symbolic name of handler
                   2nd key: type of handler
 ***********************************************************/
globle int FindHandlerByIndex(
  DEFCLASS *cls,
  SYMBOL_HN *name,
  unsigned type)
  {
   register int b,i;
   HANDLER *hnd;
   unsigned *arr;

   if ((b = FindHandlerNameGroup(cls,name)) == -1)
     return(-1);
   arr = cls->handlerOrderMap;
   hnd = cls->handlers;
   for (i = b ; i < cls->handlerCount ; i++)
     {
      if (hnd[arr[i]].name != name)
        return(-1);
      if (hnd[arr[i]].type == type)
        return(arr[i]);
     }
   return(-1);
  }

/*****************************************************
  NAME         : FindHandlerNameGroup
  DESCRIPTION  : Uses a binary search on a class's
                   handler header array
  INPUTS       : 1) The class address
                 2) The handler symbolic name
  RETURNS      : The index of the found handler group
                   -1 if not found
  SIDE EFFECTS : None
  NOTES        : Assumes array is in ascending order
                   1st key: handler name symbol bucket
 *****************************************************/
globle int FindHandlerNameGroup(
  DEFCLASS *cls,
  SYMBOL_HN *name)
  {
   register int b,e,i,j;
   HANDLER *hnd;
   unsigned *arr;
   int start;

   if (cls->handlerCount == 0)
     return(-1);
   hnd = cls->handlers;
   arr = cls->handlerOrderMap;
   b = 0;
   e = cls->handlerCount-1;
   start = -1;
   do
     {
      i = (b+e)/2;
      if (name->bucket == hnd[arr[i]].name->bucket)
        {
         for (j = i ; j >= b ; j--)
           {
            if (hnd[arr[j]].name == name)
              start = j;
            if (hnd[arr[j]].name->bucket != name->bucket)
              break;
           }
         if (start != -1)
           return(start);
         for (j = i+1 ; j <= e ; j++)
           {
            if (hnd[arr[j]].name == name)
              return(j);
            if (hnd[arr[j]].name->bucket != name->bucket)
              return(-1);
           }
         return(-1);
        }
      else if (name->bucket < hnd[arr[i]].name->bucket)
        e = i-1;
      else
        b = i+1;
     }
   while (b <= e);
   return(-1);
  }

/***************************************************
  NAME         : HandlerDeleteError
  DESCRIPTION  : Prints out an error message when
                   handlers cannot be deleted
  INPUTS       : Name-string of the class
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle void HandlerDeleteError(
  char *cname)
  {
   PrintErrorID("MSGFUN",8,FALSE);
   PrintRouter(WERROR,"Unable to delete message-handler(s) from class ");
   PrintRouter(WERROR,cname);
   PrintRouter(WERROR,".\n");
  }

#if DEBUGGING_FUNCTIONS

/********************************************************************
  NAME         : DisplayCore
  DESCRIPTION  : Gives a schematic "printout" of the
                   core framework for a message showing
                   arounds, primaries, shadows etc.
                 This routine uses recursion to print indentation
                   to indicate shadowing and where handlers begin
                   and end execution wrt one another.
  INPUTS       : 1) Logical name of output
                 2) The remaining core
                 3) The number of handlers this (partial) core
                    shadows
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : Expects that the core was created in PREVIEW mode,
                   i.e. implicit handlers are SLOT_DESC addresses
                   (in PERFORM mode they are INSTANCE_SLOT addresses)
                 Assumes (partial) core is not empty
 ********************************************************************/
globle void DisplayCore(
  char *logicalName,
  HANDLER_LINK *core,
  int sdepth)
  {
#if IMPERATIVE_MESSAGE_HANDLERS
   if (core->hnd->type == MAROUND)
     {
      PrintPreviewHandler(logicalName,core,sdepth,BEGIN_TRACE);
      if (core->nxt != NULL)
        DisplayCore(logicalName,core->nxt,sdepth+1);
      PrintPreviewHandler(logicalName,core,sdepth,END_TRACE);
     }
   else
#endif
     {
#if AUXILIARY_MESSAGE_HANDLERS
      while ((core != NULL) ? (core->hnd->type == MBEFORE) : FALSE)
        {
         PrintPreviewHandler(logicalName,core,sdepth,BEGIN_TRACE);
         PrintPreviewHandler(logicalName,core,sdepth,END_TRACE);
         core = core->nxt;
        }
      if ((core != NULL) ? (core->hnd->type == MPRIMARY) : FALSE)
#endif
        core = DisplayPrimaryCore(logicalName,core,sdepth);
#if AUXILIARY_MESSAGE_HANDLERS
      while ((core != NULL) ? (core->hnd->type == MAFTER) : FALSE)
        {
         PrintPreviewHandler(logicalName,core,sdepth,BEGIN_TRACE);
         PrintPreviewHandler(logicalName,core,sdepth,END_TRACE);
         core = core->nxt;
        }
#endif
     }
  }

/*******************************************************************
  NAME         : FindPreviewApplicableHandlers
  DESCRIPTION  : See FindApplicableHandlers
                 However, this function only examines classes rather
                   than instances for implicit slot-accessors
  INPUTS       : 1) The class address
                 2) The message name symbol
  RETURNS      : The links of applicable handlers, NULL on errors
  SIDE EFFECTS : Links are allocated for the list
  NOTES        : None
 ******************************************************************/
globle HANDLER_LINK *FindPreviewApplicableHandlers(
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

/***********************************************************
  NAME         : WatchMessage
  DESCRIPTION  : Prints a condensed description of a
                   message and its arguments
  INPUTS       : 1) The output logical name
                 2) BEGIN_TRACE or END_TRACE string
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : Uses the global variables ProcParamArray
                   and CurrentMessageName
 ***********************************************************/
globle void WatchMessage(
  char *log,
  char *tstring)
  {
   PrintRouter(log,"MSG ");
   PrintRouter(log,tstring);
   PrintRouter(log," ");
   PrintRouter(log,ValueToString(CurrentMessageName));
   PrintRouter(log," ED:");
   PrintLongInteger(log,(long) CurrentEvaluationDepth);
   PrintProcParamArray(log);
  }

/***********************************************************
  NAME         : WatchHandler
  DESCRIPTION  : Prints a condensed description of a
                   message-handler and its arguments
  INPUTS       : 1) The output logical name
                 2) The handler address
                 3) BEGIN_TRACE or END_TRACE string
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : Uses the global variables ProcParamArray
                   and CurrentMessageName
 ***********************************************************/
globle void WatchHandler(
  char *log,
  HANDLER_LINK *hndl,
  char *tstring)
  {
   HANDLER *hnd;

   PrintRouter(log,"HND ");
   PrintRouter(log,tstring);
   PrintRouter(log," ");
   hnd = hndl->hnd;
   PrintHandler(WTRACE,hnd,TRUE);
   PrintRouter(log,"       ED:");
   PrintLongInteger(log,(long) CurrentEvaluationDepth);
   PrintProcParamArray(log);
  }

#endif /* DEBUGGING_FUNCTIONS */

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

#if DEBUGGING_FUNCTIONS

/********************************************************************
  NAME         : DisplayPrimaryCore
  DESCRIPTION  : Gives a schematic "printout" of the primary
                   message showing other shadowed primaries
                 This routine uses recursion to print indentation
                   to indicate shadowing and where handlers begin
                   and end execution wrt one another.
  INPUTS       : 1) The logical name of the output
                 2) The remaining core
                 3) The number of handlers this (partial) core
                    shadows
  RETURNS      : The address of the handler following the primary
                   group of handlers in the core
  SIDE EFFECTS : None
  NOTES        : Expects that the core was created in PREVIEW mode,
                   i.e. implicit handlers are SLOT_DESC addresses
                   (in PERFORM mode they are INSTANCE_SLOT addresses)
                 Assumes (partial) core is not empty
 ********************************************************************/
static HANDLER_LINK *DisplayPrimaryCore(
  char *logicalName,
  HANDLER_LINK *core,
  int pdepth)
  {
   register HANDLER_LINK *rtn;

   PrintPreviewHandler(logicalName,core,pdepth,BEGIN_TRACE);
#if IMPERATIVE_MESSAGE_HANDLERS
   if ((core->nxt != NULL) ? (core->nxt->hnd->type == MPRIMARY) : FALSE)
     rtn = DisplayPrimaryCore(logicalName,core->nxt,pdepth+1);
   else
#endif
     rtn = core->nxt;
   PrintPreviewHandler(logicalName,core,pdepth,END_TRACE);
   return(rtn);
  }

/***************************************************
  NAME         : PrintPreviewHandler
  DESCRIPTION  : Displays a message preview
  INPUTS       : 1) The logical name of the output
                 2) Handler-link
                 3) Number of handlers shadowed
                 4) The trace-string
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
static void PrintPreviewHandler(
  char *logicalName,
  HANDLER_LINK *cptr,
  int sdepth,
  char *tstr)
  {
   register int i;

   for (i = 0 ; i < sdepth ; i++)
     PrintRouter(logicalName,"| ");
   PrintRouter(logicalName,tstr);
   PrintRouter(logicalName," ");
   PrintHandler(logicalName,cptr->hnd,TRUE);
  }

#endif

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
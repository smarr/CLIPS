   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*              CLIPS Version 6.24  05/17/06           */
   /*                                                     */
   /*           INSTANCE MODIFY AND DUPLICATE MODULE      */
   /*******************************************************/

/*************************************************************/
/* Purpose:  Instance modify and duplicate support routines  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/*                                                           */
/* Revision History:                                         */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*            Changed name of variable exp to theExp         */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Converted INSTANCE_PATTERN_MATCHING to         */
/*            DEFRULE_CONSTRUCT.                             */
/*                                                           */
/*************************************************************/

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "setup.h"

#if OBJECT_SYSTEM

#if DEFRULE_CONSTRUCT
#include "network.h"
#include "objrtmch.h"
#endif

#include "argacces.h"
#include "memalloc.h"
#include "envrnmnt.h"
#include "extnfunc.h"
#include "inscom.h"
#include "insfun.h"
#include "insmngr.h"
#include "inspsr.h"
#include "miscfun.h"
#include "msgcom.h"
#include "msgfun.h"
#include "msgpass.h"
#include "prccode.h"
#include "router.h"

#define _INSMODDP_SOURCE_
#include "insmoddp.h"

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

static DATA_OBJECT *EvaluateSlotOverrides(void *,EXEC_STATUS,EXPRESSION *,int *,int *);
static void DeleteSlotOverrideEvaluations(void *,EXEC_STATUS,DATA_OBJECT *,int);
static void ModifyMsgHandlerSupport(void *,EXEC_STATUS,DATA_OBJECT *,int);
static void DuplicateMsgHandlerSupport(void *,EXEC_STATUS,DATA_OBJECT *,int);

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

#if (! RUN_TIME)

/***************************************************
  NAME         : SetupInstanceModDupCommands
  DESCRIPTION  : Defines function interfaces for
                 modify- and duplicate- instance
                 functions
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Functions defined to KB
  NOTES        : None
 ***************************************************/
globle void SetupInstanceModDupCommands(
  void *theEnv,
  EXEC_STATUS)
  {
#if DEFRULE_CONSTRUCT
   EnvDefineFunction2(theEnv,execStatus,"modify-instance",'u',PTIEF InactiveModifyInstance,"InactiveModifyInstance",NULL);
   EnvDefineFunction2(theEnv,execStatus,"active-modify-instance",'u',PTIEF ModifyInstance,"ModifyInstance",NULL);
   AddFunctionParser(theEnv,execStatus,"active-modify-instance",ParseInitializeInstance);
   EnvDefineFunction2(theEnv,execStatus,"message-modify-instance",'u',PTIEF InactiveMsgModifyInstance,
                   "InactiveMsgModifyInstance",NULL);
   EnvDefineFunction2(theEnv,execStatus,"active-message-modify-instance",'u',PTIEF MsgModifyInstance,
                   "MsgModifyInstance",NULL);
   AddFunctionParser(theEnv,execStatus,"active-message-modify-instance",ParseInitializeInstance);

   EnvDefineFunction2(theEnv,execStatus,"duplicate-instance",'u',
                    PTIEF InactiveDuplicateInstance,"InactiveDuplicateInstance",NULL);
   EnvDefineFunction2(theEnv,execStatus,"active-duplicate-instance",'u',PTIEF DuplicateInstance,"DuplicateInstance",NULL);
   AddFunctionParser(theEnv,execStatus,"active-duplicate-instance",ParseInitializeInstance);
   EnvDefineFunction2(theEnv,execStatus,"message-duplicate-instance",'u',PTIEF InactiveMsgDuplicateInstance,
                   "InactiveMsgDuplicateInstance",NULL);
   EnvDefineFunction2(theEnv,execStatus,"active-message-duplicate-instance",'u',PTIEF MsgDuplicateInstance,
                   "MsgDuplicateInstance",NULL);
   AddFunctionParser(theEnv,execStatus,"active-message-duplicate-instance",ParseInitializeInstance);
#else
   EnvDefineFunction2(theEnv,execStatus,"modify-instance",'u',PTIEF ModifyInstance,"ModifyInstance",NULL);
   EnvDefineFunction2(theEnv,execStatus,"message-modify-instance",'u',PTIEF MsgModifyInstance,
                   "MsgModifyInstance",NULL);
   EnvDefineFunction2(theEnv,execStatus,"duplicate-instance",'u',PTIEF DuplicateInstance,"DuplicateInstance",NULL);
   EnvDefineFunction2(theEnv,execStatus,"message-duplicate-instance",'u',PTIEF MsgDuplicateInstance,
                   "MsgDuplicateInstance",NULL);
#endif

   EnvDefineFunction2(theEnv,execStatus,"(direct-modify)",'u',PTIEF DirectModifyMsgHandler,"DirectModifyMsgHandler",NULL);
   EnvDefineFunction2(theEnv,execStatus,"(message-modify)",'u',PTIEF MsgModifyMsgHandler,"MsgModifyMsgHandler",NULL);
   EnvDefineFunction2(theEnv,execStatus,"(direct-duplicate)",'u',PTIEF DirectDuplicateMsgHandler,"DirectDuplicateMsgHandler",NULL);
   EnvDefineFunction2(theEnv,execStatus,"(message-duplicate)",'u',PTIEF MsgDuplicateMsgHandler,"MsgDuplicateMsgHandler",NULL);

   AddFunctionParser(theEnv,execStatus,"modify-instance",ParseInitializeInstance);
   AddFunctionParser(theEnv,execStatus,"message-modify-instance",ParseInitializeInstance);
   AddFunctionParser(theEnv,execStatus,"duplicate-instance",ParseInitializeInstance);
   AddFunctionParser(theEnv,execStatus,"message-duplicate-instance",ParseInitializeInstance);
  }

#endif

/*************************************************************
  NAME         : ModifyInstance
  DESCRIPTION  : Modifies slots of an instance via the
                 direct-modify message
  INPUTS       : The address of the result value
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot updates performed directly
  NOTES        : H/L Syntax:
                 (modify-instance <instance> <slot-override>*)
 *************************************************************/
globle void ModifyInstance(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *result)
  {
   INSTANCE_TYPE *ins;
   EXPRESSION theExp;
   DATA_OBJECT *overrides;
   int oldOMDMV,overrideCount,error;

   /* ===========================================
      The slot-overrides need to be evaluated now
      to resolve any variable references before a
      new frame is pushed for message-handler
      execution
      =========================================== */

   overrides = EvaluateSlotOverrides(theEnv,execStatus,GetFirstArgument()->nextArg,
                                     &overrideCount,&error);
   if (error)
     {
      SetpType(result,SYMBOL);
      SetpValue(result,EnvFalseSymbol(theEnv,execStatus));
      return;
     }

   /* ==================================
      Find the instance and make sure it
      wasn't deleted by the overrides
      ================================== */
   ins = CheckInstance(theEnv,execStatus,ValueToString(ExpressionFunctionCallName(execStatus->CurrentExpression)));
   if (ins == NULL)
     {
      SetpType(result,SYMBOL);
      SetpValue(result,EnvFalseSymbol(theEnv,execStatus));
      DeleteSlotOverrideEvaluations(theEnv,execStatus,overrides,overrideCount);
      return;
     }

   /* ======================================
      We are passing the slot override
      expression information along
      to whatever message-handler implements
      the modify
      ====================================== */
   theExp.type = DATA_OBJECT_ARRAY;
   theExp.value = (void *) overrides;
   theExp.argList = NULL;
   theExp.nextArg = NULL;

   oldOMDMV = InstanceData(theEnv,execStatus)->ObjectModDupMsgValid;
   InstanceData(theEnv,execStatus)->ObjectModDupMsgValid = TRUE;
   DirectMessage(theEnv,execStatus,FindSymbolHN(theEnv,execStatus,DIRECT_MODIFY_STRING),ins,result,&theExp);
   InstanceData(theEnv,execStatus)->ObjectModDupMsgValid = oldOMDMV;

   DeleteSlotOverrideEvaluations(theEnv,execStatus,overrides,overrideCount);
  }

/*************************************************************
  NAME         : MsgModifyInstance
  DESCRIPTION  : Modifies slots of an instance via the
                 direct-modify message
  INPUTS       : The address of the result value
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot updates performed with put- messages
  NOTES        : H/L Syntax:
                 (message-modify-instance <instance>
                    <slot-override>*)
 *************************************************************/
globle void MsgModifyInstance(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *result)
  {
   INSTANCE_TYPE *ins;
   EXPRESSION theExp;
   DATA_OBJECT *overrides;
   int oldOMDMV,overrideCount,error;

   /* ===========================================
      The slot-overrides need to be evaluated now
      to resolve any variable references before a
      new frame is pushed for message-handler
      execution
      =========================================== */
   overrides = EvaluateSlotOverrides(theEnv,execStatus,GetFirstArgument()->nextArg,
                                     &overrideCount,&error);
   if (error)
     {
      SetpType(result,SYMBOL);
      SetpValue(result,EnvFalseSymbol(theEnv,execStatus));
      return;
     }

   /* ==================================
      Find the instance and make sure it
      wasn't deleted by the overrides
      ================================== */
   ins = CheckInstance(theEnv,execStatus,ValueToString(ExpressionFunctionCallName(execStatus->CurrentExpression)));
   if (ins == NULL)
     {
      SetpType(result,SYMBOL);
      SetpValue(result,EnvFalseSymbol(theEnv,execStatus));
      DeleteSlotOverrideEvaluations(theEnv,execStatus,overrides,overrideCount);
      return;
     }

   /* ======================================
      We are passing the slot override
      expression information along
      to whatever message-handler implements
      the modify
      ====================================== */
   theExp.type = DATA_OBJECT_ARRAY;
   theExp.value = (void *) overrides;
   theExp.argList = NULL;
   theExp.nextArg = NULL;

   oldOMDMV = InstanceData(theEnv,execStatus)->ObjectModDupMsgValid;
   InstanceData(theEnv,execStatus)->ObjectModDupMsgValid = TRUE;
   DirectMessage(theEnv,execStatus,FindSymbolHN(theEnv,execStatus,MSG_MODIFY_STRING),ins,result,&theExp);
   InstanceData(theEnv,execStatus)->ObjectModDupMsgValid = oldOMDMV;

   DeleteSlotOverrideEvaluations(theEnv,execStatus,overrides,overrideCount);
  }

/*************************************************************
  NAME         : DuplicateInstance
  DESCRIPTION  : Duplicates an instance via the
                 direct-duplicate message
  INPUTS       : The address of the result value
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot updates performed directly
  NOTES        : H/L Syntax:
                 (duplicate-instance <instance>
                   [to <instance-name>] <slot-override>*)
 *************************************************************/
globle void DuplicateInstance(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *result)
  {
   INSTANCE_TYPE *ins;
   DATA_OBJECT newName;
   EXPRESSION theExp[2];
   DATA_OBJECT *overrides;
   int oldOMDMV,overrideCount,error;

   /* ===========================================
      The slot-overrides need to be evaluated now
      to resolve any variable references before a
      new frame is pushed for message-handler
      execution
      =========================================== */
   overrides = EvaluateSlotOverrides(theEnv,execStatus,GetFirstArgument()->nextArg->nextArg,
                                     &overrideCount,&error);
   if (error)
     {
      SetpType(result,SYMBOL);
      SetpValue(result,EnvFalseSymbol(theEnv,execStatus));
      return;
     }

   /* ==================================
      Find the instance and make sure it
      wasn't deleted by the overrides
      ================================== */
   ins = CheckInstance(theEnv,execStatus,ValueToString(ExpressionFunctionCallName(execStatus->CurrentExpression)));
   if (ins == NULL)
     {
      SetpType(result,SYMBOL);
      SetpValue(result,EnvFalseSymbol(theEnv,execStatus));
      DeleteSlotOverrideEvaluations(theEnv,execStatus,overrides,overrideCount);
      return;
     }
   if (EnvArgTypeCheck(theEnv,execStatus,ValueToString(ExpressionFunctionCallName(execStatus->CurrentExpression)),
                    2,INSTANCE_NAME,&newName) == FALSE)
     {
      SetpType(result,SYMBOL);
      SetpValue(result,EnvFalseSymbol(theEnv,execStatus));
      DeleteSlotOverrideEvaluations(theEnv,execStatus,overrides,overrideCount);
      return;
     }

   /* ======================================
      We are passing the slot override
      expression information along
      to whatever message-handler implements
      the duplicate
      ====================================== */
   theExp[0].type = INSTANCE_NAME;
   theExp[0].value = newName.value;
   theExp[0].argList = NULL;
   theExp[0].nextArg = &theExp[1];
   theExp[1].type = DATA_OBJECT_ARRAY;
   theExp[1].value = (void *) overrides;
   theExp[1].argList = NULL;
   theExp[1].nextArg = NULL;

   oldOMDMV = InstanceData(theEnv,execStatus)->ObjectModDupMsgValid;
   InstanceData(theEnv,execStatus)->ObjectModDupMsgValid = TRUE;
   DirectMessage(theEnv,execStatus,FindSymbolHN(theEnv,execStatus,DIRECT_DUPLICATE_STRING),ins,result,&theExp[0]);
   InstanceData(theEnv,execStatus)->ObjectModDupMsgValid = oldOMDMV;

   DeleteSlotOverrideEvaluations(theEnv,execStatus,overrides,overrideCount);
  }

/*************************************************************
  NAME         : MsgDuplicateInstance
  DESCRIPTION  : Duplicates an instance via the
                 message-duplicate message
  INPUTS       : The address of the result value
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot updates performed w/ int & put- messages
  NOTES        : H/L Syntax:
                 (duplicate-instance <instance>
                   [to <instance-name>] <slot-override>*)
 *************************************************************/
globle void MsgDuplicateInstance(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *result)
  {
   INSTANCE_TYPE *ins;
   DATA_OBJECT newName;
   EXPRESSION theExp[2];
   DATA_OBJECT *overrides;
   int oldOMDMV,overrideCount,error;

   /* ===========================================
      The slot-overrides need to be evaluated now
      to resolve any variable references before a
      new frame is pushed for message-handler
      execution
      =========================================== */
   overrides = EvaluateSlotOverrides(theEnv,execStatus,GetFirstArgument()->nextArg->nextArg,
                                     &overrideCount,&error);
   if (error)
     {
      SetpType(result,SYMBOL);
      SetpValue(result,EnvFalseSymbol(theEnv,execStatus));
      return;
     }

   /* ==================================
      Find the instance and make sure it
      wasn't deleted by the overrides
      ================================== */
   ins = CheckInstance(theEnv,execStatus,ValueToString(ExpressionFunctionCallName(execStatus->CurrentExpression)));
   if (ins == NULL)
     {
      SetpType(result,SYMBOL);
      SetpValue(result,EnvFalseSymbol(theEnv,execStatus));
      DeleteSlotOverrideEvaluations(theEnv,execStatus,overrides,overrideCount);
      return;
     }
   if (EnvArgTypeCheck(theEnv,execStatus,ValueToString(ExpressionFunctionCallName(execStatus->CurrentExpression)),
                    2,INSTANCE_NAME,&newName) == FALSE)
     {
      SetpType(result,SYMBOL);
      SetpValue(result,EnvFalseSymbol(theEnv,execStatus));
      DeleteSlotOverrideEvaluations(theEnv,execStatus,overrides,overrideCount);
      return;
     }

   /* ======================================
      We are passing the slot override
      expression information along
      to whatever message-handler implements
      the duplicate
      ====================================== */
   theExp[0].type = INSTANCE_NAME;
   theExp[0].value = newName.value;
   theExp[0].argList = NULL;
   theExp[0].nextArg = &theExp[1];
   theExp[1].type = DATA_OBJECT_ARRAY;
   theExp[1].value = (void *) overrides;
   theExp[1].argList = NULL;
   theExp[1].nextArg = NULL;

   oldOMDMV = InstanceData(theEnv,execStatus)->ObjectModDupMsgValid;
   InstanceData(theEnv,execStatus)->ObjectModDupMsgValid = TRUE;
   DirectMessage(theEnv,execStatus,FindSymbolHN(theEnv,execStatus,MSG_DUPLICATE_STRING),ins,result,&theExp[0]);
   InstanceData(theEnv,execStatus)->ObjectModDupMsgValid = oldOMDMV;

   DeleteSlotOverrideEvaluations(theEnv,execStatus,overrides,overrideCount);
  }

#if DEFRULE_CONSTRUCT

/**************************************************************
  NAME         : InactiveModifyInstance
  DESCRIPTION  : Modifies slots of an instance of a class
                 Pattern-matching is automatically
                 delayed until the slot updates are done
  INPUTS       : The address of the result value
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot updates performed directly
  NOTES        : H/L Syntax:
                 (modify-instance <instance-name>
                   <slot-override>*)
 **************************************************************/
globle void InactiveModifyInstance(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *result)
  {
   int ov;

   ov = SetDelayObjectPatternMatching(theEnv,execStatus,TRUE);
   ModifyInstance(theEnv,execStatus,result);
   SetDelayObjectPatternMatching(theEnv,execStatus,ov);
  }

/**************************************************************
  NAME         : InactiveMsgModifyInstance
  DESCRIPTION  : Modifies slots of an instance of a class
                 Pattern-matching is automatically
                 delayed until the slot updates are done
  INPUTS       : The address of the result value
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot updates performed with put- messages
  NOTES        : H/L Syntax:
                 (message-modify-instance <instance-name>
                   <slot-override>*)
 **************************************************************/
globle void InactiveMsgModifyInstance(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *result)
  {
   int ov;

   ov = SetDelayObjectPatternMatching(theEnv,execStatus,TRUE);
   MsgModifyInstance(theEnv,execStatus,result);
   SetDelayObjectPatternMatching(theEnv,execStatus,ov);
  }

/*******************************************************************
  NAME         : InactiveDuplicateInstance
  DESCRIPTION  : Duplicates an instance of a class
                 Pattern-matching is automatically
                 delayed until the slot updates are done
  INPUTS       : The address of the result value
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot updates performed directly
  NOTES        : H/L Syntax:
                 (duplicate-instance <instance> [to <instance-name>]
                   <slot-override>*)
 *******************************************************************/
globle void InactiveDuplicateInstance(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *result)
  {
   int ov;

   ov = SetDelayObjectPatternMatching(theEnv,execStatus,TRUE);
   DuplicateInstance(theEnv,execStatus,result);
   SetDelayObjectPatternMatching(theEnv,execStatus,ov);
  }

/**************************************************************
  NAME         : InactiveMsgDuplicateInstance
  DESCRIPTION  : Duplicates an instance of a class
                 Pattern-matching is automatically
                 delayed until the slot updates are done
  INPUTS       : The address of the result value
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot updates performed with put- messages
  NOTES        : H/L Syntax:
                 (message-duplicate-instance <instance>
                   [to <instance-name>]
                   <slot-override>*)
 **************************************************************/
globle void InactiveMsgDuplicateInstance(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *result)
  {
   int ov;

   ov = SetDelayObjectPatternMatching(theEnv,execStatus,TRUE);
   MsgDuplicateInstance(theEnv,execStatus,result);
   SetDelayObjectPatternMatching(theEnv,execStatus,ov);
  }

#endif

/*****************************************************
  NAME         : DirectDuplicateMsgHandler
  DESCRIPTION  : Implementation for the USER class
                 handler direct-duplicate

                 Implements duplicate-instance message
                 with a series of direct slot
                 placements
  INPUTS       : A data object buffer to hold the
                 result
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot values updated
  NOTES        : None
 *****************************************************/
globle void DirectDuplicateMsgHandler(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *result)
  {
   DuplicateMsgHandlerSupport(theEnv,execStatus,result,FALSE);
  }

/*****************************************************
  NAME         : MsgDuplicateMsgHandler
  DESCRIPTION  : Implementation for the USER class
                 handler message-duplicate

                 Implements duplicate-instance message
                 with a series of put- messages
  INPUTS       : A data object buffer to hold the
                 result
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot values updated
  NOTES        : None
 *****************************************************/
globle void MsgDuplicateMsgHandler(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *result)
  {
   DuplicateMsgHandlerSupport(theEnv,execStatus,result,TRUE);
  }

/***************************************************
  NAME         : DirectModifyMsgHandler
  DESCRIPTION  : Implementation for the USER class
                 handler direct-modify

                 Implements modify-instance message
                 with a series of direct slot
                 placements
  INPUTS       : A data object buffer to hold the
                 result
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot values updated
  NOTES        : None
 ***************************************************/
globle void DirectModifyMsgHandler(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *result)
  {
   ModifyMsgHandlerSupport(theEnv,execStatus,result,FALSE);
  }

/***************************************************
  NAME         : MsgModifyMsgHandler
  DESCRIPTION  : Implementation for the USER class
                 handler message-modify

                 Implements modify-instance message
                 with a series of put- messages
  INPUTS       : A data object buffer to hold the
                 result
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot values updated
  NOTES        : None
 ***************************************************/
globle void MsgModifyMsgHandler(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *result)
  {
   ModifyMsgHandlerSupport(theEnv,execStatus,result,TRUE);
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***********************************************************
  NAME         : EvaluateSlotOverrides
  DESCRIPTION  : Evaluates the slot-override expressions
                 for modify-instance and duplicate-instance
                 Evaluations are stored in an array of
                 data objects, where the supplementalInfo
                 field points at the name of the slot
                 The data object next fields are used
                 to link the array as well.
  INPUTS       : 1) The slot override expressions
                 2) A buffer to hold the number
                    of slot overrides
                 3) A buffer to hold an error flag
  RETURNS      : The slot override data object array
  SIDE EFFECTS : Data object array allocated and initialized
                 override count and error buffers set
  NOTES        : Slot overrides must be evaluated before
                 calling supporting message-handlers for
                 modify- and duplicate-instance in the
                 event that the overrides contain variable
                 references to an outer frame
 ***********************************************************/
static DATA_OBJECT *EvaluateSlotOverrides(
  void *theEnv,
  EXEC_STATUS,
  EXPRESSION *ovExprs,
  int *ovCnt,
  int *error)
  {
   DATA_OBJECT *ovs;
   int ovi;
   void *slotName;

   *error = FALSE;

   /* ==========================================
      There are two expressions chains for every
      slot override: one for the slot name and
      one for the slot value
      ========================================== */
   *ovCnt = CountArguments(ovExprs) / 2;
   if (*ovCnt == 0)
     return(NULL);

   /* ===============================================
      Evaluate all the slot override names and values
      and store them in a contiguous array
      =============================================== */
   ovs = (DATA_OBJECT *) gm2(theEnv,execStatus,(sizeof(DATA_OBJECT) * (*ovCnt)));
   ovi = 0;
   while (ovExprs != NULL)
     {
      if (EvaluateExpression(theEnv,execStatus,ovExprs,&ovs[ovi]))
        goto EvaluateOverridesError;
      if (ovs[ovi].type != SYMBOL)
        {
         ExpectedTypeError1(theEnv,execStatus,ValueToString(ExpressionFunctionCallName(execStatus->CurrentExpression)),
                            ovi+1,"slot name");
         SetEvaluationError(theEnv,execStatus,TRUE);
         goto EvaluateOverridesError;
        }
      slotName = ovs[ovi].value;
      if (ovExprs->nextArg->argList)
        {
         if (EvaluateAndStoreInDataObject(theEnv,execStatus,FALSE,ovExprs->nextArg->argList,
                                               &ovs[ovi],TRUE) == FALSE)
           goto EvaluateOverridesError;
        }
      else
        {
         SetpDOBegin(&ovs[ovi],1);
         SetpDOEnd(&ovs[ovi],0);
         SetpType(&ovs[ovi],MULTIFIELD);
         SetpValue(&ovs[ovi],ProceduralPrimitiveData(theEnv,execStatus)->NoParamValue);
        }
      ovs[ovi].supplementalInfo = slotName;
      ovExprs = ovExprs->nextArg->nextArg;
      ovs[ovi].next = (ovExprs != NULL) ? &ovs[ovi+1] : NULL;
      ovi++;
     }
   return(ovs);

EvaluateOverridesError:
   rm(theEnv,execStatus,(void *) ovs,(sizeof(DATA_OBJECT) * (*ovCnt)));
   *error = TRUE;
   return(NULL);
  }

/**********************************************************
  NAME         : DeleteSlotOverrideEvaluations
  DESCRIPTION  : Deallocates slot override evaluation array
  INPUTS       : 1) The data object array
                 2) The number of elements
  RETURNS      : Nothing useful
  SIDE EFFECTS : Deallocates slot override data object
                 array for modify- and duplicate- instance
  NOTES        : None
 **********************************************************/
static void DeleteSlotOverrideEvaluations(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *ovEvals,
  int ovCnt)
  {
   if (ovEvals != NULL)
     rm(theEnv,execStatus,(void *) ovEvals,(sizeof(DATA_OBJECT) * ovCnt));
  }

/**********************************************************
  NAME         : ModifyMsgHandlerSupport
  DESCRIPTION  : Support routine for DirectModifyMsgHandler
                 and MsgModifyMsgHandler

                 Performs a series of slot updates
                 directly or with messages
  INPUTS       : 1) A data object buffer to hold the result
                 2) A flag indicating whether to use
                    put- messages or direct placement
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slots updated (messages sent)
  NOTES        : None
 **********************************************************/
static void ModifyMsgHandlerSupport(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *result,
  int msgpass)
  {
   DATA_OBJECT *slotOverrides,*newval,temp,junk;
   EXPRESSION msgExp;
   INSTANCE_TYPE *ins;
   INSTANCE_SLOT *insSlot;

   result->type = SYMBOL;
   result->value = EnvFalseSymbol(theEnv,execStatus);
   if (InstanceData(theEnv,execStatus)->ObjectModDupMsgValid == FALSE)
     {
      PrintErrorID(theEnv,execStatus,"INSMODDP",1,FALSE);
      EnvPrintRouter(theEnv,WERROR,"Direct/message-modify message valid only in modify-instance.\n");
      SetEvaluationError(theEnv,execStatus,TRUE);
      return;
     }
   InstanceData(theEnv,execStatus)->ObjectModDupMsgValid = FALSE;

   ins = GetActiveInstance(theEnv,execStatus);
   if (ins->garbage)
     {
      StaleInstanceAddress(theEnv,execStatus,"modify-instance",0);
      SetEvaluationError(theEnv,execStatus,TRUE);
      return;
     }

   /* =======================================
      Retrieve the slot override data objects
      passed from ModifyInstance - the slot
      name is stored in the supplementalInfo
      field - and the next fields are links
      ======================================= */
   slotOverrides = (DATA_OBJECT *) GetNthMessageArgument(theEnv,execStatus,1)->value;

   while (slotOverrides != NULL)
     {
      /* ===========================================================
         No evaluation or error checking needs to be done
         since this has already been done by EvaluateSlotOverrides()
         =========================================================== */
      insSlot = FindInstanceSlot(theEnv,execStatus,ins,(SYMBOL_HN *) slotOverrides->supplementalInfo);
      if (insSlot == NULL)
        {
         SlotExistError(theEnv,execStatus,ValueToString(slotOverrides->supplementalInfo),"modify-instance");
         SetEvaluationError(theEnv,execStatus,TRUE);
         return;
        }
      if (msgpass)
        {
         msgExp.type = slotOverrides->type;
         if (msgExp.type != MULTIFIELD)
           msgExp.value = slotOverrides->value;
         else
           msgExp.value = (void *) slotOverrides;
         msgExp.argList = NULL;
         msgExp.nextArg = NULL;
         DirectMessage(theEnv,execStatus,insSlot->desc->overrideMessage,ins,&temp,&msgExp);
         if (execStatus->EvaluationError ||
             ((temp.type == SYMBOL) && (temp.value == EnvFalseSymbol(theEnv,execStatus))))
           return;
        }
      else
        {
         if (insSlot->desc->multiple && (slotOverrides->type != MULTIFIELD))
           {
            temp.type = MULTIFIELD;
            temp.value = EnvCreateMultifield(theEnv,execStatus,1L);
            SetDOBegin(temp,1);
            SetDOEnd(temp,1);
            SetMFType(temp.value,1,(short) slotOverrides->type);
            SetMFValue(temp.value,1,slotOverrides->value);
            newval = &temp;
           }
         else
           newval = slotOverrides;
         if (PutSlotValue(theEnv,execStatus,ins,insSlot,newval,&junk,"modify-instance") == FALSE)
           return;
        }

      slotOverrides = slotOverrides->next;
     }
   result->value = EnvTrueSymbol(theEnv,execStatus);
  }

/*************************************************************
  NAME         : DuplicateMsgHandlerSupport
  DESCRIPTION  : Support routine for DirectDuplicateMsgHandler
                 and MsgDuplicateMsgHandler

                 Performs a series of slot updates
                 directly or with messages
  INPUTS       : 1) A data object buffer to hold the result
                 2) A flag indicating whether to use
                    put- messages or direct placement
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slots updated (messages sent)
  NOTES        : None
 *************************************************************/
static void DuplicateMsgHandlerSupport(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT *result,
  int msgpass)
  {
   INSTANCE_TYPE *srcins,*dstins;
   SYMBOL_HN *newName;
   DATA_OBJECT *slotOverrides;
   EXPRESSION *valArg,msgExp;
   long i;
   int oldMkInsMsgPass;
   INSTANCE_SLOT *dstInsSlot;
   DATA_OBJECT temp,junk,*newval;

   result->type = SYMBOL;
   result->value = EnvFalseSymbol(theEnv,execStatus);
   if (InstanceData(theEnv,execStatus)->ObjectModDupMsgValid == FALSE)
     {
      PrintErrorID(theEnv,execStatus,"INSMODDP",2,FALSE);
      EnvPrintRouter(theEnv,WERROR,"Direct/message-duplicate message valid only in duplicate-instance.\n");
      SetEvaluationError(theEnv,execStatus,TRUE);
      return;
     }
   InstanceData(theEnv,execStatus)->ObjectModDupMsgValid = FALSE;

   /* ==================================
      Grab the slot override expressions
      and determine the source instance
      and the name of the new instance
      ================================== */
   srcins = GetActiveInstance(theEnv,execStatus);
   newName = (SYMBOL_HN *) GetNthMessageArgument(theEnv,execStatus,1)->value;
   slotOverrides = (DATA_OBJECT *) GetNthMessageArgument(theEnv,execStatus,2)->value;
   if (srcins->garbage)
     {
      StaleInstanceAddress(theEnv,execStatus,"duplicate-instance",0);
      SetEvaluationError(theEnv,execStatus,TRUE);
      return;
     }
   if (newName == srcins->name)
     {
      PrintErrorID(theEnv,execStatus,"INSMODDP",3,FALSE);
      EnvPrintRouter(theEnv,WERROR,"Instance copy must have a different name in duplicate-instance.\n");
      SetEvaluationError(theEnv,execStatus,TRUE);
      return;
     }

   /* ==========================================
      Create an uninitialized new instance of
      the new name (delete old version - if any)
      ========================================== */
   oldMkInsMsgPass = InstanceData(theEnv,execStatus)->MkInsMsgPass;
   InstanceData(theEnv,execStatus)->MkInsMsgPass = msgpass;
   dstins = BuildInstance(theEnv,execStatus,newName,srcins->cls,TRUE);
   InstanceData(theEnv,execStatus)->MkInsMsgPass = oldMkInsMsgPass;
   if (dstins == NULL)
     return;
   dstins->busy++;

   /* ================================
      Place slot overrides directly or
      with put- messages
      ================================ */
   while (slotOverrides != NULL)
     {
      /* ===========================================================
         No evaluation or error checking needs to be done
         since this has already been done by EvaluateSlotOverrides()
         =========================================================== */
      dstInsSlot = FindInstanceSlot(theEnv,execStatus,dstins,(SYMBOL_HN *) slotOverrides->supplementalInfo);
      if (dstInsSlot == NULL)
        {
         SlotExistError(theEnv,execStatus,ValueToString(slotOverrides->supplementalInfo),
                        "duplicate-instance");
         goto DuplicateError;
        }
      if (msgpass)
        {
         msgExp.type = slotOverrides->type;
         if (msgExp.type != MULTIFIELD)
           msgExp.value = slotOverrides->value;
         else
           msgExp.value = (void *) slotOverrides;
         msgExp.argList = NULL;
         msgExp.nextArg = NULL;
         DirectMessage(theEnv,execStatus,dstInsSlot->desc->overrideMessage,dstins,&temp,&msgExp);
         if (execStatus->EvaluationError ||
             ((temp.type == SYMBOL) && (temp.value == EnvFalseSymbol(theEnv,execStatus))))
           goto DuplicateError;
        }
      else
        {
         if (dstInsSlot->desc->multiple && (slotOverrides->type != MULTIFIELD))
           {
            temp.type = MULTIFIELD;
            temp.value = EnvCreateMultifield(theEnv,execStatus,1L);
            SetDOBegin(temp,1);
            SetDOEnd(temp,1);
            SetMFType(temp.value,1,(short) slotOverrides->type);
            SetMFValue(temp.value,1,slotOverrides->value);
            newval = &temp;
           }
         else
           newval = slotOverrides;
         if (PutSlotValue(theEnv,execStatus,dstins,dstInsSlot,newval,&junk,"duplicate-instance") == FALSE)
           goto DuplicateError;
        }
      dstInsSlot->override = TRUE;
      slotOverrides = slotOverrides->next;
     }

   /* =======================================
      Copy values from source instance to new
      directly or with put- messages
      ======================================= */
   for (i = 0 ; i < dstins->cls->localInstanceSlotCount ; i++)
     {
      if (dstins->slots[i].override == FALSE)
        {
         if (msgpass)
           {
            temp.type = (unsigned short)  srcins->slots[i].type;
            temp.value = srcins->slots[i].value;
            if (temp.type == MULTIFIELD)
              {
               SetDOBegin(temp,1);
               SetDOEnd(temp,GetMFLength(temp.value));
              }
            valArg = ConvertValueToExpression(theEnv,execStatus,&temp);
            DirectMessage(theEnv,execStatus,dstins->slots[i].desc->overrideMessage,
                          dstins,&temp,valArg);
            ReturnExpression(theEnv,execStatus,valArg);
            if (execStatus->EvaluationError ||
                ((temp.type == SYMBOL) && (temp.value == EnvFalseSymbol(theEnv,execStatus))))
              goto DuplicateError;
           }
         else
           {
            temp.type = (unsigned short) srcins->slots[i].type;
            temp.value = srcins->slots[i].value;
            if (srcins->slots[i].type == MULTIFIELD)
              {
               SetDOBegin(temp,1);
               SetDOEnd(temp,GetMFLength(srcins->slots[i].value));
              }
            if (PutSlotValue(theEnv,execStatus,dstins,&dstins->slots[i],&temp,&junk,"duplicate-instance")
                 == FALSE)
              goto DuplicateError;
           }
        }
     }

   /* =======================================
      Send init message for message-duplicate
      ======================================= */
   if (msgpass)
     {
      for (i = 0 ; i < dstins->cls->instanceSlotCount ; i++)
        dstins->slotAddresses[i]->override = TRUE;
      dstins->initializeInProgress = 1;
      DirectMessage(theEnv,execStatus,MessageHandlerData(theEnv,execStatus)->INIT_SYMBOL,dstins,result,NULL);
     }
   dstins->busy--;
   if (dstins->garbage)
     {
      result->type = SYMBOL;
      result->value = EnvFalseSymbol(theEnv,execStatus);
      SetEvaluationError(theEnv,execStatus,TRUE);
     }
   else
     {
      result->type = INSTANCE_NAME;
      result->value = (void *) GetFullInstanceName(theEnv,execStatus,dstins);
     }
   return;

DuplicateError:
   dstins->busy--;
   QuashInstance(theEnv,execStatus,dstins);
   SetEvaluationError(theEnv,execStatus,TRUE);
  }

#endif



   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*              CLIPS Version 6.10  04/09/97           */
   /*                                                     */
   /*                INSTANCE FUNCTIONS MODULE            */
   /*******************************************************/

/*************************************************************/
/* Purpose:  Internal instance manipulation routines         */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
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

#include <stdlib.h>

#include "setup.h"

#if OBJECT_SYSTEM

#include "argacces.h"
#include "classcom.h"
#include "classfun.h"
#include "memalloc.h"
#include "cstrnchk.h"
#include "inscom.h"
#include "insmngr.h"
#include "modulutl.h"
#include "msgfun.h"
#include "prccode.h"
#include "router.h"
#include "utility.h"

#if INSTANCE_PATTERN_MATCHING
#include "drive.h"
#include "objrtmch.h"
#endif

#define _INSFUN_SOURCE_
#include "insfun.h"

/* =========================================
   *****************************************
                   CONSTANTS
   =========================================
   ***************************************** */
#define BIG_PRIME    11329

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

static INSTANCE_TYPE *FindImportedInstance(struct defmodule *,struct defmodule *,INSTANCE_TYPE *);
static void PrintInstanceName(char *,void *);
static void PrintInstanceLongForm(char *,void *);

#if INSTANCE_PATTERN_MATCHING
static void NetworkModifyForSharedSlot(int,DEFCLASS *,SLOT_DESC *);
static void DecrementObjectBasisCount(void *);
static void IncrementObjectBasisCount(void *);
static void MatchObjectFunction(void *);
static BOOLEAN NetworkSynchronized(void *);
#endif

/* =========================================
   *****************************************
      EXTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */
Thread globle INSTANCE_TYPE **InstanceTable = NULL;
Thread globle int MaintainGarbageInstances = FALSE;
Thread globle int MkInsMsgPass = TRUE;
Thread globle int ChangesToInstances = FALSE;
Thread globle IGARBAGE *InstanceGarbageList = NULL;

Thread globle struct patternEntityRecord InstanceInfo = { { "INSTANCE_ADDRESS",
                                                     INSTANCE_ADDRESS,0,0,0,
                                                     PrintInstanceName,
                                                     PrintInstanceLongForm,
                                                     UnmakeInstance,
                                                     NULL,
                                                     GetNextInstance,
                                                     DecrementInstanceCount,
                                                     IncrementInstanceCount,
                                                     NULL,NULL,NULL,NULL
                                                   },
#if INSTANCE_PATTERN_MATCHING
                                                  DecrementObjectBasisCount,
                                                  IncrementObjectBasisCount,
                                                  MatchObjectFunction,
                                                  NetworkSynchronized
#else
                                                  NULL,NULL,NULL,NULL
#endif
                                                };


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

/***************************************************
  NAME         : IncrementInstanceCount
  DESCRIPTION  : Increments instance busy count -
                   prevents it from being deleted
  INPUTS       : The address of the instance
  RETURNS      : Nothing useful
  SIDE EFFECTS : Count set
  NOTES        : None
 ***************************************************/
globle void IncrementInstanceCount(
  void *vptr)
  {
   ((INSTANCE_TYPE *) vptr)->busy++;
  }

/***************************************************
  NAME         : DecrementInstanceCount
  DESCRIPTION  : Decrements instance busy count -
                   might allow it to be deleted
  INPUTS       : The address of the instance
  RETURNS      : Nothing useful
  SIDE EFFECTS : Count set
  NOTES        : None
 ***************************************************/
globle void DecrementInstanceCount(
  void *vptr)
  {
   ((INSTANCE_TYPE *) vptr)->busy--;
  }

/***************************************************
  NAME         : InitializeInstanceTable
  DESCRIPTION  : Initializes instance hash table
                  to all NULL addresses
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Hash table initialized
  NOTES        : None
 ***************************************************/
globle void InitializeInstanceTable()
  {
   register int i;

   InstanceTable = (INSTANCE_TYPE **)
                    gm2((int) (sizeof(INSTANCE_TYPE *) * INSTANCE_TABLE_HASH_SIZE));
   for (i = 0 ; i < INSTANCE_TABLE_HASH_SIZE ; i++)
     InstanceTable[i] = NULL;
  }

/*******************************************************
  NAME         : CleanupInstances
  DESCRIPTION  : Iterates through instance garbage
                   list looking for nodes that
                   have become unused - and purges
                   them
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Non-busy instance garbage nodes deleted
  NOTES        : None
 *******************************************************/
globle void CleanupInstances()
  {
   IGARBAGE *gprv,*gtmp,*dump;

   if (MaintainGarbageInstances)
     return;
   gprv = NULL;
   gtmp = InstanceGarbageList;
   while (gtmp != NULL)
     {
      if ((gtmp->ins->busy == 0) && (gtmp->ins->depth > CurrentEvaluationDepth)
#if INSTANCE_PATTERN_MATCHING
          && (gtmp->ins->header.busyCount == 0)
#endif
         )
        {
         EphemeralItemCount -= 2;
         EphemeralItemSize -= InstanceSizeHeuristic(gtmp->ins) + sizeof(IGARBAGE);
         DecrementSymbolCount(gtmp->ins->name);
         rtn_struct(instance,gtmp->ins);
         if (gprv == NULL)
           InstanceGarbageList = gtmp->nxt;
         else
           gprv->nxt = gtmp->nxt;
         dump = gtmp;
         gtmp = gtmp->nxt;
         rtn_struct(igarbage,dump);
        }
      else
        {
         gprv = gtmp;
         gtmp = gtmp->nxt;
        }
     }
  }

/*******************************************************
  NAME         : HashInstance
  DESCRIPTION  : Generates a hash index for a given
                 instance name
  INPUTS       : The address of the instance name SYMBOL_HN
  RETURNS      : The hash index value
  SIDE EFFECTS : None
  NOTES        : Counts on the fact that the symbol
                 has already been hashed into the
                 symbol table - uses that hash value
                 multiplied by a prime for a new hash
 *******************************************************/
globle unsigned HashInstance(
  SYMBOL_HN *cname)
  {
   unsigned long tally;

   tally = ((unsigned long) cname->bucket) * BIG_PRIME;
   return((unsigned) (tally % INSTANCE_TABLE_HASH_SIZE));
  }

/***************************************************
  NAME         : DestroyAllInstances
  DESCRIPTION  : Deallocates all instances,
                  reinitializes hash table and
                  resets class instance pointers
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : All instances deallocated
  NOTES        : None
 ***************************************************/
globle void DestroyAllInstances()
  {
   INSTANCE_TYPE *iptr;
   int svmaintain;

   SaveCurrentModule();
   svmaintain = MaintainGarbageInstances;
   MaintainGarbageInstances = TRUE;
   iptr = InstanceList;
   while (iptr != NULL)
     {
      SetCurrentModule((void *) iptr->cls->header.whichModule->theModule);
      DirectMessage(DELETE_SYMBOL,iptr,NULL,NULL);
      iptr = iptr->nxtList;
      while ((iptr != NULL) ? iptr->garbage : FALSE)
        iptr = iptr->nxtList;
     }
   MaintainGarbageInstances = svmaintain;
   RestoreCurrentModule();
  }

/******************************************************
  NAME         : RemoveInstanceData
  DESCRIPTION  : Deallocates all the data objects
                 in instance slots and then dealloactes
                 the slots themeselves
  INPUTS       : The instance
  RETURNS      : Nothing useful
  SIDE EFFECTS : Instance slots removed
  NOTES        : An instance made with CopyInstanceData
                 will have shared values removed
                 in all cases because they are not
                 "real" instances.
                 Instance class busy count decremented
 ******************************************************/
globle void RemoveInstanceData(
  INSTANCE_TYPE *ins)
  {
   register unsigned i;
   INSTANCE_SLOT *sp;

   DecrementDefclassBusyCount((void *) ins->cls);
   for (i = 0 ; i < ins->cls->instanceSlotCount ; i++)
     {
      sp = ins->slotAddresses[i];
      if ((sp == &sp->desc->sharedValue) ?
          (--sp->desc->sharedCount == 0) : TRUE)
        {
         if (sp->desc->multiple)
           {
            MultifieldDeinstall((MULTIFIELD_PTR) sp->value);
            AddToMultifieldList((MULTIFIELD_PTR) sp->value);
           }
         else
           AtomDeinstall((int) sp->type,sp->value);
         sp->value = NULL;
        }
     }
   if (ins->cls->instanceSlotCount != 0)
     {
      rm((void *) ins->slotAddresses,
         (int) (ins->cls->instanceSlotCount * sizeof(INSTANCE_SLOT *)));
      if (ins->cls->localInstanceSlotCount != 0)
        rm((void *) ins->slots,
           (int) (ins->cls->localInstanceSlotCount * sizeof(INSTANCE_SLOT)));
     }
   ins->slots = NULL;
   ins->slotAddresses = NULL;
  }

/***************************************************************************
  NAME         : FindInstanceBySymbol
  DESCRIPTION  : Looks up a specified instance in the instance hash table
  INPUTS       : The symbol for the name of the instance
  RETURNS      : The address of the found instance, NULL otherwise
  SIDE EFFECTS : None
  NOTES        : An instance is searched for by name first in the
                 current module - then in imported modules according
                 to the order given in the current module's definition
 ***************************************************************************/
globle INSTANCE_TYPE *FindInstanceBySymbol(
  SYMBOL_HN *moduleAndInstanceName)
  {
   int modulePosition,searchImports;
   SYMBOL_HN *moduleName,*instanceName;
   struct defmodule *currentModule,*theModule;

   currentModule = ((struct defmodule *) GetCurrentModule());

   /* =======================================
      Instance names of the form [<name>] are
      searched for only in the current module
      ======================================= */
   modulePosition = FindModuleSeparator(ValueToString(moduleAndInstanceName));
   if (modulePosition == FALSE)
     {
      theModule = currentModule;
      instanceName = moduleAndInstanceName;
      searchImports = FALSE;
     }

   /* =========================================
      Instance names of the form [::<name>] are
      searched for in the current module and
      imported modules in the definition order
      ========================================= */
   else if (modulePosition == 1)
     {
      theModule = currentModule;
      instanceName = ExtractConstructName(modulePosition,ValueToString(moduleAndInstanceName));
      searchImports = TRUE;
     }

   /* =============================================
      Instance names of the form [<module>::<name>]
      are searched for in the specified module
      ============================================= */
   else
     {
      moduleName = ExtractModuleName(modulePosition,ValueToString(moduleAndInstanceName));
      theModule = (struct defmodule *) FindDefmodule(ValueToString(moduleName));
      instanceName = ExtractConstructName(modulePosition,ValueToString(moduleAndInstanceName));
      if (theModule == NULL)
        return(NULL);
      searchImports = FALSE;
     }
   return(FindInstanceInModule(instanceName,theModule,currentModule,searchImports));
  }

/***************************************************
  NAME         : FindInstanceInModule
  DESCRIPTION  : Finds an instance of the given name
                 in the given module in scope of
                 the given current module
                 (will also search imported modules
                  if specified)
  INPUTS       : 1) The instance name (no module)
                 2) The module to search
                 3) The currently active module
                 4) A flag indicating whether
                    to search imported modules of
                    given module as well
  RETURNS      : The instance (NULL if none found)
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle INSTANCE_TYPE *FindInstanceInModule(
  SYMBOL_HN *instanceName,
  struct defmodule *theModule,
  struct defmodule *currentModule,
  BOOLEAN searchImports)
  {
   INSTANCE_TYPE *startInstance,*ins;

   /* ===============================
      Find the first instance of the
      correct name in the hash chain
      =============================== */
   startInstance = InstanceTable[HashInstance(instanceName)];
   while (startInstance != NULL)
     {
      if (startInstance->name == instanceName)
        break;
      startInstance = startInstance->nxtHash;
     }

   if (startInstance == NULL)
     return(NULL);

   /* ===========================================
      Look for the instance in the specified
      module - if the class of the found instance
      is in scope of the current module, we have
      found the instance
      =========================================== */
   for (ins = startInstance ;
        (ins != NULL) ? (ins->name == startInstance->name) : FALSE ;
        ins = ins->nxtHash)
     if ((ins->cls->header.whichModule->theModule == theModule) &&
          DefclassInScope(ins->cls,currentModule))
       return(ins);

   /* ================================
      For ::<name> formats, we need to
      search imported modules too
      ================================ */
   if (searchImports == FALSE)
     return(NULL);
   MarkModulesAsUnvisited();
   return(FindImportedInstance(theModule,currentModule,startInstance));
  }

/********************************************************************
  NAME         : FindInstanceSlot
  DESCRIPTION  : Finds an instance slot by name
  INPUTS       : 1) The address of the instance
                 2) The symbolic name of the slot
  RETURNS      : The address of the slot, NULL if not found
  SIDE EFFECTS : None
  NOTES        : None
 ********************************************************************/
globle INSTANCE_SLOT *FindInstanceSlot(
  INSTANCE_TYPE *ins,
  SYMBOL_HN *sname)
  {
   register int i;

   i = FindInstanceTemplateSlot(ins->cls,sname);
   return((i != -1) ? ins->slotAddresses[i] : NULL);
  }

/********************************************************************
  NAME         : FindInstanceTemplateSlot
  DESCRIPTION  : Performs a search on an class's instance
                   template slot array to find a slot by name
  INPUTS       : 1) The address of the class
                 2) The symbolic name of the slot
  RETURNS      : The index of the slot, -1 if not found
  SIDE EFFECTS : None
  NOTES        : The slot's unique id is used as index into
                 the slot map array.
 ********************************************************************/
globle int FindInstanceTemplateSlot(
  DEFCLASS *cls,
  SYMBOL_HN *sname)
  {
   int sid;

   sid = FindSlotNameID(sname);
   if (sid == -1)
     return(-1);
   if (sid > cls->maxSlotNameID)
     return(-1);
   return((int) cls->slotNameMap[sid] - 1);
  }

/***********************************************************
  NAME         : EvaluateAndStoreInDataObject
  DESCRIPTION  : Evaluates slot-value expressions
                   and stores the result in a
                   Kernel data object
  INPUTS       : 1) Flag indicating if multifields are OK
                 2) The value-expression
                 3) The data object structure
  RETURNS      : FALSE on errors, TRUE otherwise
  SIDE EFFECTS : Segment allocated for storing
                 multifield values
  NOTES        : None
 ***********************************************************/
globle int EvaluateAndStoreInDataObject(
  int mfp,
  EXPRESSION *exp,
  DATA_OBJECT *val)
  {
   val->type = MULTIFIELD;
   val->begin = 0;
   val->end = -1;
   if (exp == NULL)
     {
      val->value = CreateMultifield(0L);
      return(TRUE);
     }
   if ((mfp == 0) && (exp->nextArg == NULL))
     EvaluateExpression(exp,val);
   else
     StoreInMultifield(val,exp,TRUE);
   return(EvaluationError ? FALSE : TRUE);
  }

/*******************************************************
  NAME         : PutSlotValue
  DESCRIPTION  : Evaluates new slot-expression and
                   stores it as a multifield
                   variable for the slot.
  INPUTS       : 1) The address of the instance
                    (NULL if no trace-messages desired)
                 2) The address of the slot
                 3) The address of the value
                 4) DATA_OBJECT_PTR to store the
                    set value
                 5) The command doing the put-
  RETURNS      : FALSE on errors, or TRUE
  SIDE EFFECTS : Old value deleted and new one allocated
                 Old value symbols deinstalled
                 New value symbols installed
  NOTES        : None
 *******************************************************/
globle int PutSlotValue(
  INSTANCE_TYPE *ins,
  INSTANCE_SLOT *sp,
  DATA_OBJECT *val,
  DATA_OBJECT *setVal,
  char *theCommand)
  {
   if (ValidSlotValue(val,sp->desc,ins,theCommand) == FALSE)
     {
      SetpType(setVal,SYMBOL);
      SetpValue(setVal,FalseSymbol);
      return(FALSE);
     }
   return(DirectPutSlotValue(ins,sp,val,setVal));
  }

/*******************************************************
  NAME         : DirectPutSlotValue
  DESCRIPTION  : Evaluates new slot-expression and
                   stores it as a multifield
                   variable for the slot.
  INPUTS       : 1) The address of the instance
                    (NULL if no trace-messages desired)
                 2) The address of the slot
                 3) The address of the value
                 4) DATA_OBJECT_PTR to store the
                    set value
  RETURNS      : FALSE on errors, or TRUE
  SIDE EFFECTS : Old value deleted and new one allocated
                 Old value symbols deinstalled
                 New value symbols installed
  NOTES        : None
 *******************************************************/
globle int DirectPutSlotValue(
  INSTANCE_TYPE *ins,
  INSTANCE_SLOT *sp,
  DATA_OBJECT *val,
  DATA_OBJECT *setVal)
  {
   register long i,j; /* 6.04 Bug Fix */
#if INSTANCE_PATTERN_MATCHING
   int sharedTraversalID;
   INSTANCE_SLOT *bsp,**spaddr;
#endif
   DATA_OBJECT tmpVal;

   SetpType(setVal,SYMBOL);
   SetpValue(setVal,FalseSymbol);
   if (val == NULL)
     {
      SystemError("INSFUN",1);
      ExitRouter(EXIT_FAILURE);
     }
   else if (GetpValue(val) == NoParamValue)
     {
      if (sp->desc->dynamicDefault)
        {
         val = &tmpVal;
         if (!EvaluateAndStoreInDataObject(sp->desc->multiple,
                                           (EXPRESSION *) sp->desc->defaultValue,val))
           return(FALSE);
        }
      else
        val = (DATA_OBJECT *) sp->desc->defaultValue;
     }
#if INSTANCE_PATTERN_MATCHING
   if (JoinOperationInProgress && sp->desc->reactive &&
       (ins->cls->reactive || sp->desc->shared))
     {
      PrintErrorID("INSFUN",5,FALSE);
      PrintRouter(WERROR,"Cannot modify reactive instance slots while\n");
      PrintRouter(WERROR,"  pattern-matching is in process.\n");
      SetEvaluationError(TRUE);
      return(FALSE);
     }

   /* =============================================
      If we are about to change a slot of an object
      which is a basis for a firing rule, we need
      to make sure that slot is copied first
      ============================================= */
   if (ins->basisSlots != NULL)
     {
      spaddr = &ins->slotAddresses[ins->cls->slotNameMap[sp->desc->slotName->id] - 1];
      bsp = ins->basisSlots + (spaddr - ins->slotAddresses);
      if (bsp->value == NULL)
        {
         bsp->type = sp->type;
         bsp->value = sp->value;
         if (sp->desc->multiple)
           MultifieldInstall((MULTIFIELD_PTR) bsp->value);
         else
           AtomInstall((int) bsp->type,bsp->value);
        }
     }

#endif
   if (sp->desc->multiple == 0)
     {
      AtomDeinstall((int) sp->type,sp->value);

      /* ======================================
         Assumed that multfield already checked
         to be of cardinality 1
         ====================================== */
      if (GetpType(val) == MULTIFIELD)
        {
         sp->type = GetMFType(GetpValue(val),GetpDOBegin(val));
         sp->value = GetMFValue(GetpValue(val),GetpDOBegin(val));
        }
      else
        {
         sp->type = val->type;
         sp->value = val->value;
        }
      AtomInstall((int) sp->type,sp->value);
      SetpType(setVal,sp->type);
      SetpValue(setVal,sp->value);
     }
   else
     {
      MultifieldDeinstall((MULTIFIELD_PTR) sp->value);
      AddToMultifieldList((MULTIFIELD_PTR) sp->value);
      sp->type = MULTIFIELD;
      if (val->type == MULTIFIELD)
        {
         sp->value = CreateMultifield2(GetpDOLength(val));
         for (i = 1 , j = GetpDOBegin(val) ; i <= GetpDOLength(val) ; i++ , j++)
           {
            SetMFType(sp->value,i,GetMFType(val->value,j));
            SetMFValue(sp->value,i,GetMFValue(val->value,j));
           }
        }
      else
        {
         sp->value = CreateMultifield2(1L);
         SetMFType(sp->value,1,(short) val->type);
         SetMFValue(sp->value,1,val->value);
        }
      MultifieldInstall((struct multifield *) sp->value);
      SetpType(setVal,MULTIFIELD);
      SetpValue(setVal,sp->value);
      SetpDOBegin(setVal,1);
      SetpDOEnd(setVal,GetMFLength(sp->value));
     }
   /* ==================================================
      6.05 Bug fix - any slot set directly or indirectly
      by a slot override or other side-effect during an
      instance initialization should not have its
      default value set
      ================================================== */

   sp->override = ins->initializeInProgress;

#if DEBUGGING_FUNCTIONS
   if (ins->cls->traceSlots)
     {
      if (sp->desc->shared)
        PrintRouter(WTRACE,"::= shared slot ");
      else
        PrintRouter(WTRACE,"::= local slot ");
      PrintRouter(WTRACE,ValueToString(sp->desc->slotName->name));
      PrintRouter(WTRACE," in instance ");
      PrintRouter(WTRACE,ValueToString(ins->name));
      PrintRouter(WTRACE," <- ");
      if (sp->type != MULTIFIELD)
        PrintAtom(WTRACE,(int) sp->type,sp->value);
      else
        PrintMultifield(WTRACE,(MULTIFIELD_PTR) sp->value,0,
                        GetInstanceSlotLength(sp) - 1,TRUE);
      PrintRouter(WTRACE,"\n");
     }
#endif
   ChangesToInstances = TRUE;

#if INSTANCE_PATTERN_MATCHING
   if (ins->cls->reactive && sp->desc->reactive)
     {
      /* ============================================
         If we have changed a shared slot, we need to
         perform a Rete update for every instance
         which contains this slot
         ============================================ */
      if (sp->desc->shared)
        {
         sharedTraversalID = GetTraversalID();
         if (sharedTraversalID != -1)
           {
            NetworkModifyForSharedSlot(sharedTraversalID,sp->desc->cls,sp->desc);
            ReleaseTraversalID();
           }
         else
           {
            PrintErrorID("INSFUN",6,FALSE);
            PrintRouter(WERROR,"Unable to pattern-match on shared slot ");
            PrintRouter(WERROR,ValueToString(sp->desc->slotName->name));
            PrintRouter(WERROR," in class ");
            PrintRouter(WERROR,GetDefclassName((void *) sp->desc->cls));
            PrintRouter(WERROR,".\n");
           }
        }
      else
        ObjectNetworkAction(OBJECT_MODIFY,(INSTANCE_TYPE *) ins,(int) sp->desc->slotName->id);
     }
#endif

   return(TRUE);
  }

/*******************************************************************
  NAME         : ValidSlotValue
  DESCRIPTION  : Determines if a value is appropriate
                   for a slot-value
  INPUTS       : 1) The value buffer
                 2) Slot descriptor
                 3) Instance for which slot is being checked
                    (can be NULL)
                 4) Buffer holding printout of the offending command
                    (if NULL assumes message-handler is executing
                     and calls PrintHandler for CurrentCore instead)
  RETURNS      : TRUE if value is OK, FALSE otherwise
  SIDE EFFECTS : Sets EvaluationError if slot is not OK
  NOTES        : Examines all fields of a multi-field
 *******************************************************************/
globle int ValidSlotValue(
  DATA_OBJECT *val,
  SLOT_DESC *sd,
  INSTANCE_TYPE *ins,
  char *theCommand)
  {
   register int violationCode;

   /* ===================================
      Special NoParamValue means to reset
      slot to default value
      =================================== */
   if (GetpValue(val) == NoParamValue)
     return(TRUE);
   if ((sd->multiple == 0) && (val->type == MULTIFIELD) &&
                              (GetpDOLength(val) != 1))
     {
      PrintErrorID("INSFUN",7,FALSE);
      PrintDataObject(WERROR,val);
      PrintRouter(WERROR," illegal for single-field ");
      PrintSlot(WERROR,sd,ins,theCommand);
      PrintRouter(WERROR,".\n");
      SetEvaluationError(TRUE);
      return(FALSE);
     }
   if (val->type == RVOID)
     {
      PrintErrorID("INSFUN",8,FALSE);
      PrintRouter(WERROR,"Void function illegal value for ");
      PrintSlot(WERROR,sd,ins,theCommand);
      PrintRouter(WERROR,".\n");
      SetEvaluationError(TRUE);
      return(FALSE);
     }
   if (GetDynamicConstraintChecking())
     {
      violationCode = ConstraintCheckDataObject(val,sd->constraint);
      if (violationCode != NO_VIOLATION)
        {
         PrintErrorID("CSTRNCHK",1,FALSE);
         if ((GetpType(val) == MULTIFIELD) && (sd->multiple == 0))
           PrintAtom(WERROR,GetMFType(GetpValue(val),GetpDOBegin(val)),
                            GetMFValue(GetpValue(val),GetpDOEnd(val)));
         else
           PrintDataObject(WERROR,val);
         PrintRouter(WERROR," for ");
         PrintSlot(WERROR,sd,ins,theCommand);
         ConstraintViolationErrorMessage(NULL,NULL,0,0,NULL,0,
                                         violationCode,sd->constraint,FALSE);
         SetEvaluationError(TRUE);
         return(FALSE);
        }
     }
   return(TRUE);
  }

/********************************************************
  NAME         : CheckInstance
  DESCRIPTION  : Checks to see if the first argument to
                 a function is a valid instance
  INPUTS       : Name of the calling function
  RETURNS      : The address of the instance
  SIDE EFFECTS : EvaluationError set and messages printed
                 on errors
  NOTES        : Used by Initialize and ModifyInstance
 ********************************************************/
globle INSTANCE_TYPE *CheckInstance(
  char *func)
  {
   INSTANCE_TYPE *ins;
   DATA_OBJECT temp;

   EvaluateExpression(GetFirstArgument(),&temp);
   if (temp.type == INSTANCE_ADDRESS)
     {
      ins = (INSTANCE_TYPE *) temp.value;
      if (ins->garbage == 1)
        {
         StaleInstanceAddress(func,0);
         SetEvaluationError(TRUE);
         return(NULL);
        }
     }
   else if ((temp.type == INSTANCE_NAME) ||
            (temp.type == SYMBOL))
     {
      ins = FindInstanceBySymbol((SYMBOL_HN *) temp.value);
      if (ins == NULL)
        {
         NoInstanceError(ValueToString(temp.value),func);
         return(NULL);
        }
     }
   else
     {
      PrintErrorID("INSFUN",1,FALSE);
      PrintRouter(WERROR,"Expected a valid instance in function ");
      PrintRouter(WERROR,func);
      PrintRouter(WERROR,".\n");
      SetEvaluationError(TRUE);
      return(NULL);
     }
   return(ins);
  }

/***************************************************
  NAME         : NoInstanceError
  DESCRIPTION  : Prints out an appropriate error
                  message when an instance cannot be
                  found for a function
  INPUTS       : 1) The instance name
                 2) The function name
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle void NoInstanceError(
  char *iname,
  char *func)
  {
   PrintErrorID("INSFUN",2,FALSE);
   PrintRouter(WERROR,"No such instance ");
   PrintRouter(WERROR,iname);
   PrintRouter(WERROR," in function ");
   PrintRouter(WERROR,func);
   PrintRouter(WERROR,".\n");
   SetEvaluationError(TRUE);
  }

/***************************************************
  NAME         : SlotExistError
  DESCRIPTION  : Prints out an appropriate error
                  message when a slot cannot be
                  found for a function
  INPUTS       : 1) The slot name
                 2) The function name
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle void SlotExistError(
  char *sname,
  char *func)
  {
   PrintErrorID("INSFUN",3,FALSE);
   PrintRouter(WERROR,"No such slot ");
   PrintRouter(WERROR,sname);
   PrintRouter(WERROR," in function ");
   PrintRouter(WERROR,func);
   PrintRouter(WERROR,".\n");
   SetEvaluationError(TRUE);
  }

/***************************************************
  NAME         : StaleInstanceAddress
  DESCRIPTION  : Prints out an appropriate error
                  message when an instance address
                  is no longer valid
  INPUTS       : The function name
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle void StaleInstanceAddress(
  char *func,
  int whichArg)
  {
   PrintErrorID("INSFUN",4,FALSE);
   PrintRouter(WERROR,"Invalid instance-address in function ");
   PrintRouter(WERROR,func);
   if (whichArg > 0)
     {
      PrintRouter(WERROR,", argument #");
      PrintLongInteger(WERROR,(long) whichArg);
     }
   PrintRouter(WERROR,".\n");
  }

/**********************************************************************
  NAME         : GetInstancesChanged
  DESCRIPTION  : Returns whether instances have changed
                   (any were added/deleted or slot values were changed)
                   since last time flag was set to FALSE
  INPUTS       : None
  RETURNS      : The instances-changed flag
  SIDE EFFECTS : None
  NOTES        : Used by interfaces to update instance windows
 **********************************************************************/
globle int GetInstancesChanged()
  {
   return(ChangesToInstances);
  }

/*******************************************************
  NAME         : SetInstancesChanged
  DESCRIPTION  : Sets instances-changed flag (see above)
  INPUTS       : The value (TRUE or FALSE)
  RETURNS      : Nothing useful
  SIDE EFFECTS : The flag is set
  NOTES        : None
 *******************************************************/
globle void SetInstancesChanged(
  int changed)
  {
   ChangesToInstances = changed;
  }

/*******************************************************************
  NAME         : PrintSlot
  DESCRIPTION  : Displays the name and origin of a slot
  INPUTS       : 1) The logical output name
                 2) The slot descriptor
                 3) The instance source (can be NULL)
                 4) Buffer holding printout of the offending command
                    (if NULL assumes message-handler is executing
                     and calls PrintHandler for CurrentCore instead)
  RETURNS      : Nothing useful
  SIDE EFFECTS : Message printed
  NOTES        : None
 *******************************************************************/
globle void PrintSlot(
  char *log,
  SLOT_DESC *sd,
  INSTANCE_TYPE *ins,
  char *theCommand)
  {
   PrintRouter(log,"slot ");
   PrintRouter(log,ValueToString(sd->slotName->name));
   if (ins != NULL)
     {
      PrintRouter(log," of instance [");
      PrintRouter(log,ValueToString(ins->name));
      PrintRouter(log,"]");
     }
   else if (sd->cls != NULL)
     {
      PrintRouter(log," of class ");
      PrintRouter(log,GetDefclassName((void *) sd->cls));
     }
   PrintRouter(log," found in ");
   if (theCommand != NULL)
     PrintRouter(log,theCommand);
   else
     PrintHandler(log,CurrentCore->hnd,FALSE);
  }

/*****************************************************
  NAME         : PrintInstanceNameAndClass
  DESCRIPTION  : Displays an instance's name and class
  INPUTS       : 1) Logical name of output
                 2) The instance
                 3) Flag indicating whether to
                    print carriage-return at end
  RETURNS      : Nothing useful
  SIDE EFFECTS : Instnace name and class printed
  NOTES        : None
 *****************************************************/
globle void PrintInstanceNameAndClass(
  char *logicalName,
  INSTANCE_TYPE *theInstance,
  BOOLEAN linefeedFlag)
  {
   PrintRouter(logicalName,"[");
   PrintRouter(logicalName,GetInstanceName((void *) theInstance));
   PrintRouter(logicalName,"] of ");
   PrintClassName(logicalName,theInstance->cls,linefeedFlag);
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/*****************************************************
  NAME         : FindImportedInstance
  DESCRIPTION  : Searches imported modules for an
                 instance of the correct name
                 The imports are searched recursively
                 in the order of the module definition
  INPUTS       : 1) The module for which to
                    search imported modules
                 2) The currently active module
                 3) The first instance of the
                    correct name (cannot be NULL)
  RETURNS      : An instance of the correct name
                 imported from another module which
                 is in scope of the current module
  SIDE EFFECTS : None
  NOTES        : None
 *****************************************************/
static INSTANCE_TYPE *FindImportedInstance(
  struct defmodule *theModule,
  struct defmodule *currentModule,
  INSTANCE_TYPE *startInstance)
  {
   struct portItem *importList;
   INSTANCE_TYPE *ins;

   if (theModule->visitedFlag)
     return(NULL);
   theModule->visitedFlag = TRUE;
   importList = theModule->importList;
   while (importList != NULL)
     {
      theModule = (struct defmodule *)
                  FindDefmodule(ValueToString(importList->moduleName));
      for (ins = startInstance ;
           (ins != NULL) ? (ins->name == startInstance->name) : FALSE ;
           ins = ins->nxtHash)
        if ((ins->cls->header.whichModule->theModule == theModule) &&
             DefclassInScope(ins->cls,currentModule))
          return(ins);
      ins = FindImportedInstance(theModule,currentModule,startInstance);
      if (ins != NULL)
        return(ins);
      importList = importList->next;
     }

   /* ========================================================
      Make sure instances of system classes are always visible
      ======================================================== */
   for (ins = startInstance ;
        (ins != NULL) ? (ins->name == startInstance->name) : FALSE ;
        ins = ins->nxtHash)
     if (ins->cls->system)
       return(ins);

   return(NULL);
  }

/***************************************************
  NAME         : PrintInstanceName
  DESCRIPTION  : Used by the rule system commands
                 such as (matches) and (agenda)
                 to print out the name of an instance
  INPUTS       : 1) The logical output name
                 2) A pointer to the instance
  RETURNS      : Nothing useful
  SIDE EFFECTS : Name of instance printed
  NOTES        : None
 ***************************************************/
static void PrintInstanceName(
  char *log,
  void *vins)
  {
   INSTANCE_TYPE *ins;

   ins = (INSTANCE_TYPE *) vins;
   if (ins->garbage)
     {
      PrintRouter(log,"<stale instance [");
      PrintRouter(log,ValueToString(ins->name));
      PrintRouter(log,"]>");
     }
   else
     {
      PrintRouter(log,"[");
      PrintRouter(log,ValueToString(GetFullInstanceName(ins)));
      PrintRouter(log,"]");
     }
  }

/***************************************************
  NAME         : PrintInstanceLongForm
  DESCRIPTION  : Used by kernel to print
                 instance addresses
  INPUTS       : 1) The logical output name
                 2) A pointer to the instance
  RETURNS      : Nothing useful
  SIDE EFFECTS : Address of instance printed
  NOTES        : None
 ***************************************************/
static void PrintInstanceLongForm(
  char *log,
  void *vins)
  {
   INSTANCE_TYPE *ins = (INSTANCE_TYPE *) vins;

   if (InstanceAddressesToNames)
     {
      if (ins == &DummyInstance)
        PrintRouter(log,"\"<Dummy Instance>\"");
      else
        {
         PrintRouter(log,"[");
         PrintRouter(log,ValueToString(GetFullInstanceName(ins)));
         PrintRouter(log,"]");
        }
     }
   else
     {
      if (AddressesToStrings)
        PrintRouter(log,"\"");
      if (ins == &DummyInstance)
        PrintRouter(log,"<Dummy Instance>");
      else if (ins->garbage)
        {
         PrintRouter(log,"<Stale Instance-");
         PrintRouter(log,ValueToString(ins->name));
         PrintRouter(log,">");
        }
      else
        {
         PrintRouter(log,"<Instance-");
         PrintRouter(log,ValueToString(GetFullInstanceName(ins)));
         PrintRouter(log,">");
        }
      if (AddressesToStrings)
        PrintRouter(log,"\"");
     }
  }

#if INSTANCE_PATTERN_MATCHING

/*****************************************************
  NAME         : NetworkModifyForSharedSlot
  DESCRIPTION  : Performs a Rete network modify for
                 all instances which contain a
                 specific shared slot
  INPUTS       : 1) The traversal id to use when
                    recursively entering subclasses
                    to prevent duplicate examinations
                    of a class
                 2) The class
                 3) The descriptor for the shared slot
  RETURNS      : Nothing useful
  SIDE EFFECTS : Instances which contain the shared
                 slot are filtered through the
                 Rete network via a retract/assert
  NOTES        : Assumes traversal id has been
                 established
 *****************************************************/
static void NetworkModifyForSharedSlot(
  int sharedTraversalID,
  DEFCLASS *cls,
  SLOT_DESC *sd)
  {
   INSTANCE_TYPE *ins;
   register unsigned i;

   /* ================================================
      Make sure we haven't already examined this class
      ================================================ */
   if (TestTraversalID(cls->traversalRecord,sharedTraversalID))
     return;
   SetTraversalID(cls->traversalRecord,sharedTraversalID);

   /* ===========================================
      If the instances of this class contain the
      shared slot, send update events to the Rete
      network for all of its instances
      =========================================== */
   if ((sd->slotName->id > cls->maxSlotNameID) ? FALSE :
       ((cls->slotNameMap[sd->slotName->id] == 0) ? FALSE :
        (cls->instanceTemplate[cls->slotNameMap[sd->slotName->id] - 1] == sd)))
     {
      for (ins = cls->instanceList ; ins != NULL ; ins = ins->nxtClass)
        ObjectNetworkAction(OBJECT_MODIFY,(INSTANCE_TYPE *) ins,(int) sd->slotName->id);
     }

   /* ==================================
      Check the subclasses of this class
      ================================== */
   for (i = 0 ; i < cls->directSubclasses.classCount ; i++)
     NetworkModifyForSharedSlot(sharedTraversalID,cls->directSubclasses.classArray[i],sd);
  }

/***************************************************
  NAME         : DecrementObjectBasisCount
  DESCRIPTION  : Decrements the basis count of an
                 object indicating that it is in
                 use by the partial match of the
                 currently executing rule
  INPUTS       : The instance address
  RETURNS      : Nothing useful
  SIDE EFFECTS : Basis count decremented and
                 basis copy (possibly) deleted
  NOTES        : When the count goes to zero, the
                 basis copy of the object (if any)
                 is deleted.
 ***************************************************/
static void DecrementObjectBasisCount(
  void *vins)
  {
   INSTANCE_TYPE *ins;
   register int i;

   ins = (INSTANCE_TYPE *) vins;
   ins->header.busyCount--;
   if (ins->header.busyCount == 0)
     {
      if (ins->garbage)
        RemoveInstanceData(ins);
      if (ins->cls->instanceSlotCount != 0)
        {
         for (i = 0 ; i < ins->cls->instanceSlotCount ; i++)
           if (ins->basisSlots[i].value != NULL)
             {
              if (ins->basisSlots[i].desc->multiple)
                MultifieldDeinstall((struct multifield *) ins->basisSlots[i].value);
              else
                AtomDeinstall((int) ins->basisSlots[i].type,
                              ins->basisSlots[i].value);
             }
         rm((void *) ins->basisSlots,
            (int) (ins->cls->instanceSlotCount * sizeof(INSTANCE_SLOT)));
         ins->basisSlots = NULL;
        }
     }
  }

/***************************************************
  NAME         : IncrementObjectBasisCount
  DESCRIPTION  : Increments the basis count of an
                 object indicating that it is in
                 use by the partial match of the
                 currently executing rule

                 If this the count was zero,
                 allocate an array of extra
                 instance slots for use by
                 slot variables
  INPUTS       : The instance address
  RETURNS      : Nothing useful
  SIDE EFFECTS : Basis count incremented
  NOTES        : None
 ***************************************************/
static void IncrementObjectBasisCount(
  void *vins)
  {
   INSTANCE_TYPE *ins;
   register int i;

   ins = (INSTANCE_TYPE *) vins;
   if (ins->header.busyCount == 0)
     {
      if (ins->cls->instanceSlotCount != 0)
        {
         ins->basisSlots = (INSTANCE_SLOT *)
                            gm2((int) (sizeof(INSTANCE_SLOT) * ins->cls->instanceSlotCount));
         for (i = 0 ; i < ins->cls->instanceSlotCount ; i++)
           {
            ins->basisSlots[i].desc = ins->slotAddresses[i]->desc;
            ins->basisSlots[i].value = NULL;
           }
        }
     }
   ins->header.busyCount++;
  }

/***************************************************
  NAME         : MatchObjectFunction
  DESCRIPTION  : Filters an instance through the
                 object pattern network
                 Used for incremental resets in
                 binary loads and run-time modules
  INPUTS       : The instance
  RETURNS      : Nothing useful
  SIDE EFFECTS : Instance pattern-matched
  NOTES        : None
 ***************************************************/
static void MatchObjectFunction(
  void *vins)
  {
   ObjectNetworkAction(OBJECT_ASSERT,(INSTANCE_TYPE *) vins,-1);
  }

/***************************************************
  NAME         : NetworkSynchronized
  DESCRIPTION  : Determines if state of instance is
                 consistent with last push through
                 pattern-matching network
  INPUTS       : The instance
  RETURNS      : TRUE if instance has not
                 changed since last push through the
                 Rete network, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
static BOOLEAN NetworkSynchronized(
  void *vins)
  {
   return(((INSTANCE_TYPE *) vins)->reteSynchronized);
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
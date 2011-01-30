   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*              CLIPS Version 6.10  04/09/97           */
   /*                                                     */
   /*                INSTANCE COMMAND MODULE              */
   /*******************************************************/

/*************************************************************/
/* Purpose:  Kernel Interface Commands for Instances         */
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

#include "argacces.h"
#include "classcom.h"
#include "classfun.h"
#include "classinf.h"
#include "memalloc.h"
#include "exprnpsr.h"
#include "evaluatn.h"
#include "insfile.h"
#include "insfun.h"
#include "insmngr.h"
#include "insmoddp.h"
#include "insmult.h"
#include "inspsr.h"
#include "msgfun.h"
#include "router.h"
#include "strngrtr.h"
#include "utility.h"
#include "commline.h"

#define _INSCOM_SOURCE_
#include "inscom.h"

/* =========================================
   *****************************************
                   CONSTANTS
   =========================================
   ***************************************** */
#define ALL_QUALIFIER      "inherit"

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

#if DEBUGGING_FUNCTIONS
static long ListInstancesInModule(int,char *,char *,BOOLEAN,BOOLEAN);
static long TabulateInstances(int,char *,DEFCLASS *,BOOLEAN,BOOLEAN);
#endif

static void PrintInstance(char *,INSTANCE_TYPE *,char *);
static INSTANCE_SLOT *FindISlotByName(INSTANCE_TYPE *,char *);

/* =========================================
   *****************************************
       EXTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */

#if THREAD_STORAGE
globle Thread INSTANCE_TYPE DummyInstance = { { NULL }, NULL,NULL, 0, 1 };
#else
globle INSTANCE_TYPE DummyInstance = { { &InstanceInfo }, NULL,NULL, 0, 1 };
#endif

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/*********************************************************
  NAME         : SetupInstances
  DESCRIPTION  : Initializes instance Hash Table,
                   Function Parsers, and Data Structures
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 *********************************************************/
globle void SetupInstances()
  {

   /*===============================*/
   /* Initialize the dummy instance */
   /*===============================*/
   DummyInstance.header.theInfo = &InstanceInfo;

   InitializeInstanceTable();
   InstallPrimitive((struct entityRecord *) &InstanceInfo,INSTANCE_ADDRESS);

#if ! RUN_TIME

#if INSTANCE_PATTERN_MATCHING
   DefineFunction2("initialize-instance",'u',
                  PTIF InactiveInitializeInstance,"InactiveInitializeInstance",NULL);
   DefineFunction2("active-initialize-instance",'u',
                  PTIF InitializeInstanceCommand,"InitializeInstanceCommand",NULL);
   AddFunctionParser("active-initialize-instance",ParseInitializeInstance);

   DefineFunction2("make-instance",'u',PTIF InactiveMakeInstance,"InactiveMakeInstance",NULL);
   DefineFunction2("active-make-instance",'u',PTIF MakeInstanceCommand,"MakeInstanceCommand",NULL);
   AddFunctionParser("active-make-instance",ParseInitializeInstance);

#else
   DefineFunction2("initialize-instance",'u',
                  PTIF InitializeInstanceCommand,"InitializeInstanceCommand",NULL);
   DefineFunction2("make-instance",'u',PTIF MakeInstanceCommand,"MakeInstanceCommand",NULL);
#endif
   AddFunctionParser("initialize-instance",ParseInitializeInstance);
   AddFunctionParser("make-instance",ParseInitializeInstance);

   DefineFunction2("init-slots",'u',PTIF InitSlotsCommand,"InitSlotsCommand","00");

   DefineFunction2("delete-instance",'b',PTIF DeleteInstanceCommand,
                   "DeleteInstanceCommand","00");
   DefineFunction2("unmake-instance",'b',PTIF UnmakeInstanceCommand,
                   "UnmakeInstanceCommand","1*e");

#if DEBUGGING_FUNCTIONS
   DefineFunction2("instances",'v',PTIF InstancesCommand,"InstancesCommand","*3w");
   DefineFunction2("ppinstance",'v',PTIF PPInstanceCommand,"PPInstanceCommand","00");
#endif

   DefineFunction2("symbol-to-instance-name",'u',
                  PTIF SymbolToInstanceName,"SymbolToInstanceName","11w");
   DefineFunction2("instance-name-to-symbol",'w',
                  PTIF InstanceNameToSymbol,"InstanceNameToSymbol","11p");
   DefineFunction2("instance-address",'u',PTIF InstanceAddressCommand,
                   "InstanceAddressCommand","12eep");
   DefineFunction2("instance-addressp",'b',PTIF InstanceAddressPCommand,
                   "InstanceAddressPCommand","11");
   DefineFunction2("instance-namep",'b',PTIF InstanceNamePCommand,
                   "InstanceNamePCommand","11");
   DefineFunction2("instance-name",'u',PTIF InstanceNameCommand,
                   "InstanceNameCommand","11e");
   DefineFunction2("instancep",'b',PTIF InstancePCommand,"InstancePCommand","11");
   DefineFunction2("instance-existp",'b',PTIF InstanceExistPCommand,
                   "InstanceExistPCommand","11e");
   DefineFunction2("class",'u',PTIF ClassCommand,"ClassCommand","11");

   SetupInstanceModDupCommands();
   SetupInstanceFileCommands();
   SetupInstanceMultifieldCommands();

#endif

   AddCleanupFunction("instances",CleanupInstances,0);
   AddResetFunction("instances",DestroyAllInstances,60);
  }

/*******************************************************************
  NAME         : DeleteInstance
  DESCRIPTION  : DIRECTLY removes a named instance from the
                   hash table and its class's
                   instance list
  INPUTS       : The instance address (NULL to delete all instances)
  RETURNS      : 1 if successful, 0 otherwise
  SIDE EFFECTS : Instance is deallocated
  NOTES        : C interface for deleting instances
 *******************************************************************/
globle BOOLEAN DeleteInstance(
  void *iptr)
  {
   INSTANCE_TYPE *ins,*itmp;
   int success = 1;

   if (iptr != NULL)
     return(QuashInstance((INSTANCE_TYPE *) iptr));
   ins = InstanceList;
   while (ins != NULL)
     {
      itmp = ins;
      ins = ins->nxtList;
      if (QuashInstance((INSTANCE_TYPE *) itmp) == 0)
        success = 0;
     }

   if ((CurrentEvaluationDepth == 0) && (! EvaluatingTopLevelCommand) &&
       (CurrentExpression == NULL))
     { PeriodicCleanup(TRUE,FALSE); }

   return(success);
  }

/*******************************************************************
  NAME         : UnmakeInstance
  DESCRIPTION  : Removes a named instance via message-passing
  INPUTS       : The instance address (NULL to delete all instances)
  RETURNS      : 1 if successful, 0 otherwise
  SIDE EFFECTS : Instance is deallocated
  NOTES        : C interface for deleting instances
 *******************************************************************/
globle BOOLEAN UnmakeInstance(
  void *iptr)
  {
   INSTANCE_TYPE *ins;
   int success = 1,svmaintain;

   svmaintain = MaintainGarbageInstances;
   MaintainGarbageInstances = TRUE;
   ins = (INSTANCE_TYPE *) iptr;
   if (ins != NULL)
     {
      if (ins->garbage)
        success = 0;
      else
        {
         DirectMessage(DELETE_SYMBOL,ins,NULL,NULL);
         if (ins->garbage == 0)
           success = 0;
        }
     }
   else
     {
      ins = InstanceList;
      while (ins != NULL)
        {
         DirectMessage(DELETE_SYMBOL,ins,NULL,NULL);
         if (ins->garbage == 0)
           success = 0;
         ins = ins->nxtList;
         while ((ins != NULL) ? ins->garbage : FALSE)
           ins = ins->nxtList;
        }
     }
   MaintainGarbageInstances = svmaintain;
   CleanupInstances();

   if ((CurrentEvaluationDepth == 0) && (! EvaluatingTopLevelCommand) &&
       (CurrentExpression == NULL))
     { PeriodicCleanup(TRUE,FALSE); }

   return(success);
  }

#if DEBUGGING_FUNCTIONS

/*******************************************************************
  NAME         : InstancesCommand
  DESCRIPTION  : Lists all instances associated
                   with a particular class
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : H/L Syntax : (instances [<class-name> [inherit]])
 *******************************************************************/
globle void InstancesCommand()
  {
   int argno, inheritFlag = FALSE;
   void *theDefmodule = (void *) GetCurrentModule();
   char *className = NULL;
   DATA_OBJECT temp;

   argno = RtnArgCount();
   if (argno > 0)
     {
      if (ArgTypeCheck("instances",1,SYMBOL,&temp) == FALSE)
        return;
      theDefmodule = FindDefmodule(DOToString(temp));
      if ((theDefmodule != NULL) ? FALSE :
          (strcmp(DOToString(temp),"*") != 0))
        {
         SetEvaluationError(TRUE);
         ExpectedTypeError1("instances",1,"defmodule name");
         return;
        }
      if (argno > 1)
        {
         if (ArgTypeCheck("instances",2,SYMBOL,&temp) == FALSE)
           return;
         className = DOToString(temp);
         if (LookupDefclassAnywhere((struct defmodule *) theDefmodule,className) == NULL)
           {
            if (strcmp(className,"*") == 0)
              className = NULL;
            else
              {
               ClassExistError("instances",className);
                 return;
              }
           }
         if (argno > 2)
           {
            if (ArgTypeCheck("instances",3,SYMBOL,&temp) == FALSE)
              return;
            if (strcmp(DOToString(temp),ALL_QUALIFIER) != 0)
              {
               SetEvaluationError(TRUE);
               ExpectedTypeError1("instances",3,"keyword \"inherit\"");
               return;
              }
            inheritFlag = TRUE;
           }
        }
     }
   Instances(WDISPLAY,theDefmodule,className,inheritFlag);
  }

/********************************************************
  NAME         : PPInstanceCommand
  DESCRIPTION  : Displays the current slot-values
                   of an instance
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : H/L Syntax : (ppinstance <instance>)
 ********************************************************/
globle void PPInstanceCommand()
  {
   INSTANCE_TYPE *ins;

   if (CheckCurrentMessage("ppinstance",TRUE) == FALSE)
     return;
   ins = GetActiveInstance();
   if (ins->garbage == 1)
     return;
   PrintInstance(WDISPLAY,ins,"\n");
   PrintRouter(WDISPLAY,"\n");
  }

/***************************************************************
  NAME         : Instances
  DESCRIPTION  : Lists instances of classes
  INPUTS       : 1) The logical name for the output
                 2) Address of the module (NULL for all classes)
                 3) Name of the class
                    (NULL for all classes in specified module)
                 4) A flag indicating whether to print instances
                    of subclasses or not
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 **************************************************************/
globle void Instances(
  char *logicalName,
  void *theVModule,
  char *className,
  int inheritFlag)
  {
   int id;
   struct defmodule *theModule;
   long count = 0L;

   /* ===========================================
      Grab a traversal id to avoid printing out
      instances twice due to multiple inheritance
      =========================================== */
  if ((id = GetTraversalID()) == -1)
    return;
  SaveCurrentModule();

   /* ====================================
      For all modules, print out instances
      of specified class(es)
      ==================================== */
   if (theVModule == NULL)
     {
      theModule = (struct defmodule *) GetNextDefmodule(NULL);
      while (theModule != NULL)
        {
         if (GetHaltExecution() == TRUE)
           {
            RestoreCurrentModule();
            ReleaseTraversalID();
            return;
           }

         PrintRouter(logicalName,GetDefmoduleName((void *) theModule));
         PrintRouter(logicalName,":\n");
         SetCurrentModule((void *) theModule);
         count += ListInstancesInModule(id,logicalName,className,inheritFlag,TRUE);
         theModule = (struct defmodule *) GetNextDefmodule((void *) theModule);
        }
     }

   /* ====================================
      For the specified module, print out
      instances of the specified class(es)
      ==================================== */
   else
     {
      SetCurrentModule((void *) theVModule);
      count = ListInstancesInModule(id,logicalName,className,inheritFlag,FALSE);
     }

   RestoreCurrentModule();
   ReleaseTraversalID();
   if (HaltExecution == FALSE)
     PrintTally(logicalName,count,"instance","instances");
  }

#endif

/*********************************************************
  NAME         : MakeInstance
  DESCRIPTION  : C Interface for creating and
                   initializing a class instance
  INPUTS       : The make-instance call string,
                    e.g. "([bill] of man (age 34))"
  RETURNS      : The instance address if instance created,
                    NULL otherwise
  SIDE EFFECTS : Creates the instance and returns
                    the result in caller's buffer
  NOTES        : None
 *********************************************************/
globle void *MakeInstance(
  char *mkstr)
  {
   char *router = "***MKINS***";
   struct token tkn;
   EXPRESSION *top;
   DATA_OBJECT result;

   result.type = SYMBOL;
   result.value = FalseSymbol;
   if (OpenStringSource(router,mkstr,0) == 0)
     return(NULL);
   GetToken(router,&tkn);
   if (tkn.type == LPAREN)
     {
      top = GenConstant(FCALL,(void *) FindFunction("make-instance"));
      if (ParseSimpleInstance(top,router) != NULL)
        {
         GetToken(router,&tkn);
         if (tkn.type == STOP)
           {
            ExpressionInstall(top);
            EvaluateExpression(top,&result);
            ExpressionDeinstall(top);
           }
         else
           SyntaxErrorMessage("instance definition");
         ReturnExpression(top);
        }
     }
   else
     SyntaxErrorMessage("instance definition");
   CloseStringSource(router);

   if ((CurrentEvaluationDepth == 0) && (! EvaluatingTopLevelCommand) &&
       (CurrentExpression == NULL))
     { PeriodicCleanup(TRUE,FALSE); }

   if ((result.type == SYMBOL) && (result.value == FalseSymbol))
     return(NULL);

   return((void *) FindInstanceBySymbol((SYMBOL_HN *) result.value));
  }

/***************************************************************
  NAME         : CreateRawInstance
  DESCRIPTION  : Creates an empty of instance of the specified
                   class.  No slot-overrides or class defaults
                   are applied.
  INPUTS       : 1) Address of class
                 2) Name of the new instance
  RETURNS      : The instance address if instance created,
                    NULL otherwise
  SIDE EFFECTS : Old instance of same name deleted (if possible)
  NOTES        : None
 ***************************************************************/
globle void *CreateRawInstance(
  void *cptr,
  char *iname)
  {
   return((void *) BuildInstance((SYMBOL_HN *) AddSymbol(iname),(DEFCLASS *) cptr,FALSE));
  }

/***************************************************************************
  NAME         : FindInstance
  DESCRIPTION  : Looks up a specified instance in the instance hash table
  INPUTS       : Name-string of the instance
  RETURNS      : The address of the found instance, NULL otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************************************/
globle void *FindInstance(
  void *theModule,
  char *iname,
  BOOLEAN searchImports)
  {
   SYMBOL_HN *isym;

   isym = FindSymbol(iname);
   if (isym == NULL)
     return(NULL);
   if (theModule == NULL)
     theModule = (void *) GetCurrentModule();
   return((void *) FindInstanceInModule(isym,(struct defmodule *) theModule,
                                        ((struct defmodule *) GetCurrentModule()),searchImports));
  }

/***************************************************************************
  NAME         : ValidInstanceAddress
  DESCRIPTION  : Determines if an instance address is still valid
  INPUTS       : Instance address
  RETURNS      : 1 if the address is still valid, 0 otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************************************/
globle int ValidInstanceAddress(
  void *iptr)
  {
   return((((INSTANCE_TYPE *) iptr)->garbage == 0) ? 1 : 0);
  }

/***************************************************
  NAME         : DirectGetSlot
  DESCRIPTION  : Gets a slot value
  INPUTS       : 1) Instance address
                 2) Slot name
                 3) Caller's result buffer
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle void DirectGetSlot(
  void *ins,
  char *sname,
  DATA_OBJECT *result)
  {
   INSTANCE_SLOT *sp;

   if (((INSTANCE_TYPE *) ins)->garbage == 1)
     {
      SetEvaluationError(TRUE);
      result->type = SYMBOL;
      result->value = FalseSymbol;
      return;
     }
   sp = FindISlotByName((INSTANCE_TYPE *) ins,sname);
   if (sp == NULL)
     {
      SetEvaluationError(TRUE);
      result->type = SYMBOL;
      result->value = FalseSymbol;
      return;
     }
   result->type = sp->type;
   result->value = sp->value;
   if (sp->type == MULTIFIELD)
     {
      result->begin = 0;
      result->end = GetInstanceSlotLength(sp) - 1;
     }
   PropagateReturnValue(result);
  }

/*********************************************************
  NAME         : DirectPutSlot
  DESCRIPTION  : Gets a slot value
  INPUTS       : 1) Instance address
                 2) Slot name
                 3) Caller's new value buffer
  RETURNS      : TRUE if put successful, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 *********************************************************/
globle int DirectPutSlot(
  void *ins,
  char *sname,
  DATA_OBJECT *val)
  {
   INSTANCE_SLOT *sp;
   DATA_OBJECT junk;

   if ((((INSTANCE_TYPE *) ins)->garbage == 1) || (val == NULL))
     {
      SetEvaluationError(TRUE);
      return(FALSE);
     }
   sp = FindISlotByName((INSTANCE_TYPE *) ins,sname);
   if (sp == NULL)
     {
      SetEvaluationError(TRUE);
      return(FALSE);
     }

   if (PutSlotValue((INSTANCE_TYPE *) ins,sp,val,&junk,"external put"))
     {
      if ((CurrentEvaluationDepth == 0) && (! EvaluatingTopLevelCommand) &&
          (CurrentExpression == NULL))
        { PeriodicCleanup(TRUE,FALSE); }
      return(TRUE);
     }
   return(FALSE);
  }

/***************************************************
  NAME         : GetInstanceName
  DESCRIPTION  : Returns name of instance
  INPUTS       : Pointer to instance
  RETURNS      : Name of instance
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle char *GetInstanceName(
  void *iptr)
  {
   if (((INSTANCE_TYPE *) iptr)->garbage == 1)
     return(NULL);
   return(ValueToString(((INSTANCE_TYPE *) iptr)->name));
  }

/***************************************************
  NAME         : GetInstanceClass
  DESCRIPTION  : Returns class of instance
  INPUTS       : Pointer to instance
  RETURNS      : Pointer to class of instance
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle void *GetInstanceClass(
  void *iptr)
  {
   if (((INSTANCE_TYPE *) iptr)->garbage == 1)
     return(NULL);
   return((void *) ((INSTANCE_TYPE *) iptr)->cls);
  }

/***************************************************
  NAME         : GetGlobalNumberOfInstances
  DESCRIPTION  : Returns the total number of
                   instances in all modules
  INPUTS       : None
  RETURNS      : The instance count
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle unsigned long GetGlobalNumberOfInstances()
  {
   return(GlobalNumberOfInstances);
  }

/***************************************************
  NAME         : GetNextInstance
  DESCRIPTION  : Returns next instance in list
                 (or first instance in list)
  INPUTS       : Pointer to previous instance
                 (or NULL to get first instance)
  RETURNS      : The next instance or first instance
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle void *GetNextInstance(
  void *iptr)
  {
   if (iptr == NULL)
     return((void *) InstanceList);
   if (((INSTANCE_TYPE *) iptr)->garbage == 1)
     return(NULL);
   return((void *) ((INSTANCE_TYPE *) iptr)->nxtList);
  }

/***************************************************
  NAME         : GetNextInstanceInScope
  DESCRIPTION  : Returns next instance in list
                 (or first instance in list)
                 which class is in scope
  INPUTS       : Pointer to previous instance
                 (or NULL to get first instance)
  RETURNS      : The next instance or first instance
                 which class is in scope of the
                 current module
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle void *GetNextInstanceInScope(
  void *iptr)
  {
   INSTANCE_TYPE *ins = (INSTANCE_TYPE *) iptr;

   if (ins == NULL)
     ins = InstanceList;
   else if (ins->garbage)
     return(NULL);
   else
     ins = ins->nxtList;
   while (ins != NULL)
     {
      if (DefclassInScope(ins->cls,NULL))
        return((void *) ins);
      ins = ins->nxtList;
     }
   return(NULL);
  }

/***************************************************
  NAME         : GetNextInstanceInClass
  DESCRIPTION  : Finds next instance of class
                 (or first instance of class)
  INPUTS       : 1) Class address
                 2) Instance address
                    (NULL to get first instance)
  RETURNS      : The next or first class instance
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle void *GetNextInstanceInClass(
  void *cptr,
  void *iptr)
  {
   if (iptr == NULL)
     return((void *) ((DEFCLASS *) cptr)->instanceList);
   if (((INSTANCE_TYPE *) iptr)->garbage == 1)
     return(NULL);
   return((void *) ((INSTANCE_TYPE *) iptr)->nxtClass);
  }

/***************************************************
  NAME         : GetNextInstanceInClassAndSubclasses
  DESCRIPTION  : Finds next instance of class
                 (or first instance of class) and
                 all of its subclasses
  INPUTS       : 1) Class address
                 2) Instance address
                    (NULL to get first instance)
  RETURNS      : The next or first class instance
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle void *GetNextInstanceInClassAndSubclasses(
  void **cptr,
  void *iptr,
  DATA_OBJECT *iterationInfo)
  {
   INSTANCE_TYPE *nextInstance;
   DEFCLASS *theClass;
   
   theClass = (DEFCLASS *) *cptr;
   
   if (iptr == NULL)
     {
      ClassSubclassAddresses(theClass,iterationInfo,TRUE);
      nextInstance = theClass->instanceList;
     }
   else if (((INSTANCE_TYPE *) iptr)->garbage == 1)
     { nextInstance = NULL; }
   else
     { nextInstance = ((INSTANCE_TYPE *) iptr)->nxtClass; }
     
   while ((nextInstance == NULL) && 
          (GetpDOBegin(iterationInfo) <= GetpDOEnd(iterationInfo)))
     {
      theClass = (struct defclass *) GetMFValue(DOPToPointer(iterationInfo),
                                                GetpDOBegin(iterationInfo));
      *cptr = theClass;
      SetpDOBegin(iterationInfo,GetpDOBegin(iterationInfo) + 1);
      nextInstance = theClass->instanceList;
     }
          
   return(nextInstance);
  }
  
/***************************************************
  NAME         : GetInstancePPForm
  DESCRIPTION  : Writes slot names and values to
                  caller's buffer
  INPUTS       : 1) Caller's buffer
                 2) Size of buffer (not including
                    space for terminating '\0')
                 3) Instance address
  RETURNS      : Nothing useful
  SIDE EFFECTS : Caller's buffer written
  NOTES        : None
 ***************************************************/
globle void GetInstancePPForm(
  char *buf,
  int buflen,
  void *iptr)
  {
   char *pbuf = "***InstancePPForm***";

   if (((INSTANCE_TYPE *) iptr)->garbage == 1)
     return;
   if (OpenStringDestination(pbuf,buf,buflen+1) == 0)
     return;
   PrintInstance(pbuf,(INSTANCE_TYPE *) iptr," ");
   CloseStringDestination(pbuf);
  }

/*********************************************************
  NAME         : ClassCommand
  DESCRIPTION  : Returns the class of an instance
  INPUTS       : Caller's result buffer
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : H/L Syntax : (class <object>)
                 Can also be called by (type <object>)
                   if you have generic functions installed
 *********************************************************/
globle void ClassCommand(
  DATA_OBJECT *result)
  {
   INSTANCE_TYPE *ins;
   char *func;
   DATA_OBJECT temp;

   func = ValueToString(((struct FunctionDefinition *)
                       CurrentExpression->value)->callFunctionName);
   result->type = SYMBOL;
   result->value = FalseSymbol;
   EvaluateExpression(GetFirstArgument(),&temp);
   if (temp.type == INSTANCE_ADDRESS)
     {
      ins = (INSTANCE_TYPE *) temp.value;
      if (ins->garbage == 1)
        {
         StaleInstanceAddress(func,0);
         SetEvaluationError(TRUE);
         return;
        }
      result->value = (void *) GetDefclassNamePointer((void *) ins->cls);
     }
   else if (temp.type == INSTANCE_NAME)
     {
      ins = FindInstanceBySymbol((SYMBOL_HN *) temp.value);
      if (ins == NULL)
        {
         NoInstanceError(ValueToString(temp.value),func);
         return;
        }
      result->value = (void *) GetDefclassNamePointer((void *) ins->cls);
     }
   else
     {
      switch (temp.type)
        {
         case INTEGER          :
         case FLOAT            :
         case SYMBOL           :
         case STRING           :
         case MULTIFIELD       :
         case EXTERNAL_ADDRESS :
         case FACT_ADDRESS     :
                          result->value = (void *)
                                           GetDefclassNamePointer((void *)
                                            PrimitiveClassMap[temp.type]);
                         return;
         default       : PrintErrorID("INSCOM",1,FALSE);
                         PrintRouter(WERROR,"Undefined type in function ");
                         PrintRouter(WERROR,func);
                         PrintRouter(WERROR,".\n");
                         SetEvaluationError(TRUE);
        }
     }
  }

/******************************************************
  NAME         : DeleteInstanceCommand
  DESCRIPTION  : Removes a named instance from the
                   hash table and its class's
                   instance list
  INPUTS       : None
  RETURNS      : TRUE if successful,
                 FALSE otherwise
  SIDE EFFECTS : Instance is deallocated
  NOTES        : This is an internal function that
                   only be called by a handler
 ******************************************************/
globle BOOLEAN DeleteInstanceCommand()
  {
   if (CheckCurrentMessage("delete-instance",TRUE))
     return(QuashInstance(GetActiveInstance()));
   return(FALSE);
  }

/********************************************************************
  NAME         : UnmakeInstanceCommand
  DESCRIPTION  : Uses message-passing to delete the
                   specified instance
  INPUTS       : None
  RETURNS      : TRUE if successful, FALSE otherwise
  SIDE EFFECTS : Instance is deallocated
  NOTES        : Syntax: (unmake-instance <instance-expression>+ | *)
 ********************************************************************/
globle BOOLEAN UnmakeInstanceCommand()
  {
   EXPRESSION *theArgument;
   DATA_OBJECT theResult;
   INSTANCE_TYPE *ins;
   int argNumber = 1,rtn = TRUE;

   theArgument = GetFirstArgument();
   while (theArgument != NULL)
     {
      EvaluateExpression(theArgument,&theResult);
      if ((theResult.type == INSTANCE_NAME) || (theResult.type == SYMBOL))
        {
         ins = FindInstanceBySymbol((SYMBOL_HN *) theResult.value);
         if ((ins == NULL) ? (strcmp(DOToString(theResult),"*") != 0) : FALSE)
           {
            NoInstanceError(DOToString(theResult),"unmake-instance");
            return(FALSE);
           }
         }
      else if (theResult.type == INSTANCE_ADDRESS)
        {
         ins = (INSTANCE_TYPE *) theResult.value;
         if (ins->garbage)
           {
            StaleInstanceAddress("unmake-instance",0);
            SetEvaluationError(TRUE);
            return(FALSE);
           }
        }
      else
        {
         ExpectedTypeError1("retract",argNumber,"instance-address, instance-name, or the symbol *");
         SetEvaluationError(TRUE);
         return(FALSE);
        }
      if (UnmakeInstance(ins) == FALSE)
        rtn = FALSE;
      if (ins == NULL)
        return(rtn);
      argNumber++;
      theArgument = GetNextArgument(theArgument);
     }
   return(rtn);
  }

/*****************************************************************
  NAME         : SymbolToInstanceName
  DESCRIPTION  : Converts a symbol from type SYMBOL
                   to type INSTANCE_NAME
  INPUTS       : The address of the value buffer
  RETURNS      : The new INSTANCE_NAME symbol
  SIDE EFFECTS : None
  NOTES        : H/L Syntax : (symbol-to-instance-name <symbol>)
 *****************************************************************/
globle void SymbolToInstanceName(
  DATA_OBJECT *result)
  {
   if (ArgTypeCheck("symbol-to-instance-name",1,SYMBOL,result) == FALSE)
     {
      SetpType(result,SYMBOL);
      SetpValue(result,FalseSymbol);
      return;
     }
   SetpType(result,INSTANCE_NAME);
  }

/*****************************************************************
  NAME         : InstanceNameToSymbol
  DESCRIPTION  : Converts a symbol from type INSTANCE_NAME
                   to type SYMBOL
  INPUTS       : None
  RETURNS      : Symbol FALSE on errors - or converted instance name
  SIDE EFFECTS : None
  NOTES        : H/L Syntax : (instance-name-to-symbol <iname>)
 *****************************************************************/
globle void *InstanceNameToSymbol()
  {
   DATA_OBJECT result;

   if (ArgTypeCheck("instance-name-to-symbol",1,INSTANCE_NAME,&result) == FALSE)
     return(FalseSymbol);
   return(result.value);
  }

/*********************************************************************************
  NAME         : InstanceAddressCommand
  DESCRIPTION  : Returns the address of an instance
  INPUTS       : The address of the value buffer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Stores instance address in caller's buffer
  NOTES        : H/L Syntax : (instance-address [<module-name>] <instance-name>)
 *********************************************************************************/
globle void InstanceAddressCommand(
  DATA_OBJECT *result)
  {
   INSTANCE_TYPE *ins;
   DATA_OBJECT temp;
   struct defmodule *theModule;
   int searchImports;

   result->type = SYMBOL;
   result->value = FalseSymbol;
   if (RtnArgCount() > 1)
     {
      if (ArgTypeCheck("instance-address",1,SYMBOL,&temp) == FALSE)
        return;
      theModule = (struct defmodule *) FindDefmodule(DOToString(temp));
      if ((theModule == NULL) ? (strcmp(DOToString(temp),"*") != 0) : FALSE)
        {
         ExpectedTypeError1("instance-address",1,"module name");
         SetEvaluationError(TRUE);
         return;
        }
      if (theModule == NULL)
        {
         searchImports = TRUE;
         theModule = ((struct defmodule *) GetCurrentModule());
        }
      else
        searchImports = FALSE;
      if (ArgTypeCheck("instance-address",2,INSTANCE_NAME,&temp)
             == FALSE)
        return;
      ins = FindInstanceInModule((SYMBOL_HN *) temp.value,theModule,
                                 ((struct defmodule *) GetCurrentModule()),searchImports);
      if (ins != NULL)
        {
         result->type = INSTANCE_ADDRESS;
         result->value = (void *) ins;
        }
      else
        NoInstanceError(ValueToString(temp.value),"instance-address");
     }
   else if (ArgTypeCheck("instance-address",1,INSTANCE_OR_INSTANCE_NAME,&temp))
     {
      if (temp.type == INSTANCE_ADDRESS)
        {
         ins = (INSTANCE_TYPE *) temp.value;
         if (ins->garbage == 0)
           {
            result->type = INSTANCE_ADDRESS;
            result->value = temp.value;
           }
         else
           {
            StaleInstanceAddress("instance-address",0);
            SetEvaluationError(TRUE);
           }
        }
      else
        {
         ins = FindInstanceBySymbol((SYMBOL_HN *) temp.value);
         if (ins != NULL)
           {
            result->type = INSTANCE_ADDRESS;
            result->value = (void *) ins;
           }
         else
           NoInstanceError(ValueToString(temp.value),"instance-address");
        }
     }
  }

/***************************************************************
  NAME         : InstanceNameCommand
  DESCRIPTION  : Gets the name of an INSTANCE
  INPUTS       : The address of the value buffer
  RETURNS      : The INSTANCE_NAME symbol
  SIDE EFFECTS : None
  NOTES        : H/L Syntax : (instance-name <instance>)
 ***************************************************************/
globle void InstanceNameCommand(
  DATA_OBJECT *result)
  {
   INSTANCE_TYPE *ins;
   DATA_OBJECT temp;

   result->type = SYMBOL;
   result->value = FalseSymbol;
   if (ArgTypeCheck("instance-name",1,INSTANCE_OR_INSTANCE_NAME,&temp) == FALSE)
     return;
   if (temp.type == INSTANCE_ADDRESS)
     {
      ins = (INSTANCE_TYPE *) temp.value;
      if (ins->garbage == 1)
        {
         StaleInstanceAddress("instance-name",0);
         SetEvaluationError(TRUE);
         return;
        }
     }
   else
     {
      ins = FindInstanceBySymbol((SYMBOL_HN *) temp.value);
      if (ins == NULL)
        {
         NoInstanceError(ValueToString(temp.value),"instance-name");
         return;
        }
     }
   result->type = INSTANCE_NAME;
   result->value = (void *) ins->name;
  }

/**************************************************************
  NAME         : InstanceAddressPCommand
  DESCRIPTION  : Determines if a value is of type INSTANCE
  INPUTS       : None
  RETURNS      : TRUE if type INSTANCE_ADDRESS, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : H/L Syntax : (instance-addressp <arg>)
 **************************************************************/
globle BOOLEAN InstanceAddressPCommand()
  {
   DATA_OBJECT temp;

   EvaluateExpression(GetFirstArgument(),&temp);
   return((GetType(temp) == INSTANCE_ADDRESS) ? TRUE : FALSE);
  }

/**************************************************************
  NAME         : InstanceNamePCommand
  DESCRIPTION  : Determines if a value is of type INSTANCE_NAME
  INPUTS       : None
  RETURNS      : TRUE if type INSTANCE_NAME, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : H/L Syntax : (instance-namep <arg>)
 **************************************************************/
globle BOOLEAN InstanceNamePCommand()
  {
   DATA_OBJECT temp;

   EvaluateExpression(GetFirstArgument(),&temp);
   return((GetType(temp) == INSTANCE_NAME) ? TRUE : FALSE);
  }

/*****************************************************************
  NAME         : InstancePCommand
  DESCRIPTION  : Determines if a value is of type INSTANCE_ADDRESS
                   or INSTANCE_NAME
  INPUTS       : None
  RETURNS      : TRUE if type INSTANCE_NAME or INSTANCE_ADDRESS,
                     FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : H/L Syntax : (instancep <arg>)
 *****************************************************************/
globle BOOLEAN InstancePCommand()
  {
   DATA_OBJECT temp;

   EvaluateExpression(GetFirstArgument(),&temp);
   if ((GetType(temp) == INSTANCE_NAME) || (GetType(temp) == INSTANCE_ADDRESS))
     return(TRUE);
   return(FALSE);
  }

/********************************************************
  NAME         : InstanceExistPCommand
  DESCRIPTION  : Determines if an instance exists
  INPUTS       : None
  RETURNS      : TRUE if instance exists, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : H/L Syntax : (instance-existp <arg>)
 ********************************************************/
globle BOOLEAN InstanceExistPCommand()
  {
   DATA_OBJECT temp;

   EvaluateExpression(GetFirstArgument(),&temp);
   if (temp.type == INSTANCE_ADDRESS)
     return((((INSTANCE_TYPE *) temp.value)->garbage == 0) ? TRUE : FALSE);
   if ((temp.type == INSTANCE_NAME) || (temp.type == SYMBOL))
     return((FindInstanceBySymbol((SYMBOL_HN *) temp.value) != NULL) ?
             TRUE : FALSE);
   ExpectedTypeError1("instance-existp",1,"instance name, instance address or symbol");
   SetEvaluationError(TRUE);
   return(FALSE);
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

#if DEBUGGING_FUNCTIONS

/***************************************************
  NAME         : ListInstancesInModule
  DESCRIPTION  : List instances of specified
                 class(es) in a module
  INPUTS       : 1) Traversal id to avoid multiple
                    passes over same class
                 2) Logical name of output
                 3) The name of the class
                    (NULL for all classes)
                 4) Flag indicating whether to
                    include instances of subclasses
                 5) A flag indicating whether to
                    indent because of module name
  RETURNS      : The number of instances listed
  SIDE EFFECTS : Instances listed to logical output
  NOTES        : Assumes defclass scope flags
                 are up to date
 ***************************************************/
static long ListInstancesInModule(
  int id,
  char *logicalName,
  char *className,
  BOOLEAN inheritFlag,
  BOOLEAN allModulesFlag)
  {
   void *theDefclass,*theInstance;
   long count = 0L;

   /* ===================================
      For the specified module, print out
      instances of all the classes
      =================================== */
   if (className == NULL)
     {
      /* ==============================================
         If instances are being listed for all modules,
         only list the instances of classes in this
         module (to avoid listing instances twice)
         ============================================== */
      if (allModulesFlag)
        {
         for (theDefclass = GetNextDefclass(NULL) ;
              theDefclass != NULL ;
              theDefclass = GetNextDefclass(theDefclass))
           count += TabulateInstances(id,logicalName,
                        (DEFCLASS *) theDefclass,FALSE,allModulesFlag);
        }

      /* ===================================================
         If instances are only be listed for one module,
         list all instances visible to the module (including
         ones belonging to classes in other modules)
         =================================================== */
      else
        {
         theInstance = GetNextInstanceInScope(NULL);
         while (theInstance != NULL)
           {
            if (GetHaltExecution() == TRUE)
              { return(count); }

            count++;
            PrintInstanceNameAndClass(logicalName,(INSTANCE_TYPE *) theInstance,TRUE);
            theInstance = GetNextInstanceInScope(theInstance);
           }
        }
     }

   /* ===================================
      For the specified module, print out
      instances of the specified class
      =================================== */
   else
     {
      theDefclass = (void *) LookupDefclassAnywhere(((struct defmodule *) GetCurrentModule()),className);
      if (theDefclass != NULL)
        {
         count += TabulateInstances(id,logicalName,
                      (DEFCLASS *) theDefclass,inheritFlag,allModulesFlag);
        }
      else if (! allModulesFlag)
        ClassExistError("instances",className);
     }
   return(count);
  }

/******************************************************
  NAME         : TabulateInstances
  DESCRIPTION  : Displays all instances for a class
  INPUTS       : 1) The traversal id for the classes
                 2) The logical name of the output
                 3) The class address
                 4) A flag indicating whether to
                    print out instances of subclasses
                    or not.
                 5) A flag indicating whether to
                    indent because of module name
  RETURNS      : The number of instances (including
                    subclasses' instances)
  SIDE EFFECTS : None
  NOTES        : None
 ******************************************************/
static long TabulateInstances(
  int id,
  char *logicalName,
  DEFCLASS *cls,
  BOOLEAN inheritFlag,
  BOOLEAN allModulesFlag)
  {
   INSTANCE_TYPE *ins;
   register unsigned i;
   long count = 0;

   if (TestTraversalID(cls->traversalRecord,id))
     return(0L);
   SetTraversalID(cls->traversalRecord,id);
   for (ins = cls->instanceList ; ins != NULL ; ins = ins->nxtClass)
     {
      if (HaltExecution)
        return(count);
      if (allModulesFlag)
        PrintRouter(logicalName,"   ");
      PrintInstanceNameAndClass(logicalName,ins,TRUE);
      count++;
     }
   if (inheritFlag)
     {
      for (i = 0 ; i < cls->directSubclasses.classCount ; i++)
        {
         if (HaltExecution)
           return(count);
         count += TabulateInstances(id,logicalName,
                     cls->directSubclasses.classArray[i],inheritFlag,allModulesFlag);
        }
     }
   return(count);
  }

#endif

/***************************************************
  NAME         : PrintInstance
  DESCRIPTION  : Displays an instance's slots
  INPUTS       : 1) Logical name for output
                 2) Instance address
                 3) String used to separate
                    slot printouts
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : Assumes instance is valid
 ***************************************************/
static void PrintInstance(
  char *logicalName,
  INSTANCE_TYPE *ins,
  char *separator)
  {
   register int i;
   register INSTANCE_SLOT *sp;

   PrintInstanceNameAndClass(logicalName,ins,FALSE);
   for (i = 0 ; i < ins->cls->instanceSlotCount ; i++)
     {
      PrintRouter(logicalName,separator);
      sp = ins->slotAddresses[i];
      PrintRouter(logicalName,"(");
      PrintRouter(logicalName,ValueToString(sp->desc->slotName->name));
      if (sp->type != MULTIFIELD)
        {
         PrintRouter(logicalName," ");
         PrintAtom(logicalName,(int) sp->type,sp->value);
        }
      else if (GetInstanceSlotLength(sp) != 0)
        {
         PrintRouter(logicalName," ");
         PrintMultifield(logicalName,(MULTIFIELD_PTR) sp->value,0,
                         GetInstanceSlotLength(sp) - 1,FALSE);
        }
      PrintRouter(logicalName,")");
     }
  }

/***************************************************
  NAME         : FindISlotByName
  DESCRIPTION  : Looks up an instance slot by
                   instance name and slot name
  INPUTS       : 1) Instance address
                 2) Instance name-string
  RETURNS      : The instance slot address, NULL if
                   does not exist
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
static INSTANCE_SLOT *FindISlotByName(
  INSTANCE_TYPE *ins,
  char *sname)
  {
   SYMBOL_HN *ssym;

   ssym = FindSymbol(sname);
   if (ssym == NULL)
     return(NULL);
   return(FindInstanceSlot(ins,ssym));
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
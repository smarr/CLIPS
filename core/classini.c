   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/13/98          */
   /*                                                     */
   /*               CLASS INITIALIZATION MODULE           */
   /*******************************************************/

/**************************************************************/
/* Purpose: Defclass Initialization Routines                  */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Brian L. Donnell                                      */
/*                                                            */
/* Contributing Programmer(s):                                */
/*                                                            */
/* Revision History:                                          */
/*                                                            */
/* Who               |     Date    | Description              */
/* ------------------+-------------+------------------------  */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS        */
/**************************************************************/

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "setup.h"

#if OBJECT_SYSTEM

#ifndef _STDIO_INCLUDED_
#define _STDIO_INCLUDED_
#include <stdio.h>
#endif

#include "classcom.h"
#include "classexm.h"
#include "classfun.h"
#include "classinf.h"
#include "classpsr.h"
#include "memalloc.h"
#include "cstrccom.h"
#include "cstrcpsr.h"
#include "extnfunc.h"
#include "inscom.h"
#include "modulpsr.h"
#include "modulutl.h"
#include "msgcom.h"
#include "watch.h"

#if DEFINSTANCES_CONSTRUCT
#include "defins.h"
#endif

#if INSTANCE_SET_QUERIES
#include "insquery.h"
#endif

#if BLOAD_AND_BSAVE || BLOAD || BLOAD_ONLY
#include "objbin.h"
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
#include "objcmp.h"
#endif

#if INSTANCE_PATTERN_MATCHING
#include "objrtbld.h"
#endif

#if RUN_TIME
#include "insfun.h"
#include "msgfun.h"
#endif

#include "classini.h"

/* =========================================
   *****************************************
                   CONSTANTS
   =========================================
   ***************************************** */
#define SUPERCLASS_RLN       "is-a"
#define NAME_RLN             "name"
#define INITIAL_OBJECT_NAME  "initial-object"

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

static void SetupDefclasses(void);

#if (! RUN_TIME)
static DEFCLASS *AddSystemClass(char *,DEFCLASS *);
static void *AllocateModule(void);
static void  ReturnModule(void *);
#endif

#if (! BLOAD_ONLY) && (! RUN_TIME) && DEFMODULE_CONSTRUCT
static void UpdateDefclassesScope(void);
#endif

/* =========================================
   *****************************************
      INTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */
Thread globle int DefclassModuleIndex;
Thread globle struct construct *DefclassConstruct;

/* =========================================
   *****************************************
      INTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */

Thread static ENTITY_RECORD DefclassEntityRecord = { "DEFCLASS_PTR", DEFCLASS_PTR,1,0,0,
                                              NULL,NULL,NULL,NULL,NULL,
                                              DecrementDefclassBusyCount,
                                              IncrementDefclassBusyCount,
                                              NULL,NULL,NULL,NULL };

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/**********************************************************
  NAME         : SetupObjectSystem
  DESCRIPTION  : Initializes all COOL constructs, functions,
                   and data structures
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : COOL initialized
  NOTES        : Order of setup calls is important
 **********************************************************/
globle void SetupObjectSystem()
  {
#if ! RUN_TIME
   ISA_SYMBOL = (SYMBOL_HN *) AddSymbol(SUPERCLASS_RLN);
   IncrementSymbolCount(ISA_SYMBOL);
   NAME_SYMBOL = (SYMBOL_HN *) AddSymbol(NAME_RLN);
   IncrementSymbolCount(NAME_SYMBOL);
#if INSTANCE_PATTERN_MATCHING
   INITIAL_OBJECT_SYMBOL = (SYMBOL_HN *) AddSymbol(INITIAL_OBJECT_NAME);
   IncrementSymbolCount(INITIAL_OBJECT_SYMBOL);
#endif
#endif

   SetupDefclasses();
   SetupInstances();
   SetupMessageHandlers();

#if DEFINSTANCES_CONSTRUCT
   SetupDefinstances();
#endif

#if INSTANCE_SET_QUERIES
   SetupQuery();
#endif

#if BLOAD_AND_BSAVE || BLOAD || BLOAD_ONLY
   SetupObjectsBload();
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
   SetupObjectsCompiler();
#endif

#if INSTANCE_PATTERN_MATCHING
   SetupObjectPatternStuff();
#endif
  }

#if RUN_TIME

/***************************************************
  NAME         : ObjectsRunTimeInitialize
  DESCRIPTION  : Initializes objects system lists
                   in a run-time module
  INPUTS       : 1) Pointer to new class hash table
                 2) Pointer to new slot name table
  RETURNS      : Nothing useful
  SIDE EFFECTS : Global pointers set
  NOTES        : None
 ***************************************************/
globle void ObjectsRunTimeInitialize(
  DEFCLASS *ctable[],
  SLOT_NAME *sntable[],
  DEFCLASS **cidmap,
  unsigned mid)
  {
   DEFCLASS *cls;
   void *tmpexp;
   register int i,j;

   if (ClassTable != NULL)
     {
      for (j = 0 ; j < CLASS_TABLE_HASH_SIZE ; j++)
        for (cls = ClassTable[j] ; cls != NULL ; cls = cls->nxtHash)
          {
           for (i = 0 ; i < cls->slotCount ; i++)
             {
              /* =====================================================================
                 For static default values, the data object value needs to deinstalled
                 and deallocated, and the expression needs to be restored (which was
                 temporarily stored in the supplementalInfo field of the data object)
                 ===================================================================== */
              if ((cls->slots[i].defaultValue != NULL) && (cls->slots[i].dynamicDefault == 0))
                {
                 tmpexp = ((DATA_OBJECT *) cls->slots[i].defaultValue)->supplementalInfo;
                 ValueDeinstall((DATA_OBJECT *) cls->slots[i].defaultValue);
                 rtn_struct(dataObject,cls->slots[i].defaultValue);
                 cls->slots[i].defaultValue = tmpexp;
                }
             }
          }
     }

   QUERY_DELIMETER_SYMBOL = FindSymbol(QUERY_DELIMETER_STRING);
   INIT_SYMBOL = FindSymbol(INIT_STRING);
   DELETE_SYMBOL = FindSymbol(DELETE_STRING);
   ISA_SYMBOL = FindSymbol(SUPERCLASS_RLN);
   NAME_SYMBOL = FindSymbol(NAME_RLN);
#if INSTANCE_PATTERN_MATCHING
   INITIAL_OBJECT_SYMBOL = FindSymbol(INITIAL_OBJECT_NAME);
#endif

   ClassTable = (DEFCLASS **) ctable;
   SlotNameTable = (SLOT_NAME **) sntable;
   ClassIDMap = (DEFCLASS **) cidmap;
   MaxClassID = (unsigned short) mid;
   PrimitiveClassMap[FLOAT] =
     LookupDefclassByMdlOrScope(FLOAT_TYPE_NAME);
   PrimitiveClassMap[INTEGER] =
     LookupDefclassByMdlOrScope(INTEGER_TYPE_NAME);
   PrimitiveClassMap[STRING] =
     LookupDefclassByMdlOrScope(STRING_TYPE_NAME);
   PrimitiveClassMap[SYMBOL] =
     LookupDefclassByMdlOrScope(SYMBOL_TYPE_NAME);
   PrimitiveClassMap[MULTIFIELD] =
     LookupDefclassByMdlOrScope(MULTIFIELD_TYPE_NAME);
   PrimitiveClassMap[EXTERNAL_ADDRESS] =
     LookupDefclassByMdlOrScope(EXTERNAL_ADDRESS_TYPE_NAME);
   PrimitiveClassMap[FACT_ADDRESS] =
     LookupDefclassByMdlOrScope(FACT_ADDRESS_TYPE_NAME);
   PrimitiveClassMap[INSTANCE_NAME] =
     LookupDefclassByMdlOrScope(INSTANCE_NAME_TYPE_NAME);
   PrimitiveClassMap[INSTANCE_ADDRESS] =
     LookupDefclassByMdlOrScope(INSTANCE_ADDRESS_TYPE_NAME);

   for (j = 0 ; j < CLASS_TABLE_HASH_SIZE ; j++)
     for (cls = ClassTable[j] ; cls != NULL ; cls = cls->nxtHash)
     {
      for (i = 0 ; i < cls->slotCount ; i++)
        {
         if ((cls->slots[i].defaultValue != NULL) && (cls->slots[i].dynamicDefault == 0))
           {
            tmpexp = cls->slots[i].defaultValue;
            cls->slots[i].defaultValue = (void *) get_struct(dataObject);
            EvaluateAndStoreInDataObject((int) cls->slots[i].multiple,(EXPRESSION *) tmpexp,
                                         (DATA_OBJECT *) cls->slots[i].defaultValue);
            ValueInstall((DATA_OBJECT *) cls->slots[i].defaultValue);
            ((DATA_OBJECT *) cls->slots[i].defaultValue)->supplementalInfo = tmpexp;
           }
        }
     }
  }

#else

/***************************************************************
  NAME         : CreateSystemClasses
  DESCRIPTION  : Creates the built-in system classes
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : System classes inserted in the
                   class hash table
  NOTES        : The binary/load save indices for the primitive
                   types (integer, float, symbol and string,
                   multifield, external-address and fact-address)
                   are very important.  Need to be able to refer
                   to types with the same index regardless of
                   whether the object system is installed or
                   not.  Thus, the bsave/blaod indices of these
                   classes match their integer codes.
                WARNING!!: Assumes no classes exist yet!
 ***************************************************************/
globle void CreateSystemClasses()
  {
   DEFCLASS *user,*any,*primitive,*number,*lexeme,*address,*instance;
#if INSTANCE_PATTERN_MATCHING
   DEFCLASS *initialObject;
#endif

   /* ===================================
      Add canonical slot name entries for
      the is-a and name fields - used for
      object patterns
      =================================== */
   AddSlotName(ISA_SYMBOL,ISA_ID,TRUE);
   AddSlotName(NAME_SYMBOL,NAME_ID,TRUE);

   /* =========================================================
      Bsave Indices for non-primitive classes start at 9
               Object is 9, Primitive is 10, Number is 11,
               Lexeme is 12, Address is 13, and Instance is 14.
      because: float = 0, integer = 1, symbol = 2, string = 3,
               multifield = 4, and external-address = 5 and
               fact-address = 6, instance-adress = 7 and
               instance-name = 8.
      ========================================================= */
   any = AddSystemClass(OBJECT_TYPE_NAME,NULL);
   primitive = AddSystemClass(PRIMITIVE_TYPE_NAME,any);
   user = AddSystemClass(USER_TYPE_NAME,any);

   number = AddSystemClass(NUMBER_TYPE_NAME,primitive);
   PrimitiveClassMap[INTEGER] = AddSystemClass(INTEGER_TYPE_NAME,number);
   PrimitiveClassMap[FLOAT] = AddSystemClass(FLOAT_TYPE_NAME,number);
   lexeme = AddSystemClass(LEXEME_TYPE_NAME,primitive);
   PrimitiveClassMap[SYMBOL] = AddSystemClass(SYMBOL_TYPE_NAME,lexeme);
   PrimitiveClassMap[STRING] = AddSystemClass(STRING_TYPE_NAME,lexeme);
   PrimitiveClassMap[MULTIFIELD] = AddSystemClass(MULTIFIELD_TYPE_NAME,primitive);
   address = AddSystemClass(ADDRESS_TYPE_NAME,primitive);
   PrimitiveClassMap[EXTERNAL_ADDRESS] = AddSystemClass(EXTERNAL_ADDRESS_TYPE_NAME,address);
   PrimitiveClassMap[FACT_ADDRESS] = AddSystemClass(FACT_ADDRESS_TYPE_NAME,address);
   instance = AddSystemClass(INSTANCE_TYPE_NAME,primitive);
   PrimitiveClassMap[INSTANCE_ADDRESS] = AddSystemClass(INSTANCE_ADDRESS_TYPE_NAME,instance);
   PrimitiveClassMap[INSTANCE_NAME] = AddSystemClass(INSTANCE_NAME_TYPE_NAME,instance);
#if INSTANCE_PATTERN_MATCHING
   initialObject = AddSystemClass(INITIAL_OBJECT_CLASS_NAME,user);
   initialObject->abstract = 0;
   initialObject->reactive = 1;
#endif

   /* ================================================================================
       INSTANCE-ADDRESS is-a INSTANCE and ADDRESS.  The links between INSTANCE-ADDRESS
       and ADDRESS still need to be made.
       =============================================================================== */
   AddClassLink(&PrimitiveClassMap[INSTANCE_ADDRESS]->directSuperclasses,address,-1);
   AddClassLink(&PrimitiveClassMap[INSTANCE_ADDRESS]->allSuperclasses,address,2);
   AddClassLink(&address->directSubclasses,PrimitiveClassMap[INSTANCE_ADDRESS],-1);

   /* =======================================================================
      The order of the class in the list MUST correspond to their type codes!
      See CONSTANT.H
      ======================================================================= */
   AddConstructToModule((struct constructHeader *) PrimitiveClassMap[FLOAT]);
   AddConstructToModule((struct constructHeader *) PrimitiveClassMap[INTEGER]);
   AddConstructToModule((struct constructHeader *) PrimitiveClassMap[SYMBOL]);
   AddConstructToModule((struct constructHeader *) PrimitiveClassMap[STRING]);
   AddConstructToModule((struct constructHeader *) PrimitiveClassMap[MULTIFIELD]);
   AddConstructToModule((struct constructHeader *) PrimitiveClassMap[EXTERNAL_ADDRESS]);
   AddConstructToModule((struct constructHeader *) PrimitiveClassMap[FACT_ADDRESS]);
   AddConstructToModule((struct constructHeader *) PrimitiveClassMap[INSTANCE_ADDRESS]);
   AddConstructToModule((struct constructHeader *) PrimitiveClassMap[INSTANCE_NAME]);
   AddConstructToModule((struct constructHeader *) any);
   AddConstructToModule((struct constructHeader *) primitive);
   AddConstructToModule((struct constructHeader *) number);
   AddConstructToModule((struct constructHeader *) lexeme);
   AddConstructToModule((struct constructHeader *) address);
   AddConstructToModule((struct constructHeader *) instance);
   AddConstructToModule((struct constructHeader *) user);
#if INSTANCE_PATTERN_MATCHING
   AddConstructToModule((struct constructHeader *) initialObject);
#endif
   for (any = (DEFCLASS *) GetNextDefclass(NULL) ;
        any != NULL ;
        any = (DEFCLASS *) GetNextDefclass((void *) any))
     AssignClassID(any);
  }

#endif

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/*********************************************************
  NAME         : SetupDefclasses
  DESCRIPTION  : Initializes Class Hash Table,
                   Function Parsers, and Data Structures
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS :
  NOTES        : None
 *********************************************************/
static void SetupDefclasses()
  {
   InstallPrimitive(&DefclassEntityRecord,DEFCLASS_PTR);

   DefclassModuleIndex =
                RegisterModuleItem("defclass",
#if (! RUN_TIME)
                                    AllocateModule,ReturnModule,
#else
                                    NULL,NULL,
#endif
#if BLOAD_AND_BSAVE || BLOAD || BLOAD_ONLY
                                    BloadDefclassModuleReference,
#else
                                    NULL,
#endif
#if CONSTRUCT_COMPILER && (! RUN_TIME)
                                    DefclassCModuleReference,
#else
                                    NULL,
#endif
                                    FindDefclass);

   DefclassConstruct =  AddConstruct("defclass","defclasses",
#if (! BLOAD_ONLY) && (! RUN_TIME)
                                     ParseDefclass,
#else
                                     NULL,
#endif
                                     FindDefclass,
                                     GetConstructNamePointer,GetConstructPPForm,
                                     GetConstructModuleItem,GetNextDefclass,
                                     SetNextConstruct,IsDefclassDeletable,
                                     Undefclass,
#if (! RUN_TIME)
                                     RemoveDefclass
#else
                                     NULL
#endif
                                     );

   AddClearReadyFunction("defclass",InstancesPurge,0);

#if ! RUN_TIME
   AddClearFunction("defclass",CreateSystemClasses,0);
   InitializeClasses();

#if ! BLOAD_ONLY
#if DEFMODULE_CONSTRUCT
   AddPortConstructItem("defclass",SYMBOL);
   AddAfterModuleDefinedFunction("defclass",UpdateDefclassesScope,0);
#endif
   DefineFunction2("undefclass",'v',PTIF UndefclassCommand,"UndefclassCommand","11w");
   AddSaveFunction("defclass",SaveDefclasses,10);
#endif

#if DEBUGGING_FUNCTIONS
   DefineFunction2("list-defclasses",'v',PTIF ListDefclassesCommand,"ListDefclassesCommand","01");
   DefineFunction2("ppdefclass",'v',PTIF PPDefclassCommand,"PPDefclassCommand","11w");
   DefineFunction2("describe-class",'v',PTIF DescribeClassCommand,"DescribeClassCommand","11w");
   DefineFunction2("browse-classes",'v',PTIF BrowseClassesCommand,"BrowseClassesCommand","01w");
#endif

   DefineFunction2("get-defclass-list",'m',PTIF GetDefclassListFunction,
                   "GetDefclassListFunction","01");
   DefineFunction2("superclassp",'b',PTIF SuperclassPCommand,"SuperclassPCommand","22w");
   DefineFunction2("subclassp",'b',PTIF SubclassPCommand,"SubclassPCommand","22w");
   DefineFunction2("class-existp",'b',PTIF ClassExistPCommand,"ClassExistPCommand","11w");
   DefineFunction2("message-handler-existp",'b',
                   PTIF MessageHandlerExistPCommand,"MessageHandlerExistPCommand","23w");
   DefineFunction2("class-abstractp",'b',PTIF ClassAbstractPCommand,"ClassAbstractPCommand","11w");
#if INSTANCE_PATTERN_MATCHING
   DefineFunction2("class-reactivep",'b',PTIF ClassReactivePCommand,"ClassReactivePCommand","11w");
#endif
   DefineFunction2("class-slots",'m',PTIF ClassSlotsCommand,"ClassSlotsCommand","12w");
   DefineFunction2("class-superclasses",'m',
                   PTIF ClassSuperclassesCommand,"ClassSuperclassesCommand","12w");
   DefineFunction2("class-subclasses",'m',
                   PTIF ClassSubclassesCommand,"ClassSubclassesCommand","12w");
   DefineFunction2("get-defmessage-handler-list",'m',
                   PTIF GetDefmessageHandlersListCmd,"GetDefmessageHandlersListCmd","02w");
   DefineFunction2("slot-existp",'b',PTIF SlotExistPCommand,"SlotExistPCommand","23w");
   DefineFunction2("slot-facets",'m',PTIF SlotFacetsCommand,"SlotFacetsCommand","22w");
   DefineFunction2("slot-sources",'m',PTIF SlotSourcesCommand,"SlotSourcesCommand","22w");
   DefineFunction2("slot-types",'m',PTIF SlotTypesCommand,"SlotTypesCommand","22w");
   DefineFunction2("slot-allowed-values",'m',PTIF SlotAllowedValuesCommand,"SlotAllowedValuesCommand","22w");
   DefineFunction2("slot-range",'m',PTIF SlotRangeCommand,"SlotRangeCommand","22w");
   DefineFunction2("slot-cardinality",'m',PTIF SlotCardinalityCommand,"SlotCardinalityCommand","22w");
   DefineFunction2("slot-writablep",'b',PTIF SlotWritablePCommand,"SlotWritablePCommand","22w");
   DefineFunction2("slot-initablep",'b',PTIF SlotInitablePCommand,"SlotInitablePCommand","22w");
   DefineFunction2("slot-publicp",'b',PTIF SlotPublicPCommand,"SlotPublicPCommand","22w");
   DefineFunction2("slot-direct-accessp",'b',PTIF SlotDirectAccessPCommand,
                   "SlotDirectAccessPCommand","22w");
   DefineFunction2("slot-default-value",'u',PTIF SlotDefaultValueCommand,
                   "SlotDefaultValueCommand","22w");
   DefineFunction2("defclass-module",'w',PTIF GetDefclassModuleCommand,
                   "GetDefclassModuleCommand","11w");
#endif

#if DEBUGGING_FUNCTIONS
   AddWatchItem("instances",0,&WatchInstances,75,DefclassWatchAccess,DefclassWatchPrint);
   AddWatchItem("slots",1,&WatchSlots,74,DefclassWatchAccess,DefclassWatchPrint);
#endif

  }

#if (! RUN_TIME)

/*********************************************************
  NAME         : AddSystemClass
  DESCRIPTION  : Performs all necessary allocations
                   for adding a system class
  INPUTS       : 1) The name-string of the system class
                 2) The address of the parent class
                    (NULL if none)
  RETURNS      : The address of the new system class
  SIDE EFFECTS : Allocations performed
  NOTES        : Assumes system-class name is unique
                 Also assumes SINGLE INHERITANCE for
                   system classes to simplify precedence
                   list determination
                 Adds classes to has table but NOT to
                  class list (this is responsibility
                  of caller)
 *********************************************************/
static DEFCLASS *AddSystemClass(
  char *name,
  DEFCLASS *parent)
  {
   DEFCLASS *sys;
   register unsigned i;
   char defaultScopeMap[1];

   sys = NewClass((SYMBOL_HN *) AddSymbol(name));
   sys->abstract = 1;
#if INSTANCE_PATTERN_MATCHING
   sys->reactive = 0;
#endif
   IncrementSymbolCount(sys->header.name);
   sys->installed = 1;
   sys->system = 1;
   sys->hashTableIndex = HashClass(sys->header.name);

   AddClassLink(&sys->allSuperclasses,sys,-1);
   if (parent != NULL)
     {
      AddClassLink(&sys->directSuperclasses,parent,-1);
      AddClassLink(&parent->directSubclasses,sys,-1);
      AddClassLink(&sys->allSuperclasses,parent,-1);
      for (i = 1 ; i < parent->allSuperclasses.classCount ; i++)
        AddClassLink(&sys->allSuperclasses,parent->allSuperclasses.classArray[i],-1);
     }
   sys->nxtHash = ClassTable[sys->hashTableIndex];
   ClassTable[sys->hashTableIndex] = sys;

   /* =========================================
      Add default scope maps for a system class
      There is only one module (MAIN) so far -
      which has an id of 0
      ========================================= */
   ClearBitString((void *) defaultScopeMap,(int) sizeof(char));
   SetBitMap(defaultScopeMap,0);
#if DEFMODULE_CONSTRUCT
   sys->scopeMap = (BITMAP_HN *) AddBitMap((void *) defaultScopeMap,(int) sizeof(char));
   IncrementBitMapCount(sys->scopeMap);
#endif
   return(sys);
  }

/*****************************************************
  NAME         : AllocateModule
  DESCRIPTION  : Creates and initializes a
                 list of deffunctions for a new module
  INPUTS       : None
  RETURNS      : The new deffunction module
  SIDE EFFECTS : Deffunction module created
  NOTES        : None
 *****************************************************/
static void *AllocateModule()
  {
   return((void *) get_struct(defclassModule));
  }

/***************************************************
  NAME         : ReturnModule
  DESCRIPTION  : Removes a deffunction module and
                 all associated deffunctions
  INPUTS       : The deffunction module
  RETURNS      : Nothing useful
  SIDE EFFECTS : Module and deffunctions deleted
  NOTES        : None
 ***************************************************/
static void ReturnModule(
  void *theItem)
  {
   FreeConstructHeaderModule((struct defmoduleItemHeader *) theItem,DefclassConstruct);
   DeleteSlotName(FindIDSlotNameHash(ISA_ID));
   DeleteSlotName(FindIDSlotNameHash(NAME_ID));
   rtn_struct(defclassModule,theItem);
  }

#endif

#if (! BLOAD_ONLY) && (! RUN_TIME) && DEFMODULE_CONSTRUCT

/***************************************************
  NAME         : UpdateDefclassesScope
  DESCRIPTION  : This function updates the scope
                 bitmaps for existing classes when
                 a new module is defined
  INPUTS       : None
  RETURNS      : Nothing
  SIDE EFFECTS : Class scope bitmaps are updated
  NOTES        : None
 ***************************************************/
static void UpdateDefclassesScope()
  {
   register unsigned i;
   DEFCLASS *theDefclass;
   int newModuleID,count;
   char *newScopeMap;
   int newScopeMapSize;
   char *className;
   struct defmodule *matchModule;

   newModuleID = (int) ((struct defmodule *) GetCurrentModule())->bsaveID;
   newScopeMapSize = (int) (sizeof(char) * ((GetNumberOfDefmodules() / BITS_PER_BYTE) + 1));
   newScopeMap = (char *) gm2(newScopeMapSize);
   for (i = 0 ; i < CLASS_TABLE_HASH_SIZE ; i++)
     for (theDefclass = ClassTable[i] ;
          theDefclass != NULL ;
          theDefclass = theDefclass->nxtHash)
       {
        matchModule = theDefclass->header.whichModule->theModule;
        className = ValueToString(theDefclass->header.name);
        ClearBitString((void *) newScopeMap,newScopeMapSize);
        GenCopyMemory(char,theDefclass->scopeMap->size,
                   newScopeMap,ValueToBitMap(theDefclass->scopeMap));
        DecrementBitMapCount(theDefclass->scopeMap);
        if (theDefclass->system)
          SetBitMap(newScopeMap,newModuleID);
        else if (FindImportedConstruct("defclass",matchModule,
                                       className,&count,TRUE,NULL) != NULL)
          SetBitMap(newScopeMap,newModuleID);
        theDefclass->scopeMap = (BITMAP_HN *) AddBitMap((void *) newScopeMap,newScopeMapSize);
        IncrementBitMapCount(theDefclass->scopeMap);
       }
   rm((void *) newScopeMap,newScopeMapSize);
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
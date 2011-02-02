   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.24  05/17/06          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Binary Load/Save Functions for Classes and their */
/*             message-handlers                              */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*      6.24: Removed IMPERATIVE_MESSAGE_HANDLERS and        */
/*            AUXILIARY_MESSAGE_HANDLERS compilation flags.  */
/*                                                           */
/*************************************************************/

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */

#include <stdlib.h>

#include "setup.h"

#if OBJECT_SYSTEM && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)

#include "bload.h"
#include "bsave.h"
#include "classcom.h"
#include "classfun.h"
#include "classini.h"
#include "cstrcbin.h"
#include "cstrnbin.h"
#include "envrnmnt.h"
#include "insfun.h"
#include "memalloc.h"
#include "modulbin.h"
#include "msgcom.h"
#include "msgfun.h"
#include "prntutil.h"
#include "router.h"

#define _OBJBIN_SOURCE_
#include "objbin.h"

/* =========================================
   *****************************************
               MACROS AND TYPES
   =========================================
   ***************************************** */

#define SlotIndex(p)             (((p) != NULL) ? (p)->bsaveIndex : -1L)
#define SlotNameIndex(p)         (p)->bsaveIndex

#define LinkPointer(i)           (((i) == -1L) ? NULL : (DEFCLASS **) &ObjectBinaryData(theEnv,execStatus)->LinkArray[i])
#define SlotPointer(i)           (((i) == -1L) ? NULL : (SLOT_DESC *) &ObjectBinaryData(theEnv,execStatus)->SlotArray[i])
#define TemplateSlotPointer(i)   (((i) == -1L) ? NULL : (SLOT_DESC **) &ObjectBinaryData(theEnv,execStatus)->TmpslotArray[i])
#define OrderedSlotPointer(i)    (((i) == -1L) ? NULL : (unsigned *) &ObjectBinaryData(theEnv,execStatus)->MapslotArray[i])
#define SlotNamePointer(i)       ((SLOT_NAME *) &ObjectBinaryData(theEnv,execStatus)->SlotNameArray[i])
#define HandlerPointer(i)        (((i) == -1L) ? NULL : (HANDLER *) &ObjectBinaryData(theEnv,execStatus)->HandlerArray[i])
#define OrderedHandlerPointer(i) (((i) == -1L) ? NULL : (unsigned *) &ObjectBinaryData(theEnv,execStatus)->MaphandlerArray[i])

typedef struct bsaveDefclassModule
  {
   struct bsaveDefmoduleItemHeader header;
  } BSAVE_DEFCLASS_MODULE;

typedef struct bsavePackedClassLinks
  {
   long classCount;
   long classArray;
  } BSAVE_PACKED_CLASS_LINKS;

typedef struct bsaveDefclass
  {
   struct bsaveConstructHeader header;
   unsigned abstract : 1;
   unsigned reactive : 1;
   unsigned system   : 1;
   unsigned id;
   BSAVE_PACKED_CLASS_LINKS directSuperclasses,
                            directSubclasses,
                            allSuperclasses;
   short slotCount,localInstanceSlotCount,
            instanceSlotCount,maxSlotNameID;
   short handlerCount;
   long slots,
        instanceTemplate,
        slotNameMap,
        handlers,
        scopeMap;
  } BSAVE_DEFCLASS;

typedef struct bsaveSlotName
  {
   short id;
   unsigned hashTableIndex;
   long name,
        putHandlerName;
  } BSAVE_SLOT_NAME;

typedef struct bsaveSlotDescriptor
  {
   unsigned shared              : 1;
   unsigned multiple            : 1;
   unsigned composite           : 1;
   unsigned noInherit           : 1;
   unsigned noWrite             : 1;
   unsigned initializeOnly      : 1;
   unsigned dynamicDefault      : 1;
   unsigned noDefault           : 1;
   unsigned reactive            : 1;
   unsigned publicVisibility    : 1;
   unsigned createReadAccessor  : 1;
   unsigned createWriteAccessor : 1;
   long cls,
        slotName,
        defaultValue,
        constraint,
        overrideMessage;
  } BSAVE_SLOT_DESC;

typedef struct bsaveMessageHandler
  {
   unsigned system : 1;
   unsigned type   : 2;
   short minParams,
       maxParams,
       localVarCount;
   long name,
        cls,
        actions;
  } BSAVE_HANDLER;

typedef struct handlerBsaveInfo
  {
   HANDLER *handlers;
   unsigned *handlerOrderMap;
   unsigned handlerCount;
  } HANDLER_BSAVE_INFO;

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

#if BLOAD_AND_BSAVE

static void BsaveObjectsFind(void *,EXEC_STATUS);
static void MarkDefclassItems(void *,EXEC_STATUS,struct constructHeader *,void *);
static void BsaveObjectsExpressions(void *,EXEC_STATUS,FILE *);
static void BsaveDefaultSlotExpressions(void *,EXEC_STATUS,struct constructHeader *,void *);
static void BsaveHandlerActionExpressions(void *,EXEC_STATUS,struct constructHeader *,void *);
static void BsaveStorageObjects(void *,EXEC_STATUS,FILE *);
static void BsaveObjects(void *,EXEC_STATUS,FILE *);
static void BsaveDefclass(void *,EXEC_STATUS,struct constructHeader *,void *);
static void BsaveClassLinks(void *,EXEC_STATUS,struct constructHeader *,void *);
static void BsaveSlots(void *,EXEC_STATUS,struct constructHeader *,void *);
static void BsaveTemplateSlots(void *,EXEC_STATUS,struct constructHeader *,void *);
static void BsaveSlotMap(void *,EXEC_STATUS,struct constructHeader *,void *);
static void BsaveHandlers(void *,EXEC_STATUS,struct constructHeader *,void *);
static void BsaveHandlerMap(void *,EXEC_STATUS,struct constructHeader *,void *);

#endif

static void BloadStorageObjects(void *,EXEC_STATUS);
static void BloadObjects(void *,EXEC_STATUS);
static void UpdatePrimitiveClassesMap(void *,EXEC_STATUS);
static void UpdateDefclassModule(void *,EXEC_STATUS,void *,long);
static void UpdateDefclass(void *,EXEC_STATUS,void *,long);
static void UpdateLink(void *,EXEC_STATUS,void *,long);
static void UpdateSlot(void *,EXEC_STATUS,void *,long);
static void UpdateSlotName(void *,EXEC_STATUS,void *,long);
static void UpdateTemplateSlot(void *,EXEC_STATUS,void *,long);
static void UpdateHandler(void *,EXEC_STATUS,void *,long);
static void ClearBloadObjects(void *,EXEC_STATUS);
static void DeallocateObjectBinaryData(void *,EXEC_STATUS);

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***********************************************************
  NAME         : SetupObjectsBload
  DESCRIPTION  : Initializes data structures and
                   routines for binary loads of
                   generic function constructs
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Routines defined and structures initialized
  NOTES        : None
 ***********************************************************/
globle void SetupObjectsBload(
  void *theEnv,
  EXEC_STATUS)
  {
   AllocateEnvironmentData(theEnv,execStatus,OBJECTBIN_DATA,sizeof(struct objectBinaryData),DeallocateObjectBinaryData);
   
   AddAbortBloadFunction(theEnv,execStatus,"defclass",CreateSystemClasses,0);

#if BLOAD_AND_BSAVE
   AddBinaryItem(theEnv,execStatus,"defclass",0,BsaveObjectsFind,BsaveObjectsExpressions,
                             BsaveStorageObjects,BsaveObjects,
                             BloadStorageObjects,BloadObjects,
                             ClearBloadObjects);
#endif
#if BLOAD || BLOAD_ONLY
   AddBinaryItem(theEnv,execStatus,"defclass",0,NULL,NULL,NULL,NULL,
                             BloadStorageObjects,BloadObjects,
                             ClearBloadObjects);
#endif

  }
  
/*******************************************************/
/* DeallocateObjectBinaryData: Deallocates environment */
/*    data for object binary functionality.            */
/*******************************************************/
static void DeallocateObjectBinaryData(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;
   long i;

#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME)
   
   space = (sizeof(DEFCLASS_MODULE) * ObjectBinaryData(theEnv,execStatus)->ModuleCount);
   if (space != 0) genfree(theEnv,execStatus,(void *) ObjectBinaryData(theEnv,execStatus)->ModuleArray,space); 

   if (ObjectBinaryData(theEnv,execStatus)->ClassCount != 0)
     { 
      if (DefclassData(theEnv,execStatus)->ClassIDMap != NULL)
        { rm(theEnv,execStatus,(void *) DefclassData(theEnv,execStatus)->ClassIDMap,(sizeof(DEFCLASS *) * DefclassData(theEnv,execStatus)->AvailClassID)); }

      for (i = 0L ; i < ObjectBinaryData(theEnv,execStatus)->SlotCount ; i++)
        {
         if ((ObjectBinaryData(theEnv,execStatus)->SlotArray[i].defaultValue != NULL) && (ObjectBinaryData(theEnv,execStatus)->SlotArray[i].dynamicDefault == 0))
           { rtn_struct(theEnv,execStatus,dataObject,ObjectBinaryData(theEnv,execStatus)->SlotArray[i].defaultValue); }
        }

      space = (sizeof(DEFCLASS) * ObjectBinaryData(theEnv,execStatus)->ClassCount);
      if (space != 0L)
        { genfree(theEnv,execStatus,(void *) ObjectBinaryData(theEnv,execStatus)->DefclassArray,space); }

      space = (sizeof(DEFCLASS *) * ObjectBinaryData(theEnv,execStatus)->LinkCount);
      if (space != 0L)
        { genfree(theEnv,execStatus,(void *) ObjectBinaryData(theEnv,execStatus)->LinkArray,space); }

      space = (sizeof(SLOT_DESC) * ObjectBinaryData(theEnv,execStatus)->SlotCount);
      if (space != 0L)
        { genfree(theEnv,execStatus,(void *) ObjectBinaryData(theEnv,execStatus)->SlotArray,space); }

      space = (sizeof(SLOT_NAME) * ObjectBinaryData(theEnv,execStatus)->SlotNameCount);
      if (space != 0L)
        { genfree(theEnv,execStatus,(void *) ObjectBinaryData(theEnv,execStatus)->SlotNameArray,space); }

      space = (sizeof(SLOT_DESC *) * ObjectBinaryData(theEnv,execStatus)->TemplateSlotCount);
      if (space != 0L)
        { genfree(theEnv,execStatus,(void *) ObjectBinaryData(theEnv,execStatus)->TmpslotArray,space); }

      space = (sizeof(unsigned) * ObjectBinaryData(theEnv,execStatus)->SlotNameMapCount);
      if (space != 0L)
        { genfree(theEnv,execStatus,(void *) ObjectBinaryData(theEnv,execStatus)->MapslotArray,space); }
     }

   if (ObjectBinaryData(theEnv,execStatus)->HandlerCount != 0L)
     {
      space = (sizeof(HANDLER) * ObjectBinaryData(theEnv,execStatus)->HandlerCount);
      if (space != 0L)
        {
         genfree(theEnv,execStatus,(void *) ObjectBinaryData(theEnv,execStatus)->HandlerArray,space);
         space = (sizeof(unsigned) * ObjectBinaryData(theEnv,execStatus)->HandlerCount);
         genfree(theEnv,execStatus,(void *) ObjectBinaryData(theEnv,execStatus)->MaphandlerArray,space);
        }
     }
#endif
  }

/***************************************************
  NAME         : BloadDefclassModuleReference
  DESCRIPTION  : Returns a pointer to the
                 appropriate defclass module
  INPUTS       : The index of the module
  RETURNS      : A pointer to the module
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle void *BloadDefclassModuleReference(
  void *theEnv,
  EXEC_STATUS,
  int theIndex)
  {
   return ((void *) &ObjectBinaryData(theEnv,execStatus)->ModuleArray[theIndex]);
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

#if BLOAD_AND_BSAVE

/***************************************************************************
  NAME         : BsaveObjectsFind
  DESCRIPTION  : For all classes and their message-handlers, this routine
                   marks all the needed symbols and system functions.
                 Also, it also counts the number of expression structures
                   needed.
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : ExpressionCount (a global from BSAVE.C) is incremented
                   for every expression needed
                 Symbols are marked in their structures
  NOTES        : Also sets bsaveIndex for each class (assumes classes
                   will be bsaved in order of binary list)
 ***************************************************************************/
static void BsaveObjectsFind(
  void *theEnv,
  EXEC_STATUS)
  {
   register unsigned i;
   SLOT_NAME *snp;

   /* ========================================================
      The counts need to be saved in case a bload is in effect
      ======================================================== */
      SaveBloadCount(theEnv,execStatus,ObjectBinaryData(theEnv,execStatus)->ModuleCount);
      SaveBloadCount(theEnv,execStatus,ObjectBinaryData(theEnv,execStatus)->ClassCount);
      SaveBloadCount(theEnv,execStatus,ObjectBinaryData(theEnv,execStatus)->LinkCount);
      SaveBloadCount(theEnv,execStatus,ObjectBinaryData(theEnv,execStatus)->SlotNameCount);
      SaveBloadCount(theEnv,execStatus,ObjectBinaryData(theEnv,execStatus)->SlotCount);
      SaveBloadCount(theEnv,execStatus,ObjectBinaryData(theEnv,execStatus)->TemplateSlotCount);
      SaveBloadCount(theEnv,execStatus,ObjectBinaryData(theEnv,execStatus)->SlotNameMapCount);
      SaveBloadCount(theEnv,execStatus,ObjectBinaryData(theEnv,execStatus)->HandlerCount);

   ObjectBinaryData(theEnv,execStatus)->ModuleCount= 0L;
   ObjectBinaryData(theEnv,execStatus)->ClassCount = 0L;
   ObjectBinaryData(theEnv,execStatus)->SlotCount = 0L;
   ObjectBinaryData(theEnv,execStatus)->SlotNameCount = 0L;
   ObjectBinaryData(theEnv,execStatus)->LinkCount = 0L;
   ObjectBinaryData(theEnv,execStatus)->TemplateSlotCount = 0L;
   ObjectBinaryData(theEnv,execStatus)->SlotNameMapCount = 0L;
   ObjectBinaryData(theEnv,execStatus)->HandlerCount = 0L;

   /* ==============================================
      Mark items needed by defclasses in all modules
      ============================================== */
   ObjectBinaryData(theEnv,execStatus)->ModuleCount = 
      DoForAllConstructs(theEnv,execStatus,MarkDefclassItems,DefclassData(theEnv,execStatus)->DefclassModuleIndex,
                                    FALSE,NULL);

   /* =============================================
      Mark items needed by canonicalized slot names
      ============================================= */
   for (i = 0 ; i < SLOT_NAME_TABLE_HASH_SIZE ; i++)
     for (snp = DefclassData(theEnv,execStatus)->SlotNameTable[i] ; snp != NULL ; snp = snp->nxt)
       {
        if ((snp->id != ISA_ID) && (snp->id != NAME_ID))
          {
           snp->bsaveIndex = ObjectBinaryData(theEnv,execStatus)->SlotNameCount++;
           snp->name->neededSymbol = TRUE;
           snp->putHandlerName->neededSymbol = TRUE;
          }
       }
  }

/***************************************************
  NAME         : MarkDefclassItems
  DESCRIPTION  : Marks needed items for a defclass
  INPUTS       : 1) The defclass
                 2) User buffer (ignored)
  RETURNS      : Nothing useful
  SIDE EFFECTS : Bsave indices set and needed
                 ephemerals marked
  NOTES        : None
 ***************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void MarkDefclassItems(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theDefclass,
  void *buf)
  {
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(buf)
#endif
   DEFCLASS *cls = (DEFCLASS *) theDefclass;
   long i;
   EXPRESSION *tmpexp;

   MarkConstructHeaderNeededItems(&cls->header,ObjectBinaryData(theEnv,execStatus)->ClassCount++);
   ObjectBinaryData(theEnv,execStatus)->LinkCount += cls->directSuperclasses.classCount +
                cls->directSubclasses.classCount +
                cls->allSuperclasses.classCount;

#if DEFMODULE_CONSTRUCT
   cls->scopeMap->neededBitMap = TRUE;
#endif

   /* ===================================================
      Mark items needed by slot default value expressions
      =================================================== */
   for (i = 0 ; i < cls->slotCount ; i++)
     {
      cls->slots[i].bsaveIndex = ObjectBinaryData(theEnv,execStatus)->SlotCount++;
      cls->slots[i].overrideMessage->neededSymbol = TRUE;
      if (cls->slots[i].defaultValue != NULL)
        {
         if (cls->slots[i].dynamicDefault)
           {
            ExpressionData(theEnv,execStatus)->ExpressionCount +=
              ExpressionSize((EXPRESSION *) cls->slots[i].defaultValue);
            MarkNeededItems(theEnv,execStatus,(EXPRESSION *) cls->slots[i].defaultValue);
           }
         else
           {
            /* =================================================
               Static default values are stotred as data objects
               and must be converted into expressions
               ================================================= */
            tmpexp =
              ConvertValueToExpression(theEnv,execStatus,(DATA_OBJECT *) cls->slots[i].defaultValue);
            ExpressionData(theEnv,execStatus)->ExpressionCount += ExpressionSize(tmpexp);
            MarkNeededItems(theEnv,execStatus,tmpexp);
            ReturnExpression(theEnv,execStatus,tmpexp);
           }
        }
     }

   /* ========================================
      Count canonical slots needed by defclass
      ======================================== */
   ObjectBinaryData(theEnv,execStatus)->TemplateSlotCount += (long) cls->instanceSlotCount;
   if (cls->instanceSlotCount != 0)
     ObjectBinaryData(theEnv,execStatus)->SlotNameMapCount += (long) cls->maxSlotNameID + 1;

   /* ===============================================
      Mark items needed by defmessage-handler actions
      =============================================== */
   for (i = 0 ; i < cls->handlerCount ; i++)
     {
      cls->handlers[i].name->neededSymbol = TRUE;
      ExpressionData(theEnv,execStatus)->ExpressionCount += ExpressionSize(cls->handlers[i].actions);
      MarkNeededItems(theEnv,execStatus,cls->handlers[i].actions);
     }
   ObjectBinaryData(theEnv,execStatus)->HandlerCount += (long) cls->handlerCount;
  }

/***************************************************
  NAME         : BsaveObjectsExpressions
  DESCRIPTION  : Writes out all expressions needed
                   by classes and handlers
  INPUTS       : The file pointer of the binary file
  RETURNS      : Nothing useful
  SIDE EFFECTS : File updated
  NOTES        : None
 ***************************************************/
static void BsaveObjectsExpressions(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   if ((ObjectBinaryData(theEnv,execStatus)->ClassCount == 0L) && (ObjectBinaryData(theEnv,execStatus)->HandlerCount == 0L))
     return;

   /* ================================================
      Save the defclass slot default value expressions
      ================================================ */
   DoForAllConstructs(theEnv,execStatus,BsaveDefaultSlotExpressions,DefclassData(theEnv,execStatus)->DefclassModuleIndex,
                      FALSE,(void *) fp);

   /* ==============================================
      Save the defmessage-handler action expressions
      ============================================== */
   DoForAllConstructs(theEnv,execStatus,BsaveHandlerActionExpressions,DefclassData(theEnv,execStatus)->DefclassModuleIndex,
                      FALSE,(void *) fp);
  }

/***************************************************
  NAME         : BsaveDefaultSlotExpressions
  DESCRIPTION  : Writes expressions for default
                  slot values to binary file
  INPUTS       : 1) The defclass
                 2) The binary file pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot value expressions written
  NOTES        : None
 ***************************************************/
static void BsaveDefaultSlotExpressions(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theDefclass,
  void *buf)
  {
   DEFCLASS *cls = (DEFCLASS *) theDefclass;
   long i;
   EXPRESSION *tmpexp;

   for (i = 0 ; i < cls->slotCount ; i++)
     {
      if (cls->slots[i].defaultValue != NULL)
        {
         if (cls->slots[i].dynamicDefault)
           BsaveExpression(theEnv,execStatus,(EXPRESSION *) cls->slots[i].defaultValue,(FILE *) buf);
         else
           {
            /* =================================================
               Static default values are stotred as data objects
               and must be converted into expressions
               ================================================= */
            tmpexp =
              ConvertValueToExpression(theEnv,execStatus,(DATA_OBJECT *) cls->slots[i].defaultValue);
            BsaveExpression(theEnv,execStatus,tmpexp,(FILE *) buf);
            ReturnExpression(theEnv,execStatus,tmpexp);
           }
        }
     }
  }

/***************************************************
  NAME         : BsaveHandlerActionExpressions
  DESCRIPTION  : Writes expressions for handler
                  actions to binary file
  INPUTS       : 1) The defclass
                 2) The binary file pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Handler actions expressions written
  NOTES        : None
 ***************************************************/
static void BsaveHandlerActionExpressions(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theDefclass,
  void *buf)
  {
   DEFCLASS *cls = (DEFCLASS *) theDefclass;
   long i;

   for (i = 0 ; i < cls->handlerCount ; i++)
     BsaveExpression(theEnv,execStatus,cls->handlers[i].actions,(FILE *) buf);
  }

/*************************************************************************************
  NAME         : BsaveStorageObjects
  DESCRIPTION  : Writes out number of each type of structure required for COOL
                 Space required for counts (unsigned long)
                 Number of class modules (long)
                 Number of classes (long)
                 Number of links to classes (long)
                 Number of slots (long)
                 Number of instance template slots (long)
                 Number of handlers (long)
                 Number of definstances (long)
  INPUTS       : File pointer of binary file
  RETURNS      : Nothing useful
  SIDE EFFECTS : Binary file adjusted
  NOTES        : None
 *************************************************************************************/
static void BsaveStorageObjects(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   size_t space;
   long maxClassID;

   if ((ObjectBinaryData(theEnv,execStatus)->ClassCount == 0L) && (ObjectBinaryData(theEnv,execStatus)->HandlerCount == 0L))
     {
      space = 0L;
      GenWrite((void *) &space,sizeof(size_t),fp);
      return;
     }
   space = sizeof(long) * 9;
   GenWrite((void *) &space,sizeof(long),fp);
   GenWrite((void *) &ObjectBinaryData(theEnv,execStatus)->ModuleCount,sizeof(long),fp);
   GenWrite((void *) &ObjectBinaryData(theEnv,execStatus)->ClassCount,sizeof(long),fp);
   GenWrite((void *) &ObjectBinaryData(theEnv,execStatus)->LinkCount,sizeof(long),fp);
   GenWrite((void *) &ObjectBinaryData(theEnv,execStatus)->SlotNameCount,sizeof(long),fp);
   GenWrite((void *) &ObjectBinaryData(theEnv,execStatus)->SlotCount,sizeof(long),fp);
   GenWrite((void *) &ObjectBinaryData(theEnv,execStatus)->TemplateSlotCount,sizeof(long),fp);
   GenWrite((void *) &ObjectBinaryData(theEnv,execStatus)->SlotNameMapCount,sizeof(long),fp);
   GenWrite((void *) &ObjectBinaryData(theEnv,execStatus)->HandlerCount,sizeof(long),fp);
   maxClassID = DefclassData(theEnv,execStatus)->MaxClassID;
   GenWrite((void *) &maxClassID,sizeof(long),fp);
  }

/*************************************************************************************
  NAME         : BsaveObjects
  DESCRIPTION  : Writes out classes and message-handlers in binary format
                 Space required (unsigned long)
                 Followed by the data structures in order
  INPUTS       : File pointer of binary file
  RETURNS      : Nothing useful
  SIDE EFFECTS : Binary file adjusted
  NOTES        : None
 *************************************************************************************/
static void BsaveObjects(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   size_t space;
   struct defmodule *theModule;
   DEFCLASS_MODULE *theModuleItem;
   BSAVE_DEFCLASS_MODULE dummy_mitem;
   BSAVE_SLOT_NAME dummy_slot_name;
   SLOT_NAME *snp;
   register unsigned i;

   if ((ObjectBinaryData(theEnv,execStatus)->ClassCount == 0L) && (ObjectBinaryData(theEnv,execStatus)->HandlerCount == 0L))
     {
      space = 0L;
      GenWrite((void *) &space,sizeof(size_t),fp);
      return;
     }
   space = (ObjectBinaryData(theEnv,execStatus)->ModuleCount * sizeof(BSAVE_DEFCLASS_MODULE)) +
           (ObjectBinaryData(theEnv,execStatus)->ClassCount * sizeof(BSAVE_DEFCLASS)) +
           (ObjectBinaryData(theEnv,execStatus)->LinkCount * sizeof(long)) +
           (ObjectBinaryData(theEnv,execStatus)->SlotCount * sizeof(BSAVE_SLOT_DESC)) +
           (ObjectBinaryData(theEnv,execStatus)->SlotNameCount * sizeof(BSAVE_SLOT_NAME)) +
           (ObjectBinaryData(theEnv,execStatus)->TemplateSlotCount * sizeof(long)) +
           (ObjectBinaryData(theEnv,execStatus)->SlotNameMapCount * sizeof(unsigned)) +
           (ObjectBinaryData(theEnv,execStatus)->HandlerCount * sizeof(BSAVE_HANDLER)) +
           (ObjectBinaryData(theEnv,execStatus)->HandlerCount * sizeof(unsigned));
   GenWrite((void *) &space,sizeof(size_t),fp);

   ObjectBinaryData(theEnv,execStatus)->ClassCount = 0L;
   ObjectBinaryData(theEnv,execStatus)->LinkCount = 0L;
   ObjectBinaryData(theEnv,execStatus)->SlotCount = 0L;
   ObjectBinaryData(theEnv,execStatus)->SlotNameCount = 0L;
   ObjectBinaryData(theEnv,execStatus)->TemplateSlotCount = 0L;
   ObjectBinaryData(theEnv,execStatus)->SlotNameMapCount = 0L;
   ObjectBinaryData(theEnv,execStatus)->HandlerCount = 0L;

   /* =================================
      Write out each defclass module
      ================================= */
   theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,NULL);
   while (theModule != NULL)
     {
      theModuleItem = (DEFCLASS_MODULE *)
                      GetModuleItem(theEnv,execStatus,theModule,FindModuleItem(theEnv,execStatus,"defclass")->moduleIndex);
      AssignBsaveDefmdlItemHdrVals(&dummy_mitem.header,&theModuleItem->header);
      GenWrite((void *) &dummy_mitem,sizeof(BSAVE_DEFCLASS_MODULE),fp);
      theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,(void *) theModule);
     }

   /* =====================
      Write out the classes
      ===================== */
   DoForAllConstructs(theEnv,execStatus,BsaveDefclass,DefclassData(theEnv,execStatus)->DefclassModuleIndex,FALSE,(void *) fp);

   /* =========================
      Write out the class links
      ========================= */
   ObjectBinaryData(theEnv,execStatus)->LinkCount = 0L;
   DoForAllConstructs(theEnv,execStatus,BsaveClassLinks,DefclassData(theEnv,execStatus)->DefclassModuleIndex,FALSE,(void *) fp);

   /* ===============================
      Write out the slot name entries
      =============================== */
   for (i = 0 ; i < SLOT_NAME_TABLE_HASH_SIZE ; i++)
     for (snp = DefclassData(theEnv,execStatus)->SlotNameTable[i] ; snp != NULL ; snp = snp->nxt)
     {
      if ((snp->id != ISA_ID) && (snp->id != NAME_ID))
        {
         dummy_slot_name.id = snp->id;
         dummy_slot_name.hashTableIndex = snp->hashTableIndex;
         dummy_slot_name.name = (long) snp->name->bucket;
         dummy_slot_name.putHandlerName = (long) snp->putHandlerName->bucket;
         GenWrite((void *) &dummy_slot_name,sizeof(BSAVE_SLOT_NAME),fp);
        }
     }

   /* ===================
      Write out the slots
      =================== */
   DoForAllConstructs(theEnv,execStatus,BsaveSlots,DefclassData(theEnv,execStatus)->DefclassModuleIndex,FALSE,(void *) fp);

   /* =====================================
      Write out the template instance slots
      ===================================== */
   DoForAllConstructs(theEnv,execStatus,BsaveTemplateSlots,DefclassData(theEnv,execStatus)->DefclassModuleIndex,FALSE,(void *) fp);

   /* =============================================
      Write out the ordered instance slot name maps
      ============================================= */
   DoForAllConstructs(theEnv,execStatus,BsaveSlotMap,DefclassData(theEnv,execStatus)->DefclassModuleIndex,FALSE,(void *) fp);

   /* ==============================
      Write out the message-handlers
      ============================== */
   DoForAllConstructs(theEnv,execStatus,BsaveHandlers,DefclassData(theEnv,execStatus)->DefclassModuleIndex,FALSE,(void *) fp);

   /* ==========================================
      Write out the ordered message-handler maps
      ========================================== */
   DoForAllConstructs(theEnv,execStatus,BsaveHandlerMap,DefclassData(theEnv,execStatus)->DefclassModuleIndex,FALSE,(void *) fp);

      RestoreBloadCount(theEnv,execStatus,&ObjectBinaryData(theEnv,execStatus)->ModuleCount);
      RestoreBloadCount(theEnv,execStatus,&ObjectBinaryData(theEnv,execStatus)->ClassCount);
      RestoreBloadCount(theEnv,execStatus,&ObjectBinaryData(theEnv,execStatus)->LinkCount);
      RestoreBloadCount(theEnv,execStatus,&ObjectBinaryData(theEnv,execStatus)->SlotCount);
      RestoreBloadCount(theEnv,execStatus,&ObjectBinaryData(theEnv,execStatus)->SlotNameCount);
      RestoreBloadCount(theEnv,execStatus,&ObjectBinaryData(theEnv,execStatus)->TemplateSlotCount);
      RestoreBloadCount(theEnv,execStatus,&ObjectBinaryData(theEnv,execStatus)->SlotNameMapCount);
      RestoreBloadCount(theEnv,execStatus,&ObjectBinaryData(theEnv,execStatus)->HandlerCount);
  }

/***************************************************
  NAME         : BsaveDefclass
  DESCRIPTION  : Writes defclass binary data
  INPUTS       : 1) The defclass
                 2) The binary file pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Defclass binary data written
  NOTES        : None
 ***************************************************/
static void BsaveDefclass(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theDefclass,
  void *buf)
  {
   DEFCLASS *cls = (DEFCLASS *) theDefclass;
   BSAVE_DEFCLASS dummy_class;

   AssignBsaveConstructHeaderVals(&dummy_class.header,&cls->header);
   dummy_class.abstract = cls->abstract;
   dummy_class.reactive = cls->reactive;
   dummy_class.system = cls->system;
   dummy_class.id = cls->id;
   dummy_class.slotCount = cls->slotCount;
   dummy_class.instanceSlotCount = cls->instanceSlotCount;
   dummy_class.localInstanceSlotCount = cls->localInstanceSlotCount;
   dummy_class.maxSlotNameID = cls->maxSlotNameID;
   dummy_class.handlerCount = cls->handlerCount;
   dummy_class.directSuperclasses.classCount = cls->directSuperclasses.classCount;
   dummy_class.directSubclasses.classCount = cls->directSubclasses.classCount;
   dummy_class.allSuperclasses.classCount = cls->allSuperclasses.classCount;
   if (cls->directSuperclasses.classCount != 0)
     {
      dummy_class.directSuperclasses.classArray = ObjectBinaryData(theEnv,execStatus)->LinkCount;
      ObjectBinaryData(theEnv,execStatus)->LinkCount += cls->directSuperclasses.classCount;
     }
   else
     dummy_class.directSuperclasses.classArray = -1L;
   if (cls->directSubclasses.classCount != 0)
     {
      dummy_class.directSubclasses.classArray = ObjectBinaryData(theEnv,execStatus)->LinkCount;
      ObjectBinaryData(theEnv,execStatus)->LinkCount += cls->directSubclasses.classCount;
     }
   else
     dummy_class.directSubclasses.classArray = -1L;
   if (cls->allSuperclasses.classCount != 0)
     {
      dummy_class.allSuperclasses.classArray = ObjectBinaryData(theEnv,execStatus)->LinkCount;
      ObjectBinaryData(theEnv,execStatus)->LinkCount += cls->allSuperclasses.classCount;
     }
   else
     dummy_class.allSuperclasses.classArray = -1L;
   if (cls->slots != NULL)
     {
      dummy_class.slots = ObjectBinaryData(theEnv,execStatus)->SlotCount;
      ObjectBinaryData(theEnv,execStatus)->SlotCount += (long) cls->slotCount;
     }
   else
     dummy_class.slots = -1L;
   if (cls->instanceTemplate != NULL)
     {
      dummy_class.instanceTemplate = ObjectBinaryData(theEnv,execStatus)->TemplateSlotCount;
      ObjectBinaryData(theEnv,execStatus)->TemplateSlotCount += (long) cls->instanceSlotCount;
      dummy_class.slotNameMap = ObjectBinaryData(theEnv,execStatus)->SlotNameMapCount;
      ObjectBinaryData(theEnv,execStatus)->SlotNameMapCount += (long) cls->maxSlotNameID + 1;
     }
   else
     {
      dummy_class.instanceTemplate = -1L;
      dummy_class.slotNameMap = -1L;
     }
   if (cls->handlers != NULL)
     {
      dummy_class.handlers = ObjectBinaryData(theEnv,execStatus)->HandlerCount;
      ObjectBinaryData(theEnv,execStatus)->HandlerCount += (long) cls->handlerCount;
     }
   else
     dummy_class.handlers = -1L;
#if DEFMODULE_CONSTRUCT
   dummy_class.scopeMap = (long) cls->scopeMap->bucket;
#else
   dummy_class.scopeMap = -1L;
#endif
   GenWrite((void *) &dummy_class,sizeof(BSAVE_DEFCLASS),(FILE *) buf);
  }

/***************************************************
  NAME         : BsaveClassLinks
  DESCRIPTION  : Writes class links binary data
  INPUTS       : 1) The defclass
                 2) The binary file pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Defclass links binary data written
  NOTES        : None
 ***************************************************/
static void BsaveClassLinks(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theDefclass,
  void *buf)
  {
   DEFCLASS *cls = (DEFCLASS *) theDefclass;
   long i;
   long dummy_class_index;

   for (i = 0 ;  i < cls->directSuperclasses.classCount ; i++)
     {
      dummy_class_index = DefclassIndex(cls->directSuperclasses.classArray[i]);
      GenWrite((void *) &dummy_class_index,sizeof(long),(FILE *) buf);
     }
   ObjectBinaryData(theEnv,execStatus)->LinkCount += cls->directSuperclasses.classCount;
   for (i = 0 ;  i < cls->directSubclasses.classCount ; i++)
     {
      dummy_class_index = DefclassIndex(cls->directSubclasses.classArray[i]);
      GenWrite((void *) &dummy_class_index,sizeof(long),(FILE *) buf);
     }
   ObjectBinaryData(theEnv,execStatus)->LinkCount += cls->directSubclasses.classCount;
   for (i = 0 ;  i < cls->allSuperclasses.classCount ; i++)
     {
      dummy_class_index = DefclassIndex(cls->allSuperclasses.classArray[i]);
      GenWrite((void *) &dummy_class_index,sizeof(long),(FILE *) buf);
     }
   ObjectBinaryData(theEnv,execStatus)->LinkCount += cls->allSuperclasses.classCount;
  }

/***************************************************
  NAME         : BsaveSlots
  DESCRIPTION  : Writes class slots binary data
  INPUTS       : 1) The defclass
                 2) The binary file pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Defclass slots binary data written
  NOTES        : None
 ***************************************************/
static void BsaveSlots(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theDefclass,
  void *buf)
  {
   DEFCLASS *cls = (DEFCLASS *) theDefclass;
   long i;
   BSAVE_SLOT_DESC dummy_slot;
   SLOT_DESC *sp;
   EXPRESSION *tmpexp;

   for (i = 0 ; i < cls->slotCount ; i++)
     {
      sp = &cls->slots[i];
      dummy_slot.dynamicDefault = sp->dynamicDefault;
      dummy_slot.noDefault = sp->noDefault;
      dummy_slot.shared = sp->shared;
      dummy_slot.multiple = sp->multiple;
      dummy_slot.composite = sp->composite;
      dummy_slot.noInherit = sp->noInherit;
      dummy_slot.noWrite = sp->noWrite;
      dummy_slot.initializeOnly = sp->initializeOnly;
      dummy_slot.reactive = sp->reactive;
      dummy_slot.publicVisibility = sp->publicVisibility;
      dummy_slot.createReadAccessor = sp->createReadAccessor;
      dummy_slot.createWriteAccessor = sp->createWriteAccessor;
      dummy_slot.cls = DefclassIndex(sp->cls);
      dummy_slot.slotName = SlotNameIndex(sp->slotName);
      dummy_slot.overrideMessage = (long) sp->overrideMessage->bucket;
      if (sp->defaultValue != NULL)
        {
         dummy_slot.defaultValue = ExpressionData(theEnv,execStatus)->ExpressionCount;
         if (sp->dynamicDefault)
           ExpressionData(theEnv,execStatus)->ExpressionCount += ExpressionSize((EXPRESSION *) sp->defaultValue);
         else
           {
            tmpexp = ConvertValueToExpression(theEnv,execStatus,(DATA_OBJECT *) sp->defaultValue);
            ExpressionData(theEnv,execStatus)->ExpressionCount += ExpressionSize(tmpexp);
            ReturnExpression(theEnv,execStatus,tmpexp);
           }
        }
      else
        dummy_slot.defaultValue = -1L;
      dummy_slot.constraint = ConstraintIndex(sp->constraint);
      GenWrite((void *) &dummy_slot,sizeof(BSAVE_SLOT_DESC),(FILE *) buf);
     }
  }

/**************************************************************
  NAME         : BsaveTemplateSlots
  DESCRIPTION  : Writes class instance template binary data
  INPUTS       : 1) The defclass
                 2) The binary file pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Defclass instance template binary data written
  NOTES        : None
 **************************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void BsaveTemplateSlots(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theDefclass,
  void *buf)
  {
   DEFCLASS *cls = (DEFCLASS *) theDefclass;
   long i;
   long tsp;
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv,execStatus)
#endif
   
   for (i = 0 ; i < cls->instanceSlotCount ; i++)
     {
      tsp = SlotIndex(cls->instanceTemplate[i]);
      GenWrite((void *) &tsp,sizeof(long),(FILE *) buf);
     }
  }

/***************************************************************
  NAME         : BsaveSlotMap
  DESCRIPTION  : Writes class canonical slot map binary data
  INPUTS       : 1) The defclass
                 2) The binary file pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Defclass canonical slot map binary data written
  NOTES        : None
 ***************************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void BsaveSlotMap(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theDefclass,
  void *buf)
  {
   DEFCLASS *cls = (DEFCLASS *) theDefclass;
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv,execStatus)
#endif

   if (cls->instanceSlotCount != 0)
     GenWrite((void *) cls->slotNameMap,
              (sizeof(unsigned) * (cls->maxSlotNameID + 1)),(FILE *) buf);
  }

/************************************************************
  NAME         : BsaveHandlers
  DESCRIPTION  : Writes class message-handlers binary data
  INPUTS       : 1) The defclass
                 2) The binary file pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Defclass message-handler binary data written
  NOTES        : None
 ************************************************************/
static void BsaveHandlers(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theDefclass,
  void *buf)
  {
   DEFCLASS *cls = (DEFCLASS *) theDefclass;
   long i;
   BSAVE_HANDLER dummy_handler;
   HANDLER *hnd;

   for (i = 0 ; i < cls->handlerCount ; i++)
     {
      hnd = &cls->handlers[i];
      dummy_handler.system = hnd->system;
      dummy_handler.type = hnd->type;
      dummy_handler.minParams = hnd->minParams;
      dummy_handler.maxParams = hnd->maxParams;
      dummy_handler.localVarCount = hnd->localVarCount;
      dummy_handler.cls = DefclassIndex(hnd->cls);
      dummy_handler.name = (long) hnd->name->bucket;
      if (hnd->actions != NULL)
        {
         dummy_handler.actions = ExpressionData(theEnv,execStatus)->ExpressionCount;
         ExpressionData(theEnv,execStatus)->ExpressionCount += ExpressionSize(hnd->actions);
        }
      else
        dummy_handler.actions = -1L;
      GenWrite((void *) &dummy_handler,sizeof(BSAVE_HANDLER),(FILE *) buf);
     }
  }

/****************************************************************
  NAME         : BsaveHandlerMap
  DESCRIPTION  : Writes class message-handler map binary data
  INPUTS       : 1) The defclass
                 2) The binary file pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Defclass message-handler map binary data written
  NOTES        : None
 ****************************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void BsaveHandlerMap(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theDefclass,
  void *buf)
  {
   DEFCLASS *cls = (DEFCLASS *) theDefclass;
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv,execStatus)
#endif

   GenWrite((void *) cls->handlerOrderMap,
            (sizeof(unsigned) * cls->handlerCount),(FILE *) buf);
  }

#endif

/***********************************************************************
  NAME         : BloadStorageObjects
  DESCRIPTION  : This routine reads class and handler information from
                 a binary file in five chunks:
                 Class count
                 Handler count
                 Class array
                 Handler array
  INPUTS       : Notthing
  RETURNS      : Nothing useful
  SIDE EFFECTS : Arrays allocated and set
  NOTES        : This routine makes no attempt to reset any pointers
                   within the structures
                 Bload fails if there are still classes in the system!!
 ***********************************************************************/
static void BloadStorageObjects(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;
   long counts[9];

   if ((DefclassData(theEnv,execStatus)->ClassIDMap != NULL) || (DefclassData(theEnv,execStatus)->MaxClassID != 0))
     {
      SystemError(theEnv,execStatus,"OBJBIN",1);
      EnvExitRouter(theEnv,execStatus,EXIT_FAILURE);
     }
   GenReadBinary(theEnv,execStatus,(void *) &space,sizeof(size_t));
   if (space == 0L)
     {
      ObjectBinaryData(theEnv,execStatus)->ClassCount = ObjectBinaryData(theEnv,execStatus)->HandlerCount = 0L;
      return;
     }
   GenReadBinary(theEnv,execStatus,(void *) counts,space);
   ObjectBinaryData(theEnv,execStatus)->ModuleCount = counts[0];
   ObjectBinaryData(theEnv,execStatus)->ClassCount = counts[1];
   ObjectBinaryData(theEnv,execStatus)->LinkCount = counts[2];
   ObjectBinaryData(theEnv,execStatus)->SlotNameCount = counts[3];
   ObjectBinaryData(theEnv,execStatus)->SlotCount = counts[4];
   ObjectBinaryData(theEnv,execStatus)->TemplateSlotCount = counts[5];
   ObjectBinaryData(theEnv,execStatus)->SlotNameMapCount = counts[6];
   ObjectBinaryData(theEnv,execStatus)->HandlerCount = counts[7];
   DefclassData(theEnv,execStatus)->MaxClassID = (unsigned short) counts[8];
   DefclassData(theEnv,execStatus)->AvailClassID = (unsigned short) counts[8];
   if (ObjectBinaryData(theEnv,execStatus)->ModuleCount != 0L)
     {
      space = (sizeof(DEFCLASS_MODULE) * ObjectBinaryData(theEnv,execStatus)->ModuleCount);
      ObjectBinaryData(theEnv,execStatus)->ModuleArray = (DEFCLASS_MODULE *) genalloc(theEnv,execStatus,space);
     }
   if (ObjectBinaryData(theEnv,execStatus)->ClassCount != 0L)
     {
      space = (sizeof(DEFCLASS) * ObjectBinaryData(theEnv,execStatus)->ClassCount);
      ObjectBinaryData(theEnv,execStatus)->DefclassArray = (DEFCLASS *) genalloc(theEnv,execStatus,space);
      DefclassData(theEnv,execStatus)->ClassIDMap = (DEFCLASS **) gm2(theEnv,execStatus,(sizeof(DEFCLASS *) * DefclassData(theEnv,execStatus)->MaxClassID));
     }
   if (ObjectBinaryData(theEnv,execStatus)->LinkCount != 0L)
     {
      space = (sizeof(DEFCLASS *) * ObjectBinaryData(theEnv,execStatus)->LinkCount);
      ObjectBinaryData(theEnv,execStatus)->LinkArray = (DEFCLASS * *) genalloc(theEnv,execStatus,space);
     }
   if (ObjectBinaryData(theEnv,execStatus)->SlotCount != 0L)
     {
      space = (sizeof(SLOT_DESC) * ObjectBinaryData(theEnv,execStatus)->SlotCount);
      ObjectBinaryData(theEnv,execStatus)->SlotArray = (SLOT_DESC *) genalloc(theEnv,execStatus,space);
     }
   if (ObjectBinaryData(theEnv,execStatus)->SlotNameCount != 0L)
     {
      space = (sizeof(SLOT_NAME) * ObjectBinaryData(theEnv,execStatus)->SlotNameCount);
      ObjectBinaryData(theEnv,execStatus)->SlotNameArray = (SLOT_NAME *) genalloc(theEnv,execStatus,space);
     }
   if (ObjectBinaryData(theEnv,execStatus)->TemplateSlotCount != 0L)
     {
      space = (sizeof(SLOT_DESC *) * ObjectBinaryData(theEnv,execStatus)->TemplateSlotCount);
      ObjectBinaryData(theEnv,execStatus)->TmpslotArray = (SLOT_DESC * *) genalloc(theEnv,execStatus,space);
     }
   if (ObjectBinaryData(theEnv,execStatus)->SlotNameMapCount != 0L)
     {
      space = (sizeof(unsigned) * ObjectBinaryData(theEnv,execStatus)->SlotNameMapCount);
      ObjectBinaryData(theEnv,execStatus)->MapslotArray = (unsigned *) genalloc(theEnv,execStatus,space);
     }
   if (ObjectBinaryData(theEnv,execStatus)->HandlerCount != 0L)
     {
      space = (sizeof(HANDLER) * ObjectBinaryData(theEnv,execStatus)->HandlerCount);
      ObjectBinaryData(theEnv,execStatus)->HandlerArray = (HANDLER *) genalloc(theEnv,execStatus,space);
      space = (sizeof(unsigned) * ObjectBinaryData(theEnv,execStatus)->HandlerCount);
      ObjectBinaryData(theEnv,execStatus)->MaphandlerArray = (unsigned *) genalloc(theEnv,execStatus,space);
     }
  }

/***************************************************************
  NAME         : BloadObjects
  DESCRIPTION  : This routine moves through the class and handler
                   binary arrays updating pointers
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Pointers reset from array indices
  NOTES        : Assumes all loading is finished
 **************************************************************/
static void BloadObjects(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;

   GenReadBinary(theEnv,execStatus,(void *) &space,sizeof(size_t));
   if (space == 0L)
     return;
   if (ObjectBinaryData(theEnv,execStatus)->ModuleCount != 0L)
     BloadandRefresh(theEnv,execStatus,ObjectBinaryData(theEnv,execStatus)->ModuleCount,sizeof(BSAVE_DEFCLASS_MODULE),UpdateDefclassModule);
   if (ObjectBinaryData(theEnv,execStatus)->ClassCount != 0L)
     {
      BloadandRefresh(theEnv,execStatus,ObjectBinaryData(theEnv,execStatus)->ClassCount,sizeof(BSAVE_DEFCLASS),UpdateDefclass);
      BloadandRefresh(theEnv,execStatus,ObjectBinaryData(theEnv,execStatus)->LinkCount,sizeof(DEFCLASS *),UpdateLink);
      BloadandRefresh(theEnv,execStatus,ObjectBinaryData(theEnv,execStatus)->SlotNameCount,sizeof(BSAVE_SLOT_NAME),UpdateSlotName);
      BloadandRefresh(theEnv,execStatus,ObjectBinaryData(theEnv,execStatus)->SlotCount,sizeof(BSAVE_SLOT_DESC),UpdateSlot);
      if (ObjectBinaryData(theEnv,execStatus)->TemplateSlotCount != 0L)
        BloadandRefresh(theEnv,execStatus,ObjectBinaryData(theEnv,execStatus)->TemplateSlotCount,sizeof(long),UpdateTemplateSlot);
      if (ObjectBinaryData(theEnv,execStatus)->SlotNameMapCount != 0L)
        {
         space = (sizeof(unsigned) * ObjectBinaryData(theEnv,execStatus)->SlotNameMapCount);
         GenReadBinary(theEnv,execStatus,(void *) ObjectBinaryData(theEnv,execStatus)->MapslotArray,space);
        }
      if (ObjectBinaryData(theEnv,execStatus)->HandlerCount != 0L)
        {
         BloadandRefresh(theEnv,execStatus,ObjectBinaryData(theEnv,execStatus)->HandlerCount,sizeof(BSAVE_HANDLER),UpdateHandler);
         space = (sizeof(unsigned) * ObjectBinaryData(theEnv,execStatus)->HandlerCount);
         GenReadBinary(theEnv,execStatus,(void *) ObjectBinaryData(theEnv,execStatus)->MaphandlerArray,space);
        }
      UpdatePrimitiveClassesMap(theEnv,execStatus);
     }
  }

/***************************************************
  NAME         : UpdatePrimitiveClassesMap
  DESCRIPTION  : Resets the pointers for the global
                 primitive classes map
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : PrimitiveClassMap pointers set
                 into bload array
  NOTES        : Looks at first nine primitive type
                 codes in the source file CONSTANT.H
 ***************************************************/
static void UpdatePrimitiveClassesMap(
  void *theEnv,
  EXEC_STATUS)
  {
   register unsigned i;

   for (i = 0 ; i < OBJECT_TYPE_CODE ; i++)
     DefclassData(theEnv,execStatus)->PrimitiveClassMap[i] = (DEFCLASS *) &ObjectBinaryData(theEnv,execStatus)->DefclassArray[i];
  }

/*********************************************************
  Refresh update routines for bsaved COOL structures
 *********************************************************/
static void UpdateDefclassModule(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   BSAVE_DEFCLASS_MODULE *bdptr;

   bdptr = (BSAVE_DEFCLASS_MODULE *) buf;
   UpdateDefmoduleItemHeader(theEnv,execStatus,&bdptr->header,&ObjectBinaryData(theEnv,execStatus)->ModuleArray[obji].header,
                             (int) sizeof(DEFCLASS),(void *) ObjectBinaryData(theEnv,execStatus)->DefclassArray);
  }

static void UpdateDefclass(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   BSAVE_DEFCLASS *bcls;
   DEFCLASS *cls;

   bcls = (BSAVE_DEFCLASS *) buf;
   cls = (DEFCLASS *) &ObjectBinaryData(theEnv,execStatus)->DefclassArray[obji];

   UpdateConstructHeader(theEnv,execStatus,&bcls->header,&cls->header,
                         (int) sizeof(DEFCLASS_MODULE),(void *) ObjectBinaryData(theEnv,execStatus)->ModuleArray,
                         (int) sizeof(DEFCLASS),(void *) ObjectBinaryData(theEnv,execStatus)->DefclassArray);
   cls->abstract = bcls->abstract;
   cls->reactive = bcls->reactive;
   cls->system = bcls->system;
   cls->id = bcls->id;
   DefclassData(theEnv,execStatus)->ClassIDMap[cls->id] = cls;
#if DEBUGGING_FUNCTIONS
   cls->traceInstances = DefclassData(theEnv,execStatus)->WatchInstances;
   cls->traceSlots = DefclassData(theEnv,execStatus)->WatchSlots;
#endif
   cls->slotCount = bcls->slotCount;
   cls->instanceSlotCount = bcls->instanceSlotCount;
   cls->localInstanceSlotCount = bcls->localInstanceSlotCount;
   cls->maxSlotNameID = bcls->maxSlotNameID;
   cls->handlerCount = bcls->handlerCount;
   cls->directSuperclasses.classCount =bcls->directSuperclasses.classCount;
   cls->directSuperclasses.classArray = LinkPointer(bcls->directSuperclasses.classArray);
   cls->directSubclasses.classCount =bcls->directSubclasses.classCount;
   cls->directSubclasses.classArray = LinkPointer(bcls->directSubclasses.classArray);
   cls->allSuperclasses.classCount =bcls->allSuperclasses.classCount;
   cls->allSuperclasses.classArray = LinkPointer(bcls->allSuperclasses.classArray);
   cls->slots = SlotPointer(bcls->slots);
   cls->instanceTemplate = TemplateSlotPointer(bcls->instanceTemplate);
   cls->slotNameMap = OrderedSlotPointer(bcls->slotNameMap);
   cls->instanceList = NULL;
   cls->handlers = HandlerPointer(bcls->handlers);
   cls->handlerOrderMap = OrderedHandlerPointer(bcls->handlers);
   cls->installed = 1;
   cls->busy = 0;
   cls->instanceList = NULL;
   cls->instanceListBottom = NULL;
#if DEFMODULE_CONSTRUCT
   cls->scopeMap = BitMapPointer(bcls->scopeMap);
   IncrementBitMapCount(cls->scopeMap);
#else
   cls->scopeMap = NULL;
#endif
   PutClassInTable(theEnv,execStatus,cls);
  }

static void UpdateLink(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   long *blink;
   
   blink = (long *) buf;
   ObjectBinaryData(theEnv,execStatus)->LinkArray[obji] = DefclassPointer(*blink);
  }

static void UpdateSlot(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   SLOT_DESC *sp;
   BSAVE_SLOT_DESC *bsp;

   sp = (SLOT_DESC *) &ObjectBinaryData(theEnv,execStatus)->SlotArray[obji];
   bsp = (BSAVE_SLOT_DESC *) buf;
   sp->dynamicDefault = bsp->dynamicDefault;
   sp->noDefault = bsp->noDefault;
   sp->shared = bsp->shared;
   sp->multiple = bsp->multiple;
   sp->composite = bsp->composite;
   sp->noInherit = bsp->noInherit;
   sp->noWrite = bsp->noWrite;
   sp->initializeOnly = bsp->initializeOnly;
   sp->reactive = bsp->reactive;
   sp->publicVisibility = bsp->publicVisibility;
   sp->createReadAccessor = bsp->createReadAccessor;
   sp->createWriteAccessor = bsp->createWriteAccessor;
   sp->cls = DefclassPointer(bsp->cls);
   sp->slotName = SlotNamePointer(bsp->slotName);
   sp->overrideMessage = SymbolPointer(bsp->overrideMessage);
   IncrementSymbolCount(sp->overrideMessage);
   if (bsp->defaultValue != -1L)
     {
      if (sp->dynamicDefault)
        sp->defaultValue = (void *) ExpressionPointer(bsp->defaultValue);
      else
        {
         sp->defaultValue = (void *) get_struct(theEnv,execStatus,dataObject);
         EvaluateAndStoreInDataObject(theEnv,execStatus,(int) sp->multiple,ExpressionPointer(bsp->defaultValue),
                                      (DATA_OBJECT *) sp->defaultValue,TRUE);
         ValueInstall(theEnv,execStatus,(DATA_OBJECT *) sp->defaultValue);
        }
     }
   else
     sp->defaultValue = NULL;
   sp->constraint = ConstraintPointer(bsp->constraint);
   sp->sharedCount = 0;
   sp->sharedValue.value = NULL;
   sp->bsaveIndex = 0L;
   if (sp->shared)
     {
      sp->sharedValue.desc = sp;
      sp->sharedValue.value = NULL;
     }
  }

static void UpdateSlotName(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   SLOT_NAME *snp;
   BSAVE_SLOT_NAME *bsnp;

   bsnp = (BSAVE_SLOT_NAME *) buf;
   snp = (SLOT_NAME *) &ObjectBinaryData(theEnv,execStatus)->SlotNameArray[obji];
   snp->id = bsnp->id;
   snp->name = SymbolPointer(bsnp->name);
   IncrementSymbolCount(snp->name);
   snp->putHandlerName = SymbolPointer(bsnp->putHandlerName);
   IncrementSymbolCount(snp->putHandlerName);
   snp->hashTableIndex = bsnp->hashTableIndex;
   snp->nxt = DefclassData(theEnv,execStatus)->SlotNameTable[snp->hashTableIndex];
   DefclassData(theEnv,execStatus)->SlotNameTable[snp->hashTableIndex] = snp;
  }

static void UpdateTemplateSlot(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   ObjectBinaryData(theEnv,execStatus)->TmpslotArray[obji] = SlotPointer(* (long *) buf);
  }

static void UpdateHandler(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   HANDLER *hnd;
   BSAVE_HANDLER *bhnd;

   hnd = (HANDLER *) &ObjectBinaryData(theEnv,execStatus)->HandlerArray[obji];
   bhnd = (BSAVE_HANDLER *) buf;
   hnd->system = bhnd->system;
   hnd->type = bhnd->type;

   hnd->minParams = bhnd->minParams;
   hnd->maxParams = bhnd->maxParams;
   hnd->localVarCount = bhnd->localVarCount;
   hnd->cls = DefclassPointer(bhnd->cls);
   hnd->name = SymbolPointer(bhnd->name);
   IncrementSymbolCount(hnd->name);
   hnd->actions = ExpressionPointer(bhnd->actions);
   hnd->ppForm = NULL;
   hnd->busy = 0;
   hnd->mark = 0;
   hnd->usrData = NULL;
#if DEBUGGING_FUNCTIONS
   hnd->trace = MessageHandlerData(theEnv,execStatus)->WatchHandlers;
#endif
  }

/***************************************************************
  NAME         : ClearBloadObjects
  DESCRIPTION  : Release all binary-loaded class and handler
                   structure arrays (and others)
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Memory cleared
  NOTES        : None
 ***************************************************************/
static void ClearBloadObjects(
  void *theEnv,
  EXEC_STATUS)
  {
   register long i;
   size_t space;

   space = (sizeof(DEFCLASS_MODULE) * ObjectBinaryData(theEnv,execStatus)->ModuleCount);
   if (space == 0L)
     return;
   genfree(theEnv,execStatus,(void *) ObjectBinaryData(theEnv,execStatus)->ModuleArray,space);
   ObjectBinaryData(theEnv,execStatus)->ModuleArray = NULL;
   ObjectBinaryData(theEnv,execStatus)->ModuleCount = 0L;

   if (ObjectBinaryData(theEnv,execStatus)->ClassCount != 0L)
     {
      rm(theEnv,execStatus,(void *) DefclassData(theEnv,execStatus)->ClassIDMap,(sizeof(DEFCLASS *) * DefclassData(theEnv,execStatus)->AvailClassID));
      DefclassData(theEnv,execStatus)->ClassIDMap = NULL;
      DefclassData(theEnv,execStatus)->MaxClassID = 0;
      DefclassData(theEnv,execStatus)->AvailClassID = 0;
      for (i = 0L ; i < ObjectBinaryData(theEnv,execStatus)->ClassCount ; i++)
        {
         UnmarkConstructHeader(theEnv,execStatus,&ObjectBinaryData(theEnv,execStatus)->DefclassArray[i].header);
#if DEFMODULE_CONSTRUCT
         DecrementBitMapCount(theEnv,execStatus,ObjectBinaryData(theEnv,execStatus)->DefclassArray[i].scopeMap);
#endif
         RemoveClassFromTable(theEnv,execStatus,(DEFCLASS *) &ObjectBinaryData(theEnv,execStatus)->DefclassArray[i]);
        }
      for (i = 0L ; i < ObjectBinaryData(theEnv,execStatus)->SlotCount ; i++)
        {
         DecrementSymbolCount(theEnv,execStatus,ObjectBinaryData(theEnv,execStatus)->SlotArray[i].overrideMessage);
         if ((ObjectBinaryData(theEnv,execStatus)->SlotArray[i].defaultValue != NULL) && (ObjectBinaryData(theEnv,execStatus)->SlotArray[i].dynamicDefault == 0))
           {
            ValueDeinstall(theEnv,execStatus,(DATA_OBJECT *) ObjectBinaryData(theEnv,execStatus)->SlotArray[i].defaultValue);
            rtn_struct(theEnv,execStatus,dataObject,ObjectBinaryData(theEnv,execStatus)->SlotArray[i].defaultValue);
           }
        }
      for (i = 0L ; i < ObjectBinaryData(theEnv,execStatus)->SlotNameCount ; i++)
        {
         DefclassData(theEnv,execStatus)->SlotNameTable[ObjectBinaryData(theEnv,execStatus)->SlotNameArray[i].hashTableIndex] = NULL;
         DecrementSymbolCount(theEnv,execStatus,ObjectBinaryData(theEnv,execStatus)->SlotNameArray[i].name);
         DecrementSymbolCount(theEnv,execStatus,ObjectBinaryData(theEnv,execStatus)->SlotNameArray[i].putHandlerName);
        }

      space = (sizeof(DEFCLASS) * ObjectBinaryData(theEnv,execStatus)->ClassCount);
      if (space != 0L)
        {
         genfree(theEnv,execStatus,(void *) ObjectBinaryData(theEnv,execStatus)->DefclassArray,space);
         ObjectBinaryData(theEnv,execStatus)->DefclassArray = NULL;
         ObjectBinaryData(theEnv,execStatus)->ClassCount = 0L;
        }

      space = (sizeof(DEFCLASS *) * ObjectBinaryData(theEnv,execStatus)->LinkCount);
      if (space != 0L)
        {
         genfree(theEnv,execStatus,(void *) ObjectBinaryData(theEnv,execStatus)->LinkArray,space);
         ObjectBinaryData(theEnv,execStatus)->LinkArray = NULL;
         ObjectBinaryData(theEnv,execStatus)->LinkCount = 0L;
        }

      space = (sizeof(SLOT_DESC) * ObjectBinaryData(theEnv,execStatus)->SlotCount);
      if (space != 0L)
        {
         genfree(theEnv,execStatus,(void *) ObjectBinaryData(theEnv,execStatus)->SlotArray,space);
         ObjectBinaryData(theEnv,execStatus)->SlotArray = NULL;
         ObjectBinaryData(theEnv,execStatus)->SlotCount = 0L;
        }

      space = (sizeof(SLOT_NAME) * ObjectBinaryData(theEnv,execStatus)->SlotNameCount);
      if (space != 0L)
        {
         genfree(theEnv,execStatus,(void *) ObjectBinaryData(theEnv,execStatus)->SlotNameArray,space);
         ObjectBinaryData(theEnv,execStatus)->SlotNameArray = NULL;
         ObjectBinaryData(theEnv,execStatus)->SlotNameCount = 0L;
        }

      space = (sizeof(SLOT_DESC *) * ObjectBinaryData(theEnv,execStatus)->TemplateSlotCount);
      if (space != 0L)
        {
         genfree(theEnv,execStatus,(void *) ObjectBinaryData(theEnv,execStatus)->TmpslotArray,space);
         ObjectBinaryData(theEnv,execStatus)->TmpslotArray = NULL;
         ObjectBinaryData(theEnv,execStatus)->TemplateSlotCount = 0L;
        }

      space = (sizeof(unsigned) * ObjectBinaryData(theEnv,execStatus)->SlotNameMapCount);
      if (space != 0L)
        {
         genfree(theEnv,execStatus,(void *) ObjectBinaryData(theEnv,execStatus)->MapslotArray,space);
         ObjectBinaryData(theEnv,execStatus)->MapslotArray = NULL;
         ObjectBinaryData(theEnv,execStatus)->SlotNameMapCount = 0L;
        }
     }

   if (ObjectBinaryData(theEnv,execStatus)->HandlerCount != 0L)
     {
      for (i = 0L ; i < ObjectBinaryData(theEnv,execStatus)->HandlerCount ; i++)
        DecrementSymbolCount(theEnv,execStatus,ObjectBinaryData(theEnv,execStatus)->HandlerArray[i].name);

      space = (sizeof(HANDLER) * ObjectBinaryData(theEnv,execStatus)->HandlerCount);
      if (space != 0L)
        {
         genfree(theEnv,execStatus,(void *) ObjectBinaryData(theEnv,execStatus)->HandlerArray,space);
         ObjectBinaryData(theEnv,execStatus)->HandlerArray = NULL;
         space = (sizeof(unsigned) * ObjectBinaryData(theEnv,execStatus)->HandlerCount);
         genfree(theEnv,execStatus,(void *) ObjectBinaryData(theEnv,execStatus)->MaphandlerArray,space);
         ObjectBinaryData(theEnv,execStatus)->MaphandlerArray = NULL;
         ObjectBinaryData(theEnv,execStatus)->HandlerCount = 0L;
        }
     }
  }

#endif


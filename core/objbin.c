   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/09/97          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Binary Load/Save Functions for Classes and their */
/*             message-handlers                              */
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

#include <stdlib.h>

#include "setup.h"

#if OBJECT_SYSTEM && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)

#include "bload.h"
#include "bsave.h"
#include "classcom.h"
#include "classfun.h"
#include "classini.h"
#include "memalloc.h"
#include "cstrcbin.h"
#include "cstrnbin.h"
#include "insfun.h"
#include "modulbin.h"
#include "msgfun.h"
#include "prntutil.h"
#include "router.h"

#define _OBJBIN_SOURCE_
#include "objbin.h"

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
typedef unsigned long UNLN;

#define SlotIndex(p)             (((p) != NULL) ? (p)->bsaveIndex : -1L)
#define SlotNameIndex(p)         (p)->bsaveIndex

#define LinkPointer(i)           (((i) == -1L) ? NULL : (DEFCLASS **) &linkArray[i])
#define SlotPointer(i)           (((i) == -1L) ? NULL : (SLOT_DESC *) &slotArray[i])
#define TemplateSlotPointer(i)   (((i) == -1L) ? NULL : (SLOT_DESC **) &tmpslotArray[i])
#define OrderedSlotPointer(i)    (((i) == -1L) ? NULL : (unsigned *) &mapslotArray[i])
#define SlotNamePointer(i)       ((SLOT_NAME *) &slotNameArray[i])
#define HandlerPointer(i)        (((i) == -1L) ? NULL : (HANDLER *) &handlerArray[i])
#define OrderedHandlerPointer(i) (((i) == -1L) ? NULL : (unsigned *) &maphandlerArray[i])

typedef struct bsaveDefclassModule
  {
   struct bsaveDefmoduleItemHeader header;
  } BSAVE_DEFCLASS_MODULE;

typedef struct bsavePackedClassLinks
  {
   unsigned short classCount;
   long classArray;
  } BSAVE_PACKED_CLASS_LINKS;

typedef struct bsaveDefclass
  {
   struct bsaveConstructHeader header;
   unsigned abstract : 1;
   unsigned reactive : 1;
   unsigned system   : 1;
   unsigned short id;
   BSAVE_PACKED_CLASS_LINKS directSuperclasses,
                            directSubclasses,
                            allSuperclasses;
   unsigned slotCount,localInstanceSlotCount,
            instanceSlotCount,maxSlotNameID;
   unsigned handlerCount;
   long slots,
        instanceTemplate,
        slotNameMap,
        handlers,
        scopeMap;
  } BSAVE_DEFCLASS;

typedef struct bsaveSlotName
  {
   unsigned id,
            hashTableIndex;
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
   int minParams,
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

static void BsaveObjectsFind(void);
static void MarkDefclassItems(struct constructHeader *,void *);
static void BsaveObjectsExpressions(FILE *);
static void BsaveDefaultSlotExpressions(struct constructHeader *,void *);
static void BsaveHandlerActionExpressions(struct constructHeader *,void *);
static void BsaveStorageObjects(FILE *);
static void BsaveObjects(FILE *);
static void BsaveDefclass(struct constructHeader *,void *);
static void BsaveClassLinks(struct constructHeader *,void *);
static void BsaveSlots(struct constructHeader *,void *);
static void BsaveTemplateSlots(struct constructHeader *,void *);
static void BsaveSlotMap(struct constructHeader *,void *);
static void BsaveHandlers(struct constructHeader *,void *);
static void BsaveHandlerMap(struct constructHeader *,void *);

#endif

static void BloadStorageObjects(void);
static void BloadObjects(void);
static void UpdatePrimitiveClassesMap(void);
static void UpdateDefclassModule(void *,long);
static void UpdateDefclass(void *,long);
static void UpdateLink(void *,long);
static void UpdateSlot(void *,long);
static void UpdateSlotName(void *,long);
static void UpdateTemplateSlot(void *,long);
static void UpdateHandler(void *,long);
static void ClearBloadObjects(void);

/* =========================================
   *****************************************
      EXTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */
Thread globle DEFCLASS *defclassArray = NULL;

/* =========================================
   *****************************************
      INTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */

Thread static long ModuleCount,
            ClassCount,
            LinkCount,
            SlotCount,
            SlotNameCount,
            TemplateSlotCount,
            SlotNameMapCount,
            HandlerCount;

Thread static DEFCLASS_MODULE *ModuleArray;
Thread static DEFCLASS * *linkArray = NULL;
Thread static SLOT_DESC *slotArray = NULL;
Thread static SLOT_DESC * *tmpslotArray = NULL;
Thread static SLOT_NAME *slotNameArray = NULL;
Thread static unsigned *mapslotArray = NULL;
Thread static HANDLER *handlerArray = NULL;
Thread static unsigned *maphandlerArray = NULL;

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
globle void SetupObjectsBload()
  {
   AddAbortBloadFunction("defclass",CreateSystemClasses,0);

#if BLOAD_AND_BSAVE
   AddBinaryItem("defclass",0,BsaveObjectsFind,BsaveObjectsExpressions,
                             BsaveStorageObjects,BsaveObjects,
                             BloadStorageObjects,BloadObjects,
                             ClearBloadObjects);
#endif
#if BLOAD || BLOAD_ONLY
   AddBinaryItem("defclass",0,NULL,NULL,NULL,NULL,
                             BloadStorageObjects,BloadObjects,
                             ClearBloadObjects);
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
  int index)
  {
   return ((void *) &ModuleArray[index]);
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
static void BsaveObjectsFind()
  {
   register unsigned i;
   SLOT_NAME *snp;

   /* ========================================================
      The counts need to be saved in case a bload is in effect
      ======================================================== */
   if (Bloaded())
     {
      SaveBloadCount(ModuleCount);
      SaveBloadCount(ClassCount);
      SaveBloadCount(LinkCount);
      SaveBloadCount(SlotNameCount);
      SaveBloadCount(SlotCount);
      SaveBloadCount(TemplateSlotCount);
      SaveBloadCount(SlotNameMapCount);
      SaveBloadCount(HandlerCount);
     }

   ModuleCount= 0L;
   ClassCount = 0L;
   SlotCount = 0L;
   SlotNameCount = 0L;
   LinkCount = 0L;
   TemplateSlotCount = 0L;
   SlotNameMapCount = 0L;
   HandlerCount = 0L;

   /* ==============================================
      Mark items needed by defclasses in all modules
      ============================================== */
   ModuleCount = DoForAllConstructs(MarkDefclassItems,DefclassModuleIndex,
                                    FALSE,NULL);

   /* =============================================
      Mark items needed by canonicalized slot names
      ============================================= */
   for (i = 0 ; i < SLOT_NAME_TABLE_HASH_SIZE ; i++)
     for (snp = SlotNameTable[i] ; snp != NULL ; snp = snp->nxt)
       {
        if ((snp->id != ISA_ID) && (snp->id != NAME_ID))
          {
           snp->bsaveIndex = SlotNameCount++;
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
#if IBM_TBC
#pragma argsused
#endif
static void MarkDefclassItems(
  struct constructHeader *theDefclass,
  void *buf)
  {
#if MAC_MPW || MAC_MCW || IBM_MCW
#pragma unused(buf)
#endif
   DEFCLASS *cls = (DEFCLASS *) theDefclass;
   register unsigned i;
   EXPRESSION *tmpexp;

   MarkConstructHeaderNeededItems(&cls->header,ClassCount++);
   LinkCount += cls->directSuperclasses.classCount +
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
      cls->slots[i].bsaveIndex = SlotCount++;
      cls->slots[i].overrideMessage->neededSymbol = TRUE;
      if (cls->slots[i].defaultValue != NULL)
        {
         if (cls->slots[i].dynamicDefault)
           {
            ExpressionCount +=
              ExpressionSize((EXPRESSION *) cls->slots[i].defaultValue);
            MarkNeededItems((EXPRESSION *) cls->slots[i].defaultValue);
           }
         else
           {
            /* =================================================
               Static default values are stotred as data objects
               and must be converted into expressions
               ================================================= */
            tmpexp =
              ConvertValueToExpression((DATA_OBJECT *) cls->slots[i].defaultValue);
            ExpressionCount += ExpressionSize(tmpexp);
            MarkNeededItems(tmpexp);
            ReturnExpression(tmpexp);
           }
        }
     }

   /* ========================================
      Count canonical slots needed by defclass
      ======================================== */
   TemplateSlotCount += cls->instanceSlotCount;
   if (cls->instanceSlotCount != 0)
     SlotNameMapCount += cls->maxSlotNameID + 1;

   /* ===============================================
      Mark items needed by defmessage-handler actions
      =============================================== */
   for (i = 0 ; i < cls->handlerCount ; i++)
     {
      cls->handlers[i].name->neededSymbol = TRUE;
      ExpressionCount += ExpressionSize(cls->handlers[i].actions);
      MarkNeededItems(cls->handlers[i].actions);
     }
   HandlerCount += cls->handlerCount;
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
  FILE *fp)
  {
   if ((ClassCount == 0L) && (HandlerCount == 0L))
     return;

   /* ================================================
      Save the defclass slot default value expressions
      ================================================ */
   DoForAllConstructs(BsaveDefaultSlotExpressions,DefclassModuleIndex,
                      FALSE,(void *) fp);

   /* ==============================================
      Save the defmessage-handler action expressions
      ============================================== */
   DoForAllConstructs(BsaveHandlerActionExpressions,DefclassModuleIndex,
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
  struct constructHeader *theDefclass,
  void *buf)
  {
   DEFCLASS *cls = (DEFCLASS *) theDefclass;
   register unsigned i;
   EXPRESSION *tmpexp;

   for (i = 0 ; i < cls->slotCount ; i++)
     {
      if (cls->slots[i].defaultValue != NULL)
        {
         if (cls->slots[i].dynamicDefault)
           BsaveExpression((EXPRESSION *) cls->slots[i].defaultValue,(FILE *) buf);
         else
           {
            /* =================================================
               Static default values are stotred as data objects
               and must be converted into expressions
               ================================================= */
            tmpexp =
              ConvertValueToExpression((DATA_OBJECT *) cls->slots[i].defaultValue);
            BsaveExpression(tmpexp,(FILE *) buf);
            ReturnExpression(tmpexp);
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
  struct constructHeader *theDefclass,
  void *buf)
  {
   DEFCLASS *cls = (DEFCLASS *) theDefclass;
   register unsigned i;

   for (i = 0 ; i < cls->handlerCount ; i++)
     BsaveExpression(cls->handlers[i].actions,(FILE *) buf);
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
  FILE *fp)
  {
   UNLN space;

   if ((ClassCount == 0L) && (HandlerCount == 0L))
     {
      space = 0L;
      GenWrite((void *) &space,(UNLN) sizeof(long),fp);
      return;
     }
   space = sizeof(long) * 9;
   GenWrite((void *) &space,(UNLN) sizeof(long),fp);
   GenWrite((void *) &ModuleCount,(UNLN) sizeof(long),fp);
   GenWrite((void *) &ClassCount,(UNLN) sizeof(long),fp);
   GenWrite((void *) &LinkCount,(UNLN) sizeof(long),fp);
   GenWrite((void *) &SlotNameCount,(UNLN) sizeof(long),fp);
   GenWrite((void *) &SlotCount,(UNLN) sizeof(long),fp);
   GenWrite((void *) &TemplateSlotCount,(UNLN) sizeof(long),fp);
   GenWrite((void *) &SlotNameMapCount,(UNLN) sizeof(long),fp);
   GenWrite((void *) &HandlerCount,(UNLN) sizeof(long),fp);
   space = (UNLN) MaxClassID;
   GenWrite((void *) &space,(UNLN) sizeof(long),fp);
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
  FILE *fp)
  {
   UNLN space;
   struct defmodule *theModule;
   DEFCLASS_MODULE *theModuleItem;
   BSAVE_DEFCLASS_MODULE dummy_mitem;
   BSAVE_SLOT_NAME dummy_slot_name;
   SLOT_NAME *snp;
   register unsigned i;

   if ((ClassCount == 0L) && (HandlerCount == 0L))
     {
      space = 0L;
      GenWrite((void *) &space,(UNLN) sizeof(UNLN),fp);
      return;
     }
   space = (ModuleCount * (UNLN) sizeof(BSAVE_DEFCLASS_MODULE)) +
           (ClassCount * (UNLN) sizeof(BSAVE_DEFCLASS)) +
           (LinkCount * (UNLN) sizeof(long)) +
           (SlotCount * (UNLN) sizeof(BSAVE_SLOT_DESC)) +
           (SlotNameCount * (UNLN) sizeof(BSAVE_SLOT_NAME)) +
           (TemplateSlotCount * (UNLN) sizeof(long)) +
           (SlotNameMapCount * (UNLN) sizeof(unsigned)) +
           (HandlerCount * (UNLN) sizeof(BSAVE_HANDLER)) +
           (HandlerCount * (UNLN) sizeof(unsigned));
   GenWrite((void *) &space,(UNLN) sizeof(UNLN),fp);

   ClassCount = 0L;
   LinkCount = 0L;
   SlotCount = 0L;
   SlotNameCount = 0L;
   TemplateSlotCount = 0L;
   SlotNameMapCount = 0L;
   HandlerCount = 0L;

   /* =================================
      Write out each defclass module
      ================================= */
   theModule = (struct defmodule *) GetNextDefmodule(NULL);
   while (theModule != NULL)
     {
      theModuleItem = (DEFCLASS_MODULE *)
                      GetModuleItem(theModule,FindModuleItem("defclass")->moduleIndex);
      AssignBsaveDefmdlItemHdrVals(&dummy_mitem.header,&theModuleItem->header);
      GenWrite((void *) &dummy_mitem,(unsigned long) sizeof(BSAVE_DEFCLASS_MODULE),fp);
      theModule = (struct defmodule *) GetNextDefmodule((void *) theModule);
     }

   /* =====================
      Write out the classes
      ===================== */
   DoForAllConstructs(BsaveDefclass,DefclassModuleIndex,FALSE,(void *) fp);

   /* =========================
      Write out the class links
      ========================= */
   LinkCount = 0L;
   DoForAllConstructs(BsaveClassLinks,DefclassModuleIndex,FALSE,(void *) fp);

   /* ===============================
      Write out the slot name entries
      =============================== */
   for (i = 0 ; i < SLOT_NAME_TABLE_HASH_SIZE ; i++)
     for (snp = SlotNameTable[i] ; snp != NULL ; snp = snp->nxt)
     {
      if ((snp->id != ISA_ID) && (snp->id != NAME_ID))
        {
         dummy_slot_name.id = snp->id;
         dummy_slot_name.hashTableIndex = snp->hashTableIndex;
         dummy_slot_name.name = (long) snp->name->bucket;
         dummy_slot_name.putHandlerName = (long) snp->putHandlerName->bucket;
         GenWrite((void *) &dummy_slot_name,(UNLN) sizeof(BSAVE_SLOT_NAME),fp);
        }
     }

   /* ===================
      Write out the slots
      =================== */
   DoForAllConstructs(BsaveSlots,DefclassModuleIndex,FALSE,(void *) fp);

   /* =====================================
      Write out the template instance slots
      ===================================== */
   DoForAllConstructs(BsaveTemplateSlots,DefclassModuleIndex,FALSE,(void *) fp);

   /* =============================================
      Write out the ordered instance slot name maps
      ============================================= */
   DoForAllConstructs(BsaveSlotMap,DefclassModuleIndex,FALSE,(void *) fp);

   /* ==============================
      Write out the message-handlers
      ============================== */
   DoForAllConstructs(BsaveHandlers,DefclassModuleIndex,FALSE,(void *) fp);

   /* ==========================================
      Write out the ordered message-handler maps
      ========================================== */
   DoForAllConstructs(BsaveHandlerMap,DefclassModuleIndex,FALSE,(void *) fp);

   if (Bloaded())
     {
      RestoreBloadCount(&ModuleCount);
      RestoreBloadCount(&ClassCount);
      RestoreBloadCount(&LinkCount);
      RestoreBloadCount(&SlotCount);
      RestoreBloadCount(&SlotNameCount);
      RestoreBloadCount(&TemplateSlotCount);
      RestoreBloadCount(&SlotNameMapCount);
      RestoreBloadCount(&HandlerCount);
     }
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
      dummy_class.directSuperclasses.classArray = LinkCount;
      LinkCount += cls->directSuperclasses.classCount;
     }
   else
     dummy_class.directSuperclasses.classArray = -1L;
   if (cls->directSubclasses.classCount != 0)
     {
      dummy_class.directSubclasses.classArray = LinkCount;
      LinkCount += cls->directSubclasses.classCount;
     }
   else
     dummy_class.directSubclasses.classArray = -1L;
   if (cls->allSuperclasses.classCount != 0)
     {
      dummy_class.allSuperclasses.classArray = LinkCount;
      LinkCount += cls->allSuperclasses.classCount;
     }
   else
     dummy_class.allSuperclasses.classArray = -1L;
   if (cls->slots != NULL)
     {
      dummy_class.slots = SlotCount;
      SlotCount += cls->slotCount;
     }
   else
     dummy_class.slots = -1L;
   if (cls->instanceTemplate != NULL)
     {
      dummy_class.instanceTemplate = TemplateSlotCount;
      TemplateSlotCount += cls->instanceSlotCount;
      dummy_class.slotNameMap = SlotNameMapCount;
      SlotNameMapCount += cls->maxSlotNameID + 1;
     }
   else
     {
      dummy_class.instanceTemplate = -1L;
      dummy_class.slotNameMap = -1L;
     }
   if (cls->handlers != NULL)
     {
      dummy_class.handlers = HandlerCount;
      HandlerCount += cls->handlerCount;
     }
   else
     dummy_class.handlers = -1L;
#if DEFMODULE_CONSTRUCT
   dummy_class.scopeMap = (long) cls->scopeMap->bucket;
#else
   dummy_class.scopeMap = -1L;
#endif
   GenWrite((void *) &dummy_class,(UNLN) sizeof(BSAVE_DEFCLASS),(FILE *) buf);
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
  struct constructHeader *theDefclass,
  void *buf)
  {
   DEFCLASS *cls = (DEFCLASS *) theDefclass;
   register unsigned i;
   long dummy_class_index;

   for (i = 0 ;  i < cls->directSuperclasses.classCount ; i++)
     {
      dummy_class_index = DefclassIndex(cls->directSuperclasses.classArray[i]);
      GenWrite((void *) &dummy_class_index,(UNLN) sizeof(long),(FILE *) buf);
     }
   LinkCount += cls->directSuperclasses.classCount;
   for (i = 0 ;  i < cls->directSubclasses.classCount ; i++)
     {
      dummy_class_index = DefclassIndex(cls->directSubclasses.classArray[i]);
      GenWrite((void *) &dummy_class_index,(UNLN) sizeof(long),(FILE *) buf);
     }
   LinkCount += cls->directSubclasses.classCount;
   for (i = 0 ;  i < cls->allSuperclasses.classCount ; i++)
     {
      dummy_class_index = DefclassIndex(cls->allSuperclasses.classArray[i]);
      GenWrite((void *) &dummy_class_index,(UNLN) sizeof(long),(FILE *) buf);
     }
   LinkCount += cls->allSuperclasses.classCount;
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
  struct constructHeader *theDefclass,
  void *buf)
  {
   DEFCLASS *cls = (DEFCLASS *) theDefclass;
   register unsigned i;
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
         dummy_slot.defaultValue = ExpressionCount;
         if (sp->dynamicDefault)
           ExpressionCount += ExpressionSize((EXPRESSION *) sp->defaultValue);
         else
           {
            tmpexp = ConvertValueToExpression((DATA_OBJECT *) sp->defaultValue);
            ExpressionCount += ExpressionSize(tmpexp);
            ReturnExpression(tmpexp);
           }
        }
      else
        dummy_slot.defaultValue = -1L;
      dummy_slot.constraint = ConstraintIndex(sp->constraint);
      GenWrite((void *) &dummy_slot,(UNLN) sizeof(BSAVE_SLOT_DESC),(FILE *) buf);
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
static void BsaveTemplateSlots(
  struct constructHeader *theDefclass,
  void *buf)
  {
   DEFCLASS *cls = (DEFCLASS *) theDefclass;
   register unsigned i;
   long tsp;

   for (i = 0 ; i < cls->instanceSlotCount ; i++)
     {
      tsp = SlotIndex(cls->instanceTemplate[i]);
      GenWrite((void *) &tsp,(UNLN) sizeof(long),(FILE *) buf);
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
static void BsaveSlotMap(
  struct constructHeader *theDefclass,
  void *buf)
  {
   DEFCLASS *cls = (DEFCLASS *) theDefclass;

   if (cls->instanceSlotCount != 0)
     GenWrite((void *) cls->slotNameMap,
              (UNLN) (sizeof(unsigned) * (cls->maxSlotNameID + 1)),(FILE *) buf);
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
  struct constructHeader *theDefclass,
  void *buf)
  {
   DEFCLASS *cls = (DEFCLASS *) theDefclass;
   register unsigned i;
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
         dummy_handler.actions = ExpressionCount;
         ExpressionCount += ExpressionSize(hnd->actions);
        }
      else
        dummy_handler.actions = -1L;
      GenWrite((void *) &dummy_handler,(UNLN) sizeof(BSAVE_HANDLER),(FILE *) buf);
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
static void BsaveHandlerMap(
  struct constructHeader *theDefclass,
  void *buf)
  {
   DEFCLASS *cls = (DEFCLASS *) theDefclass;

   GenWrite((void *) cls->handlerOrderMap,
            (UNLN) (sizeof(unsigned) * cls->handlerCount),(FILE *) buf);
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
static void BloadStorageObjects()
  {
   UNLN space;
   long counts[9];

   if ((ClassIDMap != NULL) || (MaxClassID != 0))
     {
      SystemError("OBJBIN",1);
      ExitRouter(EXIT_FAILURE);
     }
   GenRead((void *) &space,(UNLN) sizeof(UNLN));
   if (space == 0L)
     {
      ClassCount = HandlerCount = 0L;
      return;
     }
   GenRead((void *) counts,space);
   ModuleCount = counts[0];
   ClassCount = counts[1];
   LinkCount = counts[2];
   SlotNameCount = counts[3];
   SlotCount = counts[4];
   TemplateSlotCount = counts[5];
   SlotNameMapCount = counts[6];
   HandlerCount = counts[7];
   MaxClassID = (unsigned short) counts[8];
   if (ModuleCount != 0L)
     {
      space = (UNLN) (sizeof(DEFCLASS_MODULE) * ModuleCount);
      ModuleArray = (DEFCLASS_MODULE *) genlongalloc(space);
     }
   if (ClassCount != 0L)
     {
      space = (UNLN) (sizeof(DEFCLASS) * ClassCount);
      defclassArray = (DEFCLASS *) genlongalloc(space);
      ClassIDMap = (DEFCLASS **) gm2((int) (sizeof(DEFCLASS *) * MaxClassID));
     }
   if (LinkCount != 0L)
     {
      space = (UNLN) (sizeof(DEFCLASS *) * LinkCount);
      linkArray = (DEFCLASS * *) genlongalloc(space);
     }
   if (SlotCount != 0L)
     {
      space = (UNLN) (sizeof(SLOT_DESC) * SlotCount);
      slotArray = (SLOT_DESC *) genlongalloc(space);
     }
   if (SlotNameCount != 0L)
     {
      space = (UNLN) (sizeof(SLOT_NAME) * SlotNameCount);
      slotNameArray = (SLOT_NAME *) genlongalloc(space);
     }
   if (TemplateSlotCount != 0L)
     {
      space = (UNLN) (sizeof(SLOT_DESC *) * TemplateSlotCount);
      tmpslotArray = (SLOT_DESC * *) genlongalloc(space);
     }
   if (SlotNameMapCount != 0L)
     {
      space = (UNLN) (sizeof(unsigned) * SlotNameMapCount);
      mapslotArray = (unsigned *) genlongalloc(space);
     }
   if (HandlerCount != 0L)
     {
      space = (UNLN) (sizeof(HANDLER) * HandlerCount);
      handlerArray = (HANDLER *) genlongalloc(space);
      space = (UNLN) (sizeof(unsigned) * HandlerCount);
      maphandlerArray = (unsigned *) genlongalloc(space);
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
static void BloadObjects()
  {
   UNLN space;

   GenRead((void *) &space,(UNLN) sizeof(UNLN));
   if (space == 0L)
     return;
   if (ModuleCount != 0L)
     BloadandRefresh(ModuleCount,(unsigned) sizeof(BSAVE_DEFCLASS_MODULE),UpdateDefclassModule);
   if (ClassCount != 0L)
     {
      BloadandRefresh(ClassCount,(unsigned) sizeof(BSAVE_DEFCLASS),UpdateDefclass);
      BloadandRefresh(LinkCount,(unsigned) sizeof(DEFCLASS *),UpdateLink);
      BloadandRefresh(SlotNameCount,(unsigned) sizeof(BSAVE_SLOT_NAME),UpdateSlotName);
      BloadandRefresh(SlotCount,(unsigned) sizeof(BSAVE_SLOT_DESC),UpdateSlot);
      if (TemplateSlotCount != 0L)
        BloadandRefresh(TemplateSlotCount,(unsigned) sizeof(long),UpdateTemplateSlot);
      if (SlotNameMapCount != 0L)
        {
         space = (UNLN) (sizeof(unsigned) * SlotNameMapCount);
         GenRead((void *) mapslotArray,space);
        }
      if (HandlerCount != 0L)
        {
         BloadandRefresh(HandlerCount,(unsigned) sizeof(BSAVE_HANDLER),UpdateHandler);
         space = (UNLN) (sizeof(unsigned) * HandlerCount);
         GenRead((void *) maphandlerArray,space);
        }
      UpdatePrimitiveClassesMap();
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
static void UpdatePrimitiveClassesMap()
  {
   register unsigned i;

   for (i = 0 ; i < OBJECT_TYPE_CODE ; i++)
     PrimitiveClassMap[i] = (DEFCLASS *) &defclassArray[i];
  }

/*********************************************************
  Refresh update routines for bsaved COOL structures
 *********************************************************/
static void UpdateDefclassModule(
  void *buf,
  long obji)
  {
   BSAVE_DEFCLASS_MODULE *bdptr;

   bdptr = (BSAVE_DEFCLASS_MODULE *) buf;
   UpdateDefmoduleItemHeader(&bdptr->header,&ModuleArray[obji].header,
                             (int) sizeof(DEFCLASS),(void *) defclassArray);
  }

static void UpdateDefclass(
  void *buf,
  long obji)
  {
   BSAVE_DEFCLASS *bcls;
   DEFCLASS *cls;

   bcls = (BSAVE_DEFCLASS *) buf;
   cls = (DEFCLASS *) &defclassArray[obji];

   UpdateConstructHeader(&bcls->header,&cls->header,
                         (int) sizeof(DEFCLASS_MODULE),(void *) ModuleArray,
                         (int) sizeof(DEFCLASS),(void *) defclassArray);
   cls->abstract = bcls->abstract;
   cls->reactive = bcls->reactive;
   cls->system = bcls->system;
   cls->id = bcls->id;
   ClassIDMap[cls->id] = cls;
#if DEBUGGING_FUNCTIONS
   cls->traceInstances = WatchInstances;
   cls->traceSlots = WatchSlots;
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
   PutClassInTable(cls);
  }

static void UpdateLink(
  void *buf,
  long obji)
  {
   long *blink;

   blink = (long *) buf;
   linkArray[obji] = DefclassPointer(*blink);
  }

static void UpdateSlot(
  void *buf,
  long obji)
  {
   SLOT_DESC *sp;
   BSAVE_SLOT_DESC *bsp;

   sp = (SLOT_DESC *) &slotArray[obji];
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
         sp->defaultValue = (void *) get_struct(dataObject);
         EvaluateAndStoreInDataObject((int) sp->multiple,ExpressionPointer(bsp->defaultValue),
                                      (DATA_OBJECT *) sp->defaultValue);
         ValueInstall((DATA_OBJECT *) sp->defaultValue);
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
  void *buf,
  long obji)
  {
   SLOT_NAME *snp;
   BSAVE_SLOT_NAME *bsnp;

   bsnp = (BSAVE_SLOT_NAME *) buf;
   snp = (SLOT_NAME *) &slotNameArray[obji];
   snp->id = bsnp->id;
   snp->name = SymbolPointer(bsnp->name);
   IncrementSymbolCount(snp->name);
   snp->putHandlerName = SymbolPointer(bsnp->putHandlerName);
   IncrementSymbolCount(snp->putHandlerName);
   snp->hashTableIndex = bsnp->hashTableIndex;
   snp->nxt = SlotNameTable[snp->hashTableIndex];
   SlotNameTable[snp->hashTableIndex] = snp;
  }

static void UpdateTemplateSlot(
  void *buf,
  long obji)
  {
   tmpslotArray[obji] = SlotPointer(* (long *) buf);
  }

static void UpdateHandler(
  void *buf,
  long obji)
  {
   HANDLER *hnd;
   BSAVE_HANDLER *bhnd;

   hnd = (HANDLER *) &handlerArray[obji];
   bhnd = (BSAVE_HANDLER *) buf;
   hnd->system = bhnd->system;
   hnd->type = bhnd->type;
#if (! IMPERATIVE_MESSAGE_HANDLERS)
   if (hnd->type == MAROUND)
     {
      PrintWarningID("OBJBIN",1,FALSE);
      PrintRouter(WWARNING,"Around message-handlers are not\n");
      PrintRouter(WWARNING,"  supported in this environment.");
     }
#endif
#if (! AUXILIARY_MESSAGE_HANDLERS)
   if ((hnd->type == MBEFORE) || (hnd->type == MAFTER))
     {
      PrintWarningID("OBJBIN",2,FALSE);
      PrintRouter(WWARNING,"Before and after message-handlers are not\n");
      PrintRouter(WWARNING,"  supported in this environment.");
     }
#endif
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
   hnd->trace = WatchHandlers;
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
static void ClearBloadObjects()
  {
   register long i;
   UNLN space;

   space = (unsigned long) (sizeof(DEFCLASS_MODULE) * ModuleCount);
   if (space == 0L)
     return;
   genlongfree((void *) ModuleArray,space);
   ModuleArray = NULL;
   ModuleCount = 0L;

   if (ClassCount != 0L)
     {
      rm((void *) ClassIDMap,(int) (sizeof(DEFCLASS *) * MaxClassID));
      ClassIDMap = NULL;
      MaxClassID = 0;
      for (i = 0L ; i < ClassCount ; i++)
        {
         UnmarkConstructHeader(&defclassArray[i].header);
#if DEFMODULE_CONSTRUCT
         DecrementBitMapCount(defclassArray[i].scopeMap);
#endif
         RemoveClassFromTable((DEFCLASS *) &defclassArray[i]);
        }
      for (i = 0L ; i < SlotCount ; i++)
        {
         DecrementSymbolCount(slotArray[i].overrideMessage);
         if ((slotArray[i].defaultValue != NULL) && (slotArray[i].dynamicDefault == 0))
           {
            ValueDeinstall((DATA_OBJECT *) slotArray[i].defaultValue);
            rtn_struct(dataObject,slotArray[i].defaultValue);
           }
        }
      for (i = 0L ; i < SlotNameCount ; i++)
        {
         SlotNameTable[slotNameArray[i].hashTableIndex] = NULL;
         DecrementSymbolCount(slotNameArray[i].name);
         DecrementSymbolCount(slotNameArray[i].putHandlerName);
        }

      space = (UNLN) (sizeof(DEFCLASS) * ClassCount);
      if (space != 0L)
        {
         genlongfree((void *) defclassArray,space);
         defclassArray = NULL;
         ClassCount = 0L;
        }

      space = (UNLN) (sizeof(DEFCLASS *) * LinkCount);
      if (space != 0L)
        {
         genlongfree((void *) linkArray,space);
         linkArray = NULL;
         LinkCount = 0L;
        }

      space = (UNLN) (sizeof(SLOT_DESC) * SlotCount);
      if (space != 0L)
        {
         genlongfree((void *) slotArray,space);
         slotArray = NULL;
         SlotCount = 0L;
        }

      space = (UNLN) (sizeof(SLOT_NAME) * SlotNameCount);
      if (space != 0L)
        {
         genlongfree((void *) slotNameArray,space);
         slotNameArray = NULL;
         SlotNameCount = 0L;
        }

      space = (UNLN) (sizeof(SLOT_DESC *) * TemplateSlotCount);
      if (space != 0L)
        {
         genlongfree((void *) tmpslotArray,space);
         tmpslotArray = NULL;
         TemplateSlotCount = 0L;
        }

      space = (UNLN) (sizeof(unsigned) * SlotNameMapCount);
      if (space != 0L)
        {
         genlongfree((void *) mapslotArray,space);
         mapslotArray = NULL;
         SlotNameMapCount = 0L;
        }
     }

   if (HandlerCount != 0L)
     {
      for (i = 0L ; i < HandlerCount ; i++)
        DecrementSymbolCount(handlerArray[i].name);

      space = (UNLN) (sizeof(HANDLER) * HandlerCount);
      if (space != 0L)
        {
         genlongfree((void *) handlerArray,space);
         handlerArray = NULL;
         space = (UNLN) (sizeof(unsigned) * HandlerCount);
         genlongfree((void *) maphandlerArray,space);
         maphandlerArray = NULL;
         HandlerCount = 0L;
        }
     }
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
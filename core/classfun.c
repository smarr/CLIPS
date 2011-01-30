   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*                CLASS FUNCTIONS MODULE               */
   /*******************************************************/

/*************************************************************/
/* Purpose: Internal class manipulation routines             */
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

#if OBJECT_SYSTEM

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
#include "bload.h"
#endif

#include "classcom.h"
#include "classini.h"
#include "memalloc.h"
#include "constant.h"
#include "constrct.h"
#include "cstrccom.h"
#include "cstrcpsr.h"
#include "evaluatn.h"
#include "inscom.h"
#include "insfun.h"
#include "insmngr.h"
#include "modulutl.h"
#include "msgfun.h"
#include "router.h"
#include "scanner.h"
#include "utility.h"

#define _CLASSFUN_SOURCE_
#include "classfun.h"

/* =========================================
   *****************************************
                   CONSTANTS
   =========================================
   ***************************************** */
#define BIG_PRIME          11329

#define CLASS_ID_MAP_CHUNK 30

#define PUT_PREFIX         "put-"
#define PUT_PREFIX_LENGTH  4

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

static unsigned HashSlotName(SYMBOL_HN *);

#if (! RUN_TIME)
static unsigned NewSlotNameID(void);
static void DeassignClassID(unsigned);
#endif

/* =========================================
   *****************************************
      EXTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */
Thread globle DEFCLASS **ClassIDMap = NULL;
Thread globle DEFCLASS **ClassTable = NULL;
Thread globle unsigned short MaxClassID = 0;
Thread globle SLOT_NAME **SlotNameTable = NULL;
Thread globle SYMBOL_HN *ISA_SYMBOL,
       globle           *NAME_SYMBOL;

#if INSTANCE_PATTERN_MATCHING
Thread globle SYMBOL_HN *INITIAL_OBJECT_SYMBOL;
#endif

#if DEBUGGING_FUNCTIONS
Thread globle int WatchInstances = OFF,
       globle     WatchSlots = OFF;
#endif

#if IBM_TBC
#pragma warn -ias
#endif
Thread globle DEFCLASS *PrimitiveClassMap[] = { NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL };
#if IBM_TBC
#pragma warn +ias
#endif

/* =========================================
   *****************************************
      INTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */
Thread static unsigned char CTID = 0;

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***************************************************
  NAME         : IncrementDefclassBusyCount
  DESCRIPTION  : Increments use count of defclass
  INPUTS       : The class
  RETURNS      : Nothing useful
  SIDE EFFECTS : Busy count incremented
  NOTES        : None
 ***************************************************/
globle void IncrementDefclassBusyCount(
  void *theDefclass)
  {
   ((DEFCLASS *) theDefclass)->busy++;
  }

/***************************************************
  NAME         : DecrementDefclassBusyCount
  DESCRIPTION  : Decrements use count of defclass
  INPUTS       : The class
  RETURNS      : Nothing useful
  SIDE EFFECTS : Busy count decremented
  NOTES        : Since use counts are ignored on
                 a clear and defclasses might be
                 deleted already anyway, this is
                 a no-op on a clear
 ***************************************************/
globle void DecrementDefclassBusyCount(
  void *theDefclass)
  {
   if (! ClearInProgress)
     ((DEFCLASS *) theDefclass)->busy--;
  }

/****************************************************
  NAME         : InstancesPurge
  DESCRIPTION  : Removes all instances
  INPUTS       : None
  RETURNS      : TRUE if all instances deleted,
                 FALSE otherwise
  SIDE EFFECTS : The instance hash table is cleared
  NOTES        : None
 ****************************************************/
globle BOOLEAN InstancesPurge()
  {
   int svdepth;

   DestroyAllInstances();
   svdepth = CurrentEvaluationDepth;
   if (CurrentEvaluationDepth == 0)
     CurrentEvaluationDepth = -1;
   CleanupInstances();
   CurrentEvaluationDepth = svdepth;
   return((InstanceList != NULL) ? FALSE : TRUE);
  }

#if ! RUN_TIME

/***************************************************
  NAME         : InitializeClasses
  DESCRIPTION  : Allocates class hash table
                 Initializes class hash table
                   to all NULL addresses
                 Creates system classes
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Hash table initialized
  NOTES        : None
 ***************************************************/
globle void InitializeClasses()
  {
   register int i;

   ClassTable =
      (DEFCLASS **) gm2((int) (sizeof(DEFCLASS *) * CLASS_TABLE_HASH_SIZE));
   for (i = 0 ; i < CLASS_TABLE_HASH_SIZE ; i++)
     ClassTable[i] = NULL;
   SlotNameTable =
      (SLOT_NAME **) gm2((int) (sizeof(SLOT_NAME *) * SLOT_NAME_TABLE_HASH_SIZE));
   for (i = 0 ; i < SLOT_NAME_TABLE_HASH_SIZE ; i++)
     SlotNameTable[i] = NULL;
  }

#endif

/********************************************************
  NAME         : FindClassSlot
  DESCRIPTION  : Searches for a named slot in a class
  INPUTS       : 1) The class address
                 2) The symbolic slot name
  RETURNS      : Address of slot if found, NULL otherwise
  SIDE EFFECTS : None
  NOTES        : Only looks in class defn, does not
                   examine inheritance paths
 ********************************************************/
globle SLOT_DESC *FindClassSlot(
  DEFCLASS *cls,
  SYMBOL_HN *sname)
  {
   register int i;

   for (i = 0 ; i < cls->slotCount ; i++)
     {
      if (cls->slots[i].slotName->name == sname)
        return(&cls->slots[i]);
     }
   return(NULL);
  }

/***************************************************************
  NAME         : ClassExistError
  DESCRIPTION  : Prints out error message for non-existent class
  INPUTS       : 1) Name of function having the error
                 2) The name of the non-existent class
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************************/
globle void ClassExistError(
  char *func,
  char *cname)
  {
   PrintErrorID("CLASSFUN",1,FALSE);
   PrintRouter(WERROR,"Unable to find class ");
   PrintRouter(WERROR,cname);
   PrintRouter(WERROR," in function ");
   PrintRouter(WERROR,func);
   PrintRouter(WERROR,".\n");
   SetEvaluationError(TRUE);
  }

/*********************************************
  NAME         : DeleteClassLinks
  DESCRIPTION  : Deallocates a class link list
  INPUTS       : The address of the list
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 *********************************************/
globle void DeleteClassLinks(
  CLASS_LINK *clink)
  {
   CLASS_LINK *ctmp;

   for (ctmp = clink ; ctmp != NULL ; ctmp = clink)
     {
      clink = clink->nxt;
      rtn_struct(classLink,ctmp);
     }
  }

/******************************************************
  NAME         : PrintClassName
  DESCRIPTION  : Displays a class's name
  INPUTS       : 1) Logical name of output
                 2) The class
                 3) Flag indicating whether to
                    print carriage-return at end
  RETURNS      : Nothing useful
  SIDE EFFECTS : Class name printed (and module name
                 too if class is not in current module)
  NOTES        : None
 ******************************************************/
globle void PrintClassName(
  char *logicalName,
  DEFCLASS *theDefclass,
  BOOLEAN linefeedFlag)
  {
   if ((theDefclass->header.whichModule->theModule != ((struct defmodule *) GetCurrentModule())) &&
       (theDefclass->system == 0))
     {
      PrintRouter(logicalName,
                 GetDefmoduleName(theDefclass->header.whichModule->theModule));
      PrintRouter(logicalName,"::");
     }
   PrintRouter(logicalName,ValueToString(theDefclass->header.name));
   if (linefeedFlag)
     PrintRouter(logicalName,"\n");
  }

#if DEBUGGING_FUNCTIONS || ((! BLOAD_ONLY) && (! RUN_TIME))

/***************************************************
  NAME         : PrintPackedClassLinks
  DESCRIPTION  : Displays the names of classes in
                 a list with a title
  INPUTS       : 1) The logical name of the output
                 2) Title string
                 3) List of class links
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle void PrintPackedClassLinks(
  char *logicalName,
  char *title,
  PACKED_CLASS_LINKS *plinks)
  {
   register unsigned i;

   PrintRouter(logicalName,title);
   for (i = 0 ; i < plinks->classCount ; i++)
     {
      PrintRouter(logicalName," ");
      PrintClassName(logicalName,plinks->classArray[i],FALSE);
     }
   PrintRouter(logicalName,"\n");
  }

#endif

#if ! RUN_TIME

/*******************************************************
  NAME         : PutClassInTable
  DESCRIPTION  : Inserts a class in the class hash table
  INPUTS       : The class
  RETURNS      : Nothing useful
  SIDE EFFECTS : Class inserted
  NOTES        : None
 *******************************************************/
globle void PutClassInTable(
  DEFCLASS *cls)
  {
   cls->hashTableIndex = HashClass(GetDefclassNamePointer((void *) cls));
   cls->nxtHash = ClassTable[cls->hashTableIndex];
   ClassTable[cls->hashTableIndex] = cls;
  }

/*********************************************************
  NAME         : RemoveClassFromTable
  DESCRIPTION  : Removes a class from the class hash table
  INPUTS       : The class
  RETURNS      : Nothing useful
  SIDE EFFECTS : Class removed
  NOTES        : None
 *********************************************************/
globle void RemoveClassFromTable(
  DEFCLASS *cls)
  {
   DEFCLASS *prvhsh,*hshptr;

   prvhsh = NULL;
   hshptr = ClassTable[cls->hashTableIndex];
   while (hshptr != cls)
     {
      prvhsh = hshptr;
      hshptr = hshptr->nxtHash;
     }
   if (prvhsh == NULL)
     ClassTable[cls->hashTableIndex] = cls->nxtHash;
   else
     prvhsh->nxtHash = cls->nxtHash;
  }

/***************************************************
  NAME         : AddClassLink
  DESCRIPTION  : Adds a class link from one class to
                  another
  INPUTS       : 1) The packed links in which to
                    insert the new class
                 2) The subclass pointer
                 3) Index of where to place the
                    class (-1 to append)
  RETURNS      : Nothing useful
  SIDE EFFECTS : Link created and attached
  NOTES        : Assumes the pack structure belongs
                 to a class and does not need to
                 be deallocated
 ***************************************************/
globle void AddClassLink(
  PACKED_CLASS_LINKS *src,
  DEFCLASS *cls,
  int posn)
  {
   PACKED_CLASS_LINKS dst;

   dst.classArray = (DEFCLASS **) gm2((int) (sizeof(DEFCLASS *) * (src->classCount + 1)));

   if (posn == -1)
     {
      GenCopyMemory(DEFCLASS *,src->classCount,dst.classArray,src->classArray);
      dst.classArray[src->classCount] = cls;
     }
   else
     {
      if (posn != 0)
        GenCopyMemory(DEFCLASS *,posn,dst.classArray,src->classArray);
      GenCopyMemory(DEFCLASS *,src->classCount - posn,
                 dst.classArray + posn + 1,src->classArray + posn);
      dst.classArray[posn] = cls;
     }
   dst.classCount = (unsigned short) (src->classCount + 1);
   DeletePackedClassLinks(src,FALSE);
   src->classCount = dst.classCount;
   src->classArray = dst.classArray;
  }

/***************************************************
  NAME         : DeleteSubclassLink
  DESCRIPTION  : Removes a class from another
                 class's subclass list
  INPUTS       : 1) The superclass whose subclass
                    list is to be modified
                 2) The subclass to be removed
  RETURNS      : Nothing useful
  SIDE EFFECTS : The subclass list is changed
  NOTES        : None
 ***************************************************/
globle void DeleteSubclassLink(
  DEFCLASS *sclass,
  DEFCLASS *cls)
  {
   register unsigned deletedIndex;
   PACKED_CLASS_LINKS *src,dst;

   src = &sclass->directSubclasses;
   for (deletedIndex = 0 ; deletedIndex < src->classCount ; deletedIndex++)
     if (src->classArray[deletedIndex] == cls)
       break;
   if (deletedIndex == src->classCount)
     return;
   if (src->classCount > 1)
     {
      dst.classArray = (DEFCLASS **) gm2((int) (sizeof(DEFCLASS *) * (src->classCount - 1)));
      if (deletedIndex != 0)
        GenCopyMemory(DEFCLASS *,deletedIndex,dst.classArray,src->classArray);
      GenCopyMemory(DEFCLASS *,src->classCount - deletedIndex - 1,
                 dst.classArray + deletedIndex,src->classArray + deletedIndex + 1);
     }
   else
     dst.classArray = NULL;
   dst.classCount = (unsigned short) (src->classCount - 1);
   DeletePackedClassLinks(src,FALSE);
   src->classCount = dst.classCount;
   src->classArray = dst.classArray;
  }

/**************************************************************
  NAME         : NewClass
  DESCRIPTION  : Allocates and initalizes a new class structure
  INPUTS       : The symbolic name of the new class
  RETURNS      : The address of the new class
  SIDE EFFECTS : None
  NOTES        : None
 **************************************************************/
globle DEFCLASS *NewClass(
  SYMBOL_HN *className)
  {
   register DEFCLASS *cls;

   cls = get_struct(defclass);
   InitializeConstructHeader("defclass",(struct constructHeader *) cls,className);

   cls->id = 0;
   cls->installed = 0;
   cls->busy = 0;
   cls->system = 0;
   cls->abstract = 0;
   cls->reactive = 1;
#if DEBUGGING_FUNCTIONS
   cls->traceInstances = WatchInstances;
   cls->traceSlots = WatchSlots;
#endif
   cls->hashTableIndex = 0;
   cls->directSuperclasses.classCount = 0;
   cls->directSuperclasses.classArray = NULL;
   cls->directSubclasses.classCount = 0;
   cls->directSubclasses.classArray = NULL;
   cls->allSuperclasses.classCount = 0;
   cls->allSuperclasses.classArray = NULL;
   cls->slots = NULL;
   cls->instanceTemplate = NULL;
   cls->slotNameMap = NULL;
   cls->instanceSlotCount = 0;
   cls->localInstanceSlotCount = 0;
   cls->slotCount = 0;
   cls->maxSlotNameID = 0;
   cls->handlers = NULL;
   cls->handlerOrderMap = NULL;
   cls->handlerCount = 0;
   cls->instanceList = NULL;
   cls->instanceListBottom = NULL;
   cls->nxtHash = NULL;
   cls->scopeMap = NULL;
   ClearBitString(cls->traversalRecord,TRAVERSAL_BYTES);
   return(cls);
  }

/***************************************************
  NAME         : DeletePackedClassLinks
  DESCRIPTION  : Dealloacates a contiguous array
                 holding class links
  INPUTS       : 1) The class link segment
                 2) A flag indicating whether to
                    delete the top pack structure
  RETURNS      : Nothing useful
  SIDE EFFECTS : Class links deallocated
  NOTES        : None
 ***************************************************/
globle void DeletePackedClassLinks(
  PACKED_CLASS_LINKS *plp,
  int deleteTop)
  {
   if (plp->classCount > 0)
     {
      rm((void *) plp->classArray,(int) (sizeof(DEFCLASS *) * plp->classCount));
      plp->classCount = 0;
      plp->classArray = NULL;
     }
   if (deleteTop)
     rtn_struct(packedClassLinks,plp);
  }

/***************************************************
  NAME         : AssignClassID
  DESCRIPTION  : Assigns a unique id to a class
                 and puts its address in the
                 id map
  INPUTS       : The class
  RETURNS      : Nothing useful
  SIDE EFFECTS : Class id assigned and map set
  NOTES        : None
 ***************************************************/
globle void AssignClassID(
  DEFCLASS *cls)
  {
   register unsigned i;

   if ((MaxClassID % CLASS_ID_MAP_CHUNK) == 0)
     {
      ClassIDMap = (DEFCLASS **) genrealloc((void *) ClassIDMap,
                       (unsigned) (MaxClassID * sizeof(DEFCLASS *)),
                       (unsigned) ((MaxClassID + CLASS_ID_MAP_CHUNK) * sizeof(DEFCLASS *)));
      for (i = MaxClassID ; i < (MaxClassID + CLASS_ID_MAP_CHUNK) ; i++)
        ClassIDMap[i] = NULL;
     }
   ClassIDMap[MaxClassID] = cls;
   cls->id = MaxClassID++;
  }

/*********************************************************
  NAME         : AddSlotName
  DESCRIPTION  : Adds a new slot entry (or increments
                 the use count of an existing node).
  INPUTS       : 1) The slot name
                 2) The new canonical id for the slot name
                 3) A flag indicating whether the
                    given id must be used or not
  RETURNS      : The id of the (old) node
  SIDE EFFECTS : Slot name entry added or use count
                 incremented
  NOTES        : None
 *********************************************************/
globle SLOT_NAME *AddSlotName(
  SYMBOL_HN *slotName,
  unsigned newid,
  int usenewid)
  {
   SLOT_NAME *snp;
   unsigned hashTableIndex;
   char *buf;
   int bufsz;

   hashTableIndex = HashSlotName(slotName);
   snp = SlotNameTable[hashTableIndex];
   while ((snp != NULL) ? (snp->name != slotName) : FALSE)
     snp = snp->nxt;
   if (snp != NULL)
     {
      if (usenewid && (newid != snp->id))
        {
         SystemError("CLASSFUN",1);
         ExitRouter(EXIT_FAILURE);
        }
      snp->use++;
     }
   else
     {
      snp = get_struct(slotName);
      snp->name = slotName;
      snp->hashTableIndex = hashTableIndex;
      snp->use = 1;
      snp->id = usenewid ? newid : NewSlotNameID();
      snp->nxt = SlotNameTable[hashTableIndex];
      SlotNameTable[hashTableIndex] = snp;
      IncrementSymbolCount(slotName);
      bufsz = (int) (sizeof(char) *
                     (PUT_PREFIX_LENGTH + strlen(ValueToString(slotName)) + 1));
      buf = (char *) gm2(bufsz);
      strcpy(buf,PUT_PREFIX);
      strcat(buf,ValueToString(slotName));
      snp->putHandlerName = (SYMBOL_HN *) AddSymbol(buf);
      IncrementSymbolCount(snp->putHandlerName);
      rm((void *) buf,bufsz);
      snp->bsaveIndex = 0L;
    }
   return(snp);
  }

/***************************************************
  NAME         : DeleteSlotName
  DESCRIPTION  : Removes a slot name entry from
                 the table of all slot names if
                 no longer in use
  INPUTS       : The slot name hash node
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot name entry deleted or use
                 count decremented
  NOTES        : None
 ***************************************************/
globle void DeleteSlotName(
  SLOT_NAME *slotName)
  {
   SLOT_NAME *snp,*prv;

   if (slotName == NULL)
     return;
   prv = NULL;
   snp = SlotNameTable[slotName->hashTableIndex];
   while (snp != slotName)
     {
      prv = snp;
      snp = snp->nxt;
     }
   snp->use--;
   if (snp->use != 0)
     return;
   if (prv == NULL)
     SlotNameTable[snp->hashTableIndex] = snp->nxt;
   else
     prv->nxt = snp->nxt;
   DecrementSymbolCount(snp->name);
   DecrementSymbolCount(snp->putHandlerName);
   rtn_struct(slotName,snp);
  }

/*******************************************************************
  NAME         : RemoveDefclass
  DESCRIPTION  : Deallocates a class structure and
                 all its fields - returns all symbols
                 in use by the class back to the symbol
                 manager for ephemeral removal
  INPUTS       : The address of the class
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : Assumes class has no subclasses
                 IMPORTANT WARNING!! : Assumes class
                   busy count and all instances' busy
                   counts are 0 and all handlers' busy counts are 0!
 *******************************************************************/
LOCALE void RemoveDefclass(
  void *vcls)
  {
   DEFCLASS *cls = (DEFCLASS *) vcls;
   HANDLER *hnd;
   register int i;

   /* ====================================================
      Remove all of this class's superclasses' links to it
      ==================================================== */
   for (i = 0 ; i < cls->directSuperclasses.classCount ; i++)
     DeleteSubclassLink(cls->directSuperclasses.classArray[i],cls);

   RemoveClassFromTable(cls);

   InstallClass(cls,FALSE);

   DeletePackedClassLinks(&cls->directSuperclasses,FALSE);
   DeletePackedClassLinks(&cls->allSuperclasses,FALSE);
   DeletePackedClassLinks(&cls->directSubclasses,FALSE);

   for (i = 0 ; i < cls->slotCount ; i++)
     {
      if (cls->slots[i].defaultValue != NULL)
        {
         if (cls->slots[i].dynamicDefault)
           ReturnPackedExpression((EXPRESSION *) cls->slots[i].defaultValue);
         else
           rtn_struct(dataObject,cls->slots[i].defaultValue);
        }
      DeleteSlotName(cls->slots[i].slotName);
      RemoveConstraint(cls->slots[i].constraint);
     }

   if (cls->instanceSlotCount != 0)
     {
      rm((void *) cls->instanceTemplate,
         (int) (sizeof(SLOT_DESC *) * cls->instanceSlotCount));
      rm((void *) cls->slotNameMap,
         (int) (sizeof(unsigned) * (cls->maxSlotNameID + 1)));
     }
   if (cls->slotCount != 0)
     rm((void *) cls->slots,(int) (sizeof(SLOT_DESC) * cls->slotCount));

   for (i = 0 ; i < cls->handlerCount ; i++)
     {
      hnd = &cls->handlers[i];
      if (hnd->actions != NULL)
        ReturnPackedExpression(hnd->actions);
      if (hnd->ppForm != NULL)
        rm((void *) hnd->ppForm,(int) (sizeof(char) * (strlen(hnd->ppForm)+1)));
     }
   if (cls->handlerCount != 0)
     {
      rm((void *) cls->handlers,(int) (sizeof(HANDLER) * cls->handlerCount));
      rm((void *) cls->handlerOrderMap,(int) (sizeof(unsigned) * cls->handlerCount));
     }

   SetDefclassPPForm((void *) cls,NULL);
   DeassignClassID((unsigned) cls->id);
   rtn_struct(defclass,cls);
  }

/***************************************************
  NAME         : InstallClass
  DESCRIPTION  : In(De)crements all symbol counts for
                 for symbols in use by class
                 Disallows (allows) symbols to become
                 ephemeral.
  INPUTS       : 1) The class address
                 2) 1 - install, 0 - deinstall
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle void InstallClass(
  DEFCLASS *cls,
  int set)
  {
   SLOT_DESC *slot;
   HANDLER *hnd;
   register int i;

   if ((set && cls->installed) ||
       ((set == FALSE) && (cls->installed == 0)))
     return;

   /* ==================================================================
      Handler installation is handled when message-handlers are defined:
      see ParseDefmessageHandler() in MSGCOM.C

      Slot installation is handled by ParseSlot() in CLASSPSR.C
      Scope map installation is handled by CreateClassScopeMap()
      ================================================================== */
   if (set == FALSE)
     {
      cls->installed = 0;

      DecrementSymbolCount(cls->header.name);

#if DEFMODULE_CONSTRUCT
      DecrementBitMapCount(cls->scopeMap);
#endif
      ClearUserDataList(cls->header.usrData);
      
      for (i = 0 ; i < cls->slotCount ; i++)
        {
         slot = &cls->slots[i];
         DecrementSymbolCount(slot->overrideMessage);
         if (slot->defaultValue != NULL)
           {
            if (slot->dynamicDefault)
              ExpressionDeinstall((EXPRESSION *) slot->defaultValue);
            else
              ValueDeinstall((DATA_OBJECT *) slot->defaultValue);
           }
        }
      for (i = 0 ; i < cls->handlerCount ; i++)
        {
         hnd = &cls->handlers[i];
         DecrementSymbolCount(hnd->name);
         if (hnd->actions != NULL)
           ExpressionDeinstall(hnd->actions);
        }
     }
   else
     {
      cls->installed = 1;
      IncrementSymbolCount(cls->header.name);
     }
  }

#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)

/***************************************************
  NAME         : IsClassBeingUsed
  DESCRIPTION  : Checks the busy flag of a class
                   and ALL classes that inherit from
                   it to make sure that it is not
                   in use before deletion
  INPUTS       : The class
  RETURNS      : TRUE if in use, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : Recursively examines all subclasses
 ***************************************************/
globle int IsClassBeingUsed(
  DEFCLASS *cls)
  {
   register unsigned i;

   if (cls->busy > 0)
     return(TRUE);
   for (i = 0 ; i < cls->directSubclasses.classCount ; i++)
     if (IsClassBeingUsed(cls->directSubclasses.classArray[i]))
       return(TRUE);
   return(FALSE);
  }

/***************************************************
  NAME         : RemoveAllUserClasses
  DESCRIPTION  : Removes all classes
  INPUTS       : None
  RETURNS      : TRUE if succesful, FALSE otherwise
  SIDE EFFECTS : The class hash table is cleared
  NOTES        : None
 ***************************************************/
globle int RemoveAllUserClasses()
  {
   void *userClasses,*ctmp;
   int success = TRUE;

#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded())
     return(FALSE);
#endif
   /* ====================================================
      Don't delete built-in system classes at head of list
      ==================================================== */
   userClasses = GetNextDefclass(NULL);
   while (userClasses != NULL)
     {
      if (((DEFCLASS *) userClasses)->system == 0)
        break;
      userClasses = GetNextDefclass(userClasses);
     }
   while (userClasses != NULL)
     {
      ctmp = userClasses;
      userClasses = GetNextDefclass(userClasses);
      if (IsDefclassDeletable(ctmp))
        {
         RemoveConstructFromModule((struct constructHeader *) ctmp);
         RemoveDefclass(ctmp);
        }
      else
        {
         success = FALSE;
         CantDeleteItemErrorMessage("defclass",GetDefclassName(ctmp));
        }
     }
   return(success);
  }

/****************************************************
  NAME         : DeleteClassUAG
  DESCRIPTION  : Deallocates a class and all its
                 subclasses
  INPUTS       : The address of the class
  RETURNS      : 1 if successful, 0 otherwise
  SIDE EFFECTS : Removes the class from each of
                 its superclasses' subclass lists
  NOTES        : None
 ****************************************************/
globle int DeleteClassUAG(
  DEFCLASS *cls)
  {
   unsigned subCount;

   while (cls->directSubclasses.classCount != 0)
     {
      subCount = cls->directSubclasses.classCount;
      DeleteClassUAG(cls->directSubclasses.classArray[0]);
      if (cls->directSubclasses.classCount == subCount)
        return(FALSE);
     }
   if (IsDefclassDeletable((void *) cls))
     {
      RemoveConstructFromModule((struct constructHeader *) cls);
      RemoveDefclass((void *) cls);
      return(TRUE);
     }
   return(FALSE);
  }

/*********************************************************
  NAME         : MarkBitMapSubclasses
  DESCRIPTION  : Recursively marks the ids of a class
                 and all its subclasses in a bitmap
  INPUTS       : 1) The bitmap
                 2) The class
                 3) A code indicating whether to set
                    or clear the bits of the map
                    corresponding to the class ids
  RETURNS      : Nothing useful
  SIDE EFFECTS : BitMap marked
  NOTES        : IMPORTANT!!!!  Assumes the bitmap is
                 large enough to hold all ids encountered!
 *********************************************************/
globle void MarkBitMapSubclasses(
  char *map,
  DEFCLASS *cls,
  int set)
  {
   register unsigned i;

   if (set)
     SetBitMap(map,cls->id);
   else
     ClearBitMap(map,cls->id);
   for (i = 0 ; i < cls->directSubclasses.classCount ; i++)
     MarkBitMapSubclasses(map,cls->directSubclasses.classArray[i],set);
  }

#endif

/***************************************************
  NAME         : FindSlotNameID
  DESCRIPTION  : Finds the id of a slot name
  INPUTS       : The slot name
  RETURNS      : The slot name id (-1 if not found)
  SIDE EFFECTS : None
  NOTES        : A slot name always has the same
                 id regardless of what class uses
                 it.  In this way, a slot can
                 be referred to by index independent
                 of class.  Each class stores a
                 map showing which slot name indices
                 go to which slot.  This provides
                 for immediate lookup of slots
                 given the index (object pattern
                 matching uses this).
 ***************************************************/
globle int FindSlotNameID(
  SYMBOL_HN *slotName)
  {
   SLOT_NAME *snp;

   snp = SlotNameTable[HashSlotName(slotName)];
   while ((snp != NULL) ? (snp->name != slotName) : FALSE)
     snp = snp->nxt;
   return((snp != NULL) ? (int) snp->id : -1);
  }

/***************************************************
  NAME         : FindIDSlotName
  DESCRIPTION  : Finds the slot anme for an id
  INPUTS       : The id
  RETURNS      : The slot name (NULL if not found)
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle SYMBOL_HN *FindIDSlotName(
  unsigned id)
  {
   SLOT_NAME *snp;

   snp = FindIDSlotNameHash(id);
   return((snp != NULL) ? snp->name : NULL);
  }

/***************************************************
  NAME         : FindIDSlotNameHash
  DESCRIPTION  : Finds the slot anme for an id
  INPUTS       : The id
  RETURNS      : The slot name (NULL if not found)
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle SLOT_NAME *FindIDSlotNameHash(
  unsigned id)
  {
   register int i;
   SLOT_NAME *snp;

   for (i = 0 ; i < SLOT_NAME_TABLE_HASH_SIZE ; i++)
     {
      snp = SlotNameTable[i];
      while (snp != NULL)
        {
         if (snp->id == id)
           return(snp);
         snp = snp->nxt;
        }
     }
   return(NULL);
  }

/***************************************************
  NAME         : GetTraversalID
  DESCRIPTION  : Returns a unique integer ID for a
                  traversal into the class hierarchy
  INPUTS       : None
  RETURNS      : The id, or -1 if none available
  SIDE EFFECTS : EvaluationError set when no ids
                   available
  NOTES        : Used for recursive traversals of
                  class hierarchy to assure that a
                  class is only visited once
 ***************************************************/
globle int GetTraversalID()
  {
   register unsigned i;
   register DEFCLASS *cls;

   if (CTID >= MAX_TRAVERSALS)
     {
      PrintErrorID("CLASSFUN",2,FALSE);
      PrintRouter(WERROR,"Maximum number of simultaneous class hierarchy\n  traversals exceeded ");
      PrintLongInteger(WERROR,(long) MAX_TRAVERSALS);
      PrintRouter(WERROR,".\n");
      SetEvaluationError(TRUE);
      return(-1);
     }

   for (i = 0 ; i < CLASS_TABLE_HASH_SIZE ; i++)
     for (cls = ClassTable[i] ; cls != NULL ; cls = cls->nxtHash)
       ClearTraversalID(cls->traversalRecord,CTID);
   return(CTID++);
  }

/***************************************************
  NAME         : ReleaseTraversalID
  DESCRIPTION  : Releases an ID for future use
                 Also clears id from all classes
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Old ID released for later reuse
  NOTES        : Releases ID returned by most recent
                   call to GetTraversalID()
 ***************************************************/
globle void ReleaseTraversalID()
  {
   CTID--;
  }

/*******************************************************
  NAME         : HashClass
  DESCRIPTION  : Generates a hash index for a given
                 class name
  INPUTS       : The address of the class name SYMBOL_HN
  RETURNS      : The hash index value
  SIDE EFFECTS : None
  NOTES        : Counts on the fact that the symbol
                 has already been hashed into the
                 symbol table - uses that hash value
                 multiplied by a prime for a new hash
 *******************************************************/
globle unsigned HashClass(
  SYMBOL_HN *cname)
  {
   unsigned long tally;

   tally = ((unsigned long) cname->bucket) * BIG_PRIME;
   return((unsigned) (tally % CLASS_TABLE_HASH_SIZE));
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/*******************************************************
  NAME         : HashSlotName
  DESCRIPTION  : Generates a hash index for a given
                 slot name
  INPUTS       : The address of the slot name SYMBOL_HN
  RETURNS      : The hash index value
  SIDE EFFECTS : None
  NOTES        : Counts on the fact that the symbol
                 has already been hashed into the
                 symbol table - uses that hash value
                 multiplied by a prime for a new hash
 *******************************************************/
static unsigned HashSlotName(
  SYMBOL_HN *sname)
  {
   unsigned long tally;

   tally = ((unsigned long) sname->bucket) * BIG_PRIME;
   return((unsigned) (tally % SLOT_NAME_TABLE_HASH_SIZE));
  }

#if (! RUN_TIME)

/***********************************************
  NAME         : NewSlotNameID
  DESCRIPTION  : Returns  an unused slot name id
                 as close to 1 as possible
  INPUTS       : None
  RETURNS      : The new unused id
  SIDE EFFECTS : None
  NOTES        : None
 ***********************************************/
static unsigned NewSlotNameID()
  {
   unsigned newid = 0;
   register unsigned i;
   SLOT_NAME *snp;

   while (TRUE)
     {
      for (i = 0 ; i < SLOT_NAME_TABLE_HASH_SIZE ; i++)
        {
         snp = SlotNameTable[i];
         while ((snp != NULL) ? (snp->id != newid) : FALSE)
           snp = snp->nxt;
         if (snp != NULL)
           break;
        }
      if (i < SLOT_NAME_TABLE_HASH_SIZE)
        newid++;
      else
        break;
     }
   return(newid);
  }

/***************************************************
  NAME         : DeassignClassID
  DESCRIPTION  : Reduces id map and MaxClassID if
                 no ids in use above the one being
                 released.
  INPUTS       : The id
  RETURNS      : Nothing useful
  SIDE EFFECTS : ID map and MaxClassID possibly
                 reduced
  NOTES        : None
 ***************************************************/
static void DeassignClassID(
  unsigned id)
  {
   register unsigned i;
   int reallocReqd;
   unsigned oldChunk,newChunk;

   ClassIDMap[id] = NULL;
   for (i = id + 1 ; i < MaxClassID ; i++)
     if (ClassIDMap[i] != NULL)
       return;
   reallocReqd = FALSE;
   while (ClassIDMap[id] == NULL)
     {
      MaxClassID = (unsigned short) id;
      if ((MaxClassID % CLASS_ID_MAP_CHUNK) == 0)
        {
         newChunk = MaxClassID;
         if (reallocReqd == FALSE)
           {
            oldChunk = MaxClassID + CLASS_ID_MAP_CHUNK;
            reallocReqd = TRUE;
           }
        }
      if (id == 0)
        break;
      id--;
     }
   if (reallocReqd)
     ClassIDMap = (DEFCLASS **) genrealloc((void *) ClassIDMap,
                      (unsigned) (oldChunk * sizeof(DEFCLASS *)),
                      (unsigned) (newChunk * sizeof(DEFCLASS *)));
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



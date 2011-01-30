   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/13/98          */
   /*                                                     */
   /*        CLASS INFO PROGRAMMATIC ACCESS MODULE        */
   /*******************************************************/

/**************************************************************/
/* Purpose: Class Information Interface Support Routines      */
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

#include <string.h>

#include "argacces.h"
#include "classcom.h"
#include "classexm.h"
#include "classfun.h"
#include "classini.h"
#include "memalloc.h"
#include "insfun.h"
#include "msgfun.h"
#include "multifld.h"
#include "prntutil.h"

#define _CLASSINF_SOURCE_
#include "classinf.h"

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

static void SlotInfoSupportFunction(DATA_OBJECT *,char *,void (*)(void *,char *,DATA_OBJECT *));
static int CountSubclasses(DEFCLASS *,int,int);
static int StoreSubclasses(void *,int,DEFCLASS *,int,int,short);
static SLOT_DESC *SlotInfoSlot(DATA_OBJECT *,DEFCLASS *,char *,char *);

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

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/*********************************************************************
  NAME         : ClassAbstractPCommand
  DESCRIPTION  : Determines if direct instances of a class can be made
  INPUTS       : None
  RETURNS      : TRUE (1) if class is abstract, FALSE (0) if concrete
  SIDE EFFECTS : None
  NOTES        : Syntax: (class-abstractp <class>)
 *********************************************************************/
globle int ClassAbstractPCommand()
  {
   DATA_OBJECT tmp;
   DEFCLASS *cls;

   if (ArgTypeCheck("class-abstractp",1,SYMBOL,&tmp) == FALSE)
     return(FALSE);
   cls = LookupDefclassByMdlOrScope(DOToString(tmp));
   if (cls == NULL)
     {
      ClassExistError("class-abstractp",ValueToString(tmp.value));
      return(FALSE);
     }
   return(ClassAbstractP((void *) cls));
  }

#if INSTANCE_PATTERN_MATCHING

/*****************************************************************
  NAME         : ClassReactivePCommand
  DESCRIPTION  : Determines if instances of a class can match rule
                 patterns
  INPUTS       : None
  RETURNS      : TRUE (1) if class is reactive, FALSE (0)
                 if non-reactive
  SIDE EFFECTS : None
  NOTES        : Syntax: (class-reactivep <class>)
 *****************************************************************/
globle int ClassReactivePCommand()
  {
   DATA_OBJECT tmp;
   DEFCLASS *cls;

   if (ArgTypeCheck("class-reactivep",1,SYMBOL,&tmp) == FALSE)
     return(FALSE);
   cls = LookupDefclassByMdlOrScope(DOToString(tmp));
   if (cls == NULL)
     {
      ClassExistError("class-reactivep",ValueToString(tmp.value));
      return(FALSE);
     }
   return(ClassReactiveP((void *) cls));
  }

#endif

/***********************************************************
  NAME         : ClassInfoFnxArgs
  DESCRIPTION  : Examines arguments for:
                   class-slots, get-defmessage-handler-list,
                   class-superclasses and class-subclasses
  INPUTS       : 1) Name of function
                 2) A buffer to hold a flag indicating if
                    the inherit keyword was specified
  RETURNS      : Pointer to the class on success,
                   NULL on errors
  SIDE EFFECTS : inhp flag set
                 error flag set
  NOTES        : None
 ***********************************************************/
globle void *ClassInfoFnxArgs(
  char *fnx,
  int *inhp)
  {
   void *clsptr;
   DATA_OBJECT tmp;

   *inhp = 0;
   if (RtnArgCount() == 0)
     {
      ExpectedCountError(fnx,AT_LEAST,1);
      SetEvaluationError(TRUE);
      return(NULL);
     }
   if (ArgTypeCheck(fnx,1,SYMBOL,&tmp) == FALSE)
     return(NULL);
   clsptr = (void *) LookupDefclassByMdlOrScope(DOToString(tmp));
   if (clsptr == NULL)
     {
      ClassExistError(fnx,ValueToString(tmp.value));
      return(NULL);
     }
   if (RtnArgCount() == 2)
     {
      if (ArgTypeCheck(fnx,2,SYMBOL,&tmp) == FALSE)
        return(NULL);
      if (strcmp(ValueToString(tmp.value),"inherit") == 0)
        *inhp = 1;
      else
        {
         SyntaxErrorMessage(fnx);
         SetEvaluationError(TRUE);
         return(NULL);
        }
     }
   return(clsptr);
  }

/********************************************************************
  NAME         : ClassSlotsCommand
  DESCRIPTION  : Groups slot info for a class into a multifield value
                   for dynamic perusal
  INPUTS       : Data object buffer to hold the slots of the class
  RETURNS      : Nothing useful
  SIDE EFFECTS : Creates a multifield storing the names of
                    the slots of the class
  NOTES        : Syntax: (class-slots <class> [inherit])
 ********************************************************************/
globle void ClassSlotsCommand(
  DATA_OBJECT *result)
  {
   int inhp;
   void *clsptr;

   clsptr = ClassInfoFnxArgs("class-slots",&inhp);
   if (clsptr == NULL)
     {
      SetMultifieldErrorValue(result);
      return;
     }
   ClassSlots(clsptr,result,inhp);
  }

/************************************************************************
  NAME         : ClassSuperclassesCommand
  DESCRIPTION  : Groups superclasses for a class into a multifield value
                   for dynamic perusal
  INPUTS       : Data object buffer to hold the superclasses of the class
  RETURNS      : Nothing useful
  SIDE EFFECTS : Creates a multifield storing the names of
                    the superclasses of the class
  NOTES        : Syntax: (class-superclasses <class> [inherit])
 ************************************************************************/
globle void ClassSuperclassesCommand(
  DATA_OBJECT *result)
  {
   int inhp;
   void *clsptr;

   clsptr = ClassInfoFnxArgs("class-superclasses",&inhp);
   if (clsptr == NULL)
     {
      SetMultifieldErrorValue(result);
      return;
     }
   ClassSuperclasses(clsptr,result,inhp);
  }

/************************************************************************
  NAME         : ClassSubclassesCommand
  DESCRIPTION  : Groups subclasses for a class into a multifield value
                   for dynamic perusal
  INPUTS       : Data object buffer to hold the subclasses of the class
  RETURNS      : Nothing useful
  SIDE EFFECTS : Creates a multifield storing the names of
                    the subclasses of the class
  NOTES        : Syntax: (class-subclasses <class> [inherit])
 ************************************************************************/
globle void ClassSubclassesCommand(
  DATA_OBJECT *result)
  {
   int inhp;
   void *clsptr;

   clsptr = ClassInfoFnxArgs("class-subclasses",&inhp);
   if (clsptr == NULL)
     {
      SetMultifieldErrorValue(result);
      return;
     }
   ClassSubclasses(clsptr,result,inhp);
  }

/***********************************************************************
  NAME         : GetDefmessageHandlersListCmd
  DESCRIPTION  : Groups message-handlers for a class into a multifield
                   value for dynamic perusal
  INPUTS       : Data object buffer to hold the handlers of the class
  RETURNS      : Nothing useful
  SIDE EFFECTS : Creates a multifield storing the names of
                    the message-handlers of the class
  NOTES        : Syntax: (get-defmessage-handler-list <class> [inherit])
 ***********************************************************************/
globle void GetDefmessageHandlersListCmd(
  DATA_OBJECT *result)
  {
   int inhp;
   void *clsptr;

   if (RtnArgCount () == 0)
      GetDefmessageHandlerList(NULL,result,0);
   else
     {
      clsptr = ClassInfoFnxArgs("get-defmessage-handler-list",&inhp);
      if (clsptr == NULL)
        {
         SetMultifieldErrorValue(result);
         return;
        }
      GetDefmessageHandlerList(clsptr,result,inhp);
     }
  }

/*********************************
 Slot Information Access Functions
 *********************************/
globle void SlotFacetsCommand(
  DATA_OBJECT *result)
  {
   SlotInfoSupportFunction(result,"slot-facets",SlotFacets);
  }

globle void SlotSourcesCommand(
  DATA_OBJECT *result)
  {
   SlotInfoSupportFunction(result,"slot-sources",SlotSources);
  }

globle void SlotTypesCommand(
  DATA_OBJECT *result)
  {
   SlotInfoSupportFunction(result,"slot-types",SlotTypes);
  }

globle void SlotAllowedValuesCommand(
  DATA_OBJECT *result)
  {
   SlotInfoSupportFunction(result,"slot-allowed-values",SlotAllowedValues);
  }

globle void SlotRangeCommand(
  DATA_OBJECT *result)
  {
   SlotInfoSupportFunction(result,"slot-range",SlotRange);
  }

globle void SlotCardinalityCommand(
  DATA_OBJECT *result)
  {
   SlotInfoSupportFunction(result,"slot-cardinality",SlotCardinality);
  }

/********************************************************************
  NAME         : ClassAbstractP
  DESCRIPTION  : Determines if a class is abstract or not
  INPUTS       : Generic pointer to class
  RETURNS      : 1 if class is abstract, 0 otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ********************************************************************/
globle BOOLEAN ClassAbstractP(
  void *clsptr)
  {
   return(((DEFCLASS *) clsptr)->abstract);
  }

#if INSTANCE_PATTERN_MATCHING

/********************************************************************
  NAME         : ClassReactiveP
  DESCRIPTION  : Determines if a class is reactive or not
  INPUTS       : Generic pointer to class
  RETURNS      : 1 if class is reactive, 0 otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ********************************************************************/
globle BOOLEAN ClassReactiveP(
  void *clsptr)
  {
   return(((DEFCLASS *) clsptr)->reactive);
  }

#endif

/********************************************************************
  NAME         : ClassSlots
  DESCRIPTION  : Groups slot info for a class into a multifield value
                   for dynamic perusal
  INPUTS       : 1) Generic pointer to class
                 2) Data object buffer to hold the slots of the class
                 3) Include (1) or exclude (0) inherited slots
  RETURNS      : Nothing useful
  SIDE EFFECTS : Creates a multifield storing the names of
                    the slots of the class
  NOTES        : None
 ********************************************************************/
globle void ClassSlots(
  void *clsptr,
  DATA_OBJECT *result,
  int inhp)
  {
   long size; /* 6.04 Bug Fix */
   register DEFCLASS *cls;
   register long i; /* 6.04 Bug Fix */

   cls = (DEFCLASS *) clsptr;
   size = inhp ? cls->instanceSlotCount : cls->slotCount;
   result->type = MULTIFIELD;
   result->begin = 0;
   result->end = size - 1;
   result->value = (void *) CreateMultifield(size);
   if (size == 0)
     return;
   if (inhp)
     {
      for (i = 0 ; i < cls->instanceSlotCount ; i++)
        {
         SetMFType(result->value,i+1,SYMBOL);
         SetMFValue(result->value,i+1,cls->instanceTemplate[i]->slotName->name);
        }
     }
   else
     {
      for (i = 0 ; i < cls->slotCount ; i++)
        {
         SetMFType(result->value,i+1,SYMBOL);
         SetMFValue(result->value,i+1,cls->slots[i].slotName->name);
        }
     }
  }

/************************************************************************
  NAME         : GetDefmessageHandlerList
  DESCRIPTION  : Groups handler info for a class into a multifield value
                   for dynamic perusal
  INPUTS       : 1) Generic pointer to class (NULL to get handlers for
                    all classes)
                 2) Data object buffer to hold the handlers of the class
                 3) Include (1) or exclude (0) inherited handlers
  RETURNS      : Nothing useful
  SIDE EFFECTS : Creates a multifield storing the names and types of
                    the message-handlers of the class
  NOTES        : None
 ************************************************************************/
globle void GetDefmessageHandlerList(
  void *clsptr,
  DATA_OBJECT *result,
  int inhp)
  {
   DEFCLASS *cls,*svcls,*svnxt,*supcls;
   register int j,classi,classiLimit;
   long i,len,sublen;

   if (clsptr == NULL)
     {
      inhp = 0;
      cls = (DEFCLASS *) GetNextDefclass(NULL);
      svnxt = (DEFCLASS *) GetNextDefclass((void *) cls);
     }
   else
     {
      cls = (DEFCLASS *) clsptr;
      svnxt = (DEFCLASS *) GetNextDefclass((void *) cls);
      SetNextDefclass((void *) cls,NULL);
     }
   for (svcls = cls , i = 0 ;
        cls != NULL ;
        cls = (DEFCLASS *) GetNextDefclass((void *) cls))
     {
      classiLimit = inhp ? cls->allSuperclasses.classCount : 1;
      for (classi = 0 ; classi < classiLimit ; classi++)
        i += cls->allSuperclasses.classArray[classi]->handlerCount;
     }
   len = i * 3;
   result->type = MULTIFIELD;
   result->begin = 0;
   result->end = len - 1;
   result->value = (void *) CreateMultifield(len);
   for (cls = svcls , sublen = 0 ;
        cls != NULL ;
        cls = (DEFCLASS *) GetNextDefclass((void *) cls))
     {
      classiLimit = inhp ? cls->allSuperclasses.classCount : 1;
      for (classi = 0 ; classi < classiLimit ; classi++)
        {
         supcls = cls->allSuperclasses.classArray[classi];
         if (inhp == 0)
           i = sublen + 1;
         else
           i = len - (supcls->handlerCount * 3) - sublen + 1;
         for (j = 0 ; j < supcls->handlerCount ; j++)
           {
            SetMFType(result->value,i,SYMBOL);
            SetMFValue(result->value,i++,GetDefclassNamePointer((void *) supcls));
            SetMFType(result->value,i,SYMBOL);
            SetMFValue(result->value,i++,supcls->handlers[j].name);
            SetMFType(result->value,i,SYMBOL);
            SetMFValue(result->value,i++,AddSymbol(hndquals[supcls->handlers[j].type]));
           }
         sublen += supcls->handlerCount * 3;
        }
     }
   if (svcls != NULL)
     SetNextDefclass((void *) svcls,(void *) svnxt);
  }

/***************************************************************************
  NAME         : ClassSuperclasses
  DESCRIPTION  : Groups the names of superclasses into a multifield
                   value for dynamic perusal
  INPUTS       : 1) Generic pointer to class
                 2) Data object buffer to hold the superclasses of the class
                 3) Include (1) or exclude (0) indirect superclasses
  RETURNS      : Nothing useful
  SIDE EFFECTS : Creates a multifield storing the names of
                    the superclasses of the class
  NOTES        : None
 ***************************************************************************/
globle void ClassSuperclasses(
  void *clsptr,
  DATA_OBJECT *result,
  int inhp)
  {
   PACKED_CLASS_LINKS *plinks;
   int offset;
   register unsigned i,j;

   if (inhp)
     {
      plinks = &((DEFCLASS *) clsptr)->allSuperclasses;
      offset = 1;
     }
   else
     {
      plinks = &((DEFCLASS *) clsptr)->directSuperclasses;
      offset = 0;
     }
   result->type = MULTIFIELD;
   result->begin = 0;
   result->end = plinks->classCount - offset - 1;
   result->value = (void *) CreateMultifield(result->end + 1);
   if (result->end == -1)
     return;
   for (i = offset , j = 1 ; i < plinks->classCount ; i++ , j++)
     {
      SetMFType(result->value,j,SYMBOL);
      SetMFValue(result->value,j,GetDefclassNamePointer((void *) plinks->classArray[i]));
     }
  }

/**************************************************************************
  NAME         : ClassSubclasses
  DESCRIPTION  : Groups the names of subclasses for a class into a
                   multifield value for dynamic perusal
  INPUTS       : 1) Generic pointer to class
                 2) Data object buffer to hold the sublclasses of the class
                 3) Include (1) or exclude (0) indirect subclasses
  RETURNS      : Nothing useful
  SIDE EFFECTS : Creates a multifield storing the names
                    the subclasses of the class
  NOTES        : None
 **************************************************************************/
globle void ClassSubclasses(
  void *clsptr,
  DATA_OBJECT *result,
  int inhp)
  {
   register int i,id;

   if ((id = GetTraversalID()) == -1)
     return;
   i = CountSubclasses((DEFCLASS *) clsptr,inhp,id);
   ReleaseTraversalID();
   result->type = MULTIFIELD;
   result->begin = 0;
   result->end = i - 1;
   result->value = (void *) CreateMultifield(i);
   if (i == 0)
     return;
   if ((id = GetTraversalID()) == -1)
     return;
   StoreSubclasses(result->value,1,(DEFCLASS *) clsptr,inhp,id,TRUE);
   ReleaseTraversalID();
  }

/**************************************************************************
  NAME         : ClassSubclassAddresses
  DESCRIPTION  : Groups the class addresses of subclasses for a class into a
                   multifield value for dynamic perusal
  INPUTS       : 1) Generic pointer to class
                 2) Data object buffer to hold the sublclasses of the class
                 3) Include (1) or exclude (0) indirect subclasses
  RETURNS      : Nothing useful
  SIDE EFFECTS : Creates a multifield storing the subclass
                    addresss of the class
  NOTES        : None
 **************************************************************************/
globle void ClassSubclassAddresses(
  void *clsptr,
  DATA_OBJECT *result,
  int inhp)
  {
   register int i,id;

   if ((id = GetTraversalID()) == -1)
     return;
   i = CountSubclasses((DEFCLASS *) clsptr,inhp,id);
   ReleaseTraversalID();
   result->type = MULTIFIELD;
   result->begin = 0;
   result->end = i - 1;
   result->value = (void *) CreateMultifield(i);
   if (i == 0)
     return;
   if ((id = GetTraversalID()) == -1)
     return;
   StoreSubclasses(result->value,1,(DEFCLASS *) clsptr,inhp,id,FALSE);
   ReleaseTraversalID();
  }
/**************************************************************************
  NAME         : Slot...  Slot information access functions
  DESCRIPTION  : Groups the sources/facets/types/allowed-values/range or
                   cardinality  of a slot for a class into a multifield
                   value for dynamic perusal
  INPUTS       : 1) Generic pointer to class
                 2) Name of the slot
                 3) Data object buffer to hold the attributes of the class
  RETURNS      : Nothing useful
  SIDE EFFECTS : Creates a multifield storing the attributes for the slot
                   of the class
  NOTES        : None
 **************************************************************************/

globle void SlotFacets(
  void *clsptr,
  char *sname,
  DATA_OBJECT *result)
  {
   register int i;
   register SLOT_DESC *sp;

   if ((sp = SlotInfoSlot(result,(DEFCLASS *) clsptr,sname,"slot-facets")) == NULL)
     return;
#if INSTANCE_PATTERN_MATCHING
   result->end = 9;
   result->value = (void *) CreateMultifield(10L);
   for (i = 1 ; i <= 10 ; i++)
     SetMFType(result->value,i,SYMBOL);
#else
   result->end = 8;
   result->value = (void *) CreateMultifield(9L);
   for (i = 1 ; i <= 9 ; i++)
     SetMFType(result->value,i,SYMBOL);
#endif
   SetMFValue(result->value,1,AddSymbol(sp->multiple ? "MLT" : "SGL"));
   if (sp->noDefault)
     SetMFValue(result->value,2,AddSymbol("NIL"));
   else
     SetMFValue(result->value,2,AddSymbol(sp->dynamicDefault ? "DYN" : "STC"));
   SetMFValue(result->value,3,AddSymbol(sp->noInherit ? "NIL" : "INH"));
   if (sp->initializeOnly)
     SetMFValue(result->value,4,AddSymbol("INT"));
   else if (sp->noWrite)
     SetMFValue(result->value,4,AddSymbol("R"));
   else
     SetMFValue(result->value,4,AddSymbol("RW"));
   SetMFValue(result->value,5,AddSymbol(sp->shared ? "SHR" : "LCL"));
#if INSTANCE_PATTERN_MATCHING
   SetMFValue(result->value,6,AddSymbol(sp->reactive ? "RCT" : "NIL"));
   SetMFValue(result->value,7,AddSymbol(sp->composite ? "CMP" : "EXC"));
   SetMFValue(result->value,8,AddSymbol(sp->publicVisibility ? "PUB" : "PRV"));
   SetMFValue(result->value,9,AddSymbol(GetCreateAccessorString((void *) sp)));
   SetMFValue(result->value,10,sp->noWrite ? AddSymbol("NIL") : (void *) sp->overrideMessage);
#else
   SetMFValue(result->value,6,AddSymbol(sp->composite ? "CMP" : "EXC"));
   SetMFValue(result->value,7,AddSymbol(sp->publicVisibility ? "PUB" : "PRV"));
   SetMFValue(result->value,8,AddSymbol(GetCreateAccessorString((void *) sp)));
   SetMFValue(result->value,9,sp->noWrite ? AddSymbol("NIL") : (void *) sp->overrideMessage);
#endif
  }

globle void SlotSources(
  void *clsptr,
  char *sname,
  DATA_OBJECT *result)
  {
   register int i,classi;
   register SLOT_DESC *sp,*csp;
   CLASS_LINK *ctop,*ctmp;
   DEFCLASS *cls;

   if ((sp = SlotInfoSlot(result,(DEFCLASS *) clsptr,sname,"slot-sources")) == NULL)
     return;
   i = 1;
   ctop = get_struct(classLink);
   ctop->cls = sp->cls;
   ctop->nxt = NULL;
   if (sp->composite)
     {
      for (classi = 1 ; classi < sp->cls->allSuperclasses.classCount ; classi++)
        {
         cls = sp->cls->allSuperclasses.classArray[classi];
         csp = FindClassSlot(cls,sp->slotName->name);
         if ((csp != NULL) ? (csp->noInherit == 0) : FALSE)
           {
            ctmp = get_struct(classLink);
            ctmp->cls = cls;
            ctmp->nxt = ctop;
            ctop = ctmp;
            i++;
            if (csp->composite == 0)
              break;
           }
        }
     }
   result->end = i - 1;
   result->value = (void *) CreateMultifield(i);
   for (ctmp = ctop , i = 1 ; ctmp != NULL ; ctmp = ctmp->nxt , i++)
     {
      SetMFType(result->value,i,SYMBOL);
      SetMFValue(result->value,i,GetDefclassNamePointer((void *) ctmp->cls));
     }
   DeleteClassLinks(ctop);
  }

globle void SlotTypes(
  void *clsptr,
  char *sname,
  DATA_OBJECT *result)
  {
   register int i,j;
   register SLOT_DESC *sp;
   char typemap[2];
   int msize;

   if ((sp = SlotInfoSlot(result,(DEFCLASS *) clsptr,sname,"slot-types"))
== NULL)
     return;
   if ((sp->constraint != NULL) ? sp->constraint->anyAllowed : TRUE)
     {
      typemap[0] = typemap[1] = (char) 0xFF;
      ClearBitMap(typemap,MULTIFIELD);
      msize = 8;
     }
   else
     {
      typemap[0] = typemap[1] = (char) 0x00;
      msize = 0;
      if (sp->constraint->symbolsAllowed)
        {
         msize++;
         SetBitMap(typemap,SYMBOL);
        }
      if (sp->constraint->stringsAllowed)
        {
         msize++;
         SetBitMap(typemap,STRING);
        }
      if (sp->constraint->floatsAllowed)
        {
         msize++;
         SetBitMap(typemap,FLOAT);
        }
      if (sp->constraint->integersAllowed)
        {
         msize++;
         SetBitMap(typemap,INTEGER);
        }
      if (sp->constraint->instanceNamesAllowed)
        {
         msize++;
         SetBitMap(typemap,INSTANCE_NAME);
        }
      if (sp->constraint->instanceAddressesAllowed)
        {
         msize++;
         SetBitMap(typemap,INSTANCE_ADDRESS);
        }
      if (sp->constraint->externalAddressesAllowed)
        {
         msize++;
         SetBitMap(typemap,EXTERNAL_ADDRESS);
        }
      if (sp->constraint->factAddressesAllowed)
        {
         msize++;
         SetBitMap(typemap,FACT_ADDRESS);
        }
     }
   result->end = msize - 1;
   result->value = CreateMultifield(msize);
   i = 1;
   j = 0;
   while (i <= msize)
     {
      if (TestBitMap(typemap,j))
       {
        SetMFType(result->value,i,SYMBOL);
        SetMFValue(result->value,i,
                   (void *) GetDefclassNamePointer((void *)
PrimitiveClassMap[j]));
        i++;
       }
      j++;
     }
  }

globle void SlotAllowedValues(
  void *clsptr,
  char *sname,
  DATA_OBJECT *result)
  {
   register int i;
   register SLOT_DESC *sp;
   register EXPRESSION *exp;

   if ((sp = SlotInfoSlot(result,(DEFCLASS *) clsptr,sname,"slot-allowed-values")) == NULL)
     return;
   if ((sp->constraint != NULL) ? (sp->constraint->restrictionList == NULL) : TRUE)
     {
      result->type = SYMBOL;
      result->value = FalseSymbol;
      return;
     }
   result->end = ExpressionSize(sp->constraint->restrictionList) - 1;
   result->value = CreateMultifield(result->end + 1);
   i = 1;
   exp = sp->constraint->restrictionList;
   while (exp != NULL)
     {
      SetMFType(result->value,i,exp->type);
      SetMFValue(result->value,i,exp->value);
      exp = exp->nextArg;
      i++;
     }
  }

globle void SlotRange(
  void *clsptr,
  char *sname,
  DATA_OBJECT *result)
  {
   register SLOT_DESC *sp;

   if ((sp = SlotInfoSlot(result,(DEFCLASS *) clsptr,sname,"slot-range")) == NULL)
     return;
   if ((sp->constraint == NULL) ? FALSE :
       (sp->constraint->anyAllowed || sp->constraint->floatsAllowed ||
        sp->constraint->integersAllowed))
     {
      result->end = 1;
      result->value = CreateMultifield(2L);
      SetMFType(result->value,1,sp->constraint->minValue->type);
      SetMFValue(result->value,1,sp->constraint->minValue->value);
      SetMFType(result->value,2,sp->constraint->maxValue->type);
      SetMFValue(result->value,2,sp->constraint->maxValue->value);
     }
   else
     {
      result->type = SYMBOL;
      result->value = FalseSymbol;
      return;
     }
  }

globle void SlotCardinality(
  void *clsptr,
  char *sname,
  DATA_OBJECT *result)
  {
   register SLOT_DESC *sp;

   if ((sp = SlotInfoSlot(result,(DEFCLASS *) clsptr,sname,"slot-cardinality")) == NULL)
     return;
   if (sp->multiple == 0)
     {
      SetMultifieldErrorValue(result);
      return;
     }
   result->end = 1;
   result->value = CreateMultifield(2L);
   if (sp->constraint != NULL)
     {
      SetMFType(result->value,1,sp->constraint->minFields->type);
      SetMFValue(result->value,1,sp->constraint->minFields->value);
      SetMFType(result->value,2,sp->constraint->maxFields->type);
      SetMFValue(result->value,2,sp->constraint->maxFields->value);
     }
   else
     {
      SetMFType(result->value,1,INTEGER);
      SetMFValue(result->value,1,Zero);
      SetMFType(result->value,2,SYMBOL);
      SetMFValue(result->value,2,PositiveInfinity);
     }
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/*****************************************************
  NAME         : SlotInfoSupportFunction
  DESCRIPTION  : Support routine for slot-sources,
                   slot-facets, et. al.
  INPUTS       : 1) Data object buffer
                 2) Name of the H/L caller
                 3) Pointer to support function to call
  RETURNS      : Nothing useful
  SIDE EFFECTS : Support function called and data
                  object buffer set
  NOTES        : None
 *****************************************************/
static void SlotInfoSupportFunction(
  DATA_OBJECT *result,
  char *fnxname,
  void (*fnx)(void *,char *,DATA_OBJECT *))
  {
   SYMBOL_HN *ssym;
   DEFCLASS *cls;

   ssym = CheckClassAndSlot(fnxname,&cls);
   if (ssym == NULL)
     {
      SetMultifieldErrorValue(result);
      return;
     }
   (*fnx)((void *) cls,ValueToString(ssym),result);
  }

/*****************************************************************
  NAME         : CountSubclasses
  DESCRIPTION  : Counts the number of direct or indirect
                   subclasses for a class
  INPUTS       : 1) Address of class
                 2) Include (1) or exclude (0) indirect subclasses
                 3) Traversal id
  RETURNS      : The number of subclasses
  SIDE EFFECTS : None
  NOTES        : None
 *****************************************************************/
static int CountSubclasses(
  DEFCLASS *cls,
  int inhp,
  int tvid)
  {
   register unsigned i,cnt;
   register DEFCLASS *subcls;

   for (cnt = 0 , i = 0 ; i < cls->directSubclasses.classCount ; i++)
     {
      subcls = cls->directSubclasses.classArray[i];
      if (TestTraversalID(subcls->traversalRecord,tvid) == 0)
        {
         cnt++;
         SetTraversalID(subcls->traversalRecord,tvid);
         if (inhp && (subcls->directSubclasses.classCount != 0))
           cnt += CountSubclasses(subcls,inhp,tvid);
        }
     }
   return(cnt);
  }

/*********************************************************************
  NAME         : StoreSubclasses
  DESCRIPTION  : Stores the names of direct or indirect
                   subclasses for a class in a mutlifield
  INPUTS       : 1) Caller's multifield buffer
                 2) Starting index
                 3) Address of the class
                 4) Include (1) or exclude (0) indirect subclasses
                 5) Traversal id
  RETURNS      : The number of subclass names stored in the multifield
  SIDE EFFECTS : Multifield set with subclass names
  NOTES        : Assumes multifield is big enough to hold subclasses
 *********************************************************************/
static int StoreSubclasses(
  void *mfval,
  int si,
  DEFCLASS *cls,
  int inhp,
  int tvid,
  short storeName)
  {
   register unsigned i,classi;
   register DEFCLASS *subcls;

   for (i = si , classi = 0 ; classi < cls->directSubclasses.classCount ; classi++)
     {
      subcls = cls->directSubclasses.classArray[classi];
      if (TestTraversalID(subcls->traversalRecord,tvid) == 0)
        {
         SetTraversalID(subcls->traversalRecord,tvid);
         if (storeName)
           {
            SetMFType(mfval,i,SYMBOL);
            SetMFValue(mfval,i++,(void *) GetDefclassNamePointer((void *) subcls));
           }
         else
           {
            SetMFType(mfval,i,DEFCLASS_PTR);
            SetMFValue(mfval,i++,(void *) subcls);
           }
           
         if (inhp && (subcls->directSubclasses.classCount != 0))
           i += StoreSubclasses(mfval,(int) i,subcls,inhp,tvid,storeName);
        }
     }
   return(i - si);
  }

/*********************************************************
  NAME         : SlotInfoSlot
  DESCRIPTION  : Runtime support routine for slot-sources,
                   slot-facets, et. al. which looks up
                   a slot
  INPUTS       : 1) Data object buffer
                 2) Class pointer
                 3) Name-string of slot to find
                 4) The name of the calling function
  RETURNS      : Nothing useful
  SIDE EFFECTS : Support function called and data object
                  buffer initialized
  NOTES        : None
 *********************************************************/
static SLOT_DESC *SlotInfoSlot(
  DATA_OBJECT *result,
  DEFCLASS *cls,
  char *sname,
  char *fnxname)
  {
   SYMBOL_HN *ssym;
   int i;

   if ((ssym = FindSymbol(sname)) == NULL)
     {
      SetEvaluationError(TRUE);
      SetMultifieldErrorValue(result);
      return(NULL);
     }
   i = FindInstanceTemplateSlot(cls,ssym);
   if (i == -1)
     {
      SlotExistError(sname,fnxname);
      SetEvaluationError(TRUE);
      SetMultifieldErrorValue(result);
      return(NULL);
     }
   result->type = MULTIFIELD;
   result->begin = 0;
   return(cls->instanceTemplate[i]);
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
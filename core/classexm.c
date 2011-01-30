   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/13/98          */
   /*                                                     */
   /*                 CLASS EXAMINATION MODULE            */
   /*******************************************************/

/**************************************************************/
/* Purpose: Class browsing and examination commands           */
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

#include <string.h>

#include "argacces.h"
#include "classcom.h"
#include "classfun.h"
#include "classini.h"
#include "memalloc.h"
#include "insfun.h"
#include "msgcom.h"
#include "msgfun.h"
#include "router.h"
#include "strngrtr.h"

#define _CLASSEXM_SOURCE_
#include "classexm.h"

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

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

static int CheckTwoClasses(char *,DEFCLASS **,DEFCLASS **);
static SLOT_DESC *CheckSlotExists(char *,DEFCLASS **,BOOLEAN,BOOLEAN);
static SLOT_DESC *LookupSlot(DEFCLASS *,char *,BOOLEAN);

#if DEBUGGING_FUNCTIONS
static DEFCLASS *CheckClass(char *,char *);
static char *GetClassNameArgument(char *);
static void PrintClassBrowse(char *,DEFCLASS *,int);
static void DisplaySeparator(char *,char *,int,int);
static void DisplaySlotBasicInfo(char *,char *,char *,char *,DEFCLASS *);
static BOOLEAN PrintSlotSources(char *,SYMBOL_HN *,PACKED_CLASS_LINKS *,unsigned,int);
static void DisplaySlotConstraintInfo(char *,char *,char *,int,DEFCLASS *);
static char *ConstraintCode(CONSTRAINT_RECORD *,unsigned,unsigned);
#endif

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

#if DEBUGGING_FUNCTIONS

/****************************************************************
  NAME         : BrowseClassesCommand
  DESCRIPTION  : Displays a "graph" of the class hierarchy
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : Syntax : (browse-classes [<class>])
 ****************************************************************/
globle void BrowseClassesCommand()
  {
   register DEFCLASS *cls;

   if (RtnArgCount() == 0)
      /* ================================================
         Find the OBJECT root class (has no superclasses)
         ================================================ */
      cls = LookupDefclassByMdlOrScope(OBJECT_TYPE_NAME);
   else
     {
      DATA_OBJECT tmp;

      if (ArgTypeCheck("browse-classes",1,SYMBOL,&tmp) == FALSE)
        return;
      cls = LookupDefclassByMdlOrScope(DOToString(tmp));
      if (cls == NULL)
        {
         ClassExistError("browse-classes",DOToString(tmp));
         return;
        }
     }
   BrowseClasses(WDISPLAY,(void *) cls);
  }

/****************************************************************
  NAME         : BrowseClasses
  DESCRIPTION  : Displays a "graph" of the class hierarchy
  INPUTS       : 1) The logical name of the output
                 2) Class pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ****************************************************************/
globle void BrowseClasses(
  char *logicalName,
  void *clsptr)
  {
   PrintClassBrowse(logicalName,(DEFCLASS *) clsptr,0);
  }

/****************************************************************
  NAME         : DescribeClassCommand
  DESCRIPTION  : Displays direct superclasses and
                   subclasses and the entire precedence
                   list for a class
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : Syntax : (describe-class <class-name>)
 ****************************************************************/
globle void DescribeClassCommand()
  {
   char *cname;
   DEFCLASS *cls;

   cname = GetClassNameArgument("describe-class");
   if (cname == NULL)
     return;
   cls = CheckClass("describe-class",cname);
   if (cls == NULL)
     return;
   DescribeClass(WDISPLAY,(void *) cls);
  }

/******************************************************
  NAME         : DescribeClass
  DESCRIPTION  : Displays direct superclasses and
                   subclasses and the entire precedence
                   list for a class
  INPUTS       : 1) The logical name of the output
                 2) Class pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ******************************************************/
globle void DescribeClass(
  char *logicalName,
  void *clsptr)
  {
   DEFCLASS *cls;
   char buf[83],
        slotNamePrintFormat[12],
        overrideMessagePrintFormat[12];
   int i,messageBanner,
       slotNameLength,overrideMessageLength,
       maxSlotNameLength,maxOverrideMessageLength;

   cls = (DEFCLASS *) clsptr;
   DisplaySeparator(logicalName,buf,82,'=');
   DisplaySeparator(logicalName,buf,82,'*');
   if (cls->abstract)
     PrintRouter(logicalName,"Abstract: direct instances of this class cannot be created.\n\n");
   else
     {
      PrintRouter(logicalName,"Concrete: direct instances of this class can be created.\n");
#if INSTANCE_PATTERN_MATCHING
      if (cls->reactive)
        PrintRouter(logicalName,"Reactive: direct instances of this class can match defrule patterns.\n\n");
      else
        PrintRouter(logicalName,"Non-reactive: direct instances of this class cannot match defrule patterns.\n\n");
#else
      PrintRouter(logicalName,"\n");
#endif
     }
   PrintPackedClassLinks(logicalName,"Direct Superclasses:",&cls->directSuperclasses);
   PrintPackedClassLinks(logicalName,"Inheritance Precedence:",&cls->allSuperclasses);
   PrintPackedClassLinks(logicalName,"Direct Subclasses:",&cls->directSubclasses);
   if (cls->instanceTemplate != NULL)
     {
      DisplaySeparator(logicalName,buf,82,'-');
      maxSlotNameLength = 5;
      maxOverrideMessageLength = 8;
      for (i = 0 ; i < cls->instanceSlotCount ; i++)
        {
         slotNameLength = strlen(ValueToString(cls->instanceTemplate[i]->slotName->name));
         if (slotNameLength > maxSlotNameLength)
           maxSlotNameLength = slotNameLength;
         if (cls->instanceTemplate[i]->noWrite == 0)
           {
            overrideMessageLength =
              strlen(ValueToString(cls->instanceTemplate[i]->overrideMessage));
            if (overrideMessageLength > maxOverrideMessageLength)
              maxOverrideMessageLength = overrideMessageLength;
           }
        }
      if (maxSlotNameLength > 16)
        maxSlotNameLength = 16;
      if (maxOverrideMessageLength > 12)
        maxOverrideMessageLength = 12;
      sprintf(slotNamePrintFormat,"%%-%d.%ds : ",maxSlotNameLength,maxSlotNameLength);
      sprintf(overrideMessagePrintFormat,"%%-%d.%ds ",maxOverrideMessageLength,
                                              maxOverrideMessageLength);
      DisplaySlotBasicInfo(logicalName,slotNamePrintFormat,overrideMessagePrintFormat,buf,cls);
      PrintRouter(logicalName,"\nConstraint information for slots:\n\n");
      DisplaySlotConstraintInfo(logicalName,slotNamePrintFormat,buf,82,cls);
     }
   if (cls->handlerCount > 0)
     messageBanner = TRUE;
   else
     {
      messageBanner = FALSE;
      for (i = 1 ; i < cls->allSuperclasses.classCount ; i++)
        if (cls->allSuperclasses.classArray[i]->handlerCount > 0)
          {
           messageBanner = TRUE;
           break;
          }
     }
   if (messageBanner)
     {
      DisplaySeparator(logicalName,buf,82,'-');
      PrintRouter(logicalName,"Recognized message-handlers:\n");
      DisplayHandlersInLinks(logicalName,&cls->allSuperclasses,0);
     }
   DisplaySeparator(logicalName,buf,82,'*');
   DisplaySeparator(logicalName,buf,82,'=');
  }

#endif

/**********************************************************
  NAME         : GetCreateAccessorString
  DESCRIPTION  : Gets a string describing which
                 accessors are implicitly created
                 for a slot: R, W, RW or NIL
  INPUTS       : The slot descriptor
  RETURNS      : The string description
  SIDE EFFECTS : None
  NOTES        : Used by (describe-class) and (slot-facets)
 **********************************************************/
globle char *GetCreateAccessorString(
  void *vsd)
  {
   SLOT_DESC *sd = (SLOT_DESC *) vsd;

   if (sd->createReadAccessor && sd->createWriteAccessor)
     return("RW");
   if ((sd->createReadAccessor == 0) && (sd->createWriteAccessor == 0))
     return("NIL");
   else
     return(sd->createReadAccessor ? "R" : "W");
  }

/************************************************************
  NAME         : GetDefclassModuleCommand
  DESCRIPTION  : Determines to which module a class belongs
  INPUTS       : None
  RETURNS      : The symbolic name of the module
  SIDE EFFECTS : None
  NOTES        : H/L Syntax: (defclass-module <class-name>)
 ************************************************************/
globle SYMBOL_HN *GetDefclassModuleCommand()
  {
   return(GetConstructModuleCommand("defclass-module",DefclassConstruct));
  }

/*********************************************************************
  NAME         : SuperclassPCommand
  DESCRIPTION  : Determines if a class is a superclass of another
  INPUTS       : None
  RETURNS      : TRUE if class-1 is a superclass of class-2
  SIDE EFFECTS : None
  NOTES        : H/L Syntax : (superclassp <class-1> <class-2>)
 *********************************************************************/
globle BOOLEAN SuperclassPCommand()
  {
   DEFCLASS *c1,*c2;

   if (CheckTwoClasses("superclassp",&c1,&c2) == FALSE)
     return(FALSE);
   return(SuperclassP((void *) c1,(void *) c2));
  }

/***************************************************
  NAME         : SuperclassP
  DESCRIPTION  : Determines if the first class is
                 a superclass of the other
  INPUTS       : 1) First class
                 2) Second class
  RETURNS      : TRUE if first class is a
                 superclass of the first,
                 FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle BOOLEAN SuperclassP(
  void *firstClass,
  void *secondClass)
  {
   return(HasSuperclass((DEFCLASS *) secondClass,(DEFCLASS *) firstClass));
  }

/*********************************************************************
  NAME         : SubclassPCommand
  DESCRIPTION  : Determines if a class is a subclass of another
  INPUTS       : None
  RETURNS      : TRUE if class-1 is a subclass of class-2
  SIDE EFFECTS : None
  NOTES        : H/L Syntax : (subclassp <class-1> <class-2>)
 *********************************************************************/
globle BOOLEAN SubclassPCommand()
  {
   DEFCLASS *c1,*c2;

   if (CheckTwoClasses("subclassp",&c1,&c2) == FALSE)
     return(FALSE);
   return(SubclassP((void *) c1,(void *) c2));
  }

/***************************************************
  NAME         : SubclassP
  DESCRIPTION  : Determines if the first class is
                 a subclass of the other
  INPUTS       : 1) First class
                 2) Second class
  RETURNS      : TRUE if first class is a
                 subclass of the first,
                 FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle BOOLEAN SubclassP(
  void *firstClass,
  void *secondClass)
  {
   return(HasSuperclass((DEFCLASS *) firstClass,(DEFCLASS *) secondClass));
  }

/*********************************************************************
  NAME         : SlotExistPCommand
  DESCRIPTION  : Determines if a slot is present in a class
  INPUTS       : None
  RETURNS      : TRUE if the slot exists, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : H/L Syntax : (slot-existp <class> <slot> [inherit])
 *********************************************************************/
globle int SlotExistPCommand()
  {
   DEFCLASS *cls;
   SLOT_DESC *sd;
   int inheritFlag = FALSE;
   DATA_OBJECT dobj;

   sd = CheckSlotExists("slot-existp",&cls,FALSE,TRUE);
   if (sd == NULL)
     return(FALSE);
   if (RtnArgCount() == 3)
     {
      if (ArgTypeCheck("slot-existp",3,SYMBOL,&dobj) == FALSE)
        return(FALSE);
      if (strcmp(DOToString(dobj),"inherit") != 0)
        {
         ExpectedTypeError1("slot-existp",3,"keyword \"inherit\"");
         SetEvaluationError(TRUE);
         return(FALSE);
        }
      inheritFlag = TRUE;
     }
   return((sd->cls == cls) ? TRUE : inheritFlag);
  }

/***************************************************
  NAME         : SlotExistP
  DESCRIPTION  : Determines if a slot exists
  INPUTS       : 1) The class
                 2) The slot name
                 3) A flag indicating if the slot
                    can be inherited or not
  RETURNS      : TRUE if slot exists,
                 FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle BOOLEAN SlotExistP(
  void *theDefclass,
  char *slotName,
  BOOLEAN inheritFlag)
  {
   return((LookupSlot((DEFCLASS *) theDefclass,slotName,inheritFlag) != NULL)
           ? TRUE : FALSE);
  }

/************************************************************************************
  NAME         : MessageHandlerExistPCommand
  DESCRIPTION  : Determines if a message-handler is present in a class
  INPUTS       : None
  RETURNS      : TRUE if the message header is present, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : H/L Syntax : (message-handler-existp <class> <hnd> [<type>])
 ************************************************************************************/
globle int MessageHandlerExistPCommand()
  {
   DEFCLASS *cls;
   SYMBOL_HN *mname;
   DATA_OBJECT temp;
   unsigned mtype = MPRIMARY;

   if (ArgTypeCheck("message-handler-existp",1,SYMBOL,&temp) == FALSE)
     return(FALSE);
   cls = LookupDefclassByMdlOrScope(DOToString(temp));
   if (cls == NULL)
     {
      ClassExistError("message-handler-existp",DOToString(temp));
      return(FALSE);
     }
   if (ArgTypeCheck("message-handler-existp",2,SYMBOL,&temp) == FALSE)
     return(FALSE);
   mname = (SYMBOL_HN *) GetValue(temp);
   if (RtnArgCount() == 3)
     {
      if (ArgTypeCheck("message-handler-existp",3,SYMBOL,&temp) == FALSE)
        return(FALSE);
      mtype = HandlerType("message-handler-existp",DOToString(temp));
      if (mtype == MERROR)
        {
         SetEvaluationError(TRUE);
         return(FALSE);
        }
     }
   if (FindHandlerByAddress(cls,mname,mtype) != NULL)
     return(TRUE);
   return(FALSE);
  }

/**********************************************************************
  NAME         : SlotWritablePCommand
  DESCRIPTION  : Determines if an existing slot can be written to
  INPUTS       : None
  RETURNS      : TRUE if the slot is writable, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : H/L Syntax : (slot-writablep <class> <slot>)
 **********************************************************************/
globle BOOLEAN SlotWritablePCommand()
  {
   DEFCLASS *theDefclass;
   SLOT_DESC *sd;

   sd = CheckSlotExists("slot-writablep",&theDefclass,TRUE,TRUE);
   if (sd == NULL)
     return(FALSE);
   return(sd->noWrite ? FALSE : TRUE);
  }

/***************************************************
  NAME         : SlotWritableP
  DESCRIPTION  : Determines if a slot is writable
  INPUTS       : 1) The class
                 2) The slot name
  RETURNS      : TRUE if slot is writable,
                 FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle BOOLEAN SlotWritableP(
  void *theDefclass,
  char *slotName)
  {
   SLOT_DESC *sd;

   if ((sd = LookupSlot((DEFCLASS *) theDefclass,slotName,TRUE)) == NULL)
     return(FALSE);
   return(sd->noWrite ? FALSE : TRUE);
  }

/**********************************************************************
  NAME         : SlotInitablePCommand
  DESCRIPTION  : Determines if an existing slot can be initialized
                   via an init message-handler or slot-override
  INPUTS       : None
  RETURNS      : TRUE if the slot is writable, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : H/L Syntax : (slot-initablep <class> <slot>)
 **********************************************************************/
globle BOOLEAN SlotInitablePCommand()
  {
   DEFCLASS *theDefclass;
   SLOT_DESC *sd;

   sd = CheckSlotExists("slot-initablep",&theDefclass,TRUE,TRUE);
   if (sd == NULL)
     return(FALSE);
   return((sd->noWrite && (sd->initializeOnly == 0)) ? FALSE : TRUE);
  }

/***************************************************
  NAME         : SlotInitableP
  DESCRIPTION  : Determines if a slot is initable
  INPUTS       : 1) The class
                 2) The slot name
  RETURNS      : TRUE if slot is initable,
                 FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle BOOLEAN SlotInitableP(
  void *theDefclass,
  char *slotName)
  {
   SLOT_DESC *sd;

   if ((sd = LookupSlot((DEFCLASS *) theDefclass,slotName,TRUE)) == NULL)
     return(FALSE);
   return((sd->noWrite && (sd->initializeOnly == 0)) ? FALSE : TRUE);
  }

/**********************************************************************
  NAME         : SlotPublicPCommand
  DESCRIPTION  : Determines if an existing slot is publicly visible
                   for direct reference by subclasses
  INPUTS       : None
  RETURNS      : TRUE if the slot is public, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : H/L Syntax : (slot-publicp <class> <slot>)
 **********************************************************************/
globle BOOLEAN SlotPublicPCommand()
  {
   DEFCLASS *theDefclass;
   SLOT_DESC *sd;

   sd = CheckSlotExists("slot-publicp",&theDefclass,TRUE,FALSE);
   if (sd == NULL)
     return(FALSE);
   return(sd->publicVisibility ? TRUE : FALSE);
  }

/***************************************************
  NAME         : SlotPublicP
  DESCRIPTION  : Determines if a slot is public
  INPUTS       : 1) The class
                 2) The slot name
  RETURNS      : TRUE if slot is public,
                 FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle BOOLEAN SlotPublicP(
  void *theDefclass,
  char *slotName)
  {
   SLOT_DESC *sd;

   if ((sd = LookupSlot((DEFCLASS *) theDefclass,slotName,FALSE)) == NULL)
     return(FALSE);
   return(sd->publicVisibility ? TRUE : FALSE);
  }

/**********************************************************************
  NAME         : SlotDirectAccessPCommand
  DESCRIPTION  : Determines if an existing slot can be directly
                   referenced by the class - i.e., if the slot is
                   private, is the slot defined in the class
  INPUTS       : None
  RETURNS      : TRUE if the slot is private,
                    FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : H/L Syntax : (slot-direct-accessp <class> <slot>)
 **********************************************************************/
globle BOOLEAN SlotDirectAccessPCommand()
  {
   DEFCLASS *theDefclass;
   SLOT_DESC *sd;

   sd = CheckSlotExists("slot-direct-accessp",&theDefclass,TRUE,TRUE);
   if (sd == NULL)
     return(FALSE);
   return((sd->publicVisibility || (sd->cls == theDefclass)) ? TRUE : FALSE);
  }

/***************************************************
  NAME         : SlotDirectAccessP
  DESCRIPTION  : Determines if a slot is directly
                 accessible from message-handlers
                 on class
  INPUTS       : 1) The class
                 2) The slot name
  RETURNS      : TRUE if slot is directly
                 accessible, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle BOOLEAN SlotDirectAccessP(
  void *theDefclass,
  char *slotName)
  {
   SLOT_DESC *sd;

   if ((sd = LookupSlot((DEFCLASS *) theDefclass,slotName,TRUE)) == NULL)
     return(FALSE);
   return((sd->publicVisibility || (sd->cls == (DEFCLASS *) theDefclass)) ?
           TRUE : FALSE);
  }

/**********************************************************************
  NAME         : SlotDefaultValueCommand
  DESCRIPTION  : Determines the default avlue for the specified slot
                 of the specified class
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : H/L Syntax : (slot-default-value <class> <slot>)
 **********************************************************************/
globle void SlotDefaultValueCommand(
  DATA_OBJECT_PTR theValue)
  {
   DEFCLASS *theDefclass;
   SLOT_DESC *sd;

   SetpType(theValue,SYMBOL);
   SetpValue(theValue,FalseSymbol);
   sd = CheckSlotExists("slot-default-value",&theDefclass,TRUE,TRUE);
   if (sd == NULL)
     return;
   if (sd->dynamicDefault)
     EvaluateAndStoreInDataObject((int) sd->multiple,
                                  (EXPRESSION *) sd->defaultValue,
                                  theValue);
   else
     GenCopyMemory(DATA_OBJECT,1,theValue,sd->defaultValue);
  }

/*********************************************************
  NAME         : SlotDefaultValue
  DESCRIPTION  : Determines the default avlue for
                 the specified slot of the specified class
  INPUTS       : 1) The class
                 2) The slot name
  RETURNS      : TRUE if slot default value is set,
                 FALSE otherwise
  SIDE EFFECTS : Slot default value evaluated - dynamic
                 defaults will cause any side effects
  NOTES        : None
 *********************************************************/
globle BOOLEAN SlotDefaultValue(
  void *theDefclass,
  char *slotName,
  DATA_OBJECT_PTR theValue)
  {
   SLOT_DESC *sd;

   SetpType(theValue,SYMBOL);
   SetpValue(theValue,FalseSymbol);
   if ((sd = LookupSlot((DEFCLASS *) theDefclass,slotName,TRUE)) == NULL)
     return(FALSE);
   if (sd->dynamicDefault)
     return(EvaluateAndStoreInDataObject((int) sd->multiple,
                                         (EXPRESSION *) sd->defaultValue,
                                         theValue));
   GenCopyMemory(DATA_OBJECT,1,theValue,sd->defaultValue);
   return(TRUE);
  }

/********************************************************
  NAME         : ClassExistPCommand
  DESCRIPTION  : Determines if a class exists
  INPUTS       : None
  RETURNS      : TRUE if class exists, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : H/L Syntax : (class-existp <arg>)
 ********************************************************/
globle BOOLEAN ClassExistPCommand()
  {
   DATA_OBJECT temp;

   if (ArgTypeCheck("class-existp",1,SYMBOL,&temp) == FALSE)
     return(FALSE);
   return((LookupDefclassByMdlOrScope(DOToString(temp)) != NULL) ? TRUE : FALSE);
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/******************************************************
  NAME         : CheckTwoClasses
  DESCRIPTION  : Checks for exactly two class arguments
                    for a H/L function
  INPUTS       : 1) The function name
                 2) Caller's buffer for first class
                 3) Caller's buffer for second class
  RETURNS      : TRUE if both found, FALSE otherwise
  SIDE EFFECTS : Caller's buffers set
  NOTES        : Assumes exactly 2 arguments
 ******************************************************/
static int CheckTwoClasses(
  char *func,
  DEFCLASS **c1,
  DEFCLASS **c2)
  {
   DATA_OBJECT temp;

   if (ArgTypeCheck(func,1,SYMBOL,&temp) == FALSE)
     return(FALSE);
   *c1 = LookupDefclassByMdlOrScope(DOToString(temp));
   if (*c1 == NULL)
     {
      ClassExistError(func,ValueToString(temp.value));
      return(FALSE);
     }
   if (ArgTypeCheck(func,2,SYMBOL,&temp) == FALSE)
     return(FALSE);
   *c2 = LookupDefclassByMdlOrScope(DOToString(temp));
   if (*c2 == NULL)
     {
      ClassExistError(func,ValueToString(temp.value));
      return(FALSE);
     }
   return(TRUE);
  }

/***************************************************
  NAME         : CheckSlotExists
  DESCRIPTION  : Checks first two arguments of
                 a function for a valid class
                 and (inherited) slot
  INPUTS       : 1) The name of the function
                 2) A buffer to hold the found class
                 3) A flag indicating whether the
                    non-existence of the slot should
                    be an error
                 4) A flag indicating if the slot
                    can be inherited or not
  RETURNS      : NULL if slot not found, slot
                 descriptor otherwise
  SIDE EFFECTS : Class buffer set if no errors,
                 NULL on errors
  NOTES        : None
 ***************************************************/
static SLOT_DESC *CheckSlotExists(
  char *func,
  DEFCLASS **classBuffer,
  BOOLEAN existsErrorFlag,
  BOOLEAN inheritFlag)
  {
   SYMBOL_HN *ssym;
   int slotIndex;
   SLOT_DESC *sd;

   ssym = CheckClassAndSlot(func,classBuffer);
   if (ssym == NULL)
     return(NULL);
   slotIndex = FindInstanceTemplateSlot(*classBuffer,ssym);
   if (slotIndex == -1)
     {
      if (existsErrorFlag)
        {
         SlotExistError(ValueToString(ssym),func);
         SetEvaluationError(TRUE);
        }
      return(NULL);
     }
   sd = (*classBuffer)->instanceTemplate[slotIndex];
   if ((sd->cls == *classBuffer) || inheritFlag)
     return(sd);
   PrintErrorID("CLASSEXM",1,FALSE);
   PrintRouter(WERROR,"Inherited slot ");
   PrintRouter(WERROR,ValueToString(ssym));
   PrintRouter(WERROR," from class ");
   PrintClassName(WERROR,sd->cls,FALSE);
   PrintRouter(WERROR," is not valid for function ");
   PrintRouter(WERROR,func);
   PrintRouter(WERROR,"\n");
   SetEvaluationError(TRUE);
   return(NULL);
  }

/***************************************************
  NAME         : LookupSlot
  DESCRIPTION  : Finds a slot in a class
  INPUTS       : 1) The class
                 2) The slot name
                 3) A flag indicating if inherited
                    slots are OK or not
  RETURNS      : The slot descriptor address, or
                 NULL if not found
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
static SLOT_DESC *LookupSlot(
  DEFCLASS *theDefclass,
  char *slotName,
  BOOLEAN inheritFlag)
  {
   SYMBOL_HN *slotSymbol;
   int slotIndex;
   SLOT_DESC *sd;

   slotSymbol = FindSymbol(slotName);
   if (slotSymbol == NULL)
     return(NULL);
   slotIndex = FindInstanceTemplateSlot(theDefclass,slotSymbol);
   if (slotIndex == -1)
     return(NULL);
   sd = theDefclass->instanceTemplate[slotIndex];
   if ((sd->cls != theDefclass) && (inheritFlag == FALSE))
     return(NULL);
   return(sd);
  }

#if DEBUGGING_FUNCTIONS

/*****************************************************
  NAME         : CheckClass
  DESCRIPTION  : Used for to check class name for
                 class accessor functions such
                 as ppdefclass and undefclass
  INPUTS       : 1) The name of the H/L function
                 2) Name of the class
  RETURNS      : The class address,
                   or NULL if ther was an error
  SIDE EFFECTS : None
  NOTES        : None
 ******************************************************/
static DEFCLASS *CheckClass(
  char *func,
  char *cname)
  {
   DEFCLASS *cls;

   cls = LookupDefclassByMdlOrScope(cname);
   if (cls == NULL)
     ClassExistError(func,cname);
   return(cls);
  }

/*********************************************************
  NAME         : GetClassNameArgument
  DESCRIPTION  : Gets a class name-string
  INPUTS       : Calling function name
  RETURNS      : Class name (NULL on errors)
  SIDE EFFECTS : None
  NOTES        : Assumes only 1 argument
 *********************************************************/
static char *GetClassNameArgument(
  char *fname)
  {
   DATA_OBJECT temp;

   if (ArgTypeCheck(fname,1,SYMBOL,&temp) == FALSE)
     return(NULL);
   return(DOToString(temp));
  }

/****************************************************************
  NAME         : PrintClassBrowse
  DESCRIPTION  : Displays a "graph" of class and subclasses
  INPUTS       : 1) The logical name of the output
                 2) The class address
                 3) The depth of the graph
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ****************************************************************/
static void PrintClassBrowse(
  char *logicalName,
  DEFCLASS *cls,
  int depth)
  {
   register unsigned i;

   for (i = 0 ; i < depth ; i++)
     PrintRouter(logicalName,"  ");
   PrintRouter(logicalName,GetDefclassName((void *) cls));
   if (cls->directSuperclasses.classCount > 1)
     PrintRouter(logicalName," *");
   PrintRouter(logicalName,"\n");
   for (i = 0 ;i < cls->directSubclasses.classCount ; i++)
     PrintClassBrowse(logicalName,cls->directSubclasses.classArray[i],depth+1);
  }

/*********************************************************
  NAME         : DisplaySeparator
  DESCRIPTION  : Prints a separator line for DescribeClass
  INPUTS       : 1) The logical name of the output
                 2) The buffer to use for the line
                 3) The buffer size
                 4) The character to use
  RETURNS      : Nothing useful
  SIDE EFFECTS : Buffer overwritten and displayed
  NOTES        : None
 *********************************************************/
static void DisplaySeparator(
  char *logicalName,
  char *buf,
  int maxlen,
  int sepchar)
  {
   register int i;

   for (i = 0 ; i < maxlen-2 ; i++)
     buf[i] = (char) sepchar;
   buf[i++] = '\n';
   buf[i] = '\0';
   PrintRouter(logicalName,buf);
  }

/*************************************************************
  NAME         : DisplaySlotBasicInfo
  DESCRIPTION  : Displays a table summary of basic
                  facets for the slots of a class
                  including:
                  single/multiple
                  default/no-default/default-dynamic
                  inherit/no-inherit
                  read-write/initialize-only/read-only
                  local/shared
                  composite/exclusive
                  reactive/non-reactive
                  public/private
                  create-accessor read/write
                  override-message

                  The function also displays the source
                  class(es) for the facets
  INPUTS       : 1) The logical name of the output
                 2) A format string for use in sprintf
                    (for printing slot names)
                 3) A format string for use in sprintf
                    (for printing slot override message names)
                 4) A buffer to store the display in
                 5) A pointer to the class
  RETURNS      : Nothing useful
  SIDE EFFECTS : Buffer written to and displayed
  NOTES        : None
 *************************************************************/
static void DisplaySlotBasicInfo(
  char *logicalName,
  char *slotNamePrintFormat,
  char *overrideMessagePrintFormat,
  char *buf,
  DEFCLASS *cls)
  {
   register int i;
   SLOT_DESC *sp;
   char *createString;

   sprintf(buf,slotNamePrintFormat,"SLOTS");
#if INSTANCE_PATTERN_MATCHING
   strcat(buf,"FLD DEF PRP ACC STO MCH SRC VIS CRT ");
#else
   strcat(buf,"FLD DEF PRP ACC STO SRC VIS CRT ");
#endif
   PrintRouter(logicalName,buf);
   sprintf(buf,overrideMessagePrintFormat,"OVRD-MSG");
   PrintRouter(logicalName,buf);
   PrintRouter(logicalName,"SOURCE(S)\n");
   for (i = 0 ; i < cls->instanceSlotCount ; i++)
     {
      sp = cls->instanceTemplate[i];
      sprintf(buf,slotNamePrintFormat,ValueToString(sp->slotName->name));
      strcat(buf,sp->multiple ? "MLT " : "SGL ");
      if (sp->noDefault)
        strcat(buf,"NIL ");
      else
        strcat(buf,sp->dynamicDefault ? "DYN " : "STC ");
      strcat(buf,sp->noInherit ? "NIL " : "INH ");
      if (sp->initializeOnly)
        strcat(buf,"INT ");
      else if (sp->noWrite)
        strcat(buf," R  ");
      else
        strcat(buf,"RW  ");
      strcat(buf,sp->shared ? "SHR " : "LCL ");
#if INSTANCE_PATTERN_MATCHING
      strcat(buf,sp->reactive ? "RCT " : "NIL ");
#endif
      strcat(buf,sp->composite ? "CMP " : "EXC ");
      strcat(buf,sp->publicVisibility ? "PUB " : "PRV ");
      createString = GetCreateAccessorString(sp);
      if (createString[1] == '\0')
        strcat(buf," ");
      strcat(buf,createString);
      if ((createString[1] == '\0') ? TRUE : (createString[2] == '\0'))
        strcat(buf," ");
      strcat(buf," ");
      PrintRouter(logicalName,buf);
      sprintf(buf,overrideMessagePrintFormat,
              sp->noWrite ? "NIL" : ValueToString(sp->overrideMessage));
      PrintRouter(logicalName,buf);
      PrintSlotSources(logicalName,sp->slotName->name,&sp->cls->allSuperclasses,0,TRUE);
      PrintRouter(logicalName,"\n");
     }
  }

/***************************************************
  NAME         : PrintSlotSources
  DESCRIPTION  : Displays a list of source classes
                   for a composite class (in order
                   of most general to specific)
  INPUTS       : 1) The logical name of the output
                 2) The name of the slot
                 3) The precedence list of the class
                    of the slot (the source class
                    shold be first in the list)
                 4) The index into the packed
                    links array
                 5) Flag indicating whether to
                    disregard noniherit facet
  RETURNS      : TRUE if a class is printed, FALSE
                 otherwise
  SIDE EFFECTS : Recursively prints out appropriate
                 memebers from list in reverse order
  NOTES        : None
 ***************************************************/
static BOOLEAN PrintSlotSources(
  char *logicalName,
  SYMBOL_HN *sname,
  PACKED_CLASS_LINKS *sprec,
  unsigned index,
  int inhp)
  {
   SLOT_DESC *csp;

   if (index == sprec->classCount)
     return(FALSE);
   csp = FindClassSlot(sprec->classArray[index],sname);
   if ((csp != NULL) ? ((csp->noInherit == 0) || inhp) : FALSE)
     {
      if (csp->composite)
        {
         if (PrintSlotSources(logicalName,sname,sprec,index+1,FALSE))
           PrintRouter(logicalName," ");
        }
      PrintClassName(logicalName,sprec->classArray[index],FALSE);
      return(TRUE);
     }
   else
     return(PrintSlotSources(logicalName,sname,sprec,index+1,FALSE));
  }

/*********************************************************
  NAME         : DisplaySlotConstraintInfo
  DESCRIPTION  : Displays a table summary of type-checking
                  facets for the slots of a class
                  including:
                  type
                  allowed-symbols
                  allowed-integers
                  allowed-floats
                  allowed-values
                  allowed-instance-names
                  range
                  min-number-of-elements
                  max-number-of-elements

                  The function also displays the source
                  class(es) for the facets
  INPUTS       : 1) A format string for use in sprintf
                 2) A buffer to store the display in
                 3) Maximum buffer size
                 4) A pointer to the class
  RETURNS      : Nothing useful
  SIDE EFFECTS : Buffer written to and displayed
  NOTES        : None
 *********************************************************/
static void DisplaySlotConstraintInfo(
  char *logicalName,
  char *slotNamePrintFormat,
  char *buf,
  int maxlen,
  DEFCLASS *cls)
  {
   register int i;
   CONSTRAINT_RECORD *cr;
   char *strdest = "***describe-class***";

   sprintf(buf,slotNamePrintFormat,"SLOTS");
   strcat(buf,"SYM STR INN INA EXA FTA INT FLT\n");
   PrintRouter(logicalName,buf);
   for (i = 0 ; i < cls->instanceSlotCount ; i++)
     {
      cr = cls->instanceTemplate[i]->constraint;
      sprintf(buf,slotNamePrintFormat,ValueToString(cls->instanceTemplate[i]->slotName->name));
      if (cr != NULL)
        {
         strcat(buf,ConstraintCode(cr,(unsigned) cr->symbolsAllowed,
                                      (unsigned) cr->symbolRestriction));
         strcat(buf,ConstraintCode(cr,(unsigned) cr->stringsAllowed,
                                      (unsigned) cr->stringRestriction));
         strcat(buf,ConstraintCode(cr,(unsigned) cr->instanceNamesAllowed,
                                      (unsigned) cr->instanceNameRestriction));
         strcat(buf,ConstraintCode(cr,(unsigned) cr->instanceAddressesAllowed,0));
         strcat(buf,ConstraintCode(cr,(unsigned) cr->externalAddressesAllowed,0));
         strcat(buf,ConstraintCode(cr,(unsigned) cr->factAddressesAllowed,0));
         strcat(buf,ConstraintCode(cr,(unsigned) cr->integersAllowed,
                                      (unsigned) cr->integerRestriction));
         strcat(buf,ConstraintCode(cr,(unsigned) cr->floatsAllowed,
                                      (unsigned) cr->floatRestriction));
         OpenStringDestination(strdest,buf + strlen(buf),
                               (int) (maxlen - strlen(buf) - 1));
         if (cr->integersAllowed || cr->floatsAllowed || cr->anyAllowed)
           {
            PrintRouter(strdest,"RNG:[");
            PrintExpression(strdest,cr->minValue);
            PrintRouter(strdest,"..");
            PrintExpression(strdest,cr->maxValue);
            PrintRouter(strdest,"] ");
           }
         if (cls->instanceTemplate[i]->multiple)
           {
            PrintRouter(strdest,"CRD:[");
            PrintExpression(strdest,cr->minFields);
            PrintRouter(strdest,"..");
            PrintExpression(strdest,cr->maxFields);
            PrintRouter(strdest,"]");
           }
        }
      else
        {
         OpenStringDestination(strdest,buf,maxlen);
         PrintRouter(strdest," +   +   +   +   +   +   +   +  RNG:[-oo..+oo]");
         if (cls->instanceTemplate[i]->multiple)
           PrintRouter(strdest," CRD:[0..+oo]");
        }
      PrintRouter(strdest,"\n");
      CloseStringDestination(strdest);
      PrintRouter(logicalName,buf);
     }
  }

/******************************************************
  NAME         : ConstraintCode
  DESCRIPTION  : Gives a string code representing the
                 type of constraint
  INPUTS       : 1) The constraint record
                 2) Allowed Flag
                 3) Restricted Values flag
  RETURNS      : "    " for type not allowed
                 " +  " for any value of type allowed
                 " #  " for some values of type allowed
  SIDE EFFECTS : None
  NOTES        : Used by DisplaySlotConstraintInfo
 ******************************************************/
static char *ConstraintCode(
  CONSTRAINT_RECORD *cr,
  unsigned allow,
  unsigned restrict)
  {
   if (allow || cr->anyAllowed)
     return((restrict || cr->anyRestriction) ? " #  " : " +  ");
   return("    ");
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
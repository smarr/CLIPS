   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/13/98          */
   /*                                                     */
   /*                  CLASS COMMANDS MODULE              */
   /*******************************************************/

/**************************************************************/
/* Purpose: Kernel Interface Commands for Object System       */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Brian L. Donnell                                      */
/*                                                            */
/* Contributing Programmer(s):                                */
/*                                                            */
/* Revision History:                                          */
/*                                                            */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/**************************************************************/

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "setup.h"

#if OBJECT_SYSTEM

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
#include "bload.h"
#endif

#include "argacces.h"
#include "classfun.h"
#include "classini.h"
#include "modulutl.h"
#include "msgcom.h"
#include "router.h"

#define _CLASSCOM_SOURCE_
#include "classcom.h"

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

#if (! BLOAD_ONLY) && (! RUN_TIME) && DEBUGGING_FUNCTIONS
static void SaveDefclass(struct constructHeader *,void *);
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

/*******************************************************************
  NAME         : FindDefclass
  DESCRIPTION  : Looks up a specified class in the class hash table
                 (Only looks in current or specified module)
  INPUTS       : The name-string of the class (including module)
  RETURNS      : The address of the found class, NULL otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ******************************************************************/
globle void *FindDefclass(
  char *classAndModuleName)
  {
   SYMBOL_HN *classSymbol;
   DEFCLASS *cls;
   struct defmodule *theModule;

   SaveCurrentModule();
   classSymbol = FindSymbol(ExtractModuleAndConstructName(classAndModuleName));
   theModule = ((struct defmodule *) GetCurrentModule());
   RestoreCurrentModule();

   if (classSymbol == NULL)
     return(NULL);
   cls = ClassTable[HashClass(classSymbol)];
   while (cls != NULL)
     {
      if (cls->header.name == classSymbol)
        {
         if (cls->system || (cls->header.whichModule->theModule == theModule))
           return(cls->installed ? (void *) cls : NULL);
        }
      cls = cls->nxtHash;
     }
   return(NULL);
  }

/***************************************************
  NAME         : LookupDefclassByMdlOrScope
  DESCRIPTION  : Finds a class anywhere (if module
                 is specified) or in current or
                 imported modules
  INPUTS       : The class name
  RETURNS      : The class (NULL if not found)
  SIDE EFFECTS : Error message printed on
                  ambiguous references
  NOTES        : Assumes no two classes of the same
                 name are ever in the same scope
 ***************************************************/
globle DEFCLASS *LookupDefclassByMdlOrScope(
  char *classAndModuleName)
  {
   DEFCLASS *cls;
   char *className;
   SYMBOL_HN *classSymbol;
   struct defmodule *theModule;

   if (FindModuleSeparator(classAndModuleName) == FALSE)
     return(LookupDefclassInScope(classAndModuleName));

   SaveCurrentModule();
   className = ExtractModuleAndConstructName(classAndModuleName);
   theModule = ((struct defmodule *) GetCurrentModule());
   RestoreCurrentModule();
   if(className == NULL)
     return(NULL);
   if ((classSymbol = FindSymbol(className)) == NULL)
     return(NULL);
   cls = ClassTable[HashClass(classSymbol)];
   while (cls != NULL)
     {
      if ((cls->header.name == classSymbol) &&
          (cls->header.whichModule->theModule == theModule))
        return(cls->installed ? cls : NULL);
      cls = cls->nxtHash;
     }
   return(NULL);
  }

/****************************************************
  NAME         : LookupDefclassInScope
  DESCRIPTION  : Finds a class in current or imported
                   modules (module specifier
                   is not allowed)
  INPUTS       : The class name
  RETURNS      : The class (NULL if not found)
  SIDE EFFECTS : Error message printed on
                  ambiguous references
  NOTES        : Assumes no two classes of the same
                 name are ever in the same scope
 ****************************************************/
globle DEFCLASS *LookupDefclassInScope(
  char *className)
  {
   DEFCLASS *cls;
   SYMBOL_HN *classSymbol;

   if ((classSymbol = FindSymbol(className)) == NULL)
     return(NULL);
   cls = ClassTable[HashClass(classSymbol)];
   while (cls != NULL)
     {
      if ((cls->header.name == classSymbol) && DefclassInScope(cls,NULL))
        return(cls->installed ? cls : NULL);
      cls = cls->nxtHash;
     }
   return(NULL);
  }

/******************************************************
  NAME         : LookupDefclassAnywhere
  DESCRIPTION  : Finds a class in specified
                 (or any) module
  INPUTS       : 1) The module (NULL if don't care)
                 2) The class name (module specifier
                    in name not allowed)
  RETURNS      : The class (NULL if not found)
  SIDE EFFECTS : None
  NOTES        : Does *not* generate an error if
                 multiple classes of the same name
                 exist as do the other lookup functions
 ******************************************************/
globle DEFCLASS *LookupDefclassAnywhere(
  struct defmodule *theModule,
  char *className)
  {
   DEFCLASS *cls;
   SYMBOL_HN *classSymbol;

   if ((classSymbol = FindSymbol(className)) == NULL)
     return(NULL);
   cls = ClassTable[HashClass(classSymbol)];
   while (cls != NULL)
     {
      if ((cls->header.name == classSymbol) &&
          ((theModule == NULL) ||
           (cls->header.whichModule->theModule == theModule)))
        return(cls->installed ? cls : NULL);
      cls = cls->nxtHash;
     }
   return(NULL);
  }

/***************************************************
  NAME         : DefclassInScope
  DESCRIPTION  : Determines if a defclass is in
                 scope of the given module
  INPUTS       : 1) The defclass
                 2) The module (NULL for current
                    module)
  RETURNS      : TRUE if in scope,
                 FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle BOOLEAN DefclassInScope(
  DEFCLASS *theDefclass,
  struct defmodule *theModule)
  {
#if DEFMODULE_CONSTRUCT
   int moduleID;
   char *scopeMap;

   scopeMap = (char *) ValueToBitMap(theDefclass->scopeMap);
   if (theModule == NULL)
     theModule = ((struct defmodule *) GetCurrentModule());
   moduleID = (int) theModule->bsaveID;
   return(TestBitMap(scopeMap,moduleID) ? TRUE : FALSE);
#else
   return(TRUE);
#endif
  }

/***********************************************************
  NAME         : GetNextDefclass
  DESCRIPTION  : Finds first or next defclass
  INPUTS       : The address of the current defclass
  RETURNS      : The address of the next defclass
                   (NULL if none)
  SIDE EFFECTS : None
  NOTES        : If ptr == NULL, the first defclass
                    is returned.
 ***********************************************************/
globle void *GetNextDefclass(
  void *ptr)
  {
   return((void *) GetNextConstructItem((struct constructHeader *) ptr,DefclassModuleIndex));
  }

/***************************************************
  NAME         : IsDefclassDeletable
  DESCRIPTION  : Determines if a defclass
                   can be deleted
  INPUTS       : Address of the defclass
  RETURNS      : TRUE if deletable,
                 FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle BOOLEAN IsDefclassDeletable(
  void *ptr)
  {
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(ptr)
#endif

#if BLOAD_ONLY || RUN_TIME
   return(FALSE);
#else
   DEFCLASS *cls;

#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded())
     return(FALSE);
#endif
   cls = (DEFCLASS *) ptr;
   if (cls->system == 1)
     return(FALSE);
   return((IsClassBeingUsed(cls) == FALSE) ? TRUE : FALSE);
#endif
  }

/*************************************************************
  NAME         : UndefclassCommand
  DESCRIPTION  : Deletes a class and its subclasses, as
                 well as their associated instances
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : Syntax : (undefclass <class-name> | *)
 *************************************************************/
globle void UndefclassCommand()
  {
   UndefconstructCommand("undefclass",DefclassConstruct);
  }

/********************************************************
  NAME         : Undefclass
  DESCRIPTION  : Deletes the named defclass
  INPUTS       : None
  RETURNS      : TRUE if deleted, or FALSE
  SIDE EFFECTS : Defclass and handlers removed
  NOTES        : Interface for AddConstruct()
 ********************************************************/
globle BOOLEAN Undefclass(
  void *theDefclass)
  {
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(theDefclass)
#endif

#if RUN_TIME || BLOAD_ONLY
   return(FALSE);
#else
   DEFCLASS *cls;

   cls = (DEFCLASS *) theDefclass;
#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded())
     return(FALSE);
#endif
   if (cls == NULL)
     return(RemoveAllUserClasses());
   return(DeleteClassUAG(cls));
#endif
  }


#if DEBUGGING_FUNCTIONS

/*********************************************************
  NAME         : PPDefclassCommand
  DESCRIPTION  : Displays the pretty print form of
                 a class to the wdialog router.
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : Syntax : (ppdefclass <class-name>)
 *********************************************************/
globle void PPDefclassCommand()
  {
   PPConstructCommand("ppdefclass",DefclassConstruct);
  }

/***************************************************
  NAME         : ListDefclassesCommand
  DESCRIPTION  : Displays all defclass names
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Defclass names printed
  NOTES        : H/L Interface
 ***************************************************/
globle void ListDefclassesCommand()
  {
   ListConstructCommand("list-defclasses",DefclassConstruct);
  }

/***************************************************
  NAME         : ListDefclasses
  DESCRIPTION  : Displays all defclass names
  INPUTS       : 1) The logical name of the output
                 2) The module
  RETURNS      : Nothing useful
  SIDE EFFECTS : Defclass names printed
  NOTES        : C Interface
 ***************************************************/
globle void ListDefclasses(
  char *logicalName,
  struct defmodule *theModule)
  {
   ListConstruct(DefclassConstruct,logicalName,theModule);
  }

/*********************************************************
  NAME         : GetDefclassWatchInstances
  DESCRIPTION  : Determines if deletions/creations of
                 instances of this class will generate
                 trace messages or not
  INPUTS       : A pointer to the class
  RETURNS      : TRUE if a trace is active,
                 FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 *********************************************************/
globle BOOLEAN GetDefclassWatchInstances(
  void *theClass)
  {
   return(((DEFCLASS *) theClass)->traceInstances);
  }

/*********************************************************
  NAME         : SetDefclassWatchInstances
  DESCRIPTION  : Sets the trace to ON/OFF for the
                 creation/deletion of instances
                 of the class
  INPUTS       : 1) TRUE to set the trace on,
                    FALSE to set it off
                 2) A pointer to the class
  RETURNS      : Nothing useful
  SIDE EFFECTS : Watch flag for the class set
  NOTES        : None
 *********************************************************/
globle void SetDefclassWatchInstances(
  int newState,
  void *theClass)
  {
   if (((DEFCLASS *) theClass)->abstract)
     return;
   ((DEFCLASS *) theClass)->traceInstances = newState;
  }

/*********************************************************
  NAME         : GetDefclassWatchSlots
  DESCRIPTION  : Determines if changes to slots of
                 instances of this class will generate
                 trace messages or not
  INPUTS       : A pointer to the class
  RETURNS      : TRUE if a trace is active,
                 FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 *********************************************************/
globle BOOLEAN GetDefclassWatchSlots(
  void *theClass)
  {
   return(((DEFCLASS *) theClass)->traceSlots);
  }

/**********************************************************
  NAME         : SetDefclassWatchSlots
  DESCRIPTION  : Sets the trace to ON/OFF for the
                 changes to slots of instances of the class
  INPUTS       : 1) TRUE to set the trace on,
                    FALSE to set it off
                 2) A pointer to the class
  RETURNS      : Nothing useful
  SIDE EFFECTS : Watch flag for the class set
  NOTES        : None
 **********************************************************/
globle void SetDefclassWatchSlots(
  int newState,
  void *theClass)
  {
   ((DEFCLASS *) theClass)->traceSlots = newState;
  }

/******************************************************************
  NAME         : DefclassWatchAccess
  DESCRIPTION  : Parses a list of class names passed by
                 AddWatchItem() and sets the traces accordingly
  INPUTS       : 1) A code indicating which trace flag is to be set
                    0 - Watch instance creation/deletion
                    1 - Watch slot changes to instances
                 2) The value to which to set the trace flags
                 3) A list of expressions containing the names
                    of the classes for which to set traces
  RETURNS      : TRUE if all OK, FALSE otherwise
  SIDE EFFECTS : Watch flags set in specified classes
  NOTES        : Accessory function for AddWatchItem()
 ******************************************************************/
globle BOOLEAN DefclassWatchAccess(
  int code,
  int newState,
  EXPRESSION *argExprs)
  {
   if (code)
     return(ConstructSetWatchAccess(DefclassConstruct,newState,argExprs,
                                    GetDefclassWatchSlots,SetDefclassWatchSlots));
   else
     return(ConstructSetWatchAccess(DefclassConstruct,newState,argExprs,
                                    GetDefclassWatchInstances,SetDefclassWatchInstances));
  }

/***********************************************************************
  NAME         : DefclassWatchPrint
  DESCRIPTION  : Parses a list of class names passed by
                 AddWatchItem() and displays the traces accordingly
  INPUTS       : 1) The logical name of the output
                 2) A code indicating which trace flag is to be examined
                    0 - Watch instance creation/deletion
                    1 - Watch slot changes to instances
                 3) A list of expressions containing the names
                    of the classes for which to examine traces
  RETURNS      : TRUE if all OK, FALSE otherwise
  SIDE EFFECTS : Watch flags displayed for specified classes
  NOTES        : Accessory function for AddWatchItem()
 ***********************************************************************/
globle BOOLEAN DefclassWatchPrint(
  char *log,
  int code,
  EXPRESSION *argExprs)
  {
   if (code)
     return(ConstructPrintWatchAccess(DefclassConstruct,log,argExprs,
                                      GetDefclassWatchSlots,SetDefclassWatchSlots));
   else
     return(ConstructPrintWatchAccess(DefclassConstruct,log,argExprs,
                                      GetDefclassWatchInstances,SetDefclassWatchInstances));
  }

#endif

/*********************************************************
  NAME         : GetDefclassListFunction
  DESCRIPTION  : Groups names of all defclasses into
                   a multifield variable
  INPUTS       : A data object buffer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Multifield set to list of classes
  NOTES        : None
 *********************************************************/
globle void GetDefclassListFunction(
  DATA_OBJECT_PTR returnValue)
  {
   GetConstructListFunction("get-defclass-list",returnValue,DefclassConstruct);
  }

/***************************************************************
  NAME         : GetDefclassList
  DESCRIPTION  : Groups all defclass names into
                 a multifield list
  INPUTS       : 1) A data object buffer to hold
                    the multifield result
                 2) The module from which to obtain defclasses
  RETURNS      : Nothing useful
  SIDE EFFECTS : Multifield allocated and filled
  NOTES        : External C access
 ***************************************************************/
globle void GetDefclassList(
  DATA_OBJECT *returnValue,
  struct defmodule *theModule)
  {
   GetConstructList(returnValue,DefclassConstruct,theModule);
  }

/*****************************************************
  NAME         : HasSuperclass
  DESCRIPTION  : Determines if class-2 is a superclass
                   of class-1
  INPUTS       : 1) Class-1
                 2) Class-2
  RETURNS      : TRUE if class-2 is a superclass of
                   class-1, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 *****************************************************/
globle int HasSuperclass(
  DEFCLASS *c1,
  DEFCLASS *c2)
  {
   register unsigned i;

   for (i = 1 ; i < c1->allSuperclasses.classCount ; i++)
     if (c1->allSuperclasses.classArray[i] == c2)
       return(TRUE);
   return(FALSE);
  }

/********************************************************************
  NAME         : CheckClassAndSlot
  DESCRIPTION  : Checks class and slot argument for various functions
  INPUTS       : 1) Name of the calling function
                 2) Buffer for class address
  RETURNS      : Slot symbol, NULL on errors
  SIDE EFFECTS : None
  NOTES        : None
 ********************************************************************/
globle SYMBOL_HN *CheckClassAndSlot(
   char *func,
   DEFCLASS **cls)
  {
   DATA_OBJECT temp;

   if (ArgTypeCheck(func,1,SYMBOL,&temp) == FALSE)
     return(NULL);
   *cls = LookupDefclassByMdlOrScope(DOToString(temp));
   if (*cls == NULL)
     {
      ClassExistError(func,DOToString(temp));
      return(NULL);
     }
   if (ArgTypeCheck(func,2,SYMBOL,&temp) == FALSE)
     return(NULL);
   return((SYMBOL_HN *) GetValue(temp));
  }

#if (! BLOAD_ONLY) && (! RUN_TIME)

/***************************************************
  NAME         : SaveDefclasses
  DESCRIPTION  : Prints pretty print form of
                   defclasses to specified output
  INPUTS       : The  logical name of the output
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle void SaveDefclasses(
  char *logName)
  {
#if DEBUGGING_FUNCTIONS
   DoForAllConstructs(SaveDefclass,DefclassModuleIndex,FALSE,(void *) logName);
#endif
  }

#endif

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

#if (! BLOAD_ONLY) && (! RUN_TIME) && DEBUGGING_FUNCTIONS

/***************************************************
  NAME         : SaveDefclass
  DESCRIPTION  : Writes out the pretty-print forms
                 of a class and all its handlers
  INPUTS       : 1) The class
                 2) The logical name of the output
  RETURNS      : Nothing useful
  SIDE EFFECTS : Class and handlers written
  NOTES        : None
 ***************************************************/
static void SaveDefclass(
  struct constructHeader *theDefclass,
  void *userBuffer)
  {
   char *logName = (char *) userBuffer;
   unsigned hnd;
   char *ppForm;

   ppForm = GetDefclassPPForm((void *) theDefclass);
   if (ppForm != NULL)
     {
      PrintInChunks(logName,ppForm);
      PrintRouter(logName,"\n");
      hnd = GetNextDefmessageHandler((void *) theDefclass,0);
      while (hnd != 0)
        {
         ppForm = GetDefmessageHandlerPPForm((void *) theDefclass,hnd);
         if (ppForm != NULL)
           {
            PrintInChunks(logName,ppForm);
            PrintRouter(logName,"\n");
           }
         hnd = GetNextDefmessageHandler((void *) theDefclass,hnd);
        }
     }
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
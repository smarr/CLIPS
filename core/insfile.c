   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*              CLIPS Version 6.10  04/09/97           */
   /*                                                     */
   /*         INSTANCE LOAD/SAVE (ASCII/BINARY) MODULE    */
   /*******************************************************/

/*************************************************************/
/* Purpose:  File load/save routines for instances           */
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

#include "argacces.h"
#include "classcom.h"
#include "classfun.h"
#include "memalloc.h"
#include "extnfunc.h"
#include "inscom.h"
#include "insfun.h"
#include "insmngr.h"
#include "inspsr.h"
#include "object.h"
#include "router.h"
#include "strngrtr.h"
#include "symblbin.h"
#include "sysdep.h"

#if DEFTEMPLATE_CONSTRUCT && DEFRULE_CONSTRUCT
#include "factmngr.h"
#endif

#define _INSFILE_SOURCE_
#include "insfile.h"

extern Thread struct token ObjectParseToken;

/* =========================================
   *****************************************
                   CONSTANTS
   =========================================
   ***************************************** */
#define MAX_BLOCK_SIZE 10240

/* =========================================
   *****************************************
               MACROS AND TYPES
   =========================================
   ***************************************** */
struct bsaveSlotValue
  {
   long slotName;
   int valueCount;
  };

struct bsaveSlotValueAtom
  {
   int type;
   long value;
  };

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

static long InstancesSaveCommandParser(char *,long (*)(char *,int,
                                                   EXPRESSION *,BOOLEAN));
static DATA_OBJECT *ProcessSaveClassList(char *,EXPRESSION *,int,BOOLEAN);
static void ReturnSaveClassList(DATA_OBJECT *);
static long SaveOrMarkInstances(void *,int,DATA_OBJECT *,BOOLEAN,BOOLEAN,
                                         void (*)(void *,INSTANCE_TYPE *));
static long SaveOrMarkInstancesOfClass(void *,struct defmodule *,int,DEFCLASS *,
                                                BOOLEAN,int,void (*)(void *,INSTANCE_TYPE *));
static void SaveSingleInstanceText(void *,INSTANCE_TYPE *);
static void ProcessFileErrorMessage(char *,char *);
#if BSAVE_INSTANCES
static void WriteBinaryHeader(FILE *);
static void MarkSingleInstance(void *,INSTANCE_TYPE *);
static void MarkNeededAtom(int,void *);
static void SaveSingleInstanceBinary(void *,INSTANCE_TYPE *);
static void SaveAtomBinary(int,void *,FILE *);
#endif

static long LoadOrRestoreInstances(char *,int,int);

#if BLOAD_INSTANCES
static BOOLEAN VerifyBinaryHeader(char *);
static BOOLEAN LoadSingleBinaryInstance(void);
static void BinaryLoadInstanceError(SYMBOL_HN *,DEFCLASS *);
static void CreateSlotValue(DATA_OBJECT *,struct bsaveSlotValueAtom *,long); /* 6.04 Bug Fix */
static void *GetBinaryAtomValue(struct bsaveSlotValueAtom *);
static void BufferedRead(void *,unsigned long);
static void FreeReadBuffer(void);
#endif

/* =========================================
   *****************************************
       INTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */

#if BLOAD_INSTANCES || BSAVE_INSTANCES

Thread static char *InstanceBinaryPrefixID = "\5\6\7CLIPS";
Thread static char *InstanceBinaryVersionID = "V6.00";
Thread static unsigned long BinaryInstanceFileSize;

#if BLOAD_INSTANCES
Thread static unsigned long BinaryInstanceFileOffset;
Thread static char *CurrentReadBuffer = NULL;
Thread static unsigned long CurrentReadBufferSize = 0L;
#endif

#endif

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

#if (! RUN_TIME)

/***************************************************
  NAME         : SetupInstanceFileCommands
  DESCRIPTION  : Defines function interfaces for
                 saving instances to files
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Functions defined to KB
  NOTES        : None
 ***************************************************/
globle void SetupInstanceFileCommands()
  {
   DefineFunction2("save-instances",'l',PTIF SaveInstancesCommand,
                   "SaveInstancesCommand","1*wk");
   DefineFunction2("load-instances",'l',PTIF LoadInstancesCommand,
                   "LoadInstancesCommand","11k");
   DefineFunction2("restore-instances",'l',PTIF RestoreInstancesCommand,
                   "RestoreInstancesCommand","11k");

#if BSAVE_INSTANCES
   DefineFunction2("bsave-instances",'l',PTIF BinarySaveInstancesCommand,
                   "BinarySaveInstancesCommand","1*wk");
#endif
#if BLOAD_INSTANCES
   DefineFunction2("bload-instances",'l',PTIF BinaryLoadInstancesCommand,
                   "BinaryLoadInstancesCommand","11k");
#endif
  }

#endif

/****************************************************************************
  NAME         : SaveInstancesCommand
  DESCRIPTION  : H/L interface for saving
                   current instances to a file
  INPUTS       : None
  RETURNS      : The number of instances saved
  SIDE EFFECTS : Instances saved to named file
  NOTES        : H/L Syntax :
                 (save-instances <file> [local|visible [[inherit] <class>+]])
 ****************************************************************************/
globle long SaveInstancesCommand()
  {
   return(InstancesSaveCommandParser("save-instances",SaveInstances));
  }

/******************************************************
  NAME         : LoadInstancesCommand
  DESCRIPTION  : H/L interface for loading
                   instances from a file
  INPUTS       : None
  RETURNS      : The number of instances loaded
  SIDE EFFECTS : Instances loaded from named file
  NOTES        : H/L Syntax : (load-instances <file>)
 ******************************************************/
globle long LoadInstancesCommand()
  {
   char *fileFound;
   DATA_OBJECT temp;
   long instanceCount;

   if (ArgTypeCheck("load-instances",1,SYMBOL_OR_STRING,&temp) == FALSE)
     return(0L);

   fileFound = DOToString(temp);

   instanceCount = LoadInstances(fileFound);
   if (EvaluationError)
     ProcessFileErrorMessage("load-instances",fileFound);
   return(instanceCount);
  }

/***************************************************
  NAME         : LoadInstances
  DESCRIPTION  : Loads instances from named file
  INPUTS       : The name of the input file
  RETURNS      : The number of instances loaded
  SIDE EFFECTS : Instances loaded from file
  NOTES        : None
 ***************************************************/
globle long LoadInstances(
  char *file)
  {
   return(LoadOrRestoreInstances(file,TRUE,TRUE));
  }

/***************************************************
  NAME         : LoadInstancesFromString
  DESCRIPTION  : Loads instances from given string
  INPUTS       : 1) The input string
                 2) Index of char in string after
                    last valid char (-1 for all chars)
  RETURNS      : The number of instances loaded
  SIDE EFFECTS : Instances loaded from string
  NOTES        : Uses string routers
 ***************************************************/
globle long LoadInstancesFromString(
  char *theString,
  int theMax)
  {
   long theCount;
   char * theStrRouter = "*** load-instances-from-string ***";

   if ((theMax == -1) ? (!OpenStringSource(theStrRouter,theString,0)) :
                        (!OpenTextSource(theStrRouter,theString,0,theMax)))
     return(-1L);
   theCount = LoadOrRestoreInstances(theStrRouter,TRUE,FALSE);
   CloseStringSource(theStrRouter);
   return(theCount);
  }

/*********************************************************
  NAME         : RestoreInstancesCommand
  DESCRIPTION  : H/L interface for loading
                   instances from a file w/o messages
  INPUTS       : None
  RETURNS      : The number of instances restored
  SIDE EFFECTS : Instances loaded from named file
  NOTES        : H/L Syntax : (restore-instances <file>)
 *********************************************************/
globle long RestoreInstancesCommand()
  {
   char *fileFound;
   DATA_OBJECT temp;
   long instanceCount;

   if (ArgTypeCheck("restore-instances",1,SYMBOL_OR_STRING,&temp) == FALSE)
     return(0L);

   fileFound = DOToString(temp);

   instanceCount = RestoreInstances(fileFound);
   if (EvaluationError)
     ProcessFileErrorMessage("restore-instances",fileFound);
   return(instanceCount);
  }

/***************************************************
  NAME         : RestoreInstances
  DESCRIPTION  : Restores instances from named file
  INPUTS       : The name of the input file
  RETURNS      : The number of instances restored
  SIDE EFFECTS : Instances restored from file
  NOTES        : None
 ***************************************************/
globle long RestoreInstances(
  char *file)
  {
   return(LoadOrRestoreInstances(file,FALSE,TRUE));
  }

/***************************************************
  NAME         : RestoreInstancesFromString
  DESCRIPTION  : Restores instances from given string
  INPUTS       : 1) The input string
                 2) Index of char in string after
                    last valid char (-1 for all chars)
  RETURNS      : The number of instances loaded
  SIDE EFFECTS : Instances loaded from string
  NOTES        : Uses string routers
 ***************************************************/
globle long RestoreInstancesFromString(
  char *theString,
  int theMax)
  {
   long theCount;
   char * theStrRouter = "*** load-instances-from-string ***";

   if ((theMax == -1) ? (!OpenStringSource(theStrRouter,theString,0)) :
                        (!OpenTextSource(theStrRouter,theString,0,theMax)))
     return(-1L);
   theCount = LoadOrRestoreInstances(theStrRouter,FALSE,FALSE);
   CloseStringSource(theStrRouter);
   return(theCount);
  }

#if BLOAD_INSTANCES

/*******************************************************
  NAME         : BinaryLoadInstancesCommand
  DESCRIPTION  : H/L interface for loading
                   instances from a binary file
  INPUTS       : None
  RETURNS      : The number of instances loaded
  SIDE EFFECTS : Instances loaded from named binary file
  NOTES        : H/L Syntax : (bload-instances <file>)
 *******************************************************/
globle long BinaryLoadInstancesCommand()
  {
   char *fileFound;
   DATA_OBJECT temp;
   long instanceCount;

   if (ArgTypeCheck("bload-instances",1,SYMBOL_OR_STRING,&temp) == FALSE)
     return(0L);

   fileFound = DOToString(temp);

   instanceCount = BinaryLoadInstances(fileFound);
   if (EvaluationError)
     ProcessFileErrorMessage("bload-instances",fileFound);
   return(instanceCount);
  }

/****************************************************
  NAME         : BinaryLoadInstances
  DESCRIPTION  : Loads instances quickly from a
                 binary file
  INPUTS       : The file name
  RETURNS      : The number of instances loaded
  SIDE EFFECTS : Instances loaded w/o message-passing
  NOTES        : None
 ****************************************************/
globle long BinaryLoadInstances(
  char *theFile)
  {
   long i,instanceCount;

   if (GenOpen("bload-instances",theFile) == 0)
     {
      SetEvaluationError(TRUE);
      return(-1L);
     }
   if (VerifyBinaryHeader(theFile) == FALSE)
     {
      GenClose();
      SetEvaluationError(TRUE);
      return(-1L);
     }
   ReadNeededAtomicValues();

   BinaryInstanceFileOffset = 0L;

   GenRead((void *) &BinaryInstanceFileSize,(unsigned long) sizeof(unsigned long));
   GenRead((void *) &instanceCount,(unsigned long) sizeof(long));

   for (i = 0L ; i < instanceCount ; i++)
     {
      if (LoadSingleBinaryInstance() == FALSE)
        {
         FreeReadBuffer();
         FreeAtomicValueStorage();
         GenClose();
         SetEvaluationError(TRUE);
         return(i);
        }
     }

   FreeReadBuffer();
   FreeAtomicValueStorage();
   GenClose();

   return(instanceCount);
  }

#endif

/*******************************************************
  NAME         : SaveInstances
  DESCRIPTION  : Saves current instances to named file
  INPUTS       : 1) The name of the output file
                 2) A flag indicating whether to
                    save local (current module only)
                    or visible instances
                    LOCAL_SAVE or VISIBLE_SAVE
                 3) A list of expressions containing
                    the names of classes for which
                    instances are to be saved
                 4) A flag indicating if the subclasses
                    of specified classes shoudl also
                    be processed
  RETURNS      : The number of instances saved
  SIDE EFFECTS : Instances saved to file
  NOTES        : None
 *******************************************************/
globle long SaveInstances(
  char *file,
  int saveCode,
  EXPRESSION *classExpressionList,
  BOOLEAN inheritFlag)
  {
   FILE *sfile = NULL;
   int oldPEC,oldATS,oldIAN;
   DATA_OBJECT *classList;
   long instanceCount;

   classList = ProcessSaveClassList("save-instances",classExpressionList,
                                    saveCode,inheritFlag);
   if ((classList == NULL) && (classExpressionList != NULL))
     return(0L);

   SaveOrMarkInstances((void *) sfile,saveCode,classList,
                             inheritFlag,TRUE,NULL);

   if ((sfile = fopen(file,"w")) == NULL)
     {
      OpenErrorMessage("save-instances",file);
      ReturnSaveClassList(classList);
      SetEvaluationError(TRUE);
      return(0L);
     }

   oldPEC = PreserveEscapedCharacters;
   PreserveEscapedCharacters = TRUE;
   oldATS = AddressesToStrings;
   AddressesToStrings = TRUE;
   oldIAN = InstanceAddressesToNames;
   InstanceAddressesToNames = TRUE;

   SetFastSave(sfile);
   instanceCount = SaveOrMarkInstances((void *) sfile,saveCode,classList,
                                       inheritFlag,TRUE,SaveSingleInstanceText);
   fclose(sfile);
   SetFastSave(NULL);

   PreserveEscapedCharacters = oldPEC;
   AddressesToStrings = oldATS;
   InstanceAddressesToNames = oldIAN;
   ReturnSaveClassList(classList);
   return(instanceCount);
  }

#if BSAVE_INSTANCES

/****************************************************************************
  NAME         : BinarySaveInstancesCommand
  DESCRIPTION  : H/L interface for saving
                   current instances to a binary file
  INPUTS       : None
  RETURNS      : The number of instances saved
  SIDE EFFECTS : Instances saved (in binary format) to named file
  NOTES        : H/L Syntax :
                 (bsave-instances <file> [local|visible [[inherit] <class>+]])
 *****************************************************************************/
globle long BinarySaveInstancesCommand()
  {
   return(InstancesSaveCommandParser("bsave-instances",BinarySaveInstances));
  }

/*******************************************************
  NAME         : BinarySaveInstances
  DESCRIPTION  : Saves current instances to binary file
  INPUTS       : 1) The name of the output file
                 2) A flag indicating whether to
                    save local (current module only)
                    or visible instances
                    LOCAL_SAVE or VISIBLE_SAVE
                 3) A list of expressions containing
                    the names of classes for which
                    instances are to be saved
                 4) A flag indicating if the subclasses
                    of specified classes shoudl also
                    be processed
  RETURNS      : The number of instances saved
  SIDE EFFECTS : Instances saved to file
  NOTES        : None
 *******************************************************/
globle long BinarySaveInstances(
  char *file,
  int saveCode,
  EXPRESSION *classExpressionList,
  BOOLEAN inheritFlag)
  {
   DATA_OBJECT *classList;
   FILE *bsaveFP;
   long instanceCount;

   classList = ProcessSaveClassList("bsave-instances",classExpressionList,
                                    saveCode,inheritFlag);
   if ((classList == NULL) && (classExpressionList != NULL))
     return(0L);

   BinaryInstanceFileSize = 0L;
   InitAtomicValueNeededFlags();
   instanceCount = SaveOrMarkInstances(NULL,saveCode,classList,inheritFlag,
                                       FALSE,MarkSingleInstance);

   if ((bsaveFP = fopen(file,"wb")) == NULL)
     {
      OpenErrorMessage("bsave-instances",file);
      ReturnSaveClassList(classList);
      SetEvaluationError(TRUE);
      return(0L);
     }
   WriteBinaryHeader(bsaveFP);
   WriteNeededAtomicValues(bsaveFP);

   fwrite((void *) &BinaryInstanceFileSize,sizeof(unsigned long),1,bsaveFP);
   fwrite((void *) &instanceCount,sizeof(long),1,bsaveFP);

   SetAtomicValueIndices(FALSE);
   SaveOrMarkInstances((void *) bsaveFP,saveCode,classList,
                       inheritFlag,FALSE,SaveSingleInstanceBinary);
   RestoreAtomicValueBuckets();
   fclose(bsaveFP);
   ReturnSaveClassList(classList);
   return(instanceCount);
  }

#endif

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/******************************************************
  NAME         : InstancesSaveCommandParser
  DESCRIPTION  : Argument parser for save-instances
                 and bsave-instances
  INPUTS       : 1) The name of the calling function
                 2) A pointer to the support
                    function to call for the save/bsave
  RETURNS      : The number of instances saved
  SIDE EFFECTS : Instances saved/bsaved
  NOTES        : None
 ******************************************************/
static long InstancesSaveCommandParser(
  char *functionName,
  long (*saveFunction)(char *,int,EXPRESSION *,BOOLEAN))
  {
   char *fileFound;
   DATA_OBJECT temp;
   int argCount,saveCode = LOCAL_SAVE;
   EXPRESSION *classList = NULL;
   BOOLEAN inheritFlag = FALSE;

   if (ArgTypeCheck(functionName,1,SYMBOL_OR_STRING,&temp) == FALSE)
     return(0L);
   fileFound = DOToString(temp);

   argCount = RtnArgCount();
   if (argCount > 1)
     {
      if (ArgTypeCheck(functionName,2,SYMBOL,&temp) == FALSE)
        {
         ExpectedTypeError1(functionName,2,"symbol \"local\" or \"visible\"");
         SetEvaluationError(TRUE);
         return(0L);
        }
      if (strcmp(DOToString(temp),"local") == 0)
        saveCode = LOCAL_SAVE;
      else if (strcmp(DOToString(temp),"visible") == 0)
        saveCode = VISIBLE_SAVE;
      else
        {
         ExpectedTypeError1(functionName,2,"symbol \"local\" or \"visible\"");
         SetEvaluationError(TRUE);
         return(0L);
        }
      classList = GetFirstArgument()->nextArg->nextArg;

      /* ===========================
         Check for "inherit" keyword
         Must be at least one class
         name following
         =========================== */
      if ((classList != NULL) ? (classList->nextArg != NULL) : FALSE)
        {
         if ((classList->type != SYMBOL) ? FALSE :
             (strcmp(ValueToString(classList->value),"inherit") == 0))
           {
            inheritFlag = TRUE;
            classList = classList->nextArg;
           }
        }
     }

   return((*saveFunction)(fileFound,saveCode,classList,inheritFlag));
  }

/****************************************************
  NAME         : ProcessSaveClassList
  DESCRIPTION  : Evaluates a list of class name
                 expressions and stores them in a
                 data object list
  INPUTS       : 1) The name of the calling function
                 2) The class expression list
                 3) A flag indicating if only local
                    or all visible instances are
                    being saved
                 4) A flag indicating if inheritance
                    relationships should be checked
                    between classes
  RETURNS      : The evaluated class pointer data
                 objects - NULL on errors
  SIDE EFFECTS : Data objects allocated and
                 classes validated
  NOTES        : None
 ****************************************************/
static DATA_OBJECT *ProcessSaveClassList(
  char *functionName,
  EXPRESSION *classExps,
  int saveCode,
  BOOLEAN inheritFlag)
  {
   DATA_OBJECT *head = NULL,*prv,*newItem,tmp;
   DEFCLASS *theDefclass;
   struct defmodule *currentModule;
   int argIndex = inheritFlag ? 4 : 3;

   currentModule = ((struct defmodule *) GetCurrentModule());
   while (classExps != NULL)
     {
      if (EvaluateExpression(classExps,&tmp))
        goto ProcessClassListError;
      if (tmp.type != SYMBOL)
        goto ProcessClassListError;
      if (saveCode == LOCAL_SAVE)
        theDefclass = LookupDefclassAnywhere(currentModule,DOToString(tmp));
      else
        theDefclass = LookupDefclassInScope(DOToString(tmp));
      if (theDefclass == NULL)
        goto ProcessClassListError;
      else if (theDefclass->abstract && (inheritFlag == FALSE))
        goto ProcessClassListError;
      prv = newItem = head;
      while (newItem != NULL)
        {
         if (newItem->value == (void *) theDefclass)
           goto ProcessClassListError;
         else if (inheritFlag)
           {
            if (HasSuperclass((DEFCLASS *) newItem->value,theDefclass) ||
                HasSuperclass(theDefclass,(DEFCLASS *) newItem->value))
             goto ProcessClassListError;
           }
         prv = newItem;
         newItem = newItem->next;
        }
      newItem = get_struct(dataObject);
      newItem->type = DEFCLASS_PTR;
      newItem->value = (void *) theDefclass;
      newItem->next = NULL;
      if (prv == NULL)
        head = newItem;
      else
        prv->next = newItem;
      argIndex++;
      classExps = classExps->nextArg;
     }
   return(head);

ProcessClassListError:
   ExpectedTypeError1(functionName,argIndex,
                      inheritFlag ? "valid class name" : "valid concrete class name");
   ReturnSaveClassList(head);
   SetEvaluationError(TRUE);
   return(NULL);
  }

/****************************************************
  NAME         : ReturnSaveClassList
  DESCRIPTION  : Deallocates the class data object
                 list created by ProcessSaveClassList
  INPUTS       : The class data object list
  RETURNS      : Nothing useful
  SIDE EFFECTS : Class data object returned
  NOTES        : None
 ****************************************************/
static void ReturnSaveClassList(
  DATA_OBJECT *classList)
  {
   DATA_OBJECT *tmp;

   while (classList != NULL)
     {
      tmp = classList;
      classList = classList->next;
      rtn_struct(dataObject,tmp);
     }
  }

/***************************************************
  NAME         : SaveOrMarkInstances
  DESCRIPTION  : Iterates through all specified
                 instances either marking needed
                 atoms or writing instances in
                 binary/text format
  INPUTS       : 1) NULL (for marking),
                    logical name (for text saves)
                    file pointer (for binary saves)
                 2) A cope flag indicating LOCAL
                    or VISIBLE saves only
                 3) A list of data objects
                    containing the names of classes
                    of instances to be saved
                 4) A flag indicating whether to
                    include subclasses of arg #3
                 5) A flag indicating if the
                    iteration can be interrupted
                    or not
                 6) The access function to mark
                    or save an instance (can be NULL
                    if only counting instances)
  RETURNS      : The number of instances saved
  SIDE EFFECTS : Instances amrked or saved
  NOTES        : None
 ***************************************************/
static long SaveOrMarkInstances(
  void *theOutput,
  int saveCode,
  DATA_OBJECT *classList,
  BOOLEAN inheritFlag,
  BOOLEAN interruptOK,
  void (*saveInstanceFunc)(void *,INSTANCE_TYPE *))
  {
   struct defmodule *currentModule;
   int traversalID;
   DATA_OBJECT *tmp;
   INSTANCE_TYPE *ins;
   long instanceCount = 0L;

   currentModule = ((struct defmodule *) GetCurrentModule());
   if (classList != NULL)
     {
      traversalID = GetTraversalID();
      if (traversalID != -1)
        {
         for (tmp = classList ;
              (! ((tmp == NULL) || (HaltExecution && interruptOK))) ;
              tmp = tmp->next)
           instanceCount += SaveOrMarkInstancesOfClass(theOutput,currentModule,saveCode,
                                                       (DEFCLASS *) tmp->value,inheritFlag,
                                                       traversalID,saveInstanceFunc);
         ReleaseTraversalID();
        }
     }
   else
     {
      for (ins = (INSTANCE_TYPE *) GetNextInstanceInScope(NULL) ;
           (ins != NULL) && (HaltExecution != TRUE) ;
           ins = (INSTANCE_TYPE *) GetNextInstanceInScope((void *) ins))
        {
         if ((saveCode == VISIBLE_SAVE) ? TRUE :
             (ins->cls->header.whichModule->theModule == currentModule))
           {
            if (saveInstanceFunc != NULL)
              (*saveInstanceFunc)(theOutput,ins);
            instanceCount++;
           }
        }
     }
   return(instanceCount);
  }

/***************************************************
  NAME         : SaveOrMarkInstancesOfClass
  DESCRIPTION  : Saves off the direct (and indirect)
                 instance of the specified class
  INPUTS       : 1) The logical name of the output
                    (or file pointer for binary
                     output)
                 2) The current module
                 3) A flag indicating local
                    or visible saves
                 4) The defclass
                 5) A flag indicating whether to
                    save subclass instances or not
                 6) A traversal id for marking
                    visited classes
                 7) A pointer to the instance
                    manipulation function to call
                    (can be NULL for only counting
                     instances)
  RETURNS      : The number of instances saved
  SIDE EFFECTS : Appropriate instances saved
  NOTES        : None
 ***************************************************/
static long SaveOrMarkInstancesOfClass(
  void *theOutput,
  struct defmodule *currentModule,
  int saveCode,
  DEFCLASS *theDefclass,
  BOOLEAN inheritFlag,
  int traversalID,
  void (*saveInstanceFunc)(void *,INSTANCE_TYPE *))
  {
   INSTANCE_TYPE *theInstance;
   DEFCLASS *subclass;
   register unsigned i;
   long instanceCount = 0L;

   if (TestTraversalID(theDefclass->traversalRecord,traversalID))
     return(instanceCount);
   SetTraversalID(theDefclass->traversalRecord,traversalID);
   if (((saveCode == LOCAL_SAVE) &&
        (theDefclass->header.whichModule->theModule == currentModule)) ||
       ((saveCode == VISIBLE_SAVE) &&
        DefclassInScope(theDefclass,currentModule)))
     {
      for (theInstance = (INSTANCE_TYPE *)
             GetNextInstanceInClass((void *) theDefclass,NULL) ;
           theInstance != NULL ;
           theInstance = (INSTANCE_TYPE *)
           GetNextInstanceInClass((void *) theDefclass,(void *) theInstance))
        {
         if (saveInstanceFunc != NULL)
           (*saveInstanceFunc)(theOutput,theInstance);
         instanceCount++;
        }
     }
   if (inheritFlag)
     {
      for (i = 0 ; i < theDefclass->directSubclasses.classCount ; i++)
        {
         subclass = theDefclass->directSubclasses.classArray[i];
           instanceCount += SaveOrMarkInstancesOfClass(theOutput,currentModule,saveCode,
                                                       subclass,TRUE,traversalID,
                                                       saveInstanceFunc);
        }
     }
   return(instanceCount);
  }

/***************************************************
  NAME         : SaveSingleInstanceText
  DESCRIPTION  : Writes given instance to file
  INPUTS       : 1) The logical name of the output
                 2) The instance to save
  RETURNS      : Nothing useful
  SIDE EFFECTS : Instance written
  NOTES        : None
 ***************************************************/
static void SaveSingleInstanceText(
  void *vLogicalName,
  INSTANCE_TYPE *theInstance)
  {
   register unsigned i;
   INSTANCE_SLOT *sp;
   char *logicalName = (char *) vLogicalName;

   PrintRouter(logicalName,"([");
   PrintRouter(logicalName,ValueToString(theInstance->name));
   PrintRouter(logicalName,"] of ");
   PrintRouter(logicalName,ValueToString(theInstance->cls->header.name));
   for (i = 0 ; i < theInstance->cls->instanceSlotCount ; i++)
     {
      sp = theInstance->slotAddresses[i];
      PrintRouter(logicalName,"\n   (");
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
   PrintRouter(logicalName,")\n\n");
  }

#if BSAVE_INSTANCES

/***************************************************
  NAME         : WriteBinaryHeader
  DESCRIPTION  : Writes identifying string to
                 instance binary file to assist in
                 later verification
  INPUTS       : The binary file pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Binary prefix headers written
  NOTES        : None
 ***************************************************/
static void WriteBinaryHeader(
  FILE *bsaveFP)
  {
   fwrite((void *) InstanceBinaryPrefixID,
          (CLIPS_STD_SIZE) (strlen(InstanceBinaryPrefixID) + 1),1,bsaveFP);
   fwrite((void *) InstanceBinaryVersionID,
          (CLIPS_STD_SIZE) (strlen(InstanceBinaryVersionID) + 1),1,bsaveFP);
  }

/***************************************************
  NAME         : MarkSingleInstance
  DESCRIPTION  : Marks all the atoms needed in
                 the slot values of an instance
  INPUTS       : 1) The output (ignored)
                 2) The instance
  RETURNS      : Nothing useful
  SIDE EFFECTS : Instance slot value atoms marked
  NOTES        : None
 ***************************************************/
#if IBM_TBC
#pragma argsused
#endif
static void MarkSingleInstance(
  void *theOutput,
  INSTANCE_TYPE *theInstance)
  {
#if MAC_MPW || MAC_MCW || IBM_MCW
#pragma unused(theOutput)
#endif
   INSTANCE_SLOT *sp;
   register unsigned i,j;

   BinaryInstanceFileSize += (unsigned long) (sizeof(long) * 2);
   theInstance->name->neededSymbol = TRUE;
   theInstance->cls->header.name->neededSymbol = TRUE;
   BinaryInstanceFileSize +=
       (unsigned long) ((sizeof(long) * 2) +
                        (sizeof(struct bsaveSlotValue) *
                         theInstance->cls->instanceSlotCount) +
                        sizeof(unsigned long) +
                        sizeof(unsigned));
   for (i = 0 ; i < theInstance->cls->instanceSlotCount ; i++)
     {
      sp = theInstance->slotAddresses[i];
      sp->desc->slotName->name->neededSymbol = TRUE;
      if (sp->desc->multiple)
        {
         for (j = 1 ; j <= GetInstanceSlotLength(sp) ; j++)
           MarkNeededAtom(GetMFType(sp->value,j),GetMFValue(sp->value,j));
        }
      else
        MarkNeededAtom((int) sp->type,sp->value);
     }
  }

/***************************************************
  NAME         : MarkNeededAtom
  DESCRIPTION  : Marks an integer/float/symbol as
                 being need by a set of instances
  INPUTS       : 1) The type of atom
                 2) The value of the atom
  RETURNS      : Nothing useful
  SIDE EFFECTS : Atom marked for saving
  NOTES        : None
 ***************************************************/
static void MarkNeededAtom(
  int type,
  void *value)
  {
   BinaryInstanceFileSize += (unsigned long) sizeof(struct bsaveSlotValueAtom);

   /* =====================================
      Assumes slot value atoms  can only be
      floats, integers, symbols, strings,
      instance-names, instance-addresses,
      fact-addresses or external-addresses
      ===================================== */
   switch (type)
     {
      case SYMBOL:
      case STRING:
      case INSTANCE_NAME:
         ((SYMBOL_HN *) value)->neededSymbol = TRUE;
         break;
      case FLOAT:
         ((FLOAT_HN *) value)->neededFloat = TRUE;
         break;
      case INTEGER:
         ((INTEGER_HN *) value)->neededInteger = TRUE;
         break;
      case INSTANCE_ADDRESS:
         GetFullInstanceName((INSTANCE_TYPE *) value)->neededSymbol = TRUE;
         break;
     }
  }

/****************************************************
  NAME         : SaveSingleInstanceBinary
  DESCRIPTION  : Writes given instance to binary file
  INPUTS       : 1) Binary file pointer
                 2) The instance to save
  RETURNS      : Nothing useful
  SIDE EFFECTS : Instance written
  NOTES        : None
 ****************************************************/
static void SaveSingleInstanceBinary(
  void *vBsaveFP,
  INSTANCE_TYPE *theInstance)
  {
   long nameIndex;
   register unsigned i,j;
   INSTANCE_SLOT *sp;
   FILE *bsaveFP = (FILE *) vBsaveFP;
   struct bsaveSlotValue bs;
   unsigned long totalValueCount = 0L;
   int slotLen;

   /* ===========================
      Write out the instance name
      =========================== */
   nameIndex = (long) theInstance->name->bucket;
   fwrite((void *) &nameIndex,(int) sizeof(long),1,bsaveFP);

   /* ========================
      Write out the class name
      ======================== */
   nameIndex = (long) theInstance->cls->header.name->bucket;
   fwrite((void *) &nameIndex,(int) sizeof(long),1,bsaveFP);

   /* ======================================
      Write out the number of slot-overrides
      ====================================== */
   fwrite((void *) &theInstance->cls->instanceSlotCount,
          (int) sizeof(unsigned),1,bsaveFP);

   /* =========================================
      Write out the slot names and value counts
      ========================================= */
   for (i = 0 ; i < theInstance->cls->instanceSlotCount ; i++)
     {
      sp = theInstance->slotAddresses[i];

      /* ===============================================
         Write out the number of atoms in the slot value
         =============================================== */
      bs.slotName = (long) sp->desc->slotName->name->bucket;
      bs.valueCount = sp->desc->multiple ? GetInstanceSlotLength(sp) : 1;
      fwrite((void *) &bs,(int) sizeof(struct bsaveSlotValue),1,bsaveFP);
      totalValueCount += (unsigned long) bs.valueCount;
     }

   /* ==================================
      Write out the number of slot value
      atoms for the whole instance
      ================================== */
   if (totalValueCount != 0L)
     fwrite((void *) &totalValueCount,(int) sizeof(unsigned long),1,bsaveFP);

   /* ==============================
      Write out the slot value atoms
      ============================== */
   for (i = 0 ; i < theInstance->cls->instanceSlotCount ; i++)
     {
      sp = theInstance->slotAddresses[i];
      slotLen = sp->desc->multiple ? GetInstanceSlotLength(sp) : 1;

      /* =========================================
         Write out the type and index of each atom
         ========================================= */
      if (sp->desc->multiple)
        {
         for (j = 1 ; j <= slotLen ; j++)
           SaveAtomBinary(GetMFType(sp->value,j),GetMFValue(sp->value,j),bsaveFP);
        }
      else
        SaveAtomBinary((int) sp->type,sp->value,bsaveFP);
     }
  }

/***************************************************
  NAME         : SaveAtomBinary
  DESCRIPTION  : Writes out an instance slot value
                 atom to the binary file
  INPUTS       : 1) The atom type
                 2) The atom value
                 3) The binary file pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : atom written
  NOTES        :
 ***************************************************/
static void SaveAtomBinary(
  int type,
  void *value,
  FILE *bsaveFP)
  {
   struct bsaveSlotValueAtom bsa;

   /* =====================================
      Assumes slot value atoms  can only be
      floats, integers, symbols, strings,
      instance-names, instance-addresses,
      fact-addresses or external-addresses
      ===================================== */
   bsa.type = type;
   switch (type)
     {
      case SYMBOL:
      case STRING:
      case INSTANCE_NAME:
         bsa.value = (long) ((SYMBOL_HN *) value)->bucket;
         break;
      case FLOAT:
         bsa.value = (long) ((FLOAT_HN *) value)->bucket;
         break;
      case INTEGER:
         bsa.value = (long) ((INTEGER_HN *) value)->bucket;
         break;
      case INSTANCE_ADDRESS:
         bsa.type = INSTANCE_NAME;
         bsa.value = (long) GetFullInstanceName((INSTANCE_TYPE *) value)->bucket;
         break;
      default:
         bsa.value = -1L;
     }
   fwrite((void *) &bsa,(int) sizeof(struct bsaveSlotValueAtom),1,bsaveFP);
  }

#endif

/**********************************************************************
  NAME         : LoadOrRestoreInstances
  DESCRIPTION  : Loads instances from named file
  INPUTS       : 1) The name of the input file
                 2) An integer flag indicating whether or
                    not to use message-passing to create
                    the new instances and delete old versions
                 3) An integer flag indicating if arg #1
                    is a file name or the name of a string router
  RETURNS      : The number of instances loaded/restored
  SIDE EFFECTS : Instances loaded from file
  NOTES        : None
 **********************************************************************/
static long LoadOrRestoreInstances(
  char *file,
  int usemsgs,
  int isFileName)
  {
   DATA_OBJECT temp;
   FILE *sfile,*svload;
   char *ilog;
   EXPRESSION *top;
   int svoverride;
   long instanceCount = 0L;

   if (isFileName) {
     if ((sfile = fopen(file,"r")) == NULL)
       {
        SetEvaluationError(TRUE);
        return(-1L);
       }
     svload = GetFastLoad();
     ilog = (char *) sfile;
     SetFastLoad(sfile);
   } else {
     ilog = file;
   }
   top = GenConstant(FCALL,(void *) FindFunction("make-instance"));
   GetToken(ilog,&ObjectParseToken);
   svoverride = MkInsMsgPass;
   MkInsMsgPass = usemsgs;
   while ((GetType(ObjectParseToken) != STOP) && (HaltExecution != TRUE))
     {
      if (GetType(ObjectParseToken) != LPAREN)
        {
         SyntaxErrorMessage("instance definition");
         rtn_struct(expr,top);
         if (isFileName) {
           fclose(sfile);
           SetFastLoad(svload);
         }
         SetEvaluationError(TRUE);
         MkInsMsgPass = svoverride;
         return(instanceCount);
        }
      if (ParseSimpleInstance(top,ilog) == NULL)
        {
         if (isFileName) {
           fclose(sfile);
           SetFastLoad(svload);
         }
         MkInsMsgPass = svoverride;
         SetEvaluationError(TRUE);
         return(instanceCount);
        }
      ExpressionInstall(top);
      EvaluateExpression(top,&temp);
      ExpressionDeinstall(top);
      if (! EvaluationError)
        instanceCount++;
      ReturnExpression(top->argList);
      top->argList = NULL;
      GetToken(ilog,&ObjectParseToken);
     }
   rtn_struct(expr,top);
   if (isFileName) {
     fclose(sfile);
     SetFastLoad(svload);
   }
   MkInsMsgPass = svoverride;
   return(instanceCount);
  }

/***************************************************
  NAME         : ProcessFileErrorMessage
  DESCRIPTION  : Prints an error message when a
                 file containing text or binary
                 instances cannot be processed.
  INPUTS       : The name of the input file and the
                 function which opened it.
  RETURNS      : No value
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
static void ProcessFileErrorMessage(
  char *functionName,
  char *fileName)
  {
   PrintErrorID("INSFILE",1,FALSE);
   PrintRouter(WERROR,"Function ");
   PrintRouter(WERROR,functionName);
   PrintRouter(WERROR," could not completely process file ");
   PrintRouter(WERROR,fileName);
   PrintRouter(WERROR,".\n");
  }

#if BLOAD_INSTANCES

/*******************************************************
  NAME         : VerifyBinaryHeader
  DESCRIPTION  : Reads the prefix and version headers
                 from a file to verify that the
                 input is a valid binary instances file
  INPUTS       : The name of the file
  RETURNS      : TRUE if OK, FALSE otherwise
  SIDE EFFECTS : Input prefix and version read
  NOTES        : Assumes file already open with GenOpen
 *******************************************************/
static BOOLEAN VerifyBinaryHeader(
  char *theFile)
  {
   char buf[20];

   GenRead((void *) buf,(unsigned long) (strlen(InstanceBinaryPrefixID) + 1));
   if (strcmp(buf,InstanceBinaryPrefixID) != 0)
     {
      PrintErrorID("INSFILE",2,FALSE);
      PrintRouter(WERROR,theFile);
      PrintRouter(WERROR," file is not a binary instances file.\n");
      return(FALSE);
     }
   GenRead((void *) buf,(unsigned long) (strlen(InstanceBinaryVersionID) + 1));
   if (strcmp(buf,InstanceBinaryVersionID) != 0)
     {
      PrintErrorID("INSFILE",3,FALSE);
      PrintRouter(WERROR,theFile);
      PrintRouter(WERROR," file is not a compatible binary instances file.\n");
      return(FALSE);
     }
   return(TRUE);
  }

/***************************************************
  NAME         : LoadSingleBinaryInstance
  DESCRIPTION  : Reads the binary data for a new
                 instance and its slot values and
                 creates/initializes the instance
  INPUTS       : None
  RETURNS      : TRUE if all OK,
                 FALSE otherwise
  SIDE EFFECTS : Binary data read and instance
                 created
  NOTES        : Uses global GenRead()
 ***************************************************/
static BOOLEAN LoadSingleBinaryInstance()
  {
   SYMBOL_HN *instanceName,
             *className;
   unsigned slotCount;
   DEFCLASS *theDefclass;
   INSTANCE_TYPE *newInstance;
   struct bsaveSlotValue *bsArray;
   struct bsaveSlotValueAtom *bsaArray;
   long nameIndex;
   unsigned long totalValueCount;
   register unsigned i;
   unsigned long j;
   INSTANCE_SLOT *sp;
   DATA_OBJECT slotValue,junkValue;

   /* =====================
      Get the instance name
      ===================== */
   BufferedRead((void *) &nameIndex,(unsigned long) sizeof(long));
   instanceName = SymbolPointer(nameIndex);

   /* ==================
      Get the class name
      ================== */
   BufferedRead((void *) &nameIndex,(unsigned long) sizeof(long));
   className = SymbolPointer(nameIndex);

   /* ==================
      Get the slot count
      ================== */
   BufferedRead((void *) &slotCount,(unsigned long) sizeof(unsigned));

   /* =============================
      Make sure the defclass exists
      and check the slot count
      ============================= */
   theDefclass = LookupDefclassInScope(ValueToString(className));
   if (theDefclass == NULL)
     {
      ClassExistError("bload-instances",ValueToString(className));
      return(FALSE);
     }
   if (theDefclass->instanceSlotCount != slotCount)
     {
      BinaryLoadInstanceError(instanceName,theDefclass);
      return(FALSE);
     }

   /* ===================================
      Create the new unitialized instance
      =================================== */
   newInstance = BuildInstance(instanceName,theDefclass,FALSE);
   if (newInstance == NULL)
     {
      BinaryLoadInstanceError(instanceName,theDefclass);
      return(FALSE);
     }
   if (slotCount == 0)
     return(TRUE);

   /* ====================================
      Read all slot override info and slot
      value atoms into big arrays
      ==================================== */
   bsArray = (struct bsaveSlotValue *) gm2((int) (sizeof(struct bsaveSlotValue) * slotCount));
   BufferedRead((void *) bsArray,(unsigned long) (sizeof(struct bsaveSlotValue) * slotCount));

   BufferedRead((void *) &totalValueCount,(unsigned long) sizeof(unsigned long));

   if (totalValueCount != 0L)
     {
      bsaArray = (struct bsaveSlotValueAtom *)
                  gm3((long) (totalValueCount * sizeof(struct bsaveSlotValueAtom)));
      BufferedRead((void *) bsaArray,
                   (unsigned long) (totalValueCount * sizeof(struct bsaveSlotValueAtom)));
     }

   /* =========================
      Insert the values for the
      slot overrides
      ========================= */
   for (i = 0 , j = 0L ; i < slotCount ; i++)
     {
      /* ===========================================================
         Here is another check for the validity of the binary file -
         the order of the slots in the file should match the
         order in the class definition
         =========================================================== */
      sp = newInstance->slotAddresses[i];
      if (sp->desc->slotName->name != SymbolPointer(bsArray[i].slotName))
        goto LoadError;
      CreateSlotValue(&slotValue,(struct bsaveSlotValueAtom *) &bsaArray[j],
                      bsArray[i].valueCount);

      if (PutSlotValue(newInstance,sp,&slotValue,&junkValue,"bload-instances") == FALSE)
        goto LoadError;

      j += (unsigned long) bsArray[i].valueCount;
     }

   rm((void *) bsArray,(int) (sizeof(struct bsaveSlotValue) * slotCount));

   if (totalValueCount != 0L)
     rm3((void *) bsaArray,
         (long) (totalValueCount * sizeof(struct bsaveSlotValueAtom)));

   return(TRUE);

LoadError:
   BinaryLoadInstanceError(instanceName,theDefclass);
   QuashInstance(newInstance);
   rm((void *) bsArray,(int) (sizeof(struct bsaveSlotValue) * slotCount));
   rm3((void *) bsaArray,
       (long) (totalValueCount * sizeof(struct bsaveSlotValueAtom)));
   return(FALSE);
  }

/***************************************************
  NAME         : BinaryLoadInstanceError
  DESCRIPTION  : Prints out an error message when
                 an instance could not be
                 successfully loaded from a
                 binary file
  INPUTS       : 1) The instance name
                 2) The defclass
  RETURNS      : Nothing useful
  SIDE EFFECTS : Error message printed
  NOTES        : None
 ***************************************************/
static void BinaryLoadInstanceError(
  SYMBOL_HN *instanceName,
  DEFCLASS *theDefclass)
  {
   PrintErrorID("INSFILE",4,FALSE);
   PrintRouter(WERROR,"Function bload-instances unable to load instance [");
   PrintRouter(WERROR,ValueToString(instanceName));
   PrintRouter(WERROR,"] of class ");
   PrintClassName(WERROR,theDefclass,TRUE);
  }

/***************************************************
  NAME         : CreateSlotValue
  DESCRIPTION  : Creates a data object value from
                 the binary slot value atom data
  INPUTS       : 1) A data object buffer
                 2) The slot value atoms array
                 3) The number of values to put
                    in the data object
  RETURNS      : Nothing useful
  SIDE EFFECTS : Data object initialized
                 (if more than one value, a
                 multifield is created)
  NOTES        : None
 ***************************************************/
static void CreateSlotValue(
  DATA_OBJECT *result,
  struct bsaveSlotValueAtom *bsaValues,
  long valueCount)
  {
   register unsigned i;

   if (valueCount == 0)
     {
      result->type = MULTIFIELD;
      result->value = CreateMultifield(0L);
      result->begin = 0;
      result->end = -1;
     }
   else if (valueCount == 1)
     {
      result->type = bsaValues[0].type;
      result->value = GetBinaryAtomValue(&bsaValues[0]);
     }
   else
     {
      result->type = MULTIFIELD;
      result->value = CreateMultifield(valueCount);
      result->begin = 0;
      result->end = valueCount - 1;
      for (i = 1 ; i <= valueCount ; i++)
        {
         SetMFType(result->value,i,(short) bsaValues[i-1].type);
         SetMFValue(result->value,i,GetBinaryAtomValue(&bsaValues[i-1]));
        }
     }
  }

/***************************************************
  NAME         : GetBinaryAtomValue
  DESCRIPTION  : Uses the binary index of an atom
                 to find the ephemeris value
  INPUTS       : The binary type and index
  RETURNS      : The symbol/etc. pointer
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
static void *GetBinaryAtomValue(
  struct bsaveSlotValueAtom *ba)
  {
   switch (ba->type)
     {
      case SYMBOL:
      case STRING:
      case INSTANCE_NAME:
         return((void *) SymbolPointer(ba->value));
      case FLOAT:
         return((void *) FloatPointer(ba->value));
      case INTEGER:
         return((void *) IntegerPointer(ba->value));
      case FACT_ADDRESS:
#if DEFTEMPLATE_CONSTRUCT && DEFRULE_CONSTRUCT
         return((void *) &DummyFact);
#else
         return(NULL);
#endif
      case EXTERNAL_ADDRESS:
        return(NULL);
      default:
        {
         SystemError("INSFILE",1);
         ExitRouter(EXIT_FAILURE);
        }
     }
   return(NULL);
  }

/***************************************************
  NAME         : BufferedRead
  DESCRIPTION  : Reads data from binary file
                 (Larger blocks than requested size
                  may be read and buffered)
  INPUTS       : 1) The buffer
                 2) The buffer size
  RETURNS      : Nothing useful
  SIDE EFFECTS : Data stored in buffer
  NOTES        : None
 ***************************************************/
static void BufferedRead(
  void *buf,
  unsigned long bufsz)
  {
   Thread static unsigned long CurrentReadBufferOffset = 0L;
   unsigned long i,amountLeftToRead;

   if (CurrentReadBuffer != NULL)
     {
      amountLeftToRead = CurrentReadBufferSize - CurrentReadBufferOffset;
      if (bufsz <= amountLeftToRead)
        {
         for (i = 0L ; i < bufsz ; i++)
           ((char *) buf)[i] = CurrentReadBuffer[i + CurrentReadBufferOffset];
         CurrentReadBufferOffset += bufsz;
         if (CurrentReadBufferOffset == CurrentReadBufferSize)
           FreeReadBuffer();
        }
      else
        {
         if (CurrentReadBufferOffset < CurrentReadBufferSize)
           {
            for (i = 0L ; i < amountLeftToRead ; i++)
              ((char *) buf)[i] = CurrentReadBuffer[i + CurrentReadBufferOffset];
            bufsz -= amountLeftToRead;
            buf = (void *) (((char *) buf) + amountLeftToRead);
           }
         FreeReadBuffer();
         BufferedRead(buf,bufsz);
        }
     }
   else
     {
      if (bufsz > MAX_BLOCK_SIZE)
        {
         CurrentReadBufferSize = bufsz;
         if (bufsz > (BinaryInstanceFileSize - BinaryInstanceFileOffset))
           {
            SystemError("INSFILE",2);
            ExitRouter(EXIT_FAILURE);
           }
        }
      else if (MAX_BLOCK_SIZE >
              (BinaryInstanceFileSize - BinaryInstanceFileOffset))
        CurrentReadBufferSize = BinaryInstanceFileSize - BinaryInstanceFileOffset;
      else
        CurrentReadBufferSize = (unsigned long) MAX_BLOCK_SIZE;
      CurrentReadBuffer = (char *) genlongalloc(CurrentReadBufferSize);
      GenRead((void *) CurrentReadBuffer,CurrentReadBufferSize);
      for (i = 0L ; i < bufsz ; i++)
        ((char *) buf)[i] = CurrentReadBuffer[i];
      CurrentReadBufferOffset = bufsz;
      BinaryInstanceFileOffset += CurrentReadBufferSize;
     }
  }

/*****************************************************
  NAME         : FreeReadBuffer
  DESCRIPTION  : Deallocates buffer for binary reads
  INPUTS       : None
  RETURNS      : Nothing usefu
  SIDE EFFECTS : Binary global read buffer deallocated
  NOTES        : None
 *****************************************************/
static void FreeReadBuffer()
  {
   if (CurrentReadBufferSize != 0L)
     {
      genlongfree((void *) CurrentReadBuffer,CurrentReadBufferSize);
      CurrentReadBuffer = NULL;
      CurrentReadBufferSize = 0L;
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



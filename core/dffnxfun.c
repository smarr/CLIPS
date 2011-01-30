   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  01/08/99            */
   /*                                                     */
   /*                 DEFFUNCTION MODULE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Gary D. Riley                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/*      MDT          | 08-Jan-1999 |       DR831             */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "setup.h"

#if DEFFUNCTION_CONSTRUCT

#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)
#include "bload.h"
#include "dffnxbin.h"
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
#include "dffnxcmp.h"
#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)
#include "constrct.h"
#include "cstrcpsr.h"
#include "dffnxpsr.h"
#include "modulpsr.h"
#endif

#if (! RUN_TIME)
#include "extnfunc.h"
#endif

#include "dffnxexe.h"

#if DEBUGGING_FUNCTIONS
#include "watch.h"
#endif

#include "argacces.h"
#include "memalloc.h"
#include "cstrccom.h"
#include "router.h"

#define _DFFNXFUN_SOURCE_
#include "dffnxfun.h"

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

static void PrintDeffunctionCall(char *,void *);
static BOOLEAN EvaluateDeffunctionCall(void *,DATA_OBJECT *);
static void DecrementDeffunctionBusyCount(void *);
static void IncrementDeffunctionBusyCount(void *);

#if ! RUN_TIME
static void *AllocateModule(void);
static void  ReturnModule(void *);
static BOOLEAN ClearDeffunctionsReady(void);
#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)
static BOOLEAN RemoveAllDeffunctions(void);
static void DeffunctionDeleteError(char *);
static void SaveDeffunctionHeaders(char *);
static void SaveDeffunctionHeader(struct constructHeader *,void *);
static void SaveDeffunctions(char *);
#endif

#if DEBUGGING_FUNCTIONS
static BOOLEAN DeffunctionWatchAccess(int,int,EXPRESSION *);
static BOOLEAN DeffunctionWatchPrint(char *,int,EXPRESSION *);
#endif

/* =========================================
   *****************************************
      EXTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */
Thread globle struct construct *DeffunctionConstruct;
Thread globle int DeffunctionModuleIndex;

#if DEBUGGING_FUNCTIONS
Thread globle BOOLEAN WatchDeffunctions = OFF;
#endif

/* =========================================
   *****************************************
      INTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */

Thread static ENTITY_RECORD DeffunctionEntityRecord =
                     { "PCALL", PCALL,0,0,1,
                       PrintDeffunctionCall,PrintDeffunctionCall,
                       NULL,EvaluateDeffunctionCall,NULL,
                       DecrementDeffunctionBusyCount,IncrementDeffunctionBusyCount,
                       NULL,NULL,NULL,NULL };

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***************************************************
  NAME         : SetupDeffunctions
  DESCRIPTION  : Initializes parsers and access
                 functions for deffunctions
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Deffunction environment initialized
  NOTES        : None
 ***************************************************/
globle void SetupDeffunctions()
  {
   InstallPrimitive(&DeffunctionEntityRecord,PCALL);

   DeffunctionModuleIndex =
                RegisterModuleItem("deffunction",
#if (! RUN_TIME)
                                    AllocateModule,ReturnModule,
#else
                                    NULL,NULL,
#endif
#if BLOAD_AND_BSAVE || BLOAD || BLOAD_ONLY
                                    BloadDeffunctionModuleReference,
#else
                                    NULL,
#endif
#if CONSTRUCT_COMPILER && (! RUN_TIME)
                                    DeffunctionCModuleReference,
#else
                                    NULL,
#endif
                                    FindDeffunction);

   DeffunctionConstruct = AddConstruct("deffunction","deffunctions",
#if (! BLOAD_ONLY) && (! RUN_TIME)
                                       ParseDeffunction,
#else
                                       NULL,
#endif
                                       FindDeffunction,
                                       GetConstructNamePointer,GetConstructPPForm,
                                       GetConstructModuleItem,GetNextDeffunction,
                                       SetNextConstruct,IsDeffunctionDeletable,
                                       Undeffunction,
#if (! BLOAD_ONLY) && (! RUN_TIME)
                                       RemoveDeffunction
#else
                                       NULL
#endif
                                       );
#if ! RUN_TIME
   AddClearReadyFunction("deffunction",ClearDeffunctionsReady,0);

#if ! BLOAD_ONLY
#if DEFMODULE_CONSTRUCT
   AddPortConstructItem("deffunction",SYMBOL);
#endif
   AddSaveFunction("deffunction-headers",SaveDeffunctionHeaders,1000);
   AddSaveFunction("deffunctions",SaveDeffunctions,0);
   DefineFunction2("undeffunction",'v',PTIF UndeffunctionCommand,"UndeffunctionCommand","11w");
#endif

#if DEBUGGING_FUNCTIONS
   DefineFunction2("list-deffunctions",'v',PTIF ListDeffunctionsCommand,"ListDeffunctionsCommand","01");
   DefineFunction2("ppdeffunction",'v',PTIF PPDeffunctionCommand,"PPDeffunctionCommand","11w");
#endif

   DefineFunction2("get-deffunction-list",'m',PTIF GetDeffunctionListFunction,
                   "GetDeffunctionListFunction","01");

   DefineFunction2("deffunction-module",'w',PTIF GetDeffunctionModuleCommand,
                   "GetDeffunctionModuleCommand","11w");

#if BLOAD_AND_BSAVE || BLOAD || BLOAD_ONLY
   SetupDeffunctionsBload();
#endif

#if CONSTRUCT_COMPILER
   SetupDeffunctionCompiler();
#endif

#endif

#if DEBUGGING_FUNCTIONS
   AddWatchItem("deffunctions",0,&WatchDeffunctions,32,
                DeffunctionWatchAccess,DeffunctionWatchPrint);
#endif

  }

/***************************************************
  NAME         : FindDeffunction
  DESCRIPTION  : Searches for a deffunction
  INPUTS       : The name of the deffunction
                 (possibly including a module name)
  RETURNS      : Pointer to the deffunction if
                 found, otherwise NULL
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle void *FindDeffunction(
  char *dfnxModuleAndName)
  {
   return(FindNamedConstruct(dfnxModuleAndName,DeffunctionConstruct));
  }

/***************************************************
  NAME         : LookupDeffunctionByMdlOrScope
  DESCRIPTION  : Finds a deffunction anywhere (if
                 module is specified) or in current
                 or imported modules
  INPUTS       : The deffunction name
  RETURNS      : The deffunction (NULL if not found)
  SIDE EFFECTS : Error message printed on
                  ambiguous references
  NOTES        : None
 ***************************************************/
globle DEFFUNCTION *LookupDeffunctionByMdlOrScope(
  char *deffunctionName)
  {
   return((DEFFUNCTION *) LookupConstruct(DeffunctionConstruct,deffunctionName,TRUE));
  }

/***************************************************
  NAME         : LookupDeffunctionInScope
  DESCRIPTION  : Finds a deffunction in current or
                   imported modules (module
                   specifier is not allowed)
  INPUTS       : The deffunction name
  RETURNS      : The deffunction (NULL if not found)
  SIDE EFFECTS : Error message printed on
                  ambiguous references
  NOTES        : None
 ***************************************************/
globle DEFFUNCTION *LookupDeffunctionInScope(
  char *deffunctionName)
  {
   return((DEFFUNCTION *) LookupConstruct(DeffunctionConstruct,deffunctionName,FALSE));
  }

/***************************************************
  NAME         : Undeffunction
  DESCRIPTION  : External interface routine for
                 removing a deffunction
  INPUTS       : Deffunction pointer
  RETURNS      : FALSE if unsuccessful,
                 TRUE otherwise
  SIDE EFFECTS : Deffunction deleted, if possible
  NOTES        : None
 ***************************************************/
globle BOOLEAN Undeffunction(
  void *vptr)
  {
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(vptr)
#endif

#if BLOAD_ONLY || RUN_TIME
   return(FALSE);
#else

#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded() == TRUE)
     return(FALSE);
#endif
   if (vptr == NULL)
      return(RemoveAllDeffunctions());
   if (IsDeffunctionDeletable(vptr) == FALSE)
     return(FALSE);
   RemoveConstructFromModule((struct constructHeader *) vptr);
   RemoveDeffunction(vptr);
   return(TRUE);
#endif
  }

/****************************************************
  NAME         : GetNextDeffunction
  DESCRIPTION  : Accesses list of deffunctions
  INPUTS       : Deffunction pointer
  RETURNS      : The next deffunction, or the
                 first deffunction (if input is NULL)
  SIDE EFFECTS : None
  NOTES        : None
 ****************************************************/
globle void *GetNextDeffunction(
  void *ptr)
  {
   return((void *) GetNextConstructItem((struct constructHeader *) ptr,DeffunctionModuleIndex));
  }

/***************************************************
  NAME         : IsDeffunctionDeletable
  DESCRIPTION  : Determines if a deffunction is
                 executing or referenced by another
                 expression
  INPUTS       : Deffunction pointer
  RETURNS      : TRUE if the deffunction can
                 be deleted, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle int IsDeffunctionDeletable(
  void *ptr)
  {
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(ptr)
#endif

#if BLOAD_ONLY || RUN_TIME
   return(FALSE);
#else
   DEFFUNCTION *dptr;

#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded())
     return(FALSE);
#endif
   dptr = (DEFFUNCTION *) ptr;
   return(((dptr->busy == 0) && (dptr->executing == 0)) ? TRUE : FALSE);
#endif
  }

#if (! BLOAD_ONLY) && (! RUN_TIME)

/***************************************************
  NAME         : RemoveDeffunction
  DESCRIPTION  : Removes a deffunction
  INPUTS       : Deffunction pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Deffunction deallocated
  NOTES        : Assumes deffunction is not in use!!
 ***************************************************/
globle void RemoveDeffunction(
  void *vdptr)
  {
   DEFFUNCTION *dptr = (DEFFUNCTION *) vdptr;

   if (dptr == NULL)
     return;
   DecrementSymbolCount(GetDeffunctionNamePointer((void *) dptr));
   ExpressionDeinstall(dptr->code);
   ReturnPackedExpression(dptr->code);
   SetDeffunctionPPForm((void *) dptr,NULL);
   ClearUserDataList(dptr->header.usrData);
   rtn_struct(deffunctionStruct,dptr);
  }

#endif

/********************************************************
  NAME         : UndeffunctionCommand
  DESCRIPTION  : Deletes the named deffunction(s)
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Deffunction(s) removed
  NOTES        : H/L Syntax: (undeffunction <name> | *)
 ********************************************************/
globle void UndeffunctionCommand()
  {
   UndefconstructCommand("undeffunction",DeffunctionConstruct);
  }

/****************************************************************
  NAME         : GetDeffunctionModuleCommand
  DESCRIPTION  : Determines to which module a deffunction belongs
  INPUTS       : None
  RETURNS      : The symbolic name of the module
  SIDE EFFECTS : None
  NOTES        : H/L Syntax: (deffunction-module <dfnx-name>)
 ****************************************************************/
globle SYMBOL_HN *GetDeffunctionModuleCommand()
  {
   return(GetConstructModuleCommand("deffunction-module",DeffunctionConstruct));
  }

#if DEBUGGING_FUNCTIONS

/****************************************************
  NAME         : PPDeffunctionCommand
  DESCRIPTION  : Displays the pretty-print form of a
                 deffunction
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Pretty-print form displayed to
                 WDISPLAY logical name
  NOTES        : H/L Syntax: (ppdeffunction <name>)
 ****************************************************/
globle void PPDeffunctionCommand()
  {
   PPConstructCommand("ppdeffunction",DeffunctionConstruct);
  }

/***************************************************
  NAME         : ListDeffunctionsCommand
  DESCRIPTION  : Displays all deffunction names
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Deffunction name sprinted
  NOTES        : H/L Interface
 ***************************************************/
globle void ListDeffunctionsCommand()
  {
   ListConstructCommand("list-deffunctions",DeffunctionConstruct);
  }

/***************************************************
  NAME         : ListDeffunctions
  DESCRIPTION  : Displays all deffunction names
  INPUTS       : 1) The logical name of the output
                 2) The module
  RETURNS      : Nothing useful
  SIDE EFFECTS : Deffunction name sprinted
  NOTES        : C Interface
 ***************************************************/
globle void ListDeffunctions(
  char *logicalName,
  struct defmodule *theModule)
  {
   ListConstruct(DeffunctionConstruct,logicalName,theModule);
  }

#endif

/***************************************************************
  NAME         : GetDeffunctionListFunction
  DESCRIPTION  : Groups all deffunction names into
                 a multifield list
  INPUTS       : A data object buffer to hold
                 the multifield result
  RETURNS      : Nothing useful
  SIDE EFFECTS : Multifield allocated and filled
  NOTES        : H/L Syntax: (get-deffunction-list [<module>])
 ***************************************************************/
globle void GetDeffunctionListFunction(
  DATA_OBJECT*returnValue)
  {
   GetConstructListFunction("get-deffunction-list",returnValue,DeffunctionConstruct);
  }

/***************************************************************
  NAME         : GetDeffunctionList
  DESCRIPTION  : Groups all deffunction names into
                 a multifield list
  INPUTS       : 1) A data object buffer to hold
                    the multifield result
                 2) The module from which to obtain deffunctions
  RETURNS      : Nothing useful
  SIDE EFFECTS : Multifield allocated and filled
  NOTES        : External C access
 ***************************************************************/
globle void GetDeffunctionList(
  DATA_OBJECT *returnValue,
  struct defmodule *theModule)
  {
   GetConstructList(returnValue,DeffunctionConstruct,theModule);
  }

/*******************************************************
  NAME         : CheckDeffunctionCall
  DESCRIPTION  : Checks the number of arguments
                 passed to a deffunction
  INPUTS       : 1) Deffunction pointer
                 2) The number of arguments
  RETURNS      : TRUE if OK, FALSE otherwise
  SIDE EFFECTS : Message printed on errors
  NOTES        : None
 *******************************************************/
globle int CheckDeffunctionCall(
  void *vdptr,
  int args)
  {
   DEFFUNCTION *dptr;

   if (vdptr == NULL)
     return(FALSE);
   dptr = (DEFFUNCTION *) vdptr;
   if (args < dptr->minNumberOfParameters)
     {
      if (dptr->maxNumberOfParameters == -1)
        ExpectedCountError(GetDeffunctionName((void *) dptr),
                           AT_LEAST,dptr->minNumberOfParameters);
      else
        ExpectedCountError(GetDeffunctionName((void *) dptr),
                           EXACTLY,dptr->minNumberOfParameters);
      return(FALSE);
     }
   else if ((args > dptr->minNumberOfParameters) &&
            (dptr->maxNumberOfParameters != -1))
     {
      ExpectedCountError(GetDeffunctionName((void *) dptr),
                         EXACTLY,dptr->minNumberOfParameters);
      return(FALSE);
     }
   return(TRUE);
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***************************************************
  NAME         : PrintDeffunctionCall
  DESCRIPTION  : PrintExpression() support function
                 for deffunction calls
  INPUTS       : 1) The output logical name
                 2) The deffunction
  RETURNS      : Nothing useful
  SIDE EFFECTS : Call expression printed
  NOTES        : None
 ***************************************************/
#if IBM_TBC
#pragma argsused
#endif
static void PrintDeffunctionCall(
  char *log,
  void *value)
  {
#if MAC_MPW || MAC_MCW || IBM_MCW
#pragma unused(log)
#pragma unused(value)
#endif
#if DEVELOPER
   PrintRouter(log,"(");
   PrintRouter(log,GetDeffunctionName(value));
   if (GetFirstArgument() != NULL)
     {
      PrintRouter(log," ");
      PrintExpression(log,GetFirstArgument());
     }
   PrintRouter(log,")");
#endif
  }

/*******************************************************
  NAME         : EvaluateDeffunctionCall
  DESCRIPTION  : Primitive support function for
                 calling a deffunction
  INPUTS       : 1) The deffunction
                 2) A data object buffer to hold
                    the evaluation result
  RETURNS      : FALSE if the deffunction
                 returns the symbol FALSE,
                 TRUE otherwise
  SIDE EFFECTS : Data obejct buffer set and any
                 side-effects of calling the deffunction
  NOTES        : None
 *******************************************************/
static BOOLEAN EvaluateDeffunctionCall(
  void *value,
  DATA_OBJECT *result)
  {
   CallDeffunction((DEFFUNCTION *) value,GetFirstArgument(),result);
   if ((GetpType(result) == SYMBOL) &&
       (GetpValue(result) == FalseSymbol))
     return(FALSE);
   return(TRUE);
  }

/***************************************************
  NAME         : DecrementDeffunctionBusyCount
  DESCRIPTION  : Lowers the busy count of a
                 deffunction construct
  INPUTS       : The deffunction
  RETURNS      : Nothing useful
  SIDE EFFECTS : Busy count decremented if a clear
                 is not in progress (see comment)
  NOTES        : None
 ***************************************************/
static void DecrementDeffunctionBusyCount(
  void *value)
  {
   /* ==============================================
      The deffunctions to which expressions in other
      constructs may refer may already have been
      deleted - thus, it is important not to modify
      the busy flag during a clear.
      ============================================== */
   if (! ClearInProgress)
     ((DEFFUNCTION *) value)->busy--;
  }

/***************************************************
  NAME         : IncrementDeffunctionBusyCount
  DESCRIPTION  : Raises the busy count of a
                 deffunction construct
  INPUTS       : The deffunction
  RETURNS      : Nothing useful
  SIDE EFFECTS : Busy count incremented
  NOTES        : None
 ***************************************************/
static void IncrementDeffunctionBusyCount(
  void *value)
  {
   ((DEFFUNCTION *) value)->busy++;
  }

#if ! RUN_TIME

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
   return((void *) get_struct(deffunctionModule));
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
#if (! BLOAD_ONLY)
   FreeConstructHeaderModule((struct defmoduleItemHeader *) theItem,DeffunctionConstruct);
#endif
   rtn_struct(deffunctionModule,theItem);
  }

/***************************************************
  NAME         : ClearDeffunctionsReady
  DESCRIPTION  : Determines if it is safe to
                 remove all deffunctions
                 Assumes *all* constructs will be
                 deleted - only checks to see if
                 any deffunctions are currently
                 executing
  INPUTS       : None
  RETURNS      : TRUE if no deffunctions are
                 executing, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : Used by (clear) and (bload)
 ***************************************************/
static BOOLEAN ClearDeffunctionsReady()
  {
   return((ExecutingDeffunction != NULL) ? FALSE : TRUE);
  }

#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)

/***************************************************
  NAME         : RemoveAllDeffunctions
  DESCRIPTION  : Removes all deffunctions
  INPUTS       : None
  RETURNS      : TRUE if all deffunctions
                 removed, FALSE otherwise
  SIDE EFFECTS : Deffunctions removed
  NOTES        : None
 ***************************************************/
static BOOLEAN RemoveAllDeffunctions()
  {
   DEFFUNCTION *dptr,*dtmp;
   int oldbusy,success = TRUE;

#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded() == TRUE)
     return(FALSE);
#endif

   dptr = (DEFFUNCTION *) GetNextDeffunction(NULL);
   while (dptr != NULL)
     {
      if (dptr->executing > 0)
        {
         DeffunctionDeleteError(GetDeffunctionName((void *) dptr));
         success = FALSE;
        }
      else
        {
         oldbusy = dptr->busy;
         ExpressionDeinstall(dptr->code);
         dptr->busy = oldbusy;
         ReturnPackedExpression(dptr->code);
         dptr->code = NULL;
        }
      dptr = (DEFFUNCTION *) GetNextDeffunction((void *) dptr);
     }

   dptr = (DEFFUNCTION *) GetNextDeffunction(NULL);
   while (dptr != NULL)
     {
      dtmp = dptr;
      dptr = (DEFFUNCTION *) GetNextDeffunction((void *) dptr);
      if (dtmp->executing == 0)
        {
         if (dtmp->busy > 0)
           {
            PrintWarningID("DFFNXFUN",1,FALSE);
            PrintRouter(WWARNING,"Deffunction ");
            PrintRouter(WWARNING,GetDeffunctionName((void *) dtmp));
            PrintRouter(WWARNING," only partially deleted due to usage by other constructs.\n");
            SetDeffunctionPPForm((void *) dtmp,NULL);
            success = FALSE;
           }
         else
           {
            RemoveConstructFromModule((struct constructHeader *) dtmp);
            RemoveDeffunction(dtmp);
           }
        }
     }
   return(success);
  }

/****************************************************
  NAME         : DeffunctionDeleteError
  DESCRIPTION  : Prints out an error message when
                 a deffunction deletion attempt fails
  INPUTS       : The deffunction name
  RETURNS      : Nothing useful
  SIDE EFFECTS : Error message printed
  NOTES        : None
 ****************************************************/
static void DeffunctionDeleteError(
  char *dfnxName)
  {
   CantDeleteItemErrorMessage("deffunction",dfnxName);
  }

/***************************************************
  NAME         : SaveDeffunctionHeaders
  DESCRIPTION  : Writes out deffunction forward
                 declarations for (save) command
  INPUTS       : The logical output name
  RETURNS      : Nothing useful
  SIDE EFFECTS : Writes out deffunctions with no
                 body of actions
  NOTES        : Used for deffunctions which are
                 mutually recursive with other
                 constructs
 ***************************************************/
static void SaveDeffunctionHeaders(
  char *logicalName)
  {
   DoForAllConstructs(SaveDeffunctionHeader,DeffunctionModuleIndex,
                      FALSE,(void *) logicalName);
  }

/***************************************************
  NAME         : SaveDeffunctionHeader
  DESCRIPTION  : Writes a deffunction forward
                 declaration to the save file
  INPUTS       : 1) The deffunction
                 2) The logical name of the output
  RETURNS      : Nothing useful
  SIDE EFFECTS : Defffunction header written
  NOTES        : None
 ***************************************************/
static void SaveDeffunctionHeader(
  struct constructHeader *theDeffunction,
  void *userBuffer)
  {
   DEFFUNCTION *dfnxPtr = (DEFFUNCTION *) theDeffunction;
   char *logicalName = (char *) userBuffer;
   register int i;

   if (GetDeffunctionPPForm((void *) dfnxPtr) != NULL)
     {
      PrintRouter(logicalName,"(deffunction ");
      PrintRouter(logicalName,DeffunctionModule((void *) dfnxPtr));
      PrintRouter(logicalName,"::");      
      PrintRouter(logicalName,GetDeffunctionName((void *) dfnxPtr));
      PrintRouter(logicalName," (");
      for (i = 0 ; i < dfnxPtr->minNumberOfParameters ; i++)
        {
         PrintRouter(logicalName,"?p");
         PrintLongInteger(logicalName,(long) i);
         if (i != dfnxPtr->minNumberOfParameters-1)
           PrintRouter(logicalName," ");
        }
      if (dfnxPtr->maxNumberOfParameters == -1)
        {
         if (dfnxPtr->minNumberOfParameters != 0)
           PrintRouter(logicalName," ");
         PrintRouter(logicalName,"$?wildargs))\n\n");
        }
      else
        PrintRouter(logicalName,"))\n\n");
     }
  }

/***************************************************
  NAME         : SaveDeffunctions
  DESCRIPTION  : Writes out deffunctions
                 for (save) command
  INPUTS       : The logical output name
  RETURNS      : Nothing useful
  SIDE EFFECTS : Writes out deffunctions
  NOTES        : None
 ***************************************************/
static void SaveDeffunctions(
  char *logicalName)
  {
   SaveConstruct(logicalName,DeffunctionConstruct);
  }

#endif

#if DEBUGGING_FUNCTIONS

/******************************************************************
  NAME         : DeffunctionWatchAccess
  DESCRIPTION  : Parses a list of deffunction names passed by
                 AddWatchItem() and sets the traces accordingly
  INPUTS       : 1) A code indicating which trace flag is to be set
                    Ignored
                 2) The value to which to set the trace flags
                 3) A list of expressions containing the names
                    of the deffunctions for which to set traces
  RETURNS      : TRUE if all OK, FALSE otherwise
  SIDE EFFECTS : Watch flags set in specified deffunctions
  NOTES        : Accessory function for AddWatchItem()
 ******************************************************************/
#if IBM_TBC
#pragma argsused
#endif
static BOOLEAN DeffunctionWatchAccess(
  int code,
  int newState,
  EXPRESSION *argExprs)
  {
#if MAC_MPW || MAC_MCW || IBM_MCW
#pragma unused(code)
#endif
   return(ConstructSetWatchAccess(DeffunctionConstruct,newState,argExprs,
                                    GetDeffunctionWatch,SetDeffunctionWatch));
  }

/***********************************************************************
  NAME         : DeffunctionWatchPrint
  DESCRIPTION  : Parses a list of deffunction names passed by
                 AddWatchItem() and displays the traces accordingly
  INPUTS       : 1) The logical name of the output
                 2) A code indicating which trace flag is to be examined
                    Ignored
                 3) A list of expressions containing the names
                    of the deffunctions for which to examine traces
  RETURNS      : TRUE if all OK, FALSE otherwise
  SIDE EFFECTS : Watch flags displayed for specified deffunctions
  NOTES        : Accessory function for AddWatchItem()
 ***********************************************************************/
#if IBM_TBC
#pragma argsused
#endif
static BOOLEAN DeffunctionWatchPrint(
  char *log,
  int code,
  EXPRESSION *argExprs)
  {
#if MAC_MPW || MAC_MCW || IBM_MCW
#pragma unused(code)
#endif
   return(ConstructPrintWatchAccess(DeffunctionConstruct,log,argExprs,
                                    GetDeffunctionWatch,SetDeffunctionWatch));
  }

/*********************************************************
  NAME         : SetDeffunctionWatch
  DESCRIPTION  : Sets the trace to ON/OFF for the
                 deffunction
  INPUTS       : 1) TRUE to set the trace on,
                    FALSE to set it off
                 2) A pointer to the deffunction
  RETURNS      : Nothing useful
  SIDE EFFECTS : Watch flag for the deffunction set
  NOTES        : None
 *********************************************************/
globle void SetDeffunctionWatch(
  int newState,
  void *dptr)
  {
   ((DEFFUNCTION *) dptr)->trace = (unsigned short) newState;
  }

/*********************************************************
  NAME         : GetDeffunctionWatch
  DESCRIPTION  : Determines if trace messages are
                 gnerated when executing deffunction
  INPUTS       : A pointer to the deffunction
  RETURNS      : TRUE if a trace is active,
                 FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 *********************************************************/
globle int GetDeffunctionWatch(
  void *dptr)
  {
   return(((DEFFUNCTION *) dptr)->trace);
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



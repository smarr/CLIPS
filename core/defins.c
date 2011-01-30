   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/13/98          */
   /*                                                     */
   /*                  DEFINSTANCES MODULE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Kernel definstances interface commands           */
/*              and routines                                 */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
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

#if DEFINSTANCES_CONSTRUCT

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
#include "bload.h"
#include "dfinsbin.h"
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
#include "dfinscmp.h"
#endif

#include "argacces.h"
#include "classcom.h"
#include "classfun.h"
#include "memalloc.h"
#include "cstrccom.h"
#include "cstrcpsr.h"
#include "constant.h"
#include "constrct.h"
#include "evaluatn.h"
#include "extnfunc.h"
#include "insfun.h"
#include "inspsr.h"
#include "modulpsr.h"
#include "router.h"
#include "scanner.h"
#include "symbol.h"
#include "utility.h"

#define _DEFINS_SOURCE_
#include "defins.h"

#if (! BLOAD_ONLY) && (! RUN_TIME)
extern Thread struct token ObjectParseToken;
#endif

/* =========================================
   *****************************************
                   CONSTANTS
   =========================================
   ***************************************** */
#define ACTIVE_RLN "active"

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

#if (! BLOAD_ONLY) && (! RUN_TIME)
static int ParseDefinstances(char *);
static SYMBOL_HN *ParseDefinstancesName(char *,int *);
static void RemoveDefinstances(void *);
static void SaveDefinstances(char *);
static BOOLEAN RemoveAllDefinstances(void);
static void DefinstancesDeleteError(char *);

#if INSTANCE_PATTERN_MATCHING
static void CreateInitialDefinstances(void);
#endif
#endif

#if ! RUN_TIME
static void *AllocateModule(void);
static void  ReturnModule(void *);
static BOOLEAN ClearDefinstancesReady(void);
static void CheckDefinstancesBusy(struct constructHeader *,void *);
#endif

static void ResetDefinstances(void);
static void ResetDefinstancesAction(struct constructHeader *,void *);

/* =========================================
   *****************************************
      EXTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */
Thread globle int DefinstancesModuleIndex;

/* =========================================
   *****************************************
      INTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */
Thread static struct construct *DefinstancesConstruct;

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***************************************************
  NAME         : SetupDefinstances
  DESCRIPTION  : Adds the definstance support routines
                   to the Kernel
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Appropriate function lists modified
  NOTES        : None
 ***************************************************/
globle void SetupDefinstances()
  {
   DefinstancesModuleIndex =
                RegisterModuleItem("definstances",
#if (! RUN_TIME)
                                    AllocateModule,ReturnModule,
#else
                                    NULL,NULL,
#endif
#if BLOAD_AND_BSAVE || BLOAD || BLOAD_ONLY
                                    BloadDefinstancesModuleRef,
#else
                                    NULL,
#endif
#if CONSTRUCT_COMPILER && (! RUN_TIME)
                                    DefinstancesCModuleReference,
#else
                                    NULL,
#endif
                                    FindDefinstances);

   DefinstancesConstruct =
      AddConstruct("definstances","definstances",
#if (! BLOAD_ONLY) && (! RUN_TIME)
                   ParseDefinstances,
#else
                   NULL,
#endif
                   FindDefinstances,
                   GetConstructNamePointer,GetConstructPPForm,
                   GetConstructModuleItem,GetNextDefinstances,SetNextConstruct,
                   IsDefinstancesDeletable,Undefinstances,
#if (! BLOAD_ONLY) && (! RUN_TIME)
                   RemoveDefinstances
#else
                   NULL
#endif
                   );

#if ! RUN_TIME
   AddClearReadyFunction("definstances",ClearDefinstancesReady,0);

#if ! BLOAD_ONLY
   DefineFunction2("undefinstances",'v',PTIF UndefinstancesCommand,"UndefinstancesCommand","11w");
   AddSaveFunction("definstances",SaveDefinstances,0);

#if INSTANCE_PATTERN_MATCHING
   AddClearFunction("definstances",CreateInitialDefinstances,-1000);
#endif

#endif

#if DEBUGGING_FUNCTIONS
   DefineFunction2("ppdefinstances",'v',PTIF PPDefinstancesCommand ,"PPDefinstancesCommand","11w");
   DefineFunction2("list-definstances",'v',PTIF ListDefinstancesCommand,"ListDefinstancesCommand","01");
#endif

   DefineFunction2("get-definstances-list",'m',PTIF GetDefinstancesListFunction,
                   "GetDefinstancesListFunction","01");
   DefineFunction2("definstances-module",'w',PTIF GetDefinstancesModuleCommand,
                   "GetDefinstancesModuleCommand","11w");

#endif
   AddResetFunction("definstances",(void (*)(void)) ResetDefinstances,0);

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
   SetupDefinstancesBload();
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
   SetupDefinstancesCompiler();
#endif
  }

/***********************************************************
  NAME         : GetNextDefinstances
  DESCRIPTION  : Finds first or next definstances
  INPUTS       : The address of the current definstances
  RETURNS      : The address of the next definstances
                   (NULL if none)
  SIDE EFFECTS : None
  NOTES        : If ptr == NULL, the first definstances
                    is returned.
 ***********************************************************/
globle void *GetNextDefinstances(
  void *ptr)
  {
   return((void *) GetNextConstructItem((struct constructHeader *) ptr,
                                        DefinstancesModuleIndex));
  }

/***************************************************
  NAME         : FindDefinstances
  DESCRIPTION  : Looks up a definstance construct
                   by name-string
  INPUTS       : The symbolic name
  RETURNS      : The definstance address, or NULL
                    if not found
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle void *FindDefinstances(
  char *name)
  {
   return(FindNamedConstruct(name,DefinstancesConstruct));
  }

/***************************************************
  NAME         : IsDefinstancesDeletable
  DESCRIPTION  : Determines if a definstances
                   can be deleted
  INPUTS       : Address of the definstances
  RETURNS      : TRUE if deletable, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle int IsDefinstancesDeletable(
  void *ptr)
  {
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(ptr)
#endif

#if BLOAD_ONLY || RUN_TIME
   return(FALSE);
#else
#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded())
     return(FALSE);
#endif
   return((((DEFINSTANCES *) ptr)->busy == 0) ? TRUE : FALSE);
#endif
  }

/***********************************************************
  NAME         : UndefinstancesCommand
  DESCRIPTION  : Removes a definstance
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Definstance deallocated
  NOTES        : H/L Syntax : (undefinstances <name> | *)
 ***********************************************************/
globle void UndefinstancesCommand()
  {
   UndefconstructCommand("undefinstances",DefinstancesConstruct);
  }

/*****************************************************************
  NAME         : GetDefinstancesModuleCommand
  DESCRIPTION  : Determines to which module a definstances belongs
  INPUTS       : None
  RETURNS      : The symbolic name of the module
  SIDE EFFECTS : None
  NOTES        : H/L Syntax: (definstances-module <defins-name>)
 *****************************************************************/
globle SYMBOL_HN *GetDefinstancesModuleCommand()
  {
   return(GetConstructModuleCommand("definstances-module",DefinstancesConstruct));
  }

/***********************************************************
  NAME         : Undefinstances
  DESCRIPTION  : Removes a definstance
  INPUTS       : Address of definstances to remove
  RETURNS      : TRUE if successful,
                 FALSE otherwise
  SIDE EFFECTS : Definstance deallocated
  NOTES        : None
 ***********************************************************/
globle BOOLEAN Undefinstances(
  void *vptr)
  {
   DEFINSTANCES *dptr;

   dptr = (DEFINSTANCES *) vptr;
#if RUN_TIME || BLOAD_ONLY
   return(FALSE);
#else

#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded())
     return(FALSE);
#endif
   if (dptr == NULL)
     return(RemoveAllDefinstances());
   if (IsDefinstancesDeletable(vptr) == FALSE)
     return(FALSE);
   RemoveConstructFromModule((struct constructHeader *) vptr);
   RemoveDefinstances((void *) dptr);
   return(TRUE);
#endif
  }

#if DEBUGGING_FUNCTIONS

/***************************************************************
  NAME         : PPDefinstancesCommand
  DESCRIPTION  : Prints out the pretty-print form of a definstance
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : H/L Syntax : (ppdefinstances <name>)
 ***************************************************************/
globle void PPDefinstancesCommand()
  {
   PPConstructCommand("ppdefinstances",DefinstancesConstruct);
  }

/***************************************************
  NAME         : ListDefinstancesCommand
  DESCRIPTION  : Displays all definstances names
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Definstances name sprinted
  NOTES        : H/L Interface
 ***************************************************/
globle void ListDefinstancesCommand()
  {
   ListConstructCommand("list-definstances",DefinstancesConstruct);
  }

/***************************************************
  NAME         : ListDefinstances
  DESCRIPTION  : Displays all definstances names
  INPUTS       : 1) The logical name of the output
                 2) The module
  RETURNS      : Nothing useful
  SIDE EFFECTS : Definstances names printed
  NOTES        : C Interface
 ***************************************************/
globle void ListDefinstances(
  char *logicalName,
  struct defmodule *theModule)
  {
   ListConstruct(DefinstancesConstruct,logicalName,theModule);
  }

#endif

/****************************************************************
  NAME         : GetDefinstancesListFunction
  DESCRIPTION  : Groups all definstances names into
                 a multifield list
  INPUTS       : A data object buffer to hold
                 the multifield result
  RETURNS      : Nothing useful
  SIDE EFFECTS : Multifield allocated and filled
  NOTES        : H/L Syntax: (get-definstances-list [<module>])
 ****************************************************************/
globle void GetDefinstancesListFunction(
  DATA_OBJECT*returnValue)
  {
   GetConstructListFunction("get-definstances-list",returnValue,DefinstancesConstruct);
  }

/***************************************************************
  NAME         : GetDefinstancesList
  DESCRIPTION  : Groups all definstances names into
                 a multifield list
  INPUTS       : 1) A data object buffer to hold
                    the multifield result
                 2) The module from which to obtain definstances
  RETURNS      : Nothing useful
  SIDE EFFECTS : Multifield allocated and filled
  NOTES        : External C access
 ***************************************************************/
globle void GetDefinstancesList(
  DATA_OBJECT *returnValue,
  struct defmodule *theModule)
  {
   GetConstructList(returnValue,DefinstancesConstruct,theModule);
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

#if (! BLOAD_ONLY) && (! RUN_TIME)

/*********************************************************************
  NAME         : ParseDefinstances
  DESCRIPTION  : Parses and allocates a definstances construct
  INPUTS       : The logical name of the input source
  RETURNS      : FALSE if no errors, TRUE otherwise
  SIDE EFFECTS : Definstances parsed and created
  NOTES        : H/L Syntax :

                 (definstances  <name> [active] [<comment>]
                    <instance-definition>+)

                 <instance-definition> ::=
                    (<instance-name> of <class-name> <slot-override>*)

                 <slot-override> ::= (<slot-name> <value-expression>*)
 *********************************************************************/
static int ParseDefinstances(
  char *readSource)
  {
   SYMBOL_HN *dname;
   void *mkinsfcall;
   EXPRESSION *mkinstance,*mkbot = NULL;
   DEFINSTANCES *dobj;
   int active;

   SetPPBufferStatus(ON);
   FlushPPBuffer();
   SetIndentDepth(3);
   SavePPBuffer("(definstances ");

#if BLOAD || BLOAD_AND_BSAVE
   if ((Bloaded()) && (! CheckSyntaxMode))
     {
      CannotLoadWithBloadMessage("definstances");
      return(TRUE);
     }
#endif
   dname = ParseDefinstancesName(readSource,&active);
   if (dname == NULL)
     return(TRUE);

   dobj = get_struct(definstances);
   InitializeConstructHeader("definstances",(struct constructHeader *) dobj,dname);
   dobj->busy = 0;
   dobj->mkinstance = NULL;
#if INSTANCE_PATTERN_MATCHING
   mkinsfcall = (void *) FindFunction(active ? "active-make-instance" : "make-instance");
#else
   mkinsfcall = (void *) FindFunction("make-instance");
#endif
   while (GetType(ObjectParseToken) == LPAREN)
     {
      mkinstance = GenConstant(UNKNOWN_VALUE,mkinsfcall);
      mkinstance = ParseInitializeInstance(mkinstance,readSource);
      if (mkinstance == NULL)
        {
         ReturnExpression(dobj->mkinstance);
         rtn_struct(definstances,dobj);
         return(TRUE);
        }
      if (ExpressionContainsVariables(mkinstance,FALSE) == TRUE)
        {
         LocalVariableErrorMessage("definstances");
         ReturnExpression(mkinstance);
         ReturnExpression(dobj->mkinstance);
         rtn_struct(definstances,dobj);
         return(TRUE);
        }
      if (mkbot == NULL)
        dobj->mkinstance = mkinstance;
      else
        GetNextArgument(mkbot) = mkinstance;
      mkbot = mkinstance;
      GetToken(readSource,&ObjectParseToken);
      PPBackup();
      PPCRAndIndent();
      SavePPBuffer(ObjectParseToken.print_rep);
     }

   if (GetType(ObjectParseToken) != RPAREN)
     {
      ReturnExpression(dobj->mkinstance);
      rtn_struct(definstances,dobj);
      SyntaxErrorMessage("definstances");
      return(TRUE);
     }
   else
     {
      if (CheckSyntaxMode)
        {
         ReturnExpression(dobj->mkinstance);
         rtn_struct(definstances,dobj);
         return(FALSE);
        }
#if DEBUGGING_FUNCTIONS
      if (GetConserveMemory() == FALSE)
        {
         if (dobj->mkinstance != NULL)
           PPBackup();
         PPBackup();
         SavePPBuffer(")\n");
         SetDefinstancesPPForm((void *) dobj,CopyPPBuffer());
        }
#endif
      mkinstance = dobj->mkinstance;
      dobj->mkinstance = PackExpression(mkinstance);
      ReturnExpression(mkinstance);
      IncrementSymbolCount(GetDefinstancesNamePointer((void *) dobj));
      ExpressionInstall(dobj->mkinstance);
     }

   AddConstructToModule((struct constructHeader *) dobj);
   return(FALSE);
  }

/*************************************************************
  NAME         : ParseDefinstancesName
  DESCRIPTION  : Parses definstance name and optional comment
                 and optional "active" keyword
  INPUTS       : 1) The logical name of the input source
                 2) Buffer to hold flag indicating if
                    definstances should cause pattern-matching
                    to occur during slot-overrides
  RETURNS      : Address of name symbol, or
                   NULL if there was an error
  SIDE EFFECTS : Token after name or comment is scanned
  NOTES        : Assumes "(definstances" has already
                   been scanned.
 *************************************************************/
static SYMBOL_HN *ParseDefinstancesName(
  char *readSource,
  int *active)
  {
   SYMBOL_HN *dname;

   *active = FALSE;
   dname = GetConstructNameAndComment(readSource,&ObjectParseToken,"definstances",
                                      FindDefinstances,Undefinstances,"@",
                                      TRUE,FALSE,TRUE);
   if (dname == NULL)
     return(NULL);

#if INSTANCE_PATTERN_MATCHING
   if ((GetType(ObjectParseToken) != SYMBOL) ? FALSE :
       (strcmp(ValueToString(GetValue(ObjectParseToken)),ACTIVE_RLN) == 0))
     {
      PPBackup();
      PPBackup();
      SavePPBuffer(" ");
      SavePPBuffer(ObjectParseToken.print_rep);
      PPCRAndIndent();
      GetToken(readSource,&ObjectParseToken);
      *active = TRUE;
     }
#endif
   if (GetType(ObjectParseToken) == STRING)
     {
      PPBackup();
      PPBackup();
      SavePPBuffer(" ");
      SavePPBuffer(ObjectParseToken.print_rep);
      PPCRAndIndent();
      GetToken(readSource,&ObjectParseToken);
     }
   return(dname);
  }

/**************************************************************
  NAME         : RemoveDefinstances
  DESCRIPTION  : Deallocates and removes a definstance construct
  INPUTS       : The definstance address
  RETURNS      : Nothing useful
  SIDE EFFECTS : Existing definstance construct deleted
  NOTES        : Assumes busy count of definstance is 0
 **************************************************************/
static void RemoveDefinstances(
  void *vdptr)
  {
   DEFINSTANCES *dptr = (DEFINSTANCES *) vdptr;

   DecrementSymbolCount(GetDefinstancesNamePointer((void *) dptr));
   ExpressionDeinstall(dptr->mkinstance);
   ReturnPackedExpression(dptr->mkinstance);
   SetDefinstancesPPForm((void *) dptr,NULL);
   ClearUserDataList(dptr->header.usrData);
   rtn_struct(definstances,dptr);
  }

/***************************************************
  NAME         : SaveDefinstances
  DESCRIPTION  : Prints pretty print form of
                   definstances to specified output
  INPUTS       : The logical name of the output
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
static void SaveDefinstances(
  char *logName)
  {
   SaveConstruct(logName,DefinstancesConstruct);
  }

/***************************************************
  NAME         : RemoveAllDefinstances
  DESCRIPTION  : Removes all definstances constructs
  INPUTS       : None
  RETURNS      : TRUE if successful,
                 FALSE otherwise
  SIDE EFFECTS : All definstances deallocated
  NOTES        : None
 ***************************************************/
static BOOLEAN RemoveAllDefinstances()
  {
   DEFINSTANCES *dptr,*dhead;
   int success = TRUE;

#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded())
     return(FALSE);
#endif
  dhead = (DEFINSTANCES *) GetNextDefinstances(NULL);
  while (dhead != NULL)
    {
     dptr = dhead;
     dhead = (DEFINSTANCES *) GetNextDefinstances((void *) dhead);
     if (IsDefinstancesDeletable((void *) dptr))
       {
        RemoveConstructFromModule((struct constructHeader *) dptr);
        RemoveDefinstances((void *) dptr);
       }
     else
       {
        DefinstancesDeleteError(GetDefinstancesName((void *) dptr));
        success = FALSE;
       }
    }
   return(success);
  }

/***************************************************
  NAME         : DefinstancesDeleteError
  DESCRIPTION  : Prints an error message for
                 unsuccessful definstances
                 deletion attempts
  INPUTS       : The name of the definstances
  RETURNS      : Nothing useful
  SIDE EFFECTS : Error message printed
  NOTES        : None
 ***************************************************/
static void DefinstancesDeleteError(
  char *dname)
  {
   CantDeleteItemErrorMessage("definstances",dname);
  }

#if INSTANCE_PATTERN_MATCHING

/********************************************************
  NAME         : CreateInitialDefinstances
  DESCRIPTION  : Makes the initial-object definstances
                 structure for creating an initial-object
                 which will match default object patterns
                 in defrules
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : initial-object definstances created
  NOTES        : None
 ********************************************************/
static void CreateInitialDefinstances()
  {
   EXPRESSION *tmp;
   DEFINSTANCES *theDefinstances;

   theDefinstances = get_struct(definstances);
   InitializeConstructHeader("definstances",(struct constructHeader *) theDefinstances,
                             INITIAL_OBJECT_SYMBOL);
   theDefinstances->busy = 0;
   tmp = GenConstant(FCALL,(void *) FindFunction("make-instance"));
   tmp->argList = GenConstant(INSTANCE_NAME,(void *) INITIAL_OBJECT_SYMBOL);
   tmp->argList->nextArg =
       GenConstant(DEFCLASS_PTR,(void *) LookupDefclassInScope(INITIAL_OBJECT_CLASS_NAME));
   theDefinstances->mkinstance = PackExpression(tmp);
   ReturnExpression(tmp);
   IncrementSymbolCount(GetDefinstancesNamePointer((void *) theDefinstances));
   ExpressionInstall(theDefinstances->mkinstance);
   AddConstructToModule((struct constructHeader *) theDefinstances);
  }

#endif

#endif

#if ! RUN_TIME

/*****************************************************
  NAME         : AllocateModule
  DESCRIPTION  : Creates and initializes a
                 list of definstances for a new module
  INPUTS       : None
  RETURNS      : The new definstances module
  SIDE EFFECTS : Definstances module created
  NOTES        : None
 *****************************************************/
static void *AllocateModule()
  {
   return((void *) get_struct(definstancesModule));
  }

/***************************************************
  NAME         : ReturnModule
  DESCRIPTION  : Removes a definstances module and
                 all associated definstances
  INPUTS       : The definstances module
  RETURNS      : Nothing useful
  SIDE EFFECTS : Module and definstances deleted
  NOTES        : None
 ***************************************************/
static void ReturnModule(
  void *theItem)
  {
#if (! BLOAD_ONLY)
   FreeConstructHeaderModule((struct defmoduleItemHeader *) theItem,DefinstancesConstruct);
#endif
   rtn_struct(definstancesModule,theItem);
  }

/***************************************************
  NAME         : ClearDefinstancesReady
  DESCRIPTION  : Determines if it is safe to
                 remove all definstances
                 Assumes *all* constructs will be
                 deleted
  INPUTS       : None
  RETURNS      : TRUE if all definstances can
                 be deleted, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : Used by (clear) and (bload)
 ***************************************************/
static BOOLEAN ClearDefinstancesReady()
  {
   int flagBuffer = TRUE;

   DoForAllConstructs(CheckDefinstancesBusy,DefinstancesModuleIndex,
                      FALSE,(void *) &flagBuffer);
   return(flagBuffer);
  }

/***************************************************
  NAME         : CheckDefinstancesBusy
  DESCRIPTION  : Determines if a definstances is
                 in use or not
  INPUTS       : 1) The definstances
                 2) A buffer to set to 0 if the
                    the definstances is busy
  RETURNS      : Nothing useful
  SIDE EFFECTS : Buffer set to 0 if definstances
                 busy
  NOTES        : The flag buffer is not modified
                 if definstances is not busy
                 (assumed to be initialized to 1)
 ***************************************************/
static void CheckDefinstancesBusy(
  struct constructHeader *theDefinstances,
  void *userBuffer)
  {
   if (((DEFINSTANCES *) theDefinstances)->busy > 0)
     * (int *) userBuffer = FALSE;
  }

#endif

/***************************************************
  NAME         : ResetDefinstances
  DESCRIPTION  : Calls EvaluateExpression for each of
                   the make-instance calls in all
                   of the definstances constructs
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : All instances in the definstances
                   are evaluated (and created if
                   there are no errors)
                 Any previously existing instances
                 are deleted first.
  NOTES        : None
 ***************************************************/
static void ResetDefinstances()
  {
   DoForAllConstructs(ResetDefinstancesAction,DefinstancesModuleIndex,TRUE,NULL);
  }

/***************************************************
  NAME         : ResetDefinstancesAction
  DESCRIPTION  : Performs all the make-instance
                 calls in a definstances
  INPUTS       : 1) The definstances
                 2) User data buffer (ignored)
  RETURNS      : Nothing useful
  SIDE EFFECTS : Instances created
  NOTES        : None
 ***************************************************/
#if IBM_TBC
#pragma argsused
#endif
static void ResetDefinstancesAction(
  struct constructHeader *vDefinstances,
  void *userBuffer)
  {
#if MAC_MPW || MAC_MCW || IBM_MCW
#pragma unused(userBuffer)
#endif
   DEFINSTANCES *theDefinstances = (DEFINSTANCES *) vDefinstances;
   EXPRESSION *exp;
   DATA_OBJECT temp;

   SaveCurrentModule();
   SetCurrentModule((void *) vDefinstances->whichModule->theModule);
   theDefinstances->busy++;
   for (exp = theDefinstances->mkinstance ;
        exp != NULL ;
        exp = GetNextArgument(exp))
     {
      EvaluateExpression(exp,&temp);
      if (HaltExecution ||
          ((GetType(temp) == SYMBOL) &&
           (GetValue(temp) == FalseSymbol)))
        {
         RestoreCurrentModule();
         theDefinstances->busy--;
         return;
        }
     }
   theDefinstances->busy--;
   RestoreCurrentModule();
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



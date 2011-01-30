   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/09/97          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Generic Functions Parsing Routines               */
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
#include "setup.h"

#if DEFGENERIC_CONSTRUCT && (! BLOAD_ONLY) && (! RUN_TIME)

#if BLOAD || BLOAD_AND_BSAVE
#include "bload.h"
#endif

#if DEFFUNCTION_CONSTRUCT
#include "dffnxfun.h"
#endif

#if OBJECT_SYSTEM
#include "classfun.h"
#include "classcom.h"
#endif

#include "memalloc.h"
#include "cstrcpsr.h"
#include "exprnpsr.h"
#include "genrccom.h"
#include "immthpsr.h"
#include "modulutl.h"
#include "prcdrpsr.h"
#include "prccode.h"
#include "router.h"
#include "scanner.h"

#define _GENRCPSR_SOURCE_
#include "genrcpsr.h"

/* =========================================
   *****************************************
                   CONSTANTS
   =========================================
   ***************************************** */
#define HIGHER_PRECEDENCE -1
#define IDENTICAL          0
#define LOWER_PRECEDENCE   1

#define CURR_ARG_VAR "current-argument"

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

static BOOLEAN ValidGenericName(char *);
static SYMBOL_HN *ParseMethodNameAndIndex(char *,unsigned *);

#if DEBUGGING_FUNCTIONS
static void CreateDefaultGenericPPForm(DEFGENERIC *);
#endif

static int ParseMethodParameters(char *,EXPRESSION **,SYMBOL_HN **);
static RESTRICTION *ParseRestriction(char *);
static void ReplaceCurrentArgRefs(EXPRESSION *);
static int DuplicateParameters(EXPRESSION *,EXPRESSION **,SYMBOL_HN *);
static EXPRESSION *AddParameter(EXPRESSION *,EXPRESSION *,SYMBOL_HN *,RESTRICTION *);
static EXPRESSION *ValidType(SYMBOL_HN *);
static BOOLEAN RedundantClasses(void *,void *);
static DEFGENERIC *AddGeneric(SYMBOL_HN *,int *);
static DEFMETHOD *AddGenericMethod(DEFGENERIC *,int,unsigned);
static int RestrictionsCompare(EXPRESSION *,int,int,int,DEFMETHOD *);
static int TypeListCompare(RESTRICTION *,RESTRICTION *);
static DEFGENERIC *NewGeneric(SYMBOL_HN *);

/* =========================================
   *****************************************
      INTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */
Thread static struct token GenericInputToken;

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***************************************************************************
  NAME         : ParseDefgeneric
  DESCRIPTION  : Parses the defgeneric construct
  INPUTS       : The input logical name
  RETURNS      : FALSE if successful parse, TRUE otherwise
  SIDE EFFECTS : Inserts valid generic function defn into generic entry
  NOTES        : H/L Syntax :
                 (defgeneric <name> [<comment>])
 ***************************************************************************/
globle BOOLEAN ParseDefgeneric(
  char *readSource)
  {
   SYMBOL_HN *gname;
   DEFGENERIC *gfunc;
   int newGeneric;

   SetPPBufferStatus(ON);
   FlushPPBuffer();
   SavePPBuffer("(defgeneric ");
   SetIndentDepth(3);

#if BLOAD || BLOAD_AND_BSAVE
   if ((Bloaded() == TRUE) && (! CheckSyntaxMode))
     {
      CannotLoadWithBloadMessage("defgeneric");
      return(TRUE);
     }
#endif

   gname = GetConstructNameAndComment(readSource,&GenericInputToken,"defgeneric",
                                      FindDefgeneric,NULL,"^",TRUE,
                                      TRUE,TRUE);
   if (gname == NULL)
     return(TRUE);

   if (ValidGenericName(ValueToString(gname)) == FALSE)
     return(TRUE);

   if (GenericInputToken.type != RPAREN)
     {
      PrintErrorID("GENRCPSR",1,FALSE);
      PrintRouter(WERROR,"Expected ')' to complete defgeneric.\n");
      return(TRUE);
     }
   SavePPBuffer("\n");

   /* ========================================================
      If we're only checking syntax, don't add the
      successfully parsed deffacts to the KB.
      ======================================================== */

   if (CheckSyntaxMode)
     { return(FALSE); }

   gfunc = AddGeneric(gname,&newGeneric);

#if DEBUGGING_FUNCTIONS
   SetDefgenericPPForm((void *) gfunc,GetConserveMemory() ? NULL : CopyPPBuffer());
#endif
   return(FALSE);
  }

/***************************************************************************
  NAME         : ParseDefmethod
  DESCRIPTION  : Parses the defmethod construct
  INPUTS       : The input logical name
  RETURNS      : FALSE if successful parse, TRUE otherwise
  SIDE EFFECTS : Inserts valid method definition into generic entry
  NOTES        : H/L Syntax :
                 (defmethod <name> [<index>] [<comment>]
                    (<restriction>* [<wildcard>])
                    <action>*)
                 <restriction> :== ?<name> |
                                   (?<name> <type>* [<restriction-query>])
                 <wildcard>    :== $?<name> |
                                   ($?<name> <type>* [<restriction-query>])
 ***************************************************************************/
globle BOOLEAN ParseDefmethod(
  char *readSource)
  {
   SYMBOL_HN *gname;
   int rcnt,mposn,mi,newMethod,mnew = FALSE,lvars,error;
   EXPRESSION *params,*actions,*tmp;
   SYMBOL_HN *wildcard;
   DEFMETHOD *meth;
   DEFGENERIC *gfunc;
   unsigned index;

   SetPPBufferStatus(ON);
   FlushPPBuffer();
   SetIndentDepth(3);
   SavePPBuffer("(defmethod ");

#if BLOAD || BLOAD_AND_BSAVE
   if ((Bloaded() == TRUE) && (! CheckSyntaxMode))
     {
      CannotLoadWithBloadMessage("defmethod");
      return(TRUE);
     }
#endif

   gname = ParseMethodNameAndIndex(readSource,&index);
   if (gname == NULL)
     return(TRUE);

   if (ValidGenericName(ValueToString(gname)) == FALSE)
     return(TRUE);

   /* ========================================================
      Go ahead and add the header so that the generic function
      can be called recursively
      ======================================================== */
   gfunc = AddGeneric(gname,&newMethod);

#if DEBUGGING_FUNCTIONS
   if (newMethod && (! CheckSyntaxMode))
      CreateDefaultGenericPPForm(gfunc);
#endif

   IncrementIndentDepth(1);
   rcnt = ParseMethodParameters(readSource,&params,&wildcard);
   DecrementIndentDepth(1);
   if (rcnt == -1)
     goto DefmethodParseError;
   PPCRAndIndent();
   for (tmp = params ; tmp != NULL ; tmp = tmp->nextArg)
     {
      ReplaceCurrentArgRefs(((RESTRICTION *) tmp->argList)->query);
      if (ReplaceProcVars("method",((RESTRICTION *) tmp->argList)->query,
                                  params,wildcard,NULL,NULL))
        {
         DeleteTempRestricts(params);
         goto DefmethodParseError;
        }
     }
   meth = FindMethodByRestrictions(gfunc,params,rcnt,wildcard,&mposn);
   error = FALSE;
   if (meth != NULL)
     {
      if (meth->system)
        {
         PrintErrorID("GENRCPSR",17,FALSE);
         PrintRouter(WERROR,"Cannot replace the implicit system method #");
         PrintLongInteger(WERROR,(long) meth->index);
         PrintRouter(WERROR,".\n");
         error = TRUE;
        }
      else if ((index != 0) && (index != meth->index))
        {
         PrintErrorID("GENRCPSR",2,FALSE);
         PrintRouter(WERROR,"New method #");
         PrintLongInteger(WERROR,(long) index);
         PrintRouter(WERROR," would be indistinguishable from method #");
         PrintLongInteger(WERROR,(long) meth->index);
         PrintRouter(WERROR,".\n");
         error = TRUE;
        }
     }
   else if (index != 0)
     {
      mi = FindMethodByIndex(gfunc,index);
      if (mi == -1)
        mnew = TRUE;
      else if (gfunc->methods[mi].system)
        {
         PrintErrorID("GENRCPSR",17,FALSE);
         PrintRouter(WERROR,"Cannot replace the implicit system method #");
         PrintLongInteger(WERROR,(long) index);
         PrintRouter(WERROR,".\n");
         error = TRUE;
        }
     }
   else
     mnew = TRUE;
   if (error)
     {
      DeleteTempRestricts(params);
      goto DefmethodParseError;
     }
   ReturnContext = TRUE;
   actions = ParseProcActions("method",readSource,
                              &GenericInputToken,params,wildcard,
                              NULL,NULL,&lvars,NULL);
   if (actions == NULL)
     {
      DeleteTempRestricts(params);
      goto DefmethodParseError;
     }

   /*==============================================*/
   /* If we're only checking syntax, don't add the */
   /* successfully parsed deffunction to the KB.   */
   /*==============================================*/

   if (CheckSyntaxMode)
     {
      DeleteTempRestricts(params);
      ReturnPackedExpression(actions);
      if (newMethod)
        {
         RemoveConstructFromModule((struct constructHeader *) gfunc);
         RemoveDefgeneric((struct constructHeader *) gfunc);
        }
      return(FALSE);
     }

   PPBackup();
   PPBackup();
   SavePPBuffer(GenericInputToken.print_rep);
   SavePPBuffer("\n");

#if DEBUGGING_FUNCTIONS
   meth = AddMethod(gfunc,meth,mposn,index,params,rcnt,lvars,wildcard,actions,
             GetConserveMemory() ? NULL : CopyPPBuffer(),FALSE);
#else
   meth = AddMethod(gfunc,meth,mposn,index,params,rcnt,lvars,wildcard,actions,NULL,FALSE);
#endif
   DeleteTempRestricts(params);
   if (GetPrintWhileLoading() && GetCompilationsWatch())
     {
      PrintRouter(WDIALOG,"   Method #");
      PrintLongInteger(WDIALOG,(long) meth->index);
      PrintRouter(WDIALOG,mnew ? " defined.\n" : " redefined.\n");
     }
   return(FALSE);

DefmethodParseError:
   if (newMethod)
     {
      RemoveConstructFromModule((struct constructHeader *) gfunc);
      RemoveDefgeneric((void *) gfunc);
     }
   return(TRUE);
  }

/************************************************************************
  NAME         : AddMethod
  DESCRIPTION  : (Re)defines a new method for a generic
                 If method already exists, deletes old information
                    before proceeding.
  INPUTS       : 1) The generic address
                 2) The old method address (can be NULL)
                 3) The old method array position (can be -1)
                 4) The method index to assign (0 if don't care)
                 5) The parameter expression-list
                    (restrictions attached to argList pointers)
                 6) The number of restrictions
                 7) The number of locals vars reqd
                 8) The wildcard symbol (NULL if none)
                 9) Method actions
                 10) Method pretty-print form
                 11) A flag indicating whether to copy the
                     restriction types or just use the pointers
  RETURNS      : The new (old) method address
  SIDE EFFECTS : Method added to (or changed in) method array for generic
                 Restrictions repacked into new method
                 Actions and pretty-print form attached
  NOTES        : Assumes if a method is being redefined, its busy
                   count is 0!!
                 IMPORTANT: Expects that FindMethodByRestrictions() has
                   previously been called to determine if this method
                   is already present or not.  Arguments #1 and #2
                   should be the values obtained from FindMethod...().
 ************************************************************************/
globle DEFMETHOD *AddMethod(
  DEFGENERIC *gfunc,
  DEFMETHOD *meth,
  int mposn,
  unsigned mi,
  EXPRESSION *params,
  int rcnt,
  int lvars,
  SYMBOL_HN *wildcard,
  EXPRESSION *actions,
  char *ppForm,
  int copyRestricts)
  {
   RESTRICTION *rptr,*rtmp;
   register int i,j;
   int mai;

   SaveBusyCount(gfunc);
   if (meth == NULL)
     {
      mai = (mi != 0) ? FindMethodByIndex(gfunc,mi) : -1;
      if (mai == -1)
        meth = AddGenericMethod(gfunc,mposn,mi);
      else
        {
         DeleteMethodInfo(gfunc,&gfunc->methods[mai]);
         if (mai < mposn)
           {
            mposn--;
            for (i = mai+1 ; i <= mposn ; i++)
              GenCopyMemory(DEFMETHOD,1,&gfunc->methods[i-1],&gfunc->methods[i]);
           }
         else
           {
            for (i = mai-1 ; i >= mposn ; i--)
              GenCopyMemory(DEFMETHOD,1,&gfunc->methods[i+1],&gfunc->methods[i]);
           }
         meth = &gfunc->methods[mposn];
         meth->index = mi;
        }
     }
   else
     {
      /* ================================
         The old trace state is preserved
         ================================ */
      ExpressionDeinstall(meth->actions);
      ReturnPackedExpression(meth->actions);
      if (meth->ppForm != NULL)
        rm((void *) meth->ppForm,(int) (sizeof(char) * (strlen(meth->ppForm)+1)));
     }
   meth->system = 0;
   meth->actions = actions;
   ExpressionInstall(meth->actions);
   meth->ppForm = ppForm;
   if (mposn == -1)
     {
      RestoreBusyCount(gfunc);
      return(meth);
     }

   meth->localVarCount = lvars;
   meth->restrictionCount = rcnt;
   if (wildcard != NULL)
     {
      meth->minRestrictions = rcnt-1;
      meth->maxRestrictions = -1;
     }
   else
     meth->minRestrictions = meth->maxRestrictions = rcnt;
   if (rcnt != 0)
     meth->restrictions = (RESTRICTION *)
                          gm2((int) (sizeof(RESTRICTION) * rcnt));
   else
     meth->restrictions = NULL;
   for (i = 0 ; i < rcnt ; i++)
     {
      rptr = &meth->restrictions[i];
      rtmp = (RESTRICTION *) params->argList;
      rptr->query = PackExpression(rtmp->query);
      rptr->tcnt = rtmp->tcnt;
      if (copyRestricts)
        {
         if (rtmp->types != NULL)
           {
            rptr->types = (void **) gm2((int) (rptr->tcnt * sizeof(void *)));
            GenCopyMemory(void *,rptr->tcnt,rptr->types,rtmp->types);
           }
         else
           rptr->types = NULL;
        }
      else
        {
         rptr->types = rtmp->types;

         /* =====================================================
            Make sure the types-array is not deallocated when the
              temporary restriction nodes are
            ===================================================== */
         rtmp->tcnt = 0;
         rtmp->types = NULL;
        }
      ExpressionInstall(rptr->query);
      for (j = 0 ; j < rptr->tcnt ; j++)
#if OBJECT_SYSTEM
        IncrementDefclassBusyCount(rptr->types[j]);
#else
        IncrementIntegerCount((INTEGER_HN *) rptr->types[j]);
#endif
      params = params->nextArg;
     }
   RestoreBusyCount(gfunc);
   return(meth);
  }

/*****************************************************
  NAME         : PackRestrictionTypes
  DESCRIPTION  : Takes the restriction type list
                   and packs it into a contiguous
                   array of void *.
  INPUTS       : 1) The restriction structure
                 2) The types expression list
  RETURNS      : Nothing useful
  SIDE EFFECTS : Array allocated & expressions freed
  NOTES        : None
 *****************************************************/
globle void PackRestrictionTypes(
  RESTRICTION *rptr,
  EXPRESSION *types)
  {
   EXPRESSION *tmp;
   register int i;

   rptr->tcnt = 0;
   for (tmp = types ; tmp != NULL ; tmp = tmp->nextArg)
     rptr->tcnt++;
   if (rptr->tcnt != 0)
     rptr->types = (void **) gm2((int) (sizeof(void *) * rptr->tcnt));
   else
     rptr->types = NULL;
   for (i = 0 , tmp = types ; i < rptr->tcnt ; i++ , tmp = tmp->nextArg)
     rptr->types[i] = (void *) tmp->value;
   ReturnExpression(types);
  }

/***************************************************
  NAME         : DeleteTempRestricts
  DESCRIPTION  : Deallocates the method
                   temporary parameter list
  INPUTS       : The head of the list
  RETURNS      : Nothing useful
  SIDE EFFECTS : List deallocated
  NOTES        : None
 ***************************************************/
globle void DeleteTempRestricts(
  EXPRESSION *phead)
  {
   EXPRESSION *ptmp;
   RESTRICTION *rtmp;

   while (phead != NULL)
     {
      ptmp = phead;
      phead = phead->nextArg;
      rtmp = (RESTRICTION *) ptmp->argList;
      rtn_struct(expr,ptmp);
      ReturnExpression(rtmp->query);
      if (rtmp->tcnt != 0)
        rm((void *) rtmp->types,(int) (sizeof(void *) * rtmp->tcnt));
      rtn_struct(restriction,rtmp);
     }
  }

/**********************************************************
  NAME         : FindMethodByRestrictions
  DESCRIPTION  : See if a method for the specified
                   generic satsifies the given restrictions
  INPUTS       : 1) Generic function
                 2) Parameter/restriction expression list
                 3) Number of restrictions
                 4) Wildcard symbol (can be NULL)
                 5) Caller's buffer for holding array posn
                      of where to add new generic method
                      (-1 if method already present)
  RETURNS      : The address of the found method, NULL if
                    not found
  SIDE EFFECTS : Sets the caller's buffer to the index of
                   where to place the new method, -1 if
                   already present
  NOTES        : None
 **********************************************************/
globle DEFMETHOD *FindMethodByRestrictions(
  DEFGENERIC *gfunc,
  EXPRESSION *params,
  int rcnt,
  SYMBOL_HN *wildcard,
  int *posn)
  {
   register int i,cmp;
   int min,max;

   if (wildcard != NULL)
     {
      min = rcnt-1;
      max = -1;
     }
   else
     min = max = rcnt;
   for (i = 0 ; i < gfunc->mcnt ; i++)
     {
      cmp = RestrictionsCompare(params,rcnt,min,max,&gfunc->methods[i]);
      if (cmp == IDENTICAL)
        {
         *posn = -1;
         return(&gfunc->methods[i]);
        }
      else if (cmp == HIGHER_PRECEDENCE)
        {
         *posn = i;
         return(NULL);
        }
     }
   *posn = i;
   return(NULL);
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***********************************************************
  NAME         : ValidGenericName
  DESCRIPTION  : Determines if a particular function name
                    can be overloaded
  INPUTS       : The name
  RETURNS      : TRUE if OK, FALSE otherwise
  SIDE EFFECTS : Error message printed
  NOTES        : GetConstructNameAndComment() (called before
                 this function) ensures that the defgeneric
                 name does not conflict with one from
                 another module
 ***********************************************************/
static BOOLEAN ValidGenericName(
  char *theDefgenericName)
  {
   struct constructHeader *theDefgeneric;
#if DEFFUNCTION_CONSTRUCT
   struct defmodule *theModule;
   struct constructHeader *theDeffunction;
#endif
   struct FunctionDefinition *systemFunction;

   /* ============================================
      A defgeneric cannot be named the same as a
      construct type, e.g, defclass, defrule, etc.
      ============================================ */
   if (FindConstruct(theDefgenericName) != NULL)
     {
      PrintErrorID("GENRCPSR",3,FALSE);
      PrintRouter(WERROR,"Defgenerics are not allowed to replace constructs.\n");
      return(FALSE);
     }

#if DEFFUNCTION_CONSTRUCT
   /* ========================================
      A defgeneric cannot be named the same as
      a defffunction (either in this module or
      imported from another)
      ======================================== */
   theDeffunction =
      (struct constructHeader *) LookupDeffunctionInScope(theDefgenericName);
   if (theDeffunction != NULL)
     {
      theModule = GetConstructModuleItem(theDeffunction)->theModule;
      if (theModule != ((struct defmodule *) GetCurrentModule()))
        {
         PrintErrorID("GENRCPSR",4,FALSE);
         PrintRouter(WERROR,"Deffunction ");
         PrintRouter(WERROR,GetDeffunctionName((void *) theDeffunction));
         PrintRouter(WERROR," imported from module ");
         PrintRouter(WERROR,GetDefmoduleName((void *) theModule));
         PrintRouter(WERROR," conflicts with this defgeneric.\n");
         return(FALSE);
        }
      else
        {
         PrintErrorID("GENRCPSR",5,FALSE);
         PrintRouter(WERROR,"Defgenerics are not allowed to replace deffunctions.\n");
        }
      return(FALSE);
     }
#endif

   /* =========================================
      See if the defgeneric already exists in
      this module (or is imported from another)
      ========================================= */
   theDefgeneric = (struct constructHeader *) FindDefgeneric(theDefgenericName);
   if (theDefgeneric != NULL)
     {
      /* ===========================================
         And the redefinition of a defgeneric in
         the current module is only valid if none
         of its methods are executing
         =========================================== */
      if (MethodsExecuting((DEFGENERIC *) theDefgeneric))
        {
         MethodAlterError((DEFGENERIC *) theDefgeneric);
         return(FALSE);
        }
     }

   /* =======================================
      Only certain specific system functions
      may be overloaded by generic functions
      ======================================= */
   systemFunction = FindFunction(theDefgenericName);
   if ((systemFunction != NULL) ?
       (systemFunction->overloadable == FALSE) : FALSE)
     {
      PrintErrorID("GENRCPSR",16,FALSE);
      PrintRouter(WERROR,"The system function ");
      PrintRouter(WERROR,theDefgenericName);
      PrintRouter(WERROR," cannot be overloaded.\n");
      return(FALSE);
     }
   return(TRUE);
  }

#if DEBUGGING_FUNCTIONS

/***************************************************
  NAME         : CreateDefaultGenericPPForm
  DESCRIPTION  : Adds a default pretty-print form
                 for a gneric function when it is
                 impliciylt created by the defn
                 of its first method
  INPUTS       : The generic function
  RETURNS      : Nothing useful
  SIDE EFFECTS : Pretty-print form created and
                 attached.
  NOTES        : None
 ***************************************************/
static void CreateDefaultGenericPPForm(
  DEFGENERIC *gfunc)
  {
   char *moduleName,*genericName,*buf;

   moduleName = GetDefmoduleName((void *) ((struct defmodule *) GetCurrentModule()));
   genericName = GetDefgenericName((void *) gfunc);
   buf = (char *) gm2((int) (sizeof(char) * (strlen(moduleName) + strlen(genericName) + 17)));
   sprintf(buf,"(defgeneric %s::%s)\n",moduleName,genericName);
   SetDefgenericPPForm((void *) gfunc,buf);
  }

#endif

/*******************************************************
  NAME         : ParseMethodNameAndIndex
  DESCRIPTION  : Parses the name of the method and
                   optional method index
  INPUTS       : 1) The logical name of the input source
                 2) Caller's buffer for method index
                    (0 if not specified)
  RETURNS      : The symbolic name of the method
  SIDE EFFECTS : None
  NOTES        : Assumes "(defmethod " already parsed
 *******************************************************/
static SYMBOL_HN *ParseMethodNameAndIndex(
  char *readSource,
  unsigned *index)
  {
   SYMBOL_HN *gname;

   *index = 0;
   gname = GetConstructNameAndComment(readSource,&GenericInputToken,"defgeneric",
                                      FindDefgeneric,NULL,"&",TRUE,FALSE,TRUE);
   if (gname == NULL)
     return(NULL);
   if (GetType(GenericInputToken) == INTEGER)
     {
      int tmp;

      PPBackup();
      PPBackup();
      SavePPBuffer(" ");
      SavePPBuffer(GenericInputToken.print_rep);
      tmp = (int) ValueToLong(GetValue(GenericInputToken));
      if (tmp < 1)
        {
         PrintErrorID("GENRCPSR",6,FALSE);
         PrintRouter(WERROR,"Method index out of range.\n");
         return(NULL);
        }
      *index = (unsigned) tmp;
      PPCRAndIndent();
      GetToken(readSource,&GenericInputToken);
     }
   if (GetType(GenericInputToken) == STRING)
     {
      PPBackup();
      PPBackup();
      SavePPBuffer(" ");
      SavePPBuffer(GenericInputToken.print_rep);
      PPCRAndIndent();
      GetToken(readSource,&GenericInputToken);
     }
   return(gname);
  }

/************************************************************************
  NAME         : ParseMethodParameters
  DESCRIPTION  : Parses method restrictions
                   (parameter names with class and expression specifiers)
  INPUTS       : 1) The logical name of the input source
                 2) Caller's buffer for the parameter name list
                    (Restriction structures are attached to
                     argList pointers of parameter nodes)
                 3) Caller's buffer for wildcard symbol (if any)
  RETURNS      : The number of parameters, or -1 on errors
  SIDE EFFECTS : Memory allocated for parameters and restrictions
                 Parameter names stored in expression list
                 Parameter restrictions stored in contiguous array
  NOTES        : Any memory allocated is freed on errors
                 Assumes first opening parenthesis has been scanned
 ************************************************************************/
static int ParseMethodParameters(
  char *readSource,
  EXPRESSION **params,
  SYMBOL_HN **wildcard)
  {
   EXPRESSION *phead = NULL,*pprv;
   SYMBOL_HN *pname;
   RESTRICTION *rtmp;
   int rcnt = 0;

   *wildcard = NULL;
   *params = NULL;
   if (GetType(GenericInputToken) != LPAREN)
     {
      PrintErrorID("GENRCPSR",7,FALSE);
      PrintRouter(WERROR,"Expected a '(' to begin method parameter restrictions.\n");
      return(-1);
     }
   GetToken(readSource,&GenericInputToken);
   while (GenericInputToken.type != RPAREN)
     {
      if (*wildcard != NULL)
        {
         DeleteTempRestricts(phead);
         PrintErrorID("PRCCODE",8,FALSE);
         PrintRouter(WERROR,"No parameters allowed after wildcard parameter.\n");
         return(-1);
        }
      if ((GenericInputToken.type == SF_VARIABLE) || (GenericInputToken.type == MF_VARIABLE))
        {
         pname = (SYMBOL_HN *) GenericInputToken.value;
         if (DuplicateParameters(phead,&pprv,pname))
           {
            DeleteTempRestricts(phead);
            return(-1);
           }
         if (GenericInputToken.type == MF_VARIABLE)
           *wildcard = pname;
         rtmp = get_struct(restriction);
         PackRestrictionTypes(rtmp,NULL);
         rtmp->query = NULL;
         phead = AddParameter(phead,pprv,pname,rtmp);
         rcnt++;
        }
      else if (GenericInputToken.type == LPAREN)
        {
         GetToken(readSource,&GenericInputToken);
         if ((GenericInputToken.type != SF_VARIABLE) &&
             (GenericInputToken.type != MF_VARIABLE))
           {
            DeleteTempRestricts(phead);
            PrintErrorID("GENRCPSR",8,FALSE);
            PrintRouter(WERROR,"Expected a variable for parameter specification.\n");
            return(-1);
           }
         pname = (SYMBOL_HN *) GenericInputToken.value;
         if (DuplicateParameters(phead,&pprv,pname))
           {
            DeleteTempRestricts(phead);
            return(-1);
           }
         if (GenericInputToken.type == MF_VARIABLE)
           *wildcard = pname;
         SavePPBuffer(" ");
         rtmp = ParseRestriction(readSource);
         if (rtmp == NULL)
           {
            DeleteTempRestricts(phead);
            return(-1);
           }
         phead = AddParameter(phead,pprv,pname,rtmp);
         rcnt++;
        }
      else
        {
         DeleteTempRestricts(phead);
         PrintErrorID("GENRCPSR",9,FALSE);
         PrintRouter(WERROR,"Expected a variable or '(' for parameter specification.\n");
         return(-1);
        }
      PPCRAndIndent();
      GetToken(readSource,&GenericInputToken);
     }
   if (rcnt != 0)
     {
      PPBackup();
      PPBackup();
      SavePPBuffer(")");
     }
   *params = phead;
   return(rcnt);
  }

/************************************************************
  NAME         : ParseRestriction
  DESCRIPTION  : Parses the restriction for a parameter of a
                   method
                 This restriction is comprised of:
                   1) A list of classes (or types) that are
                      allowed for the parameter (None
                      if no type restriction)
                   2) And an optional restriction-query
                      expression
  INPUTS       : The logical name of the input source
  RETURNS      : The address of a RESTRICTION node, NULL on
                   errors
  SIDE EFFECTS : RESTRICTION node allocated
                   Types are in a contiguous array of void *
                   Query is an expression
  NOTES        : Assumes "(?<var> " has already been parsed
                 H/L Syntax: <type>* [<query>])
 ************************************************************/
static RESTRICTION *ParseRestriction(
  char *readSource)
  {
   EXPRESSION *types = NULL,*new_types,
              *typesbot,*tmp,*tmp2,
              *query = NULL;
   RESTRICTION *rptr;

   GetToken(readSource,&GenericInputToken);
   while (GenericInputToken.type != RPAREN)
     {
      if (query != NULL)
        {
         PrintErrorID("GENRCPSR",10,FALSE);
         PrintRouter(WERROR,"Query must be last in parameter restriction.\n");
         ReturnExpression(query);
         ReturnExpression(types);
         return(NULL);
        }
      if (GenericInputToken.type == SYMBOL)
        {
         new_types = ValidType((SYMBOL_HN *) GenericInputToken.value);
         if (new_types == NULL)
           {
            ReturnExpression(types);
            ReturnExpression(query);
            return(NULL);
           }
         if (types == NULL)
           types = new_types;
         else
           {
            for (typesbot = tmp = types ; tmp != NULL ; tmp = tmp->nextArg)
              {
               for (tmp2 = new_types ; tmp2 != NULL ; tmp2 = tmp2->nextArg)
                 {
                  if (tmp->value == tmp2->value)
                    {
                     PrintErrorID("GENRCPSR",11,FALSE);
#if OBJECT_SYSTEM
                     PrintRouter(WERROR,"Duplicate classes not allowed in parameter restriction.\n");
#else
                     PrintRouter(WERROR,"Duplicate types not allowed in parameter restriction.\n");
#endif
                     ReturnExpression(query);
                     ReturnExpression(types);
                     ReturnExpression(new_types);
                     return(NULL);
                    }
                  if (RedundantClasses(tmp->value,tmp2->value))
                    {
                     ReturnExpression(query);
                     ReturnExpression(types);
                     ReturnExpression(new_types);
                     return(NULL);
                    }
                 }
               typesbot = tmp;
              }
            typesbot->nextArg = new_types;
           }
        }
      else if (GenericInputToken.type == LPAREN)
        {
         query = Function1Parse(readSource);
         if (query == NULL)
           {
            ReturnExpression(types);
            return(NULL);
           }
         if (GetParsedBindNames() != NULL)
           {
            PrintErrorID("GENRCPSR",12,FALSE);
            PrintRouter(WERROR,"Binds are not allowed in query expressions.\n");
            ReturnExpression(query);
            ReturnExpression(types);
            return(NULL);
           }
        }
#if DEFGLOBAL_CONSTRUCT
      else if (GenericInputToken.type == GBL_VARIABLE)
        query = GenConstant(GBL_VARIABLE,GenericInputToken.value);
#endif
      else
        {
         PrintErrorID("GENRCPSR",13,FALSE);
#if OBJECT_SYSTEM
         PrintRouter(WERROR,"Expected a valid class name or query.\n");
#else
         PrintRouter(WERROR,"Expected a valid type name or query.\n");
#endif
         ReturnExpression(query);
         ReturnExpression(types);
         return(NULL);
        }
      SavePPBuffer(" ");
      GetToken(readSource,&GenericInputToken);
     }
   PPBackup();
   PPBackup();
   SavePPBuffer(")");
   if ((types == NULL) && (query == NULL))
     {
      PrintErrorID("GENRCPSR",13,FALSE);
#if OBJECT_SYSTEM
      PrintRouter(WERROR,"Expected a valid class name or query.\n");
#else
      PrintRouter(WERROR,"Expected a valid type name or query.\n");
#endif
      return(NULL);
     }
   rptr = get_struct(restriction);
   rptr->query = query;
   PackRestrictionTypes(rptr,types);
   return(rptr);
  }

/*****************************************************************
  NAME         : ReplaceCurrentArgRefs
  DESCRIPTION  : Replaces all references to ?current-argument in
                  method parameter queries with special calls
                  to (gnrc-current-arg)
  INPUTS       : The query expression
  RETURNS      : Nothing useful
  SIDE EFFECTS : Variable references to ?current-argument replaced
  NOTES        : None
 *****************************************************************/
static void ReplaceCurrentArgRefs(
  EXPRESSION *query)
  {
   while (query != NULL)
     {
      if ((query->type != SF_VARIABLE) ? FALSE :
          (strcmp(ValueToString(query->value),CURR_ARG_VAR) == 0))
        {
         query->type = FCALL;
         query->value = (void *) FindFunction("(gnrc-current-arg)");
        }
      if (query->argList != NULL)
        ReplaceCurrentArgRefs(query->argList);
      query = query->nextArg;
     }
  }

/**********************************************************
  NAME         : DuplicateParameters
  DESCRIPTION  : Examines the parameter expression
                   chain for a method looking duplicates.
  INPUTS       : 1) The parameter chain (can be NULL)
                 2) Caller's buffer for address of
                    last node searched (can be used to
                    later attach new parameter)
                 3) The name of the parameter being checked
  RETURNS      : TRUE if duplicates found, FALSE otherwise
  SIDE EFFECTS : Caller's prv address set
  NOTES        : Assumes all parameter list nodes are WORDS
 **********************************************************/
static int DuplicateParameters(
  EXPRESSION *head,
  EXPRESSION **prv,
  SYMBOL_HN *name)
  {
   *prv = NULL;
   while (head != NULL)
     {
      if (head->value == (void *) name)
        {
         PrintErrorID("PRCCODE",7,FALSE);
         PrintRouter(WERROR,"Duplicate parameter names not allowed.\n");
         return(TRUE);
        }
      *prv = head;
      head = head->nextArg;
     }
   return(FALSE);
  }

/*****************************************************************
  NAME         : AddParameter
  DESCRIPTION  : Shoves a new paramter with its restriction
                   onto the list for a method
                 The parameter list is a list of expressions
                   linked by neext_arg pointers, and the
                   argList pointers are used for the restrictions
  INPUTS       : 1) The head of the list
                 2) The bottom of the list
                 3) The parameter name
                 4) The parameter restriction
  RETURNS      : The (new) head of the list
  SIDE EFFECTS : New parameter expression node allocated, set,
                   and attached
  NOTES        : None
 *****************************************************************/
static EXPRESSION *AddParameter(
  EXPRESSION *phead,
  EXPRESSION *pprv,
  SYMBOL_HN *pname,
  RESTRICTION *rptr)
  {
   EXPRESSION *ptmp;

   ptmp = GenConstant(SYMBOL,(void *) pname);
   if (phead == NULL)
     phead = ptmp;
   else
     pprv->nextArg = ptmp;
   ptmp->argList = (EXPRESSION *) rptr;
   return(phead);
  }

/**************************************************************
  NAME         : ValidType
  DESCRIPTION  : Examines the name of a restriction type and
                   forms a list of integer-code expressions
                   corresponding to the primitive types
                 (or a Class address if COOL is installed)
  INPUTS       : The type name
  RETURNS      : The expression chain (NULL on errors)
  SIDE EFFECTS : Expression type chain allocated
                   one or more nodes holding codes for types
                   (or class addresses)
  NOTES        : None
 *************************************************************/
static EXPRESSION *ValidType(
  SYMBOL_HN *tname)
  {
#if OBJECT_SYSTEM
   DEFCLASS *cls;

   if (FindModuleSeparator(ValueToString(tname)))
     IllegalModuleSpecifierMessage();
   else
     {
      cls = LookupDefclassInScope(ValueToString(tname));
      if (cls == NULL)
        {
         PrintErrorID("GENRCPSR",14,FALSE);
         PrintRouter(WERROR,"Unknown class in method.\n");
         return(NULL);
        }
      return(GenConstant(EXTERNAL_ADDRESS,(void *) cls));
     }
#else
   if (strcmp(ValueToString(tname),INTEGER_TYPE_NAME) == 0)
     return(GenConstant(INTEGER,(void *) AddLong((long) INTEGER)));
   if (strcmp(ValueToString(tname),FLOAT_TYPE_NAME) == 0)
     return(GenConstant(INTEGER,(void *) AddLong((long) FLOAT)));
   if (strcmp(ValueToString(tname),SYMBOL_TYPE_NAME) == 0)
     return(GenConstant(INTEGER,(void *) AddLong((long) SYMBOL)));
   if (strcmp(ValueToString(tname),STRING_TYPE_NAME) == 0)
     return(GenConstant(INTEGER,(void *) AddLong((long) STRING)));
   if (strcmp(ValueToString(tname),MULTIFIELD_TYPE_NAME) == 0)
     return(GenConstant(INTEGER,(void *) AddLong((long) MULTIFIELD)));
   if (strcmp(ValueToString(tname),EXTERNAL_ADDRESS_TYPE_NAME) == 0)
     return(GenConstant(INTEGER,(void *) AddLong((long) EXTERNAL_ADDRESS)));
   if (strcmp(ValueToString(tname),FACT_ADDRESS_TYPE_NAME) == 0)
     return(GenConstant(INTEGER,(void *) AddLong((long) FACT_ADDRESS)));
   if (strcmp(ValueToString(tname),NUMBER_TYPE_NAME) == 0)
     return(GenConstant(INTEGER,(void *) AddLong((long) NUMBER_TYPE_CODE)));
   if (strcmp(ValueToString(tname),LEXEME_TYPE_NAME) == 0)
     return(GenConstant(INTEGER,(void *) AddLong((long) LEXEME_TYPE_CODE)));
   if (strcmp(ValueToString(tname),ADDRESS_TYPE_NAME) == 0)
     return(GenConstant(INTEGER,(void *) AddLong((long) ADDRESS_TYPE_CODE)));
   if (strcmp(ValueToString(tname),PRIMITIVE_TYPE_NAME) == 0)
     return(GenConstant(INTEGER,(void *) AddLong((long) PRIMITIVE_TYPE_CODE)));
   if (strcmp(ValueToString(tname),OBJECT_TYPE_NAME) == 0)
     return(GenConstant(INTEGER,(void *) AddLong((long) OBJECT_TYPE_CODE)));
   if (strcmp(ValueToString(tname),INSTANCE_TYPE_NAME) == 0)
     return(GenConstant(INTEGER,(void *) AddLong((long) INSTANCE_TYPE_CODE)));
   if (strcmp(ValueToString(tname),INSTANCE_NAME_TYPE_NAME) == 0)
     return(GenConstant(INTEGER,(void *) AddLong((long) INSTANCE_NAME)));
   if (strcmp(ValueToString(tname),INSTANCE_ADDRESS_TYPE_NAME) == 0)
     return(GenConstant(INTEGER,(void *) AddLong((long) INSTANCE_ADDRESS)));

   PrintErrorID("GENRCPSR",14,FALSE);
   PrintRouter(WERROR,"Unknown type in method.\n");
#endif
   return(NULL);
  }

/*************************************************************
  NAME         : RedundantClasses
  DESCRIPTION  : Determines if one class (type) is
                 subsumes (or is subsumed by) another.
  INPUTS       : Two void pointers which are class pointers
                 if COOL is installed or integer hash nodes
                 for type codes otherwise.
  RETURNS      : TRUE if there is subsumption, FALSE otherwise
  SIDE EFFECTS : An error message is printed, if appropriate.
  NOTES        : None
 *************************************************************/
static BOOLEAN RedundantClasses(
  void *c1,
  void *c2)
  {
   char *tname;

#if OBJECT_SYSTEM
   if (HasSuperclass((DEFCLASS *) c1,(DEFCLASS *) c2))
     tname = GetDefclassName(c1);
   else if (HasSuperclass((DEFCLASS *) c2,(DEFCLASS *) c1))
     tname = GetDefclassName(c2);
#else
   if (SubsumeType(ValueToInteger(c1),ValueToInteger(c2)))
     tname = TypeName(ValueToInteger(c1));
   else if (SubsumeType(ValueToInteger(c2),ValueToInteger(c1)))
     tname = TypeName(ValueToInteger(c2));
#endif
   else
     return(FALSE);
   PrintErrorID("GENRCPSR",15,FALSE);
   PrintRouter(WERROR,tname);
   PrintRouter(WERROR," class is redundant.\n");
   return(TRUE);
  }

/*********************************************************
  NAME         : AddGeneric
  DESCRIPTION  : Inserts a new generic function
                   header into the generic list
  INPUTS       : 1) Symbolic name of the new generic
                 2) Caller's input buffer for flag
                      if added generic is new or not
  RETURNS      : The address of the new node, or
                   address of old node if already present
  SIDE EFFECTS : Generic header inserted
                 If the node is already present, it is
                   moved to the end of the list, otherwise
                   the new node is inserted at the end
  NOTES        : None
 *********************************************************/
static DEFGENERIC *AddGeneric(
  SYMBOL_HN *name,
  int *newGeneric)
  {
   DEFGENERIC *gfunc;

   gfunc = (DEFGENERIC *) FindDefgeneric(ValueToString(name));
   if (gfunc != NULL)
     {
      *newGeneric = FALSE;

      if (CheckSyntaxMode)
        { return(gfunc); }

      /* ================================
         The old trace state is preserved
         ================================ */
      RemoveConstructFromModule((struct constructHeader *) gfunc);
     }
   else
     {
      *newGeneric = TRUE;
      gfunc = NewGeneric(name);
      IncrementSymbolCount(name);
      AddImplicitMethods(gfunc);
     }
   AddConstructToModule((struct constructHeader *) gfunc);
   return(gfunc);
  }

/**********************************************************************
  NAME         : AddGenericMethod
  DESCRIPTION  : Inserts a blank method (with the method-index set)
                   into the specified position of the generic
                   method array
  INPUTS       : 1) The generic function
                 2) The index where to add the method in the array
                 3) The method user-index (0 if don't care)
  RETURNS      : The address of the new method
  SIDE EFFECTS : Fields initialized (index set) and new method inserted
                 Generic function new method-index set to specified
                   by user-index if > current new method-index
  NOTES        : None
 **********************************************************************/
static DEFMETHOD *AddGenericMethod(
  DEFGENERIC *gfunc,
  int mposn,
  unsigned mi)
  {
   DEFMETHOD *narr;
   register int b,e;

   narr = (DEFMETHOD *) gm2((int) (sizeof(DEFMETHOD) * (gfunc->mcnt+1)));
   for (b = e = 0 ; b < gfunc->mcnt ; b++ , e++)
     {
      if (b == mposn)
        e++;
      GenCopyMemory(DEFMETHOD,1,&narr[e],&gfunc->methods[b]);
     }
   if (mi == 0)
     narr[mposn].index = gfunc->new_index++;
   else
     {
      narr[mposn].index = mi;
      if (mi >= gfunc->new_index)
        gfunc->new_index = mi+1;
     }
   narr[mposn].busy = 0;
#if DEBUGGING_FUNCTIONS
   narr[mposn].trace = WatchMethods;
#endif
   narr[mposn].minRestrictions = 0;
   narr[mposn].maxRestrictions = 0;
   narr[mposn].restrictionCount = 0;
   narr[mposn].localVarCount = 0;
   narr[mposn].system = 0;
   narr[mposn].restrictions = NULL;
   narr[mposn].actions = NULL;
   narr[mposn].ppForm = NULL;
   narr[mposn].usrData = NULL;
   if (gfunc->mcnt != 0)
     rm((void *) gfunc->methods,(int) (sizeof(DEFMETHOD) * gfunc->mcnt));
   gfunc->mcnt++;
   gfunc->methods = narr;
   return(&narr[mposn]);
  }

/****************************************************************
  NAME         : RestrictionsCompare
  DESCRIPTION  : Compares the restriction-expression list
                   with an existing methods restrictions to
                   determine an ordering
  INPUTS       : 1) The parameter/restriction expression list
                 2) The total number of restrictions
                 3) The number of minimum restrictions
                 4) The number of maximum restrictions (-1
                    if unlimited)
                 5) The method with which to compare restrictions
  RETURNS      : A code representing how the method restrictions
                   -1 : New restrictions have higher precedence
                    0 : New restrictions are identical
                    1 : New restrictions have lower precedence
  SIDE EFFECTS : None
  NOTES        : The new restrictions are stored in the argList
                   pointers of the parameter expressions
 ****************************************************************/
static int RestrictionsCompare(
  EXPRESSION *params,
  int rcnt,
  int min,
  int max,
  DEFMETHOD *meth)
  {
   register int i;
   register RESTRICTION *r1,*r2;
   int diff = FALSE,rtn;

   for (i = 0 ; (i < rcnt) && (i < meth->restrictionCount) ; i++)
     {
      /* =============================================================
         A wildcard parameter always has lower precedence than
         a regular parameter, regardless of the class restriction list
         ============================================================= */
      if ((i == rcnt-1) && (max == -1) &&
          (meth->maxRestrictions != -1))
        return(LOWER_PRECEDENCE);
      if ((i == meth->restrictionCount-1) && (max != -1) &&
          (meth->maxRestrictions == -1))
        return(HIGHER_PRECEDENCE);

      /* =============================================================
         The parameter with the most specific type list has precedence
         ============================================================= */
      r1 = (RESTRICTION *) params->argList;
      r2 = &meth->restrictions[i];
      rtn = TypeListCompare(r1,r2);
      if (rtn != IDENTICAL)
        return(rtn);

      /* =====================================================
         The parameter with a query restriction has precedence
         ===================================================== */
      if ((r1->query == NULL) && (r2->query != NULL))
        return(LOWER_PRECEDENCE);
      if ((r1->query != NULL) && (r2->query == NULL))
        return(HIGHER_PRECEDENCE);

      /* ==========================================================
         Remember if the method restrictions differ at all - query
         expressions must be identical as well for the restrictions
         to be the same
         ========================================================== */
      if (IdenticalExpression(r1->query,r2->query) == FALSE)
        diff = TRUE;
      params = params->nextArg;
     }

   /* =============================================================
      If the methods have the same number of parameters here, they
      are either the same restrictions, or they differ only in
      the query restrictions
      ============================================================= */
   if (rcnt == meth->restrictionCount)
     return(diff ? LOWER_PRECEDENCE : IDENTICAL);

   /* =============================================
      The method with the greater number of regular
      parameters has precedence

      If they require the smae # of reg params,
      then one without a wildcard has precedence
      ============================================= */
   if (min > meth->minRestrictions)
     return(HIGHER_PRECEDENCE);
   if (meth->minRestrictions < min)
     return(LOWER_PRECEDENCE);
   return((max == - 1) ? LOWER_PRECEDENCE : HIGHER_PRECEDENCE);
  }

/*****************************************************
  NAME         : TypeListCompare
  DESCRIPTION  : Determines the precedence between
                   the class lists on two restrictions
  INPUTS       : 1) Restriction address #1
                 2) Restriction address #2
  RETURNS      : -1 : r1 precedes r2
                  0 : Identical classes
                  1 : r2 precedes r1
  SIDE EFFECTS : None
  NOTES        : None
 *****************************************************/
static int TypeListCompare(
  RESTRICTION *r1,
  RESTRICTION *r2)
  {
   register int i,diff = FALSE;

   if ((r1->tcnt == 0) && (r2->tcnt == 0))
     return(IDENTICAL);
   if (r1->tcnt == 0)
     return(LOWER_PRECEDENCE);
   if (r2->tcnt == 0)
     return(HIGHER_PRECEDENCE);
   for (i = 0 ; (i < r1->tcnt) && (i < r2->tcnt) ; i++)
     {
      if (r1->types[i] != r2->types[i])
        {
         diff = TRUE;
#if OBJECT_SYSTEM
         if (HasSuperclass((DEFCLASS *) r1->types[i],(DEFCLASS *) r2->types[i]))
           return(HIGHER_PRECEDENCE);
         if (HasSuperclass((DEFCLASS *) r2->types[i],(DEFCLASS *) r1->types[i]))
           return(LOWER_PRECEDENCE);
#else
         if (SubsumeType(ValueToInteger(r1->types[i]),ValueToInteger(r2->types[i])))
           return(HIGHER_PRECEDENCE);
         if (SubsumeType(ValueToInteger(r2->types[i]),ValueToInteger(r1->types[i])))
           return(LOWER_PRECEDENCE);
#endif
        }
     }
   if (r1->tcnt < r2->tcnt)
     return(HIGHER_PRECEDENCE);
   if (r1->tcnt > r2->tcnt)
     return(LOWER_PRECEDENCE);
   if (diff)
     return(LOWER_PRECEDENCE);
   return(IDENTICAL);
  }

/***************************************************
  NAME         : NewGeneric
  DESCRIPTION  : Allocates and initializes a new
                   generic function header
  INPUTS       : The name of the new generic
  RETURNS      : The address of the new generic
  SIDE EFFECTS : Generic function  header created
  NOTES        : None
 ***************************************************/
static DEFGENERIC *NewGeneric(
  SYMBOL_HN *gname)
  {
   DEFGENERIC *ngen;

   ngen = get_struct(defgeneric);
   InitializeConstructHeader("defgeneric",(struct constructHeader *) ngen,gname);
   ngen->busy = 0;
   ngen->new_index = 1;
   ngen->methods = NULL;
   ngen->mcnt = 0;
#if DEBUGGING_FUNCTIONS
   ngen->trace = WatchGenerics;
#endif
   return(ngen);
  }

#endif /* DEFGENERIC_CONSTRUCT && (! BLOAD_ONLY) && (! RUN_TIME) */

/***************************************************
  NAME         :
  DESCRIPTION  :
  INPUTS       :
  RETURNS      :
  SIDE EFFECTS :
  NOTES        :
 ***************************************************/

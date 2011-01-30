   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/09/97          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Deffunction Parsing Routines                     */
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

#if DEFFUNCTION_CONSTRUCT && (! BLOAD_ONLY) && (! RUN_TIME)

#if BLOAD || BLOAD_AND_BSAVE
#include "bload.h"
#endif

#if DEFRULE_CONSTRUCT
#include "network.h"
#endif

#if DEFGENERIC_CONSTRUCT
#include "genrccom.h"
#endif

#include "memalloc.h"
#include "constant.h"
#include "cstrcpsr.h"
#include "constrct.h"
#include "dffnxfun.h"
#include "expressn.h"
#include "exprnpsr.h"
#include "extnfunc.h"
#include "prccode.h"
#include "router.h"
#include "scanner.h"
#include "symbol.h"

#define _DFFNXPSR_SOURCE_
#include "dffnxpsr.h"

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

static BOOLEAN ValidDeffunctionName(char *);
static DEFFUNCTION *AddDeffunction(SYMBOL_HN *,EXPRESSION *,int,int,int,int);

/* =========================================
   *****************************************
      INTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */
Thread static struct token DFInputToken;

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***************************************************************************
  NAME         : ParseDeffunction
  DESCRIPTION  : Parses the deffunction construct
  INPUTS       : The input logical name
  RETURNS      : FALSE if successful parse, TRUE otherwise
  SIDE EFFECTS : Creates valid deffunction definition
  NOTES        : H/L Syntax :
                 (deffunction <name> [<comment>]
                    (<single-field-varible>* [<multifield-variable>])
                    <action>*)
 ***************************************************************************/
globle BOOLEAN ParseDeffunction(
  char *readSource)
  {
   SYMBOL_HN *deffunctionName;
   EXPRESSION *actions;
   EXPRESSION *parameterList;
   SYMBOL_HN *wildcard;
   int min,max,lvars,DeffunctionError = FALSE;
   short overwrite = FALSE, owMin, owMax;

   DEFFUNCTION *dptr;

   SetPPBufferStatus(ON);

   FlushPPBuffer();
   SetIndentDepth(3);
   SavePPBuffer("(deffunction ");

#if BLOAD || BLOAD_AND_BSAVE
   if ((Bloaded() == TRUE) && (! CheckSyntaxMode))
     {
      CannotLoadWithBloadMessage("deffunctions");
      return(TRUE);
     }
#endif

   /* =====================================================
      Parse the name and comment fields of the deffunction.
      ===================================================== */
   deffunctionName = GetConstructNameAndComment(readSource,&DFInputToken,"deffunction",
                                                FindDeffunction,NULL,
                                                "!",TRUE,TRUE,TRUE);
   if (deffunctionName == NULL)
     return(TRUE);

   if (ValidDeffunctionName(ValueToString(deffunctionName)) == FALSE)
     return(TRUE);

   /*==========================*/
   /* Parse the argument list. */
   /*==========================*/
   parameterList = ParseProcParameters(readSource,&DFInputToken,NULL,&wildcard,
                                       &min,&max,&DeffunctionError,NULL);
   if (DeffunctionError)
     return(TRUE);

   /*===================================================================*/
   /* Go ahead and add the deffunction so it can be recursively called. */
   /*===================================================================*/

   if (CheckSyntaxMode)
     {
      dptr = (DEFFUNCTION *) FindDeffunction(ValueToString(deffunctionName));
      if (dptr == NULL)
        { dptr = AddDeffunction(deffunctionName,NULL,min,max,0,TRUE); }
      else
        {
         overwrite = TRUE;
         owMin = (short) dptr->minNumberOfParameters;
         owMax = (short) dptr->maxNumberOfParameters;
         dptr->minNumberOfParameters = min;
         dptr->maxNumberOfParameters = max;
        }
     }
   else
     { dptr = AddDeffunction(deffunctionName,NULL,min,max,0,TRUE); }

   if (dptr == NULL)
     {
      ReturnExpression(parameterList);
      return(TRUE);
     }

   /*==================================================*/
   /* Parse the actions contained within the function. */
   /*==================================================*/

   PPCRAndIndent();

   ReturnContext = TRUE;
   actions = ParseProcActions("deffunction",readSource,
                              &DFInputToken,parameterList,wildcard,
                              NULL,NULL,&lvars,NULL);
   if (actions == NULL)
     {
      ReturnExpression(parameterList);
      if (overwrite)
        {
         dptr->minNumberOfParameters = owMin;
         dptr->maxNumberOfParameters = owMax;
        }

      if ((dptr->busy == 0) && (! overwrite))
        {
         RemoveConstructFromModule((struct constructHeader *) dptr);
         RemoveDeffunction(dptr);
        }
      return(TRUE);
     }

   /*==============================================*/
   /* If we're only checking syntax, don't add the */
   /* successfully parsed deffunction to the KB.   */
   /*==============================================*/

   if (CheckSyntaxMode)
     {
      ReturnExpression(parameterList);
      ReturnPackedExpression(actions);
      if (overwrite)
        {
         dptr->minNumberOfParameters = owMin;
         dptr->maxNumberOfParameters = owMax;
        }
      else
        {
         RemoveConstructFromModule((struct constructHeader *) dptr);
         RemoveDeffunction(dptr);
        }
      return(FALSE);
     }

   /*=============================*/
   /* Reformat the closing token. */
   /*=============================*/

   PPBackup();
   PPBackup();
   SavePPBuffer(DFInputToken.print_rep);
   SavePPBuffer("\n");

   /*======================*/
   /* Add the deffunction. */
   /*======================*/

   AddDeffunction(deffunctionName,actions,min,max,lvars,FALSE);

   ReturnExpression(parameterList);

   return(DeffunctionError);
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/************************************************************
  NAME         : ValidDeffunctionName
  DESCRIPTION  : Determines if a new deffunction of the given
                 name can be defined in the current module
  INPUTS       : The new deffunction name
  RETURNS      : TRUE if OK, FALSE otherwise
  SIDE EFFECTS : Error message printed if not OK
  NOTES        : GetConstructNameAndComment() (called before
                 this function) ensures that the deffunction
                 name does not conflict with one from
                 another module
 ************************************************************/
static BOOLEAN ValidDeffunctionName(
  char *theDeffunctionName)
  {
   struct constructHeader *theDeffunction;
#if DEFGENERIC_CONSTRUCT
   struct defmodule *theModule;
   struct constructHeader *theDefgeneric;
#endif

   /* ============================================
      A deffunction cannot be named the same as a
      construct type, e.g, defclass, defrule, etc.
      ============================================ */
   if (FindConstruct(theDeffunctionName) != NULL)
     {
      PrintErrorID("DFFNXPSR",1,FALSE);
      PrintRouter(WERROR,"Deffunctions are not allowed to replace constructs.\n");
      return(FALSE);
     }

   /* ============================================
      A deffunction cannot be named the same as a
      pre-defined system function, e.g, watch,
      list-defrules, etc.
      ============================================ */
   if (FindFunction(theDeffunctionName) != NULL)
     {
      PrintErrorID("DFFNXPSR",2,FALSE);
      PrintRouter(WERROR,"Deffunctions are not allowed to replace external functions.\n");
      return(FALSE);
     }

#if DEFGENERIC_CONSTRUCT
   /* ============================================
      A deffunction cannot be named the same as a
      generic function (either in this module or
      imported from another)
      ============================================ */
   theDefgeneric =
     (struct constructHeader *) LookupDefgenericInScope(theDeffunctionName);
   if (theDefgeneric != NULL)
     {
      theModule = GetConstructModuleItem(theDefgeneric)->theModule;
      if (theModule != ((struct defmodule *) GetCurrentModule()))
        {
         PrintErrorID("DFFNXPSR",5,FALSE);
         PrintRouter(WERROR,"Defgeneric ");
         PrintRouter(WERROR,GetDefgenericName((void *) theDefgeneric));
         PrintRouter(WERROR," imported from module ");
         PrintRouter(WERROR,GetDefmoduleName((void *) theModule));
         PrintRouter(WERROR," conflicts with this deffunction.\n");
         return(FALSE);
        }
      else
        {
         PrintErrorID("DFFNXPSR",3,FALSE);
         PrintRouter(WERROR,"Deffunctions are not allowed to replace generic functions.\n");
        }
      return(FALSE);
     }
#endif

   theDeffunction = (struct constructHeader *) FindDeffunction(theDeffunctionName);
   if (theDeffunction != NULL)
     {
      /* ===========================================
         And a deffunction in the current module can
         only be redefined if it is not executing.
         =========================================== */
      if (((DEFFUNCTION *) theDeffunction)->executing)
        {
         PrintErrorID("DFNXPSR",4,FALSE);
         PrintRouter(WERROR,"Deffunction ");
         PrintRouter(WERROR,GetDeffunctionName((void *) theDeffunction));
         PrintRouter(WERROR," may not be redefined while it is executing.\n");
         return(FALSE);
        }
     }
   return(TRUE);
  }


/****************************************************
  NAME         : AddDeffunction
  DESCRIPTION  : Adds a deffunction to the list of
                 deffunctions
  INPUTS       : 1) The symbolic name
                 2) The action expressions
                 3) The minimum number of arguments
                 4) The maximum number of arguments
                    (can be -1)
                 5) The number of local variables
                 6) A flag indicating if this is
                    a header call so that the
                    deffunction can be recursively
                    called
  RETURNS      : The new deffunction (NULL on errors)
  SIDE EFFECTS : Deffunction structures allocated
  NOTES        : Assumes deffunction is not executing
 ****************************************************/
static DEFFUNCTION *AddDeffunction(
  SYMBOL_HN *name,
  EXPRESSION *actions,
  int min,
  int max,
  int lvars,
  int headerp)
  {
   DEFFUNCTION *dfuncPtr;
   int oldbusy;
#if DEBUGGING_FUNCTIONS
   int DFHadWatch = FALSE;
#endif

   /*===============================================================*/
   /* If the deffunction doesn't exist, create a new structure to   */
   /* contain it and add it to the List of deffunctions. Otherwise, */
   /* use the existing structure and remove the pretty print form   */
   /* and interpretive code.                                        */
   /*===============================================================*/
   dfuncPtr = (DEFFUNCTION *) FindDeffunction(ValueToString(name));
   if (dfuncPtr == NULL)
     {
      dfuncPtr = get_struct(deffunctionStruct);
      InitializeConstructHeader("deffunction",(struct constructHeader *) dfuncPtr,name);
      IncrementSymbolCount(name);
      dfuncPtr->code = NULL;
      dfuncPtr->minNumberOfParameters = min;
      dfuncPtr->maxNumberOfParameters = max;
      dfuncPtr->numberOfLocalVars = lvars;
      dfuncPtr->busy = 0;
      dfuncPtr->executing = 0;
     }
   else
     {
#if DEBUGGING_FUNCTIONS
      DFHadWatch = GetDeffunctionWatch((void *) dfuncPtr);
#endif
      dfuncPtr->minNumberOfParameters = min;
      dfuncPtr->maxNumberOfParameters = max;
      dfuncPtr->numberOfLocalVars = lvars;
      oldbusy = dfuncPtr->busy;
      ExpressionDeinstall(dfuncPtr->code);
      dfuncPtr->busy = oldbusy;
      ReturnPackedExpression(dfuncPtr->code);
      dfuncPtr->code = NULL;
      SetDeffunctionPPForm((void *) dfuncPtr,NULL);

      /* =======================================
         Remove the deffunction from the list so
         that it can be added at the end
         ======================================= */
      RemoveConstructFromModule((struct constructHeader *) dfuncPtr);
     }

   AddConstructToModule((struct constructHeader *) dfuncPtr);

   /* ==================================
      Install the new interpretive code.
      ================================== */

   if (actions != NULL)
     {
      /* ===============================
         If a deffunction is recursive,
         do not increment its busy count
         based on self-references
         =============================== */
      oldbusy = dfuncPtr->busy;
      ExpressionInstall(actions);
      dfuncPtr->busy = oldbusy;
      dfuncPtr->code = actions;
     }

   /* ===============================================================
      Install the pretty print form if memory is not being conserved.
      =============================================================== */

#if DEBUGGING_FUNCTIONS
   SetDeffunctionWatch(DFHadWatch ? TRUE : WatchDeffunctions,(void *) dfuncPtr);
   if ((GetConserveMemory() == FALSE) && (headerp == FALSE))
     SetDeffunctionPPForm((void *) dfuncPtr,CopyPPBuffer());
#endif
   return(dfuncPtr);
  }

#endif /* DEFFUNCTION_CONSTRUCT && (! BLOAD_ONLY) && (! RUN_TIME) */

/***************************************************
  NAME         :
  DESCRIPTION  :
  INPUTS       :
  RETURNS      :
  SIDE EFFECTS :
  NOTES        :
 ***************************************************/

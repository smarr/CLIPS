   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.22  06/15/04            */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Binary Load/Save Functions for Generic Functions */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "setup.h"

#if DEFGENERIC_CONSTRUCT && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)

#include "constant.h"
#include "envrnmnt.h"
#include "memalloc.h"
#include "bload.h"
#include "bsave.h"

#include "cstrcbin.h"

#if OBJECT_SYSTEM
#include "objbin.h"
#endif

#include "genrccom.h"
#include "modulbin.h"

#define _GENRCBIN_SOURCE_
#include "genrcbin.h"

#include "router.h"

/* =========================================
   *****************************************
               MACROS AND TYPES
   =========================================
   ***************************************** */

#define MethodPointer(i) (((i) == -1L) ? NULL : (DEFMETHOD *) &DefgenericBinaryData(theEnv,execStatus)->MethodArray[i])
#define RestrictionPointer(i) (((i) == -1L) ? NULL : (RESTRICTION *) &DefgenericBinaryData(theEnv,execStatus)->RestrictionArray[i])
#define TypePointer(i) (((i) == -1L) ? NULL : (void **) &DefgenericBinaryData(theEnv,execStatus)->TypeArray[i])

typedef struct bsaveRestriction
  {
   long types,query;
   short tcnt;
  } BSAVE_RESTRICTION;

typedef struct bsaveMethod
  {
   short index;
   short restrictionCount,
       minRestrictions,maxRestrictions,
       localVarCount;
   int system;
   long restrictions,actions;
  } BSAVE_METHOD;

typedef struct bsaveGenericFunc
  {
   struct bsaveConstructHeader header;
   long methods;
   short mcnt;
  } BSAVE_GENERIC;

typedef struct bsaveGenericModule
  {
   struct bsaveDefmoduleItemHeader header;
  } BSAVE_DEFGENERIC_MODULE;

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

#if BLOAD_AND_BSAVE

static void BsaveGenericsFind(void *,EXEC_STATUS);
static void MarkDefgenericItems(void *,EXEC_STATUS,struct constructHeader *,void *);
static void BsaveGenericsExpressions(void *,EXEC_STATUS,FILE *);
static void BsaveMethodExpressions(void *,EXEC_STATUS,struct constructHeader *,void *);
static void BsaveRestrictionExpressions(void *,EXEC_STATUS,struct constructHeader *,void *);
static void BsaveGenerics(void *,EXEC_STATUS,FILE *);
static void BsaveDefgenericHeader(void *,EXEC_STATUS,struct constructHeader *,void *);
static void BsaveMethods(void *,EXEC_STATUS,struct constructHeader *,void *);
static void BsaveMethodRestrictions(void *,EXEC_STATUS,struct constructHeader *,void *);
static void BsaveRestrictionTypes(void *,EXEC_STATUS,struct constructHeader *,void *);
static void BsaveStorageGenerics(void *,EXEC_STATUS,FILE *);

#endif

static void BloadStorageGenerics(void *,EXEC_STATUS);
static void BloadGenerics(void *,EXEC_STATUS);
static void UpdateGenericModule(void *,EXEC_STATUS,void *,long);
static void UpdateGeneric(void *,EXEC_STATUS,void *,long);
static void UpdateMethod(void *,EXEC_STATUS,void *,long);
static void UpdateRestriction(void *,EXEC_STATUS,void *,long);
static void UpdateType(void *,EXEC_STATUS,void *,long);
static void ClearBloadGenerics(void *,EXEC_STATUS);
static void DeallocateDefgenericBinaryData(void *,EXEC_STATUS);

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***********************************************************
  NAME         : SetupGenericsBload
  DESCRIPTION  : Initializes data structures and
                   routines for binary loads of
                   generic function constructs
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Routines defined and structures initialized
  NOTES        : None
 ***********************************************************/
globle void SetupGenericsBload(
  void *theEnv,
  EXEC_STATUS)
  {
   AllocateEnvironmentData(theEnv,execStatus,GENRCBIN_DATA,sizeof(struct defgenericBinaryData),DeallocateDefgenericBinaryData);
#if BLOAD_AND_BSAVE
   AddBinaryItem(theEnv,execStatus,"generic functions",0,BsaveGenericsFind,BsaveGenericsExpressions,
                             BsaveStorageGenerics,BsaveGenerics,
                             BloadStorageGenerics,BloadGenerics,
                             ClearBloadGenerics);
#endif
#if BLOAD || BLOAD_ONLY
   AddBinaryItem(theEnv,execStatus,"generic functions",0,NULL,NULL,NULL,NULL,
                             BloadStorageGenerics,BloadGenerics,
                             ClearBloadGenerics);
#endif
  }
  
/***********************************************************/
/* DeallocateDefgenericBinaryData: Deallocates environment */
/*    data for the defgeneric binary functionality.        */
/***********************************************************/
static void DeallocateDefgenericBinaryData(
  void *theEnv,
  EXEC_STATUS)
  {
#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME)
   size_t space;

   space = DefgenericBinaryData(theEnv,execStatus)->GenericCount * sizeof(struct defgeneric);
   if (space != 0) genfree(theEnv,execStatus,(void *) DefgenericBinaryData(theEnv,execStatus)->DefgenericArray,space);

   space = DefgenericBinaryData(theEnv,execStatus)->MethodCount * sizeof(struct method);
   if (space != 0) genfree(theEnv,execStatus,(void *) DefgenericBinaryData(theEnv,execStatus)->MethodArray,space);

   space = DefgenericBinaryData(theEnv,execStatus)->RestrictionCount * sizeof(struct restriction);
   if (space != 0) genfree(theEnv,execStatus,(void *) DefgenericBinaryData(theEnv,execStatus)->RestrictionArray,space);

   space = DefgenericBinaryData(theEnv,execStatus)->TypeCount * sizeof(void *,EXEC_STATUS);
   if (space != 0) genfree(theEnv,execStatus,(void *) DefgenericBinaryData(theEnv,execStatus)->TypeArray,space);

   space =  DefgenericBinaryData(theEnv,execStatus)->ModuleCount * sizeof(struct defgenericModule);
   if (space != 0) genfree(theEnv,execStatus,(void *) DefgenericBinaryData(theEnv,execStatus)->ModuleArray,space);
#endif
  }

/***************************************************
  NAME         : BloadDefgenericModuleReference
  DESCRIPTION  : Returns a pointer to the
                 appropriate defgeneric module
  INPUTS       : The index of the module
  RETURNS      : A pointer to the module
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle void *BloadDefgenericModuleReference(
  void *theEnv,
  EXEC_STATUS,
  int theIndex)
  {
   return ((void *) &DefgenericBinaryData(theEnv,execStatus)->ModuleArray[theIndex]);
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

#if BLOAD_AND_BSAVE

/***************************************************************************
  NAME         : BsaveGenericsFind
  DESCRIPTION  : For all generic functions and their
                   methods, this routine marks all
                   the needed symbols and system functions.
                 Also, it also counts the number of
                   expression structures needed.
                 Also, counts total number of generics, methods,
                   restrictions and types.
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : ExpressionCount (a global from BSAVE.C) is incremented
                   for every expression needed
                 Symbols and system function are marked in their structures
  NOTES        : Also sets bsaveIndex for each generic function (assumes
                   generic functions will be bsaved in order of binary list)
 ***************************************************************************/
static void BsaveGenericsFind(
  void *theEnv,
  EXEC_STATUS)
  {
   SaveBloadCount(theEnv,execStatus,DefgenericBinaryData(theEnv,execStatus)->ModuleCount);
   SaveBloadCount(theEnv,execStatus,DefgenericBinaryData(theEnv,execStatus)->GenericCount);
   SaveBloadCount(theEnv,execStatus,DefgenericBinaryData(theEnv,execStatus)->MethodCount);
   SaveBloadCount(theEnv,execStatus,DefgenericBinaryData(theEnv,execStatus)->RestrictionCount);
   SaveBloadCount(theEnv,execStatus,DefgenericBinaryData(theEnv,execStatus)->TypeCount);

   DefgenericBinaryData(theEnv,execStatus)->GenericCount = 0L;
   DefgenericBinaryData(theEnv,execStatus)->MethodCount = 0L;
   DefgenericBinaryData(theEnv,execStatus)->RestrictionCount = 0L;
   DefgenericBinaryData(theEnv,execStatus)->TypeCount = 0L;

   DefgenericBinaryData(theEnv,execStatus)->ModuleCount = 
      DoForAllConstructs(theEnv,execStatus,MarkDefgenericItems,DefgenericData(theEnv,execStatus)->DefgenericModuleIndex,
                                    FALSE,NULL);
  }

/***************************************************
  NAME         : MarkDefgenericItems
  DESCRIPTION  : Marks the needed items for
                 a defgeneric (and methods) bsave
  INPUTS       : 1) The defgeneric
                 2) User data buffer (ignored)
  RETURNS      : Nothing useful
  SIDE EFFECTS : Needed items marked
  NOTES        : None
 ***************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void MarkDefgenericItems(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theDefgeneric,
  void *userBuffer)
  {
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(userBuffer)
#endif
   DEFGENERIC *gfunc = (DEFGENERIC *) theDefgeneric;
   long i,j;
   DEFMETHOD *meth;
   RESTRICTION *rptr;

   MarkConstructHeaderNeededItems(&gfunc->header,DefgenericBinaryData(theEnv,execStatus)->GenericCount++);
   DefgenericBinaryData(theEnv,execStatus)->MethodCount += (long) gfunc->mcnt;
   for (i = 0 ; i < gfunc->mcnt ; i++)
     {
      meth = &gfunc->methods[i];
      ExpressionData(theEnv,execStatus)->ExpressionCount += ExpressionSize(meth->actions);
      MarkNeededItems(theEnv,execStatus,meth->actions);
      DefgenericBinaryData(theEnv,execStatus)->RestrictionCount += meth->restrictionCount;
      for (j = 0 ; j < meth->restrictionCount ; j++)
        {
         rptr = &meth->restrictions[j];
         ExpressionData(theEnv,execStatus)->ExpressionCount += ExpressionSize(rptr->query);
         MarkNeededItems(theEnv,execStatus,rptr->query);
         DefgenericBinaryData(theEnv,execStatus)->TypeCount += rptr->tcnt;
        }
     }
  }

/***************************************************
  NAME         : BsaveGenericsExpressions
  DESCRIPTION  : Writes out all expressions needed
                   by generic functions
  INPUTS       : The file pointer of the binary file
  RETURNS      : Nothing useful
  SIDE EFFECTS : File updated
  NOTES        : None
 ***************************************************/
static void BsaveGenericsExpressions(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   /* ================================================================
      Important to save all expressions for methods before any
      expressions for restrictions, since methods will be stored first
      ================================================================ */
   DoForAllConstructs(theEnv,execStatus,BsaveMethodExpressions,DefgenericData(theEnv,execStatus)->DefgenericModuleIndex,
                      FALSE,(void *) fp);

   DoForAllConstructs(theEnv,execStatus,BsaveRestrictionExpressions,DefgenericData(theEnv,execStatus)->DefgenericModuleIndex,
                      FALSE,(void *) fp);
  }

/***************************************************
  NAME         : BsaveMethodExpressions
  DESCRIPTION  : Saves the needed expressions for
                 a defgeneric methods bsave
  INPUTS       : 1) The defgeneric
                 2) Output data file pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Method action expressions saved
  NOTES        : None
 ***************************************************/
static void BsaveMethodExpressions(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theDefgeneric,
  void *userBuffer)
  {
   DEFGENERIC *gfunc = (DEFGENERIC *) theDefgeneric;
   long i;

   for (i = 0 ; i < gfunc->mcnt ; i++)
     BsaveExpression(theEnv,execStatus,gfunc->methods[i].actions,(FILE *) userBuffer);
  }

/***************************************************
  NAME         : BsaveRestrictionExpressions
  DESCRIPTION  : Saves the needed expressions for
                 a defgeneric method restriction
                 queries bsave
  INPUTS       : 1) The defgeneric
                 2) Output data file pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Method restriction query
                 expressions saved
  NOTES        : None
 ***************************************************/
static void BsaveRestrictionExpressions(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theDefgeneric,
  void *userBuffer)
  {
   DEFGENERIC *gfunc = (DEFGENERIC *) theDefgeneric;
   long i,j;
   DEFMETHOD *meth;

   for (i = 0 ; i < gfunc->mcnt ; i++)
     {
      meth = &gfunc->methods[i];
      for (j = 0 ; j < meth->restrictionCount ; j++)
        BsaveExpression(theEnv,execStatus,meth->restrictions[j].query,(FILE *) userBuffer);
     }
  }

/***********************************************************
  NAME         : BsaveStorageGenerics
  DESCRIPTION  : Writes out number of each type of structure
                   required for generics
                 Space required for counts (unsigned long)
  INPUTS       : File pointer of binary file
  RETURNS      : Nothing useful
  SIDE EFFECTS : Binary file adjusted
  NOTES        : None
 ***********************************************************/
static void BsaveStorageGenerics(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   size_t space;

   space = sizeof(long) * 5;
   GenWrite((void *) &space,sizeof(size_t),fp);
   GenWrite((void *) &DefgenericBinaryData(theEnv,execStatus)->ModuleCount,sizeof(long),fp);
   GenWrite((void *) &DefgenericBinaryData(theEnv,execStatus)->GenericCount,sizeof(long),fp);
   GenWrite((void *) &DefgenericBinaryData(theEnv,execStatus)->MethodCount,sizeof(long),fp);
   GenWrite((void *) &DefgenericBinaryData(theEnv,execStatus)->RestrictionCount,sizeof(long),fp);
   GenWrite((void *) &DefgenericBinaryData(theEnv,execStatus)->TypeCount,sizeof(long),fp);
  }

/****************************************************************************************
  NAME         : BsaveGenerics
  DESCRIPTION  : Writes out generic function in binary format
                 Space required (unsigned long)
                 All generic modules (sizeof(DEFGENERIC_MODULE) * Number of generic modules)
                 All generic headers (sizeof(DEFGENERIC) * Number of generics)
                 All methods (sizeof(DEFMETHOD) * Number of methods)
                 All method restrictions (sizeof(RESTRICTION) * Number of restrictions)
                 All restriction type arrays (sizeof(void *) * # of types)
  INPUTS       : File pointer of binary file
  RETURNS      : Nothing useful
  SIDE EFFECTS : Binary file adjusted
  NOTES        : None
 ****************************************************************************************/
static void BsaveGenerics(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   struct defmodule *theModule;
   DEFGENERIC_MODULE *theModuleItem;
   size_t space;
   BSAVE_DEFGENERIC_MODULE dummy_generic_module;

   /* =====================================================================
      Space is: Sum over all structures(sizeof(structure) * structure-cnt))
      ===================================================================== */
   space = ((unsigned long) DefgenericBinaryData(theEnv,execStatus)->ModuleCount * sizeof(BSAVE_DEFGENERIC_MODULE)) +
           ((unsigned long) DefgenericBinaryData(theEnv,execStatus)->GenericCount * sizeof(BSAVE_GENERIC)) +
           ((unsigned long) DefgenericBinaryData(theEnv,execStatus)->MethodCount * sizeof(BSAVE_METHOD)) +
           ((unsigned long) DefgenericBinaryData(theEnv,execStatus)->RestrictionCount * sizeof(BSAVE_RESTRICTION)) +
           ((unsigned long) DefgenericBinaryData(theEnv,execStatus)->TypeCount * sizeof(unsigned long));

   /* ================================================================
      Write out the total amount of space required:  modules,headers,
      methods, restrictions, types
      ================================================================ */
   GenWrite((void *) &space,sizeof(size_t),fp);

   /* ======================================
      Write out the generic function modules
      ====================================== */
   DefgenericBinaryData(theEnv,execStatus)->GenericCount = 0L;
   theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,NULL);
   while (theModule != NULL)
     {
      theModuleItem = (DEFGENERIC_MODULE *)
                      GetModuleItem(theEnv,execStatus,theModule,FindModuleItem(theEnv,execStatus,"defgeneric")->moduleIndex);
      AssignBsaveDefmdlItemHdrVals(&dummy_generic_module.header,
                                           &theModuleItem->header);
      GenWrite((void *) &dummy_generic_module,
               sizeof(BSAVE_DEFGENERIC_MODULE),fp);
      theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,(void *) theModule);
     }


   /* ======================================
      Write out the generic function headers
      ====================================== */
   DefgenericBinaryData(theEnv,execStatus)->MethodCount = 0L;
   DoForAllConstructs(theEnv,execStatus,BsaveDefgenericHeader,DefgenericData(theEnv,execStatus)->DefgenericModuleIndex,
                      FALSE,(void *) fp);

   /* =====================
      Write out the methods
      ===================== */
   DefgenericBinaryData(theEnv,execStatus)->RestrictionCount = 0L;
   DoForAllConstructs(theEnv,execStatus,BsaveMethods,DefgenericData(theEnv,execStatus)->DefgenericModuleIndex,
                      FALSE,(void *) fp);

   /* =================================
      Write out the method restrictions
      ================================= */
   DefgenericBinaryData(theEnv,execStatus)->TypeCount = 0L;
   DoForAllConstructs(theEnv,execStatus,BsaveMethodRestrictions,DefgenericData(theEnv,execStatus)->DefgenericModuleIndex,
                      FALSE,(void *) fp);

   /* =============================================================
      Finally, write out the type lists for the method restrictions
      ============================================================= */
   DoForAllConstructs(theEnv,execStatus,BsaveRestrictionTypes,DefgenericData(theEnv,execStatus)->DefgenericModuleIndex,
                      FALSE,(void *) fp);

   RestoreBloadCount(theEnv,execStatus,&DefgenericBinaryData(theEnv,execStatus)->ModuleCount);
   RestoreBloadCount(theEnv,execStatus,&DefgenericBinaryData(theEnv,execStatus)->GenericCount);
   RestoreBloadCount(theEnv,execStatus,&DefgenericBinaryData(theEnv,execStatus)->MethodCount);
   RestoreBloadCount(theEnv,execStatus,&DefgenericBinaryData(theEnv,execStatus)->RestrictionCount);
   RestoreBloadCount(theEnv,execStatus,&DefgenericBinaryData(theEnv,execStatus)->TypeCount);
  }

/***************************************************
  NAME         : BsaveDefgenericHeader
  DESCRIPTION  : Bsaves a generic function header
  INPUTS       : 1) The defgeneric
                 2) Output data file pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Defgeneric header saved
  NOTES        : None
 ***************************************************/
static void BsaveDefgenericHeader(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theDefgeneric,
  void *userBuffer)
  {
   DEFGENERIC *gfunc = (DEFGENERIC *) theDefgeneric;
   BSAVE_GENERIC dummy_generic;

   AssignBsaveConstructHeaderVals(&dummy_generic.header,&gfunc->header);
   dummy_generic.mcnt = gfunc->mcnt;
   if (gfunc->methods != NULL)
     {
      dummy_generic.methods = DefgenericBinaryData(theEnv,execStatus)->MethodCount;
      DefgenericBinaryData(theEnv,execStatus)->MethodCount += (long) gfunc->mcnt;
     }
   else
     dummy_generic.methods = -1L;
   GenWrite((void *) &dummy_generic,(unsigned long) sizeof(BSAVE_GENERIC),(FILE *) userBuffer);
  }

/***************************************************
  NAME         : BsaveMethods
  DESCRIPTION  : Bsaves defgeneric methods
  INPUTS       : 1) The defgeneric
                 2) Output data file pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Defgeneric methods saved
  NOTES        : None
 ***************************************************/
static void BsaveMethods(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theDefgeneric,
  void *userBuffer)
  {
   DEFGENERIC *gfunc = (DEFGENERIC *) theDefgeneric;
   DEFMETHOD *meth;
   BSAVE_METHOD dummy_method;
   long i;

   for (i = 0 ; i < gfunc->mcnt ; i++)
     {
      meth = &gfunc->methods[i];
      dummy_method.index = meth->index;
      dummy_method.restrictionCount = meth->restrictionCount;
      dummy_method.minRestrictions = meth->minRestrictions;
      dummy_method.maxRestrictions = meth->maxRestrictions;
      dummy_method.localVarCount = meth->localVarCount;
      dummy_method.system = meth->system;
      if (meth->restrictions != NULL)
        {
         dummy_method.restrictions = DefgenericBinaryData(theEnv,execStatus)->RestrictionCount;
         DefgenericBinaryData(theEnv,execStatus)->RestrictionCount += meth->restrictionCount;
        }
      else
        dummy_method.restrictions = -1L;
      if (meth->actions != NULL)
        {
         dummy_method.actions = ExpressionData(theEnv,execStatus)->ExpressionCount;
         ExpressionData(theEnv,execStatus)->ExpressionCount += ExpressionSize(meth->actions);
        }
      else
        dummy_method.actions = -1L;
      GenWrite((void *) &dummy_method,sizeof(BSAVE_METHOD),(FILE *) userBuffer);
     }
  }

/******************************************************
  NAME         : BsaveMethodRestrictions
  DESCRIPTION  : Bsaves defgeneric methods' retrictions
  INPUTS       : 1) The defgeneric
                 2) Output data file pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Defgeneric methods' restrictions saved
  NOTES        : None
 ******************************************************/
static void BsaveMethodRestrictions(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theDefgeneric,
  void *userBuffer)
  {
   DEFGENERIC *gfunc = (DEFGENERIC *) theDefgeneric;
   BSAVE_RESTRICTION dummy_restriction;
   RESTRICTION *rptr;
   short i,j;

   for (i = 0 ; i < gfunc->mcnt ; i++)
     {
      for (j = 0 ; j < gfunc->methods[i].restrictionCount ; j++)
        {
         rptr = &gfunc->methods[i].restrictions[j];
         dummy_restriction.tcnt = rptr->tcnt;
         if (rptr->types != NULL)
           {
            dummy_restriction.types = DefgenericBinaryData(theEnv,execStatus)->TypeCount;
            DefgenericBinaryData(theEnv,execStatus)->TypeCount += rptr->tcnt;
           }
         else
           dummy_restriction.types = -1L;
         if (rptr->query != NULL)
           {
            dummy_restriction.query = ExpressionData(theEnv,execStatus)->ExpressionCount;
            ExpressionData(theEnv,execStatus)->ExpressionCount += ExpressionSize(rptr->query);
           }
         else
           dummy_restriction.query = -1L;
         GenWrite((void *) &dummy_restriction,
                  sizeof(BSAVE_RESTRICTION),(FILE *) userBuffer);
        }
     }
  }

/*************************************************************
  NAME         : BsaveRestrictionTypes
  DESCRIPTION  : Bsaves defgeneric methods' retrictions' types
  INPUTS       : 1) The defgeneric
                 2) Output data file pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Defgeneric methods' restrictions' types saved
  NOTES        : None
 *************************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void BsaveRestrictionTypes(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theDefgeneric,
  void *userBuffer)
  {
   DEFGENERIC *gfunc = (DEFGENERIC *) theDefgeneric;
   long dummy_type;
   RESTRICTION *rptr;
   short i,j,k;
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv,execStatus)
#endif

   for (i = 0 ; i < gfunc->mcnt ; i++)
     {
      for (j = 0 ; j < gfunc->methods[i].restrictionCount ; j++)
        {
         rptr = &gfunc->methods[i].restrictions[j];
         for (k = 0 ; k < rptr->tcnt ; k++)
           {
#if OBJECT_SYSTEM
            dummy_type = DefclassIndex(rptr->types[k]);
#else
            dummy_type = (long) ((INTEGER_HN *) rptr->types[k])->contents;
#endif
            GenWrite(&dummy_type,sizeof(long),(FILE *) userBuffer);
           }
        }
     }
  }

#endif

/***********************************************************************
  NAME         : BloadStorageGenerics
  DESCRIPTION  : This routine space required for generic function
                   structures and allocates space for them
  INPUTS       : Nothing
  RETURNS      : Nothing useful
  SIDE EFFECTS : Arrays allocated and set
  NOTES        : This routine makes no attempt to reset any pointers
                   within the structures
 ***********************************************************************/
static void BloadStorageGenerics(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;
   long counts[5];

   GenReadBinary(theEnv,execStatus,(void *) &space,sizeof(size_t));
   if (space == 0L)
     return;
   GenReadBinary(theEnv,execStatus,(void *) counts,space);
   DefgenericBinaryData(theEnv,execStatus)->ModuleCount = counts[0];
   DefgenericBinaryData(theEnv,execStatus)->GenericCount = counts[1];
   DefgenericBinaryData(theEnv,execStatus)->MethodCount = counts[2];
   DefgenericBinaryData(theEnv,execStatus)->RestrictionCount = counts[3];
   DefgenericBinaryData(theEnv,execStatus)->TypeCount = counts[4];
   if (DefgenericBinaryData(theEnv,execStatus)->ModuleCount != 0L)
     {
      space = (sizeof(DEFGENERIC_MODULE) * DefgenericBinaryData(theEnv,execStatus)->ModuleCount);
      DefgenericBinaryData(theEnv,execStatus)->ModuleArray = (DEFGENERIC_MODULE *) genalloc(theEnv,execStatus,space);
     }
   else
     return;
   if (DefgenericBinaryData(theEnv,execStatus)->GenericCount != 0L)
     {
      space = (sizeof(DEFGENERIC) * DefgenericBinaryData(theEnv,execStatus)->GenericCount);
      DefgenericBinaryData(theEnv,execStatus)->DefgenericArray = (DEFGENERIC *) genalloc(theEnv,execStatus,space);
     }
   else
     return;
   if (DefgenericBinaryData(theEnv,execStatus)->MethodCount != 0L)
     {
      space = (sizeof(DEFMETHOD) * DefgenericBinaryData(theEnv,execStatus)->MethodCount);
      DefgenericBinaryData(theEnv,execStatus)->MethodArray = (DEFMETHOD *) genalloc(theEnv,execStatus,space);
     }
   else
     return;
   if (DefgenericBinaryData(theEnv,execStatus)->RestrictionCount != 0L)
     {
      space = (sizeof(RESTRICTION) * DefgenericBinaryData(theEnv,execStatus)->RestrictionCount);
      DefgenericBinaryData(theEnv,execStatus)->RestrictionArray = (RESTRICTION *) genalloc(theEnv,execStatus,space);
     }
   else
     return;
   if (DefgenericBinaryData(theEnv,execStatus)->TypeCount != 0L)
     {
      space = (sizeof(void *) * DefgenericBinaryData(theEnv,execStatus)->TypeCount);
      DefgenericBinaryData(theEnv,execStatus)->TypeArray = (void * *) genalloc(theEnv,execStatus,space);
     }
  }

/*********************************************************************
  NAME         : BloadGenerics
  DESCRIPTION  : This routine reads generic function information from
                 a binary file in four chunks:
                 Generic-header array
                 Method array
                 Method restrictions array
                 Restriction types array

                 This routine moves through the generic function
                   binary arrays updating pointers
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Pointers reset from array indices
  NOTES        : Assumes all loading is finished
 ********************************************************************/
static void BloadGenerics(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;

   GenReadBinary(theEnv,execStatus,(void *) &space,sizeof(size_t));
   if (DefgenericBinaryData(theEnv,execStatus)->ModuleCount == 0L)
     return;
   BloadandRefresh(theEnv,execStatus,DefgenericBinaryData(theEnv,execStatus)->ModuleCount,sizeof(BSAVE_DEFGENERIC_MODULE),UpdateGenericModule);
   if (DefgenericBinaryData(theEnv,execStatus)->GenericCount == 0L)
     return;
   BloadandRefresh(theEnv,execStatus,DefgenericBinaryData(theEnv,execStatus)->GenericCount,sizeof(BSAVE_GENERIC),UpdateGeneric);
   BloadandRefresh(theEnv,execStatus,DefgenericBinaryData(theEnv,execStatus)->MethodCount,sizeof(BSAVE_METHOD),UpdateMethod);
   BloadandRefresh(theEnv,execStatus,DefgenericBinaryData(theEnv,execStatus)->RestrictionCount,sizeof(BSAVE_RESTRICTION),UpdateRestriction);
   BloadandRefresh(theEnv,execStatus,DefgenericBinaryData(theEnv,execStatus)->TypeCount,sizeof(long),UpdateType);
  }

/*********************************************
  Bload update routines for generic structures
 *********************************************/
static void UpdateGenericModule(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   BSAVE_DEFGENERIC_MODULE *bdptr;

   bdptr = (BSAVE_DEFGENERIC_MODULE *) buf;
   UpdateDefmoduleItemHeader(theEnv,execStatus,&bdptr->header,&DefgenericBinaryData(theEnv,execStatus)->ModuleArray[obji].header,
                             (int) sizeof(DEFGENERIC),(void *) DefgenericBinaryData(theEnv,execStatus)->DefgenericArray);
  }

static void UpdateGeneric(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   BSAVE_GENERIC *bgp;
   DEFGENERIC *gp;

   bgp = (BSAVE_GENERIC *) buf;
   gp = (DEFGENERIC *) &DefgenericBinaryData(theEnv,execStatus)->DefgenericArray[obji];

   UpdateConstructHeader(theEnv,execStatus,&bgp->header,&gp->header,
                         (int) sizeof(DEFGENERIC_MODULE),(void *) DefgenericBinaryData(theEnv,execStatus)->ModuleArray,
                         (int) sizeof(DEFGENERIC),(void *) DefgenericBinaryData(theEnv,execStatus)->DefgenericArray);
   DefgenericBinaryData(theEnv,execStatus)->DefgenericArray[obji].busy = 0;
#if DEBUGGING_FUNCTIONS
   DefgenericBinaryData(theEnv,execStatus)->DefgenericArray[obji].trace = DefgenericData(theEnv,execStatus)->WatchGenerics;
#endif
   DefgenericBinaryData(theEnv,execStatus)->DefgenericArray[obji].methods = MethodPointer(bgp->methods);
   DefgenericBinaryData(theEnv,execStatus)->DefgenericArray[obji].mcnt = bgp->mcnt;
   DefgenericBinaryData(theEnv,execStatus)->DefgenericArray[obji].new_index = 0;
  }

static void UpdateMethod(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   BSAVE_METHOD *bmth;

   bmth = (BSAVE_METHOD *) buf;
   DefgenericBinaryData(theEnv,execStatus)->MethodArray[obji].index = bmth->index;
   DefgenericBinaryData(theEnv,execStatus)->MethodArray[obji].busy = 0;
#if DEBUGGING_FUNCTIONS
   DefgenericBinaryData(theEnv,execStatus)->MethodArray[obji].trace = DefgenericData(theEnv,execStatus)->WatchMethods;
#endif
   DefgenericBinaryData(theEnv,execStatus)->MethodArray[obji].restrictionCount = bmth->restrictionCount;
   DefgenericBinaryData(theEnv,execStatus)->MethodArray[obji].minRestrictions = bmth->minRestrictions;
   DefgenericBinaryData(theEnv,execStatus)->MethodArray[obji].maxRestrictions = bmth->maxRestrictions;
   DefgenericBinaryData(theEnv,execStatus)->MethodArray[obji].localVarCount = bmth->localVarCount;
   DefgenericBinaryData(theEnv,execStatus)->MethodArray[obji].system = bmth->system;
   DefgenericBinaryData(theEnv,execStatus)->MethodArray[obji].restrictions = RestrictionPointer(bmth->restrictions);
   DefgenericBinaryData(theEnv,execStatus)->MethodArray[obji].actions = ExpressionPointer(bmth->actions);
   DefgenericBinaryData(theEnv,execStatus)->MethodArray[obji].ppForm = NULL;
   DefgenericBinaryData(theEnv,execStatus)->MethodArray[obji].usrData = NULL;
  }

static void UpdateRestriction(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   BSAVE_RESTRICTION *brp;

   brp = (BSAVE_RESTRICTION *) buf;
   DefgenericBinaryData(theEnv,execStatus)->RestrictionArray[obji].tcnt = brp->tcnt;
   DefgenericBinaryData(theEnv,execStatus)->RestrictionArray[obji].types = TypePointer(brp->types);
   DefgenericBinaryData(theEnv,execStatus)->RestrictionArray[obji].query = ExpressionPointer(brp->query);
  }

static void UpdateType(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
#if OBJECT_SYSTEM
   DefgenericBinaryData(theEnv,execStatus)->TypeArray[obji] = (void *) DefclassPointer(* (long *) buf);
#else
   if ((* (long *) buf) > (long) INSTANCE_TYPE_CODE)
     {
      PrintWarningID(theEnv,execStatus,"GENRCBIN",1,FALSE);
      EnvPrintRouter(theEnv,execStatus,WWARNING,"COOL not installed!  User-defined class\n");
      EnvPrintRouter(theEnv,execStatus,WWARNING,"  in method restriction substituted with OBJECT.\n");
      DefgenericBinaryData(theEnv,execStatus)->TypeArray[obji] = (void *) EnvAddLong(theEnv,execStatus,(long long) OBJECT_TYPE_CODE);
     }
   else
     DefgenericBinaryData(theEnv,execStatus)->TypeArray[obji] = (void *) EnvAddLong(theEnv,execStatus,* (long *) buf);
   IncrementIntegerCount((INTEGER_HN *) DefgenericBinaryData(theEnv,execStatus)->TypeArray[obji]);
#endif
  }

/***************************************************************
  NAME         : ClearBloadGenerics
  DESCRIPTION  : Release all binary-loaded generic function
                   structure arrays
                 Resets generic function list to NULL
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Memory cleared
  NOTES        : Generic function name symbol counts decremented
 ***************************************************************/
static void ClearBloadGenerics(
  void *theEnv,
  EXEC_STATUS)
  {
   register long i;
   size_t space;

   space = (sizeof(DEFGENERIC_MODULE) * DefgenericBinaryData(theEnv,execStatus)->ModuleCount);
   if (space == 0L)
     return;
   genfree(theEnv,execStatus,(void *) DefgenericBinaryData(theEnv,execStatus)->ModuleArray,space);
   DefgenericBinaryData(theEnv,execStatus)->ModuleArray = NULL;
   DefgenericBinaryData(theEnv,execStatus)->ModuleCount = 0L;

   for (i = 0 ; i < DefgenericBinaryData(theEnv,execStatus)->GenericCount ; i++)
     UnmarkConstructHeader(theEnv,execStatus,&DefgenericBinaryData(theEnv,execStatus)->DefgenericArray[i].header);

   space = (sizeof(DEFGENERIC) * DefgenericBinaryData(theEnv,execStatus)->GenericCount);
   if (space == 0L)
     return;
   genfree(theEnv,execStatus,(void *) DefgenericBinaryData(theEnv,execStatus)->DefgenericArray,space);
   DefgenericBinaryData(theEnv,execStatus)->DefgenericArray = NULL;
   DefgenericBinaryData(theEnv,execStatus)->GenericCount = 0L;

   space = (sizeof(DEFMETHOD) * DefgenericBinaryData(theEnv,execStatus)->MethodCount);
   if (space == 0L)
     return;
   genfree(theEnv,execStatus,(void *) DefgenericBinaryData(theEnv,execStatus)->MethodArray,space);
   DefgenericBinaryData(theEnv,execStatus)->MethodArray = NULL;
   DefgenericBinaryData(theEnv,execStatus)->MethodCount = 0L;

   space = (sizeof(RESTRICTION) * DefgenericBinaryData(theEnv,execStatus)->RestrictionCount);
   if (space == 0L)
     return;
   genfree(theEnv,execStatus,(void *) DefgenericBinaryData(theEnv,execStatus)->RestrictionArray,space);
   DefgenericBinaryData(theEnv,execStatus)->RestrictionArray = NULL;
   DefgenericBinaryData(theEnv,execStatus)->RestrictionCount = 0L;

#if ! OBJECT_SYSTEM
   for (i = 0 ; i < DefgenericBinaryData(theEnv,execStatus)->TypeCount ; i++)
     DecrementIntegerCount(theEnv,execStatus,(INTEGER_HN *) DefgenericBinaryData(theEnv,execStatus)->TypeArray[i]);
#endif
   space = (sizeof(void *) * DefgenericBinaryData(theEnv,execStatus)->TypeCount);
   if (space == 0L)
     return;
   genfree(theEnv,execStatus,(void *) DefgenericBinaryData(theEnv,execStatus)->TypeArray,space);
   DefgenericBinaryData(theEnv,execStatus)->TypeArray = NULL;
   DefgenericBinaryData(theEnv,execStatus)->TypeCount = 0L;
  }

#endif


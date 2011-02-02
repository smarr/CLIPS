   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.22  06/15/04          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Binary Load/Save Functions for Deffunctions      */
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

#if DEFFUNCTION_CONSTRUCT && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)

#include "bload.h"
#include "bsave.h"

#include "memalloc.h"
#include "cstrcbin.h"
#include "envrnmnt.h"
#include "modulbin.h"

#define _DFFNXBIN_SOURCE_
#include "dffnxbin.h"

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
typedef struct bsaveDeffunctionModule
  {
   struct bsaveDefmoduleItemHeader header;
  } BSAVE_DEFFUNCTION_MODULE;

typedef struct bsaveDeffunctionStruct
  {
   struct bsaveConstructHeader header;
   int minNumberOfParameters,
       maxNumberOfParameters,
       numberOfLocalVars;
   long name,
        code;
  } BSAVE_DEFFUNCTION;

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

#if BLOAD_AND_BSAVE
static void BsaveDeffunctionFind(void *,EXEC_STATUS);
static void MarkDeffunctionItems(void *,EXEC_STATUS,struct constructHeader *,void *);
static void BsaveDeffunctionExpressions(void *,EXEC_STATUS,FILE *);
static void BsaveDeffunctionExpression(void *,EXEC_STATUS,struct constructHeader *,void *);
static void BsaveStorageDeffunctions(void *,EXEC_STATUS,FILE *);
static void BsaveDeffunctions(void *,EXEC_STATUS,FILE *);
static void BsaveDeffunction(void *,EXEC_STATUS,struct constructHeader *,void *);
#endif

static void BloadStorageDeffunctions(void *,EXEC_STATUS);
static void BloadDeffunctions(void *,EXEC_STATUS);
static void UpdateDeffunctionModule(void *,EXEC_STATUS,void *,long);
static void UpdateDeffunction(void *,EXEC_STATUS,void *,long);
static void ClearDeffunctionBload(void *,EXEC_STATUS);
static void DeallocateDeffunctionBloadData(void *,EXEC_STATUS);

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***********************************************************
  NAME         : SetupDeffunctionsBload
  DESCRIPTION  : Initializes data structures and
                   routines for binary loads of deffunctions
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Routines defined and structures initialized
  NOTES        : None
 ***********************************************************/
globle void SetupDeffunctionsBload(
  void *theEnv,
  EXEC_STATUS)
  {
   AllocateEnvironmentData(theEnv,execStatus,DFFNXBIN_DATA,sizeof(struct deffunctionBinaryData),DeallocateDeffunctionBloadData);
#if BLOAD_AND_BSAVE
   AddBinaryItem(theEnv,execStatus,"deffunctions",0,BsaveDeffunctionFind,BsaveDeffunctionExpressions,
                             BsaveStorageDeffunctions,BsaveDeffunctions,
                             BloadStorageDeffunctions,BloadDeffunctions,
                             ClearDeffunctionBload);
#else
   AddBinaryItem(theEnv,execStatus,"deffunctions",0,NULL,NULL,NULL,NULL,
                             BloadStorageDeffunctions,BloadDeffunctions,
                             ClearDeffunctionBload);
#endif
  }
  
/***********************************************************/
/* DeallocateDeffunctionBloadData: Deallocates environment */
/*    data for the deffunction bsave functionality.        */
/***********************************************************/
static void DeallocateDeffunctionBloadData(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;

#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME)
   space = DeffunctionBinaryData(theEnv,execStatus)->DeffunctionCount * sizeof(struct deffunctionStruct);
   if (space != 0) genfree(theEnv,execStatus,(void *) DeffunctionBinaryData(theEnv,execStatus)->DeffunctionArray,space);

   space =  DeffunctionBinaryData(theEnv,execStatus)->ModuleCount * sizeof(struct deffunctionModule);
   if (space != 0) genfree(theEnv,execStatus,(void *) DeffunctionBinaryData(theEnv,execStatus)->ModuleArray,space);
#endif
  }

/***************************************************
  NAME         : BloadDeffunctionModuleReference
  DESCRIPTION  : Returns a pointer to the
                 appropriate deffunction module
  INPUTS       : The index of the module
  RETURNS      : A pointer to the module
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle void *BloadDeffunctionModuleReference(
  void *theEnv,
  EXEC_STATUS,
  int theIndex)
  {
   return ((void *) &DeffunctionBinaryData(theEnv,execStatus)->ModuleArray[theIndex]);
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

#if BLOAD_AND_BSAVE

/***************************************************************************
  NAME         : BsaveDeffunctionFind
  DESCRIPTION  : For all deffunctions, this routine marks all
                   the needed symbols.
                 Also, it also counts the number of
                   expression structures needed.
                 Also, counts total number of deffunctions.
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : ExpressionCount (a global from BSAVE.C) is incremented
                   for every expression needed
                 Symbols are marked in their structures
  NOTES        : Also sets bsaveIndex for each deffunction (assumes
                   deffunctions will be bsaved in order of binary list)
 ***************************************************************************/
static void BsaveDeffunctionFind(
  void *theEnv,
  EXEC_STATUS)
  {
   SaveBloadCount(theEnv,execStatus,DeffunctionBinaryData(theEnv,execStatus)->ModuleCount);
   SaveBloadCount(theEnv,execStatus,DeffunctionBinaryData(theEnv,execStatus)->DeffunctionCount);
   DeffunctionBinaryData(theEnv,execStatus)->DeffunctionCount = 0L;

   DeffunctionBinaryData(theEnv,execStatus)->ModuleCount = 
      DoForAllConstructs(theEnv,execStatus,MarkDeffunctionItems,DeffunctionData(theEnv,execStatus)->DeffunctionModuleIndex,
                         FALSE,NULL);
  }

/***************************************************
  NAME         : MarkDeffunctionItems
  DESCRIPTION  : Marks the needed items for
                 a deffunction bsave
  INPUTS       : 1) The deffunction
                 2) User data buffer (ignored)
  RETURNS      : Nothing useful
  SIDE EFFECTS : Needed items marked
  NOTES        : None
 ***************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void MarkDeffunctionItems(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theDeffunction,
  void *userBuffer)
  {
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(userBuffer)
#endif

   MarkConstructHeaderNeededItems(theDeffunction,DeffunctionBinaryData(theEnv,execStatus)->DeffunctionCount++);
   ExpressionData(theEnv,execStatus)->ExpressionCount += ExpressionSize(((DEFFUNCTION *) theDeffunction)->code);
   MarkNeededItems(theEnv,execStatus,((DEFFUNCTION *) theDeffunction)->code);
  }

/***************************************************
  NAME         : BsaveDeffunctionExpressions
  DESCRIPTION  : Writes out all expressions needed
                   by deffunctyions
  INPUTS       : The file pointer of the binary file
  RETURNS      : Nothing useful
  SIDE EFFECTS : File updated
  NOTES        : None
 ***************************************************/
static void BsaveDeffunctionExpressions(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   DoForAllConstructs(theEnv,execStatus,BsaveDeffunctionExpression,DeffunctionData(theEnv,execStatus)->DeffunctionModuleIndex,
                      FALSE,(void *) fp);
  }

/***************************************************
  NAME         : BsaveDeffunctionExpression
  DESCRIPTION  : Saves the needed expressions for
                 a deffunction bsave
  INPUTS       : 1) The deffunction
                 2) Output data file pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Expressions saved
  NOTES        : None
 ***************************************************/
static void BsaveDeffunctionExpression(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theDeffunction,
  void *userBuffer)
  {
   BsaveExpression(theEnv,execStatus,((DEFFUNCTION *) theDeffunction)->code,(FILE *) userBuffer);
  }

/***********************************************************
  NAME         : BsaveStorageDeffunctions
  DESCRIPTION  : Writes out number of each type of structure
                   required for deffunctions
                 Space required for counts (unsigned long)
  INPUTS       : File pointer of binary file
  RETURNS      : Nothing useful
  SIDE EFFECTS : Binary file adjusted
  NOTES        : None
 ***********************************************************/
static void BsaveStorageDeffunctions(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   size_t space;

   space = sizeof(unsigned long) * 2;
   GenWrite((void *) &space,sizeof(size_t),fp);
   GenWrite((void *) &DeffunctionBinaryData(theEnv,execStatus)->ModuleCount,sizeof(unsigned long),fp);
   GenWrite((void *) &DeffunctionBinaryData(theEnv,execStatus)->DeffunctionCount,sizeof(unsigned long),fp);
  }

/*************************************************************************************
  NAME         : BsaveDeffunctions
  DESCRIPTION  : Writes out deffunction in binary format
                 Space required (unsigned long)
                 All deffunctions (sizeof(DEFFUNCTION) * Number of deffunctions)
  INPUTS       : File pointer of binary file
  RETURNS      : Nothing useful
  SIDE EFFECTS : Binary file adjusted
  NOTES        : None
 *************************************************************************************/
static void BsaveDeffunctions(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   size_t space;
   struct defmodule *theModule;
   DEFFUNCTION_MODULE *theModuleItem;
   BSAVE_DEFFUNCTION_MODULE dummy_mitem;

   space = ((sizeof(BSAVE_DEFFUNCTION_MODULE) * DeffunctionBinaryData(theEnv,execStatus)->ModuleCount) +
            (sizeof(BSAVE_DEFFUNCTION) * DeffunctionBinaryData(theEnv,execStatus)->DeffunctionCount));
   GenWrite((void *) &space,sizeof(size_t),fp);

   /* =================================
      Write out each deffunction module
      ================================= */
   DeffunctionBinaryData(theEnv,execStatus)->DeffunctionCount = 0L;
   theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,NULL);
   while (theModule != NULL)
     {
      theModuleItem = (DEFFUNCTION_MODULE *)
                      GetModuleItem(theEnv,execStatus,theModule,FindModuleItem(theEnv,execStatus,"deffunction")->moduleIndex);
      AssignBsaveDefmdlItemHdrVals(&dummy_mitem.header,&theModuleItem->header);
      GenWrite((void *) &dummy_mitem,sizeof(BSAVE_DEFFUNCTION_MODULE),fp);
      theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,(void *) theModule);
     }

   /* ==========================
      Write out each deffunction
      ========================== */
   DoForAllConstructs(theEnv,execStatus,BsaveDeffunction,DeffunctionData(theEnv,execStatus)->DeffunctionModuleIndex,
                      FALSE,(void *) fp);

   RestoreBloadCount(theEnv,execStatus,&DeffunctionBinaryData(theEnv,execStatus)->ModuleCount);
   RestoreBloadCount(theEnv,execStatus,&DeffunctionBinaryData(theEnv,execStatus)->DeffunctionCount);
  }

/***************************************************
  NAME         : BsaveDeffunction
  DESCRIPTION  : Bsaves a deffunction
  INPUTS       : 1) The deffunction
                 2) Output data file pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Deffunction saved
  NOTES        : None
 ***************************************************/
static void BsaveDeffunction(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theDeffunction,
  void *userBuffer)
  {
   DEFFUNCTION *dptr = (DEFFUNCTION *) theDeffunction;
   BSAVE_DEFFUNCTION dummy_df;

   AssignBsaveConstructHeaderVals(&dummy_df.header,&dptr->header);
   dummy_df.minNumberOfParameters = dptr->minNumberOfParameters;
   dummy_df.maxNumberOfParameters = dptr->maxNumberOfParameters;
   dummy_df.numberOfLocalVars = dptr->numberOfLocalVars;
   if (dptr->code != NULL)
     {
      dummy_df.code = ExpressionData(theEnv,execStatus)->ExpressionCount;
      ExpressionData(theEnv,execStatus)->ExpressionCount += ExpressionSize(dptr->code);
     }
   else
     dummy_df.code = -1L;
   GenWrite((void *) &dummy_df,sizeof(BSAVE_DEFFUNCTION),(FILE *) userBuffer);
  }

#endif

/***********************************************************************
  NAME         : BloadStorageDeffunctions
  DESCRIPTION  : This routine space required for deffunction
                   structures and allocates space for them
  INPUTS       : Nothing
  RETURNS      : Nothing useful
  SIDE EFFECTS : Arrays allocated and set
  NOTES        : This routine makes no attempt to reset any pointers
                   within the structures
 ***********************************************************************/
static void BloadStorageDeffunctions(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;

   GenReadBinary(theEnv,execStatus,(void *) &space,sizeof(size_t));
   if (space == 0L)
     return;
   GenReadBinary(theEnv,execStatus,(void *) &DeffunctionBinaryData(theEnv,execStatus)->ModuleCount,sizeof(unsigned long));
   GenReadBinary(theEnv,execStatus,(void *) &DeffunctionBinaryData(theEnv,execStatus)->DeffunctionCount,sizeof(unsigned long));
   if (DeffunctionBinaryData(theEnv,execStatus)->ModuleCount == 0L)
     {
      DeffunctionBinaryData(theEnv,execStatus)->ModuleArray = NULL;
      DeffunctionBinaryData(theEnv,execStatus)->DeffunctionArray = NULL;
      return;
     }

   space = (DeffunctionBinaryData(theEnv,execStatus)->ModuleCount * sizeof(DEFFUNCTION_MODULE));
   DeffunctionBinaryData(theEnv,execStatus)->ModuleArray = (DEFFUNCTION_MODULE *) genalloc(theEnv,execStatus,space);

   if (DeffunctionBinaryData(theEnv,execStatus)->DeffunctionCount == 0L)
     {
      DeffunctionBinaryData(theEnv,execStatus)->DeffunctionArray = NULL;
      return;
     }

   space = (DeffunctionBinaryData(theEnv,execStatus)->DeffunctionCount * sizeof(DEFFUNCTION));
   DeffunctionBinaryData(theEnv,execStatus)->DeffunctionArray = (DEFFUNCTION *) genalloc(theEnv,execStatus,space);
  }

/*********************************************************************
  NAME         : BloadDeffunctions
  DESCRIPTION  : This routine reads deffunction information from
                   a binary file
                 This routine moves through the deffunction
                   binary array updating pointers
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Pointers reset from array indices
  NOTES        : Assumes all loading is finished
 ********************************************************************/
static void BloadDeffunctions(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;

   GenReadBinary(theEnv,execStatus,(void *) &space,sizeof(size_t));
   BloadandRefresh(theEnv,execStatus,DeffunctionBinaryData(theEnv,execStatus)->ModuleCount,sizeof(BSAVE_DEFFUNCTION_MODULE),UpdateDeffunctionModule);
   BloadandRefresh(theEnv,execStatus,DeffunctionBinaryData(theEnv,execStatus)->DeffunctionCount,sizeof(BSAVE_DEFFUNCTION),UpdateDeffunction);
  }

/*******************************************************
  NAME         : UpdateDeffunctionModule
  DESCRIPTION  : Updates deffunction module with binary
                 load data - sets pointers from
                 offset information
  INPUTS       : 1) A pointer to the bloaded data
                 2) The index of the binary array
                    element to update
  RETURNS      : Nothing useful
  SIDE EFFECTS : Deffunction moudle pointers updated
  NOTES        : None
 *******************************************************/
static void UpdateDeffunctionModule(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   BSAVE_DEFFUNCTION_MODULE *bdptr;

   bdptr = (BSAVE_DEFFUNCTION_MODULE *) buf;
   UpdateDefmoduleItemHeader(theEnv,execStatus,&bdptr->header,&DeffunctionBinaryData(theEnv,execStatus)->ModuleArray[obji].header,
                             (int) sizeof(DEFFUNCTION),(void *) DeffunctionBinaryData(theEnv,execStatus)->DeffunctionArray);
  }

/***************************************************
  NAME         : UpdateDeffunction
  DESCRIPTION  : Updates deffunction with binary
                 load data - sets pointers from
                 offset information
  INPUTS       : 1) A pointer to the bloaded data
                 2) The index of the binary array
                    element to update
  RETURNS      : Nothing useful
  SIDE EFFECTS : Deffunction pointers upadted
  NOTES        : None
 ***************************************************/
static void UpdateDeffunction(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   BSAVE_DEFFUNCTION *bdptr;
   DEFFUNCTION *dptr;

   bdptr = (BSAVE_DEFFUNCTION *) buf;
   dptr = (DEFFUNCTION *) &DeffunctionBinaryData(theEnv,execStatus)->DeffunctionArray[obji];

   UpdateConstructHeader(theEnv,execStatus,&bdptr->header,&dptr->header,
                         (int) sizeof(DEFFUNCTION_MODULE),(void *) DeffunctionBinaryData(theEnv,execStatus)->ModuleArray,
                         (int) sizeof(DEFFUNCTION),(void *) DeffunctionBinaryData(theEnv,execStatus)->DeffunctionArray);

   dptr->code = ExpressionPointer(bdptr->code);
   dptr->busy = 0;
   dptr->executing = 0;
#if DEBUGGING_FUNCTIONS
   dptr->trace = (unsigned short) DeffunctionData(theEnv,execStatus)->WatchDeffunctions;
#endif
   dptr->minNumberOfParameters = bdptr->minNumberOfParameters;
   dptr->maxNumberOfParameters = bdptr->maxNumberOfParameters;
   dptr->numberOfLocalVars = bdptr->numberOfLocalVars;
  }

/***************************************************************
  NAME         : ClearDeffunctionBload
  DESCRIPTION  : Release all binary-loaded deffunction
                   structure arrays
                 Resets deffunction list to NULL
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Memory cleared
  NOTES        : Deffunction name symbol counts decremented
 ***************************************************************/
static void ClearDeffunctionBload(
  void *theEnv,
  EXEC_STATUS)
  {
   register long i;
   size_t space;

   space = (sizeof(DEFFUNCTION_MODULE) * DeffunctionBinaryData(theEnv,execStatus)->ModuleCount);
   if (space == 0L)
     return;
   genfree(theEnv,execStatus,(void *) DeffunctionBinaryData(theEnv,execStatus)->ModuleArray,space);
   DeffunctionBinaryData(theEnv,execStatus)->ModuleArray = NULL;
   DeffunctionBinaryData(theEnv,execStatus)->ModuleCount = 0L;

   for (i = 0L ; i < DeffunctionBinaryData(theEnv,execStatus)->DeffunctionCount ; i++)
     UnmarkConstructHeader(theEnv,execStatus,&DeffunctionBinaryData(theEnv,execStatus)->DeffunctionArray[i].header);
   space = (sizeof(DEFFUNCTION) * DeffunctionBinaryData(theEnv,execStatus)->DeffunctionCount);
   if (space == 0L)
     return;
   genfree(theEnv,execStatus,(void *) DeffunctionBinaryData(theEnv,execStatus)->DeffunctionArray,space);
   DeffunctionBinaryData(theEnv,execStatus)->DeffunctionArray = NULL;
   DeffunctionBinaryData(theEnv,execStatus)->DeffunctionCount = 0L;
  }

#endif


   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.22  06/15/04          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Binary Load/Save Functions for Definstances      */
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

#if DEFINSTANCES_CONSTRUCT && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)

#include "bload.h"
#include "bsave.h"
#include "envrnmnt.h"
#include "memalloc.h"
#include "cstrcbin.h"
#include "defins.h"
#include "modulbin.h"

#define _DFINSBIN_SOURCE_
#include "dfinsbin.h"

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
typedef struct bsaveDefinstancesModule
  {
   struct bsaveDefmoduleItemHeader header;
  } BSAVE_DEFINSTANCES_MODULE;

typedef struct bsaveDefinstances
  {
   struct bsaveConstructHeader header;
   long mkinstance;
  } BSAVE_DEFINSTANCES;

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

#if BLOAD_AND_BSAVE
static void BsaveDefinstancesFind(void *,EXEC_STATUS);
static void MarkDefinstancesItems(void *,EXEC_STATUS,struct constructHeader *,void *);
static void BsaveDefinstancesExpressions(void *,EXEC_STATUS,FILE *);
static void BsaveDefinstancesExpression(void *,EXEC_STATUS,struct constructHeader *,void *);
static void BsaveStorageDefinstances(void *,EXEC_STATUS,FILE *);
static void BsaveDefinstancesDriver(void *,EXEC_STATUS,FILE *);
static void BsaveDefinstances(void *,EXEC_STATUS,struct constructHeader *,void *);
#endif

static void BloadStorageDefinstances(void *,EXEC_STATUS);
static void BloadDefinstances(void *,EXEC_STATUS);
static void UpdateDefinstancesModule(void *,EXEC_STATUS,void *,long);
static void UpdateDefinstances(void *,EXEC_STATUS,void *,long);
static void ClearDefinstancesBload(void *,EXEC_STATUS);
static void DeallocateDefinstancesBinaryData(void *,EXEC_STATUS);

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***********************************************************
  NAME         : SetupDefinstancesBload
  DESCRIPTION  : Initializes data structures and
                   routines for binary loads of definstances
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Routines defined and structures initialized
  NOTES        : None
 ***********************************************************/
globle void SetupDefinstancesBload(
  void *theEnv,
  EXEC_STATUS)
  {
   AllocateEnvironmentData(theEnv,execStatus,DFINSBIN_DATA,sizeof(struct definstancesBinaryData),DeallocateDefinstancesBinaryData);
#if BLOAD_AND_BSAVE
   AddBinaryItem(theEnv,execStatus,"definstances",0,BsaveDefinstancesFind,BsaveDefinstancesExpressions,
                             BsaveStorageDefinstances,BsaveDefinstancesDriver,
                             BloadStorageDefinstances,BloadDefinstances,
                             ClearDefinstancesBload);
#else
   AddBinaryItem(theEnv,execStatus,"definstances",0,NULL,NULL,NULL,NULL,
                             BloadStorageDefinstances,BloadDefinstances,
                             ClearDefinstancesBload);
#endif
  }
  
/*************************************************************/
/* DeallocateDefinstancesBinaryData: Deallocates environment */
/*    data for the definstances binary functionality.        */
/*************************************************************/
static void DeallocateDefinstancesBinaryData(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;

#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME)
   space = DefinstancesBinaryData(theEnv,execStatus)->DefinstancesCount * sizeof(struct definstances);
   if (space != 0) genfree(theEnv,execStatus,(void *) DefinstancesBinaryData(theEnv,execStatus)->DefinstancesArray,space);

   space =  DefinstancesBinaryData(theEnv,execStatus)->ModuleCount * sizeof(struct definstancesModule);
   if (space != 0) genfree(theEnv,execStatus,(void *) DefinstancesBinaryData(theEnv,execStatus)->ModuleArray,space);
#endif
  }

/***************************************************
  NAME         : BloadDefinstancesModuleRef
  DESCRIPTION  : Returns a pointer to the
                 appropriate definstances module
  INPUTS       : The index of the module
  RETURNS      : A pointer to the module
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle void *BloadDefinstancesModuleRef(
  void *theEnv,
  EXEC_STATUS,
  int theIndex)
  {
   return ((void *) &DefinstancesBinaryData(theEnv,execStatus)->ModuleArray[theIndex]);
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

#if BLOAD_AND_BSAVE

/***************************************************************************
  NAME         : BsaveDefinstancesFind
  DESCRIPTION  : For all definstances, this routine marks all
                   the needed symbols.
                 Also, it also counts the number of
                   expression structures needed.
                 Also, counts total number of definstances.
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : ExpressionCount (a global from BSAVE.C) is incremented
                   for every expression needed
                 Symbols are marked in their structures
  NOTES        : Also sets bsaveIndex for each definstances (assumes
                   definstances will be bsaved in order of binary list)
 ***************************************************************************/
static void BsaveDefinstancesFind(
  void *theEnv,
  EXEC_STATUS)
  {
   SaveBloadCount(theEnv,execStatus,DefinstancesBinaryData(theEnv,execStatus)->ModuleCount);
   SaveBloadCount(theEnv,execStatus,DefinstancesBinaryData(theEnv,execStatus)->DefinstancesCount);
   DefinstancesBinaryData(theEnv,execStatus)->DefinstancesCount = 0L;

   DefinstancesBinaryData(theEnv,execStatus)->ModuleCount = 
      DoForAllConstructs(theEnv,execStatus,MarkDefinstancesItems,DefinstancesData(theEnv,execStatus)->DefinstancesModuleIndex,
                         FALSE,NULL);
  }


/***************************************************
  NAME         : MarkDefinstancesItems
  DESCRIPTION  : Marks the needed items for
                 a definstances bsave
  INPUTS       : 1) The definstances
                 2) User data buffer (ignored)
  RETURNS      : Nothing useful
  SIDE EFFECTS : Needed items marked
  NOTES        : None
 ***************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void MarkDefinstancesItems(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theDefinstances,
  void *userBuffer)
  {
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(userBuffer)
#endif

   MarkConstructHeaderNeededItems(theDefinstances,DefinstancesBinaryData(theEnv,execStatus)->DefinstancesCount++);
   ExpressionData(theEnv,execStatus)->ExpressionCount += ExpressionSize(((DEFINSTANCES *) theDefinstances)->mkinstance);
   MarkNeededItems(theEnv,execStatus,((DEFINSTANCES *) theDefinstances)->mkinstance);
  }

/***************************************************
  NAME         : BsaveDefinstancesExpressions
  DESCRIPTION  : Writes out all expressions needed
                   by deffunctyions
  INPUTS       : The file pointer of the binary file
  RETURNS      : Nothing useful
  SIDE EFFECTS : File updated
  NOTES        : None
 ***************************************************/
static void BsaveDefinstancesExpressions(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   DoForAllConstructs(theEnv,execStatus,BsaveDefinstancesExpression,DefinstancesData(theEnv,execStatus)->DefinstancesModuleIndex,
                      FALSE,(void *) fp);
  }

/***************************************************
  NAME         : BsaveDefinstancesExpression
  DESCRIPTION  : Saves the needed expressions for
                 a definstances bsave
  INPUTS       : 1) The definstances
                 2) Output data file pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Expressions saved
  NOTES        : None
 ***************************************************/
static void BsaveDefinstancesExpression(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theDefinstances,
  void *userBuffer)
  {
   BsaveExpression(theEnv,execStatus,((DEFINSTANCES *) theDefinstances)->mkinstance,(FILE *) userBuffer);
  }

/***********************************************************
  NAME         : BsaveStorageDefinstances
  DESCRIPTION  : Writes out number of each type of structure
                   required for definstances
                 Space required for counts (unsigned long)
  INPUTS       : File pointer of binary file
  RETURNS      : Nothing useful
  SIDE EFFECTS : Binary file adjusted
  NOTES        : None
 ***********************************************************/
static void BsaveStorageDefinstances(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   size_t space;

   space = sizeof(unsigned long) * 2;
   GenWrite((void *) &space,sizeof(size_t),fp);
   GenWrite((void *) &DefinstancesBinaryData(theEnv,execStatus)->ModuleCount,sizeof(unsigned long),fp);
   GenWrite((void *) &DefinstancesBinaryData(theEnv,execStatus)->DefinstancesCount,sizeof(unsigned long),fp);
  }

/*************************************************************************************
  NAME         : BsaveDefinstancesDriver
  DESCRIPTION  : Writes out definstances in binary format
                 Space required (unsigned long)
                 All definstances (sizeof(DEFINSTANCES) * Number of definstances)
  INPUTS       : File pointer of binary file
  RETURNS      : Nothing useful
  SIDE EFFECTS : Binary file adjusted
  NOTES        : None
 *************************************************************************************/
static void BsaveDefinstancesDriver(
  void *theEnv,
  EXEC_STATUS,
  FILE *fp)
  {
   size_t space;
   struct defmodule *theModule;
   DEFINSTANCES_MODULE *theModuleItem;
   BSAVE_DEFINSTANCES_MODULE dummy_mitem;

   space = ((sizeof(BSAVE_DEFINSTANCES_MODULE) * DefinstancesBinaryData(theEnv,execStatus)->ModuleCount) +
            (sizeof(BSAVE_DEFINSTANCES) * DefinstancesBinaryData(theEnv,execStatus)->DefinstancesCount));
   GenWrite((void *) &space,sizeof(size_t),fp);

   /* =================================
      Write out each definstances module
      ================================= */
   DefinstancesBinaryData(theEnv,execStatus)->DefinstancesCount = 0L;
   theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,NULL);
   while (theModule != NULL)
     {
      theModuleItem = (DEFINSTANCES_MODULE *)
                      GetModuleItem(theEnv,execStatus,theModule,FindModuleItem(theEnv,execStatus,"definstances")->moduleIndex);
      AssignBsaveDefmdlItemHdrVals(&dummy_mitem.header,&theModuleItem->header);
      GenWrite((void *) &dummy_mitem,sizeof(BSAVE_DEFINSTANCES_MODULE),fp);
      theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,(void *) theModule);
     }

   /* ==========================
      Write out each definstances
      ========================== */
   DoForAllConstructs(theEnv,execStatus,BsaveDefinstances,DefinstancesData(theEnv,execStatus)->DefinstancesModuleIndex,
                      FALSE,(void *) fp);

   RestoreBloadCount(theEnv,execStatus,&DefinstancesBinaryData(theEnv,execStatus)->ModuleCount);
   RestoreBloadCount(theEnv,execStatus,&DefinstancesBinaryData(theEnv,execStatus)->DefinstancesCount);
  }

/***************************************************
  NAME         : BsaveDefinstances
  DESCRIPTION  : Bsaves a definstances
  INPUTS       : 1) The definstances
                 2) Output data file pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Definstances saved
  NOTES        : None
 ***************************************************/
static void BsaveDefinstances(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theDefinstances,
  void *userBuffer)
  {
   DEFINSTANCES *dptr = (DEFINSTANCES *) theDefinstances;
   BSAVE_DEFINSTANCES dummy_df;

   AssignBsaveConstructHeaderVals(&dummy_df.header,&dptr->header);
  if (dptr->mkinstance != NULL)
     {
      dummy_df.mkinstance = ExpressionData(theEnv,execStatus)->ExpressionCount;
      ExpressionData(theEnv,execStatus)->ExpressionCount += ExpressionSize(dptr->mkinstance);
     }
   else
    dummy_df.mkinstance = -1L;
   GenWrite((void *) &dummy_df,(unsigned long) sizeof(BSAVE_DEFINSTANCES),(FILE *) userBuffer);
  }

#endif

/***********************************************************************
  NAME         : BloadStorageDefinstances
  DESCRIPTION  : This routine space required for definstances
                   structures and allocates space for them
  INPUTS       : Nothing
  RETURNS      : Nothing useful
  SIDE EFFECTS : Arrays allocated and set
  NOTES        : This routine makes no attempt to reset any pointers
                   within the structures
 ***********************************************************************/
static void BloadStorageDefinstances(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;

   GenReadBinary(theEnv,execStatus,(void *) &space,sizeof(size_t));
   if (space == 0L)
     return;
   GenReadBinary(theEnv,execStatus,(void *) &DefinstancesBinaryData(theEnv,execStatus)->ModuleCount,sizeof(unsigned long));
   GenReadBinary(theEnv,execStatus,(void *) &DefinstancesBinaryData(theEnv,execStatus)->DefinstancesCount,sizeof(unsigned long));
   if (DefinstancesBinaryData(theEnv,execStatus)->ModuleCount == 0L)
     {
      DefinstancesBinaryData(theEnv,execStatus)->ModuleArray = NULL;
      DefinstancesBinaryData(theEnv,execStatus)->DefinstancesArray = NULL;
      return;
     }

   space = (DefinstancesBinaryData(theEnv,execStatus)->ModuleCount * sizeof(DEFINSTANCES_MODULE));
   DefinstancesBinaryData(theEnv,execStatus)->ModuleArray = (DEFINSTANCES_MODULE *) genalloc(theEnv,execStatus,space);

   if (DefinstancesBinaryData(theEnv,execStatus)->DefinstancesCount == 0L)
     {
      DefinstancesBinaryData(theEnv,execStatus)->DefinstancesArray = NULL;
      return;
     }

   space = (DefinstancesBinaryData(theEnv,execStatus)->DefinstancesCount * sizeof(DEFINSTANCES));
   DefinstancesBinaryData(theEnv,execStatus)->DefinstancesArray = (DEFINSTANCES *) genalloc(theEnv,execStatus,space);
  }

/*********************************************************************
  NAME         : BloadDefinstances
  DESCRIPTION  : This routine reads definstances information from
                   a binary file
                 This routine moves through the definstances
                   binary array updating pointers
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Pointers reset from array indices
  NOTES        : Assumes all loading is finished
 ********************************************************************/
static void BloadDefinstances(
  void *theEnv,
  EXEC_STATUS)
  {
   size_t space;

   GenReadBinary(theEnv,execStatus,(void *) &space,sizeof(size_t));
   BloadandRefresh(theEnv,execStatus,DefinstancesBinaryData(theEnv,execStatus)->ModuleCount,sizeof(BSAVE_DEFINSTANCES_MODULE),UpdateDefinstancesModule);
   BloadandRefresh(theEnv,execStatus,DefinstancesBinaryData(theEnv,execStatus)->DefinstancesCount,sizeof(BSAVE_DEFINSTANCES),UpdateDefinstances);
  }

/*******************************************************
  NAME         : UpdateDefinstancesModule
  DESCRIPTION  : Updates definstances module with binary
                 load data - sets pointers from
                 offset information
  INPUTS       : 1) A pointer to the bloaded data
                 2) The index of the binary array
                    element to update
  RETURNS      : Nothing useful
  SIDE EFFECTS : Definstances moudle pointers updated
  NOTES        : None
 *******************************************************/
static void UpdateDefinstancesModule(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   BSAVE_DEFINSTANCES_MODULE *bdptr;

   bdptr = (BSAVE_DEFINSTANCES_MODULE *) buf;
   UpdateDefmoduleItemHeader(theEnv,execStatus,&bdptr->header,&DefinstancesBinaryData(theEnv,execStatus)->ModuleArray[obji].header,
                             (int) sizeof(DEFINSTANCES),(void *) DefinstancesBinaryData(theEnv,execStatus)->DefinstancesArray);
  }

/***************************************************
  NAME         : UpdateDefinstances
  DESCRIPTION  : Updates definstances with binary
                 load data - sets pointers from
                 offset information
  INPUTS       : 1) A pointer to the bloaded data
                 2) The index of the binary array
                    element to update
  RETURNS      : Nothing useful
  SIDE EFFECTS : Definstances pointers upadted
  NOTES        : None
 ***************************************************/
static void UpdateDefinstances(
  void *theEnv,
  EXEC_STATUS,
  void *buf,
  long obji)
  {
   BSAVE_DEFINSTANCES *bdptr;
   DEFINSTANCES *dfiptr;

   bdptr = (BSAVE_DEFINSTANCES *) buf;
   dfiptr = (DEFINSTANCES *) &DefinstancesBinaryData(theEnv,execStatus)->DefinstancesArray[obji];

   UpdateConstructHeader(theEnv,execStatus,&bdptr->header,&dfiptr->header,
                         (int) sizeof(DEFINSTANCES_MODULE),(void *) DefinstancesBinaryData(theEnv,execStatus)->ModuleArray,
                         (int) sizeof(DEFINSTANCES),(void *) DefinstancesBinaryData(theEnv,execStatus)->DefinstancesArray);
   dfiptr->mkinstance = ExpressionPointer(bdptr->mkinstance);
   dfiptr->busy = 0;
  }

/***************************************************************
  NAME         : ClearDefinstancesBload
  DESCRIPTION  : Release all binary-loaded definstances
                   structure arrays
                 Resets definstances list to NULL
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Memory cleared
  NOTES        : Definstances name symbol counts decremented
 ***************************************************************/
static void ClearDefinstancesBload(
  void *theEnv,
  EXEC_STATUS)
  {
   register long i;
   size_t space;

   space = (sizeof(DEFINSTANCES_MODULE) * DefinstancesBinaryData(theEnv,execStatus)->ModuleCount);
   if (space == 0L)
     return;
   genfree(theEnv,execStatus,(void *) DefinstancesBinaryData(theEnv,execStatus)->ModuleArray,space);
   DefinstancesBinaryData(theEnv,execStatus)->ModuleArray = NULL;
   DefinstancesBinaryData(theEnv,execStatus)->ModuleCount = 0L;

   for (i = 0L ; i < DefinstancesBinaryData(theEnv,execStatus)->DefinstancesCount ; i++)
     UnmarkConstructHeader(theEnv,execStatus,&DefinstancesBinaryData(theEnv,execStatus)->DefinstancesArray[i].header);
   space = (sizeof(DEFINSTANCES) * DefinstancesBinaryData(theEnv,execStatus)->DefinstancesCount);
   if (space == 0L)
     return;
   genfree(theEnv,execStatus,(void *) DefinstancesBinaryData(theEnv,execStatus)->DefinstancesArray,space);
   DefinstancesBinaryData(theEnv,execStatus)->DefinstancesArray = NULL;
   DefinstancesBinaryData(theEnv,execStatus)->DefinstancesCount = 0L;
  }

#endif


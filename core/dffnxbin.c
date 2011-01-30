   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/09/97          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Binary Load/Save Functions for Deffunctions      */
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

#if DEFFUNCTION_CONSTRUCT && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)

#include "bload.h"
#include "bsave.h"

#include "memalloc.h"
#include "cstrcbin.h"
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
static void BsaveDeffunctionFind(void);
static void MarkDeffunctionItems(struct constructHeader *,void *);
static void BsaveDeffunctionExpressions(FILE *);
static void BsaveDeffunctionExpression(struct constructHeader *,void *);
static void BsaveStorageDeffunctions(FILE *);
static void BsaveDeffunctions(FILE *);
static void BsaveDeffunction(struct constructHeader *,void *);
#endif

static void BloadStorageDeffunctions(void);
static void BloadDeffunctions(void);
static void UpdateDeffunctionModule(void *,long);
static void UpdateDeffunction(void *,long);
static void ClearDeffunctionBload(void);

/* =========================================
   *****************************************
      EXTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */
Thread globle DEFFUNCTION *deffunctionArray = NULL;

/* =========================================
   *****************************************
      INTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */
Thread static long DeffunctionCount = 0L,
            ModuleCount = 0L;
Thread static DEFFUNCTION_MODULE *ModuleArray;

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
globle void SetupDeffunctionsBload()
  {
#if BLOAD_AND_BSAVE
   AddBinaryItem("deffunctions",0,BsaveDeffunctionFind,BsaveDeffunctionExpressions,
                             BsaveStorageDeffunctions,BsaveDeffunctions,
                             BloadStorageDeffunctions,BloadDeffunctions,
                             ClearDeffunctionBload);
#else
   AddBinaryItem("deffunctions",0,NULL,NULL,NULL,NULL,
                             BloadStorageDeffunctions,BloadDeffunctions,
                             ClearDeffunctionBload);
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
  int index)
  {
   return ((void *) &ModuleArray[index]);
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
static void BsaveDeffunctionFind()
  {
   if (Bloaded())
     {
      SaveBloadCount(ModuleCount);
      SaveBloadCount(DeffunctionCount);
     }
   DeffunctionCount = 0L;

   ModuleCount = DoForAllConstructs(MarkDeffunctionItems,DeffunctionModuleIndex,
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
#if IBM_TBC
#pragma argsused
#endif
static void MarkDeffunctionItems(
  struct constructHeader *theDeffunction,
  void *userBuffer)
  {
#if MAC_MPW || MAC_MCW || IBM_MCW
#pragma unused(userBuffer)
#endif
   MarkConstructHeaderNeededItems(theDeffunction,DeffunctionCount++);
   ExpressionCount += ExpressionSize(((DEFFUNCTION *) theDeffunction)->code);
   MarkNeededItems(((DEFFUNCTION *) theDeffunction)->code);
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
  FILE *fp)
  {
   DoForAllConstructs(BsaveDeffunctionExpression,DeffunctionModuleIndex,
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
  struct constructHeader *theDeffunction,
  void *userBuffer)
  {
   BsaveExpression(((DEFFUNCTION *) theDeffunction)->code,(FILE *) userBuffer);
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
  FILE *fp)
  {
   unsigned long space;

   space = sizeof(unsigned long) * 2;
   GenWrite((void *) &space,(unsigned long) sizeof(unsigned long),fp);
   GenWrite((void *) &ModuleCount,(unsigned long) sizeof(long),fp);
   GenWrite((void *) &DeffunctionCount,(unsigned long) sizeof(long),fp);
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
  FILE *fp)
  {
   unsigned long space;
   struct defmodule *theModule;
   DEFFUNCTION_MODULE *theModuleItem;
   BSAVE_DEFFUNCTION_MODULE dummy_mitem;

   space = (unsigned long) ((sizeof(BSAVE_DEFFUNCTION_MODULE) * ModuleCount) +
                            (sizeof(BSAVE_DEFFUNCTION) * DeffunctionCount));
   GenWrite((void *) &space,(unsigned long) sizeof(unsigned long),fp);

   /* =================================
      Write out each deffunction module
      ================================= */
   DeffunctionCount = 0L;
   theModule = (struct defmodule *) GetNextDefmodule(NULL);
   while (theModule != NULL)
     {
      theModuleItem = (DEFFUNCTION_MODULE *)
                      GetModuleItem(theModule,FindModuleItem("deffunction")->moduleIndex);
      AssignBsaveDefmdlItemHdrVals(&dummy_mitem.header,&theModuleItem->header);
      GenWrite((void *) &dummy_mitem,(unsigned long) sizeof(BSAVE_DEFFUNCTION_MODULE),fp);
      theModule = (struct defmodule *) GetNextDefmodule((void *) theModule);
     }

   /* ==========================
      Write out each deffunction
      ========================== */
   DoForAllConstructs(BsaveDeffunction,DeffunctionModuleIndex,
                      FALSE,(void *) fp);

   if (Bloaded())
     {
      RestoreBloadCount(&ModuleCount);
      RestoreBloadCount(&DeffunctionCount);
     }
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
      dummy_df.code = ExpressionCount;
      ExpressionCount += ExpressionSize(dptr->code);
     }
   else
     dummy_df.code = -1L;
   GenWrite((void *) &dummy_df,(unsigned long) sizeof(BSAVE_DEFFUNCTION),(FILE *) userBuffer);
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
static void BloadStorageDeffunctions()
  {
   unsigned long space;

   GenRead((void *) &space,(unsigned long) sizeof(unsigned long));
   if (space == 0L)
     return;
   GenRead((void *) &ModuleCount,(unsigned long) sizeof(unsigned long));
   GenRead((void *) &DeffunctionCount,(unsigned long) sizeof(unsigned long));
   if (ModuleCount == 0L)
     {
      ModuleArray = NULL;
      deffunctionArray = NULL;
      return;
     }

   space = (unsigned long) (ModuleCount * sizeof(DEFFUNCTION_MODULE));
   ModuleArray = (DEFFUNCTION_MODULE *) genlongalloc(space);

   if (DeffunctionCount == 0L)
     {
      deffunctionArray = NULL;
      return;
     }

   space = (unsigned long) (DeffunctionCount * sizeof(DEFFUNCTION));
   deffunctionArray = (DEFFUNCTION *) genlongalloc(space);
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
static void BloadDeffunctions()
  {
   unsigned long space;

   GenRead((void *) &space,(unsigned long) sizeof(unsigned long));
   BloadandRefresh(ModuleCount,sizeof(BSAVE_DEFFUNCTION_MODULE),UpdateDeffunctionModule);
   BloadandRefresh(DeffunctionCount,sizeof(BSAVE_DEFFUNCTION),UpdateDeffunction);
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
  void *buf,
  long obji)
  {
   BSAVE_DEFFUNCTION_MODULE *bdptr;

   bdptr = (BSAVE_DEFFUNCTION_MODULE *) buf;
   UpdateDefmoduleItemHeader(&bdptr->header,&ModuleArray[obji].header,
                             (int) sizeof(DEFFUNCTION),(void *) deffunctionArray);
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
  void *buf,
  long obji)
  {
   BSAVE_DEFFUNCTION *bdptr;
   DEFFUNCTION *dptr;

   bdptr = (BSAVE_DEFFUNCTION *) buf;
   dptr = (DEFFUNCTION *) &deffunctionArray[obji];

   UpdateConstructHeader(&bdptr->header,&dptr->header,
                         (int) sizeof(DEFFUNCTION_MODULE),(void *) ModuleArray,
                         (int) sizeof(DEFFUNCTION),(void *) deffunctionArray);

   dptr->code = ExpressionPointer(bdptr->code);
   dptr->busy = 0;
   dptr->executing = 0;
#if DEBUGGING_FUNCTIONS
   dptr->trace = (unsigned short) WatchDeffunctions;
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
static void ClearDeffunctionBload()
  {
   register long i;
   unsigned long space;

   space = (unsigned long) (sizeof(DEFFUNCTION_MODULE) * ModuleCount);
   if (space == 0L)
     return;
   genlongfree((void *) ModuleArray,space);
   ModuleArray = NULL;
   ModuleCount = 0L;

   for (i = 0L ; i < DeffunctionCount ; i++)
     UnmarkConstructHeader(&deffunctionArray[i].header);
   space = (unsigned long) (sizeof(DEFFUNCTION) * DeffunctionCount);
   if (space == 0L)
     return;
   genlongfree((void *) deffunctionArray,space);
   deffunctionArray = NULL;
   DeffunctionCount = 0L;
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

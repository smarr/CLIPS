   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/09/97          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Binary Load/Save Functions for Definstances      */
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

#if DEFINSTANCES_CONSTRUCT && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)

#include "bload.h"
#include "bsave.h"
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
static void BsaveDefinstancesFind(void);
static void MarkDefinstancesItems(struct constructHeader *,void *);
static void BsaveDefinstancesExpressions(FILE *);
static void BsaveDefinstancesExpression(struct constructHeader *,void *);
static void BsaveStorageDefinstances(FILE *);
static void BsaveDefinstancesDriver(FILE *);
static void BsaveDefinstances(struct constructHeader *,void *);
#endif

static void BloadStorageDefinstances(void);
static void BloadDefinstances(void);
static void UpdateDefinstancesModule(void *,long);
static void UpdateDefinstances(void *,long);
static void ClearDefinstancesBload(void);

/* =========================================
   *****************************************
      EXTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */
Thread globle DEFINSTANCES *definstancesArray = NULL;

/* =========================================
   *****************************************
      INTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */
Thread static long DefinstancesCount = 0L,
                   ModuleCount = 0L;
Thread static DEFINSTANCES_MODULE *ModuleArray;

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
globle void SetupDefinstancesBload()
  {
#if BLOAD_AND_BSAVE
   AddBinaryItem("definstances",0,BsaveDefinstancesFind,BsaveDefinstancesExpressions,
                             BsaveStorageDefinstances,BsaveDefinstancesDriver,
                             BloadStorageDefinstances,BloadDefinstances,
                             ClearDefinstancesBload);
#else
   AddBinaryItem("definstances",0,NULL,NULL,NULL,NULL,
                             BloadStorageDefinstances,BloadDefinstances,
                             ClearDefinstancesBload);
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
static void BsaveDefinstancesFind()
  {
   if (Bloaded())
     {
      SaveBloadCount(ModuleCount);
      SaveBloadCount(DefinstancesCount);
     }
   DefinstancesCount = 0L;

   ModuleCount = DoForAllConstructs(MarkDefinstancesItems,DefinstancesModuleIndex,
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
#if IBM_TBC
#pragma argsused
#endif
static void MarkDefinstancesItems(
  struct constructHeader *theDefinstances,
  void *userBuffer)
  {
#if MAC_MPW || MAC_MCW || IBM_MCW
#pragma unused(userBuffer)
#endif
   MarkConstructHeaderNeededItems(theDefinstances,DefinstancesCount++);
   ExpressionCount += ExpressionSize(((DEFINSTANCES *) theDefinstances)->mkinstance);
   MarkNeededItems(((DEFINSTANCES *) theDefinstances)->mkinstance);
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
  FILE *fp)
  {
   DoForAllConstructs(BsaveDefinstancesExpression,DefinstancesModuleIndex,
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
  struct constructHeader *theDefinstances,
  void *userBuffer)
  {
   BsaveExpression(((DEFINSTANCES *) theDefinstances)->mkinstance,(FILE *) userBuffer);
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
  FILE *fp)
  {
   unsigned long space;

   space = sizeof(unsigned long) * 2;
   GenWrite((void *) &space,(unsigned long) sizeof(unsigned long),fp);
   GenWrite((void *) &ModuleCount,(unsigned long) sizeof(long),fp);
   GenWrite((void *) &DefinstancesCount,(unsigned long) sizeof(long),fp);
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
  FILE *fp)
  {
   unsigned long space;
   struct defmodule *theModule;
   DEFINSTANCES_MODULE *theModuleItem;
   BSAVE_DEFINSTANCES_MODULE dummy_mitem;

   space = (unsigned long) ((sizeof(BSAVE_DEFINSTANCES_MODULE) * ModuleCount) +
                            (sizeof(BSAVE_DEFINSTANCES) * DefinstancesCount));
   GenWrite((void *) &space,(unsigned long) sizeof(unsigned long),fp);

   /* =================================
      Write out each definstances module
      ================================= */
   DefinstancesCount = 0L;
   theModule = (struct defmodule *) GetNextDefmodule(NULL);
   while (theModule != NULL)
     {
      theModuleItem = (DEFINSTANCES_MODULE *)
                      GetModuleItem(theModule,FindModuleItem("definstances")->moduleIndex);
      AssignBsaveDefmdlItemHdrVals(&dummy_mitem.header,&theModuleItem->header);
      GenWrite((void *) &dummy_mitem,(unsigned long) sizeof(BSAVE_DEFINSTANCES_MODULE),fp);
      theModule = (struct defmodule *) GetNextDefmodule((void *) theModule);
     }

   /* ==========================
      Write out each definstances
      ========================== */
   DoForAllConstructs(BsaveDefinstances,DefinstancesModuleIndex,
                      FALSE,(void *) fp);

   if (Bloaded())
     {
      RestoreBloadCount(&ModuleCount);
      RestoreBloadCount(&DefinstancesCount);
     }
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
  struct constructHeader *theDefinstances,
  void *userBuffer)
  {
   DEFINSTANCES *dptr = (DEFINSTANCES *) theDefinstances;
   BSAVE_DEFINSTANCES dummy_df;

   AssignBsaveConstructHeaderVals(&dummy_df.header,&dptr->header);
  if (dptr->mkinstance != NULL)
     {
      dummy_df.mkinstance = ExpressionCount;
      ExpressionCount += ExpressionSize(dptr->mkinstance);
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
static void BloadStorageDefinstances()
  {
   unsigned long space;

   GenRead((void *) &space,(unsigned long) sizeof(unsigned long));
   if (space == 0L)
     return;
   GenRead((void *) &ModuleCount,(unsigned long) sizeof(unsigned long));
   GenRead((void *) &DefinstancesCount,(unsigned long) sizeof(unsigned long));
   if (ModuleCount == 0L)
     {
      ModuleArray = NULL;
      definstancesArray = NULL;
      return;
     }

   space = (unsigned long) (ModuleCount * sizeof(DEFINSTANCES_MODULE));
   ModuleArray = (DEFINSTANCES_MODULE *) genlongalloc(space);

   if (DefinstancesCount == 0L)
     {
      definstancesArray = NULL;
      return;
     }

   space = (unsigned long) (DefinstancesCount * sizeof(DEFINSTANCES));
   definstancesArray = (DEFINSTANCES *) genlongalloc(space);
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
static void BloadDefinstances()
  {
   unsigned long space;

   GenRead((void *) &space,(unsigned long) sizeof(unsigned long));
   BloadandRefresh(ModuleCount,sizeof(BSAVE_DEFINSTANCES_MODULE),UpdateDefinstancesModule);
   BloadandRefresh(DefinstancesCount,sizeof(BSAVE_DEFINSTANCES),UpdateDefinstances);
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
  void *buf,
  long obji)
  {
   BSAVE_DEFINSTANCES_MODULE *bdptr;

   bdptr = (BSAVE_DEFINSTANCES_MODULE *) buf;
   UpdateDefmoduleItemHeader(&bdptr->header,&ModuleArray[obji].header,
                             (int) sizeof(DEFINSTANCES),(void *) definstancesArray);
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
  void *buf,
  long obji)
  {
   BSAVE_DEFINSTANCES *bdptr;
   DEFINSTANCES *dfiptr;

   bdptr = (BSAVE_DEFINSTANCES *) buf;
   dfiptr = (DEFINSTANCES *) &definstancesArray[obji];

   UpdateConstructHeader(&bdptr->header,&dfiptr->header,
                         (int) sizeof(DEFINSTANCES_MODULE),(void *) ModuleArray,
                         (int) sizeof(DEFINSTANCES),(void *) definstancesArray);
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
static void ClearDefinstancesBload()
  {
   register long i;
   unsigned long space;

   space = (unsigned long) (sizeof(DEFINSTANCES_MODULE) * ModuleCount);
   if (space == 0L)
     return;
   genlongfree((void *) ModuleArray,space);
   ModuleArray = NULL;
   ModuleCount = 0L;

   for (i = 0L ; i < DefinstancesCount ; i++)
     UnmarkConstructHeader(&definstancesArray[i].header);
   space = (unsigned long) (sizeof(DEFINSTANCES) * DefinstancesCount);
   if (space == 0L)
     return;
   genlongfree((void *) definstancesArray,space);
   definstancesArray = NULL;
   DefinstancesCount = 0L;
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
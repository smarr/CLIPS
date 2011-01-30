   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Generic Function Construct Compiler Code         */
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

#if DEFGENERIC_CONSTRUCT && CONSTRUCT_COMPILER && (! RUN_TIME)

#if DEFRULE_CONSTRUCT
#include "network.h"
#endif

#include "genrccom.h"
#include "conscomp.h"

#if OBJECT_SYSTEM
#include "objcmp.h"
#endif

#define _GENRCCMP_SOURCE_
#include "genrccmp.h"

/* =========================================
   *****************************************
                   CONSTANTS
   =========================================
   ***************************************** */
#define MODULEI      0
#define GENERICI     1
#define METHODI      2
#define RESTRICTIONI 3
#define TYPEI        4

#define SAVE_ITEMS   5

/* =========================================
   *****************************************
               MACROS AND TYPES
   =========================================
   ***************************************** */
#define MethodPrefix()      ArbitraryPrefix(DefgenericCodeItem,2)
#define RestrictionPrefix() ArbitraryPrefix(DefgenericCodeItem,3)
#define TypePrefix()        ArbitraryPrefix(DefgenericCodeItem,4)

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

static void ReadyDefgenericsForCode(void);
static int DefgenericsToCode(char *,int,FILE *,int,int);
static void CloseDefgenericFiles(FILE *[SAVE_ITEMS],int [SAVE_ITEMS],
                                 struct CodeGeneratorFile [SAVE_ITEMS],int);
static void DefgenericModuleToCode(FILE *,struct defmodule *,int,int);
static void SingleDefgenericToCode(FILE *,int,int,DEFGENERIC *,int,int,int);
static void MethodToCode(FILE *,int,DEFMETHOD *,int,int);
static void RestrictionToCode(FILE *,int,RESTRICTION *,int,int);
static void TypeToCode(FILE *,int,void *,int);

/* =========================================
   *****************************************
      INTERNALLY VISIBLE GLOBAL VARIABLES
   =========================================
   ***************************************** */
Thread static struct CodeGeneratorItem *DefgenericCodeItem;

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***************************************************
  NAME         : SetupGenericsCompiler
  DESCRIPTION  : Initializes the construct compiler
                   item for generic functions
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Code generator item initialized
  NOTES        : None
 ***************************************************/
globle void SetupGenericsCompiler()
  {
   DefgenericCodeItem = AddCodeGeneratorItem("generics",0,ReadyDefgenericsForCode,
                                             NULL,DefgenericsToCode,5);
  }

/***************************************************
  NAME         : PrintGenericFunctionReference
  DESCRIPTION  : Prints a reference to the run-time
                 generic array for the construct
                 compiler
  INPUTS       : 1) The file output destination
                 2) A pointer to the generic
                 3) The id of the run-time image
                 4) The maximum number of indices
                    in any array
  RETURNS      : Nothing useful
  SIDE EFFECTS : Reference printed
  NOTES        : None
 ***************************************************/
globle void PrintGenericFunctionReference(
  FILE *fp,
  DEFGENERIC *gfunc,
  int imageID,
  int maxIndices)
  {
   if (gfunc == NULL)
     fprintf(fp,"NULL");
   else
     fprintf(fp,"&%s%d_%d[%d]",ConstructPrefix(DefgenericCodeItem),imageID,
                                (int) ((gfunc->header.bsaveID / maxIndices) + 1),
                                (int) (gfunc->header.bsaveID % maxIndices));
  }

/****************************************************
  NAME         : DefgenericCModuleReference
  DESCRIPTION  : Prints out a reference to a
                 defgeneric module
  INPUTS       : 1) The output file
                 2) The id of the module item
                 3) The id of the image
                 4) The maximum number of elements
                    allowed in an array
  RETURNS      : Nothing useful
  SIDE EFFECTS : Defgeneric module reference printed
  NOTES        : None
 ****************************************************/
globle void DefgenericCModuleReference(
  FILE *theFile,
  int count,
  int imageID,
  int maxIndices)
  {
   fprintf(theFile,"MIHS &%s%d_%d[%d]",
                      ModulePrefix(DefgenericCodeItem),
                      imageID,
                      (count / maxIndices) + 1,
                      (count % maxIndices));
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***************************************************
  NAME         : ReadyDefgenericsForCode
  DESCRIPTION  : Sets index of generic-functions
                   for use in compiled expressions
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : BsaveIndices set
  NOTES        : None
 ***************************************************/
static void ReadyDefgenericsForCode()
  {
   MarkConstructBsaveIDs(DefgenericModuleIndex);
  }

/*******************************************************
  NAME         : DefgenericsToCode
  DESCRIPTION  : Writes out static array code for
                   generic functions, methods, etc.
  INPUTS       : 1) The base name of the construct set
                 2) The base id for this construct
                 3) The file pointer for the header file
                 4) The base id for the construct set
                 5) The max number of indices allowed
                    in an array
  RETURNS      : -1 if no generic functions, 0 on errors,
                  1 if generic functions written
  SIDE EFFECTS : Code written to files
  NOTES        : None
 *******************************************************/
static int DefgenericsToCode(
  char *fileName,
  int fileID,
  FILE *headerFP,
  int imageID,
  int maxIndices)
  {
   int fileCount = 1;
   struct defmodule *theModule;
   DEFGENERIC *theDefgeneric;
   DEFMETHOD *theMethod;
   RESTRICTION *theRestriction;
   register unsigned i,j,k;
   int moduleCount = 0;
   int itemArrayCounts[SAVE_ITEMS];
   int itemArrayVersions[SAVE_ITEMS];
   FILE *itemFiles[SAVE_ITEMS];
   int itemReopenFlags[SAVE_ITEMS];
   struct CodeGeneratorFile itemCodeFiles[SAVE_ITEMS];

   for (i = 0 ; i < SAVE_ITEMS ; i++)
     {
      itemArrayCounts[i] = 0;
      itemArrayVersions[i] = 1;
      itemFiles[i] = NULL;
      itemReopenFlags[i] = FALSE;
      itemCodeFiles[i].filePrefix = NULL;
     }

   /* ===========================================
      Include the appropriate generic header file
      =========================================== */
   fprintf(headerFP,"#include \"genrcfun.h\"\n");

   /* =============================================================
      Loop through all the modules and all the defgenerics writing
      their C code representation to the file as they are traversed
      ============================================================= */
   theModule = (struct defmodule *) GetNextDefmodule(NULL);

   while (theModule != NULL)
     {
      SetCurrentModule((void *) theModule);

      itemFiles[MODULEI] =
         OpenFileIfNeeded(itemFiles[MODULEI],fileName,fileID,imageID,&fileCount,
                          itemArrayVersions[MODULEI],headerFP,
                          "DEFGENERIC_MODULE",ModulePrefix(DefgenericCodeItem),
                          itemReopenFlags[MODULEI],&itemCodeFiles[MODULEI]);
      if (itemFiles[MODULEI] == NULL)
        goto GenericCodeError;

      DefgenericModuleToCode(itemFiles[MODULEI],theModule,imageID,maxIndices);
      itemFiles[MODULEI] =
          CloseFileIfNeeded(itemFiles[MODULEI],&itemArrayCounts[MODULEI],
                            &itemArrayVersions[MODULEI],maxIndices,
                            &itemReopenFlags[MODULEI],&itemCodeFiles[MODULEI]);

      theDefgeneric = (DEFGENERIC *) GetNextDefgeneric(NULL);

      while (theDefgeneric != NULL)
        {
         itemFiles[GENERICI] =
            OpenFileIfNeeded(itemFiles[GENERICI],fileName,fileID,imageID,&fileCount,
                             itemArrayVersions[GENERICI],headerFP,
                             "DEFGENERIC",ConstructPrefix(DefgenericCodeItem),
                             itemReopenFlags[GENERICI],&itemCodeFiles[GENERICI]);
         if (itemFiles[GENERICI] == NULL)
           goto GenericCodeError;

         SingleDefgenericToCode(itemFiles[GENERICI],imageID,maxIndices,theDefgeneric,
                                moduleCount,itemArrayVersions[METHODI],
                                itemArrayCounts[METHODI]);
         itemArrayCounts[GENERICI]++;
         itemFiles[GENERICI] =
           CloseFileIfNeeded(itemFiles[GENERICI],&itemArrayCounts[GENERICI],
                             &itemArrayVersions[GENERICI],maxIndices,
                             &itemReopenFlags[GENERICI],&itemCodeFiles[GENERICI]);
         if (theDefgeneric->mcnt > 0)
           {

            /* ===========================================
               Make sure that all methods for a particular
               generic function go into the same array
               =========================================== */
            itemFiles[METHODI] =
                OpenFileIfNeeded(itemFiles[METHODI],fileName,fileID,imageID,&fileCount,
                                 itemArrayVersions[METHODI],headerFP,
                                 "DEFMETHOD",MethodPrefix(),
                                 itemReopenFlags[METHODI],&itemCodeFiles[METHODI]);
            if (itemFiles[METHODI] == NULL)
              goto GenericCodeError;

            for (i = 0 ; i < theDefgeneric->mcnt ; i++)
              {
               theMethod = &theDefgeneric->methods[i];
               if (i > 0)
                 fprintf(itemFiles[METHODI],",\n");
               MethodToCode(itemFiles[METHODI],imageID,theMethod,
                            itemArrayVersions[RESTRICTIONI],itemArrayCounts[RESTRICTIONI]);
               if (theMethod->restrictionCount > 0)
                 {
                  /* ========================================
                     Make sure that all restrictions for a
                     particular method go into the same array
                     ======================================== */
                  itemFiles[RESTRICTIONI] =
                     OpenFileIfNeeded(itemFiles[RESTRICTIONI],fileName,fileID,
                                      imageID,&fileCount,
                                      itemArrayVersions[RESTRICTIONI],headerFP,
                                      "RESTRICTION",RestrictionPrefix(),
                                      itemReopenFlags[RESTRICTIONI],&itemCodeFiles[RESTRICTIONI]);
                  if (itemFiles[RESTRICTIONI] == NULL)
                    goto GenericCodeError;
                  for (j = 0 ; j < theMethod->restrictionCount ; j++)
                    {
                     theRestriction = &theMethod->restrictions[j];
                     if (j > 0)
                       fprintf(itemFiles[RESTRICTIONI],",\n");
                     RestrictionToCode(itemFiles[RESTRICTIONI],imageID,theRestriction,
                                       itemArrayVersions[TYPEI],itemArrayCounts[TYPEI]);

                     if (theRestriction->tcnt > 0)
                       {
                        /* =========================================
                           Make sure that all types for a particular
                           restriction go into the same array
                           ========================================= */
                        itemFiles[TYPEI] =
                           OpenFileIfNeeded(itemFiles[TYPEI],fileName,fileID,
                                            imageID,&fileCount,
                                            itemArrayVersions[TYPEI],headerFP,
                                            "void *",TypePrefix(),
                                            itemReopenFlags[TYPEI],&itemCodeFiles[TYPEI]);
                        if (itemFiles[TYPEI] == NULL)
                          goto GenericCodeError;
                        for (k = 0 ; k < theRestriction->tcnt ; k++)
                          {
                           if (k > 0)
                             fprintf(itemFiles[TYPEI],",\n");
                           TypeToCode(itemFiles[TYPEI],imageID,
                                      theRestriction->types[k],maxIndices);
                          }
                        itemArrayCounts[TYPEI] += theRestriction->tcnt;
                        itemFiles[TYPEI] =
                           CloseFileIfNeeded(itemFiles[TYPEI],&itemArrayCounts[TYPEI],
                                             &itemArrayVersions[TYPEI],maxIndices,
                                             &itemReopenFlags[TYPEI],&itemCodeFiles[TYPEI]);
                       }
                    }
                  itemArrayCounts[RESTRICTIONI] += theMethod->restrictionCount;
                  itemFiles[RESTRICTIONI] =
                     CloseFileIfNeeded(itemFiles[RESTRICTIONI],&itemArrayCounts[RESTRICTIONI],
                                       &itemArrayVersions[RESTRICTIONI],maxIndices,
                                       &itemReopenFlags[RESTRICTIONI],&itemCodeFiles[RESTRICTIONI]);
                 }
              }
            itemArrayCounts[METHODI] += theDefgeneric->mcnt;
            itemFiles[METHODI] =
               CloseFileIfNeeded(itemFiles[METHODI],&itemArrayCounts[METHODI],
                                 &itemArrayVersions[METHODI],maxIndices,
                                 &itemReopenFlags[METHODI],&itemCodeFiles[METHODI]);
           }
         theDefgeneric = (DEFGENERIC *) GetNextDefgeneric(theDefgeneric);
        }

      theModule = (struct defmodule *) GetNextDefmodule(theModule);
      moduleCount++;
      itemArrayCounts[MODULEI]++;
     }
   CloseDefgenericFiles(itemFiles,itemReopenFlags,itemCodeFiles,maxIndices);
   return(1);

GenericCodeError:
   CloseDefgenericFiles(itemFiles,itemReopenFlags,itemCodeFiles,maxIndices);
   return(0);
  }

/******************************************************
  NAME         : CloseDefgenericFiles
  DESCRIPTION  : Closes construct compiler files
                  for defgeneric structures
  INPUTS       : 1) An array containing all the
                    pertinent file pointers
                 2) An array containing all the
                    pertinent file reopen flags
                 3) An array containing all the
                    pertinent file name/id/version info
                 4) The maximum number of indices
                    allowed in an array
  RETURNS      : Nothing useful
  SIDE EFFECTS : Files closed
  NOTES        : None
 *****************************************************/
static void CloseDefgenericFiles(
  FILE *itemFiles[SAVE_ITEMS],
  int itemReopenFlags[SAVE_ITEMS],
  struct CodeGeneratorFile itemCodeFiles[SAVE_ITEMS],
  int maxIndices)
  {
   int count = maxIndices;
   int arrayVersion = 0;
   register int i;

   for (i = 0 ; i < SAVE_ITEMS ; i++)
     {
      count = maxIndices;
      itemFiles[i] = CloseFileIfNeeded(itemFiles[i],&count,&arrayVersion,
                                       maxIndices,&itemReopenFlags[i],
                                       &itemCodeFiles[i]);
     }
  }

/***************************************************
  NAME         : DefgenericModuleToCode
  DESCRIPTION  : Writes out the C values for a
                 defgeneric module item
  INPUTS       : 1) The output file
                 2) The module for the defgenerics
                 3) The compile image id
                 4) The maximum number of elements
                    in an array
  RETURNS      : Nothing useful
  SIDE EFFECTS : Defgeneric module item written
  NOTES        : None
 ***************************************************/
static void DefgenericModuleToCode(
  FILE *theFile,
  struct defmodule *theModule,
  int imageID,
  int maxIndices)
  {
   fprintf(theFile,"{");
   ConstructModuleToCode(theFile,theModule,imageID,maxIndices,
                         DefgenericModuleIndex,ConstructPrefix(DefgenericCodeItem));
   fprintf(theFile,"}");
  }

/****************************************************************
  NAME         : SingleDefgenericToCode
  DESCRIPTION  : Writes out a single defgeneric's
                 data to the file
  INPUTS       : 1)  The output file
                 2)  The compile image id
                 3)  The maximum number of
                     elements in an array
                 4)  The defgeneric
                 5)  The module index
                 6)  The partition holding the
                     generic methods
                 7) The relative index of the
                    generics methods in the partition
  RETURNS      : Nothing useful
  SIDE EFFECTS : Defgeneric data written
  NOTES        : None
 ***************************************************************/
static void SingleDefgenericToCode(
  FILE *theFile,
  int imageID,
  int maxIndices,
  DEFGENERIC *theDefgeneric,
  int moduleCount,
  int methodArrayVersion,
  int methodArrayCount)
  {
   /* ==================
      Defgeneric Header
      ================== */
   fprintf(theFile,"{");
   ConstructHeaderToCode(theFile,&theDefgeneric->header,imageID,maxIndices,moduleCount,
                         ModulePrefix(DefgenericCodeItem),
                         ConstructPrefix(DefgenericCodeItem));

   /* =========================
      Defgeneric specific data
      ========================= */
   fprintf(theFile,",0,0,");
   if (theDefgeneric->methods == NULL)
     fprintf(theFile,"NULL");
   else
     {
      fprintf(theFile,"&%s%d_%d[%d]",MethodPrefix(),imageID,
                      methodArrayVersion,methodArrayCount);
     }
   fprintf(theFile,",%u,0}",theDefgeneric->mcnt);
  }

/****************************************************************
  NAME         : MethodToCode
  DESCRIPTION  : Writes out a single method's
                 data to the file
  INPUTS       : 1)  The output file
                 2)  The compile image id
                 3)  The method
                 4)  The partition holding the
                     method restrictions
                 5) The relative index of the
                    method restrictions in the partition
  RETURNS      : Nothing useful
  SIDE EFFECTS : Method data written
  NOTES        : None
 ***************************************************************/
static void MethodToCode(
  FILE *theFile,
  int imageID,
  DEFMETHOD *theMethod,
  int restrictionArrayVersion,
  int restrictionArrayCount)
  {
   fprintf(theFile,"{%u,0,%d,%d,%d,%d,%u,0,",
                   theMethod->index,theMethod->restrictionCount,
                   theMethod->minRestrictions,theMethod->maxRestrictions,
                   theMethod->localVarCount,theMethod->system);
   if (theMethod->restrictions == NULL)
     fprintf(theFile,"NULL,");
   else
     fprintf(theFile,"&%s%d_%d[%d],",RestrictionPrefix(),imageID,
                                     restrictionArrayVersion,restrictionArrayCount);
   ExpressionToCode(theFile,theMethod->actions);
   fprintf(theFile,",NULL}");
  }

/****************************************************************
  NAME         : RestrictionToCode
  DESCRIPTION  : Writes out a single restriction's
                 data to the file
  INPUTS       : 1)  The output file
                 2)  The compile image id
                 3)  The restriction
                 4)  The partition holding the
                     restriction types
                 5) The relative index of the
                    restriction types in the partition
  RETURNS      : Nothing useful
  SIDE EFFECTS : Restriction data written
  NOTES        : None
 ***************************************************************/
static void RestrictionToCode(
  FILE *theFile,
  int imageID,
  RESTRICTION *theRestriction,
  int typeArrayVersion,
  int typeArrayCount)
  {
   fprintf(theFile,"{");
   if (theRestriction->types == NULL)
     fprintf(theFile,"NULL,");
   else
     fprintf(theFile,"&%s%d_%d[%d],",TypePrefix(),imageID,
                                     typeArrayVersion,typeArrayCount);
   ExpressionToCode(theFile,theRestriction->query);
   fprintf(theFile,",%u}",theRestriction->tcnt);
  }

/****************************************************************
  NAME         : TypeToCode
  DESCRIPTION  : Writes out a single type's
                 data to the file
  INPUTS       : 1)  The output file
                 2)  The compile image id
                 3)  The type
  RETURNS      : Nothing useful
  SIDE EFFECTS : Type data written
  NOTES        : None
 ***************************************************************/
static void TypeToCode(
  FILE *theFile,
  int imageID,
  void *theType,
  int maxIndices)
  {
#if OBJECT_SYSTEM
   fprintf(theFile,"VS ");
   PrintClassReference(theFile,(DEFCLASS *) theType,imageID,maxIndices);
#else
   PrintIntegerReference(theFile,(INTEGER_HN *) theType);
#endif
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
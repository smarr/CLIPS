   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*            DEFGLOBAL CONSTRUCTS-TO-C MODULE         */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the constructs-to-c feature for the   */
/*    defglobal construct.                                   */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _GLOBLCMP_SOURCE_

#include "setup.h"

#if DEFGLOBAL_CONSTRUCT && CONSTRUCT_COMPILER && (! RUN_TIME)

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "conscomp.h"
#include "globldef.h"

#include "globlcmp.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static int                     ConstructToCode(char *,int,FILE *,int,int);
   static void                    DefglobalToCode(FILE *,struct defglobal *,
                                                 int,int,int);
   static void                    DefglobalModuleToCode(FILE *,struct defmodule *,int,int,int);
   static void                    CloseDefglobalFiles(FILE *,FILE *,int);
   static void                    BeforeDefglobalsToCode(void);
   static void                    InitDefglobalsCode(FILE *,int,int);

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static struct CodeGeneratorItem *DefglobalCodeItem;

/***************************************************************/
/* DefglobalCompilerSetup: Initializes the defglobal construct */
/*    for use with the constructs-to-c command.                */
/***************************************************************/
globle void DefglobalCompilerSetup()
  {
   DefglobalCodeItem = AddCodeGeneratorItem("defglobal",0,BeforeDefglobalsToCode,
                                            InitDefglobalsCode,ConstructToCode,2);
  }

/**************************************************************/
/* BeforeDefglobalsToCode: Assigns each defglobal a unique ID */
/*   which will be used for pointer references when the data  */
/*   structures are written to a file as C code               */
/**************************************************************/
static void BeforeDefglobalsToCode()
  {
   MarkConstructBsaveIDs(DefglobalModuleIndex);
  }

/*************************************************/
/* InitDefglobalsCode: Writes out initialization */
/*   code for defglobals for a run-time module.  */
/*************************************************/
#if IBM_TBC
#pragma argsused
#endif
static void InitDefglobalsCode(
  FILE *initFP,
  int imageID,
  int maxIndices)
  {
#if MAC_MPW || MAC_MCW || IBM_MCW
#pragma unused(maxIndices)
#pragma unused(imageID)
#endif
   fprintf(initFP,"   ResetDefglobals();\n");
  }

/***********************************************************/
/* ConstructToCode: Produces defglobal code for a run-time */
/*   module created using the constructs-to-c function.    */
/***********************************************************/
static int ConstructToCode(
  char *fileName,
  int fileID,
  FILE *headerFP,
  int imageID,
  int maxIndices)
  {
   int fileCount = 1;
   struct defmodule *theModule;
   struct defglobal *theDefglobal;
   int moduleCount = 0, moduleArrayCount = 0, moduleArrayVersion = 1;
   int defglobalArrayCount = 0, defglobalArrayVersion = 1;
   FILE *moduleFile = NULL, *defglobalFile = NULL;

   /*================================================*/
   /* Include the appropriate defglobal header file. */
   /*================================================*/

   fprintf(headerFP,"#include \"globldef.h\"\n");

   /*===================================================================*/
   /* Loop through all the modules and all the defglobals writing their */
   /*  C code representation to the file as they are traversed.         */
   /*===================================================================*/

   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      SetCurrentModule((void *) theModule);

      moduleFile = OpenFileIfNeeded(moduleFile,fileName,fileID,imageID,&fileCount,
                                    moduleArrayVersion,headerFP,
                                    "struct defglobalModule",ModulePrefix(DefglobalCodeItem),
                                    FALSE,NULL);

      if (moduleFile == NULL)
        {
         CloseDefglobalFiles(moduleFile,defglobalFile,maxIndices);
         return(0);
        }

      DefglobalModuleToCode(moduleFile,theModule,imageID,maxIndices,moduleCount);
      moduleFile = CloseFileIfNeeded(moduleFile,&moduleArrayCount,&moduleArrayVersion,
                                     maxIndices,NULL,NULL);

      for (theDefglobal = (struct defglobal *) GetNextDefglobal(NULL);
           theDefglobal != NULL;
           theDefglobal = (struct defglobal *) GetNextDefglobal(theDefglobal))
        {
         defglobalFile = OpenFileIfNeeded(defglobalFile,fileName,fileID,imageID,&fileCount,
                                         defglobalArrayVersion,headerFP,
                                         "struct defglobal",ConstructPrefix(DefglobalCodeItem),
                                         FALSE,NULL);
         if (defglobalFile == NULL)
           {
            CloseDefglobalFiles(moduleFile,defglobalFile,maxIndices);
            return(0);
           }

         DefglobalToCode(defglobalFile,theDefglobal,imageID,maxIndices,moduleCount);
         defglobalArrayCount++;
         defglobalFile = CloseFileIfNeeded(defglobalFile,&defglobalArrayCount,
                                           &defglobalArrayVersion,maxIndices,NULL,NULL);
        }

      moduleCount++;
      moduleArrayCount++;
     }

   CloseDefglobalFiles(moduleFile,defglobalFile,maxIndices);

   return(1);
  }

/**********************************************************/
/* CloseDefglobalFiles: Closes all of the C files created */
/*   for defglobals. Called when an error occurs or when  */
/*   the defglobals have all been written to the files.   */
/**********************************************************/
static void CloseDefglobalFiles(
  FILE *moduleFile,
  FILE *defglobalFile,
  int maxIndices)
  {
   int count = maxIndices;
   int arrayVersion = 0;

   if (defglobalFile != NULL)
     {
      count = maxIndices;
      CloseFileIfNeeded(defglobalFile,&count,&arrayVersion,maxIndices,NULL,NULL);
     }

   if (moduleFile != NULL)
     {
      count = maxIndices;
      CloseFileIfNeeded(moduleFile,&count,&arrayVersion,maxIndices,NULL,NULL);
     }
  }

/***********************************************************/
/* DefglobalModuleToCode: Writes the C code representation */
/*   of a single defglobal module to the specified file.   */
/***********************************************************/
#if IBM_TBC
#pragma argsused
#endif
static void DefglobalModuleToCode(
  FILE *theFile,
  struct defmodule *theModule,
  int imageID,
  int maxIndices,
  int moduleCount)
  {
#if MAC_MPW || MAC_MCW || IBM_MCW
#pragma unused(moduleCount)
#endif

   fprintf(theFile,"{");

   ConstructModuleToCode(theFile,theModule,imageID,maxIndices,
                                  DefglobalModuleIndex,ConstructPrefix(DefglobalCodeItem));

   fprintf(theFile,"}");
  }

/**********************************************************/
/* DefglobalToCode: Writes the C code representation of a */
/*   single defglobal construct to the specified file.    */
/**********************************************************/
static void DefglobalToCode(
  FILE *theFile,
  struct defglobal *theDefglobal,
  int imageID,
  int maxIndices,
  int moduleCount)
  {
   /*==================*/
   /* Defglobal Header */
   /*==================*/

   fprintf(theFile,"{");

   ConstructHeaderToCode(theFile,&theDefglobal->header,imageID,maxIndices,
                         moduleCount,ModulePrefix(DefglobalCodeItem),
                         ConstructPrefix(DefglobalCodeItem));

   fprintf(theFile,",");

   /*============================================*/
   /* Watch Flag, In Scope Flag, and Busy Count. */
   /*============================================*/

   fprintf(theFile,"0,0,%ld,",theDefglobal->busyCount);

   /*================*/
   /* Current Value. */
   /*================*/

   fprintf(theFile,"{NULL,RVOID}");

   /*=====================*/
   /* Initial Expression. */
   /*=====================*/

   fprintf(theFile,",");
   PrintHashedExpressionReference(theFile,theDefglobal->initial,imageID,maxIndices);

   fprintf(theFile,"}");
  }

/***************************************************************/
/* DefglobalCModuleReference: Writes the C code representation */
/*   of a reference to a defglobal module data structure.      */
/***************************************************************/
globle void DefglobalCModuleReference(
  FILE *theFile,
  int count,
  int imageID,
  int maxIndices)
  {
   fprintf(theFile,"MIHS &%s%d_%d[%d]",
                      ModulePrefix(DefglobalCodeItem),
                      imageID,
                      (count / maxIndices) + 1,
                      (count % maxIndices));
  }

/******************************************************************/
/* DefglobalCConstructReference: Writes the C code representation */
/*   of a reference to a defglobal data structure.                */
/******************************************************************/
globle void DefglobalCConstructReference(
  FILE *theFile,
  void *vTheGlobal,
  int imageID,
  int maxIndices)
  {
   struct defglobal *theGlobal = (struct defglobal *) vTheGlobal;

   if (theGlobal == NULL)
     { fprintf(theFile,"NULL"); }
   else
     {
      fprintf(theFile,"&%s%d_%ld[%ld]",ConstructPrefix(DefglobalCodeItem),
                      imageID,
                      (theGlobal->header.bsaveID / maxIndices) + 1,
                      theGlobal->header.bsaveID % maxIndices);
     }

  }

#endif /* DEFGLOBAL_CONSTRUCT && CONSTRUCT_COMPILER && (! RUN_TIME) */



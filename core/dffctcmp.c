   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*            DEFFACTS CONSTRUCTS-TO-C MODULE          */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the constructs-to-c feature for the   */
/*    deffacts construct.                                    */
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

#define _DFFCTCMP_SOURCE_

#include "setup.h"

#if DEFFACTS_CONSTRUCT && CONSTRUCT_COMPILER && (! RUN_TIME)

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "conscomp.h"
#include "dffctdef.h"

#include "dffctcmp.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static int                     ConstructToCode(char *,int,FILE *,int,int);
   static void                    DeffactsToCode(FILE *,struct deffacts *,
                                                 int,int,int);
   static void                    DeffactsModuleToCode(FILE *,struct defmodule *,int,int,int);
   static void                    CloseDeffactsFiles(FILE *,FILE *,int);
   static void                    BeforeDeffactsToCode(void);

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static struct CodeGeneratorItem *DeffactsCodeItem;

/*************************************************************/
/* DeffactsCompilerSetup: Initializes the deffacts construct */
/*    for use with the constructs-to-c command.              */
/*************************************************************/
globle void DeffactsCompilerSetup()
  {
   DeffactsCodeItem = AddCodeGeneratorItem("deffacts",0,BeforeDeffactsToCode,
                                           NULL,ConstructToCode,2);
  }

/*************************************************************/
/* BeforeDeffactsToCode: Assigns each deffacts a unique ID   */
/*   which will be used for pointer references when the data */
/*   structures are written to a file as C code              */
/*************************************************************/
static void BeforeDeffactsToCode()
  {
   MarkConstructBsaveIDs(DeffactsModuleIndex);
  }

/**********************************************************/
/* ConstructToCode: Produces deffacts code for a run-time */
/*   module created using the constructs-to-c function.   */
/**********************************************************/
static int ConstructToCode(
  char *fileName,
  int fileID,
  FILE *headerFP,
  int imageID,
  int maxIndices)
  {
   int fileCount = 1;
   struct defmodule *theModule;
   struct deffacts *theDeffacts;
   int moduleCount = 0, moduleArrayCount = 0, moduleArrayVersion = 1;
   int deffactsArrayCount = 0, deffactsArrayVersion = 1;
   FILE *moduleFile = NULL, *deffactsFile = NULL;

   /*===============================================*/
   /* Include the appropriate deffacts header file. */
   /*===============================================*/

   fprintf(headerFP,"#include \"dffctdef.h\"\n");

   /*=================================================================*/
   /* Loop through all the modules and all the deffacts writing their */
   /* C code representation to the file as they are traversed.        */
   /*=================================================================*/

   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      SetCurrentModule((void *) theModule);

      moduleFile = OpenFileIfNeeded(moduleFile,fileName,fileID,imageID,&fileCount,
                                    moduleArrayVersion,headerFP,
                                    "struct deffactsModule",ModulePrefix(DeffactsCodeItem),
                                    FALSE,NULL);

      if (moduleFile == NULL)
        {
         CloseDeffactsFiles(moduleFile,deffactsFile,maxIndices);
         return(0);
        }

      DeffactsModuleToCode(moduleFile,theModule,imageID,maxIndices,moduleCount);
      moduleFile = CloseFileIfNeeded(moduleFile,&moduleArrayCount,&moduleArrayVersion,
                                     maxIndices,NULL,NULL);

      /*===================================================*/
      /* Loop through each of the deffacts in this module. */
      /*===================================================*/

      for (theDeffacts = (struct deffacts *) GetNextDeffacts(NULL);
           theDeffacts != NULL;
           theDeffacts = (struct deffacts *) GetNextDeffacts(theDeffacts))
        {
         deffactsFile = OpenFileIfNeeded(deffactsFile,fileName,fileID,imageID,&fileCount,
                                         deffactsArrayVersion,headerFP,
                                         "struct deffacts",ConstructPrefix(DeffactsCodeItem),
                                         FALSE,NULL);
         if (deffactsFile == NULL)
           {
            CloseDeffactsFiles(moduleFile,deffactsFile,maxIndices);
            return(0);
           }

         DeffactsToCode(deffactsFile,theDeffacts,imageID,maxIndices,moduleCount);
         deffactsArrayCount++;
         deffactsFile = CloseFileIfNeeded(deffactsFile,&deffactsArrayCount,
                                          &deffactsArrayVersion,maxIndices,NULL,NULL);
        }

      moduleCount++;
      moduleArrayCount++;
     }

   CloseDeffactsFiles(moduleFile,deffactsFile,maxIndices);

   return(1);
  }

/*********************************************************/
/* CloseDeffactsFiles: Closes all of the C files created */
/*   for deffacts. Called when an error occurs or when   */
/*   the deffacts have all been written to the files.    */
/*********************************************************/
static void CloseDeffactsFiles(
  FILE *moduleFile,
  FILE *deffactsFile,
  int maxIndices)
  {
   int count = maxIndices;
   int arrayVersion = 0;

   if (deffactsFile != NULL)
     {
      count = maxIndices;
      CloseFileIfNeeded(deffactsFile,&count,&arrayVersion,maxIndices,NULL,NULL);
     }

   if (moduleFile != NULL)
     {
      count = maxIndices;
      CloseFileIfNeeded(moduleFile,&count,&arrayVersion,maxIndices,NULL,NULL);
     }
  }

/**********************************************************/
/* DeffactsModuleToCode: Writes the C code representation */
/*   of a single deffacts module to the specified file.   */
/**********************************************************/
#if IBM_TBC
#pragma argsused
#endif
static void DeffactsModuleToCode(
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
                                  DeffactsModuleIndex,ConstructPrefix(DeffactsCodeItem));

   fprintf(theFile,"}");
  }

/*********************************************************/
/* DeffactsToCode: Writes the C code representation of a */
/*   single deffacts construct to the specified file.    */
/*********************************************************/
static void DeffactsToCode(
  FILE *theFile,
  struct deffacts *theDeffacts,
  int imageID,
  int maxIndices,
  int moduleCount)
  {
   /*=================*/
   /* Deffacts Header */
   /*=================*/

   fprintf(theFile,"{");

   ConstructHeaderToCode(theFile,&theDeffacts->header,imageID,maxIndices,
                         moduleCount,ModulePrefix(DeffactsCodeItem),
                         ConstructPrefix(DeffactsCodeItem));

   fprintf(theFile,",");

   /*=============*/
   /* Assert List */
   /*=============*/

   ExpressionToCode(theFile,theDeffacts->assertList);
   fprintf(theFile,"}");
  }

/**************************************************************/
/* DeffactsCModuleReference: Writes the C code representation */
/*   of a reference to a deffacts module data structure.      */
/**************************************************************/
globle void DeffactsCModuleReference(
  FILE *theFile,
  int count,
  int imageID,
  int maxIndices)
  {
   fprintf(theFile,"MIHS &%s%d_%d[%d]",
                      ModulePrefix(DeffactsCodeItem),
                      imageID,
                      (count / maxIndices) + 1,
                      (count % maxIndices));
  }

#endif /* DEFFACTS_CONSTRUCT && CONSTRUCT_COMPILER && (! RUN_TIME) */



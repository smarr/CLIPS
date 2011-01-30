   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*           DEFMODULE CONSTRUCTS-TO-C MODULE          */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the constructs-to-c feature for the   */
/*    defmodule construct.                                   */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _MODULCMP_SOURCE_

#include "setup.h"

#if CONSTRUCT_COMPILER && (! RUN_TIME)

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "conscomp.h"
#include "moduldef.h"

#include "modulcmp.h"

/***************/
/* DEFINITIONS */
/***************/

#define ItemPrefix()      ArbitraryPrefix(DefmoduleCodeItem,0)
#define DefmodulePrefix() ArbitraryPrefix(DefmoduleCodeItem,1)
#define PortPrefix()      ArbitraryPrefix(DefmoduleCodeItem,2)

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static int                     ConstructToCode(char *,int,FILE *,int,int);
   static void                    InitDefmoduleCode(FILE *,int,int);
   static struct portItem        *GetNextPortItem(struct defmodule **,struct portItem **,
                                                  int *,int *);
   static int                     PortItemsToCode(char *,int,FILE *,int,int,int *);
   static void                    BeforeDefmodulesToCode(void);

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static struct CodeGeneratorItem *DefmoduleCodeItem;

/***************************************************************/
/* DefmoduleCompilerSetup: Initializes the defmodule construct */
/*    for use with the constructs-to-c command.                */
/***************************************************************/
globle void DefmoduleCompilerSetup()
  {
   DefmoduleCodeItem = AddCodeGeneratorItem("defmodule",200,BeforeDefmodulesToCode,
                                            InitDefmoduleCode,ConstructToCode,3);
  }

/***********************************************************/
/* BeforeDefmodulesToCode: Assigns each defmodule a unique */
/*   ID which will be used for pointer references when the */
/*   data structures are written to a file as C code       */
/***********************************************************/
static void BeforeDefmodulesToCode()
  {
   int value = 0;
   struct defmodule *theModule;

   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     { theModule->bsaveID = value++; }
  }

/*************************************************************/
/* PrintDefmoduleReference: Writes the C code representation */
/*   of a reference to a defmodule data structure.           */
/*************************************************************/
globle void PrintDefmoduleReference(
  FILE *theFile,
  struct defmodule *theModule)
  {
   if (theModule == NULL) fprintf(theFile,"NULL");
   else fprintf(theFile,"&%s%d_%ld[%ld]",DefmodulePrefix(),ImageID,
                                    (long) ((theModule->bsaveID / MaxIndices) + 1),
                                    (long) (theModule->bsaveID % MaxIndices));
  }

/************************************************/
/* InitDefmoduleCode: Writes out initialization */
/*   code for defmodules for a run-time module. */
/************************************************/
#if IBM_TBC
#pragma argsused
#endif
static void InitDefmoduleCode(
  FILE *initFP,
  int imageID,
  int maxIndices)
  {
#if MAC_MPW || MAC_MCW || IBM_MCW
#pragma unused(maxIndices)
#endif
   if (GetNextDefmodule(NULL) != NULL)
     { fprintf(initFP,"   SetListOfDefmodules((void *) %s%d_1);\n",DefmodulePrefix(),imageID); }
   else
     { fprintf(initFP,"   SetListOfDefmodules(NULL);\n"); }
   fprintf(initFP,"   SetCurrentModule((void *) GetNextDefmodule(NULL));\n");
  }

/***********************************************************/
/* ConstructToCode: Produces defmodule code for a run-time */
/*   module created using the constructs-to-c function.    */
/***********************************************************/
static int ConstructToCode(
  char *fileName,
  int fileID,
  FILE *headerFP,
  int imageID,
  int maxIndices)
  {
   struct defmodule *theConstruct;
   FILE *moduleFile = NULL, *itemsFile;
   int portItemCount = 0;
   struct portItem *portItemPtr;
   int mihCount = 0, moduleCount = 0;
   int j;
   struct moduleItem *theItem;
   int moduleArrayVersion = 1;
   int fileCount = 2;

   /*================================================*/
   /* Include the appropriate defmodule header file. */
   /*================================================*/

   fprintf(headerFP,"#include \"moduldef.h\"\n");

   /*============================================*/
   /* Open up the items file for the defmodules. */
   /* Only one file of this type is created so   */
   /* the maximum number of indices is ignored.  */
   /*============================================*/

   if ((itemsFile = NewCFile(fileName,fileID,1,FALSE)) == NULL)
     { return(FALSE); }
   fprintf(itemsFile,"struct defmoduleItemHeader *%s%d_%d[] = {\n",ItemPrefix(),imageID,1);
   fprintf(headerFP,"extern struct defmoduleItemHeader *%s%d_%d[];\n",ItemPrefix(),imageID,1);

   /*======================================================*/
   /* Loop through all the defmodules writing their C code */
   /* representation to the file as they are traversed.    */
   /*======================================================*/

   for (theConstruct = (struct defmodule *) GetNextDefmodule(NULL);
        theConstruct != NULL;
        theConstruct = (struct defmodule *) GetNextDefmodule(theConstruct))
     {
      /*===========================================*/
      /* Open a new file to write to if necessary. */
      /*===========================================*/

      moduleFile = OpenFileIfNeeded(moduleFile,fileName,fileID,imageID,
                                    &fileCount,moduleArrayVersion,headerFP,
                                    "struct defmodule",DefmodulePrefix(),
                                    FALSE,NULL);

      if (moduleFile == NULL)
        {
         moduleCount = maxIndices;
         CloseFileIfNeeded(moduleFile,&moduleCount,
                           &moduleArrayVersion,maxIndices,NULL,NULL);
         fclose(itemsFile);
         return(FALSE);
        }

      /*======================================*/
      /* Write the construct name and ppform. */
      /*======================================*/

      fprintf(moduleFile,"{");
      PrintSymbolReference(moduleFile,theConstruct->name);
      fprintf(moduleFile,",NULL,");

      /*=====================================================*/
      /* Write the items array pointers to other constructs. */
      /*=====================================================*/

      fprintf(moduleFile,"&%s%d_1[%d],",ItemPrefix(),imageID,mihCount);

      for (j = 0, theItem = GetListOfModuleItems();
           (j < GetNumberOfModuleItems()) && (theItem != NULL) ;
           j++, theItem = theItem->next)
        {
         mihCount++;
         if (theItem->constructsToCModuleReference == NULL)
           { fprintf(itemsFile,"NULL"); }
         else
           { (*theItem->constructsToCModuleReference)(itemsFile,(int) theConstruct->bsaveID,imageID,maxIndices); }

         if ((j + 1) < GetNumberOfModuleItems()) fprintf(itemsFile,",");
         else if (theConstruct->next != NULL) fprintf(itemsFile,",\n");
        }

      /*=================================*/
      /* Write the importList reference. */
      /*=================================*/

      if (theConstruct->importList == NULL)
        { fprintf(moduleFile,"NULL,"); }
      else
        {
         fprintf(moduleFile,"&%s%d_%d[%d],",PortPrefix(),imageID,
                                     (portItemCount / maxIndices) + 1,
                                     portItemCount % maxIndices);
         for (portItemPtr = theConstruct->importList;
              portItemPtr != NULL;
              portItemPtr = portItemPtr->next)
           { portItemCount++; }
        }

      /*=================================*/
      /* Write the exportList reference. */
      /*=================================*/

      if (theConstruct->exportList == NULL)
        { fprintf(moduleFile,"NULL,"); }
      else
        {
         fprintf(moduleFile,"&%s%d_%d[%d],",PortPrefix(),imageID,
                                     (portItemCount / maxIndices) + 1,
                                     portItemCount % maxIndices);
         for (portItemPtr = theConstruct->exportList;
              portItemPtr != NULL;
              portItemPtr = portItemPtr->next)
           { portItemCount++; }
        }

      /*=====================*/
      /* Write the bsave id. */
      /*=====================*/

      fprintf(moduleFile,"0,%ld,",theConstruct->bsaveID);
      
      /*======================*/
      /* Write the user data. */
      /*======================*/

      fprintf(moduleFile,"NULL,");

      /*===========================*/
      /* Write the next reference. */
      /*===========================*/

      if (theConstruct->next == NULL)
        { fprintf(moduleFile,"NULL}"); }
      else
        {
         fprintf(moduleFile,"&%s%d_%d[%d]}",ConstructPrefix(DefmoduleCodeItem),imageID,
                            (int) (theConstruct->next->bsaveID / maxIndices) + 1,
                            (int) theConstruct->next->bsaveID % maxIndices);
        }

      /*===================================================*/
      /* Increment the number of defmodule data structures */
      /* written and close the output file if necessary.   */
      /*===================================================*/

      moduleCount++;
      moduleFile = CloseFileIfNeeded(moduleFile,&moduleCount,&moduleArrayVersion,
                                        maxIndices,NULL,NULL);

     }

   /*=========================*/
   /* Close the output files. */
   /*=========================*/

   moduleCount = maxIndices;
   CloseFileIfNeeded(moduleFile,&moduleCount,
                     &moduleArrayVersion,maxIndices,NULL,NULL);
   fprintf(itemsFile,"};\n");
   fclose(itemsFile);

   /*=========================================*/
   /* Write out the portItem data structures. */
   /*=========================================*/

   if (portItemCount == 0) return(TRUE);
   return(PortItemsToCode(fileName,fileID,headerFP,imageID,maxIndices,&fileCount));
  }

/************************************************************/
/* PortItemsToCode: Writes the C code representation of all */
/*   portItem data structure nodes the specified file.      */
/************************************************************/
static int PortItemsToCode(
  char *fileName,
  int fileID,
  FILE *headerFP,
  int imageID,
  int maxIndices,
  int *fileCount)
  {
   struct defmodule *theDefmodule = NULL;
   struct portItem *thePortItem = NULL;
   int portItemCount = 0;
   int importChecked = FALSE;
   int exportChecked = FALSE;
   FILE *portItemsFile = NULL;
   int portItemArrayVersion = 1;

   /*=================================================================*/
   /* Loop through each of the portItem data structures writing their */
   /* C code representation to the file as they are traversed.        */
   /*=================================================================*/

   for (thePortItem = GetNextPortItem(&theDefmodule,&thePortItem,&importChecked,&exportChecked);
        thePortItem != NULL;
        thePortItem = GetNextPortItem(&theDefmodule,&thePortItem,&importChecked,&exportChecked))
     {
      /*===========================================*/
      /* Open a new file to write to if necessary. */
      /*===========================================*/

      portItemsFile = OpenFileIfNeeded(portItemsFile,fileName,fileID,imageID,
                                       fileCount,portItemArrayVersion,headerFP,
                                       "struct portItem",PortPrefix(),
                                       FALSE,NULL);

      if (portItemsFile == NULL)
        {
         portItemCount = maxIndices;
         CloseFileIfNeeded(portItemsFile,&portItemCount,
                           &portItemArrayVersion,maxIndices,NULL,NULL);
         return(FALSE);
        }

      /*================================================*/
      /* Write the portItem data structure to the file. */
      /*================================================*/

      fprintf(portItemsFile,"{");
      PrintSymbolReference(portItemsFile,thePortItem->moduleName);
      fprintf(portItemsFile,",");
      PrintSymbolReference(portItemsFile,thePortItem->constructType);
      fprintf(portItemsFile,",");
      PrintSymbolReference(portItemsFile,thePortItem->constructName);
      fprintf(portItemsFile,",");

      if (thePortItem->next == NULL)
        { fprintf(portItemsFile,"NULL}"); }
      else
        {
         fprintf(portItemsFile,"&%s%d_%d[%d]}",PortPrefix(),imageID,
                                  ((portItemCount+1) / maxIndices) + 1,
                                   (portItemCount+1) % maxIndices);
        }

      /*==================================================*/
      /* Increment the number of portItem data structures */
      /* written and close the output file if necessary.  */
      /*==================================================*/

      portItemCount++;
      CloseFileIfNeeded(portItemsFile,&portItemCount,&portItemArrayVersion,
                        maxIndices,NULL,NULL);
     }

   /*===================================================*/
   /* Close the output file and return TRUE to indicate */
   /* the data structures were successfully written.    */
   /*===================================================*/

   portItemCount = maxIndices;
   CloseFileIfNeeded(portItemsFile,&portItemCount,
                     &portItemArrayVersion,maxIndices,NULL,NULL);

   return(TRUE);
  }

/*********************************************************************/
/* GetNextPortItem: Given a pointer to a portItem data structure     */
/*   and its defmodule, returns the "next" portItem data structure.  */
/*   If passed a NULL value for both the defmodule and portItem      */
/*   data structure, it returns the "first" portItem data structure. */
/*********************************************************************/
static struct portItem *GetNextPortItem(
  struct defmodule **theDefmodule,
  struct portItem **thePortItem,
  int *importChecked,
  int *exportChecked)
  {
   /*====================================================*/
   /* If the defmodule pointer is NULL, then the "first" */
   /* portItem data structure should be returned. Start  */
   /* the search in the "first" defmodule.               */
   /*====================================================*/

   if (*theDefmodule == NULL)
     {
      *theDefmodule = (struct defmodule *) GetNextDefmodule(NULL);
      *thePortItem = NULL;
      *importChecked = FALSE;
      *exportChecked = FALSE;
     }

   /*==============================================*/
   /* Loop through all of the defmodules until the */
   /* "next" portItem data structure is found.     */
   /*==============================================*/

   while (*theDefmodule != NULL)
     {
      /*==========================================*/
      /* Check to see if there's another portItem */
      /* in the import/export list that's being   */
      /* checked in the module being examined.    */
      /*==========================================*/

      if (*thePortItem != NULL) *thePortItem = (*thePortItem)->next;
      if (*thePortItem != NULL) return(*thePortItem);

      /*==================================================*/
      /* If we haven't checked the import list yet, begin */
      /* checking it. If there aren't any items in the    */
      /* import list, then check the export list.         */
      /*==================================================*/

      if (! (*importChecked))
        {
         *thePortItem = (*theDefmodule)->importList;
         *importChecked = TRUE;
         if (*thePortItem == NULL)
           {
            *thePortItem = (*theDefmodule)->exportList;
            *exportChecked = TRUE;
           }
        }

      /*======================================*/
      /* Otherwise, if we haven't checked the */
      /* export list yet, begin checking it.  */
      /*======================================*/

      else if (! (*exportChecked))
        {
         *exportChecked = TRUE;
         *thePortItem = (*theDefmodule)->exportList;
        }

      /*==========================================*/
      /* If the import or export list contained a */
      /* portItem data structure, then return it. */
      /*==========================================*/

      if (*thePortItem != NULL) return(*thePortItem);

      /*==================================*/
      /* Otherwise, check the next module */
      /* for a portItem data structure.   */
      /*==================================*/

      *theDefmodule = (struct defmodule *) GetNextDefmodule(*theDefmodule);
      *importChecked = FALSE;
      *exportChecked = FALSE;
     }

   /*=======================================================*/
   /* All the portItem data structures have been traversed. */
   /* Return NULL to indicate that none are left.           */
   /*=======================================================*/

   return(NULL);
  }

#endif /* CONSTRUCT_COMPILER && (! RUN_TIME) */



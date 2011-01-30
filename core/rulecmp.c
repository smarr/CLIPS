
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*            DEFRULE CONSTRUCTS-TO-C MODULE           */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the constructs-to-c feature for the   */
/*    defrule construct.                                     */
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

#define _RULECMP_SOURCE_

#include "setup.h"

#if DEFRULE_CONSTRUCT && (! RUN_TIME) && CONSTRUCT_COMPILER

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include "factbld.h"
#include "reteutil.h"

#include "rulecmp.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static int                     ConstructToCode(char *,int,FILE *,int,int);
   static void                    JoinToCode(FILE *,struct joinNode *,int,int);
   static void                    DefruleModuleToCode(FILE *,struct defmodule *,int,int,int);
   static void                    DefruleToCode(FILE *,struct defrule *,int,int,int);
   static void                    CloseDefruleFiles(FILE *,FILE *,FILE *,int);
   static void                    BeforeDefrulesCode(void);

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle struct CodeGeneratorItem *DefruleCodeItem;

/***********************************************************/
/* DefruleCompilerSetup: Initializes the defrule construct */
/*   for use with the constructs-to-c command.             */
/***********************************************************/
globle void DefruleCompilerSetup()
  {
   DefruleCodeItem = AddCodeGeneratorItem("defrules",0,BeforeDefrulesCode,
                                          NULL,ConstructToCode,3);
  }

/**************************************************************/
/* BeforeDefrulesCode: Assigns each defrule and join with a   */
/*   unique ID which will be used for pointer references when */
/*   the data structures are written to a file as C code      */
/**************************************************************/
static void BeforeDefrulesCode()
  {
   long int moduleCount, ruleCount, joinCount;

   TagRuleNetwork(&moduleCount,&ruleCount,&joinCount);
  }

/*********************************************************/
/* ConstructToCode: Produces defrule code for a run-time */
/*   module created using the constructs-to-c function.  */
/*********************************************************/
static int ConstructToCode(
  char *fileName,
  int fileID,
  FILE *headerFP,
  int imageID,
  int maxIndices)
  {
   int fileCount = 1;
   struct defmodule *theModule;
   struct defrule *theDefrule;
   struct joinNode *theJoin;
   int joinArrayCount = 0, joinArrayVersion = 1;
   int moduleCount = 0, moduleArrayCount = 0, moduleArrayVersion = 1;
   int defruleArrayCount = 0, defruleArrayVersion = 1;
   FILE *joinFile = NULL, *moduleFile = NULL, *defruleFile = NULL;

   /*==============================================*/
   /* Include the appropriate defrule header file. */
   /*==============================================*/

   fprintf(headerFP,"#include \"ruledef.h\"\n");

   /*=========================================================*/
   /* Loop through all the modules, all the defrules, and all */
   /* the join nodes writing their C code representation to   */
   /* the file as they are traversed.                         */
   /*=========================================================*/

   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      /*=========================*/
      /* Set the current module. */
      /*=========================*/

      SetCurrentModule((void *) theModule);

      /*==========================*/
      /* Save the defrule module. */
      /*==========================*/

      moduleFile = OpenFileIfNeeded(moduleFile,fileName,fileID,imageID,&fileCount,
                                    moduleArrayVersion,headerFP,
                                    "struct defruleModule",ModulePrefix(DefruleCodeItem),
                                    FALSE,NULL);

      if (moduleFile == NULL)
        {
         CloseDefruleFiles(moduleFile,defruleFile,joinFile,maxIndices);
         return(0);
        }

      DefruleModuleToCode(moduleFile,theModule,imageID,maxIndices,moduleCount);
      moduleFile = CloseFileIfNeeded(moduleFile,&moduleArrayCount,&moduleArrayVersion,
                                     maxIndices,NULL,NULL);

      /*=========================================*/
      /* Loop through all of the defrules (and   */
      /* their disjuncts) in the current module. */
      /*=========================================*/

      theDefrule = (struct defrule *) GetNextDefrule(NULL);

      while (theDefrule != NULL)
        {
         /*===================================*/
         /* Save the defrule data structures. */
         /*===================================*/

         defruleFile = OpenFileIfNeeded(defruleFile,fileName,fileID,imageID,&fileCount,
                                        defruleArrayVersion,headerFP,
                                        "struct defrule",ConstructPrefix(DefruleCodeItem),
                                        FALSE,NULL);
         if (defruleFile == NULL)
           {
            CloseDefruleFiles(moduleFile,defruleFile,joinFile,maxIndices);
            return(0);
           }

         DefruleToCode(defruleFile,theDefrule,imageID,maxIndices,
                        moduleCount);
         defruleArrayCount++;
         defruleFile = CloseFileIfNeeded(defruleFile,&defruleArrayCount,&defruleArrayVersion,
                                         maxIndices,NULL,NULL);

         /*================================*/
         /* Save the join data structures. */
         /*================================*/

         for (theJoin = theDefrule->lastJoin;
              theJoin != NULL;
              theJoin = GetPreviousJoin(theJoin))
           {
            if (theJoin->marked)
              {
               joinFile = OpenFileIfNeeded(joinFile,fileName,fileID,imageID,&fileCount,
                                        joinArrayVersion,headerFP,
                                       "struct joinNode",JoinPrefix(),FALSE,NULL);
               if (joinFile == NULL)
                 {
                  CloseDefruleFiles(moduleFile,defruleFile,joinFile,maxIndices);
                  return(0);
                 }

               JoinToCode(joinFile,theJoin,imageID,maxIndices);
               joinArrayCount++;
               joinFile = CloseFileIfNeeded(joinFile,&joinArrayCount,&joinArrayVersion,
                                            maxIndices,NULL,NULL);
              }
           }

         /*==========================================*/
         /* Move on to the next disjunct or defrule. */
         /*==========================================*/

         if (theDefrule->disjunct != NULL) theDefrule = theDefrule->disjunct;
         else theDefrule = (struct defrule *) GetNextDefrule(theDefrule);
        }

      moduleCount++;
      moduleArrayCount++;
     }

   CloseDefruleFiles(moduleFile,defruleFile,joinFile,maxIndices);

   return(1);
  }

/********************************************************/
/* CloseDefruleFiles: Closes all of the C files created */
/*   for defrule. Called when an error occurs or when   */
/*   the defrules have all been written to the files.   */
/********************************************************/
static void CloseDefruleFiles(
  FILE *moduleFile,
  FILE *defruleFile,
  FILE *joinFile,
  int maxIndices)
  {
   int count = maxIndices;
   int arrayVersion = 0;

   if (joinFile != NULL)
     {
      count = maxIndices;
      CloseFileIfNeeded(joinFile,&count,&arrayVersion,maxIndices,NULL,NULL);
     }

   if (defruleFile != NULL)
     {
      count = maxIndices;
      CloseFileIfNeeded(defruleFile,&count,&arrayVersion,maxIndices,NULL,NULL);
     }

   if (moduleFile != NULL)
     {
      count = maxIndices;
      CloseFileIfNeeded(moduleFile,&count,&arrayVersion,maxIndices,NULL,NULL);
     }
  }

/*********************************************************/
/* DefruleModuleToCode: Writes the C code representation */
/*   of a single defrule module to the specified file.   */
/*********************************************************/
#if IBM_TBC
#pragma argsused
#endif
static void DefruleModuleToCode(
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
                                  DefruleModuleIndex,ConstructPrefix(DefruleCodeItem));

   fprintf(theFile,",NULL}");
  }

/**********************************************************/
/* DefruleToCode: Writes the C code representation of a   */
/*   single defrule data structure to the specified file. */
/**********************************************************/
static void DefruleToCode(
  FILE *theFile,
  struct defrule *theDefrule,
  int imageID,
  int maxIndices,
  int moduleCount)
  {
   /*==================*/
   /* Construct Header */
   /*==================*/

   fprintf(theFile,"{");

   ConstructHeaderToCode(theFile,&theDefrule->header,imageID,maxIndices,
                                  moduleCount,ModulePrefix(DefruleCodeItem),
                                  ConstructPrefix(DefruleCodeItem));

   /*==========================*/
   /* Flags and Integer Values */
   /*==========================*/

   fprintf(theFile,",%d,%d,%d,%d,%d,%d,%d,%d,",
                   theDefrule->salience,theDefrule->localVarCnt,
                   theDefrule->complexity,theDefrule->afterBreakpoint,
                   theDefrule->watchActivation,theDefrule->watchFiring,
                   theDefrule->autoFocus,theDefrule->executing);

   /*==================*/
   /* Dynamic Salience */
   /*==================*/

#if DYNAMIC_SALIENCE
      ExpressionToCode(theFile,theDefrule->dynamicSalience);
      fprintf(theFile,",");
#endif

   /*=============*/
   /* RHS Actions */
   /*=============*/

   ExpressionToCode(theFile,theDefrule->actions);
   fprintf(theFile,",");

   /*=========================*/
   /* Logical Dependency Join */
   /*=========================*/

#if LOGICAL_DEPENDENCIES
   if (theDefrule->logicalJoin != NULL)
     {
      fprintf(theFile,"&%s%d_%ld[%ld],",JoinPrefix(),
                     imageID,(theDefrule->logicalJoin->bsaveID / maxIndices) + 1,
                             theDefrule->logicalJoin->bsaveID % maxIndices);
     }
   else
     { fprintf(theFile,"NULL,"); }
#endif

   /*===========*/
   /* Last Join */
   /*===========*/

   if (theDefrule->lastJoin != NULL)
     {
      fprintf(theFile,"&%s%d_%ld[%ld],",JoinPrefix(),
                     imageID,(theDefrule->lastJoin->bsaveID / maxIndices) + 1,
                             theDefrule->lastJoin->bsaveID % maxIndices);
     }
   else
     { fprintf(theFile,"NULL,"); }

   /*===============*/
   /* Next Disjunct */
   /*===============*/

   if (theDefrule->disjunct != NULL)
     {
      fprintf(theFile,"&%s%d_%ld[%ld]}",ConstructPrefix(DefruleCodeItem),
                     imageID,(theDefrule->disjunct->header.bsaveID / maxIndices) + 1,
                             theDefrule->disjunct->header.bsaveID % maxIndices);
     }
   else
     { fprintf(theFile,"NULL}"); }
  }

/***************************************************/
/* JoinToCode: Writes the C code representation of */
/*   a single join node to the specified file.     */
/***************************************************/
static void JoinToCode(
  FILE *theFile,
  struct joinNode *theJoin,
  int imageID,
  int maxIndices)
  {
   struct patternParser *theParser;

   /*===========================*/
   /* Mark the join as visited. */
   /*===========================*/

   theJoin->marked = 0;

   /*===========================*/
   /* Flags and Integer Values. */
   /*===========================*/

   fprintf(theFile,"{%d,%d,%d,%d,0,0,%d,%d,0,",
                   theJoin->firstJoin,theJoin->logicalJoin,
                   theJoin->joinFromTheRight,theJoin->patternIsNegated,
                   theJoin->rhsType,theJoin->depth);

   /*==============*/
   /* Beta Memory. */
   /*==============*/

   fprintf(theFile,"NULL,");

   /*====================*/
   /* Network Expression */
   /*====================*/

   PrintHashedExpressionReference(theFile,theJoin->networkTest,imageID,maxIndices);
   fprintf(theFile,",");

   /*============================*/
   /* Right Side Entry Structure */
   /*============================*/

   if (theJoin->rightSideEntryStructure == NULL)
     { fprintf(theFile,"NULL,"); }
   else if (theJoin->joinFromTheRight == FALSE)
     {
      theParser = GetPatternParser((int) theJoin->rhsType);
      if (theParser->codeReferenceFunction == NULL) fprintf(theFile,"NULL,");
      else
        {
         fprintf(theFile,"VS ");
         (*theParser->codeReferenceFunction)(theJoin->rightSideEntryStructure,
                                             theFile,imageID,maxIndices);
         fprintf(theFile,",");
        }
     }
   else
     {
      fprintf(theFile,"&%s%d_%ld[%ld],",JoinPrefix(),
              imageID,(((struct joinNode *) theJoin->rightSideEntryStructure)->bsaveID / maxIndices) + 1,
                      ((struct joinNode *) theJoin->rightSideEntryStructure)->bsaveID % maxIndices);
     }

   /*=================*/
   /* Next Join Level */
   /*=================*/

   if (theJoin->nextLevel == NULL)
     { fprintf(theFile,"NULL,"); }
   else
     {
      fprintf(theFile,"&%s%d_%ld[%ld],",JoinPrefix(),
                    imageID,(theJoin->nextLevel->bsaveID / maxIndices) + 1,
                            theJoin->nextLevel->bsaveID % maxIndices);
     }

   /*=================*/
   /* Last Join Level */
   /*=================*/

   if (theJoin->lastLevel == NULL)
     { fprintf(theFile,"NULL,"); }
   else
     {
      fprintf(theFile,"&%s%d_%ld[%ld],",JoinPrefix(),
                    imageID,(theJoin->lastLevel->bsaveID / maxIndices) + 1,
                            theJoin->lastLevel->bsaveID % maxIndices);
     }

   /*==================*/
   /* Right Drive Node */
   /*==================*/

   if (theJoin->rightDriveNode == NULL)
     { fprintf(theFile,"NULL,"); }
   else
     {
      fprintf(theFile,"&%s%d_%ld[%ld],",JoinPrefix(),
                    imageID,(theJoin->rightDriveNode->bsaveID / maxIndices) + 1,
                            theJoin->rightDriveNode->bsaveID % maxIndices);
     }

   /*==================*/
   /* Right Match Node */
   /*==================*/

   if (theJoin->rightMatchNode == NULL)
     { fprintf(theFile,"NULL,"); }
   else
     {
      fprintf(theFile,"&%s%d_%ld[%ld],",JoinPrefix(),
                    imageID,(theJoin->rightMatchNode->bsaveID / maxIndices) + 1,
                            theJoin->rightMatchNode->bsaveID % maxIndices);
     }

   /*==================*/
   /* Rule to Activate */
   /*==================*/

   if (theJoin->ruleToActivate == NULL)
     { fprintf(theFile,"NULL}"); }
   else
     {
      fprintf(theFile,"&%s%d_%ld[%ld]}",ConstructPrefix(DefruleCodeItem),imageID,
                                    (theJoin->ruleToActivate->header.bsaveID / maxIndices) + 1,
                                    theJoin->ruleToActivate->header.bsaveID % maxIndices);
     }
  }

/*************************************************************/
/* DefruleCModuleReference: Writes the C code representation */
/*   of a reference to a defrule module data structure.      */
/*************************************************************/
globle void DefruleCModuleReference(
  FILE *theFile,
  int count,
  int imageID,
  int maxIndices)
  {
   fprintf(theFile,"MIHS &%s%d_%d[%d]",ModulePrefix(DefruleCodeItem),
                      imageID,
                      (count / maxIndices) + 1,
                      (count % maxIndices));
  }

#endif /* DEFRULE_CONSTRUCT && (! RUN_TIME) && CONSTRUCT_COMPILER */



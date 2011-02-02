   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*           SYMBOL CONSTRUCT COMPILER MODULE          */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the constructs-to-c feature for       */
/*   atomic data values: symbols, integers, floats, and      */
/*   bit maps.                                               */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Barry Cameron                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Added environment parameter to GenClose.       */
/*                                                           */
/*            Corrected code to remove compiler warnings.    */
/*                                                           */
/*************************************************************/

#define _SYMBLCMP_SOURCE_

#include "setup.h"

#if CONSTRUCT_COMPILER && (! RUN_TIME)

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include "envrnmnt.h"
#include "symbol.h"
#include "memalloc.h"
#include "constant.h"
#include "exprnpsr.h"
#include "cstrccom.h"
#include "constrct.h"
#include "argacces.h"
#include "cstrncmp.h"
#include "router.h"
#include "conscomp.h"
#include "sysdep.h"
#include "utility.h"

#include "symblcmp.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static int                         SymbolHashNodesToCode(void *,EXEC_STATUS,char *,char *,char *,int);
   static int                         BitMapHashNodesToCode(void *,EXEC_STATUS,char *,char *,char *,int);
   static int                         BitMapValuesToCode(void *,EXEC_STATUS,char *,char *, char *,int);
   static int                         FloatHashNodesToCode(void *,EXEC_STATUS,char *,char *,char *,int);
   static int                         IntegerHashNodesToCode(void *,EXEC_STATUS,char *,char *,char *,int);
   static int                         HashTablesToCode(void *,EXEC_STATUS,char *,char *,char *);
   static void                        PrintCString(FILE *,char *);

/**************************************************************/
/* AtomicValuesToCode: Driver routine for generating the code */
/*  used by the symbol, integer, float, and bit map tables.   */
/**************************************************************/
globle void AtomicValuesToCode(
  void *theEnv,
  EXEC_STATUS,
  char *fileName,
  char *pathName,
  char *fileNameBuffer)
  {
   int version;

   SetAtomicValueIndices(theEnv,execStatus,TRUE);

   HashTablesToCode(theEnv,execStatus,fileName,pathName,fileNameBuffer);

   version = SymbolHashNodesToCode(theEnv,execStatus,fileName,pathName,fileNameBuffer,5);
   version = FloatHashNodesToCode(theEnv,execStatus,fileName,pathName,fileNameBuffer,version);
   version = IntegerHashNodesToCode(theEnv,execStatus,fileName,pathName,fileNameBuffer,version);
   version = BitMapHashNodesToCode(theEnv,execStatus,fileName,pathName,fileNameBuffer,version);
   BitMapValuesToCode(theEnv,execStatus,fileName,pathName,fileNameBuffer,version);
  }

/*****************************************************/
/* SymbolHashNodesToCode: Produces the code for the  */
/*   symbol hash table entries for a run-time module */
/*   created using the constructs-to-c function.     */
/*****************************************************/
static int SymbolHashNodesToCode(
  void *theEnv,
  EXEC_STATUS,
  char *fileName,
  char *pathName,
  char *fileNameBuffer,
  int version)
  {
   unsigned long i, j;
   struct symbolHashNode *hashPtr;
   int count;
   int numberOfEntries;
   struct symbolHashNode **symbolTable;
   int newHeader = TRUE;
   int arrayVersion = 1;
   FILE *fp;

   /*====================================*/
   /* Count the total number of entries. */
   /*====================================*/

   symbolTable = GetSymbolTable(theEnv,execStatus);
   count = numberOfEntries = 0;

   for (i = 0; i < SYMBOL_HASH_SIZE; i++)
     {
      for (hashPtr = symbolTable[i];
           hashPtr != NULL;
           hashPtr = hashPtr->next)
        { numberOfEntries++; }
     }

   if (numberOfEntries == 0) return(version);

   for (i = 1; i <= (unsigned long) (numberOfEntries / ConstructCompilerData(theEnv,execStatus)->MaxIndices) + 1 ; i++)
     { fprintf(ConstructCompilerData(theEnv,execStatus)->HeaderFP,"extern struct symbolHashNode S%d_%ld[];\n",ConstructCompilerData(theEnv,execStatus)->ImageID,i); }

   /*==================*/
   /* Create the file. */
   /*==================*/

   if ((fp = NewCFile(theEnv,execStatus,fileName,pathName,fileNameBuffer,1,version,FALSE)) == NULL) return(-1);

   /*===================*/
   /* List the entries. */
   /*===================*/

   j = 0;

   for (i = 0; i < SYMBOL_HASH_SIZE; i++)
     {
      for (hashPtr = symbolTable[i];
           hashPtr != NULL;
           hashPtr = hashPtr->next)
        {
         if (newHeader)
           {
            fprintf(fp,"struct symbolHashNode S%d_%d[] = {\n",ConstructCompilerData(theEnv,execStatus)->ImageID,arrayVersion);
            newHeader = FALSE;
           }

         if (hashPtr->next == NULL)
           { fprintf(fp,"{NULL,"); }
         else
           {
            if ((j + 1) >= (unsigned long) ConstructCompilerData(theEnv,execStatus)->MaxIndices)
              { fprintf(fp,"{&S%d_%d[%d],",ConstructCompilerData(theEnv,execStatus)->ImageID,arrayVersion + 1,0); }
            else
              { fprintf(fp,"{&S%d_%d[%ld],",ConstructCompilerData(theEnv,execStatus)->ImageID,arrayVersion,j + 1); }
           }

         fprintf(fp,"%ld,0,1,0,0,%ld,",hashPtr->count + 1,i);
         PrintCString(fp,hashPtr->contents);

         count++;
         j++;

         if ((count == numberOfEntries) || (j >= (unsigned) ConstructCompilerData(theEnv,execStatus)->MaxIndices))
           {
            fprintf(fp,"}};\n");
            GenClose(theEnv,execStatus,fp);
            j = 0;
            arrayVersion++;
            version++;
            if (count < numberOfEntries)
              {
               if ((fp = NewCFile(theEnv,execStatus,fileName,pathName,fileNameBuffer,1,version,FALSE)) == NULL) return(0);
               newHeader = TRUE;
              }
           }
         else
           { fprintf(fp,"},\n"); }
        }
     }

   return(version);
  }

/******************************************************/
/* BitMapHashNodesToCode: Produces the code for the   */
/*   bit map hash table entries for a run-time module */
/*   created using the constructs-to-c function.      */
/******************************************************/
static int BitMapHashNodesToCode(
  void *theEnv,
  EXEC_STATUS,
  char *fileName,
  char *pathName,
  char *fileNameBuffer,
  int version)
  {
   int i, j;
   struct bitMapHashNode *hashPtr;
   int count;
   int numberOfEntries;
   struct bitMapHashNode **bitMapTable;
   int newHeader = TRUE;
   int arrayVersion = 1;
   FILE *fp;
   int longsReqdPartition = 1,longsReqdPartitionCount = 0;

   /*====================================*/
   /* Count the total number of entries. */
   /*====================================*/

   bitMapTable = GetBitMapTable(theEnv,execStatus);
   count = numberOfEntries = 0;

   for (i = 0; i < BITMAP_HASH_SIZE; i++)
     {
      for (hashPtr = bitMapTable[i];
           hashPtr != NULL;
           hashPtr = hashPtr->next)
        { numberOfEntries++; }
     }

   if (numberOfEntries == 0) return(version);

   for (i = 1; i <= (numberOfEntries / ConstructCompilerData(theEnv,execStatus)->MaxIndices) + 1 ; i++)
     { fprintf(ConstructCompilerData(theEnv,execStatus)->HeaderFP,"extern struct bitMapHashNode B%d_%d[];\n",ConstructCompilerData(theEnv,execStatus)->ImageID,i); }

   /*==================*/
   /* Create the file. */
   /*==================*/

   if ((fp = NewCFile(theEnv,execStatus,fileName,pathName,fileNameBuffer,1,version,FALSE)) == NULL) return(-1);

   /*===================*/
   /* List the entries. */
   /*===================*/

   j = 0;

   for (i = 0; i < BITMAP_HASH_SIZE; i++)
     {
      for (hashPtr = bitMapTable[i];
           hashPtr != NULL;
           hashPtr = hashPtr->next)
        {
         if (newHeader)
           {
            fprintf(fp,"struct bitMapHashNode B%d_%d[] = {\n",ConstructCompilerData(theEnv,execStatus)->ImageID,arrayVersion);
            newHeader = FALSE;
           }

         if (hashPtr->next == NULL)
           { fprintf(fp,"{NULL,"); }
         else
           {
            if ((j + 1) >= ConstructCompilerData(theEnv,execStatus)->MaxIndices)
              { fprintf(fp,"{&B%d_%d[%d],",ConstructCompilerData(theEnv,execStatus)->ImageID,arrayVersion + 1,0); }
            else
              { fprintf(fp,"{&B%d_%d[%d],",ConstructCompilerData(theEnv,execStatus)->ImageID,arrayVersion,j + 1); }
           }

         fprintf(fp,"%ld,0,1,0,0,%d,(char *) &L%d_%d[%d],%d",
                     hashPtr->count + 1,i,
                     ConstructCompilerData(theEnv,execStatus)->ImageID,longsReqdPartition,longsReqdPartitionCount,
                     hashPtr->size);

         longsReqdPartitionCount += (int) (hashPtr->size / sizeof(unsigned long));
         if ((hashPtr->size % sizeof(unsigned long)) != 0)
           longsReqdPartitionCount++;
         if (longsReqdPartitionCount >= ConstructCompilerData(theEnv,execStatus)->MaxIndices)
           {
            longsReqdPartitionCount = 0;
            longsReqdPartition++;
           }

         count++;
         j++;

         if ((count == numberOfEntries) || (j >= ConstructCompilerData(theEnv,execStatus)->MaxIndices))
           {
            fprintf(fp,"}};\n");
            GenClose(theEnv,execStatus,fp);
            j = 0;
            arrayVersion++;
            version++;
            if (count < numberOfEntries)
              {
               if ((fp = NewCFile(theEnv,execStatus,fileName,pathName,fileNameBuffer,1,version,FALSE)) == NULL) return(0);
               newHeader = TRUE;
              }
           }
         else
           { fprintf(fp,"},\n"); }
        }
     }

   return(version);
  }

/*****************************************************/
/* BitMapValuesToCode: Produces the code for the bit */
/*   map strings for a run-time module created using */
/*   the constructs-to-c function.                   */
/*****************************************************/
static int BitMapValuesToCode(
  void *theEnv,
  EXEC_STATUS,
  char *fileName,
  char *pathName,
  char *fileNameBuffer,
  int version)
  {
   int i, j, k;
   unsigned l;
   struct bitMapHashNode *hashPtr;
   int count;
   int numberOfEntries;
   struct bitMapHashNode **bitMapTable;
   int newHeader = TRUE;
   int arrayVersion = 1;
   FILE *fp;
   unsigned long tmpLong;
   int longsReqd;

   /*====================================*/
   /* Count the total number of entries. */
   /*====================================*/

   bitMapTable = GetBitMapTable(theEnv,execStatus);
   count = numberOfEntries = 0;

   for (i = 0; i < BITMAP_HASH_SIZE; i++)
     {
      for (hashPtr = bitMapTable[i];
           hashPtr != NULL;
           hashPtr = hashPtr->next)
        {
         numberOfEntries += (int) (hashPtr->size / sizeof(unsigned long));
         if ((hashPtr->size % sizeof(unsigned long)) != 0)
           { numberOfEntries++; }
        }
     }

   if (numberOfEntries == 0) return(version);

   for (i = 1; i <= (numberOfEntries / ConstructCompilerData(theEnv,execStatus)->MaxIndices) + 1 ; i++)
     { fprintf(ConstructCompilerData(theEnv,execStatus)->HeaderFP,"extern unsigned long L%d_%d[];\n",ConstructCompilerData(theEnv,execStatus)->ImageID,i); }

   /*==================*/
   /* Create the file. */
   /*==================*/

   if ((fp = NewCFile(theEnv,execStatus,fileName,pathName,fileNameBuffer,1,version,FALSE)) == NULL) return(-1);

   /*===================*/
   /* List the entries. */
   /*===================*/

   j = 0;

   for (i = 0; i < BITMAP_HASH_SIZE; i++)
     {
      for (hashPtr = bitMapTable[i];
           hashPtr != NULL;
           hashPtr = hashPtr->next)
        {
         if (newHeader)
           {
            fprintf(fp,"unsigned long L%d_%d[] = {\n",ConstructCompilerData(theEnv,execStatus)->ImageID,arrayVersion);
            newHeader = FALSE;
           }

         longsReqd = (int) (hashPtr->size / sizeof(unsigned long));
         if ((hashPtr->size % sizeof(unsigned long)) != 0)
           longsReqd++;

         for (k = 0 ; k < longsReqd ; k++)
           {
            if (k > 0)
              fprintf(fp,",");
            tmpLong = 0L;
            for (l = 0 ;
                 ((l < sizeof(unsigned long)) &&
                 (((k * sizeof(unsigned long)) + l) < (size_t) hashPtr->size)) ;
                 l++)
              ((char *) &tmpLong)[l] = hashPtr->contents[(k * sizeof(unsigned long)) + l];
            fprintf(fp,"0x%lxL",tmpLong);
           }

         count += longsReqd;
         j += longsReqd;

         if ((count == numberOfEntries) || (j >= ConstructCompilerData(theEnv,execStatus)->MaxIndices))
           {
            fprintf(fp,"};\n");
            GenClose(theEnv,execStatus,fp);
            j = 0;
            arrayVersion++;
            version++;
            if (count < numberOfEntries)
              {
               if ((fp = NewCFile(theEnv,execStatus,fileName,pathName,fileNameBuffer,1,version,FALSE)) == NULL) return(0);
               newHeader = TRUE;
              }
           }
         else
           { fprintf(fp,",\n"); }
        }
     }

   return(version);
  }

/****************************************************/
/* FloatHashNodesToCode: Produces the code for the  */
/*   float hash table entries for a run-time module */
/*   created using the constructs-to-c function.    */
/****************************************************/
static int FloatHashNodesToCode(
  void *theEnv,
  EXEC_STATUS,
  char *fileName,
  char *pathName,
  char *fileNameBuffer,
  int version)
  {
   int i, j;
   struct floatHashNode *hashPtr;
   int count;
   int numberOfEntries;
   struct floatHashNode **floatTable;
   int newHeader = TRUE;
   FILE *fp;
   int arrayVersion = 1;

   /*====================================*/
   /* Count the total number of entries. */
   /*====================================*/

   floatTable = GetFloatTable(theEnv,execStatus);
   count = numberOfEntries = 0;

   for (i = 0; i < FLOAT_HASH_SIZE; i++)
     {
      for (hashPtr = floatTable[i];
           hashPtr != NULL;
           hashPtr = hashPtr->next)
        { numberOfEntries++; }
     }

   if (numberOfEntries == 0) return(version);

   for (i = 1; i <= (numberOfEntries / ConstructCompilerData(theEnv,execStatus)->MaxIndices) + 1 ; i++)
     { fprintf(ConstructCompilerData(theEnv,execStatus)->HeaderFP,"extern struct floatHashNode F%d_%d[];\n",ConstructCompilerData(theEnv,execStatus)->ImageID,i); }

   /*==================*/
   /* Create the file. */
   /*==================*/

   if ((fp = NewCFile(theEnv,execStatus,fileName,pathName,fileNameBuffer,1,version,FALSE)) == NULL) return(-1);

   /*===================*/
   /* List the entries. */
   /*===================*/

   j = 0;

   for (i = 0; i < FLOAT_HASH_SIZE; i++)
     {
      for (hashPtr = floatTable[i];
           hashPtr != NULL;
           hashPtr = hashPtr->next)
        {
         if (newHeader)
           {
            fprintf(fp,"struct floatHashNode F%d_%d[] = {\n",ConstructCompilerData(theEnv,execStatus)->ImageID,arrayVersion);
            newHeader = FALSE;
           }

         if (hashPtr->next == NULL)
           { fprintf(fp,"{NULL,"); }
         else
           {
            if ((j + 1) >= ConstructCompilerData(theEnv,execStatus)->MaxIndices)
              { fprintf(fp,"{&F%d_%d[%d],",ConstructCompilerData(theEnv,execStatus)->ImageID,arrayVersion + 1,0); }
            else
              { fprintf(fp,"{&F%d_%d[%d],",ConstructCompilerData(theEnv,execStatus)->ImageID,arrayVersion,j + 1); }
           }

         fprintf(fp,"%ld,0,1,0,0,%d,",hashPtr->count + 1,i);
         fprintf(fp,"%s",FloatToString(theEnv,execStatus,hashPtr->contents));

         count++;
         j++;

         if ((count == numberOfEntries) || (j >= ConstructCompilerData(theEnv,execStatus)->MaxIndices))
           {
            fprintf(fp,"}};\n");
            GenClose(theEnv,execStatus,fp);
            j = 0;
            version++;
            arrayVersion++;
            if (count < numberOfEntries)
              {
               if ((fp = NewCFile(theEnv,execStatus,fileName,pathName,fileNameBuffer,1,version,FALSE)) == NULL) return(0);
               newHeader = TRUE;
              }
           }
         else
           { fprintf(fp,"},\n"); }
        }
     }

   return(version);
  }

/******************************************************/
/* IntegerHashNodesToCode: Produces the code for the  */
/*   integer hash table entries for a run-time module */
/*   created using the constructs-to-c function.      */
/******************************************************/
static int IntegerHashNodesToCode(
  void *theEnv,
  EXEC_STATUS,
  char *fileName,
  char *pathName,
  char *fileNameBuffer,
  int version)
  {
   int i, j;
   struct integerHashNode *hashPtr;
   int count;
   int numberOfEntries;
   struct integerHashNode **integerTable;
   int newHeader = TRUE;
   FILE *fp;
   int arrayVersion = 1;

   /*====================================*/
   /* Count the total number of entries. */
   /*====================================*/

   integerTable = GetIntegerTable(theEnv,execStatus);
   count = numberOfEntries = 0;

   for (i = 0; i < INTEGER_HASH_SIZE; i++)
     {
      for (hashPtr = integerTable[i];
           hashPtr != NULL;
           hashPtr = hashPtr->next)
        { numberOfEntries++; }
     }

   if (numberOfEntries == 0) return(version);

   for (i = 1; i <= (numberOfEntries / ConstructCompilerData(theEnv,execStatus)->MaxIndices) + 1 ; i++)
     { fprintf(ConstructCompilerData(theEnv,execStatus)->HeaderFP,"extern struct integerHashNode I%d_%d[];\n",ConstructCompilerData(theEnv,execStatus)->ImageID,i); }

   /*==================*/
   /* Create the file. */
   /*==================*/

   if ((fp = NewCFile(theEnv,execStatus,fileName,pathName,fileNameBuffer,1,version,FALSE)) == NULL) return(-1);

   /*===================*/
   /* List the entries. */
   /*===================*/

   j = 0;

   for (i = 0; i < INTEGER_HASH_SIZE; i++)
     {
      for (hashPtr = integerTable[i];
           hashPtr != NULL;
           hashPtr = hashPtr->next)
        {
         if (newHeader)
           {
            fprintf(fp,"struct integerHashNode I%d_%d[] = {\n",ConstructCompilerData(theEnv,execStatus)->ImageID,arrayVersion);
            newHeader = FALSE;
           }

         if (hashPtr->next == NULL)
           { fprintf(fp,"{NULL,"); }
         else
           {
            if ((j + 1) >= ConstructCompilerData(theEnv,execStatus)->MaxIndices)
              { fprintf(fp,"{&I%d_%d[%d],",ConstructCompilerData(theEnv,execStatus)->ImageID,arrayVersion + 1,0); }
            else
              { fprintf(fp,"{&I%d_%d[%d],",ConstructCompilerData(theEnv,execStatus)->ImageID,arrayVersion,j + 1); }
           }

         fprintf(fp,"%ld,0,1,0,0,%d,",hashPtr->count + 1,i);
         fprintf(fp,"%lldLL",hashPtr->contents);

         count++;
         j++;

         if ((count == numberOfEntries) || (j >= ConstructCompilerData(theEnv,execStatus)->MaxIndices))
           {
            fprintf(fp,"}};\n");
            GenClose(theEnv,execStatus,fp);
            j = 0;
            version++;
            arrayVersion++;
            if (count < numberOfEntries)
              {
               if ((fp = NewCFile(theEnv,execStatus,fileName,pathName,fileNameBuffer,1,version,FALSE)) == NULL) return(0);
               newHeader = TRUE;
              }
           }
         else
           { fprintf(fp,"},\n"); }
        }
     }

   return(version);
  }

/****************************************************************/
/* HashTablesToCode: Produces the code for the symbol, integer, */
/*   float, and bitmap hash tables for a run-time module        */
/*   created using the constructs-to-c function.                */
/****************************************************************/
static int HashTablesToCode(
  void *theEnv,
  EXEC_STATUS,
  char *fileName,
  char *pathName,
  char *fileNameBuffer)
  {
   unsigned long i;
   FILE *fp;
   struct symbolHashNode **symbolTable;
   struct floatHashNode **floatTable;
   struct integerHashNode **integerTable;
   struct bitMapHashNode **bitMapTable;

   /*======================================*/
   /* Write the code for the symbol table. */
   /*======================================*/

   symbolTable = GetSymbolTable(theEnv,execStatus);

   if ((fp = NewCFile(theEnv,execStatus,fileName,pathName,fileNameBuffer,1,1,FALSE)) == NULL) return(0);

   fprintf(ConstructCompilerData(theEnv,execStatus)->HeaderFP,"extern struct symbolHashNode *sht%d[];\n",ConstructCompilerData(theEnv,execStatus)->ImageID);
   fprintf(fp,"struct symbolHashNode *sht%d[%ld] = {\n",ConstructCompilerData(theEnv,execStatus)->ImageID,SYMBOL_HASH_SIZE);

   for (i = 0; i < SYMBOL_HASH_SIZE; i++)
      {
       PrintSymbolReference(theEnv,execStatus,fp,symbolTable[i]);

       if (i + 1 != SYMBOL_HASH_SIZE) fprintf(fp,",\n");
      }

    fprintf(fp,"};\n");

    GenClose(theEnv,execStatus,fp);

   /*=====================================*/
   /* Write the code for the float table. */
   /*=====================================*/

   floatTable = GetFloatTable(theEnv,execStatus);

   if ((fp = NewCFile(theEnv,execStatus,fileName,pathName,fileNameBuffer,1,2,FALSE)) == NULL) return(0);

   fprintf(ConstructCompilerData(theEnv,execStatus)->HeaderFP,"extern struct floatHashNode *fht%d[];\n",ConstructCompilerData(theEnv,execStatus)->ImageID);
   fprintf(fp,"struct floatHashNode *fht%d[%d] = {\n",ConstructCompilerData(theEnv,execStatus)->ImageID,FLOAT_HASH_SIZE);

   for (i = 0; i < FLOAT_HASH_SIZE; i++)
      {
       if (floatTable[i] == NULL) { fprintf(fp,"NULL"); }
       else PrintFloatReference(theEnv,execStatus,fp,floatTable[i]);

       if (i + 1 != FLOAT_HASH_SIZE) fprintf(fp,",\n");
      }

    fprintf(fp,"};\n");

    GenClose(theEnv,execStatus,fp);

   /*=======================================*/
   /* Write the code for the integer table. */
   /*=======================================*/

   integerTable = GetIntegerTable(theEnv,execStatus);

   if ((fp = NewCFile(theEnv,execStatus,fileName,pathName,fileNameBuffer,1,3,FALSE)) == NULL) return(0);

   fprintf(ConstructCompilerData(theEnv,execStatus)->HeaderFP,"extern struct integerHashNode *iht%d[];\n",ConstructCompilerData(theEnv,execStatus)->ImageID);
   fprintf(fp,"struct integerHashNode *iht%d[%d] = {\n",ConstructCompilerData(theEnv,execStatus)->ImageID,INTEGER_HASH_SIZE);

   for (i = 0; i < INTEGER_HASH_SIZE; i++)
      {
       if (integerTable[i] == NULL) { fprintf(fp,"NULL"); }
       else PrintIntegerReference(theEnv,execStatus,fp,integerTable[i]);

       if (i + 1 != INTEGER_HASH_SIZE) fprintf(fp,",\n");
      }

    fprintf(fp,"};\n");

    GenClose(theEnv,execStatus,fp);

   /*======================================*/
   /* Write the code for the bitmap table. */
   /*======================================*/

   bitMapTable = GetBitMapTable(theEnv,execStatus);

   if ((fp = NewCFile(theEnv,execStatus,fileName,pathName,fileNameBuffer,1,4,FALSE)) == NULL) return(0);

   fprintf(ConstructCompilerData(theEnv,execStatus)->HeaderFP,"extern struct bitMapHashNode *bmht%d[];\n",ConstructCompilerData(theEnv,execStatus)->ImageID);
   fprintf(fp,"struct bitMapHashNode *bmht%d[%d] = {\n",ConstructCompilerData(theEnv,execStatus)->ImageID,BITMAP_HASH_SIZE);

   for (i = 0; i < BITMAP_HASH_SIZE; i++)
      {
       PrintBitMapReference(theEnv,execStatus,fp,bitMapTable[i]);

       if (i + 1 != BITMAP_HASH_SIZE) fprintf(fp,",\n");
      }

    fprintf(fp,"};\n");

    GenClose(theEnv,execStatus,fp);

    return(1);
   }

/*****************************************************/
/* PrintSymbolReference: Prints the C code reference */
/*   address to the specified symbol (also used for  */
/*   strings and instance names).                    */
/*****************************************************/
globle void PrintSymbolReference(
  void *theEnv,
  EXEC_STATUS,
  FILE *theFile,
  struct symbolHashNode *theSymbol)
  {
   if (theSymbol == NULL) fprintf(theFile,"NULL");
   else fprintf(theFile,"&S%d_%d[%d]",
                        ConstructCompilerData(theEnv,execStatus)->ImageID,
                        (int) (theSymbol->bucket / ConstructCompilerData(theEnv,execStatus)->MaxIndices) + 1,
                        (int) theSymbol->bucket % ConstructCompilerData(theEnv,execStatus)->MaxIndices);
  }

/****************************************************/
/* PrintFloatReference: Prints the C code reference */
/*   address to the specified float.                */
/****************************************************/
globle void PrintFloatReference(
  void *theEnv,
  EXEC_STATUS,
  FILE *theFile,
  struct floatHashNode *theFloat)
  {
   fprintf(theFile,"&F%d_%d[%d]",
                   ConstructCompilerData(theEnv,execStatus)->ImageID,
                   (int) (theFloat->bucket / ConstructCompilerData(theEnv,execStatus)->MaxIndices) + 1,
                   (int) theFloat->bucket % ConstructCompilerData(theEnv,execStatus)->MaxIndices);
  }

/******************************************************/
/* PrintIntegerReference: Prints the C code reference */
/*   address to the specified integer.                */
/******************************************************/
globle void PrintIntegerReference(
  void *theEnv,
  EXEC_STATUS,
  FILE *theFile,
  struct integerHashNode *theInteger)
  {
   fprintf(theFile,"&I%d_%d[%d]",
                   ConstructCompilerData(theEnv,execStatus)->ImageID,
                   (int) (theInteger->bucket / ConstructCompilerData(theEnv,execStatus)->MaxIndices) + 1,
                   (int) theInteger->bucket % ConstructCompilerData(theEnv,execStatus)->MaxIndices);
  }

/*****************************************************/
/* PrintBitMapReference: Prints the C code reference */
/*   address to the specified bit map.               */
/*****************************************************/
globle void PrintBitMapReference(
  void *theEnv,
  EXEC_STATUS,
  FILE *theFile,
  struct bitMapHashNode *theBitMap)
  {
   if (theBitMap == NULL) fprintf(theFile,"NULL");
   else fprintf(theFile,"&B%d_%d[%d]",
                        ConstructCompilerData(theEnv,execStatus)->ImageID,
                        (int) (theBitMap->bucket / ConstructCompilerData(theEnv,execStatus)->MaxIndices) + 1,
                        (int) theBitMap->bucket % ConstructCompilerData(theEnv,execStatus)->MaxIndices);
  }

/*********************************************************/
/* PrintCString: Prints KB strings in the appropriate    */
/*   format for C (the " and \ characters are preceeded  */
/*   by a \ and carriage returns are replaced with \n).  */
/*********************************************************/
static void PrintCString(
  FILE *theFile,
  char *str)
  {
   unsigned i;
   size_t slen;

   /*============================================*/
   /* Print the string's opening quotation mark. */
   /*============================================*/

   fprintf(theFile,"\"");

   /*===============================================*/
   /* Convert and write each character to the file. */
   /*===============================================*/

   slen = strlen(str);
   for (i = 0 ; i < slen ; i++)
     {
      /*====================================*/
      /* Preceed " and \ with the \ escape. */
      /*====================================*/

      if ((str[i] == '"') || (str[i] == '\\'))
        {
         fputc('\\',theFile);
         fputc(str[i],theFile);
        }

      /*====================================*/
      /* Replace a carriage return with \n. */
      /*====================================*/

      else if (str[i] == '\n')
        {
         fputc('\\',theFile);
         fputc('n',theFile);
        }

      /*===============================*/
      /* All other characters can be   */
      /* printed without modification. */
      /*===============================*/

      else
        { fputc(str[i],theFile); }
     }

   /*============================================*/
   /* Print the string's closing quotation mark. */
   /*============================================*/

   fprintf(theFile,"\"");
  }

#endif /* CONSTRUCT_COMPILER && (! RUN_TIME) */

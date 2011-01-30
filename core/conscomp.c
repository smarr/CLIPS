   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*              CONSTRUCT COMPILER MODULE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides core routines for the constructs-to-c   */
/*   command.                                                */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Donnell                                     */
/*      Barry Cameron                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _CONSCOMP_SOURCE_

#include "setup.h"

#if CONSTRUCT_COMPILER && (! RUN_TIME)

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <stdlib.h>
#include <string.h>

#include "symbol.h"
#include "memalloc.h"
#include "constant.h"
#include "exprnpsr.h"
#include "cstrccom.h"
#include "constrct.h"
#include "argacces.h"
#include "cstrncmp.h"
#include "router.h"
#include "utility.h"
#include "modulcmp.h"

#if DEFRULE_CONSTRUCT
#include "network.h"
#endif

#if DEFFUNCTION_CONSTRUCT
#include "dffnxcmp.h"
#endif

#if DEFTEMPLATE_CONSTRUCT
#include "tmpltcmp.h"
#endif

#if DEFGLOBAL_CONSTRUCT
#include "globlcmp.h"
#endif

#if DEFGENERIC_CONSTRUCT
#include "genrccmp.h"
#endif

#if OBJECT_SYSTEM
#include "objcmp.h"
#endif

#include "conscomp.h"

/***************/
/* DEFINITIONS */
/***************/

#define FSIZE 80

/**********************************************/
/* CONSTRUCT CODES DEFINITIONS: The codes F,  */
/*   I, B, S, E, P, L, and C are not included */
/*   because those are already taken.         */
/*                                            */
/*   B: BitMap hash nodes                     */
/*   C: Constraint hash nodes                 */
/*   E: Expression hash nodes                 */
/*   F: Float hash nodes                      */
/*   I: Integer hash nodes                    */
/*   L: Bitmaps                               */
/*   P: Functions                             */
/*   S: Symbol hash nodes                     */
/**********************************************/

#define PRIMARY_CODES   "ADGHJKMNOQRTUVWXYZ"
#define PRIMARY_LEN     18
#define SECONDARY_CODES "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
#define SECONDARY_LEN   26

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   void                               ConstructsToCCommand(void);
   static int                         ConstructsToC(char *,int,int);
   static void                        WriteFunctionExternDeclarations(FILE *);
   static int                         FunctionsToCode(char *);
   static int                         WriteInitializationFunction(char *);
   static void                        DumpExpression(struct expr *);
   static void                        MarkConstruct(struct constructHeader *,void *);
   static void                        HashedExpressionsToCode(void);

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle int                       ImageID;
   Thread globle FILE                     *HeaderFP;
   Thread globle int                       MaxIndices = 2000;

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static FILE                     *ExpressionFP;
   Thread static char                     *FilePrefix;
   Thread static BOOLEAN                   ExpressionHeader;
   Thread static long                      ExpressionCount;
   Thread static int                       ExpressionVersion;
   Thread static struct CodeGeneratorItem *ListOfCodeGeneratorItems = NULL;

/**********************************************/
/* ConstructsToCCommand: H/L access routine   */
/*   for the constructs-to-c command.         */
/**********************************************/
globle void ConstructsToCCommand()
  {
   char *fileName;
   DATA_OBJECT theArg;
   int argCount;
   int id, max;
#if VAX_VMS || IBM_MSC || IBM_TBC || IBM_ICB || IBM_ZTC || IBM_SC
   int i;
#endif

   /*============================================*/
   /* Check for appropriate number of arguments. */
   /*============================================*/

   if ((argCount = ArgRangeCheck("constructs-to-c",2,3)) == -1) return;

   /*====================================================*/
   /* Get the name of the file in which to place C code. */
   /*====================================================*/

   if (ArgTypeCheck("constructs-to-c",1,SYMBOL_OR_STRING,&theArg) == FALSE)
     { return; }

   fileName = DOToString(theArg);

   /*================================*/
   /* File names for the VAX and IBM */
   /* PCs can't contain a period.    */
   /*================================*/

#if VAX_VMS || IBM_MSC || IBM_TBC || IBM_ICB || IBM_ZTC || IBM_SC
   for (i = 0 ; *(fileName+i) ; i++)
     {
      if (*(fileName+i) == '.')
        {
         PrintErrorID("CONSCOMP",1,FALSE);
         PrintRouter(WERROR,"Invalid file name ");
         PrintRouter(WERROR,fileName);
         PrintRouter(WERROR," contains \'.\'\n");
         return;
        }
      }
#endif

   /*===========================================*/
   /* If the base file name is greater than 3   */
   /* characters, issue a warning that the file */
   /* name lengths may exceed what is allowed   */
   /* under some operating systems.             */
   /*===========================================*/

   if (((int) strlen(fileName)) > 3)
     {
      PrintWarningID("CONSCOMP",1,FALSE);
      PrintRouter(WWARNING,"Base file name exceeds 3 characters.\n");
      PrintRouter(WWARNING,"  This may cause files to be overwritten if file name length\n");
      PrintRouter(WWARNING,"  is limited on your platform.\n");
     }

   /*====================================*/
   /* Get the runtime image ID argument. */
   /*====================================*/

   if (ArgTypeCheck("constructs-to-c",2,INTEGER,&theArg) == FALSE)
     { return; }

   id = DOToInteger(theArg);
   if (id < 0)
     {
      ExpectedTypeError1("constructs-to-c",2,"positive integer");
      return;
     }

   /*===========================================*/
   /* Get the maximum number of data structures */
   /* to store per file argument (if supplied). */
   /*===========================================*/

   if (argCount == 3)
     {
      if (ArgTypeCheck("constructs-to-c",3,INTEGER,&theArg) == FALSE)
        { return; }

      max = DOToInteger(theArg);

      if (max < 0)
        {
         ExpectedTypeError1("constructs-to-c",3,"positive integer");
         return;
        }
     }
   else
     { max = 10000; }

   /*============================*/
   /* Call the driver routine to */
   /* generate the C code.       */
   /*============================*/

   ConstructsToC(fileName,id,max);
  }

/***************************************/
/* ConstructsToC: C access routine for */
/*   the constructs-to-c command.      */
/***************************************/
static int ConstructsToC(
  char *fileName,
  int theImageID,
  int max)
  {
   char fname[FSIZE];
   int fileVersion;
   struct CodeGeneratorItem *cgPtr;

   /*===============================================*/
   /* Set the global MaxIndices variable indicating */
   /* the maximum number of data structures to save */
   /* in each file.                                 */
   /*===============================================*/

   MaxIndices = max;

   /*==================================*/
   /* Call the list of functions to be */
   /* executed before generating code. */
   /*==================================*/

   for (cgPtr = ListOfCodeGeneratorItems;
        cgPtr != NULL;
        cgPtr = cgPtr->next)
     { if (cgPtr->beforeFunction != NULL) (*cgPtr->beforeFunction)(); }

   /*=================================================*/
   /* Do a periodic cleanup without using heuristics  */
   /* to get rid of as much garbage as possible so    */
   /* that it isn't written out as C data structures. */
   /*=================================================*/

   PeriodicCleanup(FALSE,FALSE);

   /*=====================================*/
   /* Initialize some global information. */
   /*=====================================*/

   FilePrefix = fileName;
   ImageID = theImageID;
   ExpressionFP = NULL;
   ExpressionVersion = 1;
   ExpressionHeader = TRUE;
   ExpressionCount = 0;

   /*=====================================================*/
   /* Open a header file for dumping general information. */
   /*=====================================================*/

   sprintf(fname,"%s.h",fileName);
   if ((HeaderFP = fopen(fname,"w")) == NULL)
     {
      OpenErrorMessage("constructs-to-c",fname);
      return(0);
     }

   fprintf(HeaderFP,"#ifndef _CONSTRUCT_COMPILER_HEADER_\n");
   fprintf(HeaderFP,"#define _CONSTRUCT_COMPILER_HEADER_\n\n");

   fprintf(HeaderFP,"#include <stdio.h>\n");
   fprintf(HeaderFP,"#include \"setup.h\"\n");
   fprintf(HeaderFP,"#include \"expressn.h\"\n");
   fprintf(HeaderFP,"#include \"extnfunc.h\"\n");
   fprintf(HeaderFP,"#include \"%s\"\n",API_HEADER);
   fprintf(HeaderFP,"\n#define VS (void *)\n");
   fprintf(HeaderFP,"\n");

   /*=========================================================*/
   /* Give extern declarations for user and system functions. */
   /*=========================================================*/

   WriteFunctionExternDeclarations(HeaderFP);

   fprintf(HeaderFP,"\n#endif\n\n");
   fprintf(HeaderFP,"/****************************/\n");
   fprintf(HeaderFP,"/* EXTERN ARRAY DEFINITIONS */\n");
   fprintf(HeaderFP,"/****************************/\n\n");

   /*==================================*/
   /* Generate code for atomic values, */
   /* function definitions, hashed     */
   /* expressions, and constructs.     */
   /*==================================*/

   AtomicValuesToCode(fileName);

   FunctionsToCode(fileName);

   HashedExpressionsToCode();

   ConstraintsToCode(fileName,4,HeaderFP,ImageID,MaxIndices);

   /*===============================*/
   /* Call each code generator item */
   /* for the various constructs.   */
   /*===============================*/

   fileVersion = 5;
   for (cgPtr = ListOfCodeGeneratorItems;
        cgPtr != NULL;
        cgPtr = cgPtr->next)
     {
      if (cgPtr->generateFunction != NULL)
        {
         (*cgPtr->generateFunction)(fileName,fileVersion,HeaderFP,ImageID,MaxIndices);
         fileVersion++;
        }
     }

   /*=========================================*/
   /* Restore the atomic data bucket values   */
   /* (which were set to an index reference). */
   /*=========================================*/

   RestoreAtomicValueBuckets();

   /*============================*/
   /* Close the expression file. */
   /*============================*/

   if (ExpressionFP != NULL)
     {
      fprintf(ExpressionFP,"};\n");
      fclose(ExpressionFP);
     }

   /*====================================*/
   /* Write the initialization function. */
   /*====================================*/

   WriteInitializationFunction(fileName);

   /*========================*/
   /* Close the header file. */
   /*========================*/

   fclose(HeaderFP);

   /*==================================================*/
   /* Return TRUE to indicate that the constructs-to-c */
   /* command was successfully executed.               */
   /*==================================================*/

   return(TRUE);
  }

/*******************************************************/
/* WriteFunctionExternDeclarations: Loop through the   */
/*   list of function definitions and generates extern */
/*   declarations for them in the specified file.      */
/*******************************************************/
static void WriteFunctionExternDeclarations(
  FILE *fp)
  {
   struct FunctionDefinition *theFunction;

   fprintf(fp,"\n");
   fprintf(fp,"/************************************/\n");
   fprintf(fp,"/* EXTERNAL FUNCTION DEFINITIONS    */\n");
   fprintf(fp,"/************************************/\n\n");

   for (theFunction = GetFunctionList();
        theFunction != NULL;
        theFunction = theFunction->next)
     {
      fprintf(fp,"extern ");
      switch(theFunction->returnValueType)
        {
         case 'i':
         case 'b':
           fprintf(fp,"int ");
           break;

         case 'l':
           fprintf(fp,"long ");
           break;

         case 'f':
           fprintf(fp,"float ");
           break;

         case 'd':
           fprintf(fp,"double ");
           break;

         case 'w':
         case 's':
         case 'o':
           fprintf(fp,"SYMBOL_HN *");
           break;

         case 'c':
           fprintf(fp,"char ");
           break;

         case 'a':
         case 'x':
           fprintf(fp,"void * ");
           break;

         case 'v':
         case 'm':
         case 'u':
         case 'n':
         case 'j':
         case 'k':
           fprintf(fp,"void ");
           break;

         default:
           SystemError("CONSCOMP",1);
           break;
        }

      fprintf(fp,"%s(",theFunction->actualFunctionName);
      switch(theFunction->returnValueType)
        {
         case 'i':
         case 'b':
         case 'l':
         case 'f':
         case 'd':
         case 'w':
         case 's':
         case 'o':
         case 'c':
         case 'a':
         case 'x':
         case 'v':
           fprintf(fp,"void");
           break;

         case 'm':
         case 'u':
         case 'n':
         case 'j':
         case 'k':
           fprintf(fp,"DATA_OBJECT_PTR_ARG");
           break;
        }

      fprintf(fp,");\n");
     }
  }

/****************************************************/
/* FunctionsToCode: Generates C code to represent   */
/*   the function declaration data structures (used */
/*   to declare system and user defined functions). */
/****************************************************/
static int FunctionsToCode(
  char *fileName)
  {
   short i = 0;
   FILE *fp;
   int version = 1;
   int newHeader = TRUE;
   struct FunctionDefinition *fctnPtr;

   /*=============================*/
   /* Assign a reference index to */
   /* each of the functions.      */
   /*=============================*/

   for (fctnPtr = GetFunctionList();
        fctnPtr != NULL;
        fctnPtr = fctnPtr->next)
     { fctnPtr->bsaveIndex = i++; }

   /*=======================================*/
   /* Create the file in which to store the */
   /* function definition data structures.  */
   /*=======================================*/

   if ((fp = NewCFile(fileName,2,version,FALSE)) == NULL)
     { return(0); }

   /*===============================================*/
   /* Construct the definition of the function list */
   /* from the definitions of the functions.        */
   /*===============================================*/

   fprintf(fp,"\n\n");
   fprintf(fp,"/************************************/\n");
   fprintf(fp,"/* FUNCTION LIST DEFINITION         */\n");
   fprintf(fp,"/************************************/\n\n");

   i = 1;
   fctnPtr = GetFunctionList();
   while (fctnPtr != NULL)
     {
      if (newHeader)
        {
         fprintf(fp,"struct FunctionDefinition P%d_%d[] = {\n",ImageID,version);
         fprintf(HeaderFP,"extern struct FunctionDefinition P%d_%d[];\n",ImageID,version);
         newHeader = FALSE;
        }

      fprintf(fp,"{");
      PrintSymbolReference(fp,fctnPtr->callFunctionName);
      fprintf(fp,",\"%s\",",fctnPtr->actualFunctionName);
      fprintf(fp,"'%c',",fctnPtr->returnValueType);
      fprintf(fp,"PTIF %s,",fctnPtr->actualFunctionName);
      fprintf(fp,"NULL,");
      if (fctnPtr->restrictions != NULL) fprintf(fp,"\"%s\",",fctnPtr->restrictions);
      else fprintf(fp,"NULL,");
      fprintf(fp,"0,0,0,");

      PrintFunctionReference(fp,fctnPtr->next);

      i++;
      fctnPtr = fctnPtr->next;
      if ((i > MaxIndices) || (fctnPtr == NULL))
        {
         fprintf(fp,"}};\n");
         fclose(fp);
         i = 1;
         version++;
         if (fctnPtr != NULL)
           {
            if ((fp = NewCFile(fileName,2,version,FALSE)) == NULL) return(0);
            newHeader = TRUE;
           }
        }
      else
        { fprintf(fp,"},\n"); }
     }

   return(TRUE);
  }

/************************************************************/
/* PrintFunctionReference: Writes the C code representation */
/*   of a pointer to a function definition data structure.  */
/************************************************************/
globle void PrintFunctionReference(
  FILE *fp,
  struct FunctionDefinition *funcPtr)
  {
   if (funcPtr == NULL) fprintf(fp,"NULL");
   else
      fprintf(fp,"&P%d_%d[%d]",ImageID,
                                  (funcPtr->bsaveIndex / MaxIndices) + 1,
                                   funcPtr->bsaveIndex % MaxIndices);
  }

/******************************************/
/* WriteInitializationFunction: Generates */
/*   the C initialization function for    */
/*   this constructs-to-c module.         */
/******************************************/
static int WriteInitializationFunction(
  char *fileName)
  {
   char fname[FSIZE];
   FILE *fp;
   struct CodeGeneratorItem *cgPtr;

   /*===============================*/
   /* Open the initialization file. */
   /*===============================*/

   sprintf(fname,"%s.c",fileName);
   if ((fp = fopen(fname,"w")) == NULL)
     {
      OpenErrorMessage("constructs-to-c",fname);
      return(FALSE);
     }

   /*=====================================*/
   /* Write out #includes and prototypes. */
   /*=====================================*/

   fprintf(fp,"#include \"%s.h\"\n",fileName);
   fprintf(fp,"\n");
   fprintf(fp,"#include \"utility.h\"\n");
   fprintf(fp,"#include \"generate.h\"\n");
   fprintf(fp,"#include \"expressn.h\"\n");
   fprintf(fp,"#include \"extnfunc.h\"\n");
   fprintf(fp,"#include \"objrtmch.h\"\n");
   fprintf(fp,"#include \"rulebld.h\"\n\n");

   fprintf(HeaderFP,"   void InitCImage_%d(void);\n",ImageID);

   /*============================================*/
   /* Begin writing the initialization function. */
   /*============================================*/

   fprintf(fp,"\n");
   fprintf(fp,"/*******************************************/\n");
   fprintf(fp,"/* CONSTRUCT IMAGE INITIALIZATION FUNCTION */\n");
   fprintf(fp,"/*******************************************/\n");

   fprintf(fp,"\nVOID InitCImage_%d()\n",ImageID);
   fprintf(fp,"  {\n");

   fprintf(fp,"   Clear();\n");
   fprintf(fp,"   PeriodicCleanup(TRUE,FALSE);\n");
   fprintf(fp,"   SetSymbolTable(sht%d);\n",ImageID);
   fprintf(fp,"   SetFloatTable(fht%d);\n",ImageID);
   fprintf(fp,"   SetIntegerTable(iht%d);\n",ImageID);
   fprintf(fp,"   SetBitMapTable(bmht%d);\n",ImageID);
   fprintf(fp,"   RefreshSpecialSymbols();\n");
   fprintf(fp,"   InstallFunctionList(P%d_1);\n\n",ImageID);
   fprintf(fp,"   InitExpressionPointers();\n\n");

   /*==========================================*/
   /* Write construct specific initialization. */
   /*==========================================*/

   cgPtr = ListOfCodeGeneratorItems;
   while (cgPtr != NULL)
     {
      if (cgPtr->initFunction != NULL)
        {
         (*cgPtr->initFunction)(fp,ImageID,MaxIndices);
         fprintf(fp,"\n");
        }
      cgPtr = cgPtr->next;
     }

   /*================================*/
   /* Close the initialization file. */
   /*================================*/

   fprintf(fp,"  }\n");

   fclose(fp);

   /*========================================*/
   /* Return TRUE to indicate initialization */
   /* file was successfully written.         */
   /*========================================*/

   return(TRUE);
  }


/**************************************************/
/* NewCFile: Opens a new file for writing C code. */
/**************************************************/
globle FILE *NewCFile(
  char *fileName,
  int id,
  int version,
  int reopenOldFile)
  {
   char fname[FSIZE];
   FILE *newFP;

   sprintf(fname,"%s%d_%d.c",fileName,id,version);

   newFP = fopen(fname,reopenOldFile ? "a" : "w");

   if (newFP == NULL)
     {
      OpenErrorMessage("constructs-to-c",fname);
      return(NULL);
     }

   if (reopenOldFile == FALSE)
     {
      fprintf(newFP,"#include \"%s.h\"\n",fileName);
      fprintf(newFP,"\n");
     }

   return(newFP);
  }

/**********************************************************/
/* HashedExpressionsToCode: Traverses the expression hash */
/*   table and calls ExpressionToCode to write the C      */
/*   code representation to a file of every expression in */
/*   the table.                                           */
/**********************************************************/
static void HashedExpressionsToCode()
  {
   unsigned i;
   EXPRESSION_HN *exphash;

   for (i = 0; i < EXPRESSION_HASH_SIZE; i++)
     {
      for (exphash = ExpressionHashTable[i];
           exphash != NULL;
           exphash = exphash->nxt)
        {
         exphash->bsaveID = ExpressionCount + (MaxIndices * ExpressionVersion);
         ExpressionToCode(NULL,exphash->exp);
        }
     }
  }

/*****************************************************/
/* PrintHashedExpressionReference: Writes the C code */
/*   representation of a pointer to an expression    */
/*   stored in the expression hash table.            */
/*****************************************************/
globle void PrintHashedExpressionReference(
  FILE *theFile,
  struct expr *theExpression,
  int imageID,
  int maxIndices)
  {
   long theIDValue;

   if (theExpression == NULL)
     { fprintf(theFile,"NULL"); }
   else
     {
      theIDValue = HashedExpressionIndex(theExpression);

      fprintf(theFile,"&E%d_%ld[%ld]",
                      imageID,
                      theIDValue / maxIndices,
                      theIDValue % maxIndices);
     }
  }

/**************************************************************/
/* ExpressionToCode: Writes the C code reference of a pointer */
/*   to an expression and then calls DumpExpression to write  */
/*   the C code for the expression to the expression file.    */
/**************************************************************/
globle int ExpressionToCode(
  FILE *fp,
  struct expr *exprPtr)
  {
   /*========================================*/
   /* Print the reference to the expression. */
   /*========================================*/

   if (exprPtr == NULL)
     {
      if (fp != NULL) fprintf(fp,"NULL");
      return(FALSE);
     }
   else if (fp != NULL)
     { fprintf(fp,"&E%d_%d[%ld]",ImageID,ExpressionVersion,ExpressionCount); }

   /*==================================================*/
   /* Create a new expression code file, if necessary. */
   /*==================================================*/

   if (ExpressionHeader == TRUE)
     {
      if ((ExpressionFP = NewCFile(FilePrefix,3,ExpressionVersion,FALSE)) == NULL)
        { return(-1); }

      fprintf(ExpressionFP,"struct expr E%d_%d[] = {\n",ImageID,ExpressionVersion);
      fprintf(HeaderFP,"extern struct expr E%d_%d[];\n",ImageID,ExpressionVersion);
      ExpressionHeader = FALSE;
     }
   else
     { fprintf(ExpressionFP,",\n"); }

   /*===========================*/
   /* Dump the expression code. */
   /*===========================*/

   DumpExpression(exprPtr);

   /*=========================================*/
   /* Close the expression file if necessary. */
   /*=========================================*/

   if (ExpressionCount >= MaxIndices)
     {
      ExpressionCount = 0;
      ExpressionVersion++;
      fprintf(ExpressionFP,"};\n");
      fclose(ExpressionFP);
      ExpressionFP = NULL;
      ExpressionHeader = TRUE;
     }

   /*==========================================*/
   /* Return TRUE to indicate the expression   */
   /* reference and expression data structures */
   /* were succcessfully written to the file.  */
   /*==========================================*/

   return(TRUE);
  }

/**********************************************************/
/* DumpExpression: Writes the C code representation of an */
/*   expression data structure to the expression file.    */
/**********************************************************/
static void DumpExpression(
  struct expr *exprPtr)
  {

   while (exprPtr != NULL)
     {
      fprintf(ExpressionFP,"{");
      fprintf(ExpressionFP,"%d,",exprPtr->type);
      fprintf(ExpressionFP,"VS ");
      switch (exprPtr->type)
        {
         case FCALL:
           PrintFunctionReference(ExpressionFP,(struct FunctionDefinition *) exprPtr->value);
           break;

         case INTEGER:
           PrintIntegerReference(ExpressionFP,(INTEGER_HN *) exprPtr->value);
           break;

         case FLOAT:
           PrintFloatReference(ExpressionFP,(FLOAT_HN *) exprPtr->value);
           break;

         case PCALL:
#if DEFFUNCTION_CONSTRUCT
           PrintDeffunctionReference(ExpressionFP,(DEFFUNCTION *) exprPtr->value,
                                     ImageID,MaxIndices);
#else
           fprintf(ExpressionFP,"NULL");
#endif
           break;

         case GCALL:
#if DEFGENERIC_CONSTRUCT
           PrintGenericFunctionReference(ExpressionFP,(DEFGENERIC *) exprPtr->value,
                                         ImageID,MaxIndices);
#else
           fprintf(ExpressionFP,"NULL");
#endif
           break;

         case DEFTEMPLATE_PTR:
#if DEFTEMPLATE_CONSTRUCT
           DeftemplateCConstructReference(ExpressionFP,exprPtr->value,ImageID,MaxIndices);
#else
           fprintf(ExpressionFP,"NULL");
#endif
           break;

         case DEFGLOBAL_PTR:
#if DEFGLOBAL_CONSTRUCT
           DefglobalCConstructReference(ExpressionFP,exprPtr->value,ImageID,MaxIndices);
#else
           fprintf(ExpressionFP,"NULL");
#endif
           break;

         case DEFCLASS_PTR:
#if OBJECT_SYSTEM
           PrintClassReference(ExpressionFP,(DEFCLASS *) exprPtr->value,ImageID,MaxIndices);
#else
           fprintf(ExpressionFP,"NULL");
#endif
           break;

          case FACT_ADDRESS:
#if DEFTEMPLATE_CONSTRUCT
           fprintf(ExpressionFP,"&DummyFact");
#else
           fprintf(ExpressionFP,"NULL");
#endif
           break;

         case INSTANCE_ADDRESS:
#if OBJECT_SYSTEM
           fprintf(ExpressionFP,"&DummyInstance");
#else
           fprintf(ExpressionFP,"NULL");
#endif
           break;

         case STRING:
         case SYMBOL:
         case INSTANCE_NAME:
         case GBL_VARIABLE:
           PrintSymbolReference(ExpressionFP,(SYMBOL_HN *) exprPtr->value);
           break;

         case RVOID:
           fprintf(ExpressionFP,"NULL");
           break;

         default:
           if (PrimitivesArray[exprPtr->type] == NULL)
             { fprintf(ExpressionFP,"NULL"); }
           else if (PrimitivesArray[exprPtr->type]->bitMap)
             { PrintBitMapReference(ExpressionFP,(BITMAP_HN *) exprPtr->value); }
           else
             { fprintf(ExpressionFP,"NULL"); }
           break;
        }

      fprintf(ExpressionFP,",");

      ExpressionCount++;
      if (exprPtr->argList == NULL)
        { fprintf(ExpressionFP,"NULL,"); }
      else
        {
         fprintf(ExpressionFP,"&E%d_%d[%ld],",ImageID,ExpressionVersion,
                                                       ExpressionCount);
        }

      if (exprPtr->nextArg == NULL)
        { fprintf(ExpressionFP,"NULL}"); }
      else
        {
         fprintf(ExpressionFP,"&E%d_%d[%ld]}",ImageID,ExpressionVersion,
                              ExpressionCount + ExpressionSize(exprPtr->argList));
        }

      if (exprPtr->argList != NULL)
        {
         fprintf(ExpressionFP,",\n");
         DumpExpression(exprPtr->argList);
        }

      exprPtr = exprPtr->nextArg;
      if (exprPtr != NULL) fprintf(ExpressionFP,",\n");
     }
  }

/***********************************************/
/* ConstructsToCCommandDefinition: Initializes */
/*   the constructs-to-c command.              */
/***********************************************/
globle void ConstructsToCCommandDefinition()
  {
   DefineFunction2("constructs-to-c",'v',
                   PTIF ConstructsToCCommand,
                   "ConstructsToCCommand", "23*kii");
  }

/*********************************************************/
/* AddCodeGeneratorItem: Adds another code generator     */
/*   item to the list of items for which code is         */
/*   generated bythe constructs-to-c function. Typically */
/*   each construct has its own code generator item.     */
/*********************************************************/
globle struct CodeGeneratorItem *AddCodeGeneratorItem(
  char *name,
  int priority,
  void (*beforeFunction)(void),
  void (*initFunction)(FILE *,int,int),
  int (*generateFunction)(char *,int,FILE *,int,int),
  int arrayCount)
  {
   struct CodeGeneratorItem *newPtr, *currentPtr, *lastPtr = NULL;
   Thread static int theCount = 0;
   register int i;
   char theBuffer[3];

   /*======================================*/
   /* Create the code generator item data  */
   /* structure and initialize its values. */
   /*======================================*/

   newPtr = get_struct(CodeGeneratorItem);

   newPtr->name = name;
   newPtr->beforeFunction = beforeFunction;
   newPtr->initFunction = initFunction;
   newPtr->generateFunction = generateFunction;
   newPtr->priority = priority;

   /*================================================*/
   /* Create the primary and secondary codes used to */
   /* provide names for the C data structure arrays. */
   /* (The maximum number of arrays is currently     */
   /* limited to 47.                                 */
   /*================================================*/

   if (arrayCount != 0)
     {
      if ((arrayCount + theCount) > (PRIMARY_LEN + SECONDARY_LEN))
        {
         SystemError("CONSCOMP",2);
         ExitRouter(EXIT_FAILURE);
        }

      newPtr->arrayNames = (char **) gm2((int) (sizeof(char *) * arrayCount));

      for (i = 0 ; i < arrayCount ; i++)
        {
         if (theCount < PRIMARY_LEN)
           { sprintf(theBuffer,"%c",PRIMARY_CODES[theCount]); }
         else
           { sprintf(theBuffer,"%c_",SECONDARY_CODES[theCount - PRIMARY_LEN]); }
         theCount++;
         newPtr->arrayNames[i] = (char *) gm2((int) (strlen(theBuffer) + 1));
         strcpy(newPtr->arrayNames[i],theBuffer);
        }
     }
   else
     { newPtr->arrayNames = NULL; }

   /*===========================================*/
   /* Add the new item in the appropriate place */
   /* in the code generator item list.          */
   /*===========================================*/

   if (ListOfCodeGeneratorItems == NULL)
     {
      newPtr->next = NULL;
      ListOfCodeGeneratorItems = newPtr;
      return(newPtr);
     }

   currentPtr = ListOfCodeGeneratorItems;
   while ((currentPtr != NULL) ? (priority < currentPtr->priority) : FALSE)
     {
      lastPtr = currentPtr;
      currentPtr = currentPtr->next;
     }

   if (lastPtr == NULL)
     {
      newPtr->next = ListOfCodeGeneratorItems;
      ListOfCodeGeneratorItems = newPtr;
     }
   else
     {
      newPtr->next = currentPtr;
      lastPtr->next = newPtr;
     }

   /*=========================*/
   /* Return a pointer to the */
   /* code generator item.    */
   /*=========================*/

   return(newPtr);
  }

/************************************************************/
/* CloseFileIfNeeded: Determines if a C file to which data  */
/*   structures have been written should be closed. The     */
/*   file is closed either when all data structures of      */
/*   that specific type are written to files or the maximum */
/*   number of array entries for a single file has been     */
/*   exceeded.                                              */
/************************************************************/
globle FILE *CloseFileIfNeeded(
  FILE *theFile,
  int *theCount,
  int *arrayVersion,
  int maxIndices,
  int *canBeReopened,
  struct CodeGeneratorFile *codeFile)
  {
   /*==========================================*/
   /* If the maximum number of entries for the */
   /* file hasn't been exceeded, then...       */
   /*==========================================*/

   if (*theCount < maxIndices)
     {
      /*====================================*/
      /* If the file can be reopened later, */
      /* close it. Otherwise, keep it open. */
      /*====================================*/

      if (canBeReopened != NULL)
        {
         *canBeReopened = TRUE;
         fclose(theFile);
         return(NULL);
        }

      return(theFile);
     }

   /*===========================================*/
   /* Otherwise, the number of entries allowed  */
   /* in a file has been reached. Indicate that */
   /* the file can't be reopened.               */
   /*===========================================*/

   if (canBeReopened != NULL)
     { *canBeReopened = FALSE; }

   /*===============================================*/
   /* If the file is closed, then we need to reopen */
   /* it to print the final closing right brace.    */
   /*===============================================*/

   if (theFile == NULL)
     {
      if ((canBeReopened == NULL) || (codeFile == NULL))
        {
         SystemError("CONSCOMP",3);
         ExitRouter(EXIT_FAILURE);
        }

      if (codeFile->filePrefix == NULL)
        { return(NULL); }

      theFile = NewCFile(codeFile->filePrefix,codeFile->id,codeFile->version,TRUE);
      if (theFile == NULL)
        {
         SystemError("CONSCOMP",4);
         ExitRouter(EXIT_FAILURE);
        }
     }

   /*================================*/
   /* Print the final closing brace. */
   /*================================*/

   fprintf(theFile,"};\n");
   fclose(theFile);

   /*============================================*/
   /* Update index values for subsequent writing */
   /* of data structures to files.               */
   /*============================================*/

   *theCount = 0;
   (*arrayVersion)++;

   /*=========================*/
   /* Return NULL to indicate */
   /* the file is closed.     */
   /*=========================*/

   return(NULL);
  }

/**************************************************************/
/* OpenFileIfNeeded: Determines if a C file to which data  */
/*   structures have been written should be closed. The     */
/*   file is closed either when all data structures of      */
/*   that specific type are written to files or the maximum */
/*   number of array entries for a single file has been     */
/*   exceeded.                                              */
/******************************************************************/
globle FILE *OpenFileIfNeeded(
  FILE *theFile,
  char *fileName,
  int fileID,
  int imageID,
  int *fileCount,
  int arrayVersion,
  FILE *headerFP,
  char *structureName,
  char *structPrefix,
  int reopenOldFile,
  struct CodeGeneratorFile *codeFile)
  {
   char arrayName[80];
   char *newName;
   int newID, newVersion;

   /*===========================================*/
   /* If a file is being reopened, use the same */
   /* version number, name, and ID as before.   */
   /*===========================================*/

   if (reopenOldFile)
     {
      if (codeFile == NULL)
        {
         SystemError("CONSCOMP",5);
         ExitRouter(EXIT_FAILURE);
        }

      newName = codeFile->filePrefix;
      newID = codeFile->id;
      newVersion = codeFile->version;
     }

   /*=====================================================*/
   /* Otherwise, use the specified version number, name,  */
   /* and ID. If the appropriate argument is supplied,    */
   /* remember these values for later reopening the file. */
   /*=====================================================*/

   else
     {
      newName = fileName;
      newVersion = *fileCount;
      newID = fileID;

      if (codeFile != NULL)
        {
         codeFile->version = newVersion;
         codeFile->filePrefix = newName;
         codeFile->id = newID;
        }
     }

   /*=========================================*/
   /* If the file is already open, return it. */
   /*=========================================*/

   if (theFile != NULL)
     {
      fprintf(theFile,",\n");
      return(theFile);
     }

   /*================*/
   /* Open the file. */
   /*================*/

   if ((theFile = NewCFile(newName,newID,newVersion,reopenOldFile)) == NULL)
     { return(NULL); }

   /*=========================================*/
   /* If this is the first time the file has  */
   /* been opened, write out the beginning of */
   /* the array variable definition.          */
   /*=========================================*/

   if (reopenOldFile == FALSE)
     {
      (*fileCount)++;
      sprintf(arrayName,"%s%d_%d",structPrefix,imageID,arrayVersion);

#if SHORT_LINK_NAMES
      if (strlen(arrayName) > 6)
        {
         PrintWarningID("CONSCOMP",2,FALSE);
         PrintRouter(WWARNING,"Array name ");
         PrintRouter(WWARNING,arrayName);
         PrintRouter(WWARNING,"exceeds 6 characters in length.\n");
         PrintRouter(WWARNING,"   This variable may be indistinguishable from another by the linker.\n");
        }
#endif
      fprintf(theFile,"%s %s[] = {\n",structureName,arrayName);
      fprintf(headerFP,"extern %s %s[];\n",structureName,arrayName);
     }
   else
     { fprintf(theFile,",\n"); }

   /*==================*/
   /* Return the file. */
   /*==================*/

   return(theFile);
  }

/*************************************************/
/* MarkConstructBsaveIDs: Mark all occurences of */
/*  a specific construct with a unique ID.       */
/*************************************************/
globle void MarkConstructBsaveIDs(
  int constructModuleIndex)
  {
   long theCount = 0;
   DoForAllConstructs(MarkConstruct,constructModuleIndex,FALSE,&theCount);
  }

/*************************************************************/
/* MarkConstruct: Sets the bsaveID for a specific construct. */
/*  Used with the MarkConstructBsaveIDs function to mark all */
/*  occurences of a specific construct with a unique ID.     */
/*************************************************************/
static void MarkConstruct(
  struct constructHeader *theConstruct,
  void *vTheBuffer)
  {
   long *count = (long *) vTheBuffer;

   theConstruct->bsaveID = (*count)++;
  }

/***********************************************************/
/* ConstructHeaderToCode: Writes the C code representation */
/*   of a single construct header to the specified file.   */
/***********************************************************/
globle void ConstructHeaderToCode(
  FILE *theFile,
  struct constructHeader *theConstruct,
  int imageID,
  int maxIndices,
  int moduleCount,
  char *constructModulePrefix,
  char *constructPrefix)
  {
   /*================*/
   /* Construct Name */
   /*================*/

   fprintf(theFile,"{");

   PrintSymbolReference(theFile,theConstruct->name);

   /*===================*/
   /* Pretty Print Form */
   /*===================*/

   fprintf(theFile,",NULL,");

   /*====================*/
   /* Construct Module */
   /*====================*/

   fprintf(theFile,"MIHS &%s%d_%d[%d],",
                   constructModulePrefix,
                   imageID,
                   (moduleCount / maxIndices) + 1,
                   moduleCount % maxIndices);

   /*==========*/
   /* Bsave ID */
   /*==========*/

   fprintf(theFile,"0,");

   /*================*/
   /* Next Construct */
   /*================*/

   if (theConstruct->next == NULL)
     { fprintf(theFile,"NULL}"); }
   else
     {
      fprintf(theFile,"CHS &%s%d_%ld[%ld]}",
                      constructPrefix,
                      imageID,
                      (theConstruct->next->bsaveID / maxIndices) + 1,
                      theConstruct->next->bsaveID % maxIndices);
     }
  }

/***********************************************************/
/* ConstructModuleToCode: Writes the C code representation */
/*   of a single construct module to the specified file.   */
/***********************************************************/
globle void ConstructModuleToCode(
  FILE *theFile,
  struct defmodule *theModule,
  int imageID,
  int maxIndices,
  int constructIndex,
  char *constructPrefix)
  {
   struct defmoduleItemHeader *theModuleItem;

   /*======================*/
   /* Associated Defmodule */
   /*======================*/

   fprintf(theFile,"{");

   theModuleItem = (struct defmoduleItemHeader *)
                   GetModuleItem(theModule,constructIndex);

   PrintDefmoduleReference(theFile,theModule);

   fprintf(theFile,",");

   /*=============================*/
   /* First Construct Module Item */
   /*=============================*/

   if (theModuleItem->firstItem == NULL) fprintf(theFile,"NULL,");
   else fprintf(theFile,"CHS &%s%d_%ld[%ld],",
                        constructPrefix,
                        imageID,
                        (long) (theModuleItem->firstItem->bsaveID / maxIndices) + 1,
                        (long) theModuleItem->firstItem->bsaveID % maxIndices);

   /*============================*/
   /* Last Construct Module Item */
   /*============================*/

   if (theModuleItem->lastItem == NULL) fprintf(theFile,"NULL");
   else fprintf(theFile,"CHS &%s%d_%ld[%ld]",
                        constructPrefix,
                        imageID,
                        (long) (theModuleItem->lastItem->bsaveID / maxIndices) + 1,
                        (long) theModuleItem->lastItem->bsaveID % maxIndices);

   fprintf(theFile,"}");
  }

#else /* CONSTRUCT_COMPILER && (! RUN_TIME) */

   void                               ConstructsToCCommand(void);

/************************************/
/* ConstructsToCCommand: Definition */
/*   for rule compiler stub.        */
/************************************/
void ConstructsToCCommand() {}

#endif /* CONSTRUCT_COMPILER && (! RUN_TIME) */

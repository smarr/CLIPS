   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*           CONSTRUCT COMPILER HEADER FILE            */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_conscomp
#define _H_conscomp

#define ArbitraryPrefix(codeItem,i)    (codeItem)->arrayNames[(i)]

#define ModulePrefix(codeItem)         (codeItem)->arrayNames[0]
#define ConstructPrefix(codeItem)      (codeItem)->arrayNames[1]

#ifndef _H_extnfunc
#include "extnfunc.h"
#endif
#ifndef _H_symblcmp
#include "symblcmp.h"
#endif
#ifndef _H_moduldef
#include "moduldef.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifndef _STDIO_INCLUDED_
#define _STDIO_INCLUDED_
#include <stdio.h>
#endif

struct CodeGeneratorItem
  {
   char *name;
   void (*beforeFunction)(void);
   void (*initFunction)(FILE *,int,int);
   int (*generateFunction)(char *,int,FILE *,int,int);
   int priority;
   char **arrayNames;
   struct CodeGeneratorItem *next;
  };

struct CodeGeneratorFile
 {
  char *filePrefix;
  int id,version;
 };

#ifdef _CONSCOMP_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                      ConstructsToCCommandDefinition(void);
   LOCALE FILE                     *NewCFile(char *,int,int,int);
   LOCALE int                       ExpressionToCode(FILE *,struct expr *);
   LOCALE void                      PrintFunctionReference(FILE *,struct FunctionDefinition *);
   LOCALE struct CodeGeneratorItem *AddCodeGeneratorItem(char *,int,void (*)(void),
                                                         void (*)(FILE *,int,int),
                                                         int (*)(char *,int,FILE *,int,int),int);
   LOCALE FILE                     *CloseFileIfNeeded(FILE *,int *,int *,int,int *,struct CodeGeneratorFile *);
   LOCALE FILE                     *OpenFileIfNeeded(FILE *,char *,int,int,int *,int,FILE *,
                                                     char *,char *,int,struct CodeGeneratorFile *);
   LOCALE void                      MarkConstructBsaveIDs(int);
   LOCALE void                      ConstructHeaderToCode(FILE *,struct constructHeader *,int,int,
                                                         int,char *,char *);
   LOCALE void                      ConstructModuleToCode(FILE *,struct defmodule *,int,int,
                                                         int,char *);
   LOCALE void                      PrintHashedExpressionReference(FILE *,struct expr *,int,int);

#ifndef _SYMBLBIN_SOURCE_
   extern Thread globle int                       ImageID;
   extern Thread globle FILE                     *HeaderFP;
   extern Thread globle int                       MaxIndices;
#endif

#endif





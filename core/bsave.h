   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.24  06/05/06          */
   /*                                                     */
   /*                 BSAVE HEADER FILE                   */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#ifndef _H_bsave
#define _H_bsave

struct BinaryItem;

#ifndef _STDIO_INCLUDED_
#define _STDIO_INCLUDED_
#include <stdio.h>
#endif

#ifndef _H_expressn
#include "expressn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _BSAVE_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

struct BinaryItem
  {
   char *name;
   void (*findFunction)(void *);
   void (*bloadStorageFunction)(void *);
   void (*bloadFunction)(void *);
   void (*clearFunction)(void *);
   void (*expressionFunction)(void *,EXEC_STATUS,FILE *);
   void (*bsaveStorageFunction)(void *,EXEC_STATUS,FILE *);
   void (*bsaveFunction)(void *,EXEC_STATUS,FILE *);
   int priority;
   struct BinaryItem *next;
  };

#if BLOAD_AND_BSAVE
typedef struct bloadcntsv
  {
   long val;
   struct bloadcntsv *nxt;
  } BLOADCNTSV;
#endif

typedef struct bsave_expr
  {
   unsigned short type;
   long value,arg_list,next_arg;
  } BSAVE_EXPRESSION;

#define CONSTRUCT_HEADER_SIZE 20

#define BSAVE_DATA 39

struct bsaveData
  { 
   struct BinaryItem *ListOfBinaryItems;
#if BLOAD_AND_BSAVE
   BLOADCNTSV *BloadCountSaveTop;
#endif
  };

#define BsaveData(theEnv,execStatus) ((struct bsaveData *) GetEnvironmentData(theEnv,execStatus,BSAVE_DATA))

#define Bsave(a) EnvBsave(GetCurrentEnvironment(),GetCurrentExecutionState(),a)

   LOCALE void                    InitializeBsaveData(void *,EXEC_STATUS);
   LOCALE int                     BsaveCommand(void *, EXEC_STATUS);
#if BLOAD_AND_BSAVE
   LOCALE intBool                 EnvBsave(void *,EXEC_STATUS,char *);
   LOCALE void                    MarkNeededItems(void *,EXEC_STATUS,struct expr *);
   LOCALE void                    SaveBloadCount(void *,EXEC_STATUS,long);
   LOCALE void                    RestoreBloadCount(void *,EXEC_STATUS,long *);
#endif
   LOCALE intBool                 AddBinaryItem(void *,EXEC_STATUS,char *,int,
                                                void (*)(void *,EXEC_STATUS),
                                                void (*)(void *,EXEC_STATUS,FILE *),
                                                void (*)(void *,EXEC_STATUS,FILE *),
                                                void (*)(void *,EXEC_STATUS,FILE *),
                                                void (*)(void *,EXEC_STATUS),
                                                void (*)(void *,EXEC_STATUS),
                                                void (*)(void *,EXEC_STATUS));

#ifndef _BSAVE_SOURCE_
   extern struct BinaryItem      *ListOfBinaryItems;
#endif

#endif








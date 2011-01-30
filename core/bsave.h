   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/13/98          */
   /*                                                     */
   /*                 BSAVE HEADER FILE                   */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
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
   void (*findFunction)(void);
   void (*bloadStorageFunction)(void);
   void (*bloadFunction)(void);
   void (*clearFunction)(void);
   void (*expressionFunction)(FILE *);
   void (*bsaveStorageFunction)(FILE *);
   void (*bsaveFunction)(FILE *);
   int priority;
   struct BinaryItem *next;
  };


typedef struct bsave_expr
  {
   short type;
   long value,arg_list,next_arg;
  } BSAVE_EXPRESSION;

#define CONSTRUCT_HEADER_SIZE 20

   LOCALE int                     BsaveCommand(void);
#if BLOAD_AND_BSAVE
   LOCALE DllExport BOOLEAN       Bsave(char *);
   LOCALE void                    MarkNeededItems(struct expr *);
   LOCALE void                    SaveBloadCount(long);
   LOCALE void                    RestoreBloadCount(long *);
#endif
#if BLOAD_AND_BSAVE || BSAVE_INSTANCES
   LOCALE void                    GenWrite(void *,unsigned long,FILE *);
#endif
   LOCALE BOOLEAN                 AddBinaryItem(char *,int ,void (*)(void),
                                                void (*)(FILE *),void (*)(FILE *),
                                                void (*)(FILE *),void (*)(void),
                                                void (*)(void),void (*)(void));

#ifndef _BSAVE_SOURCE_
   extern Thread struct BinaryItem      *ListOfBinaryItems;
#endif

#endif








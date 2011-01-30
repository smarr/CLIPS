   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*            DEFMODULE UTILITY HEADER FILE            */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides routines for parsing module/construct   */
/*   names and searching through modules for specific        */
/*   constructs.                                             */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_modulutl
#define _H_modulutl

#ifndef _H_symbol
#include "symbol.h"
#endif
#ifndef _H_moduldef
#include "moduldef.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _MODULUTL_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE int                            FindModuleSeparator(char *);
   LOCALE SYMBOL_HN                     *ExtractModuleName(int,char *);
   LOCALE SYMBOL_HN                     *ExtractConstructName(int,char *);
   LOCALE char                          *ExtractModuleAndConstructName(char *);
   LOCALE void                          *FindImportedConstruct(char *,struct defmodule *,
                                                               char *,int *,int,struct defmodule *);
   LOCALE void                           AmbiguousReferenceErrorMessage(char *,char *);
   LOCALE void                           MarkModulesAsUnvisited(void);
   LOCALE void                           ListItemsDriver(char *,struct defmodule *,
                                                         char *,char *,
                                                          void *(*)(void *),
                                                          char *(*)(void *),
                                                          void (*)(char *,void *),
                                                          int (*)(void *));
   LOCALE long                           DoForAllModules(void (*)(struct defmodule *,void *),
                                                          int,void *);

#endif




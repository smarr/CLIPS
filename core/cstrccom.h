   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*           CONSTRUCT COMMAND HEADER MODULE           */
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

#ifndef _H_cstrccom

#define _H_cstrccom

#ifndef _H_moduldef
#include "moduldef.h"
#endif
#ifndef _H_constrct
#include "constrct.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _CSTRCCOM_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if (! RUN_TIME)
   LOCALE void                           AddConstructToModule(struct constructHeader *);
#endif
   LOCALE BOOLEAN                        DeleteNamedConstruct(char *,struct construct *);
   LOCALE void                          *FindNamedConstruct(char *,struct construct *);
   LOCALE void                           UndefconstructCommand(char *,struct construct *);
   LOCALE int                            PPConstruct(char *,char *,struct construct *);
   LOCALE SYMBOL_HN                     *GetConstructModuleCommand(char *,struct construct *);
   LOCALE struct defmodule              *GetConstructModule(char *,struct construct *);
   LOCALE BOOLEAN                        Undefconstruct(void *,struct construct *);
   LOCALE void                           SaveConstruct(char *,struct construct *);
   LOCALE char                          *GetConstructNameString(struct constructHeader *);
   LOCALE char                          *GetConstructModuleName(struct constructHeader *);
   LOCALE SYMBOL_HN                     *GetConstructNamePointer(struct constructHeader *);
   LOCALE void                           GetConstructListFunction(char *,DATA_OBJECT_PTR,
                                                                  struct construct *);
   LOCALE void                           GetConstructList(DATA_OBJECT_PTR,struct construct *,
                                                          struct defmodule *);
   LOCALE void                           ListConstructCommand(char *,struct construct *);
   LOCALE void                           ListConstruct(struct construct *,char *,struct defmodule *);
   LOCALE void                           SetNextConstruct(struct constructHeader *,struct constructHeader *);
   LOCALE struct defmoduleItemHeader    *GetConstructModuleItem(struct constructHeader *);
   LOCALE char                          *GetConstructPPForm(struct constructHeader *);
   LOCALE void                           PPConstructCommand(char *,struct construct *);
   LOCALE struct constructHeader        *GetNextConstructItem(struct constructHeader *,int);
   LOCALE struct defmoduleItemHeader    *GetConstructModuleItemByIndex(struct defmodule *,int);
   LOCALE void                           FreeConstructHeaderModule(struct defmoduleItemHeader *,
                                                                   struct construct *);
   LOCALE long                           DoForAllConstructs(void (*)(struct constructHeader *,void *),int,int,void *);
   LOCALE void                           InitializeConstructHeader(char *,struct constructHeader *,SYMBOL_HN *);
   LOCALE void                           SetConstructPPForm(struct constructHeader *,char *);
   LOCALE void                          *LookupConstruct(struct construct *,char *,BOOLEAN);
#if DEBUGGING_FUNCTIONS
   LOCALE BOOLEAN                        ConstructPrintWatchAccess(struct construct *,char *,
                                            EXPRESSION *,BOOLEAN (*)(void *),
                                            void (*)(BOOLEAN,void *));
   LOCALE BOOLEAN                        ConstructSetWatchAccess(struct construct *,BOOLEAN,
                                            EXPRESSION *,BOOLEAN (*)(void *),
                                            void (*)(BOOLEAN,void *));
#endif

#endif







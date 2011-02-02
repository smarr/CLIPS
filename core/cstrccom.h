   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
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
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added ConstructsDeletable function.            */
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
   LOCALE intBool                        DeleteNamedConstruct(void *,EXEC_STATUS,char *,struct construct *);
   LOCALE void                          *FindNamedConstruct(void *,EXEC_STATUS,char *,struct construct *);
   LOCALE void                           UndefconstructCommand(void *,EXEC_STATUS,char *,struct construct *);
   LOCALE int                            PPConstruct(void *,EXEC_STATUS,char *,char *,struct construct *);
   LOCALE SYMBOL_HN                     *GetConstructModuleCommand(void *,EXEC_STATUS,char *,struct construct *);
   LOCALE struct defmodule              *GetConstructModule(void *,EXEC_STATUS,char *,struct construct *);
   LOCALE intBool                        Undefconstruct(void *,EXEC_STATUS,void *,struct construct *);
   LOCALE void                           SaveConstruct(void *,EXEC_STATUS,void *,char *,struct construct *);
   LOCALE char                          *GetConstructNameString(struct constructHeader *);
   LOCALE char                          *EnvGetConstructNameString(void *,EXEC_STATUS,struct constructHeader *);
   LOCALE char                          *GetConstructModuleName(struct constructHeader *);
   LOCALE SYMBOL_HN                     *GetConstructNamePointer(struct constructHeader *);
   LOCALE void                           GetConstructListFunction(void *,EXEC_STATUS,char *,DATA_OBJECT_PTR,
                                                                  struct construct *);
   LOCALE void                           GetConstructList(void *,EXEC_STATUS,DATA_OBJECT_PTR,struct construct *,
                                                          struct defmodule *);
   LOCALE void                           ListConstructCommand(void *,EXEC_STATUS,char *,struct construct *);
   LOCALE void                           ListConstruct(void *,EXEC_STATUS,struct construct *,char *,struct defmodule *);
   LOCALE void                           SetNextConstruct(struct constructHeader *,struct constructHeader *);
   LOCALE struct defmoduleItemHeader    *GetConstructModuleItem(struct constructHeader *);
   LOCALE char                          *GetConstructPPForm(void *,EXEC_STATUS,struct constructHeader *);
   LOCALE void                           PPConstructCommand(void *,EXEC_STATUS,char *,struct construct *);
   LOCALE struct constructHeader        *GetNextConstructItem(void *,EXEC_STATUS,struct constructHeader *,int);
   LOCALE struct defmoduleItemHeader    *GetConstructModuleItemByIndex(void *,EXEC_STATUS,struct defmodule *,int);
   LOCALE void                           FreeConstructHeaderModule(void *,EXEC_STATUS,struct defmoduleItemHeader *,
                                                                   struct construct *);
   LOCALE long                           DoForAllConstructs(void *,EXEC_STATUS,
                                                            void (*)(void *,EXEC_STATUS,struct constructHeader *,void *),
                                                            int,int,void *);
   LOCALE void                           DoForAllConstructsInModule(void *,EXEC_STATUS,void *,
                                                            void (*)(void *,EXEC_STATUS,struct constructHeader *,void *),
                                                            int,int,void *);
   LOCALE void                           InitializeConstructHeader(void *,EXEC_STATUS,char *,struct constructHeader *,SYMBOL_HN *);
   LOCALE void                           SetConstructPPForm(void *,EXEC_STATUS,struct constructHeader *,char *);
   LOCALE void                          *LookupConstruct(void *,EXEC_STATUS,struct construct *,char *,intBool);
#if DEBUGGING_FUNCTIONS
   LOCALE unsigned                       ConstructPrintWatchAccess(void *,EXEC_STATUS,struct construct *,char *,
                                            EXPRESSION *,
                                            unsigned (*)(void *,EXEC_STATUS,void *),
                                            void (*)(void *,EXEC_STATUS,unsigned,void *));
   LOCALE unsigned                       ConstructSetWatchAccess(void *,EXEC_STATUS,struct construct *,unsigned,
                                            EXPRESSION *,
                                            unsigned (*)(void *,EXEC_STATUS,void *),
                                            void (*)(void *,EXEC_STATUS,unsigned,void *));
#endif
   LOCALE intBool                        ConstructsDeletable(void *,EXEC_STATUS);

#endif




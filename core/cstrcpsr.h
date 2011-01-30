   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*              CONSTRUCT PARSER MODULE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Parsing routines and utilities for parsing       */
/*   constructs.                                             */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_cstrcpsr

#define _H_cstrcpsr

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_scanner
#include "scanner.h"
#endif
#ifndef _H_constrct
#include "constrct.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _CSTRCPSR_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE DllExport int                  Load(char *);
   LOCALE DllExport int                  LoadConstructsFromLogicalName(char *);
   LOCALE int                            ParseConstruct(char *,char *);
   LOCALE void                           RemoveConstructFromModule(struct constructHeader *);
   LOCALE struct symbolHashNode         *GetConstructNameAndComment(char *,
                                         struct token *,char *,void *(*)(char *),
                                         int (*)(void *),char *,int,int,int);
   LOCALE void                           ImportExportConflictMessage(char *,char *,char *,char *);

#ifndef _CSTRCPSR_SOURCE_
   extern Thread int                         CheckSyntaxMode;
#endif

#endif




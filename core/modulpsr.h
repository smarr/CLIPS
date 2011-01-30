   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*             DEFMODULE PARSER HEADER FILE            */
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

#ifndef _H_modulpsr
#define _H_modulpsr

#ifndef _H_symbol
#include "symbol.h"
#endif
#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_moduldef
#include "moduldef.h"
#endif

struct portConstructItem
  {
   char *constructName;
   int typeExpected;
   struct portConstructItem *next;
  };

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _MODULPSR_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE long                           GetNumberOfDefmodules(void);
   LOCALE void                           SetNumberOfDefmodules(long);
   LOCALE void                           AddAfterModuleDefinedFunction(char *,void (*)(void),int);
   LOCALE int                            ParseDefmodule(char *);
   LOCALE void                           AddPortConstructItem(char *,int);
   LOCALE struct portConstructItem      *ValidPortConstructItem(char *);
   LOCALE int                            FindImportExportConflict(char *,struct defmodule *,char *);

#endif




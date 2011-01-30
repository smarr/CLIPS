   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*             ARGUMENT ACCESS HEADER FILE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides access routines for accessing arguments */
/*   passed to user or system functions defined using the    */
/*   DefineFunction protocol.                                */
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

#ifndef _H_argacces

#define _H_argacces

#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_moduldef
#include "moduldef.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _ARGACCES_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE char                          *RtnLexeme(int);
   LOCALE double                         RtnDouble(int);
   LOCALE long                           RtnLong(int);
   LOCALE struct dataObject             *RtnUnknown(int,struct dataObject *);
   LOCALE int                            RtnArgCount(void);
   LOCALE int                            ArgCountCheck(char *,int,int);
   LOCALE int                            ArgRangeCheck(char *,int,int);
   LOCALE int                            ArgTypeCheck(char *,int,int,struct dataObject *);
   LOCALE BOOLEAN                        GetNumericArgument(struct expr *,char *,struct dataObject *,int,int);
   LOCALE char                          *GetLogicalName(int,char *);
   LOCALE char                          *GetFileName(char *,int);
   LOCALE char                          *GetConstructName(char *,char *);
   LOCALE void                           ExpectedCountError(char *,int,int);
   LOCALE void                           OpenErrorMessage(char *,char *);
   LOCALE BOOLEAN                        CheckFunctionArgCount(char *,char *,int);
   LOCALE void                           ExpectedReturnTypeError(char *,char *);
   LOCALE void                           ExpectedTypeError1(char *,int,char *);
   LOCALE void                           ExpectedTypeError2(char *,int);
   LOCALE struct defmodule              *GetModuleName(char *,int,int *);
   LOCALE void                          *GetFactOrInstanceArgument(int,DATA_OBJECT *,char *);

#endif







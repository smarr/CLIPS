   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.20  01/31/02            */
   /*                                                     */
   /*           SYMBOL BINARY SAVE HEADER FILE            */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for      */
/*    atomic data values.                                    */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_symblbin
#define _H_symblbin

#ifndef _STDIO_INCLUDED_
#define _STDIO_INCLUDED_
#include <stdio.h>
#endif

#ifndef _H_symbol
#include "symbol.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _SYMBLBIN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define BitMapPointer(i) ((BITMAP_HN *) (SymbolData(theEnv,execStatus)->BitMapArray[i]))
#define SymbolPointer(i) ((SYMBOL_HN *) (SymbolData(theEnv,execStatus)->SymbolArray[i]))
#define FloatPointer(i) ((FLOAT_HN *) (SymbolData(theEnv,execStatus)->FloatArray[i]))
#define IntegerPointer(i) ((INTEGER_HN *) (SymbolData(theEnv,execStatus)->IntegerArray[i]))

   LOCALE void                    MarkNeededAtomicValues(void);
   LOCALE void                    WriteNeededAtomicValues(void *,EXEC_STATUS,FILE *);
   LOCALE void                    ReadNeededAtomicValues(void *,EXEC_STATUS);
   LOCALE void                    InitAtomicValueNeededFlags(void *,EXEC_STATUS);
   LOCALE void                    FreeAtomicValueStorage(void *,EXEC_STATUS);
   LOCALE void                    WriteNeededSymbols(void *,EXEC_STATUS,FILE *);
   LOCALE void                    WriteNeededFloats(void *,EXEC_STATUS,FILE *);
   LOCALE void                    WriteNeededIntegers(void *,EXEC_STATUS,FILE *);
   LOCALE void                    ReadNeededSymbols(void *,EXEC_STATUS);
   LOCALE void                    ReadNeededFloats(void *,EXEC_STATUS);
   LOCALE void                    ReadNeededIntegers(void *,EXEC_STATUS);

#endif




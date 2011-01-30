   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*           SYMBOL BINARY SAVE HEADER FILE            */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for      */
/*    atomic data values.                                    */
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

#define BitMapPointer(i) ((BITMAP_HN *) (BitMapArray[i]))
#define SymbolPointer(i) ((SYMBOL_HN *) (SymbolArray[i]))
#define FloatPointer(i) ((FLOAT_HN *) (FloatArray[i]))
#define IntegerPointer(i) ((INTEGER_HN *) (IntegerArray[i]))

   LOCALE void                    MarkNeededAtomicValues(void);
   LOCALE void                    WriteNeededAtomicValues(FILE *);
   LOCALE void                    ReadNeededAtomicValues(void);
   LOCALE void                    InitAtomicValueNeededFlags(void);
   LOCALE DllExport void          FreeAtomicValueStorage(void);

#ifndef _SYMBLBIN_SOURCE_
   extern Thread struct symbolHashNode *     *SymbolArray;
   extern Thread struct floatHashNode *      *FloatArray;
   extern Thread struct integerHashNode *    *IntegerArray;
   extern Thread struct bitMapHashNode *     *BitMapArray;
#endif

#endif








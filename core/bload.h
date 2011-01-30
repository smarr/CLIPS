   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/13/98          */
   /*                                                     */
   /*                 BLOAD HEADER FILE                   */
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

#ifndef _H_bload
#define _H_bload

#ifndef _H_extnfunc
#include "extnfunc.h"
#endif
#ifndef _H_exprnbin
#include "exprnbin.h"
#endif
#ifndef _H_symbol
#include "symbol.h"
#endif
#ifndef _H_sysdep
#include "sysdep.h"
#endif
#ifndef _H_symblbin
#include "symblbin.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif
#ifdef _BLOAD_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define FunctionPointer(i) ((struct FunctionDefinition *) (((i) == -1L) ? NULL : FunctionArray[i]))

   LOCALE int                     BloadCommand(void);
   LOCALE DllExport BOOLEAN       Bload(char *);
   LOCALE void                    BloadandRefresh(long,unsigned,void (*)(void *,long));
   LOCALE BOOLEAN                 Bloaded(void);
   LOCALE void                    AddBeforeBloadFunction(char *,void (*)(void),int);
   LOCALE void                    AddAfterBloadFunction(char *,void (*)(void),int);
   LOCALE void                    AddBloadReadyFunction(char *,int (*)(void),int);
   LOCALE void                    AddClearBloadReadyFunction(char *,int (*)(void),int);
   LOCALE void                    AddAbortBloadFunction(char *,void (*)(void),int);
   LOCALE void                    CannotLoadWithBloadMessage(char *);

#ifndef _BLOAD_SOURCE_
   extern Thread char                                  *BinaryPrefixID;
   extern Thread char                                  *BinaryVersionID;
   extern Thread struct FunctionDefinition            **FunctionArray;
#endif

#endif



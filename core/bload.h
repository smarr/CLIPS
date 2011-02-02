   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.24  06/05/06          */
   /*                                                     */
   /*                 BLOAD HEADER FILE                   */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#ifndef _H_bload
#define _H_bload

#ifndef _H_utility
#include "utility.h"
#endif
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

#define BLOAD_DATA 38

struct bloadData
  { 
   char *BinaryPrefixID;
   char *BinaryVersionID;
   struct FunctionDefinition **FunctionArray;
   int BloadActive;
   struct callFunctionItem *BeforeBloadFunctions;
   struct callFunctionItem *AfterBloadFunctions;
   struct callFunctionItem *ClearBloadReadyFunctions;
   struct callFunctionItem *AbortBloadFunctions;
  };

#define BloadData(theEnv) ((struct bloadData *) GetEnvironmentData(theEnv,execStatus,BLOAD_DATA))

#ifdef LOCALE
#undef LOCALE
#endif
#ifdef _BLOAD_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define FunctionPointer(i) ((struct FunctionDefinition *) (((i) == -1L) ? NULL : BloadData(theEnv)->FunctionArray[i]))

#define Bload(a) EnvBload(GetCurrentEnvironment(),a)

   LOCALE void                    InitializeBloadData(void *,EXEC_STATUS);
   LOCALE int                     BloadCommand(void *,EXEC_STATUS);
   LOCALE intBool                 EnvBload(void *,EXEC_STATUS,char *);
   LOCALE void                    BloadandRefresh(void *,EXEC_STATUS,long,size_t,void (*)(void *,EXEC_STATUS,void *,long));
   LOCALE intBool                 Bloaded(void *,EXEC_STATUS);
   LOCALE void                    AddBeforeBloadFunction(void *,EXEC_STATUS,char *,void (*)(void *,EXEC_STATUS),int);
   LOCALE void                    AddAfterBloadFunction(void *,EXEC_STATUS,char *,void (*)(void *,EXEC_STATUS),int);
   LOCALE void                    AddBloadReadyFunction(void *,EXEC_STATUS,char *,int (*)(void),int);
   LOCALE void                    AddClearBloadReadyFunction(void *,EXEC_STATUS,char *,int (*)(void *,EXEC_STATUS),int);
   LOCALE void                    AddAbortBloadFunction(void *,EXEC_STATUS,char *,void (*)(void *,EXEC_STATUS),int);
   LOCALE void                    CannotLoadWithBloadMessage(void *,EXEC_STATUS,char *);

#endif


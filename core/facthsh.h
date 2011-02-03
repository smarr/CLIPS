   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*                 FACT HASHING MODULE                 */
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
/*************************************************************/

#ifndef _H_facthsh

#define _H_facthsh

struct factHashEntry;

#ifndef _H_factmngr
#include "fact/fact_manager.h"
#endif

struct factHashEntry
  {
   struct fact *theFact;
   struct factHashEntry *next;
  };

#define SIZE_FACT_HASH 16231

#ifdef LOCALE
#undef LOCALE
#endif
#ifdef _FACTHSH_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define GetFactDuplication() EnvGetFactDuplication(GetCurrentEnvironment(),getCurrentExecutionState())
#define SetFactDuplication(a) EnvSetFactDuplication(GetCurrentEnvironment(),getCurrentExecutionState(),a)

   LOCALE void                           AddHashedFact(void *,EXEC_STATUS,struct fact *,unsigned long);
   LOCALE intBool                        RemoveHashedFact(void *,EXEC_STATUS,struct fact *);
   LOCALE unsigned long                  HandleFactDuplication(void *,EXEC_STATUS,void *,intBool *);
   LOCALE intBool                        EnvGetFactDuplication(void *,EXEC_STATUS);
   LOCALE intBool                        EnvSetFactDuplication(void *,EXEC_STATUS,int);
   LOCALE void                           InitializeFactHashTable(void *,EXEC_STATUS);
   LOCALE void                           ShowFactHashTable(void *,EXEC_STATUS);
   LOCALE unsigned long                  HashFact(struct fact *);

#endif



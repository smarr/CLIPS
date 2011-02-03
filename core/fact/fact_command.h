   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.21  06/15/03            */
   /*                                                     */
   /*               FACT COMMANDS HEADER FILE             */
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

#ifndef _H_factcom
#define _H_factcom

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _FACTCOM_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define Facts(a,b,c,d,e) EnvFacts(GetCurrentEnvironment(),GetCurrentExecutionState(),a,b,c,d,e)
#define LoadFacts(a) EnvLoadFacts(GetCurrentEnvironment(),GetCurrentExecutionState(),a)
#define SaveFacts(a,b,c) EnvSaveFacts(GetCurrentEnvironment(),GetCurrentExecutionState(),a,b,c)
#define LoadFactsFromString(a,b) EnvLoadFactsFromString(GetCurrentEnvironment(),GetCurrentExecutionState(),a,b)

   LOCALE void                           FactCommandDefinitions(void *, EXEC_STATUS);
   LOCALE void                           AssertCommand(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                           RetractCommand(void *, EXEC_STATUS);
   LOCALE void                           ProcessEventCommand(void *, EXEC_STATUS);
   LOCALE void                           AssertStringFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                           FactsCommand(void *, EXEC_STATUS);
   LOCALE void                           EnvFacts(void *,EXEC_STATUS,char *,void *,long long,long long,long long);
   LOCALE int                            SetFactDuplicationCommand(void *, EXEC_STATUS);
   LOCALE int                            GetFactDuplicationCommand(void *, EXEC_STATUS);
   LOCALE int                            SaveFactsCommand(void *, EXEC_STATUS);
   LOCALE int                            LoadFactsCommand(void *, EXEC_STATUS);
   LOCALE int                            EnvSaveFacts(void *,EXEC_STATUS,char *,int,struct expr *);
   LOCALE int                            EnvLoadFacts(void *,EXEC_STATUS,char *);
   LOCALE int                            EnvLoadFactsFromString(void *,EXEC_STATUS,char *,int);
   LOCALE long long                      FactIndexFunction(void *, EXEC_STATUS);

#endif



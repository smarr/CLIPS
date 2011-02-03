   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  10/19/06            */
   /*                                                     */
   /*             DEFRULE COMMANDS HEADER FILE            */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides the matches command. Also provides the  */
/*   the developer commands show-joins and rule-complexity.  */
/*   Also provides the initialization routine which          */
/*   registers rule commands found in other modules.         */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Added support for hashed alpha memories.       */
/*                                                           */
/*            Added matches-count function.                  */
/*                                                           */
/*            Added get-join-hashing and set-join-hashing    */
/*            functions.                                     */
/*                                                           */
/*************************************************************/

#ifndef _H_rulecom
#define _H_rulecom

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _RULECOM_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define Matches(a) EnvMatches(GetCurrentEnvironment(),getCurrentExecutionState(),a)
#define JoinActivity(a,b) EnvJoinActivity(GetCurrentEnvironment(),getCurrentExecutionState(),a,b)
#define MatchesCount(a) EnvMatchesCount(GetCurrentEnvironment(),getCurrentExecutionState(),a)
#define GetBetaMemoryResizing() EnvGetBetaMemoryResizing(GetCurrentEnvironment(),getCurrentExecutionState())
#define SetBetaMemoryResizing(a) EnvSetBetaMemoryResizing(GetCurrentEnvironment(),getCurrentExecutionState(),a)

   LOCALE intBool                        EnvGetBetaMemoryResizing(void *,EXEC_STATUS);
   LOCALE intBool                        EnvSetBetaMemoryResizing(void *,EXEC_STATUS,intBool);
   LOCALE int                            GetBetaMemoryResizingCommand(void *,EXEC_STATUS);
   LOCALE int                            SetBetaMemoryResizingCommand(void *,EXEC_STATUS);

   LOCALE intBool                        EnvMatches(void *,EXEC_STATUS,void *);
   LOCALE long long                      EnvJoinActivity(void *,EXEC_STATUS,void *,int);
   LOCALE intBool                        EnvMatchesCount(void *,EXEC_STATUS,void *);
   LOCALE void                           DefruleCommands(void *,EXEC_STATUS);
   LOCALE void                           MatchesCommand(void *,EXEC_STATUS);
   LOCALE long long                      JoinActivityCommand(void *,EXEC_STATUS);
   LOCALE void                           MatchesCountCommand(void *,EXEC_STATUS);
   LOCALE long long                      TimetagFunction(void *,EXEC_STATUS);
#if DEVELOPER
   LOCALE void                           ShowJoinsCommand(void *,EXEC_STATUS);
   LOCALE long                           RuleComplexityCommand(void *,EXEC_STATUS);
   LOCALE void                           ShowAlphaHashTable(void *,EXEC_STATUS);
#endif

#endif



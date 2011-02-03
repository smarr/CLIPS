   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*                 UTILITY HEADER FILE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides a set of utility functions useful to    */
/*   other modules. Primarily these are the functions for    */
/*   handling periodic garbage collection and appending      */
/*   string data.                                            */
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

#ifndef _H_utility
#define _H_utility

#ifdef LOCALE
#undef LOCALE
#endif

# include "evaluatn.h"

struct callFunctionItem
  {
   char *name;
   void (*func)(void *,EXEC_STATUS);
   int priority;
   struct callFunctionItem *next;
   short int environmentAware;
   void *context;
  };
  
struct trackedMemory
  {
   void *theMemory;
   struct trackedMemory *next;
   struct trackedMemory *prev;
   size_t memSize;
  };
  
#define UTILITY_DATA 55

struct utilityData
  { 
   struct callFunctionItem *ListOfCleanupFunctions;
   struct callFunctionItem *ListOfPeriodicFunctions;
   short GarbageCollectionLocks;
   short GarbageCollectionHeuristicsEnabled;
   short PeriodicFunctionsEnabled;
   short YieldFunctionEnabled;
   long EphemeralItemCount;
   long EphemeralItemSize;
   long CurrentEphemeralCountMax;
   long CurrentEphemeralSizeMax;
   void (*YieldTimeFunction)(void);
   int LastEvaluationDepth ;
   struct trackedMemory *trackList;
  };

#define UtilityData(theEnv,execStatus) ((struct utilityData *) GetEnvironmentData(theEnv,execStatus,UTILITY_DATA))

  /* Is c the start of a utf8 sequence? */
#define IsUTF8Start(ch) (((ch) & 0xC0) != 0x80)
#define IsUTF8MultiByteStart(ch) ((((unsigned char) ch) >= 0xC0) && (((unsigned char) ch) <= 0xF7))
#define IsUTF8MultiByteContinuation(ch) ((((unsigned char) ch) >= 0x80) && (((unsigned char) ch) <= 0xBF))

#ifdef _UTILITY_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define DecrementGCLocks() EnvDecrementGCLocks(GetCurrentEnvironment(),GetCurrentExecutionStatus())
#define IncrementGCLocks() EnvIncrementGCLocks(GetCurrentEnvironment(),GetCurrentExecutionStatus())
#define RemovePeriodicFunction(a) EnvRemovePeriodicFunction(GetCurrentEnvironment(),GetCurrentExecutionStatus(),a)

   LOCALE void                           InitializeUtilityData(void *,EXEC_STATUS);
   LOCALE void                           PeriodicCleanup(void *, EXEC_STATUS, intBool,intBool);
   LOCALE intBool                        AddCleanupFunction(void *,EXEC_STATUS,char *,void (*)(void *,EXEC_STATUS),int);
   LOCALE intBool                        EnvAddPeriodicFunction(void *,EXEC_STATUS,char *,void (*)(void *,EXEC_STATUS),int);
   LOCALE intBool                        AddPeriodicFunction(char *,void (*)(void),int);
   LOCALE intBool                        RemoveCleanupFunction(void *,EXEC_STATUS,char *);
   LOCALE intBool                        EnvRemovePeriodicFunction(void *,EXEC_STATUS,char *);
   LOCALE char                          *AppendStrings(void *,EXEC_STATUS,char *,char *);
   LOCALE char                          *StringPrintForm(void *,EXEC_STATUS,char *);
   LOCALE char                          *AppendToString(void *,EXEC_STATUS,char *,char *,size_t *,size_t *);
   LOCALE char                          *InsertInString(void *,EXEC_STATUS,char *,size_t,char *,size_t *,size_t *);
   LOCALE char                          *AppendNToString(void *,EXEC_STATUS,char *,char *,size_t,size_t *,size_t *);
   LOCALE char                          *EnlargeString(void *,EXEC_STATUS,size_t,char *,size_t *,size_t *);
   LOCALE char                          *ExpandStringWithChar(void *,EXEC_STATUS,int,char *,size_t *,size_t *,size_t);
   LOCALE struct callFunctionItem       *AddFunctionToCallList(void *,EXEC_STATUS,char *,int,void (*)(void *,EXEC_STATUS),
                                                               struct callFunctionItem *,intBool);
   LOCALE struct callFunctionItem       *AddFunctionToCallListWithContext(void *,EXEC_STATUS,char *,int,void (*)(void *,EXEC_STATUS),
                                                                          struct callFunctionItem *,intBool,void *);
   LOCALE struct callFunctionItem       *RemoveFunctionFromCallList(void *,EXEC_STATUS,char *,
                                                             struct callFunctionItem *,
                                                             int *);
   LOCALE void                           DeallocateCallList(void *,EXEC_STATUS,struct callFunctionItem *);
   LOCALE unsigned long                  ItemHashValue(void *,EXEC_STATUS,unsigned short,void *,unsigned long);
   LOCALE void                           YieldTime(void *,EXEC_STATUS);
   LOCALE short                          SetGarbageCollectionHeuristics(void *,EXEC_STATUS,short);
   LOCALE void                           EnvIncrementGCLocks(void *,EXEC_STATUS);
   LOCALE void                           EnvDecrementGCLocks(void *,EXEC_STATUS);
   LOCALE short                          EnablePeriodicFunctions(void *,EXEC_STATUS,short);
   LOCALE short                          EnableYieldFunction(void *,EXEC_STATUS,short);
   LOCALE struct trackedMemory          *AddTrackedMemory(void *,EXEC_STATUS,void *,size_t);
   LOCALE void                           RemoveTrackedMemory(void *,EXEC_STATUS,struct trackedMemory *);
   LOCALE void                           UTF8Increment(char *,size_t *);
   LOCALE size_t                         UTF8Offset(char *,size_t);
   LOCALE size_t                         UTF8Length(char *);
   LOCALE size_t                         UTF8CharNum(char *,size_t);
   
#endif





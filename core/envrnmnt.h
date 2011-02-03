   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  10/19/06            */
   /*                                                     */
   /*                ENVRNMNT HEADER FILE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Routines for supporting multiple environments.   */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added CreateRuntimeEnvironment function.       */
/*                                                           */
/*            Added support for context information when an  */
/*            environment is created (i.e a pointer from the */
/*            CLIPS environment to its parent environment).  */
/*                                                           */
/*      6.30: Added support for passing context information  */ 
/*            to user defined functions and callback         */
/*            functions.                                     */
/*                                                           */
/*************************************************************/

#ifndef _H_envrnmnt
#define _H_envrnmnt

#ifndef _H_symbol
#include "symbol.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _ENVRNMNT_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define USER_ENVIRONMENT_DATA 70
#define MAXIMUM_ENVIRONMENT_POSITIONS 100

# include <apr_pools.h>
# include <apr_thread_pool.h>
# include <apr_thread_rwlock.h>

# include "execution_status.h"


struct environmentCleanupFunction
  {
   char *name;
   void (*func)(void *,EXEC_STATUS);
   int priority;
   struct environmentCleanupFunction *next;
  };

struct environmentData
  {   
   unsigned int initialized : 1;
   unsigned long environmentIndex;
   void *context;
   void *routerContext;
   void *functionContext;
   void *callbackContext;
   void **theData;
   void (**cleanupFunctions)(void *,EXEC_STATUS);
   struct environmentCleanupFunction *listOfCleanupEnvironmentFunctions;
   struct environmentData *next;
    
    // STEFAN: Adding thread pool properties
    apr_pool_t        *memoryPool;
    apr_thread_pool_t *matcherThreadPool;
    apr_thread_pool_t *factThreadPool;
	
	// Lode: Added Reader/Writer Lock for Hash
	apr_thread_rwlock_t *factHashLock;

  };

typedef struct environmentData ENVIRONMENT_DATA;
typedef struct environmentData * ENVIRONMENT_DATA_PTR;

#define GetEnvironmentData(theEnv,execStatus,position) (((struct environmentData *) theEnv)->theData[position])
#define SetEnvironmentData(theEnv,execStatus,position,value) (((struct environmentData *) theEnv)->theData[position] = value)
#define Env(theEnv,execStatus) ((struct environmentData *)theEnv)

   LOCALE intBool                        AllocateEnvironmentData(void *,EXEC_STATUS,unsigned int,unsigned long,void (*)(void *,EXEC_STATUS));
   LOCALE intBool                        DeallocateEnvironmentData(void);
#if ALLOW_ENVIRONMENT_GLOBALS
   LOCALE void                           SetCurrentEnvironment(void *);
   LOCALE intBool                        SetCurrentEnvironmentByIndex(unsigned long);
   LOCALE void                          *GetEnvironmentByIndex(unsigned long);
   LOCALE void                          *GetCurrentEnvironment(void);
   LOCALE struct executionStatus        *GetCurrentExectionStatus(void);
   LOCALE unsigned long                  GetEnvironmentIndex(void *,EXEC_STATUS);
#endif
   LOCALE void                          *CreateEnvironment(void);
   LOCALE void                          *CreateRuntimeEnvironment(struct symbolHashNode **,struct floatHashNode **,
                                                                  struct integerHashNode **,struct bitMapHashNode **);
   LOCALE intBool                        DestroyEnvironment(void *,EXEC_STATUS);
   LOCALE intBool                        AddEnvironmentCleanupFunction(void *,EXEC_STATUS,char *,void (*)(void *,EXEC_STATUS),int);
   LOCALE void                          *GetEnvironmentContext(void *);
   LOCALE void                          *SetEnvironmentContext(void *,EXEC_STATUS,void *);
   LOCALE void                          *GetEnvironmentRouterContext(void *,EXEC_STATUS);
   LOCALE void                          *SetEnvironmentRouterContext(void *,EXEC_STATUS,void *);
   LOCALE void                          *GetEnvironmentFunctionContext(void *,EXEC_STATUS);
   LOCALE void                          *SetEnvironmentFunctionContext(void *,EXEC_STATUS,void *);
   LOCALE void                          *GetEnvironmentCallbackContext(void *);
   LOCALE void                          *SetEnvironmentCallbackContext(void *,EXEC_STATUS,void *);

#endif


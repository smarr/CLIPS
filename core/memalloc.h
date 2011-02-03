   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  10/19/06            */
   /*                                                     */
   /*            MEMORY ALLOCATION HEADER FILE            */
   /*******************************************************/

/*************************************************************/
/* Purpose: Memory allocation routines.                      */
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
/*      6.30: Added get_mem and rtn_mem macros.              */
/*                                                           */
/*************************************************************/

#ifndef _H_memalloc

#include <string.h>

#define _H_memalloc

struct chunkInfo;
struct blockInfo;
struct memoryPtr;

#define MEM_TABLE_SIZE 500

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _MEMORY_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

struct chunkInfo
  {
   struct chunkInfo *prevChunk;
   struct chunkInfo *nextFree;
   struct chunkInfo *lastFree;
   long int size;
  };

struct blockInfo
  {
   struct blockInfo *nextBlock;
   struct blockInfo *prevBlock;
   struct chunkInfo *nextFree;
   long int size;
  };

struct memoryPtr
  {
   struct memoryPtr *next;
  };

// STEFAN: TODO: make those thread-safe

#define get_struct(theEnv,execStatus,type) \
  ((MemoryData(theEnv,execStatus)->MemoryTable[sizeof(struct type)] == NULL) \
    ? ((struct type *) genalloc(theEnv,execStatus,sizeof(struct type))) \
    : ((MemoryData(theEnv,execStatus)->TempMemoryPtr = MemoryData(theEnv,execStatus)->MemoryTable[sizeof(struct type)]),\
    MemoryData(theEnv,execStatus)->MemoryTable[sizeof(struct type)] = MemoryData(theEnv,execStatus)->TempMemoryPtr->next,\
    ((struct type *) MemoryData(theEnv,execStatus)->TempMemoryPtr)))

#define rtn_struct(theEnv,execStatus,type,struct_ptr) \
  (MemoryData(theEnv,execStatus)->TempMemoryPtr = (struct memoryPtr *) struct_ptr,\
   MemoryData(theEnv,execStatus)->TempMemoryPtr->next = MemoryData(theEnv,execStatus)->MemoryTable[sizeof(struct type)], \
   MemoryData(theEnv,execStatus)->MemoryTable[sizeof(struct type)] = MemoryData(theEnv,execStatus)->TempMemoryPtr)

#define rtn_sized_struct(theEnv,execStatus,size,struct_ptr) \
  (MemoryData(theEnv,execStatus)->TempMemoryPtr = (struct memoryPtr *) struct_ptr,\
   MemoryData(theEnv,execStatus)->TempMemoryPtr->next = MemoryData(theEnv,execStatus)->MemoryTable[size], \
   MemoryData(theEnv,execStatus)->MemoryTable[size] = MemoryData(theEnv,execStatus)->TempMemoryPtr)

#define get_var_struct(theEnv,execStatus,type,vsize) \
  ((((sizeof(struct type) + vsize) <  MEM_TABLE_SIZE) ? \
    (MemoryData(theEnv,execStatus)->MemoryTable[sizeof(struct type) + vsize] == NULL) : 1) ? \
   ((struct type *) genalloc(theEnv,execStatus,(sizeof(struct type) + vsize))) :\
   ((MemoryData(theEnv,execStatus)->TempMemoryPtr = MemoryData(theEnv,execStatus)->MemoryTable[sizeof(struct type) + vsize]),\
    MemoryData(theEnv,execStatus)->MemoryTable[sizeof(struct type) + vsize] = MemoryData(theEnv,execStatus)->TempMemoryPtr->next,\
    ((struct type *) MemoryData(theEnv,execStatus)->TempMemoryPtr)))

#define rtn_var_struct(theEnv,execStatus,type,vsize,struct_ptr) \
  (MemoryData(theEnv,execStatus)->TempSize = sizeof(struct type) + vsize, \
   ((MemoryData(theEnv,execStatus)->TempSize < MEM_TABLE_SIZE) ? \
    (MemoryData(theEnv,execStatus)->TempMemoryPtr = (struct memoryPtr *) struct_ptr,\
     MemoryData(theEnv,execStatus)->TempMemoryPtr->next = MemoryData(theEnv,execStatus)->MemoryTable[MemoryData(theEnv,execStatus)->TempSize], \
     MemoryData(theEnv,execStatus)->MemoryTable[MemoryData(theEnv,execStatus)->TempSize] =  MemoryData(theEnv,execStatus)->TempMemoryPtr) : \
    (genfree(theEnv,execStatus,(void *) struct_ptr,MemoryData(theEnv,execStatus)->TempSize),(struct memoryPtr *) struct_ptr)))

#define get_mem(theEnv,execStatus,size) \
  (((size <  MEM_TABLE_SIZE) ? \
    (MemoryData(theEnv,execStatus)->MemoryTable[size] == NULL) : 1) ? \
   ((struct type *) genalloc(theEnv,execStatus,(size_t) (size))) :\
   ((MemoryData(theEnv,execStatus)->TempMemoryPtr = MemoryData(theEnv,execStatus)->MemoryTable[size]),\
    MemoryData(theEnv,execStatus)->MemoryTable[size] = MemoryData(theEnv,execStatus)->TempMemoryPtr->next,\
    ((struct type *) MemoryData(theEnv,execStatus)->TempMemoryPtr)))

#define rtn_mem(theEnv,execStatus,size,ptr) \
  (MemoryData(theEnv,execStatus)->TempSize = size, \
   ((MemoryData(theEnv,execStatus)->TempSize < MEM_TABLE_SIZE) ? \
    (MemoryData(theEnv,execStatus)->TempMemoryPtr = (struct memoryPtr *) ptr,\
     MemoryData(theEnv,execStatus)->TempMemoryPtr->next = MemoryData(theEnv,execStatus)->MemoryTable[MemoryData(theEnv,execStatus)->TempSize], \
     MemoryData(theEnv,execStatus)->MemoryTable[MemoryData(theEnv,execStatus)->TempSize] =  MemoryData(theEnv,execStatus)->TempMemoryPtr) : \
    (genfree(theEnv,execStatus,(void *) ptr,MemoryData(theEnv,execStatus)->TempSize),(struct memoryPtr *) ptr)))

#define GenCopyMemory(type,cnt,dst,src) \
   memcpy((void *) (dst),(void *) (src),sizeof(type) * (size_t) (cnt))

#define MEMORY_DATA 59

struct memoryData
  { 
   long int MemoryAmount;
   long int MemoryCalls;
   intBool ConserveMemory;
   int (*OutOfMemoryFunction)(void *,EXEC_STATUS,size_t);
#if BLOCK_MEMORY
   struct blockInfo *TopMemoryBlock;
   int BlockInfoSize;
   int ChunkInfoSize;
   int BlockMemoryInitialized;
#endif
   struct memoryPtr *TempMemoryPtr;
   struct memoryPtr **MemoryTable;
   size_t TempSize;
  };

#define MemoryData(theEnv,execStatus) ((struct memoryData *) GetEnvironmentData(theEnv,execStatus,MEMORY_DATA))

#define GetConserveMemory() EnvGetConserveMemory(GetCurrentEnvironment(),GetCurrentExecutionState())
#define MemRequests() EnvMemRequests(GetCurrentEnvironment(),GetCurrentExecutionState())
#define MemUsed() EnvMemUsed(GetCurrentEnvironment(),GetCurrentExecutionState())
#define ReleaseMem(a,b) EnvReleaseMem(GetCurrentEnvironment(),GetCurrentExecutionState(),a,b)
#define SetConserveMemory(a) EnvSetConserveMemory(GetCurrentEnvironment(),GetCurrentExecutionState(),a)
#define SetOutOfMemoryFunction(a) EnvSetOutOfMemoryFunction(GetCurrentEnvironment(),GetCurrentExecutionState(),a)


   LOCALE void                           InitializeMemory(void *,EXEC_STATUS);
   LOCALE void                          *genalloc(void *,EXEC_STATUS,size_t);
   LOCALE int                            DefaultOutOfMemoryFunction(void *,EXEC_STATUS,size_t);
   LOCALE int                          (*EnvSetOutOfMemoryFunction(void *,EXEC_STATUS,int (*)(void *,EXEC_STATUS,size_t)))(void *,EXEC_STATUS,size_t);
   LOCALE int                            genfree(void *,EXEC_STATUS,void *,size_t);
   LOCALE void                          *genrealloc(void *,EXEC_STATUS,void *,size_t,size_t);
   LOCALE long                           EnvMemUsed(void *,EXEC_STATUS);
   LOCALE long                           EnvMemRequests(void *,EXEC_STATUS);
   LOCALE long                           UpdateMemoryUsed(void *,EXEC_STATUS,long int);
   LOCALE long                           UpdateMemoryRequests(void *,EXEC_STATUS,long int);
   LOCALE long                           EnvReleaseMem(void *,EXEC_STATUS,long,int);
   LOCALE void                          *gm1(void *,EXEC_STATUS,size_t);
   LOCALE void                          *gm2(void *,EXEC_STATUS,size_t);
   LOCALE void                          *gm3(void *,EXEC_STATUS,size_t);
   LOCALE int                            rm(void *,EXEC_STATUS,void *,size_t);
   LOCALE int                            rm3(void *,EXEC_STATUS,void *,size_t);
   LOCALE unsigned long                  PoolSize(void *,EXEC_STATUS);
   LOCALE unsigned long                  ActualPoolSize(void *,EXEC_STATUS);
   LOCALE void                          *RequestChunk(void *,EXEC_STATUS,size_t);
   LOCALE int                            ReturnChunk(void *,EXEC_STATUS,void *,size_t);
   LOCALE intBool                        EnvSetConserveMemory(void *,EXEC_STATUS,intBool);
   LOCALE intBool                        EnvGetConserveMemory(void *,EXEC_STATUS);
   LOCALE void                           genmemcpy(char *,char *,unsigned long);
   LOCALE void                           ReturnAllBlocks(void *,EXEC_STATUS);

#endif







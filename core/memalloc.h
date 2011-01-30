   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/02/96            */
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
/*************************************************************/

#ifndef _H_memalloc

#include <string.h>

#define _H_memalloc

struct chunkInfo;
struct blockInfo;
struct memoryPtr;
struct longMemoryPtr;

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

struct longMemoryPtr
  {
   struct longMemoryPtr *prev;
   struct longMemoryPtr *next;
   long size;
  };

#define get_struct(type) \
  ((MemoryTable[sizeof(struct type)] == NULL) ? \
   ((struct type *) genalloc((unsigned) sizeof(struct type))) :\
   ((TempMemoryPtr = MemoryTable[sizeof(struct type)]),\
    MemoryTable[sizeof(struct type)] = TempMemoryPtr->next,\
    ((struct type *) TempMemoryPtr)))

#define rtn_struct(type,struct_ptr) \
  (TempMemoryPtr = (struct memoryPtr *) struct_ptr,\
   TempMemoryPtr->next = MemoryTable[sizeof(struct type)], \
   MemoryTable[sizeof(struct type)] = TempMemoryPtr)

#define rtn_sized_struct(size,struct_ptr) \
  (TempMemoryPtr = (struct memoryPtr *) struct_ptr,\
   TempMemoryPtr->next = MemoryTable[size], \
   MemoryTable[size] = TempMemoryPtr)

#define get_var_struct(type,vsize) \
  ((((sizeof(struct type) + vsize) <  MEM_TABLE_SIZE) ? \
    (MemoryTable[sizeof(struct type) + vsize] == NULL) : 1) ? \
   ((struct type *) genalloc((unsigned) (sizeof(struct type) + vsize))) :\
   ((TempMemoryPtr = MemoryTable[sizeof(struct type) + vsize]),\
    MemoryTable[sizeof(struct type) + vsize] = TempMemoryPtr->next,\
    ((struct type *) TempMemoryPtr)))

#define rtn_var_struct(type,vsize,struct_ptr) \
  (TempSize = sizeof(struct type) + vsize, \
   ((TempSize < MEM_TABLE_SIZE) ? \
    (TempMemoryPtr = (struct memoryPtr *) struct_ptr,\
     TempMemoryPtr->next = MemoryTable[TempSize], \
     MemoryTable[TempSize] =  TempMemoryPtr) : \
    (genfree((void *) struct_ptr,(unsigned) TempSize),(struct memoryPtr *) struct_ptr)))

#define get_var_struct2(type,vsize) \
  ((((sizeof(struct type) + vsize) <  (unsigned long) MEM_TABLE_SIZE) ? \
    (MemoryTable[sizeof(struct type) + vsize] == NULL) : 1) ? \
   ((struct type *) gm3((long) (sizeof(struct type) + vsize))) :\
   ((TempMemoryPtr = MemoryTable[sizeof(struct type) + vsize]),\
    MemoryTable[sizeof(struct type) + vsize] = TempMemoryPtr->next,\
    ((struct type *) TempMemoryPtr)))

#define rtn_var_struct2(type,vsize,struct_ptr) \
  (TempSize2 = sizeof(struct type) + vsize, \
   (((TempSize2 <  (unsigned long) MEM_TABLE_SIZE) ? \
     (TempMemoryPtr = (struct memoryPtr *) struct_ptr,\
      TempMemoryPtr->next = MemoryTable[TempSize2], \
      MemoryTable[TempSize2] =  TempMemoryPtr) : \
     (rm3((void *) struct_ptr,(long) (sizeof(struct type) + vsize)),(struct memoryPtr *) struct_ptr))))

#define GenCopyMemory(type,cnt,dst,src) \
   memcpy((void *) (dst),(void *) (src),sizeof(type) * (size_t) (cnt))

   LOCALE void                          *genalloc(unsigned int);
   LOCALE int                            DefaultOutOfMemoryFunction(unsigned long);
   LOCALE DllExport int                (*SetOutOfMemoryFunction(int (*)(unsigned long)))(unsigned long);
   LOCALE int                            genfree(void *,unsigned int);
   LOCALE void                          *genrealloc(void *,unsigned int,unsigned int);
   LOCALE DllExport long                 MemUsed(void);
   LOCALE DllExport long                 MemRequests(void);
   LOCALE long                           UpdateMemoryUsed(long int);
   LOCALE long                           UpdateMemoryRequests(long int);
   LOCALE DllExport long                 ReleaseMem(long,int);
   LOCALE void                          *gm1(int);
   LOCALE void                          *gm2(int);
   LOCALE void                          *gm3(long);
   LOCALE int                            rm(void *,int);
   LOCALE int                            rm3(void *,long);
   LOCALE unsigned long                  PoolSize(void);
   LOCALE unsigned long                  ActualPoolSize(void);
   LOCALE void                          *RequestChunk(unsigned int);
   LOCALE int                            ReturnChunk(void *,unsigned int);
   LOCALE void                          *genlongalloc(unsigned long);
   LOCALE int                            genlongfree(void *,unsigned long);
   LOCALE BOOLEAN                        SetConserveMemory(BOOLEAN);
   LOCALE BOOLEAN                        GetConserveMemory(void);
   LOCALE void                           genmemcpy(char *,char *,unsigned long);
   LOCALE void                           InitializeMemory(void);

#ifndef _MEMORY_SOURCE_
   extern Thread struct memoryPtr                      *TempMemoryPtr;
   extern Thread unsigned int                           TempSize;
   extern Thread unsigned long                          TempSize2;
   extern Thread struct memoryPtr                     **MemoryTable;
#endif

#endif







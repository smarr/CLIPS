   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                    MEMORY MODULE                    */
   /*******************************************************/

/*************************************************************/
/* Purpose: Memory allocation routines.                      */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _MEMORY_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "setup.h"

#include "constant.h"
#include "memalloc.h"
#include "router.h"
#include "utility.h"

#include <stdlib.h>

#if IBM_TBC
#include <alloc.h>
#endif
#if IBM_MSC || IBM_ICB
#include <malloc.h>
#endif
#if IBM_ZTC || IBM_SC
#include <dos.h>
#endif

#define STRICT_ALIGN_SIZE sizeof(double)

#define SpecialMalloc(sz) malloc((CLIPS_STD_SIZE) sz)
#define SpecialFree(ptr) free(ptr)

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if BLOCK_MEMORY

   static int                     InitializeBlockMemory(unsigned int);
   static int                     AllocateBlock(struct blockInfo *,unsigned int);
   static void                    AllocateChunk(struct blockInfo *,struct chunkInfo *,unsigned int);
   static void                    ReturnAllBlocks(void);
   static int                     BlockMemoryExitFunction(int);

#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static Thread long int                MemoryAmount = 0;
   static Thread long int                MemoryCalls = 0;
   static Thread BOOLEAN                 ConserveMemory = FALSE;

#if BLOCK_MEMORY
   static Thread struct longMemoryPtr   *TopLongMemoryPtr = NULL;
   static Thread struct blockInfo       *TopMemoryBlock;
   static Thread int                     BlockInfoSize;
   static Thread int                     ChunkInfoSize;
   static Thread int                     BlockMemoryInitialized = FALSE;
#endif

   static int                   (*OutOfMemoryFunction)(unsigned long)
                                       = DefaultOutOfMemoryFunction;

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle struct memoryPtr       *TempMemoryPtr;
   Thread globle struct memoryPtr      **MemoryTable;
   Thread globle unsigned int            TempSize;
   Thread globle unsigned long           TempSize2;

/********************************************/
/* InitializeMemory: Sets up memory tables. */
/********************************************/
globle void InitializeMemory()
  {
   int i;

   MemoryTable = (struct memoryPtr **)
                 malloc((CLIPS_STD_SIZE) (sizeof(struct memoryPtr *) * MEM_TABLE_SIZE));

   if (MemoryTable == NULL)
     {
      PrintErrorID("MEMORY",1,TRUE);
      PrintRouter(WERROR,"Out of memory.\n");
      ExitRouter(EXIT_FAILURE);
     }

   for (i = 0; i < MEM_TABLE_SIZE; i++) MemoryTable[i] = NULL;
  }

/***************************************************/
/* genalloc: A generic memory allocation function. */
/***************************************************/
globle void *genalloc(
  unsigned int size)
  {
   char *memPtr;

#if   BLOCK_MEMORY
   memPtr = RequestChunk(size);
   if (memPtr == NULL)
     {
      ReleaseMem((long) ((size * 5 > 4096) ? size * 5 : 4096),FALSE);
      memPtr = RequestChunk(size);
      if (memPtr == NULL)
        {
         ReleaseMem(-1L,TRUE);
         memPtr = RequestChunk(size);
         while (memPtr == NULL)
           {
            if ((*OutOfMemoryFunction)((unsigned long) size))
              return(NULL);
            memPtr = RequestChunk(size);
           }
        }
     }
#else
   memPtr = (char *) malloc((CLIPS_STD_SIZE) size);
   if (memPtr == NULL)
     {
      ReleaseMem((long) ((size * 5 > 4096) ? size * 5 : 4096),FALSE);
      memPtr = (char *) malloc((CLIPS_STD_SIZE) size);
      if (memPtr == NULL)
        {
         ReleaseMem(-1L,TRUE);
         memPtr = (char *) malloc((CLIPS_STD_SIZE) size);
         while (memPtr == NULL)
           {
            if ((*OutOfMemoryFunction)((unsigned long) size))
              return(NULL);
            memPtr = (char *) malloc((CLIPS_STD_SIZE) size);
           }
        }
     }
#endif

   MemoryAmount += size;
   MemoryCalls++;

   return((void *) memPtr);
  }

/***********************************************/
/* DefaultOutOfMemoryFunction: Function called */
/*   when the KB runs out of memory.           */
/***********************************************/
#if IBM_TBC
#pragma argsused
#endif
globle int DefaultOutOfMemoryFunction(
  unsigned long size)
  {
#if MAC_MPW || MAC_MCW || IBM_MCW
#pragma unused(size)
#endif
   PrintErrorID("MEMORY",1,TRUE);
   PrintRouter(WERROR,"Out of memory.\n");
   ExitRouter(EXIT_FAILURE);
   return(TRUE);
  }

/**********************************************************/
/* SetOutOfMemoryFunction: Allows the function which is   */
/*   called when the KB runs out of memory to be changed. */
/**********************************************************/
globle int (*SetOutOfMemoryFunction(int (*functionPtr)(unsigned long)))(unsigned long)
  {
   int (*tmpPtr)(unsigned long);

   tmpPtr = OutOfMemoryFunction;
   OutOfMemoryFunction = functionPtr;
   return(tmpPtr);
  }

/****************************************************/
/* genfree: A generic memory deallocation function. */
/****************************************************/
globle int genfree(
  void *waste,
  unsigned size)
  {
#if BLOCK_MEMORY
   if (ReturnChunk(waste,size) == FALSE)
     {
      PrintErrorID("MEMORY",2,TRUE);
      PrintRouter(WERROR,"Release error in genfree.\n");
      return(-1);
     }
#else
   free(waste);
#endif

   MemoryAmount -= size;
   MemoryCalls--;

   return(0);
  }

/******************************************************/
/* genrealloc: Simple (i.e. dumb) version of realloc. */
/******************************************************/
globle void *genrealloc(
  void *oldaddr,
  unsigned oldsz,
  unsigned newsz)
  {
   char *newaddr;
   int i, limit;

   newaddr = ((newsz != 0) ? (char *) gm2((int) newsz) : NULL);

   if (oldaddr != NULL)
     {
      limit = (oldsz < newsz) ? oldsz : newsz;
      for (i = 0 ; i < limit ; i++)
        { newaddr[i] = ((char *) oldaddr)[i]; }
      for ( ; i < newsz; i++)
        { newaddr[i] = '\0'; }
      rm((void *) oldaddr,(int) oldsz);
     }

   return((void *) newaddr);
  }

/************************************************/
/* genlongalloc: Allocates blocks of memory for */
/*   sizes expressed using long integers.       */
/************************************************/
#if IBM_TBC
#pragma warn -rch
#pragma warn -ccc
#endif
globle void *genlongalloc(
  unsigned long size)
  {
#if (! MAC) && (! IBM_TBC) && (! IBM_MSC) && (! IBM_ICB) && (! IBM_ZTC) && (! IBM_SC) && (! IBM_MCW)
   unsigned int test;
#else
   void *memPtr;
#endif
#if BLOCK_MEMORY
   struct longMemoryPtr *theLongMemory;
#endif

   if (sizeof(int) == sizeof(long))
     { return(genalloc((unsigned) size)); }

#if (! MAC) && (! IBM_TBC) && (! IBM_MSC) && (! IBM_ICB) && (! IBM_ZTC) && (! IBM_SC) && (! IBM_MCW)
   test = (unsigned int) size;
   if (test != size)
     {
      PrintErrorID("MEMORY",3,TRUE);
      PrintRouter(WERROR,"Unable to allocate memory block > 32K.\n");
      ExitRouter(EXIT_FAILURE);
     }
   return((void *) genalloc((unsigned) test));
#else

#if BLOCK_MEMORY
   size += sizeof(struct longMemoryPtr);
#endif

   memPtr = (void *) SpecialMalloc(size);
   if (memPtr == NULL)
     {
      ReleaseMem((long) ((size * 5 > 4096) ? size * 5 : 4096),FALSE);
      memPtr = (void *) SpecialMalloc(size);
      if (memPtr == NULL)
        {
         ReleaseMem(-1L,TRUE);
         memPtr = (void *) SpecialMalloc(size);
         while (memPtr == NULL)
           {
            if ((*OutOfMemoryFunction)(size))
              return(NULL);
            memPtr = (void *) SpecialMalloc(size);
           }
        }
     }
   MemoryAmount += size;
   MemoryCalls++;

#if BLOCK_MEMORY
   theLongMemory = (struct longMemoryPtr *) memPtr;
   theLongMemory->next = TopLongMemoryPtr;
   theLongMemory->prev = NULL;
   theLongMemory->size = size;
   memPtr = (void *) (theLongMemory + 1);
#endif

   return(memPtr);
#endif
  }
#if IBM_TBC
#pragma warn +rch
#pragma warn +ccc
#endif

/*********************************************/
/* genlongfree: Returns blocks of memory for */
/*   sizes expressed using long integers.    */
/*********************************************/
#if IBM_TBC
#pragma warn -rch
#pragma warn -ccc
#endif
globle int genlongfree(
  void *ptr,
  unsigned long size)
  {
#if (! MAC) && (! IBM_TBC) && (! IBM_MSC) && (! IBM_ICB) && (! IBM_ZTC) && (! IBM_SC) && (! IBM_MCW)
   unsigned int test;
#endif
#if BLOCK_MEMORY
   struct longMemoryPtr *theLongMemory;
#endif

   if (sizeof(unsigned int) == sizeof(unsigned long))
     { return(genfree((void *) ptr,(unsigned) size)); }

#if (! MAC) && (! IBM_TBC) && (! IBM_MSC) && (! IBM_ICB) && (! IBM_ZTC) && (! IBM_SC) && (! IBM_MCW)
   test = (unsigned int) size;
   if (test != size) return(-1);

   return(genfree((void *) ptr,(unsigned) test));
#endif

#if BLOCK_MEMORY
   size += sizeof(struct longMemoryPtr);
   theLongMemory = ((struct longMemoryPtr *) ptr) - 1;
   if (theLongMemory->prev == NULL)
     {
      TopLongMemoryPtr = TopLongMemoryPtr->next;
      TopLongMemoryPtr->prev = NULL;
     }
   else
     {
      theLongMemory->prev->next = theLongMemory->next;
      if (theLongMemory->next != NULL)
        { theLongMemory->next->prev = theLongMemory->next; }
     }
#endif

#if MAC || IBM_ICB || IBM_MCW
   MemoryAmount -= size;
   MemoryCalls--;
   SpecialFree(ptr);
   return(0);
#endif

#if IBM_TBC || IBM_ZTC || IBM_SC
   MemoryAmount -= size;
   MemoryCalls--;
   SpecialFree(ptr);
   return(0);
#endif

#if IBM_MSC
   MemoryAmount -= size;
   MemoryCalls--;
   return(SpecialFree(ptr));
#endif
  }
#if IBM_TBC
#pragma warn +rch
#pragma warn +ccc
#endif

/*********************************/
/* MemUsed: C access routine for */
/*   the mem-requests command.   */
/*********************************/
globle long int MemUsed()
  {
   return(MemoryAmount);
  }

/*************************************/
/* MemRequests: C access routine for */
/*   the mem-requests command.       */
/*************************************/
globle long int MemRequests()
  {
   return(MemoryCalls);
  }

/***************************************/
/* UpdateMemoryUsed: Allows the amount */
/*   of memory used to be updated.     */
/***************************************/
globle long int UpdateMemoryUsed(
  long int value)
  {
   MemoryAmount += value;
   return(MemoryAmount);
  }

/*******************************************/
/* UpdateMemoryRequests: Allows the number */
/*   of memory requests to be updated.     */
/*******************************************/
globle long int UpdateMemoryRequests(
  long int value)
  {
   MemoryCalls += value;
   return(MemoryCalls);
  }

/************************************/
/* ReleaseMem: C access routine for */
/*   the release-mem command.       */
/************************************/
globle long int ReleaseMem(
  long int maximum,
  int printMessage)
  {
   struct memoryPtr *tmpPtr, *memPtr;
   int i;
   long int returns = 0;
   long int amount = 0;

   if (printMessage == TRUE)
     { PrintRouter(WDIALOG,"\n*** DEALLOCATING MEMORY ***\n"); }

   for (i = (MEM_TABLE_SIZE - 1) ; i >= sizeof(char *) ; i--)
     {
      YieldTime();
      memPtr = MemoryTable[i];
      while (memPtr != NULL)
        {
         tmpPtr = memPtr->next;
         genfree((void *) memPtr,(unsigned) i);
         memPtr = tmpPtr;
         amount += i;
         returns++;
         if ((returns % 100) == 0)
           { YieldTime(); }
        }
      MemoryTable[i] = NULL;
      if (((amount > maximum) && (maximum > 0)) || HaltExecution)
        {
         if (printMessage == TRUE)
           { PrintRouter(WDIALOG,"*** MEMORY  DEALLOCATED ***\n"); }
         return(amount);
        }
     }

   if (printMessage == TRUE)
     { PrintRouter(WDIALOG,"*** MEMORY  DEALLOCATED ***\n"); }

   return(amount);
  }

/*****************************************************/
/* gm1: Allocates memory and sets all bytes to zero. */
/*****************************************************/
globle void *gm1(
  int size)
  {
   struct memoryPtr *memPtr;
   char *tmpPtr;
   int i;

   if (size < sizeof(char *)) size = sizeof(char *);

   if (size >= MEM_TABLE_SIZE)
     {
      tmpPtr = (char *) genalloc((unsigned) size);
      for (i = 0 ; i < size ; i++)
        { tmpPtr[i] = '\0'; }
      return((void *) tmpPtr);
     }

   memPtr = (struct memoryPtr *) MemoryTable[size];
   if (memPtr == NULL)
     {
      tmpPtr = (char *) genalloc((unsigned) size);
      for (i = 0 ; i < size ; i++)
        { tmpPtr[i] = '\0'; }
      return((void *) tmpPtr);
     }

   MemoryTable[size] = memPtr->next;

   tmpPtr = (char *) memPtr;
   for (i = 0 ; i < size ; i++)
     { tmpPtr[i] = '\0'; }

   return ((void *) tmpPtr);
  }

/*****************************************************/
/* gm2: Allocates memory and does not initialize it. */
/*****************************************************/
globle void *gm2(
  int size)
  {
   struct memoryPtr *memPtr;

   if (size < sizeof(char *)) size = sizeof(char *);

   if (size >= MEM_TABLE_SIZE) return(genalloc((unsigned) size));

   memPtr = (struct memoryPtr *) MemoryTable[size];
   if (memPtr == NULL)
     {
      return(genalloc((unsigned) size));
     }

   MemoryTable[size] = memPtr->next;

   return ((void *) memPtr);
  }

/*****************************************************/
/* gm3: Allocates memory and does not initialize it. */
/*****************************************************/
globle void *gm3(
  long size)
  {
   struct memoryPtr *memPtr;

   if (size < sizeof(char *)) size = sizeof(char *);

   if (size >= MEM_TABLE_SIZE) return(genlongalloc((unsigned long) size));

   memPtr = (struct memoryPtr *) MemoryTable[(int) size];
   if (memPtr == NULL)
     {
      return(genalloc((unsigned int) size));
     }

   MemoryTable[(int) size] = memPtr->next;

   return ((void *) memPtr);
  }

/****************************************/
/* rm: Returns a block of memory to the */
/*   maintained pool of free memory.    */
/****************************************/
globle int rm(
  void *str,
  int size)
  {
   struct memoryPtr *memPtr;

   if (size == 0)
     {
      SystemError("MEMORY",1);
      ExitRouter(EXIT_FAILURE);
     }

   if (size < sizeof(char *)) size = sizeof(char *);

   if (size >= MEM_TABLE_SIZE) return(genfree((void *) str,(unsigned) size));

   memPtr = (struct memoryPtr *) str;
   memPtr->next = MemoryTable[size];
   MemoryTable[size] = memPtr;
   return(1);
  }

/********************************************/
/* rm3: Returns a block of memory to the    */
/*   maintained pool of free memory that's  */
/*   size is indicated with a long integer. */
/********************************************/
globle int rm3(
  void *str,
  long size)
  {
   struct memoryPtr *memPtr;

   if (size == 0)
     {
      SystemError("MEMORY",1);
      ExitRouter(EXIT_FAILURE);
     }

   if (size < sizeof(char *)) size = sizeof(char *);

   if (size >= MEM_TABLE_SIZE) return(genlongfree((void *) str,(unsigned long) size));

   memPtr = (struct memoryPtr *) str;
   memPtr->next = MemoryTable[(int) size];
   MemoryTable[(int) size] = memPtr;
   return(1);
  }

/***************************************************/
/* PoolSize: Returns number of bytes in free pool. */
/***************************************************/
globle unsigned long PoolSize()
  {
   register int i;
   struct memoryPtr *memPtr;
   unsigned long cnt = 0;

   for (i = sizeof(char *) ; i < MEM_TABLE_SIZE ; i++)
     {
      memPtr = MemoryTable[i];
      while (memPtr != NULL)
        {
         cnt += (unsigned long) i;
         memPtr = memPtr->next;
        }
     }
   return(cnt);
  }

/***************************************************************/
/* ActualPoolSize : Returns number of bytes DOS requires to    */
/*   store the free pool.  This routine is functionally        */
/*   equivalent to pool_size on anything other than the IBM-PC */
/***************************************************************/
globle unsigned long ActualPoolSize()
  {
#if IBM_TBC
   register int i;
   struct memoryPtr *memPtr;
   unsigned long cnt = 0;

   for (i = sizeof(char *) ; i < MEM_TABLE_SIZE ; i++)
     {
      memPtr = MemoryTable[i];
      while (memPtr != NULL)
        {
         /*==============================================================*/
         /* For a block of size n, the Turbo-C Library routines require  */
         /* a header of size 8 bytes and further require that all memory */
         /* allotments be paragraph (16-bytes) aligned.                  */
         /*==============================================================*/

         cnt += (((unsigned long) i) + 19L) & 0xfffffff0L;
         memPtr = memPtr->next;
        }
     }
   return(cnt);
#else
   return(PoolSize());
#endif
  }

/********************************************/
/* SetConserveMemory: Allows the setting of */
/*    the memory conservation flag.         */
/********************************************/
globle BOOLEAN SetConserveMemory(
  BOOLEAN value)
  {
   int ov;

   ov = ConserveMemory;
   ConserveMemory = value;
   return(ov);
  }

/*******************************************/
/* GetConserveMemory: Returns the value of */
/*    the memory conservation flag.        */
/*******************************************/
globle BOOLEAN GetConserveMemory()
  {
   return(ConserveMemory);
  }

/**************************/
/* genmemcpy:             */
/**************************/
globle void genmemcpy(
  char *dst,
  char *src,
  unsigned long size)
  {
   unsigned long i;

   for (i = 0L ; i < size ; i++)
     dst[i] = src[i];
  }

/**************************/
/* BLOCK MEMORY FUNCTIONS */
/**************************/

#if BLOCK_MEMORY

/***************************************************/
/* InitializeBlockMemory: Initializes block memory */
/*   management and allocates the first block.     */
/***************************************************/
static int InitializeBlockMemory(
  unsigned int requestSize)
  {
   struct chunkInfo *chunkPtr;
   unsigned int initialBlockSize, usableBlockSize;

   /*===========================================*/
   /* The block memory routines depend upon the */
   /* size of a character being 1 byte.         */
   /*===========================================*/

   if (sizeof(char) != 1)
     {
      fprintf(stdout, "Size of character data is not 1\n");
      fprintf(stdout, "Memory allocation functions may not work\n");
      return(0);
     }

   ChunkInfoSize = sizeof(struct chunkInfo);
   ChunkInfoSize = (((ChunkInfoSize - 1) / STRICT_ALIGN_SIZE) + 1) * STRICT_ALIGN_SIZE;

   BlockInfoSize = sizeof(struct blockInfo);
   BlockInfoSize = (((BlockInfoSize - 1) / STRICT_ALIGN_SIZE) + 1) * STRICT_ALIGN_SIZE;

   initialBlockSize = (INITBLOCKSIZE > requestSize ? INITBLOCKSIZE : requestSize);
   initialBlockSize += ChunkInfoSize * 2 + BlockInfoSize;
   initialBlockSize = (((initialBlockSize - 1) / STRICT_ALIGN_SIZE) + 1) * STRICT_ALIGN_SIZE;

   usableBlockSize = initialBlockSize - (2 * ChunkInfoSize) - BlockInfoSize;

   /* make sure we get a buffer big enough to be usable */
   if ((requestSize < INITBLOCKSIZE) &&
       (usableBlockSize <= requestSize + ChunkInfoSize))
     {
      initialBlockSize = requestSize + ChunkInfoSize * 2 + BlockInfoSize;
      initialBlockSize = (((initialBlockSize - 1) / STRICT_ALIGN_SIZE) + 1) * STRICT_ALIGN_SIZE;
      usableBlockSize = initialBlockSize - (2 * ChunkInfoSize) - BlockInfoSize;
     }

   TopMemoryBlock = (struct blockInfo *) malloc((STD_SIZE) initialBlockSize);

   if (TopMemoryBlock == NULL)
     {
      fprintf(stdout, "Unable to allocate initial memory pool\n");
      return(0);
     }

   TopMemoryBlock->nextBlock = NULL;
   TopMemoryBlock->prevBlock = NULL;
   TopMemoryBlock->nextFree = (struct chunkInfo *) (((char *) TopMemoryBlock) + BlockInfoSize);
   TopMemoryBlock->size = usableBlockSize;

   chunkPtr = (struct chunkInfo *) (((char *) TopMemoryBlock) + BlockInfoSize + ChunkInfoSize + usableBlockSize);
   chunkPtr->nextFree = NULL;
   chunkPtr->lastFree = NULL;
   chunkPtr->prevChunk = TopMemoryBlock->nextFree;
   chunkPtr->size = 0;

   TopMemoryBlock->nextFree->nextFree = NULL;
   TopMemoryBlock->nextFree->lastFree = NULL;
   TopMemoryBlock->nextFree->prevChunk = NULL;
   TopMemoryBlock->nextFree->size = usableBlockSize;

   BlockMemoryInitialized = TRUE;
   return(1);
  }

/***************************************************************************/
/* AllocateBlock: Adds a new block of memory to the list of memory blocks. */
/***************************************************************************/
static int AllocateBlock(
  struct blockInfo *blockPtr,
  unsigned int requestSize)
  {
   unsigned int blockSize, usableBlockSize;
   struct blockInfo *newBlock;
   struct chunkInfo *newTopChunk;

   /*============================================================*/
   /* Determine the size of the block that needs to be allocated */
   /* to satisfy the request. Normally, a default block size is  */
   /* used, but if the requested size is larger than the default */
   /* size, then the requested size is used for the block size.  */
   /*============================================================*/

   blockSize = (BLOCKSIZE > requestSize ? BLOCKSIZE : requestSize);
   blockSize += BlockInfoSize + ChunkInfoSize * 2;
   blockSize = (((blockSize - 1) / STRICT_ALIGN_SIZE) + 1) * STRICT_ALIGN_SIZE;

   usableBlockSize = blockSize - BlockInfoSize - (2 * ChunkInfoSize);

   /*=========================*/
   /* Allocate the new block. */
   /*=========================*/

   newBlock = (struct blockInfo *) malloc((STD_SIZE) blockSize);
   if (newBlock == NULL) return(0);

   /*======================================*/
   /* Initialize the block data structure. */
   /*======================================*/

   newBlock->nextBlock = NULL;
   newBlock->prevBlock = blockPtr;
   newBlock->nextFree = (struct chunkInfo *) (((char *) newBlock) + BlockInfoSize);
   newBlock->size = usableBlockSize;
   blockPtr->nextBlock = newBlock;

   newTopChunk = (struct chunkInfo *) (((char *) newBlock) + BlockInfoSize + ChunkInfoSize + usableBlockSize);
   newTopChunk->nextFree = NULL;
   newTopChunk->lastFree = NULL;
   newTopChunk->size = 0;
   newTopChunk->prevChunk = newBlock->nextFree;

   newBlock->nextFree->nextFree = NULL;
   newBlock->nextFree->lastFree = NULL;
   newBlock->nextFree->prevChunk = NULL;
   newBlock->nextFree->size = usableBlockSize;

   return(1);
  }

/*******************************************************/
/* RequestChunk: Allocates memory by returning a chunk */
/*   of memory from a larger block of memory.          */
/*******************************************************/
globle void *RequestChunk(
  unsigned int requestSize)
  {
   struct chunkInfo *chunkPtr;
   struct blockInfo *blockPtr;

   /*==================================================*/
   /* Allocate initial memory pool block if it has not */
   /* already been allocated.                          */
   /*==================================================*/

   if (BlockMemoryInitialized == FALSE)
      {
       if (InitializeBlockMemory(requestSize) == 0) return(NULL);
       AddRouter("bmexit",-2000,NULL,NULL,NULL,NULL,BlockMemoryExitFunction);
      }

   /*====================================================*/
   /* Make sure that the amount of memory requested will */
   /* fall on a boundary of strictest alignment          */
   /*====================================================*/

   requestSize = (((requestSize - 1) / STRICT_ALIGN_SIZE) + 1) * STRICT_ALIGN_SIZE;

   /*=====================================================*/
   /* Search through the list of free memory for a block  */
   /* of the appropriate size.  If a block is found, then */
   /* allocate and return a pointer to it.                */
   /*=====================================================*/

   blockPtr = TopMemoryBlock;

   while (blockPtr != NULL)
     {
      chunkPtr = blockPtr->nextFree;

      while (chunkPtr != NULL)
        {
         if ((chunkPtr->size == requestSize) ||
             (chunkPtr->size > (requestSize + ChunkInfoSize)))
           {
            AllocateChunk(blockPtr,chunkPtr,requestSize);

            return((void *) (((char *) chunkPtr) + ChunkInfoSize));
           }
         chunkPtr = chunkPtr->nextFree;
        }

      if (blockPtr->nextBlock == NULL)
        {
         if (AllocateBlock(blockPtr,requestSize) == 0)  /* get another block */
           { return(NULL); }
        }
      blockPtr = blockPtr->nextBlock;
     }

   SystemError("MEMORY",2);
   ExitRouter(EXIT_FAILURE);
   return(NULL); /* Unreachable, but prevents warning. */
  }

/********************************************/
/* AllocateChunk: Allocates a chunk from an */
/*   existing chunk in a block of memory.   */
/********************************************/
static void AllocateChunk(
  struct blockInfo *parentBlock,
  struct chunkInfo *chunkPtr,
  unsigned int requestSize)
  {
   struct chunkInfo *splitChunk, *nextChunk;

   /*=============================================================*/
   /* If the size of the memory chunk is an exact match for the   */
   /* requested amount of memory, then the chunk can be allocated */
   /* without splitting it.                                       */
   /*=============================================================*/

   if (requestSize == chunkPtr->size)
     {
      chunkPtr->size = - (long int) requestSize;
      if (chunkPtr->lastFree == NULL)
        {
         if (chunkPtr->nextFree != NULL)
           { parentBlock->nextFree = chunkPtr->nextFree; }
         else
           { parentBlock->nextFree = NULL; }
        }
      else
        { chunkPtr->lastFree->nextFree = chunkPtr->nextFree; }

      if (chunkPtr->nextFree != NULL)
        { chunkPtr->nextFree->lastFree = chunkPtr->lastFree; }

      chunkPtr->lastFree = NULL;
      chunkPtr->nextFree = NULL;
      return;
     }

   /*===========================================================*/
   /* If the size of the memory chunk is larger than the memory */
   /* request, then split the chunk into two pieces.            */
   /*===========================================================*/

   nextChunk = (struct chunkInfo *)
              (((char *) chunkPtr) + ChunkInfoSize + chunkPtr->size);

   splitChunk = (struct chunkInfo *)
                  (((char *) chunkPtr) + (ChunkInfoSize + requestSize));

   splitChunk->size = chunkPtr->size - (requestSize + ChunkInfoSize);
   splitChunk->prevChunk = chunkPtr;

   splitChunk->nextFree = chunkPtr->nextFree;
   splitChunk->lastFree = chunkPtr->lastFree;

   nextChunk->prevChunk = splitChunk;

   if (splitChunk->lastFree == NULL)
     { parentBlock->nextFree = splitChunk; }
   else
     { splitChunk->lastFree->nextFree = splitChunk; }

   if (splitChunk->nextFree != NULL)
     { splitChunk->nextFree->lastFree = splitChunk; }

   chunkPtr->size = - (long int) requestSize;
   chunkPtr->lastFree = NULL;
   chunkPtr->nextFree = NULL;

   return;
  }

/***********************************************************/
/* ReturnChunk: Frees memory allocated using RequestChunk. */
/***********************************************************/
globle int ReturnChunk(
  void *memPtr,
  unsigned int size)
  {
   struct chunkInfo *chunkPtr, *lastChunk, *nextChunk, *topChunk;
   struct blockInfo *blockPtr;

   /*=====================================================*/
   /* Determine if the expected size of the chunk matches */
   /* the size stored in the chunk's information record.  */
   /*=====================================================*/

   size = (((size - 1) / STRICT_ALIGN_SIZE) + 1) * STRICT_ALIGN_SIZE;

   chunkPtr = (struct chunkInfo *) (((char *) memPtr) - ChunkInfoSize);

   if (chunkPtr == NULL)
     { return(FALSE); }

   if (chunkPtr->size >= 0)
     { return(FALSE); }

   if (chunkPtr->size != - (long int) size)
     { return(FALSE); }

   chunkPtr->size = - chunkPtr->size;

   /*=============================================*/
   /* Determine in which block the chunk resides. */
   /*=============================================*/

   topChunk = chunkPtr;
   while (topChunk->prevChunk != NULL)
     { topChunk = topChunk->prevChunk; }
   blockPtr = (struct blockInfo *) (((char *) topChunk) - BlockInfoSize);

   /*===========================================*/
   /* Determine the chunks physically preceding */
   /* and following the returned chunk.         */
   /*===========================================*/

   lastChunk = chunkPtr->prevChunk;
   nextChunk = (struct chunkInfo *) (((char *) memPtr) + size);

   /*=========================================================*/
   /* Add the chunk to the list of free chunks for the block. */
   /*=========================================================*/

   if (blockPtr->nextFree != NULL)
     { blockPtr->nextFree->lastFree = chunkPtr; }

   chunkPtr->nextFree = blockPtr->nextFree;
   chunkPtr->lastFree = NULL;

   blockPtr->nextFree = chunkPtr;

   /*=====================================================*/
   /* Combine this chunk with previous chunk if possible. */
   /*=====================================================*/

   if (lastChunk != NULL)
     {
      if (lastChunk->size > 0)
        {
         lastChunk->size += (ChunkInfoSize + chunkPtr->size);

         if (nextChunk != NULL)
           { nextChunk->prevChunk = lastChunk; }
         else
           { return(FALSE); }

         if (lastChunk->lastFree != NULL)
           { lastChunk->lastFree->nextFree = lastChunk->nextFree; }

         if (lastChunk->nextFree != NULL)
           { lastChunk->nextFree->lastFree = lastChunk->lastFree; }

         lastChunk->nextFree = chunkPtr->nextFree;
         if (chunkPtr->nextFree != NULL)
           { chunkPtr->nextFree->lastFree = lastChunk; }
         lastChunk->lastFree = NULL;

         blockPtr->nextFree = lastChunk;
         chunkPtr->lastFree = NULL;
         chunkPtr->nextFree = NULL;
         chunkPtr = lastChunk;
        }
     }

   /*=====================================================*/
   /* Combine this chunk with the next chunk if possible. */
   /*=====================================================*/

   if (nextChunk == NULL) return(FALSE);
   if (chunkPtr == NULL) return(FALSE);

   if (nextChunk->size > 0)
     {
      chunkPtr->size += (ChunkInfoSize + nextChunk->size);

      topChunk = (struct chunkInfo *) (((char *) nextChunk) + nextChunk->size + ChunkInfoSize);
      if (topChunk != NULL)
        { topChunk->prevChunk = chunkPtr; }
      else
        { return(FALSE); }

      if (nextChunk->lastFree != NULL)
        { nextChunk->lastFree->nextFree = nextChunk->nextFree; }

      if (nextChunk->nextFree != NULL)
        { nextChunk->nextFree->lastFree = nextChunk->lastFree; }

     }

   /*===========================================*/
   /* Free the buffer if we can, but don't free */
   /* the first buffer if it's the only one.    */
   /*===========================================*/

   if ((chunkPtr->prevChunk == NULL) &&
       (chunkPtr->size == blockPtr->size))
     {
      if (blockPtr->prevBlock != NULL)
        {
         blockPtr->prevBlock->nextBlock = blockPtr->nextBlock;
         if (blockPtr->nextBlock != NULL)
           { blockPtr->nextBlock->prevBlock = blockPtr->prevBlock; }
         free((char *) blockPtr);
        }
      else
        {
         if (blockPtr->nextBlock != NULL)
           {
            blockPtr->nextBlock->prevBlock = NULL;
            TopMemoryBlock = blockPtr->nextBlock;
            free((char *) blockPtr);
           }
        }
     }

   return(TRUE);
  }

/***********************************************/
/* ReturnAllBlocks: Frees all allocated blocks */
/*   back to the operating system.             */
/***********************************************/
static void ReturnAllBlocks()
  {
   struct blockInfo *theBlock, *nextBlock;
   struct longMemoryPtr *theLongMemory, *nextLongMemory;

   /*======================================*/
   /* Free up int based memory allocation. */
   /*======================================*/

   theBlock = TopMemoryBlock;
   while (theBlock != NULL)
     {
      nextBlock = theBlock->nextBlock;
      free((char *) theBlock);
      theBlock = nextBlock;
     }

   TopMemoryBlock = NULL;

   /*=======================================*/
   /* Free up long based memory allocation. */
   /*=======================================*/

   theLongMemory = TopLongMemoryPtr;
   while (theLongMemory != NULL)
     {
      nextLongMemory = theLongMemory->next;
      genlongfree(theLongMemory,theLongMemory->size);
      theLongMemory = nextLongMemory;
     }

   TopLongMemoryPtr = NULL;
  }

/***************************************/
/* BlockMemoryExitFunction: Routine to */
/*   free block memory when exiting.   */
/***************************************/
static int BlockMemoryExitFunction(
  int num)
  {
   ReturnAllBlocks();
   return(1);
  }
#endif

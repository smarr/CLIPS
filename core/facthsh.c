   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  05/17/06            */
   /*                                                     */
   /*                 FACT HASHING MODULE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides routines for maintaining a fact hash    */
/*   table so that duplication of facts can quickly be       */
/*   determined.                                             */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed LOGICAL_DEPENDENCIES compilation flag. */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#define _FACTHSH_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <stdlib.h>

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT

#include "constant.h"
#include "memalloc.h"
#include "router.h"
#include "sysdep.h"
#include "envrnmnt.h"

#if DEFRULE_CONSTRUCT
#include "lgcldpnd.h"
#endif

#include "facthsh.h"

# include <apr_thread_rwlock.h>

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static struct fact            *FactExists(void *,EXEC_STATUS,struct fact *,unsigned long);
   static struct factHashEntry  **CreateFactHashTable(void *,EXEC_STATUS,unsigned long);
   static void                    ResizeFactHashTable(void *,EXEC_STATUS);
   static void                    ResetFactHashTable(void *,EXEC_STATUS);
   
/************************************************/
/* HashFact: Returns the hash value for a fact. */
/************************************************/
unsigned long HashFact(
  struct fact *theFact)
  {
   unsigned long count = 0;

   /*============================================*/
   /* Get a hash value for the deftemplate name. */
   /*============================================*/

   count += (unsigned long) theFact->whichDeftemplate->header.name->bucket * 73981;

   /*=================================================*/
   /* Add in the hash value for the rest of the fact. */
   /*=================================================*/

   count += HashMultifield(&theFact->theProposition,0);

   /*================================*/
   /* Make sure the hash value falls */
   /* in the appropriate range.      */
   /*================================*/

   theFact->hashValue = count;

   /*========================*/
   /* Return the hash value. */
   /*========================*/

   return(count);
  }

/**********************************************/
/* FactExists: Determines if a specified fact */
/*   already exists in the fact hash table.   */
/**********************************************/
static struct fact *FactExists(
  void *theEnv,
  EXEC_STATUS,
  struct fact *theFact,
  unsigned long hashValue)
  {
   struct factHashEntry *theFactHash;

   // Lode: warning! not thread-safe but only called from HandleFactDuplication and that one _is_ safe
   hashValue = (hashValue % FactData(theEnv,execStatus)->FactHashTableSize);

   for (theFactHash = FactData(theEnv,execStatus)->FactHashTable[hashValue];
        theFactHash != NULL;
        theFactHash = theFactHash->next)
     {
      if (theFact->hashValue != theFactHash->theFact->hashValue)
        { continue; }

      if ((theFact->whichDeftemplate == theFactHash->theFact->whichDeftemplate) ?
          MultifieldsEqual(&theFact->theProposition,
                           &theFactHash->theFact->theProposition) : FALSE)
        { return(theFactHash->theFact); }
     }
	  
   return(NULL);
  }

/************************************************************/
/* AddHashedFact: Adds a fact entry to the fact hash table. */
/************************************************************/
globle void AddHashedFact(
  void *theEnv,
  EXEC_STATUS,
  struct fact *theFact,
  unsigned long hashValue)
  {
   struct factHashEntry *newhash, *temp;
	  
	// Lode: Add a write lock to the FactHashTable
	// XXX: could be removed?
	apr_thread_rwlock_wrlock(Env(theEnv,execStatus)->factHashLock);
	  
   if (FactData(theEnv,execStatus)->NumberOfFacts > FactData(theEnv,execStatus)->FactHashTableSize)
     { ResizeFactHashTable(theEnv,execStatus); }

   newhash = get_struct(theEnv,execStatus,factHashEntry);
   newhash->theFact = theFact;
	  
    
   hashValue = (hashValue % FactData(theEnv,execStatus)->FactHashTableSize);  // not thread-safe, could be resized...
   
   temp = FactData(theEnv,execStatus)->FactHashTable[hashValue];
   FactData(theEnv,execStatus)->FactHashTable[hashValue] = newhash;
   newhash->next = temp;
	  
   // Lode: Unlock
   apr_thread_rwlock_unlock(Env(theEnv,execStatus)->factHashLock);
	  
  }

/******************************************/
/* RemoveHashedFact: Removes a fact entry */
/*   from the fact hash table.            */
/******************************************/
globle intBool RemoveHashedFact(
  void *theEnv,
  EXEC_STATUS,
  struct fact *theFact)
  {
   unsigned long hashValue;
   struct factHashEntry *hptr, *prev;
	  
   hashValue = HashFact(theFact);
	  
   // Lode: Add a write lock to the FactHashTable
   apr_thread_rwlock_wrlock(Env(theEnv,execStatus)->factHashLock);
	  
   hashValue = (hashValue % FactData(theEnv,execStatus)->FactHashTableSize);

   for (hptr = FactData(theEnv,execStatus)->FactHashTable[hashValue], prev = NULL;
        hptr != NULL;
        hptr = hptr->next)
     {
      if (hptr->theFact == theFact)
        {
         if (prev == NULL)
           {
            FactData(theEnv,execStatus)->FactHashTable[hashValue] = hptr->next;
            rtn_struct(theEnv,execStatus,factHashEntry,hptr);
            if (FactData(theEnv,execStatus)->NumberOfFacts == 1)
              { ResetFactHashTable(theEnv,execStatus); }
			apr_thread_rwlock_unlock(Env(theEnv,execStatus)->factHashLock); // Lode: Unlock
            return(1);
           }
         else
           {
            prev->next = hptr->next;
            rtn_struct(theEnv,execStatus,factHashEntry,hptr);
            if (FactData(theEnv,execStatus)->NumberOfFacts == 1)
              { ResetFactHashTable(theEnv,execStatus); }
			apr_thread_rwlock_unlock(Env(theEnv,execStatus)->factHashLock); // Lode: Unlock
            return(1);
           }
        }
      prev = hptr;
     }
   
   // Lode: Unlock FactHashTable
   apr_thread_rwlock_unlock(Env(theEnv,execStatus)->factHashLock);
   
   return(0);
  }

/*****************************************************/
/* HandleFactDuplication: Determines if a fact to be */
/*   added to the fact-list is a duplicate entry and */
/*   takes appropriate action based on the current   */
/*   setting of the fact-duplication flag.           */
/*****************************************************/
globle unsigned long HandleFactDuplication(
  void *theEnv,
  EXEC_STATUS,
  void *theFact,
  intBool *duplicate)
  {
   struct fact *tempPtr;
   unsigned long hashValue;
   *duplicate = FALSE;
	  
   // Lode: Add a read lock to the FactHashTable
   // XXX: could be removed?
   apr_thread_rwlock_rdlock(Env(theEnv,execStatus)->factHashLock);
	  
   hashValue = HashFact((struct fact *) theFact);

   if (FactData(theEnv,execStatus)->FactDuplication) return(hashValue);

   tempPtr = FactExists(theEnv,execStatus,(struct fact *) theFact,hashValue);
	  if (tempPtr == NULL) {
		  apr_thread_rwlock_unlock(Env(theEnv,execStatus)->factHashLock); // Lode: Unlock
		  return(hashValue);
	  }
	  
	// Lode: Unlock FactHashTable
	apr_thread_rwlock_unlock(Env(theEnv,execStatus)->factHashLock);
	  
   // Lode: TODO check thread-safety
   ReturnFact(theEnv,execStatus,(struct fact *) theFact);
#if DEFRULE_CONSTRUCT
   AddLogicalDependencies(theEnv,execStatus,(struct patternEntity *) tempPtr,TRUE);
#endif
   *duplicate = TRUE;
	  
   return(0);
  }

/*******************************************/
/* EnvGetFactDuplication: C access routine */
/*   for the get-fact-duplication command. */
/*******************************************/
globle intBool EnvGetFactDuplication(
  void *theEnv,
  EXEC_STATUS)
  {   
   return(FactData(theEnv,execStatus)->FactDuplication); 
  }

/*******************************************/
/* EnvSetFactDuplication: C access routine */
/*   for the set-fact-duplication command. */
/*******************************************/
globle intBool EnvSetFactDuplication(
  void *theEnv,
  EXEC_STATUS,
  int value)
  {
   int ov;

   ov = FactData(theEnv,execStatus)->FactDuplication;
   FactData(theEnv,execStatus)->FactDuplication = value;
   return(ov);
  }

/**************************************************/
/* InitializeFactHashTable: Initializes the table */
/*   entries in the fact hash table to NULL.      */
/**************************************************/
globle void InitializeFactHashTable(
   void *theEnv,
  EXEC_STATUS)
   {
    FactData(theEnv,execStatus)->FactHashTable = CreateFactHashTable(theEnv,execStatus,SIZE_FACT_HASH);
    FactData(theEnv,execStatus)->FactHashTableSize = SIZE_FACT_HASH;
   }

/*******************************************************************/
/* CreateFactHashTable: Creates and initializes a fact hash table. */
/*******************************************************************/
static struct factHashEntry **CreateFactHashTable(
   void *theEnv,
  EXEC_STATUS,
   unsigned long tableSize)
   {
    unsigned long i;
    struct factHashEntry **theTable;

    theTable = (struct factHashEntry **)
               gm3(theEnv,execStatus,sizeof (struct factHashEntry *) * tableSize);

    if (theTable == NULL) EnvExitRouter(theEnv,execStatus,EXIT_FAILURE);
    
    for (i = 0; i < tableSize; i++) theTable[i] = NULL;
    
    return(theTable);
   }
 
/*******************************************************************/
/* ResizeFactHashTable: */
/*******************************************************************/
static void ResizeFactHashTable(
   void *theEnv,
  EXEC_STATUS)
   {
    unsigned long i, newSize, newLocation;
    struct factHashEntry **theTable, **newTable;
    struct factHashEntry *theEntry, *nextEntry;
	   
	// Lode: warning! not thread-safe but only called from AddHashedFact and that one _is_ safe
    theTable = FactData(theEnv,execStatus)->FactHashTable;
    
    newSize = (FactData(theEnv,execStatus)->FactHashTableSize * 2) + 1;
    newTable = CreateFactHashTable(theEnv,execStatus,newSize);

    /*========================================*/
    /* Copy the old entries to the new table. */
    /*========================================*/
    
    for (i = 0; i < FactData(theEnv,execStatus)->FactHashTableSize; i++)
      {
       theEntry = theTable[i];
       while (theEntry != NULL)
         { 
          nextEntry = theEntry->next;
          
          newLocation = theEntry->theFact->hashValue % newSize;
          theEntry->next = newTable[newLocation];
          newTable[newLocation] = theEntry;
          
          theEntry = nextEntry;
         }
      }
    
    /*=====================================================*/
    /* Replace the old hash table with the new hash table. */
    /*=====================================================*/
    
    rm3(theEnv,execStatus,theTable,sizeof(struct factHashEntry *) * FactData(theEnv,execStatus)->FactHashTableSize);
    FactData(theEnv,execStatus)->FactHashTableSize = newSize;
    FactData(theEnv,execStatus)->FactHashTable = newTable;
   }

/*******************************************************************/
/* ResetFactHashTable: */
/*******************************************************************/
static void ResetFactHashTable(
   void *theEnv,
  EXEC_STATUS)
   {
    struct factHashEntry **newTable;

    /*=============================================*/
    /* Don't reset the table unless the hash table */
    /* has been expanded from its original size.   */
    /*=============================================*/
    
    if (FactData(theEnv,execStatus)->FactHashTableSize == SIZE_FACT_HASH)
      { return; }
          
    /*=======================*/
    /* Create the new table. */
    /*=======================*/
    
    newTable = CreateFactHashTable(theEnv,execStatus,SIZE_FACT_HASH);
    
    /*=====================================================*/
    /* Replace the old hash table with the new hash table. */
    /*=====================================================*/
    
    rm3(theEnv,execStatus,FactData(theEnv,execStatus)->FactHashTable,sizeof(struct factHashEntry *) * FactData(theEnv,execStatus)->FactHashTableSize);
    FactData(theEnv,execStatus)->FactHashTableSize = SIZE_FACT_HASH;
    FactData(theEnv,execStatus)->FactHashTable = newTable;
   }
      
#if DEVELOPER

/*****************************************************/
/* ShowFactHashTable: Displays the number of entries */
/*   in each slot of the fact hash table.            */
/*****************************************************/
globle void ShowFactHashTable(
   void *theEnv,
  EXEC_STATUS)
   {
    int i, count;
    struct factHashEntry *theEntry;
    char buffer[20];

    for (i = 0; i < FactData(theEnv,execStatus)->FactHashTableSize; i++)
      {
       for (theEntry =  FactData(theEnv,execStatus)->FactHashTable[i], count = 0;
            theEntry != NULL;
            theEntry = theEntry->next)
         { count++; }

       if (count != 0)
         {
          gensprintf(buffer,"%4d: %4d\n",i,count);
          EnvPrintRouter(theEnv,execStatus,WDISPLAY,buffer);
         }
      }
   }

#endif /* DEVELOPER */

#endif /* DEFTEMPLATE_CONSTRUCT */


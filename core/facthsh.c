   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
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
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
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

#if DEFRULE_CONSTRUCT
#include "lgcldpnd.h"
#endif

#include "facthsh.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static struct fact            *FactExists(struct fact *,int);

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static struct factHashEntry  **FactHashTable;
   Thread static BOOLEAN                 FactDuplication = FALSE;

/************************************************/
/* HashFact: Returns the hash value for a fact. */
/************************************************/
int HashFact(
  struct fact *theFact)
  {
   int count = 0;
   int hashValue;

   /*============================================*/
   /* Get a hash value for the deftemplate name. */
   /*============================================*/

   count += HashSymbol(ValueToString(theFact->whichDeftemplate->header.name),
                       SIZE_FACT_HASH);

   /*=================================================*/
   /* Add in the hash value for the rest of the fact. */
   /*=================================================*/

   count += (int) HashMultifield(&theFact->theProposition,SIZE_FACT_HASH);

   /*================================*/
   /* Make sure the hash value falls */
   /* in the appropriate range.      */
   /*================================*/

   hashValue = (int) (count % SIZE_FACT_HASH);
   if (hashValue < 0) hashValue = - hashValue;

   /*========================*/
   /* Return the hash value. */
   /*========================*/

   return(hashValue);
  }

/**********************************************/
/* FactExists: Determines if a specified fact */
/*   already exists in the fact hash table.   */
/**********************************************/
static struct fact *FactExists(
  struct fact *theFact,
  int hashValue)
  {
   struct factHashEntry *theFactHash;

   for (theFactHash = FactHashTable[hashValue];
        theFactHash != NULL;
        theFactHash = theFactHash->next)
     {
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
  struct fact *theFact,
  int hashValue)
  {
   struct factHashEntry *newhash, *temp;

   newhash = get_struct(factHashEntry);
   newhash->theFact = theFact;

   temp = FactHashTable[hashValue];
   FactHashTable[hashValue] = newhash;
   newhash->next = temp;
  }

/******************************************/
/* RemoveHashedFact: Removes a fact entry */
/*   from the fact hash table.            */
/******************************************/
globle BOOLEAN RemoveHashedFact(
  struct fact *theFact)
  {
   int hashValue;
   struct factHashEntry *hptr, *prev;

   hashValue = HashFact(theFact);

   for (hptr = FactHashTable[hashValue], prev = NULL;
        hptr != NULL;
        hptr = hptr->next)
     {
      if (hptr->theFact == theFact)
        {
         if (prev == NULL)
           {
            FactHashTable[hashValue] = hptr->next;
            rtn_struct(factHashEntry,hptr);
            return(1);
           }
         else
           {
            prev->next = hptr->next;
            rtn_struct(factHashEntry,hptr);
            return(1);
           }
        }
      prev = hptr;
     }

   return(0);
  }

/*****************************************************/
/* HandleFactDuplication: Determines if a fact to be */
/*   added to the fact-list is a duplicate entry and */
/*   takes appropriate action based on the current   */
/*   setting of the fact-duplication flag.           */
/*****************************************************/
globle int HandleFactDuplication(
  void *theFact)
  {
   struct fact *tempPtr;
   int hashValue;

   hashValue = HashFact((struct fact *) theFact);

   if (FactDuplication) return(hashValue);

   tempPtr = FactExists((struct fact *) theFact,hashValue);
   if (tempPtr == NULL) return(hashValue);

   ReturnFact((struct fact *) theFact);
#if LOGICAL_DEPENDENCIES && DEFRULE_CONSTRUCT
   AddLogicalDependencies((struct patternEntity *) tempPtr,TRUE);
#endif
   return(-1);
  }

/********************************************/
/* GetFactDuplication: C access routine for */
/*   the get-fact-duplication command.      */
/********************************************/
globle BOOLEAN GetFactDuplication()
  { return(FactDuplication); }

/********************************************/
/* SetFactDuplication: C access routine for */
/*   the set-fact-duplication command.      */
/********************************************/
globle BOOLEAN SetFactDuplication(
  int value)
  {
   int ov;

   ov = FactDuplication;
   FactDuplication = value;
   return(ov);
  }

/**************************************************/
/* InitializeFactHashTable: Initializes the table */
/*   entries in the fact hash table to NULL.      */
/**************************************************/
globle void InitializeFactHashTable()
   {
    int i;

    FactHashTable = (struct factHashEntry **)
                    gm2((int) sizeof (struct factHashEntry *) * SIZE_FACT_HASH);

    if (FactHashTable == NULL) ExitRouter(EXIT_FAILURE);

    for (i = 0; i < SIZE_FACT_HASH; i++) FactHashTable[i] = NULL;
   }

#if DEVELOPER

/*****************************************************/
/* ShowFactHashTable: Displays the number of entries */
/*   in each slot of the fact hash table.            */
/*****************************************************/
globle void ShowFactHashTable()
   {
    int i, count;
    struct factHashEntry *theEntry;
    char buffer[20];

    for (i = 0; i < SIZE_FACT_HASH; i++)
      {
       for (theEntry =  FactHashTable[i], count = 0;
            theEntry != NULL;
            theEntry = theEntry->next)
         { count++; }

       if (count != 0)
         {
          sprintf(buffer,"%4d: %4d\n",i,count);
          PrintRouter(WDISPLAY,buffer);
         }
      }
   }

#endif /* DEVELOPER */

#endif /* DEFTEMPLATE_CONSTRUCT */


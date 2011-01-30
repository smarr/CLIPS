   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
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
/*************************************************************/

#ifndef _H_facthsh

#define _H_facthsh

struct factHashEntry;

#ifndef _H_factmngr
#include "factmngr.h"
#endif

struct factHashEntry
  {
   struct fact *theFact;
   struct factHashEntry *next;
  };

#define SIZE_FACT_HASH  1013

#ifdef LOCALE
#undef LOCALE
#endif
#ifdef _FACTHSH_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           AddHashedFact(struct fact *,int);
   LOCALE BOOLEAN                        RemoveHashedFact(struct fact *);
   LOCALE int                            HandleFactDuplication(void *);
   LOCALE DllExport BOOLEAN              GetFactDuplication(void);
   LOCALE DllExport BOOLEAN              SetFactDuplication(int);
   LOCALE void                           InitializeFactHashTable(void);
   LOCALE void                           ShowFactHashTable(void);
   LOCALE int                            HashFact(struct fact *);

#endif






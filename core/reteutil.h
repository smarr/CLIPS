   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  10/19/06            */
   /*                                                     */
   /*              RETE UTILITY HEADER FILE               */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides a set of utility functions useful to    */
/*   other modules.                                          */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Rule with exists CE has incorrect activation.  */
/*            DR0867                                         */
/*                                                           */
/*      6.30: Added support for hashed alpha memories.       */
/*                                                           */
/*************************************************************/

#ifndef _H_reteutil
#define _H_reteutil

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_match
#include "match.h"
#endif
#ifndef _H_network
#include "network.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _RETEUTIL_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           PrintPartialMatch(void *,EXEC_STATUS,char *,struct partialMatch *);
   LOCALE struct partialMatch           *CopyPartialMatch(void *,EXEC_STATUS,struct partialMatch *);
   LOCALE struct partialMatch           *MergePartialMatches(void *,EXEC_STATUS,struct partialMatch *,struct partialMatch *);
   LOCALE long int                       IncrementPseudoFactIndex(void);
   LOCALE struct partialMatch           *GetAlphaMemory(void *,EXEC_STATUS,struct patternNodeHeader *,unsigned long);
   LOCALE struct partialMatch           *GetLeftBetaMemory(struct joinNode *,unsigned long);
   LOCALE struct partialMatch           *GetRightBetaMemory(struct joinNode *,unsigned long);
   LOCALE void                           ReturnLeftMemory(void *,EXEC_STATUS,struct joinNode *);
   LOCALE void                           ReturnRightMemory(void *,EXEC_STATUS,struct joinNode *);
   LOCALE void                           DestroyBetaMemory(void *,EXEC_STATUS,struct joinNode *,int);
   LOCALE void                           FlushBetaMemory(void *,EXEC_STATUS,struct joinNode *,int);
   LOCALE intBool                        BetaMemoryNotEmpty(struct joinNode *);
   LOCALE void                           RemoveAlphaMemoryMatches(void *,EXEC_STATUS,struct patternNodeHeader *,struct partialMatch *,
                                                                  struct alphaMatch *); 
   LOCALE void                           DestroyAlphaMemory(void *,EXEC_STATUS,struct patternNodeHeader *,int);
   LOCALE void                           FlushAlphaMemory(void *,EXEC_STATUS,struct patternNodeHeader *);
   LOCALE void                           FlushAlphaBetaMemory(void *,EXEC_STATUS,struct partialMatch *);
   LOCALE void                           DestroyAlphaBetaMemory(void *,EXEC_STATUS,struct partialMatch *);
   LOCALE int                            GetPatternNumberFromJoin(struct joinNode *);
   LOCALE struct multifieldMarker       *CopyMultifieldMarkers(void *,EXEC_STATUS,struct multifieldMarker *);
   LOCALE struct partialMatch           *CreateAlphaMatch(void *,EXEC_STATUS,void *,struct multifieldMarker *,
                                                          struct patternNodeHeader *,unsigned long);
   LOCALE void                           TraceErrorToRule(void *,EXEC_STATUS,struct joinNode *,char *);
   LOCALE void                           InitializePatternHeader(void *,EXEC_STATUS,struct patternNodeHeader *);
   LOCALE void                           MarkRuleNetwork(void *,EXEC_STATUS,int);
   LOCALE void                           TagRuleNetwork(void *,EXEC_STATUS,long *,long *,long *,long *);
   LOCALE int                            FindEntityInPartialMatch(struct patternEntity *,struct partialMatch *);
   LOCALE unsigned long                  ComputeRightHashValue(void *,EXEC_STATUS,struct patternNodeHeader *);
   LOCALE void                           UpdateBetaPMLinks(void *,EXEC_STATUS,struct partialMatch *,struct partialMatch *,struct partialMatch *,
                                                       struct joinNode *,unsigned long,int);
   LOCALE void                           UnlinkBetaPMFromNodeAndLineage(void *,EXEC_STATUS,struct joinNode *,struct partialMatch *,int);
   LOCALE void                           UnlinkNonLeftLineage(void *,EXEC_STATUS,struct joinNode *,struct partialMatch *,int);
   LOCALE struct partialMatch           *CreateEmptyPartialMatch(void *,EXEC_STATUS);
   LOCALE void                           MarkRuleJoins(struct joinNode *,int);
   LOCALE void                           AddBlockedLink(struct partialMatch *,struct partialMatch *);
   LOCALE void                           RemoveBlockedLink(struct partialMatch *);
#endif




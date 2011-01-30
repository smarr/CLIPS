   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
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

   LOCALE void                           PrintPartialMatch(char *,struct partialMatch *);
   LOCALE struct partialMatch           *CopyPartialMatch(struct partialMatch *,int,int);
   LOCALE struct partialMatch           *MergePartialMatches(struct partialMatch *,struct partialMatch *,int,int);
   LOCALE struct partialMatch           *AddSingleMatch(struct partialMatch *,struct alphaMatch *,int,int);
   LOCALE struct partialMatch           *NewPseudoFactPartialMatch(void);
   LOCALE long int                       IncrementPseudoFactIndex(void);
   LOCALE void                           FlushAlphaBetaMemory(struct partialMatch *);
   LOCALE int                            GetPatternNumberFromJoin(struct joinNode *);
   LOCALE void                           PrimeJoin(struct joinNode *);
   LOCALE struct multifieldMarker       *CopyMultifieldMarkers(struct multifieldMarker *);
   LOCALE struct partialMatch           *CreateAlphaMatch(void *,struct multifieldMarker *,
                                                       struct patternNodeHeader *);
   LOCALE void                           TraceErrorToRule(struct joinNode *,char *);
   LOCALE void                           InitializePatternHeader(struct patternNodeHeader *);
   LOCALE void                           MarkRuleNetwork(int);
   LOCALE void                           TagRuleNetwork(long *,long *,long *);

#ifndef _RETEUTIL_SOURCE_
   extern Thread struct partialMatch           *GlobalLHSBinds;
   extern Thread struct partialMatch           *GlobalRHSBinds;
   extern Thread struct joinNode               *GlobalJoin;
#endif

#endif





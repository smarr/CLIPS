   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*      CONFLICT RESOLUTION STRATEGY HEADER MODULE     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Used to determine where a new activation is      */
/*   placed on the agenda based on the current conflict      */
/*   resolution strategy (depth, breadth, mea, lex,          */
/*   simplicity, or complexity). Also provides the           */
/*   set-strategy and get-strategy commands.                 */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_crstrtgy

#define _H_crstrtgy

#include "agenda.h"
#include "symbol.h"

#define DEPTH_STRATEGY 0
#define BREADTH_STRATEGY 1
#define LEX_STRATEGY 2
#define MEA_STRATEGY 3
#define COMPLEXITY_STRATEGY 4
#define SIMPLICITY_STRATEGY 5
#define RANDOM_STRATEGY 6

#define DEFAULT_STRATEGY DEPTH_STRATEGY

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _CRSTRTGY_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           PlaceActivation(ACTIVATION **,ACTIVATION *);
#if CONFLICT_RESOLUTION_STRATEGIES
   LOCALE DllExport int                  SetStrategy(int);
   LOCALE DllExport int                  GetStrategy(void);
   LOCALE SYMBOL_HN                     *SetStrategyCommand(void);
   LOCALE SYMBOL_HN                     *GetStrategyCommand(void);
#endif

#endif







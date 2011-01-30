   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                PATTERN HEADER FILE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides the mechanism for recognizing and       */
/*   parsing the various types of patterns that can be used  */
/*   in the LHS of a rule. In version 6.0, the only pattern  */
/*   types provided are for deftemplate and instance         */
/*   patterns.                                               */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_pattern

#define _H_pattern

#ifndef _STDIO_INCLUDED_
#include <stdio.h>
#define _STDIO_INCLUDED_
#endif

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

struct patternEntityRecord
  {
   struct entityRecord base;
   void (*decrementBasisCount)(void *);
   void (*incrementBasisCount)(void *);
   void (*matchFunction)(void *);
   BOOLEAN (*synchronized)(void *);
  };

typedef struct patternEntityRecord PTRN_ENTITY_RECORD;
typedef struct patternEntityRecord *PTRN_ENTITY_RECORD_PTR;

struct patternEntity
  {
   struct patternEntityRecord *theInfo;
#if LOGICAL_DEPENDENCIES
   void *dependents;
#endif
   unsigned busyCount;
   long int timeTag;
  };

typedef struct patternEntity PATTERN_ENTITY;
typedef struct patternEntity * PATTERN_ENTITY_PTR;

struct patternParser;

#ifndef _H_symbol
#include "symbol.h"
#endif
#ifndef _H_scanner
#include "scanner.h"
#endif
#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_match
#include "match.h"
#endif
#ifndef _H_reorder
#include "reorder.h"
#endif
#ifndef _H_constrnt
#include "constrnt.h"
#endif

#define MAXIMUM_NUMBER_OF_PATTERNS 128

struct patternParser
  {
   char *name;
   struct patternEntityRecord *entityType;
   int positionInArray;
   int (*recognizeFunction)(SYMBOL_HN *);
   struct lhsParseNode *(*parseFunction)(char *,struct token *);
   int (*postAnalysisFunction)(struct lhsParseNode *);
   struct patternNodeHeader *(*addPatternFunction)(struct lhsParseNode *);
   void (*removePatternFunction)(struct patternNodeHeader *);
   struct expr *(*genJNConstantFunction)(struct lhsParseNode *);
   void (*replaceGetJNValueFunction)(struct expr *,struct lhsParseNode *);
   struct expr *(*genGetJNValueFunction)(struct lhsParseNode *);
   struct expr *(*genCompareJNValuesFunction)(struct lhsParseNode *,struct lhsParseNode *);
   struct expr *(*genPNConstantFunction)(struct lhsParseNode *);
   void (*replaceGetPNValueFunction)(struct expr *,struct lhsParseNode *);
   struct expr *(*genGetPNValueFunction)(struct lhsParseNode *);
   struct expr *(*genComparePNValuesFunction)(struct lhsParseNode *,struct lhsParseNode *);
   void (*returnUserDataFunction)(void *);
   void *(*copyUserDataFunction)(void *);
   void (*markIRPatternFunction)(struct patternNodeHeader *,int);
   void (*incrementalResetFunction)(void);
   struct lhsParseNode *(*initialPatternFunction)(void);
   void (*codeReferenceFunction)(void *,FILE *,int,int);
   int priority;
   struct patternParser *next;
  };



#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _PATTERN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE int                            AddPatternParser(struct patternParser *);
   LOCALE struct patternParser          *FindPatternParser(char *);
   LOCALE void                           DetachPattern(int,struct patternNodeHeader *);
   LOCALE void                           GetNextPatternEntity(struct patternParser **,
                                                              struct patternEntity **);
   LOCALE struct patternParser          *GetPatternParser(int);
   LOCALE struct lhsParseNode           *RestrictionParse(char *,struct token *,int,
                                                       struct symbolHashNode *,int,
                                                       struct constraintRecord *,int);
   LOCALE int                            PostPatternAnalysis(struct lhsParseNode *);
   LOCALE void                           PatternNodeHeaderToCode(FILE *,struct patternNodeHeader *,int,int);
   LOCALE void                           AddReservedPatternSymbol(char *,char *);
   LOCALE BOOLEAN                        ReservedPatternSymbol(char *,char *);
   LOCALE void                           ReservedPatternSymbolErrorMsg(char *,char *);

#ifndef _PATTERN_SOURCE_
   extern Thread struct patternParser          *ListOfPatternParsers;
#endif

#endif










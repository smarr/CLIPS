   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*             CONSTRAINT PARSER HEADER FILE           */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides functions for parsing constraint        */
/*   declarations.                                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_cstrnpsr
#define _H_cstrnpsr

#ifndef _H_constrnt
#include "constrnt.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _CSTRNPSR_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

struct constraintParseRecord
  {
   unsigned int type : 1;
   unsigned int range : 1;
   unsigned int allowedSymbols : 1;
   unsigned int allowedStrings : 1;
   unsigned int allowedLexemes : 1;
   unsigned int allowedFloats : 1;
   unsigned int allowedIntegers : 1;
   unsigned int allowedNumbers : 1;
   unsigned int allowedValues : 1;
   unsigned int allowedInstanceNames : 1;
   unsigned int cardinality : 1;
  };

typedef struct constraintParseRecord CONSTRAINT_PARSE_RECORD;

   LOCALE BOOLEAN                        CheckConstraintParseConflicts(CONSTRAINT_RECORD *);
   LOCALE void                           AttributeConflictErrorMessage(char *,char *);
#if (! RUN_TIME) && (! BLOAD_ONLY)
   LOCALE void                           InitializeConstraintParseRecord(CONSTRAINT_PARSE_RECORD *);
   LOCALE BOOLEAN                        StandardConstraint(char *);
   LOCALE BOOLEAN                        ParseStandardConstraint(char *,char *,
                                                                 CONSTRAINT_RECORD *,
                                                                 CONSTRAINT_PARSE_RECORD *,
                                                                 int);
   LOCALE void                           OverlayConstraint(CONSTRAINT_PARSE_RECORD *,
                                                           CONSTRAINT_RECORD *,CONSTRAINT_RECORD *);
   LOCALE void                           OverlayConstraintParseRecord(CONSTRAINT_PARSE_RECORD *,
                                                                      CONSTRAINT_PARSE_RECORD *);
#endif

#endif





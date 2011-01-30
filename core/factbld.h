   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                FACT BUILD HEADER FILE               */
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

#ifndef _H_factbld

#define _H_factbld

#ifndef _H_pattern
#include "pattern.h"
#endif
#ifndef _H_network
#include "network.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

struct factPatternNode
  {
   struct patternNodeHeader header;
   long bsaveID;
   unsigned int whichField : 8;
   unsigned int whichSlot : 8;
   unsigned int leaveFields : 8;
   struct expr *networkTest;
   struct factPatternNode *nextLevel;
   struct factPatternNode *lastLevel;
   struct factPatternNode *leftNode;
   struct factPatternNode *rightNode;
  };

#ifdef _FACTBUILD_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           InitializeFactPatterns(void);

#ifndef _FACTBLD_SOURCE_
   extern Thread globle struct patternEntityRecord     FactInfo;
#endif

#endif

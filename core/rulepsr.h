   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*               RULE PARSING HEADER FILE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Coordinates parsing of a rule.                   */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_rulepsr
#define _H_rulepsr

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _RULEPSR_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE int                            ParseDefrule(char *);
   LOCALE struct lhsParseNode           *FindVariable(struct symbolHashNode *,
                                                      struct lhsParseNode *);

#endif





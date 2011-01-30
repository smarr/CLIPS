   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                 SCANNER HEADER FILE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Routines for scanning lexical tokens from an     */
/*   input source.                                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_scanner
#define _H_scanner

struct token;

#ifndef _H_pprint
#include "pprint.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _SCANNER_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

struct token
  {
   int type;
   void *value;
   char *printForm;
  };

#define print_rep printForm

   LOCALE void                           GetToken(char *,struct token *);
   LOCALE void                           CopyToken(struct token *,struct token *);
   LOCALE void                           ResetLineCount(void);
   LOCALE long                           GetLineCount(void);
   LOCALE void                           IncrementLineCount(void);
   LOCALE void                           DecrementLineCount(void);

#ifndef _SCANNER_SOURCE_
   extern Thread int                            IgnoreCompletionErrors;
#endif
#endif





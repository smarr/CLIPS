   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*            INCREMENTAL RESET HEADER FILE            */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides functionality for the incremental       */
/*   reset of the pattern and join networks when a new       */
/*   rule is added.                                          */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_incrrset

#define _H_incrrset

#ifndef _H_ruledef
#include "ruledef.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _INCRRSET_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           IncrementalReset(struct defrule *);
   LOCALE BOOLEAN                        GetIncrementalReset(void);
   LOCALE BOOLEAN                        SetIncrementalReset(BOOLEAN);
   LOCALE int                            GetIncrementalResetCommand(void);
   LOCALE int                            SetIncrementalResetCommand(void);

#if (! RUN_TIME) && (! BLOAD_ONLY)
#ifndef _INCRRSET_SOURCE_
   extern Thread int                            IncrementalResetInProgress;
#endif
#endif

#endif










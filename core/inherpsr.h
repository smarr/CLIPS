   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.20  01/31/02          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_inherpsr
#define _H_inherpsr

#if OBJECT_SYSTEM && (! BLOAD_ONLY) && (! RUN_TIME)

#ifndef _H_object
#include "object.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _INHERPSR_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

LOCALE PACKED_CLASS_LINKS *ParseSuperclasses(void *,EXEC_STATUS,char *,SYMBOL_HN *);
LOCALE PACKED_CLASS_LINKS *FindPrecedenceList(void *,EXEC_STATUS,DEFCLASS *,PACKED_CLASS_LINKS *);
LOCALE void PackClassLinks(void *,EXEC_STATUS,PACKED_CLASS_LINKS *,CLASS_LINK *);

#ifndef _INHERPSR_SOURCE_
#endif

#endif

#endif




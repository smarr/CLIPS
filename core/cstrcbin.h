   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/13/98          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_cstrcbin
#define _H_cstrcbin

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE

struct bsaveConstructHeader
  {
   long name;
   long whichModule;
   long next;
  };

#ifndef _H_constrct
#include "constrct.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _CSTRCBIN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if BLOAD_AND_BSAVE
LOCALE void MarkConstructHeaderNeededItems(struct constructHeader *,long);
LOCALE void AssignBsaveConstructHeaderVals(struct bsaveConstructHeader *,
                                             struct constructHeader *);
#endif

LOCALE void UpdateConstructHeader(struct bsaveConstructHeader *,
                                  struct constructHeader *,int,void *,int,void *);
LOCALE void UnmarkConstructHeader(struct constructHeader *);

#ifndef _CSTRCBIN_SOURCE_
#endif

#endif

#endif


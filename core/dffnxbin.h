   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/09/97          */
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

#ifndef _H_dffnxbin
#define _H_dffnxbin

#if DEFFUNCTION_CONSTRUCT && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)

#include "dffnxfun.h"

#define DeffunctionPointer(i) (((i) == -1L) ? NULL : (DEFFUNCTION *) &deffunctionArray[i])

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _DFFNXBIN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

LOCALE void SetupDeffunctionsBload(void);
LOCALE void *BloadDeffunctionModuleReference(int);

#ifndef _DFFNXBIN_SOURCE_
extern Thread DEFFUNCTION *deffunctionArray;
#endif

#endif

#endif





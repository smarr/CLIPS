   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*            FACT BLOAD/BSAVE HEADER FILE             */
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

#ifndef _H_factbin

#define _H_factbin

#include "factbld.h"

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _FACTBIN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           FactBinarySetup(void);

#ifndef _FACTBIN_SOURCE_
   extern Thread struct factPatternNode  *FactPatternArray;
#endif

#define BsaveFactPatternIndex(patPtr) ((patPtr == NULL) ? -1L : ((struct factPatternNode *) patPtr)->bsaveID)
#define BloadFactPatternPointer(i) ((struct factPatternNode *) ((i == -1L) ? NULL : &FactPatternArray[i]))

#endif


   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*        FACT RETE ACCESS FUNCTIONS HEADER FILE       */
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

#ifndef _H_factrete

#define _H_factrete

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _FACTRETE_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE BOOLEAN                        FactPNGetVar1(void *,DATA_OBJECT_PTR);
   LOCALE BOOLEAN                        FactPNGetVar2(void *,DATA_OBJECT_PTR);
   LOCALE BOOLEAN                        FactPNGetVar3(void *,DATA_OBJECT_PTR);
   LOCALE BOOLEAN                        FactJNGetVar1(void *,DATA_OBJECT_PTR);
   LOCALE BOOLEAN                        FactJNGetVar2(void *,DATA_OBJECT_PTR);
   LOCALE BOOLEAN                        FactJNGetVar3(void *,DATA_OBJECT_PTR);
   LOCALE BOOLEAN                        FactSlotLength(void *,DATA_OBJECT_PTR);
   LOCALE int                            FactJNCompVars1(void *,DATA_OBJECT_PTR);
   LOCALE int                            FactJNCompVars2(void *,DATA_OBJECT_PTR);
   LOCALE int                            FactPNCompVars1(void *,DATA_OBJECT_PTR);
   LOCALE BOOLEAN                        FactPNConstant1(void *,DATA_OBJECT_PTR);
   LOCALE BOOLEAN                        FactPNConstant2(void *,DATA_OBJECT_PTR);
   LOCALE int                            FactStoreMultifield(void *,DATA_OBJECT_PTR);
   LOCALE int                            AdjustFieldPosition(struct multifieldMarker *,int,int,int *);

#endif







   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
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
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
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

# include "execution_status.h"

   LOCALE intBool                        FactPNGetVar1(void *,EXEC_STATUS,void *,DATA_OBJECT_PTR);
   LOCALE intBool                        FactPNGetVar2(void *,EXEC_STATUS,void *,DATA_OBJECT_PTR);
   LOCALE intBool                        FactPNGetVar3(void *,EXEC_STATUS,void *,DATA_OBJECT_PTR);
   LOCALE intBool                        FactJNGetVar1(void *,EXEC_STATUS,void *,DATA_OBJECT_PTR);
   LOCALE intBool                        FactJNGetVar2(void *,EXEC_STATUS,void *,DATA_OBJECT_PTR);
   LOCALE intBool                        FactJNGetVar3(void *,EXEC_STATUS,void *,DATA_OBJECT_PTR);
   LOCALE intBool                        FactSlotLength(void *,EXEC_STATUS,void *,DATA_OBJECT_PTR);
   LOCALE int                            FactJNCompVars1(void *,EXEC_STATUS,void *,DATA_OBJECT_PTR);
   LOCALE int                            FactJNCompVars2(void *,EXEC_STATUS,void *,DATA_OBJECT_PTR);
   LOCALE int                            FactPNCompVars1(void *,EXEC_STATUS,void *,DATA_OBJECT_PTR);
   LOCALE intBool                        FactPNConstant1(void *,EXEC_STATUS,void *,DATA_OBJECT_PTR);
   LOCALE intBool                        FactPNConstant2(void *,EXEC_STATUS,void *,DATA_OBJECT_PTR);
   LOCALE int                            FactStoreMultifield(void *,EXEC_STATUS,void *,DATA_OBJECT_PTR);
   LOCALE unsigned short                 AdjustFieldPosition(void *,EXEC_STATUS,struct multifieldMarker *,
                                                             unsigned short,unsigned short,int *);

#endif



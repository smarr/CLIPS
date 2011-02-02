   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.20  01/31/02            */
   /*                                                     */
   /*         FACT RETE PRINT FUNCTIONS HEADER FILE       */
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

#ifndef _H_factprt

#define _H_factprt

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _FACTPRT_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

# include "execution_status.h"

   LOCALE void                           PrintFactJNCompVars1(void *,EXEC_STATUS,char *,void *);
   LOCALE void                           PrintFactJNCompVars2(void *,EXEC_STATUS,char *,void *);
   LOCALE void                           PrintFactPNCompVars1(void *,EXEC_STATUS,char *,void *);
   LOCALE void                           PrintFactJNGetVar1(void *,EXEC_STATUS,char *,void *);
   LOCALE void                           PrintFactJNGetVar2(void *,EXEC_STATUS,char *,void *);
   LOCALE void                           PrintFactJNGetVar3(void *,EXEC_STATUS,char *,void *);
   LOCALE void                           PrintFactPNGetVar1(void *,EXEC_STATUS,char *,void *);
   LOCALE void                           PrintFactPNGetVar2(void *,EXEC_STATUS,char *,void *);
   LOCALE void                           PrintFactPNGetVar3(void *,EXEC_STATUS,char *,void *);
   LOCALE void                           PrintFactSlotLength(void *,EXEC_STATUS,char *,void *);
   LOCALE void                           PrintFactPNConstant1(void *,EXEC_STATUS,char *,void *);
   LOCALE void                           PrintFactPNConstant2(void *,EXEC_STATUS,char *,void *);

#endif



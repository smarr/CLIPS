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

#ifndef _H_insquery
#define _H_insquery

#if INSTANCE_SET_QUERIES

#ifndef _H_object
#include "object.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _INSQUERY_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define QUERY_DELIMETER_STRING     "(QDS)"

LOCALE void SetupQuery(void);
LOCALE SYMBOL_HN *GetQueryInstance(void);
LOCALE void GetQueryInstanceSlot(DATA_OBJECT *);
LOCALE BOOLEAN AnyInstances(void);
LOCALE void QueryFindInstance(DATA_OBJECT *);
LOCALE void QueryFindAllInstances(DATA_OBJECT *);
LOCALE void QueryDoForInstance(DATA_OBJECT *);
LOCALE void QueryDoForAllInstances(DATA_OBJECT *);
LOCALE void DelayedQueryDoForAllInstances(DATA_OBJECT *);

#ifndef _INSQUERY_SOURCE_
extern Thread SYMBOL_HN *QUERY_DELIMETER_SYMBOL;
#endif

#endif

#endif






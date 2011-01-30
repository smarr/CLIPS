   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*              FACT FUNCTIONS HEADER FILE             */
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

#ifndef _H_factfun
#define _H_factfun

#ifndef _H_factmngr
#include "factmngr.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _FACTFUN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           FactFunctionDefinitions(void);
   LOCALE void                          *FactRelationFunction(void);
   LOCALE void                          *FactRelation(void *);
   LOCALE long int                       FactExistpFunction(void);
   LOCALE long int                       FactExistp(void *);
   LOCALE void                           FactSlotValueFunction(DATA_OBJECT *);
   LOCALE void                           FactSlotValue(void *,char *,DATA_OBJECT *);
   LOCALE void                           FactSlotNamesFunction(DATA_OBJECT *);
   LOCALE void                           FactSlotNames(void *,DATA_OBJECT *);
   LOCALE void                           GetFactListFunction(DATA_OBJECT *);
   LOCALE void                           GetFactList(DATA_OBJECT *,void *);
   LOCALE struct fact                   *GetFactAddressOrIndexArgument(char *,int,int);

#endif



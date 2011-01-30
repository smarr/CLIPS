   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*          PROCEDURAL FUNCTIONS HEADER FILE           */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_prcdrfun

#define _H_prcdrfun

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _PRCDRFUN_SOURCE
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           ProceduralFunctionDefinitions(void);
   LOCALE void                           WhileFunction(DATA_OBJECT_PTR);
   LOCALE void                           LoopForCountFunction(DATA_OBJECT_PTR);
   LOCALE long                           GetLoopCount(void);
   LOCALE void                           IfFunction(DATA_OBJECT_PTR);
   LOCALE void                           BindFunction(DATA_OBJECT_PTR);
   LOCALE void                           PrognFunction(DATA_OBJECT_PTR);
   LOCALE void                           ReturnFunction(DATA_OBJECT_PTR);
   LOCALE void                           BreakFunction(void);
   LOCALE void                           SwitchFunction(DATA_OBJECT_PTR);
   LOCALE BOOLEAN                        GetBoundVariable(struct dataObject *,struct symbolHashNode *);
   LOCALE void                           FlushBindList(void);

#ifndef _PRCDRFUN_SOURCE
   extern Thread int                            ReturnFlag;
   extern Thread int                            BreakFlag;
#endif

#endif







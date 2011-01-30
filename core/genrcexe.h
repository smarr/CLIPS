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

#ifndef _H_genrcexe
#define _H_genrcexe

#if DEFGENERIC_CONSTRUCT

#include "genrcfun.h"
#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _GENRCEXE_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

LOCALE void GenericDispatch(DEFGENERIC *,DEFMETHOD *,DEFMETHOD *,EXPRESSION *,DATA_OBJECT *);
LOCALE void UnboundMethodErr(void);
LOCALE BOOLEAN IsMethodApplicable(DEFMETHOD *);

#if IMPERATIVE_METHODS
LOCALE int NextMethodP(void);
LOCALE void CallNextMethod(DATA_OBJECT *);
LOCALE void CallSpecificMethod(DATA_OBJECT *);
LOCALE void OverrideNextMethod(DATA_OBJECT *);
#endif

LOCALE void GetGenericCurrentArgument(DATA_OBJECT *);

#ifndef _GENRCEXE_SOURCE_
#endif

#endif

#endif





   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.24  05/17/06          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed IMPERATIVE_METHODS compilation flag.   */
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

LOCALE void GenericDispatch(void *,EXEC_STATUS,DEFGENERIC *,DEFMETHOD *,DEFMETHOD *,EXPRESSION *,DATA_OBJECT *);
LOCALE void UnboundMethodErr(void *,EXEC_STATUS);
LOCALE intBool IsMethodApplicable(void *,EXEC_STATUS,DEFMETHOD *);

LOCALE int NextMethodP(void *,EXEC_STATUS);
LOCALE void CallNextMethod(void *,EXEC_STATUS,DATA_OBJECT *);
LOCALE void CallSpecificMethod(void *,EXEC_STATUS,DATA_OBJECT *);
LOCALE void OverrideNextMethod(void *,EXEC_STATUS,DATA_OBJECT *);

LOCALE void GetGenericCurrentArgument(void *,EXEC_STATUS,DATA_OBJECT *);

#ifndef _GENRCEXE_SOURCE_
#endif

#endif

#endif





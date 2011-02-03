   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.24  06/05/06          */
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
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#ifndef _H_insmult
#define _H_insmult

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _INSMULT_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if (! RUN_TIME)
LOCALE void SetupInstanceMultifieldCommands(void *,EXEC_STATUS);
#endif

LOCALE void MVSlotReplaceCommand(void *,EXEC_STATUS,DATA_OBJECT *);
LOCALE void MVSlotInsertCommand(void *,EXEC_STATUS,DATA_OBJECT *);
LOCALE void MVSlotDeleteCommand(void *,EXEC_STATUS,DATA_OBJECT *);
LOCALE intBool DirectMVReplaceCommand(void *,EXEC_STATUS);
LOCALE intBool DirectMVInsertCommand(void *,EXEC_STATUS);
LOCALE intBool DirectMVDeleteCommand(void *,EXEC_STATUS);

#ifndef _INSMULT_SOURCE_
#endif

#endif




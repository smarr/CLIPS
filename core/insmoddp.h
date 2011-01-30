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

#ifndef _H_insmoddp
#define _H_insmoddp

#define DIRECT_MODIFY_STRING    "direct-modify"
#define MSG_MODIFY_STRING       "message-modify"
#define DIRECT_DUPLICATE_STRING "direct-duplicate"
#define MSG_DUPLICATE_STRING    "message-duplicate"

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _INSMODDP_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if (! RUN_TIME)
LOCALE void SetupInstanceModDupCommands(void);
#endif

LOCALE void ModifyInstance(DATA_OBJECT *);
LOCALE void MsgModifyInstance(DATA_OBJECT *);
LOCALE void DuplicateInstance(DATA_OBJECT *);
LOCALE void MsgDuplicateInstance(DATA_OBJECT *);

#if INSTANCE_PATTERN_MATCHING
LOCALE void InactiveModifyInstance(DATA_OBJECT *);
LOCALE void InactiveMsgModifyInstance(DATA_OBJECT *);
LOCALE void InactiveDuplicateInstance(DATA_OBJECT *);
LOCALE void InactiveMsgDuplicateInstance(DATA_OBJECT *);
#endif

LOCALE void DirectModifyMsgHandler(DATA_OBJECT *);
LOCALE void MsgModifyMsgHandler(DATA_OBJECT *);
LOCALE void DirectDuplicateMsgHandler(DATA_OBJECT *);
LOCALE void MsgDuplicateMsgHandler(DATA_OBJECT *);

#ifndef _INSMODDP_SOURCE_
#endif

#endif








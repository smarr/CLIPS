   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.24  05/17/06          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Message-passing support functions                */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed IMPERATIVE_MESSAGE_HANDLERS            */
/*                    compilation flag.                      */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#ifndef _H_msgpass
#define _H_msgpass

#define GetActiveInstance(theEnv,execStatus) ((INSTANCE_TYPE *) GetNthMessageArgument(theEnv,execStatus,0)->value)

#ifndef _H_object
#include "object.h"
#endif

typedef struct messageHandlerLink
  {
   HANDLER *hnd;
   struct messageHandlerLink *nxt;
   struct messageHandlerLink *nxtInStack;
  } HANDLER_LINK;

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _MSGPASS_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define Send(a,b,c,d) EnvSend(GetCurrentEnvironment(),GetCurrentExecutionStatus(),a,b,c,d)

   LOCALE void             DirectMessage(void *,EXEC_STATUS,SYMBOL_HN *,INSTANCE_TYPE *,
                                         DATA_OBJECT *,EXPRESSION *);
   LOCALE void             EnvSend(void *,EXEC_STATUS,DATA_OBJECT *,char *,char *,DATA_OBJECT *);
   LOCALE void             DestroyHandlerLinks(void *,EXEC_STATUS,HANDLER_LINK *);
   LOCALE void             SendCommand(void *,EXEC_STATUS,DATA_OBJECT *);
   LOCALE DATA_OBJECT     *GetNthMessageArgument(void *,EXEC_STATUS,int);

   LOCALE int              NextHandlerAvailable(void *,EXEC_STATUS);
   LOCALE void             CallNextHandler(void *,EXEC_STATUS,DATA_OBJECT *);

   LOCALE void             FindApplicableOfName(void *,EXEC_STATUS,DEFCLASS *,HANDLER_LINK *[],
                                                HANDLER_LINK *[],SYMBOL_HN *);
   LOCALE HANDLER_LINK    *JoinHandlerLinks(void *,EXEC_STATUS,HANDLER_LINK *[],HANDLER_LINK *[],SYMBOL_HN *);

   LOCALE void             PrintHandlerSlotGetFunction(void *,EXEC_STATUS,char *,void *);
   LOCALE intBool          HandlerSlotGetFunction(void *,EXEC_STATUS,void *,DATA_OBJECT *);
   LOCALE void             PrintHandlerSlotPutFunction(void *,EXEC_STATUS,char *,void *);
   LOCALE intBool          HandlerSlotPutFunction(void *,EXEC_STATUS,void *,DATA_OBJECT *);
   LOCALE void             DynamicHandlerGetSlot(void *,EXEC_STATUS,DATA_OBJECT *);
   LOCALE void             DynamicHandlerPutSlot(void *,EXEC_STATUS,DATA_OBJECT *);

#endif








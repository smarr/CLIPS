   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.10  04/09/97          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Message-passing support functions                */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_msgpass
#define _H_msgpass

#define GetActiveInstance() ((INSTANCE_TYPE *) GetNthMessageArgument(0)->value)

#ifndef _H_object
#include "object.h"
#endif

typedef struct messageHandlerLink
  {
   HANDLER *hnd;
   struct messageHandlerLink *nxt;
  } HANDLER_LINK;

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _MSGPASS_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

LOCALE void DirectMessage(SYMBOL_HN *,INSTANCE_TYPE *,
                          DATA_OBJECT *,EXPRESSION *);
LOCALE DllExport void Send(DATA_OBJECT *,char *,char *,DATA_OBJECT *);
void DestroyHandlerLinks(HANDLER_LINK *);
LOCALE void SendCommand(DATA_OBJECT *);
LOCALE DATA_OBJECT *GetNthMessageArgument(int);

#if IMPERATIVE_MESSAGE_HANDLERS
LOCALE int NextHandlerAvailable(void);
LOCALE void CallNextHandler(DATA_OBJECT *);
#endif

LOCALE void FindApplicableOfName(DEFCLASS *,HANDLER_LINK *[],
                                 HANDLER_LINK *[],SYMBOL_HN *);
LOCALE HANDLER_LINK *JoinHandlerLinks(HANDLER_LINK *[],HANDLER_LINK *[],SYMBOL_HN *);

LOCALE void PrintHandlerSlotGetFunction(char *,void *);
LOCALE BOOLEAN HandlerSlotGetFunction(void *,DATA_OBJECT *);
LOCALE void PrintHandlerSlotPutFunction(char *,void *);
LOCALE BOOLEAN HandlerSlotPutFunction(void *,DATA_OBJECT *);
LOCALE void DynamicHandlerGetSlot(DATA_OBJECT *);
LOCALE void DynamicHandlerPutSlot(DATA_OBJECT *);

#ifndef _MSGPASS_SOURCE_
extern Thread SYMBOL_HN *CurrentMessageName;
extern Thread HANDLER_LINK *CurrentCore;
#endif

#endif








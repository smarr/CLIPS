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

#ifndef _H_msgcom
#define _H_msgcom

#ifndef _H_object
#include "object.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _MSGCOM_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define INIT_STRING   "init"
#define DELETE_STRING "delete"
#define PRINT_STRING  "print"

LOCALE void SetupMessageHandlers(void);
LOCALE char *GetDefmessageHandlerName(void *,unsigned);
LOCALE char *GetDefmessageHandlerType(void *,unsigned);
LOCALE unsigned GetNextDefmessageHandler(void *,unsigned);
LOCALE HANDLER *GetDefmessageHandlerPointer(void *,unsigned);
#if DEBUGGING_FUNCTIONS
LOCALE BOOLEAN GetDefmessageHandlerWatch(void *,unsigned);
LOCALE void SetDefmessageHandlerWatch(int,void *,unsigned);
#endif
LOCALE unsigned FindDefmessageHandler(void *,char *,char *);
LOCALE int IsDefmessageHandlerDeletable(void *,unsigned);
LOCALE void UndefmessageHandlerCommand(void);
LOCALE int UndefmessageHandler(void *,unsigned);

#if DEBUGGING_FUNCTIONS
LOCALE void PPDefmessageHandlerCommand(void);
LOCALE void ListDefmessageHandlersCommand(void);
LOCALE void PreviewSendCommand(void);
LOCALE char *GetDefmessageHandlerPPForm(void *,unsigned);
LOCALE void ListDefmessageHandlers(char *,void *,int);
LOCALE void PreviewSend(char *,void *,char *);
LOCALE long DisplayHandlersInLinks(char *,PACKED_CLASS_LINKS *,unsigned);
#endif

#ifndef _MSGCOM_SOURCE_
#endif

#endif






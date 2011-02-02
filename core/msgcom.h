   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.20  01/31/02          */
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
/*************************************************************/

#ifndef _H_msgcom
#define _H_msgcom

#ifndef _H_object
#include "object.h"
#endif

#ifndef _H_msgpass
#include "msgpass.h"
#endif

#define MESSAGE_HANDLER_DATA 32

struct messageHandlerData
  { 
   ENTITY_RECORD HandlerGetInfo;
   ENTITY_RECORD HandlerPutInfo;
   SYMBOL_HN *INIT_SYMBOL;
   SYMBOL_HN *DELETE_SYMBOL;
   SYMBOL_HN *CREATE_SYMBOL;
#if DEBUGGING_FUNCTIONS
   unsigned WatchHandlers;
   unsigned WatchMessages;
#endif
   char *hndquals[4];
   SYMBOL_HN *SELF_SYMBOL;
   SYMBOL_HN *CurrentMessageName;
   HANDLER_LINK *CurrentCore;
   HANDLER_LINK *TopOfCore;
   HANDLER_LINK *NextInCore;
   HANDLER_LINK *OldCore;
  };

#define MessageHandlerData(theEnv,execStatus) ((struct messageHandlerData *) GetEnvironmentData(theEnv,execStatus,MESSAGE_HANDLER_DATA))


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
#define CREATE_STRING "create"

#define FindDefmessageHandler(a,b,c) EnvFindDefmessageHandler(GetCurrentEnvironment(),a,b,c)
#define GetDefmessageHandlerName(a,b) EnvGetDefmessageHandlerName(GetCurrentEnvironment(),a,b)
#define GetDefmessageHandlerPPForm(a,b) EnvGetDefmessageHandlerPPForm(GetCurrentEnvironment(),a,b)
#define GetDefmessageHandlerType(a,b) EnvGetDefmessageHandlerType(GetCurrentEnvironment(),a,b)
#define GetDefmessageHandlerWatch(a,b) EnvGetDefmessageHandlerWatch(GetCurrentEnvironment(),a,b)
#define GetNextDefmessageHandler(a,b) EnvGetNextDefmessageHandler(GetCurrentEnvironment(),a,b)
#define IsDefmessageHandlerDeletable(a,b) EnvIsDefmessageHandlerDeletable(GetCurrentEnvironment(),a,b)
#define ListDefmessageHandlers(a,b,c) EnvListDefmessageHandlers(GetCurrentEnvironment(),a,b,c)
#define PreviewSend(a,b,c) EnvPreviewSend(GetCurrentEnvironment(),a,b,c)
#define SetDefmessageHandlerWatch(a,b,c) EnvSetDefmessageHandlerWatch(GetCurrentEnvironment(),a,b,c)
#define UndefmessageHandler(a,b) EnvUndefmessageHandler(GetCurrentEnvironment(),a,b)
   
   LOCALE void             SetupMessageHandlers(void *,EXEC_STATUS);
   LOCALE char            *EnvGetDefmessageHandlerName(void *,EXEC_STATUS,void *,int);
   LOCALE char            *EnvGetDefmessageHandlerType(void *,EXEC_STATUS,void *,int);
   LOCALE int              EnvGetNextDefmessageHandler(void *,EXEC_STATUS,void *,int);
   LOCALE HANDLER         *GetDefmessageHandlerPointer(void *,EXEC_STATUS,int);
#if DEBUGGING_FUNCTIONS
   LOCALE unsigned         EnvGetDefmessageHandlerWatch(void *,EXEC_STATUS,void *,int);
   LOCALE void             EnvSetDefmessageHandlerWatch(void *,EXEC_STATUS,int,void *,int);
#endif
   LOCALE unsigned         EnvFindDefmessageHandler(void *,EXEC_STATUS,void *,char *,char *); 
   LOCALE int              EnvIsDefmessageHandlerDeletable(void *,EXEC_STATUS,void *,int);
   LOCALE void             UndefmessageHandlerCommand(void *,EXEC_STATUS);
   LOCALE int              EnvUndefmessageHandler(void *,EXEC_STATUS,void *,int);

#if DEBUGGING_FUNCTIONS
   LOCALE void             PPDefmessageHandlerCommand(void *,EXEC_STATUS);
   LOCALE void             ListDefmessageHandlersCommand(void *,EXEC_STATUS);
   LOCALE void             PreviewSendCommand(void *,EXEC_STATUS); 
   LOCALE char            *EnvGetDefmessageHandlerPPForm(void *,EXEC_STATUS,void *,int);
   LOCALE void             EnvListDefmessageHandlers(void *,EXEC_STATUS,char *,void *,int);
   LOCALE void             EnvPreviewSend(void *,EXEC_STATUS,char *,void *,char *);
   LOCALE long             DisplayHandlersInLinks(void *,EXEC_STATUS,char *,PACKED_CLASS_LINKS *,int);
#endif

#endif






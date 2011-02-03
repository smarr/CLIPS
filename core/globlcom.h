   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*            DEFGLOBAL COMMANDS HEADER FILE           */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#ifndef _H_globlcom
#define _H_globlcom

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _GLOBLCOM_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define GetResetGlobals() EnvGetResetGlobals(GetCurrentEnvironment(),GetCurrentExecutionStatus())
#define SetResetGlobals(a) EnvSetResetGlobals(GetCurrentEnvironment(),GetCurrentExecutionStatus(),a)
#define ShowDefglobals(a,b) EnvShowDefglobals(GetCurrentEnvironment(),GetCurrentExecutionStatus(),a,b)

   LOCALE void                           DefglobalCommandDefinitions(void *,EXEC_STATUS);
   LOCALE int                            SetResetGlobalsCommand(void *,EXEC_STATUS);
   LOCALE intBool                        EnvSetResetGlobals(void *,EXEC_STATUS,int);
   LOCALE int                            GetResetGlobalsCommand(void *,EXEC_STATUS);
   LOCALE intBool                        EnvGetResetGlobals(void *,EXEC_STATUS);
   LOCALE void                           ShowDefglobalsCommand(void *,EXEC_STATUS);
   LOCALE void                           EnvShowDefglobals(void *,EXEC_STATUS,char *,void *);

#endif


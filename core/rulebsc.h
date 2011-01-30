   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*         DEFRULE BASIC COMMANDS HEADER FILE          */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements core commands for the defrule         */
/*   construct such as clear, reset, save, undefrule,        */
/*   ppdefrule, list-defrules, and                           */
/*   get-defrule-list.                                       */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_rulebsc
#define _H_rulebsc

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _RULEBSC_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           DefruleBasicCommands(void);
   LOCALE void                           UndefruleCommand(void);
   LOCALE BOOLEAN                        Undefrule(void *);
   LOCALE void                           GetDefruleListFunction(DATA_OBJECT_PTR);
   LOCALE void                           GetDefruleList(DATA_OBJECT_PTR,void *);
   LOCALE SYMBOL_HN                     *DefruleModuleFunction(void);
#if DEBUGGING_FUNCTIONS
   LOCALE void                           PPDefruleCommand(void);
   LOCALE int                            PPDefrule(char *,char *);
   LOCALE void                           ListDefrulesCommand(void);
   LOCALE void                           ListDefrules(char *,void *);
   LOCALE BOOLEAN                        GetDefruleWatchFirings(void *);
   LOCALE BOOLEAN                        GetDefruleWatchActivations(void *);
   LOCALE void                           SetDefruleWatchFirings(int,void *);
   LOCALE void                           SetDefruleWatchActivations(int,void *);
   LOCALE BOOLEAN                        DefruleWatchAccess(int,int,struct expr *);
   LOCALE BOOLEAN                        DefruleWatchPrint(char *,int,struct expr *);
#endif

#ifndef _RULEBSC_SOURCE_
   extern Thread BOOLEAN                        WatchRules;
#endif

#endif



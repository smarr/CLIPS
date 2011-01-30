   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*         DEFMODULE BASIC COMMANDS HEADER FILE        */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements core commands for the deffacts        */
/*   construct such as clear, reset, save, undeffacts,       */
/*   ppdeffacts, list-deffacts, and get-deffacts-list.       */
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

#ifndef _H_modulbsc
#define _H_modulbsc

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _MODULBSC_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           DefmoduleBasicCommands(void);
   LOCALE void                           GetDefmoduleList(DATA_OBJECT_PTR);
   LOCALE void                           PPDefmoduleCommand(void);
   LOCALE int                            PPDefmodule(char *,char *);
   LOCALE void                           ListDefmodulesCommand(void);
   LOCALE void                           ListDefmodules(char *);

#endif



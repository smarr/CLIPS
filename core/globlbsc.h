   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*         DEFGLOBAL BASIC COMMANDS HEADER FILE        */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_globlbsc
#define _H_globlbsc

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _GLOBLBSC_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           DefglobalBasicCommands(void);
   LOCALE void                           UndefglobalCommand(void);
   LOCALE DllExport BOOLEAN              Undefglobal(void *);
   LOCALE void                           GetDefglobalListFunction(DATA_OBJECT_PTR);
   LOCALE void                           GetDefglobalList(DATA_OBJECT_PTR,void *);
   LOCALE SYMBOL_HN                     *DefglobalModuleFunction(void);
   LOCALE void                           PPDefglobalCommand(void);
   LOCALE int                            PPDefglobal(char *,char *);
   LOCALE void                           ListDefglobalsCommand(void);
   LOCALE void                           ListDefglobals(char *,void *);
   LOCALE BOOLEAN                        GetDefglobalWatch(void *);
   LOCALE void                           SetDefglobalWatch(int,void *);
   LOCALE void                           ResetDefglobals(void);

#ifndef _GLOBLBSC_SOURCE_
   extern Thread int                            WatchGlobals;
#endif

#endif



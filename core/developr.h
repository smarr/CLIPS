   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  05/17/06            */
   /*                                                     */
   /*                 DEVELOPER HEADER FILE               */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Converted INSTANCE_PATTERN_MATCHING to          */
/*            DEFRULE_CONSTRUCT.                              */
/*                                                            */
/*************************************************************/

#ifndef _H_developr
#define _H_developr

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _DEVELOPR_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           DeveloperCommands(void *,EXEC_STATUS);
   LOCALE void                           PrimitiveTablesInfo(void *,EXEC_STATUS);
   LOCALE void                           PrimitiveTablesUsage(void *,EXEC_STATUS);
   LOCALE void                           EnableGCHeuristics(void *,EXEC_STATUS);
   LOCALE void                           DisableGCHeuristics(void *,EXEC_STATUS);

#if DEFRULE_CONSTRUCT && DEFTEMPLATE_CONSTRUCT
   LOCALE void                           ShowFactPatternNetwork(void *,EXEC_STATUS);
#endif
#if DEFRULE_CONSTRUCT && OBJECT_SYSTEM
   LOCALE void                           PrintObjectPatternNetwork(void *,EXEC_STATUS);
#endif
#if OBJECT_SYSTEM
   LOCALE void                           InstanceTableUsage(void *,EXEC_STATUS);
#endif

#endif



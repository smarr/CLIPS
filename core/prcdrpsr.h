   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*       PROCEDURAL FUNCTIONS PARSER HEADER FILE       */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#ifndef _H_prcdrpsr

#define _H_prcdrpsr

#ifndef _H_constrnt
#include "constrnt.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _PRCDRPSR_SOURCE
#define LOCALE
#else
#define LOCALE extern
#endif

struct BindInfo
  {
   struct symbolHashNode *name;
   CONSTRAINT_RECORD *constraints;
   struct BindInfo *next;
  };

#if (! RUN_TIME)
   LOCALE void                           ProceduralFunctionParsers(void *,EXEC_STATUS);
   LOCALE struct BindInfo               *GetParsedBindNames(void *,EXEC_STATUS);
   LOCALE void                           SetParsedBindNames(void *,EXEC_STATUS,struct BindInfo *);
   LOCALE void                           ClearParsedBindNames(void *,EXEC_STATUS);
   LOCALE intBool                        ParsedBindNamesEmpty(void *,EXEC_STATUS);
#endif
#if (! BLOAD_ONLY) && (! RUN_TIME)
   LOCALE int                            SearchParsedBindNames(void *,EXEC_STATUS,struct symbolHashNode *);
   LOCALE int                            CountParsedBindNames(void *,EXEC_STATUS);
   LOCALE void                           RemoveParsedBindName(void *,EXEC_STATUS,struct symbolHashNode *);
   LOCALE struct constraintRecord       *FindBindConstraints(void *,EXEC_STATUS,struct symbolHashNode *);
#endif

#endif





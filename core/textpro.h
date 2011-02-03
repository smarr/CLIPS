   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*             TEXT PROCESSING HEADER FILE             */
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
/*      6.24: Added get-region function.                     */
/*                                                           */
/*************************************************************/

#ifndef _H_textpro

#define _H_textpro

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _TEXTPRO_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if TEXTPRO_FUNCTIONS
   LOCALE void                           FetchCommand(void *,EXEC_STATUS,DATA_OBJECT *);
   LOCALE int                            PrintRegionCommand(void *,EXEC_STATUS);
   LOCALE void                          *GetRegionCommand(void *,EXEC_STATUS);
   int                                   TossCommand(void *,EXEC_STATUS);
#endif

#if HELP_FUNCTIONS
   LOCALE void                           HelpFunction(void *,EXEC_STATUS);
   LOCALE void                           HelpPathFunction(void *,EXEC_STATUS);
#endif

   LOCALE void                           HelpFunctionDefinitions(void *,EXEC_STATUS);
#endif






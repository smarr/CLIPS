   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*              COMMAND LINE HEADER FILE               */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides a set of routines for processing        */
/*   commands entered at the top level prompt.               */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_commline

#define _H_commline

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _COMMLINE_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE int                            ExpandCommandString(int);
   LOCALE void                           FlushCommandString(void);
   LOCALE void                           SetCommandString(char *);
   LOCALE void                           AppendCommandString(char *);
   LOCALE char                          *GetCommandString(void);
   LOCALE int                            CompleteCommand(char *);
   LOCALE void                           CommandLoop(void);
   LOCALE void                           PrintPrompt(void);
   LOCALE void                           SetAfterPromptFunction(int (*)(void));
   LOCALE DllExport BOOLEAN              RouteCommand(char *,int);
   LOCALE int                          (*SetEventFunction(int (*)(void)))(void);
   LOCALE BOOLEAN                        TopLevelCommand(void);
   LOCALE void                           AppendNCommandString(char *,int);
   LOCALE void                           SetNCommandString(char *,int);
   LOCALE char                          *GetCommandCompletionString(char *,int);

#ifndef _COMMLINE_SOURCE_
   extern Thread int                     EvaluatingTopLevelCommand;
#endif

#endif






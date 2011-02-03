   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
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
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Refactored several functions and added         */
/*            additional functions for use by an interface   */
/*            layered on top of CLIPS.                       */
/*                                                           */
/*************************************************************/

#ifndef _H_commline

#define _H_commline

#define COMMANDLINE_DATA 40

struct commandLineData
  { 
   int EvaluatingTopLevelCommand;
   int HaltCommandLoopBatch;
#if ! RUN_TIME
   struct expr *CurrentCommand;
   char *CommandString;
   size_t MaximumCharacters;
   int ParsingTopLevelCommand;
   char *BannerString;
   int (*EventFunction)(void *, EXEC_STATUS);
   int (*AfterPromptFunction)(void *, EXEC_STATUS);
   int (*BeforeCommandExecutionFunction)(void *, EXEC_STATUS);
#endif
  };

#define CommandLineData(theEnv,execStatus) ((struct commandLineData *) GetEnvironmentData(theEnv,execStatus,COMMANDLINE_DATA))

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _COMMLINE_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           InitializeCommandLineData(void *,EXEC_STATUS);
   LOCALE int                            ExpandCommandString(void *,EXEC_STATUS,int);
   LOCALE void                           FlushCommandString(void *,EXEC_STATUS);
   LOCALE void                           SetCommandString(void *,EXEC_STATUS,char *);
   LOCALE void                           AppendCommandString(void *,EXEC_STATUS,char *);
   LOCALE void                           InsertCommandString(void *,EXEC_STATUS,char *,unsigned);
   LOCALE char                          *GetCommandString(void *,EXEC_STATUS);
   LOCALE int                            CompleteCommand(char *);
   LOCALE void                           CommandLoop(void *,EXEC_STATUS);
   LOCALE void                           CommandLoopBatch(void *,EXEC_STATUS);
   LOCALE void                           CommandLoopBatchDriver(void *,EXEC_STATUS);
   LOCALE void                           PrintPrompt(void *,EXEC_STATUS);
   LOCALE void                           PrintBanner(void *,EXEC_STATUS);
   LOCALE void                           SetAfterPromptFunction(void *,EXEC_STATUS,int (*)(void *,EXEC_STATUS));
   LOCALE void                           SetBeforeCommandExecutionFunction(void *,EXEC_STATUS,int (*)(void *,EXEC_STATUS));
   LOCALE intBool                        RouteCommand(void *,EXEC_STATUS,char *,int);
   LOCALE int                          (*SetEventFunction(void *,EXEC_STATUS,int (*)(void *,EXEC_STATUS)))(void *,EXEC_STATUS);
   LOCALE intBool                        TopLevelCommand(void *,EXEC_STATUS);
   LOCALE void                           AppendNCommandString(void *,EXEC_STATUS,char *,unsigned);
   LOCALE void                           SetNCommandString(void *,EXEC_STATUS,char *,unsigned);
   LOCALE char                          *GetCommandCompletionString(void *,EXEC_STATUS,char *,size_t);
   LOCALE intBool                        ExecuteIfCommandComplete(void *,EXEC_STATUS);
   LOCALE void                           CommandLoopOnceThenBatch(void *,EXEC_STATUS);
   LOCALE intBool                        CommandCompleteAndNotEmpty(void *,EXEC_STATUS);
   LOCALE void                           SetHaltCommandLoopBatch(void *,EXEC_STATUS,int);
   LOCALE int                            GetHaltCommandLoopBatch(void *,EXEC_STATUS);

#endif






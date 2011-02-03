   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*              FILE COMMANDS HEADER FILE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for file commands including    */
/*   batch, dribble-on, dribble-off, save, load, bsave, and  */
/*   bload.                                                  */
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
/*************************************************************/

#ifndef _H_filecom

#define _H_filecom

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _FILECOM_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define DribbleActive() EnvDribbleActive(GetCurrentEnvironment(),GetCurrentExecutionState())
#define DribbleOn(a) EnvDribbleOn(GetCurrentEnvironment(),GetCurrentExecutionState(),a)
#define DribbleOff() EnvDribbleOff(GetCurrentEnvironment(),GetCurrentExecutionState())
#define BatchStar(a) EnvBatchStar(GetCurrentEnvironment(),GetCurrentExecutionState(),a)

# include "execution_status.h"

   LOCALE void                           FileCommandDefinitions(void *,EXEC_STATUS);
   LOCALE intBool                        EnvDribbleOn(void *,EXEC_STATUS,char *);
   LOCALE intBool                        EnvDribbleActive(void *,EXEC_STATUS);
   LOCALE intBool                        EnvDribbleOff(void *,EXEC_STATUS);
   LOCALE void                           SetDribbleStatusFunction(void *,EXEC_STATUS,int (*)(void *,EXEC_STATUS,int));
   LOCALE int                            LLGetcBatch(void *,EXEC_STATUS,char *,int);
   LOCALE int                            Batch(void *,EXEC_STATUS,char *);
   LOCALE int                            OpenBatch(void *,EXEC_STATUS,char *,int);
   LOCALE int                            OpenStringBatch(void *,EXEC_STATUS,char *,char *,int);
   LOCALE int                            RemoveBatch(void *,EXEC_STATUS);
   LOCALE intBool                        BatchActive(void *,EXEC_STATUS);
   LOCALE void                           CloseAllBatchSources(void *,EXEC_STATUS);
   LOCALE int                            BatchCommand(void *,EXEC_STATUS);
   LOCALE int                            BatchStarCommand(void *,EXEC_STATUS);
   LOCALE int                            EnvBatchStar(void *,EXEC_STATUS,char *);
   LOCALE int                            LoadCommand(void *,EXEC_STATUS);
   LOCALE int                            LoadStarCommand(void *,EXEC_STATUS);
   LOCALE int                            SaveCommand(void *,EXEC_STATUS);
   LOCALE int                            DribbleOnCommand(void *,EXEC_STATUS);
   LOCALE int                            DribbleOffCommand(void *,EXEC_STATUS);

#endif







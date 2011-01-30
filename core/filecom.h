   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
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

   LOCALE void                           FileCommandDefinitions(void);
   LOCALE DllExport BOOLEAN              DribbleOn(char *);
   LOCALE BOOLEAN                        DribbleActive(void);
   LOCALE DllExport BOOLEAN              DribbleOff(void);
   LOCALE void                           SetDribbleStatusFunction(int (*)(int));
   LOCALE int                            LLGetcBatch(char *,int);
   LOCALE DllExport int                  Batch(char *);
   LOCALE int                            OpenBatch(char *,int);
   LOCALE int                            OpenStringBatch(char *,char *,int);
   LOCALE int                            RemoveBatch(void);
   LOCALE BOOLEAN                        BatchActive(void);
   LOCALE void                           CloseAllBatchSources(void);
   LOCALE int                            BatchCommand(void);
   LOCALE int                            BatchStarCommand(void);
   LOCALE DllExport int                  BatchStar(char *);
   LOCALE int                            LoadCommand(void);
   LOCALE int                            LoadStarCommand(void);
   LOCALE int                            SaveCommand(void);
   LOCALE int                            DribbleOnCommand(void);
   LOCALE int                            DribbleOffCommand(void);

#endif







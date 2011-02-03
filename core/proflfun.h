   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24 06/05/06             */
   /*                                                     */
   /*      CONSTRUCT PROFILING FUNCTIONS HEADER FILE      */
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
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#ifndef _H_proflfun

#define _H_proflfun

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _PROFLFUN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#include "userdata.h"

struct constructProfileInfo
  {
   struct userData usrData;
   long numberOfEntries;
   unsigned int childCall : 1;
   double startTime;
   double totalSelfTime;
   double totalWithChildrenTime;
  };

struct profileFrameInfo
  {
   unsigned int parentCall : 1;
   unsigned int profileOnExit : 1;
   double parentStartTime;
   struct constructProfileInfo *oldProfileFrame;
  };
  
#define PROFLFUN_DATA 15

struct profileFunctionData
  { 
   double ProfileStartTime;
   double ProfileEndTime;
   double ProfileTotalTime;
   int LastProfileInfo;
   double PercentThreshold;
   struct userDataRecord ProfileDataInfo;
   unsigned char ProfileDataID;
   int ProfileUserFunctions;
   int ProfileConstructs;
   struct constructProfileInfo *ActiveProfileFrame;
   char *OutputString;
  };

#define ProfileFunctionData(theEnv,execStatus) ((struct profileFunctionData *) GetEnvironmentData(theEnv,execStatus,PROFLFUN_DATA))

   LOCALE void                           ConstructProfilingFunctionDefinitions(void *,EXEC_STATUS);
   LOCALE void                           ProfileCommand(void *,EXEC_STATUS);
   LOCALE void                           ProfileInfoCommand(void *,EXEC_STATUS);
   LOCALE void                           StartProfile(void *,EXEC_STATUS,
                                                      struct profileFrameInfo *,
                                                      struct userData **,
                                                      intBool);
   LOCALE void                           EndProfile(void *,EXEC_STATUS,struct profileFrameInfo *);
   LOCALE void                           ProfileResetCommand(void *,EXEC_STATUS);
   LOCALE void                           ResetProfileInfo(struct constructProfileInfo *);

   LOCALE double                         SetProfilePercentThresholdCommand(void *,EXEC_STATUS);
   LOCALE double                         SetProfilePercentThreshold(void *,EXEC_STATUS,double);
   LOCALE double                         GetProfilePercentThresholdCommand(void *,EXEC_STATUS);
   LOCALE double                         GetProfilePercentThreshold(void *,EXEC_STATUS);
   LOCALE intBool                        Profile(void *,EXEC_STATUS,char *);
   LOCALE void                           DeleteProfileData(void *,EXEC_STATUS,void *);
   LOCALE void                          *CreateProfileData(void *,EXEC_STATUS);
   LOCALE char                          *SetProfileOutputString(void *,EXEC_STATUS,char *);

#endif



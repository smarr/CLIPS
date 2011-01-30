   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10 01/31/97             */
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

   LOCALE void                           ConstructProfilingFunctionDefinitions(void);
   LOCALE void                           ProfileCommand(void);
   LOCALE void                           ProfileInfoCommand(void);
   LOCALE void                           StartProfile(struct profileFrameInfo *,
                                                      struct userData **,
                                                      BOOLEAN);
   LOCALE void                           EndProfile(struct profileFrameInfo *);
   LOCALE void                           ProfileResetCommand(void);
   LOCALE void                           ResetProfileInfo(struct constructProfileInfo *);

   LOCALE double                         SetProfilePercentThresholdCommand(void);
   LOCALE double                         SetProfilePercentThreshold(double);
   LOCALE double                         GetProfilePercentThresholdCommand(void);
   LOCALE double                         GetProfilePercentThreshold(void);
   LOCALE BOOLEAN                        Profile(char *);
   LOCALE void                           DeleteProfileData(void *);
   LOCALE void                          *CreateProfileData(void);

#ifndef _PROFLFUN_SOURCE_
   extern Thread struct constructProfileInfo   *ActiveProfileFrame;
   extern Thread int                            ProfileUserFunctions;
   extern Thread int                            ProfileConstructs;
   extern Thread unsigned char                  ProfileDataID;
#endif

#endif







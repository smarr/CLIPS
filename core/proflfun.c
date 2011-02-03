   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/02/06            */
   /*                                                     */
   /*         CONSTRUCT PROFILING FUNCTIONS MODULE        */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for profiling the amount of    */
/*   time spent in constructs and user defined functions.    */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*      6.23: Modified OutputProfileInfo to allow a before   */
/*            and after prefix so that a string buffer does  */
/*            not need to be created to contain the entire   */
/*            prefix. This allows a buffer overflow problem  */
/*            to be corrected. DR0857.                       */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added pragmas to remove compilation warnings.  */
/*                                                           */
/*            Corrected code to remove run-time program      */
/*            compiler warnings.                             */
/*                                                           */
/*************************************************************/

#define _PROFLFUN_SOURCE_

#include "setup.h"

#if PROFILING_FUNCTIONS

#include "argacces.h"
#include "classcom.h"
#include "dffnxfun.h"
#include "envrnmnt.h"
#include "extnfunc.h"
#include "genrccom.h"
#include "genrcfun.h"
#include "memalloc.h"
#include "msgcom.h"
#include "router.h"
#include "sysdep.h"

#include "proflfun.h"

#include <string.h>

#define NO_PROFILE      0
#define USER_FUNCTIONS  1
#define CONSTRUCTS_CODE 2

#define OUTPUT_STRING "%-40s %7ld %15.6f  %8.2f%%  %15.6f  %8.2f%%\n"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static intBool                     OutputProfileInfo(void *,EXEC_STATUS,char *,struct constructProfileInfo *,
                                                        char *,char *,char *,char **);
   static void                        OutputUserFunctionsInfo(void *,EXEC_STATUS);
   static void                        OutputConstructsCodeInfo(void *,EXEC_STATUS);
#if (! RUN_TIME)
   static void                        ProfileClearFunction(void *,EXEC_STATUS);
#endif

/******************************************************/
/* ConstructProfilingFunctionDefinitions: Initializes */
/*   the construct profiling functions.               */
/******************************************************/
globle void ConstructProfilingFunctionDefinitions(
  void *theEnv,
  EXEC_STATUS)
  {
   struct userDataRecord profileDataInfo = { 0, CreateProfileData, DeleteProfileData };

   AllocateEnvironmentData(theEnv,execStatus,PROFLFUN_DATA,sizeof(struct profileFunctionData),NULL);

   memcpy(&ProfileFunctionData(theEnv,execStatus)->ProfileDataInfo,&profileDataInfo,sizeof(struct userDataRecord));   
   
   ProfileFunctionData(theEnv,execStatus)->LastProfileInfo = NO_PROFILE;
   ProfileFunctionData(theEnv,execStatus)->PercentThreshold = 0.0;
   ProfileFunctionData(theEnv,execStatus)->OutputString = OUTPUT_STRING;

#if ! RUN_TIME
   EnvDefineFunction2(theEnv,execStatus,"profile",'v', PTIEF ProfileCommand,"ProfileCommand","11w");
   EnvDefineFunction2(theEnv,execStatus,"profile-info",'v', PTIEF ProfileInfoCommand,"ProfileInfoCommand","01w");
   EnvDefineFunction2(theEnv,execStatus,"profile-reset",'v', PTIEF ProfileResetCommand,"ProfileResetCommand","00");

   EnvDefineFunction2(theEnv,execStatus,"set-profile-percent-threshold",'d',
                   PTIEF SetProfilePercentThresholdCommand,
                   "SetProfilePercentThresholdCommand","11n");
   EnvDefineFunction2(theEnv,execStatus,"get-profile-percent-threshold",'d',
                   PTIEF GetProfilePercentThresholdCommand,
                   "GetProfilePercentThresholdCommand","00");
                   
   ProfileFunctionData(theEnv,execStatus)->ProfileDataID = InstallUserDataRecord(theEnv,execStatus,&ProfileFunctionData(theEnv,execStatus)->ProfileDataInfo);
   
   EnvAddClearFunction(theEnv,execStatus,"profile",ProfileClearFunction,0);
#endif
  }

/**********************************/
/* CreateProfileData: Allocates a */
/*   profile user data structure. */
/**********************************/
globle void *CreateProfileData(
  void *theEnv,
  EXEC_STATUS)
  {
   struct constructProfileInfo *theInfo;
   
   theInfo = (struct constructProfileInfo *)
             genalloc(theEnv,execStatus,sizeof(struct constructProfileInfo));

   theInfo->numberOfEntries = 0;
   theInfo->childCall = FALSE;
   theInfo->startTime = 0.0;
   theInfo->totalSelfTime = 0.0;
   theInfo->totalWithChildrenTime = 0.0;
   
   return(theInfo);
  }
  
/**************************************/
/* DeleteProfileData:          */
/**************************************/
globle void DeleteProfileData(
  void *theEnv,
  EXEC_STATUS,
  void *theData)
  {
   genfree(theEnv,execStatus,theData,sizeof(struct constructProfileInfo));
  }

/**************************************/
/* ProfileCommand: H/L access routine */
/*   for the profile command.         */
/**************************************/
globle void ProfileCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   char *argument;
   DATA_OBJECT theValue;

   if (EnvArgCountCheck(theEnv,execStatus,"profile",EXACTLY,1) == -1) return;
   if (EnvArgTypeCheck(theEnv,execStatus,"profile",1,SYMBOL,&theValue) == FALSE) return;

   argument = DOToString(theValue);

   if (! Profile(theEnv,execStatus,argument))
     {
      ExpectedTypeError1(theEnv,execStatus,"profile",1,"symbol with value constructs, user-functions, or off");
      return;
     }

   return;
  }

/******************************/
/* Profile: C access routine  */
/*   for the profile command. */
/******************************/
globle intBool Profile(
  void *theEnv,
  EXEC_STATUS,
  char *argument)
  {
   /*======================================================*/
   /* If the argument is the symbol "user-functions", then */
   /* user-defined functions should be profiled. If the    */
   /* argument is the symbol "constructs", then            */
   /* deffunctions, generic functions, message-handlers,   */
   /* and rule RHS actions are profiled.                   */
   /*======================================================*/

   if (strcmp(argument,"user-functions") == 0)
     {
      ProfileFunctionData(theEnv,execStatus)->ProfileStartTime = gentime();
      ProfileFunctionData(theEnv,execStatus)->ProfileUserFunctions = TRUE;
      ProfileFunctionData(theEnv,execStatus)->ProfileConstructs = FALSE;
      ProfileFunctionData(theEnv,execStatus)->LastProfileInfo = USER_FUNCTIONS;
     }

   else if (strcmp(argument,"constructs") == 0)
     {
      ProfileFunctionData(theEnv,execStatus)->ProfileStartTime = gentime();
      ProfileFunctionData(theEnv,execStatus)->ProfileUserFunctions = FALSE;
      ProfileFunctionData(theEnv,execStatus)->ProfileConstructs = TRUE;
      ProfileFunctionData(theEnv,execStatus)->LastProfileInfo = CONSTRUCTS_CODE;
     }

   /*======================================================*/
   /* Otherwise, if the argument is the symbol "off", then */
   /* don't profile constructs and user-defined functions. */
   /*======================================================*/

   else if (strcmp(argument,"off") == 0)
     {
      ProfileFunctionData(theEnv,execStatus)->ProfileEndTime = gentime();
      ProfileFunctionData(theEnv,execStatus)->ProfileTotalTime += (ProfileFunctionData(theEnv,execStatus)->ProfileEndTime - ProfileFunctionData(theEnv,execStatus)->ProfileStartTime);
      ProfileFunctionData(theEnv,execStatus)->ProfileUserFunctions = FALSE;
      ProfileFunctionData(theEnv,execStatus)->ProfileConstructs = FALSE;
     }

   /*=====================================================*/
   /* Otherwise, generate an error since the only allowed */
   /* arguments are "on" or "off."                        */
   /*=====================================================*/

   else
     { return(FALSE); }

   return(TRUE);
  }

/******************************************/
/* ProfileInfoCommand: H/L access routine */
/*   for the profile-info command.        */
/******************************************/
globle void ProfileInfoCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   int argCount;
   DATA_OBJECT theValue;
   char buffer[512];
   
   /*===================================*/
   /* The profile-info command expects  */
   /* at most a single symbol argument. */
   /*===================================*/

   if ((argCount = EnvArgCountCheck(theEnv,execStatus,"profile",NO_MORE_THAN,1)) == -1) return;

   /*===========================================*/
   /* The first profile-info argument indicates */
   /* the field on which sorting is performed.  */
   /*===========================================*/

   if (argCount == 1)
     {
      if (EnvArgTypeCheck(theEnv,execStatus,"profile",1,SYMBOL,&theValue) == FALSE) return;
     }

   /*==================================*/
   /* If code is still being profiled, */
   /* update the profile end time.     */
   /*==================================*/

   if (ProfileFunctionData(theEnv,execStatus)->ProfileUserFunctions || ProfileFunctionData(theEnv,execStatus)->ProfileConstructs)
     {
      ProfileFunctionData(theEnv,execStatus)->ProfileEndTime = gentime();
      ProfileFunctionData(theEnv,execStatus)->ProfileTotalTime += (ProfileFunctionData(theEnv,execStatus)->ProfileEndTime - ProfileFunctionData(theEnv,execStatus)->ProfileStartTime);
     }
      
   /*==================================*/
   /* Print the profiling information. */
   /*==================================*/
      
   if (ProfileFunctionData(theEnv,execStatus)->LastProfileInfo != NO_PROFILE)
     {
      gensprintf(buffer,"Profile elapsed time = %g seconds\n",
                      ProfileFunctionData(theEnv,execStatus)->ProfileTotalTime);
      EnvPrintRouter(theEnv,execStatus,WDISPLAY,buffer);

      if (ProfileFunctionData(theEnv,execStatus)->LastProfileInfo == USER_FUNCTIONS)
        { EnvPrintRouter(theEnv,execStatus,WDISPLAY,"Function Name                            "); }
      else if (ProfileFunctionData(theEnv,execStatus)->LastProfileInfo == CONSTRUCTS_CODE)
        { EnvPrintRouter(theEnv,execStatus,WDISPLAY,"Construct Name                           "); }            
      
      EnvPrintRouter(theEnv,execStatus,WDISPLAY,"Entries         Time           %          Time+Kids     %+Kids\n");

      if (ProfileFunctionData(theEnv,execStatus)->LastProfileInfo == USER_FUNCTIONS)
        { EnvPrintRouter(theEnv,execStatus,WDISPLAY,"-------------                            "); }
      else if (ProfileFunctionData(theEnv,execStatus)->LastProfileInfo == CONSTRUCTS_CODE)
        { EnvPrintRouter(theEnv,execStatus,WDISPLAY,"--------------                           "); }

      EnvPrintRouter(theEnv,execStatus,WDISPLAY,"-------        ------        -----        ---------     ------\n");
     }

   if (ProfileFunctionData(theEnv,execStatus)->LastProfileInfo == USER_FUNCTIONS) OutputUserFunctionsInfo(theEnv,execStatus);
   if (ProfileFunctionData(theEnv,execStatus)->LastProfileInfo == CONSTRUCTS_CODE) OutputConstructsCodeInfo(theEnv,execStatus);
  }

/**********************************************/
/* StartProfile: Initiates bookkeeping needed */
/*   to profile a construct or function.      */
/**********************************************/
globle void StartProfile(
  void *theEnv,
  EXEC_STATUS,
  struct profileFrameInfo *theFrame,
  struct userData **theList,
  intBool checkFlag)
  {
   double startTime, addTime;
   struct constructProfileInfo *profileInfo;

   if (! checkFlag)
     {
      theFrame->profileOnExit = FALSE;
      return;
     }

   profileInfo = (struct constructProfileInfo *) FetchUserData(theEnv,execStatus,ProfileFunctionData(theEnv,execStatus)->ProfileDataID,theList);
                
   theFrame->profileOnExit = TRUE;
   theFrame->parentCall = FALSE;

   startTime = gentime();
   theFrame->oldProfileFrame = ProfileFunctionData(theEnv,execStatus)->ActiveProfileFrame;

   if (ProfileFunctionData(theEnv,execStatus)->ActiveProfileFrame != NULL)
     {
      addTime = startTime - ProfileFunctionData(theEnv,execStatus)->ActiveProfileFrame->startTime;
      ProfileFunctionData(theEnv,execStatus)->ActiveProfileFrame->totalSelfTime += addTime;
     }

   ProfileFunctionData(theEnv,execStatus)->ActiveProfileFrame = profileInfo;

   ProfileFunctionData(theEnv,execStatus)->ActiveProfileFrame->numberOfEntries++;
   ProfileFunctionData(theEnv,execStatus)->ActiveProfileFrame->startTime = startTime;

   if (! ProfileFunctionData(theEnv,execStatus)->ActiveProfileFrame->childCall)
     {
      theFrame->parentCall = TRUE;
      theFrame->parentStartTime = startTime;
      ProfileFunctionData(theEnv,execStatus)->ActiveProfileFrame->childCall = TRUE;
     }
  }

/*******************************************/
/* EndProfile: Finishes bookkeeping needed */
/*   to profile a construct or function.   */
/*******************************************/
globle void EndProfile(
  void *theEnv,
  EXEC_STATUS,
  struct profileFrameInfo *theFrame)
  {
   double endTime, addTime;

   if (! theFrame->profileOnExit) return;

   endTime = gentime();

   if (theFrame->parentCall)
     {
      addTime = endTime - theFrame->parentStartTime;
      ProfileFunctionData(theEnv,execStatus)->ActiveProfileFrame->totalWithChildrenTime += addTime;
      ProfileFunctionData(theEnv,execStatus)->ActiveProfileFrame->childCall = FALSE;
     }

   ProfileFunctionData(theEnv,execStatus)->ActiveProfileFrame->totalSelfTime += (endTime - ProfileFunctionData(theEnv,execStatus)->ActiveProfileFrame->startTime);

   if (theFrame->oldProfileFrame != NULL)
     { theFrame->oldProfileFrame->startTime = endTime; }

   ProfileFunctionData(theEnv,execStatus)->ActiveProfileFrame = theFrame->oldProfileFrame;
  }

/******************************************/
/* OutputProfileInfo: Prints out a single */
/*   line of profile information.         */
/******************************************/
static intBool OutputProfileInfo(
  void *theEnv,
  EXEC_STATUS,
  char *itemName,
  struct constructProfileInfo *profileInfo,
  char *printPrefixBefore,
  char *printPrefix,
  char *printPrefixAfter,
  char **banner)
  {
   double percent = 0.0, percentWithKids = 0.0;
   char buffer[512];
   
   if (profileInfo == NULL) return(FALSE);
   
   if (profileInfo->numberOfEntries == 0) return(FALSE);

   if (ProfileFunctionData(theEnv,execStatus)->ProfileTotalTime != 0.0)
     {
      percent = (profileInfo->totalSelfTime * 100.0) / ProfileFunctionData(theEnv,execStatus)->ProfileTotalTime;
      if (percent < 0.005) percent = 0.0;
      percentWithKids = (profileInfo->totalWithChildrenTime * 100.0) / ProfileFunctionData(theEnv,execStatus)->ProfileTotalTime;
      if (percentWithKids < 0.005) percentWithKids = 0.0;
     }

   if (percent < ProfileFunctionData(theEnv,execStatus)->PercentThreshold) return(FALSE);

   if ((banner != NULL) && (*banner != NULL))
     {
      EnvPrintRouter(theEnv,execStatus,WDISPLAY,*banner);
      *banner = NULL;
     }

   if (printPrefixBefore != NULL)
     { EnvPrintRouter(theEnv,execStatus,WDISPLAY,printPrefixBefore); }
   
   if (printPrefix != NULL)
     { EnvPrintRouter(theEnv,execStatus,WDISPLAY,printPrefix); }

   if (printPrefixAfter != NULL)
     { EnvPrintRouter(theEnv,execStatus,WDISPLAY,printPrefixAfter); }

   if (strlen(itemName) >= 40)
     {
      EnvPrintRouter(theEnv,execStatus,WDISPLAY,itemName);
      EnvPrintRouter(theEnv,execStatus,WDISPLAY,"\n");
      itemName = "";
     }

   gensprintf(buffer,ProfileFunctionData(theEnv,execStatus)->OutputString,
                        itemName,
                        (long) profileInfo->numberOfEntries,

                        (double) profileInfo->totalSelfTime,
                        (double) percent,

                        (double) profileInfo->totalWithChildrenTime,
                        (double) percentWithKids);
   EnvPrintRouter(theEnv,execStatus,WDISPLAY,buffer);

   return(TRUE);
  }

/*******************************************/
/* ProfileResetCommand: H/L access routine */
/*   for the profile-reset command.        */
/*******************************************/
globle void ProfileResetCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   struct FunctionDefinition *theFunction;
   int i;
#if DEFFUNCTION_CONSTRUCT
   DEFFUNCTION *theDeffunction;
#endif
#if DEFRULE_CONSTRUCT
   struct defrule *theDefrule;
#endif
#if DEFGENERIC_CONSTRUCT
   DEFGENERIC *theDefgeneric;
   unsigned int methodIndex;
   DEFMETHOD *theMethod;
#endif
#if OBJECT_SYSTEM
   DEFCLASS *theDefclass;
   HANDLER *theHandler;
   unsigned handlerIndex;
#endif
   
   ProfileFunctionData(theEnv,execStatus)->ProfileStartTime = 0.0;
   ProfileFunctionData(theEnv,execStatus)->ProfileEndTime = 0.0;
   ProfileFunctionData(theEnv,execStatus)->ProfileTotalTime = 0.0;
   ProfileFunctionData(theEnv,execStatus)->LastProfileInfo = NO_PROFILE;

   for (theFunction = GetFunctionList(theEnv,execStatus);
        theFunction != NULL;
        theFunction = theFunction->next)
     { 
      ResetProfileInfo((struct constructProfileInfo *)
                       TestUserData(ProfileFunctionData(theEnv,execStatus)->ProfileDataID,theFunction->usrData));
     }

   for (i = 0; i < MAXIMUM_PRIMITIVES; i++)
     {
      if (EvaluationData(theEnv,execStatus)->PrimitivesArray[i] != NULL)
        {  
         ResetProfileInfo((struct constructProfileInfo *)
                          TestUserData(ProfileFunctionData(theEnv,execStatus)->ProfileDataID,EvaluationData(theEnv,execStatus)->PrimitivesArray[i]->usrData));
        }
     }

#if DEFFUNCTION_CONSTRUCT
   for (theDeffunction = (DEFFUNCTION *) EnvGetNextDeffunction(theEnv,execStatus,NULL);
        theDeffunction != NULL;
        theDeffunction = (DEFFUNCTION *) EnvGetNextDeffunction(theEnv,execStatus,theDeffunction))
     { 
      ResetProfileInfo((struct constructProfileInfo *)
                       TestUserData(ProfileFunctionData(theEnv,execStatus)->ProfileDataID,theDeffunction->header.usrData)); 
     }
#endif

#if DEFRULE_CONSTRUCT
   for (theDefrule = (struct defrule *) EnvGetNextDefrule(theEnv,execStatus,NULL);
        theDefrule != NULL;
        theDefrule = (struct defrule *) EnvGetNextDefrule(theEnv,execStatus,theDefrule))
     { 
      ResetProfileInfo((struct constructProfileInfo *)
                       TestUserData(ProfileFunctionData(theEnv,execStatus)->ProfileDataID,theDefrule->header.usrData)); 
     }
#endif

#if DEFGENERIC_CONSTRUCT
   for (theDefgeneric = (DEFGENERIC *) EnvGetNextDefgeneric(theEnv,execStatus,NULL);
        theDefgeneric != NULL;
        theDefgeneric = (DEFGENERIC *) EnvGetNextDefgeneric(theEnv,execStatus,theDefgeneric))
     {
      ResetProfileInfo((struct constructProfileInfo *)
                       TestUserData(ProfileFunctionData(theEnv,execStatus)->ProfileDataID,theDefgeneric->header.usrData)); 
      
      for (methodIndex = EnvGetNextDefmethod(theEnv,execStatus,theDefgeneric,0);
           methodIndex != 0;
           methodIndex = EnvGetNextDefmethod(theEnv,execStatus,theDefgeneric,methodIndex))
        {
         theMethod = GetDefmethodPointer(theDefgeneric,methodIndex);
         ResetProfileInfo((struct constructProfileInfo *)
                          TestUserData(ProfileFunctionData(theEnv,execStatus)->ProfileDataID,theMethod->usrData)); 
        }
     }
#endif

#if OBJECT_SYSTEM
   for (theDefclass = (DEFCLASS *) EnvGetNextDefclass(theEnv,execStatus,NULL);
        theDefclass != NULL;
        theDefclass = (DEFCLASS *) EnvGetNextDefclass(theEnv,execStatus,theDefclass))
     {
      ResetProfileInfo((struct constructProfileInfo *)
                       TestUserData(ProfileFunctionData(theEnv,execStatus)->ProfileDataID,theDefclass->header.usrData)); 
      for (handlerIndex = EnvGetNextDefmessageHandler(theEnv,execStatus,theDefclass,0);
           handlerIndex != 0;
           handlerIndex = EnvGetNextDefmessageHandler(theEnv,execStatus,theDefclass,handlerIndex))
        {
         theHandler = GetDefmessageHandlerPointer(theDefclass,handlerIndex);
         ResetProfileInfo((struct constructProfileInfo *)
                          TestUserData(ProfileFunctionData(theEnv,execStatus)->ProfileDataID,theHandler->usrData)); 
        }
     }
#endif

  }

/*************************************************/
/* ResetProfileInfo: Sets the initial values for */
/*   a constructProfileInfo data structure.      */
/*************************************************/
globle void ResetProfileInfo(
  struct constructProfileInfo *profileInfo)
  {
   if (profileInfo == NULL) return;
   
   profileInfo->numberOfEntries = 0;
   profileInfo->childCall = FALSE;
   profileInfo->startTime = 0.0;
   profileInfo->totalSelfTime = 0.0;
   profileInfo->totalWithChildrenTime = 0.0;
  }

/*************************************************/
/* OutputUserFunctionsInfo:       */
/*************************************************/
static void OutputUserFunctionsInfo(
  void *theEnv,
  EXEC_STATUS)
  {
   struct FunctionDefinition *theFunction;
   int i;

   for (theFunction = GetFunctionList(theEnv,execStatus);
        theFunction != NULL;
        theFunction = theFunction->next)
     {
      OutputProfileInfo(theEnv,execStatus,ValueToString(theFunction->callFunctionName),
                        (struct constructProfileInfo *) 
                           TestUserData(ProfileFunctionData(theEnv,execStatus)->ProfileDataID,
                        theFunction->usrData),
                        NULL,NULL,NULL,NULL);
     }

   for (i = 0; i < MAXIMUM_PRIMITIVES; i++)
     {
      if (EvaluationData(theEnv,execStatus)->PrimitivesArray[i] != NULL)
        {
         OutputProfileInfo(theEnv,execStatus,EvaluationData(theEnv,execStatus)->PrimitivesArray[i]->name,
                           (struct constructProfileInfo *)
                              TestUserData(ProfileFunctionData(theEnv,execStatus)->ProfileDataID,
                           EvaluationData(theEnv,execStatus)->PrimitivesArray[i]->usrData),
                           NULL,NULL,NULL,NULL);
        }
     }
  }

/*************************************************/
/* OutputConstructsCodeInfo:       */
/*************************************************/
#if WIN_BTC && (! DEFFUNCTION_CONSTRUCT) && (! DEFGENERIC_CONSTRUCT) && (! OBJECT_SYSTEM) && (! DEFRULE_CONSTRUCT)
#pragma argsused
#endif
static void OutputConstructsCodeInfo(
  void *theEnv,
  EXEC_STATUS)
  {
#if (! DEFFUNCTION_CONSTRUCT) && (! DEFGENERIC_CONSTRUCT) && (! OBJECT_SYSTEM) && (! DEFRULE_CONSTRUCT)
#pragma unused(theEnv,execStatus)
#endif
#if DEFFUNCTION_CONSTRUCT
   DEFFUNCTION *theDeffunction;
#endif
#if DEFRULE_CONSTRUCT
   struct defrule *theDefrule;
#endif
#if DEFGENERIC_CONSTRUCT
   DEFGENERIC *theDefgeneric;
   DEFMETHOD *theMethod;
   unsigned methodIndex;
   char methodBuffer[512];
#endif
#if OBJECT_SYSTEM
   DEFCLASS *theDefclass;
   HANDLER *theHandler;
   unsigned handlerIndex;
#endif
#if DEFGENERIC_CONSTRUCT || OBJECT_SYSTEM
   char *prefix, *prefixBefore, *prefixAfter;
#endif
   char *banner;

   banner = "\n*** Deffunctions ***\n\n";

#if DEFFUNCTION_CONSTRUCT
   for (theDeffunction = (DEFFUNCTION *) EnvGetNextDeffunction(theEnv,execStatus,NULL);
        theDeffunction != NULL;
        theDeffunction = (DEFFUNCTION *) EnvGetNextDeffunction(theEnv,execStatus,theDeffunction))
     {
      OutputProfileInfo(theEnv,execStatus,EnvGetDeffunctionName(theEnv,execStatus,theDeffunction),
                        (struct constructProfileInfo *) 
                          TestUserData(ProfileFunctionData(theEnv,execStatus)->ProfileDataID,theDeffunction->header.usrData),
                        NULL,NULL,NULL,&banner);
     }
#endif

   banner = "\n*** Defgenerics ***\n";
#if DEFGENERIC_CONSTRUCT
   for (theDefgeneric = (DEFGENERIC *) EnvGetNextDefgeneric(theEnv,execStatus,NULL);
        theDefgeneric != NULL;
        theDefgeneric = (DEFGENERIC *) EnvGetNextDefgeneric(theEnv,execStatus,theDefgeneric))
     {
      prefixBefore = "\n";
      prefix = EnvGetDefgenericName(theEnv,execStatus,theDefgeneric);
      prefixAfter = "\n";

      for (methodIndex = EnvGetNextDefmethod(theEnv,execStatus,theDefgeneric,0);
           methodIndex != 0;
           methodIndex = EnvGetNextDefmethod(theEnv,execStatus,theDefgeneric,methodIndex))
        {
         theMethod = GetDefmethodPointer(theDefgeneric,methodIndex);

         EnvGetDefmethodDescription(theEnv,execStatus,methodBuffer,510,theDefgeneric,methodIndex);
         if (OutputProfileInfo(theEnv,execStatus,methodBuffer,
                               (struct constructProfileInfo *) 
                                  TestUserData(ProfileFunctionData(theEnv,execStatus)->ProfileDataID,theMethod->usrData),
                               prefixBefore,prefix,prefixAfter,&banner))
           {
            prefixBefore = NULL; 
            prefix = NULL; 
            prefixAfter = NULL;
           }
        }
     }
#endif

   banner = "\n*** Defclasses ***\n";
#if OBJECT_SYSTEM
   for (theDefclass = (DEFCLASS *) EnvGetNextDefclass(theEnv,execStatus,NULL);
        theDefclass != NULL;
        theDefclass = (DEFCLASS *) EnvGetNextDefclass(theEnv,execStatus,theDefclass))
     {
      prefixAfter = "\n";
      prefix = EnvGetDefclassName(theEnv,execStatus,theDefclass);
      prefixBefore = "\n";
      
      for (handlerIndex = EnvGetNextDefmessageHandler(theEnv,execStatus,theDefclass,0);
           handlerIndex != 0;
           handlerIndex = EnvGetNextDefmessageHandler(theEnv,execStatus,theDefclass,handlerIndex))
        {
         theHandler = GetDefmessageHandlerPointer(theDefclass,handlerIndex);
         if (OutputProfileInfo(theEnv,execStatus,EnvGetDefmessageHandlerName(theEnv,execStatus,theDefclass,handlerIndex),
                               (struct constructProfileInfo *) 
                                  TestUserData(ProfileFunctionData(theEnv,execStatus)->ProfileDataID,
                               theHandler->usrData),
                               prefixBefore,prefix,prefixAfter,&banner))
           {
            prefixBefore = NULL; 
            prefix = NULL; 
            prefixAfter = NULL;
           }
        }

     }
#endif

   banner = "\n*** Defrules ***\n\n";

#if DEFRULE_CONSTRUCT
   for (theDefrule = (struct defrule *) EnvGetNextDefrule(theEnv,execStatus,NULL);
        theDefrule != NULL;
        theDefrule = (struct defrule *) EnvGetNextDefrule(theEnv,execStatus,theDefrule))
     {
      OutputProfileInfo(theEnv,execStatus,EnvGetDefruleName(theEnv,execStatus,theDefrule),
                        (struct constructProfileInfo *) 
                          TestUserData(ProfileFunctionData(theEnv,execStatus)->ProfileDataID,theDefrule->header.usrData),
                        NULL,NULL,NULL,&banner);
     }
#endif

  }

/*********************************************************/
/* SetProfilePercentThresholdCommand: H/L access routine */
/*   for the set-profile-percent-threshold command.      */
/*********************************************************/
globle double SetProfilePercentThresholdCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   DATA_OBJECT theValue;
   double newThreshold;
   
   if (EnvArgCountCheck(theEnv,execStatus,"set-profile-percent-threshold",EXACTLY,1) == -1)
     { return(ProfileFunctionData(theEnv,execStatus)->PercentThreshold); }

   if (EnvArgTypeCheck(theEnv,execStatus,"set-profile-percent-threshold",1,INTEGER_OR_FLOAT,&theValue) == FALSE)
      { return(ProfileFunctionData(theEnv,execStatus)->PercentThreshold); }

   if (GetType(theValue) == INTEGER)
     { newThreshold = (double) DOToLong(theValue); }
   else
     { newThreshold = (double) DOToDouble(theValue); }
     
   if ((newThreshold < 0.0) || (newThreshold > 100.0))
     { 
      ExpectedTypeError1(theEnv,execStatus,"set-profile-percent-threshold",1,
                         "number in the range 0 to 100");
      return(-1.0); 
     }

   return(SetProfilePercentThreshold(theEnv,execStatus,newThreshold));
  }

/****************************************************/
/* SetProfilePercentThreshold: C access routine for */
/*   the set-profile-percent-threshold command.     */
/****************************************************/
globle double SetProfilePercentThreshold(
  void *theEnv,
  EXEC_STATUS,
  double value)
  {
   double oldPercentThreshhold;

   if ((value < 0.0) || (value > 100.0))
     { return(-1.0); }
     
   oldPercentThreshhold = ProfileFunctionData(theEnv,execStatus)->PercentThreshold;

   ProfileFunctionData(theEnv,execStatus)->PercentThreshold = value;

   return(oldPercentThreshhold);
  }

/*********************************************************/
/* GetProfilePercentThresholdCommand: H/L access routine */
/*   for the get-profile-percent-threshold command.      */
/*********************************************************/
globle double GetProfilePercentThresholdCommand(
  void *theEnv,
  EXEC_STATUS)
  {   
   EnvArgCountCheck(theEnv,execStatus,"get-profile-percent-threshold",EXACTLY,0);

   return(ProfileFunctionData(theEnv,execStatus)->PercentThreshold);
  }

/****************************************************/
/* GetProfilePercentThreshold: C access routine for */
/*   the get-profile-percent-threshold command.     */
/****************************************************/
globle double GetProfilePercentThreshold(
  void *theEnv,
  EXEC_STATUS)
  {
   return(ProfileFunctionData(theEnv,execStatus)->PercentThreshold);
  }
  
/**********************************************************/
/* SetProfileOutputString: Sets the output string global. */
/**********************************************************/
globle char *SetProfileOutputString(
  void *theEnv,
  EXEC_STATUS,
  char *value)
  {
   char *oldOutputString;

   if (value == NULL)
     { return(ProfileFunctionData(theEnv,execStatus)->OutputString); }
     
   oldOutputString = ProfileFunctionData(theEnv,execStatus)->OutputString;

   ProfileFunctionData(theEnv,execStatus)->OutputString = value;

   return(oldOutputString);
  }

#if (! RUN_TIME)  
/******************************************************************/
/* ProfileClearFunction: Profiling clear routine for use with the */
/*   clear command. Removes user data attached to user functions. */
/******************************************************************/
static void ProfileClearFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   struct FunctionDefinition *theFunction;
   int i;

   for (theFunction = GetFunctionList(theEnv,execStatus);
        theFunction != NULL;
        theFunction = theFunction->next)
     {
      theFunction->usrData = 
        DeleteUserData(theEnv,execStatus,ProfileFunctionData(theEnv,execStatus)->ProfileDataID,theFunction->usrData);
     }

   for (i = 0; i < MAXIMUM_PRIMITIVES; i++)
     {
      if (EvaluationData(theEnv,execStatus)->PrimitivesArray[i] != NULL)
        {
         EvaluationData(theEnv,execStatus)->PrimitivesArray[i]->usrData = 
           DeleteUserData(theEnv,execStatus,ProfileFunctionData(theEnv,execStatus)->ProfileDataID,EvaluationData(theEnv,execStatus)->PrimitivesArray[i]->usrData);
        }
     }
  }
#endif

#endif /* PROFILING_FUNCTIONS */


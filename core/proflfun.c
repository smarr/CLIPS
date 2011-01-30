   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  01/31/97            */
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
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _PROFLFUN_SOURCE_

#include "setup.h"

#if PROFILING_FUNCTIONS

#include "argacces.h"
#include "classcom.h"
#include "dffnxfun.h"
#include "extnfunc.h"
#include "extobj.h"
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

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static BOOLEAN                     OutputProfileInfo(char *,struct constructProfileInfo *,char *,char **);
   static void                        OutputUserFunctionsInfo(void);
   static void                        OutputConstructsCodeInfo(void);
   static void                        ProfileClearFunction(void);

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static double              ProfileStartTime;
   Thread static double              ProfileEndTime;
   Thread static double              ProfileTotalTime;
   Thread static int                 LastProfileInfo = NO_PROFILE;
   Thread static double              PercentThreshold = 0.0;

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle struct constructProfileInfo
                              *ActiveProfileFrame = NULL;
   Thread globle int                  ProfileUserFunctions = FALSE;
   Thread globle int                  ProfileConstructs = FALSE;
   Thread globle unsigned char        ProfileDataID;
   Thread globle struct userDataRecord   
                               
                               ProfileDataInfo = { 0, CreateProfileData, DeleteProfileData };

#if ! RUN_TIME
/******************************************************/
/* ConstructProfilingFunctionDefinitions: Initializes */
/*   the construct profiling functions.               */
/******************************************************/
globle void ConstructProfilingFunctionDefinitions()
  {
   DefineFunction2("profile",'v', PTIF ProfileCommand,"ProfileCommand","11w");
   DefineFunction2("profile-info",'v', PTIF ProfileInfoCommand,"ProfileInfoCommand","01w");
   DefineFunction2("profile-reset",'v', PTIF ProfileResetCommand,"ProfileResetCommand","00");

   DefineFunction2("set-profile-percent-threshold",'d',
                   PTIF SetProfilePercentThresholdCommand,
                   "SetProfilePercentThresholdCommand","11n");
   DefineFunction2("get-profile-percent-threshold",'d',
                   PTIF GetProfilePercentThresholdCommand,
                   "GetProfilePercentThresholdCommand","00");
                   
   ProfileDataID = InstallUserDataRecord(&ProfileDataInfo);
   
   AddClearFunction("profile",ProfileClearFunction,0);
  }
#endif

/**********************************/
/* CreateProfileData: Allocates a */
/*   profile user data structure. */
/**********************************/
globle void *CreateProfileData()
  {
   struct constructProfileInfo *theInfo;
   
   theInfo = (struct constructProfileInfo *)
             genalloc(sizeof(struct constructProfileInfo));

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
  void *theData)
  {
   genfree(theData,sizeof(struct constructProfileInfo));
  }

/**************************************/
/* ProfileCommand: H/L access routine */
/*   for the profile command.         */
/**************************************/
globle void ProfileCommand()
  {
   char *argument;
   DATA_OBJECT theValue;

   if (ArgCountCheck("profile",EXACTLY,1) == -1) return;
   if (ArgTypeCheck("profile",1,SYMBOL,&theValue) == FALSE) return;

   argument = DOToString(theValue);

   if (! Profile(argument))
     {
      ExpectedTypeError1("profile",1,"symbol with value constructs, user-functions, or off");
      return;
     }

   return;
  }

/******************************/
/* Profile: C access routine  */
/*   for the profile command. */
/******************************/
globle BOOLEAN Profile(
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
      ProfileStartTime = gentime();
      ProfileUserFunctions = TRUE;
      ProfileConstructs = FALSE;
      LastProfileInfo = USER_FUNCTIONS;
     }

   else if (strcmp(argument,"constructs") == 0)
     {
      ProfileStartTime = gentime();
      ProfileUserFunctions = FALSE;
      ProfileConstructs = TRUE;
      LastProfileInfo = CONSTRUCTS_CODE;
     }

   /*======================================================*/
   /* Otherwise, if the argument is the symbol "off", then */
   /* don't profile constructs and user-defined functions. */
   /*======================================================*/

   else if (strcmp(argument,"off") == 0)
     {
      ProfileEndTime = gentime();
      ProfileTotalTime += (ProfileEndTime - ProfileStartTime);
      ProfileUserFunctions = FALSE;
      ProfileConstructs = FALSE;
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
globle void ProfileInfoCommand()
  {
   int argCount;
   DATA_OBJECT theValue;
   char buffer[512];

   /*===================================*/
   /* The profile-info command expects  */
   /* at most a single symbol argument. */
   /*===================================*/

   if ((argCount = ArgCountCheck("profile",NO_MORE_THAN,1)) == -1) return;

   /*===========================================*/
   /* The first profile-info argument indicates */
   /* the field on which sorting is performed.  */
   /*===========================================*/

   if (argCount == 1)
     {
      if (ArgTypeCheck("profile",1,SYMBOL,&theValue) == FALSE) return;
     }

   /*==================================*/
   /* If code is still being profiled, */
   /* update the profile end time.     */
   /*==================================*/

   if (ProfileUserFunctions || ProfileConstructs)
     {
      ProfileEndTime = gentime();
      ProfileTotalTime += (ProfileEndTime - ProfileStartTime);
     }
      
   /*==================================*/
   /* Print the profiling information. */
   /*==================================*/
      
   if (LastProfileInfo != NO_PROFILE)
     {
      sprintf(buffer,"Profile elapsed time = %g seconds\n\n",
                      ProfileEndTime - ProfileStartTime);
      PrintRouter(WDISPLAY,buffer);

      if (LastProfileInfo == USER_FUNCTIONS)
        { PrintRouter(WDISPLAY,"Function Name                            "); }
      else if (LastProfileInfo == CONSTRUCTS_CODE)
        { PrintRouter(WDISPLAY,"Construct Name                           "); }            
      
      PrintRouter(WDISPLAY,"Entries         Time           %          Time+Kids     %+Kids\n");

      if (LastProfileInfo == USER_FUNCTIONS)
        { PrintRouter(WDISPLAY,"-------------                            "); }
      else if (LastProfileInfo == CONSTRUCTS_CODE)
        { PrintRouter(WDISPLAY,"--------------                           "); }

      PrintRouter(WDISPLAY,"-------        ------        -----        ---------     ------\n");
     }

   if (LastProfileInfo == USER_FUNCTIONS) OutputUserFunctionsInfo();
   if (LastProfileInfo == CONSTRUCTS_CODE) OutputConstructsCodeInfo();
  }

/**********************************************/
/* StartProfile: Initiates bookkeeping needed */
/*   to profile a construct or function.      */
/**********************************************/
globle void StartProfile(
  struct profileFrameInfo *theFrame,
  struct userData **theList,
  BOOLEAN checkFlag)
  {
   double startTime, addTime;
   struct constructProfileInfo *profileInfo;

   if (! checkFlag)
     {
      theFrame->profileOnExit = FALSE;
      return;
     }

   profileInfo = (struct constructProfileInfo *) FetchUserData(ProfileDataID,theList);
                
   theFrame->profileOnExit = TRUE;
   theFrame->parentCall = FALSE;

   startTime = gentime();
   theFrame->oldProfileFrame = ActiveProfileFrame;

   if (ActiveProfileFrame != NULL)
     {
      addTime = startTime - ActiveProfileFrame->startTime;
      ActiveProfileFrame->totalSelfTime += addTime;
     }

   ActiveProfileFrame = profileInfo;

   ActiveProfileFrame->numberOfEntries++;
   ActiveProfileFrame->startTime = startTime;

   if (! ActiveProfileFrame->childCall)
     {
      theFrame->parentCall = TRUE;
      theFrame->parentStartTime = startTime;
      ActiveProfileFrame->childCall = TRUE;
     }
  }

/*******************************************/
/* EndProfile: Finishes bookkeeping needed */
/*   to profile a construct or function.   */
/*******************************************/
globle void EndProfile(
  struct profileFrameInfo *theFrame)
  {
   double endTime, addTime;

   if (! theFrame->profileOnExit) return;

   endTime = gentime();

   if (theFrame->parentCall)
     {
      addTime = endTime - theFrame->parentStartTime;
      ActiveProfileFrame->totalWithChildrenTime += addTime;
      ActiveProfileFrame->childCall = FALSE;
     }

   ActiveProfileFrame->totalSelfTime += (endTime - ActiveProfileFrame->startTime);

   if (theFrame->oldProfileFrame != NULL)
     { theFrame->oldProfileFrame->startTime = endTime; }

   ActiveProfileFrame = theFrame->oldProfileFrame;
  }

/******************************************/
/* OutputProfileInfo: Prints out a single */
/*   line of profile information.         */
/******************************************/
static BOOLEAN OutputProfileInfo(
  char *itemName,
  struct constructProfileInfo *profileInfo,
  char *printPrefix,
  char **banner)
  {
   double percent = 0.0, percentWithKids = 0.0;
   char buffer[512];

   if (profileInfo == NULL) return(FALSE);
   
   if (profileInfo->numberOfEntries == 0) return(FALSE);

   if (ProfileTotalTime != 0.0)
     {
      percent = (profileInfo->totalSelfTime * 100.0) / ProfileTotalTime;
      if (percent < 0.005) percent = 0.0;
      percentWithKids = (profileInfo->totalWithChildrenTime * 100.0) / ProfileTotalTime;
      if (percentWithKids < 0.005) percentWithKids = 0.0;
     }

   if (percent < PercentThreshold) return(FALSE);

   if ((banner != NULL) && (*banner != NULL))
     {
      PrintRouter(WDISPLAY,*banner);
      *banner = NULL;
     }
   
   if (printPrefix != NULL)
     { PrintRouter(WDISPLAY,printPrefix); }

   if (strlen(itemName) >= 40)
     {
      PrintRouter(WDISPLAY,itemName);
      PrintRouter(WDISPLAY,"\n");
      itemName = "";
     }

   sprintf(buffer,"%-40s %7ld %15.6f  %8.2f%%  %15.6f  %8.2f%%\n",
                        itemName,
                        (long) profileInfo->numberOfEntries,

                        (double) profileInfo->totalSelfTime,
                        (double) percent,

                        (double) profileInfo->totalWithChildrenTime,
                        (double) percentWithKids);
   PrintRouter(WDISPLAY,buffer);

   return(TRUE);
  }

/*******************************************/
/* ProfileResetCommand: H/L access routine */
/*   for the profile-reset command.        */
/*******************************************/
globle void ProfileResetCommand()
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

   ProfileStartTime = 0.0;
   ProfileEndTime = 0.0;
   ProfileTotalTime = 0.0;
   LastProfileInfo = NO_PROFILE;

   for (theFunction = GetFunctionList();
        theFunction != NULL;
        theFunction = theFunction->next)
     { 
      ResetProfileInfo((struct constructProfileInfo *)
                       TestUserData(ProfileDataID,theFunction->usrData));
     }

   for (i = 0; i < MAXIMUM_PRIMITIVES; i++)
     {
      if (PrimitivesArray[i] != NULL)
        {  
         ResetProfileInfo((struct constructProfileInfo *)
                          TestUserData(ProfileDataID,PrimitivesArray[i]->usrData));
        }
     }

#if DEFFUNCTION_CONSTRUCT
   for (theDeffunction = (DEFFUNCTION *) GetNextDeffunction(NULL);
        theDeffunction != NULL;
        theDeffunction = (DEFFUNCTION *) GetNextDeffunction(theDeffunction))
     { 
      ResetProfileInfo((struct constructProfileInfo *)
                       TestUserData(ProfileDataID,theDeffunction->header.usrData)); 
     }
#endif

#if DEFRULE_CONSTRUCT
   for (theDefrule = (struct defrule *) GetNextDefrule(NULL);
        theDefrule != NULL;
        theDefrule = (struct defrule *) GetNextDefrule(theDefrule))
     { 
      ResetProfileInfo((struct constructProfileInfo *)
                       TestUserData(ProfileDataID,theDefrule->header.usrData)); 
     }
#endif

#if DEFGENERIC_CONSTRUCT
   for (theDefgeneric = (DEFGENERIC *) GetNextDefgeneric(NULL);
        theDefgeneric != NULL;
        theDefgeneric = (DEFGENERIC *) GetNextDefgeneric(theDefgeneric))
     {
      ResetProfileInfo((struct constructProfileInfo *)
                       TestUserData(ProfileDataID,theDefgeneric->header.usrData)); 
      
      for (methodIndex = GetNextDefmethod(theDefgeneric,0);
           methodIndex != 0;
           methodIndex = GetNextDefmethod(theDefgeneric,methodIndex))
        {
         theMethod = GetDefmethodPointer(theDefgeneric,methodIndex);
         ResetProfileInfo((struct constructProfileInfo *)
                          TestUserData(ProfileDataID,theMethod->usrData)); 
        }
     }
#endif

#if OBJECT_SYSTEM
   for (theDefclass = (DEFCLASS *) GetNextDefclass(NULL);
        theDefclass != NULL;
        theDefclass = (DEFCLASS *) GetNextDefclass(theDefclass))
     {
      ResetProfileInfo((struct constructProfileInfo *)
                       TestUserData(ProfileDataID,theDefclass->header.usrData)); 
      for (handlerIndex = GetNextDefmessageHandler(theDefclass,0);
           handlerIndex != 0;
           handlerIndex = GetNextDefmessageHandler(theDefclass,handlerIndex))
        {
         theHandler = GetDefmessageHandlerPointer(theDefclass,handlerIndex);
         ResetProfileInfo((struct constructProfileInfo *)
                          TestUserData(ProfileDataID,theHandler->usrData)); 
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
static void OutputUserFunctionsInfo()
  {
   struct FunctionDefinition *theFunction;
   int i;

   for (theFunction = GetFunctionList();
        theFunction != NULL;
        theFunction = theFunction->next)
     {
      OutputProfileInfo(ValueToString(theFunction->callFunctionName),
                        (struct constructProfileInfo *) 
                           TestUserData(ProfileDataID,theFunction->usrData),
                        NULL,NULL);
     }

   for (i = 0; i < MAXIMUM_PRIMITIVES; i++)
     {
      if (PrimitivesArray[i] != NULL)
        {
         OutputProfileInfo(PrimitivesArray[i]->name,
                           (struct constructProfileInfo *)
                              TestUserData(ProfileDataID,PrimitivesArray[i]->usrData),
                           NULL,NULL);
        }
     }
  }

/*************************************************/
/* OutputConstructsCodeInfo:       */
/*************************************************/
static void OutputConstructsCodeInfo()
  {
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
   char buffer[512];
   char *prefix;
   char *banner;

   banner = "\n*** Deffunctions ***\n\n";

#if DEFFUNCTION_CONSTRUCT
   for (theDeffunction = (DEFFUNCTION *) GetNextDeffunction(NULL);
        theDeffunction != NULL;
        theDeffunction = (DEFFUNCTION *) GetNextDeffunction(theDeffunction))
     {
      OutputProfileInfo(GetDeffunctionName(theDeffunction),
                        (struct constructProfileInfo *) 
                          TestUserData(ProfileDataID,theDeffunction->header.usrData),
                        NULL,&banner);
     }
#endif

   banner = "\n*** Defgenerics ***\n";
#if DEFGENERIC_CONSTRUCT
   for (theDefgeneric = (DEFGENERIC *) GetNextDefgeneric(NULL);
        theDefgeneric != NULL;
        theDefgeneric = (DEFGENERIC *) GetNextDefgeneric(theDefgeneric))
     {
      sprintf(buffer,"\n%s\n",GetDefgenericName(theDefgeneric));
      prefix = buffer;

      for (methodIndex = GetNextDefmethod(theDefgeneric,0);
           methodIndex != 0;
           methodIndex = GetNextDefmethod(theDefgeneric,methodIndex))
        {
         theMethod = GetDefmethodPointer(theDefgeneric,methodIndex);

         GetDefmethodDescription(methodBuffer,510,theDefgeneric,methodIndex);
         if (OutputProfileInfo(methodBuffer,
                               (struct constructProfileInfo *) 
                                  TestUserData(ProfileDataID,theMethod->usrData),
                               prefix,&banner))
           { prefix = NULL; }
        }
     }
#endif

   banner = "\n*** Defclasses ***\n";
#if OBJECT_SYSTEM
   for (theDefclass = (DEFCLASS *) GetNextDefclass(NULL);
        theDefclass != NULL;
        theDefclass = (DEFCLASS *) GetNextDefclass(theDefclass))
     {
      sprintf(buffer,"\n%s\n",GetDefclassName(theDefclass));
      prefix = buffer;
      for (handlerIndex = GetNextDefmessageHandler(theDefclass,0);
           handlerIndex != 0;
           handlerIndex = GetNextDefmessageHandler(theDefclass,handlerIndex))
        {
         theHandler = GetDefmessageHandlerPointer(theDefclass,handlerIndex);
         if (OutputProfileInfo(GetDefmessageHandlerName(theDefclass,handlerIndex),
                               (struct constructProfileInfo *) 
                                  TestUserData(ProfileDataID,theHandler->usrData),
                           prefix,&banner))
           { prefix = NULL; }
        }

     }
#endif


   banner = "\n*** Defrules ***\n\n";

#if DEFRULE_CONSTRUCT
   for (theDefrule = (struct defrule *) GetNextDefrule(NULL);
        theDefrule != NULL;
        theDefrule = (struct defrule *) GetNextDefrule(theDefrule))
     {
      OutputProfileInfo(GetDefruleName(theDefrule),
                        (struct constructProfileInfo *) 
                          TestUserData(ProfileDataID,theDefrule->header.usrData),
                        NULL,&banner);
     }
#endif

  }

/*********************************************************/
/* SetProfilePercentThresholdCommand: H/L access routine */
/*   for the set-profile-percent-threshold command.      */
/*********************************************************/
globle double SetProfilePercentThresholdCommand()
  {
   DATA_OBJECT theValue;
   double newThreshold;

   if (ArgCountCheck("set-profile-percent-threshold",EXACTLY,1) == -1)
     { return(PercentThreshold); }

   if (ArgTypeCheck("set-profile-percent-threshold",1,INTEGER_OR_FLOAT,&theValue) == FALSE)
      { return(PercentThreshold); }

   if (GetType(theValue) == INTEGER)
     { newThreshold = (double) DOToLong(theValue); }
   else
     { newThreshold = (double) DOToDouble(theValue); }
     
   if ((newThreshold < 0.0) || (newThreshold > 100.0))
     { 
      ExpectedTypeError1("set-profile-percent-threshold",1,
                         "number in the range 0 to 100");
      return(-1.0); 
     }

   return(SetProfilePercentThreshold(newThreshold));
  }

/****************************************************/
/* SetProfilePercentThreshold: C access routine for */
/*   the set-profile-percent-threshold command.     */
/****************************************************/
globle double SetProfilePercentThreshold(
  double value)
  {
   double oldPercentThreshhold;

   if ((value < 0.0) || (value > 100.0))
     { return(-1.0); }
     
   oldPercentThreshhold = PercentThreshold;

   PercentThreshold = value;

   return(oldPercentThreshhold);
  }

/*********************************************************/
/* GetProfilePercentThresholdCommand: H/L access routine */
/*   for the get-profile-percent-threshold command.      */
/*********************************************************/
globle double GetProfilePercentThresholdCommand()
  {
   ArgCountCheck("get-profile-percent-threshold",EXACTLY,0);

   return(PercentThreshold);
  }

/****************************************************/
/* GetProfilePercentThreshold: C access routine for */
/*   the get-profile-percent-threshold command.     */
/****************************************************/
globle double GetProfilePercentThreshold()
  {
   return(PercentThreshold);
  }
  
/******************************************************************/
/* ProfileClearFunction: Profiling clear routine for use with the */
/*   clear command. Removes user data attached to user functions. */
/******************************************************************/
static void ProfileClearFunction()
  {
   struct FunctionDefinition *theFunction;
   int i;

   for (theFunction = GetFunctionList();
        theFunction != NULL;
        theFunction = theFunction->next)
     {
      theFunction->usrData = 
        DeleteUserData(ProfileDataID,theFunction->usrData);
     }

   for (i = 0; i < MAXIMUM_PRIMITIVES; i++)
     {
      if (PrimitivesArray[i] != NULL)
        {
         PrimitivesArray[i]->usrData = 
           DeleteUserData(ProfileDataID,PrimitivesArray[i]->usrData);
        }
     }
  }
  
#endif /* PROFILING_FUNCTIONS */


   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*       Microsoft Windows Version 3.0  01/31/02       */
   /*                                                     */
   /*                   REGISTRY MODULE                   */
   /*******************************************************/

/**************************************************************/
/* Purpose: Provides basic routines for storing information   */
/*   in the registry.                                         */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Gary D. Riley                                         */
/*                                                            */
/* Contributing Programmer(s):                                */
/*      Ernst Bokkelkamp                                      */
/*                                                            */
/* Revision History:                                          */
/*      6.24: Ernst's changes to remember window positions.   */
/*                                                            */
/**************************************************************/

#define _REGISTRY_SOURCE_

#include <windows.h>
#include <windowsx.h>

#include "setup.h"

#include "agenda.h"
#include "bmathfun.h"
#include "crstrtgy.h"
#include "exprnpsr.h"
#include "facthsh.h"
#include "globlcom.h"
#include "incrrset.h"
#include "watch.h"


#include "dialog1.h"

#include "Registry.h"

struct WindowInformation
  {
   int height;
   int width;
   int x;
   int y;
  };
	
struct WatchInformation
  {
   boolean compilations;
   boolean facts;
   boolean instances;
   boolean rules;
   boolean genericFunctions;
   boolean messages;
   boolean deffunctions;
   boolean statistics;
   boolean globals;
   boolean slots;
   boolean activations;
   boolean methods;
   boolean focus;
   boolean messageHandlers;
  };
  
struct ExecutionInformation
  {
   int salienceEvaluation;
   int strategy;
   boolean staticConstraintChecking;
   boolean dynamicConstraintChecking;
   boolean autoFloatDividend;
   boolean resetGlobals;
   boolean factDuplication;
   boolean incrementalReset;
   boolean sequenceOperatorRecognition;
  };
  
struct PreferenceInformation
  {
   int ruleStep;
   int warnings;
   int complete;
  };

#define CURRENT_VERSION 1

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    RestoreWatchInformation(void);
   static void                    RestoreExecutionInformation(void);
   static void                    RestorePreferenceInformation(void);
   static DWORD                   GetVersionInformation(void);
   static void                    SaveVersionInformation(void);

/***************************************************/
/* ReadRegistryInformation:              */
/***************************************************/
void ReadRegistryInformation()
  {
   DWORD theVersion;
   
   theVersion = GetVersionInformation();
   
   if (theVersion == CURRENT_VERSION)
     {
      RestoreWatchInformation();
      RestoreExecutionInformation();
      RestorePreferenceInformation();
     }
   else if (theVersion < CURRENT_VERSION)
     {
      SaveWatchInformation();
      SaveExecutionInformation();
      SavePreferenceInformation();
     }
   else 
     {
      SaveWatchInformation();
      SaveExecutionInformation();
      SavePreferenceInformation();
     }
   
   SaveVersionInformation();
  }

/*************************************/
/* SaveWatchInformation: Saves watch */
/*   information to the registry.    */
/*************************************/
void SaveWatchInformation()
  {
   HKEY hKey;
   DWORD lpdwDisposition;
   struct WatchInformation watchInfo;
   void *theEnv = GetCurrentEnvironment();
    
   if (RegCreateKeyEx(HKEY_CURRENT_USER,TEXT("Software\\CLIPS\\CLIPSWin"),0,"",0,
                      KEY_READ | KEY_WRITE,NULL,&hKey,&lpdwDisposition) != ERROR_SUCCESS)
     { return; }
              
   watchInfo.compilations = (boolean) EnvGetWatchItem(theEnv,"compilations");
   watchInfo.facts = (boolean) EnvGetWatchItem(theEnv,"facts");
   watchInfo.instances = (boolean) EnvGetWatchItem(theEnv,"instances");
   watchInfo.rules = (boolean) EnvGetWatchItem(theEnv,"rules");
   watchInfo.genericFunctions = (boolean) EnvGetWatchItem(theEnv,"generic-functions");
   watchInfo.messages = (boolean) EnvGetWatchItem(theEnv,"messages");
   watchInfo.deffunctions = (boolean) EnvGetWatchItem(theEnv,"deffunctions");
   watchInfo.statistics = (boolean) EnvGetWatchItem(theEnv,"statistics");
   watchInfo.globals = (boolean) EnvGetWatchItem(theEnv,"globals");
   watchInfo.slots = (boolean) EnvGetWatchItem(theEnv,"slots");
   watchInfo.activations = (boolean) EnvGetWatchItem(theEnv,"activations");
   watchInfo.methods = (boolean) EnvGetWatchItem(theEnv,"methods");
   watchInfo.focus = (boolean) EnvGetWatchItem(theEnv,"focus");
   watchInfo.messageHandlers = (boolean) EnvGetWatchItem(theEnv,"message-handlers");

   if (RegSetValueEx(hKey,"Watch",0,REG_BINARY,(BYTE *) &watchInfo,
                     sizeof(struct WatchInformation)) != ERROR_SUCCESS)
     {
      RegCloseKey(hKey);
      return;
     }

   RegCloseKey(hKey);
  }

/*********************************************/
/* SaveExecutionInformation: Saves execution */
/*   information to the registry.            */
/*********************************************/
void SaveExecutionInformation()
  {
   HKEY hKey;
   DWORD lpdwDisposition;
   struct ExecutionInformation executionInfo;
   void *theEnv = GetCurrentEnvironment();
   
   if (RegCreateKeyEx(HKEY_CURRENT_USER,TEXT("Software\\CLIPS\\CLIPSWin"),0,"",0,
                      KEY_READ | KEY_WRITE,NULL,&hKey,&lpdwDisposition) != ERROR_SUCCESS)
     { return; }
              
   executionInfo.salienceEvaluation = EnvGetSalienceEvaluation(theEnv);
   executionInfo.strategy = EnvGetStrategy(theEnv);
   executionInfo.staticConstraintChecking = (boolean) EnvGetStaticConstraintChecking(theEnv);
   executionInfo.dynamicConstraintChecking = (boolean) EnvGetDynamicConstraintChecking(theEnv);
   executionInfo.autoFloatDividend = (boolean) EnvGetAutoFloatDividend(theEnv);
   executionInfo.resetGlobals = (boolean) EnvGetResetGlobals(theEnv);
   executionInfo.factDuplication = (boolean) EnvGetFactDuplication(theEnv);
   executionInfo.incrementalReset = (boolean) EnvGetIncrementalReset(theEnv);
   executionInfo.sequenceOperatorRecognition = (boolean) EnvGetSequenceOperatorRecognition(theEnv);

   if (RegSetValueEx(hKey,"Execution",0,REG_BINARY,(BYTE *) &executionInfo,
                     sizeof(struct ExecutionInformation)) != ERROR_SUCCESS)
     {
      RegCloseKey(hKey);
      return;
     }

   RegCloseKey(hKey);
  }

/***********************************************/
/* SavePreferenceInformation: Saves preference */
/*   information to the registry.              */
/***********************************************/
void SavePreferenceInformation()
  {
   HKEY hKey;
   DWORD lpdwDisposition;
   struct PreferenceInformation preferenceInfo;
    
   if (RegCreateKeyEx(HKEY_CURRENT_USER,TEXT("Software\\CLIPS\\CLIPSWin"),0,"",0,
                      KEY_READ | KEY_WRITE,NULL,&hKey,&lpdwDisposition) != ERROR_SUCCESS)
     { return; }
              
   preferenceInfo.ruleStep = RuleStep;
   preferenceInfo.warnings = Warnings;
   preferenceInfo.complete = Complete;
   
   if (RegSetValueEx(hKey,"Preference",0,REG_BINARY,(BYTE *) &preferenceInfo,
                     sizeof(struct PreferenceInformation)) != ERROR_SUCCESS)
     {
      RegCloseKey(hKey);
      return;
     }

   RegCloseKey(hKey);
  }
  
/*****************************************/
/* SaveVersionInformation: Saves version */
/*   information to the registry.        */
/*****************************************/
static void SaveVersionInformation()
  {
   HKEY hKey;
   DWORD lpdwDisposition;
   DWORD version = CURRENT_VERSION;
    
   if (RegCreateKeyEx(HKEY_CURRENT_USER,TEXT("Software\\CLIPS\\CLIPSWin"),0,"",0,
                      KEY_READ | KEY_WRITE,NULL,&hKey,&lpdwDisposition) != ERROR_SUCCESS)
     { return; }
   
   if (RegSetValueEx(hKey,"Version",0,REG_DWORD,(BYTE *) &version,
                     sizeof(DWORD)) != ERROR_SUCCESS)
     {
      RegCloseKey(hKey);
      return;
     }

   RegCloseKey(hKey);
  }
  
/*******************************************/
/* RestoreWatchInformation: Restores watch */
/*   information from the registry.        */
/*******************************************/
static void RestoreWatchInformation()
  {
   HKEY hKey;
   DWORD lpdwDisposition;
   struct WatchInformation watchInfo;
   DWORD type = REG_BINARY;
   DWORD size = sizeof(struct WatchInformation);
    
   if (RegCreateKeyEx(HKEY_CURRENT_USER,TEXT("Software\\CLIPS\\CLIPSWin"),0,"",0,
                      KEY_READ | KEY_WRITE,NULL,&hKey,&lpdwDisposition) != ERROR_SUCCESS)
     { return; }

   if (RegQueryValueEx(hKey,"Watch",0,&type,(BYTE *) &watchInfo,
                       &size) != ERROR_SUCCESS)
     {
      RegCloseKey(hKey);
      return;
     }

   EnvSetWatchItem(GetCurrentEnvironment(),"compilations",watchInfo.compilations,NULL);
   EnvSetWatchItem(GetCurrentEnvironment(),"facts",watchInfo.facts,NULL);
   EnvSetWatchItem(GetCurrentEnvironment(),"instances",watchInfo.instances,NULL);
   EnvSetWatchItem(GetCurrentEnvironment(),"rules",watchInfo.rules,NULL);
   EnvSetWatchItem(GetCurrentEnvironment(),"generic-functions",watchInfo.genericFunctions,NULL);
   EnvSetWatchItem(GetCurrentEnvironment(),"messages",watchInfo.messages,NULL);
   EnvSetWatchItem(GetCurrentEnvironment(),"deffunctions",watchInfo.deffunctions,NULL);
   EnvSetWatchItem(GetCurrentEnvironment(),"statistics",watchInfo.statistics,NULL);
   EnvSetWatchItem(GetCurrentEnvironment(),"globals",watchInfo.globals,NULL);
   EnvSetWatchItem(GetCurrentEnvironment(),"slots",watchInfo.slots,NULL);
   EnvSetWatchItem(GetCurrentEnvironment(),"activations",watchInfo.activations,NULL);
   EnvSetWatchItem(GetCurrentEnvironment(),"methods",watchInfo.methods,NULL);
   EnvSetWatchItem(GetCurrentEnvironment(),"focus",watchInfo.focus,NULL);
   EnvSetWatchItem(GetCurrentEnvironment(),"message-handlers",watchInfo.messageHandlers,NULL);

   RegCloseKey(hKey);
  }
  
/***************************************************/
/* RestoreExecutionInformation: Restores execution */
/*   information from the registry.                */
/***************************************************/
static void RestoreExecutionInformation()
  {
   HKEY hKey;
   DWORD lpdwDisposition;
   struct ExecutionInformation executionInfo;
   DWORD type = REG_BINARY;
   DWORD size = sizeof(struct ExecutionInformation);
   void *theEnv = GetCurrentEnvironment();
   
   if (RegCreateKeyEx(HKEY_CURRENT_USER,TEXT("Software\\CLIPS\\CLIPSWin"),0,"",0,
                      KEY_READ | KEY_WRITE,NULL,&hKey,&lpdwDisposition) != ERROR_SUCCESS)
     { return; }

   if (RegQueryValueEx(hKey,"Execution",0,&type,(BYTE *) &executionInfo,
                       &size) != ERROR_SUCCESS)
     {
      RegCloseKey(hKey);
      return;
     }

   EnvSetSalienceEvaluation(theEnv,executionInfo.salienceEvaluation);
   EnvSetStrategy(theEnv,executionInfo.strategy);
   EnvSetStaticConstraintChecking(theEnv,executionInfo.staticConstraintChecking);
   EnvSetDynamicConstraintChecking(theEnv,executionInfo.dynamicConstraintChecking);
   EnvSetAutoFloatDividend(theEnv,executionInfo.autoFloatDividend);
   EnvSetResetGlobals(theEnv,executionInfo.resetGlobals);
   EnvSetFactDuplication(theEnv,executionInfo.factDuplication);
   EnvSetIncrementalReset(theEnv,executionInfo.incrementalReset);
   EnvSetSequenceOperatorRecognition(theEnv,executionInfo.sequenceOperatorRecognition);

   RegCloseKey(hKey);
  }
  
/******************************************************/
/* RestorePreferencesInformation: Restores preference */
/*   information from the registry.                   */
/******************************************************/
static void RestorePreferenceInformation()
  {
   HKEY hKey;
   DWORD lpdwDisposition;
   struct PreferenceInformation preferenceInfo;
   DWORD type = REG_BINARY;
   DWORD size = sizeof(struct PreferenceInformation);
    
   if (RegCreateKeyEx(HKEY_CURRENT_USER,TEXT("Software\\CLIPS\\CLIPSWin"),0,"",0,
                      KEY_READ | KEY_WRITE,NULL,&hKey,&lpdwDisposition) != ERROR_SUCCESS)
     { return; }

   if (RegQueryValueEx(hKey,"Preference",0,&type,(BYTE *) &preferenceInfo,
                       &size) != ERROR_SUCCESS)
     {
      RegCloseKey(hKey);
      return;
     }

   RuleStep = preferenceInfo.ruleStep;
   Warnings = preferenceInfo.warnings;
   Complete = preferenceInfo.complete;

   RegCloseKey(hKey);
  }
  
/*******************************************/
/* GetVersionInformation: Gets the version */
/*   number of the registry information.   */
/*******************************************/
static DWORD GetVersionInformation()
  {
   HKEY hKey;
   DWORD lpdwDisposition;
   DWORD version;
   DWORD type = REG_DWORD;
   DWORD size = sizeof(DWORD);
    
   if (RegCreateKeyEx(HKEY_CURRENT_USER,TEXT("Software\\CLIPS\\CLIPSWin"),0,"",0,
                      KEY_READ | KEY_WRITE,NULL,&hKey,&lpdwDisposition) != ERROR_SUCCESS)
     { return(1); }

   if (RegQueryValueEx(hKey,"Version",0,&type,(BYTE *) &version,
                       &size) != ERROR_SUCCESS)
     {
      RegCloseKey(hKey);
      return(1);
     }

   RegCloseKey(hKey);
   
   return(version);
  }
       

/***********************************************/
/* SaveWindowInformation: Saves Window size    */
/*   information to the registry.              */
/***********************************************/
void SaveWindowInformation(
  int h, 
  int w,
  int x,
  int y)
  {
   HKEY hKey;
   DWORD lpdwDisposition;
   struct WindowInformation windowInfo;
    
   if (RegCreateKeyEx(HKEY_CURRENT_USER,TEXT("Software\\CLIPS\\CLIPSWin"),0,"",0,
                      KEY_READ | KEY_WRITE,NULL,&hKey,&lpdwDisposition) != ERROR_SUCCESS)
     { return; }
              
   windowInfo.height = h;
   windowInfo.width  = w;
   windowInfo.x = x;
   windowInfo.y = y;
   
   if (RegSetValueEx(hKey,"Window",0,REG_BINARY,(BYTE *) &windowInfo,
                     sizeof(struct WindowInformation)) != ERROR_SUCCESS)
     {
      RegCloseKey(hKey);
      return;
     }

   RegCloseKey(hKey);
  }
       

/******************************************************/
/* LoadWindowInformation: Load window size            */
/*   information from the registry.                   */
/******************************************************/
void LoadWindowInformation(
  int *h,
  int *w,
  int *x,
  int *y)
  {
   HKEY hKey;
   DWORD lpdwDisposition;
   struct WindowInformation windowInfo;
   DWORD type = REG_BINARY;
   DWORD size = sizeof(struct WindowInformation);
   RECT theRect;

   *w = CW_USEDEFAULT;
   *h = 0;

   *x = CW_USEDEFAULT;
   *y = 0;

   if (RegCreateKeyEx(HKEY_CURRENT_USER,TEXT("Software\\CLIPS\\CLIPSWin"),0,"",0,
					  KEY_READ | KEY_WRITE,NULL,&hKey,&lpdwDisposition) != ERROR_SUCCESS)
	 { return; }

   if (RegQueryValueEx(hKey,"Window",0,&type,
					   (BYTE *) &windowInfo,&size) != ERROR_SUCCESS)
	 {
      RegCloseKey(hKey);
      return;
     }

   *h = windowInfo.height;
   *w = windowInfo.width;

   /*====================================*/
   /* Determine the size of the desktop. */
   /*====================================*/
   
   GetClientRect(GetDesktopWindow(),&theRect);
   
   /*==========================================*/
   /* If the window position falls outside the */
   /* desktop, use default window placement.   */
   /*==========================================*/
   
   if ((windowInfo.x < 0) ||
       (windowInfo.y < 0) ||
       (windowInfo.x >= theRect.right) ||
       (windowInfo.y >= theRect.bottom))
     { 
      *x = CW_USEDEFAULT; 
      *y = 0;
     }
   else
     { 
      *x = windowInfo.x;
      *y = windowInfo.y; 
     }
   
   RegCloseKey(hKey);
  }

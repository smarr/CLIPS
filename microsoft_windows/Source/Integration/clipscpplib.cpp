#include <stdio.h>

#include "clipscpp.h"

using namespace CLIPS;

#ifndef CLIPS_WIN32_DLL
#if CLIPS_COMPILED_AS_C_PLUS_PLUS
#include "clips.h"
#else
extern "C"
  {
   #include "clips.h"
  }
#endif
#else
#include <windows.h>
extern "C"
{
#include "CLIPSWin32.h"
}
#endif

/*##################*/
/* Static Functions */
/*##################*/

#ifndef CLIPS_WIN32_DLL
static int CLIPSCPPQuery(void *,char *);
static int CLIPSCPPPrint(void *,char *,char *);
static int CLIPSCPPGetc(void *,char *);
static int CLIPSCPPUngetc(void *,int,char *);
static int CLIPSCPPExit(void *,int);
#endif

/*#####################*/
/* CLIPSCPPEnv Methods */
/*#####################*/

/***************/
/* CLIPSCPPEnv */
/***************/
CLIPSCPPEnv::CLIPSCPPEnv()
  {
#ifndef CLIPS_WIN32_DLL
   theEnv = CreateEnvironment();

   SetEnvironmentContext(theEnv,this);
#else
   theEnv = __CreateEnvironment();
   /* TBD */
#endif
  }

/****************/
/* ~CLIPSCPPEnv */
/****************/
CLIPSCPPEnv::~CLIPSCPPEnv()
  {
#ifndef CLIPS_WIN32_DLL
   DestroyEnvironment(theEnv);
#else
   __DestroyEnvironment(theEnv);
#endif
  }

/*********/
/* Clear */
/*********/
void CLIPSCPPEnv::Clear()
  {
#ifndef CLIPS_WIN32_DLL
   EnvClear(theEnv);
#else
   __EnvClear(theEnv);
#endif
  }

/********/
/* Load */
/********/
int CLIPSCPPEnv::Load(
  char *theFile)
  {
#ifndef CLIPS_WIN32_DLL
   return EnvLoad(theEnv,theFile);
#else
   return __EnvLoad(theEnv,theFile);
#endif
  }

/*********/
/* Reset */
/*********/
void CLIPSCPPEnv::Reset()
  {
#ifndef CLIPS_WIN32_DLL
   EnvReset(theEnv);
#else
   __EnvReset(theEnv);
#endif
  }

/*******/
/* Run */
/*******/
long long CLIPSCPPEnv::Run(
  long long runLimit)
  {
#ifndef CLIPS_WIN32_DLL
   return EnvRun(theEnv,runLimit);
#else
   return __EnvRun(theEnv,runLimit);
#endif
  }

#ifndef CLIPS_WIN32_DLL

/*********/
/* Watch */
/*********/
int CLIPSCPPEnv::Watch(
  char *item)
  {
   return EnvWatch(theEnv,item);
  }

/***********/
/* Unwatch */
/***********/
int CLIPSCPPEnv::Unwatch(
  char *item)
  {
   return EnvUnwatch(theEnv,item);
  }

/*************/
/* AddRouter */
/*************/
int CLIPSCPPEnv::AddRouter(
  char *routerName,
  int priority,
  CLIPSCPPRouter *router)
  {
   return EnvAddRouterWithContext(theEnv,routerName,priority,CLIPSCPPQuery,
                                  CLIPSCPPPrint,CLIPSCPPGetc,CLIPSCPPUngetc,
                                  CLIPSCPPExit,router);
  }
  
/*########################*/
/* CLIPSCPPRouter Methods */
/*########################*/

/*********/
/* Query */
/*********/
int CLIPSCPPRouter::Query(
  CLIPSCPPEnv *theCPPEnv,
  char *logicalName)
  { 
   return FALSE;
  }
  
/*********/
/* Print */
/*********/
int CLIPSCPPRouter::Print(
  CLIPSCPPEnv *theCPPEnv,
  char *logicalName,
  char *printString)
  {
   return FALSE;
  }
  
/********/
/* Getc */
/********/
int CLIPSCPPRouter::Getc(
  CLIPSCPPEnv *theCPPEnv,
  char *logicalName)
  {
   return -1;
  }
  
/**********/
/* Ungetc */
/**********/
int CLIPSCPPRouter::Ungetc(
  CLIPSCPPEnv *theCPPEnv,
  int character,
  char *logicalName)
  {
   return -1;
  }

/********/
/* Exit */
/********/
int CLIPSCPPRouter::Exit(
  CLIPSCPPEnv *theCPPEnv,
  int exitCode)
  {
   return FALSE;
  }
  
/*#########################*/
/* Static Router Functions */
/*#########################*/
  
/*****************/
/* CLIPSCPPQuery */
/*****************/
static int CLIPSCPPQuery(
  void *theEnv,
  char *logicalName)
  { 
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) GetEnvironmentRouterContext(theEnv);
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) GetEnvironmentContext(theEnv);
   
   return(theRouter->Query(theCPPEnv,logicalName));
  }

/*****************/
/* CLIPSCPPPrint */
/*****************/
static int CLIPSCPPPrint(
  void *theEnv,
  char *logicalName,
  char *printString)
  { 
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) GetEnvironmentRouterContext(theEnv);
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) GetEnvironmentContext(theEnv);
   
   return(theRouter->Print(theCPPEnv,logicalName,printString));
  }

/*****************/
/* CLIPSCPPGetc */
/*****************/
static int CLIPSCPPGetc(
  void *theEnv,
  char *logicalName)
  { 
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) GetEnvironmentRouterContext(theEnv);
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) GetEnvironmentContext(theEnv);
   
   return(theRouter->Getc(theCPPEnv,logicalName));
  }
  
/*****************/
/* CLIPSCPPUngetc */
/*****************/
static int CLIPSCPPUngetc(
  void *theEnv,
  int character,
  char *logicalName)
  { 
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) GetEnvironmentRouterContext(theEnv);
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) GetEnvironmentContext(theEnv);
   
   return(theRouter->Ungetc(theCPPEnv,character,logicalName));
  }
  
/*****************/
/* CLIPSCPPExit */
/*****************/
static int CLIPSCPPExit(
  void *theEnv,
  int exitCode)
  { 
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) GetEnvironmentRouterContext(theEnv);
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) GetEnvironmentContext(theEnv);
   
   return(theRouter->Exit(theCPPEnv,exitCode));
  }

#endif
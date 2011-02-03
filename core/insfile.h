   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.24  06/05/06          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#ifndef _H_insfile
#define _H_insfile

#ifndef _H_expressn
#include "expressn.h"
#endif

#define INSTANCE_FILE_DATA 30

#if BLOAD_INSTANCES || BSAVE_INSTANCES
struct instanceFileData
  { 
#if BLOAD_INSTANCES || BSAVE_INSTANCES
   char *InstanceBinaryPrefixID;
   char *InstanceBinaryVersionID;
   unsigned long BinaryInstanceFileSize;

#if BLOAD_INSTANCES
   unsigned long BinaryInstanceFileOffset;
   char *CurrentReadBuffer;
   unsigned long CurrentReadBufferSize;
   unsigned long CurrentReadBufferOffset;
#endif

#endif
  };

#define InstanceFileData(theEnv,execStatus) ((struct instanceFileData *) GetEnvironmentData(theEnv,execStatus,INSTANCE_FILE_DATA))

#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _INSFILE_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define BinaryLoadInstances(a) EnvBinaryLoadInstances(GetCurrentEnvironment(),GetCurrentExecutionState(),a)
#define BinarySaveInstances(a,b,c,d) EnvBinarySaveInstances(GetCurrentEnvironment(),GetCurrentExecutionState(),a,b,c,d)
#define LoadInstances(a) EnvLoadInstances(GetCurrentEnvironment(),GetCurrentExecutionState(),a)
#define LoadInstancesFromString(a,b) EnvLoadInstancesFromString(GetCurrentEnvironment(),GetCurrentExecutionState(),a,b)
#define RestoreInstances(a) EnvRestoreInstances(GetCurrentEnvironment(),GetCurrentExecutionState(),a)
#define RestoreInstancesFromString(a,b) EnvRestoreInstancesFromString(GetCurrentEnvironment(),GetCurrentExecutionState(),a,b)
#define SaveInstances(a,b,c,d) EnvSaveInstances(GetCurrentEnvironment(),GetCurrentExecutionState(),a,b,c,d)

LOCALE void SetupInstanceFileCommands(void *,EXEC_STATUS);

LOCALE long SaveInstancesCommand(void *,EXEC_STATUS);
LOCALE long LoadInstancesCommand(void *,EXEC_STATUS);
LOCALE long RestoreInstancesCommand(void *,EXEC_STATUS);
LOCALE long EnvSaveInstances(void *,EXEC_STATUS,char *,int,EXPRESSION *,intBool);

#if BSAVE_INSTANCES
LOCALE long BinarySaveInstancesCommand(void *,EXEC_STATUS);
LOCALE long EnvBinarySaveInstances(void *,EXEC_STATUS,char *,int,EXPRESSION *,intBool);
#endif

#if BLOAD_INSTANCES
LOCALE long BinaryLoadInstancesCommand(void *,EXEC_STATUS);
LOCALE long EnvBinaryLoadInstances(void *,EXEC_STATUS,char *);
#endif

LOCALE long EnvLoadInstances(void *,EXEC_STATUS,char *);
LOCALE long EnvLoadInstancesFromString(void *,EXEC_STATUS,char *,int);
LOCALE long EnvRestoreInstances(void *,EXEC_STATUS,char *);
LOCALE long EnvRestoreInstancesFromString(void *,EXEC_STATUS,char *,int);

#ifndef _INSFILE_SOURCE_
#endif

#endif




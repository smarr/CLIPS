   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  07/01/05            */
   /*                                                     */
   /*              PRINT UTILITY HEADER FILE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Utility routines for printing various items      */
/*   and messages.                                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Link error occurs for the SlotExistError       */
/*            function when OBJECT_SYSTEM is set to 0 in     */
/*            setup.h. DR0865                                */
/*                                                           */
/*            Added DataObjectToString function.             */
/*                                                           */
/*            Added SlotExistError function.                 */
/*                                                           */
/*************************************************************/

#ifndef _H_prntutil
#define _H_prntutil

#ifndef _H_moduldef
#include "moduldef.h"
#endif

#ifndef _STDIO_INCLUDED_
#define _STDIO_INCLUDED_
#include <stdio.h>
#endif

#define PRINT_UTILITY_DATA 53

struct printUtilityData
  { 
   intBool PreserveEscapedCharacters;
   intBool AddressesToStrings;
   intBool InstanceAddressesToNames;
  };

#define PrintUtilityData(theEnv,execStatus) ((struct printUtilityData *) GetEnvironmentData(theEnv,execStatus,PRINT_UTILITY_DATA))

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _PRNTUTIL_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif
   LOCALE void                           InitializePrintUtilityData(void *,EXEC_STATUS);
   LOCALE void                           PrintInChunks(void *,EXEC_STATUS,char *,char *);
   LOCALE void                           PrintFloat(void *,EXEC_STATUS,char *,double);
   LOCALE void                           PrintLongInteger(void *,EXEC_STATUS,char *,long long);
   LOCALE void                           PrintAtom(void *,EXEC_STATUS,char *,int,void *);
   LOCALE void                           PrintTally(void *,EXEC_STATUS,char *,long long,char *,char *);
   LOCALE char                          *FloatToString(void *,EXEC_STATUS,double);
   LOCALE char                          *LongIntegerToString(void *,EXEC_STATUS,long long);
   LOCALE char                          *DataObjectToString(void *,EXEC_STATUS,DATA_OBJECT *);
   LOCALE void                           SyntaxErrorMessage(void *,EXEC_STATUS,char *);
   LOCALE void                           SystemError(void *,EXEC_STATUS,char *,int);
   LOCALE void                           PrintErrorID(void *,EXEC_STATUS,char *,int,int);
   LOCALE void                           PrintWarningID(void *,EXEC_STATUS,char *,int,int);
   LOCALE void                           CantFindItemErrorMessage(void *,EXEC_STATUS,char *,char *);
   LOCALE void                           CantDeleteItemErrorMessage(void *,EXEC_STATUS,char *,char *);
   LOCALE void                           AlreadyParsedErrorMessage(void *,EXEC_STATUS,char *,char *);
   LOCALE void                           LocalVariableErrorMessage(void *,EXEC_STATUS,char *);
   LOCALE void                           DivideByZeroErrorMessage(void *,EXEC_STATUS,char *);
   LOCALE void                           SalienceInformationError(void *,EXEC_STATUS,char *,char *);
   LOCALE void                           SalienceRangeError(void *,EXEC_STATUS,int,int);
   LOCALE void                           SalienceNonIntegerError(void *,EXEC_STATUS);
   LOCALE void                           CantFindItemInFunctionErrorMessage(void *,EXEC_STATUS,char *,char *,char *);
   LOCALE void                           SlotExistError(void *,EXEC_STATUS,char *,char *);

#endif







   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  10/19/06            */
   /*                                                     */
   /*            EXTERNAL FUNCTIONS HEADER FILE           */
   /*******************************************************/

/*************************************************************/
/* Purpose: Routines for adding new user or system defined   */
/*   functions.                                              */
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
/*      6.30: Added support for passing context information  */ 
/*            to user defined functions.                     */
/*                                                           */
/*************************************************************/

#ifndef _H_extnfunc

#define _H_extnfunc

#ifndef _H_symbol
#include "symbol.h"
#endif
#ifndef _H_expressn
#include "expressn.h"
#endif

#include "userdata.h"

# include "evaluatn.h"

struct FunctionDefinition
  {
   struct symbolHashNode *callFunctionName;
   char *actualFunctionName;
   char returnValueType;
   int (*functionPointer)(void);
   struct expr *(*parser)(void *,EXEC_STATUS,struct expr *,char *);
   char *restrictions;
   short int overloadable;
   short int sequenceuseok;
   short int environmentAware;
   short int bsaveIndex;
   struct FunctionDefinition *next;
   struct userData *usrData;
   void *context;
  };

#define ValueFunctionType(target) (((struct FunctionDefinition *) target)->returnValueType)
#define ExpressionFunctionType(target) (((struct FunctionDefinition *) ((target)->value))->returnValueType)
#define ExpressionFunctionPointer(target) (((struct FunctionDefinition *) ((target)->value))->functionPointer)
#define ExpressionFunctionCallName(target) (((struct FunctionDefinition *) ((target)->value))->callFunctionName)
#define ExpressionFunctionRealName(target) (((struct FunctionDefinition *) ((target)->value))->actualFunctionName)

#define PTIF (int (*)(void))
#define PTIEF (int (*)(void *,EXEC_STATUS))

/*==================*/
/* ENVIRONMENT DATA */
/*==================*/

#define EXTERNAL_FUNCTION_DATA 50

struct externalFunctionData
  { 
   struct FunctionDefinition *ListOfFunctions;
   struct FunctionHash **FunctionHashtable;
  };

#define ExternalFunctionData(theEnv,execStatus) ((struct externalFunctionData *) GetEnvironmentData(theEnv,execStatus,EXTERNAL_FUNCTION_DATA))

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _EXTNFUNC_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#ifdef LOCALE
struct FunctionHash
  {
   struct FunctionDefinition *fdPtr;
   struct FunctionHash *next;
  };

#define SIZE_FUNCTION_HASH 517
#endif

   LOCALE int                            DefineFunction(char *,int,int (*)(void),char *);
   LOCALE int                            DefineFunction2(char *,int,int (*)(void),char *,char *);

   LOCALE void                           InitializeExternalFunctionData(void *,EXEC_STATUS);
   LOCALE int                            EnvDefineFunction(void *,EXEC_STATUS,char *,int,
                                                           int (*)(void *,EXEC_STATUS),char *);
   LOCALE int                            EnvDefineFunction2(void *,EXEC_STATUS,char *,int,
                                                            int (*)(void *,EXEC_STATUS),char *,char *);
   LOCALE int                            EnvDefineFunctionWithContext(void *,EXEC_STATUS,char *,int,
                                                           int (*)(void *,EXEC_STATUS),char *,void *);
   LOCALE int                            EnvDefineFunction2WithContext(void *,EXEC_STATUS,char *,int,
                                                            int (*)(void *,EXEC_STATUS),char *,char *,void *);
   LOCALE int                            DefineFunction3(void *,EXEC_STATUS,char *,int,
                                                         int (*)(void *,EXEC_STATUS),char *,char *,intBool,void *);
   LOCALE int                            AddFunctionParser(void *,EXEC_STATUS,char *,
                                                           struct expr *(*)( void *,struct expr *,char *));
   LOCALE int                            RemoveFunctionParser(void *,EXEC_STATUS,char *);
   LOCALE int                            FuncSeqOvlFlags(void *,EXEC_STATUS,char *,int,int);
   LOCALE struct FunctionDefinition     *GetFunctionList(void *,EXEC_STATUS);
   LOCALE void                           InstallFunctionList(void *,EXEC_STATUS,struct FunctionDefinition *);
   LOCALE struct FunctionDefinition     *FindFunction(void *,EXEC_STATUS,char *);
   LOCALE int                            GetNthRestriction(struct FunctionDefinition *,int);
   LOCALE char                          *GetArgumentTypeName(int);
   LOCALE int                            UndefineFunction(void *,EXEC_STATUS,char *);
   LOCALE int                            GetMinimumArgs(struct FunctionDefinition *);
   LOCALE int                            GetMaximumArgs(struct FunctionDefinition *);

#endif




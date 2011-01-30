   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
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

struct FunctionDefinition
  {
   struct symbolHashNode *callFunctionName;
   char *actualFunctionName;
   char returnValueType;
   int (*functionPointer)(void);
   struct expr *(*parser)(struct expr *,char *);
   char *restrictions;
   short int overloadable;
   short int sequenceuseok;
   short int bsaveIndex;
   struct FunctionDefinition *next;
   struct userData *usrData;
  };

#define ValueFunctionType(target) (((struct FunctionDefinition *) target)->returnValueType)
#define ExpressionFunctionType(target) (((struct FunctionDefinition *) ((target)->value))->returnValueType)
#define ExpressionFunctionPointer(target) (((struct FunctionDefinition *) ((target)->value))->functionPointer)
#define ExpressionFunctionCallName(target) (((struct FunctionDefinition *) ((target)->value))->callFunctionName)
#define ExpressionFunctionRealName(target) (((struct FunctionDefinition *) ((target)->value))->actualFunctionName)

#define PTIF (int (*)(void))

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

#define SIZE_FUNCTION_HASH 51
#endif

   LOCALE int                            DefineFunction(char *,int,int (*)(void),char *);
   LOCALE int                            DefineFunction2(char *,int,int (*)(void),char *,char *);
   LOCALE int                            AddFunctionParser(char *,struct expr *(*)(struct expr *,char *));
   LOCALE int                            RemoveFunctionParser(char *);
   LOCALE int                            FuncSeqOvlFlags(char *,int,int);
   LOCALE struct FunctionDefinition     *GetFunctionList(void);
   LOCALE void                           InstallFunctionList(struct FunctionDefinition *);
   LOCALE struct FunctionDefinition     *FindFunction(char *);
   LOCALE int                            GetNthRestriction(struct FunctionDefinition *,int);
   LOCALE char                          *GetArgumentTypeName(int);
   LOCALE int                            UndefineFunction(char *);

#endif





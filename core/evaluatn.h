   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*               EVALUATION HEADER FILE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides routines for evaluating expressions.    */
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
/*            Added EvaluateAndStoreInDataObject function.   */
/*                                                           */
/*************************************************************/

#ifndef _H_evaluatn

#define _H_evaluatn

struct entityRecord;
struct dataObject;

#ifndef _H_constant
#include "constant.h"
#endif
#ifndef _H_symbol
#include "symbol.h"
#endif
#ifndef _H_expressn
#include "expressn.h"
#endif

struct dataObject
  {
   void *supplementalInfo;
   unsigned short type;
   void *value;
   long begin;
   long end;
   struct dataObject *next;
  };

typedef struct dataObject DATA_OBJECT;
typedef struct dataObject * DATA_OBJECT_PTR;

typedef struct expr FUNCTION_REFERENCE;

#define DATA_OBJECT_PTR_ARG DATA_OBJECT_PTR

#include "userdata.h"

struct entityRecord
  {
   char *name;
   unsigned int type : 13;
   unsigned int copyToEvaluate : 1;
   unsigned int bitMap : 1;
   unsigned int addsToRuleComplexity : 1;
   void (*shortPrintFunction)(void *,EXEC_STATUS,char *,void *);
   void (*longPrintFunction)(void *,EXEC_STATUS,char *,void *);
   intBool (*deleteFunction)(void *,EXEC_STATUS,void *);
   intBool (*evaluateFunction)(void *,EXEC_STATUS,void *,DATA_OBJECT *);
   void *(*getNextFunction)(void *,EXEC_STATUS,void *);
   void (*decrementBusyCount)(void *,EXEC_STATUS,void *);
   void (*incrementBusyCount)(void *,EXEC_STATUS,void *);
   void (*propagateDepth)(void *,EXEC_STATUS,void *);
   void (*markNeeded)(void *,EXEC_STATUS,void *);
   void (*install)(void *,EXEC_STATUS,void *);
   void (*deinstall)(void *,EXEC_STATUS,void *);
   struct userData *usrData;
  };

struct externalAddressType
  {
   char *name;
   void (*shortPrintFunction)(void *,EXEC_STATUS,char *,void *);
   void (*longPrintFunction)(void *,EXEC_STATUS,char *,void *);
   intBool (*discardFunction)(void *,EXEC_STATUS,void *);
   void (*newFunction)(void *,EXEC_STATUS,DATA_OBJECT *);
   intBool (*callFunction)(void *,EXEC_STATUS,DATA_OBJECT *,DATA_OBJECT *);
  };

typedef struct entityRecord ENTITY_RECORD;
typedef struct entityRecord * ENTITY_RECORD_PTR;

#define GetDOLength(target)       (((target).end - (target).begin) + 1)
#define GetpDOLength(target)      (((target)->end - (target)->begin) + 1)
#define GetDOBegin(target)        ((target).begin + 1)
#define GetDOEnd(target)          ((target).end + 1)
#define GetpDOBegin(target)       ((target)->begin + 1)
#define GetpDOEnd(target)         ((target)->end + 1)
#define SetDOBegin(target,val)   ((target).begin = (long) ((val) - 1))
#define SetDOEnd(target,val)     ((target).end = (long) ((val) - 1))
#define SetpDOBegin(target,val)   ((target)->begin = (long) ((val) - 1))
#define SetpDOEnd(target,val)     ((target)->end = (long) ((val) - 1))

#define EnvGetDOLength(theEnv,execStatus,target)       (((target).end - (target).begin) + 1)
#define EnvGetpDOLength(theEnv,execStatus,target)      (((target)->end - (target)->begin) + 1)
#define EnvGetDOBegin(theEnv,execStatus,target)        ((target).begin + 1)
#define EnvGetDOEnd(theEnv,execStatus,target)          ((target).end + 1)
#define EnvGetpDOBegin(theEnv,execStatus,target)       ((target)->begin + 1)
#define EnvGetpDOEnd(theEnv,execStatus,target)         ((target)->end + 1)
#define EnvSetDOBegin(theEnv,execStatus,target,val)   ((target).begin = (long) ((val) - 1))
#define EnvSetDOEnd(theEnv,execStatus,target,val)     ((target).end = (long) ((val) - 1))
#define EnvSetpDOBegin(theEnv,execStatus,target,val)   ((target)->begin = (long) ((val) - 1))
#define EnvSetpDOEnd(theEnv,execStatus,target,val)     ((target)->end = (long) ((val) - 1))

#define DOPToString(target) (((struct symbolHashNode *) ((target)->value))->contents)
#define DOPToDouble(target) (((struct floatHashNode *) ((target)->value))->contents)
#define DOPToFloat(target) ((float) (((struct floatHashNode *) ((target)->value))->contents))
#define DOPToLong(target) (((struct integerHashNode *) ((target)->value))->contents)
#define DOPToInteger(target) ((int) (((struct integerHashNode *) ((target)->value))->contents))
#define DOPToPointer(target)       ((target)->value)
#define DOPToExternalAddress(target) (((struct externalAddressHashNode *) ((target)->value))->externalAddress)

#define EnvDOPToString(theEnv,execStatus,target) (((struct symbolHashNode *) ((target)->value))->contents)
#define EnvDOPToDouble(theEnv,execStatus,target) (((struct floatHashNode *) ((target)->value))->contents)
#define EnvDOPToFloat(theEnv,execStatus,target) ((float) (((struct floatHashNode *) ((target)->value))->contents))
#define EnvDOPToLong(theEnv,execStatus,target) (((struct integerHashNode *) ((target)->value))->contents)
#define EnvDOPToInteger(theEnv,execStatus,target) ((int) (((struct integerHashNode *) ((target)->value))->contents))
#define EnvDOPToPointer(theEnv,execStatus,target)       ((target)->value)
#define EnvDOPToExternalAddress(target) (((struct externalAddressHashNode *) ((target)->value))->externalAddress)

#define DOToString(target) (((struct symbolHashNode *) ((target).value))->contents)
#define DOToDouble(target) (((struct floatHashNode *) ((target).value))->contents)
#define DOToFloat(target) ((float) (((struct floatHashNode *) ((target).value))->contents))
#define DOToLong(target) (((struct integerHashNode *) ((target).value))->contents)
#define DOToInteger(target) ((int) (((struct integerHashNode *) ((target).value))->contents))
#define DOToPointer(target)        ((target).value)
#define DOToExternalAddress(target) (((struct externalAddressHashNode *) ((target).value))->externalAddress))

#define EnvDOToString(theEnv,execStatus,target) (((struct symbolHashNode *) ((target).value))->contents)
#define EnvDOToDouble(theEnv,execStatus,target) (((struct floatHashNode *) ((target).value))->contents)
#define EnvDOToFloat(theEnv,execStatus,target) ((float) (((struct floatHashNode *) ((target).value))->contents))
#define EnvDOToLong(theEnv,execStatus,target) (((struct integerHashNode *) ((target).value))->contents)
#define EnvDOToInteger(theEnv,execStatus,target) ((int) (((struct integerHashNode *) ((target).value))->contents))
#define EnvDOToPointer(theEnv,execStatus,target)        ((target).value)
#define EnvDOToExternalAddress(target) (((struct externalAddressHashNode *) ((target).value))->externalAddress))

#define CoerceToLongInteger(t,v) ((t == INTEGER) ? ValueToLong(v) : (long int) ValueToDouble(v))
#define CoerceToInteger(t,v) ((t == INTEGER) ? (int) ValueToLong(v) : (int) ValueToDouble(v))
#define CoerceToDouble(t,v) ((t == INTEGER) ? (double) ValueToLong(v) : ValueToDouble(v))

#define GetFirstArgument()   (execStatus->CurrentExpression->argList)
#define GetNextArgument(ep)  (ep->nextArg)

#define MAXIMUM_PRIMITIVES 150
#define MAXIMUM_EXTERNAL_ADDRESS_TYPES 10

#define BITS_PER_BYTE    8

#define BitwiseTest(n,b)   ((n) & (char) (1 << (b)))
#define BitwiseSet(n,b)    (n |= (char) (1 << (b)))
#define BitwiseClear(n,b)  (n &= (char) ~(1 << (b)))

#define TestBitMap(map,id)  BitwiseTest(map[(id) / BITS_PER_BYTE],(id) % BITS_PER_BYTE)
#define SetBitMap(map,id)   BitwiseSet(map[(id) / BITS_PER_BYTE],(id) % BITS_PER_BYTE)
#define ClearBitMap(map,id) BitwiseClear(map[(id) / BITS_PER_BYTE],(id) % BITS_PER_BYTE)

#define EVALUATION_DATA 44

// STEFAN: new additional parameter that needs to be passed around similar to
//         theEnv. But needs to be handled independently for different threads.
struct executionStatus
  {
   struct expr *CurrentExpression;
   int EvaluationError;
   int HaltExecution;
   int CurrentEvaluationDepth;
  };

// STEFAN: parameter macro for the new executionStatus
#define EXEC_STATUS struct executionStatus* execStatus

struct evaluationData
  {
   int numberOfAddressTypes;
   struct entityRecord *PrimitivesArray[MAXIMUM_PRIMITIVES];
   struct externalAddressType *ExternalAddressTypes[MAXIMUM_EXTERNAL_ADDRESS_TYPES];
  };

#define EvaluationData(theEnv,execStatus) ((struct evaluationData *) GetEnvironmentData(theEnv,execStatus,EVALUATION_DATA))

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _EVALUATN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#define SetMultifieldErrorValue(a) EnvSetMultifieldErrorValue(GetCurrentEnvironment(),a)
#define FunctionCall(a,b,c) EnvFunctionCall(GetCurrentEnvironment(),a,b,c)

   LOCALE void                           InitializeEvaluationData(void *,EXEC_STATUS);
   LOCALE int                            EvaluateExpression(void *,EXEC_STATUS,struct expr *,struct dataObject *);
   LOCALE void                           SetEvaluationError(void *,EXEC_STATUS,intBool);
   LOCALE int                            GetEvaluationError(void *,EXEC_STATUS);
   LOCALE void                           SetHaltExecution(void *,EXEC_STATUS,int);
   LOCALE int                            GetHaltExecution(void *,EXEC_STATUS);
   LOCALE void                           ReturnValues(void *,EXEC_STATUS,struct dataObject *,intBool);
   LOCALE void                           PrintDataObject(void *,EXEC_STATUS,char *,struct dataObject *);
   LOCALE void                           EnvSetMultifieldErrorValue(void *,EXEC_STATUS,struct dataObject *);
   LOCALE void                           ValueInstall(void *,EXEC_STATUS,struct dataObject *);
   LOCALE void                           ValueDeinstall(void *,EXEC_STATUS,struct dataObject *);
   LOCALE void                           PropagateReturnValue(void *,EXEC_STATUS,struct dataObject *);
#if DEFFUNCTION_CONSTRUCT || DEFGENERIC_CONSTRUCT
   LOCALE int                            EnvFunctionCall(void *,EXEC_STATUS,char *,char *,DATA_OBJECT *);
   LOCALE int                            FunctionCall2(void *,EXEC_STATUS,FUNCTION_REFERENCE *,char *,DATA_OBJECT *);
#endif
   LOCALE void                           CopyDataObject(void *,EXEC_STATUS,DATA_OBJECT *,DATA_OBJECT *,int);
   LOCALE void                           AtomInstall(void *,EXEC_STATUS,int,void *);
   LOCALE void                           AtomDeinstall(void *,EXEC_STATUS,int,void *);
   LOCALE struct expr                   *ConvertValueToExpression(void *,EXEC_STATUS,DATA_OBJECT *);
   LOCALE unsigned long                  GetAtomicHashValue(unsigned short,void *,int);
   LOCALE void                           InstallPrimitive(void *,EXEC_STATUS,struct entityRecord *,int);
   LOCALE int                            InstallExternalAddressType(void *,EXEC_STATUS,struct externalAddressType *);
   LOCALE void                           TransferDataObjectValues(DATA_OBJECT *,DATA_OBJECT *);
   LOCALE struct expr                   *FunctionReferenceExpression(void *,EXEC_STATUS,char *);
   LOCALE intBool                        GetFunctionReference(void *,EXEC_STATUS,char *,FUNCTION_REFERENCE *);
   LOCALE intBool                        DOsEqual(DATA_OBJECT_PTR,DATA_OBJECT_PTR);
   LOCALE int                            EvaluateAndStoreInDataObject(void *,EXEC_STATUS,int,EXPRESSION *,DATA_OBJECT *,int);

#endif







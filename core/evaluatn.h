   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
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
   int type;
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
   void (*shortPrintFunction)(char *,void *);
   void (*longPrintFunction)(char *,void *);
   BOOLEAN (*deleteFunction)(void *);
   BOOLEAN (*evaluateFunction)(void *,DATA_OBJECT *);
   void *(*getNextFunction)(void *);
   void (*decrementBusyCount)(void *);
   void (*incrementBusyCount)(void *);
   void (*propagateDepth)(void *);
   void (*markNeeded)(void *);
   void (*install)(void *);
   void (*deinstall)(void *);
   struct userData *usrData;
  };

typedef struct entityRecord ENTITY_RECORD;
typedef struct entityRecord * ENTITY_RECORD_PTR;

#define GetDOLength(target)       (((target).end - (target).begin) + 1)
#define GetpDOLength(target)      (((target)->end - (target)->begin) + 1)
#define GetDOBegin(target)        ((target).begin + 1)
#define GetDOEnd(target)          ((target).end + 1)
#define GetpDOBegin(target)       ((target)->begin + 1)
#define GetpDOEnd(target)         ((target)->end + 1)
#define SetDOBegin(target,val)   ((target).begin = (val) - 1)
#define SetDOEnd(target,val)     ((target).end = (val) - 1)
#define SetpDOBegin(target,val)   ((target)->begin = (val) - 1)
#define SetpDOEnd(target,val)     ((target)->end = (val) - 1)

#define DOPToString(target) (((struct symbolHashNode *) (target->value))->contents)
#define DOPToDouble(target) (((struct floatHashNode *) (target->value))->contents)
#define DOPToFloat(target) ((float) (((struct floatHashNode *) (target->value))->contents))
#define DOPToLong(target) (((struct integerHashNode *) (target->value))->contents)
#define DOPToInteger(target) ((int) (((struct integerHashNode *) (target->value))->contents))
#define DOPToPointer(target)       ((target)->value)

#define DOToString(target) (((struct symbolHashNode *) (target.value))->contents)
#define DOToDouble(target) (((struct floatHashNode *) (target.value))->contents)
#define DOToFloat(target) ((float) (((struct floatHashNode *) (target.value))->contents))
#define DOToLong(target) (((struct integerHashNode *) (target.value))->contents)
#define DOToInteger(target) ((int) (((struct integerHashNode *) (target.value))->contents))
#define DOToPointer(target)        ((target).value)

#define CoerceToLongInteger(t,v) ((t == INTEGER) ? ValueToLong(v) : (long int) ValueToDouble(v))
#define CoerceToInteger(t,v) ((t == INTEGER) ? (int) ValueToLong(v) : (int) ValueToDouble(v))
#define CoerceToDouble(t,v) ((t == INTEGER) ? (double) ValueToLong(v) : ValueToDouble(v))

#define GetFirstArgument()           (CurrentExpression->argList)
#define GetNextArgument(ep)          (ep->nextArg)

#define MAXIMUM_PRIMITIVES 150

#define BITS_PER_BYTE    8

#define BitwiseTest(n,b)   ((n) & (char) (1 << (b)))
#define BitwiseSet(n,b)    (n |= (char) (1 << (b)))
#define BitwiseClear(n,b)  (n &= (char) ~(1 << (b)))

#define TestBitMap(map,id)  BitwiseTest(map[(id) / BITS_PER_BYTE],(id) % BITS_PER_BYTE)
#define SetBitMap(map,id)   BitwiseSet(map[(id) / BITS_PER_BYTE],(id) % BITS_PER_BYTE)
#define ClearBitMap(map,id) BitwiseClear(map[(id) / BITS_PER_BYTE],(id) % BITS_PER_BYTE)

#define CLIPSFunctionCall(x,y,z) FunctionCall(x,y,z)

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _EVALUATN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE int                            EvaluateExpression(struct expr *,struct dataObject *);
   LOCALE void                           SetEvaluationError(BOOLEAN);
   LOCALE int                            GetEvaluationError(void);
   LOCALE DllExport void                 SetHaltExecution(int);
   LOCALE int                            GetHaltExecution(void);
   LOCALE void                           ReturnValues(struct dataObject *);
   LOCALE void                           PrintDataObject(char *,struct dataObject *);
   LOCALE DllExport void                 SetMultifieldErrorValue(struct dataObject *);
   LOCALE void                           ValueInstall(struct dataObject *);
   LOCALE void                           ValueDeinstall(struct dataObject *);
   LOCALE void                           PropagateReturnValue(struct dataObject *);
#if DEFFUNCTION_CONSTRUCT || DEFGENERIC_CONSTRUCT
   LOCALE int                            FunctionCall(char *,char *,DATA_OBJECT *);
   LOCALE int                            FunctionCall2(FUNCTION_REFERENCE *,char *,DATA_OBJECT *);
#endif
   LOCALE void                           CopyDataObject(DATA_OBJECT *,DATA_OBJECT *,int);
   LOCALE void                           AtomInstall(int,void *);
   LOCALE void                           AtomDeinstall(int,void *);
   LOCALE struct expr                   *ConvertValueToExpression(DATA_OBJECT *);
   LOCALE unsigned int                   GetAtomicHashValue(int,void *,int);
   LOCALE void                           InstallPrimitive(struct entityRecord *,int);
   LOCALE void                           TransferDataObjectValues(DATA_OBJECT *,DATA_OBJECT *);
   LOCALE struct expr                   *FunctionReferenceExpression(char *);
   LOCALE BOOLEAN                        GetFunctionReference(char *,FUNCTION_REFERENCE *);
   LOCALE BOOLEAN                        DOsEqual(DATA_OBJECT_PTR,DATA_OBJECT_PTR);

#ifndef _EVALUATN_SOURCE_
   extern Thread struct expr            *CurrentExpression;
   extern Thread int                     EvaluationError;
   extern Thread int                     HaltExecution;
   extern Thread int                     CurrentEvaluationDepth;
   extern Thread struct entityRecord    *PrimitivesArray[];
#endif

#endif







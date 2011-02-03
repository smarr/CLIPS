   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*                MULTIFIELD HEADER FILE               */
   /*******************************************************/

/*************************************************************/
/* Purpose: Routines for creating and manipulating           */
/*   multifield values.                                      */
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
/*            Moved ImplodeMultifield from multifun.c.       */
/*                                                           */
/*************************************************************/

#ifndef _H_multifld

#define _H_multifld

struct field;
struct multifield;

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

struct field
  {
   unsigned short type;
   void *value;
  };

struct multifield
  {
   unsigned busyCount;
   short depth;
   long multifieldLength;
   struct multifield *next;
   struct field theFields[1];
  };

typedef struct multifield SEGMENT;
typedef struct multifield * SEGMENT_PTR;
typedef struct multifield * MULTIFIELD_PTR;
typedef struct field FIELD;
typedef struct field * FIELD_PTR;

#define GetMFLength(target)     (((struct multifield *) (target))->multifieldLength)
#define GetMFPtr(target,index)  (&(((struct field *) ((struct multifield *) (target))->theFields)[index-1]))
#define SetMFType(target,index,value)  (((struct field *) ((struct multifield *) (target))->theFields)[index-1].type = (unsigned short) (value))
#define SetMFValue(target,index,val)  (((struct field *) ((struct multifield *) (target))->theFields)[index-1].value = (void *) (val))
#define GetMFType(target,index)  (((struct field *) ((struct multifield *) (target))->theFields)[index-1].type)
#define GetMFValue(target,index)  (((struct field *) ((struct multifield *) (target))->theFields)[index-1].value)

#define EnvGetMFLength(theEnv,execStatus,target)     (((struct multifield *) (target))->multifieldLength)
#define EnvGetMFPtr(theEnv,execStatus,target,index)  (&(((struct field *) ((struct multifield *) (target))->theFields)[index-1]))
#define EnvSetMFType(theEnv,execStatus,target,index,value)  (((struct field *) ((struct multifield *) (target))->theFields)[index-1].type = (unsigned short) (value))
#define EnvSetMFValue(theEnv,execStatus,target,index,val)  (((struct field *) ((struct multifield *) (target))->theFields)[index-1].value = (void *) (val))
#define EnvGetMFType(theEnv,execStatus,target,index)  (((struct field *) ((struct multifield *) (target))->theFields)[index-1].type)
#define EnvGetMFValue(theEnv,execStatus,target,index)  (((struct field *) ((struct multifield *) (target))->theFields)[index-1].value)

/*==================*/
/* ENVIRONMENT DATA */
/*==================*/

#define MULTIFIELD_DATA 51

struct multifieldData
  { 
   struct multifield *ListOfMultifields;
  };

#define MultifieldData(theEnv,execStatus) ((struct multifieldData *) GetEnvironmentData(theEnv,execStatus,MULTIFIELD_DATA))

#ifdef LOCALE
#undef LOCALE
#endif
#ifdef _MULTIFLD_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif
   
#define CreateMultifield(a) EnvCreateMultifield(GetCurrentEnvironment(),GetCurrentExecutionStatus(),a)

   LOCALE void                           InitializeMultifieldData(void *,EXEC_STATUS);
   LOCALE void                          *CreateMultifield2(void *,EXEC_STATUS,long);
   LOCALE void                           ReturnMultifield(void *,EXEC_STATUS,struct multifield *);
   LOCALE void                           MultifieldInstall(void *,EXEC_STATUS,struct multifield *);
   LOCALE void                           MultifieldDeinstall(void *,EXEC_STATUS,struct multifield *);
   LOCALE struct multifield             *StringToMultifield(void *,EXEC_STATUS,char *);
   LOCALE void                          *EnvCreateMultifield(void *,EXEC_STATUS,long);
   LOCALE void                           AddToMultifieldList(void *,EXEC_STATUS,struct multifield *);
   LOCALE void                           FlushMultifields(void *,EXEC_STATUS);
   LOCALE void                           DuplicateMultifield(void *,EXEC_STATUS,struct dataObject *,struct dataObject *);
   LOCALE void                           PrintMultifield(void *,EXEC_STATUS,char *,SEGMENT_PTR,long,long,int);
   LOCALE intBool                        MultifieldDOsEqual(DATA_OBJECT_PTR,DATA_OBJECT_PTR);
   LOCALE void                           StoreInMultifield(void *,EXEC_STATUS,DATA_OBJECT *,EXPRESSION *,int);
   LOCALE void                          *CopyMultifield(void *,EXEC_STATUS,struct multifield *);
   LOCALE intBool                        MultifieldsEqual(struct multifield *,struct multifield *);
   LOCALE void                          *DOToMultifield(void *,EXEC_STATUS,DATA_OBJECT *);
   LOCALE unsigned long                  HashMultifield(struct multifield *,unsigned long);
   LOCALE struct multifield             *GetMultifieldList(void *,EXEC_STATUS);
   LOCALE void                          *ImplodeMultifield(void *,EXEC_STATUS,DATA_OBJECT *);

#endif





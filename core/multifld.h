   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
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
   short int type;
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
#define SetMFType(target,index,value)  (((struct field *) ((struct multifield *) (target))->theFields)[index-1].type = (value))
#define SetMFValue(target,index,val)  (((struct field *) ((struct multifield *) (target))->theFields)[index-1].value = (void *) (val))
#define GetMFType(target,index)  (((struct field *) ((struct multifield *) (target))->theFields)[index-1].type)
#define GetMFValue(target,index)  (((struct field *) ((struct multifield *) (target))->theFields)[index-1].value)

#ifdef LOCALE
#undef LOCALE
#endif
#ifdef _MULTIFLD_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                          *CreateMultifield2(long);
   LOCALE void                           ReturnMultifield(struct multifield *);
   LOCALE void                           MultifieldInstall(struct multifield *);
   LOCALE void                           MultifieldDeinstall(struct multifield *);
   LOCALE struct multifield             *StringToMultifield(char *);
   LOCALE DllExport void                *CreateMultifield(long);
   LOCALE void                           AddToMultifieldList(struct multifield *);
   LOCALE void                           FlushMultifields(void);
   LOCALE void                           DuplicateMultifield(struct dataObject *,struct dataObject *);
   LOCALE void                           PrintMultifield(char *,SEGMENT_PTR,long,long,int);
   LOCALE BOOLEAN                        MultifieldDOsEqual(DATA_OBJECT_PTR,DATA_OBJECT_PTR);
   LOCALE void                           StoreInMultifield(DATA_OBJECT *,EXPRESSION *,int);
   LOCALE void                          *CopyMultifield(struct multifield *);
   LOCALE BOOLEAN                        MultifieldsEqual(struct multifield *,struct multifield *);
   LOCALE void                          *DOToMultifield(DATA_OBJECT *);
   LOCALE int                            HashMultifield(struct multifield *,int);
   LOCALE struct multifield             *GetMultifieldList(void);

#endif






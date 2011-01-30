   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.05  04/09/97          */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_objrtfnx
#define _H_objrtfnx

#if INSTANCE_PATTERN_MATCHING

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_object
#include "object.h"
#endif
#ifndef _H_match
#include "match.h"
#endif

struct ObjectMatchVar1
  {
   unsigned whichSlot     : 15;
   unsigned whichPattern  : 8;
   unsigned whichField    : 8;
   unsigned objectAddress : 1;
   unsigned allFields     : 1;
  };

struct ObjectMatchVar2
  {
   unsigned whichSlot       : 15;
   unsigned fromBeginning   : 1;
   unsigned beginningOffset : 7;
   unsigned fromEnd         : 1;
   unsigned endOffset       : 7;
   unsigned whichPattern    : 8;
  };

struct ObjectMatchLength
  {
   unsigned minLength : 15;
   unsigned exactly   : 1;
  };

struct ObjectCmpPNConstant
  {
   unsigned offset        : 7;
   unsigned pass          : 1;
   unsigned fail          : 1;
   unsigned general       : 1;
   unsigned fromBeginning : 1;
  };

struct ObjectCmpPNSingleSlotVars1
  {
   unsigned firstSlot  : 15;
   unsigned pass       : 1;
   unsigned secondSlot : 15;
   unsigned fail       : 1;
  };

struct ObjectCmpPNSingleSlotVars2
  {
   unsigned firstSlot     : 15;
   unsigned pass          : 1;
   unsigned secondSlot    : 15;
   unsigned fail          : 1;
   unsigned offset        : 7;
   unsigned fromBeginning : 1;
  };

struct ObjectCmpPNSingleSlotVars3
  {
   unsigned firstSlot           : 15;
   unsigned pass                : 1;
   unsigned secondSlot          : 15;
   unsigned fail                : 1;
   unsigned firstOffset         : 7;
   unsigned firstFromBeginning  : 1;
   unsigned secondOffset        : 7;
   unsigned secondFromBeginning : 1;
  };

struct ObjectCmpJoinSingleSlotVars1
  {
   unsigned firstSlot     : 15;
   unsigned pass          : 1;
   unsigned secondSlot    : 15;
   unsigned fail          : 1;
   unsigned firstPattern  : 8;
   unsigned secondPattern : 8;
  };

struct ObjectCmpJoinSingleSlotVars2
  {
   unsigned firstSlot     : 15;
   unsigned pass          : 1;
   unsigned secondSlot    : 15;
   unsigned fromBeginning : 1;
   unsigned firstPattern  : 8;
   unsigned secondPattern : 8;
   unsigned offset        : 7;
   unsigned fail          : 1;
  };

struct ObjectCmpJoinSingleSlotVars3
  {
   unsigned firstSlot           : 15;
   unsigned pass                : 1;
   unsigned secondSlot          : 15;
   unsigned fail                : 1;
   unsigned firstPattern        : 8;
   unsigned secondPattern       : 8;
   unsigned firstOffset         : 7;
   unsigned firstFromBeginning  : 1;
   unsigned secondOffset        : 7;
   unsigned secondFromBeginning : 1;
  };

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _OBJRTFNX_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

LOCALE void InstallObjectPrimitives(void);
LOCALE BOOLEAN ObjectCmpConstantFunction(void *,DATA_OBJECT *);

#ifndef _OBJRTFNX_SOURCE_
extern Thread INSTANCE_TYPE *CurrentPatternObject;
extern Thread INSTANCE_SLOT *CurrentPatternObjectSlot;
extern Thread int CurrentObjectSlotLength;
extern Thread struct multifieldMarker *CurrentPatternObjectMarks;
#endif

#endif

#endif







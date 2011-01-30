   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*           DEFMODULE BSAVE/BLOAD HEADER FILE         */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_modulbin

#define _H_modulbin

#ifndef _H_moduldef
#include "moduldef.h"
#endif

struct bsaveDefmodule
  {
   long name;
   long importList;
   long exportList;
   long next;
   long bsaveID;
  };

struct bsaveDefmoduleItemHeader
  {
   long theModule;
   long firstItem;
   long lastItem;
  };

struct bsavePortItem
  {
   long moduleName;
   long constructType;
   long constructName;
   long next;
  };

#define ModulePointer(i) ((struct defmodule *) (&DefmoduleArray[i]))

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _MODULBIN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           DefmoduleBinarySetup(void);
   LOCALE void                           UpdateDefmoduleItemHeader
                                                 (struct bsaveDefmoduleItemHeader *,
                                                  struct defmoduleItemHeader *,int,void *);

#if BLOAD_AND_BSAVE
   LOCALE void                           AssignBsaveDefmdlItemHdrVals
                                                 (struct bsaveDefmoduleItemHeader *,
                                                  struct defmoduleItemHeader *);
#endif

#ifndef _MODULBIN_SOURCE_
   extern Thread struct defmodule            *DefmoduleArray;
#endif

#endif








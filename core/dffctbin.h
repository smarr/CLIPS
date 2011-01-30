   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*           DEFFACTS BSAVE/BLOAD HEADER FILE          */
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

#if (! RUN_TIME)
#ifndef _H_dffctbin

#define _H_dffctbin

#include "modulbin.h"
#include "cstrcbin.h"
#ifndef _H_constrct
#include "constrct.h"
#endif

struct bsaveDeffacts
  {
   struct bsaveConstructHeader header;
   long assertList;
  };

struct bsaveDeffactsModule
  {
   struct bsaveDefmoduleItemHeader header;
  };

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _DFFCTBIN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           DeffactsBinarySetup(void);
   LOCALE void                          *BloadDeffactsModuleReference(int);

#endif
#endif







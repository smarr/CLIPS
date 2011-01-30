   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                DEFFACTS HEADER FILE                 */
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

#ifndef _H_dffctdef
#define _H_dffctdef

#ifndef _H_symbol
#include "symbol.h"
#endif
#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_constrct
#include "constrct.h"
#endif
#ifndef _H_moduldef
#include "moduldef.h"
#endif
#ifndef _H_cstrccom
#include "cstrccom.h"
#endif

struct deffacts
  {
   struct constructHeader header;
   struct expr *assertList;
  };

struct deffactsModule
  {
   struct defmoduleItemHeader header;
  };

#define GetDeffactsName(x) GetConstructNameString((struct constructHeader *) x)
#define GetDeffactsPPForm(x) GetConstructPPForm((struct constructHeader *) x)
#define DeffactsModule(x) GetConstructModuleName((struct constructHeader *) x)

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _DFFCTDEF_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           InitializeDeffacts(void);
   LOCALE void                          *FindDeffacts(char *);
   LOCALE void                          *GetNextDeffacts(void *);
   LOCALE void                           CreateInitialFactDeffacts(void);
   LOCALE BOOLEAN                        IsDeffactsDeletable(void *);
   LOCALE struct deffactsModule         *GetDeffactsModuleItem(struct defmodule *);

#ifndef _DFFCTDEF_SOURCE_
   extern Thread struct construct              *DeffactsConstruct;
   extern Thread int                            DeffactsModuleIndex;
#endif


#endif



   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                DEFGLOBAL HEADER FILE                */
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

#ifndef _H_globldef
#define _H_globldef

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

struct defglobal
  {
   struct constructHeader header;
   unsigned int watch   : 1;
   unsigned int inScope : 1;
   long busyCount;
   DATA_OBJECT current;
   struct expr *initial;
  };

struct defglobalModule
  {
   struct defmoduleItemHeader header;
  };

#define GetDefglobalName(x) GetConstructNameString((struct constructHeader *) x)
#define GetDefglobalPPForm(x) GetConstructPPForm((struct constructHeader *) x)
#define DefglobalModule(x) GetConstructModuleName((struct constructHeader *) x)

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _GLOBLDEF_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           InitializeDefglobals(void);
   LOCALE DllExport void                *FindDefglobal(char *);
   LOCALE DllExport void                *GetNextDefglobal(void *);
   LOCALE void                           CreateInitialFactDefglobal(void);
   LOCALE BOOLEAN                        IsDefglobalDeletable(void *);
   LOCALE struct defglobalModule        *GetDefglobalModuleItem(struct defmodule *);
   LOCALE void                           QSetDefglobalValue(struct defglobal *,DATA_OBJECT_PTR,int);
   LOCALE struct defglobal              *QFindDefglobal(struct symbolHashNode *);
   LOCALE DllExport void                 GetDefglobalValueForm(char *,int,void *);
   LOCALE int                            GetGlobalsChanged(void);
   LOCALE void                           SetGlobalsChanged(int);
   LOCALE DllExport BOOLEAN              GetDefglobalValue(char *,DATA_OBJECT_PTR);
   LOCALE BOOLEAN                        SetDefglobalValue(char *,DATA_OBJECT_PTR);
   LOCALE void                           UpdateDefglobalScope(void);
   LOCALE void                          *GetNextDefglobalInScope(void *);
   LOCALE int                            QGetDefglobalValue(void *,DATA_OBJECT_PTR);

#ifndef _GLOBLDEF_SOURCE_
   extern Thread struct construct              *DefglobalConstruct;
   extern Thread int                            DefglobalModuleIndex;
   extern Thread int                            ChangeToGlobals;
#endif


#endif



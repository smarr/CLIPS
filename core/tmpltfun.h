   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*          DEFTEMPLATE FUNCTION HEADER FILE           */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_tmpltfun

#define _H_tmpltfun

#ifndef _H_symbol
#include "symbol.h"
#endif
#ifndef _H_scanner
#include "scanner.h"
#endif
#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_factmngr
#include "factmngr.h"
#endif
#ifndef _H_tmpltdef
#include "tmpltdef.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _TMPLTFUN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE BOOLEAN                        UpdateModifyDuplicate(struct expr *,char *,void *);
   LOCALE struct expr                   *ModifyParse(struct expr *,char *);
   LOCALE struct expr                   *DuplicateParse(struct expr *,char *);
   LOCALE void                           DeftemplateFunctions(void);
   LOCALE void                           ModifyCommand(DATA_OBJECT_PTR);
   LOCALE void                           DuplicateCommand(DATA_OBJECT_PTR);

#endif










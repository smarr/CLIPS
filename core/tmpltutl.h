   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*          DEFTEMPLATE UTILITIES HEADER FILE          */
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

#ifndef _H_tmpltutl

#define _H_tmpltutl

#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_factmngr
#include "factmngr.h"
#endif
#ifndef _H_constrnt
#include "constrnt.h"
#endif
#ifndef _H_symbol
#include "symbol.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _TMPLTUTL_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           InvalidDeftemplateSlotMessage(char *,char *);
   LOCALE void                           SingleFieldSlotCardinalityError(char *);
   LOCALE void                           MultiIntoSingleFieldSlotError(struct templateSlot *,struct deftemplate *);
   LOCALE void                           CheckTemplateFact(struct fact *);
   LOCALE BOOLEAN                        CheckRHSSlotTypes(struct expr *,struct templateSlot *,char *);
   LOCALE struct templateSlot           *GetNthSlot(struct deftemplate *,int);
   LOCALE int                            FindSlotPosition(struct deftemplate *,struct symbolHashNode *);
   LOCALE void                           PrintTemplateFact(char *,struct fact *);
   LOCALE void                           UpdateDeftemplateScope(void);
   LOCALE struct templateSlot           *FindSlot(struct deftemplate *,struct symbolHashNode *,int *);
   LOCALE struct deftemplate            *CreateImpliedDeftemplate(SYMBOL_HN *,int);

#endif






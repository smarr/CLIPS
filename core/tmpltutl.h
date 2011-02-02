   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
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
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added additional arguments to                  */
/*            InvalidDeftemplateSlotMessage function.        */
/*                                                           */
/*            Added additional arguments to                  */
/*            PrintTemplateFact function.                    */
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
#include "fact/fact_manager.h"
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

   LOCALE void                           InvalidDeftemplateSlotMessage(void *,EXEC_STATUS,char *,char *,int);
   LOCALE void                           SingleFieldSlotCardinalityError(void *,EXEC_STATUS,char *);
   LOCALE void                           MultiIntoSingleFieldSlotError(void *,EXEC_STATUS,struct templateSlot *,struct deftemplate *);
   LOCALE void                           CheckTemplateFact(void *,EXEC_STATUS,struct fact *);
   LOCALE intBool                        CheckRHSSlotTypes(void *,EXEC_STATUS,struct expr *,struct templateSlot *,char *);
   LOCALE struct templateSlot           *GetNthSlot(struct deftemplate *,int);
   LOCALE int                            FindSlotPosition(struct deftemplate *,struct symbolHashNode *);
   LOCALE void                           PrintTemplateFact(void *,EXEC_STATUS,char *,struct fact *,int,int);
   LOCALE void                           UpdateDeftemplateScope(void *,EXEC_STATUS);
   LOCALE struct templateSlot           *FindSlot(struct deftemplate *,struct symbolHashNode *,short *);
   LOCALE struct deftemplate            *CreateImpliedDeftemplate(void *,EXEC_STATUS,SYMBOL_HN *,int);

#endif




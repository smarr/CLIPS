   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*           MULTIFIELD FUNCTIONS HEADER FILE          */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary Riley and Brian Donnell                         */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_multifun
#define _H_multifun

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _MULTIFUN_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ! RUN_TIME
   LOCALE void                    MultifieldFunctionDefinitions(void);
#endif
#if MULTIFIELD_FUNCTIONS
   LOCALE void                    DeleteFunction(DATA_OBJECT_PTR);
   LOCALE void                    MVDeleteFunction(DATA_OBJECT_PTR);
   LOCALE void                    ReplaceFunction(DATA_OBJECT_PTR);
   LOCALE void                    MVReplaceFunction(DATA_OBJECT_PTR);
   LOCALE void                    DeleteMemberFunction(DATA_OBJECT_PTR);
   LOCALE void                    ReplaceMemberFunction(DATA_OBJECT_PTR);
   LOCALE void                    InsertFunction(DATA_OBJECT_PTR);
   LOCALE void                    ExplodeFunction(DATA_OBJECT_PTR);
   LOCALE void                   *ImplodeFunction(void);
   LOCALE void                    SubseqFunction(DATA_OBJECT_PTR);
   LOCALE void                    MVSubseqFunction(DATA_OBJECT_PTR);
   LOCALE void                    FirstFunction(DATA_OBJECT_PTR);
   LOCALE void                    RestFunction(DATA_OBJECT_PTR);
   LOCALE void                    NthFunction(DATA_OBJECT_PTR);
   LOCALE BOOLEAN                 SubsetpFunction(void);
   LOCALE void                    MemberFunction(DATA_OBJECT_PTR);
   LOCALE void                    MultifieldPrognFunction(DATA_OBJECT_PTR);
   LOCALE void                    GetMvPrognField(DATA_OBJECT_PTR);
   LOCALE long                    GetMvPrognIndex(void);
#endif
   LOCALE int                     ReplaceMultiValueField(struct dataObject *,
                                                         struct dataObject *,
                                                         long,long,struct dataObject *,char *);
   LOCALE int                     InsertMultiValueField(struct dataObject *,
                                                        struct dataObject *,
                                                        long,struct dataObject *,char *);
   LOCALE int                     DeleteMultiValueField(struct dataObject *,struct dataObject *,
                                                        long,long,char *);

#endif


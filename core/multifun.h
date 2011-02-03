   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*           MULTIFIELD FUNCTIONS HEADER FILE          */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary Riley and Brian Dantes                          */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Moved ImplodeMultifield to multifld.c.         */
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

   LOCALE void                    MultifieldFunctionDefinitions(void *,EXEC_STATUS);
#if MULTIFIELD_FUNCTIONS
   LOCALE void                    DeleteFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                    MVDeleteFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                    ReplaceFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                    MVReplaceFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                    DeleteMemberFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                    ReplaceMemberFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                    InsertFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                    ExplodeFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                   *ImplodeFunction(void *,EXEC_STATUS);
   LOCALE void                    SubseqFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                    MVSubseqFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                    FirstFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                    RestFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                    NthFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE intBool                 SubsetpFunction(void *,EXEC_STATUS);
   LOCALE void                    MemberFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                    MultifieldPrognFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                    ForeachFunction(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE void                    GetMvPrognField(void *,EXEC_STATUS,DATA_OBJECT_PTR);
   LOCALE long                    GetMvPrognIndex(void *,EXEC_STATUS);
   LOCALE intBool                 FindDOsInSegment(DATA_OBJECT_PTR,int,DATA_OBJECT_PTR,
                                                   long *,long *,long *,int);
#endif
   LOCALE int                     ReplaceMultiValueField(void *,EXEC_STATUS,struct dataObject *,
                                                         struct dataObject *,
                                                         long,long,
                                                         struct dataObject *,char *);
   LOCALE int                     InsertMultiValueField(void *,EXEC_STATUS,struct dataObject *,
                                                        struct dataObject *,
                                                        long,struct dataObject *,char *);
   LOCALE int                     DeleteMultiValueField(void *,EXEC_STATUS,struct dataObject *,struct dataObject *,
                                                        long,long,char *);

#endif


   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  10/19/06            */
   /*                                                     */
   /*                  DRIVE HEADER FILE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Handles join network activity associated with    */
/*   with the addition of a data entity such as a fact or    */
/*   instance.                                               */
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
/*      6.30: Split out functions to improve performance.    */
/*                                                           */
/*************************************************************/

#ifndef _H_drive

#define _H_drive

#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_match
#include "match.h"
#endif
#ifndef _H_network
#include "network.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _DRIVE_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

void             NetworkAssert(void *,EXEC_STATUS,struct partialMatch *,struct joinNode *,
                               struct partialMatch** CallersLHSBinds,
                               struct partialMatch** CallersRHSBinds);
   intBool          EvaluateJoinExpression(void *,EXEC_STATUS,struct expr *,struct joinNode *);
   void             NetworkAssertLeft(void *,EXEC_STATUS,
                                      struct partialMatch*,
                                      struct joinNode*,
                                      struct partialMatch** CallersLHSBinds,
                                      struct partialMatch** CallersRHSBinds);
   void             NetworkAssertRight(void *,EXEC_STATUS,
                                       struct partialMatch*,
                                       struct joinNode*,
                                       struct partialMatch** CallersLHSBinds,
                                       struct partialMatch** CallersRHSBinds);
   void             PPDrive(void *,EXEC_STATUS,struct partialMatch *,struct partialMatch *,struct joinNode *);
   unsigned long    BetaMemoryHashValue(void *,EXEC_STATUS,struct expr *,struct partialMatch *,struct partialMatch *,struct joinNode *);
intBool          EvaluateSecondaryNetworkTest(void *,EXEC_STATUS,struct partialMatch *,struct joinNode *,
                                              struct partialMatch** CallersLHSBinds,
                                              struct partialMatch** CallersRHSBinds);
void             EPMDrive(void *,EXEC_STATUS,struct partialMatch *,struct joinNode *,
                          struct partialMatch** CallersLHSBinds,
                          struct partialMatch** CallersRHSBinds);
   
#endif






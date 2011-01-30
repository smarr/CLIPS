   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*           FACT RETE PRINT FUNCTIONS MODULE          */
   /*******************************************************/

/*************************************************************/
/* Purpose: Print routines for the fact rete primitives.     */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _FACTPRT_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT && DEFRULE_CONSTRUCT

#include "symbol.h"
#include "router.h"
#include "factgen.h"

#include "factprt.h"

/***************************************/
/* PrintFactJNCompVars1: Print routine */
/*   for the FactJNCompVars1 function. */
/***************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle void PrintFactJNCompVars1(
  char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   struct factCompVarsJN1Call *hack;

   hack = (struct factCompVarsJN1Call *) ValueToBitMap(theValue);
   PrintRouter(logicalName,"(fact-jn-cmp-vars1 ");
   if (hack->pass) PrintRouter(logicalName,"p ");
   else PrintRouter(logicalName,"n ");
   PrintLongInteger(logicalName,(long) hack->slot1);
   PrintRouter(logicalName," ");
   PrintLongInteger(logicalName,(long) hack->pattern2);
   PrintRouter(logicalName," ");
   PrintLongInteger(logicalName,(long) hack->slot2);
   PrintRouter(logicalName,")");
#else
#if MAC_MPW || MAC_MCW
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/***************************************/
/* PrintFactJNCompVars2: Print routine */
/*   for the FactJNCompVars2 function. */
/***************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle void PrintFactJNCompVars2(
  char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   struct factCompVarsJN2Call *hack;

   hack = (struct factCompVarsJN2Call *) ValueToBitMap(theValue);
   PrintRouter(logicalName,"(fact-jn-cmp-vars2 ");
   if (hack->pass) PrintRouter(logicalName,"p ");
   else PrintRouter(logicalName,"n ");

   PrintRouter(logicalName,"s");
   PrintLongInteger(logicalName,(long) hack->slot1);
   PrintRouter(logicalName," ");

   if (hack->fromBeginning1) PrintRouter(logicalName,"b ");
   else PrintRouter(logicalName,"e ");

   PrintRouter(logicalName,"f");
   PrintLongInteger(logicalName,(long) hack->offset1);
   PrintRouter(logicalName," ");

   PrintRouter(logicalName,"p");
   PrintLongInteger(logicalName,(long) hack->pattern2);
   PrintRouter(logicalName," ");

   PrintRouter(logicalName,"s");
   PrintLongInteger(logicalName,(long) hack->slot2);
   PrintRouter(logicalName," ");

   if (hack->fromBeginning2) PrintRouter(logicalName,"b ");
   else PrintRouter(logicalName,"e ");

   PrintRouter(logicalName,"f");
   PrintLongInteger(logicalName,(long) hack->offset2);
   PrintRouter(logicalName,")");
#else
#if MAC_MPW || MAC_MCW
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/***************************************/
/* PrintFactPNCompVars1: Print routine */
/*   for the FactPNCompVars1 function. */
/***************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle void PrintFactPNCompVars1(
  char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   struct factCompVarsPN1Call *hack;

   hack = (struct factCompVarsPN1Call *) ValueToBitMap(theValue);
   PrintRouter(logicalName,"(fact-pn-cmp-vars ");
   if (hack->pass) PrintRouter(logicalName,"p ");
   else PrintRouter(logicalName,"n ");
   PrintLongInteger(logicalName,(long) hack->field1);
   PrintRouter(logicalName," ");
   PrintLongInteger(logicalName,(long) hack->field2);
   PrintRouter(logicalName,")");
#else
#if MAC_MPW || MAC_MCW
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/**************************************/
/* PrintFactSlotLength: Print routine */
/*   for the FactSlotLength function. */
/**************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle void PrintFactSlotLength(
  char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   struct factCheckLengthPNCall *hack;

   hack = (struct factCheckLengthPNCall *) ValueToBitMap(theValue);

   PrintRouter(logicalName,"(slot-length ");
   PrintLongInteger(logicalName,(long) hack->whichSlot);
   PrintRouter(logicalName," ");
   if (hack->exactly) PrintRouter(logicalName,"= ");
   else PrintRouter(logicalName,">= ");
   PrintLongInteger(logicalName,(long) hack->minLength);
   PrintRouter(logicalName,")");
#else
#if MAC_MPW || MAC_MCW
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/*************************************/
/* PrintFactJNGetVar1: Print routine */
/*   for the FactJNGetvar1 function. */
/*************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle void PrintFactJNGetVar1(
  char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   struct factGetVarJN1Call *hack;

   hack = (struct factGetVarJN1Call *) ValueToBitMap(theValue);
   PrintRouter(logicalName,"(fact-jn-getvar-1 ");
   if (hack->factAddress) PrintRouter(logicalName,"t ");
   else PrintRouter(logicalName,"f ");
   if (hack->allFields) PrintRouter(logicalName,"t ");
   else PrintRouter(logicalName,"f ");

   PrintLongInteger(logicalName,(long) hack->whichPattern);
   PrintRouter(logicalName," ");
   PrintLongInteger(logicalName,(long) hack->whichField);
   PrintRouter(logicalName," ");
   PrintLongInteger(logicalName,(long) hack->whichSlot);
   PrintRouter(logicalName,")");
#else
#if MAC_MPW || MAC_MCW
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/*************************************/
/* PrintFactJNGetVar2: Print routine */
/*   for the FactJNGetvar2 function. */
/*************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle void PrintFactJNGetVar2(
  char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   struct factGetVarJN2Call *hack;

   hack = (struct factGetVarJN2Call *) ValueToBitMap(theValue);
   PrintRouter(logicalName,"(fact-jn-getvar-2 ");

   PrintLongInteger(logicalName,(long) hack->whichPattern);
   PrintRouter(logicalName," ");
   PrintLongInteger(logicalName,(long) hack->whichSlot);
   PrintRouter(logicalName,")");
#else
#if MAC_MPW || MAC_MCW
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/*************************************/
/* PrintFactJNGetVar3: Print routine */
/*   for the FactJNGetVar3 function. */
/*************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle void PrintFactJNGetVar3(
  char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   struct factGetVarJN3Call *hack;

   hack = (struct factGetVarJN3Call *) ValueToBitMap(theValue);
   PrintRouter(logicalName,"(fact-jn-getvar-3 ");
   if (hack->fromBeginning) PrintRouter(logicalName,"t ");
   else PrintRouter(logicalName,"f ");
   if (hack->fromEnd) PrintRouter(logicalName,"t ");
   else PrintRouter(logicalName,"f ");

   PrintLongInteger(logicalName,(long) hack->beginOffset);
   PrintRouter(logicalName," ");
   PrintLongInteger(logicalName,(long) hack->endOffset);
   PrintRouter(logicalName," ");
   PrintLongInteger(logicalName,(long) hack->whichSlot);
   PrintRouter(logicalName,")");
#else
#if MAC_MPW || MAC_MCW
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/*************************************/
/* PrintFactPNGetVar1: Print routine */
/*   for the FactPNGetvar1 function. */
/*************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle void PrintFactPNGetVar1(
  char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   struct factGetVarPN1Call *hack;

   hack = (struct factGetVarPN1Call *) ValueToBitMap(theValue);
   PrintRouter(logicalName,"(fact-pn-getvar-1 ");
   if (hack->factAddress) PrintRouter(logicalName,"t ");
   else PrintRouter(logicalName,"f ");
   if (hack->allFields) PrintRouter(logicalName,"t F");
   else PrintRouter(logicalName,"f F");

   PrintLongInteger(logicalName,(long) hack->whichField);
   PrintRouter(logicalName," S");
   PrintLongInteger(logicalName,(long) hack->whichSlot);
   PrintRouter(logicalName,")");
#else
#if MAC_MPW || MAC_MCW
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/*************************************/
/* PrintFactPNGetVar2: Print routine */
/*   for the FactPNGetvar2 function. */
/*************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle void PrintFactPNGetVar2(
  char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   struct factGetVarPN2Call *hack;

   hack = (struct factGetVarPN2Call *) ValueToBitMap(theValue);;
   PrintRouter(logicalName,"(fact-pn-getvar-2 S");
   PrintLongInteger(logicalName,(long) hack->whichSlot);
   PrintRouter(logicalName,")");
#else
#if MAC_MPW || MAC_MCW
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/*************************************/
/* PrintFactPNGetVar3: Print routine */
/*   for the FactPNGetvar3 function. */
/*************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle void PrintFactPNGetVar3(
  char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   struct factGetVarPN3Call *hack;

   hack = (struct factGetVarPN3Call *) ValueToBitMap(theValue);
   PrintRouter(logicalName,"(fact-pn-getvar-3 ");

   if (hack->fromBeginning) PrintRouter(logicalName,"t ");
   else PrintRouter(logicalName,"f ");
   if (hack->fromEnd) PrintRouter(logicalName,"t B");
   else PrintRouter(logicalName,"f B");

   PrintLongInteger(logicalName,(long) hack->beginOffset);
   PrintRouter(logicalName," E");
   PrintLongInteger(logicalName,(long) hack->endOffset);
   PrintRouter(logicalName," S");
   PrintLongInteger(logicalName,(long) hack->whichSlot);
   PrintRouter(logicalName,")");
#else
#if MAC_MPW || MAC_MCW
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/***************************************/
/* PrintFactPNConstant1: Print routine */
/*   for the FactPNConstant1 function. */
/***************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle void PrintFactPNConstant1(
  char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   struct factConstantPN1Call *hack;

   hack = (struct factConstantPN1Call *) ValueToBitMap(theValue);

   PrintRouter(logicalName,"(fact-pn-constant1 ");

   PrintLongInteger(logicalName,(long) hack->whichSlot);

   if (hack->testForEquality) PrintRouter(logicalName," = ");
   else PrintRouter(logicalName," != ");

   PrintAtom(logicalName,GetFirstArgument()->type,GetFirstArgument()->value);
   PrintRouter(logicalName,")");
#else
#if MAC_MPW || MAC_MCW
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/***************************************/
/* PrintFactPNConstant2: Print routine */
/*   for the FactPNConstant2 function. */
/***************************************/
#if IBM_TBC && (! DEVELOPER)
#pragma argsused
#endif
globle void PrintFactPNConstant2(
  char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   struct factConstantPN2Call *hack;

   hack = (struct factConstantPN2Call *) ValueToBitMap(theValue);

   PrintRouter(logicalName,"(fact-pn-constant2 ");

   PrintLongInteger(logicalName,(long) hack->whichSlot);

   PrintRouter(logicalName," ");

   PrintLongInteger(logicalName,(long) hack->offset);

   if (hack->testForEquality) PrintRouter(logicalName," = ");
   else PrintRouter(logicalName," != ");

   PrintAtom(logicalName,GetFirstArgument()->type,GetFirstArgument()->value);
   PrintRouter(logicalName,")");
#else
#if MAC_MPW || MAC_MCW
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

#endif /* DEFTEMPLATE_CONSTRUCT && DEFRULE_CONSTRUCT */



   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.22  06/15/04            */
   /*                                                     */
   /*                DEFFACTS PARSER MODULE               */
   /*******************************************************/

/*************************************************************/
/* Purpose: Parses a deffacts construct.                     */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#define _DFFCTPSR_SOURCE_

#include "setup.h"

#if DEFFACTS_CONSTRUCT

#include "envrnmnt.h"
#include "memalloc.h"
#include "router.h"
#include "cstrcpsr.h"
#include "factrhs.h"
#if BLOAD || BLOAD_AND_BSAVE
#include "bload.h"
#endif
#include "dffctdef.h"
#include "dffctbsc.h"

#include "dffctpsr.h"

/************************************************************/
/* ParseDeffacts: Coordinates all actions necessary for the */
/*   addition of a deffacts construct into the current      */
/*   environment. Called when parsing a construct after the */
/*   deffacts keyword has been found.                       */
/************************************************************/
globle int ParseDeffacts(
  void *theEnv,
  EXEC_STATUS,
  char *readSource)
  {
#if (MAC_MCW || WIN_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(theEnv,execStatus,readSource)
#endif

#if (! RUN_TIME) && (! BLOAD_ONLY)
   SYMBOL_HN *deffactsName;
   struct expr *temp;
   struct deffacts *newDeffacts;
   int deffactsError;
   struct token inputToken;

   /*=========================*/
   /* Parsing initialization. */
   /*=========================*/

   deffactsError = FALSE;
   SetPPBufferStatus(theEnv,execStatus,ON);

   FlushPPBuffer(theEnv,execStatus);
   SetIndentDepth(theEnv,execStatus,3);
   SavePPBuffer(theEnv,execStatus,"(deffacts ");

   /*==========================================================*/
   /* Deffacts can not be added when a binary image is loaded. */
   /*==========================================================*/

#if BLOAD || BLOAD_AND_BSAVE
   if ((Bloaded(theEnv,execStatus) == TRUE) && (! ConstructData(theEnv,execStatus)->CheckSyntaxMode))
     {
      CannotLoadWithBloadMessage(theEnv,execStatus,"deffacts");
      return(TRUE);
     }
#endif

   /*============================*/
   /* Parse the deffacts header. */
   /*============================*/

   deffactsName = GetConstructNameAndComment(theEnv,execStatus,readSource,&inputToken,"deffacts",
                                             EnvFindDeffacts,EnvUndeffacts,"$",TRUE,
                                             TRUE,TRUE);
   if (deffactsName == NULL) { return(TRUE); }

   /*===============================================*/
   /* Parse the list of facts in the deffacts body. */
   /*===============================================*/

   temp = BuildRHSAssert(theEnv,execStatus,readSource,&inputToken,&deffactsError,FALSE,FALSE,"deffacts","assert");

   if (deffactsError == TRUE) { return(TRUE); }

   if (ExpressionContainsVariables(temp,FALSE))
     {
      LocalVariableErrorMessage(theEnv,execStatus,"a deffacts construct");
      ReturnExpression(theEnv,execStatus,temp);
      return(TRUE);
     }

   SavePPBuffer(theEnv,execStatus,"\n");

   /*==============================================*/
   /* If we're only checking syntax, don't add the */
   /* successfully parsed deffacts to the KB.      */
   /*==============================================*/

   if (ConstructData(theEnv,execStatus)->CheckSyntaxMode)
     {
      ReturnExpression(theEnv,execStatus,temp);
      return(FALSE);
     }

   /*==========================*/
   /* Create the new deffacts. */
   /*==========================*/

   ExpressionInstall(theEnv,execStatus,temp);
   newDeffacts = get_struct(theEnv,execStatus,deffacts);
   newDeffacts->header.name = deffactsName;
   IncrementSymbolCount(deffactsName);
   newDeffacts->assertList = PackExpression(theEnv,execStatus,temp);
   newDeffacts->header.whichModule = (struct defmoduleItemHeader *)
                              GetModuleItem(theEnv,execStatus,NULL,FindModuleItem(theEnv,execStatus,"deffacts")->moduleIndex);

   newDeffacts->header.next = NULL;
   newDeffacts->header.usrData = NULL;
   ReturnExpression(theEnv,execStatus,temp);

   /*=======================================================*/
   /* Save the pretty print representation of the deffacts. */
   /*=======================================================*/

   if (EnvGetConserveMemory(theEnv,execStatus) == TRUE)
     { newDeffacts->header.ppForm = NULL; }
   else
     { newDeffacts->header.ppForm = CopyPPBuffer(theEnv,execStatus); }

   /*=============================================*/
   /* Add the deffacts to the appropriate module. */
   /*=============================================*/

   AddConstructToModule(&newDeffacts->header);

#endif /* (! RUN_TIME) && (! BLOAD_ONLY) */

   /*================================================================*/
   /* Return FALSE to indicate the deffacts was successfully parsed. */
   /*================================================================*/

   return(FALSE);
  }

#endif /* DEFFACTS_CONSTRUCT */



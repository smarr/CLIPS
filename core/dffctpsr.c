   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
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
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _DFFCTPSR_SOURCE_

#include "setup.h"

#if DEFFACTS_CONSTRUCT

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
  char *readSource)
  {
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(readSource)
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
   SetPPBufferStatus(ON);

   FlushPPBuffer();
   SetIndentDepth(3);
   SavePPBuffer("(deffacts ");

   /*==========================================================*/
   /* Deffacts can not be added when a binary image is loaded. */
   /*==========================================================*/

#if BLOAD || BLOAD_AND_BSAVE
   if ((Bloaded() == TRUE) && (! CheckSyntaxMode))
     {
      CannotLoadWithBloadMessage("deffacts");
      return(TRUE);
     }
#endif

   /*============================*/
   /* Parse the deffacts header. */
   /*============================*/

   deffactsName = GetConstructNameAndComment(readSource,&inputToken,"deffacts",
                                             FindDeffacts,Undeffacts,"$",TRUE,
                                             TRUE,TRUE);
   if (deffactsName == NULL) { return(TRUE); }

   /*===============================================*/
   /* Parse the list of facts in the deffacts body. */
   /*===============================================*/

   temp = BuildRHSAssert(readSource,&inputToken,&deffactsError,FALSE,FALSE,"deffacts");

   if (deffactsError == TRUE) { return(TRUE); }

   if (ExpressionContainsVariables(temp,FALSE))
     {
      LocalVariableErrorMessage("a deffacts construct");
      ReturnExpression(temp);
      return(TRUE);
     }

   SavePPBuffer("\n");

   /*==============================================*/
   /* If we're only checking syntax, don't add the */
   /* successfully parsed deffacts to the KB.      */
   /*==============================================*/

   if (CheckSyntaxMode)
     {
      ReturnExpression(temp);
      return(FALSE);
     }

   /*==========================*/
   /* Create the new deffacts. */
   /*==========================*/

   ExpressionInstall(temp);
   newDeffacts = get_struct(deffacts);
   newDeffacts->header.name = deffactsName;
   IncrementSymbolCount(deffactsName);
   newDeffacts->assertList = PackExpression(temp);
   newDeffacts->header.whichModule = (struct defmoduleItemHeader *)
                              GetModuleItem(NULL,FindModuleItem("deffacts")->moduleIndex);

   newDeffacts->header.next = NULL;
   newDeffacts->header.usrData = NULL;
   ReturnExpression(temp);

   /*=======================================================*/
   /* Save the pretty print representation of the deffacts. */
   /*=======================================================*/

   if (GetConserveMemory() == TRUE)
     { newDeffacts->header.ppForm = NULL; }
   else
     { newDeffacts->header.ppForm = CopyPPBuffer(); }

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



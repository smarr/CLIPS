   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*              DEFTEMPLATE PARSER MODULE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Parses the deftemplate construct.                */
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

#define _TMPLTPSR_SOURCE_

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include "constant.h"
#include "memalloc.h"
#include "symbol.h"
#include "scanner.h"
#include "exprnpsr.h"
#include "router.h"
#include "constrct.h"
#include "factmngr.h"
#include "cstrnchk.h"
#include "cstrnpsr.h"
#include "cstrcpsr.h"
#if BLOAD || BLOAD_AND_BSAVE
#include "bload.h"
#endif
#include "default.h"
#include "pattern.h"
#include "watch.h"
#include "cstrnutl.h"

#include "tmpltdef.h"
#include "tmpltbsc.h"

#include "tmpltpsr.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if (! RUN_TIME) && (! BLOAD_ONLY)
   static struct templateSlot    *SlotDeclarations(char *,struct token *);
   static struct templateSlot    *ParseSlot(char *,struct token *,struct templateSlot *);
   static struct templateSlot    *DefinedSlots(char *,SYMBOL_HN *,int,struct token *);
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

#if (! RUN_TIME) && (! BLOAD_ONLY)
   Thread static int              DeftemplateError;
#endif

/*******************************************************/
/* ParseDeftemplate: Parses the deftemplate construct. */
/*******************************************************/
globle int ParseDeftemplate(
  char *readSource)
  {
#if (MAC_MPW || MAC_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(readSource)
#endif

#if (! RUN_TIME) && (! BLOAD_ONLY)
   SYMBOL_HN *deftemplateName;
   struct deftemplate *newDeftemplate;
   struct templateSlot *slots;
   struct token inputToken;

   /*================================================*/
   /* Initialize pretty print and error information. */
   /*================================================*/

   DeftemplateError = FALSE;
   SetPPBufferStatus(ON);
   FlushPPBuffer();
   SavePPBuffer("(deftemplate ");

   /*==============================================================*/
   /* Deftemplates can not be added when a binary image is loaded. */
   /*==============================================================*/

#if BLOAD || BLOAD_AND_BSAVE
   if ((Bloaded() == TRUE) && (! CheckSyntaxMode))
     {
      CannotLoadWithBloadMessage("deftemplate");
      return(TRUE);
     }
#endif

   /*=======================================================*/
   /* Parse the name and comment fields of the deftemplate. */
   /*=======================================================*/

#if DEBUGGING_FUNCTIONS
   DeletedTemplateDebugFlags = 0;
#endif

   deftemplateName = GetConstructNameAndComment(readSource,&inputToken,"deftemplate",
                                                FindDeftemplate,Undeftemplate,"%",
                                                TRUE,TRUE,TRUE);
   if (deftemplateName == NULL) return(TRUE);

   if (ReservedPatternSymbol(ValueToString(deftemplateName),"deftemplate"))
     {
      ReservedPatternSymbolErrorMsg(ValueToString(deftemplateName),"a deftemplate name");
      return(TRUE);
     }

   /*===========================================*/
   /* Parse the slot fields of the deftemplate. */
   /*===========================================*/

   slots = SlotDeclarations(readSource,&inputToken);
   if (DeftemplateError == TRUE) return(TRUE);

   /*==============================================*/
   /* If we're only checking syntax, don't add the */
   /* successfully parsed deftemplate to the KB.   */
   /*==============================================*/

   if (CheckSyntaxMode)
     {
      ReturnSlots(slots);
      return(FALSE);
     }

   /*=====================================*/
   /* Create a new deftemplate structure. */
   /*=====================================*/

   newDeftemplate = get_struct(deftemplate);
   newDeftemplate->header.name =  deftemplateName;
   newDeftemplate->header.next = NULL;
   newDeftemplate->header.usrData = NULL;
   newDeftemplate->slotList = slots;
   newDeftemplate->implied = FALSE;
   newDeftemplate->numberOfSlots = 0;
   newDeftemplate->busyCount = 0;
   newDeftemplate->watch = 0;
   newDeftemplate->inScope = TRUE;
   newDeftemplate->patternNetwork = NULL;
   newDeftemplate->header.whichModule = (struct defmoduleItemHeader *)
                                        GetModuleItem(NULL,DeftemplateModuleIndex);

   /*================================*/
   /* Determine the number of slots. */
   /*================================*/

   while (slots != NULL)
     {
      newDeftemplate->numberOfSlots++;
      slots = slots->next;
     }

   /*====================================*/
   /* Store pretty print representation. */
   /*====================================*/

   if (GetConserveMemory() == TRUE)
     { newDeftemplate->header.ppForm = NULL; }
   else
     { newDeftemplate->header.ppForm = CopyPPBuffer(); }

   /*=======================================================================*/
   /* If a template is redefined, then we want to restore its watch status. */
   /*=======================================================================*/

#if DEBUGGING_FUNCTIONS
   if ((BitwiseTest(DeletedTemplateDebugFlags,0)) || GetWatchItem("facts"))
     { SetDeftemplateWatch(ON,(void *) newDeftemplate); }
#endif

   /*==============================================*/
   /* Add deftemplate to the list of deftemplates. */
   /*==============================================*/

   AddConstructToModule(&newDeftemplate->header);

   InstallDeftemplate(newDeftemplate);

#endif

   return(FALSE);
  }

#if (! RUN_TIME) && (! BLOAD_ONLY)

/**************************************************************/
/* InstallDeftemplate: Increments all occurrences in the hash */
/*   table of symbols found in an deftemplate and adds it to  */
/*   the hash table.                                          */
/**************************************************************/
globle void InstallDeftemplate(
  struct deftemplate *theDeftemplate)
  {
   struct templateSlot *slotPtr;
   struct expr *tempExpr;

   IncrementSymbolCount(theDeftemplate->header.name);

   for (slotPtr = theDeftemplate->slotList;
        slotPtr != NULL;
        slotPtr = slotPtr->next)
     {
      IncrementSymbolCount(slotPtr->slotName);
      tempExpr = AddHashedExpression(slotPtr->defaultList);
      ReturnExpression(slotPtr->defaultList);
      slotPtr->defaultList = tempExpr;
      slotPtr->constraints = AddConstraint(slotPtr->constraints);
     }
  }

/********************************************************************/
/* SlotDeclarations: Parses the slot declarations of a deftemplate. */
/********************************************************************/
static struct templateSlot *SlotDeclarations(
  char *readSource,
  struct token *inputToken)
  {
   struct templateSlot *newSlot, *slotList = NULL, *lastSlot = NULL;
   struct templateSlot *multiSlot = NULL;

   while (inputToken->type != RPAREN)
     {
      /*====================================================*/
      /* Slots begin with a '(' followed by a slot keyword. */
      /*====================================================*/

      if (inputToken->type != LPAREN)
        {
         SyntaxErrorMessage("deftemplate");
         ReturnSlots(slotList);
         ReturnSlots(multiSlot);
         DeftemplateError = TRUE;
         return(NULL);
        }

      GetToken(readSource,inputToken);
      if (inputToken->type != SYMBOL)
        {
         SyntaxErrorMessage("deftemplate");
         ReturnSlots(slotList);
         ReturnSlots(multiSlot);
         DeftemplateError = TRUE;
         return(NULL);
        }

      /*=================*/
      /* Parse the slot. */
      /*=================*/

      newSlot = ParseSlot(readSource,inputToken,slotList);
      if (DeftemplateError == TRUE)
        {
         ReturnSlots(newSlot);
         ReturnSlots(slotList);
         ReturnSlots(multiSlot);
         return(NULL);
        }

      /*===========================================*/
      /* Attach the new slot to the list of slots. */
      /*===========================================*/

      if (newSlot != NULL)
        {
         if (lastSlot == NULL)
           { slotList = newSlot; }
         else
           { lastSlot->next = newSlot; }
         lastSlot = newSlot;
        }

      /*================================*/
      /* Check for closing parenthesis. */
      /*================================*/

      GetToken(readSource,inputToken);
      if (inputToken->type != RPAREN)
        {
         PPBackup();
         SavePPBuffer("\n   ");
         SavePPBuffer(inputToken->printForm);
        }
     }

  SavePPBuffer("\n");

  /*=======================*/
  /* Return the slot list. */
  /*=======================*/

  return(slotList);
 }

/*****************************************************/
/* ParseSlot: Parses a single slot of a deftemplate. */
/*****************************************************/
static struct templateSlot *ParseSlot(
  char *readSource,
  struct token *inputToken,
  struct templateSlot *slotList)
  {
   int parsingMultislot;
   SYMBOL_HN *slotName;
   struct templateSlot *newSlot;
   int rv;

   /*=====================================================*/
   /* Slots must  begin with keyword field or multifield. */
   /*=====================================================*/

   if ((strcmp(ValueToString(inputToken->value),"field") != 0) &&
       (strcmp(ValueToString(inputToken->value),"multifield") != 0) &&
       (strcmp(ValueToString(inputToken->value),"slot") != 0) &&
       (strcmp(ValueToString(inputToken->value),"multislot") != 0))
     {
      SyntaxErrorMessage("deftemplate");
      DeftemplateError = TRUE;
      return(NULL);
     }

   /*===============================================*/
   /* Determine if multifield slot is being parsed. */
   /*===============================================*/

   if ((strcmp(ValueToString(inputToken->value),"multifield") == 0) ||
       (strcmp(ValueToString(inputToken->value),"multislot") == 0))
     { parsingMultislot = TRUE; }
   else
     { parsingMultislot = FALSE; }

   /*========================================*/
   /* The name of the slot must be a symbol. */
   /*========================================*/

   SavePPBuffer(" ");
   GetToken(readSource,inputToken);
   if (inputToken->type != SYMBOL)
     {
      SyntaxErrorMessage("deftemplate");
      DeftemplateError = TRUE;
      return(NULL);
     }

   slotName = (SYMBOL_HN *) inputToken->value;

   /*================================================*/
   /* Determine if the slot has already been parsed. */
   /*================================================*/

   while (slotList != NULL)
     {
      if (slotList->slotName == slotName)
        {
         AlreadyParsedErrorMessage("slot ",ValueToString(slotList->slotName));
         DeftemplateError = TRUE;
         return(NULL);
        }

      slotList = slotList->next;
     }

   /*===================================*/
   /* Parse the attributes of the slot. */
   /*===================================*/

   newSlot = DefinedSlots(readSource,slotName,parsingMultislot,inputToken);
   if (newSlot == NULL)
     {
      DeftemplateError = TRUE;
      return(NULL);
     }

   /*=================================*/
   /* Check for slot conflict errors. */
   /*=================================*/

   if (CheckConstraintParseConflicts(newSlot->constraints) == FALSE)
     {
      ReturnSlots(newSlot);
      DeftemplateError = TRUE;
      return(NULL);
     }

   if ((newSlot->defaultPresent) || (newSlot->defaultDynamic))
     { rv = ConstraintCheckExpressionChain(newSlot->defaultList,newSlot->constraints); }
   else
     { rv = NO_VIOLATION; }

   if ((rv != NO_VIOLATION) && GetStaticConstraintChecking())
     {
      char *temp;
      if (newSlot->defaultDynamic) temp = "the default-dynamic attribute";
      else temp = "the default attribute";
      ConstraintViolationErrorMessage("An expression",temp,FALSE,0,
                                      newSlot->slotName,0,rv,newSlot->constraints,TRUE);
      ReturnSlots(newSlot);
      DeftemplateError = TRUE;
      return(NULL);
     }

   /*==================*/
   /* Return the slot. */
   /*==================*/

   return(newSlot);
  }

/**************************************************************/
/* DefinedSlots: Parses a field or multifield slot attribute. */
/**************************************************************/
static struct templateSlot *DefinedSlots(
  char *readSource,
  SYMBOL_HN *slotName,
  int multifieldSlot,
  struct token *inputToken)
  {
   struct templateSlot *newSlot;
   struct expr *defaultList;
   int defaultFound = FALSE;
   int noneSpecified, deriveSpecified;
   CONSTRAINT_PARSE_RECORD parsedConstraints;

   /*===========================*/
   /* Build the slot container. */
   /*===========================*/

   newSlot = get_struct(templateSlot);
   newSlot->slotName = slotName;
   newSlot->defaultList = NULL;
   newSlot->constraints = GetConstraintRecord();
   if (multifieldSlot)
     { newSlot->constraints->multifieldsAllowed = TRUE; }
   newSlot->multislot = multifieldSlot;
   newSlot->noDefault = FALSE;
   newSlot->defaultPresent = FALSE;
   newSlot->defaultDynamic = FALSE;
   newSlot->next = NULL;

   /*========================================*/
   /* Parse the primitive slot if it exists. */
   /*========================================*/

   InitializeConstraintParseRecord(&parsedConstraints);
   GetToken(readSource,inputToken);

   while (inputToken->type != RPAREN)
     {
      PPBackup();
      SavePPBuffer(" ");
      SavePPBuffer(inputToken->printForm);

      /*================================================*/
      /* Slot attributes begin with a left parenthesis. */
      /*================================================*/

      if (inputToken->type != LPAREN)
        {
         SyntaxErrorMessage("deftemplate");
         ReturnSlots(newSlot);
         DeftemplateError = TRUE;
         return(NULL);
        }

      /*=============================================*/
      /* The name of the attribute must be a symbol. */
      /*=============================================*/

      GetToken(readSource,inputToken);
      if (inputToken->type != SYMBOL)
        {
         SyntaxErrorMessage("deftemplate");
         ReturnSlots(newSlot);
         DeftemplateError = TRUE;
         return(NULL);
        }

      /*================================================================*/
      /* Determine if the attribute is one of the standard constraints. */
      /*================================================================*/

      if (StandardConstraint(ValueToString(inputToken->value)))
        {
         if (ParseStandardConstraint(readSource,(ValueToString(inputToken->value)),
                                     newSlot->constraints,&parsedConstraints,
                                     multifieldSlot) == FALSE)
           {
            DeftemplateError = TRUE;
            ReturnSlots(newSlot);
            return(NULL);
           }
        }

      /*=================================================*/
      /* else if the attribute is the default attribute, */
      /* then get the default list for this slot.        */
      /*=================================================*/

      else if ((strcmp(ValueToString(inputToken->value),"default") == 0) ||
               (strcmp(ValueToString(inputToken->value),"default-dynamic") == 0))
        {
         /*======================================================*/
         /* Check to see if the default has already been parsed. */
         /*======================================================*/

         if (defaultFound)
           {
            AlreadyParsedErrorMessage("default attribute",NULL);
            DeftemplateError = TRUE;
            ReturnSlots(newSlot);
            return(NULL);
           }

         newSlot->noDefault = FALSE;

         /*=====================================================*/
         /* Determine whether the default is dynamic or static. */
         /*=====================================================*/

         if (strcmp(ValueToString(inputToken->value),"default") == 0)
           {
            newSlot->defaultPresent = TRUE;
            newSlot->defaultDynamic = FALSE;
           }
         else
           {
            newSlot->defaultPresent = FALSE;
            newSlot->defaultDynamic = TRUE;
           }

         /*===================================*/
         /* Parse the list of default values. */
         /*===================================*/

         defaultList = ParseDefault(readSource,multifieldSlot,(int) newSlot->defaultDynamic,
                                  TRUE,&noneSpecified,&deriveSpecified,&DeftemplateError);
         if (DeftemplateError == TRUE)
           {
            ReturnSlots(newSlot);
            return(NULL);
           }

         /*==================================*/
         /* Store the default with the slot. */
         /*==================================*/

         defaultFound = TRUE;
         if (deriveSpecified) newSlot->defaultPresent = FALSE;
         else if (noneSpecified)
           {
            newSlot->noDefault = TRUE;
            newSlot->defaultPresent = FALSE;
           }
         newSlot->defaultList = defaultList;
        }

      /*============================================*/
      /* Otherwise the attribute is an invalid one. */
      /*============================================*/

      else
        {
         SyntaxErrorMessage("slot attributes");
         ReturnSlots(newSlot);
         DeftemplateError = TRUE;
         return(NULL);
        }

      /*===================================*/
      /* Begin parsing the next attribute. */
      /*===================================*/

      GetToken(readSource,inputToken);
     }

   /*============================*/
   /* Return the attribute list. */
   /*============================*/

   return(newSlot);
  }

#endif /* (! RUN_TIME) && (! BLOAD_ONLY) */

#endif /* DEFTEMPLATE_CONSTRUCT */

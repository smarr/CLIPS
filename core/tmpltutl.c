   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*            DEFTEMPLATE UTILITIES MODULE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides utility routines for deftemplates.      */
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

#define  _TMPLTUTL_SOURCE_

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT

#include <stdio.h>

#define _STDIO_INCLUDED_

#include <string.h>

#include "extnfunc.h"
#include "memalloc.h"
#include "constrct.h"
#include "router.h"
#include "argacces.h"
#include "cstrnchk.h"
#include "tmpltfun.h"
#include "tmpltpsr.h"
#include "modulutl.h"
#include "watch.h"
#include "tmpltbsc.h"
#include "tmpltdef.h"

#include "tmpltutl.h"

/********************************************************/
/* InvalidDeftemplateSlotMessage: Generic error message */
/*   for use when a specified slot name isn't defined   */
/*   in its corresponding deftemplate.                  */
/********************************************************/
globle void InvalidDeftemplateSlotMessage(
  char *slotName,
  char *deftemplateName)
  {
   PrintErrorID("TMPLTDEF",1,TRUE);
   PrintRouter(WERROR,"Invalid slot ");
   PrintRouter(WERROR,slotName);
   PrintRouter(WERROR," not defined in corresponding deftemplate ");
   PrintRouter(WERROR,deftemplateName);
   PrintRouter(WERROR,".\n");
  }

/**********************************************************/
/* SingleFieldSlotCardinalityError: Generic error message */
/*   used when an attempt is made to placed a multifield  */
/*   value into a single field slot.                      */
/**********************************************************/
globle void SingleFieldSlotCardinalityError(
  char *slotName)
  {
   PrintErrorID("TMPLTDEF",2,TRUE);
   PrintRouter(WERROR,"The single field slot ");
   PrintRouter(WERROR,slotName);
   PrintRouter(WERROR," can only contain a single field value.\n");
  }

/**********************************************************************/
/* MultiIntoSingleFieldSlotError: Determines if a multifield value is */
/*   being placed into a single field slot of a deftemplate fact.     */
/**********************************************************************/
globle void MultiIntoSingleFieldSlotError(
  struct templateSlot *theSlot,
  struct deftemplate *theDeftemplate)
  {
   PrintErrorID("TMPLTFUN",2,TRUE);
   PrintRouter(WERROR,"Attempted to assert a multifield value \n");
   PrintRouter(WERROR,"into the single field slot ");
   if (theSlot != NULL) PrintRouter(WERROR,theSlot->slotName->contents);
   else PrintRouter(WERROR,"<<unknown>>");
   PrintRouter(WERROR," of deftemplate ");
   if (theDeftemplate != NULL) PrintRouter(WERROR,theDeftemplate->header.name->contents);
   else PrintRouter(WERROR,"<<unknown>>");
   PrintRouter(WERROR,".\n");

   SetEvaluationError(TRUE);
  }

/**************************************************************/
/* CheckTemplateFact: Checks a fact to see if it violates any */
/*   deftemplate type, allowed-..., or range specifications.  */
/**************************************************************/
globle void CheckTemplateFact(
  struct fact *theFact)
  {
   struct field *sublist;
   int i;
   struct deftemplate *theDeftemplate;
   struct templateSlot *slotPtr;
   DATA_OBJECT theData;
   char thePlace[20];
   int rv;

   if (! GetDynamicConstraintChecking()) return;

   sublist = theFact->theProposition.theFields;

   /*========================================================*/
   /* If the deftemplate corresponding to the first field of */
   /* of the fact cannot be found, then the fact cannot be   */
   /* checked against the deftemplate format.                */
   /*========================================================*/

   theDeftemplate = theFact->whichDeftemplate;
   if (theDeftemplate == NULL) return;
   if (theDeftemplate->implied) return;

   /*=============================================*/
   /* Check each of the slots of the deftemplate. */
   /*=============================================*/

   i = 0;
   for (slotPtr = theDeftemplate->slotList;
        slotPtr != NULL;
        slotPtr = slotPtr->next)
     {
      /*================================================*/
      /* Store the slot value in the appropriate format */
      /* for a call to the constraint checking routine. */
      /*================================================*/

      if (slotPtr->multislot == FALSE)
        {
         theData.type = sublist[i].type;
         theData.value = sublist[i].value;
         i++;
        }
      else
        {
         theData.type = MULTIFIELD;
         theData.value = (void *) sublist[i].value;
         theData.begin = 0;
         theData.end = ((struct multifield *) sublist[i].value)->multifieldLength-1;
         i++;
        }

      /*=============================================*/
      /* Call the constraint checking routine to see */
      /* if a constraint violation occurred.         */
      /*=============================================*/

      rv = ConstraintCheckDataObject(&theData,slotPtr->constraints);
      if (rv != NO_VIOLATION)
        {
         sprintf(thePlace,"fact f-%-5ld ",theFact->factIndex);

         PrintErrorID("CSTRNCHK",1,TRUE);
         PrintRouter(WERROR,"Slot value ");
         PrintDataObject(WERROR,&theData);
         PrintRouter(WERROR," ");
         ConstraintViolationErrorMessage(NULL,thePlace,FALSE,0,slotPtr->slotName,
                                         0,rv,slotPtr->constraints,TRUE);
         SetHaltExecution(TRUE);
         return;
        }
     }

   return;
  }

/***********************************************************************/
/* CheckRHSSlotTypes: Checks the validity of a change to a slot as the */
/*   result of an assert, modify, or duplicate command. This checking  */
/*   is performed statically (i.e. when the command is being parsed).  */
/***********************************************************************/
globle BOOLEAN CheckRHSSlotTypes(
  struct expr *rhsSlots,
  struct templateSlot *slotPtr,
  char *thePlace)
  {
   int rv;
   char *theName;

   if (GetStaticConstraintChecking() == FALSE) return(TRUE);
      rv = ConstraintCheckExpressionChain(rhsSlots,slotPtr->constraints);
      if (rv != NO_VIOLATION)
        {
         if (rv != CARDINALITY_VIOLATION) theName = "A literal slot value";
         else theName = "Literal slot values";
         ConstraintViolationErrorMessage(theName,thePlace,TRUE,0,
                                         slotPtr->slotName,0,rv,slotPtr->constraints,TRUE);
         return(0);
        }

   return(1);
  }

/*********************************************************/
/* GetNthSlot: Given a deftemplate and an integer index, */
/*   returns the nth slot of a deftemplate.              */
/*********************************************************/
globle struct templateSlot *GetNthSlot(
  struct deftemplate *theDeftemplate,
  int position)
  {
   struct templateSlot *slotPtr;
   int i = 0;

   slotPtr = theDeftemplate->slotList;
   while (slotPtr != NULL)
     {
      if (i == position) return(slotPtr);
      slotPtr = slotPtr->next;
      i++;
     }

   return(NULL);
  }

/*******************************************************/
/* FindSlotPosition: Finds the position of a specified */
/*   slot in a deftemplate structure.                  */
/*******************************************************/
globle int FindSlotPosition(
  struct deftemplate *theDeftemplate,
  SYMBOL_HN *name)
  {
   struct templateSlot *slotPtr;
   int position;

   for (slotPtr = theDeftemplate->slotList, position = 1;
        slotPtr != NULL;
        slotPtr = slotPtr->next, position++)
     {
      if (slotPtr->slotName == name)
        { return(position); }
     }

   return(0);
  }

/*******************************************************************/
/* PrintTemplateFact: Prints a fact using the deftemplate format.  */
/*   Returns TRUE if the fact was printed using this format, */
/*   otherwise FALSE.                                        */
/*******************************************************************/
globle void PrintTemplateFact(
  char *logicalName,
  struct fact *theFact)
  {
   struct field *sublist;
   int i;
   struct deftemplate *theDeftemplate;
   struct templateSlot *slotPtr;

   /*==============================*/
   /* Initialize some information. */
   /*==============================*/

   theDeftemplate = theFact->whichDeftemplate;
   sublist = theFact->theProposition.theFields;

   /*=============================================*/
   /* Print the relation name of the deftemplate. */
   /*=============================================*/

   PrintRouter(logicalName,"(");
   PrintRouter(logicalName,theDeftemplate->header.name->contents);
   if (theDeftemplate->slotList != NULL) PrintRouter(logicalName," ");

   /*===================================================*/
   /* Print each of the field slots of the deftemplate. */
   /*===================================================*/

   slotPtr = theDeftemplate->slotList;

   i = 0;
   while (slotPtr != NULL)
     {
      /*===========================================*/
      /* Print the closing parenthesis of the slot */
      /* and the slot name.                        */
      /*===========================================*/

      PrintRouter(logicalName,"(");
      PrintRouter(logicalName,slotPtr->slotName->contents);

      /*======================================================*/
      /* Print the value of the slot for a single field slot. */
      /*======================================================*/

      if (slotPtr->multislot == FALSE)
        {
         PrintRouter(logicalName," ");
         PrintAtom(logicalName,sublist[i].type,sublist[i].value);
        }

      /*==========================================================*/
      /* Else print the value of the slot for a multi field slot. */
      /*==========================================================*/

      else
        {
         struct multifield *theSegment;

         theSegment = (struct multifield *) sublist[i].value;
         if (theSegment->multifieldLength > 0)
           {
            PrintRouter(logicalName," ");
            PrintMultifield(logicalName,(struct multifield *) sublist[i].value,
                            0,theSegment->multifieldLength-1,FALSE);
           }
        }

      /*============================================*/
      /* Print the closing parenthesis of the slot. */
      /*============================================*/

      i++;
      PrintRouter(logicalName,")");
      slotPtr = slotPtr->next;
      if (slotPtr != NULL) PrintRouter(logicalName," ");
     }

   PrintRouter(logicalName,")");
  }

/***************************************************************************/
/* UpdateDeftemplateScope: Updates the scope flag of all the deftemplates. */
/***************************************************************************/
globle void UpdateDeftemplateScope()
  {
   struct deftemplate *theDeftemplate;
   int moduleCount;
   struct defmodule *theModule;
   struct defmoduleItemHeader *theItem;

   /*==================================*/
   /* Loop through all of the modules. */
   /*==================================*/

   for (theModule = (struct defmodule *) GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      /*======================================================*/
      /* Loop through each of the deftemplates in the module. */
      /*======================================================*/

      theItem = (struct defmoduleItemHeader *)
                GetModuleItem(theModule,DeftemplateModuleIndex);

      for (theDeftemplate = (struct deftemplate *) theItem->firstItem;
           theDeftemplate != NULL ;
           theDeftemplate = (struct deftemplate *) GetNextDeftemplate(theDeftemplate))
        {
         /*=======================================*/
         /* If the deftemplate can be seen by the */
         /* current module, then it is in scope.  */
         /*=======================================*/

         if (FindImportedConstruct("deftemplate",theModule,
                                   ValueToString(theDeftemplate->header.name),
                                   &moduleCount,TRUE,NULL) != NULL)
           { theDeftemplate->inScope = TRUE; }
         else
           { theDeftemplate->inScope = FALSE; }
        }
     }
  }

/****************************************************************/
/* FindSlot: Finds a specified slot in a deftemplate structure. */
/****************************************************************/
globle struct templateSlot *FindSlot(
  struct deftemplate *theDeftemplate,
  SYMBOL_HN *name,
  int *whichOne)
  {
   struct templateSlot *slotPtr;

   *whichOne = 1;
   slotPtr = theDeftemplate->slotList;
   while (slotPtr != NULL)
     {
      if (slotPtr->slotName == name)
        { return(slotPtr); }
      (*whichOne)++;
      slotPtr = slotPtr->next;
     }

   *whichOne = -1;
   return(NULL);
  }

#if (! RUN_TIME) && (! BLOAD_ONLY)

/************************************************************/
/* CreateImpliedDeftemplate: Creates an implied deftemplate */
/*   and adds it to the list of deftemplates.               */
/************************************************************/
globle struct deftemplate *CreateImpliedDeftemplate(
  SYMBOL_HN *deftemplateName,
  int setFlag)
  {
   struct deftemplate *newDeftemplate;

   newDeftemplate = get_struct(deftemplate);
   newDeftemplate->header.name = deftemplateName;
   newDeftemplate->header.ppForm = NULL;
   newDeftemplate->header.usrData = NULL;
   newDeftemplate->slotList = NULL;
   newDeftemplate->implied = setFlag;
   newDeftemplate->numberOfSlots = 0;
   newDeftemplate->inScope = 1;
   newDeftemplate->patternNetwork = NULL;
   newDeftemplate->busyCount = 0;
   newDeftemplate->watch = FALSE;
   newDeftemplate->header.next = NULL;

#if DEBUGGING_FUNCTIONS
   if (GetWatchItem("facts"))
     { SetDeftemplateWatch(ON,(void *) newDeftemplate); }
#endif

   newDeftemplate->header.whichModule = (struct defmoduleItemHeader *)
                                        GetModuleItem(NULL,DeftemplateModuleIndex);

   AddConstructToModule(&newDeftemplate->header);
   InstallDeftemplate(newDeftemplate);

   return(newDeftemplate);
  }

#endif

#endif /* DEFTEMPLATE_CONSTRUCT */

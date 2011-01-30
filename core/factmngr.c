   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  09/13/99            */
   /*                                                     */
   /*                 FACT MANAGER MODULE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides core routines for maintaining the fact  */
/*   list including assert/retract operations, data          */
/*   structure creation/deletion, printing, slot access,     */
/*   and other utility functions.                            */
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
/*      MDT          | 13-Sep-1999 |        DR833            */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/


#define _FACTMNGR_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT && DEFRULE_CONSTRUCT

#include "constant.h"
#include "symbol.h"
#include "memalloc.h"
#include "exprnpsr.h"
#include "argacces.h"
#include "scanner.h"
#include "router.h"
#include "strngrtr.h"
#include "match.h"
#include "factbld.h"
#include "reteutil.h"
#include "retract.h"
#include "filecom.h"
#include "factfun.h"
#include "constrct.h"
#include "factrhs.h"
#include "factmch.h"
#include "watch.h"
#include "utility.h"
#include "factbin.h"
#include "factmngr.h"
#include "facthsh.h"
#include "default.h"
#include "commline.h"

#include "engine.h"
#include "lgcldpnd.h"
#include "drive.h"
#include "ruledlt.h"

#include "tmpltbsc.h"
#include "tmpltdef.h"
#include "tmpltutl.h"
#include "tmpltfun.h"

/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern void                    FactCommandDefinitions(void);
   extern void                    FactPatternsCompilerSetup(void);

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    ResetFacts(void);
   static int                     ClearFactsReady(void);
   static void                    RemoveGarbageFacts(void);

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   
   Thread globle int              ChangeToFactList = FALSE;
#if THREAD_STORAGE
   Thread globle struct fact      DummyFact = { { NULL }, NULL, NULL, -1L, 0, 1,
                                                        NULL, NULL, { 1, 0, 0 } };
#else
   globle struct fact      DummyFact = { { &FactInfo }, NULL, NULL, -1L, 0, 1,
                                                        NULL, NULL, { 1, 0, 0 } };
#endif

#if DEBUGGING_FUNCTIONS
   Thread globle int              WatchFacts = OFF;
#endif


/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static struct fact            *GarbageFacts = NULL;
   Thread static struct fact            *LastFact = NULL;
   Thread static struct fact            *FactList = NULL;
   Thread static long int                NextFactIndex = 0L;
   Thread static long int                NumberOfFacts = 0;

/**************************************************************/
/* InitializeFacts: Initializes the fact data representation. */
/*   Facts are only available when both the defrule and       */
/*   deftemplate constructs are available.                    */
/**************************************************************/
globle void InitializeFacts()
  {

   /*===========================*/
   /* Initialize the dummy fact */
   /*===========================*/
   DummyFact.factHeader.theInfo = &FactInfo;
   
   /*=========================================*/
   /* Initialize the fact hash table (used to */
   /* quickly determine if a fact exists).    */
   /*=========================================*/

   InitializeFactHashTable();

   /*============================================*/
   /* Initialize the fact callback functions for */
   /* use with the reset and clear commands.     */
   /*============================================*/

   AddResetFunction("facts",ResetFacts,60);
   AddClearReadyFunction("facts",ClearFactsReady,0);

   /*=============================*/
   /* Initialize periodic garbage */
   /* collection for facts.       */
   /*=============================*/

   AddCleanupFunction("facts",RemoveGarbageFacts,0);

   /*===================================*/
   /* Initialize fact pattern matching. */
   /*===================================*/

   InitializeFactPatterns();

   /*==================================*/
   /* Initialize the facts keyword for */
   /* use with the watch command.      */
   /*==================================*/

#if DEBUGGING_FUNCTIONS
   AddWatchItem("facts",0,&WatchFacts,80,DeftemplateWatchAccess,DeftemplateWatchPrint);
#endif

   /*=========================================*/
   /* Initialize fact commands and functions. */
   /*=========================================*/

   FactCommandDefinitions();
   FactFunctionDefinitions();

   /*==================================*/
   /* Initialize fact patterns for use */
   /* with the bload/bsave commands.   */
   /*==================================*/

#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME)
   FactBinarySetup();
#endif

   /*===================================*/
   /* Initialize fact patterns for use  */
   /* with the constructs-to-c command. */
   /*===================================*/

#if CONSTRUCT_COMPILER && (! RUN_TIME)
   FactPatternsCompilerSetup();
#endif
  }

/**********************************************/
/* PrintFactWithIdentifier: Displays a single */
/*   fact preceded by its fact identifier.    */
/**********************************************/
globle void PrintFactWithIdentifier(
  char *logicalName,
  struct fact *factPtr)
  {
   char printSpace[20];

   sprintf(printSpace,"f-%-5ld ",factPtr->factIndex);
   PrintRouter(logicalName,printSpace);
   PrintFact(logicalName,factPtr);
  }

/****************************************************/
/* PrintFactIdentifier: Displays a fact identifier. */
/****************************************************/
globle void PrintFactIdentifier(
  char *logicalName,
  void *factPtr)
  {
   char printSpace[20];

   sprintf(printSpace,"f-%ld",((struct fact *) factPtr)->factIndex);
   PrintRouter(logicalName,printSpace);
  }

/********************************************/
/* PrintFactIdentifierInLongForm: Display a */
/*   fact identifier in a longer format.    */
/********************************************/
globle void PrintFactIdentifierInLongForm(
  char *logicalName,
  void *factPtr)
  {
   if (AddressesToStrings) PrintRouter(logicalName,"\"");
   if (factPtr != (void *) &DummyFact)
     {
      PrintRouter(logicalName,"<Fact-");
      PrintLongInteger(logicalName,((struct fact *) factPtr)->factIndex);
      PrintRouter(logicalName,">");
     }
   else
     { PrintRouter(logicalName,"<Dummy Fact>"); }

   if (AddressesToStrings) PrintRouter(logicalName,"\"");
  }

/*******************************************/
/* DecrementFactBasisCount: Decrements the */
/*   partial match busy count of a fact    */
/*******************************************/
globle void DecrementFactBasisCount(
  void *vFactPtr)
  {
   struct fact *factPtr = (struct fact *) vFactPtr;
   struct multifield *theSegment;
   int i;

   DecrementFactCount(factPtr);

   theSegment = &factPtr->theProposition;

   for (i = 0 ; i < (int) theSegment->multifieldLength ; i++)
     {
      AtomDeinstall(theSegment->theFields[i].type,theSegment->theFields[i].value);
     }
  }

/*******************************************/
/* IncrementFactBasisCount: Increments the */
/*   partial match busy count of a fact.   */
/*******************************************/
globle void IncrementFactBasisCount(
  void *vFactPtr)
  {
   struct fact *factPtr = (struct fact *) vFactPtr;
   struct multifield *theSegment;
   int i;

   IncrementFactCount(factPtr);

   theSegment = &factPtr->theProposition;

   for (i = 0 ; i < (int) theSegment->multifieldLength ; i++)
     {
      AtomInstall(theSegment->theFields[i].type,theSegment->theFields[i].value);
     }
  }

/**************************************************/
/* PrintFact: Displays the printed representation */
/*   of a fact containing the relation name and   */
/*   all of the fact's slots or fields.           */
/**************************************************/
globle void PrintFact(
  char *logicalName,
  struct fact *factPtr)
  {
   struct multifield *theMultifield;

   /*=========================================*/
   /* Print a deftemplate (non-ordered) fact. */
   /*=========================================*/

   if (factPtr->whichDeftemplate->implied == FALSE)
     {
      PrintTemplateFact(logicalName,factPtr);
      return;
     }

   /*==============================*/
   /* Print an ordered fact (which */
   /* has an implied deftemplate). */
   /*==============================*/

   PrintRouter(logicalName,"(");

   PrintRouter(logicalName,factPtr->whichDeftemplate->header.name->contents);

   theMultifield = (struct multifield *) factPtr->theProposition.theFields[0].value;
   if (theMultifield->multifieldLength != 0)
     {
      PrintRouter(logicalName," ");
      PrintMultifield(logicalName,theMultifield,0,
                      theMultifield->multifieldLength - 1,
                      FALSE);
     }

   PrintRouter(logicalName,")");
  }

/*********************************************/
/* MatchFactFunction: Filters a fact through */
/*   the appropriate fact pattern network.   */
/*********************************************/
globle void MatchFactFunction(
  void *vTheFact)
  {
   struct fact *theFact = (struct fact *) vTheFact;

   FactPatternMatch(theFact,theFact->whichDeftemplate->patternNetwork,0,NULL,NULL);
  }

/******************************************************/
/* Retract: C access routine for the retract command. */
/******************************************************/
globle BOOLEAN Retract(
  void *vTheFact)
  {
   struct fact *theFact = (struct fact *) vTheFact;

   /*===========================================*/
   /* A fact can not be retracted while another */
   /* fact is being asserted or retracted.      */
   /*===========================================*/

   if (JoinOperationInProgress)
     {
      PrintErrorID("FACTMNGR",1,TRUE);
      PrintRouter(WERROR,"Facts may not be retracted during pattern-matching\n");
      return(FALSE);
     }

   /*====================================*/
   /* A NULL fact pointer indicates that */
   /* all facts should be retracted.     */
   /*====================================*/

   if (theFact == NULL)
     {
      RemoveAllFacts();
      return(TRUE);
     }

   /*======================================================*/
   /* Check to see if the fact has already been retracted. */
   /*======================================================*/

   if (theFact->garbage) return(FALSE);

   /*============================*/
   /* Print retraction output if */
   /* facts are being watched.   */
   /*============================*/

#if DEBUGGING_FUNCTIONS
   if (theFact->whichDeftemplate->watch)
     {
      PrintRouter(WTRACE,"<== ");
      PrintFactWithIdentifier(WTRACE,theFact);
      PrintRouter(WTRACE,"\n");
     }
#endif

   /*==================================*/
   /* Set the change flag to indicate  */
   /* the fact-list has been modified. */
   /*==================================*/

   ChangeToFactList = TRUE;

   /*===============================================*/
   /* Remove any links between the fact and partial */
   /* matches in the join network. These links are  */
   /* used to keep track of logical dependencies.   */
   /*===============================================*/

#if LOGICAL_DEPENDENCIES
   RemoveEntityDependencies((struct patternEntity *) theFact);
#endif

   /*===========================================*/
   /* Remove the fact from the fact hash table. */
   /*===========================================*/

   RemoveHashedFact(theFact);

   /*=====================================*/
   /* Remove the fact from the fact list. */
   /*=====================================*/

   if (theFact == LastFact)
     { LastFact = theFact->previousFact; }

   if (theFact->previousFact == NULL)
     {
      FactList = FactList->nextFact;
      if (FactList != NULL)
        { FactList->previousFact = NULL; }
     }
   else
     {
      theFact->previousFact->nextFact = theFact->nextFact;
      if (theFact->nextFact != NULL)
        { theFact->nextFact->previousFact = theFact->previousFact; }
     }

   /*==================================*/
   /* Update busy counts and ephemeral */
   /* garbage information.             */
   /*==================================*/

   FactDeinstall(theFact);
   EphemeralItemCount++;
   EphemeralItemSize += sizeof(struct fact) + (sizeof(struct field) * theFact->theProposition.multifieldLength);

   /*========================================*/
   /* Add the fact to the fact garbage list. */
   /*========================================*/

   theFact->nextFact = GarbageFacts;
   GarbageFacts = theFact;
   theFact->garbage = TRUE;

   /*===================================================*/
   /* Reset the evaluation error flag since expressions */
   /* will be evaluated as part of the retract.         */
   /*===================================================*/

   SetEvaluationError(FALSE);

   /*===========================================*/
   /* Loop through the list of all the patterns */
   /* that matched the fact and process the     */
   /* retract operation for each one.           */
   /*===========================================*/

   JoinOperationInProgress = TRUE;
   NetworkRetract((struct patternMatch *) theFact->list);
   JoinOperationInProgress = FALSE;

   /*=========================================*/
   /* Free partial matches that were released */
   /* by the retraction of the fact.          */
   /*=========================================*/

   if (ExecutingRule == NULL)
     { FlushGarbagePartialMatches(); }

   /*=========================================*/
   /* Retract other facts that were logically */
   /* dependent on the fact just retracted.   */
   /*=========================================*/

#if LOGICAL_DEPENDENCIES
   ForceLogicalRetractions();
#endif

   /*===========================================*/
   /* Force periodic cleanup if the retract was */
   /* executed from an embedded application.    */
   /*===========================================*/

   if ((CurrentEvaluationDepth == 0) && (! EvaluatingTopLevelCommand) &&
       (CurrentExpression == NULL))
     { PeriodicCleanup(TRUE,FALSE); }

   /*==================================*/
   /* Return TRUE to indicate the fact */
   /* was successfully retracted.      */
   /*==================================*/

   return(TRUE);
  }

/*******************************************************************/
/* RemoveGarbageFacts: Returns facts that have been retracted to   */
/*   the pool of available memory. It is necessary to postpone     */
/*   returning the facts to memory because RHS actions retrieve    */
/*   their variable bindings directly from the fact data structure */
/*   and the facts may be in use in other data structures.         */
/*******************************************************************/
static void RemoveGarbageFacts()
  {
   struct fact *factPtr, *nextPtr, *lastPtr = NULL;

   factPtr = GarbageFacts;

   while (factPtr != NULL)
     {
      nextPtr = factPtr->nextFact;
      if ((factPtr->factHeader.busyCount == 0) &&
          (((int) factPtr->depth) > CurrentEvaluationDepth))
        {
         EphemeralItemCount--;
         EphemeralItemSize -= sizeof(struct fact) + (sizeof(struct field) * factPtr->theProposition.multifieldLength);
         ReturnFact(factPtr);
         if (lastPtr == NULL) GarbageFacts = nextPtr;
         else lastPtr->nextFact = nextPtr;
        }
      else
        { lastPtr = factPtr; }

      factPtr = nextPtr;
     }
  }

/*****************************************************/
/* Assert: C access routine for the assert function. */
/*****************************************************/
globle void *Assert(
  void *vTheFact)
  {
   int hashValue;
   int length, i;
   struct field *theField;
   struct fact *theFact = (struct fact *) vTheFact;

   /*==========================================*/
   /* A fact can not be asserted while another */
   /* fact is being asserted or retracted.     */
   /*==========================================*/

   if (JoinOperationInProgress)
     {
      ReturnFact(theFact);
      PrintErrorID("FACTMNGR",2,TRUE);
      PrintRouter(WERROR,"Facts may not be asserted during pattern-matching\n");
      return(NULL);
     }

   /*=============================================================*/
   /* Replace invalid data types in the fact with the symbol nil. */
   /*=============================================================*/

   length = theFact->theProposition.multifieldLength;
   theField = theFact->theProposition.theFields;

   for (i = 0; i < length; i++)
     {
      if (theField[i].type == RVOID)
        {
         theField[i].type = SYMBOL;
         theField[i].value = (void *) AddSymbol("nil");
        }
     }

   /*========================================================*/
   /* If fact assertions are being checked for duplications, */
   /* then search the fact list for a duplicate fact.        */
   /*========================================================*/

   hashValue = HandleFactDuplication(theFact);
   if (hashValue < 0) return(NULL);

   /*==========================================================*/
   /* If necessary, add logical dependency links between the   */
   /* fact and the partial match which is its logical support. */
   /*==========================================================*/

#if LOGICAL_DEPENDENCIES
   if (AddLogicalDependencies((struct patternEntity *) theFact,FALSE) == FALSE)
     {
      ReturnFact(theFact);
      return(NULL);
     }
#endif

   /*======================================*/
   /* Add the fact to the fact hash table. */
   /*======================================*/

   AddHashedFact(theFact,hashValue);

   /*================================*/
   /* Add the fact to the fact list. */
   /*================================*/

   theFact->nextFact = NULL;
   theFact->list = NULL;
   theFact->previousFact = LastFact;
   if (LastFact == NULL)
     { FactList = theFact; }
   else
     { LastFact->nextFact = theFact; }
   LastFact = theFact;

   /*==================================*/
   /* Set the fact index and time tag. */
   /*==================================*/

   theFact->factIndex = NextFactIndex++;
   theFact->factHeader.timeTag = CurrentEntityTimeTag++;

   /*=====================*/
   /* Update busy counts. */
   /*=====================*/

   FactInstall(theFact);

   /*==========================*/
   /* Print assert output if   */
   /* facts are being watched. */
   /*==========================*/

#if DEBUGGING_FUNCTIONS
   if (theFact->whichDeftemplate->watch)
     {
      PrintRouter(WTRACE,"==> ");
      PrintFactWithIdentifier(WTRACE,theFact);
      PrintRouter(WTRACE,"\n");
     }
#endif

   /*==================================*/
   /* Set the change flag to indicate  */
   /* the fact-list has been modified. */
   /*==================================*/

   ChangeToFactList = TRUE;

   /*==========================================*/
   /* Check for constraint errors in the fact. */
   /*==========================================*/

   CheckTemplateFact(theFact);

   /*===================================================*/
   /* Reset the evaluation error flag since expressions */
   /* will be evaluated as part of the assert .         */
   /*===================================================*/

   SetEvaluationError(FALSE);

   /*=============================================*/
   /* Pattern match the fact using the associated */
   /* deftemplate's pattern network.              */
   /*=============================================*/

   JoinOperationInProgress = TRUE;
   FactPatternMatch(theFact,theFact->whichDeftemplate->patternNetwork,0,NULL,NULL);
   JoinOperationInProgress = FALSE;

   /*===================================================*/
   /* Retract other facts that were logically dependent */
   /* on the non-existence of the fact just asserted.   */
   /*===================================================*/

#if LOGICAL_DEPENDENCIES
   ForceLogicalRetractions();
#endif

   /*=========================================*/
   /* Free partial matches that were released */
   /* by the assertion of the fact.           */
   /*=========================================*/

   if (ExecutingRule == NULL) FlushGarbagePartialMatches();

   /*==========================================*/
   /* Force periodic cleanup if the assert was */
   /* executed from an embedded application.   */
   /*==========================================*/

   if ((CurrentEvaluationDepth == 0) && (! EvaluatingTopLevelCommand) &&
       (CurrentExpression == NULL))
     { PeriodicCleanup(TRUE,FALSE); }

   /*===============================*/
   /* Return a pointer to the fact. */
   /*===============================*/

   return((void *) theFact);
  }

/**************************************/
/* RemoveAllFacts: Loops through the  */
/*   fact-list and removes each fact. */
/**************************************/
globle void RemoveAllFacts()
  {
   while (FactList != NULL)
     { Retract((void *) FactList); }
  }

/*********************************************/
/* CreateFact: Creates a fact data structure */
/*   of the specified deftemplate.           */
/*********************************************/
globle struct fact *CreateFact(
  void *vTheDeftemplate)
  {
   struct deftemplate *theDeftemplate = (struct deftemplate *) vTheDeftemplate;
   struct fact *newFact;
   int i;

   /*=================================*/
   /* A deftemplate must be specified */
   /* in order to create a fact.      */
   /*=================================*/

   if (theDeftemplate == NULL) return(NULL);

   /*============================================*/
   /* Create a fact for an explicit deftemplate. */
   /*============================================*/

   if (theDeftemplate->implied == FALSE)
     {
      newFact = CreateFactBySize((int) theDeftemplate->numberOfSlots);
      for (i = 0;
           i < (int) theDeftemplate->numberOfSlots;
           i++)
        { newFact->theProposition.theFields[i].type = RVOID; }
     }

   /*===========================================*/
   /* Create a fact for an implied deftemplate. */
   /*===========================================*/

   else
     {
      newFact = CreateFactBySize(1);
      newFact->theProposition.theFields[0].type = MULTIFIELD;
      newFact->theProposition.theFields[0].value = CreateMultifield2(0L);
     }

   /*===============================*/
   /* Return a pointer to the fact. */
   /*===============================*/

   newFact->whichDeftemplate = theDeftemplate;

   return(newFact);
  }

/********************************************/
/* GetFactSlot: Returns the slot value from */
/*   the specified slot of a fact.          */
/********************************************/
globle BOOLEAN GetFactSlot(
  void *vTheFact,
  char *slotName,
  DATA_OBJECT *theValue)
  {
   struct fact *theFact = (struct fact *) vTheFact;
   struct deftemplate *theDeftemplate;
   int whichSlot;

   /*===============================================*/
   /* Get the deftemplate associated with the fact. */
   /*===============================================*/

   theDeftemplate = theFact->whichDeftemplate;

   /*==============================================*/
   /* Handle retrieving the slot value from a fact */
   /* having an implied deftemplate. An implied    */
   /* facts has a single multifield slot.          */
   /*==============================================*/

   if (theDeftemplate->implied)
     {
      if (slotName != NULL) return(FALSE);
      theValue->type = theFact->theProposition.theFields[0].type;
      theValue->value = theFact->theProposition.theFields[0].value;
      SetpDOBegin(theValue,1);
      SetpDOEnd(theValue,((struct multifield *) theValue->value)->multifieldLength);
      return(TRUE);
     }

   /*===================================*/
   /* Make sure the slot name requested */
   /* corresponds to a valid slot name. */
   /*===================================*/

   if (FindSlot(theDeftemplate,(SYMBOL_HN *) AddSymbol(slotName),&whichSlot) == NULL)
     { return(FALSE); }

   /*======================================================*/
   /* Return the slot value. If the slot value wasn't set, */
   /* then return FALSE to indicate that an appropriate    */
   /* slot value wasn't available.                         */
   /*======================================================*/

   theValue->type = theFact->theProposition.theFields[whichSlot-1].type;
   theValue->value = theFact->theProposition.theFields[whichSlot-1].value;
   if (theValue->type == MULTIFIELD)
     {
      SetpDOBegin(theValue,1);
      SetpDOEnd(theValue,((struct multifield *) theValue->value)->multifieldLength);
     }

   if (theValue->type == RVOID) return(FALSE);

   return(TRUE);
  }

/***************************************/
/* PutFactSlot: Sets the slot value of */
/*   the specified slot of a fact.     */
/***************************************/
globle BOOLEAN PutFactSlot(
  void *vTheFact,
  char *slotName,
  DATA_OBJECT *theValue)
  {
   struct fact *theFact = (struct fact *) vTheFact;
   struct deftemplate *theDeftemplate;
   struct templateSlot *theSlot;
   int whichSlot;

   /*===============================================*/
   /* Get the deftemplate associated with the fact. */
   /*===============================================*/

   theDeftemplate = theFact->whichDeftemplate;

   /*============================================*/
   /* Handle setting the slot value of a fact    */
   /* having an implied deftemplate. An implied  */
   /* facts has a single multifield slot.        */
   /*============================================*/

   if (theDeftemplate->implied)
     {
      if ((slotName != NULL) || (theValue->type != MULTIFIELD))
        { return(FALSE); }

      AtomDeinstall(theFact->theProposition.theFields[0].type,
                    theFact->theProposition.theFields[0].value);

      if (theFact->theProposition.theFields[0].type == MULTIFIELD)
        { ReturnMultifield((struct multifield *) theFact->theProposition.theFields[0].value); }

      theFact->theProposition.theFields[0].type = theValue->type;
      theFact->theProposition.theFields[0].value = DOToMultifield(theValue);
      
      AtomInstall(theFact->theProposition.theFields[0].type,
                  theFact->theProposition.theFields[0].value);

      return(TRUE);
     }

   /*===================================*/
   /* Make sure the slot name requested */
   /* corresponds to a valid slot name. */
   /*===================================*/

   if ((theSlot = FindSlot(theDeftemplate,(SYMBOL_HN *) AddSymbol(slotName),&whichSlot)) == NULL)
     { return(FALSE); }

   /*=============================================*/
   /* Make sure a single field value is not being */
   /* stored in a multifield slot or vice versa.  */
   /*=============================================*/

   if (((theSlot->multislot == 0) && (theValue->type == MULTIFIELD)) ||
       ((theSlot->multislot == 1) && (theValue->type != MULTIFIELD)))
     { return(FALSE); }

   /*=====================*/
   /* Set the slot value. */
   /*=====================*/

   AtomDeinstall(theFact->theProposition.theFields[whichSlot-1].type,
                 theFact->theProposition.theFields[whichSlot-1].value);

   if (theFact->theProposition.theFields[whichSlot-1].type == MULTIFIELD)
     { ReturnMultifield((struct multifield *) theFact->theProposition.theFields[whichSlot-1].value); }

   theFact->theProposition.theFields[whichSlot-1].type = theValue->type;

   if (theValue->type == MULTIFIELD)
     { theFact->theProposition.theFields[whichSlot-1].value = DOToMultifield(theValue); }
   else
     { theFact->theProposition.theFields[whichSlot-1].value = theValue->value; }
   
   AtomInstall(theFact->theProposition.theFields[whichSlot-1].type,
               theFact->theProposition.theFields[whichSlot-1].value);

   return(TRUE);
  }

/********************************************************/
/* AssignFactSlotDefaults: Sets a fact's slot values to */
/*   its default value if the value of the slot has not */
/*   yet been set.                                      */
/********************************************************/
globle BOOLEAN AssignFactSlotDefaults(
  void *vTheFact)
  {
   struct fact *theFact = (struct fact *) vTheFact;
   struct deftemplate *theDeftemplate;
   struct templateSlot *slotPtr;
   int i;
   DATA_OBJECT theResult;

   /*===============================================*/
   /* Get the deftemplate associated with the fact. */
   /*===============================================*/

   theDeftemplate = theFact->whichDeftemplate;

   /*================================================*/
   /* The value for the implied multifield slot of   */
   /* an implied deftemplate is set to a multifield  */
   /* of length zero when the fact is created.       */
   /*================================================*/

   if (theDeftemplate->implied) return(TRUE);

   /*============================================*/
   /* Loop through each slot of the deftemplate. */
   /*============================================*/

   for (i = 0, slotPtr = theDeftemplate->slotList;
        i < (int) theDeftemplate->numberOfSlots;
        i++, slotPtr = slotPtr->next)
     {
      /*===================================*/
      /* If the slot's value has been set, */
      /* then move on to the next slot.    */
      /*===================================*/

      if (theFact->theProposition.theFields[i].type != RVOID) continue;

      /*===============================================*/
      /* If the (default ?NONE) attribute was declared */
      /* for the slot, then return FALSE to indicate   */
      /* the default values for the fact couldn't be   */
      /* supplied since this attribute requires that a */
      /* default value can't be used for the slot.     */
      /*===============================================*/

      if (slotPtr->noDefault) return(FALSE);

      /*==============================================*/
      /* Otherwise if a static default was specified, */
      /* use this as the default value.               */
      /*==============================================*/

      else if (slotPtr->defaultPresent)
        {
         if (slotPtr->multislot)
           {
            StoreInMultifield(&theResult,slotPtr->defaultList,TRUE);
            theFact->theProposition.theFields[i].value = DOToMultifield(&theResult);
           }
         else
           {
           theFact->theProposition.theFields[i].type = slotPtr->defaultList->type;
           theFact->theProposition.theFields[i].value = slotPtr->defaultList->value;
          }
        }

      /*================================================*/
      /* Otherwise if a dynamic-default was specified,  */
      /* evaluate it and use this as the default value. */
      /*================================================*/

      else if (slotPtr->defaultDynamic)
        {
         EvaluateExpression(slotPtr->defaultList,&theResult);
         if (EvaluationError) return(FALSE);
         theFact->theProposition.theFields[i].type = (short) theResult.type;
         if (theResult.type == MULTIFIELD)
           { theFact->theProposition.theFields[i].value = DOToMultifield(&theResult); }
         else
           { theFact->theProposition.theFields[i].value = theResult.value; }
        }

      /*====================================*/
      /* Otherwise derive the default value */
      /* from the slot's constraints.       */
      /*====================================*/

      else
        {
         DeriveDefaultFromConstraints(slotPtr->constraints,&theResult,
                                     (int) slotPtr->multislot);

         theFact->theProposition.theFields[i].type = (short) theResult.type;
         if (theResult.type == MULTIFIELD)
           { theFact->theProposition.theFields[i].value = DOToMultifield(&theResult); }
         else
           { theFact->theProposition.theFields[i].value = theResult.value; }
        }
     }

   /*==========================================*/
   /* Return TRUE to indicate that the default */
   /* values have been successfully set.       */
   /*==========================================*/

   return(TRUE);
  }

/***************************************************************/
/* CopyFactSlotValues: Copies the slot values from one fact to */
/*   another. Both facts must have the same relation name.     */
/***************************************************************/
globle BOOLEAN CopyFactSlotValues(
  void *vTheDestFact,
  void *vTheSourceFact)
  {
   struct fact *theDestFact = (struct fact *) vTheDestFact;
   struct fact *theSourceFact = (struct fact *) vTheSourceFact;
   struct deftemplate *theDeftemplate;
   struct templateSlot *slotPtr;
   int i;

   /*===================================*/
   /* Both facts must be the same type. */
   /*===================================*/

   theDeftemplate = theSourceFact->whichDeftemplate;
   if (theDestFact->whichDeftemplate != theDeftemplate)
     { return(FALSE); }

   /*===================================================*/
   /* Loop through each slot of the deftemplate copying */
   /* the source fact value to the destination fact.    */
   /*===================================================*/

   for (i = 0, slotPtr = theDeftemplate->slotList;
        i < (int) theDeftemplate->numberOfSlots;
        i++, slotPtr = slotPtr->next)
     {
      theDestFact->theProposition.theFields[i].type =
         theSourceFact->theProposition.theFields[i].type;
      if (theSourceFact->theProposition.theFields[i].type != MULTIFIELD)
        {
         theDestFact->theProposition.theFields[i].value =
           theSourceFact->theProposition.theFields[i].value;
        }
      else
        {
         theDestFact->theProposition.theFields[i].value =
           CopyMultifield((struct multifield *) theSourceFact->theProposition.theFields[i].value);
        }
     }

   /*========================================*/
   /* Return TRUE to indicate that fact slot */
   /* values were successfully copied.       */
   /*========================================*/

   return(TRUE);
  }

/*********************************************/
/* CreateFactBySize: Allocates a fact data   */
/*   structure based on the number of slots. */
/*********************************************/
globle struct fact *CreateFactBySize(
  int size)
  {
   struct fact *theFact;
   int newSize;

   if (size <= 0) newSize = 1;
   else newSize = size;

   theFact = get_var_struct2(fact,sizeof(struct field) * (newSize - 1));

   theFact->depth = (unsigned) CurrentEvaluationDepth;
   theFact->garbage = FALSE;
   theFact->factIndex = 0L;
   theFact->factHeader.busyCount = 0;
   theFact->factHeader.theInfo = &FactInfo;
#if LOGICAL_DEPENDENCIES
   theFact->factHeader.dependents = NULL;
#endif
   theFact->whichDeftemplate = NULL;
   theFact->nextFact = NULL;
   theFact->previousFact = NULL;
   theFact->list = NULL;

   theFact->theProposition.multifieldLength = size;
   theFact->theProposition.depth = (short) CurrentEvaluationDepth;
   theFact->theProposition.busyCount = 0;

   return(theFact);
  }

/*********************************************/
/* ReturnFact: Returns a fact data structure */
/*   to the pool of free memory.             */
/*********************************************/
globle void ReturnFact(
  struct fact *theFact)
  {
   struct multifield *theSegment;
   int newSize, i;

   theSegment = &theFact->theProposition;

   for (i = 0; i < (int) theSegment->multifieldLength; i++)
     {
      if (theSegment->theFields[i].type == MULTIFIELD)
        { ReturnMultifield((struct multifield *) theSegment->theFields[i].value); }
     }

   if (theFact->theProposition.multifieldLength == 0) newSize = 1;
   else newSize = theFact->theProposition.multifieldLength;

   rtn_var_struct2(fact,sizeof(struct field) * (newSize - 1),theFact);
  }

/*************************************************************/
/* FactInstall: Increments the fact, deftemplate, and atomic */
/*   data value busy counts associated with the fact.        */
/*************************************************************/
globle void FactInstall(
  struct fact *newFact)
  {
   struct multifield *theSegment;
   int i;

   NumberOfFacts++;
   newFact->whichDeftemplate->busyCount++;
   theSegment = &newFact->theProposition;

   for (i = 0 ; i < (int) theSegment->multifieldLength ; i++)
     {
      AtomInstall(theSegment->theFields[i].type,theSegment->theFields[i].value);
     }

   newFact->factHeader.busyCount++;
  }

/***************************************************************/
/* FactDeinstall: Decrements the fact, deftemplate, and atomic */
/*   data value busy counts associated with the fact.          */
/***************************************************************/
globle void FactDeinstall(
  struct fact *newFact)
  {
   struct multifield *theSegment;
   int i;

   NumberOfFacts--;
   theSegment = &newFact->theProposition;
   newFact->whichDeftemplate->busyCount--;

   for (i = 0 ; i < (int) theSegment->multifieldLength ; i++)
     {
      AtomDeinstall(theSegment->theFields[i].type,theSegment->theFields[i].value);
     }

   newFact->factHeader.busyCount--;
  }

/*********************************************/
/* IncrementFactCount: Increments the number */
/*   of references to a specified fact.      */
/*********************************************/
globle void IncrementFactCount(
  void *factPtr)
  {
   ((struct fact *) factPtr)->factHeader.busyCount++;
  }

/*********************************************/
/* DecrementFactCount: Decrements the number */
/*   of references to a specified fact.      */
/*********************************************/
globle void DecrementFactCount(
  void *factPtr)
  {
   ((struct fact *) factPtr)->factHeader.busyCount--;
  }

/************************************************************/
/* GetNextFact: If passed a NULL pointer, returns the first */
/*   fact in the fact-list. Otherwise returns the next fact */
/*   following the fact passed as an argument.              */
/************************************************************/
globle void *GetNextFact(
  void *factPtr)
  {
   if (factPtr == NULL)
     { return((void *) FactList); }

   if (((struct fact *) factPtr)->garbage) return(NULL);

   return((void *) ((struct fact *) factPtr)->nextFact);
  }

/**************************************************/
/* GetNextFactInScope: Returns the next fact that */
/*   is in scope of the current module. Works in  */
/*   a similar fashion to GetNextFact, but skips  */
/*   facts that are out of scope.                 */
/**************************************************/
globle void *GetNextFactInScope(
  void *vTheFact)
  {
   Thread static long lastModuleIndex = -1;
   struct fact *theFact = (struct fact *) vTheFact;

   /*=======================================================*/
   /* If fact passed as an argument is a NULL pointer, then */
   /* we're just beginning a traversal of the fact list. If */
   /* the module index has changed since that last time the */
   /* fact list was traversed by this routine, then         */
   /* determine all of the deftemplates that are in scope   */
   /* of the current module.                                */
   /*=======================================================*/

   if (theFact == NULL)
     {
      theFact = FactList;
      if (lastModuleIndex != ModuleChangeIndex)
        {
         UpdateDeftemplateScope();
         lastModuleIndex = ModuleChangeIndex;
        }
     }

   /*==================================================*/
   /* Otherwise, if the fact passed as an argument has */
   /* been retracted, then there's no way to determine */
   /* the next fact, so return a NULL pointer.         */
   /*==================================================*/

   else if (((struct fact *) theFact)->garbage)
     { return(NULL); }

   /*==================================================*/
   /* Otherwise, start the search for the next fact in */
   /* scope with the fact immediately following the    */
   /* fact passed as an argument.                      */
   /*==================================================*/

   else
     { theFact = theFact->nextFact; }

   /*================================================*/
   /* Continue traversing the fact-list until a fact */
   /* is found that's associated with a deftemplate  */
   /* that's in scope.                               */
   /*================================================*/

   while (theFact != NULL)
     {
      if (theFact->whichDeftemplate->inScope) return((void *) theFact);

      theFact = theFact->nextFact;
     }

   return(NULL);
  }

/*******************************************/
/* GetFactPPForm: Returns the pretty print */
/*   representation of a fact.             */
/*******************************************/
globle void GetFactPPForm(
  char *buffer,
  int bufferLength,
  void *theFact)
  {
   OpenStringDestination("FactPPForm",buffer,bufferLength);
   PrintFactWithIdentifier("FactPPForm",(struct fact *) theFact);
   CloseStringDestination("FactPPForm");
  }

/***********************************/
/* FactIndex: C access routine for */
/*   the fact-index function.      */
/***********************************/
globle long int FactIndex(
  void *factPtr)
  {
   return(((struct fact *) factPtr)->factIndex);
  }

/**************************************/
/* AssertString: C access routine for */
/*   the assert-string function.      */
/**************************************/
globle void *AssertString(
  char *theString)
  {
   struct fact *theFact;

   if ((theFact = StringToFact(theString)) == NULL) return(NULL);

   return((void *) Assert((void *) theFact));
  }

/******************************************************/
/* GetFactListChanged: Returns the flag indicating    */
/*   whether a change to the fact-list has been made. */
/******************************************************/
globle int GetFactListChanged()
  { return(ChangeToFactList); }

/********************************************************/
/* SetFactListChanged: Sets the flag indicating whether */
/*   a change to the fact-list has been made.           */
/********************************************************/
globle void SetFactListChanged(
  int value)
  {
   ChangeToFactList = value;
  }

/****************************************/
/* GetNumberOfFacts: Returns the number */
/* of facts in the fact-list.           */
/****************************************/
globle long int GetNumberOfFacts()
  { return(NumberOfFacts); }

/***********************************************************/
/* ResetFacts: Reset function for facts. Sets the starting */
/*   fact index to zero and removes all facts.             */
/***********************************************************/
static void ResetFacts()
  {
   /*====================================*/
   /* Initialize the fact index to zero. */
   /*====================================*/

   NextFactIndex = 0L;

   /*======================================*/
   /* Remove all facts from the fact list. */
   /*======================================*/

   RemoveAllFacts();
  }

/************************************************************/
/* ClearFactsReady: Clear ready function for facts. Returns */
/*   TRUE if facts were successfully removed and the clear  */
/*   command can continue, otherwise FALSE.                 */
/************************************************************/
static int ClearFactsReady()
  {
   /*====================================*/
   /* Initialize the fact index to zero. */
   /*====================================*/

   NextFactIndex = 0L;

   /*======================================*/
   /* Remove all facts from the fact list. */
   /*======================================*/

   RemoveAllFacts();

   /*==============================================*/
   /* If for some reason there are any facts still */
   /* remaining, don't continue with the clear.    */
   /*==============================================*/

   if (GetNextFact(NULL) != NULL) return(FALSE);

   /*=============================*/
   /* Return TRUE to indicate the */
   /* clear command can continue. */
   /*=============================*/

   return(TRUE);
  }

/***************************************************/
/* FindIndexedFact: Returns a pointer to a fact in */
/*   the fact list with the specified fact index.  */
/***************************************************/
globle struct fact *FindIndexedFact(
  long int factIndexSought)
  {
   struct fact *theFact;

   for (theFact = (struct fact *) GetNextFact(NULL);
        theFact != NULL;
        theFact = (struct fact *) GetNextFact(theFact))
     {
      if (theFact->factIndex == factIndexSought)
        { return(theFact); }
     }

   return(NULL);
  }

#endif /* DEFTEMPLATE_CONSTRUCT && DEFRULE_CONSTRUCT */


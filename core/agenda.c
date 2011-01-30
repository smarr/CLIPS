   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*                    AGENDA MODULE                    */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*   Provides functionality for examining, manipulating,     */
/*   adding, and removing activations from the agenda.       */
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

#define _AGENDA_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include "setup.h"

#if DEFRULE_CONSTRUCT

#include "argacces.h"
#include "constant.h"
#include "crstrtgy.h"
#include "engine.h"
#include "extnfunc.h"
#include "memalloc.h"
#include "moduldef.h"
#include "modulutl.h"
#include "reteutil.h"
#include "retract.h"
#include "router.h"
#include "rulebsc.h"
#include "ruledef.h"
#include "strngrtr.h"
#include "sysdep.h"
#include "watch.h"

#include "agenda.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    PrintActivation(char *,void *);
   static void                    AgendaClearFunction(void);
#if DYNAMIC_SALIENCE
   static char                   *SalienceEvaluationName(int);
   static int                     EvaluateSalience(void *);
#endif

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

#if DEBUGGING_FUNCTIONS
   globle Thread BOOLEAN              WatchActivations = OFF;
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static long int             NumberOfActivations = 0;
   Thread static unsigned long int    CurrentTimetag = 0;
   Thread static int                  AgendaChanged = FALSE;
#if DYNAMIC_SALIENCE
   Thread static BOOLEAN              SalienceEvaluation = WHEN_DEFINED;
#endif

/*************************************************/
/* InitializeAgenda: Initializes the activations */
/*   watch item and the H/L commands for         */
/*   manipulating the agenda.                    */
/*************************************************/
globle void InitializeAgenda()
  {
   AddClearFunction("agenda",AgendaClearFunction,0);
#if DEBUGGING_FUNCTIONS
   AddWatchItem("activations",1,&WatchActivations,40,DefruleWatchAccess,DefruleWatchPrint);
#endif
#if ! RUN_TIME
   DefineFunction2("refresh", 'v', PTIF RefreshCommand, "RefreshCommand", "11w");
#if DYNAMIC_SALIENCE
   DefineFunction2("refresh-agenda",'v',
                   PTIF RefreshAgendaCommand,"RefreshAgendaCommand", "01w");
   DefineFunction2("get-salience-evaluation",'w',
                   PTIF GetSalienceEvaluationCommand,
                   "GetSalienceEvaluationCommand", "00");
   DefineFunction2("set-salience-evaluation",'w',
                   PTIF SetSalienceEvaluationCommand,
                   "SetSalienceEvaluationCommand",
                   "11w");
#endif
#if DEBUGGING_FUNCTIONS
   DefineFunction2("agenda", 'v', PTIF AgendaCommand, "AgendaCommand", "01w");
#endif
#endif
  }

/*****************************************************************/
/* AddActivation: Creates a rule activation to be added to the   */
/*   Agenda and links the activation with its associated partial */
/*   match. The function PlaceActivation is then called to place */
/*   the activation on the Agenda. Typically called when all     */
/*   patterns on the LHS of a rule have been satisfied.          */
/*****************************************************************/
globle void AddActivation(
  void *vTheRule,
  void *vBinds)
  {
   struct activation *newActivation;
   struct defrule *theRule = (struct defrule *) vTheRule;
   struct partialMatch *binds = (struct partialMatch *) vBinds;
   struct defruleModule *theModuleItem;

   /*=======================================*/
   /* Focus on the module if the activation */
   /* is from an auto-focus rule.           */
   /*=======================================*/

   if (theRule->autoFocus)
     { Focus((void *) theRule->header.whichModule->theModule); }

   /*=======================================================*/
   /* Create the activation. The activation stores pointers */
   /* to its associated partial match and defrule. The      */
   /* activation is given a time tag, its salience is       */
   /* evaluated, and it is assigned a random number for use */
   /* with the random conflict resolution strategy.         */
   /*=======================================================*/

   newActivation = get_struct(activation);
   newActivation->theRule = theRule;
   newActivation->basis = binds;
   newActivation->timetag = CurrentTimetag++;
#if DYNAMIC_SALIENCE
   newActivation->salience = EvaluateSalience(theRule);
#else
   newActivation->salience = theRule->salience;
#endif
#if CONFLICT_RESOLUTION_STRATEGIES
   newActivation->sortedBasis = NULL;
   newActivation->randomID = genrand();
#endif
   newActivation->prev = NULL;
   newActivation->next = NULL;

   NumberOfActivations++;

   /*=======================================================*/
   /* Point the partial match to the activation to complete */
   /* the link between the join network and the agenda.     */
   /*=======================================================*/

   binds->binds[binds->bcount].gm.theValue = (void *) newActivation;

   /*====================================================*/
   /* If activations are being watch, display a message. */
   /*====================================================*/

#if DEBUGGING_FUNCTIONS
   if (newActivation->theRule->watchActivation)
     {
      PrintRouter(WTRACE,"==> Activation ");
      PrintActivation(WTRACE,(void *) newActivation);
      PrintRouter(WTRACE,"\n");
     }
#endif

    /*=====================================*/
    /* Place the activation on the agenda. */
    /*=====================================*/

    theModuleItem = (struct defruleModule *) theRule->header.whichModule;
    PlaceActivation(&(theModuleItem->agenda),newActivation);
   }

/***************************************************************/
/* ClearRuleFromAgenda: Clears the agenda of a specified rule. */
/***************************************************************/
globle void ClearRuleFromAgenda(
  void *vTheRule)
  {
   struct defrule *theRule = (struct defrule *) vTheRule;
   struct defrule *tempRule;
   struct activation *agendaPtr, *agendaNext;

   /*============================================*/
   /* Get a pointer to the agenda for the module */
   /* in which the rule is contained.            */
   /*============================================*/

   agendaPtr = ((struct defruleModule *) theRule->header.whichModule)->agenda;

   /*==============================================*/
   /* Loop through every activation on the agenda. */
   /*==============================================*/

   while (agendaPtr != NULL)
     {
      agendaNext = agendaPtr->next;

      /*========================================================*/
      /* Check each disjunct of the rule against the activation */
      /* to determine if the activation points to the rule. If  */
      /* it does, then remove the activation from the agenda.   */
      /*========================================================*/

      for (tempRule = theRule;
           tempRule != NULL;
           tempRule = tempRule->disjunct)
        {
         if (agendaPtr->theRule == tempRule)
           {
            RemoveActivation(agendaPtr,TRUE,TRUE);
            break;
           }
        }

      agendaPtr = agendaNext;
     }
  }

/***************************************************************/
/* GetNextActivation: Returns an activation from the Agenda.   */
/*   If its argument is NULL, then the first activation on the */
/*   Agenda is returned. If its argument is not NULL, the next */
/*   activation after the argument is returned.                */
/***************************************************************/
globle void *GetNextActivation(
  void *actPtr)
  {
   struct defruleModule *theModuleItem;

   if (actPtr == NULL)
     {
      theModuleItem = (struct defruleModule *) GetModuleItem(NULL,DefruleModuleIndex);
      if (theModuleItem == NULL) return(NULL);
      return((void *) theModuleItem->agenda);
     }
   else
     { return((void *) (((struct activation *) actPtr)->next)); }
  }

/**********************************************************************************/
/* GetActivationName: Returns the name of the rule associated with an activation. */
/**********************************************************************************/
globle char *GetActivationName(
  void *actPtr)
  { return(ValueToString(((struct activation *) actPtr)->theRule->header.name)); }

/********************************************************************/
/* SetActivationSalience: Sets the salience value of an activation. */
/********************************************************************/
globle int SetActivationSalience(
  void *actPtr,
  int value)
  {
   int temp;

   temp = ((struct activation *) actPtr)->salience;
   ((struct activation *) actPtr)->salience = value;
   return(temp);
  }

/**********************************************************************************/
/* GetActivationPPForm: Returns the pretty print representation of an activation. */
/**********************************************************************************/
globle void GetActivationPPForm(
  char *buffer,
  int bufferLength,
  void *theActivation)
  {
   OpenStringDestination("ActPPForm",buffer,bufferLength);
   PrintActivation("ActPPForm",(void *) theActivation);
   CloseStringDestination("ActPPForm");
  }

/********************************************/
/* MoveActivationToTop: Moves the specified */
/*   activation to the top of the agenda.   */
/********************************************/
globle BOOLEAN MoveActivationToTop(
  void *vtheActivation)
  {
   struct activation *prevPtr;
   struct activation *theActivation = (struct activation *) vtheActivation;
   struct defruleModule *theModuleItem;

   /*====================================*/
   /* Determine the module of the agenda */
   /* in which the activation is stored. */
   /*====================================*/

   theModuleItem = (struct defruleModule *) theActivation->theRule->header.whichModule;

   /*============================================*/
   /* If the activation is already at the top of */
   /* the agenda, then nothing needs to be done. */
   /*============================================*/

   if (theActivation == theModuleItem->agenda) return(FALSE);

   /*=================================================*/
   /* Update the pointers of the activation preceding */
   /* and following the activation being moved.       */
   /*=================================================*/

   prevPtr = theActivation->prev;
   prevPtr->next = theActivation->next;
   if (theActivation->next != NULL) theActivation->next->prev = prevPtr;

   /*=======================================================*/
   /* Move the activation and then update its pointers, the */
   /* pointers of the activation following it, and the      */
   /* module pointer to the top activation on the agenda.   */
   /*=======================================================*/

   theActivation->next = theModuleItem->agenda;
   theModuleItem->agenda->prev = theActivation;
   theActivation->prev = NULL;
   theModuleItem->agenda = theActivation;

   /*=============================*/
   /* Mark the agenda as changed. */
   /*=============================*/

   AgendaChanged = TRUE;

   return(TRUE);
  }

/******************************************************/
/* DeleteActivation: Removes the specified activation */
/*   from the agenda.                                 */
/******************************************************/
globle BOOLEAN DeleteActivation(
  void *theActivation)
  {
   if (theActivation == NULL) RemoveAllActivations();
   else RemoveActivation((struct activation *) theActivation,TRUE,TRUE);

   return(TRUE);
  }

/*******************************************************/
/* DetachActivation: Detaches the specified activation */
/*   from the list of activations on the Agenda.       */
/*******************************************************/
globle BOOLEAN DetachActivation(
  void *vTheActivation)
  {
   struct defruleModule *theModuleItem;
   struct activation *theActivation = (struct activation *) vTheActivation;

   /*============================*/
   /* A NULL pointer is invalid. */
   /*============================*/

   if (theActivation == NULL) SystemError("AGENDA",1);

   /*====================================*/
   /* Determine the module of the agenda */
   /* in which the activation is stored. */
   /*====================================*/

   theModuleItem = (struct defruleModule *) theActivation->theRule->header.whichModule;

   /*========================================================*/
   /* If the activation is the top activation on the agenda, */
   /* then update the module pointer to agenda.              */
   /*========================================================*/

   if (theActivation == theModuleItem->agenda)
     { theModuleItem->agenda = theActivation->next; }

   /*==================================================*/
   /* Update the pointers in the preceding activation. */
   /*==================================================*/

   if (theActivation->prev != NULL)
     { theActivation->prev->next = theActivation->next; }

   /*==================================================*/
   /* Update the pointers in the following activation. */
   /*==================================================*/

   if (theActivation->next != NULL)
     { theActivation->next->prev = theActivation->prev; }

   /*=================================================*/
   /* Update the pointers in the detached activation. */
   /*=================================================*/

   theActivation->prev = NULL;
   theActivation->next = NULL;

   /*=============================*/
   /* Mark the agenda as changed. */
   /*=============================*/

   AgendaChanged = TRUE;

   return(TRUE);
  }

/****************************************************************************/
/* PrintActivation: Prints an activation in a "pretty" format. Salience,    */
/*   rule name, and the partial match which activated the rule are printed. */
/****************************************************************************/
static void PrintActivation(
  char *logicalName,
  void *vTheActivation)
  {
   struct activation *theActivation = (struct activation *) vTheActivation;
   char printSpace[20];

   sprintf(printSpace,"%-6d ",theActivation->salience);
   PrintRouter(logicalName,printSpace);
   PrintRouter(logicalName,ValueToString(theActivation->theRule->header.name));
   PrintRouter(logicalName,": ");
   PrintPartialMatch(logicalName,theActivation->basis);
  }

/****************************************************/
/* Agenda: C access routine for the agenda command. */
/****************************************************/
globle void Agenda(
  char *logicalName,
  void *vTheModule)
  {
   struct defmodule *theModule = (struct defmodule *) vTheModule;

   ListItemsDriver(logicalName,theModule,"activation","activations",
                   GetNextActivation,NULL,PrintActivation,NULL);
  }

/*******************************************************************/
/* RemoveActivation: Returns an activation and its associated data */
/*   structures to the Memory Manager. Links to other activations  */
/*   and partial matches may also be updated.                      */
/*******************************************************************/
globle void RemoveActivation(
  void *vTheActivation,
  int updateAgenda,
  int updateLinks)
  {
   struct defruleModule *theModuleItem;
   struct activation *theActivation = (struct activation *) vTheActivation;

   /*====================================*/
   /* Determine the module of the agenda */
   /* in which the activation is stored. */
   /*====================================*/

   theModuleItem = (struct defruleModule *) theActivation->theRule->header.whichModule;

   /*=================================*/
   /* Update the agenda if necessary. */
   /*=================================*/

   if (updateAgenda == TRUE)
     {
      /*===============================================*/
      /* Update the pointer links between activations. */
      /*===============================================*/

      if (theActivation->prev == NULL)
        {
         theModuleItem->agenda = theModuleItem->agenda->next;
         if (theModuleItem->agenda != NULL) theModuleItem->agenda->prev = NULL;
        }
      else
        {
         theActivation->prev->next = theActivation->next;
         if (theActivation->next != NULL)
           { theActivation->next->prev = theActivation->prev; }
        }

      /*===================================*/
      /* Indicate removal of activation if */
      /* activations are being watched.    */
      /*===================================*/

#if DEBUGGING_FUNCTIONS
      if (theActivation->theRule->watchActivation)
        {
         PrintRouter(WTRACE,"<== Activation ");
         PrintActivation(WTRACE,(void *) theActivation);
         PrintRouter(WTRACE,"\n");
        }
#endif

      /*=============================*/
      /* Mark the agenda as changed. */
      /*=============================*/

      AgendaChanged = TRUE;
     }

   /*============================================*/
   /* Update join and agenda links if necessary. */
   /*============================================*/

   if ((updateLinks == TRUE) && (theActivation->basis != NULL))
     { theActivation->basis->binds[theActivation->basis->bcount].gm.theValue = NULL; }

   /*================================================*/
   /* Return the activation to the free memory pool. */
   /*================================================*/

   NumberOfActivations--;

#if CONFLICT_RESOLUTION_STRATEGIES
   if (theActivation->sortedBasis != NULL)
     { ReturnPartialMatch(theActivation->sortedBasis); }
#endif

   rtn_struct(activation,theActivation);
  }

/**************************************************************/
/* AgendaClearFunction: Agenda clear routine for use with the */
/*   clear command. Resets the current time tag to zero.      */
/**************************************************************/
static void AgendaClearFunction()
  {
   CurrentTimetag = 0;
  }

/*************************************************/
/* RemoveAllActivations: Removes all activations */
/*   from the agenda of the current module.      */
/*************************************************/
globle void RemoveAllActivations()
  {
   struct activation *tempPtr, *theActivation;

   theActivation = GetDefruleModuleItem(NULL)->agenda;
   while (theActivation != NULL)
     {
      tempPtr = theActivation->next;
      RemoveActivation(theActivation,TRUE,TRUE);
      theActivation = tempPtr;
     }
  }

/*****************************************************************/
/* GetAgendaChanged: Returns the value of the boolean flag which */
/*   indicates whether any changes have been made to the agenda. */
/*****************************************************************/
globle int GetAgendaChanged()
  {
   return(AgendaChanged);
  }

/*****************************************************************/
/* SetAgendaChanged: Sets the value of the boolean flag which    */
/*   indicates whether any changes have been made to the agenda. */
/*****************************************************************/
globle void SetAgendaChanged(
  int value)
  {
   AgendaChanged = value;
  }

/*******************************************************/
/* ReorderAgenda: Completely reorders the agenda based */
/*   on the current conflict resolution strategy.      */
/*******************************************************/
globle void ReorderAgenda(
  void *vTheModule)
  {
   struct activation *theActivation, *tempPtr;
   struct defmodule *theModule = (struct defmodule *) vTheModule;
   int allModules = FALSE;
   struct defruleModule *theModuleItem;

   /*=============================================*/
   /* If the module specified is a NULL pointer,  */
   /* then every module has its agenda reordered. */
   /*=============================================*/

   if (theModule == NULL)
     {
      allModules = TRUE;
      theModule = (struct defmodule *) GetNextDefmodule(NULL);
     }

   /*========================*/
   /* Reorder the agenda(s). */
   /*========================*/

   for (;
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      /*=================================*/
      /* Get the list of activations and */
      /* remove them from the agenda.    */
      /*=================================*/

      theModuleItem = GetDefruleModuleItem(theModule);
      theActivation = theModuleItem->agenda;
      theModuleItem->agenda = NULL;

      /*=========================================*/
      /* Reorder the activations by placing them */
      /* back on the agenda one by one.          */
      /*=========================================*/

      while (theActivation != NULL)
        {
         tempPtr = theActivation->next;
         theActivation->next = NULL;
         theActivation->prev = NULL;
         PlaceActivation(&(theModuleItem->agenda),theActivation);
         theActivation = tempPtr;
        }

      /*===============================================*/
      /* Return if only one agenda is being reordered. */
      /*===============================================*/

      if (! allModules) return;
     }
  }

/****************************************************/
/* GetNumberOfActivations: Returns the value of the */
/*   total number of activations on all agendas.    */
/****************************************************/
globle long int GetNumberOfActivations()
  { return(NumberOfActivations); }

/******************************************************/
/* RefreshCommand: H/L Command for refreshing a rule. */
/*   Syntax: (refresh <defrule-name>)                 */
/******************************************************/
globle void RefreshCommand()
  {
   char *ruleName;
   void *rulePtr;

   /*===========================*/
   /* Get the name of the rule. */
   /*===========================*/

   ruleName = GetConstructName("refresh","rule name");
   if (ruleName == NULL) return;

   /*===============================*/
   /* Determine if the rule exists. */
   /*===============================*/

   rulePtr = FindDefrule(ruleName);
   if (rulePtr == NULL)
     {
      CantFindItemErrorMessage("defrule",ruleName);
      return;
     }

   /*===================*/
   /* Refresh the rule. */
   /*===================*/

   Refresh(rulePtr);
  }

/***********************************************************/
/* Refresh: Refreshes a defrule. Activations of the rule   */
/*   that have already been fired are added to the agenda. */
/***********************************************************/
globle BOOLEAN Refresh(
  void *theRule)
  {
   struct defrule *rulePtr;
   struct partialMatch *listOfMatches;

   /*====================================*/
   /* Refresh each disjunct of the rule. */
   /*====================================*/

   for (rulePtr = (struct defrule *) theRule;
        rulePtr != NULL;
        rulePtr = rulePtr->disjunct)
     {
      /*================================*/
      /* Check each partial match that  */
      /* satisfies the LHS of the rule. */
      /*================================*/

      for (listOfMatches = rulePtr->lastJoin->beta;
           listOfMatches != NULL;
           listOfMatches = listOfMatches->next)
        {
         /*=======================================================*/
         /* If the partial match is associated with an activation */
         /* (which it should always be) and it isn't associated   */
         /* with a not CE that still has matches, then place a    */
         /* new activation on the agenda if this partial match    */
         /* doesn't have an activation associated with it.        */
         /*=======================================================*/

         if ((listOfMatches->activationf) && (! listOfMatches->counterf))
           {
            if (listOfMatches->binds[listOfMatches->bcount].gm.theValue == NULL)
              { AddActivation(rulePtr,listOfMatches); }
           }
        }
     }

   return(TRUE);
  }

#if DYNAMIC_SALIENCE

/**********************************************/
/* RefreshAgendaCommand: H/L access routine   */
/*   for the refresh-agenda command.          */
/**********************************************/
globle void RefreshAgendaCommand()
  {
   int numArgs, error;
   struct defmodule *theModule;

   /*==============================================*/
   /* This function can have at most one argument. */
   /*==============================================*/

   if ((numArgs = ArgCountCheck("refresh-agenda",NO_MORE_THAN,1)) == -1) return;

   /*===============================================================*/
   /* If a module name is specified, then the agenda of that module */
   /* is refreshed. Otherwise, the agenda of the current module is  */
   /* refreshed.                                                    */
   /*===============================================================*/

   if (numArgs == 1)
     {
      theModule = GetModuleName("refresh-agenda",1,&error);
      if (error) return;
     }
   else
     { theModule = ((struct defmodule *) GetCurrentModule()); }

   /*===============================================*/
   /* Refresh the agenda of the appropriate module. */
   /*===============================================*/

   RefreshAgenda(theModule);
  }

/***************************************/
/* RefreshAgenda: C access routine for */
/*   the refresh-agenda command.       */
/***************************************/
globle void RefreshAgenda(
  void *vTheModule)
  {
   struct activation *theActivation;
   struct defmodule *theModule = (struct defmodule *) vTheModule;
   BOOLEAN oldValue;
   int allModules = FALSE;

   /*==========================*/
   /* Save the current module. */
   /*==========================*/

   SaveCurrentModule();

   /*=============================================*/
   /* If the module specified is a NULL pointer,  */
   /* then every module has its agenda refreshed. */
   /*=============================================*/

   if (theModule == NULL)
     {
      allModules = TRUE;
      theModule = (struct defmodule *) GetNextDefmodule(NULL);
     }

   /*=======================================================*/
   /* Remember the current setting for salience evaluation. */
   /* To perform the refresh, the when activated setting is */
   /* used to recompute the salience values.                */
   /*=======================================================*/

   oldValue = GetSalienceEvaluation();
   SetSalienceEvaluation(WHEN_ACTIVATED);

   /*========================*/
   /* Refresh the agenda(s). */
   /*========================*/

   for (;
        theModule != NULL;
        theModule = (struct defmodule *) GetNextDefmodule(theModule))
     {
      /*=========================================*/
      /* Change the current module to the module */
      /* of the agenda being refreshed.          */
      /*=========================================*/

      SetCurrentModule((void *) theModule);

      /*================================================================*/
      /* Recompute the salience values for the current module's agenda. */
      /*================================================================*/

      for (theActivation = (struct activation *) GetNextActivation(NULL);
           theActivation != NULL;
           theActivation = (struct activation *) GetNextActivation(theActivation))
        { theActivation->salience = EvaluateSalience(theActivation->theRule); }

      /*======================================================*/
      /* Reorder the agenda based on the new salience values. */
      /*======================================================*/

      ReorderAgenda(theModule);

      /*===============================================*/
      /* Return if only one agenda is being refreshed. */
      /*===============================================*/

      if (! allModules)
        {
         SetSalienceEvaluation(oldValue);
         RestoreCurrentModule();
         return;
        }
     }

   /*==========================================*/
   /* Restore the salience evaluation setting. */
   /*==========================================*/

   SetSalienceEvaluation(oldValue);

   /*=============================*/
   /* Restore the current module. */
   /*=============================*/

   RestoreCurrentModule();
  }

/*********************************************************/
/* SetSalienceEvaluationCommand: H/L Command for setting */
/*   the salience evaluation behavior.                   */
/*   Syntax: (set-salience-evaluation-behavior <symbol>) */
/*********************************************************/
globle SYMBOL_HN *SetSalienceEvaluationCommand()
  {
   DATA_OBJECT argPtr;
   char *argument, *oldValue;

   /*==================================================*/
   /* Get the current setting for salience evaluation. */
   /*==================================================*/

   oldValue = SalienceEvaluationName(GetSalienceEvaluation());

   /*=========================================*/
   /* This function expects a single argument */
   /* which must be a symbol.                 */
   /*=========================================*/

   if (ArgCountCheck("set-salience-evaluation",EXACTLY,1) == -1)
     { return((SYMBOL_HN *) AddSymbol(oldValue)); }

   if (ArgTypeCheck("set-salience-evaluation",1,SYMBOL,&argPtr) == FALSE)
     { return((SYMBOL_HN *) AddSymbol(oldValue)); }

   /*=============================================================*/
   /* The allowed symbols to pass as an argument to this function */
   /* are when-defined, when-activated, and every-cycle.          */
   /*=============================================================*/

   argument = DOToString(argPtr);

   if (strcmp(argument,"when-defined") == 0)
     { SetSalienceEvaluation(WHEN_DEFINED); }
   else if (strcmp(argument,"when-activated") == 0)
     { SetSalienceEvaluation(WHEN_ACTIVATED); }
   else if (strcmp(argument,"every-cycle") == 0)
     { SetSalienceEvaluation(EVERY_CYCLE); }
   else
     {
      ExpectedTypeError1("set-salience-evaluation",1,
      "symbol with value when-defined, when-activated, or every-cycle");
      return((SYMBOL_HN *) AddSymbol(oldValue));
     }

   /*=================================================*/
   /* Return the old setting for salience evaluation. */
   /*=================================================*/

   return((SYMBOL_HN *) AddSymbol(oldValue));
  }

/*********************************************************/
/* GetSalienceEvaluationCommand: H/L Command for getting */
/*   the salience evaluation behavior.                   */
/*   Syntax: (get-salience-evaluation-behavior)          */
/*********************************************************/
globle SYMBOL_HN *GetSalienceEvaluationCommand()
  {
   ArgCountCheck("get-salience-evaluation",EXACTLY,0);

   return((SYMBOL_HN *) AddSymbol(SalienceEvaluationName(GetSalienceEvaluation())));
  }

/*****************************************************************/
/* SalienceEvaluationName: Given the integer value corresponding */
/*   to a specified salience evaluation behavior, returns a      */
/*   character string of the behavior's name.                    */
/*****************************************************************/
static char *SalienceEvaluationName(
  int strategy)
  {
   char *sname;

   switch (strategy)
     {
      case WHEN_DEFINED:
        sname = "when-defined";
        break;
      case WHEN_ACTIVATED:
        sname = "when-activated";
        break;
      case EVERY_CYCLE:
        sname = "every-cycle";
        break;
      default:
        sname = "unknown";
        break;
     }

   return(sname);
  }

/***************************************************************/
/* GetSalienceEvaluation: Returns the value of current type of */
/*  salience evaluation (e.g., when defined, when activated,   */
/*  or every cycle).                                           */
/***************************************************************/
globle BOOLEAN GetSalienceEvaluation()
  { return(SalienceEvaluation); }

/*************************************************************/
/* SetSalienceEvaluation: Sets the value of the current type */
/*   of salience evaluation.                                 */
/*************************************************************/
globle BOOLEAN SetSalienceEvaluation(
  int value)
  {
   int ov;

   ov = SalienceEvaluation;
   SalienceEvaluation = value;
   return(ov);
  }

/*****************************************************************/
/* EvaluateSalience: Returns the salience value of the specified */
/*   defrule. If salience evaluation is currently set to         */
/*   when-defined, then the current value of the rule's salience */
/*   is returned. Otherwise the salience expression associated   */
/*   with the rule is reevaluated, the value is stored as the    */
/*   rule's current salience, and it is then returned.           */
/*****************************************************************/
static int EvaluateSalience(
 void *vPtr)
 {
  struct defrule *rPtr = (struct defrule *) vPtr;
  DATA_OBJECT salienceValue;
  int salience;

  /*==================================================*/
  /* If saliences are only being evaluated when rules */
  /* are defined, then just return the last salience  */
  /* value evaluated for the rule.                    */
  /*==================================================*/

  if (GetSalienceEvaluation() == WHEN_DEFINED)
    { return(rPtr->salience); }

  /*=================================================================*/
  /* If the rule's salience value was defined as an integer constant */
  /* (i.e., not an expression or global variable which could change  */
  /* on reevaluation), then just return the salience value computed  */
  /* for the rule when it was defined.                               */
  /*=================================================================*/

  if (rPtr->dynamicSalience == NULL) return(rPtr->salience);

  /*====================================================*/
  /* Reevaluate the rule's salience. If an error occurs */
  /* during evaluation, print an error message.         */
  /*====================================================*/

  SetEvaluationError(FALSE);
  if (EvaluateExpression(rPtr->dynamicSalience,&salienceValue))
    {
     SalienceInformationError("defrule",ValueToString(rPtr->header.name));
     return(rPtr->salience);
    }

  /*========================================*/
  /* The salience value must be an integer. */
  /*========================================*/

  if (salienceValue.type != INTEGER)
    {
     SalienceNonIntegerError();
     SalienceInformationError("defrule",ValueToString(rPtr->header.name));
     SetEvaluationError(TRUE);
     return(rPtr->salience);
    }

  /*==========================================*/
  /* The salience value must fall between the */
  /* minimum and maximum allowed values.      */
  /*==========================================*/

  salience = (int) ValueToLong(salienceValue.value);

  if ((salience > MAX_DEFRULE_SALIENCE) || (salience < MIN_DEFRULE_SALIENCE))
    {
     SalienceRangeError(MIN_DEFRULE_SALIENCE,MAX_DEFRULE_SALIENCE);
     SetEvaluationError(TRUE);
     SalienceInformationError("defrule",ValueToString(((struct defrule *) rPtr)->header.name));
     return(rPtr->salience);
    }

  /*===================================*/
  /* Store the new salience value with */
  /* the rule and return this value.   */
  /*===================================*/

  rPtr->salience = salience;
  return(rPtr->salience);
 }

#endif /* DYNAMIC_SALIENCE */

#if DEBUGGING_FUNCTIONS

/***********************************************/
/* AgendaCommand: Prints out the agenda of the */
/*   rules that are ready to fire.             */
/*   Syntax: (agenda)                          */
/***********************************************/
globle void AgendaCommand()
  {
   int numArgs, error;
   struct defmodule *theModule;

   /*==============================================*/
   /* This function can have at most one argument. */
   /*==============================================*/

   if ((numArgs = ArgCountCheck("agenda",NO_MORE_THAN,1)) == -1) return;

   /*===============================================================*/
   /* If a module name is specified, then the agenda of that module */
   /* is displayed. Otherwise, the agenda of the current module is  */
   /* displayed.                                                    */
   /*===============================================================*/

   if (numArgs == 1)
     {
      theModule = GetModuleName("agenda",1,&error);
      if (error) return;
     }
   else
     { theModule = ((struct defmodule *) GetCurrentModule()); }

   /*===============================================*/
   /* Display the agenda of the appropriate module. */
   /*===============================================*/

   Agenda(WDISPLAY,theModule);
  }

#endif /* DEBUGGING_FUNCTIONS */

#endif /* DEFRULE_CONSTRUCT */


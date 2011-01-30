   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                    ENGINE MODULE                    */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides functionality primarily associated with */
/*   the run and focus commands.                             */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Bebe Ly                                              */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _ENGINE_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include "setup.h"

#if DEFRULE_CONSTRUCT

#include "agenda.h"
#include "argacces.h"
#include "constant.h"
#include "factmngr.h"
#include "inscom.h"
#include "memalloc.h"
#include "modulutl.h"
#include "prccode.h"
#include "prcdrfun.h"
#include "proflfun.h"
#include "reteutil.h"
#include "retract.h"
#include "router.h"
#include "ruledlt.h"
#include "sysdep.h"
#include "utility.h"
#include "watch.h"

#include "engine.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static struct activation      *NextActivationToFire(void);
   static struct defmodule       *RemoveFocus(struct defmodule *);

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle struct defrule      *ExecutingRule = NULL;
   Thread globle BOOLEAN              HaltRules = FALSE;
#if LOGICAL_DEPENDENCIES
   Thread globle struct joinNode     *TheLogicalJoin = NULL;
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static struct callFunctionItem
                              *ListOfRunFunctions = NULL;
   Thread static struct focus        *CurrentFocus = NULL;
   Thread static int                  FocusChanged = FALSE;
#if DEBUGGING_FUNCTIONS
   Thread static int                  WatchStatistics = OFF;
   Thread static int                  WatchFocus = OFF;
#endif

/**********************************************/
/* Run: C access routine for the run command. */
/**********************************************/
globle long int Run(
  long int runLimit)
  {
   long int rulesFired = 0;
   DATA_OBJECT result;
   struct callFunctionItem *theRunFunction;
#if DEBUGGING_FUNCTIONS
   long int maxActivations = 0, sumActivations = 0;
#if DEFTEMPLATE_CONSTRUCT
   long int maxFacts = 0, sumFacts = 0;
#endif
#if OBJECT_SYSTEM
   long int maxInstances = 0, sumInstances = 0;
#endif
   double endTime, startTime = 0.0;
   long int tempValue;
#endif
   unsigned int i;
   Thread static int alreadyRunning = FALSE;
   struct patternEntity *theMatchingItem;
   struct partialMatch *theBasis;
   ACTIVATION *theActivation;
   char *ruleFiring;
#if PROFILING_FUNCTIONS
   struct profileFrameInfo profileFrame;
#endif

   /*=====================================================*/
   /* Make sure the run command is not already executing. */
   /*=====================================================*/

   if (alreadyRunning) return(0);
   alreadyRunning = TRUE;

   /*================================*/
   /* Set up statistics information. */
   /*================================*/

#if DEBUGGING_FUNCTIONS
   if (WatchStatistics)
     {
#if DEFTEMPLATE_CONSTRUCT
      maxFacts = GetNumberOfFacts();
      sumFacts = maxFacts;
#endif
#if OBJECT_SYSTEM
      maxInstances = GetGlobalNumberOfInstances();
      sumInstances = maxInstances;
#endif
      maxActivations = GetNumberOfActivations();
      sumActivations = maxActivations;
      startTime = gentime();
     }
#endif

   /*=============================*/
   /* Set up execution variables. */
   /*=============================*/

   if (CurrentEvaluationDepth == 0) SetHaltExecution(FALSE);
   HaltRules = FALSE;

   /*=====================================================*/
   /* Fire rules until the agenda is empty, the run limit */
   /* has been reached, or a rule execution error occurs. */
   /*=====================================================*/

   theActivation = NextActivationToFire();
   while ((theActivation != NULL) &&
          (runLimit != 0) &&
          (HaltExecution == FALSE) &&
          (HaltRules == FALSE))
     {
      /*===========================================*/
      /* Detach the activation from the agenda and */
      /* determine which rule is firing.           */
      /*===========================================*/

      DetachActivation(theActivation);
      ruleFiring = GetActivationName(theActivation);
      theBasis = (struct partialMatch *) GetActivationBasis(theActivation);
      ExecutingRule = (struct defrule *) GetActivationRule(theActivation);

      /*=============================================*/
      /* Update the number of rules that have fired. */
      /*=============================================*/

      rulesFired++;
      if (runLimit > 0) { runLimit--; }

      /*==================================*/
      /* If rules are being watched, then */
      /* print an information message.    */
      /*==================================*/

#if DEBUGGING_FUNCTIONS
      if (ExecutingRule->watchFiring)
        {
         char printSpace[60];

         sprintf(printSpace,"FIRE %4ld ",rulesFired);
         PrintRouter(WTRACE,printSpace);
         PrintRouter(WTRACE,ruleFiring);
         PrintRouter(WTRACE,": ");
         PrintPartialMatch(WTRACE,theBasis);
         PrintRouter(WTRACE,"\n");
        }
#endif

      /*=================================================*/
      /* Remove the link between the activation and the  */
      /* completed match for the rule. Set the busy flag */
      /* for the completed match to TRUE (so the match   */
      /* upon which our RHS variables are dependent is   */
      /* not deleted while our rule is firing). Set up   */
      /* the global pointers to the completed match for  */
      /* routines which do variable extractions.         */
      /*=================================================*/

      theBasis->binds[theBasis->bcount].gm.theValue = NULL;
      theBasis->busy = TRUE;

      GlobalLHSBinds = theBasis;
      GlobalRHSBinds = NULL;

      /*===================================================================*/
      /* Increment the count for each of the facts/objects associated with */
      /* the rule activation so that the facts/objects cannot be deleted   */
      /* by garbage collection while the rule is executing.                */
      /*===================================================================*/

      for (i = 0; i < theBasis->bcount; i++)
        {
         theMatchingItem = theBasis->binds[i].gm.theMatch->matchingItem;
         if (theMatchingItem != NULL)
           { (*theMatchingItem->theInfo->incrementBasisCount)(theMatchingItem); }
        }

      /*====================================================*/
      /* Execute the rule's right hand side actions. If the */
      /* rule has logical CEs, set up the pointer to the    */
      /* rules logical join so the assert command will      */
      /* attach the appropriate dependencies to the facts.  */
      /*====================================================*/

#if LOGICAL_DEPENDENCIES
      TheLogicalJoin = ExecutingRule->logicalJoin;
#endif
      CurrentEvaluationDepth++;
      SetEvaluationError(FALSE);
      ExecutingRule->executing = TRUE;

#if PROFILING_FUNCTIONS
      StartProfile(&profileFrame,
                   &ExecutingRule->header.usrData,
                   ProfileConstructs);
#endif

      EvaluateProcActions(ExecutingRule->header.whichModule->theModule,
                          ExecutingRule->actions,ExecutingRule->localVarCnt,
                          &result,NULL);

#if PROFILING_FUNCTIONS
      EndProfile(&profileFrame);
#endif

      ExecutingRule->executing = FALSE;
      SetEvaluationError(FALSE);
      CurrentEvaluationDepth--;
#if LOGICAL_DEPENDENCIES
      TheLogicalJoin = NULL;
#endif

      /*=====================================================*/
      /* If rule execution was halted, then print a message. */
      /*=====================================================*/

#if DEBUGGING_FUNCTIONS
      if ((HaltExecution) || (HaltRules && ExecutingRule->watchFiring))
#else
      if ((HaltExecution) || (HaltRules))
#endif

        {
         PrintErrorID("PRCCODE",4,FALSE);
         PrintRouter(WERROR,"Execution halted during the actions of defrule ");
         PrintRouter(WERROR,ruleFiring);
         PrintRouter(WERROR,".\n");
        }

      /*===================================================================*/
      /* Decrement the count for each of the facts/objects associated with */
      /* the rule activation. If the last match for the activation         */
      /* is from a not CE, then we need to make sure that the last         */
      /* match is an actual match for the CE and not a counter.            */
      /*===================================================================*/

      theBasis->busy = FALSE;

      for (i = 0; i < (theBasis->bcount - 1); i++)
        {
         theMatchingItem = theBasis->binds[i].gm.theMatch->matchingItem;
         if (theMatchingItem != NULL)
           { (*theMatchingItem->theInfo->decrementBasisCount)(theMatchingItem); }
        }

      i = theBasis->bcount - 1;
      if (theBasis->counterf == FALSE)
        {
         theMatchingItem = theBasis->binds[i].gm.theMatch->matchingItem;
         if (theMatchingItem != NULL)
           { (*theMatchingItem->theInfo->decrementBasisCount)(theMatchingItem); }
        }

      /*========================================*/
      /* Return the agenda node to free memory. */
      /*========================================*/

      RemoveActivation(theActivation,FALSE,FALSE);

      /*======================================*/
      /* Get rid of partial matches discarded */
      /* while executing the rule's RHS.      */
      /*======================================*/

      FlushGarbagePartialMatches();

      /*==================================*/
      /* Get rid of other garbage created */
      /* while executing the rule's RHS.  */
      /*==================================*/

      PeriodicCleanup(FALSE,TRUE);

      /*==========================*/
      /* Keep up with statistics. */
      /*==========================*/

#if DEBUGGING_FUNCTIONS
      if (WatchStatistics)
        {
#if DEFTEMPLATE_CONSTRUCT
         tempValue = GetNumberOfFacts();
         if (tempValue > maxFacts) maxFacts = tempValue;
         sumFacts += tempValue;
#endif
#if OBJECT_SYSTEM
         tempValue = GetGlobalNumberOfInstances();
         if (tempValue > maxInstances) maxInstances = tempValue;
         sumInstances += tempValue;
#endif
         tempValue = GetNumberOfActivations();
         if (tempValue > maxActivations) maxActivations = tempValue;
         sumActivations += tempValue;
        }
#endif

      /*==================================*/
      /* Update saliences if appropriate. */
      /*==================================*/

#if DYNAMIC_SALIENCE
      if (GetSalienceEvaluation() == EVERY_CYCLE) RefreshAgenda(NULL);
#endif

      /*========================================*/
      /* Execute the list of functions that are */
      /* to be called after each rule firing.   */
      /*========================================*/

      for (theRunFunction = ListOfRunFunctions;
           theRunFunction != NULL;
           theRunFunction = theRunFunction->next)
        { (*theRunFunction->func)(); }

      /*========================================*/
      /* If a return was issued on the RHS of a */
      /* rule, then remove *that* rule's module */
      /* from the focus stack                   */
      /*========================================*/

      if (ReturnFlag == TRUE)
        { RemoveFocus(ExecutingRule->header.whichModule->theModule); }
      ReturnFlag = FALSE;

      /*========================================*/
      /* Determine the next activation to fire. */
      /*========================================*/

      theActivation = (struct activation *) NextActivationToFire();

      /*==============================*/
      /* Check for a rule breakpoint. */
      /*==============================*/

      if (theActivation != NULL)
        {
         if (((struct defrule *) GetActivationRule(theActivation))->afterBreakpoint)
           {
            HaltRules = TRUE;
            PrintRouter(WDIALOG,"Breaking on rule ");
            PrintRouter(WDIALOG,GetActivationName(theActivation));
            PrintRouter(WDIALOG,".\n");
           }
        }
     }

   /*=====================================================*/
   /* Make sure run functions are executed at least once. */
   /*=====================================================*/

   if (rulesFired == 0)
     {
      for (theRunFunction = ListOfRunFunctions;
           theRunFunction != NULL;
           theRunFunction = theRunFunction->next)
        { (*theRunFunction->func)(); }
     }

   /*======================================================*/
   /* If rule execution was halted because the rule firing */
   /* limit was reached, then print a message.             */
   /*======================================================*/

   if (runLimit == rulesFired)
     { PrintRouter(WDIALOG,"rule firing limit reached\n"); }

   /*==============================*/
   /* Restore execution variables. */
   /*==============================*/

   ExecutingRule = NULL;
   HaltRules = FALSE;

   /*=================================================*/
   /* Print out statistics if they are being watched. */
   /*=================================================*/

#if DEBUGGING_FUNCTIONS
   if (WatchStatistics)
     {
      char printSpace[60];

      endTime = gentime();

      PrintLongInteger(WDIALOG,rulesFired);
      PrintRouter(WDIALOG," rules fired");

#if (! GENERIC)
      if (startTime != endTime)
        {
         PrintRouter(WDIALOG,"        Run time is ");
         PrintFloat(WDIALOG,endTime - startTime);
         PrintRouter(WDIALOG," seconds.\n");
         PrintFloat(WDIALOG,(double) rulesFired / (endTime - startTime));
         PrintRouter(WDIALOG," rules per second.\n");
        }
      else
        { PrintRouter(WDIALOG,"\n"); }
#endif

#if DEFTEMPLATE_CONSTRUCT
      sprintf(printSpace,"%ld mean number of facts (%ld maximum).\n",
                          (long) (((double) sumFacts / (rulesFired + 1)) + 0.5),
                          maxFacts);
      PrintRouter(WDIALOG,printSpace);
#endif

#if OBJECT_SYSTEM
      sprintf(printSpace,"%ld mean number of instances (%ld maximum).\n",
                          (long) (((double) sumInstances / (rulesFired + 1)) + 0.5),
                          maxInstances);
      PrintRouter(WDIALOG,printSpace);
#endif

      sprintf(printSpace,"%ld mean number of activations (%ld maximum).\n",
                          (long) (((double) sumActivations / (rulesFired + 1)) + 0.5),
                          maxActivations);
      PrintRouter(WDIALOG,printSpace);
     }
#endif

   /*==========================================*/
   /* The current module should be the current */
   /* focus when the run finishes.             */
   /*==========================================*/

   if (CurrentFocus != NULL)
     {
      if (CurrentFocus->theModule != ((struct defmodule *) GetCurrentModule()))
        { SetCurrentModule((void *) CurrentFocus->theModule); }
     }

   /*===================================*/
   /* Return the number of rules fired. */
   /*===================================*/

   alreadyRunning = FALSE;
   return(rulesFired);
  }

/***********************************************************/
/* NextActivationToFire: Returns the next activation which */
/*   should be executed based on the current focus.        */
/***********************************************************/
static struct activation *NextActivationToFire()
  {
   struct activation *theActivation;
   struct defmodule *theModule;

   /*====================================*/
   /* If there is no current focus, then */
   /* focus on the MAIN module.          */
   /*====================================*/

   if (CurrentFocus == NULL)
     {
      theModule = (struct defmodule *) FindDefmodule("MAIN");
      Focus(theModule);
     }

   /*===========================================================*/
   /* Determine the top activation on the agenda of the current */
   /* focus. If the current focus has no activations on its     */
   /* agenda, then pop the focus off the focus stack until      */
   /* a focus that has an activation on its agenda is found.    */
   /*===========================================================*/

   theActivation = CurrentFocus->theDefruleModule->agenda;
   while ((theActivation == NULL) && (CurrentFocus != NULL))
     {
      if (CurrentFocus != NULL) PopFocus();
      if (CurrentFocus != NULL) theActivation = CurrentFocus->theDefruleModule->agenda;
     }

   /*=========================================*/
   /* Return the next activation to be fired. */
   /*=========================================*/

   return(theActivation);
  }

/***************************************************/
/* RemoveFocus: Removes the first occurence of the */
/*   specified module from the focus stack.        */
/***************************************************/
static struct defmodule *RemoveFocus(
  struct defmodule *theModule)
  {
   struct focus *tempFocus,*prevFocus, *nextFocus;
   int found = FALSE;
   int currentFocusRemoved = FALSE;

   /*====================================*/
   /* Return NULL if there is nothing on */
   /* the focus stack to remove.         */
   /*====================================*/

   if (CurrentFocus == NULL) return(NULL);

   /*=============================================*/
   /* Remove the first occurence of the specified */
   /* module from the focus stack.                */
   /*=============================================*/

   prevFocus = NULL;
   tempFocus = CurrentFocus;
   while ((tempFocus != NULL) && (! found))
     {
      if (tempFocus->theModule == theModule)
        {
         found = TRUE;

         nextFocus = tempFocus->next;
         rtn_struct(focus,tempFocus);
         tempFocus = nextFocus;

         if (prevFocus == NULL)
           {
            currentFocusRemoved = TRUE;
            CurrentFocus = tempFocus;
           }
         else
           { prevFocus->next = tempFocus; }
        }
      else
        {
         prevFocus = tempFocus;
         tempFocus = tempFocus->next;
        }
     }

   /*=========================================*/
   /* If the given module is not in the focus */
   /* stack, simply return the current focus  */
   /*=========================================*/

   if (! found) return(CurrentFocus->theModule);

   /*========================================*/
   /* If the current focus is being watched, */
   /* then print an informational message.   */
   /*========================================*/

#if DEBUGGING_FUNCTIONS
   if (WatchFocus)
     {
      PrintRouter(WTRACE,"<== Focus ");
      PrintRouter(WTRACE,ValueToString(theModule->name));

      if ((CurrentFocus != NULL) && currentFocusRemoved)
        {
         PrintRouter(WTRACE," to ");
         PrintRouter(WTRACE,ValueToString(CurrentFocus->theModule->name));
        }

      PrintRouter(WTRACE,"\n");
     }
#endif

   /*======================================================*/
   /* Set the current module to the module associated with */
   /* the current focus (if it changed) and set a boolean  */
   /* flag indicating that the focus has changed.          */
   /*======================================================*/

   if ((CurrentFocus != NULL) && currentFocusRemoved)
     { SetCurrentModule((void *) CurrentFocus->theModule); }
   FocusChanged = TRUE;

   /*====================================*/
   /* Return the module that was removed */
   /* from the focus stack.              */
   /*====================================*/

   return(theModule);
  }

/**********************************************************/
/* PopFocus: C access routine for the pop-focus function. */
/**********************************************************/
globle void *PopFocus()
  {
   if (CurrentFocus == NULL) return(NULL);
   return((void *) RemoveFocus(CurrentFocus->theModule));
  }

/*************************************************************/
/* GetNextFocus: Returns the next focus on the focus stack. */
/*************************************************************/
globle void *GetNextFocus(
  void *theFocus)
  {
   /*==================================================*/
   /* If NULL is passed as an argument, return the top */
   /* focus on the focus stack (the current focus).    */
   /*==================================================*/

   if (theFocus == NULL) return((void *) CurrentFocus);

   /*=======================================*/
   /* Otherwise, return the focus following */
   /* the focus passed as an argument.      */
   /*=======================================*/

   return((void *) ((struct focus *) theFocus)->next);
  }

/***************************************************/
/* Focus: C access routine for the focus function. */
/***************************************************/
globle void Focus(
  void *vTheModule)
  {
   struct defmodule *theModule = (struct defmodule *) vTheModule;
   struct focus *tempFocus;

   /*==================================================*/
   /* Make the specified module be the current module. */
   /* If the specified module is the current focus,    */
   /* then no further action is needed.                */
   /*==================================================*/

   SetCurrentModule((void *) theModule);
   if (CurrentFocus != NULL)
     { if (CurrentFocus->theModule == theModule) return; }

   /*=====================================*/
   /* If the focus is being watched, then */
   /* print an information message.       */
   /*=====================================*/

#if DEBUGGING_FUNCTIONS
   if (WatchFocus)
     {
      PrintRouter(WTRACE,"==> Focus ");
      PrintRouter(WTRACE,ValueToString(theModule->name));
      if (CurrentFocus != NULL)
        {
         PrintRouter(WTRACE," from ");
         PrintRouter(WTRACE,ValueToString(CurrentFocus->theModule->name));
        }
      PrintRouter(WTRACE,"\n");
     }
#endif

   /*=======================================*/
   /* Add the new focus to the focus stack. */
   /*=======================================*/

   tempFocus = get_struct(focus);
   tempFocus->theModule = theModule;
   tempFocus->theDefruleModule = GetDefruleModuleItem(theModule);
   tempFocus->next = CurrentFocus;
   CurrentFocus = tempFocus;
   FocusChanged = TRUE;
  }

/************************************************/
/* ClearFocusStackCommand: H/L access routine   */
/*   for the clear-focus-stack command.         */
/************************************************/
globle void ClearFocusStackCommand()
  {
   if (ArgCountCheck("list-focus-stack",EXACTLY,0) == -1) return;

   ClearFocusStack();
  }

/****************************************/
/* ClearFocusStack: C access routine    */
/*   for the clear-focus-stack command. */
/****************************************/
globle void ClearFocusStack()
  {
   while (CurrentFocus != NULL) PopFocus();

   FocusChanged = TRUE;
  }

/**************************************************************/
/* AddRunFunction: Adds a function to the ListOfRunFunctions. */
/**************************************************************/
globle BOOLEAN AddRunFunction(
  char *name,
  void (*functionPtr)(void),
  int priority)
  {
   ListOfRunFunctions = AddFunctionToCallList(name,priority,
                                              functionPtr,
                                              ListOfRunFunctions);
   return(1);
  }

/**********************************************************************/
/* RemoveRunFunction: Removes a function from the ListOfRunFunctions. */
/**********************************************************************/
globle BOOLEAN RemoveRunFunction(
  char *name)
  {
   int found;

   ListOfRunFunctions = RemoveFunctionFromCallList(name,ListOfRunFunctions,&found);

   if (found) return(TRUE);

   return(FALSE);
  }

/*****************************************************************************/
/* InitializeEngine: Initializes the activations and statistics watch items. */
/*****************************************************************************/
globle void InitializeEngine()
  {
#if DEBUGGING_FUNCTIONS
   AddWatchItem("statistics",0,&WatchStatistics,20,NULL,NULL);
   AddWatchItem("focus",0,&WatchFocus,0,NULL,NULL);
#endif
  }

/*********************************************************/
/* RunCommand: H/L access routine for the run command.   */
/*********************************************************/
globle void RunCommand()
  {
   int numArgs;
   long int runLimit = -1;
   DATA_OBJECT argPtr;

   if ((numArgs = ArgCountCheck("run",NO_MORE_THAN,1)) == -1) return;

   if (numArgs == 0)
     { runLimit = -1; }
   else if (numArgs == 1)
     {
      if (ArgTypeCheck("run",1,INTEGER,&argPtr) == FALSE) return;
      runLimit = DOToLong(argPtr);
     }

   Run(runLimit);

   return;
  }

/***********************************************/
/* HaltCommand: Causes rule execution to halt. */
/***********************************************/
globle void HaltCommand()
  {
   ArgCountCheck("halt",EXACTLY,0);
   HaltRules = TRUE;
  }

#if DEBUGGING_FUNCTIONS

/*********************************************************/
/* SetBreak: C access routine for the set-break command. */
/*********************************************************/
globle void SetBreak(
  void *theRule)
  {
   struct defrule *thePtr;

   for (thePtr = (struct defrule *) theRule;
        thePtr != NULL;
        thePtr = thePtr->disjunct)
     { thePtr->afterBreakpoint = 1; }
  }

/***************************************************************/
/* RemoveBreak: C access routine for the remove-break command. */
/***************************************************************/
globle BOOLEAN RemoveBreak(
  void *theRule)
  {
   struct defrule *thePtr;
   int rv = FALSE;

   for (thePtr = (struct defrule *) theRule;
        thePtr != NULL;
        thePtr = thePtr->disjunct)
     {
      if (thePtr->afterBreakpoint == 1)
        {
         thePtr->afterBreakpoint = 0;
         rv = TRUE;
        }
     }

   return(rv);
  }

/**************************************************/
/* RemoveAllBreakpoints: Removes all breakpoints. */
/**************************************************/
globle void RemoveAllBreakpoints()
  {
   void *theRule;
   void *theDefmodule = NULL;

   while ((theDefmodule = GetNextDefmodule(theDefmodule)) != NULL)
     {
      theRule = NULL;
      while ((theRule = GetNextDefrule(theRule)) != NULL)
        { RemoveBreak(theRule); }
     }
  }

/*************************************************************/
/* ShowBreaks: C access routine for the show-breaks command. */
/*************************************************************/
globle void ShowBreaks(
  char *logicalName,
  void *vTheModule)
  {
   ListItemsDriver(logicalName,(struct defmodule *) vTheModule,
                   NULL,NULL,
                   GetNextDefrule,(char *(*)(void *)) GetConstructNameString,
                   NULL,DefruleHasBreakpoint);
   }

/************************************************************************************/
/* DefruleHasBreakpoint: Indicates whether the specified rule has a breakpoint set. */
/************************************************************************************/
globle BOOLEAN DefruleHasBreakpoint(
  void *theRule)
  {
   return(((struct defrule *) theRule)->afterBreakpoint);
  }

/*****************************************/
/* SetBreakCommand: H/L access routine   */
/*   for the set-break command.          */
/*****************************************/
globle void SetBreakCommand()
  {
   DATA_OBJECT argPtr;
   char *argument;
   void *defrulePtr;

   if (ArgCountCheck("set-break",EXACTLY,1) == -1) return;

   if (ArgTypeCheck("set-break",1,SYMBOL,&argPtr) == FALSE) return;

   argument = DOToString(argPtr);

   if ((defrulePtr = FindDefrule(argument)) == NULL)
     {
      CantFindItemErrorMessage("defrule",argument);
      return;
     }

   SetBreak(defrulePtr);
  }

/********************************************/
/* RemoveBreakCommand: H/L access routine   */
/*   for the remove-break command.          */
/********************************************/
globle void RemoveBreakCommand()
  {
   DATA_OBJECT argPtr;
   char *argument;
   int nargs;
   void *defrulePtr;

   if ((nargs = ArgCountCheck("remove-break",NO_MORE_THAN,1)) == -1)
     { return; }

   if (nargs == 0)
     {
      RemoveAllBreakpoints();
      return;
     }

   if (ArgTypeCheck("remove-break",1,SYMBOL,&argPtr) == FALSE) return;

   argument = DOToString(argPtr);

   if ((defrulePtr = FindDefrule(argument)) == NULL)
     {
      CantFindItemErrorMessage("defrule",argument);
      return;
     }

   if (RemoveBreak(defrulePtr) == FALSE)
     {
      PrintRouter(WERROR,"Rule ");
      PrintRouter(WERROR,argument);
      PrintRouter(WERROR," does not have a breakpoint set.\n");
     }
  }

/*******************************************/
/* ShowBreaksCommand: H/L access routine   */
/*   for the show-breaks command.          */
/*******************************************/
globle void ShowBreaksCommand()
  {
   int numArgs, error;
   struct defmodule *theModule;

   if ((numArgs = ArgCountCheck("show-breaks",NO_MORE_THAN,1)) == -1) return;

   if (numArgs == 1)
     {
      theModule = GetModuleName("show-breaks",1,&error);
      if (error) return;
     }
   else
     { theModule = ((struct defmodule *) GetCurrentModule()); }

   ShowBreaks(WDISPLAY,theModule);
  }

/***********************************************/
/* ListFocusStackCommand: H/L access routine   */
/*   for the list-focus-stack command.         */
/***********************************************/
globle void ListFocusStackCommand()
  {
   if (ArgCountCheck("list-focus-stack",EXACTLY,0) == -1) return;

   ListFocusStack(WDISPLAY);
  }

/****************************************/
/* ListFocusStack: C access routine for */
/*   the list-focus-stack command.      */
/****************************************/
globle void ListFocusStack(
  char *logicalName)
  {
   struct focus *theFocus;

   for (theFocus = CurrentFocus;
        theFocus != NULL;
        theFocus = theFocus->next)
     {
      PrintRouter(logicalName,GetDefmoduleName(theFocus->theModule));
      PrintRouter(logicalName,"\n");
     }
  }

#endif

/***********************************************/
/* GetFocusStackFunction: H/L access routine   */
/*   for the get-focus-stack function.         */
/***********************************************/
globle void GetFocusStackFunction(
  DATA_OBJECT_PTR returnValue)
  {
   if (ArgCountCheck("get-focus-stack",EXACTLY,0) == -1) return;

   GetFocusStack(returnValue);
  }

/***************************************/
/* GetFocusStack: C access routine for */
/*   the get-focus-stack function.     */
/***************************************/
globle void GetFocusStack(
  DATA_OBJECT_PTR returnValue)
  {
   struct focus *theFocus;
   struct multifield *theList;
   long count = 0;

   /*===========================================*/
   /* If there is no current focus, then return */
   /* a multifield value of length zero.        */
   /*===========================================*/

   if (CurrentFocus == NULL)
     {
      SetpType(returnValue,MULTIFIELD);
      SetpDOBegin(returnValue,1);
      SetpDOEnd(returnValue,0);
      SetpValue(returnValue,(void *) CreateMultifield(0L));
      return;
     }

   /*=====================================================*/
   /* Determine the number of modules on the focus stack. */
   /*=====================================================*/

   for (theFocus = CurrentFocus; theFocus != NULL; theFocus = theFocus->next)
     { count++; }

   /*=============================================*/
   /* Create a multifield of the appropriate size */
   /* in which to store the module names.         */
   /*=============================================*/

   SetpType(returnValue,MULTIFIELD);
   SetpDOBegin(returnValue,1);
   SetpDOEnd(returnValue,count);
   theList = (struct multifield *) CreateMultifield(count);
   SetpValue(returnValue,(void *) theList);

   /*=================================================*/
   /* Store the module names in the multifield value. */
   /*=================================================*/

   for (theFocus = CurrentFocus, count = 1;
        theFocus != NULL;
        theFocus = theFocus->next, count++)
     {
      SetMFType(theList,count,SYMBOL);
      SetMFValue(theList,count,theFocus->theModule->name);
     }
  }

/******************************************/
/* PopFocusFunction: H/L access routine   */
/*   for the pop-focus function.          */
/******************************************/
globle SYMBOL_HN *PopFocusFunction()
  {
   struct defmodule *theModule;

   ArgCountCheck("pop-focus",EXACTLY,0);

   theModule = (struct defmodule *) PopFocus();
   if (theModule == NULL) return((SYMBOL_HN *) FalseSymbol);
   return(theModule->name);
  }

/******************************************/
/* GetFocusFunction: H/L access routine   */
/*   for the get-focus function.          */
/******************************************/
globle SYMBOL_HN *GetFocusFunction()
  {
   struct defmodule *rv;

   ArgCountCheck("get-focus",EXACTLY,0);
   rv = (struct defmodule *) WRGetFocus();
   if (rv == NULL) return((SYMBOL_HN *) FalseSymbol);
   return(rv->name);
  }

/**********************************/
/* GetFocus: C access routine for */
/*   the get-focus function.      */
/**********************************/
globle void *WRGetFocus()
  {
   if (CurrentFocus == NULL) return(NULL);

   return((void *) CurrentFocus->theModule);
  }

/**************************************/
/* FocusCommand: H/L access routine   */
/*   for the focus function.          */
/**************************************/
globle int FocusCommand()
  {
   DATA_OBJECT argPtr;
   char *argument;
   struct defmodule *theModule;
   int argCount, i;

   /*=====================================================*/
   /* Check for the correct number and type of arguments. */
   /*=====================================================*/

   if ((argCount = ArgCountCheck("focus",AT_LEAST,1)) == -1)
     { return(FALSE); }

   /*===========================================*/
   /* Focus on the specified defrule module(s). */
   /*===========================================*/

   for (i = argCount; i > 0; i--)
     {
      if (ArgTypeCheck("focus",i,SYMBOL,&argPtr) == FALSE)
        { return(FALSE); }

      argument = DOToString(argPtr);
      theModule = (struct defmodule *) FindDefmodule(argument);

      if (theModule == NULL)
        {
         CantFindItemErrorMessage("defmodule",argument);
         return(FALSE);
        }

      Focus((void *) theModule);
     }

   /*===================================================*/
   /* Return TRUE to indicate success of focus command. */
   /*===================================================*/

   return(TRUE);
  }

/********************************************************************/
/* GetFocusChanged: Returns the value of the variable FocusChanged. */
/********************************************************************/
globle int GetFocusChanged()
  {
   return(FocusChanged);
  }

/*****************************************************************/
/* SetFocusChanged: Sets the value of the variable FocusChanged. */
/*****************************************************************/
globle void SetFocusChanged(
  int value)
  {
   FocusChanged = value;
  }

#endif /* DEFRULE_CONSTRUCT */


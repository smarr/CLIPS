   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*                 AGENDA HEADER FILE                  */
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
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_agenda

#define _H_agenda

#ifndef _H_ruledef
#include "ruledef.h"
#endif
#ifndef _H_symbol
#include "symbol.h"
#endif
#ifndef _H_match
#include "match.h"
#endif

#define WHEN_DEFINED 0
#define WHEN_ACTIVATED 1
#define EVERY_CYCLE 2

#define MAX_DEFRULE_SALIENCE  10000
#define MIN_DEFRULE_SALIENCE -10000

/*******************/
/* DATA STRUCTURES */
/*******************/

struct activation
  {
   struct defrule *theRule;
   struct partialMatch *basis;
   int salience;
   unsigned long int timetag;
#if CONFLICT_RESOLUTION_STRATEGIES
   struct partialMatch *sortedBasis;
   int randomID;
#endif
   struct activation *prev;
   struct activation *next;
  };

typedef struct activation ACTIVATION;

#define GetActivationSalience(actPtr) (((struct activation *) actPtr)->salience)
#define GetActivationRule(actPtr) (((struct activation *) actPtr)->theRule)
#define GetActivationBasis(actPtr) (((struct activation *) actPtr)->basis)
#define GetActivationSortedBasis(actPtr) (((struct activation *) actPtr)->sortedBasis)

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _AGENDA_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   LOCALE void                    AddActivation(void *,void *);
   LOCALE void                    ClearRuleFromAgenda(void *);
   LOCALE DllExport void         *GetNextActivation(void *);
   LOCALE char                   *GetActivationName(void *);
   LOCALE int                     SetActivationSalience(void *,int);
   LOCALE void                    GetActivationPPForm(char *,int,void *);
   LOCALE BOOLEAN                 MoveActivationToTop(void *);
   LOCALE BOOLEAN                 DeleteActivation(void *);
   LOCALE BOOLEAN                 DetachActivation(void *);
   LOCALE DllExport void          Agenda(char *,void *);
   LOCALE void                    RemoveActivation(void *,int,int);
   LOCALE void                    RemoveAllActivations(void);
   LOCALE int                     GetAgendaChanged(void);
   LOCALE void                    SetAgendaChanged(int);
   LOCALE long int                GetNumberOfActivations(void);
   LOCALE BOOLEAN                 GetSalienceEvaluation(void);
   LOCALE BOOLEAN                 SetSalienceEvaluation(BOOLEAN);
   LOCALE void                    RefreshAgenda(void *);
   LOCALE void                    ReorderAgenda(void *);
   LOCALE void                    InitializeAgenda(void);
   LOCALE SYMBOL_HN              *SetSalienceEvaluationCommand(void);
   LOCALE SYMBOL_HN              *GetSalienceEvaluationCommand(void);
   LOCALE void                    RefreshAgendaCommand(void);
   LOCALE void                    RefreshCommand(void);
   LOCALE BOOLEAN                 Refresh(void *);
#if DEBUGGING_FUNCTIONS
   LOCALE void                    AgendaCommand(void);
#endif

#ifndef _AGENDA_SOURCE_
#if DEBUGGING_FUNCTIONS
   extern Thread BOOLEAN              WatchActivations;
#endif
#endif

#endif


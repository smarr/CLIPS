   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*         CONFLICT RESOLUTION STRATEGY MODULE         */
   /*******************************************************/

/*************************************************************/
/* Purpose: Used to determine where a new activation is      */
/*   placed on the agenda based on the current conflict      */
/*   resolution strategy (depth, breadth, mea, lex,          */
/*   simplicity, or complexity). Also provides the           */
/*   set-strategy and get-strategy commands.                 */
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

#define _CRSTRTGY_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include "setup.h"

#if DEFRULE_CONSTRUCT

#include "constant.h"
#include "pattern.h"
#include "reteutil.h"
#include "argacces.h"
#include "agenda.h"
#include "crstrtgy.h"

#define GetMatchingItem(x,i) (x->basis->binds[i].gm.theMatch->matchingItem)

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static ACTIVATION             *PlaceDepthActivation(ACTIVATION *,ACTIVATION *);
#if CONFLICT_RESOLUTION_STRATEGIES
   static ACTIVATION             *PlaceBreadthActivation(ACTIVATION *,ACTIVATION *);
   static ACTIVATION             *PlaceLEXActivation(ACTIVATION *,ACTIVATION *);
   static ACTIVATION             *PlaceMEAActivation(ACTIVATION *,ACTIVATION *);
   static ACTIVATION             *PlaceComplexityActivation(ACTIVATION *,ACTIVATION *);
   static ACTIVATION             *PlaceSimplicityActivation(ACTIVATION *,ACTIVATION *);
   static ACTIVATION             *PlaceRandomActivation(ACTIVATION *,ACTIVATION *);
   static struct partialMatch    *SortPartialMatch(struct partialMatch *);
   static int                     ComparePartialMatches(ACTIVATION *,ACTIVATION *);
   static char                   *GetStrategyName(int);
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

#if CONFLICT_RESOLUTION_STRATEGIES
   Thread static int                  Strategy = DEFAULT_STRATEGY;
#endif

/******************************************************************/
/* PlaceActivation: Coordinates placement of an activation on the */
/*   Agenda based on the current conflict resolution strategy.    */
/******************************************************************/
globle void PlaceActivation(
  ACTIVATION **whichAgenda,
  ACTIVATION *newActivation)
  {
   ACTIVATION *placeAfter = NULL;

   /*================================================*/
   /* Set the flag which indicates that a change has */
   /* been made to the agenda.                       */
   /*================================================*/

   SetAgendaChanged(TRUE);

   /*=============================================*/
   /* Determine the location where the activation */
   /* should be placed in the agenda based on the */
   /* current conflict resolution strategy.       */
   /*==============================================*/

#if ! CONFLICT_RESOLUTION_STRATEGIES
   if (*whichAgenda != NULL) placeAfter = PlaceDepthActivation(*whichAgenda,newActivation);
#else
   if (*whichAgenda != NULL) switch (Strategy)
     {
      case DEPTH_STRATEGY:
        placeAfter = PlaceDepthActivation(*whichAgenda,newActivation);
        break;

      case BREADTH_STRATEGY:
        placeAfter = PlaceBreadthActivation(*whichAgenda,newActivation);
        break;

      case LEX_STRATEGY:
        placeAfter = PlaceLEXActivation(*whichAgenda,newActivation);
        break;

      case MEA_STRATEGY:
        placeAfter = PlaceMEAActivation(*whichAgenda,newActivation);
        break;

      case COMPLEXITY_STRATEGY:
        placeAfter = PlaceComplexityActivation(*whichAgenda,newActivation);
        break;

      case SIMPLICITY_STRATEGY:
        placeAfter = PlaceSimplicityActivation(*whichAgenda,newActivation);
        break;

      case RANDOM_STRATEGY:
        placeAfter = PlaceRandomActivation(*whichAgenda,newActivation);
        break;
     }
#endif

   /*==============================================================*/
   /* Place the activation at the appropriate place in the agenda. */
   /*==============================================================*/

   if (placeAfter == NULL) /* then place it at the beginning of then agenda. */
     {
      newActivation->next = *whichAgenda;
      *whichAgenda = newActivation;
      if (newActivation->next != NULL) newActivation->next->prev = newActivation;
     }
   else /* insert it in the agenda. */
     {
      newActivation->next = placeAfter->next;
      newActivation->prev = placeAfter;
      placeAfter->next = newActivation;
      if (newActivation->next != NULL)
        { newActivation->next->prev = newActivation; }
     }
  }

/*******************************************************************/
/* PlaceDepthActivation: Determines the location in the agenda     */
/*    where a new activation should be placed for the depth        */
/*    strategy. Returns a pointer to the activation after which    */
/*    the new activation should be placed (or NULL if the          */
/*    activation should be placed at the beginning of the agenda). */
/*******************************************************************/
static ACTIVATION *PlaceDepthActivation(
  ACTIVATION *actPtr,
  ACTIVATION *newActivation)
  {
   int salience;
   unsigned long timetag;
   ACTIVATION *lastAct;

   /*============================================*/
   /* Set up initial information for the search. */
   /*============================================*/

   salience = newActivation->salience;
   timetag = newActivation->timetag;
   lastAct = NULL;

   /*=========================================================*/
   /* Find the insertion point in the agenda. The activation  */
   /* is placed before activations of lower salience and      */
   /* after activations of higher salience. Among activations */
   /* of equal salience, the activation is placed before      */
   /* activations with an equal or lower timetag (yielding    */
   /* depth first traversal).                                 */
   /*=========================================================*/

   while (actPtr != NULL)
     {
      if (actPtr->salience > salience)
        {
         lastAct = actPtr;
         actPtr = actPtr->next;
        }
      else if (actPtr->salience < salience)
        { return(lastAct); }
      else if (timetag < actPtr->timetag)
        {
         lastAct = actPtr;
         actPtr = actPtr->next;
        }
      else
        { return(lastAct); }
     }

   /*===========================================*/
   /* Return the insertion point in the agenda. */
   /*===========================================*/

   return(lastAct);
  }

#if CONFLICT_RESOLUTION_STRATEGIES
/*******************************************************************/
/* PlaceBreadthActivation: Determines the location in the agenda   */
/*    where a new activation should be placed for the breadth      */
/*    strategy. Returns a pointer to the activation after which    */
/*    the new activation should be placed (or NULL if the          */
/*    activation should be placed at the beginning of the agenda). */
/*******************************************************************/
static ACTIVATION *PlaceBreadthActivation(
  ACTIVATION *actPtr,
  ACTIVATION *newActivation)
  {
   int salience;
   unsigned long timetag;
   ACTIVATION *lastAct;

   /*============================================*/
   /* Set up initial information for the search. */
   /*============================================*/

   salience = newActivation->salience;
   timetag = newActivation->timetag;
   lastAct = NULL;

   /*=========================================================*/
   /* Find the insertion point in the agenda. The activation  */
   /* is placed before activations of lower salience and      */
   /* after activations of higher salience. Among activations */
   /* of equal salience, the activation is placed after       */
   /* activations with a lessor timetag (yielding breadth     */
   /* first traversal).                                       */
   /*=========================================================*/

   while (actPtr != NULL)
     {
      if (actPtr->salience > salience)
        {
         lastAct = actPtr;
         actPtr = actPtr->next;
        }
      else if (actPtr->salience < salience)
        { return(lastAct); }
      else if (timetag > actPtr->timetag)
        {
         lastAct = actPtr;
         actPtr = actPtr->next;
        }
      else
       {  return(lastAct); }
     }

   /*===========================================*/
   /* Return the insertion point in the agenda. */
   /*===========================================*/

   return(lastAct);
  }

/*******************************************************************/
/* PlaceLEXActivation: Determines the location in the agenda       */
/*    where a new activation should be placed for the lex          */
/*    strategy. Returns a pointer to the activation after which    */
/*    the new activation should be placed (or NULL if the          */
/*    activation should be placed at the beginning of the agenda). */
/*******************************************************************/
static ACTIVATION *PlaceLEXActivation(
  ACTIVATION *actPtr,
  ACTIVATION *newActivation)
  {
   int salience;
   unsigned long timetag;
   ACTIVATION *lastAct;
   int flag;

   /*===============================================*/
   /* Sort the fact identifiers for the activation. */
   /*===============================================*/

   if (newActivation->sortedBasis == NULL)
     { newActivation->sortedBasis = SortPartialMatch(newActivation->basis); }

   /*============================================*/
   /* Set up initial information for the search. */
   /*============================================*/

   timetag = newActivation->timetag;
   salience = newActivation->salience;
   lastAct = NULL;

   /*=========================================================*/
   /* Find the insertion point in the agenda. The activation  */
   /* is placed before activations of lower salience and      */
   /* after activations of higher salience. Among activations */
   /* of equal salience, the OPS5 lex strategy is used for    */
   /* determining placement.                                  */
   /*=========================================================*/

   while (actPtr != NULL)
     {
      if (actPtr->salience > salience)
        {
         lastAct = actPtr;
         actPtr = actPtr->next;
        }
      else if (actPtr->salience < salience)
        { return(lastAct); }
      else
        {
         flag = ComparePartialMatches(actPtr,newActivation);

         if (flag == LESS_THAN)
           {
            lastAct = actPtr;
            actPtr = actPtr->next;
           }
         else if (flag == GREATER_THAN)
           { return(lastAct); }
         else /* flag == EQUAL */
           {
            if (timetag > actPtr->timetag)
              {
               lastAct = actPtr;
               actPtr = actPtr->next;
              }
            else
             { return(lastAct); }
           }
        }
     }

   /*===========================================*/
   /* Return the insertion point in the agenda. */
   /*===========================================*/

   return(lastAct);
  }

/*******************************************************************/
/* PlaceMEAActivation: Determines the location in the agenda       */
/*    where a new activation should be placed for the mea          */
/*    strategy. Returns a pointer to the activation after which    */
/*    the new activation should be placed (or NULL if the          */
/*    activation should be placed at the beginning of the agenda). */
/*******************************************************************/
static ACTIVATION *PlaceMEAActivation(
  ACTIVATION *actPtr,
  ACTIVATION *newActivation)
  {
   int salience;
   unsigned long timetag;
   ACTIVATION *lastAct;
   int flag;
   long int cWhoset, oWhoset;

   if (newActivation->sortedBasis == NULL)
     { newActivation->sortedBasis = SortPartialMatch(newActivation->basis); }

   /*============================================*/
   /* Set up initial information for the search. */
   /*============================================*/

   timetag = newActivation->timetag;
   salience = newActivation->salience;
   lastAct = NULL;

   /*=========================================================*/
   /* Find the insertion point in the agenda. The activation  */
   /* is placed before activations of lower salience and      */
   /* after activations of higher salience. Among activations */
   /* of equal salience, the OPS5 mea strategy is used for    */
   /* determining placement.                                  */
   /*=========================================================*/

   while (actPtr != NULL)
     {
      if (actPtr->salience > salience)
        {
         lastAct = actPtr;
         actPtr = actPtr->next;
        }
      else if (actPtr->salience < salience)
        { return(lastAct); }
      else
        {
         cWhoset = -1;
         oWhoset = -1;
         if (GetMatchingItem(newActivation,0) != NULL)
           { cWhoset = GetMatchingItem(newActivation,0)->timeTag; }
         if (GetMatchingItem(actPtr,0) != NULL)
           { oWhoset = GetMatchingItem(actPtr,0)->timeTag; }
         if (oWhoset < cWhoset)
           {
            if (cWhoset > 0) flag = GREATER_THAN;
            else flag = LESS_THAN;
           }
         else if (oWhoset > cWhoset)
           {
            if (oWhoset > 0) flag = LESS_THAN;
            else flag = GREATER_THAN;
           }
         else
           { flag = ComparePartialMatches(actPtr,newActivation); }

         if (flag == LESS_THAN)
           {
            lastAct = actPtr;
            actPtr = actPtr->next;
           }
         else if (flag == GREATER_THAN)
           { return(lastAct); }
         else /* flag == EQUAL */
           {
            if (timetag > actPtr->timetag)
              {
               lastAct = actPtr;
               actPtr = actPtr->next;
              }
            else
             { return(lastAct); }
           }
        }
     }

   /*===========================================*/
   /* Return the insertion point in the agenda. */
   /*===========================================*/

   return(lastAct);
  }

/*********************************************************************/
/* PlaceComplexityActivation: Determines the location in the agenda  */
/*    where a new activation should be placed for the complexity     */
/*    strategy. Returns a pointer to the activation  after which the */
/*    new activation should be placed (or NULL if the activation     */
/*    should be placed at the beginning of the agenda).              */
/*********************************************************************/
static ACTIVATION *PlaceComplexityActivation(
  ACTIVATION *actPtr,
  ACTIVATION *newActivation)
  {
   int salience, complexity;
   unsigned long timetag;
   ACTIVATION *lastAct;

   /*========================================*/
   /* Set up initial information for search. */
   /*========================================*/

   timetag = newActivation->timetag;
   salience = newActivation->salience;
   complexity = newActivation->theRule->complexity;
   lastAct = NULL;

   /*=========================================================*/
   /* Find the insertion point in the agenda. The activation  */
   /* is placed before activations of lower salience and      */
   /* after activations of higher salience. Among activations */
   /* of equal salience, the activation is placed before      */
   /* activations of equal or lessor complexity.              */
   /*=========================================================*/

   while (actPtr != NULL)
     {
      if (actPtr->salience > salience)
        {
         lastAct = actPtr;
         actPtr = actPtr->next;
        }
      else if (actPtr->salience < salience)
        { return(lastAct); }
      else if (complexity < (int) actPtr->theRule->complexity)
        {
         lastAct = actPtr;
         actPtr = actPtr->next;
        }
      else if (complexity > (int) actPtr->theRule->complexity)
        { return(lastAct); }
      else if (timetag > actPtr->timetag)
        {
         lastAct = actPtr;
         actPtr = actPtr->next;
        }
      else
        { return(lastAct); }
     }

   /*===========================================*/
   /* Return the insertion point in the agenda. */
   /*===========================================*/

   return(lastAct);
  }

/*********************************************************************/
/* PlaceSimplicityActivation: Determines the location in the agenda  */
/*    where a new activation should be placed for the simplicity     */
/*    strategy. Returns a pointer to the activation  after which the */
/*    new activation should be placed (or NULL if the activation     */
/*    should be placed at the beginning of the agenda).              */
/*********************************************************************/
static ACTIVATION *PlaceSimplicityActivation(
  ACTIVATION *actPtr,
  ACTIVATION *newActivation)
  {
   int salience, complexity;
   unsigned long timetag;
   ACTIVATION *lastAct;

   /*============================================*/
   /* Set up initial information for the search. */
   /*============================================*/

   timetag = newActivation->timetag;
   salience = newActivation->salience;
   complexity = newActivation->theRule->complexity;
   lastAct = NULL;

   /*=========================================================*/
   /* Find the insertion point in the agenda. The activation  */
   /* is placed before activations of lower salience and      */
   /* after activations of higher salience. Among activations */
   /* of equal salience, the activation is placed after       */
   /* activations of equal or greater complexity.             */
   /*=========================================================*/

   while (actPtr != NULL)
     {
      if (actPtr->salience > salience)
        {
         lastAct = actPtr;
         actPtr = actPtr->next;
        }
      else if (actPtr->salience < salience)
        { return(lastAct); }
      else if (complexity > (int) actPtr->theRule->complexity)
        {
         lastAct = actPtr;
         actPtr = actPtr->next;
        }
      else if (complexity < (int) actPtr->theRule->complexity)
        { return(lastAct); }
      else if (timetag > actPtr->timetag)
        {
         lastAct = actPtr;
         actPtr = actPtr->next;
        }
      else
       { return(lastAct); }
     }

   /*===========================================*/
   /* Return the insertion point in the agenda. */
   /*===========================================*/

   return(lastAct);
  }

/*******************************************************************/
/* PlaceRandomActivation: Determines the location in the agenda    */
/*    where a new activation should be placed for the random       */
/*    strategy. Returns a pointer to the activation  after which   */
/*    the new activation should be placed (or NULL if the          */
/*    activation should be placed at the beginning of the agenda). */
/*******************************************************************/
static ACTIVATION *PlaceRandomActivation(
  ACTIVATION *actPtr,
  ACTIVATION *newActivation)
  {
   int salience, randomID;
   unsigned long timetag;
   ACTIVATION *lastAct;

   /*============================================*/
   /* Set up initial information for the search. */
   /*============================================*/

   timetag = newActivation->timetag;
   salience = newActivation->salience;
   randomID = newActivation->randomID;
   lastAct = NULL;

   /*=========================================================*/
   /* Find the insertion point in the agenda. The activation  */
   /* is placed before activations of lower salience and      */
   /* after activations of higher salience. Among activations */
   /* of equal salience, the placement of the activation is   */
   /* determined through the generation of a random number.   */
   /*=========================================================*/

   while (actPtr != NULL)
     {
      if (actPtr->salience > salience)
        {
         lastAct = actPtr;
         actPtr = actPtr->next;
        }
      else if (actPtr->salience < salience)
        { return(lastAct); }
      else if (randomID > actPtr->randomID)
        {
         lastAct = actPtr;
         actPtr = actPtr->next;
        }
      else if (randomID < actPtr->randomID)
       { return(lastAct); }
      else if (timetag > actPtr->timetag)
        {
         lastAct = actPtr;
         actPtr = actPtr->next;
        }
      else
       { return(lastAct); }
     }

   /*===========================================*/
   /* Return the insertion point in the agenda. */
   /*===========================================*/

   return(lastAct);
  }

/******************************************************************/
/* SortPartialMatch: Copies a partial match and then sorts the    */
/*   fact-indices in the copied partial match in ascending order. */
/******************************************************************/
static struct partialMatch *SortPartialMatch(
  struct partialMatch *binds)
  {
   struct partialMatch *nbinds;
   struct alphaMatch *temp;
   int flag, j, k;

   /*=================*/
   /* Copy the array. */
   /*=================*/

   nbinds = CopyPartialMatch(binds,0,0);

   /*=================*/
   /* Sort the array. */
   /*=================*/

   for (flag = TRUE, k = binds->bcount - 1;
        flag == TRUE;
        k--)
     {
      flag = FALSE;
      for (j = 0 ; j < k ; j++)
        {
         if ((nbinds->binds[j].gm.theMatch->matchingItem != NULL) &&
             (nbinds->binds[j + 1].gm.theMatch->matchingItem != NULL))
           {
            if (nbinds->binds[j].gm.theMatch->matchingItem->timeTag <
                nbinds->binds[j + 1].gm.theMatch->matchingItem->timeTag)
              {
               temp = nbinds->binds[j].gm.theMatch;
               nbinds->binds[j].gm.theMatch = nbinds->binds[j+1].gm.theMatch;
               nbinds->binds[j+1].gm.theMatch = temp;
               flag = TRUE;
              }
           }
        }
     }

   /*===================*/
   /* Return the array. */
   /*===================*/

   return(nbinds);
  }

/**************************************************************************/
/* ComparePartialMatches: Compares two activations using the lex conflict */
/*   resolution strategy to determine which activation should be placed   */
/*   first on the agenda. This lexicographic comparison function is used  */
/*   for both the lex and mea strategies.                                 */
/**************************************************************************/
static int ComparePartialMatches(
  ACTIVATION *actPtr,
  ACTIVATION *newActivation)
  {
   int cCount, oCount, mCount, i;

   /*=================================================*/
   /* If the activation already on the agenda doesn't */
   /* have a set of sorted timetags, then create one. */
   /*=================================================*/

   if (actPtr->sortedBasis == NULL)
     { actPtr->sortedBasis = SortPartialMatch(actPtr->basis); }

   /*==============================================================*/
   /* Determine the number of timetags in each of the activations. */
   /* The number of timetags to be compared is the lessor of these */
   /* two numbers.                                                 */
   /*==============================================================*/

   cCount = newActivation->sortedBasis->bcount;
   oCount = actPtr->sortedBasis->bcount;
   if (oCount > cCount) mCount = cCount;
   else mCount = oCount;

   /*===========================================================*/
   /* Compare the sorted timetags one by one until there are no */
   /* more timetags to compare or the timetags being compared   */
   /* are not equal. If the timetags aren't equal, then the     */
   /* activation containing the larger timetag is placed before */
   /* the activation containing the smaller timetag.            */
   /*===========================================================*/

   for (i = 0 ; i < mCount ; i++)
     {
      if ((actPtr->sortedBasis->binds[i].gm.theMatch->matchingItem != NULL) &&
          (newActivation->sortedBasis->binds[i].gm.theMatch->matchingItem != NULL))
        {
         if (newActivation->sortedBasis->binds[i].gm.theMatch->matchingItem->timeTag <
             actPtr->sortedBasis->binds[i].gm.theMatch->matchingItem->timeTag)
           { return(LESS_THAN); }
         else if (newActivation->sortedBasis->binds[i].gm.theMatch->matchingItem->timeTag >
                  actPtr->sortedBasis->binds[i].gm.theMatch->matchingItem->timeTag)
           { return(GREATER_THAN); }
        }
      else if (newActivation->sortedBasis->binds[i].gm.theMatch->matchingItem != NULL)
        { return(GREATER_THAN); }
      else if (actPtr->sortedBasis->binds[i].gm.theMatch->matchingItem != NULL)
        { return(LESS_THAN); }
     }

   /*==========================================================*/
   /* If the sorted timetags are identical up to the number of */
   /* timetags contained in the smaller partial match, then    */
   /* the activation containing more timetags should be        */
   /* placed before the activation containing fewer timetags.  */
   /*==========================================================*/

   if (cCount < oCount) return(LESS_THAN);
   else if (cCount > oCount) return(GREATER_THAN);

   /*=========================================================*/
   /* If the sorted partial matches for both activations are  */
   /* identical (containing the same number and values of     */
   /* timetags), then the activation associated with the rule */
   /* having the highest complexity is placed before the      */
   /* other partial match.                                    */
   /*=========================================================*/

   if (newActivation->theRule->complexity < actPtr->theRule->complexity)
     { return(LESS_THAN); }
   else if (newActivation->theRule->complexity > actPtr->theRule->complexity)
     { return(GREATER_THAN); }

   /*================================================*/
   /* The two partial matches are equal for purposes */
   /* of placement on the agenda for the lex and mea */
   /* conflict resolution strategies.                */
   /*================================================*/

   return(EQUAL);
  }

/*************************************/
/* SetStrategy: C access routine for */
/*   the set-strategy command.       */
/*************************************/
globle int SetStrategy(
  int value)
  {
   int oldStrategy;

   oldStrategy = Strategy;
   Strategy = value;

   if (oldStrategy != Strategy) ReorderAgenda(NULL);

   return(oldStrategy);
  }

/*************************************/
/* GetStrategy: C access routine for */
/*   the get-strategy command.       */
/*************************************/
globle int GetStrategy()
  {
   return(Strategy);
  }

/********************************************/
/* GetStrategyCommand: H/L access routine   */
/*   for the get-strategy command.          */
/********************************************/
globle SYMBOL_HN *GetStrategyCommand()
  {
   ArgCountCheck("get-strategy",EXACTLY,0);

   return((SYMBOL_HN *) AddSymbol(GetStrategyName(GetStrategy())));
  }

/********************************************/
/* SetStrategyCommand: H/L access routine   */
/*   for the set-strategy command.          */
/********************************************/
globle SYMBOL_HN *SetStrategyCommand()
  {
   DATA_OBJECT argPtr;
   char *argument;
   int oldStrategy = Strategy;

   /*=====================================================*/
   /* Check for the correct number and type of arguments. */
   /*=====================================================*/

   if (ArgCountCheck("set-strategy",EXACTLY,1) == -1)
     { return((SYMBOL_HN *) AddSymbol(GetStrategyName(GetStrategy()))); }

   if (ArgTypeCheck("set-strategy",1,SYMBOL,&argPtr) == FALSE)
     { return((SYMBOL_HN *) AddSymbol(GetStrategyName(GetStrategy()))); }

   argument = DOToString(argPtr);

   /*=============================================*/
   /* Set the strategy to the specified strategy. */
   /*=============================================*/

   if (strcmp(argument,"depth") == 0)
     { SetStrategy(DEPTH_STRATEGY); }
   else if (strcmp(argument,"breadth") == 0)
     { SetStrategy(BREADTH_STRATEGY); }
   else if (strcmp(argument,"lex") == 0)
     { SetStrategy(LEX_STRATEGY); }
   else if (strcmp(argument,"mea") == 0)
     { SetStrategy(MEA_STRATEGY); }
   else if (strcmp(argument,"complexity") == 0)
     { SetStrategy(COMPLEXITY_STRATEGY); }
   else if (strcmp(argument,"simplicity") == 0)
     { SetStrategy(SIMPLICITY_STRATEGY); }
   else if (strcmp(argument,"random") == 0)
     { SetStrategy(RANDOM_STRATEGY); }
   else
     {
      ExpectedTypeError1("set-strategy",1,
      "symbol with value depth, breadth, lex, mea, complexity, simplicity, or random");
      return((SYMBOL_HN *) AddSymbol(GetStrategyName(GetStrategy())));
     }

   /*=======================================*/
   /* Return the old value of the strategy. */
   /*=======================================*/

   return((SYMBOL_HN *) AddSymbol(GetStrategyName(oldStrategy)));
  }

/**********************************************************/
/* GetStrategyName: Given the integer value corresponding */
/*   to a specified strategy, return a character string   */
/*   of the strategy's name.                              */
/**********************************************************/
static char *GetStrategyName(
  int strategy)
  {
   char *sname;

   switch (strategy)
     {
      case DEPTH_STRATEGY:
        sname = "depth";
        break;
      case BREADTH_STRATEGY:
        sname = "breadth";
        break;
      case LEX_STRATEGY:
        sname = "lex";
        break;
      case MEA_STRATEGY:
        sname = "mea";
        break;
      case COMPLEXITY_STRATEGY:
        sname = "complexity";
        break;
      case SIMPLICITY_STRATEGY:
        sname = "simplicity";
        break;
      case RANDOM_STRATEGY:
        sname = "random";
        break;
      default:
        sname = "unknown";
        break;
     }

   return(sname);
  }
#endif /* CONFLICT_RESOLUTION_STRATEGIES */

#endif /* DEFRULE_CONSTRUCT */


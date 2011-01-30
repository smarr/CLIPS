   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*             LOGICAL DEPENDENCIES MODULE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provide support routines for managing truth      */
/*   maintenance using the logical conditional element.      */
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

#define _LGCLDPND_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "setup.h"

#if DEFRULE_CONSTRUCT && LOGICAL_DEPENDENCIES

#include "memalloc.h"
#include "router.h"
#include "evaluatn.h"
#include "engine.h"
#include "reteutil.h"
#include "pattern.h"
#include "argacces.h"
#include "factmngr.h"

#if OBJECT_SYSTEM
#include "insfun.h"
#endif

#include "lgcldpnd.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static struct partialMatch    *FindLogicalBind(struct joinNode *,struct partialMatch *);
   static int                     FindEntityInPartialMatch(struct patternEntity *,struct partialMatch *);
   static struct dependency      *DetachAssociatedDependencies(struct dependency *,void *);

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static struct dependency     *UnsupportedDataEntities = NULL;

/***********************************************************************/
/* AddLogicalDependencies: Adds the logical dependency links between a */
/*   data entity (such as a fact or instance) and the partial match    */
/*   which logically supports that data entity. If a data entity is    */
/*   unconditionally asserted (i.e. the global variable TheLogicalJoin */
/*   is NULL), then existing logical support for the data entity is no */
/*   longer needed and it is removed. If a data entity is already      */
/*   unconditionally supported and that data entity is conditionally   */
/*   asserted (i.e. the global variable TheLogicalJoin is not NULL),   */
/*   then the logical support is ignored. Otherwise, the partial match */
/*   is linked to the data entity and the data entity is linked to the */
/*   partial match. Note that the word assert is used to refer to      */
/*   creating a fact with the assert command and creating an instance  */
/*   with the make-instance command.                                   */
/***********************************************************************/
globle BOOLEAN AddLogicalDependencies(
  struct patternEntity *theEntity,
  int existingEntity)
  {
   struct partialMatch *theBinds;
   struct dependency *newDependency;

   /*==============================================*/
   /* If the rule has no logical patterns, then no */
   /* dependencies have to be established.         */
   /*==============================================*/

   if (TheLogicalJoin == NULL)
     {
      if (existingEntity) RemoveEntityDependencies(theEntity);
      return(TRUE);
     }
   else if (existingEntity && (theEntity->dependents == NULL))
     { return(TRUE); }

   /*============================================================*/
   /* Find the partial match in the logical join associated with */
   /* activation partial match. If the partial match cannot be   */
   /* found, then the partial match must have been deleted by a  */
   /* previous RHS action and the dependency link should not be  */
   /* added.                                                     */
   /*============================================================*/

   theBinds = FindLogicalBind(TheLogicalJoin,GlobalLHSBinds);
   if (theBinds == NULL) return(FALSE);

   /*==============================================================*/
   /* Add a dependency link between the partial match and the data */
   /* entity. The dependency links are stored in the partial match */
   /* behind the data entities stored in the partial match and the */
   /* activation link, if any.                                     */
   /*==============================================================*/

   newDependency = get_struct(dependency);
   newDependency->dPtr = (void *) theEntity;
   newDependency->next = (struct dependency *)
                         theBinds->binds[theBinds->bcount + theBinds->activationf].gm.theValue;
   theBinds->binds[theBinds->bcount + theBinds->activationf].gm.theValue = (void *) newDependency;

   /*================================================================*/
   /* Add a dependency link between the entity and the partialMatch. */
   /*================================================================*/

   newDependency = get_struct(dependency);
   newDependency->dPtr = (void *) theBinds;
   newDependency->next = (struct dependency *) theEntity->dependents;
   theEntity->dependents = (void *) newDependency;

   /*==================================================================*/
   /* Return TRUE to indicate that the data entity should be asserted. */
   /*==================================================================*/

   return(TRUE);
  }

/************************************************************************/
/* FindLogicalBind: Finds the partial match associated with the logical */
/*   CE which will provide logical support for a data entity asserted   */
/*   from the currently executing rule. The function is called when     */
/*   creating logical support links between the data entity and         */
/*   supporting partial matches. It compares each partial match found   */
/*   at a specified join to the partial match associated with a rule    */
/*   activation until it finds the partial match that generated the     */
/*   rule activation.                                                   */
/************************************************************************/
static struct partialMatch *FindLogicalBind(
  struct joinNode *theJoin,
  struct partialMatch *theBinds)
  {
   struct partialMatch *compPtr;
   unsigned int i;
   int found;

   /*==================================*/
   /* Loop through each of the partial */
   /* matches in the beta memory.      */
   /*==================================*/

   for (compPtr = theJoin->beta;
        compPtr != NULL;
        compPtr = compPtr->next)
     {
      /*==================================================*/
      /* Compare each of the data entities in the partial */
      /* match being examined and the partial match used  */
      /* in the dependency link.                          */
      /*==================================================*/

      found = TRUE;

      for (i = 0; i < compPtr->bcount; i++)
        {
         if (compPtr->binds[i].gm.theMatch != theBinds->binds[i].gm.theMatch)
           {
            found = FALSE;
            break;
           }
        }

      /*========================================================*/
      /* If all of the data entities in the partial match are   */
      /* identical to the partial match in the dependency link, */
      /* then this is the partial match we're looking for.      */
      /*========================================================*/

      if (found) return(compPtr);
     }

   /*========================================*/
   /* The partial match corresponding to the */
   /* logical dependency couldn't be found.  */
   /*========================================*/

   return(NULL);
  }

/*********************************************************************/
/* RemoveEntityDependencies: Removes all logical support links from  */
/*   a pattern entity that point to partial matches or other pattern */
/*   entities. Also removes the associated links from the partial    */
/*   matches or pattern entities which point back to the pattern     */
/*   entities.                                                       */
/*********************************************************************/
globle void RemoveEntityDependencies(
  struct patternEntity *theEntity)
  {
   struct dependency *fdPtr, *nextPtr, *theList;
   struct partialMatch *theBinds;

   /*===============================*/
   /* Get the list of dependencies. */
   /*===============================*/

   fdPtr = (struct dependency *) theEntity->dependents;

   /*========================================*/
   /* Loop through each of the dependencies. */
   /*========================================*/

   while (fdPtr != NULL)
     {
      /*===============================*/
      /* Remember the next dependency. */
      /*===============================*/

      nextPtr = fdPtr->next;

      /*================================================================*/
      /* Remove the link between the data entity and the partial match. */
      /*================================================================*/

      theBinds = (struct partialMatch *) fdPtr->dPtr;
      theList = (struct dependency *)
                theBinds->binds[theBinds->bcount + theBinds->activationf].gm.theValue;
      theList = DetachAssociatedDependencies(theList,(void *) theEntity);
      theBinds->binds[theBinds->bcount + theBinds->activationf].gm.theValue = (void *) theList;

      /*========================*/
      /* Return the dependency. */
      /*========================*/

      rtn_struct(dependency,fdPtr);

      /*=================================*/
      /* Move on to the next dependency. */
      /*=================================*/

      fdPtr = nextPtr;
     }

   /*=====================================================*/
   /* Set the dependency list of the data entity to NULL. */
   /*=====================================================*/

   theEntity->dependents = NULL;
  }

/*******************************************************************/
/* DetachAssociatedDependencies: Removes all logical support links */
/*   which pointer to a pattern entity from a list of dependencies */
/*   (which may be associated with either a partial match or       */
/*   another pattern entity). Does not remove links which point in */
/*   the other direction.                                          */
/*******************************************************************/
static struct dependency *DetachAssociatedDependencies(
  struct dependency *theList,
  void *theEntity)
  {
   struct dependency *fdPtr, *nextPtr, *lastPtr = NULL;

   fdPtr = theList;

   while (fdPtr != NULL)
     {
      if (fdPtr->dPtr == theEntity)
        {
         nextPtr = fdPtr->next;
         if (lastPtr == NULL) theList = nextPtr;
         else lastPtr->next = nextPtr;
         rtn_struct(dependency,fdPtr);
         fdPtr = nextPtr;
        }
      else
        {
         lastPtr = fdPtr;
         fdPtr = fdPtr->next;
        }
     }

   return(theList);
  }

/**************************************************************************/
/* RemovePMDependencies: Removes all logical support links from a partial */
/*   match that point to any data entities. Also removes the associated   */
/*   links from the data entities which point back to the partial match.  */
/**************************************************************************/
globle void RemovePMDependencies(
  struct partialMatch *theBinds)
  {
   struct dependency *fdPtr, *nextPtr, *theList;
   struct patternEntity *theEntity;

   fdPtr = (struct dependency *) theBinds->binds[theBinds->bcount + theBinds->activationf].gm.theValue;

   while (fdPtr != NULL)
     {
      nextPtr = fdPtr->next;

      theEntity = (struct patternEntity *) fdPtr->dPtr;

      theList = (struct dependency *) theEntity->dependents;
      theList = DetachAssociatedDependencies(theList,(void *) theBinds);
      theEntity->dependents = (void *) theList;

      rtn_struct(dependency,fdPtr);
      fdPtr = nextPtr;
     }

   theBinds->binds[theBinds->bcount + theBinds->activationf].gm.theValue = NULL;
  }

/************************************************************************/
/* RemoveLogicalSupport: Removes the dependency links between a partial */
/*   match and the data entities it logically supports. Also removes    */
/*   the associated links from the data entities which point back to    */
/*   the partial match by calling DetachAssociatedEntityDependencies.   */
/*   If an entity has all of its logical support removed as a result of */
/*   this procedure, the dependency link from the partial match is      */
/*   added to the list of unsupported data entities so that the entity  */
/*   will be deleted as a result of losing its logical support.         */
/************************************************************************/
globle void RemoveLogicalSupport(
  struct partialMatch *theBinds)
  {
   struct dependency *dlPtr, *tempPtr, *theList;
   struct patternEntity *theEntity;

   /*========================================*/
   /* If the partial match has no associated */
   /* dependencies, then return.             */
   /*========================================*/

   if (theBinds->dependentsf == FALSE) return;

   /*=======================================*/
   /* Loop through each of the dependencies */
   /* attached to the partial match.        */
   /*=======================================*/

   dlPtr = (struct dependency *) theBinds->binds[theBinds->bcount + theBinds->activationf].gm.theValue;

   while (dlPtr != NULL)
     {
      /*===============================*/
      /* Remember the next dependency. */
      /*===============================*/

      tempPtr = dlPtr->next;

      /*==========================================================*/
      /* Determine the data entity associated with the dependency */
      /* structure and delete its dependency references to this   */
      /* partial match.                                           */
      /*==========================================================*/

      theEntity = (struct patternEntity *) dlPtr->dPtr;

      theList = (struct dependency *) theEntity->dependents;
      theList = DetachAssociatedDependencies(theList,(void *) theBinds);
      theEntity->dependents = (void *) theList;

      /*==============================================================*/
      /* If the data entity has lost all of its logical support, then */
      /* add the dependency structure from the partial match to the   */
      /* list of unsupported data entities to be deleted. Otherwise,  */
      /* just delete the dependency structure.                        */
      /*==============================================================*/

      if (theEntity->dependents == NULL)
        {
         (*theEntity->theInfo->base.incrementBusyCount)(theEntity);
         dlPtr->next = UnsupportedDataEntities;
         UnsupportedDataEntities = dlPtr;
        }
      else
        { rtn_struct(dependency,dlPtr); }

      /*==================================*/
      /* Move on to the next dependency.  */
      /*==================================*/

      dlPtr = tempPtr;
     }

   /*=====================================*/
   /* The partial match no longer has any */
   /* dependencies associated with it.    */
   /*=====================================*/

   theBinds->binds[theBinds->bcount + theBinds->activationf].gm.theValue = NULL;
  }

/********************************************************************/
/* ForceLogicalRetractions: Deletes the data entities found on the  */
/*   list of items that have lost their logical support. The delete */
/*   function associated with each data entity is called to delete  */
/*   that data entity. Calling the delete function may in turn      */
/*   add more data entities to the list of data entities which have */
/*   lost their logical support.                                    */
/********************************************************************/
globle void ForceLogicalRetractions()
  {
   struct dependency *tempPtr;
   struct patternEntity *theEntity;
   Thread static int alreadyEntered = FALSE;

   /*===================================================*/
   /* Don't reenter this function once it's called. Any */
   /* new additions to the list of items to be deleted  */
   /* as a result of losing their logical support will  */
   /* be handled properly.                              */
   /*===================================================*/

   if (alreadyEntered) return;
   alreadyEntered = TRUE;

   /*=======================================================*/
   /* Continue to delete the first item on the list as long */
   /* as one exists. This is done because new items may be  */
   /* placed at the beginning of the list as other data     */
   /* entities are deleted.                                 */
   /*=======================================================*/

   while (UnsupportedDataEntities != NULL)
     {
      /*==========================================*/
      /* Determine the data entity to be deleted. */
      /*==========================================*/

      theEntity = (struct patternEntity *) UnsupportedDataEntities->dPtr;

      /*================================================*/
      /* Remove the dependency structure from the list. */
      /*================================================*/

      tempPtr = UnsupportedDataEntities;
      UnsupportedDataEntities = UnsupportedDataEntities->next;
      rtn_struct(dependency,tempPtr);

      /*=========================*/
      /* Delete the data entity. */
      /*=========================*/

      (*theEntity->theInfo->base.decrementBusyCount)(theEntity);
      (*theEntity->theInfo->base.deleteFunction)(theEntity);
     }

   /*============================================*/
   /* Deletion of items on the list is complete. */
   /*============================================*/

   alreadyEntered = FALSE;
  }

/****************************************************************/
/* Dependencies: C access routine for the dependencies command. */
/****************************************************************/
globle void Dependencies(
  struct patternEntity *theEntity)
  {
   struct dependency *fdPtr;

   /*=========================================*/
   /* If the data entity has no dependencies, */
   /* then print "None" and return.           */
   /*=========================================*/

   if (theEntity->dependents == NULL)
     {
      PrintRouter(WDISPLAY,"None\n");
      return;
     }

   /*============================================*/
   /* Loop through the list of the data entities */
   /* dependencies and print them.               */
   /*============================================*/

   for (fdPtr = (struct dependency *) theEntity->dependents;
        fdPtr != NULL;
        fdPtr = fdPtr->next)
     {
      if (GetHaltExecution() == TRUE) return;
      PrintPartialMatch(WDISPLAY,(struct partialMatch *) fdPtr->dPtr);
      PrintRouter(WDISPLAY,"\n");
     }
  }

/************************************************************/
/* Dependents: C access routine for the dependents command. */
/************************************************************/
globle void Dependents(
  struct patternEntity *theEntity)
  {
   struct patternEntity *entityPtr = NULL;
   struct patternParser *theParser = NULL;
   struct dependency *fdPtr;
   struct partialMatch *theBinds;
   int found = FALSE;

   /*=================================*/
   /* Loop through every data entity. */
   /*=================================*/

   for (GetNextPatternEntity(&theParser,&entityPtr);
        entityPtr != NULL;
        GetNextPatternEntity(&theParser,&entityPtr))
     {
      if (GetHaltExecution() == TRUE) return;

      /*====================================*/
      /* Loop through every dependency link */
      /* associated with the data entity.   */
      /*====================================*/

      for (fdPtr = (struct dependency *) entityPtr->dependents;
           fdPtr != NULL;
           fdPtr = fdPtr->next)
        {
         if (GetHaltExecution() == TRUE) return;

         /*=====================================================*/
         /* If the data entity which was the argument passed to */
         /* the dependents command is contained in one of the   */
         /* partial matches of the data entity currently being  */
         /* examined, then the data entity being examined is a  */
         /* dependent. Print the data entity and then move on   */
         /* to the next data entity.                            */
         /*=====================================================*/

         theBinds = (struct partialMatch *) fdPtr->dPtr;
         if (FindEntityInPartialMatch(theEntity,theBinds) == TRUE)
           {
            if (found) PrintRouter(WDISPLAY,",");
            (*entityPtr->theInfo->base.shortPrintFunction)(WDISPLAY,entityPtr);
            found = TRUE;
            break;
           }
        }
     }

   /*=================================================*/
   /* If no dependents were found, then print "None." */
   /* Otherwise print a carriage return after the     */
   /* list of dependents.                             */
   /*=================================================*/

   if (! found) PrintRouter(WDISPLAY,"None\n");
   else PrintRouter(WDISPLAY,"\n");
  }

/******************************************************/
/* FindEntityInPartialMatch: Searches for a specified */
/*   data entity in a partial match.                  */
/******************************************************/
static int FindEntityInPartialMatch(
  struct patternEntity *theEntity,
  struct partialMatch *thePartialMatch)
  {
   short int i;

   for (i = 0 ; i < (int) thePartialMatch->bcount; i++)
     {
      if (thePartialMatch->binds[i].gm.theMatch->matchingItem == theEntity)
        { return(TRUE); }
     }

   return(FALSE);
  }

#if DEBUGGING_FUNCTIONS

/*********************************************/
/* DependenciesCommand: H/L access routine   */
/*   for the dependencies command.           */
/*********************************************/
globle void DependenciesCommand()
  {
   DATA_OBJECT item;
   void *ptr;

   if (ArgCountCheck("dependencies",EXACTLY,1) == -1) return;

   ptr = GetFactOrInstanceArgument(1,&item,"dependencies");

   if (ptr == NULL) return;

#if DEFRULE_CONSTRUCT
   Dependencies((struct patternEntity *) ptr);
#else
   PrintRouter(WDISPLAY,"None\n");
#endif
  }

/*******************************************/
/* DependentsCommand: H/L access routine   */
/*   for the dependents command.           */
/*******************************************/
globle void DependentsCommand()
  {
   DATA_OBJECT item;
   void *ptr;

   if (ArgCountCheck("dependents",EXACTLY,1) == -1) return;

   ptr = GetFactOrInstanceArgument(1,&item,"dependents");

   if (ptr == NULL) return;

#if DEFRULE_CONSTRUCT
   Dependents((struct patternEntity *) ptr);
#else
   PrintRouter(WDISPLAY,"None\n");
#endif
  }

#endif /* DEBUGGING_FUNCTIONS */

#endif /* DEFRULE_CONSTRUCT && LOGICAL_DEPENDENCIES */


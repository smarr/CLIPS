   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*                  EXPRESSION MODULE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains routines for creating, deleting,        */
/*   compacting, installing, and hashing expressions.        */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Changed name of variable exp to theExp         */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Corrected link errors with non-default         */
/*            setup.h configuration settings.                */
/*                                                           */
/*************************************************************/

#define _EXPRESSN_SOURCE_

#include "setup.h"

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "bload.h"
#include "memalloc.h"
#include "envrnmnt.h"
#include "router.h"
#include "extnfunc.h"
#include "exprnops.h"
#include "prntutil.h"
#include "evaluatn.h"

#include "expressn.h"

#define PRIME_ONE   257
#define PRIME_TWO   263
#define PRIME_THREE 269

/****************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS  */
/****************************************/

#if (! RUN_TIME)
   static long                    ListToPacked(struct expr *,
                                               struct expr *,long);
   static EXPRESSION_HN          *FindHashedExpression(void *,EXEC_STATUS,EXPRESSION *,unsigned *,EXPRESSION_HN **);
   static unsigned                HashExpression(EXPRESSION *);
#endif
   static void                    DeallocateExpressionData(void *,EXEC_STATUS);

/**************************************************/
/* InitExpressionData: Initializes the function   */
/*   pointers used in generating some expressions */
/*   and the expression hash table.               */
/**************************************************/
globle void InitExpressionData(
  void *theEnv,
  EXEC_STATUS)
  {
#if ! RUN_TIME
   register unsigned i;
#endif

   AllocateEnvironmentData(theEnv,execStatus,EXPRESSION_DATA,sizeof(struct expressionData),DeallocateExpressionData);

#if ! RUN_TIME
   InitExpressionPointers(theEnv,execStatus);

   ExpressionData(theEnv,execStatus)->ExpressionHashTable = (EXPRESSION_HN **)
     gm2(theEnv,execStatus,(int) (sizeof(EXPRESSION_HN *) * EXPRESSION_HASH_SIZE));
   for (i = 0 ; i < EXPRESSION_HASH_SIZE ; i++)
     ExpressionData(theEnv,execStatus)->ExpressionHashTable[i] = NULL;
#endif
  }
  
/*****************************************/
/* DeallocateExpressionData: Deallocates */
/*    environment data for expressions.  */
/*****************************************/
static void DeallocateExpressionData(
  void *theEnv,
  EXEC_STATUS)
  {
#if ! RUN_TIME
   int i;
   EXPRESSION_HN *tmpPtr, *nextPtr;
   
#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)
   if (! Bloaded(theEnv,execStatus))
#endif
     {
      for (i = 0; i < EXPRESSION_HASH_SIZE; i++)
        {
         tmpPtr = ExpressionData(theEnv,execStatus)->ExpressionHashTable[i];
         while (tmpPtr != NULL)
           {
            nextPtr = tmpPtr->next;
            ReturnPackedExpression(theEnv,execStatus,tmpPtr->exp);
            rtn_struct(theEnv,execStatus,exprHashNode,tmpPtr);
            tmpPtr = nextPtr;
           }
        }
     }
     
   rm(theEnv,execStatus,ExpressionData(theEnv,execStatus)->ExpressionHashTable,
      (int) (sizeof(EXPRESSION_HN *) * EXPRESSION_HASH_SIZE));
#else
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv,execStatus)
#endif
#endif
   
#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)
   if ((ExpressionData(theEnv,execStatus)->NumberOfExpressions != 0) && Bloaded(theEnv,execStatus))
     {
      genfree(theEnv,execStatus,(void *) ExpressionData(theEnv,execStatus)->ExpressionArray,
                  ExpressionData(theEnv,execStatus)->NumberOfExpressions * sizeof(struct expr));
     }
#endif
  }

/****************************************************/
/* InitExpressionPointers: Initializes the function */
/*   pointers used in generating some expressions.  */
/****************************************************/
globle void InitExpressionPointers(
  void *theEnv,
  EXEC_STATUS)
  {
   ExpressionData(theEnv,execStatus)->PTR_AND = (void *) FindFunction(theEnv,execStatus,"and");
   ExpressionData(theEnv,execStatus)->PTR_OR = (void *) FindFunction(theEnv,execStatus,"or");
   ExpressionData(theEnv,execStatus)->PTR_EQ = (void *) FindFunction(theEnv,execStatus,"eq");
   ExpressionData(theEnv,execStatus)->PTR_NEQ = (void *) FindFunction(theEnv,execStatus,"neq");
   ExpressionData(theEnv,execStatus)->PTR_NOT = (void *) FindFunction(theEnv,execStatus,"not");

   if ((ExpressionData(theEnv,execStatus)->PTR_AND == NULL) || (ExpressionData(theEnv,execStatus)->PTR_OR == NULL) ||
       (ExpressionData(theEnv,execStatus)->PTR_EQ == NULL) || (ExpressionData(theEnv,execStatus)->PTR_NEQ == NULL) || (ExpressionData(theEnv,execStatus)->PTR_NOT == NULL))
     {
      SystemError(theEnv,execStatus,"EXPRESSN",1);
      EnvExitRouter(theEnv,execStatus,EXIT_FAILURE);
     }
  }

/***************************************************/
/* ExpressionInstall: Increments the busy count of */
/*   atomic data values found in an expression.    */
/***************************************************/
globle void ExpressionInstall(
  void *theEnv,
  EXEC_STATUS,
  struct expr *expression)
  {
   if (expression == NULL) return;

   while (expression != NULL)
     {
      AtomInstall(theEnv,execStatus,expression->type,expression->value);
      ExpressionInstall(theEnv,execStatus,expression->argList);
      expression->refCount++;
      expression = expression->nextArg;
     }
  }

/*****************************************************/
/* ExpressionDeinstall: Decrements the busy count of */
/*   atomic data values found in an expression.      */
/*****************************************************/
globle void ExpressionDeinstall(
  void *theEnv,
  EXEC_STATUS,
  struct expr *expression)
  {
    if (expression == NULL) return;
    
    while (expression != NULL)
    {
      AtomDeinstall(theEnv,execStatus,expression->type,expression->value);
      ExpressionDeinstall(theEnv,execStatus,expression->argList);
      expression->refCount--;
      expression = expression->nextArg;
    }
    
  }


#if (! RUN_TIME)

/***********************************************************************/
/* PackExpression: Copies an expression (created using multiple memory */
/*   requests) into an array (created using a single memory request)   */
/*   while maintaining all appropriate links in the expression. A      */
/*   packed expression requires less total memory because it reduces   */
/*   the overhead required for multiple memory allocations.            */
/***********************************************************************/
globle struct expr *PackExpression(
  void *theEnv,
  EXEC_STATUS,
  struct expr *original)
  {
   struct expr *packPtr;

   if (original == NULL) return (NULL);
   packPtr = (struct expr *)
             gm3(theEnv,execStatus,(long) sizeof (struct expr) *
                 (long) ExpressionSize(original));
   ListToPacked(original,packPtr,0L);
   return(packPtr);
  }

/***********************************************************/
/* ListToPacked: Copies a list of expressions to an array. */
/***********************************************************/
static long ListToPacked(
  struct expr *original,
  struct expr *destination,
  long count)
  {
   long i;

   if (original == NULL) { return(count); }

   while (original != NULL)
     {
      i = count;
      count++;

      destination[i].type = original->type;
	  destination[i].value = original->value;
	  destination[i].refCount = original->refCount; // Lode Propagate refCount

      if (original->argList == NULL)
        { destination[i].argList = NULL; }
      else
        {
         destination[i].argList =
           (struct expr *) &destination[(long) count];
         count = ListToPacked(original->argList,destination,count);
        }

      if (original->nextArg == NULL)
        { destination[i].nextArg = NULL; }
      else
        {
         destination[i].nextArg =
           (struct expr *) &destination[(long) count];
        }

      original = original->nextArg;
     }

   return(count);
  }

/***************************************************************/
/* ReturnPackedExpression: Returns a packed expression created */
/*   using PackExpression to the memory manager.               */
/***************************************************************/
globle void ReturnPackedExpression(
  void *theEnv,
  EXEC_STATUS,
  struct expr *packPtr)
  {
   if (packPtr != NULL)
     {
      rm3(theEnv,execStatus,(void *) packPtr,(long) sizeof (struct expr) *
                           ExpressionSize(packPtr));
     }
  }

#endif /* (! RUN_TIME) */

/***********************************************/
/* ReturnExpression: Returns a multiply linked */
/*   list of expr data structures.             */
/***********************************************/
globle void ReturnExpression(
  void *theEnv,
  EXEC_STATUS,
  struct expr *waste)
  {
   register struct expr *tmp;

   while (waste != NULL && waste->refCount < 1)
     {
      if (waste->argList != NULL) ReturnExpression(theEnv,execStatus,waste->argList);
      tmp = waste;
      waste = waste->nextArg;
      rtn_struct(theEnv,execStatus,expr,tmp);
     }
  }

#if (! RUN_TIME)

/***************************************************
  NAME         : FindHashedExpression
  DESCRIPTION  : Determines if a given expression
                 is in the expression hash table
  INPUTS       : 1) The expression
                 2) A buffer to hold the hash
                    value
                 3) A buffer to hold the previous
                    node in the hash chain
  RETURNS      : The expression hash table entry
                 (NULL if not found)
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
static EXPRESSION_HN *FindHashedExpression(
  void *theEnv,
  EXEC_STATUS,
  EXPRESSION *theExp,
  unsigned *hashval,
  EXPRESSION_HN **prv)
  {
   EXPRESSION_HN *exphash;

   if (theExp == NULL)
     return(NULL);
   *hashval = HashExpression(theExp);
   *prv = NULL;
   exphash = ExpressionData(theEnv,execStatus)->ExpressionHashTable[*hashval];
   while (exphash != NULL)
     {
      if (IdenticalExpression(exphash->exp,theExp))
        return(exphash);
      *prv = exphash;
      exphash = exphash->next;
     }
   return(NULL);
  }

/***************************************************
  NAME         : HashExpression
  DESCRIPTION  : Assigns a deterministic number to
                 an expression
  INPUTS       : The expression
  RETURNS      : The "value" of the expression
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
static unsigned HashExpression(
  EXPRESSION *theExp)
  {
   unsigned long tally = PRIME_THREE;
   union
     {
      void *vv;
      unsigned long uv;
     } fis;
     
   if (theExp->argList != NULL)
     tally += HashExpression(theExp->argList) * PRIME_ONE;
   while (theExp != NULL)
     {
      tally += (unsigned long) (theExp->type * PRIME_TWO);
      fis.uv = 0;
      fis.vv = theExp->value;
      tally += fis.uv;
      theExp = theExp->nextArg;
     }
   return((unsigned) (tally % EXPRESSION_HASH_SIZE));
  }

/***************************************************
  NAME         : RemoveHashedExpression
  DESCRIPTION  : Removes a hashed expression from
                 the hash table
  INPUTS       : The expression
  RETURNS      : Nothing useful
  SIDE EFFECTS : Hash node removed (or use count
                 decremented).  If the hash node
                 is removed, the expression is
                 deinstalled and deleted
  NOTES        : If the expression is in use by
                 others, then the use count is
                 merely decremented
 ***************************************************/
globle void RemoveHashedExpression(
  void *theEnv,
  EXEC_STATUS,
  EXPRESSION *theExp)
  {
   EXPRESSION_HN *exphash,*prv;
   unsigned hashval;

   exphash = FindHashedExpression(theEnv,execStatus,theExp,&hashval,&prv);
   if (exphash == NULL)
     return;
   if (--exphash->count != 0)
     return;
   if (prv == NULL)
     ExpressionData(theEnv,execStatus)->ExpressionHashTable[hashval] = exphash->next;
   else
     prv->next = exphash->next;
   ExpressionDeinstall(theEnv,execStatus,exphash->exp);
   ReturnPackedExpression(theEnv,execStatus,exphash->exp);
   rtn_struct(theEnv,execStatus,exprHashNode,exphash);
  }

#endif /* (! RUN_TIME) */

#if (! BLOAD_ONLY) && (! RUN_TIME)

/*****************************************************
  NAME         : AddHashedExpression
  DESCRIPTION  : Adds a new expression to the
                 expression hash table (or increments
                 the use count if it is already there)
  INPUTS       : The (new) expression
  RETURNS      : A pointer to the (new) hash node
  SIDE EFFECTS : Adds the new hash node or increments
                 the count of an existing one
  NOTES        : It is the caller's responsibility to
                 delete the passed expression.  This
                 routine copies, packs and installs
                 the given expression
 *****************************************************/
globle EXPRESSION *AddHashedExpression(
  void *theEnv,
  EXEC_STATUS,
  EXPRESSION *theExp)
  {
   EXPRESSION_HN *prv,*exphash;
   unsigned hashval;

   if (theExp == NULL) return(NULL);
   exphash = FindHashedExpression(theEnv,execStatus,theExp,&hashval,&prv);
   if (exphash != NULL)
     {
      exphash->count++;
      return(exphash->exp);
     }
   exphash = get_struct(theEnv,execStatus,exprHashNode);
   exphash->hashval = hashval;
   exphash->count = 1;
   exphash->exp = PackExpression(theEnv,execStatus,theExp);
   ExpressionInstall(theEnv,execStatus,exphash->exp);
   exphash->next = ExpressionData(theEnv,execStatus)->ExpressionHashTable[exphash->hashval];
   ExpressionData(theEnv,execStatus)->ExpressionHashTable[exphash->hashval] = exphash;
   exphash->bsaveID = 0L;
   return(exphash->exp);
  }

#endif /* (! BLOAD_ONLY) && (! RUN_TIME) */

#if (BLOAD_AND_BSAVE || BLOAD_ONLY || BLOAD || CONSTRUCT_COMPILER) && (! RUN_TIME)

/***************************************************
  NAME         : HashedExpressionIndex
  DESCRIPTION  : Finds the expression bload array
                 index for a hashed expression
  INPUTS       : The expression
  RETURNS      : The bload index
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle long HashedExpressionIndex(
  void *theEnv,
  EXEC_STATUS,
  EXPRESSION *theExp)
  {
   EXPRESSION_HN *exphash,*prv;
   unsigned hashval;

   if (theExp == NULL)
     return(-1L);
   exphash = FindHashedExpression(theEnv,execStatus,theExp,&hashval,&prv);
   return((exphash != NULL) ? exphash->bsaveID : -1L);
  }

#endif /* (BLOAD_AND_BSAVE || BLOAD_ONLY || BLOAD || CONSTRUCT_COMPILER) && (! RUN_TIME) */


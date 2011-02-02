   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*              DEFGLOBAL COMMANDS MODULE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides the show-defglobals, set-reset-globals, */
/*   and get-reset-globals commands.                         */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

#define _GLOBLCOM_SOURCE_

#include "setup.h"

#if DEFGLOBAL_CONSTRUCT

#include "extnfunc.h"
#include "argacces.h"
#include "prntutil.h"
#include "router.h"
#include "envrnmnt.h"

#include "globldef.h"

#include "globlcom.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if DEBUGGING_FUNCTIONS
   static void                       PrintDefglobalValueForm(void *,EXEC_STATUS,char *,void *);
#endif

/************************************************************/
/* DefglobalCommandDefinitions: Defines defglobal commands. */
/************************************************************/
globle void DefglobalCommandDefinitions(
  void *theEnv,
  EXEC_STATUS)
  {
#if ! RUN_TIME
   EnvDefineFunction2(theEnv,execStatus,"set-reset-globals",'b',
                  SetResetGlobalsCommand,"SetResetGlobalsCommand", "11");
   EnvDefineFunction2(theEnv,execStatus,"get-reset-globals",'b',
                   GetResetGlobalsCommand,"GetResetGlobalsCommand", "00");

#if DEBUGGING_FUNCTIONS
   EnvDefineFunction2(theEnv,execStatus,"show-defglobals",'v',
                   PTIEF ShowDefglobalsCommand,"ShowDefglobalsCommand", "01w");
#endif

#else
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv,execStatus)
#endif
#endif
  }

/************************************************/
/* SetResetGlobalsCommand: H/L access routine   */
/*   for the get-reset-globals command.         */
/************************************************/
globle int SetResetGlobalsCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   int oldValue;
   DATA_OBJECT arg_ptr;

   /*===========================================*/
   /* Remember the old value of this attribute. */
   /*===========================================*/

   oldValue = EnvGetResetGlobals(theEnv,execStatus);

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/

   if (EnvArgCountCheck(theEnv,execStatus,"set-reset-globals",EXACTLY,1) == -1)
     { return(oldValue); }

   /*===========================================*/
   /* Determine the new value of the attribute. */
   /*===========================================*/

   EnvRtnUnknown(theEnv,execStatus,1,&arg_ptr);

   if ((arg_ptr.value == EnvFalseSymbol(theEnv,execStatus)) && (arg_ptr.type == SYMBOL))
     { EnvSetResetGlobals(theEnv,execStatus,FALSE); }
   else
     { EnvSetResetGlobals(theEnv,execStatus,TRUE); }

   /*========================================*/
   /* Return the old value of the attribute. */
   /*========================================*/

   return(oldValue);
  }

/****************************************/
/* EnvSetResetGlobals: C access routine */
/*   for the set-reset-globals command. */
/****************************************/
globle intBool EnvSetResetGlobals(
  void *theEnv,
  EXEC_STATUS,
  int value)
  {
   int ov;

   ov = DefglobalData(theEnv,execStatus)->ResetGlobals;
   DefglobalData(theEnv,execStatus)->ResetGlobals = value;
   return(ov);
  }

/************************************************/
/* GetResetGlobalsCommand: H/L access routine   */
/*   for the get-reset-globals command.         */
/************************************************/
globle int GetResetGlobalsCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   int oldValue;

   oldValue = EnvGetResetGlobals(theEnv,execStatus);

   if (EnvArgCountCheck(theEnv,execStatus,"get-reset-globals",EXACTLY,0) == -1)
     { return(oldValue); }

   return(oldValue);
  }

/****************************************/
/* EnvGetResetGlobals: C access routine */
/*   for the get-reset-globals command. */
/****************************************/
globle intBool EnvGetResetGlobals(
  void *theEnv,
  EXEC_STATUS)
  {   
   return(DefglobalData(theEnv,execStatus)->ResetGlobals); 
  }

#if DEBUGGING_FUNCTIONS

/***********************************************/
/* ShowDefglobalsCommand: H/L access routine   */
/*   for the show-defglobals command.          */
/***********************************************/
globle void ShowDefglobalsCommand(
  void *theEnv,
  EXEC_STATUS)
  {
   struct defmodule *theModule;
   int numArgs, error;

   if ((numArgs = EnvArgCountCheck(theEnv,execStatus,"show-defglobals",NO_MORE_THAN,1)) == -1) return;

   if (numArgs == 1)
     {
      theModule = GetModuleName(theEnv,execStatus,"show-defglobals",1,&error);
      if (error) return;
     }
   else
     { theModule = ((struct defmodule *) EnvGetCurrentModule(theEnv,execStatus)); }

   EnvShowDefglobals(theEnv,execStatus,WDISPLAY,theModule);
  }

/***************************************/
/* EnvShowDefglobals: C access routine */
/*   for the show-defglobals command.  */
/***************************************/
globle void EnvShowDefglobals(
  void *theEnv,
  EXEC_STATUS,
  char *logicalName,
  void *vTheModule)
  {
   struct defmodule *theModule = (struct defmodule *) vTheModule;
   struct constructHeader *constructPtr;
   int allModules = FALSE;
   struct defmoduleItemHeader *theModuleItem;

   /*=======================================*/
   /* If the module specified is NULL, then */
   /* list all constructs in all modules.   */
   /*=======================================*/

   if (theModule == NULL)
     {
      theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,NULL);
      allModules = TRUE;
     }

   /*======================================================*/
   /* Print out the constructs in the specified module(s). */
   /*======================================================*/

   for (;
        theModule != NULL;
        theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,theModule))
     {
      /*===========================================*/
      /* Print the module name before every group  */
      /* of defglobals listed if we're listing the */
      /* defglobals from every module.             */
      /*===========================================*/

      if (allModules)
        {
         EnvPrintRouter(theEnv,execStatus,logicalName,EnvGetDefmoduleName(theEnv,execStatus,theModule));
         EnvPrintRouter(theEnv,execStatus,logicalName,":\n");
        }

      /*=====================================*/
      /* Print every defglobal in the module */
      /* currently being examined.           */
      /*=====================================*/

      theModuleItem = (struct defmoduleItemHeader *) GetModuleItem(theEnv,execStatus,theModule,DefglobalData(theEnv,execStatus)->DefglobalModuleIndex);

      for (constructPtr = theModuleItem->firstItem;
           constructPtr != NULL;
           constructPtr = constructPtr->next)
        {
         if (execStatus->HaltExecution == TRUE) return;

         if (allModules) EnvPrintRouter(theEnv,execStatus,logicalName,"   ");
         PrintDefglobalValueForm(theEnv,execStatus,logicalName,(void *) constructPtr);
         EnvPrintRouter(theEnv,execStatus,logicalName,"\n");
        }

      /*===================================*/
      /* If we're only listing the globals */
      /* for one module, then return.      */
      /*===================================*/

      if (! allModules) return;
     }
  }

/*****************************************************/
/* PrintDefglobalValueForm: Prints the value form of */
/*   a defglobal (the current value). For example,   */
/*   ?*x* = 3                                        */
/*****************************************************/
static void PrintDefglobalValueForm(
  void *theEnv,
  EXEC_STATUS,
  char *logicalName,
  void *vTheGlobal)
  {
   struct defglobal *theGlobal = (struct defglobal *) vTheGlobal;

   EnvPrintRouter(theEnv,execStatus,logicalName,"?*");
   EnvPrintRouter(theEnv,execStatus,logicalName,ValueToString(theGlobal->header.name));
   EnvPrintRouter(theEnv,execStatus,logicalName,"* = ");
   PrintDataObject(theEnv,execStatus,logicalName,&theGlobal->current);
  }

#endif /* DEBUGGING_FUNCTIONS */

#endif /* DEFGLOBAL_CONSTRUCT */


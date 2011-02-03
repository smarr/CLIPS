   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/05/06            */
   /*                                                     */
   /*                  DEFGLOBAL MODULE                   */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides core routines for the creation and      */
/*   maintenance of the defglobal construct.                 */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Corrected code to remove run-time program      */
/*            compiler warning.                              */
/*                                                           */
/*************************************************************/

#define _GLOBLDEF_SOURCE_

#include "setup.h"

#if DEFGLOBAL_CONSTRUCT

#include <stdio.h>
#define _STDIO_INCLUDED_

#include "memalloc.h"
#include "modulpsr.h"
#include "multifld.h"
#include "router.h"
#include "strngrtr.h"
#include "modulutl.h"
#include "globlbsc.h"
#include "globlpsr.h"
#include "globlcom.h"
#include "utility.h"
#include "commline.h"
#include "envrnmnt.h"

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
#include "bload.h"
#include "globlbin.h"
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
#include "globlcmp.h"
#endif

#include "globldef.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                   *AllocateModule(void *,EXEC_STATUS);
   static void                    ReturnModule(void *,EXEC_STATUS,void *);
   static void                    ReturnDefglobal(void *,EXEC_STATUS,void *);
   static void                    InitializeDefglobalModules(void *,EXEC_STATUS);
   static intBool                 GetDefglobalValue2(void *,EXEC_STATUS,void *,DATA_OBJECT_PTR);
   static void                    IncrementDefglobalBusyCount(void *,EXEC_STATUS,void *);
   static void                    DecrementDefglobalBusyCount(void *,EXEC_STATUS,void *);
   static void                    DeallocateDefglobalData(void *,EXEC_STATUS);
   static void                    DestroyDefglobalAction(void *,EXEC_STATUS,struct constructHeader *,void *);
   static void                    DestroyDefglobal(void *,EXEC_STATUS,void *);

/**************************************************************/
/* InitializeDefglobals: Initializes the defglobal construct. */
/**************************************************************/
globle void InitializeDefglobals(
  void *theEnv,
  EXEC_STATUS)
  {  
   struct entityRecord globalInfo = { "GBL_VARIABLE", GBL_VARIABLE,0,0,0,
                                                       NULL,
                                                       NULL,
                                                       NULL,
                                                       GetDefglobalValue2,
                                                       NULL,NULL,
                                                       NULL,NULL,NULL,NULL,NULL,NULL };

   struct entityRecord defglobalPtrRecord = { "DEFGLOBAL_PTR", DEFGLOBAL_PTR,0,0,0,
                                                       NULL,NULL,NULL,
                                                       QGetDefglobalValue,
                                                       NULL,
                                                       DecrementDefglobalBusyCount,
                                                       IncrementDefglobalBusyCount,
                                                       NULL,NULL,NULL,NULL,NULL };
   
   AllocateEnvironmentData(theEnv,execStatus,DEFGLOBAL_DATA,sizeof(struct defglobalData),DeallocateDefglobalData);
   
   memcpy(&DefglobalData(theEnv,execStatus)->GlobalInfo,&globalInfo,sizeof(struct entityRecord));   
   memcpy(&DefglobalData(theEnv,execStatus)->DefglobalPtrRecord,&defglobalPtrRecord,sizeof(struct entityRecord));   

   DefglobalData(theEnv,execStatus)->ResetGlobals = TRUE;
   DefglobalData(theEnv,execStatus)->LastModuleIndex = -1;
   
   InstallPrimitive(theEnv,execStatus,&DefglobalData(theEnv,execStatus)->GlobalInfo,GBL_VARIABLE);
   InstallPrimitive(theEnv,execStatus,&DefglobalData(theEnv,execStatus)->DefglobalPtrRecord,DEFGLOBAL_PTR);

   InitializeDefglobalModules(theEnv,execStatus);

   DefglobalBasicCommands(theEnv,execStatus);
   DefglobalCommandDefinitions(theEnv,execStatus);

   DefglobalData(theEnv,execStatus)->DefglobalConstruct =
      AddConstruct(theEnv,execStatus,"defglobal","defglobals",ParseDefglobal,EnvFindDefglobal,
                   GetConstructNamePointer,GetConstructPPForm,
                   GetConstructModuleItem,EnvGetNextDefglobal,SetNextConstruct,
                   EnvIsDefglobalDeletable,EnvUndefglobal,ReturnDefglobal);
  }

/****************************************************/
/* DeallocateDefglobalData: Deallocates environment */
/*    data for the defglobal construct.             */
/****************************************************/
static void DeallocateDefglobalData(
  void *theEnv,
  EXEC_STATUS)
  {
#if ! RUN_TIME
   struct defglobalModule *theModuleItem;
   void *theModule;
   
#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded(theEnv,execStatus)) return;
#endif

   DoForAllConstructs(theEnv,execStatus,DestroyDefglobalAction,DefglobalData(theEnv,execStatus)->DefglobalModuleIndex,FALSE,NULL); 

   for (theModule = EnvGetNextDefmodule(theEnv,execStatus,NULL);
        theModule != NULL;
        theModule = EnvGetNextDefmodule(theEnv,execStatus,theModule))
     {
      theModuleItem = (struct defglobalModule *)
                      GetModuleItem(theEnv,execStatus,(struct defmodule *) theModule,
                                    DefglobalData(theEnv,execStatus)->DefglobalModuleIndex);
      rtn_struct(theEnv,execStatus,defglobalModule,theModuleItem);
     }
#else
   DoForAllConstructs(theEnv,execStatus,DestroyDefglobalAction,DefglobalData(theEnv,execStatus)->DefglobalModuleIndex,FALSE,NULL); 
#endif
  }
  
/***************************************************/
/* DestroyDefglobalAction: Action used to remove   */
/*   defglobals as a result of DestroyEnvironment. */
/***************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void DestroyDefglobalAction(
  void *theEnv,
  EXEC_STATUS,
  struct constructHeader *theConstruct,
  void *buffer)
  {
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(buffer)
#endif
#if (! BLOAD_ONLY)
   struct defglobal *theDefglobal = (struct defglobal *) theConstruct;
   
   if (theDefglobal == NULL) return;

   DestroyDefglobal(theEnv,execStatus,theDefglobal);
#else
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv,execStatus,theConstruct)
#endif
#endif
  }

/*********************************************************/
/* InitializeDefglobalModules: Initializes the defglobal */
/*   construct for use with the defmodule construct.     */
/*********************************************************/
static void InitializeDefglobalModules(
  void *theEnv,
  EXEC_STATUS)
  {
   DefglobalData(theEnv,execStatus)->DefglobalModuleIndex = RegisterModuleItem(theEnv,execStatus,"defglobal",
                                    AllocateModule,
                                    ReturnModule,
#if BLOAD_AND_BSAVE || BLOAD || BLOAD_ONLY
                                    BloadDefglobalModuleReference,
#else
                                    NULL,
#endif
#if CONSTRUCT_COMPILER && (! RUN_TIME)
                                    DefglobalCModuleReference,
#else
                                    NULL,
#endif
                                    EnvFindDefglobal);

#if (! BLOAD_ONLY) && (! RUN_TIME) && DEFMODULE_CONSTRUCT
   AddPortConstructItem(theEnv,execStatus,"defglobal",SYMBOL);
#endif
  }

/*************************************************/
/* AllocateModule: Allocates a defglobal module. */
/*************************************************/
static void *AllocateModule(
  void *theEnv,
  EXEC_STATUS)
  {   
   return((void *) get_struct(theEnv,execStatus,defglobalModule)); 
  }

/***********************************************/
/* ReturnModule: Deallocates a defglobal module. */
/***********************************************/
static void ReturnModule(
  void *theEnv,
  EXEC_STATUS,
  void *theItem)
  {
   FreeConstructHeaderModule(theEnv,execStatus,(struct defmoduleItemHeader *) theItem,DefglobalData(theEnv,execStatus)->DefglobalConstruct);
   rtn_struct(theEnv,execStatus,defglobalModule,theItem);
  }

/**************************************************************/
/* GetDefglobalModuleItem: Returns a pointer to the defmodule */
/*  item for the specified defglobal or defmodule.            */
/**************************************************************/
globle struct defglobalModule *GetDefglobalModuleItem(
  void *theEnv,
  EXEC_STATUS,
  struct defmodule *theModule)
  {
   return((struct defglobalModule *) GetConstructModuleItemByIndex(theEnv,execStatus,theModule,DefglobalData(theEnv,execStatus)->DefglobalModuleIndex));
  }

/*****************************************************/
/* EnvFindDefglobal: Searches for a defglobal in the */
/*   list of defglobals. Returns a pointer to the    */
/*   defglobal if found, otherwise NULL.             */
/*****************************************************/
globle void *EnvFindDefglobal(
  void *theEnv,
  EXEC_STATUS,
  char *defglobalName)
  { 
   return(FindNamedConstruct(theEnv,execStatus,defglobalName,DefglobalData(theEnv,execStatus)->DefglobalConstruct)); 
  }

/********************************************************************/
/* EnvGetNextDefglobal: If passed a NULL pointer, returns the first */
/*   defglobal in the defglobal list. Otherwise returns the next    */
/*   defglobal following the defglobal passed as an argument.       */
/********************************************************************/
globle void *EnvGetNextDefglobal(
  void *theEnv,
  EXEC_STATUS,
  void *defglobalPtr)
  { 
   return((void *) GetNextConstructItem(theEnv,execStatus,(struct constructHeader *) defglobalPtr,DefglobalData(theEnv,execStatus)->DefglobalModuleIndex)); 
  }

/*********************************************************/
/* EnvIsDefglobalDeletable: Returns TRUE if a particular */
/*   defglobal can be deleted, otherwise returns FALSE.  */
/*********************************************************/
globle intBool EnvIsDefglobalDeletable(
  void *theEnv,
  EXEC_STATUS,
  void *ptr)
  {
   if (! ConstructsDeletable(theEnv,execStatus))
     { return FALSE; }

   if (((struct defglobal *) ptr)->busyCount) return(FALSE);

   return(TRUE);
  }

/************************************************************/
/* ReturnDefglobal: Returns the data structures associated  */
/*   with a defglobal construct to the pool of free memory. */
/************************************************************/
static void ReturnDefglobal(
  void *theEnv,
  EXEC_STATUS,
  void *vTheDefglobal)
  {
#if (MAC_MCW || WIN_MCW) && (RUN_TIME || BLOAD_ONLY)
#pragma unused(theEnv,execStatus,vTheDefglobal)
#endif
   
#if (! BLOAD_ONLY) && (! RUN_TIME)
   struct defglobal *theDefglobal = (struct defglobal *) vTheDefglobal;
   
   if (theDefglobal == NULL) return;

   /*====================================*/
   /* Return the global's current value. */
   /*====================================*/

   ValueDeinstall(theEnv,execStatus,&theDefglobal->current);
   if (theDefglobal->current.type == MULTIFIELD)
     { ReturnMultifield(theEnv,execStatus,(struct multifield *) theDefglobal->current.value); }

   /*================================================*/
   /* Return the expression representing the initial */
   /* value of the defglobal when it was defined.    */
   /*================================================*/

   RemoveHashedExpression(theEnv,execStatus,theDefglobal->initial);

   /*===============================*/
   /* Release items stored in the   */
   /* defglobal's construct header. */
   /*===============================*/

   DeinstallConstructHeader(theEnv,execStatus,&theDefglobal->header);

   /*======================================*/
   /* Return the defglobal data structure. */
   /*======================================*/

   rtn_struct(theEnv,execStatus,defglobal,theDefglobal);

   /*===========================================*/
   /* Set the variable indicating that a change */
   /* has been made to a global variable.       */
   /*===========================================*/

   DefglobalData(theEnv,execStatus)->ChangeToGlobals = TRUE;
#endif
  }
  
/************************************************************/
/* DestroyDefglobal: Returns the data structures associated  */
/*   with a defglobal construct to the pool of free memory. */
/************************************************************/
static void DestroyDefglobal(
  void *theEnv,
  EXEC_STATUS,
  void *vTheDefglobal)
  {
#if (MAC_MCW || WIN_MCW) && BLOAD_ONLY
#pragma unused(theEnv,execStatus,vTheDefglobal)
#endif
   
#if (! BLOAD_ONLY)
   struct defglobal *theDefglobal = (struct defglobal *) vTheDefglobal;
   
   if (theDefglobal == NULL) return;

   /*====================================*/
   /* Return the global's current value. */
   /*====================================*/

   if (theDefglobal->current.type == MULTIFIELD)
     { ReturnMultifield(theEnv,execStatus,(struct multifield *) theDefglobal->current.value); }
     
#if (! RUN_TIME)

   /*===============================*/
   /* Release items stored in the   */
   /* defglobal's construct header. */
   /*===============================*/

   DeinstallConstructHeader(theEnv,execStatus,&theDefglobal->header);

   /*======================================*/
   /* Return the defglobal data structure. */
   /*======================================*/

   rtn_struct(theEnv,execStatus,defglobal,theDefglobal);
#endif
#endif
  }
  
/************************************************/
/* QSetDefglobalValue: Lowest level routine for */
/*   setting a defglobal's value.               */
/************************************************/
globle void QSetDefglobalValue(
  void *theEnv,
  EXEC_STATUS,
  struct defglobal *theGlobal,
  DATA_OBJECT_PTR vPtr,
  int resetVar)
  {
   /*====================================================*/
   /* If the new value passed for the defglobal is NULL, */
   /* then reset the defglobal to the initial value it   */
   /* had when it was defined.                           */
   /*====================================================*/

   if (resetVar)
     {
      EvaluateExpression(theEnv,execStatus,theGlobal->initial,vPtr);
      if (execStatus->EvaluationError)
        {
         vPtr->type = SYMBOL;
         vPtr->value = EnvFalseSymbol(theEnv,execStatus);
        }
     }

   /*==========================================*/
   /* If globals are being watch, then display */
   /* the change to the global variable.       */
   /*==========================================*/

#if DEBUGGING_FUNCTIONS
   if (theGlobal->watch)
     {
      EnvPrintRouter(theEnv,execStatus,WTRACE,":== ?*");
      EnvPrintRouter(theEnv,execStatus,WTRACE,ValueToString(theGlobal->header.name));
      EnvPrintRouter(theEnv,execStatus,WTRACE,"* ==> ");
      PrintDataObject(theEnv,execStatus,WTRACE,vPtr);
      EnvPrintRouter(theEnv,execStatus,WTRACE," <== ");
      PrintDataObject(theEnv,execStatus,WTRACE,&theGlobal->current);
      EnvPrintRouter(theEnv,execStatus,WTRACE,"\n");
     }
#endif

   /*==============================================*/
   /* Remove the old value of the global variable. */
   /*==============================================*/

   ValueDeinstall(theEnv,execStatus,&theGlobal->current);
   if (theGlobal->current.type == MULTIFIELD)
     { ReturnMultifield(theEnv,execStatus,(struct multifield *) theGlobal->current.value); }

   /*===========================================*/
   /* Set the new value of the global variable. */
   /*===========================================*/

   theGlobal->current.type = vPtr->type;
   if (vPtr->type != MULTIFIELD) theGlobal->current.value = vPtr->value;
   else DuplicateMultifield(theEnv,execStatus,&theGlobal->current,vPtr);
   ValueInstall(theEnv,execStatus,&theGlobal->current);

   /*===========================================*/
   /* Set the variable indicating that a change */
   /* has been made to a global variable.       */
   /*===========================================*/

   DefglobalData(theEnv,execStatus)->ChangeToGlobals = TRUE;

   if ((execStatus->CurrentEvaluationDepth == 0) && (! CommandLineData(theEnv,execStatus)->EvaluatingTopLevelCommand) &&
       (execStatus->CurrentExpression == NULL))
     { PeriodicCleanup(theEnv,execStatus,TRUE,FALSE); }
  }

/**************************************************************/
/* QFindDefglobal: Searches for a defglobal in the list of    */
/*   defglobals. Returns a pointer to the defglobal if found, */
/*   otherwise NULL.                                          */
/**************************************************************/
globle struct defglobal *QFindDefglobal(
  void *theEnv,
  EXEC_STATUS,
  SYMBOL_HN *defglobalName)
  {
   struct defglobal *theDefglobal;

   for (theDefglobal = (struct defglobal *) EnvGetNextDefglobal(theEnv,execStatus,NULL);
        theDefglobal != NULL;
        theDefglobal = (struct defglobal *) EnvGetNextDefglobal(theEnv,execStatus,theDefglobal))
     { if (defglobalName == theDefglobal->header.name) return (theDefglobal); }

   return(NULL);
  }

/*********************************************************************/
/* EnvGetDefglobalValueForm: Returns the pretty print representation */
/*   of the current value of the specified defglobal. For example,   */
/*   if the current value of ?*x* is 5, the string "?*x* = 5" would  */
/*   be returned.                                                    */
/*********************************************************************/
globle void EnvGetDefglobalValueForm(
  void *theEnv,
  EXEC_STATUS,
  char *buffer,
  unsigned bufferLength,
  void *vTheGlobal)
  {
   struct defglobal *theGlobal = (struct defglobal *) vTheGlobal;

   OpenStringDestination(theEnv,execStatus,"GlobalValueForm",buffer,bufferLength);
   EnvPrintRouter(theEnv,execStatus,"GlobalValueForm","?*");
   EnvPrintRouter(theEnv,execStatus,"GlobalValueForm",ValueToString(theGlobal->header.name));
   EnvPrintRouter(theEnv,execStatus,"GlobalValueForm","* = ");
   PrintDataObject(theEnv,execStatus,"GlobalValueForm",&theGlobal->current);
   CloseStringDestination(theEnv,execStatus,"GlobalValueForm");
  }

/************************************************************/
/* EnvGetGlobalsChanged: Returns the defglobal change flag. */
/************************************************************/
globle int EnvGetGlobalsChanged(
  void *theEnv,
  EXEC_STATUS)
  {    
   return(DefglobalData(theEnv,execStatus)->ChangeToGlobals); 
  }

/*********************************************************/
/* EnvSetGlobalsChanged: Sets the defglobal change flag. */
/*********************************************************/
globle void EnvSetGlobalsChanged(
  void *theEnv,
  EXEC_STATUS,
  int value)
  {
   DefglobalData(theEnv,execStatus)->ChangeToGlobals = value; 
  }

/**********************************************************/
/* GetDefglobalValue2: Returns the value of the specified */
/*   global variable in the supplied DATA_OBJECT.         */
/**********************************************************/
static intBool GetDefglobalValue2(
  void *theEnv,
  EXEC_STATUS,
  void *theValue,
  DATA_OBJECT_PTR vPtr)
  {
   struct defglobal *theGlobal;
   int count;

   /*===========================================*/
   /* Search for the specified defglobal in the */
   /* modules visible to the current module.    */
   /*===========================================*/

   theGlobal = (struct defglobal *)
               FindImportedConstruct(theEnv,execStatus,"defglobal",NULL,ValueToString(theValue),
               &count,TRUE,NULL);

   /*=============================================*/
   /* If it wasn't found, print an error message. */
   /*=============================================*/

   if (theGlobal == NULL)
     {
      PrintErrorID(theEnv,execStatus,"GLOBLDEF",1,FALSE);
      EnvPrintRouter(theEnv,execStatus,WERROR,"Global variable ?*");
      EnvPrintRouter(theEnv,execStatus,WERROR,ValueToString(theValue));
      EnvPrintRouter(theEnv,execStatus,WERROR,"* is unbound.\n");
      vPtr->type = SYMBOL;
      vPtr->value = EnvFalseSymbol(theEnv,execStatus);
      SetEvaluationError(theEnv,execStatus,TRUE);
      return(FALSE);
     }

   /*========================================================*/
   /* The current implementation of the defmodules shouldn't */
   /* allow a construct to be defined which would cause an   */
   /* ambiguous reference, but we'll check for it anyway.    */
   /*========================================================*/

   if (count > 1)
     {
      AmbiguousReferenceErrorMessage(theEnv,execStatus,"defglobal",ValueToString(theValue));
      vPtr->type = SYMBOL;
      vPtr->value = EnvFalseSymbol(theEnv,execStatus);
      SetEvaluationError(theEnv,execStatus,TRUE);
      return(FALSE);
     }

   /*=================================*/
   /* Get the value of the defglobal. */
   /*=================================*/

   QGetDefglobalValue(theEnv,execStatus,theGlobal,vPtr);

   return(TRUE);
  }

/***************************************************************/
/* QGetDefglobalValue: Returns the value of a global variable. */
/***************************************************************/
globle int QGetDefglobalValue(
  void *theEnv,
  EXEC_STATUS,
  void *vTheGlobal,
  DATA_OBJECT_PTR vPtr)
  {
   struct defglobal *theGlobal = (struct defglobal *) vTheGlobal;

   /*===============================================*/
   /* Transfer values which can be copied directly. */
   /*===============================================*/

   vPtr->type = theGlobal->current.type;
   vPtr->value = theGlobal->current.value;
   vPtr->begin = theGlobal->current.begin;
   vPtr->end = theGlobal->current.end;

   /*===========================================================*/
   /* If the global contains a multifield value, return a copy  */
   /* of the value so that routines which use this value are    */
   /* not affected if the value of the global is later changed. */
   /*===========================================================*/

   if (vPtr->type == MULTIFIELD)
     {
      vPtr->value = EnvCreateMultifield(theEnv,execStatus,(unsigned long) (vPtr->end + 1));
      GenCopyMemory(struct field,vPtr->end + 1,
                                &((struct multifield *) vPtr->value)->theFields[0],
                                &((struct multifield *) theGlobal->current.value)->theFields[theGlobal->current.begin]);
     }

   return(TRUE);
  }

/************************************************************/
/* EnvGetDefglobalValue: Returns the value of the specified */
/*   global variable in the supplied DATA_OBJECT.           */
/************************************************************/
globle intBool EnvGetDefglobalValue(
  void *theEnv,
  EXEC_STATUS,
  char *variableName,
  DATA_OBJECT_PTR vPtr)
  {
   struct defglobal *theDefglobal;

   if ((theDefglobal = (struct defglobal *) EnvFindDefglobal(theEnv,execStatus,variableName)) == NULL)
     { return(FALSE); }

   QGetDefglobalValue(theEnv,execStatus,theDefglobal,vPtr);

   return(TRUE);
  }

/****************************************************************/
/* EnvSetDefglobalValue: Sets the value of the specified global */
/*   variable to the value stored in the supplied DATA_OBJECT.  */
/****************************************************************/
globle intBool EnvSetDefglobalValue(
  void *theEnv,
  EXEC_STATUS,
  char *variableName,
  DATA_OBJECT_PTR vPtr)
  {
   struct defglobal *theGlobal;

   if ((theGlobal = QFindDefglobal(theEnv,execStatus,(SYMBOL_HN *) EnvAddSymbol(theEnv,execStatus,variableName))) == NULL)
     { return(FALSE); }

   QSetDefglobalValue(theEnv,execStatus,theGlobal,vPtr,FALSE);

   return(TRUE);
  }

/**********************************************************/
/* DecrementDefglobalBusyCount: Decrements the busy count */
/*   of a defglobal data structure.                       */
/**********************************************************/
static void DecrementDefglobalBusyCount(
  void *theEnv,
  EXEC_STATUS,
  void *vTheGlobal)
  {
   struct defglobal *theGlobal = (struct defglobal *) vTheGlobal;

   if (! ConstructData(theEnv,execStatus)->ClearInProgress) theGlobal->busyCount--;
  }

/**********************************************************/
/* IncrementDefglobalBusyCount: Increments the busy count */
/*   of a defglobal data structure.                       */
/**********************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void IncrementDefglobalBusyCount(
  void *theEnv,
  EXEC_STATUS,
  void *vTheGlobal)
  {
   struct defglobal *theGlobal = (struct defglobal *) vTheGlobal;
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv,execStatus)
#endif

   theGlobal->busyCount++;
  }

/***********************************************************************/
/* UpdateDefglobalScope: Updates the scope flag of all the defglobals. */
/***********************************************************************/
globle void UpdateDefglobalScope(
  void *theEnv,
  EXEC_STATUS)
  {
   struct defglobal *theDefglobal;
   int moduleCount;
   struct defmodule *theModule;
   struct defmoduleItemHeader *theItem;
   
   /*============================*/
   /* Loop through every module. */
   /*============================*/

   for (theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,NULL);
        theModule != NULL;
        theModule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,theModule))
     {
      /*============================================================*/
      /* Loop through every defglobal in the module being examined. */
      /*============================================================*/

      theItem = (struct defmoduleItemHeader *)
                GetModuleItem(theEnv,execStatus,theModule,DefglobalData(theEnv,execStatus)->DefglobalModuleIndex);

      for (theDefglobal = (struct defglobal *) theItem->firstItem;
           theDefglobal != NULL ;
           theDefglobal = (struct defglobal *) EnvGetNextDefglobal(theEnv,execStatus,theDefglobal))
        {
         /*====================================================*/
         /* If the defglobal is visible to the current module, */
         /* then mark it as being in scope, otherwise mark it  */
         /* as being out of scope.                             */
         /*====================================================*/

         if (FindImportedConstruct(theEnv,execStatus,"defglobal",theModule,
                                   ValueToString(theDefglobal->header.name),
                                   &moduleCount,TRUE,NULL) != NULL)
           { theDefglobal->inScope = TRUE; }
         else
           { theDefglobal->inScope = FALSE; }
        }
     }
  }

/*******************************************************/
/* GetNextDefglobalInScope: Returns the next defglobal */
/*   that is scope of the current module. Works in a   */
/*   similar fashion to GetNextDefglobal, but skips    */
/*   defglobals that are out of scope.                 */
/*******************************************************/
globle void *GetNextDefglobalInScope(
  void *theEnv,
  EXEC_STATUS,
  void *vTheGlobal)
  {
   struct defglobal *theGlobal = (struct defglobal *) vTheGlobal;
   struct defmoduleItemHeader *theItem;

   /*=======================================*/
   /* If we're beginning the search for the */
   /* first defglobal in scope, then ...    */
   /*=======================================*/

   if (theGlobal == NULL)
     {
      /*==============================================*/
      /* If the current module has been changed since */
      /* the last time the scopes were computed, then */
      /* recompute the scopes.                        */
      /*==============================================*/

      if (DefglobalData(theEnv,execStatus)->LastModuleIndex != DefmoduleData(theEnv,execStatus)->ModuleChangeIndex)
        {
         UpdateDefglobalScope(theEnv,execStatus);
         DefglobalData(theEnv,execStatus)->LastModuleIndex = DefmoduleData(theEnv,execStatus)->ModuleChangeIndex;
        }

      /*==========================================*/
      /* Get the first module and first defglobal */
      /* to start the search with.                */
      /*==========================================*/

      DefglobalData(theEnv,execStatus)->TheDefmodule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,NULL);
      theItem = (struct defmoduleItemHeader *)
                GetModuleItem(theEnv,execStatus,DefglobalData(theEnv,execStatus)->TheDefmodule,DefglobalData(theEnv,execStatus)->DefglobalModuleIndex);
      theGlobal = (struct defglobal *) theItem->firstItem;
     }

   /*==================================================*/
   /* Otherwise, see if the last defglobal returned by */
   /* this function has a defglobal following it.      */
   /*==================================================*/

   else
     { theGlobal = (struct defglobal *) EnvGetNextDefglobal(theEnv,execStatus,theGlobal); }

   /*======================================*/
   /* Continue looping through the modules */
   /* until a defglobal in scope is found. */
   /*======================================*/

   while (DefglobalData(theEnv,execStatus)->TheDefmodule != NULL)
     {
      /*=====================================================*/
      /* Loop through the defglobals in the module currently */
      /* being examined to see if one is in scope.           */
      /*=====================================================*/

      for (;
           theGlobal != NULL;
           theGlobal = (struct defglobal *) EnvGetNextDefglobal(theEnv,execStatus,theGlobal))
        { if (theGlobal->inScope) return((void *) theGlobal); }

      /*================================================*/
      /* If a global in scope couldn't be found in this */
      /* module, then move on to the next module.       */
      /*================================================*/

      DefglobalData(theEnv,execStatus)->TheDefmodule = (struct defmodule *) EnvGetNextDefmodule(theEnv,execStatus,DefglobalData(theEnv,execStatus)->TheDefmodule);
      theItem = (struct defmoduleItemHeader *)
                GetModuleItem(theEnv,execStatus,DefglobalData(theEnv,execStatus)->TheDefmodule,DefglobalData(theEnv,execStatus)->DefglobalModuleIndex);
      theGlobal = (struct defglobal *) theItem->firstItem;
     }

   /*====================================*/
   /* All the globals in scope have been */
   /* traversed and there are none left. */
   /*====================================*/

   return(NULL);
  }

#endif /* DEFGLOBAL_CONSTRUCT */



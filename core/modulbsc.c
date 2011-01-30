   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*         DEFMODULE BASIC COMMANDS HEADER FILE        */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements core commands for the defmodule       */
/*   construct such as clear, reset, save, ppdefmodule       */
/*   list-defmodules, and get-defmodule-list.                */
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

#define _MODULBSC_SOURCE_

#include "setup.h"

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include "constrct.h"
#include "extnfunc.h"
#include "modulbin.h"
#include "prntutil.h"
#include "modulcmp.h"
#include "router.h"
#include "argacces.h"
#include "bload.h"

#include "modulbsc.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    ClearDefmodules(void);
#if DEFMODULE_CONSTRUCT
   static void                    SaveDefmodules(char *);
#endif

/*****************************************************************/
/* DefmoduleBasicCommands: Initializes basic defmodule commands. */
/*****************************************************************/
globle void DefmoduleBasicCommands()
  {
   AddClearFunction("defmodule",ClearDefmodules,2000);

#if DEFMODULE_CONSTRUCT
   AddSaveFunction("defmodule",SaveDefmodules,1100);

#if ! RUN_TIME
   DefineFunction2("get-defmodule-list",'m',PTIF GetDefmoduleList,"GetDefmoduleList","00");

#if DEBUGGING_FUNCTIONS
   DefineFunction2("list-defmodules",'v', PTIF ListDefmodulesCommand,"ListDefmodulesCommand","00");
   DefineFunction2("ppdefmodule",'v',PTIF PPDefmoduleCommand,"PPDefmoduleCommand","11w");
#endif
#endif
#endif

#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)
   DefmoduleBinarySetup();
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
   DefmoduleCompilerSetup();
#endif
  }

/*********************************************************/
/* ClearDefmodules: Defmodule clear routine for use with */
/*   the clear command. Creates the MAIN module.         */
/*********************************************************/
static void ClearDefmodules()
  {
#if (BLOAD || BLOAD_AND_BSAVE || BLOAD_ONLY) && (! RUN_TIME)
   if (Bloaded() == TRUE) return;
#endif
#if (! RUN_TIME)
   RemoveAllDefmodules();
#endif
#if (! RUN_TIME)
   CreateMainModule();
   MainModuleRedefinable = TRUE;
#endif
  }

#if DEFMODULE_CONSTRUCT

/******************************************/
/* SaveDefmodules: Defmodule save routine */
/*   for use with the save command.       */
/******************************************/
static void SaveDefmodules(
  char *logicalName)
  {
   void *defmodulePtr;
   char *ppform;

   for (defmodulePtr = GetNextDefmodule(NULL);
        defmodulePtr != NULL;
        defmodulePtr = GetNextDefmodule(defmodulePtr))
     {
      ppform = GetDefmodulePPForm(defmodulePtr);
      if (ppform != NULL)
        {
         PrintInChunks(logicalName,ppform);
         PrintRouter(logicalName,"\n");
        }
     }
  }

/**********************************************/
/* GetDefmoduleList: H/L and C access routine */
/*   for the get-defmodule-list function.     */
/**********************************************/
globle void GetDefmoduleList(
  DATA_OBJECT_PTR returnValue)
  { OldGetConstructList(returnValue,GetNextDefmodule,GetDefmoduleName); }

#if DEBUGGING_FUNCTIONS

/********************************************/
/* PPDefmoduleCommand: H/L access routine   */
/*   for the ppdefmodule command.           */
/********************************************/
globle void PPDefmoduleCommand()
  {
   char *defmoduleName;

   defmoduleName = GetConstructName("ppdefmodule","defmodule name");
   if (defmoduleName == NULL) return;

   PPDefmodule(defmoduleName,WDISPLAY);

   return;
  }

/*************************************/
/* PPDefmodule: C access routine for */
/*   the ppdefmodule command.        */
/*************************************/
globle int PPDefmodule(
  char *defmoduleName,
  char *logicalName)
  {
   void *defmodulePtr;

   defmodulePtr = FindDefmodule(defmoduleName);
   if (defmodulePtr == NULL)
     {
      CantFindItemErrorMessage("defmodule",defmoduleName);
      return(FALSE);
     }

   if (GetDefmodulePPForm(defmodulePtr) == NULL) return(TRUE);
   PrintInChunks(logicalName,GetDefmodulePPForm(defmodulePtr));
   return(TRUE);
  }

/***********************************************/
/* ListDefmodulesCommand: H/L access routine   */
/*   for the list-defmodules command.          */
/***********************************************/
globle void ListDefmodulesCommand()
  {
   if (ArgCountCheck("list-defmodules",EXACTLY,0) == -1) return;

   ListDefmodules(WDISPLAY);
  }

/****************************************/
/* ListDefmodules: C access routine for */
/*   the list-defmodules command.       */
/****************************************/
globle void ListDefmodules(
  char *logicalName)
  {
   void *theModule;
   int count = 0;

   for (theModule = GetNextDefmodule(NULL);
        theModule != NULL;
        theModule = GetNextDefmodule(theModule))
    {
     PrintRouter(logicalName,GetDefmoduleName(theModule));
     PrintRouter(logicalName,"\n");
     count++;
    }

   PrintTally(logicalName,count,"defmodule","defmodules");
  }

#endif /* DEBUGGING_FUNCTIONS */

#endif /* DEFMODULE_CONSTRUCT */



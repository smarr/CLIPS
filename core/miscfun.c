   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*            MISCELLANEOUS FUNCTIONS MODULE           */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
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

#define _MISCFUN_SOURCE_

#include <windows.h>

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include "setup.h"

#include "memalloc.h"
#include "sysdep.h"
#include "multifld.h"
#include "exprnpsr.h"
#include "argacces.h"
#include "router.h"
#include "utility.h"

#if DEFFUNCTION_CONSTRUCT
#include "dffnxfun.h"
#endif

#include "miscfun.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    ExpandFuncMultifield(DATA_OBJECT *,EXPRESSION *,
                                                       EXPRESSION **,void *);
   long							  GetThreadID(void);

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static long int         GensymNumber = 1;

#if ! RUN_TIME
/*****************************************************************/
/* MiscFunctionDefinitions: Initializes miscellaneous functions. */
/*****************************************************************/
globle void MiscFunctionDefinitions()
  {
   DefineFunction2("gensym",           'w', PTIF GensymFunction,      "GensymFunction", "00");
   DefineFunction2("gensym*",          'w', PTIF GensymStarFunction,  "GensymStarFunction", "00");
   DefineFunction2("setgen",           'l', PTIF SetgenFunction,      "SetgenFunction", "11i");
   DefineFunction2("system",           'v', PTIF gensystem,           "gensystem", "1*k");
   DefineFunction2("length",           'l', PTIF LengthFunction,      "LengthFunction", "11q");
   DefineFunction2("length$",          'l', PTIF LengthFunction,      "LengthFunction", "11q");
   DefineFunction2("time",             'd', PTIF gentime,             "gentime", "00");
   DefineFunction2("random",           'l', PTIF RandomFunction,      "RandomFunction", "00");
   DefineFunction2("seed",             'v', PTIF SeedFunction,        "SeedFunction", "11i");
   DefineFunction2("conserve-mem",     'v', PTIF ConserveMemCommand,  "ConserveMemCommand", "11w");
   DefineFunction2("release-mem",      'l', PTIF ReleaseMemCommand,   "ReleaseMemCommand", "00");
#if DEBUGGING_FUNCTIONS
   DefineFunction2("mem-used",         'l', PTIF MemUsedCommand,      "MemUsedCommand", "00");
   DefineFunction2("mem-requests",     'l', PTIF MemRequestsCommand,  "MemRequestsCommand", "00");
#endif
   DefineFunction2("options",          'v', PTIF OptionsCommand,      "OptionsCommand", "00");
   DefineFunction2("(expansion-call)", 'u', PTIF ExpandFuncCall,      "ExpandFuncCall",NULL);
   DefineFunction2("expand$",'u', PTIF DummyExpandFuncMultifield,
                                           "DummyExpandFuncMultifield","11m");
   FuncSeqOvlFlags("expand$",FALSE,FALSE);
   DefineFunction2("(set-evaluation-error)",
                                       'w', PTIF CauseEvaluationError,"CauseEvaluationError",NULL);
   DefineFunction2("set-sequence-operator-recognition",
                                       'b', PTIF SetSORCommand,"SetSORCommand","11w");
   DefineFunction2("get-sequence-operator-recognition",'b',
                    PTIF GetSequenceOperatorRecognition,"GetSequenceOperatorRecognition","00");
   DefineFunction2("get-function-restrictions",'s',
                   PTIF GetFunctionRestrictions,"GetFunctionRestrictions","11w");
   DefineFunction2("create$",     'm', PTIF CreateFunction,  "CreateFunction", NULL);
   DefineFunction2("mv-append",   'm', PTIF CreateFunction,  "CreateFunction", NULL);
   DefineFunction2("apropos",   'v', PTIF AproposCommand,  "AproposCommand", "11w");
   DefineFunction2("get-function-list",   'm', PTIF GetFunctionListFunction,  "GetFunctionListFunction", "00");
   DefineFunction2("GetCurrentThreadID", 'l', PTIF GetThreadID, "GetThreadID", "00");
  }
#endif

/******************************************************************/
/* CreateFunction: H/L access routine for the create$ function.   */
/******************************************************************/
globle void CreateFunction(
  DATA_OBJECT_PTR returnValue)
  {
   StoreInMultifield(returnValue,GetFirstArgument(),TRUE);
  }

/*****************************************************************/
/* SetgenFunction: H/L access routine for the setgen function.   */
/*****************************************************************/
globle long int SetgenFunction()
  {
   long theLong;
   DATA_OBJECT theValue;

   /*==========================================================*/
   /* Check to see that a single integer argument is provided. */
   /*==========================================================*/

   if (ArgCountCheck("setgen",EXACTLY,1) == -1) return(GensymNumber);
   if (ArgTypeCheck("setgen",1,INTEGER,&theValue) == FALSE) return(GensymNumber);

   /*========================================*/
   /* The integer must be greater than zero. */
   /*========================================*/

   theLong = ValueToLong(theValue.value);

   if (theLong < 1L)
     {
      ExpectedTypeError1("setgen",1,"number (greater than or equal to 1)");
      return(GensymNumber);
     }

   /*====================================*/
   /* Set the gensym index to the number */
   /* provided and return this value.    */
   /*====================================*/

   GensymNumber = theLong;
   return(theLong);
  }

/****************************************/
/* GensymFunction: H/L access routine   */
/*   for the gensym function.           */
/****************************************/
globle void *GensymFunction()
  {
   char genstring[15];

   /*===========================================*/
   /* The gensym function accepts no arguments. */
   /*===========================================*/

   ArgCountCheck("gensym",EXACTLY,0);

   /*================================================*/
   /* Create a symbol using the current gensym index */
   /* as the postfix.                                */
   /*================================================*/

   sprintf(genstring,"gen%ld",GensymNumber);
   GensymNumber++;

   /*====================*/
   /* Return the symbol. */
   /*====================*/

   return(AddSymbol(genstring));
  }

/************************************************/
/* GensymStarFunction: H/L access routine for   */
/*   the gensym* function.                      */
/************************************************/
globle void *GensymStarFunction()
  {
   /*============================================*/
   /* The gensym* function accepts no arguments. */
   /*============================================*/

   ArgCountCheck("gensym*",EXACTLY,0);

   /*====================*/
   /* Return the symbol. */
   /*====================*/

   return(GensymStar());
  }

/************************************/
/* GensymStar: C access routine for */
/*   the gensym* function.          */
/************************************/
globle void *GensymStar()
  {
   char genstring[15];

   /*=======================================================*/
   /* Create a symbol using the current gensym index as the */
   /* postfix. If the symbol is already present in the      */
   /* symbol table, then continue generating symbols until  */
   /* a unique symbol is found.                             */
   /*=======================================================*/

   do
     {
      sprintf(genstring,"gen%ld",GensymNumber);
      GensymNumber++;
     }
   while (FindSymbol(genstring) != NULL);

   /*====================*/
   /* Return the symbol. */
   /*====================*/

   return(AddSymbol(genstring));
  }

/********************************************/
/* RandomFunction: H/L access routine for   */
/*   the random function.                   */
/********************************************/
globle long RandomFunction()
  {
   /*===========================================*/
   /* The random function accepts no arguments. */
   /*===========================================*/

   ArgCountCheck("random",EXACTLY,0);

   /*========================================*/
   /* Return the randomly generated integer. */
   /*========================================*/

   return((long) genrand());
  }

/******************************************/
/* SeedFunction: H/L access routine for   */
/*   the seed function.                   */
/******************************************/
globle void SeedFunction()
  {
   DATA_OBJECT theValue;

   /*==========================================================*/
   /* Check to see that a single integer argument is provided. */
   /*==========================================================*/

   if (ArgCountCheck("seed",EXACTLY,1) == -1) return;
   if (ArgTypeCheck("seed",1,INTEGER,&theValue) == FALSE) return;

   /*=============================================================*/
   /* Seed the random number generator with the provided integer. */
   /*=============================================================*/

   genseed((int) DOToLong(theValue));
  }

/********************************************/
/* LengthFunction: H/L access routine for   */
/*   the length$ function.                  */
/********************************************/
globle long int LengthFunction()
  {
   DATA_OBJECT item;

   /*====================================================*/
   /* The length$ function expects exactly one argument. */
   /*====================================================*/

   if (ArgCountCheck("length$",EXACTLY,1) == -1) return(-1L);
   RtnUnknown(1,&item);

   /*====================================================*/
   /* If the argument is a string or symbol, then return */
   /* the number of characters in the argument.          */
   /*====================================================*/

   if ((GetType(item) == STRING) || (GetType(item) == SYMBOL))
     {  return( (long) strlen(DOToString(item))); }

   /*====================================================*/
   /* If the argument is a multifield value, then return */
   /* the number of fields in the argument.              */
   /*====================================================*/

   if (GetType(item) == MULTIFIELD)
     { return ( (long) GetDOLength(item)); }

   /*=============================================*/
   /* If the argument wasn't a string, symbol, or */
   /* multifield value, then generate an error.   */
   /*=============================================*/

   SetEvaluationError(TRUE);
   ExpectedTypeError2("length$",1);
   return(-1L);
  }

/*******************************************/
/* ReleaseMemCommand: H/L access routine   */
/*   for the release-mem function.         */
/*******************************************/
globle long ReleaseMemCommand()
  {
   /*================================================*/
   /* The release-mem function accepts no arguments. */
   /*================================================*/

   if (ArgCountCheck("release-mem",EXACTLY,0) == -1) return(0);

   /*========================================*/
   /* Release memory to the operating system */
   /* and return the amount of memory freed. */
   /*========================================*/

   return(ReleaseMem(-1L,FALSE));
  }

/******************************************/
/* ConserveMemCommand: H/L access routine */
/*   for the conserve-mem command.        */
/******************************************/
globle void ConserveMemCommand()
  {
   char *argument;
   DATA_OBJECT theValue;

   /*===================================*/
   /* The conserve-mem function expects */
   /* a single symbol argument.         */
   /*===================================*/

   if (ArgCountCheck("conserve-mem",EXACTLY,1) == -1) return;
   if (ArgTypeCheck("conserve-mem",1,SYMBOL,&theValue) == FALSE) return;

   argument = DOToString(theValue);

   /*====================================================*/
   /* If the argument is the symbol "on", then store the */
   /* pretty print representation of a construct when it */
   /* is defined.                                        */
   /*====================================================*/

   if (strcmp(argument,"on") == 0)
     { SetConserveMemory(TRUE); }

   /*======================================================*/
   /* Otherwise, if the argument is the symbol "off", then */
   /* don't store the pretty print representation of a     */
   /* construct when it is defined.                        */
   /*======================================================*/

   else if (strcmp(argument,"off") == 0)
     { SetConserveMemory(FALSE); }

   /*=====================================================*/
   /* Otherwise, generate an error since the only allowed */
   /* arguments are "on" or "off."                        */
   /*=====================================================*/

   else
     {
      ExpectedTypeError1("conserve-mem",1,"symbol with value on or off");
      return;
     }

   return;
  }

#if DEBUGGING_FUNCTIONS

/****************************************/
/* MemUsedCommand: H/L access routine   */
/*   for the mem-used command.          */
/****************************************/
globle long int MemUsedCommand()
  {
   /*=============================================*/
   /* The mem-used function accepts no arguments. */
   /*=============================================*/

   if (ArgCountCheck("mem-used",EXACTLY,0) == -1) return(0);

   /*============================================*/
   /* Return the amount of memory currently held */
   /* (both for current use and for later use).  */
   /*============================================*/

   return(MemUsed());
  }

/********************************************/
/* MemRequestsCommand: H/L access routine   */
/*   for the mem-requests command.          */
/********************************************/
globle long int MemRequestsCommand()
  {
   /*=================================================*/
   /* The mem-requests function accepts no arguments. */
   /*=================================================*/

   if (ArgCountCheck("mem-requests",EXACTLY,0) == -1) return(0);

   /*==================================*/
   /* Return the number of outstanding */
   /* memory requests.                 */
   /*==================================*/

   return(MemRequests());
  }

#endif

/****************************************/
/* AproposCommand: H/L access routine   */
/*   for the apropos command.           */
/****************************************/
globle void AproposCommand()
  {
   char *argument;
   DATA_OBJECT argPtr;
   struct symbolHashNode *hashPtr = NULL;
   int theLength;

   /*=======================================================*/
   /* The apropos command expects a single symbol argument. */
   /*=======================================================*/

   if (ArgCountCheck("apropos",EXACTLY,1) == -1) return;
   if (ArgTypeCheck("apropos",1,SYMBOL,&argPtr) == FALSE) return;

   /*=======================================*/
   /* Determine the length of the argument. */
   /*=======================================*/

   argument = DOToString(argPtr);
   theLength = strlen(argument);

   /*====================================================================*/
   /* Print each entry in the symbol table that contains the argument as */
   /* a substring. When using a non-ANSI compiler, only those strings    */
   /* that contain the substring starting at the beginning of the string */
   /* are printed.                                                       */
   /*====================================================================*/

   while ((hashPtr = GetNextSymbolMatch(argument,theLength,hashPtr,TRUE,NULL)) != NULL)
     {
      PrintRouter(WDISPLAY,ValueToString(hashPtr));
      PrintRouter(WDISPLAY,"\n");
     }
  }

/****************************************/
/* OptionsCommand: H/L access routine   */
/*   for the options command.           */
/****************************************/
globle void OptionsCommand()
  {
   /*===========================================*/
   /* The options command accepts no arguments. */
   /*===========================================*/

   if (ArgCountCheck("options",EXACTLY,0) == -1) return;

   /*=================================*/
   /* Print the state of the compiler */
   /* flags for this executable.      */
   /*=================================*/

   PrintRouter(WDISPLAY,"Machine type: ");

#if GENERIC
   PrintRouter(WDISPLAY,"Generic ");
#endif
#if VAX_VMS
   PrintRouter(WDISPLAY,"VAX VMS ");
#endif
#if UNIX_V
   PrintRouter(WDISPLAY,"UNIX System V or 4.2BSD ");
#endif
#if UNIX_7
   PrintRouter(WDISPLAY,"UNIX System III Version 7 or Sun Unix ");
#endif
#if MAC_SC6
   PrintRouter(WDISPLAY,"Apple Macintosh with Symantec C 6.0");
#endif
#if MAC_SC7
   PrintRouter(WDISPLAY,"Apple Macintosh with Symantec C 7.0");
#endif
#if MAC_SC8
   PrintRouter(WDISPLAY,"Apple Macintosh with Symantec C 8.0");
#endif
#if MAC_MPW
   PrintRouter(WDISPLAY,"Apple Macintosh with MPW C");
#endif
#if MAC_MCW
   PrintRouter(WDISPLAY,"Apple Macintosh with CodeWarrior");
#endif
#if IBM_MSC
   PrintRouter(WDISPLAY,"IBM PC with Microsoft C");
#endif
#if IBM_ZTC
   PrintRouter(WDISPLAY,"IBM PC with Zortech C");
#endif
#if IBM_SC
   PrintRouter(WDISPLAY,"IBM PC with Symantec C++");
#endif
#if IBM_ICB
   PrintRouter(WDISPLAY,"IBM PC with Intel C Code Builder");
#endif
#if IBM_TBC
   PrintRouter(WDISPLAY,"IBM PC with Turbo C");
#endif
#if IBM_MCW
   PrintRouter(WDISPLAY,"IBM PC with Metrowerks CodeWarrior");
#endif
PrintRouter(WDISPLAY,"\n");

PrintRouter(WDISPLAY,"Defrule construct is ");
#if DEFRULE_CONSTRUCT
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

#if DEFRULE_CONSTRUCT

PrintRouter(WDISPLAY,"  Conflict resolution strategies are ");
#if CONFLICT_RESOLUTION_STRATEGIES
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"  Dynamic salience is ");
#if DYNAMIC_SALIENCE
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"  Incremental reset is ");
#if INCREMENTAL_RESET
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"  Logical dependencies (truth maintenance) are ");
#if LOGICAL_DEPENDENCIES
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

#endif

PrintRouter(WDISPLAY,"Defmodule construct is ");
#if DEFMODULE_CONSTRUCT
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"Deftemplate construct is ");
#if DEFTEMPLATE_CONSTRUCT
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

#if DEFTEMPLATE_CONSTRUCT

PrintRouter(WDISPLAY,"  Deffacts construct is ");
#if DEFFACTS_CONSTRUCT
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

#endif

PrintRouter(WDISPLAY,"Defglobal construct is ");
#if DEFGLOBAL_CONSTRUCT
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"Deffunction construct is ");
#if DEFFUNCTION_CONSTRUCT
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"Defgeneric/Defmethod constructs are ");
#if DEFGENERIC_CONSTRUCT
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

#if DEFGENERIC_CONSTRUCT

PrintRouter(WDISPLAY,"  Imperative methods are ");
#if IMPERATIVE_METHODS
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

#endif

PrintRouter(WDISPLAY,"Object System is ");
#if OBJECT_SYSTEM
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

#if OBJECT_SYSTEM

PrintRouter(WDISPLAY,"  Definstances construct is ");
#if DEFINSTANCES_CONSTRUCT
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"  Imperative (around/shadowed) message-handlers are ");
#if IMPERATIVE_MESSAGE_HANDLERS
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"  Auxiliary (before/after) message-handlers are ");
#if AUXILIARY_MESSAGE_HANDLERS
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"  Instance-set queries are ");
#if INSTANCE_SET_QUERIES
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"  Direct pattern-matching on instances is ");
#if INSTANCE_PATTERN_MATCHING
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"  Binary loading of instances is ");
#if BLOAD_INSTANCES
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"  Binary saving of instances is ");
#if BSAVE_INSTANCES
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

#endif

PrintRouter(WDISPLAY,"Extended math package is ");
#if EX_MATH
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"Text processing package is ");
#if TEXTPRO_FUNCTIONS
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"Help system is ");
#if HELP_FUNCTIONS
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"Bload capability is ");
#if BLOAD_ONLY
  PrintRouter(WDISPLAY,"BLOAD ONLY");
#endif
#if BLOAD
  PrintRouter(WDISPLAY,"BLOAD");
#endif
#if BLOAD_AND_BSAVE
  PrintRouter(WDISPLAY,"BLOAD AND BSAVE");
#endif
#if (! BLOAD_ONLY) && (! BLOAD) && (! BLOAD_AND_BSAVE)
  PrintRouter(WDISPLAY,"OFF ");
#endif
PrintRouter(WDISPLAY,"\n");

PrintRouter(WDISPLAY,"EMACS Editor is ");
#if EMACS_EDITOR
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"Construct compiler is ");
#if CONSTRUCT_COMPILER
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"Basic I/O is ");
#if BASIC_IO
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"Extended I/O is ");
#if EXT_IO
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"String function package is ");
#if STRING_FUNCTIONS
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"Multifield function package is ");
#if MULTIFIELD_FUNCTIONS
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"Debugging functions are ");
#if DEBUGGING_FUNCTIONS
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"Block memory is ");
#if BLOCK_MEMORY
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"Window Interface flag is ");
#if WINDOW_INTERFACE
   PrintRouter(WDISPLAY,"ON\n");
#else
   PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"Short link names are ");
#if SHORT_LINK_NAMES
   PrintRouter(WDISPLAY,"ON\n");
#else
   PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"Developer flag is ");
#if DEVELOPER
   PrintRouter(WDISPLAY,"ON\n");
#else
   PrintRouter(WDISPLAY,"OFF\n");
#endif

PrintRouter(WDISPLAY,"Run time module is ");
#if RUN_TIME
  PrintRouter(WDISPLAY,"ON\n");
#else
  PrintRouter(WDISPLAY,"OFF\n");
#endif
  }

/********************************************************************
  NAME         : ExpandFuncCall
  DESCRIPTION  : This function is a wrap-around for a normal
                   function call.  It preexamines the argument
                   expression list and expands any references to the
                   sequence operator.  It builds a copy of the
                   function call expression with these new arguments
                   inserted and evaluates the function call.
  INPUTS       : A data object buffer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Expressions alloctaed/deallocated
                 Function called and arguments evaluated
                 EvaluationError set on errors
  NOTES        : None
 *******************************************************************/
globle void ExpandFuncCall(
  DATA_OBJECT *result)
  {
   EXPRESSION *newargexp,*fcallexp;
   struct FunctionDefinition *func;

   /* ======================================================================
      Copy the original function call's argument expression list.
      Look for expand$ function callsexpressions and replace those
        with the equivalent expressions of the expansions of evaluations
        of the arguments.
      ====================================================================== */
   newargexp = CopyExpression(GetFirstArgument()->argList);
   ExpandFuncMultifield(result,newargexp,&newargexp,
                        (void *) FindFunction("expand$"));

   /* ===================================================================
      Build the new function call expression with the expanded arguments.
      Check the number of arguments, if necessary, and call the thing.
      =================================================================== */
   fcallexp = get_struct(expr);
   fcallexp->type = GetFirstArgument()->type;
   fcallexp->value = GetFirstArgument()->value;
   fcallexp->nextArg = NULL;
   fcallexp->argList = newargexp;
   if (fcallexp->type == FCALL)
     {
      func = (struct FunctionDefinition *) fcallexp->value;
      if (CheckFunctionArgCount(ValueToString(func->callFunctionName),
                                func->restrictions,CountArguments(newargexp)) == FALSE)
        {
         result->type = SYMBOL;
         result->value = FalseSymbol;
         ReturnExpression(fcallexp);
         return;
        }
     }
#if DEFFUNCTION_CONSTRUCT
   else if (fcallexp->type == PCALL)
     {
      if (CheckDeffunctionCall(fcallexp->value,
              CountArguments(fcallexp->argList)) == FALSE)
        {
         result->type = SYMBOL;
         result->value = FalseSymbol;
         ReturnExpression(fcallexp);
         SetEvaluationError(TRUE);
         return;
        }
     }
#endif

   EvaluateExpression(fcallexp,result);
   ReturnExpression(fcallexp);
  }

/***********************************************************************
  NAME         : DummyExpandFuncMultifield
  DESCRIPTION  : The expansion of multifield arguments is valid only
                 when done for a function call.  All these expansions
                 are handled by the H/L wrap-around function
                 (expansion-call) - see ExpandFuncCall.  If the H/L
                 function, epand-multifield is ever called directly,
                 it is an error.
  INPUTS       : Data object buffer
  RETURNS      : Nothing useful
  SIDE EFFECTS : EvaluationError set
  NOTES        : None
 **********************************************************************/
globle void DummyExpandFuncMultifield(
  DATA_OBJECT *result)
  {
   result->type = SYMBOL;
   result->value = FalseSymbol;
   SetEvaluationError(TRUE);
   PrintErrorID("MISCFUN",1,FALSE);
   PrintRouter(WERROR,"expand$ must be used in the argument list of a function call.\n");
  }

/***********************************************************************
  NAME         : ExpandFuncMultifield
  DESCRIPTION  : Recursively examines an expression and replaces
                   PROC_EXPAND_MULTIFIELD expressions with the expanded
                   evaluation expression of its argument
  INPUTS       : 1) A data object result buffer
                 2) The expression to modify
                 3) The address of the expression, in case it is
                    deleted entirely
                 4) The address of the H/L function expand$
  RETURNS      : Nothing useful
  SIDE EFFECTS : Expressions allocated/deallocated as necessary
                 Evaluations performed
                 On errors, argument expression set to call a function
                   which causes an evaluation error when evaluated
                   a second time by actual caller.
  NOTES        : THIS ROUTINE MODIFIES EXPRESSIONS AT RUNTIME!!  MAKE
                 SURE THAT THE EXPRESSION PASSED IS SAFE TO CHANGE!!
 **********************************************************************/
static void ExpandFuncMultifield(
  DATA_OBJECT *result,
  EXPRESSION *exp,
  EXPRESSION **sto,
  void *expmult)
  {
   EXPRESSION *newexp,*top,*bot;
   register long i; /* 6.04 Bug Fix */

   while (exp != NULL)
     {
      if (exp->value == expmult)
        {
         EvaluateExpression(exp->argList,result);
         ReturnExpression(exp->argList);
         if ((EvaluationError) || (result->type != MULTIFIELD))
           {
            exp->argList = NULL;
            if ((EvaluationError == FALSE) && (result->type != MULTIFIELD))
              ExpectedTypeError2("expand$",1);
            exp->value = (void *) FindFunction("(set-evaluation-error)");
            EvaluationError = FALSE;
            HaltExecution = FALSE;
            return;
           }
         top = bot = NULL;
         for (i = GetpDOBegin(result) ; i <= GetpDOEnd(result) ; i++)
           {
            newexp = get_struct(expr);
            newexp->type = GetMFType(result->value,i);
            newexp->value = GetMFValue(result->value,i);
            newexp->argList = NULL;
            newexp->nextArg = NULL;
            if (top == NULL)
              top = newexp;
            else
              bot->nextArg = newexp;
            bot = newexp;
           }
         if (top == NULL)
           {
            *sto = exp->nextArg;
            rtn_struct(expr,exp);
            exp = *sto;
           }
         else
           {
            bot->nextArg = exp->nextArg;
            *sto = top;
            rtn_struct(expr,exp);
            sto = &bot->nextArg;
            exp = bot->nextArg;
           }
        }
      else
        {
         if (exp->argList != NULL)
           ExpandFuncMultifield(result,exp->argList,&exp->argList,expmult);
         sto = &exp->nextArg;
         exp = exp->nextArg;
        }
     }
  }

/****************************************************************
  NAME         : CauseEvaluationError
  DESCRIPTION  : Dummy function use to cause evaluation errors on
                   a function call to generate error messages
  INPUTS       : None
  RETURNS      : A pointer to the FalseSymbol
  SIDE EFFECTS : EvaluationError set
  NOTES        : None
 ****************************************************************/
globle SYMBOL_HN *CauseEvaluationError()
  {
   SetEvaluationError(TRUE);
   return((SYMBOL_HN *) FalseSymbol);
  }

/****************************************************************
  NAME         : SetSORCommand
  DESCRIPTION  : Toggles SequenceOpMode - if TRUE, multifield
                   references are replaced with sequence
                   expansion operators
  INPUTS       : None
  RETURNS      : The old value of SequenceOpMode
  SIDE EFFECTS : SequenceOpMode toggled
  NOTES        : None
 ****************************************************************/
globle BOOLEAN SetSORCommand()
  {
#if (! RUN_TIME) && (! BLOAD_ONLY)
   DATA_OBJECT arg;

   if (ArgTypeCheck("set-sequence-operator-recognition",1,SYMBOL,&arg) == FALSE)
     return(SequenceOpMode);
   return(SetSequenceOperatorRecognition((arg.value == FalseSymbol) ?
                                         FALSE : TRUE));
#else
     return(SequenceOpMode);
#endif
  }

/********************************************************************
  NAME         : GetFunctionRestrictions
  DESCRIPTION  : Gets DefineFunction2() restriction list for function
  INPUTS       : None
  RETURNS      : A string containing the function restriction codes
  SIDE EFFECTS : EvaluationError set on errors
  NOTES        : None
 ********************************************************************/
globle SYMBOL_HN *GetFunctionRestrictions()
  {
   DATA_OBJECT temp;
   struct FunctionDefinition *fptr;

   if (ArgTypeCheck("get-function-restrictions",1,SYMBOL,&temp) == FALSE)
     return((SYMBOL_HN *) AddSymbol(""));
   fptr = FindFunction(DOToString(temp));
   if (fptr == NULL)
     {
      CantFindItemErrorMessage("function",DOToString(temp));
      SetEvaluationError(TRUE);
      return((SYMBOL_HN *) AddSymbol(""));
     }
   if (fptr->restrictions == NULL)
     return((SYMBOL_HN *) AddSymbol("0**"));
   return((SYMBOL_HN *) AddSymbol(fptr->restrictions));
  }

/*************************************************/
/* GetFunctionListFunction: H/L access routine   */
/*   for the get-function-list function.         */
/*************************************************/
globle void GetFunctionListFunction(
  DATA_OBJECT *returnValue)
  {
   struct FunctionDefinition *theFunction;
   struct multifield *theList;
   int functionCount = 0;

   if (ArgCountCheck("get-function-list",EXACTLY,0) == -1)
     {
      SetMultifieldErrorValue(returnValue);
      return;
     }

   for (theFunction = GetFunctionList();
        theFunction != NULL;
        theFunction = theFunction->next)
     { functionCount++; }

   SetpType(returnValue,MULTIFIELD);
   SetpDOBegin(returnValue,1);
   SetpDOEnd(returnValue,functionCount);
   theList = (struct multifield *) CreateMultifield((int) functionCount);
   SetpValue(returnValue,(void *) theList);

   for (theFunction = GetFunctionList(), functionCount = 1;
        theFunction != NULL;
        theFunction = theFunction->next, functionCount++)
     {
      SetMFType(theList,functionCount,SYMBOL);
      SetMFValue(theList,functionCount,theFunction->callFunctionName);
     }
  }

/******************************************/
/* Get the Thread ID currently being used */
/******************************************/
long GetThreadID()
{
	return(GetCurrentThreadId());
}

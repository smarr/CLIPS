   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                PRINT UTILITY MODULE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Utility routines for printing various items      */
/*   and messages.                                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description               */
/* ------------------+-------------+------------------------   */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS         */
/*************************************************************/

#define _PRNTUTIL_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include "setup.h"

#include "constant.h"
#include "symbol.h"
#include "utility.h"
#include "evaluatn.h"
#include "argacces.h"
#include "router.h"

#include "prntutil.h"

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle BOOLEAN              PreserveEscapedCharacters = FALSE;
   Thread globle BOOLEAN              AddressesToStrings = FALSE;
   Thread globle BOOLEAN              InstanceAddressesToNames = FALSE;

/***********************************************************/
/* PrintInChunks:  Prints a string in chunks to accomodate */
/*   systems which have a limit on the maximum size of a   */
/*   string which can be printed.                          */
/***********************************************************/
globle void PrintInChunks(
  char *logicalName,
  char *bigString)
  {
   char tc, *subString;

   subString = bigString;

   if (subString == NULL) return;

   while (((int) strlen(subString)) > 500)
     {
      if (HaltExecution) return;
      tc = subString[500];
      subString[500] = EOS;
      PrintRouter(logicalName,subString);
      subString[500] = tc;
      subString += 500;
     }

   PrintRouter(logicalName,subString);
  }

/************************************************************/
/* PrintFloat: Controls printout of floating point numbers. */
/************************************************************/
globle void PrintFloat(
  char *fileid,
  double number)
  {
   char *theString;

   theString = FloatToString(number);
   PrintRouter(fileid,theString);
  }

/****************************************************/
/* PrintLongInteger: Controls printout of integers. */
/****************************************************/
globle void PrintLongInteger(
  char *logicalName,
  long int number)
  {
   char printBuffer[32];

   sprintf(printBuffer,"%ld",number);
   PrintRouter(logicalName,printBuffer);
  }

/**************************************/
/* PrintAtom: Prints an atomic value. */
/**************************************/
globle void PrintAtom(
  char *logicalName,
  int type,
  void *value)
  {
   char buffer[20];

   switch (type)
     {
      case FLOAT:
        PrintFloat(logicalName,ValueToDouble(value));
        break;
      case INTEGER:
        PrintLongInteger(logicalName,ValueToLong(value));
        break;
      case SYMBOL:
        PrintRouter(logicalName,ValueToString(value));
        break;
      case STRING:
        if (PreserveEscapedCharacters)
          { PrintRouter(logicalName,StringPrintForm(ValueToString(value))); }
        else
          {
           PrintRouter(logicalName,"\"");
           PrintRouter(logicalName,ValueToString(value));
           PrintRouter(logicalName,"\"");
          }
        break;

      case EXTERNAL_ADDRESS:
        if (AddressesToStrings) PrintRouter(logicalName,"\"");
        PrintRouter(logicalName,"<Pointer-");
        sprintf(buffer,"%p",value);
        PrintRouter(logicalName,buffer);
        PrintRouter(logicalName,">");
        if (AddressesToStrings) PrintRouter(logicalName,"\"");
        break;

#if OBJECT_SYSTEM
      case INSTANCE_NAME:
        PrintRouter(logicalName,"[");
        PrintRouter(logicalName,ValueToString(value));
        PrintRouter(logicalName,"]");
        break;
#endif

      case RVOID:
        break;

      default:
        if (PrimitivesArray[type] == NULL) break;
        if (PrimitivesArray[type]->longPrintFunction == NULL)
          {
           PrintRouter(logicalName,"<unknown atom type>");
           break;
          }
        (*PrimitivesArray[type]->longPrintFunction)(logicalName,value);
        break;
     }
  }

/**********************************************************/
/* PrintTally: Prints a tally count indicating the number */
/*   of items that have been displayed. Used by functions */
/*   such as list-defrules.                               */
/**********************************************************/
globle void PrintTally(
  char *logicalName,
  long count,
  char *singular,
  char *plural)
  {
   if (count == 0) return;

   PrintRouter(logicalName,"For a total of ");
   PrintLongInteger(logicalName,count);
   PrintRouter(logicalName," ");

   if (count == 1) PrintRouter(logicalName,singular);
   else PrintRouter(logicalName,plural);

   PrintRouter(logicalName,".\n");
  }

/********************************************/
/* PrintErrorID: Prints the module name and */
/*   error ID for an error message.         */
/********************************************/
globle void PrintErrorID(
  char *module,
  int errorID,
  int printCR)
  {
   if (printCR) PrintRouter(WERROR,"\n");
   PrintRouter(WERROR,"[");
   PrintRouter(WERROR,module);
   PrintLongInteger(WERROR,(long int) errorID);
   PrintRouter(WERROR,"] ");
  }

/**********************************************/
/* PrintWarningID: Prints the module name and */
/*   warning ID for a warning message.        */
/**********************************************/
globle void PrintWarningID(
  char *module,
  int warningID,
  int printCR)
  {
   if (printCR) PrintRouter(WWARNING,"\n");
   PrintRouter(WWARNING,"[");
   PrintRouter(WWARNING,module);
   PrintLongInteger(WWARNING,(long int) warningID);
   PrintRouter(WWARNING,"] WARNING: ");
  }

/***************************************************/
/* CantFindItemErrorMessage: Generic error message */
/*  when an "item" can not be found.               */
/***************************************************/
globle void CantFindItemErrorMessage(
  char *itemType,
  char *itemName)
  {
   PrintErrorID("PRNTUTIL",1,FALSE);
   PrintRouter(WERROR,"Unable to find ");
   PrintRouter(WERROR,itemType);
   PrintRouter(WERROR," ");
   PrintRouter(WERROR,itemName);
   PrintRouter(WERROR,".\n");
  }

/*****************************************************/
/* CantDeleteItemErrorMessage: Generic error message */
/*  when an "item" can not be deleted.               */
/*****************************************************/
globle void CantDeleteItemErrorMessage(
  char *itemType,
  char *itemName)
  {
   PrintErrorID("PRNTUTIL",4,FALSE);
   PrintRouter(WERROR,"Unable to delete ");
   PrintRouter(WERROR,itemType);
   PrintRouter(WERROR," ");
   PrintRouter(WERROR,itemName);
   PrintRouter(WERROR,".\n");
  }

/****************************************************/
/* AlreadyParsedErrorMessage: Generic error message */
/*  when an "item" has already been parsed.         */
/****************************************************/
globle void AlreadyParsedErrorMessage(
  char *itemType,
  char *itemName)
  {
   PrintErrorID("PRNTUTIL",5,TRUE);
   PrintRouter(WERROR,"The ");
   if (itemType != NULL) PrintRouter(WERROR,itemType);
   if (itemName != NULL) PrintRouter(WERROR,itemName);
   PrintRouter(WERROR," has already been parsed.\n");
  }

/*********************************************************/
/* SyntaxErrorMessage: Generalized syntax error message. */
/*********************************************************/
globle void SyntaxErrorMessage(
  char *location)
  {
   PrintErrorID("PRNTUTIL",2,TRUE);
   PrintRouter(WERROR,"Syntax Error");
   if (location != NULL)
     {
      PrintRouter(WERROR,":  Check appropriate syntax for ");
      PrintRouter(WERROR,location);
     }

   PrintRouter(WERROR,".\n");
   SetEvaluationError(TRUE);
  }

/****************************************************/
/* LocalVariableErrorMessage: Generic error message */
/*  when a local variable is accessed by an "item"  */
/*  which can not access local variables.           */
/****************************************************/
globle void LocalVariableErrorMessage(
  char *byWhat)
  {
   PrintErrorID("PRNTUTIL",6,TRUE);
   PrintRouter(WERROR,"Local variables can not be accessed by ");
   PrintRouter(WERROR,byWhat);
   PrintRouter(WERROR,".\n");
  }

/******************************************/
/* SystemError: Generalized error message */
/*   for major internal errors.           */
/******************************************/
globle void SystemError(
  char *module,
  int errorID)
  {
   PrintErrorID("PRNTUTIL",3,TRUE);

   PrintRouter(WERROR,"\n*** ");
   PrintRouter(WERROR,APPLICATION_NAME);
   PrintRouter(WERROR," SYSTEM ERROR ***\n");

   PrintRouter(WERROR,"ID = ");
   PrintRouter(WERROR,module);
   PrintLongInteger(WERROR,(long int) errorID);
   PrintRouter(WERROR,"\n");

   PrintRouter(WERROR,APPLICATION_NAME);
   PrintRouter(WERROR," data structures are in an inconsistent or corrupted state.\n");
   PrintRouter(WERROR,"This error may have occurred from errors in user defined code.\n");
   PrintRouter(WERROR,"**************************\n");
  }

/*******************************************************/
/* DivideByZeroErrorMessage: Generalized error message */
/*   for when a function attempts to divide by zero.   */
/*******************************************************/
globle void DivideByZeroErrorMessage(
  char *functionName)
  {
   PrintErrorID("PRNTUTIL",7,FALSE);
   PrintRouter(WERROR,"Attempt to divide by zero in ");
   PrintRouter(WERROR,functionName);
   PrintRouter(WERROR," function.\n");
  }

/*******************************************************/
/* FloatToString: Converts number to KB string format. */
/*******************************************************/
globle char *FloatToString(
  double number)
  {
   char floatString[40];
   int i;
   char x;
   void *thePtr;

   sprintf(floatString,"%.16g",number);

   for (i = 0; (x = floatString[i]) != '\0'; i++)
     {
      if ((x == '.') || (x == 'e'))
        {
         thePtr = AddSymbol(floatString);
         return(ValueToString(thePtr));
        }
     }

   strcat(floatString,".0");

   thePtr = AddSymbol(floatString);
   return(ValueToString(thePtr));
  }

/*******************************************************************/
/* LongIntegerToString: Converts long integer to KB string format. */
/*******************************************************************/
globle char *LongIntegerToString(
  long number)
  {
   char buffer[30];
   void *thePtr;

   sprintf(buffer,"%ld",number);

   thePtr = AddSymbol(buffer);
   return(ValueToString(thePtr));
  }

/************************************************************/
/* SalienceInformationError: Error message for errors which */
/*   occur during the evaluation of a salience value.       */
/************************************************************/
globle void SalienceInformationError(
  char *constructType,
  char *constructName)
  {
   PrintErrorID("PRNTUTIL",8,TRUE);
   PrintRouter(WERROR,"This error occurred while evaluating the salience");
   if (constructName != NULL)
     {
      PrintRouter(WERROR," for ");
      PrintRouter(WERROR,constructType);
      PrintRouter(WERROR," ");
      PrintRouter(WERROR,constructName);
     }
   PrintRouter(WERROR,".\n");
  }

/**********************************************************/
/* SalienceRangeError: Error message that is printed when */
/*   a salience value does not fall between the minimum   */
/*   and maximum salience values.                         */
/**********************************************************/
globle void SalienceRangeError(
  int min,
  int max)
  {
   PrintErrorID("PRNTUTIL",9,TRUE);
   PrintRouter(WERROR,"Salience value out of range ");
   PrintLongInteger(WERROR,(long int) min);
   PrintRouter(WERROR," to ");
   PrintLongInteger(WERROR,(long int) max);
   PrintRouter(WERROR,".\n");
  }

/***************************************************************/
/* SalienceNonIntegerError: Error message that is printed when */
/*   a rule's salience does not evaluate to an integer.        */
/***************************************************************/
globle void SalienceNonIntegerError()
  {
   PrintErrorID("PRNTUTIL",10,TRUE);
   PrintRouter(WERROR,"Salience value must be an integer value.\n");
  }


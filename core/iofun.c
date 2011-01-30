   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                 I/O FUNCTIONS MODULE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for several I/O functions      */
/*   including printout, read, open, close, remove, rename,  */
/*   format, and readline.                                   */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Donnell                                     */
/*      Gary D. Riley                                        */
/*      Bebe Ly                                              */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _IOFUN_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include "setup.h"

#include "router.h"
#include "strngrtr.h"
#include "filertr.h"
#include "argacces.h"
#include "extnfunc.h"
#include "scanner.h"
#include "constant.h"
#include "memalloc.h"
#include "commline.h"
#include "sysdep.h"
#include "utility.h"

#include "iofun.h"

/***************/
/* DEFINITIONS */
/***************/

#define FORMAT_MAX 512
#define FLAG_MAX    80

/****************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS  */
/****************************************/

#if BASIC_IO
   static void             ReadTokenFromStdin(struct token *);
#endif
#if BASIC_IO || EXT_IO
   static void             IllegalLogicalNameMessage(char *);
#endif
#if EXT_IO
   static char            *ControlStringCheck(int);
   static char             FindFormatFlag(char *,int *,char *,int *);
   static char            *PrintFormatFlag (char *,int,int,int);
   static char            *FillBuffer(char *,int *,int *);
#endif

#if ! RUN_TIME
/**************************************/
/* IOFunctionDefinitions: Initializes */
/*   the I/O functions.               */
/**************************************/
globle void IOFunctionDefinitions()
  {
#if BASIC_IO
   DefineFunction2("printout", 'v', PTIF PrintoutFunction, "PrintoutFunction", "1*");
   DefineFunction2("read",     'u', PTIF ReadFunction,  "ReadFunction", "*1");
   DefineFunction2("open",     'b', OpenFunction,  "OpenFunction", "23*k");
   DefineFunction2("close",    'b', CloseFunction, "CloseFunction", "*1");
#endif

#if EXT_IO
   DefineFunction2("remove",   'b', RemoveFunction,  "RemoveFunction", "11k");
   DefineFunction2("rename",   'b', RenameFunction, "RenameFunction", "22k");
   DefineFunction2("format",   's', PTIF FormatFunction, "FormatFunction", "2**us");
   DefineFunction2("readline", 'k', PTIF ReadlineFunction, "ReadlineFunction", "*1");
#endif
  }
#endif

#if BASIC_IO

/******************************************/
/* PrintoutFunction: H/L access routine   */
/*   for the printout function.           */
/******************************************/
globle void PrintoutFunction()
  {
   char *dummyid;
   int i, argCount;
   DATA_OBJECT theArgument;

   /*=======================================================*/
   /* The printout function requires at least one argument. */
   /*=======================================================*/

   if ((argCount = ArgCountCheck("printout",AT_LEAST,1)) == -1) return;

   /*=====================================================*/
   /* Get the logical name to which output is to be sent. */
   /*=====================================================*/

   dummyid = GetLogicalName(1,"stdout");
   if (dummyid == NULL)
     {
      IllegalLogicalNameMessage("printout");
      SetHaltExecution(TRUE);
      SetEvaluationError(TRUE);
      return;
     }

   /*============================================================*/
   /* Determine if any router recognizes the output destination. */
   /*============================================================*/

   if (QueryRouters(dummyid) == FALSE)
     {
      UnrecognizedRouterMessage(dummyid);
      return;
     }

   /*===============================================*/
   /* Print each of the arguments sent to printout. */
   /*===============================================*/

   for (i = 2; i <= argCount; i++)
     {
      RtnUnknown(i,&theArgument);
      if (HaltExecution) break;

      switch(GetType(theArgument))
        {
         case SYMBOL:
           if (strcmp(DOToString(theArgument),"crlf") == 0)
             { PrintRouter(dummyid,"\n"); }
           else if (strcmp(DOToString(theArgument),"tab") == 0)
             { PrintRouter(dummyid,"\t"); }
           else if (strcmp(DOToString(theArgument),"vtab") == 0)
             { PrintRouter(dummyid,"\v"); }
           else if (strcmp(DOToString(theArgument),"ff") == 0)
             { PrintRouter(dummyid,"\f"); }
           else if (strcmp(DOToString(theArgument),"t") == 0)
             { PrintRouter(dummyid,"\n"); }
           else
             { PrintRouter(dummyid,DOToString(theArgument)); }
           break;

         case STRING:
           PrintRouter(dummyid,DOToString(theArgument));
           break;

         default:
           PrintDataObject(dummyid,&theArgument);
           break;
        }
     }
  }

/*************************************************************/
/* ReadFunction: H/L access routine for the read function.   */
/*************************************************************/
globle void ReadFunction(
  DATA_OBJECT_PTR returnValue)
  {
   struct token theToken;
   int numberOfArguments;
   char *logicalName = NULL;

   /*===============================================*/
   /* Check for an appropriate number of arguments. */
   /*===============================================*/

   if ((numberOfArguments = ArgCountCheck("read",NO_MORE_THAN,1)) == -1)
     {
      returnValue->type = STRING;
      returnValue->value = (void *) AddSymbol("*** READ ERROR ***");
      return;
     }

   /*======================================================*/
   /* Determine the logical name from which input is read. */
   /*======================================================*/

   if (numberOfArguments == 0)
     { logicalName = "stdin"; }
   else if (numberOfArguments == 1)
     {
      logicalName = GetLogicalName(1,"stdin");
      if (logicalName == NULL)
        {
         IllegalLogicalNameMessage("read");
         SetHaltExecution(TRUE);
         SetEvaluationError(TRUE);
         returnValue->type = STRING;
         returnValue->value = (void *) AddSymbol("*** READ ERROR ***");
         return;
        }
     }

   /*============================================*/
   /* Check to see that the logical name exists. */
   /*============================================*/

   if (QueryRouters(logicalName) == FALSE)
     {
      UnrecognizedRouterMessage(logicalName);
      SetHaltExecution(TRUE);
      SetEvaluationError(TRUE);
      returnValue->type = STRING;
      returnValue->value = (void *) AddSymbol("*** READ ERROR ***");
      return;
     }

   /*=======================================*/
   /* Collect input into string if the read */
   /* source is stdin, else just get token. */
   /*=======================================*/

   if (strcmp(logicalName,"stdin") == 0)
     { ReadTokenFromStdin(&theToken); }
   else
     { GetToken(logicalName,&theToken); }

   CommandBufferInputCount = -1;

   /*====================================================*/
   /* Copy the token to the return value data structure. */
   /*====================================================*/

   returnValue->type = theToken.type;
   if ((theToken.type == FLOAT) || (theToken.type == STRING) ||
#if OBJECT_SYSTEM
       (theToken.type == INSTANCE_NAME) ||
#endif
       (theToken.type == SYMBOL) || (theToken.type == INTEGER))
     { returnValue->value = theToken.value; }
   else if (theToken.type == STOP)
     {
      returnValue->type = SYMBOL;
      returnValue->value = (void *) AddSymbol("EOF");
     }
   else if (theToken.type == UNKNOWN_VALUE)
     {
      returnValue->type = STRING;
      returnValue->value = (void *) AddSymbol("*** READ ERROR ***");
     }
   else
     {
      returnValue->type = STRING;
      returnValue->value = (void *) AddSymbol(theToken.printForm);
     }

   return;
  }

/********************************************************/
/* ReadTokenFromStdin: Special routine used by the read */
/*   function to read a token from standard input.      */
/********************************************************/
static void ReadTokenFromStdin(
  struct token *theToken)
  {
   char *inputString;
   int inputStringSize;
   int inchar;

   /*=============================================*/
   /* Continue processing until a token is found. */
   /*=============================================*/

   theToken->type = STOP;
   while (theToken->type == STOP)
     {
      /*===========================================*/
      /* Initialize the variables used for storing */
      /* the characters retrieved from stdin.      */
      /*===========================================*/

      inputString = NULL;
      inputStringSize = CommandBufferInputCount = 0;
      inchar = GetcRouter("stdin");

      /*========================================================*/
      /* Continue reading characters until a carriage return is */
      /* entered or the user halts execution (usually with      */
      /* control-c). Waiting for the carriage return prevents   */
      /* the input from being prematurely parsed (such as when  */
      /* a space is entered after a symbol has been typed).     */
      /*========================================================*/

      while ((inchar != '\n') && (inchar != '\r') && (inchar != EOF) &&
             (! GetHaltExecution()))
        {
         inputString = ExpandStringWithChar(inchar,inputString,&CommandBufferInputCount,
                                            &inputStringSize,inputStringSize + 80);
         inchar = GetcRouter("stdin");
        }

      /*==================================================*/
      /* Open a string input source using the characters  */
      /* retrieved from stdin and extract the first token */
      /* contained in the string.                         */
      /*==================================================*/

      OpenStringSource("read",inputString,0);
      GetToken("read",theToken);
      CloseStringSource("read");
      if (inputStringSize > 0) rm(inputString,inputStringSize);

      /*===========================================*/
      /* Pressing control-c (or comparable action) */
      /* aborts the read function.                 */
      /*===========================================*/

      if (GetHaltExecution())
        {
         theToken->type = STRING;
         theToken->value = (void *) AddSymbol("*** READ ERROR ***");
        }

      /*====================================================*/
      /* Return the EOF symbol if the end of file for stdin */
      /* has been encountered. This typically won't occur,  */
      /* but is possible (for example by pressing control-d */
      /* in the UNIX operating system).                     */
      /*====================================================*/

      if ((theToken->type == STOP) && (inchar == EOF))
        {
         theToken->type = SYMBOL;
         theToken->value = (void *) AddSymbol("EOF");
        }
     }
  }

/*************************************************************/
/* OpenFunction: H/L access routine for the open function.   */
/*************************************************************/
globle int OpenFunction()
  {
   int numberOfArguments;
   char *fileName, *logicalName, *accessMode = NULL;
   DATA_OBJECT theArgument;

   /*========================================*/
   /* Check for a valid number of arguments. */
   /*========================================*/

   if ((numberOfArguments = ArgRangeCheck("open",2,3)) == -1) return(0);

   /*====================*/
   /* Get the file name. */
   /*====================*/

   if ((fileName = GetFileName("open",1)) == NULL) return(0);

   /*=======================================*/
   /* Get the logical name to be associated */
   /* with the opened file.                 */
   /*=======================================*/

   logicalName = GetLogicalName(2,NULL);
   if (logicalName == NULL)
     {
      SetHaltExecution(TRUE);
      SetEvaluationError(TRUE);
      IllegalLogicalNameMessage("open");
      return(0);
     }

   /*==================================*/
   /* Check to see if the logical name */
   /* is already in use.               */
   /*==================================*/

   if (FindFile(logicalName))
     {
      SetHaltExecution(TRUE);
      SetEvaluationError(TRUE);
      PrintErrorID("IOFUN",2,FALSE);
      PrintRouter(WERROR,"Logical name ");
      PrintRouter(WERROR,logicalName);
      PrintRouter(WERROR," already in use.\n");
      return(0);
     }

   /*===========================*/
   /* Get the file access mode. */
   /*===========================*/

   if (numberOfArguments == 2)
     { accessMode = "r"; }
   else if (numberOfArguments == 3)
     {
      if (ArgTypeCheck("open",3,STRING,&theArgument) == FALSE) return(0);
      accessMode = DOToString(theArgument);
     }

   /*=====================================*/
   /* Check for a valid file access mode. */
   /*=====================================*/

   if ((strcmp(accessMode,"r") != 0) &&
       (strcmp(accessMode,"r+") != 0) &&
       (strcmp(accessMode,"w") != 0) &&
       (strcmp(accessMode,"a") != 0) &&
       (strcmp(accessMode,"wb") != 0))
     {
      SetHaltExecution(TRUE);
      SetEvaluationError(TRUE);
      ExpectedTypeError1("open",3,"string with value \"r\", \"r+\", \"w\", \"wb\", or \"a\"");
      return(0);
     }

   /*================================================*/
   /* Open the named file and associate it with the  */
   /* specified logical name. Return TRUE if the     */
   /* file was opened successfully, otherwise FALSE. */
   /*================================================*/

   return(OpenAFile(fileName,accessMode,logicalName));
  }

/***************************************************************/
/* CloseFunction: H/L access routine for the close function.   */
/***************************************************************/
globle int CloseFunction()
  {
   int numberOfArguments;
   char *logicalName;

   /*======================================*/
   /* Check for valid number of arguments. */
   /*======================================*/

   if ((numberOfArguments = ArgCountCheck("close",NO_MORE_THAN,1)) == -1) return(0);

   /*=====================================================*/
   /* If no arguments are specified, then close all files */
   /* opened with the open command. Return TRUE if all    */
   /* files were closed successfully, otherwise FALSE.    */
   /*=====================================================*/

   if (numberOfArguments == 0) return(CloseAllFiles());

   /*================================*/
   /* Get the logical name argument. */
   /*================================*/

   logicalName = GetLogicalName(1,NULL);
   if (logicalName == NULL)
     {
      IllegalLogicalNameMessage("close");
      SetHaltExecution(TRUE);
      SetEvaluationError(TRUE);
      return(0);
     }

   /*========================================================*/
   /* Close the file associated with the specified logical   */
   /* name. Return TRUE if the file was closed successfully, */
   /* otherwise false.                                       */
   /*========================================================*/

   return(CloseFile(logicalName));
  }


#endif

#if EXT_IO

/****************************************/
/* RemoveFunction: H/L access routine   */
/*   for the remove function.           */
/****************************************/
globle int RemoveFunction()
  {
   char *theFileName;

   /*======================================*/
   /* Check for valid number of arguments. */
   /*======================================*/

   if (ArgCountCheck("remove",EXACTLY,1) == -1) return(FALSE);

   /*====================*/
   /* Get the file name. */
   /*====================*/

   if ((theFileName = GetFileName("remove",1)) == NULL) return(FALSE);

   /*==============================================*/
   /* Remove the file. Return TRUE if the file was */
   /* sucessfully removed, otherwise FALSE.        */
   /*==============================================*/

   return(genremove(theFileName));
  }

/****************************************/
/* RenameFunction: H/L access routine   */
/*   for the rename function.           */
/****************************************/
globle int RenameFunction()
  {
   char *oldFileName, *newFileName;

   /*========================================*/
   /* Check for a valid number of arguments. */
   /*========================================*/

   if (ArgCountCheck("rename",EXACTLY,2) == -1) return(FALSE);

   /*===========================*/
   /* Check for the file names. */
   /*===========================*/

   if ((oldFileName = GetFileName("rename",1)) == NULL) return(FALSE);
   if ((newFileName = GetFileName("rename",2)) == NULL) return(FALSE);

   /*==============================================*/
   /* Rename the file. Return TRUE if the file was */
   /* sucessfully renamed, otherwise FALSE.        */
   /*==============================================*/

   return(genrename(oldFileName,newFileName));
  }

/****************************************/
/* FormatFunction: H/L access routine   */
/*   for the format function.           */
/****************************************/
globle void *FormatFunction()
  {
   int argCount, start_pos;
   char *formatString, *logicalName;
   char formatFlagType;
   int  f_cur_arg = 3;
   int form_pos = 0;
   char buffer[FORMAT_MAX];
   char percentBuffer[FLAG_MAX];
   char *fstr = NULL;
   int fmax = 0, fpos = 0;
   void *hptr;
   int longFound;
   char *theString;

   /*======================================*/
   /* Set default return value for errors. */
   /*======================================*/

   hptr = AddSymbol("");

   /*=========================================*/
   /* Format requires at least two arguments: */
   /* a logical name and a format string.     */
   /*=========================================*/

   if ((argCount = ArgCountCheck("format",AT_LEAST,2)) == -1)
     { return(hptr); }

   /*========================================*/
   /* First argument must be a logical name. */
   /*========================================*/

   if ((logicalName = GetLogicalName(1,"stdout")) == NULL)
     {
      IllegalLogicalNameMessage("format");
      SetHaltExecution(TRUE);
      SetEvaluationError(TRUE);
      return(hptr);
     }

   if (strcmp(logicalName,"nil") == 0)
     { /* do nothing */ }
   else if (QueryRouters(logicalName) == FALSE)
     {
      UnrecognizedRouterMessage(logicalName);
      return(hptr);
     }

   /*=====================================================*/
   /* Second argument must be a string.  The appropriate  */
   /* number of arguments specified by the string must be */
   /* present in the argument list.                       */
   /*=====================================================*/

   if ((formatString = ControlStringCheck (argCount)) == NULL)
     { return (hptr); }

   /*==============================================*/
   /* Locate a string of 80 character for scanning */
   /* sub_string from control_string               */
   /*==============================================*/

   /* Scanning and print the format */

   while (formatString[form_pos] != '\0')
     {
      if (formatString[form_pos] != '%')
        {
         start_pos = form_pos;
         while ((formatString[form_pos] != '%') &&
                (formatString[form_pos] != '\0') &&
                ((form_pos - start_pos) < FLAG_MAX))
           { form_pos++; }
         fstr = AppendNToString(&formatString[start_pos],fstr,form_pos-start_pos,&fpos,&fmax);
        }
      else
        {
         start_pos = form_pos;
         form_pos++;
         formatFlagType = FindFormatFlag(formatString,&form_pos,buffer,&longFound);
         if (formatFlagType != ' ')
           {
            strncpy(percentBuffer,&formatString[start_pos],
                    (CLIPS_STD_SIZE) form_pos-start_pos);
            percentBuffer[form_pos-start_pos] = EOS;
            if ((! longFound) &&
                ((formatFlagType == 'd') || (formatFlagType == 'o') ||
                 (formatFlagType == 'u') || (formatFlagType == 'x')))
              {
               longFound = TRUE;
               percentBuffer[(form_pos-start_pos) - 1] = 'l';
               percentBuffer[form_pos-start_pos] = formatFlagType;
               percentBuffer[(form_pos-start_pos) + 1] = EOS;
              }

            if ((theString = PrintFormatFlag(percentBuffer,f_cur_arg,formatFlagType,longFound)) == NULL)
              {
               if (fstr != NULL) rm(fstr,fmax);
               return (hptr);
              }
            fstr = AppendToString(theString,fstr,&fpos,&fmax);
            if (fstr == NULL) return(hptr);
            f_cur_arg++;
           }
         else
           {
            fstr = AppendToString(buffer,fstr,&fpos,&fmax);
            if (fstr == NULL) return(hptr);
           }
        }
     }

   if (fstr != NULL)
     {
      hptr = AddSymbol(fstr);
      if (strcmp(logicalName,"nil") != 0) PrintRouter(logicalName,fstr);
      rm(fstr,fmax);
     }
   else
     { hptr = AddSymbol(""); }

   return(hptr);
  }

/*********************************************************************/
/* ControlStringCheck:  Checks the 2nd parameter which is the format */
/*   control string to see if there are enough matching arguments.   */
/*********************************************************************/
static char *ControlStringCheck(
  int argCount)
  {
   DATA_OBJECT t_ptr;
   char *str_array;
   char print_buff[10];
   int longFound;

   int i,per_count;

   if (ArgTypeCheck("format",2,STRING,&t_ptr) == FALSE) return(NULL);

   per_count = 0;
   str_array = ValueToString(t_ptr.value);
   for (i= 0 ; str_array[i] != '\0' ; )
     {
      if (str_array[i] == '%')
        {
         i++;
         if (FindFormatFlag(str_array,&i,print_buff,&longFound) != ' ')
           { per_count++; }
        }
      else
        { i++; }
     }

   if (per_count != (argCount - 2))
     {
      ExpectedCountError("format",EXACTLY,per_count+2);
      SetEvaluationError(TRUE);
      return (NULL);
     }

   return(str_array);
  }

/***********************************************/
/* FindFormatFlag:  This function searches for */
/*   a format flag in the format string.       */
/***********************************************/
static char FindFormatFlag(
  char *formatString,
  int *a,
  char *formatBuffer,
  int *longFound)
  {
   char inchar, formatFlagType;
   int start_pos, copy_pos = 0;

   /*===========================================================*/
   /* Set return values to the default value. A blank character */
   /* indicates that no format flag was found which requires a  */
   /* parameter. The longFound flag indicates whether the       */
   /* character 'l' was used with the float or integer flag to  */
   /* indicate a double precision float or a long integer.      */
   /*===========================================================*/

   formatFlagType = ' ';
   *longFound = FALSE;

   /*=====================================================*/
   /* The format flags for carriage returns, line feeds,  */
   /* horizontal and vertical tabs, and the percent sign, */
   /* do not require a parameter.                         */
   /*=====================================================*/

   if (formatString[*a] == 'n')
     {
      sprintf(formatBuffer,"\n");
      (*a)++;
      return(formatFlagType);
     }
   else if (formatString[*a] == 'r')
     {
      sprintf(formatBuffer,"\r");
      (*a)++;
      return(formatFlagType);
     }
   else if (formatString[*a] == 't')
     {
      sprintf(formatBuffer,"\t");
      (*a)++;
      return(formatFlagType);
     }
   else if (formatString[*a] == 'v')
     {
      sprintf(formatBuffer,"\v");
      (*a)++;
      return(formatFlagType);
     }
   else if (formatString[*a] == '%')
     {
      sprintf(formatBuffer,"%%");
      (*a)++;
      return(formatFlagType);
     }

   /*======================================================*/
   /* Identify the format flag which requires a parameter. */
   /*======================================================*/

   start_pos = *a;
   formatBuffer[copy_pos] = '\0';
   while ((formatString[*a] != '%') &&
          (formatString[*a] != '\0') &&
          ((*a - start_pos) < FLAG_MAX))
     {
      inchar = formatString[*a];
      formatBuffer[copy_pos++] = inchar;
      formatBuffer[copy_pos] = '\0';
      if ( (inchar == 'd') ||
           (inchar == 'o') ||
           (inchar == 'x') ||
           (inchar == 'u') ||
           (inchar == 'c') ||
           (inchar == 's') ||
           (inchar == 'e') ||
           (inchar == 'f') ||
           (inchar == 'g') )
        {
         formatFlagType = inchar;
         if (formatString[(*a) - 1] == 'l')
           { *longFound = TRUE; }
         (*a)++;
         return(formatFlagType);
        }
      (*a)++;
     }

   return(formatFlagType);
  }

/**********************************************************************/
/* PrintFormatFlag:  Prints out part of the total format string along */
/*   with the argument for that part of the format string.            */
/**********************************************************************/
static char *PrintFormatFlag(
  char *formatString,
  int whichArg,
  int formatType,
  int longFound)
  {
   DATA_OBJECT theResult;
   char *theString, *printBuffer;
   int theLength;

   /*=================*/
   /* String argument */
   /*=================*/

   switch (formatType)
     {
      case 's':
        if (ArgTypeCheck("format",whichArg,SYMBOL_OR_STRING,&theResult) == FALSE) return(NULL);
        theLength = strlen(formatString) + strlen(ValueToString(theResult.value)) + 200;
        printBuffer = (char *) gm2 (((int) sizeof(char) * theLength));
        sprintf(printBuffer,formatString,ValueToString(theResult.value));
        break;

      case 'c':
        RtnUnknown(whichArg,&theResult);
        if ((GetType(theResult) == STRING) ||
            (GetType(theResult) == SYMBOL))
          {
           theLength = strlen(formatString) + 200;
           printBuffer = (char *) gm2 (((int) sizeof(char) * theLength));
           sprintf(printBuffer,formatString,(ValueToString(theResult.value))[0]);
          }
        else if (GetType(theResult) == INTEGER)
          {
           theLength = strlen(formatString) + 200;
           printBuffer = (char *) gm2 (((int) sizeof(char) * theLength));
           sprintf(printBuffer,formatString,(char) DOToLong(theResult));
          }
        else
          {
           ExpectedTypeError1("format",whichArg,"symbol, string, or integer");
           return(NULL);
          }
        break;

      case 'd':
      case 'x':
      case 'o':
      case 'u':
        if (ArgTypeCheck("format",whichArg,INTEGER_OR_FLOAT,&theResult) == FALSE) return(NULL);
        theLength = strlen(formatString) + 200;
        printBuffer = (char *) gm2 (((int) sizeof(char) * theLength));
        if (GetType(theResult) == FLOAT)
          {
           if (longFound)
             { sprintf(printBuffer,formatString,(long) ValueToDouble(theResult.value)); }
           else
             { sprintf(printBuffer,formatString,(int) ValueToDouble(theResult.value)); }
          }
        else
          {
           if (longFound)
             { sprintf(printBuffer,formatString,(long) ValueToLong(theResult.value)); }
           else
             { sprintf(printBuffer,formatString,(int) ValueToLong(theResult.value)); }
          }
        break;

      case 'f':
      case 'g':
      case 'e':
        if (ArgTypeCheck("format",whichArg,INTEGER_OR_FLOAT,&theResult) == FALSE) return(NULL);
        theLength = strlen(formatString) + 200;
        printBuffer = (char *) gm2 (((int) sizeof(char) * theLength));

        if (GetType(theResult) == FLOAT)
          { sprintf(printBuffer,formatString,ValueToDouble(theResult.value)); }
        else
          { sprintf(printBuffer,formatString,(double) ValueToLong(theResult.value)); }
        break;

      default:
         PrintRouter (WERROR," Error in format, the conversion character");
         PrintRouter (WERROR," for formatted output is not valid\n");
         return(FALSE);
     }

   theString = ValueToString(AddSymbol(printBuffer));
   rm(printBuffer,(int) sizeof(char) * theLength);
   return(theString);
  }

/******************************************/
/* ReadlineFunction: H/L access routine   */
/*   for the readline function.           */
/******************************************/
globle void ReadlineFunction(
  DATA_OBJECT_PTR returnValue)
  {
   char *buffer;
   int line_max = 0;
   int numberOfArguments;

   char *logicalName;

   returnValue->type = STRING;

   if ((numberOfArguments = ArgCountCheck("readline",NO_MORE_THAN,1)) == -1)
     {
      returnValue->value = (void *) AddSymbol("*** READ ERROR ***");
      return;
     }

   if (numberOfArguments == 0 )
     { logicalName = "stdin"; }
   else
     {
      logicalName = GetLogicalName(1,"stdin");
      if (logicalName == NULL)
        {
         IllegalLogicalNameMessage("readline");
         SetHaltExecution(TRUE);
         SetEvaluationError(TRUE);
         returnValue->value = (void *) AddSymbol("*** READ ERROR ***");
         return;
        }
     }

   if (QueryRouters(logicalName) == FALSE)
     {
      UnrecognizedRouterMessage(logicalName);
      SetHaltExecution(TRUE);
      SetEvaluationError(TRUE);
      returnValue->value = (void *) AddSymbol("*** READ ERROR ***");
      return;
     }

   CommandBufferInputCount = 0;
   buffer = FillBuffer(logicalName,&CommandBufferInputCount,&line_max);
   CommandBufferInputCount = -1;

   if (GetHaltExecution())
     {
      returnValue->value = (void *) AddSymbol("*** READ ERROR ***");
      if (buffer != NULL) rm(buffer,(int) sizeof (char) * line_max);
      return;
     }

   if (buffer == NULL)
     {
      returnValue->value = (void *) AddSymbol("EOF");
      returnValue->type = SYMBOL;
      return;
     }

   returnValue->value = (void *) AddSymbol(buffer);
   rm(buffer,(int) sizeof (char) * line_max);
   return;
  }

/*************************************************************/
/* FillBuffer: Read characters from a specified logical name */
/*   and places them into a buffer until a carriage return   */
/*   or end-of-file character is read.                       */
/*************************************************************/
static char *FillBuffer(
  char *logicalName,
  int *currentPosition,
  int *maximumSize)
  {
   int c;
   char *buf = NULL;

   /*================================*/
   /* Read until end of line or eof. */
   /*================================*/

   c = GetcRouter(logicalName);

   if (c == EOF)
     { return(NULL); }

   /*==================================*/
   /* Grab characters until cr or eof. */
   /*==================================*/

   while ((c != '\n') && (c != '\r') && (c != EOF) &&
          (! GetHaltExecution()))
     {
      buf = ExpandStringWithChar(c,buf,currentPosition,maximumSize,*maximumSize+80);
      c = GetcRouter(logicalName);
     }

   /*==================*/
   /* Add closing EOS. */
   /*==================*/

   buf = ExpandStringWithChar(EOS,buf,currentPosition,maximumSize,*maximumSize+80);
   return (buf);
  }
#endif

#if BASIC_IO || EXT_IO

/****************************************************/
/* IllegalLogicalNameMessage: Generic error message */
/*   for illegal logical names.                     */
/****************************************************/
static void IllegalLogicalNameMessage(
  char *theFunction)
  {
   PrintErrorID("IOFUN",1,FALSE);
   PrintRouter(WERROR,"Illegal logical name used for ");
   PrintRouter(WERROR,theFunction);
   PrintRouter(WERROR," function.\n");
  }

#endif


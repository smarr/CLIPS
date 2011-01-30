   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/13/98            */
   /*                                                     */
   /*                COMMAND LINE MODULE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides a set of routines for processing        */
/*   commands entered at the top level prompt.               */
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

#define _COMMLINE_SOURCE_

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>
#include <ctype.h>

#include "setup.h"
#include "constant.h"

#include "symbol.h"
#include "memalloc.h"
#include "scanner.h"
#include "exprnpsr.h"
#include "argacces.h"
#include "router.h"
#include "strngrtr.h"
#include "constrct.h"
#include "prcdrfun.h"
#include "prcdrpsr.h"
#include "utility.h"
#include "filecom.h"
#include "cstrcpsr.h"

#include "commline.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if ! RUN_TIME
   static int                     DoString(char *,int,int *);
   static int                     DoComment(char *,int);
   static int                     DoWhiteSpace(char *,int);
   static void                    DefaultGetNextEvent(void);
#endif

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle int               EvaluatingTopLevelCommand = FALSE;

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

#if ! RUN_TIME

   Thread static char             *CommandString = NULL;
   Thread static int               MaximumCharacters = 0;
   Thread static int               ParsingTopLevelCommand = FALSE;
   Thread static char             *BannerString = BANNER_STRING;

   Thread static int              (*EventFunction)(void) =
                                           (int (*)(void)) DefaultGetNextEvent;
   Thread static int              (*AfterPromptFunction)(void) = NULL;

/***************************************************/
/* ExpandCommandString: Appends a character to the */
/*   command string. Returns TRUE if the command   */
/*   string was successfully expanded, otherwise   */
/*   FALSE. Expanding the string also includes     */
/*   adding a backspace character which reduces    */
/*   string's length.                              */
/***************************************************/
globle int ExpandCommandString(
  int inchar)
  {
   register int k;

   k = CommandBufferInputCount;
   CommandString = ExpandStringWithChar(inchar,CommandString,&CommandBufferInputCount,
                                        &MaximumCharacters,MaximumCharacters+80);
   return((CommandBufferInputCount != k) ? TRUE : FALSE);
  }

/******************************************************************/
/* FlushCommandString: Empties the contents of the CommandString. */
/******************************************************************/
globle void FlushCommandString()
  {
   if (CommandString != NULL) rm(CommandString,MaximumCharacters);
   CommandString = NULL;
   MaximumCharacters = 0;
   CommandBufferInputCount = 0;
  }

/*********************************************************************************/
/* SetCommandString: Sets the contents of the CommandString to a specific value. */
/*********************************************************************************/
globle void SetCommandString(
  char *str)
  {
   int length;

   FlushCommandString();
   length = strlen(str);
   CommandString = (char *)
                   genrealloc(CommandString,(unsigned) MaximumCharacters,
                              (unsigned) MaximumCharacters + length + 1);

   strcpy(CommandString,str);
   MaximumCharacters += (length + 1);
   CommandBufferInputCount += length;
  }

/*************************************************************/
/* SetNCommandString: Sets the contents of the CommandString */
/*   to a specific value up to N characters.                 */
/*************************************************************/
globle void SetNCommandString(
  char *str,
  int length)
  {
   FlushCommandString();
   CommandString = (char *)
                   genrealloc(CommandString,(unsigned) MaximumCharacters,
                              (unsigned) MaximumCharacters + length + 1);

   strncpy(CommandString,str,length);
   CommandString[MaximumCharacters + length] = 0;
   MaximumCharacters += (length + 1);
   CommandBufferInputCount += length;
  }

/******************************************************************************/
/* AppendCommandString: Appends a value to the contents of the CommandString. */
/******************************************************************************/
globle void AppendCommandString(
  char *str)
  {
   CommandString = AppendToString(str,CommandString,&CommandBufferInputCount,&MaximumCharacters);
  }

/************************************************************/
/* AppendNCommandString: Appends a value up to N characters */
/*   to the contents of the CommandString.                  */
/************************************************************/
globle void AppendNCommandString(
  char *str,
  int length)
  {
   CommandString = AppendNToString(str,CommandString,length,&CommandBufferInputCount,&MaximumCharacters);
  }

/*****************************************************************************/
/* GetCommandString: Returns a pointer to the contents of the CommandString. */
/*****************************************************************************/
globle char *GetCommandString()
  {
   return(CommandString);
  }

/**************************************************************************/
/* CompleteCommand: Determines whether a string forms a complete command. */
/*   A complete command is either a constant, a variable, or a function   */
/*   call which is followed (at some point) by a carriage return. Once a  */
/*   complete command is found (not including the parenthesis),           */
/*   extraneous parenthesis and other tokens are ignored. If a complete   */
/*   command exists, then 1 is returned. 0 is returned if the command was */
/*   not complete and without errors. -1 is returned if the command       */
/*   contains an error.                                                   */
/**************************************************************************/
globle int CompleteCommand(
  char *mstring)
  {
   int i;
   char inchar;
   int depth = 0;
   int moreThanZero = 0;
   int complete;
   int error = 0;

   if (mstring == NULL) return(0);

   /*===================================================*/
   /* Loop through each character of the command string */
   /* to determine if there is a complete command.      */
   /*===================================================*/

   i = 0;
   while ((inchar = mstring[i++]) != EOS)
     {
      switch(inchar)
        {
         /*======================================================*/
         /* If a carriage return or line feed is found, there is */
         /* at least one completed token in the command buffer,  */
         /* and parentheses are balanced, then a complete        */
         /* command has been found. Otherwise, remove all white  */
         /* space beginning with the current character.          */
         /*======================================================*/

         case '\n' :
         case '\r' :
           if (error) return(-1);
           if (moreThanZero && (depth == 0)) return(1);
           i = DoWhiteSpace(mstring,i);
           break;

         /*=====================*/
         /* Remove white space. */
         /*=====================*/

         case ' ' :
         case '\f' :
         case '\t' :
           i = DoWhiteSpace(mstring,i);
           break;

         /*======================================================*/
         /* If the opening quotation of a string is encountered, */
         /* determine if the closing quotation of the string is  */
         /* in the command buffer. Until the closing quotation   */
         /* is found, a complete command can not be made.        */
         /*======================================================*/

         case '"' :
           i = DoString(mstring,i,&complete);
           if ((depth == 0) && complete) moreThanZero = TRUE;
           break;

         /*====================*/
         /* Process a comment. */
         /*====================*/

         case ';' :
           i = DoComment(mstring,i);
           if (moreThanZero && (depth == 0) && (mstring[i] != EOS))
             {
              if (error) return(-1);
              else return(1);
             }
           else if (mstring[i] != EOS) i++;
           break;

         /*====================================================*/
         /* A left parenthesis increases the nesting depth of  */
         /* the current command by 1. Don't bother to increase */
         /* the depth if the first token encountered was not   */
         /* a parenthesis (e.g. for the command string         */
         /* "red (+ 3 4", the symbol red already forms a       */
         /* complete command, so the next carriage return will */
         /* cause evaluation of red--the closing parenthesis   */
         /* for "(+ 3 4" does not have to be found).           */
         /*====================================================*/

         case '(' :
           if ((depth > 0) || (moreThanZero == FALSE))
             {
              depth++;
              moreThanZero = TRUE;
             }
           break;

         /*====================================================*/
         /* A right parenthesis decreases the nesting depth of */
         /* the current command by 1. If the parenthesis is    */
         /* the first token of the command, then an error is   */
         /* generated.                                         */
         /*====================================================*/

         case ')' :
           if (depth > 0) depth--;
           else if (moreThanZero == FALSE) error = TRUE;
           break;

         /*=====================================================*/
         /* If the command begins with any other character and  */
         /* an opening parenthesis hasn't yet been found, then  */
         /* skip all characters on the same line. If a carriage */
         /* return or line feed is found, then a complete       */
         /* command exists.                                     */
         /*=====================================================*/

         default:
           if (depth == 0)
             {
              if (isprint(inchar))
                {
                 while ((inchar = mstring[i++]) != EOS)
                   {
                    if ((inchar == '\n') || (inchar == '\r'))
                      {
                       if (error) return(-1);
                       else return(1);
                      }
                   }
                 return(0);
                }
             }
           break;
        }
     }

   /*====================================================*/
   /* Return 0 because a complete command was not found. */
   /*====================================================*/

   return(0);
  }

/***********************************************************/
/* DoString: Skips over a string contained within a string */
/*   until the closing quotation mark is encountered.      */
/***********************************************************/
static int DoString(
  char *str,
  int pos,
  int *complete)
  {
   int inchar;

   /*=================================================*/
   /* Process the string character by character until */
   /* the closing quotation mark is found.            */
   /*=================================================*/

   inchar = str[pos];
   while (inchar  != '"')
     {
      /*=====================================================*/
      /* If a \ is found, then the next character is ignored */
      /* even if it is a closing quotation mark.             */
      /*=====================================================*/

      if (inchar == '\\')
        {
         pos++;
         inchar = str[pos];
        }

      /*===================================================*/
      /* If the end of input is reached before the closing */
      /* quotation mark is found, the return the last      */
      /* position that was reached and indicate that a     */
      /* complete string was not found.                    */
      /*===================================================*/

      if (inchar == EOS)
        {
         *complete = FALSE;
         return(pos);
        }

      /*================================*/
      /* Move on to the next character. */
      /*================================*/

      pos++;
      inchar = str[pos];
     }

   /*======================================================*/
   /* Indicate that a complete string was found and return */
   /* the position of the closing quotation mark.          */
   /*======================================================*/

   pos++;
   *complete = TRUE;
   return(pos);
  }

/*************************************************************/
/* DoComment: Skips over a comment contained within a string */
/*   until a line feed or carriage return is encountered.    */
/*************************************************************/
static int DoComment(
  char *str,
  int pos)
  {
   int inchar;

   inchar = str[pos];
   while ((inchar != '\n') && (inchar != '\r'))
     {
      if (inchar == EOS)
        { return(pos); }

      pos++;
      inchar = str[pos];
     }

   return(pos);
  }

/**************************************************************/
/* DoWhiteSpace: Skips over white space consisting of spaces, */
/*   tabs, and form feeds that is contained within a string.  */
/**************************************************************/
static int DoWhiteSpace(
  char *str,
  int pos)
  {
   int inchar;

   inchar = str[pos];
   while ((inchar == ' ') || (inchar == '\f') || (inchar == '\t'))
     {
      pos++;
      inchar = str[pos];
     }

   return(pos);
  }

/********************************************************************/
/* CommandLoop: Endless loop which waits for user commands and then */
/*   executes them. The command loop will bypass the EventFunction  */
/*   if there is an active batch file.                              */
/********************************************************************/
globle void CommandLoop()
  {
   int inchar;

   PrintRouter(WPROMPT,BannerString);
   SetHaltExecution(FALSE);
   SetEvaluationError(FALSE);
   PeriodicCleanup(TRUE,FALSE);
   PrintPrompt();
   CommandBufferInputCount = 0;

   while (TRUE)
     {
      /*===================================================*/
      /* If a batch file is active, grab the command input */
      /* directly from the batch file, otherwise call the  */
      /* event function.                                   */
      /*===================================================*/

      if (BatchActive() == TRUE)
        {
         inchar = LLGetcBatch("stdin",TRUE);
         if (inchar == EOF)
           { (*EventFunction)(); }
         else
           { ExpandCommandString((char) inchar); }
        }
      else
        { (*EventFunction)(); }

      /*=================================================*/
      /* If execution was halted, then remove everything */
      /* from the command buffer.                        */
      /*=================================================*/

      if (GetHaltExecution() == TRUE)
        {
         SetHaltExecution(FALSE);
         SetEvaluationError(FALSE);
         FlushCommandString();
#if ! WINDOW_INTERFACE
         fflush(stdin);
#endif
         PrintRouter(WPROMPT,"\n");
         PrintPrompt();
        }

      /*=========================================*/
      /* If a complete command is in the command */
      /* buffer, then execute it.                */
      /*=========================================*/

      if ((CompleteCommand(CommandString) != 0) && (CommandBufferInputCount > 0))
        {
         FlushPPBuffer();
         SetPPBufferStatus(OFF);
         CommandBufferInputCount = -1;
         RouteCommand(CommandString,TRUE);
         FlushPPBuffer();
         SetHaltExecution(FALSE);
         SetEvaluationError(FALSE);
         FlushCommandString();
         FlushBindList();
         PeriodicCleanup(TRUE,FALSE);
         PrintPrompt();
        }
     }
  }

/*******************************************/
/* PrintPrompt: Prints the command prompt. */
/*******************************************/
globle void PrintPrompt()
   {
    PrintRouter(WPROMPT,COMMAND_PROMPT);

    if (AfterPromptFunction != NULL)
      { (*AfterPromptFunction)(); }
   }

/************************************************/
/* SetAfterPromptFunction: Replaces the current */
/*   value of AfterPromptFunction.              */
/************************************************/
globle void SetAfterPromptFunction(
  int (*funptr)(void))
  {
   AfterPromptFunction = funptr;
  }

/************************************************/
/* RouteCommand: Processes a completed command. */
/************************************************/
globle BOOLEAN RouteCommand(
  char *command,
  int printResult)
  {
   DATA_OBJECT result;
   struct expr *top;
   char *commandName;
   struct token theToken;

   if (command == NULL)
     { return(0); }

   /*========================================*/
   /* Open a string input source and get the */
   /* first token from that source.          */
   /*========================================*/

   OpenStringSource("command",command,0);

   GetToken("command",&theToken);

   /*=====================*/
   /* Evaluate constants. */
   /*=====================*/

   if ((theToken.type == SYMBOL) || (theToken.type == STRING) ||
       (theToken.type == FLOAT) || (theToken.type == INTEGER) ||
       (theToken.type == INSTANCE_NAME))
     {
      CloseStringSource("command");
      if (printResult)
        {
         PrintAtom("stdout",theToken.type,theToken.value);
         PrintRouter("stdout","\n");
        }
      return(1);
     }

   /*============================*/
   /* Evaluate global variables. */
   /*============================*/

   if (theToken.type == GBL_VARIABLE)
     {
      CloseStringSource("command");
      top = GenConstant(theToken.type,theToken.value);
      EvaluateExpression(top,&result);
      rtn_struct(expr,top);
      if (printResult)
        {
         PrintDataObject("stdout",&result);
         PrintRouter("stdout","\n");
        }
      return(1);
     }

   /*========================================================*/
   /* If the next token isn't the beginning left parenthesis */
   /* of a command or construct, then whatever was entered   */
   /* cannot be evaluated at the command prompt.             */
   /*========================================================*/

   if (theToken.type != LPAREN)
     {
      PrintErrorID("COMMLINE",1,FALSE);
      PrintRouter(WERROR,"Expected a '(', constant, or global variable\n");
      CloseStringSource("command");
      return(0);
     }

   /*===========================================================*/
   /* The next token must be a function name or construct type. */
   /*===========================================================*/

   GetToken("command",&theToken);
   if (theToken.type != SYMBOL)
     {
      PrintErrorID("COMMLINE",2,FALSE);
      PrintRouter(WERROR,"Expected a command.\n");
      CloseStringSource("command");
      return(0);
     }

   commandName = ValueToString(theToken.value);

   /*======================*/
   /* Evaluate constructs. */
   /*======================*/

#if (! RUN_TIME) && (! BLOAD_ONLY)
   {
    int errorFlag;

    errorFlag = ParseConstruct(commandName,"command");
    if (errorFlag != -1)
      {
       CloseStringSource("command");
       if (errorFlag == 1)
         {
          PrintRouter(WERROR,"\nERROR:\n");
          PrintInChunks(WERROR,GetPPBuffer());
          PrintRouter(WERROR,"\n");
         }
       DestroyPPBuffer();
       return(errorFlag);
      }
   }
#endif

   /*========================*/
   /* Parse a function call. */
   /*========================*/

   ParsingTopLevelCommand = TRUE;
   top = Function2Parse("command",commandName);
   ParsingTopLevelCommand = FALSE;
   ClearParsedBindNames();

   /*================================*/
   /* Close the string input source. */
   /*================================*/

   CloseStringSource("command");

   /*=========================*/
   /* Evaluate function call. */
   /*=========================*/

   if (top == NULL) return(0);
   EvaluatingTopLevelCommand = TRUE;
   ExpressionInstall(top);
   EvaluateExpression(top,&result);
   ExpressionDeinstall(top);
   EvaluatingTopLevelCommand = FALSE;
   ReturnExpression(top);

   if ((result.type != RVOID) && printResult)
     {
      PrintDataObject("stdout",&result);
      PrintRouter("stdout","\n");
     }

   return(1);
  }

/*****************************************************************/
/* DefaultGetNextEvent: Default event-handling function. Handles */
/*   only keyboard events by first calling GetcRouter to get a   */
/*   character and then calling ExpandCommandString to add the   */
/*   character to the CommandString.                             */
/*****************************************************************/
static void DefaultGetNextEvent()
  {
   int inchar;

   inchar = GetcRouter("stdin");

   if (inchar == EOF) inchar = '\n';

   ExpandCommandString((char) inchar);
  }

/*************************************/
/* SetEventFunction: Replaces the    */
/*   current value of EventFunction. */
/*************************************/
globle int (*SetEventFunction(int (*theFunction)(void)))(void)
  {
   int (*tmp_ptr)(void);

   tmp_ptr = EventFunction;
   EventFunction = theFunction;
   return(tmp_ptr);
  }

/****************************************/
/* TopLevelCommand: Indicates whether a */
/*   top-level command is being parsed. */
/****************************************/
globle BOOLEAN TopLevelCommand()
  {
   return(ParsingTopLevelCommand);
  }

/***********************************************************/
/* GetCommandCompletionString: Returns the last token in a */
/*   string if it is a valid token for command completion. */
/***********************************************************/
globle char *GetCommandCompletionString(
  char *theString,
  int maxPosition)
  {
   struct token lastToken;
   struct token theToken;
   char lastChar;
   char *rs;
   int length;

   /*=========================*/
   /* Get the command string. */
   /*=========================*/

   if (theString == NULL) return("");

   /*=========================================================================*/
   /* If the last character in the command string is a space, character       */
   /* return, or quotation mark, then the command completion can be anything. */
   /*=========================================================================*/

   lastChar = theString[maxPosition - 1];
   if ((lastChar == ' ') || (lastChar == '"') ||
       (lastChar == '\t') || (lastChar == '\f') ||
       (lastChar == '\n') || (lastChar == '\r'))
     { return(""); }

   /*============================================*/
   /* Find the last token in the command string. */
   /*============================================*/

   OpenTextSource("CommandCompletion",theString,0,maxPosition);
   IgnoreCompletionErrors = TRUE;
   GetToken("CommandCompletion",&theToken);
   CopyToken(&lastToken,&theToken);
   while (theToken.type != STOP)
     {
      CopyToken(&lastToken,&theToken);
      GetToken("CommandCompletion",&theToken);
     }
   CloseStringSource("CommandCompletion");
   IgnoreCompletionErrors = FALSE;

   /*===============================================*/
   /* Determine if the last token can be completed. */
   /*===============================================*/

   if (lastToken.type == SYMBOL)
     {
      rs = ValueToString(lastToken.value);
      if (rs[0] == '[') return (&rs[1]);
      return(ValueToString(lastToken.value));
     }
   else if (lastToken.type == SF_VARIABLE)
     { return(ValueToString(lastToken.value)); }
   else if (lastToken.type == MF_VARIABLE)
     { return(ValueToString(lastToken.value)); }
   else if ((lastToken.type == GBL_VARIABLE) || (lastToken.type == MF_GBL_VARIABLE) ||
            (lastToken.type == INSTANCE_NAME))
     { return(NULL); }
   else if (lastToken.type == STRING)
     {
      length = strlen(ValueToString(lastToken.value));
      return(GetCommandCompletionString(ValueToString(lastToken.value),length));
     }
   else if ((lastToken.type == FLOAT) || (lastToken.type == INTEGER))
     { return(NULL); }

   return("");
  }

#endif




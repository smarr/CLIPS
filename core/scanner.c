   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                    SCANNER MODULE                   */
   /*******************************************************/

/*************************************************************/
/* Purpose: Routines for scanning lexical tokens from an     */
/*   input source.                                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Chris Culbert                                        */
/*      Brian Donnell                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _SCANNER_SOURCE_

#include <ctype.h>
#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>
#include <limits.h>

#include "setup.h"
#include "constant.h"
#include "router.h"
#include "symbol.h"
#include "utility.h"
#include "memalloc.h"

#include "scanner.h"

#include <stdlib.h>

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                   *ScanSymbol(char *,int,int *);
   static void                   *ScanString(char *);
   static void                    ScanNumber(char *,struct token *);

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread static char            *GlobalString = NULL;
   Thread static int              GlobalMax = 0;
   Thread static int              GlobalPos = 0;
   Thread static long             LineCount = 0;

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   Thread globle int              IgnoreCompletionErrors = FALSE;

/***********************************************************************/
/* GetToken: Reads next token from the input stream. The pointer to    */
/*   the token data structure passed as an argument is set to contain  */
/*   the type of token (e.g., symbol, string, integer, etc.), the data */
/*   value for the token (i.e., a symbol table location if it is a     */
/*   symbol or string, an integer table location if it is an integer), */
/*   and the pretty print representation.                              */
/***********************************************************************/
globle void GetToken(
 char *logicalName,
 struct token *theToken)
 {
   int inchar;
   int type;

   /*=======================================*/
   /* Set Unknown default values for token. */
   /*=======================================*/

   theToken->type = UNKNOWN_VALUE;
   theToken->value = NULL;
   theToken->printForm = "unknown";
   GlobalPos = 0;
   GlobalMax = 0;

   /*==============================================*/
   /* Remove all white space before processing the */
   /* GetToken() request.                          */
   /*==============================================*/

   inchar = GetcRouter(logicalName);
   while ((inchar == ' ') || (inchar == '\n') || (inchar == '\f') ||
          (inchar == '\r') || (inchar == ';') || (inchar == '\t'))
     {
      /*=======================*/
      /* Remove comment lines. */
      /*=======================*/

      if (inchar == ';')
        {
         inchar = GetcRouter(logicalName);
         while ((inchar != '\n') && (inchar != '\r') && (inchar != EOF) )
           { inchar = GetcRouter(logicalName); }
        }
      inchar = GetcRouter(logicalName);
     }

   /*==========================*/
   /* Process Symbolic Tokens. */
   /*==========================*/

   if (isalpha(inchar))
     {
      theToken->type = SYMBOL;
      UngetcRouter(inchar,logicalName);
      theToken->value = (void *) ScanSymbol(logicalName,0,&type);
      theToken->printForm = ValueToString(theToken->value);
     }

   /*===============================================*/
   /* Process Number Tokens beginning with a digit. */
   /*===============================================*/

   else if (isdigit(inchar))
     {
      UngetcRouter(inchar,logicalName);
      ScanNumber(logicalName,theToken);
     }

   else switch (inchar)
     {
      /*========================*/
      /* Process String Tokens. */
      /*========================*/

      case '"':
         theToken->value = (void *) ScanString(logicalName);
         theToken->type = STRING;
         theToken->printForm = StringPrintForm(ValueToString(theToken->value));
         break;

      /*=======================================*/
      /* Process Tokens that might be numbers. */
      /*=======================================*/

      case '-':
      case '.':
      case '+':
         UngetcRouter(inchar,logicalName);
         ScanNumber(logicalName,theToken);
         break;

      /*===================================*/
      /* Process ? and ?<variable> Tokens. */
      /*===================================*/

       case '?':
          inchar = GetcRouter(logicalName);
          if (isalpha(inchar)
#if DEFGLOBAL_CONSTRUCT
              || (inchar == '*'))
#else
              )
#endif
            {
             UngetcRouter(inchar,logicalName);
             theToken->value = (void *) ScanSymbol(logicalName,0,&type);
             theToken->type = SF_VARIABLE;
#if DEFGLOBAL_CONSTRUCT
             if ((ValueToString(theToken->value)[0] == '*') &&
                 (((int) strlen(ValueToString(theToken->value))) > 1) &&
                 (ValueToString(theToken->value)[strlen(ValueToString(theToken->value)) - 1] == '*'))
               {
                int count;

                theToken->type = GBL_VARIABLE;
                theToken->printForm = AppendStrings("?",ValueToString(theToken->value));
                count = strlen(GlobalString);
                GlobalString[count-1] = EOS;
                theToken->value = AddSymbol(GlobalString+1);
                GlobalString[count-1] = (char) inchar;

               }
             else
#endif
             theToken->printForm = AppendStrings("?",ValueToString(theToken->value));
            }
          else
            {
             theToken->type = SF_WILDCARD;
             theToken->value = (void *) AddSymbol("?");
             UngetcRouter(inchar,logicalName);
             theToken->printForm = "?";
            }
          break;

      /*=====================================*/
      /* Process $? and $?<variable> Tokens. */
      /*=====================================*/

      case '$':
         if ((inchar = GetcRouter(logicalName)) == '?')
           {
            inchar = GetcRouter(logicalName);
            if (isalpha(inchar)
#if DEFGLOBAL_CONSTRUCT
                 || (inchar == '*'))
#else
                 )
#endif
              {
               UngetcRouter(inchar,logicalName);
               theToken->value = (void *) ScanSymbol(logicalName,0,&type);
               theToken->type = MF_VARIABLE;
#if DEFGLOBAL_CONSTRUCT
             if ((ValueToString(theToken->value)[0] == '*') &&
                 ((int) (strlen(ValueToString(theToken->value))) > 1) &&
                 (ValueToString(theToken->value)[strlen(ValueToString(theToken->value)) - 1] == '*'))
               {
                int count;

                theToken->type = MF_GBL_VARIABLE;
                theToken->printForm = AppendStrings("$?",ValueToString(theToken->value));
                count = strlen(GlobalString);
                GlobalString[count-1] = EOS;
                theToken->value = AddSymbol(GlobalString+1);
                GlobalString[count-1] = (char) inchar;
               }
             else
#endif
               theToken->printForm = AppendStrings("$?",ValueToString(theToken->value));
              }
            else
              {
               theToken->type = MF_WILDCARD;
               theToken->value = (void *) AddSymbol("$?");
               theToken->printForm = "$?";
               UngetcRouter(inchar,logicalName);
              }
           }
         else
           {
            theToken->type = SYMBOL;
            GlobalString = ExpandStringWithChar('$',GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            UngetcRouter(inchar,logicalName);
            theToken->value = (void *) ScanSymbol(logicalName,1,&type);
            theToken->printForm = ValueToString(theToken->value);
           }
         break;

      /*============================*/
      /* Symbols beginning with '<' */
      /*============================*/

      case '<':
         theToken->type = SYMBOL;
         GlobalString = ExpandStringWithChar('<',GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
         theToken->value = (void *) ScanSymbol(logicalName,1,&type);
         theToken->printForm = ValueToString(theToken->value);
         break;

      /*=============================================*/
      /* Process "(", ")", "~", "|", and "&" Tokens. */
      /*=============================================*/

      case '(':
         theToken->type = LPAREN;
         theToken->value = (void *) AddSymbol("(");
         theToken->printForm = "(";
         break;

      case ')':
         theToken->type= RPAREN;
         theToken->value = (void *) AddSymbol(")");
         theToken->printForm = ")";
         break;

      case '~':
         theToken->type = NOT_CONSTRAINT;
         theToken->value = (void *) AddSymbol("~");
         theToken->printForm = "~";
         break;

      case '|':
         theToken->type = OR_CONSTRAINT;
         theToken->value = (void *) AddSymbol("|");
         theToken->printForm = "|";
         break;

      case '&':
         theToken->type =  AND_CONSTRAINT;
         theToken->value = (void *) AddSymbol("&");
         theToken->printForm = "&";
         break;

      /*============================*/
      /* Process End-of-File Token. */
      /*============================*/

      case EOF:
      case 0:
      case 3:
         theToken->type = STOP;
         theToken->value = (void *) AddSymbol("stop");
         theToken->printForm = "";
         break;

      /*=======================*/
      /* Process Other Tokens. */
      /*=======================*/

      default:
         if (isprint(inchar))
           {
            UngetcRouter(inchar,logicalName);
            theToken->value = (void *) ScanSymbol(logicalName,0,&type);
            theToken->type = type;
            theToken->printForm = ValueToString(theToken->value);
           }
         else
           { theToken->printForm = "<<<unprintable character>>>"; }
         break;
     }

   /*===============================================*/
   /* Put the new token in the pretty print buffer. */
   /*===============================================*/

#if (! RUN_TIME) && (! BLOAD_ONLY)
   if (theToken->type == INSTANCE_NAME)
     {
      SavePPBuffer("[");
      SavePPBuffer(theToken->printForm);
      SavePPBuffer("]");
     }
   else
     { SavePPBuffer(theToken->printForm); }
#endif

   /*=========================================================*/
   /* Return the temporary memory used in scanning the token. */
   /*=========================================================*/

   if (GlobalString != NULL)
     {
      rm(GlobalString,GlobalMax);
      GlobalString = NULL;
     }

   return;
  }

/*************************************/
/* ScanSymbol: Scans a symbol token. */
/*************************************/
static void *ScanSymbol(
  char *logicalName,
  int count,
  int *type)
  {
   int inchar;
#if OBJECT_SYSTEM
   void *symbol;
#endif

   /*=====================================*/
   /* Scan characters and add them to the */
   /* symbol until a delimiter is found.  */
   /*=====================================*/

   inchar = GetcRouter(logicalName);
   while ( (inchar != '<') && (inchar != '"') &&
           (inchar != '(') && (inchar != ')') &&
           (inchar != '&') && (inchar != '|') && (inchar != '~') &&
           (inchar != ' ') && (inchar != ';') &&
           isprint(inchar) )
     {
      GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);


      count++;
      inchar = GetcRouter(logicalName);
     }

   /*===================================================*/
   /* Return the last character scanned (the delimiter) */
   /* to the input stream so it will be scanned as part */
   /* of the next token.                                */
   /*===================================================*/

   UngetcRouter(inchar,logicalName);

   /*====================================================*/
   /* Add the symbol to the symbol table and return the  */
   /* symbol table address of the symbol. Symbols of the */
   /* form [<symbol>] are instance names, so the type    */
   /* returned is INSTANCE_NAME rather than SYMBOL.      */
   /*====================================================*/

#if OBJECT_SYSTEM
   if (count > 2)
     {
      if ((GlobalString[0] == '[') ? (GlobalString[count-1] == ']') : FALSE)
        {
         *type = INSTANCE_NAME;
         inchar = ']';
        }
      else
        {
         *type = SYMBOL;
         return(AddSymbol(GlobalString));
        }
      GlobalString[count-1] = EOS;
      symbol = AddSymbol(GlobalString+1);
      GlobalString[count-1] = (char) inchar;
      return(symbol);
     }
   else
     {
      *type = SYMBOL;
      return(AddSymbol(GlobalString));
     }
#else
   *type = SYMBOL;
   return(AddSymbol(GlobalString));
#endif
  }

/*************************************/
/* ScanString: Scans a string token. */
/*************************************/
static void *ScanString(
  char *logicalName)
  {
   int inchar;
   int pos = 0, max = 0;
   char *theString = NULL;
   void *thePtr;

   /*============================================*/
   /* Scan characters and add them to the string */
   /* until the " delimiter is found.            */
   /*============================================*/

   inchar = GetcRouter(logicalName);
   while ((inchar != '"') && (inchar != EOF))
     {
      if (inchar == '\\')
        { inchar = GetcRouter(logicalName); }

      theString = ExpandStringWithChar(inchar,theString,&pos,&max,max+80);
      inchar = GetcRouter(logicalName);
     }

   if ((inchar == EOF) && (IgnoreCompletionErrors == FALSE))
     { PrintRouter(WERROR,"\nEncountered End-Of-File while scanning a string\n"); }

   /*===============================================*/
   /* Add the string to the symbol table and return */
   /* the symbol table address of the string.       */
   /*===============================================*/

   if (theString == NULL)
     { thePtr = AddSymbol(""); }
   else
     {
      thePtr = AddSymbol(theString);
      rm(theString,max);
     }

   return(thePtr);
  }

/**************************************/
/* ScanNumber: Scans a numeric token. */
/**************************************/
static void ScanNumber(
  char *logicalName,
  struct token *theToken)
  {
   int count = 0;
   int inchar, phase;
   int digitFound = FALSE;
   int processFloat = FALSE;
   double fvalue;
   long lvalue;
   int type;

   /* Phases:              */
   /*  -1 = sign           */
   /*   0 = integral       */
   /*   1 = decimal        */
   /*   2 = exponent-begin */
   /*   3 = exponent-value */
   /*   5 = done           */
   /*   9 = error          */

   inchar = GetcRouter(logicalName);
   phase = -1;

   while ((phase != 5) && (phase != 9))
     {
      if (phase == -1)
        {
         if (isdigit(inchar))
           {
            phase = 0;
            digitFound = TRUE;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
           }
         else if ((inchar == '+') || (inchar == '-'))
           {
            phase = 0;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
           }
         else if (inchar == '.')
           {
            processFloat = TRUE;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
            phase = 1;
           }
         else if ((inchar == 'E') || (inchar == 'e'))
           {
            processFloat = TRUE;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
            phase = 2;
           }
         else if ( (inchar == '<') || (inchar == '"') ||
                   (inchar == '(') || (inchar == ')') ||
                   (inchar == '&') || (inchar == '|') || (inchar == '~') ||
                   (inchar == ' ') || (inchar == ';') ||
                   (isprint(inchar) == 0) )
           { phase = 5; }
         else
           {
            phase = 9;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
           }
        }
      else if (phase == 0)
        {
         if (isdigit(inchar))
           {
            digitFound = TRUE;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
           }
         else if (inchar == '.')
           {
            processFloat = TRUE;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
            phase = 1;
           }
         else if ((inchar == 'E') || (inchar == 'e'))
           {
            processFloat = TRUE;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
            phase = 2;
           }
         else if ( (inchar == '<') || (inchar == '"') ||
                   (inchar == '(') || (inchar == ')') ||
                   (inchar == '&') || (inchar == '|') || (inchar == '~') ||
                   (inchar == ' ') || (inchar == ';') ||
                   (isprint(inchar) == 0) )
           { phase = 5; }
         else
           {
            phase = 9;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
           }
        }
      else if (phase == 1)
        {
         if (isdigit(inchar))
           {
            digitFound = TRUE;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
           }
         else if ((inchar == 'E') || (inchar == 'e'))
           {
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
            phase = 2;
           }
         else if ( (inchar == '<') || (inchar == '"') ||
                   (inchar == '(') || (inchar == ')') ||
                   (inchar == '&') || (inchar == '|') || (inchar == '~') ||
                   (inchar == ' ') || (inchar == ';') ||
                   (isprint(inchar) == 0) )
           { phase = 5; }
         else
           {
            phase = 9;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
           }
        }
      else if (phase == 2)
        {
         if (isdigit(inchar))
           {
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
            phase = 3;
           }
         else if ((inchar == '+') || (inchar == '-'))
           {
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
            phase = 3;
           }
         else if ( (inchar == '<') || (inchar == '"') ||
                   (inchar == '(') || (inchar == ')') ||
                   (inchar == '&') || (inchar == '|') || (inchar == '~') ||
                   (inchar == ' ') || (inchar == ';') ||
                   (isprint(inchar) == 0) )
           {
            digitFound = FALSE;
            phase = 5;
           }
         else
           {
            phase = 9;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
           }
        }
      else if (phase == 3)
        {
         if (isdigit(inchar))
           {
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
           }
         else if ( (inchar == '<') || (inchar == '"') ||
                   (inchar == '(') || (inchar == ')') ||
                   (inchar == '&') || (inchar == '|') || (inchar == '~') ||
                   (inchar == ' ') || (inchar == ';') ||
                   (isprint(inchar) == 0) )
           {
            if ((GlobalString[count-1] == '+') || (GlobalString[count-1] == '-'))
              { digitFound = FALSE; }
            phase = 5;
           }
         else
           {
            phase = 9;
            GlobalString = ExpandStringWithChar(inchar,GlobalString,&GlobalPos,&GlobalMax,GlobalMax+80);
            count++;
           }
        }

      if ((phase != 5) && (phase != 9))
        { inchar = GetcRouter(logicalName); }
     }

   if (phase == 9)
     {
      theToken->value = (void *) ScanSymbol(logicalName,count,&type);
      theToken->type = type;
      theToken->printForm = ValueToString(theToken->value);
      return;
     }

   /*=======================================*/
   /* Stuff last character back into buffer */
   /* and return the number.                */
   /*=======================================*/

   UngetcRouter(inchar,logicalName);

   if (! digitFound)
     {
      theToken->type = SYMBOL;
      theToken->value = (void *) AddSymbol(GlobalString);
      theToken->printForm = ValueToString(theToken->value);
      return;
     }

   if (processFloat)
     {
      fvalue = atof(GlobalString);
      theToken->type = FLOAT;
      theToken->value = (void *) AddDouble(fvalue);
      theToken->printForm = FloatToString(ValueToDouble(theToken->value));
     }
   else
     {
      lvalue = atol(GlobalString);
      if ((lvalue == LONG_MAX) || (lvalue == LONG_MIN))
        {
         PrintWarningID("SCANNER",1,FALSE);
         PrintRouter(WWARNING,"Over or underflow of long integer.\n");
        }
      theToken->type = INTEGER;
      theToken->value = (void *) AddLong(lvalue);
      theToken->printForm = LongIntegerToString(ValueToLong(theToken->value));
     }

   return;
  }

/***********************************************************/
/* CopyToken: Copies values of one token to another token. */
/***********************************************************/
globle void CopyToken(
  struct token *destination,
  struct token *source)
  {
   destination->type = source->type;
   destination->value = source->value;
   destination->printForm = source->printForm;
  }

/****************************************/
/* ResetLineCount: Resets the scanner's */
/*   line count to zero.                */
/****************************************/
globle void ResetLineCount()
  {
   LineCount = 0;
  }

/****************************************************/
/* GettLineCount: Returns the scanner's line count. */
/****************************************************/
globle long GetLineCount()
  {
   return(LineCount);
  }

/**********************************/
/* IncrementLineCount: Increments */
/*   the scanner's line count.    */
/**********************************/
globle void IncrementLineCount()
  {
   LineCount++;
  }

/**********************************/
/* DecrementLineCount: Decrements */
/*   the scanner's line count.    */
/**********************************/
globle void DecrementLineCount()
  {
   LineCount--;
  }

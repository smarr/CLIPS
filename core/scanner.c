   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  03/06/08            */
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
/*      Brian Dantes                                         */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Added UTF-8 support.                           */
/*                                                           */
/*************************************************************/

#define _SCANNER_SOURCE_

#include <ctype.h>
#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>
#include <limits.h>
#include <errno.h>

#include "setup.h"
#include "constant.h"
#include "envrnmnt.h"
#include "router.h"
#include "symbol.h"
#include "utility.h"
#include "memalloc.h"
#include "sysdep.h"

#include "scanner.h"

#include <stdlib.h>

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                   *ScanSymbol(void *,EXEC_STATUS,char *,int,unsigned short *);
   static void                   *ScanString(void *,EXEC_STATUS,char *);
   static void                    ScanNumber(void *,EXEC_STATUS,char *,struct token *);
   static void                    DeallocateScannerData(void *,EXEC_STATUS);

/************************************************/
/* InitializeScannerData: Allocates environment */
/*    data for scanner routines.                */
/************************************************/
globle void InitializeScannerData(
  void *theEnv,
  EXEC_STATUS)
  {
   AllocateEnvironmentData(theEnv,execStatus,SCANNER_DATA,sizeof(struct scannerData),DeallocateScannerData);
  }
  
/**************************************************/
/* DeallocateScannerData: Deallocates environment */
/*    data for scanner routines.                  */
/**************************************************/
static void DeallocateScannerData(
  void *theEnv,
  EXEC_STATUS)
  {
   if (ScannerData(theEnv,execStatus)->GlobalMax !=  0)
     { genfree(theEnv,execStatus,ScannerData(theEnv,execStatus)->GlobalString,ScannerData(theEnv,execStatus)->GlobalMax); }
  }

/***********************************************************************/
/* GetToken: Reads next token from the input stream. The pointer to    */
/*   the token data structure passed as an argument is set to contain  */
/*   the type of token (e.g., symbol, string, integer, etc.), the data */
/*   value for the token (i.e., a symbol table location if it is a     */
/*   symbol or string, an integer table location if it is an integer), */
/*   and the pretty print representation.                              */
/***********************************************************************/
globle void GetToken(
 void *theEnv,
  EXEC_STATUS,
 char *logicalName,
 struct token *theToken)
 {
   int inchar;
   unsigned short type;

   /*=======================================*/
   /* Set Unknown default values for token. */
   /*=======================================*/

   theToken->type = UNKNOWN_VALUE;
   theToken->value = NULL;
   theToken->printForm = "unknown";
   ScannerData(theEnv,execStatus)->GlobalPos = 0;
   ScannerData(theEnv,execStatus)->GlobalMax = 0;

   /*==============================================*/
   /* Remove all white space before processing the */
   /* GetToken() request.                          */
   /*==============================================*/

   inchar = EnvGetcRouter(theEnv,execStatus,logicalName);
   while ((inchar == ' ') || (inchar == '\n') || (inchar == '\f') ||
          (inchar == '\r') || (inchar == ';') || (inchar == '\t'))
     {
      /*=======================*/
      /* Remove comment lines. */
      /*=======================*/

      if (inchar == ';')
        {
         inchar = EnvGetcRouter(theEnv,execStatus,logicalName);
         while ((inchar != '\n') && (inchar != '\r') && (inchar != EOF) )
           { inchar = EnvGetcRouter(theEnv,execStatus,logicalName); }
        }
      inchar = EnvGetcRouter(theEnv,execStatus,logicalName);
     }

   /*==========================*/
   /* Process Symbolic Tokens. */
   /*==========================*/

   if (isalpha(inchar) || IsUTF8MultiByteStart(inchar))
     {
      theToken->type = SYMBOL;
      EnvUngetcRouter(theEnv,execStatus,inchar,logicalName);
      theToken->value = (void *) ScanSymbol(theEnv,execStatus,logicalName,0,&type);
      theToken->printForm = ValueToString(theToken->value);
     }

   /*===============================================*/
   /* Process Number Tokens beginning with a digit. */
   /*===============================================*/

   else if (isdigit(inchar))
     {
      EnvUngetcRouter(theEnv,execStatus,inchar,logicalName);
      ScanNumber(theEnv,execStatus,logicalName,theToken);
     }

   else switch (inchar)
     {
      /*========================*/
      /* Process String Tokens. */
      /*========================*/

      case '"':
         theToken->value = (void *) ScanString(theEnv,execStatus,logicalName);
         theToken->type = STRING;
         theToken->printForm = StringPrintForm(theEnv,execStatus,ValueToString(theToken->value));
         break;

      /*=======================================*/
      /* Process Tokens that might be numbers. */
      /*=======================================*/

      case '-':
      case '.':
      case '+':
         EnvUngetcRouter(theEnv,execStatus,inchar,logicalName);
         ScanNumber(theEnv,execStatus,logicalName,theToken);
         break;

      /*===================================*/
      /* Process ? and ?<variable> Tokens. */
      /*===================================*/

       case '?':
          inchar = EnvGetcRouter(theEnv,execStatus,logicalName);
          if (isalpha(inchar) || IsUTF8MultiByteStart(inchar)
#if DEFGLOBAL_CONSTRUCT
              || (inchar == '*'))
#else
              )
#endif
            {
             EnvUngetcRouter(theEnv,execStatus,inchar,logicalName);
             theToken->value = (void *) ScanSymbol(theEnv,execStatus,logicalName,0,&type);
             theToken->type = SF_VARIABLE;
#if DEFGLOBAL_CONSTRUCT
             if ((ValueToString(theToken->value)[0] == '*') &&
                 (((int) strlen(ValueToString(theToken->value))) > 1) &&
                 (ValueToString(theToken->value)[strlen(ValueToString(theToken->value)) - 1] == '*'))
               {
                size_t count;

                theToken->type = GBL_VARIABLE;
                theToken->printForm = AppendStrings(theEnv,execStatus,"?",ValueToString(theToken->value));
                count = strlen(ScannerData(theEnv,execStatus)->GlobalString);
                ScannerData(theEnv,execStatus)->GlobalString[count-1] = EOS;
                theToken->value = EnvAddSymbol(theEnv,execStatus,ScannerData(theEnv,execStatus)->GlobalString+1);
                ScannerData(theEnv,execStatus)->GlobalString[count-1] = (char) inchar;

               }
             else
#endif
             theToken->printForm = AppendStrings(theEnv,execStatus,"?",ValueToString(theToken->value));
            }
          else
            {
             theToken->type = SF_WILDCARD;
             theToken->value = (void *) EnvAddSymbol(theEnv,execStatus,"?");
             EnvUngetcRouter(theEnv,execStatus,inchar,logicalName);
             theToken->printForm = "?";
            }
          break;

      /*=====================================*/
      /* Process $? and $?<variable> Tokens. */
      /*=====================================*/

      case '$':
         if ((inchar = EnvGetcRouter(theEnv,execStatus,logicalName)) == '?')
           {
            inchar = EnvGetcRouter(theEnv,execStatus,logicalName);
            if (isalpha(inchar) || IsUTF8MultiByteStart(inchar)
#if DEFGLOBAL_CONSTRUCT
                 || (inchar == '*'))
#else
                 )
#endif
              {
               EnvUngetcRouter(theEnv,execStatus,inchar,logicalName);
               theToken->value = (void *) ScanSymbol(theEnv,execStatus,logicalName,0,&type);
               theToken->type = MF_VARIABLE;
#if DEFGLOBAL_CONSTRUCT
             if ((ValueToString(theToken->value)[0] == '*') &&
                 ((int) (strlen(ValueToString(theToken->value))) > 1) &&
                 (ValueToString(theToken->value)[strlen(ValueToString(theToken->value)) - 1] == '*'))
               {
                size_t count;

                theToken->type = MF_GBL_VARIABLE;
                theToken->printForm = AppendStrings(theEnv,execStatus,"$?",ValueToString(theToken->value));
                count = strlen(ScannerData(theEnv,execStatus)->GlobalString);
                ScannerData(theEnv,execStatus)->GlobalString[count-1] = EOS;
                theToken->value = EnvAddSymbol(theEnv,execStatus,ScannerData(theEnv,execStatus)->GlobalString+1);
                ScannerData(theEnv,execStatus)->GlobalString[count-1] = (char) inchar;
               }
             else
#endif
               theToken->printForm = AppendStrings(theEnv,execStatus,"$?",ValueToString(theToken->value));
              }
            else
              {
               theToken->type = MF_WILDCARD;
               theToken->value = (void *) EnvAddSymbol(theEnv,execStatus,"$?");
               theToken->printForm = "$?";
               EnvUngetcRouter(theEnv,execStatus,inchar,logicalName);
              }
           }
         else
           {
            theToken->type = SYMBOL;
            ScannerData(theEnv,execStatus)->GlobalString = ExpandStringWithChar(theEnv,execStatus,'$',ScannerData(theEnv,execStatus)->GlobalString,&ScannerData(theEnv,execStatus)->GlobalPos,&ScannerData(theEnv,execStatus)->GlobalMax,ScannerData(theEnv,execStatus)->GlobalMax+80);
            EnvUngetcRouter(theEnv,execStatus,inchar,logicalName);
            theToken->value = (void *) ScanSymbol(theEnv,execStatus,logicalName,1,&type);
            theToken->printForm = ValueToString(theToken->value);
           }
         break;

      /*============================*/
      /* Symbols beginning with '<' */
      /*============================*/

      case '<':
         theToken->type = SYMBOL;
         ScannerData(theEnv,execStatus)->GlobalString = ExpandStringWithChar(theEnv,execStatus,'<',ScannerData(theEnv,execStatus)->GlobalString,&ScannerData(theEnv,execStatus)->GlobalPos,&ScannerData(theEnv,execStatus)->GlobalMax,ScannerData(theEnv,execStatus)->GlobalMax+80);
         theToken->value = (void *) ScanSymbol(theEnv,execStatus,logicalName,1,&type);
         theToken->printForm = ValueToString(theToken->value);
         break;

      /*=============================================*/
      /* Process "(", ")", "~", "|", and "&" Tokens. */
      /*=============================================*/

      case '(':
         theToken->type = LPAREN;
         theToken->value = (void *) EnvAddSymbol(theEnv,execStatus,"(");
         theToken->printForm = "(";
         break;

      case ')':
         theToken->type= RPAREN;
         theToken->value = (void *) EnvAddSymbol(theEnv,execStatus,")");
         theToken->printForm = ")";
         break;

      case '~':
         theToken->type = NOT_CONSTRAINT;
         theToken->value = (void *) EnvAddSymbol(theEnv,execStatus,"~");
         theToken->printForm = "~";
         break;

      case '|':
         theToken->type = OR_CONSTRAINT;
         theToken->value = (void *) EnvAddSymbol(theEnv,execStatus,"|");
         theToken->printForm = "|";
         break;

      case '&':
         theToken->type =  AND_CONSTRAINT;
         theToken->value = (void *) EnvAddSymbol(theEnv,execStatus,"&");
         theToken->printForm = "&";
         break;

      /*============================*/
      /* Process End-of-File Token. */
      /*============================*/

      case EOF:
      case 0:
      case 3:
         theToken->type = STOP;
         theToken->value = (void *) EnvAddSymbol(theEnv,execStatus,"stop");
         theToken->printForm = "";
         break;

      /*=======================*/
      /* Process Other Tokens. */
      /*=======================*/

      default:
         if (isprint(inchar))
           {
            EnvUngetcRouter(theEnv,execStatus,inchar,logicalName);
            theToken->value = (void *) ScanSymbol(theEnv,execStatus,logicalName,0,&type);
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
      SavePPBuffer(theEnv,execStatus,"[");
      SavePPBuffer(theEnv,execStatus,theToken->printForm);
      SavePPBuffer(theEnv,execStatus,"]");
     }
   else
     { SavePPBuffer(theEnv,execStatus,theToken->printForm); }
#endif

   /*=========================================================*/
   /* Return the temporary memory used in scanning the token. */
   /*=========================================================*/

   if (ScannerData(theEnv,execStatus)->GlobalString != NULL)
     {
      rm(theEnv,execStatus,ScannerData(theEnv,execStatus)->GlobalString,ScannerData(theEnv,execStatus)->GlobalMax);
      ScannerData(theEnv,execStatus)->GlobalString = NULL;
      ScannerData(theEnv,execStatus)->GlobalMax = 0;
      ScannerData(theEnv,execStatus)->GlobalPos = 0;
     }

   return;
  }

/*************************************/
/* ScanSymbol: Scans a symbol token. */
/*************************************/
static void *ScanSymbol(
  void *theEnv,
  EXEC_STATUS,
  char *logicalName,
  int count,
  unsigned short *type)
  {
   int inchar;
#if OBJECT_SYSTEM
   void *symbol;
#endif

   /*=====================================*/
   /* Scan characters and add them to the */
   /* symbol until a delimiter is found.  */
   /*=====================================*/

   inchar = EnvGetcRouter(theEnv,execStatus,logicalName);
   while ( (inchar != '<') && (inchar != '"') &&
           (inchar != '(') && (inchar != ')') &&
           (inchar != '&') && (inchar != '|') && (inchar != '~') &&
           (inchar != ' ') && (inchar != ';') &&
           (isprint(inchar) ||
            IsUTF8MultiByteStart(inchar) || 
            IsUTF8MultiByteContinuation(inchar)))
     {
      ScannerData(theEnv,execStatus)->GlobalString = ExpandStringWithChar(theEnv,execStatus,inchar,ScannerData(theEnv,execStatus)->GlobalString,&ScannerData(theEnv,execStatus)->GlobalPos,&ScannerData(theEnv,execStatus)->GlobalMax,ScannerData(theEnv,execStatus)->GlobalMax+80);

      count++;
      inchar = EnvGetcRouter(theEnv,execStatus,logicalName);
     }

   /*===================================================*/
   /* Return the last character scanned (the delimiter) */
   /* to the input stream so it will be scanned as part */
   /* of the next token.                                */
   /*===================================================*/

   EnvUngetcRouter(theEnv,execStatus,inchar,logicalName);

   /*====================================================*/
   /* Add the symbol to the symbol table and return the  */
   /* symbol table address of the symbol. Symbols of the */
   /* form [<symbol>] are instance names, so the type    */
   /* returned is INSTANCE_NAME rather than SYMBOL.      */
   /*====================================================*/

#if OBJECT_SYSTEM
   if (count > 2)
     {
      if ((ScannerData(theEnv,execStatus)->GlobalString[0] == '[') ? (ScannerData(theEnv,execStatus)->GlobalString[count-1] == ']') : FALSE)
        {
         *type = INSTANCE_NAME;
         inchar = ']';
        }
      else
        {
         *type = SYMBOL;
         return(EnvAddSymbol(theEnv,execStatus,ScannerData(theEnv,execStatus)->GlobalString));
        }
      ScannerData(theEnv,execStatus)->GlobalString[count-1] = EOS;
      symbol = EnvAddSymbol(theEnv,execStatus,ScannerData(theEnv,execStatus)->GlobalString+1);
      ScannerData(theEnv,execStatus)->GlobalString[count-1] = (char) inchar;
      return(symbol);
     }
   else
     {
      *type = SYMBOL;
      return(EnvAddSymbol(theEnv,execStatus,ScannerData(theEnv,execStatus)->GlobalString));
     }
#else
   *type = SYMBOL;
   return(EnvAddSymbol(theEnv,execStatus,ScannerData(theEnv,execStatus)->GlobalString));
#endif
  }

/*************************************/
/* ScanString: Scans a string token. */
/*************************************/
static void *ScanString(
  void *theEnv,
  EXEC_STATUS,
  char *logicalName)
  {
   int inchar;
   size_t pos = 0;
   size_t max = 0;
   char *theString = NULL;
   void *thePtr;

   /*============================================*/
   /* Scan characters and add them to the string */
   /* until the " delimiter is found.            */
   /*============================================*/

   inchar = EnvGetcRouter(theEnv,execStatus,logicalName);
   while ((inchar != '"') && (inchar != EOF))
     {
      if (inchar == '\\')
        { inchar = EnvGetcRouter(theEnv,execStatus,logicalName); }

      theString = ExpandStringWithChar(theEnv,execStatus,inchar,theString,&pos,&max,max+80);
      inchar = EnvGetcRouter(theEnv,execStatus,logicalName);
     }

   if ((inchar == EOF) && (ScannerData(theEnv,execStatus)->IgnoreCompletionErrors == FALSE))
     { 
      PrintErrorID(theEnv,execStatus,"SCANNER",1,TRUE);
      EnvPrintRouter(theEnv,execStatus,WERROR,"Encountered End-Of-File while scanning a string\n"); 
     }

   /*===============================================*/
   /* Add the string to the symbol table and return */
   /* the symbol table address of the string.       */
   /*===============================================*/

   if (theString == NULL)
     { thePtr = EnvAddSymbol(theEnv,execStatus,""); }
   else
     {
      thePtr = EnvAddSymbol(theEnv,execStatus,theString);
      rm(theEnv,execStatus,theString,max);
     }

   return(thePtr);
  }

/**************************************/
/* ScanNumber: Scans a numeric token. */
/**************************************/
static void ScanNumber(
  void *theEnv,
  EXEC_STATUS,
  char *logicalName,
  struct token *theToken)
  {
   int count = 0;
   int inchar, phase;
   int digitFound = FALSE;
   int processFloat = FALSE;
   double fvalue;
   long long lvalue;
   unsigned short type;

   /* Phases:              */
   /*  -1 = sign           */
   /*   0 = integral       */
   /*   1 = decimal        */
   /*   2 = exponent-begin */
   /*   3 = exponent-value */
   /*   5 = done           */
   /*   9 = error          */

   inchar = EnvGetcRouter(theEnv,execStatus,logicalName);
   phase = -1;

   while ((phase != 5) && (phase != 9))
     {
      if (phase == -1)
        {
         if (isdigit(inchar))
           {
            phase = 0;
            digitFound = TRUE;
            ScannerData(theEnv,execStatus)->GlobalString = ExpandStringWithChar(theEnv,execStatus,inchar,ScannerData(theEnv,execStatus)->GlobalString,&ScannerData(theEnv,execStatus)->GlobalPos,&ScannerData(theEnv,execStatus)->GlobalMax,ScannerData(theEnv,execStatus)->GlobalMax+80);
            count++;
           }
         else if ((inchar == '+') || (inchar == '-'))
           {
            phase = 0;
            ScannerData(theEnv,execStatus)->GlobalString = ExpandStringWithChar(theEnv,execStatus,inchar,ScannerData(theEnv,execStatus)->GlobalString,&ScannerData(theEnv,execStatus)->GlobalPos,&ScannerData(theEnv,execStatus)->GlobalMax,ScannerData(theEnv,execStatus)->GlobalMax+80);
            count++;
           }
         else if (inchar == '.')
           {
            processFloat = TRUE;
            ScannerData(theEnv,execStatus)->GlobalString = ExpandStringWithChar(theEnv,execStatus,inchar,ScannerData(theEnv,execStatus)->GlobalString,&ScannerData(theEnv,execStatus)->GlobalPos,&ScannerData(theEnv,execStatus)->GlobalMax,ScannerData(theEnv,execStatus)->GlobalMax+80);
            count++;
            phase = 1;
           }
         else if ((inchar == 'E') || (inchar == 'e'))
           {
            processFloat = TRUE;
            ScannerData(theEnv,execStatus)->GlobalString = ExpandStringWithChar(theEnv,execStatus,inchar,ScannerData(theEnv,execStatus)->GlobalString,&ScannerData(theEnv,execStatus)->GlobalPos,&ScannerData(theEnv,execStatus)->GlobalMax,ScannerData(theEnv,execStatus)->GlobalMax+80);
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
            ScannerData(theEnv,execStatus)->GlobalString = ExpandStringWithChar(theEnv,execStatus,inchar,ScannerData(theEnv,execStatus)->GlobalString,&ScannerData(theEnv,execStatus)->GlobalPos,&ScannerData(theEnv,execStatus)->GlobalMax,ScannerData(theEnv,execStatus)->GlobalMax+80);
            count++;
           }
        }
      else if (phase == 0)
        {
         if (isdigit(inchar))
           {
            digitFound = TRUE;
            ScannerData(theEnv,execStatus)->GlobalString = ExpandStringWithChar(theEnv,execStatus,inchar,ScannerData(theEnv,execStatus)->GlobalString,&ScannerData(theEnv,execStatus)->GlobalPos,&ScannerData(theEnv,execStatus)->GlobalMax,ScannerData(theEnv,execStatus)->GlobalMax+80);
            count++;
           }
         else if (inchar == '.')
           {
            processFloat = TRUE;
            ScannerData(theEnv,execStatus)->GlobalString = ExpandStringWithChar(theEnv,execStatus,inchar,ScannerData(theEnv,execStatus)->GlobalString,&ScannerData(theEnv,execStatus)->GlobalPos,&ScannerData(theEnv,execStatus)->GlobalMax,ScannerData(theEnv,execStatus)->GlobalMax+80);
            count++;
            phase = 1;
           }
         else if ((inchar == 'E') || (inchar == 'e'))
           {
            processFloat = TRUE;
            ScannerData(theEnv,execStatus)->GlobalString = ExpandStringWithChar(theEnv,execStatus,inchar,ScannerData(theEnv,execStatus)->GlobalString,&ScannerData(theEnv,execStatus)->GlobalPos,&ScannerData(theEnv,execStatus)->GlobalMax,ScannerData(theEnv,execStatus)->GlobalMax+80);
            count++;
            phase = 2;
           }
         else if ( (inchar == '<') || (inchar == '"') ||
                   (inchar == '(') || (inchar == ')') ||
                   (inchar == '&') || (inchar == '|') || (inchar == '~') ||
                   (inchar == ' ') || (inchar == ';') ||
                   ((isprint(inchar) == 0) && (! IsUTF8MultiByteStart(inchar))) )
           { phase = 5; }
         else
           {
            phase = 9;
            ScannerData(theEnv,execStatus)->GlobalString = ExpandStringWithChar(theEnv,execStatus,inchar,ScannerData(theEnv,execStatus)->GlobalString,&ScannerData(theEnv,execStatus)->GlobalPos,&ScannerData(theEnv,execStatus)->GlobalMax,ScannerData(theEnv,execStatus)->GlobalMax+80);
            count++;
           }
        }
      else if (phase == 1)
        {
         if (isdigit(inchar))
           {
            digitFound = TRUE;
            ScannerData(theEnv,execStatus)->GlobalString = ExpandStringWithChar(theEnv,execStatus,inchar,ScannerData(theEnv,execStatus)->GlobalString,&ScannerData(theEnv,execStatus)->GlobalPos,&ScannerData(theEnv,execStatus)->GlobalMax,ScannerData(theEnv,execStatus)->GlobalMax+80);
            count++;
           }
         else if ((inchar == 'E') || (inchar == 'e'))
           {
            ScannerData(theEnv,execStatus)->GlobalString = ExpandStringWithChar(theEnv,execStatus,inchar,ScannerData(theEnv,execStatus)->GlobalString,&ScannerData(theEnv,execStatus)->GlobalPos,&ScannerData(theEnv,execStatus)->GlobalMax,ScannerData(theEnv,execStatus)->GlobalMax+80);
            count++;
            phase = 2;
           }
         else if ( (inchar == '<') || (inchar == '"') ||
                   (inchar == '(') || (inchar == ')') ||
                   (inchar == '&') || (inchar == '|') || (inchar == '~') ||
                   (inchar == ' ') || (inchar == ';') ||
                   ((isprint(inchar) == 0) && (! IsUTF8MultiByteStart(inchar))) )
           { phase = 5; }
         else
           {
            phase = 9;
            ScannerData(theEnv,execStatus)->GlobalString = ExpandStringWithChar(theEnv,execStatus,inchar,ScannerData(theEnv,execStatus)->GlobalString,&ScannerData(theEnv,execStatus)->GlobalPos,&ScannerData(theEnv,execStatus)->GlobalMax,ScannerData(theEnv,execStatus)->GlobalMax+80);
            count++;
           }
        }
      else if (phase == 2)
        {
         if (isdigit(inchar))
           {
            ScannerData(theEnv,execStatus)->GlobalString = ExpandStringWithChar(theEnv,execStatus,inchar,ScannerData(theEnv,execStatus)->GlobalString,&ScannerData(theEnv,execStatus)->GlobalPos,&ScannerData(theEnv,execStatus)->GlobalMax,ScannerData(theEnv,execStatus)->GlobalMax+80);
            count++;
            phase = 3;
           }
         else if ((inchar == '+') || (inchar == '-'))
           {
            ScannerData(theEnv,execStatus)->GlobalString = ExpandStringWithChar(theEnv,execStatus,inchar,ScannerData(theEnv,execStatus)->GlobalString,&ScannerData(theEnv,execStatus)->GlobalPos,&ScannerData(theEnv,execStatus)->GlobalMax,ScannerData(theEnv,execStatus)->GlobalMax+80);
            count++;
            phase = 3;
           }
         else if ( (inchar == '<') || (inchar == '"') ||
                   (inchar == '(') || (inchar == ')') ||
                   (inchar == '&') || (inchar == '|') || (inchar == '~') ||
                   (inchar == ' ') || (inchar == ';') ||
                   ((isprint(inchar) == 0) && (! IsUTF8MultiByteStart(inchar))) )
           {
            digitFound = FALSE;
            phase = 5;
           }
         else
           {
            phase = 9;
            ScannerData(theEnv,execStatus)->GlobalString = ExpandStringWithChar(theEnv,execStatus,inchar,ScannerData(theEnv,execStatus)->GlobalString,&ScannerData(theEnv,execStatus)->GlobalPos,&ScannerData(theEnv,execStatus)->GlobalMax,ScannerData(theEnv,execStatus)->GlobalMax+80);
            count++;
           }
        }
      else if (phase == 3)
        {
         if (isdigit(inchar))
           {
            ScannerData(theEnv,execStatus)->GlobalString = ExpandStringWithChar(theEnv,execStatus,inchar,ScannerData(theEnv,execStatus)->GlobalString,&ScannerData(theEnv,execStatus)->GlobalPos,&ScannerData(theEnv,execStatus)->GlobalMax,ScannerData(theEnv,execStatus)->GlobalMax+80);
            count++;
           }
         else if ( (inchar == '<') || (inchar == '"') ||
                   (inchar == '(') || (inchar == ')') ||
                   (inchar == '&') || (inchar == '|') || (inchar == '~') ||
                   (inchar == ' ') || (inchar == ';') ||
                   ((isprint(inchar) == 0) && (! IsUTF8MultiByteStart(inchar))) )
           {
            if ((ScannerData(theEnv,execStatus)->GlobalString[count-1] == '+') || (ScannerData(theEnv,execStatus)->GlobalString[count-1] == '-'))
              { digitFound = FALSE; }
            phase = 5;
           }
         else
           {
            phase = 9;
            ScannerData(theEnv,execStatus)->GlobalString = ExpandStringWithChar(theEnv,execStatus,inchar,ScannerData(theEnv,execStatus)->GlobalString,&ScannerData(theEnv,execStatus)->GlobalPos,&ScannerData(theEnv,execStatus)->GlobalMax,ScannerData(theEnv,execStatus)->GlobalMax+80);
            count++;
           }
        }

      if ((phase != 5) && (phase != 9))
        { inchar = EnvGetcRouter(theEnv,execStatus,logicalName); }
     }

   if (phase == 9)
     {
      theToken->value = (void *) ScanSymbol(theEnv,execStatus,logicalName,count,&type);
      theToken->type = type;
      theToken->printForm = ValueToString(theToken->value);
      return;
     }

   /*=======================================*/
   /* Stuff last character back into buffer */
   /* and return the number.                */
   /*=======================================*/

   EnvUngetcRouter(theEnv,execStatus,inchar,logicalName);

   if (! digitFound)
     {
      theToken->type = SYMBOL;
      theToken->value = (void *) EnvAddSymbol(theEnv,execStatus,ScannerData(theEnv,execStatus)->GlobalString);
      theToken->printForm = ValueToString(theToken->value);
      return;
     }

   if (processFloat)
     {
      fvalue = atof(ScannerData(theEnv,execStatus)->GlobalString);
      theToken->type = FLOAT;
      theToken->value = (void *) EnvAddDouble(theEnv,execStatus,fvalue);
      theToken->printForm = FloatToString(theEnv,execStatus,ValueToDouble(theToken->value));
     }
   else
     {
      errno = 0;
#if WIN_MVC
      lvalue = _strtoi64(ScannerData(theEnv,execStatus)->GlobalString,NULL,10);
#else
      lvalue = strtoll(ScannerData(theEnv,execStatus)->GlobalString,NULL,10);
#endif
      if (errno)
        {
         PrintWarningID(theEnv,execStatus,"SCANNER",1,FALSE);
         EnvPrintRouter(theEnv,execStatus,WWARNING,"Over or underflow of long long integer.\n");
        }
      theToken->type = INTEGER;
      theToken->value = (void *) EnvAddLong(theEnv,execStatus,lvalue);
      theToken->printForm = LongIntegerToString(theEnv,execStatus,ValueToLong(theToken->value));
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
globle void ResetLineCount(
  void *theEnv,
  EXEC_STATUS)
  {
   ScannerData(theEnv,execStatus)->LineCount = 0;
  }

/****************************************************/
/* GettLineCount: Returns the scanner's line count. */
/****************************************************/
globle long GetLineCount(
  void *theEnv,
  EXEC_STATUS)
  {
   return(ScannerData(theEnv,execStatus)->LineCount);
  }

/**********************************/
/* IncrementLineCount: Increments */
/*   the scanner's line count.    */
/**********************************/
globle void IncrementLineCount(
  void *theEnv,
  EXEC_STATUS)
  {
   ScannerData(theEnv,execStatus)->LineCount++;
  }

/**********************************/
/* DecrementLineCount: Decrements */
/*   the scanner's line count.    */
/**********************************/
globle void DecrementLineCount(
  void *theEnv,
  EXEC_STATUS)
  {
   ScannerData(theEnv,execStatus)->LineCount--;
  }

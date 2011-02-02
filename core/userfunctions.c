   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.24  04/21/06          */
   /*                                                     */
   /*                USER FUNCTIONS MODULE                */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Created file to seperate UserFunctions and     */
/*            EnvUserFunctions from main.c.                  */
/*                                                           */
/*************************************************************/

/***************************************************************************/
/*                                                                         */
/* Permission is hereby granted, free of charge, to any person obtaining   */
/* a copy of this software and associated documentation files (the         */
/* "Software"), to deal in the Software without restriction, including     */
/* without limitation the rights to use, copy, modify, merge, publish,     */
/* distribute, and/or sell copies of the Software, and to permit persons   */
/* to whom the Software is furnished to do so.                             */
/*                                                                         */
/* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS */
/* OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF              */
/* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT   */
/* OF THIRD PARTY RIGHTS. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY  */
/* CLAIM, OR ANY SPECIAL INDIRECT OR CONSEQUENTIAL DAMAGES, OR ANY DAMAGES */
/* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN   */
/* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF */
/* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.          */
/*                                                                         */
/***************************************************************************/

#include "clips.h"

#include "factrhs.h"

void UserFunctions(void);
void EnvUserFunctions(void *);

/*********************************************************/
/* UserFunctions: Informs the expert system environment  */
/*   of any user defined functions. In the default case, */
/*   there are no user defined functions. To define      */
/*   functions, either this function must be replaced by */
/*   a function with the same name within this file, or  */
/*   this function can be deleted from this file and     */
/*   included in another file.                           */
/*********************************************************/
void UserFunctions()
  {   
  }

static struct expr *ProcessEventParse(
                                void *theEnv,
  															EXEC_STATUS,
                                struct expr *top,
                                char *logicalName)
{
  int error;
  struct expr *rv;
  struct token theToken;
  
  ReturnExpression(theEnv,execStatus,top);
  SavePPBuffer(theEnv,execStatus," ");
  IncrementIndentDepth(theEnv,execStatus,8);
  rv = BuildRHSAssert(theEnv,execStatus,logicalName,&theToken,&error,TRUE,TRUE,"proc-event command", "proc-event");
  DecrementIndentDepth(theEnv,execStatus,8);
  return(rv);
}
  
/***********************************************************/
/* EnvUserFunctions: Informs the expert system environment */
/*   of any user defined functions. In the default case,   */
/*   there are no user defined functions. To define        */
/*   functions, either this function must be replaced by   */
/*   a function with the same name within this file, or    */
/*   this function can be deleted from this file and       */
/*   included in another file.                             */
/***********************************************************/
#if WIN_BTC
#pragma argsused
#endif
void EnvUserFunctions(
  void *theEnv,
  EXEC_STATUS)
  {
    
    // STEFAN: try to add a new primitive which enters a event/fact into our
    //         parallel processing system
    EnvDefineFunction(theEnv,execStatus, "proc-event", 'v', PTIEF ProcessEventCommand,
                      "ProcessEventCommand"); 

    AddFunctionParser(theEnv,execStatus,"proc-event",ProcessEventParse);
    FuncSeqOvlFlags(theEnv,execStatus,"proc-event",FALSE,FALSE);

    
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv)
#endif
  }


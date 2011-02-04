/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version X.YY  DD/MM/YY            */
/*                                                     */
/*               EXPRESSION HEADER FILE                */
/*******************************************************/


#ifndef _H_execution_status

#define _H_execution_status

# include "expression.h"

// STEFAN: new additional parameter that needs to be passed around similar to
//         theEnv. But needs to be handled independently for different threads.
struct executionStatus
{
  struct expr *CurrentExpression;
  intBool      EvaluationError;
  intBool      HaltExecution;
  int          CurrentEvaluationDepth;
  intBool      RunningInParallel;
  int          DepthInReteNetwork;
};

// STEFAN: parameter macro for the new executionStatus
#define EXEC_STATUS struct executionStatus* execStatus


#endif

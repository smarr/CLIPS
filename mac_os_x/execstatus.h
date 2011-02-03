/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.30  10/19/06            */
/*                                                     */
/*             EXECUTION STATUS HEADER FILE            */
/*******************************************************/


# ifndef _H_execstatus
# define _H_execstatus

# include "evaluatn.h"

// STEFAN: new additional parameter that needs to be passed around similar to
//         theEnv. But needs to be handled independently for different threads.
struct executionStatus
{
	struct expr *CurrentExpression;
	int EvaluationError;
	int HaltExecution;
	int CurrentEvaluationDepth;
};

// STEFAN: parameter macro for the new executionStatus
# define EXEC_STATUS struct executionStatus* execStatus

# include "envrnmnt.h"
struct evaluationData
{
	int numberOfAddressTypes;
	struct entityRecord *PrimitivesArray[MAXIMUM_PRIMITIVES];
	struct externalAddressType *ExternalAddressTypes[MAXIMUM_EXTERNAL_ADDRESS_TYPES];
};

# define EvaluationData(theEnv,execStatus) ((struct evaluationData *) GetEnvironmentData(theEnv,execStatus,EVALUATION_DATA))

# endif

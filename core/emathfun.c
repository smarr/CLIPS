   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.22  06/15/04            */
   /*                                                     */
   /*            EXTENDED MATH FUNCTIONS MODULE           */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for numerous extended math     */
/*   functions including cos, sin, tan, sec, csc, cot, acos, */
/*   asin, atan, asec, acsc, acot, cosh, sinh, tanh, sech,   */
/*   csch, coth, acosh, asinh, atanh, asech, acsch, acoth,   */
/*   mod, exp, log, log10, sqrt, pi, deg-rad, rad-deg,       */
/*   deg-grad, grad-deg, **, and round.                      */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Gary D. Riley                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#include "setup.h"
#include "argacces.h"
#include "envrnmnt.h"
#include "extnfunc.h"
#include "router.h"

#include "emathfun.h"

#if EXTENDED_MATH_FUNCTIONS

#include <math.h>

/***************/
/* DEFINITIONS */
/***************/

#ifndef PI
#define PI   3.14159265358979323846
#endif

#ifndef PID2
#define PID2 1.57079632679489661923 /* PI divided by 2 */
#endif

#define SMALLEST_ALLOWED_NUMBER 1e-15
#define dtrunc(x) (((x) < 0.0) ? ceil(x) : floor(x))

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static int                     SingleNumberCheck(void *,EXEC_STATUS,char *,double *);
   static int                     TestProximity(double,double);
   static void                    DomainErrorMessage(void *,EXEC_STATUS,char *);
   static void                    ArgumentOverflowErrorMessage(void *,EXEC_STATUS,char *);
   static void                    SingularityErrorMessage(void *,EXEC_STATUS,char *);
   static double                  genacosh(double);
   static double                  genasinh(double);
   static double                  genatanh(double);
   static double                  genasech(double);
   static double                  genacsch(double);
   static double                  genacoth(double);

/************************************************/
/* ExtendedMathFunctionDefinitions: Initializes */
/*   the extended math functions.               */
/************************************************/
globle void ExtendedMathFunctionDefinitions(
  void *theEnv,
  EXEC_STATUS)
  {
#if ! RUN_TIME
   EnvDefineFunction2(theEnv,execStatus,"cos",      'd', PTIEF CosFunction,      "CosFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"sin",      'd', PTIEF SinFunction,      "SinFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"tan",      'd', PTIEF TanFunction,      "TanFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"sec",      'd', PTIEF SecFunction,      "SecFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"csc",      'd', PTIEF CscFunction,      "CscFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"cot",      'd', PTIEF CotFunction,      "CotFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"acos",     'd', PTIEF AcosFunction,     "AcosFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"asin",     'd', PTIEF AsinFunction,     "AsinFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"atan",     'd', PTIEF AtanFunction,     "AtanFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"asec",     'd', PTIEF AsecFunction,     "AsecFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"acsc",     'd', PTIEF AcscFunction,     "AcscFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"acot",     'd', PTIEF AcotFunction,     "AcotFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"cosh",     'd', PTIEF CoshFunction,     "CoshFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"sinh",     'd', PTIEF SinhFunction,     "SinhFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"tanh",     'd', PTIEF TanhFunction,     "TanhFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"sech",     'd', PTIEF SechFunction,     "SechFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"csch",     'd', PTIEF CschFunction,     "CschFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"coth",     'd', PTIEF CothFunction,     "CothFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"acosh",    'd', PTIEF AcoshFunction,    "AcoshFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"asinh",    'd', PTIEF AsinhFunction,    "AsinhFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"atanh",    'd', PTIEF AtanhFunction,    "AtanhFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"asech",    'd', PTIEF AsechFunction,    "AsechFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"acsch",    'd', PTIEF AcschFunction,    "AcschFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"acoth",    'd', PTIEF AcothFunction,    "AcothFunction", "11n");

   EnvDefineFunction2(theEnv,execStatus,"mod",      'n', PTIEF ModFunction,      "ModFunction", "22n");
   EnvDefineFunction2(theEnv,execStatus,"exp",      'd', PTIEF ExpFunction,      "ExpFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"log",      'd', PTIEF LogFunction,      "LogFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"log10",    'd', PTIEF Log10Function,    "Log10Function", "11n");
   EnvDefineFunction2(theEnv,execStatus,"sqrt",     'd', PTIEF SqrtFunction,     "SqrtFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"pi",       'd', PTIEF PiFunction,       "PiFunction", "00");
   EnvDefineFunction2(theEnv,execStatus,"deg-rad",  'd', PTIEF DegRadFunction,   "DegRadFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"rad-deg",  'd', PTIEF RadDegFunction,   "RadDegFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"deg-grad", 'd', PTIEF DegGradFunction,  "DegGradFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"grad-deg", 'd', PTIEF GradDegFunction,  "GradDegFunction", "11n");
   EnvDefineFunction2(theEnv,execStatus,"**",       'd', PTIEF PowFunction,      "PowFunction", "22n");
   EnvDefineFunction2(theEnv,execStatus,"round",    'g', PTIEF RoundFunction,    "RoundFunction", "11n");
#else
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv,execStatus)
#endif
#endif
  }

/************************************************************/
/* SingleNumberCheck: Retrieves the numeric argument for    */
/*   extended math functions which expect a single floating */
/*   point argument.                                        */
/************************************************************/
static int SingleNumberCheck(
  void *theEnv,
  EXEC_STATUS,
  char *functionName,
  double *theNumber)
  {
   DATA_OBJECT theValue;

   if (EnvArgCountCheck(theEnv,execStatus,functionName,EXACTLY,1) == -1) return(FALSE);
   if (EnvArgTypeCheck(theEnv,execStatus,functionName,1,FLOAT,&theValue) == FALSE) return(FALSE);

   *theNumber = DOToDouble(theValue);
   return(TRUE);
  }

/**************************************************************/
/* TestProximity: Returns TRUE if the specified number falls  */
/*   within the specified range, otherwise FALSE is returned. */
/**************************************************************/
static int TestProximity(
  double theNumber,
  double range)
  {
   if ((theNumber >= (- range)) && (theNumber <= range)) return TRUE;
   else return FALSE;
  }

/********************************************************/
/* DomainErrorMessage: Generic error message used when  */
/*   a domain error is detected during a call to one of */
/*   the extended math functions.                       */
/********************************************************/
static void DomainErrorMessage(
  void *theEnv,
  EXEC_STATUS,
  char *functionName)
  {
   PrintErrorID(theEnv,execStatus,"EMATHFUN",1,FALSE);
   EnvPrintRouter(theEnv,WERROR,"Domain error for ");
   EnvPrintRouter(theEnv,WERROR,functionName);
   EnvPrintRouter(theEnv,WERROR," function.\n");
   SetHaltExecution(theEnv,execStatus,TRUE);
   SetEvaluationError(theEnv,execStatus,TRUE);
  }

/************************************************************/
/* ArgumentOverflowErrorMessage: Generic error message used */
/*   when an argument overflow is detected during a call to */
/*   one of the extended math functions.                    */
/************************************************************/
static void ArgumentOverflowErrorMessage(
  void *theEnv,
  EXEC_STATUS,
  char *functionName)
  {
   PrintErrorID(theEnv,execStatus,"EMATHFUN",2,FALSE);
   EnvPrintRouter(theEnv,WERROR,"Argument overflow for ");
   EnvPrintRouter(theEnv,WERROR,functionName);
   EnvPrintRouter(theEnv,WERROR," function.\n");
   SetHaltExecution(theEnv,execStatus,TRUE);
   SetEvaluationError(theEnv,execStatus,TRUE);
  }

/************************************************************/
/* SingularityErrorMessage: Generic error message used when */
/*   a singularity is detected during a call to one of the  */
/*   extended math functions.                               */
/************************************************************/
static void SingularityErrorMessage(
  void *theEnv,
  EXEC_STATUS,
  char *functionName)
  {
   PrintErrorID(theEnv,execStatus,"EMATHFUN",3,FALSE);
   EnvPrintRouter(theEnv,WERROR,"Singularity at asymptote in ");
   EnvPrintRouter(theEnv,WERROR,functionName);
   EnvPrintRouter(theEnv,WERROR," function.\n");
   SetHaltExecution(theEnv,execStatus,TRUE);
   SetEvaluationError(theEnv,execStatus,TRUE);
  }

/*************************************/
/* CosFunction: H/L access routine   */
/*   for the cos function.           */
/*************************************/
globle double CosFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"cos",&num) == FALSE) return(0.0);
   return(cos(num));
  }

/*************************************/
/* SinFunction: H/L access routine   */
/*   for the sin function.           */
/*************************************/
globle double SinFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"sin",&num) == FALSE) return(0.0);
   return(sin(num));
  }

/*************************************/
/* TanFunction: H/L access routine   */
/*   for the tan function.           */
/*************************************/
globle double TanFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num, tv;

   if (SingleNumberCheck(theEnv,execStatus,"tan",&num) == FALSE) return (0.0);
   tv = cos(num);
   if ((tv < SMALLEST_ALLOWED_NUMBER) && (tv > -SMALLEST_ALLOWED_NUMBER))
     {
      SingularityErrorMessage(theEnv,execStatus,"tan");
      return(0.0);
     }

   return(sin(num) / tv);
  }

/*************************************/
/* SecFunction: H/L access routine   */
/*   for the sec function.           */
/*************************************/
globle double SecFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num, tv;

   if (SingleNumberCheck(theEnv,execStatus,"sec",&num) == FALSE) return(0.0);

   tv = cos(num);
   if ((tv < SMALLEST_ALLOWED_NUMBER) && (tv > -SMALLEST_ALLOWED_NUMBER))
     {
      SingularityErrorMessage(theEnv,execStatus,"sec");
      return(0.0);
     }

   return(1.0 / tv);
  }

/*************************************/
/* CscFunction: H/L access routine   */
/*   for the csc function.           */
/*************************************/
globle double CscFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num, tv;

   if (SingleNumberCheck(theEnv,execStatus,"csc",&num) == FALSE) return(0.0);
   tv = sin(num);
   if ((tv < SMALLEST_ALLOWED_NUMBER) && (tv > -SMALLEST_ALLOWED_NUMBER))
     {
      SingularityErrorMessage(theEnv,execStatus,"csc");
      return(0.0);
     }

   return(1.0 / tv);
  }

/*************************************/
/* CotFunction: H/L access routine   */
/*   for the cot function.           */
/*************************************/
globle double CotFunction(
  void *theEnv,
  EXEC_STATUS)
  {
    double num, tv;

    if (SingleNumberCheck(theEnv,execStatus,"cot",&num) == FALSE) return(0.0);

    tv = sin(num);
    if ((tv < SMALLEST_ALLOWED_NUMBER) && (tv > -SMALLEST_ALLOWED_NUMBER))
      {
       SingularityErrorMessage(theEnv,execStatus,"cot");
       return(0.0);
      }

    return(cos(num) / tv);
  }

/**************************************/
/* AcosFunction: H/L access routine   */
/*   for the acos function.           */
/**************************************/
globle double AcosFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"acos",&num) == FALSE) return(0.0);
   if ((num > 1.0) || (num < -1.0))
     {
      DomainErrorMessage(theEnv,execStatus,"acos");
      return(0.0);
     }
    return(acos(num));
  }

/**************************************/
/* AsinFunction: H/L access routine   */
/*   for the asin function.           */
/**************************************/
globle double AsinFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"asin",&num) == FALSE) return(0.0);
   if ((num > 1.0) || (num < -1.0))
     {
      DomainErrorMessage(theEnv,execStatus,"asin");
      return(0.0);
     }
   return(asin(num));
  }

/**************************************/
/* AtanFunction: H/L access routine   */
/*   for the atan function.           */
/**************************************/
globle double AtanFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"atan",&num) == FALSE) return(0.0);
   return(atan(num));
  }

/**************************************/
/* AsecFunction: H/L access routine   */
/*   for the asec function.           */
/**************************************/
globle double AsecFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"asec",&num) == FALSE) return(0.0);
   if ((num < 1.0) && (num > -1.0))
     {
      DomainErrorMessage(theEnv,execStatus,"asec");
      return(0.0);
     }
    num = 1.0 / num;
    return(acos(num));
  }

/**************************************/
/* AcscFunction: H/L access routine   */
/*   for the acsc function.           */
/**************************************/
globle double AcscFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"acsc",&num) == FALSE) return(0.0);
   if ((num < 1.0) && (num > -1.0))
     {
      DomainErrorMessage(theEnv,execStatus,"acsc");
      return(0.0);
     }
    num = 1.0 / num;
    return(asin(num));
  }

/**************************************/
/* AcotFunction: H/L access routine   */
/*   for the acot function.           */
/**************************************/
globle double AcotFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"acot",&num) == FALSE) return(0.0);
   if (TestProximity(num,1e-25) == TRUE)  return(PID2);
   num = 1.0 / num;
   return(atan(num));
  }

/**************************************/
/* CoshFunction: H/L access routine   */
/*   for the cosh function.           */
/**************************************/
globle double CoshFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"cosh",&num) == FALSE) return(0.0);
   return(cosh(num));
  }

/**************************************/
/* SinhFunction: H/L access routine   */
/*   for the sinh function.           */
/**************************************/
globle double SinhFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"sinh",&num) == FALSE) return(0.0);
   return(sinh(num));
  }

/**************************************/
/* TanhFunction: H/L access routine   */
/*   for the tanh function.           */
/**************************************/
globle double TanhFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"tanh",&num) == FALSE) return(0.0);
   return(tanh(num));
  }

/**************************************/
/* SechFunction: H/L access routine   */
/*   for the sech function.           */
/**************************************/
globle double SechFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"sech",&num) == FALSE) return(0.0);
   return(1.0 / cosh(num));
  }

/**************************************/
/* CschFunction: H/L access routine   */
/*   for the csch function.           */
/**************************************/
globle double CschFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"csch",&num) == FALSE) return(0.0);
   if (num == 0.0)
     {
      SingularityErrorMessage(theEnv,execStatus,"csch");
      return(0.0);
     }
   else if (TestProximity(num,1e-25) == TRUE)
     {
      ArgumentOverflowErrorMessage(theEnv,execStatus,"csch");
      return(0.0);
     }
   return(1.0 / sinh(num));
  }

/**************************************/
/* CothFunction: H/L access routine   */
/*   for the coth function.           */
/**************************************/
globle double CothFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"coth",&num) == FALSE) return(0.0);
   if (num == 0.0)
     {
      SingularityErrorMessage(theEnv,execStatus,"coth");
      return(0.0);
     }
   else if (TestProximity(num,1e-25) == TRUE)
     {
      ArgumentOverflowErrorMessage(theEnv,execStatus,"coth");
      return(0.0);
     }
   return(1.0 / tanh(num));
  }

/***************************************/
/* AcoshFunction: H/L access routine   */
/*   for the acosh function.           */
/***************************************/
globle double AcoshFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"acosh",&num) == FALSE) return(0.0);
   if (num < 1.0)
     {
      DomainErrorMessage(theEnv,execStatus,"acosh");
      return(0.0);
     }
   return(genacosh(num));
  }

/***************************************/
/* AsinhFunction: H/L access routine   */
/*   for the asinh function.           */
/***************************************/
globle double AsinhFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"asinh",&num) == FALSE) return(0.0);
   return(genasinh(num));
  }

/***************************************/
/* AtanhFunction: H/L access routine   */
/*   for the atanh function.           */
/***************************************/
globle double AtanhFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"atanh",&num) == FALSE) return(0.0);
   if ((num >= 1.0) || (num <= -1.0))
     {
      DomainErrorMessage(theEnv,execStatus,"atanh");
      return(0.0);
     }
   return(genatanh(num));
  }

/***************************************/
/* AsechFunction: H/L access routine   */
/*   for the asech function.           */
/***************************************/
globle double AsechFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"asech",&num) == FALSE) return(0.0);
   if ((num > 1.0) || (num <= 0.0))
     {
      DomainErrorMessage(theEnv,execStatus,"asech");
      return(0.0);
     }
   return(genasech(num));
  }

/***************************************/
/* AcschFunction: H/L access routine   */
/*   for the acsch function.           */
/***************************************/
globle double AcschFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"acsch",&num) == FALSE) return(0.0);
   if (num == 0.0)
     {
      DomainErrorMessage(theEnv,execStatus,"acsch");
      return(0.0);
     }
   return(genacsch(num));
  }

/***************************************/
/* AcothFunction: H/L access routine   */
/*   for the acoth function.           */
/***************************************/
globle double AcothFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"acoth",&num) == FALSE) return(0.0);
   if ((num <= 1.0) && (num >= -1.0))
     {
      DomainErrorMessage(theEnv,execStatus,"acoth");
      return(0.0);
     }
   return(genacoth(num));
  }

/*************************************/
/* ExpFunction: H/L access routine   */
/*   for the exp function.           */
/*************************************/
globle double ExpFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"exp",&num) == FALSE) return(0.0);
   return(exp(num));
  }

/*************************************/
/* LogFunction: H/L access routine   */
/*   for the log function.           */
/*************************************/
globle double LogFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"log",&num) == FALSE) return(0.0);
   if (num < 0.0)
     {
      DomainErrorMessage(theEnv,execStatus,"log");
      return(0.0);
     }
   else if (num == 0.0)
     {
      ArgumentOverflowErrorMessage(theEnv,execStatus,"log");
      return(0.0);
     }

   return(log(num));
  }

/***************************************/
/* Log10Function: H/L access routine   */
/*   for the log10 function.           */
/***************************************/
globle double Log10Function(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"log10",&num) == FALSE) return(0.0);
   if (num < 0.0)
     {
      DomainErrorMessage(theEnv,execStatus,"log10");
      return(0.0);
     }
   else if (num == 0.0)
     {
      ArgumentOverflowErrorMessage(theEnv,execStatus,"log10");
      return(0.0);
     }

    return(log10(num));
   }

/**************************************/
/* SqrtFunction: H/L access routine   */
/*   for the sqrt function.           */
/**************************************/
globle double SqrtFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"sqrt",&num) == FALSE) return(0.0);
   if (num < 0.00000)
     {
      DomainErrorMessage(theEnv,execStatus,"sqrt");
      return(0.0);
     }
   return(sqrt(num));
  }

/*************************************/
/* PowFunction: H/L access routine   */
/*   for the pow function.           */
/*************************************/
globle double PowFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   DATA_OBJECT value1, value2;

   if (EnvArgCountCheck(theEnv,execStatus,"**",EXACTLY,2) == -1) return(0.0);

   if (EnvArgTypeCheck(theEnv,execStatus,"**",1,FLOAT,&value1) == FALSE) return(0.0);
   if (EnvArgTypeCheck(theEnv,execStatus,"**",2,FLOAT,&value2) == FALSE) return(0.0);

    if (((DOToDouble(value1) == 0.0) &&
        (DOToDouble(value2) <= 0.0)) ||
       ((DOToDouble(value1) < 0.0) &&
        (dtrunc((double) DOToDouble(value2)) != DOToDouble(value2))))
     {
      DomainErrorMessage(theEnv,execStatus,"**");
      SetHaltExecution(theEnv,execStatus,TRUE);
      SetEvaluationError(theEnv,execStatus,TRUE);
      return(0.0);
     }

   return (pow(DOToDouble(value1),DOToDouble(value2)));
  }

/*************************************/
/* ModFunction: H/L access routine   */
/*   for the mod function.           */
/*************************************/
globle void ModFunction(
  void *theEnv,
  EXEC_STATUS,
  DATA_OBJECT_PTR result)
  {
   DATA_OBJECT item1, item2;
   double fnum1, fnum2;
   long long lnum1, lnum2;

   if (EnvArgCountCheck(theEnv,execStatus,"mod",EXACTLY,2) == -1)
     {
      result->type = INTEGER;
      result->value = (void *) EnvAddLong(theEnv,execStatus,0L);
      return;
     }

   if (EnvArgTypeCheck(theEnv,execStatus,"mod",1,INTEGER_OR_FLOAT,&item1) == FALSE)
     {
      result->type = INTEGER;
      result->value = (void *) EnvAddLong(theEnv,execStatus,0L);
      return;
     }

   if (EnvArgTypeCheck(theEnv,execStatus,"mod",2,INTEGER_OR_FLOAT,&item2) == FALSE)
     {
      result->type = INTEGER;
      result->value = (void *) EnvAddLong(theEnv,execStatus,0L);
      return;
     }

   if (((item2.type == INTEGER) ? (ValueToLong(item2.value) == 0L) : FALSE) ||
       ((item2.type == FLOAT) ? ValueToDouble(item2.value) == 0.0 : FALSE))
     {
      DivideByZeroErrorMessage(theEnv,execStatus,"mod");
      SetEvaluationError(theEnv,execStatus,TRUE);
      result->type = INTEGER;
      result->value = (void *) EnvAddLong(theEnv,execStatus,0L);
      return;
     }

   if ((item1.type == FLOAT) || (item2.type == FLOAT))
     {
      fnum1 = CoerceToDouble(item1.type,item1.value);
      fnum2 = CoerceToDouble(item2.type,item2.value);
      result->type = FLOAT;
      result->value = (void *) EnvAddDouble(theEnv,execStatus,fnum1 - (dtrunc(fnum1 / fnum2) * fnum2));
     }
   else
     {
      lnum1 = DOToLong(item1);
      lnum2 = DOToLong(item2);
      result->type = INTEGER;
      result->value = (void *) EnvAddLong(theEnv,execStatus,lnum1 - (lnum1 / lnum2) * lnum2);
     }
  }

/************************************/
/* PiFunction: H/L access routine   */
/*   for the pi function.           */
/************************************/
globle double PiFunction(
  void *theEnv,
  EXEC_STATUS)
  {

   if (EnvArgCountCheck(theEnv,execStatus,"pi",EXACTLY,0) == -1) return(acos(-1.0));
   return(acos(-1.0));
  }

/****************************************/
/* DegRadFunction: H/L access routine   */
/*   for the deg-rad function.          */
/****************************************/
globle double DegRadFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"deg-rad",&num) == FALSE) return(0.0);
   return(num * PI / 180.0);
  }

/****************************************/
/* RadDegFunction: H/L access routine   */
/*   for the rad-deg function.          */
/****************************************/
globle double RadDegFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"rad-deg",&num) == FALSE) return(0.0);
   return(num * 180.0 / PI);
  }

/*****************************************/
/* DegGradFunction: H/L access routine   */
/*   for the deg-grad function.          */
/*****************************************/
globle double DegGradFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"deg-grad",&num) == FALSE) return(0.0);
   return(num / 0.9);
  }

/*****************************************/
/* GradDegFunction: H/L access routine   */
/*   for the grad-deg function.          */
/*****************************************/
globle double GradDegFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   double num;

   if (SingleNumberCheck(theEnv,execStatus,"grad-deg",&num) == FALSE) return(0.0);
   return(num * 0.9);
  }

/***************************************/
/* RoundFunction: H/L access routine   */
/*   for the round function.           */
/***************************************/
globle long long RoundFunction(
  void *theEnv,
  EXEC_STATUS)
  {
   DATA_OBJECT result;

   if (EnvArgCountCheck(theEnv,execStatus,"round",EXACTLY,1) == -1)
     { return(0LL); }

   if (EnvArgTypeCheck(theEnv,execStatus,"round",1,INTEGER_OR_FLOAT,&result) == FALSE)
     { return(0LL); }

   if (result.type == INTEGER)
     { return(ValueToLong(result.value)); }
   else
     { return((long long) ceil(ValueToDouble(result.value) - 0.5)); }
  }

/*******************************************/
/* genacosh: Generic routine for computing */
/*   the hyperbolic arccosine.             */
/*******************************************/
static double genacosh(
  double num)
  {
   return(log(num + sqrt(num * num - 1.0)));
  }

/*******************************************/
/* genasinh: Generic routine for computing */
/*   the hyperbolic arcsine.               */
/*******************************************/
static double genasinh(
  double num)
  {
   return(log(num + sqrt(num * num + 1.0)));
  }

/*******************************************/
/* genatanh: Generic routine for computing */
/*   the hyperbolic arctangent.            */
/*******************************************/
static double genatanh(
  double num)
  {
   return((0.5) * log((1.0 + num) / (1.0 - num)));
  }

/*******************************************/
/* genasech: Generic routine for computing */
/*   the hyperbolic arcsecant.             */
/*******************************************/
static double genasech(
  double num)
  {
   return(log(1.0 / num + sqrt(1.0 / (num * num) - 1.0)));
  }

/*******************************************/
/* genacsch: Generic routine for computing */
/*   the hyperbolic arccosecant.           */
/*******************************************/
static double genacsch(
  double num)
  {
   return(log(1.0 / num + sqrt(1.0 / (num * num) + 1.0)));
  }

/*******************************************/
/* genacoth: Generic routine for computing */
/*   the hyperbolic arccotangent.          */
/*******************************************/
static double genacoth(
  double num)
  {
   return((0.5) * log((num + 1.0) / (num - 1.0)));
  }

#endif


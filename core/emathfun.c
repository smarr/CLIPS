   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
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
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Gary D. Riley                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#include "setup.h"
#include "argacces.h"
#include "extnfunc.h"
#include "router.h"

#include "emathfun.h"

#if EX_MATH

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

   static int                     SingleNumberCheck(char *,double *);
   static int                     TestProximity(double,double);
   static void                    DomainErrorMessage(char *);
   static void                    ArgumentOverflowErrorMessage(char *);
   static void                    SingularityErrorMessage(char *);
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
#if ! RUN_TIME
globle void ExtendedMathFunctionDefinitions()
  {
   DefineFunction2("cos",      'd', PTIF CosFunction,      "CosFunction", "11n");
   DefineFunction2("sin",      'd', PTIF SinFunction,      "SinFunction", "11n");
   DefineFunction2("tan",      'd', PTIF TanFunction,      "TanFunction", "11n");
   DefineFunction2("sec",      'd', PTIF SecFunction,      "SecFunction", "11n");
   DefineFunction2("csc",      'd', PTIF CscFunction,      "CscFunction", "11n");
   DefineFunction2("cot",      'd', PTIF CotFunction,      "CotFunction", "11n");
   DefineFunction2("acos",     'd', PTIF AcosFunction,     "AcosFunction", "11n");
   DefineFunction2("asin",     'd', PTIF AsinFunction,     "AsinFunction", "11n");
   DefineFunction2("atan",     'd', PTIF AtanFunction,     "AtanFunction", "11n");
   DefineFunction2("asec",     'd', PTIF AsecFunction,     "AsecFunction", "11n");
   DefineFunction2("acsc",     'd', PTIF AcscFunction,     "AcscFunction", "11n");
   DefineFunction2("acot",     'd', PTIF AcotFunction,     "AcotFunction", "11n");
   DefineFunction2("cosh",     'd', PTIF CoshFunction,     "CoshFunction", "11n");
   DefineFunction2("sinh",     'd', PTIF SinhFunction,     "SinhFunction", "11n");
   DefineFunction2("tanh",     'd', PTIF TanhFunction,     "TanhFunction", "11n");
   DefineFunction2("sech",     'd', PTIF SechFunction,     "SechFunction", "11n");
   DefineFunction2("csch",     'd', PTIF CschFunction,     "CschFunction", "11n");
   DefineFunction2("coth",     'd', PTIF CothFunction,     "CothFunction", "11n");
   DefineFunction2("acosh",    'd', PTIF AcoshFunction,    "AcoshFunction", "11n");
   DefineFunction2("asinh",    'd', PTIF AsinhFunction,    "AsinhFunction", "11n");
   DefineFunction2("atanh",    'd', PTIF AtanhFunction,    "AtanhFunction", "11n");
   DefineFunction2("asech",    'd', PTIF AsechFunction,    "AsechFunction", "11n");
   DefineFunction2("acsch",    'd', PTIF AcschFunction,    "AcschFunction", "11n");
   DefineFunction2("acoth",    'd', PTIF AcothFunction,    "AcothFunction", "11n");

   DefineFunction2("mod",      'n', PTIF ModFunction,      "ModFunction", "22n");
   DefineFunction2("exp",      'd', PTIF ExpFunction,      "ExpFunction", "11n");
   DefineFunction2("log",      'd', PTIF LogFunction,      "LogFunction", "11n");
   DefineFunction2("log10",    'd', PTIF Log10Function,    "Log10Function", "11n");
   DefineFunction2("sqrt",     'd', PTIF SqrtFunction,     "SqrtFunction", "11n");
   DefineFunction2("pi",       'd', PTIF PiFunction,       "PiFunction", "00");
   DefineFunction2("deg-rad",  'd', PTIF DegRadFunction,   "DegRadFunction", "11n");
   DefineFunction2("rad-deg",  'd', PTIF RadDegFunction,   "RadDegFunction", "11n");
   DefineFunction2("deg-grad", 'd', PTIF DegGradFunction,  "DegGradFunction", "11n");
   DefineFunction2("grad-deg", 'd', PTIF GradDegFunction,  "GradDegFunction", "11n");
   DefineFunction2("**",       'd', PTIF PowFunction,      "PowFunction", "22n");
   DefineFunction2("round",    'l', PTIF RoundFunction,    "RoundFunction", "11n");
  }
#endif

/************************************************************/
/* SingleNumberCheck: Retrieves the numeric argument for    */
/*   extended math functions which expect a single floating */
/*   point argument.                                        */
/************************************************************/
static int SingleNumberCheck(
  char *functionName,
  double *theNumber)
  {
   DATA_OBJECT theValue;

   if (ArgCountCheck(functionName,EXACTLY,1) == -1) return(FALSE);
   if (ArgTypeCheck(functionName,1,FLOAT,&theValue) == FALSE) return(FALSE);

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
  char *functionName)
  {
   PrintErrorID("EMATHFUN",1,FALSE);
   PrintRouter(WERROR,"Domain error for ");
   PrintRouter(WERROR,functionName);
   PrintRouter(WERROR," function.\n");
   SetHaltExecution(TRUE);
   SetEvaluationError(TRUE);
  }

/************************************************************/
/* ArgumentOverflowErrorMessage: Generic error message used */
/*   when an argument overflow is detected during a call to */
/*   one of the extended math functions.                    */
/************************************************************/
static void ArgumentOverflowErrorMessage(
  char *functionName)
  {
   PrintErrorID("EMATHFUN",2,FALSE);
   PrintRouter(WERROR,"Argument overflow for ");
   PrintRouter(WERROR,functionName);
   PrintRouter(WERROR," function.\n");
   SetHaltExecution(TRUE);
   SetEvaluationError(TRUE);
  }

/************************************************************/
/* SingularityErrorMessage: Generic error message used when */
/*   a singularity is detected during a call to one of the  */
/*   extended math functions.                               */
/************************************************************/
static void SingularityErrorMessage(
  char *functionName)
  {
   PrintErrorID("EMATHFUN",3,FALSE);
   PrintRouter(WERROR,"Singularity at asymptote in ");
   PrintRouter(WERROR,functionName);
   PrintRouter(WERROR," function.\n");
   SetHaltExecution(TRUE);
   SetEvaluationError(TRUE);
  }

/*************************************/
/* CosFunction: H/L access routine   */
/*   for the cos function.           */
/*************************************/
globle double CosFunction()
  {
   double num;

   if (SingleNumberCheck("cos",&num) == FALSE) return(0.0);
   return(cos(num));
  }

/*************************************/
/* SinFunction: H/L access routine   */
/*   for the sin function.           */
/*************************************/
globle double SinFunction()
  {
   double num;

   if (SingleNumberCheck("sin",&num) == FALSE) return(0.0);
   return(sin(num));
  }

/*************************************/
/* TanFunction: H/L access routine   */
/*   for the tan function.           */
/*************************************/
globle double TanFunction()
  {
   double num, tv;

   if (SingleNumberCheck("tan",&num) == FALSE) return (0.0);
   tv = cos(num);
   if ((tv < SMALLEST_ALLOWED_NUMBER) && (tv > -SMALLEST_ALLOWED_NUMBER))
     {
      SingularityErrorMessage("tan");
      return(0.0);
     }

   return(sin(num) / tv);
  }

/*************************************/
/* SecFunction: H/L access routine   */
/*   for the sec function.           */
/*************************************/
globle double SecFunction()
  {
   double num, tv;

   if (SingleNumberCheck("sec",&num) == FALSE) return(0.0);

   tv = cos(num);
   if ((tv < SMALLEST_ALLOWED_NUMBER) && (tv > -SMALLEST_ALLOWED_NUMBER))
     {
      SingularityErrorMessage("sec");
      return(0.0);
     }

   return(1.0 / tv);
  }

/*************************************/
/* CscFunction: H/L access routine   */
/*   for the csc function.           */
/*************************************/
globle double CscFunction()
  {
   double num, tv;

   if (SingleNumberCheck("csc",&num) == FALSE) return(0.0);
   tv = sin(num);
   if ((tv < SMALLEST_ALLOWED_NUMBER) && (tv > -SMALLEST_ALLOWED_NUMBER))
     {
      SingularityErrorMessage("csc");
      return(0.0);
     }

   return(1.0 / tv);
  }

/*************************************/
/* CotFunction: H/L access routine   */
/*   for the cot function.           */
/*************************************/
globle double CotFunction()
  {
    double num, tv;

    if (SingleNumberCheck("cot",&num) == FALSE) return(0.0);

    tv = sin(num);
    if ((tv < SMALLEST_ALLOWED_NUMBER) && (tv > -SMALLEST_ALLOWED_NUMBER))
      {
       SingularityErrorMessage("cot");
       return(0.0);
      }

    return(cos(num) / tv);
  }

/**************************************/
/* AcosFunction: H/L access routine   */
/*   for the acos function.           */
/**************************************/
globle double AcosFunction()
  {
   double num;

   if (SingleNumberCheck("acos",&num) == FALSE) return(0.0);
   if ((num > 1.0) || (num < -1.0))
     {
      DomainErrorMessage("acos");
      return(0.0);
     }
    return(acos(num));
  }

/**************************************/
/* AsinFunction: H/L access routine   */
/*   for the asin function.           */
/**************************************/
globle double AsinFunction()
  {
   double num;

   if (SingleNumberCheck("asin",&num) == FALSE) return(0.0);
   if ((num > 1.0) || (num < -1.0))
     {
      DomainErrorMessage("asin");
      return(0.0);
     }
   return(asin(num));
  }

/**************************************/
/* AtanFunction: H/L access routine   */
/*   for the atan function.           */
/**************************************/
globle double AtanFunction()
  {
    double num;

    if (SingleNumberCheck("atan",&num) == FALSE) return(0.0);
    return(atan(num));
  }

/**************************************/
/* AsecFunction: H/L access routine   */
/*   for the asec function.           */
/**************************************/
globle double AsecFunction()
  {
   double num;

   if (SingleNumberCheck("asec",&num) == FALSE) return(0.0);
   if ((num < 1.0) && (num > -1.0))
     {
      DomainErrorMessage("asec");
      return(0.0);
     }
    num = 1.0 / num;
    return(acos(num));
  }

/**************************************/
/* AcscFunction: H/L access routine   */
/*   for the acsc function.           */
/**************************************/
globle double AcscFunction()
  {
   double num;

   if (SingleNumberCheck("acsc",&num) == FALSE) return(0.0);
   if ((num < 1.0) && (num > -1.0))
     {
      DomainErrorMessage("acsc");
      return(0.0);
     }
    num = 1.0 / num;
    return(asin(num));
  }

/**************************************/
/* AcotFunction: H/L access routine   */
/*   for the acot function.           */
/**************************************/
globle double AcotFunction()
  {
   double num;

   if (SingleNumberCheck("acot",&num) == FALSE) return(0.0);
   if (TestProximity(num,1e-25) == TRUE)  return(PID2);
   num = 1.0 / num;
   return(atan(num));
  }

/**************************************/
/* CoshFunction: H/L access routine   */
/*   for the cosh function.           */
/**************************************/
globle double CoshFunction()
  {
   double num;

   if (SingleNumberCheck("cosh",&num) == FALSE) return(0.0);
   return(cosh(num));
  }

/**************************************/
/* SinhFunction: H/L access routine   */
/*   for the sinh function.           */
/**************************************/
globle double SinhFunction()
  {
   double num;

   if (SingleNumberCheck("sinh",&num) == FALSE) return(0.0);
   return(sinh(num));
  }

/**************************************/
/* TanhFunction: H/L access routine   */
/*   for the tanh function.           */
/**************************************/
globle double TanhFunction()
  {
   double num;

   if (SingleNumberCheck("tanh",&num) == FALSE) return(0.0);
   return(tanh(num));
  }

/**************************************/
/* SechFunction: H/L access routine   */
/*   for the sech function.           */
/**************************************/
globle double SechFunction()
  {
   double num;

   if (SingleNumberCheck("sech",&num) == FALSE) return(0.0);
   return(1.0 / cosh(num));
  }

/**************************************/
/* CschFunction: H/L access routine   */
/*   for the csch function.           */
/**************************************/
globle double CschFunction()
  {
   double num;

   if (SingleNumberCheck("csch",&num) == FALSE) return(0.0);
   if (num == 0.0)
     {
      SingularityErrorMessage("csch");
      return(0.0);
     }
   else if (TestProximity(num,1e-25) == TRUE)
     {
      ArgumentOverflowErrorMessage("csch");
      return(0.0);
     }
   return(1.0 / sinh(num));
  }

/**************************************/
/* CothFunction: H/L access routine   */
/*   for the coth function.           */
/**************************************/
globle double CothFunction()
  {
   double num;

   if (SingleNumberCheck("coth",&num) == FALSE) return(0.0);
   if (num == 0.0)
     {
      SingularityErrorMessage("coth");
      return(0.0);
     }
   else if (TestProximity(num,1e-25) == TRUE)
     {
      ArgumentOverflowErrorMessage("coth");
      return(0.0);
     }
   return(1.0 / tanh(num));
  }

/***************************************/
/* AcoshFunction: H/L access routine   */
/*   for the acosh function.           */
/***************************************/
globle double AcoshFunction()
  {
   double num;

   if (SingleNumberCheck("acosh",&num) == FALSE) return(0.0);
   if (num < 1.0)
     {
      DomainErrorMessage("acosh");
      return(0.0);
     }
   return(genacosh(num));
  }

/***************************************/
/* AsinhFunction: H/L access routine   */
/*   for the asinh function.           */
/***************************************/
globle double AsinhFunction()
  {
   double num;

   if (SingleNumberCheck("asinh",&num) == FALSE) return(0.0);
   return(genasinh(num));
  }

/***************************************/
/* AtanhFunction: H/L access routine   */
/*   for the atanh function.           */
/***************************************/
globle double AtanhFunction()
  {
   double num;

   if (SingleNumberCheck("atanh",&num) == FALSE) return(0.0);
   if ((num >= 1.0) || (num <= -1.0))
     {
      DomainErrorMessage("atanh");
      return(0.0);
     }
   return(genatanh(num));
  }

/***************************************/
/* AsechFunction: H/L access routine   */
/*   for the asech function.           */
/***************************************/
globle double AsechFunction()
  {
   double num;

   if (SingleNumberCheck("asech",&num) == FALSE) return(0.0);
   if ((num > 1.0) || (num <= 0.0))
     {
      DomainErrorMessage("asech");
      return(0.0);
     }
   return(genasech(num));
  }

/***************************************/
/* AcschFunction: H/L access routine   */
/*   for the acsch function.           */
/***************************************/
globle double AcschFunction()
  {
   double num;

   if (SingleNumberCheck("acsch",&num) == FALSE) return(0.0);
   if (num == 0.0)
     {
      DomainErrorMessage("acsch");
      return(0.0);
     }
   return(genacsch(num));
  }

/***************************************/
/* AcothFunction: H/L access routine   */
/*   for the acoth function.           */
/***************************************/
globle double AcothFunction()
  {
   double num;

   if (SingleNumberCheck("acoth",&num) == FALSE) return(0.0);
   if ((num <= 1.0) && (num >= -1.0))
     {
      DomainErrorMessage("acoth");
      return(0.0);
     }
   return(genacoth(num));
  }

/*************************************/
/* ExpFunction: H/L access routine   */
/*   for the exp function.           */
/*************************************/
globle double ExpFunction()
  {
   double num;

   if (SingleNumberCheck("exp",&num) == FALSE) return(0.0);
   return(exp(num));
  }

/*************************************/
/* LogFunction: H/L access routine   */
/*   for the log function.           */
/*************************************/
globle double LogFunction()
  {
   double num;

   if (SingleNumberCheck("log",&num) == FALSE) return(0.0);
   if (num < 0.0)
     {
      DomainErrorMessage("log");
      return(0.0);
     }
   else if (num == 0.0)
     {
      ArgumentOverflowErrorMessage("log");
      return(0.0);
     }

   return(log(num));
  }

/***************************************/
/* Log10Function: H/L access routine   */
/*   for the log10 function.           */
/***************************************/
globle double Log10Function()
  {
   double num;

   if (SingleNumberCheck("log10",&num) == FALSE) return(0.0);
   if (num < 0.0)
     {
      DomainErrorMessage("log10");
      return(0.0);
     }
   else if (num == 0.0)
     {
      ArgumentOverflowErrorMessage("log10");
      return(0.0);
     }

    return(log10(num));
   }

/**************************************/
/* SqrtFunction: H/L access routine   */
/*   for the sqrt function.           */
/**************************************/
globle double SqrtFunction()
  {
   double num;

   if (SingleNumberCheck("sqrt",&num) == FALSE) return(0.0);
   if (num < 0.00000)
     {
      DomainErrorMessage("sqrt");
      return(0.0);
     }
   return(sqrt(num));
  }

/*************************************/
/* PowFunction: H/L access routine   */
/*   for the pow function.           */
/*************************************/
globle double PowFunction()
  {
   DATA_OBJECT value1, value2;

   if (ArgCountCheck("**",EXACTLY,2) == -1) return(0.0);

   if (ArgTypeCheck("**",1,FLOAT,&value1) == FALSE) return(0.0);
   if (ArgTypeCheck("**",2,FLOAT,&value2) == FALSE) return(0.0);

    if (((DOToDouble(value1) == 0.0) &&
        (DOToDouble(value2) <= 0.0)) ||
       ((DOToDouble(value1) < 0.0) &&
        (dtrunc((double) DOToDouble(value2)) != DOToDouble(value2))))
     {
      DomainErrorMessage("**");
      SetHaltExecution(TRUE);
      SetEvaluationError(TRUE);
      return(0.0);
     }

   return (pow(DOToDouble(value1),DOToDouble(value2)));
  }

/*************************************/
/* ModFunction: H/L access routine   */
/*   for the mod function.           */
/*************************************/
globle void ModFunction(
  DATA_OBJECT_PTR result)
  {
   DATA_OBJECT item1, item2;
   double fnum1, fnum2;
   long lnum1, lnum2;

   if (ArgCountCheck("mod",EXACTLY,2) == -1)
     {
      result->type = INTEGER;
      result->value = (void *) AddLong(0L);
      return;
     }

   if (ArgTypeCheck("mod",1,INTEGER_OR_FLOAT,&item1) == FALSE)
     {
      result->type = INTEGER;
      result->value = (void *) AddLong(0L);
      return;
     }

   if (ArgTypeCheck("mod",2,INTEGER_OR_FLOAT,&item2) == FALSE)
     {
      result->type = INTEGER;
      result->value = (void *) AddLong(0L);
      return;
     }

   if (((item2.type == INTEGER) ? (ValueToLong(item2.value) == 0L) : FALSE) ||
       ((item2.type == FLOAT) ? ValueToDouble(item2.value) == 0.0 : FALSE))
     {
      DivideByZeroErrorMessage("mod");
      SetEvaluationError(TRUE);
      result->type = INTEGER;
      result->value = (void *) AddLong(0L);
      return;
     }

   if ((item1.type == FLOAT) || (item2.type == FLOAT))
     {
      fnum1 = CoerceToDouble(item1.type,item1.value);
      fnum2 = CoerceToDouble(item2.type,item2.value);
      result->type = FLOAT;
      result->value = (void *) AddDouble(fnum1 - (dtrunc(fnum1 / fnum2) * fnum2));
     }
   else
     {
      lnum1 = DOToLong(item1);
      lnum2 = DOToLong(item2);
      result->type = INTEGER;
      result->value = (void *) AddLong(lnum1 - (lnum1 / lnum2) * lnum2);
     }
  }

/************************************/
/* PiFunction: H/L access routine   */
/*   for the pi function.           */
/************************************/
globle double PiFunction()
  {
   if (ArgCountCheck("pi",EXACTLY,0) == -1) return(acos(-1.0));
   return(acos(-1.0));
  }

/****************************************/
/* DegRadFunction: H/L access routine   */
/*   for the deg-rad function.          */
/****************************************/
globle double DegRadFunction()
  {
   double num;

   if (SingleNumberCheck("deg-rad",&num) == FALSE) return(0.0);
   return(num * PI / 180.0);
  }

/****************************************/
/* RadDegFunction: H/L access routine   */
/*   for the rad-deg function.          */
/****************************************/
globle double RadDegFunction()
  {
   double num;

   if (SingleNumberCheck("rad-deg",&num) == FALSE) return(0.0);
   return(num * 180.0 / PI);
  }

/*****************************************/
/* DegGradFunction: H/L access routine   */
/*   for the deg-grad function.          */
/*****************************************/
globle double DegGradFunction()
  {
   double num;

   if (SingleNumberCheck("deg-grad",&num) == FALSE) return(0.0);
   return(num / 0.9);
  }

/*****************************************/
/* GradDegFunction: H/L access routine   */
/*   for the grad-deg function.          */
/*****************************************/
globle double GradDegFunction()
  {
   double num;

   if (SingleNumberCheck("grad-deg",&num) == FALSE) return(0.0);
   return(num * 0.9);
  }

/***************************************/
/* RoundFunction: H/L access routine   */
/*   for the round function.           */
/***************************************/
globle long int RoundFunction()
  {
   DATA_OBJECT result;

   if (ArgCountCheck("round",EXACTLY,1) == -1)
     { return(0L); }

   if (ArgTypeCheck("round",1,INTEGER_OR_FLOAT,&result) == FALSE)
     { return(0L); }

   if (result.type == INTEGER)
     { return(ValueToLong(result.value)); }
   else
     { return((long) ceil(ValueToDouble(result.value) - 0.5)); }
  }

/*******************************************/
/* genacsch: Generic routine for computing */
/*   the hyperbolic arccosine.             */
/*******************************************/
static double genacosh(
  double num)
  {
   return(log(num + sqrt(num * num - 1.0)));
  }

/*******************************************/
/* genacsch: Generic routine for computing */
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
/* genacsch: Generic routine for computing */
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


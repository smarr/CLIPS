   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  05/17/06            */
   /*                                                     */
   /*               SYSTEM DEPENDENT MODULE               */
   /*******************************************************/

/*************************************************************/
/* Purpose: Isolation of system dependent routines.          */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*      6.23: Modified GenOpen to check the file length      */
/*            against the system constant FILENAME_MAX.      */
/*                                                           */
/*      6.24: Support for run-time programs directly passing */
/*            the hash tables for initialization.            */
/*                                                           */
/*            Made gensystem functional for Xcode.           */ 
/*                                                           */
/*            Added BeforeOpenFunction and AfterOpenFunction */
/*            hooks.                                         */
/*                                                           */
/*            Added environment parameter to GenClose.       */
/*            Added environment parameter to GenOpen.        */
/*                                                           */
/*            Updated UNIX_V gentime functionality.          */
/*                                                           */
/*            Removed GenOpen check against FILENAME_MAX.    */
/*                                                           */
/*************************************************************/

#define _SYSDEP_SOURCE_

#include "setup.h"

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include <stdlib.h>
#include <time.h>
#include <stdarg.h>

#if   VAX_VMS
#include timeb
#include <descrip.h>
#include <ssdef.h>
#include <stsdef.h>
#include signal
extern int LIB$SPAWN();
#endif

#if MAC_MCW || MAC_XCD
#include <Carbon/Carbon.h> 
#define kTwoPower32 (4294967296.0)      /* 2^32 */
#endif

#if MAC_MCW || MAC_XCD
#include <strings.h>
#endif

#if MAC_MCW || WIN_MCW || MAC_XCD 
#include <unistd.h>
#endif

#if WIN_MVC || WIN_BTC
#define _UNICODE
#define UNICODE 
#include <Windows.h>
#endif

#if WIN_MVC
#include <sys\types.h>
#include <sys\timeb.h>
#include <io.h>
#include <fcntl.h>
#include <limits.h>
#include <process.h>
#include <signal.h>
#endif

#if WIN_BTC
#include <io.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#endif

#if WIN_MCW
#include <io.h>
#include <limits.h>
#endif

#if   UNIX_7 || WIN_GCC
#include <sys/types.h>
#include <sys/timeb.h>
#include <signal.h>
#endif

#if   UNIX_V || LINUX || DARWIN
#include <sys/types.h>
#include <sys/time.h>
#include <sys/times.h>
#include <unistd.h>
#include <signal.h>
#endif

#include "argacces.h"
#include "bmathfun.h"
#include "commline.h"
#include "conscomp.h"
#include "constrnt.h"
#include "constrct.h"
#include "cstrcpsr.h"
#include "emathfun.h"
#include "envrnmnt.h"
#include "filecom.h"
#include "iofun.h"
#include "memalloc.h"
#include "miscfun.h"
#include "multifld.h"
#include "multifun.h"
#include "parsefun.h"
#include "prccode.h"
#include "prdctfun.h"
#include "proflfun.h"
#include "prcdrfun.h"
#include "router.h"
#include "sortfun.h"
#include "strngfun.h"
#include "textpro.h"
#include "utility.h"
#include "watch.h"

#include "sysdep.h"

#if DEFFACTS_CONSTRUCT
#include "dffctdef.h"
#endif

#if DEFRULE_CONSTRUCT
#include "ruledef.h"
#endif

#if DEFGENERIC_CONSTRUCT
#include "genrccom.h"
#endif

#if DEFFUNCTION_CONSTRUCT
#include "dffnxfun.h"
#endif

#if DEFGLOBAL_CONSTRUCT
#include "globldef.h"
#endif

#if DEFTEMPLATE_CONSTRUCT
#include "tmpltdef.h"
#endif

#if OBJECT_SYSTEM
#include "classini.h"
#endif

#include "moduldef.h"

#if EMACS_EDITOR
#include "ed.h"
#endif

#if DEVELOPER
#include "developr.h"
#endif

/***************/
/* DEFINITIONS */
/***************/

#define NO_SWITCH         0
#define BATCH_SWITCH      1
#define BATCH_STAR_SWITCH 2
#define LOAD_SWITCH       3

/********************/
/* ENVIRONMENT DATA */
/********************/

#define SYSTEM_DEPENDENT_DATA 58

struct systemDependentData
  { 
   void (*RedrawScreenFunction)(void *,EXEC_STATUS);
   void (*PauseEnvFunction)(void *,EXEC_STATUS);
   void (*ContinueEnvFunction)(void *,EXEC_STATUS,int);
/*
#if ! WINDOW_INTERFACE
#if WIN_BTC
   void interrupt (*OldCtrlC)(void);
   void interrupt (*OldBreak)(void);
#endif
#if WIN_MVC
   void (interrupt *OldCtrlC)(void);
   void (interrupt *OldBreak)(void);
#endif
#endif
*/
#if WIN_BTC || WIN_MVC
   int BinaryFileHandle;
   unsigned char getcBuffer[7];
   int getcLength;
   int getcPosition;
#endif
#if (! WIN_BTC) && (! WIN_MVC)
   FILE *BinaryFP;
#endif
   int (*BeforeOpenFunction)(void *,EXEC_STATUS);
   int (*AfterOpenFunction)(void *,EXEC_STATUS);
   jmp_buf *jmpBuffer;
  };

#define SystemDependentData(theEnv,execStatus) ((struct systemDependentData *) GetEnvironmentData(theEnv,execStatus,SYSTEM_DEPENDENT_DATA))

/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern void                    UserFunctions(void);
   extern void                    EnvUserFunctions(void *,EXEC_STATUS);

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    InitializeSystemDependentData(void *,EXEC_STATUS);
   static void                    SystemFunctionDefinitions(void *,EXEC_STATUS);
   static void                    InitializeKeywords(void *,EXEC_STATUS);
   static void                    InitializeNonportableFeatures(void *);
#if   (VAX_VMS || UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_BTC || WIN_MVC) && (! WINDOW_INTERFACE)
   static void                    CatchCtrlC(int);
#endif
/*
#if   (WIN_MVC) && (! WINDOW_INTERFACE)
   static void interrupt          CatchCtrlC(void);
   static void                    RestoreInterruptVectors(void);
#endif
*/

/********************************************************/
/* InitializeSystemDependentData: Allocates environment */
/*    data for system dependent routines.               */
/********************************************************/
static void InitializeSystemDependentData(
  void *theEnv,
  EXEC_STATUS)
  {
   AllocateEnvironmentData(theEnv,execStatus,SYSTEM_DEPENDENT_DATA,sizeof(struct systemDependentData),NULL);
  }

/**************************************************/
/* InitializeEnvironment: Performs initialization */
/*   of the KB environment.                       */
/**************************************************/
#if ALLOW_ENVIRONMENT_GLOBALS
globle void InitializeEnvironment()
   {
    if (GetCurrentEnvironment() == NULL)
      { CreateEnvironment(); }
   }
#endif

/*****************************************************/
/* EnvInitializeEnvironment: Performs initialization */
/*   of the KB environment.                          */
/*****************************************************/
globle void EnvInitializeEnvironment(
  void *vtheEnvironment,
  EXEC_STATUS,
  struct symbolHashNode **symbolTable,
  struct floatHashNode **floatTable,
  struct integerHashNode **integerTable,
  struct bitMapHashNode **bitmapTable,
  struct externalAddressHashNode **externalAddressTable)
  {
   struct environmentData *theEnvironment = (struct environmentData *) vtheEnvironment;
   
   /*================================================*/
   /* Don't allow the initialization to occur twice. */
   /*================================================*/

   if (theEnvironment->initialized) return;
     
   /*================================*/
   /* Initialize the memory manager. */
   /*================================*/

   InitializeMemory(theEnvironment, execStatus);

   /*===================================================*/
   /* Initialize environment data for various features. */
   /*===================================================*/
   
   InitializeCommandLineData(theEnvironment, execStatus);
#if CONSTRUCT_COMPILER && (! RUN_TIME)
   InitializeConstructCompilerData(theEnvironment, execStatus);
#endif
   InitializeConstructData(theEnvironment, execStatus);
   InitializeEvaluationData(theEnvironment, execStatus);
   InitializeExternalFunctionData(theEnvironment, execStatus);
   InitializeMultifieldData(theEnvironment, execStatus);
   InitializePrettyPrintData(theEnvironment, execStatus);
   InitializePrintUtilityData(theEnvironment, execStatus);
   InitializeScannerData(theEnvironment, execStatus);
   InitializeSystemDependentData(theEnvironment, execStatus);
   InitializeUserDataData(theEnvironment, execStatus);
   InitializeUtilityData(theEnvironment, execStatus);
#if DEBUGGING_FUNCTIONS
   InitializeWatchData(theEnvironment, execStatus);
#endif
   
   /*===============================================*/
   /* Initialize the hash tables for atomic values. */
   /*===============================================*/

   InitializeAtomTables(theEnvironment,execStatus,symbolTable,floatTable,integerTable,bitmapTable,externalAddressTable);

   /*=========================================*/
   /* Initialize file and string I/O routers. */
   /*=========================================*/

   InitializeDefaultRouters(theEnvironment, execStatus);

   /*=========================================================*/
   /* Initialize some system dependent features such as time. */
   /*=========================================================*/

   InitializeNonportableFeatures(theEnvironment);

   /*=============================================*/
   /* Register system and user defined functions. */
   /*=============================================*/

   SystemFunctionDefinitions(theEnvironment, execStatus);
   UserFunctions();
   EnvUserFunctions(theEnvironment, execStatus);

   /*====================================*/
   /* Initialize the constraint manager. */
   /*====================================*/

   InitializeConstraints(theEnvironment, execStatus);

   /*==========================================*/
   /* Initialize the expression hash table and */
   /* pointers to specific functions.          */
   /*==========================================*/

   InitExpressionData(theEnvironment, execStatus);

   /*===================================*/
   /* Initialize the construct manager. */
   /*===================================*/

#if ! RUN_TIME
   InitializeConstructs(theEnvironment, execStatus);
#endif

   /*=====================================*/
   /* Initialize the defmodule construct. */
   /*=====================================*/

   AllocateDefmoduleGlobals(theEnvironment, execStatus);

   /*===================================*/
   /* Initialize the defrule construct. */
   /*===================================*/

#if DEFRULE_CONSTRUCT
   InitializeDefrules(theEnvironment, execStatus);
#endif

   /*====================================*/
   /* Initialize the deffacts construct. */
   /*====================================*/

#if DEFFACTS_CONSTRUCT
   InitializeDeffacts(theEnvironment, execStatus);
#endif

   /*=====================================================*/
   /* Initialize the defgeneric and defmethod constructs. */
   /*=====================================================*/

#if DEFGENERIC_CONSTRUCT
   SetupGenericFunctions(theEnvironment, execStatus);
#endif

   /*=======================================*/
   /* Initialize the deffunction construct. */
   /*=======================================*/

#if DEFFUNCTION_CONSTRUCT
   SetupDeffunctions(theEnvironment, execStatus);
#endif

   /*=====================================*/
   /* Initialize the defglobal construct. */
   /*=====================================*/

#if DEFGLOBAL_CONSTRUCT
   InitializeDefglobals(theEnvironment, execStatus);
#endif

   /*=======================================*/
   /* Initialize the deftemplate construct. */
   /*=======================================*/

#if DEFTEMPLATE_CONSTRUCT
   InitializeDeftemplates(theEnvironment, execStatus);
#endif

   /*=============================*/
   /* Initialize COOL constructs. */
   /*=============================*/

#if OBJECT_SYSTEM
   SetupObjectSystem(theEnvironment, execStatus);
#endif

   /*=====================================*/
   /* Initialize the defmodule construct. */
   /*=====================================*/

   InitializeDefmodules(theEnvironment, execStatus);

   /*======================================================*/
   /* Register commands and functions for development use. */
   /*======================================================*/

#if DEVELOPER
   DeveloperCommands(theEnvironment, execStatus);
#endif

   /*=========================================*/
   /* Install the special function primitives */
   /* used by procedural code in constructs.  */
   /*=========================================*/

   InstallProcedurePrimitives(theEnvironment, execStatus);

   /*==============================================*/
   /* Install keywords in the symbol table so that */
   /* they are available for command completion.   */
   /*==============================================*/

   InitializeKeywords(theEnvironment, execStatus);

   /*========================*/
   /* Issue a clear command. */
   /*========================*/
   
   EnvClear(theEnvironment, execStatus);

   /*=============================*/
   /* Initialization is complete. */
   /*=============================*/

   theEnvironment->initialized = TRUE;
  }

/******************************************************/
/* SetRedrawFunction: Sets the redraw screen function */
/*   for use with a user interface that may be        */
/*   overwritten by execution of a command.           */
/******************************************************/
globle void SetRedrawFunction(
  void *theEnv,
  EXEC_STATUS,
  void (*theFunction)(void *,EXEC_STATUS))
  {
   SystemDependentData(theEnv,execStatus)->RedrawScreenFunction = theFunction;
  }

/******************************************************/
/* SetPauseEnvFunction: Set the normal state function */
/*   which puts terminal in a normal state.           */
/******************************************************/
globle void SetPauseEnvFunction(
  void *theEnv,
  EXEC_STATUS,
  void (*theFunction)(void *,EXEC_STATUS))
  {
   SystemDependentData(theEnv,execStatus)->PauseEnvFunction = theFunction;
  }

/*********************************************************/
/* SetContinueEnvFunction: Sets the continue environment */
/*   function which returns the terminal to a special    */
/*   screen interface state.                             */
/*********************************************************/
globle void SetContinueEnvFunction(
  void *theEnv,
  EXEC_STATUS,
  void (*theFunction)(void *,EXEC_STATUS,int))
  {
   SystemDependentData(theEnv,execStatus)->ContinueEnvFunction = theFunction;
  }

/*******************************************************/
/* GetRedrawFunction: Gets the redraw screen function. */
/*******************************************************/
globle void (*GetRedrawFunction(void *theEnv,EXEC_STATUS))(void *,EXEC_STATUS)
  {
   return SystemDependentData(theEnv,execStatus)->RedrawScreenFunction;
  }

/*****************************************************/
/* GetPauseEnvFunction: Gets the normal state function. */
/*****************************************************/
globle void (*GetPauseEnvFunction(void *theEnv,EXEC_STATUS))(void *,EXEC_STATUS)
  {
   return SystemDependentData(theEnv,execStatus)->PauseEnvFunction;
  }

/*********************************************/
/* GetContinueEnvFunction: Gets the continue */
/*   environment function.                   */
/*********************************************/
globle void (*GetContinueEnvFunction(void *theEnv,EXEC_STATUS))(void *,EXEC_STATUS,int)
  {
   return SystemDependentData(theEnv,execStatus)->ContinueEnvFunction;
  }

/*************************************************/
/* RerouteStdin: Processes the -f, -f2, and -l   */
/*   options available on machines which support */
/*   argc and arv command line options.          */
/*************************************************/
globle void RerouteStdin(
  void *theEnv,
  EXEC_STATUS,
  int argc,
  char *argv[])
  {
   int i;
   int theSwitch = NO_SWITCH;

   /*======================================*/
   /* If there aren't enough arguments for */
   /* the -f argument, then return.        */
   /*======================================*/

   if (argc < 3)
     { return; }

   /*=====================================*/
   /* If argv was not passed then return. */
   /*=====================================*/

   if (argv == NULL) return;

   /*=============================================*/
   /* Process each of the command line arguments. */
   /*=============================================*/

   for (i = 1 ; i < argc ; i++)
     {
      if (strcmp(argv[i],"-f") == 0) theSwitch = BATCH_SWITCH;
#if ! RUN_TIME
      else if (strcmp(argv[i],"-f2") == 0) theSwitch = BATCH_STAR_SWITCH;
      else if (strcmp(argv[i],"-l") == 0) theSwitch = LOAD_SWITCH;
#endif
      else if (theSwitch == NO_SWITCH)
        {
         PrintErrorID(theEnv,execStatus,"SYSDEP",2,FALSE);
         EnvPrintRouter(theEnv,execStatus,WERROR,"Invalid option\n");
        }

      if (i > (argc-1))
        {
         PrintErrorID(theEnv,execStatus,"SYSDEP",1,FALSE);
         EnvPrintRouter(theEnv,execStatus,WERROR,"No file found for ");

         switch(theSwitch)
           {
            case BATCH_SWITCH:
               EnvPrintRouter(theEnv,execStatus,WERROR,"-f");
               break;

            case BATCH_STAR_SWITCH:
               EnvPrintRouter(theEnv,execStatus,WERROR,"-f2");
               break;

            case LOAD_SWITCH:
               EnvPrintRouter(theEnv,execStatus,WERROR,"-l");
           }

         EnvPrintRouter(theEnv,execStatus,WERROR," option\n");
         return;
        }

      switch(theSwitch)
        {
         case BATCH_SWITCH:
            OpenBatch(theEnv,execStatus,argv[++i],TRUE);
            break;

#if (! RUN_TIME) && (! BLOAD_ONLY)
         case BATCH_STAR_SWITCH:
            EnvBatchStar(theEnv,execStatus,argv[++i]);
            break;

         case LOAD_SWITCH:
            EnvLoad(theEnv,execStatus,argv[++i]);
            break;
#endif
        }
     }
  }

/**************************************************/
/* SystemFunctionDefinitions: Sets up definitions */
/*   of system defined functions.                 */
/**************************************************/
static void SystemFunctionDefinitions(
  void *theEnv,
  EXEC_STATUS)
  {
   ProceduralFunctionDefinitions(theEnv,execStatus);
   MiscFunctionDefinitions(theEnv,execStatus);

#if IO_FUNCTIONS
   IOFunctionDefinitions(theEnv,execStatus);
#endif

   PredicateFunctionDefinitions(theEnv,execStatus);
   BasicMathFunctionDefinitions(theEnv,execStatus);
   FileCommandDefinitions(theEnv,execStatus);
   SortFunctionDefinitions(theEnv,execStatus);

#if DEBUGGING_FUNCTIONS
   WatchFunctionDefinitions(theEnv,execStatus);
#endif

#if MULTIFIELD_FUNCTIONS
   MultifieldFunctionDefinitions(theEnv,execStatus);
#endif

#if STRING_FUNCTIONS
   StringFunctionDefinitions(theEnv,execStatus);
#endif

#if EXTENDED_MATH_FUNCTIONS
   ExtendedMathFunctionDefinitions(theEnv,execStatus);
#endif

#if TEXTPRO_FUNCTIONS || HELP_FUNCTIONS
   HelpFunctionDefinitions(theEnv,execStatus);
#endif

#if EMACS_EDITOR
   EditorFunctionDefinition(theEnv,execStatus);
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
   ConstructsToCCommandDefinition(theEnv,execStatus);
#endif

#if PROFILING_FUNCTIONS
   ConstructProfilingFunctionDefinitions(theEnv,execStatus);
#endif

   ParseFunctionDefinitions(theEnv,execStatus);
  }
  
/*********************************************************/
/* gentime: A function to return a floating point number */
/*   which indicates the present time. Used internally   */
/*   for timing rule firings and debugging.              */
/*********************************************************/
globle double gentime()
  {
#if   MAC_XCD || MAC_MCW
   UnsignedWide result;

   Microseconds(&result);

   return(((((double) result.hi) * kTwoPower32) + result.lo) / 1000000.0);

#elif WIN_MCW
   unsigned long int result;

   result = GetTickCount();

   return((double) result / 1000.0);
/*
#elif   WIN_BTC && (! WINDOW_INTERFACE)
   unsigned long int result;

   result = biostime(0,(long int) 0);

   return((double) result / 18.2);
*/
#elif UNIX_V || DARWIN
#if defined(_POSIX_TIMERS) && (_POSIX_TIMERS > 0)
   struct timespec now;
   clock_gettime(

#if defined(_POSIX_MONOTONIC_CLOCK)
       CLOCK_MONOTONIC,
#else
       CLOCK_REALTIME,
#endif
       &now);
  return (now.tv_nsec / 1000000000.0) + now.tv_sec;
#else
   struct timeval now;
   gettimeofday(&now, 0);
   return (now.tv_usec / 1000000.0) + now.tv_sec;
#endif

#elif LINUX
#if defined(_POSIX_TIMERS) && (_POSIX_TIMERS > 0) && defined(_POSIX_C_SOURCE) && (_POSIX_C_SOURCE >= 199309L)
   struct timespec now;
   clock_gettime(

#if defined(_POSIX_MONOTONIC_CLOCK)
       CLOCK_MONOTONIC,
#else
       CLOCK_REALTIME,
#endif
       &now);
  return (now.tv_nsec / 1000000000.0) + now.tv_sec;
#else
   struct timeval now;
   gettimeofday(&now, 0);
   return (now.tv_usec / 1000000.0) + now.tv_sec;
#endif

#elif UNIX_7
   struct timeval now;
   gettimeofday(&now, 0);
   return (now.tv_usec / 1000000.0) + now.tv_sec;

#else
   return((double) clock() / (double) CLOCKS_PER_SEC);
#endif
  }

/*****************************************************/
/* gensystem: Generic routine for passing a string   */
/*   representing a command to the operating system. */
/*****************************************************/
globle void gensystem(
  void *theEnv,
  EXEC_STATUS)
  {
   char *commandBuffer = NULL;
   size_t bufferPosition = 0;
   size_t bufferMaximum = 0;
   int numa, i;
   DATA_OBJECT tempValue;
   char *theString;

   /*===========================================*/
   /* Check for the corret number of arguments. */
   /*===========================================*/

   if ((numa = EnvArgCountCheck(theEnv,execStatus,"system",AT_LEAST,1)) == -1) return;

   /*============================================================*/
   /* Concatenate the arguments together to form a single string */
   /* containing the command to be sent to the operating system. */
   /*============================================================*/

   for (i = 1 ; i <= numa; i++)
     {
      EnvRtnUnknown(theEnv,execStatus,i,&tempValue);
      if ((GetType(tempValue) != STRING) &&
          (GetType(tempValue) != SYMBOL))
        {
         SetHaltExecution(theEnv,execStatus,TRUE);
         SetEvaluationError(theEnv,execStatus,TRUE);
         ExpectedTypeError2(theEnv,execStatus,"system",i);
         return;
        }

     theString = DOToString(tempValue);

     commandBuffer = AppendToString(theEnv,execStatus,theString,commandBuffer,&bufferPosition,&bufferMaximum);
    }

   if (commandBuffer == NULL) return;

   /*=======================================*/
   /* Execute the operating system command. */
   /*=======================================*/

#if VAX_VMS
   if (SystemDependentData(theEnv,execStatus)->PauseEnvFunction != NULL) (*SystemDependentData(theEnv,execStatus)->PauseEnvFunction)(theEnv,execStatus);
   VMSSystem(commandBuffer);
   putchar('\n');
   if (SystemDependentData(theEnv,execStatus)->ContinueEnvFunction != NULL) (*SystemDependentData(theEnv,execStatus)->ContinueEnvFunction)(theEnv,execStatus,1);
   if (SystemDependentData(theEnv,execStatus)->RedrawScreenFunction != NULL) (*SystemDependentData(theEnv,execStatus)->RedrawScreenFunction)(theEnv,execStatus);
#endif

#if   UNIX_7 || UNIX_V || LINUX || DARWIN || WIN_MVC || WIN_BTC || WIN_MCW || WIN_GCC || MAC_XCD
   if (SystemDependentData(theEnv,execStatus)->PauseEnvFunction != NULL) (*SystemDependentData(theEnv,execStatus)->PauseEnvFunction)(theEnv,execStatus);
   system(commandBuffer);
   if (SystemDependentData(theEnv,execStatus)->ContinueEnvFunction != NULL) (*SystemDependentData(theEnv,execStatus)->ContinueEnvFunction)(theEnv,execStatus,1);
   if (SystemDependentData(theEnv,execStatus)->RedrawScreenFunction != NULL) (*SystemDependentData(theEnv,execStatus)->RedrawScreenFunction)(theEnv,execStatus);
#else

#if ! VAX_VMS
   EnvPrintRouter(theEnv,execStatus,WDIALOG,
            "System function not fully defined for this system.\n");
#endif

#endif

   /*==================================================*/
   /* Return the string buffer containing the command. */
   /*==================================================*/

   rm(theEnv,execStatus,commandBuffer,bufferMaximum);

   return;
  }

#if   VAX_VMS
/*************************************************/
/* VMSSystem: Implements system command for VMS. */
/*************************************************/
globle void VMSSystem(
  char *cmd)
  {
   long status, complcode;
   struct dsc$descriptor_s cmd_desc;

   cmd_desc.dsc$w_length = strlen(cmd);
   cmd_desc.dsc$a_pointer = cmd;
   cmd_desc.dsc$b_class = DSC$K_CLASS_S;
   cmd_desc.dsc$b_dtype = DSC$K_DTYPE_T;

   status = LIB$SPAWN(&cmd_desc,0,0,0,0,0,&complcode,0,0,0);
  }

#endif

/*******************************************/
/* gengetchar: Generic routine for getting */
/*    a character from stdin.              */
/*******************************************/
globle int gengetchar(
  void *theEnv,
  EXEC_STATUS)
  {
#if WIN_BTC || WIN_MVC
   if (SystemDependentData(theEnv,execStatus)->getcLength ==
       SystemDependentData(theEnv,execStatus)->getcPosition)
     {
      TCHAR tBuffer = 0;
      DWORD count = 0;
      WCHAR wBuffer = 0;

      ReadConsole(GetStdHandle(STD_INPUT_HANDLE),&tBuffer,1,&count,NULL);
      
      wBuffer = tBuffer;
      
      SystemDependentData(theEnv,execStatus)->getcLength = 
         WideCharToMultiByte(CP_UTF8,0,&wBuffer,1,
                             (char *) SystemDependentData(theEnv,execStatus)->getcBuffer,
                             7,NULL,NULL);
                             
      SystemDependentData(theEnv,execStatus)->getcPosition = 0;
     }
     
   return SystemDependentData(theEnv,execStatus)->getcBuffer[SystemDependentData(theEnv,execStatus)->getcPosition++];
#else
   return(getc(stdin));
#endif
  }

/***********************************************/
/* genungetchar: Generic routine for ungetting */
/*    a character from stdin.                  */
/***********************************************/
globle int genungetchar(
  void *theEnv,
  EXEC_STATUS,
  int theChar)
  {
#if WIN_BTC || WIN_MVC
   if (SystemDependentData(theEnv,execStatus)->getcPosition > 0)
     { 
      SystemDependentData(theEnv,execStatus)->getcPosition--;
      return theChar;
     }
   else
     { return EOF; }
#else
   return(ungetc(theChar,stdin));
#endif
  }

/****************************************************/
/* genprintfile: Generic routine for print a single */
/*   character string to a file (including stdout). */
/****************************************************/
globle void genprintfile(
  void *theEnv,
  EXEC_STATUS,
  FILE *fptr,
  char *str)
  {
   if (fptr != stdout)
     {
      fprintf(fptr,"%s",str);
      fflush(fptr);
     }
   else
     {
#if WIN_MVC
/*
      int rv;
      wchar_t *wbuffer;
      size_t len = strlen(str);

      wbuffer = genalloc(theEnv,execStatus,sizeof(wchar_t) * (len + 1));
      rv = MultiByteToWideChar(CP_UTF8,MB_ERR_INVALID_CHARS,str,-1,wbuffer,len+1);
      
      fwprintf(fptr,L"%ls",wbuffer);
      fflush(fptr);
      genfree(theEnv,execStatus,wbuffer,sizeof(wchar_t) * (len + 1));
*/
      fprintf(fptr,"%s",str);
      fflush(fptr);
#else
      fprintf(fptr,"%s",str);
      fflush(fptr);
#endif
     }
  }
  
/***********************************************************/
/* InitializeNonportableFeatures: Initializes non-portable */
/*   features. Currently, the only non-portable feature    */
/*   requiring initialization is the interrupt handler     */
/*   which allows execution to be halted.                  */
/***********************************************************/
#if WIN_BTC
#pragma argsused
#endif
static void InitializeNonportableFeatures(
  void *theEnv)
  {
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv)
#endif
#if ! WINDOW_INTERFACE

#if VAX_VMS || UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_BTC || WIN_MVC
   signal(SIGINT,CatchCtrlC);
#endif

/*
#if WIN_BTC
   SystemDependentData(theEnv,execStatus)->OldCtrlC = getvect(0x23);
   SystemDependentData(theEnv,execStatus)->OldBreak = getvect(0x1b);
   setvect(0x23,CatchCtrlC);
   setvect(0x1b,CatchCtrlC);
   atexit(RestoreInterruptVectors);
#endif
*/
/*
#if WIN_MVC
   SystemDependentData(theEnv,execStatus)->OldCtrlC = _dos_getvect(0x23);
   SystemDependentData(theEnv,execStatus)->OldBreak = _dos_getvect(0x1b);
   _dos_setvect(0x23,CatchCtrlC);
   _dos_setvect(0x1b,CatchCtrlC);
   atexit(RestoreInterruptVectors);
#endif
*/
#endif
  }

/*************************************************************/
/* Functions For Handling Control C Interrupt: The following */
/*   functions handle interrupt processing for several       */
/*   machines. For the Macintosh control-c is not handle,    */
/*   but a function is provided to call periodically which   */
/*   calls SystemTask (allowing periodic tasks to be handled */
/*   by the operating system).                               */
/*************************************************************/

#if ! WINDOW_INTERFACE

#if   VAX_VMS || UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_BTC || WIN_MVC || DARWIN
/**********************************************/
/* CatchCtrlC: VMS and UNIX specific function */
/*   to allow control-c interrupts.           */
/**********************************************/
#if WIN_BTC
#pragma argsused
#endif
static void CatchCtrlC(
  int sgnl)
  {
#if ALLOW_ENVIRONMENT_GLOBALS
   SetHaltExecution(GetCurrentEnvironment(),GetCurrentExecutionState(),TRUE);
   CloseAllBatchSources(GetCurrentEnvironment(),GetCurrentExecutionState());
#endif
   signal(SIGINT,CatchCtrlC);
  }
#endif

#if   WIN_MVC
/******************************************************/
/* CatchCtrlC: IBM Microsoft C and Borland Turbo C    */
/*   specific function to allow control-c interrupts. */
/******************************************************/
/*
static void interrupt CatchCtrlC()
  {
#if ALLOW_ENVIRONMENT_GLOBALS
   SetHaltExecution(GetCurrentEnvironment(),GetCurrentExecutionState(),TRUE);
   CloseAllBatchSources(GetCurrentEnvironment(),GetCurrentExecutionState());
#endif
  }
*/
/**************************************************************/
/* RestoreInterruptVectors: IBM Microsoft C and Borland Turbo */
/*   C specific function for restoring interrupt vectors.     */
/**************************************************************/
/*
static void RestoreInterruptVectors()
  {
#if ALLOW_ENVIRONMENT_GLOBALS
   void *theEnv;
   
   theEnv = GetCurrentEnvironment();

   _dos_setvect(0x23,SystemDependentData(theEnv,execStatus)->OldCtrlC);
   _dos_setvect(0x1b,SystemDependentData(theEnv,execStatus)->OldBreak);
#endif
  }
*/
#endif

#endif

/**************************************/
/* genexit:  A generic exit function. */
/**************************************/
globle void genexit(
  void *theEnv,
  EXEC_STATUS,
  int num)
  {
   if (SystemDependentData(theEnv,execStatus)->jmpBuffer != NULL)
     { longjmp(*SystemDependentData(theEnv,execStatus)->jmpBuffer,1); }
     
   exit(num);
  }

/**************************************/
/* SetJmpBuffer: */
/**************************************/
globle void SetJmpBuffer(
  void *theEnv,
  EXEC_STATUS,
  jmp_buf *theJmpBuffer)
  {
   SystemDependentData(theEnv,execStatus)->jmpBuffer = theJmpBuffer;
  }
  
/******************************************/
/* genstrcpy: Generic genstrcpy function. */
/******************************************/
char *genstrcpy(
  char *dest,
  const char *src)
  {
   return strcpy(dest,src);
  }

/********************************************/
/* genstrncpy: Generic genstrncpy function. */
/********************************************/
char *genstrncpy(
  char *dest,
  const char *src,
  size_t n)
  {
   return strncpy(dest,src,n);
  }
  
/******************************************/
/* genstrcat: Generic genstrcat function. */
/******************************************/
char *genstrcat(
  char *dest,
  const char *src)
  {
   return strcat(dest,src);
  }

/********************************************/
/* genstrncat: Generic genstrncat function. */
/********************************************/
char *genstrncat(
  char *dest,
  const char *src,
  size_t n)
  {
   return strncat(dest,src,n);
  }
  
/*****************************************/
/* gensprintf: Generic sprintf function. */
/*****************************************/
int gensprintf(
  char *buffer,
  const char *restrictStr,
  ...)
  {
   va_list args;
   int rv;
   
   va_start(args,restrictStr);
   
   rv = vsprintf(buffer,restrictStr,args);
   
   va_end(args);
   
   return rv;
  }
  
/******************************************************/
/* genrand: Generic random number generator function. */
/******************************************************/
int genrand()
  {
   return(rand());
  }
  
/**********************************************************************/
/* genseed: Generic function for seeding the random number generator. */
/**********************************************************************/
globle void genseed(
  int seed)
  {
   srand((unsigned) seed);
  }

/*********************************************/
/* gengetcwd: Generic function for returning */
/*   the current directory.                  */
/*********************************************/
#if WIN_BTC
#pragma argsused
#endif
globle char *gengetcwd(
  char *buffer,
  int buflength)
  {
#if MAC_MCW || WIN_MCW || MAC_XCD
   return(getcwd(buffer,buflength));
#endif

   if (buffer != NULL)
     { buffer[0] = 0; }
   return(buffer);
  }

/****************************************************/
/* genremove: Generic function for removing a file. */
/****************************************************/
globle int genremove(
  char *fileName)
  {
   if (remove(fileName)) return(FALSE);

   return(TRUE);
  }

/****************************************************/
/* genrename: Generic function for renaming a file. */
/****************************************************/
globle int genrename(
  char *oldFileName,
  char *newFileName)
  {
   if (rename(oldFileName,newFileName)) return(FALSE);

   return(TRUE);
  }

/**************************************/
/* EnvSetBeforeOpenFunction: Sets the */
/*  value of BeforeOpenFunction.      */
/**************************************/
globle int (*EnvSetBeforeOpenFunction(void *theEnv,EXEC_STATUS,
                                      int (*theFunction)(void *,EXEC_STATUS)))(void *,EXEC_STATUS)
  {
   int (*tempFunction)(void *,EXEC_STATUS);

   tempFunction = SystemDependentData(theEnv,execStatus)->BeforeOpenFunction;
   SystemDependentData(theEnv,execStatus)->BeforeOpenFunction = theFunction;
   return(tempFunction);
  }

/*************************************/
/* EnvSetAfterOpenFunction: Sets the */
/*  value of AfterOpenFunction.      */
/*************************************/
globle int (*EnvSetAfterOpenFunction(void *theEnv,EXEC_STATUS,
                                     int (*theFunction)(void *,EXEC_STATUS)))(void *,EXEC_STATUS)
  {
   int (*tempFunction)(void *,EXEC_STATUS);

   tempFunction = SystemDependentData(theEnv,execStatus)->AfterOpenFunction;
   SystemDependentData(theEnv,execStatus)->AfterOpenFunction = theFunction;
   return(tempFunction);
  }

/*********************************************/
/* GenOpen: Trap routine for opening a file. */
/*********************************************/
globle FILE *GenOpen(
  void *theEnv,
  EXEC_STATUS,
  char *fileName,
  char *accessType)
  {
   FILE *theFile;
   
   if (SystemDependentData(theEnv,execStatus)->BeforeOpenFunction != NULL)
     { (*SystemDependentData(theEnv,execStatus)->BeforeOpenFunction)(theEnv,execStatus); }

#if WIN_MVC
#if _MSC_VER >= 1400
   fopen_s(&theFile,fileName,accessType);
#else
   theFile = fopen(fileName,accessType);
#endif
#else
   theFile = fopen(fileName,accessType);
#endif
   
   if (SystemDependentData(theEnv,execStatus)->AfterOpenFunction != NULL)
     { (*SystemDependentData(theEnv,execStatus)->AfterOpenFunction)(theEnv,execStatus); }
     
   return theFile;
  }
  
/**********************************************/
/* GenClose: Trap routine for closing a file. */
/**********************************************/
globle int GenClose(
  void *theEnv,
  EXEC_STATUS,
  FILE *theFile)
  {
   int rv;
   
   if (SystemDependentData(theEnv,execStatus)->BeforeOpenFunction != NULL)
     { (*SystemDependentData(theEnv,execStatus)->BeforeOpenFunction)(theEnv,execStatus); }

   rv = fclose(theFile);

   if (SystemDependentData(theEnv,execStatus)->AfterOpenFunction != NULL)
     { (*SystemDependentData(theEnv,execStatus)->AfterOpenFunction)(theEnv,execStatus); }
   
   return rv;
  }
  
/************************************************************/
/* GenOpenReadBinary: Generic and machine specific code for */
/*   opening a file for binary access. Only one file may be */
/*   open at a time when using this function since the file */
/*   pointer is stored in a global variable.                */
/************************************************************/
globle int GenOpenReadBinary(
  void *theEnv,
  EXEC_STATUS,
  char *funcName,
  char *fileName)
  {
   if (SystemDependentData(theEnv,execStatus)->BeforeOpenFunction != NULL)
     { (*SystemDependentData(theEnv,execStatus)->BeforeOpenFunction)(theEnv,execStatus); }

#if WIN_BTC || WIN_MVC

#if WIN_MVC
   SystemDependentData(theEnv,execStatus)->BinaryFileHandle = _open(fileName,O_RDONLY | O_BINARY);
#else
   SystemDependentData(theEnv,execStatus)->BinaryFileHandle = open(fileName,O_RDONLY | O_BINARY);
#endif
   if (SystemDependentData(theEnv,execStatus)->BinaryFileHandle == -1)
     {
      if (SystemDependentData(theEnv,execStatus)->AfterOpenFunction != NULL)
        { (*SystemDependentData(theEnv,execStatus)->AfterOpenFunction)(theEnv,execStatus); }
      OpenErrorMessage(theEnv,execStatus,funcName,fileName);
      return(FALSE);
     }
#endif

#if (! WIN_BTC) && (! WIN_MVC)

   if ((SystemDependentData(theEnv,execStatus)->BinaryFP = fopen(fileName,"rb")) == NULL)
     {
      if (SystemDependentData(theEnv,execStatus)->AfterOpenFunction != NULL)
        { (*SystemDependentData(theEnv,execStatus)->AfterOpenFunction)(theEnv,execStatus); }
      OpenErrorMessage(theEnv,execStatus,funcName,fileName);
      return(FALSE);
     }
#endif

   if (SystemDependentData(theEnv,execStatus)->AfterOpenFunction != NULL)
     { (*SystemDependentData(theEnv,execStatus)->AfterOpenFunction)(theEnv,execStatus); }

   return(TRUE);
  }

/***********************************************/
/* GenReadBinary: Generic and machine specific */
/*   code for reading from a file.             */
/***********************************************/
globle void GenReadBinary(
  void *theEnv,
  EXEC_STATUS,
  void *dataPtr,
  size_t size)
  {
#if WIN_MVC
   char *tempPtr;

   tempPtr = (char *) dataPtr;
   while (size > INT_MAX)
     {
      _read(SystemDependentData(theEnv,execStatus)->BinaryFileHandle,tempPtr,INT_MAX);
      size -= INT_MAX;
      tempPtr = tempPtr + INT_MAX;
     }

   if (size > 0) 
     { _read(SystemDependentData(theEnv,execStatus)->BinaryFileHandle,tempPtr,(unsigned int) size); }
#endif

#if WIN_BTC
   char *tempPtr;

   tempPtr = (char *) dataPtr;
   while (size > INT_MAX)
     {
      read(SystemDependentData(theEnv,execStatus)->BinaryFileHandle,tempPtr,INT_MAX);
      size -= INT_MAX;
      tempPtr = tempPtr + INT_MAX;
     }

   if (size > 0) 
     { read(SystemDependentData(theEnv,execStatus)->BinaryFileHandle,tempPtr,(STD_SIZE) size); }
#endif

#if (! WIN_BTC) && (! WIN_MVC)
   fread(dataPtr,size,1,SystemDependentData(theEnv,execStatus)->BinaryFP); 
#endif
  }

/***************************************************/
/* GetSeekCurBinary:  Generic and machine specific */
/*   code for seeking a position in a file.        */
/***************************************************/
globle void GetSeekCurBinary(
  void *theEnv,
  EXEC_STATUS,
  long offset)
  {
#if WIN_BTC
   lseek(SystemDependentData(theEnv,execStatus)->BinaryFileHandle,offset,SEEK_CUR);
#endif

#if WIN_MVC
   _lseek(SystemDependentData(theEnv,execStatus)->BinaryFileHandle,offset,SEEK_CUR);
#endif

#if (! WIN_BTC) && (! WIN_MVC)
   fseek(SystemDependentData(theEnv,execStatus)->BinaryFP,offset,SEEK_CUR);
#endif
  }
  
/***************************************************/
/* GetSeekSetBinary:  Generic and machine specific */
/*   code for seeking a position in a file.        */
/***************************************************/
globle void GetSeekSetBinary(
  void *theEnv,
  EXEC_STATUS,
  long offset)
  {
#if WIN_BTC
   lseek(SystemDependentData(theEnv,execStatus)->BinaryFileHandle,offset,SEEK_SET);
#endif

#if WIN_MVC
   _lseek(SystemDependentData(theEnv,execStatus)->BinaryFileHandle,offset,SEEK_SET);
#endif

#if (! WIN_BTC) && (! WIN_MVC)
   fseek(SystemDependentData(theEnv,execStatus)->BinaryFP,offset,SEEK_SET);
#endif
  }

/************************************************/
/* GenTellBinary:  Generic and machine specific */
/*   code for telling a position in a file.     */
/************************************************/
globle void GenTellBinary(
  void *theEnv,
  EXEC_STATUS,
  long *offset)
  {
#if WIN_BTC
   *offset = lseek(SystemDependentData(theEnv,execStatus)->BinaryFileHandle,0,SEEK_CUR);
#endif

#if WIN_MVC
   *offset = _lseek(SystemDependentData(theEnv,execStatus)->BinaryFileHandle,0,SEEK_CUR);
#endif

#if (! WIN_BTC) && (! WIN_MVC)
   *offset = ftell(SystemDependentData(theEnv,execStatus)->BinaryFP);
#endif
  }

/****************************************/
/* GenCloseBinary:  Generic and machine */
/*   specific code for closing a file.  */
/****************************************/
globle void GenCloseBinary(
  void *theEnv,
  EXEC_STATUS)
  {
   if (SystemDependentData(theEnv,execStatus)->BeforeOpenFunction != NULL)
     { (*SystemDependentData(theEnv,execStatus)->BeforeOpenFunction)(theEnv,execStatus); }

#if WIN_BTC
   close(SystemDependentData(theEnv,execStatus)->BinaryFileHandle);
#endif

#if WIN_MVC
   _close(SystemDependentData(theEnv,execStatus)->BinaryFileHandle);
#endif

#if (! WIN_BTC) && (! WIN_MVC)
   fclose(SystemDependentData(theEnv,execStatus)->BinaryFP);
#endif

   if (SystemDependentData(theEnv,execStatus)->AfterOpenFunction != NULL)
     { (*SystemDependentData(theEnv,execStatus)->AfterOpenFunction)(theEnv,execStatus); }
  }
  
/***********************************************/
/* GenWrite: Generic routine for writing to a  */
/*   file. No machine specific code as of yet. */
/***********************************************/
globle void GenWrite(
  void *dataPtr,
  size_t size,
  FILE *fp)
  {
   if (size == 0) return;
#if UNIX_7
   fwrite(dataPtr,size,1,fp);
#else
   fwrite(dataPtr,size,1,fp);
#endif
  }

/*********************************************/
/* InitializeKeywords: Adds key words to the */
/*   symbol table so that they are available */
/*   for command completion.                 */
/*********************************************/
#if WIN_BTC && (RUN_TIME || (! WINDOW_INTERFACE))
#pragma argsused
#endif
static void InitializeKeywords(
  void *theEnv,
  EXEC_STATUS)
  {
#if (! RUN_TIME) && WINDOW_INTERFACE
   void *ts;

   /*====================*/
   /* construct keywords */
   /*====================*/

   ts = EnvAddSymbol(theEnv,execStatus,"defrule");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"defglobal");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"deftemplate");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"deffacts");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"deffunction");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"defmethod");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"defgeneric");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"defclass");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"defmessage-handler");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"definstances");
   IncrementSymbolCount(ts);

   /*=======================*/
   /* set-strategy keywords */
   /*=======================*/

   ts = EnvAddSymbol(theEnv,execStatus,"depth");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"breadth");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"lex");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"mea");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"simplicity");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"complexity");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"random");
   IncrementSymbolCount(ts);

   /*==================================*/
   /* set-salience-evaluation keywords */
   /*==================================*/

   ts = EnvAddSymbol(theEnv,execStatus,"when-defined");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"when-activated");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"every-cycle");
   IncrementSymbolCount(ts);

   /*======================*/
   /* deftemplate keywords */
   /*======================*/

   ts = EnvAddSymbol(theEnv,execStatus,"field");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"multifield");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"default");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"type");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"allowed-symbols");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"allowed-strings");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"allowed-numbers");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"allowed-integers");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"allowed-floats");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"allowed-values");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"min-number-of-elements");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"max-number-of-elements");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"NONE");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"VARIABLE");
   IncrementSymbolCount(ts);

   /*==================*/
   /* defrule keywords */
   /*==================*/

   ts = EnvAddSymbol(theEnv,execStatus,"declare");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"salience");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"test");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"or");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"and");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"not");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"logical");
   IncrementSymbolCount(ts);

   /*===============*/
   /* COOL keywords */
   /*===============*/

   ts = EnvAddSymbol(theEnv,execStatus,"is-a");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"role");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"abstract");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"concrete");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"pattern-match");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"reactive");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"non-reactive");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"slot");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"field");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"multiple");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"single");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"storage");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"shared");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"local");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"access");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"read");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"write");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"read-only");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"read-write");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"initialize-only");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"propagation");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"inherit");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"no-inherit");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"source");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"composite");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"exclusive");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"allowed-lexemes");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"allowed-instances");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"around");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"before");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"primary");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"after");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"of");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"self");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"visibility");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"override-message");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"private");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"public");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"create-accessor");
   IncrementSymbolCount(ts);

   /*================*/
   /* watch keywords */
   /*================*/

   ts = EnvAddSymbol(theEnv,execStatus,"compilations");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"deffunctions");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"globals");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"rules");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"activations");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"statistics");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"facts");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"generic-functions");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"methods");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"instances");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"slots");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"messages");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"message-handlers");
   IncrementSymbolCount(ts);
   ts = EnvAddSymbol(theEnv,execStatus,"focus");
   IncrementSymbolCount(ts);
#else
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv,execStatus)
#endif
#endif
  }

#if WIN_BTC
/*********************************************/
/* strtoll: Convert string to long long int. */
/*    Note supported by Turbo C++ 2006.      */
/*********************************************/
__int64 _RTLENTRY _EXPFUNC strtoll(
  const char * str,
  char**endptr,
  int base)
  // convert string to long long int
  {
   if (endptr != NULL)
	 *endptr = (char*)str + (base == 10 ? strspn(str, "0123456789"): 0);
   return(_atoi64(str));
  }

/*******************************************/
/* llabs: absolute value of long long int. */
/*    Note supported by Turbo C++ 2006.    */
/*******************************************/
__int64 _RTLENTRY _EXPFUNC llabs(
  __int64 val)
  {
   if (val >=0) return(val);
   else	return(-val);
  }

#endif

   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
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
/*      Brian L. Donnell                                     */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Who               |     Date    | Description             */
/* ------------------+-------------+------------------------ */
/* M.Giordano        | 23-Mar-2000 | Mods made for TLS       */
/*************************************************************/

#define _SYSDEP_SOURCE_

#include "setup.h"

#include <stdio.h>
#define _STDIO_INCLUDED_
#include <string.h>

#include <stdlib.h>
#include <time.h>

#if   VAX_VMS
#include timeb
#include <descrip.h>
#include <ssdef.h>
#include <stsdef.h>
#include signal
extern int LIB$SPAWN();
#endif

#if MAC
#include <Events.h>
#include <Menus.h>
#include <Devices.h>
#include <Types.h>
#include <Files.h>
#include <Timer.h>
#define kTwoPower32 (4294967296.0)      /* 2^32 */
#if MAC_MPW || MAC_MCW
#include <strings.h>
#else
#include <pascal.h>
#endif
#endif

#if MAC_MCW
#include <unistd.h>
#endif

#if IBM_ICB
#include <i32.h>
#include <stk.h>
#include <sys\types.h>
#include <sys\timeb.h>
#include <io.h>
#include <fcntl.h>
#include <limits.h>
#include <process.h>
#endif

#if   IBM_MSC
#include <sys\types.h>
#include <sys\timeb.h>
#include <io.h>
#include <fcntl.h>
#include <limits.h>
#include <process.h>
#endif

#if   IBM_TBC
#include <bios.h>
#include <io.h>
#include <fcntl.h>
#include <limits.h>
#if WINDOW_INTERFACE
#undef BOOLEAN
#undef VOID
#include <windows.h>
#undef BOOLEAN
#define BOOLEAN int
#endif
#endif

#if IBM_MCW
#include <io.h>
#include <limits.h>
#endif

#if   IBM_ZTC || IBM_SC
#include <time.h>
#include <controlc.h>
#include <io.h>
#include <fcntl.h>
#include <limits.h>
#include <process.h>
#endif

#if   UNIX_7 || IBM_GCC
#include <sys/types.h>
#include <sys/timeb.h>
#include <signal.h>
#endif

#if   UNIX_V
#include <sys/types.h>
#include <sys/times.h>
#include <signal.h>
#endif

#include "argacces.h"
#include "bmathfun.h"
#include "conscomp.h"
#include "constrnt.h"
#include "constrct.h"
#include "cstrcpsr.h"
#include "emathfun.h"
#include "filecom.h"
#include "iofun.h"
#include "memalloc.h"
#include "miscfun.h"
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
#include "extobj.h"
#endif

#include "moduldef.h"

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

/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern void                    UserFunctions(void);

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if ! RUN_TIME
   static void                    SystemFunctionDefinitions(void);
#endif
   static void                    InitializeKeywords(void);
   static void                    InitializeNonportableFeatures(void);
#if   (VAX_VMS || UNIX_V || UNIX_7 || IBM_GCC) && (! WINDOW_INTERFACE)
   static void                    CatchCtrlC(int);
#endif
#if   (IBM_TBC || IBM_MSC) && (! WINDOW_INTERFACE)
   static void interrupt          CatchCtrlC(void);
   static void                    RestoreInterruptVectors(void);
#endif
#if   IBM_ICB && (! WINDOW_INTERFACE)
#pragma interrupt (CatchCtrlC)
   static void                    CatchCtrlC(void);
   static void                    RestoreInterruptVectors(void);
#endif
#if   (IBM_ZTC || IBM_SC) && (! WINDOW_INTERFACE)
   static void _cdecl             CatchCtrlC(void);
#endif
#if MAC
   static void                    CallSystemTask(void);
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

#if ! WINDOW_INTERFACE
#if IBM_TBC
   static void interrupt  (*OldCtrlC)(void);
   static void interrupt  (*OldBreak)(void);
#endif
#if IBM_MSC
   static void  (interrupt *OldCtrlC)(void);
   static void  (interrupt *OldBreak)(void);
#endif
#if   IBM_ICB
#pragma interrupt (OldCtrlC,OldBreak)
   void         (*OldCtrlC)(void);
   void         (*OldBreak)(void);
#endif
#endif

#if  MAC
   Thread static short int        BinaryRefNum;
#endif

#if IBM_TBC || IBM_MSC || IBM_ICB || IBM_MCW /* || IBM_ZTC */
   Thread static int              BinaryFileHandle;
#endif

#if (! MAC) && (! IBM_TBC) && (! IBM_MSC) && (! IBM_ICB) && (! IBM_MCW) /* && (! IBM_ZTC) */
   Thread static FILE            *BinaryFP;
#endif

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   Thread globle void             (*RedrawScreenFunction)(void) = NULL;
   Thread globle void             (*PauseEnvFunction)(void) = NULL;
   Thread globle void             (*ContinueEnvFunction)(int) = NULL;

/**************************************************/
/* InitializeEnvironment: Performs initialization */
/*   of the KB environment.                       */
/**************************************************/
globle void InitializeEnvironment()
  {
   Thread static BOOLEAN alreadyInitialized = FALSE;

   /*================================================*/
   /* Don't allow the initialization to occur twice. */
   /*================================================*/

   if (alreadyInitialized) return;

   /*================================*/
   /* Initialize the memory manager. */
   /*================================*/

   InitializeMemory();

   /*===============================================*/
   /* Initialize the hash tables for atomic values. */
   /*===============================================*/

#if ! RUN_TIME
   InitializeAtomTables();
#endif

   /*=========================================*/
   /* Initialize file and string I/O routers. */
   /*=========================================*/

   InitializeDefaultRouters();

   /*=========================================================*/
   /* Initialize some system dependent features such as time. */
   /*=========================================================*/

   InitializeNonportableFeatures();

   /*=============================================*/
   /* Register system and user defined functions. */
   /*=============================================*/

#if ! RUN_TIME
   SystemFunctionDefinitions();
#ifndef _USRDLL
   UserFunctions();
#endif
#endif

   /*====================================*/
   /* Initialize the constraint manager. */
   /*====================================*/

   InitializeConstraints();

   /*==========================================*/
   /* Initialize the expression hash table and */
   /* pointers to specific functions.          */
   /*==========================================*/

#if ! RUN_TIME
   InitExpressionData();
#endif

   /*===================================*/
   /* Initialize the construct manager. */
   /*===================================*/

#if ! RUN_TIME
   InitializeConstructs();
#endif

   /*===================================*/
   /* Initialize the defrule construct. */
   /*===================================*/

#if DEFRULE_CONSTRUCT
   InitializeDefrules();
#endif

   /*====================================*/
   /* Initialize the deffacts construct. */
   /*====================================*/

#if DEFFACTS_CONSTRUCT
   InitializeDeffacts();
#endif

   /*=====================================================*/
   /* Initialize the defgeneric and defmethod constructs. */
   /*=====================================================*/

#if DEFGENERIC_CONSTRUCT
   SetupGenericFunctions();
#endif

   /*=======================================*/
   /* Initialize the deffunction construct. */
   /*=======================================*/

#if DEFFUNCTION_CONSTRUCT
   SetupDeffunctions();
#endif

   /*=====================================*/
   /* Initialize the defglobal construct. */
   /*=====================================*/

#if DEFGLOBAL_CONSTRUCT
   InitializeDefglobals();
#endif

   /*=======================================*/
   /* Initialize the deftemplate construct. */
   /*=======================================*/

#if DEFTEMPLATE_CONSTRUCT
   InitializeDeftemplates();
#endif

   /*=============================*/
   /* Initialize COOL constructs. */
   /*=============================*/

#if OBJECT_SYSTEM
   SetupObjectSystem();
#endif

   /*=====================================*/
   /* Initialize the defmodule construct. */
   /*=====================================*/

   InitializeDefmodules();

   /*======================================================*/
   /* Register commands and functions for development use. */
   /*======================================================*/

#if DEVELOPER
   DeveloperCommands();
#endif

   /*=========================================*/
   /* Install the special function primitives */
   /* used by procedural code in constructs.  */
   /*=========================================*/

   InstallProcedurePrimitives();

   /*==============================================*/
   /* Install keywords in the symbol table so that */
   /* they are available for command completion.   */
   /*==============================================*/

   InitializeKeywords();

   /*========================*/
   /* Issue a clear command. */
   /*========================*/

   Clear();

   /*=============================*/
   /* Initialization is complete. */
   /*=============================*/

   alreadyInitialized = TRUE;
  }

/******************************************************/
/* SetRedrawFunction: Sets the redraw screen function */
/*   for use with a user interface that may be        */
/*   overwritten by execution of a command.           */
/******************************************************/
globle void SetRedrawFunction(
  void (*theFunction)(void))
  {
   RedrawScreenFunction = theFunction;
  }

/******************************************************/
/* SetPauseEnvFunction: Set the normal state function */
/*   which puts terminal in a normal state.           */
/******************************************************/
globle void SetPauseEnvFunction(
  void (*theFunction)(void))
  {
   PauseEnvFunction = theFunction;
  }

/*********************************************************/
/* SetContinueEnvFunction: Sets the continue environment */
/*   function which returns the terminal to a special    */
/*   screen interface state.                             */
/*********************************************************/
globle void SetContinueEnvFunction(
   void (*theFunction)(int))
  {
   ContinueEnvFunction = theFunction;
  }

/*************************************************/
/* RerouteStdin: Processes the -f, -f2, and -l   */
/*   options available on machines which support */
/*   argc and arv command line options.          */
/*************************************************/
globle void RerouteStdin(
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
         PrintErrorID("SYSDEP",2,FALSE);
         PrintRouter(WERROR,"Invalid option\n");
        }

      if (i > (argc-1))
        {
         PrintErrorID("SYSDEP",1,FALSE);
         PrintRouter(WERROR,"No file found for ");

         switch(theSwitch)
           {
            case BATCH_SWITCH:
               PrintRouter(WERROR,"-f");
               break;

            case BATCH_STAR_SWITCH:
               PrintRouter(WERROR,"-f2");
               break;

            case LOAD_SWITCH:
               PrintRouter(WERROR,"-l");
           }

         PrintRouter(WERROR," option\n");
         return;
        }

      switch(theSwitch)
        {
         case BATCH_SWITCH:
            OpenBatch(argv[++i],TRUE);
            break;

#if ! RUN_TIME
         case BATCH_STAR_SWITCH:
            BatchStar(argv[++i]);
            break;

         case LOAD_SWITCH:
            Load(argv[++i]);
            break;
#endif
        }
     }
  }

#if ! RUN_TIME
/**************************************************/
/* SystemFunctionDefinitions: Sets up definitions */
/*   of system defined functions.                 */
/**************************************************/
static void SystemFunctionDefinitions()
  {
   ProceduralFunctionDefinitions();
   MiscFunctionDefinitions();
   IOFunctionDefinitions();
   PredicateFunctionDefinitions();
   BasicMathFunctionDefinitions();
   FileCommandDefinitions();
   SortFunctionDefinitions();

#if DEBUGGING_FUNCTIONS
   WatchFunctionDefinitions();
#endif

#if MULTIFIELD_FUNCTIONS
   MultifieldFunctionDefinitions();
#endif

#if STRING_FUNCTIONS
   StringFunctionDefinitions();
#endif

#if EX_MATH
   ExtendedMathFunctionDefinitions();
#endif

#if TEXTPRO_FUNCTIONS || HELP_FUNCTIONS
   HelpFunctionDefinitions();
#endif

#if EMACS_EDITOR
   EditorFunctionDefinition();
#endif

#if CONSTRUCT_COMPILER
   ConstructsToCCommandDefinition();
#endif

#if PROFILING_FUNCTIONS
   ConstructProfilingFunctionDefinitions();
#endif

   ParseFunctionDefinitions();
  }
#endif

/*********************************************************/
/* gentime: A function to return a floating point number */
/*   which indicates the present time. Used internally   */
/*   for timing rule firings and debugging.              */
/*********************************************************/
globle double gentime()
  {
#if   MAC
   UnsignedWide result;

   Microseconds(&result);

   return(((((double) result.hi) * kTwoPower32) + result.lo) / 1000000.0);
#endif

#if IBM_MCW || (IBM_TBC && WINDOW_INTERFACE)
   unsigned long int result;

   result = GetTickCount();

   return((double) result / 1000.0);
#endif

#if   IBM_TBC && (! WINDOW_INTERFACE)
   unsigned long int result;

   result = biostime(0,(long int) 0);

   return((double) result / 18.2);
#endif

#if GENERIC || ((! MAC) && (! IBM_TBC)) 
   return((double) clock() / (double) CLOCKS_PER_SEC);
#endif
  }

/*****************************************************/
/* gensystem: Generic routine for passing a string   */
/*   representing a command to the operating system. */
/*****************************************************/
globle void gensystem()
  {
   char *commandBuffer = NULL;
   int bufferPosition = 0;
   int bufferMaximum = 0;
   int numa, i;
   DATA_OBJECT tempValue;
   char *theString;

   /*===========================================*/
   /* Check for the corret number of arguments. */
   /*===========================================*/

   if ((numa = ArgCountCheck("system",AT_LEAST,1)) == -1) return;

   /*============================================================*/
   /* Concatenate the arguments together to form a single string */
   /* containing the command to be sent to the operating system. */
   /*============================================================*/

   for (i = 1 ; i <= numa; i++)
     {
      RtnUnknown(i,&tempValue);
      if ((GetType(tempValue) != STRING) &&
          (GetType(tempValue) != SYMBOL))
        {
         SetHaltExecution(TRUE);
         SetEvaluationError(TRUE);
         ExpectedTypeError2("system",i);
         return;
        }

     theString = DOToString(tempValue);

     commandBuffer = AppendToString(theString,commandBuffer,&bufferPosition,&bufferMaximum);
    }

   if (commandBuffer == NULL) return;

   /*=======================================*/
   /* Execute the operating system command. */
   /*=======================================*/

#if VAX_VMS
   if (PauseEnvFunction != NULL) (*PauseEnvFunction)();
   VMSSystem(commandBuffer);
   putchar('\n');
   if (ContinueEnvFunction != NULL) (*ContinueEnvFunction)(1);
   if (RedrawScreenFunction != NULL) (*RedrawScreenFunction)();
#endif

#if   UNIX_7 || UNIX_V || IBM_MSC || IBM_TBC || IBM_ICB || IBM_ZTC || IBM_SC || IBM_MCW || IBM_GCC
   if (PauseEnvFunction != NULL) (*PauseEnvFunction)();
   system(commandBuffer);
   if (ContinueEnvFunction != NULL) (*ContinueEnvFunction)(1);
   if (RedrawScreenFunction != NULL) (*RedrawScreenFunction)();
#else

#if ! VAX_VMS
   PrintRouter(WDIALOG,
            "System function not fully defined for this system.\n");
#endif

#endif

   /*==================================================*/
   /* Return the string buffer containing the command. */
   /*==================================================*/

   rm(commandBuffer,bufferMaximum);

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

/***********************************************************/
/* InitializeNonportableFeatures: Initializes non-portable */
/*   features. Currently, the only non-portable feature    */
/*   requiring initialization is the interrupt handler     */
/*   which allows execution to be halted.                  */
/***********************************************************/
static void InitializeNonportableFeatures()
  {
#if ! WINDOW_INTERFACE

#if MAC
   AddPeriodicFunction("systemtask",CallSystemTask,0);
#endif

#if VAX_VMS || UNIX_V || UNIX_7 || IBM_GCC
   signal(SIGINT,CatchCtrlC);
#endif

#if IBM_TBC
   OldCtrlC = getvect(0x23);
   OldBreak = getvect(0x1b);
   setvect(0x23,CatchCtrlC);
   setvect(0x1b,CatchCtrlC);
   atexit(RestoreInterruptVectors);
#endif

#if IBM_MSC || IBM_ICB
   OldCtrlC = _dos_getvect(0x23);
   OldBreak = _dos_getvect(0x1b);
   _dos_setvect(0x23,CatchCtrlC);
   _dos_setvect(0x1b,CatchCtrlC);
   atexit(RestoreInterruptVectors);
#endif

#if IBM_ZTC || IBM_SC
   _controlc_handler = CatchCtrlC;
   controlc_open();
#endif

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

#if MAC
/************************************************************/
/* CallSystemTask: Macintosh specific function which allows */
/*   periodic tasks to be handled by the operating system.  */
/************************************************************/
static void CallSystemTask()
  {
   Thread static unsigned long int lastCall;

   if (TickCount() < (lastCall + 10)) return;
   SystemTask();
   lastCall = TickCount();
   return;
  }
#endif

#if   VAX_VMS || UNIX_V || UNIX_7 || IBM_GCC
/**********************************************/
/* CatchCtrlC: VMS and UNIX specific function */
/*   to allow control-c interrupts.           */
/**********************************************/
static void CatchCtrlC(
  int sgnl)
  {
   SetHaltExecution(TRUE);
   CloseAllBatchSources();
   signal(SIGINT,CatchCtrlC);
  }
#endif

#if   IBM_TBC || IBM_MSC
/******************************************************/
/* CatchCtrlC: IBM Microsoft C and Borland Turbo C    */
/*   specific function to allow control-c interrupts. */
/******************************************************/
static void interrupt CatchCtrlC()
  {
   SetHaltExecution(TRUE);
   CloseAllBatchSources();
  }

/**************************************************************/
/* RestoreInterruptVectors: IBM Microsoft C and Borland Turbo */
/*   C specific function for restoring interrupt vectors.     */
/**************************************************************/
static void RestoreInterruptVectors()
  {
#if IBM_TBC
   setvect(0x23,OldCtrlC);
   setvect(0x1b,OldBreak);
#else
   _dos_setvect(0x23,OldCtrlC);
   _dos_setvect(0x1b,OldBreak);
#endif
  }
#endif

#if IBM_ZTC || IBM_SC
/***********************************************/
/* CatchCtrlC: IBM Zortech C specific function */
/*   to allow control-c interrupts.            */
/***********************************************/
static void _cdecl CatchCtrlC()
  {
   SetHaltExecution(TRUE);
   CloseAllBatchSources();
  }
#endif

#if   IBM_ICB
/*************************************************/
/* CatchCtrlC: IBM Intel C Code Builder specific */
/*   function to allow control-c interrupts.     */
/*************************************************/
static void CatchCtrlC()
  {
   _XSTACK *sf;                        /* Real-mode interrupt handler stack frame. */

   sf = (_XSTACK *) _get_stk_frame();  /* Get pointer to V86 _XSTACK frame. */
   SetHaltExecution(TRUE);             /* Terminate execution and */
   CloseAllBatchSources();             /* return to the command prompt.        */
   sf->opts |= _STK_NOINT;             /* Set _ST_NOINT to prevent V86 call. */
  }

/********************************************************/
/* RestoreInterruptVectors: IBM Intel C Code Builder    */
/*   specific function for restoring interrupt vectors. */
/********************************************************/
static void RestoreInterruptVectors()
  {
   _dos_setvect(0x23,OldCtrlC);
   _dos_setvect(0x1b,OldBreak);
  }
#endif

#endif

/**************************************/
/* GENEXIT:  A generic exit function. */
/**************************************/
globle void genexit(
  int num)
  {
   exit(num);
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

/*****************************************************************/
/* GenOpen: Generic and machine specific code for opening a file */
/*   for binary access. Only one file may be open at a time when */
/*   using this function since the file pointer is stored in a   */
/*   global variable.                                            */
/*****************************************************************/
globle int GenOpen(
  char *funcName,
  char *fileName)
  {
#if  MAC
   Str255 tempName;
   OSErr resultCode;
   Str255 volName;
   short int vRefNum;

   resultCode = GetVol(volName,&vRefNum);
   if (resultCode != noErr)
     {
      OpenErrorMessage(funcName,fileName);
      return(0);
     }
   strcpy((char *) tempName,fileName);
#if MAC_SC7 || MAC_SC8
   C2PStr((char *) tempName);
#else
   c2pstr((char *) tempName);
#endif

   resultCode = FSOpen(tempName,vRefNum,&BinaryRefNum);
   if (resultCode != noErr)
     {
      OpenErrorMessage(funcName,fileName);
      return(FALSE);
     }

#endif

#if IBM_TBC || IBM_MSC || IBM_ICB || IBM_MCW /* || IBM_ZTC */
   BinaryFileHandle = open(fileName,O_RDONLY | O_BINARY);
   if (BinaryFileHandle == -1)
     {
      OpenErrorMessage(funcName,fileName);
      return(FALSE);
     }
#endif

#if (! MAC) && (! IBM_TBC) && (! IBM_MSC) && (! IBM_ICB) && (! IBM_MCW) /* && (! IBM_ZTC) */
   if ((BinaryFP = fopen(fileName,"rb")) == NULL)
     {
      OpenErrorMessage(funcName,fileName);
      return(FALSE);
     }
#endif

   return(TRUE);
  }

/*****************************************/
/* GenRead: Generic and machine specific */
/*   code for reading from a file.       */
/*****************************************/
globle void GenRead(
  void *dataPtr,
  unsigned long size)
  {
#if MAC
   long dataSize;

   dataSize = size;
   FSRead(BinaryRefNum,&dataSize,dataPtr);
#endif

#if IBM_TBC || IBM_MSC || IBM_ICB || IBM_MCW /* || IBM_ZTC */
   char *tempPtr;

   tempPtr = (char *) dataPtr;
   while (size > INT_MAX)
     {
      /* read(BinaryFileHandle,(void *) tempPtr,(unsigned int) INT_MAX); */
      read(BinaryFileHandle,tempPtr,INT_MAX);
      size -= INT_MAX;
      tempPtr = tempPtr + INT_MAX;
     }

   if (size > 0) 
     { read(BinaryFileHandle,tempPtr,(int) size); }
     /* { read(BinaryFileHandle,(void *) tempPtr,(unsigned int) size); } */
#endif

#if (! MAC) && (! IBM_TBC) && (! IBM_MSC) && (! IBM_ICB) && (! IBM_MCW) /* && (! IBM_ZTC) */
   unsigned int temp, number_of_reads, read_size;

   if (sizeof(int) == sizeof(long))
     { read_size = size; }
   else
     { read_size = (1L << (sizeof(int) * 8L)) - 1L ; }
   number_of_reads = size / read_size;
   temp = size - ((long) number_of_reads * (long) read_size);

   while (number_of_reads > 0)
     {
      fread(dataPtr,(CLIPS_STD_SIZE) read_size,1,BinaryFP);
      dataPtr = ((char *) dataPtr) + read_size;
      number_of_reads--;
     }

   fread(dataPtr,(CLIPS_STD_SIZE) temp,1,BinaryFP);
#endif
  }

/*******************************************/
/* GenSeek:  Generic and machine specific */
/*   code for closing a file.              */
/*******************************************/
globle void GenSeek(
  long offset)
  {
#if  MAC
   SetFPos(BinaryRefNum,3,offset);
#endif

#if IBM_TBC || IBM_MSC || IBM_ICB || IBM_MCW /* || IBM_ZTC */
   lseek(BinaryFileHandle,offset,SEEK_CUR);
#endif

#if (! MAC) && (! IBM_TBC) && (! IBM_MSC) && (! IBM_ICB) && (! IBM_MCW) /* && (! IBM_ZTC) */
   fseek(BinaryFP,offset,SEEK_CUR);
#endif
  }

/*******************************************/
/* GenClose:  Generic and machine specific */
/*   code for closing a file.              */
/*******************************************/
globle void GenClose()
  {
#if  MAC
   FSClose(BinaryRefNum);
#endif

#if IBM_TBC || IBM_MSC || IBM_ICB || IBM_MCW /* || IBM_ZTC */
   close(BinaryFileHandle);
#endif

#if (! MAC) && (! IBM_TBC) && (! IBM_MSC) && (! IBM_ICB) && (! IBM_MCW) /* && (! IBM_ZTC) */
   fclose(BinaryFP);
#endif
  }

/*********************************************/
/* InitializeKeywords: Adds key words to the */
/*   symbol table so that they are available */
/*   for command completion.                 */
/*********************************************/
static void InitializeKeywords()
  {
#if (! RUN_TIME) && WINDOW_INTERFACE
   void *ts;

   /*====================*/
   /* construct keywords */
   /*====================*/

   ts = AddSymbol("defrule");
   IncrementSymbolCount(ts);
   ts = AddSymbol("defglobal");
   IncrementSymbolCount(ts);
   ts = AddSymbol("deftemplate");
   IncrementSymbolCount(ts);
   ts = AddSymbol("deffacts");
   IncrementSymbolCount(ts);
   ts = AddSymbol("deffunction");
   IncrementSymbolCount(ts);
   ts = AddSymbol("defmethod");
   IncrementSymbolCount(ts);
   ts = AddSymbol("defgeneric");
   IncrementSymbolCount(ts);
   ts = AddSymbol("defclass");
   IncrementSymbolCount(ts);
   ts = AddSymbol("defmessage-handler");
   IncrementSymbolCount(ts);
   ts = AddSymbol("definstances");
   IncrementSymbolCount(ts);

   /*=======================*/
   /* set-strategy keywords */
   /*=======================*/

   ts = AddSymbol("depth");
   IncrementSymbolCount(ts);
   ts = AddSymbol("breadth");
   IncrementSymbolCount(ts);
   ts = AddSymbol("lex");
   IncrementSymbolCount(ts);
   ts = AddSymbol("mea");
   IncrementSymbolCount(ts);
   ts = AddSymbol("simplicity");
   IncrementSymbolCount(ts);
   ts = AddSymbol("complexity");
   IncrementSymbolCount(ts);
   ts = AddSymbol("random");
   IncrementSymbolCount(ts);

   /*==================================*/
   /* set-salience-evaluation keywords */
   /*==================================*/

   ts = AddSymbol("when-defined");
   IncrementSymbolCount(ts);
   ts = AddSymbol("when-activated");
   IncrementSymbolCount(ts);
   ts = AddSymbol("every-cycle");
   IncrementSymbolCount(ts);

   /*======================*/
   /* deftemplate keywords */
   /*======================*/

   ts = AddSymbol("field");
   IncrementSymbolCount(ts);
   ts = AddSymbol("multifield");
   IncrementSymbolCount(ts);
   ts = AddSymbol("default");
   IncrementSymbolCount(ts);
   ts = AddSymbol("type");
   IncrementSymbolCount(ts);
   ts = AddSymbol("allowed-symbols");
   IncrementSymbolCount(ts);
   ts = AddSymbol("allowed-strings");
   IncrementSymbolCount(ts);
   ts = AddSymbol("allowed-numbers");
   IncrementSymbolCount(ts);
   ts = AddSymbol("allowed-integers");
   IncrementSymbolCount(ts);
   ts = AddSymbol("allowed-floats");
   IncrementSymbolCount(ts);
   ts = AddSymbol("allowed-values");
   IncrementSymbolCount(ts);
   ts = AddSymbol("min-number-of-elements");
   IncrementSymbolCount(ts);
   ts = AddSymbol("max-number-of-elements");
   IncrementSymbolCount(ts);
   ts = AddSymbol("NONE");
   IncrementSymbolCount(ts);
   ts = AddSymbol("VARIABLE");
   IncrementSymbolCount(ts);

   /*==================*/
   /* defrule keywords */
   /*==================*/

   ts = AddSymbol("declare");
   IncrementSymbolCount(ts);
   ts = AddSymbol("salience");
   IncrementSymbolCount(ts);
   ts = AddSymbol("test");
   IncrementSymbolCount(ts);
   ts = AddSymbol("or");
   IncrementSymbolCount(ts);
   ts = AddSymbol("and");
   IncrementSymbolCount(ts);
   ts = AddSymbol("not");
   IncrementSymbolCount(ts);
   ts = AddSymbol("logical");
   IncrementSymbolCount(ts);

   /*===============*/
   /* COOL keywords */
   /*===============*/

   ts = AddSymbol("is-a");
   IncrementSymbolCount(ts);
   ts = AddSymbol("role");
   IncrementSymbolCount(ts);
   ts = AddSymbol("abstract");
   IncrementSymbolCount(ts);
   ts = AddSymbol("concrete");
   IncrementSymbolCount(ts);
   ts = AddSymbol("pattern-match");
   IncrementSymbolCount(ts);
   ts = AddSymbol("reactive");
   IncrementSymbolCount(ts);
   ts = AddSymbol("non-reactive");
   IncrementSymbolCount(ts);
   ts = AddSymbol("slot");
   IncrementSymbolCount(ts);
   ts = AddSymbol("field");
   IncrementSymbolCount(ts);
   ts = AddSymbol("multiple");
   IncrementSymbolCount(ts);
   ts = AddSymbol("single");
   IncrementSymbolCount(ts);
   ts = AddSymbol("storage");
   IncrementSymbolCount(ts);
   ts = AddSymbol("shared");
   IncrementSymbolCount(ts);
   ts = AddSymbol("local");
   IncrementSymbolCount(ts);
   ts = AddSymbol("access");
   IncrementSymbolCount(ts);
   ts = AddSymbol("read");
   IncrementSymbolCount(ts);
   ts = AddSymbol("write");
   IncrementSymbolCount(ts);
   ts = AddSymbol("read-only");
   IncrementSymbolCount(ts);
   ts = AddSymbol("read-write");
   IncrementSymbolCount(ts);
   ts = AddSymbol("initialize-only");
   IncrementSymbolCount(ts);
   ts = AddSymbol("propagation");
   IncrementSymbolCount(ts);
   ts = AddSymbol("inherit");
   IncrementSymbolCount(ts);
   ts = AddSymbol("no-inherit");
   IncrementSymbolCount(ts);
   ts = AddSymbol("source");
   IncrementSymbolCount(ts);
   ts = AddSymbol("composite");
   IncrementSymbolCount(ts);
   ts = AddSymbol("exclusive");
   IncrementSymbolCount(ts);
   ts = AddSymbol("allowed-lexemes");
   IncrementSymbolCount(ts);
   ts = AddSymbol("allowed-instances");
   IncrementSymbolCount(ts);
   ts = AddSymbol("around");
   IncrementSymbolCount(ts);
   ts = AddSymbol("before");
   IncrementSymbolCount(ts);
   ts = AddSymbol("primary");
   IncrementSymbolCount(ts);
   ts = AddSymbol("after");
   IncrementSymbolCount(ts);
   ts = AddSymbol("of");
   IncrementSymbolCount(ts);
   ts = AddSymbol("self");
   IncrementSymbolCount(ts);
   ts = AddSymbol("visibility");
   IncrementSymbolCount(ts);
   ts = AddSymbol("override-message");
   IncrementSymbolCount(ts);
   ts = AddSymbol("private");
   IncrementSymbolCount(ts);
   ts = AddSymbol("public");
   IncrementSymbolCount(ts);
   ts = AddSymbol("create-accessor");
   IncrementSymbolCount(ts);

   /*================*/
   /* watch keywords */
   /*================*/

   ts = AddSymbol("compilations");
   IncrementSymbolCount(ts);
   ts = AddSymbol("deffunctions");
   IncrementSymbolCount(ts);
   ts = AddSymbol("globals");
   IncrementSymbolCount(ts);
   ts = AddSymbol("rules");
   IncrementSymbolCount(ts);
   ts = AddSymbol("activations");
   IncrementSymbolCount(ts);
   ts = AddSymbol("statistics");
   IncrementSymbolCount(ts);
   ts = AddSymbol("facts");
   IncrementSymbolCount(ts);
   ts = AddSymbol("generic-functions");
   IncrementSymbolCount(ts);
   ts = AddSymbol("methods");
   IncrementSymbolCount(ts);
   ts = AddSymbol("instances");
   IncrementSymbolCount(ts);
   ts = AddSymbol("slots");
   IncrementSymbolCount(ts);
   ts = AddSymbol("messages");
   IncrementSymbolCount(ts);
   ts = AddSymbol("message-handlers");
   IncrementSymbolCount(ts);
   ts = AddSymbol("focus");
   IncrementSymbolCount(ts);
#endif
  }

LOCALE void *GetGenFP(void)
{
    return BinaryFP;
}

   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.10  04/09/97            */
   /*                                                     */
   /*                  SETUP HEADER FILE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: This file is the general header file included by */
/*   all of the .c source files. It contains global          */
/*   definitions and the compiler flags which must be edited */
/*   to create a version for a specific machine, operating   */
/*   system, or feature set.                                 */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Donnell                                     */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_setup
#define _H_setup

/****************************************************************/
/* -------------------- COMPILER FLAGS ------------------------ */
/****************************************************************/

/*********************************************************************/
/* Flag denoting the environment in which the executable is to run.  */
/* Only one of these flags should be turned on (set to 1) at a time. */
/*********************************************************************/

#define GENERIC 1   /* Generic (any machine)                  */
#define VAX_VMS 0   /* VAX VMS                                */
#define UNIX_V  0   /* UNIX System V or 4.2bsd or HP Unix     */
#define UNIX_7  0   /* UNIX System III Version 7 or Sun Unix  */
#define MAC_SC6 0   /* Macintosh, with Symantec C 6.0         */
#define MAC_SC7 0   /* Macintosh, with Symantec C 7.0         */
#define MAC_SC8 0   /* Macintosh, with Symantec C 8.0         */
#define MAC_MPW 0   /* Macintosh, with MPW 3.2 C              */
#define MAC_MCW 0   /* Macintosh, with Code Warrior 3.0       */
#define IBM_MCW 0   /* IBM PC, with CodeWarrior 3.0           */
#define IBM_ZTC 0   /* IBM PC, with Zortech C++ 3.1           */
#define IBM_MSC 0   /* IBM PC, with Microsoft C 6.0           */
#define IBM_TBC 0   /* IBM PC, with Borland C++ 5.0           */
#define IBM_ICB 0   /* IBM PC, with Intel C Code Builder 1.0  */
#define IBM_SC  0   /* IBM PC, with Symantec C++ 6.1          */
#define IBM_GCC 0   /* IBM PC, with DJGPP 2.01                */

#if IBM_ZTC || IBM_MSC || IBM_TBC || IBM_ICB || IBM_SC || IBM_MCW
#define IBM 1
#else
#define IBM 0
#endif

#if MAC_SC6 || MAC_SC7 || MAC_SC8
#define MAC_SC 1
#else
#define MAC_SC 0
#endif

#if MAC_SC || MAC_MPW || MAC_MCW
#define MAC 1
#else
#define MAC 0
#endif

/*********************************************/
/* Old definitions when K&R C was supported. */
/*********************************************/

#define VOID     void
#define VOID_ARG void
#define CLIPS_STD_SIZE size_t

#define BOOLEAN int
#define globle

/*******************************************/
/* RUN_TIME:  Specifies whether a run-time */
/*   module is being created.              */
/*******************************************/

#ifndef RUN_TIME
#define RUN_TIME 0
#endif

/*************************************************/
/* DEFRULE_CONSTRUCT: Determines whether defrule */
/*   construct is included.                      */
/*************************************************/

#define DEFRULE_CONSTRUCT 1

#define CONFLICT_RESOLUTION_STRATEGIES 1
#define DYNAMIC_SALIENCE 1
#define INCREMENTAL_RESET 1
#define LOGICAL_DEPENDENCIES  1

#if ! DEFRULE_CONSTRUCT
#undef CONFLICT_RESOLUTION_STRATEGIES
#undef DYNAMIC_SALIENCE
#undef INCREMENTAL_RESET
#undef LOGICAL_DEPENDENCIES
#define CONFLICT_RESOLUTION_STRATEGIES  0
#define DYNAMIC_SALIENCE                0
#define INCREMENTAL_RESET               0
#define LOGICAL_DEPENDENCIES            0
#endif

/************************************************/
/* DEFMODULE_CONSTRUCT:  Determines whether the */
/*   defmodule construct is included.           */
/************************************************/

#define DEFMODULE_CONSTRUCT 1

/****************************************************/
/* DEFTEMPLATE_CONSTRUCT:  Determines whether facts */
/*   and the deftemplate construct are included.    */
/****************************************************/

#define DEFTEMPLATE_CONSTRUCT 1

#if ! DEFRULE_CONSTRUCT
#undef DEFTEMPLATE_CONSTRUCT
#define DEFTEMPLATE_CONSTRUCT 0
#endif

/****************************************************/
/* DEFFACTS_CONSTRUCT:  Determines whether deffacts */
/*   construct is included.                         */
/****************************************************/

#define DEFFACTS_CONSTRUCT 1

#if ! DEFTEMPLATE_CONSTRUCT
#undef DEFFACTS_CONSTRUCT
#define DEFFACTS_CONSTRUCT 0
#endif

/************************************************/
/* DEFGLOBAL_CONSTRUCT:  Determines whether the */
/*   defglobal construct is included.           */
/************************************************/

#define DEFGLOBAL_CONSTRUCT 1

/**********************************************/
/* DEFFUNCTION_CONSTRUCT:  Determines whether */
/*   deffunction construct is included.       */
/**********************************************/

#define DEFFUNCTION_CONSTRUCT 1

/*********************************************/
/* DEFGENERIC_CONSTRUCT:  Determines whether */
/*   generic functions  are included.        */
/*********************************************/

#define DEFGENERIC_CONSTRUCT 1

/******************************************************************/
/* IMPERATIVE_METHODS: Determines if call-next-method and         */
/*   override-next-method can be used to execute shadowed methods */
/******************************************************************/

#define IMPERATIVE_METHODS 1

#if ! DEFGENERIC_CONSTRUCT
#undef IMPERATIVE_METHODS
#define IMPERATIVE_METHODS 0
#endif

/*****************************************************************/
/* OBJECT_SYSTEM:  Determines whether object system is included. */
/*   The MULTIFIELD_FUNCTIONS flag should also be on if you want */
/*   to be able to manipulate multi-field slots.                 */
/*****************************************************************/

#define OBJECT_SYSTEM 1

/*****************************************************************/
/* DEFINSTANCES_CONSTRUCT: Determines whether the definstances   */
/*   construct is enabled.                                       */
/*****************************************************************/

#define DEFINSTANCES_CONSTRUCT      1                   /* sed */

#if ! OBJECT_SYSTEM
#undef DEFINSTANCES_CONSTRUCT
#define DEFINSTANCES_CONSTRUCT      0
#endif

/******************************************************************/
/* IMPERATIVE_MESSAGE_HANDLERS: Determines if "around" message-   */
/*   handlers are allowed. Also determines if call-next-handler   */
/*   and override-next-handler can be used to execute shadowed    */
/*   handlers                                                     */
/* AUXILIARY_MESSAGE_HANDLERS: Determines if "before" and "after" */
/*   message-handlers are allowed.                                */
/* Turning both flags off can increase the speed of message       */
/*   dispatch.                                                    */
/******************************************************************/

#define IMPERATIVE_MESSAGE_HANDLERS 1
#define AUXILIARY_MESSAGE_HANDLERS 1

#if ! OBJECT_SYSTEM
#undef IMPERATIVE_MESSAGE_HANDLERS
#undef AUXILIARY_MESSAGE_HANDLERS
#define IMPERATIVE_MESSAGE_HANDLERS 0
#define AUXILIARY_MESSAGE_HANDLERS  0
#endif

/********************************************************************/
/* INSTANCE_SET_QUERIES: Determines if instance-set query functions */
/*  such as any-instancep and do-for-all-instances are included.    */
/********************************************************************/

#define INSTANCE_SET_QUERIES 1

#if ! OBJECT_SYSTEM
#undef INSTANCE_SET_QUERIES
#define INSTANCE_SET_QUERIES        0
#endif

/********************************************************************/
/* INSTANCE_PATTERN_MATCHING: Determines if direct pattern-matching */
/*   on objects on the LHS of rules is allowed.                     */
/********************************************************************/

#define INSTANCE_PATTERN_MATCHING 1

#if (! OBJECT_SYSTEM) || (! DEFRULE_CONSTRUCT)
#undef INSTANCE_PATTERN_MATCHING
#define INSTANCE_PATTERN_MATCHING 0
#endif

/******************************************************************/
/* Check for consistencies associated with the defrule construct. */
/******************************************************************/

#if (! DEFTEMPLATE_CONSTRUCT) && (! INSTANCE_PATTERN_MATCHING)
#undef DEFRULE_CONSTRUCT
#define DEFRULE_CONSTRUCT 0
#endif

#if (! DEFRULE_CONSTRUCT)
#undef CONFLICT_RESOLUTION_STRATEGIES
#define CONFLICT_RESOLUTION_STRATEGIES  0
#undef DYNAMIC_SALIENCE
#define DYNAMIC_SALIENCE                0
#undef INCREMENTAL_RESET
#define INCREMENTAL_RESET               0
#undef LOGICAL_DEPENDENCIES
#define LOGICAL_DEPENDENCIES            0
#endif

/*******************************************************************/
/* BLOAD/BSAVE_INSTANCES: Determines if the save/restore-instances */
/*  functions can be enhanced to perform more quickly by using     */
/*  binary files                                                   */
/*******************************************************************/

#define BLOAD_INSTANCES 1
#define BSAVE_INSTANCES 1

#if ! OBJECT_SYSTEM
#undef BLOAD_INSTANCES
#undef BSAVE_INSTANCES
#define BLOAD_INSTANCES             0
#define BSAVE_INSTANCES             0
#endif

/****************************************************************/
/* EXTENDED MATH PACKAGE FLAG: If this is on, then the extended */
/* math package functions will be available for use, (normal    */
/* default). If this flag is off, then the extended math        */
/* functions will not be available, and the 30K or so of space  */
/* they require will be free. Usually a concern only on PC type */
/* machines.                                                    */
/****************************************************************/

#define EX_MATH 1

/****************************************************************/
/* TEXT PROCESSING : Turn on this flag for support of the       */
/* hierarchical lookup system.                                  */
/****************************************************************/

#define TEXTPRO_FUNCTIONS 1

/****************************************************************/
/* HELP: To implement the help facility, set the flag below and */
/* specify the path and name of the help data file your system. */
/****************************************************************/

#define HELP_FUNCTIONS 1

#if HELP_FUNCTIONS
#define HELP_DEFAULT "clips.hlp"
#endif

/*************************************************************************/
/* BLOAD_ONLY:      Enables bload command and disables the load command. */
/* BLOAD:           Enables bload command.                               */
/* BLOAD_AND_BSAVE: Enables bload, and bsave commands.                   */
/*************************************************************************/

#define BLOAD_ONLY 0
#define BLOAD 0
#define BLOAD_AND_BSAVE 1

#if RUN_TIME
#undef BLOAD_ONLY
#define BLOAD_ONLY      0
#undef BLOAD
#define BLOAD           0
#undef BLOAD_AND_BSAVE
#define BLOAD_AND_BSAVE 0
#endif

/****************************************************************/
/* EMACS_EDITOR: If this flag is turned on, an integrated EMACS */
/*   style editor can be utilized on supported machines.        */
/****************************************************************/

#define  EMACS_EDITOR 0

#if GENERIC || MAC
#undef EMACS_EDITOR                         /* Editor can't be used */
#define  EMACS_EDITOR  0                    /* with Generic or Mac  */
#endif

/********************************************************************/
/* CONSTRUCT COMPILER: If this flag is turned on, you can generate  */
/*   C code representing the constructs in the current environment. */
/*   With the RUN_TIME flag set, this code can be compiled and      */
/*   linked to create a stand-alone run-time executable.            */
/********************************************************************/

#define  CONSTRUCT_COMPILER 1

#if CONSTRUCT_COMPILER
#define API_HEADER "clips.h"
#endif

/*******************************************/
/* BASIC_IO: Includes printout, fprintout, */
/*   read, open, and close functions.      */
/*******************************************/

#define BASIC_IO 1

/***************************************************/
/* EXT_IO: Includes format and readline functions. */
/***************************************************/

#define EXT_IO 1

/************************************************/
/* STRING_FUNCTIONS: Includes string functions: */
/*   str-length, str-compare, upcase, lowcase,  */
/*   sub-string, str-index, and eval.           */
/************************************************/

#define STRING_FUNCTIONS 1

/*********************************************/
/* MULTIFIELD_FUNCTIONS: Includes multifield */
/*   functions:  mv-subseq, mv-delete,       */
/*   mv-append, str-explode, str-implode.    */
/*********************************************/

#define MULTIFIELD_FUNCTIONS 1

/****************************************************/
/* DEBUGGING_FUNCTIONS: Includes functions such as  */
/*   rules, facts, matches, ppdefrule, etc.         */
/****************************************************/

#define DEBUGGING_FUNCTIONS 1

/************************************************************************/
/* BLOCK_MEMORY: Causes memory to be allocated in large blocks.         */
/*   INITBUFFERSIZE and BLOCKSIZE should both be set to less than the  */
/*   maximum size of a signed integer. On a 16-bit machine, they should */
/*   be less than 32768. If a LightSpeed (Think) C version 2.x or 3.x   */
/*   compiler is being used, then this option should be turned on.      */
/************************************************************************/

#define BLOCK_MEMORY 0

#if BLOCK_MEMORY

#define INITBLOCKSIZE 32000
#define BLOCKSIZE 32000

#endif

/*****************************************************************/
/* WINDOW_INTERFACE : Set this flag if you are recompiling the   */
/*   IBM-PC MS-DOS Window Interface or the Macintosh LSC Window  */
/*   Interface. Currently, when enabled, this flag disables the  */
/*   more processing used by the help system.                    */
/*   This flag also prevents any input or output being directly  */
/*   sent to stdin or stdout.                                    */
/*****************************************************************/

#ifndef WINDOW_INTERFACE
#define WINDOW_INTERFACE 1
#endif

#if WINDOW_INTERFACE
#undef EMACS_EDITOR                         /* Editor can't be used with */
#define  EMACS_EDITOR  0                    /* windowed interface        */
#endif

/*****************************************************************/
/* SHORT_LINK_NAMES: Converts some function and global variable  */
/*   names to 6 characters. Use with linkers that recognize      */
/*   fewer significant characters than the C compiler generating */
/*   the object code.                                            */
/*****************************************************************/

#define SHORT_LINK_NAMES 0

#if SHORT_LINK_NAMES
#include "shrtlnkn.h"
#endif

/********************************************/
/* DEVELOPER: Enables code for debugging a  */
/*   development version of the executable. */
/********************************************/

#ifndef DEVELOPER
#define DEVELOPER 0
#endif

#if DEVELOPER
#include <assert.h>
#define Bogus(x) assert(! (x))
#else
#define Bogus(x)
#endif

/***************************************************/
/* PROFILING_FUNCTIONS: Enables code for profiling */
/*   constructs and user functions.                */
/***************************************************/

#define PROFILING_FUNCTIONS 1

/***************************************************/
/*                Thread definitions               */
/***************************************************/
#define THREAD_STORAGE    1

#if  THREAD_STORAGE
	#ifdef WIN32
		#define Thread  __declspec( thread )
	#else
		#define Thread __thread
	#endif
#else
	#define Thread
#endif

/**************************************************/
/*             DllExport definition               */
/**************************************************/
#define DLL_Build 1
#if DLL_Build
	#define DllExport	__declspec( dllexport )
#else
	#define DllExport
#endif

#endif
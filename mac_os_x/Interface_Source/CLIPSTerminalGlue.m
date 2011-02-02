/*
 *  CLIPSTerminalGlue.c
 *  CLIPS
 *
 *  Created by Gary Riley on 3/25/06.
 *
 */

#import "CLIPSTerminalGlue.h"

#import "CLIPSTerminalController.h"
#import "CLIPSEnvironment.h"
#import "EnvController.h"

/*********************************************/
/* ClearEnvironmentWindowCommand: H/L access */
/*   routine for the clear-window command.   */
/*********************************************/
void ClearEnvironmentWindowCommand(
  void *theEnv, EXEC_STATUS)
  {
   if (EnvArgCountCheck(theEnv,execStatus,"clear-window",EXACTLY,0) == -1) return;
   id theObject = GetEnvironmentContext(theEnv);
   
   [theObject clearScrollbackFunction];
  }
  
/**********************************************************/
/* QueryInterfaceRouter: Router function which recognizes */
/*   I/O directed to the display window.                  */
/**********************************************************/
intBool QueryInterfaceRouter(
  void *theEnv,
  char *logicalName)
  {
#if MAC_MCW || MAC_XCD
#pragma unused(theEnv)
#endif
   
   if ( (strcmp(logicalName,"stdout") == 0) ||
        (strcmp(logicalName,"stdin") == 0) ||
        (strcmp(logicalName,WPROMPT) == 0) ||
        (strcmp(logicalName,WTRACE) == 0) ||
        (strcmp(logicalName,WERROR) == 0) ||
        (strcmp(logicalName,WWARNING) == 0) ||
        (strcmp(logicalName,WDISPLAY) == 0) ||
        (strcmp(logicalName,WDIALOG) == 0) )
     { return(TRUE); }

    return(FALSE);
  }

/*****************************************/
/* PrintInterfaceRouter: Router function */
/*   which prints to the display window. */
/*****************************************/
int PrintInterfaceRouter(
  void *theEnv,
  char *logicalName,
  char *str)
  {
   FILE *fptr;
   NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
   id theObject = GetEnvironmentRouterContext(theEnv);

   fptr = FindFptr(theEnv,logicalName);
   if (fptr == stdout)
     { [theObject printC: str]; }
   else
     { fprintf(fptr,"%s",str); } // TBD Is this necessary?
 
   [pool release];

   return(TRUE);
  }

/*******************************************/
/* GetcInterfaceRouter: Router function to */
/*   get input from the display window and */
/*   process other events.                 */
/*******************************************/
int GetcInterfaceRouter(
  void *theEnv,
  char *logicalName)
  {
   NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
   int theChar;
   id theObject = GetEnvironmentRouterContext(theEnv);

   theChar = [theObject waitForChar];

   [pool release];
   
   return(theChar);
  }
  
/*************************************************/
/* ExitInterfaceRouter: Routine to check an exit */
/*   from the dialog window to make sure that    */
/*   the application doesn't exit.               */
/*************************************************/
int ExitInterfaceRouter(
  void *theEnv,
  int num)
  {   
   CLIPSTerminalController *theController = GetEnvironmentRouterContext(theEnv);
   [theController exit];
   /* AbortExit(theEnv); */
   return(TRUE);
  }  
    
/**************************************/
/* MacPeriodicFunction:               */
/**************************************/
void MacPeriodicFunction(
  void *theEnv)
  {
   CLIPSTerminalController *theController = GetEnvironmentContext(theEnv);
   NSConditionLock *theAgendaLock, *theFactsLock;
   NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
   
   /* TBD See if there are other ways to check the locks */
   
   if ([[theController pauseLock] condition] == EXECUTION_IS_PAUSED)
     {
      [[theController pauseLock] lockWhenCondition: EXECUTION_IS_NOT_PAUSED];
      [[theController pauseLock] unlock];
     }

   /*============================================================*/ 
   /* If there are debugging windows displaying the state of the */
   /* agenda, then update the agenda if necessary. Acquiring the */
   /* lock as frequently as this function is called can kill     */
   /* performance so we first check to see if there are windows  */
   /* that need the agenda and an agenda fetch has been issued.  */
   /*============================================================*/ 

   theAgendaLock = [[theController environment] agendaLock];
   if (([[theController environment] agendaListenerCount] != 0) &&
       ([theAgendaLock condition] == FETCH_AGENDA))
     {
      [theAgendaLock lock];
      switch ([theAgendaLock condition])
        {
         case FETCH_AGENDA:
           [[theController environment] fetchAgenda: NO];
           [theAgendaLock unlockWithCondition: AGENDA_FETCHED];
           break;
        
         default:
           [theAgendaLock unlock];
           break;
        }
     }

   theFactsLock = [[theController environment] factsLock];
   if (([[theController environment] factsListenerCount] != 0) &&
       ([theFactsLock condition] == FETCH_FACTS))
     {
      [theFactsLock lock];
      switch ([theFactsLock condition])
        {
         case FETCH_FACTS:
           [[theController environment] fetchFacts: NO];
           [theFactsLock unlockWithCondition: FACTS_FETCHED];
           break;
        
         default:
           [theFactsLock unlock];
           break;
        }
     }
     
   [pool release];
  }
    
/**************************************/
/* MacYieldTimeFunction:              */
/**************************************/
void MacYieldTimeFunction()
  {
  }
  
/**************************************/
/* MacBeforeOpenFunction:             */
/**************************************/
int MacBeforeOpenFunction(
  void *theEnv)
  {
   CLIPSTerminalController *theController = GetEnvironmentRouterContext(theEnv);
     
   NSLock *theLock = [[theController envController] fileOpenLock];
   
   [theLock lock];
   
   [[NSFileManager defaultManager] changeCurrentDirectoryPath: [theController currentDirectory]];

   return TRUE;
  }  
  
/**************************************/
/* MacAfterOpenFunction:             */
/**************************************/
int MacAfterOpenFunction(
  void *theEnv)
  {
   CLIPSTerminalController *theController = GetEnvironmentRouterContext(theEnv);
   
   NSLock *theLock = [[theController envController] fileOpenLock];
   
   [theLock unlock];

   //NSLog(@"After theLock = %@",theLock);
   
   return TRUE;
  } 

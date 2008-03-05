
#import "CLIPSEnvironment.h"
#import "CLIPSFactInstance.h"
#import "CLIPSFocus.h"
#import "CLIPSModule.h"
#import "CLIPSActivation.h"

#import <CLIPS/clips.h>

#include <setjmp.h>

@implementation CLIPSEnvironment

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Initialization/Deallocation Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*********/
/* init: */
/*********/
- (id) init
  {
   self = [super init];
   
   if (self) 
     {    
      environment = CreateEnvironment();
      if (environment == NULL)
        {
         [self release];
         return nil;
        }
      
      executionLock = [[NSLock alloc] init];
      accessLock = [[NSLock alloc] init];
      
      agendaLock = [[NSConditionLock alloc] initWithCondition: NO_AGENDA_LISTENERS];
      agendaListenerCount = 0;
      
      focusStack = [[NSArray alloc] init];
      runningFocusStack = NULL;

      factsLock = [[NSConditionLock alloc] initWithCondition: NO_FACTS_LISTENERS];
      factsListenerCount = 0;

      factModule = [[NSArray alloc] init];
      runningFactModule = NULL;

      factList = [[NSArray alloc] init];
      runningFactList = NULL;

      instancesLock = [[NSConditionLock alloc] initWithCondition: NO_FACTS_LISTENERS];
      instancesListenerCount = 0;

      instanceModule = [[NSArray alloc] init];
      runningInstanceModule = NULL;

      instanceList = [[NSArray alloc] init];
      runningInstanceList = NULL;
      
      lastAgendaFetch = -1;
      lastFactsFetch = -1;
      lastInstancesFetch = -1;
      exited = FALSE;
     }
   
   return self;
  }

/************/    
/* dealloc: */
/************/    
- (void) dealloc
  {
   NSLog(@"CLIPSEnvironment dealloc");

   /*================================*/
   /* Destroy the CLIPS environment. */
   /*================================*/
   
   if ([executionLock tryLock])
     {
      [focusStack release];
      focusStack = nil;
      [runningFocusStack release];
      runningFocusStack = nil;

      [factModule release];
      factModule = nil;
      [runningFactModule release];
      runningFactModule = nil;
      [factList release];
      factList = nil;
      [runningFactList release];
      runningFactList = nil;

      [instanceModule release];
      instanceModule = nil;
      [runningInstanceModule release];
      runningInstanceModule = nil;
      [instanceList release];
      instanceList = nil;
      [runningInstanceList release];
      runningInstanceList = nil;

      DestroyEnvironment(environment);
      [executionLock unlock];
     }
   else
     { NSLog(@"CLIPSEnvironment not deallocated because it was terminated"); }

   /*====================================================*/
   /* Release the objects associated with this instance. */
   /*====================================================*/
   
   [name release];
   /* TBD restore
   [focusStack release];
   [runningFocusStack release];
   [factModule release];
   [runningFactModule release];
   [factList release];
   [runningFactList release];
   */
   [executionLock release];
   [accessLock release];
   [agendaLock release];
   [factsLock release];
   [instancesLock release];

   /*=====================================*/
   /* Call the superclass dealloc method. */
   /*=====================================*/
   
   [super dealloc];
  }

/****************/
/* description: */
/****************/
- (NSString *) description
  {
   if (name != nil) return name;
   
   return [super description];
  }

/*%%%%%%%%%%%%%%%*/
/* Other Methods */
/*%%%%%%%%%%%%%%%*/

/**************/
/* doCommand: */
/**************/
- (void) doCommand: (NSString *) theCommand
  {
   char *str; 

   if ([delegate respondsToSelector: @selector(allowExecution:ofCommand:)])
     {
      if ([delegate performSelector: @selector(allowExecution:ofCommand:) 
                    withObject: self 
                    withObject: theCommand] == NO)
        { return; }
     }

   if ([delegate respondsToSelector: @selector(beforeExecution:ofCommand:)])
     {
      [delegate performSelector: @selector(beforeExecution:ofCommand:) 
                withObject: self 
                withObject: theCommand];
     } 

   if ([delegate respondsToSelector: @selector(clearCurrentCommand:)])
     {
      [delegate performSelector: @selector(clearCurrentCommand:) 
                withObject: self];
     } 
     
   FlushCommandString(environment);
   
   str = (char *) [theCommand UTF8String];
   
   EnvPrintRouter(environment,WDIALOG,str);
   AppendCommandString(environment,str);
  }

/*************************************************************/
/* performCommandIfPresent: Looks for a command and executes */
/*   it if present returning YES, otherwise returns NO.      */
/*************************************************************/
- (BOOL) performCommandIfPresent
  {
#if ! RUN_TIME
   if (CommandLineData(environment)->EvaluatingTopLevelCommand) return NO;
#endif
   
   /*===============================================================*/
   /* If a batch file is active, always pull command input from it. */
   /*===============================================================*/
   
   if (BatchActive(environment)) 
     {
      CommandLoopBatchDriver(environment);
      [self checkForChanges];
      return YES;
     }
     
   /*========================================================*/
   /* If a command is already executing, don't look for one. */
   /*========================================================*/
   
   if (! CommandCompleteAndNotEmpty(environment)) return NO;

   [NSThread detachNewThreadSelector: @selector(CommandLoopOnceThenBatchThread:) 
                  toTarget: self 
                  withObject: nil]; 

   return YES;
  }

/***********************************/
/* CommandLoopOnceThenBatchThread: */
/***********************************/
- (void) CommandLoopOnceThenBatchThread: (id) anObject 
  { 
   jmp_buf theJmpBuffer;
   int status;
   
   [accessLock lock];
   executionThread = [NSThread currentThread];
   [accessLock unlock];

   [executionLock lock];
      
   NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init]; 

   [self setValue: [NSNumber numberWithBool:YES] forKey: @"executing"];
      
   /*===============================================*/
   /* Set up the long jump so if the user issues an */
   /* exit command it will return to this location. */
   /*===============================================*/

   SetJmpBuffer(environment,&theJmpBuffer);
   
   status = setjmp(theJmpBuffer);
   
   if (status != 0)
     {
      [self setValue: [NSNumber numberWithBool: NO] forKey: @"executing"];

      [pool release]; 
   
      [executionLock unlock]; 

      [accessLock lock];
      executionThread = nil;
      [accessLock unlock];
      
      exited = TRUE;
      return;
     }
   
   /*======================*/
   /* Execute the command. */
   /*======================*/
   
   CommandLoopOnceThenBatch(environment);

   /*=========================================*/
   /* Cleanup after the command has executed. */
   /*=========================================*/
   
   SetJmpBuffer(environment,NULL);
   
   [self setValue: [NSNumber numberWithBool: NO] forKey: @"executing"];

   [pool release]; 
   
   [executionLock unlock]; 

   [accessLock lock];
   executionThread = nil;
   [accessLock unlock];
   
   return;
  } 

/**********************/
/* checkForChanges:   */
/**********************/
- (void) checkForChanges
  {
   if (agendaListenerCount > 0)
     {
      if ((EnvGetAgendaChanged(environment)) ||
          (EnvGetFocusChanged(environment)))
        { [self fetchAgenda: YES]; }
        
      if (runningFocusStack != NULL)
        { [self transferAgenda : YES]; }
     }

   if (factsListenerCount > 0)
     {
      if (EnvGetFactListChanged(environment))
        { [self fetchFacts: YES]; }
        
      if ((runningFactModule != NULL) ||
          (runningFactList != NULL))
        { [self transferFacts : YES]; }
     }

   if (instancesListenerCount > 0)
     {
      if (EnvGetInstancesChanged(environment))
        { [self fetchInstances: YES]; }
        
      if ((runningInstanceModule != NULL) ||
          (runningInstanceList != NULL))
        { [self transferInstances : YES]; }
     }
  }
  
/************/    
/* destroy: */
/************/    
- (void) destroy
  {
   /*======================================*/
   /* Get the default notification center. */
   /*======================================*/
      
   NSNotificationCenter *nc;
   nc = [NSNotificationCenter defaultCenter];
   
   /*========================================*/
   /* Notify any interested windows that the */
   /* the environment has been deallocated   */
   /*========================================*/
   
   [nc postNotificationName: @"CLIPSEnvironmentWillBeDestroyed" object: self];   
  }

/********************/
/* fetchAgenda: */
/********************/
- (void) fetchAgenda: (BOOL) lockAgenda
  {
   struct focus *theFocus;
   struct activation *theActivation;
   char *moduleName;
   NSString *theStr;
   unsigned focusCount, agendaCount;
   CLIPSFocus *newFocus;
   CLIPSActivation *newActivation;
   NSMutableArray *newAgenda;
   NSMutableDictionary *agendaDictionary;
   char bindingsBuffer[1024];
   
   /*================================================*/
   /* If nothing has changed since the last time the */
   /* agenda was fetched, nothing needs to be done.  */
   /*================================================*/
   
   if ((EnvGetAgendaChanged(environment) == FALSE) &&
       (EnvGetFocusChanged(environment) == FALSE)) return;
   
   EnvSetAgendaChanged(environment,FALSE);
   EnvSetFocusChanged(environment,FALSE);

   if (lockAgenda)
     { [agendaLock lock]; }

   /*=====================================================*/
   /* Determine the number of modules in the focus stack. */
   /*=====================================================*/
   
   focusCount = 0;
   for (theFocus = EnvGetNextFocus(environment,NULL);
        theFocus != NULL;
        theFocus = EnvGetNextFocus(environment,theFocus))
     { focusCount++; }
     
   /*============================================================*/
   /* If a controller already exists but has not be transferred, */
   /* empty and use it. Otherwise create a new controller.       */
   /*============================================================*/
   
   [runningFocusStack release];
   runningFocusStack = [[NSMutableArray alloc] initWithCapacity: focusCount];

   agendaDictionary = [[NSMutableDictionary alloc] init];

   /*=====================================*/
   /* Iterate over the CLIPS focus stack. */
   /*=====================================*/
   
   for (theFocus = EnvGetNextFocus(environment,NULL);
        theFocus != NULL;
        theFocus = EnvGetNextFocus(environment,theFocus))
     {
      /*==========================================*/
      /* Create an object to represent the focus. */
      /*==========================================*/
      
      newFocus = [[CLIPSFocus alloc] init];

      moduleName = EnvGetDefmoduleName(environment,theFocus->theModule);

      theStr = [NSString stringWithCString: moduleName encoding: NSUTF8StringEncoding];

      [newFocus setModuleName: theStr];

      /*=====================================================================*/
      /* If the agenda for this module is already in the agenda dictionary,  */
      /* add the focus to the stack controlller and move on the next module. */
      /*=====================================================================*/
         
      if ([agendaDictionary objectForKey: [newFocus moduleName]] != nil)
        { 
         newAgenda = [agendaDictionary objectForKey: [newFocus moduleName]];
         [newFocus setValue: newAgenda forKey: @"agenda"];
         [runningFocusStack addObject: newFocus];
         [newFocus release];
         continue; 
        }
        
      /*=================================*/
      /* Create an array for the agenda. */
      /*=================================*/

      agendaCount = 0;
      for (theActivation = theFocus->theDefruleModule->agenda;
           theActivation != NULL;
           theActivation = EnvGetNextActivation(environment,theActivation))
        { agendaCount++; }
     
      newAgenda = [[NSMutableArray alloc] initWithCapacity: agendaCount];
      
      /*==============================================*/
      /* Add each activation to the array controller. */
      /*==============================================*/
      
      for (theActivation = theFocus->theDefruleModule->agenda;
           theActivation != NULL;
           theActivation = EnvGetNextActivation(environment,theActivation))
        {
         newActivation  = [[CLIPSActivation alloc] init];
         
         [newActivation setActivation: theActivation];
         
         [newActivation setSalience: [NSNumber numberWithInt: theActivation->salience]];
         
         [newActivation setRuleName: [NSString stringWithFormat:@"%s", EnvGetDefruleName(environment,theActivation->theRule)]];

         EnvGetActivationBasisPPForm(environment,bindingsBuffer,1024,theActivation);
         [newActivation setBindings: [NSString stringWithFormat:@"%s", bindingsBuffer]];

         [newAgenda addObject: newActivation];
         
         [newActivation release];
        }
              
      /*=====================*/
      /* Install the agenda. */
      /*=====================*/
            
      [newFocus setValue: newAgenda forKey: @"agenda"];
      
      [agendaDictionary setValue: newAgenda forKey: [newFocus moduleName]];
      
      [newAgenda release];
      
      /*==============================================*/
      /* Add the focus to the focus stack controller. */
      /*==============================================*/
      
      [runningFocusStack addObject: newFocus];
      [newFocus release];
     }
   
   [agendaDictionary release];

   if (lockAgenda)
     { [agendaLock unlock]; }
  }

/********************/
/* fetchFacts: */
/********************/
- (void) fetchFacts: (BOOL) lockFacts
  {
   struct defmodule *theModule;
   CLIPSModule *newModule;
   char *moduleName;
   NSString *theStr;
   struct fact *theFact;
   CLIPSFactInstance *newFact;
   unsigned moduleCount, factCount;

   /*===================================================*/
   /* If nothing has changed since the last time the    */
   /* fact list was fetched, nothing needs to be done.  */
   /*===================================================*/
   
   if (EnvGetFactListChanged(environment) == FALSE) return;
   EnvSetFactListChanged(environment,FALSE);

   if (lockFacts)
     { [factsLock lock]; }

   /*=====================================*/
   /* Determine the number of modules and */
   /* create an array to contain them.    */
   /*=====================================*/
   
   moduleCount = 0;
   for (theModule = EnvGetNextDefmodule(environment,NULL);
        theModule != NULL;
        theModule = EnvGetNextDefmodule(environment,theModule))
     { moduleCount++; }

   [runningFactModule release];
   runningFactModule = [[NSMutableArray alloc] initWithCapacity: moduleCount];

   /*===================================*/
   /* Determine the number of facts and */
   /* create an array to contain them.  */
   /*===================================*/

   factCount = 0;
   for (theFact = EnvGetNextFact(environment,NULL);
        theFact != NULL;
        theFact = EnvGetNextFact(environment,theFact))
     { factCount++; }
     
   [runningFactList release];
   runningFactList = [[NSMutableArray alloc] initWithCapacity: factCount];

   /*================================*/
   /* Get a list of all the modules. */
   /*================================*/
   
   for (theModule = EnvGetNextDefmodule(environment,NULL);
        theModule != NULL;
        theModule = EnvGetNextDefmodule(environment,theModule))
     {
      newModule = [[CLIPSModule alloc] init];

      moduleName = EnvGetDefmoduleName(environment,theModule);

      theStr = [NSString stringWithCString: moduleName encoding: NSUTF8StringEncoding];

      [newModule setModuleName: theStr];

      [runningFactModule addObject: newModule];
      [newModule release];
     }
   
   /*================================*/
   /* Get the list of all the facts. */
   /*================================*/
   
   for (theFact = EnvGetNextFact(environment,NULL);
        theFact != NULL;
        theFact = EnvGetNextFact(environment,theFact))
     {
      newFact = [[CLIPSFactInstance alloc] initWithFact: theFact fromEnvironment: environment];

      [runningFactList addObject: newFact];

      [newFact release];
     }

   if (lockFacts)
     { [factsLock unlock]; }
  }

/*******************/
/* fetchInstances: */
/*******************/
- (void) fetchInstances: (BOOL) lockInstances
  {
   struct defmodule *theModule;
   CLIPSModule *newModule;
   char *moduleName;
   NSString *theStr;
   struct instance *theInstance;
   CLIPSFactInstance *newInstance;
   unsigned moduleCount, instanceCount;

   /*================================================*/
   /* If nothing has changed since the last time the */
   /* instance list was fetched, nothing needs to be */ 
   /* done.                                          */
   /*================================================*/
   
   if (EnvGetInstancesChanged(environment) == FALSE) return;
   EnvSetInstancesChanged(environment,FALSE);

   if (lockInstances)
     { [instancesLock lock]; }

   /*=====================================*/
   /* Determine the number of modules and */
   /* create an array to contain them.    */
   /*=====================================*/
   
   moduleCount = 0;
   for (theModule = EnvGetNextDefmodule(environment,NULL);
        theModule != NULL;
        theModule = EnvGetNextDefmodule(environment,theModule))
     { moduleCount++; }

   [runningInstanceModule release];
   runningInstanceModule = [[NSMutableArray alloc] initWithCapacity: moduleCount];

   /*=======================================*/
   /* Determine the number of instances and */
   /* create an array to contain them.      */
   /*=======================================*/

   instanceCount = 0;
   for (theInstance = EnvGetNextInstance(environment,NULL);
        theInstance != NULL;
        theInstance = EnvGetNextInstance(environment,theInstance))
     { instanceCount++; }
     
   [runningInstanceList release];
   runningInstanceList = [[NSMutableArray alloc] initWithCapacity: instanceCount];

   /*================================*/
   /* Get a list of all the modules. */
   /*================================*/
   
   for (theModule = EnvGetNextDefmodule(environment,NULL);
        theModule != NULL;
        theModule = EnvGetNextDefmodule(environment,theModule))
     {
      newModule = [[CLIPSModule alloc] init];

      moduleName = EnvGetDefmoduleName(environment,theModule);

      theStr = [NSString stringWithCString: moduleName encoding: NSUTF8StringEncoding];

      [newModule setModuleName: theStr];

      [runningInstanceModule addObject: newModule];
      [newModule release];
     }
   
   /*====================================*/
   /* Get the list of all the instances. */
   /*====================================*/
   
   for (theInstance = EnvGetNextInstance(environment,NULL);
        theInstance != NULL;
        theInstance = EnvGetNextInstance(environment,theInstance))
     {
      newInstance = [[CLIPSFactInstance alloc] initWithInstance: theInstance fromEnvironment: environment];

      [runningInstanceList addObject: newInstance];

      [newInstance release];
     }

   if (lockInstances)
     { [instancesLock unlock]; }
  }
  
/********************/
/* transferAgenda: */
/********************/
- (void) transferAgenda: (BOOL) lockAgenda
  {
   if (lockAgenda)
     { [agendaLock lock]; }

   if (runningFocusStack == NULL) 
     {
      if (lockAgenda)
        { [agendaLock unlock]; }
      return;
     }
      
   [self setValue: runningFocusStack forKey: @"focusStack"];

   [runningFocusStack release];
   runningFocusStack = NULL;

   [self setValue: [NSNumber numberWithLong: (agendaChanged + 1)] forKey: @"agendaChanged"];
      
   if (lockAgenda)
     { [agendaLock unlock]; }
  }

/********************/
/* transferFacts: */
/********************/
- (void) transferFacts: (BOOL) lockFacts
  {
   if (lockFacts)
     { [factsLock lock]; }

   if ((runningFactModule == NULL) &&
       (runningFactList == NULL))
     {
      if (lockFacts)
        { [factsLock unlock]; }
      return;
     }

   [self setValue: runningFactModule forKey: @"factModule"];

   [runningFactModule release];
   runningFactModule = NULL;

   [self setValue: runningFactList forKey: @"factList"];

   [runningFactList release];
   runningFactList = NULL;

   [self setValue: [NSNumber numberWithLong: (factsChanged + 1)] forKey: @"factsChanged"];
      
   if (lockFacts)
     { [factsLock unlock]; }
  }

/**********************/
/* transferInstances: */
/**********************/
- (void) transferInstances: (BOOL) lockInstances
  {
   if (lockInstances)
     { [instancesLock lock]; }

   if ((runningInstanceModule == NULL) &&
       (runningInstanceList == NULL))
     {
      if (lockInstances)
        { [instancesLock unlock]; }
      return;
     }

   [self setValue: runningInstanceModule forKey: @"instanceModule"];

   [runningInstanceModule release];
   runningInstanceModule = NULL;

   [self setValue: runningInstanceList forKey: @"instanceList"];

   [runningInstanceList release];
   runningInstanceList = NULL;

   [self setValue: [NSNumber numberWithLong: (instancesChanged + 1)] forKey: @"instancesChanged"];
      
   if (lockInstances)
     { [instancesLock unlock]; }
  }
   
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/****************/
/* setDelegate: */
/****************/
/*
- (void) setDelegate: (id) theDelegate
  {
   NSLog(@"CLIPSEnvironment setDelegate");
   [theDelegate retain];
   [delegate release];
   delegate = theDelegate;
  }
*/  
/*************/
/* delegate: */
/*************/
/*
- (id) delegate
  {
   return delegate;
  }
*/

/*********************/
/* setExecutionLock: */
/*********************/
- (void) setExecutionLock: (NSLock *) theLock
  {
   executionLock = theLock;
  }

/******************/
/* executionLock: */
/******************/
- (NSLock *) executionLock
  {
   return executionLock;
  }

/***************/
/* accessLock: */
/***************/
- (NSLock *) accessLock
  {
   return accessLock;
  }

/***************/
/* agendaLock: */
/***************/
- (NSConditionLock *) agendaLock
  {
   return agendaLock;
  }

/***************/
/* factsLock: */
/***************/
- (NSConditionLock *) factsLock
  {
   return factsLock;
  }

/******************/
/* instancesLock: */
/******************/
- (NSConditionLock *) instancesLock
  {
   return instancesLock;
  }
  
/*******************/
/* setEnvironment: */
/*******************/
- (void) setEnvironment: (void *) theEnvironment
  {
   environment = theEnvironment;
  }

/****************/
/* environment: */
/****************/
- (void *) environment
  {
   return environment;
  }

/************/
/* setName: */
/************/
- (void) setName: (NSString *) theName
  {
   [theName retain];
   [name release];
   name = theName;
  }

/*********/
/* name: */
/*********/
- (NSString *) name
  {
   return name;
  }

/*********************/
/* setAgendaChanged: */
/*********************/
- (void) setAgendaChanged: (long int) theCount
  {
   agendaChanged = theCount;
  }

/******************/
/* agendaChanged: */
/******************/
- (long int) agendaChanged
  {
   return agendaChanged;
  }

/*********************/
/* setFactsChanged: */
/*********************/
- (void) setFactsChanged: (long int) theCount
  {
   factsChanged = theCount;
  }

/******************/
/* factsChanged: */
/******************/
- (long int) factsChanged
  {
   return factsChanged;
  }

/************************/
/* setInstancesChanged: */
/************************/
- (void) setInstancesChanged: (long int) theCount
  {
   instancesChanged = theCount;
  }

/*********************/
/* instancesChanged: */
/*********************/
- (long int) instancesChanged
  {
   return instancesChanged;
  }
      
/****************************/
/* setFocusStack: */
/****************************/
- (void) setFocusStack: (NSArray *) theFocusStack
  {
   [theFocusStack retain];
   [focusStack release];
   focusStack = theFocusStack;
  }

/*************************/
/* focusStack: */
/*************************/
- (NSArray *) focusStack
  {
   return focusStack;
  }

/****************************/
/* setFactModule: */
/****************************/
- (void) setFactModule: (NSArray *) theFactModule
  {
   [theFactModule retain];
   [factModule release];
   factModule = theFactModule;
  }

/*************************/
/* factModule: */
/*************************/
- (NSArray *) factModule
  {
   return factModule;
  }

/****************************/
/* setFactList: */
/****************************/
- (void) setFactList: (NSArray *) theFactList
  {
   [theFactList retain];
   [factList release];
   factList = theFactList;
  }

/*************************/
/* factList: */
/*************************/
- (NSArray *) factList
  {
   return factList;
  }

/***********************/
/* setInstancetModule: */
/***********************/
- (void) setInstanceModule: (NSArray *) theInstanceModule
  {
   [theInstanceModule retain];
   [instanceModule release];
   instanceModule = theInstanceModule;
  }

/*******************/
/* instanceModule: */
/*******************/
- (NSArray *) instanceModule
  {
   return instanceModule;
  }

/********************/
/* setInstanceList: */
/********************/
- (void) setInstanceList: (NSArray *) theInstanceList
  {
   [theInstanceList retain];
   [instanceList release];
   instanceList = theInstanceList;
  }

/*****************/
/* instanceList: */
/*****************/
- (NSArray *) instanceList
  {
   return instanceList;
  }


/*****************/
/* setExecuting: */
/*****************/
- (void) setExecuting: (BOOL) theValue
  {
   executing = theValue;
  }

/**************/
/* executing: */
/**************/
- (BOOL) executing
  {
   return executing;
  }

/**************/
/* setExited: */
/*****************/
- (void) setExited: (BOOL) theValue
  {
   exited = theValue;
  }

/**************/
/* exited: */
/**************/
- (BOOL) exited
  {
   return exited;
  }
  
/********************/
/* executionThread: */
/********************/
- (NSThread *) executionThread
  {
   return executionThread;
  }

/*****************************/
/* incrementAgendaListeners: */
/*****************************/
- (void) incrementAgendaListeners
  {
   [agendaLock lock];
   
   agendaListenerCount++;

   /* NSLog(@"increment %@ agendaListenerCount = %d",self,agendaListenerCount); */

   if (agendaListenerCount == 1)
     { [agendaLock unlockWithCondition: START_AGENDA_LISTENING]; }
   else
     { [agendaLock unlock]; }
  }

/*****************************/
/* decrementAgendaListeners: */
/*****************************/
- (void) decrementAgendaListeners
  {
   [agendaLock lock];

   agendaListenerCount--;
   
   /* NSLog(@"decrement %@ agendaListenerCount = %d",self,agendaListenerCount); */

   if (agendaListenerCount == 0)
     { [agendaLock unlock]; }
   else
     { [agendaLock unlockWithCondition: STOP_AGENDA_LISTENING]; }
  }
  
/************************/
/* agendaListenerCount: */
/************************/
- (int) agendaListenerCount
  {
   return agendaListenerCount;
  }

/*****************************/
/* incrementFactsListeners: */
/*****************************/
- (void) incrementFactsListeners
  {
   [factsLock lock];
   
   factsListenerCount++;

   /* NSLog(@"increment %@ factsListenerCount = %d",self,factsListenerCount); */

   if (factsListenerCount == 1)
     { [factsLock unlockWithCondition: START_FACTS_LISTENING]; }
   else
     { [factsLock unlock]; }
  }

/*****************************/
/* decrementFactsListeners: */
/*****************************/
- (void) decrementFactsListeners
  {
   [factsLock lock];

   factsListenerCount--;
   
   /* NSLog(@"decrement %@ factsListenerCount = %d",self,factsListenerCount); */

   if (factsListenerCount == 0)
     { [factsLock unlock]; }
   else
     { [factsLock unlockWithCondition: STOP_FACTS_LISTENING]; }
  }
  
/************************/
/* factsListenerCount: */
/************************/
- (int) factsListenerCount
  {
   return factsListenerCount;
  }

/*****************************/
/* incrementInstancesListeners: */
/*****************************/
- (void) incrementInstancesListeners
  {
   [instancesLock lock];
   
   instancesListenerCount++;

   /* NSLog(@"increment %@ instancesListenerCount = %d",self,instancesListenerCount); */

   if (instancesListenerCount == 1)
     { [instancesLock unlockWithCondition: START_INSTANCES_LISTENING]; }
   else
     { [instancesLock unlock]; }
  }

/*****************************/
/* decrementInstancesListeners: */
/*****************************/
- (void) decrementInstancesListeners
  {
   [instancesLock lock];

   instancesListenerCount--;
   
   /* NSLog(@"decrement %@ factsListenerCount = %d",self,factsListenerCount); */

   if (instancesListenerCount == 0)
     { [instancesLock unlock]; }
   else
     { [instancesLock unlockWithCondition: STOP_INSTANCES_LISTENING]; }
  }
  
/************************/
/* instancesListenerCount: */
/************************/
- (int) instancesListenerCount
  {
   return instancesListenerCount;
  }
      
@end

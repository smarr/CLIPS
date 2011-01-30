//
//  CLIPSAgendaController.m
//  CLIPS
//
//  Created by Gary Riley on 3/10/06.
//

#import "CLIPSAgendaController.h"

#import "AppController.h"
#import "EnvController.h"
#import "CLIPSEnvironment.h"
#import "CLIPSActivation.h"

#include <core/clips.h>

@implementation CLIPSAgendaController

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Initialization/Deallocation Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*********/
/* init: */
/*********/
- (id) init
  {
   self = [super initWithWindowNibName:@"CLIPSAgendaBrowser"];

   if (self)
     {
     }
     
   return self;
  }

/************/    
/* dealloc: */
/************/    
- (void) dealloc
  {
   NSLog(@"CLIPSAgendaController dealloc");
   
   [super dealloc];
  }

/*****************/
/* awakeFromNib: */
/*****************/
- (void) awakeFromNib
  {
   NSArrayController *theArrayController;
   NSMutableDictionary *bindingOptions = [NSMutableDictionary dictionary];
   
   /*=====================================================================*/
   /* Create the binding for the environment displayed in the popup menu. */
   /*=====================================================================*/
   
   [bindingOptions setObject:@"Unattached" forKey:@"NSNullPlaceholder"];
   [bindingOptions setObject: [NSNumber numberWithBool:YES] forKey:@"NSInsertsNullPlaceholder"];

   [environmentList bind: @"content" 
                    toObject: [[[NSApp delegate] envController] environmentArrayController]
                    withKeyPath: @"arrangedObjects"
                    options: bindingOptions];

   /*=============================================================*/
   /* Locate and assign the application's environment controller. */
   /*=============================================================*/
  
   [self setEnvironmentController: [[NSApp delegate] envController]];
  
   /*====================================================================*/
   /* Determine the environment to which this window should be attached. */
   /*====================================================================*/
    
   theArrayController 
      = [[[NSApp delegate] envController] environmentArrayController];
      
   NSArray *theArray;
   NSUInteger theIndex;

   theArray = [theArrayController arrangedObjects];
   theIndex = [theArrayController selectionIndex]; 
   
   if (theIndex != NSNotFound)
     { [self setValue: [theArray objectAtIndex: theIndex] forKey: @"environment"]; }
   else if ([theArray count] != 0)
     { [self setValue: [theArray objectAtIndex: 0] forKey: @"environment"]; }
   else
     { [self setValue: nil forKey: @"environment"]; }
          
   [self setValue: [NSNumber numberWithInt: 10] forKey: @"fontSize"]; 
   [self setValue: [NSNumber numberWithInt: 13] forKey: @"rowHeight"]; 

   /*=================================================*/
   /* This setting for this attribute isn't preserved */
   /* when set in Interface Builder.                  */
   /*=================================================*/
   
   [executionIndicator setDisplayedWhenStopped: NO];
        
   /*==========================================================*/
   /* If we can get the execution lock, then the environment   */
   /* isn't executing, so we can directly retrieve the agenda. */
   /*==========================================================*/
   
   if ([[environment executionLock] tryLock]) 
     {
      [environment fetchAgenda: YES];
      [environment transferAgenda: YES];
      [runButton setEnabled: YES];
      [resetButton setEnabled: YES];
      [stepButton setEnabled: YES]; 
      [haltButton setEnabled: NO]; 
      [[environment executionLock] unlock];
     }
   else
     {
      [runButton setEnabled: NO];
      [resetButton setEnabled: NO];
      [stepButton setEnabled: NO];
      [haltButton setEnabled: YES]; 
      [executionIndicator startAnimation: nil];
     }
  }

/**************************************************/
/* observeValueForKeyPath: */
/**************************************************/
- (void) observeValueForKeyPath: (NSString *) keyPath 
                       ofObject: (id) object 
                         change: (NSDictionary *) change 
                        context: (void *) context 
  {
   if ([keyPath isEqual:@"agendaChanged"])
     {
      [focusStack selectRowIndexes: [NSIndexSet indexSetWithIndex: 0] byExtendingSelection: NO];
      [agendaList selectRowIndexes: [NSIndexSet indexSetWithIndex: 0] byExtendingSelection: NO];
      [self updateAgendaInspectorText];
     }
   else if ([keyPath isEqual:@"executing"])
     { 
      if ([[change valueForKey: NSKeyValueChangeKindKey] intValue] == NSKeyValueChangeSetting)
        {
         if ([[change valueForKey: NSKeyValueChangeNewKey] intValue])
           { 
            [runButton setEnabled: NO];
            [resetButton setEnabled: NO];
            [stepButton setEnabled: NO];
            [haltButton setEnabled: YES]; 
            [executionIndicator startAnimation: nil]; 
           }
         else
           { 
            [runButton setEnabled: YES];
            [resetButton setEnabled: YES];
            [stepButton setEnabled: YES];
            [haltButton setEnabled: NO]; 
            [executionIndicator stopAnimation: nil]; 
           }
        }
        
      [self updateAgendaInspectorText];
     }
  }
   
/**************************************************/
/* splitView:constraintMinCoordinate:ofSubviewAt: */
/**************************************************/
- (float) splitView: (NSSplitView *) sender 
          constrainMinCoordinate: (float) proposedMin 
          ofSubviewAt: (int) offset
  {
   return 100.0;
  }

/**************************************************/
/* splitView:constraintMaxCoordinate:ofSubviewAt: */
/**************************************************/
- (float) splitView: (NSSplitView *) sender 
          constrainMaxCoordinate: (float) proposedMax 
          ofSubviewAt: (int) offset
  {
   return [sender bounds].size.width - 200.0;
  }

/*********************************/
/* splitView:canCollapseSubview: */
/*********************************/
- (BOOL) splitView: (NSSplitView *) sender 
         canCollapseSubview: (NSView *) subview
  {
   return YES;
  }

/*%%%%%%%%%%%%%%%%*/
/* Action Methods */
/*%%%%%%%%%%%%%%%%*/

/***********************************/  
/* reset: Handle the reset button. */
/***********************************/  
- (IBAction) reset: (id) sender
  {
   /*======================================================*/
   /* Disable the browser buttons. These will be reenabled */
   /* when the environment finishes executing or changes.  */
   /*======================================================*/
   
   [runButton setEnabled: NO];
   [resetButton setEnabled: NO];
   [stepButton setEnabled: NO]; 
   [haltButton setEnabled: NO]; 
   
   /*======================================*/
   /* Send the command to the environment. */
   /*======================================*/

   [environment doCommand: @"(reset)\n"];
  }
  
/*******************************/  
/* run: Handle the run button. */
/*******************************/  
- (IBAction) run: (id) sender
  {
   /*======================================================*/
   /* Disable the browser buttons. These will be reenabled */
   /* when the environment finishes executing or changes.  */
   /*======================================================*/
   
   [runButton setEnabled: NO];
   [resetButton setEnabled: NO];
   [stepButton setEnabled: NO]; 
   [haltButton setEnabled: NO]; 
   
   /*======================================*/
   /* Send the command to the environment. */
   /*======================================*/

   [environment doCommand: @"(run)\n"];
  }
  
/**********************************/  
/* step: Handles the step button. */
/**********************************/  
- (IBAction) step: (id) sender
  {
   /*======================================================*/
   /* Disable the browser buttons. These will be reenabled */
   /* when the environment finishes executing or changes.  */
   /*======================================================*/
   
   [runButton setEnabled: NO];
   [resetButton setEnabled: NO];
   [stepButton setEnabled: NO]; 
   [haltButton setEnabled: NO]; 
   
   /*======================================*/
   /* Send the command to the environment. */
   /*======================================*/
   
   [environment doCommand: @"(run 1)\n"];
  }

/****************/  
/* showDefrule: */
/****************/  
- (IBAction) showDefrule: (id) sender
  {
   NSLog(@"CLIPSAgendaController showDefrule");
  }
 
/*********************/    
/* halt: */
/*********************/    
- (IBAction) halt: (id) sender
  {
   NSLog(@"CLIPSAgendaController halt");
   EnvSetHaltRules([environment environment],TRUE);
  }

/*********************/    
/* haltImmediately: */
/*********************/    
- (IBAction) haltImmediately: (id) sender
  {
   NSLog(@"CLIPSAgendaController haltImmediately");
   /* TBD Need to abort waitForChar */
   /* TBD Need to abort batch */
   SetHaltCommandLoopBatch([environment environment],TRUE);
   SetHaltExecution([environment environment],TRUE);
  }
 
/*%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Window Delegate Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%*/

/********************/
/* validateMenuItem: */
/********************/
- (BOOL) validateMenuItem: (NSMenuItem *) menuItem
  {
   /*===================================================*/
   /* The Halt and Halt Immediately menu items are only */
   /* available if the CLIPS environment is executing.  */
   /*===================================================*/
   
   if (([menuItem action] == @selector(halt:)) ||
       ([menuItem action] == @selector(haltImmediately:)))
     {
      if (environment == nil)
        { return NO; }
        
      if ([[environment executionLock] tryLock])
        {
         [[environment executionLock] unlock];
         return NO;
        }
      else
        { return YES; }
     }

   if ([menuItem action] == @selector(showDefrule:))
     {
      if ([agendaList selectedRow] == -1) 
        { return NO; } 
     }
     
   return YES;
  }
  
/********************/
/* windowWillClose: */
/********************/
- (void) windowWillClose: (NSNotification *) aNotification
  {
   NSLog(@"CLIPSAgendaController windowWillClose:");
   
   [self setValue: nil forKey: @"environment"]; 
   [self setValue: nil forKey: @"environmentController"]; 

   [self autorelease];
  }

/**************************************************/
/* tableViewSelectionDidChange: Called when the   */
/*   selection changes in the agenda NSTableView. */
/**************************************************/
- (void) tableViewSelectionDidChange: (NSNotification *) aNotification
  {
   if (([aNotification object] == agendaList) ||
       ([aNotification object] == focusStack))
     {
      [self updateAgendaInspectorText];
     }
  }

/**************************************************/
/* updateAgendaInspectorText:  */
/**************************************************/
- (void) updateAgendaInspectorText
  {
   int theRow = [agendaList selectedRow];

   /*===============================================*/
   /* If the environment is executing, don't update */
   /* the construct inspector since the activation  */
   /* may be stale.                                 */
   /*===============================================*/

   if (! [[environment executionLock] tryLock]) 
     { return; }
   [[environment executionLock] unlock];

   if (theRow == -1)
     { [environmentController setValue: nil forKey: @"constructInspectorText"]; }
   else
     {   
      void *theEnvironment = [environment environment];
      NSArray *theArray = [focusStackController valueForKeyPath: @"selection.agenda"];
         
      struct activation *theActivation = [[theArray objectAtIndex: theRow] activation];
         
      NSString *thePPForm = [NSString stringWithUTF8String: EnvGetDefrulePPForm(theEnvironment,theActivation->theRule)];

      [environmentController setValue: thePPForm forKey: @"constructInspectorText"];
     }
  }

/*%%%%%%%%%%%%%%%%%%%%%%*/
/* Notification Methods */
/*%%%%%%%%%%%%%%%%%%%%%%*/

/*********************************/
/* targetEnvironmentDeallocated: */
/*********************************/
- (void) targetEnvironmentDeallocated: (NSNotification *) note
  {
   [self setValue: nil forKey: @"environment"];
  }

/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
  
/*****************************/
/* setEnvironmentController: */
/*****************************/
- (void) setEnvironmentController: (EnvController *) theController
  {
   [theController retain];
   [environmentController release];
   environmentController = theController;
  }

/**************************/
/* environmentController: */
/**************************/
- (EnvController *) environmentController
  {
   return environmentController;
  }

/*******************/
/* setEnvironment: */
/*******************/
- (void) setEnvironment: (CLIPSEnvironment *) theEnvironment
  {
   /*=============================*/
   /* Retain the new environment. */
   /*=============================*/
   
   [theEnvironment retain];

   /*=====================================*/
   /* Stop observing the old environment. */
   /*=====================================*/
   
   NSNotificationCenter  *nc;
   nc = [NSNotificationCenter defaultCenter];
   
   if (environment != nil)
     {
      [nc removeObserver: self
          name:@"CLIPSEnvironmentWillBeDestroyed"
          object: environment];
          
      [environment removeObserver: self 
                       forKeyPath: @"agendaChanged"]; 

      [environment removeObserver: self 
                       forKeyPath: @"executing"]; 
                    
      [environment decrementAgendaListeners];
     }

   /*======================================*/
   /* Start observing the new environment. */
   /*======================================*/

   if (theEnvironment != nil)
     {
      [nc addObserver: self 
          selector: @selector(targetEnvironmentDeallocated:)
          name: @"CLIPSEnvironmentWillBeDestroyed"
          object: theEnvironment];
          
      [theEnvironment addObserver: self 
                      forKeyPath: @"agendaChanged" 
                      options: (NSKeyValueObservingOptionNew | 
                                NSKeyValueObservingOptionOld) 
                      context: nil]; 

      [theEnvironment addObserver: self 
                      forKeyPath: @"executing" 
                      options: (NSKeyValueObservingOptionNew | 
                                NSKeyValueObservingOptionOld) 
                      context: nil]; 
            
      /*=================================================*/
      /* Fetch the agenda if no other agenda controllers */
      /* are attached to the environment.                */
      /*=================================================*/
   
      if ([theEnvironment agendaListenerCount] == 0)
        {
         /*==========================================================*/
         /* If we can get the execution lock, then the environment   */
         /* isn't executing, so we can directly retrieve the agenda. */
         /*==========================================================*/

         if ([[theEnvironment executionLock] tryLock]) 
           {
            [theEnvironment fetchAgenda: YES];
            [theEnvironment transferAgenda: YES];
            [[theEnvironment executionLock] unlock];
           }
        }

      /*===============================================*/
      /* If the new environment is not executing, then */
      /* the reset/run/step buttons are available,     */
      /* otherwise they aren't.                        */
      /*===============================================*/
      
      if ([[theEnvironment executionLock] tryLock]) 
        {
         [runButton setEnabled: YES];
         [resetButton setEnabled: YES];
         [stepButton setEnabled: YES]; 
         [haltButton setEnabled: NO]; 
         [executionIndicator stopAnimation: nil];  
         [[theEnvironment executionLock] unlock];
        }
      else
        {
         [runButton setEnabled: NO];
         [resetButton setEnabled: NO];
         [stepButton setEnabled: NO]; 
         [haltButton setEnabled: YES]; 
         [executionIndicator startAnimation: nil];
        }
      
      /*=========================================================*/
      /* Increment the count of agenda controllers watching the  */
      /* specified enviroment. For performance, the agenda isn't */
      /* retrieved unless there are controllers interested in    */
      /* the content of the agenda.                              */
      /*=========================================================*/
      
      [theEnvironment incrementAgendaListeners];
     }
   
   /*=================================================================*/
   /* Otherwise, the agenda browser isn't attached to an environment. */
   /* The buttons and execution indicator should be disabled.         */
   /*=================================================================*/
   
   else
     {
      [runButton setEnabled: NO];
      [resetButton setEnabled: NO];
      [stepButton setEnabled: NO];         
      [haltButton setEnabled: NO]; 
      [executionIndicator stopAnimation: nil];  
     }

   /*=============================*/
   /* Release the old environment */
   /* and assign the new value.   */
   /*=============================*/
   
   [environment release];
   environment = theEnvironment;
  }

/****************/
/* environment: */
/****************/
- (CLIPSEnvironment *) environment
  {
   return environment;
  }

@end

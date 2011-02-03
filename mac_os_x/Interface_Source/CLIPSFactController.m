//
//  CLIPSFactController.m
//  CLIPS
//
//  Created by Gary Riley on 3/10/06.
//

#import "CLIPSFactController.h"

#import "AppController.h"
#import "EnvController.h"
#import "CLIPSEnvironment.h"
#import "CLIPSFactInstance.h"
#import "ModuleArrayController.h"

#include <core/clips.h>

@implementation CLIPSFactController

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Initialization/Deallocation Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*********/
/* init: */
/*********/
- (id) init
  {
   self = [super initWithWindowNibName:@"CLIPSFactBrowser"];

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
   NSLog(@"CLIPSFactController dealloc");
   
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
   
   /*=====================================================*/
   /* Initialize with the last setting the user chose for */
   /* whether defaulted values should be displayed.       */
   /*=====================================================*/
    
   NSUserDefaults *theValues;
   
   theValues = [[NSUserDefaultsController sharedUserDefaultsController] values];
   
   if ([[theValues valueForKey: @"factsDisplayDefaultedValues"] boolValue]) 
     { 
      [self setValue: nil forKey: @"slotFilter"]; 
      [displayDefaultedValuesButton setState: NSOnState];
     }
   else
     {
      NSPredicate *predicate = [NSPredicate predicateWithFormat: @"slotDefault = TRUE"]; 
      [self setValue: predicate forKey: @"slotFilter"]; 
      [displayDefaultedValuesButton setState: NSOffState];
     }

   /*=================================================*/
   /* This setting for this attribute isn't preserved */
   /* when set in Interface Builder.                  */
   /*=================================================*/
   
   [executionIndicator setDisplayedWhenStopped: NO];
        
   /*=========================================================*/
   /* If we can get the execution lock, then the environment  */
   /* isn't executing, so we can directly retrieve the facts. */
   /*=========================================================*/
   
   if ([[environment executionLock] tryLock]) 
     {
      [environment fetchFacts: YES];
      [environment transferFacts: YES];
      [[environment executionLock] unlock];
     }
   else
     {
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
   if ([keyPath isEqual:@"factsChanged"])
     {
      [moduleList selectRowIndexes: [NSIndexSet indexSetWithIndex: 0] byExtendingSelection: NO];
      [factList selectRowIndexes: [NSIndexSet indexSetWithIndex: 0] byExtendingSelection: NO];
     }
   else if ([keyPath isEqual:@"executing"])
     { 
      if ([[change valueForKey: NSKeyValueChangeKindKey] intValue] == NSKeyValueChangeSetting)
        {
         if ([[change valueForKey: NSKeyValueChangeNewKey] intValue])
           { [executionIndicator startAnimation: nil]; }
         else
           { [executionIndicator stopAnimation: nil]; }
        }
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

/******************************************************************/
/* displayDefaultedValues: Handles the "Display Defaulted Values" */
/*   checkbox. If checked, then all of the slot values of a fact  */
/*   are displayed by the inspector. If unchecked, then the slot  */
/*   values that are the same as the static default for the slot  */
/*   are not displayed.                                           */
/******************************************************************/
- (IBAction) displayDefaultedValues: (id) sender
  {
   NSUserDefaults *theValues;
   NSNumber *displayValue;
   NSUserDefaultsController *theDefaultsController;
   
   theDefaultsController = [NSUserDefaultsController sharedUserDefaultsController];
   
   theValues = [theDefaultsController values];

   if ([sender state])
     { 
      [self setValue: nil forKey: @"slotFilter"]; 
      displayValue = [NSNumber numberWithBool:YES];
     }
   else
     {
      NSPredicate *predicate = [NSPredicate predicateWithFormat: @"slotDefault = TRUE"]; 
      [self setValue: predicate forKey: @"slotFilter"]; 
      displayValue = [NSNumber numberWithBool:NO];
     }
     
   [theValues setValue: displayValue forKey: @"factsDisplayDefaultedValues"];
   [theDefaultsController save: self];
  }

/****************/  
/* search: */
/****************/  
- (IBAction) search: (id) sender
 {
  NSLog(@"CLIPSFactController search");
  [factListController search: sender];
/*
  [self setSearchString: [sender stringValue]];
  [self rearrangeObjects];
*/
 } 
  
/*%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Window Delegate Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%*/

/********************/
/* validateMenuItem: */
/********************/
- (BOOL) validateMenuItem: (NSMenuItem *) menuItem
  {
   if ([menuItem action] == @selector(showDefrule:))
     {
      if ([factList selectedRow] == -1) 
        { return NO; } 
     }
     
   return YES;
  }
  
/********************/
/* windowWillClose: */
/********************/
- (void) windowWillClose: (NSNotification *) aNotification
  {
   NSLog(@"CLIPSFactController windowWillClose:");
   
   [self setValue: nil forKey: @"environment"]; 
   [self setValue: nil forKey: @"environmentController"]; 

   [self autorelease];
  }

/************************************************/
/* tableViewSelectionDidChange: Called when the */
/*   selection changes in the fact NSTableView. */
/************************************************/
- (void) tableViewSelectionDidChange: (NSNotification *) aNotification
  {
   int theRow;
   NSString *thePPForm = nil;
   
   if ([aNotification object] == moduleList)
     {
      theRow = [moduleList selectedRow];

      [factListController setModuleIndex: theRow];
     }
     
   theRow = [factList selectedRow];
   if (theRow != -1)
     {
      void *theEnvironment = [environment environment];
      EXEC_STATUS = [environment executionStatus];
      struct fact *clipsFact;
      long long theFactIndex;
      NSArray *theArray = [factListController arrangedObjects];
      CLIPSFactInstance *theFact = [theArray objectAtIndex: theRow];
      
      /*============================================================*/
      /* Use the fact index stored with the GUI fact object to find */
      /* the actual CLIPS fact referenced. TBD: It would be more    */
      /* efficient to directly store the pointer to the fact with   */
      /* the GUI fact object.                                       */
      /*============================================================*/
      
      theFactIndex = [[theFact index] longLongValue];
      clipsFact = FindIndexedFact(theEnvironment,execStatus,theFactIndex);
      
      /*========================================================*/
      /* If we were able to find the corresponding CLIPS fact,  */
      /* then retrieve the pretty print form of the deftemplate */
      /* associated with the fact.                              */
      /*========================================================*/
      
      if ((clipsFact != NULL) && 
          EnvGetDeftemplatePPForm(theEnvironment,execStatus,EnvFactDeftemplate(theEnvironment,execStatus,clipsFact)) != NULL)
        { thePPForm = [NSString stringWithUTF8String: EnvGetDeftemplatePPForm(theEnvironment,execStatus,EnvFactDeftemplate(theEnvironment,execStatus,clipsFact))]; }
     }
     
   [environmentController setValue: thePPForm forKey: @"constructInspectorText"];
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
                       forKeyPath: @"factsChanged"]; 

      [environment removeObserver: self 
                       forKeyPath: @"executing"]; 
                    
      [environment decrementFactsListeners];
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
                      forKeyPath: @"factsChanged" 
                      options: (NSKeyValueObservingOptionNew | 
                                NSKeyValueObservingOptionOld) 
                      context: nil]; 

      [theEnvironment addObserver: self 
                      forKeyPath: @"executing" 
                      options: (NSKeyValueObservingOptionNew | 
                                NSKeyValueObservingOptionOld) 
                      context: nil]; 
            
      /*==================================================*/
      /* Fetch the fact list if no other fact controllers */
      /* are attached to the environment.                 */
      /*==================================================*/
   
      if ([theEnvironment factsListenerCount] == 0)
        {
         /*========================================*/
         /* If we can get the execution lock, then */
         /* the environment isn't executing, so we */
         /* can directly retrieve the fact list.   */
         /*========================================*/

         if ([[theEnvironment executionLock] tryLock]) 
           {
            [theEnvironment fetchFacts: YES];
            [theEnvironment transferFacts: YES];
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
         [executionIndicator stopAnimation: nil];  
         [[theEnvironment executionLock] unlock];
        }
      else
        {
         [executionIndicator startAnimation: nil];
        }
      
      /*======================================================*/
      /* Increment the count of fact controllers watching the */
      /* specified enviroment. For performance, the fact list */
      /* isn't retrieved unless there are controllers         */
      /* interested in the content of the fact list.          */
      /*======================================================*/
      
      [theEnvironment incrementFactsListeners];
     }
   
   /*===============================================================*/
   /* Otherwise, the fact browser isn't attached to an environment. */
   /* The buttons and execution indicator should be disabled.       */
   /*===============================================================*/
   
   else
     {
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

/*****************************/
/* setSlotFilter: */
/*****************************/
- (void) setSlotFilter: (NSPredicate *) theSlotFilter
  {
   [theSlotFilter retain];
   [slotFilter release];
   slotFilter = theSlotFilter;
  }

/**************************/
/* slotFilter: */
/**************************/
- (NSPredicate *) slotFilter
  {
   return slotFilter;
  }
  
@end

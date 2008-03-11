//
//  PreferenceController.m
//  CLIPS
//
//  Created by Gary Riley on 2/25/06.
//

#import "PreferenceController.h"

@implementation PreferenceController

/***************/
/* initialize: */
/***************/
+ (void) initialize
  { 
   /*======================================================*/
   /* Create and register the font name value transformer. */
   /* In the Preferences panel, the font name value will   */
   /* be converted to the display name for the font.       */
   /*======================================================*/
    
   NSValueTransformer *transformer = [[FontNameToDisplayNameTransformer alloc] init];
   [NSValueTransformer setValueTransformer:transformer forName:@"FontNameToDisplayNameTransformer"];
  } 

/*********/
/* init: */
/*********/
- (id) init
  {
   self = [super initWithWindowNibName:@"Preferences"];
   //NSLog(@"PreferencesController init self = %@", self);
   //NSLog(@"PreferencesController init window = %@", [self window]);
   return self;
  }

/******************/
/* awakeFromNib: */
/******************/
- (void) awakeFromNib
  {
   //NSLog(@"PreferencesController awakeFromNib window = %@", [self window]);
  }

/******************/
/* windowDidLoad: */
/******************/
- (void) windowDidLoad
  {
   //NSLog(@"PreferencesController windowDidLoad window = %@", [self window]);
  }
  
/**************/
/* showPanel: */
/**************/
- (void) showPanel
  {
   //NSLog(@"PreferenceController showPanel");
   
   NSUserDefaultsController *theDefaultsController;
   NSWindow *panel = [self window];
   
   [panel setHidesOnDeactivate:NO];
   [panel setExcludedFromWindowsMenu:YES];
   [panel setMenu:nil];
   [panel center];

   /*============================================================================*/
   /* One of the undocumented effects of setAppliesImmediately is that it marks  */
   /* the defaults as having unapplied changes even though they were just loaded */
   /* and the user has yet to change anything. As a result, the defaults need to */
   /* be saved so that it can be correctly determined when the user has actually */
   /* made a change.                                                             */
   /*============================================================================*/ 
      
   theDefaultsController = [NSUserDefaultsController sharedUserDefaultsController];
   [theDefaultsController setAppliesImmediately: NO];
   [theDefaultsController save: self];
     
   /*====================*/
   /* Display the panel. */
   /*====================*/
        
   [panel makeKeyAndOrderFront:nil];
  }
    
/*************************************************************************/  
/* changeFont: Handles the changeFont action sent from the font manager. */
/*   Relies on the following connections:                                */
/*   1) The delegate of the Preferences panel (from Preferences.nib)     */
/*      should be the File's Owner (the Preferences class).              */
/*************************************************************************/  
- (void) changeFont: (id) fontManager
  {
   NSUserDefaults *theValues;
   NSFont *selectedFont, *panelFont;
   
   selectedFont = [fontManager selectedFont];
   if (selectedFont == nil)
	 { selectedFont = [NSFont userFixedPitchFontOfSize:0.0]; }

   panelFont = [fontManager convertFont:selectedFont];
   
   if (panelFont != nil)
     { 
      theValues = [[NSUserDefaultsController sharedUserDefaultsController] values];
      [theValues setValue: [panelFont fontName] forKey: @"editorTextFontName"];
      [theValues setValue: [NSNumber numberWithFloat: [panelFont pointSize]] forKey: @"editorTextFontSize"];
     }
  }

/*********************************************************************/
/* changeEditorFont: Handles the action generated when the Change... */
/*   button is clicked in the Editor Preferences tab.                */
/*********************************************************************/
- (void) changeEditorFont: (id) sender
  {
   NSFontManager *fontManager;
   NSFont *selectedFont;
   NSUserDefaults *theValues;
   NSWindow *panel = [self window];
   
   /*============================*/
   /* Retrieve the font manager. */
   /*============================*/
   
   fontManager = [NSFontManager sharedFontManager];
      
   /*==============================*/
   /* Make it the first responder. */
   /*==============================*/
   
   [panel makeFirstResponder:panel];

   /*======================================*/
   /* First determine if a font is already */
   /* selected in the font manager.        */
   /*======================================*/
   
   selectedFont = [fontManager selectedFont];
   
   /*==============================================*/
   /* If no font is selected, try finding the font */
   /* specified in the user's preferences.         */
   /*==============================================*/
   
   if (selectedFont == nil)
     {
      theValues = [[NSUserDefaultsController sharedUserDefaultsController] values];
      
      selectedFont = [NSFont fontWithName: [theValues valueForKey: @"editorTextFontName"] 
                             size: [[theValues valueForKey: @"editorTextFontSize"] floatValue]];
     }
   
   /*=================================*/
   /* If all else fails, just use the */
   /* default fixed width font.       */
   /*=================================*/
   
   if (selectedFont == nil)
	 { selectedFont = [NSFont userFixedPitchFontOfSize:0.0]; }
     
   /*============================================*/
   /* Set the selected font in the font manager. */
   /*============================================*/
        
   if (selectedFont != nil)
     { [fontManager setSelectedFont: selectedFont isMultiple:NO]; }

   /*======================================*/
   /* Bring the font manager to the front. */
   /*======================================*/
      
   [fontManager orderFrontFontPanel:self];
  }
  
/*************/
/* watchAll: */
/*************/
- (IBAction) watchAll: (id) sender
  {
   NSUserDefaults *theValues;
   NSNumber *watchValue;

   //NSLog(@"watchAll:");
   
   theValues = [[NSUserDefaultsController sharedUserDefaultsController] values];
   watchValue = [NSNumber numberWithBool:YES];
   
   [theValues setValue: watchValue forKey: @"watchCompilations"];
   [theValues setValue: watchValue forKey: @"watchFacts"];
   [theValues setValue: watchValue forKey: @"watchRules"];
   [theValues setValue: watchValue forKey: @"watchStatistics"];
   [theValues setValue: watchValue forKey: @"watchActivations"];
   [theValues setValue: watchValue forKey: @"watchFocus"];
   [theValues setValue: watchValue forKey: @"watchGlobals"];
   [theValues setValue: watchValue forKey: @"watchDeffunctions"];
   [theValues setValue: watchValue forKey: @"watchGenericFunctions"];
   [theValues setValue: watchValue forKey: @"watchMethods"];
   [theValues setValue: watchValue forKey: @"watchInstances"];
   [theValues setValue: watchValue forKey: @"watchSlots"];
   [theValues setValue: watchValue forKey: @"watchMessageHandlers"];
   [theValues setValue: watchValue forKey: @"watchMessages"];
  }
  
/*************/
/* watchNone: */
/*************/
- (IBAction) watchNone: (id) sender
  {
   NSUserDefaults *theValues;
   NSNumber *watchValue;

   //NSLog(@"watchNone:");
   
   theValues = [[NSUserDefaultsController sharedUserDefaultsController] values];
   watchValue = [NSNumber numberWithBool:NO];
   
   [theValues setValue: watchValue forKey: @"watchCompilations"];
   [theValues setValue: watchValue forKey: @"watchFacts"];
   [theValues setValue: watchValue forKey: @"watchRules"];
   [theValues setValue: watchValue forKey: @"watchStatistics"];
   [theValues setValue: watchValue forKey: @"watchActivations"];
   [theValues setValue: watchValue forKey: @"watchFocus"];
   [theValues setValue: watchValue forKey: @"watchGlobals"];
   [theValues setValue: watchValue forKey: @"watchDeffunctions"];
   [theValues setValue: watchValue forKey: @"watchGenericFunctions"];
   [theValues setValue: watchValue forKey: @"watchMethods"];
   [theValues setValue: watchValue forKey: @"watchInstances"];
   [theValues setValue: watchValue forKey: @"watchSlots"];
   [theValues setValue: watchValue forKey: @"watchMessageHandlers"];
   [theValues setValue: watchValue forKey: @"watchMessages"];
  }

/************/
/* doApply: */
/************/
- (IBAction) doApply: (id) sender
  {
   NSUserDefaultsController *theDefaultsController;
   
   theDefaultsController = [NSUserDefaultsController sharedUserDefaultsController];

   [theDefaultsController save: self];
  }

/**********************/
/* windowShouldClose: */
/**********************/  
- (BOOL) windowShouldClose: (id) sender
  {
   NSUserDefaultsController *theDefaultsController;

   theDefaultsController = [NSUserDefaultsController sharedUserDefaultsController];

   if (! [theDefaultsController hasUnappliedChanges])
     { return YES; }
           
   NSBeginAlertSheet(@"Do you want to save changes to your Preferences?",
                      @"Save", @"Don't Save", @"Cancel",
                      [self window],self,                  
                      @selector(sheetDidEndShouldClose:returnCode:contextInfo:),
                      NULL,sender,@"",nil);
                         
   return NO;
  }
  
/***************************************************/
/* sheetDidEndShouldClose:returnCode:contextInfo: */
/***************************************************/
- (void) sheetDidEndShouldClose: (NSWindow *) sheet
         returnCode: (int) returnCode
         contextInfo: (void *) contextInfo
  {
   NSUserDefaultsController *theDefaultsController;
   NSWindow *panel = [self window];

   theDefaultsController = [NSUserDefaultsController sharedUserDefaultsController];

   if (returnCode == NSAlertDefaultReturn) // Save
     { 
      [theDefaultsController save: self];
      [panel close];
     }
   else if (returnCode == NSAlertOtherReturn) // Cancel
     { 
      [sheet orderOut: nil];
      [panel makeKeyAndOrderFront: nil]; 
      }
   else if (returnCode == NSAlertAlternateReturn)
     { 
      [theDefaultsController revert: self];
      [panel close];
     }
  }
  
/************/
/* doCancel: */
/************/
- (IBAction) doCancel: (id) sender
  {
   NSUserDefaultsController *theDefaultsController;
   
   theDefaultsController = [NSUserDefaultsController sharedUserDefaultsController];

   [theDefaultsController revert: self];
   
   [[self window] close];
  }
  
/*********/
/* doOK: */
/*********/
- (IBAction) doOK: (id) sender
  {
   NSUserDefaultsController *theDefaultsController;
   
   theDefaultsController = [NSUserDefaultsController sharedUserDefaultsController];

   [theDefaultsController save: self];
   
   [[self window] close];
  }

/************************************/
/* reviewPreferencesBeforeQuitting: */
/************************************/  
- (NSApplicationTerminateReply) reviewPreferencesBeforeQuitting
  {
   NSUserDefaultsController *theDefaultsController;
      
   theDefaultsController = [NSUserDefaultsController sharedUserDefaultsController];

   if (! [theDefaultsController hasUnappliedChanges])
     { return NSTerminateNow; }
           
   NSBeginAlertSheet(@"Do you want to save changes to your Preferences?",
                      @"Save", @"Don't Save", @"Cancel",
                      [self window],self,                  
                      @selector(sheetDidEndShouldQuit:returnCode:contextInfo:),
                      NULL,self,@"",nil);
                         
   return NSTerminateLater;  
  }

/*************************************************/
/* sheetDidEndShouldQuit:returnCode:contextInfo: */
/*************************************************/
- (void) sheetDidEndShouldQuit: (NSWindow *) sheet
         returnCode: (int) returnCode
         contextInfo: (void *) contextInfo
  {
   NSUserDefaultsController *theDefaultsController;
   NSWindow *panel = [self window];
      
   theDefaultsController = [NSUserDefaultsController sharedUserDefaultsController];

   if (returnCode == NSAlertDefaultReturn) // Save
     { 
      [panel close];
      [theDefaultsController save: self];
      [theDefaultsController setAppliesImmediately: YES]; // Saving does not appear to work reliably
      [theDefaultsController setAppliesImmediately: NO];  // Hence the need to force the save
      [NSApp replyToApplicationShouldTerminate: YES];
     }
   else if (returnCode == NSAlertOtherReturn) // Cancel
     { 
      [sheet orderOut: nil];
      [panel makeKeyAndOrderFront: nil]; 
      [NSApp replyToApplicationShouldTerminate: NO];
     }
   else if (returnCode == NSAlertAlternateReturn)
     { 
      [theDefaultsController revert: self];
      [panel close];
      [NSApp replyToApplicationShouldTerminate: YES];
     }
  }

@end

/*##################################*/
/* FontNameToDisplayNameTransformer */
/*##################################*/

@implementation FontNameToDisplayNameTransformer

+ (Class) transformedValueClass
  {
   return [NSString class];
  }

+ (BOOL) allowsReverseTransformation
  {
   return NO;
  }

- (id) transformedValue: (id) aValue
  {
   NSFont *font = [NSFont fontWithName:aValue size:12];
   return [font displayName];
  }

@end

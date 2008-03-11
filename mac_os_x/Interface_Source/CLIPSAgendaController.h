//
//  CLIPSAgendaController.h
//  CLIPS
//
//  Created by Gary Riley on 3/10/06.
//

#import <Cocoa/Cocoa.h>

@class CLIPSEnvironment;
@class EnvController;

@interface CLIPSAgendaController : NSWindowController 
  {
   IBOutlet NSTableView *focusStack;
   IBOutlet NSTableView *agendaList;
   IBOutlet NSSplitView *splitView;
   IBOutlet NSPopUpButton *environmentList;
   IBOutlet NSButton *resetButton;
   IBOutlet NSButton *stepButton;
   IBOutlet NSButton *runButton;
   IBOutlet NSButton *haltButton;
   IBOutlet NSProgressIndicator *executionIndicator;
   IBOutlet NSArrayController *focusStackController;
   CLIPSEnvironment *environment;
   EnvController *environmentController;
   int fontSize;
   int rowHeight;
  }

/*%%%%%%%%%%%%%%%%*/
/* Action Methods */
/*%%%%%%%%%%%%%%%%*/
  
- (IBAction)                     reset: (id) sender;
- (IBAction)                     run: (id) sender;
- (IBAction)                     step: (id) sender;
- (IBAction)                     showDefrule: (id) sender;
- (IBAction)                     halt: (id) sender;
- (IBAction)                     haltImmediately: (id) sender;

/*%%%%%%%%%%%%%%%*/
/* Other Methods */
/*%%%%%%%%%%%%%%%*/

- (void)                         updateAgendaInspectorText;
  
/*%%%%%%%%%%%%%%%%%%%%%%*/
/* Notification Methods */
/*%%%%%%%%%%%%%%%%%%%%%%*/

- (void)                         targetEnvironmentDeallocated: (NSNotification *) note;

/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

- (void)                         setEnvironment: (CLIPSEnvironment *) theEnvironment;
- (CLIPSEnvironment *)           environment;

- (void)                         setEnvironmentController: (EnvController *) theController;
- (EnvController *)              environmentController;

@end

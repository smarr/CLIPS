//
//  CLIPSInstanceController.h
//  CLIPS
//
//  Created by Gary Riley on 3/3/2008.
//

#import <Cocoa/Cocoa.h>

@class CLIPSEnvironment;
@class EnvController;
@class ModuleArrayController;

@interface CLIPSInstanceController : NSWindowController 
  {
   IBOutlet NSTableView *moduleList;
   IBOutlet NSTableView *instanceList;
   IBOutlet NSTableView *slotValues;
   IBOutlet NSSplitView *splitView;
   IBOutlet NSPopUpButton *environmentList;
   IBOutlet NSButton *displayDefaultedValuesButton;
   IBOutlet NSProgressIndicator *executionIndicator;
   IBOutlet NSArrayController *moduleListController;
   IBOutlet ModuleArrayController *instanceListController;
   IBOutlet NSArrayController *slotValuesController;
   IBOutlet NSSearchField *searchField;
   NSPredicate *slotFilter;
   CLIPSEnvironment *environment;
   EnvController *environmentController;
   int fontSize;
   int rowHeight;
  }

/*%%%%%%%%%%%%%%%%*/
/* Action Methods */
/*%%%%%%%%%%%%%%%%*/
  
- (IBAction)                     displayDefaultedValues: (id) sender;
- (IBAction)                     search: (id) sender;

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

- (void)                         setSlotFilter: (NSPredicate *) theSlotFilter;
- (NSPredicate *)                slotFilter;

@end

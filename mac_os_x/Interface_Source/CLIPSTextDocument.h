//
//  CLIPSTextDocument.h
//  CLIPSEditor
//
//  Created by Gary Riley on 2/14/06.
//  Copyright __MyCompanyName__ 2006 . All rights reserved.
//


#import <Cocoa/Cocoa.h>

@class CLIPSTerminalController;
@class EnvController;
@class CLIPSTextView;

@interface CLIPSTextDocument : NSDocument
  {
   NSString *string;
   IBOutlet CLIPSTextView *textView;
   IBOutlet NSPopUpButton *environmentList;
   IBOutlet NSButton *popupActivator;
   CLIPSTerminalController *terminalController;
   EnvController *environmentController;
   NSDictionary *hiliteColor;
  }

- (void) balanceIt: (NSString *) theText
         leftMiddle: (unsigned int) leftMiddle
         rightMiddle: (unsigned int) rightMiddle
         leftCount: (int) leftCount
         rightCount: (int) rightCount
         textLength: (unsigned int) textLength;

/*%%%%%%%%%%%%%%%%*/
/* Action Methods */
/*%%%%%%%%%%%%%%%%*/

- (IBAction)           loadSelection: (id) sender;
- (IBAction)           batchSelection: (id) sender;
- (IBAction)           loadBuffer: (id) sender;
- (IBAction)           balance: (id) sender;
- (IBAction)           comment: (id) sender;
- (IBAction)           uncomment: (id) sender;

/*%%%%%%%%%%%%%%%%%%%%%%*/
/* Notification Methods */
/*%%%%%%%%%%%%%%%%%%%%%%*/

- (void) targetTerminalClosed: (NSNotification *) note;

/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

- (void)                         setTerminalController: (CLIPSTerminalController *) theController;
- (CLIPSTerminalController *)    terminalController;

- (void)                         setEnvironmentController: (EnvController *) theController;
- (EnvController *)              environmentController;

@end

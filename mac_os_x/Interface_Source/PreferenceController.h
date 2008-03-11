//
//  PreferenceController.h
//  CLIPS
//
//  Created by Gary Riley on 2/25/06.
//

#import <Cocoa/Cocoa.h>


@interface PreferenceController : NSWindowController
  {
  }
  
- (IBAction) changeEditorFont: (id) sender;
- (IBAction) watchAll: (id) sender;
- (IBAction) watchNone: (id) sender;
- (IBAction) doApply: (id) sender;
- (IBAction) doCancel: (id) sender;
- (IBAction) doOK: (id) sender;
- (void) changeFont: (id) fontManager;
- (NSApplicationTerminateReply) reviewPreferencesBeforeQuitting;
- (void) showPanel;

@end

@interface FontNameToDisplayNameTransformer : NSValueTransformer
  {
  }

@end

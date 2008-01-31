/* CLIPSConstructInspectorController */

#import <Cocoa/Cocoa.h>

@class EnvController;

@interface CLIPSConstructInspectorController : NSWindowController
  {
   IBOutlet NSTextView *textView;
   EnvController *environmentController;
  }

- (void) showPanel;

/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

- (void)                         setEnvironmentController: (EnvController *) theController;
- (EnvController *)              environmentController;

@end

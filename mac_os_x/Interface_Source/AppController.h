/* Controller */

#import <Cocoa/Cocoa.h>

@class Preferences;
@class PreferenceController;
@class EnvController;
@class CLIPSTextMenu;

@interface AppController : NSObject
  {
   PreferenceController *preferenceController;
   EnvController *envController;
   CLIPSTextMenu *textMenu;
  }

/*%%%%%%%%%%%%%%%%*/
/* Action Methods */
/*%%%%%%%%%%%%%%%%*/

- (IBAction) showPreferencePanel: (id) sender;
- (IBAction) showCLIPSHomePage: (id) sender;
- (IBAction) showCLIPSDevelopersForum: (id) sender;

/*%%%%%%%%%%%%%%%%%%*/
/* Delegate Methods */
/*%%%%%%%%%%%%%%%%%%*/

- (NSApplicationTerminateReply) applicationShouldTerminate: (NSApplication *) app;

/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

- (void)               setEnvController: (EnvController *) theController;
- (EnvController *)    envController;

@end

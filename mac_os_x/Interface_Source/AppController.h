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
- (IBAction) showCLIPSExpertSystemGroup: (id) sender;
- (IBAction) showCLIPSSourceForgeForums: (id) sender;
- (IBAction) showUsersGuide: (id) sender;
- (IBAction) showBasicProgrammingGuide: (id) sender;
- (IBAction) showAdvancedProgrammingGuide: (id) sender;
- (IBAction) showInterfacesGuide: (id) sender;

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

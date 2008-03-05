/* EnvController */

#import <Cocoa/Cocoa.h>
@class CLIPSTerminalController;
@class CLIPSConstructInspectorController;

@interface EnvController : NSObject
  {
   IBOutlet NSArrayController *terminalArrayController;
   IBOutlet NSArrayController *environmentArrayController;
   IBOutlet NSMenu *environmentMenu;
   IBOutlet NSMenu *debugMenu;
   CLIPSConstructInspectorController *constructInspectorController;
   NSString *constructInspectorText;
   NSLock *fileOpenLock;
  }
  
- (IBAction) newEnvironment: (id) sender;
- (IBAction) newDebugAgenda: (id) sender;
- (IBAction) newDebugFactBrowser: (id) sender;
- (IBAction) newDebugInstanceBrowser: (id) sender;
- (IBAction) newDebugGenericInspector: (id) sender;

- (IBAction) showConstructInspectorPanel: (id) sender;

- (void) setTerminal: (CLIPSTerminalController *) theController;

/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

- (void)                   setTerminalArrayController: (NSArrayController *) theArrayController;
- (NSArrayController *)    terminalArrayController;

- (void)                   setEnvironmentArrayController: (NSArrayController *) theArrayController;
- (NSArrayController *)    environmentArrayController;

- (NSLock *)               fileOpenLock;

- (BOOL)                   terminalExists;
- (void)                   setTerminalExists: (BOOL) value;

- (BOOL)                   environmentExists;
- (void)                   setEnvironmentExists: (BOOL) value;

- (NSString *)             constructInspectorText;
- (void)                   setConstructInspectorText: (NSString *) theConstructInspectorText;

@end

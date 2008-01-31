//
//  GenericController.h
//  CLIPS
//
//  Created by Gary Riley on 3/22/06.
//  Copyright 2006 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@class EnvController;
@class CLIPSEnvironment;

@interface GenericController : NSWindowController 
  {
   IBOutlet NSPopUpButton *environmentList;
   CLIPSEnvironment *environment;
   EnvController *environmentController;
  }
  
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

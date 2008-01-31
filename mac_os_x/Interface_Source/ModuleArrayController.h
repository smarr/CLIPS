//
//  ModuleArrayController.h
//  CLIPS
//
//  Created by Gary Riley on 3/21/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface ModuleArrayController : NSArrayController 
  {
   int moduleIndex;
   NSString *searchString;
  }

- (void)                         setModuleIndex: (int) theModule;
- (int)                          moduleIndex;
- (IBAction)                     search: (id) sender;
- (void)                         setSearchString: (NSString *) string;
@end

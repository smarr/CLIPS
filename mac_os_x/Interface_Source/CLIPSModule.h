//
//  CLIPSModule.h
//  CLIPS
//
//  Created by Gary Riley on 3/15/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface CLIPSModule : NSObject 
  {
   NSString *moduleName;
  }

- (NSString *) description;

- (void)                         setModuleName: (NSString *) theModuleName;
- (NSString *)                   moduleName;

@end
